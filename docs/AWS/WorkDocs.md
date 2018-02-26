## Module AWS.WorkDocs

<p>The WorkDocs API is designed for the following use cases:</p> <ul> <li> <p>File Migration: File migration applications are supported for users who want to migrate their files from an on-premises or off-premises file system or service. Users can insert files into a user directory structure, as well as allow for basic metadata changes, such as modifications to the permissions of files.</p> </li> <li> <p>Security: Support security applications are supported for users who have additional security needs, such as antivirus or data loss prevention. The API actions, along with AWS CloudTrail, allow these applications to detect when changes occur in Amazon WorkDocs. Then, the application can take the necessary actions and replace the target file. If the target file violates the policy, the application can also choose to email the user.</p> </li> <li> <p>eDiscovery/Analytics: General administrative applications are supported, such as eDiscovery and analytics. These applications can choose to mimic or record the actions in an Amazon WorkDocs site, along with AWS CloudTrail, to replicate data for eDiscovery, backup, or analytical applications.</p> </li> </ul> <p>All Amazon WorkDocs API actions are Amazon authenticated and certificate-signed. They not only require the use of the AWS SDK, but also allow for the exclusive use of IAM users and roles to help facilitate access, trust, and permission policies. By creating a role and allowing an IAM user to access the Amazon WorkDocs site, the IAM user gains full administrative visibility into the entire Amazon WorkDocs site (or as set in the IAM policy). This includes, but is not limited to, the ability to modify file permissions and upload any file to any user. This allows developers to perform the three use cases above, as well as give users the ability to grant access on a selective basis using the IAM model.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `abortDocumentVersionUpload`

``` purescript
abortDocumentVersionUpload :: forall eff. AbortDocumentVersionUploadRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Aborts the upload of the specified document version that was previously initiated by <a>InitiateDocumentVersionUpload</a>. The client should make this call only when it no longer intends to upload the document version, or fails to do so.</p>

#### `activateUser`

``` purescript
activateUser :: forall eff. ActivateUserRequest -> Aff (err :: RequestError | eff) ActivateUserResponse
```

<p>Activates the specified user. Only active users can access Amazon WorkDocs.</p>

#### `addResourcePermissions`

``` purescript
addResourcePermissions :: forall eff. AddResourcePermissionsRequest -> Aff (err :: RequestError | eff) AddResourcePermissionsResponse
```

<p>Creates a set of permissions for the specified folder or document. The resource permissions are overwritten if the principals already have different permissions.</p>

#### `createComment`

``` purescript
createComment :: forall eff. CreateCommentRequest -> Aff (err :: RequestError | eff) CreateCommentResponse
```

<p>Adds a new comment to the specified document version.</p>

#### `createCustomMetadata`

``` purescript
createCustomMetadata :: forall eff. CreateCustomMetadataRequest -> Aff (err :: RequestError | eff) CreateCustomMetadataResponse
```

<p>Adds one or more custom properties to the specified resource (a folder, document, or version).</p>

#### `createFolder`

``` purescript
createFolder :: forall eff. CreateFolderRequest -> Aff (err :: RequestError | eff) CreateFolderResponse
```

<p>Creates a folder with the specified name and parent folder.</p>

#### `createLabels`

``` purescript
createLabels :: forall eff. CreateLabelsRequest -> Aff (err :: RequestError | eff) CreateLabelsResponse
```

<p>Adds the specified list of labels to the given resource (a document or folder)</p>

#### `createNotificationSubscription`

``` purescript
createNotificationSubscription :: forall eff. CreateNotificationSubscriptionRequest -> Aff (err :: RequestError | eff) CreateNotificationSubscriptionResponse
```

<p>Configure WorkDocs to use Amazon SNS notifications.</p> <p>The endpoint receives a confirmation message, and must confirm the subscription. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SendMessageToHttp.html#SendMessageToHttp.confirm">Confirm the Subscription</a> in the <i>Amazon Simple Notification Service Developer Guide</i>.</p>

#### `createUser`

``` purescript
createUser :: forall eff. CreateUserRequest -> Aff (err :: RequestError | eff) CreateUserResponse
```

<p>Creates a user in a Simple AD or Microsoft AD directory. The status of a newly created user is "ACTIVE". New users can access Amazon WorkDocs.</p>

#### `deactivateUser`

``` purescript
deactivateUser :: forall eff. DeactivateUserRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deactivates the specified user, which revokes the user's access to Amazon WorkDocs.</p>

#### `deleteComment`

``` purescript
deleteComment :: forall eff. DeleteCommentRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified comment from the document version.</p>

#### `deleteCustomMetadata`

``` purescript
deleteCustomMetadata :: forall eff. DeleteCustomMetadataRequest -> Aff (err :: RequestError | eff) DeleteCustomMetadataResponse
```

<p>Deletes custom metadata from the specified resource.</p>

#### `deleteDocument`

``` purescript
deleteDocument :: forall eff. DeleteDocumentRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Permanently deletes the specified document and its associated metadata.</p>

#### `deleteFolder`

``` purescript
deleteFolder :: forall eff. DeleteFolderRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Permanently deletes the specified folder and its contents.</p>

#### `deleteFolderContents`

``` purescript
deleteFolderContents :: forall eff. DeleteFolderContentsRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the contents of the specified folder.</p>

#### `deleteLabels`

``` purescript
deleteLabels :: forall eff. DeleteLabelsRequest -> Aff (err :: RequestError | eff) DeleteLabelsResponse
```

<p>Deletes the specified list of labels from a resource.</p>

#### `deleteNotificationSubscription`

``` purescript
deleteNotificationSubscription :: forall eff. DeleteNotificationSubscriptionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified subscription from the specified organization.</p>

#### `deleteUser`

``` purescript
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified user from a Simple AD or Microsoft AD directory.</p>

#### `describeActivities`

``` purescript
describeActivities :: forall eff. DescribeActivitiesRequest -> Aff (err :: RequestError | eff) DescribeActivitiesResponse
```

<p>Describes the user activities in a specified time period.</p>

#### `describeComments`

``` purescript
describeComments :: forall eff. DescribeCommentsRequest -> Aff (err :: RequestError | eff) DescribeCommentsResponse
```

<p>List all the comments for the specified document version.</p>

#### `describeDocumentVersions`

``` purescript
describeDocumentVersions :: forall eff. DescribeDocumentVersionsRequest -> Aff (err :: RequestError | eff) DescribeDocumentVersionsResponse
```

<p>Retrieves the document versions for the specified document.</p> <p>By default, only active versions are returned.</p>

#### `describeFolderContents`

``` purescript
describeFolderContents :: forall eff. DescribeFolderContentsRequest -> Aff (err :: RequestError | eff) DescribeFolderContentsResponse
```

<p>Describes the contents of the specified folder, including its documents and subfolders.</p> <p>By default, Amazon WorkDocs returns the first 100 active document and folder metadata items. If there are more results, the response includes a marker that you can use to request the next set of results. You can also request initialized documents.</p>

#### `describeGroups`

``` purescript
describeGroups :: forall eff. DescribeGroupsRequest -> Aff (err :: RequestError | eff) DescribeGroupsResponse
```

<p>Describes the groups specified by query.</p>

#### `describeNotificationSubscriptions`

``` purescript
describeNotificationSubscriptions :: forall eff. DescribeNotificationSubscriptionsRequest -> Aff (err :: RequestError | eff) DescribeNotificationSubscriptionsResponse
```

<p>Lists the specified notification subscriptions.</p>

#### `describeResourcePermissions`

``` purescript
describeResourcePermissions :: forall eff. DescribeResourcePermissionsRequest -> Aff (err :: RequestError | eff) DescribeResourcePermissionsResponse
```

<p>Describes the permissions of a specified resource.</p>

#### `describeRootFolders`

``` purescript
describeRootFolders :: forall eff. DescribeRootFoldersRequest -> Aff (err :: RequestError | eff) DescribeRootFoldersResponse
```

<p>Describes the current user's special folders; the <code>RootFolder</code> and the <code>RecycleBin</code>. <code>RootFolder</code> is the root of user's files and folders and <code>RecycleBin</code> is the root of recycled items. This is not a valid action for SigV4 (administrative API) clients.</p>

#### `describeUsers`

``` purescript
describeUsers :: forall eff. DescribeUsersRequest -> Aff (err :: RequestError | eff) DescribeUsersResponse
```

<p>Describes the specified users. You can describe all users or filter the results (for example, by status or organization).</p> <p>By default, Amazon WorkDocs returns the first 24 active or pending users. If there are more results, the response includes a marker that you can use to request the next set of results.</p>

#### `getCurrentUser`

``` purescript
getCurrentUser :: forall eff. GetCurrentUserRequest -> Aff (err :: RequestError | eff) GetCurrentUserResponse
```

<p>Retrieves details of the current user for whom the authentication token was generated. This is not a valid action for SigV4 (administrative API) clients.</p>

#### `getDocument`

``` purescript
getDocument :: forall eff. GetDocumentRequest -> Aff (err :: RequestError | eff) GetDocumentResponse
```

<p>Retrieves details of a document.</p>

#### `getDocumentPath`

``` purescript
getDocumentPath :: forall eff. GetDocumentPathRequest -> Aff (err :: RequestError | eff) GetDocumentPathResponse
```

<p>Retrieves the path information (the hierarchy from the root folder) for the requested document.</p> <p>By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested document and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the names of the parent folders.</p>

#### `getDocumentVersion`

``` purescript
getDocumentVersion :: forall eff. GetDocumentVersionRequest -> Aff (err :: RequestError | eff) GetDocumentVersionResponse
```

<p>Retrieves version metadata for the specified document.</p>

#### `getFolder`

``` purescript
getFolder :: forall eff. GetFolderRequest -> Aff (err :: RequestError | eff) GetFolderResponse
```

<p>Retrieves the metadata of the specified folder.</p>

#### `getFolderPath`

``` purescript
getFolderPath :: forall eff. GetFolderPathRequest -> Aff (err :: RequestError | eff) GetFolderPathResponse
```

<p>Retrieves the path information (the hierarchy from the root folder) for the specified folder.</p> <p>By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested folder and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the parent folder names.</p>

#### `initiateDocumentVersionUpload`

``` purescript
initiateDocumentVersionUpload :: forall eff. InitiateDocumentVersionUploadRequest -> Aff (err :: RequestError | eff) InitiateDocumentVersionUploadResponse
```

<p>Creates a new document object and version object.</p> <p>The client specifies the parent folder ID and name of the document to upload. The ID is optionally specified when creating a new version of an existing document. This is the first step to upload a document. Next, upload the document to the URL returned from the call, and then call <a>UpdateDocumentVersion</a>.</p> <p>To cancel the document upload, call <a>AbortDocumentVersionUpload</a>.</p>

#### `removeAllResourcePermissions`

``` purescript
removeAllResourcePermissions :: forall eff. RemoveAllResourcePermissionsRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes all the permissions from the specified resource.</p>

#### `removeResourcePermission`

``` purescript
removeResourcePermission :: forall eff. RemoveResourcePermissionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes the permission for the specified principal from the specified resource.</p>

#### `updateDocument`

``` purescript
updateDocument :: forall eff. UpdateDocumentRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates the specified attributes of a document. The user must have access to both the document and its parent folder, if applicable.</p>

#### `updateDocumentVersion`

``` purescript
updateDocumentVersion :: forall eff. UpdateDocumentVersionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Changes the status of the document version to ACTIVE. </p> <p>Amazon WorkDocs also sets its document container to ACTIVE. This is the last step in a document upload, after the client uploads the document to an S3-presigned URL returned by <a>InitiateDocumentVersionUpload</a>. </p>

#### `updateFolder`

``` purescript
updateFolder :: forall eff. UpdateFolderRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates the specified attributes of the specified folder. The user must have access to both the folder and its parent folder, if applicable.</p>

#### `updateUser`

``` purescript
updateUser :: forall eff. UpdateUserRequest -> Aff (err :: RequestError | eff) UpdateUserResponse
```

<p>Updates the specified attributes of the specified user, and grants or revokes administrative privileges to the Amazon WorkDocs site.</p>

#### `AbortDocumentVersionUploadRequest`

``` purescript
newtype AbortDocumentVersionUploadRequest
  = AbortDocumentVersionUploadRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "DocumentId" :: ResourceIdType, "VersionId" :: DocumentVersionIdType }
```

#### `ActivateUserRequest`

``` purescript
newtype ActivateUserRequest
  = ActivateUserRequest { "UserId" :: IdType, "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType) }
```

#### `ActivateUserResponse`

``` purescript
newtype ActivateUserResponse
  = ActivateUserResponse { "User" :: NullOrUndefined (User) }
```

#### `Activity`

``` purescript
newtype Activity
  = Activity { "Type" :: NullOrUndefined (ActivityType), "TimeStamp" :: NullOrUndefined (TimestampType), "OrganizationId" :: NullOrUndefined (IdType), "Initiator" :: NullOrUndefined (UserMetadata), "Participants" :: NullOrUndefined (Participants), "ResourceMetadata" :: NullOrUndefined (ResourceMetadata), "OriginalParent" :: NullOrUndefined (ResourceMetadata), "CommentMetadata" :: NullOrUndefined (CommentMetadata) }
```

<p>Describes the activity information.</p>

#### `ActivityType`

``` purescript
newtype ActivityType
  = ActivityType String
```

#### `AddResourcePermissionsRequest`

``` purescript
newtype AddResourcePermissionsRequest
  = AddResourcePermissionsRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "ResourceId" :: ResourceIdType, "Principals" :: SharePrincipalList, "NotificationOptions" :: NullOrUndefined (NotificationOptions) }
```

#### `AddResourcePermissionsResponse`

``` purescript
newtype AddResourcePermissionsResponse
  = AddResourcePermissionsResponse { "ShareResults" :: NullOrUndefined (ShareResultsList) }
```

#### `AuthenticationHeaderType`

``` purescript
newtype AuthenticationHeaderType
  = AuthenticationHeaderType String
```

#### `BooleanEnumType`

``` purescript
newtype BooleanEnumType
  = BooleanEnumType String
```

#### `BooleanType`

``` purescript
newtype BooleanType
  = BooleanType Boolean
```

#### `Comment`

``` purescript
newtype Comment
  = Comment { "CommentId" :: CommentIdType, "ParentId" :: NullOrUndefined (CommentIdType), "ThreadId" :: NullOrUndefined (CommentIdType), "Text" :: NullOrUndefined (CommentTextType), "Contributor" :: NullOrUndefined (User), "CreatedTimestamp" :: NullOrUndefined (TimestampType), "Status" :: NullOrUndefined (CommentStatusType), "Visibility" :: NullOrUndefined (CommentVisibilityType), "RecipientId" :: NullOrUndefined (IdType) }
```

<p>Describes a comment.</p>

#### `CommentIdType`

``` purescript
newtype CommentIdType
  = CommentIdType String
```

#### `CommentList`

``` purescript
newtype CommentList
  = CommentList (Array Comment)
```

#### `CommentMetadata`

``` purescript
newtype CommentMetadata
  = CommentMetadata { "CommentId" :: NullOrUndefined (CommentIdType), "Contributor" :: NullOrUndefined (User), "CreatedTimestamp" :: NullOrUndefined (TimestampType), "CommentStatus" :: NullOrUndefined (CommentStatusType), "RecipientId" :: NullOrUndefined (IdType) }
```

<p>Describes the metadata of a comment.</p>

#### `CommentStatusType`

``` purescript
newtype CommentStatusType
  = CommentStatusType String
```

#### `CommentTextType`

``` purescript
newtype CommentTextType
  = CommentTextType String
```

#### `CommentVisibilityType`

``` purescript
newtype CommentVisibilityType
  = CommentVisibilityType String
```

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The resource hierarchy is changing.</p>

#### `CreateCommentRequest`

``` purescript
newtype CreateCommentRequest
  = CreateCommentRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "DocumentId" :: ResourceIdType, "VersionId" :: DocumentVersionIdType, "ParentId" :: NullOrUndefined (CommentIdType), "ThreadId" :: NullOrUndefined (CommentIdType), "Text" :: CommentTextType, "Visibility" :: NullOrUndefined (CommentVisibilityType), "NotifyCollaborators" :: NullOrUndefined (BooleanType) }
```

#### `CreateCommentResponse`

``` purescript
newtype CreateCommentResponse
  = CreateCommentResponse { "Comment" :: NullOrUndefined (Comment) }
```

#### `CreateCustomMetadataRequest`

``` purescript
newtype CreateCustomMetadataRequest
  = CreateCustomMetadataRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "ResourceId" :: ResourceIdType, "VersionId" :: NullOrUndefined (DocumentVersionIdType), "CustomMetadata" :: CustomMetadataMap }
```

#### `CreateCustomMetadataResponse`

``` purescript
newtype CreateCustomMetadataResponse
  = CreateCustomMetadataResponse {  }
```

#### `CreateFolderRequest`

``` purescript
newtype CreateFolderRequest
  = CreateFolderRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "Name" :: NullOrUndefined (ResourceNameType), "ParentFolderId" :: ResourceIdType }
```

#### `CreateFolderResponse`

``` purescript
newtype CreateFolderResponse
  = CreateFolderResponse { "Metadata" :: NullOrUndefined (FolderMetadata) }
```

#### `CreateLabelsRequest`

``` purescript
newtype CreateLabelsRequest
  = CreateLabelsRequest { "ResourceId" :: ResourceIdType, "Labels" :: SharedLabels, "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType) }
```

#### `CreateLabelsResponse`

``` purescript
newtype CreateLabelsResponse
  = CreateLabelsResponse {  }
```

#### `CreateNotificationSubscriptionRequest`

``` purescript
newtype CreateNotificationSubscriptionRequest
  = CreateNotificationSubscriptionRequest { "OrganizationId" :: IdType, "Endpoint" :: SubscriptionEndPointType, "Protocol" :: SubscriptionProtocolType, "SubscriptionType" :: SubscriptionType }
```

#### `CreateNotificationSubscriptionResponse`

``` purescript
newtype CreateNotificationSubscriptionResponse
  = CreateNotificationSubscriptionResponse { "Subscription" :: NullOrUndefined (Subscription) }
```

#### `CreateUserRequest`

``` purescript
newtype CreateUserRequest
  = CreateUserRequest { "OrganizationId" :: NullOrUndefined (IdType), "Username" :: UsernameType, "EmailAddress" :: NullOrUndefined (EmailAddressType), "GivenName" :: UserAttributeValueType, "Surname" :: UserAttributeValueType, "Password" :: PasswordType, "TimeZoneId" :: NullOrUndefined (TimeZoneIdType), "StorageRule" :: NullOrUndefined (StorageRuleType), "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType) }
```

#### `CreateUserResponse`

``` purescript
newtype CreateUserResponse
  = CreateUserResponse { "User" :: NullOrUndefined (User) }
```

#### `CustomMetadataKeyList`

``` purescript
newtype CustomMetadataKeyList
  = CustomMetadataKeyList (Array CustomMetadataKeyType)
```

#### `CustomMetadataKeyType`

``` purescript
newtype CustomMetadataKeyType
  = CustomMetadataKeyType String
```

#### `CustomMetadataLimitExceededException`

``` purescript
newtype CustomMetadataLimitExceededException
  = CustomMetadataLimitExceededException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The limit has been reached on the number of custom properties for the specified resource.</p>

#### `CustomMetadataMap`

``` purescript
newtype CustomMetadataMap
  = CustomMetadataMap (Map CustomMetadataKeyType CustomMetadataValueType)
```

#### `CustomMetadataValueType`

``` purescript
newtype CustomMetadataValueType
  = CustomMetadataValueType String
```

#### `DeactivateUserRequest`

``` purescript
newtype DeactivateUserRequest
  = DeactivateUserRequest { "UserId" :: IdType, "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType) }
```

#### `DeactivatingLastSystemUserException`

``` purescript
newtype DeactivatingLastSystemUserException
  = DeactivatingLastSystemUserException {  }
```

<p>The last user in the organization is being deactivated.</p>

#### `DeleteCommentRequest`

``` purescript
newtype DeleteCommentRequest
  = DeleteCommentRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "DocumentId" :: ResourceIdType, "VersionId" :: DocumentVersionIdType, "CommentId" :: CommentIdType }
```

#### `DeleteCustomMetadataRequest`

``` purescript
newtype DeleteCustomMetadataRequest
  = DeleteCustomMetadataRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "ResourceId" :: ResourceIdType, "VersionId" :: NullOrUndefined (DocumentVersionIdType), "Keys" :: NullOrUndefined (CustomMetadataKeyList), "DeleteAll" :: NullOrUndefined (BooleanType) }
```

#### `DeleteCustomMetadataResponse`

``` purescript
newtype DeleteCustomMetadataResponse
  = DeleteCustomMetadataResponse {  }
```

#### `DeleteDocumentRequest`

``` purescript
newtype DeleteDocumentRequest
  = DeleteDocumentRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "DocumentId" :: ResourceIdType }
```

#### `DeleteFolderContentsRequest`

``` purescript
newtype DeleteFolderContentsRequest
  = DeleteFolderContentsRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "FolderId" :: ResourceIdType }
```

#### `DeleteFolderRequest`

``` purescript
newtype DeleteFolderRequest
  = DeleteFolderRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "FolderId" :: ResourceIdType }
```

#### `DeleteLabelsRequest`

``` purescript
newtype DeleteLabelsRequest
  = DeleteLabelsRequest { "ResourceId" :: ResourceIdType, "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "Labels" :: NullOrUndefined (SharedLabels), "DeleteAll" :: NullOrUndefined (BooleanType) }
```

#### `DeleteLabelsResponse`

``` purescript
newtype DeleteLabelsResponse
  = DeleteLabelsResponse {  }
```

#### `DeleteNotificationSubscriptionRequest`

``` purescript
newtype DeleteNotificationSubscriptionRequest
  = DeleteNotificationSubscriptionRequest { "SubscriptionId" :: IdType, "OrganizationId" :: IdType }
```

#### `DeleteUserRequest`

``` purescript
newtype DeleteUserRequest
  = DeleteUserRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "UserId" :: IdType }
```

#### `DescribeActivitiesRequest`

``` purescript
newtype DescribeActivitiesRequest
  = DescribeActivitiesRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "StartTime" :: NullOrUndefined (TimestampType), "EndTime" :: NullOrUndefined (TimestampType), "OrganizationId" :: NullOrUndefined (IdType), "UserId" :: NullOrUndefined (IdType), "Limit" :: NullOrUndefined (LimitType), "Marker" :: NullOrUndefined (MarkerType) }
```

#### `DescribeActivitiesResponse`

``` purescript
newtype DescribeActivitiesResponse
  = DescribeActivitiesResponse { "UserActivities" :: NullOrUndefined (UserActivities), "Marker" :: NullOrUndefined (MarkerType) }
```

#### `DescribeCommentsRequest`

``` purescript
newtype DescribeCommentsRequest
  = DescribeCommentsRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "DocumentId" :: ResourceIdType, "VersionId" :: DocumentVersionIdType, "Limit" :: NullOrUndefined (LimitType), "Marker" :: NullOrUndefined (MarkerType) }
```

#### `DescribeCommentsResponse`

``` purescript
newtype DescribeCommentsResponse
  = DescribeCommentsResponse { "Comments" :: NullOrUndefined (CommentList), "Marker" :: NullOrUndefined (MarkerType) }
```

#### `DescribeDocumentVersionsRequest`

``` purescript
newtype DescribeDocumentVersionsRequest
  = DescribeDocumentVersionsRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "DocumentId" :: ResourceIdType, "Marker" :: NullOrUndefined (PageMarkerType), "Limit" :: NullOrUndefined (LimitType), "Include" :: NullOrUndefined (FieldNamesType), "Fields" :: NullOrUndefined (FieldNamesType) }
```

#### `DescribeDocumentVersionsResponse`

``` purescript
newtype DescribeDocumentVersionsResponse
  = DescribeDocumentVersionsResponse { "DocumentVersions" :: NullOrUndefined (DocumentVersionMetadataList), "Marker" :: NullOrUndefined (PageMarkerType) }
```

#### `DescribeFolderContentsRequest`

``` purescript
newtype DescribeFolderContentsRequest
  = DescribeFolderContentsRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "FolderId" :: ResourceIdType, "Sort" :: NullOrUndefined (ResourceSortType), "Order" :: NullOrUndefined (OrderType), "Limit" :: NullOrUndefined (LimitType), "Marker" :: NullOrUndefined (PageMarkerType), "Type" :: NullOrUndefined (FolderContentType), "Include" :: NullOrUndefined (FieldNamesType) }
```

#### `DescribeFolderContentsResponse`

``` purescript
newtype DescribeFolderContentsResponse
  = DescribeFolderContentsResponse { "Folders" :: NullOrUndefined (FolderMetadataList), "Documents" :: NullOrUndefined (DocumentMetadataList), "Marker" :: NullOrUndefined (PageMarkerType) }
```

#### `DescribeGroupsRequest`

``` purescript
newtype DescribeGroupsRequest
  = DescribeGroupsRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "SearchQuery" :: SearchQueryType, "OrganizationId" :: NullOrUndefined (IdType), "Marker" :: NullOrUndefined (MarkerType), "Limit" :: NullOrUndefined (PositiveIntegerType) }
```

#### `DescribeGroupsResponse`

``` purescript
newtype DescribeGroupsResponse
  = DescribeGroupsResponse { "Groups" :: NullOrUndefined (GroupMetadataList), "Marker" :: NullOrUndefined (MarkerType) }
```

#### `DescribeNotificationSubscriptionsRequest`

``` purescript
newtype DescribeNotificationSubscriptionsRequest
  = DescribeNotificationSubscriptionsRequest { "OrganizationId" :: IdType, "Marker" :: NullOrUndefined (PageMarkerType), "Limit" :: NullOrUndefined (LimitType) }
```

#### `DescribeNotificationSubscriptionsResponse`

``` purescript
newtype DescribeNotificationSubscriptionsResponse
  = DescribeNotificationSubscriptionsResponse { "Subscriptions" :: NullOrUndefined (SubscriptionList), "Marker" :: NullOrUndefined (PageMarkerType) }
```

#### `DescribeResourcePermissionsRequest`

``` purescript
newtype DescribeResourcePermissionsRequest
  = DescribeResourcePermissionsRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "ResourceId" :: ResourceIdType, "PrincipalId" :: NullOrUndefined (IdType), "Limit" :: NullOrUndefined (LimitType), "Marker" :: NullOrUndefined (PageMarkerType) }
```

#### `DescribeResourcePermissionsResponse`

``` purescript
newtype DescribeResourcePermissionsResponse
  = DescribeResourcePermissionsResponse { "Principals" :: NullOrUndefined (PrincipalList), "Marker" :: NullOrUndefined (PageMarkerType) }
```

#### `DescribeRootFoldersRequest`

``` purescript
newtype DescribeRootFoldersRequest
  = DescribeRootFoldersRequest { "AuthenticationToken" :: AuthenticationHeaderType, "Limit" :: NullOrUndefined (LimitType), "Marker" :: NullOrUndefined (PageMarkerType) }
```

#### `DescribeRootFoldersResponse`

``` purescript
newtype DescribeRootFoldersResponse
  = DescribeRootFoldersResponse { "Folders" :: NullOrUndefined (FolderMetadataList), "Marker" :: NullOrUndefined (PageMarkerType) }
```

#### `DescribeUsersRequest`

``` purescript
newtype DescribeUsersRequest
  = DescribeUsersRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "OrganizationId" :: NullOrUndefined (IdType), "UserIds" :: NullOrUndefined (UserIdsType), "Query" :: NullOrUndefined (SearchQueryType), "Include" :: NullOrUndefined (UserFilterType), "Order" :: NullOrUndefined (OrderType), "Sort" :: NullOrUndefined (UserSortType), "Marker" :: NullOrUndefined (PageMarkerType), "Limit" :: NullOrUndefined (LimitType), "Fields" :: NullOrUndefined (FieldNamesType) }
```

#### `DescribeUsersResponse`

``` purescript
newtype DescribeUsersResponse
  = DescribeUsersResponse { "Users" :: NullOrUndefined (OrganizationUserList), "TotalNumberOfUsers" :: NullOrUndefined (SizeType), "Marker" :: NullOrUndefined (PageMarkerType) }
```

#### `DocumentContentType`

``` purescript
newtype DocumentContentType
  = DocumentContentType String
```

#### `DocumentLockedForCommentsException`

``` purescript
newtype DocumentLockedForCommentsException
  = DocumentLockedForCommentsException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>This exception is thrown when the document is locked for comments and user tries to create or delete a comment on that document.</p>

#### `DocumentMetadata`

``` purescript
newtype DocumentMetadata
  = DocumentMetadata { "Id" :: NullOrUndefined (ResourceIdType), "CreatorId" :: NullOrUndefined (IdType), "ParentFolderId" :: NullOrUndefined (ResourceIdType), "CreatedTimestamp" :: NullOrUndefined (TimestampType), "ModifiedTimestamp" :: NullOrUndefined (TimestampType), "LatestVersionMetadata" :: NullOrUndefined (DocumentVersionMetadata), "ResourceState" :: NullOrUndefined (ResourceStateType), "Labels" :: NullOrUndefined (SharedLabels) }
```

<p>Describes the document.</p>

#### `DocumentMetadataList`

``` purescript
newtype DocumentMetadataList
  = DocumentMetadataList (Array DocumentMetadata)
```

#### `DocumentSourceType`

``` purescript
newtype DocumentSourceType
  = DocumentSourceType String
```

#### `DocumentSourceUrlMap`

``` purescript
newtype DocumentSourceUrlMap
  = DocumentSourceUrlMap (Map DocumentSourceType UrlType)
```

#### `DocumentStatusType`

``` purescript
newtype DocumentStatusType
  = DocumentStatusType String
```

#### `DocumentThumbnailType`

``` purescript
newtype DocumentThumbnailType
  = DocumentThumbnailType String
```

#### `DocumentThumbnailUrlMap`

``` purescript
newtype DocumentThumbnailUrlMap
  = DocumentThumbnailUrlMap (Map DocumentThumbnailType UrlType)
```

#### `DocumentVersionIdType`

``` purescript
newtype DocumentVersionIdType
  = DocumentVersionIdType String
```

#### `DocumentVersionMetadata`

``` purescript
newtype DocumentVersionMetadata
  = DocumentVersionMetadata { "Id" :: NullOrUndefined (DocumentVersionIdType), "Name" :: NullOrUndefined (ResourceNameType), "ContentType" :: NullOrUndefined (DocumentContentType), "Size" :: NullOrUndefined (SizeType), "Signature" :: NullOrUndefined (HashType), "Status" :: NullOrUndefined (DocumentStatusType), "CreatedTimestamp" :: NullOrUndefined (TimestampType), "ModifiedTimestamp" :: NullOrUndefined (TimestampType), "ContentCreatedTimestamp" :: NullOrUndefined (TimestampType), "ContentModifiedTimestamp" :: NullOrUndefined (TimestampType), "CreatorId" :: NullOrUndefined (IdType), "Thumbnail" :: NullOrUndefined (DocumentThumbnailUrlMap), "Source" :: NullOrUndefined (DocumentSourceUrlMap) }
```

<p>Describes a version of a document.</p>

#### `DocumentVersionMetadataList`

``` purescript
newtype DocumentVersionMetadataList
  = DocumentVersionMetadataList (Array DocumentVersionMetadata)
```

#### `DocumentVersionStatus`

``` purescript
newtype DocumentVersionStatus
  = DocumentVersionStatus String
```

#### `DraftUploadOutOfSyncException`

``` purescript
newtype DraftUploadOutOfSyncException
  = DraftUploadOutOfSyncException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>This exception is thrown when a valid checkout ID is not presented on document version upload calls for a document that has been checked out from Web client.</p>

#### `EmailAddressType`

``` purescript
newtype EmailAddressType
  = EmailAddressType String
```

#### `EntityAlreadyExistsException`

``` purescript
newtype EntityAlreadyExistsException
  = EntityAlreadyExistsException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The resource already exists.</p>

#### `EntityIdList`

``` purescript
newtype EntityIdList
  = EntityIdList (Array IdType)
```

#### `EntityNotExistsException`

``` purescript
newtype EntityNotExistsException
  = EntityNotExistsException { "Message" :: NullOrUndefined (ErrorMessageType), "EntityIds" :: NullOrUndefined (EntityIdList) }
```

<p>The resource does not exist.</p>

#### `ErrorMessageType`

``` purescript
newtype ErrorMessageType
  = ErrorMessageType String
```

#### `FailedDependencyException`

``` purescript
newtype FailedDependencyException
  = FailedDependencyException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The AWS Directory Service cannot reach an on-premises instance. Or a dependency under the control of the organization is failing, such as a connected Active Directory.</p>

#### `FieldNamesType`

``` purescript
newtype FieldNamesType
  = FieldNamesType String
```

#### `FolderContentType`

``` purescript
newtype FolderContentType
  = FolderContentType String
```

#### `FolderMetadata`

``` purescript
newtype FolderMetadata
  = FolderMetadata { "Id" :: NullOrUndefined (ResourceIdType), "Name" :: NullOrUndefined (ResourceNameType), "CreatorId" :: NullOrUndefined (IdType), "ParentFolderId" :: NullOrUndefined (ResourceIdType), "CreatedTimestamp" :: NullOrUndefined (TimestampType), "ModifiedTimestamp" :: NullOrUndefined (TimestampType), "ResourceState" :: NullOrUndefined (ResourceStateType), "Signature" :: NullOrUndefined (HashType), "Labels" :: NullOrUndefined (SharedLabels), "Size" :: NullOrUndefined (SizeType), "LatestVersionSize" :: NullOrUndefined (SizeType) }
```

<p>Describes a folder.</p>

#### `FolderMetadataList`

``` purescript
newtype FolderMetadataList
  = FolderMetadataList (Array FolderMetadata)
```

#### `GetCurrentUserRequest`

``` purescript
newtype GetCurrentUserRequest
  = GetCurrentUserRequest { "AuthenticationToken" :: AuthenticationHeaderType }
```

#### `GetCurrentUserResponse`

``` purescript
newtype GetCurrentUserResponse
  = GetCurrentUserResponse { "User" :: NullOrUndefined (User) }
```

#### `GetDocumentPathRequest`

``` purescript
newtype GetDocumentPathRequest
  = GetDocumentPathRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "DocumentId" :: IdType, "Limit" :: NullOrUndefined (LimitType), "Fields" :: NullOrUndefined (FieldNamesType), "Marker" :: NullOrUndefined (PageMarkerType) }
```

#### `GetDocumentPathResponse`

``` purescript
newtype GetDocumentPathResponse
  = GetDocumentPathResponse { "Path" :: NullOrUndefined (ResourcePath) }
```

#### `GetDocumentRequest`

``` purescript
newtype GetDocumentRequest
  = GetDocumentRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "DocumentId" :: ResourceIdType, "IncludeCustomMetadata" :: NullOrUndefined (BooleanType) }
```

#### `GetDocumentResponse`

``` purescript
newtype GetDocumentResponse
  = GetDocumentResponse { "Metadata" :: NullOrUndefined (DocumentMetadata), "CustomMetadata" :: NullOrUndefined (CustomMetadataMap) }
```

#### `GetDocumentVersionRequest`

``` purescript
newtype GetDocumentVersionRequest
  = GetDocumentVersionRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "DocumentId" :: ResourceIdType, "VersionId" :: DocumentVersionIdType, "Fields" :: NullOrUndefined (FieldNamesType), "IncludeCustomMetadata" :: NullOrUndefined (BooleanType) }
```

#### `GetDocumentVersionResponse`

``` purescript
newtype GetDocumentVersionResponse
  = GetDocumentVersionResponse { "Metadata" :: NullOrUndefined (DocumentVersionMetadata), "CustomMetadata" :: NullOrUndefined (CustomMetadataMap) }
```

#### `GetFolderPathRequest`

``` purescript
newtype GetFolderPathRequest
  = GetFolderPathRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "FolderId" :: IdType, "Limit" :: NullOrUndefined (LimitType), "Fields" :: NullOrUndefined (FieldNamesType), "Marker" :: NullOrUndefined (PageMarkerType) }
```

#### `GetFolderPathResponse`

``` purescript
newtype GetFolderPathResponse
  = GetFolderPathResponse { "Path" :: NullOrUndefined (ResourcePath) }
```

#### `GetFolderRequest`

``` purescript
newtype GetFolderRequest
  = GetFolderRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "FolderId" :: ResourceIdType, "IncludeCustomMetadata" :: NullOrUndefined (BooleanType) }
```

#### `GetFolderResponse`

``` purescript
newtype GetFolderResponse
  = GetFolderResponse { "Metadata" :: NullOrUndefined (FolderMetadata), "CustomMetadata" :: NullOrUndefined (CustomMetadataMap) }
```

#### `GroupMetadata`

``` purescript
newtype GroupMetadata
  = GroupMetadata { "Id" :: NullOrUndefined (IdType), "Name" :: NullOrUndefined (GroupNameType) }
```

<p>Describes the metadata of a user group.</p>

#### `GroupMetadataList`

``` purescript
newtype GroupMetadataList
  = GroupMetadataList (Array GroupMetadata)
```

#### `GroupNameType`

``` purescript
newtype GroupNameType
  = GroupNameType String
```

#### `HashType`

``` purescript
newtype HashType
  = HashType String
```

#### `HeaderNameType`

``` purescript
newtype HeaderNameType
  = HeaderNameType String
```

#### `HeaderValueType`

``` purescript
newtype HeaderValueType
  = HeaderValueType String
```

#### `IdType`

``` purescript
newtype IdType
  = IdType String
```

#### `IllegalUserStateException`

``` purescript
newtype IllegalUserStateException
  = IllegalUserStateException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The user is undergoing transfer of ownership.</p>

#### `InitiateDocumentVersionUploadRequest`

``` purescript
newtype InitiateDocumentVersionUploadRequest
  = InitiateDocumentVersionUploadRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "Id" :: NullOrUndefined (ResourceIdType), "Name" :: NullOrUndefined (ResourceNameType), "ContentCreatedTimestamp" :: NullOrUndefined (TimestampType), "ContentModifiedTimestamp" :: NullOrUndefined (TimestampType), "ContentType" :: NullOrUndefined (DocumentContentType), "DocumentSizeInBytes" :: NullOrUndefined (SizeType), "ParentFolderId" :: ResourceIdType }
```

#### `InitiateDocumentVersionUploadResponse`

``` purescript
newtype InitiateDocumentVersionUploadResponse
  = InitiateDocumentVersionUploadResponse { "Metadata" :: NullOrUndefined (DocumentMetadata), "UploadMetadata" :: NullOrUndefined (UploadMetadata) }
```

#### `InvalidArgumentException`

``` purescript
newtype InvalidArgumentException
  = InvalidArgumentException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The pagination marker or limit fields are not valid.</p>

#### `InvalidOperationException`

``` purescript
newtype InvalidOperationException
  = InvalidOperationException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The operation is invalid.</p>

#### `InvalidPasswordException`

``` purescript
newtype InvalidPasswordException
  = InvalidPasswordException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The password is invalid.</p>

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The maximum of 100,000 folders under the parent folder has been exceeded.</p>

#### `LimitType`

``` purescript
newtype LimitType
  = LimitType Int
```

#### `LocaleType`

``` purescript
newtype LocaleType
  = LocaleType String
```

#### `MarkerType`

``` purescript
newtype MarkerType
  = MarkerType String
```

#### `MessageType`

``` purescript
newtype MessageType
  = MessageType String
```

#### `NotificationOptions`

``` purescript
newtype NotificationOptions
  = NotificationOptions { "SendEmail" :: NullOrUndefined (BooleanType), "EmailMessage" :: NullOrUndefined (MessageType) }
```

<p>Set of options which defines notification preferences of given action.</p>

#### `OrderType`

``` purescript
newtype OrderType
  = OrderType String
```

#### `OrganizationUserList`

``` purescript
newtype OrganizationUserList
  = OrganizationUserList (Array User)
```

#### `PageMarkerType`

``` purescript
newtype PageMarkerType
  = PageMarkerType String
```

#### `Participants`

``` purescript
newtype Participants
  = Participants { "Users" :: NullOrUndefined (UserMetadataList), "Groups" :: NullOrUndefined (GroupMetadataList) }
```

<p>Describes the users or user groups.</p>

#### `PasswordType`

``` purescript
newtype PasswordType
  = PasswordType String
```

#### `PermissionInfo`

``` purescript
newtype PermissionInfo
  = PermissionInfo { "Role" :: NullOrUndefined (RoleType), "Type" :: NullOrUndefined (RolePermissionType) }
```

<p>Describes the permissions.</p>

#### `PermissionInfoList`

``` purescript
newtype PermissionInfoList
  = PermissionInfoList (Array PermissionInfo)
```

#### `PositiveIntegerType`

``` purescript
newtype PositiveIntegerType
  = PositiveIntegerType Int
```

#### `PositiveSizeType`

``` purescript
newtype PositiveSizeType
  = PositiveSizeType Number
```

#### `Principal`

``` purescript
newtype Principal
  = Principal { "Id" :: NullOrUndefined (IdType), "Type" :: NullOrUndefined (PrincipalType), "Roles" :: NullOrUndefined (PermissionInfoList) }
```

<p>Describes a resource.</p>

#### `PrincipalList`

``` purescript
newtype PrincipalList
  = PrincipalList (Array Principal)
```

#### `PrincipalType`

``` purescript
newtype PrincipalType
  = PrincipalType String
```

#### `ProhibitedStateException`

``` purescript
newtype ProhibitedStateException
  = ProhibitedStateException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The specified document version is not in the INITIALIZED state.</p>

#### `RemoveAllResourcePermissionsRequest`

``` purescript
newtype RemoveAllResourcePermissionsRequest
  = RemoveAllResourcePermissionsRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "ResourceId" :: ResourceIdType }
```

#### `RemoveResourcePermissionRequest`

``` purescript
newtype RemoveResourcePermissionRequest
  = RemoveResourcePermissionRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "ResourceId" :: ResourceIdType, "PrincipalId" :: IdType, "PrincipalType" :: NullOrUndefined (PrincipalType) }
```

#### `ResourceAlreadyCheckedOutException`

``` purescript
newtype ResourceAlreadyCheckedOutException
  = ResourceAlreadyCheckedOutException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The resource is already checked out.</p>

#### `ResourceIdType`

``` purescript
newtype ResourceIdType
  = ResourceIdType String
```

#### `ResourceMetadata`

``` purescript
newtype ResourceMetadata
  = ResourceMetadata { "Type" :: NullOrUndefined (ResourceType), "Name" :: NullOrUndefined (ResourceNameType), "OriginalName" :: NullOrUndefined (ResourceNameType), "Id" :: NullOrUndefined (ResourceIdType), "VersionId" :: NullOrUndefined (DocumentVersionIdType), "Owner" :: NullOrUndefined (UserMetadata), "ParentId" :: NullOrUndefined (ResourceIdType) }
```

<p>Describes the metadata of a resource.</p>

#### `ResourceNameType`

``` purescript
newtype ResourceNameType
  = ResourceNameType String
```

#### `ResourcePath`

``` purescript
newtype ResourcePath
  = ResourcePath { "Components" :: NullOrUndefined (ResourcePathComponentList) }
```

<p>Describes the path information of a resource.</p>

#### `ResourcePathComponent`

``` purescript
newtype ResourcePathComponent
  = ResourcePathComponent { "Id" :: NullOrUndefined (IdType), "Name" :: NullOrUndefined (ResourceNameType) }
```

<p>Describes the resource path.</p>

#### `ResourcePathComponentList`

``` purescript
newtype ResourcePathComponentList
  = ResourcePathComponentList (Array ResourcePathComponent)
```

#### `ResourceSortType`

``` purescript
newtype ResourceSortType
  = ResourceSortType String
```

#### `ResourceStateType`

``` purescript
newtype ResourceStateType
  = ResourceStateType String
```

#### `ResourceType`

``` purescript
newtype ResourceType
  = ResourceType String
```

#### `RolePermissionType`

``` purescript
newtype RolePermissionType
  = RolePermissionType String
```

#### `RoleType`

``` purescript
newtype RoleType
  = RoleType String
```

#### `SearchQueryType`

``` purescript
newtype SearchQueryType
  = SearchQueryType String
```

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>One or more of the dependencies is unavailable.</p>

#### `SharePrincipal`

``` purescript
newtype SharePrincipal
  = SharePrincipal { "Id" :: IdType, "Type" :: PrincipalType, "Role" :: RoleType }
```

<p>Describes the recipient type and ID, if available.</p>

#### `SharePrincipalList`

``` purescript
newtype SharePrincipalList
  = SharePrincipalList (Array SharePrincipal)
```

#### `ShareResult`

``` purescript
newtype ShareResult
  = ShareResult { "PrincipalId" :: NullOrUndefined (IdType), "Role" :: NullOrUndefined (RoleType), "Status" :: NullOrUndefined (ShareStatusType), "ShareId" :: NullOrUndefined (ResourceIdType), "StatusMessage" :: NullOrUndefined (MessageType) }
```

<p>Describes the share results of a resource.</p>

#### `ShareResultsList`

``` purescript
newtype ShareResultsList
  = ShareResultsList (Array ShareResult)
```

#### `ShareStatusType`

``` purescript
newtype ShareStatusType
  = ShareStatusType String
```

#### `SharedLabel`

``` purescript
newtype SharedLabel
  = SharedLabel String
```

#### `SharedLabels`

``` purescript
newtype SharedLabels
  = SharedLabels (Array SharedLabel)
```

#### `SignedHeaderMap`

``` purescript
newtype SignedHeaderMap
  = SignedHeaderMap (Map HeaderNameType HeaderValueType)
```

#### `SizeType`

``` purescript
newtype SizeType
  = SizeType Number
```

#### `StorageLimitExceededException`

``` purescript
newtype StorageLimitExceededException
  = StorageLimitExceededException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The storage limit has been exceeded.</p>

#### `StorageLimitWillExceedException`

``` purescript
newtype StorageLimitWillExceedException
  = StorageLimitWillExceedException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The storage limit will be exceeded.</p>

#### `StorageRuleType`

``` purescript
newtype StorageRuleType
  = StorageRuleType { "StorageAllocatedInBytes" :: NullOrUndefined (PositiveSizeType), "StorageType" :: NullOrUndefined (StorageType) }
```

<p>Describes the storage for a user.</p>

#### `StorageType`

``` purescript
newtype StorageType
  = StorageType String
```

#### `Subscription`

``` purescript
newtype Subscription
  = Subscription { "SubscriptionId" :: NullOrUndefined (IdType), "EndPoint" :: NullOrUndefined (SubscriptionEndPointType), "Protocol" :: NullOrUndefined (SubscriptionProtocolType) }
```

<p>Describes a subscription.</p>

#### `SubscriptionEndPointType`

``` purescript
newtype SubscriptionEndPointType
  = SubscriptionEndPointType String
```

#### `SubscriptionList`

``` purescript
newtype SubscriptionList
  = SubscriptionList (Array Subscription)
```

#### `SubscriptionProtocolType`

``` purescript
newtype SubscriptionProtocolType
  = SubscriptionProtocolType String
```

#### `SubscriptionType`

``` purescript
newtype SubscriptionType
  = SubscriptionType String
```

#### `TimeZoneIdType`

``` purescript
newtype TimeZoneIdType
  = TimeZoneIdType String
```

#### `TimestampType`

``` purescript
newtype TimestampType
  = TimestampType Number
```

#### `TooManyLabelsException`

``` purescript
newtype TooManyLabelsException
  = TooManyLabelsException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The limit has been reached on the number of labels for the specified resource.</p>

#### `TooManySubscriptionsException`

``` purescript
newtype TooManySubscriptionsException
  = TooManySubscriptionsException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>You've reached the limit on the number of subscriptions for the WorkDocs instance.</p>

#### `UnauthorizedOperationException`

``` purescript
newtype UnauthorizedOperationException
  = UnauthorizedOperationException {  }
```

<p>The operation is not permitted.</p>

#### `UnauthorizedResourceAccessException`

``` purescript
newtype UnauthorizedResourceAccessException
  = UnauthorizedResourceAccessException { "Message" :: NullOrUndefined (ErrorMessageType) }
```

<p>The caller does not have access to perform the action on the resource.</p>

#### `UpdateDocumentRequest`

``` purescript
newtype UpdateDocumentRequest
  = UpdateDocumentRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "DocumentId" :: ResourceIdType, "Name" :: NullOrUndefined (ResourceNameType), "ParentFolderId" :: NullOrUndefined (ResourceIdType), "ResourceState" :: NullOrUndefined (ResourceStateType) }
```

#### `UpdateDocumentVersionRequest`

``` purescript
newtype UpdateDocumentVersionRequest
  = UpdateDocumentVersionRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "DocumentId" :: ResourceIdType, "VersionId" :: DocumentVersionIdType, "VersionStatus" :: NullOrUndefined (DocumentVersionStatus) }
```

#### `UpdateFolderRequest`

``` purescript
newtype UpdateFolderRequest
  = UpdateFolderRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "FolderId" :: ResourceIdType, "Name" :: NullOrUndefined (ResourceNameType), "ParentFolderId" :: NullOrUndefined (ResourceIdType), "ResourceState" :: NullOrUndefined (ResourceStateType) }
```

#### `UpdateUserRequest`

``` purescript
newtype UpdateUserRequest
  = UpdateUserRequest { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType), "UserId" :: IdType, "GivenName" :: NullOrUndefined (UserAttributeValueType), "Surname" :: NullOrUndefined (UserAttributeValueType), "Type" :: NullOrUndefined (UserType), "StorageRule" :: NullOrUndefined (StorageRuleType), "TimeZoneId" :: NullOrUndefined (TimeZoneIdType), "Locale" :: NullOrUndefined (LocaleType), "GrantPoweruserPrivileges" :: NullOrUndefined (BooleanEnumType) }
```

#### `UpdateUserResponse`

``` purescript
newtype UpdateUserResponse
  = UpdateUserResponse { "User" :: NullOrUndefined (User) }
```

#### `UploadMetadata`

``` purescript
newtype UploadMetadata
  = UploadMetadata { "UploadUrl" :: NullOrUndefined (UrlType), "SignedHeaders" :: NullOrUndefined (SignedHeaderMap) }
```

<p>Describes the upload.</p>

#### `UrlType`

``` purescript
newtype UrlType
  = UrlType String
```

#### `User`

``` purescript
newtype User
  = User { "Id" :: NullOrUndefined (IdType), "Username" :: NullOrUndefined (UsernameType), "EmailAddress" :: NullOrUndefined (EmailAddressType), "GivenName" :: NullOrUndefined (UserAttributeValueType), "Surname" :: NullOrUndefined (UserAttributeValueType), "OrganizationId" :: NullOrUndefined (IdType), "RootFolderId" :: NullOrUndefined (ResourceIdType), "RecycleBinFolderId" :: NullOrUndefined (ResourceIdType), "Status" :: NullOrUndefined (UserStatusType), "Type" :: NullOrUndefined (UserType), "CreatedTimestamp" :: NullOrUndefined (TimestampType), "ModifiedTimestamp" :: NullOrUndefined (TimestampType), "TimeZoneId" :: NullOrUndefined (TimeZoneIdType), "Locale" :: NullOrUndefined (LocaleType), "Storage" :: NullOrUndefined (UserStorageMetadata) }
```

<p>Describes a user.</p>

#### `UserActivities`

``` purescript
newtype UserActivities
  = UserActivities (Array Activity)
```

#### `UserAttributeValueType`

``` purescript
newtype UserAttributeValueType
  = UserAttributeValueType String
```

#### `UserFilterType`

``` purescript
newtype UserFilterType
  = UserFilterType String
```

#### `UserIdsType`

``` purescript
newtype UserIdsType
  = UserIdsType String
```

#### `UserMetadata`

``` purescript
newtype UserMetadata
  = UserMetadata { "Id" :: NullOrUndefined (IdType), "Username" :: NullOrUndefined (UsernameType), "GivenName" :: NullOrUndefined (UserAttributeValueType), "Surname" :: NullOrUndefined (UserAttributeValueType), "EmailAddress" :: NullOrUndefined (EmailAddressType) }
```

<p>Describes the metadata of the user.</p>

#### `UserMetadataList`

``` purescript
newtype UserMetadataList
  = UserMetadataList (Array UserMetadata)
```

#### `UserSortType`

``` purescript
newtype UserSortType
  = UserSortType String
```

#### `UserStatusType`

``` purescript
newtype UserStatusType
  = UserStatusType String
```

#### `UserStorageMetadata`

``` purescript
newtype UserStorageMetadata
  = UserStorageMetadata { "StorageUtilizedInBytes" :: NullOrUndefined (SizeType), "StorageRule" :: NullOrUndefined (StorageRuleType) }
```

<p>Describes the storage for a user.</p>

#### `UserType`

``` purescript
newtype UserType
  = UserType String
```

#### `UsernameType`

``` purescript
newtype UsernameType
  = UsernameType String
```


