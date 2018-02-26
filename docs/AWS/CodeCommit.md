## Module AWS.CodeCommit

<fullname>AWS CodeCommit</fullname> <p>This is the <i>AWS CodeCommit API Reference</i>. This reference provides descriptions of the operations and data types for AWS CodeCommit API along with usage examples.</p> <p>You can use the AWS CodeCommit API to work with the following objects:</p> <p>Repositories, by calling the following:</p> <ul> <li> <p> <a>BatchGetRepositories</a>, which returns information about one or more repositories associated with your AWS account.</p> </li> <li> <p> <a>CreateRepository</a>, which creates an AWS CodeCommit repository.</p> </li> <li> <p> <a>DeleteRepository</a>, which deletes an AWS CodeCommit repository.</p> </li> <li> <p> <a>GetRepository</a>, which returns information about a specified repository.</p> </li> <li> <p> <a>ListRepositories</a>, which lists all AWS CodeCommit repositories associated with your AWS account.</p> </li> <li> <p> <a>UpdateRepositoryDescription</a>, which sets or updates the description of the repository.</p> </li> <li> <p> <a>UpdateRepositoryName</a>, which changes the name of the repository. If you change the name of a repository, no other users of that repository will be able to access it until you send them the new HTTPS or SSH URL to use.</p> </li> </ul> <p>Branches, by calling the following:</p> <ul> <li> <p> <a>CreateBranch</a>, which creates a new branch in a specified repository.</p> </li> <li> <p> <a>DeleteBranch</a>, which deletes the specified branch in a repository unless it is the default branch.</p> </li> <li> <p> <a>GetBranch</a>, which returns information about a specified branch.</p> </li> <li> <p> <a>ListBranches</a>, which lists all branches for a specified repository.</p> </li> <li> <p> <a>UpdateDefaultBranch</a>, which changes the default branch for a repository.</p> </li> </ul> <p>Files, by calling the following:</p> <ul> <li> <p> <a>PutFile</a>, which adds or modifies a file in a specified repository and branch.</p> </li> </ul> <p>Information about committed code in a repository, by calling the following:</p> <ul> <li> <p> <a>GetBlob</a>, which returns the base-64 encoded content of an individual Git blob object within a repository.</p> </li> <li> <p> <a>GetCommit</a>, which returns information about a commit, including commit messages and author and committer information.</p> </li> <li> <p> <a>GetDifferences</a>, which returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID or other fully qualified reference).</p> </li> </ul> <p>Pull requests, by calling the following:</p> <ul> <li> <p> <a>CreatePullRequest</a>, which creates a pull request in a specified repository.</p> </li> <li> <p> <a>DescribePullRequestEvents</a>, which returns information about one or more pull request events.</p> </li> <li> <p> <a>GetCommentsForPullRequest</a>, which returns information about comments on a specified pull request.</p> </li> <li> <p> <a>GetMergeConflicts</a>, which returns information about merge conflicts between the source and destination branch in a pull request.</p> </li> <li> <p> <a>GetPullRequest</a>, which returns information about a specified pull request.</p> </li> <li> <p> <a>ListPullRequests</a>, which lists all pull requests for a repository.</p> </li> <li> <p> <a>MergePullRequestByFastForward</a>, which merges the source destination branch of a pull request into the specified destination branch for that pull request using the fast-forward merge option.</p> </li> <li> <p> <a>PostCommentForPullRequest</a>, which posts a comment to a pull request at the specified line, file, or request.</p> </li> <li> <p> <a>UpdatePullRequestDescription</a>, which updates the description of a pull request.</p> </li> <li> <p> <a>UpdatePullRequestStatus</a>, which updates the status of a pull request.</p> </li> <li> <p> <a>UpdatePullRequestTitle</a>, which updates the title of a pull request.</p> </li> </ul> <p>Information about comments in a repository, by calling the following:</p> <ul> <li> <p> <a>DeleteCommentContent</a>, which deletes the content of a comment on a commit in a repository.</p> </li> <li> <p> <a>GetComment</a>, which returns information about a comment on a commit.</p> </li> <li> <p> <a>GetCommentsForComparedCommit</a>, which returns information about comments on the comparison between two commit specifiers in a repository.</p> </li> <li> <p> <a>PostCommentForComparedCommit</a>, which creates a comment on the comparison between two commit specifiers in a repository.</p> </li> <li> <p> <a>PostCommentReply</a>, which creates a reply to a comment.</p> </li> <li> <p> <a>UpdateComment</a>, which updates the content of a comment on a commit in a repository.</p> </li> </ul> <p>Triggers, by calling the following:</p> <ul> <li> <p> <a>GetRepositoryTriggers</a>, which returns information about triggers configured for a repository.</p> </li> <li> <p> <a>PutRepositoryTriggers</a>, which replaces all triggers for a repository and can be used to create or delete triggers.</p> </li> <li> <p> <a>TestRepositoryTriggers</a>, which tests the functionality of a repository trigger by sending data to the trigger target.</p> </li> </ul> <p>For information about how to use AWS CodeCommit, see the <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html">AWS CodeCommit User Guide</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `batchGetRepositories`

``` purescript
batchGetRepositories :: forall eff. BatchGetRepositoriesInput -> Aff (err :: RequestError | eff) BatchGetRepositoriesOutput
```

<p>Returns information about one or more repositories.</p> <note> <p>The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.</p> </note>

#### `createBranch`

``` purescript
createBranch :: forall eff. CreateBranchInput -> Aff (err :: RequestError | eff) Unit
```

<p>Creates a new branch in a repository and points the branch to a commit.</p> <note> <p>Calling the create branch operation does not set a repository's default branch. To do this, call the update default branch operation.</p> </note>

#### `createPullRequest`

``` purescript
createPullRequest :: forall eff. CreatePullRequestInput -> Aff (err :: RequestError | eff) CreatePullRequestOutput
```

<p>Creates a pull request in the specified repository.</p>

#### `createRepository`

``` purescript
createRepository :: forall eff. CreateRepositoryInput -> Aff (err :: RequestError | eff) CreateRepositoryOutput
```

<p>Creates a new, empty repository.</p>

#### `deleteBranch`

``` purescript
deleteBranch :: forall eff. DeleteBranchInput -> Aff (err :: RequestError | eff) DeleteBranchOutput
```

<p>Deletes a branch from a repository, unless that branch is the default branch for the repository. </p>

#### `deleteCommentContent`

``` purescript
deleteCommentContent :: forall eff. DeleteCommentContentInput -> Aff (err :: RequestError | eff) DeleteCommentContentOutput
```

<p>Deletes the content of a comment made on a change, file, or commit in a repository.</p>

#### `deleteRepository`

``` purescript
deleteRepository :: forall eff. DeleteRepositoryInput -> Aff (err :: RequestError | eff) DeleteRepositoryOutput
```

<p>Deletes a repository. If a specified repository was already deleted, a null repository ID will be returned.</p> <important> <p>Deleting a repository also deletes all associated objects and metadata. After a repository is deleted, all future push calls to the deleted repository will fail.</p> </important>

#### `describePullRequestEvents`

``` purescript
describePullRequestEvents :: forall eff. DescribePullRequestEventsInput -> Aff (err :: RequestError | eff) DescribePullRequestEventsOutput
```

<p>Returns information about one or more pull request events.</p>

#### `getBlob`

``` purescript
getBlob :: forall eff. GetBlobInput -> Aff (err :: RequestError | eff) GetBlobOutput
```

<p>Returns the base-64 encoded content of an individual blob within a repository.</p>

#### `getBranch`

``` purescript
getBranch :: forall eff. GetBranchInput -> Aff (err :: RequestError | eff) GetBranchOutput
```

<p>Returns information about a repository branch, including its name and the last commit ID.</p>

#### `getComment`

``` purescript
getComment :: forall eff. GetCommentInput -> Aff (err :: RequestError | eff) GetCommentOutput
```

<p>Returns the content of a comment made on a change, file, or commit in a repository.</p>

#### `getCommentsForComparedCommit`

``` purescript
getCommentsForComparedCommit :: forall eff. GetCommentsForComparedCommitInput -> Aff (err :: RequestError | eff) GetCommentsForComparedCommitOutput
```

<p>Returns information about comments made on the comparison between two commits.</p>

#### `getCommentsForPullRequest`

``` purescript
getCommentsForPullRequest :: forall eff. GetCommentsForPullRequestInput -> Aff (err :: RequestError | eff) GetCommentsForPullRequestOutput
```

<p>Returns comments made on a pull request.</p>

#### `getCommit`

``` purescript
getCommit :: forall eff. GetCommitInput -> Aff (err :: RequestError | eff) GetCommitOutput
```

<p>Returns information about a commit, including commit message and committer information.</p>

#### `getDifferences`

``` purescript
getDifferences :: forall eff. GetDifferencesInput -> Aff (err :: RequestError | eff) GetDifferencesOutput
```

<p>Returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID or other fully qualified reference). Results can be limited to a specified path.</p>

#### `getMergeConflicts`

``` purescript
getMergeConflicts :: forall eff. GetMergeConflictsInput -> Aff (err :: RequestError | eff) GetMergeConflictsOutput
```

<p>Returns information about merge conflicts between the before and after commit IDs for a pull request in a repository.</p>

#### `getPullRequest`

``` purescript
getPullRequest :: forall eff. GetPullRequestInput -> Aff (err :: RequestError | eff) GetPullRequestOutput
```

<p>Gets information about a pull request in a specified repository.</p>

#### `getRepository`

``` purescript
getRepository :: forall eff. GetRepositoryInput -> Aff (err :: RequestError | eff) GetRepositoryOutput
```

<p>Returns information about a repository.</p> <note> <p>The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.</p> </note>

#### `getRepositoryTriggers`

``` purescript
getRepositoryTriggers :: forall eff. GetRepositoryTriggersInput -> Aff (err :: RequestError | eff) GetRepositoryTriggersOutput
```

<p>Gets information about triggers configured for a repository.</p>

#### `listBranches`

``` purescript
listBranches :: forall eff. ListBranchesInput -> Aff (err :: RequestError | eff) ListBranchesOutput
```

<p>Gets information about one or more branches in a repository.</p>

#### `listPullRequests`

``` purescript
listPullRequests :: forall eff. ListPullRequestsInput -> Aff (err :: RequestError | eff) ListPullRequestsOutput
```

<p>Returns a list of pull requests for a specified repository. The return list can be refined by pull request status or pull request author ARN.</p>

#### `listRepositories`

``` purescript
listRepositories :: forall eff. ListRepositoriesInput -> Aff (err :: RequestError | eff) ListRepositoriesOutput
```

<p>Gets information about one or more repositories.</p>

#### `mergePullRequestByFastForward`

``` purescript
mergePullRequestByFastForward :: forall eff. MergePullRequestByFastForwardInput -> Aff (err :: RequestError | eff) MergePullRequestByFastForwardOutput
```

<p>Closes a pull request and attempts to merge the source commit of a pull request into the specified destination branch for that pull request at the specified commit using the fast-forward merge option.</p>

#### `postCommentForComparedCommit`

``` purescript
postCommentForComparedCommit :: forall eff. PostCommentForComparedCommitInput -> Aff (err :: RequestError | eff) PostCommentForComparedCommitOutput
```

<p>Posts a comment on the comparison between two commits.</p>

#### `postCommentForPullRequest`

``` purescript
postCommentForPullRequest :: forall eff. PostCommentForPullRequestInput -> Aff (err :: RequestError | eff) PostCommentForPullRequestOutput
```

<p>Posts a comment on a pull request.</p>

#### `postCommentReply`

``` purescript
postCommentReply :: forall eff. PostCommentReplyInput -> Aff (err :: RequestError | eff) PostCommentReplyOutput
```

<p>Posts a comment in reply to an existing comment on a comparison between commits or a pull request.</p>

#### `putFile`

``` purescript
putFile :: forall eff. PutFileInput -> Aff (err :: RequestError | eff) PutFileOutput
```

<p>Adds or updates a file in an AWS CodeCommit repository.</p>

#### `putRepositoryTriggers`

``` purescript
putRepositoryTriggers :: forall eff. PutRepositoryTriggersInput -> Aff (err :: RequestError | eff) PutRepositoryTriggersOutput
```

<p>Replaces all triggers for a repository. This can be used to create or delete triggers.</p>

#### `testRepositoryTriggers`

``` purescript
testRepositoryTriggers :: forall eff. TestRepositoryTriggersInput -> Aff (err :: RequestError | eff) TestRepositoryTriggersOutput
```

<p>Tests the functionality of repository triggers by sending information to the trigger target. If real data is available in the repository, the test will send data from the last commit. If no data is available, sample data will be generated.</p>

#### `updateComment`

``` purescript
updateComment :: forall eff. UpdateCommentInput -> Aff (err :: RequestError | eff) UpdateCommentOutput
```

<p>Replaces the contents of a comment.</p>

#### `updateDefaultBranch`

``` purescript
updateDefaultBranch :: forall eff. UpdateDefaultBranchInput -> Aff (err :: RequestError | eff) Unit
```

<p>Sets or changes the default branch name for the specified repository.</p> <note> <p>If you use this operation to change the default branch name to the current default branch name, a success message is returned even though the default branch did not change.</p> </note>

#### `updatePullRequestDescription`

``` purescript
updatePullRequestDescription :: forall eff. UpdatePullRequestDescriptionInput -> Aff (err :: RequestError | eff) UpdatePullRequestDescriptionOutput
```

<p>Replaces the contents of the description of a pull request.</p>

#### `updatePullRequestStatus`

``` purescript
updatePullRequestStatus :: forall eff. UpdatePullRequestStatusInput -> Aff (err :: RequestError | eff) UpdatePullRequestStatusOutput
```

<p>Updates the status of a pull request. </p>

#### `updatePullRequestTitle`

``` purescript
updatePullRequestTitle :: forall eff. UpdatePullRequestTitleInput -> Aff (err :: RequestError | eff) UpdatePullRequestTitleOutput
```

<p>Replaces the title of a pull request.</p>

#### `updateRepositoryDescription`

``` purescript
updateRepositoryDescription :: forall eff. UpdateRepositoryDescriptionInput -> Aff (err :: RequestError | eff) Unit
```

<p>Sets or changes the comment or description for a repository.</p> <note> <p>The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.</p> </note>

#### `updateRepositoryName`

``` purescript
updateRepositoryName :: forall eff. UpdateRepositoryNameInput -> Aff (err :: RequestError | eff) Unit
```

<p>Renames a repository. The repository name must be unique across the calling AWS account. In addition, repository names are limited to 100 alphanumeric, dash, and underscore characters, and cannot include certain characters. The suffix ".git" is prohibited. For a full description of the limits on repository names, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/limits.html">Limits</a> in the AWS CodeCommit User Guide.</p>

#### `AccountId`

``` purescript
newtype AccountId
  = AccountId String
```

#### `ActorDoesNotExistException`

``` purescript
newtype ActorDoesNotExistException
  = ActorDoesNotExistException {  }
```

<p>The specified Amazon Resource Name (ARN) does not exist in the AWS account.</p>

#### `AdditionalData`

``` purescript
newtype AdditionalData
  = AdditionalData String
```

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

#### `AuthorDoesNotExistException`

``` purescript
newtype AuthorDoesNotExistException
  = AuthorDoesNotExistException {  }
```

<p>The specified Amazon Resource Name (ARN) does not exist in the AWS account.</p>

#### `BatchGetRepositoriesInput`

``` purescript
newtype BatchGetRepositoriesInput
  = BatchGetRepositoriesInput { "RepositoryNames'" :: RepositoryNameList }
```

<p>Represents the input of a batch get repositories operation.</p>

#### `BatchGetRepositoriesOutput`

``` purescript
newtype BatchGetRepositoriesOutput
  = BatchGetRepositoriesOutput { "Repositories'" :: NullOrUndefined (RepositoryMetadataList), "RepositoriesNotFound'" :: NullOrUndefined (RepositoryNotFoundList) }
```

<p>Represents the output of a batch get repositories operation.</p>

#### `BeforeCommitIdAndAfterCommitIdAreSameException`

``` purescript
newtype BeforeCommitIdAndAfterCommitIdAreSameException
  = BeforeCommitIdAndAfterCommitIdAreSameException {  }
```

<p>The before commit ID and the after commit ID are the same, which is not valid. The before commit ID and the after commit ID must be different commit IDs.</p>

#### `BlobIdDoesNotExistException`

``` purescript
newtype BlobIdDoesNotExistException
  = BlobIdDoesNotExistException {  }
```

<p>The specified blob does not exist.</p>

#### `BlobIdRequiredException`

``` purescript
newtype BlobIdRequiredException
  = BlobIdRequiredException {  }
```

<p>A blob ID is required but was not specified.</p>

#### `BlobMetadata`

``` purescript
newtype BlobMetadata
  = BlobMetadata { "BlobId'" :: NullOrUndefined (ObjectId), "Path'" :: NullOrUndefined (Path), "Mode'" :: NullOrUndefined (Mode) }
```

<p>Returns information about a specific Git blob object.</p>

#### `BranchDoesNotExistException`

``` purescript
newtype BranchDoesNotExistException
  = BranchDoesNotExistException {  }
```

<p>The specified branch does not exist.</p>

#### `BranchInfo`

``` purescript
newtype BranchInfo
  = BranchInfo { "BranchName'" :: NullOrUndefined (BranchName), "CommitId'" :: NullOrUndefined (CommitId) }
```

<p>Returns information about a branch.</p>

#### `BranchName`

``` purescript
newtype BranchName
  = BranchName String
```

#### `BranchNameExistsException`

``` purescript
newtype BranchNameExistsException
  = BranchNameExistsException {  }
```

<p>The specified branch name already exists.</p>

#### `BranchNameIsTagNameException`

``` purescript
newtype BranchNameIsTagNameException
  = BranchNameIsTagNameException {  }
```

<p>The specified branch name is not valid because it is a tag name. Type the name of a current branch in the repository. For a list of valid branch names, use <a>ListBranches</a>.</p>

#### `BranchNameList`

``` purescript
newtype BranchNameList
  = BranchNameList (Array BranchName)
```

#### `BranchNameRequiredException`

``` purescript
newtype BranchNameRequiredException
  = BranchNameRequiredException {  }
```

<p>A branch name is required but was not specified.</p>

#### `ChangeTypeEnum`

``` purescript
newtype ChangeTypeEnum
  = ChangeTypeEnum String
```

#### `ClientRequestToken`

``` purescript
newtype ClientRequestToken
  = ClientRequestToken String
```

#### `ClientRequestTokenRequiredException`

``` purescript
newtype ClientRequestTokenRequiredException
  = ClientRequestTokenRequiredException {  }
```

<p>A client request token is required. A client request token is an unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.</p>

#### `CloneUrlHttp`

``` purescript
newtype CloneUrlHttp
  = CloneUrlHttp String
```

#### `CloneUrlSsh`

``` purescript
newtype CloneUrlSsh
  = CloneUrlSsh String
```

#### `Comment`

``` purescript
newtype Comment
  = Comment { "CommentId'" :: NullOrUndefined (CommentId), "Content'" :: NullOrUndefined (Content), "InReplyTo'" :: NullOrUndefined (CommentId), "CreationDate'" :: NullOrUndefined (CreationDate), "LastModifiedDate'" :: NullOrUndefined (LastModifiedDate), "AuthorArn'" :: NullOrUndefined (Arn), "Deleted'" :: NullOrUndefined (IsCommentDeleted), "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken) }
```

<p>Returns information about a specific comment.</p>

#### `CommentContentRequiredException`

``` purescript
newtype CommentContentRequiredException
  = CommentContentRequiredException {  }
```

<p>The comment is empty. You must provide some content for a comment. The content cannot be null.</p>

#### `CommentContentSizeLimitExceededException`

``` purescript
newtype CommentContentSizeLimitExceededException
  = CommentContentSizeLimitExceededException {  }
```

<p>The comment is too large. Comments are limited to 1,000 characters.</p>

#### `CommentDeletedException`

``` purescript
newtype CommentDeletedException
  = CommentDeletedException {  }
```

<p>This comment has already been deleted. You cannot edit or delete a deleted comment.</p>

#### `CommentDoesNotExistException`

``` purescript
newtype CommentDoesNotExistException
  = CommentDoesNotExistException {  }
```

<p>No comment exists with the provided ID. Verify that you have provided the correct ID, and then try again.</p>

#### `CommentId`

``` purescript
newtype CommentId
  = CommentId String
```

#### `CommentIdRequiredException`

``` purescript
newtype CommentIdRequiredException
  = CommentIdRequiredException {  }
```

<p>The comment ID is missing or null. A comment ID is required.</p>

#### `CommentNotCreatedByCallerException`

``` purescript
newtype CommentNotCreatedByCallerException
  = CommentNotCreatedByCallerException {  }
```

<p>You cannot modify or delete this comment. Only comment authors can modify or delete their comments.</p>

#### `Comments`

``` purescript
newtype Comments
  = Comments (Array Comment)
```

#### `CommentsForComparedCommit`

``` purescript
newtype CommentsForComparedCommit
  = CommentsForComparedCommit { "RepositoryName'" :: NullOrUndefined (RepositoryName), "BeforeCommitId'" :: NullOrUndefined (CommitId), "AfterCommitId'" :: NullOrUndefined (CommitId), "BeforeBlobId'" :: NullOrUndefined (ObjectId), "AfterBlobId'" :: NullOrUndefined (ObjectId), "Location'" :: NullOrUndefined (Location), "Comments'" :: NullOrUndefined (Comments) }
```

<p>Returns information about comments on the comparison between two commits.</p>

#### `CommentsForComparedCommitData`

``` purescript
newtype CommentsForComparedCommitData
  = CommentsForComparedCommitData (Array CommentsForComparedCommit)
```

#### `CommentsForPullRequest`

``` purescript
newtype CommentsForPullRequest
  = CommentsForPullRequest { "PullRequestId'" :: NullOrUndefined (PullRequestId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "BeforeCommitId'" :: NullOrUndefined (CommitId), "AfterCommitId'" :: NullOrUndefined (CommitId), "BeforeBlobId'" :: NullOrUndefined (ObjectId), "AfterBlobId'" :: NullOrUndefined (ObjectId), "Location'" :: NullOrUndefined (Location), "Comments'" :: NullOrUndefined (Comments) }
```

<p>Returns information about comments on a pull request.</p>

#### `CommentsForPullRequestData`

``` purescript
newtype CommentsForPullRequestData
  = CommentsForPullRequestData (Array CommentsForPullRequest)
```

#### `Commit`

``` purescript
newtype Commit
  = Commit { "CommitId'" :: NullOrUndefined (ObjectId), "TreeId'" :: NullOrUndefined (ObjectId), "Parents'" :: NullOrUndefined (ParentList), "Message'" :: NullOrUndefined (Message), "Author'" :: NullOrUndefined (UserInfo), "Committer'" :: NullOrUndefined (UserInfo), "AdditionalData'" :: NullOrUndefined (AdditionalData) }
```

<p>Returns information about a specific commit.</p>

#### `CommitDoesNotExistException`

``` purescript
newtype CommitDoesNotExistException
  = CommitDoesNotExistException {  }
```

<p>The specified commit does not exist or no commit was specified, and the specified repository has no default branch.</p>

#### `CommitId`

``` purescript
newtype CommitId
  = CommitId String
```

#### `CommitIdDoesNotExistException`

``` purescript
newtype CommitIdDoesNotExistException
  = CommitIdDoesNotExistException {  }
```

<p>The specified commit ID does not exist.</p>

#### `CommitIdRequiredException`

``` purescript
newtype CommitIdRequiredException
  = CommitIdRequiredException {  }
```

<p>A commit ID was not specified.</p>

#### `CommitMessageLengthExceededException`

``` purescript
newtype CommitMessageLengthExceededException
  = CommitMessageLengthExceededException {  }
```

<p>The commit message is too long. Provide a shorter string. </p>

#### `CommitName`

``` purescript
newtype CommitName
  = CommitName String
```

#### `CommitRequiredException`

``` purescript
newtype CommitRequiredException
  = CommitRequiredException {  }
```

<p>A commit was not specified.</p>

#### `Content`

``` purescript
newtype Content
  = Content String
```

#### `CreateBranchInput`

``` purescript
newtype CreateBranchInput
  = CreateBranchInput { "RepositoryName'" :: RepositoryName, "BranchName'" :: BranchName, "CommitId'" :: CommitId }
```

<p>Represents the input of a create branch operation.</p>

#### `CreatePullRequestInput`

``` purescript
newtype CreatePullRequestInput
  = CreatePullRequestInput { "Title'" :: Title, "Description'" :: NullOrUndefined (Description), "Targets'" :: TargetList, "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken) }
```

#### `CreatePullRequestOutput`

``` purescript
newtype CreatePullRequestOutput
  = CreatePullRequestOutput { "PullRequest'" :: PullRequest }
```

#### `CreateRepositoryInput`

``` purescript
newtype CreateRepositoryInput
  = CreateRepositoryInput { "RepositoryName'" :: RepositoryName, "RepositoryDescription'" :: NullOrUndefined (RepositoryDescription) }
```

<p>Represents the input of a create repository operation.</p>

#### `CreateRepositoryOutput`

``` purescript
newtype CreateRepositoryOutput
  = CreateRepositoryOutput { "RepositoryMetadata'" :: NullOrUndefined (RepositoryMetadata) }
```

<p>Represents the output of a create repository operation.</p>

#### `CreationDate`

``` purescript
newtype CreationDate
  = CreationDate Number
```

#### `Date`

``` purescript
newtype Date
  = Date String
```

#### `DefaultBranchCannotBeDeletedException`

``` purescript
newtype DefaultBranchCannotBeDeletedException
  = DefaultBranchCannotBeDeletedException {  }
```

<p>The specified branch is the default branch for the repository, and cannot be deleted. To delete this branch, you must first set another branch as the default branch.</p>

#### `DeleteBranchInput`

``` purescript
newtype DeleteBranchInput
  = DeleteBranchInput { "RepositoryName'" :: RepositoryName, "BranchName'" :: BranchName }
```

<p>Represents the input of a delete branch operation.</p>

#### `DeleteBranchOutput`

``` purescript
newtype DeleteBranchOutput
  = DeleteBranchOutput { "DeletedBranch'" :: NullOrUndefined (BranchInfo) }
```

<p>Represents the output of a delete branch operation.</p>

#### `DeleteCommentContentInput`

``` purescript
newtype DeleteCommentContentInput
  = DeleteCommentContentInput { "CommentId'" :: CommentId }
```

#### `DeleteCommentContentOutput`

``` purescript
newtype DeleteCommentContentOutput
  = DeleteCommentContentOutput { "Comment'" :: NullOrUndefined (Comment) }
```

#### `DeleteRepositoryInput`

``` purescript
newtype DeleteRepositoryInput
  = DeleteRepositoryInput { "RepositoryName'" :: RepositoryName }
```

<p>Represents the input of a delete repository operation.</p>

#### `DeleteRepositoryOutput`

``` purescript
newtype DeleteRepositoryOutput
  = DeleteRepositoryOutput { "RepositoryId'" :: NullOrUndefined (RepositoryId) }
```

<p>Represents the output of a delete repository operation.</p>

#### `DescribePullRequestEventsInput`

``` purescript
newtype DescribePullRequestEventsInput
  = DescribePullRequestEventsInput { "PullRequestId'" :: PullRequestId, "PullRequestEventType'" :: NullOrUndefined (PullRequestEventType), "ActorArn'" :: NullOrUndefined (Arn), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `DescribePullRequestEventsOutput`

``` purescript
newtype DescribePullRequestEventsOutput
  = DescribePullRequestEventsOutput { "PullRequestEvents'" :: PullRequestEventList, "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

#### `Difference`

``` purescript
newtype Difference
  = Difference { "BeforeBlob'" :: NullOrUndefined (BlobMetadata), "AfterBlob'" :: NullOrUndefined (BlobMetadata), "ChangeType'" :: NullOrUndefined (ChangeTypeEnum) }
```

<p>Returns information about a set of differences for a commit specifier.</p>

#### `DifferenceList`

``` purescript
newtype DifferenceList
  = DifferenceList (Array Difference)
```

#### `DirectoryNameConflictsWithFileNameException`

``` purescript
newtype DirectoryNameConflictsWithFileNameException
  = DirectoryNameConflictsWithFileNameException {  }
```

<p>A file cannot be added to the repository because the specified path name has the same name as a file that already exists in this repository. Either provide a different name for the file, or specify a different path for the file.</p>

#### `Email`

``` purescript
newtype Email
  = Email String
```

#### `EncryptionIntegrityChecksFailedException`

``` purescript
newtype EncryptionIntegrityChecksFailedException
  = EncryptionIntegrityChecksFailedException {  }
```

<p>An encryption integrity check failed.</p>

#### `EncryptionKeyAccessDeniedException`

``` purescript
newtype EncryptionKeyAccessDeniedException
  = EncryptionKeyAccessDeniedException {  }
```

<p>An encryption key could not be accessed.</p>

#### `EncryptionKeyDisabledException`

``` purescript
newtype EncryptionKeyDisabledException
  = EncryptionKeyDisabledException {  }
```

<p>The encryption key is disabled.</p>

#### `EncryptionKeyNotFoundException`

``` purescript
newtype EncryptionKeyNotFoundException
  = EncryptionKeyNotFoundException {  }
```

<p>No encryption key was found.</p>

#### `EncryptionKeyUnavailableException`

``` purescript
newtype EncryptionKeyUnavailableException
  = EncryptionKeyUnavailableException {  }
```

<p>The encryption key is not available.</p>

#### `EventDate`

``` purescript
newtype EventDate
  = EventDate Number
```

#### `FileContent`

``` purescript
newtype FileContent
  = FileContent String
```

#### `FileContentRequiredException`

``` purescript
newtype FileContentRequiredException
  = FileContentRequiredException {  }
```

<p>The file cannot be added because it is empty. Empty files cannot be added to the repository with this API.</p>

#### `FileContentSizeLimitExceededException`

``` purescript
newtype FileContentSizeLimitExceededException
  = FileContentSizeLimitExceededException {  }
```

<p>The file cannot be added because it is too large. The maximum file size that can be added using PutFile is 6 MB. For files larger than 6 MB but smaller than 2 GB, add them using a Git client.</p>

#### `FileModeTypeEnum`

``` purescript
newtype FileModeTypeEnum
  = FileModeTypeEnum String
```

#### `FileNameConflictsWithDirectoryNameException`

``` purescript
newtype FileNameConflictsWithDirectoryNameException
  = FileNameConflictsWithDirectoryNameException {  }
```

<p>A file cannot be added to the repository because the specified file name has the same name as a directory in this repository. Either provide another name for the file, or add the file in a directory that does not match the file name.</p>

#### `FileTooLargeException`

``` purescript
newtype FileTooLargeException
  = FileTooLargeException {  }
```

<p>The specified file exceeds the file size limit for AWS CodeCommit. For more information about limits in AWS CodeCommit, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/limits.html">AWS CodeCommit User Guide</a>.</p>

#### `GetBlobInput`

``` purescript
newtype GetBlobInput
  = GetBlobInput { "RepositoryName'" :: RepositoryName, "BlobId'" :: ObjectId }
```

<p>Represents the input of a get blob operation.</p>

#### `GetBlobOutput`

``` purescript
newtype GetBlobOutput
  = GetBlobOutput { "Content'" :: String }
```

<p>Represents the output of a get blob operation.</p>

#### `GetBranchInput`

``` purescript
newtype GetBranchInput
  = GetBranchInput { "RepositoryName'" :: NullOrUndefined (RepositoryName), "BranchName'" :: NullOrUndefined (BranchName) }
```

<p>Represents the input of a get branch operation.</p>

#### `GetBranchOutput`

``` purescript
newtype GetBranchOutput
  = GetBranchOutput { "Branch'" :: NullOrUndefined (BranchInfo) }
```

<p>Represents the output of a get branch operation.</p>

#### `GetCommentInput`

``` purescript
newtype GetCommentInput
  = GetCommentInput { "CommentId'" :: CommentId }
```

#### `GetCommentOutput`

``` purescript
newtype GetCommentOutput
  = GetCommentOutput { "Comment'" :: NullOrUndefined (Comment) }
```

#### `GetCommentsForComparedCommitInput`

``` purescript
newtype GetCommentsForComparedCommitInput
  = GetCommentsForComparedCommitInput { "RepositoryName'" :: RepositoryName, "BeforeCommitId'" :: NullOrUndefined (CommitId), "AfterCommitId'" :: CommitId, "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `GetCommentsForComparedCommitOutput`

``` purescript
newtype GetCommentsForComparedCommitOutput
  = GetCommentsForComparedCommitOutput { "CommentsForComparedCommitData'" :: NullOrUndefined (CommentsForComparedCommitData), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `GetCommentsForPullRequestInput`

``` purescript
newtype GetCommentsForPullRequestInput
  = GetCommentsForPullRequestInput { "PullRequestId'" :: PullRequestId, "RepositoryName'" :: NullOrUndefined (RepositoryName), "BeforeCommitId'" :: NullOrUndefined (CommitId), "AfterCommitId'" :: NullOrUndefined (CommitId), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `GetCommentsForPullRequestOutput`

``` purescript
newtype GetCommentsForPullRequestOutput
  = GetCommentsForPullRequestOutput { "CommentsForPullRequestData'" :: NullOrUndefined (CommentsForPullRequestData), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `GetCommitInput`

``` purescript
newtype GetCommitInput
  = GetCommitInput { "RepositoryName'" :: RepositoryName, "CommitId'" :: ObjectId }
```

<p>Represents the input of a get commit operation.</p>

#### `GetCommitOutput`

``` purescript
newtype GetCommitOutput
  = GetCommitOutput { "Commit'" :: Commit }
```

<p>Represents the output of a get commit operation.</p>

#### `GetDifferencesInput`

``` purescript
newtype GetDifferencesInput
  = GetDifferencesInput { "RepositoryName'" :: RepositoryName, "BeforeCommitSpecifier'" :: NullOrUndefined (CommitName), "AfterCommitSpecifier'" :: CommitName, "BeforePath'" :: NullOrUndefined (Path), "AfterPath'" :: NullOrUndefined (Path), "MaxResults" :: NullOrUndefined (Limit), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `GetDifferencesOutput`

``` purescript
newtype GetDifferencesOutput
  = GetDifferencesOutput { "Differences'" :: NullOrUndefined (DifferenceList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `GetMergeConflictsInput`

``` purescript
newtype GetMergeConflictsInput
  = GetMergeConflictsInput { "RepositoryName'" :: RepositoryName, "DestinationCommitSpecifier'" :: CommitName, "SourceCommitSpecifier'" :: CommitName, "MergeOption'" :: MergeOptionTypeEnum }
```

#### `GetMergeConflictsOutput`

``` purescript
newtype GetMergeConflictsOutput
  = GetMergeConflictsOutput { "Mergeable'" :: IsMergeable, "DestinationCommitId'" :: CommitId, "SourceCommitId'" :: CommitId }
```

#### `GetPullRequestInput`

``` purescript
newtype GetPullRequestInput
  = GetPullRequestInput { "PullRequestId'" :: PullRequestId }
```

#### `GetPullRequestOutput`

``` purescript
newtype GetPullRequestOutput
  = GetPullRequestOutput { "PullRequest'" :: PullRequest }
```

#### `GetRepositoryInput`

``` purescript
newtype GetRepositoryInput
  = GetRepositoryInput { "RepositoryName'" :: RepositoryName }
```

<p>Represents the input of a get repository operation.</p>

#### `GetRepositoryOutput`

``` purescript
newtype GetRepositoryOutput
  = GetRepositoryOutput { "RepositoryMetadata'" :: NullOrUndefined (RepositoryMetadata) }
```

<p>Represents the output of a get repository operation.</p>

#### `GetRepositoryTriggersInput`

``` purescript
newtype GetRepositoryTriggersInput
  = GetRepositoryTriggersInput { "RepositoryName'" :: RepositoryName }
```

<p>Represents the input of a get repository triggers operation.</p>

#### `GetRepositoryTriggersOutput`

``` purescript
newtype GetRepositoryTriggersOutput
  = GetRepositoryTriggersOutput { "ConfigurationId'" :: NullOrUndefined (RepositoryTriggersConfigurationId), "Triggers'" :: NullOrUndefined (RepositoryTriggersList) }
```

<p>Represents the output of a get repository triggers operation.</p>

#### `IdempotencyParameterMismatchException`

``` purescript
newtype IdempotencyParameterMismatchException
  = IdempotencyParameterMismatchException {  }
```

<p>The client request token is not valid. Either the token is not in a valid format, or the token has been used in a previous request and cannot be re-used.</p>

#### `InvalidActorArnException`

``` purescript
newtype InvalidActorArnException
  = InvalidActorArnException {  }
```

<p>The Amazon Resource Name (ARN) is not valid. Make sure that you have provided the full ARN for the user who initiated the change for the pull request, and then try again.</p>

#### `InvalidAuthorArnException`

``` purescript
newtype InvalidAuthorArnException
  = InvalidAuthorArnException {  }
```

<p>The Amazon Resource Name (ARN) is not valid. Make sure that you have provided the full ARN for the author of the pull request, and then try again.</p>

#### `InvalidBlobIdException`

``` purescript
newtype InvalidBlobIdException
  = InvalidBlobIdException {  }
```

<p>The specified blob is not valid.</p>

#### `InvalidBranchNameException`

``` purescript
newtype InvalidBranchNameException
  = InvalidBranchNameException {  }
```

<p>The specified reference name is not valid.</p>

#### `InvalidClientRequestTokenException`

``` purescript
newtype InvalidClientRequestTokenException
  = InvalidClientRequestTokenException {  }
```

<p>The client request token is not valid.</p>

#### `InvalidCommentIdException`

``` purescript
newtype InvalidCommentIdException
  = InvalidCommentIdException {  }
```

<p>The comment ID is not in a valid format. Make sure that you have provided the full comment ID.</p>

#### `InvalidCommitException`

``` purescript
newtype InvalidCommitException
  = InvalidCommitException {  }
```

<p>The specified commit is not valid.</p>

#### `InvalidCommitIdException`

``` purescript
newtype InvalidCommitIdException
  = InvalidCommitIdException {  }
```

<p>The specified commit ID is not valid.</p>

#### `InvalidContinuationTokenException`

``` purescript
newtype InvalidContinuationTokenException
  = InvalidContinuationTokenException {  }
```

<p>The specified continuation token is not valid.</p>

#### `InvalidDescriptionException`

``` purescript
newtype InvalidDescriptionException
  = InvalidDescriptionException {  }
```

<p>The pull request description is not valid. Descriptions are limited to 1,000 characters in length.</p>

#### `InvalidDestinationCommitSpecifierException`

``` purescript
newtype InvalidDestinationCommitSpecifierException
  = InvalidDestinationCommitSpecifierException {  }
```

<p>The destination commit specifier is not valid. You must provide a valid branch name, tag, or full commit ID. </p>

#### `InvalidEmailException`

``` purescript
newtype InvalidEmailException
  = InvalidEmailException {  }
```

<p>The specified email address either contains one or more characters that are not allowed, or it exceeds the maximum number of characters allowed for an email address.</p>

#### `InvalidFileLocationException`

``` purescript
newtype InvalidFileLocationException
  = InvalidFileLocationException {  }
```

<p>The location of the file is not valid. Make sure that you include the extension of the file as well as the file name.</p>

#### `InvalidFileModeException`

``` purescript
newtype InvalidFileModeException
  = InvalidFileModeException {  }
```

<p>The specified file mode permission is not valid. For a list of valid file mode permissions, see <a>PutFile</a>. </p>

#### `InvalidFilePositionException`

``` purescript
newtype InvalidFilePositionException
  = InvalidFilePositionException {  }
```

<p>The position is not valid. Make sure that the line number exists in the version of the file you want to comment on.</p>

#### `InvalidMaxResultsException`

``` purescript
newtype InvalidMaxResultsException
  = InvalidMaxResultsException {  }
```

<p>The specified number of maximum results is not valid.</p>

#### `InvalidMergeOptionException`

``` purescript
newtype InvalidMergeOptionException
  = InvalidMergeOptionException {  }
```

<p>The specified merge option is not valid. The only valid value is FAST_FORWARD_MERGE.</p>

#### `InvalidOrderException`

``` purescript
newtype InvalidOrderException
  = InvalidOrderException {  }
```

<p>The specified sort order is not valid.</p>

#### `InvalidParentCommitIdException`

``` purescript
newtype InvalidParentCommitIdException
  = InvalidParentCommitIdException {  }
```

<p>The parent commit ID is not valid. The commit ID cannot be empty, and must match the head commit ID for the branch of the repository where you want to add or update a file.</p>

#### `InvalidPathException`

``` purescript
newtype InvalidPathException
  = InvalidPathException {  }
```

<p>The specified path is not valid.</p>

#### `InvalidPullRequestEventTypeException`

``` purescript
newtype InvalidPullRequestEventTypeException
  = InvalidPullRequestEventTypeException {  }
```

<p>The pull request event type is not valid. </p>

#### `InvalidPullRequestIdException`

``` purescript
newtype InvalidPullRequestIdException
  = InvalidPullRequestIdException {  }
```

<p>The pull request ID is not valid. Make sure that you have provided the full ID and that the pull request is in the specified repository, and then try again.</p>

#### `InvalidPullRequestStatusException`

``` purescript
newtype InvalidPullRequestStatusException
  = InvalidPullRequestStatusException {  }
```

<p>The pull request status is not valid. The only valid values are <code>OPEN</code> and <code>CLOSED</code>.</p>

#### `InvalidPullRequestStatusUpdateException`

``` purescript
newtype InvalidPullRequestStatusUpdateException
  = InvalidPullRequestStatusUpdateException {  }
```

<p>The pull request status update is not valid. The only valid update is from <code>OPEN</code> to <code>CLOSED</code>.</p>

#### `InvalidReferenceNameException`

``` purescript
newtype InvalidReferenceNameException
  = InvalidReferenceNameException {  }
```

<p>The specified reference name format is not valid. Reference names must conform to the Git references format, for example refs/heads/master. For more information, see <a href="https://git-scm.com/book/en/v2/Git-Internals-Git-References">Git Internals - Git References</a> or consult your Git documentation.</p>

#### `InvalidRelativeFileVersionEnumException`

``` purescript
newtype InvalidRelativeFileVersionEnumException
  = InvalidRelativeFileVersionEnumException {  }
```

<p>Either the enum is not in a valid format, or the specified file version enum is not valid in respect to the current file version.</p>

#### `InvalidRepositoryDescriptionException`

``` purescript
newtype InvalidRepositoryDescriptionException
  = InvalidRepositoryDescriptionException {  }
```

<p>The specified repository description is not valid.</p>

#### `InvalidRepositoryNameException`

``` purescript
newtype InvalidRepositoryNameException
  = InvalidRepositoryNameException {  }
```

<p>At least one specified repository name is not valid.</p> <note> <p>This exception only occurs when a specified repository name is not valid. Other exceptions occur when a required repository parameter is missing, or when a specified repository does not exist.</p> </note>

#### `InvalidRepositoryTriggerBranchNameException`

``` purescript
newtype InvalidRepositoryTriggerBranchNameException
  = InvalidRepositoryTriggerBranchNameException {  }
```

<p>One or more branch names specified for the trigger is not valid.</p>

#### `InvalidRepositoryTriggerCustomDataException`

``` purescript
newtype InvalidRepositoryTriggerCustomDataException
  = InvalidRepositoryTriggerCustomDataException {  }
```

<p>The custom data provided for the trigger is not valid.</p>

#### `InvalidRepositoryTriggerDestinationArnException`

``` purescript
newtype InvalidRepositoryTriggerDestinationArnException
  = InvalidRepositoryTriggerDestinationArnException {  }
```

<p>The Amazon Resource Name (ARN) for the trigger is not valid for the specified destination. The most common reason for this error is that the ARN does not meet the requirements for the service type.</p>

#### `InvalidRepositoryTriggerEventsException`

``` purescript
newtype InvalidRepositoryTriggerEventsException
  = InvalidRepositoryTriggerEventsException {  }
```

<p>One or more events specified for the trigger is not valid. Check to make sure that all events specified match the requirements for allowed events.</p>

#### `InvalidRepositoryTriggerNameException`

``` purescript
newtype InvalidRepositoryTriggerNameException
  = InvalidRepositoryTriggerNameException {  }
```

<p>The name of the trigger is not valid.</p>

#### `InvalidRepositoryTriggerRegionException`

``` purescript
newtype InvalidRepositoryTriggerRegionException
  = InvalidRepositoryTriggerRegionException {  }
```

<p>The region for the trigger target does not match the region for the repository. Triggers must be created in the same region as the target for the trigger.</p>

#### `InvalidSortByException`

``` purescript
newtype InvalidSortByException
  = InvalidSortByException {  }
```

<p>The specified sort by value is not valid.</p>

#### `InvalidSourceCommitSpecifierException`

``` purescript
newtype InvalidSourceCommitSpecifierException
  = InvalidSourceCommitSpecifierException {  }
```

<p>The source commit specifier is not valid. You must provide a valid branch name, tag, or full commit ID.</p>

#### `InvalidTargetException`

``` purescript
newtype InvalidTargetException
  = InvalidTargetException {  }
```

<p>The target for the pull request is not valid. A target must contain the full values for the repository name, source branch, and destination branch for the pull request.</p>

#### `InvalidTargetsException`

``` purescript
newtype InvalidTargetsException
  = InvalidTargetsException {  }
```

<p>The targets for the pull request is not valid or not in a valid format. Targets are a list of target objects. Each target object must contain the full values for the repository name, source branch, and destination branch for a pull request.</p>

#### `InvalidTitleException`

``` purescript
newtype InvalidTitleException
  = InvalidTitleException {  }
```

<p>The title of the pull request is not valid. Pull request titles cannot exceed 100 characters in length.</p>

#### `IsCommentDeleted`

``` purescript
newtype IsCommentDeleted
  = IsCommentDeleted Boolean
```

#### `IsMergeable`

``` purescript
newtype IsMergeable
  = IsMergeable Boolean
```

#### `IsMerged`

``` purescript
newtype IsMerged
  = IsMerged Boolean
```

#### `LastModifiedDate`

``` purescript
newtype LastModifiedDate
  = LastModifiedDate Number
```

#### `Limit`

``` purescript
newtype Limit
  = Limit Int
```

#### `ListBranchesInput`

``` purescript
newtype ListBranchesInput
  = ListBranchesInput { "RepositoryName'" :: RepositoryName, "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a list branches operation.</p>

#### `ListBranchesOutput`

``` purescript
newtype ListBranchesOutput
  = ListBranchesOutput { "Branches'" :: NullOrUndefined (BranchNameList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a list branches operation.</p>

#### `ListPullRequestsInput`

``` purescript
newtype ListPullRequestsInput
  = ListPullRequestsInput { "RepositoryName'" :: RepositoryName, "AuthorArn'" :: NullOrUndefined (Arn), "PullRequestStatus'" :: NullOrUndefined (PullRequestStatusEnum), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `ListPullRequestsOutput`

``` purescript
newtype ListPullRequestsOutput
  = ListPullRequestsOutput { "PullRequestIds'" :: PullRequestIdList, "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListRepositoriesInput`

``` purescript
newtype ListRepositoriesInput
  = ListRepositoriesInput { "NextToken'" :: NullOrUndefined (NextToken), "SortBy'" :: NullOrUndefined (SortByEnum), "Order'" :: NullOrUndefined (OrderEnum) }
```

<p>Represents the input of a list repositories operation.</p>

#### `ListRepositoriesOutput`

``` purescript
newtype ListRepositoriesOutput
  = ListRepositoriesOutput { "Repositories'" :: NullOrUndefined (RepositoryNameIdPairList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a list repositories operation.</p>

#### `Location`

``` purescript
newtype Location
  = Location { "FilePath'" :: NullOrUndefined (Path), "FilePosition'" :: NullOrUndefined (Position), "RelativeFileVersion'" :: NullOrUndefined (RelativeFileVersionEnum) }
```

<p>Returns information about the location of a change or comment in the comparison between two commits or a pull request.</p>

#### `ManualMergeRequiredException`

``` purescript
newtype ManualMergeRequiredException
  = ManualMergeRequiredException {  }
```

<p>The pull request cannot be merged automatically into the destination branch. You must manually merge the branches and resolve any conflicts.</p>

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `MaximumBranchesExceededException`

``` purescript
newtype MaximumBranchesExceededException
  = MaximumBranchesExceededException {  }
```

<p>The number of branches for the trigger was exceeded.</p>

#### `MaximumOpenPullRequestsExceededException`

``` purescript
newtype MaximumOpenPullRequestsExceededException
  = MaximumOpenPullRequestsExceededException {  }
```

<p>You cannot create the pull request because the repository has too many open pull requests. The maximum number of open pull requests for a repository is 1,000. Close one or more open pull requests, and then try again.</p>

#### `MaximumRepositoryNamesExceededException`

``` purescript
newtype MaximumRepositoryNamesExceededException
  = MaximumRepositoryNamesExceededException {  }
```

<p>The maximum number of allowed repository names was exceeded. Currently, this number is 25.</p>

#### `MaximumRepositoryTriggersExceededException`

``` purescript
newtype MaximumRepositoryTriggersExceededException
  = MaximumRepositoryTriggersExceededException {  }
```

<p>The number of triggers allowed for the repository was exceeded.</p>

#### `MergeMetadata`

``` purescript
newtype MergeMetadata
  = MergeMetadata { "IsMerged'" :: NullOrUndefined (IsMerged), "MergedBy'" :: NullOrUndefined (Arn) }
```

<p>Returns information about a merge or potential merge between a source reference and a destination reference in a pull request.</p>

#### `MergeOptionRequiredException`

``` purescript
newtype MergeOptionRequiredException
  = MergeOptionRequiredException {  }
```

<p>A merge option or stategy is required, and none was provided.</p>

#### `MergeOptionTypeEnum`

``` purescript
newtype MergeOptionTypeEnum
  = MergeOptionTypeEnum String
```

#### `MergePullRequestByFastForwardInput`

``` purescript
newtype MergePullRequestByFastForwardInput
  = MergePullRequestByFastForwardInput { "PullRequestId'" :: PullRequestId, "RepositoryName'" :: RepositoryName, "SourceCommitId'" :: NullOrUndefined (CommitId) }
```

#### `MergePullRequestByFastForwardOutput`

``` purescript
newtype MergePullRequestByFastForwardOutput
  = MergePullRequestByFastForwardOutput { "PullRequest'" :: NullOrUndefined (PullRequest) }
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

#### `Mode`

``` purescript
newtype Mode
  = Mode String
```

#### `MultipleRepositoriesInPullRequestException`

``` purescript
newtype MultipleRepositoriesInPullRequestException
  = MultipleRepositoriesInPullRequestException {  }
```

<p>You cannot include more than one repository in a pull request. Make sure you have specified only one repository name in your request, and then try again.</p>

#### `Name`

``` purescript
newtype Name
  = Name String
```

#### `NameLengthExceededException`

``` purescript
newtype NameLengthExceededException
  = NameLengthExceededException {  }
```

<p>The file name is not valid because it has exceeded the character limit for file names. File names, including the path to the file, cannot exceed the character limit. </p>

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `ObjectId`

``` purescript
newtype ObjectId
  = ObjectId String
```

#### `OrderEnum`

``` purescript
newtype OrderEnum
  = OrderEnum String
```

#### `ParentCommitDoesNotExistException`

``` purescript
newtype ParentCommitDoesNotExistException
  = ParentCommitDoesNotExistException {  }
```

<p>The parent commit ID is not valid. The specified parent commit ID does not exist in the specified branch of the repository.</p>

#### `ParentCommitIdOutdatedException`

``` purescript
newtype ParentCommitIdOutdatedException
  = ParentCommitIdOutdatedException {  }
```

<p>The file could not be added because the provided parent commit ID is not the current tip of the specified branch. To view the full commit ID of the current head of the branch, use <a>GetBranch</a>.</p>

#### `ParentCommitIdRequiredException`

``` purescript
newtype ParentCommitIdRequiredException
  = ParentCommitIdRequiredException {  }
```

<p>A parent commit ID is required. To view the full commit ID of a branch in a repository, use <a>GetBranch</a> or a Git command (for example, git pull or git log).</p>

#### `ParentList`

``` purescript
newtype ParentList
  = ParentList (Array ObjectId)
```

#### `Path`

``` purescript
newtype Path
  = Path String
```

#### `PathDoesNotExistException`

``` purescript
newtype PathDoesNotExistException
  = PathDoesNotExistException {  }
```

<p>The specified path does not exist.</p>

#### `PathRequiredException`

``` purescript
newtype PathRequiredException
  = PathRequiredException {  }
```

<p>The filePath for a location cannot be empty or null.</p>

#### `Position`

``` purescript
newtype Position
  = Position Number
```

#### `PostCommentForComparedCommitInput`

``` purescript
newtype PostCommentForComparedCommitInput
  = PostCommentForComparedCommitInput { "RepositoryName'" :: RepositoryName, "BeforeCommitId'" :: NullOrUndefined (CommitId), "AfterCommitId'" :: CommitId, "Location'" :: NullOrUndefined (Location), "Content'" :: Content, "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken) }
```

#### `PostCommentForComparedCommitOutput`

``` purescript
newtype PostCommentForComparedCommitOutput
  = PostCommentForComparedCommitOutput { "RepositoryName'" :: NullOrUndefined (RepositoryName), "BeforeCommitId'" :: NullOrUndefined (CommitId), "AfterCommitId'" :: NullOrUndefined (CommitId), "BeforeBlobId'" :: NullOrUndefined (ObjectId), "AfterBlobId'" :: NullOrUndefined (ObjectId), "Location'" :: NullOrUndefined (Location), "Comment'" :: NullOrUndefined (Comment) }
```

#### `PostCommentForPullRequestInput`

``` purescript
newtype PostCommentForPullRequestInput
  = PostCommentForPullRequestInput { "PullRequestId'" :: PullRequestId, "RepositoryName'" :: RepositoryName, "BeforeCommitId'" :: CommitId, "AfterCommitId'" :: CommitId, "Location'" :: NullOrUndefined (Location), "Content'" :: Content, "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken) }
```

#### `PostCommentForPullRequestOutput`

``` purescript
newtype PostCommentForPullRequestOutput
  = PostCommentForPullRequestOutput { "RepositoryName'" :: NullOrUndefined (RepositoryName), "PullRequestId'" :: NullOrUndefined (PullRequestId), "BeforeCommitId'" :: NullOrUndefined (CommitId), "AfterCommitId'" :: NullOrUndefined (CommitId), "BeforeBlobId'" :: NullOrUndefined (ObjectId), "AfterBlobId'" :: NullOrUndefined (ObjectId), "Location'" :: NullOrUndefined (Location), "Comment'" :: NullOrUndefined (Comment) }
```

#### `PostCommentReplyInput`

``` purescript
newtype PostCommentReplyInput
  = PostCommentReplyInput { "InReplyTo'" :: CommentId, "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken), "Content'" :: Content }
```

#### `PostCommentReplyOutput`

``` purescript
newtype PostCommentReplyOutput
  = PostCommentReplyOutput { "Comment'" :: NullOrUndefined (Comment) }
```

#### `PullRequest`

``` purescript
newtype PullRequest
  = PullRequest { "PullRequestId'" :: NullOrUndefined (PullRequestId), "Title'" :: NullOrUndefined (Title), "Description'" :: NullOrUndefined (Description), "LastActivityDate'" :: NullOrUndefined (LastModifiedDate), "CreationDate'" :: NullOrUndefined (CreationDate), "PullRequestStatus'" :: NullOrUndefined (PullRequestStatusEnum), "AuthorArn'" :: NullOrUndefined (Arn), "PullRequestTargets'" :: NullOrUndefined (PullRequestTargetList), "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken) }
```

<p>Returns information about a pull request.</p>

#### `PullRequestAlreadyClosedException`

``` purescript
newtype PullRequestAlreadyClosedException
  = PullRequestAlreadyClosedException {  }
```

<p>The pull request status cannot be updated because it is already closed.</p>

#### `PullRequestDoesNotExistException`

``` purescript
newtype PullRequestDoesNotExistException
  = PullRequestDoesNotExistException {  }
```

<p>The pull request ID could not be found. Make sure that you have specified the correct repository name and pull request ID, and then try again.</p>

#### `PullRequestEvent`

``` purescript
newtype PullRequestEvent
  = PullRequestEvent { "PullRequestId'" :: NullOrUndefined (PullRequestId), "EventDate'" :: NullOrUndefined (EventDate), "PullRequestEventType'" :: NullOrUndefined (PullRequestEventType), "ActorArn'" :: NullOrUndefined (Arn), "PullRequestStatusChangedEventMetadata'" :: NullOrUndefined (PullRequestStatusChangedEventMetadata), "PullRequestSourceReferenceUpdatedEventMetadata'" :: NullOrUndefined (PullRequestSourceReferenceUpdatedEventMetadata), "PullRequestMergedStateChangedEventMetadata'" :: NullOrUndefined (PullRequestMergedStateChangedEventMetadata) }
```

<p>Returns information about a pull request event.</p>

#### `PullRequestEventList`

``` purescript
newtype PullRequestEventList
  = PullRequestEventList (Array PullRequestEvent)
```

#### `PullRequestEventType`

``` purescript
newtype PullRequestEventType
  = PullRequestEventType String
```

#### `PullRequestId`

``` purescript
newtype PullRequestId
  = PullRequestId String
```

#### `PullRequestIdList`

``` purescript
newtype PullRequestIdList
  = PullRequestIdList (Array PullRequestId)
```

#### `PullRequestIdRequiredException`

``` purescript
newtype PullRequestIdRequiredException
  = PullRequestIdRequiredException {  }
```

<p>A pull request ID is required, but none was provided.</p>

#### `PullRequestMergedStateChangedEventMetadata`

``` purescript
newtype PullRequestMergedStateChangedEventMetadata
  = PullRequestMergedStateChangedEventMetadata { "RepositoryName'" :: NullOrUndefined (RepositoryName), "DestinationReference'" :: NullOrUndefined (ReferenceName), "MergeMetadata'" :: NullOrUndefined (MergeMetadata) }
```

<p>Returns information about the change in the merge state for a pull request event. </p>

#### `PullRequestSourceReferenceUpdatedEventMetadata`

``` purescript
newtype PullRequestSourceReferenceUpdatedEventMetadata
  = PullRequestSourceReferenceUpdatedEventMetadata { "RepositoryName'" :: NullOrUndefined (RepositoryName), "BeforeCommitId'" :: NullOrUndefined (CommitId), "AfterCommitId'" :: NullOrUndefined (CommitId) }
```

<p>Information about an update to the source branch of a pull request.</p>

#### `PullRequestStatusChangedEventMetadata`

``` purescript
newtype PullRequestStatusChangedEventMetadata
  = PullRequestStatusChangedEventMetadata { "PullRequestStatus'" :: NullOrUndefined (PullRequestStatusEnum) }
```

<p>Information about a change to the status of a pull request.</p>

#### `PullRequestStatusEnum`

``` purescript
newtype PullRequestStatusEnum
  = PullRequestStatusEnum String
```

#### `PullRequestStatusRequiredException`

``` purescript
newtype PullRequestStatusRequiredException
  = PullRequestStatusRequiredException {  }
```

<p>A pull request status is required, but none was provided.</p>

#### `PullRequestTarget`

``` purescript
newtype PullRequestTarget
  = PullRequestTarget { "RepositoryName'" :: NullOrUndefined (RepositoryName), "SourceReference'" :: NullOrUndefined (ReferenceName), "DestinationReference'" :: NullOrUndefined (ReferenceName), "DestinationCommit'" :: NullOrUndefined (CommitId), "SourceCommit'" :: NullOrUndefined (CommitId), "MergeMetadata'" :: NullOrUndefined (MergeMetadata) }
```

<p>Returns information about a pull request target.</p>

#### `PullRequestTargetList`

``` purescript
newtype PullRequestTargetList
  = PullRequestTargetList (Array PullRequestTarget)
```

#### `PutFileInput`

``` purescript
newtype PutFileInput
  = PutFileInput { "RepositoryName'" :: RepositoryName, "BranchName'" :: BranchName, "FileContent'" :: FileContent, "FilePath'" :: Path, "FileMode'" :: NullOrUndefined (FileModeTypeEnum), "ParentCommitId'" :: NullOrUndefined (CommitId), "CommitMessage'" :: NullOrUndefined (Message), "Name'" :: NullOrUndefined (Name), "Email'" :: NullOrUndefined (Email) }
```

#### `PutFileOutput`

``` purescript
newtype PutFileOutput
  = PutFileOutput { "CommitId'" :: ObjectId, "BlobId'" :: ObjectId, "TreeId'" :: ObjectId }
```

#### `PutRepositoryTriggersInput`

``` purescript
newtype PutRepositoryTriggersInput
  = PutRepositoryTriggersInput { "RepositoryName'" :: RepositoryName, "Triggers'" :: RepositoryTriggersList }
```

<p>Represents the input ofa put repository triggers operation.</p>

#### `PutRepositoryTriggersOutput`

``` purescript
newtype PutRepositoryTriggersOutput
  = PutRepositoryTriggersOutput { "ConfigurationId'" :: NullOrUndefined (RepositoryTriggersConfigurationId) }
```

<p>Represents the output of a put repository triggers operation.</p>

#### `ReferenceDoesNotExistException`

``` purescript
newtype ReferenceDoesNotExistException
  = ReferenceDoesNotExistException {  }
```

<p>The specified reference does not exist. You must provide a full commit ID.</p>

#### `ReferenceName`

``` purescript
newtype ReferenceName
  = ReferenceName String
```

#### `ReferenceNameRequiredException`

``` purescript
newtype ReferenceNameRequiredException
  = ReferenceNameRequiredException {  }
```

<p>A reference name is required, but none was provided.</p>

#### `ReferenceTypeNotSupportedException`

``` purescript
newtype ReferenceTypeNotSupportedException
  = ReferenceTypeNotSupportedException {  }
```

<p>The specified reference is not a supported type. </p>

#### `RelativeFileVersionEnum`

``` purescript
newtype RelativeFileVersionEnum
  = RelativeFileVersionEnum String
```

#### `RepositoryDescription`

``` purescript
newtype RepositoryDescription
  = RepositoryDescription String
```

#### `RepositoryDoesNotExistException`

``` purescript
newtype RepositoryDoesNotExistException
  = RepositoryDoesNotExistException {  }
```

<p>The specified repository does not exist.</p>

#### `RepositoryId`

``` purescript
newtype RepositoryId
  = RepositoryId String
```

#### `RepositoryLimitExceededException`

``` purescript
newtype RepositoryLimitExceededException
  = RepositoryLimitExceededException {  }
```

<p>A repository resource limit was exceeded.</p>

#### `RepositoryMetadata`

``` purescript
newtype RepositoryMetadata
  = RepositoryMetadata { "AccountId'" :: NullOrUndefined (AccountId), "RepositoryId'" :: NullOrUndefined (RepositoryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "RepositoryDescription'" :: NullOrUndefined (RepositoryDescription), "DefaultBranch'" :: NullOrUndefined (BranchName), "LastModifiedDate'" :: NullOrUndefined (LastModifiedDate), "CreationDate'" :: NullOrUndefined (CreationDate), "CloneUrlHttp'" :: NullOrUndefined (CloneUrlHttp), "CloneUrlSsh'" :: NullOrUndefined (CloneUrlSsh), "Arn" :: NullOrUndefined (Arn) }
```

<p>Information about a repository.</p>

#### `RepositoryMetadataList`

``` purescript
newtype RepositoryMetadataList
  = RepositoryMetadataList (Array RepositoryMetadata)
```

#### `RepositoryName`

``` purescript
newtype RepositoryName
  = RepositoryName String
```

#### `RepositoryNameExistsException`

``` purescript
newtype RepositoryNameExistsException
  = RepositoryNameExistsException {  }
```

<p>The specified repository name already exists.</p>

#### `RepositoryNameIdPair`

``` purescript
newtype RepositoryNameIdPair
  = RepositoryNameIdPair { "RepositoryName'" :: NullOrUndefined (RepositoryName), "RepositoryId'" :: NullOrUndefined (RepositoryId) }
```

<p>Information about a repository name and ID.</p>

#### `RepositoryNameIdPairList`

``` purescript
newtype RepositoryNameIdPairList
  = RepositoryNameIdPairList (Array RepositoryNameIdPair)
```

#### `RepositoryNameList`

``` purescript
newtype RepositoryNameList
  = RepositoryNameList (Array RepositoryName)
```

#### `RepositoryNameRequiredException`

``` purescript
newtype RepositoryNameRequiredException
  = RepositoryNameRequiredException {  }
```

<p>A repository name is required but was not specified.</p>

#### `RepositoryNamesRequiredException`

``` purescript
newtype RepositoryNamesRequiredException
  = RepositoryNamesRequiredException {  }
```

<p>A repository names object is required but was not specified.</p>

#### `RepositoryNotAssociatedWithPullRequestException`

``` purescript
newtype RepositoryNotAssociatedWithPullRequestException
  = RepositoryNotAssociatedWithPullRequestException {  }
```

<p>The repository does not contain any pull requests with that pull request ID. Check to make sure you have provided the correct repository name for the pull request.</p>

#### `RepositoryNotFoundList`

``` purescript
newtype RepositoryNotFoundList
  = RepositoryNotFoundList (Array RepositoryName)
```

#### `RepositoryTrigger`

``` purescript
newtype RepositoryTrigger
  = RepositoryTrigger { "Name'" :: RepositoryTriggerName, "DestinationArn'" :: Arn, "CustomData'" :: NullOrUndefined (RepositoryTriggerCustomData), "Branches'" :: NullOrUndefined (BranchNameList), "Events'" :: RepositoryTriggerEventList }
```

<p>Information about a trigger for a repository.</p>

#### `RepositoryTriggerBranchNameListRequiredException`

``` purescript
newtype RepositoryTriggerBranchNameListRequiredException
  = RepositoryTriggerBranchNameListRequiredException {  }
```

<p>At least one branch name is required but was not specified in the trigger configuration.</p>

#### `RepositoryTriggerCustomData`

``` purescript
newtype RepositoryTriggerCustomData
  = RepositoryTriggerCustomData String
```

#### `RepositoryTriggerDestinationArnRequiredException`

``` purescript
newtype RepositoryTriggerDestinationArnRequiredException
  = RepositoryTriggerDestinationArnRequiredException {  }
```

<p>A destination ARN for the target service for the trigger is required but was not specified.</p>

#### `RepositoryTriggerEventEnum`

``` purescript
newtype RepositoryTriggerEventEnum
  = RepositoryTriggerEventEnum String
```

#### `RepositoryTriggerEventList`

``` purescript
newtype RepositoryTriggerEventList
  = RepositoryTriggerEventList (Array RepositoryTriggerEventEnum)
```

#### `RepositoryTriggerEventsListRequiredException`

``` purescript
newtype RepositoryTriggerEventsListRequiredException
  = RepositoryTriggerEventsListRequiredException {  }
```

<p>At least one event for the trigger is required but was not specified.</p>

#### `RepositoryTriggerExecutionFailure`

``` purescript
newtype RepositoryTriggerExecutionFailure
  = RepositoryTriggerExecutionFailure { "Trigger'" :: NullOrUndefined (RepositoryTriggerName), "FailureMessage'" :: NullOrUndefined (RepositoryTriggerExecutionFailureMessage) }
```

<p>A trigger failed to run.</p>

#### `RepositoryTriggerExecutionFailureList`

``` purescript
newtype RepositoryTriggerExecutionFailureList
  = RepositoryTriggerExecutionFailureList (Array RepositoryTriggerExecutionFailure)
```

#### `RepositoryTriggerExecutionFailureMessage`

``` purescript
newtype RepositoryTriggerExecutionFailureMessage
  = RepositoryTriggerExecutionFailureMessage String
```

#### `RepositoryTriggerName`

``` purescript
newtype RepositoryTriggerName
  = RepositoryTriggerName String
```

#### `RepositoryTriggerNameList`

``` purescript
newtype RepositoryTriggerNameList
  = RepositoryTriggerNameList (Array RepositoryTriggerName)
```

#### `RepositoryTriggerNameRequiredException`

``` purescript
newtype RepositoryTriggerNameRequiredException
  = RepositoryTriggerNameRequiredException {  }
```

<p>A name for the trigger is required but was not specified.</p>

#### `RepositoryTriggersConfigurationId`

``` purescript
newtype RepositoryTriggersConfigurationId
  = RepositoryTriggersConfigurationId String
```

#### `RepositoryTriggersList`

``` purescript
newtype RepositoryTriggersList
  = RepositoryTriggersList (Array RepositoryTrigger)
```

#### `RepositoryTriggersListRequiredException`

``` purescript
newtype RepositoryTriggersListRequiredException
  = RepositoryTriggersListRequiredException {  }
```

<p>The list of triggers for the repository is required but was not specified.</p>

#### `SameFileContentException`

``` purescript
newtype SameFileContentException
  = SameFileContentException {  }
```

<p>The file was not added or updated because the content of the file is exactly the same as the content of that file in the repository and branch that you specified.</p>

#### `SortByEnum`

``` purescript
newtype SortByEnum
  = SortByEnum String
```

#### `SourceAndDestinationAreSameException`

``` purescript
newtype SourceAndDestinationAreSameException
  = SourceAndDestinationAreSameException {  }
```

<p>The source branch and the destination branch for the pull request are the same. You must specify different branches for the source and destination.</p>

#### `Target`

``` purescript
newtype Target
  = Target { "RepositoryName'" :: RepositoryName, "SourceReference'" :: ReferenceName, "DestinationReference'" :: NullOrUndefined (ReferenceName) }
```

<p>Returns information about a target for a pull request.</p>

#### `TargetList`

``` purescript
newtype TargetList
  = TargetList (Array Target)
```

#### `TargetRequiredException`

``` purescript
newtype TargetRequiredException
  = TargetRequiredException {  }
```

<p>A pull request target is required. It cannot be empty or null. A pull request target must contain the full values for the repository name, source branch, and destination branch for the pull request.</p>

#### `TargetsRequiredException`

``` purescript
newtype TargetsRequiredException
  = TargetsRequiredException {  }
```

<p>An array of target objects is required. It cannot be empty or null.</p>

#### `TestRepositoryTriggersInput`

``` purescript
newtype TestRepositoryTriggersInput
  = TestRepositoryTriggersInput { "RepositoryName'" :: RepositoryName, "Triggers'" :: RepositoryTriggersList }
```

<p>Represents the input of a test repository triggers operation.</p>

#### `TestRepositoryTriggersOutput`

``` purescript
newtype TestRepositoryTriggersOutput
  = TestRepositoryTriggersOutput { "SuccessfulExecutions'" :: NullOrUndefined (RepositoryTriggerNameList), "FailedExecutions'" :: NullOrUndefined (RepositoryTriggerExecutionFailureList) }
```

<p>Represents the output of a test repository triggers operation.</p>

#### `TipOfSourceReferenceIsDifferentException`

``` purescript
newtype TipOfSourceReferenceIsDifferentException
  = TipOfSourceReferenceIsDifferentException {  }
```

<p>The tip of the source branch in the destination repository does not match the tip of the source branch specified in your request. The pull request might have been updated. Make sure that you have the latest changes.</p>

#### `TipsDivergenceExceededException`

``` purescript
newtype TipsDivergenceExceededException
  = TipsDivergenceExceededException {  }
```

<p>The divergence between the tips of the provided commit specifiers is too great to determine whether there might be any merge conflicts. Locally compare the specifiers using <code>git diff</code> or a diff tool.</p>

#### `Title`

``` purescript
newtype Title
  = Title String
```

#### `TitleRequiredException`

``` purescript
newtype TitleRequiredException
  = TitleRequiredException {  }
```

<p>A pull request title is required. It cannot be empty or null.</p>

#### `UpdateCommentInput`

``` purescript
newtype UpdateCommentInput
  = UpdateCommentInput { "CommentId'" :: CommentId, "Content'" :: Content }
```

#### `UpdateCommentOutput`

``` purescript
newtype UpdateCommentOutput
  = UpdateCommentOutput { "Comment'" :: NullOrUndefined (Comment) }
```

#### `UpdateDefaultBranchInput`

``` purescript
newtype UpdateDefaultBranchInput
  = UpdateDefaultBranchInput { "RepositoryName'" :: RepositoryName, "DefaultBranchName'" :: BranchName }
```

<p>Represents the input of an update default branch operation.</p>

#### `UpdatePullRequestDescriptionInput`

``` purescript
newtype UpdatePullRequestDescriptionInput
  = UpdatePullRequestDescriptionInput { "PullRequestId'" :: PullRequestId, "Description'" :: Description }
```

#### `UpdatePullRequestDescriptionOutput`

``` purescript
newtype UpdatePullRequestDescriptionOutput
  = UpdatePullRequestDescriptionOutput { "PullRequest'" :: PullRequest }
```

#### `UpdatePullRequestStatusInput`

``` purescript
newtype UpdatePullRequestStatusInput
  = UpdatePullRequestStatusInput { "PullRequestId'" :: PullRequestId, "PullRequestStatus'" :: PullRequestStatusEnum }
```

#### `UpdatePullRequestStatusOutput`

``` purescript
newtype UpdatePullRequestStatusOutput
  = UpdatePullRequestStatusOutput { "PullRequest'" :: PullRequest }
```

#### `UpdatePullRequestTitleInput`

``` purescript
newtype UpdatePullRequestTitleInput
  = UpdatePullRequestTitleInput { "PullRequestId'" :: PullRequestId, "Title'" :: Title }
```

#### `UpdatePullRequestTitleOutput`

``` purescript
newtype UpdatePullRequestTitleOutput
  = UpdatePullRequestTitleOutput { "PullRequest'" :: PullRequest }
```

#### `UpdateRepositoryDescriptionInput`

``` purescript
newtype UpdateRepositoryDescriptionInput
  = UpdateRepositoryDescriptionInput { "RepositoryName'" :: RepositoryName, "RepositoryDescription'" :: NullOrUndefined (RepositoryDescription) }
```

<p>Represents the input of an update repository description operation.</p>

#### `UpdateRepositoryNameInput`

``` purescript
newtype UpdateRepositoryNameInput
  = UpdateRepositoryNameInput { "OldName'" :: RepositoryName, "NewName'" :: RepositoryName }
```

<p>Represents the input of an update repository description operation.</p>

#### `UserInfo`

``` purescript
newtype UserInfo
  = UserInfo { "Name'" :: NullOrUndefined (Name), "Email'" :: NullOrUndefined (Email), "Date'" :: NullOrUndefined (Date) }
```

<p>Information about the user who made a specified commit.</p>


