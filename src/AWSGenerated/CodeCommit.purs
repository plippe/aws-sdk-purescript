

-- | <fullname>AWS CodeCommit</fullname> <p>This is the <i>AWS CodeCommit API Reference</i>. This reference provides descriptions of the operations and data types for AWS CodeCommit API along with usage examples.</p> <p>You can use the AWS CodeCommit API to work with the following objects:</p> <p>Repositories, by calling the following:</p> <ul> <li> <p> <a>BatchGetRepositories</a>, which returns information about one or more repositories associated with your AWS account.</p> </li> <li> <p> <a>CreateRepository</a>, which creates an AWS CodeCommit repository.</p> </li> <li> <p> <a>DeleteRepository</a>, which deletes an AWS CodeCommit repository.</p> </li> <li> <p> <a>GetRepository</a>, which returns information about a specified repository.</p> </li> <li> <p> <a>ListRepositories</a>, which lists all AWS CodeCommit repositories associated with your AWS account.</p> </li> <li> <p> <a>UpdateRepositoryDescription</a>, which sets or updates the description of the repository.</p> </li> <li> <p> <a>UpdateRepositoryName</a>, which changes the name of the repository. If you change the name of a repository, no other users of that repository will be able to access it until you send them the new HTTPS or SSH URL to use.</p> </li> </ul> <p>Branches, by calling the following:</p> <ul> <li> <p> <a>CreateBranch</a>, which creates a new branch in a specified repository.</p> </li> <li> <p> <a>DeleteBranch</a>, which deletes the specified branch in a repository unless it is the default branch.</p> </li> <li> <p> <a>GetBranch</a>, which returns information about a specified branch.</p> </li> <li> <p> <a>ListBranches</a>, which lists all branches for a specified repository.</p> </li> <li> <p> <a>UpdateDefaultBranch</a>, which changes the default branch for a repository.</p> </li> </ul> <p>Files, by calling the following:</p> <ul> <li> <p> <a>PutFile</a>, which adds or modifies a file in a specified repository and branch.</p> </li> </ul> <p>Information about committed code in a repository, by calling the following:</p> <ul> <li> <p> <a>GetBlob</a>, which returns the base-64 encoded content of an individual Git blob object within a repository.</p> </li> <li> <p> <a>GetCommit</a>, which returns information about a commit, including commit messages and author and committer information.</p> </li> <li> <p> <a>GetDifferences</a>, which returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID or other fully qualified reference).</p> </li> </ul> <p>Pull requests, by calling the following:</p> <ul> <li> <p> <a>CreatePullRequest</a>, which creates a pull request in a specified repository.</p> </li> <li> <p> <a>DescribePullRequestEvents</a>, which returns information about one or more pull request events.</p> </li> <li> <p> <a>GetCommentsForPullRequest</a>, which returns information about comments on a specified pull request.</p> </li> <li> <p> <a>GetMergeConflicts</a>, which returns information about merge conflicts between the source and destination branch in a pull request.</p> </li> <li> <p> <a>GetPullRequest</a>, which returns information about a specified pull request.</p> </li> <li> <p> <a>ListPullRequests</a>, which lists all pull requests for a repository.</p> </li> <li> <p> <a>MergePullRequestByFastForward</a>, which merges the source destination branch of a pull request into the specified destination branch for that pull request using the fast-forward merge option.</p> </li> <li> <p> <a>PostCommentForPullRequest</a>, which posts a comment to a pull request at the specified line, file, or request.</p> </li> <li> <p> <a>UpdatePullRequestDescription</a>, which updates the description of a pull request.</p> </li> <li> <p> <a>UpdatePullRequestStatus</a>, which updates the status of a pull request.</p> </li> <li> <p> <a>UpdatePullRequestTitle</a>, which updates the title of a pull request.</p> </li> </ul> <p>Information about comments in a repository, by calling the following:</p> <ul> <li> <p> <a>DeleteCommentContent</a>, which deletes the content of a comment on a commit in a repository.</p> </li> <li> <p> <a>GetComment</a>, which returns information about a comment on a commit.</p> </li> <li> <p> <a>GetCommentsForComparedCommit</a>, which returns information about comments on the comparison between two commit specifiers in a repository.</p> </li> <li> <p> <a>PostCommentForComparedCommit</a>, which creates a comment on the comparison between two commit specifiers in a repository.</p> </li> <li> <p> <a>PostCommentReply</a>, which creates a reply to a comment.</p> </li> <li> <p> <a>UpdateComment</a>, which updates the content of a comment on a commit in a repository.</p> </li> </ul> <p>Triggers, by calling the following:</p> <ul> <li> <p> <a>GetRepositoryTriggers</a>, which returns information about triggers configured for a repository.</p> </li> <li> <p> <a>PutRepositoryTriggers</a>, which replaces all triggers for a repository and can be used to create or delete triggers.</p> </li> <li> <p> <a>TestRepositoryTriggers</a>, which tests the functionality of a repository trigger by sending data to the trigger target.</p> </li> </ul> <p>For information about how to use AWS CodeCommit, see the <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html">AWS CodeCommit User Guide</a>.</p>
module AWS.CodeCommit where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CodeCommit" :: String


-- | <p>Returns information about one or more repositories.</p> <note> <p>The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.</p> </note>
batchGetRepositories :: forall eff. BatchGetRepositoriesInput -> Aff (err :: AWS.RequestError | eff) BatchGetRepositoriesOutput
batchGetRepositories = AWS.request serviceName "BatchGetRepositories" 


-- | <p>Creates a new branch in a repository and points the branch to a commit.</p> <note> <p>Calling the create branch operation does not set a repository's default branch. To do this, call the update default branch operation.</p> </note>
createBranch :: forall eff. CreateBranchInput -> Aff (err :: AWS.RequestError | eff) Unit
createBranch = AWS.request serviceName "CreateBranch" 


-- | <p>Creates a pull request in the specified repository.</p>
createPullRequest :: forall eff. CreatePullRequestInput -> Aff (err :: AWS.RequestError | eff) CreatePullRequestOutput
createPullRequest = AWS.request serviceName "CreatePullRequest" 


-- | <p>Creates a new, empty repository.</p>
createRepository :: forall eff. CreateRepositoryInput -> Aff (err :: AWS.RequestError | eff) CreateRepositoryOutput
createRepository = AWS.request serviceName "CreateRepository" 


-- | <p>Deletes a branch from a repository, unless that branch is the default branch for the repository. </p>
deleteBranch :: forall eff. DeleteBranchInput -> Aff (err :: AWS.RequestError | eff) DeleteBranchOutput
deleteBranch = AWS.request serviceName "DeleteBranch" 


-- | <p>Deletes the content of a comment made on a change, file, or commit in a repository.</p>
deleteCommentContent :: forall eff. DeleteCommentContentInput -> Aff (err :: AWS.RequestError | eff) DeleteCommentContentOutput
deleteCommentContent = AWS.request serviceName "DeleteCommentContent" 


-- | <p>Deletes a repository. If a specified repository was already deleted, a null repository ID will be returned.</p> <important> <p>Deleting a repository also deletes all associated objects and metadata. After a repository is deleted, all future push calls to the deleted repository will fail.</p> </important>
deleteRepository :: forall eff. DeleteRepositoryInput -> Aff (err :: AWS.RequestError | eff) DeleteRepositoryOutput
deleteRepository = AWS.request serviceName "DeleteRepository" 


-- | <p>Returns information about one or more pull request events.</p>
describePullRequestEvents :: forall eff. DescribePullRequestEventsInput -> Aff (err :: AWS.RequestError | eff) DescribePullRequestEventsOutput
describePullRequestEvents = AWS.request serviceName "DescribePullRequestEvents" 


-- | <p>Returns the base-64 encoded content of an individual blob within a repository.</p>
getBlob :: forall eff. GetBlobInput -> Aff (err :: AWS.RequestError | eff) GetBlobOutput
getBlob = AWS.request serviceName "GetBlob" 


-- | <p>Returns information about a repository branch, including its name and the last commit ID.</p>
getBranch :: forall eff. GetBranchInput -> Aff (err :: AWS.RequestError | eff) GetBranchOutput
getBranch = AWS.request serviceName "GetBranch" 


-- | <p>Returns the content of a comment made on a change, file, or commit in a repository.</p>
getComment :: forall eff. GetCommentInput -> Aff (err :: AWS.RequestError | eff) GetCommentOutput
getComment = AWS.request serviceName "GetComment" 


-- | <p>Returns information about comments made on the comparison between two commits.</p>
getCommentsForComparedCommit :: forall eff. GetCommentsForComparedCommitInput -> Aff (err :: AWS.RequestError | eff) GetCommentsForComparedCommitOutput
getCommentsForComparedCommit = AWS.request serviceName "GetCommentsForComparedCommit" 


-- | <p>Returns comments made on a pull request.</p>
getCommentsForPullRequest :: forall eff. GetCommentsForPullRequestInput -> Aff (err :: AWS.RequestError | eff) GetCommentsForPullRequestOutput
getCommentsForPullRequest = AWS.request serviceName "GetCommentsForPullRequest" 


-- | <p>Returns information about a commit, including commit message and committer information.</p>
getCommit :: forall eff. GetCommitInput -> Aff (err :: AWS.RequestError | eff) GetCommitOutput
getCommit = AWS.request serviceName "GetCommit" 


-- | <p>Returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID or other fully qualified reference). Results can be limited to a specified path.</p>
getDifferences :: forall eff. GetDifferencesInput -> Aff (err :: AWS.RequestError | eff) GetDifferencesOutput
getDifferences = AWS.request serviceName "GetDifferences" 


-- | <p>Returns information about merge conflicts between the before and after commit IDs for a pull request in a repository.</p>
getMergeConflicts :: forall eff. GetMergeConflictsInput -> Aff (err :: AWS.RequestError | eff) GetMergeConflictsOutput
getMergeConflicts = AWS.request serviceName "GetMergeConflicts" 


-- | <p>Gets information about a pull request in a specified repository.</p>
getPullRequest :: forall eff. GetPullRequestInput -> Aff (err :: AWS.RequestError | eff) GetPullRequestOutput
getPullRequest = AWS.request serviceName "GetPullRequest" 


-- | <p>Returns information about a repository.</p> <note> <p>The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.</p> </note>
getRepository :: forall eff. GetRepositoryInput -> Aff (err :: AWS.RequestError | eff) GetRepositoryOutput
getRepository = AWS.request serviceName "GetRepository" 


-- | <p>Gets information about triggers configured for a repository.</p>
getRepositoryTriggers :: forall eff. GetRepositoryTriggersInput -> Aff (err :: AWS.RequestError | eff) GetRepositoryTriggersOutput
getRepositoryTriggers = AWS.request serviceName "GetRepositoryTriggers" 


-- | <p>Gets information about one or more branches in a repository.</p>
listBranches :: forall eff. ListBranchesInput -> Aff (err :: AWS.RequestError | eff) ListBranchesOutput
listBranches = AWS.request serviceName "ListBranches" 


-- | <p>Returns a list of pull requests for a specified repository. The return list can be refined by pull request status or pull request author ARN.</p>
listPullRequests :: forall eff. ListPullRequestsInput -> Aff (err :: AWS.RequestError | eff) ListPullRequestsOutput
listPullRequests = AWS.request serviceName "ListPullRequests" 


-- | <p>Gets information about one or more repositories.</p>
listRepositories :: forall eff. ListRepositoriesInput -> Aff (err :: AWS.RequestError | eff) ListRepositoriesOutput
listRepositories = AWS.request serviceName "ListRepositories" 


-- | <p>Closes a pull request and attempts to merge the source commit of a pull request into the specified destination branch for that pull request at the specified commit using the fast-forward merge option.</p>
mergePullRequestByFastForward :: forall eff. MergePullRequestByFastForwardInput -> Aff (err :: AWS.RequestError | eff) MergePullRequestByFastForwardOutput
mergePullRequestByFastForward = AWS.request serviceName "MergePullRequestByFastForward" 


-- | <p>Posts a comment on the comparison between two commits.</p>
postCommentForComparedCommit :: forall eff. PostCommentForComparedCommitInput -> Aff (err :: AWS.RequestError | eff) PostCommentForComparedCommitOutput
postCommentForComparedCommit = AWS.request serviceName "PostCommentForComparedCommit" 


-- | <p>Posts a comment on a pull request.</p>
postCommentForPullRequest :: forall eff. PostCommentForPullRequestInput -> Aff (err :: AWS.RequestError | eff) PostCommentForPullRequestOutput
postCommentForPullRequest = AWS.request serviceName "PostCommentForPullRequest" 


-- | <p>Posts a comment in reply to an existing comment on a comparison between commits or a pull request.</p>
postCommentReply :: forall eff. PostCommentReplyInput -> Aff (err :: AWS.RequestError | eff) PostCommentReplyOutput
postCommentReply = AWS.request serviceName "PostCommentReply" 


-- | <p>Adds or updates a file in an AWS CodeCommit repository.</p>
putFile :: forall eff. PutFileInput -> Aff (err :: AWS.RequestError | eff) PutFileOutput
putFile = AWS.request serviceName "PutFile" 


-- | <p>Replaces all triggers for a repository. This can be used to create or delete triggers.</p>
putRepositoryTriggers :: forall eff. PutRepositoryTriggersInput -> Aff (err :: AWS.RequestError | eff) PutRepositoryTriggersOutput
putRepositoryTriggers = AWS.request serviceName "PutRepositoryTriggers" 


-- | <p>Tests the functionality of repository triggers by sending information to the trigger target. If real data is available in the repository, the test will send data from the last commit. If no data is available, sample data will be generated.</p>
testRepositoryTriggers :: forall eff. TestRepositoryTriggersInput -> Aff (err :: AWS.RequestError | eff) TestRepositoryTriggersOutput
testRepositoryTriggers = AWS.request serviceName "TestRepositoryTriggers" 


-- | <p>Replaces the contents of a comment.</p>
updateComment :: forall eff. UpdateCommentInput -> Aff (err :: AWS.RequestError | eff) UpdateCommentOutput
updateComment = AWS.request serviceName "UpdateComment" 


-- | <p>Sets or changes the default branch name for the specified repository.</p> <note> <p>If you use this operation to change the default branch name to the current default branch name, a success message is returned even though the default branch did not change.</p> </note>
updateDefaultBranch :: forall eff. UpdateDefaultBranchInput -> Aff (err :: AWS.RequestError | eff) Unit
updateDefaultBranch = AWS.request serviceName "UpdateDefaultBranch" 


-- | <p>Replaces the contents of the description of a pull request.</p>
updatePullRequestDescription :: forall eff. UpdatePullRequestDescriptionInput -> Aff (err :: AWS.RequestError | eff) UpdatePullRequestDescriptionOutput
updatePullRequestDescription = AWS.request serviceName "UpdatePullRequestDescription" 


-- | <p>Updates the status of a pull request. </p>
updatePullRequestStatus :: forall eff. UpdatePullRequestStatusInput -> Aff (err :: AWS.RequestError | eff) UpdatePullRequestStatusOutput
updatePullRequestStatus = AWS.request serviceName "UpdatePullRequestStatus" 


-- | <p>Replaces the title of a pull request.</p>
updatePullRequestTitle :: forall eff. UpdatePullRequestTitleInput -> Aff (err :: AWS.RequestError | eff) UpdatePullRequestTitleOutput
updatePullRequestTitle = AWS.request serviceName "UpdatePullRequestTitle" 


-- | <p>Sets or changes the comment or description for a repository.</p> <note> <p>The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.</p> </note>
updateRepositoryDescription :: forall eff. UpdateRepositoryDescriptionInput -> Aff (err :: AWS.RequestError | eff) Unit
updateRepositoryDescription = AWS.request serviceName "UpdateRepositoryDescription" 


-- | <p>Renames a repository. The repository name must be unique across the calling AWS account. In addition, repository names are limited to 100 alphanumeric, dash, and underscore characters, and cannot include certain characters. The suffix ".git" is prohibited. For a full description of the limits on repository names, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/limits.html">Limits</a> in the AWS CodeCommit User Guide.</p>
updateRepositoryName :: forall eff. UpdateRepositoryNameInput -> Aff (err :: AWS.RequestError | eff) Unit
updateRepositoryName = AWS.request serviceName "UpdateRepositoryName" 


newtype AccountId = AccountId String


-- | <p>The specified Amazon Resource Name (ARN) does not exist in the AWS account.</p>
newtype ActorDoesNotExistException = ActorDoesNotExistException 
  { 
  }


newtype AdditionalData = AdditionalData String


newtype Arn = Arn String


-- | <p>The specified Amazon Resource Name (ARN) does not exist in the AWS account.</p>
newtype AuthorDoesNotExistException = AuthorDoesNotExistException 
  { 
  }


-- | <p>Represents the input of a batch get repositories operation.</p>
newtype BatchGetRepositoriesInput = BatchGetRepositoriesInput 
  { "RepositoryNames'" :: (RepositoryNameList)
  }


-- | <p>Represents the output of a batch get repositories operation.</p>
newtype BatchGetRepositoriesOutput = BatchGetRepositoriesOutput 
  { "Repositories'" :: NullOrUndefined (RepositoryMetadataList)
  , "RepositoriesNotFound'" :: NullOrUndefined (RepositoryNotFoundList)
  }


-- | <p>The before commit ID and the after commit ID are the same, which is not valid. The before commit ID and the after commit ID must be different commit IDs.</p>
newtype BeforeCommitIdAndAfterCommitIdAreSameException = BeforeCommitIdAndAfterCommitIdAreSameException 
  { 
  }


-- | <p>The specified blob does not exist.</p>
newtype BlobIdDoesNotExistException = BlobIdDoesNotExistException 
  { 
  }


-- | <p>A blob ID is required but was not specified.</p>
newtype BlobIdRequiredException = BlobIdRequiredException 
  { 
  }


-- | <p>Returns information about a specific Git blob object.</p>
newtype BlobMetadata = BlobMetadata 
  { "BlobId'" :: NullOrUndefined (ObjectId)
  , "Path'" :: NullOrUndefined (Path)
  , "Mode'" :: NullOrUndefined (Mode)
  }


-- | <p>The specified branch does not exist.</p>
newtype BranchDoesNotExistException = BranchDoesNotExistException 
  { 
  }


-- | <p>Returns information about a branch.</p>
newtype BranchInfo = BranchInfo 
  { "BranchName'" :: NullOrUndefined (BranchName)
  , "CommitId'" :: NullOrUndefined (CommitId)
  }


newtype BranchName = BranchName String


-- | <p>The specified branch name already exists.</p>
newtype BranchNameExistsException = BranchNameExistsException 
  { 
  }


-- | <p>The specified branch name is not valid because it is a tag name. Type the name of a current branch in the repository. For a list of valid branch names, use <a>ListBranches</a>.</p>
newtype BranchNameIsTagNameException = BranchNameIsTagNameException 
  { 
  }


newtype BranchNameList = BranchNameList (Array BranchName)


-- | <p>A branch name is required but was not specified.</p>
newtype BranchNameRequiredException = BranchNameRequiredException 
  { 
  }


newtype ChangeTypeEnum = ChangeTypeEnum String


newtype ClientRequestToken = ClientRequestToken String


-- | <p>A client request token is required. A client request token is an unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.</p>
newtype ClientRequestTokenRequiredException = ClientRequestTokenRequiredException 
  { 
  }


newtype CloneUrlHttp = CloneUrlHttp String


newtype CloneUrlSsh = CloneUrlSsh String


-- | <p>Returns information about a specific comment.</p>
newtype Comment = Comment 
  { "CommentId'" :: NullOrUndefined (CommentId)
  , "Content'" :: NullOrUndefined (Content)
  , "InReplyTo'" :: NullOrUndefined (CommentId)
  , "CreationDate'" :: NullOrUndefined (CreationDate)
  , "LastModifiedDate'" :: NullOrUndefined (LastModifiedDate)
  , "AuthorArn'" :: NullOrUndefined (Arn)
  , "Deleted'" :: NullOrUndefined (IsCommentDeleted)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  }


-- | <p>The comment is empty. You must provide some content for a comment. The content cannot be null.</p>
newtype CommentContentRequiredException = CommentContentRequiredException 
  { 
  }


-- | <p>The comment is too large. Comments are limited to 1,000 characters.</p>
newtype CommentContentSizeLimitExceededException = CommentContentSizeLimitExceededException 
  { 
  }


-- | <p>This comment has already been deleted. You cannot edit or delete a deleted comment.</p>
newtype CommentDeletedException = CommentDeletedException 
  { 
  }


-- | <p>No comment exists with the provided ID. Verify that you have provided the correct ID, and then try again.</p>
newtype CommentDoesNotExistException = CommentDoesNotExistException 
  { 
  }


newtype CommentId = CommentId String


-- | <p>The comment ID is missing or null. A comment ID is required.</p>
newtype CommentIdRequiredException = CommentIdRequiredException 
  { 
  }


-- | <p>You cannot modify or delete this comment. Only comment authors can modify or delete their comments.</p>
newtype CommentNotCreatedByCallerException = CommentNotCreatedByCallerException 
  { 
  }


newtype Comments = Comments (Array Comment)


-- | <p>Returns information about comments on the comparison between two commits.</p>
newtype CommentsForComparedCommit = CommentsForComparedCommit 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined (CommitId)
  , "BeforeBlobId'" :: NullOrUndefined (ObjectId)
  , "AfterBlobId'" :: NullOrUndefined (ObjectId)
  , "Location'" :: NullOrUndefined (Location)
  , "Comments'" :: NullOrUndefined (Comments)
  }


newtype CommentsForComparedCommitData = CommentsForComparedCommitData (Array CommentsForComparedCommit)


-- | <p>Returns information about comments on a pull request.</p>
newtype CommentsForPullRequest = CommentsForPullRequest 
  { "PullRequestId'" :: NullOrUndefined (PullRequestId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined (CommitId)
  , "BeforeBlobId'" :: NullOrUndefined (ObjectId)
  , "AfterBlobId'" :: NullOrUndefined (ObjectId)
  , "Location'" :: NullOrUndefined (Location)
  , "Comments'" :: NullOrUndefined (Comments)
  }


newtype CommentsForPullRequestData = CommentsForPullRequestData (Array CommentsForPullRequest)


-- | <p>Returns information about a specific commit.</p>
newtype Commit = Commit 
  { "CommitId'" :: NullOrUndefined (ObjectId)
  , "TreeId'" :: NullOrUndefined (ObjectId)
  , "Parents'" :: NullOrUndefined (ParentList)
  , "Message'" :: NullOrUndefined (Message)
  , "Author'" :: NullOrUndefined (UserInfo)
  , "Committer'" :: NullOrUndefined (UserInfo)
  , "AdditionalData'" :: NullOrUndefined (AdditionalData)
  }


-- | <p>The specified commit does not exist or no commit was specified, and the specified repository has no default branch.</p>
newtype CommitDoesNotExistException = CommitDoesNotExistException 
  { 
  }


newtype CommitId = CommitId String


-- | <p>The specified commit ID does not exist.</p>
newtype CommitIdDoesNotExistException = CommitIdDoesNotExistException 
  { 
  }


-- | <p>A commit ID was not specified.</p>
newtype CommitIdRequiredException = CommitIdRequiredException 
  { 
  }


-- | <p>The commit message is too long. Provide a shorter string. </p>
newtype CommitMessageLengthExceededException = CommitMessageLengthExceededException 
  { 
  }


newtype CommitName = CommitName String


-- | <p>A commit was not specified.</p>
newtype CommitRequiredException = CommitRequiredException 
  { 
  }


newtype Content = Content String


-- | <p>Represents the input of a create branch operation.</p>
newtype CreateBranchInput = CreateBranchInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BranchName'" :: (BranchName)
  , "CommitId'" :: (CommitId)
  }


newtype CreatePullRequestInput = CreatePullRequestInput 
  { "Title'" :: (Title)
  , "Description'" :: NullOrUndefined (Description)
  , "Targets'" :: (TargetList)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  }


newtype CreatePullRequestOutput = CreatePullRequestOutput 
  { "PullRequest'" :: (PullRequest)
  }


-- | <p>Represents the input of a create repository operation.</p>
newtype CreateRepositoryInput = CreateRepositoryInput 
  { "RepositoryName'" :: (RepositoryName)
  , "RepositoryDescription'" :: NullOrUndefined (RepositoryDescription)
  }


-- | <p>Represents the output of a create repository operation.</p>
newtype CreateRepositoryOutput = CreateRepositoryOutput 
  { "RepositoryMetadata'" :: NullOrUndefined (RepositoryMetadata)
  }


newtype CreationDate = CreationDate Number


newtype Date = Date String


-- | <p>The specified branch is the default branch for the repository, and cannot be deleted. To delete this branch, you must first set another branch as the default branch.</p>
newtype DefaultBranchCannotBeDeletedException = DefaultBranchCannotBeDeletedException 
  { 
  }


-- | <p>Represents the input of a delete branch operation.</p>
newtype DeleteBranchInput = DeleteBranchInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BranchName'" :: (BranchName)
  }


-- | <p>Represents the output of a delete branch operation.</p>
newtype DeleteBranchOutput = DeleteBranchOutput 
  { "DeletedBranch'" :: NullOrUndefined (BranchInfo)
  }


newtype DeleteCommentContentInput = DeleteCommentContentInput 
  { "CommentId'" :: (CommentId)
  }


newtype DeleteCommentContentOutput = DeleteCommentContentOutput 
  { "Comment'" :: NullOrUndefined (Comment)
  }


-- | <p>Represents the input of a delete repository operation.</p>
newtype DeleteRepositoryInput = DeleteRepositoryInput 
  { "RepositoryName'" :: (RepositoryName)
  }


-- | <p>Represents the output of a delete repository operation.</p>
newtype DeleteRepositoryOutput = DeleteRepositoryOutput 
  { "RepositoryId'" :: NullOrUndefined (RepositoryId)
  }


newtype DescribePullRequestEventsInput = DescribePullRequestEventsInput 
  { "PullRequestId'" :: (PullRequestId)
  , "PullRequestEventType'" :: NullOrUndefined (PullRequestEventType)
  , "ActorArn'" :: NullOrUndefined (Arn)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype DescribePullRequestEventsOutput = DescribePullRequestEventsOutput 
  { "PullRequestEvents'" :: (PullRequestEventList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype Description = Description String


-- | <p>Returns information about a set of differences for a commit specifier.</p>
newtype Difference = Difference 
  { "BeforeBlob'" :: NullOrUndefined (BlobMetadata)
  , "AfterBlob'" :: NullOrUndefined (BlobMetadata)
  , "ChangeType'" :: NullOrUndefined (ChangeTypeEnum)
  }


newtype DifferenceList = DifferenceList (Array Difference)


-- | <p>A file cannot be added to the repository because the specified path name has the same name as a file that already exists in this repository. Either provide a different name for the file, or specify a different path for the file.</p>
newtype DirectoryNameConflictsWithFileNameException = DirectoryNameConflictsWithFileNameException 
  { 
  }


newtype Email = Email String


-- | <p>An encryption integrity check failed.</p>
newtype EncryptionIntegrityChecksFailedException = EncryptionIntegrityChecksFailedException 
  { 
  }


-- | <p>An encryption key could not be accessed.</p>
newtype EncryptionKeyAccessDeniedException = EncryptionKeyAccessDeniedException 
  { 
  }


-- | <p>The encryption key is disabled.</p>
newtype EncryptionKeyDisabledException = EncryptionKeyDisabledException 
  { 
  }


-- | <p>No encryption key was found.</p>
newtype EncryptionKeyNotFoundException = EncryptionKeyNotFoundException 
  { 
  }


-- | <p>The encryption key is not available.</p>
newtype EncryptionKeyUnavailableException = EncryptionKeyUnavailableException 
  { 
  }


newtype EventDate = EventDate Number


newtype FileContent = FileContent String


-- | <p>The file cannot be added because it is empty. Empty files cannot be added to the repository with this API.</p>
newtype FileContentRequiredException = FileContentRequiredException 
  { 
  }


-- | <p>The file cannot be added because it is too large. The maximum file size that can be added using PutFile is 6 MB. For files larger than 6 MB but smaller than 2 GB, add them using a Git client.</p>
newtype FileContentSizeLimitExceededException = FileContentSizeLimitExceededException 
  { 
  }


newtype FileModeTypeEnum = FileModeTypeEnum String


-- | <p>A file cannot be added to the repository because the specified file name has the same name as a directory in this repository. Either provide another name for the file, or add the file in a directory that does not match the file name.</p>
newtype FileNameConflictsWithDirectoryNameException = FileNameConflictsWithDirectoryNameException 
  { 
  }


-- | <p>The specified file exceeds the file size limit for AWS CodeCommit. For more information about limits in AWS CodeCommit, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/limits.html">AWS CodeCommit User Guide</a>.</p>
newtype FileTooLargeException = FileTooLargeException 
  { 
  }


-- | <p>Represents the input of a get blob operation.</p>
newtype GetBlobInput = GetBlobInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BlobId'" :: (ObjectId)
  }


-- | <p>Represents the output of a get blob operation.</p>
newtype GetBlobOutput = GetBlobOutput 
  { "Content'" :: (String)
  }


-- | <p>Represents the input of a get branch operation.</p>
newtype GetBranchInput = GetBranchInput 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "BranchName'" :: NullOrUndefined (BranchName)
  }


-- | <p>Represents the output of a get branch operation.</p>
newtype GetBranchOutput = GetBranchOutput 
  { "Branch'" :: NullOrUndefined (BranchInfo)
  }


newtype GetCommentInput = GetCommentInput 
  { "CommentId'" :: (CommentId)
  }


newtype GetCommentOutput = GetCommentOutput 
  { "Comment'" :: NullOrUndefined (Comment)
  }


newtype GetCommentsForComparedCommitInput = GetCommentsForComparedCommitInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: (CommitId)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype GetCommentsForComparedCommitOutput = GetCommentsForComparedCommitOutput 
  { "CommentsForComparedCommitData'" :: NullOrUndefined (CommentsForComparedCommitData)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype GetCommentsForPullRequestInput = GetCommentsForPullRequestInput 
  { "PullRequestId'" :: (PullRequestId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined (CommitId)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype GetCommentsForPullRequestOutput = GetCommentsForPullRequestOutput 
  { "CommentsForPullRequestData'" :: NullOrUndefined (CommentsForPullRequestData)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the input of a get commit operation.</p>
newtype GetCommitInput = GetCommitInput 
  { "RepositoryName'" :: (RepositoryName)
  , "CommitId'" :: (ObjectId)
  }


-- | <p>Represents the output of a get commit operation.</p>
newtype GetCommitOutput = GetCommitOutput 
  { "Commit'" :: (Commit)
  }


newtype GetDifferencesInput = GetDifferencesInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BeforeCommitSpecifier'" :: NullOrUndefined (CommitName)
  , "AfterCommitSpecifier'" :: (CommitName)
  , "BeforePath'" :: NullOrUndefined (Path)
  , "AfterPath'" :: NullOrUndefined (Path)
  , "MaxResults" :: NullOrUndefined (Limit)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype GetDifferencesOutput = GetDifferencesOutput 
  { "Differences'" :: NullOrUndefined (DifferenceList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype GetMergeConflictsInput = GetMergeConflictsInput 
  { "RepositoryName'" :: (RepositoryName)
  , "DestinationCommitSpecifier'" :: (CommitName)
  , "SourceCommitSpecifier'" :: (CommitName)
  , "MergeOption'" :: (MergeOptionTypeEnum)
  }


newtype GetMergeConflictsOutput = GetMergeConflictsOutput 
  { "Mergeable'" :: (IsMergeable)
  , "DestinationCommitId'" :: (CommitId)
  , "SourceCommitId'" :: (CommitId)
  }


newtype GetPullRequestInput = GetPullRequestInput 
  { "PullRequestId'" :: (PullRequestId)
  }


newtype GetPullRequestOutput = GetPullRequestOutput 
  { "PullRequest'" :: (PullRequest)
  }


-- | <p>Represents the input of a get repository operation.</p>
newtype GetRepositoryInput = GetRepositoryInput 
  { "RepositoryName'" :: (RepositoryName)
  }


-- | <p>Represents the output of a get repository operation.</p>
newtype GetRepositoryOutput = GetRepositoryOutput 
  { "RepositoryMetadata'" :: NullOrUndefined (RepositoryMetadata)
  }


-- | <p>Represents the input of a get repository triggers operation.</p>
newtype GetRepositoryTriggersInput = GetRepositoryTriggersInput 
  { "RepositoryName'" :: (RepositoryName)
  }


-- | <p>Represents the output of a get repository triggers operation.</p>
newtype GetRepositoryTriggersOutput = GetRepositoryTriggersOutput 
  { "ConfigurationId'" :: NullOrUndefined (RepositoryTriggersConfigurationId)
  , "Triggers'" :: NullOrUndefined (RepositoryTriggersList)
  }


-- | <p>The client request token is not valid. Either the token is not in a valid format, or the token has been used in a previous request and cannot be re-used.</p>
newtype IdempotencyParameterMismatchException = IdempotencyParameterMismatchException 
  { 
  }


-- | <p>The Amazon Resource Name (ARN) is not valid. Make sure that you have provided the full ARN for the user who initiated the change for the pull request, and then try again.</p>
newtype InvalidActorArnException = InvalidActorArnException 
  { 
  }


-- | <p>The Amazon Resource Name (ARN) is not valid. Make sure that you have provided the full ARN for the author of the pull request, and then try again.</p>
newtype InvalidAuthorArnException = InvalidAuthorArnException 
  { 
  }


-- | <p>The specified blob is not valid.</p>
newtype InvalidBlobIdException = InvalidBlobIdException 
  { 
  }


-- | <p>The specified reference name is not valid.</p>
newtype InvalidBranchNameException = InvalidBranchNameException 
  { 
  }


-- | <p>The client request token is not valid.</p>
newtype InvalidClientRequestTokenException = InvalidClientRequestTokenException 
  { 
  }


-- | <p>The comment ID is not in a valid format. Make sure that you have provided the full comment ID.</p>
newtype InvalidCommentIdException = InvalidCommentIdException 
  { 
  }


-- | <p>The specified commit is not valid.</p>
newtype InvalidCommitException = InvalidCommitException 
  { 
  }


-- | <p>The specified commit ID is not valid.</p>
newtype InvalidCommitIdException = InvalidCommitIdException 
  { 
  }


-- | <p>The specified continuation token is not valid.</p>
newtype InvalidContinuationTokenException = InvalidContinuationTokenException 
  { 
  }


-- | <p>The pull request description is not valid. Descriptions are limited to 1,000 characters in length.</p>
newtype InvalidDescriptionException = InvalidDescriptionException 
  { 
  }


-- | <p>The destination commit specifier is not valid. You must provide a valid branch name, tag, or full commit ID. </p>
newtype InvalidDestinationCommitSpecifierException = InvalidDestinationCommitSpecifierException 
  { 
  }


-- | <p>The specified email address either contains one or more characters that are not allowed, or it exceeds the maximum number of characters allowed for an email address.</p>
newtype InvalidEmailException = InvalidEmailException 
  { 
  }


-- | <p>The location of the file is not valid. Make sure that you include the extension of the file as well as the file name.</p>
newtype InvalidFileLocationException = InvalidFileLocationException 
  { 
  }


-- | <p>The specified file mode permission is not valid. For a list of valid file mode permissions, see <a>PutFile</a>. </p>
newtype InvalidFileModeException = InvalidFileModeException 
  { 
  }


-- | <p>The position is not valid. Make sure that the line number exists in the version of the file you want to comment on.</p>
newtype InvalidFilePositionException = InvalidFilePositionException 
  { 
  }


-- | <p>The specified number of maximum results is not valid.</p>
newtype InvalidMaxResultsException = InvalidMaxResultsException 
  { 
  }


-- | <p>The specified merge option is not valid. The only valid value is FAST_FORWARD_MERGE.</p>
newtype InvalidMergeOptionException = InvalidMergeOptionException 
  { 
  }


-- | <p>The specified sort order is not valid.</p>
newtype InvalidOrderException = InvalidOrderException 
  { 
  }


-- | <p>The parent commit ID is not valid. The commit ID cannot be empty, and must match the head commit ID for the branch of the repository where you want to add or update a file.</p>
newtype InvalidParentCommitIdException = InvalidParentCommitIdException 
  { 
  }


-- | <p>The specified path is not valid.</p>
newtype InvalidPathException = InvalidPathException 
  { 
  }


-- | <p>The pull request event type is not valid. </p>
newtype InvalidPullRequestEventTypeException = InvalidPullRequestEventTypeException 
  { 
  }


-- | <p>The pull request ID is not valid. Make sure that you have provided the full ID and that the pull request is in the specified repository, and then try again.</p>
newtype InvalidPullRequestIdException = InvalidPullRequestIdException 
  { 
  }


-- | <p>The pull request status is not valid. The only valid values are <code>OPEN</code> and <code>CLOSED</code>.</p>
newtype InvalidPullRequestStatusException = InvalidPullRequestStatusException 
  { 
  }


-- | <p>The pull request status update is not valid. The only valid update is from <code>OPEN</code> to <code>CLOSED</code>.</p>
newtype InvalidPullRequestStatusUpdateException = InvalidPullRequestStatusUpdateException 
  { 
  }


-- | <p>The specified reference name format is not valid. Reference names must conform to the Git references format, for example refs/heads/master. For more information, see <a href="https://git-scm.com/book/en/v2/Git-Internals-Git-References">Git Internals - Git References</a> or consult your Git documentation.</p>
newtype InvalidReferenceNameException = InvalidReferenceNameException 
  { 
  }


-- | <p>Either the enum is not in a valid format, or the specified file version enum is not valid in respect to the current file version.</p>
newtype InvalidRelativeFileVersionEnumException = InvalidRelativeFileVersionEnumException 
  { 
  }


-- | <p>The specified repository description is not valid.</p>
newtype InvalidRepositoryDescriptionException = InvalidRepositoryDescriptionException 
  { 
  }


-- | <p>At least one specified repository name is not valid.</p> <note> <p>This exception only occurs when a specified repository name is not valid. Other exceptions occur when a required repository parameter is missing, or when a specified repository does not exist.</p> </note>
newtype InvalidRepositoryNameException = InvalidRepositoryNameException 
  { 
  }


-- | <p>One or more branch names specified for the trigger is not valid.</p>
newtype InvalidRepositoryTriggerBranchNameException = InvalidRepositoryTriggerBranchNameException 
  { 
  }


-- | <p>The custom data provided for the trigger is not valid.</p>
newtype InvalidRepositoryTriggerCustomDataException = InvalidRepositoryTriggerCustomDataException 
  { 
  }


-- | <p>The Amazon Resource Name (ARN) for the trigger is not valid for the specified destination. The most common reason for this error is that the ARN does not meet the requirements for the service type.</p>
newtype InvalidRepositoryTriggerDestinationArnException = InvalidRepositoryTriggerDestinationArnException 
  { 
  }


-- | <p>One or more events specified for the trigger is not valid. Check to make sure that all events specified match the requirements for allowed events.</p>
newtype InvalidRepositoryTriggerEventsException = InvalidRepositoryTriggerEventsException 
  { 
  }


-- | <p>The name of the trigger is not valid.</p>
newtype InvalidRepositoryTriggerNameException = InvalidRepositoryTriggerNameException 
  { 
  }


-- | <p>The region for the trigger target does not match the region for the repository. Triggers must be created in the same region as the target for the trigger.</p>
newtype InvalidRepositoryTriggerRegionException = InvalidRepositoryTriggerRegionException 
  { 
  }


-- | <p>The specified sort by value is not valid.</p>
newtype InvalidSortByException = InvalidSortByException 
  { 
  }


-- | <p>The source commit specifier is not valid. You must provide a valid branch name, tag, or full commit ID.</p>
newtype InvalidSourceCommitSpecifierException = InvalidSourceCommitSpecifierException 
  { 
  }


-- | <p>The target for the pull request is not valid. A target must contain the full values for the repository name, source branch, and destination branch for the pull request.</p>
newtype InvalidTargetException = InvalidTargetException 
  { 
  }


-- | <p>The targets for the pull request is not valid or not in a valid format. Targets are a list of target objects. Each target object must contain the full values for the repository name, source branch, and destination branch for a pull request.</p>
newtype InvalidTargetsException = InvalidTargetsException 
  { 
  }


-- | <p>The title of the pull request is not valid. Pull request titles cannot exceed 100 characters in length.</p>
newtype InvalidTitleException = InvalidTitleException 
  { 
  }


newtype IsCommentDeleted = IsCommentDeleted Boolean


newtype IsMergeable = IsMergeable Boolean


newtype IsMerged = IsMerged Boolean


newtype LastModifiedDate = LastModifiedDate Number


newtype Limit = Limit Int


-- | <p>Represents the input of a list branches operation.</p>
newtype ListBranchesInput = ListBranchesInput 
  { "RepositoryName'" :: (RepositoryName)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of a list branches operation.</p>
newtype ListBranchesOutput = ListBranchesOutput 
  { "Branches'" :: NullOrUndefined (BranchNameList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListPullRequestsInput = ListPullRequestsInput 
  { "RepositoryName'" :: (RepositoryName)
  , "AuthorArn'" :: NullOrUndefined (Arn)
  , "PullRequestStatus'" :: NullOrUndefined (PullRequestStatusEnum)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype ListPullRequestsOutput = ListPullRequestsOutput 
  { "PullRequestIds'" :: (PullRequestIdList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the input of a list repositories operation.</p>
newtype ListRepositoriesInput = ListRepositoriesInput 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "SortBy'" :: NullOrUndefined (SortByEnum)
  , "Order'" :: NullOrUndefined (OrderEnum)
  }


-- | <p>Represents the output of a list repositories operation.</p>
newtype ListRepositoriesOutput = ListRepositoriesOutput 
  { "Repositories'" :: NullOrUndefined (RepositoryNameIdPairList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Returns information about the location of a change or comment in the comparison between two commits or a pull request.</p>
newtype Location = Location 
  { "FilePath'" :: NullOrUndefined (Path)
  , "FilePosition'" :: NullOrUndefined (Position)
  , "RelativeFileVersion'" :: NullOrUndefined (RelativeFileVersionEnum)
  }


-- | <p>The pull request cannot be merged automatically into the destination branch. You must manually merge the branches and resolve any conflicts.</p>
newtype ManualMergeRequiredException = ManualMergeRequiredException 
  { 
  }


newtype MaxResults = MaxResults Int


-- | <p>The number of branches for the trigger was exceeded.</p>
newtype MaximumBranchesExceededException = MaximumBranchesExceededException 
  { 
  }


-- | <p>You cannot create the pull request because the repository has too many open pull requests. The maximum number of open pull requests for a repository is 1,000. Close one or more open pull requests, and then try again.</p>
newtype MaximumOpenPullRequestsExceededException = MaximumOpenPullRequestsExceededException 
  { 
  }


-- | <p>The maximum number of allowed repository names was exceeded. Currently, this number is 25.</p>
newtype MaximumRepositoryNamesExceededException = MaximumRepositoryNamesExceededException 
  { 
  }


-- | <p>The number of triggers allowed for the repository was exceeded.</p>
newtype MaximumRepositoryTriggersExceededException = MaximumRepositoryTriggersExceededException 
  { 
  }


-- | <p>Returns information about a merge or potential merge between a source reference and a destination reference in a pull request.</p>
newtype MergeMetadata = MergeMetadata 
  { "IsMerged'" :: NullOrUndefined (IsMerged)
  , "MergedBy'" :: NullOrUndefined (Arn)
  }


-- | <p>A merge option or stategy is required, and none was provided.</p>
newtype MergeOptionRequiredException = MergeOptionRequiredException 
  { 
  }


newtype MergeOptionTypeEnum = MergeOptionTypeEnum String


newtype MergePullRequestByFastForwardInput = MergePullRequestByFastForwardInput 
  { "PullRequestId'" :: (PullRequestId)
  , "RepositoryName'" :: (RepositoryName)
  , "SourceCommitId'" :: NullOrUndefined (CommitId)
  }


newtype MergePullRequestByFastForwardOutput = MergePullRequestByFastForwardOutput 
  { "PullRequest'" :: NullOrUndefined (PullRequest)
  }


newtype Message = Message String


newtype Mode = Mode String


-- | <p>You cannot include more than one repository in a pull request. Make sure you have specified only one repository name in your request, and then try again.</p>
newtype MultipleRepositoriesInPullRequestException = MultipleRepositoriesInPullRequestException 
  { 
  }


newtype Name = Name String


-- | <p>The file name is not valid because it has exceeded the character limit for file names. File names, including the path to the file, cannot exceed the character limit. </p>
newtype NameLengthExceededException = NameLengthExceededException 
  { 
  }


newtype NextToken = NextToken String


newtype ObjectId = ObjectId String


newtype OrderEnum = OrderEnum String


-- | <p>The parent commit ID is not valid. The specified parent commit ID does not exist in the specified branch of the repository.</p>
newtype ParentCommitDoesNotExistException = ParentCommitDoesNotExistException 
  { 
  }


-- | <p>The file could not be added because the provided parent commit ID is not the current tip of the specified branch. To view the full commit ID of the current head of the branch, use <a>GetBranch</a>.</p>
newtype ParentCommitIdOutdatedException = ParentCommitIdOutdatedException 
  { 
  }


-- | <p>A parent commit ID is required. To view the full commit ID of a branch in a repository, use <a>GetBranch</a> or a Git command (for example, git pull or git log).</p>
newtype ParentCommitIdRequiredException = ParentCommitIdRequiredException 
  { 
  }


newtype ParentList = ParentList (Array ObjectId)


newtype Path = Path String


-- | <p>The specified path does not exist.</p>
newtype PathDoesNotExistException = PathDoesNotExistException 
  { 
  }


-- | <p>The filePath for a location cannot be empty or null.</p>
newtype PathRequiredException = PathRequiredException 
  { 
  }


newtype Position = Position Number


newtype PostCommentForComparedCommitInput = PostCommentForComparedCommitInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: (CommitId)
  , "Location'" :: NullOrUndefined (Location)
  , "Content'" :: (Content)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  }


newtype PostCommentForComparedCommitOutput = PostCommentForComparedCommitOutput 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined (CommitId)
  , "BeforeBlobId'" :: NullOrUndefined (ObjectId)
  , "AfterBlobId'" :: NullOrUndefined (ObjectId)
  , "Location'" :: NullOrUndefined (Location)
  , "Comment'" :: NullOrUndefined (Comment)
  }


newtype PostCommentForPullRequestInput = PostCommentForPullRequestInput 
  { "PullRequestId'" :: (PullRequestId)
  , "RepositoryName'" :: (RepositoryName)
  , "BeforeCommitId'" :: (CommitId)
  , "AfterCommitId'" :: (CommitId)
  , "Location'" :: NullOrUndefined (Location)
  , "Content'" :: (Content)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  }


newtype PostCommentForPullRequestOutput = PostCommentForPullRequestOutput 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "PullRequestId'" :: NullOrUndefined (PullRequestId)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined (CommitId)
  , "BeforeBlobId'" :: NullOrUndefined (ObjectId)
  , "AfterBlobId'" :: NullOrUndefined (ObjectId)
  , "Location'" :: NullOrUndefined (Location)
  , "Comment'" :: NullOrUndefined (Comment)
  }


newtype PostCommentReplyInput = PostCommentReplyInput 
  { "InReplyTo'" :: (CommentId)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  , "Content'" :: (Content)
  }


newtype PostCommentReplyOutput = PostCommentReplyOutput 
  { "Comment'" :: NullOrUndefined (Comment)
  }


-- | <p>Returns information about a pull request.</p>
newtype PullRequest = PullRequest 
  { "PullRequestId'" :: NullOrUndefined (PullRequestId)
  , "Title'" :: NullOrUndefined (Title)
  , "Description'" :: NullOrUndefined (Description)
  , "LastActivityDate'" :: NullOrUndefined (LastModifiedDate)
  , "CreationDate'" :: NullOrUndefined (CreationDate)
  , "PullRequestStatus'" :: NullOrUndefined (PullRequestStatusEnum)
  , "AuthorArn'" :: NullOrUndefined (Arn)
  , "PullRequestTargets'" :: NullOrUndefined (PullRequestTargetList)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  }


-- | <p>The pull request status cannot be updated because it is already closed.</p>
newtype PullRequestAlreadyClosedException = PullRequestAlreadyClosedException 
  { 
  }


-- | <p>The pull request ID could not be found. Make sure that you have specified the correct repository name and pull request ID, and then try again.</p>
newtype PullRequestDoesNotExistException = PullRequestDoesNotExistException 
  { 
  }


-- | <p>Returns information about a pull request event.</p>
newtype PullRequestEvent = PullRequestEvent 
  { "PullRequestId'" :: NullOrUndefined (PullRequestId)
  , "EventDate'" :: NullOrUndefined (EventDate)
  , "PullRequestEventType'" :: NullOrUndefined (PullRequestEventType)
  , "ActorArn'" :: NullOrUndefined (Arn)
  , "PullRequestStatusChangedEventMetadata'" :: NullOrUndefined (PullRequestStatusChangedEventMetadata)
  , "PullRequestSourceReferenceUpdatedEventMetadata'" :: NullOrUndefined (PullRequestSourceReferenceUpdatedEventMetadata)
  , "PullRequestMergedStateChangedEventMetadata'" :: NullOrUndefined (PullRequestMergedStateChangedEventMetadata)
  }


newtype PullRequestEventList = PullRequestEventList (Array PullRequestEvent)


newtype PullRequestEventType = PullRequestEventType String


newtype PullRequestId = PullRequestId String


newtype PullRequestIdList = PullRequestIdList (Array PullRequestId)


-- | <p>A pull request ID is required, but none was provided.</p>
newtype PullRequestIdRequiredException = PullRequestIdRequiredException 
  { 
  }


-- | <p>Returns information about the change in the merge state for a pull request event. </p>
newtype PullRequestMergedStateChangedEventMetadata = PullRequestMergedStateChangedEventMetadata 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "DestinationReference'" :: NullOrUndefined (ReferenceName)
  , "MergeMetadata'" :: NullOrUndefined (MergeMetadata)
  }


-- | <p>Information about an update to the source branch of a pull request.</p>
newtype PullRequestSourceReferenceUpdatedEventMetadata = PullRequestSourceReferenceUpdatedEventMetadata 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined (CommitId)
  }


-- | <p>Information about a change to the status of a pull request.</p>
newtype PullRequestStatusChangedEventMetadata = PullRequestStatusChangedEventMetadata 
  { "PullRequestStatus'" :: NullOrUndefined (PullRequestStatusEnum)
  }


newtype PullRequestStatusEnum = PullRequestStatusEnum String


-- | <p>A pull request status is required, but none was provided.</p>
newtype PullRequestStatusRequiredException = PullRequestStatusRequiredException 
  { 
  }


-- | <p>Returns information about a pull request target.</p>
newtype PullRequestTarget = PullRequestTarget 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "SourceReference'" :: NullOrUndefined (ReferenceName)
  , "DestinationReference'" :: NullOrUndefined (ReferenceName)
  , "DestinationCommit'" :: NullOrUndefined (CommitId)
  , "SourceCommit'" :: NullOrUndefined (CommitId)
  , "MergeMetadata'" :: NullOrUndefined (MergeMetadata)
  }


newtype PullRequestTargetList = PullRequestTargetList (Array PullRequestTarget)


newtype PutFileInput = PutFileInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BranchName'" :: (BranchName)
  , "FileContent'" :: (FileContent)
  , "FilePath'" :: (Path)
  , "FileMode'" :: NullOrUndefined (FileModeTypeEnum)
  , "ParentCommitId'" :: NullOrUndefined (CommitId)
  , "CommitMessage'" :: NullOrUndefined (Message)
  , "Name'" :: NullOrUndefined (Name)
  , "Email'" :: NullOrUndefined (Email)
  }


newtype PutFileOutput = PutFileOutput 
  { "CommitId'" :: (ObjectId)
  , "BlobId'" :: (ObjectId)
  , "TreeId'" :: (ObjectId)
  }


-- | <p>Represents the input ofa put repository triggers operation.</p>
newtype PutRepositoryTriggersInput = PutRepositoryTriggersInput 
  { "RepositoryName'" :: (RepositoryName)
  , "Triggers'" :: (RepositoryTriggersList)
  }


-- | <p>Represents the output of a put repository triggers operation.</p>
newtype PutRepositoryTriggersOutput = PutRepositoryTriggersOutput 
  { "ConfigurationId'" :: NullOrUndefined (RepositoryTriggersConfigurationId)
  }


-- | <p>The specified reference does not exist. You must provide a full commit ID.</p>
newtype ReferenceDoesNotExistException = ReferenceDoesNotExistException 
  { 
  }


newtype ReferenceName = ReferenceName String


-- | <p>A reference name is required, but none was provided.</p>
newtype ReferenceNameRequiredException = ReferenceNameRequiredException 
  { 
  }


-- | <p>The specified reference is not a supported type. </p>
newtype ReferenceTypeNotSupportedException = ReferenceTypeNotSupportedException 
  { 
  }


newtype RelativeFileVersionEnum = RelativeFileVersionEnum String


newtype RepositoryDescription = RepositoryDescription String


-- | <p>The specified repository does not exist.</p>
newtype RepositoryDoesNotExistException = RepositoryDoesNotExistException 
  { 
  }


newtype RepositoryId = RepositoryId String


-- | <p>A repository resource limit was exceeded.</p>
newtype RepositoryLimitExceededException = RepositoryLimitExceededException 
  { 
  }


-- | <p>Information about a repository.</p>
newtype RepositoryMetadata = RepositoryMetadata 
  { "AccountId'" :: NullOrUndefined (AccountId)
  , "RepositoryId'" :: NullOrUndefined (RepositoryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "RepositoryDescription'" :: NullOrUndefined (RepositoryDescription)
  , "DefaultBranch'" :: NullOrUndefined (BranchName)
  , "LastModifiedDate'" :: NullOrUndefined (LastModifiedDate)
  , "CreationDate'" :: NullOrUndefined (CreationDate)
  , "CloneUrlHttp'" :: NullOrUndefined (CloneUrlHttp)
  , "CloneUrlSsh'" :: NullOrUndefined (CloneUrlSsh)
  , "Arn" :: NullOrUndefined (Arn)
  }


newtype RepositoryMetadataList = RepositoryMetadataList (Array RepositoryMetadata)


newtype RepositoryName = RepositoryName String


-- | <p>The specified repository name already exists.</p>
newtype RepositoryNameExistsException = RepositoryNameExistsException 
  { 
  }


-- | <p>Information about a repository name and ID.</p>
newtype RepositoryNameIdPair = RepositoryNameIdPair 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "RepositoryId'" :: NullOrUndefined (RepositoryId)
  }


newtype RepositoryNameIdPairList = RepositoryNameIdPairList (Array RepositoryNameIdPair)


newtype RepositoryNameList = RepositoryNameList (Array RepositoryName)


-- | <p>A repository name is required but was not specified.</p>
newtype RepositoryNameRequiredException = RepositoryNameRequiredException 
  { 
  }


-- | <p>A repository names object is required but was not specified.</p>
newtype RepositoryNamesRequiredException = RepositoryNamesRequiredException 
  { 
  }


-- | <p>The repository does not contain any pull requests with that pull request ID. Check to make sure you have provided the correct repository name for the pull request.</p>
newtype RepositoryNotAssociatedWithPullRequestException = RepositoryNotAssociatedWithPullRequestException 
  { 
  }


newtype RepositoryNotFoundList = RepositoryNotFoundList (Array RepositoryName)


-- | <p>Information about a trigger for a repository.</p>
newtype RepositoryTrigger = RepositoryTrigger 
  { "Name'" :: (RepositoryTriggerName)
  , "DestinationArn'" :: (Arn)
  , "CustomData'" :: NullOrUndefined (RepositoryTriggerCustomData)
  , "Branches'" :: NullOrUndefined (BranchNameList)
  , "Events'" :: (RepositoryTriggerEventList)
  }


-- | <p>At least one branch name is required but was not specified in the trigger configuration.</p>
newtype RepositoryTriggerBranchNameListRequiredException = RepositoryTriggerBranchNameListRequiredException 
  { 
  }


newtype RepositoryTriggerCustomData = RepositoryTriggerCustomData String


-- | <p>A destination ARN for the target service for the trigger is required but was not specified.</p>
newtype RepositoryTriggerDestinationArnRequiredException = RepositoryTriggerDestinationArnRequiredException 
  { 
  }


newtype RepositoryTriggerEventEnum = RepositoryTriggerEventEnum String


newtype RepositoryTriggerEventList = RepositoryTriggerEventList (Array RepositoryTriggerEventEnum)


-- | <p>At least one event for the trigger is required but was not specified.</p>
newtype RepositoryTriggerEventsListRequiredException = RepositoryTriggerEventsListRequiredException 
  { 
  }


-- | <p>A trigger failed to run.</p>
newtype RepositoryTriggerExecutionFailure = RepositoryTriggerExecutionFailure 
  { "Trigger'" :: NullOrUndefined (RepositoryTriggerName)
  , "FailureMessage'" :: NullOrUndefined (RepositoryTriggerExecutionFailureMessage)
  }


newtype RepositoryTriggerExecutionFailureList = RepositoryTriggerExecutionFailureList (Array RepositoryTriggerExecutionFailure)


newtype RepositoryTriggerExecutionFailureMessage = RepositoryTriggerExecutionFailureMessage String


newtype RepositoryTriggerName = RepositoryTriggerName String


newtype RepositoryTriggerNameList = RepositoryTriggerNameList (Array RepositoryTriggerName)


-- | <p>A name for the trigger is required but was not specified.</p>
newtype RepositoryTriggerNameRequiredException = RepositoryTriggerNameRequiredException 
  { 
  }


newtype RepositoryTriggersConfigurationId = RepositoryTriggersConfigurationId String


newtype RepositoryTriggersList = RepositoryTriggersList (Array RepositoryTrigger)


-- | <p>The list of triggers for the repository is required but was not specified.</p>
newtype RepositoryTriggersListRequiredException = RepositoryTriggersListRequiredException 
  { 
  }


-- | <p>The file was not added or updated because the content of the file is exactly the same as the content of that file in the repository and branch that you specified.</p>
newtype SameFileContentException = SameFileContentException 
  { 
  }


newtype SortByEnum = SortByEnum String


-- | <p>The source branch and the destination branch for the pull request are the same. You must specify different branches for the source and destination.</p>
newtype SourceAndDestinationAreSameException = SourceAndDestinationAreSameException 
  { 
  }


-- | <p>Returns information about a target for a pull request.</p>
newtype Target = Target 
  { "RepositoryName'" :: (RepositoryName)
  , "SourceReference'" :: (ReferenceName)
  , "DestinationReference'" :: NullOrUndefined (ReferenceName)
  }


newtype TargetList = TargetList (Array Target)


-- | <p>A pull request target is required. It cannot be empty or null. A pull request target must contain the full values for the repository name, source branch, and destination branch for the pull request.</p>
newtype TargetRequiredException = TargetRequiredException 
  { 
  }


-- | <p>An array of target objects is required. It cannot be empty or null.</p>
newtype TargetsRequiredException = TargetsRequiredException 
  { 
  }


-- | <p>Represents the input of a test repository triggers operation.</p>
newtype TestRepositoryTriggersInput = TestRepositoryTriggersInput 
  { "RepositoryName'" :: (RepositoryName)
  , "Triggers'" :: (RepositoryTriggersList)
  }


-- | <p>Represents the output of a test repository triggers operation.</p>
newtype TestRepositoryTriggersOutput = TestRepositoryTriggersOutput 
  { "SuccessfulExecutions'" :: NullOrUndefined (RepositoryTriggerNameList)
  , "FailedExecutions'" :: NullOrUndefined (RepositoryTriggerExecutionFailureList)
  }


-- | <p>The tip of the source branch in the destination repository does not match the tip of the source branch specified in your request. The pull request might have been updated. Make sure that you have the latest changes.</p>
newtype TipOfSourceReferenceIsDifferentException = TipOfSourceReferenceIsDifferentException 
  { 
  }


-- | <p>The divergence between the tips of the provided commit specifiers is too great to determine whether there might be any merge conflicts. Locally compare the specifiers using <code>git diff</code> or a diff tool.</p>
newtype TipsDivergenceExceededException = TipsDivergenceExceededException 
  { 
  }


newtype Title = Title String


-- | <p>A pull request title is required. It cannot be empty or null.</p>
newtype TitleRequiredException = TitleRequiredException 
  { 
  }


newtype UpdateCommentInput = UpdateCommentInput 
  { "CommentId'" :: (CommentId)
  , "Content'" :: (Content)
  }


newtype UpdateCommentOutput = UpdateCommentOutput 
  { "Comment'" :: NullOrUndefined (Comment)
  }


-- | <p>Represents the input of an update default branch operation.</p>
newtype UpdateDefaultBranchInput = UpdateDefaultBranchInput 
  { "RepositoryName'" :: (RepositoryName)
  , "DefaultBranchName'" :: (BranchName)
  }


newtype UpdatePullRequestDescriptionInput = UpdatePullRequestDescriptionInput 
  { "PullRequestId'" :: (PullRequestId)
  , "Description'" :: (Description)
  }


newtype UpdatePullRequestDescriptionOutput = UpdatePullRequestDescriptionOutput 
  { "PullRequest'" :: (PullRequest)
  }


newtype UpdatePullRequestStatusInput = UpdatePullRequestStatusInput 
  { "PullRequestId'" :: (PullRequestId)
  , "PullRequestStatus'" :: (PullRequestStatusEnum)
  }


newtype UpdatePullRequestStatusOutput = UpdatePullRequestStatusOutput 
  { "PullRequest'" :: (PullRequest)
  }


newtype UpdatePullRequestTitleInput = UpdatePullRequestTitleInput 
  { "PullRequestId'" :: (PullRequestId)
  , "Title'" :: (Title)
  }


newtype UpdatePullRequestTitleOutput = UpdatePullRequestTitleOutput 
  { "PullRequest'" :: (PullRequest)
  }


-- | <p>Represents the input of an update repository description operation.</p>
newtype UpdateRepositoryDescriptionInput = UpdateRepositoryDescriptionInput 
  { "RepositoryName'" :: (RepositoryName)
  , "RepositoryDescription'" :: NullOrUndefined (RepositoryDescription)
  }


-- | <p>Represents the input of an update repository description operation.</p>
newtype UpdateRepositoryNameInput = UpdateRepositoryNameInput 
  { "OldName'" :: (RepositoryName)
  , "NewName'" :: (RepositoryName)
  }


-- | <p>Information about the user who made a specified commit.</p>
newtype UserInfo = UserInfo 
  { "Name'" :: NullOrUndefined (Name)
  , "Email'" :: NullOrUndefined (Email)
  , "Date'" :: NullOrUndefined (Date)
  }
