

-- | <fullname>AWS CodeCommit</fullname> <p>This is the <i>AWS CodeCommit API Reference</i>. This reference provides descriptions of the operations and data types for AWS CodeCommit API along with usage examples.</p> <p>You can use the AWS CodeCommit API to work with the following objects:</p> <p>Repositories, by calling the following:</p> <ul> <li> <p> <a>BatchGetRepositories</a>, which returns information about one or more repositories associated with your AWS account.</p> </li> <li> <p> <a>CreateRepository</a>, which creates an AWS CodeCommit repository.</p> </li> <li> <p> <a>DeleteRepository</a>, which deletes an AWS CodeCommit repository.</p> </li> <li> <p> <a>GetRepository</a>, which returns information about a specified repository.</p> </li> <li> <p> <a>ListRepositories</a>, which lists all AWS CodeCommit repositories associated with your AWS account.</p> </li> <li> <p> <a>UpdateRepositoryDescription</a>, which sets or updates the description of the repository.</p> </li> <li> <p> <a>UpdateRepositoryName</a>, which changes the name of the repository. If you change the name of a repository, no other users of that repository will be able to access it until you send them the new HTTPS or SSH URL to use.</p> </li> </ul> <p>Branches, by calling the following:</p> <ul> <li> <p> <a>CreateBranch</a>, which creates a new branch in a specified repository.</p> </li> <li> <p> <a>DeleteBranch</a>, which deletes the specified branch in a repository unless it is the default branch.</p> </li> <li> <p> <a>GetBranch</a>, which returns information about a specified branch.</p> </li> <li> <p> <a>ListBranches</a>, which lists all branches for a specified repository.</p> </li> <li> <p> <a>UpdateDefaultBranch</a>, which changes the default branch for a repository.</p> </li> </ul> <p>Files, by calling the following:</p> <ul> <li> <p> <a>PutFile</a>, which adds or modifies a file in a specified repository and branch.</p> </li> </ul> <p>Information about committed code in a repository, by calling the following:</p> <ul> <li> <p> <a>GetBlob</a>, which returns the base-64 encoded content of an individual Git blob object within a repository.</p> </li> <li> <p> <a>GetCommit</a>, which returns information about a commit, including commit messages and author and committer information.</p> </li> <li> <p> <a>GetDifferences</a>, which returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID or other fully qualified reference).</p> </li> </ul> <p>Pull requests, by calling the following:</p> <ul> <li> <p> <a>CreatePullRequest</a>, which creates a pull request in a specified repository.</p> </li> <li> <p> <a>DescribePullRequestEvents</a>, which returns information about one or more pull request events.</p> </li> <li> <p> <a>GetCommentsForPullRequest</a>, which returns information about comments on a specified pull request.</p> </li> <li> <p> <a>GetMergeConflicts</a>, which returns information about merge conflicts between the source and destination branch in a pull request.</p> </li> <li> <p> <a>GetPullRequest</a>, which returns information about a specified pull request.</p> </li> <li> <p> <a>ListPullRequests</a>, which lists all pull requests for a repository.</p> </li> <li> <p> <a>MergePullRequestByFastForward</a>, which merges the source destination branch of a pull request into the specified destination branch for that pull request using the fast-forward merge option.</p> </li> <li> <p> <a>PostCommentForPullRequest</a>, which posts a comment to a pull request at the specified line, file, or request.</p> </li> <li> <p> <a>UpdatePullRequestDescription</a>, which updates the description of a pull request.</p> </li> <li> <p> <a>UpdatePullRequestStatus</a>, which updates the status of a pull request.</p> </li> <li> <p> <a>UpdatePullRequestTitle</a>, which updates the title of a pull request.</p> </li> </ul> <p>Information about comments in a repository, by calling the following:</p> <ul> <li> <p> <a>DeleteCommentContent</a>, which deletes the content of a comment on a commit in a repository.</p> </li> <li> <p> <a>GetComment</a>, which returns information about a comment on a commit.</p> </li> <li> <p> <a>GetCommentsForComparedCommit</a>, which returns information about comments on the comparison between two commit specifiers in a repository.</p> </li> <li> <p> <a>PostCommentForComparedCommit</a>, which creates a comment on the comparison between two commit specifiers in a repository.</p> </li> <li> <p> <a>PostCommentReply</a>, which creates a reply to a comment.</p> </li> <li> <p> <a>UpdateComment</a>, which updates the content of a comment on a commit in a repository.</p> </li> </ul> <p>Triggers, by calling the following:</p> <ul> <li> <p> <a>GetRepositoryTriggers</a>, which returns information about triggers configured for a repository.</p> </li> <li> <p> <a>PutRepositoryTriggers</a>, which replaces all triggers for a repository and can be used to create or delete triggers.</p> </li> <li> <p> <a>TestRepositoryTriggers</a>, which tests the functionality of a repository trigger by sending data to the trigger target.</p> </li> </ul> <p>For information about how to use AWS CodeCommit, see the <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html">AWS CodeCommit User Guide</a>.</p>
module AWS.CodeCommit where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CodeCommit" :: String


-- | <p>Returns information about one or more repositories.</p> <note> <p>The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.</p> </note>
batchGetRepositories :: forall eff. BatchGetRepositoriesInput -> Aff (err :: AWS.RequestError | eff) BatchGetRepositoriesOutput
batchGetRepositories = AWS.request serviceName "batchGetRepositories" 


-- | <p>Creates a new branch in a repository and points the branch to a commit.</p> <note> <p>Calling the create branch operation does not set a repository's default branch. To do this, call the update default branch operation.</p> </note>
createBranch :: forall eff. CreateBranchInput -> Aff (err :: AWS.RequestError | eff) Unit
createBranch = AWS.request serviceName "createBranch" 


-- | <p>Creates a pull request in the specified repository.</p>
createPullRequest :: forall eff. CreatePullRequestInput -> Aff (err :: AWS.RequestError | eff) CreatePullRequestOutput
createPullRequest = AWS.request serviceName "createPullRequest" 


-- | <p>Creates a new, empty repository.</p>
createRepository :: forall eff. CreateRepositoryInput -> Aff (err :: AWS.RequestError | eff) CreateRepositoryOutput
createRepository = AWS.request serviceName "createRepository" 


-- | <p>Deletes a branch from a repository, unless that branch is the default branch for the repository. </p>
deleteBranch :: forall eff. DeleteBranchInput -> Aff (err :: AWS.RequestError | eff) DeleteBranchOutput
deleteBranch = AWS.request serviceName "deleteBranch" 


-- | <p>Deletes the content of a comment made on a change, file, or commit in a repository.</p>
deleteCommentContent :: forall eff. DeleteCommentContentInput -> Aff (err :: AWS.RequestError | eff) DeleteCommentContentOutput
deleteCommentContent = AWS.request serviceName "deleteCommentContent" 


-- | <p>Deletes a repository. If a specified repository was already deleted, a null repository ID will be returned.</p> <important> <p>Deleting a repository also deletes all associated objects and metadata. After a repository is deleted, all future push calls to the deleted repository will fail.</p> </important>
deleteRepository :: forall eff. DeleteRepositoryInput -> Aff (err :: AWS.RequestError | eff) DeleteRepositoryOutput
deleteRepository = AWS.request serviceName "deleteRepository" 


-- | <p>Returns information about one or more pull request events.</p>
describePullRequestEvents :: forall eff. DescribePullRequestEventsInput -> Aff (err :: AWS.RequestError | eff) DescribePullRequestEventsOutput
describePullRequestEvents = AWS.request serviceName "describePullRequestEvents" 


-- | <p>Returns the base-64 encoded content of an individual blob within a repository.</p>
getBlob :: forall eff. GetBlobInput -> Aff (err :: AWS.RequestError | eff) GetBlobOutput
getBlob = AWS.request serviceName "getBlob" 


-- | <p>Returns information about a repository branch, including its name and the last commit ID.</p>
getBranch :: forall eff. GetBranchInput -> Aff (err :: AWS.RequestError | eff) GetBranchOutput
getBranch = AWS.request serviceName "getBranch" 


-- | <p>Returns the content of a comment made on a change, file, or commit in a repository.</p>
getComment :: forall eff. GetCommentInput -> Aff (err :: AWS.RequestError | eff) GetCommentOutput
getComment = AWS.request serviceName "getComment" 


-- | <p>Returns information about comments made on the comparison between two commits.</p>
getCommentsForComparedCommit :: forall eff. GetCommentsForComparedCommitInput -> Aff (err :: AWS.RequestError | eff) GetCommentsForComparedCommitOutput
getCommentsForComparedCommit = AWS.request serviceName "getCommentsForComparedCommit" 


-- | <p>Returns comments made on a pull request.</p>
getCommentsForPullRequest :: forall eff. GetCommentsForPullRequestInput -> Aff (err :: AWS.RequestError | eff) GetCommentsForPullRequestOutput
getCommentsForPullRequest = AWS.request serviceName "getCommentsForPullRequest" 


-- | <p>Returns information about a commit, including commit message and committer information.</p>
getCommit :: forall eff. GetCommitInput -> Aff (err :: AWS.RequestError | eff) GetCommitOutput
getCommit = AWS.request serviceName "getCommit" 


-- | <p>Returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID or other fully qualified reference). Results can be limited to a specified path.</p>
getDifferences :: forall eff. GetDifferencesInput -> Aff (err :: AWS.RequestError | eff) GetDifferencesOutput
getDifferences = AWS.request serviceName "getDifferences" 


-- | <p>Returns information about merge conflicts between the before and after commit IDs for a pull request in a repository.</p>
getMergeConflicts :: forall eff. GetMergeConflictsInput -> Aff (err :: AWS.RequestError | eff) GetMergeConflictsOutput
getMergeConflicts = AWS.request serviceName "getMergeConflicts" 


-- | <p>Gets information about a pull request in a specified repository.</p>
getPullRequest :: forall eff. GetPullRequestInput -> Aff (err :: AWS.RequestError | eff) GetPullRequestOutput
getPullRequest = AWS.request serviceName "getPullRequest" 


-- | <p>Returns information about a repository.</p> <note> <p>The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.</p> </note>
getRepository :: forall eff. GetRepositoryInput -> Aff (err :: AWS.RequestError | eff) GetRepositoryOutput
getRepository = AWS.request serviceName "getRepository" 


-- | <p>Gets information about triggers configured for a repository.</p>
getRepositoryTriggers :: forall eff. GetRepositoryTriggersInput -> Aff (err :: AWS.RequestError | eff) GetRepositoryTriggersOutput
getRepositoryTriggers = AWS.request serviceName "getRepositoryTriggers" 


-- | <p>Gets information about one or more branches in a repository.</p>
listBranches :: forall eff. ListBranchesInput -> Aff (err :: AWS.RequestError | eff) ListBranchesOutput
listBranches = AWS.request serviceName "listBranches" 


-- | <p>Returns a list of pull requests for a specified repository. The return list can be refined by pull request status or pull request author ARN.</p>
listPullRequests :: forall eff. ListPullRequestsInput -> Aff (err :: AWS.RequestError | eff) ListPullRequestsOutput
listPullRequests = AWS.request serviceName "listPullRequests" 


-- | <p>Gets information about one or more repositories.</p>
listRepositories :: forall eff. ListRepositoriesInput -> Aff (err :: AWS.RequestError | eff) ListRepositoriesOutput
listRepositories = AWS.request serviceName "listRepositories" 


-- | <p>Closes a pull request and attempts to merge the source commit of a pull request into the specified destination branch for that pull request at the specified commit using the fast-forward merge option.</p>
mergePullRequestByFastForward :: forall eff. MergePullRequestByFastForwardInput -> Aff (err :: AWS.RequestError | eff) MergePullRequestByFastForwardOutput
mergePullRequestByFastForward = AWS.request serviceName "mergePullRequestByFastForward" 


-- | <p>Posts a comment on the comparison between two commits.</p>
postCommentForComparedCommit :: forall eff. PostCommentForComparedCommitInput -> Aff (err :: AWS.RequestError | eff) PostCommentForComparedCommitOutput
postCommentForComparedCommit = AWS.request serviceName "postCommentForComparedCommit" 


-- | <p>Posts a comment on a pull request.</p>
postCommentForPullRequest :: forall eff. PostCommentForPullRequestInput -> Aff (err :: AWS.RequestError | eff) PostCommentForPullRequestOutput
postCommentForPullRequest = AWS.request serviceName "postCommentForPullRequest" 


-- | <p>Posts a comment in reply to an existing comment on a comparison between commits or a pull request.</p>
postCommentReply :: forall eff. PostCommentReplyInput -> Aff (err :: AWS.RequestError | eff) PostCommentReplyOutput
postCommentReply = AWS.request serviceName "postCommentReply" 


-- | <p>Adds or updates a file in an AWS CodeCommit repository.</p>
putFile :: forall eff. PutFileInput -> Aff (err :: AWS.RequestError | eff) PutFileOutput
putFile = AWS.request serviceName "putFile" 


-- | <p>Replaces all triggers for a repository. This can be used to create or delete triggers.</p>
putRepositoryTriggers :: forall eff. PutRepositoryTriggersInput -> Aff (err :: AWS.RequestError | eff) PutRepositoryTriggersOutput
putRepositoryTriggers = AWS.request serviceName "putRepositoryTriggers" 


-- | <p>Tests the functionality of repository triggers by sending information to the trigger target. If real data is available in the repository, the test will send data from the last commit. If no data is available, sample data will be generated.</p>
testRepositoryTriggers :: forall eff. TestRepositoryTriggersInput -> Aff (err :: AWS.RequestError | eff) TestRepositoryTriggersOutput
testRepositoryTriggers = AWS.request serviceName "testRepositoryTriggers" 


-- | <p>Replaces the contents of a comment.</p>
updateComment :: forall eff. UpdateCommentInput -> Aff (err :: AWS.RequestError | eff) UpdateCommentOutput
updateComment = AWS.request serviceName "updateComment" 


-- | <p>Sets or changes the default branch name for the specified repository.</p> <note> <p>If you use this operation to change the default branch name to the current default branch name, a success message is returned even though the default branch did not change.</p> </note>
updateDefaultBranch :: forall eff. UpdateDefaultBranchInput -> Aff (err :: AWS.RequestError | eff) Unit
updateDefaultBranch = AWS.request serviceName "updateDefaultBranch" 


-- | <p>Replaces the contents of the description of a pull request.</p>
updatePullRequestDescription :: forall eff. UpdatePullRequestDescriptionInput -> Aff (err :: AWS.RequestError | eff) UpdatePullRequestDescriptionOutput
updatePullRequestDescription = AWS.request serviceName "updatePullRequestDescription" 


-- | <p>Updates the status of a pull request. </p>
updatePullRequestStatus :: forall eff. UpdatePullRequestStatusInput -> Aff (err :: AWS.RequestError | eff) UpdatePullRequestStatusOutput
updatePullRequestStatus = AWS.request serviceName "updatePullRequestStatus" 


-- | <p>Replaces the title of a pull request.</p>
updatePullRequestTitle :: forall eff. UpdatePullRequestTitleInput -> Aff (err :: AWS.RequestError | eff) UpdatePullRequestTitleOutput
updatePullRequestTitle = AWS.request serviceName "updatePullRequestTitle" 


-- | <p>Sets or changes the comment or description for a repository.</p> <note> <p>The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.</p> </note>
updateRepositoryDescription :: forall eff. UpdateRepositoryDescriptionInput -> Aff (err :: AWS.RequestError | eff) Unit
updateRepositoryDescription = AWS.request serviceName "updateRepositoryDescription" 


-- | <p>Renames a repository. The repository name must be unique across the calling AWS account. In addition, repository names are limited to 100 alphanumeric, dash, and underscore characters, and cannot include certain characters. The suffix ".git" is prohibited. For a full description of the limits on repository names, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/limits.html">Limits</a> in the AWS CodeCommit User Guide.</p>
updateRepositoryName :: forall eff. UpdateRepositoryNameInput -> Aff (err :: AWS.RequestError | eff) Unit
updateRepositoryName = AWS.request serviceName "updateRepositoryName" 


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _


-- | <p>The specified Amazon Resource Name (ARN) does not exist in the AWS account.</p>
newtype ActorDoesNotExistException = ActorDoesNotExistException 
  { 
  }
derive instance newtypeActorDoesNotExistException :: Newtype ActorDoesNotExistException _


newtype AdditionalData = AdditionalData String
derive instance newtypeAdditionalData :: Newtype AdditionalData _


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _


-- | <p>The specified Amazon Resource Name (ARN) does not exist in the AWS account.</p>
newtype AuthorDoesNotExistException = AuthorDoesNotExistException 
  { 
  }
derive instance newtypeAuthorDoesNotExistException :: Newtype AuthorDoesNotExistException _


-- | <p>Represents the input of a batch get repositories operation.</p>
newtype BatchGetRepositoriesInput = BatchGetRepositoriesInput 
  { "RepositoryNames'" :: (RepositoryNameList)
  }
derive instance newtypeBatchGetRepositoriesInput :: Newtype BatchGetRepositoriesInput _


-- | <p>Represents the output of a batch get repositories operation.</p>
newtype BatchGetRepositoriesOutput = BatchGetRepositoriesOutput 
  { "Repositories'" :: NullOrUndefined (RepositoryMetadataList)
  , "RepositoriesNotFound'" :: NullOrUndefined (RepositoryNotFoundList)
  }
derive instance newtypeBatchGetRepositoriesOutput :: Newtype BatchGetRepositoriesOutput _


-- | <p>The before commit ID and the after commit ID are the same, which is not valid. The before commit ID and the after commit ID must be different commit IDs.</p>
newtype BeforeCommitIdAndAfterCommitIdAreSameException = BeforeCommitIdAndAfterCommitIdAreSameException 
  { 
  }
derive instance newtypeBeforeCommitIdAndAfterCommitIdAreSameException :: Newtype BeforeCommitIdAndAfterCommitIdAreSameException _


-- | <p>The specified blob does not exist.</p>
newtype BlobIdDoesNotExistException = BlobIdDoesNotExistException 
  { 
  }
derive instance newtypeBlobIdDoesNotExistException :: Newtype BlobIdDoesNotExistException _


-- | <p>A blob ID is required but was not specified.</p>
newtype BlobIdRequiredException = BlobIdRequiredException 
  { 
  }
derive instance newtypeBlobIdRequiredException :: Newtype BlobIdRequiredException _


-- | <p>Returns information about a specific Git blob object.</p>
newtype BlobMetadata = BlobMetadata 
  { "BlobId'" :: NullOrUndefined (ObjectId)
  , "Path'" :: NullOrUndefined (Path)
  , "Mode'" :: NullOrUndefined (Mode)
  }
derive instance newtypeBlobMetadata :: Newtype BlobMetadata _


-- | <p>The specified branch does not exist.</p>
newtype BranchDoesNotExistException = BranchDoesNotExistException 
  { 
  }
derive instance newtypeBranchDoesNotExistException :: Newtype BranchDoesNotExistException _


-- | <p>Returns information about a branch.</p>
newtype BranchInfo = BranchInfo 
  { "BranchName'" :: NullOrUndefined (BranchName)
  , "CommitId'" :: NullOrUndefined (CommitId)
  }
derive instance newtypeBranchInfo :: Newtype BranchInfo _


newtype BranchName = BranchName String
derive instance newtypeBranchName :: Newtype BranchName _


-- | <p>The specified branch name already exists.</p>
newtype BranchNameExistsException = BranchNameExistsException 
  { 
  }
derive instance newtypeBranchNameExistsException :: Newtype BranchNameExistsException _


-- | <p>The specified branch name is not valid because it is a tag name. Type the name of a current branch in the repository. For a list of valid branch names, use <a>ListBranches</a>.</p>
newtype BranchNameIsTagNameException = BranchNameIsTagNameException 
  { 
  }
derive instance newtypeBranchNameIsTagNameException :: Newtype BranchNameIsTagNameException _


newtype BranchNameList = BranchNameList (Array BranchName)
derive instance newtypeBranchNameList :: Newtype BranchNameList _


-- | <p>A branch name is required but was not specified.</p>
newtype BranchNameRequiredException = BranchNameRequiredException 
  { 
  }
derive instance newtypeBranchNameRequiredException :: Newtype BranchNameRequiredException _


newtype ChangeTypeEnum = ChangeTypeEnum String
derive instance newtypeChangeTypeEnum :: Newtype ChangeTypeEnum _


newtype ClientRequestToken = ClientRequestToken String
derive instance newtypeClientRequestToken :: Newtype ClientRequestToken _


-- | <p>A client request token is required. A client request token is an unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.</p>
newtype ClientRequestTokenRequiredException = ClientRequestTokenRequiredException 
  { 
  }
derive instance newtypeClientRequestTokenRequiredException :: Newtype ClientRequestTokenRequiredException _


newtype CloneUrlHttp = CloneUrlHttp String
derive instance newtypeCloneUrlHttp :: Newtype CloneUrlHttp _


newtype CloneUrlSsh = CloneUrlSsh String
derive instance newtypeCloneUrlSsh :: Newtype CloneUrlSsh _


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
derive instance newtypeComment :: Newtype Comment _


-- | <p>The comment is empty. You must provide some content for a comment. The content cannot be null.</p>
newtype CommentContentRequiredException = CommentContentRequiredException 
  { 
  }
derive instance newtypeCommentContentRequiredException :: Newtype CommentContentRequiredException _


-- | <p>The comment is too large. Comments are limited to 1,000 characters.</p>
newtype CommentContentSizeLimitExceededException = CommentContentSizeLimitExceededException 
  { 
  }
derive instance newtypeCommentContentSizeLimitExceededException :: Newtype CommentContentSizeLimitExceededException _


-- | <p>This comment has already been deleted. You cannot edit or delete a deleted comment.</p>
newtype CommentDeletedException = CommentDeletedException 
  { 
  }
derive instance newtypeCommentDeletedException :: Newtype CommentDeletedException _


-- | <p>No comment exists with the provided ID. Verify that you have provided the correct ID, and then try again.</p>
newtype CommentDoesNotExistException = CommentDoesNotExistException 
  { 
  }
derive instance newtypeCommentDoesNotExistException :: Newtype CommentDoesNotExistException _


newtype CommentId = CommentId String
derive instance newtypeCommentId :: Newtype CommentId _


-- | <p>The comment ID is missing or null. A comment ID is required.</p>
newtype CommentIdRequiredException = CommentIdRequiredException 
  { 
  }
derive instance newtypeCommentIdRequiredException :: Newtype CommentIdRequiredException _


-- | <p>You cannot modify or delete this comment. Only comment authors can modify or delete their comments.</p>
newtype CommentNotCreatedByCallerException = CommentNotCreatedByCallerException 
  { 
  }
derive instance newtypeCommentNotCreatedByCallerException :: Newtype CommentNotCreatedByCallerException _


newtype Comments = Comments (Array Comment)
derive instance newtypeComments :: Newtype Comments _


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
derive instance newtypeCommentsForComparedCommit :: Newtype CommentsForComparedCommit _


newtype CommentsForComparedCommitData = CommentsForComparedCommitData (Array CommentsForComparedCommit)
derive instance newtypeCommentsForComparedCommitData :: Newtype CommentsForComparedCommitData _


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
derive instance newtypeCommentsForPullRequest :: Newtype CommentsForPullRequest _


newtype CommentsForPullRequestData = CommentsForPullRequestData (Array CommentsForPullRequest)
derive instance newtypeCommentsForPullRequestData :: Newtype CommentsForPullRequestData _


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
derive instance newtypeCommit :: Newtype Commit _


-- | <p>The specified commit does not exist or no commit was specified, and the specified repository has no default branch.</p>
newtype CommitDoesNotExistException = CommitDoesNotExistException 
  { 
  }
derive instance newtypeCommitDoesNotExistException :: Newtype CommitDoesNotExistException _


newtype CommitId = CommitId String
derive instance newtypeCommitId :: Newtype CommitId _


-- | <p>The specified commit ID does not exist.</p>
newtype CommitIdDoesNotExistException = CommitIdDoesNotExistException 
  { 
  }
derive instance newtypeCommitIdDoesNotExistException :: Newtype CommitIdDoesNotExistException _


-- | <p>A commit ID was not specified.</p>
newtype CommitIdRequiredException = CommitIdRequiredException 
  { 
  }
derive instance newtypeCommitIdRequiredException :: Newtype CommitIdRequiredException _


-- | <p>The commit message is too long. Provide a shorter string. </p>
newtype CommitMessageLengthExceededException = CommitMessageLengthExceededException 
  { 
  }
derive instance newtypeCommitMessageLengthExceededException :: Newtype CommitMessageLengthExceededException _


newtype CommitName = CommitName String
derive instance newtypeCommitName :: Newtype CommitName _


-- | <p>A commit was not specified.</p>
newtype CommitRequiredException = CommitRequiredException 
  { 
  }
derive instance newtypeCommitRequiredException :: Newtype CommitRequiredException _


newtype Content = Content String
derive instance newtypeContent :: Newtype Content _


-- | <p>Represents the input of a create branch operation.</p>
newtype CreateBranchInput = CreateBranchInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BranchName'" :: (BranchName)
  , "CommitId'" :: (CommitId)
  }
derive instance newtypeCreateBranchInput :: Newtype CreateBranchInput _


newtype CreatePullRequestInput = CreatePullRequestInput 
  { "Title'" :: (Title)
  , "Description'" :: NullOrUndefined (Description)
  , "Targets'" :: (TargetList)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeCreatePullRequestInput :: Newtype CreatePullRequestInput _


newtype CreatePullRequestOutput = CreatePullRequestOutput 
  { "PullRequest'" :: (PullRequest)
  }
derive instance newtypeCreatePullRequestOutput :: Newtype CreatePullRequestOutput _


-- | <p>Represents the input of a create repository operation.</p>
newtype CreateRepositoryInput = CreateRepositoryInput 
  { "RepositoryName'" :: (RepositoryName)
  , "RepositoryDescription'" :: NullOrUndefined (RepositoryDescription)
  }
derive instance newtypeCreateRepositoryInput :: Newtype CreateRepositoryInput _


-- | <p>Represents the output of a create repository operation.</p>
newtype CreateRepositoryOutput = CreateRepositoryOutput 
  { "RepositoryMetadata'" :: NullOrUndefined (RepositoryMetadata)
  }
derive instance newtypeCreateRepositoryOutput :: Newtype CreateRepositoryOutput _


newtype CreationDate = CreationDate Number
derive instance newtypeCreationDate :: Newtype CreationDate _


newtype Date = Date String
derive instance newtypeDate :: Newtype Date _


-- | <p>The specified branch is the default branch for the repository, and cannot be deleted. To delete this branch, you must first set another branch as the default branch.</p>
newtype DefaultBranchCannotBeDeletedException = DefaultBranchCannotBeDeletedException 
  { 
  }
derive instance newtypeDefaultBranchCannotBeDeletedException :: Newtype DefaultBranchCannotBeDeletedException _


-- | <p>Represents the input of a delete branch operation.</p>
newtype DeleteBranchInput = DeleteBranchInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BranchName'" :: (BranchName)
  }
derive instance newtypeDeleteBranchInput :: Newtype DeleteBranchInput _


-- | <p>Represents the output of a delete branch operation.</p>
newtype DeleteBranchOutput = DeleteBranchOutput 
  { "DeletedBranch'" :: NullOrUndefined (BranchInfo)
  }
derive instance newtypeDeleteBranchOutput :: Newtype DeleteBranchOutput _


newtype DeleteCommentContentInput = DeleteCommentContentInput 
  { "CommentId'" :: (CommentId)
  }
derive instance newtypeDeleteCommentContentInput :: Newtype DeleteCommentContentInput _


newtype DeleteCommentContentOutput = DeleteCommentContentOutput 
  { "Comment'" :: NullOrUndefined (Comment)
  }
derive instance newtypeDeleteCommentContentOutput :: Newtype DeleteCommentContentOutput _


-- | <p>Represents the input of a delete repository operation.</p>
newtype DeleteRepositoryInput = DeleteRepositoryInput 
  { "RepositoryName'" :: (RepositoryName)
  }
derive instance newtypeDeleteRepositoryInput :: Newtype DeleteRepositoryInput _


-- | <p>Represents the output of a delete repository operation.</p>
newtype DeleteRepositoryOutput = DeleteRepositoryOutput 
  { "RepositoryId'" :: NullOrUndefined (RepositoryId)
  }
derive instance newtypeDeleteRepositoryOutput :: Newtype DeleteRepositoryOutput _


newtype DescribePullRequestEventsInput = DescribePullRequestEventsInput 
  { "PullRequestId'" :: (PullRequestId)
  , "PullRequestEventType'" :: NullOrUndefined (PullRequestEventType)
  , "ActorArn'" :: NullOrUndefined (Arn)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeDescribePullRequestEventsInput :: Newtype DescribePullRequestEventsInput _


newtype DescribePullRequestEventsOutput = DescribePullRequestEventsOutput 
  { "PullRequestEvents'" :: (PullRequestEventList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribePullRequestEventsOutput :: Newtype DescribePullRequestEventsOutput _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


-- | <p>Returns information about a set of differences for a commit specifier.</p>
newtype Difference = Difference 
  { "BeforeBlob'" :: NullOrUndefined (BlobMetadata)
  , "AfterBlob'" :: NullOrUndefined (BlobMetadata)
  , "ChangeType'" :: NullOrUndefined (ChangeTypeEnum)
  }
derive instance newtypeDifference :: Newtype Difference _


newtype DifferenceList = DifferenceList (Array Difference)
derive instance newtypeDifferenceList :: Newtype DifferenceList _


-- | <p>A file cannot be added to the repository because the specified path name has the same name as a file that already exists in this repository. Either provide a different name for the file, or specify a different path for the file.</p>
newtype DirectoryNameConflictsWithFileNameException = DirectoryNameConflictsWithFileNameException 
  { 
  }
derive instance newtypeDirectoryNameConflictsWithFileNameException :: Newtype DirectoryNameConflictsWithFileNameException _


newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _


-- | <p>An encryption integrity check failed.</p>
newtype EncryptionIntegrityChecksFailedException = EncryptionIntegrityChecksFailedException 
  { 
  }
derive instance newtypeEncryptionIntegrityChecksFailedException :: Newtype EncryptionIntegrityChecksFailedException _


-- | <p>An encryption key could not be accessed.</p>
newtype EncryptionKeyAccessDeniedException = EncryptionKeyAccessDeniedException 
  { 
  }
derive instance newtypeEncryptionKeyAccessDeniedException :: Newtype EncryptionKeyAccessDeniedException _


-- | <p>The encryption key is disabled.</p>
newtype EncryptionKeyDisabledException = EncryptionKeyDisabledException 
  { 
  }
derive instance newtypeEncryptionKeyDisabledException :: Newtype EncryptionKeyDisabledException _


-- | <p>No encryption key was found.</p>
newtype EncryptionKeyNotFoundException = EncryptionKeyNotFoundException 
  { 
  }
derive instance newtypeEncryptionKeyNotFoundException :: Newtype EncryptionKeyNotFoundException _


-- | <p>The encryption key is not available.</p>
newtype EncryptionKeyUnavailableException = EncryptionKeyUnavailableException 
  { 
  }
derive instance newtypeEncryptionKeyUnavailableException :: Newtype EncryptionKeyUnavailableException _


newtype EventDate = EventDate Number
derive instance newtypeEventDate :: Newtype EventDate _


newtype FileContent = FileContent String
derive instance newtypeFileContent :: Newtype FileContent _


-- | <p>The file cannot be added because it is empty. Empty files cannot be added to the repository with this API.</p>
newtype FileContentRequiredException = FileContentRequiredException 
  { 
  }
derive instance newtypeFileContentRequiredException :: Newtype FileContentRequiredException _


-- | <p>The file cannot be added because it is too large. The maximum file size that can be added using PutFile is 6 MB. For files larger than 6 MB but smaller than 2 GB, add them using a Git client.</p>
newtype FileContentSizeLimitExceededException = FileContentSizeLimitExceededException 
  { 
  }
derive instance newtypeFileContentSizeLimitExceededException :: Newtype FileContentSizeLimitExceededException _


newtype FileModeTypeEnum = FileModeTypeEnum String
derive instance newtypeFileModeTypeEnum :: Newtype FileModeTypeEnum _


-- | <p>A file cannot be added to the repository because the specified file name has the same name as a directory in this repository. Either provide another name for the file, or add the file in a directory that does not match the file name.</p>
newtype FileNameConflictsWithDirectoryNameException = FileNameConflictsWithDirectoryNameException 
  { 
  }
derive instance newtypeFileNameConflictsWithDirectoryNameException :: Newtype FileNameConflictsWithDirectoryNameException _


-- | <p>The specified file exceeds the file size limit for AWS CodeCommit. For more information about limits in AWS CodeCommit, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/limits.html">AWS CodeCommit User Guide</a>.</p>
newtype FileTooLargeException = FileTooLargeException 
  { 
  }
derive instance newtypeFileTooLargeException :: Newtype FileTooLargeException _


-- | <p>Represents the input of a get blob operation.</p>
newtype GetBlobInput = GetBlobInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BlobId'" :: (ObjectId)
  }
derive instance newtypeGetBlobInput :: Newtype GetBlobInput _


-- | <p>Represents the output of a get blob operation.</p>
newtype GetBlobOutput = GetBlobOutput 
  { "Content'" :: (String)
  }
derive instance newtypeGetBlobOutput :: Newtype GetBlobOutput _


-- | <p>Represents the input of a get branch operation.</p>
newtype GetBranchInput = GetBranchInput 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "BranchName'" :: NullOrUndefined (BranchName)
  }
derive instance newtypeGetBranchInput :: Newtype GetBranchInput _


-- | <p>Represents the output of a get branch operation.</p>
newtype GetBranchOutput = GetBranchOutput 
  { "Branch'" :: NullOrUndefined (BranchInfo)
  }
derive instance newtypeGetBranchOutput :: Newtype GetBranchOutput _


newtype GetCommentInput = GetCommentInput 
  { "CommentId'" :: (CommentId)
  }
derive instance newtypeGetCommentInput :: Newtype GetCommentInput _


newtype GetCommentOutput = GetCommentOutput 
  { "Comment'" :: NullOrUndefined (Comment)
  }
derive instance newtypeGetCommentOutput :: Newtype GetCommentOutput _


newtype GetCommentsForComparedCommitInput = GetCommentsForComparedCommitInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: (CommitId)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeGetCommentsForComparedCommitInput :: Newtype GetCommentsForComparedCommitInput _


newtype GetCommentsForComparedCommitOutput = GetCommentsForComparedCommitOutput 
  { "CommentsForComparedCommitData'" :: NullOrUndefined (CommentsForComparedCommitData)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetCommentsForComparedCommitOutput :: Newtype GetCommentsForComparedCommitOutput _


newtype GetCommentsForPullRequestInput = GetCommentsForPullRequestInput 
  { "PullRequestId'" :: (PullRequestId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined (CommitId)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeGetCommentsForPullRequestInput :: Newtype GetCommentsForPullRequestInput _


newtype GetCommentsForPullRequestOutput = GetCommentsForPullRequestOutput 
  { "CommentsForPullRequestData'" :: NullOrUndefined (CommentsForPullRequestData)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetCommentsForPullRequestOutput :: Newtype GetCommentsForPullRequestOutput _


-- | <p>Represents the input of a get commit operation.</p>
newtype GetCommitInput = GetCommitInput 
  { "RepositoryName'" :: (RepositoryName)
  , "CommitId'" :: (ObjectId)
  }
derive instance newtypeGetCommitInput :: Newtype GetCommitInput _


-- | <p>Represents the output of a get commit operation.</p>
newtype GetCommitOutput = GetCommitOutput 
  { "Commit'" :: (Commit)
  }
derive instance newtypeGetCommitOutput :: Newtype GetCommitOutput _


newtype GetDifferencesInput = GetDifferencesInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BeforeCommitSpecifier'" :: NullOrUndefined (CommitName)
  , "AfterCommitSpecifier'" :: (CommitName)
  , "BeforePath'" :: NullOrUndefined (Path)
  , "AfterPath'" :: NullOrUndefined (Path)
  , "MaxResults" :: NullOrUndefined (Limit)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetDifferencesInput :: Newtype GetDifferencesInput _


newtype GetDifferencesOutput = GetDifferencesOutput 
  { "Differences'" :: NullOrUndefined (DifferenceList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetDifferencesOutput :: Newtype GetDifferencesOutput _


newtype GetMergeConflictsInput = GetMergeConflictsInput 
  { "RepositoryName'" :: (RepositoryName)
  , "DestinationCommitSpecifier'" :: (CommitName)
  , "SourceCommitSpecifier'" :: (CommitName)
  , "MergeOption'" :: (MergeOptionTypeEnum)
  }
derive instance newtypeGetMergeConflictsInput :: Newtype GetMergeConflictsInput _


newtype GetMergeConflictsOutput = GetMergeConflictsOutput 
  { "Mergeable'" :: (IsMergeable)
  , "DestinationCommitId'" :: (CommitId)
  , "SourceCommitId'" :: (CommitId)
  }
derive instance newtypeGetMergeConflictsOutput :: Newtype GetMergeConflictsOutput _


newtype GetPullRequestInput = GetPullRequestInput 
  { "PullRequestId'" :: (PullRequestId)
  }
derive instance newtypeGetPullRequestInput :: Newtype GetPullRequestInput _


newtype GetPullRequestOutput = GetPullRequestOutput 
  { "PullRequest'" :: (PullRequest)
  }
derive instance newtypeGetPullRequestOutput :: Newtype GetPullRequestOutput _


-- | <p>Represents the input of a get repository operation.</p>
newtype GetRepositoryInput = GetRepositoryInput 
  { "RepositoryName'" :: (RepositoryName)
  }
derive instance newtypeGetRepositoryInput :: Newtype GetRepositoryInput _


-- | <p>Represents the output of a get repository operation.</p>
newtype GetRepositoryOutput = GetRepositoryOutput 
  { "RepositoryMetadata'" :: NullOrUndefined (RepositoryMetadata)
  }
derive instance newtypeGetRepositoryOutput :: Newtype GetRepositoryOutput _


-- | <p>Represents the input of a get repository triggers operation.</p>
newtype GetRepositoryTriggersInput = GetRepositoryTriggersInput 
  { "RepositoryName'" :: (RepositoryName)
  }
derive instance newtypeGetRepositoryTriggersInput :: Newtype GetRepositoryTriggersInput _


-- | <p>Represents the output of a get repository triggers operation.</p>
newtype GetRepositoryTriggersOutput = GetRepositoryTriggersOutput 
  { "ConfigurationId'" :: NullOrUndefined (RepositoryTriggersConfigurationId)
  , "Triggers'" :: NullOrUndefined (RepositoryTriggersList)
  }
derive instance newtypeGetRepositoryTriggersOutput :: Newtype GetRepositoryTriggersOutput _


-- | <p>The client request token is not valid. Either the token is not in a valid format, or the token has been used in a previous request and cannot be re-used.</p>
newtype IdempotencyParameterMismatchException = IdempotencyParameterMismatchException 
  { 
  }
derive instance newtypeIdempotencyParameterMismatchException :: Newtype IdempotencyParameterMismatchException _


-- | <p>The Amazon Resource Name (ARN) is not valid. Make sure that you have provided the full ARN for the user who initiated the change for the pull request, and then try again.</p>
newtype InvalidActorArnException = InvalidActorArnException 
  { 
  }
derive instance newtypeInvalidActorArnException :: Newtype InvalidActorArnException _


-- | <p>The Amazon Resource Name (ARN) is not valid. Make sure that you have provided the full ARN for the author of the pull request, and then try again.</p>
newtype InvalidAuthorArnException = InvalidAuthorArnException 
  { 
  }
derive instance newtypeInvalidAuthorArnException :: Newtype InvalidAuthorArnException _


-- | <p>The specified blob is not valid.</p>
newtype InvalidBlobIdException = InvalidBlobIdException 
  { 
  }
derive instance newtypeInvalidBlobIdException :: Newtype InvalidBlobIdException _


-- | <p>The specified reference name is not valid.</p>
newtype InvalidBranchNameException = InvalidBranchNameException 
  { 
  }
derive instance newtypeInvalidBranchNameException :: Newtype InvalidBranchNameException _


-- | <p>The client request token is not valid.</p>
newtype InvalidClientRequestTokenException = InvalidClientRequestTokenException 
  { 
  }
derive instance newtypeInvalidClientRequestTokenException :: Newtype InvalidClientRequestTokenException _


-- | <p>The comment ID is not in a valid format. Make sure that you have provided the full comment ID.</p>
newtype InvalidCommentIdException = InvalidCommentIdException 
  { 
  }
derive instance newtypeInvalidCommentIdException :: Newtype InvalidCommentIdException _


-- | <p>The specified commit is not valid.</p>
newtype InvalidCommitException = InvalidCommitException 
  { 
  }
derive instance newtypeInvalidCommitException :: Newtype InvalidCommitException _


-- | <p>The specified commit ID is not valid.</p>
newtype InvalidCommitIdException = InvalidCommitIdException 
  { 
  }
derive instance newtypeInvalidCommitIdException :: Newtype InvalidCommitIdException _


-- | <p>The specified continuation token is not valid.</p>
newtype InvalidContinuationTokenException = InvalidContinuationTokenException 
  { 
  }
derive instance newtypeInvalidContinuationTokenException :: Newtype InvalidContinuationTokenException _


-- | <p>The pull request description is not valid. Descriptions are limited to 1,000 characters in length.</p>
newtype InvalidDescriptionException = InvalidDescriptionException 
  { 
  }
derive instance newtypeInvalidDescriptionException :: Newtype InvalidDescriptionException _


-- | <p>The destination commit specifier is not valid. You must provide a valid branch name, tag, or full commit ID. </p>
newtype InvalidDestinationCommitSpecifierException = InvalidDestinationCommitSpecifierException 
  { 
  }
derive instance newtypeInvalidDestinationCommitSpecifierException :: Newtype InvalidDestinationCommitSpecifierException _


-- | <p>The specified email address either contains one or more characters that are not allowed, or it exceeds the maximum number of characters allowed for an email address.</p>
newtype InvalidEmailException = InvalidEmailException 
  { 
  }
derive instance newtypeInvalidEmailException :: Newtype InvalidEmailException _


-- | <p>The location of the file is not valid. Make sure that you include the extension of the file as well as the file name.</p>
newtype InvalidFileLocationException = InvalidFileLocationException 
  { 
  }
derive instance newtypeInvalidFileLocationException :: Newtype InvalidFileLocationException _


-- | <p>The specified file mode permission is not valid. For a list of valid file mode permissions, see <a>PutFile</a>. </p>
newtype InvalidFileModeException = InvalidFileModeException 
  { 
  }
derive instance newtypeInvalidFileModeException :: Newtype InvalidFileModeException _


-- | <p>The position is not valid. Make sure that the line number exists in the version of the file you want to comment on.</p>
newtype InvalidFilePositionException = InvalidFilePositionException 
  { 
  }
derive instance newtypeInvalidFilePositionException :: Newtype InvalidFilePositionException _


-- | <p>The specified number of maximum results is not valid.</p>
newtype InvalidMaxResultsException = InvalidMaxResultsException 
  { 
  }
derive instance newtypeInvalidMaxResultsException :: Newtype InvalidMaxResultsException _


-- | <p>The specified merge option is not valid. The only valid value is FAST_FORWARD_MERGE.</p>
newtype InvalidMergeOptionException = InvalidMergeOptionException 
  { 
  }
derive instance newtypeInvalidMergeOptionException :: Newtype InvalidMergeOptionException _


-- | <p>The specified sort order is not valid.</p>
newtype InvalidOrderException = InvalidOrderException 
  { 
  }
derive instance newtypeInvalidOrderException :: Newtype InvalidOrderException _


-- | <p>The parent commit ID is not valid. The commit ID cannot be empty, and must match the head commit ID for the branch of the repository where you want to add or update a file.</p>
newtype InvalidParentCommitIdException = InvalidParentCommitIdException 
  { 
  }
derive instance newtypeInvalidParentCommitIdException :: Newtype InvalidParentCommitIdException _


-- | <p>The specified path is not valid.</p>
newtype InvalidPathException = InvalidPathException 
  { 
  }
derive instance newtypeInvalidPathException :: Newtype InvalidPathException _


-- | <p>The pull request event type is not valid. </p>
newtype InvalidPullRequestEventTypeException = InvalidPullRequestEventTypeException 
  { 
  }
derive instance newtypeInvalidPullRequestEventTypeException :: Newtype InvalidPullRequestEventTypeException _


-- | <p>The pull request ID is not valid. Make sure that you have provided the full ID and that the pull request is in the specified repository, and then try again.</p>
newtype InvalidPullRequestIdException = InvalidPullRequestIdException 
  { 
  }
derive instance newtypeInvalidPullRequestIdException :: Newtype InvalidPullRequestIdException _


-- | <p>The pull request status is not valid. The only valid values are <code>OPEN</code> and <code>CLOSED</code>.</p>
newtype InvalidPullRequestStatusException = InvalidPullRequestStatusException 
  { 
  }
derive instance newtypeInvalidPullRequestStatusException :: Newtype InvalidPullRequestStatusException _


-- | <p>The pull request status update is not valid. The only valid update is from <code>OPEN</code> to <code>CLOSED</code>.</p>
newtype InvalidPullRequestStatusUpdateException = InvalidPullRequestStatusUpdateException 
  { 
  }
derive instance newtypeInvalidPullRequestStatusUpdateException :: Newtype InvalidPullRequestStatusUpdateException _


-- | <p>The specified reference name format is not valid. Reference names must conform to the Git references format, for example refs/heads/master. For more information, see <a href="https://git-scm.com/book/en/v2/Git-Internals-Git-References">Git Internals - Git References</a> or consult your Git documentation.</p>
newtype InvalidReferenceNameException = InvalidReferenceNameException 
  { 
  }
derive instance newtypeInvalidReferenceNameException :: Newtype InvalidReferenceNameException _


-- | <p>Either the enum is not in a valid format, or the specified file version enum is not valid in respect to the current file version.</p>
newtype InvalidRelativeFileVersionEnumException = InvalidRelativeFileVersionEnumException 
  { 
  }
derive instance newtypeInvalidRelativeFileVersionEnumException :: Newtype InvalidRelativeFileVersionEnumException _


-- | <p>The specified repository description is not valid.</p>
newtype InvalidRepositoryDescriptionException = InvalidRepositoryDescriptionException 
  { 
  }
derive instance newtypeInvalidRepositoryDescriptionException :: Newtype InvalidRepositoryDescriptionException _


-- | <p>At least one specified repository name is not valid.</p> <note> <p>This exception only occurs when a specified repository name is not valid. Other exceptions occur when a required repository parameter is missing, or when a specified repository does not exist.</p> </note>
newtype InvalidRepositoryNameException = InvalidRepositoryNameException 
  { 
  }
derive instance newtypeInvalidRepositoryNameException :: Newtype InvalidRepositoryNameException _


-- | <p>One or more branch names specified for the trigger is not valid.</p>
newtype InvalidRepositoryTriggerBranchNameException = InvalidRepositoryTriggerBranchNameException 
  { 
  }
derive instance newtypeInvalidRepositoryTriggerBranchNameException :: Newtype InvalidRepositoryTriggerBranchNameException _


-- | <p>The custom data provided for the trigger is not valid.</p>
newtype InvalidRepositoryTriggerCustomDataException = InvalidRepositoryTriggerCustomDataException 
  { 
  }
derive instance newtypeInvalidRepositoryTriggerCustomDataException :: Newtype InvalidRepositoryTriggerCustomDataException _


-- | <p>The Amazon Resource Name (ARN) for the trigger is not valid for the specified destination. The most common reason for this error is that the ARN does not meet the requirements for the service type.</p>
newtype InvalidRepositoryTriggerDestinationArnException = InvalidRepositoryTriggerDestinationArnException 
  { 
  }
derive instance newtypeInvalidRepositoryTriggerDestinationArnException :: Newtype InvalidRepositoryTriggerDestinationArnException _


-- | <p>One or more events specified for the trigger is not valid. Check to make sure that all events specified match the requirements for allowed events.</p>
newtype InvalidRepositoryTriggerEventsException = InvalidRepositoryTriggerEventsException 
  { 
  }
derive instance newtypeInvalidRepositoryTriggerEventsException :: Newtype InvalidRepositoryTriggerEventsException _


-- | <p>The name of the trigger is not valid.</p>
newtype InvalidRepositoryTriggerNameException = InvalidRepositoryTriggerNameException 
  { 
  }
derive instance newtypeInvalidRepositoryTriggerNameException :: Newtype InvalidRepositoryTriggerNameException _


-- | <p>The region for the trigger target does not match the region for the repository. Triggers must be created in the same region as the target for the trigger.</p>
newtype InvalidRepositoryTriggerRegionException = InvalidRepositoryTriggerRegionException 
  { 
  }
derive instance newtypeInvalidRepositoryTriggerRegionException :: Newtype InvalidRepositoryTriggerRegionException _


-- | <p>The specified sort by value is not valid.</p>
newtype InvalidSortByException = InvalidSortByException 
  { 
  }
derive instance newtypeInvalidSortByException :: Newtype InvalidSortByException _


-- | <p>The source commit specifier is not valid. You must provide a valid branch name, tag, or full commit ID.</p>
newtype InvalidSourceCommitSpecifierException = InvalidSourceCommitSpecifierException 
  { 
  }
derive instance newtypeInvalidSourceCommitSpecifierException :: Newtype InvalidSourceCommitSpecifierException _


-- | <p>The target for the pull request is not valid. A target must contain the full values for the repository name, source branch, and destination branch for the pull request.</p>
newtype InvalidTargetException = InvalidTargetException 
  { 
  }
derive instance newtypeInvalidTargetException :: Newtype InvalidTargetException _


-- | <p>The targets for the pull request is not valid or not in a valid format. Targets are a list of target objects. Each target object must contain the full values for the repository name, source branch, and destination branch for a pull request.</p>
newtype InvalidTargetsException = InvalidTargetsException 
  { 
  }
derive instance newtypeInvalidTargetsException :: Newtype InvalidTargetsException _


-- | <p>The title of the pull request is not valid. Pull request titles cannot exceed 100 characters in length.</p>
newtype InvalidTitleException = InvalidTitleException 
  { 
  }
derive instance newtypeInvalidTitleException :: Newtype InvalidTitleException _


newtype IsCommentDeleted = IsCommentDeleted Boolean
derive instance newtypeIsCommentDeleted :: Newtype IsCommentDeleted _


newtype IsMergeable = IsMergeable Boolean
derive instance newtypeIsMergeable :: Newtype IsMergeable _


newtype IsMerged = IsMerged Boolean
derive instance newtypeIsMerged :: Newtype IsMerged _


newtype LastModifiedDate = LastModifiedDate Number
derive instance newtypeLastModifiedDate :: Newtype LastModifiedDate _


newtype Limit = Limit Int
derive instance newtypeLimit :: Newtype Limit _


-- | <p>Represents the input of a list branches operation.</p>
newtype ListBranchesInput = ListBranchesInput 
  { "RepositoryName'" :: (RepositoryName)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListBranchesInput :: Newtype ListBranchesInput _


-- | <p>Represents the output of a list branches operation.</p>
newtype ListBranchesOutput = ListBranchesOutput 
  { "Branches'" :: NullOrUndefined (BranchNameList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListBranchesOutput :: Newtype ListBranchesOutput _


newtype ListPullRequestsInput = ListPullRequestsInput 
  { "RepositoryName'" :: (RepositoryName)
  , "AuthorArn'" :: NullOrUndefined (Arn)
  , "PullRequestStatus'" :: NullOrUndefined (PullRequestStatusEnum)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListPullRequestsInput :: Newtype ListPullRequestsInput _


newtype ListPullRequestsOutput = ListPullRequestsOutput 
  { "PullRequestIds'" :: (PullRequestIdList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListPullRequestsOutput :: Newtype ListPullRequestsOutput _


-- | <p>Represents the input of a list repositories operation.</p>
newtype ListRepositoriesInput = ListRepositoriesInput 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "SortBy'" :: NullOrUndefined (SortByEnum)
  , "Order'" :: NullOrUndefined (OrderEnum)
  }
derive instance newtypeListRepositoriesInput :: Newtype ListRepositoriesInput _


-- | <p>Represents the output of a list repositories operation.</p>
newtype ListRepositoriesOutput = ListRepositoriesOutput 
  { "Repositories'" :: NullOrUndefined (RepositoryNameIdPairList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListRepositoriesOutput :: Newtype ListRepositoriesOutput _


-- | <p>Returns information about the location of a change or comment in the comparison between two commits or a pull request.</p>
newtype Location = Location 
  { "FilePath'" :: NullOrUndefined (Path)
  , "FilePosition'" :: NullOrUndefined (Position)
  , "RelativeFileVersion'" :: NullOrUndefined (RelativeFileVersionEnum)
  }
derive instance newtypeLocation :: Newtype Location _


-- | <p>The pull request cannot be merged automatically into the destination branch. You must manually merge the branches and resolve any conflicts.</p>
newtype ManualMergeRequiredException = ManualMergeRequiredException 
  { 
  }
derive instance newtypeManualMergeRequiredException :: Newtype ManualMergeRequiredException _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | <p>The number of branches for the trigger was exceeded.</p>
newtype MaximumBranchesExceededException = MaximumBranchesExceededException 
  { 
  }
derive instance newtypeMaximumBranchesExceededException :: Newtype MaximumBranchesExceededException _


-- | <p>You cannot create the pull request because the repository has too many open pull requests. The maximum number of open pull requests for a repository is 1,000. Close one or more open pull requests, and then try again.</p>
newtype MaximumOpenPullRequestsExceededException = MaximumOpenPullRequestsExceededException 
  { 
  }
derive instance newtypeMaximumOpenPullRequestsExceededException :: Newtype MaximumOpenPullRequestsExceededException _


-- | <p>The maximum number of allowed repository names was exceeded. Currently, this number is 25.</p>
newtype MaximumRepositoryNamesExceededException = MaximumRepositoryNamesExceededException 
  { 
  }
derive instance newtypeMaximumRepositoryNamesExceededException :: Newtype MaximumRepositoryNamesExceededException _


-- | <p>The number of triggers allowed for the repository was exceeded.</p>
newtype MaximumRepositoryTriggersExceededException = MaximumRepositoryTriggersExceededException 
  { 
  }
derive instance newtypeMaximumRepositoryTriggersExceededException :: Newtype MaximumRepositoryTriggersExceededException _


-- | <p>Returns information about a merge or potential merge between a source reference and a destination reference in a pull request.</p>
newtype MergeMetadata = MergeMetadata 
  { "IsMerged'" :: NullOrUndefined (IsMerged)
  , "MergedBy'" :: NullOrUndefined (Arn)
  }
derive instance newtypeMergeMetadata :: Newtype MergeMetadata _


-- | <p>A merge option or stategy is required, and none was provided.</p>
newtype MergeOptionRequiredException = MergeOptionRequiredException 
  { 
  }
derive instance newtypeMergeOptionRequiredException :: Newtype MergeOptionRequiredException _


newtype MergeOptionTypeEnum = MergeOptionTypeEnum String
derive instance newtypeMergeOptionTypeEnum :: Newtype MergeOptionTypeEnum _


newtype MergePullRequestByFastForwardInput = MergePullRequestByFastForwardInput 
  { "PullRequestId'" :: (PullRequestId)
  , "RepositoryName'" :: (RepositoryName)
  , "SourceCommitId'" :: NullOrUndefined (CommitId)
  }
derive instance newtypeMergePullRequestByFastForwardInput :: Newtype MergePullRequestByFastForwardInput _


newtype MergePullRequestByFastForwardOutput = MergePullRequestByFastForwardOutput 
  { "PullRequest'" :: NullOrUndefined (PullRequest)
  }
derive instance newtypeMergePullRequestByFastForwardOutput :: Newtype MergePullRequestByFastForwardOutput _


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


newtype Mode = Mode String
derive instance newtypeMode :: Newtype Mode _


-- | <p>You cannot include more than one repository in a pull request. Make sure you have specified only one repository name in your request, and then try again.</p>
newtype MultipleRepositoriesInPullRequestException = MultipleRepositoriesInPullRequestException 
  { 
  }
derive instance newtypeMultipleRepositoriesInPullRequestException :: Newtype MultipleRepositoriesInPullRequestException _


newtype Name = Name String
derive instance newtypeName :: Newtype Name _


-- | <p>The file name is not valid because it has exceeded the character limit for file names. File names, including the path to the file, cannot exceed the character limit. </p>
newtype NameLengthExceededException = NameLengthExceededException 
  { 
  }
derive instance newtypeNameLengthExceededException :: Newtype NameLengthExceededException _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


newtype ObjectId = ObjectId String
derive instance newtypeObjectId :: Newtype ObjectId _


newtype OrderEnum = OrderEnum String
derive instance newtypeOrderEnum :: Newtype OrderEnum _


-- | <p>The parent commit ID is not valid. The specified parent commit ID does not exist in the specified branch of the repository.</p>
newtype ParentCommitDoesNotExistException = ParentCommitDoesNotExistException 
  { 
  }
derive instance newtypeParentCommitDoesNotExistException :: Newtype ParentCommitDoesNotExistException _


-- | <p>The file could not be added because the provided parent commit ID is not the current tip of the specified branch. To view the full commit ID of the current head of the branch, use <a>GetBranch</a>.</p>
newtype ParentCommitIdOutdatedException = ParentCommitIdOutdatedException 
  { 
  }
derive instance newtypeParentCommitIdOutdatedException :: Newtype ParentCommitIdOutdatedException _


-- | <p>A parent commit ID is required. To view the full commit ID of a branch in a repository, use <a>GetBranch</a> or a Git command (for example, git pull or git log).</p>
newtype ParentCommitIdRequiredException = ParentCommitIdRequiredException 
  { 
  }
derive instance newtypeParentCommitIdRequiredException :: Newtype ParentCommitIdRequiredException _


newtype ParentList = ParentList (Array ObjectId)
derive instance newtypeParentList :: Newtype ParentList _


newtype Path = Path String
derive instance newtypePath :: Newtype Path _


-- | <p>The specified path does not exist.</p>
newtype PathDoesNotExistException = PathDoesNotExistException 
  { 
  }
derive instance newtypePathDoesNotExistException :: Newtype PathDoesNotExistException _


-- | <p>The filePath for a location cannot be empty or null.</p>
newtype PathRequiredException = PathRequiredException 
  { 
  }
derive instance newtypePathRequiredException :: Newtype PathRequiredException _


newtype Position = Position Number
derive instance newtypePosition :: Newtype Position _


newtype PostCommentForComparedCommitInput = PostCommentForComparedCommitInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: (CommitId)
  , "Location'" :: NullOrUndefined (Location)
  , "Content'" :: (Content)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypePostCommentForComparedCommitInput :: Newtype PostCommentForComparedCommitInput _


newtype PostCommentForComparedCommitOutput = PostCommentForComparedCommitOutput 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined (CommitId)
  , "BeforeBlobId'" :: NullOrUndefined (ObjectId)
  , "AfterBlobId'" :: NullOrUndefined (ObjectId)
  , "Location'" :: NullOrUndefined (Location)
  , "Comment'" :: NullOrUndefined (Comment)
  }
derive instance newtypePostCommentForComparedCommitOutput :: Newtype PostCommentForComparedCommitOutput _


newtype PostCommentForPullRequestInput = PostCommentForPullRequestInput 
  { "PullRequestId'" :: (PullRequestId)
  , "RepositoryName'" :: (RepositoryName)
  , "BeforeCommitId'" :: (CommitId)
  , "AfterCommitId'" :: (CommitId)
  , "Location'" :: NullOrUndefined (Location)
  , "Content'" :: (Content)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypePostCommentForPullRequestInput :: Newtype PostCommentForPullRequestInput _


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
derive instance newtypePostCommentForPullRequestOutput :: Newtype PostCommentForPullRequestOutput _


newtype PostCommentReplyInput = PostCommentReplyInput 
  { "InReplyTo'" :: (CommentId)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  , "Content'" :: (Content)
  }
derive instance newtypePostCommentReplyInput :: Newtype PostCommentReplyInput _


newtype PostCommentReplyOutput = PostCommentReplyOutput 
  { "Comment'" :: NullOrUndefined (Comment)
  }
derive instance newtypePostCommentReplyOutput :: Newtype PostCommentReplyOutput _


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
derive instance newtypePullRequest :: Newtype PullRequest _


-- | <p>The pull request status cannot be updated because it is already closed.</p>
newtype PullRequestAlreadyClosedException = PullRequestAlreadyClosedException 
  { 
  }
derive instance newtypePullRequestAlreadyClosedException :: Newtype PullRequestAlreadyClosedException _


-- | <p>The pull request ID could not be found. Make sure that you have specified the correct repository name and pull request ID, and then try again.</p>
newtype PullRequestDoesNotExistException = PullRequestDoesNotExistException 
  { 
  }
derive instance newtypePullRequestDoesNotExistException :: Newtype PullRequestDoesNotExistException _


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
derive instance newtypePullRequestEvent :: Newtype PullRequestEvent _


newtype PullRequestEventList = PullRequestEventList (Array PullRequestEvent)
derive instance newtypePullRequestEventList :: Newtype PullRequestEventList _


newtype PullRequestEventType = PullRequestEventType String
derive instance newtypePullRequestEventType :: Newtype PullRequestEventType _


newtype PullRequestId = PullRequestId String
derive instance newtypePullRequestId :: Newtype PullRequestId _


newtype PullRequestIdList = PullRequestIdList (Array PullRequestId)
derive instance newtypePullRequestIdList :: Newtype PullRequestIdList _


-- | <p>A pull request ID is required, but none was provided.</p>
newtype PullRequestIdRequiredException = PullRequestIdRequiredException 
  { 
  }
derive instance newtypePullRequestIdRequiredException :: Newtype PullRequestIdRequiredException _


-- | <p>Returns information about the change in the merge state for a pull request event. </p>
newtype PullRequestMergedStateChangedEventMetadata = PullRequestMergedStateChangedEventMetadata 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "DestinationReference'" :: NullOrUndefined (ReferenceName)
  , "MergeMetadata'" :: NullOrUndefined (MergeMetadata)
  }
derive instance newtypePullRequestMergedStateChangedEventMetadata :: Newtype PullRequestMergedStateChangedEventMetadata _


-- | <p>Information about an update to the source branch of a pull request.</p>
newtype PullRequestSourceReferenceUpdatedEventMetadata = PullRequestSourceReferenceUpdatedEventMetadata 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined (CommitId)
  }
derive instance newtypePullRequestSourceReferenceUpdatedEventMetadata :: Newtype PullRequestSourceReferenceUpdatedEventMetadata _


-- | <p>Information about a change to the status of a pull request.</p>
newtype PullRequestStatusChangedEventMetadata = PullRequestStatusChangedEventMetadata 
  { "PullRequestStatus'" :: NullOrUndefined (PullRequestStatusEnum)
  }
derive instance newtypePullRequestStatusChangedEventMetadata :: Newtype PullRequestStatusChangedEventMetadata _


newtype PullRequestStatusEnum = PullRequestStatusEnum String
derive instance newtypePullRequestStatusEnum :: Newtype PullRequestStatusEnum _


-- | <p>A pull request status is required, but none was provided.</p>
newtype PullRequestStatusRequiredException = PullRequestStatusRequiredException 
  { 
  }
derive instance newtypePullRequestStatusRequiredException :: Newtype PullRequestStatusRequiredException _


-- | <p>Returns information about a pull request target.</p>
newtype PullRequestTarget = PullRequestTarget 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "SourceReference'" :: NullOrUndefined (ReferenceName)
  , "DestinationReference'" :: NullOrUndefined (ReferenceName)
  , "DestinationCommit'" :: NullOrUndefined (CommitId)
  , "SourceCommit'" :: NullOrUndefined (CommitId)
  , "MergeMetadata'" :: NullOrUndefined (MergeMetadata)
  }
derive instance newtypePullRequestTarget :: Newtype PullRequestTarget _


newtype PullRequestTargetList = PullRequestTargetList (Array PullRequestTarget)
derive instance newtypePullRequestTargetList :: Newtype PullRequestTargetList _


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
derive instance newtypePutFileInput :: Newtype PutFileInput _


newtype PutFileOutput = PutFileOutput 
  { "CommitId'" :: (ObjectId)
  , "BlobId'" :: (ObjectId)
  , "TreeId'" :: (ObjectId)
  }
derive instance newtypePutFileOutput :: Newtype PutFileOutput _


-- | <p>Represents the input ofa put repository triggers operation.</p>
newtype PutRepositoryTriggersInput = PutRepositoryTriggersInput 
  { "RepositoryName'" :: (RepositoryName)
  , "Triggers'" :: (RepositoryTriggersList)
  }
derive instance newtypePutRepositoryTriggersInput :: Newtype PutRepositoryTriggersInput _


-- | <p>Represents the output of a put repository triggers operation.</p>
newtype PutRepositoryTriggersOutput = PutRepositoryTriggersOutput 
  { "ConfigurationId'" :: NullOrUndefined (RepositoryTriggersConfigurationId)
  }
derive instance newtypePutRepositoryTriggersOutput :: Newtype PutRepositoryTriggersOutput _


-- | <p>The specified reference does not exist. You must provide a full commit ID.</p>
newtype ReferenceDoesNotExistException = ReferenceDoesNotExistException 
  { 
  }
derive instance newtypeReferenceDoesNotExistException :: Newtype ReferenceDoesNotExistException _


newtype ReferenceName = ReferenceName String
derive instance newtypeReferenceName :: Newtype ReferenceName _


-- | <p>A reference name is required, but none was provided.</p>
newtype ReferenceNameRequiredException = ReferenceNameRequiredException 
  { 
  }
derive instance newtypeReferenceNameRequiredException :: Newtype ReferenceNameRequiredException _


-- | <p>The specified reference is not a supported type. </p>
newtype ReferenceTypeNotSupportedException = ReferenceTypeNotSupportedException 
  { 
  }
derive instance newtypeReferenceTypeNotSupportedException :: Newtype ReferenceTypeNotSupportedException _


newtype RelativeFileVersionEnum = RelativeFileVersionEnum String
derive instance newtypeRelativeFileVersionEnum :: Newtype RelativeFileVersionEnum _


newtype RepositoryDescription = RepositoryDescription String
derive instance newtypeRepositoryDescription :: Newtype RepositoryDescription _


-- | <p>The specified repository does not exist.</p>
newtype RepositoryDoesNotExistException = RepositoryDoesNotExistException 
  { 
  }
derive instance newtypeRepositoryDoesNotExistException :: Newtype RepositoryDoesNotExistException _


newtype RepositoryId = RepositoryId String
derive instance newtypeRepositoryId :: Newtype RepositoryId _


-- | <p>A repository resource limit was exceeded.</p>
newtype RepositoryLimitExceededException = RepositoryLimitExceededException 
  { 
  }
derive instance newtypeRepositoryLimitExceededException :: Newtype RepositoryLimitExceededException _


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
derive instance newtypeRepositoryMetadata :: Newtype RepositoryMetadata _


newtype RepositoryMetadataList = RepositoryMetadataList (Array RepositoryMetadata)
derive instance newtypeRepositoryMetadataList :: Newtype RepositoryMetadataList _


newtype RepositoryName = RepositoryName String
derive instance newtypeRepositoryName :: Newtype RepositoryName _


-- | <p>The specified repository name already exists.</p>
newtype RepositoryNameExistsException = RepositoryNameExistsException 
  { 
  }
derive instance newtypeRepositoryNameExistsException :: Newtype RepositoryNameExistsException _


-- | <p>Information about a repository name and ID.</p>
newtype RepositoryNameIdPair = RepositoryNameIdPair 
  { "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "RepositoryId'" :: NullOrUndefined (RepositoryId)
  }
derive instance newtypeRepositoryNameIdPair :: Newtype RepositoryNameIdPair _


newtype RepositoryNameIdPairList = RepositoryNameIdPairList (Array RepositoryNameIdPair)
derive instance newtypeRepositoryNameIdPairList :: Newtype RepositoryNameIdPairList _


newtype RepositoryNameList = RepositoryNameList (Array RepositoryName)
derive instance newtypeRepositoryNameList :: Newtype RepositoryNameList _


-- | <p>A repository name is required but was not specified.</p>
newtype RepositoryNameRequiredException = RepositoryNameRequiredException 
  { 
  }
derive instance newtypeRepositoryNameRequiredException :: Newtype RepositoryNameRequiredException _


-- | <p>A repository names object is required but was not specified.</p>
newtype RepositoryNamesRequiredException = RepositoryNamesRequiredException 
  { 
  }
derive instance newtypeRepositoryNamesRequiredException :: Newtype RepositoryNamesRequiredException _


-- | <p>The repository does not contain any pull requests with that pull request ID. Check to make sure you have provided the correct repository name for the pull request.</p>
newtype RepositoryNotAssociatedWithPullRequestException = RepositoryNotAssociatedWithPullRequestException 
  { 
  }
derive instance newtypeRepositoryNotAssociatedWithPullRequestException :: Newtype RepositoryNotAssociatedWithPullRequestException _


newtype RepositoryNotFoundList = RepositoryNotFoundList (Array RepositoryName)
derive instance newtypeRepositoryNotFoundList :: Newtype RepositoryNotFoundList _


-- | <p>Information about a trigger for a repository.</p>
newtype RepositoryTrigger = RepositoryTrigger 
  { "Name'" :: (RepositoryTriggerName)
  , "DestinationArn'" :: (Arn)
  , "CustomData'" :: NullOrUndefined (RepositoryTriggerCustomData)
  , "Branches'" :: NullOrUndefined (BranchNameList)
  , "Events'" :: (RepositoryTriggerEventList)
  }
derive instance newtypeRepositoryTrigger :: Newtype RepositoryTrigger _


-- | <p>At least one branch name is required but was not specified in the trigger configuration.</p>
newtype RepositoryTriggerBranchNameListRequiredException = RepositoryTriggerBranchNameListRequiredException 
  { 
  }
derive instance newtypeRepositoryTriggerBranchNameListRequiredException :: Newtype RepositoryTriggerBranchNameListRequiredException _


newtype RepositoryTriggerCustomData = RepositoryTriggerCustomData String
derive instance newtypeRepositoryTriggerCustomData :: Newtype RepositoryTriggerCustomData _


-- | <p>A destination ARN for the target service for the trigger is required but was not specified.</p>
newtype RepositoryTriggerDestinationArnRequiredException = RepositoryTriggerDestinationArnRequiredException 
  { 
  }
derive instance newtypeRepositoryTriggerDestinationArnRequiredException :: Newtype RepositoryTriggerDestinationArnRequiredException _


newtype RepositoryTriggerEventEnum = RepositoryTriggerEventEnum String
derive instance newtypeRepositoryTriggerEventEnum :: Newtype RepositoryTriggerEventEnum _


newtype RepositoryTriggerEventList = RepositoryTriggerEventList (Array RepositoryTriggerEventEnum)
derive instance newtypeRepositoryTriggerEventList :: Newtype RepositoryTriggerEventList _


-- | <p>At least one event for the trigger is required but was not specified.</p>
newtype RepositoryTriggerEventsListRequiredException = RepositoryTriggerEventsListRequiredException 
  { 
  }
derive instance newtypeRepositoryTriggerEventsListRequiredException :: Newtype RepositoryTriggerEventsListRequiredException _


-- | <p>A trigger failed to run.</p>
newtype RepositoryTriggerExecutionFailure = RepositoryTriggerExecutionFailure 
  { "Trigger'" :: NullOrUndefined (RepositoryTriggerName)
  , "FailureMessage'" :: NullOrUndefined (RepositoryTriggerExecutionFailureMessage)
  }
derive instance newtypeRepositoryTriggerExecutionFailure :: Newtype RepositoryTriggerExecutionFailure _


newtype RepositoryTriggerExecutionFailureList = RepositoryTriggerExecutionFailureList (Array RepositoryTriggerExecutionFailure)
derive instance newtypeRepositoryTriggerExecutionFailureList :: Newtype RepositoryTriggerExecutionFailureList _


newtype RepositoryTriggerExecutionFailureMessage = RepositoryTriggerExecutionFailureMessage String
derive instance newtypeRepositoryTriggerExecutionFailureMessage :: Newtype RepositoryTriggerExecutionFailureMessage _


newtype RepositoryTriggerName = RepositoryTriggerName String
derive instance newtypeRepositoryTriggerName :: Newtype RepositoryTriggerName _


newtype RepositoryTriggerNameList = RepositoryTriggerNameList (Array RepositoryTriggerName)
derive instance newtypeRepositoryTriggerNameList :: Newtype RepositoryTriggerNameList _


-- | <p>A name for the trigger is required but was not specified.</p>
newtype RepositoryTriggerNameRequiredException = RepositoryTriggerNameRequiredException 
  { 
  }
derive instance newtypeRepositoryTriggerNameRequiredException :: Newtype RepositoryTriggerNameRequiredException _


newtype RepositoryTriggersConfigurationId = RepositoryTriggersConfigurationId String
derive instance newtypeRepositoryTriggersConfigurationId :: Newtype RepositoryTriggersConfigurationId _


newtype RepositoryTriggersList = RepositoryTriggersList (Array RepositoryTrigger)
derive instance newtypeRepositoryTriggersList :: Newtype RepositoryTriggersList _


-- | <p>The list of triggers for the repository is required but was not specified.</p>
newtype RepositoryTriggersListRequiredException = RepositoryTriggersListRequiredException 
  { 
  }
derive instance newtypeRepositoryTriggersListRequiredException :: Newtype RepositoryTriggersListRequiredException _


-- | <p>The file was not added or updated because the content of the file is exactly the same as the content of that file in the repository and branch that you specified.</p>
newtype SameFileContentException = SameFileContentException 
  { 
  }
derive instance newtypeSameFileContentException :: Newtype SameFileContentException _


newtype SortByEnum = SortByEnum String
derive instance newtypeSortByEnum :: Newtype SortByEnum _


-- | <p>The source branch and the destination branch for the pull request are the same. You must specify different branches for the source and destination.</p>
newtype SourceAndDestinationAreSameException = SourceAndDestinationAreSameException 
  { 
  }
derive instance newtypeSourceAndDestinationAreSameException :: Newtype SourceAndDestinationAreSameException _


-- | <p>Returns information about a target for a pull request.</p>
newtype Target = Target 
  { "RepositoryName'" :: (RepositoryName)
  , "SourceReference'" :: (ReferenceName)
  , "DestinationReference'" :: NullOrUndefined (ReferenceName)
  }
derive instance newtypeTarget :: Newtype Target _


newtype TargetList = TargetList (Array Target)
derive instance newtypeTargetList :: Newtype TargetList _


-- | <p>A pull request target is required. It cannot be empty or null. A pull request target must contain the full values for the repository name, source branch, and destination branch for the pull request.</p>
newtype TargetRequiredException = TargetRequiredException 
  { 
  }
derive instance newtypeTargetRequiredException :: Newtype TargetRequiredException _


-- | <p>An array of target objects is required. It cannot be empty or null.</p>
newtype TargetsRequiredException = TargetsRequiredException 
  { 
  }
derive instance newtypeTargetsRequiredException :: Newtype TargetsRequiredException _


-- | <p>Represents the input of a test repository triggers operation.</p>
newtype TestRepositoryTriggersInput = TestRepositoryTriggersInput 
  { "RepositoryName'" :: (RepositoryName)
  , "Triggers'" :: (RepositoryTriggersList)
  }
derive instance newtypeTestRepositoryTriggersInput :: Newtype TestRepositoryTriggersInput _


-- | <p>Represents the output of a test repository triggers operation.</p>
newtype TestRepositoryTriggersOutput = TestRepositoryTriggersOutput 
  { "SuccessfulExecutions'" :: NullOrUndefined (RepositoryTriggerNameList)
  , "FailedExecutions'" :: NullOrUndefined (RepositoryTriggerExecutionFailureList)
  }
derive instance newtypeTestRepositoryTriggersOutput :: Newtype TestRepositoryTriggersOutput _


-- | <p>The tip of the source branch in the destination repository does not match the tip of the source branch specified in your request. The pull request might have been updated. Make sure that you have the latest changes.</p>
newtype TipOfSourceReferenceIsDifferentException = TipOfSourceReferenceIsDifferentException 
  { 
  }
derive instance newtypeTipOfSourceReferenceIsDifferentException :: Newtype TipOfSourceReferenceIsDifferentException _


-- | <p>The divergence between the tips of the provided commit specifiers is too great to determine whether there might be any merge conflicts. Locally compare the specifiers using <code>git diff</code> or a diff tool.</p>
newtype TipsDivergenceExceededException = TipsDivergenceExceededException 
  { 
  }
derive instance newtypeTipsDivergenceExceededException :: Newtype TipsDivergenceExceededException _


newtype Title = Title String
derive instance newtypeTitle :: Newtype Title _


-- | <p>A pull request title is required. It cannot be empty or null.</p>
newtype TitleRequiredException = TitleRequiredException 
  { 
  }
derive instance newtypeTitleRequiredException :: Newtype TitleRequiredException _


newtype UpdateCommentInput = UpdateCommentInput 
  { "CommentId'" :: (CommentId)
  , "Content'" :: (Content)
  }
derive instance newtypeUpdateCommentInput :: Newtype UpdateCommentInput _


newtype UpdateCommentOutput = UpdateCommentOutput 
  { "Comment'" :: NullOrUndefined (Comment)
  }
derive instance newtypeUpdateCommentOutput :: Newtype UpdateCommentOutput _


-- | <p>Represents the input of an update default branch operation.</p>
newtype UpdateDefaultBranchInput = UpdateDefaultBranchInput 
  { "RepositoryName'" :: (RepositoryName)
  , "DefaultBranchName'" :: (BranchName)
  }
derive instance newtypeUpdateDefaultBranchInput :: Newtype UpdateDefaultBranchInput _


newtype UpdatePullRequestDescriptionInput = UpdatePullRequestDescriptionInput 
  { "PullRequestId'" :: (PullRequestId)
  , "Description'" :: (Description)
  }
derive instance newtypeUpdatePullRequestDescriptionInput :: Newtype UpdatePullRequestDescriptionInput _


newtype UpdatePullRequestDescriptionOutput = UpdatePullRequestDescriptionOutput 
  { "PullRequest'" :: (PullRequest)
  }
derive instance newtypeUpdatePullRequestDescriptionOutput :: Newtype UpdatePullRequestDescriptionOutput _


newtype UpdatePullRequestStatusInput = UpdatePullRequestStatusInput 
  { "PullRequestId'" :: (PullRequestId)
  , "PullRequestStatus'" :: (PullRequestStatusEnum)
  }
derive instance newtypeUpdatePullRequestStatusInput :: Newtype UpdatePullRequestStatusInput _


newtype UpdatePullRequestStatusOutput = UpdatePullRequestStatusOutput 
  { "PullRequest'" :: (PullRequest)
  }
derive instance newtypeUpdatePullRequestStatusOutput :: Newtype UpdatePullRequestStatusOutput _


newtype UpdatePullRequestTitleInput = UpdatePullRequestTitleInput 
  { "PullRequestId'" :: (PullRequestId)
  , "Title'" :: (Title)
  }
derive instance newtypeUpdatePullRequestTitleInput :: Newtype UpdatePullRequestTitleInput _


newtype UpdatePullRequestTitleOutput = UpdatePullRequestTitleOutput 
  { "PullRequest'" :: (PullRequest)
  }
derive instance newtypeUpdatePullRequestTitleOutput :: Newtype UpdatePullRequestTitleOutput _


-- | <p>Represents the input of an update repository description operation.</p>
newtype UpdateRepositoryDescriptionInput = UpdateRepositoryDescriptionInput 
  { "RepositoryName'" :: (RepositoryName)
  , "RepositoryDescription'" :: NullOrUndefined (RepositoryDescription)
  }
derive instance newtypeUpdateRepositoryDescriptionInput :: Newtype UpdateRepositoryDescriptionInput _


-- | <p>Represents the input of an update repository description operation.</p>
newtype UpdateRepositoryNameInput = UpdateRepositoryNameInput 
  { "OldName'" :: (RepositoryName)
  , "NewName'" :: (RepositoryName)
  }
derive instance newtypeUpdateRepositoryNameInput :: Newtype UpdateRepositoryNameInput _


-- | <p>Information about the user who made a specified commit.</p>
newtype UserInfo = UserInfo 
  { "Name'" :: NullOrUndefined (Name)
  , "Email'" :: NullOrUndefined (Email)
  , "Date'" :: NullOrUndefined (Date)
  }
derive instance newtypeUserInfo :: Newtype UserInfo _
