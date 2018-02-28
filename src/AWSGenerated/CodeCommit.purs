

-- | <fullname>AWS CodeCommit</fullname> <p>This is the <i>AWS CodeCommit API Reference</i>. This reference provides descriptions of the operations and data types for AWS CodeCommit API along with usage examples.</p> <p>You can use the AWS CodeCommit API to work with the following objects:</p> <p>Repositories, by calling the following:</p> <ul> <li> <p> <a>BatchGetRepositories</a>, which returns information about one or more repositories associated with your AWS account.</p> </li> <li> <p> <a>CreateRepository</a>, which creates an AWS CodeCommit repository.</p> </li> <li> <p> <a>DeleteRepository</a>, which deletes an AWS CodeCommit repository.</p> </li> <li> <p> <a>GetRepository</a>, which returns information about a specified repository.</p> </li> <li> <p> <a>ListRepositories</a>, which lists all AWS CodeCommit repositories associated with your AWS account.</p> </li> <li> <p> <a>UpdateRepositoryDescription</a>, which sets or updates the description of the repository.</p> </li> <li> <p> <a>UpdateRepositoryName</a>, which changes the name of the repository. If you change the name of a repository, no other users of that repository will be able to access it until you send them the new HTTPS or SSH URL to use.</p> </li> </ul> <p>Branches, by calling the following:</p> <ul> <li> <p> <a>CreateBranch</a>, which creates a new branch in a specified repository.</p> </li> <li> <p> <a>DeleteBranch</a>, which deletes the specified branch in a repository unless it is the default branch.</p> </li> <li> <p> <a>GetBranch</a>, which returns information about a specified branch.</p> </li> <li> <p> <a>ListBranches</a>, which lists all branches for a specified repository.</p> </li> <li> <p> <a>UpdateDefaultBranch</a>, which changes the default branch for a repository.</p> </li> </ul> <p>Files, by calling the following:</p> <ul> <li> <p> <a>PutFile</a>, which adds or modifies a file in a specified repository and branch.</p> </li> </ul> <p>Information about committed code in a repository, by calling the following:</p> <ul> <li> <p> <a>GetBlob</a>, which returns the base-64 encoded content of an individual Git blob object within a repository.</p> </li> <li> <p> <a>GetCommit</a>, which returns information about a commit, including commit messages and author and committer information.</p> </li> <li> <p> <a>GetDifferences</a>, which returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID or other fully qualified reference).</p> </li> </ul> <p>Pull requests, by calling the following:</p> <ul> <li> <p> <a>CreatePullRequest</a>, which creates a pull request in a specified repository.</p> </li> <li> <p> <a>DescribePullRequestEvents</a>, which returns information about one or more pull request events.</p> </li> <li> <p> <a>GetCommentsForPullRequest</a>, which returns information about comments on a specified pull request.</p> </li> <li> <p> <a>GetMergeConflicts</a>, which returns information about merge conflicts between the source and destination branch in a pull request.</p> </li> <li> <p> <a>GetPullRequest</a>, which returns information about a specified pull request.</p> </li> <li> <p> <a>ListPullRequests</a>, which lists all pull requests for a repository.</p> </li> <li> <p> <a>MergePullRequestByFastForward</a>, which merges the source destination branch of a pull request into the specified destination branch for that pull request using the fast-forward merge option.</p> </li> <li> <p> <a>PostCommentForPullRequest</a>, which posts a comment to a pull request at the specified line, file, or request.</p> </li> <li> <p> <a>UpdatePullRequestDescription</a>, which updates the description of a pull request.</p> </li> <li> <p> <a>UpdatePullRequestStatus</a>, which updates the status of a pull request.</p> </li> <li> <p> <a>UpdatePullRequestTitle</a>, which updates the title of a pull request.</p> </li> </ul> <p>Information about comments in a repository, by calling the following:</p> <ul> <li> <p> <a>DeleteCommentContent</a>, which deletes the content of a comment on a commit in a repository.</p> </li> <li> <p> <a>GetComment</a>, which returns information about a comment on a commit.</p> </li> <li> <p> <a>GetCommentsForComparedCommit</a>, which returns information about comments on the comparison between two commit specifiers in a repository.</p> </li> <li> <p> <a>PostCommentForComparedCommit</a>, which creates a comment on the comparison between two commit specifiers in a repository.</p> </li> <li> <p> <a>PostCommentReply</a>, which creates a reply to a comment.</p> </li> <li> <p> <a>UpdateComment</a>, which updates the content of a comment on a commit in a repository.</p> </li> </ul> <p>Triggers, by calling the following:</p> <ul> <li> <p> <a>GetRepositoryTriggers</a>, which returns information about triggers configured for a repository.</p> </li> <li> <p> <a>PutRepositoryTriggers</a>, which replaces all triggers for a repository and can be used to create or delete triggers.</p> </li> <li> <p> <a>TestRepositoryTriggers</a>, which tests the functionality of a repository trigger by sending data to the trigger target.</p> </li> </ul> <p>For information about how to use AWS CodeCommit, see the <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html">AWS CodeCommit User Guide</a>.</p>
module AWS.CodeCommit where

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

serviceName = "CodeCommit" :: String


-- | <p>Returns information about one or more repositories.</p> <note> <p>The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.</p> </note>
batchGetRepositories :: forall eff. BatchGetRepositoriesInput -> Aff (exception :: EXCEPTION | eff) BatchGetRepositoriesOutput
batchGetRepositories = Request.request serviceName "batchGetRepositories" 


-- | <p>Creates a new branch in a repository and points the branch to a commit.</p> <note> <p>Calling the create branch operation does not set a repository's default branch. To do this, call the update default branch operation.</p> </note>
createBranch :: forall eff. CreateBranchInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
createBranch = Request.request serviceName "createBranch" 


-- | <p>Creates a pull request in the specified repository.</p>
createPullRequest :: forall eff. CreatePullRequestInput -> Aff (exception :: EXCEPTION | eff) CreatePullRequestOutput
createPullRequest = Request.request serviceName "createPullRequest" 


-- | <p>Creates a new, empty repository.</p>
createRepository :: forall eff. CreateRepositoryInput -> Aff (exception :: EXCEPTION | eff) CreateRepositoryOutput
createRepository = Request.request serviceName "createRepository" 


-- | <p>Deletes a branch from a repository, unless that branch is the default branch for the repository. </p>
deleteBranch :: forall eff. DeleteBranchInput -> Aff (exception :: EXCEPTION | eff) DeleteBranchOutput
deleteBranch = Request.request serviceName "deleteBranch" 


-- | <p>Deletes the content of a comment made on a change, file, or commit in a repository.</p>
deleteCommentContent :: forall eff. DeleteCommentContentInput -> Aff (exception :: EXCEPTION | eff) DeleteCommentContentOutput
deleteCommentContent = Request.request serviceName "deleteCommentContent" 


-- | <p>Deletes a repository. If a specified repository was already deleted, a null repository ID will be returned.</p> <important> <p>Deleting a repository also deletes all associated objects and metadata. After a repository is deleted, all future push calls to the deleted repository will fail.</p> </important>
deleteRepository :: forall eff. DeleteRepositoryInput -> Aff (exception :: EXCEPTION | eff) DeleteRepositoryOutput
deleteRepository = Request.request serviceName "deleteRepository" 


-- | <p>Returns information about one or more pull request events.</p>
describePullRequestEvents :: forall eff. DescribePullRequestEventsInput -> Aff (exception :: EXCEPTION | eff) DescribePullRequestEventsOutput
describePullRequestEvents = Request.request serviceName "describePullRequestEvents" 


-- | <p>Returns the base-64 encoded content of an individual blob within a repository.</p>
getBlob :: forall eff. GetBlobInput -> Aff (exception :: EXCEPTION | eff) GetBlobOutput
getBlob = Request.request serviceName "getBlob" 


-- | <p>Returns information about a repository branch, including its name and the last commit ID.</p>
getBranch :: forall eff. GetBranchInput -> Aff (exception :: EXCEPTION | eff) GetBranchOutput
getBranch = Request.request serviceName "getBranch" 


-- | <p>Returns the content of a comment made on a change, file, or commit in a repository.</p>
getComment :: forall eff. GetCommentInput -> Aff (exception :: EXCEPTION | eff) GetCommentOutput
getComment = Request.request serviceName "getComment" 


-- | <p>Returns information about comments made on the comparison between two commits.</p>
getCommentsForComparedCommit :: forall eff. GetCommentsForComparedCommitInput -> Aff (exception :: EXCEPTION | eff) GetCommentsForComparedCommitOutput
getCommentsForComparedCommit = Request.request serviceName "getCommentsForComparedCommit" 


-- | <p>Returns comments made on a pull request.</p>
getCommentsForPullRequest :: forall eff. GetCommentsForPullRequestInput -> Aff (exception :: EXCEPTION | eff) GetCommentsForPullRequestOutput
getCommentsForPullRequest = Request.request serviceName "getCommentsForPullRequest" 


-- | <p>Returns information about a commit, including commit message and committer information.</p>
getCommit :: forall eff. GetCommitInput -> Aff (exception :: EXCEPTION | eff) GetCommitOutput
getCommit = Request.request serviceName "getCommit" 


-- | <p>Returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID or other fully qualified reference). Results can be limited to a specified path.</p>
getDifferences :: forall eff. GetDifferencesInput -> Aff (exception :: EXCEPTION | eff) GetDifferencesOutput
getDifferences = Request.request serviceName "getDifferences" 


-- | <p>Returns information about merge conflicts between the before and after commit IDs for a pull request in a repository.</p>
getMergeConflicts :: forall eff. GetMergeConflictsInput -> Aff (exception :: EXCEPTION | eff) GetMergeConflictsOutput
getMergeConflicts = Request.request serviceName "getMergeConflicts" 


-- | <p>Gets information about a pull request in a specified repository.</p>
getPullRequest :: forall eff. GetPullRequestInput -> Aff (exception :: EXCEPTION | eff) GetPullRequestOutput
getPullRequest = Request.request serviceName "getPullRequest" 


-- | <p>Returns information about a repository.</p> <note> <p>The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.</p> </note>
getRepository :: forall eff. GetRepositoryInput -> Aff (exception :: EXCEPTION | eff) GetRepositoryOutput
getRepository = Request.request serviceName "getRepository" 


-- | <p>Gets information about triggers configured for a repository.</p>
getRepositoryTriggers :: forall eff. GetRepositoryTriggersInput -> Aff (exception :: EXCEPTION | eff) GetRepositoryTriggersOutput
getRepositoryTriggers = Request.request serviceName "getRepositoryTriggers" 


-- | <p>Gets information about one or more branches in a repository.</p>
listBranches :: forall eff. ListBranchesInput -> Aff (exception :: EXCEPTION | eff) ListBranchesOutput
listBranches = Request.request serviceName "listBranches" 


-- | <p>Returns a list of pull requests for a specified repository. The return list can be refined by pull request status or pull request author ARN.</p>
listPullRequests :: forall eff. ListPullRequestsInput -> Aff (exception :: EXCEPTION | eff) ListPullRequestsOutput
listPullRequests = Request.request serviceName "listPullRequests" 


-- | <p>Gets information about one or more repositories.</p>
listRepositories :: forall eff. ListRepositoriesInput -> Aff (exception :: EXCEPTION | eff) ListRepositoriesOutput
listRepositories = Request.request serviceName "listRepositories" 


-- | <p>Closes a pull request and attempts to merge the source commit of a pull request into the specified destination branch for that pull request at the specified commit using the fast-forward merge option.</p>
mergePullRequestByFastForward :: forall eff. MergePullRequestByFastForwardInput -> Aff (exception :: EXCEPTION | eff) MergePullRequestByFastForwardOutput
mergePullRequestByFastForward = Request.request serviceName "mergePullRequestByFastForward" 


-- | <p>Posts a comment on the comparison between two commits.</p>
postCommentForComparedCommit :: forall eff. PostCommentForComparedCommitInput -> Aff (exception :: EXCEPTION | eff) PostCommentForComparedCommitOutput
postCommentForComparedCommit = Request.request serviceName "postCommentForComparedCommit" 


-- | <p>Posts a comment on a pull request.</p>
postCommentForPullRequest :: forall eff. PostCommentForPullRequestInput -> Aff (exception :: EXCEPTION | eff) PostCommentForPullRequestOutput
postCommentForPullRequest = Request.request serviceName "postCommentForPullRequest" 


-- | <p>Posts a comment in reply to an existing comment on a comparison between commits or a pull request.</p>
postCommentReply :: forall eff. PostCommentReplyInput -> Aff (exception :: EXCEPTION | eff) PostCommentReplyOutput
postCommentReply = Request.request serviceName "postCommentReply" 


-- | <p>Adds or updates a file in an AWS CodeCommit repository.</p>
putFile :: forall eff. PutFileInput -> Aff (exception :: EXCEPTION | eff) PutFileOutput
putFile = Request.request serviceName "putFile" 


-- | <p>Replaces all triggers for a repository. This can be used to create or delete triggers.</p>
putRepositoryTriggers :: forall eff. PutRepositoryTriggersInput -> Aff (exception :: EXCEPTION | eff) PutRepositoryTriggersOutput
putRepositoryTriggers = Request.request serviceName "putRepositoryTriggers" 


-- | <p>Tests the functionality of repository triggers by sending information to the trigger target. If real data is available in the repository, the test will send data from the last commit. If no data is available, sample data will be generated.</p>
testRepositoryTriggers :: forall eff. TestRepositoryTriggersInput -> Aff (exception :: EXCEPTION | eff) TestRepositoryTriggersOutput
testRepositoryTriggers = Request.request serviceName "testRepositoryTriggers" 


-- | <p>Replaces the contents of a comment.</p>
updateComment :: forall eff. UpdateCommentInput -> Aff (exception :: EXCEPTION | eff) UpdateCommentOutput
updateComment = Request.request serviceName "updateComment" 


-- | <p>Sets or changes the default branch name for the specified repository.</p> <note> <p>If you use this operation to change the default branch name to the current default branch name, a success message is returned even though the default branch did not change.</p> </note>
updateDefaultBranch :: forall eff. UpdateDefaultBranchInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateDefaultBranch = Request.request serviceName "updateDefaultBranch" 


-- | <p>Replaces the contents of the description of a pull request.</p>
updatePullRequestDescription :: forall eff. UpdatePullRequestDescriptionInput -> Aff (exception :: EXCEPTION | eff) UpdatePullRequestDescriptionOutput
updatePullRequestDescription = Request.request serviceName "updatePullRequestDescription" 


-- | <p>Updates the status of a pull request. </p>
updatePullRequestStatus :: forall eff. UpdatePullRequestStatusInput -> Aff (exception :: EXCEPTION | eff) UpdatePullRequestStatusOutput
updatePullRequestStatus = Request.request serviceName "updatePullRequestStatus" 


-- | <p>Replaces the title of a pull request.</p>
updatePullRequestTitle :: forall eff. UpdatePullRequestTitleInput -> Aff (exception :: EXCEPTION | eff) UpdatePullRequestTitleOutput
updatePullRequestTitle = Request.request serviceName "updatePullRequestTitle" 


-- | <p>Sets or changes the comment or description for a repository.</p> <note> <p>The description field for a repository accepts all HTML characters and all valid Unicode characters. Applications that do not HTML-encode the description and display it in a web page could expose users to potentially malicious code. Make sure that you HTML-encode the description field in any application that uses this API to display the repository description on a web page.</p> </note>
updateRepositoryDescription :: forall eff. UpdateRepositoryDescriptionInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateRepositoryDescription = Request.request serviceName "updateRepositoryDescription" 


-- | <p>Renames a repository. The repository name must be unique across the calling AWS account. In addition, repository names are limited to 100 alphanumeric, dash, and underscore characters, and cannot include certain characters. The suffix ".git" is prohibited. For a full description of the limits on repository names, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/limits.html">Limits</a> in the AWS CodeCommit User Guide.</p>
updateRepositoryName :: forall eff. UpdateRepositoryNameInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateRepositoryName = Request.request serviceName "updateRepositoryName" 


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _
derive instance repGenericAccountId :: Generic AccountId _
instance showAccountId :: Show AccountId where
  show = genericShow
instance decodeAccountId :: Decode AccountId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountId :: Encode AccountId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified Amazon Resource Name (ARN) does not exist in the AWS account.</p>
newtype ActorDoesNotExistException = ActorDoesNotExistException Types.NoArguments
derive instance newtypeActorDoesNotExistException :: Newtype ActorDoesNotExistException _
derive instance repGenericActorDoesNotExistException :: Generic ActorDoesNotExistException _
instance showActorDoesNotExistException :: Show ActorDoesNotExistException where
  show = genericShow
instance decodeActorDoesNotExistException :: Decode ActorDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActorDoesNotExistException :: Encode ActorDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdditionalData = AdditionalData String
derive instance newtypeAdditionalData :: Newtype AdditionalData _
derive instance repGenericAdditionalData :: Generic AdditionalData _
instance showAdditionalData :: Show AdditionalData where
  show = genericShow
instance decodeAdditionalData :: Decode AdditionalData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdditionalData :: Encode AdditionalData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _
derive instance repGenericArn :: Generic Arn _
instance showArn :: Show Arn where
  show = genericShow
instance decodeArn :: Decode Arn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArn :: Encode Arn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified Amazon Resource Name (ARN) does not exist in the AWS account.</p>
newtype AuthorDoesNotExistException = AuthorDoesNotExistException Types.NoArguments
derive instance newtypeAuthorDoesNotExistException :: Newtype AuthorDoesNotExistException _
derive instance repGenericAuthorDoesNotExistException :: Generic AuthorDoesNotExistException _
instance showAuthorDoesNotExistException :: Show AuthorDoesNotExistException where
  show = genericShow
instance decodeAuthorDoesNotExistException :: Decode AuthorDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorDoesNotExistException :: Encode AuthorDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a batch get repositories operation.</p>
newtype BatchGetRepositoriesInput = BatchGetRepositoriesInput 
  { "RepositoryNames'" :: (RepositoryNameList)
  }
derive instance newtypeBatchGetRepositoriesInput :: Newtype BatchGetRepositoriesInput _
derive instance repGenericBatchGetRepositoriesInput :: Generic BatchGetRepositoriesInput _
instance showBatchGetRepositoriesInput :: Show BatchGetRepositoriesInput where
  show = genericShow
instance decodeBatchGetRepositoriesInput :: Decode BatchGetRepositoriesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetRepositoriesInput :: Encode BatchGetRepositoriesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a batch get repositories operation.</p>
newtype BatchGetRepositoriesOutput = BatchGetRepositoriesOutput 
  { "Repositories'" :: NullOrUndefined.NullOrUndefined (RepositoryMetadataList)
  , "RepositoriesNotFound'" :: NullOrUndefined.NullOrUndefined (RepositoryNotFoundList)
  }
derive instance newtypeBatchGetRepositoriesOutput :: Newtype BatchGetRepositoriesOutput _
derive instance repGenericBatchGetRepositoriesOutput :: Generic BatchGetRepositoriesOutput _
instance showBatchGetRepositoriesOutput :: Show BatchGetRepositoriesOutput where
  show = genericShow
instance decodeBatchGetRepositoriesOutput :: Decode BatchGetRepositoriesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetRepositoriesOutput :: Encode BatchGetRepositoriesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The before commit ID and the after commit ID are the same, which is not valid. The before commit ID and the after commit ID must be different commit IDs.</p>
newtype BeforeCommitIdAndAfterCommitIdAreSameException = BeforeCommitIdAndAfterCommitIdAreSameException Types.NoArguments
derive instance newtypeBeforeCommitIdAndAfterCommitIdAreSameException :: Newtype BeforeCommitIdAndAfterCommitIdAreSameException _
derive instance repGenericBeforeCommitIdAndAfterCommitIdAreSameException :: Generic BeforeCommitIdAndAfterCommitIdAreSameException _
instance showBeforeCommitIdAndAfterCommitIdAreSameException :: Show BeforeCommitIdAndAfterCommitIdAreSameException where
  show = genericShow
instance decodeBeforeCommitIdAndAfterCommitIdAreSameException :: Decode BeforeCommitIdAndAfterCommitIdAreSameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBeforeCommitIdAndAfterCommitIdAreSameException :: Encode BeforeCommitIdAndAfterCommitIdAreSameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified blob does not exist.</p>
newtype BlobIdDoesNotExistException = BlobIdDoesNotExistException Types.NoArguments
derive instance newtypeBlobIdDoesNotExistException :: Newtype BlobIdDoesNotExistException _
derive instance repGenericBlobIdDoesNotExistException :: Generic BlobIdDoesNotExistException _
instance showBlobIdDoesNotExistException :: Show BlobIdDoesNotExistException where
  show = genericShow
instance decodeBlobIdDoesNotExistException :: Decode BlobIdDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlobIdDoesNotExistException :: Encode BlobIdDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A blob ID is required but was not specified.</p>
newtype BlobIdRequiredException = BlobIdRequiredException Types.NoArguments
derive instance newtypeBlobIdRequiredException :: Newtype BlobIdRequiredException _
derive instance repGenericBlobIdRequiredException :: Generic BlobIdRequiredException _
instance showBlobIdRequiredException :: Show BlobIdRequiredException where
  show = genericShow
instance decodeBlobIdRequiredException :: Decode BlobIdRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlobIdRequiredException :: Encode BlobIdRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about a specific Git blob object.</p>
newtype BlobMetadata = BlobMetadata 
  { "BlobId'" :: NullOrUndefined.NullOrUndefined (ObjectId)
  , "Path'" :: NullOrUndefined.NullOrUndefined (Path)
  , "Mode'" :: NullOrUndefined.NullOrUndefined (Mode)
  }
derive instance newtypeBlobMetadata :: Newtype BlobMetadata _
derive instance repGenericBlobMetadata :: Generic BlobMetadata _
instance showBlobMetadata :: Show BlobMetadata where
  show = genericShow
instance decodeBlobMetadata :: Decode BlobMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlobMetadata :: Encode BlobMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified branch does not exist.</p>
newtype BranchDoesNotExistException = BranchDoesNotExistException Types.NoArguments
derive instance newtypeBranchDoesNotExistException :: Newtype BranchDoesNotExistException _
derive instance repGenericBranchDoesNotExistException :: Generic BranchDoesNotExistException _
instance showBranchDoesNotExistException :: Show BranchDoesNotExistException where
  show = genericShow
instance decodeBranchDoesNotExistException :: Decode BranchDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBranchDoesNotExistException :: Encode BranchDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about a branch.</p>
newtype BranchInfo = BranchInfo 
  { "BranchName'" :: NullOrUndefined.NullOrUndefined (BranchName)
  , "CommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  }
derive instance newtypeBranchInfo :: Newtype BranchInfo _
derive instance repGenericBranchInfo :: Generic BranchInfo _
instance showBranchInfo :: Show BranchInfo where
  show = genericShow
instance decodeBranchInfo :: Decode BranchInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBranchInfo :: Encode BranchInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BranchName = BranchName String
derive instance newtypeBranchName :: Newtype BranchName _
derive instance repGenericBranchName :: Generic BranchName _
instance showBranchName :: Show BranchName where
  show = genericShow
instance decodeBranchName :: Decode BranchName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBranchName :: Encode BranchName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified branch name already exists.</p>
newtype BranchNameExistsException = BranchNameExistsException Types.NoArguments
derive instance newtypeBranchNameExistsException :: Newtype BranchNameExistsException _
derive instance repGenericBranchNameExistsException :: Generic BranchNameExistsException _
instance showBranchNameExistsException :: Show BranchNameExistsException where
  show = genericShow
instance decodeBranchNameExistsException :: Decode BranchNameExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBranchNameExistsException :: Encode BranchNameExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified branch name is not valid because it is a tag name. Type the name of a current branch in the repository. For a list of valid branch names, use <a>ListBranches</a>.</p>
newtype BranchNameIsTagNameException = BranchNameIsTagNameException Types.NoArguments
derive instance newtypeBranchNameIsTagNameException :: Newtype BranchNameIsTagNameException _
derive instance repGenericBranchNameIsTagNameException :: Generic BranchNameIsTagNameException _
instance showBranchNameIsTagNameException :: Show BranchNameIsTagNameException where
  show = genericShow
instance decodeBranchNameIsTagNameException :: Decode BranchNameIsTagNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBranchNameIsTagNameException :: Encode BranchNameIsTagNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BranchNameList = BranchNameList (Array BranchName)
derive instance newtypeBranchNameList :: Newtype BranchNameList _
derive instance repGenericBranchNameList :: Generic BranchNameList _
instance showBranchNameList :: Show BranchNameList where
  show = genericShow
instance decodeBranchNameList :: Decode BranchNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBranchNameList :: Encode BranchNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A branch name is required but was not specified.</p>
newtype BranchNameRequiredException = BranchNameRequiredException Types.NoArguments
derive instance newtypeBranchNameRequiredException :: Newtype BranchNameRequiredException _
derive instance repGenericBranchNameRequiredException :: Generic BranchNameRequiredException _
instance showBranchNameRequiredException :: Show BranchNameRequiredException where
  show = genericShow
instance decodeBranchNameRequiredException :: Decode BranchNameRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBranchNameRequiredException :: Encode BranchNameRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeTypeEnum = ChangeTypeEnum String
derive instance newtypeChangeTypeEnum :: Newtype ChangeTypeEnum _
derive instance repGenericChangeTypeEnum :: Generic ChangeTypeEnum _
instance showChangeTypeEnum :: Show ChangeTypeEnum where
  show = genericShow
instance decodeChangeTypeEnum :: Decode ChangeTypeEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeTypeEnum :: Encode ChangeTypeEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientRequestToken = ClientRequestToken String
derive instance newtypeClientRequestToken :: Newtype ClientRequestToken _
derive instance repGenericClientRequestToken :: Generic ClientRequestToken _
instance showClientRequestToken :: Show ClientRequestToken where
  show = genericShow
instance decodeClientRequestToken :: Decode ClientRequestToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientRequestToken :: Encode ClientRequestToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A client request token is required. A client request token is an unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.</p>
newtype ClientRequestTokenRequiredException = ClientRequestTokenRequiredException Types.NoArguments
derive instance newtypeClientRequestTokenRequiredException :: Newtype ClientRequestTokenRequiredException _
derive instance repGenericClientRequestTokenRequiredException :: Generic ClientRequestTokenRequiredException _
instance showClientRequestTokenRequiredException :: Show ClientRequestTokenRequiredException where
  show = genericShow
instance decodeClientRequestTokenRequiredException :: Decode ClientRequestTokenRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientRequestTokenRequiredException :: Encode ClientRequestTokenRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloneUrlHttp = CloneUrlHttp String
derive instance newtypeCloneUrlHttp :: Newtype CloneUrlHttp _
derive instance repGenericCloneUrlHttp :: Generic CloneUrlHttp _
instance showCloneUrlHttp :: Show CloneUrlHttp where
  show = genericShow
instance decodeCloneUrlHttp :: Decode CloneUrlHttp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloneUrlHttp :: Encode CloneUrlHttp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloneUrlSsh = CloneUrlSsh String
derive instance newtypeCloneUrlSsh :: Newtype CloneUrlSsh _
derive instance repGenericCloneUrlSsh :: Generic CloneUrlSsh _
instance showCloneUrlSsh :: Show CloneUrlSsh where
  show = genericShow
instance decodeCloneUrlSsh :: Decode CloneUrlSsh where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloneUrlSsh :: Encode CloneUrlSsh where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about a specific comment.</p>
newtype Comment = Comment 
  { "CommentId'" :: NullOrUndefined.NullOrUndefined (CommentId)
  , "Content'" :: NullOrUndefined.NullOrUndefined (Content)
  , "InReplyTo'" :: NullOrUndefined.NullOrUndefined (CommentId)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (CreationDate)
  , "LastModifiedDate'" :: NullOrUndefined.NullOrUndefined (LastModifiedDate)
  , "AuthorArn'" :: NullOrUndefined.NullOrUndefined (Arn)
  , "Deleted'" :: NullOrUndefined.NullOrUndefined (IsCommentDeleted)
  , "ClientRequestToken'" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeComment :: Newtype Comment _
derive instance repGenericComment :: Generic Comment _
instance showComment :: Show Comment where
  show = genericShow
instance decodeComment :: Decode Comment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComment :: Encode Comment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The comment is empty. You must provide some content for a comment. The content cannot be null.</p>
newtype CommentContentRequiredException = CommentContentRequiredException Types.NoArguments
derive instance newtypeCommentContentRequiredException :: Newtype CommentContentRequiredException _
derive instance repGenericCommentContentRequiredException :: Generic CommentContentRequiredException _
instance showCommentContentRequiredException :: Show CommentContentRequiredException where
  show = genericShow
instance decodeCommentContentRequiredException :: Decode CommentContentRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentContentRequiredException :: Encode CommentContentRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The comment is too large. Comments are limited to 1,000 characters.</p>
newtype CommentContentSizeLimitExceededException = CommentContentSizeLimitExceededException Types.NoArguments
derive instance newtypeCommentContentSizeLimitExceededException :: Newtype CommentContentSizeLimitExceededException _
derive instance repGenericCommentContentSizeLimitExceededException :: Generic CommentContentSizeLimitExceededException _
instance showCommentContentSizeLimitExceededException :: Show CommentContentSizeLimitExceededException where
  show = genericShow
instance decodeCommentContentSizeLimitExceededException :: Decode CommentContentSizeLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentContentSizeLimitExceededException :: Encode CommentContentSizeLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This comment has already been deleted. You cannot edit or delete a deleted comment.</p>
newtype CommentDeletedException = CommentDeletedException Types.NoArguments
derive instance newtypeCommentDeletedException :: Newtype CommentDeletedException _
derive instance repGenericCommentDeletedException :: Generic CommentDeletedException _
instance showCommentDeletedException :: Show CommentDeletedException where
  show = genericShow
instance decodeCommentDeletedException :: Decode CommentDeletedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentDeletedException :: Encode CommentDeletedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>No comment exists with the provided ID. Verify that you have provided the correct ID, and then try again.</p>
newtype CommentDoesNotExistException = CommentDoesNotExistException Types.NoArguments
derive instance newtypeCommentDoesNotExistException :: Newtype CommentDoesNotExistException _
derive instance repGenericCommentDoesNotExistException :: Generic CommentDoesNotExistException _
instance showCommentDoesNotExistException :: Show CommentDoesNotExistException where
  show = genericShow
instance decodeCommentDoesNotExistException :: Decode CommentDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentDoesNotExistException :: Encode CommentDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommentId = CommentId String
derive instance newtypeCommentId :: Newtype CommentId _
derive instance repGenericCommentId :: Generic CommentId _
instance showCommentId :: Show CommentId where
  show = genericShow
instance decodeCommentId :: Decode CommentId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentId :: Encode CommentId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The comment ID is missing or null. A comment ID is required.</p>
newtype CommentIdRequiredException = CommentIdRequiredException Types.NoArguments
derive instance newtypeCommentIdRequiredException :: Newtype CommentIdRequiredException _
derive instance repGenericCommentIdRequiredException :: Generic CommentIdRequiredException _
instance showCommentIdRequiredException :: Show CommentIdRequiredException where
  show = genericShow
instance decodeCommentIdRequiredException :: Decode CommentIdRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentIdRequiredException :: Encode CommentIdRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot modify or delete this comment. Only comment authors can modify or delete their comments.</p>
newtype CommentNotCreatedByCallerException = CommentNotCreatedByCallerException Types.NoArguments
derive instance newtypeCommentNotCreatedByCallerException :: Newtype CommentNotCreatedByCallerException _
derive instance repGenericCommentNotCreatedByCallerException :: Generic CommentNotCreatedByCallerException _
instance showCommentNotCreatedByCallerException :: Show CommentNotCreatedByCallerException where
  show = genericShow
instance decodeCommentNotCreatedByCallerException :: Decode CommentNotCreatedByCallerException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentNotCreatedByCallerException :: Encode CommentNotCreatedByCallerException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Comments = Comments (Array Comment)
derive instance newtypeComments :: Newtype Comments _
derive instance repGenericComments :: Generic Comments _
instance showComments :: Show Comments where
  show = genericShow
instance decodeComments :: Decode Comments where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComments :: Encode Comments where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about comments on the comparison between two commits.</p>
newtype CommentsForComparedCommit = CommentsForComparedCommit 
  { "RepositoryName'" :: NullOrUndefined.NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "BeforeBlobId'" :: NullOrUndefined.NullOrUndefined (ObjectId)
  , "AfterBlobId'" :: NullOrUndefined.NullOrUndefined (ObjectId)
  , "Location'" :: NullOrUndefined.NullOrUndefined (Location)
  , "Comments'" :: NullOrUndefined.NullOrUndefined (Comments)
  }
derive instance newtypeCommentsForComparedCommit :: Newtype CommentsForComparedCommit _
derive instance repGenericCommentsForComparedCommit :: Generic CommentsForComparedCommit _
instance showCommentsForComparedCommit :: Show CommentsForComparedCommit where
  show = genericShow
instance decodeCommentsForComparedCommit :: Decode CommentsForComparedCommit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentsForComparedCommit :: Encode CommentsForComparedCommit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommentsForComparedCommitData = CommentsForComparedCommitData (Array CommentsForComparedCommit)
derive instance newtypeCommentsForComparedCommitData :: Newtype CommentsForComparedCommitData _
derive instance repGenericCommentsForComparedCommitData :: Generic CommentsForComparedCommitData _
instance showCommentsForComparedCommitData :: Show CommentsForComparedCommitData where
  show = genericShow
instance decodeCommentsForComparedCommitData :: Decode CommentsForComparedCommitData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentsForComparedCommitData :: Encode CommentsForComparedCommitData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about comments on a pull request.</p>
newtype CommentsForPullRequest = CommentsForPullRequest 
  { "PullRequestId'" :: NullOrUndefined.NullOrUndefined (PullRequestId)
  , "RepositoryName'" :: NullOrUndefined.NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "BeforeBlobId'" :: NullOrUndefined.NullOrUndefined (ObjectId)
  , "AfterBlobId'" :: NullOrUndefined.NullOrUndefined (ObjectId)
  , "Location'" :: NullOrUndefined.NullOrUndefined (Location)
  , "Comments'" :: NullOrUndefined.NullOrUndefined (Comments)
  }
derive instance newtypeCommentsForPullRequest :: Newtype CommentsForPullRequest _
derive instance repGenericCommentsForPullRequest :: Generic CommentsForPullRequest _
instance showCommentsForPullRequest :: Show CommentsForPullRequest where
  show = genericShow
instance decodeCommentsForPullRequest :: Decode CommentsForPullRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentsForPullRequest :: Encode CommentsForPullRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommentsForPullRequestData = CommentsForPullRequestData (Array CommentsForPullRequest)
derive instance newtypeCommentsForPullRequestData :: Newtype CommentsForPullRequestData _
derive instance repGenericCommentsForPullRequestData :: Generic CommentsForPullRequestData _
instance showCommentsForPullRequestData :: Show CommentsForPullRequestData where
  show = genericShow
instance decodeCommentsForPullRequestData :: Decode CommentsForPullRequestData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentsForPullRequestData :: Encode CommentsForPullRequestData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about a specific commit.</p>
newtype Commit = Commit 
  { "CommitId'" :: NullOrUndefined.NullOrUndefined (ObjectId)
  , "TreeId'" :: NullOrUndefined.NullOrUndefined (ObjectId)
  , "Parents'" :: NullOrUndefined.NullOrUndefined (ParentList)
  , "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Author'" :: NullOrUndefined.NullOrUndefined (UserInfo)
  , "Committer'" :: NullOrUndefined.NullOrUndefined (UserInfo)
  , "AdditionalData'" :: NullOrUndefined.NullOrUndefined (AdditionalData)
  }
derive instance newtypeCommit :: Newtype Commit _
derive instance repGenericCommit :: Generic Commit _
instance showCommit :: Show Commit where
  show = genericShow
instance decodeCommit :: Decode Commit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommit :: Encode Commit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified commit does not exist or no commit was specified, and the specified repository has no default branch.</p>
newtype CommitDoesNotExistException = CommitDoesNotExistException Types.NoArguments
derive instance newtypeCommitDoesNotExistException :: Newtype CommitDoesNotExistException _
derive instance repGenericCommitDoesNotExistException :: Generic CommitDoesNotExistException _
instance showCommitDoesNotExistException :: Show CommitDoesNotExistException where
  show = genericShow
instance decodeCommitDoesNotExistException :: Decode CommitDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommitDoesNotExistException :: Encode CommitDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommitId = CommitId String
derive instance newtypeCommitId :: Newtype CommitId _
derive instance repGenericCommitId :: Generic CommitId _
instance showCommitId :: Show CommitId where
  show = genericShow
instance decodeCommitId :: Decode CommitId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommitId :: Encode CommitId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified commit ID does not exist.</p>
newtype CommitIdDoesNotExistException = CommitIdDoesNotExistException Types.NoArguments
derive instance newtypeCommitIdDoesNotExistException :: Newtype CommitIdDoesNotExistException _
derive instance repGenericCommitIdDoesNotExistException :: Generic CommitIdDoesNotExistException _
instance showCommitIdDoesNotExistException :: Show CommitIdDoesNotExistException where
  show = genericShow
instance decodeCommitIdDoesNotExistException :: Decode CommitIdDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommitIdDoesNotExistException :: Encode CommitIdDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A commit ID was not specified.</p>
newtype CommitIdRequiredException = CommitIdRequiredException Types.NoArguments
derive instance newtypeCommitIdRequiredException :: Newtype CommitIdRequiredException _
derive instance repGenericCommitIdRequiredException :: Generic CommitIdRequiredException _
instance showCommitIdRequiredException :: Show CommitIdRequiredException where
  show = genericShow
instance decodeCommitIdRequiredException :: Decode CommitIdRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommitIdRequiredException :: Encode CommitIdRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The commit message is too long. Provide a shorter string. </p>
newtype CommitMessageLengthExceededException = CommitMessageLengthExceededException Types.NoArguments
derive instance newtypeCommitMessageLengthExceededException :: Newtype CommitMessageLengthExceededException _
derive instance repGenericCommitMessageLengthExceededException :: Generic CommitMessageLengthExceededException _
instance showCommitMessageLengthExceededException :: Show CommitMessageLengthExceededException where
  show = genericShow
instance decodeCommitMessageLengthExceededException :: Decode CommitMessageLengthExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommitMessageLengthExceededException :: Encode CommitMessageLengthExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommitName = CommitName String
derive instance newtypeCommitName :: Newtype CommitName _
derive instance repGenericCommitName :: Generic CommitName _
instance showCommitName :: Show CommitName where
  show = genericShow
instance decodeCommitName :: Decode CommitName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommitName :: Encode CommitName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A commit was not specified.</p>
newtype CommitRequiredException = CommitRequiredException Types.NoArguments
derive instance newtypeCommitRequiredException :: Newtype CommitRequiredException _
derive instance repGenericCommitRequiredException :: Generic CommitRequiredException _
instance showCommitRequiredException :: Show CommitRequiredException where
  show = genericShow
instance decodeCommitRequiredException :: Decode CommitRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommitRequiredException :: Encode CommitRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Content = Content String
derive instance newtypeContent :: Newtype Content _
derive instance repGenericContent :: Generic Content _
instance showContent :: Show Content where
  show = genericShow
instance decodeContent :: Decode Content where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContent :: Encode Content where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a create branch operation.</p>
newtype CreateBranchInput = CreateBranchInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BranchName'" :: (BranchName)
  , "CommitId'" :: (CommitId)
  }
derive instance newtypeCreateBranchInput :: Newtype CreateBranchInput _
derive instance repGenericCreateBranchInput :: Generic CreateBranchInput _
instance showCreateBranchInput :: Show CreateBranchInput where
  show = genericShow
instance decodeCreateBranchInput :: Decode CreateBranchInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBranchInput :: Encode CreateBranchInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatePullRequestInput = CreatePullRequestInput 
  { "Title'" :: (Title)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "Targets'" :: (TargetList)
  , "ClientRequestToken'" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeCreatePullRequestInput :: Newtype CreatePullRequestInput _
derive instance repGenericCreatePullRequestInput :: Generic CreatePullRequestInput _
instance showCreatePullRequestInput :: Show CreatePullRequestInput where
  show = genericShow
instance decodeCreatePullRequestInput :: Decode CreatePullRequestInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePullRequestInput :: Encode CreatePullRequestInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatePullRequestOutput = CreatePullRequestOutput 
  { "PullRequest'" :: (PullRequest)
  }
derive instance newtypeCreatePullRequestOutput :: Newtype CreatePullRequestOutput _
derive instance repGenericCreatePullRequestOutput :: Generic CreatePullRequestOutput _
instance showCreatePullRequestOutput :: Show CreatePullRequestOutput where
  show = genericShow
instance decodeCreatePullRequestOutput :: Decode CreatePullRequestOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePullRequestOutput :: Encode CreatePullRequestOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a create repository operation.</p>
newtype CreateRepositoryInput = CreateRepositoryInput 
  { "RepositoryName'" :: (RepositoryName)
  , "RepositoryDescription'" :: NullOrUndefined.NullOrUndefined (RepositoryDescription)
  }
derive instance newtypeCreateRepositoryInput :: Newtype CreateRepositoryInput _
derive instance repGenericCreateRepositoryInput :: Generic CreateRepositoryInput _
instance showCreateRepositoryInput :: Show CreateRepositoryInput where
  show = genericShow
instance decodeCreateRepositoryInput :: Decode CreateRepositoryInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRepositoryInput :: Encode CreateRepositoryInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a create repository operation.</p>
newtype CreateRepositoryOutput = CreateRepositoryOutput 
  { "RepositoryMetadata'" :: NullOrUndefined.NullOrUndefined (RepositoryMetadata)
  }
derive instance newtypeCreateRepositoryOutput :: Newtype CreateRepositoryOutput _
derive instance repGenericCreateRepositoryOutput :: Generic CreateRepositoryOutput _
instance showCreateRepositoryOutput :: Show CreateRepositoryOutput where
  show = genericShow
instance decodeCreateRepositoryOutput :: Decode CreateRepositoryOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRepositoryOutput :: Encode CreateRepositoryOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreationDate = CreationDate Number
derive instance newtypeCreationDate :: Newtype CreationDate _
derive instance repGenericCreationDate :: Generic CreationDate _
instance showCreationDate :: Show CreationDate where
  show = genericShow
instance decodeCreationDate :: Decode CreationDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreationDate :: Encode CreationDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Date = Date String
derive instance newtypeDate :: Newtype Date _
derive instance repGenericDate :: Generic Date _
instance showDate :: Show Date where
  show = genericShow
instance decodeDate :: Decode Date where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDate :: Encode Date where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified branch is the default branch for the repository, and cannot be deleted. To delete this branch, you must first set another branch as the default branch.</p>
newtype DefaultBranchCannotBeDeletedException = DefaultBranchCannotBeDeletedException Types.NoArguments
derive instance newtypeDefaultBranchCannotBeDeletedException :: Newtype DefaultBranchCannotBeDeletedException _
derive instance repGenericDefaultBranchCannotBeDeletedException :: Generic DefaultBranchCannotBeDeletedException _
instance showDefaultBranchCannotBeDeletedException :: Show DefaultBranchCannotBeDeletedException where
  show = genericShow
instance decodeDefaultBranchCannotBeDeletedException :: Decode DefaultBranchCannotBeDeletedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultBranchCannotBeDeletedException :: Encode DefaultBranchCannotBeDeletedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a delete branch operation.</p>
newtype DeleteBranchInput = DeleteBranchInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BranchName'" :: (BranchName)
  }
derive instance newtypeDeleteBranchInput :: Newtype DeleteBranchInput _
derive instance repGenericDeleteBranchInput :: Generic DeleteBranchInput _
instance showDeleteBranchInput :: Show DeleteBranchInput where
  show = genericShow
instance decodeDeleteBranchInput :: Decode DeleteBranchInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBranchInput :: Encode DeleteBranchInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a delete branch operation.</p>
newtype DeleteBranchOutput = DeleteBranchOutput 
  { "DeletedBranch'" :: NullOrUndefined.NullOrUndefined (BranchInfo)
  }
derive instance newtypeDeleteBranchOutput :: Newtype DeleteBranchOutput _
derive instance repGenericDeleteBranchOutput :: Generic DeleteBranchOutput _
instance showDeleteBranchOutput :: Show DeleteBranchOutput where
  show = genericShow
instance decodeDeleteBranchOutput :: Decode DeleteBranchOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBranchOutput :: Encode DeleteBranchOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCommentContentInput = DeleteCommentContentInput 
  { "CommentId'" :: (CommentId)
  }
derive instance newtypeDeleteCommentContentInput :: Newtype DeleteCommentContentInput _
derive instance repGenericDeleteCommentContentInput :: Generic DeleteCommentContentInput _
instance showDeleteCommentContentInput :: Show DeleteCommentContentInput where
  show = genericShow
instance decodeDeleteCommentContentInput :: Decode DeleteCommentContentInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCommentContentInput :: Encode DeleteCommentContentInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCommentContentOutput = DeleteCommentContentOutput 
  { "Comment'" :: NullOrUndefined.NullOrUndefined (Comment)
  }
derive instance newtypeDeleteCommentContentOutput :: Newtype DeleteCommentContentOutput _
derive instance repGenericDeleteCommentContentOutput :: Generic DeleteCommentContentOutput _
instance showDeleteCommentContentOutput :: Show DeleteCommentContentOutput where
  show = genericShow
instance decodeDeleteCommentContentOutput :: Decode DeleteCommentContentOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCommentContentOutput :: Encode DeleteCommentContentOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a delete repository operation.</p>
newtype DeleteRepositoryInput = DeleteRepositoryInput 
  { "RepositoryName'" :: (RepositoryName)
  }
derive instance newtypeDeleteRepositoryInput :: Newtype DeleteRepositoryInput _
derive instance repGenericDeleteRepositoryInput :: Generic DeleteRepositoryInput _
instance showDeleteRepositoryInput :: Show DeleteRepositoryInput where
  show = genericShow
instance decodeDeleteRepositoryInput :: Decode DeleteRepositoryInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRepositoryInput :: Encode DeleteRepositoryInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a delete repository operation.</p>
newtype DeleteRepositoryOutput = DeleteRepositoryOutput 
  { "RepositoryId'" :: NullOrUndefined.NullOrUndefined (RepositoryId)
  }
derive instance newtypeDeleteRepositoryOutput :: Newtype DeleteRepositoryOutput _
derive instance repGenericDeleteRepositoryOutput :: Generic DeleteRepositoryOutput _
instance showDeleteRepositoryOutput :: Show DeleteRepositoryOutput where
  show = genericShow
instance decodeDeleteRepositoryOutput :: Decode DeleteRepositoryOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRepositoryOutput :: Encode DeleteRepositoryOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribePullRequestEventsInput = DescribePullRequestEventsInput 
  { "PullRequestId'" :: (PullRequestId)
  , "PullRequestEventType'" :: NullOrUndefined.NullOrUndefined (PullRequestEventType)
  , "ActorArn'" :: NullOrUndefined.NullOrUndefined (Arn)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeDescribePullRequestEventsInput :: Newtype DescribePullRequestEventsInput _
derive instance repGenericDescribePullRequestEventsInput :: Generic DescribePullRequestEventsInput _
instance showDescribePullRequestEventsInput :: Show DescribePullRequestEventsInput where
  show = genericShow
instance decodeDescribePullRequestEventsInput :: Decode DescribePullRequestEventsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribePullRequestEventsInput :: Encode DescribePullRequestEventsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribePullRequestEventsOutput = DescribePullRequestEventsOutput 
  { "PullRequestEvents'" :: (PullRequestEventList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribePullRequestEventsOutput :: Newtype DescribePullRequestEventsOutput _
derive instance repGenericDescribePullRequestEventsOutput :: Generic DescribePullRequestEventsOutput _
instance showDescribePullRequestEventsOutput :: Show DescribePullRequestEventsOutput where
  show = genericShow
instance decodeDescribePullRequestEventsOutput :: Decode DescribePullRequestEventsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribePullRequestEventsOutput :: Encode DescribePullRequestEventsOutput where
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


-- | <p>Returns information about a set of differences for a commit specifier.</p>
newtype Difference = Difference 
  { "BeforeBlob'" :: NullOrUndefined.NullOrUndefined (BlobMetadata)
  , "AfterBlob'" :: NullOrUndefined.NullOrUndefined (BlobMetadata)
  , "ChangeType'" :: NullOrUndefined.NullOrUndefined (ChangeTypeEnum)
  }
derive instance newtypeDifference :: Newtype Difference _
derive instance repGenericDifference :: Generic Difference _
instance showDifference :: Show Difference where
  show = genericShow
instance decodeDifference :: Decode Difference where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDifference :: Encode Difference where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DifferenceList = DifferenceList (Array Difference)
derive instance newtypeDifferenceList :: Newtype DifferenceList _
derive instance repGenericDifferenceList :: Generic DifferenceList _
instance showDifferenceList :: Show DifferenceList where
  show = genericShow
instance decodeDifferenceList :: Decode DifferenceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDifferenceList :: Encode DifferenceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A file cannot be added to the repository because the specified path name has the same name as a file that already exists in this repository. Either provide a different name for the file, or specify a different path for the file.</p>
newtype DirectoryNameConflictsWithFileNameException = DirectoryNameConflictsWithFileNameException Types.NoArguments
derive instance newtypeDirectoryNameConflictsWithFileNameException :: Newtype DirectoryNameConflictsWithFileNameException _
derive instance repGenericDirectoryNameConflictsWithFileNameException :: Generic DirectoryNameConflictsWithFileNameException _
instance showDirectoryNameConflictsWithFileNameException :: Show DirectoryNameConflictsWithFileNameException where
  show = genericShow
instance decodeDirectoryNameConflictsWithFileNameException :: Decode DirectoryNameConflictsWithFileNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryNameConflictsWithFileNameException :: Encode DirectoryNameConflictsWithFileNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _
derive instance repGenericEmail :: Generic Email _
instance showEmail :: Show Email where
  show = genericShow
instance decodeEmail :: Decode Email where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmail :: Encode Email where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An encryption integrity check failed.</p>
newtype EncryptionIntegrityChecksFailedException = EncryptionIntegrityChecksFailedException Types.NoArguments
derive instance newtypeEncryptionIntegrityChecksFailedException :: Newtype EncryptionIntegrityChecksFailedException _
derive instance repGenericEncryptionIntegrityChecksFailedException :: Generic EncryptionIntegrityChecksFailedException _
instance showEncryptionIntegrityChecksFailedException :: Show EncryptionIntegrityChecksFailedException where
  show = genericShow
instance decodeEncryptionIntegrityChecksFailedException :: Decode EncryptionIntegrityChecksFailedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionIntegrityChecksFailedException :: Encode EncryptionIntegrityChecksFailedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An encryption key could not be accessed.</p>
newtype EncryptionKeyAccessDeniedException = EncryptionKeyAccessDeniedException Types.NoArguments
derive instance newtypeEncryptionKeyAccessDeniedException :: Newtype EncryptionKeyAccessDeniedException _
derive instance repGenericEncryptionKeyAccessDeniedException :: Generic EncryptionKeyAccessDeniedException _
instance showEncryptionKeyAccessDeniedException :: Show EncryptionKeyAccessDeniedException where
  show = genericShow
instance decodeEncryptionKeyAccessDeniedException :: Decode EncryptionKeyAccessDeniedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionKeyAccessDeniedException :: Encode EncryptionKeyAccessDeniedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The encryption key is disabled.</p>
newtype EncryptionKeyDisabledException = EncryptionKeyDisabledException Types.NoArguments
derive instance newtypeEncryptionKeyDisabledException :: Newtype EncryptionKeyDisabledException _
derive instance repGenericEncryptionKeyDisabledException :: Generic EncryptionKeyDisabledException _
instance showEncryptionKeyDisabledException :: Show EncryptionKeyDisabledException where
  show = genericShow
instance decodeEncryptionKeyDisabledException :: Decode EncryptionKeyDisabledException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionKeyDisabledException :: Encode EncryptionKeyDisabledException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>No encryption key was found.</p>
newtype EncryptionKeyNotFoundException = EncryptionKeyNotFoundException Types.NoArguments
derive instance newtypeEncryptionKeyNotFoundException :: Newtype EncryptionKeyNotFoundException _
derive instance repGenericEncryptionKeyNotFoundException :: Generic EncryptionKeyNotFoundException _
instance showEncryptionKeyNotFoundException :: Show EncryptionKeyNotFoundException where
  show = genericShow
instance decodeEncryptionKeyNotFoundException :: Decode EncryptionKeyNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionKeyNotFoundException :: Encode EncryptionKeyNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The encryption key is not available.</p>
newtype EncryptionKeyUnavailableException = EncryptionKeyUnavailableException Types.NoArguments
derive instance newtypeEncryptionKeyUnavailableException :: Newtype EncryptionKeyUnavailableException _
derive instance repGenericEncryptionKeyUnavailableException :: Generic EncryptionKeyUnavailableException _
instance showEncryptionKeyUnavailableException :: Show EncryptionKeyUnavailableException where
  show = genericShow
instance decodeEncryptionKeyUnavailableException :: Decode EncryptionKeyUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionKeyUnavailableException :: Encode EncryptionKeyUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventDate = EventDate Number
derive instance newtypeEventDate :: Newtype EventDate _
derive instance repGenericEventDate :: Generic EventDate _
instance showEventDate :: Show EventDate where
  show = genericShow
instance decodeEventDate :: Decode EventDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDate :: Encode EventDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FileContent = FileContent String
derive instance newtypeFileContent :: Newtype FileContent _
derive instance repGenericFileContent :: Generic FileContent _
instance showFileContent :: Show FileContent where
  show = genericShow
instance decodeFileContent :: Decode FileContent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileContent :: Encode FileContent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The file cannot be added because it is empty. Empty files cannot be added to the repository with this API.</p>
newtype FileContentRequiredException = FileContentRequiredException Types.NoArguments
derive instance newtypeFileContentRequiredException :: Newtype FileContentRequiredException _
derive instance repGenericFileContentRequiredException :: Generic FileContentRequiredException _
instance showFileContentRequiredException :: Show FileContentRequiredException where
  show = genericShow
instance decodeFileContentRequiredException :: Decode FileContentRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileContentRequiredException :: Encode FileContentRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The file cannot be added because it is too large. The maximum file size that can be added using PutFile is 6 MB. For files larger than 6 MB but smaller than 2 GB, add them using a Git client.</p>
newtype FileContentSizeLimitExceededException = FileContentSizeLimitExceededException Types.NoArguments
derive instance newtypeFileContentSizeLimitExceededException :: Newtype FileContentSizeLimitExceededException _
derive instance repGenericFileContentSizeLimitExceededException :: Generic FileContentSizeLimitExceededException _
instance showFileContentSizeLimitExceededException :: Show FileContentSizeLimitExceededException where
  show = genericShow
instance decodeFileContentSizeLimitExceededException :: Decode FileContentSizeLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileContentSizeLimitExceededException :: Encode FileContentSizeLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FileModeTypeEnum = FileModeTypeEnum String
derive instance newtypeFileModeTypeEnum :: Newtype FileModeTypeEnum _
derive instance repGenericFileModeTypeEnum :: Generic FileModeTypeEnum _
instance showFileModeTypeEnum :: Show FileModeTypeEnum where
  show = genericShow
instance decodeFileModeTypeEnum :: Decode FileModeTypeEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileModeTypeEnum :: Encode FileModeTypeEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A file cannot be added to the repository because the specified file name has the same name as a directory in this repository. Either provide another name for the file, or add the file in a directory that does not match the file name.</p>
newtype FileNameConflictsWithDirectoryNameException = FileNameConflictsWithDirectoryNameException Types.NoArguments
derive instance newtypeFileNameConflictsWithDirectoryNameException :: Newtype FileNameConflictsWithDirectoryNameException _
derive instance repGenericFileNameConflictsWithDirectoryNameException :: Generic FileNameConflictsWithDirectoryNameException _
instance showFileNameConflictsWithDirectoryNameException :: Show FileNameConflictsWithDirectoryNameException where
  show = genericShow
instance decodeFileNameConflictsWithDirectoryNameException :: Decode FileNameConflictsWithDirectoryNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileNameConflictsWithDirectoryNameException :: Encode FileNameConflictsWithDirectoryNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified file exceeds the file size limit for AWS CodeCommit. For more information about limits in AWS CodeCommit, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/limits.html">AWS CodeCommit User Guide</a>.</p>
newtype FileTooLargeException = FileTooLargeException Types.NoArguments
derive instance newtypeFileTooLargeException :: Newtype FileTooLargeException _
derive instance repGenericFileTooLargeException :: Generic FileTooLargeException _
instance showFileTooLargeException :: Show FileTooLargeException where
  show = genericShow
instance decodeFileTooLargeException :: Decode FileTooLargeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileTooLargeException :: Encode FileTooLargeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a get blob operation.</p>
newtype GetBlobInput = GetBlobInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BlobId'" :: (ObjectId)
  }
derive instance newtypeGetBlobInput :: Newtype GetBlobInput _
derive instance repGenericGetBlobInput :: Generic GetBlobInput _
instance showGetBlobInput :: Show GetBlobInput where
  show = genericShow
instance decodeGetBlobInput :: Decode GetBlobInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBlobInput :: Encode GetBlobInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a get blob operation.</p>
newtype GetBlobOutput = GetBlobOutput 
  { "Content'" :: (String)
  }
derive instance newtypeGetBlobOutput :: Newtype GetBlobOutput _
derive instance repGenericGetBlobOutput :: Generic GetBlobOutput _
instance showGetBlobOutput :: Show GetBlobOutput where
  show = genericShow
instance decodeGetBlobOutput :: Decode GetBlobOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBlobOutput :: Encode GetBlobOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a get branch operation.</p>
newtype GetBranchInput = GetBranchInput 
  { "RepositoryName'" :: NullOrUndefined.NullOrUndefined (RepositoryName)
  , "BranchName'" :: NullOrUndefined.NullOrUndefined (BranchName)
  }
derive instance newtypeGetBranchInput :: Newtype GetBranchInput _
derive instance repGenericGetBranchInput :: Generic GetBranchInput _
instance showGetBranchInput :: Show GetBranchInput where
  show = genericShow
instance decodeGetBranchInput :: Decode GetBranchInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBranchInput :: Encode GetBranchInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a get branch operation.</p>
newtype GetBranchOutput = GetBranchOutput 
  { "Branch'" :: NullOrUndefined.NullOrUndefined (BranchInfo)
  }
derive instance newtypeGetBranchOutput :: Newtype GetBranchOutput _
derive instance repGenericGetBranchOutput :: Generic GetBranchOutput _
instance showGetBranchOutput :: Show GetBranchOutput where
  show = genericShow
instance decodeGetBranchOutput :: Decode GetBranchOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBranchOutput :: Encode GetBranchOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCommentInput = GetCommentInput 
  { "CommentId'" :: (CommentId)
  }
derive instance newtypeGetCommentInput :: Newtype GetCommentInput _
derive instance repGenericGetCommentInput :: Generic GetCommentInput _
instance showGetCommentInput :: Show GetCommentInput where
  show = genericShow
instance decodeGetCommentInput :: Decode GetCommentInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCommentInput :: Encode GetCommentInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCommentOutput = GetCommentOutput 
  { "Comment'" :: NullOrUndefined.NullOrUndefined (Comment)
  }
derive instance newtypeGetCommentOutput :: Newtype GetCommentOutput _
derive instance repGenericGetCommentOutput :: Generic GetCommentOutput _
instance showGetCommentOutput :: Show GetCommentOutput where
  show = genericShow
instance decodeGetCommentOutput :: Decode GetCommentOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCommentOutput :: Encode GetCommentOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCommentsForComparedCommitInput = GetCommentsForComparedCommitInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "AfterCommitId'" :: (CommitId)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeGetCommentsForComparedCommitInput :: Newtype GetCommentsForComparedCommitInput _
derive instance repGenericGetCommentsForComparedCommitInput :: Generic GetCommentsForComparedCommitInput _
instance showGetCommentsForComparedCommitInput :: Show GetCommentsForComparedCommitInput where
  show = genericShow
instance decodeGetCommentsForComparedCommitInput :: Decode GetCommentsForComparedCommitInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCommentsForComparedCommitInput :: Encode GetCommentsForComparedCommitInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCommentsForComparedCommitOutput = GetCommentsForComparedCommitOutput 
  { "CommentsForComparedCommitData'" :: NullOrUndefined.NullOrUndefined (CommentsForComparedCommitData)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetCommentsForComparedCommitOutput :: Newtype GetCommentsForComparedCommitOutput _
derive instance repGenericGetCommentsForComparedCommitOutput :: Generic GetCommentsForComparedCommitOutput _
instance showGetCommentsForComparedCommitOutput :: Show GetCommentsForComparedCommitOutput where
  show = genericShow
instance decodeGetCommentsForComparedCommitOutput :: Decode GetCommentsForComparedCommitOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCommentsForComparedCommitOutput :: Encode GetCommentsForComparedCommitOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCommentsForPullRequestInput = GetCommentsForPullRequestInput 
  { "PullRequestId'" :: (PullRequestId)
  , "RepositoryName'" :: NullOrUndefined.NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeGetCommentsForPullRequestInput :: Newtype GetCommentsForPullRequestInput _
derive instance repGenericGetCommentsForPullRequestInput :: Generic GetCommentsForPullRequestInput _
instance showGetCommentsForPullRequestInput :: Show GetCommentsForPullRequestInput where
  show = genericShow
instance decodeGetCommentsForPullRequestInput :: Decode GetCommentsForPullRequestInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCommentsForPullRequestInput :: Encode GetCommentsForPullRequestInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCommentsForPullRequestOutput = GetCommentsForPullRequestOutput 
  { "CommentsForPullRequestData'" :: NullOrUndefined.NullOrUndefined (CommentsForPullRequestData)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetCommentsForPullRequestOutput :: Newtype GetCommentsForPullRequestOutput _
derive instance repGenericGetCommentsForPullRequestOutput :: Generic GetCommentsForPullRequestOutput _
instance showGetCommentsForPullRequestOutput :: Show GetCommentsForPullRequestOutput where
  show = genericShow
instance decodeGetCommentsForPullRequestOutput :: Decode GetCommentsForPullRequestOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCommentsForPullRequestOutput :: Encode GetCommentsForPullRequestOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a get commit operation.</p>
newtype GetCommitInput = GetCommitInput 
  { "RepositoryName'" :: (RepositoryName)
  , "CommitId'" :: (ObjectId)
  }
derive instance newtypeGetCommitInput :: Newtype GetCommitInput _
derive instance repGenericGetCommitInput :: Generic GetCommitInput _
instance showGetCommitInput :: Show GetCommitInput where
  show = genericShow
instance decodeGetCommitInput :: Decode GetCommitInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCommitInput :: Encode GetCommitInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a get commit operation.</p>
newtype GetCommitOutput = GetCommitOutput 
  { "Commit'" :: (Commit)
  }
derive instance newtypeGetCommitOutput :: Newtype GetCommitOutput _
derive instance repGenericGetCommitOutput :: Generic GetCommitOutput _
instance showGetCommitOutput :: Show GetCommitOutput where
  show = genericShow
instance decodeGetCommitOutput :: Decode GetCommitOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCommitOutput :: Encode GetCommitOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDifferencesInput = GetDifferencesInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BeforeCommitSpecifier'" :: NullOrUndefined.NullOrUndefined (CommitName)
  , "AfterCommitSpecifier'" :: (CommitName)
  , "BeforePath'" :: NullOrUndefined.NullOrUndefined (Path)
  , "AfterPath'" :: NullOrUndefined.NullOrUndefined (Path)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (Limit)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetDifferencesInput :: Newtype GetDifferencesInput _
derive instance repGenericGetDifferencesInput :: Generic GetDifferencesInput _
instance showGetDifferencesInput :: Show GetDifferencesInput where
  show = genericShow
instance decodeGetDifferencesInput :: Decode GetDifferencesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDifferencesInput :: Encode GetDifferencesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDifferencesOutput = GetDifferencesOutput 
  { "Differences'" :: NullOrUndefined.NullOrUndefined (DifferenceList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetDifferencesOutput :: Newtype GetDifferencesOutput _
derive instance repGenericGetDifferencesOutput :: Generic GetDifferencesOutput _
instance showGetDifferencesOutput :: Show GetDifferencesOutput where
  show = genericShow
instance decodeGetDifferencesOutput :: Decode GetDifferencesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDifferencesOutput :: Encode GetDifferencesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMergeConflictsInput = GetMergeConflictsInput 
  { "RepositoryName'" :: (RepositoryName)
  , "DestinationCommitSpecifier'" :: (CommitName)
  , "SourceCommitSpecifier'" :: (CommitName)
  , "MergeOption'" :: (MergeOptionTypeEnum)
  }
derive instance newtypeGetMergeConflictsInput :: Newtype GetMergeConflictsInput _
derive instance repGenericGetMergeConflictsInput :: Generic GetMergeConflictsInput _
instance showGetMergeConflictsInput :: Show GetMergeConflictsInput where
  show = genericShow
instance decodeGetMergeConflictsInput :: Decode GetMergeConflictsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMergeConflictsInput :: Encode GetMergeConflictsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMergeConflictsOutput = GetMergeConflictsOutput 
  { "Mergeable'" :: (IsMergeable)
  , "DestinationCommitId'" :: (CommitId)
  , "SourceCommitId'" :: (CommitId)
  }
derive instance newtypeGetMergeConflictsOutput :: Newtype GetMergeConflictsOutput _
derive instance repGenericGetMergeConflictsOutput :: Generic GetMergeConflictsOutput _
instance showGetMergeConflictsOutput :: Show GetMergeConflictsOutput where
  show = genericShow
instance decodeGetMergeConflictsOutput :: Decode GetMergeConflictsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMergeConflictsOutput :: Encode GetMergeConflictsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPullRequestInput = GetPullRequestInput 
  { "PullRequestId'" :: (PullRequestId)
  }
derive instance newtypeGetPullRequestInput :: Newtype GetPullRequestInput _
derive instance repGenericGetPullRequestInput :: Generic GetPullRequestInput _
instance showGetPullRequestInput :: Show GetPullRequestInput where
  show = genericShow
instance decodeGetPullRequestInput :: Decode GetPullRequestInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPullRequestInput :: Encode GetPullRequestInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPullRequestOutput = GetPullRequestOutput 
  { "PullRequest'" :: (PullRequest)
  }
derive instance newtypeGetPullRequestOutput :: Newtype GetPullRequestOutput _
derive instance repGenericGetPullRequestOutput :: Generic GetPullRequestOutput _
instance showGetPullRequestOutput :: Show GetPullRequestOutput where
  show = genericShow
instance decodeGetPullRequestOutput :: Decode GetPullRequestOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPullRequestOutput :: Encode GetPullRequestOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a get repository operation.</p>
newtype GetRepositoryInput = GetRepositoryInput 
  { "RepositoryName'" :: (RepositoryName)
  }
derive instance newtypeGetRepositoryInput :: Newtype GetRepositoryInput _
derive instance repGenericGetRepositoryInput :: Generic GetRepositoryInput _
instance showGetRepositoryInput :: Show GetRepositoryInput where
  show = genericShow
instance decodeGetRepositoryInput :: Decode GetRepositoryInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRepositoryInput :: Encode GetRepositoryInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a get repository operation.</p>
newtype GetRepositoryOutput = GetRepositoryOutput 
  { "RepositoryMetadata'" :: NullOrUndefined.NullOrUndefined (RepositoryMetadata)
  }
derive instance newtypeGetRepositoryOutput :: Newtype GetRepositoryOutput _
derive instance repGenericGetRepositoryOutput :: Generic GetRepositoryOutput _
instance showGetRepositoryOutput :: Show GetRepositoryOutput where
  show = genericShow
instance decodeGetRepositoryOutput :: Decode GetRepositoryOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRepositoryOutput :: Encode GetRepositoryOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a get repository triggers operation.</p>
newtype GetRepositoryTriggersInput = GetRepositoryTriggersInput 
  { "RepositoryName'" :: (RepositoryName)
  }
derive instance newtypeGetRepositoryTriggersInput :: Newtype GetRepositoryTriggersInput _
derive instance repGenericGetRepositoryTriggersInput :: Generic GetRepositoryTriggersInput _
instance showGetRepositoryTriggersInput :: Show GetRepositoryTriggersInput where
  show = genericShow
instance decodeGetRepositoryTriggersInput :: Decode GetRepositoryTriggersInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRepositoryTriggersInput :: Encode GetRepositoryTriggersInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a get repository triggers operation.</p>
newtype GetRepositoryTriggersOutput = GetRepositoryTriggersOutput 
  { "ConfigurationId'" :: NullOrUndefined.NullOrUndefined (RepositoryTriggersConfigurationId)
  , "Triggers'" :: NullOrUndefined.NullOrUndefined (RepositoryTriggersList)
  }
derive instance newtypeGetRepositoryTriggersOutput :: Newtype GetRepositoryTriggersOutput _
derive instance repGenericGetRepositoryTriggersOutput :: Generic GetRepositoryTriggersOutput _
instance showGetRepositoryTriggersOutput :: Show GetRepositoryTriggersOutput where
  show = genericShow
instance decodeGetRepositoryTriggersOutput :: Decode GetRepositoryTriggersOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRepositoryTriggersOutput :: Encode GetRepositoryTriggersOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The client request token is not valid. Either the token is not in a valid format, or the token has been used in a previous request and cannot be re-used.</p>
newtype IdempotencyParameterMismatchException = IdempotencyParameterMismatchException Types.NoArguments
derive instance newtypeIdempotencyParameterMismatchException :: Newtype IdempotencyParameterMismatchException _
derive instance repGenericIdempotencyParameterMismatchException :: Generic IdempotencyParameterMismatchException _
instance showIdempotencyParameterMismatchException :: Show IdempotencyParameterMismatchException where
  show = genericShow
instance decodeIdempotencyParameterMismatchException :: Decode IdempotencyParameterMismatchException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdempotencyParameterMismatchException :: Encode IdempotencyParameterMismatchException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon Resource Name (ARN) is not valid. Make sure that you have provided the full ARN for the user who initiated the change for the pull request, and then try again.</p>
newtype InvalidActorArnException = InvalidActorArnException Types.NoArguments
derive instance newtypeInvalidActorArnException :: Newtype InvalidActorArnException _
derive instance repGenericInvalidActorArnException :: Generic InvalidActorArnException _
instance showInvalidActorArnException :: Show InvalidActorArnException where
  show = genericShow
instance decodeInvalidActorArnException :: Decode InvalidActorArnException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidActorArnException :: Encode InvalidActorArnException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon Resource Name (ARN) is not valid. Make sure that you have provided the full ARN for the author of the pull request, and then try again.</p>
newtype InvalidAuthorArnException = InvalidAuthorArnException Types.NoArguments
derive instance newtypeInvalidAuthorArnException :: Newtype InvalidAuthorArnException _
derive instance repGenericInvalidAuthorArnException :: Generic InvalidAuthorArnException _
instance showInvalidAuthorArnException :: Show InvalidAuthorArnException where
  show = genericShow
instance decodeInvalidAuthorArnException :: Decode InvalidAuthorArnException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidAuthorArnException :: Encode InvalidAuthorArnException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified blob is not valid.</p>
newtype InvalidBlobIdException = InvalidBlobIdException Types.NoArguments
derive instance newtypeInvalidBlobIdException :: Newtype InvalidBlobIdException _
derive instance repGenericInvalidBlobIdException :: Generic InvalidBlobIdException _
instance showInvalidBlobIdException :: Show InvalidBlobIdException where
  show = genericShow
instance decodeInvalidBlobIdException :: Decode InvalidBlobIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidBlobIdException :: Encode InvalidBlobIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified reference name is not valid.</p>
newtype InvalidBranchNameException = InvalidBranchNameException Types.NoArguments
derive instance newtypeInvalidBranchNameException :: Newtype InvalidBranchNameException _
derive instance repGenericInvalidBranchNameException :: Generic InvalidBranchNameException _
instance showInvalidBranchNameException :: Show InvalidBranchNameException where
  show = genericShow
instance decodeInvalidBranchNameException :: Decode InvalidBranchNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidBranchNameException :: Encode InvalidBranchNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The client request token is not valid.</p>
newtype InvalidClientRequestTokenException = InvalidClientRequestTokenException Types.NoArguments
derive instance newtypeInvalidClientRequestTokenException :: Newtype InvalidClientRequestTokenException _
derive instance repGenericInvalidClientRequestTokenException :: Generic InvalidClientRequestTokenException _
instance showInvalidClientRequestTokenException :: Show InvalidClientRequestTokenException where
  show = genericShow
instance decodeInvalidClientRequestTokenException :: Decode InvalidClientRequestTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidClientRequestTokenException :: Encode InvalidClientRequestTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The comment ID is not in a valid format. Make sure that you have provided the full comment ID.</p>
newtype InvalidCommentIdException = InvalidCommentIdException Types.NoArguments
derive instance newtypeInvalidCommentIdException :: Newtype InvalidCommentIdException _
derive instance repGenericInvalidCommentIdException :: Generic InvalidCommentIdException _
instance showInvalidCommentIdException :: Show InvalidCommentIdException where
  show = genericShow
instance decodeInvalidCommentIdException :: Decode InvalidCommentIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCommentIdException :: Encode InvalidCommentIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified commit is not valid.</p>
newtype InvalidCommitException = InvalidCommitException Types.NoArguments
derive instance newtypeInvalidCommitException :: Newtype InvalidCommitException _
derive instance repGenericInvalidCommitException :: Generic InvalidCommitException _
instance showInvalidCommitException :: Show InvalidCommitException where
  show = genericShow
instance decodeInvalidCommitException :: Decode InvalidCommitException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCommitException :: Encode InvalidCommitException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified commit ID is not valid.</p>
newtype InvalidCommitIdException = InvalidCommitIdException Types.NoArguments
derive instance newtypeInvalidCommitIdException :: Newtype InvalidCommitIdException _
derive instance repGenericInvalidCommitIdException :: Generic InvalidCommitIdException _
instance showInvalidCommitIdException :: Show InvalidCommitIdException where
  show = genericShow
instance decodeInvalidCommitIdException :: Decode InvalidCommitIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCommitIdException :: Encode InvalidCommitIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified continuation token is not valid.</p>
newtype InvalidContinuationTokenException = InvalidContinuationTokenException Types.NoArguments
derive instance newtypeInvalidContinuationTokenException :: Newtype InvalidContinuationTokenException _
derive instance repGenericInvalidContinuationTokenException :: Generic InvalidContinuationTokenException _
instance showInvalidContinuationTokenException :: Show InvalidContinuationTokenException where
  show = genericShow
instance decodeInvalidContinuationTokenException :: Decode InvalidContinuationTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidContinuationTokenException :: Encode InvalidContinuationTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pull request description is not valid. Descriptions are limited to 1,000 characters in length.</p>
newtype InvalidDescriptionException = InvalidDescriptionException Types.NoArguments
derive instance newtypeInvalidDescriptionException :: Newtype InvalidDescriptionException _
derive instance repGenericInvalidDescriptionException :: Generic InvalidDescriptionException _
instance showInvalidDescriptionException :: Show InvalidDescriptionException where
  show = genericShow
instance decodeInvalidDescriptionException :: Decode InvalidDescriptionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDescriptionException :: Encode InvalidDescriptionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The destination commit specifier is not valid. You must provide a valid branch name, tag, or full commit ID. </p>
newtype InvalidDestinationCommitSpecifierException = InvalidDestinationCommitSpecifierException Types.NoArguments
derive instance newtypeInvalidDestinationCommitSpecifierException :: Newtype InvalidDestinationCommitSpecifierException _
derive instance repGenericInvalidDestinationCommitSpecifierException :: Generic InvalidDestinationCommitSpecifierException _
instance showInvalidDestinationCommitSpecifierException :: Show InvalidDestinationCommitSpecifierException where
  show = genericShow
instance decodeInvalidDestinationCommitSpecifierException :: Decode InvalidDestinationCommitSpecifierException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDestinationCommitSpecifierException :: Encode InvalidDestinationCommitSpecifierException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified email address either contains one or more characters that are not allowed, or it exceeds the maximum number of characters allowed for an email address.</p>
newtype InvalidEmailException = InvalidEmailException Types.NoArguments
derive instance newtypeInvalidEmailException :: Newtype InvalidEmailException _
derive instance repGenericInvalidEmailException :: Generic InvalidEmailException _
instance showInvalidEmailException :: Show InvalidEmailException where
  show = genericShow
instance decodeInvalidEmailException :: Decode InvalidEmailException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidEmailException :: Encode InvalidEmailException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The location of the file is not valid. Make sure that you include the extension of the file as well as the file name.</p>
newtype InvalidFileLocationException = InvalidFileLocationException Types.NoArguments
derive instance newtypeInvalidFileLocationException :: Newtype InvalidFileLocationException _
derive instance repGenericInvalidFileLocationException :: Generic InvalidFileLocationException _
instance showInvalidFileLocationException :: Show InvalidFileLocationException where
  show = genericShow
instance decodeInvalidFileLocationException :: Decode InvalidFileLocationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidFileLocationException :: Encode InvalidFileLocationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified file mode permission is not valid. For a list of valid file mode permissions, see <a>PutFile</a>. </p>
newtype InvalidFileModeException = InvalidFileModeException Types.NoArguments
derive instance newtypeInvalidFileModeException :: Newtype InvalidFileModeException _
derive instance repGenericInvalidFileModeException :: Generic InvalidFileModeException _
instance showInvalidFileModeException :: Show InvalidFileModeException where
  show = genericShow
instance decodeInvalidFileModeException :: Decode InvalidFileModeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidFileModeException :: Encode InvalidFileModeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The position is not valid. Make sure that the line number exists in the version of the file you want to comment on.</p>
newtype InvalidFilePositionException = InvalidFilePositionException Types.NoArguments
derive instance newtypeInvalidFilePositionException :: Newtype InvalidFilePositionException _
derive instance repGenericInvalidFilePositionException :: Generic InvalidFilePositionException _
instance showInvalidFilePositionException :: Show InvalidFilePositionException where
  show = genericShow
instance decodeInvalidFilePositionException :: Decode InvalidFilePositionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidFilePositionException :: Encode InvalidFilePositionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified number of maximum results is not valid.</p>
newtype InvalidMaxResultsException = InvalidMaxResultsException Types.NoArguments
derive instance newtypeInvalidMaxResultsException :: Newtype InvalidMaxResultsException _
derive instance repGenericInvalidMaxResultsException :: Generic InvalidMaxResultsException _
instance showInvalidMaxResultsException :: Show InvalidMaxResultsException where
  show = genericShow
instance decodeInvalidMaxResultsException :: Decode InvalidMaxResultsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidMaxResultsException :: Encode InvalidMaxResultsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified merge option is not valid. The only valid value is FAST_FORWARD_MERGE.</p>
newtype InvalidMergeOptionException = InvalidMergeOptionException Types.NoArguments
derive instance newtypeInvalidMergeOptionException :: Newtype InvalidMergeOptionException _
derive instance repGenericInvalidMergeOptionException :: Generic InvalidMergeOptionException _
instance showInvalidMergeOptionException :: Show InvalidMergeOptionException where
  show = genericShow
instance decodeInvalidMergeOptionException :: Decode InvalidMergeOptionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidMergeOptionException :: Encode InvalidMergeOptionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified sort order is not valid.</p>
newtype InvalidOrderException = InvalidOrderException Types.NoArguments
derive instance newtypeInvalidOrderException :: Newtype InvalidOrderException _
derive instance repGenericInvalidOrderException :: Generic InvalidOrderException _
instance showInvalidOrderException :: Show InvalidOrderException where
  show = genericShow
instance decodeInvalidOrderException :: Decode InvalidOrderException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidOrderException :: Encode InvalidOrderException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parent commit ID is not valid. The commit ID cannot be empty, and must match the head commit ID for the branch of the repository where you want to add or update a file.</p>
newtype InvalidParentCommitIdException = InvalidParentCommitIdException Types.NoArguments
derive instance newtypeInvalidParentCommitIdException :: Newtype InvalidParentCommitIdException _
derive instance repGenericInvalidParentCommitIdException :: Generic InvalidParentCommitIdException _
instance showInvalidParentCommitIdException :: Show InvalidParentCommitIdException where
  show = genericShow
instance decodeInvalidParentCommitIdException :: Decode InvalidParentCommitIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParentCommitIdException :: Encode InvalidParentCommitIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified path is not valid.</p>
newtype InvalidPathException = InvalidPathException Types.NoArguments
derive instance newtypeInvalidPathException :: Newtype InvalidPathException _
derive instance repGenericInvalidPathException :: Generic InvalidPathException _
instance showInvalidPathException :: Show InvalidPathException where
  show = genericShow
instance decodeInvalidPathException :: Decode InvalidPathException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPathException :: Encode InvalidPathException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pull request event type is not valid. </p>
newtype InvalidPullRequestEventTypeException = InvalidPullRequestEventTypeException Types.NoArguments
derive instance newtypeInvalidPullRequestEventTypeException :: Newtype InvalidPullRequestEventTypeException _
derive instance repGenericInvalidPullRequestEventTypeException :: Generic InvalidPullRequestEventTypeException _
instance showInvalidPullRequestEventTypeException :: Show InvalidPullRequestEventTypeException where
  show = genericShow
instance decodeInvalidPullRequestEventTypeException :: Decode InvalidPullRequestEventTypeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPullRequestEventTypeException :: Encode InvalidPullRequestEventTypeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pull request ID is not valid. Make sure that you have provided the full ID and that the pull request is in the specified repository, and then try again.</p>
newtype InvalidPullRequestIdException = InvalidPullRequestIdException Types.NoArguments
derive instance newtypeInvalidPullRequestIdException :: Newtype InvalidPullRequestIdException _
derive instance repGenericInvalidPullRequestIdException :: Generic InvalidPullRequestIdException _
instance showInvalidPullRequestIdException :: Show InvalidPullRequestIdException where
  show = genericShow
instance decodeInvalidPullRequestIdException :: Decode InvalidPullRequestIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPullRequestIdException :: Encode InvalidPullRequestIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pull request status is not valid. The only valid values are <code>OPEN</code> and <code>CLOSED</code>.</p>
newtype InvalidPullRequestStatusException = InvalidPullRequestStatusException Types.NoArguments
derive instance newtypeInvalidPullRequestStatusException :: Newtype InvalidPullRequestStatusException _
derive instance repGenericInvalidPullRequestStatusException :: Generic InvalidPullRequestStatusException _
instance showInvalidPullRequestStatusException :: Show InvalidPullRequestStatusException where
  show = genericShow
instance decodeInvalidPullRequestStatusException :: Decode InvalidPullRequestStatusException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPullRequestStatusException :: Encode InvalidPullRequestStatusException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pull request status update is not valid. The only valid update is from <code>OPEN</code> to <code>CLOSED</code>.</p>
newtype InvalidPullRequestStatusUpdateException = InvalidPullRequestStatusUpdateException Types.NoArguments
derive instance newtypeInvalidPullRequestStatusUpdateException :: Newtype InvalidPullRequestStatusUpdateException _
derive instance repGenericInvalidPullRequestStatusUpdateException :: Generic InvalidPullRequestStatusUpdateException _
instance showInvalidPullRequestStatusUpdateException :: Show InvalidPullRequestStatusUpdateException where
  show = genericShow
instance decodeInvalidPullRequestStatusUpdateException :: Decode InvalidPullRequestStatusUpdateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPullRequestStatusUpdateException :: Encode InvalidPullRequestStatusUpdateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified reference name format is not valid. Reference names must conform to the Git references format, for example refs/heads/master. For more information, see <a href="https://git-scm.com/book/en/v2/Git-Internals-Git-References">Git Internals - Git References</a> or consult your Git documentation.</p>
newtype InvalidReferenceNameException = InvalidReferenceNameException Types.NoArguments
derive instance newtypeInvalidReferenceNameException :: Newtype InvalidReferenceNameException _
derive instance repGenericInvalidReferenceNameException :: Generic InvalidReferenceNameException _
instance showInvalidReferenceNameException :: Show InvalidReferenceNameException where
  show = genericShow
instance decodeInvalidReferenceNameException :: Decode InvalidReferenceNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidReferenceNameException :: Encode InvalidReferenceNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Either the enum is not in a valid format, or the specified file version enum is not valid in respect to the current file version.</p>
newtype InvalidRelativeFileVersionEnumException = InvalidRelativeFileVersionEnumException Types.NoArguments
derive instance newtypeInvalidRelativeFileVersionEnumException :: Newtype InvalidRelativeFileVersionEnumException _
derive instance repGenericInvalidRelativeFileVersionEnumException :: Generic InvalidRelativeFileVersionEnumException _
instance showInvalidRelativeFileVersionEnumException :: Show InvalidRelativeFileVersionEnumException where
  show = genericShow
instance decodeInvalidRelativeFileVersionEnumException :: Decode InvalidRelativeFileVersionEnumException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRelativeFileVersionEnumException :: Encode InvalidRelativeFileVersionEnumException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified repository description is not valid.</p>
newtype InvalidRepositoryDescriptionException = InvalidRepositoryDescriptionException Types.NoArguments
derive instance newtypeInvalidRepositoryDescriptionException :: Newtype InvalidRepositoryDescriptionException _
derive instance repGenericInvalidRepositoryDescriptionException :: Generic InvalidRepositoryDescriptionException _
instance showInvalidRepositoryDescriptionException :: Show InvalidRepositoryDescriptionException where
  show = genericShow
instance decodeInvalidRepositoryDescriptionException :: Decode InvalidRepositoryDescriptionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRepositoryDescriptionException :: Encode InvalidRepositoryDescriptionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>At least one specified repository name is not valid.</p> <note> <p>This exception only occurs when a specified repository name is not valid. Other exceptions occur when a required repository parameter is missing, or when a specified repository does not exist.</p> </note>
newtype InvalidRepositoryNameException = InvalidRepositoryNameException Types.NoArguments
derive instance newtypeInvalidRepositoryNameException :: Newtype InvalidRepositoryNameException _
derive instance repGenericInvalidRepositoryNameException :: Generic InvalidRepositoryNameException _
instance showInvalidRepositoryNameException :: Show InvalidRepositoryNameException where
  show = genericShow
instance decodeInvalidRepositoryNameException :: Decode InvalidRepositoryNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRepositoryNameException :: Encode InvalidRepositoryNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more branch names specified for the trigger is not valid.</p>
newtype InvalidRepositoryTriggerBranchNameException = InvalidRepositoryTriggerBranchNameException Types.NoArguments
derive instance newtypeInvalidRepositoryTriggerBranchNameException :: Newtype InvalidRepositoryTriggerBranchNameException _
derive instance repGenericInvalidRepositoryTriggerBranchNameException :: Generic InvalidRepositoryTriggerBranchNameException _
instance showInvalidRepositoryTriggerBranchNameException :: Show InvalidRepositoryTriggerBranchNameException where
  show = genericShow
instance decodeInvalidRepositoryTriggerBranchNameException :: Decode InvalidRepositoryTriggerBranchNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRepositoryTriggerBranchNameException :: Encode InvalidRepositoryTriggerBranchNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The custom data provided for the trigger is not valid.</p>
newtype InvalidRepositoryTriggerCustomDataException = InvalidRepositoryTriggerCustomDataException Types.NoArguments
derive instance newtypeInvalidRepositoryTriggerCustomDataException :: Newtype InvalidRepositoryTriggerCustomDataException _
derive instance repGenericInvalidRepositoryTriggerCustomDataException :: Generic InvalidRepositoryTriggerCustomDataException _
instance showInvalidRepositoryTriggerCustomDataException :: Show InvalidRepositoryTriggerCustomDataException where
  show = genericShow
instance decodeInvalidRepositoryTriggerCustomDataException :: Decode InvalidRepositoryTriggerCustomDataException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRepositoryTriggerCustomDataException :: Encode InvalidRepositoryTriggerCustomDataException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon Resource Name (ARN) for the trigger is not valid for the specified destination. The most common reason for this error is that the ARN does not meet the requirements for the service type.</p>
newtype InvalidRepositoryTriggerDestinationArnException = InvalidRepositoryTriggerDestinationArnException Types.NoArguments
derive instance newtypeInvalidRepositoryTriggerDestinationArnException :: Newtype InvalidRepositoryTriggerDestinationArnException _
derive instance repGenericInvalidRepositoryTriggerDestinationArnException :: Generic InvalidRepositoryTriggerDestinationArnException _
instance showInvalidRepositoryTriggerDestinationArnException :: Show InvalidRepositoryTriggerDestinationArnException where
  show = genericShow
instance decodeInvalidRepositoryTriggerDestinationArnException :: Decode InvalidRepositoryTriggerDestinationArnException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRepositoryTriggerDestinationArnException :: Encode InvalidRepositoryTriggerDestinationArnException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more events specified for the trigger is not valid. Check to make sure that all events specified match the requirements for allowed events.</p>
newtype InvalidRepositoryTriggerEventsException = InvalidRepositoryTriggerEventsException Types.NoArguments
derive instance newtypeInvalidRepositoryTriggerEventsException :: Newtype InvalidRepositoryTriggerEventsException _
derive instance repGenericInvalidRepositoryTriggerEventsException :: Generic InvalidRepositoryTriggerEventsException _
instance showInvalidRepositoryTriggerEventsException :: Show InvalidRepositoryTriggerEventsException where
  show = genericShow
instance decodeInvalidRepositoryTriggerEventsException :: Decode InvalidRepositoryTriggerEventsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRepositoryTriggerEventsException :: Encode InvalidRepositoryTriggerEventsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of the trigger is not valid.</p>
newtype InvalidRepositoryTriggerNameException = InvalidRepositoryTriggerNameException Types.NoArguments
derive instance newtypeInvalidRepositoryTriggerNameException :: Newtype InvalidRepositoryTriggerNameException _
derive instance repGenericInvalidRepositoryTriggerNameException :: Generic InvalidRepositoryTriggerNameException _
instance showInvalidRepositoryTriggerNameException :: Show InvalidRepositoryTriggerNameException where
  show = genericShow
instance decodeInvalidRepositoryTriggerNameException :: Decode InvalidRepositoryTriggerNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRepositoryTriggerNameException :: Encode InvalidRepositoryTriggerNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The region for the trigger target does not match the region for the repository. Triggers must be created in the same region as the target for the trigger.</p>
newtype InvalidRepositoryTriggerRegionException = InvalidRepositoryTriggerRegionException Types.NoArguments
derive instance newtypeInvalidRepositoryTriggerRegionException :: Newtype InvalidRepositoryTriggerRegionException _
derive instance repGenericInvalidRepositoryTriggerRegionException :: Generic InvalidRepositoryTriggerRegionException _
instance showInvalidRepositoryTriggerRegionException :: Show InvalidRepositoryTriggerRegionException where
  show = genericShow
instance decodeInvalidRepositoryTriggerRegionException :: Decode InvalidRepositoryTriggerRegionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRepositoryTriggerRegionException :: Encode InvalidRepositoryTriggerRegionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified sort by value is not valid.</p>
newtype InvalidSortByException = InvalidSortByException Types.NoArguments
derive instance newtypeInvalidSortByException :: Newtype InvalidSortByException _
derive instance repGenericInvalidSortByException :: Generic InvalidSortByException _
instance showInvalidSortByException :: Show InvalidSortByException where
  show = genericShow
instance decodeInvalidSortByException :: Decode InvalidSortByException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSortByException :: Encode InvalidSortByException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The source commit specifier is not valid. You must provide a valid branch name, tag, or full commit ID.</p>
newtype InvalidSourceCommitSpecifierException = InvalidSourceCommitSpecifierException Types.NoArguments
derive instance newtypeInvalidSourceCommitSpecifierException :: Newtype InvalidSourceCommitSpecifierException _
derive instance repGenericInvalidSourceCommitSpecifierException :: Generic InvalidSourceCommitSpecifierException _
instance showInvalidSourceCommitSpecifierException :: Show InvalidSourceCommitSpecifierException where
  show = genericShow
instance decodeInvalidSourceCommitSpecifierException :: Decode InvalidSourceCommitSpecifierException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSourceCommitSpecifierException :: Encode InvalidSourceCommitSpecifierException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The target for the pull request is not valid. A target must contain the full values for the repository name, source branch, and destination branch for the pull request.</p>
newtype InvalidTargetException = InvalidTargetException Types.NoArguments
derive instance newtypeInvalidTargetException :: Newtype InvalidTargetException _
derive instance repGenericInvalidTargetException :: Generic InvalidTargetException _
instance showInvalidTargetException :: Show InvalidTargetException where
  show = genericShow
instance decodeInvalidTargetException :: Decode InvalidTargetException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTargetException :: Encode InvalidTargetException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The targets for the pull request is not valid or not in a valid format. Targets are a list of target objects. Each target object must contain the full values for the repository name, source branch, and destination branch for a pull request.</p>
newtype InvalidTargetsException = InvalidTargetsException Types.NoArguments
derive instance newtypeInvalidTargetsException :: Newtype InvalidTargetsException _
derive instance repGenericInvalidTargetsException :: Generic InvalidTargetsException _
instance showInvalidTargetsException :: Show InvalidTargetsException where
  show = genericShow
instance decodeInvalidTargetsException :: Decode InvalidTargetsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTargetsException :: Encode InvalidTargetsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The title of the pull request is not valid. Pull request titles cannot exceed 100 characters in length.</p>
newtype InvalidTitleException = InvalidTitleException Types.NoArguments
derive instance newtypeInvalidTitleException :: Newtype InvalidTitleException _
derive instance repGenericInvalidTitleException :: Generic InvalidTitleException _
instance showInvalidTitleException :: Show InvalidTitleException where
  show = genericShow
instance decodeInvalidTitleException :: Decode InvalidTitleException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTitleException :: Encode InvalidTitleException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsCommentDeleted = IsCommentDeleted Boolean
derive instance newtypeIsCommentDeleted :: Newtype IsCommentDeleted _
derive instance repGenericIsCommentDeleted :: Generic IsCommentDeleted _
instance showIsCommentDeleted :: Show IsCommentDeleted where
  show = genericShow
instance decodeIsCommentDeleted :: Decode IsCommentDeleted where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsCommentDeleted :: Encode IsCommentDeleted where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsMergeable = IsMergeable Boolean
derive instance newtypeIsMergeable :: Newtype IsMergeable _
derive instance repGenericIsMergeable :: Generic IsMergeable _
instance showIsMergeable :: Show IsMergeable where
  show = genericShow
instance decodeIsMergeable :: Decode IsMergeable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsMergeable :: Encode IsMergeable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsMerged = IsMerged Boolean
derive instance newtypeIsMerged :: Newtype IsMerged _
derive instance repGenericIsMerged :: Generic IsMerged _
instance showIsMerged :: Show IsMerged where
  show = genericShow
instance decodeIsMerged :: Decode IsMerged where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsMerged :: Encode IsMerged where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastModifiedDate = LastModifiedDate Number
derive instance newtypeLastModifiedDate :: Newtype LastModifiedDate _
derive instance repGenericLastModifiedDate :: Generic LastModifiedDate _
instance showLastModifiedDate :: Show LastModifiedDate where
  show = genericShow
instance decodeLastModifiedDate :: Decode LastModifiedDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastModifiedDate :: Encode LastModifiedDate where
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


-- | <p>Represents the input of a list branches operation.</p>
newtype ListBranchesInput = ListBranchesInput 
  { "RepositoryName'" :: (RepositoryName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListBranchesInput :: Newtype ListBranchesInput _
derive instance repGenericListBranchesInput :: Generic ListBranchesInput _
instance showListBranchesInput :: Show ListBranchesInput where
  show = genericShow
instance decodeListBranchesInput :: Decode ListBranchesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBranchesInput :: Encode ListBranchesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a list branches operation.</p>
newtype ListBranchesOutput = ListBranchesOutput 
  { "Branches'" :: NullOrUndefined.NullOrUndefined (BranchNameList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListBranchesOutput :: Newtype ListBranchesOutput _
derive instance repGenericListBranchesOutput :: Generic ListBranchesOutput _
instance showListBranchesOutput :: Show ListBranchesOutput where
  show = genericShow
instance decodeListBranchesOutput :: Decode ListBranchesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBranchesOutput :: Encode ListBranchesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListPullRequestsInput = ListPullRequestsInput 
  { "RepositoryName'" :: (RepositoryName)
  , "AuthorArn'" :: NullOrUndefined.NullOrUndefined (Arn)
  , "PullRequestStatus'" :: NullOrUndefined.NullOrUndefined (PullRequestStatusEnum)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListPullRequestsInput :: Newtype ListPullRequestsInput _
derive instance repGenericListPullRequestsInput :: Generic ListPullRequestsInput _
instance showListPullRequestsInput :: Show ListPullRequestsInput where
  show = genericShow
instance decodeListPullRequestsInput :: Decode ListPullRequestsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPullRequestsInput :: Encode ListPullRequestsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListPullRequestsOutput = ListPullRequestsOutput 
  { "PullRequestIds'" :: (PullRequestIdList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListPullRequestsOutput :: Newtype ListPullRequestsOutput _
derive instance repGenericListPullRequestsOutput :: Generic ListPullRequestsOutput _
instance showListPullRequestsOutput :: Show ListPullRequestsOutput where
  show = genericShow
instance decodeListPullRequestsOutput :: Decode ListPullRequestsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPullRequestsOutput :: Encode ListPullRequestsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a list repositories operation.</p>
newtype ListRepositoriesInput = ListRepositoriesInput 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "SortBy'" :: NullOrUndefined.NullOrUndefined (SortByEnum)
  , "Order'" :: NullOrUndefined.NullOrUndefined (OrderEnum)
  }
derive instance newtypeListRepositoriesInput :: Newtype ListRepositoriesInput _
derive instance repGenericListRepositoriesInput :: Generic ListRepositoriesInput _
instance showListRepositoriesInput :: Show ListRepositoriesInput where
  show = genericShow
instance decodeListRepositoriesInput :: Decode ListRepositoriesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRepositoriesInput :: Encode ListRepositoriesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a list repositories operation.</p>
newtype ListRepositoriesOutput = ListRepositoriesOutput 
  { "Repositories'" :: NullOrUndefined.NullOrUndefined (RepositoryNameIdPairList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListRepositoriesOutput :: Newtype ListRepositoriesOutput _
derive instance repGenericListRepositoriesOutput :: Generic ListRepositoriesOutput _
instance showListRepositoriesOutput :: Show ListRepositoriesOutput where
  show = genericShow
instance decodeListRepositoriesOutput :: Decode ListRepositoriesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRepositoriesOutput :: Encode ListRepositoriesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about the location of a change or comment in the comparison between two commits or a pull request.</p>
newtype Location = Location 
  { "FilePath'" :: NullOrUndefined.NullOrUndefined (Path)
  , "FilePosition'" :: NullOrUndefined.NullOrUndefined (Position)
  , "RelativeFileVersion'" :: NullOrUndefined.NullOrUndefined (RelativeFileVersionEnum)
  }
derive instance newtypeLocation :: Newtype Location _
derive instance repGenericLocation :: Generic Location _
instance showLocation :: Show Location where
  show = genericShow
instance decodeLocation :: Decode Location where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocation :: Encode Location where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pull request cannot be merged automatically into the destination branch. You must manually merge the branches and resolve any conflicts.</p>
newtype ManualMergeRequiredException = ManualMergeRequiredException Types.NoArguments
derive instance newtypeManualMergeRequiredException :: Newtype ManualMergeRequiredException _
derive instance repGenericManualMergeRequiredException :: Generic ManualMergeRequiredException _
instance showManualMergeRequiredException :: Show ManualMergeRequiredException where
  show = genericShow
instance decodeManualMergeRequiredException :: Decode ManualMergeRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeManualMergeRequiredException :: Encode ManualMergeRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of branches for the trigger was exceeded.</p>
newtype MaximumBranchesExceededException = MaximumBranchesExceededException Types.NoArguments
derive instance newtypeMaximumBranchesExceededException :: Newtype MaximumBranchesExceededException _
derive instance repGenericMaximumBranchesExceededException :: Generic MaximumBranchesExceededException _
instance showMaximumBranchesExceededException :: Show MaximumBranchesExceededException where
  show = genericShow
instance decodeMaximumBranchesExceededException :: Decode MaximumBranchesExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaximumBranchesExceededException :: Encode MaximumBranchesExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot create the pull request because the repository has too many open pull requests. The maximum number of open pull requests for a repository is 1,000. Close one or more open pull requests, and then try again.</p>
newtype MaximumOpenPullRequestsExceededException = MaximumOpenPullRequestsExceededException Types.NoArguments
derive instance newtypeMaximumOpenPullRequestsExceededException :: Newtype MaximumOpenPullRequestsExceededException _
derive instance repGenericMaximumOpenPullRequestsExceededException :: Generic MaximumOpenPullRequestsExceededException _
instance showMaximumOpenPullRequestsExceededException :: Show MaximumOpenPullRequestsExceededException where
  show = genericShow
instance decodeMaximumOpenPullRequestsExceededException :: Decode MaximumOpenPullRequestsExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaximumOpenPullRequestsExceededException :: Encode MaximumOpenPullRequestsExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum number of allowed repository names was exceeded. Currently, this number is 25.</p>
newtype MaximumRepositoryNamesExceededException = MaximumRepositoryNamesExceededException Types.NoArguments
derive instance newtypeMaximumRepositoryNamesExceededException :: Newtype MaximumRepositoryNamesExceededException _
derive instance repGenericMaximumRepositoryNamesExceededException :: Generic MaximumRepositoryNamesExceededException _
instance showMaximumRepositoryNamesExceededException :: Show MaximumRepositoryNamesExceededException where
  show = genericShow
instance decodeMaximumRepositoryNamesExceededException :: Decode MaximumRepositoryNamesExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaximumRepositoryNamesExceededException :: Encode MaximumRepositoryNamesExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of triggers allowed for the repository was exceeded.</p>
newtype MaximumRepositoryTriggersExceededException = MaximumRepositoryTriggersExceededException Types.NoArguments
derive instance newtypeMaximumRepositoryTriggersExceededException :: Newtype MaximumRepositoryTriggersExceededException _
derive instance repGenericMaximumRepositoryTriggersExceededException :: Generic MaximumRepositoryTriggersExceededException _
instance showMaximumRepositoryTriggersExceededException :: Show MaximumRepositoryTriggersExceededException where
  show = genericShow
instance decodeMaximumRepositoryTriggersExceededException :: Decode MaximumRepositoryTriggersExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaximumRepositoryTriggersExceededException :: Encode MaximumRepositoryTriggersExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about a merge or potential merge between a source reference and a destination reference in a pull request.</p>
newtype MergeMetadata = MergeMetadata 
  { "IsMerged'" :: NullOrUndefined.NullOrUndefined (IsMerged)
  , "MergedBy'" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeMergeMetadata :: Newtype MergeMetadata _
derive instance repGenericMergeMetadata :: Generic MergeMetadata _
instance showMergeMetadata :: Show MergeMetadata where
  show = genericShow
instance decodeMergeMetadata :: Decode MergeMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMergeMetadata :: Encode MergeMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A merge option or stategy is required, and none was provided.</p>
newtype MergeOptionRequiredException = MergeOptionRequiredException Types.NoArguments
derive instance newtypeMergeOptionRequiredException :: Newtype MergeOptionRequiredException _
derive instance repGenericMergeOptionRequiredException :: Generic MergeOptionRequiredException _
instance showMergeOptionRequiredException :: Show MergeOptionRequiredException where
  show = genericShow
instance decodeMergeOptionRequiredException :: Decode MergeOptionRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMergeOptionRequiredException :: Encode MergeOptionRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MergeOptionTypeEnum = MergeOptionTypeEnum String
derive instance newtypeMergeOptionTypeEnum :: Newtype MergeOptionTypeEnum _
derive instance repGenericMergeOptionTypeEnum :: Generic MergeOptionTypeEnum _
instance showMergeOptionTypeEnum :: Show MergeOptionTypeEnum where
  show = genericShow
instance decodeMergeOptionTypeEnum :: Decode MergeOptionTypeEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMergeOptionTypeEnum :: Encode MergeOptionTypeEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MergePullRequestByFastForwardInput = MergePullRequestByFastForwardInput 
  { "PullRequestId'" :: (PullRequestId)
  , "RepositoryName'" :: (RepositoryName)
  , "SourceCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  }
derive instance newtypeMergePullRequestByFastForwardInput :: Newtype MergePullRequestByFastForwardInput _
derive instance repGenericMergePullRequestByFastForwardInput :: Generic MergePullRequestByFastForwardInput _
instance showMergePullRequestByFastForwardInput :: Show MergePullRequestByFastForwardInput where
  show = genericShow
instance decodeMergePullRequestByFastForwardInput :: Decode MergePullRequestByFastForwardInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMergePullRequestByFastForwardInput :: Encode MergePullRequestByFastForwardInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MergePullRequestByFastForwardOutput = MergePullRequestByFastForwardOutput 
  { "PullRequest'" :: NullOrUndefined.NullOrUndefined (PullRequest)
  }
derive instance newtypeMergePullRequestByFastForwardOutput :: Newtype MergePullRequestByFastForwardOutput _
derive instance repGenericMergePullRequestByFastForwardOutput :: Generic MergePullRequestByFastForwardOutput _
instance showMergePullRequestByFastForwardOutput :: Show MergePullRequestByFastForwardOutput where
  show = genericShow
instance decodeMergePullRequestByFastForwardOutput :: Decode MergePullRequestByFastForwardOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMergePullRequestByFastForwardOutput :: Encode MergePullRequestByFastForwardOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Mode = Mode String
derive instance newtypeMode :: Newtype Mode _
derive instance repGenericMode :: Generic Mode _
instance showMode :: Show Mode where
  show = genericShow
instance decodeMode :: Decode Mode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMode :: Encode Mode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot include more than one repository in a pull request. Make sure you have specified only one repository name in your request, and then try again.</p>
newtype MultipleRepositoriesInPullRequestException = MultipleRepositoriesInPullRequestException Types.NoArguments
derive instance newtypeMultipleRepositoriesInPullRequestException :: Newtype MultipleRepositoriesInPullRequestException _
derive instance repGenericMultipleRepositoriesInPullRequestException :: Generic MultipleRepositoriesInPullRequestException _
instance showMultipleRepositoriesInPullRequestException :: Show MultipleRepositoriesInPullRequestException where
  show = genericShow
instance decodeMultipleRepositoriesInPullRequestException :: Decode MultipleRepositoriesInPullRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMultipleRepositoriesInPullRequestException :: Encode MultipleRepositoriesInPullRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive instance repGenericName :: Generic Name _
instance showName :: Show Name where
  show = genericShow
instance decodeName :: Decode Name where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeName :: Encode Name where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The file name is not valid because it has exceeded the character limit for file names. File names, including the path to the file, cannot exceed the character limit. </p>
newtype NameLengthExceededException = NameLengthExceededException Types.NoArguments
derive instance newtypeNameLengthExceededException :: Newtype NameLengthExceededException _
derive instance repGenericNameLengthExceededException :: Generic NameLengthExceededException _
instance showNameLengthExceededException :: Show NameLengthExceededException where
  show = genericShow
instance decodeNameLengthExceededException :: Decode NameLengthExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNameLengthExceededException :: Encode NameLengthExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectId = ObjectId String
derive instance newtypeObjectId :: Newtype ObjectId _
derive instance repGenericObjectId :: Generic ObjectId _
instance showObjectId :: Show ObjectId where
  show = genericShow
instance decodeObjectId :: Decode ObjectId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectId :: Encode ObjectId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrderEnum = OrderEnum String
derive instance newtypeOrderEnum :: Newtype OrderEnum _
derive instance repGenericOrderEnum :: Generic OrderEnum _
instance showOrderEnum :: Show OrderEnum where
  show = genericShow
instance decodeOrderEnum :: Decode OrderEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderEnum :: Encode OrderEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parent commit ID is not valid. The specified parent commit ID does not exist in the specified branch of the repository.</p>
newtype ParentCommitDoesNotExistException = ParentCommitDoesNotExistException Types.NoArguments
derive instance newtypeParentCommitDoesNotExistException :: Newtype ParentCommitDoesNotExistException _
derive instance repGenericParentCommitDoesNotExistException :: Generic ParentCommitDoesNotExistException _
instance showParentCommitDoesNotExistException :: Show ParentCommitDoesNotExistException where
  show = genericShow
instance decodeParentCommitDoesNotExistException :: Decode ParentCommitDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParentCommitDoesNotExistException :: Encode ParentCommitDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The file could not be added because the provided parent commit ID is not the current tip of the specified branch. To view the full commit ID of the current head of the branch, use <a>GetBranch</a>.</p>
newtype ParentCommitIdOutdatedException = ParentCommitIdOutdatedException Types.NoArguments
derive instance newtypeParentCommitIdOutdatedException :: Newtype ParentCommitIdOutdatedException _
derive instance repGenericParentCommitIdOutdatedException :: Generic ParentCommitIdOutdatedException _
instance showParentCommitIdOutdatedException :: Show ParentCommitIdOutdatedException where
  show = genericShow
instance decodeParentCommitIdOutdatedException :: Decode ParentCommitIdOutdatedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParentCommitIdOutdatedException :: Encode ParentCommitIdOutdatedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A parent commit ID is required. To view the full commit ID of a branch in a repository, use <a>GetBranch</a> or a Git command (for example, git pull or git log).</p>
newtype ParentCommitIdRequiredException = ParentCommitIdRequiredException Types.NoArguments
derive instance newtypeParentCommitIdRequiredException :: Newtype ParentCommitIdRequiredException _
derive instance repGenericParentCommitIdRequiredException :: Generic ParentCommitIdRequiredException _
instance showParentCommitIdRequiredException :: Show ParentCommitIdRequiredException where
  show = genericShow
instance decodeParentCommitIdRequiredException :: Decode ParentCommitIdRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParentCommitIdRequiredException :: Encode ParentCommitIdRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParentList = ParentList (Array ObjectId)
derive instance newtypeParentList :: Newtype ParentList _
derive instance repGenericParentList :: Generic ParentList _
instance showParentList :: Show ParentList where
  show = genericShow
instance decodeParentList :: Decode ParentList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParentList :: Encode ParentList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Path = Path String
derive instance newtypePath :: Newtype Path _
derive instance repGenericPath :: Generic Path _
instance showPath :: Show Path where
  show = genericShow
instance decodePath :: Decode Path where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePath :: Encode Path where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified path does not exist.</p>
newtype PathDoesNotExistException = PathDoesNotExistException Types.NoArguments
derive instance newtypePathDoesNotExistException :: Newtype PathDoesNotExistException _
derive instance repGenericPathDoesNotExistException :: Generic PathDoesNotExistException _
instance showPathDoesNotExistException :: Show PathDoesNotExistException where
  show = genericShow
instance decodePathDoesNotExistException :: Decode PathDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePathDoesNotExistException :: Encode PathDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The filePath for a location cannot be empty or null.</p>
newtype PathRequiredException = PathRequiredException Types.NoArguments
derive instance newtypePathRequiredException :: Newtype PathRequiredException _
derive instance repGenericPathRequiredException :: Generic PathRequiredException _
instance showPathRequiredException :: Show PathRequiredException where
  show = genericShow
instance decodePathRequiredException :: Decode PathRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePathRequiredException :: Encode PathRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Position = Position Number
derive instance newtypePosition :: Newtype Position _
derive instance repGenericPosition :: Generic Position _
instance showPosition :: Show Position where
  show = genericShow
instance decodePosition :: Decode Position where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePosition :: Encode Position where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PostCommentForComparedCommitInput = PostCommentForComparedCommitInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "AfterCommitId'" :: (CommitId)
  , "Location'" :: NullOrUndefined.NullOrUndefined (Location)
  , "Content'" :: (Content)
  , "ClientRequestToken'" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypePostCommentForComparedCommitInput :: Newtype PostCommentForComparedCommitInput _
derive instance repGenericPostCommentForComparedCommitInput :: Generic PostCommentForComparedCommitInput _
instance showPostCommentForComparedCommitInput :: Show PostCommentForComparedCommitInput where
  show = genericShow
instance decodePostCommentForComparedCommitInput :: Decode PostCommentForComparedCommitInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePostCommentForComparedCommitInput :: Encode PostCommentForComparedCommitInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PostCommentForComparedCommitOutput = PostCommentForComparedCommitOutput 
  { "RepositoryName'" :: NullOrUndefined.NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "BeforeBlobId'" :: NullOrUndefined.NullOrUndefined (ObjectId)
  , "AfterBlobId'" :: NullOrUndefined.NullOrUndefined (ObjectId)
  , "Location'" :: NullOrUndefined.NullOrUndefined (Location)
  , "Comment'" :: NullOrUndefined.NullOrUndefined (Comment)
  }
derive instance newtypePostCommentForComparedCommitOutput :: Newtype PostCommentForComparedCommitOutput _
derive instance repGenericPostCommentForComparedCommitOutput :: Generic PostCommentForComparedCommitOutput _
instance showPostCommentForComparedCommitOutput :: Show PostCommentForComparedCommitOutput where
  show = genericShow
instance decodePostCommentForComparedCommitOutput :: Decode PostCommentForComparedCommitOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePostCommentForComparedCommitOutput :: Encode PostCommentForComparedCommitOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PostCommentForPullRequestInput = PostCommentForPullRequestInput 
  { "PullRequestId'" :: (PullRequestId)
  , "RepositoryName'" :: (RepositoryName)
  , "BeforeCommitId'" :: (CommitId)
  , "AfterCommitId'" :: (CommitId)
  , "Location'" :: NullOrUndefined.NullOrUndefined (Location)
  , "Content'" :: (Content)
  , "ClientRequestToken'" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypePostCommentForPullRequestInput :: Newtype PostCommentForPullRequestInput _
derive instance repGenericPostCommentForPullRequestInput :: Generic PostCommentForPullRequestInput _
instance showPostCommentForPullRequestInput :: Show PostCommentForPullRequestInput where
  show = genericShow
instance decodePostCommentForPullRequestInput :: Decode PostCommentForPullRequestInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePostCommentForPullRequestInput :: Encode PostCommentForPullRequestInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PostCommentForPullRequestOutput = PostCommentForPullRequestOutput 
  { "RepositoryName'" :: NullOrUndefined.NullOrUndefined (RepositoryName)
  , "PullRequestId'" :: NullOrUndefined.NullOrUndefined (PullRequestId)
  , "BeforeCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "BeforeBlobId'" :: NullOrUndefined.NullOrUndefined (ObjectId)
  , "AfterBlobId'" :: NullOrUndefined.NullOrUndefined (ObjectId)
  , "Location'" :: NullOrUndefined.NullOrUndefined (Location)
  , "Comment'" :: NullOrUndefined.NullOrUndefined (Comment)
  }
derive instance newtypePostCommentForPullRequestOutput :: Newtype PostCommentForPullRequestOutput _
derive instance repGenericPostCommentForPullRequestOutput :: Generic PostCommentForPullRequestOutput _
instance showPostCommentForPullRequestOutput :: Show PostCommentForPullRequestOutput where
  show = genericShow
instance decodePostCommentForPullRequestOutput :: Decode PostCommentForPullRequestOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePostCommentForPullRequestOutput :: Encode PostCommentForPullRequestOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PostCommentReplyInput = PostCommentReplyInput 
  { "InReplyTo'" :: (CommentId)
  , "ClientRequestToken'" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  , "Content'" :: (Content)
  }
derive instance newtypePostCommentReplyInput :: Newtype PostCommentReplyInput _
derive instance repGenericPostCommentReplyInput :: Generic PostCommentReplyInput _
instance showPostCommentReplyInput :: Show PostCommentReplyInput where
  show = genericShow
instance decodePostCommentReplyInput :: Decode PostCommentReplyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePostCommentReplyInput :: Encode PostCommentReplyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PostCommentReplyOutput = PostCommentReplyOutput 
  { "Comment'" :: NullOrUndefined.NullOrUndefined (Comment)
  }
derive instance newtypePostCommentReplyOutput :: Newtype PostCommentReplyOutput _
derive instance repGenericPostCommentReplyOutput :: Generic PostCommentReplyOutput _
instance showPostCommentReplyOutput :: Show PostCommentReplyOutput where
  show = genericShow
instance decodePostCommentReplyOutput :: Decode PostCommentReplyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePostCommentReplyOutput :: Encode PostCommentReplyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about a pull request.</p>
newtype PullRequest = PullRequest 
  { "PullRequestId'" :: NullOrUndefined.NullOrUndefined (PullRequestId)
  , "Title'" :: NullOrUndefined.NullOrUndefined (Title)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "LastActivityDate'" :: NullOrUndefined.NullOrUndefined (LastModifiedDate)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (CreationDate)
  , "PullRequestStatus'" :: NullOrUndefined.NullOrUndefined (PullRequestStatusEnum)
  , "AuthorArn'" :: NullOrUndefined.NullOrUndefined (Arn)
  , "PullRequestTargets'" :: NullOrUndefined.NullOrUndefined (PullRequestTargetList)
  , "ClientRequestToken'" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypePullRequest :: Newtype PullRequest _
derive instance repGenericPullRequest :: Generic PullRequest _
instance showPullRequest :: Show PullRequest where
  show = genericShow
instance decodePullRequest :: Decode PullRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequest :: Encode PullRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pull request status cannot be updated because it is already closed.</p>
newtype PullRequestAlreadyClosedException = PullRequestAlreadyClosedException Types.NoArguments
derive instance newtypePullRequestAlreadyClosedException :: Newtype PullRequestAlreadyClosedException _
derive instance repGenericPullRequestAlreadyClosedException :: Generic PullRequestAlreadyClosedException _
instance showPullRequestAlreadyClosedException :: Show PullRequestAlreadyClosedException where
  show = genericShow
instance decodePullRequestAlreadyClosedException :: Decode PullRequestAlreadyClosedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestAlreadyClosedException :: Encode PullRequestAlreadyClosedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pull request ID could not be found. Make sure that you have specified the correct repository name and pull request ID, and then try again.</p>
newtype PullRequestDoesNotExistException = PullRequestDoesNotExistException Types.NoArguments
derive instance newtypePullRequestDoesNotExistException :: Newtype PullRequestDoesNotExistException _
derive instance repGenericPullRequestDoesNotExistException :: Generic PullRequestDoesNotExistException _
instance showPullRequestDoesNotExistException :: Show PullRequestDoesNotExistException where
  show = genericShow
instance decodePullRequestDoesNotExistException :: Decode PullRequestDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestDoesNotExistException :: Encode PullRequestDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about a pull request event.</p>
newtype PullRequestEvent = PullRequestEvent 
  { "PullRequestId'" :: NullOrUndefined.NullOrUndefined (PullRequestId)
  , "EventDate'" :: NullOrUndefined.NullOrUndefined (EventDate)
  , "PullRequestEventType'" :: NullOrUndefined.NullOrUndefined (PullRequestEventType)
  , "ActorArn'" :: NullOrUndefined.NullOrUndefined (Arn)
  , "PullRequestStatusChangedEventMetadata'" :: NullOrUndefined.NullOrUndefined (PullRequestStatusChangedEventMetadata)
  , "PullRequestSourceReferenceUpdatedEventMetadata'" :: NullOrUndefined.NullOrUndefined (PullRequestSourceReferenceUpdatedEventMetadata)
  , "PullRequestMergedStateChangedEventMetadata'" :: NullOrUndefined.NullOrUndefined (PullRequestMergedStateChangedEventMetadata)
  }
derive instance newtypePullRequestEvent :: Newtype PullRequestEvent _
derive instance repGenericPullRequestEvent :: Generic PullRequestEvent _
instance showPullRequestEvent :: Show PullRequestEvent where
  show = genericShow
instance decodePullRequestEvent :: Decode PullRequestEvent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestEvent :: Encode PullRequestEvent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PullRequestEventList = PullRequestEventList (Array PullRequestEvent)
derive instance newtypePullRequestEventList :: Newtype PullRequestEventList _
derive instance repGenericPullRequestEventList :: Generic PullRequestEventList _
instance showPullRequestEventList :: Show PullRequestEventList where
  show = genericShow
instance decodePullRequestEventList :: Decode PullRequestEventList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestEventList :: Encode PullRequestEventList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PullRequestEventType = PullRequestEventType String
derive instance newtypePullRequestEventType :: Newtype PullRequestEventType _
derive instance repGenericPullRequestEventType :: Generic PullRequestEventType _
instance showPullRequestEventType :: Show PullRequestEventType where
  show = genericShow
instance decodePullRequestEventType :: Decode PullRequestEventType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestEventType :: Encode PullRequestEventType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PullRequestId = PullRequestId String
derive instance newtypePullRequestId :: Newtype PullRequestId _
derive instance repGenericPullRequestId :: Generic PullRequestId _
instance showPullRequestId :: Show PullRequestId where
  show = genericShow
instance decodePullRequestId :: Decode PullRequestId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestId :: Encode PullRequestId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PullRequestIdList = PullRequestIdList (Array PullRequestId)
derive instance newtypePullRequestIdList :: Newtype PullRequestIdList _
derive instance repGenericPullRequestIdList :: Generic PullRequestIdList _
instance showPullRequestIdList :: Show PullRequestIdList where
  show = genericShow
instance decodePullRequestIdList :: Decode PullRequestIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestIdList :: Encode PullRequestIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A pull request ID is required, but none was provided.</p>
newtype PullRequestIdRequiredException = PullRequestIdRequiredException Types.NoArguments
derive instance newtypePullRequestIdRequiredException :: Newtype PullRequestIdRequiredException _
derive instance repGenericPullRequestIdRequiredException :: Generic PullRequestIdRequiredException _
instance showPullRequestIdRequiredException :: Show PullRequestIdRequiredException where
  show = genericShow
instance decodePullRequestIdRequiredException :: Decode PullRequestIdRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestIdRequiredException :: Encode PullRequestIdRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about the change in the merge state for a pull request event. </p>
newtype PullRequestMergedStateChangedEventMetadata = PullRequestMergedStateChangedEventMetadata 
  { "RepositoryName'" :: NullOrUndefined.NullOrUndefined (RepositoryName)
  , "DestinationReference'" :: NullOrUndefined.NullOrUndefined (ReferenceName)
  , "MergeMetadata'" :: NullOrUndefined.NullOrUndefined (MergeMetadata)
  }
derive instance newtypePullRequestMergedStateChangedEventMetadata :: Newtype PullRequestMergedStateChangedEventMetadata _
derive instance repGenericPullRequestMergedStateChangedEventMetadata :: Generic PullRequestMergedStateChangedEventMetadata _
instance showPullRequestMergedStateChangedEventMetadata :: Show PullRequestMergedStateChangedEventMetadata where
  show = genericShow
instance decodePullRequestMergedStateChangedEventMetadata :: Decode PullRequestMergedStateChangedEventMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestMergedStateChangedEventMetadata :: Encode PullRequestMergedStateChangedEventMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about an update to the source branch of a pull request.</p>
newtype PullRequestSourceReferenceUpdatedEventMetadata = PullRequestSourceReferenceUpdatedEventMetadata 
  { "RepositoryName'" :: NullOrUndefined.NullOrUndefined (RepositoryName)
  , "BeforeCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "AfterCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  }
derive instance newtypePullRequestSourceReferenceUpdatedEventMetadata :: Newtype PullRequestSourceReferenceUpdatedEventMetadata _
derive instance repGenericPullRequestSourceReferenceUpdatedEventMetadata :: Generic PullRequestSourceReferenceUpdatedEventMetadata _
instance showPullRequestSourceReferenceUpdatedEventMetadata :: Show PullRequestSourceReferenceUpdatedEventMetadata where
  show = genericShow
instance decodePullRequestSourceReferenceUpdatedEventMetadata :: Decode PullRequestSourceReferenceUpdatedEventMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestSourceReferenceUpdatedEventMetadata :: Encode PullRequestSourceReferenceUpdatedEventMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a change to the status of a pull request.</p>
newtype PullRequestStatusChangedEventMetadata = PullRequestStatusChangedEventMetadata 
  { "PullRequestStatus'" :: NullOrUndefined.NullOrUndefined (PullRequestStatusEnum)
  }
derive instance newtypePullRequestStatusChangedEventMetadata :: Newtype PullRequestStatusChangedEventMetadata _
derive instance repGenericPullRequestStatusChangedEventMetadata :: Generic PullRequestStatusChangedEventMetadata _
instance showPullRequestStatusChangedEventMetadata :: Show PullRequestStatusChangedEventMetadata where
  show = genericShow
instance decodePullRequestStatusChangedEventMetadata :: Decode PullRequestStatusChangedEventMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestStatusChangedEventMetadata :: Encode PullRequestStatusChangedEventMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PullRequestStatusEnum = PullRequestStatusEnum String
derive instance newtypePullRequestStatusEnum :: Newtype PullRequestStatusEnum _
derive instance repGenericPullRequestStatusEnum :: Generic PullRequestStatusEnum _
instance showPullRequestStatusEnum :: Show PullRequestStatusEnum where
  show = genericShow
instance decodePullRequestStatusEnum :: Decode PullRequestStatusEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestStatusEnum :: Encode PullRequestStatusEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A pull request status is required, but none was provided.</p>
newtype PullRequestStatusRequiredException = PullRequestStatusRequiredException Types.NoArguments
derive instance newtypePullRequestStatusRequiredException :: Newtype PullRequestStatusRequiredException _
derive instance repGenericPullRequestStatusRequiredException :: Generic PullRequestStatusRequiredException _
instance showPullRequestStatusRequiredException :: Show PullRequestStatusRequiredException where
  show = genericShow
instance decodePullRequestStatusRequiredException :: Decode PullRequestStatusRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestStatusRequiredException :: Encode PullRequestStatusRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about a pull request target.</p>
newtype PullRequestTarget = PullRequestTarget 
  { "RepositoryName'" :: NullOrUndefined.NullOrUndefined (RepositoryName)
  , "SourceReference'" :: NullOrUndefined.NullOrUndefined (ReferenceName)
  , "DestinationReference'" :: NullOrUndefined.NullOrUndefined (ReferenceName)
  , "DestinationCommit'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "SourceCommit'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "MergeMetadata'" :: NullOrUndefined.NullOrUndefined (MergeMetadata)
  }
derive instance newtypePullRequestTarget :: Newtype PullRequestTarget _
derive instance repGenericPullRequestTarget :: Generic PullRequestTarget _
instance showPullRequestTarget :: Show PullRequestTarget where
  show = genericShow
instance decodePullRequestTarget :: Decode PullRequestTarget where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestTarget :: Encode PullRequestTarget where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PullRequestTargetList = PullRequestTargetList (Array PullRequestTarget)
derive instance newtypePullRequestTargetList :: Newtype PullRequestTargetList _
derive instance repGenericPullRequestTargetList :: Generic PullRequestTargetList _
instance showPullRequestTargetList :: Show PullRequestTargetList where
  show = genericShow
instance decodePullRequestTargetList :: Decode PullRequestTargetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePullRequestTargetList :: Encode PullRequestTargetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutFileInput = PutFileInput 
  { "RepositoryName'" :: (RepositoryName)
  , "BranchName'" :: (BranchName)
  , "FileContent'" :: (FileContent)
  , "FilePath'" :: (Path)
  , "FileMode'" :: NullOrUndefined.NullOrUndefined (FileModeTypeEnum)
  , "ParentCommitId'" :: NullOrUndefined.NullOrUndefined (CommitId)
  , "CommitMessage'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Email'" :: NullOrUndefined.NullOrUndefined (Email)
  }
derive instance newtypePutFileInput :: Newtype PutFileInput _
derive instance repGenericPutFileInput :: Generic PutFileInput _
instance showPutFileInput :: Show PutFileInput where
  show = genericShow
instance decodePutFileInput :: Decode PutFileInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutFileInput :: Encode PutFileInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutFileOutput = PutFileOutput 
  { "CommitId'" :: (ObjectId)
  , "BlobId'" :: (ObjectId)
  , "TreeId'" :: (ObjectId)
  }
derive instance newtypePutFileOutput :: Newtype PutFileOutput _
derive instance repGenericPutFileOutput :: Generic PutFileOutput _
instance showPutFileOutput :: Show PutFileOutput where
  show = genericShow
instance decodePutFileOutput :: Decode PutFileOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutFileOutput :: Encode PutFileOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input ofa put repository triggers operation.</p>
newtype PutRepositoryTriggersInput = PutRepositoryTriggersInput 
  { "RepositoryName'" :: (RepositoryName)
  , "Triggers'" :: (RepositoryTriggersList)
  }
derive instance newtypePutRepositoryTriggersInput :: Newtype PutRepositoryTriggersInput _
derive instance repGenericPutRepositoryTriggersInput :: Generic PutRepositoryTriggersInput _
instance showPutRepositoryTriggersInput :: Show PutRepositoryTriggersInput where
  show = genericShow
instance decodePutRepositoryTriggersInput :: Decode PutRepositoryTriggersInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRepositoryTriggersInput :: Encode PutRepositoryTriggersInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a put repository triggers operation.</p>
newtype PutRepositoryTriggersOutput = PutRepositoryTriggersOutput 
  { "ConfigurationId'" :: NullOrUndefined.NullOrUndefined (RepositoryTriggersConfigurationId)
  }
derive instance newtypePutRepositoryTriggersOutput :: Newtype PutRepositoryTriggersOutput _
derive instance repGenericPutRepositoryTriggersOutput :: Generic PutRepositoryTriggersOutput _
instance showPutRepositoryTriggersOutput :: Show PutRepositoryTriggersOutput where
  show = genericShow
instance decodePutRepositoryTriggersOutput :: Decode PutRepositoryTriggersOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRepositoryTriggersOutput :: Encode PutRepositoryTriggersOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified reference does not exist. You must provide a full commit ID.</p>
newtype ReferenceDoesNotExistException = ReferenceDoesNotExistException Types.NoArguments
derive instance newtypeReferenceDoesNotExistException :: Newtype ReferenceDoesNotExistException _
derive instance repGenericReferenceDoesNotExistException :: Generic ReferenceDoesNotExistException _
instance showReferenceDoesNotExistException :: Show ReferenceDoesNotExistException where
  show = genericShow
instance decodeReferenceDoesNotExistException :: Decode ReferenceDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReferenceDoesNotExistException :: Encode ReferenceDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReferenceName = ReferenceName String
derive instance newtypeReferenceName :: Newtype ReferenceName _
derive instance repGenericReferenceName :: Generic ReferenceName _
instance showReferenceName :: Show ReferenceName where
  show = genericShow
instance decodeReferenceName :: Decode ReferenceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReferenceName :: Encode ReferenceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A reference name is required, but none was provided.</p>
newtype ReferenceNameRequiredException = ReferenceNameRequiredException Types.NoArguments
derive instance newtypeReferenceNameRequiredException :: Newtype ReferenceNameRequiredException _
derive instance repGenericReferenceNameRequiredException :: Generic ReferenceNameRequiredException _
instance showReferenceNameRequiredException :: Show ReferenceNameRequiredException where
  show = genericShow
instance decodeReferenceNameRequiredException :: Decode ReferenceNameRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReferenceNameRequiredException :: Encode ReferenceNameRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified reference is not a supported type. </p>
newtype ReferenceTypeNotSupportedException = ReferenceTypeNotSupportedException Types.NoArguments
derive instance newtypeReferenceTypeNotSupportedException :: Newtype ReferenceTypeNotSupportedException _
derive instance repGenericReferenceTypeNotSupportedException :: Generic ReferenceTypeNotSupportedException _
instance showReferenceTypeNotSupportedException :: Show ReferenceTypeNotSupportedException where
  show = genericShow
instance decodeReferenceTypeNotSupportedException :: Decode ReferenceTypeNotSupportedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReferenceTypeNotSupportedException :: Encode ReferenceTypeNotSupportedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RelativeFileVersionEnum = RelativeFileVersionEnum String
derive instance newtypeRelativeFileVersionEnum :: Newtype RelativeFileVersionEnum _
derive instance repGenericRelativeFileVersionEnum :: Generic RelativeFileVersionEnum _
instance showRelativeFileVersionEnum :: Show RelativeFileVersionEnum where
  show = genericShow
instance decodeRelativeFileVersionEnum :: Decode RelativeFileVersionEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRelativeFileVersionEnum :: Encode RelativeFileVersionEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryDescription = RepositoryDescription String
derive instance newtypeRepositoryDescription :: Newtype RepositoryDescription _
derive instance repGenericRepositoryDescription :: Generic RepositoryDescription _
instance showRepositoryDescription :: Show RepositoryDescription where
  show = genericShow
instance decodeRepositoryDescription :: Decode RepositoryDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryDescription :: Encode RepositoryDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified repository does not exist.</p>
newtype RepositoryDoesNotExistException = RepositoryDoesNotExistException Types.NoArguments
derive instance newtypeRepositoryDoesNotExistException :: Newtype RepositoryDoesNotExistException _
derive instance repGenericRepositoryDoesNotExistException :: Generic RepositoryDoesNotExistException _
instance showRepositoryDoesNotExistException :: Show RepositoryDoesNotExistException where
  show = genericShow
instance decodeRepositoryDoesNotExistException :: Decode RepositoryDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryDoesNotExistException :: Encode RepositoryDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryId = RepositoryId String
derive instance newtypeRepositoryId :: Newtype RepositoryId _
derive instance repGenericRepositoryId :: Generic RepositoryId _
instance showRepositoryId :: Show RepositoryId where
  show = genericShow
instance decodeRepositoryId :: Decode RepositoryId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryId :: Encode RepositoryId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A repository resource limit was exceeded.</p>
newtype RepositoryLimitExceededException = RepositoryLimitExceededException Types.NoArguments
derive instance newtypeRepositoryLimitExceededException :: Newtype RepositoryLimitExceededException _
derive instance repGenericRepositoryLimitExceededException :: Generic RepositoryLimitExceededException _
instance showRepositoryLimitExceededException :: Show RepositoryLimitExceededException where
  show = genericShow
instance decodeRepositoryLimitExceededException :: Decode RepositoryLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryLimitExceededException :: Encode RepositoryLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a repository.</p>
newtype RepositoryMetadata = RepositoryMetadata 
  { "AccountId'" :: NullOrUndefined.NullOrUndefined (AccountId)
  , "RepositoryId'" :: NullOrUndefined.NullOrUndefined (RepositoryId)
  , "RepositoryName'" :: NullOrUndefined.NullOrUndefined (RepositoryName)
  , "RepositoryDescription'" :: NullOrUndefined.NullOrUndefined (RepositoryDescription)
  , "DefaultBranch'" :: NullOrUndefined.NullOrUndefined (BranchName)
  , "LastModifiedDate'" :: NullOrUndefined.NullOrUndefined (LastModifiedDate)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (CreationDate)
  , "CloneUrlHttp'" :: NullOrUndefined.NullOrUndefined (CloneUrlHttp)
  , "CloneUrlSsh'" :: NullOrUndefined.NullOrUndefined (CloneUrlSsh)
  , "Arn" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeRepositoryMetadata :: Newtype RepositoryMetadata _
derive instance repGenericRepositoryMetadata :: Generic RepositoryMetadata _
instance showRepositoryMetadata :: Show RepositoryMetadata where
  show = genericShow
instance decodeRepositoryMetadata :: Decode RepositoryMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryMetadata :: Encode RepositoryMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryMetadataList = RepositoryMetadataList (Array RepositoryMetadata)
derive instance newtypeRepositoryMetadataList :: Newtype RepositoryMetadataList _
derive instance repGenericRepositoryMetadataList :: Generic RepositoryMetadataList _
instance showRepositoryMetadataList :: Show RepositoryMetadataList where
  show = genericShow
instance decodeRepositoryMetadataList :: Decode RepositoryMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryMetadataList :: Encode RepositoryMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryName = RepositoryName String
derive instance newtypeRepositoryName :: Newtype RepositoryName _
derive instance repGenericRepositoryName :: Generic RepositoryName _
instance showRepositoryName :: Show RepositoryName where
  show = genericShow
instance decodeRepositoryName :: Decode RepositoryName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryName :: Encode RepositoryName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified repository name already exists.</p>
newtype RepositoryNameExistsException = RepositoryNameExistsException Types.NoArguments
derive instance newtypeRepositoryNameExistsException :: Newtype RepositoryNameExistsException _
derive instance repGenericRepositoryNameExistsException :: Generic RepositoryNameExistsException _
instance showRepositoryNameExistsException :: Show RepositoryNameExistsException where
  show = genericShow
instance decodeRepositoryNameExistsException :: Decode RepositoryNameExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryNameExistsException :: Encode RepositoryNameExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a repository name and ID.</p>
newtype RepositoryNameIdPair = RepositoryNameIdPair 
  { "RepositoryName'" :: NullOrUndefined.NullOrUndefined (RepositoryName)
  , "RepositoryId'" :: NullOrUndefined.NullOrUndefined (RepositoryId)
  }
derive instance newtypeRepositoryNameIdPair :: Newtype RepositoryNameIdPair _
derive instance repGenericRepositoryNameIdPair :: Generic RepositoryNameIdPair _
instance showRepositoryNameIdPair :: Show RepositoryNameIdPair where
  show = genericShow
instance decodeRepositoryNameIdPair :: Decode RepositoryNameIdPair where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryNameIdPair :: Encode RepositoryNameIdPair where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryNameIdPairList = RepositoryNameIdPairList (Array RepositoryNameIdPair)
derive instance newtypeRepositoryNameIdPairList :: Newtype RepositoryNameIdPairList _
derive instance repGenericRepositoryNameIdPairList :: Generic RepositoryNameIdPairList _
instance showRepositoryNameIdPairList :: Show RepositoryNameIdPairList where
  show = genericShow
instance decodeRepositoryNameIdPairList :: Decode RepositoryNameIdPairList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryNameIdPairList :: Encode RepositoryNameIdPairList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryNameList = RepositoryNameList (Array RepositoryName)
derive instance newtypeRepositoryNameList :: Newtype RepositoryNameList _
derive instance repGenericRepositoryNameList :: Generic RepositoryNameList _
instance showRepositoryNameList :: Show RepositoryNameList where
  show = genericShow
instance decodeRepositoryNameList :: Decode RepositoryNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryNameList :: Encode RepositoryNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A repository name is required but was not specified.</p>
newtype RepositoryNameRequiredException = RepositoryNameRequiredException Types.NoArguments
derive instance newtypeRepositoryNameRequiredException :: Newtype RepositoryNameRequiredException _
derive instance repGenericRepositoryNameRequiredException :: Generic RepositoryNameRequiredException _
instance showRepositoryNameRequiredException :: Show RepositoryNameRequiredException where
  show = genericShow
instance decodeRepositoryNameRequiredException :: Decode RepositoryNameRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryNameRequiredException :: Encode RepositoryNameRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A repository names object is required but was not specified.</p>
newtype RepositoryNamesRequiredException = RepositoryNamesRequiredException Types.NoArguments
derive instance newtypeRepositoryNamesRequiredException :: Newtype RepositoryNamesRequiredException _
derive instance repGenericRepositoryNamesRequiredException :: Generic RepositoryNamesRequiredException _
instance showRepositoryNamesRequiredException :: Show RepositoryNamesRequiredException where
  show = genericShow
instance decodeRepositoryNamesRequiredException :: Decode RepositoryNamesRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryNamesRequiredException :: Encode RepositoryNamesRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The repository does not contain any pull requests with that pull request ID. Check to make sure you have provided the correct repository name for the pull request.</p>
newtype RepositoryNotAssociatedWithPullRequestException = RepositoryNotAssociatedWithPullRequestException Types.NoArguments
derive instance newtypeRepositoryNotAssociatedWithPullRequestException :: Newtype RepositoryNotAssociatedWithPullRequestException _
derive instance repGenericRepositoryNotAssociatedWithPullRequestException :: Generic RepositoryNotAssociatedWithPullRequestException _
instance showRepositoryNotAssociatedWithPullRequestException :: Show RepositoryNotAssociatedWithPullRequestException where
  show = genericShow
instance decodeRepositoryNotAssociatedWithPullRequestException :: Decode RepositoryNotAssociatedWithPullRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryNotAssociatedWithPullRequestException :: Encode RepositoryNotAssociatedWithPullRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryNotFoundList = RepositoryNotFoundList (Array RepositoryName)
derive instance newtypeRepositoryNotFoundList :: Newtype RepositoryNotFoundList _
derive instance repGenericRepositoryNotFoundList :: Generic RepositoryNotFoundList _
instance showRepositoryNotFoundList :: Show RepositoryNotFoundList where
  show = genericShow
instance decodeRepositoryNotFoundList :: Decode RepositoryNotFoundList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryNotFoundList :: Encode RepositoryNotFoundList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a trigger for a repository.</p>
newtype RepositoryTrigger = RepositoryTrigger 
  { "Name'" :: (RepositoryTriggerName)
  , "DestinationArn'" :: (Arn)
  , "CustomData'" :: NullOrUndefined.NullOrUndefined (RepositoryTriggerCustomData)
  , "Branches'" :: NullOrUndefined.NullOrUndefined (BranchNameList)
  , "Events'" :: (RepositoryTriggerEventList)
  }
derive instance newtypeRepositoryTrigger :: Newtype RepositoryTrigger _
derive instance repGenericRepositoryTrigger :: Generic RepositoryTrigger _
instance showRepositoryTrigger :: Show RepositoryTrigger where
  show = genericShow
instance decodeRepositoryTrigger :: Decode RepositoryTrigger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTrigger :: Encode RepositoryTrigger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>At least one branch name is required but was not specified in the trigger configuration.</p>
newtype RepositoryTriggerBranchNameListRequiredException = RepositoryTriggerBranchNameListRequiredException Types.NoArguments
derive instance newtypeRepositoryTriggerBranchNameListRequiredException :: Newtype RepositoryTriggerBranchNameListRequiredException _
derive instance repGenericRepositoryTriggerBranchNameListRequiredException :: Generic RepositoryTriggerBranchNameListRequiredException _
instance showRepositoryTriggerBranchNameListRequiredException :: Show RepositoryTriggerBranchNameListRequiredException where
  show = genericShow
instance decodeRepositoryTriggerBranchNameListRequiredException :: Decode RepositoryTriggerBranchNameListRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggerBranchNameListRequiredException :: Encode RepositoryTriggerBranchNameListRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryTriggerCustomData = RepositoryTriggerCustomData String
derive instance newtypeRepositoryTriggerCustomData :: Newtype RepositoryTriggerCustomData _
derive instance repGenericRepositoryTriggerCustomData :: Generic RepositoryTriggerCustomData _
instance showRepositoryTriggerCustomData :: Show RepositoryTriggerCustomData where
  show = genericShow
instance decodeRepositoryTriggerCustomData :: Decode RepositoryTriggerCustomData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggerCustomData :: Encode RepositoryTriggerCustomData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A destination ARN for the target service for the trigger is required but was not specified.</p>
newtype RepositoryTriggerDestinationArnRequiredException = RepositoryTriggerDestinationArnRequiredException Types.NoArguments
derive instance newtypeRepositoryTriggerDestinationArnRequiredException :: Newtype RepositoryTriggerDestinationArnRequiredException _
derive instance repGenericRepositoryTriggerDestinationArnRequiredException :: Generic RepositoryTriggerDestinationArnRequiredException _
instance showRepositoryTriggerDestinationArnRequiredException :: Show RepositoryTriggerDestinationArnRequiredException where
  show = genericShow
instance decodeRepositoryTriggerDestinationArnRequiredException :: Decode RepositoryTriggerDestinationArnRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggerDestinationArnRequiredException :: Encode RepositoryTriggerDestinationArnRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryTriggerEventEnum = RepositoryTriggerEventEnum String
derive instance newtypeRepositoryTriggerEventEnum :: Newtype RepositoryTriggerEventEnum _
derive instance repGenericRepositoryTriggerEventEnum :: Generic RepositoryTriggerEventEnum _
instance showRepositoryTriggerEventEnum :: Show RepositoryTriggerEventEnum where
  show = genericShow
instance decodeRepositoryTriggerEventEnum :: Decode RepositoryTriggerEventEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggerEventEnum :: Encode RepositoryTriggerEventEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryTriggerEventList = RepositoryTriggerEventList (Array RepositoryTriggerEventEnum)
derive instance newtypeRepositoryTriggerEventList :: Newtype RepositoryTriggerEventList _
derive instance repGenericRepositoryTriggerEventList :: Generic RepositoryTriggerEventList _
instance showRepositoryTriggerEventList :: Show RepositoryTriggerEventList where
  show = genericShow
instance decodeRepositoryTriggerEventList :: Decode RepositoryTriggerEventList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggerEventList :: Encode RepositoryTriggerEventList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>At least one event for the trigger is required but was not specified.</p>
newtype RepositoryTriggerEventsListRequiredException = RepositoryTriggerEventsListRequiredException Types.NoArguments
derive instance newtypeRepositoryTriggerEventsListRequiredException :: Newtype RepositoryTriggerEventsListRequiredException _
derive instance repGenericRepositoryTriggerEventsListRequiredException :: Generic RepositoryTriggerEventsListRequiredException _
instance showRepositoryTriggerEventsListRequiredException :: Show RepositoryTriggerEventsListRequiredException where
  show = genericShow
instance decodeRepositoryTriggerEventsListRequiredException :: Decode RepositoryTriggerEventsListRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggerEventsListRequiredException :: Encode RepositoryTriggerEventsListRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A trigger failed to run.</p>
newtype RepositoryTriggerExecutionFailure = RepositoryTriggerExecutionFailure 
  { "Trigger'" :: NullOrUndefined.NullOrUndefined (RepositoryTriggerName)
  , "FailureMessage'" :: NullOrUndefined.NullOrUndefined (RepositoryTriggerExecutionFailureMessage)
  }
derive instance newtypeRepositoryTriggerExecutionFailure :: Newtype RepositoryTriggerExecutionFailure _
derive instance repGenericRepositoryTriggerExecutionFailure :: Generic RepositoryTriggerExecutionFailure _
instance showRepositoryTriggerExecutionFailure :: Show RepositoryTriggerExecutionFailure where
  show = genericShow
instance decodeRepositoryTriggerExecutionFailure :: Decode RepositoryTriggerExecutionFailure where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggerExecutionFailure :: Encode RepositoryTriggerExecutionFailure where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryTriggerExecutionFailureList = RepositoryTriggerExecutionFailureList (Array RepositoryTriggerExecutionFailure)
derive instance newtypeRepositoryTriggerExecutionFailureList :: Newtype RepositoryTriggerExecutionFailureList _
derive instance repGenericRepositoryTriggerExecutionFailureList :: Generic RepositoryTriggerExecutionFailureList _
instance showRepositoryTriggerExecutionFailureList :: Show RepositoryTriggerExecutionFailureList where
  show = genericShow
instance decodeRepositoryTriggerExecutionFailureList :: Decode RepositoryTriggerExecutionFailureList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggerExecutionFailureList :: Encode RepositoryTriggerExecutionFailureList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryTriggerExecutionFailureMessage = RepositoryTriggerExecutionFailureMessage String
derive instance newtypeRepositoryTriggerExecutionFailureMessage :: Newtype RepositoryTriggerExecutionFailureMessage _
derive instance repGenericRepositoryTriggerExecutionFailureMessage :: Generic RepositoryTriggerExecutionFailureMessage _
instance showRepositoryTriggerExecutionFailureMessage :: Show RepositoryTriggerExecutionFailureMessage where
  show = genericShow
instance decodeRepositoryTriggerExecutionFailureMessage :: Decode RepositoryTriggerExecutionFailureMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggerExecutionFailureMessage :: Encode RepositoryTriggerExecutionFailureMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryTriggerName = RepositoryTriggerName String
derive instance newtypeRepositoryTriggerName :: Newtype RepositoryTriggerName _
derive instance repGenericRepositoryTriggerName :: Generic RepositoryTriggerName _
instance showRepositoryTriggerName :: Show RepositoryTriggerName where
  show = genericShow
instance decodeRepositoryTriggerName :: Decode RepositoryTriggerName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggerName :: Encode RepositoryTriggerName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryTriggerNameList = RepositoryTriggerNameList (Array RepositoryTriggerName)
derive instance newtypeRepositoryTriggerNameList :: Newtype RepositoryTriggerNameList _
derive instance repGenericRepositoryTriggerNameList :: Generic RepositoryTriggerNameList _
instance showRepositoryTriggerNameList :: Show RepositoryTriggerNameList where
  show = genericShow
instance decodeRepositoryTriggerNameList :: Decode RepositoryTriggerNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggerNameList :: Encode RepositoryTriggerNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A name for the trigger is required but was not specified.</p>
newtype RepositoryTriggerNameRequiredException = RepositoryTriggerNameRequiredException Types.NoArguments
derive instance newtypeRepositoryTriggerNameRequiredException :: Newtype RepositoryTriggerNameRequiredException _
derive instance repGenericRepositoryTriggerNameRequiredException :: Generic RepositoryTriggerNameRequiredException _
instance showRepositoryTriggerNameRequiredException :: Show RepositoryTriggerNameRequiredException where
  show = genericShow
instance decodeRepositoryTriggerNameRequiredException :: Decode RepositoryTriggerNameRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggerNameRequiredException :: Encode RepositoryTriggerNameRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryTriggersConfigurationId = RepositoryTriggersConfigurationId String
derive instance newtypeRepositoryTriggersConfigurationId :: Newtype RepositoryTriggersConfigurationId _
derive instance repGenericRepositoryTriggersConfigurationId :: Generic RepositoryTriggersConfigurationId _
instance showRepositoryTriggersConfigurationId :: Show RepositoryTriggersConfigurationId where
  show = genericShow
instance decodeRepositoryTriggersConfigurationId :: Decode RepositoryTriggersConfigurationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggersConfigurationId :: Encode RepositoryTriggersConfigurationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepositoryTriggersList = RepositoryTriggersList (Array RepositoryTrigger)
derive instance newtypeRepositoryTriggersList :: Newtype RepositoryTriggersList _
derive instance repGenericRepositoryTriggersList :: Generic RepositoryTriggersList _
instance showRepositoryTriggersList :: Show RepositoryTriggersList where
  show = genericShow
instance decodeRepositoryTriggersList :: Decode RepositoryTriggersList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggersList :: Encode RepositoryTriggersList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The list of triggers for the repository is required but was not specified.</p>
newtype RepositoryTriggersListRequiredException = RepositoryTriggersListRequiredException Types.NoArguments
derive instance newtypeRepositoryTriggersListRequiredException :: Newtype RepositoryTriggersListRequiredException _
derive instance repGenericRepositoryTriggersListRequiredException :: Generic RepositoryTriggersListRequiredException _
instance showRepositoryTriggersListRequiredException :: Show RepositoryTriggersListRequiredException where
  show = genericShow
instance decodeRepositoryTriggersListRequiredException :: Decode RepositoryTriggersListRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepositoryTriggersListRequiredException :: Encode RepositoryTriggersListRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The file was not added or updated because the content of the file is exactly the same as the content of that file in the repository and branch that you specified.</p>
newtype SameFileContentException = SameFileContentException Types.NoArguments
derive instance newtypeSameFileContentException :: Newtype SameFileContentException _
derive instance repGenericSameFileContentException :: Generic SameFileContentException _
instance showSameFileContentException :: Show SameFileContentException where
  show = genericShow
instance decodeSameFileContentException :: Decode SameFileContentException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSameFileContentException :: Encode SameFileContentException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SortByEnum = SortByEnum String
derive instance newtypeSortByEnum :: Newtype SortByEnum _
derive instance repGenericSortByEnum :: Generic SortByEnum _
instance showSortByEnum :: Show SortByEnum where
  show = genericShow
instance decodeSortByEnum :: Decode SortByEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSortByEnum :: Encode SortByEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The source branch and the destination branch for the pull request are the same. You must specify different branches for the source and destination.</p>
newtype SourceAndDestinationAreSameException = SourceAndDestinationAreSameException Types.NoArguments
derive instance newtypeSourceAndDestinationAreSameException :: Newtype SourceAndDestinationAreSameException _
derive instance repGenericSourceAndDestinationAreSameException :: Generic SourceAndDestinationAreSameException _
instance showSourceAndDestinationAreSameException :: Show SourceAndDestinationAreSameException where
  show = genericShow
instance decodeSourceAndDestinationAreSameException :: Decode SourceAndDestinationAreSameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceAndDestinationAreSameException :: Encode SourceAndDestinationAreSameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about a target for a pull request.</p>
newtype Target = Target 
  { "RepositoryName'" :: (RepositoryName)
  , "SourceReference'" :: (ReferenceName)
  , "DestinationReference'" :: NullOrUndefined.NullOrUndefined (ReferenceName)
  }
derive instance newtypeTarget :: Newtype Target _
derive instance repGenericTarget :: Generic Target _
instance showTarget :: Show Target where
  show = genericShow
instance decodeTarget :: Decode Target where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTarget :: Encode Target where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetList = TargetList (Array Target)
derive instance newtypeTargetList :: Newtype TargetList _
derive instance repGenericTargetList :: Generic TargetList _
instance showTargetList :: Show TargetList where
  show = genericShow
instance decodeTargetList :: Decode TargetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetList :: Encode TargetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A pull request target is required. It cannot be empty or null. A pull request target must contain the full values for the repository name, source branch, and destination branch for the pull request.</p>
newtype TargetRequiredException = TargetRequiredException Types.NoArguments
derive instance newtypeTargetRequiredException :: Newtype TargetRequiredException _
derive instance repGenericTargetRequiredException :: Generic TargetRequiredException _
instance showTargetRequiredException :: Show TargetRequiredException where
  show = genericShow
instance decodeTargetRequiredException :: Decode TargetRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetRequiredException :: Encode TargetRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An array of target objects is required. It cannot be empty or null.</p>
newtype TargetsRequiredException = TargetsRequiredException Types.NoArguments
derive instance newtypeTargetsRequiredException :: Newtype TargetsRequiredException _
derive instance repGenericTargetsRequiredException :: Generic TargetsRequiredException _
instance showTargetsRequiredException :: Show TargetsRequiredException where
  show = genericShow
instance decodeTargetsRequiredException :: Decode TargetsRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetsRequiredException :: Encode TargetsRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a test repository triggers operation.</p>
newtype TestRepositoryTriggersInput = TestRepositoryTriggersInput 
  { "RepositoryName'" :: (RepositoryName)
  , "Triggers'" :: (RepositoryTriggersList)
  }
derive instance newtypeTestRepositoryTriggersInput :: Newtype TestRepositoryTriggersInput _
derive instance repGenericTestRepositoryTriggersInput :: Generic TestRepositoryTriggersInput _
instance showTestRepositoryTriggersInput :: Show TestRepositoryTriggersInput where
  show = genericShow
instance decodeTestRepositoryTriggersInput :: Decode TestRepositoryTriggersInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestRepositoryTriggersInput :: Encode TestRepositoryTriggersInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a test repository triggers operation.</p>
newtype TestRepositoryTriggersOutput = TestRepositoryTriggersOutput 
  { "SuccessfulExecutions'" :: NullOrUndefined.NullOrUndefined (RepositoryTriggerNameList)
  , "FailedExecutions'" :: NullOrUndefined.NullOrUndefined (RepositoryTriggerExecutionFailureList)
  }
derive instance newtypeTestRepositoryTriggersOutput :: Newtype TestRepositoryTriggersOutput _
derive instance repGenericTestRepositoryTriggersOutput :: Generic TestRepositoryTriggersOutput _
instance showTestRepositoryTriggersOutput :: Show TestRepositoryTriggersOutput where
  show = genericShow
instance decodeTestRepositoryTriggersOutput :: Decode TestRepositoryTriggersOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestRepositoryTriggersOutput :: Encode TestRepositoryTriggersOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The tip of the source branch in the destination repository does not match the tip of the source branch specified in your request. The pull request might have been updated. Make sure that you have the latest changes.</p>
newtype TipOfSourceReferenceIsDifferentException = TipOfSourceReferenceIsDifferentException Types.NoArguments
derive instance newtypeTipOfSourceReferenceIsDifferentException :: Newtype TipOfSourceReferenceIsDifferentException _
derive instance repGenericTipOfSourceReferenceIsDifferentException :: Generic TipOfSourceReferenceIsDifferentException _
instance showTipOfSourceReferenceIsDifferentException :: Show TipOfSourceReferenceIsDifferentException where
  show = genericShow
instance decodeTipOfSourceReferenceIsDifferentException :: Decode TipOfSourceReferenceIsDifferentException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTipOfSourceReferenceIsDifferentException :: Encode TipOfSourceReferenceIsDifferentException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The divergence between the tips of the provided commit specifiers is too great to determine whether there might be any merge conflicts. Locally compare the specifiers using <code>git diff</code> or a diff tool.</p>
newtype TipsDivergenceExceededException = TipsDivergenceExceededException Types.NoArguments
derive instance newtypeTipsDivergenceExceededException :: Newtype TipsDivergenceExceededException _
derive instance repGenericTipsDivergenceExceededException :: Generic TipsDivergenceExceededException _
instance showTipsDivergenceExceededException :: Show TipsDivergenceExceededException where
  show = genericShow
instance decodeTipsDivergenceExceededException :: Decode TipsDivergenceExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTipsDivergenceExceededException :: Encode TipsDivergenceExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Title = Title String
derive instance newtypeTitle :: Newtype Title _
derive instance repGenericTitle :: Generic Title _
instance showTitle :: Show Title where
  show = genericShow
instance decodeTitle :: Decode Title where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTitle :: Encode Title where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A pull request title is required. It cannot be empty or null.</p>
newtype TitleRequiredException = TitleRequiredException Types.NoArguments
derive instance newtypeTitleRequiredException :: Newtype TitleRequiredException _
derive instance repGenericTitleRequiredException :: Generic TitleRequiredException _
instance showTitleRequiredException :: Show TitleRequiredException where
  show = genericShow
instance decodeTitleRequiredException :: Decode TitleRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTitleRequiredException :: Encode TitleRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateCommentInput = UpdateCommentInput 
  { "CommentId'" :: (CommentId)
  , "Content'" :: (Content)
  }
derive instance newtypeUpdateCommentInput :: Newtype UpdateCommentInput _
derive instance repGenericUpdateCommentInput :: Generic UpdateCommentInput _
instance showUpdateCommentInput :: Show UpdateCommentInput where
  show = genericShow
instance decodeUpdateCommentInput :: Decode UpdateCommentInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCommentInput :: Encode UpdateCommentInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateCommentOutput = UpdateCommentOutput 
  { "Comment'" :: NullOrUndefined.NullOrUndefined (Comment)
  }
derive instance newtypeUpdateCommentOutput :: Newtype UpdateCommentOutput _
derive instance repGenericUpdateCommentOutput :: Generic UpdateCommentOutput _
instance showUpdateCommentOutput :: Show UpdateCommentOutput where
  show = genericShow
instance decodeUpdateCommentOutput :: Decode UpdateCommentOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCommentOutput :: Encode UpdateCommentOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of an update default branch operation.</p>
newtype UpdateDefaultBranchInput = UpdateDefaultBranchInput 
  { "RepositoryName'" :: (RepositoryName)
  , "DefaultBranchName'" :: (BranchName)
  }
derive instance newtypeUpdateDefaultBranchInput :: Newtype UpdateDefaultBranchInput _
derive instance repGenericUpdateDefaultBranchInput :: Generic UpdateDefaultBranchInput _
instance showUpdateDefaultBranchInput :: Show UpdateDefaultBranchInput where
  show = genericShow
instance decodeUpdateDefaultBranchInput :: Decode UpdateDefaultBranchInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDefaultBranchInput :: Encode UpdateDefaultBranchInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePullRequestDescriptionInput = UpdatePullRequestDescriptionInput 
  { "PullRequestId'" :: (PullRequestId)
  , "Description'" :: (Description)
  }
derive instance newtypeUpdatePullRequestDescriptionInput :: Newtype UpdatePullRequestDescriptionInput _
derive instance repGenericUpdatePullRequestDescriptionInput :: Generic UpdatePullRequestDescriptionInput _
instance showUpdatePullRequestDescriptionInput :: Show UpdatePullRequestDescriptionInput where
  show = genericShow
instance decodeUpdatePullRequestDescriptionInput :: Decode UpdatePullRequestDescriptionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePullRequestDescriptionInput :: Encode UpdatePullRequestDescriptionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePullRequestDescriptionOutput = UpdatePullRequestDescriptionOutput 
  { "PullRequest'" :: (PullRequest)
  }
derive instance newtypeUpdatePullRequestDescriptionOutput :: Newtype UpdatePullRequestDescriptionOutput _
derive instance repGenericUpdatePullRequestDescriptionOutput :: Generic UpdatePullRequestDescriptionOutput _
instance showUpdatePullRequestDescriptionOutput :: Show UpdatePullRequestDescriptionOutput where
  show = genericShow
instance decodeUpdatePullRequestDescriptionOutput :: Decode UpdatePullRequestDescriptionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePullRequestDescriptionOutput :: Encode UpdatePullRequestDescriptionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePullRequestStatusInput = UpdatePullRequestStatusInput 
  { "PullRequestId'" :: (PullRequestId)
  , "PullRequestStatus'" :: (PullRequestStatusEnum)
  }
derive instance newtypeUpdatePullRequestStatusInput :: Newtype UpdatePullRequestStatusInput _
derive instance repGenericUpdatePullRequestStatusInput :: Generic UpdatePullRequestStatusInput _
instance showUpdatePullRequestStatusInput :: Show UpdatePullRequestStatusInput where
  show = genericShow
instance decodeUpdatePullRequestStatusInput :: Decode UpdatePullRequestStatusInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePullRequestStatusInput :: Encode UpdatePullRequestStatusInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePullRequestStatusOutput = UpdatePullRequestStatusOutput 
  { "PullRequest'" :: (PullRequest)
  }
derive instance newtypeUpdatePullRequestStatusOutput :: Newtype UpdatePullRequestStatusOutput _
derive instance repGenericUpdatePullRequestStatusOutput :: Generic UpdatePullRequestStatusOutput _
instance showUpdatePullRequestStatusOutput :: Show UpdatePullRequestStatusOutput where
  show = genericShow
instance decodeUpdatePullRequestStatusOutput :: Decode UpdatePullRequestStatusOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePullRequestStatusOutput :: Encode UpdatePullRequestStatusOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePullRequestTitleInput = UpdatePullRequestTitleInput 
  { "PullRequestId'" :: (PullRequestId)
  , "Title'" :: (Title)
  }
derive instance newtypeUpdatePullRequestTitleInput :: Newtype UpdatePullRequestTitleInput _
derive instance repGenericUpdatePullRequestTitleInput :: Generic UpdatePullRequestTitleInput _
instance showUpdatePullRequestTitleInput :: Show UpdatePullRequestTitleInput where
  show = genericShow
instance decodeUpdatePullRequestTitleInput :: Decode UpdatePullRequestTitleInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePullRequestTitleInput :: Encode UpdatePullRequestTitleInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePullRequestTitleOutput = UpdatePullRequestTitleOutput 
  { "PullRequest'" :: (PullRequest)
  }
derive instance newtypeUpdatePullRequestTitleOutput :: Newtype UpdatePullRequestTitleOutput _
derive instance repGenericUpdatePullRequestTitleOutput :: Generic UpdatePullRequestTitleOutput _
instance showUpdatePullRequestTitleOutput :: Show UpdatePullRequestTitleOutput where
  show = genericShow
instance decodeUpdatePullRequestTitleOutput :: Decode UpdatePullRequestTitleOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePullRequestTitleOutput :: Encode UpdatePullRequestTitleOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of an update repository description operation.</p>
newtype UpdateRepositoryDescriptionInput = UpdateRepositoryDescriptionInput 
  { "RepositoryName'" :: (RepositoryName)
  , "RepositoryDescription'" :: NullOrUndefined.NullOrUndefined (RepositoryDescription)
  }
derive instance newtypeUpdateRepositoryDescriptionInput :: Newtype UpdateRepositoryDescriptionInput _
derive instance repGenericUpdateRepositoryDescriptionInput :: Generic UpdateRepositoryDescriptionInput _
instance showUpdateRepositoryDescriptionInput :: Show UpdateRepositoryDescriptionInput where
  show = genericShow
instance decodeUpdateRepositoryDescriptionInput :: Decode UpdateRepositoryDescriptionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRepositoryDescriptionInput :: Encode UpdateRepositoryDescriptionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of an update repository description operation.</p>
newtype UpdateRepositoryNameInput = UpdateRepositoryNameInput 
  { "OldName'" :: (RepositoryName)
  , "NewName'" :: (RepositoryName)
  }
derive instance newtypeUpdateRepositoryNameInput :: Newtype UpdateRepositoryNameInput _
derive instance repGenericUpdateRepositoryNameInput :: Generic UpdateRepositoryNameInput _
instance showUpdateRepositoryNameInput :: Show UpdateRepositoryNameInput where
  show = genericShow
instance decodeUpdateRepositoryNameInput :: Decode UpdateRepositoryNameInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRepositoryNameInput :: Encode UpdateRepositoryNameInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the user who made a specified commit.</p>
newtype UserInfo = UserInfo 
  { "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Email'" :: NullOrUndefined.NullOrUndefined (Email)
  , "Date'" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeUserInfo :: Newtype UserInfo _
derive instance repGenericUserInfo :: Generic UserInfo _
instance showUserInfo :: Show UserInfo where
  show = genericShow
instance decodeUserInfo :: Decode UserInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserInfo :: Encode UserInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
