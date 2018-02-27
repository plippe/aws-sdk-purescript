

-- | <fullname>AWS CodeStar</fullname> <p>This is the API reference for AWS CodeStar. This reference provides descriptions of the operations and data types for the AWS CodeStar API along with usage examples.</p> <p>You can use the AWS CodeStar API to work with:</p> <p>Projects and their resources, by calling the following:</p> <ul> <li> <p> <code>DeleteProject</code>, which deletes a project.</p> </li> <li> <p> <code>DescribeProject</code>, which lists the attributes of a project.</p> </li> <li> <p> <code>ListProjects</code>, which lists all projects associated with your AWS account.</p> </li> <li> <p> <code>ListResources</code>, which lists the resources associated with a project.</p> </li> <li> <p> <code>ListTagsForProject</code>, which lists the tags associated with a project.</p> </li> <li> <p> <code>TagProject</code>, which adds tags to a project.</p> </li> <li> <p> <code>UntagProject</code>, which removes tags from a project.</p> </li> <li> <p> <code>UpdateProject</code>, which updates the attributes of a project.</p> </li> </ul> <p>Teams and team members, by calling the following:</p> <ul> <li> <p> <code>AssociateTeamMember</code>, which adds an IAM user to the team for a project.</p> </li> <li> <p> <code>DisassociateTeamMember</code>, which removes an IAM user from the team for a project.</p> </li> <li> <p> <code>ListTeamMembers</code>, which lists all the IAM users in the team for a project, including their roles and attributes.</p> </li> <li> <p> <code>UpdateTeamMember</code>, which updates a team member's attributes in a project.</p> </li> </ul> <p>Users, by calling the following:</p> <ul> <li> <p> <code>CreateUserProfile</code>, which creates a user profile that contains data associated with the user across all projects.</p> </li> <li> <p> <code>DeleteUserProfile</code>, which deletes all user profile information across all projects.</p> </li> <li> <p> <code>DescribeUserProfile</code>, which describes the profile of a user.</p> </li> <li> <p> <code>ListUserProfiles</code>, which lists all user profiles.</p> </li> <li> <p> <code>UpdateUserProfile</code>, which updates the profile for a user.</p> </li> </ul>
module AWS.CodeStar where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CodeStar" :: String


-- | <p>Adds an IAM user to the team for an AWS CodeStar project.</p>
associateTeamMember :: forall eff. AssociateTeamMemberRequest -> Aff (err :: AWS.RequestError | eff) AssociateTeamMemberResult
associateTeamMember = AWS.request serviceName "AssociateTeamMember" 


-- | <p>Reserved for future use. To create a project, use the AWS CodeStar console.</p>
createProject :: forall eff. CreateProjectRequest -> Aff (err :: AWS.RequestError | eff) CreateProjectResult
createProject = AWS.request serviceName "CreateProject" 


-- | <p>Creates a profile for a user that includes user preferences, such as the display name and email address assocciated with the user, in AWS CodeStar. The user profile is not project-specific. Information in the user profile is displayed wherever the user's information appears to other users in AWS CodeStar.</p>
createUserProfile :: forall eff. CreateUserProfileRequest -> Aff (err :: AWS.RequestError | eff) CreateUserProfileResult
createUserProfile = AWS.request serviceName "CreateUserProfile" 


-- | <p>Deletes a project, including project resources. Does not delete users associated with the project, but does delete the IAM roles that allowed access to the project.</p>
deleteProject :: forall eff. DeleteProjectRequest -> Aff (err :: AWS.RequestError | eff) DeleteProjectResult
deleteProject = AWS.request serviceName "DeleteProject" 


-- | <p>Deletes a user profile in AWS CodeStar, including all personal preference data associated with that profile, such as display name and email address. It does not delete the history of that user, for example the history of commits made by that user.</p>
deleteUserProfile :: forall eff. DeleteUserProfileRequest -> Aff (err :: AWS.RequestError | eff) DeleteUserProfileResult
deleteUserProfile = AWS.request serviceName "DeleteUserProfile" 


-- | <p>Describes a project and its resources.</p>
describeProject :: forall eff. DescribeProjectRequest -> Aff (err :: AWS.RequestError | eff) DescribeProjectResult
describeProject = AWS.request serviceName "DescribeProject" 


-- | <p>Describes a user in AWS CodeStar and the user attributes across all projects.</p>
describeUserProfile :: forall eff. DescribeUserProfileRequest -> Aff (err :: AWS.RequestError | eff) DescribeUserProfileResult
describeUserProfile = AWS.request serviceName "DescribeUserProfile" 


-- | <p>Removes a user from a project. Removing a user from a project also removes the IAM policies from that user that allowed access to the project and its resources. Disassociating a team member does not remove that user's profile from AWS CodeStar. It does not remove the user from IAM.</p>
disassociateTeamMember :: forall eff. DisassociateTeamMemberRequest -> Aff (err :: AWS.RequestError | eff) DisassociateTeamMemberResult
disassociateTeamMember = AWS.request serviceName "DisassociateTeamMember" 


-- | <p>Lists all projects in AWS CodeStar associated with your AWS account.</p>
listProjects :: forall eff. ListProjectsRequest -> Aff (err :: AWS.RequestError | eff) ListProjectsResult
listProjects = AWS.request serviceName "ListProjects" 


-- | <p>Lists resources associated with a project in AWS CodeStar.</p>
listResources :: forall eff. ListResourcesRequest -> Aff (err :: AWS.RequestError | eff) ListResourcesResult
listResources = AWS.request serviceName "ListResources" 


-- | <p>Gets the tags for a project.</p>
listTagsForProject :: forall eff. ListTagsForProjectRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForProjectResult
listTagsForProject = AWS.request serviceName "ListTagsForProject" 


-- | <p>Lists all team members associated with a project.</p>
listTeamMembers :: forall eff. ListTeamMembersRequest -> Aff (err :: AWS.RequestError | eff) ListTeamMembersResult
listTeamMembers = AWS.request serviceName "ListTeamMembers" 


-- | <p>Lists all the user profiles configured for your AWS account in AWS CodeStar.</p>
listUserProfiles :: forall eff. ListUserProfilesRequest -> Aff (err :: AWS.RequestError | eff) ListUserProfilesResult
listUserProfiles = AWS.request serviceName "ListUserProfiles" 


-- | <p>Adds tags to a project.</p>
tagProject :: forall eff. TagProjectRequest -> Aff (err :: AWS.RequestError | eff) TagProjectResult
tagProject = AWS.request serviceName "TagProject" 


-- | <p>Removes tags from a project.</p>
untagProject :: forall eff. UntagProjectRequest -> Aff (err :: AWS.RequestError | eff) UntagProjectResult
untagProject = AWS.request serviceName "UntagProject" 


-- | <p>Updates a project in AWS CodeStar.</p>
updateProject :: forall eff. UpdateProjectRequest -> Aff (err :: AWS.RequestError | eff) UpdateProjectResult
updateProject = AWS.request serviceName "UpdateProject" 


-- | <p>Updates a team member's attributes in an AWS CodeStar project. For example, you can change a team member's role in the project, or change whether they have remote access to project resources.</p>
updateTeamMember :: forall eff. UpdateTeamMemberRequest -> Aff (err :: AWS.RequestError | eff) UpdateTeamMemberResult
updateTeamMember = AWS.request serviceName "UpdateTeamMember" 


-- | <p>Updates a user's profile in AWS CodeStar. The user profile is not project-specific. Information in the user profile is displayed wherever the user's information appears to other users in AWS CodeStar. </p>
updateUserProfile :: forall eff. UpdateUserProfileRequest -> Aff (err :: AWS.RequestError | eff) UpdateUserProfileResult
updateUserProfile = AWS.request serviceName "UpdateUserProfile" 


newtype AssociateTeamMemberRequest = AssociateTeamMemberRequest 
  { "ProjectId'" :: (ProjectId)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  , "UserArn'" :: (UserArn)
  , "ProjectRole'" :: (Role)
  , "RemoteAccessAllowed'" :: NullOrUndefined (RemoteAccessAllowed)
  }
derive instance newtypeAssociateTeamMemberRequest :: Newtype AssociateTeamMemberRequest _


newtype AssociateTeamMemberResult = AssociateTeamMemberResult 
  { "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeAssociateTeamMemberResult :: Newtype AssociateTeamMemberResult _


newtype ClientRequestToken = ClientRequestToken String
derive instance newtypeClientRequestToken :: Newtype ClientRequestToken _


-- | <p>Another modification is being made. That modification must complete before you can make your change.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { 
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _


newtype CreateProjectRequest = CreateProjectRequest 
  { "Name'" :: (ProjectName)
  , "Id'" :: (ProjectId)
  , "Description'" :: NullOrUndefined (ProjectDescription)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeCreateProjectRequest :: Newtype CreateProjectRequest _


newtype CreateProjectResult = CreateProjectResult 
  { "Id'" :: (ProjectId)
  , "Arn'" :: (ProjectArn)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  , "ProjectTemplateId'" :: NullOrUndefined (ProjectTemplateId)
  }
derive instance newtypeCreateProjectResult :: Newtype CreateProjectResult _


newtype CreateUserProfileRequest = CreateUserProfileRequest 
  { "UserArn'" :: (UserArn)
  , "DisplayName'" :: (UserProfileDisplayName)
  , "EmailAddress'" :: (Email)
  , "SshPublicKey'" :: NullOrUndefined (SshPublicKey)
  }
derive instance newtypeCreateUserProfileRequest :: Newtype CreateUserProfileRequest _


newtype CreateUserProfileResult = CreateUserProfileResult 
  { "UserArn'" :: (UserArn)
  , "DisplayName'" :: NullOrUndefined (UserProfileDisplayName)
  , "EmailAddress'" :: NullOrUndefined (Email)
  , "SshPublicKey'" :: NullOrUndefined (SshPublicKey)
  , "CreatedTimestamp'" :: NullOrUndefined (CreatedTimestamp)
  , "LastModifiedTimestamp'" :: NullOrUndefined (LastModifiedTimestamp)
  }
derive instance newtypeCreateUserProfileResult :: Newtype CreateUserProfileResult _


newtype CreatedTimestamp = CreatedTimestamp Number
derive instance newtypeCreatedTimestamp :: Newtype CreatedTimestamp _


newtype DeleteProjectRequest = DeleteProjectRequest 
  { "Id'" :: (ProjectId)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  , "DeleteStack'" :: NullOrUndefined (DeleteStack)
  }
derive instance newtypeDeleteProjectRequest :: Newtype DeleteProjectRequest _


newtype DeleteProjectResult = DeleteProjectResult 
  { "StackId'" :: NullOrUndefined (StackId)
  , "ProjectArn'" :: NullOrUndefined (ProjectArn)
  }
derive instance newtypeDeleteProjectResult :: Newtype DeleteProjectResult _


newtype DeleteStack = DeleteStack Boolean
derive instance newtypeDeleteStack :: Newtype DeleteStack _


newtype DeleteUserProfileRequest = DeleteUserProfileRequest 
  { "UserArn'" :: (UserArn)
  }
derive instance newtypeDeleteUserProfileRequest :: Newtype DeleteUserProfileRequest _


newtype DeleteUserProfileResult = DeleteUserProfileResult 
  { "UserArn'" :: (UserArn)
  }
derive instance newtypeDeleteUserProfileResult :: Newtype DeleteUserProfileResult _


newtype DescribeProjectRequest = DescribeProjectRequest 
  { "Id'" :: (ProjectId)
  }
derive instance newtypeDescribeProjectRequest :: Newtype DescribeProjectRequest _


newtype DescribeProjectResult = DescribeProjectResult 
  { "Name'" :: NullOrUndefined (ProjectName)
  , "Id'" :: NullOrUndefined (ProjectId)
  , "Arn'" :: NullOrUndefined (ProjectArn)
  , "Description'" :: NullOrUndefined (ProjectDescription)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  , "CreatedTimeStamp'" :: NullOrUndefined (CreatedTimestamp)
  , "StackId'" :: NullOrUndefined (StackId)
  , "ProjectTemplateId'" :: NullOrUndefined (ProjectTemplateId)
  }
derive instance newtypeDescribeProjectResult :: Newtype DescribeProjectResult _


newtype DescribeUserProfileRequest = DescribeUserProfileRequest 
  { "UserArn'" :: (UserArn)
  }
derive instance newtypeDescribeUserProfileRequest :: Newtype DescribeUserProfileRequest _


newtype DescribeUserProfileResult = DescribeUserProfileResult 
  { "UserArn'" :: (UserArn)
  , "DisplayName'" :: NullOrUndefined (UserProfileDisplayName)
  , "EmailAddress'" :: NullOrUndefined (Email)
  , "SshPublicKey'" :: NullOrUndefined (SshPublicKey)
  , "CreatedTimestamp'" :: (CreatedTimestamp)
  , "LastModifiedTimestamp'" :: (LastModifiedTimestamp)
  }
derive instance newtypeDescribeUserProfileResult :: Newtype DescribeUserProfileResult _


newtype DisassociateTeamMemberRequest = DisassociateTeamMemberRequest 
  { "ProjectId'" :: (ProjectId)
  , "UserArn'" :: (UserArn)
  }
derive instance newtypeDisassociateTeamMemberRequest :: Newtype DisassociateTeamMemberRequest _


newtype DisassociateTeamMemberResult = DisassociateTeamMemberResult 
  { 
  }
derive instance newtypeDisassociateTeamMemberResult :: Newtype DisassociateTeamMemberResult _


newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _


-- | <p>The next token is not valid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { 
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _


-- | <p>The service role is not valid.</p>
newtype InvalidServiceRoleException = InvalidServiceRoleException 
  { 
  }
derive instance newtypeInvalidServiceRoleException :: Newtype InvalidServiceRoleException _


newtype LastModifiedTimestamp = LastModifiedTimestamp Number
derive instance newtypeLastModifiedTimestamp :: Newtype LastModifiedTimestamp _


-- | <p>A resource limit has been exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListProjectsRequest = ListProjectsRequest 
  { "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListProjectsRequest :: Newtype ListProjectsRequest _


newtype ListProjectsResult = ListProjectsResult 
  { "Projects'" :: (ProjectsList)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListProjectsResult :: Newtype ListProjectsResult _


newtype ListResourcesRequest = ListResourcesRequest 
  { "ProjectId'" :: (ProjectId)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListResourcesRequest :: Newtype ListResourcesRequest _


newtype ListResourcesResult = ListResourcesResult 
  { "Resources'" :: NullOrUndefined (ResourcesResult)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListResourcesResult :: Newtype ListResourcesResult _


newtype ListTagsForProjectRequest = ListTagsForProjectRequest 
  { "Id'" :: (ProjectId)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListTagsForProjectRequest :: Newtype ListTagsForProjectRequest _


newtype ListTagsForProjectResult = ListTagsForProjectResult 
  { "Tags'" :: NullOrUndefined (Tags)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListTagsForProjectResult :: Newtype ListTagsForProjectResult _


newtype ListTeamMembersRequest = ListTeamMembersRequest 
  { "ProjectId'" :: (ProjectId)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListTeamMembersRequest :: Newtype ListTeamMembersRequest _


newtype ListTeamMembersResult = ListTeamMembersResult 
  { "TeamMembers'" :: (TeamMemberResult)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListTeamMembersResult :: Newtype ListTeamMembersResult _


newtype ListUserProfilesRequest = ListUserProfilesRequest 
  { "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListUserProfilesRequest :: Newtype ListUserProfilesRequest _


newtype ListUserProfilesResult = ListUserProfilesResult 
  { "UserProfiles'" :: (UserProfilesList)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListUserProfilesResult :: Newtype ListUserProfilesResult _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _


-- | <p>An AWS CodeStar project with the same ID already exists in this region for the AWS account. AWS CodeStar project IDs must be unique within a region for the AWS account.</p>
newtype ProjectAlreadyExistsException = ProjectAlreadyExistsException 
  { 
  }
derive instance newtypeProjectAlreadyExistsException :: Newtype ProjectAlreadyExistsException _


newtype ProjectArn = ProjectArn String
derive instance newtypeProjectArn :: Newtype ProjectArn _


-- | <p>Project configuration information is required but not specified.</p>
newtype ProjectConfigurationException = ProjectConfigurationException 
  { 
  }
derive instance newtypeProjectConfigurationException :: Newtype ProjectConfigurationException _


-- | <p>The project creation request was valid, but a nonspecific exception or error occurred during project creation. The project could not be created in AWS CodeStar.</p>
newtype ProjectCreationFailedException = ProjectCreationFailedException 
  { 
  }
derive instance newtypeProjectCreationFailedException :: Newtype ProjectCreationFailedException _


newtype ProjectDescription = ProjectDescription String
derive instance newtypeProjectDescription :: Newtype ProjectDescription _


newtype ProjectId = ProjectId String
derive instance newtypeProjectId :: Newtype ProjectId _


newtype ProjectName = ProjectName String
derive instance newtypeProjectName :: Newtype ProjectName _


-- | <p>The specified AWS CodeStar project was not found.</p>
newtype ProjectNotFoundException = ProjectNotFoundException 
  { 
  }
derive instance newtypeProjectNotFoundException :: Newtype ProjectNotFoundException _


-- | <p>Information about the metadata for a project.</p>
newtype ProjectSummary = ProjectSummary 
  { "ProjectId'" :: NullOrUndefined (ProjectId)
  , "ProjectArn'" :: NullOrUndefined (ProjectArn)
  }
derive instance newtypeProjectSummary :: Newtype ProjectSummary _


newtype ProjectTemplateId = ProjectTemplateId String
derive instance newtypeProjectTemplateId :: Newtype ProjectTemplateId _


newtype ProjectsList = ProjectsList (Array ProjectSummary)
derive instance newtypeProjectsList :: Newtype ProjectsList _


newtype RemoteAccessAllowed = RemoteAccessAllowed Boolean
derive instance newtypeRemoteAccessAllowed :: Newtype RemoteAccessAllowed _


-- | <p>Information about a resource for a project.</p>
newtype Resource = Resource 
  { "Id'" :: (ResourceId)
  }
derive instance newtypeResource :: Newtype Resource _


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _


newtype ResourcesResult = ResourcesResult (Array Resource)
derive instance newtypeResourcesResult :: Newtype ResourcesResult _


newtype Role = Role String
derive instance newtypeRole :: Newtype Role _


newtype SshPublicKey = SshPublicKey String
derive instance newtypeSshPublicKey :: Newtype SshPublicKey _


newtype StackId = StackId String
derive instance newtypeStackId :: Newtype StackId _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeys = TagKeys (Array TagKey)
derive instance newtypeTagKeys :: Newtype TagKeys _


newtype TagProjectRequest = TagProjectRequest 
  { "Id'" :: (ProjectId)
  , "Tags'" :: (Tags)
  }
derive instance newtypeTagProjectRequest :: Newtype TagProjectRequest _


newtype TagProjectResult = TagProjectResult 
  { "Tags'" :: NullOrUndefined (Tags)
  }
derive instance newtypeTagProjectResult :: Newtype TagProjectResult _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype Tags = Tags (Map TagKey TagValue)
derive instance newtypeTags :: Newtype Tags _


-- | <p>Information about a team member in a project.</p>
newtype TeamMember = TeamMember 
  { "UserArn'" :: (UserArn)
  , "ProjectRole'" :: (Role)
  , "RemoteAccessAllowed'" :: NullOrUndefined (RemoteAccessAllowed)
  }
derive instance newtypeTeamMember :: Newtype TeamMember _


-- | <p>The team member is already associated with a role in this project.</p>
newtype TeamMemberAlreadyAssociatedException = TeamMemberAlreadyAssociatedException 
  { 
  }
derive instance newtypeTeamMemberAlreadyAssociatedException :: Newtype TeamMemberAlreadyAssociatedException _


-- | <p>The specified team member was not found.</p>
newtype TeamMemberNotFoundException = TeamMemberNotFoundException 
  { 
  }
derive instance newtypeTeamMemberNotFoundException :: Newtype TeamMemberNotFoundException _


newtype TeamMemberResult = TeamMemberResult (Array TeamMember)
derive instance newtypeTeamMemberResult :: Newtype TeamMemberResult _


newtype UntagProjectRequest = UntagProjectRequest 
  { "Id'" :: (ProjectId)
  , "Tags'" :: (TagKeys)
  }
derive instance newtypeUntagProjectRequest :: Newtype UntagProjectRequest _


newtype UntagProjectResult = UntagProjectResult 
  { 
  }
derive instance newtypeUntagProjectResult :: Newtype UntagProjectResult _


newtype UpdateProjectRequest = UpdateProjectRequest 
  { "Id'" :: (ProjectId)
  , "Name'" :: NullOrUndefined (ProjectName)
  , "Description'" :: NullOrUndefined (ProjectDescription)
  }
derive instance newtypeUpdateProjectRequest :: Newtype UpdateProjectRequest _


newtype UpdateProjectResult = UpdateProjectResult 
  { 
  }
derive instance newtypeUpdateProjectResult :: Newtype UpdateProjectResult _


newtype UpdateTeamMemberRequest = UpdateTeamMemberRequest 
  { "ProjectId'" :: (ProjectId)
  , "UserArn'" :: (UserArn)
  , "ProjectRole'" :: NullOrUndefined (Role)
  , "RemoteAccessAllowed'" :: NullOrUndefined (RemoteAccessAllowed)
  }
derive instance newtypeUpdateTeamMemberRequest :: Newtype UpdateTeamMemberRequest _


newtype UpdateTeamMemberResult = UpdateTeamMemberResult 
  { "UserArn'" :: NullOrUndefined (UserArn)
  , "ProjectRole'" :: NullOrUndefined (Role)
  , "RemoteAccessAllowed'" :: NullOrUndefined (RemoteAccessAllowed)
  }
derive instance newtypeUpdateTeamMemberResult :: Newtype UpdateTeamMemberResult _


newtype UpdateUserProfileRequest = UpdateUserProfileRequest 
  { "UserArn'" :: (UserArn)
  , "DisplayName'" :: NullOrUndefined (UserProfileDisplayName)
  , "EmailAddress'" :: NullOrUndefined (Email)
  , "SshPublicKey'" :: NullOrUndefined (SshPublicKey)
  }
derive instance newtypeUpdateUserProfileRequest :: Newtype UpdateUserProfileRequest _


newtype UpdateUserProfileResult = UpdateUserProfileResult 
  { "UserArn'" :: (UserArn)
  , "DisplayName'" :: NullOrUndefined (UserProfileDisplayName)
  , "EmailAddress'" :: NullOrUndefined (Email)
  , "SshPublicKey'" :: NullOrUndefined (SshPublicKey)
  , "CreatedTimestamp'" :: NullOrUndefined (CreatedTimestamp)
  , "LastModifiedTimestamp'" :: NullOrUndefined (LastModifiedTimestamp)
  }
derive instance newtypeUpdateUserProfileResult :: Newtype UpdateUserProfileResult _


newtype UserArn = UserArn String
derive instance newtypeUserArn :: Newtype UserArn _


-- | <p>A user profile with that name already exists in this region for the AWS account. AWS CodeStar user profile names must be unique within a region for the AWS account. </p>
newtype UserProfileAlreadyExistsException = UserProfileAlreadyExistsException 
  { 
  }
derive instance newtypeUserProfileAlreadyExistsException :: Newtype UserProfileAlreadyExistsException _


newtype UserProfileDisplayName = UserProfileDisplayName String
derive instance newtypeUserProfileDisplayName :: Newtype UserProfileDisplayName _


-- | <p>The user profile was not found.</p>
newtype UserProfileNotFoundException = UserProfileNotFoundException 
  { 
  }
derive instance newtypeUserProfileNotFoundException :: Newtype UserProfileNotFoundException _


-- | <p>Information about a user's profile in AWS CodeStar.</p>
newtype UserProfileSummary = UserProfileSummary 
  { "UserArn'" :: NullOrUndefined (UserArn)
  , "DisplayName'" :: NullOrUndefined (UserProfileDisplayName)
  , "EmailAddress'" :: NullOrUndefined (Email)
  , "SshPublicKey'" :: NullOrUndefined (SshPublicKey)
  }
derive instance newtypeUserProfileSummary :: Newtype UserProfileSummary _


newtype UserProfilesList = UserProfilesList (Array UserProfileSummary)
derive instance newtypeUserProfilesList :: Newtype UserProfilesList _


-- | <p>The specified input is either not valid, or it could not be validated.</p>
newtype ValidationException = ValidationException 
  { 
  }
derive instance newtypeValidationException :: Newtype ValidationException _
