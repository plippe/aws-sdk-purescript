## Module AWS.CodeStar

<fullname>AWS CodeStar</fullname> <p>This is the API reference for AWS CodeStar. This reference provides descriptions of the operations and data types for the AWS CodeStar API along with usage examples.</p> <p>You can use the AWS CodeStar API to work with:</p> <p>Projects and their resources, by calling the following:</p> <ul> <li> <p> <code>DeleteProject</code>, which deletes a project.</p> </li> <li> <p> <code>DescribeProject</code>, which lists the attributes of a project.</p> </li> <li> <p> <code>ListProjects</code>, which lists all projects associated with your AWS account.</p> </li> <li> <p> <code>ListResources</code>, which lists the resources associated with a project.</p> </li> <li> <p> <code>ListTagsForProject</code>, which lists the tags associated with a project.</p> </li> <li> <p> <code>TagProject</code>, which adds tags to a project.</p> </li> <li> <p> <code>UntagProject</code>, which removes tags from a project.</p> </li> <li> <p> <code>UpdateProject</code>, which updates the attributes of a project.</p> </li> </ul> <p>Teams and team members, by calling the following:</p> <ul> <li> <p> <code>AssociateTeamMember</code>, which adds an IAM user to the team for a project.</p> </li> <li> <p> <code>DisassociateTeamMember</code>, which removes an IAM user from the team for a project.</p> </li> <li> <p> <code>ListTeamMembers</code>, which lists all the IAM users in the team for a project, including their roles and attributes.</p> </li> <li> <p> <code>UpdateTeamMember</code>, which updates a team member's attributes in a project.</p> </li> </ul> <p>Users, by calling the following:</p> <ul> <li> <p> <code>CreateUserProfile</code>, which creates a user profile that contains data associated with the user across all projects.</p> </li> <li> <p> <code>DeleteUserProfile</code>, which deletes all user profile information across all projects.</p> </li> <li> <p> <code>DescribeUserProfile</code>, which describes the profile of a user.</p> </li> <li> <p> <code>ListUserProfiles</code>, which lists all user profiles.</p> </li> <li> <p> <code>UpdateUserProfile</code>, which updates the profile for a user.</p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `associateTeamMember`

``` purescript
associateTeamMember :: forall eff. AssociateTeamMemberRequest -> Aff (err :: RequestError | eff) AssociateTeamMemberResult
```

<p>Adds an IAM user to the team for an AWS CodeStar project.</p>

#### `createProject`

``` purescript
createProject :: forall eff. CreateProjectRequest -> Aff (err :: RequestError | eff) CreateProjectResult
```

<p>Reserved for future use. To create a project, use the AWS CodeStar console.</p>

#### `createUserProfile`

``` purescript
createUserProfile :: forall eff. CreateUserProfileRequest -> Aff (err :: RequestError | eff) CreateUserProfileResult
```

<p>Creates a profile for a user that includes user preferences, such as the display name and email address assocciated with the user, in AWS CodeStar. The user profile is not project-specific. Information in the user profile is displayed wherever the user's information appears to other users in AWS CodeStar.</p>

#### `deleteProject`

``` purescript
deleteProject :: forall eff. DeleteProjectRequest -> Aff (err :: RequestError | eff) DeleteProjectResult
```

<p>Deletes a project, including project resources. Does not delete users associated with the project, but does delete the IAM roles that allowed access to the project.</p>

#### `deleteUserProfile`

``` purescript
deleteUserProfile :: forall eff. DeleteUserProfileRequest -> Aff (err :: RequestError | eff) DeleteUserProfileResult
```

<p>Deletes a user profile in AWS CodeStar, including all personal preference data associated with that profile, such as display name and email address. It does not delete the history of that user, for example the history of commits made by that user.</p>

#### `describeProject`

``` purescript
describeProject :: forall eff. DescribeProjectRequest -> Aff (err :: RequestError | eff) DescribeProjectResult
```

<p>Describes a project and its resources.</p>

#### `describeUserProfile`

``` purescript
describeUserProfile :: forall eff. DescribeUserProfileRequest -> Aff (err :: RequestError | eff) DescribeUserProfileResult
```

<p>Describes a user in AWS CodeStar and the user attributes across all projects.</p>

#### `disassociateTeamMember`

``` purescript
disassociateTeamMember :: forall eff. DisassociateTeamMemberRequest -> Aff (err :: RequestError | eff) DisassociateTeamMemberResult
```

<p>Removes a user from a project. Removing a user from a project also removes the IAM policies from that user that allowed access to the project and its resources. Disassociating a team member does not remove that user's profile from AWS CodeStar. It does not remove the user from IAM.</p>

#### `listProjects`

``` purescript
listProjects :: forall eff. ListProjectsRequest -> Aff (err :: RequestError | eff) ListProjectsResult
```

<p>Lists all projects in AWS CodeStar associated with your AWS account.</p>

#### `listResources`

``` purescript
listResources :: forall eff. ListResourcesRequest -> Aff (err :: RequestError | eff) ListResourcesResult
```

<p>Lists resources associated with a project in AWS CodeStar.</p>

#### `listTagsForProject`

``` purescript
listTagsForProject :: forall eff. ListTagsForProjectRequest -> Aff (err :: RequestError | eff) ListTagsForProjectResult
```

<p>Gets the tags for a project.</p>

#### `listTeamMembers`

``` purescript
listTeamMembers :: forall eff. ListTeamMembersRequest -> Aff (err :: RequestError | eff) ListTeamMembersResult
```

<p>Lists all team members associated with a project.</p>

#### `listUserProfiles`

``` purescript
listUserProfiles :: forall eff. ListUserProfilesRequest -> Aff (err :: RequestError | eff) ListUserProfilesResult
```

<p>Lists all the user profiles configured for your AWS account in AWS CodeStar.</p>

#### `tagProject`

``` purescript
tagProject :: forall eff. TagProjectRequest -> Aff (err :: RequestError | eff) TagProjectResult
```

<p>Adds tags to a project.</p>

#### `untagProject`

``` purescript
untagProject :: forall eff. UntagProjectRequest -> Aff (err :: RequestError | eff) UntagProjectResult
```

<p>Removes tags from a project.</p>

#### `updateProject`

``` purescript
updateProject :: forall eff. UpdateProjectRequest -> Aff (err :: RequestError | eff) UpdateProjectResult
```

<p>Updates a project in AWS CodeStar.</p>

#### `updateTeamMember`

``` purescript
updateTeamMember :: forall eff. UpdateTeamMemberRequest -> Aff (err :: RequestError | eff) UpdateTeamMemberResult
```

<p>Updates a team member's attributes in an AWS CodeStar project. For example, you can change a team member's role in the project, or change whether they have remote access to project resources.</p>

#### `updateUserProfile`

``` purescript
updateUserProfile :: forall eff. UpdateUserProfileRequest -> Aff (err :: RequestError | eff) UpdateUserProfileResult
```

<p>Updates a user's profile in AWS CodeStar. The user profile is not project-specific. Information in the user profile is displayed wherever the user's information appears to other users in AWS CodeStar. </p>

#### `AssociateTeamMemberRequest`

``` purescript
newtype AssociateTeamMemberRequest
  = AssociateTeamMemberRequest { "ProjectId'" :: ProjectId, "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken), "UserArn'" :: UserArn, "ProjectRole'" :: Role, "RemoteAccessAllowed'" :: NullOrUndefined (RemoteAccessAllowed) }
```

##### Instances
``` purescript
Newtype AssociateTeamMemberRequest _
```

#### `AssociateTeamMemberResult`

``` purescript
newtype AssociateTeamMemberResult
  = AssociateTeamMemberResult { "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken) }
```

##### Instances
``` purescript
Newtype AssociateTeamMemberResult _
```

#### `ClientRequestToken`

``` purescript
newtype ClientRequestToken
  = ClientRequestToken String
```

##### Instances
``` purescript
Newtype ClientRequestToken _
```

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException {  }
```

<p>Another modification is being made. That modification must complete before you can make your change.</p>

##### Instances
``` purescript
Newtype ConcurrentModificationException _
```

#### `CreateProjectRequest`

``` purescript
newtype CreateProjectRequest
  = CreateProjectRequest { "Name'" :: ProjectName, "Id'" :: ProjectId, "Description'" :: NullOrUndefined (ProjectDescription), "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken) }
```

##### Instances
``` purescript
Newtype CreateProjectRequest _
```

#### `CreateProjectResult`

``` purescript
newtype CreateProjectResult
  = CreateProjectResult { "Id'" :: ProjectId, "Arn'" :: ProjectArn, "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken), "ProjectTemplateId'" :: NullOrUndefined (ProjectTemplateId) }
```

##### Instances
``` purescript
Newtype CreateProjectResult _
```

#### `CreateUserProfileRequest`

``` purescript
newtype CreateUserProfileRequest
  = CreateUserProfileRequest { "UserArn'" :: UserArn, "DisplayName'" :: UserProfileDisplayName, "EmailAddress'" :: Email, "SshPublicKey'" :: NullOrUndefined (SshPublicKey) }
```

##### Instances
``` purescript
Newtype CreateUserProfileRequest _
```

#### `CreateUserProfileResult`

``` purescript
newtype CreateUserProfileResult
  = CreateUserProfileResult { "UserArn'" :: UserArn, "DisplayName'" :: NullOrUndefined (UserProfileDisplayName), "EmailAddress'" :: NullOrUndefined (Email), "SshPublicKey'" :: NullOrUndefined (SshPublicKey), "CreatedTimestamp'" :: NullOrUndefined (CreatedTimestamp), "LastModifiedTimestamp'" :: NullOrUndefined (LastModifiedTimestamp) }
```

##### Instances
``` purescript
Newtype CreateUserProfileResult _
```

#### `CreatedTimestamp`

``` purescript
newtype CreatedTimestamp
  = CreatedTimestamp Number
```

##### Instances
``` purescript
Newtype CreatedTimestamp _
```

#### `DeleteProjectRequest`

``` purescript
newtype DeleteProjectRequest
  = DeleteProjectRequest { "Id'" :: ProjectId, "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken), "DeleteStack'" :: NullOrUndefined (DeleteStack) }
```

##### Instances
``` purescript
Newtype DeleteProjectRequest _
```

#### `DeleteProjectResult`

``` purescript
newtype DeleteProjectResult
  = DeleteProjectResult { "StackId'" :: NullOrUndefined (StackId), "ProjectArn'" :: NullOrUndefined (ProjectArn) }
```

##### Instances
``` purescript
Newtype DeleteProjectResult _
```

#### `DeleteStack`

``` purescript
newtype DeleteStack
  = DeleteStack Boolean
```

##### Instances
``` purescript
Newtype DeleteStack _
```

#### `DeleteUserProfileRequest`

``` purescript
newtype DeleteUserProfileRequest
  = DeleteUserProfileRequest { "UserArn'" :: UserArn }
```

##### Instances
``` purescript
Newtype DeleteUserProfileRequest _
```

#### `DeleteUserProfileResult`

``` purescript
newtype DeleteUserProfileResult
  = DeleteUserProfileResult { "UserArn'" :: UserArn }
```

##### Instances
``` purescript
Newtype DeleteUserProfileResult _
```

#### `DescribeProjectRequest`

``` purescript
newtype DescribeProjectRequest
  = DescribeProjectRequest { "Id'" :: ProjectId }
```

##### Instances
``` purescript
Newtype DescribeProjectRequest _
```

#### `DescribeProjectResult`

``` purescript
newtype DescribeProjectResult
  = DescribeProjectResult { "Name'" :: NullOrUndefined (ProjectName), "Id'" :: NullOrUndefined (ProjectId), "Arn'" :: NullOrUndefined (ProjectArn), "Description'" :: NullOrUndefined (ProjectDescription), "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken), "CreatedTimeStamp'" :: NullOrUndefined (CreatedTimestamp), "StackId'" :: NullOrUndefined (StackId), "ProjectTemplateId'" :: NullOrUndefined (ProjectTemplateId) }
```

##### Instances
``` purescript
Newtype DescribeProjectResult _
```

#### `DescribeUserProfileRequest`

``` purescript
newtype DescribeUserProfileRequest
  = DescribeUserProfileRequest { "UserArn'" :: UserArn }
```

##### Instances
``` purescript
Newtype DescribeUserProfileRequest _
```

#### `DescribeUserProfileResult`

``` purescript
newtype DescribeUserProfileResult
  = DescribeUserProfileResult { "UserArn'" :: UserArn, "DisplayName'" :: NullOrUndefined (UserProfileDisplayName), "EmailAddress'" :: NullOrUndefined (Email), "SshPublicKey'" :: NullOrUndefined (SshPublicKey), "CreatedTimestamp'" :: CreatedTimestamp, "LastModifiedTimestamp'" :: LastModifiedTimestamp }
```

##### Instances
``` purescript
Newtype DescribeUserProfileResult _
```

#### `DisassociateTeamMemberRequest`

``` purescript
newtype DisassociateTeamMemberRequest
  = DisassociateTeamMemberRequest { "ProjectId'" :: ProjectId, "UserArn'" :: UserArn }
```

##### Instances
``` purescript
Newtype DisassociateTeamMemberRequest _
```

#### `DisassociateTeamMemberResult`

``` purescript
newtype DisassociateTeamMemberResult
  = DisassociateTeamMemberResult {  }
```

##### Instances
``` purescript
Newtype DisassociateTeamMemberResult _
```

#### `Email`

``` purescript
newtype Email
  = Email String
```

##### Instances
``` purescript
Newtype Email _
```

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException {  }
```

<p>The next token is not valid.</p>

##### Instances
``` purescript
Newtype InvalidNextTokenException _
```

#### `InvalidServiceRoleException`

``` purescript
newtype InvalidServiceRoleException
  = InvalidServiceRoleException {  }
```

<p>The service role is not valid.</p>

##### Instances
``` purescript
Newtype InvalidServiceRoleException _
```

#### `LastModifiedTimestamp`

``` purescript
newtype LastModifiedTimestamp
  = LastModifiedTimestamp Number
```

##### Instances
``` purescript
Newtype LastModifiedTimestamp _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>A resource limit has been exceeded.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListProjectsRequest`

``` purescript
newtype ListProjectsRequest
  = ListProjectsRequest { "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListProjectsRequest _
```

#### `ListProjectsResult`

``` purescript
newtype ListProjectsResult
  = ListProjectsResult { "Projects'" :: ProjectsList, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListProjectsResult _
```

#### `ListResourcesRequest`

``` purescript
newtype ListResourcesRequest
  = ListResourcesRequest { "ProjectId'" :: ProjectId, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListResourcesRequest _
```

#### `ListResourcesResult`

``` purescript
newtype ListResourcesResult
  = ListResourcesResult { "Resources'" :: NullOrUndefined (ResourcesResult), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListResourcesResult _
```

#### `ListTagsForProjectRequest`

``` purescript
newtype ListTagsForProjectRequest
  = ListTagsForProjectRequest { "Id'" :: ProjectId, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListTagsForProjectRequest _
```

#### `ListTagsForProjectResult`

``` purescript
newtype ListTagsForProjectResult
  = ListTagsForProjectResult { "Tags'" :: NullOrUndefined (Tags), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListTagsForProjectResult _
```

#### `ListTeamMembersRequest`

``` purescript
newtype ListTeamMembersRequest
  = ListTeamMembersRequest { "ProjectId'" :: ProjectId, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListTeamMembersRequest _
```

#### `ListTeamMembersResult`

``` purescript
newtype ListTeamMembersResult
  = ListTeamMembersResult { "TeamMembers'" :: TeamMemberResult, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListTeamMembersResult _
```

#### `ListUserProfilesRequest`

``` purescript
newtype ListUserProfilesRequest
  = ListUserProfilesRequest { "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListUserProfilesRequest _
```

#### `ListUserProfilesResult`

``` purescript
newtype ListUserProfilesResult
  = ListUserProfilesResult { "UserProfiles'" :: UserProfilesList, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListUserProfilesResult _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

##### Instances
``` purescript
Newtype MaxResults _
```

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

##### Instances
``` purescript
Newtype PaginationToken _
```

#### `ProjectAlreadyExistsException`

``` purescript
newtype ProjectAlreadyExistsException
  = ProjectAlreadyExistsException {  }
```

<p>An AWS CodeStar project with the same ID already exists in this region for the AWS account. AWS CodeStar project IDs must be unique within a region for the AWS account.</p>

##### Instances
``` purescript
Newtype ProjectAlreadyExistsException _
```

#### `ProjectArn`

``` purescript
newtype ProjectArn
  = ProjectArn String
```

##### Instances
``` purescript
Newtype ProjectArn _
```

#### `ProjectConfigurationException`

``` purescript
newtype ProjectConfigurationException
  = ProjectConfigurationException {  }
```

<p>Project configuration information is required but not specified.</p>

##### Instances
``` purescript
Newtype ProjectConfigurationException _
```

#### `ProjectCreationFailedException`

``` purescript
newtype ProjectCreationFailedException
  = ProjectCreationFailedException {  }
```

<p>The project creation request was valid, but a nonspecific exception or error occurred during project creation. The project could not be created in AWS CodeStar.</p>

##### Instances
``` purescript
Newtype ProjectCreationFailedException _
```

#### `ProjectDescription`

``` purescript
newtype ProjectDescription
  = ProjectDescription String
```

##### Instances
``` purescript
Newtype ProjectDescription _
```

#### `ProjectId`

``` purescript
newtype ProjectId
  = ProjectId String
```

##### Instances
``` purescript
Newtype ProjectId _
```

#### `ProjectName`

``` purescript
newtype ProjectName
  = ProjectName String
```

##### Instances
``` purescript
Newtype ProjectName _
```

#### `ProjectNotFoundException`

``` purescript
newtype ProjectNotFoundException
  = ProjectNotFoundException {  }
```

<p>The specified AWS CodeStar project was not found.</p>

##### Instances
``` purescript
Newtype ProjectNotFoundException _
```

#### `ProjectSummary`

``` purescript
newtype ProjectSummary
  = ProjectSummary { "ProjectId'" :: NullOrUndefined (ProjectId), "ProjectArn'" :: NullOrUndefined (ProjectArn) }
```

<p>Information about the metadata for a project.</p>

##### Instances
``` purescript
Newtype ProjectSummary _
```

#### `ProjectTemplateId`

``` purescript
newtype ProjectTemplateId
  = ProjectTemplateId String
```

##### Instances
``` purescript
Newtype ProjectTemplateId _
```

#### `ProjectsList`

``` purescript
newtype ProjectsList
  = ProjectsList (Array ProjectSummary)
```

##### Instances
``` purescript
Newtype ProjectsList _
```

#### `RemoteAccessAllowed`

``` purescript
newtype RemoteAccessAllowed
  = RemoteAccessAllowed Boolean
```

##### Instances
``` purescript
Newtype RemoteAccessAllowed _
```

#### `Resource`

``` purescript
newtype Resource
  = Resource { "Id'" :: ResourceId }
```

<p>Information about a resource for a project.</p>

##### Instances
``` purescript
Newtype Resource _
```

#### `ResourceId`

``` purescript
newtype ResourceId
  = ResourceId String
```

##### Instances
``` purescript
Newtype ResourceId _
```

#### `ResourcesResult`

``` purescript
newtype ResourcesResult
  = ResourcesResult (Array Resource)
```

##### Instances
``` purescript
Newtype ResourcesResult _
```

#### `Role`

``` purescript
newtype Role
  = Role String
```

##### Instances
``` purescript
Newtype Role _
```

#### `SshPublicKey`

``` purescript
newtype SshPublicKey
  = SshPublicKey String
```

##### Instances
``` purescript
Newtype SshPublicKey _
```

#### `StackId`

``` purescript
newtype StackId
  = StackId String
```

##### Instances
``` purescript
Newtype StackId _
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

##### Instances
``` purescript
Newtype TagKey _
```

#### `TagKeys`

``` purescript
newtype TagKeys
  = TagKeys (Array TagKey)
```

##### Instances
``` purescript
Newtype TagKeys _
```

#### `TagProjectRequest`

``` purescript
newtype TagProjectRequest
  = TagProjectRequest { "Id'" :: ProjectId, "Tags'" :: Tags }
```

##### Instances
``` purescript
Newtype TagProjectRequest _
```

#### `TagProjectResult`

``` purescript
newtype TagProjectResult
  = TagProjectResult { "Tags'" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype TagProjectResult _
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

##### Instances
``` purescript
Newtype TagValue _
```

#### `Tags`

``` purescript
newtype Tags
  = Tags (Map TagKey TagValue)
```

##### Instances
``` purescript
Newtype Tags _
```

#### `TeamMember`

``` purescript
newtype TeamMember
  = TeamMember { "UserArn'" :: UserArn, "ProjectRole'" :: Role, "RemoteAccessAllowed'" :: NullOrUndefined (RemoteAccessAllowed) }
```

<p>Information about a team member in a project.</p>

##### Instances
``` purescript
Newtype TeamMember _
```

#### `TeamMemberAlreadyAssociatedException`

``` purescript
newtype TeamMemberAlreadyAssociatedException
  = TeamMemberAlreadyAssociatedException {  }
```

<p>The team member is already associated with a role in this project.</p>

##### Instances
``` purescript
Newtype TeamMemberAlreadyAssociatedException _
```

#### `TeamMemberNotFoundException`

``` purescript
newtype TeamMemberNotFoundException
  = TeamMemberNotFoundException {  }
```

<p>The specified team member was not found.</p>

##### Instances
``` purescript
Newtype TeamMemberNotFoundException _
```

#### `TeamMemberResult`

``` purescript
newtype TeamMemberResult
  = TeamMemberResult (Array TeamMember)
```

##### Instances
``` purescript
Newtype TeamMemberResult _
```

#### `UntagProjectRequest`

``` purescript
newtype UntagProjectRequest
  = UntagProjectRequest { "Id'" :: ProjectId, "Tags'" :: TagKeys }
```

##### Instances
``` purescript
Newtype UntagProjectRequest _
```

#### `UntagProjectResult`

``` purescript
newtype UntagProjectResult
  = UntagProjectResult {  }
```

##### Instances
``` purescript
Newtype UntagProjectResult _
```

#### `UpdateProjectRequest`

``` purescript
newtype UpdateProjectRequest
  = UpdateProjectRequest { "Id'" :: ProjectId, "Name'" :: NullOrUndefined (ProjectName), "Description'" :: NullOrUndefined (ProjectDescription) }
```

##### Instances
``` purescript
Newtype UpdateProjectRequest _
```

#### `UpdateProjectResult`

``` purescript
newtype UpdateProjectResult
  = UpdateProjectResult {  }
```

##### Instances
``` purescript
Newtype UpdateProjectResult _
```

#### `UpdateTeamMemberRequest`

``` purescript
newtype UpdateTeamMemberRequest
  = UpdateTeamMemberRequest { "ProjectId'" :: ProjectId, "UserArn'" :: UserArn, "ProjectRole'" :: NullOrUndefined (Role), "RemoteAccessAllowed'" :: NullOrUndefined (RemoteAccessAllowed) }
```

##### Instances
``` purescript
Newtype UpdateTeamMemberRequest _
```

#### `UpdateTeamMemberResult`

``` purescript
newtype UpdateTeamMemberResult
  = UpdateTeamMemberResult { "UserArn'" :: NullOrUndefined (UserArn), "ProjectRole'" :: NullOrUndefined (Role), "RemoteAccessAllowed'" :: NullOrUndefined (RemoteAccessAllowed) }
```

##### Instances
``` purescript
Newtype UpdateTeamMemberResult _
```

#### `UpdateUserProfileRequest`

``` purescript
newtype UpdateUserProfileRequest
  = UpdateUserProfileRequest { "UserArn'" :: UserArn, "DisplayName'" :: NullOrUndefined (UserProfileDisplayName), "EmailAddress'" :: NullOrUndefined (Email), "SshPublicKey'" :: NullOrUndefined (SshPublicKey) }
```

##### Instances
``` purescript
Newtype UpdateUserProfileRequest _
```

#### `UpdateUserProfileResult`

``` purescript
newtype UpdateUserProfileResult
  = UpdateUserProfileResult { "UserArn'" :: UserArn, "DisplayName'" :: NullOrUndefined (UserProfileDisplayName), "EmailAddress'" :: NullOrUndefined (Email), "SshPublicKey'" :: NullOrUndefined (SshPublicKey), "CreatedTimestamp'" :: NullOrUndefined (CreatedTimestamp), "LastModifiedTimestamp'" :: NullOrUndefined (LastModifiedTimestamp) }
```

##### Instances
``` purescript
Newtype UpdateUserProfileResult _
```

#### `UserArn`

``` purescript
newtype UserArn
  = UserArn String
```

##### Instances
``` purescript
Newtype UserArn _
```

#### `UserProfileAlreadyExistsException`

``` purescript
newtype UserProfileAlreadyExistsException
  = UserProfileAlreadyExistsException {  }
```

<p>A user profile with that name already exists in this region for the AWS account. AWS CodeStar user profile names must be unique within a region for the AWS account. </p>

##### Instances
``` purescript
Newtype UserProfileAlreadyExistsException _
```

#### `UserProfileDisplayName`

``` purescript
newtype UserProfileDisplayName
  = UserProfileDisplayName String
```

##### Instances
``` purescript
Newtype UserProfileDisplayName _
```

#### `UserProfileNotFoundException`

``` purescript
newtype UserProfileNotFoundException
  = UserProfileNotFoundException {  }
```

<p>The user profile was not found.</p>

##### Instances
``` purescript
Newtype UserProfileNotFoundException _
```

#### `UserProfileSummary`

``` purescript
newtype UserProfileSummary
  = UserProfileSummary { "UserArn'" :: NullOrUndefined (UserArn), "DisplayName'" :: NullOrUndefined (UserProfileDisplayName), "EmailAddress'" :: NullOrUndefined (Email), "SshPublicKey'" :: NullOrUndefined (SshPublicKey) }
```

<p>Information about a user's profile in AWS CodeStar.</p>

##### Instances
``` purescript
Newtype UserProfileSummary _
```

#### `UserProfilesList`

``` purescript
newtype UserProfilesList
  = UserProfilesList (Array UserProfileSummary)
```

##### Instances
``` purescript
Newtype UserProfilesList _
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException {  }
```

<p>The specified input is either not valid, or it could not be validated.</p>

##### Instances
``` purescript
Newtype ValidationException _
```


