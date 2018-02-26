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

#### `AssociateTeamMemberResult`

``` purescript
newtype AssociateTeamMemberResult
  = AssociateTeamMemberResult { "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken) }
```

#### `ClientRequestToken`

``` purescript
newtype ClientRequestToken
  = ClientRequestToken String
```

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException {  }
```

<p>Another modification is being made. That modification must complete before you can make your change.</p>

#### `CreateProjectRequest`

``` purescript
newtype CreateProjectRequest
  = CreateProjectRequest { "Name'" :: ProjectName, "Id'" :: ProjectId, "Description'" :: NullOrUndefined (ProjectDescription), "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken) }
```

#### `CreateProjectResult`

``` purescript
newtype CreateProjectResult
  = CreateProjectResult { "Id'" :: ProjectId, "Arn'" :: ProjectArn, "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken), "ProjectTemplateId'" :: NullOrUndefined (ProjectTemplateId) }
```

#### `CreateUserProfileRequest`

``` purescript
newtype CreateUserProfileRequest
  = CreateUserProfileRequest { "UserArn'" :: UserArn, "DisplayName'" :: UserProfileDisplayName, "EmailAddress'" :: Email, "SshPublicKey'" :: NullOrUndefined (SshPublicKey) }
```

#### `CreateUserProfileResult`

``` purescript
newtype CreateUserProfileResult
  = CreateUserProfileResult { "UserArn'" :: UserArn, "DisplayName'" :: NullOrUndefined (UserProfileDisplayName), "EmailAddress'" :: NullOrUndefined (Email), "SshPublicKey'" :: NullOrUndefined (SshPublicKey), "CreatedTimestamp'" :: NullOrUndefined (CreatedTimestamp), "LastModifiedTimestamp'" :: NullOrUndefined (LastModifiedTimestamp) }
```

#### `CreatedTimestamp`

``` purescript
newtype CreatedTimestamp
  = CreatedTimestamp Number
```

#### `DeleteProjectRequest`

``` purescript
newtype DeleteProjectRequest
  = DeleteProjectRequest { "Id'" :: ProjectId, "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken), "DeleteStack'" :: NullOrUndefined (DeleteStack) }
```

#### `DeleteProjectResult`

``` purescript
newtype DeleteProjectResult
  = DeleteProjectResult { "StackId'" :: NullOrUndefined (StackId), "ProjectArn'" :: NullOrUndefined (ProjectArn) }
```

#### `DeleteStack`

``` purescript
newtype DeleteStack
  = DeleteStack Boolean
```

#### `DeleteUserProfileRequest`

``` purescript
newtype DeleteUserProfileRequest
  = DeleteUserProfileRequest { "UserArn'" :: UserArn }
```

#### `DeleteUserProfileResult`

``` purescript
newtype DeleteUserProfileResult
  = DeleteUserProfileResult { "UserArn'" :: UserArn }
```

#### `DescribeProjectRequest`

``` purescript
newtype DescribeProjectRequest
  = DescribeProjectRequest { "Id'" :: ProjectId }
```

#### `DescribeProjectResult`

``` purescript
newtype DescribeProjectResult
  = DescribeProjectResult { "Name'" :: NullOrUndefined (ProjectName), "Id'" :: NullOrUndefined (ProjectId), "Arn'" :: NullOrUndefined (ProjectArn), "Description'" :: NullOrUndefined (ProjectDescription), "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken), "CreatedTimeStamp'" :: NullOrUndefined (CreatedTimestamp), "StackId'" :: NullOrUndefined (StackId), "ProjectTemplateId'" :: NullOrUndefined (ProjectTemplateId) }
```

#### `DescribeUserProfileRequest`

``` purescript
newtype DescribeUserProfileRequest
  = DescribeUserProfileRequest { "UserArn'" :: UserArn }
```

#### `DescribeUserProfileResult`

``` purescript
newtype DescribeUserProfileResult
  = DescribeUserProfileResult { "UserArn'" :: UserArn, "DisplayName'" :: NullOrUndefined (UserProfileDisplayName), "EmailAddress'" :: NullOrUndefined (Email), "SshPublicKey'" :: NullOrUndefined (SshPublicKey), "CreatedTimestamp'" :: CreatedTimestamp, "LastModifiedTimestamp'" :: LastModifiedTimestamp }
```

#### `DisassociateTeamMemberRequest`

``` purescript
newtype DisassociateTeamMemberRequest
  = DisassociateTeamMemberRequest { "ProjectId'" :: ProjectId, "UserArn'" :: UserArn }
```

#### `DisassociateTeamMemberResult`

``` purescript
newtype DisassociateTeamMemberResult
  = DisassociateTeamMemberResult {  }
```

#### `Email`

``` purescript
newtype Email
  = Email String
```

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException {  }
```

<p>The next token is not valid.</p>

#### `InvalidServiceRoleException`

``` purescript
newtype InvalidServiceRoleException
  = InvalidServiceRoleException {  }
```

<p>The service role is not valid.</p>

#### `LastModifiedTimestamp`

``` purescript
newtype LastModifiedTimestamp
  = LastModifiedTimestamp Number
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>A resource limit has been exceeded.</p>

#### `ListProjectsRequest`

``` purescript
newtype ListProjectsRequest
  = ListProjectsRequest { "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `ListProjectsResult`

``` purescript
newtype ListProjectsResult
  = ListProjectsResult { "Projects'" :: ProjectsList, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `ListResourcesRequest`

``` purescript
newtype ListResourcesRequest
  = ListResourcesRequest { "ProjectId'" :: ProjectId, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `ListResourcesResult`

``` purescript
newtype ListResourcesResult
  = ListResourcesResult { "Resources'" :: NullOrUndefined (ResourcesResult), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `ListTagsForProjectRequest`

``` purescript
newtype ListTagsForProjectRequest
  = ListTagsForProjectRequest { "Id'" :: ProjectId, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `ListTagsForProjectResult`

``` purescript
newtype ListTagsForProjectResult
  = ListTagsForProjectResult { "Tags'" :: NullOrUndefined (Tags), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `ListTeamMembersRequest`

``` purescript
newtype ListTeamMembersRequest
  = ListTeamMembersRequest { "ProjectId'" :: ProjectId, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `ListTeamMembersResult`

``` purescript
newtype ListTeamMembersResult
  = ListTeamMembersResult { "TeamMembers'" :: TeamMemberResult, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `ListUserProfilesRequest`

``` purescript
newtype ListUserProfilesRequest
  = ListUserProfilesRequest { "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `ListUserProfilesResult`

``` purescript
newtype ListUserProfilesResult
  = ListUserProfilesResult { "UserProfiles'" :: UserProfilesList, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

#### `ProjectAlreadyExistsException`

``` purescript
newtype ProjectAlreadyExistsException
  = ProjectAlreadyExistsException {  }
```

<p>An AWS CodeStar project with the same ID already exists in this region for the AWS account. AWS CodeStar project IDs must be unique within a region for the AWS account.</p>

#### `ProjectArn`

``` purescript
newtype ProjectArn
  = ProjectArn String
```

#### `ProjectConfigurationException`

``` purescript
newtype ProjectConfigurationException
  = ProjectConfigurationException {  }
```

<p>Project configuration information is required but not specified.</p>

#### `ProjectCreationFailedException`

``` purescript
newtype ProjectCreationFailedException
  = ProjectCreationFailedException {  }
```

<p>The project creation request was valid, but a nonspecific exception or error occurred during project creation. The project could not be created in AWS CodeStar.</p>

#### `ProjectDescription`

``` purescript
newtype ProjectDescription
  = ProjectDescription String
```

#### `ProjectId`

``` purescript
newtype ProjectId
  = ProjectId String
```

#### `ProjectName`

``` purescript
newtype ProjectName
  = ProjectName String
```

#### `ProjectNotFoundException`

``` purescript
newtype ProjectNotFoundException
  = ProjectNotFoundException {  }
```

<p>The specified AWS CodeStar project was not found.</p>

#### `ProjectSummary`

``` purescript
newtype ProjectSummary
  = ProjectSummary { "ProjectId'" :: NullOrUndefined (ProjectId), "ProjectArn'" :: NullOrUndefined (ProjectArn) }
```

<p>Information about the metadata for a project.</p>

#### `ProjectTemplateId`

``` purescript
newtype ProjectTemplateId
  = ProjectTemplateId String
```

#### `ProjectsList`

``` purescript
newtype ProjectsList
  = ProjectsList (Array ProjectSummary)
```

#### `RemoteAccessAllowed`

``` purescript
newtype RemoteAccessAllowed
  = RemoteAccessAllowed Boolean
```

#### `Resource`

``` purescript
newtype Resource
  = Resource { "Id'" :: ResourceId }
```

<p>Information about a resource for a project.</p>

#### `ResourceId`

``` purescript
newtype ResourceId
  = ResourceId String
```

#### `ResourcesResult`

``` purescript
newtype ResourcesResult
  = ResourcesResult (Array Resource)
```

#### `Role`

``` purescript
newtype Role
  = Role String
```

#### `SshPublicKey`

``` purescript
newtype SshPublicKey
  = SshPublicKey String
```

#### `StackId`

``` purescript
newtype StackId
  = StackId String
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagKeys`

``` purescript
newtype TagKeys
  = TagKeys (Array TagKey)
```

#### `TagProjectRequest`

``` purescript
newtype TagProjectRequest
  = TagProjectRequest { "Id'" :: ProjectId, "Tags'" :: Tags }
```

#### `TagProjectResult`

``` purescript
newtype TagProjectResult
  = TagProjectResult { "Tags'" :: NullOrUndefined (Tags) }
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `Tags`

``` purescript
newtype Tags
  = Tags (Map TagKey TagValue)
```

#### `TeamMember`

``` purescript
newtype TeamMember
  = TeamMember { "UserArn'" :: UserArn, "ProjectRole'" :: Role, "RemoteAccessAllowed'" :: NullOrUndefined (RemoteAccessAllowed) }
```

<p>Information about a team member in a project.</p>

#### `TeamMemberAlreadyAssociatedException`

``` purescript
newtype TeamMemberAlreadyAssociatedException
  = TeamMemberAlreadyAssociatedException {  }
```

<p>The team member is already associated with a role in this project.</p>

#### `TeamMemberNotFoundException`

``` purescript
newtype TeamMemberNotFoundException
  = TeamMemberNotFoundException {  }
```

<p>The specified team member was not found.</p>

#### `TeamMemberResult`

``` purescript
newtype TeamMemberResult
  = TeamMemberResult (Array TeamMember)
```

#### `UntagProjectRequest`

``` purescript
newtype UntagProjectRequest
  = UntagProjectRequest { "Id'" :: ProjectId, "Tags'" :: TagKeys }
```

#### `UntagProjectResult`

``` purescript
newtype UntagProjectResult
  = UntagProjectResult {  }
```

#### `UpdateProjectRequest`

``` purescript
newtype UpdateProjectRequest
  = UpdateProjectRequest { "Id'" :: ProjectId, "Name'" :: NullOrUndefined (ProjectName), "Description'" :: NullOrUndefined (ProjectDescription) }
```

#### `UpdateProjectResult`

``` purescript
newtype UpdateProjectResult
  = UpdateProjectResult {  }
```

#### `UpdateTeamMemberRequest`

``` purescript
newtype UpdateTeamMemberRequest
  = UpdateTeamMemberRequest { "ProjectId'" :: ProjectId, "UserArn'" :: UserArn, "ProjectRole'" :: NullOrUndefined (Role), "RemoteAccessAllowed'" :: NullOrUndefined (RemoteAccessAllowed) }
```

#### `UpdateTeamMemberResult`

``` purescript
newtype UpdateTeamMemberResult
  = UpdateTeamMemberResult { "UserArn'" :: NullOrUndefined (UserArn), "ProjectRole'" :: NullOrUndefined (Role), "RemoteAccessAllowed'" :: NullOrUndefined (RemoteAccessAllowed) }
```

#### `UpdateUserProfileRequest`

``` purescript
newtype UpdateUserProfileRequest
  = UpdateUserProfileRequest { "UserArn'" :: UserArn, "DisplayName'" :: NullOrUndefined (UserProfileDisplayName), "EmailAddress'" :: NullOrUndefined (Email), "SshPublicKey'" :: NullOrUndefined (SshPublicKey) }
```

#### `UpdateUserProfileResult`

``` purescript
newtype UpdateUserProfileResult
  = UpdateUserProfileResult { "UserArn'" :: UserArn, "DisplayName'" :: NullOrUndefined (UserProfileDisplayName), "EmailAddress'" :: NullOrUndefined (Email), "SshPublicKey'" :: NullOrUndefined (SshPublicKey), "CreatedTimestamp'" :: NullOrUndefined (CreatedTimestamp), "LastModifiedTimestamp'" :: NullOrUndefined (LastModifiedTimestamp) }
```

#### `UserArn`

``` purescript
newtype UserArn
  = UserArn String
```

#### `UserProfileAlreadyExistsException`

``` purescript
newtype UserProfileAlreadyExistsException
  = UserProfileAlreadyExistsException {  }
```

<p>A user profile with that name already exists in this region for the AWS account. AWS CodeStar user profile names must be unique within a region for the AWS account. </p>

#### `UserProfileDisplayName`

``` purescript
newtype UserProfileDisplayName
  = UserProfileDisplayName String
```

#### `UserProfileNotFoundException`

``` purescript
newtype UserProfileNotFoundException
  = UserProfileNotFoundException {  }
```

<p>The user profile was not found.</p>

#### `UserProfileSummary`

``` purescript
newtype UserProfileSummary
  = UserProfileSummary { "UserArn'" :: NullOrUndefined (UserArn), "DisplayName'" :: NullOrUndefined (UserProfileDisplayName), "EmailAddress'" :: NullOrUndefined (Email), "SshPublicKey'" :: NullOrUndefined (SshPublicKey) }
```

<p>Information about a user's profile in AWS CodeStar.</p>

#### `UserProfilesList`

``` purescript
newtype UserProfilesList
  = UserProfilesList (Array UserProfileSummary)
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException {  }
```

<p>The specified input is either not valid, or it could not be validated.</p>


