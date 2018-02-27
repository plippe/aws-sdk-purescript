## Module AWS.WorkMail

<p>Amazon WorkMail is a secure, managed business email and calendaring service with support for existing desktop and mobile email clients. You can access your email, contacts, and calendars using Microsoft Outlook, your browser, or their native iOS and Android email applications. You can integrate Amazon WorkMail with your existing corporate directory and control both the keys that encrypt your data and the location in which your data is stored.</p> <p>The Amazon WorkMail API is designed for the following scenarios:</p> <ul> <li> <p>Listing and describing organizations</p> </li> </ul> <ul> <li> <p>Managing users</p> </li> </ul> <ul> <li> <p>Managing groups</p> </li> </ul> <ul> <li> <p>Managing resources</p> </li> </ul> <p>All Amazon WorkMail API actions are Amazon-authenticated and certificate-signed. They not only require the use of the AWS SDK, but also allow for the exclusive use of IAM users and roles to help facilitate access, trust, and permission policies. By creating a role and allowing an IAM user to access the Amazon WorkMail site, the IAM user gains full administrative visibility into the entire Amazon WorkMail organization (or as set in the IAM policy). This includes, but is not limited to, the ability to create, update, and delete users, groups, and resources. This allows developers to perform the scenarios listed above, as well as give users the ability to grant access on a selective basis using the IAM model.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `associateDelegateToResource`

``` purescript
associateDelegateToResource :: forall eff. AssociateDelegateToResourceRequest -> Aff (err :: RequestError | eff) AssociateDelegateToResourceResponse
```

<p>Adds a member to the resource's set of delegates.</p>

#### `associateMemberToGroup`

``` purescript
associateMemberToGroup :: forall eff. AssociateMemberToGroupRequest -> Aff (err :: RequestError | eff) AssociateMemberToGroupResponse
```

<p>Adds a member to the group's set.</p>

#### `createAlias`

``` purescript
createAlias :: forall eff. CreateAliasRequest -> Aff (err :: RequestError | eff) CreateAliasResponse
```

<p>Adds an alias to the set of a given member of Amazon WorkMail.</p>

#### `createGroup`

``` purescript
createGroup :: forall eff. CreateGroupRequest -> Aff (err :: RequestError | eff) CreateGroupResponse
```

<p>Creates a group that can be used in Amazon WorkMail by calling the RegisterToWorkMail operation.</p>

#### `createResource`

``` purescript
createResource :: forall eff. CreateResourceRequest -> Aff (err :: RequestError | eff) CreateResourceResponse
```

<p>Creates a new Amazon WorkMail resource. The available types are equipment and room.</p>

#### `createUser`

``` purescript
createUser :: forall eff. CreateUserRequest -> Aff (err :: RequestError | eff) CreateUserResponse
```

<p>Creates a user who can be used in Amazon WorkMail by calling the RegisterToWorkMail operation.</p>

#### `deleteAlias`

``` purescript
deleteAlias :: forall eff. DeleteAliasRequest -> Aff (err :: RequestError | eff) DeleteAliasResponse
```

<p>Remove the alias from a set of aliases for a given user.</p>

#### `deleteGroup`

``` purescript
deleteGroup :: forall eff. DeleteGroupRequest -> Aff (err :: RequestError | eff) DeleteGroupResponse
```

<p>Deletes a group from Amazon WorkMail.</p>

#### `deleteResource`

``` purescript
deleteResource :: forall eff. DeleteResourceRequest -> Aff (err :: RequestError | eff) DeleteResourceResponse
```

<p>Deletes the specified resource. </p>

#### `deleteUser`

``` purescript
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: RequestError | eff) DeleteUserResponse
```

<p>Deletes a user from Amazon WorkMail and all subsequent systems. The action can't be undone. The mailbox is kept as-is for a minimum of 30 days, without any means to restore it. </p>

#### `deregisterFromWorkMail`

``` purescript
deregisterFromWorkMail :: forall eff. DeregisterFromWorkMailRequest -> Aff (err :: RequestError | eff) DeregisterFromWorkMailResponse
```

<p>Mark a user, group, or resource as no longer used in Amazon WorkMail. This action disassociates the mailbox and schedules it for clean-up. Amazon WorkMail keeps mailboxes for 30 days before they are permanently removed. The functionality in the console is <i>Disable</i>.</p>

#### `describeGroup`

``` purescript
describeGroup :: forall eff. DescribeGroupRequest -> Aff (err :: RequestError | eff) DescribeGroupResponse
```

<p>Returns the data available for the group.</p>

#### `describeOrganization`

``` purescript
describeOrganization :: forall eff. DescribeOrganizationRequest -> Aff (err :: RequestError | eff) DescribeOrganizationResponse
```

<p>Provides more information regarding a given organization based on its identifier.</p>

#### `describeResource`

``` purescript
describeResource :: forall eff. DescribeResourceRequest -> Aff (err :: RequestError | eff) DescribeResourceResponse
```

<p>Returns the data available for the resource.</p>

#### `describeUser`

``` purescript
describeUser :: forall eff. DescribeUserRequest -> Aff (err :: RequestError | eff) DescribeUserResponse
```

<p>Provides information regarding the user.</p>

#### `disassociateDelegateFromResource`

``` purescript
disassociateDelegateFromResource :: forall eff. DisassociateDelegateFromResourceRequest -> Aff (err :: RequestError | eff) DisassociateDelegateFromResourceResponse
```

<p>Removes a member from the resource's set of delegates.</p>

#### `disassociateMemberFromGroup`

``` purescript
disassociateMemberFromGroup :: forall eff. DisassociateMemberFromGroupRequest -> Aff (err :: RequestError | eff) DisassociateMemberFromGroupResponse
```

<p>Removes a member from a group.</p>

#### `listAliases`

``` purescript
listAliases :: forall eff. ListAliasesRequest -> Aff (err :: RequestError | eff) ListAliasesResponse
```

<p>Creates a paginated call to list the aliases associated with a given entity.</p>

#### `listGroupMembers`

``` purescript
listGroupMembers :: forall eff. ListGroupMembersRequest -> Aff (err :: RequestError | eff) ListGroupMembersResponse
```

<p>Returns an overview of the members of a group.</p>

#### `listGroups`

``` purescript
listGroups :: forall eff. ListGroupsRequest -> Aff (err :: RequestError | eff) ListGroupsResponse
```

<p>Returns summaries of the organization's groups.</p>

#### `listOrganizations`

``` purescript
listOrganizations :: forall eff. ListOrganizationsRequest -> Aff (err :: RequestError | eff) ListOrganizationsResponse
```

<p>Returns summaries of the customer's non-deleted organizations.</p>

#### `listResourceDelegates`

``` purescript
listResourceDelegates :: forall eff. ListResourceDelegatesRequest -> Aff (err :: RequestError | eff) ListResourceDelegatesResponse
```

<p>Lists the delegates associated with a resource. Users and groups can be resource delegates and answer requests on behalf of the resource.</p>

#### `listResources`

``` purescript
listResources :: forall eff. ListResourcesRequest -> Aff (err :: RequestError | eff) ListResourcesResponse
```

<p>Returns summaries of the organization's resources.</p>

#### `listUsers`

``` purescript
listUsers :: forall eff. ListUsersRequest -> Aff (err :: RequestError | eff) ListUsersResponse
```

<p>Returns summaries of the organization's users.</p>

#### `registerToWorkMail`

``` purescript
registerToWorkMail :: forall eff. RegisterToWorkMailRequest -> Aff (err :: RequestError | eff) RegisterToWorkMailResponse
```

<p>Registers an existing and disabled user, group, or resource/entity for Amazon WorkMail use by associating a mailbox and calendaring capabilities. It performs no change if the entity is enabled and fails if the entity is deleted. This operation results in the accumulation of costs. For more information, see <a href="http://aws.amazon.com/workmail/pricing">Pricing</a>. The equivalent console functionality for this operation is <i>Enable</i>. Users can either be created by calling the CreateUser API or they can be synchronized from your directory. For more information, see DeregisterFromWorkMail.</p>

#### `resetPassword`

``` purescript
resetPassword :: forall eff. ResetPasswordRequest -> Aff (err :: RequestError | eff) ResetPasswordResponse
```

<p>Allows the administrator to reset the password for a user.</p>

#### `updatePrimaryEmailAddress`

``` purescript
updatePrimaryEmailAddress :: forall eff. UpdatePrimaryEmailAddressRequest -> Aff (err :: RequestError | eff) UpdatePrimaryEmailAddressResponse
```

<p>Updates the primary email for an entity. The current email is moved into the list of aliases (or swapped between an existing alias and the current primary email) and the email provided in the input is promoted as the primary.</p>

#### `updateResource`

``` purescript
updateResource :: forall eff. UpdateResourceRequest -> Aff (err :: RequestError | eff) UpdateResourceResponse
```

<p>Updates data for the resource. It must be preceded by a describe call in order to have the latest information. The dataset in the request should be the one expected when performing another describe call.</p>

#### `Aliases`

``` purescript
newtype Aliases
  = Aliases (Array EmailAddress)
```

##### Instances
``` purescript
Newtype Aliases _
```

#### `AssociateDelegateToResourceRequest`

``` purescript
newtype AssociateDelegateToResourceRequest
  = AssociateDelegateToResourceRequest { "OrganizationId" :: OrganizationId, "ResourceId" :: ResourceId, "EntityId" :: WorkMailIdentifier }
```

##### Instances
``` purescript
Newtype AssociateDelegateToResourceRequest _
```

#### `AssociateDelegateToResourceResponse`

``` purescript
newtype AssociateDelegateToResourceResponse
  = AssociateDelegateToResourceResponse {  }
```

##### Instances
``` purescript
Newtype AssociateDelegateToResourceResponse _
```

#### `AssociateMemberToGroupRequest`

``` purescript
newtype AssociateMemberToGroupRequest
  = AssociateMemberToGroupRequest { "OrganizationId" :: OrganizationId, "GroupId" :: WorkMailIdentifier, "MemberId" :: WorkMailIdentifier }
```

##### Instances
``` purescript
Newtype AssociateMemberToGroupRequest _
```

#### `AssociateMemberToGroupResponse`

``` purescript
newtype AssociateMemberToGroupResponse
  = AssociateMemberToGroupResponse {  }
```

##### Instances
``` purescript
Newtype AssociateMemberToGroupResponse _
```

#### `BookingOptions`

``` purescript
newtype BookingOptions
  = BookingOptions { "AutoAcceptRequests" :: NullOrUndefined (Boolean), "AutoDeclineRecurringRequests" :: NullOrUndefined (Boolean), "AutoDeclineConflictingRequests" :: NullOrUndefined (Boolean) }
```

<p>At least one delegate must be associated to the resource to disable automatic replies from the resource.</p>

##### Instances
``` purescript
Newtype BookingOptions _
```

#### `CreateAliasRequest`

``` purescript
newtype CreateAliasRequest
  = CreateAliasRequest { "OrganizationId" :: OrganizationId, "EntityId" :: WorkMailIdentifier, "Alias" :: EmailAddress }
```

##### Instances
``` purescript
Newtype CreateAliasRequest _
```

#### `CreateAliasResponse`

``` purescript
newtype CreateAliasResponse
  = CreateAliasResponse {  }
```

##### Instances
``` purescript
Newtype CreateAliasResponse _
```

#### `CreateGroupRequest`

``` purescript
newtype CreateGroupRequest
  = CreateGroupRequest { "OrganizationId" :: OrganizationId, "Name" :: GroupName }
```

##### Instances
``` purescript
Newtype CreateGroupRequest _
```

#### `CreateGroupResponse`

``` purescript
newtype CreateGroupResponse
  = CreateGroupResponse { "GroupId" :: NullOrUndefined (WorkMailIdentifier) }
```

##### Instances
``` purescript
Newtype CreateGroupResponse _
```

#### `CreateResourceRequest`

``` purescript
newtype CreateResourceRequest
  = CreateResourceRequest { "OrganizationId" :: OrganizationId, "Name" :: ResourceName, "Type" :: ResourceType }
```

##### Instances
``` purescript
Newtype CreateResourceRequest _
```

#### `CreateResourceResponse`

``` purescript
newtype CreateResourceResponse
  = CreateResourceResponse { "ResourceId" :: NullOrUndefined (ResourceId) }
```

##### Instances
``` purescript
Newtype CreateResourceResponse _
```

#### `CreateUserRequest`

``` purescript
newtype CreateUserRequest
  = CreateUserRequest { "OrganizationId" :: OrganizationId, "Name" :: UserName, "DisplayName" :: String, "Password" :: Password }
```

##### Instances
``` purescript
Newtype CreateUserRequest _
```

#### `CreateUserResponse`

``` purescript
newtype CreateUserResponse
  = CreateUserResponse { "UserId" :: NullOrUndefined (WorkMailIdentifier) }
```

##### Instances
``` purescript
Newtype CreateUserResponse _
```

#### `Delegate`

``` purescript
newtype Delegate
  = Delegate { "Id" :: String, "Type" :: MemberType }
```

<p>The name of the attribute, which is one of the values defined in the UserAttribute enumeration.</p>

##### Instances
``` purescript
Newtype Delegate _
```

#### `DeleteAliasRequest`

``` purescript
newtype DeleteAliasRequest
  = DeleteAliasRequest { "OrganizationId" :: OrganizationId, "EntityId" :: WorkMailIdentifier, "Alias" :: EmailAddress }
```

##### Instances
``` purescript
Newtype DeleteAliasRequest _
```

#### `DeleteAliasResponse`

``` purescript
newtype DeleteAliasResponse
  = DeleteAliasResponse {  }
```

##### Instances
``` purescript
Newtype DeleteAliasResponse _
```

#### `DeleteGroupRequest`

``` purescript
newtype DeleteGroupRequest
  = DeleteGroupRequest { "OrganizationId" :: OrganizationId, "GroupId" :: WorkMailIdentifier }
```

##### Instances
``` purescript
Newtype DeleteGroupRequest _
```

#### `DeleteGroupResponse`

``` purescript
newtype DeleteGroupResponse
  = DeleteGroupResponse {  }
```

##### Instances
``` purescript
Newtype DeleteGroupResponse _
```

#### `DeleteResourceRequest`

``` purescript
newtype DeleteResourceRequest
  = DeleteResourceRequest { "OrganizationId" :: OrganizationId, "ResourceId" :: ResourceId }
```

##### Instances
``` purescript
Newtype DeleteResourceRequest _
```

#### `DeleteResourceResponse`

``` purescript
newtype DeleteResourceResponse
  = DeleteResourceResponse {  }
```

##### Instances
``` purescript
Newtype DeleteResourceResponse _
```

#### `DeleteUserRequest`

``` purescript
newtype DeleteUserRequest
  = DeleteUserRequest { "OrganizationId" :: OrganizationId, "UserId" :: WorkMailIdentifier }
```

##### Instances
``` purescript
Newtype DeleteUserRequest _
```

#### `DeleteUserResponse`

``` purescript
newtype DeleteUserResponse
  = DeleteUserResponse {  }
```

##### Instances
``` purescript
Newtype DeleteUserResponse _
```

#### `DeregisterFromWorkMailRequest`

``` purescript
newtype DeregisterFromWorkMailRequest
  = DeregisterFromWorkMailRequest { "OrganizationId" :: OrganizationId, "EntityId" :: WorkMailIdentifier }
```

##### Instances
``` purescript
Newtype DeregisterFromWorkMailRequest _
```

#### `DeregisterFromWorkMailResponse`

``` purescript
newtype DeregisterFromWorkMailResponse
  = DeregisterFromWorkMailResponse {  }
```

##### Instances
``` purescript
Newtype DeregisterFromWorkMailResponse _
```

#### `DescribeGroupRequest`

``` purescript
newtype DescribeGroupRequest
  = DescribeGroupRequest { "OrganizationId" :: OrganizationId, "GroupId" :: WorkMailIdentifier }
```

##### Instances
``` purescript
Newtype DescribeGroupRequest _
```

#### `DescribeGroupResponse`

``` purescript
newtype DescribeGroupResponse
  = DescribeGroupResponse { "GroupId" :: NullOrUndefined (WorkMailIdentifier), "Name" :: NullOrUndefined (GroupName), "Email" :: NullOrUndefined (EmailAddress), "State" :: NullOrUndefined (EntityState), "EnabledDate" :: NullOrUndefined (Number), "DisabledDate" :: NullOrUndefined (Number) }
```

##### Instances
``` purescript
Newtype DescribeGroupResponse _
```

#### `DescribeOrganizationRequest`

``` purescript
newtype DescribeOrganizationRequest
  = DescribeOrganizationRequest { "OrganizationId" :: OrganizationId }
```

##### Instances
``` purescript
Newtype DescribeOrganizationRequest _
```

#### `DescribeOrganizationResponse`

``` purescript
newtype DescribeOrganizationResponse
  = DescribeOrganizationResponse { "OrganizationId" :: NullOrUndefined (OrganizationId), "Alias" :: NullOrUndefined (OrganizationName), "State" :: NullOrUndefined (String), "DirectoryId" :: NullOrUndefined (String), "DirectoryType" :: NullOrUndefined (String), "DefaultMailDomain" :: NullOrUndefined (String), "CompletedDate" :: NullOrUndefined (Number), "ErrorMessage" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeOrganizationResponse _
```

#### `DescribeResourceRequest`

``` purescript
newtype DescribeResourceRequest
  = DescribeResourceRequest { "OrganizationId" :: OrganizationId, "ResourceId" :: ResourceId }
```

##### Instances
``` purescript
Newtype DescribeResourceRequest _
```

#### `DescribeResourceResponse`

``` purescript
newtype DescribeResourceResponse
  = DescribeResourceResponse { "ResourceId" :: NullOrUndefined (ResourceId), "Email" :: NullOrUndefined (EmailAddress), "Name" :: NullOrUndefined (ResourceName), "Type" :: NullOrUndefined (ResourceType), "BookingOptions" :: NullOrUndefined (BookingOptions), "State" :: NullOrUndefined (EntityState), "EnabledDate" :: NullOrUndefined (Number), "DisabledDate" :: NullOrUndefined (Number) }
```

##### Instances
``` purescript
Newtype DescribeResourceResponse _
```

#### `DescribeUserRequest`

``` purescript
newtype DescribeUserRequest
  = DescribeUserRequest { "OrganizationId" :: OrganizationId, "UserId" :: WorkMailIdentifier }
```

##### Instances
``` purescript
Newtype DescribeUserRequest _
```

#### `DescribeUserResponse`

``` purescript
newtype DescribeUserResponse
  = DescribeUserResponse { "UserId" :: NullOrUndefined (WorkMailIdentifier), "Name" :: NullOrUndefined (UserName), "Email" :: NullOrUndefined (EmailAddress), "DisplayName" :: NullOrUndefined (String), "State" :: NullOrUndefined (EntityState), "UserRole" :: NullOrUndefined (UserRole), "EnabledDate" :: NullOrUndefined (Number), "DisabledDate" :: NullOrUndefined (Number) }
```

##### Instances
``` purescript
Newtype DescribeUserResponse _
```

#### `DirectoryServiceAuthenticationFailedException`

``` purescript
newtype DirectoryServiceAuthenticationFailedException
  = DirectoryServiceAuthenticationFailedException { "Message" :: NullOrUndefined (String) }
```

<p>The Directory Service doesn't recognize the credentials supplied by the Amazon WorkMail service.</p>

##### Instances
``` purescript
Newtype DirectoryServiceAuthenticationFailedException _
```

#### `DirectoryUnavailableException`

``` purescript
newtype DirectoryUnavailableException
  = DirectoryUnavailableException { "Message" :: NullOrUndefined (String) }
```

<p>The directory that you are trying to perform operations on isn't available.</p>

##### Instances
``` purescript
Newtype DirectoryUnavailableException _
```

#### `DisassociateDelegateFromResourceRequest`

``` purescript
newtype DisassociateDelegateFromResourceRequest
  = DisassociateDelegateFromResourceRequest { "OrganizationId" :: OrganizationId, "ResourceId" :: ResourceId, "EntityId" :: WorkMailIdentifier }
```

##### Instances
``` purescript
Newtype DisassociateDelegateFromResourceRequest _
```

#### `DisassociateDelegateFromResourceResponse`

``` purescript
newtype DisassociateDelegateFromResourceResponse
  = DisassociateDelegateFromResourceResponse {  }
```

##### Instances
``` purescript
Newtype DisassociateDelegateFromResourceResponse _
```

#### `DisassociateMemberFromGroupRequest`

``` purescript
newtype DisassociateMemberFromGroupRequest
  = DisassociateMemberFromGroupRequest { "OrganizationId" :: OrganizationId, "GroupId" :: WorkMailIdentifier, "MemberId" :: WorkMailIdentifier }
```

##### Instances
``` purescript
Newtype DisassociateMemberFromGroupRequest _
```

#### `DisassociateMemberFromGroupResponse`

``` purescript
newtype DisassociateMemberFromGroupResponse
  = DisassociateMemberFromGroupResponse {  }
```

##### Instances
``` purescript
Newtype DisassociateMemberFromGroupResponse _
```

#### `EmailAddress`

``` purescript
newtype EmailAddress
  = EmailAddress String
```

##### Instances
``` purescript
Newtype EmailAddress _
```

#### `EmailAddressInUseException`

``` purescript
newtype EmailAddressInUseException
  = EmailAddressInUseException { "Message" :: NullOrUndefined (String) }
```

<p>The email address that you're trying to assign is already created for a different user, group, or resource.</p>

##### Instances
``` purescript
Newtype EmailAddressInUseException _
```

#### `EntityAlreadyRegisteredException`

``` purescript
newtype EntityAlreadyRegisteredException
  = EntityAlreadyRegisteredException { "Message" :: NullOrUndefined (String) }
```

<p>The user, group, or resource that you're trying to register is already registered.</p>

##### Instances
``` purescript
Newtype EntityAlreadyRegisteredException _
```

#### `EntityNotFoundException`

``` purescript
newtype EntityNotFoundException
  = EntityNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>The identifier supplied for the entity is valid, but it does not exist in your organization.</p>

##### Instances
``` purescript
Newtype EntityNotFoundException _
```

#### `EntityState`

``` purescript
newtype EntityState
  = EntityState String
```

##### Instances
``` purescript
Newtype EntityState _
```

#### `EntityStateException`

``` purescript
newtype EntityStateException
  = EntityStateException { "Message" :: NullOrUndefined (String) }
```

<p>You are performing an operation on an entity that isn't in the expected state, such as trying to update a deleted user.</p>

##### Instances
``` purescript
Newtype EntityStateException _
```

#### `Group`

``` purescript
newtype Group
  = Group { "Id" :: NullOrUndefined (WorkMailIdentifier), "Email" :: NullOrUndefined (EmailAddress), "Name" :: NullOrUndefined (GroupName), "State" :: NullOrUndefined (EntityState), "EnabledDate" :: NullOrUndefined (Number), "DisabledDate" :: NullOrUndefined (Number) }
```

<p>The representation of an Amazon WorkMail group.</p>

##### Instances
``` purescript
Newtype Group _
```

#### `GroupName`

``` purescript
newtype GroupName
  = GroupName String
```

##### Instances
``` purescript
Newtype GroupName _
```

#### `Groups`

``` purescript
newtype Groups
  = Groups (Array Group)
```

##### Instances
``` purescript
Newtype Groups _
```

#### `InvalidConfigurationException`

``` purescript
newtype InvalidConfigurationException
  = InvalidConfigurationException { "Message" :: NullOrUndefined (String) }
```

<p>The configuration for a resource isn't valid. A resource must either be able to auto-respond to requests or have at least one delegate associated that can do it on its behalf.</p>

##### Instances
``` purescript
Newtype InvalidConfigurationException _
```

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message" :: NullOrUndefined (String) }
```

<p>One or more of the input parameters don't match the service's restrictions.</p>

##### Instances
``` purescript
Newtype InvalidParameterException _
```

#### `InvalidPasswordException`

``` purescript
newtype InvalidPasswordException
  = InvalidPasswordException { "Message" :: NullOrUndefined (String) }
```

<p>The supplied password doesn't match the minimum security constraints, such as length or use of special characters.</p>

##### Instances
``` purescript
Newtype InvalidPasswordException _
```

#### `ListAliasesRequest`

``` purescript
newtype ListAliasesRequest
  = ListAliasesRequest { "OrganizationId" :: OrganizationId, "EntityId" :: WorkMailIdentifier, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListAliasesRequest _
```

#### `ListAliasesResponse`

``` purescript
newtype ListAliasesResponse
  = ListAliasesResponse { "Aliases" :: NullOrUndefined (Aliases), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListAliasesResponse _
```

#### `ListGroupMembersRequest`

``` purescript
newtype ListGroupMembersRequest
  = ListGroupMembersRequest { "OrganizationId" :: OrganizationId, "GroupId" :: WorkMailIdentifier, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListGroupMembersRequest _
```

#### `ListGroupMembersResponse`

``` purescript
newtype ListGroupMembersResponse
  = ListGroupMembersResponse { "Members" :: NullOrUndefined (Members), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListGroupMembersResponse _
```

#### `ListGroupsRequest`

``` purescript
newtype ListGroupsRequest
  = ListGroupsRequest { "OrganizationId" :: OrganizationId, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListGroupsRequest _
```

#### `ListGroupsResponse`

``` purescript
newtype ListGroupsResponse
  = ListGroupsResponse { "Groups" :: NullOrUndefined (Groups), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListGroupsResponse _
```

#### `ListOrganizationsRequest`

``` purescript
newtype ListOrganizationsRequest
  = ListOrganizationsRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListOrganizationsRequest _
```

#### `ListOrganizationsResponse`

``` purescript
newtype ListOrganizationsResponse
  = ListOrganizationsResponse { "OrganizationSummaries" :: NullOrUndefined (OrganizationSummaries), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListOrganizationsResponse _
```

#### `ListResourceDelegatesRequest`

``` purescript
newtype ListResourceDelegatesRequest
  = ListResourceDelegatesRequest { "OrganizationId" :: OrganizationId, "ResourceId" :: WorkMailIdentifier, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListResourceDelegatesRequest _
```

#### `ListResourceDelegatesResponse`

``` purescript
newtype ListResourceDelegatesResponse
  = ListResourceDelegatesResponse { "Delegates" :: NullOrUndefined (ResourceDelegates), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListResourceDelegatesResponse _
```

#### `ListResourcesRequest`

``` purescript
newtype ListResourcesRequest
  = ListResourcesRequest { "OrganizationId" :: OrganizationId, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListResourcesRequest _
```

#### `ListResourcesResponse`

``` purescript
newtype ListResourcesResponse
  = ListResourcesResponse { "Resources" :: NullOrUndefined (Resources), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListResourcesResponse _
```

#### `ListUsersRequest`

``` purescript
newtype ListUsersRequest
  = ListUsersRequest { "OrganizationId" :: OrganizationId, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListUsersRequest _
```

#### `ListUsersResponse`

``` purescript
newtype ListUsersResponse
  = ListUsersResponse { "Users" :: NullOrUndefined (Users), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListUsersResponse _
```

#### `MailDomainNotFoundException`

``` purescript
newtype MailDomainNotFoundException
  = MailDomainNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>For an email or alias to be created in Amazon WorkMail, the included domain must be defined in the organization.</p>

##### Instances
``` purescript
Newtype MailDomainNotFoundException _
```

#### `MailDomainStateException`

``` purescript
newtype MailDomainStateException
  = MailDomainStateException { "Message" :: NullOrUndefined (String) }
```

<p>After a domain has been added to the organization, it must be verified. The domain is not yet verified.</p>

##### Instances
``` purescript
Newtype MailDomainStateException _
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

#### `Member`

``` purescript
newtype Member
  = Member { "Id" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "Type" :: NullOrUndefined (MemberType), "State" :: NullOrUndefined (EntityState), "EnabledDate" :: NullOrUndefined (Number), "DisabledDate" :: NullOrUndefined (Number) }
```

<p>The representation of a group member (user or group).</p>

##### Instances
``` purescript
Newtype Member _
```

#### `MemberType`

``` purescript
newtype MemberType
  = MemberType String
```

##### Instances
``` purescript
Newtype MemberType _
```

#### `Members`

``` purescript
newtype Members
  = Members (Array Member)
```

##### Instances
``` purescript
Newtype Members _
```

#### `NameAvailabilityException`

``` purescript
newtype NameAvailabilityException
  = NameAvailabilityException { "Message" :: NullOrUndefined (String) }
```

<p>The entity (user, group, or user) name isn't unique in Amazon WorkMail.</p>

##### Instances
``` purescript
Newtype NameAvailabilityException _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

##### Instances
``` purescript
Newtype NextToken _
```

#### `OrganizationId`

``` purescript
newtype OrganizationId
  = OrganizationId String
```

##### Instances
``` purescript
Newtype OrganizationId _
```

#### `OrganizationName`

``` purescript
newtype OrganizationName
  = OrganizationName String
```

##### Instances
``` purescript
Newtype OrganizationName _
```

#### `OrganizationNotFoundException`

``` purescript
newtype OrganizationNotFoundException
  = OrganizationNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>An operation received a valid organization identifier that either doesn't belong or exist in the system.</p>

##### Instances
``` purescript
Newtype OrganizationNotFoundException _
```

#### `OrganizationStateException`

``` purescript
newtype OrganizationStateException
  = OrganizationStateException { "Message" :: NullOrUndefined (String) }
```

<p>The organization must have a valid state (Active or Synchronizing) to perform certain operations on the organization or its entities.</p>

##### Instances
``` purescript
Newtype OrganizationStateException _
```

#### `OrganizationSummaries`

``` purescript
newtype OrganizationSummaries
  = OrganizationSummaries (Array OrganizationSummary)
```

##### Instances
``` purescript
Newtype OrganizationSummaries _
```

#### `OrganizationSummary`

``` purescript
newtype OrganizationSummary
  = OrganizationSummary { "OrganizationId" :: NullOrUndefined (OrganizationId), "Alias" :: NullOrUndefined (OrganizationName), "ErrorMessage" :: NullOrUndefined (String), "State" :: NullOrUndefined (String) }
```

<p>The brief overview associated with an organization.</p>

##### Instances
``` purescript
Newtype OrganizationSummary _
```

#### `Password`

``` purescript
newtype Password
  = Password String
```

##### Instances
``` purescript
Newtype Password _
```

#### `RegisterToWorkMailRequest`

``` purescript
newtype RegisterToWorkMailRequest
  = RegisterToWorkMailRequest { "OrganizationId" :: OrganizationId, "EntityId" :: WorkMailIdentifier, "Email" :: EmailAddress }
```

##### Instances
``` purescript
Newtype RegisterToWorkMailRequest _
```

#### `RegisterToWorkMailResponse`

``` purescript
newtype RegisterToWorkMailResponse
  = RegisterToWorkMailResponse {  }
```

##### Instances
``` purescript
Newtype RegisterToWorkMailResponse _
```

#### `ReservedNameException`

``` purescript
newtype ReservedNameException
  = ReservedNameException { "Message" :: NullOrUndefined (String) }
```

<p>This entity name is not allowed in Amazon WorkMail.</p>

##### Instances
``` purescript
Newtype ReservedNameException _
```

#### `ResetPasswordRequest`

``` purescript
newtype ResetPasswordRequest
  = ResetPasswordRequest { "OrganizationId" :: OrganizationId, "UserId" :: WorkMailIdentifier, "Password" :: Password }
```

##### Instances
``` purescript
Newtype ResetPasswordRequest _
```

#### `ResetPasswordResponse`

``` purescript
newtype ResetPasswordResponse
  = ResetPasswordResponse {  }
```

##### Instances
``` purescript
Newtype ResetPasswordResponse _
```

#### `Resource`

``` purescript
newtype Resource
  = Resource { "Id" :: NullOrUndefined (WorkMailIdentifier), "Email" :: NullOrUndefined (EmailAddress), "Name" :: NullOrUndefined (ResourceName), "Type" :: NullOrUndefined (ResourceType), "State" :: NullOrUndefined (EntityState), "EnabledDate" :: NullOrUndefined (Number), "DisabledDate" :: NullOrUndefined (Number) }
```

<p>The overview for a resource containing relevant data regarding it.</p>

##### Instances
``` purescript
Newtype Resource _
```

#### `ResourceDelegates`

``` purescript
newtype ResourceDelegates
  = ResourceDelegates (Array Delegate)
```

##### Instances
``` purescript
Newtype ResourceDelegates _
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

#### `ResourceName`

``` purescript
newtype ResourceName
  = ResourceName String
```

##### Instances
``` purescript
Newtype ResourceName _
```

#### `ResourceType`

``` purescript
newtype ResourceType
  = ResourceType String
```

##### Instances
``` purescript
Newtype ResourceType _
```

#### `Resources`

``` purescript
newtype Resources
  = Resources (Array Resource)
```

##### Instances
``` purescript
Newtype Resources _
```

#### `UnsupportedOperationException`

``` purescript
newtype UnsupportedOperationException
  = UnsupportedOperationException { "Message" :: NullOrUndefined (String) }
```

<p>You can't perform a write operation against a read-only directory.</p>

##### Instances
``` purescript
Newtype UnsupportedOperationException _
```

#### `UpdatePrimaryEmailAddressRequest`

``` purescript
newtype UpdatePrimaryEmailAddressRequest
  = UpdatePrimaryEmailAddressRequest { "OrganizationId" :: OrganizationId, "EntityId" :: WorkMailIdentifier, "Email" :: EmailAddress }
```

##### Instances
``` purescript
Newtype UpdatePrimaryEmailAddressRequest _
```

#### `UpdatePrimaryEmailAddressResponse`

``` purescript
newtype UpdatePrimaryEmailAddressResponse
  = UpdatePrimaryEmailAddressResponse {  }
```

##### Instances
``` purescript
Newtype UpdatePrimaryEmailAddressResponse _
```

#### `UpdateResourceRequest`

``` purescript
newtype UpdateResourceRequest
  = UpdateResourceRequest { "OrganizationId" :: OrganizationId, "ResourceId" :: ResourceId, "Name" :: NullOrUndefined (ResourceName), "BookingOptions" :: NullOrUndefined (BookingOptions) }
```

##### Instances
``` purescript
Newtype UpdateResourceRequest _
```

#### `UpdateResourceResponse`

``` purescript
newtype UpdateResourceResponse
  = UpdateResourceResponse {  }
```

##### Instances
``` purescript
Newtype UpdateResourceResponse _
```

#### `User`

``` purescript
newtype User
  = User { "Id" :: NullOrUndefined (WorkMailIdentifier), "Email" :: NullOrUndefined (EmailAddress), "Name" :: NullOrUndefined (UserName), "DisplayName" :: NullOrUndefined (String), "State" :: NullOrUndefined (EntityState), "UserRole" :: NullOrUndefined (UserRole), "EnabledDate" :: NullOrUndefined (Number), "DisabledDate" :: NullOrUndefined (Number) }
```

<p>The representation of an Amazon WorkMail user.</p>

##### Instances
``` purescript
Newtype User _
```

#### `UserName`

``` purescript
newtype UserName
  = UserName String
```

##### Instances
``` purescript
Newtype UserName _
```

#### `UserRole`

``` purescript
newtype UserRole
  = UserRole String
```

##### Instances
``` purescript
Newtype UserRole _
```

#### `Users`

``` purescript
newtype Users
  = Users (Array User)
```

##### Instances
``` purescript
Newtype Users _
```

#### `WorkMailIdentifier`

``` purescript
newtype WorkMailIdentifier
  = WorkMailIdentifier String
```

##### Instances
``` purescript
Newtype WorkMailIdentifier _
```


