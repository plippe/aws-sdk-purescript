

-- | <p>Amazon WorkMail is a secure, managed business email and calendaring service with support for existing desktop and mobile email clients. You can access your email, contacts, and calendars using Microsoft Outlook, your browser, or their native iOS and Android email applications. You can integrate Amazon WorkMail with your existing corporate directory and control both the keys that encrypt your data and the location in which your data is stored.</p> <p>The Amazon WorkMail API is designed for the following scenarios:</p> <ul> <li> <p>Listing and describing organizations</p> </li> </ul> <ul> <li> <p>Managing users</p> </li> </ul> <ul> <li> <p>Managing groups</p> </li> </ul> <ul> <li> <p>Managing resources</p> </li> </ul> <p>All Amazon WorkMail API actions are Amazon-authenticated and certificate-signed. They not only require the use of the AWS SDK, but also allow for the exclusive use of IAM users and roles to help facilitate access, trust, and permission policies. By creating a role and allowing an IAM user to access the Amazon WorkMail site, the IAM user gains full administrative visibility into the entire Amazon WorkMail organization (or as set in the IAM policy). This includes, but is not limited to, the ability to create, update, and delete users, groups, and resources. This allows developers to perform the scenarios listed above, as well as give users the ability to grant access on a selective basis using the IAM model.</p>
module AWS.WorkMail where

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

serviceName = "WorkMail" :: String


-- | <p>Adds a member to the resource's set of delegates.</p>
associateDelegateToResource :: forall eff. AssociateDelegateToResourceRequest -> Aff (exception :: EXCEPTION | eff) AssociateDelegateToResourceResponse
associateDelegateToResource = Request.request serviceName "associateDelegateToResource" 


-- | <p>Adds a member to the group's set.</p>
associateMemberToGroup :: forall eff. AssociateMemberToGroupRequest -> Aff (exception :: EXCEPTION | eff) AssociateMemberToGroupResponse
associateMemberToGroup = Request.request serviceName "associateMemberToGroup" 


-- | <p>Adds an alias to the set of a given member of Amazon WorkMail.</p>
createAlias :: forall eff. CreateAliasRequest -> Aff (exception :: EXCEPTION | eff) CreateAliasResponse
createAlias = Request.request serviceName "createAlias" 


-- | <p>Creates a group that can be used in Amazon WorkMail by calling the RegisterToWorkMail operation.</p>
createGroup :: forall eff. CreateGroupRequest -> Aff (exception :: EXCEPTION | eff) CreateGroupResponse
createGroup = Request.request serviceName "createGroup" 


-- | <p>Creates a new Amazon WorkMail resource. The available types are equipment and room.</p>
createResource :: forall eff. CreateResourceRequest -> Aff (exception :: EXCEPTION | eff) CreateResourceResponse
createResource = Request.request serviceName "createResource" 


-- | <p>Creates a user who can be used in Amazon WorkMail by calling the RegisterToWorkMail operation.</p>
createUser :: forall eff. CreateUserRequest -> Aff (exception :: EXCEPTION | eff) CreateUserResponse
createUser = Request.request serviceName "createUser" 


-- | <p>Remove the alias from a set of aliases for a given user.</p>
deleteAlias :: forall eff. DeleteAliasRequest -> Aff (exception :: EXCEPTION | eff) DeleteAliasResponse
deleteAlias = Request.request serviceName "deleteAlias" 


-- | <p>Deletes a group from Amazon WorkMail.</p>
deleteGroup :: forall eff. DeleteGroupRequest -> Aff (exception :: EXCEPTION | eff) DeleteGroupResponse
deleteGroup = Request.request serviceName "deleteGroup" 


-- | <p>Deletes the specified resource. </p>
deleteResource :: forall eff. DeleteResourceRequest -> Aff (exception :: EXCEPTION | eff) DeleteResourceResponse
deleteResource = Request.request serviceName "deleteResource" 


-- | <p>Deletes a user from Amazon WorkMail and all subsequent systems. The action can't be undone. The mailbox is kept as-is for a minimum of 30 days, without any means to restore it. </p>
deleteUser :: forall eff. DeleteUserRequest -> Aff (exception :: EXCEPTION | eff) DeleteUserResponse
deleteUser = Request.request serviceName "deleteUser" 


-- | <p>Mark a user, group, or resource as no longer used in Amazon WorkMail. This action disassociates the mailbox and schedules it for clean-up. Amazon WorkMail keeps mailboxes for 30 days before they are permanently removed. The functionality in the console is <i>Disable</i>.</p>
deregisterFromWorkMail :: forall eff. DeregisterFromWorkMailRequest -> Aff (exception :: EXCEPTION | eff) DeregisterFromWorkMailResponse
deregisterFromWorkMail = Request.request serviceName "deregisterFromWorkMail" 


-- | <p>Returns the data available for the group.</p>
describeGroup :: forall eff. DescribeGroupRequest -> Aff (exception :: EXCEPTION | eff) DescribeGroupResponse
describeGroup = Request.request serviceName "describeGroup" 


-- | <p>Provides more information regarding a given organization based on its identifier.</p>
describeOrganization :: forall eff. DescribeOrganizationRequest -> Aff (exception :: EXCEPTION | eff) DescribeOrganizationResponse
describeOrganization = Request.request serviceName "describeOrganization" 


-- | <p>Returns the data available for the resource.</p>
describeResource :: forall eff. DescribeResourceRequest -> Aff (exception :: EXCEPTION | eff) DescribeResourceResponse
describeResource = Request.request serviceName "describeResource" 


-- | <p>Provides information regarding the user.</p>
describeUser :: forall eff. DescribeUserRequest -> Aff (exception :: EXCEPTION | eff) DescribeUserResponse
describeUser = Request.request serviceName "describeUser" 


-- | <p>Removes a member from the resource's set of delegates.</p>
disassociateDelegateFromResource :: forall eff. DisassociateDelegateFromResourceRequest -> Aff (exception :: EXCEPTION | eff) DisassociateDelegateFromResourceResponse
disassociateDelegateFromResource = Request.request serviceName "disassociateDelegateFromResource" 


-- | <p>Removes a member from a group.</p>
disassociateMemberFromGroup :: forall eff. DisassociateMemberFromGroupRequest -> Aff (exception :: EXCEPTION | eff) DisassociateMemberFromGroupResponse
disassociateMemberFromGroup = Request.request serviceName "disassociateMemberFromGroup" 


-- | <p>Creates a paginated call to list the aliases associated with a given entity.</p>
listAliases :: forall eff. ListAliasesRequest -> Aff (exception :: EXCEPTION | eff) ListAliasesResponse
listAliases = Request.request serviceName "listAliases" 


-- | <p>Returns an overview of the members of a group.</p>
listGroupMembers :: forall eff. ListGroupMembersRequest -> Aff (exception :: EXCEPTION | eff) ListGroupMembersResponse
listGroupMembers = Request.request serviceName "listGroupMembers" 


-- | <p>Returns summaries of the organization's groups.</p>
listGroups :: forall eff. ListGroupsRequest -> Aff (exception :: EXCEPTION | eff) ListGroupsResponse
listGroups = Request.request serviceName "listGroups" 


-- | <p>Returns summaries of the customer's non-deleted organizations.</p>
listOrganizations :: forall eff. ListOrganizationsRequest -> Aff (exception :: EXCEPTION | eff) ListOrganizationsResponse
listOrganizations = Request.request serviceName "listOrganizations" 


-- | <p>Lists the delegates associated with a resource. Users and groups can be resource delegates and answer requests on behalf of the resource.</p>
listResourceDelegates :: forall eff. ListResourceDelegatesRequest -> Aff (exception :: EXCEPTION | eff) ListResourceDelegatesResponse
listResourceDelegates = Request.request serviceName "listResourceDelegates" 


-- | <p>Returns summaries of the organization's resources.</p>
listResources :: forall eff. ListResourcesRequest -> Aff (exception :: EXCEPTION | eff) ListResourcesResponse
listResources = Request.request serviceName "listResources" 


-- | <p>Returns summaries of the organization's users.</p>
listUsers :: forall eff. ListUsersRequest -> Aff (exception :: EXCEPTION | eff) ListUsersResponse
listUsers = Request.request serviceName "listUsers" 


-- | <p>Registers an existing and disabled user, group, or resource/entity for Amazon WorkMail use by associating a mailbox and calendaring capabilities. It performs no change if the entity is enabled and fails if the entity is deleted. This operation results in the accumulation of costs. For more information, see <a href="http://aws.amazon.com/workmail/pricing">Pricing</a>. The equivalent console functionality for this operation is <i>Enable</i>. Users can either be created by calling the CreateUser API or they can be synchronized from your directory. For more information, see DeregisterFromWorkMail.</p>
registerToWorkMail :: forall eff. RegisterToWorkMailRequest -> Aff (exception :: EXCEPTION | eff) RegisterToWorkMailResponse
registerToWorkMail = Request.request serviceName "registerToWorkMail" 


-- | <p>Allows the administrator to reset the password for a user.</p>
resetPassword :: forall eff. ResetPasswordRequest -> Aff (exception :: EXCEPTION | eff) ResetPasswordResponse
resetPassword = Request.request serviceName "resetPassword" 


-- | <p>Updates the primary email for an entity. The current email is moved into the list of aliases (or swapped between an existing alias and the current primary email) and the email provided in the input is promoted as the primary.</p>
updatePrimaryEmailAddress :: forall eff. UpdatePrimaryEmailAddressRequest -> Aff (exception :: EXCEPTION | eff) UpdatePrimaryEmailAddressResponse
updatePrimaryEmailAddress = Request.request serviceName "updatePrimaryEmailAddress" 


-- | <p>Updates data for the resource. It must be preceded by a describe call in order to have the latest information. The dataset in the request should be the one expected when performing another describe call.</p>
updateResource :: forall eff. UpdateResourceRequest -> Aff (exception :: EXCEPTION | eff) UpdateResourceResponse
updateResource = Request.request serviceName "updateResource" 


newtype Aliases = Aliases (Array EmailAddress)
derive instance newtypeAliases :: Newtype Aliases _
derive instance repGenericAliases :: Generic Aliases _
instance showAliases :: Show Aliases where
  show = genericShow
instance decodeAliases :: Decode Aliases where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliases :: Encode Aliases where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateDelegateToResourceRequest = AssociateDelegateToResourceRequest 
  { "OrganizationId" :: (OrganizationId)
  , "ResourceId" :: (ResourceId)
  , "EntityId" :: (WorkMailIdentifier)
  }
derive instance newtypeAssociateDelegateToResourceRequest :: Newtype AssociateDelegateToResourceRequest _
derive instance repGenericAssociateDelegateToResourceRequest :: Generic AssociateDelegateToResourceRequest _
instance showAssociateDelegateToResourceRequest :: Show AssociateDelegateToResourceRequest where
  show = genericShow
instance decodeAssociateDelegateToResourceRequest :: Decode AssociateDelegateToResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateDelegateToResourceRequest :: Encode AssociateDelegateToResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateDelegateToResourceResponse = AssociateDelegateToResourceResponse Types.NoArguments
derive instance newtypeAssociateDelegateToResourceResponse :: Newtype AssociateDelegateToResourceResponse _
derive instance repGenericAssociateDelegateToResourceResponse :: Generic AssociateDelegateToResourceResponse _
instance showAssociateDelegateToResourceResponse :: Show AssociateDelegateToResourceResponse where
  show = genericShow
instance decodeAssociateDelegateToResourceResponse :: Decode AssociateDelegateToResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateDelegateToResourceResponse :: Encode AssociateDelegateToResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateMemberToGroupRequest = AssociateMemberToGroupRequest 
  { "OrganizationId" :: (OrganizationId)
  , "GroupId" :: (WorkMailIdentifier)
  , "MemberId" :: (WorkMailIdentifier)
  }
derive instance newtypeAssociateMemberToGroupRequest :: Newtype AssociateMemberToGroupRequest _
derive instance repGenericAssociateMemberToGroupRequest :: Generic AssociateMemberToGroupRequest _
instance showAssociateMemberToGroupRequest :: Show AssociateMemberToGroupRequest where
  show = genericShow
instance decodeAssociateMemberToGroupRequest :: Decode AssociateMemberToGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateMemberToGroupRequest :: Encode AssociateMemberToGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateMemberToGroupResponse = AssociateMemberToGroupResponse Types.NoArguments
derive instance newtypeAssociateMemberToGroupResponse :: Newtype AssociateMemberToGroupResponse _
derive instance repGenericAssociateMemberToGroupResponse :: Generic AssociateMemberToGroupResponse _
instance showAssociateMemberToGroupResponse :: Show AssociateMemberToGroupResponse where
  show = genericShow
instance decodeAssociateMemberToGroupResponse :: Decode AssociateMemberToGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateMemberToGroupResponse :: Encode AssociateMemberToGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>At least one delegate must be associated to the resource to disable automatic replies from the resource.</p>
newtype BookingOptions = BookingOptions 
  { "AutoAcceptRequests" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "AutoDeclineRecurringRequests" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "AutoDeclineConflictingRequests" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeBookingOptions :: Newtype BookingOptions _
derive instance repGenericBookingOptions :: Generic BookingOptions _
instance showBookingOptions :: Show BookingOptions where
  show = genericShow
instance decodeBookingOptions :: Decode BookingOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBookingOptions :: Encode BookingOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateAliasRequest = CreateAliasRequest 
  { "OrganizationId" :: (OrganizationId)
  , "EntityId" :: (WorkMailIdentifier)
  , "Alias" :: (EmailAddress)
  }
derive instance newtypeCreateAliasRequest :: Newtype CreateAliasRequest _
derive instance repGenericCreateAliasRequest :: Generic CreateAliasRequest _
instance showCreateAliasRequest :: Show CreateAliasRequest where
  show = genericShow
instance decodeCreateAliasRequest :: Decode CreateAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAliasRequest :: Encode CreateAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateAliasResponse = CreateAliasResponse Types.NoArguments
derive instance newtypeCreateAliasResponse :: Newtype CreateAliasResponse _
derive instance repGenericCreateAliasResponse :: Generic CreateAliasResponse _
instance showCreateAliasResponse :: Show CreateAliasResponse where
  show = genericShow
instance decodeCreateAliasResponse :: Decode CreateAliasResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAliasResponse :: Encode CreateAliasResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGroupRequest = CreateGroupRequest 
  { "OrganizationId" :: (OrganizationId)
  , "Name" :: (GroupName)
  }
derive instance newtypeCreateGroupRequest :: Newtype CreateGroupRequest _
derive instance repGenericCreateGroupRequest :: Generic CreateGroupRequest _
instance showCreateGroupRequest :: Show CreateGroupRequest where
  show = genericShow
instance decodeCreateGroupRequest :: Decode CreateGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGroupRequest :: Encode CreateGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGroupResponse = CreateGroupResponse 
  { "GroupId" :: NullOrUndefined.NullOrUndefined (WorkMailIdentifier)
  }
derive instance newtypeCreateGroupResponse :: Newtype CreateGroupResponse _
derive instance repGenericCreateGroupResponse :: Generic CreateGroupResponse _
instance showCreateGroupResponse :: Show CreateGroupResponse where
  show = genericShow
instance decodeCreateGroupResponse :: Decode CreateGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGroupResponse :: Encode CreateGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateResourceRequest = CreateResourceRequest 
  { "OrganizationId" :: (OrganizationId)
  , "Name" :: (ResourceName)
  , "Type" :: (ResourceType)
  }
derive instance newtypeCreateResourceRequest :: Newtype CreateResourceRequest _
derive instance repGenericCreateResourceRequest :: Generic CreateResourceRequest _
instance showCreateResourceRequest :: Show CreateResourceRequest where
  show = genericShow
instance decodeCreateResourceRequest :: Decode CreateResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResourceRequest :: Encode CreateResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateResourceResponse = CreateResourceResponse 
  { "ResourceId" :: NullOrUndefined.NullOrUndefined (ResourceId)
  }
derive instance newtypeCreateResourceResponse :: Newtype CreateResourceResponse _
derive instance repGenericCreateResourceResponse :: Generic CreateResourceResponse _
instance showCreateResourceResponse :: Show CreateResourceResponse where
  show = genericShow
instance decodeCreateResourceResponse :: Decode CreateResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResourceResponse :: Encode CreateResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateUserRequest = CreateUserRequest 
  { "OrganizationId" :: (OrganizationId)
  , "Name" :: (UserName)
  , "DisplayName" :: (String)
  , "Password" :: (Password)
  }
derive instance newtypeCreateUserRequest :: Newtype CreateUserRequest _
derive instance repGenericCreateUserRequest :: Generic CreateUserRequest _
instance showCreateUserRequest :: Show CreateUserRequest where
  show = genericShow
instance decodeCreateUserRequest :: Decode CreateUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserRequest :: Encode CreateUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateUserResponse = CreateUserResponse 
  { "UserId" :: NullOrUndefined.NullOrUndefined (WorkMailIdentifier)
  }
derive instance newtypeCreateUserResponse :: Newtype CreateUserResponse _
derive instance repGenericCreateUserResponse :: Generic CreateUserResponse _
instance showCreateUserResponse :: Show CreateUserResponse where
  show = genericShow
instance decodeCreateUserResponse :: Decode CreateUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserResponse :: Encode CreateUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of the attribute, which is one of the values defined in the UserAttribute enumeration.</p>
newtype Delegate = Delegate 
  { "Id" :: (String)
  , "Type" :: (MemberType)
  }
derive instance newtypeDelegate :: Newtype Delegate _
derive instance repGenericDelegate :: Generic Delegate _
instance showDelegate :: Show Delegate where
  show = genericShow
instance decodeDelegate :: Decode Delegate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelegate :: Encode Delegate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAliasRequest = DeleteAliasRequest 
  { "OrganizationId" :: (OrganizationId)
  , "EntityId" :: (WorkMailIdentifier)
  , "Alias" :: (EmailAddress)
  }
derive instance newtypeDeleteAliasRequest :: Newtype DeleteAliasRequest _
derive instance repGenericDeleteAliasRequest :: Generic DeleteAliasRequest _
instance showDeleteAliasRequest :: Show DeleteAliasRequest where
  show = genericShow
instance decodeDeleteAliasRequest :: Decode DeleteAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAliasRequest :: Encode DeleteAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAliasResponse = DeleteAliasResponse Types.NoArguments
derive instance newtypeDeleteAliasResponse :: Newtype DeleteAliasResponse _
derive instance repGenericDeleteAliasResponse :: Generic DeleteAliasResponse _
instance showDeleteAliasResponse :: Show DeleteAliasResponse where
  show = genericShow
instance decodeDeleteAliasResponse :: Decode DeleteAliasResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAliasResponse :: Encode DeleteAliasResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGroupRequest = DeleteGroupRequest 
  { "OrganizationId" :: (OrganizationId)
  , "GroupId" :: (WorkMailIdentifier)
  }
derive instance newtypeDeleteGroupRequest :: Newtype DeleteGroupRequest _
derive instance repGenericDeleteGroupRequest :: Generic DeleteGroupRequest _
instance showDeleteGroupRequest :: Show DeleteGroupRequest where
  show = genericShow
instance decodeDeleteGroupRequest :: Decode DeleteGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGroupRequest :: Encode DeleteGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGroupResponse = DeleteGroupResponse Types.NoArguments
derive instance newtypeDeleteGroupResponse :: Newtype DeleteGroupResponse _
derive instance repGenericDeleteGroupResponse :: Generic DeleteGroupResponse _
instance showDeleteGroupResponse :: Show DeleteGroupResponse where
  show = genericShow
instance decodeDeleteGroupResponse :: Decode DeleteGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGroupResponse :: Encode DeleteGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteResourceRequest = DeleteResourceRequest 
  { "OrganizationId" :: (OrganizationId)
  , "ResourceId" :: (ResourceId)
  }
derive instance newtypeDeleteResourceRequest :: Newtype DeleteResourceRequest _
derive instance repGenericDeleteResourceRequest :: Generic DeleteResourceRequest _
instance showDeleteResourceRequest :: Show DeleteResourceRequest where
  show = genericShow
instance decodeDeleteResourceRequest :: Decode DeleteResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteResourceRequest :: Encode DeleteResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteResourceResponse = DeleteResourceResponse Types.NoArguments
derive instance newtypeDeleteResourceResponse :: Newtype DeleteResourceResponse _
derive instance repGenericDeleteResourceResponse :: Generic DeleteResourceResponse _
instance showDeleteResourceResponse :: Show DeleteResourceResponse where
  show = genericShow
instance decodeDeleteResourceResponse :: Decode DeleteResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteResourceResponse :: Encode DeleteResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteUserRequest = DeleteUserRequest 
  { "OrganizationId" :: (OrganizationId)
  , "UserId" :: (WorkMailIdentifier)
  }
derive instance newtypeDeleteUserRequest :: Newtype DeleteUserRequest _
derive instance repGenericDeleteUserRequest :: Generic DeleteUserRequest _
instance showDeleteUserRequest :: Show DeleteUserRequest where
  show = genericShow
instance decodeDeleteUserRequest :: Decode DeleteUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUserRequest :: Encode DeleteUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteUserResponse = DeleteUserResponse Types.NoArguments
derive instance newtypeDeleteUserResponse :: Newtype DeleteUserResponse _
derive instance repGenericDeleteUserResponse :: Generic DeleteUserResponse _
instance showDeleteUserResponse :: Show DeleteUserResponse where
  show = genericShow
instance decodeDeleteUserResponse :: Decode DeleteUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUserResponse :: Encode DeleteUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterFromWorkMailRequest = DeregisterFromWorkMailRequest 
  { "OrganizationId" :: (OrganizationId)
  , "EntityId" :: (WorkMailIdentifier)
  }
derive instance newtypeDeregisterFromWorkMailRequest :: Newtype DeregisterFromWorkMailRequest _
derive instance repGenericDeregisterFromWorkMailRequest :: Generic DeregisterFromWorkMailRequest _
instance showDeregisterFromWorkMailRequest :: Show DeregisterFromWorkMailRequest where
  show = genericShow
instance decodeDeregisterFromWorkMailRequest :: Decode DeregisterFromWorkMailRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterFromWorkMailRequest :: Encode DeregisterFromWorkMailRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterFromWorkMailResponse = DeregisterFromWorkMailResponse Types.NoArguments
derive instance newtypeDeregisterFromWorkMailResponse :: Newtype DeregisterFromWorkMailResponse _
derive instance repGenericDeregisterFromWorkMailResponse :: Generic DeregisterFromWorkMailResponse _
instance showDeregisterFromWorkMailResponse :: Show DeregisterFromWorkMailResponse where
  show = genericShow
instance decodeDeregisterFromWorkMailResponse :: Decode DeregisterFromWorkMailResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterFromWorkMailResponse :: Encode DeregisterFromWorkMailResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeGroupRequest = DescribeGroupRequest 
  { "OrganizationId" :: (OrganizationId)
  , "GroupId" :: (WorkMailIdentifier)
  }
derive instance newtypeDescribeGroupRequest :: Newtype DescribeGroupRequest _
derive instance repGenericDescribeGroupRequest :: Generic DescribeGroupRequest _
instance showDescribeGroupRequest :: Show DescribeGroupRequest where
  show = genericShow
instance decodeDescribeGroupRequest :: Decode DescribeGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGroupRequest :: Encode DescribeGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeGroupResponse = DescribeGroupResponse 
  { "GroupId" :: NullOrUndefined.NullOrUndefined (WorkMailIdentifier)
  , "Name" :: NullOrUndefined.NullOrUndefined (GroupName)
  , "Email" :: NullOrUndefined.NullOrUndefined (EmailAddress)
  , "State" :: NullOrUndefined.NullOrUndefined (EntityState)
  , "EnabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  , "DisabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeDescribeGroupResponse :: Newtype DescribeGroupResponse _
derive instance repGenericDescribeGroupResponse :: Generic DescribeGroupResponse _
instance showDescribeGroupResponse :: Show DescribeGroupResponse where
  show = genericShow
instance decodeDescribeGroupResponse :: Decode DescribeGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGroupResponse :: Encode DescribeGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeOrganizationRequest = DescribeOrganizationRequest 
  { "OrganizationId" :: (OrganizationId)
  }
derive instance newtypeDescribeOrganizationRequest :: Newtype DescribeOrganizationRequest _
derive instance repGenericDescribeOrganizationRequest :: Generic DescribeOrganizationRequest _
instance showDescribeOrganizationRequest :: Show DescribeOrganizationRequest where
  show = genericShow
instance decodeDescribeOrganizationRequest :: Decode DescribeOrganizationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeOrganizationRequest :: Encode DescribeOrganizationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeOrganizationResponse = DescribeOrganizationResponse 
  { "OrganizationId" :: NullOrUndefined.NullOrUndefined (OrganizationId)
  , "Alias" :: NullOrUndefined.NullOrUndefined (OrganizationName)
  , "State" :: NullOrUndefined.NullOrUndefined (String)
  , "DirectoryId" :: NullOrUndefined.NullOrUndefined (String)
  , "DirectoryType" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultMailDomain" :: NullOrUndefined.NullOrUndefined (String)
  , "CompletedDate" :: NullOrUndefined.NullOrUndefined (Number)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeOrganizationResponse :: Newtype DescribeOrganizationResponse _
derive instance repGenericDescribeOrganizationResponse :: Generic DescribeOrganizationResponse _
instance showDescribeOrganizationResponse :: Show DescribeOrganizationResponse where
  show = genericShow
instance decodeDescribeOrganizationResponse :: Decode DescribeOrganizationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeOrganizationResponse :: Encode DescribeOrganizationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeResourceRequest = DescribeResourceRequest 
  { "OrganizationId" :: (OrganizationId)
  , "ResourceId" :: (ResourceId)
  }
derive instance newtypeDescribeResourceRequest :: Newtype DescribeResourceRequest _
derive instance repGenericDescribeResourceRequest :: Generic DescribeResourceRequest _
instance showDescribeResourceRequest :: Show DescribeResourceRequest where
  show = genericShow
instance decodeDescribeResourceRequest :: Decode DescribeResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeResourceRequest :: Encode DescribeResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeResourceResponse = DescribeResourceResponse 
  { "ResourceId" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "Email" :: NullOrUndefined.NullOrUndefined (EmailAddress)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Type" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "BookingOptions" :: NullOrUndefined.NullOrUndefined (BookingOptions)
  , "State" :: NullOrUndefined.NullOrUndefined (EntityState)
  , "EnabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  , "DisabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeDescribeResourceResponse :: Newtype DescribeResourceResponse _
derive instance repGenericDescribeResourceResponse :: Generic DescribeResourceResponse _
instance showDescribeResourceResponse :: Show DescribeResourceResponse where
  show = genericShow
instance decodeDescribeResourceResponse :: Decode DescribeResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeResourceResponse :: Encode DescribeResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeUserRequest = DescribeUserRequest 
  { "OrganizationId" :: (OrganizationId)
  , "UserId" :: (WorkMailIdentifier)
  }
derive instance newtypeDescribeUserRequest :: Newtype DescribeUserRequest _
derive instance repGenericDescribeUserRequest :: Generic DescribeUserRequest _
instance showDescribeUserRequest :: Show DescribeUserRequest where
  show = genericShow
instance decodeDescribeUserRequest :: Decode DescribeUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUserRequest :: Encode DescribeUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeUserResponse = DescribeUserResponse 
  { "UserId" :: NullOrUndefined.NullOrUndefined (WorkMailIdentifier)
  , "Name" :: NullOrUndefined.NullOrUndefined (UserName)
  , "Email" :: NullOrUndefined.NullOrUndefined (EmailAddress)
  , "DisplayName" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (EntityState)
  , "UserRole" :: NullOrUndefined.NullOrUndefined (UserRole)
  , "EnabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  , "DisabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeDescribeUserResponse :: Newtype DescribeUserResponse _
derive instance repGenericDescribeUserResponse :: Generic DescribeUserResponse _
instance showDescribeUserResponse :: Show DescribeUserResponse where
  show = genericShow
instance decodeDescribeUserResponse :: Decode DescribeUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUserResponse :: Encode DescribeUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Directory Service doesn't recognize the credentials supplied by the Amazon WorkMail service.</p>
newtype DirectoryServiceAuthenticationFailedException = DirectoryServiceAuthenticationFailedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDirectoryServiceAuthenticationFailedException :: Newtype DirectoryServiceAuthenticationFailedException _
derive instance repGenericDirectoryServiceAuthenticationFailedException :: Generic DirectoryServiceAuthenticationFailedException _
instance showDirectoryServiceAuthenticationFailedException :: Show DirectoryServiceAuthenticationFailedException where
  show = genericShow
instance decodeDirectoryServiceAuthenticationFailedException :: Decode DirectoryServiceAuthenticationFailedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryServiceAuthenticationFailedException :: Encode DirectoryServiceAuthenticationFailedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The directory that you are trying to perform operations on isn't available.</p>
newtype DirectoryUnavailableException = DirectoryUnavailableException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDirectoryUnavailableException :: Newtype DirectoryUnavailableException _
derive instance repGenericDirectoryUnavailableException :: Generic DirectoryUnavailableException _
instance showDirectoryUnavailableException :: Show DirectoryUnavailableException where
  show = genericShow
instance decodeDirectoryUnavailableException :: Decode DirectoryUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryUnavailableException :: Encode DirectoryUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateDelegateFromResourceRequest = DisassociateDelegateFromResourceRequest 
  { "OrganizationId" :: (OrganizationId)
  , "ResourceId" :: (ResourceId)
  , "EntityId" :: (WorkMailIdentifier)
  }
derive instance newtypeDisassociateDelegateFromResourceRequest :: Newtype DisassociateDelegateFromResourceRequest _
derive instance repGenericDisassociateDelegateFromResourceRequest :: Generic DisassociateDelegateFromResourceRequest _
instance showDisassociateDelegateFromResourceRequest :: Show DisassociateDelegateFromResourceRequest where
  show = genericShow
instance decodeDisassociateDelegateFromResourceRequest :: Decode DisassociateDelegateFromResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateDelegateFromResourceRequest :: Encode DisassociateDelegateFromResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateDelegateFromResourceResponse = DisassociateDelegateFromResourceResponse Types.NoArguments
derive instance newtypeDisassociateDelegateFromResourceResponse :: Newtype DisassociateDelegateFromResourceResponse _
derive instance repGenericDisassociateDelegateFromResourceResponse :: Generic DisassociateDelegateFromResourceResponse _
instance showDisassociateDelegateFromResourceResponse :: Show DisassociateDelegateFromResourceResponse where
  show = genericShow
instance decodeDisassociateDelegateFromResourceResponse :: Decode DisassociateDelegateFromResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateDelegateFromResourceResponse :: Encode DisassociateDelegateFromResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateMemberFromGroupRequest = DisassociateMemberFromGroupRequest 
  { "OrganizationId" :: (OrganizationId)
  , "GroupId" :: (WorkMailIdentifier)
  , "MemberId" :: (WorkMailIdentifier)
  }
derive instance newtypeDisassociateMemberFromGroupRequest :: Newtype DisassociateMemberFromGroupRequest _
derive instance repGenericDisassociateMemberFromGroupRequest :: Generic DisassociateMemberFromGroupRequest _
instance showDisassociateMemberFromGroupRequest :: Show DisassociateMemberFromGroupRequest where
  show = genericShow
instance decodeDisassociateMemberFromGroupRequest :: Decode DisassociateMemberFromGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateMemberFromGroupRequest :: Encode DisassociateMemberFromGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateMemberFromGroupResponse = DisassociateMemberFromGroupResponse Types.NoArguments
derive instance newtypeDisassociateMemberFromGroupResponse :: Newtype DisassociateMemberFromGroupResponse _
derive instance repGenericDisassociateMemberFromGroupResponse :: Generic DisassociateMemberFromGroupResponse _
instance showDisassociateMemberFromGroupResponse :: Show DisassociateMemberFromGroupResponse where
  show = genericShow
instance decodeDisassociateMemberFromGroupResponse :: Decode DisassociateMemberFromGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateMemberFromGroupResponse :: Encode DisassociateMemberFromGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EmailAddress = EmailAddress String
derive instance newtypeEmailAddress :: Newtype EmailAddress _
derive instance repGenericEmailAddress :: Generic EmailAddress _
instance showEmailAddress :: Show EmailAddress where
  show = genericShow
instance decodeEmailAddress :: Decode EmailAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailAddress :: Encode EmailAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The email address that you're trying to assign is already created for a different user, group, or resource.</p>
newtype EmailAddressInUseException = EmailAddressInUseException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEmailAddressInUseException :: Newtype EmailAddressInUseException _
derive instance repGenericEmailAddressInUseException :: Generic EmailAddressInUseException _
instance showEmailAddressInUseException :: Show EmailAddressInUseException where
  show = genericShow
instance decodeEmailAddressInUseException :: Decode EmailAddressInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailAddressInUseException :: Encode EmailAddressInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The user, group, or resource that you're trying to register is already registered.</p>
newtype EntityAlreadyRegisteredException = EntityAlreadyRegisteredException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEntityAlreadyRegisteredException :: Newtype EntityAlreadyRegisteredException _
derive instance repGenericEntityAlreadyRegisteredException :: Generic EntityAlreadyRegisteredException _
instance showEntityAlreadyRegisteredException :: Show EntityAlreadyRegisteredException where
  show = genericShow
instance decodeEntityAlreadyRegisteredException :: Decode EntityAlreadyRegisteredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityAlreadyRegisteredException :: Encode EntityAlreadyRegisteredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The identifier supplied for the entity is valid, but it does not exist in your organization.</p>
newtype EntityNotFoundException = EntityNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEntityNotFoundException :: Newtype EntityNotFoundException _
derive instance repGenericEntityNotFoundException :: Generic EntityNotFoundException _
instance showEntityNotFoundException :: Show EntityNotFoundException where
  show = genericShow
instance decodeEntityNotFoundException :: Decode EntityNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityNotFoundException :: Encode EntityNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EntityState = EntityState String
derive instance newtypeEntityState :: Newtype EntityState _
derive instance repGenericEntityState :: Generic EntityState _
instance showEntityState :: Show EntityState where
  show = genericShow
instance decodeEntityState :: Decode EntityState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityState :: Encode EntityState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You are performing an operation on an entity that isn't in the expected state, such as trying to update a deleted user.</p>
newtype EntityStateException = EntityStateException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEntityStateException :: Newtype EntityStateException _
derive instance repGenericEntityStateException :: Generic EntityStateException _
instance showEntityStateException :: Show EntityStateException where
  show = genericShow
instance decodeEntityStateException :: Decode EntityStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityStateException :: Encode EntityStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The representation of an Amazon WorkMail group.</p>
newtype Group = Group 
  { "Id" :: NullOrUndefined.NullOrUndefined (WorkMailIdentifier)
  , "Email" :: NullOrUndefined.NullOrUndefined (EmailAddress)
  , "Name" :: NullOrUndefined.NullOrUndefined (GroupName)
  , "State" :: NullOrUndefined.NullOrUndefined (EntityState)
  , "EnabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  , "DisabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeGroup :: Newtype Group _
derive instance repGenericGroup :: Generic Group _
instance showGroup :: Show Group where
  show = genericShow
instance decodeGroup :: Decode Group where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroup :: Encode Group where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupName = GroupName String
derive instance newtypeGroupName :: Newtype GroupName _
derive instance repGenericGroupName :: Generic GroupName _
instance showGroupName :: Show GroupName where
  show = genericShow
instance decodeGroupName :: Decode GroupName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupName :: Encode GroupName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Groups = Groups (Array Group)
derive instance newtypeGroups :: Newtype Groups _
derive instance repGenericGroups :: Generic Groups _
instance showGroups :: Show Groups where
  show = genericShow
instance decodeGroups :: Decode Groups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroups :: Encode Groups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The configuration for a resource isn't valid. A resource must either be able to auto-respond to requests or have at least one delegate associated that can do it on its behalf.</p>
newtype InvalidConfigurationException = InvalidConfigurationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidConfigurationException :: Newtype InvalidConfigurationException _
derive instance repGenericInvalidConfigurationException :: Generic InvalidConfigurationException _
instance showInvalidConfigurationException :: Show InvalidConfigurationException where
  show = genericShow
instance decodeInvalidConfigurationException :: Decode InvalidConfigurationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidConfigurationException :: Encode InvalidConfigurationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more of the input parameters don't match the service's restrictions.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The supplied password doesn't match the minimum security constraints, such as length or use of special characters.</p>
newtype InvalidPasswordException = InvalidPasswordException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidPasswordException :: Newtype InvalidPasswordException _
derive instance repGenericInvalidPasswordException :: Generic InvalidPasswordException _
instance showInvalidPasswordException :: Show InvalidPasswordException where
  show = genericShow
instance decodeInvalidPasswordException :: Decode InvalidPasswordException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPasswordException :: Encode InvalidPasswordException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAliasesRequest = ListAliasesRequest 
  { "OrganizationId" :: (OrganizationId)
  , "EntityId" :: (WorkMailIdentifier)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListAliasesRequest :: Newtype ListAliasesRequest _
derive instance repGenericListAliasesRequest :: Generic ListAliasesRequest _
instance showListAliasesRequest :: Show ListAliasesRequest where
  show = genericShow
instance decodeListAliasesRequest :: Decode ListAliasesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAliasesRequest :: Encode ListAliasesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAliasesResponse = ListAliasesResponse 
  { "Aliases" :: NullOrUndefined.NullOrUndefined (Aliases)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListAliasesResponse :: Newtype ListAliasesResponse _
derive instance repGenericListAliasesResponse :: Generic ListAliasesResponse _
instance showListAliasesResponse :: Show ListAliasesResponse where
  show = genericShow
instance decodeListAliasesResponse :: Decode ListAliasesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAliasesResponse :: Encode ListAliasesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupMembersRequest = ListGroupMembersRequest 
  { "OrganizationId" :: (OrganizationId)
  , "GroupId" :: (WorkMailIdentifier)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListGroupMembersRequest :: Newtype ListGroupMembersRequest _
derive instance repGenericListGroupMembersRequest :: Generic ListGroupMembersRequest _
instance showListGroupMembersRequest :: Show ListGroupMembersRequest where
  show = genericShow
instance decodeListGroupMembersRequest :: Decode ListGroupMembersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupMembersRequest :: Encode ListGroupMembersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupMembersResponse = ListGroupMembersResponse 
  { "Members" :: NullOrUndefined.NullOrUndefined (Members)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListGroupMembersResponse :: Newtype ListGroupMembersResponse _
derive instance repGenericListGroupMembersResponse :: Generic ListGroupMembersResponse _
instance showListGroupMembersResponse :: Show ListGroupMembersResponse where
  show = genericShow
instance decodeListGroupMembersResponse :: Decode ListGroupMembersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupMembersResponse :: Encode ListGroupMembersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupsRequest = ListGroupsRequest 
  { "OrganizationId" :: (OrganizationId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListGroupsRequest :: Newtype ListGroupsRequest _
derive instance repGenericListGroupsRequest :: Generic ListGroupsRequest _
instance showListGroupsRequest :: Show ListGroupsRequest where
  show = genericShow
instance decodeListGroupsRequest :: Decode ListGroupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupsRequest :: Encode ListGroupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupsResponse = ListGroupsResponse 
  { "Groups" :: NullOrUndefined.NullOrUndefined (Groups)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListGroupsResponse :: Newtype ListGroupsResponse _
derive instance repGenericListGroupsResponse :: Generic ListGroupsResponse _
instance showListGroupsResponse :: Show ListGroupsResponse where
  show = genericShow
instance decodeListGroupsResponse :: Decode ListGroupsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupsResponse :: Encode ListGroupsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOrganizationsRequest = ListOrganizationsRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListOrganizationsRequest :: Newtype ListOrganizationsRequest _
derive instance repGenericListOrganizationsRequest :: Generic ListOrganizationsRequest _
instance showListOrganizationsRequest :: Show ListOrganizationsRequest where
  show = genericShow
instance decodeListOrganizationsRequest :: Decode ListOrganizationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOrganizationsRequest :: Encode ListOrganizationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOrganizationsResponse = ListOrganizationsResponse 
  { "OrganizationSummaries" :: NullOrUndefined.NullOrUndefined (OrganizationSummaries)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListOrganizationsResponse :: Newtype ListOrganizationsResponse _
derive instance repGenericListOrganizationsResponse :: Generic ListOrganizationsResponse _
instance showListOrganizationsResponse :: Show ListOrganizationsResponse where
  show = genericShow
instance decodeListOrganizationsResponse :: Decode ListOrganizationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOrganizationsResponse :: Encode ListOrganizationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceDelegatesRequest = ListResourceDelegatesRequest 
  { "OrganizationId" :: (OrganizationId)
  , "ResourceId" :: (WorkMailIdentifier)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListResourceDelegatesRequest :: Newtype ListResourceDelegatesRequest _
derive instance repGenericListResourceDelegatesRequest :: Generic ListResourceDelegatesRequest _
instance showListResourceDelegatesRequest :: Show ListResourceDelegatesRequest where
  show = genericShow
instance decodeListResourceDelegatesRequest :: Decode ListResourceDelegatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceDelegatesRequest :: Encode ListResourceDelegatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceDelegatesResponse = ListResourceDelegatesResponse 
  { "Delegates" :: NullOrUndefined.NullOrUndefined (ResourceDelegates)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListResourceDelegatesResponse :: Newtype ListResourceDelegatesResponse _
derive instance repGenericListResourceDelegatesResponse :: Generic ListResourceDelegatesResponse _
instance showListResourceDelegatesResponse :: Show ListResourceDelegatesResponse where
  show = genericShow
instance decodeListResourceDelegatesResponse :: Decode ListResourceDelegatesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceDelegatesResponse :: Encode ListResourceDelegatesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourcesRequest = ListResourcesRequest 
  { "OrganizationId" :: (OrganizationId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListResourcesRequest :: Newtype ListResourcesRequest _
derive instance repGenericListResourcesRequest :: Generic ListResourcesRequest _
instance showListResourcesRequest :: Show ListResourcesRequest where
  show = genericShow
instance decodeListResourcesRequest :: Decode ListResourcesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourcesRequest :: Encode ListResourcesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourcesResponse = ListResourcesResponse 
  { "Resources" :: NullOrUndefined.NullOrUndefined (Resources)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListResourcesResponse :: Newtype ListResourcesResponse _
derive instance repGenericListResourcesResponse :: Generic ListResourcesResponse _
instance showListResourcesResponse :: Show ListResourcesResponse where
  show = genericShow
instance decodeListResourcesResponse :: Decode ListResourcesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourcesResponse :: Encode ListResourcesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListUsersRequest = ListUsersRequest 
  { "OrganizationId" :: (OrganizationId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListUsersRequest :: Newtype ListUsersRequest _
derive instance repGenericListUsersRequest :: Generic ListUsersRequest _
instance showListUsersRequest :: Show ListUsersRequest where
  show = genericShow
instance decodeListUsersRequest :: Decode ListUsersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUsersRequest :: Encode ListUsersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListUsersResponse = ListUsersResponse 
  { "Users" :: NullOrUndefined.NullOrUndefined (Users)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListUsersResponse :: Newtype ListUsersResponse _
derive instance repGenericListUsersResponse :: Generic ListUsersResponse _
instance showListUsersResponse :: Show ListUsersResponse where
  show = genericShow
instance decodeListUsersResponse :: Decode ListUsersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUsersResponse :: Encode ListUsersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>For an email or alias to be created in Amazon WorkMail, the included domain must be defined in the organization.</p>
newtype MailDomainNotFoundException = MailDomainNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMailDomainNotFoundException :: Newtype MailDomainNotFoundException _
derive instance repGenericMailDomainNotFoundException :: Generic MailDomainNotFoundException _
instance showMailDomainNotFoundException :: Show MailDomainNotFoundException where
  show = genericShow
instance decodeMailDomainNotFoundException :: Decode MailDomainNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMailDomainNotFoundException :: Encode MailDomainNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>After a domain has been added to the organization, it must be verified. The domain is not yet verified.</p>
newtype MailDomainStateException = MailDomainStateException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMailDomainStateException :: Newtype MailDomainStateException _
derive instance repGenericMailDomainStateException :: Generic MailDomainStateException _
instance showMailDomainStateException :: Show MailDomainStateException where
  show = genericShow
instance decodeMailDomainStateException :: Decode MailDomainStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMailDomainStateException :: Encode MailDomainStateException where
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


-- | <p>The representation of a group member (user or group).</p>
newtype Member = Member 
  { "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Type" :: NullOrUndefined.NullOrUndefined (MemberType)
  , "State" :: NullOrUndefined.NullOrUndefined (EntityState)
  , "EnabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  , "DisabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeMember :: Newtype Member _
derive instance repGenericMember :: Generic Member _
instance showMember :: Show Member where
  show = genericShow
instance decodeMember :: Decode Member where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMember :: Encode Member where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MemberType = MemberType String
derive instance newtypeMemberType :: Newtype MemberType _
derive instance repGenericMemberType :: Generic MemberType _
instance showMemberType :: Show MemberType where
  show = genericShow
instance decodeMemberType :: Decode MemberType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMemberType :: Encode MemberType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Members = Members (Array Member)
derive instance newtypeMembers :: Newtype Members _
derive instance repGenericMembers :: Generic Members _
instance showMembers :: Show Members where
  show = genericShow
instance decodeMembers :: Decode Members where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMembers :: Encode Members where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The entity (user, group, or user) name isn't unique in Amazon WorkMail.</p>
newtype NameAvailabilityException = NameAvailabilityException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNameAvailabilityException :: Newtype NameAvailabilityException _
derive instance repGenericNameAvailabilityException :: Generic NameAvailabilityException _
instance showNameAvailabilityException :: Show NameAvailabilityException where
  show = genericShow
instance decodeNameAvailabilityException :: Decode NameAvailabilityException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNameAvailabilityException :: Encode NameAvailabilityException where
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


newtype OrganizationId = OrganizationId String
derive instance newtypeOrganizationId :: Newtype OrganizationId _
derive instance repGenericOrganizationId :: Generic OrganizationId _
instance showOrganizationId :: Show OrganizationId where
  show = genericShow
instance decodeOrganizationId :: Decode OrganizationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrganizationId :: Encode OrganizationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrganizationName = OrganizationName String
derive instance newtypeOrganizationName :: Newtype OrganizationName _
derive instance repGenericOrganizationName :: Generic OrganizationName _
instance showOrganizationName :: Show OrganizationName where
  show = genericShow
instance decodeOrganizationName :: Decode OrganizationName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrganizationName :: Encode OrganizationName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An operation received a valid organization identifier that either doesn't belong or exist in the system.</p>
newtype OrganizationNotFoundException = OrganizationNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOrganizationNotFoundException :: Newtype OrganizationNotFoundException _
derive instance repGenericOrganizationNotFoundException :: Generic OrganizationNotFoundException _
instance showOrganizationNotFoundException :: Show OrganizationNotFoundException where
  show = genericShow
instance decodeOrganizationNotFoundException :: Decode OrganizationNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrganizationNotFoundException :: Encode OrganizationNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The organization must have a valid state (Active or Synchronizing) to perform certain operations on the organization or its entities.</p>
newtype OrganizationStateException = OrganizationStateException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOrganizationStateException :: Newtype OrganizationStateException _
derive instance repGenericOrganizationStateException :: Generic OrganizationStateException _
instance showOrganizationStateException :: Show OrganizationStateException where
  show = genericShow
instance decodeOrganizationStateException :: Decode OrganizationStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrganizationStateException :: Encode OrganizationStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrganizationSummaries = OrganizationSummaries (Array OrganizationSummary)
derive instance newtypeOrganizationSummaries :: Newtype OrganizationSummaries _
derive instance repGenericOrganizationSummaries :: Generic OrganizationSummaries _
instance showOrganizationSummaries :: Show OrganizationSummaries where
  show = genericShow
instance decodeOrganizationSummaries :: Decode OrganizationSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrganizationSummaries :: Encode OrganizationSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The brief overview associated with an organization.</p>
newtype OrganizationSummary = OrganizationSummary 
  { "OrganizationId" :: NullOrUndefined.NullOrUndefined (OrganizationId)
  , "Alias" :: NullOrUndefined.NullOrUndefined (OrganizationName)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOrganizationSummary :: Newtype OrganizationSummary _
derive instance repGenericOrganizationSummary :: Generic OrganizationSummary _
instance showOrganizationSummary :: Show OrganizationSummary where
  show = genericShow
instance decodeOrganizationSummary :: Decode OrganizationSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrganizationSummary :: Encode OrganizationSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Password = Password String
derive instance newtypePassword :: Newtype Password _
derive instance repGenericPassword :: Generic Password _
instance showPassword :: Show Password where
  show = genericShow
instance decodePassword :: Decode Password where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePassword :: Encode Password where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterToWorkMailRequest = RegisterToWorkMailRequest 
  { "OrganizationId" :: (OrganizationId)
  , "EntityId" :: (WorkMailIdentifier)
  , "Email" :: (EmailAddress)
  }
derive instance newtypeRegisterToWorkMailRequest :: Newtype RegisterToWorkMailRequest _
derive instance repGenericRegisterToWorkMailRequest :: Generic RegisterToWorkMailRequest _
instance showRegisterToWorkMailRequest :: Show RegisterToWorkMailRequest where
  show = genericShow
instance decodeRegisterToWorkMailRequest :: Decode RegisterToWorkMailRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterToWorkMailRequest :: Encode RegisterToWorkMailRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterToWorkMailResponse = RegisterToWorkMailResponse Types.NoArguments
derive instance newtypeRegisterToWorkMailResponse :: Newtype RegisterToWorkMailResponse _
derive instance repGenericRegisterToWorkMailResponse :: Generic RegisterToWorkMailResponse _
instance showRegisterToWorkMailResponse :: Show RegisterToWorkMailResponse where
  show = genericShow
instance decodeRegisterToWorkMailResponse :: Decode RegisterToWorkMailResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterToWorkMailResponse :: Encode RegisterToWorkMailResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This entity name is not allowed in Amazon WorkMail.</p>
newtype ReservedNameException = ReservedNameException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeReservedNameException :: Newtype ReservedNameException _
derive instance repGenericReservedNameException :: Generic ReservedNameException _
instance showReservedNameException :: Show ReservedNameException where
  show = genericShow
instance decodeReservedNameException :: Decode ReservedNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedNameException :: Encode ReservedNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResetPasswordRequest = ResetPasswordRequest 
  { "OrganizationId" :: (OrganizationId)
  , "UserId" :: (WorkMailIdentifier)
  , "Password" :: (Password)
  }
derive instance newtypeResetPasswordRequest :: Newtype ResetPasswordRequest _
derive instance repGenericResetPasswordRequest :: Generic ResetPasswordRequest _
instance showResetPasswordRequest :: Show ResetPasswordRequest where
  show = genericShow
instance decodeResetPasswordRequest :: Decode ResetPasswordRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResetPasswordRequest :: Encode ResetPasswordRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResetPasswordResponse = ResetPasswordResponse Types.NoArguments
derive instance newtypeResetPasswordResponse :: Newtype ResetPasswordResponse _
derive instance repGenericResetPasswordResponse :: Generic ResetPasswordResponse _
instance showResetPasswordResponse :: Show ResetPasswordResponse where
  show = genericShow
instance decodeResetPasswordResponse :: Decode ResetPasswordResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResetPasswordResponse :: Encode ResetPasswordResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The overview for a resource containing relevant data regarding it.</p>
newtype Resource = Resource 
  { "Id" :: NullOrUndefined.NullOrUndefined (WorkMailIdentifier)
  , "Email" :: NullOrUndefined.NullOrUndefined (EmailAddress)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Type" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "State" :: NullOrUndefined.NullOrUndefined (EntityState)
  , "EnabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  , "DisabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeResource :: Newtype Resource _
derive instance repGenericResource :: Generic Resource _
instance showResource :: Show Resource where
  show = genericShow
instance decodeResource :: Decode Resource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResource :: Encode Resource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceDelegates = ResourceDelegates (Array Delegate)
derive instance newtypeResourceDelegates :: Newtype ResourceDelegates _
derive instance repGenericResourceDelegates :: Generic ResourceDelegates _
instance showResourceDelegates :: Show ResourceDelegates where
  show = genericShow
instance decodeResourceDelegates :: Decode ResourceDelegates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDelegates :: Encode ResourceDelegates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _
derive instance repGenericResourceId :: Generic ResourceId _
instance showResourceId :: Show ResourceId where
  show = genericShow
instance decodeResourceId :: Decode ResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceId :: Encode ResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _
derive instance repGenericResourceName :: Generic ResourceName _
instance showResourceName :: Show ResourceName where
  show = genericShow
instance decodeResourceName :: Decode ResourceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceName :: Encode ResourceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _
derive instance repGenericResourceType :: Generic ResourceType _
instance showResourceType :: Show ResourceType where
  show = genericShow
instance decodeResourceType :: Decode ResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceType :: Encode ResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Resources = Resources (Array Resource)
derive instance newtypeResources :: Newtype Resources _
derive instance repGenericResources :: Generic Resources _
instance showResources :: Show Resources where
  show = genericShow
instance decodeResources :: Decode Resources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResources :: Encode Resources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You can't perform a write operation against a read-only directory.</p>
newtype UnsupportedOperationException = UnsupportedOperationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnsupportedOperationException :: Newtype UnsupportedOperationException _
derive instance repGenericUnsupportedOperationException :: Generic UnsupportedOperationException _
instance showUnsupportedOperationException :: Show UnsupportedOperationException where
  show = genericShow
instance decodeUnsupportedOperationException :: Decode UnsupportedOperationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedOperationException :: Encode UnsupportedOperationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePrimaryEmailAddressRequest = UpdatePrimaryEmailAddressRequest 
  { "OrganizationId" :: (OrganizationId)
  , "EntityId" :: (WorkMailIdentifier)
  , "Email" :: (EmailAddress)
  }
derive instance newtypeUpdatePrimaryEmailAddressRequest :: Newtype UpdatePrimaryEmailAddressRequest _
derive instance repGenericUpdatePrimaryEmailAddressRequest :: Generic UpdatePrimaryEmailAddressRequest _
instance showUpdatePrimaryEmailAddressRequest :: Show UpdatePrimaryEmailAddressRequest where
  show = genericShow
instance decodeUpdatePrimaryEmailAddressRequest :: Decode UpdatePrimaryEmailAddressRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePrimaryEmailAddressRequest :: Encode UpdatePrimaryEmailAddressRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePrimaryEmailAddressResponse = UpdatePrimaryEmailAddressResponse Types.NoArguments
derive instance newtypeUpdatePrimaryEmailAddressResponse :: Newtype UpdatePrimaryEmailAddressResponse _
derive instance repGenericUpdatePrimaryEmailAddressResponse :: Generic UpdatePrimaryEmailAddressResponse _
instance showUpdatePrimaryEmailAddressResponse :: Show UpdatePrimaryEmailAddressResponse where
  show = genericShow
instance decodeUpdatePrimaryEmailAddressResponse :: Decode UpdatePrimaryEmailAddressResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePrimaryEmailAddressResponse :: Encode UpdatePrimaryEmailAddressResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateResourceRequest = UpdateResourceRequest 
  { "OrganizationId" :: (OrganizationId)
  , "ResourceId" :: (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "BookingOptions" :: NullOrUndefined.NullOrUndefined (BookingOptions)
  }
derive instance newtypeUpdateResourceRequest :: Newtype UpdateResourceRequest _
derive instance repGenericUpdateResourceRequest :: Generic UpdateResourceRequest _
instance showUpdateResourceRequest :: Show UpdateResourceRequest where
  show = genericShow
instance decodeUpdateResourceRequest :: Decode UpdateResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateResourceRequest :: Encode UpdateResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateResourceResponse = UpdateResourceResponse Types.NoArguments
derive instance newtypeUpdateResourceResponse :: Newtype UpdateResourceResponse _
derive instance repGenericUpdateResourceResponse :: Generic UpdateResourceResponse _
instance showUpdateResourceResponse :: Show UpdateResourceResponse where
  show = genericShow
instance decodeUpdateResourceResponse :: Decode UpdateResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateResourceResponse :: Encode UpdateResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The representation of an Amazon WorkMail user.</p>
newtype User = User 
  { "Id" :: NullOrUndefined.NullOrUndefined (WorkMailIdentifier)
  , "Email" :: NullOrUndefined.NullOrUndefined (EmailAddress)
  , "Name" :: NullOrUndefined.NullOrUndefined (UserName)
  , "DisplayName" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (EntityState)
  , "UserRole" :: NullOrUndefined.NullOrUndefined (UserRole)
  , "EnabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  , "DisabledDate" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeUser :: Newtype User _
derive instance repGenericUser :: Generic User _
instance showUser :: Show User where
  show = genericShow
instance decodeUser :: Decode User where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUser :: Encode User where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserName = UserName String
derive instance newtypeUserName :: Newtype UserName _
derive instance repGenericUserName :: Generic UserName _
instance showUserName :: Show UserName where
  show = genericShow
instance decodeUserName :: Decode UserName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserName :: Encode UserName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserRole = UserRole String
derive instance newtypeUserRole :: Newtype UserRole _
derive instance repGenericUserRole :: Generic UserRole _
instance showUserRole :: Show UserRole where
  show = genericShow
instance decodeUserRole :: Decode UserRole where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserRole :: Encode UserRole where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Users = Users (Array User)
derive instance newtypeUsers :: Newtype Users _
derive instance repGenericUsers :: Generic Users _
instance showUsers :: Show Users where
  show = genericShow
instance decodeUsers :: Decode Users where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsers :: Encode Users where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkMailIdentifier = WorkMailIdentifier String
derive instance newtypeWorkMailIdentifier :: Newtype WorkMailIdentifier _
derive instance repGenericWorkMailIdentifier :: Generic WorkMailIdentifier _
instance showWorkMailIdentifier :: Show WorkMailIdentifier where
  show = genericShow
instance decodeWorkMailIdentifier :: Decode WorkMailIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkMailIdentifier :: Encode WorkMailIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
