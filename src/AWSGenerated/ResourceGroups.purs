

-- | <fullname>AWS Resource Groups</fullname> <p>AWS Resource Groups lets you organize AWS resources such as Amazon EC2 instances, Amazon Relational Database Service databases, and Amazon S3 buckets into groups using criteria that you define as tags. A resource group is a collection of resources that match the resource types specified in a query, and share one or more tags or portions of tags. You can create a group of resources based on their roles in your cloud infrastructure, lifecycle stages, regions, application layers, or virtually any criteria. Resource groups enable you to automate management tasks, such as those in AWS Systems Manager Automation documents, on tag-related resources in AWS Systems Manager. Groups of tagged resources also let you quickly view a custom console in AWS Systems Manager that shows AWS Config compliance and other monitoring data about member resources.</p> <p>To create a resource group, build a resource query, and specify tags that identify the criteria that members of the group have in common. Tags are key-value pairs.</p> <p>For more information about Resource Groups, see the <a href="https://docs.aws.amazon.com/ARG/latest/userguide/welcome.html">AWS Resource Groups User Guide</a>.</p> <p>AWS Resource Groups uses a REST-compliant API that you can use to perform the following types of operations.</p> <ul> <li> <p>Create, Read, Update, and Delete (CRUD) operations on resource groups and resource query entities</p> </li> <li> <p>Applying, editing, and removing tags from resource groups</p> </li> <li> <p>Resolving resource group member ARNs so they can be returned as search results</p> </li> <li> <p>Getting data about resources that are members of a group</p> </li> <li> <p>Searching AWS resources based on a resource query</p> </li> </ul>
module AWS.ResourceGroups where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ResourceGroups" :: String


-- | <p>Creates a group with a specified name, description, and resource query.</p>
createGroup :: forall eff. CreateGroupInput -> Aff (err :: AWS.RequestError | eff) CreateGroupOutput
createGroup = AWS.request serviceName "createGroup" 


-- | <p>Deletes a specified resource group. Deleting a resource group does not delete resources that are members of the group; it only deletes the group structure.</p>
deleteGroup :: forall eff. DeleteGroupInput -> Aff (err :: AWS.RequestError | eff) DeleteGroupOutput
deleteGroup = AWS.request serviceName "deleteGroup" 


-- | <p>Returns information about a specified resource group.</p>
getGroup :: forall eff. GetGroupInput -> Aff (err :: AWS.RequestError | eff) GetGroupOutput
getGroup = AWS.request serviceName "getGroup" 


-- | <p>Returns the resource query associated with the specified resource group.</p>
getGroupQuery :: forall eff. GetGroupQueryInput -> Aff (err :: AWS.RequestError | eff) GetGroupQueryOutput
getGroupQuery = AWS.request serviceName "getGroupQuery" 


-- | <p>Returns a list of tags that are associated with a resource, specified by an ARN.</p>
getTags :: forall eff. GetTagsInput -> Aff (err :: AWS.RequestError | eff) GetTagsOutput
getTags = AWS.request serviceName "getTags" 


-- | <p>Returns a list of ARNs of resources that are members of a specified resource group.</p>
listGroupResources :: forall eff. ListGroupResourcesInput -> Aff (err :: AWS.RequestError | eff) ListGroupResourcesOutput
listGroupResources = AWS.request serviceName "listGroupResources" 


-- | <p>Returns a list of existing resource groups in your account.</p>
listGroups :: forall eff. ListGroupsInput -> Aff (err :: AWS.RequestError | eff) ListGroupsOutput
listGroups = AWS.request serviceName "listGroups" 


-- | <p>Returns a list of AWS resource identifiers that matches a specified query. The query uses the same format as a resource query in a CreateGroup or UpdateGroupQuery operation.</p>
searchResources :: forall eff. SearchResourcesInput -> Aff (err :: AWS.RequestError | eff) SearchResourcesOutput
searchResources = AWS.request serviceName "searchResources" 


-- | <p>Adds specified tags to a resource with the specified ARN. Existing tags on a resource are not changed if they are not specified in the request parameters.</p>
tag :: forall eff. TagInput -> Aff (err :: AWS.RequestError | eff) TagOutput
tag = AWS.request serviceName "tag" 


-- | <p>Deletes specified tags from a specified resource.</p>
untag :: forall eff. UntagInput -> Aff (err :: AWS.RequestError | eff) UntagOutput
untag = AWS.request serviceName "untag" 


-- | <p>Updates an existing group with a new or changed description. You cannot update the name of a resource group.</p>
updateGroup :: forall eff. UpdateGroupInput -> Aff (err :: AWS.RequestError | eff) UpdateGroupOutput
updateGroup = AWS.request serviceName "updateGroup" 


-- | <p>Updates the resource query of a group.</p>
updateGroupQuery :: forall eff. UpdateGroupQueryInput -> Aff (err :: AWS.RequestError | eff) UpdateGroupQueryOutput
updateGroupQuery = AWS.request serviceName "updateGroupQuery" 


-- | <p>The request does not comply with validation rules that are defined for the request parameters.</p>
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _


newtype CreateGroupInput = CreateGroupInput 
  { "Name" :: (GroupName)
  , "Description" :: NullOrUndefined (GroupDescription)
  , "ResourceQuery" :: (ResourceQuery)
  , "Tags" :: NullOrUndefined (Tags)
  }
derive instance newtypeCreateGroupInput :: Newtype CreateGroupInput _


newtype CreateGroupOutput = CreateGroupOutput 
  { "Group" :: NullOrUndefined (Group)
  , "ResourceQuery" :: NullOrUndefined (ResourceQuery)
  , "Tags" :: NullOrUndefined (Tags)
  }
derive instance newtypeCreateGroupOutput :: Newtype CreateGroupOutput _


newtype DeleteGroupInput = DeleteGroupInput 
  { "GroupName" :: (GroupName)
  }
derive instance newtypeDeleteGroupInput :: Newtype DeleteGroupInput _


newtype DeleteGroupOutput = DeleteGroupOutput 
  { "Group" :: NullOrUndefined (Group)
  }
derive instance newtypeDeleteGroupOutput :: Newtype DeleteGroupOutput _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p>The caller is not authorized to make the request.</p>
newtype ForbiddenException = ForbiddenException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeForbiddenException :: Newtype ForbiddenException _


newtype GetGroupInput = GetGroupInput 
  { "GroupName" :: (GroupName)
  }
derive instance newtypeGetGroupInput :: Newtype GetGroupInput _


newtype GetGroupOutput = GetGroupOutput 
  { "Group" :: NullOrUndefined (Group)
  }
derive instance newtypeGetGroupOutput :: Newtype GetGroupOutput _


newtype GetGroupQueryInput = GetGroupQueryInput 
  { "GroupName" :: (GroupName)
  }
derive instance newtypeGetGroupQueryInput :: Newtype GetGroupQueryInput _


newtype GetGroupQueryOutput = GetGroupQueryOutput 
  { "GroupQuery" :: NullOrUndefined (GroupQuery)
  }
derive instance newtypeGetGroupQueryOutput :: Newtype GetGroupQueryOutput _


newtype GetTagsInput = GetTagsInput 
  { "Arn" :: (GroupArn)
  }
derive instance newtypeGetTagsInput :: Newtype GetTagsInput _


newtype GetTagsOutput = GetTagsOutput 
  { "Arn" :: NullOrUndefined (GroupArn)
  , "Tags" :: NullOrUndefined (Tags)
  }
derive instance newtypeGetTagsOutput :: Newtype GetTagsOutput _


-- | <p>A resource group.</p>
newtype Group = Group 
  { "GroupArn" :: (GroupArn)
  , "Name" :: (GroupName)
  , "Description" :: NullOrUndefined (GroupDescription)
  }
derive instance newtypeGroup :: Newtype Group _


newtype GroupArn = GroupArn String
derive instance newtypeGroupArn :: Newtype GroupArn _


newtype GroupDescription = GroupDescription String
derive instance newtypeGroupDescription :: Newtype GroupDescription _


newtype GroupList = GroupList (Array Group)
derive instance newtypeGroupList :: Newtype GroupList _


newtype GroupName = GroupName String
derive instance newtypeGroupName :: Newtype GroupName _


-- | <p>The underlying resource query of a resource group. Resources that match query results are part of the group.</p>
newtype GroupQuery = GroupQuery 
  { "GroupName" :: (GroupName)
  , "ResourceQuery" :: (ResourceQuery)
  }
derive instance newtypeGroupQuery :: Newtype GroupQuery _


-- | <p>An internal error occurred while processing the request.</p>
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _


newtype ListGroupResourcesInput = ListGroupResourcesInput 
  { "GroupName" :: (GroupName)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListGroupResourcesInput :: Newtype ListGroupResourcesInput _


newtype ListGroupResourcesOutput = ListGroupResourcesOutput 
  { "ResourceIdentifiers" :: NullOrUndefined (ResourceIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListGroupResourcesOutput :: Newtype ListGroupResourcesOutput _


newtype ListGroupsInput = ListGroupsInput 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListGroupsInput :: Newtype ListGroupsInput _


newtype ListGroupsOutput = ListGroupsOutput 
  { "Groups" :: NullOrUndefined (GroupList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListGroupsOutput :: Newtype ListGroupsOutput _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | <p>The request uses an HTTP method which is not allowed for the specified resource.</p>
newtype MethodNotAllowedException = MethodNotAllowedException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMethodNotAllowedException :: Newtype MethodNotAllowedException _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>One or more resources specified in the request do not exist.</p>
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


newtype Query = Query String
derive instance newtypeQuery :: Newtype Query _


newtype QueryType = QueryType String
derive instance newtypeQueryType :: Newtype QueryType _


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _


-- | <p>The ARN of a resource, and its resource type.</p>
newtype ResourceIdentifier = ResourceIdentifier 
  { "ResourceArn" :: NullOrUndefined (ResourceArn)
  , "ResourceType" :: NullOrUndefined (ResourceType)
  }
derive instance newtypeResourceIdentifier :: Newtype ResourceIdentifier _


newtype ResourceIdentifierList = ResourceIdentifierList (Array ResourceIdentifier)
derive instance newtypeResourceIdentifierList :: Newtype ResourceIdentifierList _


-- | <p>The query that is used to define a resource group or a search for resources.</p>
newtype ResourceQuery = ResourceQuery 
  { "Type" :: (QueryType)
  , "Query" :: (Query)
  }
derive instance newtypeResourceQuery :: Newtype ResourceQuery _


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _


newtype SearchResourcesInput = SearchResourcesInput 
  { "ResourceQuery" :: (ResourceQuery)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeSearchResourcesInput :: Newtype SearchResourcesInput _


newtype SearchResourcesOutput = SearchResourcesOutput 
  { "ResourceIdentifiers" :: NullOrUndefined (ResourceIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeSearchResourcesOutput :: Newtype SearchResourcesOutput _


newtype TagInput = TagInput 
  { "Arn" :: (GroupArn)
  , "Tags" :: (Tags)
  }
derive instance newtypeTagInput :: Newtype TagInput _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagOutput = TagOutput 
  { "Arn" :: NullOrUndefined (GroupArn)
  , "Tags" :: NullOrUndefined (Tags)
  }
derive instance newtypeTagOutput :: Newtype TagOutput _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype Tags = Tags (Map TagKey TagValue)
derive instance newtypeTags :: Newtype Tags _


-- | <p>The caller has exceeded throttling limits.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _


-- | <p>The request has not been applied because it lacks valid authentication credentials for the target resource.</p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnauthorizedException :: Newtype UnauthorizedException _


newtype UntagInput = UntagInput 
  { "Arn" :: (GroupArn)
  , "Keys" :: (TagKeyList)
  }
derive instance newtypeUntagInput :: Newtype UntagInput _


newtype UntagOutput = UntagOutput 
  { "Arn" :: NullOrUndefined (GroupArn)
  , "Keys" :: NullOrUndefined (TagKeyList)
  }
derive instance newtypeUntagOutput :: Newtype UntagOutput _


newtype UpdateGroupInput = UpdateGroupInput 
  { "GroupName" :: (GroupName)
  , "Description" :: NullOrUndefined (GroupDescription)
  }
derive instance newtypeUpdateGroupInput :: Newtype UpdateGroupInput _


newtype UpdateGroupOutput = UpdateGroupOutput 
  { "Group" :: NullOrUndefined (Group)
  }
derive instance newtypeUpdateGroupOutput :: Newtype UpdateGroupOutput _


newtype UpdateGroupQueryInput = UpdateGroupQueryInput 
  { "GroupName" :: (GroupName)
  , "ResourceQuery" :: (ResourceQuery)
  }
derive instance newtypeUpdateGroupQueryInput :: Newtype UpdateGroupQueryInput _


newtype UpdateGroupQueryOutput = UpdateGroupQueryOutput 
  { "GroupQuery" :: NullOrUndefined (GroupQuery)
  }
derive instance newtypeUpdateGroupQueryOutput :: Newtype UpdateGroupQueryOutput _
