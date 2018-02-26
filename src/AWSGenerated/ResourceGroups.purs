

-- | <fullname>AWS Resource Groups</fullname> <p>AWS Resource Groups lets you organize AWS resources such as Amazon EC2 instances, Amazon Relational Database Service databases, and Amazon S3 buckets into groups using criteria that you define as tags. A resource group is a collection of resources that match the resource types specified in a query, and share one or more tags or portions of tags. You can create a group of resources based on their roles in your cloud infrastructure, lifecycle stages, regions, application layers, or virtually any criteria. Resource groups enable you to automate management tasks, such as those in AWS Systems Manager Automation documents, on tag-related resources in AWS Systems Manager. Groups of tagged resources also let you quickly view a custom console in AWS Systems Manager that shows AWS Config compliance and other monitoring data about member resources.</p> <p>To create a resource group, build a resource query, and specify tags that identify the criteria that members of the group have in common. Tags are key-value pairs.</p> <p>For more information about Resource Groups, see the <a href="https://docs.aws.amazon.com/ARG/latest/userguide/welcome.html">AWS Resource Groups User Guide</a>.</p> <p>AWS Resource Groups uses a REST-compliant API that you can use to perform the following types of operations.</p> <ul> <li> <p>Create, Read, Update, and Delete (CRUD) operations on resource groups and resource query entities</p> </li> <li> <p>Applying, editing, and removing tags from resource groups</p> </li> <li> <p>Resolving resource group member ARNs so they can be returned as search results</p> </li> <li> <p>Getting data about resources that are members of a group</p> </li> <li> <p>Searching AWS resources based on a resource query</p> </li> </ul>
module AWS.ResourceGroups where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ResourceGroups" :: String


-- | <p>Creates a group with a specified name, description, and resource query.</p>
createGroup :: forall eff. CreateGroupInput -> Aff (err :: AWS.RequestError | eff) CreateGroupOutput
createGroup = AWS.request serviceName "CreateGroup" 


-- | <p>Deletes a specified resource group. Deleting a resource group does not delete resources that are members of the group; it only deletes the group structure.</p>
deleteGroup :: forall eff. DeleteGroupInput -> Aff (err :: AWS.RequestError | eff) DeleteGroupOutput
deleteGroup = AWS.request serviceName "DeleteGroup" 


-- | <p>Returns information about a specified resource group.</p>
getGroup :: forall eff. GetGroupInput -> Aff (err :: AWS.RequestError | eff) GetGroupOutput
getGroup = AWS.request serviceName "GetGroup" 


-- | <p>Returns the resource query associated with the specified resource group.</p>
getGroupQuery :: forall eff. GetGroupQueryInput -> Aff (err :: AWS.RequestError | eff) GetGroupQueryOutput
getGroupQuery = AWS.request serviceName "GetGroupQuery" 


-- | <p>Returns a list of tags that are associated with a resource, specified by an ARN.</p>
getTags :: forall eff. GetTagsInput -> Aff (err :: AWS.RequestError | eff) GetTagsOutput
getTags = AWS.request serviceName "GetTags" 


-- | <p>Returns a list of ARNs of resources that are members of a specified resource group.</p>
listGroupResources :: forall eff. ListGroupResourcesInput -> Aff (err :: AWS.RequestError | eff) ListGroupResourcesOutput
listGroupResources = AWS.request serviceName "ListGroupResources" 


-- | <p>Returns a list of existing resource groups in your account.</p>
listGroups :: forall eff. ListGroupsInput -> Aff (err :: AWS.RequestError | eff) ListGroupsOutput
listGroups = AWS.request serviceName "ListGroups" 


-- | <p>Returns a list of AWS resource identifiers that matches a specified query. The query uses the same format as a resource query in a CreateGroup or UpdateGroupQuery operation.</p>
searchResources :: forall eff. SearchResourcesInput -> Aff (err :: AWS.RequestError | eff) SearchResourcesOutput
searchResources = AWS.request serviceName "SearchResources" 


-- | <p>Adds specified tags to a resource with the specified ARN. Existing tags on a resource are not changed if they are not specified in the request parameters.</p>
tag :: forall eff. TagInput -> Aff (err :: AWS.RequestError | eff) TagOutput
tag = AWS.request serviceName "Tag" 


-- | <p>Deletes specified tags from a specified resource.</p>
untag :: forall eff. UntagInput -> Aff (err :: AWS.RequestError | eff) UntagOutput
untag = AWS.request serviceName "Untag" 


-- | <p>Updates an existing group with a new or changed description. You cannot update the name of a resource group.</p>
updateGroup :: forall eff. UpdateGroupInput -> Aff (err :: AWS.RequestError | eff) UpdateGroupOutput
updateGroup = AWS.request serviceName "UpdateGroup" 


-- | <p>Updates the resource query of a group.</p>
updateGroupQuery :: forall eff. UpdateGroupQueryInput -> Aff (err :: AWS.RequestError | eff) UpdateGroupQueryOutput
updateGroupQuery = AWS.request serviceName "UpdateGroupQuery" 


-- | <p>The request does not comply with validation rules that are defined for the request parameters.</p>
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype CreateGroupInput = CreateGroupInput 
  { "Name" :: (GroupName)
  , "Description" :: NullOrUndefined (GroupDescription)
  , "ResourceQuery" :: (ResourceQuery)
  , "Tags" :: NullOrUndefined (Tags)
  }


newtype CreateGroupOutput = CreateGroupOutput 
  { "Group" :: NullOrUndefined (Group)
  , "ResourceQuery" :: NullOrUndefined (ResourceQuery)
  , "Tags" :: NullOrUndefined (Tags)
  }


newtype DeleteGroupInput = DeleteGroupInput 
  { "GroupName" :: (GroupName)
  }


newtype DeleteGroupOutput = DeleteGroupOutput 
  { "Group" :: NullOrUndefined (Group)
  }


newtype ErrorMessage = ErrorMessage String


-- | <p>The caller is not authorized to make the request.</p>
newtype ForbiddenException = ForbiddenException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype GetGroupInput = GetGroupInput 
  { "GroupName" :: (GroupName)
  }


newtype GetGroupOutput = GetGroupOutput 
  { "Group" :: NullOrUndefined (Group)
  }


newtype GetGroupQueryInput = GetGroupQueryInput 
  { "GroupName" :: (GroupName)
  }


newtype GetGroupQueryOutput = GetGroupQueryOutput 
  { "GroupQuery" :: NullOrUndefined (GroupQuery)
  }


newtype GetTagsInput = GetTagsInput 
  { "Arn" :: (GroupArn)
  }


newtype GetTagsOutput = GetTagsOutput 
  { "Arn" :: NullOrUndefined (GroupArn)
  , "Tags" :: NullOrUndefined (Tags)
  }


-- | <p>A resource group.</p>
newtype Group = Group 
  { "GroupArn" :: (GroupArn)
  , "Name" :: (GroupName)
  , "Description" :: NullOrUndefined (GroupDescription)
  }


newtype GroupArn = GroupArn String


newtype GroupDescription = GroupDescription String


newtype GroupList = GroupList (Array Group)


newtype GroupName = GroupName String


-- | <p>The underlying resource query of a resource group. Resources that match query results are part of the group.</p>
newtype GroupQuery = GroupQuery 
  { "GroupName" :: (GroupName)
  , "ResourceQuery" :: (ResourceQuery)
  }


-- | <p>An internal error occurred while processing the request.</p>
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ListGroupResourcesInput = ListGroupResourcesInput 
  { "GroupName" :: (GroupName)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListGroupResourcesOutput = ListGroupResourcesOutput 
  { "ResourceIdentifiers" :: NullOrUndefined (ResourceIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListGroupsInput = ListGroupsInput 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListGroupsOutput = ListGroupsOutput 
  { "Groups" :: NullOrUndefined (GroupList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype MaxResults = MaxResults Int


-- | <p>The request uses an HTTP method which is not allowed for the specified resource.</p>
newtype MethodNotAllowedException = MethodNotAllowedException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype NextToken = NextToken String


-- | <p>One or more resources specified in the request do not exist.</p>
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype Query = Query String


newtype QueryType = QueryType String


newtype ResourceArn = ResourceArn String


-- | <p>The ARN of a resource, and its resource type.</p>
newtype ResourceIdentifier = ResourceIdentifier 
  { "ResourceArn" :: NullOrUndefined (ResourceArn)
  , "ResourceType" :: NullOrUndefined (ResourceType)
  }


newtype ResourceIdentifierList = ResourceIdentifierList (Array ResourceIdentifier)


-- | <p>The query that is used to define a resource group or a search for resources.</p>
newtype ResourceQuery = ResourceQuery 
  { "Type" :: (QueryType)
  , "Query" :: (Query)
  }


newtype ResourceType = ResourceType String


newtype SearchResourcesInput = SearchResourcesInput 
  { "ResourceQuery" :: (ResourceQuery)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype SearchResourcesOutput = SearchResourcesOutput 
  { "ResourceIdentifiers" :: NullOrUndefined (ResourceIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype TagInput = TagInput 
  { "Arn" :: (GroupArn)
  , "Tags" :: (Tags)
  }


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array TagKey)


newtype TagOutput = TagOutput 
  { "Arn" :: NullOrUndefined (GroupArn)
  , "Tags" :: NullOrUndefined (Tags)
  }


newtype TagValue = TagValue String


newtype Tags = Tags (Map TagKey TagValue)


-- | <p>The caller has exceeded throttling limits.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The request has not been applied because it lacks valid authentication credentials for the target resource.</p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype UntagInput = UntagInput 
  { "Arn" :: (GroupArn)
  , "Keys" :: (TagKeyList)
  }


newtype UntagOutput = UntagOutput 
  { "Arn" :: NullOrUndefined (GroupArn)
  , "Keys" :: NullOrUndefined (TagKeyList)
  }


newtype UpdateGroupInput = UpdateGroupInput 
  { "GroupName" :: (GroupName)
  , "Description" :: NullOrUndefined (GroupDescription)
  }


newtype UpdateGroupOutput = UpdateGroupOutput 
  { "Group" :: NullOrUndefined (Group)
  }


newtype UpdateGroupQueryInput = UpdateGroupQueryInput 
  { "GroupName" :: (GroupName)
  , "ResourceQuery" :: (ResourceQuery)
  }


newtype UpdateGroupQueryOutput = UpdateGroupQueryOutput 
  { "GroupQuery" :: NullOrUndefined (GroupQuery)
  }
