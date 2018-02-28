

-- | <fullname>AWS Resource Groups</fullname> <p>AWS Resource Groups lets you organize AWS resources such as Amazon EC2 instances, Amazon Relational Database Service databases, and Amazon S3 buckets into groups using criteria that you define as tags. A resource group is a collection of resources that match the resource types specified in a query, and share one or more tags or portions of tags. You can create a group of resources based on their roles in your cloud infrastructure, lifecycle stages, regions, application layers, or virtually any criteria. Resource groups enable you to automate management tasks, such as those in AWS Systems Manager Automation documents, on tag-related resources in AWS Systems Manager. Groups of tagged resources also let you quickly view a custom console in AWS Systems Manager that shows AWS Config compliance and other monitoring data about member resources.</p> <p>To create a resource group, build a resource query, and specify tags that identify the criteria that members of the group have in common. Tags are key-value pairs.</p> <p>For more information about Resource Groups, see the <a href="https://docs.aws.amazon.com/ARG/latest/userguide/welcome.html">AWS Resource Groups User Guide</a>.</p> <p>AWS Resource Groups uses a REST-compliant API that you can use to perform the following types of operations.</p> <ul> <li> <p>Create, Read, Update, and Delete (CRUD) operations on resource groups and resource query entities</p> </li> <li> <p>Applying, editing, and removing tags from resource groups</p> </li> <li> <p>Resolving resource group member ARNs so they can be returned as search results</p> </li> <li> <p>Getting data about resources that are members of a group</p> </li> <li> <p>Searching AWS resources based on a resource query</p> </li> </ul>
module AWS.ResourceGroups where

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

serviceName = "ResourceGroups" :: String


-- | <p>Creates a group with a specified name, description, and resource query.</p>
createGroup :: forall eff. CreateGroupInput -> Aff (exception :: EXCEPTION | eff) CreateGroupOutput
createGroup = Request.request serviceName "createGroup" 


-- | <p>Deletes a specified resource group. Deleting a resource group does not delete resources that are members of the group; it only deletes the group structure.</p>
deleteGroup :: forall eff. DeleteGroupInput -> Aff (exception :: EXCEPTION | eff) DeleteGroupOutput
deleteGroup = Request.request serviceName "deleteGroup" 


-- | <p>Returns information about a specified resource group.</p>
getGroup :: forall eff. GetGroupInput -> Aff (exception :: EXCEPTION | eff) GetGroupOutput
getGroup = Request.request serviceName "getGroup" 


-- | <p>Returns the resource query associated with the specified resource group.</p>
getGroupQuery :: forall eff. GetGroupQueryInput -> Aff (exception :: EXCEPTION | eff) GetGroupQueryOutput
getGroupQuery = Request.request serviceName "getGroupQuery" 


-- | <p>Returns a list of tags that are associated with a resource, specified by an ARN.</p>
getTags :: forall eff. GetTagsInput -> Aff (exception :: EXCEPTION | eff) GetTagsOutput
getTags = Request.request serviceName "getTags" 


-- | <p>Returns a list of ARNs of resources that are members of a specified resource group.</p>
listGroupResources :: forall eff. ListGroupResourcesInput -> Aff (exception :: EXCEPTION | eff) ListGroupResourcesOutput
listGroupResources = Request.request serviceName "listGroupResources" 


-- | <p>Returns a list of existing resource groups in your account.</p>
listGroups :: forall eff. ListGroupsInput -> Aff (exception :: EXCEPTION | eff) ListGroupsOutput
listGroups = Request.request serviceName "listGroups" 


-- | <p>Returns a list of AWS resource identifiers that matches a specified query. The query uses the same format as a resource query in a CreateGroup or UpdateGroupQuery operation.</p>
searchResources :: forall eff. SearchResourcesInput -> Aff (exception :: EXCEPTION | eff) SearchResourcesOutput
searchResources = Request.request serviceName "searchResources" 


-- | <p>Adds specified tags to a resource with the specified ARN. Existing tags on a resource are not changed if they are not specified in the request parameters.</p>
tag :: forall eff. TagInput -> Aff (exception :: EXCEPTION | eff) TagOutput
tag = Request.request serviceName "tag" 


-- | <p>Deletes specified tags from a specified resource.</p>
untag :: forall eff. UntagInput -> Aff (exception :: EXCEPTION | eff) UntagOutput
untag = Request.request serviceName "untag" 


-- | <p>Updates an existing group with a new or changed description. You cannot update the name of a resource group.</p>
updateGroup :: forall eff. UpdateGroupInput -> Aff (exception :: EXCEPTION | eff) UpdateGroupOutput
updateGroup = Request.request serviceName "updateGroup" 


-- | <p>Updates the resource query of a group.</p>
updateGroupQuery :: forall eff. UpdateGroupQueryInput -> Aff (exception :: EXCEPTION | eff) UpdateGroupQueryOutput
updateGroupQuery = Request.request serviceName "updateGroupQuery" 


-- | <p>The request does not comply with validation rules that are defined for the request parameters.</p>
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _
derive instance repGenericBadRequestException :: Generic BadRequestException _
instance showBadRequestException :: Show BadRequestException where
  show = genericShow
instance decodeBadRequestException :: Decode BadRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequestException :: Encode BadRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGroupInput = CreateGroupInput 
  { "Name" :: (GroupName)
  , "Description" :: NullOrUndefined.NullOrUndefined (GroupDescription)
  , "ResourceQuery" :: (ResourceQuery)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  }
derive instance newtypeCreateGroupInput :: Newtype CreateGroupInput _
derive instance repGenericCreateGroupInput :: Generic CreateGroupInput _
instance showCreateGroupInput :: Show CreateGroupInput where
  show = genericShow
instance decodeCreateGroupInput :: Decode CreateGroupInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGroupInput :: Encode CreateGroupInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGroupOutput = CreateGroupOutput 
  { "Group" :: NullOrUndefined.NullOrUndefined (Group)
  , "ResourceQuery" :: NullOrUndefined.NullOrUndefined (ResourceQuery)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  }
derive instance newtypeCreateGroupOutput :: Newtype CreateGroupOutput _
derive instance repGenericCreateGroupOutput :: Generic CreateGroupOutput _
instance showCreateGroupOutput :: Show CreateGroupOutput where
  show = genericShow
instance decodeCreateGroupOutput :: Decode CreateGroupOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGroupOutput :: Encode CreateGroupOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGroupInput = DeleteGroupInput 
  { "GroupName" :: (GroupName)
  }
derive instance newtypeDeleteGroupInput :: Newtype DeleteGroupInput _
derive instance repGenericDeleteGroupInput :: Generic DeleteGroupInput _
instance showDeleteGroupInput :: Show DeleteGroupInput where
  show = genericShow
instance decodeDeleteGroupInput :: Decode DeleteGroupInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGroupInput :: Encode DeleteGroupInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGroupOutput = DeleteGroupOutput 
  { "Group" :: NullOrUndefined.NullOrUndefined (Group)
  }
derive instance newtypeDeleteGroupOutput :: Newtype DeleteGroupOutput _
derive instance repGenericDeleteGroupOutput :: Generic DeleteGroupOutput _
instance showDeleteGroupOutput :: Show DeleteGroupOutput where
  show = genericShow
instance decodeDeleteGroupOutput :: Decode DeleteGroupOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGroupOutput :: Encode DeleteGroupOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The caller is not authorized to make the request.</p>
newtype ForbiddenException = ForbiddenException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeForbiddenException :: Newtype ForbiddenException _
derive instance repGenericForbiddenException :: Generic ForbiddenException _
instance showForbiddenException :: Show ForbiddenException where
  show = genericShow
instance decodeForbiddenException :: Decode ForbiddenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeForbiddenException :: Encode ForbiddenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupInput = GetGroupInput 
  { "GroupName" :: (GroupName)
  }
derive instance newtypeGetGroupInput :: Newtype GetGroupInput _
derive instance repGenericGetGroupInput :: Generic GetGroupInput _
instance showGetGroupInput :: Show GetGroupInput where
  show = genericShow
instance decodeGetGroupInput :: Decode GetGroupInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupInput :: Encode GetGroupInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupOutput = GetGroupOutput 
  { "Group" :: NullOrUndefined.NullOrUndefined (Group)
  }
derive instance newtypeGetGroupOutput :: Newtype GetGroupOutput _
derive instance repGenericGetGroupOutput :: Generic GetGroupOutput _
instance showGetGroupOutput :: Show GetGroupOutput where
  show = genericShow
instance decodeGetGroupOutput :: Decode GetGroupOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupOutput :: Encode GetGroupOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupQueryInput = GetGroupQueryInput 
  { "GroupName" :: (GroupName)
  }
derive instance newtypeGetGroupQueryInput :: Newtype GetGroupQueryInput _
derive instance repGenericGetGroupQueryInput :: Generic GetGroupQueryInput _
instance showGetGroupQueryInput :: Show GetGroupQueryInput where
  show = genericShow
instance decodeGetGroupQueryInput :: Decode GetGroupQueryInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupQueryInput :: Encode GetGroupQueryInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupQueryOutput = GetGroupQueryOutput 
  { "GroupQuery" :: NullOrUndefined.NullOrUndefined (GroupQuery)
  }
derive instance newtypeGetGroupQueryOutput :: Newtype GetGroupQueryOutput _
derive instance repGenericGetGroupQueryOutput :: Generic GetGroupQueryOutput _
instance showGetGroupQueryOutput :: Show GetGroupQueryOutput where
  show = genericShow
instance decodeGetGroupQueryOutput :: Decode GetGroupQueryOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupQueryOutput :: Encode GetGroupQueryOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTagsInput = GetTagsInput 
  { "Arn" :: (GroupArn)
  }
derive instance newtypeGetTagsInput :: Newtype GetTagsInput _
derive instance repGenericGetTagsInput :: Generic GetTagsInput _
instance showGetTagsInput :: Show GetTagsInput where
  show = genericShow
instance decodeGetTagsInput :: Decode GetTagsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTagsInput :: Encode GetTagsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTagsOutput = GetTagsOutput 
  { "Arn" :: NullOrUndefined.NullOrUndefined (GroupArn)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  }
derive instance newtypeGetTagsOutput :: Newtype GetTagsOutput _
derive instance repGenericGetTagsOutput :: Generic GetTagsOutput _
instance showGetTagsOutput :: Show GetTagsOutput where
  show = genericShow
instance decodeGetTagsOutput :: Decode GetTagsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTagsOutput :: Encode GetTagsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A resource group.</p>
newtype Group = Group 
  { "GroupArn" :: (GroupArn)
  , "Name" :: (GroupName)
  , "Description" :: NullOrUndefined.NullOrUndefined (GroupDescription)
  }
derive instance newtypeGroup :: Newtype Group _
derive instance repGenericGroup :: Generic Group _
instance showGroup :: Show Group where
  show = genericShow
instance decodeGroup :: Decode Group where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroup :: Encode Group where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupArn = GroupArn String
derive instance newtypeGroupArn :: Newtype GroupArn _
derive instance repGenericGroupArn :: Generic GroupArn _
instance showGroupArn :: Show GroupArn where
  show = genericShow
instance decodeGroupArn :: Decode GroupArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupArn :: Encode GroupArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupDescription = GroupDescription String
derive instance newtypeGroupDescription :: Newtype GroupDescription _
derive instance repGenericGroupDescription :: Generic GroupDescription _
instance showGroupDescription :: Show GroupDescription where
  show = genericShow
instance decodeGroupDescription :: Decode GroupDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupDescription :: Encode GroupDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupList = GroupList (Array Group)
derive instance newtypeGroupList :: Newtype GroupList _
derive instance repGenericGroupList :: Generic GroupList _
instance showGroupList :: Show GroupList where
  show = genericShow
instance decodeGroupList :: Decode GroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupList :: Encode GroupList where
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


-- | <p>The underlying resource query of a resource group. Resources that match query results are part of the group.</p>
newtype GroupQuery = GroupQuery 
  { "GroupName" :: (GroupName)
  , "ResourceQuery" :: (ResourceQuery)
  }
derive instance newtypeGroupQuery :: Newtype GroupQuery _
derive instance repGenericGroupQuery :: Generic GroupQuery _
instance showGroupQuery :: Show GroupQuery where
  show = genericShow
instance decodeGroupQuery :: Decode GroupQuery where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupQuery :: Encode GroupQuery where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An internal error occurred while processing the request.</p>
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _
derive instance repGenericInternalServerErrorException :: Generic InternalServerErrorException _
instance showInternalServerErrorException :: Show InternalServerErrorException where
  show = genericShow
instance decodeInternalServerErrorException :: Decode InternalServerErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerErrorException :: Encode InternalServerErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupResourcesInput = ListGroupResourcesInput 
  { "GroupName" :: (GroupName)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListGroupResourcesInput :: Newtype ListGroupResourcesInput _
derive instance repGenericListGroupResourcesInput :: Generic ListGroupResourcesInput _
instance showListGroupResourcesInput :: Show ListGroupResourcesInput where
  show = genericShow
instance decodeListGroupResourcesInput :: Decode ListGroupResourcesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupResourcesInput :: Encode ListGroupResourcesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupResourcesOutput = ListGroupResourcesOutput 
  { "ResourceIdentifiers" :: NullOrUndefined.NullOrUndefined (ResourceIdentifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListGroupResourcesOutput :: Newtype ListGroupResourcesOutput _
derive instance repGenericListGroupResourcesOutput :: Generic ListGroupResourcesOutput _
instance showListGroupResourcesOutput :: Show ListGroupResourcesOutput where
  show = genericShow
instance decodeListGroupResourcesOutput :: Decode ListGroupResourcesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupResourcesOutput :: Encode ListGroupResourcesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupsInput = ListGroupsInput 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListGroupsInput :: Newtype ListGroupsInput _
derive instance repGenericListGroupsInput :: Generic ListGroupsInput _
instance showListGroupsInput :: Show ListGroupsInput where
  show = genericShow
instance decodeListGroupsInput :: Decode ListGroupsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupsInput :: Encode ListGroupsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupsOutput = ListGroupsOutput 
  { "Groups" :: NullOrUndefined.NullOrUndefined (GroupList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListGroupsOutput :: Newtype ListGroupsOutput _
derive instance repGenericListGroupsOutput :: Generic ListGroupsOutput _
instance showListGroupsOutput :: Show ListGroupsOutput where
  show = genericShow
instance decodeListGroupsOutput :: Decode ListGroupsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupsOutput :: Encode ListGroupsOutput where
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


-- | <p>The request uses an HTTP method which is not allowed for the specified resource.</p>
newtype MethodNotAllowedException = MethodNotAllowedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMethodNotAllowedException :: Newtype MethodNotAllowedException _
derive instance repGenericMethodNotAllowedException :: Generic MethodNotAllowedException _
instance showMethodNotAllowedException :: Show MethodNotAllowedException where
  show = genericShow
instance decodeMethodNotAllowedException :: Decode MethodNotAllowedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMethodNotAllowedException :: Encode MethodNotAllowedException where
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


-- | <p>One or more resources specified in the request do not exist.</p>
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Query = Query String
derive instance newtypeQuery :: Newtype Query _
derive instance repGenericQuery :: Generic Query _
instance showQuery :: Show Query where
  show = genericShow
instance decodeQuery :: Decode Query where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQuery :: Encode Query where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueryType = QueryType String
derive instance newtypeQueryType :: Newtype QueryType _
derive instance repGenericQueryType :: Generic QueryType _
instance showQueryType :: Show QueryType where
  show = genericShow
instance decodeQueryType :: Decode QueryType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryType :: Encode QueryType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _
derive instance repGenericResourceArn :: Generic ResourceArn _
instance showResourceArn :: Show ResourceArn where
  show = genericShow
instance decodeResourceArn :: Decode ResourceArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceArn :: Encode ResourceArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ARN of a resource, and its resource type.</p>
newtype ResourceIdentifier = ResourceIdentifier 
  { "ResourceArn" :: NullOrUndefined.NullOrUndefined (ResourceArn)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (ResourceType)
  }
derive instance newtypeResourceIdentifier :: Newtype ResourceIdentifier _
derive instance repGenericResourceIdentifier :: Generic ResourceIdentifier _
instance showResourceIdentifier :: Show ResourceIdentifier where
  show = genericShow
instance decodeResourceIdentifier :: Decode ResourceIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceIdentifier :: Encode ResourceIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceIdentifierList = ResourceIdentifierList (Array ResourceIdentifier)
derive instance newtypeResourceIdentifierList :: Newtype ResourceIdentifierList _
derive instance repGenericResourceIdentifierList :: Generic ResourceIdentifierList _
instance showResourceIdentifierList :: Show ResourceIdentifierList where
  show = genericShow
instance decodeResourceIdentifierList :: Decode ResourceIdentifierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceIdentifierList :: Encode ResourceIdentifierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The query that is used to define a resource group or a search for resources.</p>
newtype ResourceQuery = ResourceQuery 
  { "Type" :: (QueryType)
  , "Query" :: (Query)
  }
derive instance newtypeResourceQuery :: Newtype ResourceQuery _
derive instance repGenericResourceQuery :: Generic ResourceQuery _
instance showResourceQuery :: Show ResourceQuery where
  show = genericShow
instance decodeResourceQuery :: Decode ResourceQuery where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceQuery :: Encode ResourceQuery where
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


newtype SearchResourcesInput = SearchResourcesInput 
  { "ResourceQuery" :: (ResourceQuery)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeSearchResourcesInput :: Newtype SearchResourcesInput _
derive instance repGenericSearchResourcesInput :: Generic SearchResourcesInput _
instance showSearchResourcesInput :: Show SearchResourcesInput where
  show = genericShow
instance decodeSearchResourcesInput :: Decode SearchResourcesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchResourcesInput :: Encode SearchResourcesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchResourcesOutput = SearchResourcesOutput 
  { "ResourceIdentifiers" :: NullOrUndefined.NullOrUndefined (ResourceIdentifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeSearchResourcesOutput :: Newtype SearchResourcesOutput _
derive instance repGenericSearchResourcesOutput :: Generic SearchResourcesOutput _
instance showSearchResourcesOutput :: Show SearchResourcesOutput where
  show = genericShow
instance decodeSearchResourcesOutput :: Decode SearchResourcesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchResourcesOutput :: Encode SearchResourcesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagInput = TagInput 
  { "Arn" :: (GroupArn)
  , "Tags" :: (Tags)
  }
derive instance newtypeTagInput :: Newtype TagInput _
derive instance repGenericTagInput :: Generic TagInput _
instance showTagInput :: Show TagInput where
  show = genericShow
instance decodeTagInput :: Decode TagInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagInput :: Encode TagInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagOutput = TagOutput 
  { "Arn" :: NullOrUndefined.NullOrUndefined (GroupArn)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  }
derive instance newtypeTagOutput :: Newtype TagOutput _
derive instance repGenericTagOutput :: Generic TagOutput _
instance showTagOutput :: Show TagOutput where
  show = genericShow
instance decodeTagOutput :: Decode TagOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagOutput :: Encode TagOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tags = Tags (StrMap.StrMap TagValue)
derive instance newtypeTags :: Newtype Tags _
derive instance repGenericTags :: Generic Tags _
instance showTags :: Show Tags where
  show = genericShow
instance decodeTags :: Decode Tags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTags :: Encode Tags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The caller has exceeded throttling limits.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _
derive instance repGenericTooManyRequestsException :: Generic TooManyRequestsException _
instance showTooManyRequestsException :: Show TooManyRequestsException where
  show = genericShow
instance decodeTooManyRequestsException :: Decode TooManyRequestsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyRequestsException :: Encode TooManyRequestsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request has not been applied because it lacks valid authentication credentials for the target resource.</p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnauthorizedException :: Newtype UnauthorizedException _
derive instance repGenericUnauthorizedException :: Generic UnauthorizedException _
instance showUnauthorizedException :: Show UnauthorizedException where
  show = genericShow
instance decodeUnauthorizedException :: Decode UnauthorizedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnauthorizedException :: Encode UnauthorizedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UntagInput = UntagInput 
  { "Arn" :: (GroupArn)
  , "Keys" :: (TagKeyList)
  }
derive instance newtypeUntagInput :: Newtype UntagInput _
derive instance repGenericUntagInput :: Generic UntagInput _
instance showUntagInput :: Show UntagInput where
  show = genericShow
instance decodeUntagInput :: Decode UntagInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagInput :: Encode UntagInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UntagOutput = UntagOutput 
  { "Arn" :: NullOrUndefined.NullOrUndefined (GroupArn)
  , "Keys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  }
derive instance newtypeUntagOutput :: Newtype UntagOutput _
derive instance repGenericUntagOutput :: Generic UntagOutput _
instance showUntagOutput :: Show UntagOutput where
  show = genericShow
instance decodeUntagOutput :: Decode UntagOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagOutput :: Encode UntagOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGroupInput = UpdateGroupInput 
  { "GroupName" :: (GroupName)
  , "Description" :: NullOrUndefined.NullOrUndefined (GroupDescription)
  }
derive instance newtypeUpdateGroupInput :: Newtype UpdateGroupInput _
derive instance repGenericUpdateGroupInput :: Generic UpdateGroupInput _
instance showUpdateGroupInput :: Show UpdateGroupInput where
  show = genericShow
instance decodeUpdateGroupInput :: Decode UpdateGroupInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGroupInput :: Encode UpdateGroupInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGroupOutput = UpdateGroupOutput 
  { "Group" :: NullOrUndefined.NullOrUndefined (Group)
  }
derive instance newtypeUpdateGroupOutput :: Newtype UpdateGroupOutput _
derive instance repGenericUpdateGroupOutput :: Generic UpdateGroupOutput _
instance showUpdateGroupOutput :: Show UpdateGroupOutput where
  show = genericShow
instance decodeUpdateGroupOutput :: Decode UpdateGroupOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGroupOutput :: Encode UpdateGroupOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGroupQueryInput = UpdateGroupQueryInput 
  { "GroupName" :: (GroupName)
  , "ResourceQuery" :: (ResourceQuery)
  }
derive instance newtypeUpdateGroupQueryInput :: Newtype UpdateGroupQueryInput _
derive instance repGenericUpdateGroupQueryInput :: Generic UpdateGroupQueryInput _
instance showUpdateGroupQueryInput :: Show UpdateGroupQueryInput where
  show = genericShow
instance decodeUpdateGroupQueryInput :: Decode UpdateGroupQueryInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGroupQueryInput :: Encode UpdateGroupQueryInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGroupQueryOutput = UpdateGroupQueryOutput 
  { "GroupQuery" :: NullOrUndefined.NullOrUndefined (GroupQuery)
  }
derive instance newtypeUpdateGroupQueryOutput :: Newtype UpdateGroupQueryOutput _
derive instance repGenericUpdateGroupQueryOutput :: Generic UpdateGroupQueryOutput _
instance showUpdateGroupQueryOutput :: Show UpdateGroupQueryOutput where
  show = genericShow
instance decodeUpdateGroupQueryOutput :: Decode UpdateGroupQueryOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGroupQueryOutput :: Encode UpdateGroupQueryOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
