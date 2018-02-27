## Module AWS.ResourceGroups

<fullname>AWS Resource Groups</fullname> <p>AWS Resource Groups lets you organize AWS resources such as Amazon EC2 instances, Amazon Relational Database Service databases, and Amazon S3 buckets into groups using criteria that you define as tags. A resource group is a collection of resources that match the resource types specified in a query, and share one or more tags or portions of tags. You can create a group of resources based on their roles in your cloud infrastructure, lifecycle stages, regions, application layers, or virtually any criteria. Resource groups enable you to automate management tasks, such as those in AWS Systems Manager Automation documents, on tag-related resources in AWS Systems Manager. Groups of tagged resources also let you quickly view a custom console in AWS Systems Manager that shows AWS Config compliance and other monitoring data about member resources.</p> <p>To create a resource group, build a resource query, and specify tags that identify the criteria that members of the group have in common. Tags are key-value pairs.</p> <p>For more information about Resource Groups, see the <a href="https://docs.aws.amazon.com/ARG/latest/userguide/welcome.html">AWS Resource Groups User Guide</a>.</p> <p>AWS Resource Groups uses a REST-compliant API that you can use to perform the following types of operations.</p> <ul> <li> <p>Create, Read, Update, and Delete (CRUD) operations on resource groups and resource query entities</p> </li> <li> <p>Applying, editing, and removing tags from resource groups</p> </li> <li> <p>Resolving resource group member ARNs so they can be returned as search results</p> </li> <li> <p>Getting data about resources that are members of a group</p> </li> <li> <p>Searching AWS resources based on a resource query</p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createGroup`

``` purescript
createGroup :: forall eff. CreateGroupInput -> Aff (err :: RequestError | eff) CreateGroupOutput
```

<p>Creates a group with a specified name, description, and resource query.</p>

#### `deleteGroup`

``` purescript
deleteGroup :: forall eff. DeleteGroupInput -> Aff (err :: RequestError | eff) DeleteGroupOutput
```

<p>Deletes a specified resource group. Deleting a resource group does not delete resources that are members of the group; it only deletes the group structure.</p>

#### `getGroup`

``` purescript
getGroup :: forall eff. GetGroupInput -> Aff (err :: RequestError | eff) GetGroupOutput
```

<p>Returns information about a specified resource group.</p>

#### `getGroupQuery`

``` purescript
getGroupQuery :: forall eff. GetGroupQueryInput -> Aff (err :: RequestError | eff) GetGroupQueryOutput
```

<p>Returns the resource query associated with the specified resource group.</p>

#### `getTags`

``` purescript
getTags :: forall eff. GetTagsInput -> Aff (err :: RequestError | eff) GetTagsOutput
```

<p>Returns a list of tags that are associated with a resource, specified by an ARN.</p>

#### `listGroupResources`

``` purescript
listGroupResources :: forall eff. ListGroupResourcesInput -> Aff (err :: RequestError | eff) ListGroupResourcesOutput
```

<p>Returns a list of ARNs of resources that are members of a specified resource group.</p>

#### `listGroups`

``` purescript
listGroups :: forall eff. ListGroupsInput -> Aff (err :: RequestError | eff) ListGroupsOutput
```

<p>Returns a list of existing resource groups in your account.</p>

#### `searchResources`

``` purescript
searchResources :: forall eff. SearchResourcesInput -> Aff (err :: RequestError | eff) SearchResourcesOutput
```

<p>Returns a list of AWS resource identifiers that matches a specified query. The query uses the same format as a resource query in a CreateGroup or UpdateGroupQuery operation.</p>

#### `tag`

``` purescript
tag :: forall eff. TagInput -> Aff (err :: RequestError | eff) TagOutput
```

<p>Adds specified tags to a resource with the specified ARN. Existing tags on a resource are not changed if they are not specified in the request parameters.</p>

#### `untag`

``` purescript
untag :: forall eff. UntagInput -> Aff (err :: RequestError | eff) UntagOutput
```

<p>Deletes specified tags from a specified resource.</p>

#### `updateGroup`

``` purescript
updateGroup :: forall eff. UpdateGroupInput -> Aff (err :: RequestError | eff) UpdateGroupOutput
```

<p>Updates an existing group with a new or changed description. You cannot update the name of a resource group.</p>

#### `updateGroupQuery`

``` purescript
updateGroupQuery :: forall eff. UpdateGroupQueryInput -> Aff (err :: RequestError | eff) UpdateGroupQueryOutput
```

<p>Updates the resource query of a group.</p>

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The request does not comply with validation rules that are defined for the request parameters.</p>

##### Instances
``` purescript
Newtype BadRequestException _
```

#### `CreateGroupInput`

``` purescript
newtype CreateGroupInput
  = CreateGroupInput { "Name" :: GroupName, "Description" :: NullOrUndefined (GroupDescription), "ResourceQuery" :: ResourceQuery, "Tags" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype CreateGroupInput _
```

#### `CreateGroupOutput`

``` purescript
newtype CreateGroupOutput
  = CreateGroupOutput { "Group" :: NullOrUndefined (Group), "ResourceQuery" :: NullOrUndefined (ResourceQuery), "Tags" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype CreateGroupOutput _
```

#### `DeleteGroupInput`

``` purescript
newtype DeleteGroupInput
  = DeleteGroupInput { "GroupName" :: GroupName }
```

##### Instances
``` purescript
Newtype DeleteGroupInput _
```

#### `DeleteGroupOutput`

``` purescript
newtype DeleteGroupOutput
  = DeleteGroupOutput { "Group" :: NullOrUndefined (Group) }
```

##### Instances
``` purescript
Newtype DeleteGroupOutput _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `ForbiddenException`

``` purescript
newtype ForbiddenException
  = ForbiddenException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The caller is not authorized to make the request.</p>

##### Instances
``` purescript
Newtype ForbiddenException _
```

#### `GetGroupInput`

``` purescript
newtype GetGroupInput
  = GetGroupInput { "GroupName" :: GroupName }
```

##### Instances
``` purescript
Newtype GetGroupInput _
```

#### `GetGroupOutput`

``` purescript
newtype GetGroupOutput
  = GetGroupOutput { "Group" :: NullOrUndefined (Group) }
```

##### Instances
``` purescript
Newtype GetGroupOutput _
```

#### `GetGroupQueryInput`

``` purescript
newtype GetGroupQueryInput
  = GetGroupQueryInput { "GroupName" :: GroupName }
```

##### Instances
``` purescript
Newtype GetGroupQueryInput _
```

#### `GetGroupQueryOutput`

``` purescript
newtype GetGroupQueryOutput
  = GetGroupQueryOutput { "GroupQuery" :: NullOrUndefined (GroupQuery) }
```

##### Instances
``` purescript
Newtype GetGroupQueryOutput _
```

#### `GetTagsInput`

``` purescript
newtype GetTagsInput
  = GetTagsInput { "Arn" :: GroupArn }
```

##### Instances
``` purescript
Newtype GetTagsInput _
```

#### `GetTagsOutput`

``` purescript
newtype GetTagsOutput
  = GetTagsOutput { "Arn" :: NullOrUndefined (GroupArn), "Tags" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype GetTagsOutput _
```

#### `Group`

``` purescript
newtype Group
  = Group { "GroupArn" :: GroupArn, "Name" :: GroupName, "Description" :: NullOrUndefined (GroupDescription) }
```

<p>A resource group.</p>

##### Instances
``` purescript
Newtype Group _
```

#### `GroupArn`

``` purescript
newtype GroupArn
  = GroupArn String
```

##### Instances
``` purescript
Newtype GroupArn _
```

#### `GroupDescription`

``` purescript
newtype GroupDescription
  = GroupDescription String
```

##### Instances
``` purescript
Newtype GroupDescription _
```

#### `GroupList`

``` purescript
newtype GroupList
  = GroupList (Array Group)
```

##### Instances
``` purescript
Newtype GroupList _
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

#### `GroupQuery`

``` purescript
newtype GroupQuery
  = GroupQuery { "GroupName" :: GroupName, "ResourceQuery" :: ResourceQuery }
```

<p>The underlying resource query of a resource group. Resources that match query results are part of the group.</p>

##### Instances
``` purescript
Newtype GroupQuery _
```

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>An internal error occurred while processing the request.</p>

##### Instances
``` purescript
Newtype InternalServerErrorException _
```

#### `ListGroupResourcesInput`

``` purescript
newtype ListGroupResourcesInput
  = ListGroupResourcesInput { "GroupName" :: GroupName, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListGroupResourcesInput _
```

#### `ListGroupResourcesOutput`

``` purescript
newtype ListGroupResourcesOutput
  = ListGroupResourcesOutput { "ResourceIdentifiers" :: NullOrUndefined (ResourceIdentifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListGroupResourcesOutput _
```

#### `ListGroupsInput`

``` purescript
newtype ListGroupsInput
  = ListGroupsInput { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListGroupsInput _
```

#### `ListGroupsOutput`

``` purescript
newtype ListGroupsOutput
  = ListGroupsOutput { "Groups" :: NullOrUndefined (GroupList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListGroupsOutput _
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

#### `MethodNotAllowedException`

``` purescript
newtype MethodNotAllowedException
  = MethodNotAllowedException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The request uses an HTTP method which is not allowed for the specified resource.</p>

##### Instances
``` purescript
Newtype MethodNotAllowedException _
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

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>One or more resources specified in the request do not exist.</p>

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `Query`

``` purescript
newtype Query
  = Query String
```

##### Instances
``` purescript
Newtype Query _
```

#### `QueryType`

``` purescript
newtype QueryType
  = QueryType String
```

##### Instances
``` purescript
Newtype QueryType _
```

#### `ResourceArn`

``` purescript
newtype ResourceArn
  = ResourceArn String
```

##### Instances
``` purescript
Newtype ResourceArn _
```

#### `ResourceIdentifier`

``` purescript
newtype ResourceIdentifier
  = ResourceIdentifier { "ResourceArn" :: NullOrUndefined (ResourceArn), "ResourceType" :: NullOrUndefined (ResourceType) }
```

<p>The ARN of a resource, and its resource type.</p>

##### Instances
``` purescript
Newtype ResourceIdentifier _
```

#### `ResourceIdentifierList`

``` purescript
newtype ResourceIdentifierList
  = ResourceIdentifierList (Array ResourceIdentifier)
```

##### Instances
``` purescript
Newtype ResourceIdentifierList _
```

#### `ResourceQuery`

``` purescript
newtype ResourceQuery
  = ResourceQuery { "Type" :: QueryType, "Query" :: Query }
```

<p>The query that is used to define a resource group or a search for resources.</p>

##### Instances
``` purescript
Newtype ResourceQuery _
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

#### `SearchResourcesInput`

``` purescript
newtype SearchResourcesInput
  = SearchResourcesInput { "ResourceQuery" :: ResourceQuery, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype SearchResourcesInput _
```

#### `SearchResourcesOutput`

``` purescript
newtype SearchResourcesOutput
  = SearchResourcesOutput { "ResourceIdentifiers" :: NullOrUndefined (ResourceIdentifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype SearchResourcesOutput _
```

#### `TagInput`

``` purescript
newtype TagInput
  = TagInput { "Arn" :: GroupArn, "Tags" :: Tags }
```

##### Instances
``` purescript
Newtype TagInput _
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

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

##### Instances
``` purescript
Newtype TagKeyList _
```

#### `TagOutput`

``` purescript
newtype TagOutput
  = TagOutput { "Arn" :: NullOrUndefined (GroupArn), "Tags" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype TagOutput _
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

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The caller has exceeded throttling limits.</p>

##### Instances
``` purescript
Newtype TooManyRequestsException _
```

#### `UnauthorizedException`

``` purescript
newtype UnauthorizedException
  = UnauthorizedException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The request has not been applied because it lacks valid authentication credentials for the target resource.</p>

##### Instances
``` purescript
Newtype UnauthorizedException _
```

#### `UntagInput`

``` purescript
newtype UntagInput
  = UntagInput { "Arn" :: GroupArn, "Keys" :: TagKeyList }
```

##### Instances
``` purescript
Newtype UntagInput _
```

#### `UntagOutput`

``` purescript
newtype UntagOutput
  = UntagOutput { "Arn" :: NullOrUndefined (GroupArn), "Keys" :: NullOrUndefined (TagKeyList) }
```

##### Instances
``` purescript
Newtype UntagOutput _
```

#### `UpdateGroupInput`

``` purescript
newtype UpdateGroupInput
  = UpdateGroupInput { "GroupName" :: GroupName, "Description" :: NullOrUndefined (GroupDescription) }
```

##### Instances
``` purescript
Newtype UpdateGroupInput _
```

#### `UpdateGroupOutput`

``` purescript
newtype UpdateGroupOutput
  = UpdateGroupOutput { "Group" :: NullOrUndefined (Group) }
```

##### Instances
``` purescript
Newtype UpdateGroupOutput _
```

#### `UpdateGroupQueryInput`

``` purescript
newtype UpdateGroupQueryInput
  = UpdateGroupQueryInput { "GroupName" :: GroupName, "ResourceQuery" :: ResourceQuery }
```

##### Instances
``` purescript
Newtype UpdateGroupQueryInput _
```

#### `UpdateGroupQueryOutput`

``` purescript
newtype UpdateGroupQueryOutput
  = UpdateGroupQueryOutput { "GroupQuery" :: NullOrUndefined (GroupQuery) }
```

##### Instances
``` purescript
Newtype UpdateGroupQueryOutput _
```


