## Module AWS.ResourceGroupsTaggingAPI

<fullname>Resource Groups Tagging API</fullname> <p>This guide describes the API operations for the resource groups tagging.</p> <p>A tag is a label that you assign to an AWS resource. A tag consists of a key and a value, both of which you define. For example, if you have two Amazon EC2 instances, you might assign both a tag key of "Stack." But the value of "Stack" might be "Testing" for one and "Production" for the other.</p> <p>Tagging can help you organize your resources and enables you to simplify resource management, access management and cost allocation. For more information about tagging, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/tag-editor.html">Working with Tag Editor</a> and <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/resource-groups.html">Working with Resource Groups</a>. For more information about permissions you need to use the resource groups tagging APIs, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-resource-groups.html">Obtaining Permissions for Resource Groups </a> and <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html">Obtaining Permissions for Tagging </a>.</p> <p>You can use the resource groups tagging APIs to complete the following tasks:</p> <ul> <li> <p>Tag and untag supported resources located in the specified region for the AWS account</p> </li> <li> <p>Use tag-based filters to search for resources located in the specified region for the AWS account</p> </li> <li> <p>List all existing tag keys in the specified region for the AWS account</p> </li> <li> <p>List all existing values for the specified key in the specified region for the AWS account</p> </li> </ul> <p>Not all resources can have tags. For a lists of resources that you can tag, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/supported-resources.html">Supported Resources</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p> <p>To make full use of the resource groups tagging APIs, you might need additional IAM permissions, including permission to access the resources of individual services as well as permission to view and apply tags to those resources. For more information, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html">Obtaining Permissions for Tagging</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `getResources`

``` purescript
getResources :: forall eff. GetResourcesInput -> Aff (err :: RequestError | eff) GetResourcesOutput
```

<p>Returns all the tagged resources that are associated with the specified tags (keys and values) located in the specified region for the AWS account. The tags and the resource types that you specify in the request are known as <i>filters</i>. The response includes all tags that are associated with the requested resources. If no filter is provided, this action returns a paginated resource list with the associated tags.</p>

#### `getTagKeys`

``` purescript
getTagKeys :: forall eff. GetTagKeysInput -> Aff (err :: RequestError | eff) GetTagKeysOutput
```

<p>Returns all tag keys in the specified region for the AWS account.</p>

#### `getTagValues`

``` purescript
getTagValues :: forall eff. GetTagValuesInput -> Aff (err :: RequestError | eff) GetTagValuesOutput
```

<p>Returns all tag values for the specified key in the specified region for the AWS account.</p>

#### `tagResources`

``` purescript
tagResources :: forall eff. TagResourcesInput -> Aff (err :: RequestError | eff) TagResourcesOutput
```

<p>Applies one or more tags to the specified resources. Note the following:</p> <ul> <li> <p>Not all resources can have tags. For a list of resources that support tagging, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/supported-resources.html">Supported Resources</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p> </li> <li> <p>Each resource can have up to 50 tags. For other limits, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-restrictions">Tag Restrictions</a> in the <i>Amazon EC2 User Guide for Linux Instances</i>.</p> </li> <li> <p>You can only tag resources that are located in the specified region for the AWS account.</p> </li> <li> <p>To add tags to a resource, you need the necessary permissions for the service that the resource belongs to as well as permissions for adding tags. For more information, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html">Obtaining Permissions for Tagging</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p> </li> </ul>

#### `untagResources`

``` purescript
untagResources :: forall eff. UntagResourcesInput -> Aff (err :: RequestError | eff) UntagResourcesOutput
```

<p>Removes the specified tags from the specified resources. When you specify a tag key, the action removes both that key and its associated value. The operation succeeds even if you attempt to remove tags from a resource that were already removed. Note the following:</p> <ul> <li> <p>To remove tags from a resource, you need the necessary permissions for the service that the resource belongs to as well as permissions for removing tags. For more information, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html">Obtaining Permissions for Tagging</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p> </li> <li> <p>You can only tag resources that are located in the specified region for the AWS account.</p> </li> </ul>

#### `AmazonResourceType`

``` purescript
newtype AmazonResourceType
  = AmazonResourceType String
```

#### `ErrorCode`

``` purescript
newtype ErrorCode
  = ErrorCode String
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

#### `FailedResourcesMap`

``` purescript
newtype FailedResourcesMap
  = FailedResourcesMap (Map ResourceARN FailureInfo)
```

#### `FailureInfo`

``` purescript
newtype FailureInfo
  = FailureInfo { "StatusCode" :: NullOrUndefined (StatusCode), "ErrorCode" :: NullOrUndefined (ErrorCode), "ErrorMessage" :: NullOrUndefined (ErrorMessage) }
```

<p>Details of the common errors that all actions return.</p>

#### `GetResourcesInput`

``` purescript
newtype GetResourcesInput
  = GetResourcesInput { "PaginationToken" :: NullOrUndefined (PaginationToken), "TagFilters" :: NullOrUndefined (TagFilterList), "ResourcesPerPage" :: NullOrUndefined (ResourcesPerPage), "TagsPerPage" :: NullOrUndefined (TagsPerPage), "ResourceTypeFilters" :: NullOrUndefined (ResourceTypeFilterList) }
```

#### `GetResourcesOutput`

``` purescript
newtype GetResourcesOutput
  = GetResourcesOutput { "PaginationToken" :: NullOrUndefined (PaginationToken), "ResourceTagMappingList" :: NullOrUndefined (ResourceTagMappingList) }
```

#### `GetTagKeysInput`

``` purescript
newtype GetTagKeysInput
  = GetTagKeysInput { "PaginationToken" :: NullOrUndefined (PaginationToken) }
```

#### `GetTagKeysOutput`

``` purescript
newtype GetTagKeysOutput
  = GetTagKeysOutput { "PaginationToken" :: NullOrUndefined (PaginationToken), "TagKeys" :: NullOrUndefined (TagKeyList) }
```

#### `GetTagValuesInput`

``` purescript
newtype GetTagValuesInput
  = GetTagValuesInput { "PaginationToken" :: NullOrUndefined (PaginationToken), "Key" :: TagKey }
```

#### `GetTagValuesOutput`

``` purescript
newtype GetTagValuesOutput
  = GetTagValuesOutput { "PaginationToken" :: NullOrUndefined (PaginationToken), "TagValues" :: NullOrUndefined (TagValuesOutputList) }
```

#### `InternalServiceException`

``` purescript
newtype InternalServiceException
  = InternalServiceException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The request processing failed because of an unknown error, exception, or failure. You can retry the request.</p>

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>A parameter is missing or a malformed string or invalid or out-of-range value was supplied for the request parameter.</p>

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

#### `PaginationTokenExpiredException`

``` purescript
newtype PaginationTokenExpiredException
  = PaginationTokenExpiredException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>A <code>PaginationToken</code> is valid for a maximum of 15 minutes. Your request was denied because the specified <code>PaginationToken</code> has expired.</p>

#### `ResourceARN`

``` purescript
newtype ResourceARN
  = ResourceARN String
```

#### `ResourceARNList`

``` purescript
newtype ResourceARNList
  = ResourceARNList (Array ResourceARN)
```

#### `ResourceTagMapping`

``` purescript
newtype ResourceTagMapping
  = ResourceTagMapping { "ResourceARN" :: NullOrUndefined (ResourceARN), "Tags" :: NullOrUndefined (TagList) }
```

<p>A list of resource ARNs and the tags (keys and values) that are associated with each.</p>

#### `ResourceTagMappingList`

``` purescript
newtype ResourceTagMappingList
  = ResourceTagMappingList (Array ResourceTagMapping)
```

#### `ResourceTypeFilterList`

``` purescript
newtype ResourceTypeFilterList
  = ResourceTypeFilterList (Array AmazonResourceType)
```

#### `ResourcesPerPage`

``` purescript
newtype ResourcesPerPage
  = ResourcesPerPage Int
```

#### `StatusCode`

``` purescript
newtype StatusCode
  = StatusCode Int
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: TagValue }
```

<p>The metadata that you apply to AWS resources to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-basics">Tag Basics</a> in the <i>Amazon EC2 User Guide for Linux Instances</i>.</p>

#### `TagFilter`

``` purescript
newtype TagFilter
  = TagFilter { "Key" :: NullOrUndefined (TagKey), "Values" :: NullOrUndefined (TagValueList) }
```

<p>A list of tags (keys and values) that are used to specify the associated resources.</p>

#### `TagFilterList`

``` purescript
newtype TagFilterList
  = TagFilterList (Array TagFilter)
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

#### `TagKeyListForUntag`

``` purescript
newtype TagKeyListForUntag
  = TagKeyListForUntag (Array TagKey)
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

#### `TagMap`

``` purescript
newtype TagMap
  = TagMap (Map TagKey TagValue)
```

#### `TagResourcesInput`

``` purescript
newtype TagResourcesInput
  = TagResourcesInput { "ResourceARNList" :: ResourceARNList, "Tags" :: TagMap }
```

#### `TagResourcesOutput`

``` purescript
newtype TagResourcesOutput
  = TagResourcesOutput { "FailedResourcesMap" :: NullOrUndefined (FailedResourcesMap) }
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `TagValueList`

``` purescript
newtype TagValueList
  = TagValueList (Array TagValue)
```

#### `TagValuesOutputList`

``` purescript
newtype TagValuesOutputList
  = TagValuesOutputList (Array TagValue)
```

#### `TagsPerPage`

``` purescript
newtype TagsPerPage
  = TagsPerPage Int
```

#### `ThrottledException`

``` purescript
newtype ThrottledException
  = ThrottledException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The request was denied to limit the frequency of submitted requests.</p>

#### `UntagResourcesInput`

``` purescript
newtype UntagResourcesInput
  = UntagResourcesInput { "ResourceARNList" :: ResourceARNList, "TagKeys" :: TagKeyListForUntag }
```

#### `UntagResourcesOutput`

``` purescript
newtype UntagResourcesOutput
  = UntagResourcesOutput { "FailedResourcesMap" :: NullOrUndefined (FailedResourcesMap) }
```


