

-- | <fullname>Resource Groups Tagging API</fullname> <p>This guide describes the API operations for the resource groups tagging.</p> <p>A tag is a label that you assign to an AWS resource. A tag consists of a key and a value, both of which you define. For example, if you have two Amazon EC2 instances, you might assign both a tag key of "Stack." But the value of "Stack" might be "Testing" for one and "Production" for the other.</p> <p>Tagging can help you organize your resources and enables you to simplify resource management, access management and cost allocation. For more information about tagging, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/tag-editor.html">Working with Tag Editor</a> and <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/resource-groups.html">Working with Resource Groups</a>. For more information about permissions you need to use the resource groups tagging APIs, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-resource-groups.html">Obtaining Permissions for Resource Groups </a> and <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html">Obtaining Permissions for Tagging </a>.</p> <p>You can use the resource groups tagging APIs to complete the following tasks:</p> <ul> <li> <p>Tag and untag supported resources located in the specified region for the AWS account</p> </li> <li> <p>Use tag-based filters to search for resources located in the specified region for the AWS account</p> </li> <li> <p>List all existing tag keys in the specified region for the AWS account</p> </li> <li> <p>List all existing values for the specified key in the specified region for the AWS account</p> </li> </ul> <p>Not all resources can have tags. For a lists of resources that you can tag, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/supported-resources.html">Supported Resources</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p> <p>To make full use of the resource groups tagging APIs, you might need additional IAM permissions, including permission to access the resources of individual services as well as permission to view and apply tags to those resources. For more information, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html">Obtaining Permissions for Tagging</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p>
module AWS.ResourceGroupsTaggingAPI where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ResourceGroupsTaggingAPI" :: String


-- | <p>Returns all the tagged resources that are associated with the specified tags (keys and values) located in the specified region for the AWS account. The tags and the resource types that you specify in the request are known as <i>filters</i>. The response includes all tags that are associated with the requested resources. If no filter is provided, this action returns a paginated resource list with the associated tags.</p>
getResources :: forall eff. GetResourcesInput -> Aff (err :: AWS.RequestError | eff) GetResourcesOutput
getResources = AWS.request serviceName "GetResources" 


-- | <p>Returns all tag keys in the specified region for the AWS account.</p>
getTagKeys :: forall eff. GetTagKeysInput -> Aff (err :: AWS.RequestError | eff) GetTagKeysOutput
getTagKeys = AWS.request serviceName "GetTagKeys" 


-- | <p>Returns all tag values for the specified key in the specified region for the AWS account.</p>
getTagValues :: forall eff. GetTagValuesInput -> Aff (err :: AWS.RequestError | eff) GetTagValuesOutput
getTagValues = AWS.request serviceName "GetTagValues" 


-- | <p>Applies one or more tags to the specified resources. Note the following:</p> <ul> <li> <p>Not all resources can have tags. For a list of resources that support tagging, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/supported-resources.html">Supported Resources</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p> </li> <li> <p>Each resource can have up to 50 tags. For other limits, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-restrictions">Tag Restrictions</a> in the <i>Amazon EC2 User Guide for Linux Instances</i>.</p> </li> <li> <p>You can only tag resources that are located in the specified region for the AWS account.</p> </li> <li> <p>To add tags to a resource, you need the necessary permissions for the service that the resource belongs to as well as permissions for adding tags. For more information, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html">Obtaining Permissions for Tagging</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p> </li> </ul>
tagResources :: forall eff. TagResourcesInput -> Aff (err :: AWS.RequestError | eff) TagResourcesOutput
tagResources = AWS.request serviceName "TagResources" 


-- | <p>Removes the specified tags from the specified resources. When you specify a tag key, the action removes both that key and its associated value. The operation succeeds even if you attempt to remove tags from a resource that were already removed. Note the following:</p> <ul> <li> <p>To remove tags from a resource, you need the necessary permissions for the service that the resource belongs to as well as permissions for removing tags. For more information, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html">Obtaining Permissions for Tagging</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p> </li> <li> <p>You can only tag resources that are located in the specified region for the AWS account.</p> </li> </ul>
untagResources :: forall eff. UntagResourcesInput -> Aff (err :: AWS.RequestError | eff) UntagResourcesOutput
untagResources = AWS.request serviceName "UntagResources" 


newtype AmazonResourceType = AmazonResourceType String


newtype ErrorCode = ErrorCode String


newtype ErrorMessage = ErrorMessage String


newtype ExceptionMessage = ExceptionMessage String


newtype FailedResourcesMap = FailedResourcesMap (Map ResourceARN FailureInfo)


-- | <p>Details of the common errors that all actions return.</p>
newtype FailureInfo = FailureInfo 
  { "StatusCode" :: NullOrUndefined (StatusCode)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined (ErrorMessage)
  }


newtype GetResourcesInput = GetResourcesInput 
  { "PaginationToken" :: NullOrUndefined (PaginationToken)
  , "TagFilters" :: NullOrUndefined (TagFilterList)
  , "ResourcesPerPage" :: NullOrUndefined (ResourcesPerPage)
  , "TagsPerPage" :: NullOrUndefined (TagsPerPage)
  , "ResourceTypeFilters" :: NullOrUndefined (ResourceTypeFilterList)
  }


newtype GetResourcesOutput = GetResourcesOutput 
  { "PaginationToken" :: NullOrUndefined (PaginationToken)
  , "ResourceTagMappingList" :: NullOrUndefined (ResourceTagMappingList)
  }


newtype GetTagKeysInput = GetTagKeysInput 
  { "PaginationToken" :: NullOrUndefined (PaginationToken)
  }


newtype GetTagKeysOutput = GetTagKeysOutput 
  { "PaginationToken" :: NullOrUndefined (PaginationToken)
  , "TagKeys" :: NullOrUndefined (TagKeyList)
  }


newtype GetTagValuesInput = GetTagValuesInput 
  { "PaginationToken" :: NullOrUndefined (PaginationToken)
  , "Key" :: (TagKey)
  }


newtype GetTagValuesOutput = GetTagValuesOutput 
  { "PaginationToken" :: NullOrUndefined (PaginationToken)
  , "TagValues" :: NullOrUndefined (TagValuesOutputList)
  }


-- | <p>The request processing failed because of an unknown error, exception, or failure. You can retry the request.</p>
newtype InternalServiceException = InternalServiceException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>A parameter is missing or a malformed string or invalid or out-of-range value was supplied for the request parameter.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype PaginationToken = PaginationToken String


-- | <p>A <code>PaginationToken</code> is valid for a maximum of 15 minutes. Your request was denied because the specified <code>PaginationToken</code> has expired.</p>
newtype PaginationTokenExpiredException = PaginationTokenExpiredException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype ResourceARN = ResourceARN String


newtype ResourceARNList = ResourceARNList (Array ResourceARN)


-- | <p>A list of resource ARNs and the tags (keys and values) that are associated with each.</p>
newtype ResourceTagMapping = ResourceTagMapping 
  { "ResourceARN" :: NullOrUndefined (ResourceARN)
  , "Tags" :: NullOrUndefined (TagList)
  }


newtype ResourceTagMappingList = ResourceTagMappingList (Array ResourceTagMapping)


newtype ResourceTypeFilterList = ResourceTypeFilterList (Array AmazonResourceType)


newtype ResourcesPerPage = ResourcesPerPage Int


newtype StatusCode = StatusCode Int


-- | <p>The metadata that you apply to AWS resources to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-basics">Tag Basics</a> in the <i>Amazon EC2 User Guide for Linux Instances</i>.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }


-- | <p>A list of tags (keys and values) that are used to specify the associated resources.</p>
newtype TagFilter = TagFilter 
  { "Key" :: NullOrUndefined (TagKey)
  , "Values" :: NullOrUndefined (TagValueList)
  }


newtype TagFilterList = TagFilterList (Array TagFilter)


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array TagKey)


newtype TagKeyListForUntag = TagKeyListForUntag (Array TagKey)


newtype TagList = TagList (Array Tag)


newtype TagMap = TagMap (Map TagKey TagValue)


newtype TagResourcesInput = TagResourcesInput 
  { "ResourceARNList" :: (ResourceARNList)
  , "Tags" :: (TagMap)
  }


newtype TagResourcesOutput = TagResourcesOutput 
  { "FailedResourcesMap" :: NullOrUndefined (FailedResourcesMap)
  }


newtype TagValue = TagValue String


newtype TagValueList = TagValueList (Array TagValue)


newtype TagValuesOutputList = TagValuesOutputList (Array TagValue)


newtype TagsPerPage = TagsPerPage Int


-- | <p>The request was denied to limit the frequency of submitted requests.</p>
newtype ThrottledException = ThrottledException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype UntagResourcesInput = UntagResourcesInput 
  { "ResourceARNList" :: (ResourceARNList)
  , "TagKeys" :: (TagKeyListForUntag)
  }


newtype UntagResourcesOutput = UntagResourcesOutput 
  { "FailedResourcesMap" :: NullOrUndefined (FailedResourcesMap)
  }
