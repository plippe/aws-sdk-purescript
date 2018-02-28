

-- | <fullname>Resource Groups Tagging API</fullname> <p>This guide describes the API operations for the resource groups tagging.</p> <p>A tag is a label that you assign to an AWS resource. A tag consists of a key and a value, both of which you define. For example, if you have two Amazon EC2 instances, you might assign both a tag key of "Stack." But the value of "Stack" might be "Testing" for one and "Production" for the other.</p> <p>Tagging can help you organize your resources and enables you to simplify resource management, access management and cost allocation. For more information about tagging, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/tag-editor.html">Working with Tag Editor</a> and <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/resource-groups.html">Working with Resource Groups</a>. For more information about permissions you need to use the resource groups tagging APIs, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-resource-groups.html">Obtaining Permissions for Resource Groups </a> and <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html">Obtaining Permissions for Tagging </a>.</p> <p>You can use the resource groups tagging APIs to complete the following tasks:</p> <ul> <li> <p>Tag and untag supported resources located in the specified region for the AWS account</p> </li> <li> <p>Use tag-based filters to search for resources located in the specified region for the AWS account</p> </li> <li> <p>List all existing tag keys in the specified region for the AWS account</p> </li> <li> <p>List all existing values for the specified key in the specified region for the AWS account</p> </li> </ul> <p>Not all resources can have tags. For a lists of resources that you can tag, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/supported-resources.html">Supported Resources</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p> <p>To make full use of the resource groups tagging APIs, you might need additional IAM permissions, including permission to access the resources of individual services as well as permission to view and apply tags to those resources. For more information, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html">Obtaining Permissions for Tagging</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p>
module AWS.ResourceGroupsTaggingAPI where

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

serviceName = "ResourceGroupsTaggingAPI" :: String


-- | <p>Returns all the tagged resources that are associated with the specified tags (keys and values) located in the specified region for the AWS account. The tags and the resource types that you specify in the request are known as <i>filters</i>. The response includes all tags that are associated with the requested resources. If no filter is provided, this action returns a paginated resource list with the associated tags.</p>
getResources :: forall eff. GetResourcesInput -> Aff (exception :: EXCEPTION | eff) GetResourcesOutput
getResources = Request.request serviceName "getResources" 


-- | <p>Returns all tag keys in the specified region for the AWS account.</p>
getTagKeys :: forall eff. GetTagKeysInput -> Aff (exception :: EXCEPTION | eff) GetTagKeysOutput
getTagKeys = Request.request serviceName "getTagKeys" 


-- | <p>Returns all tag values for the specified key in the specified region for the AWS account.</p>
getTagValues :: forall eff. GetTagValuesInput -> Aff (exception :: EXCEPTION | eff) GetTagValuesOutput
getTagValues = Request.request serviceName "getTagValues" 


-- | <p>Applies one or more tags to the specified resources. Note the following:</p> <ul> <li> <p>Not all resources can have tags. For a list of resources that support tagging, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/supported-resources.html">Supported Resources</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p> </li> <li> <p>Each resource can have up to 50 tags. For other limits, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-restrictions">Tag Restrictions</a> in the <i>Amazon EC2 User Guide for Linux Instances</i>.</p> </li> <li> <p>You can only tag resources that are located in the specified region for the AWS account.</p> </li> <li> <p>To add tags to a resource, you need the necessary permissions for the service that the resource belongs to as well as permissions for adding tags. For more information, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html">Obtaining Permissions for Tagging</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p> </li> </ul>
tagResources :: forall eff. TagResourcesInput -> Aff (exception :: EXCEPTION | eff) TagResourcesOutput
tagResources = Request.request serviceName "tagResources" 


-- | <p>Removes the specified tags from the specified resources. When you specify a tag key, the action removes both that key and its associated value. The operation succeeds even if you attempt to remove tags from a resource that were already removed. Note the following:</p> <ul> <li> <p>To remove tags from a resource, you need the necessary permissions for the service that the resource belongs to as well as permissions for removing tags. For more information, see <a href="http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html">Obtaining Permissions for Tagging</a> in the <i>AWS Resource Groups and Tag Editor User Guide</i>.</p> </li> <li> <p>You can only tag resources that are located in the specified region for the AWS account.</p> </li> </ul>
untagResources :: forall eff. UntagResourcesInput -> Aff (exception :: EXCEPTION | eff) UntagResourcesOutput
untagResources = Request.request serviceName "untagResources" 


newtype AmazonResourceType = AmazonResourceType String
derive instance newtypeAmazonResourceType :: Newtype AmazonResourceType _
derive instance repGenericAmazonResourceType :: Generic AmazonResourceType _
instance showAmazonResourceType :: Show AmazonResourceType where
  show = genericShow
instance decodeAmazonResourceType :: Decode AmazonResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAmazonResourceType :: Encode AmazonResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _
derive instance repGenericErrorCode :: Generic ErrorCode _
instance showErrorCode :: Show ErrorCode where
  show = genericShow
instance decodeErrorCode :: Decode ErrorCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorCode :: Encode ErrorCode where
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


newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _
derive instance repGenericExceptionMessage :: Generic ExceptionMessage _
instance showExceptionMessage :: Show ExceptionMessage where
  show = genericShow
instance decodeExceptionMessage :: Decode ExceptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExceptionMessage :: Encode ExceptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailedResourcesMap = FailedResourcesMap (StrMap.StrMap FailureInfo)
derive instance newtypeFailedResourcesMap :: Newtype FailedResourcesMap _
derive instance repGenericFailedResourcesMap :: Generic FailedResourcesMap _
instance showFailedResourcesMap :: Show FailedResourcesMap where
  show = genericShow
instance decodeFailedResourcesMap :: Decode FailedResourcesMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedResourcesMap :: Encode FailedResourcesMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details of the common errors that all actions return.</p>
newtype FailureInfo = FailureInfo 
  { "StatusCode" :: NullOrUndefined.NullOrUndefined (StatusCode)
  , "ErrorCode" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeFailureInfo :: Newtype FailureInfo _
derive instance repGenericFailureInfo :: Generic FailureInfo _
instance showFailureInfo :: Show FailureInfo where
  show = genericShow
instance decodeFailureInfo :: Decode FailureInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailureInfo :: Encode FailureInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetResourcesInput = GetResourcesInput 
  { "PaginationToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "TagFilters" :: NullOrUndefined.NullOrUndefined (TagFilterList)
  , "ResourcesPerPage" :: NullOrUndefined.NullOrUndefined (ResourcesPerPage)
  , "TagsPerPage" :: NullOrUndefined.NullOrUndefined (TagsPerPage)
  , "ResourceTypeFilters" :: NullOrUndefined.NullOrUndefined (ResourceTypeFilterList)
  }
derive instance newtypeGetResourcesInput :: Newtype GetResourcesInput _
derive instance repGenericGetResourcesInput :: Generic GetResourcesInput _
instance showGetResourcesInput :: Show GetResourcesInput where
  show = genericShow
instance decodeGetResourcesInput :: Decode GetResourcesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetResourcesInput :: Encode GetResourcesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetResourcesOutput = GetResourcesOutput 
  { "PaginationToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "ResourceTagMappingList" :: NullOrUndefined.NullOrUndefined (ResourceTagMappingList)
  }
derive instance newtypeGetResourcesOutput :: Newtype GetResourcesOutput _
derive instance repGenericGetResourcesOutput :: Generic GetResourcesOutput _
instance showGetResourcesOutput :: Show GetResourcesOutput where
  show = genericShow
instance decodeGetResourcesOutput :: Decode GetResourcesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetResourcesOutput :: Encode GetResourcesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTagKeysInput = GetTagKeysInput 
  { "PaginationToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeGetTagKeysInput :: Newtype GetTagKeysInput _
derive instance repGenericGetTagKeysInput :: Generic GetTagKeysInput _
instance showGetTagKeysInput :: Show GetTagKeysInput where
  show = genericShow
instance decodeGetTagKeysInput :: Decode GetTagKeysInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTagKeysInput :: Encode GetTagKeysInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTagKeysOutput = GetTagKeysOutput 
  { "PaginationToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "TagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  }
derive instance newtypeGetTagKeysOutput :: Newtype GetTagKeysOutput _
derive instance repGenericGetTagKeysOutput :: Generic GetTagKeysOutput _
instance showGetTagKeysOutput :: Show GetTagKeysOutput where
  show = genericShow
instance decodeGetTagKeysOutput :: Decode GetTagKeysOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTagKeysOutput :: Encode GetTagKeysOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTagValuesInput = GetTagValuesInput 
  { "PaginationToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "Key" :: (TagKey)
  }
derive instance newtypeGetTagValuesInput :: Newtype GetTagValuesInput _
derive instance repGenericGetTagValuesInput :: Generic GetTagValuesInput _
instance showGetTagValuesInput :: Show GetTagValuesInput where
  show = genericShow
instance decodeGetTagValuesInput :: Decode GetTagValuesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTagValuesInput :: Encode GetTagValuesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTagValuesOutput = GetTagValuesOutput 
  { "PaginationToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "TagValues" :: NullOrUndefined.NullOrUndefined (TagValuesOutputList)
  }
derive instance newtypeGetTagValuesOutput :: Newtype GetTagValuesOutput _
derive instance repGenericGetTagValuesOutput :: Generic GetTagValuesOutput _
instance showGetTagValuesOutput :: Show GetTagValuesOutput where
  show = genericShow
instance decodeGetTagValuesOutput :: Decode GetTagValuesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTagValuesOutput :: Encode GetTagValuesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request processing failed because of an unknown error, exception, or failure. You can retry the request.</p>
newtype InternalServiceException = InternalServiceException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInternalServiceException :: Newtype InternalServiceException _
derive instance repGenericInternalServiceException :: Generic InternalServiceException _
instance showInternalServiceException :: Show InternalServiceException where
  show = genericShow
instance decodeInternalServiceException :: Decode InternalServiceException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServiceException :: Encode InternalServiceException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A parameter is missing or a malformed string or invalid or out-of-range value was supplied for the request parameter.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _
derive instance repGenericPaginationToken :: Generic PaginationToken _
instance showPaginationToken :: Show PaginationToken where
  show = genericShow
instance decodePaginationToken :: Decode PaginationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaginationToken :: Encode PaginationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A <code>PaginationToken</code> is valid for a maximum of 15 minutes. Your request was denied because the specified <code>PaginationToken</code> has expired.</p>
newtype PaginationTokenExpiredException = PaginationTokenExpiredException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypePaginationTokenExpiredException :: Newtype PaginationTokenExpiredException _
derive instance repGenericPaginationTokenExpiredException :: Generic PaginationTokenExpiredException _
instance showPaginationTokenExpiredException :: Show PaginationTokenExpiredException where
  show = genericShow
instance decodePaginationTokenExpiredException :: Decode PaginationTokenExpiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaginationTokenExpiredException :: Encode PaginationTokenExpiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceARN = ResourceARN String
derive instance newtypeResourceARN :: Newtype ResourceARN _
derive instance repGenericResourceARN :: Generic ResourceARN _
instance showResourceARN :: Show ResourceARN where
  show = genericShow
instance decodeResourceARN :: Decode ResourceARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceARN :: Encode ResourceARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceARNList = ResourceARNList (Array ResourceARN)
derive instance newtypeResourceARNList :: Newtype ResourceARNList _
derive instance repGenericResourceARNList :: Generic ResourceARNList _
instance showResourceARNList :: Show ResourceARNList where
  show = genericShow
instance decodeResourceARNList :: Decode ResourceARNList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceARNList :: Encode ResourceARNList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of resource ARNs and the tags (keys and values) that are associated with each.</p>
newtype ResourceTagMapping = ResourceTagMapping 
  { "ResourceARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeResourceTagMapping :: Newtype ResourceTagMapping _
derive instance repGenericResourceTagMapping :: Generic ResourceTagMapping _
instance showResourceTagMapping :: Show ResourceTagMapping where
  show = genericShow
instance decodeResourceTagMapping :: Decode ResourceTagMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTagMapping :: Encode ResourceTagMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceTagMappingList = ResourceTagMappingList (Array ResourceTagMapping)
derive instance newtypeResourceTagMappingList :: Newtype ResourceTagMappingList _
derive instance repGenericResourceTagMappingList :: Generic ResourceTagMappingList _
instance showResourceTagMappingList :: Show ResourceTagMappingList where
  show = genericShow
instance decodeResourceTagMappingList :: Decode ResourceTagMappingList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTagMappingList :: Encode ResourceTagMappingList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceTypeFilterList = ResourceTypeFilterList (Array AmazonResourceType)
derive instance newtypeResourceTypeFilterList :: Newtype ResourceTypeFilterList _
derive instance repGenericResourceTypeFilterList :: Generic ResourceTypeFilterList _
instance showResourceTypeFilterList :: Show ResourceTypeFilterList where
  show = genericShow
instance decodeResourceTypeFilterList :: Decode ResourceTypeFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTypeFilterList :: Encode ResourceTypeFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourcesPerPage = ResourcesPerPage Int
derive instance newtypeResourcesPerPage :: Newtype ResourcesPerPage _
derive instance repGenericResourcesPerPage :: Generic ResourcesPerPage _
instance showResourcesPerPage :: Show ResourcesPerPage where
  show = genericShow
instance decodeResourcesPerPage :: Decode ResourcesPerPage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourcesPerPage :: Encode ResourcesPerPage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StatusCode = StatusCode Int
derive instance newtypeStatusCode :: Newtype StatusCode _
derive instance repGenericStatusCode :: Generic StatusCode _
instance showStatusCode :: Show StatusCode where
  show = genericShow
instance decodeStatusCode :: Decode StatusCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusCode :: Encode StatusCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The metadata that you apply to AWS resources to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-basics">Tag Basics</a> in the <i>Amazon EC2 User Guide for Linux Instances</i>.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of tags (keys and values) that are used to specify the associated resources.</p>
newtype TagFilter = TagFilter 
  { "Key" :: NullOrUndefined.NullOrUndefined (TagKey)
  , "Values" :: NullOrUndefined.NullOrUndefined (TagValueList)
  }
derive instance newtypeTagFilter :: Newtype TagFilter _
derive instance repGenericTagFilter :: Generic TagFilter _
instance showTagFilter :: Show TagFilter where
  show = genericShow
instance decodeTagFilter :: Decode TagFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagFilter :: Encode TagFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagFilterList = TagFilterList (Array TagFilter)
derive instance newtypeTagFilterList :: Newtype TagFilterList _
derive instance repGenericTagFilterList :: Generic TagFilterList _
instance showTagFilterList :: Show TagFilterList where
  show = genericShow
instance decodeTagFilterList :: Decode TagFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagFilterList :: Encode TagFilterList where
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


newtype TagKeyListForUntag = TagKeyListForUntag (Array TagKey)
derive instance newtypeTagKeyListForUntag :: Newtype TagKeyListForUntag _
derive instance repGenericTagKeyListForUntag :: Generic TagKeyListForUntag _
instance showTagKeyListForUntag :: Show TagKeyListForUntag where
  show = genericShow
instance decodeTagKeyListForUntag :: Decode TagKeyListForUntag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyListForUntag :: Encode TagKeyListForUntag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagMap = TagMap (StrMap.StrMap TagValue)
derive instance newtypeTagMap :: Newtype TagMap _
derive instance repGenericTagMap :: Generic TagMap _
instance showTagMap :: Show TagMap where
  show = genericShow
instance decodeTagMap :: Decode TagMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagMap :: Encode TagMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagResourcesInput = TagResourcesInput 
  { "ResourceARNList" :: (ResourceARNList)
  , "Tags" :: (TagMap)
  }
derive instance newtypeTagResourcesInput :: Newtype TagResourcesInput _
derive instance repGenericTagResourcesInput :: Generic TagResourcesInput _
instance showTagResourcesInput :: Show TagResourcesInput where
  show = genericShow
instance decodeTagResourcesInput :: Decode TagResourcesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourcesInput :: Encode TagResourcesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagResourcesOutput = TagResourcesOutput 
  { "FailedResourcesMap" :: NullOrUndefined.NullOrUndefined (FailedResourcesMap)
  }
derive instance newtypeTagResourcesOutput :: Newtype TagResourcesOutput _
derive instance repGenericTagResourcesOutput :: Generic TagResourcesOutput _
instance showTagResourcesOutput :: Show TagResourcesOutput where
  show = genericShow
instance decodeTagResourcesOutput :: Decode TagResourcesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourcesOutput :: Encode TagResourcesOutput where
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


newtype TagValueList = TagValueList (Array TagValue)
derive instance newtypeTagValueList :: Newtype TagValueList _
derive instance repGenericTagValueList :: Generic TagValueList _
instance showTagValueList :: Show TagValueList where
  show = genericShow
instance decodeTagValueList :: Decode TagValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValueList :: Encode TagValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValuesOutputList = TagValuesOutputList (Array TagValue)
derive instance newtypeTagValuesOutputList :: Newtype TagValuesOutputList _
derive instance repGenericTagValuesOutputList :: Generic TagValuesOutputList _
instance showTagValuesOutputList :: Show TagValuesOutputList where
  show = genericShow
instance decodeTagValuesOutputList :: Decode TagValuesOutputList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValuesOutputList :: Encode TagValuesOutputList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagsPerPage = TagsPerPage Int
derive instance newtypeTagsPerPage :: Newtype TagsPerPage _
derive instance repGenericTagsPerPage :: Generic TagsPerPage _
instance showTagsPerPage :: Show TagsPerPage where
  show = genericShow
instance decodeTagsPerPage :: Decode TagsPerPage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagsPerPage :: Encode TagsPerPage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was denied to limit the frequency of submitted requests.</p>
newtype ThrottledException = ThrottledException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeThrottledException :: Newtype ThrottledException _
derive instance repGenericThrottledException :: Generic ThrottledException _
instance showThrottledException :: Show ThrottledException where
  show = genericShow
instance decodeThrottledException :: Decode ThrottledException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThrottledException :: Encode ThrottledException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UntagResourcesInput = UntagResourcesInput 
  { "ResourceARNList" :: (ResourceARNList)
  , "TagKeys" :: (TagKeyListForUntag)
  }
derive instance newtypeUntagResourcesInput :: Newtype UntagResourcesInput _
derive instance repGenericUntagResourcesInput :: Generic UntagResourcesInput _
instance showUntagResourcesInput :: Show UntagResourcesInput where
  show = genericShow
instance decodeUntagResourcesInput :: Decode UntagResourcesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagResourcesInput :: Encode UntagResourcesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UntagResourcesOutput = UntagResourcesOutput 
  { "FailedResourcesMap" :: NullOrUndefined.NullOrUndefined (FailedResourcesMap)
  }
derive instance newtypeUntagResourcesOutput :: Newtype UntagResourcesOutput _
derive instance repGenericUntagResourcesOutput :: Generic UntagResourcesOutput _
instance showUntagResourcesOutput :: Show UntagResourcesOutput where
  show = genericShow
instance decodeUntagResourcesOutput :: Decode UntagResourcesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagResourcesOutput :: Encode UntagResourcesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
