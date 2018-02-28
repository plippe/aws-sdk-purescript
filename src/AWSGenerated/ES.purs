

-- | <fullname>Amazon Elasticsearch Configuration Service</fullname> <p>Use the Amazon Elasticsearch configuration API to create, configure, and manage Elasticsearch domains.</p> <p>The endpoint for configuration service requests is region-specific: es.<i>region</i>.amazonaws.com. For example, es.us-east-1.amazonaws.com. For a current list of supported regions and endpoints, see <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#elasticsearch-service-regions" target="_blank">Regions and Endpoints</a>.</p>
module AWS.ES where

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

serviceName = "ES" :: String


-- | <p>Attaches tags to an existing Elasticsearch domain. Tags are a set of case-sensitive key value pairs. An Elasticsearch domain may have up to 10 tags. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-awsresorcetagging" target="_blank"> Tagging Amazon Elasticsearch Service Domains for more information.</a></p>
addTags :: forall eff. AddTagsRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
addTags = Request.request serviceName "addTags" 


-- | <p>Creates a new Elasticsearch domain. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains" target="_blank">Creating Elasticsearch Domains</a> in the <i>Amazon Elasticsearch Service Developer Guide</i>.</p>
createElasticsearchDomain :: forall eff. CreateElasticsearchDomainRequest -> Aff (exception :: EXCEPTION | eff) CreateElasticsearchDomainResponse
createElasticsearchDomain = Request.request serviceName "createElasticsearchDomain" 


-- | <p>Permanently deletes the specified Elasticsearch domain and all of its data. Once a domain is deleted, it cannot be recovered.</p>
deleteElasticsearchDomain :: forall eff. DeleteElasticsearchDomainRequest -> Aff (exception :: EXCEPTION | eff) DeleteElasticsearchDomainResponse
deleteElasticsearchDomain = Request.request serviceName "deleteElasticsearchDomain" 


-- | <p>Deletes the service-linked role that Elasticsearch Service uses to manage and maintain VPC domains. Role deletion will fail if any existing VPC domains use the role. You must delete any such Elasticsearch domains before deleting the role. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-enabling-slr" target="_blank">Deleting Elasticsearch Service Role</a> in <i>VPC Endpoints for Amazon Elasticsearch Service Domains</i>.</p>
deleteElasticsearchServiceRole :: forall eff.  Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteElasticsearchServiceRole = Request.request serviceName "deleteElasticsearchServiceRole" (Types.NoInput unit)


-- | <p>Returns domain configuration information about the specified Elasticsearch domain, including the domain ID, domain endpoint, and domain ARN.</p>
describeElasticsearchDomain :: forall eff. DescribeElasticsearchDomainRequest -> Aff (exception :: EXCEPTION | eff) DescribeElasticsearchDomainResponse
describeElasticsearchDomain = Request.request serviceName "describeElasticsearchDomain" 


-- | <p>Provides cluster configuration information about the specified Elasticsearch domain, such as the state, creation date, update version, and update date for cluster options.</p>
describeElasticsearchDomainConfig :: forall eff. DescribeElasticsearchDomainConfigRequest -> Aff (exception :: EXCEPTION | eff) DescribeElasticsearchDomainConfigResponse
describeElasticsearchDomainConfig = Request.request serviceName "describeElasticsearchDomainConfig" 


-- | <p>Returns domain configuration information about the specified Elasticsearch domains, including the domain ID, domain endpoint, and domain ARN.</p>
describeElasticsearchDomains :: forall eff. DescribeElasticsearchDomainsRequest -> Aff (exception :: EXCEPTION | eff) DescribeElasticsearchDomainsResponse
describeElasticsearchDomains = Request.request serviceName "describeElasticsearchDomains" 


-- | <p> Describe Elasticsearch Limits for a given InstanceType and ElasticsearchVersion. When modifying existing Domain, specify the <code> <a>DomainName</a> </code> to know what Limits are supported for modifying. </p>
describeElasticsearchInstanceTypeLimits :: forall eff. DescribeElasticsearchInstanceTypeLimitsRequest -> Aff (exception :: EXCEPTION | eff) DescribeElasticsearchInstanceTypeLimitsResponse
describeElasticsearchInstanceTypeLimits = Request.request serviceName "describeElasticsearchInstanceTypeLimits" 


-- | <p>Returns the name of all Elasticsearch domains owned by the current user's account. </p>
listDomainNames :: forall eff.  Aff (exception :: EXCEPTION | eff) ListDomainNamesResponse
listDomainNames = Request.request serviceName "listDomainNames" (Types.NoInput unit)


-- | <p>List all Elasticsearch instance types that are supported for given ElasticsearchVersion</p>
listElasticsearchInstanceTypes :: forall eff. ListElasticsearchInstanceTypesRequest -> Aff (exception :: EXCEPTION | eff) ListElasticsearchInstanceTypesResponse
listElasticsearchInstanceTypes = Request.request serviceName "listElasticsearchInstanceTypes" 


-- | <p>List all supported Elasticsearch versions</p>
listElasticsearchVersions :: forall eff. ListElasticsearchVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListElasticsearchVersionsResponse
listElasticsearchVersions = Request.request serviceName "listElasticsearchVersions" 


-- | <p>Returns all tags for the given Elasticsearch domain.</p>
listTags :: forall eff. ListTagsRequest -> Aff (exception :: EXCEPTION | eff) ListTagsResponse
listTags = Request.request serviceName "listTags" 


-- | <p>Removes the specified set of tags from the specified Elasticsearch domain.</p>
removeTags :: forall eff. RemoveTagsRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
removeTags = Request.request serviceName "removeTags" 


-- | <p>Modifies the cluster configuration of the specified Elasticsearch domain, setting as setting the instance type and the number of instances. </p>
updateElasticsearchDomainConfig :: forall eff. UpdateElasticsearchDomainConfigRequest -> Aff (exception :: EXCEPTION | eff) UpdateElasticsearchDomainConfigResponse
updateElasticsearchDomainConfig = Request.request serviceName "updateElasticsearchDomainConfig" 


-- | <p>The Amazon Resource Name (ARN) of the Elasticsearch domain. See <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html" target="_blank">Identifiers for IAM Entities</a> in <i>Using AWS Identity and Access Management</i> for more information.</p>
newtype ARN = ARN String
derive instance newtypeARN :: Newtype ARN _
derive instance repGenericARN :: Generic ARN _
instance showARN :: Show ARN where
  show = genericShow
instance decodeARN :: Decode ARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeARN :: Encode ARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The configured access rules for the domain's document and search endpoints, and the current status of those rules.</p>
newtype AccessPoliciesStatus = AccessPoliciesStatus 
  { "Options" :: (PolicyDocument)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeAccessPoliciesStatus :: Newtype AccessPoliciesStatus _
derive instance repGenericAccessPoliciesStatus :: Generic AccessPoliciesStatus _
instance showAccessPoliciesStatus :: Show AccessPoliciesStatus where
  show = genericShow
instance decodeAccessPoliciesStatus :: Decode AccessPoliciesStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessPoliciesStatus :: Encode AccessPoliciesStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the <code><a>AddTags</a></code> operation. Specify the tags that you want to attach to the Elasticsearch domain.</p>
newtype AddTagsRequest = AddTagsRequest 
  { "ARN" :: (ARN)
  , "TagList" :: (TagList)
  }
derive instance newtypeAddTagsRequest :: Newtype AddTagsRequest _
derive instance repGenericAddTagsRequest :: Generic AddTagsRequest _
instance showAddTagsRequest :: Show AddTagsRequest where
  show = genericShow
instance decodeAddTagsRequest :: Decode AddTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsRequest :: Encode AddTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> List of limits that are specific to a given InstanceType and for each of it's <code> <a>InstanceRole</a> </code> . </p>
newtype AdditionalLimit = AdditionalLimit 
  { "LimitName" :: NullOrUndefined.NullOrUndefined (LimitName)
  , "LimitValues" :: NullOrUndefined.NullOrUndefined (LimitValueList)
  }
derive instance newtypeAdditionalLimit :: Newtype AdditionalLimit _
derive instance repGenericAdditionalLimit :: Generic AdditionalLimit _
instance showAdditionalLimit :: Show AdditionalLimit where
  show = genericShow
instance decodeAdditionalLimit :: Decode AdditionalLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdditionalLimit :: Encode AdditionalLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdditionalLimitList = AdditionalLimitList (Array AdditionalLimit)
derive instance newtypeAdditionalLimitList :: Newtype AdditionalLimitList _
derive instance repGenericAdditionalLimitList :: Generic AdditionalLimitList _
instance showAdditionalLimitList :: Show AdditionalLimitList where
  show = genericShow
instance decodeAdditionalLimitList :: Decode AdditionalLimitList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdditionalLimitList :: Encode AdditionalLimitList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Exposes select native Elasticsearch configuration values from <code>elasticsearch.yml</code>. Currently, the following advanced options are available:</p> <ul> <li>Option to allow references to indices in an HTTP request body. Must be <code>false</code> when configuring access to individual sub-resources. By default, the value is <code>true</code>. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options" target="_blank">Configuration Advanced Options</a> for more information.</li> <li>Option to specify the percentage of heap space that is allocated to field data. By default, this setting is unbounded.</li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options">Configuring Advanced Options</a>.</p>
newtype AdvancedOptions = AdvancedOptions (StrMap.StrMap String)
derive instance newtypeAdvancedOptions :: Newtype AdvancedOptions _
derive instance repGenericAdvancedOptions :: Generic AdvancedOptions _
instance showAdvancedOptions :: Show AdvancedOptions where
  show = genericShow
instance decodeAdvancedOptions :: Decode AdvancedOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdvancedOptions :: Encode AdvancedOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Status of the advanced options for the specified Elasticsearch domain. Currently, the following advanced options are available:</p> <ul> <li>Option to allow references to indices in an HTTP request body. Must be <code>false</code> when configuring access to individual sub-resources. By default, the value is <code>true</code>. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options" target="_blank">Configuration Advanced Options</a> for more information.</li> <li>Option to specify the percentage of heap space that is allocated to field data. By default, this setting is unbounded.</li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options">Configuring Advanced Options</a>.</p>
newtype AdvancedOptionsStatus = AdvancedOptionsStatus 
  { "Options" :: (AdvancedOptions)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeAdvancedOptionsStatus :: Newtype AdvancedOptionsStatus _
derive instance repGenericAdvancedOptionsStatus :: Generic AdvancedOptionsStatus _
instance showAdvancedOptionsStatus :: Show AdvancedOptionsStatus where
  show = genericShow
instance decodeAdvancedOptionsStatus :: Decode AdvancedOptionsStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdvancedOptionsStatus :: Encode AdvancedOptionsStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error occurred while processing the request.</p>
newtype BaseException = BaseException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBaseException :: Newtype BaseException _
derive instance repGenericBaseException :: Generic BaseException _
instance showBaseException :: Show BaseException where
  show = genericShow
instance decodeBaseException :: Decode BaseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBaseException :: Encode BaseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>ARN of the Cloudwatch log group to which log needs to be published.</p>
newtype CloudWatchLogsLogGroupArn = CloudWatchLogsLogGroupArn String
derive instance newtypeCloudWatchLogsLogGroupArn :: Newtype CloudWatchLogsLogGroupArn _
derive instance repGenericCloudWatchLogsLogGroupArn :: Generic CloudWatchLogsLogGroupArn _
instance showCloudWatchLogsLogGroupArn :: Show CloudWatchLogsLogGroupArn where
  show = genericShow
instance decodeCloudWatchLogsLogGroupArn :: Decode CloudWatchLogsLogGroupArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchLogsLogGroupArn :: Encode CloudWatchLogsLogGroupArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateElasticsearchDomainRequest = CreateElasticsearchDomainRequest 
  { "DomainName" :: (DomainName)
  , "ElasticsearchVersion" :: NullOrUndefined.NullOrUndefined (ElasticsearchVersionString)
  , "ElasticsearchClusterConfig" :: NullOrUndefined.NullOrUndefined (ElasticsearchClusterConfig)
  , "EBSOptions" :: NullOrUndefined.NullOrUndefined (EBSOptions)
  , "AccessPolicies" :: NullOrUndefined.NullOrUndefined (PolicyDocument)
  , "SnapshotOptions" :: NullOrUndefined.NullOrUndefined (SnapshotOptions)
  , "VPCOptions" :: NullOrUndefined.NullOrUndefined (VPCOptions)
  , "EncryptionAtRestOptions" :: NullOrUndefined.NullOrUndefined (EncryptionAtRestOptions)
  , "AdvancedOptions" :: NullOrUndefined.NullOrUndefined (AdvancedOptions)
  , "LogPublishingOptions" :: NullOrUndefined.NullOrUndefined (LogPublishingOptions)
  }
derive instance newtypeCreateElasticsearchDomainRequest :: Newtype CreateElasticsearchDomainRequest _
derive instance repGenericCreateElasticsearchDomainRequest :: Generic CreateElasticsearchDomainRequest _
instance showCreateElasticsearchDomainRequest :: Show CreateElasticsearchDomainRequest where
  show = genericShow
instance decodeCreateElasticsearchDomainRequest :: Decode CreateElasticsearchDomainRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateElasticsearchDomainRequest :: Encode CreateElasticsearchDomainRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a <code>CreateElasticsearchDomain</code> operation. Contains the status of the newly created Elasticsearch domain.</p>
newtype CreateElasticsearchDomainResponse = CreateElasticsearchDomainResponse 
  { "DomainStatus" :: NullOrUndefined.NullOrUndefined (ElasticsearchDomainStatus)
  }
derive instance newtypeCreateElasticsearchDomainResponse :: Newtype CreateElasticsearchDomainResponse _
derive instance repGenericCreateElasticsearchDomainResponse :: Generic CreateElasticsearchDomainResponse _
instance showCreateElasticsearchDomainResponse :: Show CreateElasticsearchDomainResponse where
  show = genericShow
instance decodeCreateElasticsearchDomainResponse :: Decode CreateElasticsearchDomainResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateElasticsearchDomainResponse :: Encode CreateElasticsearchDomainResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the <code><a>DeleteElasticsearchDomain</a></code> operation. Specifies the name of the Elasticsearch domain that you want to delete.</p>
newtype DeleteElasticsearchDomainRequest = DeleteElasticsearchDomainRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDeleteElasticsearchDomainRequest :: Newtype DeleteElasticsearchDomainRequest _
derive instance repGenericDeleteElasticsearchDomainRequest :: Generic DeleteElasticsearchDomainRequest _
instance showDeleteElasticsearchDomainRequest :: Show DeleteElasticsearchDomainRequest where
  show = genericShow
instance decodeDeleteElasticsearchDomainRequest :: Decode DeleteElasticsearchDomainRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteElasticsearchDomainRequest :: Encode DeleteElasticsearchDomainRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a <code>DeleteElasticsearchDomain</code> request. Contains the status of the pending deletion, or no status if the domain and all of its resources have been deleted.</p>
newtype DeleteElasticsearchDomainResponse = DeleteElasticsearchDomainResponse 
  { "DomainStatus" :: NullOrUndefined.NullOrUndefined (ElasticsearchDomainStatus)
  }
derive instance newtypeDeleteElasticsearchDomainResponse :: Newtype DeleteElasticsearchDomainResponse _
derive instance repGenericDeleteElasticsearchDomainResponse :: Generic DeleteElasticsearchDomainResponse _
instance showDeleteElasticsearchDomainResponse :: Show DeleteElasticsearchDomainResponse where
  show = genericShow
instance decodeDeleteElasticsearchDomainResponse :: Decode DeleteElasticsearchDomainResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteElasticsearchDomainResponse :: Encode DeleteElasticsearchDomainResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Container for the parameters to the <code>DescribeElasticsearchDomainConfig</code> operation. Specifies the domain name for which you want configuration information.</p>
newtype DescribeElasticsearchDomainConfigRequest = DescribeElasticsearchDomainConfigRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDescribeElasticsearchDomainConfigRequest :: Newtype DescribeElasticsearchDomainConfigRequest _
derive instance repGenericDescribeElasticsearchDomainConfigRequest :: Generic DescribeElasticsearchDomainConfigRequest _
instance showDescribeElasticsearchDomainConfigRequest :: Show DescribeElasticsearchDomainConfigRequest where
  show = genericShow
instance decodeDescribeElasticsearchDomainConfigRequest :: Decode DescribeElasticsearchDomainConfigRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeElasticsearchDomainConfigRequest :: Encode DescribeElasticsearchDomainConfigRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a <code>DescribeElasticsearchDomainConfig</code> request. Contains the configuration information of the requested domain.</p>
newtype DescribeElasticsearchDomainConfigResponse = DescribeElasticsearchDomainConfigResponse 
  { "DomainConfig" :: (ElasticsearchDomainConfig)
  }
derive instance newtypeDescribeElasticsearchDomainConfigResponse :: Newtype DescribeElasticsearchDomainConfigResponse _
derive instance repGenericDescribeElasticsearchDomainConfigResponse :: Generic DescribeElasticsearchDomainConfigResponse _
instance showDescribeElasticsearchDomainConfigResponse :: Show DescribeElasticsearchDomainConfigResponse where
  show = genericShow
instance decodeDescribeElasticsearchDomainConfigResponse :: Decode DescribeElasticsearchDomainConfigResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeElasticsearchDomainConfigResponse :: Encode DescribeElasticsearchDomainConfigResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the <code><a>DescribeElasticsearchDomain</a></code> operation.</p>
newtype DescribeElasticsearchDomainRequest = DescribeElasticsearchDomainRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDescribeElasticsearchDomainRequest :: Newtype DescribeElasticsearchDomainRequest _
derive instance repGenericDescribeElasticsearchDomainRequest :: Generic DescribeElasticsearchDomainRequest _
instance showDescribeElasticsearchDomainRequest :: Show DescribeElasticsearchDomainRequest where
  show = genericShow
instance decodeDescribeElasticsearchDomainRequest :: Decode DescribeElasticsearchDomainRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeElasticsearchDomainRequest :: Encode DescribeElasticsearchDomainRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a <code>DescribeElasticsearchDomain</code> request. Contains the status of the domain specified in the request.</p>
newtype DescribeElasticsearchDomainResponse = DescribeElasticsearchDomainResponse 
  { "DomainStatus" :: (ElasticsearchDomainStatus)
  }
derive instance newtypeDescribeElasticsearchDomainResponse :: Newtype DescribeElasticsearchDomainResponse _
derive instance repGenericDescribeElasticsearchDomainResponse :: Generic DescribeElasticsearchDomainResponse _
instance showDescribeElasticsearchDomainResponse :: Show DescribeElasticsearchDomainResponse where
  show = genericShow
instance decodeDescribeElasticsearchDomainResponse :: Decode DescribeElasticsearchDomainResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeElasticsearchDomainResponse :: Encode DescribeElasticsearchDomainResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the <code><a>DescribeElasticsearchDomains</a></code> operation. By default, the API returns the status of all Elasticsearch domains.</p>
newtype DescribeElasticsearchDomainsRequest = DescribeElasticsearchDomainsRequest 
  { "DomainNames" :: (DomainNameList)
  }
derive instance newtypeDescribeElasticsearchDomainsRequest :: Newtype DescribeElasticsearchDomainsRequest _
derive instance repGenericDescribeElasticsearchDomainsRequest :: Generic DescribeElasticsearchDomainsRequest _
instance showDescribeElasticsearchDomainsRequest :: Show DescribeElasticsearchDomainsRequest where
  show = genericShow
instance decodeDescribeElasticsearchDomainsRequest :: Decode DescribeElasticsearchDomainsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeElasticsearchDomainsRequest :: Encode DescribeElasticsearchDomainsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a <code>DescribeElasticsearchDomains</code> request. Contains the status of the specified domains or all domains owned by the account.</p>
newtype DescribeElasticsearchDomainsResponse = DescribeElasticsearchDomainsResponse 
  { "DomainStatusList" :: (ElasticsearchDomainStatusList)
  }
derive instance newtypeDescribeElasticsearchDomainsResponse :: Newtype DescribeElasticsearchDomainsResponse _
derive instance repGenericDescribeElasticsearchDomainsResponse :: Generic DescribeElasticsearchDomainsResponse _
instance showDescribeElasticsearchDomainsResponse :: Show DescribeElasticsearchDomainsResponse where
  show = genericShow
instance decodeDescribeElasticsearchDomainsResponse :: Decode DescribeElasticsearchDomainsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeElasticsearchDomainsResponse :: Encode DescribeElasticsearchDomainsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Container for the parameters to <code> <a>DescribeElasticsearchInstanceTypeLimits</a> </code> operation. </p>
newtype DescribeElasticsearchInstanceTypeLimitsRequest = DescribeElasticsearchInstanceTypeLimitsRequest 
  { "DomainName" :: NullOrUndefined.NullOrUndefined (DomainName)
  , "InstanceType" :: (ESPartitionInstanceType)
  , "ElasticsearchVersion" :: (ElasticsearchVersionString)
  }
derive instance newtypeDescribeElasticsearchInstanceTypeLimitsRequest :: Newtype DescribeElasticsearchInstanceTypeLimitsRequest _
derive instance repGenericDescribeElasticsearchInstanceTypeLimitsRequest :: Generic DescribeElasticsearchInstanceTypeLimitsRequest _
instance showDescribeElasticsearchInstanceTypeLimitsRequest :: Show DescribeElasticsearchInstanceTypeLimitsRequest where
  show = genericShow
instance decodeDescribeElasticsearchInstanceTypeLimitsRequest :: Decode DescribeElasticsearchInstanceTypeLimitsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeElasticsearchInstanceTypeLimitsRequest :: Encode DescribeElasticsearchInstanceTypeLimitsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Container for the parameters received from <code> <a>DescribeElasticsearchInstanceTypeLimits</a> </code> operation. </p>
newtype DescribeElasticsearchInstanceTypeLimitsResponse = DescribeElasticsearchInstanceTypeLimitsResponse 
  { "LimitsByRole" :: NullOrUndefined.NullOrUndefined (LimitsByRole)
  }
derive instance newtypeDescribeElasticsearchInstanceTypeLimitsResponse :: Newtype DescribeElasticsearchInstanceTypeLimitsResponse _
derive instance repGenericDescribeElasticsearchInstanceTypeLimitsResponse :: Generic DescribeElasticsearchInstanceTypeLimitsResponse _
instance showDescribeElasticsearchInstanceTypeLimitsResponse :: Show DescribeElasticsearchInstanceTypeLimitsResponse where
  show = genericShow
instance decodeDescribeElasticsearchInstanceTypeLimitsResponse :: Decode DescribeElasticsearchInstanceTypeLimitsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeElasticsearchInstanceTypeLimitsResponse :: Encode DescribeElasticsearchInstanceTypeLimitsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error occured because the client wanted to access a not supported operation. Gives http status code of 409.</p>
newtype DisabledOperationException = DisabledOperationException Types.NoArguments
derive instance newtypeDisabledOperationException :: Newtype DisabledOperationException _
derive instance repGenericDisabledOperationException :: Generic DisabledOperationException _
instance showDisabledOperationException :: Show DisabledOperationException where
  show = genericShow
instance decodeDisabledOperationException :: Decode DisabledOperationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisabledOperationException :: Encode DisabledOperationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Unique identifier for an Elasticsearch domain.</p>
newtype DomainId = DomainId String
derive instance newtypeDomainId :: Newtype DomainId _
derive instance repGenericDomainId :: Generic DomainId _
instance showDomainId :: Show DomainId where
  show = genericShow
instance decodeDomainId :: Decode DomainId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainId :: Encode DomainId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainInfo = DomainInfo 
  { "DomainName" :: NullOrUndefined.NullOrUndefined (DomainName)
  }
derive instance newtypeDomainInfo :: Newtype DomainInfo _
derive instance repGenericDomainInfo :: Generic DomainInfo _
instance showDomainInfo :: Show DomainInfo where
  show = genericShow
instance decodeDomainInfo :: Decode DomainInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainInfo :: Encode DomainInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Contains the list of Elasticsearch domain information.</p>
newtype DomainInfoList = DomainInfoList (Array DomainInfo)
derive instance newtypeDomainInfoList :: Newtype DomainInfoList _
derive instance repGenericDomainInfoList :: Generic DomainInfoList _
instance showDomainInfoList :: Show DomainInfoList where
  show = genericShow
instance decodeDomainInfoList :: Decode DomainInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainInfoList :: Encode DomainInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of an Elasticsearch domain. Domain names are unique across the domains owned by an account within an AWS region. Domain names start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).</p>
newtype DomainName = DomainName String
derive instance newtypeDomainName :: Newtype DomainName _
derive instance repGenericDomainName :: Generic DomainName _
instance showDomainName :: Show DomainName where
  show = genericShow
instance decodeDomainName :: Decode DomainName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainName :: Encode DomainName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of Elasticsearch domain names.</p>
newtype DomainNameList = DomainNameList (Array DomainName)
derive instance newtypeDomainNameList :: Newtype DomainNameList _
derive instance repGenericDomainNameList :: Generic DomainNameList _
instance showDomainNameList :: Show DomainNameList where
  show = genericShow
instance decodeDomainNameList :: Decode DomainNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainNameList :: Encode DomainNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Options to enable, disable, and specify the properties of EBS storage volumes. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs" target="_blank"> Configuring EBS-based Storage</a>.</p>
newtype EBSOptions = EBSOptions 
  { "EBSEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "VolumeType" :: NullOrUndefined.NullOrUndefined (VolumeType)
  , "VolumeSize" :: NullOrUndefined.NullOrUndefined (IntegerClass)
  , "Iops" :: NullOrUndefined.NullOrUndefined (IntegerClass)
  }
derive instance newtypeEBSOptions :: Newtype EBSOptions _
derive instance repGenericEBSOptions :: Generic EBSOptions _
instance showEBSOptions :: Show EBSOptions where
  show = genericShow
instance decodeEBSOptions :: Decode EBSOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEBSOptions :: Encode EBSOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Status of the EBS options for the specified Elasticsearch domain.</p>
newtype EBSOptionsStatus = EBSOptionsStatus 
  { "Options" :: (EBSOptions)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeEBSOptionsStatus :: Newtype EBSOptionsStatus _
derive instance repGenericEBSOptionsStatus :: Generic EBSOptionsStatus _
instance showEBSOptionsStatus :: Show EBSOptionsStatus where
  show = genericShow
instance decodeEBSOptionsStatus :: Decode EBSOptionsStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEBSOptionsStatus :: Encode EBSOptionsStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ESPartitionInstanceType = ESPartitionInstanceType String
derive instance newtypeESPartitionInstanceType :: Newtype ESPartitionInstanceType _
derive instance repGenericESPartitionInstanceType :: Generic ESPartitionInstanceType _
instance showESPartitionInstanceType :: Show ESPartitionInstanceType where
  show = genericShow
instance decodeESPartitionInstanceType :: Decode ESPartitionInstanceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeESPartitionInstanceType :: Encode ESPartitionInstanceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the configuration for the domain cluster, such as the type and number of instances.</p>
newtype ElasticsearchClusterConfig = ElasticsearchClusterConfig 
  { "InstanceType" :: NullOrUndefined.NullOrUndefined (ESPartitionInstanceType)
  , "InstanceCount" :: NullOrUndefined.NullOrUndefined (IntegerClass)
  , "DedicatedMasterEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ZoneAwarenessEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "DedicatedMasterType" :: NullOrUndefined.NullOrUndefined (ESPartitionInstanceType)
  , "DedicatedMasterCount" :: NullOrUndefined.NullOrUndefined (IntegerClass)
  }
derive instance newtypeElasticsearchClusterConfig :: Newtype ElasticsearchClusterConfig _
derive instance repGenericElasticsearchClusterConfig :: Generic ElasticsearchClusterConfig _
instance showElasticsearchClusterConfig :: Show ElasticsearchClusterConfig where
  show = genericShow
instance decodeElasticsearchClusterConfig :: Decode ElasticsearchClusterConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchClusterConfig :: Encode ElasticsearchClusterConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Specifies the configuration status for the specified Elasticsearch domain.</p>
newtype ElasticsearchClusterConfigStatus = ElasticsearchClusterConfigStatus 
  { "Options" :: (ElasticsearchClusterConfig)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeElasticsearchClusterConfigStatus :: Newtype ElasticsearchClusterConfigStatus _
derive instance repGenericElasticsearchClusterConfigStatus :: Generic ElasticsearchClusterConfigStatus _
instance showElasticsearchClusterConfigStatus :: Show ElasticsearchClusterConfigStatus where
  show = genericShow
instance decodeElasticsearchClusterConfigStatus :: Decode ElasticsearchClusterConfigStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchClusterConfigStatus :: Encode ElasticsearchClusterConfigStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The configuration of an Elasticsearch domain.</p>
newtype ElasticsearchDomainConfig = ElasticsearchDomainConfig 
  { "ElasticsearchVersion" :: NullOrUndefined.NullOrUndefined (ElasticsearchVersionStatus)
  , "ElasticsearchClusterConfig" :: NullOrUndefined.NullOrUndefined (ElasticsearchClusterConfigStatus)
  , "EBSOptions" :: NullOrUndefined.NullOrUndefined (EBSOptionsStatus)
  , "AccessPolicies" :: NullOrUndefined.NullOrUndefined (AccessPoliciesStatus)
  , "SnapshotOptions" :: NullOrUndefined.NullOrUndefined (SnapshotOptionsStatus)
  , "VPCOptions" :: NullOrUndefined.NullOrUndefined (VPCDerivedInfoStatus)
  , "EncryptionAtRestOptions" :: NullOrUndefined.NullOrUndefined (EncryptionAtRestOptionsStatus)
  , "AdvancedOptions" :: NullOrUndefined.NullOrUndefined (AdvancedOptionsStatus)
  , "LogPublishingOptions" :: NullOrUndefined.NullOrUndefined (LogPublishingOptionsStatus)
  }
derive instance newtypeElasticsearchDomainConfig :: Newtype ElasticsearchDomainConfig _
derive instance repGenericElasticsearchDomainConfig :: Generic ElasticsearchDomainConfig _
instance showElasticsearchDomainConfig :: Show ElasticsearchDomainConfig where
  show = genericShow
instance decodeElasticsearchDomainConfig :: Decode ElasticsearchDomainConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchDomainConfig :: Encode ElasticsearchDomainConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The current status of an Elasticsearch domain.</p>
newtype ElasticsearchDomainStatus = ElasticsearchDomainStatus 
  { "DomainId" :: (DomainId)
  , "DomainName" :: (DomainName)
  , "ARN" :: (ARN)
  , "Created" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Deleted" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Endpoint" :: NullOrUndefined.NullOrUndefined (ServiceUrl)
  , "Endpoints" :: NullOrUndefined.NullOrUndefined (EndpointsMap)
  , "Processing" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ElasticsearchVersion" :: NullOrUndefined.NullOrUndefined (ElasticsearchVersionString)
  , "ElasticsearchClusterConfig" :: (ElasticsearchClusterConfig)
  , "EBSOptions" :: NullOrUndefined.NullOrUndefined (EBSOptions)
  , "AccessPolicies" :: NullOrUndefined.NullOrUndefined (PolicyDocument)
  , "SnapshotOptions" :: NullOrUndefined.NullOrUndefined (SnapshotOptions)
  , "VPCOptions" :: NullOrUndefined.NullOrUndefined (VPCDerivedInfo)
  , "EncryptionAtRestOptions" :: NullOrUndefined.NullOrUndefined (EncryptionAtRestOptions)
  , "AdvancedOptions" :: NullOrUndefined.NullOrUndefined (AdvancedOptions)
  , "LogPublishingOptions" :: NullOrUndefined.NullOrUndefined (LogPublishingOptions)
  }
derive instance newtypeElasticsearchDomainStatus :: Newtype ElasticsearchDomainStatus _
derive instance repGenericElasticsearchDomainStatus :: Generic ElasticsearchDomainStatus _
instance showElasticsearchDomainStatus :: Show ElasticsearchDomainStatus where
  show = genericShow
instance decodeElasticsearchDomainStatus :: Decode ElasticsearchDomainStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchDomainStatus :: Encode ElasticsearchDomainStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list that contains the status of each requested Elasticsearch domain.</p>
newtype ElasticsearchDomainStatusList = ElasticsearchDomainStatusList (Array ElasticsearchDomainStatus)
derive instance newtypeElasticsearchDomainStatusList :: Newtype ElasticsearchDomainStatusList _
derive instance repGenericElasticsearchDomainStatusList :: Generic ElasticsearchDomainStatusList _
instance showElasticsearchDomainStatusList :: Show ElasticsearchDomainStatusList where
  show = genericShow
instance decodeElasticsearchDomainStatusList :: Decode ElasticsearchDomainStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchDomainStatusList :: Encode ElasticsearchDomainStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> List of instance types supported by Amazon Elasticsearch service. </p>
newtype ElasticsearchInstanceTypeList = ElasticsearchInstanceTypeList (Array ESPartitionInstanceType)
derive instance newtypeElasticsearchInstanceTypeList :: Newtype ElasticsearchInstanceTypeList _
derive instance repGenericElasticsearchInstanceTypeList :: Generic ElasticsearchInstanceTypeList _
instance showElasticsearchInstanceTypeList :: Show ElasticsearchInstanceTypeList where
  show = genericShow
instance decodeElasticsearchInstanceTypeList :: Decode ElasticsearchInstanceTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchInstanceTypeList :: Encode ElasticsearchInstanceTypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>List of supported elastic search versions. </p>
newtype ElasticsearchVersionList = ElasticsearchVersionList (Array ElasticsearchVersionString)
derive instance newtypeElasticsearchVersionList :: Newtype ElasticsearchVersionList _
derive instance repGenericElasticsearchVersionList :: Generic ElasticsearchVersionList _
instance showElasticsearchVersionList :: Show ElasticsearchVersionList where
  show = genericShow
instance decodeElasticsearchVersionList :: Decode ElasticsearchVersionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchVersionList :: Encode ElasticsearchVersionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Status of the Elasticsearch version options for the specified Elasticsearch domain.</p>
newtype ElasticsearchVersionStatus = ElasticsearchVersionStatus 
  { "Options" :: (ElasticsearchVersionString)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeElasticsearchVersionStatus :: Newtype ElasticsearchVersionStatus _
derive instance repGenericElasticsearchVersionStatus :: Generic ElasticsearchVersionStatus _
instance showElasticsearchVersionStatus :: Show ElasticsearchVersionStatus where
  show = genericShow
instance decodeElasticsearchVersionStatus :: Decode ElasticsearchVersionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchVersionStatus :: Encode ElasticsearchVersionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchVersionString = ElasticsearchVersionString String
derive instance newtypeElasticsearchVersionString :: Newtype ElasticsearchVersionString _
derive instance repGenericElasticsearchVersionString :: Generic ElasticsearchVersionString _
instance showElasticsearchVersionString :: Show ElasticsearchVersionString where
  show = genericShow
instance decodeElasticsearchVersionString :: Decode ElasticsearchVersionString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchVersionString :: Encode ElasticsearchVersionString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the Encryption At Rest Options.</p>
newtype EncryptionAtRestOptions = EncryptionAtRestOptions 
  { "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (KmsKeyId)
  }
derive instance newtypeEncryptionAtRestOptions :: Newtype EncryptionAtRestOptions _
derive instance repGenericEncryptionAtRestOptions :: Generic EncryptionAtRestOptions _
instance showEncryptionAtRestOptions :: Show EncryptionAtRestOptions where
  show = genericShow
instance decodeEncryptionAtRestOptions :: Decode EncryptionAtRestOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionAtRestOptions :: Encode EncryptionAtRestOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Status of the Encryption At Rest options for the specified Elasticsearch domain.</p>
newtype EncryptionAtRestOptionsStatus = EncryptionAtRestOptionsStatus 
  { "Options" :: (EncryptionAtRestOptions)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeEncryptionAtRestOptionsStatus :: Newtype EncryptionAtRestOptionsStatus _
derive instance repGenericEncryptionAtRestOptionsStatus :: Generic EncryptionAtRestOptionsStatus _
instance showEncryptionAtRestOptionsStatus :: Show EncryptionAtRestOptionsStatus where
  show = genericShow
instance decodeEncryptionAtRestOptionsStatus :: Decode EncryptionAtRestOptionsStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionAtRestOptionsStatus :: Encode EncryptionAtRestOptionsStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EndpointsMap = EndpointsMap (StrMap.StrMap ServiceUrl)
derive instance newtypeEndpointsMap :: Newtype EndpointsMap _
derive instance repGenericEndpointsMap :: Generic EndpointsMap _
instance showEndpointsMap :: Show EndpointsMap where
  show = genericShow
instance decodeEndpointsMap :: Decode EndpointsMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointsMap :: Encode EndpointsMap where
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


-- | <p> InstanceCountLimits represents the limits on number of instances that be created in Amazon Elasticsearch for given InstanceType. </p>
newtype InstanceCountLimits = InstanceCountLimits 
  { "MinimumInstanceCount" :: NullOrUndefined.NullOrUndefined (MinimumInstanceCount)
  , "MaximumInstanceCount" :: NullOrUndefined.NullOrUndefined (MaximumInstanceCount)
  }
derive instance newtypeInstanceCountLimits :: Newtype InstanceCountLimits _
derive instance repGenericInstanceCountLimits :: Generic InstanceCountLimits _
instance showInstanceCountLimits :: Show InstanceCountLimits where
  show = genericShow
instance decodeInstanceCountLimits :: Decode InstanceCountLimits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceCountLimits :: Encode InstanceCountLimits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>InstanceLimits represents the list of instance related attributes that are available for given InstanceType. </p>
newtype InstanceLimits = InstanceLimits 
  { "InstanceCountLimits" :: NullOrUndefined.NullOrUndefined (InstanceCountLimits)
  }
derive instance newtypeInstanceLimits :: Newtype InstanceLimits _
derive instance repGenericInstanceLimits :: Generic InstanceLimits _
instance showInstanceLimits :: Show InstanceLimits where
  show = genericShow
instance decodeInstanceLimits :: Decode InstanceLimits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceLimits :: Encode InstanceLimits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceRole = InstanceRole String
derive instance newtypeInstanceRole :: Newtype InstanceRole _
derive instance repGenericInstanceRole :: Generic InstanceRole _
instance showInstanceRole :: Show InstanceRole where
  show = genericShow
instance decodeInstanceRole :: Decode InstanceRole where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceRole :: Encode InstanceRole where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntegerClass = IntegerClass Int
derive instance newtypeIntegerClass :: Newtype IntegerClass _
derive instance repGenericIntegerClass :: Generic IntegerClass _
instance showIntegerClass :: Show IntegerClass where
  show = genericShow
instance decodeIntegerClass :: Decode IntegerClass where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntegerClass :: Encode IntegerClass where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request processing has failed because of an unknown error, exception or failure (the failure is internal to the service) . Gives http status code of 500.</p>
newtype InternalException = InternalException Types.NoArguments
derive instance newtypeInternalException :: Newtype InternalException _
derive instance repGenericInternalException :: Generic InternalException _
instance showInternalException :: Show InternalException where
  show = genericShow
instance decodeInternalException :: Decode InternalException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalException :: Encode InternalException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An exception for trying to create or access sub-resource that is either invalid or not supported. Gives http status code of 409.</p>
newtype InvalidTypeException = InvalidTypeException Types.NoArguments
derive instance newtypeInvalidTypeException :: Newtype InvalidTypeException _
derive instance repGenericInvalidTypeException :: Generic InvalidTypeException _
instance showInvalidTypeException :: Show InvalidTypeException where
  show = genericShow
instance decodeInvalidTypeException :: Decode InvalidTypeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTypeException :: Encode InvalidTypeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KmsKeyId = KmsKeyId String
derive instance newtypeKmsKeyId :: Newtype KmsKeyId _
derive instance repGenericKmsKeyId :: Generic KmsKeyId _
instance showKmsKeyId :: Show KmsKeyId where
  show = genericShow
instance decodeKmsKeyId :: Decode KmsKeyId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKmsKeyId :: Encode KmsKeyId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An exception for trying to create more than allowed resources or sub-resources. Gives http status code of 409.</p>
newtype LimitExceededException = LimitExceededException Types.NoArguments
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LimitName = LimitName String
derive instance newtypeLimitName :: Newtype LimitName _
derive instance repGenericLimitName :: Generic LimitName _
instance showLimitName :: Show LimitName where
  show = genericShow
instance decodeLimitName :: Decode LimitName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitName :: Encode LimitName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LimitValue = LimitValue String
derive instance newtypeLimitValue :: Newtype LimitValue _
derive instance repGenericLimitValue :: Generic LimitValue _
instance showLimitValue :: Show LimitValue where
  show = genericShow
instance decodeLimitValue :: Decode LimitValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitValue :: Encode LimitValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LimitValueList = LimitValueList (Array LimitValue)
derive instance newtypeLimitValueList :: Newtype LimitValueList _
derive instance repGenericLimitValueList :: Generic LimitValueList _
instance showLimitValueList :: Show LimitValueList where
  show = genericShow
instance decodeLimitValueList :: Decode LimitValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitValueList :: Encode LimitValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Limits for given InstanceType and for each of it's role. <br/> Limits contains following <code> <a>StorageTypes,</a> </code> <code> <a>InstanceLimits</a> </code> and <code> <a>AdditionalLimits</a> </code> </p>
newtype Limits = Limits 
  { "StorageTypes" :: NullOrUndefined.NullOrUndefined (StorageTypeList)
  , "InstanceLimits" :: NullOrUndefined.NullOrUndefined (InstanceLimits)
  , "AdditionalLimits" :: NullOrUndefined.NullOrUndefined (AdditionalLimitList)
  }
derive instance newtypeLimits :: Newtype Limits _
derive instance repGenericLimits :: Generic Limits _
instance showLimits :: Show Limits where
  show = genericShow
instance decodeLimits :: Decode Limits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimits :: Encode Limits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Map of Role of the Instance and Limits that are applicable. Role performed by given Instance in Elasticsearch can be one of the following: <ul> <li>Data: If the given InstanceType is used as Data node</li> <li>Master: If the given InstanceType is used as Master node</li> </ul> </p>
newtype LimitsByRole = LimitsByRole (StrMap.StrMap Limits)
derive instance newtypeLimitsByRole :: Newtype LimitsByRole _
derive instance repGenericLimitsByRole :: Generic LimitsByRole _
instance showLimitsByRole :: Show LimitsByRole where
  show = genericShow
instance decodeLimitsByRole :: Decode LimitsByRole where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitsByRole :: Encode LimitsByRole where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a <code>ListDomainNames</code> operation. Contains the names of all Elasticsearch domains owned by this account.</p>
newtype ListDomainNamesResponse = ListDomainNamesResponse 
  { "DomainNames" :: NullOrUndefined.NullOrUndefined (DomainInfoList)
  }
derive instance newtypeListDomainNamesResponse :: Newtype ListDomainNamesResponse _
derive instance repGenericListDomainNamesResponse :: Generic ListDomainNamesResponse _
instance showListDomainNamesResponse :: Show ListDomainNamesResponse where
  show = genericShow
instance decodeListDomainNamesResponse :: Decode ListDomainNamesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDomainNamesResponse :: Encode ListDomainNamesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Container for the parameters to the <code> <a>ListElasticsearchInstanceTypes</a> </code> operation. </p>
newtype ListElasticsearchInstanceTypesRequest = ListElasticsearchInstanceTypesRequest 
  { "ElasticsearchVersion" :: (ElasticsearchVersionString)
  , "DomainName" :: NullOrUndefined.NullOrUndefined (DomainName)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListElasticsearchInstanceTypesRequest :: Newtype ListElasticsearchInstanceTypesRequest _
derive instance repGenericListElasticsearchInstanceTypesRequest :: Generic ListElasticsearchInstanceTypesRequest _
instance showListElasticsearchInstanceTypesRequest :: Show ListElasticsearchInstanceTypesRequest where
  show = genericShow
instance decodeListElasticsearchInstanceTypesRequest :: Decode ListElasticsearchInstanceTypesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListElasticsearchInstanceTypesRequest :: Encode ListElasticsearchInstanceTypesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Container for the parameters returned by <code> <a>ListElasticsearchInstanceTypes</a> </code> operation. </p>
newtype ListElasticsearchInstanceTypesResponse = ListElasticsearchInstanceTypesResponse 
  { "ElasticsearchInstanceTypes" :: NullOrUndefined.NullOrUndefined (ElasticsearchInstanceTypeList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListElasticsearchInstanceTypesResponse :: Newtype ListElasticsearchInstanceTypesResponse _
derive instance repGenericListElasticsearchInstanceTypesResponse :: Generic ListElasticsearchInstanceTypesResponse _
instance showListElasticsearchInstanceTypesResponse :: Show ListElasticsearchInstanceTypesResponse where
  show = genericShow
instance decodeListElasticsearchInstanceTypesResponse :: Decode ListElasticsearchInstanceTypesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListElasticsearchInstanceTypesResponse :: Encode ListElasticsearchInstanceTypesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Container for the parameters to the <code> <a>ListElasticsearchVersions</a> </code> operation. <p> Use <code> <a>MaxResults</a> </code> to control the maximum number of results to retrieve in a single call. </p> <p> Use <code> <a>NextToken</a> </code> in response to retrieve more results. If the received response does not contain a NextToken, then there are no more results to retrieve. </p> </p>
newtype ListElasticsearchVersionsRequest = ListElasticsearchVersionsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListElasticsearchVersionsRequest :: Newtype ListElasticsearchVersionsRequest _
derive instance repGenericListElasticsearchVersionsRequest :: Generic ListElasticsearchVersionsRequest _
instance showListElasticsearchVersionsRequest :: Show ListElasticsearchVersionsRequest where
  show = genericShow
instance decodeListElasticsearchVersionsRequest :: Decode ListElasticsearchVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListElasticsearchVersionsRequest :: Encode ListElasticsearchVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Container for the parameters for response received from <code> <a>ListElasticsearchVersions</a> </code> operation. </p>
newtype ListElasticsearchVersionsResponse = ListElasticsearchVersionsResponse 
  { "ElasticsearchVersions" :: NullOrUndefined.NullOrUndefined (ElasticsearchVersionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListElasticsearchVersionsResponse :: Newtype ListElasticsearchVersionsResponse _
derive instance repGenericListElasticsearchVersionsResponse :: Generic ListElasticsearchVersionsResponse _
instance showListElasticsearchVersionsResponse :: Show ListElasticsearchVersionsResponse where
  show = genericShow
instance decodeListElasticsearchVersionsResponse :: Decode ListElasticsearchVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListElasticsearchVersionsResponse :: Encode ListElasticsearchVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the <code><a>ListTags</a></code> operation. Specify the <code>ARN</code> for the Elasticsearch domain to which the tags are attached that you want to view are attached.</p>
newtype ListTagsRequest = ListTagsRequest 
  { "ARN" :: (ARN)
  }
derive instance newtypeListTagsRequest :: Newtype ListTagsRequest _
derive instance repGenericListTagsRequest :: Generic ListTagsRequest _
instance showListTagsRequest :: Show ListTagsRequest where
  show = genericShow
instance decodeListTagsRequest :: Decode ListTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsRequest :: Encode ListTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a <code>ListTags</code> operation. Contains tags for all requested Elasticsearch domains.</p>
newtype ListTagsResponse = ListTagsResponse 
  { "TagList" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeListTagsResponse :: Newtype ListTagsResponse _
derive instance repGenericListTagsResponse :: Generic ListTagsResponse _
instance showListTagsResponse :: Show ListTagsResponse where
  show = genericShow
instance decodeListTagsResponse :: Decode ListTagsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsResponse :: Encode ListTagsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Log Publishing option that is set for given domain. <br/>Attributes and their details: <ul> <li>CloudWatchLogsLogGroupArn: ARN of the Cloudwatch log group to which log needs to be published.</li> <li>Enabled: Whether the log publishing for given log type is enabled or not</li> </ul> </p>
newtype LogPublishingOption = LogPublishingOption 
  { "CloudWatchLogsLogGroupArn" :: NullOrUndefined.NullOrUndefined (CloudWatchLogsLogGroupArn)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeLogPublishingOption :: Newtype LogPublishingOption _
derive instance repGenericLogPublishingOption :: Generic LogPublishingOption _
instance showLogPublishingOption :: Show LogPublishingOption where
  show = genericShow
instance decodeLogPublishingOption :: Decode LogPublishingOption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogPublishingOption :: Encode LogPublishingOption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogPublishingOptions = LogPublishingOptions (StrMap.StrMap LogPublishingOption)
derive instance newtypeLogPublishingOptions :: Newtype LogPublishingOptions _
derive instance repGenericLogPublishingOptions :: Generic LogPublishingOptions _
instance showLogPublishingOptions :: Show LogPublishingOptions where
  show = genericShow
instance decodeLogPublishingOptions :: Decode LogPublishingOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogPublishingOptions :: Encode LogPublishingOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The configured log publishing options for the domain and their current status.</p>
newtype LogPublishingOptionsStatus = LogPublishingOptionsStatus 
  { "Options" :: NullOrUndefined.NullOrUndefined (LogPublishingOptions)
  , "Status" :: NullOrUndefined.NullOrUndefined (OptionStatus)
  }
derive instance newtypeLogPublishingOptionsStatus :: Newtype LogPublishingOptionsStatus _
derive instance repGenericLogPublishingOptionsStatus :: Generic LogPublishingOptionsStatus _
instance showLogPublishingOptionsStatus :: Show LogPublishingOptionsStatus where
  show = genericShow
instance decodeLogPublishingOptionsStatus :: Decode LogPublishingOptionsStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogPublishingOptionsStatus :: Encode LogPublishingOptionsStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Type of Log File, it can be one of the following: <ul> <li>INDEX_SLOW_LOGS: Index slow logs contains insert requests that took more time than configured index query log threshold to execute.</li> <li>SEARCH_SLOW_LOGS: Search slow logs contains search queries that took more time than configured search query log threshold to execute.</li> </ul> </p>
newtype LogType = LogType String
derive instance newtypeLogType :: Newtype LogType _
derive instance repGenericLogType :: Generic LogType _
instance showLogType :: Show LogType where
  show = genericShow
instance decodeLogType :: Decode LogType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogType :: Encode LogType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Set this value to limit the number of results returned. </p>
newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Maximum number of Instances that can be instantiated for given InstanceType. </p>
newtype MaximumInstanceCount = MaximumInstanceCount Int
derive instance newtypeMaximumInstanceCount :: Newtype MaximumInstanceCount _
derive instance repGenericMaximumInstanceCount :: Generic MaximumInstanceCount _
instance showMaximumInstanceCount :: Show MaximumInstanceCount where
  show = genericShow
instance decodeMaximumInstanceCount :: Decode MaximumInstanceCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaximumInstanceCount :: Encode MaximumInstanceCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Minimum number of Instances that can be instantiated for given InstanceType. </p>
newtype MinimumInstanceCount = MinimumInstanceCount Int
derive instance newtypeMinimumInstanceCount :: Newtype MinimumInstanceCount _
derive instance repGenericMinimumInstanceCount :: Generic MinimumInstanceCount _
instance showMinimumInstanceCount :: Show MinimumInstanceCount where
  show = genericShow
instance decodeMinimumInstanceCount :: Decode MinimumInstanceCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMinimumInstanceCount :: Encode MinimumInstanceCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Paginated APIs accepts NextToken input to returns next page results and provides a NextToken output in the response which can be used by the client to retrieve more results. </p>
newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The state of a requested change. One of the following:</p> <ul> <li>Processing: The request change is still in-process.</li> <li>Active: The request change is processed and deployed to the Elasticsearch domain.</li> </ul>
newtype OptionState = OptionState String
derive instance newtypeOptionState :: Newtype OptionState _
derive instance repGenericOptionState :: Generic OptionState _
instance showOptionState :: Show OptionState where
  show = genericShow
instance decodeOptionState :: Decode OptionState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionState :: Encode OptionState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the current status of the entity.</p>
newtype OptionStatus = OptionStatus 
  { "CreationDate" :: (UpdateTimestamp)
  , "UpdateDate" :: (UpdateTimestamp)
  , "UpdateVersion" :: NullOrUndefined.NullOrUndefined (UIntValue)
  , "State" :: (OptionState)
  , "PendingDeletion" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeOptionStatus :: Newtype OptionStatus _
derive instance repGenericOptionStatus :: Generic OptionStatus _
instance showOptionStatus :: Show OptionStatus where
  show = genericShow
instance decodeOptionStatus :: Decode OptionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionStatus :: Encode OptionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Access policy rules for an Elasticsearch domain service endpoints. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-access-policies" target="_blank">Configuring Access Policies</a> in the <i>Amazon Elasticsearch Service Developer Guide</i>. The maximum size of a policy document is 100 KB.</p>
newtype PolicyDocument = PolicyDocument String
derive instance newtypePolicyDocument :: Newtype PolicyDocument _
derive instance repGenericPolicyDocument :: Generic PolicyDocument _
instance showPolicyDocument :: Show PolicyDocument where
  show = genericShow
instance decodePolicyDocument :: Decode PolicyDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyDocument :: Encode PolicyDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the <code><a>RemoveTags</a></code> operation. Specify the <code>ARN</code> for the Elasticsearch domain from which you want to remove the specified <code>TagKey</code>.</p>
newtype RemoveTagsRequest = RemoveTagsRequest 
  { "ARN" :: (ARN)
  , "TagKeys" :: (StringList)
  }
derive instance newtypeRemoveTagsRequest :: Newtype RemoveTagsRequest _
derive instance repGenericRemoveTagsRequest :: Generic RemoveTagsRequest _
instance showRemoveTagsRequest :: Show RemoveTagsRequest where
  show = genericShow
instance decodeRemoveTagsRequest :: Decode RemoveTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsRequest :: Encode RemoveTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An exception for creating a resource that already exists. Gives http status code of 400.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException Types.NoArguments
derive instance newtypeResourceAlreadyExistsException :: Newtype ResourceAlreadyExistsException _
derive instance repGenericResourceAlreadyExistsException :: Generic ResourceAlreadyExistsException _
instance showResourceAlreadyExistsException :: Show ResourceAlreadyExistsException where
  show = genericShow
instance decodeResourceAlreadyExistsException :: Decode ResourceAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceAlreadyExistsException :: Encode ResourceAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An exception for accessing or deleting a resource that does not exist. Gives http status code of 400.</p>
newtype ResourceNotFoundException = ResourceNotFoundException Types.NoArguments
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The endpoint to which service requests are submitted. For example, <code>search-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.es.amazonaws.com</code> or <code>doc-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.es.amazonaws.com</code>.</p>
newtype ServiceUrl = ServiceUrl String
derive instance newtypeServiceUrl :: Newtype ServiceUrl _
derive instance repGenericServiceUrl :: Generic ServiceUrl _
instance showServiceUrl :: Show ServiceUrl where
  show = genericShow
instance decodeServiceUrl :: Decode ServiceUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUrl :: Encode ServiceUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is <code>0</code> hours.</p>
newtype SnapshotOptions = SnapshotOptions 
  { "AutomatedSnapshotStartHour" :: NullOrUndefined.NullOrUndefined (IntegerClass)
  }
derive instance newtypeSnapshotOptions :: Newtype SnapshotOptions _
derive instance repGenericSnapshotOptions :: Generic SnapshotOptions _
instance showSnapshotOptions :: Show SnapshotOptions where
  show = genericShow
instance decodeSnapshotOptions :: Decode SnapshotOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotOptions :: Encode SnapshotOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Status of a daily automated snapshot.</p>
newtype SnapshotOptionsStatus = SnapshotOptionsStatus 
  { "Options" :: (SnapshotOptions)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeSnapshotOptionsStatus :: Newtype SnapshotOptionsStatus _
derive instance repGenericSnapshotOptionsStatus :: Generic SnapshotOptionsStatus _
instance showSnapshotOptionsStatus :: Show SnapshotOptionsStatus where
  show = genericShow
instance decodeSnapshotOptionsStatus :: Decode SnapshotOptionsStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotOptionsStatus :: Encode SnapshotOptionsStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> SubType of the given storage type. List of available sub-storage options: For "instance" storageType we wont have any storageSubType, in case of "ebs" storageType we will have following valid storageSubTypes <ol> <li>standard</li> <li>gp2</li> <li>io1</li> </ol> Refer <code><a>VolumeType</a></code> for more information regarding above EBS storage options. </p>
newtype StorageSubTypeName = StorageSubTypeName String
derive instance newtypeStorageSubTypeName :: Newtype StorageSubTypeName _
derive instance repGenericStorageSubTypeName :: Generic StorageSubTypeName _
instance showStorageSubTypeName :: Show StorageSubTypeName where
  show = genericShow
instance decodeStorageSubTypeName :: Decode StorageSubTypeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageSubTypeName :: Encode StorageSubTypeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>StorageTypes represents the list of storage related types and their attributes that are available for given InstanceType. </p>
newtype StorageType = StorageType 
  { "StorageTypeName" :: NullOrUndefined.NullOrUndefined (StorageTypeName)
  , "StorageSubTypeName" :: NullOrUndefined.NullOrUndefined (StorageSubTypeName)
  , "StorageTypeLimits" :: NullOrUndefined.NullOrUndefined (StorageTypeLimitList)
  }
derive instance newtypeStorageType :: Newtype StorageType _
derive instance repGenericStorageType :: Generic StorageType _
instance showStorageType :: Show StorageType where
  show = genericShow
instance decodeStorageType :: Decode StorageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageType :: Encode StorageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Limits that are applicable for given storage type. </p>
newtype StorageTypeLimit = StorageTypeLimit 
  { "LimitName" :: NullOrUndefined.NullOrUndefined (LimitName)
  , "LimitValues" :: NullOrUndefined.NullOrUndefined (LimitValueList)
  }
derive instance newtypeStorageTypeLimit :: Newtype StorageTypeLimit _
derive instance repGenericStorageTypeLimit :: Generic StorageTypeLimit _
instance showStorageTypeLimit :: Show StorageTypeLimit where
  show = genericShow
instance decodeStorageTypeLimit :: Decode StorageTypeLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageTypeLimit :: Encode StorageTypeLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StorageTypeLimitList = StorageTypeLimitList (Array StorageTypeLimit)
derive instance newtypeStorageTypeLimitList :: Newtype StorageTypeLimitList _
derive instance repGenericStorageTypeLimitList :: Generic StorageTypeLimitList _
instance showStorageTypeLimitList :: Show StorageTypeLimitList where
  show = genericShow
instance decodeStorageTypeLimitList :: Decode StorageTypeLimitList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageTypeLimitList :: Encode StorageTypeLimitList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StorageTypeList = StorageTypeList (Array StorageType)
derive instance newtypeStorageTypeList :: Newtype StorageTypeList _
derive instance repGenericStorageTypeList :: Generic StorageTypeList _
instance showStorageTypeList :: Show StorageTypeList where
  show = genericShow
instance decodeStorageTypeList :: Decode StorageTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageTypeList :: Encode StorageTypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Type of the storage. List of available storage options: <ol> <li>instance</li> Inbuilt storage available for the given Instance <li>ebs</li> Elastic block storage that would be attached to the given Instance </ol> </p>
newtype StorageTypeName = StorageTypeName String
derive instance newtypeStorageTypeName :: Newtype StorageTypeName _
derive instance repGenericStorageTypeName :: Generic StorageTypeName _
instance showStorageTypeName :: Show StorageTypeName where
  show = genericShow
instance decodeStorageTypeName :: Decode StorageTypeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageTypeName :: Encode StorageTypeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _
derive instance repGenericStringList :: Generic StringList _
instance showStringList :: Show StringList where
  show = genericShow
instance decodeStringList :: Decode StringList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringList :: Encode StringList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a key value pair for a resource tag.</p>
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


-- | <p>A string of length from 1 to 128 characters that specifies the key for a Tag. Tag keys must be unique for the Elasticsearch domain to which they are attached.</p>
newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of <code>Tag</code> </p>
newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A string of length from 0 to 256 characters that specifies the value for a Tag. Tag values can be null and do not have to be unique in a tag set.</p>
newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UIntValue = UIntValue Int
derive instance newtypeUIntValue :: Newtype UIntValue _
derive instance repGenericUIntValue :: Generic UIntValue _
instance showUIntValue :: Show UIntValue where
  show = genericShow
instance decodeUIntValue :: Decode UIntValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUIntValue :: Encode UIntValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the <code><a>UpdateElasticsearchDomain</a></code> operation. Specifies the type and number of instances in the domain cluster.</p>
newtype UpdateElasticsearchDomainConfigRequest = UpdateElasticsearchDomainConfigRequest 
  { "DomainName" :: (DomainName)
  , "ElasticsearchClusterConfig" :: NullOrUndefined.NullOrUndefined (ElasticsearchClusterConfig)
  , "EBSOptions" :: NullOrUndefined.NullOrUndefined (EBSOptions)
  , "SnapshotOptions" :: NullOrUndefined.NullOrUndefined (SnapshotOptions)
  , "VPCOptions" :: NullOrUndefined.NullOrUndefined (VPCOptions)
  , "AdvancedOptions" :: NullOrUndefined.NullOrUndefined (AdvancedOptions)
  , "AccessPolicies" :: NullOrUndefined.NullOrUndefined (PolicyDocument)
  , "LogPublishingOptions" :: NullOrUndefined.NullOrUndefined (LogPublishingOptions)
  }
derive instance newtypeUpdateElasticsearchDomainConfigRequest :: Newtype UpdateElasticsearchDomainConfigRequest _
derive instance repGenericUpdateElasticsearchDomainConfigRequest :: Generic UpdateElasticsearchDomainConfigRequest _
instance showUpdateElasticsearchDomainConfigRequest :: Show UpdateElasticsearchDomainConfigRequest where
  show = genericShow
instance decodeUpdateElasticsearchDomainConfigRequest :: Decode UpdateElasticsearchDomainConfigRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateElasticsearchDomainConfigRequest :: Encode UpdateElasticsearchDomainConfigRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of an <code>UpdateElasticsearchDomain</code> request. Contains the status of the Elasticsearch domain being updated.</p>
newtype UpdateElasticsearchDomainConfigResponse = UpdateElasticsearchDomainConfigResponse 
  { "DomainConfig" :: (ElasticsearchDomainConfig)
  }
derive instance newtypeUpdateElasticsearchDomainConfigResponse :: Newtype UpdateElasticsearchDomainConfigResponse _
derive instance repGenericUpdateElasticsearchDomainConfigResponse :: Generic UpdateElasticsearchDomainConfigResponse _
instance showUpdateElasticsearchDomainConfigResponse :: Show UpdateElasticsearchDomainConfigResponse where
  show = genericShow
instance decodeUpdateElasticsearchDomainConfigResponse :: Decode UpdateElasticsearchDomainConfigResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateElasticsearchDomainConfigResponse :: Encode UpdateElasticsearchDomainConfigResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTimestamp = UpdateTimestamp Number
derive instance newtypeUpdateTimestamp :: Newtype UpdateTimestamp _
derive instance repGenericUpdateTimestamp :: Generic UpdateTimestamp _
instance showUpdateTimestamp :: Show UpdateTimestamp where
  show = genericShow
instance decodeUpdateTimestamp :: Decode UpdateTimestamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTimestamp :: Encode UpdateTimestamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Options to specify the subnets and security groups for VPC endpoint. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html" target="_blank"> VPC Endpoints for Amazon Elasticsearch Service Domains</a>.</p>
newtype VPCDerivedInfo = VPCDerivedInfo 
  { "VPCId" :: NullOrUndefined.NullOrUndefined (String)
  , "SubnetIds" :: NullOrUndefined.NullOrUndefined (StringList)
  , "AvailabilityZones" :: NullOrUndefined.NullOrUndefined (StringList)
  , "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (StringList)
  }
derive instance newtypeVPCDerivedInfo :: Newtype VPCDerivedInfo _
derive instance repGenericVPCDerivedInfo :: Generic VPCDerivedInfo _
instance showVPCDerivedInfo :: Show VPCDerivedInfo where
  show = genericShow
instance decodeVPCDerivedInfo :: Decode VPCDerivedInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVPCDerivedInfo :: Encode VPCDerivedInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Status of the VPC options for the specified Elasticsearch domain.</p>
newtype VPCDerivedInfoStatus = VPCDerivedInfoStatus 
  { "Options" :: (VPCDerivedInfo)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeVPCDerivedInfoStatus :: Newtype VPCDerivedInfoStatus _
derive instance repGenericVPCDerivedInfoStatus :: Generic VPCDerivedInfoStatus _
instance showVPCDerivedInfoStatus :: Show VPCDerivedInfoStatus where
  show = genericShow
instance decodeVPCDerivedInfoStatus :: Decode VPCDerivedInfoStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVPCDerivedInfoStatus :: Encode VPCDerivedInfoStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Options to specify the subnets and security groups for VPC endpoint. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html" target="_blank"> VPC Endpoints for Amazon Elasticsearch Service Domains</a>.</p>
newtype VPCOptions = VPCOptions 
  { "SubnetIds" :: NullOrUndefined.NullOrUndefined (StringList)
  , "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (StringList)
  }
derive instance newtypeVPCOptions :: Newtype VPCOptions _
derive instance repGenericVPCOptions :: Generic VPCOptions _
instance showVPCOptions :: Show VPCOptions where
  show = genericShow
instance decodeVPCOptions :: Decode VPCOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVPCOptions :: Encode VPCOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An exception for missing / invalid input fields. Gives http status code of 400.</p>
newtype ValidationException = ValidationException Types.NoArguments
derive instance newtypeValidationException :: Newtype ValidationException _
derive instance repGenericValidationException :: Generic ValidationException _
instance showValidationException :: Show ValidationException where
  show = genericShow
instance decodeValidationException :: Decode ValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationException :: Encode ValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The type of EBS volume, standard, gp2, or io1. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs" target="_blank">Configuring EBS-based Storage</a>for more information.</p>
newtype VolumeType = VolumeType String
derive instance newtypeVolumeType :: Newtype VolumeType _
derive instance repGenericVolumeType :: Generic VolumeType _
instance showVolumeType :: Show VolumeType where
  show = genericShow
instance decodeVolumeType :: Decode VolumeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeType :: Encode VolumeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
