

-- | <fullname>Amazon Elasticsearch Configuration Service</fullname> <p>Use the Amazon Elasticsearch configuration API to create, configure, and manage Elasticsearch domains.</p> <p>The endpoint for configuration service requests is region-specific: es.<i>region</i>.amazonaws.com. For example, es.us-east-1.amazonaws.com. For a current list of supported regions and endpoints, see <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#elasticsearch-service-regions" target="_blank">Regions and Endpoints</a>.</p>
module AWS.ES where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ES" :: String


-- | <p>Attaches tags to an existing Elasticsearch domain. Tags are a set of case-sensitive key value pairs. An Elasticsearch domain may have up to 10 tags. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-awsresorcetagging" target="_blank"> Tagging Amazon Elasticsearch Service Domains for more information.</a></p>
addTags :: forall eff. AddTagsRequest -> Aff (err :: AWS.RequestError | eff) Unit
addTags = AWS.request serviceName "AddTags" 


-- | <p>Creates a new Elasticsearch domain. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains" target="_blank">Creating Elasticsearch Domains</a> in the <i>Amazon Elasticsearch Service Developer Guide</i>.</p>
createElasticsearchDomain :: forall eff. CreateElasticsearchDomainRequest -> Aff (err :: AWS.RequestError | eff) CreateElasticsearchDomainResponse
createElasticsearchDomain = AWS.request serviceName "CreateElasticsearchDomain" 


-- | <p>Permanently deletes the specified Elasticsearch domain and all of its data. Once a domain is deleted, it cannot be recovered.</p>
deleteElasticsearchDomain :: forall eff. DeleteElasticsearchDomainRequest -> Aff (err :: AWS.RequestError | eff) DeleteElasticsearchDomainResponse
deleteElasticsearchDomain = AWS.request serviceName "DeleteElasticsearchDomain" 


-- | <p>Deletes the service-linked role that Elasticsearch Service uses to manage and maintain VPC domains. Role deletion will fail if any existing VPC domains use the role. You must delete any such Elasticsearch domains before deleting the role. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-enabling-slr" target="_blank">Deleting Elasticsearch Service Role</a> in <i>VPC Endpoints for Amazon Elasticsearch Service Domains</i>.</p>
deleteElasticsearchServiceRole :: forall eff.  Aff (err :: AWS.RequestError | eff) Unit
deleteElasticsearchServiceRole = AWS.request serviceName "DeleteElasticsearchServiceRole" unit


-- | <p>Returns domain configuration information about the specified Elasticsearch domain, including the domain ID, domain endpoint, and domain ARN.</p>
describeElasticsearchDomain :: forall eff. DescribeElasticsearchDomainRequest -> Aff (err :: AWS.RequestError | eff) DescribeElasticsearchDomainResponse
describeElasticsearchDomain = AWS.request serviceName "DescribeElasticsearchDomain" 


-- | <p>Provides cluster configuration information about the specified Elasticsearch domain, such as the state, creation date, update version, and update date for cluster options.</p>
describeElasticsearchDomainConfig :: forall eff. DescribeElasticsearchDomainConfigRequest -> Aff (err :: AWS.RequestError | eff) DescribeElasticsearchDomainConfigResponse
describeElasticsearchDomainConfig = AWS.request serviceName "DescribeElasticsearchDomainConfig" 


-- | <p>Returns domain configuration information about the specified Elasticsearch domains, including the domain ID, domain endpoint, and domain ARN.</p>
describeElasticsearchDomains :: forall eff. DescribeElasticsearchDomainsRequest -> Aff (err :: AWS.RequestError | eff) DescribeElasticsearchDomainsResponse
describeElasticsearchDomains = AWS.request serviceName "DescribeElasticsearchDomains" 


-- | <p> Describe Elasticsearch Limits for a given InstanceType and ElasticsearchVersion. When modifying existing Domain, specify the <code> <a>DomainName</a> </code> to know what Limits are supported for modifying. </p>
describeElasticsearchInstanceTypeLimits :: forall eff. DescribeElasticsearchInstanceTypeLimitsRequest -> Aff (err :: AWS.RequestError | eff) DescribeElasticsearchInstanceTypeLimitsResponse
describeElasticsearchInstanceTypeLimits = AWS.request serviceName "DescribeElasticsearchInstanceTypeLimits" 


-- | <p>Returns the name of all Elasticsearch domains owned by the current user's account. </p>
listDomainNames :: forall eff.  Aff (err :: AWS.RequestError | eff) ListDomainNamesResponse
listDomainNames = AWS.request serviceName "ListDomainNames" unit


-- | <p>List all Elasticsearch instance types that are supported for given ElasticsearchVersion</p>
listElasticsearchInstanceTypes :: forall eff. ListElasticsearchInstanceTypesRequest -> Aff (err :: AWS.RequestError | eff) ListElasticsearchInstanceTypesResponse
listElasticsearchInstanceTypes = AWS.request serviceName "ListElasticsearchInstanceTypes" 


-- | <p>List all supported Elasticsearch versions</p>
listElasticsearchVersions :: forall eff. ListElasticsearchVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListElasticsearchVersionsResponse
listElasticsearchVersions = AWS.request serviceName "ListElasticsearchVersions" 


-- | <p>Returns all tags for the given Elasticsearch domain.</p>
listTags :: forall eff. ListTagsRequest -> Aff (err :: AWS.RequestError | eff) ListTagsResponse
listTags = AWS.request serviceName "ListTags" 


-- | <p>Removes the specified set of tags from the specified Elasticsearch domain.</p>
removeTags :: forall eff. RemoveTagsRequest -> Aff (err :: AWS.RequestError | eff) Unit
removeTags = AWS.request serviceName "RemoveTags" 


-- | <p>Modifies the cluster configuration of the specified Elasticsearch domain, setting as setting the instance type and the number of instances. </p>
updateElasticsearchDomainConfig :: forall eff. UpdateElasticsearchDomainConfigRequest -> Aff (err :: AWS.RequestError | eff) UpdateElasticsearchDomainConfigResponse
updateElasticsearchDomainConfig = AWS.request serviceName "UpdateElasticsearchDomainConfig" 


-- | <p>The Amazon Resource Name (ARN) of the Elasticsearch domain. See <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html" target="_blank">Identifiers for IAM Entities</a> in <i>Using AWS Identity and Access Management</i> for more information.</p>
newtype ARN = ARN String
derive instance newtypeARN :: Newtype ARN _


-- | <p>The configured access rules for the domain's document and search endpoints, and the current status of those rules.</p>
newtype AccessPoliciesStatus = AccessPoliciesStatus 
  { "Options" :: (PolicyDocument)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeAccessPoliciesStatus :: Newtype AccessPoliciesStatus _


-- | <p>Container for the parameters to the <code><a>AddTags</a></code> operation. Specify the tags that you want to attach to the Elasticsearch domain.</p>
newtype AddTagsRequest = AddTagsRequest 
  { "ARN" :: (ARN)
  , "TagList" :: (TagList)
  }
derive instance newtypeAddTagsRequest :: Newtype AddTagsRequest _


-- | <p> List of limits that are specific to a given InstanceType and for each of it's <code> <a>InstanceRole</a> </code> . </p>
newtype AdditionalLimit = AdditionalLimit 
  { "LimitName" :: NullOrUndefined (LimitName)
  , "LimitValues" :: NullOrUndefined (LimitValueList)
  }
derive instance newtypeAdditionalLimit :: Newtype AdditionalLimit _


newtype AdditionalLimitList = AdditionalLimitList (Array AdditionalLimit)
derive instance newtypeAdditionalLimitList :: Newtype AdditionalLimitList _


-- | <p> Exposes select native Elasticsearch configuration values from <code>elasticsearch.yml</code>. Currently, the following advanced options are available:</p> <ul> <li>Option to allow references to indices in an HTTP request body. Must be <code>false</code> when configuring access to individual sub-resources. By default, the value is <code>true</code>. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options" target="_blank">Configuration Advanced Options</a> for more information.</li> <li>Option to specify the percentage of heap space that is allocated to field data. By default, this setting is unbounded.</li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options">Configuring Advanced Options</a>.</p>
newtype AdvancedOptions = AdvancedOptions (Map String String)
derive instance newtypeAdvancedOptions :: Newtype AdvancedOptions _


-- | <p> Status of the advanced options for the specified Elasticsearch domain. Currently, the following advanced options are available:</p> <ul> <li>Option to allow references to indices in an HTTP request body. Must be <code>false</code> when configuring access to individual sub-resources. By default, the value is <code>true</code>. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options" target="_blank">Configuration Advanced Options</a> for more information.</li> <li>Option to specify the percentage of heap space that is allocated to field data. By default, this setting is unbounded.</li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options">Configuring Advanced Options</a>.</p>
newtype AdvancedOptionsStatus = AdvancedOptionsStatus 
  { "Options" :: (AdvancedOptions)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeAdvancedOptionsStatus :: Newtype AdvancedOptionsStatus _


-- | <p>An error occurred while processing the request.</p>
newtype BaseException = BaseException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBaseException :: Newtype BaseException _


-- | <p>ARN of the Cloudwatch log group to which log needs to be published.</p>
newtype CloudWatchLogsLogGroupArn = CloudWatchLogsLogGroupArn String
derive instance newtypeCloudWatchLogsLogGroupArn :: Newtype CloudWatchLogsLogGroupArn _


newtype CreateElasticsearchDomainRequest = CreateElasticsearchDomainRequest 
  { "DomainName" :: (DomainName)
  , "ElasticsearchVersion" :: NullOrUndefined (ElasticsearchVersionString)
  , "ElasticsearchClusterConfig" :: NullOrUndefined (ElasticsearchClusterConfig)
  , "EBSOptions" :: NullOrUndefined (EBSOptions)
  , "AccessPolicies" :: NullOrUndefined (PolicyDocument)
  , "SnapshotOptions" :: NullOrUndefined (SnapshotOptions)
  , "VPCOptions" :: NullOrUndefined (VPCOptions)
  , "EncryptionAtRestOptions" :: NullOrUndefined (EncryptionAtRestOptions)
  , "AdvancedOptions" :: NullOrUndefined (AdvancedOptions)
  , "LogPublishingOptions" :: NullOrUndefined (LogPublishingOptions)
  }
derive instance newtypeCreateElasticsearchDomainRequest :: Newtype CreateElasticsearchDomainRequest _


-- | <p>The result of a <code>CreateElasticsearchDomain</code> operation. Contains the status of the newly created Elasticsearch domain.</p>
newtype CreateElasticsearchDomainResponse = CreateElasticsearchDomainResponse 
  { "DomainStatus" :: NullOrUndefined (ElasticsearchDomainStatus)
  }
derive instance newtypeCreateElasticsearchDomainResponse :: Newtype CreateElasticsearchDomainResponse _


-- | <p>Container for the parameters to the <code><a>DeleteElasticsearchDomain</a></code> operation. Specifies the name of the Elasticsearch domain that you want to delete.</p>
newtype DeleteElasticsearchDomainRequest = DeleteElasticsearchDomainRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDeleteElasticsearchDomainRequest :: Newtype DeleteElasticsearchDomainRequest _


-- | <p>The result of a <code>DeleteElasticsearchDomain</code> request. Contains the status of the pending deletion, or no status if the domain and all of its resources have been deleted.</p>
newtype DeleteElasticsearchDomainResponse = DeleteElasticsearchDomainResponse 
  { "DomainStatus" :: NullOrUndefined (ElasticsearchDomainStatus)
  }
derive instance newtypeDeleteElasticsearchDomainResponse :: Newtype DeleteElasticsearchDomainResponse _


-- | <p> Container for the parameters to the <code>DescribeElasticsearchDomainConfig</code> operation. Specifies the domain name for which you want configuration information.</p>
newtype DescribeElasticsearchDomainConfigRequest = DescribeElasticsearchDomainConfigRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDescribeElasticsearchDomainConfigRequest :: Newtype DescribeElasticsearchDomainConfigRequest _


-- | <p>The result of a <code>DescribeElasticsearchDomainConfig</code> request. Contains the configuration information of the requested domain.</p>
newtype DescribeElasticsearchDomainConfigResponse = DescribeElasticsearchDomainConfigResponse 
  { "DomainConfig" :: (ElasticsearchDomainConfig)
  }
derive instance newtypeDescribeElasticsearchDomainConfigResponse :: Newtype DescribeElasticsearchDomainConfigResponse _


-- | <p>Container for the parameters to the <code><a>DescribeElasticsearchDomain</a></code> operation.</p>
newtype DescribeElasticsearchDomainRequest = DescribeElasticsearchDomainRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDescribeElasticsearchDomainRequest :: Newtype DescribeElasticsearchDomainRequest _


-- | <p>The result of a <code>DescribeElasticsearchDomain</code> request. Contains the status of the domain specified in the request.</p>
newtype DescribeElasticsearchDomainResponse = DescribeElasticsearchDomainResponse 
  { "DomainStatus" :: (ElasticsearchDomainStatus)
  }
derive instance newtypeDescribeElasticsearchDomainResponse :: Newtype DescribeElasticsearchDomainResponse _


-- | <p>Container for the parameters to the <code><a>DescribeElasticsearchDomains</a></code> operation. By default, the API returns the status of all Elasticsearch domains.</p>
newtype DescribeElasticsearchDomainsRequest = DescribeElasticsearchDomainsRequest 
  { "DomainNames" :: (DomainNameList)
  }
derive instance newtypeDescribeElasticsearchDomainsRequest :: Newtype DescribeElasticsearchDomainsRequest _


-- | <p>The result of a <code>DescribeElasticsearchDomains</code> request. Contains the status of the specified domains or all domains owned by the account.</p>
newtype DescribeElasticsearchDomainsResponse = DescribeElasticsearchDomainsResponse 
  { "DomainStatusList" :: (ElasticsearchDomainStatusList)
  }
derive instance newtypeDescribeElasticsearchDomainsResponse :: Newtype DescribeElasticsearchDomainsResponse _


-- | <p> Container for the parameters to <code> <a>DescribeElasticsearchInstanceTypeLimits</a> </code> operation. </p>
newtype DescribeElasticsearchInstanceTypeLimitsRequest = DescribeElasticsearchInstanceTypeLimitsRequest 
  { "DomainName" :: NullOrUndefined (DomainName)
  , "InstanceType" :: (ESPartitionInstanceType)
  , "ElasticsearchVersion" :: (ElasticsearchVersionString)
  }
derive instance newtypeDescribeElasticsearchInstanceTypeLimitsRequest :: Newtype DescribeElasticsearchInstanceTypeLimitsRequest _


-- | <p> Container for the parameters received from <code> <a>DescribeElasticsearchInstanceTypeLimits</a> </code> operation. </p>
newtype DescribeElasticsearchInstanceTypeLimitsResponse = DescribeElasticsearchInstanceTypeLimitsResponse 
  { "LimitsByRole" :: NullOrUndefined (LimitsByRole)
  }
derive instance newtypeDescribeElasticsearchInstanceTypeLimitsResponse :: Newtype DescribeElasticsearchInstanceTypeLimitsResponse _


-- | <p>An error occured because the client wanted to access a not supported operation. Gives http status code of 409.</p>
newtype DisabledOperationException = DisabledOperationException 
  { 
  }
derive instance newtypeDisabledOperationException :: Newtype DisabledOperationException _


-- | <p>Unique identifier for an Elasticsearch domain.</p>
newtype DomainId = DomainId String
derive instance newtypeDomainId :: Newtype DomainId _


newtype DomainInfo = DomainInfo 
  { "DomainName" :: NullOrUndefined (DomainName)
  }
derive instance newtypeDomainInfo :: Newtype DomainInfo _


-- | <p> Contains the list of Elasticsearch domain information.</p>
newtype DomainInfoList = DomainInfoList (Array DomainInfo)
derive instance newtypeDomainInfoList :: Newtype DomainInfoList _


-- | <p>The name of an Elasticsearch domain. Domain names are unique across the domains owned by an account within an AWS region. Domain names start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).</p>
newtype DomainName = DomainName String
derive instance newtypeDomainName :: Newtype DomainName _


-- | <p>A list of Elasticsearch domain names.</p>
newtype DomainNameList = DomainNameList (Array DomainName)
derive instance newtypeDomainNameList :: Newtype DomainNameList _


-- | <p>Options to enable, disable, and specify the properties of EBS storage volumes. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs" target="_blank"> Configuring EBS-based Storage</a>.</p>
newtype EBSOptions = EBSOptions 
  { "EBSEnabled" :: NullOrUndefined (Boolean)
  , "VolumeType" :: NullOrUndefined (VolumeType)
  , "VolumeSize" :: NullOrUndefined (IntegerClass)
  , "Iops" :: NullOrUndefined (IntegerClass)
  }
derive instance newtypeEBSOptions :: Newtype EBSOptions _


-- | <p> Status of the EBS options for the specified Elasticsearch domain.</p>
newtype EBSOptionsStatus = EBSOptionsStatus 
  { "Options" :: (EBSOptions)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeEBSOptionsStatus :: Newtype EBSOptionsStatus _


newtype ESPartitionInstanceType = ESPartitionInstanceType String
derive instance newtypeESPartitionInstanceType :: Newtype ESPartitionInstanceType _


-- | <p>Specifies the configuration for the domain cluster, such as the type and number of instances.</p>
newtype ElasticsearchClusterConfig = ElasticsearchClusterConfig 
  { "InstanceType" :: NullOrUndefined (ESPartitionInstanceType)
  , "InstanceCount" :: NullOrUndefined (IntegerClass)
  , "DedicatedMasterEnabled" :: NullOrUndefined (Boolean)
  , "ZoneAwarenessEnabled" :: NullOrUndefined (Boolean)
  , "DedicatedMasterType" :: NullOrUndefined (ESPartitionInstanceType)
  , "DedicatedMasterCount" :: NullOrUndefined (IntegerClass)
  }
derive instance newtypeElasticsearchClusterConfig :: Newtype ElasticsearchClusterConfig _


-- | <p> Specifies the configuration status for the specified Elasticsearch domain.</p>
newtype ElasticsearchClusterConfigStatus = ElasticsearchClusterConfigStatus 
  { "Options" :: (ElasticsearchClusterConfig)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeElasticsearchClusterConfigStatus :: Newtype ElasticsearchClusterConfigStatus _


-- | <p>The configuration of an Elasticsearch domain.</p>
newtype ElasticsearchDomainConfig = ElasticsearchDomainConfig 
  { "ElasticsearchVersion" :: NullOrUndefined (ElasticsearchVersionStatus)
  , "ElasticsearchClusterConfig" :: NullOrUndefined (ElasticsearchClusterConfigStatus)
  , "EBSOptions" :: NullOrUndefined (EBSOptionsStatus)
  , "AccessPolicies" :: NullOrUndefined (AccessPoliciesStatus)
  , "SnapshotOptions" :: NullOrUndefined (SnapshotOptionsStatus)
  , "VPCOptions" :: NullOrUndefined (VPCDerivedInfoStatus)
  , "EncryptionAtRestOptions" :: NullOrUndefined (EncryptionAtRestOptionsStatus)
  , "AdvancedOptions" :: NullOrUndefined (AdvancedOptionsStatus)
  , "LogPublishingOptions" :: NullOrUndefined (LogPublishingOptionsStatus)
  }
derive instance newtypeElasticsearchDomainConfig :: Newtype ElasticsearchDomainConfig _


-- | <p>The current status of an Elasticsearch domain.</p>
newtype ElasticsearchDomainStatus = ElasticsearchDomainStatus 
  { "DomainId" :: (DomainId)
  , "DomainName" :: (DomainName)
  , "ARN" :: (ARN)
  , "Created" :: NullOrUndefined (Boolean)
  , "Deleted" :: NullOrUndefined (Boolean)
  , "Endpoint" :: NullOrUndefined (ServiceUrl)
  , "Endpoints" :: NullOrUndefined (EndpointsMap)
  , "Processing" :: NullOrUndefined (Boolean)
  , "ElasticsearchVersion" :: NullOrUndefined (ElasticsearchVersionString)
  , "ElasticsearchClusterConfig" :: (ElasticsearchClusterConfig)
  , "EBSOptions" :: NullOrUndefined (EBSOptions)
  , "AccessPolicies" :: NullOrUndefined (PolicyDocument)
  , "SnapshotOptions" :: NullOrUndefined (SnapshotOptions)
  , "VPCOptions" :: NullOrUndefined (VPCDerivedInfo)
  , "EncryptionAtRestOptions" :: NullOrUndefined (EncryptionAtRestOptions)
  , "AdvancedOptions" :: NullOrUndefined (AdvancedOptions)
  , "LogPublishingOptions" :: NullOrUndefined (LogPublishingOptions)
  }
derive instance newtypeElasticsearchDomainStatus :: Newtype ElasticsearchDomainStatus _


-- | <p>A list that contains the status of each requested Elasticsearch domain.</p>
newtype ElasticsearchDomainStatusList = ElasticsearchDomainStatusList (Array ElasticsearchDomainStatus)
derive instance newtypeElasticsearchDomainStatusList :: Newtype ElasticsearchDomainStatusList _


-- | <p> List of instance types supported by Amazon Elasticsearch service. </p>
newtype ElasticsearchInstanceTypeList = ElasticsearchInstanceTypeList (Array ESPartitionInstanceType)
derive instance newtypeElasticsearchInstanceTypeList :: Newtype ElasticsearchInstanceTypeList _


-- | <p>List of supported elastic search versions. </p>
newtype ElasticsearchVersionList = ElasticsearchVersionList (Array ElasticsearchVersionString)
derive instance newtypeElasticsearchVersionList :: Newtype ElasticsearchVersionList _


-- | <p> Status of the Elasticsearch version options for the specified Elasticsearch domain.</p>
newtype ElasticsearchVersionStatus = ElasticsearchVersionStatus 
  { "Options" :: (ElasticsearchVersionString)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeElasticsearchVersionStatus :: Newtype ElasticsearchVersionStatus _


newtype ElasticsearchVersionString = ElasticsearchVersionString String
derive instance newtypeElasticsearchVersionString :: Newtype ElasticsearchVersionString _


-- | <p>Specifies the Encryption At Rest Options.</p>
newtype EncryptionAtRestOptions = EncryptionAtRestOptions 
  { "Enabled" :: NullOrUndefined (Boolean)
  , "KmsKeyId" :: NullOrUndefined (KmsKeyId)
  }
derive instance newtypeEncryptionAtRestOptions :: Newtype EncryptionAtRestOptions _


-- | <p> Status of the Encryption At Rest options for the specified Elasticsearch domain.</p>
newtype EncryptionAtRestOptionsStatus = EncryptionAtRestOptionsStatus 
  { "Options" :: (EncryptionAtRestOptions)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeEncryptionAtRestOptionsStatus :: Newtype EncryptionAtRestOptionsStatus _


newtype EndpointsMap = EndpointsMap (Map String ServiceUrl)
derive instance newtypeEndpointsMap :: Newtype EndpointsMap _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p> InstanceCountLimits represents the limits on number of instances that be created in Amazon Elasticsearch for given InstanceType. </p>
newtype InstanceCountLimits = InstanceCountLimits 
  { "MinimumInstanceCount" :: NullOrUndefined (MinimumInstanceCount)
  , "MaximumInstanceCount" :: NullOrUndefined (MaximumInstanceCount)
  }
derive instance newtypeInstanceCountLimits :: Newtype InstanceCountLimits _


-- | <p>InstanceLimits represents the list of instance related attributes that are available for given InstanceType. </p>
newtype InstanceLimits = InstanceLimits 
  { "InstanceCountLimits" :: NullOrUndefined (InstanceCountLimits)
  }
derive instance newtypeInstanceLimits :: Newtype InstanceLimits _


newtype InstanceRole = InstanceRole String
derive instance newtypeInstanceRole :: Newtype InstanceRole _


newtype IntegerClass = IntegerClass Int
derive instance newtypeIntegerClass :: Newtype IntegerClass _


-- | <p>The request processing has failed because of an unknown error, exception or failure (the failure is internal to the service) . Gives http status code of 500.</p>
newtype InternalException = InternalException 
  { 
  }
derive instance newtypeInternalException :: Newtype InternalException _


-- | <p>An exception for trying to create or access sub-resource that is either invalid or not supported. Gives http status code of 409.</p>
newtype InvalidTypeException = InvalidTypeException 
  { 
  }
derive instance newtypeInvalidTypeException :: Newtype InvalidTypeException _


newtype KmsKeyId = KmsKeyId String
derive instance newtypeKmsKeyId :: Newtype KmsKeyId _


-- | <p>An exception for trying to create more than allowed resources or sub-resources. Gives http status code of 409.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype LimitName = LimitName String
derive instance newtypeLimitName :: Newtype LimitName _


newtype LimitValue = LimitValue String
derive instance newtypeLimitValue :: Newtype LimitValue _


newtype LimitValueList = LimitValueList (Array LimitValue)
derive instance newtypeLimitValueList :: Newtype LimitValueList _


-- | <p> Limits for given InstanceType and for each of it's role. <br/> Limits contains following <code> <a>StorageTypes,</a> </code> <code> <a>InstanceLimits</a> </code> and <code> <a>AdditionalLimits</a> </code> </p>
newtype Limits = Limits 
  { "StorageTypes" :: NullOrUndefined (StorageTypeList)
  , "InstanceLimits" :: NullOrUndefined (InstanceLimits)
  , "AdditionalLimits" :: NullOrUndefined (AdditionalLimitList)
  }
derive instance newtypeLimits :: Newtype Limits _


-- | <p> Map of Role of the Instance and Limits that are applicable. Role performed by given Instance in Elasticsearch can be one of the following: <ul> <li>Data: If the given InstanceType is used as Data node</li> <li>Master: If the given InstanceType is used as Master node</li> </ul> </p>
newtype LimitsByRole = LimitsByRole (Map InstanceRole Limits)
derive instance newtypeLimitsByRole :: Newtype LimitsByRole _


-- | <p>The result of a <code>ListDomainNames</code> operation. Contains the names of all Elasticsearch domains owned by this account.</p>
newtype ListDomainNamesResponse = ListDomainNamesResponse 
  { "DomainNames" :: NullOrUndefined (DomainInfoList)
  }
derive instance newtypeListDomainNamesResponse :: Newtype ListDomainNamesResponse _


-- | <p> Container for the parameters to the <code> <a>ListElasticsearchInstanceTypes</a> </code> operation. </p>
newtype ListElasticsearchInstanceTypesRequest = ListElasticsearchInstanceTypesRequest 
  { "ElasticsearchVersion" :: (ElasticsearchVersionString)
  , "DomainName" :: NullOrUndefined (DomainName)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListElasticsearchInstanceTypesRequest :: Newtype ListElasticsearchInstanceTypesRequest _


-- | <p> Container for the parameters returned by <code> <a>ListElasticsearchInstanceTypes</a> </code> operation. </p>
newtype ListElasticsearchInstanceTypesResponse = ListElasticsearchInstanceTypesResponse 
  { "ElasticsearchInstanceTypes" :: NullOrUndefined (ElasticsearchInstanceTypeList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListElasticsearchInstanceTypesResponse :: Newtype ListElasticsearchInstanceTypesResponse _


-- | <p> Container for the parameters to the <code> <a>ListElasticsearchVersions</a> </code> operation. <p> Use <code> <a>MaxResults</a> </code> to control the maximum number of results to retrieve in a single call. </p> <p> Use <code> <a>NextToken</a> </code> in response to retrieve more results. If the received response does not contain a NextToken, then there are no more results to retrieve. </p> </p>
newtype ListElasticsearchVersionsRequest = ListElasticsearchVersionsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListElasticsearchVersionsRequest :: Newtype ListElasticsearchVersionsRequest _


-- | <p> Container for the parameters for response received from <code> <a>ListElasticsearchVersions</a> </code> operation. </p>
newtype ListElasticsearchVersionsResponse = ListElasticsearchVersionsResponse 
  { "ElasticsearchVersions" :: NullOrUndefined (ElasticsearchVersionList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListElasticsearchVersionsResponse :: Newtype ListElasticsearchVersionsResponse _


-- | <p>Container for the parameters to the <code><a>ListTags</a></code> operation. Specify the <code>ARN</code> for the Elasticsearch domain to which the tags are attached that you want to view are attached.</p>
newtype ListTagsRequest = ListTagsRequest 
  { "ARN" :: (ARN)
  }
derive instance newtypeListTagsRequest :: Newtype ListTagsRequest _


-- | <p>The result of a <code>ListTags</code> operation. Contains tags for all requested Elasticsearch domains.</p>
newtype ListTagsResponse = ListTagsResponse 
  { "TagList" :: NullOrUndefined (TagList)
  }
derive instance newtypeListTagsResponse :: Newtype ListTagsResponse _


-- | <p>Log Publishing option that is set for given domain. <br/>Attributes and their details: <ul> <li>CloudWatchLogsLogGroupArn: ARN of the Cloudwatch log group to which log needs to be published.</li> <li>Enabled: Whether the log publishing for given log type is enabled or not</li> </ul> </p>
newtype LogPublishingOption = LogPublishingOption 
  { "CloudWatchLogsLogGroupArn" :: NullOrUndefined (CloudWatchLogsLogGroupArn)
  , "Enabled" :: NullOrUndefined (Boolean)
  }
derive instance newtypeLogPublishingOption :: Newtype LogPublishingOption _


newtype LogPublishingOptions = LogPublishingOptions (Map LogType LogPublishingOption)
derive instance newtypeLogPublishingOptions :: Newtype LogPublishingOptions _


-- | <p>The configured log publishing options for the domain and their current status.</p>
newtype LogPublishingOptionsStatus = LogPublishingOptionsStatus 
  { "Options" :: NullOrUndefined (LogPublishingOptions)
  , "Status" :: NullOrUndefined (OptionStatus)
  }
derive instance newtypeLogPublishingOptionsStatus :: Newtype LogPublishingOptionsStatus _


-- | <p>Type of Log File, it can be one of the following: <ul> <li>INDEX_SLOW_LOGS: Index slow logs contains insert requests that took more time than configured index query log threshold to execute.</li> <li>SEARCH_SLOW_LOGS: Search slow logs contains search queries that took more time than configured search query log threshold to execute.</li> </ul> </p>
newtype LogType = LogType String
derive instance newtypeLogType :: Newtype LogType _


-- | <p> Set this value to limit the number of results returned. </p>
newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | <p> Maximum number of Instances that can be instantiated for given InstanceType. </p>
newtype MaximumInstanceCount = MaximumInstanceCount Int
derive instance newtypeMaximumInstanceCount :: Newtype MaximumInstanceCount _


-- | <p> Minimum number of Instances that can be instantiated for given InstanceType. </p>
newtype MinimumInstanceCount = MinimumInstanceCount Int
derive instance newtypeMinimumInstanceCount :: Newtype MinimumInstanceCount _


-- | <p> Paginated APIs accepts NextToken input to returns next page results and provides a NextToken output in the response which can be used by the client to retrieve more results. </p>
newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>The state of a requested change. One of the following:</p> <ul> <li>Processing: The request change is still in-process.</li> <li>Active: The request change is processed and deployed to the Elasticsearch domain.</li> </ul>
newtype OptionState = OptionState String
derive instance newtypeOptionState :: Newtype OptionState _


-- | <p>Provides the current status of the entity.</p>
newtype OptionStatus = OptionStatus 
  { "CreationDate" :: (UpdateTimestamp)
  , "UpdateDate" :: (UpdateTimestamp)
  , "UpdateVersion" :: NullOrUndefined (UIntValue)
  , "State" :: (OptionState)
  , "PendingDeletion" :: NullOrUndefined (Boolean)
  }
derive instance newtypeOptionStatus :: Newtype OptionStatus _


-- | <p>Access policy rules for an Elasticsearch domain service endpoints. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-access-policies" target="_blank">Configuring Access Policies</a> in the <i>Amazon Elasticsearch Service Developer Guide</i>. The maximum size of a policy document is 100 KB.</p>
newtype PolicyDocument = PolicyDocument String
derive instance newtypePolicyDocument :: Newtype PolicyDocument _


-- | <p>Container for the parameters to the <code><a>RemoveTags</a></code> operation. Specify the <code>ARN</code> for the Elasticsearch domain from which you want to remove the specified <code>TagKey</code>.</p>
newtype RemoveTagsRequest = RemoveTagsRequest 
  { "ARN" :: (ARN)
  , "TagKeys" :: (StringList)
  }
derive instance newtypeRemoveTagsRequest :: Newtype RemoveTagsRequest _


-- | <p>An exception for creating a resource that already exists. Gives http status code of 400.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { 
  }
derive instance newtypeResourceAlreadyExistsException :: Newtype ResourceAlreadyExistsException _


-- | <p>An exception for accessing or deleting a resource that does not exist. Gives http status code of 400.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>The endpoint to which service requests are submitted. For example, <code>search-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.es.amazonaws.com</code> or <code>doc-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.es.amazonaws.com</code>.</p>
newtype ServiceUrl = ServiceUrl String
derive instance newtypeServiceUrl :: Newtype ServiceUrl _


-- | <p>Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is <code>0</code> hours.</p>
newtype SnapshotOptions = SnapshotOptions 
  { "AutomatedSnapshotStartHour" :: NullOrUndefined (IntegerClass)
  }
derive instance newtypeSnapshotOptions :: Newtype SnapshotOptions _


-- | <p>Status of a daily automated snapshot.</p>
newtype SnapshotOptionsStatus = SnapshotOptionsStatus 
  { "Options" :: (SnapshotOptions)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeSnapshotOptionsStatus :: Newtype SnapshotOptionsStatus _


-- | <p> SubType of the given storage type. List of available sub-storage options: For "instance" storageType we wont have any storageSubType, in case of "ebs" storageType we will have following valid storageSubTypes <ol> <li>standard</li> <li>gp2</li> <li>io1</li> </ol> Refer <code><a>VolumeType</a></code> for more information regarding above EBS storage options. </p>
newtype StorageSubTypeName = StorageSubTypeName String
derive instance newtypeStorageSubTypeName :: Newtype StorageSubTypeName _


-- | <p>StorageTypes represents the list of storage related types and their attributes that are available for given InstanceType. </p>
newtype StorageType = StorageType 
  { "StorageTypeName" :: NullOrUndefined (StorageTypeName)
  , "StorageSubTypeName" :: NullOrUndefined (StorageSubTypeName)
  , "StorageTypeLimits" :: NullOrUndefined (StorageTypeLimitList)
  }
derive instance newtypeStorageType :: Newtype StorageType _


-- | <p>Limits that are applicable for given storage type. </p>
newtype StorageTypeLimit = StorageTypeLimit 
  { "LimitName" :: NullOrUndefined (LimitName)
  , "LimitValues" :: NullOrUndefined (LimitValueList)
  }
derive instance newtypeStorageTypeLimit :: Newtype StorageTypeLimit _


newtype StorageTypeLimitList = StorageTypeLimitList (Array StorageTypeLimit)
derive instance newtypeStorageTypeLimitList :: Newtype StorageTypeLimitList _


newtype StorageTypeList = StorageTypeList (Array StorageType)
derive instance newtypeStorageTypeList :: Newtype StorageTypeList _


-- | <p> Type of the storage. List of available storage options: <ol> <li>instance</li> Inbuilt storage available for the given Instance <li>ebs</li> Elastic block storage that would be attached to the given Instance </ol> </p>
newtype StorageTypeName = StorageTypeName String
derive instance newtypeStorageTypeName :: Newtype StorageTypeName _


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _


-- | <p>Specifies a key value pair for a resource tag.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


-- | <p>A string of length from 1 to 128 characters that specifies the key for a Tag. Tag keys must be unique for the Elasticsearch domain to which they are attached.</p>
newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


-- | <p>A list of <code>Tag</code> </p>
newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


-- | <p>A string of length from 0 to 256 characters that specifies the value for a Tag. Tag values can be null and do not have to be unique in a tag set.</p>
newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype UIntValue = UIntValue Int
derive instance newtypeUIntValue :: Newtype UIntValue _


-- | <p>Container for the parameters to the <code><a>UpdateElasticsearchDomain</a></code> operation. Specifies the type and number of instances in the domain cluster.</p>
newtype UpdateElasticsearchDomainConfigRequest = UpdateElasticsearchDomainConfigRequest 
  { "DomainName" :: (DomainName)
  , "ElasticsearchClusterConfig" :: NullOrUndefined (ElasticsearchClusterConfig)
  , "EBSOptions" :: NullOrUndefined (EBSOptions)
  , "SnapshotOptions" :: NullOrUndefined (SnapshotOptions)
  , "VPCOptions" :: NullOrUndefined (VPCOptions)
  , "AdvancedOptions" :: NullOrUndefined (AdvancedOptions)
  , "AccessPolicies" :: NullOrUndefined (PolicyDocument)
  , "LogPublishingOptions" :: NullOrUndefined (LogPublishingOptions)
  }
derive instance newtypeUpdateElasticsearchDomainConfigRequest :: Newtype UpdateElasticsearchDomainConfigRequest _


-- | <p>The result of an <code>UpdateElasticsearchDomain</code> request. Contains the status of the Elasticsearch domain being updated.</p>
newtype UpdateElasticsearchDomainConfigResponse = UpdateElasticsearchDomainConfigResponse 
  { "DomainConfig" :: (ElasticsearchDomainConfig)
  }
derive instance newtypeUpdateElasticsearchDomainConfigResponse :: Newtype UpdateElasticsearchDomainConfigResponse _


newtype UpdateTimestamp = UpdateTimestamp Number
derive instance newtypeUpdateTimestamp :: Newtype UpdateTimestamp _


-- | <p>Options to specify the subnets and security groups for VPC endpoint. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html" target="_blank"> VPC Endpoints for Amazon Elasticsearch Service Domains</a>.</p>
newtype VPCDerivedInfo = VPCDerivedInfo 
  { "VPCId" :: NullOrUndefined (String)
  , "SubnetIds" :: NullOrUndefined (StringList)
  , "AvailabilityZones" :: NullOrUndefined (StringList)
  , "SecurityGroupIds" :: NullOrUndefined (StringList)
  }
derive instance newtypeVPCDerivedInfo :: Newtype VPCDerivedInfo _


-- | <p> Status of the VPC options for the specified Elasticsearch domain.</p>
newtype VPCDerivedInfoStatus = VPCDerivedInfoStatus 
  { "Options" :: (VPCDerivedInfo)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeVPCDerivedInfoStatus :: Newtype VPCDerivedInfoStatus _


-- | <p>Options to specify the subnets and security groups for VPC endpoint. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html" target="_blank"> VPC Endpoints for Amazon Elasticsearch Service Domains</a>.</p>
newtype VPCOptions = VPCOptions 
  { "SubnetIds" :: NullOrUndefined (StringList)
  , "SecurityGroupIds" :: NullOrUndefined (StringList)
  }
derive instance newtypeVPCOptions :: Newtype VPCOptions _


-- | <p>An exception for missing / invalid input fields. Gives http status code of 400.</p>
newtype ValidationException = ValidationException 
  { 
  }
derive instance newtypeValidationException :: Newtype ValidationException _


-- | <p> The type of EBS volume, standard, gp2, or io1. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs" target="_blank">Configuring EBS-based Storage</a>for more information.</p>
newtype VolumeType = VolumeType String
derive instance newtypeVolumeType :: Newtype VolumeType _
