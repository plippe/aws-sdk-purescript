## Module AWS.ES

<fullname>Amazon Elasticsearch Configuration Service</fullname> <p>Use the Amazon Elasticsearch configuration API to create, configure, and manage Elasticsearch domains.</p> <p>The endpoint for configuration service requests is region-specific: es.<i>region</i>.amazonaws.com. For example, es.us-east-1.amazonaws.com. For a current list of supported regions and endpoints, see <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#elasticsearch-service-regions" target="_blank">Regions and Endpoints</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addTags`

``` purescript
addTags :: forall eff. AddTagsRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Attaches tags to an existing Elasticsearch domain. Tags are a set of case-sensitive key value pairs. An Elasticsearch domain may have up to 10 tags. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-awsresorcetagging" target="_blank"> Tagging Amazon Elasticsearch Service Domains for more information.</a></p>

#### `createElasticsearchDomain`

``` purescript
createElasticsearchDomain :: forall eff. CreateElasticsearchDomainRequest -> Aff (err :: RequestError | eff) CreateElasticsearchDomainResponse
```

<p>Creates a new Elasticsearch domain. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains" target="_blank">Creating Elasticsearch Domains</a> in the <i>Amazon Elasticsearch Service Developer Guide</i>.</p>

#### `deleteElasticsearchDomain`

``` purescript
deleteElasticsearchDomain :: forall eff. DeleteElasticsearchDomainRequest -> Aff (err :: RequestError | eff) DeleteElasticsearchDomainResponse
```

<p>Permanently deletes the specified Elasticsearch domain and all of its data. Once a domain is deleted, it cannot be recovered.</p>

#### `deleteElasticsearchServiceRole`

``` purescript
deleteElasticsearchServiceRole :: forall eff. Aff (err :: RequestError | eff) Unit
```

<p>Deletes the service-linked role that Elasticsearch Service uses to manage and maintain VPC domains. Role deletion will fail if any existing VPC domains use the role. You must delete any such Elasticsearch domains before deleting the role. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-enabling-slr" target="_blank">Deleting Elasticsearch Service Role</a> in <i>VPC Endpoints for Amazon Elasticsearch Service Domains</i>.</p>

#### `describeElasticsearchDomain`

``` purescript
describeElasticsearchDomain :: forall eff. DescribeElasticsearchDomainRequest -> Aff (err :: RequestError | eff) DescribeElasticsearchDomainResponse
```

<p>Returns domain configuration information about the specified Elasticsearch domain, including the domain ID, domain endpoint, and domain ARN.</p>

#### `describeElasticsearchDomainConfig`

``` purescript
describeElasticsearchDomainConfig :: forall eff. DescribeElasticsearchDomainConfigRequest -> Aff (err :: RequestError | eff) DescribeElasticsearchDomainConfigResponse
```

<p>Provides cluster configuration information about the specified Elasticsearch domain, such as the state, creation date, update version, and update date for cluster options.</p>

#### `describeElasticsearchDomains`

``` purescript
describeElasticsearchDomains :: forall eff. DescribeElasticsearchDomainsRequest -> Aff (err :: RequestError | eff) DescribeElasticsearchDomainsResponse
```

<p>Returns domain configuration information about the specified Elasticsearch domains, including the domain ID, domain endpoint, and domain ARN.</p>

#### `describeElasticsearchInstanceTypeLimits`

``` purescript
describeElasticsearchInstanceTypeLimits :: forall eff. DescribeElasticsearchInstanceTypeLimitsRequest -> Aff (err :: RequestError | eff) DescribeElasticsearchInstanceTypeLimitsResponse
```

<p> Describe Elasticsearch Limits for a given InstanceType and ElasticsearchVersion. When modifying existing Domain, specify the <code> <a>DomainName</a> </code> to know what Limits are supported for modifying. </p>

#### `listDomainNames`

``` purescript
listDomainNames :: forall eff. Aff (err :: RequestError | eff) ListDomainNamesResponse
```

<p>Returns the name of all Elasticsearch domains owned by the current user's account. </p>

#### `listElasticsearchInstanceTypes`

``` purescript
listElasticsearchInstanceTypes :: forall eff. ListElasticsearchInstanceTypesRequest -> Aff (err :: RequestError | eff) ListElasticsearchInstanceTypesResponse
```

<p>List all Elasticsearch instance types that are supported for given ElasticsearchVersion</p>

#### `listElasticsearchVersions`

``` purescript
listElasticsearchVersions :: forall eff. ListElasticsearchVersionsRequest -> Aff (err :: RequestError | eff) ListElasticsearchVersionsResponse
```

<p>List all supported Elasticsearch versions</p>

#### `listTags`

``` purescript
listTags :: forall eff. ListTagsRequest -> Aff (err :: RequestError | eff) ListTagsResponse
```

<p>Returns all tags for the given Elasticsearch domain.</p>

#### `removeTags`

``` purescript
removeTags :: forall eff. RemoveTagsRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes the specified set of tags from the specified Elasticsearch domain.</p>

#### `updateElasticsearchDomainConfig`

``` purescript
updateElasticsearchDomainConfig :: forall eff. UpdateElasticsearchDomainConfigRequest -> Aff (err :: RequestError | eff) UpdateElasticsearchDomainConfigResponse
```

<p>Modifies the cluster configuration of the specified Elasticsearch domain, setting as setting the instance type and the number of instances. </p>

#### `ARN`

``` purescript
newtype ARN
  = ARN String
```

<p>The Amazon Resource Name (ARN) of the Elasticsearch domain. See <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html" target="_blank">Identifiers for IAM Entities</a> in <i>Using AWS Identity and Access Management</i> for more information.</p>

#### `AccessPoliciesStatus`

``` purescript
newtype AccessPoliciesStatus
  = AccessPoliciesStatus { "Options" :: PolicyDocument, "Status" :: OptionStatus }
```

<p>The configured access rules for the domain's document and search endpoints, and the current status of those rules.</p>

#### `AddTagsRequest`

``` purescript
newtype AddTagsRequest
  = AddTagsRequest { "ARN" :: ARN, "TagList" :: TagList }
```

<p>Container for the parameters to the <code><a>AddTags</a></code> operation. Specify the tags that you want to attach to the Elasticsearch domain.</p>

#### `AdditionalLimit`

``` purescript
newtype AdditionalLimit
  = AdditionalLimit { "LimitName" :: NullOrUndefined (LimitName), "LimitValues" :: NullOrUndefined (LimitValueList) }
```

<p> List of limits that are specific to a given InstanceType and for each of it's <code> <a>InstanceRole</a> </code> . </p>

#### `AdditionalLimitList`

``` purescript
newtype AdditionalLimitList
  = AdditionalLimitList (Array AdditionalLimit)
```

#### `AdvancedOptions`

``` purescript
newtype AdvancedOptions
  = AdvancedOptions (Map String String)
```

<p> Exposes select native Elasticsearch configuration values from <code>elasticsearch.yml</code>. Currently, the following advanced options are available:</p> <ul> <li>Option to allow references to indices in an HTTP request body. Must be <code>false</code> when configuring access to individual sub-resources. By default, the value is <code>true</code>. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options" target="_blank">Configuration Advanced Options</a> for more information.</li> <li>Option to specify the percentage of heap space that is allocated to field data. By default, this setting is unbounded.</li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options">Configuring Advanced Options</a>.</p>

#### `AdvancedOptionsStatus`

``` purescript
newtype AdvancedOptionsStatus
  = AdvancedOptionsStatus { "Options" :: AdvancedOptions, "Status" :: OptionStatus }
```

<p> Status of the advanced options for the specified Elasticsearch domain. Currently, the following advanced options are available:</p> <ul> <li>Option to allow references to indices in an HTTP request body. Must be <code>false</code> when configuring access to individual sub-resources. By default, the value is <code>true</code>. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options" target="_blank">Configuration Advanced Options</a> for more information.</li> <li>Option to specify the percentage of heap space that is allocated to field data. By default, this setting is unbounded.</li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options">Configuring Advanced Options</a>.</p>

#### `BaseException`

``` purescript
newtype BaseException
  = BaseException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>An error occurred while processing the request.</p>

#### `CloudWatchLogsLogGroupArn`

``` purescript
newtype CloudWatchLogsLogGroupArn
  = CloudWatchLogsLogGroupArn String
```

<p>ARN of the Cloudwatch log group to which log needs to be published.</p>

#### `CreateElasticsearchDomainRequest`

``` purescript
newtype CreateElasticsearchDomainRequest
  = CreateElasticsearchDomainRequest { "DomainName" :: DomainName, "ElasticsearchVersion" :: NullOrUndefined (ElasticsearchVersionString), "ElasticsearchClusterConfig" :: NullOrUndefined (ElasticsearchClusterConfig), "EBSOptions" :: NullOrUndefined (EBSOptions), "AccessPolicies" :: NullOrUndefined (PolicyDocument), "SnapshotOptions" :: NullOrUndefined (SnapshotOptions), "VPCOptions" :: NullOrUndefined (VPCOptions), "EncryptionAtRestOptions" :: NullOrUndefined (EncryptionAtRestOptions), "AdvancedOptions" :: NullOrUndefined (AdvancedOptions), "LogPublishingOptions" :: NullOrUndefined (LogPublishingOptions) }
```

#### `CreateElasticsearchDomainResponse`

``` purescript
newtype CreateElasticsearchDomainResponse
  = CreateElasticsearchDomainResponse { "DomainStatus" :: NullOrUndefined (ElasticsearchDomainStatus) }
```

<p>The result of a <code>CreateElasticsearchDomain</code> operation. Contains the status of the newly created Elasticsearch domain.</p>

#### `DeleteElasticsearchDomainRequest`

``` purescript
newtype DeleteElasticsearchDomainRequest
  = DeleteElasticsearchDomainRequest { "DomainName" :: DomainName }
```

<p>Container for the parameters to the <code><a>DeleteElasticsearchDomain</a></code> operation. Specifies the name of the Elasticsearch domain that you want to delete.</p>

#### `DeleteElasticsearchDomainResponse`

``` purescript
newtype DeleteElasticsearchDomainResponse
  = DeleteElasticsearchDomainResponse { "DomainStatus" :: NullOrUndefined (ElasticsearchDomainStatus) }
```

<p>The result of a <code>DeleteElasticsearchDomain</code> request. Contains the status of the pending deletion, or no status if the domain and all of its resources have been deleted.</p>

#### `DescribeElasticsearchDomainConfigRequest`

``` purescript
newtype DescribeElasticsearchDomainConfigRequest
  = DescribeElasticsearchDomainConfigRequest { "DomainName" :: DomainName }
```

<p> Container for the parameters to the <code>DescribeElasticsearchDomainConfig</code> operation. Specifies the domain name for which you want configuration information.</p>

#### `DescribeElasticsearchDomainConfigResponse`

``` purescript
newtype DescribeElasticsearchDomainConfigResponse
  = DescribeElasticsearchDomainConfigResponse { "DomainConfig" :: ElasticsearchDomainConfig }
```

<p>The result of a <code>DescribeElasticsearchDomainConfig</code> request. Contains the configuration information of the requested domain.</p>

#### `DescribeElasticsearchDomainRequest`

``` purescript
newtype DescribeElasticsearchDomainRequest
  = DescribeElasticsearchDomainRequest { "DomainName" :: DomainName }
```

<p>Container for the parameters to the <code><a>DescribeElasticsearchDomain</a></code> operation.</p>

#### `DescribeElasticsearchDomainResponse`

``` purescript
newtype DescribeElasticsearchDomainResponse
  = DescribeElasticsearchDomainResponse { "DomainStatus" :: ElasticsearchDomainStatus }
```

<p>The result of a <code>DescribeElasticsearchDomain</code> request. Contains the status of the domain specified in the request.</p>

#### `DescribeElasticsearchDomainsRequest`

``` purescript
newtype DescribeElasticsearchDomainsRequest
  = DescribeElasticsearchDomainsRequest { "DomainNames" :: DomainNameList }
```

<p>Container for the parameters to the <code><a>DescribeElasticsearchDomains</a></code> operation. By default, the API returns the status of all Elasticsearch domains.</p>

#### `DescribeElasticsearchDomainsResponse`

``` purescript
newtype DescribeElasticsearchDomainsResponse
  = DescribeElasticsearchDomainsResponse { "DomainStatusList" :: ElasticsearchDomainStatusList }
```

<p>The result of a <code>DescribeElasticsearchDomains</code> request. Contains the status of the specified domains or all domains owned by the account.</p>

#### `DescribeElasticsearchInstanceTypeLimitsRequest`

``` purescript
newtype DescribeElasticsearchInstanceTypeLimitsRequest
  = DescribeElasticsearchInstanceTypeLimitsRequest { "DomainName" :: NullOrUndefined (DomainName), "InstanceType" :: ESPartitionInstanceType, "ElasticsearchVersion" :: ElasticsearchVersionString }
```

<p> Container for the parameters to <code> <a>DescribeElasticsearchInstanceTypeLimits</a> </code> operation. </p>

#### `DescribeElasticsearchInstanceTypeLimitsResponse`

``` purescript
newtype DescribeElasticsearchInstanceTypeLimitsResponse
  = DescribeElasticsearchInstanceTypeLimitsResponse { "LimitsByRole" :: NullOrUndefined (LimitsByRole) }
```

<p> Container for the parameters received from <code> <a>DescribeElasticsearchInstanceTypeLimits</a> </code> operation. </p>

#### `DisabledOperationException`

``` purescript
newtype DisabledOperationException
  = DisabledOperationException {  }
```

<p>An error occured because the client wanted to access a not supported operation. Gives http status code of 409.</p>

#### `DomainId`

``` purescript
newtype DomainId
  = DomainId String
```

<p>Unique identifier for an Elasticsearch domain.</p>

#### `DomainInfo`

``` purescript
newtype DomainInfo
  = DomainInfo { "DomainName" :: NullOrUndefined (DomainName) }
```

#### `DomainInfoList`

``` purescript
newtype DomainInfoList
  = DomainInfoList (Array DomainInfo)
```

<p> Contains the list of Elasticsearch domain information.</p>

#### `DomainName`

``` purescript
newtype DomainName
  = DomainName String
```

<p>The name of an Elasticsearch domain. Domain names are unique across the domains owned by an account within an AWS region. Domain names start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).</p>

#### `DomainNameList`

``` purescript
newtype DomainNameList
  = DomainNameList (Array DomainName)
```

<p>A list of Elasticsearch domain names.</p>

#### `EBSOptions`

``` purescript
newtype EBSOptions
  = EBSOptions { "EBSEnabled" :: NullOrUndefined (Boolean), "VolumeType" :: NullOrUndefined (VolumeType), "VolumeSize" :: NullOrUndefined (IntegerClass), "Iops" :: NullOrUndefined (IntegerClass) }
```

<p>Options to enable, disable, and specify the properties of EBS storage volumes. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs" target="_blank"> Configuring EBS-based Storage</a>.</p>

#### `EBSOptionsStatus`

``` purescript
newtype EBSOptionsStatus
  = EBSOptionsStatus { "Options" :: EBSOptions, "Status" :: OptionStatus }
```

<p> Status of the EBS options for the specified Elasticsearch domain.</p>

#### `ESPartitionInstanceType`

``` purescript
newtype ESPartitionInstanceType
  = ESPartitionInstanceType String
```

#### `ElasticsearchClusterConfig`

``` purescript
newtype ElasticsearchClusterConfig
  = ElasticsearchClusterConfig { "InstanceType" :: NullOrUndefined (ESPartitionInstanceType), "InstanceCount" :: NullOrUndefined (IntegerClass), "DedicatedMasterEnabled" :: NullOrUndefined (Boolean), "ZoneAwarenessEnabled" :: NullOrUndefined (Boolean), "DedicatedMasterType" :: NullOrUndefined (ESPartitionInstanceType), "DedicatedMasterCount" :: NullOrUndefined (IntegerClass) }
```

<p>Specifies the configuration for the domain cluster, such as the type and number of instances.</p>

#### `ElasticsearchClusterConfigStatus`

``` purescript
newtype ElasticsearchClusterConfigStatus
  = ElasticsearchClusterConfigStatus { "Options" :: ElasticsearchClusterConfig, "Status" :: OptionStatus }
```

<p> Specifies the configuration status for the specified Elasticsearch domain.</p>

#### `ElasticsearchDomainConfig`

``` purescript
newtype ElasticsearchDomainConfig
  = ElasticsearchDomainConfig { "ElasticsearchVersion" :: NullOrUndefined (ElasticsearchVersionStatus), "ElasticsearchClusterConfig" :: NullOrUndefined (ElasticsearchClusterConfigStatus), "EBSOptions" :: NullOrUndefined (EBSOptionsStatus), "AccessPolicies" :: NullOrUndefined (AccessPoliciesStatus), "SnapshotOptions" :: NullOrUndefined (SnapshotOptionsStatus), "VPCOptions" :: NullOrUndefined (VPCDerivedInfoStatus), "EncryptionAtRestOptions" :: NullOrUndefined (EncryptionAtRestOptionsStatus), "AdvancedOptions" :: NullOrUndefined (AdvancedOptionsStatus), "LogPublishingOptions" :: NullOrUndefined (LogPublishingOptionsStatus) }
```

<p>The configuration of an Elasticsearch domain.</p>

#### `ElasticsearchDomainStatus`

``` purescript
newtype ElasticsearchDomainStatus
  = ElasticsearchDomainStatus { "DomainId" :: DomainId, "DomainName" :: DomainName, "ARN" :: ARN, "Created" :: NullOrUndefined (Boolean), "Deleted" :: NullOrUndefined (Boolean), "Endpoint" :: NullOrUndefined (ServiceUrl), "Endpoints" :: NullOrUndefined (EndpointsMap), "Processing" :: NullOrUndefined (Boolean), "ElasticsearchVersion" :: NullOrUndefined (ElasticsearchVersionString), "ElasticsearchClusterConfig" :: ElasticsearchClusterConfig, "EBSOptions" :: NullOrUndefined (EBSOptions), "AccessPolicies" :: NullOrUndefined (PolicyDocument), "SnapshotOptions" :: NullOrUndefined (SnapshotOptions), "VPCOptions" :: NullOrUndefined (VPCDerivedInfo), "EncryptionAtRestOptions" :: NullOrUndefined (EncryptionAtRestOptions), "AdvancedOptions" :: NullOrUndefined (AdvancedOptions), "LogPublishingOptions" :: NullOrUndefined (LogPublishingOptions) }
```

<p>The current status of an Elasticsearch domain.</p>

#### `ElasticsearchDomainStatusList`

``` purescript
newtype ElasticsearchDomainStatusList
  = ElasticsearchDomainStatusList (Array ElasticsearchDomainStatus)
```

<p>A list that contains the status of each requested Elasticsearch domain.</p>

#### `ElasticsearchInstanceTypeList`

``` purescript
newtype ElasticsearchInstanceTypeList
  = ElasticsearchInstanceTypeList (Array ESPartitionInstanceType)
```

<p> List of instance types supported by Amazon Elasticsearch service. </p>

#### `ElasticsearchVersionList`

``` purescript
newtype ElasticsearchVersionList
  = ElasticsearchVersionList (Array ElasticsearchVersionString)
```

<p>List of supported elastic search versions. </p>

#### `ElasticsearchVersionStatus`

``` purescript
newtype ElasticsearchVersionStatus
  = ElasticsearchVersionStatus { "Options" :: ElasticsearchVersionString, "Status" :: OptionStatus }
```

<p> Status of the Elasticsearch version options for the specified Elasticsearch domain.</p>

#### `ElasticsearchVersionString`

``` purescript
newtype ElasticsearchVersionString
  = ElasticsearchVersionString String
```

#### `EncryptionAtRestOptions`

``` purescript
newtype EncryptionAtRestOptions
  = EncryptionAtRestOptions { "Enabled" :: NullOrUndefined (Boolean), "KmsKeyId" :: NullOrUndefined (KmsKeyId) }
```

<p>Specifies the Encryption At Rest Options.</p>

#### `EncryptionAtRestOptionsStatus`

``` purescript
newtype EncryptionAtRestOptionsStatus
  = EncryptionAtRestOptionsStatus { "Options" :: EncryptionAtRestOptions, "Status" :: OptionStatus }
```

<p> Status of the Encryption At Rest options for the specified Elasticsearch domain.</p>

#### `EndpointsMap`

``` purescript
newtype EndpointsMap
  = EndpointsMap (Map String ServiceUrl)
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `InstanceCountLimits`

``` purescript
newtype InstanceCountLimits
  = InstanceCountLimits { "MinimumInstanceCount" :: NullOrUndefined (MinimumInstanceCount), "MaximumInstanceCount" :: NullOrUndefined (MaximumInstanceCount) }
```

<p> InstanceCountLimits represents the limits on number of instances that be created in Amazon Elasticsearch for given InstanceType. </p>

#### `InstanceLimits`

``` purescript
newtype InstanceLimits
  = InstanceLimits { "InstanceCountLimits" :: NullOrUndefined (InstanceCountLimits) }
```

<p>InstanceLimits represents the list of instance related attributes that are available for given InstanceType. </p>

#### `InstanceRole`

``` purescript
newtype InstanceRole
  = InstanceRole String
```

#### `IntegerClass`

``` purescript
newtype IntegerClass
  = IntegerClass Int
```

#### `InternalException`

``` purescript
newtype InternalException
  = InternalException {  }
```

<p>The request processing has failed because of an unknown error, exception or failure (the failure is internal to the service) . Gives http status code of 500.</p>

#### `InvalidTypeException`

``` purescript
newtype InvalidTypeException
  = InvalidTypeException {  }
```

<p>An exception for trying to create or access sub-resource that is either invalid or not supported. Gives http status code of 409.</p>

#### `KmsKeyId`

``` purescript
newtype KmsKeyId
  = KmsKeyId String
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>An exception for trying to create more than allowed resources or sub-resources. Gives http status code of 409.</p>

#### `LimitName`

``` purescript
newtype LimitName
  = LimitName String
```

#### `LimitValue`

``` purescript
newtype LimitValue
  = LimitValue String
```

#### `LimitValueList`

``` purescript
newtype LimitValueList
  = LimitValueList (Array LimitValue)
```

#### `Limits`

``` purescript
newtype Limits
  = Limits { "StorageTypes" :: NullOrUndefined (StorageTypeList), "InstanceLimits" :: NullOrUndefined (InstanceLimits), "AdditionalLimits" :: NullOrUndefined (AdditionalLimitList) }
```

<p> Limits for given InstanceType and for each of it's role. <br/> Limits contains following <code> <a>StorageTypes,</a> </code> <code> <a>InstanceLimits</a> </code> and <code> <a>AdditionalLimits</a> </code> </p>

#### `LimitsByRole`

``` purescript
newtype LimitsByRole
  = LimitsByRole (Map InstanceRole Limits)
```

<p> Map of Role of the Instance and Limits that are applicable. Role performed by given Instance in Elasticsearch can be one of the following: <ul> <li>Data: If the given InstanceType is used as Data node</li> <li>Master: If the given InstanceType is used as Master node</li> </ul> </p>

#### `ListDomainNamesResponse`

``` purescript
newtype ListDomainNamesResponse
  = ListDomainNamesResponse { "DomainNames" :: NullOrUndefined (DomainInfoList) }
```

<p>The result of a <code>ListDomainNames</code> operation. Contains the names of all Elasticsearch domains owned by this account.</p>

#### `ListElasticsearchInstanceTypesRequest`

``` purescript
newtype ListElasticsearchInstanceTypesRequest
  = ListElasticsearchInstanceTypesRequest { "ElasticsearchVersion" :: ElasticsearchVersionString, "DomainName" :: NullOrUndefined (DomainName), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

<p> Container for the parameters to the <code> <a>ListElasticsearchInstanceTypes</a> </code> operation. </p>

#### `ListElasticsearchInstanceTypesResponse`

``` purescript
newtype ListElasticsearchInstanceTypesResponse
  = ListElasticsearchInstanceTypesResponse { "ElasticsearchInstanceTypes" :: NullOrUndefined (ElasticsearchInstanceTypeList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p> Container for the parameters returned by <code> <a>ListElasticsearchInstanceTypes</a> </code> operation. </p>

#### `ListElasticsearchVersionsRequest`

``` purescript
newtype ListElasticsearchVersionsRequest
  = ListElasticsearchVersionsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

<p> Container for the parameters to the <code> <a>ListElasticsearchVersions</a> </code> operation. <p> Use <code> <a>MaxResults</a> </code> to control the maximum number of results to retrieve in a single call. </p> <p> Use <code> <a>NextToken</a> </code> in response to retrieve more results. If the received response does not contain a NextToken, then there are no more results to retrieve. </p> </p>

#### `ListElasticsearchVersionsResponse`

``` purescript
newtype ListElasticsearchVersionsResponse
  = ListElasticsearchVersionsResponse { "ElasticsearchVersions" :: NullOrUndefined (ElasticsearchVersionList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p> Container for the parameters for response received from <code> <a>ListElasticsearchVersions</a> </code> operation. </p>

#### `ListTagsRequest`

``` purescript
newtype ListTagsRequest
  = ListTagsRequest { "ARN" :: ARN }
```

<p>Container for the parameters to the <code><a>ListTags</a></code> operation. Specify the <code>ARN</code> for the Elasticsearch domain to which the tags are attached that you want to view are attached.</p>

#### `ListTagsResponse`

``` purescript
newtype ListTagsResponse
  = ListTagsResponse { "TagList" :: NullOrUndefined (TagList) }
```

<p>The result of a <code>ListTags</code> operation. Contains tags for all requested Elasticsearch domains.</p>

#### `LogPublishingOption`

``` purescript
newtype LogPublishingOption
  = LogPublishingOption { "CloudWatchLogsLogGroupArn" :: NullOrUndefined (CloudWatchLogsLogGroupArn), "Enabled" :: NullOrUndefined (Boolean) }
```

<p>Log Publishing option that is set for given domain. <br/>Attributes and their details: <ul> <li>CloudWatchLogsLogGroupArn: ARN of the Cloudwatch log group to which log needs to be published.</li> <li>Enabled: Whether the log publishing for given log type is enabled or not</li> </ul> </p>

#### `LogPublishingOptions`

``` purescript
newtype LogPublishingOptions
  = LogPublishingOptions (Map LogType LogPublishingOption)
```

#### `LogPublishingOptionsStatus`

``` purescript
newtype LogPublishingOptionsStatus
  = LogPublishingOptionsStatus { "Options" :: NullOrUndefined (LogPublishingOptions), "Status" :: NullOrUndefined (OptionStatus) }
```

<p>The configured log publishing options for the domain and their current status.</p>

#### `LogType`

``` purescript
newtype LogType
  = LogType String
```

<p>Type of Log File, it can be one of the following: <ul> <li>INDEX_SLOW_LOGS: Index slow logs contains insert requests that took more time than configured index query log threshold to execute.</li> <li>SEARCH_SLOW_LOGS: Search slow logs contains search queries that took more time than configured search query log threshold to execute.</li> </ul> </p>

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

<p> Set this value to limit the number of results returned. </p>

#### `MaximumInstanceCount`

``` purescript
newtype MaximumInstanceCount
  = MaximumInstanceCount Int
```

<p> Maximum number of Instances that can be instantiated for given InstanceType. </p>

#### `MinimumInstanceCount`

``` purescript
newtype MinimumInstanceCount
  = MinimumInstanceCount Int
```

<p> Minimum number of Instances that can be instantiated for given InstanceType. </p>

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

<p> Paginated APIs accepts NextToken input to returns next page results and provides a NextToken output in the response which can be used by the client to retrieve more results. </p>

#### `OptionState`

``` purescript
newtype OptionState
  = OptionState String
```

<p>The state of a requested change. One of the following:</p> <ul> <li>Processing: The request change is still in-process.</li> <li>Active: The request change is processed and deployed to the Elasticsearch domain.</li> </ul>

#### `OptionStatus`

``` purescript
newtype OptionStatus
  = OptionStatus { "CreationDate" :: UpdateTimestamp, "UpdateDate" :: UpdateTimestamp, "UpdateVersion" :: NullOrUndefined (UIntValue), "State" :: OptionState, "PendingDeletion" :: NullOrUndefined (Boolean) }
```

<p>Provides the current status of the entity.</p>

#### `PolicyDocument`

``` purescript
newtype PolicyDocument
  = PolicyDocument String
```

<p>Access policy rules for an Elasticsearch domain service endpoints. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-access-policies" target="_blank">Configuring Access Policies</a> in the <i>Amazon Elasticsearch Service Developer Guide</i>. The maximum size of a policy document is 100 KB.</p>

#### `RemoveTagsRequest`

``` purescript
newtype RemoveTagsRequest
  = RemoveTagsRequest { "ARN" :: ARN, "TagKeys" :: StringList }
```

<p>Container for the parameters to the <code><a>RemoveTags</a></code> operation. Specify the <code>ARN</code> for the Elasticsearch domain from which you want to remove the specified <code>TagKey</code>.</p>

#### `ResourceAlreadyExistsException`

``` purescript
newtype ResourceAlreadyExistsException
  = ResourceAlreadyExistsException {  }
```

<p>An exception for creating a resource that already exists. Gives http status code of 400.</p>

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException {  }
```

<p>An exception for accessing or deleting a resource that does not exist. Gives http status code of 400.</p>

#### `ServiceUrl`

``` purescript
newtype ServiceUrl
  = ServiceUrl String
```

<p>The endpoint to which service requests are submitted. For example, <code>search-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.es.amazonaws.com</code> or <code>doc-imdb-movies-oopcnjfn6ugofer3zx5iadxxca.eu-west-1.es.amazonaws.com</code>.</p>

#### `SnapshotOptions`

``` purescript
newtype SnapshotOptions
  = SnapshotOptions { "AutomatedSnapshotStartHour" :: NullOrUndefined (IntegerClass) }
```

<p>Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is <code>0</code> hours.</p>

#### `SnapshotOptionsStatus`

``` purescript
newtype SnapshotOptionsStatus
  = SnapshotOptionsStatus { "Options" :: SnapshotOptions, "Status" :: OptionStatus }
```

<p>Status of a daily automated snapshot.</p>

#### `StorageSubTypeName`

``` purescript
newtype StorageSubTypeName
  = StorageSubTypeName String
```

<p> SubType of the given storage type. List of available sub-storage options: For "instance" storageType we wont have any storageSubType, in case of "ebs" storageType we will have following valid storageSubTypes <ol> <li>standard</li> <li>gp2</li> <li>io1</li> </ol> Refer <code><a>VolumeType</a></code> for more information regarding above EBS storage options. </p>

#### `StorageType`

``` purescript
newtype StorageType
  = StorageType { "StorageTypeName" :: NullOrUndefined (StorageTypeName), "StorageSubTypeName" :: NullOrUndefined (StorageSubTypeName), "StorageTypeLimits" :: NullOrUndefined (StorageTypeLimitList) }
```

<p>StorageTypes represents the list of storage related types and their attributes that are available for given InstanceType. </p>

#### `StorageTypeLimit`

``` purescript
newtype StorageTypeLimit
  = StorageTypeLimit { "LimitName" :: NullOrUndefined (LimitName), "LimitValues" :: NullOrUndefined (LimitValueList) }
```

<p>Limits that are applicable for given storage type. </p>

#### `StorageTypeLimitList`

``` purescript
newtype StorageTypeLimitList
  = StorageTypeLimitList (Array StorageTypeLimit)
```

#### `StorageTypeList`

``` purescript
newtype StorageTypeList
  = StorageTypeList (Array StorageType)
```

#### `StorageTypeName`

``` purescript
newtype StorageTypeName
  = StorageTypeName String
```

<p> Type of the storage. List of available storage options: <ol> <li>instance</li> Inbuilt storage available for the given Instance <li>ebs</li> Elastic block storage that would be attached to the given Instance </ol> </p>

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array String)
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: TagValue }
```

<p>Specifies a key value pair for a resource tag.</p>

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

<p>A string of length from 1 to 128 characters that specifies the key for a Tag. Tag keys must be unique for the Elasticsearch domain to which they are attached.</p>

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

<p>A list of <code>Tag</code> </p>

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

<p>A string of length from 0 to 256 characters that specifies the value for a Tag. Tag values can be null and do not have to be unique in a tag set.</p>

#### `UIntValue`

``` purescript
newtype UIntValue
  = UIntValue Int
```

#### `UpdateElasticsearchDomainConfigRequest`

``` purescript
newtype UpdateElasticsearchDomainConfigRequest
  = UpdateElasticsearchDomainConfigRequest { "DomainName" :: DomainName, "ElasticsearchClusterConfig" :: NullOrUndefined (ElasticsearchClusterConfig), "EBSOptions" :: NullOrUndefined (EBSOptions), "SnapshotOptions" :: NullOrUndefined (SnapshotOptions), "VPCOptions" :: NullOrUndefined (VPCOptions), "AdvancedOptions" :: NullOrUndefined (AdvancedOptions), "AccessPolicies" :: NullOrUndefined (PolicyDocument), "LogPublishingOptions" :: NullOrUndefined (LogPublishingOptions) }
```

<p>Container for the parameters to the <code><a>UpdateElasticsearchDomain</a></code> operation. Specifies the type and number of instances in the domain cluster.</p>

#### `UpdateElasticsearchDomainConfigResponse`

``` purescript
newtype UpdateElasticsearchDomainConfigResponse
  = UpdateElasticsearchDomainConfigResponse { "DomainConfig" :: ElasticsearchDomainConfig }
```

<p>The result of an <code>UpdateElasticsearchDomain</code> request. Contains the status of the Elasticsearch domain being updated.</p>

#### `UpdateTimestamp`

``` purescript
newtype UpdateTimestamp
  = UpdateTimestamp Number
```

#### `VPCDerivedInfo`

``` purescript
newtype VPCDerivedInfo
  = VPCDerivedInfo { "VPCId" :: NullOrUndefined (String), "SubnetIds" :: NullOrUndefined (StringList), "AvailabilityZones" :: NullOrUndefined (StringList), "SecurityGroupIds" :: NullOrUndefined (StringList) }
```

<p>Options to specify the subnets and security groups for VPC endpoint. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html" target="_blank"> VPC Endpoints for Amazon Elasticsearch Service Domains</a>.</p>

#### `VPCDerivedInfoStatus`

``` purescript
newtype VPCDerivedInfoStatus
  = VPCDerivedInfoStatus { "Options" :: VPCDerivedInfo, "Status" :: OptionStatus }
```

<p> Status of the VPC options for the specified Elasticsearch domain.</p>

#### `VPCOptions`

``` purescript
newtype VPCOptions
  = VPCOptions { "SubnetIds" :: NullOrUndefined (StringList), "SecurityGroupIds" :: NullOrUndefined (StringList) }
```

<p>Options to specify the subnets and security groups for VPC endpoint. For more information, see <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html" target="_blank"> VPC Endpoints for Amazon Elasticsearch Service Domains</a>.</p>

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException {  }
```

<p>An exception for missing / invalid input fields. Gives http status code of 400.</p>

#### `VolumeType`

``` purescript
newtype VolumeType
  = VolumeType String
```

<p> The type of EBS volume, standard, gp2, or io1. See <a href="http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs" target="_blank">Configuring EBS-based Storage</a>for more information.</p>


