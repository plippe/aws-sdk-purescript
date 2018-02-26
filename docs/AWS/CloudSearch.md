## Module AWS.CloudSearch

<fullname>Amazon CloudSearch Configuration Service</fullname> <p>You use the configuration service to create, configure, and manage search domains. Configuration service requests are submitted using the AWS Query protocol. AWS Query requests are HTTP or HTTPS requests submitted via HTTP GET or POST with a query parameter named Action.</p> <p>The endpoint for configuration service requests is region-specific: cloudsearch.<i>region</i>.amazonaws.com. For example, cloudsearch.us-east-1.amazonaws.com. For a current list of supported regions and endpoints, see <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#cloudsearch_region">Regions and Endpoints</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createDomain`

``` purescript
createDomain :: forall eff. CreateDomainRequest -> Aff (err :: RequestError | eff) CreateDomainResponse
```

<p>Creates a new search domain.</p>

#### `defineIndexField`

``` purescript
defineIndexField :: forall eff. DefineIndexFieldRequest -> Aff (err :: RequestError | eff) DefineIndexFieldResponse
```

<p>Configures an <code>IndexField</code> for the search domain. Used to create new fields and modify existing ones. If the field exists, the new configuration replaces the old one. You can configure a maximum of 200 index fields.</p>

#### `defineRankExpression`

``` purescript
defineRankExpression :: forall eff. DefineRankExpressionRequest -> Aff (err :: RequestError | eff) DefineRankExpressionResponse
```

<p>Configures a <code>RankExpression</code> for the search domain. Used to create new rank expressions and modify existing ones. If the expression exists, the new configuration replaces the old one. You can configure a maximum of 50 rank expressions.</p>

#### `deleteDomain`

``` purescript
deleteDomain :: forall eff. DeleteDomainRequest -> Aff (err :: RequestError | eff) DeleteDomainResponse
```

<p>Permanently deletes a search domain and all of its data.</p>

#### `deleteIndexField`

``` purescript
deleteIndexField :: forall eff. DeleteIndexFieldRequest -> Aff (err :: RequestError | eff) DeleteIndexFieldResponse
```

<p>Removes an <code>IndexField</code> from the search domain.</p>

#### `deleteRankExpression`

``` purescript
deleteRankExpression :: forall eff. DeleteRankExpressionRequest -> Aff (err :: RequestError | eff) DeleteRankExpressionResponse
```

<p>Removes a <code>RankExpression</code> from the search domain.</p>

#### `describeAvailabilityOptions`

``` purescript
describeAvailabilityOptions :: forall eff. DescribeAvailabilityOptionsRequest -> Aff (err :: RequestError | eff) DescribeAvailabilityOptionsResponse
```

<p>Gets the availability options configured for a domain. By default, shows the configuration with any pending changes. Set the <code>Deployed</code> option to <code>true</code> to show the active configuration and exclude pending changes. For more information, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html" target="_blank">Configuring Availability Options</a> in the <i>Amazon CloudSearch Developer Guide</i>.</p>

#### `describeDefaultSearchField`

``` purescript
describeDefaultSearchField :: forall eff. DescribeDefaultSearchFieldRequest -> Aff (err :: RequestError | eff) DescribeDefaultSearchFieldResponse
```

<p>Gets the default search field configured for the search domain.</p>

#### `describeDomains`

``` purescript
describeDomains :: forall eff. DescribeDomainsRequest -> Aff (err :: RequestError | eff) DescribeDomainsResponse
```

<p>Gets information about the search domains owned by this account. Can be limited to specific domains. Shows all domains by default.</p>

#### `describeIndexFields`

``` purescript
describeIndexFields :: forall eff. DescribeIndexFieldsRequest -> Aff (err :: RequestError | eff) DescribeIndexFieldsResponse
```

<p>Gets information about the index fields configured for the search domain. Can be limited to specific fields by name. Shows all fields by default.</p>

#### `describeRankExpressions`

``` purescript
describeRankExpressions :: forall eff. DescribeRankExpressionsRequest -> Aff (err :: RequestError | eff) DescribeRankExpressionsResponse
```

<p>Gets the rank expressions configured for the search domain. Can be limited to specific rank expressions by name. Shows all rank expressions by default. </p>

#### `describeServiceAccessPolicies`

``` purescript
describeServiceAccessPolicies :: forall eff. DescribeServiceAccessPoliciesRequest -> Aff (err :: RequestError | eff) DescribeServiceAccessPoliciesResponse
```

<p>Gets information about the resource-based policies that control access to the domain's document and search services.</p>

#### `describeStemmingOptions`

``` purescript
describeStemmingOptions :: forall eff. DescribeStemmingOptionsRequest -> Aff (err :: RequestError | eff) DescribeStemmingOptionsResponse
```

<p>Gets the stemming dictionary configured for the search domain.</p>

#### `describeStopwordOptions`

``` purescript
describeStopwordOptions :: forall eff. DescribeStopwordOptionsRequest -> Aff (err :: RequestError | eff) DescribeStopwordOptionsResponse
```

<p>Gets the stopwords configured for the search domain.</p>

#### `describeSynonymOptions`

``` purescript
describeSynonymOptions :: forall eff. DescribeSynonymOptionsRequest -> Aff (err :: RequestError | eff) DescribeSynonymOptionsResponse
```

<p>Gets the synonym dictionary configured for the search domain.</p>

#### `indexDocuments`

``` purescript
indexDocuments :: forall eff. IndexDocumentsRequest -> Aff (err :: RequestError | eff) IndexDocumentsResponse
```

<p>Tells the search domain to start indexing its documents using the latest text processing options and <code>IndexFields</code>. This operation must be invoked to make options whose <a>OptionStatus</a> has <code>OptionState</code> of <code>RequiresIndexDocuments</code> visible in search results.</p>

#### `updateAvailabilityOptions`

``` purescript
updateAvailabilityOptions :: forall eff. UpdateAvailabilityOptionsRequest -> Aff (err :: RequestError | eff) UpdateAvailabilityOptionsResponse
```

<p>Configures the availability options for a domain. Enabling the Multi-AZ option expands an Amazon CloudSearch domain to an additional Availability Zone in the same Region to increase fault tolerance in the event of a service disruption. Changes to the Multi-AZ option can take about half an hour to become active. For more information, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html" target="_blank">Configuring Availability Options</a> in the <i>Amazon CloudSearch Developer Guide</i>.</p>

#### `updateDefaultSearchField`

``` purescript
updateDefaultSearchField :: forall eff. UpdateDefaultSearchFieldRequest -> Aff (err :: RequestError | eff) UpdateDefaultSearchFieldResponse
```

<p>Configures the default search field for the search domain. The default search field is the text field that is searched when a search request does not specify which fields to search. By default, it is configured to include the contents of all of the domain's text fields. </p>

#### `updateServiceAccessPolicies`

``` purescript
updateServiceAccessPolicies :: forall eff. UpdateServiceAccessPoliciesRequest -> Aff (err :: RequestError | eff) UpdateServiceAccessPoliciesResponse
```

<p>Configures the policies that control access to the domain's document and search services. The maximum size of an access policy document is 100 KB.</p>

#### `updateStemmingOptions`

``` purescript
updateStemmingOptions :: forall eff. UpdateStemmingOptionsRequest -> Aff (err :: RequestError | eff) UpdateStemmingOptionsResponse
```

<p>Configures a stemming dictionary for the search domain. The stemming dictionary is used during indexing and when processing search requests. The maximum size of the stemming dictionary is 500 KB.</p>

#### `updateStopwordOptions`

``` purescript
updateStopwordOptions :: forall eff. UpdateStopwordOptionsRequest -> Aff (err :: RequestError | eff) UpdateStopwordOptionsResponse
```

<p>Configures stopwords for the search domain. Stopwords are used during indexing and when processing search requests. The maximum size of the stopwords dictionary is 10 KB.</p>

#### `updateSynonymOptions`

``` purescript
updateSynonymOptions :: forall eff. UpdateSynonymOptionsRequest -> Aff (err :: RequestError | eff) UpdateSynonymOptionsResponse
```

<p>Configures a synonym dictionary for the search domain. The synonym dictionary is used during indexing to configure mappings for terms that occur in text fields. The maximum size of the synonym dictionary is 100 KB. </p>

#### `AccessPoliciesStatus`

``` purescript
newtype AccessPoliciesStatus
  = AccessPoliciesStatus { "Options" :: PolicyDocument, "Status" :: OptionStatus }
```

<p>A <code>PolicyDocument</code> that specifies access policies for the search domain's services, and the current status of those policies.</p>

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

<p>An Amazon Resource Name (ARN). See <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html" target="_blank">Identifiers for IAM Entities</a> in <i>Using AWS Identity and Access Management</i> for more information.</p>

#### `AvailabilityOptionsStatus`

``` purescript
newtype AvailabilityOptionsStatus
  = AvailabilityOptionsStatus { "Options" :: MultiAZ, "Status" :: OptionStatus }
```

<p>The status and configuration of the domain's availability options.</p>

#### `BaseException`

``` purescript
newtype BaseException
  = BaseException { "Code" :: NullOrUndefined (ErrorCode), "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>An error occurred while processing the request.</p>

#### `CreateDomainRequest`

``` purescript
newtype CreateDomainRequest
  = CreateDomainRequest { "DomainName" :: DomainName }
```

#### `CreateDomainResponse`

``` purescript
newtype CreateDomainResponse
  = CreateDomainResponse { "DomainStatus" :: NullOrUndefined (DomainStatus) }
```

<p>A response message that contains the status of a newly created domain.</p>

#### `DefaultSearchFieldStatus`

``` purescript
newtype DefaultSearchFieldStatus
  = DefaultSearchFieldStatus { "Options" :: FieldName, "Status" :: OptionStatus }
```

<p>The value of the <code>DefaultSearchField</code> configured for this search domain and its current status.</p>

#### `DefineIndexFieldRequest`

``` purescript
newtype DefineIndexFieldRequest
  = DefineIndexFieldRequest { "DomainName" :: DomainName, "IndexField" :: IndexField }
```

#### `DefineIndexFieldResponse`

``` purescript
newtype DefineIndexFieldResponse
  = DefineIndexFieldResponse { "IndexField" :: IndexFieldStatus }
```

<p>A response message that contains the status of an updated index field.</p>

#### `DefineRankExpressionRequest`

``` purescript
newtype DefineRankExpressionRequest
  = DefineRankExpressionRequest { "DomainName" :: DomainName, "RankExpression" :: NamedRankExpression }
```

#### `DefineRankExpressionResponse`

``` purescript
newtype DefineRankExpressionResponse
  = DefineRankExpressionResponse { "RankExpression" :: RankExpressionStatus }
```

<p>A response message that contains the status of an updated <code>RankExpression</code>.</p>

#### `DeleteDomainRequest`

``` purescript
newtype DeleteDomainRequest
  = DeleteDomainRequest { "DomainName" :: DomainName }
```

#### `DeleteDomainResponse`

``` purescript
newtype DeleteDomainResponse
  = DeleteDomainResponse { "DomainStatus" :: NullOrUndefined (DomainStatus) }
```

<p>A response message that contains the status of a newly deleted domain, or no status if the domain has already been completely deleted.</p>

#### `DeleteIndexFieldRequest`

``` purescript
newtype DeleteIndexFieldRequest
  = DeleteIndexFieldRequest { "DomainName" :: DomainName, "IndexFieldName" :: FieldName }
```

#### `DeleteIndexFieldResponse`

``` purescript
newtype DeleteIndexFieldResponse
  = DeleteIndexFieldResponse { "IndexField" :: IndexFieldStatus }
```

<p>A response message that contains the status of a deleted index field.</p>

#### `DeleteRankExpressionRequest`

``` purescript
newtype DeleteRankExpressionRequest
  = DeleteRankExpressionRequest { "DomainName" :: DomainName, "RankName" :: FieldName }
```

#### `DeleteRankExpressionResponse`

``` purescript
newtype DeleteRankExpressionResponse
  = DeleteRankExpressionResponse { "RankExpression" :: RankExpressionStatus }
```

<p>A response message that contains the status of a deleted <code>RankExpression</code>.</p>

#### `DescribeAvailabilityOptionsRequest`

``` purescript
newtype DescribeAvailabilityOptionsRequest
  = DescribeAvailabilityOptionsRequest { "DomainName" :: DomainName }
```

<p>Container for the parameters to the <code><a>DescribeAvailabilityOptions</a></code> operation. Specifies the name of the domain you want to describe. To show the active configuration and exclude any pending changes, set the Deployed option to <code>true</code>.</p>

#### `DescribeAvailabilityOptionsResponse`

``` purescript
newtype DescribeAvailabilityOptionsResponse
  = DescribeAvailabilityOptionsResponse { "AvailabilityOptions" :: NullOrUndefined (AvailabilityOptionsStatus) }
```

<p>The result of a <code>DescribeAvailabilityOptions</code> request. Indicates whether or not the Multi-AZ option is enabled for the domain specified in the request. </p>

#### `DescribeDefaultSearchFieldRequest`

``` purescript
newtype DescribeDefaultSearchFieldRequest
  = DescribeDefaultSearchFieldRequest { "DomainName" :: DomainName }
```

#### `DescribeDefaultSearchFieldResponse`

``` purescript
newtype DescribeDefaultSearchFieldResponse
  = DescribeDefaultSearchFieldResponse { "DefaultSearchField" :: DefaultSearchFieldStatus }
```

<p>A response message that contains the default search field for a search domain.</p>

#### `DescribeDomainsRequest`

``` purescript
newtype DescribeDomainsRequest
  = DescribeDomainsRequest { "DomainNames" :: NullOrUndefined (DomainNameList) }
```

#### `DescribeDomainsResponse`

``` purescript
newtype DescribeDomainsResponse
  = DescribeDomainsResponse { "DomainStatusList" :: DomainStatusList }
```

<p>A response message that contains the status of one or more domains.</p>

#### `DescribeIndexFieldsRequest`

``` purescript
newtype DescribeIndexFieldsRequest
  = DescribeIndexFieldsRequest { "DomainName" :: DomainName, "FieldNames" :: NullOrUndefined (FieldNameList) }
```

#### `DescribeIndexFieldsResponse`

``` purescript
newtype DescribeIndexFieldsResponse
  = DescribeIndexFieldsResponse { "IndexFields" :: IndexFieldStatusList }
```

<p>A response message that contains the index fields for a search domain.</p>

#### `DescribeRankExpressionsRequest`

``` purescript
newtype DescribeRankExpressionsRequest
  = DescribeRankExpressionsRequest { "DomainName" :: DomainName, "RankNames" :: NullOrUndefined (FieldNameList) }
```

#### `DescribeRankExpressionsResponse`

``` purescript
newtype DescribeRankExpressionsResponse
  = DescribeRankExpressionsResponse { "RankExpressions" :: RankExpressionStatusList }
```

<p>A response message that contains the rank expressions for a search domain.</p>

#### `DescribeServiceAccessPoliciesRequest`

``` purescript
newtype DescribeServiceAccessPoliciesRequest
  = DescribeServiceAccessPoliciesRequest { "DomainName" :: DomainName }
```

#### `DescribeServiceAccessPoliciesResponse`

``` purescript
newtype DescribeServiceAccessPoliciesResponse
  = DescribeServiceAccessPoliciesResponse { "AccessPolicies" :: AccessPoliciesStatus }
```

<p>A response message that contains the access policies for a domain.</p>

#### `DescribeStemmingOptionsRequest`

``` purescript
newtype DescribeStemmingOptionsRequest
  = DescribeStemmingOptionsRequest { "DomainName" :: DomainName }
```

#### `DescribeStemmingOptionsResponse`

``` purescript
newtype DescribeStemmingOptionsResponse
  = DescribeStemmingOptionsResponse { "Stems" :: StemmingOptionsStatus }
```

<p>A response message that contains the stemming options for a search domain.</p>

#### `DescribeStopwordOptionsRequest`

``` purescript
newtype DescribeStopwordOptionsRequest
  = DescribeStopwordOptionsRequest { "DomainName" :: DomainName }
```

#### `DescribeStopwordOptionsResponse`

``` purescript
newtype DescribeStopwordOptionsResponse
  = DescribeStopwordOptionsResponse { "Stopwords" :: StopwordOptionsStatus }
```

<p>A response message that contains the stopword options for a search domain.</p>

#### `DescribeSynonymOptionsRequest`

``` purescript
newtype DescribeSynonymOptionsRequest
  = DescribeSynonymOptionsRequest { "DomainName" :: DomainName }
```

#### `DescribeSynonymOptionsResponse`

``` purescript
newtype DescribeSynonymOptionsResponse
  = DescribeSynonymOptionsResponse { "Synonyms" :: SynonymOptionsStatus }
```

<p>A response message that contains the synonym options for a search domain.</p>

#### `DisabledOperationException`

``` purescript
newtype DisabledOperationException
  = DisabledOperationException {  }
```

<p>The request was rejected because it attempted an operation which is not enabled.</p>

#### `DocumentCount`

``` purescript
newtype DocumentCount
  = DocumentCount Number
```

#### `DomainId`

``` purescript
newtype DomainId
  = DomainId String
```

<p>An internally generated unique identifier for a domain.</p>

#### `DomainName`

``` purescript
newtype DomainName
  = DomainName String
```

<p>A string that represents the name of a domain. Domain names must be unique across the domains owned by an account within an AWS region. Domain names must start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and underscores are not allowed.</p>

#### `DomainNameList`

``` purescript
newtype DomainNameList
  = DomainNameList (Array DomainName)
```

<p>A list of domain names.</p>

#### `DomainStatus`

``` purescript
newtype DomainStatus
  = DomainStatus { "DomainId" :: DomainId, "DomainName" :: DomainName, "Created" :: NullOrUndefined (Boolean), "Deleted" :: NullOrUndefined (Boolean), "NumSearchableDocs" :: NullOrUndefined (DocumentCount), "DocService" :: NullOrUndefined (ServiceEndpoint), "SearchService" :: NullOrUndefined (ServiceEndpoint), "RequiresIndexDocuments" :: Boolean, "Processing" :: NullOrUndefined (Boolean), "SearchInstanceType" :: NullOrUndefined (SearchInstanceType), "SearchPartitionCount" :: NullOrUndefined (PartitionCount), "SearchInstanceCount" :: NullOrUndefined (InstanceCount) }
```

<p>The current status of the search domain.</p>

#### `DomainStatusList`

``` purescript
newtype DomainStatusList
  = DomainStatusList (Array DomainStatus)
```

<p>The current status of all of your search domains.</p>

#### `ErrorCode`

``` purescript
newtype ErrorCode
  = ErrorCode String
```

<p>A machine-parsable string error or warning code.</p>

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

<p>A human-readable string error or warning message.</p>

#### `FieldName`

``` purescript
newtype FieldName
  = FieldName String
```

<p>A string that represents the name of an index field. Field names must begin with a letter and can contain the following characters: a-z (lowercase), 0-9, and _ (underscore). Uppercase letters and hyphens are not allowed. The names "body", "docid", and "text_relevance" are reserved and cannot be specified as field or rank expression names.</p>

#### `FieldNameList`

``` purescript
newtype FieldNameList
  = FieldNameList (Array FieldName)
```

#### `FieldValue`

``` purescript
newtype FieldValue
  = FieldValue String
```

<p>The value of a field or source document attribute.</p>

#### `IndexDocumentsRequest`

``` purescript
newtype IndexDocumentsRequest
  = IndexDocumentsRequest { "DomainName" :: DomainName }
```

#### `IndexDocumentsResponse`

``` purescript
newtype IndexDocumentsResponse
  = IndexDocumentsResponse { "FieldNames" :: NullOrUndefined (FieldNameList) }
```

<p>The result of an <code>IndexDocuments</code> action.</p>

#### `IndexField`

``` purescript
newtype IndexField
  = IndexField { "IndexFieldName" :: FieldName, "IndexFieldType" :: IndexFieldType, "UIntOptions" :: NullOrUndefined (UIntOptions), "LiteralOptions" :: NullOrUndefined (LiteralOptions), "TextOptions" :: NullOrUndefined (TextOptions), "SourceAttributes" :: NullOrUndefined (SourceAttributeList) }
```

<p>Defines a field in the index, including its name, type, and the source of its data. The <code>IndexFieldType</code> indicates which of the options will be present. It is invalid to specify options for a type other than the <code>IndexFieldType</code>.</p>

#### `IndexFieldStatus`

``` purescript
newtype IndexFieldStatus
  = IndexFieldStatus { "Options" :: IndexField, "Status" :: OptionStatus }
```

<p>The value of an <code>IndexField</code> and its current status.</p>

#### `IndexFieldStatusList`

``` purescript
newtype IndexFieldStatusList
  = IndexFieldStatusList (Array IndexFieldStatus)
```

#### `IndexFieldType`

``` purescript
newtype IndexFieldType
  = IndexFieldType String
```

<p>The type of <code>IndexField</code>.</p>

#### `InstanceCount`

``` purescript
newtype InstanceCount
  = InstanceCount Int
```

#### `InternalException`

``` purescript
newtype InternalException
  = InternalException {  }
```

<p>An internal error occurred while processing the request. If this problem persists, report an issue from the <a href="http://status.aws.amazon.com/">Service Health Dashboard</a>.</p>

#### `InvalidTypeException`

``` purescript
newtype InvalidTypeException
  = InvalidTypeException {  }
```

<p>The request was rejected because it specified an invalid type definition.</p>

#### `Language`

``` purescript
newtype Language
  = Language String
```

<p>An <a href="http://tools.ietf.org/html/rfc4646">IETF RFC 4646</a> language code. Only the primary language is considered. English (en) is currently the only supported language.</p>

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>The request was rejected because a resource limit has already been met.</p>

#### `LiteralOptions`

``` purescript
newtype LiteralOptions
  = LiteralOptions { "DefaultValue" :: NullOrUndefined (FieldValue), "SearchEnabled" :: NullOrUndefined (Boolean), "FacetEnabled" :: NullOrUndefined (Boolean), "ResultEnabled" :: NullOrUndefined (Boolean) }
```

<p>Options that define a literal field in the search index.</p>

#### `MultiAZ`

``` purescript
newtype MultiAZ
  = MultiAZ Boolean
```

#### `NamedRankExpression`

``` purescript
newtype NamedRankExpression
  = NamedRankExpression { "RankName" :: FieldName, "RankExpression" :: RankExpression }
```

<p>A named expression that can be evaluated at search time and used for ranking or thresholding in a search query. </p>

#### `OptionState`

``` purescript
newtype OptionState
  = OptionState String
```

<p>The state of processing a change to an option.</p>

#### `OptionStatus`

``` purescript
newtype OptionStatus
  = OptionStatus { "CreationDate" :: UpdateTimestamp, "UpdateDate" :: UpdateTimestamp, "UpdateVersion" :: NullOrUndefined (UIntValue), "State" :: OptionState, "PendingDeletion" :: NullOrUndefined (Boolean) }
```

<p>The status of an option, including when it was last updated and whether it is actively in use for searches.</p>

#### `PartitionCount`

``` purescript
newtype PartitionCount
  = PartitionCount Int
```

#### `PolicyDocument`

``` purescript
newtype PolicyDocument
  = PolicyDocument String
```

<p>An IAM access policy as described in <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?AccessPolicyLanguage.html" target="_blank">The Access Policy Language</a> in <i>Using AWS Identity and Access Management</i>. The maximum size of an access policy document is 100 KB.</p> <p>Example: <code>{"Statement": [{"Effect":"Allow", "Action": "*", "Resource": "arn:aws:cs:us-east-1:1234567890:search/movies", "Condition": { "IpAddress": { "aws:SourceIp": ["203.0.113.1/32"] } }}, {"Effect":"Allow", "Action": "*", "Resource": "arn:aws:cs:us-east-1:1234567890:documents/movies", "Condition": { "IpAddress": { "aws:SourceIp": ["203.0.113.1/32"] } }} ] }</code></p>

#### `RankExpression`

``` purescript
newtype RankExpression
  = RankExpression String
```

<p>The current status of the rank expression.</p>

#### `RankExpressionStatus`

``` purescript
newtype RankExpressionStatus
  = RankExpressionStatus { "Options" :: NamedRankExpression, "Status" :: OptionStatus }
```

<p>The value of a <code>RankExpression</code> and its current status.</p>

#### `RankExpressionStatusList`

``` purescript
newtype RankExpressionStatusList
  = RankExpressionStatusList (Array RankExpressionStatus)
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException {  }
```

<p>The request was rejected because it attempted to reference a resource that does not exist.</p>

#### `SearchInstanceType`

``` purescript
newtype SearchInstanceType
  = SearchInstanceType String
```

#### `ServiceEndpoint`

``` purescript
newtype ServiceEndpoint
  = ServiceEndpoint { "Arn" :: NullOrUndefined (Arn), "Endpoint" :: NullOrUndefined (ServiceUrl) }
```

<p>The endpoint to which service requests can be submitted, including the actual URL prefix for sending requests and the Amazon Resource Name (ARN) so the endpoint can be referenced in other API calls such as <a>UpdateServiceAccessPolicies</a>.</p>

#### `ServiceUrl`

``` purescript
newtype ServiceUrl
  = ServiceUrl String
```

<p>The URL (including /version/pathPrefix) to which service requests can be submitted.</p>

#### `SourceAttribute`

``` purescript
newtype SourceAttribute
  = SourceAttribute { "SourceDataFunction" :: SourceDataFunction, "SourceDataCopy" :: NullOrUndefined (SourceData), "SourceDataTrimTitle" :: NullOrUndefined (SourceDataTrimTitle), "SourceDataMap" :: NullOrUndefined (SourceDataMap) }
```

<p>Identifies the source data for an index field. An optional data transformation can be applied to the source data when populating the index field. By default, the value of the source attribute is copied to the index field.</p>

#### `SourceAttributeList`

``` purescript
newtype SourceAttributeList
  = SourceAttributeList (Array SourceAttribute)
```

#### `SourceData`

``` purescript
newtype SourceData
  = SourceData { "SourceName" :: FieldName, "DefaultValue" :: NullOrUndefined (FieldValue) }
```

<p>The source attribute name and an optional default value to use if a document doesn't have an attribute of that name.</p>

#### `SourceDataFunction`

``` purescript
newtype SourceDataFunction
  = SourceDataFunction String
```

#### `SourceDataMap`

``` purescript
newtype SourceDataMap
  = SourceDataMap { "SourceName" :: FieldName, "DefaultValue" :: NullOrUndefined (FieldValue), "Cases" :: NullOrUndefined (StringCaseMap) }
```

<p>Specifies how to map source attribute values to custom values when populating an <code>IndexField</code>.</p>

#### `SourceDataTrimTitle`

``` purescript
newtype SourceDataTrimTitle
  = SourceDataTrimTitle { "SourceName" :: FieldName, "DefaultValue" :: NullOrUndefined (FieldValue), "Separator" :: NullOrUndefined (String), "Language" :: NullOrUndefined (Language) }
```

<p>Specifies how to trim common words from the beginning of a field to enable title sorting by that field.</p>

#### `StemmingOptionsStatus`

``` purescript
newtype StemmingOptionsStatus
  = StemmingOptionsStatus { "Options" :: StemsDocument, "Status" :: OptionStatus }
```

<p>The stemming options configured for this search domain and the current status of those options.</p>

#### `StemsDocument`

``` purescript
newtype StemsDocument
  = StemsDocument String
```

<p>Maps terms to their stems, serialized as a JSON document. The document has a single object with one property "stems" whose value is an object mapping terms to their stems. The maximum size of a stemming document is 500 KB. Example: <code>{ "stems": {"people": "person", "walking": "walk"} }</code></p>

#### `StopwordOptionsStatus`

``` purescript
newtype StopwordOptionsStatus
  = StopwordOptionsStatus { "Options" :: StopwordsDocument, "Status" :: OptionStatus }
```

<p>The stopword options configured for this search domain and the current status of those options.</p>

#### `StopwordsDocument`

``` purescript
newtype StopwordsDocument
  = StopwordsDocument String
```

<p>Lists stopwords serialized as a JSON document. The document has a single object with one property "stopwords" whose value is an array of strings. The maximum size of a stopwords document is 10 KB. Example: <code>{ "stopwords": ["a", "an", "the", "of"] }</code></p>

#### `StringCaseMap`

``` purescript
newtype StringCaseMap
  = StringCaseMap (Map FieldValue FieldValue)
```

#### `SynonymOptionsStatus`

``` purescript
newtype SynonymOptionsStatus
  = SynonymOptionsStatus { "Options" :: SynonymsDocument, "Status" :: OptionStatus }
```

<p>The synonym options configured for this search domain and the current status of those options.</p>

#### `SynonymsDocument`

``` purescript
newtype SynonymsDocument
  = SynonymsDocument String
```

<p>Maps terms to their synonyms, serialized as a JSON document. The document has a single object with one property "synonyms" whose value is an object mapping terms to their synonyms. Each synonym is a simple string or an array of strings. The maximum size of a stopwords document is 100 KB. Example: <code>{ "synonyms": {"cat": ["feline", "kitten"], "puppy": "dog"} }</code></p>

#### `TextOptions`

``` purescript
newtype TextOptions
  = TextOptions { "DefaultValue" :: NullOrUndefined (FieldValue), "FacetEnabled" :: NullOrUndefined (Boolean), "ResultEnabled" :: NullOrUndefined (Boolean), "TextProcessor" :: NullOrUndefined (FieldName) }
```

<p>Options that define a text field in the search index.</p>

#### `UIntOptions`

``` purescript
newtype UIntOptions
  = UIntOptions { "DefaultValue" :: NullOrUndefined (UIntValue) }
```

<p>Options that define a <code>uint</code> field in the search index.</p>

#### `UIntValue`

``` purescript
newtype UIntValue
  = UIntValue Int
```

#### `UpdateAvailabilityOptionsRequest`

``` purescript
newtype UpdateAvailabilityOptionsRequest
  = UpdateAvailabilityOptionsRequest { "DomainName" :: DomainName, "MultiAZ" :: Boolean }
```

<p>Container for the parameters to the <code><a>UpdateAvailabilityOptions</a></code> operation. Specifies the name of the domain you want to update and the Multi-AZ availability option.</p>

#### `UpdateAvailabilityOptionsResponse`

``` purescript
newtype UpdateAvailabilityOptionsResponse
  = UpdateAvailabilityOptionsResponse { "AvailabilityOptions" :: NullOrUndefined (AvailabilityOptionsStatus) }
```

<p>The result of a <code>UpdateAvailabilityOptions</code> request. Contains the status of the domain's availability options. </p>

#### `UpdateDefaultSearchFieldRequest`

``` purescript
newtype UpdateDefaultSearchFieldRequest
  = UpdateDefaultSearchFieldRequest { "DomainName" :: DomainName, "DefaultSearchField" :: String }
```

#### `UpdateDefaultSearchFieldResponse`

``` purescript
newtype UpdateDefaultSearchFieldResponse
  = UpdateDefaultSearchFieldResponse { "DefaultSearchField" :: DefaultSearchFieldStatus }
```

<p>A response message that contains the status of an updated default search field.</p>

#### `UpdateServiceAccessPoliciesRequest`

``` purescript
newtype UpdateServiceAccessPoliciesRequest
  = UpdateServiceAccessPoliciesRequest { "DomainName" :: DomainName, "AccessPolicies" :: PolicyDocument }
```

#### `UpdateServiceAccessPoliciesResponse`

``` purescript
newtype UpdateServiceAccessPoliciesResponse
  = UpdateServiceAccessPoliciesResponse { "AccessPolicies" :: AccessPoliciesStatus }
```

<p>A response message that contains the status of updated access policies.</p>

#### `UpdateStemmingOptionsRequest`

``` purescript
newtype UpdateStemmingOptionsRequest
  = UpdateStemmingOptionsRequest { "DomainName" :: DomainName, "Stems" :: StemsDocument }
```

#### `UpdateStemmingOptionsResponse`

``` purescript
newtype UpdateStemmingOptionsResponse
  = UpdateStemmingOptionsResponse { "Stems" :: StemmingOptionsStatus }
```

<p>A response message that contains the status of updated stemming options.</p>

#### `UpdateStopwordOptionsRequest`

``` purescript
newtype UpdateStopwordOptionsRequest
  = UpdateStopwordOptionsRequest { "DomainName" :: DomainName, "Stopwords" :: StopwordsDocument }
```

#### `UpdateStopwordOptionsResponse`

``` purescript
newtype UpdateStopwordOptionsResponse
  = UpdateStopwordOptionsResponse { "Stopwords" :: StopwordOptionsStatus }
```

<p>A response message that contains the status of updated stopword options.</p>

#### `UpdateSynonymOptionsRequest`

``` purescript
newtype UpdateSynonymOptionsRequest
  = UpdateSynonymOptionsRequest { "DomainName" :: DomainName, "Synonyms" :: SynonymsDocument }
```

#### `UpdateSynonymOptionsResponse`

``` purescript
newtype UpdateSynonymOptionsResponse
  = UpdateSynonymOptionsResponse { "Synonyms" :: SynonymOptionsStatus }
```

<p>A response message that contains the status of updated synonym options.</p>

#### `UpdateTimestamp`

``` purescript
newtype UpdateTimestamp
  = UpdateTimestamp Number
```


