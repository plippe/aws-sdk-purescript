

-- | <fullname>Amazon CloudSearch Configuration Service</fullname> <p>You use the configuration service to create, configure, and manage search domains. Configuration service requests are submitted using the AWS Query protocol. AWS Query requests are HTTP or HTTPS requests submitted via HTTP GET or POST with a query parameter named Action.</p> <p>The endpoint for configuration service requests is region-specific: cloudsearch.<i>region</i>.amazonaws.com. For example, cloudsearch.us-east-1.amazonaws.com. For a current list of supported regions and endpoints, see <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#cloudsearch_region">Regions and Endpoints</a>.</p>
module AWS.CloudSearch where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudSearch" :: String


-- | <p>Creates a new search domain.</p>
createDomain :: forall eff. CreateDomainRequest -> Aff (err :: AWS.RequestError | eff) CreateDomainResponse
createDomain = AWS.request serviceName "CreateDomain" 


-- | <p>Configures an <code>IndexField</code> for the search domain. Used to create new fields and modify existing ones. If the field exists, the new configuration replaces the old one. You can configure a maximum of 200 index fields.</p>
defineIndexField :: forall eff. DefineIndexFieldRequest -> Aff (err :: AWS.RequestError | eff) DefineIndexFieldResponse
defineIndexField = AWS.request serviceName "DefineIndexField" 


-- | <p>Configures a <code>RankExpression</code> for the search domain. Used to create new rank expressions and modify existing ones. If the expression exists, the new configuration replaces the old one. You can configure a maximum of 50 rank expressions.</p>
defineRankExpression :: forall eff. DefineRankExpressionRequest -> Aff (err :: AWS.RequestError | eff) DefineRankExpressionResponse
defineRankExpression = AWS.request serviceName "DefineRankExpression" 


-- | <p>Permanently deletes a search domain and all of its data.</p>
deleteDomain :: forall eff. DeleteDomainRequest -> Aff (err :: AWS.RequestError | eff) DeleteDomainResponse
deleteDomain = AWS.request serviceName "DeleteDomain" 


-- | <p>Removes an <code>IndexField</code> from the search domain.</p>
deleteIndexField :: forall eff. DeleteIndexFieldRequest -> Aff (err :: AWS.RequestError | eff) DeleteIndexFieldResponse
deleteIndexField = AWS.request serviceName "DeleteIndexField" 


-- | <p>Removes a <code>RankExpression</code> from the search domain.</p>
deleteRankExpression :: forall eff. DeleteRankExpressionRequest -> Aff (err :: AWS.RequestError | eff) DeleteRankExpressionResponse
deleteRankExpression = AWS.request serviceName "DeleteRankExpression" 


-- | <p>Gets the availability options configured for a domain. By default, shows the configuration with any pending changes. Set the <code>Deployed</code> option to <code>true</code> to show the active configuration and exclude pending changes. For more information, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html" target="_blank">Configuring Availability Options</a> in the <i>Amazon CloudSearch Developer Guide</i>.</p>
describeAvailabilityOptions :: forall eff. DescribeAvailabilityOptionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeAvailabilityOptionsResponse
describeAvailabilityOptions = AWS.request serviceName "DescribeAvailabilityOptions" 


-- | <p>Gets the default search field configured for the search domain.</p>
describeDefaultSearchField :: forall eff. DescribeDefaultSearchFieldRequest -> Aff (err :: AWS.RequestError | eff) DescribeDefaultSearchFieldResponse
describeDefaultSearchField = AWS.request serviceName "DescribeDefaultSearchField" 


-- | <p>Gets information about the search domains owned by this account. Can be limited to specific domains. Shows all domains by default.</p>
describeDomains :: forall eff. DescribeDomainsRequest -> Aff (err :: AWS.RequestError | eff) DescribeDomainsResponse
describeDomains = AWS.request serviceName "DescribeDomains" 


-- | <p>Gets information about the index fields configured for the search domain. Can be limited to specific fields by name. Shows all fields by default.</p>
describeIndexFields :: forall eff. DescribeIndexFieldsRequest -> Aff (err :: AWS.RequestError | eff) DescribeIndexFieldsResponse
describeIndexFields = AWS.request serviceName "DescribeIndexFields" 


-- | <p>Gets the rank expressions configured for the search domain. Can be limited to specific rank expressions by name. Shows all rank expressions by default. </p>
describeRankExpressions :: forall eff. DescribeRankExpressionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeRankExpressionsResponse
describeRankExpressions = AWS.request serviceName "DescribeRankExpressions" 


-- | <p>Gets information about the resource-based policies that control access to the domain's document and search services.</p>
describeServiceAccessPolicies :: forall eff. DescribeServiceAccessPoliciesRequest -> Aff (err :: AWS.RequestError | eff) DescribeServiceAccessPoliciesResponse
describeServiceAccessPolicies = AWS.request serviceName "DescribeServiceAccessPolicies" 


-- | <p>Gets the stemming dictionary configured for the search domain.</p>
describeStemmingOptions :: forall eff. DescribeStemmingOptionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeStemmingOptionsResponse
describeStemmingOptions = AWS.request serviceName "DescribeStemmingOptions" 


-- | <p>Gets the stopwords configured for the search domain.</p>
describeStopwordOptions :: forall eff. DescribeStopwordOptionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeStopwordOptionsResponse
describeStopwordOptions = AWS.request serviceName "DescribeStopwordOptions" 


-- | <p>Gets the synonym dictionary configured for the search domain.</p>
describeSynonymOptions :: forall eff. DescribeSynonymOptionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeSynonymOptionsResponse
describeSynonymOptions = AWS.request serviceName "DescribeSynonymOptions" 


-- | <p>Tells the search domain to start indexing its documents using the latest text processing options and <code>IndexFields</code>. This operation must be invoked to make options whose <a>OptionStatus</a> has <code>OptionState</code> of <code>RequiresIndexDocuments</code> visible in search results.</p>
indexDocuments :: forall eff. IndexDocumentsRequest -> Aff (err :: AWS.RequestError | eff) IndexDocumentsResponse
indexDocuments = AWS.request serviceName "IndexDocuments" 


-- | <p>Configures the availability options for a domain. Enabling the Multi-AZ option expands an Amazon CloudSearch domain to an additional Availability Zone in the same Region to increase fault tolerance in the event of a service disruption. Changes to the Multi-AZ option can take about half an hour to become active. For more information, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html" target="_blank">Configuring Availability Options</a> in the <i>Amazon CloudSearch Developer Guide</i>.</p>
updateAvailabilityOptions :: forall eff. UpdateAvailabilityOptionsRequest -> Aff (err :: AWS.RequestError | eff) UpdateAvailabilityOptionsResponse
updateAvailabilityOptions = AWS.request serviceName "UpdateAvailabilityOptions" 


-- | <p>Configures the default search field for the search domain. The default search field is the text field that is searched when a search request does not specify which fields to search. By default, it is configured to include the contents of all of the domain's text fields. </p>
updateDefaultSearchField :: forall eff. UpdateDefaultSearchFieldRequest -> Aff (err :: AWS.RequestError | eff) UpdateDefaultSearchFieldResponse
updateDefaultSearchField = AWS.request serviceName "UpdateDefaultSearchField" 


-- | <p>Configures the policies that control access to the domain's document and search services. The maximum size of an access policy document is 100 KB.</p>
updateServiceAccessPolicies :: forall eff. UpdateServiceAccessPoliciesRequest -> Aff (err :: AWS.RequestError | eff) UpdateServiceAccessPoliciesResponse
updateServiceAccessPolicies = AWS.request serviceName "UpdateServiceAccessPolicies" 


-- | <p>Configures a stemming dictionary for the search domain. The stemming dictionary is used during indexing and when processing search requests. The maximum size of the stemming dictionary is 500 KB.</p>
updateStemmingOptions :: forall eff. UpdateStemmingOptionsRequest -> Aff (err :: AWS.RequestError | eff) UpdateStemmingOptionsResponse
updateStemmingOptions = AWS.request serviceName "UpdateStemmingOptions" 


-- | <p>Configures stopwords for the search domain. Stopwords are used during indexing and when processing search requests. The maximum size of the stopwords dictionary is 10 KB.</p>
updateStopwordOptions :: forall eff. UpdateStopwordOptionsRequest -> Aff (err :: AWS.RequestError | eff) UpdateStopwordOptionsResponse
updateStopwordOptions = AWS.request serviceName "UpdateStopwordOptions" 


-- | <p>Configures a synonym dictionary for the search domain. The synonym dictionary is used during indexing to configure mappings for terms that occur in text fields. The maximum size of the synonym dictionary is 100 KB. </p>
updateSynonymOptions :: forall eff. UpdateSynonymOptionsRequest -> Aff (err :: AWS.RequestError | eff) UpdateSynonymOptionsResponse
updateSynonymOptions = AWS.request serviceName "UpdateSynonymOptions" 


-- | <p>A <code>PolicyDocument</code> that specifies access policies for the search domain's services, and the current status of those policies.</p>
newtype AccessPoliciesStatus = AccessPoliciesStatus 
  { "Options" :: (PolicyDocument)
  , "Status" :: (OptionStatus)
  }


-- | <p>An Amazon Resource Name (ARN). See <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html" target="_blank">Identifiers for IAM Entities</a> in <i>Using AWS Identity and Access Management</i> for more information.</p>
newtype Arn = Arn String


-- | <p>The status and configuration of the domain's availability options.</p>
newtype AvailabilityOptionsStatus = AvailabilityOptionsStatus 
  { "Options" :: (MultiAZ)
  , "Status" :: (OptionStatus)
  }


-- | <p>An error occurred while processing the request.</p>
newtype BaseException = BaseException 
  { "Code" :: NullOrUndefined (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype CreateDomainRequest = CreateDomainRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>A response message that contains the status of a newly created domain.</p>
newtype CreateDomainResponse = CreateDomainResponse 
  { "DomainStatus" :: NullOrUndefined (DomainStatus)
  }


-- | <p>The value of the <code>DefaultSearchField</code> configured for this search domain and its current status.</p>
newtype DefaultSearchFieldStatus = DefaultSearchFieldStatus 
  { "Options" :: (FieldName)
  , "Status" :: (OptionStatus)
  }


newtype DefineIndexFieldRequest = DefineIndexFieldRequest 
  { "DomainName" :: (DomainName)
  , "IndexField" :: (IndexField)
  }


-- | <p>A response message that contains the status of an updated index field.</p>
newtype DefineIndexFieldResponse = DefineIndexFieldResponse 
  { "IndexField" :: (IndexFieldStatus)
  }


newtype DefineRankExpressionRequest = DefineRankExpressionRequest 
  { "DomainName" :: (DomainName)
  , "RankExpression" :: (NamedRankExpression)
  }


-- | <p>A response message that contains the status of an updated <code>RankExpression</code>.</p>
newtype DefineRankExpressionResponse = DefineRankExpressionResponse 
  { "RankExpression" :: (RankExpressionStatus)
  }


newtype DeleteDomainRequest = DeleteDomainRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>A response message that contains the status of a newly deleted domain, or no status if the domain has already been completely deleted.</p>
newtype DeleteDomainResponse = DeleteDomainResponse 
  { "DomainStatus" :: NullOrUndefined (DomainStatus)
  }


newtype DeleteIndexFieldRequest = DeleteIndexFieldRequest 
  { "DomainName" :: (DomainName)
  , "IndexFieldName" :: (FieldName)
  }


-- | <p>A response message that contains the status of a deleted index field.</p>
newtype DeleteIndexFieldResponse = DeleteIndexFieldResponse 
  { "IndexField" :: (IndexFieldStatus)
  }


newtype DeleteRankExpressionRequest = DeleteRankExpressionRequest 
  { "DomainName" :: (DomainName)
  , "RankName" :: (FieldName)
  }


-- | <p>A response message that contains the status of a deleted <code>RankExpression</code>.</p>
newtype DeleteRankExpressionResponse = DeleteRankExpressionResponse 
  { "RankExpression" :: (RankExpressionStatus)
  }


-- | <p>Container for the parameters to the <code><a>DescribeAvailabilityOptions</a></code> operation. Specifies the name of the domain you want to describe. To show the active configuration and exclude any pending changes, set the Deployed option to <code>true</code>.</p>
newtype DescribeAvailabilityOptionsRequest = DescribeAvailabilityOptionsRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>The result of a <code>DescribeAvailabilityOptions</code> request. Indicates whether or not the Multi-AZ option is enabled for the domain specified in the request. </p>
newtype DescribeAvailabilityOptionsResponse = DescribeAvailabilityOptionsResponse 
  { "AvailabilityOptions" :: NullOrUndefined (AvailabilityOptionsStatus)
  }


newtype DescribeDefaultSearchFieldRequest = DescribeDefaultSearchFieldRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>A response message that contains the default search field for a search domain.</p>
newtype DescribeDefaultSearchFieldResponse = DescribeDefaultSearchFieldResponse 
  { "DefaultSearchField" :: (DefaultSearchFieldStatus)
  }


newtype DescribeDomainsRequest = DescribeDomainsRequest 
  { "DomainNames" :: NullOrUndefined (DomainNameList)
  }


-- | <p>A response message that contains the status of one or more domains.</p>
newtype DescribeDomainsResponse = DescribeDomainsResponse 
  { "DomainStatusList" :: (DomainStatusList)
  }


newtype DescribeIndexFieldsRequest = DescribeIndexFieldsRequest 
  { "DomainName" :: (DomainName)
  , "FieldNames" :: NullOrUndefined (FieldNameList)
  }


-- | <p>A response message that contains the index fields for a search domain.</p>
newtype DescribeIndexFieldsResponse = DescribeIndexFieldsResponse 
  { "IndexFields" :: (IndexFieldStatusList)
  }


newtype DescribeRankExpressionsRequest = DescribeRankExpressionsRequest 
  { "DomainName" :: (DomainName)
  , "RankNames" :: NullOrUndefined (FieldNameList)
  }


-- | <p>A response message that contains the rank expressions for a search domain.</p>
newtype DescribeRankExpressionsResponse = DescribeRankExpressionsResponse 
  { "RankExpressions" :: (RankExpressionStatusList)
  }


newtype DescribeServiceAccessPoliciesRequest = DescribeServiceAccessPoliciesRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>A response message that contains the access policies for a domain.</p>
newtype DescribeServiceAccessPoliciesResponse = DescribeServiceAccessPoliciesResponse 
  { "AccessPolicies" :: (AccessPoliciesStatus)
  }


newtype DescribeStemmingOptionsRequest = DescribeStemmingOptionsRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>A response message that contains the stemming options for a search domain.</p>
newtype DescribeStemmingOptionsResponse = DescribeStemmingOptionsResponse 
  { "Stems" :: (StemmingOptionsStatus)
  }


newtype DescribeStopwordOptionsRequest = DescribeStopwordOptionsRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>A response message that contains the stopword options for a search domain.</p>
newtype DescribeStopwordOptionsResponse = DescribeStopwordOptionsResponse 
  { "Stopwords" :: (StopwordOptionsStatus)
  }


newtype DescribeSynonymOptionsRequest = DescribeSynonymOptionsRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>A response message that contains the synonym options for a search domain.</p>
newtype DescribeSynonymOptionsResponse = DescribeSynonymOptionsResponse 
  { "Synonyms" :: (SynonymOptionsStatus)
  }


-- | <p>The request was rejected because it attempted an operation which is not enabled.</p>
newtype DisabledOperationException = DisabledOperationException 
  { 
  }


newtype DocumentCount = DocumentCount Number


-- | <p>An internally generated unique identifier for a domain.</p>
newtype DomainId = DomainId String


-- | <p>A string that represents the name of a domain. Domain names must be unique across the domains owned by an account within an AWS region. Domain names must start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and underscores are not allowed.</p>
newtype DomainName = DomainName String


-- | <p>A list of domain names.</p>
newtype DomainNameList = DomainNameList (Array DomainName)


-- | <p>The current status of the search domain.</p>
newtype DomainStatus = DomainStatus 
  { "DomainId" :: (DomainId)
  , "DomainName" :: (DomainName)
  , "Created" :: NullOrUndefined (Boolean)
  , "Deleted" :: NullOrUndefined (Boolean)
  , "NumSearchableDocs" :: NullOrUndefined (DocumentCount)
  , "DocService" :: NullOrUndefined (ServiceEndpoint)
  , "SearchService" :: NullOrUndefined (ServiceEndpoint)
  , "RequiresIndexDocuments" :: (Boolean)
  , "Processing" :: NullOrUndefined (Boolean)
  , "SearchInstanceType" :: NullOrUndefined (SearchInstanceType)
  , "SearchPartitionCount" :: NullOrUndefined (PartitionCount)
  , "SearchInstanceCount" :: NullOrUndefined (InstanceCount)
  }


-- | <p>The current status of all of your search domains.</p>
newtype DomainStatusList = DomainStatusList (Array DomainStatus)


-- | <p>A machine-parsable string error or warning code.</p>
newtype ErrorCode = ErrorCode String


-- | <p>A human-readable string error or warning message.</p>
newtype ErrorMessage = ErrorMessage String


-- | <p>A string that represents the name of an index field. Field names must begin with a letter and can contain the following characters: a-z (lowercase), 0-9, and _ (underscore). Uppercase letters and hyphens are not allowed. The names "body", "docid", and "text_relevance" are reserved and cannot be specified as field or rank expression names.</p>
newtype FieldName = FieldName String


newtype FieldNameList = FieldNameList (Array FieldName)


-- | <p>The value of a field or source document attribute.</p>
newtype FieldValue = FieldValue String


newtype IndexDocumentsRequest = IndexDocumentsRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>The result of an <code>IndexDocuments</code> action.</p>
newtype IndexDocumentsResponse = IndexDocumentsResponse 
  { "FieldNames" :: NullOrUndefined (FieldNameList)
  }


-- | <p>Defines a field in the index, including its name, type, and the source of its data. The <code>IndexFieldType</code> indicates which of the options will be present. It is invalid to specify options for a type other than the <code>IndexFieldType</code>.</p>
newtype IndexField = IndexField 
  { "IndexFieldName" :: (FieldName)
  , "IndexFieldType" :: (IndexFieldType)
  , "UIntOptions" :: NullOrUndefined (UIntOptions)
  , "LiteralOptions" :: NullOrUndefined (LiteralOptions)
  , "TextOptions" :: NullOrUndefined (TextOptions)
  , "SourceAttributes" :: NullOrUndefined (SourceAttributeList)
  }


-- | <p>The value of an <code>IndexField</code> and its current status.</p>
newtype IndexFieldStatus = IndexFieldStatus 
  { "Options" :: (IndexField)
  , "Status" :: (OptionStatus)
  }


newtype IndexFieldStatusList = IndexFieldStatusList (Array IndexFieldStatus)


-- | <p>The type of <code>IndexField</code>.</p>
newtype IndexFieldType = IndexFieldType String


newtype InstanceCount = InstanceCount Int


-- | <p>An internal error occurred while processing the request. If this problem persists, report an issue from the <a href="http://status.aws.amazon.com/">Service Health Dashboard</a>.</p>
newtype InternalException = InternalException 
  { 
  }


-- | <p>The request was rejected because it specified an invalid type definition.</p>
newtype InvalidTypeException = InvalidTypeException 
  { 
  }


-- | <p>An <a href="http://tools.ietf.org/html/rfc4646">IETF RFC 4646</a> language code. Only the primary language is considered. English (en) is currently the only supported language.</p>
newtype Language = Language String


-- | <p>The request was rejected because a resource limit has already been met.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }


-- | <p>Options that define a literal field in the search index.</p>
newtype LiteralOptions = LiteralOptions 
  { "DefaultValue" :: NullOrUndefined (FieldValue)
  , "SearchEnabled" :: NullOrUndefined (Boolean)
  , "FacetEnabled" :: NullOrUndefined (Boolean)
  , "ResultEnabled" :: NullOrUndefined (Boolean)
  }


newtype MultiAZ = MultiAZ Boolean


-- | <p>A named expression that can be evaluated at search time and used for ranking or thresholding in a search query. </p>
newtype NamedRankExpression = NamedRankExpression 
  { "RankName" :: (FieldName)
  , "RankExpression" :: (RankExpression)
  }


-- | <p>The state of processing a change to an option.</p>
newtype OptionState = OptionState String


-- | <p>The status of an option, including when it was last updated and whether it is actively in use for searches.</p>
newtype OptionStatus = OptionStatus 
  { "CreationDate" :: (UpdateTimestamp)
  , "UpdateDate" :: (UpdateTimestamp)
  , "UpdateVersion" :: NullOrUndefined (UIntValue)
  , "State" :: (OptionState)
  , "PendingDeletion" :: NullOrUndefined (Boolean)
  }


newtype PartitionCount = PartitionCount Int


-- | <p>An IAM access policy as described in <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?AccessPolicyLanguage.html" target="_blank">The Access Policy Language</a> in <i>Using AWS Identity and Access Management</i>. The maximum size of an access policy document is 100 KB.</p> <p>Example: <code>{"Statement": [{"Effect":"Allow", "Action": "*", "Resource": "arn:aws:cs:us-east-1:1234567890:search/movies", "Condition": { "IpAddress": { "aws:SourceIp": ["203.0.113.1/32"] } }}, {"Effect":"Allow", "Action": "*", "Resource": "arn:aws:cs:us-east-1:1234567890:documents/movies", "Condition": { "IpAddress": { "aws:SourceIp": ["203.0.113.1/32"] } }} ] }</code></p>
newtype PolicyDocument = PolicyDocument String


-- | <p>The current status of the rank expression.</p>
newtype RankExpression = RankExpression String


-- | <p>The value of a <code>RankExpression</code> and its current status.</p>
newtype RankExpressionStatus = RankExpressionStatus 
  { "Options" :: (NamedRankExpression)
  , "Status" :: (OptionStatus)
  }


newtype RankExpressionStatusList = RankExpressionStatusList (Array RankExpressionStatus)


-- | <p>The request was rejected because it attempted to reference a resource that does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }


newtype SearchInstanceType = SearchInstanceType String


-- | <p>The endpoint to which service requests can be submitted, including the actual URL prefix for sending requests and the Amazon Resource Name (ARN) so the endpoint can be referenced in other API calls such as <a>UpdateServiceAccessPolicies</a>.</p>
newtype ServiceEndpoint = ServiceEndpoint 
  { "Arn" :: NullOrUndefined (Arn)
  , "Endpoint" :: NullOrUndefined (ServiceUrl)
  }


-- | <p>The URL (including /version/pathPrefix) to which service requests can be submitted.</p>
newtype ServiceUrl = ServiceUrl String


-- | <p>Identifies the source data for an index field. An optional data transformation can be applied to the source data when populating the index field. By default, the value of the source attribute is copied to the index field.</p>
newtype SourceAttribute = SourceAttribute 
  { "SourceDataFunction" :: (SourceDataFunction)
  , "SourceDataCopy" :: NullOrUndefined (SourceData)
  , "SourceDataTrimTitle" :: NullOrUndefined (SourceDataTrimTitle)
  , "SourceDataMap" :: NullOrUndefined (SourceDataMap)
  }


newtype SourceAttributeList = SourceAttributeList (Array SourceAttribute)


-- | <p>The source attribute name and an optional default value to use if a document doesn't have an attribute of that name.</p>
newtype SourceData = SourceData 
  { "SourceName" :: (FieldName)
  , "DefaultValue" :: NullOrUndefined (FieldValue)
  }


newtype SourceDataFunction = SourceDataFunction String


-- | <p>Specifies how to map source attribute values to custom values when populating an <code>IndexField</code>.</p>
newtype SourceDataMap = SourceDataMap 
  { "SourceName" :: (FieldName)
  , "DefaultValue" :: NullOrUndefined (FieldValue)
  , "Cases" :: NullOrUndefined (StringCaseMap)
  }


-- | <p>Specifies how to trim common words from the beginning of a field to enable title sorting by that field.</p>
newtype SourceDataTrimTitle = SourceDataTrimTitle 
  { "SourceName" :: (FieldName)
  , "DefaultValue" :: NullOrUndefined (FieldValue)
  , "Separator" :: NullOrUndefined (String)
  , "Language" :: NullOrUndefined (Language)
  }


-- | <p>The stemming options configured for this search domain and the current status of those options.</p>
newtype StemmingOptionsStatus = StemmingOptionsStatus 
  { "Options" :: (StemsDocument)
  , "Status" :: (OptionStatus)
  }


-- | <p>Maps terms to their stems, serialized as a JSON document. The document has a single object with one property "stems" whose value is an object mapping terms to their stems. The maximum size of a stemming document is 500 KB. Example: <code>{ "stems": {"people": "person", "walking": "walk"} }</code></p>
newtype StemsDocument = StemsDocument String


-- | <p>The stopword options configured for this search domain and the current status of those options.</p>
newtype StopwordOptionsStatus = StopwordOptionsStatus 
  { "Options" :: (StopwordsDocument)
  , "Status" :: (OptionStatus)
  }


-- | <p>Lists stopwords serialized as a JSON document. The document has a single object with one property "stopwords" whose value is an array of strings. The maximum size of a stopwords document is 10 KB. Example: <code>{ "stopwords": ["a", "an", "the", "of"] }</code></p>
newtype StopwordsDocument = StopwordsDocument String


newtype StringCaseMap = StringCaseMap (Map FieldValue FieldValue)


-- | <p>The synonym options configured for this search domain and the current status of those options.</p>
newtype SynonymOptionsStatus = SynonymOptionsStatus 
  { "Options" :: (SynonymsDocument)
  , "Status" :: (OptionStatus)
  }


-- | <p>Maps terms to their synonyms, serialized as a JSON document. The document has a single object with one property "synonyms" whose value is an object mapping terms to their synonyms. Each synonym is a simple string or an array of strings. The maximum size of a stopwords document is 100 KB. Example: <code>{ "synonyms": {"cat": ["feline", "kitten"], "puppy": "dog"} }</code></p>
newtype SynonymsDocument = SynonymsDocument String


-- | <p>Options that define a text field in the search index.</p>
newtype TextOptions = TextOptions 
  { "DefaultValue" :: NullOrUndefined (FieldValue)
  , "FacetEnabled" :: NullOrUndefined (Boolean)
  , "ResultEnabled" :: NullOrUndefined (Boolean)
  , "TextProcessor" :: NullOrUndefined (FieldName)
  }


-- | <p>Options that define a <code>uint</code> field in the search index.</p>
newtype UIntOptions = UIntOptions 
  { "DefaultValue" :: NullOrUndefined (UIntValue)
  }


newtype UIntValue = UIntValue Int


-- | <p>Container for the parameters to the <code><a>UpdateAvailabilityOptions</a></code> operation. Specifies the name of the domain you want to update and the Multi-AZ availability option.</p>
newtype UpdateAvailabilityOptionsRequest = UpdateAvailabilityOptionsRequest 
  { "DomainName" :: (DomainName)
  , "MultiAZ" :: (Boolean)
  }


-- | <p>The result of a <code>UpdateAvailabilityOptions</code> request. Contains the status of the domain's availability options. </p>
newtype UpdateAvailabilityOptionsResponse = UpdateAvailabilityOptionsResponse 
  { "AvailabilityOptions" :: NullOrUndefined (AvailabilityOptionsStatus)
  }


newtype UpdateDefaultSearchFieldRequest = UpdateDefaultSearchFieldRequest 
  { "DomainName" :: (DomainName)
  , "DefaultSearchField" :: (String)
  }


-- | <p>A response message that contains the status of an updated default search field.</p>
newtype UpdateDefaultSearchFieldResponse = UpdateDefaultSearchFieldResponse 
  { "DefaultSearchField" :: (DefaultSearchFieldStatus)
  }


newtype UpdateServiceAccessPoliciesRequest = UpdateServiceAccessPoliciesRequest 
  { "DomainName" :: (DomainName)
  , "AccessPolicies" :: (PolicyDocument)
  }


-- | <p>A response message that contains the status of updated access policies.</p>
newtype UpdateServiceAccessPoliciesResponse = UpdateServiceAccessPoliciesResponse 
  { "AccessPolicies" :: (AccessPoliciesStatus)
  }


newtype UpdateStemmingOptionsRequest = UpdateStemmingOptionsRequest 
  { "DomainName" :: (DomainName)
  , "Stems" :: (StemsDocument)
  }


-- | <p>A response message that contains the status of updated stemming options.</p>
newtype UpdateStemmingOptionsResponse = UpdateStemmingOptionsResponse 
  { "Stems" :: (StemmingOptionsStatus)
  }


newtype UpdateStopwordOptionsRequest = UpdateStopwordOptionsRequest 
  { "DomainName" :: (DomainName)
  , "Stopwords" :: (StopwordsDocument)
  }


-- | <p>A response message that contains the status of updated stopword options.</p>
newtype UpdateStopwordOptionsResponse = UpdateStopwordOptionsResponse 
  { "Stopwords" :: (StopwordOptionsStatus)
  }


newtype UpdateSynonymOptionsRequest = UpdateSynonymOptionsRequest 
  { "DomainName" :: (DomainName)
  , "Synonyms" :: (SynonymsDocument)
  }


-- | <p>A response message that contains the status of updated synonym options.</p>
newtype UpdateSynonymOptionsResponse = UpdateSynonymOptionsResponse 
  { "Synonyms" :: (SynonymOptionsStatus)
  }


newtype UpdateTimestamp = UpdateTimestamp Number
