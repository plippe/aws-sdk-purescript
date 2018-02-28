

-- | <fullname>Amazon CloudSearch Configuration Service</fullname> <p>You use the configuration service to create, configure, and manage search domains. Configuration service requests are submitted using the AWS Query protocol. AWS Query requests are HTTP or HTTPS requests submitted via HTTP GET or POST with a query parameter named Action.</p> <p>The endpoint for configuration service requests is region-specific: cloudsearch.<i>region</i>.amazonaws.com. For example, cloudsearch.us-east-1.amazonaws.com. For a current list of supported regions and endpoints, see <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#cloudsearch_region">Regions and Endpoints</a>.</p>
module AWS.CloudSearch where

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

serviceName = "CloudSearch" :: String


-- | <p>Creates a new search domain.</p>
createDomain :: forall eff. CreateDomainRequest -> Aff (exception :: EXCEPTION | eff) CreateDomainResponse
createDomain = Request.request serviceName "createDomain" 


-- | <p>Configures an <code>IndexField</code> for the search domain. Used to create new fields and modify existing ones. If the field exists, the new configuration replaces the old one. You can configure a maximum of 200 index fields.</p>
defineIndexField :: forall eff. DefineIndexFieldRequest -> Aff (exception :: EXCEPTION | eff) DefineIndexFieldResponse
defineIndexField = Request.request serviceName "defineIndexField" 


-- | <p>Configures a <code>RankExpression</code> for the search domain. Used to create new rank expressions and modify existing ones. If the expression exists, the new configuration replaces the old one. You can configure a maximum of 50 rank expressions.</p>
defineRankExpression :: forall eff. DefineRankExpressionRequest -> Aff (exception :: EXCEPTION | eff) DefineRankExpressionResponse
defineRankExpression = Request.request serviceName "defineRankExpression" 


-- | <p>Permanently deletes a search domain and all of its data.</p>
deleteDomain :: forall eff. DeleteDomainRequest -> Aff (exception :: EXCEPTION | eff) DeleteDomainResponse
deleteDomain = Request.request serviceName "deleteDomain" 


-- | <p>Removes an <code>IndexField</code> from the search domain.</p>
deleteIndexField :: forall eff. DeleteIndexFieldRequest -> Aff (exception :: EXCEPTION | eff) DeleteIndexFieldResponse
deleteIndexField = Request.request serviceName "deleteIndexField" 


-- | <p>Removes a <code>RankExpression</code> from the search domain.</p>
deleteRankExpression :: forall eff. DeleteRankExpressionRequest -> Aff (exception :: EXCEPTION | eff) DeleteRankExpressionResponse
deleteRankExpression = Request.request serviceName "deleteRankExpression" 


-- | <p>Gets the availability options configured for a domain. By default, shows the configuration with any pending changes. Set the <code>Deployed</code> option to <code>true</code> to show the active configuration and exclude pending changes. For more information, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html" target="_blank">Configuring Availability Options</a> in the <i>Amazon CloudSearch Developer Guide</i>.</p>
describeAvailabilityOptions :: forall eff. DescribeAvailabilityOptionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeAvailabilityOptionsResponse
describeAvailabilityOptions = Request.request serviceName "describeAvailabilityOptions" 


-- | <p>Gets the default search field configured for the search domain.</p>
describeDefaultSearchField :: forall eff. DescribeDefaultSearchFieldRequest -> Aff (exception :: EXCEPTION | eff) DescribeDefaultSearchFieldResponse
describeDefaultSearchField = Request.request serviceName "describeDefaultSearchField" 


-- | <p>Gets information about the search domains owned by this account. Can be limited to specific domains. Shows all domains by default.</p>
describeDomains :: forall eff. DescribeDomainsRequest -> Aff (exception :: EXCEPTION | eff) DescribeDomainsResponse
describeDomains = Request.request serviceName "describeDomains" 


-- | <p>Gets information about the index fields configured for the search domain. Can be limited to specific fields by name. Shows all fields by default.</p>
describeIndexFields :: forall eff. DescribeIndexFieldsRequest -> Aff (exception :: EXCEPTION | eff) DescribeIndexFieldsResponse
describeIndexFields = Request.request serviceName "describeIndexFields" 


-- | <p>Gets the rank expressions configured for the search domain. Can be limited to specific rank expressions by name. Shows all rank expressions by default. </p>
describeRankExpressions :: forall eff. DescribeRankExpressionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeRankExpressionsResponse
describeRankExpressions = Request.request serviceName "describeRankExpressions" 


-- | <p>Gets information about the resource-based policies that control access to the domain's document and search services.</p>
describeServiceAccessPolicies :: forall eff. DescribeServiceAccessPoliciesRequest -> Aff (exception :: EXCEPTION | eff) DescribeServiceAccessPoliciesResponse
describeServiceAccessPolicies = Request.request serviceName "describeServiceAccessPolicies" 


-- | <p>Gets the stemming dictionary configured for the search domain.</p>
describeStemmingOptions :: forall eff. DescribeStemmingOptionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeStemmingOptionsResponse
describeStemmingOptions = Request.request serviceName "describeStemmingOptions" 


-- | <p>Gets the stopwords configured for the search domain.</p>
describeStopwordOptions :: forall eff. DescribeStopwordOptionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeStopwordOptionsResponse
describeStopwordOptions = Request.request serviceName "describeStopwordOptions" 


-- | <p>Gets the synonym dictionary configured for the search domain.</p>
describeSynonymOptions :: forall eff. DescribeSynonymOptionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeSynonymOptionsResponse
describeSynonymOptions = Request.request serviceName "describeSynonymOptions" 


-- | <p>Tells the search domain to start indexing its documents using the latest text processing options and <code>IndexFields</code>. This operation must be invoked to make options whose <a>OptionStatus</a> has <code>OptionState</code> of <code>RequiresIndexDocuments</code> visible in search results.</p>
indexDocuments :: forall eff. IndexDocumentsRequest -> Aff (exception :: EXCEPTION | eff) IndexDocumentsResponse
indexDocuments = Request.request serviceName "indexDocuments" 


-- | <p>Configures the availability options for a domain. Enabling the Multi-AZ option expands an Amazon CloudSearch domain to an additional Availability Zone in the same Region to increase fault tolerance in the event of a service disruption. Changes to the Multi-AZ option can take about half an hour to become active. For more information, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html" target="_blank">Configuring Availability Options</a> in the <i>Amazon CloudSearch Developer Guide</i>.</p>
updateAvailabilityOptions :: forall eff. UpdateAvailabilityOptionsRequest -> Aff (exception :: EXCEPTION | eff) UpdateAvailabilityOptionsResponse
updateAvailabilityOptions = Request.request serviceName "updateAvailabilityOptions" 


-- | <p>Configures the default search field for the search domain. The default search field is the text field that is searched when a search request does not specify which fields to search. By default, it is configured to include the contents of all of the domain's text fields. </p>
updateDefaultSearchField :: forall eff. UpdateDefaultSearchFieldRequest -> Aff (exception :: EXCEPTION | eff) UpdateDefaultSearchFieldResponse
updateDefaultSearchField = Request.request serviceName "updateDefaultSearchField" 


-- | <p>Configures the policies that control access to the domain's document and search services. The maximum size of an access policy document is 100 KB.</p>
updateServiceAccessPolicies :: forall eff. UpdateServiceAccessPoliciesRequest -> Aff (exception :: EXCEPTION | eff) UpdateServiceAccessPoliciesResponse
updateServiceAccessPolicies = Request.request serviceName "updateServiceAccessPolicies" 


-- | <p>Configures a stemming dictionary for the search domain. The stemming dictionary is used during indexing and when processing search requests. The maximum size of the stemming dictionary is 500 KB.</p>
updateStemmingOptions :: forall eff. UpdateStemmingOptionsRequest -> Aff (exception :: EXCEPTION | eff) UpdateStemmingOptionsResponse
updateStemmingOptions = Request.request serviceName "updateStemmingOptions" 


-- | <p>Configures stopwords for the search domain. Stopwords are used during indexing and when processing search requests. The maximum size of the stopwords dictionary is 10 KB.</p>
updateStopwordOptions :: forall eff. UpdateStopwordOptionsRequest -> Aff (exception :: EXCEPTION | eff) UpdateStopwordOptionsResponse
updateStopwordOptions = Request.request serviceName "updateStopwordOptions" 


-- | <p>Configures a synonym dictionary for the search domain. The synonym dictionary is used during indexing to configure mappings for terms that occur in text fields. The maximum size of the synonym dictionary is 100 KB. </p>
updateSynonymOptions :: forall eff. UpdateSynonymOptionsRequest -> Aff (exception :: EXCEPTION | eff) UpdateSynonymOptionsResponse
updateSynonymOptions = Request.request serviceName "updateSynonymOptions" 


-- | <p>A <code>PolicyDocument</code> that specifies access policies for the search domain's services, and the current status of those policies.</p>
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


-- | <p>An Amazon Resource Name (ARN). See <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html" target="_blank">Identifiers for IAM Entities</a> in <i>Using AWS Identity and Access Management</i> for more information.</p>
newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _
derive instance repGenericArn :: Generic Arn _
instance showArn :: Show Arn where
  show = genericShow
instance decodeArn :: Decode Arn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArn :: Encode Arn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The status and configuration of the domain's availability options.</p>
newtype AvailabilityOptionsStatus = AvailabilityOptionsStatus 
  { "Options" :: (MultiAZ)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeAvailabilityOptionsStatus :: Newtype AvailabilityOptionsStatus _
derive instance repGenericAvailabilityOptionsStatus :: Generic AvailabilityOptionsStatus _
instance showAvailabilityOptionsStatus :: Show AvailabilityOptionsStatus where
  show = genericShow
instance decodeAvailabilityOptionsStatus :: Decode AvailabilityOptionsStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityOptionsStatus :: Encode AvailabilityOptionsStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error occurred while processing the request.</p>
newtype BaseException = BaseException 
  { "Code" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBaseException :: Newtype BaseException _
derive instance repGenericBaseException :: Generic BaseException _
instance showBaseException :: Show BaseException where
  show = genericShow
instance decodeBaseException :: Decode BaseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBaseException :: Encode BaseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDomainRequest = CreateDomainRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeCreateDomainRequest :: Newtype CreateDomainRequest _
derive instance repGenericCreateDomainRequest :: Generic CreateDomainRequest _
instance showCreateDomainRequest :: Show CreateDomainRequest where
  show = genericShow
instance decodeCreateDomainRequest :: Decode CreateDomainRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDomainRequest :: Encode CreateDomainRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the status of a newly created domain.</p>
newtype CreateDomainResponse = CreateDomainResponse 
  { "DomainStatus" :: NullOrUndefined.NullOrUndefined (DomainStatus)
  }
derive instance newtypeCreateDomainResponse :: Newtype CreateDomainResponse _
derive instance repGenericCreateDomainResponse :: Generic CreateDomainResponse _
instance showCreateDomainResponse :: Show CreateDomainResponse where
  show = genericShow
instance decodeCreateDomainResponse :: Decode CreateDomainResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDomainResponse :: Encode CreateDomainResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value of the <code>DefaultSearchField</code> configured for this search domain and its current status.</p>
newtype DefaultSearchFieldStatus = DefaultSearchFieldStatus 
  { "Options" :: (FieldName)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeDefaultSearchFieldStatus :: Newtype DefaultSearchFieldStatus _
derive instance repGenericDefaultSearchFieldStatus :: Generic DefaultSearchFieldStatus _
instance showDefaultSearchFieldStatus :: Show DefaultSearchFieldStatus where
  show = genericShow
instance decodeDefaultSearchFieldStatus :: Decode DefaultSearchFieldStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultSearchFieldStatus :: Encode DefaultSearchFieldStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DefineIndexFieldRequest = DefineIndexFieldRequest 
  { "DomainName" :: (DomainName)
  , "IndexField" :: (IndexField)
  }
derive instance newtypeDefineIndexFieldRequest :: Newtype DefineIndexFieldRequest _
derive instance repGenericDefineIndexFieldRequest :: Generic DefineIndexFieldRequest _
instance showDefineIndexFieldRequest :: Show DefineIndexFieldRequest where
  show = genericShow
instance decodeDefineIndexFieldRequest :: Decode DefineIndexFieldRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefineIndexFieldRequest :: Encode DefineIndexFieldRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the status of an updated index field.</p>
newtype DefineIndexFieldResponse = DefineIndexFieldResponse 
  { "IndexField" :: (IndexFieldStatus)
  }
derive instance newtypeDefineIndexFieldResponse :: Newtype DefineIndexFieldResponse _
derive instance repGenericDefineIndexFieldResponse :: Generic DefineIndexFieldResponse _
instance showDefineIndexFieldResponse :: Show DefineIndexFieldResponse where
  show = genericShow
instance decodeDefineIndexFieldResponse :: Decode DefineIndexFieldResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefineIndexFieldResponse :: Encode DefineIndexFieldResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DefineRankExpressionRequest = DefineRankExpressionRequest 
  { "DomainName" :: (DomainName)
  , "RankExpression" :: (NamedRankExpression)
  }
derive instance newtypeDefineRankExpressionRequest :: Newtype DefineRankExpressionRequest _
derive instance repGenericDefineRankExpressionRequest :: Generic DefineRankExpressionRequest _
instance showDefineRankExpressionRequest :: Show DefineRankExpressionRequest where
  show = genericShow
instance decodeDefineRankExpressionRequest :: Decode DefineRankExpressionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefineRankExpressionRequest :: Encode DefineRankExpressionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the status of an updated <code>RankExpression</code>.</p>
newtype DefineRankExpressionResponse = DefineRankExpressionResponse 
  { "RankExpression" :: (RankExpressionStatus)
  }
derive instance newtypeDefineRankExpressionResponse :: Newtype DefineRankExpressionResponse _
derive instance repGenericDefineRankExpressionResponse :: Generic DefineRankExpressionResponse _
instance showDefineRankExpressionResponse :: Show DefineRankExpressionResponse where
  show = genericShow
instance decodeDefineRankExpressionResponse :: Decode DefineRankExpressionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefineRankExpressionResponse :: Encode DefineRankExpressionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDomainRequest = DeleteDomainRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDeleteDomainRequest :: Newtype DeleteDomainRequest _
derive instance repGenericDeleteDomainRequest :: Generic DeleteDomainRequest _
instance showDeleteDomainRequest :: Show DeleteDomainRequest where
  show = genericShow
instance decodeDeleteDomainRequest :: Decode DeleteDomainRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDomainRequest :: Encode DeleteDomainRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the status of a newly deleted domain, or no status if the domain has already been completely deleted.</p>
newtype DeleteDomainResponse = DeleteDomainResponse 
  { "DomainStatus" :: NullOrUndefined.NullOrUndefined (DomainStatus)
  }
derive instance newtypeDeleteDomainResponse :: Newtype DeleteDomainResponse _
derive instance repGenericDeleteDomainResponse :: Generic DeleteDomainResponse _
instance showDeleteDomainResponse :: Show DeleteDomainResponse where
  show = genericShow
instance decodeDeleteDomainResponse :: Decode DeleteDomainResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDomainResponse :: Encode DeleteDomainResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteIndexFieldRequest = DeleteIndexFieldRequest 
  { "DomainName" :: (DomainName)
  , "IndexFieldName" :: (FieldName)
  }
derive instance newtypeDeleteIndexFieldRequest :: Newtype DeleteIndexFieldRequest _
derive instance repGenericDeleteIndexFieldRequest :: Generic DeleteIndexFieldRequest _
instance showDeleteIndexFieldRequest :: Show DeleteIndexFieldRequest where
  show = genericShow
instance decodeDeleteIndexFieldRequest :: Decode DeleteIndexFieldRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIndexFieldRequest :: Encode DeleteIndexFieldRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the status of a deleted index field.</p>
newtype DeleteIndexFieldResponse = DeleteIndexFieldResponse 
  { "IndexField" :: (IndexFieldStatus)
  }
derive instance newtypeDeleteIndexFieldResponse :: Newtype DeleteIndexFieldResponse _
derive instance repGenericDeleteIndexFieldResponse :: Generic DeleteIndexFieldResponse _
instance showDeleteIndexFieldResponse :: Show DeleteIndexFieldResponse where
  show = genericShow
instance decodeDeleteIndexFieldResponse :: Decode DeleteIndexFieldResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIndexFieldResponse :: Encode DeleteIndexFieldResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRankExpressionRequest = DeleteRankExpressionRequest 
  { "DomainName" :: (DomainName)
  , "RankName" :: (FieldName)
  }
derive instance newtypeDeleteRankExpressionRequest :: Newtype DeleteRankExpressionRequest _
derive instance repGenericDeleteRankExpressionRequest :: Generic DeleteRankExpressionRequest _
instance showDeleteRankExpressionRequest :: Show DeleteRankExpressionRequest where
  show = genericShow
instance decodeDeleteRankExpressionRequest :: Decode DeleteRankExpressionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRankExpressionRequest :: Encode DeleteRankExpressionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the status of a deleted <code>RankExpression</code>.</p>
newtype DeleteRankExpressionResponse = DeleteRankExpressionResponse 
  { "RankExpression" :: (RankExpressionStatus)
  }
derive instance newtypeDeleteRankExpressionResponse :: Newtype DeleteRankExpressionResponse _
derive instance repGenericDeleteRankExpressionResponse :: Generic DeleteRankExpressionResponse _
instance showDeleteRankExpressionResponse :: Show DeleteRankExpressionResponse where
  show = genericShow
instance decodeDeleteRankExpressionResponse :: Decode DeleteRankExpressionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRankExpressionResponse :: Encode DeleteRankExpressionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the <code><a>DescribeAvailabilityOptions</a></code> operation. Specifies the name of the domain you want to describe. To show the active configuration and exclude any pending changes, set the Deployed option to <code>true</code>.</p>
newtype DescribeAvailabilityOptionsRequest = DescribeAvailabilityOptionsRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDescribeAvailabilityOptionsRequest :: Newtype DescribeAvailabilityOptionsRequest _
derive instance repGenericDescribeAvailabilityOptionsRequest :: Generic DescribeAvailabilityOptionsRequest _
instance showDescribeAvailabilityOptionsRequest :: Show DescribeAvailabilityOptionsRequest where
  show = genericShow
instance decodeDescribeAvailabilityOptionsRequest :: Decode DescribeAvailabilityOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAvailabilityOptionsRequest :: Encode DescribeAvailabilityOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a <code>DescribeAvailabilityOptions</code> request. Indicates whether or not the Multi-AZ option is enabled for the domain specified in the request. </p>
newtype DescribeAvailabilityOptionsResponse = DescribeAvailabilityOptionsResponse 
  { "AvailabilityOptions" :: NullOrUndefined.NullOrUndefined (AvailabilityOptionsStatus)
  }
derive instance newtypeDescribeAvailabilityOptionsResponse :: Newtype DescribeAvailabilityOptionsResponse _
derive instance repGenericDescribeAvailabilityOptionsResponse :: Generic DescribeAvailabilityOptionsResponse _
instance showDescribeAvailabilityOptionsResponse :: Show DescribeAvailabilityOptionsResponse where
  show = genericShow
instance decodeDescribeAvailabilityOptionsResponse :: Decode DescribeAvailabilityOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAvailabilityOptionsResponse :: Encode DescribeAvailabilityOptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDefaultSearchFieldRequest = DescribeDefaultSearchFieldRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDescribeDefaultSearchFieldRequest :: Newtype DescribeDefaultSearchFieldRequest _
derive instance repGenericDescribeDefaultSearchFieldRequest :: Generic DescribeDefaultSearchFieldRequest _
instance showDescribeDefaultSearchFieldRequest :: Show DescribeDefaultSearchFieldRequest where
  show = genericShow
instance decodeDescribeDefaultSearchFieldRequest :: Decode DescribeDefaultSearchFieldRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDefaultSearchFieldRequest :: Encode DescribeDefaultSearchFieldRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the default search field for a search domain.</p>
newtype DescribeDefaultSearchFieldResponse = DescribeDefaultSearchFieldResponse 
  { "DefaultSearchField" :: (DefaultSearchFieldStatus)
  }
derive instance newtypeDescribeDefaultSearchFieldResponse :: Newtype DescribeDefaultSearchFieldResponse _
derive instance repGenericDescribeDefaultSearchFieldResponse :: Generic DescribeDefaultSearchFieldResponse _
instance showDescribeDefaultSearchFieldResponse :: Show DescribeDefaultSearchFieldResponse where
  show = genericShow
instance decodeDescribeDefaultSearchFieldResponse :: Decode DescribeDefaultSearchFieldResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDefaultSearchFieldResponse :: Encode DescribeDefaultSearchFieldResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDomainsRequest = DescribeDomainsRequest 
  { "DomainNames" :: NullOrUndefined.NullOrUndefined (DomainNameList)
  }
derive instance newtypeDescribeDomainsRequest :: Newtype DescribeDomainsRequest _
derive instance repGenericDescribeDomainsRequest :: Generic DescribeDomainsRequest _
instance showDescribeDomainsRequest :: Show DescribeDomainsRequest where
  show = genericShow
instance decodeDescribeDomainsRequest :: Decode DescribeDomainsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDomainsRequest :: Encode DescribeDomainsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the status of one or more domains.</p>
newtype DescribeDomainsResponse = DescribeDomainsResponse 
  { "DomainStatusList" :: (DomainStatusList)
  }
derive instance newtypeDescribeDomainsResponse :: Newtype DescribeDomainsResponse _
derive instance repGenericDescribeDomainsResponse :: Generic DescribeDomainsResponse _
instance showDescribeDomainsResponse :: Show DescribeDomainsResponse where
  show = genericShow
instance decodeDescribeDomainsResponse :: Decode DescribeDomainsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDomainsResponse :: Encode DescribeDomainsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeIndexFieldsRequest = DescribeIndexFieldsRequest 
  { "DomainName" :: (DomainName)
  , "FieldNames" :: NullOrUndefined.NullOrUndefined (FieldNameList)
  }
derive instance newtypeDescribeIndexFieldsRequest :: Newtype DescribeIndexFieldsRequest _
derive instance repGenericDescribeIndexFieldsRequest :: Generic DescribeIndexFieldsRequest _
instance showDescribeIndexFieldsRequest :: Show DescribeIndexFieldsRequest where
  show = genericShow
instance decodeDescribeIndexFieldsRequest :: Decode DescribeIndexFieldsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeIndexFieldsRequest :: Encode DescribeIndexFieldsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the index fields for a search domain.</p>
newtype DescribeIndexFieldsResponse = DescribeIndexFieldsResponse 
  { "IndexFields" :: (IndexFieldStatusList)
  }
derive instance newtypeDescribeIndexFieldsResponse :: Newtype DescribeIndexFieldsResponse _
derive instance repGenericDescribeIndexFieldsResponse :: Generic DescribeIndexFieldsResponse _
instance showDescribeIndexFieldsResponse :: Show DescribeIndexFieldsResponse where
  show = genericShow
instance decodeDescribeIndexFieldsResponse :: Decode DescribeIndexFieldsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeIndexFieldsResponse :: Encode DescribeIndexFieldsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeRankExpressionsRequest = DescribeRankExpressionsRequest 
  { "DomainName" :: (DomainName)
  , "RankNames" :: NullOrUndefined.NullOrUndefined (FieldNameList)
  }
derive instance newtypeDescribeRankExpressionsRequest :: Newtype DescribeRankExpressionsRequest _
derive instance repGenericDescribeRankExpressionsRequest :: Generic DescribeRankExpressionsRequest _
instance showDescribeRankExpressionsRequest :: Show DescribeRankExpressionsRequest where
  show = genericShow
instance decodeDescribeRankExpressionsRequest :: Decode DescribeRankExpressionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRankExpressionsRequest :: Encode DescribeRankExpressionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the rank expressions for a search domain.</p>
newtype DescribeRankExpressionsResponse = DescribeRankExpressionsResponse 
  { "RankExpressions" :: (RankExpressionStatusList)
  }
derive instance newtypeDescribeRankExpressionsResponse :: Newtype DescribeRankExpressionsResponse _
derive instance repGenericDescribeRankExpressionsResponse :: Generic DescribeRankExpressionsResponse _
instance showDescribeRankExpressionsResponse :: Show DescribeRankExpressionsResponse where
  show = genericShow
instance decodeDescribeRankExpressionsResponse :: Decode DescribeRankExpressionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRankExpressionsResponse :: Encode DescribeRankExpressionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeServiceAccessPoliciesRequest = DescribeServiceAccessPoliciesRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDescribeServiceAccessPoliciesRequest :: Newtype DescribeServiceAccessPoliciesRequest _
derive instance repGenericDescribeServiceAccessPoliciesRequest :: Generic DescribeServiceAccessPoliciesRequest _
instance showDescribeServiceAccessPoliciesRequest :: Show DescribeServiceAccessPoliciesRequest where
  show = genericShow
instance decodeDescribeServiceAccessPoliciesRequest :: Decode DescribeServiceAccessPoliciesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeServiceAccessPoliciesRequest :: Encode DescribeServiceAccessPoliciesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the access policies for a domain.</p>
newtype DescribeServiceAccessPoliciesResponse = DescribeServiceAccessPoliciesResponse 
  { "AccessPolicies" :: (AccessPoliciesStatus)
  }
derive instance newtypeDescribeServiceAccessPoliciesResponse :: Newtype DescribeServiceAccessPoliciesResponse _
derive instance repGenericDescribeServiceAccessPoliciesResponse :: Generic DescribeServiceAccessPoliciesResponse _
instance showDescribeServiceAccessPoliciesResponse :: Show DescribeServiceAccessPoliciesResponse where
  show = genericShow
instance decodeDescribeServiceAccessPoliciesResponse :: Decode DescribeServiceAccessPoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeServiceAccessPoliciesResponse :: Encode DescribeServiceAccessPoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStemmingOptionsRequest = DescribeStemmingOptionsRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDescribeStemmingOptionsRequest :: Newtype DescribeStemmingOptionsRequest _
derive instance repGenericDescribeStemmingOptionsRequest :: Generic DescribeStemmingOptionsRequest _
instance showDescribeStemmingOptionsRequest :: Show DescribeStemmingOptionsRequest where
  show = genericShow
instance decodeDescribeStemmingOptionsRequest :: Decode DescribeStemmingOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStemmingOptionsRequest :: Encode DescribeStemmingOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the stemming options for a search domain.</p>
newtype DescribeStemmingOptionsResponse = DescribeStemmingOptionsResponse 
  { "Stems" :: (StemmingOptionsStatus)
  }
derive instance newtypeDescribeStemmingOptionsResponse :: Newtype DescribeStemmingOptionsResponse _
derive instance repGenericDescribeStemmingOptionsResponse :: Generic DescribeStemmingOptionsResponse _
instance showDescribeStemmingOptionsResponse :: Show DescribeStemmingOptionsResponse where
  show = genericShow
instance decodeDescribeStemmingOptionsResponse :: Decode DescribeStemmingOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStemmingOptionsResponse :: Encode DescribeStemmingOptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStopwordOptionsRequest = DescribeStopwordOptionsRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDescribeStopwordOptionsRequest :: Newtype DescribeStopwordOptionsRequest _
derive instance repGenericDescribeStopwordOptionsRequest :: Generic DescribeStopwordOptionsRequest _
instance showDescribeStopwordOptionsRequest :: Show DescribeStopwordOptionsRequest where
  show = genericShow
instance decodeDescribeStopwordOptionsRequest :: Decode DescribeStopwordOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStopwordOptionsRequest :: Encode DescribeStopwordOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the stopword options for a search domain.</p>
newtype DescribeStopwordOptionsResponse = DescribeStopwordOptionsResponse 
  { "Stopwords" :: (StopwordOptionsStatus)
  }
derive instance newtypeDescribeStopwordOptionsResponse :: Newtype DescribeStopwordOptionsResponse _
derive instance repGenericDescribeStopwordOptionsResponse :: Generic DescribeStopwordOptionsResponse _
instance showDescribeStopwordOptionsResponse :: Show DescribeStopwordOptionsResponse where
  show = genericShow
instance decodeDescribeStopwordOptionsResponse :: Decode DescribeStopwordOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStopwordOptionsResponse :: Encode DescribeStopwordOptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeSynonymOptionsRequest = DescribeSynonymOptionsRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDescribeSynonymOptionsRequest :: Newtype DescribeSynonymOptionsRequest _
derive instance repGenericDescribeSynonymOptionsRequest :: Generic DescribeSynonymOptionsRequest _
instance showDescribeSynonymOptionsRequest :: Show DescribeSynonymOptionsRequest where
  show = genericShow
instance decodeDescribeSynonymOptionsRequest :: Decode DescribeSynonymOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSynonymOptionsRequest :: Encode DescribeSynonymOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the synonym options for a search domain.</p>
newtype DescribeSynonymOptionsResponse = DescribeSynonymOptionsResponse 
  { "Synonyms" :: (SynonymOptionsStatus)
  }
derive instance newtypeDescribeSynonymOptionsResponse :: Newtype DescribeSynonymOptionsResponse _
derive instance repGenericDescribeSynonymOptionsResponse :: Generic DescribeSynonymOptionsResponse _
instance showDescribeSynonymOptionsResponse :: Show DescribeSynonymOptionsResponse where
  show = genericShow
instance decodeDescribeSynonymOptionsResponse :: Decode DescribeSynonymOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSynonymOptionsResponse :: Encode DescribeSynonymOptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because it attempted an operation which is not enabled.</p>
newtype DisabledOperationException = DisabledOperationException Types.NoArguments
derive instance newtypeDisabledOperationException :: Newtype DisabledOperationException _
derive instance repGenericDisabledOperationException :: Generic DisabledOperationException _
instance showDisabledOperationException :: Show DisabledOperationException where
  show = genericShow
instance decodeDisabledOperationException :: Decode DisabledOperationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisabledOperationException :: Encode DisabledOperationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentCount = DocumentCount Number
derive instance newtypeDocumentCount :: Newtype DocumentCount _
derive instance repGenericDocumentCount :: Generic DocumentCount _
instance showDocumentCount :: Show DocumentCount where
  show = genericShow
instance decodeDocumentCount :: Decode DocumentCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentCount :: Encode DocumentCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An internally generated unique identifier for a domain.</p>
newtype DomainId = DomainId String
derive instance newtypeDomainId :: Newtype DomainId _
derive instance repGenericDomainId :: Generic DomainId _
instance showDomainId :: Show DomainId where
  show = genericShow
instance decodeDomainId :: Decode DomainId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainId :: Encode DomainId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A string that represents the name of a domain. Domain names must be unique across the domains owned by an account within an AWS region. Domain names must start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and underscores are not allowed.</p>
newtype DomainName = DomainName String
derive instance newtypeDomainName :: Newtype DomainName _
derive instance repGenericDomainName :: Generic DomainName _
instance showDomainName :: Show DomainName where
  show = genericShow
instance decodeDomainName :: Decode DomainName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainName :: Encode DomainName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of domain names.</p>
newtype DomainNameList = DomainNameList (Array DomainName)
derive instance newtypeDomainNameList :: Newtype DomainNameList _
derive instance repGenericDomainNameList :: Generic DomainNameList _
instance showDomainNameList :: Show DomainNameList where
  show = genericShow
instance decodeDomainNameList :: Decode DomainNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainNameList :: Encode DomainNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The current status of the search domain.</p>
newtype DomainStatus = DomainStatus 
  { "DomainId" :: (DomainId)
  , "DomainName" :: (DomainName)
  , "Created" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Deleted" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "NumSearchableDocs" :: NullOrUndefined.NullOrUndefined (DocumentCount)
  , "DocService" :: NullOrUndefined.NullOrUndefined (ServiceEndpoint)
  , "SearchService" :: NullOrUndefined.NullOrUndefined (ServiceEndpoint)
  , "RequiresIndexDocuments" :: (Boolean)
  , "Processing" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "SearchInstanceType" :: NullOrUndefined.NullOrUndefined (SearchInstanceType)
  , "SearchPartitionCount" :: NullOrUndefined.NullOrUndefined (PartitionCount)
  , "SearchInstanceCount" :: NullOrUndefined.NullOrUndefined (InstanceCount)
  }
derive instance newtypeDomainStatus :: Newtype DomainStatus _
derive instance repGenericDomainStatus :: Generic DomainStatus _
instance showDomainStatus :: Show DomainStatus where
  show = genericShow
instance decodeDomainStatus :: Decode DomainStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainStatus :: Encode DomainStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The current status of all of your search domains.</p>
newtype DomainStatusList = DomainStatusList (Array DomainStatus)
derive instance newtypeDomainStatusList :: Newtype DomainStatusList _
derive instance repGenericDomainStatusList :: Generic DomainStatusList _
instance showDomainStatusList :: Show DomainStatusList where
  show = genericShow
instance decodeDomainStatusList :: Decode DomainStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainStatusList :: Encode DomainStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A machine-parsable string error or warning code.</p>
newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _
derive instance repGenericErrorCode :: Generic ErrorCode _
instance showErrorCode :: Show ErrorCode where
  show = genericShow
instance decodeErrorCode :: Decode ErrorCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorCode :: Encode ErrorCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A human-readable string error or warning message.</p>
newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A string that represents the name of an index field. Field names must begin with a letter and can contain the following characters: a-z (lowercase), 0-9, and _ (underscore). Uppercase letters and hyphens are not allowed. The names "body", "docid", and "text_relevance" are reserved and cannot be specified as field or rank expression names.</p>
newtype FieldName = FieldName String
derive instance newtypeFieldName :: Newtype FieldName _
derive instance repGenericFieldName :: Generic FieldName _
instance showFieldName :: Show FieldName where
  show = genericShow
instance decodeFieldName :: Decode FieldName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFieldName :: Encode FieldName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FieldNameList = FieldNameList (Array FieldName)
derive instance newtypeFieldNameList :: Newtype FieldNameList _
derive instance repGenericFieldNameList :: Generic FieldNameList _
instance showFieldNameList :: Show FieldNameList where
  show = genericShow
instance decodeFieldNameList :: Decode FieldNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFieldNameList :: Encode FieldNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value of a field or source document attribute.</p>
newtype FieldValue = FieldValue String
derive instance newtypeFieldValue :: Newtype FieldValue _
derive instance repGenericFieldValue :: Generic FieldValue _
instance showFieldValue :: Show FieldValue where
  show = genericShow
instance decodeFieldValue :: Decode FieldValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFieldValue :: Encode FieldValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IndexDocumentsRequest = IndexDocumentsRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeIndexDocumentsRequest :: Newtype IndexDocumentsRequest _
derive instance repGenericIndexDocumentsRequest :: Generic IndexDocumentsRequest _
instance showIndexDocumentsRequest :: Show IndexDocumentsRequest where
  show = genericShow
instance decodeIndexDocumentsRequest :: Decode IndexDocumentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexDocumentsRequest :: Encode IndexDocumentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of an <code>IndexDocuments</code> action.</p>
newtype IndexDocumentsResponse = IndexDocumentsResponse 
  { "FieldNames" :: NullOrUndefined.NullOrUndefined (FieldNameList)
  }
derive instance newtypeIndexDocumentsResponse :: Newtype IndexDocumentsResponse _
derive instance repGenericIndexDocumentsResponse :: Generic IndexDocumentsResponse _
instance showIndexDocumentsResponse :: Show IndexDocumentsResponse where
  show = genericShow
instance decodeIndexDocumentsResponse :: Decode IndexDocumentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexDocumentsResponse :: Encode IndexDocumentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines a field in the index, including its name, type, and the source of its data. The <code>IndexFieldType</code> indicates which of the options will be present. It is invalid to specify options for a type other than the <code>IndexFieldType</code>.</p>
newtype IndexField = IndexField 
  { "IndexFieldName" :: (FieldName)
  , "IndexFieldType" :: (IndexFieldType)
  , "UIntOptions" :: NullOrUndefined.NullOrUndefined (UIntOptions)
  , "LiteralOptions" :: NullOrUndefined.NullOrUndefined (LiteralOptions)
  , "TextOptions" :: NullOrUndefined.NullOrUndefined (TextOptions)
  , "SourceAttributes" :: NullOrUndefined.NullOrUndefined (SourceAttributeList)
  }
derive instance newtypeIndexField :: Newtype IndexField _
derive instance repGenericIndexField :: Generic IndexField _
instance showIndexField :: Show IndexField where
  show = genericShow
instance decodeIndexField :: Decode IndexField where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexField :: Encode IndexField where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value of an <code>IndexField</code> and its current status.</p>
newtype IndexFieldStatus = IndexFieldStatus 
  { "Options" :: (IndexField)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeIndexFieldStatus :: Newtype IndexFieldStatus _
derive instance repGenericIndexFieldStatus :: Generic IndexFieldStatus _
instance showIndexFieldStatus :: Show IndexFieldStatus where
  show = genericShow
instance decodeIndexFieldStatus :: Decode IndexFieldStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexFieldStatus :: Encode IndexFieldStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IndexFieldStatusList = IndexFieldStatusList (Array IndexFieldStatus)
derive instance newtypeIndexFieldStatusList :: Newtype IndexFieldStatusList _
derive instance repGenericIndexFieldStatusList :: Generic IndexFieldStatusList _
instance showIndexFieldStatusList :: Show IndexFieldStatusList where
  show = genericShow
instance decodeIndexFieldStatusList :: Decode IndexFieldStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexFieldStatusList :: Encode IndexFieldStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The type of <code>IndexField</code>.</p>
newtype IndexFieldType = IndexFieldType String
derive instance newtypeIndexFieldType :: Newtype IndexFieldType _
derive instance repGenericIndexFieldType :: Generic IndexFieldType _
instance showIndexFieldType :: Show IndexFieldType where
  show = genericShow
instance decodeIndexFieldType :: Decode IndexFieldType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexFieldType :: Encode IndexFieldType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceCount = InstanceCount Int
derive instance newtypeInstanceCount :: Newtype InstanceCount _
derive instance repGenericInstanceCount :: Generic InstanceCount _
instance showInstanceCount :: Show InstanceCount where
  show = genericShow
instance decodeInstanceCount :: Decode InstanceCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceCount :: Encode InstanceCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An internal error occurred while processing the request. If this problem persists, report an issue from the <a href="http://status.aws.amazon.com/">Service Health Dashboard</a>.</p>
newtype InternalException = InternalException Types.NoArguments
derive instance newtypeInternalException :: Newtype InternalException _
derive instance repGenericInternalException :: Generic InternalException _
instance showInternalException :: Show InternalException where
  show = genericShow
instance decodeInternalException :: Decode InternalException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalException :: Encode InternalException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because it specified an invalid type definition.</p>
newtype InvalidTypeException = InvalidTypeException Types.NoArguments
derive instance newtypeInvalidTypeException :: Newtype InvalidTypeException _
derive instance repGenericInvalidTypeException :: Generic InvalidTypeException _
instance showInvalidTypeException :: Show InvalidTypeException where
  show = genericShow
instance decodeInvalidTypeException :: Decode InvalidTypeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTypeException :: Encode InvalidTypeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An <a href="http://tools.ietf.org/html/rfc4646">IETF RFC 4646</a> language code. Only the primary language is considered. English (en) is currently the only supported language.</p>
newtype Language = Language String
derive instance newtypeLanguage :: Newtype Language _
derive instance repGenericLanguage :: Generic Language _
instance showLanguage :: Show Language where
  show = genericShow
instance decodeLanguage :: Decode Language where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLanguage :: Encode Language where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because a resource limit has already been met.</p>
newtype LimitExceededException = LimitExceededException Types.NoArguments
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Options that define a literal field in the search index.</p>
newtype LiteralOptions = LiteralOptions 
  { "DefaultValue" :: NullOrUndefined.NullOrUndefined (FieldValue)
  , "SearchEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "FacetEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ResultEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeLiteralOptions :: Newtype LiteralOptions _
derive instance repGenericLiteralOptions :: Generic LiteralOptions _
instance showLiteralOptions :: Show LiteralOptions where
  show = genericShow
instance decodeLiteralOptions :: Decode LiteralOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLiteralOptions :: Encode LiteralOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MultiAZ = MultiAZ Boolean
derive instance newtypeMultiAZ :: Newtype MultiAZ _
derive instance repGenericMultiAZ :: Generic MultiAZ _
instance showMultiAZ :: Show MultiAZ where
  show = genericShow
instance decodeMultiAZ :: Decode MultiAZ where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMultiAZ :: Encode MultiAZ where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A named expression that can be evaluated at search time and used for ranking or thresholding in a search query. </p>
newtype NamedRankExpression = NamedRankExpression 
  { "RankName" :: (FieldName)
  , "RankExpression" :: (RankExpression)
  }
derive instance newtypeNamedRankExpression :: Newtype NamedRankExpression _
derive instance repGenericNamedRankExpression :: Generic NamedRankExpression _
instance showNamedRankExpression :: Show NamedRankExpression where
  show = genericShow
instance decodeNamedRankExpression :: Decode NamedRankExpression where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNamedRankExpression :: Encode NamedRankExpression where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The state of processing a change to an option.</p>
newtype OptionState = OptionState String
derive instance newtypeOptionState :: Newtype OptionState _
derive instance repGenericOptionState :: Generic OptionState _
instance showOptionState :: Show OptionState where
  show = genericShow
instance decodeOptionState :: Decode OptionState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionState :: Encode OptionState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The status of an option, including when it was last updated and whether it is actively in use for searches.</p>
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


newtype PartitionCount = PartitionCount Int
derive instance newtypePartitionCount :: Newtype PartitionCount _
derive instance repGenericPartitionCount :: Generic PartitionCount _
instance showPartitionCount :: Show PartitionCount where
  show = genericShow
instance decodePartitionCount :: Decode PartitionCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartitionCount :: Encode PartitionCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An IAM access policy as described in <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?AccessPolicyLanguage.html" target="_blank">The Access Policy Language</a> in <i>Using AWS Identity and Access Management</i>. The maximum size of an access policy document is 100 KB.</p> <p>Example: <code>{"Statement": [{"Effect":"Allow", "Action": "*", "Resource": "arn:aws:cs:us-east-1:1234567890:search/movies", "Condition": { "IpAddress": { "aws:SourceIp": ["203.0.113.1/32"] } }}, {"Effect":"Allow", "Action": "*", "Resource": "arn:aws:cs:us-east-1:1234567890:documents/movies", "Condition": { "IpAddress": { "aws:SourceIp": ["203.0.113.1/32"] } }} ] }</code></p>
newtype PolicyDocument = PolicyDocument String
derive instance newtypePolicyDocument :: Newtype PolicyDocument _
derive instance repGenericPolicyDocument :: Generic PolicyDocument _
instance showPolicyDocument :: Show PolicyDocument where
  show = genericShow
instance decodePolicyDocument :: Decode PolicyDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyDocument :: Encode PolicyDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The current status of the rank expression.</p>
newtype RankExpression = RankExpression String
derive instance newtypeRankExpression :: Newtype RankExpression _
derive instance repGenericRankExpression :: Generic RankExpression _
instance showRankExpression :: Show RankExpression where
  show = genericShow
instance decodeRankExpression :: Decode RankExpression where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRankExpression :: Encode RankExpression where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value of a <code>RankExpression</code> and its current status.</p>
newtype RankExpressionStatus = RankExpressionStatus 
  { "Options" :: (NamedRankExpression)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeRankExpressionStatus :: Newtype RankExpressionStatus _
derive instance repGenericRankExpressionStatus :: Generic RankExpressionStatus _
instance showRankExpressionStatus :: Show RankExpressionStatus where
  show = genericShow
instance decodeRankExpressionStatus :: Decode RankExpressionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRankExpressionStatus :: Encode RankExpressionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RankExpressionStatusList = RankExpressionStatusList (Array RankExpressionStatus)
derive instance newtypeRankExpressionStatusList :: Newtype RankExpressionStatusList _
derive instance repGenericRankExpressionStatusList :: Generic RankExpressionStatusList _
instance showRankExpressionStatusList :: Show RankExpressionStatusList where
  show = genericShow
instance decodeRankExpressionStatusList :: Decode RankExpressionStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRankExpressionStatusList :: Encode RankExpressionStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because it attempted to reference a resource that does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException Types.NoArguments
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchInstanceType = SearchInstanceType String
derive instance newtypeSearchInstanceType :: Newtype SearchInstanceType _
derive instance repGenericSearchInstanceType :: Generic SearchInstanceType _
instance showSearchInstanceType :: Show SearchInstanceType where
  show = genericShow
instance decodeSearchInstanceType :: Decode SearchInstanceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchInstanceType :: Encode SearchInstanceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The endpoint to which service requests can be submitted, including the actual URL prefix for sending requests and the Amazon Resource Name (ARN) so the endpoint can be referenced in other API calls such as <a>UpdateServiceAccessPolicies</a>.</p>
newtype ServiceEndpoint = ServiceEndpoint 
  { "Arn" :: NullOrUndefined.NullOrUndefined (Arn)
  , "Endpoint" :: NullOrUndefined.NullOrUndefined (ServiceUrl)
  }
derive instance newtypeServiceEndpoint :: Newtype ServiceEndpoint _
derive instance repGenericServiceEndpoint :: Generic ServiceEndpoint _
instance showServiceEndpoint :: Show ServiceEndpoint where
  show = genericShow
instance decodeServiceEndpoint :: Decode ServiceEndpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceEndpoint :: Encode ServiceEndpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The URL (including /version/pathPrefix) to which service requests can be submitted.</p>
newtype ServiceUrl = ServiceUrl String
derive instance newtypeServiceUrl :: Newtype ServiceUrl _
derive instance repGenericServiceUrl :: Generic ServiceUrl _
instance showServiceUrl :: Show ServiceUrl where
  show = genericShow
instance decodeServiceUrl :: Decode ServiceUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUrl :: Encode ServiceUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Identifies the source data for an index field. An optional data transformation can be applied to the source data when populating the index field. By default, the value of the source attribute is copied to the index field.</p>
newtype SourceAttribute = SourceAttribute 
  { "SourceDataFunction" :: (SourceDataFunction)
  , "SourceDataCopy" :: NullOrUndefined.NullOrUndefined (SourceData)
  , "SourceDataTrimTitle" :: NullOrUndefined.NullOrUndefined (SourceDataTrimTitle)
  , "SourceDataMap" :: NullOrUndefined.NullOrUndefined (SourceDataMap)
  }
derive instance newtypeSourceAttribute :: Newtype SourceAttribute _
derive instance repGenericSourceAttribute :: Generic SourceAttribute _
instance showSourceAttribute :: Show SourceAttribute where
  show = genericShow
instance decodeSourceAttribute :: Decode SourceAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceAttribute :: Encode SourceAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SourceAttributeList = SourceAttributeList (Array SourceAttribute)
derive instance newtypeSourceAttributeList :: Newtype SourceAttributeList _
derive instance repGenericSourceAttributeList :: Generic SourceAttributeList _
instance showSourceAttributeList :: Show SourceAttributeList where
  show = genericShow
instance decodeSourceAttributeList :: Decode SourceAttributeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceAttributeList :: Encode SourceAttributeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The source attribute name and an optional default value to use if a document doesn't have an attribute of that name.</p>
newtype SourceData = SourceData 
  { "SourceName" :: (FieldName)
  , "DefaultValue" :: NullOrUndefined.NullOrUndefined (FieldValue)
  }
derive instance newtypeSourceData :: Newtype SourceData _
derive instance repGenericSourceData :: Generic SourceData _
instance showSourceData :: Show SourceData where
  show = genericShow
instance decodeSourceData :: Decode SourceData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceData :: Encode SourceData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SourceDataFunction = SourceDataFunction String
derive instance newtypeSourceDataFunction :: Newtype SourceDataFunction _
derive instance repGenericSourceDataFunction :: Generic SourceDataFunction _
instance showSourceDataFunction :: Show SourceDataFunction where
  show = genericShow
instance decodeSourceDataFunction :: Decode SourceDataFunction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceDataFunction :: Encode SourceDataFunction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies how to map source attribute values to custom values when populating an <code>IndexField</code>.</p>
newtype SourceDataMap = SourceDataMap 
  { "SourceName" :: (FieldName)
  , "DefaultValue" :: NullOrUndefined.NullOrUndefined (FieldValue)
  , "Cases" :: NullOrUndefined.NullOrUndefined (StringCaseMap)
  }
derive instance newtypeSourceDataMap :: Newtype SourceDataMap _
derive instance repGenericSourceDataMap :: Generic SourceDataMap _
instance showSourceDataMap :: Show SourceDataMap where
  show = genericShow
instance decodeSourceDataMap :: Decode SourceDataMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceDataMap :: Encode SourceDataMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies how to trim common words from the beginning of a field to enable title sorting by that field.</p>
newtype SourceDataTrimTitle = SourceDataTrimTitle 
  { "SourceName" :: (FieldName)
  , "DefaultValue" :: NullOrUndefined.NullOrUndefined (FieldValue)
  , "Separator" :: NullOrUndefined.NullOrUndefined (String)
  , "Language" :: NullOrUndefined.NullOrUndefined (Language)
  }
derive instance newtypeSourceDataTrimTitle :: Newtype SourceDataTrimTitle _
derive instance repGenericSourceDataTrimTitle :: Generic SourceDataTrimTitle _
instance showSourceDataTrimTitle :: Show SourceDataTrimTitle where
  show = genericShow
instance decodeSourceDataTrimTitle :: Decode SourceDataTrimTitle where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceDataTrimTitle :: Encode SourceDataTrimTitle where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The stemming options configured for this search domain and the current status of those options.</p>
newtype StemmingOptionsStatus = StemmingOptionsStatus 
  { "Options" :: (StemsDocument)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeStemmingOptionsStatus :: Newtype StemmingOptionsStatus _
derive instance repGenericStemmingOptionsStatus :: Generic StemmingOptionsStatus _
instance showStemmingOptionsStatus :: Show StemmingOptionsStatus where
  show = genericShow
instance decodeStemmingOptionsStatus :: Decode StemmingOptionsStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStemmingOptionsStatus :: Encode StemmingOptionsStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Maps terms to their stems, serialized as a JSON document. The document has a single object with one property "stems" whose value is an object mapping terms to their stems. The maximum size of a stemming document is 500 KB. Example: <code>{ "stems": {"people": "person", "walking": "walk"} }</code></p>
newtype StemsDocument = StemsDocument String
derive instance newtypeStemsDocument :: Newtype StemsDocument _
derive instance repGenericStemsDocument :: Generic StemsDocument _
instance showStemsDocument :: Show StemsDocument where
  show = genericShow
instance decodeStemsDocument :: Decode StemsDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStemsDocument :: Encode StemsDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The stopword options configured for this search domain and the current status of those options.</p>
newtype StopwordOptionsStatus = StopwordOptionsStatus 
  { "Options" :: (StopwordsDocument)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeStopwordOptionsStatus :: Newtype StopwordOptionsStatus _
derive instance repGenericStopwordOptionsStatus :: Generic StopwordOptionsStatus _
instance showStopwordOptionsStatus :: Show StopwordOptionsStatus where
  show = genericShow
instance decodeStopwordOptionsStatus :: Decode StopwordOptionsStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopwordOptionsStatus :: Encode StopwordOptionsStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lists stopwords serialized as a JSON document. The document has a single object with one property "stopwords" whose value is an array of strings. The maximum size of a stopwords document is 10 KB. Example: <code>{ "stopwords": ["a", "an", "the", "of"] }</code></p>
newtype StopwordsDocument = StopwordsDocument String
derive instance newtypeStopwordsDocument :: Newtype StopwordsDocument _
derive instance repGenericStopwordsDocument :: Generic StopwordsDocument _
instance showStopwordsDocument :: Show StopwordsDocument where
  show = genericShow
instance decodeStopwordsDocument :: Decode StopwordsDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopwordsDocument :: Encode StopwordsDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringCaseMap = StringCaseMap (StrMap.StrMap FieldValue)
derive instance newtypeStringCaseMap :: Newtype StringCaseMap _
derive instance repGenericStringCaseMap :: Generic StringCaseMap _
instance showStringCaseMap :: Show StringCaseMap where
  show = genericShow
instance decodeStringCaseMap :: Decode StringCaseMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringCaseMap :: Encode StringCaseMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The synonym options configured for this search domain and the current status of those options.</p>
newtype SynonymOptionsStatus = SynonymOptionsStatus 
  { "Options" :: (SynonymsDocument)
  , "Status" :: (OptionStatus)
  }
derive instance newtypeSynonymOptionsStatus :: Newtype SynonymOptionsStatus _
derive instance repGenericSynonymOptionsStatus :: Generic SynonymOptionsStatus _
instance showSynonymOptionsStatus :: Show SynonymOptionsStatus where
  show = genericShow
instance decodeSynonymOptionsStatus :: Decode SynonymOptionsStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSynonymOptionsStatus :: Encode SynonymOptionsStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Maps terms to their synonyms, serialized as a JSON document. The document has a single object with one property "synonyms" whose value is an object mapping terms to their synonyms. Each synonym is a simple string or an array of strings. The maximum size of a stopwords document is 100 KB. Example: <code>{ "synonyms": {"cat": ["feline", "kitten"], "puppy": "dog"} }</code></p>
newtype SynonymsDocument = SynonymsDocument String
derive instance newtypeSynonymsDocument :: Newtype SynonymsDocument _
derive instance repGenericSynonymsDocument :: Generic SynonymsDocument _
instance showSynonymsDocument :: Show SynonymsDocument where
  show = genericShow
instance decodeSynonymsDocument :: Decode SynonymsDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSynonymsDocument :: Encode SynonymsDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Options that define a text field in the search index.</p>
newtype TextOptions = TextOptions 
  { "DefaultValue" :: NullOrUndefined.NullOrUndefined (FieldValue)
  , "FacetEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ResultEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "TextProcessor" :: NullOrUndefined.NullOrUndefined (FieldName)
  }
derive instance newtypeTextOptions :: Newtype TextOptions _
derive instance repGenericTextOptions :: Generic TextOptions _
instance showTextOptions :: Show TextOptions where
  show = genericShow
instance decodeTextOptions :: Decode TextOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTextOptions :: Encode TextOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Options that define a <code>uint</code> field in the search index.</p>
newtype UIntOptions = UIntOptions 
  { "DefaultValue" :: NullOrUndefined.NullOrUndefined (UIntValue)
  }
derive instance newtypeUIntOptions :: Newtype UIntOptions _
derive instance repGenericUIntOptions :: Generic UIntOptions _
instance showUIntOptions :: Show UIntOptions where
  show = genericShow
instance decodeUIntOptions :: Decode UIntOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUIntOptions :: Encode UIntOptions where
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


-- | <p>Container for the parameters to the <code><a>UpdateAvailabilityOptions</a></code> operation. Specifies the name of the domain you want to update and the Multi-AZ availability option.</p>
newtype UpdateAvailabilityOptionsRequest = UpdateAvailabilityOptionsRequest 
  { "DomainName" :: (DomainName)
  , "MultiAZ" :: (Boolean)
  }
derive instance newtypeUpdateAvailabilityOptionsRequest :: Newtype UpdateAvailabilityOptionsRequest _
derive instance repGenericUpdateAvailabilityOptionsRequest :: Generic UpdateAvailabilityOptionsRequest _
instance showUpdateAvailabilityOptionsRequest :: Show UpdateAvailabilityOptionsRequest where
  show = genericShow
instance decodeUpdateAvailabilityOptionsRequest :: Decode UpdateAvailabilityOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAvailabilityOptionsRequest :: Encode UpdateAvailabilityOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a <code>UpdateAvailabilityOptions</code> request. Contains the status of the domain's availability options. </p>
newtype UpdateAvailabilityOptionsResponse = UpdateAvailabilityOptionsResponse 
  { "AvailabilityOptions" :: NullOrUndefined.NullOrUndefined (AvailabilityOptionsStatus)
  }
derive instance newtypeUpdateAvailabilityOptionsResponse :: Newtype UpdateAvailabilityOptionsResponse _
derive instance repGenericUpdateAvailabilityOptionsResponse :: Generic UpdateAvailabilityOptionsResponse _
instance showUpdateAvailabilityOptionsResponse :: Show UpdateAvailabilityOptionsResponse where
  show = genericShow
instance decodeUpdateAvailabilityOptionsResponse :: Decode UpdateAvailabilityOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAvailabilityOptionsResponse :: Encode UpdateAvailabilityOptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDefaultSearchFieldRequest = UpdateDefaultSearchFieldRequest 
  { "DomainName" :: (DomainName)
  , "DefaultSearchField" :: (String)
  }
derive instance newtypeUpdateDefaultSearchFieldRequest :: Newtype UpdateDefaultSearchFieldRequest _
derive instance repGenericUpdateDefaultSearchFieldRequest :: Generic UpdateDefaultSearchFieldRequest _
instance showUpdateDefaultSearchFieldRequest :: Show UpdateDefaultSearchFieldRequest where
  show = genericShow
instance decodeUpdateDefaultSearchFieldRequest :: Decode UpdateDefaultSearchFieldRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDefaultSearchFieldRequest :: Encode UpdateDefaultSearchFieldRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the status of an updated default search field.</p>
newtype UpdateDefaultSearchFieldResponse = UpdateDefaultSearchFieldResponse 
  { "DefaultSearchField" :: (DefaultSearchFieldStatus)
  }
derive instance newtypeUpdateDefaultSearchFieldResponse :: Newtype UpdateDefaultSearchFieldResponse _
derive instance repGenericUpdateDefaultSearchFieldResponse :: Generic UpdateDefaultSearchFieldResponse _
instance showUpdateDefaultSearchFieldResponse :: Show UpdateDefaultSearchFieldResponse where
  show = genericShow
instance decodeUpdateDefaultSearchFieldResponse :: Decode UpdateDefaultSearchFieldResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDefaultSearchFieldResponse :: Encode UpdateDefaultSearchFieldResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateServiceAccessPoliciesRequest = UpdateServiceAccessPoliciesRequest 
  { "DomainName" :: (DomainName)
  , "AccessPolicies" :: (PolicyDocument)
  }
derive instance newtypeUpdateServiceAccessPoliciesRequest :: Newtype UpdateServiceAccessPoliciesRequest _
derive instance repGenericUpdateServiceAccessPoliciesRequest :: Generic UpdateServiceAccessPoliciesRequest _
instance showUpdateServiceAccessPoliciesRequest :: Show UpdateServiceAccessPoliciesRequest where
  show = genericShow
instance decodeUpdateServiceAccessPoliciesRequest :: Decode UpdateServiceAccessPoliciesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateServiceAccessPoliciesRequest :: Encode UpdateServiceAccessPoliciesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the status of updated access policies.</p>
newtype UpdateServiceAccessPoliciesResponse = UpdateServiceAccessPoliciesResponse 
  { "AccessPolicies" :: (AccessPoliciesStatus)
  }
derive instance newtypeUpdateServiceAccessPoliciesResponse :: Newtype UpdateServiceAccessPoliciesResponse _
derive instance repGenericUpdateServiceAccessPoliciesResponse :: Generic UpdateServiceAccessPoliciesResponse _
instance showUpdateServiceAccessPoliciesResponse :: Show UpdateServiceAccessPoliciesResponse where
  show = genericShow
instance decodeUpdateServiceAccessPoliciesResponse :: Decode UpdateServiceAccessPoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateServiceAccessPoliciesResponse :: Encode UpdateServiceAccessPoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateStemmingOptionsRequest = UpdateStemmingOptionsRequest 
  { "DomainName" :: (DomainName)
  , "Stems" :: (StemsDocument)
  }
derive instance newtypeUpdateStemmingOptionsRequest :: Newtype UpdateStemmingOptionsRequest _
derive instance repGenericUpdateStemmingOptionsRequest :: Generic UpdateStemmingOptionsRequest _
instance showUpdateStemmingOptionsRequest :: Show UpdateStemmingOptionsRequest where
  show = genericShow
instance decodeUpdateStemmingOptionsRequest :: Decode UpdateStemmingOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStemmingOptionsRequest :: Encode UpdateStemmingOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the status of updated stemming options.</p>
newtype UpdateStemmingOptionsResponse = UpdateStemmingOptionsResponse 
  { "Stems" :: (StemmingOptionsStatus)
  }
derive instance newtypeUpdateStemmingOptionsResponse :: Newtype UpdateStemmingOptionsResponse _
derive instance repGenericUpdateStemmingOptionsResponse :: Generic UpdateStemmingOptionsResponse _
instance showUpdateStemmingOptionsResponse :: Show UpdateStemmingOptionsResponse where
  show = genericShow
instance decodeUpdateStemmingOptionsResponse :: Decode UpdateStemmingOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStemmingOptionsResponse :: Encode UpdateStemmingOptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateStopwordOptionsRequest = UpdateStopwordOptionsRequest 
  { "DomainName" :: (DomainName)
  , "Stopwords" :: (StopwordsDocument)
  }
derive instance newtypeUpdateStopwordOptionsRequest :: Newtype UpdateStopwordOptionsRequest _
derive instance repGenericUpdateStopwordOptionsRequest :: Generic UpdateStopwordOptionsRequest _
instance showUpdateStopwordOptionsRequest :: Show UpdateStopwordOptionsRequest where
  show = genericShow
instance decodeUpdateStopwordOptionsRequest :: Decode UpdateStopwordOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStopwordOptionsRequest :: Encode UpdateStopwordOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the status of updated stopword options.</p>
newtype UpdateStopwordOptionsResponse = UpdateStopwordOptionsResponse 
  { "Stopwords" :: (StopwordOptionsStatus)
  }
derive instance newtypeUpdateStopwordOptionsResponse :: Newtype UpdateStopwordOptionsResponse _
derive instance repGenericUpdateStopwordOptionsResponse :: Generic UpdateStopwordOptionsResponse _
instance showUpdateStopwordOptionsResponse :: Show UpdateStopwordOptionsResponse where
  show = genericShow
instance decodeUpdateStopwordOptionsResponse :: Decode UpdateStopwordOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStopwordOptionsResponse :: Encode UpdateStopwordOptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateSynonymOptionsRequest = UpdateSynonymOptionsRequest 
  { "DomainName" :: (DomainName)
  , "Synonyms" :: (SynonymsDocument)
  }
derive instance newtypeUpdateSynonymOptionsRequest :: Newtype UpdateSynonymOptionsRequest _
derive instance repGenericUpdateSynonymOptionsRequest :: Generic UpdateSynonymOptionsRequest _
instance showUpdateSynonymOptionsRequest :: Show UpdateSynonymOptionsRequest where
  show = genericShow
instance decodeUpdateSynonymOptionsRequest :: Decode UpdateSynonymOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSynonymOptionsRequest :: Encode UpdateSynonymOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response message that contains the status of updated synonym options.</p>
newtype UpdateSynonymOptionsResponse = UpdateSynonymOptionsResponse 
  { "Synonyms" :: (SynonymOptionsStatus)
  }
derive instance newtypeUpdateSynonymOptionsResponse :: Newtype UpdateSynonymOptionsResponse _
derive instance repGenericUpdateSynonymOptionsResponse :: Generic UpdateSynonymOptionsResponse _
instance showUpdateSynonymOptionsResponse :: Show UpdateSynonymOptionsResponse where
  show = genericShow
instance decodeUpdateSynonymOptionsResponse :: Decode UpdateSynonymOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSynonymOptionsResponse :: Encode UpdateSynonymOptionsResponse where
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
