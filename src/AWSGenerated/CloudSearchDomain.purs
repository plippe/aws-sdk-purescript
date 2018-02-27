

-- | <p>You use the AmazonCloudSearch2013 API to upload documents to a search domain and search those documents. </p> <p>The endpoints for submitting <code>UploadDocuments</code>, <code>Search</code>, and <code>Suggest</code> requests are domain-specific. To get the endpoints for your domain, use the Amazon CloudSearch configuration service <code>DescribeDomains</code> action. The domain endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. You submit suggest requests to the search endpoint. </p> <p>For more information, see the <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide">Amazon CloudSearch Developer Guide</a>.</p>
module AWS.CloudSearchDomain where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudSearchDomain" :: String


-- | <p>Retrieves a list of documents that match the specified search criteria. How you specify the search criteria depends on which query parser you use. Amazon CloudSearch supports four query parsers:</p> <ul> <li><code>simple</code>: search all <code>text</code> and <code>text-array</code> fields for the specified string. Search for phrases, individual terms, and prefixes. </li> <li><code>structured</code>: search specific fields, construct compound queries using Boolean operators, and use advanced features such as term boosting and proximity searching.</li> <li><code>lucene</code>: specify search criteria using the Apache Lucene query parser syntax.</li> <li><code>dismax</code>: specify search criteria using the simplified subset of the Apache Lucene query parser syntax defined by the DisMax query parser.</li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching.html">Searching Your Data</a> in the <i>Amazon CloudSearch Developer Guide</i>.</p> <p>The endpoint for submitting <code>Search</code> requests is domain-specific. You submit search requests to a domain's search endpoint. To get the search endpoint for your domain, use the Amazon CloudSearch configuration service <code>DescribeDomains</code> action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. </p>
search :: forall eff. SearchRequest -> Aff (err :: AWS.RequestError | eff) SearchResponse
search = AWS.request serviceName "search" 


-- | <p>Retrieves autocomplete suggestions for a partial query string. You can use suggestions enable you to display likely matches before users finish typing. In Amazon CloudSearch, suggestions are based on the contents of a particular text field. When you request suggestions, Amazon CloudSearch finds all of the documents whose values in the suggester field start with the specified query string. The beginning of the field must match the query string to be considered a match. </p> <p>For more information about configuring suggesters and retrieving suggestions, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html">Getting Suggestions</a> in the <i>Amazon CloudSearch Developer Guide</i>. </p> <p>The endpoint for submitting <code>Suggest</code> requests is domain-specific. You submit suggest requests to a domain's search endpoint. To get the search endpoint for your domain, use the Amazon CloudSearch configuration service <code>DescribeDomains</code> action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. </p>
suggest :: forall eff. SuggestRequest -> Aff (err :: AWS.RequestError | eff) SuggestResponse
suggest = AWS.request serviceName "suggest" 


-- | <p>Posts a batch of documents to a search domain for indexing. A document batch is a collection of add and delete operations that represent the documents you want to add, update, or delete from your domain. Batches can be described in either JSON or XML. Each item that you want Amazon CloudSearch to return as a search result (such as a product) is represented as a document. Every document has a unique ID and one or more fields that contain the data that you want to search and return in results. Individual documents cannot contain more than 1 MB of data. The entire batch cannot exceed 5 MB. To get the best possible upload performance, group add and delete operations in batches that are close the 5 MB limit. Submitting a large volume of single-document batches can overload a domain's document service. </p> <p>The endpoint for submitting <code>UploadDocuments</code> requests is domain-specific. To get the document endpoint for your domain, use the Amazon CloudSearch configuration service <code>DescribeDomains</code> action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. </p> <p>For more information about formatting your data for Amazon CloudSearch, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/preparing-data.html">Preparing Your Data</a> in the <i>Amazon CloudSearch Developer Guide</i>. For more information about uploading data for indexing, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/uploading-data.html">Uploading Data</a> in the <i>Amazon CloudSearch Developer Guide</i>. </p>
uploadDocuments :: forall eff. UploadDocumentsRequest -> Aff (err :: AWS.RequestError | eff) UploadDocumentsResponse
uploadDocuments = AWS.request serviceName "uploadDocuments" 


newtype Adds = Adds Number
derive instance newtypeAdds :: Newtype Adds _


-- | <p>A container for facet information. </p>
newtype Bucket = Bucket 
  { "Value'" :: NullOrUndefined (String)
  , "Count'" :: NullOrUndefined (Number)
  }
derive instance newtypeBucket :: Newtype Bucket _


-- | <p>A container for the calculated facet values and counts.</p>
newtype BucketInfo = BucketInfo 
  { "Buckets'" :: NullOrUndefined (BucketList)
  }
derive instance newtypeBucketInfo :: Newtype BucketInfo _


newtype BucketList = BucketList (Array Bucket)
derive instance newtypeBucketList :: Newtype BucketList _


newtype ContentType = ContentType String
derive instance newtypeContentType :: Newtype ContentType _


newtype Cursor = Cursor String
derive instance newtypeCursor :: Newtype Cursor _


newtype Deletes = Deletes Number
derive instance newtypeDeletes :: Newtype Deletes _


-- | <p>Information about any problems encountered while processing an upload request.</p>
newtype DocumentServiceException = DocumentServiceException 
  { "Status'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeDocumentServiceException :: Newtype DocumentServiceException _


-- | <p>A warning returned by the document service when an issue is discovered while processing an upload request.</p>
newtype DocumentServiceWarning = DocumentServiceWarning 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeDocumentServiceWarning :: Newtype DocumentServiceWarning _


newtype DocumentServiceWarnings = DocumentServiceWarnings (Array DocumentServiceWarning)
derive instance newtypeDocumentServiceWarnings :: Newtype DocumentServiceWarnings _


newtype Expr = Expr String
derive instance newtypeExpr :: Newtype Expr _


newtype Exprs = Exprs (Map String String)
derive instance newtypeExprs :: Newtype Exprs _


newtype Facet = Facet String
derive instance newtypeFacet :: Newtype Facet _


newtype Facets = Facets (Map String BucketInfo)
derive instance newtypeFacets :: Newtype Facets _


-- | <p>The statistics for a field calculated in the request.</p>
newtype FieldStats = FieldStats 
  { "Min'" :: NullOrUndefined (String)
  , "Max'" :: NullOrUndefined (String)
  , "Count'" :: NullOrUndefined (Number)
  , "Missing'" :: NullOrUndefined (Number)
  , "Sum'" :: NullOrUndefined (Number)
  , "SumOfSquares'" :: NullOrUndefined (Number)
  , "Mean'" :: NullOrUndefined (String)
  , "Stddev'" :: NullOrUndefined (Number)
  }
derive instance newtypeFieldStats :: Newtype FieldStats _


newtype FieldValue = FieldValue (Array String)
derive instance newtypeFieldValue :: Newtype FieldValue _


newtype Fields = Fields (Map String FieldValue)
derive instance newtypeFields :: Newtype Fields _


newtype FilterQuery = FilterQuery String
derive instance newtypeFilterQuery :: Newtype FilterQuery _


newtype Highlight = Highlight String
derive instance newtypeHighlight :: Newtype Highlight _


newtype Highlights = Highlights (Map String String)
derive instance newtypeHighlights :: Newtype Highlights _


-- | <p>Information about a document that matches the search request.</p>
newtype Hit = Hit 
  { "Id'" :: NullOrUndefined (String)
  , "Fields'" :: NullOrUndefined (Fields)
  , "Exprs'" :: NullOrUndefined (Exprs)
  , "Highlights'" :: NullOrUndefined (Highlights)
  }
derive instance newtypeHit :: Newtype Hit _


newtype HitList = HitList (Array Hit)
derive instance newtypeHitList :: Newtype HitList _


-- | <p>The collection of documents that match the search request.</p>
newtype Hits = Hits 
  { "Found'" :: NullOrUndefined (Number)
  , "Start'" :: NullOrUndefined (Number)
  , "Cursor'" :: NullOrUndefined (String)
  , "Hit'" :: NullOrUndefined (HitList)
  }
derive instance newtypeHits :: Newtype Hits _


newtype Partial = Partial Boolean
derive instance newtypePartial :: Newtype Partial _


newtype Query = Query String
derive instance newtypeQuery :: Newtype Query _


newtype QueryOptions = QueryOptions String
derive instance newtypeQueryOptions :: Newtype QueryOptions _


newtype QueryParser = QueryParser String
derive instance newtypeQueryParser :: Newtype QueryParser _


newtype Return = Return String
derive instance newtypeReturn :: Newtype Return _


-- | <p>Information about any problems encountered while processing a search request.</p>
newtype SearchException = SearchException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeSearchException :: Newtype SearchException _


-- | <p>Container for the parameters to the <code>Search</code> request.</p>
newtype SearchRequest = SearchRequest 
  { "Cursor'" :: NullOrUndefined (Cursor)
  , "Expr'" :: NullOrUndefined (Expr)
  , "Facet'" :: NullOrUndefined (Facet)
  , "FilterQuery'" :: NullOrUndefined (FilterQuery)
  , "Highlight'" :: NullOrUndefined (Highlight)
  , "Partial'" :: NullOrUndefined (Partial)
  , "Query'" :: (Query)
  , "QueryOptions'" :: NullOrUndefined (QueryOptions)
  , "QueryParser'" :: NullOrUndefined (QueryParser)
  , "Return'" :: NullOrUndefined (Return)
  , "Size'" :: NullOrUndefined (Size)
  , "Sort'" :: NullOrUndefined (Sort)
  , "Start'" :: NullOrUndefined (Start)
  , "Stats'" :: NullOrUndefined (Stat)
  }
derive instance newtypeSearchRequest :: Newtype SearchRequest _


-- | <p>The result of a <code>Search</code> request. Contains the documents that match the specified search criteria and any requested fields, highlights, and facet information.</p>
newtype SearchResponse = SearchResponse 
  { "Status'" :: NullOrUndefined (SearchStatus)
  , "Hits'" :: NullOrUndefined (Hits)
  , "Facets'" :: NullOrUndefined (Facets)
  , "Stats'" :: NullOrUndefined (Stats)
  }
derive instance newtypeSearchResponse :: Newtype SearchResponse _


-- | <p>Contains the resource id (<code>rid</code>) and the time it took to process the request (<code>timems</code>).</p>
newtype SearchStatus = SearchStatus 
  { "Timems'" :: NullOrUndefined (Number)
  , "Rid'" :: NullOrUndefined (String)
  }
derive instance newtypeSearchStatus :: Newtype SearchStatus _


newtype Size = Size Number
derive instance newtypeSize :: Newtype Size _


newtype Sort = Sort String
derive instance newtypeSort :: Newtype Sort _


newtype Start = Start Number
derive instance newtypeStart :: Newtype Start _


newtype Stat = Stat String
derive instance newtypeStat :: Newtype Stat _


-- | <p>The statistics calculated in the request.</p>
newtype Stats = Stats (Map String FieldStats)
derive instance newtypeStats :: Newtype Stats _


-- | <p>Container for the suggestion information returned in a <code>SuggestResponse</code>.</p>
newtype SuggestModel = SuggestModel 
  { "Query'" :: NullOrUndefined (String)
  , "Found'" :: NullOrUndefined (Number)
  , "Suggestions'" :: NullOrUndefined (Suggestions)
  }
derive instance newtypeSuggestModel :: Newtype SuggestModel _


-- | <p>Container for the parameters to the <code>Suggest</code> request.</p>
newtype SuggestRequest = SuggestRequest 
  { "Query'" :: (Query)
  , "Suggester'" :: (Suggester)
  , "Size'" :: NullOrUndefined (SuggestionsSize)
  }
derive instance newtypeSuggestRequest :: Newtype SuggestRequest _


-- | <p>Contains the response to a <code>Suggest</code> request.</p>
newtype SuggestResponse = SuggestResponse 
  { "Status'" :: NullOrUndefined (SuggestStatus)
  , "Suggest'" :: NullOrUndefined (SuggestModel)
  }
derive instance newtypeSuggestResponse :: Newtype SuggestResponse _


-- | <p>Contains the resource id (<code>rid</code>) and the time it took to process the request (<code>timems</code>).</p>
newtype SuggestStatus = SuggestStatus 
  { "Timems'" :: NullOrUndefined (Number)
  , "Rid'" :: NullOrUndefined (String)
  }
derive instance newtypeSuggestStatus :: Newtype SuggestStatus _


newtype Suggester = Suggester String
derive instance newtypeSuggester :: Newtype Suggester _


-- | <p>An autocomplete suggestion that matches the query string specified in a <code>SuggestRequest</code>. </p>
newtype SuggestionMatch = SuggestionMatch 
  { "Suggestion'" :: NullOrUndefined (String)
  , "Score'" :: NullOrUndefined (Number)
  , "Id'" :: NullOrUndefined (String)
  }
derive instance newtypeSuggestionMatch :: Newtype SuggestionMatch _


newtype Suggestions = Suggestions (Array SuggestionMatch)
derive instance newtypeSuggestions :: Newtype Suggestions _


newtype SuggestionsSize = SuggestionsSize Number
derive instance newtypeSuggestionsSize :: Newtype SuggestionsSize _


-- | <p>Container for the parameters to the <code>UploadDocuments</code> request.</p>
newtype UploadDocumentsRequest = UploadDocumentsRequest 
  { "Documents'" :: (String)
  , "ContentType'" :: (ContentType)
  }
derive instance newtypeUploadDocumentsRequest :: Newtype UploadDocumentsRequest _


-- | <p>Contains the response to an <code>UploadDocuments</code> request.</p>
newtype UploadDocumentsResponse = UploadDocumentsResponse 
  { "Status'" :: NullOrUndefined (String)
  , "Adds'" :: NullOrUndefined (Adds)
  , "Deletes'" :: NullOrUndefined (Deletes)
  , "Warnings'" :: NullOrUndefined (DocumentServiceWarnings)
  }
derive instance newtypeUploadDocumentsResponse :: Newtype UploadDocumentsResponse _
