

-- | <p>You use the AmazonCloudSearch2013 API to upload documents to a search domain and search those documents. </p> <p>The endpoints for submitting <code>UploadDocuments</code>, <code>Search</code>, and <code>Suggest</code> requests are domain-specific. To get the endpoints for your domain, use the Amazon CloudSearch configuration service <code>DescribeDomains</code> action. The domain endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. You submit suggest requests to the search endpoint. </p> <p>For more information, see the <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide">Amazon CloudSearch Developer Guide</a>.</p>
module AWS.CloudSearchDomain where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudSearchDomain" :: String


-- | <p>Retrieves a list of documents that match the specified search criteria. How you specify the search criteria depends on which query parser you use. Amazon CloudSearch supports four query parsers:</p> <ul> <li><code>simple</code>: search all <code>text</code> and <code>text-array</code> fields for the specified string. Search for phrases, individual terms, and prefixes. </li> <li><code>structured</code>: search specific fields, construct compound queries using Boolean operators, and use advanced features such as term boosting and proximity searching.</li> <li><code>lucene</code>: specify search criteria using the Apache Lucene query parser syntax.</li> <li><code>dismax</code>: specify search criteria using the simplified subset of the Apache Lucene query parser syntax defined by the DisMax query parser.</li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching.html">Searching Your Data</a> in the <i>Amazon CloudSearch Developer Guide</i>.</p> <p>The endpoint for submitting <code>Search</code> requests is domain-specific. You submit search requests to a domain's search endpoint. To get the search endpoint for your domain, use the Amazon CloudSearch configuration service <code>DescribeDomains</code> action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. </p>
search :: forall eff. SearchRequest -> Aff (err :: AWS.RequestError | eff) SearchResponse
search = AWS.request serviceName "Search" 


-- | <p>Retrieves autocomplete suggestions for a partial query string. You can use suggestions enable you to display likely matches before users finish typing. In Amazon CloudSearch, suggestions are based on the contents of a particular text field. When you request suggestions, Amazon CloudSearch finds all of the documents whose values in the suggester field start with the specified query string. The beginning of the field must match the query string to be considered a match. </p> <p>For more information about configuring suggesters and retrieving suggestions, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html">Getting Suggestions</a> in the <i>Amazon CloudSearch Developer Guide</i>. </p> <p>The endpoint for submitting <code>Suggest</code> requests is domain-specific. You submit suggest requests to a domain's search endpoint. To get the search endpoint for your domain, use the Amazon CloudSearch configuration service <code>DescribeDomains</code> action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. </p>
suggest :: forall eff. SuggestRequest -> Aff (err :: AWS.RequestError | eff) SuggestResponse
suggest = AWS.request serviceName "Suggest" 


-- | <p>Posts a batch of documents to a search domain for indexing. A document batch is a collection of add and delete operations that represent the documents you want to add, update, or delete from your domain. Batches can be described in either JSON or XML. Each item that you want Amazon CloudSearch to return as a search result (such as a product) is represented as a document. Every document has a unique ID and one or more fields that contain the data that you want to search and return in results. Individual documents cannot contain more than 1 MB of data. The entire batch cannot exceed 5 MB. To get the best possible upload performance, group add and delete operations in batches that are close the 5 MB limit. Submitting a large volume of single-document batches can overload a domain's document service. </p> <p>The endpoint for submitting <code>UploadDocuments</code> requests is domain-specific. To get the document endpoint for your domain, use the Amazon CloudSearch configuration service <code>DescribeDomains</code> action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. </p> <p>For more information about formatting your data for Amazon CloudSearch, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/preparing-data.html">Preparing Your Data</a> in the <i>Amazon CloudSearch Developer Guide</i>. For more information about uploading data for indexing, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/uploading-data.html">Uploading Data</a> in the <i>Amazon CloudSearch Developer Guide</i>. </p>
uploadDocuments :: forall eff. UploadDocumentsRequest -> Aff (err :: AWS.RequestError | eff) UploadDocumentsResponse
uploadDocuments = AWS.request serviceName "UploadDocuments" 


newtype Adds = Adds Number


-- | <p>A container for facet information. </p>
newtype Bucket = Bucket 
  { "Value'" :: NullOrUndefined (String)
  , "Count'" :: NullOrUndefined (Number)
  }


-- | <p>A container for the calculated facet values and counts.</p>
newtype BucketInfo = BucketInfo 
  { "Buckets'" :: NullOrUndefined (BucketList)
  }


newtype BucketList = BucketList (Array Bucket)


newtype ContentType = ContentType String


newtype Cursor = Cursor String


newtype Deletes = Deletes Number


-- | <p>Information about any problems encountered while processing an upload request.</p>
newtype DocumentServiceException = DocumentServiceException 
  { "Status'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }


-- | <p>A warning returned by the document service when an issue is discovered while processing an upload request.</p>
newtype DocumentServiceWarning = DocumentServiceWarning 
  { "Message'" :: NullOrUndefined (String)
  }


newtype DocumentServiceWarnings = DocumentServiceWarnings (Array DocumentServiceWarning)


newtype Expr = Expr String


newtype Exprs = Exprs (Map String String)


newtype Facet = Facet String


newtype Facets = Facets (Map String BucketInfo)


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


newtype FieldValue = FieldValue (Array String)


newtype Fields = Fields (Map String FieldValue)


newtype FilterQuery = FilterQuery String


newtype Highlight = Highlight String


newtype Highlights = Highlights (Map String String)


-- | <p>Information about a document that matches the search request.</p>
newtype Hit = Hit 
  { "Id'" :: NullOrUndefined (String)
  , "Fields'" :: NullOrUndefined (Fields)
  , "Exprs'" :: NullOrUndefined (Exprs)
  , "Highlights'" :: NullOrUndefined (Highlights)
  }


newtype HitList = HitList (Array Hit)


-- | <p>The collection of documents that match the search request.</p>
newtype Hits = Hits 
  { "Found'" :: NullOrUndefined (Number)
  , "Start'" :: NullOrUndefined (Number)
  , "Cursor'" :: NullOrUndefined (String)
  , "Hit'" :: NullOrUndefined (HitList)
  }


newtype Partial = Partial Boolean


newtype Query = Query String


newtype QueryOptions = QueryOptions String


newtype QueryParser = QueryParser String


newtype Return = Return String


-- | <p>Information about any problems encountered while processing a search request.</p>
newtype SearchException = SearchException 
  { "Message'" :: NullOrUndefined (String)
  }


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


-- | <p>The result of a <code>Search</code> request. Contains the documents that match the specified search criteria and any requested fields, highlights, and facet information.</p>
newtype SearchResponse = SearchResponse 
  { "Status'" :: NullOrUndefined (SearchStatus)
  , "Hits'" :: NullOrUndefined (Hits)
  , "Facets'" :: NullOrUndefined (Facets)
  , "Stats'" :: NullOrUndefined (Stats)
  }


-- | <p>Contains the resource id (<code>rid</code>) and the time it took to process the request (<code>timems</code>).</p>
newtype SearchStatus = SearchStatus 
  { "Timems'" :: NullOrUndefined (Number)
  , "Rid'" :: NullOrUndefined (String)
  }


newtype Size = Size Number


newtype Sort = Sort String


newtype Start = Start Number


newtype Stat = Stat String


-- | <p>The statistics calculated in the request.</p>
newtype Stats = Stats (Map String FieldStats)


-- | <p>Container for the suggestion information returned in a <code>SuggestResponse</code>.</p>
newtype SuggestModel = SuggestModel 
  { "Query'" :: NullOrUndefined (String)
  , "Found'" :: NullOrUndefined (Number)
  , "Suggestions'" :: NullOrUndefined (Suggestions)
  }


-- | <p>Container for the parameters to the <code>Suggest</code> request.</p>
newtype SuggestRequest = SuggestRequest 
  { "Query'" :: (Query)
  , "Suggester'" :: (Suggester)
  , "Size'" :: NullOrUndefined (SuggestionsSize)
  }


-- | <p>Contains the response to a <code>Suggest</code> request.</p>
newtype SuggestResponse = SuggestResponse 
  { "Status'" :: NullOrUndefined (SuggestStatus)
  , "Suggest'" :: NullOrUndefined (SuggestModel)
  }


-- | <p>Contains the resource id (<code>rid</code>) and the time it took to process the request (<code>timems</code>).</p>
newtype SuggestStatus = SuggestStatus 
  { "Timems'" :: NullOrUndefined (Number)
  , "Rid'" :: NullOrUndefined (String)
  }


newtype Suggester = Suggester String


-- | <p>An autocomplete suggestion that matches the query string specified in a <code>SuggestRequest</code>. </p>
newtype SuggestionMatch = SuggestionMatch 
  { "Suggestion'" :: NullOrUndefined (String)
  , "Score'" :: NullOrUndefined (Number)
  , "Id'" :: NullOrUndefined (String)
  }


newtype Suggestions = Suggestions (Array SuggestionMatch)


newtype SuggestionsSize = SuggestionsSize Number


-- | <p>Container for the parameters to the <code>UploadDocuments</code> request.</p>
newtype UploadDocumentsRequest = UploadDocumentsRequest 
  { "Documents'" :: (String)
  , "ContentType'" :: (ContentType)
  }


-- | <p>Contains the response to an <code>UploadDocuments</code> request.</p>
newtype UploadDocumentsResponse = UploadDocumentsResponse 
  { "Status'" :: NullOrUndefined (String)
  , "Adds'" :: NullOrUndefined (Adds)
  , "Deletes'" :: NullOrUndefined (Deletes)
  , "Warnings'" :: NullOrUndefined (DocumentServiceWarnings)
  }
