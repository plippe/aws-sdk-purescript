## Module AWS.CloudSearchDomain

<p>You use the AmazonCloudSearch2013 API to upload documents to a search domain and search those documents. </p> <p>The endpoints for submitting <code>UploadDocuments</code>, <code>Search</code>, and <code>Suggest</code> requests are domain-specific. To get the endpoints for your domain, use the Amazon CloudSearch configuration service <code>DescribeDomains</code> action. The domain endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. You submit suggest requests to the search endpoint. </p> <p>For more information, see the <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide">Amazon CloudSearch Developer Guide</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `search`

``` purescript
search :: forall eff. SearchRequest -> Aff (err :: RequestError | eff) SearchResponse
```

<p>Retrieves a list of documents that match the specified search criteria. How you specify the search criteria depends on which query parser you use. Amazon CloudSearch supports four query parsers:</p> <ul> <li><code>simple</code>: search all <code>text</code> and <code>text-array</code> fields for the specified string. Search for phrases, individual terms, and prefixes. </li> <li><code>structured</code>: search specific fields, construct compound queries using Boolean operators, and use advanced features such as term boosting and proximity searching.</li> <li><code>lucene</code>: specify search criteria using the Apache Lucene query parser syntax.</li> <li><code>dismax</code>: specify search criteria using the simplified subset of the Apache Lucene query parser syntax defined by the DisMax query parser.</li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/searching.html">Searching Your Data</a> in the <i>Amazon CloudSearch Developer Guide</i>.</p> <p>The endpoint for submitting <code>Search</code> requests is domain-specific. You submit search requests to a domain's search endpoint. To get the search endpoint for your domain, use the Amazon CloudSearch configuration service <code>DescribeDomains</code> action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. </p>

#### `suggest`

``` purescript
suggest :: forall eff. SuggestRequest -> Aff (err :: RequestError | eff) SuggestResponse
```

<p>Retrieves autocomplete suggestions for a partial query string. You can use suggestions enable you to display likely matches before users finish typing. In Amazon CloudSearch, suggestions are based on the contents of a particular text field. When you request suggestions, Amazon CloudSearch finds all of the documents whose values in the suggester field start with the specified query string. The beginning of the field must match the query string to be considered a match. </p> <p>For more information about configuring suggesters and retrieving suggestions, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html">Getting Suggestions</a> in the <i>Amazon CloudSearch Developer Guide</i>. </p> <p>The endpoint for submitting <code>Suggest</code> requests is domain-specific. You submit suggest requests to a domain's search endpoint. To get the search endpoint for your domain, use the Amazon CloudSearch configuration service <code>DescribeDomains</code> action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. </p>

#### `uploadDocuments`

``` purescript
uploadDocuments :: forall eff. UploadDocumentsRequest -> Aff (err :: RequestError | eff) UploadDocumentsResponse
```

<p>Posts a batch of documents to a search domain for indexing. A document batch is a collection of add and delete operations that represent the documents you want to add, update, or delete from your domain. Batches can be described in either JSON or XML. Each item that you want Amazon CloudSearch to return as a search result (such as a product) is represented as a document. Every document has a unique ID and one or more fields that contain the data that you want to search and return in results. Individual documents cannot contain more than 1 MB of data. The entire batch cannot exceed 5 MB. To get the best possible upload performance, group add and delete operations in batches that are close the 5 MB limit. Submitting a large volume of single-document batches can overload a domain's document service. </p> <p>The endpoint for submitting <code>UploadDocuments</code> requests is domain-specific. To get the document endpoint for your domain, use the Amazon CloudSearch configuration service <code>DescribeDomains</code> action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console. </p> <p>For more information about formatting your data for Amazon CloudSearch, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/preparing-data.html">Preparing Your Data</a> in the <i>Amazon CloudSearch Developer Guide</i>. For more information about uploading data for indexing, see <a href="http://docs.aws.amazon.com/cloudsearch/latest/developerguide/uploading-data.html">Uploading Data</a> in the <i>Amazon CloudSearch Developer Guide</i>. </p>

#### `Adds`

``` purescript
newtype Adds
  = Adds Number
```

##### Instances
``` purescript
Newtype Adds _
```

#### `Bucket`

``` purescript
newtype Bucket
  = Bucket { "Value'" :: NullOrUndefined (String), "Count'" :: NullOrUndefined (Number) }
```

<p>A container for facet information. </p>

##### Instances
``` purescript
Newtype Bucket _
```

#### `BucketInfo`

``` purescript
newtype BucketInfo
  = BucketInfo { "Buckets'" :: NullOrUndefined (BucketList) }
```

<p>A container for the calculated facet values and counts.</p>

##### Instances
``` purescript
Newtype BucketInfo _
```

#### `BucketList`

``` purescript
newtype BucketList
  = BucketList (Array Bucket)
```

##### Instances
``` purescript
Newtype BucketList _
```

#### `ContentType`

``` purescript
newtype ContentType
  = ContentType String
```

##### Instances
``` purescript
Newtype ContentType _
```

#### `Cursor`

``` purescript
newtype Cursor
  = Cursor String
```

##### Instances
``` purescript
Newtype Cursor _
```

#### `Deletes`

``` purescript
newtype Deletes
  = Deletes Number
```

##### Instances
``` purescript
Newtype Deletes _
```

#### `DocumentServiceException`

``` purescript
newtype DocumentServiceException
  = DocumentServiceException { "Status'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String) }
```

<p>Information about any problems encountered while processing an upload request.</p>

##### Instances
``` purescript
Newtype DocumentServiceException _
```

#### `DocumentServiceWarning`

``` purescript
newtype DocumentServiceWarning
  = DocumentServiceWarning { "Message'" :: NullOrUndefined (String) }
```

<p>A warning returned by the document service when an issue is discovered while processing an upload request.</p>

##### Instances
``` purescript
Newtype DocumentServiceWarning _
```

#### `DocumentServiceWarnings`

``` purescript
newtype DocumentServiceWarnings
  = DocumentServiceWarnings (Array DocumentServiceWarning)
```

##### Instances
``` purescript
Newtype DocumentServiceWarnings _
```

#### `Expr`

``` purescript
newtype Expr
  = Expr String
```

##### Instances
``` purescript
Newtype Expr _
```

#### `Exprs`

``` purescript
newtype Exprs
  = Exprs (Map String String)
```

##### Instances
``` purescript
Newtype Exprs _
```

#### `Facet`

``` purescript
newtype Facet
  = Facet String
```

##### Instances
``` purescript
Newtype Facet _
```

#### `Facets`

``` purescript
newtype Facets
  = Facets (Map String BucketInfo)
```

##### Instances
``` purescript
Newtype Facets _
```

#### `FieldStats`

``` purescript
newtype FieldStats
  = FieldStats { "Min'" :: NullOrUndefined (String), "Max'" :: NullOrUndefined (String), "Count'" :: NullOrUndefined (Number), "Missing'" :: NullOrUndefined (Number), "Sum'" :: NullOrUndefined (Number), "SumOfSquares'" :: NullOrUndefined (Number), "Mean'" :: NullOrUndefined (String), "Stddev'" :: NullOrUndefined (Number) }
```

<p>The statistics for a field calculated in the request.</p>

##### Instances
``` purescript
Newtype FieldStats _
```

#### `FieldValue`

``` purescript
newtype FieldValue
  = FieldValue (Array String)
```

##### Instances
``` purescript
Newtype FieldValue _
```

#### `Fields`

``` purescript
newtype Fields
  = Fields (Map String FieldValue)
```

##### Instances
``` purescript
Newtype Fields _
```

#### `FilterQuery`

``` purescript
newtype FilterQuery
  = FilterQuery String
```

##### Instances
``` purescript
Newtype FilterQuery _
```

#### `Highlight`

``` purescript
newtype Highlight
  = Highlight String
```

##### Instances
``` purescript
Newtype Highlight _
```

#### `Highlights`

``` purescript
newtype Highlights
  = Highlights (Map String String)
```

##### Instances
``` purescript
Newtype Highlights _
```

#### `Hit`

``` purescript
newtype Hit
  = Hit { "Id'" :: NullOrUndefined (String), "Fields'" :: NullOrUndefined (Fields), "Exprs'" :: NullOrUndefined (Exprs), "Highlights'" :: NullOrUndefined (Highlights) }
```

<p>Information about a document that matches the search request.</p>

##### Instances
``` purescript
Newtype Hit _
```

#### `HitList`

``` purescript
newtype HitList
  = HitList (Array Hit)
```

##### Instances
``` purescript
Newtype HitList _
```

#### `Hits`

``` purescript
newtype Hits
  = Hits { "Found'" :: NullOrUndefined (Number), "Start'" :: NullOrUndefined (Number), "Cursor'" :: NullOrUndefined (String), "Hit'" :: NullOrUndefined (HitList) }
```

<p>The collection of documents that match the search request.</p>

##### Instances
``` purescript
Newtype Hits _
```

#### `Partial`

``` purescript
newtype Partial
  = Partial Boolean
```

##### Instances
``` purescript
Newtype Partial _
```

#### `Query`

``` purescript
newtype Query
  = Query String
```

##### Instances
``` purescript
Newtype Query _
```

#### `QueryOptions`

``` purescript
newtype QueryOptions
  = QueryOptions String
```

##### Instances
``` purescript
Newtype QueryOptions _
```

#### `QueryParser`

``` purescript
newtype QueryParser
  = QueryParser String
```

##### Instances
``` purescript
Newtype QueryParser _
```

#### `Return`

``` purescript
newtype Return
  = Return String
```

##### Instances
``` purescript
Newtype Return _
```

#### `SearchException`

``` purescript
newtype SearchException
  = SearchException { "Message'" :: NullOrUndefined (String) }
```

<p>Information about any problems encountered while processing a search request.</p>

##### Instances
``` purescript
Newtype SearchException _
```

#### `SearchRequest`

``` purescript
newtype SearchRequest
  = SearchRequest { "Cursor'" :: NullOrUndefined (Cursor), "Expr'" :: NullOrUndefined (Expr), "Facet'" :: NullOrUndefined (Facet), "FilterQuery'" :: NullOrUndefined (FilterQuery), "Highlight'" :: NullOrUndefined (Highlight), "Partial'" :: NullOrUndefined (Partial), "Query'" :: Query, "QueryOptions'" :: NullOrUndefined (QueryOptions), "QueryParser'" :: NullOrUndefined (QueryParser), "Return'" :: NullOrUndefined (Return), "Size'" :: NullOrUndefined (Size), "Sort'" :: NullOrUndefined (Sort), "Start'" :: NullOrUndefined (Start), "Stats'" :: NullOrUndefined (Stat) }
```

<p>Container for the parameters to the <code>Search</code> request.</p>

##### Instances
``` purescript
Newtype SearchRequest _
```

#### `SearchResponse`

``` purescript
newtype SearchResponse
  = SearchResponse { "Status'" :: NullOrUndefined (SearchStatus), "Hits'" :: NullOrUndefined (Hits), "Facets'" :: NullOrUndefined (Facets), "Stats'" :: NullOrUndefined (Stats) }
```

<p>The result of a <code>Search</code> request. Contains the documents that match the specified search criteria and any requested fields, highlights, and facet information.</p>

##### Instances
``` purescript
Newtype SearchResponse _
```

#### `SearchStatus`

``` purescript
newtype SearchStatus
  = SearchStatus { "Timems'" :: NullOrUndefined (Number), "Rid'" :: NullOrUndefined (String) }
```

<p>Contains the resource id (<code>rid</code>) and the time it took to process the request (<code>timems</code>).</p>

##### Instances
``` purescript
Newtype SearchStatus _
```

#### `Size`

``` purescript
newtype Size
  = Size Number
```

##### Instances
``` purescript
Newtype Size _
```

#### `Sort`

``` purescript
newtype Sort
  = Sort String
```

##### Instances
``` purescript
Newtype Sort _
```

#### `Start`

``` purescript
newtype Start
  = Start Number
```

##### Instances
``` purescript
Newtype Start _
```

#### `Stat`

``` purescript
newtype Stat
  = Stat String
```

##### Instances
``` purescript
Newtype Stat _
```

#### `Stats`

``` purescript
newtype Stats
  = Stats (Map String FieldStats)
```

<p>The statistics calculated in the request.</p>

##### Instances
``` purescript
Newtype Stats _
```

#### `SuggestModel`

``` purescript
newtype SuggestModel
  = SuggestModel { "Query'" :: NullOrUndefined (String), "Found'" :: NullOrUndefined (Number), "Suggestions'" :: NullOrUndefined (Suggestions) }
```

<p>Container for the suggestion information returned in a <code>SuggestResponse</code>.</p>

##### Instances
``` purescript
Newtype SuggestModel _
```

#### `SuggestRequest`

``` purescript
newtype SuggestRequest
  = SuggestRequest { "Query'" :: Query, "Suggester'" :: Suggester, "Size'" :: NullOrUndefined (SuggestionsSize) }
```

<p>Container for the parameters to the <code>Suggest</code> request.</p>

##### Instances
``` purescript
Newtype SuggestRequest _
```

#### `SuggestResponse`

``` purescript
newtype SuggestResponse
  = SuggestResponse { "Status'" :: NullOrUndefined (SuggestStatus), "Suggest'" :: NullOrUndefined (SuggestModel) }
```

<p>Contains the response to a <code>Suggest</code> request.</p>

##### Instances
``` purescript
Newtype SuggestResponse _
```

#### `SuggestStatus`

``` purescript
newtype SuggestStatus
  = SuggestStatus { "Timems'" :: NullOrUndefined (Number), "Rid'" :: NullOrUndefined (String) }
```

<p>Contains the resource id (<code>rid</code>) and the time it took to process the request (<code>timems</code>).</p>

##### Instances
``` purescript
Newtype SuggestStatus _
```

#### `Suggester`

``` purescript
newtype Suggester
  = Suggester String
```

##### Instances
``` purescript
Newtype Suggester _
```

#### `SuggestionMatch`

``` purescript
newtype SuggestionMatch
  = SuggestionMatch { "Suggestion'" :: NullOrUndefined (String), "Score'" :: NullOrUndefined (Number), "Id'" :: NullOrUndefined (String) }
```

<p>An autocomplete suggestion that matches the query string specified in a <code>SuggestRequest</code>. </p>

##### Instances
``` purescript
Newtype SuggestionMatch _
```

#### `Suggestions`

``` purescript
newtype Suggestions
  = Suggestions (Array SuggestionMatch)
```

##### Instances
``` purescript
Newtype Suggestions _
```

#### `SuggestionsSize`

``` purescript
newtype SuggestionsSize
  = SuggestionsSize Number
```

##### Instances
``` purescript
Newtype SuggestionsSize _
```

#### `UploadDocumentsRequest`

``` purescript
newtype UploadDocumentsRequest
  = UploadDocumentsRequest { "Documents'" :: String, "ContentType'" :: ContentType }
```

<p>Container for the parameters to the <code>UploadDocuments</code> request.</p>

##### Instances
``` purescript
Newtype UploadDocumentsRequest _
```

#### `UploadDocumentsResponse`

``` purescript
newtype UploadDocumentsResponse
  = UploadDocumentsResponse { "Status'" :: NullOrUndefined (String), "Adds'" :: NullOrUndefined (Adds), "Deletes'" :: NullOrUndefined (Deletes), "Warnings'" :: NullOrUndefined (DocumentServiceWarnings) }
```

<p>Contains the response to an <code>UploadDocuments</code> request.</p>

##### Instances
``` purescript
Newtype UploadDocumentsResponse _
```


