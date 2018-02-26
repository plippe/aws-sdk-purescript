## Module AWS.SimpleDB

Amazon SimpleDB is a web service providing the core database functions of data indexing and querying in the cloud. By offloading the time and effort associated with building and operating a web-scale database, SimpleDB provides developers the freedom to focus on application development. <p> A traditional, clustered relational database requires a sizable upfront capital outlay, is complex to design, and often requires extensive and repetitive database administration. Amazon SimpleDB is dramatically simpler, requiring no schema, automatically indexing your data and providing a simple API for storage and access. This approach eliminates the administrative burden of data modeling, index maintenance, and performance tuning. Developers gain access to this functionality within Amazon's proven computing environment, are able to scale instantly, and pay only for what they use. </p> <p> Visit <a href="http://aws.amazon.com/simpledb/">http://aws.amazon.com/simpledb/</a> for more information. </p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `batchDeleteAttributes`

``` purescript
batchDeleteAttributes :: forall eff. BatchDeleteAttributesRequest -> Aff (err :: RequestError | eff) Unit
```

<p> Performs multiple DeleteAttributes operations in a single call, which reduces round trips and latencies. This enables Amazon SimpleDB to optimize requests, which generally yields better throughput. </p> <note> <p> If you specify BatchDeleteAttributes without attributes or values, all the attributes for the item are deleted. </p> <p> BatchDeleteAttributes is an idempotent operation; running it multiple times on the same item or attribute doesn't result in an error. </p> <p> The BatchDeleteAttributes operation succeeds or fails in its entirety. There are no partial deletes. You can execute multiple BatchDeleteAttributes operations and other operations in parallel. However, large numbers of concurrent BatchDeleteAttributes calls can result in Service Unavailable (503) responses. </p> <p> This operation is vulnerable to exceeding the maximum URL size when making a REST request using the HTTP GET method. </p> <p> This operation does not support conditions using Expected.X.Name, Expected.X.Value, or Expected.X.Exists. </p> </note> <p> The following limitations are enforced for this operation: <ul> <li>1 MB request size</li> <li>25 item limit per BatchDeleteAttributes operation</li> </ul> </p>

#### `batchPutAttributes`

``` purescript
batchPutAttributes :: forall eff. BatchPutAttributesRequest -> Aff (err :: RequestError | eff) Unit
```

<p> The <code>BatchPutAttributes</code> operation creates or replaces attributes within one or more items. By using this operation, the client can perform multiple <a>PutAttribute</a> operation with a single call. This helps yield savings in round trips and latencies, enabling Amazon SimpleDB to optimize requests and generally produce better throughput. </p> <p> The client may specify the item name with the <code>Item.X.ItemName</code> parameter. The client may specify new attributes using a combination of the <code>Item.X.Attribute.Y.Name</code> and <code>Item.X.Attribute.Y.Value</code> parameters. The client may specify the first attribute for the first item using the parameters <code>Item.0.Attribute.0.Name</code> and <code>Item.0.Attribute.0.Value</code>, and for the second attribute for the first item by the parameters <code>Item.0.Attribute.1.Name</code> and <code>Item.0.Attribute.1.Value</code>, and so on. </p> <p> Attributes are uniquely identified within an item by their name/value combination. For example, a single item can have the attributes <code>{ "first_name", "first_value" }</code> and <code>{ "first_name", "second_value" }</code>. However, it cannot have two attribute instances where both the <code>Item.X.Attribute.Y.Name</code> and <code>Item.X.Attribute.Y.Value</code> are the same. </p> <p> Optionally, the requester can supply the <code>Replace</code> parameter for each individual value. Setting this value to <code>true</code> will cause the new attribute values to replace the existing attribute values. For example, if an item <code>I</code> has the attributes <code>{ 'a', '1' }, { 'b', '2'}</code> and <code>{ 'b', '3' }</code> and the requester does a BatchPutAttributes of <code>{'I', 'b', '4' }</code> with the Replace parameter set to true, the final attributes of the item will be <code>{ 'a', '1' }</code> and <code>{ 'b', '4' }</code>, replacing the previous values of the 'b' attribute with the new value. </p> <note> You cannot specify an empty string as an item or as an attribute name. The <code>BatchPutAttributes</code> operation succeeds or fails in its entirety. There are no partial puts. </note> <important> This operation is vulnerable to exceeding the maximum URL size when making a REST request using the HTTP GET method. This operation does not support conditions using <code>Expected.X.Name</code>, <code>Expected.X.Value</code>, or <code>Expected.X.Exists</code>. </important> <p> You can execute multiple <code>BatchPutAttributes</code> operations and other operations in parallel. However, large numbers of concurrent <code>BatchPutAttributes</code> calls can result in Service Unavailable (503) responses. </p> <p> The following limitations are enforced for this operation: <ul> <li>256 attribute name-value pairs per item</li> <li>1 MB request size</li> <li>1 billion attributes per domain</li> <li>10 GB of total user data storage per domain</li> <li>25 item limit per <code>BatchPutAttributes</code> operation</li> </ul> </p>

#### `createDomain`

``` purescript
createDomain :: forall eff. CreateDomainRequest -> Aff (err :: RequestError | eff) Unit
```

<p> The <code>CreateDomain</code> operation creates a new domain. The domain name should be unique among the domains associated with the Access Key ID provided in the request. The <code>CreateDomain</code> operation may take 10 or more seconds to complete. </p> <note> CreateDomain is an idempotent operation; running it multiple times using the same domain name will not result in an error response. </note> <p> The client can create up to 100 domains per account. </p> <p> If the client requires additional domains, go to <a href="http://aws.amazon.com/contact-us/simpledb-limit-request/"> http://aws.amazon.com/contact-us/simpledb-limit-request/</a>. </p>

#### `deleteAttributes`

``` purescript
deleteAttributes :: forall eff. DeleteAttributesRequest -> Aff (err :: RequestError | eff) Unit
```

<p> Deletes one or more attributes associated with an item. If all attributes of the item are deleted, the item is deleted. </p> <note> If <code>DeleteAttributes</code> is called without being passed any attributes or values specified, all the attributes for the item are deleted. </note> <p> <code>DeleteAttributes</code> is an idempotent operation; running it multiple times on the same item or attribute does not result in an error response. </p> <p> Because Amazon SimpleDB makes multiple copies of item data and uses an eventual consistency update model, performing a <a>GetAttributes</a> or <a>Select</a> operation (read) immediately after a <code>DeleteAttributes</code> or <a>PutAttributes</a> operation (write) might not return updated item data. </p>

#### `deleteDomain`

``` purescript
deleteDomain :: forall eff. DeleteDomainRequest -> Aff (err :: RequestError | eff) Unit
```

<p> The <code>DeleteDomain</code> operation deletes a domain. Any items (and their attributes) in the domain are deleted as well. The <code>DeleteDomain</code> operation might take 10 or more seconds to complete. </p> <note> Running <code>DeleteDomain</code> on a domain that does not exist or running the function multiple times using the same domain name will not result in an error response. </note>

#### `domainMetadata`

``` purescript
domainMetadata :: forall eff. DomainMetadataRequest -> Aff (err :: RequestError | eff) DomainMetadataResult
```

<p> Returns information about the domain, including when the domain was created, the number of items and attributes in the domain, and the size of the attribute names and values. </p>

#### `getAttributes`

``` purescript
getAttributes :: forall eff. GetAttributesRequest -> Aff (err :: RequestError | eff) GetAttributesResult
```

<p> Returns all of the attributes associated with the specified item. Optionally, the attributes returned can be limited to one or more attributes by specifying an attribute name parameter. </p> <p> If the item does not exist on the replica that was accessed for this operation, an empty set is returned. The system does not return an error as it cannot guarantee the item does not exist on other replicas. </p> <note> If GetAttributes is called without being passed any attribute names, all the attributes for the item are returned. </note>

#### `listDomains`

``` purescript
listDomains :: forall eff. ListDomainsRequest -> Aff (err :: RequestError | eff) ListDomainsResult
```

<p> The <code>ListDomains</code> operation lists all domains associated with the Access Key ID. It returns domain names up to the limit set by <a href="#MaxNumberOfDomains">MaxNumberOfDomains</a>. A <a href="#NextToken">NextToken</a> is returned if there are more than <code>MaxNumberOfDomains</code> domains. Calling <code>ListDomains</code> successive times with the <code>NextToken</code> provided by the operation returns up to <code>MaxNumberOfDomains</code> more domain names with each successive operation call. </p>

#### `putAttributes`

``` purescript
putAttributes :: forall eff. PutAttributesRequest -> Aff (err :: RequestError | eff) Unit
```

<p> The PutAttributes operation creates or replaces attributes in an item. The client may specify new attributes using a combination of the <code>Attribute.X.Name</code> and <code>Attribute.X.Value</code> parameters. The client specifies the first attribute by the parameters <code>Attribute.0.Name</code> and <code>Attribute.0.Value</code>, the second attribute by the parameters <code>Attribute.1.Name</code> and <code>Attribute.1.Value</code>, and so on. </p> <p> Attributes are uniquely identified in an item by their name/value combination. For example, a single item can have the attributes <code>{ "first_name", "first_value" }</code> and <code>{ "first_name", second_value" }</code>. However, it cannot have two attribute instances where both the <code>Attribute.X.Name</code> and <code>Attribute.X.Value</code> are the same. </p> <p> Optionally, the requestor can supply the <code>Replace</code> parameter for each individual attribute. Setting this value to <code>true</code> causes the new attribute value to replace the existing attribute value(s). For example, if an item has the attributes <code>{ 'a', '1' }</code>, <code>{ 'b', '2'}</code> and <code>{ 'b', '3' }</code> and the requestor calls <code>PutAttributes</code> using the attributes <code>{ 'b', '4' }</code> with the <code>Replace</code> parameter set to true, the final attributes of the item are changed to <code>{ 'a', '1' }</code> and <code>{ 'b', '4' }</code>, which replaces the previous values of the 'b' attribute with the new value. </p> <note> Using <code>PutAttributes</code> to replace attribute values that do not exist will not result in an error response. </note> <p> You cannot specify an empty string as an attribute name. </p> <p> Because Amazon SimpleDB makes multiple copies of client data and uses an eventual consistency update model, an immediate <a>GetAttributes</a> or <a>Select</a> operation (read) immediately after a <a>PutAttributes</a> or <a>DeleteAttributes</a> operation (write) might not return the updated data. </p> <p> The following limitations are enforced for this operation: <ul> <li>256 total attribute name-value pairs per item</li> <li>One billion attributes per domain</li> <li>10 GB of total user data storage per domain</li> </ul> </p>

#### `select`

``` purescript
select :: forall eff. SelectRequest -> Aff (err :: RequestError | eff) SelectResult
```

<p> The <code>Select</code> operation returns a set of attributes for <code>ItemNames</code> that match the select expression. <code>Select</code> is similar to the standard SQL SELECT statement. </p> <p> The total size of the response cannot exceed 1 MB in total size. Amazon SimpleDB automatically adjusts the number of items returned per page to enforce this limit. For example, if the client asks to retrieve 2500 items, but each individual item is 10 kB in size, the system returns 100 items and an appropriate <code>NextToken</code> so the client can access the next page of results. </p> <p> For information on how to construct select expressions, see Using Select to Create Amazon SimpleDB Queries in the Developer Guide. </p>

#### `Attribute`

``` purescript
newtype Attribute
  = Attribute { "Name" :: String, "AlternateNameEncoding" :: NullOrUndefined (String), "Value" :: String, "AlternateValueEncoding" :: NullOrUndefined (String) }
```

<p></p>

#### `AttributeDoesNotExist`

``` purescript
newtype AttributeDoesNotExist
  = AttributeDoesNotExist { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>The specified attribute does not exist.</p>

#### `AttributeList`

``` purescript
newtype AttributeList
  = AttributeList (Array Attribute)
```

#### `AttributeNameList`

``` purescript
newtype AttributeNameList
  = AttributeNameList (Array String)
```

#### `BatchDeleteAttributesRequest`

``` purescript
newtype BatchDeleteAttributesRequest
  = BatchDeleteAttributesRequest { "DomainName" :: String, "Items" :: DeletableItemList }
```

#### `BatchPutAttributesRequest`

``` purescript
newtype BatchPutAttributesRequest
  = BatchPutAttributesRequest { "DomainName" :: String, "Items" :: ReplaceableItemList }
```

#### `CreateDomainRequest`

``` purescript
newtype CreateDomainRequest
  = CreateDomainRequest { "DomainName" :: String }
```

#### `DeletableAttribute`

``` purescript
newtype DeletableAttribute
  = DeletableAttribute { "Name" :: String, "Value" :: NullOrUndefined (String) }
```

<p></p>

#### `DeletableAttributeList`

``` purescript
newtype DeletableAttributeList
  = DeletableAttributeList (Array DeletableAttribute)
```

#### `DeletableItem`

``` purescript
newtype DeletableItem
  = DeletableItem { "Name" :: String, "Attributes" :: NullOrUndefined (DeletableAttributeList) }
```

#### `DeletableItemList`

``` purescript
newtype DeletableItemList
  = DeletableItemList (Array DeletableItem)
```

#### `DeleteAttributesRequest`

``` purescript
newtype DeleteAttributesRequest
  = DeleteAttributesRequest { "DomainName" :: String, "ItemName" :: String, "Attributes" :: NullOrUndefined (DeletableAttributeList), "Expected" :: NullOrUndefined (UpdateCondition) }
```

#### `DeleteDomainRequest`

``` purescript
newtype DeleteDomainRequest
  = DeleteDomainRequest { "DomainName" :: String }
```

#### `DomainMetadataRequest`

``` purescript
newtype DomainMetadataRequest
  = DomainMetadataRequest { "DomainName" :: String }
```

#### `DomainMetadataResult`

``` purescript
newtype DomainMetadataResult
  = DomainMetadataResult { "ItemCount" :: NullOrUndefined (Int), "ItemNamesSizeBytes" :: NullOrUndefined (Number), "AttributeNameCount" :: NullOrUndefined (Int), "AttributeNamesSizeBytes" :: NullOrUndefined (Number), "AttributeValueCount" :: NullOrUndefined (Int), "AttributeValuesSizeBytes" :: NullOrUndefined (Number), "Number" :: NullOrUndefined (Int) }
```

#### `DomainNameList`

``` purescript
newtype DomainNameList
  = DomainNameList (Array String)
```

#### `DuplicateItemName`

``` purescript
newtype DuplicateItemName
  = DuplicateItemName { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>The item name was specified more than once. </p>

#### `GetAttributesRequest`

``` purescript
newtype GetAttributesRequest
  = GetAttributesRequest { "DomainName" :: String, "ItemName" :: String, "AttributeNames" :: NullOrUndefined (AttributeNameList), "ConsistentRead" :: NullOrUndefined (Boolean) }
```

#### `GetAttributesResult`

``` purescript
newtype GetAttributesResult
  = GetAttributesResult { "Attributes" :: NullOrUndefined (AttributeList) }
```

#### `InvalidNextToken`

``` purescript
newtype InvalidNextToken
  = InvalidNextToken { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>The specified NextToken is not valid. </p>

#### `InvalidNumberPredicates`

``` purescript
newtype InvalidNumberPredicates
  = InvalidNumberPredicates { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>Too many predicates exist in the query expression.</p>

#### `InvalidNumberValueTests`

``` purescript
newtype InvalidNumberValueTests
  = InvalidNumberValueTests { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>Too many predicates exist in the query expression.</p>

#### `InvalidParameterValue`

``` purescript
newtype InvalidParameterValue
  = InvalidParameterValue { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>The value for a parameter is invalid.</p>

#### `InvalidQueryExpression`

``` purescript
newtype InvalidQueryExpression
  = InvalidQueryExpression { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>The specified query expression syntax is not valid.</p>

#### `Item`

``` purescript
newtype Item
  = Item { "Name" :: String, "AlternateNameEncoding" :: NullOrUndefined (String), "Attributes" :: AttributeList }
```

<p></p>

#### `ItemList`

``` purescript
newtype ItemList
  = ItemList (Array Item)
```

#### `ListDomainsRequest`

``` purescript
newtype ListDomainsRequest
  = ListDomainsRequest { "MaxNumberOfDomains" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

#### `ListDomainsResult`

``` purescript
newtype ListDomainsResult
  = ListDomainsResult { "DomainNames" :: NullOrUndefined (DomainNameList), "NextToken" :: NullOrUndefined (String) }
```

#### `MissingParameter`

``` purescript
newtype MissingParameter
  = MissingParameter { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>The request must contain the specified missing parameter.</p>

#### `NoSuchDomain`

``` purescript
newtype NoSuchDomain
  = NoSuchDomain { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>The specified domain does not exist.</p>

#### `NumberDomainAttributesExceeded`

``` purescript
newtype NumberDomainAttributesExceeded
  = NumberDomainAttributesExceeded { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>Too many attributes in this domain.</p>

#### `NumberDomainBytesExceeded`

``` purescript
newtype NumberDomainBytesExceeded
  = NumberDomainBytesExceeded { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>Too many bytes in this domain.</p>

#### `NumberDomainsExceeded`

``` purescript
newtype NumberDomainsExceeded
  = NumberDomainsExceeded { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>Too many domains exist per this account.</p>

#### `NumberItemAttributesExceeded`

``` purescript
newtype NumberItemAttributesExceeded
  = NumberItemAttributesExceeded { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>Too many attributes in this item.</p>

#### `NumberSubmittedAttributesExceeded`

``` purescript
newtype NumberSubmittedAttributesExceeded
  = NumberSubmittedAttributesExceeded { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>Too many attributes exist in a single call.</p>

#### `NumberSubmittedItemsExceeded`

``` purescript
newtype NumberSubmittedItemsExceeded
  = NumberSubmittedItemsExceeded { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>Too many items exist in a single call.</p>

#### `PutAttributesRequest`

``` purescript
newtype PutAttributesRequest
  = PutAttributesRequest { "DomainName" :: String, "ItemName" :: String, "Attributes" :: ReplaceableAttributeList, "Expected" :: NullOrUndefined (UpdateCondition) }
```

#### `ReplaceableAttribute`

``` purescript
newtype ReplaceableAttribute
  = ReplaceableAttribute { "Name" :: String, "Value" :: String, "Replace" :: NullOrUndefined (Boolean) }
```

<p></p>

#### `ReplaceableAttributeList`

``` purescript
newtype ReplaceableAttributeList
  = ReplaceableAttributeList (Array ReplaceableAttribute)
```

#### `ReplaceableItem`

``` purescript
newtype ReplaceableItem
  = ReplaceableItem { "Name" :: String, "Attributes" :: ReplaceableAttributeList }
```

<p></p>

#### `ReplaceableItemList`

``` purescript
newtype ReplaceableItemList
  = ReplaceableItemList (Array ReplaceableItem)
```

#### `RequestTimeout`

``` purescript
newtype RequestTimeout
  = RequestTimeout { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>A timeout occurred when attempting to query the specified domain with specified query expression.</p>

#### `SelectRequest`

``` purescript
newtype SelectRequest
  = SelectRequest { "SelectExpression" :: String, "NextToken" :: NullOrUndefined (String), "ConsistentRead" :: NullOrUndefined (Boolean) }
```

#### `SelectResult`

``` purescript
newtype SelectResult
  = SelectResult { "Items" :: NullOrUndefined (ItemList), "NextToken" :: NullOrUndefined (String) }
```

#### `TooManyRequestedAttributes`

``` purescript
newtype TooManyRequestedAttributes
  = TooManyRequestedAttributes { "BoxUsage" :: NullOrUndefined (Number) }
```

<p>Too many attributes requested.</p>

#### `UpdateCondition`

``` purescript
newtype UpdateCondition
  = UpdateCondition { "Name" :: NullOrUndefined (String), "Value" :: NullOrUndefined (String), "Exists" :: NullOrUndefined (Boolean) }
```

<p> Specifies the conditions under which data should be updated. If an update condition is specified for a request, the data will only be updated if the condition is satisfied. For example, if an attribute with a specific name and value exists, or if a specific attribute doesn't exist. </p>


