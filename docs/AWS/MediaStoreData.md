## Module AWS.MediaStoreData

<p>An AWS Elemental MediaStore asset is an object, similar to an object in the Amazon S3 service. Objects are the fundamental entities that are stored in AWS Elemental MediaStore.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `deleteObject`

``` purescript
deleteObject :: forall eff. DeleteObjectRequest -> Aff (err :: RequestError | eff) DeleteObjectResponse
```

<p>Deletes an object at the specified path.</p>

#### `describeObject`

``` purescript
describeObject :: forall eff. DescribeObjectRequest -> Aff (err :: RequestError | eff) DescribeObjectResponse
```

<p>Gets the headers for an object at the specified path.</p>

#### `getObject`

``` purescript
getObject :: forall eff. GetObjectRequest -> Aff (err :: RequestError | eff) GetObjectResponse
```

<p>Downloads the object at the specified path.</p>

#### `listItems`

``` purescript
listItems :: forall eff. ListItemsRequest -> Aff (err :: RequestError | eff) ListItemsResponse
```

<p>Provides a list of metadata entries about folders and objects in the specified folder.</p>

#### `putObject`

``` purescript
putObject :: forall eff. PutObjectRequest -> Aff (err :: RequestError | eff) PutObjectResponse
```

<p>Uploads an object to the specified path. Object sizes are limited to 10 MB.</p>

#### `ContainerNotFoundException`

``` purescript
newtype ContainerNotFoundException
  = ContainerNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified container was not found for the specified account.</p>

#### `ContentRangePattern`

``` purescript
newtype ContentRangePattern
  = ContentRangePattern String
```

#### `ContentType`

``` purescript
newtype ContentType
  = ContentType String
```

#### `DeleteObjectRequest`

``` purescript
newtype DeleteObjectRequest
  = DeleteObjectRequest { "Path" :: PathNaming }
```

#### `DeleteObjectResponse`

``` purescript
newtype DeleteObjectResponse
  = DeleteObjectResponse {  }
```

#### `DescribeObjectRequest`

``` purescript
newtype DescribeObjectRequest
  = DescribeObjectRequest { "Path" :: PathNaming }
```

#### `DescribeObjectResponse`

``` purescript
newtype DescribeObjectResponse
  = DescribeObjectResponse { "ETag" :: NullOrUndefined (ETag), "ContentType" :: NullOrUndefined (ContentType), "ContentLength" :: NullOrUndefined (NonNegativeLong), "CacheControl" :: NullOrUndefined (StringPrimitive), "LastModified" :: NullOrUndefined (TimeStamp) }
```

#### `ETag`

``` purescript
newtype ETag
  = ETag String
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `GetObjectRequest`

``` purescript
newtype GetObjectRequest
  = GetObjectRequest { "Path" :: PathNaming, "Range" :: NullOrUndefined (RangePattern) }
```

#### `GetObjectResponse`

``` purescript
newtype GetObjectResponse
  = GetObjectResponse { "Body" :: NullOrUndefined (PayloadBlob), "CacheControl" :: NullOrUndefined (StringPrimitive), "ContentRange" :: NullOrUndefined (ContentRangePattern), "ContentLength" :: NullOrUndefined (NonNegativeLong), "ContentType" :: NullOrUndefined (ContentType), "ETag" :: NullOrUndefined (ETag), "LastModified" :: NullOrUndefined (TimeStamp), "StatusCode" :: StatusCode' }
```

#### `InternalServerError`

``` purescript
newtype InternalServerError
  = InternalServerError { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The service is temporarily unavailable.</p>

#### `Item`

``` purescript
newtype Item
  = Item { "Name" :: NullOrUndefined (ItemName), "Type" :: NullOrUndefined (ItemType), "ETag" :: NullOrUndefined (ETag), "LastModified" :: NullOrUndefined (TimeStamp), "ContentType" :: NullOrUndefined (ContentType), "ContentLength" :: NullOrUndefined (NonNegativeLong) }
```

<p>A metadata entry for a folder or object.</p>

#### `ItemList`

``` purescript
newtype ItemList
  = ItemList (Array Item)
```

#### `ItemName`

``` purescript
newtype ItemName
  = ItemName String
```

#### `ItemType`

``` purescript
newtype ItemType
  = ItemType String
```

#### `ListItemsRequest`

``` purescript
newtype ListItemsRequest
  = ListItemsRequest { "Path" :: NullOrUndefined (ListPathNaming), "MaxResults" :: NullOrUndefined (ListLimit), "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `ListItemsResponse`

``` purescript
newtype ListItemsResponse
  = ListItemsResponse { "Items" :: NullOrUndefined (ItemList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `ListLimit`

``` purescript
newtype ListLimit
  = ListLimit Int
```

#### `ListPathNaming`

``` purescript
newtype ListPathNaming
  = ListPathNaming String
```

#### `NonNegativeLong`

``` purescript
newtype NonNegativeLong
  = NonNegativeLong Number
```

#### `ObjectNotFoundException`

``` purescript
newtype ObjectNotFoundException
  = ObjectNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Could not perform an operation on an object that does not exist.</p>

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

#### `PathNaming`

``` purescript
newtype PathNaming
  = PathNaming String
```

#### `PayloadBlob`

``` purescript
newtype PayloadBlob
  = PayloadBlob String
```

#### `PutObjectRequest`

``` purescript
newtype PutObjectRequest
  = PutObjectRequest { "Body" :: PayloadBlob, "Path" :: PathNaming, "ContentType" :: NullOrUndefined (ContentType), "CacheControl" :: NullOrUndefined (StringPrimitive), "StorageClass" :: NullOrUndefined (StorageClass) }
```

#### `PutObjectResponse`

``` purescript
newtype PutObjectResponse
  = PutObjectResponse { "ContentSHA256" :: NullOrUndefined (SHA256Hash), "ETag" :: NullOrUndefined (ETag), "StorageClass" :: NullOrUndefined (StorageClass) }
```

#### `RangePattern`

``` purescript
newtype RangePattern
  = RangePattern String
```

#### `RequestedRangeNotSatisfiableException`

``` purescript
newtype RequestedRangeNotSatisfiableException
  = RequestedRangeNotSatisfiableException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The requested content range is not valid.</p>

#### `SHA256Hash`

``` purescript
newtype SHA256Hash
  = SHA256Hash String
```

#### `StorageClass`

``` purescript
newtype StorageClass
  = StorageClass String
```

#### `StringPrimitive`

``` purescript
newtype StringPrimitive
  = StringPrimitive String
```

#### `TimeStamp`

``` purescript
newtype TimeStamp
  = TimeStamp Number
```

#### `StatusCode'`

``` purescript
newtype StatusCode'
  = StatusCode' Int
```


