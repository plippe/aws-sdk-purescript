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

##### Instances
``` purescript
Newtype ContainerNotFoundException _
```

#### `ContentRangePattern`

``` purescript
newtype ContentRangePattern
  = ContentRangePattern String
```

##### Instances
``` purescript
Newtype ContentRangePattern _
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

#### `DeleteObjectRequest`

``` purescript
newtype DeleteObjectRequest
  = DeleteObjectRequest { "Path" :: PathNaming }
```

##### Instances
``` purescript
Newtype DeleteObjectRequest _
```

#### `DeleteObjectResponse`

``` purescript
newtype DeleteObjectResponse
  = DeleteObjectResponse {  }
```

##### Instances
``` purescript
Newtype DeleteObjectResponse _
```

#### `DescribeObjectRequest`

``` purescript
newtype DescribeObjectRequest
  = DescribeObjectRequest { "Path" :: PathNaming }
```

##### Instances
``` purescript
Newtype DescribeObjectRequest _
```

#### `DescribeObjectResponse`

``` purescript
newtype DescribeObjectResponse
  = DescribeObjectResponse { "ETag" :: NullOrUndefined (ETag), "ContentType" :: NullOrUndefined (ContentType), "ContentLength" :: NullOrUndefined (NonNegativeLong), "CacheControl" :: NullOrUndefined (StringPrimitive), "LastModified" :: NullOrUndefined (TimeStamp) }
```

##### Instances
``` purescript
Newtype DescribeObjectResponse _
```

#### `ETag`

``` purescript
newtype ETag
  = ETag String
```

##### Instances
``` purescript
Newtype ETag _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `GetObjectRequest`

``` purescript
newtype GetObjectRequest
  = GetObjectRequest { "Path" :: PathNaming, "Range" :: NullOrUndefined (RangePattern) }
```

##### Instances
``` purescript
Newtype GetObjectRequest _
```

#### `GetObjectResponse`

``` purescript
newtype GetObjectResponse
  = GetObjectResponse { "Body" :: NullOrUndefined (PayloadBlob), "CacheControl" :: NullOrUndefined (StringPrimitive), "ContentRange" :: NullOrUndefined (ContentRangePattern), "ContentLength" :: NullOrUndefined (NonNegativeLong), "ContentType" :: NullOrUndefined (ContentType), "ETag" :: NullOrUndefined (ETag), "LastModified" :: NullOrUndefined (TimeStamp), "StatusCode" :: StatusCode' }
```

##### Instances
``` purescript
Newtype GetObjectResponse _
```

#### `InternalServerError`

``` purescript
newtype InternalServerError
  = InternalServerError { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The service is temporarily unavailable.</p>

##### Instances
``` purescript
Newtype InternalServerError _
```

#### `Item`

``` purescript
newtype Item
  = Item { "Name" :: NullOrUndefined (ItemName), "Type" :: NullOrUndefined (ItemType), "ETag" :: NullOrUndefined (ETag), "LastModified" :: NullOrUndefined (TimeStamp), "ContentType" :: NullOrUndefined (ContentType), "ContentLength" :: NullOrUndefined (NonNegativeLong) }
```

<p>A metadata entry for a folder or object.</p>

##### Instances
``` purescript
Newtype Item _
```

#### `ItemList`

``` purescript
newtype ItemList
  = ItemList (Array Item)
```

##### Instances
``` purescript
Newtype ItemList _
```

#### `ItemName`

``` purescript
newtype ItemName
  = ItemName String
```

##### Instances
``` purescript
Newtype ItemName _
```

#### `ItemType`

``` purescript
newtype ItemType
  = ItemType String
```

##### Instances
``` purescript
Newtype ItemType _
```

#### `ListItemsRequest`

``` purescript
newtype ListItemsRequest
  = ListItemsRequest { "Path" :: NullOrUndefined (ListPathNaming), "MaxResults" :: NullOrUndefined (ListLimit), "NextToken" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListItemsRequest _
```

#### `ListItemsResponse`

``` purescript
newtype ListItemsResponse
  = ListItemsResponse { "Items" :: NullOrUndefined (ItemList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListItemsResponse _
```

#### `ListLimit`

``` purescript
newtype ListLimit
  = ListLimit Int
```

##### Instances
``` purescript
Newtype ListLimit _
```

#### `ListPathNaming`

``` purescript
newtype ListPathNaming
  = ListPathNaming String
```

##### Instances
``` purescript
Newtype ListPathNaming _
```

#### `NonNegativeLong`

``` purescript
newtype NonNegativeLong
  = NonNegativeLong Number
```

##### Instances
``` purescript
Newtype NonNegativeLong _
```

#### `ObjectNotFoundException`

``` purescript
newtype ObjectNotFoundException
  = ObjectNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Could not perform an operation on an object that does not exist.</p>

##### Instances
``` purescript
Newtype ObjectNotFoundException _
```

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

##### Instances
``` purescript
Newtype PaginationToken _
```

#### `PathNaming`

``` purescript
newtype PathNaming
  = PathNaming String
```

##### Instances
``` purescript
Newtype PathNaming _
```

#### `PayloadBlob`

``` purescript
newtype PayloadBlob
  = PayloadBlob String
```

##### Instances
``` purescript
Newtype PayloadBlob _
```

#### `PutObjectRequest`

``` purescript
newtype PutObjectRequest
  = PutObjectRequest { "Body" :: PayloadBlob, "Path" :: PathNaming, "ContentType" :: NullOrUndefined (ContentType), "CacheControl" :: NullOrUndefined (StringPrimitive), "StorageClass" :: NullOrUndefined (StorageClass) }
```

##### Instances
``` purescript
Newtype PutObjectRequest _
```

#### `PutObjectResponse`

``` purescript
newtype PutObjectResponse
  = PutObjectResponse { "ContentSHA256" :: NullOrUndefined (SHA256Hash), "ETag" :: NullOrUndefined (ETag), "StorageClass" :: NullOrUndefined (StorageClass) }
```

##### Instances
``` purescript
Newtype PutObjectResponse _
```

#### `RangePattern`

``` purescript
newtype RangePattern
  = RangePattern String
```

##### Instances
``` purescript
Newtype RangePattern _
```

#### `RequestedRangeNotSatisfiableException`

``` purescript
newtype RequestedRangeNotSatisfiableException
  = RequestedRangeNotSatisfiableException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The requested content range is not valid.</p>

##### Instances
``` purescript
Newtype RequestedRangeNotSatisfiableException _
```

#### `SHA256Hash`

``` purescript
newtype SHA256Hash
  = SHA256Hash String
```

##### Instances
``` purescript
Newtype SHA256Hash _
```

#### `StorageClass`

``` purescript
newtype StorageClass
  = StorageClass String
```

##### Instances
``` purescript
Newtype StorageClass _
```

#### `StringPrimitive`

``` purescript
newtype StringPrimitive
  = StringPrimitive String
```

##### Instances
``` purescript
Newtype StringPrimitive _
```

#### `TimeStamp`

``` purescript
newtype TimeStamp
  = TimeStamp Number
```

##### Instances
``` purescript
Newtype TimeStamp _
```

#### `StatusCode'`

``` purescript
newtype StatusCode'
  = StatusCode' Int
```

##### Instances
``` purescript
Newtype StatusCode' _
```


