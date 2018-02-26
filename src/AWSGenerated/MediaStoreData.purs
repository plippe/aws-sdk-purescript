

-- | <p>An AWS Elemental MediaStore asset is an object, similar to an object in the Amazon S3 service. Objects are the fundamental entities that are stored in AWS Elemental MediaStore.</p>
module AWS.MediaStoreData where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MediaStoreData" :: String


-- | <p>Deletes an object at the specified path.</p>
deleteObject :: forall eff. DeleteObjectRequest -> Aff (err :: AWS.RequestError | eff) DeleteObjectResponse
deleteObject = AWS.request serviceName "DeleteObject" 


-- | <p>Gets the headers for an object at the specified path.</p>
describeObject :: forall eff. DescribeObjectRequest -> Aff (err :: AWS.RequestError | eff) DescribeObjectResponse
describeObject = AWS.request serviceName "DescribeObject" 


-- | <p>Downloads the object at the specified path.</p>
getObject :: forall eff. GetObjectRequest -> Aff (err :: AWS.RequestError | eff) GetObjectResponse
getObject = AWS.request serviceName "GetObject" 


-- | <p>Provides a list of metadata entries about folders and objects in the specified folder.</p>
listItems :: forall eff. ListItemsRequest -> Aff (err :: AWS.RequestError | eff) ListItemsResponse
listItems = AWS.request serviceName "ListItems" 


-- | <p>Uploads an object to the specified path. Object sizes are limited to 10 MB.</p>
putObject :: forall eff. PutObjectRequest -> Aff (err :: AWS.RequestError | eff) PutObjectResponse
putObject = AWS.request serviceName "PutObject" 


-- | <p>The specified container was not found for the specified account.</p>
newtype ContainerNotFoundException = ContainerNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ContentRangePattern = ContentRangePattern String


newtype ContentType = ContentType String


newtype DeleteObjectRequest = DeleteObjectRequest 
  { "Path" :: (PathNaming)
  }


newtype DeleteObjectResponse = DeleteObjectResponse 
  { 
  }


newtype DescribeObjectRequest = DescribeObjectRequest 
  { "Path" :: (PathNaming)
  }


newtype DescribeObjectResponse = DescribeObjectResponse 
  { "ETag" :: NullOrUndefined (ETag)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "ContentLength" :: NullOrUndefined (NonNegativeLong)
  , "CacheControl" :: NullOrUndefined (StringPrimitive)
  , "LastModified" :: NullOrUndefined (TimeStamp)
  }


newtype ETag = ETag String


newtype ErrorMessage = ErrorMessage String


newtype GetObjectRequest = GetObjectRequest 
  { "Path" :: (PathNaming)
  , "Range" :: NullOrUndefined (RangePattern)
  }


newtype GetObjectResponse = GetObjectResponse 
  { "Body" :: NullOrUndefined (PayloadBlob)
  , "CacheControl" :: NullOrUndefined (StringPrimitive)
  , "ContentRange" :: NullOrUndefined (ContentRangePattern)
  , "ContentLength" :: NullOrUndefined (NonNegativeLong)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "ETag" :: NullOrUndefined (ETag)
  , "LastModified" :: NullOrUndefined (TimeStamp)
  , "StatusCode" :: (StatusCode')
  }


-- | <p>The service is temporarily unavailable.</p>
newtype InternalServerError = InternalServerError 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>A metadata entry for a folder or object.</p>
newtype Item = Item 
  { "Name" :: NullOrUndefined (ItemName)
  , "Type" :: NullOrUndefined (ItemType)
  , "ETag" :: NullOrUndefined (ETag)
  , "LastModified" :: NullOrUndefined (TimeStamp)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "ContentLength" :: NullOrUndefined (NonNegativeLong)
  }


newtype ItemList = ItemList (Array Item)


newtype ItemName = ItemName String


newtype ItemType = ItemType String


newtype ListItemsRequest = ListItemsRequest 
  { "Path" :: NullOrUndefined (ListPathNaming)
  , "MaxResults" :: NullOrUndefined (ListLimit)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype ListItemsResponse = ListItemsResponse 
  { "Items" :: NullOrUndefined (ItemList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype ListLimit = ListLimit Int


newtype ListPathNaming = ListPathNaming String


newtype NonNegativeLong = NonNegativeLong Number


-- | <p>Could not perform an operation on an object that does not exist.</p>
newtype ObjectNotFoundException = ObjectNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype PaginationToken = PaginationToken String


newtype PathNaming = PathNaming String


newtype PayloadBlob = PayloadBlob String


newtype PutObjectRequest = PutObjectRequest 
  { "Body" :: (PayloadBlob)
  , "Path" :: (PathNaming)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "CacheControl" :: NullOrUndefined (StringPrimitive)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  }


newtype PutObjectResponse = PutObjectResponse 
  { "ContentSHA256" :: NullOrUndefined (SHA256Hash)
  , "ETag" :: NullOrUndefined (ETag)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  }


newtype RangePattern = RangePattern String


-- | <p>The requested content range is not valid.</p>
newtype RequestedRangeNotSatisfiableException = RequestedRangeNotSatisfiableException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype SHA256Hash = SHA256Hash String


newtype StorageClass = StorageClass String


newtype StringPrimitive = StringPrimitive String


newtype TimeStamp = TimeStamp Number


newtype StatusCode' = StatusCode' Int
