

-- | <p>An AWS Elemental MediaStore asset is an object, similar to an object in the Amazon S3 service. Objects are the fundamental entities that are stored in AWS Elemental MediaStore.</p>
module AWS.MediaStoreData where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MediaStoreData" :: String


-- | <p>Deletes an object at the specified path.</p>
deleteObject :: forall eff. DeleteObjectRequest -> Aff (err :: AWS.RequestError | eff) DeleteObjectResponse
deleteObject = AWS.request serviceName "deleteObject" 


-- | <p>Gets the headers for an object at the specified path.</p>
describeObject :: forall eff. DescribeObjectRequest -> Aff (err :: AWS.RequestError | eff) DescribeObjectResponse
describeObject = AWS.request serviceName "describeObject" 


-- | <p>Downloads the object at the specified path.</p>
getObject :: forall eff. GetObjectRequest -> Aff (err :: AWS.RequestError | eff) GetObjectResponse
getObject = AWS.request serviceName "getObject" 


-- | <p>Provides a list of metadata entries about folders and objects in the specified folder.</p>
listItems :: forall eff. ListItemsRequest -> Aff (err :: AWS.RequestError | eff) ListItemsResponse
listItems = AWS.request serviceName "listItems" 


-- | <p>Uploads an object to the specified path. Object sizes are limited to 10 MB.</p>
putObject :: forall eff. PutObjectRequest -> Aff (err :: AWS.RequestError | eff) PutObjectResponse
putObject = AWS.request serviceName "putObject" 


-- | <p>The specified container was not found for the specified account.</p>
newtype ContainerNotFoundException = ContainerNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeContainerNotFoundException :: Newtype ContainerNotFoundException _


newtype ContentRangePattern = ContentRangePattern String
derive instance newtypeContentRangePattern :: Newtype ContentRangePattern _


newtype ContentType = ContentType String
derive instance newtypeContentType :: Newtype ContentType _


newtype DeleteObjectRequest = DeleteObjectRequest 
  { "Path" :: (PathNaming)
  }
derive instance newtypeDeleteObjectRequest :: Newtype DeleteObjectRequest _


newtype DeleteObjectResponse = DeleteObjectResponse 
  { 
  }
derive instance newtypeDeleteObjectResponse :: Newtype DeleteObjectResponse _


newtype DescribeObjectRequest = DescribeObjectRequest 
  { "Path" :: (PathNaming)
  }
derive instance newtypeDescribeObjectRequest :: Newtype DescribeObjectRequest _


newtype DescribeObjectResponse = DescribeObjectResponse 
  { "ETag" :: NullOrUndefined (ETag)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "ContentLength" :: NullOrUndefined (NonNegativeLong)
  , "CacheControl" :: NullOrUndefined (StringPrimitive)
  , "LastModified" :: NullOrUndefined (TimeStamp)
  }
derive instance newtypeDescribeObjectResponse :: Newtype DescribeObjectResponse _


newtype ETag = ETag String
derive instance newtypeETag :: Newtype ETag _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype GetObjectRequest = GetObjectRequest 
  { "Path" :: (PathNaming)
  , "Range" :: NullOrUndefined (RangePattern)
  }
derive instance newtypeGetObjectRequest :: Newtype GetObjectRequest _


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
derive instance newtypeGetObjectResponse :: Newtype GetObjectResponse _


-- | <p>The service is temporarily unavailable.</p>
newtype InternalServerError = InternalServerError 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _


-- | <p>A metadata entry for a folder or object.</p>
newtype Item = Item 
  { "Name" :: NullOrUndefined (ItemName)
  , "Type" :: NullOrUndefined (ItemType)
  , "ETag" :: NullOrUndefined (ETag)
  , "LastModified" :: NullOrUndefined (TimeStamp)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "ContentLength" :: NullOrUndefined (NonNegativeLong)
  }
derive instance newtypeItem :: Newtype Item _


newtype ItemList = ItemList (Array Item)
derive instance newtypeItemList :: Newtype ItemList _


newtype ItemName = ItemName String
derive instance newtypeItemName :: Newtype ItemName _


newtype ItemType = ItemType String
derive instance newtypeItemType :: Newtype ItemType _


newtype ListItemsRequest = ListItemsRequest 
  { "Path" :: NullOrUndefined (ListPathNaming)
  , "MaxResults" :: NullOrUndefined (ListLimit)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListItemsRequest :: Newtype ListItemsRequest _


newtype ListItemsResponse = ListItemsResponse 
  { "Items" :: NullOrUndefined (ItemList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListItemsResponse :: Newtype ListItemsResponse _


newtype ListLimit = ListLimit Int
derive instance newtypeListLimit :: Newtype ListLimit _


newtype ListPathNaming = ListPathNaming String
derive instance newtypeListPathNaming :: Newtype ListPathNaming _


newtype NonNegativeLong = NonNegativeLong Number
derive instance newtypeNonNegativeLong :: Newtype NonNegativeLong _


-- | <p>Could not perform an operation on an object that does not exist.</p>
newtype ObjectNotFoundException = ObjectNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeObjectNotFoundException :: Newtype ObjectNotFoundException _


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _


newtype PathNaming = PathNaming String
derive instance newtypePathNaming :: Newtype PathNaming _


newtype PayloadBlob = PayloadBlob String
derive instance newtypePayloadBlob :: Newtype PayloadBlob _


newtype PutObjectRequest = PutObjectRequest 
  { "Body" :: (PayloadBlob)
  , "Path" :: (PathNaming)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "CacheControl" :: NullOrUndefined (StringPrimitive)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  }
derive instance newtypePutObjectRequest :: Newtype PutObjectRequest _


newtype PutObjectResponse = PutObjectResponse 
  { "ContentSHA256" :: NullOrUndefined (SHA256Hash)
  , "ETag" :: NullOrUndefined (ETag)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  }
derive instance newtypePutObjectResponse :: Newtype PutObjectResponse _


newtype RangePattern = RangePattern String
derive instance newtypeRangePattern :: Newtype RangePattern _


-- | <p>The requested content range is not valid.</p>
newtype RequestedRangeNotSatisfiableException = RequestedRangeNotSatisfiableException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeRequestedRangeNotSatisfiableException :: Newtype RequestedRangeNotSatisfiableException _


newtype SHA256Hash = SHA256Hash String
derive instance newtypeSHA256Hash :: Newtype SHA256Hash _


newtype StorageClass = StorageClass String
derive instance newtypeStorageClass :: Newtype StorageClass _


newtype StringPrimitive = StringPrimitive String
derive instance newtypeStringPrimitive :: Newtype StringPrimitive _


newtype TimeStamp = TimeStamp Number
derive instance newtypeTimeStamp :: Newtype TimeStamp _


newtype StatusCode' = StatusCode' Int
derive instance newtypeStatusCode' :: Newtype StatusCode' _
