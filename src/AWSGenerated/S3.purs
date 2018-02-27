

module AWS.S3 where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "S3" :: String


-- | <p>Aborts a multipart upload.</p><p>To verify that all parts have been removed, so you don't get charged for the part storage, you should call the List Parts operation and ensure the parts list is empty.</p>
abortMultipartUpload :: forall eff. AbortMultipartUploadRequest -> Aff (err :: AWS.RequestError | eff) AbortMultipartUploadOutput
abortMultipartUpload = AWS.request serviceName "AbortMultipartUpload" 


-- | Completes a multipart upload by assembling previously uploaded parts.
completeMultipartUpload :: forall eff. CompleteMultipartUploadRequest -> Aff (err :: AWS.RequestError | eff) CompleteMultipartUploadOutput
completeMultipartUpload = AWS.request serviceName "CompleteMultipartUpload" 


-- | Creates a copy of an object that is already stored in Amazon S3.
copyObject :: forall eff. CopyObjectRequest -> Aff (err :: AWS.RequestError | eff) CopyObjectOutput
copyObject = AWS.request serviceName "CopyObject" 


-- | Creates a new bucket.
createBucket :: forall eff. CreateBucketRequest -> Aff (err :: AWS.RequestError | eff) CreateBucketOutput
createBucket = AWS.request serviceName "CreateBucket" 


-- | <p>Initiates a multipart upload and returns an upload ID.</p><p><b>Note:</b> After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.</p>
createMultipartUpload :: forall eff. CreateMultipartUploadRequest -> Aff (err :: AWS.RequestError | eff) CreateMultipartUploadOutput
createMultipartUpload = AWS.request serviceName "CreateMultipartUpload" 


-- | Deletes the bucket. All objects (including all object versions and Delete Markers) in the bucket must be deleted before the bucket itself can be deleted.
deleteBucket :: forall eff. DeleteBucketRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteBucket = AWS.request serviceName "DeleteBucket" 


-- | Deletes an analytics configuration for the bucket (specified by the analytics configuration ID).
deleteBucketAnalyticsConfiguration :: forall eff. DeleteBucketAnalyticsConfigurationRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteBucketAnalyticsConfiguration = AWS.request serviceName "DeleteBucketAnalyticsConfiguration" 


-- | Deletes the cors configuration information set for the bucket.
deleteBucketCors :: forall eff. DeleteBucketCorsRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteBucketCors = AWS.request serviceName "DeleteBucketCors" 


-- | Deletes the server-side encryption configuration from the bucket.
deleteBucketEncryption :: forall eff. DeleteBucketEncryptionRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteBucketEncryption = AWS.request serviceName "DeleteBucketEncryption" 


-- | Deletes an inventory configuration (identified by the inventory ID) from the bucket.
deleteBucketInventoryConfiguration :: forall eff. DeleteBucketInventoryConfigurationRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteBucketInventoryConfiguration = AWS.request serviceName "DeleteBucketInventoryConfiguration" 


-- | Deletes the lifecycle configuration from the bucket.
deleteBucketLifecycle :: forall eff. DeleteBucketLifecycleRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteBucketLifecycle = AWS.request serviceName "DeleteBucketLifecycle" 


-- | Deletes a metrics configuration (specified by the metrics configuration ID) from the bucket.
deleteBucketMetricsConfiguration :: forall eff. DeleteBucketMetricsConfigurationRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteBucketMetricsConfiguration = AWS.request serviceName "DeleteBucketMetricsConfiguration" 


-- | Deletes the policy from the bucket.
deleteBucketPolicy :: forall eff. DeleteBucketPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteBucketPolicy = AWS.request serviceName "DeleteBucketPolicy" 


-- | Deletes the replication configuration from the bucket.
deleteBucketReplication :: forall eff. DeleteBucketReplicationRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteBucketReplication = AWS.request serviceName "DeleteBucketReplication" 


-- | Deletes the tags from the bucket.
deleteBucketTagging :: forall eff. DeleteBucketTaggingRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteBucketTagging = AWS.request serviceName "DeleteBucketTagging" 


-- | This operation removes the website configuration from the bucket.
deleteBucketWebsite :: forall eff. DeleteBucketWebsiteRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteBucketWebsite = AWS.request serviceName "DeleteBucketWebsite" 


-- | Removes the null version (if there is one) of an object and inserts a delete marker, which becomes the latest version of the object. If there isn't a null version, Amazon S3 does not remove any objects.
deleteObject :: forall eff. DeleteObjectRequest -> Aff (err :: AWS.RequestError | eff) DeleteObjectOutput
deleteObject = AWS.request serviceName "DeleteObject" 


-- | Removes the tag-set from an existing object.
deleteObjectTagging :: forall eff. DeleteObjectTaggingRequest -> Aff (err :: AWS.RequestError | eff) DeleteObjectTaggingOutput
deleteObjectTagging = AWS.request serviceName "DeleteObjectTagging" 


-- | This operation enables you to delete multiple objects from a bucket using a single HTTP request. You may specify up to 1000 keys.
deleteObjects :: forall eff. DeleteObjectsRequest -> Aff (err :: AWS.RequestError | eff) DeleteObjectsOutput
deleteObjects = AWS.request serviceName "DeleteObjects" 


-- | Returns the accelerate configuration of a bucket.
getBucketAccelerateConfiguration :: forall eff. GetBucketAccelerateConfigurationRequest -> Aff (err :: AWS.RequestError | eff) GetBucketAccelerateConfigurationOutput
getBucketAccelerateConfiguration = AWS.request serviceName "GetBucketAccelerateConfiguration" 


-- | Gets the access control policy for the bucket.
getBucketAcl :: forall eff. GetBucketAclRequest -> Aff (err :: AWS.RequestError | eff) GetBucketAclOutput
getBucketAcl = AWS.request serviceName "GetBucketAcl" 


-- | Gets an analytics configuration for the bucket (specified by the analytics configuration ID).
getBucketAnalyticsConfiguration :: forall eff. GetBucketAnalyticsConfigurationRequest -> Aff (err :: AWS.RequestError | eff) GetBucketAnalyticsConfigurationOutput
getBucketAnalyticsConfiguration = AWS.request serviceName "GetBucketAnalyticsConfiguration" 


-- | Returns the cors configuration for the bucket.
getBucketCors :: forall eff. GetBucketCorsRequest -> Aff (err :: AWS.RequestError | eff) GetBucketCorsOutput
getBucketCors = AWS.request serviceName "GetBucketCors" 


-- | Returns the server-side encryption configuration of a bucket.
getBucketEncryption :: forall eff. GetBucketEncryptionRequest -> Aff (err :: AWS.RequestError | eff) GetBucketEncryptionOutput
getBucketEncryption = AWS.request serviceName "GetBucketEncryption" 


-- | Returns an inventory configuration (identified by the inventory ID) from the bucket.
getBucketInventoryConfiguration :: forall eff. GetBucketInventoryConfigurationRequest -> Aff (err :: AWS.RequestError | eff) GetBucketInventoryConfigurationOutput
getBucketInventoryConfiguration = AWS.request serviceName "GetBucketInventoryConfiguration" 


-- | Deprecated, see the GetBucketLifecycleConfiguration operation.
getBucketLifecycle :: forall eff. GetBucketLifecycleRequest -> Aff (err :: AWS.RequestError | eff) GetBucketLifecycleOutput
getBucketLifecycle = AWS.request serviceName "GetBucketLifecycle" 


-- | Returns the lifecycle configuration information set on the bucket.
getBucketLifecycleConfiguration :: forall eff. GetBucketLifecycleConfigurationRequest -> Aff (err :: AWS.RequestError | eff) GetBucketLifecycleConfigurationOutput
getBucketLifecycleConfiguration = AWS.request serviceName "GetBucketLifecycleConfiguration" 


-- | Returns the region the bucket resides in.
getBucketLocation :: forall eff. GetBucketLocationRequest -> Aff (err :: AWS.RequestError | eff) GetBucketLocationOutput
getBucketLocation = AWS.request serviceName "GetBucketLocation" 


-- | Returns the logging status of a bucket and the permissions users have to view and modify that status. To use GET, you must be the bucket owner.
getBucketLogging :: forall eff. GetBucketLoggingRequest -> Aff (err :: AWS.RequestError | eff) GetBucketLoggingOutput
getBucketLogging = AWS.request serviceName "GetBucketLogging" 


-- | Gets a metrics configuration (specified by the metrics configuration ID) from the bucket.
getBucketMetricsConfiguration :: forall eff. GetBucketMetricsConfigurationRequest -> Aff (err :: AWS.RequestError | eff) GetBucketMetricsConfigurationOutput
getBucketMetricsConfiguration = AWS.request serviceName "GetBucketMetricsConfiguration" 


-- | Deprecated, see the GetBucketNotificationConfiguration operation.
getBucketNotification :: forall eff. GetBucketNotificationConfigurationRequest -> Aff (err :: AWS.RequestError | eff) NotificationConfigurationDeprecated
getBucketNotification = AWS.request serviceName "GetBucketNotification" 


-- | Returns the notification configuration of a bucket.
getBucketNotificationConfiguration :: forall eff. GetBucketNotificationConfigurationRequest -> Aff (err :: AWS.RequestError | eff) NotificationConfiguration
getBucketNotificationConfiguration = AWS.request serviceName "GetBucketNotificationConfiguration" 


-- | Returns the policy of a specified bucket.
getBucketPolicy :: forall eff. GetBucketPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetBucketPolicyOutput
getBucketPolicy = AWS.request serviceName "GetBucketPolicy" 


-- | Returns the replication configuration of a bucket.
getBucketReplication :: forall eff. GetBucketReplicationRequest -> Aff (err :: AWS.RequestError | eff) GetBucketReplicationOutput
getBucketReplication = AWS.request serviceName "GetBucketReplication" 


-- | Returns the request payment configuration of a bucket.
getBucketRequestPayment :: forall eff. GetBucketRequestPaymentRequest -> Aff (err :: AWS.RequestError | eff) GetBucketRequestPaymentOutput
getBucketRequestPayment = AWS.request serviceName "GetBucketRequestPayment" 


-- | Returns the tag set associated with the bucket.
getBucketTagging :: forall eff. GetBucketTaggingRequest -> Aff (err :: AWS.RequestError | eff) GetBucketTaggingOutput
getBucketTagging = AWS.request serviceName "GetBucketTagging" 


-- | Returns the versioning state of a bucket.
getBucketVersioning :: forall eff. GetBucketVersioningRequest -> Aff (err :: AWS.RequestError | eff) GetBucketVersioningOutput
getBucketVersioning = AWS.request serviceName "GetBucketVersioning" 


-- | Returns the website configuration for a bucket.
getBucketWebsite :: forall eff. GetBucketWebsiteRequest -> Aff (err :: AWS.RequestError | eff) GetBucketWebsiteOutput
getBucketWebsite = AWS.request serviceName "GetBucketWebsite" 


-- | Retrieves objects from Amazon S3.
getObject :: forall eff. GetObjectRequest -> Aff (err :: AWS.RequestError | eff) GetObjectOutput
getObject = AWS.request serviceName "GetObject" 


-- | Returns the access control list (ACL) of an object.
getObjectAcl :: forall eff. GetObjectAclRequest -> Aff (err :: AWS.RequestError | eff) GetObjectAclOutput
getObjectAcl = AWS.request serviceName "GetObjectAcl" 


-- | Returns the tag-set of an object.
getObjectTagging :: forall eff. GetObjectTaggingRequest -> Aff (err :: AWS.RequestError | eff) GetObjectTaggingOutput
getObjectTagging = AWS.request serviceName "GetObjectTagging" 


-- | Return torrent files from a bucket.
getObjectTorrent :: forall eff. GetObjectTorrentRequest -> Aff (err :: AWS.RequestError | eff) GetObjectTorrentOutput
getObjectTorrent = AWS.request serviceName "GetObjectTorrent" 


-- | This operation is useful to determine if a bucket exists and you have permission to access it.
headBucket :: forall eff. HeadBucketRequest -> Aff (err :: AWS.RequestError | eff) Unit
headBucket = AWS.request serviceName "HeadBucket" 


-- | The HEAD operation retrieves metadata from an object without returning the object itself. This operation is useful if you're only interested in an object's metadata. To use HEAD, you must have READ access to the object.
headObject :: forall eff. HeadObjectRequest -> Aff (err :: AWS.RequestError | eff) HeadObjectOutput
headObject = AWS.request serviceName "HeadObject" 


-- | Lists the analytics configurations for the bucket.
listBucketAnalyticsConfigurations :: forall eff. ListBucketAnalyticsConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) ListBucketAnalyticsConfigurationsOutput
listBucketAnalyticsConfigurations = AWS.request serviceName "ListBucketAnalyticsConfigurations" 


-- | Returns a list of inventory configurations for the bucket.
listBucketInventoryConfigurations :: forall eff. ListBucketInventoryConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) ListBucketInventoryConfigurationsOutput
listBucketInventoryConfigurations = AWS.request serviceName "ListBucketInventoryConfigurations" 


-- | Lists the metrics configurations for the bucket.
listBucketMetricsConfigurations :: forall eff. ListBucketMetricsConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) ListBucketMetricsConfigurationsOutput
listBucketMetricsConfigurations = AWS.request serviceName "ListBucketMetricsConfigurations" 


-- | Returns a list of all buckets owned by the authenticated sender of the request.
listBuckets :: forall eff.  Aff (err :: AWS.RequestError | eff) ListBucketsOutput
listBuckets = AWS.request serviceName "ListBuckets" unit


-- | This operation lists in-progress multipart uploads.
listMultipartUploads :: forall eff. ListMultipartUploadsRequest -> Aff (err :: AWS.RequestError | eff) ListMultipartUploadsOutput
listMultipartUploads = AWS.request serviceName "ListMultipartUploads" 


-- | Returns metadata about all of the versions of objects in a bucket.
listObjectVersions :: forall eff. ListObjectVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListObjectVersionsOutput
listObjectVersions = AWS.request serviceName "ListObjectVersions" 


-- | Returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket.
listObjects :: forall eff. ListObjectsRequest -> Aff (err :: AWS.RequestError | eff) ListObjectsOutput
listObjects = AWS.request serviceName "ListObjects" 


-- | Returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket. Note: ListObjectsV2 is the revised List Objects API and we recommend you use this revised API for new application development.
listObjectsV2 :: forall eff. ListObjectsV2Request -> Aff (err :: AWS.RequestError | eff) ListObjectsV2Output
listObjectsV2 = AWS.request serviceName "ListObjectsV2" 


-- | Lists the parts that have been uploaded for a specific multipart upload.
listParts :: forall eff. ListPartsRequest -> Aff (err :: AWS.RequestError | eff) ListPartsOutput
listParts = AWS.request serviceName "ListParts" 


-- | Sets the accelerate configuration of an existing bucket.
putBucketAccelerateConfiguration :: forall eff. PutBucketAccelerateConfigurationRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketAccelerateConfiguration = AWS.request serviceName "PutBucketAccelerateConfiguration" 


-- | Sets the permissions on a bucket using access control lists (ACL).
putBucketAcl :: forall eff. PutBucketAclRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketAcl = AWS.request serviceName "PutBucketAcl" 


-- | Sets an analytics configuration for the bucket (specified by the analytics configuration ID).
putBucketAnalyticsConfiguration :: forall eff. PutBucketAnalyticsConfigurationRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketAnalyticsConfiguration = AWS.request serviceName "PutBucketAnalyticsConfiguration" 


-- | Sets the cors configuration for a bucket.
putBucketCors :: forall eff. PutBucketCorsRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketCors = AWS.request serviceName "PutBucketCors" 


-- | Creates a new server-side encryption configuration (or replaces an existing one, if present).
putBucketEncryption :: forall eff. PutBucketEncryptionRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketEncryption = AWS.request serviceName "PutBucketEncryption" 


-- | Adds an inventory configuration (identified by the inventory ID) from the bucket.
putBucketInventoryConfiguration :: forall eff. PutBucketInventoryConfigurationRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketInventoryConfiguration = AWS.request serviceName "PutBucketInventoryConfiguration" 


-- | Deprecated, see the PutBucketLifecycleConfiguration operation.
putBucketLifecycle :: forall eff. PutBucketLifecycleRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketLifecycle = AWS.request serviceName "PutBucketLifecycle" 


-- | Sets lifecycle configuration for your bucket. If a lifecycle configuration exists, it replaces it.
putBucketLifecycleConfiguration :: forall eff. PutBucketLifecycleConfigurationRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketLifecycleConfiguration = AWS.request serviceName "PutBucketLifecycleConfiguration" 


-- | Set the logging parameters for a bucket and to specify permissions for who can view and modify the logging parameters. To set the logging status of a bucket, you must be the bucket owner.
putBucketLogging :: forall eff. PutBucketLoggingRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketLogging = AWS.request serviceName "PutBucketLogging" 


-- | Sets a metrics configuration (specified by the metrics configuration ID) for the bucket.
putBucketMetricsConfiguration :: forall eff. PutBucketMetricsConfigurationRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketMetricsConfiguration = AWS.request serviceName "PutBucketMetricsConfiguration" 


-- | Deprecated, see the PutBucketNotificationConfiguraiton operation.
putBucketNotification :: forall eff. PutBucketNotificationRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketNotification = AWS.request serviceName "PutBucketNotification" 


-- | Enables notifications of specified events for a bucket.
putBucketNotificationConfiguration :: forall eff. PutBucketNotificationConfigurationRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketNotificationConfiguration = AWS.request serviceName "PutBucketNotificationConfiguration" 


-- | Replaces a policy on a bucket. If the bucket already has a policy, the one in this request completely replaces it.
putBucketPolicy :: forall eff. PutBucketPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketPolicy = AWS.request serviceName "PutBucketPolicy" 


-- | Creates a new replication configuration (or replaces an existing one, if present).
putBucketReplication :: forall eff. PutBucketReplicationRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketReplication = AWS.request serviceName "PutBucketReplication" 


-- | Sets the request payment configuration for a bucket. By default, the bucket owner pays for downloads from the bucket. This configuration parameter enables the bucket owner (only) to specify that the person requesting the download will be charged for the download. Documentation on requester pays buckets can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html
putBucketRequestPayment :: forall eff. PutBucketRequestPaymentRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketRequestPayment = AWS.request serviceName "PutBucketRequestPayment" 


-- | Sets the tags for a bucket.
putBucketTagging :: forall eff. PutBucketTaggingRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketTagging = AWS.request serviceName "PutBucketTagging" 


-- | Sets the versioning state of an existing bucket. To set the versioning state, you must be the bucket owner.
putBucketVersioning :: forall eff. PutBucketVersioningRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketVersioning = AWS.request serviceName "PutBucketVersioning" 


-- | Set the website configuration for a bucket.
putBucketWebsite :: forall eff. PutBucketWebsiteRequest -> Aff (err :: AWS.RequestError | eff) Unit
putBucketWebsite = AWS.request serviceName "PutBucketWebsite" 


-- | Adds an object to a bucket.
putObject :: forall eff. PutObjectRequest -> Aff (err :: AWS.RequestError | eff) PutObjectOutput
putObject = AWS.request serviceName "PutObject" 


-- | uses the acl subresource to set the access control list (ACL) permissions for an object that already exists in a bucket
putObjectAcl :: forall eff. PutObjectAclRequest -> Aff (err :: AWS.RequestError | eff) PutObjectAclOutput
putObjectAcl = AWS.request serviceName "PutObjectAcl" 


-- | Sets the supplied tag-set to an object that already exists in a bucket
putObjectTagging :: forall eff. PutObjectTaggingRequest -> Aff (err :: AWS.RequestError | eff) PutObjectTaggingOutput
putObjectTagging = AWS.request serviceName "PutObjectTagging" 


-- | Restores an archived copy of an object back into Amazon S3
restoreObject :: forall eff. RestoreObjectRequest -> Aff (err :: AWS.RequestError | eff) RestoreObjectOutput
restoreObject = AWS.request serviceName "RestoreObject" 


-- | <p>Uploads a part in a multipart upload.</p><p><b>Note:</b> After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.</p>
uploadPart :: forall eff. UploadPartRequest -> Aff (err :: AWS.RequestError | eff) UploadPartOutput
uploadPart = AWS.request serviceName "UploadPart" 


-- | Uploads a part by copying data from an existing object as data source.
uploadPartCopy :: forall eff. UploadPartCopyRequest -> Aff (err :: AWS.RequestError | eff) UploadPartCopyOutput
uploadPartCopy = AWS.request serviceName "UploadPartCopy" 


newtype AbortDate = AbortDate Number
derive instance newtypeAbortDate :: Newtype AbortDate _


-- | Specifies the days since the initiation of an Incomplete Multipart Upload that Lifecycle will wait before permanently removing all parts of the upload.
newtype AbortIncompleteMultipartUpload = AbortIncompleteMultipartUpload 
  { "DaysAfterInitiation" :: NullOrUndefined (DaysAfterInitiation)
  }
derive instance newtypeAbortIncompleteMultipartUpload :: Newtype AbortIncompleteMultipartUpload _


newtype AbortMultipartUploadOutput = AbortMultipartUploadOutput 
  { "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeAbortMultipartUploadOutput :: Newtype AbortMultipartUploadOutput _


newtype AbortMultipartUploadRequest = AbortMultipartUploadRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "UploadId" :: (MultipartUploadId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeAbortMultipartUploadRequest :: Newtype AbortMultipartUploadRequest _


newtype AbortRuleId = AbortRuleId String
derive instance newtypeAbortRuleId :: Newtype AbortRuleId _


newtype AccelerateConfiguration = AccelerateConfiguration 
  { "Status" :: NullOrUndefined (BucketAccelerateStatus)
  }
derive instance newtypeAccelerateConfiguration :: Newtype AccelerateConfiguration _


newtype AcceptRanges = AcceptRanges String
derive instance newtypeAcceptRanges :: Newtype AcceptRanges _


newtype AccessControlPolicy = AccessControlPolicy 
  { "Grants" :: NullOrUndefined (Grants)
  , "Owner" :: NullOrUndefined (Owner)
  }
derive instance newtypeAccessControlPolicy :: Newtype AccessControlPolicy _


-- | Container for information regarding the access control for replicas.
newtype AccessControlTranslation = AccessControlTranslation 
  { "Owner" :: (OwnerOverride)
  }
derive instance newtypeAccessControlTranslation :: Newtype AccessControlTranslation _


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _


newtype AllowedHeader = AllowedHeader String
derive instance newtypeAllowedHeader :: Newtype AllowedHeader _


newtype AllowedHeaders = AllowedHeaders (Array AllowedHeader)
derive instance newtypeAllowedHeaders :: Newtype AllowedHeaders _


newtype AllowedMethod = AllowedMethod String
derive instance newtypeAllowedMethod :: Newtype AllowedMethod _


newtype AllowedMethods = AllowedMethods (Array AllowedMethod)
derive instance newtypeAllowedMethods :: Newtype AllowedMethods _


newtype AllowedOrigin = AllowedOrigin String
derive instance newtypeAllowedOrigin :: Newtype AllowedOrigin _


newtype AllowedOrigins = AllowedOrigins (Array AllowedOrigin)
derive instance newtypeAllowedOrigins :: Newtype AllowedOrigins _


newtype AnalyticsAndOperator = AnalyticsAndOperator 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tags" :: NullOrUndefined (TagSet)
  }
derive instance newtypeAnalyticsAndOperator :: Newtype AnalyticsAndOperator _


newtype AnalyticsConfiguration = AnalyticsConfiguration 
  { "Id" :: (AnalyticsId)
  , "Filter" :: NullOrUndefined (AnalyticsFilter)
  , "StorageClassAnalysis" :: (StorageClassAnalysis)
  }
derive instance newtypeAnalyticsConfiguration :: Newtype AnalyticsConfiguration _


newtype AnalyticsConfigurationList = AnalyticsConfigurationList (Array AnalyticsConfiguration)
derive instance newtypeAnalyticsConfigurationList :: Newtype AnalyticsConfigurationList _


newtype AnalyticsExportDestination = AnalyticsExportDestination 
  { "S3BucketDestination" :: (AnalyticsS3BucketDestination)
  }
derive instance newtypeAnalyticsExportDestination :: Newtype AnalyticsExportDestination _


newtype AnalyticsFilter = AnalyticsFilter 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tag" :: NullOrUndefined (Tag)
  , "And" :: NullOrUndefined (AnalyticsAndOperator)
  }
derive instance newtypeAnalyticsFilter :: Newtype AnalyticsFilter _


newtype AnalyticsId = AnalyticsId String
derive instance newtypeAnalyticsId :: Newtype AnalyticsId _


newtype AnalyticsS3BucketDestination = AnalyticsS3BucketDestination 
  { "Format" :: (AnalyticsS3ExportFileFormat)
  , "BucketAccountId" :: NullOrUndefined (AccountId)
  , "Bucket" :: (BucketName)
  , "Prefix" :: NullOrUndefined (Prefix)
  }
derive instance newtypeAnalyticsS3BucketDestination :: Newtype AnalyticsS3BucketDestination _


newtype AnalyticsS3ExportFileFormat = AnalyticsS3ExportFileFormat String
derive instance newtypeAnalyticsS3ExportFileFormat :: Newtype AnalyticsS3ExportFileFormat _


newtype Body = Body String
derive instance newtypeBody :: Newtype Body _


newtype Bucket = Bucket 
  { "Name" :: NullOrUndefined (BucketName)
  , "CreationDate" :: NullOrUndefined (CreationDate)
  }
derive instance newtypeBucket :: Newtype Bucket _


newtype BucketAccelerateStatus = BucketAccelerateStatus String
derive instance newtypeBucketAccelerateStatus :: Newtype BucketAccelerateStatus _


-- | The requested bucket name is not available. The bucket namespace is shared by all users of the system. Please select a different name and try again.
newtype BucketAlreadyExists = BucketAlreadyExists 
  { 
  }
derive instance newtypeBucketAlreadyExists :: Newtype BucketAlreadyExists _


newtype BucketAlreadyOwnedByYou = BucketAlreadyOwnedByYou 
  { 
  }
derive instance newtypeBucketAlreadyOwnedByYou :: Newtype BucketAlreadyOwnedByYou _


newtype BucketCannedACL = BucketCannedACL String
derive instance newtypeBucketCannedACL :: Newtype BucketCannedACL _


newtype BucketLifecycleConfiguration = BucketLifecycleConfiguration 
  { "Rules" :: (LifecycleRules)
  }
derive instance newtypeBucketLifecycleConfiguration :: Newtype BucketLifecycleConfiguration _


newtype BucketLocationConstraint = BucketLocationConstraint String
derive instance newtypeBucketLocationConstraint :: Newtype BucketLocationConstraint _


newtype BucketLoggingStatus = BucketLoggingStatus 
  { "LoggingEnabled" :: NullOrUndefined (LoggingEnabled)
  }
derive instance newtypeBucketLoggingStatus :: Newtype BucketLoggingStatus _


newtype BucketLogsPermission = BucketLogsPermission String
derive instance newtypeBucketLogsPermission :: Newtype BucketLogsPermission _


newtype BucketName = BucketName String
derive instance newtypeBucketName :: Newtype BucketName _


newtype BucketVersioningStatus = BucketVersioningStatus String
derive instance newtypeBucketVersioningStatus :: Newtype BucketVersioningStatus _


newtype Buckets = Buckets (Array Bucket)
derive instance newtypeBuckets :: Newtype Buckets _


newtype CORSConfiguration = CORSConfiguration 
  { "CORSRules" :: (CORSRules)
  }
derive instance newtypeCORSConfiguration :: Newtype CORSConfiguration _


newtype CORSRule = CORSRule 
  { "AllowedHeaders" :: NullOrUndefined (AllowedHeaders)
  , "AllowedMethods" :: (AllowedMethods)
  , "AllowedOrigins" :: (AllowedOrigins)
  , "ExposeHeaders" :: NullOrUndefined (ExposeHeaders)
  , "MaxAgeSeconds" :: NullOrUndefined (MaxAgeSeconds)
  }
derive instance newtypeCORSRule :: Newtype CORSRule _


newtype CORSRules = CORSRules (Array CORSRule)
derive instance newtypeCORSRules :: Newtype CORSRules _


-- | Describes how a CSV-formatted input object is formatted.
newtype CSVInput = CSVInput 
  { "FileHeaderInfo" :: NullOrUndefined (FileHeaderInfo)
  , "Comments" :: NullOrUndefined (Comments)
  , "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter)
  , "RecordDelimiter" :: NullOrUndefined (RecordDelimiter)
  , "FieldDelimiter" :: NullOrUndefined (FieldDelimiter)
  , "QuoteCharacter" :: NullOrUndefined (QuoteCharacter)
  }
derive instance newtypeCSVInput :: Newtype CSVInput _


-- | Describes how CSV-formatted results are formatted.
newtype CSVOutput = CSVOutput 
  { "QuoteFields" :: NullOrUndefined (QuoteFields)
  , "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter)
  , "RecordDelimiter" :: NullOrUndefined (RecordDelimiter)
  , "FieldDelimiter" :: NullOrUndefined (FieldDelimiter)
  , "QuoteCharacter" :: NullOrUndefined (QuoteCharacter)
  }
derive instance newtypeCSVOutput :: Newtype CSVOutput _


newtype CacheControl = CacheControl String
derive instance newtypeCacheControl :: Newtype CacheControl _


newtype CloudFunction = CloudFunction String
derive instance newtypeCloudFunction :: Newtype CloudFunction _


newtype CloudFunctionConfiguration = CloudFunctionConfiguration 
  { "Id" :: NullOrUndefined (NotificationId)
  , "Event" :: NullOrUndefined (Event)
  , "Events" :: NullOrUndefined (EventList)
  , "CloudFunction" :: NullOrUndefined (CloudFunction)
  , "InvocationRole" :: NullOrUndefined (CloudFunctionInvocationRole)
  }
derive instance newtypeCloudFunctionConfiguration :: Newtype CloudFunctionConfiguration _


newtype CloudFunctionInvocationRole = CloudFunctionInvocationRole String
derive instance newtypeCloudFunctionInvocationRole :: Newtype CloudFunctionInvocationRole _


newtype Code = Code String
derive instance newtypeCode :: Newtype Code _


newtype Comments = Comments String
derive instance newtypeComments :: Newtype Comments _


newtype CommonPrefix = CommonPrefix 
  { "Prefix" :: NullOrUndefined (Prefix)
  }
derive instance newtypeCommonPrefix :: Newtype CommonPrefix _


newtype CommonPrefixList = CommonPrefixList (Array CommonPrefix)
derive instance newtypeCommonPrefixList :: Newtype CommonPrefixList _


newtype CompleteMultipartUploadOutput = CompleteMultipartUploadOutput 
  { "Location" :: NullOrUndefined (Location)
  , "Bucket" :: NullOrUndefined (BucketName)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "Expiration" :: NullOrUndefined (Expiration)
  , "ETag" :: NullOrUndefined (ETag)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeCompleteMultipartUploadOutput :: Newtype CompleteMultipartUploadOutput _


newtype CompleteMultipartUploadRequest = CompleteMultipartUploadRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "MultipartUpload" :: NullOrUndefined (CompletedMultipartUpload)
  , "UploadId" :: (MultipartUploadId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeCompleteMultipartUploadRequest :: Newtype CompleteMultipartUploadRequest _


newtype CompletedMultipartUpload = CompletedMultipartUpload 
  { "Parts" :: NullOrUndefined (CompletedPartList)
  }
derive instance newtypeCompletedMultipartUpload :: Newtype CompletedMultipartUpload _


newtype CompletedPart = CompletedPart 
  { "ETag" :: NullOrUndefined (ETag)
  , "PartNumber" :: NullOrUndefined (PartNumber)
  }
derive instance newtypeCompletedPart :: Newtype CompletedPart _


newtype CompletedPartList = CompletedPartList (Array CompletedPart)
derive instance newtypeCompletedPartList :: Newtype CompletedPartList _


newtype Condition = Condition 
  { "HttpErrorCodeReturnedEquals" :: NullOrUndefined (HttpErrorCodeReturnedEquals)
  , "KeyPrefixEquals" :: NullOrUndefined (KeyPrefixEquals)
  }
derive instance newtypeCondition :: Newtype Condition _


newtype ConfirmRemoveSelfBucketAccess = ConfirmRemoveSelfBucketAccess Boolean
derive instance newtypeConfirmRemoveSelfBucketAccess :: Newtype ConfirmRemoveSelfBucketAccess _


newtype ContentDisposition = ContentDisposition String
derive instance newtypeContentDisposition :: Newtype ContentDisposition _


newtype ContentEncoding = ContentEncoding String
derive instance newtypeContentEncoding :: Newtype ContentEncoding _


newtype ContentLanguage = ContentLanguage String
derive instance newtypeContentLanguage :: Newtype ContentLanguage _


newtype ContentLength = ContentLength Number
derive instance newtypeContentLength :: Newtype ContentLength _


newtype ContentMD5 = ContentMD5 String
derive instance newtypeContentMD5 :: Newtype ContentMD5 _


newtype ContentRange = ContentRange String
derive instance newtypeContentRange :: Newtype ContentRange _


newtype ContentType = ContentType String
derive instance newtypeContentType :: Newtype ContentType _


newtype CopyObjectOutput = CopyObjectOutput 
  { "CopyObjectResult" :: NullOrUndefined (CopyObjectResult)
  , "Expiration" :: NullOrUndefined (Expiration)
  , "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeCopyObjectOutput :: Newtype CopyObjectOutput _


newtype CopyObjectRequest = CopyObjectRequest 
  { "ACL" :: NullOrUndefined (ObjectCannedACL)
  , "Bucket" :: (BucketName)
  , "CacheControl" :: NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined (ContentLanguage)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "CopySource" :: (CopySource)
  , "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch)
  , "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince)
  , "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch)
  , "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince)
  , "Expires" :: NullOrUndefined (Expires)
  , "GrantFullControl" :: NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined (GrantReadACP)
  , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP)
  , "Key" :: (ObjectKey)
  , "Metadata" :: NullOrUndefined (Metadata)
  , "MetadataDirective" :: NullOrUndefined (MetadataDirective)
  , "TaggingDirective" :: NullOrUndefined (TaggingDirective)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm)
  , "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey)
  , "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  , "Tagging" :: NullOrUndefined (TaggingHeader)
  }
derive instance newtypeCopyObjectRequest :: Newtype CopyObjectRequest _


newtype CopyObjectResult = CopyObjectResult 
  { "ETag" :: NullOrUndefined (ETag)
  , "LastModified" :: NullOrUndefined (LastModified)
  }
derive instance newtypeCopyObjectResult :: Newtype CopyObjectResult _


newtype CopyPartResult = CopyPartResult 
  { "ETag" :: NullOrUndefined (ETag)
  , "LastModified" :: NullOrUndefined (LastModified)
  }
derive instance newtypeCopyPartResult :: Newtype CopyPartResult _


newtype CopySource = CopySource String
derive instance newtypeCopySource :: Newtype CopySource _


newtype CopySourceIfMatch = CopySourceIfMatch String
derive instance newtypeCopySourceIfMatch :: Newtype CopySourceIfMatch _


newtype CopySourceIfModifiedSince = CopySourceIfModifiedSince Number
derive instance newtypeCopySourceIfModifiedSince :: Newtype CopySourceIfModifiedSince _


newtype CopySourceIfNoneMatch = CopySourceIfNoneMatch String
derive instance newtypeCopySourceIfNoneMatch :: Newtype CopySourceIfNoneMatch _


newtype CopySourceIfUnmodifiedSince = CopySourceIfUnmodifiedSince Number
derive instance newtypeCopySourceIfUnmodifiedSince :: Newtype CopySourceIfUnmodifiedSince _


newtype CopySourceRange = CopySourceRange String
derive instance newtypeCopySourceRange :: Newtype CopySourceRange _


newtype CopySourceSSECustomerAlgorithm = CopySourceSSECustomerAlgorithm String
derive instance newtypeCopySourceSSECustomerAlgorithm :: Newtype CopySourceSSECustomerAlgorithm _


newtype CopySourceSSECustomerKey = CopySourceSSECustomerKey String
derive instance newtypeCopySourceSSECustomerKey :: Newtype CopySourceSSECustomerKey _


newtype CopySourceSSECustomerKeyMD5 = CopySourceSSECustomerKeyMD5 String
derive instance newtypeCopySourceSSECustomerKeyMD5 :: Newtype CopySourceSSECustomerKeyMD5 _


newtype CopySourceVersionId = CopySourceVersionId String
derive instance newtypeCopySourceVersionId :: Newtype CopySourceVersionId _


newtype CreateBucketConfiguration = CreateBucketConfiguration 
  { "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint)
  }
derive instance newtypeCreateBucketConfiguration :: Newtype CreateBucketConfiguration _


newtype CreateBucketOutput = CreateBucketOutput 
  { "Location" :: NullOrUndefined (Location)
  }
derive instance newtypeCreateBucketOutput :: Newtype CreateBucketOutput _


newtype CreateBucketRequest = CreateBucketRequest 
  { "ACL" :: NullOrUndefined (BucketCannedACL)
  , "Bucket" :: (BucketName)
  , "CreateBucketConfiguration" :: NullOrUndefined (CreateBucketConfiguration)
  , "GrantFullControl" :: NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined (GrantReadACP)
  , "GrantWrite" :: NullOrUndefined (GrantWrite)
  , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP)
  }
derive instance newtypeCreateBucketRequest :: Newtype CreateBucketRequest _


newtype CreateMultipartUploadOutput = CreateMultipartUploadOutput 
  { "AbortDate" :: NullOrUndefined (AbortDate)
  , "AbortRuleId" :: NullOrUndefined (AbortRuleId)
  , "Bucket" :: NullOrUndefined (BucketName)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "UploadId" :: NullOrUndefined (MultipartUploadId)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeCreateMultipartUploadOutput :: Newtype CreateMultipartUploadOutput _


newtype CreateMultipartUploadRequest = CreateMultipartUploadRequest 
  { "ACL" :: NullOrUndefined (ObjectCannedACL)
  , "Bucket" :: (BucketName)
  , "CacheControl" :: NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined (ContentLanguage)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "Expires" :: NullOrUndefined (Expires)
  , "GrantFullControl" :: NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined (GrantReadACP)
  , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP)
  , "Key" :: (ObjectKey)
  , "Metadata" :: NullOrUndefined (Metadata)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  , "Tagging" :: NullOrUndefined (TaggingHeader)
  }
derive instance newtypeCreateMultipartUploadRequest :: Newtype CreateMultipartUploadRequest _


newtype CreationDate = CreationDate Number
derive instance newtypeCreationDate :: Newtype CreationDate _


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _


newtype Days = Days Int
derive instance newtypeDays :: Newtype Days _


newtype DaysAfterInitiation = DaysAfterInitiation Int
derive instance newtypeDaysAfterInitiation :: Newtype DaysAfterInitiation _


newtype Delete = Delete 
  { "Objects" :: (ObjectIdentifierList)
  , "Quiet" :: NullOrUndefined (Quiet)
  }
derive instance newtypeDelete :: Newtype Delete _


newtype DeleteBucketAnalyticsConfigurationRequest = DeleteBucketAnalyticsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (AnalyticsId)
  }
derive instance newtypeDeleteBucketAnalyticsConfigurationRequest :: Newtype DeleteBucketAnalyticsConfigurationRequest _


newtype DeleteBucketCorsRequest = DeleteBucketCorsRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketCorsRequest :: Newtype DeleteBucketCorsRequest _


newtype DeleteBucketEncryptionRequest = DeleteBucketEncryptionRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketEncryptionRequest :: Newtype DeleteBucketEncryptionRequest _


newtype DeleteBucketInventoryConfigurationRequest = DeleteBucketInventoryConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (InventoryId)
  }
derive instance newtypeDeleteBucketInventoryConfigurationRequest :: Newtype DeleteBucketInventoryConfigurationRequest _


newtype DeleteBucketLifecycleRequest = DeleteBucketLifecycleRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketLifecycleRequest :: Newtype DeleteBucketLifecycleRequest _


newtype DeleteBucketMetricsConfigurationRequest = DeleteBucketMetricsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (MetricsId)
  }
derive instance newtypeDeleteBucketMetricsConfigurationRequest :: Newtype DeleteBucketMetricsConfigurationRequest _


newtype DeleteBucketPolicyRequest = DeleteBucketPolicyRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketPolicyRequest :: Newtype DeleteBucketPolicyRequest _


newtype DeleteBucketReplicationRequest = DeleteBucketReplicationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketReplicationRequest :: Newtype DeleteBucketReplicationRequest _


newtype DeleteBucketRequest = DeleteBucketRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketRequest :: Newtype DeleteBucketRequest _


newtype DeleteBucketTaggingRequest = DeleteBucketTaggingRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketTaggingRequest :: Newtype DeleteBucketTaggingRequest _


newtype DeleteBucketWebsiteRequest = DeleteBucketWebsiteRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketWebsiteRequest :: Newtype DeleteBucketWebsiteRequest _


newtype DeleteMarker = DeleteMarker Boolean
derive instance newtypeDeleteMarker :: Newtype DeleteMarker _


newtype DeleteMarkerEntry = DeleteMarkerEntry 
  { "Owner" :: NullOrUndefined (Owner)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "IsLatest" :: NullOrUndefined (IsLatest)
  , "LastModified" :: NullOrUndefined (LastModified)
  }
derive instance newtypeDeleteMarkerEntry :: Newtype DeleteMarkerEntry _


newtype DeleteMarkerVersionId = DeleteMarkerVersionId String
derive instance newtypeDeleteMarkerVersionId :: Newtype DeleteMarkerVersionId _


newtype DeleteMarkers = DeleteMarkers (Array DeleteMarkerEntry)
derive instance newtypeDeleteMarkers :: Newtype DeleteMarkers _


newtype DeleteObjectOutput = DeleteObjectOutput 
  { "DeleteMarker" :: NullOrUndefined (DeleteMarker)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeDeleteObjectOutput :: Newtype DeleteObjectOutput _


newtype DeleteObjectRequest = DeleteObjectRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "MFA" :: NullOrUndefined (MFA)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeDeleteObjectRequest :: Newtype DeleteObjectRequest _


newtype DeleteObjectTaggingOutput = DeleteObjectTaggingOutput 
  { "VersionId" :: NullOrUndefined (ObjectVersionId)
  }
derive instance newtypeDeleteObjectTaggingOutput :: Newtype DeleteObjectTaggingOutput _


newtype DeleteObjectTaggingRequest = DeleteObjectTaggingRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  }
derive instance newtypeDeleteObjectTaggingRequest :: Newtype DeleteObjectTaggingRequest _


newtype DeleteObjectsOutput = DeleteObjectsOutput 
  { "Deleted" :: NullOrUndefined (DeletedObjects)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  , "Errors" :: NullOrUndefined (Errors)
  }
derive instance newtypeDeleteObjectsOutput :: Newtype DeleteObjectsOutput _


newtype DeleteObjectsRequest = DeleteObjectsRequest 
  { "Bucket" :: (BucketName)
  , "Delete" :: (Delete)
  , "MFA" :: NullOrUndefined (MFA)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeDeleteObjectsRequest :: Newtype DeleteObjectsRequest _


newtype DeletedObject = DeletedObject 
  { "Key" :: NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "DeleteMarker" :: NullOrUndefined (DeleteMarker)
  , "DeleteMarkerVersionId" :: NullOrUndefined (DeleteMarkerVersionId)
  }
derive instance newtypeDeletedObject :: Newtype DeletedObject _


newtype DeletedObjects = DeletedObjects (Array DeletedObject)
derive instance newtypeDeletedObjects :: Newtype DeletedObjects _


newtype Delimiter = Delimiter String
derive instance newtypeDelimiter :: Newtype Delimiter _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


-- | Container for replication destination information.
newtype Destination = Destination 
  { "Bucket" :: (BucketName)
  , "Account" :: NullOrUndefined (AccountId)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "AccessControlTranslation" :: NullOrUndefined (AccessControlTranslation)
  , "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration)
  }
derive instance newtypeDestination :: Newtype Destination _


newtype DisplayName = DisplayName String
derive instance newtypeDisplayName :: Newtype DisplayName _


newtype ETag = ETag String
derive instance newtypeETag :: Newtype ETag _


newtype EmailAddress = EmailAddress String
derive instance newtypeEmailAddress :: Newtype EmailAddress _


-- | Requests Amazon S3 to encode the object keys in the response and specifies the encoding method to use. An object key may contain any Unicode character; however, XML 1.0 parser cannot parse some characters, such as characters with an ASCII value from 0 to 10. For characters that are not supported in XML 1.0, you can add this parameter to request that Amazon S3 encode the keys in the response.
newtype EncodingType = EncodingType String
derive instance newtypeEncodingType :: Newtype EncodingType _


-- | Describes the server-side encryption that will be applied to the restore results.
newtype Encryption = Encryption 
  { "EncryptionType" :: (ServerSideEncryption)
  , "KMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "KMSContext" :: NullOrUndefined (KMSContext)
  }
derive instance newtypeEncryption :: Newtype Encryption _


-- | Container for information regarding encryption based configuration for replicas.
newtype EncryptionConfiguration = EncryptionConfiguration 
  { "ReplicaKmsKeyID" :: NullOrUndefined (ReplicaKmsKeyID)
  }
derive instance newtypeEncryptionConfiguration :: Newtype EncryptionConfiguration _


newtype Error = Error 
  { "Key" :: NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "Code" :: NullOrUndefined (Code)
  , "Message" :: NullOrUndefined (Message)
  }
derive instance newtypeError :: Newtype Error _


newtype ErrorDocument = ErrorDocument 
  { "Key" :: (ObjectKey)
  }
derive instance newtypeErrorDocument :: Newtype ErrorDocument _


newtype Errors = Errors (Array Error)
derive instance newtypeErrors :: Newtype Errors _


-- | Bucket event for which to send notifications.
newtype Event = Event String
derive instance newtypeEvent :: Newtype Event _


newtype EventList = EventList (Array Event)
derive instance newtypeEventList :: Newtype EventList _


newtype Expiration = Expiration String
derive instance newtypeExpiration :: Newtype Expiration _


newtype ExpirationStatus = ExpirationStatus String
derive instance newtypeExpirationStatus :: Newtype ExpirationStatus _


newtype ExpiredObjectDeleteMarker = ExpiredObjectDeleteMarker Boolean
derive instance newtypeExpiredObjectDeleteMarker :: Newtype ExpiredObjectDeleteMarker _


newtype Expires = Expires Number
derive instance newtypeExpires :: Newtype Expires _


newtype ExposeHeader = ExposeHeader String
derive instance newtypeExposeHeader :: Newtype ExposeHeader _


newtype ExposeHeaders = ExposeHeaders (Array ExposeHeader)
derive instance newtypeExposeHeaders :: Newtype ExposeHeaders _


newtype Expression = Expression String
derive instance newtypeExpression :: Newtype Expression _


newtype ExpressionType = ExpressionType String
derive instance newtypeExpressionType :: Newtype ExpressionType _


newtype FetchOwner = FetchOwner Boolean
derive instance newtypeFetchOwner :: Newtype FetchOwner _


newtype FieldDelimiter = FieldDelimiter String
derive instance newtypeFieldDelimiter :: Newtype FieldDelimiter _


newtype FileHeaderInfo = FileHeaderInfo String
derive instance newtypeFileHeaderInfo :: Newtype FileHeaderInfo _


-- | Container for key value pair that defines the criteria for the filter rule.
newtype FilterRule = FilterRule 
  { "Name" :: NullOrUndefined (FilterRuleName)
  , "Value" :: NullOrUndefined (FilterRuleValue)
  }
derive instance newtypeFilterRule :: Newtype FilterRule _


-- | A list of containers for key value pair that defines the criteria for the filter rule.
newtype FilterRuleList = FilterRuleList (Array FilterRule)
derive instance newtypeFilterRuleList :: Newtype FilterRuleList _


newtype FilterRuleName = FilterRuleName String
derive instance newtypeFilterRuleName :: Newtype FilterRuleName _


newtype FilterRuleValue = FilterRuleValue String
derive instance newtypeFilterRuleValue :: Newtype FilterRuleValue _


newtype GetBucketAccelerateConfigurationOutput = GetBucketAccelerateConfigurationOutput 
  { "Status" :: NullOrUndefined (BucketAccelerateStatus)
  }
derive instance newtypeGetBucketAccelerateConfigurationOutput :: Newtype GetBucketAccelerateConfigurationOutput _


newtype GetBucketAccelerateConfigurationRequest = GetBucketAccelerateConfigurationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketAccelerateConfigurationRequest :: Newtype GetBucketAccelerateConfigurationRequest _


newtype GetBucketAclOutput = GetBucketAclOutput 
  { "Owner" :: NullOrUndefined (Owner)
  , "Grants" :: NullOrUndefined (Grants)
  }
derive instance newtypeGetBucketAclOutput :: Newtype GetBucketAclOutput _


newtype GetBucketAclRequest = GetBucketAclRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketAclRequest :: Newtype GetBucketAclRequest _


newtype GetBucketAnalyticsConfigurationOutput = GetBucketAnalyticsConfigurationOutput 
  { "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfiguration)
  }
derive instance newtypeGetBucketAnalyticsConfigurationOutput :: Newtype GetBucketAnalyticsConfigurationOutput _


newtype GetBucketAnalyticsConfigurationRequest = GetBucketAnalyticsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (AnalyticsId)
  }
derive instance newtypeGetBucketAnalyticsConfigurationRequest :: Newtype GetBucketAnalyticsConfigurationRequest _


newtype GetBucketCorsOutput = GetBucketCorsOutput 
  { "CORSRules" :: NullOrUndefined (CORSRules)
  }
derive instance newtypeGetBucketCorsOutput :: Newtype GetBucketCorsOutput _


newtype GetBucketCorsRequest = GetBucketCorsRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketCorsRequest :: Newtype GetBucketCorsRequest _


newtype GetBucketEncryptionOutput = GetBucketEncryptionOutput 
  { "ServerSideEncryptionConfiguration" :: NullOrUndefined (ServerSideEncryptionConfiguration)
  }
derive instance newtypeGetBucketEncryptionOutput :: Newtype GetBucketEncryptionOutput _


newtype GetBucketEncryptionRequest = GetBucketEncryptionRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketEncryptionRequest :: Newtype GetBucketEncryptionRequest _


newtype GetBucketInventoryConfigurationOutput = GetBucketInventoryConfigurationOutput 
  { "InventoryConfiguration" :: NullOrUndefined (InventoryConfiguration)
  }
derive instance newtypeGetBucketInventoryConfigurationOutput :: Newtype GetBucketInventoryConfigurationOutput _


newtype GetBucketInventoryConfigurationRequest = GetBucketInventoryConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (InventoryId)
  }
derive instance newtypeGetBucketInventoryConfigurationRequest :: Newtype GetBucketInventoryConfigurationRequest _


newtype GetBucketLifecycleConfigurationOutput = GetBucketLifecycleConfigurationOutput 
  { "Rules" :: NullOrUndefined (LifecycleRules)
  }
derive instance newtypeGetBucketLifecycleConfigurationOutput :: Newtype GetBucketLifecycleConfigurationOutput _


newtype GetBucketLifecycleConfigurationRequest = GetBucketLifecycleConfigurationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketLifecycleConfigurationRequest :: Newtype GetBucketLifecycleConfigurationRequest _


newtype GetBucketLifecycleOutput = GetBucketLifecycleOutput 
  { "Rules" :: NullOrUndefined (Rules)
  }
derive instance newtypeGetBucketLifecycleOutput :: Newtype GetBucketLifecycleOutput _


newtype GetBucketLifecycleRequest = GetBucketLifecycleRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketLifecycleRequest :: Newtype GetBucketLifecycleRequest _


newtype GetBucketLocationOutput = GetBucketLocationOutput 
  { "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint)
  }
derive instance newtypeGetBucketLocationOutput :: Newtype GetBucketLocationOutput _


newtype GetBucketLocationRequest = GetBucketLocationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketLocationRequest :: Newtype GetBucketLocationRequest _


newtype GetBucketLoggingOutput = GetBucketLoggingOutput 
  { "LoggingEnabled" :: NullOrUndefined (LoggingEnabled)
  }
derive instance newtypeGetBucketLoggingOutput :: Newtype GetBucketLoggingOutput _


newtype GetBucketLoggingRequest = GetBucketLoggingRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketLoggingRequest :: Newtype GetBucketLoggingRequest _


newtype GetBucketMetricsConfigurationOutput = GetBucketMetricsConfigurationOutput 
  { "MetricsConfiguration" :: NullOrUndefined (MetricsConfiguration)
  }
derive instance newtypeGetBucketMetricsConfigurationOutput :: Newtype GetBucketMetricsConfigurationOutput _


newtype GetBucketMetricsConfigurationRequest = GetBucketMetricsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (MetricsId)
  }
derive instance newtypeGetBucketMetricsConfigurationRequest :: Newtype GetBucketMetricsConfigurationRequest _


newtype GetBucketNotificationConfigurationRequest = GetBucketNotificationConfigurationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketNotificationConfigurationRequest :: Newtype GetBucketNotificationConfigurationRequest _


newtype GetBucketPolicyOutput = GetBucketPolicyOutput 
  { "Policy" :: NullOrUndefined (Policy)
  }
derive instance newtypeGetBucketPolicyOutput :: Newtype GetBucketPolicyOutput _


newtype GetBucketPolicyRequest = GetBucketPolicyRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketPolicyRequest :: Newtype GetBucketPolicyRequest _


newtype GetBucketReplicationOutput = GetBucketReplicationOutput 
  { "ReplicationConfiguration" :: NullOrUndefined (ReplicationConfiguration)
  }
derive instance newtypeGetBucketReplicationOutput :: Newtype GetBucketReplicationOutput _


newtype GetBucketReplicationRequest = GetBucketReplicationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketReplicationRequest :: Newtype GetBucketReplicationRequest _


newtype GetBucketRequestPaymentOutput = GetBucketRequestPaymentOutput 
  { "Payer" :: NullOrUndefined (Payer)
  }
derive instance newtypeGetBucketRequestPaymentOutput :: Newtype GetBucketRequestPaymentOutput _


newtype GetBucketRequestPaymentRequest = GetBucketRequestPaymentRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketRequestPaymentRequest :: Newtype GetBucketRequestPaymentRequest _


newtype GetBucketTaggingOutput = GetBucketTaggingOutput 
  { "TagSet" :: (TagSet)
  }
derive instance newtypeGetBucketTaggingOutput :: Newtype GetBucketTaggingOutput _


newtype GetBucketTaggingRequest = GetBucketTaggingRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketTaggingRequest :: Newtype GetBucketTaggingRequest _


newtype GetBucketVersioningOutput = GetBucketVersioningOutput 
  { "Status" :: NullOrUndefined (BucketVersioningStatus)
  , "MFADelete" :: NullOrUndefined (MFADeleteStatus)
  }
derive instance newtypeGetBucketVersioningOutput :: Newtype GetBucketVersioningOutput _


newtype GetBucketVersioningRequest = GetBucketVersioningRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketVersioningRequest :: Newtype GetBucketVersioningRequest _


newtype GetBucketWebsiteOutput = GetBucketWebsiteOutput 
  { "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo)
  , "IndexDocument" :: NullOrUndefined (IndexDocument)
  , "ErrorDocument" :: NullOrUndefined (ErrorDocument)
  , "RoutingRules" :: NullOrUndefined (RoutingRules)
  }
derive instance newtypeGetBucketWebsiteOutput :: Newtype GetBucketWebsiteOutput _


newtype GetBucketWebsiteRequest = GetBucketWebsiteRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketWebsiteRequest :: Newtype GetBucketWebsiteRequest _


newtype GetObjectAclOutput = GetObjectAclOutput 
  { "Owner" :: NullOrUndefined (Owner)
  , "Grants" :: NullOrUndefined (Grants)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeGetObjectAclOutput :: Newtype GetObjectAclOutput _


newtype GetObjectAclRequest = GetObjectAclRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeGetObjectAclRequest :: Newtype GetObjectAclRequest _


newtype GetObjectOutput = GetObjectOutput 
  { "Body" :: NullOrUndefined (Body)
  , "DeleteMarker" :: NullOrUndefined (DeleteMarker)
  , "AcceptRanges" :: NullOrUndefined (AcceptRanges)
  , "Expiration" :: NullOrUndefined (Expiration)
  , "Restore" :: NullOrUndefined (Restore)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "ContentLength" :: NullOrUndefined (ContentLength)
  , "ETag" :: NullOrUndefined (ETag)
  , "MissingMeta" :: NullOrUndefined (MissingMeta)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "CacheControl" :: NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined (ContentLanguage)
  , "ContentRange" :: NullOrUndefined (ContentRange)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "Expires" :: NullOrUndefined (Expires)
  , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "Metadata" :: NullOrUndefined (Metadata)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  , "ReplicationStatus" :: NullOrUndefined (ReplicationStatus)
  , "PartsCount" :: NullOrUndefined (PartsCount)
  , "TagCount" :: NullOrUndefined (TagCount)
  }
derive instance newtypeGetObjectOutput :: Newtype GetObjectOutput _


newtype GetObjectRequest = GetObjectRequest 
  { "Bucket" :: (BucketName)
  , "IfMatch" :: NullOrUndefined (IfMatch)
  , "IfModifiedSince" :: NullOrUndefined (IfModifiedSince)
  , "IfNoneMatch" :: NullOrUndefined (IfNoneMatch)
  , "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince)
  , "Key" :: (ObjectKey)
  , "Range" :: NullOrUndefined (Range)
  , "ResponseCacheControl" :: NullOrUndefined (ResponseCacheControl)
  , "ResponseContentDisposition" :: NullOrUndefined (ResponseContentDisposition)
  , "ResponseContentEncoding" :: NullOrUndefined (ResponseContentEncoding)
  , "ResponseContentLanguage" :: NullOrUndefined (ResponseContentLanguage)
  , "ResponseContentType" :: NullOrUndefined (ResponseContentType)
  , "ResponseExpires" :: NullOrUndefined (ResponseExpires)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  , "PartNumber" :: NullOrUndefined (PartNumber)
  }
derive instance newtypeGetObjectRequest :: Newtype GetObjectRequest _


newtype GetObjectTaggingOutput = GetObjectTaggingOutput 
  { "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "TagSet" :: (TagSet)
  }
derive instance newtypeGetObjectTaggingOutput :: Newtype GetObjectTaggingOutput _


newtype GetObjectTaggingRequest = GetObjectTaggingRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  }
derive instance newtypeGetObjectTaggingRequest :: Newtype GetObjectTaggingRequest _


newtype GetObjectTorrentOutput = GetObjectTorrentOutput 
  { "Body" :: NullOrUndefined (Body)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeGetObjectTorrentOutput :: Newtype GetObjectTorrentOutput _


newtype GetObjectTorrentRequest = GetObjectTorrentRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeGetObjectTorrentRequest :: Newtype GetObjectTorrentRequest _


newtype GlacierJobParameters = GlacierJobParameters 
  { "Tier" :: (Tier)
  }
derive instance newtypeGlacierJobParameters :: Newtype GlacierJobParameters _


newtype Grant = Grant 
  { "Grantee" :: NullOrUndefined (Grantee)
  , "Permission" :: NullOrUndefined (Permission)
  }
derive instance newtypeGrant :: Newtype Grant _


newtype GrantFullControl = GrantFullControl String
derive instance newtypeGrantFullControl :: Newtype GrantFullControl _


newtype GrantRead = GrantRead String
derive instance newtypeGrantRead :: Newtype GrantRead _


newtype GrantReadACP = GrantReadACP String
derive instance newtypeGrantReadACP :: Newtype GrantReadACP _


newtype GrantWrite = GrantWrite String
derive instance newtypeGrantWrite :: Newtype GrantWrite _


newtype GrantWriteACP = GrantWriteACP String
derive instance newtypeGrantWriteACP :: Newtype GrantWriteACP _


newtype Grantee = Grantee 
  { "DisplayName" :: NullOrUndefined (DisplayName)
  , "EmailAddress" :: NullOrUndefined (EmailAddress)
  , "ID" :: NullOrUndefined (ID)
  , "Type" :: (Type)
  , "URI" :: NullOrUndefined (URI)
  }
derive instance newtypeGrantee :: Newtype Grantee _


newtype Grants = Grants (Array Grant)
derive instance newtypeGrants :: Newtype Grants _


newtype HeadBucketRequest = HeadBucketRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeHeadBucketRequest :: Newtype HeadBucketRequest _


newtype HeadObjectOutput = HeadObjectOutput 
  { "DeleteMarker" :: NullOrUndefined (DeleteMarker)
  , "AcceptRanges" :: NullOrUndefined (AcceptRanges)
  , "Expiration" :: NullOrUndefined (Expiration)
  , "Restore" :: NullOrUndefined (Restore)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "ContentLength" :: NullOrUndefined (ContentLength)
  , "ETag" :: NullOrUndefined (ETag)
  , "MissingMeta" :: NullOrUndefined (MissingMeta)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "CacheControl" :: NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined (ContentLanguage)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "Expires" :: NullOrUndefined (Expires)
  , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "Metadata" :: NullOrUndefined (Metadata)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  , "ReplicationStatus" :: NullOrUndefined (ReplicationStatus)
  , "PartsCount" :: NullOrUndefined (PartsCount)
  }
derive instance newtypeHeadObjectOutput :: Newtype HeadObjectOutput _


newtype HeadObjectRequest = HeadObjectRequest 
  { "Bucket" :: (BucketName)
  , "IfMatch" :: NullOrUndefined (IfMatch)
  , "IfModifiedSince" :: NullOrUndefined (IfModifiedSince)
  , "IfNoneMatch" :: NullOrUndefined (IfNoneMatch)
  , "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince)
  , "Key" :: (ObjectKey)
  , "Range" :: NullOrUndefined (Range)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  , "PartNumber" :: NullOrUndefined (PartNumber)
  }
derive instance newtypeHeadObjectRequest :: Newtype HeadObjectRequest _


newtype HostName = HostName String
derive instance newtypeHostName :: Newtype HostName _


newtype HttpErrorCodeReturnedEquals = HttpErrorCodeReturnedEquals String
derive instance newtypeHttpErrorCodeReturnedEquals :: Newtype HttpErrorCodeReturnedEquals _


newtype HttpRedirectCode = HttpRedirectCode String
derive instance newtypeHttpRedirectCode :: Newtype HttpRedirectCode _


newtype ID = ID String
derive instance newtypeID :: Newtype ID _


newtype IfMatch = IfMatch String
derive instance newtypeIfMatch :: Newtype IfMatch _


newtype IfModifiedSince = IfModifiedSince Number
derive instance newtypeIfModifiedSince :: Newtype IfModifiedSince _


newtype IfNoneMatch = IfNoneMatch String
derive instance newtypeIfNoneMatch :: Newtype IfNoneMatch _


newtype IfUnmodifiedSince = IfUnmodifiedSince Number
derive instance newtypeIfUnmodifiedSince :: Newtype IfUnmodifiedSince _


newtype IndexDocument = IndexDocument 
  { "Suffix" :: (Suffix)
  }
derive instance newtypeIndexDocument :: Newtype IndexDocument _


newtype Initiated = Initiated Number
derive instance newtypeInitiated :: Newtype Initiated _


newtype Initiator = Initiator 
  { "ID" :: NullOrUndefined (ID)
  , "DisplayName" :: NullOrUndefined (DisplayName)
  }
derive instance newtypeInitiator :: Newtype Initiator _


-- | Describes the serialization format of the object.
newtype InputSerialization = InputSerialization 
  { "CSV" :: NullOrUndefined (CSVInput)
  }
derive instance newtypeInputSerialization :: Newtype InputSerialization _


newtype InventoryConfiguration = InventoryConfiguration 
  { "Destination" :: (InventoryDestination)
  , "IsEnabled" :: (IsEnabled)
  , "Filter" :: NullOrUndefined (InventoryFilter)
  , "Id" :: (InventoryId)
  , "IncludedObjectVersions" :: (InventoryIncludedObjectVersions)
  , "OptionalFields" :: NullOrUndefined (InventoryOptionalFields)
  , "Schedule" :: (InventorySchedule)
  }
derive instance newtypeInventoryConfiguration :: Newtype InventoryConfiguration _


newtype InventoryConfigurationList = InventoryConfigurationList (Array InventoryConfiguration)
derive instance newtypeInventoryConfigurationList :: Newtype InventoryConfigurationList _


newtype InventoryDestination = InventoryDestination 
  { "S3BucketDestination" :: (InventoryS3BucketDestination)
  }
derive instance newtypeInventoryDestination :: Newtype InventoryDestination _


-- | Contains the type of server-side encryption used to encrypt the inventory results.
newtype InventoryEncryption = InventoryEncryption 
  { "SSES3" :: NullOrUndefined (SSES3)
  , "SSEKMS" :: NullOrUndefined (SSEKMS)
  }
derive instance newtypeInventoryEncryption :: Newtype InventoryEncryption _


newtype InventoryFilter = InventoryFilter 
  { "Prefix" :: (Prefix)
  }
derive instance newtypeInventoryFilter :: Newtype InventoryFilter _


newtype InventoryFormat = InventoryFormat String
derive instance newtypeInventoryFormat :: Newtype InventoryFormat _


newtype InventoryFrequency = InventoryFrequency String
derive instance newtypeInventoryFrequency :: Newtype InventoryFrequency _


newtype InventoryId = InventoryId String
derive instance newtypeInventoryId :: Newtype InventoryId _


newtype InventoryIncludedObjectVersions = InventoryIncludedObjectVersions String
derive instance newtypeInventoryIncludedObjectVersions :: Newtype InventoryIncludedObjectVersions _


newtype InventoryOptionalField = InventoryOptionalField String
derive instance newtypeInventoryOptionalField :: Newtype InventoryOptionalField _


newtype InventoryOptionalFields = InventoryOptionalFields (Array InventoryOptionalField)
derive instance newtypeInventoryOptionalFields :: Newtype InventoryOptionalFields _


newtype InventoryS3BucketDestination = InventoryS3BucketDestination 
  { "AccountId" :: NullOrUndefined (AccountId)
  , "Bucket" :: (BucketName)
  , "Format" :: (InventoryFormat)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Encryption" :: NullOrUndefined (InventoryEncryption)
  }
derive instance newtypeInventoryS3BucketDestination :: Newtype InventoryS3BucketDestination _


newtype InventorySchedule = InventorySchedule 
  { "Frequency" :: (InventoryFrequency)
  }
derive instance newtypeInventorySchedule :: Newtype InventorySchedule _


newtype IsEnabled = IsEnabled Boolean
derive instance newtypeIsEnabled :: Newtype IsEnabled _


newtype IsLatest = IsLatest Boolean
derive instance newtypeIsLatest :: Newtype IsLatest _


newtype IsTruncated = IsTruncated Boolean
derive instance newtypeIsTruncated :: Newtype IsTruncated _


newtype KMSContext = KMSContext String
derive instance newtypeKMSContext :: Newtype KMSContext _


newtype KeyCount = KeyCount Int
derive instance newtypeKeyCount :: Newtype KeyCount _


newtype KeyMarker = KeyMarker String
derive instance newtypeKeyMarker :: Newtype KeyMarker _


newtype KeyPrefixEquals = KeyPrefixEquals String
derive instance newtypeKeyPrefixEquals :: Newtype KeyPrefixEquals _


newtype LambdaFunctionArn = LambdaFunctionArn String
derive instance newtypeLambdaFunctionArn :: Newtype LambdaFunctionArn _


-- | Container for specifying the AWS Lambda notification configuration.
newtype LambdaFunctionConfiguration = LambdaFunctionConfiguration 
  { "Id" :: NullOrUndefined (NotificationId)
  , "LambdaFunctionArn" :: (LambdaFunctionArn)
  , "Events" :: (EventList)
  , "Filter" :: NullOrUndefined (NotificationConfigurationFilter)
  }
derive instance newtypeLambdaFunctionConfiguration :: Newtype LambdaFunctionConfiguration _


newtype LambdaFunctionConfigurationList = LambdaFunctionConfigurationList (Array LambdaFunctionConfiguration)
derive instance newtypeLambdaFunctionConfigurationList :: Newtype LambdaFunctionConfigurationList _


newtype LastModified = LastModified Number
derive instance newtypeLastModified :: Newtype LastModified _


newtype LifecycleConfiguration = LifecycleConfiguration 
  { "Rules" :: (Rules)
  }
derive instance newtypeLifecycleConfiguration :: Newtype LifecycleConfiguration _


newtype LifecycleExpiration = LifecycleExpiration 
  { "Date" :: NullOrUndefined (Date)
  , "Days" :: NullOrUndefined (Days)
  , "ExpiredObjectDeleteMarker" :: NullOrUndefined (ExpiredObjectDeleteMarker)
  }
derive instance newtypeLifecycleExpiration :: Newtype LifecycleExpiration _


newtype LifecycleRule = LifecycleRule 
  { "Expiration" :: NullOrUndefined (LifecycleExpiration)
  , "ID" :: NullOrUndefined (ID)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Filter" :: NullOrUndefined (LifecycleRuleFilter)
  , "Status" :: (ExpirationStatus)
  , "Transitions" :: NullOrUndefined (TransitionList)
  , "NoncurrentVersionTransitions" :: NullOrUndefined (NoncurrentVersionTransitionList)
  , "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration)
  , "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload)
  }
derive instance newtypeLifecycleRule :: Newtype LifecycleRule _


-- | This is used in a Lifecycle Rule Filter to apply a logical AND to two or more predicates. The Lifecycle Rule will apply to any object matching all of the predicates configured inside the And operator.
newtype LifecycleRuleAndOperator = LifecycleRuleAndOperator 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tags" :: NullOrUndefined (TagSet)
  }
derive instance newtypeLifecycleRuleAndOperator :: Newtype LifecycleRuleAndOperator _


-- | The Filter is used to identify objects that a Lifecycle Rule applies to. A Filter must have exactly one of Prefix, Tag, or And specified.
newtype LifecycleRuleFilter = LifecycleRuleFilter 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tag" :: NullOrUndefined (Tag)
  , "And" :: NullOrUndefined (LifecycleRuleAndOperator)
  }
derive instance newtypeLifecycleRuleFilter :: Newtype LifecycleRuleFilter _


newtype LifecycleRules = LifecycleRules (Array LifecycleRule)
derive instance newtypeLifecycleRules :: Newtype LifecycleRules _


newtype ListBucketAnalyticsConfigurationsOutput = ListBucketAnalyticsConfigurationsOutput 
  { "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "ContinuationToken" :: NullOrUndefined (Token)
  , "NextContinuationToken" :: NullOrUndefined (NextToken)
  , "AnalyticsConfigurationList" :: NullOrUndefined (AnalyticsConfigurationList)
  }
derive instance newtypeListBucketAnalyticsConfigurationsOutput :: Newtype ListBucketAnalyticsConfigurationsOutput _


newtype ListBucketAnalyticsConfigurationsRequest = ListBucketAnalyticsConfigurationsRequest 
  { "Bucket" :: (BucketName)
  , "ContinuationToken" :: NullOrUndefined (Token)
  }
derive instance newtypeListBucketAnalyticsConfigurationsRequest :: Newtype ListBucketAnalyticsConfigurationsRequest _


newtype ListBucketInventoryConfigurationsOutput = ListBucketInventoryConfigurationsOutput 
  { "ContinuationToken" :: NullOrUndefined (Token)
  , "InventoryConfigurationList" :: NullOrUndefined (InventoryConfigurationList)
  , "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "NextContinuationToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListBucketInventoryConfigurationsOutput :: Newtype ListBucketInventoryConfigurationsOutput _


newtype ListBucketInventoryConfigurationsRequest = ListBucketInventoryConfigurationsRequest 
  { "Bucket" :: (BucketName)
  , "ContinuationToken" :: NullOrUndefined (Token)
  }
derive instance newtypeListBucketInventoryConfigurationsRequest :: Newtype ListBucketInventoryConfigurationsRequest _


newtype ListBucketMetricsConfigurationsOutput = ListBucketMetricsConfigurationsOutput 
  { "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "ContinuationToken" :: NullOrUndefined (Token)
  , "NextContinuationToken" :: NullOrUndefined (NextToken)
  , "MetricsConfigurationList" :: NullOrUndefined (MetricsConfigurationList)
  }
derive instance newtypeListBucketMetricsConfigurationsOutput :: Newtype ListBucketMetricsConfigurationsOutput _


newtype ListBucketMetricsConfigurationsRequest = ListBucketMetricsConfigurationsRequest 
  { "Bucket" :: (BucketName)
  , "ContinuationToken" :: NullOrUndefined (Token)
  }
derive instance newtypeListBucketMetricsConfigurationsRequest :: Newtype ListBucketMetricsConfigurationsRequest _


newtype ListBucketsOutput = ListBucketsOutput 
  { "Buckets" :: NullOrUndefined (Buckets)
  , "Owner" :: NullOrUndefined (Owner)
  }
derive instance newtypeListBucketsOutput :: Newtype ListBucketsOutput _


newtype ListMultipartUploadsOutput = ListMultipartUploadsOutput 
  { "Bucket" :: NullOrUndefined (BucketName)
  , "KeyMarker" :: NullOrUndefined (KeyMarker)
  , "UploadIdMarker" :: NullOrUndefined (UploadIdMarker)
  , "NextKeyMarker" :: NullOrUndefined (NextKeyMarker)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "NextUploadIdMarker" :: NullOrUndefined (NextUploadIdMarker)
  , "MaxUploads" :: NullOrUndefined (MaxUploads)
  , "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "Uploads" :: NullOrUndefined (MultipartUploadList)
  , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  }
derive instance newtypeListMultipartUploadsOutput :: Newtype ListMultipartUploadsOutput _


newtype ListMultipartUploadsRequest = ListMultipartUploadsRequest 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "KeyMarker" :: NullOrUndefined (KeyMarker)
  , "MaxUploads" :: NullOrUndefined (MaxUploads)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "UploadIdMarker" :: NullOrUndefined (UploadIdMarker)
  }
derive instance newtypeListMultipartUploadsRequest :: Newtype ListMultipartUploadsRequest _


newtype ListObjectVersionsOutput = ListObjectVersionsOutput 
  { "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "KeyMarker" :: NullOrUndefined (KeyMarker)
  , "VersionIdMarker" :: NullOrUndefined (VersionIdMarker)
  , "NextKeyMarker" :: NullOrUndefined (NextKeyMarker)
  , "NextVersionIdMarker" :: NullOrUndefined (NextVersionIdMarker)
  , "Versions" :: NullOrUndefined (ObjectVersionList)
  , "DeleteMarkers" :: NullOrUndefined (DeleteMarkers)
  , "Name" :: NullOrUndefined (BucketName)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  }
derive instance newtypeListObjectVersionsOutput :: Newtype ListObjectVersionsOutput _


newtype ListObjectVersionsRequest = ListObjectVersionsRequest 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "KeyMarker" :: NullOrUndefined (KeyMarker)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "VersionIdMarker" :: NullOrUndefined (VersionIdMarker)
  }
derive instance newtypeListObjectVersionsRequest :: Newtype ListObjectVersionsRequest _


newtype ListObjectsOutput = ListObjectsOutput 
  { "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "Marker" :: NullOrUndefined (Marker)
  , "NextMarker" :: NullOrUndefined (NextMarker)
  , "Contents" :: NullOrUndefined (ObjectList)
  , "Name" :: NullOrUndefined (BucketName)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  }
derive instance newtypeListObjectsOutput :: Newtype ListObjectsOutput _


newtype ListObjectsRequest = ListObjectsRequest 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "Marker" :: NullOrUndefined (Marker)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeListObjectsRequest :: Newtype ListObjectsRequest _


newtype ListObjectsV2Output = ListObjectsV2Output 
  { "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "Contents" :: NullOrUndefined (ObjectList)
  , "Name" :: NullOrUndefined (BucketName)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "KeyCount" :: NullOrUndefined (KeyCount)
  , "ContinuationToken" :: NullOrUndefined (Token)
  , "NextContinuationToken" :: NullOrUndefined (NextToken)
  , "StartAfter" :: NullOrUndefined (StartAfter)
  }
derive instance newtypeListObjectsV2Output :: Newtype ListObjectsV2Output _


newtype ListObjectsV2Request = ListObjectsV2Request 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "ContinuationToken" :: NullOrUndefined (Token)
  , "FetchOwner" :: NullOrUndefined (FetchOwner)
  , "StartAfter" :: NullOrUndefined (StartAfter)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeListObjectsV2Request :: Newtype ListObjectsV2Request _


newtype ListPartsOutput = ListPartsOutput 
  { "AbortDate" :: NullOrUndefined (AbortDate)
  , "AbortRuleId" :: NullOrUndefined (AbortRuleId)
  , "Bucket" :: NullOrUndefined (BucketName)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "UploadId" :: NullOrUndefined (MultipartUploadId)
  , "PartNumberMarker" :: NullOrUndefined (PartNumberMarker)
  , "NextPartNumberMarker" :: NullOrUndefined (NextPartNumberMarker)
  , "MaxParts" :: NullOrUndefined (MaxParts)
  , "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "Parts" :: NullOrUndefined (Parts)
  , "Initiator" :: NullOrUndefined (Initiator)
  , "Owner" :: NullOrUndefined (Owner)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeListPartsOutput :: Newtype ListPartsOutput _


newtype ListPartsRequest = ListPartsRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "MaxParts" :: NullOrUndefined (MaxParts)
  , "PartNumberMarker" :: NullOrUndefined (PartNumberMarker)
  , "UploadId" :: (MultipartUploadId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeListPartsRequest :: Newtype ListPartsRequest _


newtype Location = Location String
derive instance newtypeLocation :: Newtype Location _


newtype LocationPrefix = LocationPrefix String
derive instance newtypeLocationPrefix :: Newtype LocationPrefix _


newtype LoggingEnabled = LoggingEnabled 
  { "TargetBucket" :: NullOrUndefined (TargetBucket)
  , "TargetGrants" :: NullOrUndefined (TargetGrants)
  , "TargetPrefix" :: NullOrUndefined (TargetPrefix)
  }
derive instance newtypeLoggingEnabled :: Newtype LoggingEnabled _


newtype MFA = MFA String
derive instance newtypeMFA :: Newtype MFA _


newtype MFADelete = MFADelete String
derive instance newtypeMFADelete :: Newtype MFADelete _


newtype MFADeleteStatus = MFADeleteStatus String
derive instance newtypeMFADeleteStatus :: Newtype MFADeleteStatus _


newtype Marker = Marker String
derive instance newtypeMarker :: Newtype Marker _


newtype MaxAgeSeconds = MaxAgeSeconds Int
derive instance newtypeMaxAgeSeconds :: Newtype MaxAgeSeconds _


newtype MaxKeys = MaxKeys Int
derive instance newtypeMaxKeys :: Newtype MaxKeys _


newtype MaxParts = MaxParts Int
derive instance newtypeMaxParts :: Newtype MaxParts _


newtype MaxUploads = MaxUploads Int
derive instance newtypeMaxUploads :: Newtype MaxUploads _


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


newtype Metadata = Metadata (Map MetadataKey MetadataValue)
derive instance newtypeMetadata :: Newtype Metadata _


newtype MetadataDirective = MetadataDirective String
derive instance newtypeMetadataDirective :: Newtype MetadataDirective _


-- | A metadata key-value pair to store with an object.
newtype MetadataEntry = MetadataEntry 
  { "Name" :: NullOrUndefined (MetadataKey)
  , "Value" :: NullOrUndefined (MetadataValue)
  }
derive instance newtypeMetadataEntry :: Newtype MetadataEntry _


newtype MetadataKey = MetadataKey String
derive instance newtypeMetadataKey :: Newtype MetadataKey _


newtype MetadataValue = MetadataValue String
derive instance newtypeMetadataValue :: Newtype MetadataValue _


newtype MetricsAndOperator = MetricsAndOperator 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tags" :: NullOrUndefined (TagSet)
  }
derive instance newtypeMetricsAndOperator :: Newtype MetricsAndOperator _


newtype MetricsConfiguration = MetricsConfiguration 
  { "Id" :: (MetricsId)
  , "Filter" :: NullOrUndefined (MetricsFilter)
  }
derive instance newtypeMetricsConfiguration :: Newtype MetricsConfiguration _


newtype MetricsConfigurationList = MetricsConfigurationList (Array MetricsConfiguration)
derive instance newtypeMetricsConfigurationList :: Newtype MetricsConfigurationList _


newtype MetricsFilter = MetricsFilter 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tag" :: NullOrUndefined (Tag)
  , "And" :: NullOrUndefined (MetricsAndOperator)
  }
derive instance newtypeMetricsFilter :: Newtype MetricsFilter _


newtype MetricsId = MetricsId String
derive instance newtypeMetricsId :: Newtype MetricsId _


newtype MissingMeta = MissingMeta Int
derive instance newtypeMissingMeta :: Newtype MissingMeta _


newtype MultipartUpload = MultipartUpload 
  { "UploadId" :: NullOrUndefined (MultipartUploadId)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "Initiated" :: NullOrUndefined (Initiated)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "Owner" :: NullOrUndefined (Owner)
  , "Initiator" :: NullOrUndefined (Initiator)
  }
derive instance newtypeMultipartUpload :: Newtype MultipartUpload _


newtype MultipartUploadId = MultipartUploadId String
derive instance newtypeMultipartUploadId :: Newtype MultipartUploadId _


newtype MultipartUploadList = MultipartUploadList (Array MultipartUpload)
derive instance newtypeMultipartUploadList :: Newtype MultipartUploadList _


newtype NextKeyMarker = NextKeyMarker String
derive instance newtypeNextKeyMarker :: Newtype NextKeyMarker _


newtype NextMarker = NextMarker String
derive instance newtypeNextMarker :: Newtype NextMarker _


newtype NextPartNumberMarker = NextPartNumberMarker Int
derive instance newtypeNextPartNumberMarker :: Newtype NextPartNumberMarker _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


newtype NextUploadIdMarker = NextUploadIdMarker String
derive instance newtypeNextUploadIdMarker :: Newtype NextUploadIdMarker _


newtype NextVersionIdMarker = NextVersionIdMarker String
derive instance newtypeNextVersionIdMarker :: Newtype NextVersionIdMarker _


-- | The specified bucket does not exist.
newtype NoSuchBucket = NoSuchBucket 
  { 
  }
derive instance newtypeNoSuchBucket :: Newtype NoSuchBucket _


-- | The specified key does not exist.
newtype NoSuchKey = NoSuchKey 
  { 
  }
derive instance newtypeNoSuchKey :: Newtype NoSuchKey _


-- | The specified multipart upload does not exist.
newtype NoSuchUpload = NoSuchUpload 
  { 
  }
derive instance newtypeNoSuchUpload :: Newtype NoSuchUpload _


-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon S3 permanently deletes the noncurrent object versions. You set this lifecycle configuration action on a bucket that has versioning enabled (or suspended) to request that Amazon S3 delete noncurrent object versions at a specific period in the object's lifetime.
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration 
  { "NoncurrentDays" :: NullOrUndefined (Days)
  }
derive instance newtypeNoncurrentVersionExpiration :: Newtype NoncurrentVersionExpiration _


-- | Container for the transition rule that describes when noncurrent objects transition to the STANDARD_IA or GLACIER storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to the STANDARD_IA or GLACIER storage class at a specific period in the object's lifetime.
newtype NoncurrentVersionTransition = NoncurrentVersionTransition 
  { "NoncurrentDays" :: NullOrUndefined (Days)
  , "StorageClass" :: NullOrUndefined (TransitionStorageClass)
  }
derive instance newtypeNoncurrentVersionTransition :: Newtype NoncurrentVersionTransition _


newtype NoncurrentVersionTransitionList = NoncurrentVersionTransitionList (Array NoncurrentVersionTransition)
derive instance newtypeNoncurrentVersionTransitionList :: Newtype NoncurrentVersionTransitionList _


-- | Container for specifying the notification configuration of the bucket. If this element is empty, notifications are turned off on the bucket.
newtype NotificationConfiguration = NotificationConfiguration 
  { "TopicConfigurations" :: NullOrUndefined (TopicConfigurationList)
  , "QueueConfigurations" :: NullOrUndefined (QueueConfigurationList)
  , "LambdaFunctionConfigurations" :: NullOrUndefined (LambdaFunctionConfigurationList)
  }
derive instance newtypeNotificationConfiguration :: Newtype NotificationConfiguration _


newtype NotificationConfigurationDeprecated = NotificationConfigurationDeprecated 
  { "TopicConfiguration" :: NullOrUndefined (TopicConfigurationDeprecated)
  , "QueueConfiguration" :: NullOrUndefined (QueueConfigurationDeprecated)
  , "CloudFunctionConfiguration" :: NullOrUndefined (CloudFunctionConfiguration)
  }
derive instance newtypeNotificationConfigurationDeprecated :: Newtype NotificationConfigurationDeprecated _


-- | Container for object key name filtering rules. For information about key name filtering, go to <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html">Configuring Event Notifications</a> in the Amazon Simple Storage Service Developer Guide.
newtype NotificationConfigurationFilter = NotificationConfigurationFilter 
  { "Key" :: NullOrUndefined (S3KeyFilter)
  }
derive instance newtypeNotificationConfigurationFilter :: Newtype NotificationConfigurationFilter _


-- | Optional unique identifier for configurations in a notification configuration. If you don't provide one, Amazon S3 will assign an ID.
newtype NotificationId = NotificationId String
derive instance newtypeNotificationId :: Newtype NotificationId _


newtype Object = Object 
  { "Key" :: NullOrUndefined (ObjectKey)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "ETag" :: NullOrUndefined (ETag)
  , "Size" :: NullOrUndefined (Size)
  , "StorageClass" :: NullOrUndefined (ObjectStorageClass)
  , "Owner" :: NullOrUndefined (Owner)
  }
derive instance newtypeObject :: Newtype Object _


-- | This operation is not allowed against this storage tier
newtype ObjectAlreadyInActiveTierError = ObjectAlreadyInActiveTierError 
  { 
  }
derive instance newtypeObjectAlreadyInActiveTierError :: Newtype ObjectAlreadyInActiveTierError _


newtype ObjectCannedACL = ObjectCannedACL String
derive instance newtypeObjectCannedACL :: Newtype ObjectCannedACL _


newtype ObjectIdentifier = ObjectIdentifier 
  { "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  }
derive instance newtypeObjectIdentifier :: Newtype ObjectIdentifier _


newtype ObjectIdentifierList = ObjectIdentifierList (Array ObjectIdentifier)
derive instance newtypeObjectIdentifierList :: Newtype ObjectIdentifierList _


newtype ObjectKey = ObjectKey String
derive instance newtypeObjectKey :: Newtype ObjectKey _


newtype ObjectList = ObjectList (Array Object)
derive instance newtypeObjectList :: Newtype ObjectList _


-- | The source object of the COPY operation is not in the active tier and is only stored in Amazon Glacier.
newtype ObjectNotInActiveTierError = ObjectNotInActiveTierError 
  { 
  }
derive instance newtypeObjectNotInActiveTierError :: Newtype ObjectNotInActiveTierError _


newtype ObjectStorageClass = ObjectStorageClass String
derive instance newtypeObjectStorageClass :: Newtype ObjectStorageClass _


newtype ObjectVersion = ObjectVersion 
  { "ETag" :: NullOrUndefined (ETag)
  , "Size" :: NullOrUndefined (Size)
  , "StorageClass" :: NullOrUndefined (ObjectVersionStorageClass)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "IsLatest" :: NullOrUndefined (IsLatest)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "Owner" :: NullOrUndefined (Owner)
  }
derive instance newtypeObjectVersion :: Newtype ObjectVersion _


newtype ObjectVersionId = ObjectVersionId String
derive instance newtypeObjectVersionId :: Newtype ObjectVersionId _


newtype ObjectVersionList = ObjectVersionList (Array ObjectVersion)
derive instance newtypeObjectVersionList :: Newtype ObjectVersionList _


newtype ObjectVersionStorageClass = ObjectVersionStorageClass String
derive instance newtypeObjectVersionStorageClass :: Newtype ObjectVersionStorageClass _


-- | Describes the location where the restore job's output is stored.
newtype OutputLocation = OutputLocation 
  { "S3" :: NullOrUndefined (S3Location)
  }
derive instance newtypeOutputLocation :: Newtype OutputLocation _


-- | Describes how results of the Select job are serialized.
newtype OutputSerialization = OutputSerialization 
  { "CSV" :: NullOrUndefined (CSVOutput)
  }
derive instance newtypeOutputSerialization :: Newtype OutputSerialization _


newtype Owner = Owner 
  { "DisplayName" :: NullOrUndefined (DisplayName)
  , "ID" :: NullOrUndefined (ID)
  }
derive instance newtypeOwner :: Newtype Owner _


newtype OwnerOverride = OwnerOverride String
derive instance newtypeOwnerOverride :: Newtype OwnerOverride _


newtype Part = Part 
  { "PartNumber" :: NullOrUndefined (PartNumber)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "ETag" :: NullOrUndefined (ETag)
  , "Size" :: NullOrUndefined (Size)
  }
derive instance newtypePart :: Newtype Part _


newtype PartNumber = PartNumber Int
derive instance newtypePartNumber :: Newtype PartNumber _


newtype PartNumberMarker = PartNumberMarker Int
derive instance newtypePartNumberMarker :: Newtype PartNumberMarker _


newtype Parts = Parts (Array Part)
derive instance newtypeParts :: Newtype Parts _


newtype PartsCount = PartsCount Int
derive instance newtypePartsCount :: Newtype PartsCount _


newtype Payer = Payer String
derive instance newtypePayer :: Newtype Payer _


newtype Permission = Permission String
derive instance newtypePermission :: Newtype Permission _


newtype Policy = Policy String
derive instance newtypePolicy :: Newtype Policy _


newtype Prefix = Prefix String
derive instance newtypePrefix :: Newtype Prefix _


newtype Protocol = Protocol String
derive instance newtypeProtocol :: Newtype Protocol _


newtype PutBucketAccelerateConfigurationRequest = PutBucketAccelerateConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "AccelerateConfiguration" :: (AccelerateConfiguration)
  }
derive instance newtypePutBucketAccelerateConfigurationRequest :: Newtype PutBucketAccelerateConfigurationRequest _


newtype PutBucketAclRequest = PutBucketAclRequest 
  { "ACL" :: NullOrUndefined (BucketCannedACL)
  , "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy)
  , "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "GrantFullControl" :: NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined (GrantReadACP)
  , "GrantWrite" :: NullOrUndefined (GrantWrite)
  , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP)
  }
derive instance newtypePutBucketAclRequest :: Newtype PutBucketAclRequest _


newtype PutBucketAnalyticsConfigurationRequest = PutBucketAnalyticsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (AnalyticsId)
  , "AnalyticsConfiguration" :: (AnalyticsConfiguration)
  }
derive instance newtypePutBucketAnalyticsConfigurationRequest :: Newtype PutBucketAnalyticsConfigurationRequest _


newtype PutBucketCorsRequest = PutBucketCorsRequest 
  { "Bucket" :: (BucketName)
  , "CORSConfiguration" :: (CORSConfiguration)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  }
derive instance newtypePutBucketCorsRequest :: Newtype PutBucketCorsRequest _


newtype PutBucketEncryptionRequest = PutBucketEncryptionRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "ServerSideEncryptionConfiguration" :: (ServerSideEncryptionConfiguration)
  }
derive instance newtypePutBucketEncryptionRequest :: Newtype PutBucketEncryptionRequest _


newtype PutBucketInventoryConfigurationRequest = PutBucketInventoryConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (InventoryId)
  , "InventoryConfiguration" :: (InventoryConfiguration)
  }
derive instance newtypePutBucketInventoryConfigurationRequest :: Newtype PutBucketInventoryConfigurationRequest _


newtype PutBucketLifecycleConfigurationRequest = PutBucketLifecycleConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "LifecycleConfiguration" :: NullOrUndefined (BucketLifecycleConfiguration)
  }
derive instance newtypePutBucketLifecycleConfigurationRequest :: Newtype PutBucketLifecycleConfigurationRequest _


newtype PutBucketLifecycleRequest = PutBucketLifecycleRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "LifecycleConfiguration" :: NullOrUndefined (LifecycleConfiguration)
  }
derive instance newtypePutBucketLifecycleRequest :: Newtype PutBucketLifecycleRequest _


newtype PutBucketLoggingRequest = PutBucketLoggingRequest 
  { "Bucket" :: (BucketName)
  , "BucketLoggingStatus" :: (BucketLoggingStatus)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  }
derive instance newtypePutBucketLoggingRequest :: Newtype PutBucketLoggingRequest _


newtype PutBucketMetricsConfigurationRequest = PutBucketMetricsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (MetricsId)
  , "MetricsConfiguration" :: (MetricsConfiguration)
  }
derive instance newtypePutBucketMetricsConfigurationRequest :: Newtype PutBucketMetricsConfigurationRequest _


newtype PutBucketNotificationConfigurationRequest = PutBucketNotificationConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "NotificationConfiguration" :: (NotificationConfiguration)
  }
derive instance newtypePutBucketNotificationConfigurationRequest :: Newtype PutBucketNotificationConfigurationRequest _


newtype PutBucketNotificationRequest = PutBucketNotificationRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "NotificationConfiguration" :: (NotificationConfigurationDeprecated)
  }
derive instance newtypePutBucketNotificationRequest :: Newtype PutBucketNotificationRequest _


newtype PutBucketPolicyRequest = PutBucketPolicyRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "ConfirmRemoveSelfBucketAccess" :: NullOrUndefined (ConfirmRemoveSelfBucketAccess)
  , "Policy" :: (Policy)
  }
derive instance newtypePutBucketPolicyRequest :: Newtype PutBucketPolicyRequest _


newtype PutBucketReplicationRequest = PutBucketReplicationRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "ReplicationConfiguration" :: (ReplicationConfiguration)
  }
derive instance newtypePutBucketReplicationRequest :: Newtype PutBucketReplicationRequest _


newtype PutBucketRequestPaymentRequest = PutBucketRequestPaymentRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "RequestPaymentConfiguration" :: (RequestPaymentConfiguration)
  }
derive instance newtypePutBucketRequestPaymentRequest :: Newtype PutBucketRequestPaymentRequest _


newtype PutBucketTaggingRequest = PutBucketTaggingRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "Tagging" :: (Tagging)
  }
derive instance newtypePutBucketTaggingRequest :: Newtype PutBucketTaggingRequest _


newtype PutBucketVersioningRequest = PutBucketVersioningRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "MFA" :: NullOrUndefined (MFA)
  , "VersioningConfiguration" :: (VersioningConfiguration)
  }
derive instance newtypePutBucketVersioningRequest :: Newtype PutBucketVersioningRequest _


newtype PutBucketWebsiteRequest = PutBucketWebsiteRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "WebsiteConfiguration" :: (WebsiteConfiguration)
  }
derive instance newtypePutBucketWebsiteRequest :: Newtype PutBucketWebsiteRequest _


newtype PutObjectAclOutput = PutObjectAclOutput 
  { "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypePutObjectAclOutput :: Newtype PutObjectAclOutput _


newtype PutObjectAclRequest = PutObjectAclRequest 
  { "ACL" :: NullOrUndefined (ObjectCannedACL)
  , "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy)
  , "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "GrantFullControl" :: NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined (GrantReadACP)
  , "GrantWrite" :: NullOrUndefined (GrantWrite)
  , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP)
  , "Key" :: (ObjectKey)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  }
derive instance newtypePutObjectAclRequest :: Newtype PutObjectAclRequest _


newtype PutObjectOutput = PutObjectOutput 
  { "Expiration" :: NullOrUndefined (Expiration)
  , "ETag" :: NullOrUndefined (ETag)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypePutObjectOutput :: Newtype PutObjectOutput _


newtype PutObjectRequest = PutObjectRequest 
  { "ACL" :: NullOrUndefined (ObjectCannedACL)
  , "Body" :: NullOrUndefined (Body)
  , "Bucket" :: (BucketName)
  , "CacheControl" :: NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined (ContentLanguage)
  , "ContentLength" :: NullOrUndefined (ContentLength)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "Expires" :: NullOrUndefined (Expires)
  , "GrantFullControl" :: NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined (GrantReadACP)
  , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP)
  , "Key" :: (ObjectKey)
  , "Metadata" :: NullOrUndefined (Metadata)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  , "Tagging" :: NullOrUndefined (TaggingHeader)
  }
derive instance newtypePutObjectRequest :: Newtype PutObjectRequest _


newtype PutObjectTaggingOutput = PutObjectTaggingOutput 
  { "VersionId" :: NullOrUndefined (ObjectVersionId)
  }
derive instance newtypePutObjectTaggingOutput :: Newtype PutObjectTaggingOutput _


newtype PutObjectTaggingRequest = PutObjectTaggingRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "Tagging" :: (Tagging)
  }
derive instance newtypePutObjectTaggingRequest :: Newtype PutObjectTaggingRequest _


newtype QueueArn = QueueArn String
derive instance newtypeQueueArn :: Newtype QueueArn _


-- | Container for specifying an configuration when you want Amazon S3 to publish events to an Amazon Simple Queue Service (Amazon SQS) queue.
newtype QueueConfiguration = QueueConfiguration 
  { "Id" :: NullOrUndefined (NotificationId)
  , "QueueArn" :: (QueueArn)
  , "Events" :: (EventList)
  , "Filter" :: NullOrUndefined (NotificationConfigurationFilter)
  }
derive instance newtypeQueueConfiguration :: Newtype QueueConfiguration _


newtype QueueConfigurationDeprecated = QueueConfigurationDeprecated 
  { "Id" :: NullOrUndefined (NotificationId)
  , "Event" :: NullOrUndefined (Event)
  , "Events" :: NullOrUndefined (EventList)
  , "Queue" :: NullOrUndefined (QueueArn)
  }
derive instance newtypeQueueConfigurationDeprecated :: Newtype QueueConfigurationDeprecated _


newtype QueueConfigurationList = QueueConfigurationList (Array QueueConfiguration)
derive instance newtypeQueueConfigurationList :: Newtype QueueConfigurationList _


newtype Quiet = Quiet Boolean
derive instance newtypeQuiet :: Newtype Quiet _


newtype QuoteCharacter = QuoteCharacter String
derive instance newtypeQuoteCharacter :: Newtype QuoteCharacter _


newtype QuoteEscapeCharacter = QuoteEscapeCharacter String
derive instance newtypeQuoteEscapeCharacter :: Newtype QuoteEscapeCharacter _


newtype QuoteFields = QuoteFields String
derive instance newtypeQuoteFields :: Newtype QuoteFields _


newtype Range = Range String
derive instance newtypeRange :: Newtype Range _


newtype RecordDelimiter = RecordDelimiter String
derive instance newtypeRecordDelimiter :: Newtype RecordDelimiter _


newtype Redirect = Redirect 
  { "HostName" :: NullOrUndefined (HostName)
  , "HttpRedirectCode" :: NullOrUndefined (HttpRedirectCode)
  , "Protocol" :: NullOrUndefined (Protocol)
  , "ReplaceKeyPrefixWith" :: NullOrUndefined (ReplaceKeyPrefixWith)
  , "ReplaceKeyWith" :: NullOrUndefined (ReplaceKeyWith)
  }
derive instance newtypeRedirect :: Newtype Redirect _


newtype RedirectAllRequestsTo = RedirectAllRequestsTo 
  { "HostName" :: (HostName)
  , "Protocol" :: NullOrUndefined (Protocol)
  }
derive instance newtypeRedirectAllRequestsTo :: Newtype RedirectAllRequestsTo _


newtype ReplaceKeyPrefixWith = ReplaceKeyPrefixWith String
derive instance newtypeReplaceKeyPrefixWith :: Newtype ReplaceKeyPrefixWith _


newtype ReplaceKeyWith = ReplaceKeyWith String
derive instance newtypeReplaceKeyWith :: Newtype ReplaceKeyWith _


newtype ReplicaKmsKeyID = ReplicaKmsKeyID String
derive instance newtypeReplicaKmsKeyID :: Newtype ReplicaKmsKeyID _


-- | Container for replication rules. You can add as many as 1,000 rules. Total replication configuration size can be up to 2 MB.
newtype ReplicationConfiguration = ReplicationConfiguration 
  { "Role" :: (Role)
  , "Rules" :: (ReplicationRules)
  }
derive instance newtypeReplicationConfiguration :: Newtype ReplicationConfiguration _


-- | Container for information about a particular replication rule.
newtype ReplicationRule = ReplicationRule 
  { "ID" :: NullOrUndefined (ID)
  , "Prefix" :: (Prefix)
  , "Status" :: (ReplicationRuleStatus)
  , "SourceSelectionCriteria" :: NullOrUndefined (SourceSelectionCriteria)
  , "Destination" :: (Destination)
  }
derive instance newtypeReplicationRule :: Newtype ReplicationRule _


newtype ReplicationRuleStatus = ReplicationRuleStatus String
derive instance newtypeReplicationRuleStatus :: Newtype ReplicationRuleStatus _


newtype ReplicationRules = ReplicationRules (Array ReplicationRule)
derive instance newtypeReplicationRules :: Newtype ReplicationRules _


newtype ReplicationStatus = ReplicationStatus String
derive instance newtypeReplicationStatus :: Newtype ReplicationStatus _


-- | If present, indicates that the requester was successfully charged for the request.
newtype RequestCharged = RequestCharged String
derive instance newtypeRequestCharged :: Newtype RequestCharged _


-- | Confirms that the requester knows that she or he will be charged for the request. Bucket owners need not specify this parameter in their requests. Documentation on downloading objects from requester pays buckets can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/ObjectsinRequesterPaysBuckets.html
newtype RequestPayer = RequestPayer String
derive instance newtypeRequestPayer :: Newtype RequestPayer _


newtype RequestPaymentConfiguration = RequestPaymentConfiguration 
  { "Payer" :: (Payer)
  }
derive instance newtypeRequestPaymentConfiguration :: Newtype RequestPaymentConfiguration _


newtype ResponseCacheControl = ResponseCacheControl String
derive instance newtypeResponseCacheControl :: Newtype ResponseCacheControl _


newtype ResponseContentDisposition = ResponseContentDisposition String
derive instance newtypeResponseContentDisposition :: Newtype ResponseContentDisposition _


newtype ResponseContentEncoding = ResponseContentEncoding String
derive instance newtypeResponseContentEncoding :: Newtype ResponseContentEncoding _


newtype ResponseContentLanguage = ResponseContentLanguage String
derive instance newtypeResponseContentLanguage :: Newtype ResponseContentLanguage _


newtype ResponseContentType = ResponseContentType String
derive instance newtypeResponseContentType :: Newtype ResponseContentType _


newtype ResponseExpires = ResponseExpires Number
derive instance newtypeResponseExpires :: Newtype ResponseExpires _


newtype Restore = Restore String
derive instance newtypeRestore :: Newtype Restore _


newtype RestoreObjectOutput = RestoreObjectOutput 
  { "RequestCharged" :: NullOrUndefined (RequestCharged)
  , "RestoreOutputPath" :: NullOrUndefined (RestoreOutputPath)
  }
derive instance newtypeRestoreObjectOutput :: Newtype RestoreObjectOutput _


newtype RestoreObjectRequest = RestoreObjectRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "RestoreRequest" :: NullOrUndefined (RestoreRequest)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeRestoreObjectRequest :: Newtype RestoreObjectRequest _


newtype RestoreOutputPath = RestoreOutputPath String
derive instance newtypeRestoreOutputPath :: Newtype RestoreOutputPath _


-- | Container for restore job parameters.
newtype RestoreRequest = RestoreRequest 
  { "Days" :: NullOrUndefined (Days)
  , "GlacierJobParameters" :: NullOrUndefined (GlacierJobParameters)
  , "Type" :: NullOrUndefined (RestoreRequestType)
  , "Tier" :: NullOrUndefined (Tier)
  , "Description" :: NullOrUndefined (Description)
  , "SelectParameters" :: NullOrUndefined (SelectParameters)
  , "OutputLocation" :: NullOrUndefined (OutputLocation)
  }
derive instance newtypeRestoreRequest :: Newtype RestoreRequest _


newtype RestoreRequestType = RestoreRequestType String
derive instance newtypeRestoreRequestType :: Newtype RestoreRequestType _


newtype Role = Role String
derive instance newtypeRole :: Newtype Role _


newtype RoutingRule = RoutingRule 
  { "Condition" :: NullOrUndefined (Condition)
  , "Redirect" :: (Redirect)
  }
derive instance newtypeRoutingRule :: Newtype RoutingRule _


newtype RoutingRules = RoutingRules (Array RoutingRule)
derive instance newtypeRoutingRules :: Newtype RoutingRules _


newtype Rule = Rule 
  { "Expiration" :: NullOrUndefined (LifecycleExpiration)
  , "ID" :: NullOrUndefined (ID)
  , "Prefix" :: (Prefix)
  , "Status" :: (ExpirationStatus)
  , "Transition" :: NullOrUndefined (Transition)
  , "NoncurrentVersionTransition" :: NullOrUndefined (NoncurrentVersionTransition)
  , "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration)
  , "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload)
  }
derive instance newtypeRule :: Newtype Rule _


newtype Rules = Rules (Array Rule)
derive instance newtypeRules :: Newtype Rules _


-- | Container for object key name prefix and suffix filtering rules.
newtype S3KeyFilter = S3KeyFilter 
  { "FilterRules" :: NullOrUndefined (FilterRuleList)
  }
derive instance newtypeS3KeyFilter :: Newtype S3KeyFilter _


-- | Describes an S3 location that will receive the results of the restore request.
newtype S3Location = S3Location 
  { "BucketName" :: (BucketName)
  , "Prefix" :: (LocationPrefix)
  , "Encryption" :: NullOrUndefined (Encryption)
  , "CannedACL" :: NullOrUndefined (ObjectCannedACL)
  , "AccessControlList" :: NullOrUndefined (Grants)
  , "Tagging" :: NullOrUndefined (Tagging)
  , "UserMetadata" :: NullOrUndefined (UserMetadata)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  }
derive instance newtypeS3Location :: Newtype S3Location _


newtype SSECustomerAlgorithm = SSECustomerAlgorithm String
derive instance newtypeSSECustomerAlgorithm :: Newtype SSECustomerAlgorithm _


newtype SSECustomerKey = SSECustomerKey String
derive instance newtypeSSECustomerKey :: Newtype SSECustomerKey _


newtype SSECustomerKeyMD5 = SSECustomerKeyMD5 String
derive instance newtypeSSECustomerKeyMD5 :: Newtype SSECustomerKeyMD5 _


-- | Specifies the use of SSE-KMS to encrypt delievered Inventory reports.
newtype SSEKMS = SSEKMS 
  { "KeyId" :: (SSEKMSKeyId)
  }
derive instance newtypeSSEKMS :: Newtype SSEKMS _


newtype SSEKMSKeyId = SSEKMSKeyId String
derive instance newtypeSSEKMSKeyId :: Newtype SSEKMSKeyId _


-- | Specifies the use of SSE-S3 to encrypt delievered Inventory reports.
newtype SSES3 = SSES3 
  { 
  }
derive instance newtypeSSES3 :: Newtype SSES3 _


-- | Describes the parameters for Select job types.
newtype SelectParameters = SelectParameters 
  { "InputSerialization" :: (InputSerialization)
  , "ExpressionType" :: (ExpressionType)
  , "Expression" :: (Expression)
  , "OutputSerialization" :: (OutputSerialization)
  }
derive instance newtypeSelectParameters :: Newtype SelectParameters _


newtype ServerSideEncryption = ServerSideEncryption String
derive instance newtypeServerSideEncryption :: Newtype ServerSideEncryption _


-- | Describes the default server-side encryption to apply to new objects in the bucket. If Put Object request does not specify any server-side encryption, this default encryption will be applied.
newtype ServerSideEncryptionByDefault = ServerSideEncryptionByDefault 
  { "SSEAlgorithm" :: (ServerSideEncryption)
  , "KMSMasterKeyID" :: NullOrUndefined (SSEKMSKeyId)
  }
derive instance newtypeServerSideEncryptionByDefault :: Newtype ServerSideEncryptionByDefault _


-- | Container for server-side encryption configuration rules. Currently S3 supports one rule only.
newtype ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration 
  { "Rules" :: (ServerSideEncryptionRules)
  }
derive instance newtypeServerSideEncryptionConfiguration :: Newtype ServerSideEncryptionConfiguration _


-- | Container for information about a particular server-side encryption configuration rule.
newtype ServerSideEncryptionRule = ServerSideEncryptionRule 
  { "ApplyServerSideEncryptionByDefault" :: NullOrUndefined (ServerSideEncryptionByDefault)
  }
derive instance newtypeServerSideEncryptionRule :: Newtype ServerSideEncryptionRule _


newtype ServerSideEncryptionRules = ServerSideEncryptionRules (Array ServerSideEncryptionRule)
derive instance newtypeServerSideEncryptionRules :: Newtype ServerSideEncryptionRules _


newtype Size = Size Int
derive instance newtypeSize :: Newtype Size _


-- | Container for filters that define which source objects should be replicated.
newtype SourceSelectionCriteria = SourceSelectionCriteria 
  { "SseKmsEncryptedObjects" :: NullOrUndefined (SseKmsEncryptedObjects)
  }
derive instance newtypeSourceSelectionCriteria :: Newtype SourceSelectionCriteria _


-- | Container for filter information of selection of KMS Encrypted S3 objects.
newtype SseKmsEncryptedObjects = SseKmsEncryptedObjects 
  { "Status" :: (SseKmsEncryptedObjectsStatus)
  }
derive instance newtypeSseKmsEncryptedObjects :: Newtype SseKmsEncryptedObjects _


newtype SseKmsEncryptedObjectsStatus = SseKmsEncryptedObjectsStatus String
derive instance newtypeSseKmsEncryptedObjectsStatus :: Newtype SseKmsEncryptedObjectsStatus _


newtype StartAfter = StartAfter String
derive instance newtypeStartAfter :: Newtype StartAfter _


newtype StorageClass = StorageClass String
derive instance newtypeStorageClass :: Newtype StorageClass _


newtype StorageClassAnalysis = StorageClassAnalysis 
  { "DataExport" :: NullOrUndefined (StorageClassAnalysisDataExport)
  }
derive instance newtypeStorageClassAnalysis :: Newtype StorageClassAnalysis _


newtype StorageClassAnalysisDataExport = StorageClassAnalysisDataExport 
  { "OutputSchemaVersion" :: (StorageClassAnalysisSchemaVersion)
  , "Destination" :: (AnalyticsExportDestination)
  }
derive instance newtypeStorageClassAnalysisDataExport :: Newtype StorageClassAnalysisDataExport _


newtype StorageClassAnalysisSchemaVersion = StorageClassAnalysisSchemaVersion String
derive instance newtypeStorageClassAnalysisSchemaVersion :: Newtype StorageClassAnalysisSchemaVersion _


newtype Suffix = Suffix String
derive instance newtypeSuffix :: Newtype Suffix _


newtype Tag = Tag 
  { "Key" :: (ObjectKey)
  , "Value" :: (Value)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagCount = TagCount Int
derive instance newtypeTagCount :: Newtype TagCount _


newtype TagSet = TagSet (Array Tag)
derive instance newtypeTagSet :: Newtype TagSet _


newtype Tagging = Tagging 
  { "TagSet" :: (TagSet)
  }
derive instance newtypeTagging :: Newtype Tagging _


newtype TaggingDirective = TaggingDirective String
derive instance newtypeTaggingDirective :: Newtype TaggingDirective _


newtype TaggingHeader = TaggingHeader String
derive instance newtypeTaggingHeader :: Newtype TaggingHeader _


newtype TargetBucket = TargetBucket String
derive instance newtypeTargetBucket :: Newtype TargetBucket _


newtype TargetGrant = TargetGrant 
  { "Grantee" :: NullOrUndefined (Grantee)
  , "Permission" :: NullOrUndefined (BucketLogsPermission)
  }
derive instance newtypeTargetGrant :: Newtype TargetGrant _


newtype TargetGrants = TargetGrants (Array TargetGrant)
derive instance newtypeTargetGrants :: Newtype TargetGrants _


newtype TargetPrefix = TargetPrefix String
derive instance newtypeTargetPrefix :: Newtype TargetPrefix _


newtype Tier = Tier String
derive instance newtypeTier :: Newtype Tier _


newtype Token = Token String
derive instance newtypeToken :: Newtype Token _


newtype TopicArn = TopicArn String
derive instance newtypeTopicArn :: Newtype TopicArn _


-- | Container for specifying the configuration when you want Amazon S3 to publish events to an Amazon Simple Notification Service (Amazon SNS) topic.
newtype TopicConfiguration = TopicConfiguration 
  { "Id" :: NullOrUndefined (NotificationId)
  , "TopicArn" :: (TopicArn)
  , "Events" :: (EventList)
  , "Filter" :: NullOrUndefined (NotificationConfigurationFilter)
  }
derive instance newtypeTopicConfiguration :: Newtype TopicConfiguration _


newtype TopicConfigurationDeprecated = TopicConfigurationDeprecated 
  { "Id" :: NullOrUndefined (NotificationId)
  , "Events" :: NullOrUndefined (EventList)
  , "Event" :: NullOrUndefined (Event)
  , "Topic" :: NullOrUndefined (TopicArn)
  }
derive instance newtypeTopicConfigurationDeprecated :: Newtype TopicConfigurationDeprecated _


newtype TopicConfigurationList = TopicConfigurationList (Array TopicConfiguration)
derive instance newtypeTopicConfigurationList :: Newtype TopicConfigurationList _


newtype Transition = Transition 
  { "Date" :: NullOrUndefined (Date)
  , "Days" :: NullOrUndefined (Days)
  , "StorageClass" :: NullOrUndefined (TransitionStorageClass)
  }
derive instance newtypeTransition :: Newtype Transition _


newtype TransitionList = TransitionList (Array Transition)
derive instance newtypeTransitionList :: Newtype TransitionList _


newtype TransitionStorageClass = TransitionStorageClass String
derive instance newtypeTransitionStorageClass :: Newtype TransitionStorageClass _


newtype Type = Type String
derive instance newtypeType :: Newtype Type _


newtype URI = URI String
derive instance newtypeURI :: Newtype URI _


newtype UploadIdMarker = UploadIdMarker String
derive instance newtypeUploadIdMarker :: Newtype UploadIdMarker _


newtype UploadPartCopyOutput = UploadPartCopyOutput 
  { "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId)
  , "CopyPartResult" :: NullOrUndefined (CopyPartResult)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeUploadPartCopyOutput :: Newtype UploadPartCopyOutput _


newtype UploadPartCopyRequest = UploadPartCopyRequest 
  { "Bucket" :: (BucketName)
  , "CopySource" :: (CopySource)
  , "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch)
  , "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince)
  , "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch)
  , "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince)
  , "CopySourceRange" :: NullOrUndefined (CopySourceRange)
  , "Key" :: (ObjectKey)
  , "PartNumber" :: (PartNumber)
  , "UploadId" :: (MultipartUploadId)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm)
  , "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey)
  , "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeUploadPartCopyRequest :: Newtype UploadPartCopyRequest _


newtype UploadPartOutput = UploadPartOutput 
  { "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "ETag" :: NullOrUndefined (ETag)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeUploadPartOutput :: Newtype UploadPartOutput _


newtype UploadPartRequest = UploadPartRequest 
  { "Body" :: NullOrUndefined (Body)
  , "Bucket" :: (BucketName)
  , "ContentLength" :: NullOrUndefined (ContentLength)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "Key" :: (ObjectKey)
  , "PartNumber" :: (PartNumber)
  , "UploadId" :: (MultipartUploadId)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeUploadPartRequest :: Newtype UploadPartRequest _


newtype UserMetadata = UserMetadata (Array MetadataEntry)
derive instance newtypeUserMetadata :: Newtype UserMetadata _


newtype Value = Value String
derive instance newtypeValue :: Newtype Value _


newtype VersionIdMarker = VersionIdMarker String
derive instance newtypeVersionIdMarker :: Newtype VersionIdMarker _


newtype VersioningConfiguration = VersioningConfiguration 
  { "MFADelete" :: NullOrUndefined (MFADelete)
  , "Status" :: NullOrUndefined (BucketVersioningStatus)
  }
derive instance newtypeVersioningConfiguration :: Newtype VersioningConfiguration _


newtype WebsiteConfiguration = WebsiteConfiguration 
  { "ErrorDocument" :: NullOrUndefined (ErrorDocument)
  , "IndexDocument" :: NullOrUndefined (IndexDocument)
  , "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo)
  , "RoutingRules" :: NullOrUndefined (RoutingRules)
  }
derive instance newtypeWebsiteConfiguration :: Newtype WebsiteConfiguration _


newtype WebsiteRedirectLocation = WebsiteRedirectLocation String
derive instance newtypeWebsiteRedirectLocation :: Newtype WebsiteRedirectLocation _
