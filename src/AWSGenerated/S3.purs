

module AWS.S3 where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
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


-- | Specifies the days since the initiation of an Incomplete Multipart Upload that Lifecycle will wait before permanently removing all parts of the upload.
newtype AbortIncompleteMultipartUpload = AbortIncompleteMultipartUpload 
  { "DaysAfterInitiation" :: NullOrUndefined (DaysAfterInitiation)
  }


newtype AbortMultipartUploadOutput = AbortMultipartUploadOutput 
  { "RequestCharged" :: NullOrUndefined (RequestCharged)
  }


newtype AbortMultipartUploadRequest = AbortMultipartUploadRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "UploadId" :: (MultipartUploadId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }


newtype AbortRuleId = AbortRuleId String


newtype AccelerateConfiguration = AccelerateConfiguration 
  { "Status" :: NullOrUndefined (BucketAccelerateStatus)
  }


newtype AcceptRanges = AcceptRanges String


newtype AccessControlPolicy = AccessControlPolicy 
  { "Grants" :: NullOrUndefined (Grants)
  , "Owner" :: NullOrUndefined (Owner)
  }


-- | Container for information regarding the access control for replicas.
newtype AccessControlTranslation = AccessControlTranslation 
  { "Owner" :: (OwnerOverride)
  }


newtype AccountId = AccountId String


newtype AllowedHeader = AllowedHeader String


newtype AllowedHeaders = AllowedHeaders (Array AllowedHeader)


newtype AllowedMethod = AllowedMethod String


newtype AllowedMethods = AllowedMethods (Array AllowedMethod)


newtype AllowedOrigin = AllowedOrigin String


newtype AllowedOrigins = AllowedOrigins (Array AllowedOrigin)


newtype AnalyticsAndOperator = AnalyticsAndOperator 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tags" :: NullOrUndefined (TagSet)
  }


newtype AnalyticsConfiguration = AnalyticsConfiguration 
  { "Id" :: (AnalyticsId)
  , "Filter" :: NullOrUndefined (AnalyticsFilter)
  , "StorageClassAnalysis" :: (StorageClassAnalysis)
  }


newtype AnalyticsConfigurationList = AnalyticsConfigurationList (Array AnalyticsConfiguration)


newtype AnalyticsExportDestination = AnalyticsExportDestination 
  { "S3BucketDestination" :: (AnalyticsS3BucketDestination)
  }


newtype AnalyticsFilter = AnalyticsFilter 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tag" :: NullOrUndefined (Tag)
  , "And" :: NullOrUndefined (AnalyticsAndOperator)
  }


newtype AnalyticsId = AnalyticsId String


newtype AnalyticsS3BucketDestination = AnalyticsS3BucketDestination 
  { "Format" :: (AnalyticsS3ExportFileFormat)
  , "BucketAccountId" :: NullOrUndefined (AccountId)
  , "Bucket" :: (BucketName)
  , "Prefix" :: NullOrUndefined (Prefix)
  }


newtype AnalyticsS3ExportFileFormat = AnalyticsS3ExportFileFormat String


newtype Body = Body String


newtype Bucket = Bucket 
  { "Name" :: NullOrUndefined (BucketName)
  , "CreationDate" :: NullOrUndefined (CreationDate)
  }


newtype BucketAccelerateStatus = BucketAccelerateStatus String


-- | The requested bucket name is not available. The bucket namespace is shared by all users of the system. Please select a different name and try again.
newtype BucketAlreadyExists = BucketAlreadyExists 
  { 
  }


newtype BucketAlreadyOwnedByYou = BucketAlreadyOwnedByYou 
  { 
  }


newtype BucketCannedACL = BucketCannedACL String


newtype BucketLifecycleConfiguration = BucketLifecycleConfiguration 
  { "Rules" :: (LifecycleRules)
  }


newtype BucketLocationConstraint = BucketLocationConstraint String


newtype BucketLoggingStatus = BucketLoggingStatus 
  { "LoggingEnabled" :: NullOrUndefined (LoggingEnabled)
  }


newtype BucketLogsPermission = BucketLogsPermission String


newtype BucketName = BucketName String


newtype BucketVersioningStatus = BucketVersioningStatus String


newtype Buckets = Buckets (Array Bucket)


newtype CORSConfiguration = CORSConfiguration 
  { "CORSRules" :: (CORSRules)
  }


newtype CORSRule = CORSRule 
  { "AllowedHeaders" :: NullOrUndefined (AllowedHeaders)
  , "AllowedMethods" :: (AllowedMethods)
  , "AllowedOrigins" :: (AllowedOrigins)
  , "ExposeHeaders" :: NullOrUndefined (ExposeHeaders)
  , "MaxAgeSeconds" :: NullOrUndefined (MaxAgeSeconds)
  }


newtype CORSRules = CORSRules (Array CORSRule)


-- | Describes how a CSV-formatted input object is formatted.
newtype CSVInput = CSVInput 
  { "FileHeaderInfo" :: NullOrUndefined (FileHeaderInfo)
  , "Comments" :: NullOrUndefined (Comments)
  , "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter)
  , "RecordDelimiter" :: NullOrUndefined (RecordDelimiter)
  , "FieldDelimiter" :: NullOrUndefined (FieldDelimiter)
  , "QuoteCharacter" :: NullOrUndefined (QuoteCharacter)
  }


-- | Describes how CSV-formatted results are formatted.
newtype CSVOutput = CSVOutput 
  { "QuoteFields" :: NullOrUndefined (QuoteFields)
  , "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter)
  , "RecordDelimiter" :: NullOrUndefined (RecordDelimiter)
  , "FieldDelimiter" :: NullOrUndefined (FieldDelimiter)
  , "QuoteCharacter" :: NullOrUndefined (QuoteCharacter)
  }


newtype CacheControl = CacheControl String


newtype CloudFunction = CloudFunction String


newtype CloudFunctionConfiguration = CloudFunctionConfiguration 
  { "Id" :: NullOrUndefined (NotificationId)
  , "Event" :: NullOrUndefined (Event)
  , "Events" :: NullOrUndefined (EventList)
  , "CloudFunction" :: NullOrUndefined (CloudFunction)
  , "InvocationRole" :: NullOrUndefined (CloudFunctionInvocationRole)
  }


newtype CloudFunctionInvocationRole = CloudFunctionInvocationRole String


newtype Code = Code String


newtype Comments = Comments String


newtype CommonPrefix = CommonPrefix 
  { "Prefix" :: NullOrUndefined (Prefix)
  }


newtype CommonPrefixList = CommonPrefixList (Array CommonPrefix)


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


newtype CompleteMultipartUploadRequest = CompleteMultipartUploadRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "MultipartUpload" :: NullOrUndefined (CompletedMultipartUpload)
  , "UploadId" :: (MultipartUploadId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }


newtype CompletedMultipartUpload = CompletedMultipartUpload 
  { "Parts" :: NullOrUndefined (CompletedPartList)
  }


newtype CompletedPart = CompletedPart 
  { "ETag" :: NullOrUndefined (ETag)
  , "PartNumber" :: NullOrUndefined (PartNumber)
  }


newtype CompletedPartList = CompletedPartList (Array CompletedPart)


newtype Condition = Condition 
  { "HttpErrorCodeReturnedEquals" :: NullOrUndefined (HttpErrorCodeReturnedEquals)
  , "KeyPrefixEquals" :: NullOrUndefined (KeyPrefixEquals)
  }


newtype ConfirmRemoveSelfBucketAccess = ConfirmRemoveSelfBucketAccess Boolean


newtype ContentDisposition = ContentDisposition String


newtype ContentEncoding = ContentEncoding String


newtype ContentLanguage = ContentLanguage String


newtype ContentLength = ContentLength Number


newtype ContentMD5 = ContentMD5 String


newtype ContentRange = ContentRange String


newtype ContentType = ContentType String


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


newtype CopyObjectResult = CopyObjectResult 
  { "ETag" :: NullOrUndefined (ETag)
  , "LastModified" :: NullOrUndefined (LastModified)
  }


newtype CopyPartResult = CopyPartResult 
  { "ETag" :: NullOrUndefined (ETag)
  , "LastModified" :: NullOrUndefined (LastModified)
  }


newtype CopySource = CopySource String


newtype CopySourceIfMatch = CopySourceIfMatch String


newtype CopySourceIfModifiedSince = CopySourceIfModifiedSince Number


newtype CopySourceIfNoneMatch = CopySourceIfNoneMatch String


newtype CopySourceIfUnmodifiedSince = CopySourceIfUnmodifiedSince Number


newtype CopySourceRange = CopySourceRange String


newtype CopySourceSSECustomerAlgorithm = CopySourceSSECustomerAlgorithm String


newtype CopySourceSSECustomerKey = CopySourceSSECustomerKey String


newtype CopySourceSSECustomerKeyMD5 = CopySourceSSECustomerKeyMD5 String


newtype CopySourceVersionId = CopySourceVersionId String


newtype CreateBucketConfiguration = CreateBucketConfiguration 
  { "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint)
  }


newtype CreateBucketOutput = CreateBucketOutput 
  { "Location" :: NullOrUndefined (Location)
  }


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


newtype CreationDate = CreationDate Number


newtype Date = Date Number


newtype Days = Days Int


newtype DaysAfterInitiation = DaysAfterInitiation Int


newtype Delete = Delete 
  { "Objects" :: (ObjectIdentifierList)
  , "Quiet" :: NullOrUndefined (Quiet)
  }


newtype DeleteBucketAnalyticsConfigurationRequest = DeleteBucketAnalyticsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (AnalyticsId)
  }


newtype DeleteBucketCorsRequest = DeleteBucketCorsRequest 
  { "Bucket" :: (BucketName)
  }


newtype DeleteBucketEncryptionRequest = DeleteBucketEncryptionRequest 
  { "Bucket" :: (BucketName)
  }


newtype DeleteBucketInventoryConfigurationRequest = DeleteBucketInventoryConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (InventoryId)
  }


newtype DeleteBucketLifecycleRequest = DeleteBucketLifecycleRequest 
  { "Bucket" :: (BucketName)
  }


newtype DeleteBucketMetricsConfigurationRequest = DeleteBucketMetricsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (MetricsId)
  }


newtype DeleteBucketPolicyRequest = DeleteBucketPolicyRequest 
  { "Bucket" :: (BucketName)
  }


newtype DeleteBucketReplicationRequest = DeleteBucketReplicationRequest 
  { "Bucket" :: (BucketName)
  }


newtype DeleteBucketRequest = DeleteBucketRequest 
  { "Bucket" :: (BucketName)
  }


newtype DeleteBucketTaggingRequest = DeleteBucketTaggingRequest 
  { "Bucket" :: (BucketName)
  }


newtype DeleteBucketWebsiteRequest = DeleteBucketWebsiteRequest 
  { "Bucket" :: (BucketName)
  }


newtype DeleteMarker = DeleteMarker Boolean


newtype DeleteMarkerEntry = DeleteMarkerEntry 
  { "Owner" :: NullOrUndefined (Owner)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "IsLatest" :: NullOrUndefined (IsLatest)
  , "LastModified" :: NullOrUndefined (LastModified)
  }


newtype DeleteMarkerVersionId = DeleteMarkerVersionId String


newtype DeleteMarkers = DeleteMarkers (Array DeleteMarkerEntry)


newtype DeleteObjectOutput = DeleteObjectOutput 
  { "DeleteMarker" :: NullOrUndefined (DeleteMarker)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }


newtype DeleteObjectRequest = DeleteObjectRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "MFA" :: NullOrUndefined (MFA)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }


newtype DeleteObjectTaggingOutput = DeleteObjectTaggingOutput 
  { "VersionId" :: NullOrUndefined (ObjectVersionId)
  }


newtype DeleteObjectTaggingRequest = DeleteObjectTaggingRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  }


newtype DeleteObjectsOutput = DeleteObjectsOutput 
  { "Deleted" :: NullOrUndefined (DeletedObjects)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  , "Errors" :: NullOrUndefined (Errors)
  }


newtype DeleteObjectsRequest = DeleteObjectsRequest 
  { "Bucket" :: (BucketName)
  , "Delete" :: (Delete)
  , "MFA" :: NullOrUndefined (MFA)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }


newtype DeletedObject = DeletedObject 
  { "Key" :: NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "DeleteMarker" :: NullOrUndefined (DeleteMarker)
  , "DeleteMarkerVersionId" :: NullOrUndefined (DeleteMarkerVersionId)
  }


newtype DeletedObjects = DeletedObjects (Array DeletedObject)


newtype Delimiter = Delimiter String


newtype Description = Description String


-- | Container for replication destination information.
newtype Destination = Destination 
  { "Bucket" :: (BucketName)
  , "Account" :: NullOrUndefined (AccountId)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "AccessControlTranslation" :: NullOrUndefined (AccessControlTranslation)
  , "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration)
  }


newtype DisplayName = DisplayName String


newtype ETag = ETag String


newtype EmailAddress = EmailAddress String


-- | Requests Amazon S3 to encode the object keys in the response and specifies the encoding method to use. An object key may contain any Unicode character; however, XML 1.0 parser cannot parse some characters, such as characters with an ASCII value from 0 to 10. For characters that are not supported in XML 1.0, you can add this parameter to request that Amazon S3 encode the keys in the response.
newtype EncodingType = EncodingType String


-- | Describes the server-side encryption that will be applied to the restore results.
newtype Encryption = Encryption 
  { "EncryptionType" :: (ServerSideEncryption)
  , "KMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "KMSContext" :: NullOrUndefined (KMSContext)
  }


-- | Container for information regarding encryption based configuration for replicas.
newtype EncryptionConfiguration = EncryptionConfiguration 
  { "ReplicaKmsKeyID" :: NullOrUndefined (ReplicaKmsKeyID)
  }


newtype Error = Error 
  { "Key" :: NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "Code" :: NullOrUndefined (Code)
  , "Message" :: NullOrUndefined (Message)
  }


newtype ErrorDocument = ErrorDocument 
  { "Key" :: (ObjectKey)
  }


newtype Errors = Errors (Array Error)


-- | Bucket event for which to send notifications.
newtype Event = Event String


newtype EventList = EventList (Array Event)


newtype Expiration = Expiration String


newtype ExpirationStatus = ExpirationStatus String


newtype ExpiredObjectDeleteMarker = ExpiredObjectDeleteMarker Boolean


newtype Expires = Expires Number


newtype ExposeHeader = ExposeHeader String


newtype ExposeHeaders = ExposeHeaders (Array ExposeHeader)


newtype Expression = Expression String


newtype ExpressionType = ExpressionType String


newtype FetchOwner = FetchOwner Boolean


newtype FieldDelimiter = FieldDelimiter String


newtype FileHeaderInfo = FileHeaderInfo String


-- | Container for key value pair that defines the criteria for the filter rule.
newtype FilterRule = FilterRule 
  { "Name" :: NullOrUndefined (FilterRuleName)
  , "Value" :: NullOrUndefined (FilterRuleValue)
  }


-- | A list of containers for key value pair that defines the criteria for the filter rule.
newtype FilterRuleList = FilterRuleList (Array FilterRule)


newtype FilterRuleName = FilterRuleName String


newtype FilterRuleValue = FilterRuleValue String


newtype GetBucketAccelerateConfigurationOutput = GetBucketAccelerateConfigurationOutput 
  { "Status" :: NullOrUndefined (BucketAccelerateStatus)
  }


newtype GetBucketAccelerateConfigurationRequest = GetBucketAccelerateConfigurationRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketAclOutput = GetBucketAclOutput 
  { "Owner" :: NullOrUndefined (Owner)
  , "Grants" :: NullOrUndefined (Grants)
  }


newtype GetBucketAclRequest = GetBucketAclRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketAnalyticsConfigurationOutput = GetBucketAnalyticsConfigurationOutput 
  { "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfiguration)
  }


newtype GetBucketAnalyticsConfigurationRequest = GetBucketAnalyticsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (AnalyticsId)
  }


newtype GetBucketCorsOutput = GetBucketCorsOutput 
  { "CORSRules" :: NullOrUndefined (CORSRules)
  }


newtype GetBucketCorsRequest = GetBucketCorsRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketEncryptionOutput = GetBucketEncryptionOutput 
  { "ServerSideEncryptionConfiguration" :: NullOrUndefined (ServerSideEncryptionConfiguration)
  }


newtype GetBucketEncryptionRequest = GetBucketEncryptionRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketInventoryConfigurationOutput = GetBucketInventoryConfigurationOutput 
  { "InventoryConfiguration" :: NullOrUndefined (InventoryConfiguration)
  }


newtype GetBucketInventoryConfigurationRequest = GetBucketInventoryConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (InventoryId)
  }


newtype GetBucketLifecycleConfigurationOutput = GetBucketLifecycleConfigurationOutput 
  { "Rules" :: NullOrUndefined (LifecycleRules)
  }


newtype GetBucketLifecycleConfigurationRequest = GetBucketLifecycleConfigurationRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketLifecycleOutput = GetBucketLifecycleOutput 
  { "Rules" :: NullOrUndefined (Rules)
  }


newtype GetBucketLifecycleRequest = GetBucketLifecycleRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketLocationOutput = GetBucketLocationOutput 
  { "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint)
  }


newtype GetBucketLocationRequest = GetBucketLocationRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketLoggingOutput = GetBucketLoggingOutput 
  { "LoggingEnabled" :: NullOrUndefined (LoggingEnabled)
  }


newtype GetBucketLoggingRequest = GetBucketLoggingRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketMetricsConfigurationOutput = GetBucketMetricsConfigurationOutput 
  { "MetricsConfiguration" :: NullOrUndefined (MetricsConfiguration)
  }


newtype GetBucketMetricsConfigurationRequest = GetBucketMetricsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (MetricsId)
  }


newtype GetBucketNotificationConfigurationRequest = GetBucketNotificationConfigurationRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketPolicyOutput = GetBucketPolicyOutput 
  { "Policy" :: NullOrUndefined (Policy)
  }


newtype GetBucketPolicyRequest = GetBucketPolicyRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketReplicationOutput = GetBucketReplicationOutput 
  { "ReplicationConfiguration" :: NullOrUndefined (ReplicationConfiguration)
  }


newtype GetBucketReplicationRequest = GetBucketReplicationRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketRequestPaymentOutput = GetBucketRequestPaymentOutput 
  { "Payer" :: NullOrUndefined (Payer)
  }


newtype GetBucketRequestPaymentRequest = GetBucketRequestPaymentRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketTaggingOutput = GetBucketTaggingOutput 
  { "TagSet" :: (TagSet)
  }


newtype GetBucketTaggingRequest = GetBucketTaggingRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketVersioningOutput = GetBucketVersioningOutput 
  { "Status" :: NullOrUndefined (BucketVersioningStatus)
  , "MFADelete" :: NullOrUndefined (MFADeleteStatus)
  }


newtype GetBucketVersioningRequest = GetBucketVersioningRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetBucketWebsiteOutput = GetBucketWebsiteOutput 
  { "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo)
  , "IndexDocument" :: NullOrUndefined (IndexDocument)
  , "ErrorDocument" :: NullOrUndefined (ErrorDocument)
  , "RoutingRules" :: NullOrUndefined (RoutingRules)
  }


newtype GetBucketWebsiteRequest = GetBucketWebsiteRequest 
  { "Bucket" :: (BucketName)
  }


newtype GetObjectAclOutput = GetObjectAclOutput 
  { "Owner" :: NullOrUndefined (Owner)
  , "Grants" :: NullOrUndefined (Grants)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }


newtype GetObjectAclRequest = GetObjectAclRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }


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


newtype GetObjectTaggingOutput = GetObjectTaggingOutput 
  { "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "TagSet" :: (TagSet)
  }


newtype GetObjectTaggingRequest = GetObjectTaggingRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  }


newtype GetObjectTorrentOutput = GetObjectTorrentOutput 
  { "Body" :: NullOrUndefined (Body)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }


newtype GetObjectTorrentRequest = GetObjectTorrentRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }


newtype GlacierJobParameters = GlacierJobParameters 
  { "Tier" :: (Tier)
  }


newtype Grant = Grant 
  { "Grantee" :: NullOrUndefined (Grantee)
  , "Permission" :: NullOrUndefined (Permission)
  }


newtype GrantFullControl = GrantFullControl String


newtype GrantRead = GrantRead String


newtype GrantReadACP = GrantReadACP String


newtype GrantWrite = GrantWrite String


newtype GrantWriteACP = GrantWriteACP String


newtype Grantee = Grantee 
  { "DisplayName" :: NullOrUndefined (DisplayName)
  , "EmailAddress" :: NullOrUndefined (EmailAddress)
  , "ID" :: NullOrUndefined (ID)
  , "Type" :: (Type)
  , "URI" :: NullOrUndefined (URI)
  }


newtype Grants = Grants (Array Grant)


newtype HeadBucketRequest = HeadBucketRequest 
  { "Bucket" :: (BucketName)
  }


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


newtype HostName = HostName String


newtype HttpErrorCodeReturnedEquals = HttpErrorCodeReturnedEquals String


newtype HttpRedirectCode = HttpRedirectCode String


newtype ID = ID String


newtype IfMatch = IfMatch String


newtype IfModifiedSince = IfModifiedSince Number


newtype IfNoneMatch = IfNoneMatch String


newtype IfUnmodifiedSince = IfUnmodifiedSince Number


newtype IndexDocument = IndexDocument 
  { "Suffix" :: (Suffix)
  }


newtype Initiated = Initiated Number


newtype Initiator = Initiator 
  { "ID" :: NullOrUndefined (ID)
  , "DisplayName" :: NullOrUndefined (DisplayName)
  }


-- | Describes the serialization format of the object.
newtype InputSerialization = InputSerialization 
  { "CSV" :: NullOrUndefined (CSVInput)
  }


newtype InventoryConfiguration = InventoryConfiguration 
  { "Destination" :: (InventoryDestination)
  , "IsEnabled" :: (IsEnabled)
  , "Filter" :: NullOrUndefined (InventoryFilter)
  , "Id" :: (InventoryId)
  , "IncludedObjectVersions" :: (InventoryIncludedObjectVersions)
  , "OptionalFields" :: NullOrUndefined (InventoryOptionalFields)
  , "Schedule" :: (InventorySchedule)
  }


newtype InventoryConfigurationList = InventoryConfigurationList (Array InventoryConfiguration)


newtype InventoryDestination = InventoryDestination 
  { "S3BucketDestination" :: (InventoryS3BucketDestination)
  }


-- | Contains the type of server-side encryption used to encrypt the inventory results.
newtype InventoryEncryption = InventoryEncryption 
  { "SSES3" :: NullOrUndefined (SSES3)
  , "SSEKMS" :: NullOrUndefined (SSEKMS)
  }


newtype InventoryFilter = InventoryFilter 
  { "Prefix" :: (Prefix)
  }


newtype InventoryFormat = InventoryFormat String


newtype InventoryFrequency = InventoryFrequency String


newtype InventoryId = InventoryId String


newtype InventoryIncludedObjectVersions = InventoryIncludedObjectVersions String


newtype InventoryOptionalField = InventoryOptionalField String


newtype InventoryOptionalFields = InventoryOptionalFields (Array InventoryOptionalField)


newtype InventoryS3BucketDestination = InventoryS3BucketDestination 
  { "AccountId" :: NullOrUndefined (AccountId)
  , "Bucket" :: (BucketName)
  , "Format" :: (InventoryFormat)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Encryption" :: NullOrUndefined (InventoryEncryption)
  }


newtype InventorySchedule = InventorySchedule 
  { "Frequency" :: (InventoryFrequency)
  }


newtype IsEnabled = IsEnabled Boolean


newtype IsLatest = IsLatest Boolean


newtype IsTruncated = IsTruncated Boolean


newtype KMSContext = KMSContext String


newtype KeyCount = KeyCount Int


newtype KeyMarker = KeyMarker String


newtype KeyPrefixEquals = KeyPrefixEquals String


newtype LambdaFunctionArn = LambdaFunctionArn String


-- | Container for specifying the AWS Lambda notification configuration.
newtype LambdaFunctionConfiguration = LambdaFunctionConfiguration 
  { "Id" :: NullOrUndefined (NotificationId)
  , "LambdaFunctionArn" :: (LambdaFunctionArn)
  , "Events" :: (EventList)
  , "Filter" :: NullOrUndefined (NotificationConfigurationFilter)
  }


newtype LambdaFunctionConfigurationList = LambdaFunctionConfigurationList (Array LambdaFunctionConfiguration)


newtype LastModified = LastModified Number


newtype LifecycleConfiguration = LifecycleConfiguration 
  { "Rules" :: (Rules)
  }


newtype LifecycleExpiration = LifecycleExpiration 
  { "Date" :: NullOrUndefined (Date)
  , "Days" :: NullOrUndefined (Days)
  , "ExpiredObjectDeleteMarker" :: NullOrUndefined (ExpiredObjectDeleteMarker)
  }


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


-- | This is used in a Lifecycle Rule Filter to apply a logical AND to two or more predicates. The Lifecycle Rule will apply to any object matching all of the predicates configured inside the And operator.
newtype LifecycleRuleAndOperator = LifecycleRuleAndOperator 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tags" :: NullOrUndefined (TagSet)
  }


-- | The Filter is used to identify objects that a Lifecycle Rule applies to. A Filter must have exactly one of Prefix, Tag, or And specified.
newtype LifecycleRuleFilter = LifecycleRuleFilter 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tag" :: NullOrUndefined (Tag)
  , "And" :: NullOrUndefined (LifecycleRuleAndOperator)
  }


newtype LifecycleRules = LifecycleRules (Array LifecycleRule)


newtype ListBucketAnalyticsConfigurationsOutput = ListBucketAnalyticsConfigurationsOutput 
  { "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "ContinuationToken" :: NullOrUndefined (Token)
  , "NextContinuationToken" :: NullOrUndefined (NextToken)
  , "AnalyticsConfigurationList" :: NullOrUndefined (AnalyticsConfigurationList)
  }


newtype ListBucketAnalyticsConfigurationsRequest = ListBucketAnalyticsConfigurationsRequest 
  { "Bucket" :: (BucketName)
  , "ContinuationToken" :: NullOrUndefined (Token)
  }


newtype ListBucketInventoryConfigurationsOutput = ListBucketInventoryConfigurationsOutput 
  { "ContinuationToken" :: NullOrUndefined (Token)
  , "InventoryConfigurationList" :: NullOrUndefined (InventoryConfigurationList)
  , "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "NextContinuationToken" :: NullOrUndefined (NextToken)
  }


newtype ListBucketInventoryConfigurationsRequest = ListBucketInventoryConfigurationsRequest 
  { "Bucket" :: (BucketName)
  , "ContinuationToken" :: NullOrUndefined (Token)
  }


newtype ListBucketMetricsConfigurationsOutput = ListBucketMetricsConfigurationsOutput 
  { "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "ContinuationToken" :: NullOrUndefined (Token)
  , "NextContinuationToken" :: NullOrUndefined (NextToken)
  , "MetricsConfigurationList" :: NullOrUndefined (MetricsConfigurationList)
  }


newtype ListBucketMetricsConfigurationsRequest = ListBucketMetricsConfigurationsRequest 
  { "Bucket" :: (BucketName)
  , "ContinuationToken" :: NullOrUndefined (Token)
  }


newtype ListBucketsOutput = ListBucketsOutput 
  { "Buckets" :: NullOrUndefined (Buckets)
  , "Owner" :: NullOrUndefined (Owner)
  }


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


newtype ListMultipartUploadsRequest = ListMultipartUploadsRequest 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "KeyMarker" :: NullOrUndefined (KeyMarker)
  , "MaxUploads" :: NullOrUndefined (MaxUploads)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "UploadIdMarker" :: NullOrUndefined (UploadIdMarker)
  }


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


newtype ListObjectVersionsRequest = ListObjectVersionsRequest 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "KeyMarker" :: NullOrUndefined (KeyMarker)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "VersionIdMarker" :: NullOrUndefined (VersionIdMarker)
  }


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


newtype ListObjectsRequest = ListObjectsRequest 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "Marker" :: NullOrUndefined (Marker)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }


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


newtype ListPartsRequest = ListPartsRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "MaxParts" :: NullOrUndefined (MaxParts)
  , "PartNumberMarker" :: NullOrUndefined (PartNumberMarker)
  , "UploadId" :: (MultipartUploadId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }


newtype Location = Location String


newtype LocationPrefix = LocationPrefix String


newtype LoggingEnabled = LoggingEnabled 
  { "TargetBucket" :: NullOrUndefined (TargetBucket)
  , "TargetGrants" :: NullOrUndefined (TargetGrants)
  , "TargetPrefix" :: NullOrUndefined (TargetPrefix)
  }


newtype MFA = MFA String


newtype MFADelete = MFADelete String


newtype MFADeleteStatus = MFADeleteStatus String


newtype Marker = Marker String


newtype MaxAgeSeconds = MaxAgeSeconds Int


newtype MaxKeys = MaxKeys Int


newtype MaxParts = MaxParts Int


newtype MaxUploads = MaxUploads Int


newtype Message = Message String


newtype Metadata = Metadata (Map MetadataKey MetadataValue)


newtype MetadataDirective = MetadataDirective String


-- | A metadata key-value pair to store with an object.
newtype MetadataEntry = MetadataEntry 
  { "Name" :: NullOrUndefined (MetadataKey)
  , "Value" :: NullOrUndefined (MetadataValue)
  }


newtype MetadataKey = MetadataKey String


newtype MetadataValue = MetadataValue String


newtype MetricsAndOperator = MetricsAndOperator 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tags" :: NullOrUndefined (TagSet)
  }


newtype MetricsConfiguration = MetricsConfiguration 
  { "Id" :: (MetricsId)
  , "Filter" :: NullOrUndefined (MetricsFilter)
  }


newtype MetricsConfigurationList = MetricsConfigurationList (Array MetricsConfiguration)


newtype MetricsFilter = MetricsFilter 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tag" :: NullOrUndefined (Tag)
  , "And" :: NullOrUndefined (MetricsAndOperator)
  }


newtype MetricsId = MetricsId String


newtype MissingMeta = MissingMeta Int


newtype MultipartUpload = MultipartUpload 
  { "UploadId" :: NullOrUndefined (MultipartUploadId)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "Initiated" :: NullOrUndefined (Initiated)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "Owner" :: NullOrUndefined (Owner)
  , "Initiator" :: NullOrUndefined (Initiator)
  }


newtype MultipartUploadId = MultipartUploadId String


newtype MultipartUploadList = MultipartUploadList (Array MultipartUpload)


newtype NextKeyMarker = NextKeyMarker String


newtype NextMarker = NextMarker String


newtype NextPartNumberMarker = NextPartNumberMarker Int


newtype NextToken = NextToken String


newtype NextUploadIdMarker = NextUploadIdMarker String


newtype NextVersionIdMarker = NextVersionIdMarker String


-- | The specified bucket does not exist.
newtype NoSuchBucket = NoSuchBucket 
  { 
  }


-- | The specified key does not exist.
newtype NoSuchKey = NoSuchKey 
  { 
  }


-- | The specified multipart upload does not exist.
newtype NoSuchUpload = NoSuchUpload 
  { 
  }


-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon S3 permanently deletes the noncurrent object versions. You set this lifecycle configuration action on a bucket that has versioning enabled (or suspended) to request that Amazon S3 delete noncurrent object versions at a specific period in the object's lifetime.
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration 
  { "NoncurrentDays" :: NullOrUndefined (Days)
  }


-- | Container for the transition rule that describes when noncurrent objects transition to the STANDARD_IA or GLACIER storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to the STANDARD_IA or GLACIER storage class at a specific period in the object's lifetime.
newtype NoncurrentVersionTransition = NoncurrentVersionTransition 
  { "NoncurrentDays" :: NullOrUndefined (Days)
  , "StorageClass" :: NullOrUndefined (TransitionStorageClass)
  }


newtype NoncurrentVersionTransitionList = NoncurrentVersionTransitionList (Array NoncurrentVersionTransition)


-- | Container for specifying the notification configuration of the bucket. If this element is empty, notifications are turned off on the bucket.
newtype NotificationConfiguration = NotificationConfiguration 
  { "TopicConfigurations" :: NullOrUndefined (TopicConfigurationList)
  , "QueueConfigurations" :: NullOrUndefined (QueueConfigurationList)
  , "LambdaFunctionConfigurations" :: NullOrUndefined (LambdaFunctionConfigurationList)
  }


newtype NotificationConfigurationDeprecated = NotificationConfigurationDeprecated 
  { "TopicConfiguration" :: NullOrUndefined (TopicConfigurationDeprecated)
  , "QueueConfiguration" :: NullOrUndefined (QueueConfigurationDeprecated)
  , "CloudFunctionConfiguration" :: NullOrUndefined (CloudFunctionConfiguration)
  }


-- | Container for object key name filtering rules. For information about key name filtering, go to <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html">Configuring Event Notifications</a> in the Amazon Simple Storage Service Developer Guide.
newtype NotificationConfigurationFilter = NotificationConfigurationFilter 
  { "Key" :: NullOrUndefined (S3KeyFilter)
  }


-- | Optional unique identifier for configurations in a notification configuration. If you don't provide one, Amazon S3 will assign an ID.
newtype NotificationId = NotificationId String


newtype Object = Object 
  { "Key" :: NullOrUndefined (ObjectKey)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "ETag" :: NullOrUndefined (ETag)
  , "Size" :: NullOrUndefined (Size)
  , "StorageClass" :: NullOrUndefined (ObjectStorageClass)
  , "Owner" :: NullOrUndefined (Owner)
  }


-- | This operation is not allowed against this storage tier
newtype ObjectAlreadyInActiveTierError = ObjectAlreadyInActiveTierError 
  { 
  }


newtype ObjectCannedACL = ObjectCannedACL String


newtype ObjectIdentifier = ObjectIdentifier 
  { "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  }


newtype ObjectIdentifierList = ObjectIdentifierList (Array ObjectIdentifier)


newtype ObjectKey = ObjectKey String


newtype ObjectList = ObjectList (Array Object)


-- | The source object of the COPY operation is not in the active tier and is only stored in Amazon Glacier.
newtype ObjectNotInActiveTierError = ObjectNotInActiveTierError 
  { 
  }


newtype ObjectStorageClass = ObjectStorageClass String


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


newtype ObjectVersionId = ObjectVersionId String


newtype ObjectVersionList = ObjectVersionList (Array ObjectVersion)


newtype ObjectVersionStorageClass = ObjectVersionStorageClass String


-- | Describes the location where the restore job's output is stored.
newtype OutputLocation = OutputLocation 
  { "S3" :: NullOrUndefined (S3Location)
  }


-- | Describes how results of the Select job are serialized.
newtype OutputSerialization = OutputSerialization 
  { "CSV" :: NullOrUndefined (CSVOutput)
  }


newtype Owner = Owner 
  { "DisplayName" :: NullOrUndefined (DisplayName)
  , "ID" :: NullOrUndefined (ID)
  }


newtype OwnerOverride = OwnerOverride String


newtype Part = Part 
  { "PartNumber" :: NullOrUndefined (PartNumber)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "ETag" :: NullOrUndefined (ETag)
  , "Size" :: NullOrUndefined (Size)
  }


newtype PartNumber = PartNumber Int


newtype PartNumberMarker = PartNumberMarker Int


newtype Parts = Parts (Array Part)


newtype PartsCount = PartsCount Int


newtype Payer = Payer String


newtype Permission = Permission String


newtype Policy = Policy String


newtype Prefix = Prefix String


newtype Protocol = Protocol String


newtype PutBucketAccelerateConfigurationRequest = PutBucketAccelerateConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "AccelerateConfiguration" :: (AccelerateConfiguration)
  }


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


newtype PutBucketAnalyticsConfigurationRequest = PutBucketAnalyticsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (AnalyticsId)
  , "AnalyticsConfiguration" :: (AnalyticsConfiguration)
  }


newtype PutBucketCorsRequest = PutBucketCorsRequest 
  { "Bucket" :: (BucketName)
  , "CORSConfiguration" :: (CORSConfiguration)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  }


newtype PutBucketEncryptionRequest = PutBucketEncryptionRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "ServerSideEncryptionConfiguration" :: (ServerSideEncryptionConfiguration)
  }


newtype PutBucketInventoryConfigurationRequest = PutBucketInventoryConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (InventoryId)
  , "InventoryConfiguration" :: (InventoryConfiguration)
  }


newtype PutBucketLifecycleConfigurationRequest = PutBucketLifecycleConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "LifecycleConfiguration" :: NullOrUndefined (BucketLifecycleConfiguration)
  }


newtype PutBucketLifecycleRequest = PutBucketLifecycleRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "LifecycleConfiguration" :: NullOrUndefined (LifecycleConfiguration)
  }


newtype PutBucketLoggingRequest = PutBucketLoggingRequest 
  { "Bucket" :: (BucketName)
  , "BucketLoggingStatus" :: (BucketLoggingStatus)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  }


newtype PutBucketMetricsConfigurationRequest = PutBucketMetricsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (MetricsId)
  , "MetricsConfiguration" :: (MetricsConfiguration)
  }


newtype PutBucketNotificationConfigurationRequest = PutBucketNotificationConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "NotificationConfiguration" :: (NotificationConfiguration)
  }


newtype PutBucketNotificationRequest = PutBucketNotificationRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "NotificationConfiguration" :: (NotificationConfigurationDeprecated)
  }


newtype PutBucketPolicyRequest = PutBucketPolicyRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "ConfirmRemoveSelfBucketAccess" :: NullOrUndefined (ConfirmRemoveSelfBucketAccess)
  , "Policy" :: (Policy)
  }


newtype PutBucketReplicationRequest = PutBucketReplicationRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "ReplicationConfiguration" :: (ReplicationConfiguration)
  }


newtype PutBucketRequestPaymentRequest = PutBucketRequestPaymentRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "RequestPaymentConfiguration" :: (RequestPaymentConfiguration)
  }


newtype PutBucketTaggingRequest = PutBucketTaggingRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "Tagging" :: (Tagging)
  }


newtype PutBucketVersioningRequest = PutBucketVersioningRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "MFA" :: NullOrUndefined (MFA)
  , "VersioningConfiguration" :: (VersioningConfiguration)
  }


newtype PutBucketWebsiteRequest = PutBucketWebsiteRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "WebsiteConfiguration" :: (WebsiteConfiguration)
  }


newtype PutObjectAclOutput = PutObjectAclOutput 
  { "RequestCharged" :: NullOrUndefined (RequestCharged)
  }


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


newtype PutObjectTaggingOutput = PutObjectTaggingOutput 
  { "VersionId" :: NullOrUndefined (ObjectVersionId)
  }


newtype PutObjectTaggingRequest = PutObjectTaggingRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "Tagging" :: (Tagging)
  }


newtype QueueArn = QueueArn String


-- | Container for specifying an configuration when you want Amazon S3 to publish events to an Amazon Simple Queue Service (Amazon SQS) queue.
newtype QueueConfiguration = QueueConfiguration 
  { "Id" :: NullOrUndefined (NotificationId)
  , "QueueArn" :: (QueueArn)
  , "Events" :: (EventList)
  , "Filter" :: NullOrUndefined (NotificationConfigurationFilter)
  }


newtype QueueConfigurationDeprecated = QueueConfigurationDeprecated 
  { "Id" :: NullOrUndefined (NotificationId)
  , "Event" :: NullOrUndefined (Event)
  , "Events" :: NullOrUndefined (EventList)
  , "Queue" :: NullOrUndefined (QueueArn)
  }


newtype QueueConfigurationList = QueueConfigurationList (Array QueueConfiguration)


newtype Quiet = Quiet Boolean


newtype QuoteCharacter = QuoteCharacter String


newtype QuoteEscapeCharacter = QuoteEscapeCharacter String


newtype QuoteFields = QuoteFields String


newtype Range = Range String


newtype RecordDelimiter = RecordDelimiter String


newtype Redirect = Redirect 
  { "HostName" :: NullOrUndefined (HostName)
  , "HttpRedirectCode" :: NullOrUndefined (HttpRedirectCode)
  , "Protocol" :: NullOrUndefined (Protocol)
  , "ReplaceKeyPrefixWith" :: NullOrUndefined (ReplaceKeyPrefixWith)
  , "ReplaceKeyWith" :: NullOrUndefined (ReplaceKeyWith)
  }


newtype RedirectAllRequestsTo = RedirectAllRequestsTo 
  { "HostName" :: (HostName)
  , "Protocol" :: NullOrUndefined (Protocol)
  }


newtype ReplaceKeyPrefixWith = ReplaceKeyPrefixWith String


newtype ReplaceKeyWith = ReplaceKeyWith String


newtype ReplicaKmsKeyID = ReplicaKmsKeyID String


-- | Container for replication rules. You can add as many as 1,000 rules. Total replication configuration size can be up to 2 MB.
newtype ReplicationConfiguration = ReplicationConfiguration 
  { "Role" :: (Role)
  , "Rules" :: (ReplicationRules)
  }


-- | Container for information about a particular replication rule.
newtype ReplicationRule = ReplicationRule 
  { "ID" :: NullOrUndefined (ID)
  , "Prefix" :: (Prefix)
  , "Status" :: (ReplicationRuleStatus)
  , "SourceSelectionCriteria" :: NullOrUndefined (SourceSelectionCriteria)
  , "Destination" :: (Destination)
  }


newtype ReplicationRuleStatus = ReplicationRuleStatus String


newtype ReplicationRules = ReplicationRules (Array ReplicationRule)


newtype ReplicationStatus = ReplicationStatus String


-- | If present, indicates that the requester was successfully charged for the request.
newtype RequestCharged = RequestCharged String


-- | Confirms that the requester knows that she or he will be charged for the request. Bucket owners need not specify this parameter in their requests. Documentation on downloading objects from requester pays buckets can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/ObjectsinRequesterPaysBuckets.html
newtype RequestPayer = RequestPayer String


newtype RequestPaymentConfiguration = RequestPaymentConfiguration 
  { "Payer" :: (Payer)
  }


newtype ResponseCacheControl = ResponseCacheControl String


newtype ResponseContentDisposition = ResponseContentDisposition String


newtype ResponseContentEncoding = ResponseContentEncoding String


newtype ResponseContentLanguage = ResponseContentLanguage String


newtype ResponseContentType = ResponseContentType String


newtype ResponseExpires = ResponseExpires Number


newtype Restore = Restore String


newtype RestoreObjectOutput = RestoreObjectOutput 
  { "RequestCharged" :: NullOrUndefined (RequestCharged)
  , "RestoreOutputPath" :: NullOrUndefined (RestoreOutputPath)
  }


newtype RestoreObjectRequest = RestoreObjectRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "RestoreRequest" :: NullOrUndefined (RestoreRequest)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }


newtype RestoreOutputPath = RestoreOutputPath String


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


newtype RestoreRequestType = RestoreRequestType String


newtype Role = Role String


newtype RoutingRule = RoutingRule 
  { "Condition" :: NullOrUndefined (Condition)
  , "Redirect" :: (Redirect)
  }


newtype RoutingRules = RoutingRules (Array RoutingRule)


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


newtype Rules = Rules (Array Rule)


-- | Container for object key name prefix and suffix filtering rules.
newtype S3KeyFilter = S3KeyFilter 
  { "FilterRules" :: NullOrUndefined (FilterRuleList)
  }


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


newtype SSECustomerAlgorithm = SSECustomerAlgorithm String


newtype SSECustomerKey = SSECustomerKey String


newtype SSECustomerKeyMD5 = SSECustomerKeyMD5 String


-- | Specifies the use of SSE-KMS to encrypt delievered Inventory reports.
newtype SSEKMS = SSEKMS 
  { "KeyId" :: (SSEKMSKeyId)
  }


newtype SSEKMSKeyId = SSEKMSKeyId String


-- | Specifies the use of SSE-S3 to encrypt delievered Inventory reports.
newtype SSES3 = SSES3 
  { 
  }


-- | Describes the parameters for Select job types.
newtype SelectParameters = SelectParameters 
  { "InputSerialization" :: (InputSerialization)
  , "ExpressionType" :: (ExpressionType)
  , "Expression" :: (Expression)
  , "OutputSerialization" :: (OutputSerialization)
  }


newtype ServerSideEncryption = ServerSideEncryption String


-- | Describes the default server-side encryption to apply to new objects in the bucket. If Put Object request does not specify any server-side encryption, this default encryption will be applied.
newtype ServerSideEncryptionByDefault = ServerSideEncryptionByDefault 
  { "SSEAlgorithm" :: (ServerSideEncryption)
  , "KMSMasterKeyID" :: NullOrUndefined (SSEKMSKeyId)
  }


-- | Container for server-side encryption configuration rules. Currently S3 supports one rule only.
newtype ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration 
  { "Rules" :: (ServerSideEncryptionRules)
  }


-- | Container for information about a particular server-side encryption configuration rule.
newtype ServerSideEncryptionRule = ServerSideEncryptionRule 
  { "ApplyServerSideEncryptionByDefault" :: NullOrUndefined (ServerSideEncryptionByDefault)
  }


newtype ServerSideEncryptionRules = ServerSideEncryptionRules (Array ServerSideEncryptionRule)


newtype Size = Size Int


-- | Container for filters that define which source objects should be replicated.
newtype SourceSelectionCriteria = SourceSelectionCriteria 
  { "SseKmsEncryptedObjects" :: NullOrUndefined (SseKmsEncryptedObjects)
  }


-- | Container for filter information of selection of KMS Encrypted S3 objects.
newtype SseKmsEncryptedObjects = SseKmsEncryptedObjects 
  { "Status" :: (SseKmsEncryptedObjectsStatus)
  }


newtype SseKmsEncryptedObjectsStatus = SseKmsEncryptedObjectsStatus String


newtype StartAfter = StartAfter String


newtype StorageClass = StorageClass String


newtype StorageClassAnalysis = StorageClassAnalysis 
  { "DataExport" :: NullOrUndefined (StorageClassAnalysisDataExport)
  }


newtype StorageClassAnalysisDataExport = StorageClassAnalysisDataExport 
  { "OutputSchemaVersion" :: (StorageClassAnalysisSchemaVersion)
  , "Destination" :: (AnalyticsExportDestination)
  }


newtype StorageClassAnalysisSchemaVersion = StorageClassAnalysisSchemaVersion String


newtype Suffix = Suffix String


newtype Tag = Tag 
  { "Key" :: (ObjectKey)
  , "Value" :: (Value)
  }


newtype TagCount = TagCount Int


newtype TagSet = TagSet (Array Tag)


newtype Tagging = Tagging 
  { "TagSet" :: (TagSet)
  }


newtype TaggingDirective = TaggingDirective String


newtype TaggingHeader = TaggingHeader String


newtype TargetBucket = TargetBucket String


newtype TargetGrant = TargetGrant 
  { "Grantee" :: NullOrUndefined (Grantee)
  , "Permission" :: NullOrUndefined (BucketLogsPermission)
  }


newtype TargetGrants = TargetGrants (Array TargetGrant)


newtype TargetPrefix = TargetPrefix String


newtype Tier = Tier String


newtype Token = Token String


newtype TopicArn = TopicArn String


-- | Container for specifying the configuration when you want Amazon S3 to publish events to an Amazon Simple Notification Service (Amazon SNS) topic.
newtype TopicConfiguration = TopicConfiguration 
  { "Id" :: NullOrUndefined (NotificationId)
  , "TopicArn" :: (TopicArn)
  , "Events" :: (EventList)
  , "Filter" :: NullOrUndefined (NotificationConfigurationFilter)
  }


newtype TopicConfigurationDeprecated = TopicConfigurationDeprecated 
  { "Id" :: NullOrUndefined (NotificationId)
  , "Events" :: NullOrUndefined (EventList)
  , "Event" :: NullOrUndefined (Event)
  , "Topic" :: NullOrUndefined (TopicArn)
  }


newtype TopicConfigurationList = TopicConfigurationList (Array TopicConfiguration)


newtype Transition = Transition 
  { "Date" :: NullOrUndefined (Date)
  , "Days" :: NullOrUndefined (Days)
  , "StorageClass" :: NullOrUndefined (TransitionStorageClass)
  }


newtype TransitionList = TransitionList (Array Transition)


newtype TransitionStorageClass = TransitionStorageClass String


newtype Type = Type String


newtype URI = URI String


newtype UploadIdMarker = UploadIdMarker String


newtype UploadPartCopyOutput = UploadPartCopyOutput 
  { "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId)
  , "CopyPartResult" :: NullOrUndefined (CopyPartResult)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }


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


newtype UploadPartOutput = UploadPartOutput 
  { "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "ETag" :: NullOrUndefined (ETag)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }


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


newtype UserMetadata = UserMetadata (Array MetadataEntry)


newtype Value = Value String


newtype VersionIdMarker = VersionIdMarker String


newtype VersioningConfiguration = VersioningConfiguration 
  { "MFADelete" :: NullOrUndefined (MFADelete)
  , "Status" :: NullOrUndefined (BucketVersioningStatus)
  }


newtype WebsiteConfiguration = WebsiteConfiguration 
  { "ErrorDocument" :: NullOrUndefined (ErrorDocument)
  , "IndexDocument" :: NullOrUndefined (IndexDocument)
  , "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo)
  , "RoutingRules" :: NullOrUndefined (RoutingRules)
  }


newtype WebsiteRedirectLocation = WebsiteRedirectLocation String
