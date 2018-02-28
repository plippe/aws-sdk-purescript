

module AWS.S3 where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined as NullOrUndefined
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap

import AWS.Request as Request
import AWS.Request.Types as Types

serviceName = "S3" :: String


-- | <p>Aborts a multipart upload.</p><p>To verify that all parts have been removed, so you don't get charged for the part storage, you should call the List Parts operation and ensure the parts list is empty.</p>
abortMultipartUpload :: forall eff. AbortMultipartUploadRequest -> Aff (exception :: EXCEPTION | eff) AbortMultipartUploadOutput
abortMultipartUpload = Request.request serviceName "abortMultipartUpload" 


-- | Completes a multipart upload by assembling previously uploaded parts.
completeMultipartUpload :: forall eff. CompleteMultipartUploadRequest -> Aff (exception :: EXCEPTION | eff) CompleteMultipartUploadOutput
completeMultipartUpload = Request.request serviceName "completeMultipartUpload" 


-- | Creates a copy of an object that is already stored in Amazon S3.
copyObject :: forall eff. CopyObjectRequest -> Aff (exception :: EXCEPTION | eff) CopyObjectOutput
copyObject = Request.request serviceName "copyObject" 


-- | Creates a new bucket.
createBucket :: forall eff. CreateBucketRequest -> Aff (exception :: EXCEPTION | eff) CreateBucketOutput
createBucket = Request.request serviceName "createBucket" 


-- | <p>Initiates a multipart upload and returns an upload ID.</p><p><b>Note:</b> After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.</p>
createMultipartUpload :: forall eff. CreateMultipartUploadRequest -> Aff (exception :: EXCEPTION | eff) CreateMultipartUploadOutput
createMultipartUpload = Request.request serviceName "createMultipartUpload" 


-- | Deletes the bucket. All objects (including all object versions and Delete Markers) in the bucket must be deleted before the bucket itself can be deleted.
deleteBucket :: forall eff. DeleteBucketRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBucket = Request.request serviceName "deleteBucket" 


-- | Deletes an analytics configuration for the bucket (specified by the analytics configuration ID).
deleteBucketAnalyticsConfiguration :: forall eff. DeleteBucketAnalyticsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBucketAnalyticsConfiguration = Request.request serviceName "deleteBucketAnalyticsConfiguration" 


-- | Deletes the cors configuration information set for the bucket.
deleteBucketCors :: forall eff. DeleteBucketCorsRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBucketCors = Request.request serviceName "deleteBucketCors" 


-- | Deletes the server-side encryption configuration from the bucket.
deleteBucketEncryption :: forall eff. DeleteBucketEncryptionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBucketEncryption = Request.request serviceName "deleteBucketEncryption" 


-- | Deletes an inventory configuration (identified by the inventory ID) from the bucket.
deleteBucketInventoryConfiguration :: forall eff. DeleteBucketInventoryConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBucketInventoryConfiguration = Request.request serviceName "deleteBucketInventoryConfiguration" 


-- | Deletes the lifecycle configuration from the bucket.
deleteBucketLifecycle :: forall eff. DeleteBucketLifecycleRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBucketLifecycle = Request.request serviceName "deleteBucketLifecycle" 


-- | Deletes a metrics configuration (specified by the metrics configuration ID) from the bucket.
deleteBucketMetricsConfiguration :: forall eff. DeleteBucketMetricsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBucketMetricsConfiguration = Request.request serviceName "deleteBucketMetricsConfiguration" 


-- | Deletes the policy from the bucket.
deleteBucketPolicy :: forall eff. DeleteBucketPolicyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBucketPolicy = Request.request serviceName "deleteBucketPolicy" 


-- | Deletes the replication configuration from the bucket.
deleteBucketReplication :: forall eff. DeleteBucketReplicationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBucketReplication = Request.request serviceName "deleteBucketReplication" 


-- | Deletes the tags from the bucket.
deleteBucketTagging :: forall eff. DeleteBucketTaggingRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBucketTagging = Request.request serviceName "deleteBucketTagging" 


-- | This operation removes the website configuration from the bucket.
deleteBucketWebsite :: forall eff. DeleteBucketWebsiteRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBucketWebsite = Request.request serviceName "deleteBucketWebsite" 


-- | Removes the null version (if there is one) of an object and inserts a delete marker, which becomes the latest version of the object. If there isn't a null version, Amazon S3 does not remove any objects.
deleteObject :: forall eff. DeleteObjectRequest -> Aff (exception :: EXCEPTION | eff) DeleteObjectOutput
deleteObject = Request.request serviceName "deleteObject" 


-- | Removes the tag-set from an existing object.
deleteObjectTagging :: forall eff. DeleteObjectTaggingRequest -> Aff (exception :: EXCEPTION | eff) DeleteObjectTaggingOutput
deleteObjectTagging = Request.request serviceName "deleteObjectTagging" 


-- | This operation enables you to delete multiple objects from a bucket using a single HTTP request. You may specify up to 1000 keys.
deleteObjects :: forall eff. DeleteObjectsRequest -> Aff (exception :: EXCEPTION | eff) DeleteObjectsOutput
deleteObjects = Request.request serviceName "deleteObjects" 


-- | Returns the accelerate configuration of a bucket.
getBucketAccelerateConfiguration :: forall eff. GetBucketAccelerateConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketAccelerateConfigurationOutput
getBucketAccelerateConfiguration = Request.request serviceName "getBucketAccelerateConfiguration" 


-- | Gets the access control policy for the bucket.
getBucketAcl :: forall eff. GetBucketAclRequest -> Aff (exception :: EXCEPTION | eff) GetBucketAclOutput
getBucketAcl = Request.request serviceName "getBucketAcl" 


-- | Gets an analytics configuration for the bucket (specified by the analytics configuration ID).
getBucketAnalyticsConfiguration :: forall eff. GetBucketAnalyticsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketAnalyticsConfigurationOutput
getBucketAnalyticsConfiguration = Request.request serviceName "getBucketAnalyticsConfiguration" 


-- | Returns the cors configuration for the bucket.
getBucketCors :: forall eff. GetBucketCorsRequest -> Aff (exception :: EXCEPTION | eff) GetBucketCorsOutput
getBucketCors = Request.request serviceName "getBucketCors" 


-- | Returns the server-side encryption configuration of a bucket.
getBucketEncryption :: forall eff. GetBucketEncryptionRequest -> Aff (exception :: EXCEPTION | eff) GetBucketEncryptionOutput
getBucketEncryption = Request.request serviceName "getBucketEncryption" 


-- | Returns an inventory configuration (identified by the inventory ID) from the bucket.
getBucketInventoryConfiguration :: forall eff. GetBucketInventoryConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketInventoryConfigurationOutput
getBucketInventoryConfiguration = Request.request serviceName "getBucketInventoryConfiguration" 


-- | Deprecated, see the GetBucketLifecycleConfiguration operation.
getBucketLifecycle :: forall eff. GetBucketLifecycleRequest -> Aff (exception :: EXCEPTION | eff) GetBucketLifecycleOutput
getBucketLifecycle = Request.request serviceName "getBucketLifecycle" 


-- | Returns the lifecycle configuration information set on the bucket.
getBucketLifecycleConfiguration :: forall eff. GetBucketLifecycleConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketLifecycleConfigurationOutput
getBucketLifecycleConfiguration = Request.request serviceName "getBucketLifecycleConfiguration" 


-- | Returns the region the bucket resides in.
getBucketLocation :: forall eff. GetBucketLocationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketLocationOutput
getBucketLocation = Request.request serviceName "getBucketLocation" 


-- | Returns the logging status of a bucket and the permissions users have to view and modify that status. To use GET, you must be the bucket owner.
getBucketLogging :: forall eff. GetBucketLoggingRequest -> Aff (exception :: EXCEPTION | eff) GetBucketLoggingOutput
getBucketLogging = Request.request serviceName "getBucketLogging" 


-- | Gets a metrics configuration (specified by the metrics configuration ID) from the bucket.
getBucketMetricsConfiguration :: forall eff. GetBucketMetricsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketMetricsConfigurationOutput
getBucketMetricsConfiguration = Request.request serviceName "getBucketMetricsConfiguration" 


-- | Deprecated, see the GetBucketNotificationConfiguration operation.
getBucketNotification :: forall eff. GetBucketNotificationConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NotificationConfigurationDeprecated
getBucketNotification = Request.request serviceName "getBucketNotification" 


-- | Returns the notification configuration of a bucket.
getBucketNotificationConfiguration :: forall eff. GetBucketNotificationConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NotificationConfiguration
getBucketNotificationConfiguration = Request.request serviceName "getBucketNotificationConfiguration" 


-- | Returns the policy of a specified bucket.
getBucketPolicy :: forall eff. GetBucketPolicyRequest -> Aff (exception :: EXCEPTION | eff) GetBucketPolicyOutput
getBucketPolicy = Request.request serviceName "getBucketPolicy" 


-- | Returns the replication configuration of a bucket.
getBucketReplication :: forall eff. GetBucketReplicationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketReplicationOutput
getBucketReplication = Request.request serviceName "getBucketReplication" 


-- | Returns the request payment configuration of a bucket.
getBucketRequestPayment :: forall eff. GetBucketRequestPaymentRequest -> Aff (exception :: EXCEPTION | eff) GetBucketRequestPaymentOutput
getBucketRequestPayment = Request.request serviceName "getBucketRequestPayment" 


-- | Returns the tag set associated with the bucket.
getBucketTagging :: forall eff. GetBucketTaggingRequest -> Aff (exception :: EXCEPTION | eff) GetBucketTaggingOutput
getBucketTagging = Request.request serviceName "getBucketTagging" 


-- | Returns the versioning state of a bucket.
getBucketVersioning :: forall eff. GetBucketVersioningRequest -> Aff (exception :: EXCEPTION | eff) GetBucketVersioningOutput
getBucketVersioning = Request.request serviceName "getBucketVersioning" 


-- | Returns the website configuration for a bucket.
getBucketWebsite :: forall eff. GetBucketWebsiteRequest -> Aff (exception :: EXCEPTION | eff) GetBucketWebsiteOutput
getBucketWebsite = Request.request serviceName "getBucketWebsite" 


-- | Retrieves objects from Amazon S3.
getObject :: forall eff. GetObjectRequest -> Aff (exception :: EXCEPTION | eff) GetObjectOutput
getObject = Request.request serviceName "getObject" 


-- | Returns the access control list (ACL) of an object.
getObjectAcl :: forall eff. GetObjectAclRequest -> Aff (exception :: EXCEPTION | eff) GetObjectAclOutput
getObjectAcl = Request.request serviceName "getObjectAcl" 


-- | Returns the tag-set of an object.
getObjectTagging :: forall eff. GetObjectTaggingRequest -> Aff (exception :: EXCEPTION | eff) GetObjectTaggingOutput
getObjectTagging = Request.request serviceName "getObjectTagging" 


-- | Return torrent files from a bucket.
getObjectTorrent :: forall eff. GetObjectTorrentRequest -> Aff (exception :: EXCEPTION | eff) GetObjectTorrentOutput
getObjectTorrent = Request.request serviceName "getObjectTorrent" 


-- | This operation is useful to determine if a bucket exists and you have permission to access it.
headBucket :: forall eff. HeadBucketRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
headBucket = Request.request serviceName "headBucket" 


-- | The HEAD operation retrieves metadata from an object without returning the object itself. This operation is useful if you're only interested in an object's metadata. To use HEAD, you must have READ access to the object.
headObject :: forall eff. HeadObjectRequest -> Aff (exception :: EXCEPTION | eff) HeadObjectOutput
headObject = Request.request serviceName "headObject" 


-- | Lists the analytics configurations for the bucket.
listBucketAnalyticsConfigurations :: forall eff. ListBucketAnalyticsConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) ListBucketAnalyticsConfigurationsOutput
listBucketAnalyticsConfigurations = Request.request serviceName "listBucketAnalyticsConfigurations" 


-- | Returns a list of inventory configurations for the bucket.
listBucketInventoryConfigurations :: forall eff. ListBucketInventoryConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) ListBucketInventoryConfigurationsOutput
listBucketInventoryConfigurations = Request.request serviceName "listBucketInventoryConfigurations" 


-- | Lists the metrics configurations for the bucket.
listBucketMetricsConfigurations :: forall eff. ListBucketMetricsConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) ListBucketMetricsConfigurationsOutput
listBucketMetricsConfigurations = Request.request serviceName "listBucketMetricsConfigurations" 


-- | Returns a list of all buckets owned by the authenticated sender of the request.
listBuckets :: forall eff.  Aff (exception :: EXCEPTION | eff) ListBucketsOutput
listBuckets = Request.request serviceName "listBuckets" (Types.NoInput unit)


-- | This operation lists in-progress multipart uploads.
listMultipartUploads :: forall eff. ListMultipartUploadsRequest -> Aff (exception :: EXCEPTION | eff) ListMultipartUploadsOutput
listMultipartUploads = Request.request serviceName "listMultipartUploads" 


-- | Returns metadata about all of the versions of objects in a bucket.
listObjectVersions :: forall eff. ListObjectVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListObjectVersionsOutput
listObjectVersions = Request.request serviceName "listObjectVersions" 


-- | Returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket.
listObjects :: forall eff. ListObjectsRequest -> Aff (exception :: EXCEPTION | eff) ListObjectsOutput
listObjects = Request.request serviceName "listObjects" 


-- | Returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket. Note: ListObjectsV2 is the revised List Objects API and we recommend you use this revised API for new application development.
listObjectsV2 :: forall eff. ListObjectsV2Request -> Aff (exception :: EXCEPTION | eff) ListObjectsV2Output
listObjectsV2 = Request.request serviceName "listObjectsV2" 


-- | Lists the parts that have been uploaded for a specific multipart upload.
listParts :: forall eff. ListPartsRequest -> Aff (exception :: EXCEPTION | eff) ListPartsOutput
listParts = Request.request serviceName "listParts" 


-- | Sets the accelerate configuration of an existing bucket.
putBucketAccelerateConfiguration :: forall eff. PutBucketAccelerateConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketAccelerateConfiguration = Request.request serviceName "putBucketAccelerateConfiguration" 


-- | Sets the permissions on a bucket using access control lists (ACL).
putBucketAcl :: forall eff. PutBucketAclRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketAcl = Request.request serviceName "putBucketAcl" 


-- | Sets an analytics configuration for the bucket (specified by the analytics configuration ID).
putBucketAnalyticsConfiguration :: forall eff. PutBucketAnalyticsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketAnalyticsConfiguration = Request.request serviceName "putBucketAnalyticsConfiguration" 


-- | Sets the cors configuration for a bucket.
putBucketCors :: forall eff. PutBucketCorsRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketCors = Request.request serviceName "putBucketCors" 


-- | Creates a new server-side encryption configuration (or replaces an existing one, if present).
putBucketEncryption :: forall eff. PutBucketEncryptionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketEncryption = Request.request serviceName "putBucketEncryption" 


-- | Adds an inventory configuration (identified by the inventory ID) from the bucket.
putBucketInventoryConfiguration :: forall eff. PutBucketInventoryConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketInventoryConfiguration = Request.request serviceName "putBucketInventoryConfiguration" 


-- | Deprecated, see the PutBucketLifecycleConfiguration operation.
putBucketLifecycle :: forall eff. PutBucketLifecycleRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketLifecycle = Request.request serviceName "putBucketLifecycle" 


-- | Sets lifecycle configuration for your bucket. If a lifecycle configuration exists, it replaces it.
putBucketLifecycleConfiguration :: forall eff. PutBucketLifecycleConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketLifecycleConfiguration = Request.request serviceName "putBucketLifecycleConfiguration" 


-- | Set the logging parameters for a bucket and to specify permissions for who can view and modify the logging parameters. To set the logging status of a bucket, you must be the bucket owner.
putBucketLogging :: forall eff. PutBucketLoggingRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketLogging = Request.request serviceName "putBucketLogging" 


-- | Sets a metrics configuration (specified by the metrics configuration ID) for the bucket.
putBucketMetricsConfiguration :: forall eff. PutBucketMetricsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketMetricsConfiguration = Request.request serviceName "putBucketMetricsConfiguration" 


-- | Deprecated, see the PutBucketNotificationConfiguraiton operation.
putBucketNotification :: forall eff. PutBucketNotificationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketNotification = Request.request serviceName "putBucketNotification" 


-- | Enables notifications of specified events for a bucket.
putBucketNotificationConfiguration :: forall eff. PutBucketNotificationConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketNotificationConfiguration = Request.request serviceName "putBucketNotificationConfiguration" 


-- | Replaces a policy on a bucket. If the bucket already has a policy, the one in this request completely replaces it.
putBucketPolicy :: forall eff. PutBucketPolicyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketPolicy = Request.request serviceName "putBucketPolicy" 


-- | Creates a new replication configuration (or replaces an existing one, if present).
putBucketReplication :: forall eff. PutBucketReplicationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketReplication = Request.request serviceName "putBucketReplication" 


-- | Sets the request payment configuration for a bucket. By default, the bucket owner pays for downloads from the bucket. This configuration parameter enables the bucket owner (only) to specify that the person requesting the download will be charged for the download. Documentation on requester pays buckets can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html
putBucketRequestPayment :: forall eff. PutBucketRequestPaymentRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketRequestPayment = Request.request serviceName "putBucketRequestPayment" 


-- | Sets the tags for a bucket.
putBucketTagging :: forall eff. PutBucketTaggingRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketTagging = Request.request serviceName "putBucketTagging" 


-- | Sets the versioning state of an existing bucket. To set the versioning state, you must be the bucket owner.
putBucketVersioning :: forall eff. PutBucketVersioningRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketVersioning = Request.request serviceName "putBucketVersioning" 


-- | Set the website configuration for a bucket.
putBucketWebsite :: forall eff. PutBucketWebsiteRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putBucketWebsite = Request.request serviceName "putBucketWebsite" 


-- | Adds an object to a bucket.
putObject :: forall eff. PutObjectRequest -> Aff (exception :: EXCEPTION | eff) PutObjectOutput
putObject = Request.request serviceName "putObject" 


-- | uses the acl subresource to set the access control list (ACL) permissions for an object that already exists in a bucket
putObjectAcl :: forall eff. PutObjectAclRequest -> Aff (exception :: EXCEPTION | eff) PutObjectAclOutput
putObjectAcl = Request.request serviceName "putObjectAcl" 


-- | Sets the supplied tag-set to an object that already exists in a bucket
putObjectTagging :: forall eff. PutObjectTaggingRequest -> Aff (exception :: EXCEPTION | eff) PutObjectTaggingOutput
putObjectTagging = Request.request serviceName "putObjectTagging" 


-- | Restores an archived copy of an object back into Amazon S3
restoreObject :: forall eff. RestoreObjectRequest -> Aff (exception :: EXCEPTION | eff) RestoreObjectOutput
restoreObject = Request.request serviceName "restoreObject" 


-- | <p>Uploads a part in a multipart upload.</p><p><b>Note:</b> After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.</p>
uploadPart :: forall eff. UploadPartRequest -> Aff (exception :: EXCEPTION | eff) UploadPartOutput
uploadPart = Request.request serviceName "uploadPart" 


-- | Uploads a part by copying data from an existing object as data source.
uploadPartCopy :: forall eff. UploadPartCopyRequest -> Aff (exception :: EXCEPTION | eff) UploadPartCopyOutput
uploadPartCopy = Request.request serviceName "uploadPartCopy" 


newtype AbortDate = AbortDate Number
derive instance newtypeAbortDate :: Newtype AbortDate _
derive instance repGenericAbortDate :: Generic AbortDate _
instance showAbortDate :: Show AbortDate where
  show = genericShow
instance decodeAbortDate :: Decode AbortDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAbortDate :: Encode AbortDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the days since the initiation of an Incomplete Multipart Upload that Lifecycle will wait before permanently removing all parts of the upload.
newtype AbortIncompleteMultipartUpload = AbortIncompleteMultipartUpload 
  { "DaysAfterInitiation" :: NullOrUndefined.NullOrUndefined (DaysAfterInitiation)
  }
derive instance newtypeAbortIncompleteMultipartUpload :: Newtype AbortIncompleteMultipartUpload _
derive instance repGenericAbortIncompleteMultipartUpload :: Generic AbortIncompleteMultipartUpload _
instance showAbortIncompleteMultipartUpload :: Show AbortIncompleteMultipartUpload where
  show = genericShow
instance decodeAbortIncompleteMultipartUpload :: Decode AbortIncompleteMultipartUpload where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAbortIncompleteMultipartUpload :: Encode AbortIncompleteMultipartUpload where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AbortMultipartUploadOutput = AbortMultipartUploadOutput 
  { "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  }
derive instance newtypeAbortMultipartUploadOutput :: Newtype AbortMultipartUploadOutput _
derive instance repGenericAbortMultipartUploadOutput :: Generic AbortMultipartUploadOutput _
instance showAbortMultipartUploadOutput :: Show AbortMultipartUploadOutput where
  show = genericShow
instance decodeAbortMultipartUploadOutput :: Decode AbortMultipartUploadOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAbortMultipartUploadOutput :: Encode AbortMultipartUploadOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AbortMultipartUploadRequest = AbortMultipartUploadRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "UploadId" :: (MultipartUploadId)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  }
derive instance newtypeAbortMultipartUploadRequest :: Newtype AbortMultipartUploadRequest _
derive instance repGenericAbortMultipartUploadRequest :: Generic AbortMultipartUploadRequest _
instance showAbortMultipartUploadRequest :: Show AbortMultipartUploadRequest where
  show = genericShow
instance decodeAbortMultipartUploadRequest :: Decode AbortMultipartUploadRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAbortMultipartUploadRequest :: Encode AbortMultipartUploadRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AbortRuleId = AbortRuleId String
derive instance newtypeAbortRuleId :: Newtype AbortRuleId _
derive instance repGenericAbortRuleId :: Generic AbortRuleId _
instance showAbortRuleId :: Show AbortRuleId where
  show = genericShow
instance decodeAbortRuleId :: Decode AbortRuleId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAbortRuleId :: Encode AbortRuleId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccelerateConfiguration = AccelerateConfiguration 
  { "Status" :: NullOrUndefined.NullOrUndefined (BucketAccelerateStatus)
  }
derive instance newtypeAccelerateConfiguration :: Newtype AccelerateConfiguration _
derive instance repGenericAccelerateConfiguration :: Generic AccelerateConfiguration _
instance showAccelerateConfiguration :: Show AccelerateConfiguration where
  show = genericShow
instance decodeAccelerateConfiguration :: Decode AccelerateConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccelerateConfiguration :: Encode AccelerateConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AcceptRanges = AcceptRanges String
derive instance newtypeAcceptRanges :: Newtype AcceptRanges _
derive instance repGenericAcceptRanges :: Generic AcceptRanges _
instance showAcceptRanges :: Show AcceptRanges where
  show = genericShow
instance decodeAcceptRanges :: Decode AcceptRanges where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAcceptRanges :: Encode AcceptRanges where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccessControlPolicy = AccessControlPolicy 
  { "Grants" :: NullOrUndefined.NullOrUndefined (Grants)
  , "Owner" :: NullOrUndefined.NullOrUndefined (Owner)
  }
derive instance newtypeAccessControlPolicy :: Newtype AccessControlPolicy _
derive instance repGenericAccessControlPolicy :: Generic AccessControlPolicy _
instance showAccessControlPolicy :: Show AccessControlPolicy where
  show = genericShow
instance decodeAccessControlPolicy :: Decode AccessControlPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessControlPolicy :: Encode AccessControlPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for information regarding the access control for replicas.
newtype AccessControlTranslation = AccessControlTranslation 
  { "Owner" :: (OwnerOverride)
  }
derive instance newtypeAccessControlTranslation :: Newtype AccessControlTranslation _
derive instance repGenericAccessControlTranslation :: Generic AccessControlTranslation _
instance showAccessControlTranslation :: Show AccessControlTranslation where
  show = genericShow
instance decodeAccessControlTranslation :: Decode AccessControlTranslation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessControlTranslation :: Encode AccessControlTranslation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _
derive instance repGenericAccountId :: Generic AccountId _
instance showAccountId :: Show AccountId where
  show = genericShow
instance decodeAccountId :: Decode AccountId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountId :: Encode AccountId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllowedHeader = AllowedHeader String
derive instance newtypeAllowedHeader :: Newtype AllowedHeader _
derive instance repGenericAllowedHeader :: Generic AllowedHeader _
instance showAllowedHeader :: Show AllowedHeader where
  show = genericShow
instance decodeAllowedHeader :: Decode AllowedHeader where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedHeader :: Encode AllowedHeader where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllowedHeaders = AllowedHeaders (Array AllowedHeader)
derive instance newtypeAllowedHeaders :: Newtype AllowedHeaders _
derive instance repGenericAllowedHeaders :: Generic AllowedHeaders _
instance showAllowedHeaders :: Show AllowedHeaders where
  show = genericShow
instance decodeAllowedHeaders :: Decode AllowedHeaders where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedHeaders :: Encode AllowedHeaders where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllowedMethod = AllowedMethod String
derive instance newtypeAllowedMethod :: Newtype AllowedMethod _
derive instance repGenericAllowedMethod :: Generic AllowedMethod _
instance showAllowedMethod :: Show AllowedMethod where
  show = genericShow
instance decodeAllowedMethod :: Decode AllowedMethod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedMethod :: Encode AllowedMethod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllowedMethods = AllowedMethods (Array AllowedMethod)
derive instance newtypeAllowedMethods :: Newtype AllowedMethods _
derive instance repGenericAllowedMethods :: Generic AllowedMethods _
instance showAllowedMethods :: Show AllowedMethods where
  show = genericShow
instance decodeAllowedMethods :: Decode AllowedMethods where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedMethods :: Encode AllowedMethods where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllowedOrigin = AllowedOrigin String
derive instance newtypeAllowedOrigin :: Newtype AllowedOrigin _
derive instance repGenericAllowedOrigin :: Generic AllowedOrigin _
instance showAllowedOrigin :: Show AllowedOrigin where
  show = genericShow
instance decodeAllowedOrigin :: Decode AllowedOrigin where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedOrigin :: Encode AllowedOrigin where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllowedOrigins = AllowedOrigins (Array AllowedOrigin)
derive instance newtypeAllowedOrigins :: Newtype AllowedOrigins _
derive instance repGenericAllowedOrigins :: Generic AllowedOrigins _
instance showAllowedOrigins :: Show AllowedOrigins where
  show = genericShow
instance decodeAllowedOrigins :: Decode AllowedOrigins where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedOrigins :: Encode AllowedOrigins where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AnalyticsAndOperator = AnalyticsAndOperator 
  { "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagSet)
  }
derive instance newtypeAnalyticsAndOperator :: Newtype AnalyticsAndOperator _
derive instance repGenericAnalyticsAndOperator :: Generic AnalyticsAndOperator _
instance showAnalyticsAndOperator :: Show AnalyticsAndOperator where
  show = genericShow
instance decodeAnalyticsAndOperator :: Decode AnalyticsAndOperator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnalyticsAndOperator :: Encode AnalyticsAndOperator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AnalyticsConfiguration = AnalyticsConfiguration 
  { "Id" :: (AnalyticsId)
  , "Filter" :: NullOrUndefined.NullOrUndefined (AnalyticsFilter)
  , "StorageClassAnalysis" :: (StorageClassAnalysis)
  }
derive instance newtypeAnalyticsConfiguration :: Newtype AnalyticsConfiguration _
derive instance repGenericAnalyticsConfiguration :: Generic AnalyticsConfiguration _
instance showAnalyticsConfiguration :: Show AnalyticsConfiguration where
  show = genericShow
instance decodeAnalyticsConfiguration :: Decode AnalyticsConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnalyticsConfiguration :: Encode AnalyticsConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AnalyticsConfigurationList = AnalyticsConfigurationList (Array AnalyticsConfiguration)
derive instance newtypeAnalyticsConfigurationList :: Newtype AnalyticsConfigurationList _
derive instance repGenericAnalyticsConfigurationList :: Generic AnalyticsConfigurationList _
instance showAnalyticsConfigurationList :: Show AnalyticsConfigurationList where
  show = genericShow
instance decodeAnalyticsConfigurationList :: Decode AnalyticsConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnalyticsConfigurationList :: Encode AnalyticsConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AnalyticsExportDestination = AnalyticsExportDestination 
  { "S3BucketDestination" :: (AnalyticsS3BucketDestination)
  }
derive instance newtypeAnalyticsExportDestination :: Newtype AnalyticsExportDestination _
derive instance repGenericAnalyticsExportDestination :: Generic AnalyticsExportDestination _
instance showAnalyticsExportDestination :: Show AnalyticsExportDestination where
  show = genericShow
instance decodeAnalyticsExportDestination :: Decode AnalyticsExportDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnalyticsExportDestination :: Encode AnalyticsExportDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AnalyticsFilter = AnalyticsFilter 
  { "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "Tag" :: NullOrUndefined.NullOrUndefined (Tag)
  , "And" :: NullOrUndefined.NullOrUndefined (AnalyticsAndOperator)
  }
derive instance newtypeAnalyticsFilter :: Newtype AnalyticsFilter _
derive instance repGenericAnalyticsFilter :: Generic AnalyticsFilter _
instance showAnalyticsFilter :: Show AnalyticsFilter where
  show = genericShow
instance decodeAnalyticsFilter :: Decode AnalyticsFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnalyticsFilter :: Encode AnalyticsFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AnalyticsId = AnalyticsId String
derive instance newtypeAnalyticsId :: Newtype AnalyticsId _
derive instance repGenericAnalyticsId :: Generic AnalyticsId _
instance showAnalyticsId :: Show AnalyticsId where
  show = genericShow
instance decodeAnalyticsId :: Decode AnalyticsId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnalyticsId :: Encode AnalyticsId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AnalyticsS3BucketDestination = AnalyticsS3BucketDestination 
  { "Format" :: (AnalyticsS3ExportFileFormat)
  , "BucketAccountId" :: NullOrUndefined.NullOrUndefined (AccountId)
  , "Bucket" :: (BucketName)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  }
derive instance newtypeAnalyticsS3BucketDestination :: Newtype AnalyticsS3BucketDestination _
derive instance repGenericAnalyticsS3BucketDestination :: Generic AnalyticsS3BucketDestination _
instance showAnalyticsS3BucketDestination :: Show AnalyticsS3BucketDestination where
  show = genericShow
instance decodeAnalyticsS3BucketDestination :: Decode AnalyticsS3BucketDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnalyticsS3BucketDestination :: Encode AnalyticsS3BucketDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AnalyticsS3ExportFileFormat = AnalyticsS3ExportFileFormat String
derive instance newtypeAnalyticsS3ExportFileFormat :: Newtype AnalyticsS3ExportFileFormat _
derive instance repGenericAnalyticsS3ExportFileFormat :: Generic AnalyticsS3ExportFileFormat _
instance showAnalyticsS3ExportFileFormat :: Show AnalyticsS3ExportFileFormat where
  show = genericShow
instance decodeAnalyticsS3ExportFileFormat :: Decode AnalyticsS3ExportFileFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnalyticsS3ExportFileFormat :: Encode AnalyticsS3ExportFileFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Body = Body String
derive instance newtypeBody :: Newtype Body _
derive instance repGenericBody :: Generic Body _
instance showBody :: Show Body where
  show = genericShow
instance decodeBody :: Decode Body where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBody :: Encode Body where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Bucket = Bucket 
  { "Name" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (CreationDate)
  }
derive instance newtypeBucket :: Newtype Bucket _
derive instance repGenericBucket :: Generic Bucket _
instance showBucket :: Show Bucket where
  show = genericShow
instance decodeBucket :: Decode Bucket where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucket :: Encode Bucket where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketAccelerateStatus = BucketAccelerateStatus String
derive instance newtypeBucketAccelerateStatus :: Newtype BucketAccelerateStatus _
derive instance repGenericBucketAccelerateStatus :: Generic BucketAccelerateStatus _
instance showBucketAccelerateStatus :: Show BucketAccelerateStatus where
  show = genericShow
instance decodeBucketAccelerateStatus :: Decode BucketAccelerateStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketAccelerateStatus :: Encode BucketAccelerateStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The requested bucket name is not available. The bucket namespace is shared by all users of the system. Please select a different name and try again.
newtype BucketAlreadyExists = BucketAlreadyExists Types.NoArguments
derive instance newtypeBucketAlreadyExists :: Newtype BucketAlreadyExists _
derive instance repGenericBucketAlreadyExists :: Generic BucketAlreadyExists _
instance showBucketAlreadyExists :: Show BucketAlreadyExists where
  show = genericShow
instance decodeBucketAlreadyExists :: Decode BucketAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketAlreadyExists :: Encode BucketAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketAlreadyOwnedByYou = BucketAlreadyOwnedByYou Types.NoArguments
derive instance newtypeBucketAlreadyOwnedByYou :: Newtype BucketAlreadyOwnedByYou _
derive instance repGenericBucketAlreadyOwnedByYou :: Generic BucketAlreadyOwnedByYou _
instance showBucketAlreadyOwnedByYou :: Show BucketAlreadyOwnedByYou where
  show = genericShow
instance decodeBucketAlreadyOwnedByYou :: Decode BucketAlreadyOwnedByYou where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketAlreadyOwnedByYou :: Encode BucketAlreadyOwnedByYou where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketCannedACL = BucketCannedACL String
derive instance newtypeBucketCannedACL :: Newtype BucketCannedACL _
derive instance repGenericBucketCannedACL :: Generic BucketCannedACL _
instance showBucketCannedACL :: Show BucketCannedACL where
  show = genericShow
instance decodeBucketCannedACL :: Decode BucketCannedACL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketCannedACL :: Encode BucketCannedACL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketLifecycleConfiguration = BucketLifecycleConfiguration 
  { "Rules" :: (LifecycleRules)
  }
derive instance newtypeBucketLifecycleConfiguration :: Newtype BucketLifecycleConfiguration _
derive instance repGenericBucketLifecycleConfiguration :: Generic BucketLifecycleConfiguration _
instance showBucketLifecycleConfiguration :: Show BucketLifecycleConfiguration where
  show = genericShow
instance decodeBucketLifecycleConfiguration :: Decode BucketLifecycleConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketLifecycleConfiguration :: Encode BucketLifecycleConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketLocationConstraint = BucketLocationConstraint String
derive instance newtypeBucketLocationConstraint :: Newtype BucketLocationConstraint _
derive instance repGenericBucketLocationConstraint :: Generic BucketLocationConstraint _
instance showBucketLocationConstraint :: Show BucketLocationConstraint where
  show = genericShow
instance decodeBucketLocationConstraint :: Decode BucketLocationConstraint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketLocationConstraint :: Encode BucketLocationConstraint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketLoggingStatus = BucketLoggingStatus 
  { "LoggingEnabled" :: NullOrUndefined.NullOrUndefined (LoggingEnabled)
  }
derive instance newtypeBucketLoggingStatus :: Newtype BucketLoggingStatus _
derive instance repGenericBucketLoggingStatus :: Generic BucketLoggingStatus _
instance showBucketLoggingStatus :: Show BucketLoggingStatus where
  show = genericShow
instance decodeBucketLoggingStatus :: Decode BucketLoggingStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketLoggingStatus :: Encode BucketLoggingStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketLogsPermission = BucketLogsPermission String
derive instance newtypeBucketLogsPermission :: Newtype BucketLogsPermission _
derive instance repGenericBucketLogsPermission :: Generic BucketLogsPermission _
instance showBucketLogsPermission :: Show BucketLogsPermission where
  show = genericShow
instance decodeBucketLogsPermission :: Decode BucketLogsPermission where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketLogsPermission :: Encode BucketLogsPermission where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketName = BucketName String
derive instance newtypeBucketName :: Newtype BucketName _
derive instance repGenericBucketName :: Generic BucketName _
instance showBucketName :: Show BucketName where
  show = genericShow
instance decodeBucketName :: Decode BucketName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketName :: Encode BucketName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketVersioningStatus = BucketVersioningStatus String
derive instance newtypeBucketVersioningStatus :: Newtype BucketVersioningStatus _
derive instance repGenericBucketVersioningStatus :: Generic BucketVersioningStatus _
instance showBucketVersioningStatus :: Show BucketVersioningStatus where
  show = genericShow
instance decodeBucketVersioningStatus :: Decode BucketVersioningStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketVersioningStatus :: Encode BucketVersioningStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Buckets = Buckets (Array Bucket)
derive instance newtypeBuckets :: Newtype Buckets _
derive instance repGenericBuckets :: Generic Buckets _
instance showBuckets :: Show Buckets where
  show = genericShow
instance decodeBuckets :: Decode Buckets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuckets :: Encode Buckets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CORSConfiguration = CORSConfiguration 
  { "CORSRules" :: (CORSRules)
  }
derive instance newtypeCORSConfiguration :: Newtype CORSConfiguration _
derive instance repGenericCORSConfiguration :: Generic CORSConfiguration _
instance showCORSConfiguration :: Show CORSConfiguration where
  show = genericShow
instance decodeCORSConfiguration :: Decode CORSConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCORSConfiguration :: Encode CORSConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CORSRule = CORSRule 
  { "AllowedHeaders" :: NullOrUndefined.NullOrUndefined (AllowedHeaders)
  , "AllowedMethods" :: (AllowedMethods)
  , "AllowedOrigins" :: (AllowedOrigins)
  , "ExposeHeaders" :: NullOrUndefined.NullOrUndefined (ExposeHeaders)
  , "MaxAgeSeconds" :: NullOrUndefined.NullOrUndefined (MaxAgeSeconds)
  }
derive instance newtypeCORSRule :: Newtype CORSRule _
derive instance repGenericCORSRule :: Generic CORSRule _
instance showCORSRule :: Show CORSRule where
  show = genericShow
instance decodeCORSRule :: Decode CORSRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCORSRule :: Encode CORSRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CORSRules = CORSRules (Array CORSRule)
derive instance newtypeCORSRules :: Newtype CORSRules _
derive instance repGenericCORSRules :: Generic CORSRules _
instance showCORSRules :: Show CORSRules where
  show = genericShow
instance decodeCORSRules :: Decode CORSRules where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCORSRules :: Encode CORSRules where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Describes how a CSV-formatted input object is formatted.
newtype CSVInput = CSVInput 
  { "FileHeaderInfo" :: NullOrUndefined.NullOrUndefined (FileHeaderInfo)
  , "Comments" :: NullOrUndefined.NullOrUndefined (Comments)
  , "QuoteEscapeCharacter" :: NullOrUndefined.NullOrUndefined (QuoteEscapeCharacter)
  , "RecordDelimiter" :: NullOrUndefined.NullOrUndefined (RecordDelimiter)
  , "FieldDelimiter" :: NullOrUndefined.NullOrUndefined (FieldDelimiter)
  , "QuoteCharacter" :: NullOrUndefined.NullOrUndefined (QuoteCharacter)
  }
derive instance newtypeCSVInput :: Newtype CSVInput _
derive instance repGenericCSVInput :: Generic CSVInput _
instance showCSVInput :: Show CSVInput where
  show = genericShow
instance decodeCSVInput :: Decode CSVInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCSVInput :: Encode CSVInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Describes how CSV-formatted results are formatted.
newtype CSVOutput = CSVOutput 
  { "QuoteFields" :: NullOrUndefined.NullOrUndefined (QuoteFields)
  , "QuoteEscapeCharacter" :: NullOrUndefined.NullOrUndefined (QuoteEscapeCharacter)
  , "RecordDelimiter" :: NullOrUndefined.NullOrUndefined (RecordDelimiter)
  , "FieldDelimiter" :: NullOrUndefined.NullOrUndefined (FieldDelimiter)
  , "QuoteCharacter" :: NullOrUndefined.NullOrUndefined (QuoteCharacter)
  }
derive instance newtypeCSVOutput :: Newtype CSVOutput _
derive instance repGenericCSVOutput :: Generic CSVOutput _
instance showCSVOutput :: Show CSVOutput where
  show = genericShow
instance decodeCSVOutput :: Decode CSVOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCSVOutput :: Encode CSVOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheControl = CacheControl String
derive instance newtypeCacheControl :: Newtype CacheControl _
derive instance repGenericCacheControl :: Generic CacheControl _
instance showCacheControl :: Show CacheControl where
  show = genericShow
instance decodeCacheControl :: Decode CacheControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheControl :: Encode CacheControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudFunction = CloudFunction String
derive instance newtypeCloudFunction :: Newtype CloudFunction _
derive instance repGenericCloudFunction :: Generic CloudFunction _
instance showCloudFunction :: Show CloudFunction where
  show = genericShow
instance decodeCloudFunction :: Decode CloudFunction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudFunction :: Encode CloudFunction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudFunctionConfiguration = CloudFunctionConfiguration 
  { "Id" :: NullOrUndefined.NullOrUndefined (NotificationId)
  , "Event" :: NullOrUndefined.NullOrUndefined (Event)
  , "Events" :: NullOrUndefined.NullOrUndefined (EventList)
  , "CloudFunction" :: NullOrUndefined.NullOrUndefined (CloudFunction)
  , "InvocationRole" :: NullOrUndefined.NullOrUndefined (CloudFunctionInvocationRole)
  }
derive instance newtypeCloudFunctionConfiguration :: Newtype CloudFunctionConfiguration _
derive instance repGenericCloudFunctionConfiguration :: Generic CloudFunctionConfiguration _
instance showCloudFunctionConfiguration :: Show CloudFunctionConfiguration where
  show = genericShow
instance decodeCloudFunctionConfiguration :: Decode CloudFunctionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudFunctionConfiguration :: Encode CloudFunctionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudFunctionInvocationRole = CloudFunctionInvocationRole String
derive instance newtypeCloudFunctionInvocationRole :: Newtype CloudFunctionInvocationRole _
derive instance repGenericCloudFunctionInvocationRole :: Generic CloudFunctionInvocationRole _
instance showCloudFunctionInvocationRole :: Show CloudFunctionInvocationRole where
  show = genericShow
instance decodeCloudFunctionInvocationRole :: Decode CloudFunctionInvocationRole where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudFunctionInvocationRole :: Encode CloudFunctionInvocationRole where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Code = Code String
derive instance newtypeCode :: Newtype Code _
derive instance repGenericCode :: Generic Code _
instance showCode :: Show Code where
  show = genericShow
instance decodeCode :: Decode Code where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCode :: Encode Code where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Comments = Comments String
derive instance newtypeComments :: Newtype Comments _
derive instance repGenericComments :: Generic Comments _
instance showComments :: Show Comments where
  show = genericShow
instance decodeComments :: Decode Comments where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComments :: Encode Comments where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommonPrefix = CommonPrefix 
  { "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  }
derive instance newtypeCommonPrefix :: Newtype CommonPrefix _
derive instance repGenericCommonPrefix :: Generic CommonPrefix _
instance showCommonPrefix :: Show CommonPrefix where
  show = genericShow
instance decodeCommonPrefix :: Decode CommonPrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommonPrefix :: Encode CommonPrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommonPrefixList = CommonPrefixList (Array CommonPrefix)
derive instance newtypeCommonPrefixList :: Newtype CommonPrefixList _
derive instance repGenericCommonPrefixList :: Generic CommonPrefixList _
instance showCommonPrefixList :: Show CommonPrefixList where
  show = genericShow
instance decodeCommonPrefixList :: Decode CommonPrefixList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommonPrefixList :: Encode CommonPrefixList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompleteMultipartUploadOutput = CompleteMultipartUploadOutput 
  { "Location" :: NullOrUndefined.NullOrUndefined (Location)
  , "Bucket" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "Key" :: NullOrUndefined.NullOrUndefined (ObjectKey)
  , "Expiration" :: NullOrUndefined.NullOrUndefined (Expiration)
  , "ETag" :: NullOrUndefined.NullOrUndefined (ETag)
  , "ServerSideEncryption" :: NullOrUndefined.NullOrUndefined (ServerSideEncryption)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "SSEKMSKeyId" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  }
derive instance newtypeCompleteMultipartUploadOutput :: Newtype CompleteMultipartUploadOutput _
derive instance repGenericCompleteMultipartUploadOutput :: Generic CompleteMultipartUploadOutput _
instance showCompleteMultipartUploadOutput :: Show CompleteMultipartUploadOutput where
  show = genericShow
instance decodeCompleteMultipartUploadOutput :: Decode CompleteMultipartUploadOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompleteMultipartUploadOutput :: Encode CompleteMultipartUploadOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompleteMultipartUploadRequest = CompleteMultipartUploadRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "MultipartUpload" :: NullOrUndefined.NullOrUndefined (CompletedMultipartUpload)
  , "UploadId" :: (MultipartUploadId)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  }
derive instance newtypeCompleteMultipartUploadRequest :: Newtype CompleteMultipartUploadRequest _
derive instance repGenericCompleteMultipartUploadRequest :: Generic CompleteMultipartUploadRequest _
instance showCompleteMultipartUploadRequest :: Show CompleteMultipartUploadRequest where
  show = genericShow
instance decodeCompleteMultipartUploadRequest :: Decode CompleteMultipartUploadRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompleteMultipartUploadRequest :: Encode CompleteMultipartUploadRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompletedMultipartUpload = CompletedMultipartUpload 
  { "Parts" :: NullOrUndefined.NullOrUndefined (CompletedPartList)
  }
derive instance newtypeCompletedMultipartUpload :: Newtype CompletedMultipartUpload _
derive instance repGenericCompletedMultipartUpload :: Generic CompletedMultipartUpload _
instance showCompletedMultipartUpload :: Show CompletedMultipartUpload where
  show = genericShow
instance decodeCompletedMultipartUpload :: Decode CompletedMultipartUpload where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompletedMultipartUpload :: Encode CompletedMultipartUpload where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompletedPart = CompletedPart 
  { "ETag" :: NullOrUndefined.NullOrUndefined (ETag)
  , "PartNumber" :: NullOrUndefined.NullOrUndefined (PartNumber)
  }
derive instance newtypeCompletedPart :: Newtype CompletedPart _
derive instance repGenericCompletedPart :: Generic CompletedPart _
instance showCompletedPart :: Show CompletedPart where
  show = genericShow
instance decodeCompletedPart :: Decode CompletedPart where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompletedPart :: Encode CompletedPart where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompletedPartList = CompletedPartList (Array CompletedPart)
derive instance newtypeCompletedPartList :: Newtype CompletedPartList _
derive instance repGenericCompletedPartList :: Generic CompletedPartList _
instance showCompletedPartList :: Show CompletedPartList where
  show = genericShow
instance decodeCompletedPartList :: Decode CompletedPartList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompletedPartList :: Encode CompletedPartList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Condition = Condition 
  { "HttpErrorCodeReturnedEquals" :: NullOrUndefined.NullOrUndefined (HttpErrorCodeReturnedEquals)
  , "KeyPrefixEquals" :: NullOrUndefined.NullOrUndefined (KeyPrefixEquals)
  }
derive instance newtypeCondition :: Newtype Condition _
derive instance repGenericCondition :: Generic Condition _
instance showCondition :: Show Condition where
  show = genericShow
instance decodeCondition :: Decode Condition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCondition :: Encode Condition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfirmRemoveSelfBucketAccess = ConfirmRemoveSelfBucketAccess Boolean
derive instance newtypeConfirmRemoveSelfBucketAccess :: Newtype ConfirmRemoveSelfBucketAccess _
derive instance repGenericConfirmRemoveSelfBucketAccess :: Generic ConfirmRemoveSelfBucketAccess _
instance showConfirmRemoveSelfBucketAccess :: Show ConfirmRemoveSelfBucketAccess where
  show = genericShow
instance decodeConfirmRemoveSelfBucketAccess :: Decode ConfirmRemoveSelfBucketAccess where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmRemoveSelfBucketAccess :: Encode ConfirmRemoveSelfBucketAccess where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentDisposition = ContentDisposition String
derive instance newtypeContentDisposition :: Newtype ContentDisposition _
derive instance repGenericContentDisposition :: Generic ContentDisposition _
instance showContentDisposition :: Show ContentDisposition where
  show = genericShow
instance decodeContentDisposition :: Decode ContentDisposition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentDisposition :: Encode ContentDisposition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentEncoding = ContentEncoding String
derive instance newtypeContentEncoding :: Newtype ContentEncoding _
derive instance repGenericContentEncoding :: Generic ContentEncoding _
instance showContentEncoding :: Show ContentEncoding where
  show = genericShow
instance decodeContentEncoding :: Decode ContentEncoding where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentEncoding :: Encode ContentEncoding where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentLanguage = ContentLanguage String
derive instance newtypeContentLanguage :: Newtype ContentLanguage _
derive instance repGenericContentLanguage :: Generic ContentLanguage _
instance showContentLanguage :: Show ContentLanguage where
  show = genericShow
instance decodeContentLanguage :: Decode ContentLanguage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentLanguage :: Encode ContentLanguage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentLength = ContentLength Number
derive instance newtypeContentLength :: Newtype ContentLength _
derive instance repGenericContentLength :: Generic ContentLength _
instance showContentLength :: Show ContentLength where
  show = genericShow
instance decodeContentLength :: Decode ContentLength where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentLength :: Encode ContentLength where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentMD5 = ContentMD5 String
derive instance newtypeContentMD5 :: Newtype ContentMD5 _
derive instance repGenericContentMD5 :: Generic ContentMD5 _
instance showContentMD5 :: Show ContentMD5 where
  show = genericShow
instance decodeContentMD5 :: Decode ContentMD5 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentMD5 :: Encode ContentMD5 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentRange = ContentRange String
derive instance newtypeContentRange :: Newtype ContentRange _
derive instance repGenericContentRange :: Generic ContentRange _
instance showContentRange :: Show ContentRange where
  show = genericShow
instance decodeContentRange :: Decode ContentRange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentRange :: Encode ContentRange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentType = ContentType String
derive instance newtypeContentType :: Newtype ContentType _
derive instance repGenericContentType :: Generic ContentType _
instance showContentType :: Show ContentType where
  show = genericShow
instance decodeContentType :: Decode ContentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentType :: Encode ContentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopyObjectOutput = CopyObjectOutput 
  { "CopyObjectResult" :: NullOrUndefined.NullOrUndefined (CopyObjectResult)
  , "Expiration" :: NullOrUndefined.NullOrUndefined (Expiration)
  , "CopySourceVersionId" :: NullOrUndefined.NullOrUndefined (CopySourceVersionId)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "ServerSideEncryption" :: NullOrUndefined.NullOrUndefined (ServerSideEncryption)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  }
derive instance newtypeCopyObjectOutput :: Newtype CopyObjectOutput _
derive instance repGenericCopyObjectOutput :: Generic CopyObjectOutput _
instance showCopyObjectOutput :: Show CopyObjectOutput where
  show = genericShow
instance decodeCopyObjectOutput :: Decode CopyObjectOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopyObjectOutput :: Encode CopyObjectOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopyObjectRequest = CopyObjectRequest 
  { "ACL" :: NullOrUndefined.NullOrUndefined (ObjectCannedACL)
  , "Bucket" :: (BucketName)
  , "CacheControl" :: NullOrUndefined.NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined.NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined.NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined.NullOrUndefined (ContentLanguage)
  , "ContentType" :: NullOrUndefined.NullOrUndefined (ContentType)
  , "CopySource" :: (CopySource)
  , "CopySourceIfMatch" :: NullOrUndefined.NullOrUndefined (CopySourceIfMatch)
  , "CopySourceIfModifiedSince" :: NullOrUndefined.NullOrUndefined (CopySourceIfModifiedSince)
  , "CopySourceIfNoneMatch" :: NullOrUndefined.NullOrUndefined (CopySourceIfNoneMatch)
  , "CopySourceIfUnmodifiedSince" :: NullOrUndefined.NullOrUndefined (CopySourceIfUnmodifiedSince)
  , "Expires" :: NullOrUndefined.NullOrUndefined (Expires)
  , "GrantFullControl" :: NullOrUndefined.NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined.NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined.NullOrUndefined (GrantReadACP)
  , "GrantWriteACP" :: NullOrUndefined.NullOrUndefined (GrantWriteACP)
  , "Key" :: (ObjectKey)
  , "Metadata" :: NullOrUndefined.NullOrUndefined (Metadata)
  , "MetadataDirective" :: NullOrUndefined.NullOrUndefined (MetadataDirective)
  , "TaggingDirective" :: NullOrUndefined.NullOrUndefined (TaggingDirective)
  , "ServerSideEncryption" :: NullOrUndefined.NullOrUndefined (ServerSideEncryption)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  , "WebsiteRedirectLocation" :: NullOrUndefined.NullOrUndefined (WebsiteRedirectLocation)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined.NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  , "CopySourceSSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (CopySourceSSECustomerAlgorithm)
  , "CopySourceSSECustomerKey" :: NullOrUndefined.NullOrUndefined (CopySourceSSECustomerKey)
  , "CopySourceSSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (CopySourceSSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  , "Tagging" :: NullOrUndefined.NullOrUndefined (TaggingHeader)
  }
derive instance newtypeCopyObjectRequest :: Newtype CopyObjectRequest _
derive instance repGenericCopyObjectRequest :: Generic CopyObjectRequest _
instance showCopyObjectRequest :: Show CopyObjectRequest where
  show = genericShow
instance decodeCopyObjectRequest :: Decode CopyObjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopyObjectRequest :: Encode CopyObjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopyObjectResult = CopyObjectResult 
  { "ETag" :: NullOrUndefined.NullOrUndefined (ETag)
  , "LastModified" :: NullOrUndefined.NullOrUndefined (LastModified)
  }
derive instance newtypeCopyObjectResult :: Newtype CopyObjectResult _
derive instance repGenericCopyObjectResult :: Generic CopyObjectResult _
instance showCopyObjectResult :: Show CopyObjectResult where
  show = genericShow
instance decodeCopyObjectResult :: Decode CopyObjectResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopyObjectResult :: Encode CopyObjectResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopyPartResult = CopyPartResult 
  { "ETag" :: NullOrUndefined.NullOrUndefined (ETag)
  , "LastModified" :: NullOrUndefined.NullOrUndefined (LastModified)
  }
derive instance newtypeCopyPartResult :: Newtype CopyPartResult _
derive instance repGenericCopyPartResult :: Generic CopyPartResult _
instance showCopyPartResult :: Show CopyPartResult where
  show = genericShow
instance decodeCopyPartResult :: Decode CopyPartResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopyPartResult :: Encode CopyPartResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopySource = CopySource String
derive instance newtypeCopySource :: Newtype CopySource _
derive instance repGenericCopySource :: Generic CopySource _
instance showCopySource :: Show CopySource where
  show = genericShow
instance decodeCopySource :: Decode CopySource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopySource :: Encode CopySource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopySourceIfMatch = CopySourceIfMatch String
derive instance newtypeCopySourceIfMatch :: Newtype CopySourceIfMatch _
derive instance repGenericCopySourceIfMatch :: Generic CopySourceIfMatch _
instance showCopySourceIfMatch :: Show CopySourceIfMatch where
  show = genericShow
instance decodeCopySourceIfMatch :: Decode CopySourceIfMatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopySourceIfMatch :: Encode CopySourceIfMatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopySourceIfModifiedSince = CopySourceIfModifiedSince Number
derive instance newtypeCopySourceIfModifiedSince :: Newtype CopySourceIfModifiedSince _
derive instance repGenericCopySourceIfModifiedSince :: Generic CopySourceIfModifiedSince _
instance showCopySourceIfModifiedSince :: Show CopySourceIfModifiedSince where
  show = genericShow
instance decodeCopySourceIfModifiedSince :: Decode CopySourceIfModifiedSince where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopySourceIfModifiedSince :: Encode CopySourceIfModifiedSince where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopySourceIfNoneMatch = CopySourceIfNoneMatch String
derive instance newtypeCopySourceIfNoneMatch :: Newtype CopySourceIfNoneMatch _
derive instance repGenericCopySourceIfNoneMatch :: Generic CopySourceIfNoneMatch _
instance showCopySourceIfNoneMatch :: Show CopySourceIfNoneMatch where
  show = genericShow
instance decodeCopySourceIfNoneMatch :: Decode CopySourceIfNoneMatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopySourceIfNoneMatch :: Encode CopySourceIfNoneMatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopySourceIfUnmodifiedSince = CopySourceIfUnmodifiedSince Number
derive instance newtypeCopySourceIfUnmodifiedSince :: Newtype CopySourceIfUnmodifiedSince _
derive instance repGenericCopySourceIfUnmodifiedSince :: Generic CopySourceIfUnmodifiedSince _
instance showCopySourceIfUnmodifiedSince :: Show CopySourceIfUnmodifiedSince where
  show = genericShow
instance decodeCopySourceIfUnmodifiedSince :: Decode CopySourceIfUnmodifiedSince where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopySourceIfUnmodifiedSince :: Encode CopySourceIfUnmodifiedSince where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopySourceRange = CopySourceRange String
derive instance newtypeCopySourceRange :: Newtype CopySourceRange _
derive instance repGenericCopySourceRange :: Generic CopySourceRange _
instance showCopySourceRange :: Show CopySourceRange where
  show = genericShow
instance decodeCopySourceRange :: Decode CopySourceRange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopySourceRange :: Encode CopySourceRange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopySourceSSECustomerAlgorithm = CopySourceSSECustomerAlgorithm String
derive instance newtypeCopySourceSSECustomerAlgorithm :: Newtype CopySourceSSECustomerAlgorithm _
derive instance repGenericCopySourceSSECustomerAlgorithm :: Generic CopySourceSSECustomerAlgorithm _
instance showCopySourceSSECustomerAlgorithm :: Show CopySourceSSECustomerAlgorithm where
  show = genericShow
instance decodeCopySourceSSECustomerAlgorithm :: Decode CopySourceSSECustomerAlgorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopySourceSSECustomerAlgorithm :: Encode CopySourceSSECustomerAlgorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopySourceSSECustomerKey = CopySourceSSECustomerKey String
derive instance newtypeCopySourceSSECustomerKey :: Newtype CopySourceSSECustomerKey _
derive instance repGenericCopySourceSSECustomerKey :: Generic CopySourceSSECustomerKey _
instance showCopySourceSSECustomerKey :: Show CopySourceSSECustomerKey where
  show = genericShow
instance decodeCopySourceSSECustomerKey :: Decode CopySourceSSECustomerKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopySourceSSECustomerKey :: Encode CopySourceSSECustomerKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopySourceSSECustomerKeyMD5 = CopySourceSSECustomerKeyMD5 String
derive instance newtypeCopySourceSSECustomerKeyMD5 :: Newtype CopySourceSSECustomerKeyMD5 _
derive instance repGenericCopySourceSSECustomerKeyMD5 :: Generic CopySourceSSECustomerKeyMD5 _
instance showCopySourceSSECustomerKeyMD5 :: Show CopySourceSSECustomerKeyMD5 where
  show = genericShow
instance decodeCopySourceSSECustomerKeyMD5 :: Decode CopySourceSSECustomerKeyMD5 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopySourceSSECustomerKeyMD5 :: Encode CopySourceSSECustomerKeyMD5 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopySourceVersionId = CopySourceVersionId String
derive instance newtypeCopySourceVersionId :: Newtype CopySourceVersionId _
derive instance repGenericCopySourceVersionId :: Generic CopySourceVersionId _
instance showCopySourceVersionId :: Show CopySourceVersionId where
  show = genericShow
instance decodeCopySourceVersionId :: Decode CopySourceVersionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopySourceVersionId :: Encode CopySourceVersionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateBucketConfiguration = CreateBucketConfiguration 
  { "LocationConstraint" :: NullOrUndefined.NullOrUndefined (BucketLocationConstraint)
  }
derive instance newtypeCreateBucketConfiguration :: Newtype CreateBucketConfiguration _
derive instance repGenericCreateBucketConfiguration :: Generic CreateBucketConfiguration _
instance showCreateBucketConfiguration :: Show CreateBucketConfiguration where
  show = genericShow
instance decodeCreateBucketConfiguration :: Decode CreateBucketConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBucketConfiguration :: Encode CreateBucketConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateBucketOutput = CreateBucketOutput 
  { "Location" :: NullOrUndefined.NullOrUndefined (Location)
  }
derive instance newtypeCreateBucketOutput :: Newtype CreateBucketOutput _
derive instance repGenericCreateBucketOutput :: Generic CreateBucketOutput _
instance showCreateBucketOutput :: Show CreateBucketOutput where
  show = genericShow
instance decodeCreateBucketOutput :: Decode CreateBucketOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBucketOutput :: Encode CreateBucketOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateBucketRequest = CreateBucketRequest 
  { "ACL" :: NullOrUndefined.NullOrUndefined (BucketCannedACL)
  , "Bucket" :: (BucketName)
  , "CreateBucketConfiguration" :: NullOrUndefined.NullOrUndefined (CreateBucketConfiguration)
  , "GrantFullControl" :: NullOrUndefined.NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined.NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined.NullOrUndefined (GrantReadACP)
  , "GrantWrite" :: NullOrUndefined.NullOrUndefined (GrantWrite)
  , "GrantWriteACP" :: NullOrUndefined.NullOrUndefined (GrantWriteACP)
  }
derive instance newtypeCreateBucketRequest :: Newtype CreateBucketRequest _
derive instance repGenericCreateBucketRequest :: Generic CreateBucketRequest _
instance showCreateBucketRequest :: Show CreateBucketRequest where
  show = genericShow
instance decodeCreateBucketRequest :: Decode CreateBucketRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBucketRequest :: Encode CreateBucketRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateMultipartUploadOutput = CreateMultipartUploadOutput 
  { "AbortDate" :: NullOrUndefined.NullOrUndefined (AbortDate)
  , "AbortRuleId" :: NullOrUndefined.NullOrUndefined (AbortRuleId)
  , "Bucket" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "Key" :: NullOrUndefined.NullOrUndefined (ObjectKey)
  , "UploadId" :: NullOrUndefined.NullOrUndefined (MultipartUploadId)
  , "ServerSideEncryption" :: NullOrUndefined.NullOrUndefined (ServerSideEncryption)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  }
derive instance newtypeCreateMultipartUploadOutput :: Newtype CreateMultipartUploadOutput _
derive instance repGenericCreateMultipartUploadOutput :: Generic CreateMultipartUploadOutput _
instance showCreateMultipartUploadOutput :: Show CreateMultipartUploadOutput where
  show = genericShow
instance decodeCreateMultipartUploadOutput :: Decode CreateMultipartUploadOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMultipartUploadOutput :: Encode CreateMultipartUploadOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateMultipartUploadRequest = CreateMultipartUploadRequest 
  { "ACL" :: NullOrUndefined.NullOrUndefined (ObjectCannedACL)
  , "Bucket" :: (BucketName)
  , "CacheControl" :: NullOrUndefined.NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined.NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined.NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined.NullOrUndefined (ContentLanguage)
  , "ContentType" :: NullOrUndefined.NullOrUndefined (ContentType)
  , "Expires" :: NullOrUndefined.NullOrUndefined (Expires)
  , "GrantFullControl" :: NullOrUndefined.NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined.NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined.NullOrUndefined (GrantReadACP)
  , "GrantWriteACP" :: NullOrUndefined.NullOrUndefined (GrantWriteACP)
  , "Key" :: (ObjectKey)
  , "Metadata" :: NullOrUndefined.NullOrUndefined (Metadata)
  , "ServerSideEncryption" :: NullOrUndefined.NullOrUndefined (ServerSideEncryption)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  , "WebsiteRedirectLocation" :: NullOrUndefined.NullOrUndefined (WebsiteRedirectLocation)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined.NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  , "Tagging" :: NullOrUndefined.NullOrUndefined (TaggingHeader)
  }
derive instance newtypeCreateMultipartUploadRequest :: Newtype CreateMultipartUploadRequest _
derive instance repGenericCreateMultipartUploadRequest :: Generic CreateMultipartUploadRequest _
instance showCreateMultipartUploadRequest :: Show CreateMultipartUploadRequest where
  show = genericShow
instance decodeCreateMultipartUploadRequest :: Decode CreateMultipartUploadRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMultipartUploadRequest :: Encode CreateMultipartUploadRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreationDate = CreationDate Number
derive instance newtypeCreationDate :: Newtype CreationDate _
derive instance repGenericCreationDate :: Generic CreationDate _
instance showCreationDate :: Show CreationDate where
  show = genericShow
instance decodeCreationDate :: Decode CreationDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreationDate :: Encode CreationDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _
derive instance repGenericDate :: Generic Date _
instance showDate :: Show Date where
  show = genericShow
instance decodeDate :: Decode Date where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDate :: Encode Date where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Days = Days Int
derive instance newtypeDays :: Newtype Days _
derive instance repGenericDays :: Generic Days _
instance showDays :: Show Days where
  show = genericShow
instance decodeDays :: Decode Days where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDays :: Encode Days where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DaysAfterInitiation = DaysAfterInitiation Int
derive instance newtypeDaysAfterInitiation :: Newtype DaysAfterInitiation _
derive instance repGenericDaysAfterInitiation :: Generic DaysAfterInitiation _
instance showDaysAfterInitiation :: Show DaysAfterInitiation where
  show = genericShow
instance decodeDaysAfterInitiation :: Decode DaysAfterInitiation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDaysAfterInitiation :: Encode DaysAfterInitiation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Delete = Delete 
  { "Objects" :: (ObjectIdentifierList)
  , "Quiet" :: NullOrUndefined.NullOrUndefined (Quiet)
  }
derive instance newtypeDelete :: Newtype Delete _
derive instance repGenericDelete :: Generic Delete _
instance showDelete :: Show Delete where
  show = genericShow
instance decodeDelete :: Decode Delete where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelete :: Encode Delete where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBucketAnalyticsConfigurationRequest = DeleteBucketAnalyticsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (AnalyticsId)
  }
derive instance newtypeDeleteBucketAnalyticsConfigurationRequest :: Newtype DeleteBucketAnalyticsConfigurationRequest _
derive instance repGenericDeleteBucketAnalyticsConfigurationRequest :: Generic DeleteBucketAnalyticsConfigurationRequest _
instance showDeleteBucketAnalyticsConfigurationRequest :: Show DeleteBucketAnalyticsConfigurationRequest where
  show = genericShow
instance decodeDeleteBucketAnalyticsConfigurationRequest :: Decode DeleteBucketAnalyticsConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBucketAnalyticsConfigurationRequest :: Encode DeleteBucketAnalyticsConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBucketCorsRequest = DeleteBucketCorsRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketCorsRequest :: Newtype DeleteBucketCorsRequest _
derive instance repGenericDeleteBucketCorsRequest :: Generic DeleteBucketCorsRequest _
instance showDeleteBucketCorsRequest :: Show DeleteBucketCorsRequest where
  show = genericShow
instance decodeDeleteBucketCorsRequest :: Decode DeleteBucketCorsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBucketCorsRequest :: Encode DeleteBucketCorsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBucketEncryptionRequest = DeleteBucketEncryptionRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketEncryptionRequest :: Newtype DeleteBucketEncryptionRequest _
derive instance repGenericDeleteBucketEncryptionRequest :: Generic DeleteBucketEncryptionRequest _
instance showDeleteBucketEncryptionRequest :: Show DeleteBucketEncryptionRequest where
  show = genericShow
instance decodeDeleteBucketEncryptionRequest :: Decode DeleteBucketEncryptionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBucketEncryptionRequest :: Encode DeleteBucketEncryptionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBucketInventoryConfigurationRequest = DeleteBucketInventoryConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (InventoryId)
  }
derive instance newtypeDeleteBucketInventoryConfigurationRequest :: Newtype DeleteBucketInventoryConfigurationRequest _
derive instance repGenericDeleteBucketInventoryConfigurationRequest :: Generic DeleteBucketInventoryConfigurationRequest _
instance showDeleteBucketInventoryConfigurationRequest :: Show DeleteBucketInventoryConfigurationRequest where
  show = genericShow
instance decodeDeleteBucketInventoryConfigurationRequest :: Decode DeleteBucketInventoryConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBucketInventoryConfigurationRequest :: Encode DeleteBucketInventoryConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBucketLifecycleRequest = DeleteBucketLifecycleRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketLifecycleRequest :: Newtype DeleteBucketLifecycleRequest _
derive instance repGenericDeleteBucketLifecycleRequest :: Generic DeleteBucketLifecycleRequest _
instance showDeleteBucketLifecycleRequest :: Show DeleteBucketLifecycleRequest where
  show = genericShow
instance decodeDeleteBucketLifecycleRequest :: Decode DeleteBucketLifecycleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBucketLifecycleRequest :: Encode DeleteBucketLifecycleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBucketMetricsConfigurationRequest = DeleteBucketMetricsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (MetricsId)
  }
derive instance newtypeDeleteBucketMetricsConfigurationRequest :: Newtype DeleteBucketMetricsConfigurationRequest _
derive instance repGenericDeleteBucketMetricsConfigurationRequest :: Generic DeleteBucketMetricsConfigurationRequest _
instance showDeleteBucketMetricsConfigurationRequest :: Show DeleteBucketMetricsConfigurationRequest where
  show = genericShow
instance decodeDeleteBucketMetricsConfigurationRequest :: Decode DeleteBucketMetricsConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBucketMetricsConfigurationRequest :: Encode DeleteBucketMetricsConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBucketPolicyRequest = DeleteBucketPolicyRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketPolicyRequest :: Newtype DeleteBucketPolicyRequest _
derive instance repGenericDeleteBucketPolicyRequest :: Generic DeleteBucketPolicyRequest _
instance showDeleteBucketPolicyRequest :: Show DeleteBucketPolicyRequest where
  show = genericShow
instance decodeDeleteBucketPolicyRequest :: Decode DeleteBucketPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBucketPolicyRequest :: Encode DeleteBucketPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBucketReplicationRequest = DeleteBucketReplicationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketReplicationRequest :: Newtype DeleteBucketReplicationRequest _
derive instance repGenericDeleteBucketReplicationRequest :: Generic DeleteBucketReplicationRequest _
instance showDeleteBucketReplicationRequest :: Show DeleteBucketReplicationRequest where
  show = genericShow
instance decodeDeleteBucketReplicationRequest :: Decode DeleteBucketReplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBucketReplicationRequest :: Encode DeleteBucketReplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBucketRequest = DeleteBucketRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketRequest :: Newtype DeleteBucketRequest _
derive instance repGenericDeleteBucketRequest :: Generic DeleteBucketRequest _
instance showDeleteBucketRequest :: Show DeleteBucketRequest where
  show = genericShow
instance decodeDeleteBucketRequest :: Decode DeleteBucketRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBucketRequest :: Encode DeleteBucketRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBucketTaggingRequest = DeleteBucketTaggingRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketTaggingRequest :: Newtype DeleteBucketTaggingRequest _
derive instance repGenericDeleteBucketTaggingRequest :: Generic DeleteBucketTaggingRequest _
instance showDeleteBucketTaggingRequest :: Show DeleteBucketTaggingRequest where
  show = genericShow
instance decodeDeleteBucketTaggingRequest :: Decode DeleteBucketTaggingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBucketTaggingRequest :: Encode DeleteBucketTaggingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBucketWebsiteRequest = DeleteBucketWebsiteRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketWebsiteRequest :: Newtype DeleteBucketWebsiteRequest _
derive instance repGenericDeleteBucketWebsiteRequest :: Generic DeleteBucketWebsiteRequest _
instance showDeleteBucketWebsiteRequest :: Show DeleteBucketWebsiteRequest where
  show = genericShow
instance decodeDeleteBucketWebsiteRequest :: Decode DeleteBucketWebsiteRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBucketWebsiteRequest :: Encode DeleteBucketWebsiteRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteMarker = DeleteMarker Boolean
derive instance newtypeDeleteMarker :: Newtype DeleteMarker _
derive instance repGenericDeleteMarker :: Generic DeleteMarker _
instance showDeleteMarker :: Show DeleteMarker where
  show = genericShow
instance decodeDeleteMarker :: Decode DeleteMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMarker :: Encode DeleteMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteMarkerEntry = DeleteMarkerEntry 
  { "Owner" :: NullOrUndefined.NullOrUndefined (Owner)
  , "Key" :: NullOrUndefined.NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "IsLatest" :: NullOrUndefined.NullOrUndefined (IsLatest)
  , "LastModified" :: NullOrUndefined.NullOrUndefined (LastModified)
  }
derive instance newtypeDeleteMarkerEntry :: Newtype DeleteMarkerEntry _
derive instance repGenericDeleteMarkerEntry :: Generic DeleteMarkerEntry _
instance showDeleteMarkerEntry :: Show DeleteMarkerEntry where
  show = genericShow
instance decodeDeleteMarkerEntry :: Decode DeleteMarkerEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMarkerEntry :: Encode DeleteMarkerEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteMarkerVersionId = DeleteMarkerVersionId String
derive instance newtypeDeleteMarkerVersionId :: Newtype DeleteMarkerVersionId _
derive instance repGenericDeleteMarkerVersionId :: Generic DeleteMarkerVersionId _
instance showDeleteMarkerVersionId :: Show DeleteMarkerVersionId where
  show = genericShow
instance decodeDeleteMarkerVersionId :: Decode DeleteMarkerVersionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMarkerVersionId :: Encode DeleteMarkerVersionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteMarkers = DeleteMarkers (Array DeleteMarkerEntry)
derive instance newtypeDeleteMarkers :: Newtype DeleteMarkers _
derive instance repGenericDeleteMarkers :: Generic DeleteMarkers _
instance showDeleteMarkers :: Show DeleteMarkers where
  show = genericShow
instance decodeDeleteMarkers :: Decode DeleteMarkers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMarkers :: Encode DeleteMarkers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteObjectOutput = DeleteObjectOutput 
  { "DeleteMarker" :: NullOrUndefined.NullOrUndefined (DeleteMarker)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  }
derive instance newtypeDeleteObjectOutput :: Newtype DeleteObjectOutput _
derive instance repGenericDeleteObjectOutput :: Generic DeleteObjectOutput _
instance showDeleteObjectOutput :: Show DeleteObjectOutput where
  show = genericShow
instance decodeDeleteObjectOutput :: Decode DeleteObjectOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteObjectOutput :: Encode DeleteObjectOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteObjectRequest = DeleteObjectRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "MFA" :: NullOrUndefined.NullOrUndefined (MFA)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  }
derive instance newtypeDeleteObjectRequest :: Newtype DeleteObjectRequest _
derive instance repGenericDeleteObjectRequest :: Generic DeleteObjectRequest _
instance showDeleteObjectRequest :: Show DeleteObjectRequest where
  show = genericShow
instance decodeDeleteObjectRequest :: Decode DeleteObjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteObjectRequest :: Encode DeleteObjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteObjectTaggingOutput = DeleteObjectTaggingOutput 
  { "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  }
derive instance newtypeDeleteObjectTaggingOutput :: Newtype DeleteObjectTaggingOutput _
derive instance repGenericDeleteObjectTaggingOutput :: Generic DeleteObjectTaggingOutput _
instance showDeleteObjectTaggingOutput :: Show DeleteObjectTaggingOutput where
  show = genericShow
instance decodeDeleteObjectTaggingOutput :: Decode DeleteObjectTaggingOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteObjectTaggingOutput :: Encode DeleteObjectTaggingOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteObjectTaggingRequest = DeleteObjectTaggingRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  }
derive instance newtypeDeleteObjectTaggingRequest :: Newtype DeleteObjectTaggingRequest _
derive instance repGenericDeleteObjectTaggingRequest :: Generic DeleteObjectTaggingRequest _
instance showDeleteObjectTaggingRequest :: Show DeleteObjectTaggingRequest where
  show = genericShow
instance decodeDeleteObjectTaggingRequest :: Decode DeleteObjectTaggingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteObjectTaggingRequest :: Encode DeleteObjectTaggingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteObjectsOutput = DeleteObjectsOutput 
  { "Deleted" :: NullOrUndefined.NullOrUndefined (DeletedObjects)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  , "Errors" :: NullOrUndefined.NullOrUndefined (Errors)
  }
derive instance newtypeDeleteObjectsOutput :: Newtype DeleteObjectsOutput _
derive instance repGenericDeleteObjectsOutput :: Generic DeleteObjectsOutput _
instance showDeleteObjectsOutput :: Show DeleteObjectsOutput where
  show = genericShow
instance decodeDeleteObjectsOutput :: Decode DeleteObjectsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteObjectsOutput :: Encode DeleteObjectsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteObjectsRequest = DeleteObjectsRequest 
  { "Bucket" :: (BucketName)
  , "Delete" :: (Delete)
  , "MFA" :: NullOrUndefined.NullOrUndefined (MFA)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  }
derive instance newtypeDeleteObjectsRequest :: Newtype DeleteObjectsRequest _
derive instance repGenericDeleteObjectsRequest :: Generic DeleteObjectsRequest _
instance showDeleteObjectsRequest :: Show DeleteObjectsRequest where
  show = genericShow
instance decodeDeleteObjectsRequest :: Decode DeleteObjectsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteObjectsRequest :: Encode DeleteObjectsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletedObject = DeletedObject 
  { "Key" :: NullOrUndefined.NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "DeleteMarker" :: NullOrUndefined.NullOrUndefined (DeleteMarker)
  , "DeleteMarkerVersionId" :: NullOrUndefined.NullOrUndefined (DeleteMarkerVersionId)
  }
derive instance newtypeDeletedObject :: Newtype DeletedObject _
derive instance repGenericDeletedObject :: Generic DeletedObject _
instance showDeletedObject :: Show DeletedObject where
  show = genericShow
instance decodeDeletedObject :: Decode DeletedObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletedObject :: Encode DeletedObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletedObjects = DeletedObjects (Array DeletedObject)
derive instance newtypeDeletedObjects :: Newtype DeletedObjects _
derive instance repGenericDeletedObjects :: Generic DeletedObjects _
instance showDeletedObjects :: Show DeletedObjects where
  show = genericShow
instance decodeDeletedObjects :: Decode DeletedObjects where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletedObjects :: Encode DeletedObjects where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Delimiter = Delimiter String
derive instance newtypeDelimiter :: Newtype Delimiter _
derive instance repGenericDelimiter :: Generic Delimiter _
instance showDelimiter :: Show Delimiter where
  show = genericShow
instance decodeDelimiter :: Decode Delimiter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelimiter :: Encode Delimiter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _
derive instance repGenericDescription :: Generic Description _
instance showDescription :: Show Description where
  show = genericShow
instance decodeDescription :: Decode Description where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescription :: Encode Description where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for replication destination information.
newtype Destination = Destination 
  { "Bucket" :: (BucketName)
  , "Account" :: NullOrUndefined.NullOrUndefined (AccountId)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  , "AccessControlTranslation" :: NullOrUndefined.NullOrUndefined (AccessControlTranslation)
  , "EncryptionConfiguration" :: NullOrUndefined.NullOrUndefined (EncryptionConfiguration)
  }
derive instance newtypeDestination :: Newtype Destination _
derive instance repGenericDestination :: Generic Destination _
instance showDestination :: Show Destination where
  show = genericShow
instance decodeDestination :: Decode Destination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDestination :: Encode Destination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisplayName = DisplayName String
derive instance newtypeDisplayName :: Newtype DisplayName _
derive instance repGenericDisplayName :: Generic DisplayName _
instance showDisplayName :: Show DisplayName where
  show = genericShow
instance decodeDisplayName :: Decode DisplayName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisplayName :: Encode DisplayName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ETag = ETag String
derive instance newtypeETag :: Newtype ETag _
derive instance repGenericETag :: Generic ETag _
instance showETag :: Show ETag where
  show = genericShow
instance decodeETag :: Decode ETag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeETag :: Encode ETag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EmailAddress = EmailAddress String
derive instance newtypeEmailAddress :: Newtype EmailAddress _
derive instance repGenericEmailAddress :: Generic EmailAddress _
instance showEmailAddress :: Show EmailAddress where
  show = genericShow
instance decodeEmailAddress :: Decode EmailAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailAddress :: Encode EmailAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Requests Amazon S3 to encode the object keys in the response and specifies the encoding method to use. An object key may contain any Unicode character; however, XML 1.0 parser cannot parse some characters, such as characters with an ASCII value from 0 to 10. For characters that are not supported in XML 1.0, you can add this parameter to request that Amazon S3 encode the keys in the response.
newtype EncodingType = EncodingType String
derive instance newtypeEncodingType :: Newtype EncodingType _
derive instance repGenericEncodingType :: Generic EncodingType _
instance showEncodingType :: Show EncodingType where
  show = genericShow
instance decodeEncodingType :: Decode EncodingType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncodingType :: Encode EncodingType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Describes the server-side encryption that will be applied to the restore results.
newtype Encryption = Encryption 
  { "EncryptionType" :: (ServerSideEncryption)
  , "KMSKeyId" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  , "KMSContext" :: NullOrUndefined.NullOrUndefined (KMSContext)
  }
derive instance newtypeEncryption :: Newtype Encryption _
derive instance repGenericEncryption :: Generic Encryption _
instance showEncryption :: Show Encryption where
  show = genericShow
instance decodeEncryption :: Decode Encryption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryption :: Encode Encryption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for information regarding encryption based configuration for replicas.
newtype EncryptionConfiguration = EncryptionConfiguration 
  { "ReplicaKmsKeyID" :: NullOrUndefined.NullOrUndefined (ReplicaKmsKeyID)
  }
derive instance newtypeEncryptionConfiguration :: Newtype EncryptionConfiguration _
derive instance repGenericEncryptionConfiguration :: Generic EncryptionConfiguration _
instance showEncryptionConfiguration :: Show EncryptionConfiguration where
  show = genericShow
instance decodeEncryptionConfiguration :: Decode EncryptionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionConfiguration :: Encode EncryptionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Error = Error 
  { "Key" :: NullOrUndefined.NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "Code" :: NullOrUndefined.NullOrUndefined (Code)
  , "Message" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeError :: Newtype Error _
derive instance repGenericError :: Generic Error _
instance showError :: Show Error where
  show = genericShow
instance decodeError :: Decode Error where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeError :: Encode Error where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorDocument = ErrorDocument 
  { "Key" :: (ObjectKey)
  }
derive instance newtypeErrorDocument :: Newtype ErrorDocument _
derive instance repGenericErrorDocument :: Generic ErrorDocument _
instance showErrorDocument :: Show ErrorDocument where
  show = genericShow
instance decodeErrorDocument :: Decode ErrorDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorDocument :: Encode ErrorDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Errors = Errors (Array Error)
derive instance newtypeErrors :: Newtype Errors _
derive instance repGenericErrors :: Generic Errors _
instance showErrors :: Show Errors where
  show = genericShow
instance decodeErrors :: Decode Errors where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrors :: Encode Errors where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Bucket event for which to send notifications.
newtype Event = Event String
derive instance newtypeEvent :: Newtype Event _
derive instance repGenericEvent :: Generic Event _
instance showEvent :: Show Event where
  show = genericShow
instance decodeEvent :: Decode Event where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvent :: Encode Event where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventList = EventList (Array Event)
derive instance newtypeEventList :: Newtype EventList _
derive instance repGenericEventList :: Generic EventList _
instance showEventList :: Show EventList where
  show = genericShow
instance decodeEventList :: Decode EventList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventList :: Encode EventList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Expiration = Expiration String
derive instance newtypeExpiration :: Newtype Expiration _
derive instance repGenericExpiration :: Generic Expiration _
instance showExpiration :: Show Expiration where
  show = genericShow
instance decodeExpiration :: Decode Expiration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpiration :: Encode Expiration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExpirationStatus = ExpirationStatus String
derive instance newtypeExpirationStatus :: Newtype ExpirationStatus _
derive instance repGenericExpirationStatus :: Generic ExpirationStatus _
instance showExpirationStatus :: Show ExpirationStatus where
  show = genericShow
instance decodeExpirationStatus :: Decode ExpirationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpirationStatus :: Encode ExpirationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExpiredObjectDeleteMarker = ExpiredObjectDeleteMarker Boolean
derive instance newtypeExpiredObjectDeleteMarker :: Newtype ExpiredObjectDeleteMarker _
derive instance repGenericExpiredObjectDeleteMarker :: Generic ExpiredObjectDeleteMarker _
instance showExpiredObjectDeleteMarker :: Show ExpiredObjectDeleteMarker where
  show = genericShow
instance decodeExpiredObjectDeleteMarker :: Decode ExpiredObjectDeleteMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpiredObjectDeleteMarker :: Encode ExpiredObjectDeleteMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Expires = Expires Number
derive instance newtypeExpires :: Newtype Expires _
derive instance repGenericExpires :: Generic Expires _
instance showExpires :: Show Expires where
  show = genericShow
instance decodeExpires :: Decode Expires where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpires :: Encode Expires where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExposeHeader = ExposeHeader String
derive instance newtypeExposeHeader :: Newtype ExposeHeader _
derive instance repGenericExposeHeader :: Generic ExposeHeader _
instance showExposeHeader :: Show ExposeHeader where
  show = genericShow
instance decodeExposeHeader :: Decode ExposeHeader where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExposeHeader :: Encode ExposeHeader where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExposeHeaders = ExposeHeaders (Array ExposeHeader)
derive instance newtypeExposeHeaders :: Newtype ExposeHeaders _
derive instance repGenericExposeHeaders :: Generic ExposeHeaders _
instance showExposeHeaders :: Show ExposeHeaders where
  show = genericShow
instance decodeExposeHeaders :: Decode ExposeHeaders where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExposeHeaders :: Encode ExposeHeaders where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Expression = Expression String
derive instance newtypeExpression :: Newtype Expression _
derive instance repGenericExpression :: Generic Expression _
instance showExpression :: Show Expression where
  show = genericShow
instance decodeExpression :: Decode Expression where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpression :: Encode Expression where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExpressionType = ExpressionType String
derive instance newtypeExpressionType :: Newtype ExpressionType _
derive instance repGenericExpressionType :: Generic ExpressionType _
instance showExpressionType :: Show ExpressionType where
  show = genericShow
instance decodeExpressionType :: Decode ExpressionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpressionType :: Encode ExpressionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FetchOwner = FetchOwner Boolean
derive instance newtypeFetchOwner :: Newtype FetchOwner _
derive instance repGenericFetchOwner :: Generic FetchOwner _
instance showFetchOwner :: Show FetchOwner where
  show = genericShow
instance decodeFetchOwner :: Decode FetchOwner where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFetchOwner :: Encode FetchOwner where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FieldDelimiter = FieldDelimiter String
derive instance newtypeFieldDelimiter :: Newtype FieldDelimiter _
derive instance repGenericFieldDelimiter :: Generic FieldDelimiter _
instance showFieldDelimiter :: Show FieldDelimiter where
  show = genericShow
instance decodeFieldDelimiter :: Decode FieldDelimiter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFieldDelimiter :: Encode FieldDelimiter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FileHeaderInfo = FileHeaderInfo String
derive instance newtypeFileHeaderInfo :: Newtype FileHeaderInfo _
derive instance repGenericFileHeaderInfo :: Generic FileHeaderInfo _
instance showFileHeaderInfo :: Show FileHeaderInfo where
  show = genericShow
instance decodeFileHeaderInfo :: Decode FileHeaderInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileHeaderInfo :: Encode FileHeaderInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for key value pair that defines the criteria for the filter rule.
newtype FilterRule = FilterRule 
  { "Name" :: NullOrUndefined.NullOrUndefined (FilterRuleName)
  , "Value" :: NullOrUndefined.NullOrUndefined (FilterRuleValue)
  }
derive instance newtypeFilterRule :: Newtype FilterRule _
derive instance repGenericFilterRule :: Generic FilterRule _
instance showFilterRule :: Show FilterRule where
  show = genericShow
instance decodeFilterRule :: Decode FilterRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterRule :: Encode FilterRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of containers for key value pair that defines the criteria for the filter rule.
newtype FilterRuleList = FilterRuleList (Array FilterRule)
derive instance newtypeFilterRuleList :: Newtype FilterRuleList _
derive instance repGenericFilterRuleList :: Generic FilterRuleList _
instance showFilterRuleList :: Show FilterRuleList where
  show = genericShow
instance decodeFilterRuleList :: Decode FilterRuleList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterRuleList :: Encode FilterRuleList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterRuleName = FilterRuleName String
derive instance newtypeFilterRuleName :: Newtype FilterRuleName _
derive instance repGenericFilterRuleName :: Generic FilterRuleName _
instance showFilterRuleName :: Show FilterRuleName where
  show = genericShow
instance decodeFilterRuleName :: Decode FilterRuleName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterRuleName :: Encode FilterRuleName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterRuleValue = FilterRuleValue String
derive instance newtypeFilterRuleValue :: Newtype FilterRuleValue _
derive instance repGenericFilterRuleValue :: Generic FilterRuleValue _
instance showFilterRuleValue :: Show FilterRuleValue where
  show = genericShow
instance decodeFilterRuleValue :: Decode FilterRuleValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterRuleValue :: Encode FilterRuleValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketAccelerateConfigurationOutput = GetBucketAccelerateConfigurationOutput 
  { "Status" :: NullOrUndefined.NullOrUndefined (BucketAccelerateStatus)
  }
derive instance newtypeGetBucketAccelerateConfigurationOutput :: Newtype GetBucketAccelerateConfigurationOutput _
derive instance repGenericGetBucketAccelerateConfigurationOutput :: Generic GetBucketAccelerateConfigurationOutput _
instance showGetBucketAccelerateConfigurationOutput :: Show GetBucketAccelerateConfigurationOutput where
  show = genericShow
instance decodeGetBucketAccelerateConfigurationOutput :: Decode GetBucketAccelerateConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketAccelerateConfigurationOutput :: Encode GetBucketAccelerateConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketAccelerateConfigurationRequest = GetBucketAccelerateConfigurationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketAccelerateConfigurationRequest :: Newtype GetBucketAccelerateConfigurationRequest _
derive instance repGenericGetBucketAccelerateConfigurationRequest :: Generic GetBucketAccelerateConfigurationRequest _
instance showGetBucketAccelerateConfigurationRequest :: Show GetBucketAccelerateConfigurationRequest where
  show = genericShow
instance decodeGetBucketAccelerateConfigurationRequest :: Decode GetBucketAccelerateConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketAccelerateConfigurationRequest :: Encode GetBucketAccelerateConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketAclOutput = GetBucketAclOutput 
  { "Owner" :: NullOrUndefined.NullOrUndefined (Owner)
  , "Grants" :: NullOrUndefined.NullOrUndefined (Grants)
  }
derive instance newtypeGetBucketAclOutput :: Newtype GetBucketAclOutput _
derive instance repGenericGetBucketAclOutput :: Generic GetBucketAclOutput _
instance showGetBucketAclOutput :: Show GetBucketAclOutput where
  show = genericShow
instance decodeGetBucketAclOutput :: Decode GetBucketAclOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketAclOutput :: Encode GetBucketAclOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketAclRequest = GetBucketAclRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketAclRequest :: Newtype GetBucketAclRequest _
derive instance repGenericGetBucketAclRequest :: Generic GetBucketAclRequest _
instance showGetBucketAclRequest :: Show GetBucketAclRequest where
  show = genericShow
instance decodeGetBucketAclRequest :: Decode GetBucketAclRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketAclRequest :: Encode GetBucketAclRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketAnalyticsConfigurationOutput = GetBucketAnalyticsConfigurationOutput 
  { "AnalyticsConfiguration" :: NullOrUndefined.NullOrUndefined (AnalyticsConfiguration)
  }
derive instance newtypeGetBucketAnalyticsConfigurationOutput :: Newtype GetBucketAnalyticsConfigurationOutput _
derive instance repGenericGetBucketAnalyticsConfigurationOutput :: Generic GetBucketAnalyticsConfigurationOutput _
instance showGetBucketAnalyticsConfigurationOutput :: Show GetBucketAnalyticsConfigurationOutput where
  show = genericShow
instance decodeGetBucketAnalyticsConfigurationOutput :: Decode GetBucketAnalyticsConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketAnalyticsConfigurationOutput :: Encode GetBucketAnalyticsConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketAnalyticsConfigurationRequest = GetBucketAnalyticsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (AnalyticsId)
  }
derive instance newtypeGetBucketAnalyticsConfigurationRequest :: Newtype GetBucketAnalyticsConfigurationRequest _
derive instance repGenericGetBucketAnalyticsConfigurationRequest :: Generic GetBucketAnalyticsConfigurationRequest _
instance showGetBucketAnalyticsConfigurationRequest :: Show GetBucketAnalyticsConfigurationRequest where
  show = genericShow
instance decodeGetBucketAnalyticsConfigurationRequest :: Decode GetBucketAnalyticsConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketAnalyticsConfigurationRequest :: Encode GetBucketAnalyticsConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketCorsOutput = GetBucketCorsOutput 
  { "CORSRules" :: NullOrUndefined.NullOrUndefined (CORSRules)
  }
derive instance newtypeGetBucketCorsOutput :: Newtype GetBucketCorsOutput _
derive instance repGenericGetBucketCorsOutput :: Generic GetBucketCorsOutput _
instance showGetBucketCorsOutput :: Show GetBucketCorsOutput where
  show = genericShow
instance decodeGetBucketCorsOutput :: Decode GetBucketCorsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketCorsOutput :: Encode GetBucketCorsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketCorsRequest = GetBucketCorsRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketCorsRequest :: Newtype GetBucketCorsRequest _
derive instance repGenericGetBucketCorsRequest :: Generic GetBucketCorsRequest _
instance showGetBucketCorsRequest :: Show GetBucketCorsRequest where
  show = genericShow
instance decodeGetBucketCorsRequest :: Decode GetBucketCorsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketCorsRequest :: Encode GetBucketCorsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketEncryptionOutput = GetBucketEncryptionOutput 
  { "ServerSideEncryptionConfiguration" :: NullOrUndefined.NullOrUndefined (ServerSideEncryptionConfiguration)
  }
derive instance newtypeGetBucketEncryptionOutput :: Newtype GetBucketEncryptionOutput _
derive instance repGenericGetBucketEncryptionOutput :: Generic GetBucketEncryptionOutput _
instance showGetBucketEncryptionOutput :: Show GetBucketEncryptionOutput where
  show = genericShow
instance decodeGetBucketEncryptionOutput :: Decode GetBucketEncryptionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketEncryptionOutput :: Encode GetBucketEncryptionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketEncryptionRequest = GetBucketEncryptionRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketEncryptionRequest :: Newtype GetBucketEncryptionRequest _
derive instance repGenericGetBucketEncryptionRequest :: Generic GetBucketEncryptionRequest _
instance showGetBucketEncryptionRequest :: Show GetBucketEncryptionRequest where
  show = genericShow
instance decodeGetBucketEncryptionRequest :: Decode GetBucketEncryptionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketEncryptionRequest :: Encode GetBucketEncryptionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketInventoryConfigurationOutput = GetBucketInventoryConfigurationOutput 
  { "InventoryConfiguration" :: NullOrUndefined.NullOrUndefined (InventoryConfiguration)
  }
derive instance newtypeGetBucketInventoryConfigurationOutput :: Newtype GetBucketInventoryConfigurationOutput _
derive instance repGenericGetBucketInventoryConfigurationOutput :: Generic GetBucketInventoryConfigurationOutput _
instance showGetBucketInventoryConfigurationOutput :: Show GetBucketInventoryConfigurationOutput where
  show = genericShow
instance decodeGetBucketInventoryConfigurationOutput :: Decode GetBucketInventoryConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketInventoryConfigurationOutput :: Encode GetBucketInventoryConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketInventoryConfigurationRequest = GetBucketInventoryConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (InventoryId)
  }
derive instance newtypeGetBucketInventoryConfigurationRequest :: Newtype GetBucketInventoryConfigurationRequest _
derive instance repGenericGetBucketInventoryConfigurationRequest :: Generic GetBucketInventoryConfigurationRequest _
instance showGetBucketInventoryConfigurationRequest :: Show GetBucketInventoryConfigurationRequest where
  show = genericShow
instance decodeGetBucketInventoryConfigurationRequest :: Decode GetBucketInventoryConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketInventoryConfigurationRequest :: Encode GetBucketInventoryConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketLifecycleConfigurationOutput = GetBucketLifecycleConfigurationOutput 
  { "Rules" :: NullOrUndefined.NullOrUndefined (LifecycleRules)
  }
derive instance newtypeGetBucketLifecycleConfigurationOutput :: Newtype GetBucketLifecycleConfigurationOutput _
derive instance repGenericGetBucketLifecycleConfigurationOutput :: Generic GetBucketLifecycleConfigurationOutput _
instance showGetBucketLifecycleConfigurationOutput :: Show GetBucketLifecycleConfigurationOutput where
  show = genericShow
instance decodeGetBucketLifecycleConfigurationOutput :: Decode GetBucketLifecycleConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketLifecycleConfigurationOutput :: Encode GetBucketLifecycleConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketLifecycleConfigurationRequest = GetBucketLifecycleConfigurationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketLifecycleConfigurationRequest :: Newtype GetBucketLifecycleConfigurationRequest _
derive instance repGenericGetBucketLifecycleConfigurationRequest :: Generic GetBucketLifecycleConfigurationRequest _
instance showGetBucketLifecycleConfigurationRequest :: Show GetBucketLifecycleConfigurationRequest where
  show = genericShow
instance decodeGetBucketLifecycleConfigurationRequest :: Decode GetBucketLifecycleConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketLifecycleConfigurationRequest :: Encode GetBucketLifecycleConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketLifecycleOutput = GetBucketLifecycleOutput 
  { "Rules" :: NullOrUndefined.NullOrUndefined (Rules)
  }
derive instance newtypeGetBucketLifecycleOutput :: Newtype GetBucketLifecycleOutput _
derive instance repGenericGetBucketLifecycleOutput :: Generic GetBucketLifecycleOutput _
instance showGetBucketLifecycleOutput :: Show GetBucketLifecycleOutput where
  show = genericShow
instance decodeGetBucketLifecycleOutput :: Decode GetBucketLifecycleOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketLifecycleOutput :: Encode GetBucketLifecycleOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketLifecycleRequest = GetBucketLifecycleRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketLifecycleRequest :: Newtype GetBucketLifecycleRequest _
derive instance repGenericGetBucketLifecycleRequest :: Generic GetBucketLifecycleRequest _
instance showGetBucketLifecycleRequest :: Show GetBucketLifecycleRequest where
  show = genericShow
instance decodeGetBucketLifecycleRequest :: Decode GetBucketLifecycleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketLifecycleRequest :: Encode GetBucketLifecycleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketLocationOutput = GetBucketLocationOutput 
  { "LocationConstraint" :: NullOrUndefined.NullOrUndefined (BucketLocationConstraint)
  }
derive instance newtypeGetBucketLocationOutput :: Newtype GetBucketLocationOutput _
derive instance repGenericGetBucketLocationOutput :: Generic GetBucketLocationOutput _
instance showGetBucketLocationOutput :: Show GetBucketLocationOutput where
  show = genericShow
instance decodeGetBucketLocationOutput :: Decode GetBucketLocationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketLocationOutput :: Encode GetBucketLocationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketLocationRequest = GetBucketLocationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketLocationRequest :: Newtype GetBucketLocationRequest _
derive instance repGenericGetBucketLocationRequest :: Generic GetBucketLocationRequest _
instance showGetBucketLocationRequest :: Show GetBucketLocationRequest where
  show = genericShow
instance decodeGetBucketLocationRequest :: Decode GetBucketLocationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketLocationRequest :: Encode GetBucketLocationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketLoggingOutput = GetBucketLoggingOutput 
  { "LoggingEnabled" :: NullOrUndefined.NullOrUndefined (LoggingEnabled)
  }
derive instance newtypeGetBucketLoggingOutput :: Newtype GetBucketLoggingOutput _
derive instance repGenericGetBucketLoggingOutput :: Generic GetBucketLoggingOutput _
instance showGetBucketLoggingOutput :: Show GetBucketLoggingOutput where
  show = genericShow
instance decodeGetBucketLoggingOutput :: Decode GetBucketLoggingOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketLoggingOutput :: Encode GetBucketLoggingOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketLoggingRequest = GetBucketLoggingRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketLoggingRequest :: Newtype GetBucketLoggingRequest _
derive instance repGenericGetBucketLoggingRequest :: Generic GetBucketLoggingRequest _
instance showGetBucketLoggingRequest :: Show GetBucketLoggingRequest where
  show = genericShow
instance decodeGetBucketLoggingRequest :: Decode GetBucketLoggingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketLoggingRequest :: Encode GetBucketLoggingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketMetricsConfigurationOutput = GetBucketMetricsConfigurationOutput 
  { "MetricsConfiguration" :: NullOrUndefined.NullOrUndefined (MetricsConfiguration)
  }
derive instance newtypeGetBucketMetricsConfigurationOutput :: Newtype GetBucketMetricsConfigurationOutput _
derive instance repGenericGetBucketMetricsConfigurationOutput :: Generic GetBucketMetricsConfigurationOutput _
instance showGetBucketMetricsConfigurationOutput :: Show GetBucketMetricsConfigurationOutput where
  show = genericShow
instance decodeGetBucketMetricsConfigurationOutput :: Decode GetBucketMetricsConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketMetricsConfigurationOutput :: Encode GetBucketMetricsConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketMetricsConfigurationRequest = GetBucketMetricsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (MetricsId)
  }
derive instance newtypeGetBucketMetricsConfigurationRequest :: Newtype GetBucketMetricsConfigurationRequest _
derive instance repGenericGetBucketMetricsConfigurationRequest :: Generic GetBucketMetricsConfigurationRequest _
instance showGetBucketMetricsConfigurationRequest :: Show GetBucketMetricsConfigurationRequest where
  show = genericShow
instance decodeGetBucketMetricsConfigurationRequest :: Decode GetBucketMetricsConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketMetricsConfigurationRequest :: Encode GetBucketMetricsConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketNotificationConfigurationRequest = GetBucketNotificationConfigurationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketNotificationConfigurationRequest :: Newtype GetBucketNotificationConfigurationRequest _
derive instance repGenericGetBucketNotificationConfigurationRequest :: Generic GetBucketNotificationConfigurationRequest _
instance showGetBucketNotificationConfigurationRequest :: Show GetBucketNotificationConfigurationRequest where
  show = genericShow
instance decodeGetBucketNotificationConfigurationRequest :: Decode GetBucketNotificationConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketNotificationConfigurationRequest :: Encode GetBucketNotificationConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketPolicyOutput = GetBucketPolicyOutput 
  { "Policy" :: NullOrUndefined.NullOrUndefined (Policy)
  }
derive instance newtypeGetBucketPolicyOutput :: Newtype GetBucketPolicyOutput _
derive instance repGenericGetBucketPolicyOutput :: Generic GetBucketPolicyOutput _
instance showGetBucketPolicyOutput :: Show GetBucketPolicyOutput where
  show = genericShow
instance decodeGetBucketPolicyOutput :: Decode GetBucketPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketPolicyOutput :: Encode GetBucketPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketPolicyRequest = GetBucketPolicyRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketPolicyRequest :: Newtype GetBucketPolicyRequest _
derive instance repGenericGetBucketPolicyRequest :: Generic GetBucketPolicyRequest _
instance showGetBucketPolicyRequest :: Show GetBucketPolicyRequest where
  show = genericShow
instance decodeGetBucketPolicyRequest :: Decode GetBucketPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketPolicyRequest :: Encode GetBucketPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketReplicationOutput = GetBucketReplicationOutput 
  { "ReplicationConfiguration" :: NullOrUndefined.NullOrUndefined (ReplicationConfiguration)
  }
derive instance newtypeGetBucketReplicationOutput :: Newtype GetBucketReplicationOutput _
derive instance repGenericGetBucketReplicationOutput :: Generic GetBucketReplicationOutput _
instance showGetBucketReplicationOutput :: Show GetBucketReplicationOutput where
  show = genericShow
instance decodeGetBucketReplicationOutput :: Decode GetBucketReplicationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketReplicationOutput :: Encode GetBucketReplicationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketReplicationRequest = GetBucketReplicationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketReplicationRequest :: Newtype GetBucketReplicationRequest _
derive instance repGenericGetBucketReplicationRequest :: Generic GetBucketReplicationRequest _
instance showGetBucketReplicationRequest :: Show GetBucketReplicationRequest where
  show = genericShow
instance decodeGetBucketReplicationRequest :: Decode GetBucketReplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketReplicationRequest :: Encode GetBucketReplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketRequestPaymentOutput = GetBucketRequestPaymentOutput 
  { "Payer" :: NullOrUndefined.NullOrUndefined (Payer)
  }
derive instance newtypeGetBucketRequestPaymentOutput :: Newtype GetBucketRequestPaymentOutput _
derive instance repGenericGetBucketRequestPaymentOutput :: Generic GetBucketRequestPaymentOutput _
instance showGetBucketRequestPaymentOutput :: Show GetBucketRequestPaymentOutput where
  show = genericShow
instance decodeGetBucketRequestPaymentOutput :: Decode GetBucketRequestPaymentOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketRequestPaymentOutput :: Encode GetBucketRequestPaymentOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketRequestPaymentRequest = GetBucketRequestPaymentRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketRequestPaymentRequest :: Newtype GetBucketRequestPaymentRequest _
derive instance repGenericGetBucketRequestPaymentRequest :: Generic GetBucketRequestPaymentRequest _
instance showGetBucketRequestPaymentRequest :: Show GetBucketRequestPaymentRequest where
  show = genericShow
instance decodeGetBucketRequestPaymentRequest :: Decode GetBucketRequestPaymentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketRequestPaymentRequest :: Encode GetBucketRequestPaymentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketTaggingOutput = GetBucketTaggingOutput 
  { "TagSet" :: (TagSet)
  }
derive instance newtypeGetBucketTaggingOutput :: Newtype GetBucketTaggingOutput _
derive instance repGenericGetBucketTaggingOutput :: Generic GetBucketTaggingOutput _
instance showGetBucketTaggingOutput :: Show GetBucketTaggingOutput where
  show = genericShow
instance decodeGetBucketTaggingOutput :: Decode GetBucketTaggingOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketTaggingOutput :: Encode GetBucketTaggingOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketTaggingRequest = GetBucketTaggingRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketTaggingRequest :: Newtype GetBucketTaggingRequest _
derive instance repGenericGetBucketTaggingRequest :: Generic GetBucketTaggingRequest _
instance showGetBucketTaggingRequest :: Show GetBucketTaggingRequest where
  show = genericShow
instance decodeGetBucketTaggingRequest :: Decode GetBucketTaggingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketTaggingRequest :: Encode GetBucketTaggingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketVersioningOutput = GetBucketVersioningOutput 
  { "Status" :: NullOrUndefined.NullOrUndefined (BucketVersioningStatus)
  , "MFADelete" :: NullOrUndefined.NullOrUndefined (MFADeleteStatus)
  }
derive instance newtypeGetBucketVersioningOutput :: Newtype GetBucketVersioningOutput _
derive instance repGenericGetBucketVersioningOutput :: Generic GetBucketVersioningOutput _
instance showGetBucketVersioningOutput :: Show GetBucketVersioningOutput where
  show = genericShow
instance decodeGetBucketVersioningOutput :: Decode GetBucketVersioningOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketVersioningOutput :: Encode GetBucketVersioningOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketVersioningRequest = GetBucketVersioningRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketVersioningRequest :: Newtype GetBucketVersioningRequest _
derive instance repGenericGetBucketVersioningRequest :: Generic GetBucketVersioningRequest _
instance showGetBucketVersioningRequest :: Show GetBucketVersioningRequest where
  show = genericShow
instance decodeGetBucketVersioningRequest :: Decode GetBucketVersioningRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketVersioningRequest :: Encode GetBucketVersioningRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketWebsiteOutput = GetBucketWebsiteOutput 
  { "RedirectAllRequestsTo" :: NullOrUndefined.NullOrUndefined (RedirectAllRequestsTo)
  , "IndexDocument" :: NullOrUndefined.NullOrUndefined (IndexDocument)
  , "ErrorDocument" :: NullOrUndefined.NullOrUndefined (ErrorDocument)
  , "RoutingRules" :: NullOrUndefined.NullOrUndefined (RoutingRules)
  }
derive instance newtypeGetBucketWebsiteOutput :: Newtype GetBucketWebsiteOutput _
derive instance repGenericGetBucketWebsiteOutput :: Generic GetBucketWebsiteOutput _
instance showGetBucketWebsiteOutput :: Show GetBucketWebsiteOutput where
  show = genericShow
instance decodeGetBucketWebsiteOutput :: Decode GetBucketWebsiteOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketWebsiteOutput :: Encode GetBucketWebsiteOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBucketWebsiteRequest = GetBucketWebsiteRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketWebsiteRequest :: Newtype GetBucketWebsiteRequest _
derive instance repGenericGetBucketWebsiteRequest :: Generic GetBucketWebsiteRequest _
instance showGetBucketWebsiteRequest :: Show GetBucketWebsiteRequest where
  show = genericShow
instance decodeGetBucketWebsiteRequest :: Decode GetBucketWebsiteRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBucketWebsiteRequest :: Encode GetBucketWebsiteRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetObjectAclOutput = GetObjectAclOutput 
  { "Owner" :: NullOrUndefined.NullOrUndefined (Owner)
  , "Grants" :: NullOrUndefined.NullOrUndefined (Grants)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  }
derive instance newtypeGetObjectAclOutput :: Newtype GetObjectAclOutput _
derive instance repGenericGetObjectAclOutput :: Generic GetObjectAclOutput _
instance showGetObjectAclOutput :: Show GetObjectAclOutput where
  show = genericShow
instance decodeGetObjectAclOutput :: Decode GetObjectAclOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetObjectAclOutput :: Encode GetObjectAclOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetObjectAclRequest = GetObjectAclRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  }
derive instance newtypeGetObjectAclRequest :: Newtype GetObjectAclRequest _
derive instance repGenericGetObjectAclRequest :: Generic GetObjectAclRequest _
instance showGetObjectAclRequest :: Show GetObjectAclRequest where
  show = genericShow
instance decodeGetObjectAclRequest :: Decode GetObjectAclRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetObjectAclRequest :: Encode GetObjectAclRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetObjectOutput = GetObjectOutput 
  { "Body" :: NullOrUndefined.NullOrUndefined (Body)
  , "DeleteMarker" :: NullOrUndefined.NullOrUndefined (DeleteMarker)
  , "AcceptRanges" :: NullOrUndefined.NullOrUndefined (AcceptRanges)
  , "Expiration" :: NullOrUndefined.NullOrUndefined (Expiration)
  , "Restore" :: NullOrUndefined.NullOrUndefined (Restore)
  , "LastModified" :: NullOrUndefined.NullOrUndefined (LastModified)
  , "ContentLength" :: NullOrUndefined.NullOrUndefined (ContentLength)
  , "ETag" :: NullOrUndefined.NullOrUndefined (ETag)
  , "MissingMeta" :: NullOrUndefined.NullOrUndefined (MissingMeta)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "CacheControl" :: NullOrUndefined.NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined.NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined.NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined.NullOrUndefined (ContentLanguage)
  , "ContentRange" :: NullOrUndefined.NullOrUndefined (ContentRange)
  , "ContentType" :: NullOrUndefined.NullOrUndefined (ContentType)
  , "Expires" :: NullOrUndefined.NullOrUndefined (Expires)
  , "WebsiteRedirectLocation" :: NullOrUndefined.NullOrUndefined (WebsiteRedirectLocation)
  , "ServerSideEncryption" :: NullOrUndefined.NullOrUndefined (ServerSideEncryption)
  , "Metadata" :: NullOrUndefined.NullOrUndefined (Metadata)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  , "ReplicationStatus" :: NullOrUndefined.NullOrUndefined (ReplicationStatus)
  , "PartsCount" :: NullOrUndefined.NullOrUndefined (PartsCount)
  , "TagCount" :: NullOrUndefined.NullOrUndefined (TagCount)
  }
derive instance newtypeGetObjectOutput :: Newtype GetObjectOutput _
derive instance repGenericGetObjectOutput :: Generic GetObjectOutput _
instance showGetObjectOutput :: Show GetObjectOutput where
  show = genericShow
instance decodeGetObjectOutput :: Decode GetObjectOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetObjectOutput :: Encode GetObjectOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetObjectRequest = GetObjectRequest 
  { "Bucket" :: (BucketName)
  , "IfMatch" :: NullOrUndefined.NullOrUndefined (IfMatch)
  , "IfModifiedSince" :: NullOrUndefined.NullOrUndefined (IfModifiedSince)
  , "IfNoneMatch" :: NullOrUndefined.NullOrUndefined (IfNoneMatch)
  , "IfUnmodifiedSince" :: NullOrUndefined.NullOrUndefined (IfUnmodifiedSince)
  , "Key" :: (ObjectKey)
  , "Range" :: NullOrUndefined.NullOrUndefined (Range)
  , "ResponseCacheControl" :: NullOrUndefined.NullOrUndefined (ResponseCacheControl)
  , "ResponseContentDisposition" :: NullOrUndefined.NullOrUndefined (ResponseContentDisposition)
  , "ResponseContentEncoding" :: NullOrUndefined.NullOrUndefined (ResponseContentEncoding)
  , "ResponseContentLanguage" :: NullOrUndefined.NullOrUndefined (ResponseContentLanguage)
  , "ResponseContentType" :: NullOrUndefined.NullOrUndefined (ResponseContentType)
  , "ResponseExpires" :: NullOrUndefined.NullOrUndefined (ResponseExpires)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined.NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  , "PartNumber" :: NullOrUndefined.NullOrUndefined (PartNumber)
  }
derive instance newtypeGetObjectRequest :: Newtype GetObjectRequest _
derive instance repGenericGetObjectRequest :: Generic GetObjectRequest _
instance showGetObjectRequest :: Show GetObjectRequest where
  show = genericShow
instance decodeGetObjectRequest :: Decode GetObjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetObjectRequest :: Encode GetObjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetObjectTaggingOutput = GetObjectTaggingOutput 
  { "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "TagSet" :: (TagSet)
  }
derive instance newtypeGetObjectTaggingOutput :: Newtype GetObjectTaggingOutput _
derive instance repGenericGetObjectTaggingOutput :: Generic GetObjectTaggingOutput _
instance showGetObjectTaggingOutput :: Show GetObjectTaggingOutput where
  show = genericShow
instance decodeGetObjectTaggingOutput :: Decode GetObjectTaggingOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetObjectTaggingOutput :: Encode GetObjectTaggingOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetObjectTaggingRequest = GetObjectTaggingRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  }
derive instance newtypeGetObjectTaggingRequest :: Newtype GetObjectTaggingRequest _
derive instance repGenericGetObjectTaggingRequest :: Generic GetObjectTaggingRequest _
instance showGetObjectTaggingRequest :: Show GetObjectTaggingRequest where
  show = genericShow
instance decodeGetObjectTaggingRequest :: Decode GetObjectTaggingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetObjectTaggingRequest :: Encode GetObjectTaggingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetObjectTorrentOutput = GetObjectTorrentOutput 
  { "Body" :: NullOrUndefined.NullOrUndefined (Body)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  }
derive instance newtypeGetObjectTorrentOutput :: Newtype GetObjectTorrentOutput _
derive instance repGenericGetObjectTorrentOutput :: Generic GetObjectTorrentOutput _
instance showGetObjectTorrentOutput :: Show GetObjectTorrentOutput where
  show = genericShow
instance decodeGetObjectTorrentOutput :: Decode GetObjectTorrentOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetObjectTorrentOutput :: Encode GetObjectTorrentOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetObjectTorrentRequest = GetObjectTorrentRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  }
derive instance newtypeGetObjectTorrentRequest :: Newtype GetObjectTorrentRequest _
derive instance repGenericGetObjectTorrentRequest :: Generic GetObjectTorrentRequest _
instance showGetObjectTorrentRequest :: Show GetObjectTorrentRequest where
  show = genericShow
instance decodeGetObjectTorrentRequest :: Decode GetObjectTorrentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetObjectTorrentRequest :: Encode GetObjectTorrentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GlacierJobParameters = GlacierJobParameters 
  { "Tier" :: (Tier)
  }
derive instance newtypeGlacierJobParameters :: Newtype GlacierJobParameters _
derive instance repGenericGlacierJobParameters :: Generic GlacierJobParameters _
instance showGlacierJobParameters :: Show GlacierJobParameters where
  show = genericShow
instance decodeGlacierJobParameters :: Decode GlacierJobParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGlacierJobParameters :: Encode GlacierJobParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Grant = Grant 
  { "Grantee" :: NullOrUndefined.NullOrUndefined (Grantee)
  , "Permission" :: NullOrUndefined.NullOrUndefined (Permission)
  }
derive instance newtypeGrant :: Newtype Grant _
derive instance repGenericGrant :: Generic Grant _
instance showGrant :: Show Grant where
  show = genericShow
instance decodeGrant :: Decode Grant where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrant :: Encode Grant where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrantFullControl = GrantFullControl String
derive instance newtypeGrantFullControl :: Newtype GrantFullControl _
derive instance repGenericGrantFullControl :: Generic GrantFullControl _
instance showGrantFullControl :: Show GrantFullControl where
  show = genericShow
instance decodeGrantFullControl :: Decode GrantFullControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantFullControl :: Encode GrantFullControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrantRead = GrantRead String
derive instance newtypeGrantRead :: Newtype GrantRead _
derive instance repGenericGrantRead :: Generic GrantRead _
instance showGrantRead :: Show GrantRead where
  show = genericShow
instance decodeGrantRead :: Decode GrantRead where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantRead :: Encode GrantRead where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrantReadACP = GrantReadACP String
derive instance newtypeGrantReadACP :: Newtype GrantReadACP _
derive instance repGenericGrantReadACP :: Generic GrantReadACP _
instance showGrantReadACP :: Show GrantReadACP where
  show = genericShow
instance decodeGrantReadACP :: Decode GrantReadACP where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantReadACP :: Encode GrantReadACP where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrantWrite = GrantWrite String
derive instance newtypeGrantWrite :: Newtype GrantWrite _
derive instance repGenericGrantWrite :: Generic GrantWrite _
instance showGrantWrite :: Show GrantWrite where
  show = genericShow
instance decodeGrantWrite :: Decode GrantWrite where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantWrite :: Encode GrantWrite where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrantWriteACP = GrantWriteACP String
derive instance newtypeGrantWriteACP :: Newtype GrantWriteACP _
derive instance repGenericGrantWriteACP :: Generic GrantWriteACP _
instance showGrantWriteACP :: Show GrantWriteACP where
  show = genericShow
instance decodeGrantWriteACP :: Decode GrantWriteACP where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantWriteACP :: Encode GrantWriteACP where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Grantee = Grantee 
  { "DisplayName" :: NullOrUndefined.NullOrUndefined (DisplayName)
  , "EmailAddress" :: NullOrUndefined.NullOrUndefined (EmailAddress)
  , "ID" :: NullOrUndefined.NullOrUndefined (ID)
  , "Type" :: (Type)
  , "URI" :: NullOrUndefined.NullOrUndefined (URI)
  }
derive instance newtypeGrantee :: Newtype Grantee _
derive instance repGenericGrantee :: Generic Grantee _
instance showGrantee :: Show Grantee where
  show = genericShow
instance decodeGrantee :: Decode Grantee where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantee :: Encode Grantee where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Grants = Grants (Array Grant)
derive instance newtypeGrants :: Newtype Grants _
derive instance repGenericGrants :: Generic Grants _
instance showGrants :: Show Grants where
  show = genericShow
instance decodeGrants :: Decode Grants where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrants :: Encode Grants where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HeadBucketRequest = HeadBucketRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeHeadBucketRequest :: Newtype HeadBucketRequest _
derive instance repGenericHeadBucketRequest :: Generic HeadBucketRequest _
instance showHeadBucketRequest :: Show HeadBucketRequest where
  show = genericShow
instance decodeHeadBucketRequest :: Decode HeadBucketRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHeadBucketRequest :: Encode HeadBucketRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HeadObjectOutput = HeadObjectOutput 
  { "DeleteMarker" :: NullOrUndefined.NullOrUndefined (DeleteMarker)
  , "AcceptRanges" :: NullOrUndefined.NullOrUndefined (AcceptRanges)
  , "Expiration" :: NullOrUndefined.NullOrUndefined (Expiration)
  , "Restore" :: NullOrUndefined.NullOrUndefined (Restore)
  , "LastModified" :: NullOrUndefined.NullOrUndefined (LastModified)
  , "ContentLength" :: NullOrUndefined.NullOrUndefined (ContentLength)
  , "ETag" :: NullOrUndefined.NullOrUndefined (ETag)
  , "MissingMeta" :: NullOrUndefined.NullOrUndefined (MissingMeta)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "CacheControl" :: NullOrUndefined.NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined.NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined.NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined.NullOrUndefined (ContentLanguage)
  , "ContentType" :: NullOrUndefined.NullOrUndefined (ContentType)
  , "Expires" :: NullOrUndefined.NullOrUndefined (Expires)
  , "WebsiteRedirectLocation" :: NullOrUndefined.NullOrUndefined (WebsiteRedirectLocation)
  , "ServerSideEncryption" :: NullOrUndefined.NullOrUndefined (ServerSideEncryption)
  , "Metadata" :: NullOrUndefined.NullOrUndefined (Metadata)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  , "ReplicationStatus" :: NullOrUndefined.NullOrUndefined (ReplicationStatus)
  , "PartsCount" :: NullOrUndefined.NullOrUndefined (PartsCount)
  }
derive instance newtypeHeadObjectOutput :: Newtype HeadObjectOutput _
derive instance repGenericHeadObjectOutput :: Generic HeadObjectOutput _
instance showHeadObjectOutput :: Show HeadObjectOutput where
  show = genericShow
instance decodeHeadObjectOutput :: Decode HeadObjectOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHeadObjectOutput :: Encode HeadObjectOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HeadObjectRequest = HeadObjectRequest 
  { "Bucket" :: (BucketName)
  , "IfMatch" :: NullOrUndefined.NullOrUndefined (IfMatch)
  , "IfModifiedSince" :: NullOrUndefined.NullOrUndefined (IfModifiedSince)
  , "IfNoneMatch" :: NullOrUndefined.NullOrUndefined (IfNoneMatch)
  , "IfUnmodifiedSince" :: NullOrUndefined.NullOrUndefined (IfUnmodifiedSince)
  , "Key" :: (ObjectKey)
  , "Range" :: NullOrUndefined.NullOrUndefined (Range)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined.NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  , "PartNumber" :: NullOrUndefined.NullOrUndefined (PartNumber)
  }
derive instance newtypeHeadObjectRequest :: Newtype HeadObjectRequest _
derive instance repGenericHeadObjectRequest :: Generic HeadObjectRequest _
instance showHeadObjectRequest :: Show HeadObjectRequest where
  show = genericShow
instance decodeHeadObjectRequest :: Decode HeadObjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHeadObjectRequest :: Encode HeadObjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HostName = HostName String
derive instance newtypeHostName :: Newtype HostName _
derive instance repGenericHostName :: Generic HostName _
instance showHostName :: Show HostName where
  show = genericShow
instance decodeHostName :: Decode HostName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostName :: Encode HostName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HttpErrorCodeReturnedEquals = HttpErrorCodeReturnedEquals String
derive instance newtypeHttpErrorCodeReturnedEquals :: Newtype HttpErrorCodeReturnedEquals _
derive instance repGenericHttpErrorCodeReturnedEquals :: Generic HttpErrorCodeReturnedEquals _
instance showHttpErrorCodeReturnedEquals :: Show HttpErrorCodeReturnedEquals where
  show = genericShow
instance decodeHttpErrorCodeReturnedEquals :: Decode HttpErrorCodeReturnedEquals where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHttpErrorCodeReturnedEquals :: Encode HttpErrorCodeReturnedEquals where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HttpRedirectCode = HttpRedirectCode String
derive instance newtypeHttpRedirectCode :: Newtype HttpRedirectCode _
derive instance repGenericHttpRedirectCode :: Generic HttpRedirectCode _
instance showHttpRedirectCode :: Show HttpRedirectCode where
  show = genericShow
instance decodeHttpRedirectCode :: Decode HttpRedirectCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHttpRedirectCode :: Encode HttpRedirectCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ID = ID String
derive instance newtypeID :: Newtype ID _
derive instance repGenericID :: Generic ID _
instance showID :: Show ID where
  show = genericShow
instance decodeID :: Decode ID where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeID :: Encode ID where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IfMatch = IfMatch String
derive instance newtypeIfMatch :: Newtype IfMatch _
derive instance repGenericIfMatch :: Generic IfMatch _
instance showIfMatch :: Show IfMatch where
  show = genericShow
instance decodeIfMatch :: Decode IfMatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIfMatch :: Encode IfMatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IfModifiedSince = IfModifiedSince Number
derive instance newtypeIfModifiedSince :: Newtype IfModifiedSince _
derive instance repGenericIfModifiedSince :: Generic IfModifiedSince _
instance showIfModifiedSince :: Show IfModifiedSince where
  show = genericShow
instance decodeIfModifiedSince :: Decode IfModifiedSince where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIfModifiedSince :: Encode IfModifiedSince where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IfNoneMatch = IfNoneMatch String
derive instance newtypeIfNoneMatch :: Newtype IfNoneMatch _
derive instance repGenericIfNoneMatch :: Generic IfNoneMatch _
instance showIfNoneMatch :: Show IfNoneMatch where
  show = genericShow
instance decodeIfNoneMatch :: Decode IfNoneMatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIfNoneMatch :: Encode IfNoneMatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IfUnmodifiedSince = IfUnmodifiedSince Number
derive instance newtypeIfUnmodifiedSince :: Newtype IfUnmodifiedSince _
derive instance repGenericIfUnmodifiedSince :: Generic IfUnmodifiedSince _
instance showIfUnmodifiedSince :: Show IfUnmodifiedSince where
  show = genericShow
instance decodeIfUnmodifiedSince :: Decode IfUnmodifiedSince where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIfUnmodifiedSince :: Encode IfUnmodifiedSince where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IndexDocument = IndexDocument 
  { "Suffix" :: (Suffix)
  }
derive instance newtypeIndexDocument :: Newtype IndexDocument _
derive instance repGenericIndexDocument :: Generic IndexDocument _
instance showIndexDocument :: Show IndexDocument where
  show = genericShow
instance decodeIndexDocument :: Decode IndexDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexDocument :: Encode IndexDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Initiated = Initiated Number
derive instance newtypeInitiated :: Newtype Initiated _
derive instance repGenericInitiated :: Generic Initiated _
instance showInitiated :: Show Initiated where
  show = genericShow
instance decodeInitiated :: Decode Initiated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiated :: Encode Initiated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Initiator = Initiator 
  { "ID" :: NullOrUndefined.NullOrUndefined (ID)
  , "DisplayName" :: NullOrUndefined.NullOrUndefined (DisplayName)
  }
derive instance newtypeInitiator :: Newtype Initiator _
derive instance repGenericInitiator :: Generic Initiator _
instance showInitiator :: Show Initiator where
  show = genericShow
instance decodeInitiator :: Decode Initiator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiator :: Encode Initiator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Describes the serialization format of the object.
newtype InputSerialization = InputSerialization 
  { "CSV" :: NullOrUndefined.NullOrUndefined (CSVInput)
  }
derive instance newtypeInputSerialization :: Newtype InputSerialization _
derive instance repGenericInputSerialization :: Generic InputSerialization _
instance showInputSerialization :: Show InputSerialization where
  show = genericShow
instance decodeInputSerialization :: Decode InputSerialization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputSerialization :: Encode InputSerialization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryConfiguration = InventoryConfiguration 
  { "Destination" :: (InventoryDestination)
  , "IsEnabled" :: (IsEnabled)
  , "Filter" :: NullOrUndefined.NullOrUndefined (InventoryFilter)
  , "Id" :: (InventoryId)
  , "IncludedObjectVersions" :: (InventoryIncludedObjectVersions)
  , "OptionalFields" :: NullOrUndefined.NullOrUndefined (InventoryOptionalFields)
  , "Schedule" :: (InventorySchedule)
  }
derive instance newtypeInventoryConfiguration :: Newtype InventoryConfiguration _
derive instance repGenericInventoryConfiguration :: Generic InventoryConfiguration _
instance showInventoryConfiguration :: Show InventoryConfiguration where
  show = genericShow
instance decodeInventoryConfiguration :: Decode InventoryConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryConfiguration :: Encode InventoryConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryConfigurationList = InventoryConfigurationList (Array InventoryConfiguration)
derive instance newtypeInventoryConfigurationList :: Newtype InventoryConfigurationList _
derive instance repGenericInventoryConfigurationList :: Generic InventoryConfigurationList _
instance showInventoryConfigurationList :: Show InventoryConfigurationList where
  show = genericShow
instance decodeInventoryConfigurationList :: Decode InventoryConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryConfigurationList :: Encode InventoryConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryDestination = InventoryDestination 
  { "S3BucketDestination" :: (InventoryS3BucketDestination)
  }
derive instance newtypeInventoryDestination :: Newtype InventoryDestination _
derive instance repGenericInventoryDestination :: Generic InventoryDestination _
instance showInventoryDestination :: Show InventoryDestination where
  show = genericShow
instance decodeInventoryDestination :: Decode InventoryDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryDestination :: Encode InventoryDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Contains the type of server-side encryption used to encrypt the inventory results.
newtype InventoryEncryption = InventoryEncryption 
  { "SSES3" :: NullOrUndefined.NullOrUndefined (SSES3)
  , "SSEKMS" :: NullOrUndefined.NullOrUndefined (SSEKMS)
  }
derive instance newtypeInventoryEncryption :: Newtype InventoryEncryption _
derive instance repGenericInventoryEncryption :: Generic InventoryEncryption _
instance showInventoryEncryption :: Show InventoryEncryption where
  show = genericShow
instance decodeInventoryEncryption :: Decode InventoryEncryption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryEncryption :: Encode InventoryEncryption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryFilter = InventoryFilter 
  { "Prefix" :: (Prefix)
  }
derive instance newtypeInventoryFilter :: Newtype InventoryFilter _
derive instance repGenericInventoryFilter :: Generic InventoryFilter _
instance showInventoryFilter :: Show InventoryFilter where
  show = genericShow
instance decodeInventoryFilter :: Decode InventoryFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryFilter :: Encode InventoryFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryFormat = InventoryFormat String
derive instance newtypeInventoryFormat :: Newtype InventoryFormat _
derive instance repGenericInventoryFormat :: Generic InventoryFormat _
instance showInventoryFormat :: Show InventoryFormat where
  show = genericShow
instance decodeInventoryFormat :: Decode InventoryFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryFormat :: Encode InventoryFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryFrequency = InventoryFrequency String
derive instance newtypeInventoryFrequency :: Newtype InventoryFrequency _
derive instance repGenericInventoryFrequency :: Generic InventoryFrequency _
instance showInventoryFrequency :: Show InventoryFrequency where
  show = genericShow
instance decodeInventoryFrequency :: Decode InventoryFrequency where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryFrequency :: Encode InventoryFrequency where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryId = InventoryId String
derive instance newtypeInventoryId :: Newtype InventoryId _
derive instance repGenericInventoryId :: Generic InventoryId _
instance showInventoryId :: Show InventoryId where
  show = genericShow
instance decodeInventoryId :: Decode InventoryId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryId :: Encode InventoryId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryIncludedObjectVersions = InventoryIncludedObjectVersions String
derive instance newtypeInventoryIncludedObjectVersions :: Newtype InventoryIncludedObjectVersions _
derive instance repGenericInventoryIncludedObjectVersions :: Generic InventoryIncludedObjectVersions _
instance showInventoryIncludedObjectVersions :: Show InventoryIncludedObjectVersions where
  show = genericShow
instance decodeInventoryIncludedObjectVersions :: Decode InventoryIncludedObjectVersions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryIncludedObjectVersions :: Encode InventoryIncludedObjectVersions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryOptionalField = InventoryOptionalField String
derive instance newtypeInventoryOptionalField :: Newtype InventoryOptionalField _
derive instance repGenericInventoryOptionalField :: Generic InventoryOptionalField _
instance showInventoryOptionalField :: Show InventoryOptionalField where
  show = genericShow
instance decodeInventoryOptionalField :: Decode InventoryOptionalField where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryOptionalField :: Encode InventoryOptionalField where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryOptionalFields = InventoryOptionalFields (Array InventoryOptionalField)
derive instance newtypeInventoryOptionalFields :: Newtype InventoryOptionalFields _
derive instance repGenericInventoryOptionalFields :: Generic InventoryOptionalFields _
instance showInventoryOptionalFields :: Show InventoryOptionalFields where
  show = genericShow
instance decodeInventoryOptionalFields :: Decode InventoryOptionalFields where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryOptionalFields :: Encode InventoryOptionalFields where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryS3BucketDestination = InventoryS3BucketDestination 
  { "AccountId" :: NullOrUndefined.NullOrUndefined (AccountId)
  , "Bucket" :: (BucketName)
  , "Format" :: (InventoryFormat)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (InventoryEncryption)
  }
derive instance newtypeInventoryS3BucketDestination :: Newtype InventoryS3BucketDestination _
derive instance repGenericInventoryS3BucketDestination :: Generic InventoryS3BucketDestination _
instance showInventoryS3BucketDestination :: Show InventoryS3BucketDestination where
  show = genericShow
instance decodeInventoryS3BucketDestination :: Decode InventoryS3BucketDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryS3BucketDestination :: Encode InventoryS3BucketDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventorySchedule = InventorySchedule 
  { "Frequency" :: (InventoryFrequency)
  }
derive instance newtypeInventorySchedule :: Newtype InventorySchedule _
derive instance repGenericInventorySchedule :: Generic InventorySchedule _
instance showInventorySchedule :: Show InventorySchedule where
  show = genericShow
instance decodeInventorySchedule :: Decode InventorySchedule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventorySchedule :: Encode InventorySchedule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsEnabled = IsEnabled Boolean
derive instance newtypeIsEnabled :: Newtype IsEnabled _
derive instance repGenericIsEnabled :: Generic IsEnabled _
instance showIsEnabled :: Show IsEnabled where
  show = genericShow
instance decodeIsEnabled :: Decode IsEnabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsEnabled :: Encode IsEnabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsLatest = IsLatest Boolean
derive instance newtypeIsLatest :: Newtype IsLatest _
derive instance repGenericIsLatest :: Generic IsLatest _
instance showIsLatest :: Show IsLatest where
  show = genericShow
instance decodeIsLatest :: Decode IsLatest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsLatest :: Encode IsLatest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsTruncated = IsTruncated Boolean
derive instance newtypeIsTruncated :: Newtype IsTruncated _
derive instance repGenericIsTruncated :: Generic IsTruncated _
instance showIsTruncated :: Show IsTruncated where
  show = genericShow
instance decodeIsTruncated :: Decode IsTruncated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsTruncated :: Encode IsTruncated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KMSContext = KMSContext String
derive instance newtypeKMSContext :: Newtype KMSContext _
derive instance repGenericKMSContext :: Generic KMSContext _
instance showKMSContext :: Show KMSContext where
  show = genericShow
instance decodeKMSContext :: Decode KMSContext where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKMSContext :: Encode KMSContext where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyCount = KeyCount Int
derive instance newtypeKeyCount :: Newtype KeyCount _
derive instance repGenericKeyCount :: Generic KeyCount _
instance showKeyCount :: Show KeyCount where
  show = genericShow
instance decodeKeyCount :: Decode KeyCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyCount :: Encode KeyCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyMarker = KeyMarker String
derive instance newtypeKeyMarker :: Newtype KeyMarker _
derive instance repGenericKeyMarker :: Generic KeyMarker _
instance showKeyMarker :: Show KeyMarker where
  show = genericShow
instance decodeKeyMarker :: Decode KeyMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyMarker :: Encode KeyMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyPrefixEquals = KeyPrefixEquals String
derive instance newtypeKeyPrefixEquals :: Newtype KeyPrefixEquals _
derive instance repGenericKeyPrefixEquals :: Generic KeyPrefixEquals _
instance showKeyPrefixEquals :: Show KeyPrefixEquals where
  show = genericShow
instance decodeKeyPrefixEquals :: Decode KeyPrefixEquals where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyPrefixEquals :: Encode KeyPrefixEquals where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LambdaFunctionArn = LambdaFunctionArn String
derive instance newtypeLambdaFunctionArn :: Newtype LambdaFunctionArn _
derive instance repGenericLambdaFunctionArn :: Generic LambdaFunctionArn _
instance showLambdaFunctionArn :: Show LambdaFunctionArn where
  show = genericShow
instance decodeLambdaFunctionArn :: Decode LambdaFunctionArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionArn :: Encode LambdaFunctionArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for specifying the AWS Lambda notification configuration.
newtype LambdaFunctionConfiguration = LambdaFunctionConfiguration 
  { "Id" :: NullOrUndefined.NullOrUndefined (NotificationId)
  , "LambdaFunctionArn" :: (LambdaFunctionArn)
  , "Events" :: (EventList)
  , "Filter" :: NullOrUndefined.NullOrUndefined (NotificationConfigurationFilter)
  }
derive instance newtypeLambdaFunctionConfiguration :: Newtype LambdaFunctionConfiguration _
derive instance repGenericLambdaFunctionConfiguration :: Generic LambdaFunctionConfiguration _
instance showLambdaFunctionConfiguration :: Show LambdaFunctionConfiguration where
  show = genericShow
instance decodeLambdaFunctionConfiguration :: Decode LambdaFunctionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionConfiguration :: Encode LambdaFunctionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LambdaFunctionConfigurationList = LambdaFunctionConfigurationList (Array LambdaFunctionConfiguration)
derive instance newtypeLambdaFunctionConfigurationList :: Newtype LambdaFunctionConfigurationList _
derive instance repGenericLambdaFunctionConfigurationList :: Generic LambdaFunctionConfigurationList _
instance showLambdaFunctionConfigurationList :: Show LambdaFunctionConfigurationList where
  show = genericShow
instance decodeLambdaFunctionConfigurationList :: Decode LambdaFunctionConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionConfigurationList :: Encode LambdaFunctionConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastModified = LastModified Number
derive instance newtypeLastModified :: Newtype LastModified _
derive instance repGenericLastModified :: Generic LastModified _
instance showLastModified :: Show LastModified where
  show = genericShow
instance decodeLastModified :: Decode LastModified where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastModified :: Encode LastModified where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LifecycleConfiguration = LifecycleConfiguration 
  { "Rules" :: (Rules)
  }
derive instance newtypeLifecycleConfiguration :: Newtype LifecycleConfiguration _
derive instance repGenericLifecycleConfiguration :: Generic LifecycleConfiguration _
instance showLifecycleConfiguration :: Show LifecycleConfiguration where
  show = genericShow
instance decodeLifecycleConfiguration :: Decode LifecycleConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLifecycleConfiguration :: Encode LifecycleConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LifecycleExpiration = LifecycleExpiration 
  { "Date" :: NullOrUndefined.NullOrUndefined (Date)
  , "Days" :: NullOrUndefined.NullOrUndefined (Days)
  , "ExpiredObjectDeleteMarker" :: NullOrUndefined.NullOrUndefined (ExpiredObjectDeleteMarker)
  }
derive instance newtypeLifecycleExpiration :: Newtype LifecycleExpiration _
derive instance repGenericLifecycleExpiration :: Generic LifecycleExpiration _
instance showLifecycleExpiration :: Show LifecycleExpiration where
  show = genericShow
instance decodeLifecycleExpiration :: Decode LifecycleExpiration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLifecycleExpiration :: Encode LifecycleExpiration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LifecycleRule = LifecycleRule 
  { "Expiration" :: NullOrUndefined.NullOrUndefined (LifecycleExpiration)
  , "ID" :: NullOrUndefined.NullOrUndefined (ID)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "Filter" :: NullOrUndefined.NullOrUndefined (LifecycleRuleFilter)
  , "Status" :: (ExpirationStatus)
  , "Transitions" :: NullOrUndefined.NullOrUndefined (TransitionList)
  , "NoncurrentVersionTransitions" :: NullOrUndefined.NullOrUndefined (NoncurrentVersionTransitionList)
  , "NoncurrentVersionExpiration" :: NullOrUndefined.NullOrUndefined (NoncurrentVersionExpiration)
  , "AbortIncompleteMultipartUpload" :: NullOrUndefined.NullOrUndefined (AbortIncompleteMultipartUpload)
  }
derive instance newtypeLifecycleRule :: Newtype LifecycleRule _
derive instance repGenericLifecycleRule :: Generic LifecycleRule _
instance showLifecycleRule :: Show LifecycleRule where
  show = genericShow
instance decodeLifecycleRule :: Decode LifecycleRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLifecycleRule :: Encode LifecycleRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This is used in a Lifecycle Rule Filter to apply a logical AND to two or more predicates. The Lifecycle Rule will apply to any object matching all of the predicates configured inside the And operator.
newtype LifecycleRuleAndOperator = LifecycleRuleAndOperator 
  { "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagSet)
  }
derive instance newtypeLifecycleRuleAndOperator :: Newtype LifecycleRuleAndOperator _
derive instance repGenericLifecycleRuleAndOperator :: Generic LifecycleRuleAndOperator _
instance showLifecycleRuleAndOperator :: Show LifecycleRuleAndOperator where
  show = genericShow
instance decodeLifecycleRuleAndOperator :: Decode LifecycleRuleAndOperator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLifecycleRuleAndOperator :: Encode LifecycleRuleAndOperator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The Filter is used to identify objects that a Lifecycle Rule applies to. A Filter must have exactly one of Prefix, Tag, or And specified.
newtype LifecycleRuleFilter = LifecycleRuleFilter 
  { "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "Tag" :: NullOrUndefined.NullOrUndefined (Tag)
  , "And" :: NullOrUndefined.NullOrUndefined (LifecycleRuleAndOperator)
  }
derive instance newtypeLifecycleRuleFilter :: Newtype LifecycleRuleFilter _
derive instance repGenericLifecycleRuleFilter :: Generic LifecycleRuleFilter _
instance showLifecycleRuleFilter :: Show LifecycleRuleFilter where
  show = genericShow
instance decodeLifecycleRuleFilter :: Decode LifecycleRuleFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLifecycleRuleFilter :: Encode LifecycleRuleFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LifecycleRules = LifecycleRules (Array LifecycleRule)
derive instance newtypeLifecycleRules :: Newtype LifecycleRules _
derive instance repGenericLifecycleRules :: Generic LifecycleRules _
instance showLifecycleRules :: Show LifecycleRules where
  show = genericShow
instance decodeLifecycleRules :: Decode LifecycleRules where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLifecycleRules :: Encode LifecycleRules where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListBucketAnalyticsConfigurationsOutput = ListBucketAnalyticsConfigurationsOutput 
  { "IsTruncated" :: NullOrUndefined.NullOrUndefined (IsTruncated)
  , "ContinuationToken" :: NullOrUndefined.NullOrUndefined (Token)
  , "NextContinuationToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "AnalyticsConfigurationList" :: NullOrUndefined.NullOrUndefined (AnalyticsConfigurationList)
  }
derive instance newtypeListBucketAnalyticsConfigurationsOutput :: Newtype ListBucketAnalyticsConfigurationsOutput _
derive instance repGenericListBucketAnalyticsConfigurationsOutput :: Generic ListBucketAnalyticsConfigurationsOutput _
instance showListBucketAnalyticsConfigurationsOutput :: Show ListBucketAnalyticsConfigurationsOutput where
  show = genericShow
instance decodeListBucketAnalyticsConfigurationsOutput :: Decode ListBucketAnalyticsConfigurationsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBucketAnalyticsConfigurationsOutput :: Encode ListBucketAnalyticsConfigurationsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListBucketAnalyticsConfigurationsRequest = ListBucketAnalyticsConfigurationsRequest 
  { "Bucket" :: (BucketName)
  , "ContinuationToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeListBucketAnalyticsConfigurationsRequest :: Newtype ListBucketAnalyticsConfigurationsRequest _
derive instance repGenericListBucketAnalyticsConfigurationsRequest :: Generic ListBucketAnalyticsConfigurationsRequest _
instance showListBucketAnalyticsConfigurationsRequest :: Show ListBucketAnalyticsConfigurationsRequest where
  show = genericShow
instance decodeListBucketAnalyticsConfigurationsRequest :: Decode ListBucketAnalyticsConfigurationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBucketAnalyticsConfigurationsRequest :: Encode ListBucketAnalyticsConfigurationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListBucketInventoryConfigurationsOutput = ListBucketInventoryConfigurationsOutput 
  { "ContinuationToken" :: NullOrUndefined.NullOrUndefined (Token)
  , "InventoryConfigurationList" :: NullOrUndefined.NullOrUndefined (InventoryConfigurationList)
  , "IsTruncated" :: NullOrUndefined.NullOrUndefined (IsTruncated)
  , "NextContinuationToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListBucketInventoryConfigurationsOutput :: Newtype ListBucketInventoryConfigurationsOutput _
derive instance repGenericListBucketInventoryConfigurationsOutput :: Generic ListBucketInventoryConfigurationsOutput _
instance showListBucketInventoryConfigurationsOutput :: Show ListBucketInventoryConfigurationsOutput where
  show = genericShow
instance decodeListBucketInventoryConfigurationsOutput :: Decode ListBucketInventoryConfigurationsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBucketInventoryConfigurationsOutput :: Encode ListBucketInventoryConfigurationsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListBucketInventoryConfigurationsRequest = ListBucketInventoryConfigurationsRequest 
  { "Bucket" :: (BucketName)
  , "ContinuationToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeListBucketInventoryConfigurationsRequest :: Newtype ListBucketInventoryConfigurationsRequest _
derive instance repGenericListBucketInventoryConfigurationsRequest :: Generic ListBucketInventoryConfigurationsRequest _
instance showListBucketInventoryConfigurationsRequest :: Show ListBucketInventoryConfigurationsRequest where
  show = genericShow
instance decodeListBucketInventoryConfigurationsRequest :: Decode ListBucketInventoryConfigurationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBucketInventoryConfigurationsRequest :: Encode ListBucketInventoryConfigurationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListBucketMetricsConfigurationsOutput = ListBucketMetricsConfigurationsOutput 
  { "IsTruncated" :: NullOrUndefined.NullOrUndefined (IsTruncated)
  , "ContinuationToken" :: NullOrUndefined.NullOrUndefined (Token)
  , "NextContinuationToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MetricsConfigurationList" :: NullOrUndefined.NullOrUndefined (MetricsConfigurationList)
  }
derive instance newtypeListBucketMetricsConfigurationsOutput :: Newtype ListBucketMetricsConfigurationsOutput _
derive instance repGenericListBucketMetricsConfigurationsOutput :: Generic ListBucketMetricsConfigurationsOutput _
instance showListBucketMetricsConfigurationsOutput :: Show ListBucketMetricsConfigurationsOutput where
  show = genericShow
instance decodeListBucketMetricsConfigurationsOutput :: Decode ListBucketMetricsConfigurationsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBucketMetricsConfigurationsOutput :: Encode ListBucketMetricsConfigurationsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListBucketMetricsConfigurationsRequest = ListBucketMetricsConfigurationsRequest 
  { "Bucket" :: (BucketName)
  , "ContinuationToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeListBucketMetricsConfigurationsRequest :: Newtype ListBucketMetricsConfigurationsRequest _
derive instance repGenericListBucketMetricsConfigurationsRequest :: Generic ListBucketMetricsConfigurationsRequest _
instance showListBucketMetricsConfigurationsRequest :: Show ListBucketMetricsConfigurationsRequest where
  show = genericShow
instance decodeListBucketMetricsConfigurationsRequest :: Decode ListBucketMetricsConfigurationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBucketMetricsConfigurationsRequest :: Encode ListBucketMetricsConfigurationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListBucketsOutput = ListBucketsOutput 
  { "Buckets" :: NullOrUndefined.NullOrUndefined (Buckets)
  , "Owner" :: NullOrUndefined.NullOrUndefined (Owner)
  }
derive instance newtypeListBucketsOutput :: Newtype ListBucketsOutput _
derive instance repGenericListBucketsOutput :: Generic ListBucketsOutput _
instance showListBucketsOutput :: Show ListBucketsOutput where
  show = genericShow
instance decodeListBucketsOutput :: Decode ListBucketsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBucketsOutput :: Encode ListBucketsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListMultipartUploadsOutput = ListMultipartUploadsOutput 
  { "Bucket" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "KeyMarker" :: NullOrUndefined.NullOrUndefined (KeyMarker)
  , "UploadIdMarker" :: NullOrUndefined.NullOrUndefined (UploadIdMarker)
  , "NextKeyMarker" :: NullOrUndefined.NullOrUndefined (NextKeyMarker)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "Delimiter" :: NullOrUndefined.NullOrUndefined (Delimiter)
  , "NextUploadIdMarker" :: NullOrUndefined.NullOrUndefined (NextUploadIdMarker)
  , "MaxUploads" :: NullOrUndefined.NullOrUndefined (MaxUploads)
  , "IsTruncated" :: NullOrUndefined.NullOrUndefined (IsTruncated)
  , "Uploads" :: NullOrUndefined.NullOrUndefined (MultipartUploadList)
  , "CommonPrefixes" :: NullOrUndefined.NullOrUndefined (CommonPrefixList)
  , "EncodingType" :: NullOrUndefined.NullOrUndefined (EncodingType)
  }
derive instance newtypeListMultipartUploadsOutput :: Newtype ListMultipartUploadsOutput _
derive instance repGenericListMultipartUploadsOutput :: Generic ListMultipartUploadsOutput _
instance showListMultipartUploadsOutput :: Show ListMultipartUploadsOutput where
  show = genericShow
instance decodeListMultipartUploadsOutput :: Decode ListMultipartUploadsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListMultipartUploadsOutput :: Encode ListMultipartUploadsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListMultipartUploadsRequest = ListMultipartUploadsRequest 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined.NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined.NullOrUndefined (EncodingType)
  , "KeyMarker" :: NullOrUndefined.NullOrUndefined (KeyMarker)
  , "MaxUploads" :: NullOrUndefined.NullOrUndefined (MaxUploads)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "UploadIdMarker" :: NullOrUndefined.NullOrUndefined (UploadIdMarker)
  }
derive instance newtypeListMultipartUploadsRequest :: Newtype ListMultipartUploadsRequest _
derive instance repGenericListMultipartUploadsRequest :: Generic ListMultipartUploadsRequest _
instance showListMultipartUploadsRequest :: Show ListMultipartUploadsRequest where
  show = genericShow
instance decodeListMultipartUploadsRequest :: Decode ListMultipartUploadsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListMultipartUploadsRequest :: Encode ListMultipartUploadsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectVersionsOutput = ListObjectVersionsOutput 
  { "IsTruncated" :: NullOrUndefined.NullOrUndefined (IsTruncated)
  , "KeyMarker" :: NullOrUndefined.NullOrUndefined (KeyMarker)
  , "VersionIdMarker" :: NullOrUndefined.NullOrUndefined (VersionIdMarker)
  , "NextKeyMarker" :: NullOrUndefined.NullOrUndefined (NextKeyMarker)
  , "NextVersionIdMarker" :: NullOrUndefined.NullOrUndefined (NextVersionIdMarker)
  , "Versions" :: NullOrUndefined.NullOrUndefined (ObjectVersionList)
  , "DeleteMarkers" :: NullOrUndefined.NullOrUndefined (DeleteMarkers)
  , "Name" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "Delimiter" :: NullOrUndefined.NullOrUndefined (Delimiter)
  , "MaxKeys" :: NullOrUndefined.NullOrUndefined (MaxKeys)
  , "CommonPrefixes" :: NullOrUndefined.NullOrUndefined (CommonPrefixList)
  , "EncodingType" :: NullOrUndefined.NullOrUndefined (EncodingType)
  }
derive instance newtypeListObjectVersionsOutput :: Newtype ListObjectVersionsOutput _
derive instance repGenericListObjectVersionsOutput :: Generic ListObjectVersionsOutput _
instance showListObjectVersionsOutput :: Show ListObjectVersionsOutput where
  show = genericShow
instance decodeListObjectVersionsOutput :: Decode ListObjectVersionsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectVersionsOutput :: Encode ListObjectVersionsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectVersionsRequest = ListObjectVersionsRequest 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined.NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined.NullOrUndefined (EncodingType)
  , "KeyMarker" :: NullOrUndefined.NullOrUndefined (KeyMarker)
  , "MaxKeys" :: NullOrUndefined.NullOrUndefined (MaxKeys)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "VersionIdMarker" :: NullOrUndefined.NullOrUndefined (VersionIdMarker)
  }
derive instance newtypeListObjectVersionsRequest :: Newtype ListObjectVersionsRequest _
derive instance repGenericListObjectVersionsRequest :: Generic ListObjectVersionsRequest _
instance showListObjectVersionsRequest :: Show ListObjectVersionsRequest where
  show = genericShow
instance decodeListObjectVersionsRequest :: Decode ListObjectVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectVersionsRequest :: Encode ListObjectVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectsOutput = ListObjectsOutput 
  { "IsTruncated" :: NullOrUndefined.NullOrUndefined (IsTruncated)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Contents" :: NullOrUndefined.NullOrUndefined (ObjectList)
  , "Name" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "Delimiter" :: NullOrUndefined.NullOrUndefined (Delimiter)
  , "MaxKeys" :: NullOrUndefined.NullOrUndefined (MaxKeys)
  , "CommonPrefixes" :: NullOrUndefined.NullOrUndefined (CommonPrefixList)
  , "EncodingType" :: NullOrUndefined.NullOrUndefined (EncodingType)
  }
derive instance newtypeListObjectsOutput :: Newtype ListObjectsOutput _
derive instance repGenericListObjectsOutput :: Generic ListObjectsOutput _
instance showListObjectsOutput :: Show ListObjectsOutput where
  show = genericShow
instance decodeListObjectsOutput :: Decode ListObjectsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectsOutput :: Encode ListObjectsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectsRequest = ListObjectsRequest 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined.NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined.NullOrUndefined (EncodingType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "MaxKeys" :: NullOrUndefined.NullOrUndefined (MaxKeys)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  }
derive instance newtypeListObjectsRequest :: Newtype ListObjectsRequest _
derive instance repGenericListObjectsRequest :: Generic ListObjectsRequest _
instance showListObjectsRequest :: Show ListObjectsRequest where
  show = genericShow
instance decodeListObjectsRequest :: Decode ListObjectsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectsRequest :: Encode ListObjectsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectsV2Output = ListObjectsV2Output 
  { "IsTruncated" :: NullOrUndefined.NullOrUndefined (IsTruncated)
  , "Contents" :: NullOrUndefined.NullOrUndefined (ObjectList)
  , "Name" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "Delimiter" :: NullOrUndefined.NullOrUndefined (Delimiter)
  , "MaxKeys" :: NullOrUndefined.NullOrUndefined (MaxKeys)
  , "CommonPrefixes" :: NullOrUndefined.NullOrUndefined (CommonPrefixList)
  , "EncodingType" :: NullOrUndefined.NullOrUndefined (EncodingType)
  , "KeyCount" :: NullOrUndefined.NullOrUndefined (KeyCount)
  , "ContinuationToken" :: NullOrUndefined.NullOrUndefined (Token)
  , "NextContinuationToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "StartAfter" :: NullOrUndefined.NullOrUndefined (StartAfter)
  }
derive instance newtypeListObjectsV2Output :: Newtype ListObjectsV2Output _
derive instance repGenericListObjectsV2Output :: Generic ListObjectsV2Output _
instance showListObjectsV2Output :: Show ListObjectsV2Output where
  show = genericShow
instance decodeListObjectsV2Output :: Decode ListObjectsV2Output where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectsV2Output :: Encode ListObjectsV2Output where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectsV2Request = ListObjectsV2Request 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined.NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined.NullOrUndefined (EncodingType)
  , "MaxKeys" :: NullOrUndefined.NullOrUndefined (MaxKeys)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "ContinuationToken" :: NullOrUndefined.NullOrUndefined (Token)
  , "FetchOwner" :: NullOrUndefined.NullOrUndefined (FetchOwner)
  , "StartAfter" :: NullOrUndefined.NullOrUndefined (StartAfter)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  }
derive instance newtypeListObjectsV2Request :: Newtype ListObjectsV2Request _
derive instance repGenericListObjectsV2Request :: Generic ListObjectsV2Request _
instance showListObjectsV2Request :: Show ListObjectsV2Request where
  show = genericShow
instance decodeListObjectsV2Request :: Decode ListObjectsV2Request where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectsV2Request :: Encode ListObjectsV2Request where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListPartsOutput = ListPartsOutput 
  { "AbortDate" :: NullOrUndefined.NullOrUndefined (AbortDate)
  , "AbortRuleId" :: NullOrUndefined.NullOrUndefined (AbortRuleId)
  , "Bucket" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "Key" :: NullOrUndefined.NullOrUndefined (ObjectKey)
  , "UploadId" :: NullOrUndefined.NullOrUndefined (MultipartUploadId)
  , "PartNumberMarker" :: NullOrUndefined.NullOrUndefined (PartNumberMarker)
  , "NextPartNumberMarker" :: NullOrUndefined.NullOrUndefined (NextPartNumberMarker)
  , "MaxParts" :: NullOrUndefined.NullOrUndefined (MaxParts)
  , "IsTruncated" :: NullOrUndefined.NullOrUndefined (IsTruncated)
  , "Parts" :: NullOrUndefined.NullOrUndefined (Parts)
  , "Initiator" :: NullOrUndefined.NullOrUndefined (Initiator)
  , "Owner" :: NullOrUndefined.NullOrUndefined (Owner)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  }
derive instance newtypeListPartsOutput :: Newtype ListPartsOutput _
derive instance repGenericListPartsOutput :: Generic ListPartsOutput _
instance showListPartsOutput :: Show ListPartsOutput where
  show = genericShow
instance decodeListPartsOutput :: Decode ListPartsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPartsOutput :: Encode ListPartsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListPartsRequest = ListPartsRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "MaxParts" :: NullOrUndefined.NullOrUndefined (MaxParts)
  , "PartNumberMarker" :: NullOrUndefined.NullOrUndefined (PartNumberMarker)
  , "UploadId" :: (MultipartUploadId)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  }
derive instance newtypeListPartsRequest :: Newtype ListPartsRequest _
derive instance repGenericListPartsRequest :: Generic ListPartsRequest _
instance showListPartsRequest :: Show ListPartsRequest where
  show = genericShow
instance decodeListPartsRequest :: Decode ListPartsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPartsRequest :: Encode ListPartsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Location = Location String
derive instance newtypeLocation :: Newtype Location _
derive instance repGenericLocation :: Generic Location _
instance showLocation :: Show Location where
  show = genericShow
instance decodeLocation :: Decode Location where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocation :: Encode Location where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LocationPrefix = LocationPrefix String
derive instance newtypeLocationPrefix :: Newtype LocationPrefix _
derive instance repGenericLocationPrefix :: Generic LocationPrefix _
instance showLocationPrefix :: Show LocationPrefix where
  show = genericShow
instance decodeLocationPrefix :: Decode LocationPrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocationPrefix :: Encode LocationPrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoggingEnabled = LoggingEnabled 
  { "TargetBucket" :: NullOrUndefined.NullOrUndefined (TargetBucket)
  , "TargetGrants" :: NullOrUndefined.NullOrUndefined (TargetGrants)
  , "TargetPrefix" :: NullOrUndefined.NullOrUndefined (TargetPrefix)
  }
derive instance newtypeLoggingEnabled :: Newtype LoggingEnabled _
derive instance repGenericLoggingEnabled :: Generic LoggingEnabled _
instance showLoggingEnabled :: Show LoggingEnabled where
  show = genericShow
instance decodeLoggingEnabled :: Decode LoggingEnabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoggingEnabled :: Encode LoggingEnabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MFA = MFA String
derive instance newtypeMFA :: Newtype MFA _
derive instance repGenericMFA :: Generic MFA _
instance showMFA :: Show MFA where
  show = genericShow
instance decodeMFA :: Decode MFA where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMFA :: Encode MFA where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MFADelete = MFADelete String
derive instance newtypeMFADelete :: Newtype MFADelete _
derive instance repGenericMFADelete :: Generic MFADelete _
instance showMFADelete :: Show MFADelete where
  show = genericShow
instance decodeMFADelete :: Decode MFADelete where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMFADelete :: Encode MFADelete where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MFADeleteStatus = MFADeleteStatus String
derive instance newtypeMFADeleteStatus :: Newtype MFADeleteStatus _
derive instance repGenericMFADeleteStatus :: Generic MFADeleteStatus _
instance showMFADeleteStatus :: Show MFADeleteStatus where
  show = genericShow
instance decodeMFADeleteStatus :: Decode MFADeleteStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMFADeleteStatus :: Encode MFADeleteStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Marker = Marker String
derive instance newtypeMarker :: Newtype Marker _
derive instance repGenericMarker :: Generic Marker _
instance showMarker :: Show Marker where
  show = genericShow
instance decodeMarker :: Decode Marker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarker :: Encode Marker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxAgeSeconds = MaxAgeSeconds Int
derive instance newtypeMaxAgeSeconds :: Newtype MaxAgeSeconds _
derive instance repGenericMaxAgeSeconds :: Generic MaxAgeSeconds _
instance showMaxAgeSeconds :: Show MaxAgeSeconds where
  show = genericShow
instance decodeMaxAgeSeconds :: Decode MaxAgeSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxAgeSeconds :: Encode MaxAgeSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxKeys = MaxKeys Int
derive instance newtypeMaxKeys :: Newtype MaxKeys _
derive instance repGenericMaxKeys :: Generic MaxKeys _
instance showMaxKeys :: Show MaxKeys where
  show = genericShow
instance decodeMaxKeys :: Decode MaxKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxKeys :: Encode MaxKeys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxParts = MaxParts Int
derive instance newtypeMaxParts :: Newtype MaxParts _
derive instance repGenericMaxParts :: Generic MaxParts _
instance showMaxParts :: Show MaxParts where
  show = genericShow
instance decodeMaxParts :: Decode MaxParts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxParts :: Encode MaxParts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxUploads = MaxUploads Int
derive instance newtypeMaxUploads :: Newtype MaxUploads _
derive instance repGenericMaxUploads :: Generic MaxUploads _
instance showMaxUploads :: Show MaxUploads where
  show = genericShow
instance decodeMaxUploads :: Decode MaxUploads where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxUploads :: Encode MaxUploads where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Metadata = Metadata (StrMap.StrMap MetadataValue)
derive instance newtypeMetadata :: Newtype Metadata _
derive instance repGenericMetadata :: Generic Metadata _
instance showMetadata :: Show Metadata where
  show = genericShow
instance decodeMetadata :: Decode Metadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetadata :: Encode Metadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetadataDirective = MetadataDirective String
derive instance newtypeMetadataDirective :: Newtype MetadataDirective _
derive instance repGenericMetadataDirective :: Generic MetadataDirective _
instance showMetadataDirective :: Show MetadataDirective where
  show = genericShow
instance decodeMetadataDirective :: Decode MetadataDirective where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetadataDirective :: Encode MetadataDirective where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A metadata key-value pair to store with an object.
newtype MetadataEntry = MetadataEntry 
  { "Name" :: NullOrUndefined.NullOrUndefined (MetadataKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (MetadataValue)
  }
derive instance newtypeMetadataEntry :: Newtype MetadataEntry _
derive instance repGenericMetadataEntry :: Generic MetadataEntry _
instance showMetadataEntry :: Show MetadataEntry where
  show = genericShow
instance decodeMetadataEntry :: Decode MetadataEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetadataEntry :: Encode MetadataEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetadataKey = MetadataKey String
derive instance newtypeMetadataKey :: Newtype MetadataKey _
derive instance repGenericMetadataKey :: Generic MetadataKey _
instance showMetadataKey :: Show MetadataKey where
  show = genericShow
instance decodeMetadataKey :: Decode MetadataKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetadataKey :: Encode MetadataKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetadataValue = MetadataValue String
derive instance newtypeMetadataValue :: Newtype MetadataValue _
derive instance repGenericMetadataValue :: Generic MetadataValue _
instance showMetadataValue :: Show MetadataValue where
  show = genericShow
instance decodeMetadataValue :: Decode MetadataValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetadataValue :: Encode MetadataValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricsAndOperator = MetricsAndOperator 
  { "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagSet)
  }
derive instance newtypeMetricsAndOperator :: Newtype MetricsAndOperator _
derive instance repGenericMetricsAndOperator :: Generic MetricsAndOperator _
instance showMetricsAndOperator :: Show MetricsAndOperator where
  show = genericShow
instance decodeMetricsAndOperator :: Decode MetricsAndOperator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricsAndOperator :: Encode MetricsAndOperator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricsConfiguration = MetricsConfiguration 
  { "Id" :: (MetricsId)
  , "Filter" :: NullOrUndefined.NullOrUndefined (MetricsFilter)
  }
derive instance newtypeMetricsConfiguration :: Newtype MetricsConfiguration _
derive instance repGenericMetricsConfiguration :: Generic MetricsConfiguration _
instance showMetricsConfiguration :: Show MetricsConfiguration where
  show = genericShow
instance decodeMetricsConfiguration :: Decode MetricsConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricsConfiguration :: Encode MetricsConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricsConfigurationList = MetricsConfigurationList (Array MetricsConfiguration)
derive instance newtypeMetricsConfigurationList :: Newtype MetricsConfigurationList _
derive instance repGenericMetricsConfigurationList :: Generic MetricsConfigurationList _
instance showMetricsConfigurationList :: Show MetricsConfigurationList where
  show = genericShow
instance decodeMetricsConfigurationList :: Decode MetricsConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricsConfigurationList :: Encode MetricsConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricsFilter = MetricsFilter 
  { "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "Tag" :: NullOrUndefined.NullOrUndefined (Tag)
  , "And" :: NullOrUndefined.NullOrUndefined (MetricsAndOperator)
  }
derive instance newtypeMetricsFilter :: Newtype MetricsFilter _
derive instance repGenericMetricsFilter :: Generic MetricsFilter _
instance showMetricsFilter :: Show MetricsFilter where
  show = genericShow
instance decodeMetricsFilter :: Decode MetricsFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricsFilter :: Encode MetricsFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricsId = MetricsId String
derive instance newtypeMetricsId :: Newtype MetricsId _
derive instance repGenericMetricsId :: Generic MetricsId _
instance showMetricsId :: Show MetricsId where
  show = genericShow
instance decodeMetricsId :: Decode MetricsId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricsId :: Encode MetricsId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MissingMeta = MissingMeta Int
derive instance newtypeMissingMeta :: Newtype MissingMeta _
derive instance repGenericMissingMeta :: Generic MissingMeta _
instance showMissingMeta :: Show MissingMeta where
  show = genericShow
instance decodeMissingMeta :: Decode MissingMeta where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingMeta :: Encode MissingMeta where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MultipartUpload = MultipartUpload 
  { "UploadId" :: NullOrUndefined.NullOrUndefined (MultipartUploadId)
  , "Key" :: NullOrUndefined.NullOrUndefined (ObjectKey)
  , "Initiated" :: NullOrUndefined.NullOrUndefined (Initiated)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  , "Owner" :: NullOrUndefined.NullOrUndefined (Owner)
  , "Initiator" :: NullOrUndefined.NullOrUndefined (Initiator)
  }
derive instance newtypeMultipartUpload :: Newtype MultipartUpload _
derive instance repGenericMultipartUpload :: Generic MultipartUpload _
instance showMultipartUpload :: Show MultipartUpload where
  show = genericShow
instance decodeMultipartUpload :: Decode MultipartUpload where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMultipartUpload :: Encode MultipartUpload where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MultipartUploadId = MultipartUploadId String
derive instance newtypeMultipartUploadId :: Newtype MultipartUploadId _
derive instance repGenericMultipartUploadId :: Generic MultipartUploadId _
instance showMultipartUploadId :: Show MultipartUploadId where
  show = genericShow
instance decodeMultipartUploadId :: Decode MultipartUploadId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMultipartUploadId :: Encode MultipartUploadId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MultipartUploadList = MultipartUploadList (Array MultipartUpload)
derive instance newtypeMultipartUploadList :: Newtype MultipartUploadList _
derive instance repGenericMultipartUploadList :: Generic MultipartUploadList _
instance showMultipartUploadList :: Show MultipartUploadList where
  show = genericShow
instance decodeMultipartUploadList :: Decode MultipartUploadList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMultipartUploadList :: Encode MultipartUploadList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextKeyMarker = NextKeyMarker String
derive instance newtypeNextKeyMarker :: Newtype NextKeyMarker _
derive instance repGenericNextKeyMarker :: Generic NextKeyMarker _
instance showNextKeyMarker :: Show NextKeyMarker where
  show = genericShow
instance decodeNextKeyMarker :: Decode NextKeyMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextKeyMarker :: Encode NextKeyMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextMarker = NextMarker String
derive instance newtypeNextMarker :: Newtype NextMarker _
derive instance repGenericNextMarker :: Generic NextMarker _
instance showNextMarker :: Show NextMarker where
  show = genericShow
instance decodeNextMarker :: Decode NextMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextMarker :: Encode NextMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextPartNumberMarker = NextPartNumberMarker Int
derive instance newtypeNextPartNumberMarker :: Newtype NextPartNumberMarker _
derive instance repGenericNextPartNumberMarker :: Generic NextPartNumberMarker _
instance showNextPartNumberMarker :: Show NextPartNumberMarker where
  show = genericShow
instance decodeNextPartNumberMarker :: Decode NextPartNumberMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextPartNumberMarker :: Encode NextPartNumberMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextUploadIdMarker = NextUploadIdMarker String
derive instance newtypeNextUploadIdMarker :: Newtype NextUploadIdMarker _
derive instance repGenericNextUploadIdMarker :: Generic NextUploadIdMarker _
instance showNextUploadIdMarker :: Show NextUploadIdMarker where
  show = genericShow
instance decodeNextUploadIdMarker :: Decode NextUploadIdMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextUploadIdMarker :: Encode NextUploadIdMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextVersionIdMarker = NextVersionIdMarker String
derive instance newtypeNextVersionIdMarker :: Newtype NextVersionIdMarker _
derive instance repGenericNextVersionIdMarker :: Generic NextVersionIdMarker _
instance showNextVersionIdMarker :: Show NextVersionIdMarker where
  show = genericShow
instance decodeNextVersionIdMarker :: Decode NextVersionIdMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextVersionIdMarker :: Encode NextVersionIdMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The specified bucket does not exist.
newtype NoSuchBucket = NoSuchBucket Types.NoArguments
derive instance newtypeNoSuchBucket :: Newtype NoSuchBucket _
derive instance repGenericNoSuchBucket :: Generic NoSuchBucket _
instance showNoSuchBucket :: Show NoSuchBucket where
  show = genericShow
instance decodeNoSuchBucket :: Decode NoSuchBucket where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchBucket :: Encode NoSuchBucket where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The specified key does not exist.
newtype NoSuchKey = NoSuchKey Types.NoArguments
derive instance newtypeNoSuchKey :: Newtype NoSuchKey _
derive instance repGenericNoSuchKey :: Generic NoSuchKey _
instance showNoSuchKey :: Show NoSuchKey where
  show = genericShow
instance decodeNoSuchKey :: Decode NoSuchKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchKey :: Encode NoSuchKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The specified multipart upload does not exist.
newtype NoSuchUpload = NoSuchUpload Types.NoArguments
derive instance newtypeNoSuchUpload :: Newtype NoSuchUpload _
derive instance repGenericNoSuchUpload :: Generic NoSuchUpload _
instance showNoSuchUpload :: Show NoSuchUpload where
  show = genericShow
instance decodeNoSuchUpload :: Decode NoSuchUpload where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchUpload :: Encode NoSuchUpload where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon S3 permanently deletes the noncurrent object versions. You set this lifecycle configuration action on a bucket that has versioning enabled (or suspended) to request that Amazon S3 delete noncurrent object versions at a specific period in the object's lifetime.
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration 
  { "NoncurrentDays" :: NullOrUndefined.NullOrUndefined (Days)
  }
derive instance newtypeNoncurrentVersionExpiration :: Newtype NoncurrentVersionExpiration _
derive instance repGenericNoncurrentVersionExpiration :: Generic NoncurrentVersionExpiration _
instance showNoncurrentVersionExpiration :: Show NoncurrentVersionExpiration where
  show = genericShow
instance decodeNoncurrentVersionExpiration :: Decode NoncurrentVersionExpiration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoncurrentVersionExpiration :: Encode NoncurrentVersionExpiration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for the transition rule that describes when noncurrent objects transition to the STANDARD_IA or GLACIER storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to the STANDARD_IA or GLACIER storage class at a specific period in the object's lifetime.
newtype NoncurrentVersionTransition = NoncurrentVersionTransition 
  { "NoncurrentDays" :: NullOrUndefined.NullOrUndefined (Days)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (TransitionStorageClass)
  }
derive instance newtypeNoncurrentVersionTransition :: Newtype NoncurrentVersionTransition _
derive instance repGenericNoncurrentVersionTransition :: Generic NoncurrentVersionTransition _
instance showNoncurrentVersionTransition :: Show NoncurrentVersionTransition where
  show = genericShow
instance decodeNoncurrentVersionTransition :: Decode NoncurrentVersionTransition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoncurrentVersionTransition :: Encode NoncurrentVersionTransition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NoncurrentVersionTransitionList = NoncurrentVersionTransitionList (Array NoncurrentVersionTransition)
derive instance newtypeNoncurrentVersionTransitionList :: Newtype NoncurrentVersionTransitionList _
derive instance repGenericNoncurrentVersionTransitionList :: Generic NoncurrentVersionTransitionList _
instance showNoncurrentVersionTransitionList :: Show NoncurrentVersionTransitionList where
  show = genericShow
instance decodeNoncurrentVersionTransitionList :: Decode NoncurrentVersionTransitionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoncurrentVersionTransitionList :: Encode NoncurrentVersionTransitionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for specifying the notification configuration of the bucket. If this element is empty, notifications are turned off on the bucket.
newtype NotificationConfiguration = NotificationConfiguration 
  { "TopicConfigurations" :: NullOrUndefined.NullOrUndefined (TopicConfigurationList)
  , "QueueConfigurations" :: NullOrUndefined.NullOrUndefined (QueueConfigurationList)
  , "LambdaFunctionConfigurations" :: NullOrUndefined.NullOrUndefined (LambdaFunctionConfigurationList)
  }
derive instance newtypeNotificationConfiguration :: Newtype NotificationConfiguration _
derive instance repGenericNotificationConfiguration :: Generic NotificationConfiguration _
instance showNotificationConfiguration :: Show NotificationConfiguration where
  show = genericShow
instance decodeNotificationConfiguration :: Decode NotificationConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationConfiguration :: Encode NotificationConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NotificationConfigurationDeprecated = NotificationConfigurationDeprecated 
  { "TopicConfiguration" :: NullOrUndefined.NullOrUndefined (TopicConfigurationDeprecated)
  , "QueueConfiguration" :: NullOrUndefined.NullOrUndefined (QueueConfigurationDeprecated)
  , "CloudFunctionConfiguration" :: NullOrUndefined.NullOrUndefined (CloudFunctionConfiguration)
  }
derive instance newtypeNotificationConfigurationDeprecated :: Newtype NotificationConfigurationDeprecated _
derive instance repGenericNotificationConfigurationDeprecated :: Generic NotificationConfigurationDeprecated _
instance showNotificationConfigurationDeprecated :: Show NotificationConfigurationDeprecated where
  show = genericShow
instance decodeNotificationConfigurationDeprecated :: Decode NotificationConfigurationDeprecated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationConfigurationDeprecated :: Encode NotificationConfigurationDeprecated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for object key name filtering rules. For information about key name filtering, go to <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html">Configuring Event Notifications</a> in the Amazon Simple Storage Service Developer Guide.
newtype NotificationConfigurationFilter = NotificationConfigurationFilter 
  { "Key" :: NullOrUndefined.NullOrUndefined (S3KeyFilter)
  }
derive instance newtypeNotificationConfigurationFilter :: Newtype NotificationConfigurationFilter _
derive instance repGenericNotificationConfigurationFilter :: Generic NotificationConfigurationFilter _
instance showNotificationConfigurationFilter :: Show NotificationConfigurationFilter where
  show = genericShow
instance decodeNotificationConfigurationFilter :: Decode NotificationConfigurationFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationConfigurationFilter :: Encode NotificationConfigurationFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Optional unique identifier for configurations in a notification configuration. If you don't provide one, Amazon S3 will assign an ID.
newtype NotificationId = NotificationId String
derive instance newtypeNotificationId :: Newtype NotificationId _
derive instance repGenericNotificationId :: Generic NotificationId _
instance showNotificationId :: Show NotificationId where
  show = genericShow
instance decodeNotificationId :: Decode NotificationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationId :: Encode NotificationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Object = Object 
  { "Key" :: NullOrUndefined.NullOrUndefined (ObjectKey)
  , "LastModified" :: NullOrUndefined.NullOrUndefined (LastModified)
  , "ETag" :: NullOrUndefined.NullOrUndefined (ETag)
  , "Size" :: NullOrUndefined.NullOrUndefined (Size)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (ObjectStorageClass)
  , "Owner" :: NullOrUndefined.NullOrUndefined (Owner)
  }
derive instance newtypeObject :: Newtype Object _
derive instance repGenericObject :: Generic Object _
instance showObject :: Show Object where
  show = genericShow
instance decodeObject :: Decode Object where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObject :: Encode Object where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This operation is not allowed against this storage tier
newtype ObjectAlreadyInActiveTierError = ObjectAlreadyInActiveTierError Types.NoArguments
derive instance newtypeObjectAlreadyInActiveTierError :: Newtype ObjectAlreadyInActiveTierError _
derive instance repGenericObjectAlreadyInActiveTierError :: Generic ObjectAlreadyInActiveTierError _
instance showObjectAlreadyInActiveTierError :: Show ObjectAlreadyInActiveTierError where
  show = genericShow
instance decodeObjectAlreadyInActiveTierError :: Decode ObjectAlreadyInActiveTierError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectAlreadyInActiveTierError :: Encode ObjectAlreadyInActiveTierError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectCannedACL = ObjectCannedACL String
derive instance newtypeObjectCannedACL :: Newtype ObjectCannedACL _
derive instance repGenericObjectCannedACL :: Generic ObjectCannedACL _
instance showObjectCannedACL :: Show ObjectCannedACL where
  show = genericShow
instance decodeObjectCannedACL :: Decode ObjectCannedACL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectCannedACL :: Encode ObjectCannedACL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectIdentifier = ObjectIdentifier 
  { "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  }
derive instance newtypeObjectIdentifier :: Newtype ObjectIdentifier _
derive instance repGenericObjectIdentifier :: Generic ObjectIdentifier _
instance showObjectIdentifier :: Show ObjectIdentifier where
  show = genericShow
instance decodeObjectIdentifier :: Decode ObjectIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectIdentifier :: Encode ObjectIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectIdentifierList = ObjectIdentifierList (Array ObjectIdentifier)
derive instance newtypeObjectIdentifierList :: Newtype ObjectIdentifierList _
derive instance repGenericObjectIdentifierList :: Generic ObjectIdentifierList _
instance showObjectIdentifierList :: Show ObjectIdentifierList where
  show = genericShow
instance decodeObjectIdentifierList :: Decode ObjectIdentifierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectIdentifierList :: Encode ObjectIdentifierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectKey = ObjectKey String
derive instance newtypeObjectKey :: Newtype ObjectKey _
derive instance repGenericObjectKey :: Generic ObjectKey _
instance showObjectKey :: Show ObjectKey where
  show = genericShow
instance decodeObjectKey :: Decode ObjectKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectKey :: Encode ObjectKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectList = ObjectList (Array Object)
derive instance newtypeObjectList :: Newtype ObjectList _
derive instance repGenericObjectList :: Generic ObjectList _
instance showObjectList :: Show ObjectList where
  show = genericShow
instance decodeObjectList :: Decode ObjectList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectList :: Encode ObjectList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The source object of the COPY operation is not in the active tier and is only stored in Amazon Glacier.
newtype ObjectNotInActiveTierError = ObjectNotInActiveTierError Types.NoArguments
derive instance newtypeObjectNotInActiveTierError :: Newtype ObjectNotInActiveTierError _
derive instance repGenericObjectNotInActiveTierError :: Generic ObjectNotInActiveTierError _
instance showObjectNotInActiveTierError :: Show ObjectNotInActiveTierError where
  show = genericShow
instance decodeObjectNotInActiveTierError :: Decode ObjectNotInActiveTierError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectNotInActiveTierError :: Encode ObjectNotInActiveTierError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectStorageClass = ObjectStorageClass String
derive instance newtypeObjectStorageClass :: Newtype ObjectStorageClass _
derive instance repGenericObjectStorageClass :: Generic ObjectStorageClass _
instance showObjectStorageClass :: Show ObjectStorageClass where
  show = genericShow
instance decodeObjectStorageClass :: Decode ObjectStorageClass where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectStorageClass :: Encode ObjectStorageClass where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectVersion = ObjectVersion 
  { "ETag" :: NullOrUndefined.NullOrUndefined (ETag)
  , "Size" :: NullOrUndefined.NullOrUndefined (Size)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (ObjectVersionStorageClass)
  , "Key" :: NullOrUndefined.NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "IsLatest" :: NullOrUndefined.NullOrUndefined (IsLatest)
  , "LastModified" :: NullOrUndefined.NullOrUndefined (LastModified)
  , "Owner" :: NullOrUndefined.NullOrUndefined (Owner)
  }
derive instance newtypeObjectVersion :: Newtype ObjectVersion _
derive instance repGenericObjectVersion :: Generic ObjectVersion _
instance showObjectVersion :: Show ObjectVersion where
  show = genericShow
instance decodeObjectVersion :: Decode ObjectVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectVersion :: Encode ObjectVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectVersionId = ObjectVersionId String
derive instance newtypeObjectVersionId :: Newtype ObjectVersionId _
derive instance repGenericObjectVersionId :: Generic ObjectVersionId _
instance showObjectVersionId :: Show ObjectVersionId where
  show = genericShow
instance decodeObjectVersionId :: Decode ObjectVersionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectVersionId :: Encode ObjectVersionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectVersionList = ObjectVersionList (Array ObjectVersion)
derive instance newtypeObjectVersionList :: Newtype ObjectVersionList _
derive instance repGenericObjectVersionList :: Generic ObjectVersionList _
instance showObjectVersionList :: Show ObjectVersionList where
  show = genericShow
instance decodeObjectVersionList :: Decode ObjectVersionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectVersionList :: Encode ObjectVersionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectVersionStorageClass = ObjectVersionStorageClass String
derive instance newtypeObjectVersionStorageClass :: Newtype ObjectVersionStorageClass _
derive instance repGenericObjectVersionStorageClass :: Generic ObjectVersionStorageClass _
instance showObjectVersionStorageClass :: Show ObjectVersionStorageClass where
  show = genericShow
instance decodeObjectVersionStorageClass :: Decode ObjectVersionStorageClass where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectVersionStorageClass :: Encode ObjectVersionStorageClass where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Describes the location where the restore job's output is stored.
newtype OutputLocation = OutputLocation 
  { "S3" :: NullOrUndefined.NullOrUndefined (S3Location)
  }
derive instance newtypeOutputLocation :: Newtype OutputLocation _
derive instance repGenericOutputLocation :: Generic OutputLocation _
instance showOutputLocation :: Show OutputLocation where
  show = genericShow
instance decodeOutputLocation :: Decode OutputLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputLocation :: Encode OutputLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Describes how results of the Select job are serialized.
newtype OutputSerialization = OutputSerialization 
  { "CSV" :: NullOrUndefined.NullOrUndefined (CSVOutput)
  }
derive instance newtypeOutputSerialization :: Newtype OutputSerialization _
derive instance repGenericOutputSerialization :: Generic OutputSerialization _
instance showOutputSerialization :: Show OutputSerialization where
  show = genericShow
instance decodeOutputSerialization :: Decode OutputSerialization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputSerialization :: Encode OutputSerialization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Owner = Owner 
  { "DisplayName" :: NullOrUndefined.NullOrUndefined (DisplayName)
  , "ID" :: NullOrUndefined.NullOrUndefined (ID)
  }
derive instance newtypeOwner :: Newtype Owner _
derive instance repGenericOwner :: Generic Owner _
instance showOwner :: Show Owner where
  show = genericShow
instance decodeOwner :: Decode Owner where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOwner :: Encode Owner where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OwnerOverride = OwnerOverride String
derive instance newtypeOwnerOverride :: Newtype OwnerOverride _
derive instance repGenericOwnerOverride :: Generic OwnerOverride _
instance showOwnerOverride :: Show OwnerOverride where
  show = genericShow
instance decodeOwnerOverride :: Decode OwnerOverride where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOwnerOverride :: Encode OwnerOverride where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Part = Part 
  { "PartNumber" :: NullOrUndefined.NullOrUndefined (PartNumber)
  , "LastModified" :: NullOrUndefined.NullOrUndefined (LastModified)
  , "ETag" :: NullOrUndefined.NullOrUndefined (ETag)
  , "Size" :: NullOrUndefined.NullOrUndefined (Size)
  }
derive instance newtypePart :: Newtype Part _
derive instance repGenericPart :: Generic Part _
instance showPart :: Show Part where
  show = genericShow
instance decodePart :: Decode Part where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePart :: Encode Part where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartNumber = PartNumber Int
derive instance newtypePartNumber :: Newtype PartNumber _
derive instance repGenericPartNumber :: Generic PartNumber _
instance showPartNumber :: Show PartNumber where
  show = genericShow
instance decodePartNumber :: Decode PartNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartNumber :: Encode PartNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartNumberMarker = PartNumberMarker Int
derive instance newtypePartNumberMarker :: Newtype PartNumberMarker _
derive instance repGenericPartNumberMarker :: Generic PartNumberMarker _
instance showPartNumberMarker :: Show PartNumberMarker where
  show = genericShow
instance decodePartNumberMarker :: Decode PartNumberMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartNumberMarker :: Encode PartNumberMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Parts = Parts (Array Part)
derive instance newtypeParts :: Newtype Parts _
derive instance repGenericParts :: Generic Parts _
instance showParts :: Show Parts where
  show = genericShow
instance decodeParts :: Decode Parts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParts :: Encode Parts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartsCount = PartsCount Int
derive instance newtypePartsCount :: Newtype PartsCount _
derive instance repGenericPartsCount :: Generic PartsCount _
instance showPartsCount :: Show PartsCount where
  show = genericShow
instance decodePartsCount :: Decode PartsCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartsCount :: Encode PartsCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Payer = Payer String
derive instance newtypePayer :: Newtype Payer _
derive instance repGenericPayer :: Generic Payer _
instance showPayer :: Show Payer where
  show = genericShow
instance decodePayer :: Decode Payer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePayer :: Encode Payer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Permission = Permission String
derive instance newtypePermission :: Newtype Permission _
derive instance repGenericPermission :: Generic Permission _
instance showPermission :: Show Permission where
  show = genericShow
instance decodePermission :: Decode Permission where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePermission :: Encode Permission where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Policy = Policy String
derive instance newtypePolicy :: Newtype Policy _
derive instance repGenericPolicy :: Generic Policy _
instance showPolicy :: Show Policy where
  show = genericShow
instance decodePolicy :: Decode Policy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicy :: Encode Policy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Prefix = Prefix String
derive instance newtypePrefix :: Newtype Prefix _
derive instance repGenericPrefix :: Generic Prefix _
instance showPrefix :: Show Prefix where
  show = genericShow
instance decodePrefix :: Decode Prefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrefix :: Encode Prefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Protocol = Protocol String
derive instance newtypeProtocol :: Newtype Protocol _
derive instance repGenericProtocol :: Generic Protocol _
instance showProtocol :: Show Protocol where
  show = genericShow
instance decodeProtocol :: Decode Protocol where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProtocol :: Encode Protocol where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketAccelerateConfigurationRequest = PutBucketAccelerateConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "AccelerateConfiguration" :: (AccelerateConfiguration)
  }
derive instance newtypePutBucketAccelerateConfigurationRequest :: Newtype PutBucketAccelerateConfigurationRequest _
derive instance repGenericPutBucketAccelerateConfigurationRequest :: Generic PutBucketAccelerateConfigurationRequest _
instance showPutBucketAccelerateConfigurationRequest :: Show PutBucketAccelerateConfigurationRequest where
  show = genericShow
instance decodePutBucketAccelerateConfigurationRequest :: Decode PutBucketAccelerateConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketAccelerateConfigurationRequest :: Encode PutBucketAccelerateConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketAclRequest = PutBucketAclRequest 
  { "ACL" :: NullOrUndefined.NullOrUndefined (BucketCannedACL)
  , "AccessControlPolicy" :: NullOrUndefined.NullOrUndefined (AccessControlPolicy)
  , "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "GrantFullControl" :: NullOrUndefined.NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined.NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined.NullOrUndefined (GrantReadACP)
  , "GrantWrite" :: NullOrUndefined.NullOrUndefined (GrantWrite)
  , "GrantWriteACP" :: NullOrUndefined.NullOrUndefined (GrantWriteACP)
  }
derive instance newtypePutBucketAclRequest :: Newtype PutBucketAclRequest _
derive instance repGenericPutBucketAclRequest :: Generic PutBucketAclRequest _
instance showPutBucketAclRequest :: Show PutBucketAclRequest where
  show = genericShow
instance decodePutBucketAclRequest :: Decode PutBucketAclRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketAclRequest :: Encode PutBucketAclRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketAnalyticsConfigurationRequest = PutBucketAnalyticsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (AnalyticsId)
  , "AnalyticsConfiguration" :: (AnalyticsConfiguration)
  }
derive instance newtypePutBucketAnalyticsConfigurationRequest :: Newtype PutBucketAnalyticsConfigurationRequest _
derive instance repGenericPutBucketAnalyticsConfigurationRequest :: Generic PutBucketAnalyticsConfigurationRequest _
instance showPutBucketAnalyticsConfigurationRequest :: Show PutBucketAnalyticsConfigurationRequest where
  show = genericShow
instance decodePutBucketAnalyticsConfigurationRequest :: Decode PutBucketAnalyticsConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketAnalyticsConfigurationRequest :: Encode PutBucketAnalyticsConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketCorsRequest = PutBucketCorsRequest 
  { "Bucket" :: (BucketName)
  , "CORSConfiguration" :: (CORSConfiguration)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  }
derive instance newtypePutBucketCorsRequest :: Newtype PutBucketCorsRequest _
derive instance repGenericPutBucketCorsRequest :: Generic PutBucketCorsRequest _
instance showPutBucketCorsRequest :: Show PutBucketCorsRequest where
  show = genericShow
instance decodePutBucketCorsRequest :: Decode PutBucketCorsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketCorsRequest :: Encode PutBucketCorsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketEncryptionRequest = PutBucketEncryptionRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "ServerSideEncryptionConfiguration" :: (ServerSideEncryptionConfiguration)
  }
derive instance newtypePutBucketEncryptionRequest :: Newtype PutBucketEncryptionRequest _
derive instance repGenericPutBucketEncryptionRequest :: Generic PutBucketEncryptionRequest _
instance showPutBucketEncryptionRequest :: Show PutBucketEncryptionRequest where
  show = genericShow
instance decodePutBucketEncryptionRequest :: Decode PutBucketEncryptionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketEncryptionRequest :: Encode PutBucketEncryptionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketInventoryConfigurationRequest = PutBucketInventoryConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (InventoryId)
  , "InventoryConfiguration" :: (InventoryConfiguration)
  }
derive instance newtypePutBucketInventoryConfigurationRequest :: Newtype PutBucketInventoryConfigurationRequest _
derive instance repGenericPutBucketInventoryConfigurationRequest :: Generic PutBucketInventoryConfigurationRequest _
instance showPutBucketInventoryConfigurationRequest :: Show PutBucketInventoryConfigurationRequest where
  show = genericShow
instance decodePutBucketInventoryConfigurationRequest :: Decode PutBucketInventoryConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketInventoryConfigurationRequest :: Encode PutBucketInventoryConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketLifecycleConfigurationRequest = PutBucketLifecycleConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "LifecycleConfiguration" :: NullOrUndefined.NullOrUndefined (BucketLifecycleConfiguration)
  }
derive instance newtypePutBucketLifecycleConfigurationRequest :: Newtype PutBucketLifecycleConfigurationRequest _
derive instance repGenericPutBucketLifecycleConfigurationRequest :: Generic PutBucketLifecycleConfigurationRequest _
instance showPutBucketLifecycleConfigurationRequest :: Show PutBucketLifecycleConfigurationRequest where
  show = genericShow
instance decodePutBucketLifecycleConfigurationRequest :: Decode PutBucketLifecycleConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketLifecycleConfigurationRequest :: Encode PutBucketLifecycleConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketLifecycleRequest = PutBucketLifecycleRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "LifecycleConfiguration" :: NullOrUndefined.NullOrUndefined (LifecycleConfiguration)
  }
derive instance newtypePutBucketLifecycleRequest :: Newtype PutBucketLifecycleRequest _
derive instance repGenericPutBucketLifecycleRequest :: Generic PutBucketLifecycleRequest _
instance showPutBucketLifecycleRequest :: Show PutBucketLifecycleRequest where
  show = genericShow
instance decodePutBucketLifecycleRequest :: Decode PutBucketLifecycleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketLifecycleRequest :: Encode PutBucketLifecycleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketLoggingRequest = PutBucketLoggingRequest 
  { "Bucket" :: (BucketName)
  , "BucketLoggingStatus" :: (BucketLoggingStatus)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  }
derive instance newtypePutBucketLoggingRequest :: Newtype PutBucketLoggingRequest _
derive instance repGenericPutBucketLoggingRequest :: Generic PutBucketLoggingRequest _
instance showPutBucketLoggingRequest :: Show PutBucketLoggingRequest where
  show = genericShow
instance decodePutBucketLoggingRequest :: Decode PutBucketLoggingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketLoggingRequest :: Encode PutBucketLoggingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketMetricsConfigurationRequest = PutBucketMetricsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (MetricsId)
  , "MetricsConfiguration" :: (MetricsConfiguration)
  }
derive instance newtypePutBucketMetricsConfigurationRequest :: Newtype PutBucketMetricsConfigurationRequest _
derive instance repGenericPutBucketMetricsConfigurationRequest :: Generic PutBucketMetricsConfigurationRequest _
instance showPutBucketMetricsConfigurationRequest :: Show PutBucketMetricsConfigurationRequest where
  show = genericShow
instance decodePutBucketMetricsConfigurationRequest :: Decode PutBucketMetricsConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketMetricsConfigurationRequest :: Encode PutBucketMetricsConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketNotificationConfigurationRequest = PutBucketNotificationConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "NotificationConfiguration" :: (NotificationConfiguration)
  }
derive instance newtypePutBucketNotificationConfigurationRequest :: Newtype PutBucketNotificationConfigurationRequest _
derive instance repGenericPutBucketNotificationConfigurationRequest :: Generic PutBucketNotificationConfigurationRequest _
instance showPutBucketNotificationConfigurationRequest :: Show PutBucketNotificationConfigurationRequest where
  show = genericShow
instance decodePutBucketNotificationConfigurationRequest :: Decode PutBucketNotificationConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketNotificationConfigurationRequest :: Encode PutBucketNotificationConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketNotificationRequest = PutBucketNotificationRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "NotificationConfiguration" :: (NotificationConfigurationDeprecated)
  }
derive instance newtypePutBucketNotificationRequest :: Newtype PutBucketNotificationRequest _
derive instance repGenericPutBucketNotificationRequest :: Generic PutBucketNotificationRequest _
instance showPutBucketNotificationRequest :: Show PutBucketNotificationRequest where
  show = genericShow
instance decodePutBucketNotificationRequest :: Decode PutBucketNotificationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketNotificationRequest :: Encode PutBucketNotificationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketPolicyRequest = PutBucketPolicyRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "ConfirmRemoveSelfBucketAccess" :: NullOrUndefined.NullOrUndefined (ConfirmRemoveSelfBucketAccess)
  , "Policy" :: (Policy)
  }
derive instance newtypePutBucketPolicyRequest :: Newtype PutBucketPolicyRequest _
derive instance repGenericPutBucketPolicyRequest :: Generic PutBucketPolicyRequest _
instance showPutBucketPolicyRequest :: Show PutBucketPolicyRequest where
  show = genericShow
instance decodePutBucketPolicyRequest :: Decode PutBucketPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketPolicyRequest :: Encode PutBucketPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketReplicationRequest = PutBucketReplicationRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "ReplicationConfiguration" :: (ReplicationConfiguration)
  }
derive instance newtypePutBucketReplicationRequest :: Newtype PutBucketReplicationRequest _
derive instance repGenericPutBucketReplicationRequest :: Generic PutBucketReplicationRequest _
instance showPutBucketReplicationRequest :: Show PutBucketReplicationRequest where
  show = genericShow
instance decodePutBucketReplicationRequest :: Decode PutBucketReplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketReplicationRequest :: Encode PutBucketReplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketRequestPaymentRequest = PutBucketRequestPaymentRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "RequestPaymentConfiguration" :: (RequestPaymentConfiguration)
  }
derive instance newtypePutBucketRequestPaymentRequest :: Newtype PutBucketRequestPaymentRequest _
derive instance repGenericPutBucketRequestPaymentRequest :: Generic PutBucketRequestPaymentRequest _
instance showPutBucketRequestPaymentRequest :: Show PutBucketRequestPaymentRequest where
  show = genericShow
instance decodePutBucketRequestPaymentRequest :: Decode PutBucketRequestPaymentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketRequestPaymentRequest :: Encode PutBucketRequestPaymentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketTaggingRequest = PutBucketTaggingRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "Tagging" :: (Tagging)
  }
derive instance newtypePutBucketTaggingRequest :: Newtype PutBucketTaggingRequest _
derive instance repGenericPutBucketTaggingRequest :: Generic PutBucketTaggingRequest _
instance showPutBucketTaggingRequest :: Show PutBucketTaggingRequest where
  show = genericShow
instance decodePutBucketTaggingRequest :: Decode PutBucketTaggingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketTaggingRequest :: Encode PutBucketTaggingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketVersioningRequest = PutBucketVersioningRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "MFA" :: NullOrUndefined.NullOrUndefined (MFA)
  , "VersioningConfiguration" :: (VersioningConfiguration)
  }
derive instance newtypePutBucketVersioningRequest :: Newtype PutBucketVersioningRequest _
derive instance repGenericPutBucketVersioningRequest :: Generic PutBucketVersioningRequest _
instance showPutBucketVersioningRequest :: Show PutBucketVersioningRequest where
  show = genericShow
instance decodePutBucketVersioningRequest :: Decode PutBucketVersioningRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketVersioningRequest :: Encode PutBucketVersioningRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBucketWebsiteRequest = PutBucketWebsiteRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "WebsiteConfiguration" :: (WebsiteConfiguration)
  }
derive instance newtypePutBucketWebsiteRequest :: Newtype PutBucketWebsiteRequest _
derive instance repGenericPutBucketWebsiteRequest :: Generic PutBucketWebsiteRequest _
instance showPutBucketWebsiteRequest :: Show PutBucketWebsiteRequest where
  show = genericShow
instance decodePutBucketWebsiteRequest :: Decode PutBucketWebsiteRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBucketWebsiteRequest :: Encode PutBucketWebsiteRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutObjectAclOutput = PutObjectAclOutput 
  { "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  }
derive instance newtypePutObjectAclOutput :: Newtype PutObjectAclOutput _
derive instance repGenericPutObjectAclOutput :: Generic PutObjectAclOutput _
instance showPutObjectAclOutput :: Show PutObjectAclOutput where
  show = genericShow
instance decodePutObjectAclOutput :: Decode PutObjectAclOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutObjectAclOutput :: Encode PutObjectAclOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutObjectAclRequest = PutObjectAclRequest 
  { "ACL" :: NullOrUndefined.NullOrUndefined (ObjectCannedACL)
  , "AccessControlPolicy" :: NullOrUndefined.NullOrUndefined (AccessControlPolicy)
  , "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "GrantFullControl" :: NullOrUndefined.NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined.NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined.NullOrUndefined (GrantReadACP)
  , "GrantWrite" :: NullOrUndefined.NullOrUndefined (GrantWrite)
  , "GrantWriteACP" :: NullOrUndefined.NullOrUndefined (GrantWriteACP)
  , "Key" :: (ObjectKey)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  }
derive instance newtypePutObjectAclRequest :: Newtype PutObjectAclRequest _
derive instance repGenericPutObjectAclRequest :: Generic PutObjectAclRequest _
instance showPutObjectAclRequest :: Show PutObjectAclRequest where
  show = genericShow
instance decodePutObjectAclRequest :: Decode PutObjectAclRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutObjectAclRequest :: Encode PutObjectAclRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutObjectOutput = PutObjectOutput 
  { "Expiration" :: NullOrUndefined.NullOrUndefined (Expiration)
  , "ETag" :: NullOrUndefined.NullOrUndefined (ETag)
  , "ServerSideEncryption" :: NullOrUndefined.NullOrUndefined (ServerSideEncryption)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  }
derive instance newtypePutObjectOutput :: Newtype PutObjectOutput _
derive instance repGenericPutObjectOutput :: Generic PutObjectOutput _
instance showPutObjectOutput :: Show PutObjectOutput where
  show = genericShow
instance decodePutObjectOutput :: Decode PutObjectOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutObjectOutput :: Encode PutObjectOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutObjectRequest = PutObjectRequest 
  { "ACL" :: NullOrUndefined.NullOrUndefined (ObjectCannedACL)
  , "Body" :: NullOrUndefined.NullOrUndefined (Body)
  , "Bucket" :: (BucketName)
  , "CacheControl" :: NullOrUndefined.NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined.NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined.NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined.NullOrUndefined (ContentLanguage)
  , "ContentLength" :: NullOrUndefined.NullOrUndefined (ContentLength)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "ContentType" :: NullOrUndefined.NullOrUndefined (ContentType)
  , "Expires" :: NullOrUndefined.NullOrUndefined (Expires)
  , "GrantFullControl" :: NullOrUndefined.NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined.NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined.NullOrUndefined (GrantReadACP)
  , "GrantWriteACP" :: NullOrUndefined.NullOrUndefined (GrantWriteACP)
  , "Key" :: (ObjectKey)
  , "Metadata" :: NullOrUndefined.NullOrUndefined (Metadata)
  , "ServerSideEncryption" :: NullOrUndefined.NullOrUndefined (ServerSideEncryption)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  , "WebsiteRedirectLocation" :: NullOrUndefined.NullOrUndefined (WebsiteRedirectLocation)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined.NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  , "Tagging" :: NullOrUndefined.NullOrUndefined (TaggingHeader)
  }
derive instance newtypePutObjectRequest :: Newtype PutObjectRequest _
derive instance repGenericPutObjectRequest :: Generic PutObjectRequest _
instance showPutObjectRequest :: Show PutObjectRequest where
  show = genericShow
instance decodePutObjectRequest :: Decode PutObjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutObjectRequest :: Encode PutObjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutObjectTaggingOutput = PutObjectTaggingOutput 
  { "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  }
derive instance newtypePutObjectTaggingOutput :: Newtype PutObjectTaggingOutput _
derive instance repGenericPutObjectTaggingOutput :: Generic PutObjectTaggingOutput _
instance showPutObjectTaggingOutput :: Show PutObjectTaggingOutput where
  show = genericShow
instance decodePutObjectTaggingOutput :: Decode PutObjectTaggingOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutObjectTaggingOutput :: Encode PutObjectTaggingOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutObjectTaggingRequest = PutObjectTaggingRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "Tagging" :: (Tagging)
  }
derive instance newtypePutObjectTaggingRequest :: Newtype PutObjectTaggingRequest _
derive instance repGenericPutObjectTaggingRequest :: Generic PutObjectTaggingRequest _
instance showPutObjectTaggingRequest :: Show PutObjectTaggingRequest where
  show = genericShow
instance decodePutObjectTaggingRequest :: Decode PutObjectTaggingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutObjectTaggingRequest :: Encode PutObjectTaggingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueueArn = QueueArn String
derive instance newtypeQueueArn :: Newtype QueueArn _
derive instance repGenericQueueArn :: Generic QueueArn _
instance showQueueArn :: Show QueueArn where
  show = genericShow
instance decodeQueueArn :: Decode QueueArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueArn :: Encode QueueArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for specifying an configuration when you want Amazon S3 to publish events to an Amazon Simple Queue Service (Amazon SQS) queue.
newtype QueueConfiguration = QueueConfiguration 
  { "Id" :: NullOrUndefined.NullOrUndefined (NotificationId)
  , "QueueArn" :: (QueueArn)
  , "Events" :: (EventList)
  , "Filter" :: NullOrUndefined.NullOrUndefined (NotificationConfigurationFilter)
  }
derive instance newtypeQueueConfiguration :: Newtype QueueConfiguration _
derive instance repGenericQueueConfiguration :: Generic QueueConfiguration _
instance showQueueConfiguration :: Show QueueConfiguration where
  show = genericShow
instance decodeQueueConfiguration :: Decode QueueConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueConfiguration :: Encode QueueConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueueConfigurationDeprecated = QueueConfigurationDeprecated 
  { "Id" :: NullOrUndefined.NullOrUndefined (NotificationId)
  , "Event" :: NullOrUndefined.NullOrUndefined (Event)
  , "Events" :: NullOrUndefined.NullOrUndefined (EventList)
  , "Queue" :: NullOrUndefined.NullOrUndefined (QueueArn)
  }
derive instance newtypeQueueConfigurationDeprecated :: Newtype QueueConfigurationDeprecated _
derive instance repGenericQueueConfigurationDeprecated :: Generic QueueConfigurationDeprecated _
instance showQueueConfigurationDeprecated :: Show QueueConfigurationDeprecated where
  show = genericShow
instance decodeQueueConfigurationDeprecated :: Decode QueueConfigurationDeprecated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueConfigurationDeprecated :: Encode QueueConfigurationDeprecated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueueConfigurationList = QueueConfigurationList (Array QueueConfiguration)
derive instance newtypeQueueConfigurationList :: Newtype QueueConfigurationList _
derive instance repGenericQueueConfigurationList :: Generic QueueConfigurationList _
instance showQueueConfigurationList :: Show QueueConfigurationList where
  show = genericShow
instance decodeQueueConfigurationList :: Decode QueueConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueConfigurationList :: Encode QueueConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Quiet = Quiet Boolean
derive instance newtypeQuiet :: Newtype Quiet _
derive instance repGenericQuiet :: Generic Quiet _
instance showQuiet :: Show Quiet where
  show = genericShow
instance decodeQuiet :: Decode Quiet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQuiet :: Encode Quiet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QuoteCharacter = QuoteCharacter String
derive instance newtypeQuoteCharacter :: Newtype QuoteCharacter _
derive instance repGenericQuoteCharacter :: Generic QuoteCharacter _
instance showQuoteCharacter :: Show QuoteCharacter where
  show = genericShow
instance decodeQuoteCharacter :: Decode QuoteCharacter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQuoteCharacter :: Encode QuoteCharacter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QuoteEscapeCharacter = QuoteEscapeCharacter String
derive instance newtypeQuoteEscapeCharacter :: Newtype QuoteEscapeCharacter _
derive instance repGenericQuoteEscapeCharacter :: Generic QuoteEscapeCharacter _
instance showQuoteEscapeCharacter :: Show QuoteEscapeCharacter where
  show = genericShow
instance decodeQuoteEscapeCharacter :: Decode QuoteEscapeCharacter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQuoteEscapeCharacter :: Encode QuoteEscapeCharacter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QuoteFields = QuoteFields String
derive instance newtypeQuoteFields :: Newtype QuoteFields _
derive instance repGenericQuoteFields :: Generic QuoteFields _
instance showQuoteFields :: Show QuoteFields where
  show = genericShow
instance decodeQuoteFields :: Decode QuoteFields where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQuoteFields :: Encode QuoteFields where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Range = Range String
derive instance newtypeRange :: Newtype Range _
derive instance repGenericRange :: Generic Range _
instance showRange :: Show Range where
  show = genericShow
instance decodeRange :: Decode Range where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRange :: Encode Range where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordDelimiter = RecordDelimiter String
derive instance newtypeRecordDelimiter :: Newtype RecordDelimiter _
derive instance repGenericRecordDelimiter :: Generic RecordDelimiter _
instance showRecordDelimiter :: Show RecordDelimiter where
  show = genericShow
instance decodeRecordDelimiter :: Decode RecordDelimiter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordDelimiter :: Encode RecordDelimiter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Redirect = Redirect 
  { "HostName" :: NullOrUndefined.NullOrUndefined (HostName)
  , "HttpRedirectCode" :: NullOrUndefined.NullOrUndefined (HttpRedirectCode)
  , "Protocol" :: NullOrUndefined.NullOrUndefined (Protocol)
  , "ReplaceKeyPrefixWith" :: NullOrUndefined.NullOrUndefined (ReplaceKeyPrefixWith)
  , "ReplaceKeyWith" :: NullOrUndefined.NullOrUndefined (ReplaceKeyWith)
  }
derive instance newtypeRedirect :: Newtype Redirect _
derive instance repGenericRedirect :: Generic Redirect _
instance showRedirect :: Show Redirect where
  show = genericShow
instance decodeRedirect :: Decode Redirect where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedirect :: Encode Redirect where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RedirectAllRequestsTo = RedirectAllRequestsTo 
  { "HostName" :: (HostName)
  , "Protocol" :: NullOrUndefined.NullOrUndefined (Protocol)
  }
derive instance newtypeRedirectAllRequestsTo :: Newtype RedirectAllRequestsTo _
derive instance repGenericRedirectAllRequestsTo :: Generic RedirectAllRequestsTo _
instance showRedirectAllRequestsTo :: Show RedirectAllRequestsTo where
  show = genericShow
instance decodeRedirectAllRequestsTo :: Decode RedirectAllRequestsTo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedirectAllRequestsTo :: Encode RedirectAllRequestsTo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReplaceKeyPrefixWith = ReplaceKeyPrefixWith String
derive instance newtypeReplaceKeyPrefixWith :: Newtype ReplaceKeyPrefixWith _
derive instance repGenericReplaceKeyPrefixWith :: Generic ReplaceKeyPrefixWith _
instance showReplaceKeyPrefixWith :: Show ReplaceKeyPrefixWith where
  show = genericShow
instance decodeReplaceKeyPrefixWith :: Decode ReplaceKeyPrefixWith where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplaceKeyPrefixWith :: Encode ReplaceKeyPrefixWith where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReplaceKeyWith = ReplaceKeyWith String
derive instance newtypeReplaceKeyWith :: Newtype ReplaceKeyWith _
derive instance repGenericReplaceKeyWith :: Generic ReplaceKeyWith _
instance showReplaceKeyWith :: Show ReplaceKeyWith where
  show = genericShow
instance decodeReplaceKeyWith :: Decode ReplaceKeyWith where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplaceKeyWith :: Encode ReplaceKeyWith where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReplicaKmsKeyID = ReplicaKmsKeyID String
derive instance newtypeReplicaKmsKeyID :: Newtype ReplicaKmsKeyID _
derive instance repGenericReplicaKmsKeyID :: Generic ReplicaKmsKeyID _
instance showReplicaKmsKeyID :: Show ReplicaKmsKeyID where
  show = genericShow
instance decodeReplicaKmsKeyID :: Decode ReplicaKmsKeyID where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicaKmsKeyID :: Encode ReplicaKmsKeyID where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for replication rules. You can add as many as 1,000 rules. Total replication configuration size can be up to 2 MB.
newtype ReplicationConfiguration = ReplicationConfiguration 
  { "Role" :: (Role)
  , "Rules" :: (ReplicationRules)
  }
derive instance newtypeReplicationConfiguration :: Newtype ReplicationConfiguration _
derive instance repGenericReplicationConfiguration :: Generic ReplicationConfiguration _
instance showReplicationConfiguration :: Show ReplicationConfiguration where
  show = genericShow
instance decodeReplicationConfiguration :: Decode ReplicationConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationConfiguration :: Encode ReplicationConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for information about a particular replication rule.
newtype ReplicationRule = ReplicationRule 
  { "ID" :: NullOrUndefined.NullOrUndefined (ID)
  , "Prefix" :: (Prefix)
  , "Status" :: (ReplicationRuleStatus)
  , "SourceSelectionCriteria" :: NullOrUndefined.NullOrUndefined (SourceSelectionCriteria)
  , "Destination" :: (Destination)
  }
derive instance newtypeReplicationRule :: Newtype ReplicationRule _
derive instance repGenericReplicationRule :: Generic ReplicationRule _
instance showReplicationRule :: Show ReplicationRule where
  show = genericShow
instance decodeReplicationRule :: Decode ReplicationRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationRule :: Encode ReplicationRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReplicationRuleStatus = ReplicationRuleStatus String
derive instance newtypeReplicationRuleStatus :: Newtype ReplicationRuleStatus _
derive instance repGenericReplicationRuleStatus :: Generic ReplicationRuleStatus _
instance showReplicationRuleStatus :: Show ReplicationRuleStatus where
  show = genericShow
instance decodeReplicationRuleStatus :: Decode ReplicationRuleStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationRuleStatus :: Encode ReplicationRuleStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReplicationRules = ReplicationRules (Array ReplicationRule)
derive instance newtypeReplicationRules :: Newtype ReplicationRules _
derive instance repGenericReplicationRules :: Generic ReplicationRules _
instance showReplicationRules :: Show ReplicationRules where
  show = genericShow
instance decodeReplicationRules :: Decode ReplicationRules where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationRules :: Encode ReplicationRules where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReplicationStatus = ReplicationStatus String
derive instance newtypeReplicationStatus :: Newtype ReplicationStatus _
derive instance repGenericReplicationStatus :: Generic ReplicationStatus _
instance showReplicationStatus :: Show ReplicationStatus where
  show = genericShow
instance decodeReplicationStatus :: Decode ReplicationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationStatus :: Encode ReplicationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If present, indicates that the requester was successfully charged for the request.
newtype RequestCharged = RequestCharged String
derive instance newtypeRequestCharged :: Newtype RequestCharged _
derive instance repGenericRequestCharged :: Generic RequestCharged _
instance showRequestCharged :: Show RequestCharged where
  show = genericShow
instance decodeRequestCharged :: Decode RequestCharged where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCharged :: Encode RequestCharged where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Confirms that the requester knows that she or he will be charged for the request. Bucket owners need not specify this parameter in their requests. Documentation on downloading objects from requester pays buckets can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/ObjectsinRequesterPaysBuckets.html
newtype RequestPayer = RequestPayer String
derive instance newtypeRequestPayer :: Newtype RequestPayer _
derive instance repGenericRequestPayer :: Generic RequestPayer _
instance showRequestPayer :: Show RequestPayer where
  show = genericShow
instance decodeRequestPayer :: Decode RequestPayer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestPayer :: Encode RequestPayer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequestPaymentConfiguration = RequestPaymentConfiguration 
  { "Payer" :: (Payer)
  }
derive instance newtypeRequestPaymentConfiguration :: Newtype RequestPaymentConfiguration _
derive instance repGenericRequestPaymentConfiguration :: Generic RequestPaymentConfiguration _
instance showRequestPaymentConfiguration :: Show RequestPaymentConfiguration where
  show = genericShow
instance decodeRequestPaymentConfiguration :: Decode RequestPaymentConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestPaymentConfiguration :: Encode RequestPaymentConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResponseCacheControl = ResponseCacheControl String
derive instance newtypeResponseCacheControl :: Newtype ResponseCacheControl _
derive instance repGenericResponseCacheControl :: Generic ResponseCacheControl _
instance showResponseCacheControl :: Show ResponseCacheControl where
  show = genericShow
instance decodeResponseCacheControl :: Decode ResponseCacheControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResponseCacheControl :: Encode ResponseCacheControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResponseContentDisposition = ResponseContentDisposition String
derive instance newtypeResponseContentDisposition :: Newtype ResponseContentDisposition _
derive instance repGenericResponseContentDisposition :: Generic ResponseContentDisposition _
instance showResponseContentDisposition :: Show ResponseContentDisposition where
  show = genericShow
instance decodeResponseContentDisposition :: Decode ResponseContentDisposition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResponseContentDisposition :: Encode ResponseContentDisposition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResponseContentEncoding = ResponseContentEncoding String
derive instance newtypeResponseContentEncoding :: Newtype ResponseContentEncoding _
derive instance repGenericResponseContentEncoding :: Generic ResponseContentEncoding _
instance showResponseContentEncoding :: Show ResponseContentEncoding where
  show = genericShow
instance decodeResponseContentEncoding :: Decode ResponseContentEncoding where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResponseContentEncoding :: Encode ResponseContentEncoding where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResponseContentLanguage = ResponseContentLanguage String
derive instance newtypeResponseContentLanguage :: Newtype ResponseContentLanguage _
derive instance repGenericResponseContentLanguage :: Generic ResponseContentLanguage _
instance showResponseContentLanguage :: Show ResponseContentLanguage where
  show = genericShow
instance decodeResponseContentLanguage :: Decode ResponseContentLanguage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResponseContentLanguage :: Encode ResponseContentLanguage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResponseContentType = ResponseContentType String
derive instance newtypeResponseContentType :: Newtype ResponseContentType _
derive instance repGenericResponseContentType :: Generic ResponseContentType _
instance showResponseContentType :: Show ResponseContentType where
  show = genericShow
instance decodeResponseContentType :: Decode ResponseContentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResponseContentType :: Encode ResponseContentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResponseExpires = ResponseExpires Number
derive instance newtypeResponseExpires :: Newtype ResponseExpires _
derive instance repGenericResponseExpires :: Generic ResponseExpires _
instance showResponseExpires :: Show ResponseExpires where
  show = genericShow
instance decodeResponseExpires :: Decode ResponseExpires where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResponseExpires :: Encode ResponseExpires where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Restore = Restore String
derive instance newtypeRestore :: Newtype Restore _
derive instance repGenericRestore :: Generic Restore _
instance showRestore :: Show Restore where
  show = genericShow
instance decodeRestore :: Decode Restore where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestore :: Encode Restore where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestoreObjectOutput = RestoreObjectOutput 
  { "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  , "RestoreOutputPath" :: NullOrUndefined.NullOrUndefined (RestoreOutputPath)
  }
derive instance newtypeRestoreObjectOutput :: Newtype RestoreObjectOutput _
derive instance repGenericRestoreObjectOutput :: Generic RestoreObjectOutput _
instance showRestoreObjectOutput :: Show RestoreObjectOutput where
  show = genericShow
instance decodeRestoreObjectOutput :: Decode RestoreObjectOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreObjectOutput :: Encode RestoreObjectOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestoreObjectRequest = RestoreObjectRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (ObjectVersionId)
  , "RestoreRequest" :: NullOrUndefined.NullOrUndefined (RestoreRequest)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  }
derive instance newtypeRestoreObjectRequest :: Newtype RestoreObjectRequest _
derive instance repGenericRestoreObjectRequest :: Generic RestoreObjectRequest _
instance showRestoreObjectRequest :: Show RestoreObjectRequest where
  show = genericShow
instance decodeRestoreObjectRequest :: Decode RestoreObjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreObjectRequest :: Encode RestoreObjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestoreOutputPath = RestoreOutputPath String
derive instance newtypeRestoreOutputPath :: Newtype RestoreOutputPath _
derive instance repGenericRestoreOutputPath :: Generic RestoreOutputPath _
instance showRestoreOutputPath :: Show RestoreOutputPath where
  show = genericShow
instance decodeRestoreOutputPath :: Decode RestoreOutputPath where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreOutputPath :: Encode RestoreOutputPath where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for restore job parameters.
newtype RestoreRequest = RestoreRequest 
  { "Days" :: NullOrUndefined.NullOrUndefined (Days)
  , "GlacierJobParameters" :: NullOrUndefined.NullOrUndefined (GlacierJobParameters)
  , "Type" :: NullOrUndefined.NullOrUndefined (RestoreRequestType)
  , "Tier" :: NullOrUndefined.NullOrUndefined (Tier)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "SelectParameters" :: NullOrUndefined.NullOrUndefined (SelectParameters)
  , "OutputLocation" :: NullOrUndefined.NullOrUndefined (OutputLocation)
  }
derive instance newtypeRestoreRequest :: Newtype RestoreRequest _
derive instance repGenericRestoreRequest :: Generic RestoreRequest _
instance showRestoreRequest :: Show RestoreRequest where
  show = genericShow
instance decodeRestoreRequest :: Decode RestoreRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreRequest :: Encode RestoreRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestoreRequestType = RestoreRequestType String
derive instance newtypeRestoreRequestType :: Newtype RestoreRequestType _
derive instance repGenericRestoreRequestType :: Generic RestoreRequestType _
instance showRestoreRequestType :: Show RestoreRequestType where
  show = genericShow
instance decodeRestoreRequestType :: Decode RestoreRequestType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreRequestType :: Encode RestoreRequestType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Role = Role String
derive instance newtypeRole :: Newtype Role _
derive instance repGenericRole :: Generic Role _
instance showRole :: Show Role where
  show = genericShow
instance decodeRole :: Decode Role where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRole :: Encode Role where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoutingRule = RoutingRule 
  { "Condition" :: NullOrUndefined.NullOrUndefined (Condition)
  , "Redirect" :: (Redirect)
  }
derive instance newtypeRoutingRule :: Newtype RoutingRule _
derive instance repGenericRoutingRule :: Generic RoutingRule _
instance showRoutingRule :: Show RoutingRule where
  show = genericShow
instance decodeRoutingRule :: Decode RoutingRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoutingRule :: Encode RoutingRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoutingRules = RoutingRules (Array RoutingRule)
derive instance newtypeRoutingRules :: Newtype RoutingRules _
derive instance repGenericRoutingRules :: Generic RoutingRules _
instance showRoutingRules :: Show RoutingRules where
  show = genericShow
instance decodeRoutingRules :: Decode RoutingRules where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoutingRules :: Encode RoutingRules where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Rule = Rule 
  { "Expiration" :: NullOrUndefined.NullOrUndefined (LifecycleExpiration)
  , "ID" :: NullOrUndefined.NullOrUndefined (ID)
  , "Prefix" :: (Prefix)
  , "Status" :: (ExpirationStatus)
  , "Transition" :: NullOrUndefined.NullOrUndefined (Transition)
  , "NoncurrentVersionTransition" :: NullOrUndefined.NullOrUndefined (NoncurrentVersionTransition)
  , "NoncurrentVersionExpiration" :: NullOrUndefined.NullOrUndefined (NoncurrentVersionExpiration)
  , "AbortIncompleteMultipartUpload" :: NullOrUndefined.NullOrUndefined (AbortIncompleteMultipartUpload)
  }
derive instance newtypeRule :: Newtype Rule _
derive instance repGenericRule :: Generic Rule _
instance showRule :: Show Rule where
  show = genericShow
instance decodeRule :: Decode Rule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRule :: Encode Rule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Rules = Rules (Array Rule)
derive instance newtypeRules :: Newtype Rules _
derive instance repGenericRules :: Generic Rules _
instance showRules :: Show Rules where
  show = genericShow
instance decodeRules :: Decode Rules where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRules :: Encode Rules where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for object key name prefix and suffix filtering rules.
newtype S3KeyFilter = S3KeyFilter 
  { "FilterRules" :: NullOrUndefined.NullOrUndefined (FilterRuleList)
  }
derive instance newtypeS3KeyFilter :: Newtype S3KeyFilter _
derive instance repGenericS3KeyFilter :: Generic S3KeyFilter _
instance showS3KeyFilter :: Show S3KeyFilter where
  show = genericShow
instance decodeS3KeyFilter :: Decode S3KeyFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3KeyFilter :: Encode S3KeyFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Describes an S3 location that will receive the results of the restore request.
newtype S3Location = S3Location 
  { "BucketName" :: (BucketName)
  , "Prefix" :: (LocationPrefix)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (Encryption)
  , "CannedACL" :: NullOrUndefined.NullOrUndefined (ObjectCannedACL)
  , "AccessControlList" :: NullOrUndefined.NullOrUndefined (Grants)
  , "Tagging" :: NullOrUndefined.NullOrUndefined (Tagging)
  , "UserMetadata" :: NullOrUndefined.NullOrUndefined (UserMetadata)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  }
derive instance newtypeS3Location :: Newtype S3Location _
derive instance repGenericS3Location :: Generic S3Location _
instance showS3Location :: Show S3Location where
  show = genericShow
instance decodeS3Location :: Decode S3Location where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Location :: Encode S3Location where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SSECustomerAlgorithm = SSECustomerAlgorithm String
derive instance newtypeSSECustomerAlgorithm :: Newtype SSECustomerAlgorithm _
derive instance repGenericSSECustomerAlgorithm :: Generic SSECustomerAlgorithm _
instance showSSECustomerAlgorithm :: Show SSECustomerAlgorithm where
  show = genericShow
instance decodeSSECustomerAlgorithm :: Decode SSECustomerAlgorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSSECustomerAlgorithm :: Encode SSECustomerAlgorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SSECustomerKey = SSECustomerKey String
derive instance newtypeSSECustomerKey :: Newtype SSECustomerKey _
derive instance repGenericSSECustomerKey :: Generic SSECustomerKey _
instance showSSECustomerKey :: Show SSECustomerKey where
  show = genericShow
instance decodeSSECustomerKey :: Decode SSECustomerKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSSECustomerKey :: Encode SSECustomerKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SSECustomerKeyMD5 = SSECustomerKeyMD5 String
derive instance newtypeSSECustomerKeyMD5 :: Newtype SSECustomerKeyMD5 _
derive instance repGenericSSECustomerKeyMD5 :: Generic SSECustomerKeyMD5 _
instance showSSECustomerKeyMD5 :: Show SSECustomerKeyMD5 where
  show = genericShow
instance decodeSSECustomerKeyMD5 :: Decode SSECustomerKeyMD5 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSSECustomerKeyMD5 :: Encode SSECustomerKeyMD5 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the use of SSE-KMS to encrypt delievered Inventory reports.
newtype SSEKMS = SSEKMS 
  { "KeyId" :: (SSEKMSKeyId)
  }
derive instance newtypeSSEKMS :: Newtype SSEKMS _
derive instance repGenericSSEKMS :: Generic SSEKMS _
instance showSSEKMS :: Show SSEKMS where
  show = genericShow
instance decodeSSEKMS :: Decode SSEKMS where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSSEKMS :: Encode SSEKMS where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SSEKMSKeyId = SSEKMSKeyId String
derive instance newtypeSSEKMSKeyId :: Newtype SSEKMSKeyId _
derive instance repGenericSSEKMSKeyId :: Generic SSEKMSKeyId _
instance showSSEKMSKeyId :: Show SSEKMSKeyId where
  show = genericShow
instance decodeSSEKMSKeyId :: Decode SSEKMSKeyId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSSEKMSKeyId :: Encode SSEKMSKeyId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the use of SSE-S3 to encrypt delievered Inventory reports.
newtype SSES3 = SSES3 Types.NoArguments
derive instance newtypeSSES3 :: Newtype SSES3 _
derive instance repGenericSSES3 :: Generic SSES3 _
instance showSSES3 :: Show SSES3 where
  show = genericShow
instance decodeSSES3 :: Decode SSES3 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSSES3 :: Encode SSES3 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Describes the parameters for Select job types.
newtype SelectParameters = SelectParameters 
  { "InputSerialization" :: (InputSerialization)
  , "ExpressionType" :: (ExpressionType)
  , "Expression" :: (Expression)
  , "OutputSerialization" :: (OutputSerialization)
  }
derive instance newtypeSelectParameters :: Newtype SelectParameters _
derive instance repGenericSelectParameters :: Generic SelectParameters _
instance showSelectParameters :: Show SelectParameters where
  show = genericShow
instance decodeSelectParameters :: Decode SelectParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSelectParameters :: Encode SelectParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServerSideEncryption = ServerSideEncryption String
derive instance newtypeServerSideEncryption :: Newtype ServerSideEncryption _
derive instance repGenericServerSideEncryption :: Generic ServerSideEncryption _
instance showServerSideEncryption :: Show ServerSideEncryption where
  show = genericShow
instance decodeServerSideEncryption :: Decode ServerSideEncryption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerSideEncryption :: Encode ServerSideEncryption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Describes the default server-side encryption to apply to new objects in the bucket. If Put Object request does not specify any server-side encryption, this default encryption will be applied.
newtype ServerSideEncryptionByDefault = ServerSideEncryptionByDefault 
  { "SSEAlgorithm" :: (ServerSideEncryption)
  , "KMSMasterKeyID" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  }
derive instance newtypeServerSideEncryptionByDefault :: Newtype ServerSideEncryptionByDefault _
derive instance repGenericServerSideEncryptionByDefault :: Generic ServerSideEncryptionByDefault _
instance showServerSideEncryptionByDefault :: Show ServerSideEncryptionByDefault where
  show = genericShow
instance decodeServerSideEncryptionByDefault :: Decode ServerSideEncryptionByDefault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerSideEncryptionByDefault :: Encode ServerSideEncryptionByDefault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for server-side encryption configuration rules. Currently S3 supports one rule only.
newtype ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration 
  { "Rules" :: (ServerSideEncryptionRules)
  }
derive instance newtypeServerSideEncryptionConfiguration :: Newtype ServerSideEncryptionConfiguration _
derive instance repGenericServerSideEncryptionConfiguration :: Generic ServerSideEncryptionConfiguration _
instance showServerSideEncryptionConfiguration :: Show ServerSideEncryptionConfiguration where
  show = genericShow
instance decodeServerSideEncryptionConfiguration :: Decode ServerSideEncryptionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerSideEncryptionConfiguration :: Encode ServerSideEncryptionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for information about a particular server-side encryption configuration rule.
newtype ServerSideEncryptionRule = ServerSideEncryptionRule 
  { "ApplyServerSideEncryptionByDefault" :: NullOrUndefined.NullOrUndefined (ServerSideEncryptionByDefault)
  }
derive instance newtypeServerSideEncryptionRule :: Newtype ServerSideEncryptionRule _
derive instance repGenericServerSideEncryptionRule :: Generic ServerSideEncryptionRule _
instance showServerSideEncryptionRule :: Show ServerSideEncryptionRule where
  show = genericShow
instance decodeServerSideEncryptionRule :: Decode ServerSideEncryptionRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerSideEncryptionRule :: Encode ServerSideEncryptionRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServerSideEncryptionRules = ServerSideEncryptionRules (Array ServerSideEncryptionRule)
derive instance newtypeServerSideEncryptionRules :: Newtype ServerSideEncryptionRules _
derive instance repGenericServerSideEncryptionRules :: Generic ServerSideEncryptionRules _
instance showServerSideEncryptionRules :: Show ServerSideEncryptionRules where
  show = genericShow
instance decodeServerSideEncryptionRules :: Decode ServerSideEncryptionRules where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerSideEncryptionRules :: Encode ServerSideEncryptionRules where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Size = Size Int
derive instance newtypeSize :: Newtype Size _
derive instance repGenericSize :: Generic Size _
instance showSize :: Show Size where
  show = genericShow
instance decodeSize :: Decode Size where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSize :: Encode Size where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for filters that define which source objects should be replicated.
newtype SourceSelectionCriteria = SourceSelectionCriteria 
  { "SseKmsEncryptedObjects" :: NullOrUndefined.NullOrUndefined (SseKmsEncryptedObjects)
  }
derive instance newtypeSourceSelectionCriteria :: Newtype SourceSelectionCriteria _
derive instance repGenericSourceSelectionCriteria :: Generic SourceSelectionCriteria _
instance showSourceSelectionCriteria :: Show SourceSelectionCriteria where
  show = genericShow
instance decodeSourceSelectionCriteria :: Decode SourceSelectionCriteria where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceSelectionCriteria :: Encode SourceSelectionCriteria where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for filter information of selection of KMS Encrypted S3 objects.
newtype SseKmsEncryptedObjects = SseKmsEncryptedObjects 
  { "Status" :: (SseKmsEncryptedObjectsStatus)
  }
derive instance newtypeSseKmsEncryptedObjects :: Newtype SseKmsEncryptedObjects _
derive instance repGenericSseKmsEncryptedObjects :: Generic SseKmsEncryptedObjects _
instance showSseKmsEncryptedObjects :: Show SseKmsEncryptedObjects where
  show = genericShow
instance decodeSseKmsEncryptedObjects :: Decode SseKmsEncryptedObjects where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSseKmsEncryptedObjects :: Encode SseKmsEncryptedObjects where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SseKmsEncryptedObjectsStatus = SseKmsEncryptedObjectsStatus String
derive instance newtypeSseKmsEncryptedObjectsStatus :: Newtype SseKmsEncryptedObjectsStatus _
derive instance repGenericSseKmsEncryptedObjectsStatus :: Generic SseKmsEncryptedObjectsStatus _
instance showSseKmsEncryptedObjectsStatus :: Show SseKmsEncryptedObjectsStatus where
  show = genericShow
instance decodeSseKmsEncryptedObjectsStatus :: Decode SseKmsEncryptedObjectsStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSseKmsEncryptedObjectsStatus :: Encode SseKmsEncryptedObjectsStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartAfter = StartAfter String
derive instance newtypeStartAfter :: Newtype StartAfter _
derive instance repGenericStartAfter :: Generic StartAfter _
instance showStartAfter :: Show StartAfter where
  show = genericShow
instance decodeStartAfter :: Decode StartAfter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartAfter :: Encode StartAfter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StorageClass = StorageClass String
derive instance newtypeStorageClass :: Newtype StorageClass _
derive instance repGenericStorageClass :: Generic StorageClass _
instance showStorageClass :: Show StorageClass where
  show = genericShow
instance decodeStorageClass :: Decode StorageClass where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageClass :: Encode StorageClass where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StorageClassAnalysis = StorageClassAnalysis 
  { "DataExport" :: NullOrUndefined.NullOrUndefined (StorageClassAnalysisDataExport)
  }
derive instance newtypeStorageClassAnalysis :: Newtype StorageClassAnalysis _
derive instance repGenericStorageClassAnalysis :: Generic StorageClassAnalysis _
instance showStorageClassAnalysis :: Show StorageClassAnalysis where
  show = genericShow
instance decodeStorageClassAnalysis :: Decode StorageClassAnalysis where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageClassAnalysis :: Encode StorageClassAnalysis where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StorageClassAnalysisDataExport = StorageClassAnalysisDataExport 
  { "OutputSchemaVersion" :: (StorageClassAnalysisSchemaVersion)
  , "Destination" :: (AnalyticsExportDestination)
  }
derive instance newtypeStorageClassAnalysisDataExport :: Newtype StorageClassAnalysisDataExport _
derive instance repGenericStorageClassAnalysisDataExport :: Generic StorageClassAnalysisDataExport _
instance showStorageClassAnalysisDataExport :: Show StorageClassAnalysisDataExport where
  show = genericShow
instance decodeStorageClassAnalysisDataExport :: Decode StorageClassAnalysisDataExport where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageClassAnalysisDataExport :: Encode StorageClassAnalysisDataExport where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StorageClassAnalysisSchemaVersion = StorageClassAnalysisSchemaVersion String
derive instance newtypeStorageClassAnalysisSchemaVersion :: Newtype StorageClassAnalysisSchemaVersion _
derive instance repGenericStorageClassAnalysisSchemaVersion :: Generic StorageClassAnalysisSchemaVersion _
instance showStorageClassAnalysisSchemaVersion :: Show StorageClassAnalysisSchemaVersion where
  show = genericShow
instance decodeStorageClassAnalysisSchemaVersion :: Decode StorageClassAnalysisSchemaVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageClassAnalysisSchemaVersion :: Encode StorageClassAnalysisSchemaVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Suffix = Suffix String
derive instance newtypeSuffix :: Newtype Suffix _
derive instance repGenericSuffix :: Generic Suffix _
instance showSuffix :: Show Suffix where
  show = genericShow
instance decodeSuffix :: Decode Suffix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSuffix :: Encode Suffix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tag = Tag 
  { "Key" :: (ObjectKey)
  , "Value" :: (Value)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagCount = TagCount Int
derive instance newtypeTagCount :: Newtype TagCount _
derive instance repGenericTagCount :: Generic TagCount _
instance showTagCount :: Show TagCount where
  show = genericShow
instance decodeTagCount :: Decode TagCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagCount :: Encode TagCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagSet = TagSet (Array Tag)
derive instance newtypeTagSet :: Newtype TagSet _
derive instance repGenericTagSet :: Generic TagSet _
instance showTagSet :: Show TagSet where
  show = genericShow
instance decodeTagSet :: Decode TagSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagSet :: Encode TagSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tagging = Tagging 
  { "TagSet" :: (TagSet)
  }
derive instance newtypeTagging :: Newtype Tagging _
derive instance repGenericTagging :: Generic Tagging _
instance showTagging :: Show Tagging where
  show = genericShow
instance decodeTagging :: Decode Tagging where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagging :: Encode Tagging where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaggingDirective = TaggingDirective String
derive instance newtypeTaggingDirective :: Newtype TaggingDirective _
derive instance repGenericTaggingDirective :: Generic TaggingDirective _
instance showTaggingDirective :: Show TaggingDirective where
  show = genericShow
instance decodeTaggingDirective :: Decode TaggingDirective where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaggingDirective :: Encode TaggingDirective where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaggingHeader = TaggingHeader String
derive instance newtypeTaggingHeader :: Newtype TaggingHeader _
derive instance repGenericTaggingHeader :: Generic TaggingHeader _
instance showTaggingHeader :: Show TaggingHeader where
  show = genericShow
instance decodeTaggingHeader :: Decode TaggingHeader where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaggingHeader :: Encode TaggingHeader where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetBucket = TargetBucket String
derive instance newtypeTargetBucket :: Newtype TargetBucket _
derive instance repGenericTargetBucket :: Generic TargetBucket _
instance showTargetBucket :: Show TargetBucket where
  show = genericShow
instance decodeTargetBucket :: Decode TargetBucket where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetBucket :: Encode TargetBucket where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetGrant = TargetGrant 
  { "Grantee" :: NullOrUndefined.NullOrUndefined (Grantee)
  , "Permission" :: NullOrUndefined.NullOrUndefined (BucketLogsPermission)
  }
derive instance newtypeTargetGrant :: Newtype TargetGrant _
derive instance repGenericTargetGrant :: Generic TargetGrant _
instance showTargetGrant :: Show TargetGrant where
  show = genericShow
instance decodeTargetGrant :: Decode TargetGrant where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGrant :: Encode TargetGrant where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetGrants = TargetGrants (Array TargetGrant)
derive instance newtypeTargetGrants :: Newtype TargetGrants _
derive instance repGenericTargetGrants :: Generic TargetGrants _
instance showTargetGrants :: Show TargetGrants where
  show = genericShow
instance decodeTargetGrants :: Decode TargetGrants where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGrants :: Encode TargetGrants where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetPrefix = TargetPrefix String
derive instance newtypeTargetPrefix :: Newtype TargetPrefix _
derive instance repGenericTargetPrefix :: Generic TargetPrefix _
instance showTargetPrefix :: Show TargetPrefix where
  show = genericShow
instance decodeTargetPrefix :: Decode TargetPrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetPrefix :: Encode TargetPrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tier = Tier String
derive instance newtypeTier :: Newtype Tier _
derive instance repGenericTier :: Generic Tier _
instance showTier :: Show Tier where
  show = genericShow
instance decodeTier :: Decode Tier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTier :: Encode Tier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Token = Token String
derive instance newtypeToken :: Newtype Token _
derive instance repGenericToken :: Generic Token _
instance showToken :: Show Token where
  show = genericShow
instance decodeToken :: Decode Token where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeToken :: Encode Token where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicArn = TopicArn String
derive instance newtypeTopicArn :: Newtype TopicArn _
derive instance repGenericTopicArn :: Generic TopicArn _
instance showTopicArn :: Show TopicArn where
  show = genericShow
instance decodeTopicArn :: Decode TopicArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicArn :: Encode TopicArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for specifying the configuration when you want Amazon S3 to publish events to an Amazon Simple Notification Service (Amazon SNS) topic.
newtype TopicConfiguration = TopicConfiguration 
  { "Id" :: NullOrUndefined.NullOrUndefined (NotificationId)
  , "TopicArn" :: (TopicArn)
  , "Events" :: (EventList)
  , "Filter" :: NullOrUndefined.NullOrUndefined (NotificationConfigurationFilter)
  }
derive instance newtypeTopicConfiguration :: Newtype TopicConfiguration _
derive instance repGenericTopicConfiguration :: Generic TopicConfiguration _
instance showTopicConfiguration :: Show TopicConfiguration where
  show = genericShow
instance decodeTopicConfiguration :: Decode TopicConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicConfiguration :: Encode TopicConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicConfigurationDeprecated = TopicConfigurationDeprecated 
  { "Id" :: NullOrUndefined.NullOrUndefined (NotificationId)
  , "Events" :: NullOrUndefined.NullOrUndefined (EventList)
  , "Event" :: NullOrUndefined.NullOrUndefined (Event)
  , "Topic" :: NullOrUndefined.NullOrUndefined (TopicArn)
  }
derive instance newtypeTopicConfigurationDeprecated :: Newtype TopicConfigurationDeprecated _
derive instance repGenericTopicConfigurationDeprecated :: Generic TopicConfigurationDeprecated _
instance showTopicConfigurationDeprecated :: Show TopicConfigurationDeprecated where
  show = genericShow
instance decodeTopicConfigurationDeprecated :: Decode TopicConfigurationDeprecated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicConfigurationDeprecated :: Encode TopicConfigurationDeprecated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicConfigurationList = TopicConfigurationList (Array TopicConfiguration)
derive instance newtypeTopicConfigurationList :: Newtype TopicConfigurationList _
derive instance repGenericTopicConfigurationList :: Generic TopicConfigurationList _
instance showTopicConfigurationList :: Show TopicConfigurationList where
  show = genericShow
instance decodeTopicConfigurationList :: Decode TopicConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicConfigurationList :: Encode TopicConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Transition = Transition 
  { "Date" :: NullOrUndefined.NullOrUndefined (Date)
  , "Days" :: NullOrUndefined.NullOrUndefined (Days)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (TransitionStorageClass)
  }
derive instance newtypeTransition :: Newtype Transition _
derive instance repGenericTransition :: Generic Transition _
instance showTransition :: Show Transition where
  show = genericShow
instance decodeTransition :: Decode Transition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransition :: Encode Transition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TransitionList = TransitionList (Array Transition)
derive instance newtypeTransitionList :: Newtype TransitionList _
derive instance repGenericTransitionList :: Generic TransitionList _
instance showTransitionList :: Show TransitionList where
  show = genericShow
instance decodeTransitionList :: Decode TransitionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransitionList :: Encode TransitionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TransitionStorageClass = TransitionStorageClass String
derive instance newtypeTransitionStorageClass :: Newtype TransitionStorageClass _
derive instance repGenericTransitionStorageClass :: Generic TransitionStorageClass _
instance showTransitionStorageClass :: Show TransitionStorageClass where
  show = genericShow
instance decodeTransitionStorageClass :: Decode TransitionStorageClass where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransitionStorageClass :: Encode TransitionStorageClass where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Type = Type String
derive instance newtypeType :: Newtype Type _
derive instance repGenericType :: Generic Type _
instance showType :: Show Type where
  show = genericShow
instance decodeType :: Decode Type where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeType :: Encode Type where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype URI = URI String
derive instance newtypeURI :: Newtype URI _
derive instance repGenericURI :: Generic URI _
instance showURI :: Show URI where
  show = genericShow
instance decodeURI :: Decode URI where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeURI :: Encode URI where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UploadIdMarker = UploadIdMarker String
derive instance newtypeUploadIdMarker :: Newtype UploadIdMarker _
derive instance repGenericUploadIdMarker :: Generic UploadIdMarker _
instance showUploadIdMarker :: Show UploadIdMarker where
  show = genericShow
instance decodeUploadIdMarker :: Decode UploadIdMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadIdMarker :: Encode UploadIdMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UploadPartCopyOutput = UploadPartCopyOutput 
  { "CopySourceVersionId" :: NullOrUndefined.NullOrUndefined (CopySourceVersionId)
  , "CopyPartResult" :: NullOrUndefined.NullOrUndefined (CopyPartResult)
  , "ServerSideEncryption" :: NullOrUndefined.NullOrUndefined (ServerSideEncryption)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  }
derive instance newtypeUploadPartCopyOutput :: Newtype UploadPartCopyOutput _
derive instance repGenericUploadPartCopyOutput :: Generic UploadPartCopyOutput _
instance showUploadPartCopyOutput :: Show UploadPartCopyOutput where
  show = genericShow
instance decodeUploadPartCopyOutput :: Decode UploadPartCopyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadPartCopyOutput :: Encode UploadPartCopyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UploadPartCopyRequest = UploadPartCopyRequest 
  { "Bucket" :: (BucketName)
  , "CopySource" :: (CopySource)
  , "CopySourceIfMatch" :: NullOrUndefined.NullOrUndefined (CopySourceIfMatch)
  , "CopySourceIfModifiedSince" :: NullOrUndefined.NullOrUndefined (CopySourceIfModifiedSince)
  , "CopySourceIfNoneMatch" :: NullOrUndefined.NullOrUndefined (CopySourceIfNoneMatch)
  , "CopySourceIfUnmodifiedSince" :: NullOrUndefined.NullOrUndefined (CopySourceIfUnmodifiedSince)
  , "CopySourceRange" :: NullOrUndefined.NullOrUndefined (CopySourceRange)
  , "Key" :: (ObjectKey)
  , "PartNumber" :: (PartNumber)
  , "UploadId" :: (MultipartUploadId)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined.NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "CopySourceSSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (CopySourceSSECustomerAlgorithm)
  , "CopySourceSSECustomerKey" :: NullOrUndefined.NullOrUndefined (CopySourceSSECustomerKey)
  , "CopySourceSSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (CopySourceSSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  }
derive instance newtypeUploadPartCopyRequest :: Newtype UploadPartCopyRequest _
derive instance repGenericUploadPartCopyRequest :: Generic UploadPartCopyRequest _
instance showUploadPartCopyRequest :: Show UploadPartCopyRequest where
  show = genericShow
instance decodeUploadPartCopyRequest :: Decode UploadPartCopyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadPartCopyRequest :: Encode UploadPartCopyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UploadPartOutput = UploadPartOutput 
  { "ServerSideEncryption" :: NullOrUndefined.NullOrUndefined (ServerSideEncryption)
  , "ETag" :: NullOrUndefined.NullOrUndefined (ETag)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined.NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined.NullOrUndefined (RequestCharged)
  }
derive instance newtypeUploadPartOutput :: Newtype UploadPartOutput _
derive instance repGenericUploadPartOutput :: Generic UploadPartOutput _
instance showUploadPartOutput :: Show UploadPartOutput where
  show = genericShow
instance decodeUploadPartOutput :: Decode UploadPartOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadPartOutput :: Encode UploadPartOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UploadPartRequest = UploadPartRequest 
  { "Body" :: NullOrUndefined.NullOrUndefined (Body)
  , "Bucket" :: (BucketName)
  , "ContentLength" :: NullOrUndefined.NullOrUndefined (ContentLength)
  , "ContentMD5" :: NullOrUndefined.NullOrUndefined (ContentMD5)
  , "Key" :: (ObjectKey)
  , "PartNumber" :: (PartNumber)
  , "UploadId" :: (MultipartUploadId)
  , "SSECustomerAlgorithm" :: NullOrUndefined.NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined.NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined.NullOrUndefined (SSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined.NullOrUndefined (RequestPayer)
  }
derive instance newtypeUploadPartRequest :: Newtype UploadPartRequest _
derive instance repGenericUploadPartRequest :: Generic UploadPartRequest _
instance showUploadPartRequest :: Show UploadPartRequest where
  show = genericShow
instance decodeUploadPartRequest :: Decode UploadPartRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadPartRequest :: Encode UploadPartRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserMetadata = UserMetadata (Array MetadataEntry)
derive instance newtypeUserMetadata :: Newtype UserMetadata _
derive instance repGenericUserMetadata :: Generic UserMetadata _
instance showUserMetadata :: Show UserMetadata where
  show = genericShow
instance decodeUserMetadata :: Decode UserMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserMetadata :: Encode UserMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Value = Value String
derive instance newtypeValue :: Newtype Value _
derive instance repGenericValue :: Generic Value _
instance showValue :: Show Value where
  show = genericShow
instance decodeValue :: Decode Value where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValue :: Encode Value where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VersionIdMarker = VersionIdMarker String
derive instance newtypeVersionIdMarker :: Newtype VersionIdMarker _
derive instance repGenericVersionIdMarker :: Generic VersionIdMarker _
instance showVersionIdMarker :: Show VersionIdMarker where
  show = genericShow
instance decodeVersionIdMarker :: Decode VersionIdMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionIdMarker :: Encode VersionIdMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VersioningConfiguration = VersioningConfiguration 
  { "MFADelete" :: NullOrUndefined.NullOrUndefined (MFADelete)
  , "Status" :: NullOrUndefined.NullOrUndefined (BucketVersioningStatus)
  }
derive instance newtypeVersioningConfiguration :: Newtype VersioningConfiguration _
derive instance repGenericVersioningConfiguration :: Generic VersioningConfiguration _
instance showVersioningConfiguration :: Show VersioningConfiguration where
  show = genericShow
instance decodeVersioningConfiguration :: Decode VersioningConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersioningConfiguration :: Encode VersioningConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WebsiteConfiguration = WebsiteConfiguration 
  { "ErrorDocument" :: NullOrUndefined.NullOrUndefined (ErrorDocument)
  , "IndexDocument" :: NullOrUndefined.NullOrUndefined (IndexDocument)
  , "RedirectAllRequestsTo" :: NullOrUndefined.NullOrUndefined (RedirectAllRequestsTo)
  , "RoutingRules" :: NullOrUndefined.NullOrUndefined (RoutingRules)
  }
derive instance newtypeWebsiteConfiguration :: Newtype WebsiteConfiguration _
derive instance repGenericWebsiteConfiguration :: Generic WebsiteConfiguration _
instance showWebsiteConfiguration :: Show WebsiteConfiguration where
  show = genericShow
instance decodeWebsiteConfiguration :: Decode WebsiteConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWebsiteConfiguration :: Encode WebsiteConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WebsiteRedirectLocation = WebsiteRedirectLocation String
derive instance newtypeWebsiteRedirectLocation :: Newtype WebsiteRedirectLocation _
derive instance repGenericWebsiteRedirectLocation :: Generic WebsiteRedirectLocation _
instance showWebsiteRedirectLocation :: Show WebsiteRedirectLocation where
  show = genericShow
instance decodeWebsiteRedirectLocation :: Decode WebsiteRedirectLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWebsiteRedirectLocation :: Encode WebsiteRedirectLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
