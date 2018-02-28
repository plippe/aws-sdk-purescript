

-- | <p>Amazon Glacier is a storage solution for "cold data."</p> <p>Amazon Glacier is an extremely low-cost storage service that provides secure, durable, and easy-to-use storage for data backup and archival. With Amazon Glacier, customers can store their data cost effectively for months, years, or decades. Amazon Glacier also enables customers to offload the administrative burdens of operating and scaling storage to AWS, so they don't have to worry about capacity planning, hardware provisioning, data replication, hardware failure and recovery, or time-consuming hardware migrations.</p> <p>Amazon Glacier is a great storage choice when low storage cost is paramount, your data is rarely retrieved, and retrieval latency of several hours is acceptable. If your application requires fast or frequent access to your data, consider using Amazon S3. For more information, see <a href="http://aws.amazon.com/s3/">Amazon Simple Storage Service (Amazon S3)</a>.</p> <p>You can store any kind of data in any format. There is no maximum limit on the total amount of data you can store in Amazon Glacier.</p> <p>If you are a first-time user of Amazon Glacier, we recommend that you begin by reading the following sections in the <i>Amazon Glacier Developer Guide</i>:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/introduction.html">What is Amazon Glacier</a> - This section of the Developer Guide describes the underlying data model, the operations it supports, and the AWS SDKs that you can use to interact with the service.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/amazon-glacier-getting-started.html">Getting Started with Amazon Glacier</a> - The Getting Started section walks you through the process of creating a vault, uploading archives, creating jobs to download archives, retrieving the job output, and deleting archives.</p> </li> </ul>
module AWS.Glacier where

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

serviceName = "Glacier" :: String


-- | <p>This operation aborts a multipart upload identified by the upload ID.</p> <p>After the Abort Multipart Upload request succeeds, you cannot upload any more parts to the multipart upload or complete the multipart upload. Aborting a completed upload fails. However, aborting an already-aborted upload will succeed, for a short time. For more information about uploading a part and completing a multipart upload, see <a>UploadMultipartPart</a> and <a>CompleteMultipartUpload</a>.</p> <p>This operation is idempotent.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html">Working with Archives in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html">Abort Multipart Upload</a> in the <i>Amazon Glacier Developer Guide</i>. </p>
abortMultipartUpload :: forall eff. AbortMultipartUploadInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
abortMultipartUpload = Request.request serviceName "abortMultipartUpload" 


-- | <p>This operation aborts the vault locking process if the vault lock is not in the <code>Locked</code> state. If the vault lock is in the <code>Locked</code> state when this operation is requested, the operation returns an <code>AccessDeniedException</code> error. Aborting the vault locking process removes the vault lock policy from the specified vault. </p> <p>A vault lock is put into the <code>InProgress</code> state by calling <a>InitiateVaultLock</a>. A vault lock is put into the <code>Locked</code> state by calling <a>CompleteVaultLock</a>. You can get the state of a vault lock by calling <a>GetVaultLock</a>. For more information about the vault locking process, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html">Amazon Glacier Vault Lock</a>. For more information about vault lock policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html">Amazon Glacier Access Control with Vault Lock Policies</a>. </p> <p>This operation is idempotent. You can successfully invoke this operation multiple times, if the vault lock is in the <code>InProgress</code> state or if there is no policy associated with the vault.</p>
abortVaultLock :: forall eff. AbortVaultLockInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
abortVaultLock = Request.request serviceName "abortVaultLock" 


-- | <p>This operation adds the specified tags to a vault. Each tag is composed of a key and a value. Each vault can have up to 10 tags. If your request would cause the tag limit for the vault to be exceeded, the operation throws the <code>LimitExceededException</code> error. If a tag already exists on the vault under a specified key, the existing key value will be overwritten. For more information about tags, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html">Tagging Amazon Glacier Resources</a>. </p>
addTagsToVault :: forall eff. AddTagsToVaultInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
addTagsToVault = Request.request serviceName "addTagsToVault" 


-- | <p>You call this operation to inform Amazon Glacier that all the archive parts have been uploaded and that Amazon Glacier can now assemble the archive from the uploaded parts. After assembling and saving the archive to the vault, Amazon Glacier returns the URI path of the newly created archive resource. Using the URI path, you can then access the archive. After you upload an archive, you should save the archive ID returned to retrieve the archive at a later point. You can also get the vault inventory to obtain a list of archive IDs in a vault. For more information, see <a>InitiateJob</a>.</p> <p>In the request, you must include the computed SHA256 tree hash of the entire archive you have uploaded. For information about computing a SHA256 tree hash, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html">Computing Checksums</a>. On the server side, Amazon Glacier also constructs the SHA256 tree hash of the assembled archive. If the values match, Amazon Glacier saves the archive to the vault; otherwise, it returns an error, and the operation fails. The <a>ListParts</a> operation returns a list of parts uploaded for a specific multipart upload. It includes checksum information for each uploaded part that can be used to debug a bad checksum issue.</p> <p>Additionally, Amazon Glacier also checks for any missing content ranges when assembling the archive, if missing content ranges are found, Amazon Glacier returns an error and the operation fails.</p> <p>Complete Multipart Upload is an idempotent operation. After your first successful complete multipart upload, if you call the operation again within a short period, the operation will succeed and return the same archive ID. This is useful in the event you experience a network issue that causes an aborted connection or receive a 500 server error, in which case you can repeat your Complete Multipart Upload request and get the same archive ID without creating duplicate archives. Note, however, that after the multipart upload completes, you cannot call the List Parts operation and the multipart upload will not appear in List Multipart Uploads response, even if idempotent complete is possible.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html">Uploading Large Archives in Parts (Multipart Upload)</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-complete-upload.html">Complete Multipart Upload</a> in the <i>Amazon Glacier Developer Guide</i>. </p>
completeMultipartUpload :: forall eff. CompleteMultipartUploadInput -> Aff (exception :: EXCEPTION | eff) ArchiveCreationOutput
completeMultipartUpload = Request.request serviceName "completeMultipartUpload" 


-- | <p>This operation completes the vault locking process by transitioning the vault lock from the <code>InProgress</code> state to the <code>Locked</code> state, which causes the vault lock policy to become unchangeable. A vault lock is put into the <code>InProgress</code> state by calling <a>InitiateVaultLock</a>. You can obtain the state of the vault lock by calling <a>GetVaultLock</a>. For more information about the vault locking process, <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html">Amazon Glacier Vault Lock</a>. </p> <p>This operation is idempotent. This request is always successful if the vault lock is in the <code>Locked</code> state and the provided lock ID matches the lock ID originally used to lock the vault.</p> <p>If an invalid lock ID is passed in the request when the vault lock is in the <code>Locked</code> state, the operation returns an <code>AccessDeniedException</code> error. If an invalid lock ID is passed in the request when the vault lock is in the <code>InProgress</code> state, the operation throws an <code>InvalidParameter</code> error.</p>
completeVaultLock :: forall eff. CompleteVaultLockInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
completeVaultLock = Request.request serviceName "completeVaultLock" 


-- | <p>This operation creates a new vault with the specified name. The name of the vault must be unique within a region for an AWS account. You can create up to 1,000 vaults per account. If you need to create more vaults, contact Amazon Glacier.</p> <p>You must use the following guidelines when naming a vault.</p> <ul> <li> <p>Names can be between 1 and 255 characters long.</p> </li> <li> <p>Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), and '.' (period).</p> </li> </ul> <p>This operation is idempotent.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/creating-vaults.html">Creating a Vault in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-put.html">Create Vault </a> in the <i>Amazon Glacier Developer Guide</i>. </p>
createVault :: forall eff. CreateVaultInput -> Aff (exception :: EXCEPTION | eff) CreateVaultOutput
createVault = Request.request serviceName "createVault" 


-- | <p>This operation deletes an archive from a vault. Subsequent requests to initiate a retrieval of this archive will fail. Archive retrievals that are in progress for this archive ID may or may not succeed according to the following scenarios:</p> <ul> <li> <p>If the archive retrieval job is actively preparing the data for download when Amazon Glacier receives the delete archive request, the archival retrieval operation might fail.</p> </li> <li> <p>If the archive retrieval job has successfully prepared the archive for download when Amazon Glacier receives the delete archive request, you will be able to download the output.</p> </li> </ul> <p>This operation is idempotent. Attempting to delete an already-deleted archive does not result in an error.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-an-archive.html">Deleting an Archive in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html">Delete Archive</a> in the <i>Amazon Glacier Developer Guide</i>. </p>
deleteArchive :: forall eff. DeleteArchiveInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteArchive = Request.request serviceName "deleteArchive" 


-- | <p>This operation deletes a vault. Amazon Glacier will delete a vault only if there are no archives in the vault as of the last inventory and there have been no writes to the vault since the last inventory. If either of these conditions is not satisfied, the vault deletion fails (that is, the vault is not removed) and Amazon Glacier returns an error. You can use <a>DescribeVault</a> to return the number of archives in a vault, and you can use <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html">Initiate a Job (POST jobs)</a> to initiate a new inventory retrieval for a vault. The inventory contains the archive IDs you use to delete archives using <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html">Delete Archive (DELETE archive)</a>.</p> <p>This operation is idempotent.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-vaults.html">Deleting a Vault in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-delete.html">Delete Vault </a> in the <i>Amazon Glacier Developer Guide</i>. </p>
deleteVault :: forall eff. DeleteVaultInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteVault = Request.request serviceName "deleteVault" 


-- | <p>This operation deletes the access policy associated with the specified vault. The operation is eventually consistent; that is, it might take some time for Amazon Glacier to completely remove the access policy, and you might still see the effect of the policy for a short time after you send the delete request.</p> <p>This operation is idempotent. You can invoke delete multiple times, even if there is no policy associated with the vault. For more information about vault access policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html">Amazon Glacier Access Control with Vault Access Policies</a>. </p>
deleteVaultAccessPolicy :: forall eff. DeleteVaultAccessPolicyInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteVaultAccessPolicy = Request.request serviceName "deleteVaultAccessPolicy" 


-- | <p>This operation deletes the notification configuration set for a vault. The operation is eventually consistent; that is, it might take some time for Amazon Glacier to completely disable the notifications and you might still receive some notifications for a short time after you send the delete request.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html">Configuring Vault Notifications in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-delete.html">Delete Vault Notification Configuration </a> in the Amazon Glacier Developer Guide. </p>
deleteVaultNotifications :: forall eff. DeleteVaultNotificationsInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteVaultNotifications = Request.request serviceName "deleteVaultNotifications" 


-- | <p>This operation returns information about a job you previously initiated, including the job initiation date, the user who initiated the job, the job status code/message and the Amazon SNS topic to notify after Amazon Glacier completes the job. For more information about initiating a job, see <a>InitiateJob</a>. </p> <note> <p>This operation enables you to check the status of your job. However, it is strongly recommended that you set up an Amazon SNS topic and specify it in your initiate job request so that Amazon Glacier can notify the topic after it completes the job.</p> </note> <p>A job ID will not expire for at least 24 hours after Amazon Glacier completes the job.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For more information about using this operation, see the documentation for the underlying REST API <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-describe-job-get.html">Describe Job</a> in the <i>Amazon Glacier Developer Guide</i>. </p>
describeJob :: forall eff. DescribeJobInput -> Aff (exception :: EXCEPTION | eff) GlacierJobDescription
describeJob = Request.request serviceName "describeJob" 


-- | <p>This operation returns information about a vault, including the vault's Amazon Resource Name (ARN), the date the vault was created, the number of archives it contains, and the total size of all the archives in the vault. The number of archives and their total size are as of the last inventory generation. This means that if you add or remove an archive from a vault, and then immediately use Describe Vault, the change in contents will not be immediately reflected. If you want to retrieve the latest inventory of the vault, use <a>InitiateJob</a>. Amazon Glacier generates vault inventories approximately daily. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html">Downloading a Vault Inventory in Amazon Glacier</a>. </p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html">Retrieving Vault Metadata in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-get.html">Describe Vault </a> in the <i>Amazon Glacier Developer Guide</i>. </p>
describeVault :: forall eff. DescribeVaultInput -> Aff (exception :: EXCEPTION | eff) DescribeVaultOutput
describeVault = Request.request serviceName "describeVault" 


-- | <p>This operation returns the current data retrieval policy for the account and region specified in the GET request. For more information about data retrieval policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html">Amazon Glacier Data Retrieval Policies</a>.</p>
getDataRetrievalPolicy :: forall eff. GetDataRetrievalPolicyInput -> Aff (exception :: EXCEPTION | eff) GetDataRetrievalPolicyOutput
getDataRetrievalPolicy = Request.request serviceName "getDataRetrievalPolicy" 


-- | <p>This operation downloads the output of the job you initiated using <a>InitiateJob</a>. Depending on the job type you specified when you initiated the job, the output will be either the content of an archive or a vault inventory.</p> <p>You can download all the job output or download a portion of the output by specifying a byte range. In the case of an archive retrieval job, depending on the byte range you specify, Amazon Glacier returns the checksum for the portion of the data. You can compute the checksum on the client and verify that the values match to ensure the portion you downloaded is the correct data.</p> <p>A job ID will not expire for at least 24 hours after Amazon Glacier completes the job. That a byte range. For both archive and inventory retrieval jobs, you should verify the downloaded size against the size returned in the headers from the <b>Get Job Output</b> response.</p> <p>For archive retrieval jobs, you should also verify that the size is what you expected. If you download a portion of the output, the expected size is based on the range of bytes you specified. For example, if you specify a range of <code>bytes=0-1048575</code>, you should verify your download size is 1,048,576 bytes. If you download an entire archive, the expected size is the size of the archive when you uploaded it to Amazon Glacier The expected size is also returned in the headers from the <b>Get Job Output</b> response.</p> <p>In the case of an archive retrieval job, depending on the byte range you specify, Amazon Glacier returns the checksum for the portion of the data. To ensure the portion you downloaded is the correct data, compute the checksum on the client, verify that the values match, and verify that the size is what you expected.</p> <p>A job ID does not expire for at least 24 hours after Amazon Glacier completes the job. That is, you can download the job output within the 24 hours period after Amazon Glacier completes the job.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and the underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html">Downloading a Vault Inventory</a>, <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/downloading-an-archive.html">Downloading an Archive</a>, and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-job-output-get.html">Get Job Output </a> </p>
getJobOutput :: forall eff. GetJobOutputInput -> Aff (exception :: EXCEPTION | eff) GetJobOutputOutput
getJobOutput = Request.request serviceName "getJobOutput" 


-- | <p>This operation retrieves the <code>access-policy</code> subresource set on the vault; for more information on setting this subresource, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-SetVaultAccessPolicy.html">Set Vault Access Policy (PUT access-policy)</a>. If there is no access policy set on the vault, the operation returns a <code>404 Not found</code> error. For more information about vault access policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html">Amazon Glacier Access Control with Vault Access Policies</a>.</p>
getVaultAccessPolicy :: forall eff. GetVaultAccessPolicyInput -> Aff (exception :: EXCEPTION | eff) GetVaultAccessPolicyOutput
getVaultAccessPolicy = Request.request serviceName "getVaultAccessPolicy" 


-- | <p>This operation retrieves the following attributes from the <code>lock-policy</code> subresource set on the specified vault: </p> <ul> <li> <p>The vault lock policy set on the vault.</p> </li> <li> <p>The state of the vault lock, which is either <code>InProgess</code> or <code>Locked</code>.</p> </li> <li> <p>When the lock ID expires. The lock ID is used to complete the vault locking process.</p> </li> <li> <p>When the vault lock was initiated and put into the <code>InProgress</code> state.</p> </li> </ul> <p>A vault lock is put into the <code>InProgress</code> state by calling <a>InitiateVaultLock</a>. A vault lock is put into the <code>Locked</code> state by calling <a>CompleteVaultLock</a>. You can abort the vault locking process by calling <a>AbortVaultLock</a>. For more information about the vault locking process, <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html">Amazon Glacier Vault Lock</a>. </p> <p>If there is no vault lock policy set on the vault, the operation returns a <code>404 Not found</code> error. For more information about vault lock policies, <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html">Amazon Glacier Access Control with Vault Lock Policies</a>. </p>
getVaultLock :: forall eff. GetVaultLockInput -> Aff (exception :: EXCEPTION | eff) GetVaultLockOutput
getVaultLock = Request.request serviceName "getVaultLock" 


-- | <p>This operation retrieves the <code>notification-configuration</code> subresource of the specified vault.</p> <p>For information about setting a notification configuration on a vault, see <a>SetVaultNotifications</a>. If a notification configuration for a vault is not set, the operation returns a <code>404 Not Found</code> error. For more information about vault notifications, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html">Configuring Vault Notifications in Amazon Glacier</a>. </p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html">Configuring Vault Notifications in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-get.html">Get Vault Notification Configuration </a> in the <i>Amazon Glacier Developer Guide</i>. </p>
getVaultNotifications :: forall eff. GetVaultNotificationsInput -> Aff (exception :: EXCEPTION | eff) GetVaultNotificationsOutput
getVaultNotifications = Request.request serviceName "getVaultNotifications" 


-- | <p>This operation initiates a job of the specified type, which can be a select, an archival retrieval, or a vault retrieval. For more information about using this operation, see the documentation for the underlying REST API <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html">Initiate a Job</a>. </p>
initiateJob :: forall eff. InitiateJobInput -> Aff (exception :: EXCEPTION | eff) InitiateJobOutput
initiateJob = Request.request serviceName "initiateJob" 


-- | <p>This operation initiates a multipart upload. Amazon Glacier creates a multipart upload resource and returns its ID in the response. The multipart upload ID is used in subsequent requests to upload parts of an archive (see <a>UploadMultipartPart</a>).</p> <p>When you initiate a multipart upload, you specify the part size in number of bytes. The part size must be a megabyte (1024 KB) multiplied by a power of 2-for example, 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8 MB), and so on. The minimum allowable part size is 1 MB, and the maximum is 4 GB.</p> <p>Every part you upload to this resource (see <a>UploadMultipartPart</a>), except the last one, must have the same size. The last one can be the same size or smaller. For example, suppose you want to upload a 16.2 MB file. If you initiate the multipart upload with a part size of 4 MB, you will upload four parts of 4 MB each and one part of 0.2 MB. </p> <note> <p>You don't need to know the size of the archive when you start a multipart upload because Amazon Glacier does not require you to specify the overall archive size.</p> </note> <p>After you complete the multipart upload, Amazon Glacier removes the multipart upload resource referenced by the ID. Amazon Glacier also removes the multipart upload resource if you cancel the multipart upload or it may be removed if there is no activity for a period of 24 hours.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html">Uploading Large Archives in Parts (Multipart Upload)</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-initiate-upload.html">Initiate Multipart Upload</a> in the <i>Amazon Glacier Developer Guide</i>.</p>
initiateMultipartUpload :: forall eff. InitiateMultipartUploadInput -> Aff (exception :: EXCEPTION | eff) InitiateMultipartUploadOutput
initiateMultipartUpload = Request.request serviceName "initiateMultipartUpload" 


-- | <p>This operation initiates the vault locking process by doing the following:</p> <ul> <li> <p>Installing a vault lock policy on the specified vault.</p> </li> <li> <p>Setting the lock state of vault lock to <code>InProgress</code>.</p> </li> <li> <p>Returning a lock ID, which is used to complete the vault locking process.</p> </li> </ul> <p>You can set one vault lock policy for each vault and this policy can be up to 20 KB in size. For more information about vault lock policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html">Amazon Glacier Access Control with Vault Lock Policies</a>. </p> <p>You must complete the vault locking process within 24 hours after the vault lock enters the <code>InProgress</code> state. After the 24 hour window ends, the lock ID expires, the vault automatically exits the <code>InProgress</code> state, and the vault lock policy is removed from the vault. You call <a>CompleteVaultLock</a> to complete the vault locking process by setting the state of the vault lock to <code>Locked</code>. </p> <p>After a vault lock is in the <code>Locked</code> state, you cannot initiate a new vault lock for the vault.</p> <p>You can abort the vault locking process by calling <a>AbortVaultLock</a>. You can get the state of the vault lock by calling <a>GetVaultLock</a>. For more information about the vault locking process, <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html">Amazon Glacier Vault Lock</a>.</p> <p>If this operation is called when the vault lock is in the <code>InProgress</code> state, the operation returns an <code>AccessDeniedException</code> error. When the vault lock is in the <code>InProgress</code> state you must call <a>AbortVaultLock</a> before you can initiate a new vault lock policy. </p>
initiateVaultLock :: forall eff. InitiateVaultLockInput -> Aff (exception :: EXCEPTION | eff) InitiateVaultLockOutput
initiateVaultLock = Request.request serviceName "initiateVaultLock" 


-- | <p>This operation lists jobs for a vault, including jobs that are in-progress and jobs that have recently finished. The List Job operation returns a list of these jobs sorted by job initiation time.</p> <note> <p>Amazon Glacier retains recently completed jobs for a period before deleting them; however, it eventually removes completed jobs. The output of completed jobs can be retrieved. Retaining completed jobs for a period of time after they have completed enables you to get a job output in the event you miss the job completion notification or your first attempt to download it fails. For example, suppose you start an archive retrieval job to download an archive. After the job completes, you start to download the archive but encounter a network error. In this scenario, you can retry and download the archive while the job exists.</p> </note> <p>The List Jobs operation supports pagination. You should always check the response <code>Marker</code> field. If there are no more jobs to list, the <code>Marker</code> field is set to <code>null</code>. If there are more jobs to list, the <code>Marker</code> field is set to a non-null value, which you can use to continue the pagination of the list. To return a list of jobs that begins at a specific job, set the marker request parameter to the <code>Marker</code> value for that job that you obtained from a previous List Jobs request.</p> <p>You can set a maximum limit for the number of jobs returned in the response by specifying the <code>limit</code> parameter in the request. The default limit is 1000. The number of jobs returned might be fewer than the limit, but the number of returned jobs never exceeds the limit.</p> <p>Additionally, you can filter the jobs list returned by specifying the optional <code>statuscode</code> parameter or <code>completed</code> parameter, or both. Using the <code>statuscode</code> parameter, you can specify to return only jobs that match either the <code>InProgress</code>, <code>Succeeded</code>, or <code>Failed</code> status. Using the <code>completed</code> parameter, you can specify to return only jobs that were completed (<code>true</code>) or jobs that were not completed (<code>false</code>).</p> <p>For more information about using this operation, see the documentation for the underlying REST API <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-jobs-get.html">List Jobs</a>. </p>
listJobs :: forall eff. ListJobsInput -> Aff (exception :: EXCEPTION | eff) ListJobsOutput
listJobs = Request.request serviceName "listJobs" 


-- | <p>This operation lists in-progress multipart uploads for the specified vault. An in-progress multipart upload is a multipart upload that has been initiated by an <a>InitiateMultipartUpload</a> request, but has not yet been completed or aborted. The list returned in the List Multipart Upload response has no guaranteed order. </p> <p>The List Multipart Uploads operation supports pagination. By default, this operation returns up to 1,000 multipart uploads in the response. You should always check the response for a <code>marker</code> at which to continue the list; if there are no more items the <code>marker</code> is <code>null</code>. To return a list of multipart uploads that begins at a specific upload, set the <code>marker</code> request parameter to the value you obtained from a previous List Multipart Upload request. You can also limit the number of uploads returned in the response by specifying the <code>limit</code> parameter in the request.</p> <p>Note the difference between this operation and listing parts (<a>ListParts</a>). The List Multipart Uploads operation lists all multipart uploads for a vault and does not require a multipart upload ID. The List Parts operation requires a multipart upload ID since parts are associated with a single upload.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and the underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html">Working with Archives in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-uploads.html">List Multipart Uploads </a> in the <i>Amazon Glacier Developer Guide</i>.</p>
listMultipartUploads :: forall eff. ListMultipartUploadsInput -> Aff (exception :: EXCEPTION | eff) ListMultipartUploadsOutput
listMultipartUploads = Request.request serviceName "listMultipartUploads" 


-- | <p>This operation lists the parts of an archive that have been uploaded in a specific multipart upload. You can make this request at any time during an in-progress multipart upload before you complete the upload (see <a>CompleteMultipartUpload</a>. List Parts returns an error for completed uploads. The list returned in the List Parts response is sorted by part range. </p> <p>The List Parts operation supports pagination. By default, this operation returns up to 1,000 uploaded parts in the response. You should always check the response for a <code>marker</code> at which to continue the list; if there are no more items the <code>marker</code> is <code>null</code>. To return a list of parts that begins at a specific part, set the <code>marker</code> request parameter to the value you obtained from a previous List Parts request. You can also limit the number of parts returned in the response by specifying the <code>limit</code> parameter in the request. </p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and the underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html">Working with Archives in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-parts.html">List Parts</a> in the <i>Amazon Glacier Developer Guide</i>.</p>
listParts :: forall eff. ListPartsInput -> Aff (exception :: EXCEPTION | eff) ListPartsOutput
listParts = Request.request serviceName "listParts" 


-- | <p>This operation lists the provisioned capacity units for the specified AWS account.</p>
listProvisionedCapacity :: forall eff. ListProvisionedCapacityInput -> Aff (exception :: EXCEPTION | eff) ListProvisionedCapacityOutput
listProvisionedCapacity = Request.request serviceName "listProvisionedCapacity" 


-- | <p>This operation lists all the tags attached to a vault. The operation returns an empty map if there are no tags. For more information about tags, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html">Tagging Amazon Glacier Resources</a>.</p>
listTagsForVault :: forall eff. ListTagsForVaultInput -> Aff (exception :: EXCEPTION | eff) ListTagsForVaultOutput
listTagsForVault = Request.request serviceName "listTagsForVault" 


-- | <p>This operation lists all vaults owned by the calling user's account. The list returned in the response is ASCII-sorted by vault name.</p> <p>By default, this operation returns up to 1,000 items. If there are more vaults to list, the response <code>marker</code> field contains the vault Amazon Resource Name (ARN) at which to continue the list with a new List Vaults request; otherwise, the <code>marker</code> field is <code>null</code>. To return a list of vaults that begins at a specific vault, set the <code>marker</code> request parameter to the vault ARN you obtained from a previous List Vaults request. You can also limit the number of vaults returned in the response by specifying the <code>limit</code> parameter in the request. </p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html">Retrieving Vault Metadata in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vaults-get.html">List Vaults </a> in the <i>Amazon Glacier Developer Guide</i>. </p>
listVaults :: forall eff. ListVaultsInput -> Aff (exception :: EXCEPTION | eff) ListVaultsOutput
listVaults = Request.request serviceName "listVaults" 


-- | <p>This operation purchases a provisioned capacity unit for an AWS account. </p>
purchaseProvisionedCapacity :: forall eff. PurchaseProvisionedCapacityInput -> Aff (exception :: EXCEPTION | eff) PurchaseProvisionedCapacityOutput
purchaseProvisionedCapacity = Request.request serviceName "purchaseProvisionedCapacity" 


-- | <p>This operation removes one or more tags from the set of tags attached to a vault. For more information about tags, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html">Tagging Amazon Glacier Resources</a>. This operation is idempotent. The operation will be successful, even if there are no tags attached to the vault. </p>
removeTagsFromVault :: forall eff. RemoveTagsFromVaultInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
removeTagsFromVault = Request.request serviceName "removeTagsFromVault" 


-- | <p>This operation sets and then enacts a data retrieval policy in the region specified in the PUT request. You can set one policy per region for an AWS account. The policy is enacted within a few minutes of a successful PUT operation.</p> <p>The set policy operation does not affect retrieval jobs that were in progress before the policy was enacted. For more information about data retrieval policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html">Amazon Glacier Data Retrieval Policies</a>. </p>
setDataRetrievalPolicy :: forall eff. SetDataRetrievalPolicyInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setDataRetrievalPolicy = Request.request serviceName "setDataRetrievalPolicy" 


-- | <p>This operation configures an access policy for a vault and will overwrite an existing policy. To configure a vault access policy, send a PUT request to the <code>access-policy</code> subresource of the vault. An access policy is specific to a vault and is also called a vault subresource. You can set one access policy per vault and the policy can be up to 20 KB in size. For more information about vault access policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html">Amazon Glacier Access Control with Vault Access Policies</a>. </p>
setVaultAccessPolicy :: forall eff. SetVaultAccessPolicyInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setVaultAccessPolicy = Request.request serviceName "setVaultAccessPolicy" 


-- | <p>This operation configures notifications that will be sent when specific events happen to a vault. By default, you don't get any notifications.</p> <p>To configure vault notifications, send a PUT request to the <code>notification-configuration</code> subresource of the vault. The request should include a JSON document that provides an Amazon SNS topic and specific events for which you want Amazon Glacier to send notifications to the topic.</p> <p>Amazon SNS topics must grant permission to the vault to be allowed to publish notifications to the topic. You can configure a vault to publish a notification for the following vault events:</p> <ul> <li> <p> <b>ArchiveRetrievalCompleted</b> This event occurs when a job that was initiated for an archive retrieval is completed (<a>InitiateJob</a>). The status of the completed job can be "Succeeded" or "Failed". The notification sent to the SNS topic is the same output as returned from <a>DescribeJob</a>. </p> </li> <li> <p> <b>InventoryRetrievalCompleted</b> This event occurs when a job that was initiated for an inventory retrieval is completed (<a>InitiateJob</a>). The status of the completed job can be "Succeeded" or "Failed". The notification sent to the SNS topic is the same output as returned from <a>DescribeJob</a>. </p> </li> </ul> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html">Configuring Vault Notifications in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-put.html">Set Vault Notification Configuration </a> in the <i>Amazon Glacier Developer Guide</i>. </p>
setVaultNotifications :: forall eff. SetVaultNotificationsInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setVaultNotifications = Request.request serviceName "setVaultNotifications" 


-- | <p>This operation adds an archive to a vault. This is a synchronous operation, and for a successful upload, your data is durably persisted. Amazon Glacier returns the archive ID in the <code>x-amz-archive-id</code> header of the response. </p> <p>You must use the archive ID to access your data in Amazon Glacier. After you upload an archive, you should save the archive ID returned so that you can retrieve or delete the archive later. Besides saving the archive ID, you can also index it and give it a friendly name to allow for better searching. You can also use the optional archive description field to specify how the archive is referred to in an external index of archives, such as you might create in Amazon DynamoDB. You can also get the vault inventory to obtain a list of archive IDs in a vault. For more information, see <a>InitiateJob</a>. </p> <p>You must provide a SHA256 tree hash of the data you are uploading. For information about computing a SHA256 tree hash, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html">Computing Checksums</a>. </p> <p>You can optionally specify an archive description of up to 1,024 printable ASCII characters. You can get the archive description when you either retrieve the archive or get the vault inventory. For more information, see <a>InitiateJob</a>. Amazon Glacier does not interpret the description in any way. An archive description does not need to be unique. You cannot use the description to retrieve or sort the archive list. </p> <p>Archives are immutable. After you upload an archive, you cannot edit the archive or its description.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-an-archive.html">Uploading an Archive in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html">Upload Archive</a> in the <i>Amazon Glacier Developer Guide</i>. </p>
uploadArchive :: forall eff. UploadArchiveInput -> Aff (exception :: EXCEPTION | eff) ArchiveCreationOutput
uploadArchive = Request.request serviceName "uploadArchive" 


-- | <p>This operation uploads a part of an archive. You can upload archive parts in any order. You can also upload them in parallel. You can upload up to 10,000 parts for a multipart upload.</p> <p>Amazon Glacier rejects your upload part request if any of the following conditions is true:</p> <ul> <li> <p> <b>SHA256 tree hash does not match</b>To ensure that part data is not corrupted in transmission, you compute a SHA256 tree hash of the part and include it in your request. Upon receiving the part data, Amazon Glacier also computes a SHA256 tree hash. If these hash values don't match, the operation fails. For information about computing a SHA256 tree hash, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html">Computing Checksums</a>.</p> </li> <li> <p> <b>Part size does not match</b>The size of each part except the last must match the size specified in the corresponding <a>InitiateMultipartUpload</a> request. The size of the last part must be the same size as, or smaller than, the specified size.</p> <note> <p>If you upload a part whose size is smaller than the part size you specified in your initiate multipart upload request and that part is not the last part, then the upload part request will succeed. However, the subsequent Complete Multipart Upload request will fail.</p> </note> </li> <li> <p> <b>Range does not align</b>The byte range value in the request does not align with the part size specified in the corresponding initiate request. For example, if you specify a part size of 4194304 bytes (4 MB), then 0 to 4194303 bytes (4 MB - 1) and 4194304 (4 MB) to 8388607 (8 MB - 1) are valid part ranges. However, if you set a range value of 2 MB to 6 MB, the range does not align with the part size and the upload will fail. </p> </li> </ul> <p>This operation is idempotent. If you upload the same part multiple times, the data included in the most recent request overwrites the previously uploaded data.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html">Uploading Large Archives in Parts (Multipart Upload)</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-upload-part.html">Upload Part </a> in the <i>Amazon Glacier Developer Guide</i>.</p>
uploadMultipartPart :: forall eff. UploadMultipartPartInput -> Aff (exception :: EXCEPTION | eff) UploadMultipartPartOutput
uploadMultipartPart = Request.request serviceName "uploadMultipartPart" 


-- | <p>Provides options to abort a multipart upload identified by the upload ID.</p> <p>For information about the underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html">Abort Multipart Upload</a>. For conceptual information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html">Working with Archives in Amazon Glacier</a>.</p>
newtype AbortMultipartUploadInput = AbortMultipartUploadInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "UploadId'" :: (String)
  }
derive instance newtypeAbortMultipartUploadInput :: Newtype AbortMultipartUploadInput _
derive instance repGenericAbortMultipartUploadInput :: Generic AbortMultipartUploadInput _
instance showAbortMultipartUploadInput :: Show AbortMultipartUploadInput where
  show = genericShow
instance decodeAbortMultipartUploadInput :: Decode AbortMultipartUploadInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAbortMultipartUploadInput :: Encode AbortMultipartUploadInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input values for <code>AbortVaultLock</code>.</p>
newtype AbortVaultLockInput = AbortVaultLockInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }
derive instance newtypeAbortVaultLockInput :: Newtype AbortVaultLockInput _
derive instance repGenericAbortVaultLockInput :: Generic AbortVaultLockInput _
instance showAbortVaultLockInput :: Show AbortVaultLockInput where
  show = genericShow
instance decodeAbortVaultLockInput :: Decode AbortVaultLockInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAbortVaultLockInput :: Encode AbortVaultLockInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccessControlPolicyList = AccessControlPolicyList (Array Grant)
derive instance newtypeAccessControlPolicyList :: Newtype AccessControlPolicyList _
derive instance repGenericAccessControlPolicyList :: Generic AccessControlPolicyList _
instance showAccessControlPolicyList :: Show AccessControlPolicyList where
  show = genericShow
instance decodeAccessControlPolicyList :: Decode AccessControlPolicyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessControlPolicyList :: Encode AccessControlPolicyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionCode = ActionCode String
derive instance newtypeActionCode :: Newtype ActionCode _
derive instance repGenericActionCode :: Generic ActionCode _
instance showActionCode :: Show ActionCode where
  show = genericShow
instance decodeActionCode :: Decode ActionCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionCode :: Encode ActionCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input values for <code>AddTagsToVault</code>.</p>
newtype AddTagsToVaultInput = AddTagsToVaultInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagMap)
  }
derive instance newtypeAddTagsToVaultInput :: Newtype AddTagsToVaultInput _
derive instance repGenericAddTagsToVaultInput :: Generic AddTagsToVaultInput _
instance showAddTagsToVaultInput :: Show AddTagsToVaultInput where
  show = genericShow
instance decodeAddTagsToVaultInput :: Decode AddTagsToVaultInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsToVaultInput :: Encode AddTagsToVaultInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p> <p>For information about the underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html">Upload Archive</a>. For conceptual information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html">Working with Archives in Amazon Glacier</a>.</p>
newtype ArchiveCreationOutput = ArchiveCreationOutput 
  { "Location'" :: NullOrUndefined.NullOrUndefined (String)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "ArchiveId'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeArchiveCreationOutput :: Newtype ArchiveCreationOutput _
derive instance repGenericArchiveCreationOutput :: Generic ArchiveCreationOutput _
instance showArchiveCreationOutput :: Show ArchiveCreationOutput where
  show = genericShow
instance decodeArchiveCreationOutput :: Decode ArchiveCreationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArchiveCreationOutput :: Encode ArchiveCreationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the comma-separated value (CSV) file to select from.</p>
newtype CSVInput = CSVInput 
  { "FileHeaderInfo" :: NullOrUndefined.NullOrUndefined (FileHeaderInfo)
  , "Comments" :: NullOrUndefined.NullOrUndefined (String)
  , "QuoteEscapeCharacter" :: NullOrUndefined.NullOrUndefined (String)
  , "RecordDelimiter" :: NullOrUndefined.NullOrUndefined (String)
  , "FieldDelimiter" :: NullOrUndefined.NullOrUndefined (String)
  , "QuoteCharacter" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCSVInput :: Newtype CSVInput _
derive instance repGenericCSVInput :: Generic CSVInput _
instance showCSVInput :: Show CSVInput where
  show = genericShow
instance decodeCSVInput :: Decode CSVInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCSVInput :: Encode CSVInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the comma-separated value (CSV) file that the job results are stored in.</p>
newtype CSVOutput = CSVOutput 
  { "QuoteFields" :: NullOrUndefined.NullOrUndefined (QuoteFields)
  , "QuoteEscapeCharacter" :: NullOrUndefined.NullOrUndefined (String)
  , "RecordDelimiter" :: NullOrUndefined.NullOrUndefined (String)
  , "FieldDelimiter" :: NullOrUndefined.NullOrUndefined (String)
  , "QuoteCharacter" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCSVOutput :: Newtype CSVOutput _
derive instance repGenericCSVOutput :: Generic CSVOutput _
instance showCSVOutput :: Show CSVOutput where
  show = genericShow
instance decodeCSVOutput :: Decode CSVOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCSVOutput :: Encode CSVOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CannedACL = CannedACL String
derive instance newtypeCannedACL :: Newtype CannedACL _
derive instance repGenericCannedACL :: Generic CannedACL _
instance showCannedACL :: Show CannedACL where
  show = genericShow
instance decodeCannedACL :: Decode CannedACL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCannedACL :: Encode CannedACL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options to complete a multipart upload operation. This informs Amazon Glacier that all the archive parts have been uploaded and Amazon Glacier can now assemble the archive from the uploaded parts. After assembling and saving the archive to the vault, Amazon Glacier returns the URI path of the newly created archive resource.</p>
newtype CompleteMultipartUploadInput = CompleteMultipartUploadInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "UploadId'" :: (String)
  , "ArchiveSize'" :: NullOrUndefined.NullOrUndefined (String)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCompleteMultipartUploadInput :: Newtype CompleteMultipartUploadInput _
derive instance repGenericCompleteMultipartUploadInput :: Generic CompleteMultipartUploadInput _
instance showCompleteMultipartUploadInput :: Show CompleteMultipartUploadInput where
  show = genericShow
instance decodeCompleteMultipartUploadInput :: Decode CompleteMultipartUploadInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompleteMultipartUploadInput :: Encode CompleteMultipartUploadInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input values for <code>CompleteVaultLock</code>.</p>
newtype CompleteVaultLockInput = CompleteVaultLockInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "LockId'" :: (String)
  }
derive instance newtypeCompleteVaultLockInput :: Newtype CompleteVaultLockInput _
derive instance repGenericCompleteVaultLockInput :: Generic CompleteVaultLockInput _
instance showCompleteVaultLockInput :: Show CompleteVaultLockInput where
  show = genericShow
instance decodeCompleteVaultLockInput :: Decode CompleteVaultLockInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompleteVaultLockInput :: Encode CompleteVaultLockInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options to create a vault.</p>
newtype CreateVaultInput = CreateVaultInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }
derive instance newtypeCreateVaultInput :: Newtype CreateVaultInput _
derive instance repGenericCreateVaultInput :: Generic CreateVaultInput _
instance showCreateVaultInput :: Show CreateVaultInput where
  show = genericShow
instance decodeCreateVaultInput :: Decode CreateVaultInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateVaultInput :: Encode CreateVaultInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype CreateVaultOutput = CreateVaultOutput 
  { "Location'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateVaultOutput :: Newtype CreateVaultOutput _
derive instance repGenericCreateVaultOutput :: Generic CreateVaultOutput _
instance showCreateVaultOutput :: Show CreateVaultOutput where
  show = genericShow
instance decodeCreateVaultOutput :: Decode CreateVaultOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateVaultOutput :: Encode CreateVaultOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Data retrieval policy.</p>
newtype DataRetrievalPolicy = DataRetrievalPolicy 
  { "Rules" :: NullOrUndefined.NullOrUndefined (DataRetrievalRulesList)
  }
derive instance newtypeDataRetrievalPolicy :: Newtype DataRetrievalPolicy _
derive instance repGenericDataRetrievalPolicy :: Generic DataRetrievalPolicy _
instance showDataRetrievalPolicy :: Show DataRetrievalPolicy where
  show = genericShow
instance decodeDataRetrievalPolicy :: Decode DataRetrievalPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataRetrievalPolicy :: Encode DataRetrievalPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Data retrieval policy rule.</p>
newtype DataRetrievalRule = DataRetrievalRule 
  { "Strategy" :: NullOrUndefined.NullOrUndefined (String)
  , "BytesPerHour" :: NullOrUndefined.NullOrUndefined (NullableLong)
  }
derive instance newtypeDataRetrievalRule :: Newtype DataRetrievalRule _
derive instance repGenericDataRetrievalRule :: Generic DataRetrievalRule _
instance showDataRetrievalRule :: Show DataRetrievalRule where
  show = genericShow
instance decodeDataRetrievalRule :: Decode DataRetrievalRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataRetrievalRule :: Encode DataRetrievalRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataRetrievalRulesList = DataRetrievalRulesList (Array DataRetrievalRule)
derive instance newtypeDataRetrievalRulesList :: Newtype DataRetrievalRulesList _
derive instance repGenericDataRetrievalRulesList :: Generic DataRetrievalRulesList _
instance showDataRetrievalRulesList :: Show DataRetrievalRulesList where
  show = genericShow
instance decodeDataRetrievalRulesList :: Decode DataRetrievalRulesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataRetrievalRulesList :: Encode DataRetrievalRulesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DateTime = DateTime String
derive instance newtypeDateTime :: Newtype DateTime _
derive instance repGenericDateTime :: Generic DateTime _
instance showDateTime :: Show DateTime where
  show = genericShow
instance decodeDateTime :: Decode DateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDateTime :: Encode DateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for deleting an archive from an Amazon Glacier vault.</p>
newtype DeleteArchiveInput = DeleteArchiveInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "ArchiveId'" :: (String)
  }
derive instance newtypeDeleteArchiveInput :: Newtype DeleteArchiveInput _
derive instance repGenericDeleteArchiveInput :: Generic DeleteArchiveInput _
instance showDeleteArchiveInput :: Show DeleteArchiveInput where
  show = genericShow
instance decodeDeleteArchiveInput :: Decode DeleteArchiveInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteArchiveInput :: Encode DeleteArchiveInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DeleteVaultAccessPolicy input.</p>
newtype DeleteVaultAccessPolicyInput = DeleteVaultAccessPolicyInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }
derive instance newtypeDeleteVaultAccessPolicyInput :: Newtype DeleteVaultAccessPolicyInput _
derive instance repGenericDeleteVaultAccessPolicyInput :: Generic DeleteVaultAccessPolicyInput _
instance showDeleteVaultAccessPolicyInput :: Show DeleteVaultAccessPolicyInput where
  show = genericShow
instance decodeDeleteVaultAccessPolicyInput :: Decode DeleteVaultAccessPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVaultAccessPolicyInput :: Encode DeleteVaultAccessPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for deleting a vault from Amazon Glacier.</p>
newtype DeleteVaultInput = DeleteVaultInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }
derive instance newtypeDeleteVaultInput :: Newtype DeleteVaultInput _
derive instance repGenericDeleteVaultInput :: Generic DeleteVaultInput _
instance showDeleteVaultInput :: Show DeleteVaultInput where
  show = genericShow
instance decodeDeleteVaultInput :: Decode DeleteVaultInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVaultInput :: Encode DeleteVaultInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for deleting a vault notification configuration from an Amazon Glacier vault.</p>
newtype DeleteVaultNotificationsInput = DeleteVaultNotificationsInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }
derive instance newtypeDeleteVaultNotificationsInput :: Newtype DeleteVaultNotificationsInput _
derive instance repGenericDeleteVaultNotificationsInput :: Generic DeleteVaultNotificationsInput _
instance showDeleteVaultNotificationsInput :: Show DeleteVaultNotificationsInput where
  show = genericShow
instance decodeDeleteVaultNotificationsInput :: Decode DeleteVaultNotificationsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVaultNotificationsInput :: Encode DeleteVaultNotificationsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for retrieving a job description.</p>
newtype DescribeJobInput = DescribeJobInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "JobId'" :: (String)
  }
derive instance newtypeDescribeJobInput :: Newtype DescribeJobInput _
derive instance repGenericDescribeJobInput :: Generic DescribeJobInput _
instance showDescribeJobInput :: Show DescribeJobInput where
  show = genericShow
instance decodeDescribeJobInput :: Decode DescribeJobInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobInput :: Encode DescribeJobInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for retrieving metadata for a specific vault in Amazon Glacier.</p>
newtype DescribeVaultInput = DescribeVaultInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }
derive instance newtypeDescribeVaultInput :: Newtype DescribeVaultInput _
derive instance repGenericDescribeVaultInput :: Generic DescribeVaultInput _
instance showDescribeVaultInput :: Show DescribeVaultInput where
  show = genericShow
instance decodeDescribeVaultInput :: Decode DescribeVaultInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeVaultInput :: Encode DescribeVaultInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype DescribeVaultOutput = DescribeVaultOutput 
  { "VaultARN" :: NullOrUndefined.NullOrUndefined (String)
  , "VaultName" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "LastInventoryDate" :: NullOrUndefined.NullOrUndefined (String)
  , "NumberOfArchives" :: NullOrUndefined.NullOrUndefined (Number)
  , "SizeInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeDescribeVaultOutput :: Newtype DescribeVaultOutput _
derive instance repGenericDescribeVaultOutput :: Generic DescribeVaultOutput _
instance showDescribeVaultOutput :: Show DescribeVaultOutput where
  show = genericShow
instance decodeDescribeVaultOutput :: Decode DescribeVaultOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeVaultOutput :: Encode DescribeVaultOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the encryption used to store the job results in Amazon S3. </p>
newtype Encryption = Encryption 
  { "EncryptionType" :: NullOrUndefined.NullOrUndefined (EncryptionType)
  , "KMSKeyId" :: NullOrUndefined.NullOrUndefined (String)
  , "KMSContext" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEncryption :: Newtype Encryption _
derive instance repGenericEncryption :: Generic Encryption _
instance showEncryption :: Show Encryption where
  show = genericShow
instance decodeEncryption :: Decode Encryption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryption :: Encode Encryption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EncryptionType = EncryptionType String
derive instance newtypeEncryptionType :: Newtype EncryptionType _
derive instance repGenericEncryptionType :: Generic EncryptionType _
instance showEncryptionType :: Show EncryptionType where
  show = genericShow
instance decodeEncryptionType :: Decode EncryptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionType :: Encode EncryptionType where
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


newtype FileHeaderInfo = FileHeaderInfo String
derive instance newtypeFileHeaderInfo :: Newtype FileHeaderInfo _
derive instance repGenericFileHeaderInfo :: Generic FileHeaderInfo _
instance showFileHeaderInfo :: Show FileHeaderInfo where
  show = genericShow
instance decodeFileHeaderInfo :: Decode FileHeaderInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileHeaderInfo :: Encode FileHeaderInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for GetDataRetrievalPolicy.</p>
newtype GetDataRetrievalPolicyInput = GetDataRetrievalPolicyInput 
  { "AccountId'" :: (String)
  }
derive instance newtypeGetDataRetrievalPolicyInput :: Newtype GetDataRetrievalPolicyInput _
derive instance repGenericGetDataRetrievalPolicyInput :: Generic GetDataRetrievalPolicyInput _
instance showGetDataRetrievalPolicyInput :: Show GetDataRetrievalPolicyInput where
  show = genericShow
instance decodeGetDataRetrievalPolicyInput :: Decode GetDataRetrievalPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDataRetrievalPolicyInput :: Encode GetDataRetrievalPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to the <code>GetDataRetrievalPolicy</code> request.</p>
newtype GetDataRetrievalPolicyOutput = GetDataRetrievalPolicyOutput 
  { "Policy" :: NullOrUndefined.NullOrUndefined (DataRetrievalPolicy)
  }
derive instance newtypeGetDataRetrievalPolicyOutput :: Newtype GetDataRetrievalPolicyOutput _
derive instance repGenericGetDataRetrievalPolicyOutput :: Generic GetDataRetrievalPolicyOutput _
instance showGetDataRetrievalPolicyOutput :: Show GetDataRetrievalPolicyOutput where
  show = genericShow
instance decodeGetDataRetrievalPolicyOutput :: Decode GetDataRetrievalPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDataRetrievalPolicyOutput :: Encode GetDataRetrievalPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for downloading output of an Amazon Glacier job.</p>
newtype GetJobOutputInput = GetJobOutputInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "JobId'" :: (String)
  , "Range'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetJobOutputInput :: Newtype GetJobOutputInput _
derive instance repGenericGetJobOutputInput :: Generic GetJobOutputInput _
instance showGetJobOutputInput :: Show GetJobOutputInput where
  show = genericShow
instance decodeGetJobOutputInput :: Decode GetJobOutputInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobOutputInput :: Encode GetJobOutputInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype GetJobOutputOutput = GetJobOutputOutput 
  { "Body'" :: NullOrUndefined.NullOrUndefined (Stream)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "Status'" :: NullOrUndefined.NullOrUndefined (Httpstatus')
  , "ContentRange'" :: NullOrUndefined.NullOrUndefined (String)
  , "AcceptRanges'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContentType'" :: NullOrUndefined.NullOrUndefined (String)
  , "ArchiveDescription'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetJobOutputOutput :: Newtype GetJobOutputOutput _
derive instance repGenericGetJobOutputOutput :: Generic GetJobOutputOutput _
instance showGetJobOutputOutput :: Show GetJobOutputOutput where
  show = genericShow
instance decodeGetJobOutputOutput :: Decode GetJobOutputOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobOutputOutput :: Encode GetJobOutputOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for GetVaultAccessPolicy.</p>
newtype GetVaultAccessPolicyInput = GetVaultAccessPolicyInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }
derive instance newtypeGetVaultAccessPolicyInput :: Newtype GetVaultAccessPolicyInput _
derive instance repGenericGetVaultAccessPolicyInput :: Generic GetVaultAccessPolicyInput _
instance showGetVaultAccessPolicyInput :: Show GetVaultAccessPolicyInput where
  show = genericShow
instance decodeGetVaultAccessPolicyInput :: Decode GetVaultAccessPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetVaultAccessPolicyInput :: Encode GetVaultAccessPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Output for GetVaultAccessPolicy.</p>
newtype GetVaultAccessPolicyOutput = GetVaultAccessPolicyOutput 
  { "Policy'" :: NullOrUndefined.NullOrUndefined (VaultAccessPolicy)
  }
derive instance newtypeGetVaultAccessPolicyOutput :: Newtype GetVaultAccessPolicyOutput _
derive instance repGenericGetVaultAccessPolicyOutput :: Generic GetVaultAccessPolicyOutput _
instance showGetVaultAccessPolicyOutput :: Show GetVaultAccessPolicyOutput where
  show = genericShow
instance decodeGetVaultAccessPolicyOutput :: Decode GetVaultAccessPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetVaultAccessPolicyOutput :: Encode GetVaultAccessPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input values for <code>GetVaultLock</code>.</p>
newtype GetVaultLockInput = GetVaultLockInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }
derive instance newtypeGetVaultLockInput :: Newtype GetVaultLockInput _
derive instance repGenericGetVaultLockInput :: Generic GetVaultLockInput _
instance showGetVaultLockInput :: Show GetVaultLockInput where
  show = genericShow
instance decodeGetVaultLockInput :: Decode GetVaultLockInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetVaultLockInput :: Encode GetVaultLockInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype GetVaultLockOutput = GetVaultLockOutput 
  { "Policy" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (String)
  , "ExpirationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetVaultLockOutput :: Newtype GetVaultLockOutput _
derive instance repGenericGetVaultLockOutput :: Generic GetVaultLockOutput _
instance showGetVaultLockOutput :: Show GetVaultLockOutput where
  show = genericShow
instance decodeGetVaultLockOutput :: Decode GetVaultLockOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetVaultLockOutput :: Encode GetVaultLockOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for retrieving the notification configuration set on an Amazon Glacier vault.</p>
newtype GetVaultNotificationsInput = GetVaultNotificationsInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }
derive instance newtypeGetVaultNotificationsInput :: Newtype GetVaultNotificationsInput _
derive instance repGenericGetVaultNotificationsInput :: Generic GetVaultNotificationsInput _
instance showGetVaultNotificationsInput :: Show GetVaultNotificationsInput where
  show = genericShow
instance decodeGetVaultNotificationsInput :: Decode GetVaultNotificationsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetVaultNotificationsInput :: Encode GetVaultNotificationsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype GetVaultNotificationsOutput = GetVaultNotificationsOutput 
  { "VaultNotificationConfig'" :: NullOrUndefined.NullOrUndefined (VaultNotificationConfig)
  }
derive instance newtypeGetVaultNotificationsOutput :: Newtype GetVaultNotificationsOutput _
derive instance repGenericGetVaultNotificationsOutput :: Generic GetVaultNotificationsOutput _
instance showGetVaultNotificationsOutput :: Show GetVaultNotificationsOutput where
  show = genericShow
instance decodeGetVaultNotificationsOutput :: Decode GetVaultNotificationsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetVaultNotificationsOutput :: Encode GetVaultNotificationsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the description of an Amazon Glacier job.</p>
newtype GlacierJobDescription = GlacierJobDescription 
  { "JobId" :: NullOrUndefined.NullOrUndefined (String)
  , "JobDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "Action" :: NullOrUndefined.NullOrUndefined (ActionCode)
  , "ArchiveId" :: NullOrUndefined.NullOrUndefined (String)
  , "VaultARN" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Completed" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "StatusCode" :: NullOrUndefined.NullOrUndefined (StatusCode)
  , "StatusMessage" :: NullOrUndefined.NullOrUndefined (String)
  , "ArchiveSizeInBytes" :: NullOrUndefined.NullOrUndefined (Size)
  , "InventorySizeInBytes" :: NullOrUndefined.NullOrUndefined (Size)
  , "SNSTopic" :: NullOrUndefined.NullOrUndefined (String)
  , "CompletionDate" :: NullOrUndefined.NullOrUndefined (String)
  , "SHA256TreeHash" :: NullOrUndefined.NullOrUndefined (String)
  , "ArchiveSHA256TreeHash" :: NullOrUndefined.NullOrUndefined (String)
  , "RetrievalByteRange" :: NullOrUndefined.NullOrUndefined (String)
  , "Tier" :: NullOrUndefined.NullOrUndefined (String)
  , "InventoryRetrievalParameters" :: NullOrUndefined.NullOrUndefined (InventoryRetrievalJobDescription)
  , "JobOutputPath" :: NullOrUndefined.NullOrUndefined (String)
  , "SelectParameters" :: NullOrUndefined.NullOrUndefined (SelectParameters)
  , "OutputLocation" :: NullOrUndefined.NullOrUndefined (OutputLocation)
  }
derive instance newtypeGlacierJobDescription :: Newtype GlacierJobDescription _
derive instance repGenericGlacierJobDescription :: Generic GlacierJobDescription _
instance showGlacierJobDescription :: Show GlacierJobDescription where
  show = genericShow
instance decodeGlacierJobDescription :: Decode GlacierJobDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGlacierJobDescription :: Encode GlacierJobDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about a grant.</p>
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


-- | <p>Contains information about the grantee.</p>
newtype Grantee = Grantee 
  { "Type" :: (Type)
  , "DisplayName" :: NullOrUndefined.NullOrUndefined (String)
  , "URI" :: NullOrUndefined.NullOrUndefined (String)
  , "ID" :: NullOrUndefined.NullOrUndefined (String)
  , "EmailAddress" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGrantee :: Newtype Grantee _
derive instance repGenericGrantee :: Generic Grantee _
instance showGrantee :: Show Grantee where
  show = genericShow
instance decodeGrantee :: Decode Grantee where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantee :: Encode Grantee where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for initiating an Amazon Glacier job.</p>
newtype InitiateJobInput = InitiateJobInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "JobParameters'" :: NullOrUndefined.NullOrUndefined (JobParameters)
  }
derive instance newtypeInitiateJobInput :: Newtype InitiateJobInput _
derive instance repGenericInitiateJobInput :: Generic InitiateJobInput _
instance showInitiateJobInput :: Show InitiateJobInput where
  show = genericShow
instance decodeInitiateJobInput :: Decode InitiateJobInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiateJobInput :: Encode InitiateJobInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype InitiateJobOutput = InitiateJobOutput 
  { "Location'" :: NullOrUndefined.NullOrUndefined (String)
  , "JobId'" :: NullOrUndefined.NullOrUndefined (String)
  , "JobOutputPath'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInitiateJobOutput :: Newtype InitiateJobOutput _
derive instance repGenericInitiateJobOutput :: Generic InitiateJobOutput _
instance showInitiateJobOutput :: Show InitiateJobOutput where
  show = genericShow
instance decodeInitiateJobOutput :: Decode InitiateJobOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiateJobOutput :: Encode InitiateJobOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for initiating a multipart upload to an Amazon Glacier vault.</p>
newtype InitiateMultipartUploadInput = InitiateMultipartUploadInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "ArchiveDescription'" :: NullOrUndefined.NullOrUndefined (String)
  , "PartSize'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInitiateMultipartUploadInput :: Newtype InitiateMultipartUploadInput _
derive instance repGenericInitiateMultipartUploadInput :: Generic InitiateMultipartUploadInput _
instance showInitiateMultipartUploadInput :: Show InitiateMultipartUploadInput where
  show = genericShow
instance decodeInitiateMultipartUploadInput :: Decode InitiateMultipartUploadInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiateMultipartUploadInput :: Encode InitiateMultipartUploadInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon Glacier response to your request.</p>
newtype InitiateMultipartUploadOutput = InitiateMultipartUploadOutput 
  { "Location'" :: NullOrUndefined.NullOrUndefined (String)
  , "UploadId'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInitiateMultipartUploadOutput :: Newtype InitiateMultipartUploadOutput _
derive instance repGenericInitiateMultipartUploadOutput :: Generic InitiateMultipartUploadOutput _
instance showInitiateMultipartUploadOutput :: Show InitiateMultipartUploadOutput where
  show = genericShow
instance decodeInitiateMultipartUploadOutput :: Decode InitiateMultipartUploadOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiateMultipartUploadOutput :: Encode InitiateMultipartUploadOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input values for <code>InitiateVaultLock</code>.</p>
newtype InitiateVaultLockInput = InitiateVaultLockInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "Policy'" :: NullOrUndefined.NullOrUndefined (VaultLockPolicy)
  }
derive instance newtypeInitiateVaultLockInput :: Newtype InitiateVaultLockInput _
derive instance repGenericInitiateVaultLockInput :: Generic InitiateVaultLockInput _
instance showInitiateVaultLockInput :: Show InitiateVaultLockInput where
  show = genericShow
instance decodeInitiateVaultLockInput :: Decode InitiateVaultLockInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiateVaultLockInput :: Encode InitiateVaultLockInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype InitiateVaultLockOutput = InitiateVaultLockOutput 
  { "LockId'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInitiateVaultLockOutput :: Newtype InitiateVaultLockOutput _
derive instance repGenericInitiateVaultLockOutput :: Generic InitiateVaultLockOutput _
instance showInitiateVaultLockOutput :: Show InitiateVaultLockOutput where
  show = genericShow
instance decodeInitiateVaultLockOutput :: Decode InitiateVaultLockOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiateVaultLockOutput :: Encode InitiateVaultLockOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes how the archive is serialized.</p>
newtype InputSerialization = InputSerialization 
  { "Csv'" :: NullOrUndefined.NullOrUndefined (CSVInput)
  }
derive instance newtypeInputSerialization :: Newtype InputSerialization _
derive instance repGenericInputSerialization :: Generic InputSerialization _
instance showInputSerialization :: Show InputSerialization where
  show = genericShow
instance decodeInputSerialization :: Decode InputSerialization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputSerialization :: Encode InputSerialization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if there is insufficient capacity to process this expedited request. This error only applies to expedited retrievals and not to standard or bulk retrievals.</p>
newtype InsufficientCapacityException = InsufficientCapacityException 
  { "Type'" :: NullOrUndefined.NullOrUndefined (String)
  , "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInsufficientCapacityException :: Newtype InsufficientCapacityException _
derive instance repGenericInsufficientCapacityException :: Generic InsufficientCapacityException _
instance showInsufficientCapacityException :: Show InsufficientCapacityException where
  show = genericShow
instance decodeInsufficientCapacityException :: Decode InsufficientCapacityException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientCapacityException :: Encode InsufficientCapacityException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if a parameter of the request is incorrectly specified.</p>
newtype InvalidParameterValueException = InvalidParameterValueException 
  { "Type'" :: NullOrUndefined.NullOrUndefined (String)
  , "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidParameterValueException :: Newtype InvalidParameterValueException _
derive instance repGenericInvalidParameterValueException :: Generic InvalidParameterValueException _
instance showInvalidParameterValueException :: Show InvalidParameterValueException where
  show = genericShow
instance decodeInvalidParameterValueException :: Decode InvalidParameterValueException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterValueException :: Encode InvalidParameterValueException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the options for a range inventory retrieval job.</p>
newtype InventoryRetrievalJobDescription = InventoryRetrievalJobDescription 
  { "Format" :: NullOrUndefined.NullOrUndefined (String)
  , "StartDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "EndDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Limit" :: NullOrUndefined.NullOrUndefined (String)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInventoryRetrievalJobDescription :: Newtype InventoryRetrievalJobDescription _
derive instance repGenericInventoryRetrievalJobDescription :: Generic InventoryRetrievalJobDescription _
instance showInventoryRetrievalJobDescription :: Show InventoryRetrievalJobDescription where
  show = genericShow
instance decodeInventoryRetrievalJobDescription :: Decode InventoryRetrievalJobDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryRetrievalJobDescription :: Encode InventoryRetrievalJobDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for specifying a range inventory retrieval job.</p>
newtype InventoryRetrievalJobInput = InventoryRetrievalJobInput 
  { "StartDate" :: NullOrUndefined.NullOrUndefined (String)
  , "EndDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit" :: NullOrUndefined.NullOrUndefined (String)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInventoryRetrievalJobInput :: Newtype InventoryRetrievalJobInput _
derive instance repGenericInventoryRetrievalJobInput :: Generic InventoryRetrievalJobInput _
instance showInventoryRetrievalJobInput :: Show InventoryRetrievalJobInput where
  show = genericShow
instance decodeInventoryRetrievalJobInput :: Decode InventoryRetrievalJobInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryRetrievalJobInput :: Encode InventoryRetrievalJobInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobList = JobList (Array GlacierJobDescription)
derive instance newtypeJobList :: Newtype JobList _
derive instance repGenericJobList :: Generic JobList _
instance showJobList :: Show JobList where
  show = genericShow
instance decodeJobList :: Decode JobList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobList :: Encode JobList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for defining a job.</p>
newtype JobParameters = JobParameters 
  { "Format" :: NullOrUndefined.NullOrUndefined (String)
  , "Type" :: NullOrUndefined.NullOrUndefined (String)
  , "ArchiveId" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "SNSTopic" :: NullOrUndefined.NullOrUndefined (String)
  , "RetrievalByteRange" :: NullOrUndefined.NullOrUndefined (String)
  , "Tier" :: NullOrUndefined.NullOrUndefined (String)
  , "InventoryRetrievalParameters" :: NullOrUndefined.NullOrUndefined (InventoryRetrievalJobInput)
  , "SelectParameters" :: NullOrUndefined.NullOrUndefined (SelectParameters)
  , "OutputLocation" :: NullOrUndefined.NullOrUndefined (OutputLocation)
  }
derive instance newtypeJobParameters :: Newtype JobParameters _
derive instance repGenericJobParameters :: Generic JobParameters _
instance showJobParameters :: Show JobParameters where
  show = genericShow
instance decodeJobParameters :: Decode JobParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobParameters :: Encode JobParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if the request results in a vault or account limit being exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Type'" :: NullOrUndefined.NullOrUndefined (String)
  , "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for retrieving a job list for an Amazon Glacier vault.</p>
newtype ListJobsInput = ListJobsInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (String)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (String)
  , "Statuscode'" :: NullOrUndefined.NullOrUndefined (String)
  , "Completed'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListJobsInput :: Newtype ListJobsInput _
derive instance repGenericListJobsInput :: Generic ListJobsInput _
instance showListJobsInput :: Show ListJobsInput where
  show = genericShow
instance decodeListJobsInput :: Decode ListJobsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsInput :: Encode ListJobsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype ListJobsOutput = ListJobsOutput 
  { "JobList" :: NullOrUndefined.NullOrUndefined (JobList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListJobsOutput :: Newtype ListJobsOutput _
derive instance repGenericListJobsOutput :: Generic ListJobsOutput _
instance showListJobsOutput :: Show ListJobsOutput where
  show = genericShow
instance decodeListJobsOutput :: Decode ListJobsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsOutput :: Encode ListJobsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for retrieving list of in-progress multipart uploads for an Amazon Glacier vault.</p>
newtype ListMultipartUploadsInput = ListMultipartUploadsInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListMultipartUploadsInput :: Newtype ListMultipartUploadsInput _
derive instance repGenericListMultipartUploadsInput :: Generic ListMultipartUploadsInput _
instance showListMultipartUploadsInput :: Show ListMultipartUploadsInput where
  show = genericShow
instance decodeListMultipartUploadsInput :: Decode ListMultipartUploadsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListMultipartUploadsInput :: Encode ListMultipartUploadsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype ListMultipartUploadsOutput = ListMultipartUploadsOutput 
  { "UploadsList" :: NullOrUndefined.NullOrUndefined (UploadsList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListMultipartUploadsOutput :: Newtype ListMultipartUploadsOutput _
derive instance repGenericListMultipartUploadsOutput :: Generic ListMultipartUploadsOutput _
instance showListMultipartUploadsOutput :: Show ListMultipartUploadsOutput where
  show = genericShow
instance decodeListMultipartUploadsOutput :: Decode ListMultipartUploadsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListMultipartUploadsOutput :: Encode ListMultipartUploadsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for retrieving a list of parts of an archive that have been uploaded in a specific multipart upload.</p>
newtype ListPartsInput = ListPartsInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "UploadId'" :: (String)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListPartsInput :: Newtype ListPartsInput _
derive instance repGenericListPartsInput :: Generic ListPartsInput _
instance showListPartsInput :: Show ListPartsInput where
  show = genericShow
instance decodeListPartsInput :: Decode ListPartsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPartsInput :: Encode ListPartsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype ListPartsOutput = ListPartsOutput 
  { "MultipartUploadId" :: NullOrUndefined.NullOrUndefined (String)
  , "VaultARN" :: NullOrUndefined.NullOrUndefined (String)
  , "ArchiveDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "PartSizeInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Parts" :: NullOrUndefined.NullOrUndefined (PartList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListPartsOutput :: Newtype ListPartsOutput _
derive instance repGenericListPartsOutput :: Generic ListPartsOutput _
instance showListPartsOutput :: Show ListPartsOutput where
  show = genericShow
instance decodeListPartsOutput :: Decode ListPartsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPartsOutput :: Encode ListPartsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListProvisionedCapacityInput = ListProvisionedCapacityInput 
  { "AccountId'" :: (String)
  }
derive instance newtypeListProvisionedCapacityInput :: Newtype ListProvisionedCapacityInput _
derive instance repGenericListProvisionedCapacityInput :: Generic ListProvisionedCapacityInput _
instance showListProvisionedCapacityInput :: Show ListProvisionedCapacityInput where
  show = genericShow
instance decodeListProvisionedCapacityInput :: Decode ListProvisionedCapacityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListProvisionedCapacityInput :: Encode ListProvisionedCapacityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListProvisionedCapacityOutput = ListProvisionedCapacityOutput 
  { "ProvisionedCapacityList" :: NullOrUndefined.NullOrUndefined (ProvisionedCapacityList)
  }
derive instance newtypeListProvisionedCapacityOutput :: Newtype ListProvisionedCapacityOutput _
derive instance repGenericListProvisionedCapacityOutput :: Generic ListProvisionedCapacityOutput _
instance showListProvisionedCapacityOutput :: Show ListProvisionedCapacityOutput where
  show = genericShow
instance decodeListProvisionedCapacityOutput :: Decode ListProvisionedCapacityOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListProvisionedCapacityOutput :: Encode ListProvisionedCapacityOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input value for <code>ListTagsForVaultInput</code>.</p>
newtype ListTagsForVaultInput = ListTagsForVaultInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }
derive instance newtypeListTagsForVaultInput :: Newtype ListTagsForVaultInput _
derive instance repGenericListTagsForVaultInput :: Generic ListTagsForVaultInput _
instance showListTagsForVaultInput :: Show ListTagsForVaultInput where
  show = genericShow
instance decodeListTagsForVaultInput :: Decode ListTagsForVaultInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForVaultInput :: Encode ListTagsForVaultInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype ListTagsForVaultOutput = ListTagsForVaultOutput 
  { "Tags" :: NullOrUndefined.NullOrUndefined (TagMap)
  }
derive instance newtypeListTagsForVaultOutput :: Newtype ListTagsForVaultOutput _
derive instance repGenericListTagsForVaultOutput :: Generic ListTagsForVaultOutput _
instance showListTagsForVaultOutput :: Show ListTagsForVaultOutput where
  show = genericShow
instance decodeListTagsForVaultOutput :: Decode ListTagsForVaultOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForVaultOutput :: Encode ListTagsForVaultOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options to retrieve the vault list owned by the calling user's account. The list provides metadata information for each vault.</p>
newtype ListVaultsInput = ListVaultsInput 
  { "AccountId'" :: (String)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListVaultsInput :: Newtype ListVaultsInput _
derive instance repGenericListVaultsInput :: Generic ListVaultsInput _
instance showListVaultsInput :: Show ListVaultsInput where
  show = genericShow
instance decodeListVaultsInput :: Decode ListVaultsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListVaultsInput :: Encode ListVaultsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype ListVaultsOutput = ListVaultsOutput 
  { "VaultList" :: NullOrUndefined.NullOrUndefined (VaultList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListVaultsOutput :: Newtype ListVaultsOutput _
derive instance repGenericListVaultsOutput :: Generic ListVaultsOutput _
instance showListVaultsOutput :: Show ListVaultsOutput where
  show = genericShow
instance decodeListVaultsOutput :: Decode ListVaultsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListVaultsOutput :: Encode ListVaultsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if a required header or parameter is missing from the request.</p>
newtype MissingParameterValueException = MissingParameterValueException 
  { "Type'" :: NullOrUndefined.NullOrUndefined (String)
  , "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMissingParameterValueException :: Newtype MissingParameterValueException _
derive instance repGenericMissingParameterValueException :: Generic MissingParameterValueException _
instance showMissingParameterValueException :: Show MissingParameterValueException where
  show = genericShow
instance decodeMissingParameterValueException :: Decode MissingParameterValueException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingParameterValueException :: Encode MissingParameterValueException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NotificationEventList = NotificationEventList (Array String)
derive instance newtypeNotificationEventList :: Newtype NotificationEventList _
derive instance repGenericNotificationEventList :: Generic NotificationEventList _
instance showNotificationEventList :: Show NotificationEventList where
  show = genericShow
instance decodeNotificationEventList :: Decode NotificationEventList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationEventList :: Encode NotificationEventList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableLong = NullableLong Number
derive instance newtypeNullableLong :: Newtype NullableLong _
derive instance repGenericNullableLong :: Generic NullableLong _
instance showNullableLong :: Show NullableLong where
  show = genericShow
instance decodeNullableLong :: Decode NullableLong where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableLong :: Encode NullableLong where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the location where the select job results are stored.</p>
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


-- | <p>Describes how the select output is serialized.</p>
newtype OutputSerialization = OutputSerialization 
  { "Csv'" :: NullOrUndefined.NullOrUndefined (CSVOutput)
  }
derive instance newtypeOutputSerialization :: Newtype OutputSerialization _
derive instance repGenericOutputSerialization :: Generic OutputSerialization _
instance showOutputSerialization :: Show OutputSerialization where
  show = genericShow
instance decodeOutputSerialization :: Decode OutputSerialization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputSerialization :: Encode OutputSerialization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartList = PartList (Array PartListElement)
derive instance newtypePartList :: Newtype PartList _
derive instance repGenericPartList :: Generic PartList _
instance showPartList :: Show PartList where
  show = genericShow
instance decodePartList :: Decode PartList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartList :: Encode PartList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of the part sizes of the multipart upload.</p>
newtype PartListElement = PartListElement 
  { "RangeInBytes" :: NullOrUndefined.NullOrUndefined (String)
  , "SHA256TreeHash" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePartListElement :: Newtype PartListElement _
derive instance repGenericPartListElement :: Generic PartListElement _
instance showPartListElement :: Show PartListElement where
  show = genericShow
instance decodePartListElement :: Decode PartListElement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartListElement :: Encode PartListElement where
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


-- | <p>Returned if a retrieval job would exceed the current data policy's retrieval rate limit. For more information about data retrieval policies,</p>
newtype PolicyEnforcedException = PolicyEnforcedException 
  { "Type'" :: NullOrUndefined.NullOrUndefined (String)
  , "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePolicyEnforcedException :: Newtype PolicyEnforcedException _
derive instance repGenericPolicyEnforcedException :: Generic PolicyEnforcedException _
instance showPolicyEnforcedException :: Show PolicyEnforcedException where
  show = genericShow
instance decodePolicyEnforcedException :: Decode PolicyEnforcedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyEnforcedException :: Encode PolicyEnforcedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The definition for a provisioned capacity unit.</p>
newtype ProvisionedCapacityDescription = ProvisionedCapacityDescription 
  { "CapacityId" :: NullOrUndefined.NullOrUndefined (String)
  , "StartDate" :: NullOrUndefined.NullOrUndefined (String)
  , "ExpirationDate" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeProvisionedCapacityDescription :: Newtype ProvisionedCapacityDescription _
derive instance repGenericProvisionedCapacityDescription :: Generic ProvisionedCapacityDescription _
instance showProvisionedCapacityDescription :: Show ProvisionedCapacityDescription where
  show = genericShow
instance decodeProvisionedCapacityDescription :: Decode ProvisionedCapacityDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProvisionedCapacityDescription :: Encode ProvisionedCapacityDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProvisionedCapacityList = ProvisionedCapacityList (Array ProvisionedCapacityDescription)
derive instance newtypeProvisionedCapacityList :: Newtype ProvisionedCapacityList _
derive instance repGenericProvisionedCapacityList :: Generic ProvisionedCapacityList _
instance showProvisionedCapacityList :: Show ProvisionedCapacityList where
  show = genericShow
instance decodeProvisionedCapacityList :: Decode ProvisionedCapacityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProvisionedCapacityList :: Encode ProvisionedCapacityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PurchaseProvisionedCapacityInput = PurchaseProvisionedCapacityInput 
  { "AccountId'" :: (String)
  }
derive instance newtypePurchaseProvisionedCapacityInput :: Newtype PurchaseProvisionedCapacityInput _
derive instance repGenericPurchaseProvisionedCapacityInput :: Generic PurchaseProvisionedCapacityInput _
instance showPurchaseProvisionedCapacityInput :: Show PurchaseProvisionedCapacityInput where
  show = genericShow
instance decodePurchaseProvisionedCapacityInput :: Decode PurchaseProvisionedCapacityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurchaseProvisionedCapacityInput :: Encode PurchaseProvisionedCapacityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PurchaseProvisionedCapacityOutput = PurchaseProvisionedCapacityOutput 
  { "CapacityId'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePurchaseProvisionedCapacityOutput :: Newtype PurchaseProvisionedCapacityOutput _
derive instance repGenericPurchaseProvisionedCapacityOutput :: Generic PurchaseProvisionedCapacityOutput _
instance showPurchaseProvisionedCapacityOutput :: Show PurchaseProvisionedCapacityOutput where
  show = genericShow
instance decodePurchaseProvisionedCapacityOutput :: Decode PurchaseProvisionedCapacityOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurchaseProvisionedCapacityOutput :: Encode PurchaseProvisionedCapacityOutput where
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


-- | <p>The input value for <code>RemoveTagsFromVaultInput</code>.</p>
newtype RemoveTagsFromVaultInput = RemoveTagsFromVaultInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "TagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  }
derive instance newtypeRemoveTagsFromVaultInput :: Newtype RemoveTagsFromVaultInput _
derive instance repGenericRemoveTagsFromVaultInput :: Generic RemoveTagsFromVaultInput _
instance showRemoveTagsFromVaultInput :: Show RemoveTagsFromVaultInput where
  show = genericShow
instance decodeRemoveTagsFromVaultInput :: Decode RemoveTagsFromVaultInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsFromVaultInput :: Encode RemoveTagsFromVaultInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if, when uploading an archive, Amazon Glacier times out while receiving the upload.</p>
newtype RequestTimeoutException = RequestTimeoutException 
  { "Type'" :: NullOrUndefined.NullOrUndefined (String)
  , "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRequestTimeoutException :: Newtype RequestTimeoutException _
derive instance repGenericRequestTimeoutException :: Generic RequestTimeoutException _
instance showRequestTimeoutException :: Show RequestTimeoutException where
  show = genericShow
instance decodeRequestTimeoutException :: Decode RequestTimeoutException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestTimeoutException :: Encode RequestTimeoutException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if the specified resource (such as a vault, upload ID, or job ID) doesn't exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Type'" :: NullOrUndefined.NullOrUndefined (String)
  , "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the location in Amazon S3 where the select job results are stored.</p>
newtype S3Location = S3Location 
  { "BucketName" :: NullOrUndefined.NullOrUndefined (String)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (String)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (Encryption)
  , "CannedACL" :: NullOrUndefined.NullOrUndefined (CannedACL)
  , "AccessControlList" :: NullOrUndefined.NullOrUndefined (AccessControlPolicyList)
  , "Tagging" :: NullOrUndefined.NullOrUndefined (Hashmap')
  , "UserMetadata" :: NullOrUndefined.NullOrUndefined (Hashmap')
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


-- | <p>Contains information about the parameters used for a select.</p>
newtype SelectParameters = SelectParameters 
  { "InputSerialization" :: NullOrUndefined.NullOrUndefined (InputSerialization)
  , "ExpressionType" :: NullOrUndefined.NullOrUndefined (ExpressionType)
  , "Expression" :: NullOrUndefined.NullOrUndefined (String)
  , "OutputSerialization" :: NullOrUndefined.NullOrUndefined (OutputSerialization)
  }
derive instance newtypeSelectParameters :: Newtype SelectParameters _
derive instance repGenericSelectParameters :: Generic SelectParameters _
instance showSelectParameters :: Show SelectParameters where
  show = genericShow
instance decodeSelectParameters :: Decode SelectParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSelectParameters :: Encode SelectParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if the service cannot complete the request.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Type'" :: NullOrUndefined.NullOrUndefined (String)
  , "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _
derive instance repGenericServiceUnavailableException :: Generic ServiceUnavailableException _
instance showServiceUnavailableException :: Show ServiceUnavailableException where
  show = genericShow
instance decodeServiceUnavailableException :: Decode ServiceUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUnavailableException :: Encode ServiceUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>SetDataRetrievalPolicy input.</p>
newtype SetDataRetrievalPolicyInput = SetDataRetrievalPolicyInput 
  { "AccountId'" :: (String)
  , "Policy" :: NullOrUndefined.NullOrUndefined (DataRetrievalPolicy)
  }
derive instance newtypeSetDataRetrievalPolicyInput :: Newtype SetDataRetrievalPolicyInput _
derive instance repGenericSetDataRetrievalPolicyInput :: Generic SetDataRetrievalPolicyInput _
instance showSetDataRetrievalPolicyInput :: Show SetDataRetrievalPolicyInput where
  show = genericShow
instance decodeSetDataRetrievalPolicyInput :: Decode SetDataRetrievalPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetDataRetrievalPolicyInput :: Encode SetDataRetrievalPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>SetVaultAccessPolicy input.</p>
newtype SetVaultAccessPolicyInput = SetVaultAccessPolicyInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "Policy'" :: NullOrUndefined.NullOrUndefined (VaultAccessPolicy)
  }
derive instance newtypeSetVaultAccessPolicyInput :: Newtype SetVaultAccessPolicyInput _
derive instance repGenericSetVaultAccessPolicyInput :: Generic SetVaultAccessPolicyInput _
instance showSetVaultAccessPolicyInput :: Show SetVaultAccessPolicyInput where
  show = genericShow
instance decodeSetVaultAccessPolicyInput :: Decode SetVaultAccessPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetVaultAccessPolicyInput :: Encode SetVaultAccessPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options to configure notifications that will be sent when specific events happen to a vault.</p>
newtype SetVaultNotificationsInput = SetVaultNotificationsInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "VaultNotificationConfig'" :: NullOrUndefined.NullOrUndefined (VaultNotificationConfig)
  }
derive instance newtypeSetVaultNotificationsInput :: Newtype SetVaultNotificationsInput _
derive instance repGenericSetVaultNotificationsInput :: Generic SetVaultNotificationsInput _
instance showSetVaultNotificationsInput :: Show SetVaultNotificationsInput where
  show = genericShow
instance decodeSetVaultNotificationsInput :: Decode SetVaultNotificationsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetVaultNotificationsInput :: Encode SetVaultNotificationsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Size = Size Number
derive instance newtypeSize :: Newtype Size _
derive instance repGenericSize :: Generic Size _
instance showSize :: Show Size where
  show = genericShow
instance decodeSize :: Decode Size where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSize :: Encode Size where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StatusCode = StatusCode String
derive instance newtypeStatusCode :: Newtype StatusCode _
derive instance repGenericStatusCode :: Generic StatusCode _
instance showStatusCode :: Show StatusCode where
  show = genericShow
instance decodeStatusCode :: Decode StatusCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusCode :: Encode StatusCode where
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


newtype Stream = Stream String
derive instance newtypeStream :: Newtype Stream _
derive instance repGenericStream :: Generic Stream _
instance showStream :: Show Stream where
  show = genericShow
instance decodeStream :: Decode Stream where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStream :: Encode Stream where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeyList = TagKeyList (Array String)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagMap = TagMap (StrMap.StrMap TagValue)
derive instance newtypeTagMap :: Newtype TagMap _
derive instance repGenericTagMap :: Generic TagMap _
instance showTagMap :: Show TagMap where
  show = genericShow
instance decodeTagMap :: Decode TagMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagMap :: Encode TagMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
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


-- | <p>Provides options to add an archive to a vault.</p>
newtype UploadArchiveInput = UploadArchiveInput 
  { "VaultName'" :: (String)
  , "AccountId'" :: (String)
  , "ArchiveDescription'" :: NullOrUndefined.NullOrUndefined (String)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "Body'" :: NullOrUndefined.NullOrUndefined (Stream)
  }
derive instance newtypeUploadArchiveInput :: Newtype UploadArchiveInput _
derive instance repGenericUploadArchiveInput :: Generic UploadArchiveInput _
instance showUploadArchiveInput :: Show UploadArchiveInput where
  show = genericShow
instance decodeUploadArchiveInput :: Decode UploadArchiveInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadArchiveInput :: Encode UploadArchiveInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of in-progress multipart uploads for a vault.</p>
newtype UploadListElement = UploadListElement 
  { "MultipartUploadId" :: NullOrUndefined.NullOrUndefined (String)
  , "VaultARN" :: NullOrUndefined.NullOrUndefined (String)
  , "ArchiveDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "PartSizeInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUploadListElement :: Newtype UploadListElement _
derive instance repGenericUploadListElement :: Generic UploadListElement _
instance showUploadListElement :: Show UploadListElement where
  show = genericShow
instance decodeUploadListElement :: Decode UploadListElement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadListElement :: Encode UploadListElement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options to upload a part of an archive in a multipart upload operation.</p>
newtype UploadMultipartPartInput = UploadMultipartPartInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "UploadId'" :: (String)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "Range'" :: NullOrUndefined.NullOrUndefined (String)
  , "Body'" :: NullOrUndefined.NullOrUndefined (Stream)
  }
derive instance newtypeUploadMultipartPartInput :: Newtype UploadMultipartPartInput _
derive instance repGenericUploadMultipartPartInput :: Generic UploadMultipartPartInput _
instance showUploadMultipartPartInput :: Show UploadMultipartPartInput where
  show = genericShow
instance decodeUploadMultipartPartInput :: Decode UploadMultipartPartInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadMultipartPartInput :: Encode UploadMultipartPartInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype UploadMultipartPartOutput = UploadMultipartPartOutput 
  { "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUploadMultipartPartOutput :: Newtype UploadMultipartPartOutput _
derive instance repGenericUploadMultipartPartOutput :: Generic UploadMultipartPartOutput _
instance showUploadMultipartPartOutput :: Show UploadMultipartPartOutput where
  show = genericShow
instance decodeUploadMultipartPartOutput :: Decode UploadMultipartPartOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadMultipartPartOutput :: Encode UploadMultipartPartOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UploadsList = UploadsList (Array UploadListElement)
derive instance newtypeUploadsList :: Newtype UploadsList _
derive instance repGenericUploadsList :: Generic UploadsList _
instance showUploadsList :: Show UploadsList where
  show = genericShow
instance decodeUploadsList :: Decode UploadsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadsList :: Encode UploadsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the vault access policy.</p>
newtype VaultAccessPolicy = VaultAccessPolicy 
  { "Policy" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeVaultAccessPolicy :: Newtype VaultAccessPolicy _
derive instance repGenericVaultAccessPolicy :: Generic VaultAccessPolicy _
instance showVaultAccessPolicy :: Show VaultAccessPolicy where
  show = genericShow
instance decodeVaultAccessPolicy :: Decode VaultAccessPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVaultAccessPolicy :: Encode VaultAccessPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VaultList = VaultList (Array DescribeVaultOutput)
derive instance newtypeVaultList :: Newtype VaultList _
derive instance repGenericVaultList :: Generic VaultList _
instance showVaultList :: Show VaultList where
  show = genericShow
instance decodeVaultList :: Decode VaultList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVaultList :: Encode VaultList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the vault lock policy.</p>
newtype VaultLockPolicy = VaultLockPolicy 
  { "Policy" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeVaultLockPolicy :: Newtype VaultLockPolicy _
derive instance repGenericVaultLockPolicy :: Generic VaultLockPolicy _
instance showVaultLockPolicy :: Show VaultLockPolicy where
  show = genericShow
instance decodeVaultLockPolicy :: Decode VaultLockPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVaultLockPolicy :: Encode VaultLockPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a vault's notification configuration.</p>
newtype VaultNotificationConfig = VaultNotificationConfig 
  { "SNSTopic" :: NullOrUndefined.NullOrUndefined (String)
  , "Events" :: NullOrUndefined.NullOrUndefined (NotificationEventList)
  }
derive instance newtypeVaultNotificationConfig :: Newtype VaultNotificationConfig _
derive instance repGenericVaultNotificationConfig :: Generic VaultNotificationConfig _
instance showVaultNotificationConfig :: Show VaultNotificationConfig where
  show = genericShow
instance decodeVaultNotificationConfig :: Decode VaultNotificationConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVaultNotificationConfig :: Encode VaultNotificationConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Hashmap' = Hashmap' (StrMap.StrMap String)
derive instance newtypeHashmap' :: Newtype Hashmap' _
derive instance repGenericHashmap' :: Generic Hashmap' _
instance showHashmap' :: Show Hashmap' where
  show = genericShow
instance decodeHashmap' :: Decode Hashmap' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHashmap' :: Encode Hashmap' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Httpstatus' = Httpstatus' Int
derive instance newtypeHttpstatus' :: Newtype Httpstatus' _
derive instance repGenericHttpstatus' :: Generic Httpstatus' _
instance showHttpstatus' :: Show Httpstatus' where
  show = genericShow
instance decodeHttpstatus' :: Decode Httpstatus' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHttpstatus' :: Encode Httpstatus' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
