

-- | <p>Amazon Glacier is a storage solution for "cold data."</p> <p>Amazon Glacier is an extremely low-cost storage service that provides secure, durable, and easy-to-use storage for data backup and archival. With Amazon Glacier, customers can store their data cost effectively for months, years, or decades. Amazon Glacier also enables customers to offload the administrative burdens of operating and scaling storage to AWS, so they don't have to worry about capacity planning, hardware provisioning, data replication, hardware failure and recovery, or time-consuming hardware migrations.</p> <p>Amazon Glacier is a great storage choice when low storage cost is paramount, your data is rarely retrieved, and retrieval latency of several hours is acceptable. If your application requires fast or frequent access to your data, consider using Amazon S3. For more information, see <a href="http://aws.amazon.com/s3/">Amazon Simple Storage Service (Amazon S3)</a>.</p> <p>You can store any kind of data in any format. There is no maximum limit on the total amount of data you can store in Amazon Glacier.</p> <p>If you are a first-time user of Amazon Glacier, we recommend that you begin by reading the following sections in the <i>Amazon Glacier Developer Guide</i>:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/introduction.html">What is Amazon Glacier</a> - This section of the Developer Guide describes the underlying data model, the operations it supports, and the AWS SDKs that you can use to interact with the service.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/amazon-glacier-getting-started.html">Getting Started with Amazon Glacier</a> - The Getting Started section walks you through the process of creating a vault, uploading archives, creating jobs to download archives, retrieving the job output, and deleting archives.</p> </li> </ul>
module AWS.Glacier where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Glacier" :: String


-- | <p>This operation aborts a multipart upload identified by the upload ID.</p> <p>After the Abort Multipart Upload request succeeds, you cannot upload any more parts to the multipart upload or complete the multipart upload. Aborting a completed upload fails. However, aborting an already-aborted upload will succeed, for a short time. For more information about uploading a part and completing a multipart upload, see <a>UploadMultipartPart</a> and <a>CompleteMultipartUpload</a>.</p> <p>This operation is idempotent.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html">Working with Archives in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html">Abort Multipart Upload</a> in the <i>Amazon Glacier Developer Guide</i>. </p>
abortMultipartUpload :: forall eff. AbortMultipartUploadInput -> Aff (err :: AWS.RequestError | eff) Unit
abortMultipartUpload = AWS.request serviceName "AbortMultipartUpload" 


-- | <p>This operation aborts the vault locking process if the vault lock is not in the <code>Locked</code> state. If the vault lock is in the <code>Locked</code> state when this operation is requested, the operation returns an <code>AccessDeniedException</code> error. Aborting the vault locking process removes the vault lock policy from the specified vault. </p> <p>A vault lock is put into the <code>InProgress</code> state by calling <a>InitiateVaultLock</a>. A vault lock is put into the <code>Locked</code> state by calling <a>CompleteVaultLock</a>. You can get the state of a vault lock by calling <a>GetVaultLock</a>. For more information about the vault locking process, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html">Amazon Glacier Vault Lock</a>. For more information about vault lock policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html">Amazon Glacier Access Control with Vault Lock Policies</a>. </p> <p>This operation is idempotent. You can successfully invoke this operation multiple times, if the vault lock is in the <code>InProgress</code> state or if there is no policy associated with the vault.</p>
abortVaultLock :: forall eff. AbortVaultLockInput -> Aff (err :: AWS.RequestError | eff) Unit
abortVaultLock = AWS.request serviceName "AbortVaultLock" 


-- | <p>This operation adds the specified tags to a vault. Each tag is composed of a key and a value. Each vault can have up to 10 tags. If your request would cause the tag limit for the vault to be exceeded, the operation throws the <code>LimitExceededException</code> error. If a tag already exists on the vault under a specified key, the existing key value will be overwritten. For more information about tags, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html">Tagging Amazon Glacier Resources</a>. </p>
addTagsToVault :: forall eff. AddTagsToVaultInput -> Aff (err :: AWS.RequestError | eff) Unit
addTagsToVault = AWS.request serviceName "AddTagsToVault" 


-- | <p>You call this operation to inform Amazon Glacier that all the archive parts have been uploaded and that Amazon Glacier can now assemble the archive from the uploaded parts. After assembling and saving the archive to the vault, Amazon Glacier returns the URI path of the newly created archive resource. Using the URI path, you can then access the archive. After you upload an archive, you should save the archive ID returned to retrieve the archive at a later point. You can also get the vault inventory to obtain a list of archive IDs in a vault. For more information, see <a>InitiateJob</a>.</p> <p>In the request, you must include the computed SHA256 tree hash of the entire archive you have uploaded. For information about computing a SHA256 tree hash, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html">Computing Checksums</a>. On the server side, Amazon Glacier also constructs the SHA256 tree hash of the assembled archive. If the values match, Amazon Glacier saves the archive to the vault; otherwise, it returns an error, and the operation fails. The <a>ListParts</a> operation returns a list of parts uploaded for a specific multipart upload. It includes checksum information for each uploaded part that can be used to debug a bad checksum issue.</p> <p>Additionally, Amazon Glacier also checks for any missing content ranges when assembling the archive, if missing content ranges are found, Amazon Glacier returns an error and the operation fails.</p> <p>Complete Multipart Upload is an idempotent operation. After your first successful complete multipart upload, if you call the operation again within a short period, the operation will succeed and return the same archive ID. This is useful in the event you experience a network issue that causes an aborted connection or receive a 500 server error, in which case you can repeat your Complete Multipart Upload request and get the same archive ID without creating duplicate archives. Note, however, that after the multipart upload completes, you cannot call the List Parts operation and the multipart upload will not appear in List Multipart Uploads response, even if idempotent complete is possible.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html">Uploading Large Archives in Parts (Multipart Upload)</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-complete-upload.html">Complete Multipart Upload</a> in the <i>Amazon Glacier Developer Guide</i>. </p>
completeMultipartUpload :: forall eff. CompleteMultipartUploadInput -> Aff (err :: AWS.RequestError | eff) ArchiveCreationOutput
completeMultipartUpload = AWS.request serviceName "CompleteMultipartUpload" 


-- | <p>This operation completes the vault locking process by transitioning the vault lock from the <code>InProgress</code> state to the <code>Locked</code> state, which causes the vault lock policy to become unchangeable. A vault lock is put into the <code>InProgress</code> state by calling <a>InitiateVaultLock</a>. You can obtain the state of the vault lock by calling <a>GetVaultLock</a>. For more information about the vault locking process, <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html">Amazon Glacier Vault Lock</a>. </p> <p>This operation is idempotent. This request is always successful if the vault lock is in the <code>Locked</code> state and the provided lock ID matches the lock ID originally used to lock the vault.</p> <p>If an invalid lock ID is passed in the request when the vault lock is in the <code>Locked</code> state, the operation returns an <code>AccessDeniedException</code> error. If an invalid lock ID is passed in the request when the vault lock is in the <code>InProgress</code> state, the operation throws an <code>InvalidParameter</code> error.</p>
completeVaultLock :: forall eff. CompleteVaultLockInput -> Aff (err :: AWS.RequestError | eff) Unit
completeVaultLock = AWS.request serviceName "CompleteVaultLock" 


-- | <p>This operation creates a new vault with the specified name. The name of the vault must be unique within a region for an AWS account. You can create up to 1,000 vaults per account. If you need to create more vaults, contact Amazon Glacier.</p> <p>You must use the following guidelines when naming a vault.</p> <ul> <li> <p>Names can be between 1 and 255 characters long.</p> </li> <li> <p>Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), and '.' (period).</p> </li> </ul> <p>This operation is idempotent.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/creating-vaults.html">Creating a Vault in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-put.html">Create Vault </a> in the <i>Amazon Glacier Developer Guide</i>. </p>
createVault :: forall eff. CreateVaultInput -> Aff (err :: AWS.RequestError | eff) CreateVaultOutput
createVault = AWS.request serviceName "CreateVault" 


-- | <p>This operation deletes an archive from a vault. Subsequent requests to initiate a retrieval of this archive will fail. Archive retrievals that are in progress for this archive ID may or may not succeed according to the following scenarios:</p> <ul> <li> <p>If the archive retrieval job is actively preparing the data for download when Amazon Glacier receives the delete archive request, the archival retrieval operation might fail.</p> </li> <li> <p>If the archive retrieval job has successfully prepared the archive for download when Amazon Glacier receives the delete archive request, you will be able to download the output.</p> </li> </ul> <p>This operation is idempotent. Attempting to delete an already-deleted archive does not result in an error.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-an-archive.html">Deleting an Archive in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html">Delete Archive</a> in the <i>Amazon Glacier Developer Guide</i>. </p>
deleteArchive :: forall eff. DeleteArchiveInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteArchive = AWS.request serviceName "DeleteArchive" 


-- | <p>This operation deletes a vault. Amazon Glacier will delete a vault only if there are no archives in the vault as of the last inventory and there have been no writes to the vault since the last inventory. If either of these conditions is not satisfied, the vault deletion fails (that is, the vault is not removed) and Amazon Glacier returns an error. You can use <a>DescribeVault</a> to return the number of archives in a vault, and you can use <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html">Initiate a Job (POST jobs)</a> to initiate a new inventory retrieval for a vault. The inventory contains the archive IDs you use to delete archives using <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html">Delete Archive (DELETE archive)</a>.</p> <p>This operation is idempotent.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-vaults.html">Deleting a Vault in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-delete.html">Delete Vault </a> in the <i>Amazon Glacier Developer Guide</i>. </p>
deleteVault :: forall eff. DeleteVaultInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteVault = AWS.request serviceName "DeleteVault" 


-- | <p>This operation deletes the access policy associated with the specified vault. The operation is eventually consistent; that is, it might take some time for Amazon Glacier to completely remove the access policy, and you might still see the effect of the policy for a short time after you send the delete request.</p> <p>This operation is idempotent. You can invoke delete multiple times, even if there is no policy associated with the vault. For more information about vault access policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html">Amazon Glacier Access Control with Vault Access Policies</a>. </p>
deleteVaultAccessPolicy :: forall eff. DeleteVaultAccessPolicyInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteVaultAccessPolicy = AWS.request serviceName "DeleteVaultAccessPolicy" 


-- | <p>This operation deletes the notification configuration set for a vault. The operation is eventually consistent; that is, it might take some time for Amazon Glacier to completely disable the notifications and you might still receive some notifications for a short time after you send the delete request.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html">Configuring Vault Notifications in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-delete.html">Delete Vault Notification Configuration </a> in the Amazon Glacier Developer Guide. </p>
deleteVaultNotifications :: forall eff. DeleteVaultNotificationsInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteVaultNotifications = AWS.request serviceName "DeleteVaultNotifications" 


-- | <p>This operation returns information about a job you previously initiated, including the job initiation date, the user who initiated the job, the job status code/message and the Amazon SNS topic to notify after Amazon Glacier completes the job. For more information about initiating a job, see <a>InitiateJob</a>. </p> <note> <p>This operation enables you to check the status of your job. However, it is strongly recommended that you set up an Amazon SNS topic and specify it in your initiate job request so that Amazon Glacier can notify the topic after it completes the job.</p> </note> <p>A job ID will not expire for at least 24 hours after Amazon Glacier completes the job.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For more information about using this operation, see the documentation for the underlying REST API <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-describe-job-get.html">Describe Job</a> in the <i>Amazon Glacier Developer Guide</i>. </p>
describeJob :: forall eff. DescribeJobInput -> Aff (err :: AWS.RequestError | eff) GlacierJobDescription
describeJob = AWS.request serviceName "DescribeJob" 


-- | <p>This operation returns information about a vault, including the vault's Amazon Resource Name (ARN), the date the vault was created, the number of archives it contains, and the total size of all the archives in the vault. The number of archives and their total size are as of the last inventory generation. This means that if you add or remove an archive from a vault, and then immediately use Describe Vault, the change in contents will not be immediately reflected. If you want to retrieve the latest inventory of the vault, use <a>InitiateJob</a>. Amazon Glacier generates vault inventories approximately daily. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html">Downloading a Vault Inventory in Amazon Glacier</a>. </p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html">Retrieving Vault Metadata in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-get.html">Describe Vault </a> in the <i>Amazon Glacier Developer Guide</i>. </p>
describeVault :: forall eff. DescribeVaultInput -> Aff (err :: AWS.RequestError | eff) DescribeVaultOutput
describeVault = AWS.request serviceName "DescribeVault" 


-- | <p>This operation returns the current data retrieval policy for the account and region specified in the GET request. For more information about data retrieval policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html">Amazon Glacier Data Retrieval Policies</a>.</p>
getDataRetrievalPolicy :: forall eff. GetDataRetrievalPolicyInput -> Aff (err :: AWS.RequestError | eff) GetDataRetrievalPolicyOutput
getDataRetrievalPolicy = AWS.request serviceName "GetDataRetrievalPolicy" 


-- | <p>This operation downloads the output of the job you initiated using <a>InitiateJob</a>. Depending on the job type you specified when you initiated the job, the output will be either the content of an archive or a vault inventory.</p> <p>You can download all the job output or download a portion of the output by specifying a byte range. In the case of an archive retrieval job, depending on the byte range you specify, Amazon Glacier returns the checksum for the portion of the data. You can compute the checksum on the client and verify that the values match to ensure the portion you downloaded is the correct data.</p> <p>A job ID will not expire for at least 24 hours after Amazon Glacier completes the job. That a byte range. For both archive and inventory retrieval jobs, you should verify the downloaded size against the size returned in the headers from the <b>Get Job Output</b> response.</p> <p>For archive retrieval jobs, you should also verify that the size is what you expected. If you download a portion of the output, the expected size is based on the range of bytes you specified. For example, if you specify a range of <code>bytes=0-1048575</code>, you should verify your download size is 1,048,576 bytes. If you download an entire archive, the expected size is the size of the archive when you uploaded it to Amazon Glacier The expected size is also returned in the headers from the <b>Get Job Output</b> response.</p> <p>In the case of an archive retrieval job, depending on the byte range you specify, Amazon Glacier returns the checksum for the portion of the data. To ensure the portion you downloaded is the correct data, compute the checksum on the client, verify that the values match, and verify that the size is what you expected.</p> <p>A job ID does not expire for at least 24 hours after Amazon Glacier completes the job. That is, you can download the job output within the 24 hours period after Amazon Glacier completes the job.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and the underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html">Downloading a Vault Inventory</a>, <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/downloading-an-archive.html">Downloading an Archive</a>, and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-job-output-get.html">Get Job Output </a> </p>
getJobOutput :: forall eff. GetJobOutputInput -> Aff (err :: AWS.RequestError | eff) GetJobOutputOutput
getJobOutput = AWS.request serviceName "GetJobOutput" 


-- | <p>This operation retrieves the <code>access-policy</code> subresource set on the vault; for more information on setting this subresource, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-SetVaultAccessPolicy.html">Set Vault Access Policy (PUT access-policy)</a>. If there is no access policy set on the vault, the operation returns a <code>404 Not found</code> error. For more information about vault access policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html">Amazon Glacier Access Control with Vault Access Policies</a>.</p>
getVaultAccessPolicy :: forall eff. GetVaultAccessPolicyInput -> Aff (err :: AWS.RequestError | eff) GetVaultAccessPolicyOutput
getVaultAccessPolicy = AWS.request serviceName "GetVaultAccessPolicy" 


-- | <p>This operation retrieves the following attributes from the <code>lock-policy</code> subresource set on the specified vault: </p> <ul> <li> <p>The vault lock policy set on the vault.</p> </li> <li> <p>The state of the vault lock, which is either <code>InProgess</code> or <code>Locked</code>.</p> </li> <li> <p>When the lock ID expires. The lock ID is used to complete the vault locking process.</p> </li> <li> <p>When the vault lock was initiated and put into the <code>InProgress</code> state.</p> </li> </ul> <p>A vault lock is put into the <code>InProgress</code> state by calling <a>InitiateVaultLock</a>. A vault lock is put into the <code>Locked</code> state by calling <a>CompleteVaultLock</a>. You can abort the vault locking process by calling <a>AbortVaultLock</a>. For more information about the vault locking process, <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html">Amazon Glacier Vault Lock</a>. </p> <p>If there is no vault lock policy set on the vault, the operation returns a <code>404 Not found</code> error. For more information about vault lock policies, <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html">Amazon Glacier Access Control with Vault Lock Policies</a>. </p>
getVaultLock :: forall eff. GetVaultLockInput -> Aff (err :: AWS.RequestError | eff) GetVaultLockOutput
getVaultLock = AWS.request serviceName "GetVaultLock" 


-- | <p>This operation retrieves the <code>notification-configuration</code> subresource of the specified vault.</p> <p>For information about setting a notification configuration on a vault, see <a>SetVaultNotifications</a>. If a notification configuration for a vault is not set, the operation returns a <code>404 Not Found</code> error. For more information about vault notifications, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html">Configuring Vault Notifications in Amazon Glacier</a>. </p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html">Configuring Vault Notifications in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-get.html">Get Vault Notification Configuration </a> in the <i>Amazon Glacier Developer Guide</i>. </p>
getVaultNotifications :: forall eff. GetVaultNotificationsInput -> Aff (err :: AWS.RequestError | eff) GetVaultNotificationsOutput
getVaultNotifications = AWS.request serviceName "GetVaultNotifications" 


-- | <p>This operation initiates a job of the specified type, which can be a select, an archival retrieval, or a vault retrieval. For more information about using this operation, see the documentation for the underlying REST API <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html">Initiate a Job</a>. </p>
initiateJob :: forall eff. InitiateJobInput -> Aff (err :: AWS.RequestError | eff) InitiateJobOutput
initiateJob = AWS.request serviceName "InitiateJob" 


-- | <p>This operation initiates a multipart upload. Amazon Glacier creates a multipart upload resource and returns its ID in the response. The multipart upload ID is used in subsequent requests to upload parts of an archive (see <a>UploadMultipartPart</a>).</p> <p>When you initiate a multipart upload, you specify the part size in number of bytes. The part size must be a megabyte (1024 KB) multiplied by a power of 2-for example, 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8 MB), and so on. The minimum allowable part size is 1 MB, and the maximum is 4 GB.</p> <p>Every part you upload to this resource (see <a>UploadMultipartPart</a>), except the last one, must have the same size. The last one can be the same size or smaller. For example, suppose you want to upload a 16.2 MB file. If you initiate the multipart upload with a part size of 4 MB, you will upload four parts of 4 MB each and one part of 0.2 MB. </p> <note> <p>You don't need to know the size of the archive when you start a multipart upload because Amazon Glacier does not require you to specify the overall archive size.</p> </note> <p>After you complete the multipart upload, Amazon Glacier removes the multipart upload resource referenced by the ID. Amazon Glacier also removes the multipart upload resource if you cancel the multipart upload or it may be removed if there is no activity for a period of 24 hours.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html">Uploading Large Archives in Parts (Multipart Upload)</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-initiate-upload.html">Initiate Multipart Upload</a> in the <i>Amazon Glacier Developer Guide</i>.</p>
initiateMultipartUpload :: forall eff. InitiateMultipartUploadInput -> Aff (err :: AWS.RequestError | eff) InitiateMultipartUploadOutput
initiateMultipartUpload = AWS.request serviceName "InitiateMultipartUpload" 


-- | <p>This operation initiates the vault locking process by doing the following:</p> <ul> <li> <p>Installing a vault lock policy on the specified vault.</p> </li> <li> <p>Setting the lock state of vault lock to <code>InProgress</code>.</p> </li> <li> <p>Returning a lock ID, which is used to complete the vault locking process.</p> </li> </ul> <p>You can set one vault lock policy for each vault and this policy can be up to 20 KB in size. For more information about vault lock policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html">Amazon Glacier Access Control with Vault Lock Policies</a>. </p> <p>You must complete the vault locking process within 24 hours after the vault lock enters the <code>InProgress</code> state. After the 24 hour window ends, the lock ID expires, the vault automatically exits the <code>InProgress</code> state, and the vault lock policy is removed from the vault. You call <a>CompleteVaultLock</a> to complete the vault locking process by setting the state of the vault lock to <code>Locked</code>. </p> <p>After a vault lock is in the <code>Locked</code> state, you cannot initiate a new vault lock for the vault.</p> <p>You can abort the vault locking process by calling <a>AbortVaultLock</a>. You can get the state of the vault lock by calling <a>GetVaultLock</a>. For more information about the vault locking process, <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html">Amazon Glacier Vault Lock</a>.</p> <p>If this operation is called when the vault lock is in the <code>InProgress</code> state, the operation returns an <code>AccessDeniedException</code> error. When the vault lock is in the <code>InProgress</code> state you must call <a>AbortVaultLock</a> before you can initiate a new vault lock policy. </p>
initiateVaultLock :: forall eff. InitiateVaultLockInput -> Aff (err :: AWS.RequestError | eff) InitiateVaultLockOutput
initiateVaultLock = AWS.request serviceName "InitiateVaultLock" 


-- | <p>This operation lists jobs for a vault, including jobs that are in-progress and jobs that have recently finished. The List Job operation returns a list of these jobs sorted by job initiation time.</p> <note> <p>Amazon Glacier retains recently completed jobs for a period before deleting them; however, it eventually removes completed jobs. The output of completed jobs can be retrieved. Retaining completed jobs for a period of time after they have completed enables you to get a job output in the event you miss the job completion notification or your first attempt to download it fails. For example, suppose you start an archive retrieval job to download an archive. After the job completes, you start to download the archive but encounter a network error. In this scenario, you can retry and download the archive while the job exists.</p> </note> <p>The List Jobs operation supports pagination. You should always check the response <code>Marker</code> field. If there are no more jobs to list, the <code>Marker</code> field is set to <code>null</code>. If there are more jobs to list, the <code>Marker</code> field is set to a non-null value, which you can use to continue the pagination of the list. To return a list of jobs that begins at a specific job, set the marker request parameter to the <code>Marker</code> value for that job that you obtained from a previous List Jobs request.</p> <p>You can set a maximum limit for the number of jobs returned in the response by specifying the <code>limit</code> parameter in the request. The default limit is 1000. The number of jobs returned might be fewer than the limit, but the number of returned jobs never exceeds the limit.</p> <p>Additionally, you can filter the jobs list returned by specifying the optional <code>statuscode</code> parameter or <code>completed</code> parameter, or both. Using the <code>statuscode</code> parameter, you can specify to return only jobs that match either the <code>InProgress</code>, <code>Succeeded</code>, or <code>Failed</code> status. Using the <code>completed</code> parameter, you can specify to return only jobs that were completed (<code>true</code>) or jobs that were not completed (<code>false</code>).</p> <p>For more information about using this operation, see the documentation for the underlying REST API <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-jobs-get.html">List Jobs</a>. </p>
listJobs :: forall eff. ListJobsInput -> Aff (err :: AWS.RequestError | eff) ListJobsOutput
listJobs = AWS.request serviceName "ListJobs" 


-- | <p>This operation lists in-progress multipart uploads for the specified vault. An in-progress multipart upload is a multipart upload that has been initiated by an <a>InitiateMultipartUpload</a> request, but has not yet been completed or aborted. The list returned in the List Multipart Upload response has no guaranteed order. </p> <p>The List Multipart Uploads operation supports pagination. By default, this operation returns up to 1,000 multipart uploads in the response. You should always check the response for a <code>marker</code> at which to continue the list; if there are no more items the <code>marker</code> is <code>null</code>. To return a list of multipart uploads that begins at a specific upload, set the <code>marker</code> request parameter to the value you obtained from a previous List Multipart Upload request. You can also limit the number of uploads returned in the response by specifying the <code>limit</code> parameter in the request.</p> <p>Note the difference between this operation and listing parts (<a>ListParts</a>). The List Multipart Uploads operation lists all multipart uploads for a vault and does not require a multipart upload ID. The List Parts operation requires a multipart upload ID since parts are associated with a single upload.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and the underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html">Working with Archives in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-uploads.html">List Multipart Uploads </a> in the <i>Amazon Glacier Developer Guide</i>.</p>
listMultipartUploads :: forall eff. ListMultipartUploadsInput -> Aff (err :: AWS.RequestError | eff) ListMultipartUploadsOutput
listMultipartUploads = AWS.request serviceName "ListMultipartUploads" 


-- | <p>This operation lists the parts of an archive that have been uploaded in a specific multipart upload. You can make this request at any time during an in-progress multipart upload before you complete the upload (see <a>CompleteMultipartUpload</a>. List Parts returns an error for completed uploads. The list returned in the List Parts response is sorted by part range. </p> <p>The List Parts operation supports pagination. By default, this operation returns up to 1,000 uploaded parts in the response. You should always check the response for a <code>marker</code> at which to continue the list; if there are no more items the <code>marker</code> is <code>null</code>. To return a list of parts that begins at a specific part, set the <code>marker</code> request parameter to the value you obtained from a previous List Parts request. You can also limit the number of parts returned in the response by specifying the <code>limit</code> parameter in the request. </p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and the underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html">Working with Archives in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-parts.html">List Parts</a> in the <i>Amazon Glacier Developer Guide</i>.</p>
listParts :: forall eff. ListPartsInput -> Aff (err :: AWS.RequestError | eff) ListPartsOutput
listParts = AWS.request serviceName "ListParts" 


-- | <p>This operation lists the provisioned capacity units for the specified AWS account.</p>
listProvisionedCapacity :: forall eff. ListProvisionedCapacityInput -> Aff (err :: AWS.RequestError | eff) ListProvisionedCapacityOutput
listProvisionedCapacity = AWS.request serviceName "ListProvisionedCapacity" 


-- | <p>This operation lists all the tags attached to a vault. The operation returns an empty map if there are no tags. For more information about tags, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html">Tagging Amazon Glacier Resources</a>.</p>
listTagsForVault :: forall eff. ListTagsForVaultInput -> Aff (err :: AWS.RequestError | eff) ListTagsForVaultOutput
listTagsForVault = AWS.request serviceName "ListTagsForVault" 


-- | <p>This operation lists all vaults owned by the calling user's account. The list returned in the response is ASCII-sorted by vault name.</p> <p>By default, this operation returns up to 1,000 items. If there are more vaults to list, the response <code>marker</code> field contains the vault Amazon Resource Name (ARN) at which to continue the list with a new List Vaults request; otherwise, the <code>marker</code> field is <code>null</code>. To return a list of vaults that begins at a specific vault, set the <code>marker</code> request parameter to the vault ARN you obtained from a previous List Vaults request. You can also limit the number of vaults returned in the response by specifying the <code>limit</code> parameter in the request. </p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html">Retrieving Vault Metadata in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vaults-get.html">List Vaults </a> in the <i>Amazon Glacier Developer Guide</i>. </p>
listVaults :: forall eff. ListVaultsInput -> Aff (err :: AWS.RequestError | eff) ListVaultsOutput
listVaults = AWS.request serviceName "ListVaults" 


-- | <p>This operation purchases a provisioned capacity unit for an AWS account. </p>
purchaseProvisionedCapacity :: forall eff. PurchaseProvisionedCapacityInput -> Aff (err :: AWS.RequestError | eff) PurchaseProvisionedCapacityOutput
purchaseProvisionedCapacity = AWS.request serviceName "PurchaseProvisionedCapacity" 


-- | <p>This operation removes one or more tags from the set of tags attached to a vault. For more information about tags, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/tagging.html">Tagging Amazon Glacier Resources</a>. This operation is idempotent. The operation will be successful, even if there are no tags attached to the vault. </p>
removeTagsFromVault :: forall eff. RemoveTagsFromVaultInput -> Aff (err :: AWS.RequestError | eff) Unit
removeTagsFromVault = AWS.request serviceName "RemoveTagsFromVault" 


-- | <p>This operation sets and then enacts a data retrieval policy in the region specified in the PUT request. You can set one policy per region for an AWS account. The policy is enacted within a few minutes of a successful PUT operation.</p> <p>The set policy operation does not affect retrieval jobs that were in progress before the policy was enacted. For more information about data retrieval policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html">Amazon Glacier Data Retrieval Policies</a>. </p>
setDataRetrievalPolicy :: forall eff. SetDataRetrievalPolicyInput -> Aff (err :: AWS.RequestError | eff) Unit
setDataRetrievalPolicy = AWS.request serviceName "SetDataRetrievalPolicy" 


-- | <p>This operation configures an access policy for a vault and will overwrite an existing policy. To configure a vault access policy, send a PUT request to the <code>access-policy</code> subresource of the vault. An access policy is specific to a vault and is also called a vault subresource. You can set one access policy per vault and the policy can be up to 20 KB in size. For more information about vault access policies, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html">Amazon Glacier Access Control with Vault Access Policies</a>. </p>
setVaultAccessPolicy :: forall eff. SetVaultAccessPolicyInput -> Aff (err :: AWS.RequestError | eff) Unit
setVaultAccessPolicy = AWS.request serviceName "SetVaultAccessPolicy" 


-- | <p>This operation configures notifications that will be sent when specific events happen to a vault. By default, you don't get any notifications.</p> <p>To configure vault notifications, send a PUT request to the <code>notification-configuration</code> subresource of the vault. The request should include a JSON document that provides an Amazon SNS topic and specific events for which you want Amazon Glacier to send notifications to the topic.</p> <p>Amazon SNS topics must grant permission to the vault to be allowed to publish notifications to the topic. You can configure a vault to publish a notification for the following vault events:</p> <ul> <li> <p> <b>ArchiveRetrievalCompleted</b> This event occurs when a job that was initiated for an archive retrieval is completed (<a>InitiateJob</a>). The status of the completed job can be "Succeeded" or "Failed". The notification sent to the SNS topic is the same output as returned from <a>DescribeJob</a>. </p> </li> <li> <p> <b>InventoryRetrievalCompleted</b> This event occurs when a job that was initiated for an inventory retrieval is completed (<a>InitiateJob</a>). The status of the completed job can be "Succeeded" or "Failed". The notification sent to the SNS topic is the same output as returned from <a>DescribeJob</a>. </p> </li> </ul> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p>For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html">Configuring Vault Notifications in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-put.html">Set Vault Notification Configuration </a> in the <i>Amazon Glacier Developer Guide</i>. </p>
setVaultNotifications :: forall eff. SetVaultNotificationsInput -> Aff (err :: AWS.RequestError | eff) Unit
setVaultNotifications = AWS.request serviceName "SetVaultNotifications" 


-- | <p>This operation adds an archive to a vault. This is a synchronous operation, and for a successful upload, your data is durably persisted. Amazon Glacier returns the archive ID in the <code>x-amz-archive-id</code> header of the response. </p> <p>You must use the archive ID to access your data in Amazon Glacier. After you upload an archive, you should save the archive ID returned so that you can retrieve or delete the archive later. Besides saving the archive ID, you can also index it and give it a friendly name to allow for better searching. You can also use the optional archive description field to specify how the archive is referred to in an external index of archives, such as you might create in Amazon DynamoDB. You can also get the vault inventory to obtain a list of archive IDs in a vault. For more information, see <a>InitiateJob</a>. </p> <p>You must provide a SHA256 tree hash of the data you are uploading. For information about computing a SHA256 tree hash, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html">Computing Checksums</a>. </p> <p>You can optionally specify an archive description of up to 1,024 printable ASCII characters. You can get the archive description when you either retrieve the archive or get the vault inventory. For more information, see <a>InitiateJob</a>. Amazon Glacier does not interpret the description in any way. An archive description does not need to be unique. You cannot use the description to retrieve or sort the archive list. </p> <p>Archives are immutable. After you upload an archive, you cannot edit the archive or its description.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-an-archive.html">Uploading an Archive in Amazon Glacier</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html">Upload Archive</a> in the <i>Amazon Glacier Developer Guide</i>. </p>
uploadArchive :: forall eff. UploadArchiveInput -> Aff (err :: AWS.RequestError | eff) ArchiveCreationOutput
uploadArchive = AWS.request serviceName "UploadArchive" 


-- | <p>This operation uploads a part of an archive. You can upload archive parts in any order. You can also upload them in parallel. You can upload up to 10,000 parts for a multipart upload.</p> <p>Amazon Glacier rejects your upload part request if any of the following conditions is true:</p> <ul> <li> <p> <b>SHA256 tree hash does not match</b>To ensure that part data is not corrupted in transmission, you compute a SHA256 tree hash of the part and include it in your request. Upon receiving the part data, Amazon Glacier also computes a SHA256 tree hash. If these hash values don't match, the operation fails. For information about computing a SHA256 tree hash, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html">Computing Checksums</a>.</p> </li> <li> <p> <b>Part size does not match</b>The size of each part except the last must match the size specified in the corresponding <a>InitiateMultipartUpload</a> request. The size of the last part must be the same size as, or smaller than, the specified size.</p> <note> <p>If you upload a part whose size is smaller than the part size you specified in your initiate multipart upload request and that part is not the last part, then the upload part request will succeed. However, the subsequent Complete Multipart Upload request will fail.</p> </note> </li> <li> <p> <b>Range does not align</b>The byte range value in the request does not align with the part size specified in the corresponding initiate request. For example, if you specify a part size of 4194304 bytes (4 MB), then 0 to 4194303 bytes (4 MB - 1) and 4194304 (4 MB) to 8388607 (8 MB - 1) are valid part ranges. However, if you set a range value of 2 MB to 6 MB, the range does not align with the part size and the upload will fail. </p> </li> </ul> <p>This operation is idempotent. If you upload the same part multiple times, the data included in the most recent request overwrites the previously uploaded data.</p> <p>An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html">Access Control Using AWS Identity and Access Management (IAM)</a>.</p> <p> For conceptual information and underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html">Uploading Large Archives in Parts (Multipart Upload)</a> and <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-upload-part.html">Upload Part </a> in the <i>Amazon Glacier Developer Guide</i>.</p>
uploadMultipartPart :: forall eff. UploadMultipartPartInput -> Aff (err :: AWS.RequestError | eff) UploadMultipartPartOutput
uploadMultipartPart = AWS.request serviceName "UploadMultipartPart" 


-- | <p>Provides options to abort a multipart upload identified by the upload ID.</p> <p>For information about the underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html">Abort Multipart Upload</a>. For conceptual information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html">Working with Archives in Amazon Glacier</a>.</p>
newtype AbortMultipartUploadInput = AbortMultipartUploadInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "UploadId'" :: (String)
  }


-- | <p>The input values for <code>AbortVaultLock</code>.</p>
newtype AbortVaultLockInput = AbortVaultLockInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }


newtype AccessControlPolicyList = AccessControlPolicyList (Array Grant)


newtype ActionCode = ActionCode String


-- | <p>The input values for <code>AddTagsToVault</code>.</p>
newtype AddTagsToVaultInput = AddTagsToVaultInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "Tags" :: NullOrUndefined (TagMap)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p> <p>For information about the underlying REST API, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html">Upload Archive</a>. For conceptual information, see <a href="http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html">Working with Archives in Amazon Glacier</a>.</p>
newtype ArchiveCreationOutput = ArchiveCreationOutput 
  { "Location'" :: NullOrUndefined (String)
  , "Checksum'" :: NullOrUndefined (String)
  , "ArchiveId'" :: NullOrUndefined (String)
  }


-- | <p>Contains information about the comma-separated value (CSV) file to select from.</p>
newtype CSVInput = CSVInput 
  { "FileHeaderInfo" :: NullOrUndefined (FileHeaderInfo)
  , "Comments" :: NullOrUndefined (String)
  , "QuoteEscapeCharacter" :: NullOrUndefined (String)
  , "RecordDelimiter" :: NullOrUndefined (String)
  , "FieldDelimiter" :: NullOrUndefined (String)
  , "QuoteCharacter" :: NullOrUndefined (String)
  }


-- | <p>Contains information about the comma-separated value (CSV) file that the job results are stored in.</p>
newtype CSVOutput = CSVOutput 
  { "QuoteFields" :: NullOrUndefined (QuoteFields)
  , "QuoteEscapeCharacter" :: NullOrUndefined (String)
  , "RecordDelimiter" :: NullOrUndefined (String)
  , "FieldDelimiter" :: NullOrUndefined (String)
  , "QuoteCharacter" :: NullOrUndefined (String)
  }


newtype CannedACL = CannedACL String


-- | <p>Provides options to complete a multipart upload operation. This informs Amazon Glacier that all the archive parts have been uploaded and Amazon Glacier can now assemble the archive from the uploaded parts. After assembling and saving the archive to the vault, Amazon Glacier returns the URI path of the newly created archive resource.</p>
newtype CompleteMultipartUploadInput = CompleteMultipartUploadInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "UploadId'" :: (String)
  , "ArchiveSize'" :: NullOrUndefined (String)
  , "Checksum'" :: NullOrUndefined (String)
  }


-- | <p>The input values for <code>CompleteVaultLock</code>.</p>
newtype CompleteVaultLockInput = CompleteVaultLockInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "LockId'" :: (String)
  }


-- | <p>Provides options to create a vault.</p>
newtype CreateVaultInput = CreateVaultInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype CreateVaultOutput = CreateVaultOutput 
  { "Location'" :: NullOrUndefined (String)
  }


-- | <p>Data retrieval policy.</p>
newtype DataRetrievalPolicy = DataRetrievalPolicy 
  { "Rules" :: NullOrUndefined (DataRetrievalRulesList)
  }


-- | <p>Data retrieval policy rule.</p>
newtype DataRetrievalRule = DataRetrievalRule 
  { "Strategy" :: NullOrUndefined (String)
  , "BytesPerHour" :: NullOrUndefined (NullableLong)
  }


newtype DataRetrievalRulesList = DataRetrievalRulesList (Array DataRetrievalRule)


newtype DateTime = DateTime String


-- | <p>Provides options for deleting an archive from an Amazon Glacier vault.</p>
newtype DeleteArchiveInput = DeleteArchiveInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "ArchiveId'" :: (String)
  }


-- | <p>DeleteVaultAccessPolicy input.</p>
newtype DeleteVaultAccessPolicyInput = DeleteVaultAccessPolicyInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }


-- | <p>Provides options for deleting a vault from Amazon Glacier.</p>
newtype DeleteVaultInput = DeleteVaultInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }


-- | <p>Provides options for deleting a vault notification configuration from an Amazon Glacier vault.</p>
newtype DeleteVaultNotificationsInput = DeleteVaultNotificationsInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }


-- | <p>Provides options for retrieving a job description.</p>
newtype DescribeJobInput = DescribeJobInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "JobId'" :: (String)
  }


-- | <p>Provides options for retrieving metadata for a specific vault in Amazon Glacier.</p>
newtype DescribeVaultInput = DescribeVaultInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype DescribeVaultOutput = DescribeVaultOutput 
  { "VaultARN" :: NullOrUndefined (String)
  , "VaultName" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "LastInventoryDate" :: NullOrUndefined (String)
  , "NumberOfArchives" :: NullOrUndefined (Number)
  , "SizeInBytes" :: NullOrUndefined (Number)
  }


-- | <p>Contains information about the encryption used to store the job results in Amazon S3. </p>
newtype Encryption = Encryption 
  { "EncryptionType" :: NullOrUndefined (EncryptionType)
  , "KMSKeyId" :: NullOrUndefined (String)
  , "KMSContext" :: NullOrUndefined (String)
  }


newtype EncryptionType = EncryptionType String


newtype ExpressionType = ExpressionType String


newtype FileHeaderInfo = FileHeaderInfo String


-- | <p>Input for GetDataRetrievalPolicy.</p>
newtype GetDataRetrievalPolicyInput = GetDataRetrievalPolicyInput 
  { "AccountId'" :: (String)
  }


-- | <p>Contains the Amazon Glacier response to the <code>GetDataRetrievalPolicy</code> request.</p>
newtype GetDataRetrievalPolicyOutput = GetDataRetrievalPolicyOutput 
  { "Policy" :: NullOrUndefined (DataRetrievalPolicy)
  }


-- | <p>Provides options for downloading output of an Amazon Glacier job.</p>
newtype GetJobOutputInput = GetJobOutputInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "JobId'" :: (String)
  , "Range'" :: NullOrUndefined (String)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype GetJobOutputOutput = GetJobOutputOutput 
  { "Body'" :: NullOrUndefined (Stream)
  , "Checksum'" :: NullOrUndefined (String)
  , "Status'" :: NullOrUndefined (Httpstatus')
  , "ContentRange'" :: NullOrUndefined (String)
  , "AcceptRanges'" :: NullOrUndefined (String)
  , "ContentType'" :: NullOrUndefined (String)
  , "ArchiveDescription'" :: NullOrUndefined (String)
  }


-- | <p>Input for GetVaultAccessPolicy.</p>
newtype GetVaultAccessPolicyInput = GetVaultAccessPolicyInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }


-- | <p>Output for GetVaultAccessPolicy.</p>
newtype GetVaultAccessPolicyOutput = GetVaultAccessPolicyOutput 
  { "Policy'" :: NullOrUndefined (VaultAccessPolicy)
  }


-- | <p>The input values for <code>GetVaultLock</code>.</p>
newtype GetVaultLockInput = GetVaultLockInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype GetVaultLockOutput = GetVaultLockOutput 
  { "Policy" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (String)
  , "ExpirationDate" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  }


-- | <p>Provides options for retrieving the notification configuration set on an Amazon Glacier vault.</p>
newtype GetVaultNotificationsInput = GetVaultNotificationsInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype GetVaultNotificationsOutput = GetVaultNotificationsOutput 
  { "VaultNotificationConfig'" :: NullOrUndefined (VaultNotificationConfig)
  }


-- | <p>Contains the description of an Amazon Glacier job.</p>
newtype GlacierJobDescription = GlacierJobDescription 
  { "JobId" :: NullOrUndefined (String)
  , "JobDescription" :: NullOrUndefined (String)
  , "Action" :: NullOrUndefined (ActionCode)
  , "ArchiveId" :: NullOrUndefined (String)
  , "VaultARN" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "Completed" :: NullOrUndefined (Boolean)
  , "StatusCode" :: NullOrUndefined (StatusCode)
  , "StatusMessage" :: NullOrUndefined (String)
  , "ArchiveSizeInBytes" :: NullOrUndefined (Size)
  , "InventorySizeInBytes" :: NullOrUndefined (Size)
  , "SNSTopic" :: NullOrUndefined (String)
  , "CompletionDate" :: NullOrUndefined (String)
  , "SHA256TreeHash" :: NullOrUndefined (String)
  , "ArchiveSHA256TreeHash" :: NullOrUndefined (String)
  , "RetrievalByteRange" :: NullOrUndefined (String)
  , "Tier" :: NullOrUndefined (String)
  , "InventoryRetrievalParameters" :: NullOrUndefined (InventoryRetrievalJobDescription)
  , "JobOutputPath" :: NullOrUndefined (String)
  , "SelectParameters" :: NullOrUndefined (SelectParameters)
  , "OutputLocation" :: NullOrUndefined (OutputLocation)
  }


-- | <p>Contains information about a grant.</p>
newtype Grant = Grant 
  { "Grantee" :: NullOrUndefined (Grantee)
  , "Permission" :: NullOrUndefined (Permission)
  }


-- | <p>Contains information about the grantee.</p>
newtype Grantee = Grantee 
  { "Type" :: (Type)
  , "DisplayName" :: NullOrUndefined (String)
  , "URI" :: NullOrUndefined (String)
  , "ID" :: NullOrUndefined (String)
  , "EmailAddress" :: NullOrUndefined (String)
  }


-- | <p>Provides options for initiating an Amazon Glacier job.</p>
newtype InitiateJobInput = InitiateJobInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "JobParameters'" :: NullOrUndefined (JobParameters)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype InitiateJobOutput = InitiateJobOutput 
  { "Location'" :: NullOrUndefined (String)
  , "JobId'" :: NullOrUndefined (String)
  , "JobOutputPath'" :: NullOrUndefined (String)
  }


-- | <p>Provides options for initiating a multipart upload to an Amazon Glacier vault.</p>
newtype InitiateMultipartUploadInput = InitiateMultipartUploadInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "ArchiveDescription'" :: NullOrUndefined (String)
  , "PartSize'" :: NullOrUndefined (String)
  }


-- | <p>The Amazon Glacier response to your request.</p>
newtype InitiateMultipartUploadOutput = InitiateMultipartUploadOutput 
  { "Location'" :: NullOrUndefined (String)
  , "UploadId'" :: NullOrUndefined (String)
  }


-- | <p>The input values for <code>InitiateVaultLock</code>.</p>
newtype InitiateVaultLockInput = InitiateVaultLockInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "Policy'" :: NullOrUndefined (VaultLockPolicy)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype InitiateVaultLockOutput = InitiateVaultLockOutput 
  { "LockId'" :: NullOrUndefined (String)
  }


-- | <p>Describes how the archive is serialized.</p>
newtype InputSerialization = InputSerialization 
  { "Csv'" :: NullOrUndefined (CSVInput)
  }


-- | <p>Returned if there is insufficient capacity to process this expedited request. This error only applies to expedited retrievals and not to standard or bulk retrievals.</p>
newtype InsufficientCapacityException = InsufficientCapacityException 
  { "Type'" :: NullOrUndefined (String)
  , "Code'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }


-- | <p>Returned if a parameter of the request is incorrectly specified.</p>
newtype InvalidParameterValueException = InvalidParameterValueException 
  { "Type'" :: NullOrUndefined (String)
  , "Code'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }


-- | <p>Describes the options for a range inventory retrieval job.</p>
newtype InventoryRetrievalJobDescription = InventoryRetrievalJobDescription 
  { "Format" :: NullOrUndefined (String)
  , "StartDate" :: NullOrUndefined (DateTime)
  , "EndDate" :: NullOrUndefined (DateTime)
  , "Limit" :: NullOrUndefined (String)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>Provides options for specifying a range inventory retrieval job.</p>
newtype InventoryRetrievalJobInput = InventoryRetrievalJobInput 
  { "StartDate" :: NullOrUndefined (String)
  , "EndDate" :: NullOrUndefined (String)
  , "Limit" :: NullOrUndefined (String)
  , "Marker" :: NullOrUndefined (String)
  }


newtype JobList = JobList (Array GlacierJobDescription)


-- | <p>Provides options for defining a job.</p>
newtype JobParameters = JobParameters 
  { "Format" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  , "ArchiveId" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "SNSTopic" :: NullOrUndefined (String)
  , "RetrievalByteRange" :: NullOrUndefined (String)
  , "Tier" :: NullOrUndefined (String)
  , "InventoryRetrievalParameters" :: NullOrUndefined (InventoryRetrievalJobInput)
  , "SelectParameters" :: NullOrUndefined (SelectParameters)
  , "OutputLocation" :: NullOrUndefined (OutputLocation)
  }


-- | <p>Returned if the request results in a vault or account limit being exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Type'" :: NullOrUndefined (String)
  , "Code'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }


-- | <p>Provides options for retrieving a job list for an Amazon Glacier vault.</p>
newtype ListJobsInput = ListJobsInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "Limit'" :: NullOrUndefined (String)
  , "Marker'" :: NullOrUndefined (String)
  , "Statuscode'" :: NullOrUndefined (String)
  , "Completed'" :: NullOrUndefined (String)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype ListJobsOutput = ListJobsOutput 
  { "JobList" :: NullOrUndefined (JobList)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>Provides options for retrieving list of in-progress multipart uploads for an Amazon Glacier vault.</p>
newtype ListMultipartUploadsInput = ListMultipartUploadsInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "Marker'" :: NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined (String)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype ListMultipartUploadsOutput = ListMultipartUploadsOutput 
  { "UploadsList" :: NullOrUndefined (UploadsList)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>Provides options for retrieving a list of parts of an archive that have been uploaded in a specific multipart upload.</p>
newtype ListPartsInput = ListPartsInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "UploadId'" :: (String)
  , "Marker'" :: NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined (String)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype ListPartsOutput = ListPartsOutput 
  { "MultipartUploadId" :: NullOrUndefined (String)
  , "VaultARN" :: NullOrUndefined (String)
  , "ArchiveDescription" :: NullOrUndefined (String)
  , "PartSizeInBytes" :: NullOrUndefined (Number)
  , "CreationDate" :: NullOrUndefined (String)
  , "Parts" :: NullOrUndefined (PartList)
  , "Marker" :: NullOrUndefined (String)
  }


newtype ListProvisionedCapacityInput = ListProvisionedCapacityInput 
  { "AccountId'" :: (String)
  }


newtype ListProvisionedCapacityOutput = ListProvisionedCapacityOutput 
  { "ProvisionedCapacityList" :: NullOrUndefined (ProvisionedCapacityList)
  }


-- | <p>The input value for <code>ListTagsForVaultInput</code>.</p>
newtype ListTagsForVaultInput = ListTagsForVaultInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype ListTagsForVaultOutput = ListTagsForVaultOutput 
  { "Tags" :: NullOrUndefined (TagMap)
  }


-- | <p>Provides options to retrieve the vault list owned by the calling user's account. The list provides metadata information for each vault.</p>
newtype ListVaultsInput = ListVaultsInput 
  { "AccountId'" :: (String)
  , "Marker'" :: NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined (String)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype ListVaultsOutput = ListVaultsOutput 
  { "VaultList" :: NullOrUndefined (VaultList)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>Returned if a required header or parameter is missing from the request.</p>
newtype MissingParameterValueException = MissingParameterValueException 
  { "Type'" :: NullOrUndefined (String)
  , "Code'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }


newtype NotificationEventList = NotificationEventList (Array String)


newtype NullableLong = NullableLong Number


-- | <p>Contains information about the location where the select job results are stored.</p>
newtype OutputLocation = OutputLocation 
  { "S3" :: NullOrUndefined (S3Location)
  }


-- | <p>Describes how the select output is serialized.</p>
newtype OutputSerialization = OutputSerialization 
  { "Csv'" :: NullOrUndefined (CSVOutput)
  }


newtype PartList = PartList (Array PartListElement)


-- | <p>A list of the part sizes of the multipart upload.</p>
newtype PartListElement = PartListElement 
  { "RangeInBytes" :: NullOrUndefined (String)
  , "SHA256TreeHash" :: NullOrUndefined (String)
  }


newtype Permission = Permission String


-- | <p>Returned if a retrieval job would exceed the current data policy's retrieval rate limit. For more information about data retrieval policies,</p>
newtype PolicyEnforcedException = PolicyEnforcedException 
  { "Type'" :: NullOrUndefined (String)
  , "Code'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }


-- | <p>The definition for a provisioned capacity unit.</p>
newtype ProvisionedCapacityDescription = ProvisionedCapacityDescription 
  { "CapacityId" :: NullOrUndefined (String)
  , "StartDate" :: NullOrUndefined (String)
  , "ExpirationDate" :: NullOrUndefined (String)
  }


newtype ProvisionedCapacityList = ProvisionedCapacityList (Array ProvisionedCapacityDescription)


newtype PurchaseProvisionedCapacityInput = PurchaseProvisionedCapacityInput 
  { "AccountId'" :: (String)
  }


newtype PurchaseProvisionedCapacityOutput = PurchaseProvisionedCapacityOutput 
  { "CapacityId'" :: NullOrUndefined (String)
  }


newtype QuoteFields = QuoteFields String


-- | <p>The input value for <code>RemoveTagsFromVaultInput</code>.</p>
newtype RemoveTagsFromVaultInput = RemoveTagsFromVaultInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "TagKeys" :: NullOrUndefined (TagKeyList)
  }


-- | <p>Returned if, when uploading an archive, Amazon Glacier times out while receiving the upload.</p>
newtype RequestTimeoutException = RequestTimeoutException 
  { "Type'" :: NullOrUndefined (String)
  , "Code'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }


-- | <p>Returned if the specified resource (such as a vault, upload ID, or job ID) doesn't exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Type'" :: NullOrUndefined (String)
  , "Code'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }


-- | <p>Contains information about the location in Amazon S3 where the select job results are stored.</p>
newtype S3Location = S3Location 
  { "BucketName" :: NullOrUndefined (String)
  , "Prefix" :: NullOrUndefined (String)
  , "Encryption" :: NullOrUndefined (Encryption)
  , "CannedACL" :: NullOrUndefined (CannedACL)
  , "AccessControlList" :: NullOrUndefined (AccessControlPolicyList)
  , "Tagging" :: NullOrUndefined (Hashmap')
  , "UserMetadata" :: NullOrUndefined (Hashmap')
  , "StorageClass" :: NullOrUndefined (StorageClass)
  }


-- | <p>Contains information about the parameters used for a select.</p>
newtype SelectParameters = SelectParameters 
  { "InputSerialization" :: NullOrUndefined (InputSerialization)
  , "ExpressionType" :: NullOrUndefined (ExpressionType)
  , "Expression" :: NullOrUndefined (String)
  , "OutputSerialization" :: NullOrUndefined (OutputSerialization)
  }


-- | <p>Returned if the service cannot complete the request.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Type'" :: NullOrUndefined (String)
  , "Code'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }


-- | <p>SetDataRetrievalPolicy input.</p>
newtype SetDataRetrievalPolicyInput = SetDataRetrievalPolicyInput 
  { "AccountId'" :: (String)
  , "Policy" :: NullOrUndefined (DataRetrievalPolicy)
  }


-- | <p>SetVaultAccessPolicy input.</p>
newtype SetVaultAccessPolicyInput = SetVaultAccessPolicyInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "Policy'" :: NullOrUndefined (VaultAccessPolicy)
  }


-- | <p>Provides options to configure notifications that will be sent when specific events happen to a vault.</p>
newtype SetVaultNotificationsInput = SetVaultNotificationsInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "VaultNotificationConfig'" :: NullOrUndefined (VaultNotificationConfig)
  }


newtype Size = Size Number


newtype StatusCode = StatusCode String


newtype StorageClass = StorageClass String


newtype Stream = Stream String


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array String)


newtype TagMap = TagMap (Map TagKey TagValue)


newtype TagValue = TagValue String


newtype Type = Type String


-- | <p>Provides options to add an archive to a vault.</p>
newtype UploadArchiveInput = UploadArchiveInput 
  { "VaultName'" :: (String)
  , "AccountId'" :: (String)
  , "ArchiveDescription'" :: NullOrUndefined (String)
  , "Checksum'" :: NullOrUndefined (String)
  , "Body'" :: NullOrUndefined (Stream)
  }


-- | <p>A list of in-progress multipart uploads for a vault.</p>
newtype UploadListElement = UploadListElement 
  { "MultipartUploadId" :: NullOrUndefined (String)
  , "VaultARN" :: NullOrUndefined (String)
  , "ArchiveDescription" :: NullOrUndefined (String)
  , "PartSizeInBytes" :: NullOrUndefined (Number)
  , "CreationDate" :: NullOrUndefined (String)
  }


-- | <p>Provides options to upload a part of an archive in a multipart upload operation.</p>
newtype UploadMultipartPartInput = UploadMultipartPartInput 
  { "AccountId'" :: (String)
  , "VaultName'" :: (String)
  , "UploadId'" :: (String)
  , "Checksum'" :: NullOrUndefined (String)
  , "Range'" :: NullOrUndefined (String)
  , "Body'" :: NullOrUndefined (Stream)
  }


-- | <p>Contains the Amazon Glacier response to your request.</p>
newtype UploadMultipartPartOutput = UploadMultipartPartOutput 
  { "Checksum'" :: NullOrUndefined (String)
  }


newtype UploadsList = UploadsList (Array UploadListElement)


-- | <p>Contains the vault access policy.</p>
newtype VaultAccessPolicy = VaultAccessPolicy 
  { "Policy" :: NullOrUndefined (String)
  }


newtype VaultList = VaultList (Array DescribeVaultOutput)


-- | <p>Contains the vault lock policy.</p>
newtype VaultLockPolicy = VaultLockPolicy 
  { "Policy" :: NullOrUndefined (String)
  }


-- | <p>Represents a vault's notification configuration.</p>
newtype VaultNotificationConfig = VaultNotificationConfig 
  { "SNSTopic" :: NullOrUndefined (String)
  , "Events" :: NullOrUndefined (NotificationEventList)
  }


newtype Hashmap' = Hashmap' (Map String String)


newtype Httpstatus' = Httpstatus' Int
