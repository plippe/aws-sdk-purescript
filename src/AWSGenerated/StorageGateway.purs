

-- | <fullname>AWS Storage Gateway Service</fullname> <p>AWS Storage Gateway is the service that connects an on-premises software appliance with cloud-based storage to provide seamless and secure integration between an organization's on-premises IT environment and AWS's storage infrastructure. The service enables you to securely upload data to the AWS cloud for cost effective backup and rapid disaster recovery.</p> <p>Use the following links to get started using the <i>AWS Storage Gateway Service API Reference</i>:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewayHTTPRequestsHeaders">AWS Storage Gateway Required Request Headers</a>: Describes the required headers that you must send with every POST request to AWS Storage Gateway.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewaySigningRequests">Signing Requests</a>: AWS Storage Gateway requires that you authenticate every request you send; this topic describes how sign such a request.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#APIErrorResponses">Error Responses</a>: Provides reference information about AWS Storage Gateway errors.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_Operations.html">Operations in AWS Storage Gateway</a>: Contains detailed descriptions of all AWS Storage Gateway operations, their request parameters, response elements, possible errors, and examples of requests and responses.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#sg_region">AWS Storage Gateway Regions and Endpoints:</a> Provides a list of each region and endpoints available for use with AWS Storage Gateway. </p> </li> </ul> <note> <p>AWS Storage Gateway resource IDs are in uppercase. When you use these resource IDs with the Amazon EC2 API, EC2 expects resource IDs in lowercase. You must change your resource ID to lowercase to use it with the EC2 API. For example, in Storage Gateway the ID for a volume might be <code>vol-AA22BB012345DAF670</code>. When you use this ID with the EC2 API, you must change it to <code>vol-aa22bb012345daf670</code>. Otherwise, the EC2 API might not behave as expected.</p> </note> <important> <p>IDs for Storage Gateway volumes and Amazon EBS snapshots created from gateway volumes are changing to a longer format. Starting in December 2016, all new volumes and snapshots will be created with a 17-character string. Starting in April 2016, you will be able to use these longer IDs so you can test your systems with the new format. For more information, see <a href="https://aws.amazon.com/ec2/faqs/#longer-ids">Longer EC2 and EBS Resource IDs</a>.</p> <p> For example, a volume Amazon Resource Name (ARN) with the longer volume ID format looks like the following:</p> <p> <code>arn:aws:storagegateway:us-west-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABBCCDDEEFFG</code>.</p> <p>A snapshot ID with the longer ID format looks like the following: <code>snap-78e226633445566ee</code>.</p> <p>For more information, see <a href="https://forums.aws.amazon.com/ann.jspa?annID=3557">Announcement: Heads-up – Longer AWS Storage Gateway volume and snapshot IDs coming in 2016</a>.</p> </important>
module AWS.StorageGateway where

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

serviceName = "StorageGateway" :: String


-- | <p>Activates the gateway you previously deployed on your host. For more information, see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedActivateGateway-common.html"> Activate the AWS Storage Gateway</a>. In the activation process, you specify information such as the region you want to use for storing snapshots or tapes, the time zone for scheduled snapshots the gateway snapshot schedule window, an activation key, and a name for your gateway. The activation process also associates your gateway with your account; for more information, see <a>UpdateGatewayInformation</a>.</p> <note> <p>You must turn on the gateway VM before you can activate your gateway.</p> </note>
activateGateway :: forall eff. ActivateGatewayInput -> Aff (exception :: EXCEPTION | eff) ActivateGatewayOutput
activateGateway = Request.request serviceName "activateGateway" 


-- | <p>Configures one or more gateway local disks as cache for a gateway. This operation is only supported in the cached volume, tape and file gateway type (see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/StorageGatewayConcepts.html">Storage Gateway Concepts</a>).</p> <p>In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add cache, and one or more disk IDs that you want to configure as cache.</p>
addCache :: forall eff. AddCacheInput -> Aff (exception :: EXCEPTION | eff) AddCacheOutput
addCache = Request.request serviceName "addCache" 


-- | <p>Adds one or more tags to the specified resource. You use tags to add metadata to resources, which you can use to categorize these resources. For example, you can categorize resources by purpose, owner, environment, or team. Each tag consists of a key and a value, which you define. You can add tags to the following AWS Storage Gateway resources:</p> <ul> <li> <p>Storage gateways of all types</p> </li> </ul> <ul> <li> <p>Storage Volumes</p> </li> </ul> <ul> <li> <p>Virtual Tapes</p> </li> </ul> <p>You can create a maximum of 10 tags for each resource. Virtual tapes and storage volumes that are recovered to a new gateway maintain their tags.</p>
addTagsToResource :: forall eff. AddTagsToResourceInput -> Aff (exception :: EXCEPTION | eff) AddTagsToResourceOutput
addTagsToResource = Request.request serviceName "addTagsToResource" 


-- | <p>Configures one or more gateway local disks as upload buffer for a specified gateway. This operation is supported for the stored volume, cached volume and tape gateway types.</p> <p>In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add upload buffer, and one or more disk IDs that you want to configure as upload buffer.</p>
addUploadBuffer :: forall eff. AddUploadBufferInput -> Aff (exception :: EXCEPTION | eff) AddUploadBufferOutput
addUploadBuffer = Request.request serviceName "addUploadBuffer" 


-- | <p>Configures one or more gateway local disks as working storage for a gateway. This operation is only supported in the stored volume gateway type. This operation is deprecated in cached volume API version 20120630. Use <a>AddUploadBuffer</a> instead.</p> <note> <p>Working storage is also referred to as upload buffer. You can also use the <a>AddUploadBuffer</a> operation to add upload buffer to a stored volume gateway.</p> </note> <p>In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add working storage, and one or more disk IDs that you want to configure as working storage.</p>
addWorkingStorage :: forall eff. AddWorkingStorageInput -> Aff (exception :: EXCEPTION | eff) AddWorkingStorageOutput
addWorkingStorage = Request.request serviceName "addWorkingStorage" 


-- | <p>Cancels archiving of a virtual tape to the virtual tape shelf (VTS) after the archiving process is initiated. This operation is only supported in the tape gateway type.</p>
cancelArchival :: forall eff. CancelArchivalInput -> Aff (exception :: EXCEPTION | eff) CancelArchivalOutput
cancelArchival = Request.request serviceName "cancelArchival" 


-- | <p>Cancels retrieval of a virtual tape from the virtual tape shelf (VTS) to a gateway after the retrieval process is initiated. The virtual tape is returned to the VTS. This operation is only supported in the tape gateway type.</p>
cancelRetrieval :: forall eff. CancelRetrievalInput -> Aff (exception :: EXCEPTION | eff) CancelRetrievalOutput
cancelRetrieval = Request.request serviceName "cancelRetrieval" 


-- | <p>Creates a cached volume on a specified cached volume gateway. This operation is only supported in the cached volume gateway type.</p> <note> <p>Cache storage must be allocated to the gateway before you can create a cached volume. Use the <a>AddCache</a> operation to add cache storage to a gateway. </p> </note> <p>In the request, you must specify the gateway, size of the volume in bytes, the iSCSI target name, an IP address on which to expose the target, and a unique client token. In response, the gateway creates the volume and returns information about it. This information includes the volume Amazon Resource Name (ARN), its size, and the iSCSI target ARN that initiators can use to connect to the volume target.</p> <p>Optionally, you can provide the ARN for an existing volume as the <code>SourceVolumeARN</code> for this cached volume, which creates an exact copy of the existing volume’s latest recovery point. The <code>VolumeSizeInBytes</code> value must be equal to or larger than the size of the copied volume, in bytes.</p>
createCachediSCSIVolume :: forall eff. CreateCachediSCSIVolumeInput -> Aff (exception :: EXCEPTION | eff) CreateCachediSCSIVolumeOutput
createCachediSCSIVolume = Request.request serviceName "createCachediSCSIVolume" 


-- | <p>Creates a file share on an existing file gateway. In Storage Gateway, a file share is a file system mount point backed by Amazon S3 cloud storage. Storage Gateway exposes file shares using a Network File System (NFS) interface. This operation is only supported in the file gateway type.</p> <important> <p>File gateway requires AWS Security Token Service (AWS STS) to be activated to enable you create a file share. Make sure AWS STS is activated in the region you are creating your file gateway in. If AWS STS is not activated in the region, activate it. For information about how to activate AWS STS, see Activating and Deactivating AWS STS in an AWS Region in the AWS Identity and Access Management User Guide. </p> <p>File gateway does not support creating hard or symbolic links on a file share.</p> </important>
createNFSFileShare :: forall eff. CreateNFSFileShareInput -> Aff (exception :: EXCEPTION | eff) CreateNFSFileShareOutput
createNFSFileShare = Request.request serviceName "createNFSFileShare" 


-- | <p>Initiates a snapshot of a volume.</p> <p>AWS Storage Gateway provides the ability to back up point-in-time snapshots of your data to Amazon Simple Storage (S3) for durable off-site recovery, as well as import the data to an Amazon Elastic Block Store (EBS) volume in Amazon Elastic Compute Cloud (EC2). You can take snapshots of your gateway volume on a scheduled or ad-hoc basis. This API enables you to take ad-hoc snapshot. For more information, see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#SchedulingSnapshot">Editing a Snapshot Schedule</a>.</p> <p>In the CreateSnapshot request you identify the volume by providing its Amazon Resource Name (ARN). You must also provide description for the snapshot. When AWS Storage Gateway takes the snapshot of specified volume, the snapshot and description appears in the AWS Storage Gateway Console. In response, AWS Storage Gateway returns you a snapshot ID. You can use this snapshot ID to check the snapshot progress or later use it when you want to create a volume from a snapshot. This operation is only supported in stored and cached volume gateway type.</p> <note> <p>To list or delete a snapshot, you must use the Amazon EC2 API. For more information, see DescribeSnapshots or DeleteSnapshot in the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Operations.html">EC2 API reference</a>.</p> </note> <important> <p>Volume and snapshot IDs are changing to a longer length ID format. For more information, see the important note on the <a href="http://docs.aws.amazon.com/storagegateway/latest/APIReference/Welcome.html">Welcome</a> page.</p> </important>
createSnapshot :: forall eff. CreateSnapshotInput -> Aff (exception :: EXCEPTION | eff) CreateSnapshotOutput
createSnapshot = Request.request serviceName "createSnapshot" 


-- | <p>Initiates a snapshot of a gateway from a volume recovery point. This operation is only supported in the cached volume gateway type.</p> <p>A volume recovery point is a point in time at which all data of the volume is consistent and from which you can create a snapshot. To get a list of volume recovery point for cached volume gateway, use <a>ListVolumeRecoveryPoints</a>.</p> <p>In the <code>CreateSnapshotFromVolumeRecoveryPoint</code> request, you identify the volume by providing its Amazon Resource Name (ARN). You must also provide a description for the snapshot. When the gateway takes a snapshot of the specified volume, the snapshot and its description appear in the AWS Storage Gateway console. In response, the gateway returns you a snapshot ID. You can use this snapshot ID to check the snapshot progress or later use it when you want to create a volume from a snapshot.</p> <note> <p>To list or delete a snapshot, you must use the Amazon EC2 API. For more information, in <i>Amazon Elastic Compute Cloud API Reference</i>.</p> </note>
createSnapshotFromVolumeRecoveryPoint :: forall eff. CreateSnapshotFromVolumeRecoveryPointInput -> Aff (exception :: EXCEPTION | eff) CreateSnapshotFromVolumeRecoveryPointOutput
createSnapshotFromVolumeRecoveryPoint = Request.request serviceName "createSnapshotFromVolumeRecoveryPoint" 


-- | <p>Creates a volume on a specified gateway. This operation is only supported in the stored volume gateway type.</p> <p>The size of the volume to create is inferred from the disk size. You can choose to preserve existing data on the disk, create volume from an existing snapshot, or create an empty volume. If you choose to create an empty gateway volume, then any existing data on the disk is erased.</p> <p>In the request you must specify the gateway and the disk information on which you are creating the volume. In response, the gateway creates the volume and returns volume information such as the volume Amazon Resource Name (ARN), its size, and the iSCSI target ARN that initiators can use to connect to the volume target.</p>
createStorediSCSIVolume :: forall eff. CreateStorediSCSIVolumeInput -> Aff (exception :: EXCEPTION | eff) CreateStorediSCSIVolumeOutput
createStorediSCSIVolume = Request.request serviceName "createStorediSCSIVolume" 


-- | <p>Creates a virtual tape by using your own barcode. You write data to the virtual tape and then archive the tape. A barcode is unique and can not be reused if it has already been used on a tape . This applies to barcodes used on deleted tapes. This operation is only supported in the tape gateway type.</p> <note> <p>Cache storage must be allocated to the gateway before you can create a virtual tape. Use the <a>AddCache</a> operation to add cache storage to a gateway.</p> </note>
createTapeWithBarcode :: forall eff. CreateTapeWithBarcodeInput -> Aff (exception :: EXCEPTION | eff) CreateTapeWithBarcodeOutput
createTapeWithBarcode = Request.request serviceName "createTapeWithBarcode" 


-- | <p>Creates one or more virtual tapes. You write data to the virtual tapes and then archive the tapes. This operation is only supported in the tape gateway type.</p> <note> <p>Cache storage must be allocated to the gateway before you can create virtual tapes. Use the <a>AddCache</a> operation to add cache storage to a gateway. </p> </note>
createTapes :: forall eff. CreateTapesInput -> Aff (exception :: EXCEPTION | eff) CreateTapesOutput
createTapes = Request.request serviceName "createTapes" 


-- | <p>Deletes the bandwidth rate limits of a gateway. You can delete either the upload and download bandwidth rate limit, or you can delete both. If you delete only one of the limits, the other limit remains unchanged. To specify which gateway to work with, use the Amazon Resource Name (ARN) of the gateway in your request.</p>
deleteBandwidthRateLimit :: forall eff. DeleteBandwidthRateLimitInput -> Aff (exception :: EXCEPTION | eff) DeleteBandwidthRateLimitOutput
deleteBandwidthRateLimit = Request.request serviceName "deleteBandwidthRateLimit" 


-- | <p>Deletes Challenge-Handshake Authentication Protocol (CHAP) credentials for a specified iSCSI target and initiator pair.</p>
deleteChapCredentials :: forall eff. DeleteChapCredentialsInput -> Aff (exception :: EXCEPTION | eff) DeleteChapCredentialsOutput
deleteChapCredentials = Request.request serviceName "deleteChapCredentials" 


-- | <p>Deletes a file share from a file gateway. This operation is only supported in the file gateway type.</p>
deleteFileShare :: forall eff. DeleteFileShareInput -> Aff (exception :: EXCEPTION | eff) DeleteFileShareOutput
deleteFileShare = Request.request serviceName "deleteFileShare" 


-- | <p>Deletes a gateway. To specify which gateway to delete, use the Amazon Resource Name (ARN) of the gateway in your request. The operation deletes the gateway; however, it does not delete the gateway virtual machine (VM) from your host computer.</p> <p>After you delete a gateway, you cannot reactivate it. Completed snapshots of the gateway volumes are not deleted upon deleting the gateway, however, pending snapshots will not complete. After you delete a gateway, your next step is to remove it from your environment.</p> <important> <p>You no longer pay software charges after the gateway is deleted; however, your existing Amazon EBS snapshots persist and you will continue to be billed for these snapshots. You can choose to remove all remaining Amazon EBS snapshots by canceling your Amazon EC2 subscription.  If you prefer not to cancel your Amazon EC2 subscription, you can delete your snapshots using the Amazon EC2 console. For more information, see the <a href="http://aws.amazon.com/storagegateway"> AWS Storage Gateway Detail Page</a>. </p> </important>
deleteGateway :: forall eff. DeleteGatewayInput -> Aff (exception :: EXCEPTION | eff) DeleteGatewayOutput
deleteGateway = Request.request serviceName "deleteGateway" 


-- | <p>Deletes a snapshot of a volume.</p> <p>You can take snapshots of your gateway volumes on a scheduled or ad hoc basis. This API action enables you to delete a snapshot schedule for a volume. For more information, see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/WorkingWithSnapshots.html">Working with Snapshots</a>. In the <code>DeleteSnapshotSchedule</code> request, you identify the volume by providing its Amazon Resource Name (ARN). This operation is only supported in stored and cached volume gateway types.</p> <note> <p>To list or delete a snapshot, you must use the Amazon EC2 API. in <i>Amazon Elastic Compute Cloud API Reference</i>.</p> </note>
deleteSnapshotSchedule :: forall eff. DeleteSnapshotScheduleInput -> Aff (exception :: EXCEPTION | eff) DeleteSnapshotScheduleOutput
deleteSnapshotSchedule = Request.request serviceName "deleteSnapshotSchedule" 


-- | <p>Deletes the specified virtual tape. This operation is only supported in the tape gateway type.</p>
deleteTape :: forall eff. DeleteTapeInput -> Aff (exception :: EXCEPTION | eff) DeleteTapeOutput
deleteTape = Request.request serviceName "deleteTape" 


-- | <p>Deletes the specified virtual tape from the virtual tape shelf (VTS). This operation is only supported in the tape gateway type.</p>
deleteTapeArchive :: forall eff. DeleteTapeArchiveInput -> Aff (exception :: EXCEPTION | eff) DeleteTapeArchiveOutput
deleteTapeArchive = Request.request serviceName "deleteTapeArchive" 


-- | <p>Deletes the specified storage volume that you previously created using the <a>CreateCachediSCSIVolume</a> or <a>CreateStorediSCSIVolume</a> API. This operation is only supported in the cached volume and stored volume types. For stored volume gateways, the local disk that was configured as the storage volume is not deleted. You can reuse the local disk to create another storage volume. </p> <p>Before you delete a volume, make sure there are no iSCSI connections to the volume you are deleting. You should also make sure there is no snapshot in progress. You can use the Amazon Elastic Compute Cloud (Amazon EC2) API to query snapshots on the volume you are deleting and check the snapshot status. For more information, go to <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html">DescribeSnapshots</a> in the <i>Amazon Elastic Compute Cloud API Reference</i>.</p> <p>In the request, you must provide the Amazon Resource Name (ARN) of the storage volume you want to delete.</p>
deleteVolume :: forall eff. DeleteVolumeInput -> Aff (exception :: EXCEPTION | eff) DeleteVolumeOutput
deleteVolume = Request.request serviceName "deleteVolume" 


-- | <p>Returns the bandwidth rate limits of a gateway. By default, these limits are not set, which means no bandwidth rate limiting is in effect.</p> <p>This operation only returns a value for a bandwidth rate limit only if the limit is set. If no limits are set for the gateway, then this operation returns only the gateway ARN in the response body. To specify which gateway to describe, use the Amazon Resource Name (ARN) of the gateway in your request.</p>
describeBandwidthRateLimit :: forall eff. DescribeBandwidthRateLimitInput -> Aff (exception :: EXCEPTION | eff) DescribeBandwidthRateLimitOutput
describeBandwidthRateLimit = Request.request serviceName "describeBandwidthRateLimit" 


-- | <p>Returns information about the cache of a gateway. This operation is only supported in the cached volume, tape and file gateway types.</p> <p>The response includes disk IDs that are configured as cache, and it includes the amount of cache allocated and used.</p>
describeCache :: forall eff. DescribeCacheInput -> Aff (exception :: EXCEPTION | eff) DescribeCacheOutput
describeCache = Request.request serviceName "describeCache" 


-- | <p>Returns a description of the gateway volumes specified in the request. This operation is only supported in the cached volume gateway types.</p> <p>The list of gateway volumes in the request must be from one gateway. In the response Amazon Storage Gateway returns volume information sorted by volume Amazon Resource Name (ARN).</p>
describeCachediSCSIVolumes :: forall eff. DescribeCachediSCSIVolumesInput -> Aff (exception :: EXCEPTION | eff) DescribeCachediSCSIVolumesOutput
describeCachediSCSIVolumes = Request.request serviceName "describeCachediSCSIVolumes" 


-- | <p>Returns an array of Challenge-Handshake Authentication Protocol (CHAP) credentials information for a specified iSCSI target, one for each target-initiator pair.</p>
describeChapCredentials :: forall eff. DescribeChapCredentialsInput -> Aff (exception :: EXCEPTION | eff) DescribeChapCredentialsOutput
describeChapCredentials = Request.request serviceName "describeChapCredentials" 


-- | <p>Returns metadata about a gateway such as its name, network interfaces, configured time zone, and the state (whether the gateway is running or not). To specify which gateway to describe, use the Amazon Resource Name (ARN) of the gateway in your request.</p>
describeGatewayInformation :: forall eff. DescribeGatewayInformationInput -> Aff (exception :: EXCEPTION | eff) DescribeGatewayInformationOutput
describeGatewayInformation = Request.request serviceName "describeGatewayInformation" 


-- | <p>Returns your gateway's weekly maintenance start time including the day and time of the week. Note that values are in terms of the gateway's time zone.</p>
describeMaintenanceStartTime :: forall eff. DescribeMaintenanceStartTimeInput -> Aff (exception :: EXCEPTION | eff) DescribeMaintenanceStartTimeOutput
describeMaintenanceStartTime = Request.request serviceName "describeMaintenanceStartTime" 


-- | <p>Gets a description for one or more file shares from a file gateway. This operation is only supported in the file gateway type.</p>
describeNFSFileShares :: forall eff. DescribeNFSFileSharesInput -> Aff (exception :: EXCEPTION | eff) DescribeNFSFileSharesOutput
describeNFSFileShares = Request.request serviceName "describeNFSFileShares" 


-- | <p>Describes the snapshot schedule for the specified gateway volume. The snapshot schedule information includes intervals at which snapshots are automatically initiated on the volume. This operation is only supported in the cached volume and stored volume types.</p>
describeSnapshotSchedule :: forall eff. DescribeSnapshotScheduleInput -> Aff (exception :: EXCEPTION | eff) DescribeSnapshotScheduleOutput
describeSnapshotSchedule = Request.request serviceName "describeSnapshotSchedule" 


-- | <p>Returns the description of the gateway volumes specified in the request. The list of gateway volumes in the request must be from one gateway. In the response Amazon Storage Gateway returns volume information sorted by volume ARNs. This operation is only supported in stored volume gateway type.</p>
describeStorediSCSIVolumes :: forall eff. DescribeStorediSCSIVolumesInput -> Aff (exception :: EXCEPTION | eff) DescribeStorediSCSIVolumesOutput
describeStorediSCSIVolumes = Request.request serviceName "describeStorediSCSIVolumes" 


-- | <p>Returns a description of specified virtual tapes in the virtual tape shelf (VTS). This operation is only supported in the tape gateway type.</p> <p>If a specific <code>TapeARN</code> is not specified, AWS Storage Gateway returns a description of all virtual tapes found in the VTS associated with your account.</p>
describeTapeArchives :: forall eff. DescribeTapeArchivesInput -> Aff (exception :: EXCEPTION | eff) DescribeTapeArchivesOutput
describeTapeArchives = Request.request serviceName "describeTapeArchives" 


-- | <p>Returns a list of virtual tape recovery points that are available for the specified tape gateway.</p> <p>A recovery point is a point-in-time view of a virtual tape at which all the data on the virtual tape is consistent. If your gateway crashes, virtual tapes that have recovery points can be recovered to a new gateway. This operation is only supported in the tape gateway type.</p>
describeTapeRecoveryPoints :: forall eff. DescribeTapeRecoveryPointsInput -> Aff (exception :: EXCEPTION | eff) DescribeTapeRecoveryPointsOutput
describeTapeRecoveryPoints = Request.request serviceName "describeTapeRecoveryPoints" 


-- | <p>Returns a description of the specified Amazon Resource Name (ARN) of virtual tapes. If a <code>TapeARN</code> is not specified, returns a description of all virtual tapes associated with the specified gateway. This operation is only supported in the tape gateway type.</p>
describeTapes :: forall eff. DescribeTapesInput -> Aff (exception :: EXCEPTION | eff) DescribeTapesOutput
describeTapes = Request.request serviceName "describeTapes" 


-- | <p>Returns information about the upload buffer of a gateway. This operation is supported for the stored volume, cached volume and tape gateway types.</p> <p>The response includes disk IDs that are configured as upload buffer space, and it includes the amount of upload buffer space allocated and used.</p>
describeUploadBuffer :: forall eff. DescribeUploadBufferInput -> Aff (exception :: EXCEPTION | eff) DescribeUploadBufferOutput
describeUploadBuffer = Request.request serviceName "describeUploadBuffer" 


-- | <p>Returns a description of virtual tape library (VTL) devices for the specified tape gateway. In the response, AWS Storage Gateway returns VTL device information.</p> <p>This operation is only supported in the tape gateway type.</p>
describeVTLDevices :: forall eff. DescribeVTLDevicesInput -> Aff (exception :: EXCEPTION | eff) DescribeVTLDevicesOutput
describeVTLDevices = Request.request serviceName "describeVTLDevices" 


-- | <p>Returns information about the working storage of a gateway. This operation is only supported in the stored volumes gateway type. This operation is deprecated in cached volumes API version (20120630). Use DescribeUploadBuffer instead.</p> <note> <p>Working storage is also referred to as upload buffer. You can also use the DescribeUploadBuffer operation to add upload buffer to a stored volume gateway.</p> </note> <p>The response includes disk IDs that are configured as working storage, and it includes the amount of working storage allocated and used.</p>
describeWorkingStorage :: forall eff. DescribeWorkingStorageInput -> Aff (exception :: EXCEPTION | eff) DescribeWorkingStorageOutput
describeWorkingStorage = Request.request serviceName "describeWorkingStorage" 


-- | <p>Disables a tape gateway when the gateway is no longer functioning. For example, if your gateway VM is damaged, you can disable the gateway so you can recover virtual tapes.</p> <p>Use this operation for a tape gateway that is not reachable or not functioning. This operation is only supported in the tape gateway type.</p> <important> <p>Once a gateway is disabled it cannot be enabled.</p> </important>
disableGateway :: forall eff. DisableGatewayInput -> Aff (exception :: EXCEPTION | eff) DisableGatewayOutput
disableGateway = Request.request serviceName "disableGateway" 


-- | <p>Gets a list of the file shares for a specific file gateway, or the list of file shares that belong to the calling user account. This operation is only supported in the file gateway type.</p>
listFileShares :: forall eff. ListFileSharesInput -> Aff (exception :: EXCEPTION | eff) ListFileSharesOutput
listFileShares = Request.request serviceName "listFileShares" 


-- | <p>Lists gateways owned by an AWS account in a region specified in the request. The returned list is ordered by gateway Amazon Resource Name (ARN).</p> <p>By default, the operation returns a maximum of 100 gateways. This operation supports pagination that allows you to optionally reduce the number of gateways returned in a response.</p> <p>If you have more gateways than are returned in a response (that is, the response returns only a truncated list of your gateways), the response contains a marker that you can specify in your next request to fetch the next page of gateways.</p>
listGateways :: forall eff. ListGatewaysInput -> Aff (exception :: EXCEPTION | eff) ListGatewaysOutput
listGateways = Request.request serviceName "listGateways" 


-- | <p>Returns a list of the gateway's local disks. To specify which gateway to describe, you use the Amazon Resource Name (ARN) of the gateway in the body of the request.</p> <p>The request returns a list of all disks, specifying which are configured as working storage, cache storage, or stored volume or not configured at all. The response includes a <code>DiskStatus</code> field. This field can have a value of present (the disk is available to use), missing (the disk is no longer connected to the gateway), or mismatch (the disk node is occupied by a disk that has incorrect metadata or the disk content is corrupted).</p>
listLocalDisks :: forall eff. ListLocalDisksInput -> Aff (exception :: EXCEPTION | eff) ListLocalDisksOutput
listLocalDisks = Request.request serviceName "listLocalDisks" 


-- | <p>Lists the tags that have been added to the specified resource. This operation is only supported in the cached volume, stored volume and tape gateway type.</p>
listTagsForResource :: forall eff. ListTagsForResourceInput -> Aff (exception :: EXCEPTION | eff) ListTagsForResourceOutput
listTagsForResource = Request.request serviceName "listTagsForResource" 


-- | <p>Lists virtual tapes in your virtual tape library (VTL) and your virtual tape shelf (VTS). You specify the tapes to list by specifying one or more tape Amazon Resource Names (ARNs). If you don't specify a tape ARN, the operation lists all virtual tapes in both your VTL and VTS.</p> <p>This operation supports pagination. By default, the operation returns a maximum of up to 100 tapes. You can optionally specify the <code>Limit</code> parameter in the body to limit the number of tapes in the response. If the number of tapes returned in the response is truncated, the response includes a <code>Marker</code> element that you can use in your subsequent request to retrieve the next set of tapes. This operation is only supported in the tape gateway type.</p>
listTapes :: forall eff. ListTapesInput -> Aff (exception :: EXCEPTION | eff) ListTapesOutput
listTapes = Request.request serviceName "listTapes" 


-- | <p>Lists iSCSI initiators that are connected to a volume. You can use this operation to determine whether a volume is being used or not. This operation is only supported in the cached volume and stored volume gateway types.</p>
listVolumeInitiators :: forall eff. ListVolumeInitiatorsInput -> Aff (exception :: EXCEPTION | eff) ListVolumeInitiatorsOutput
listVolumeInitiators = Request.request serviceName "listVolumeInitiators" 


-- | <p>Lists the recovery points for a specified gateway. This operation is only supported in the cached volume gateway type.</p> <p>Each cache volume has one recovery point. A volume recovery point is a point in time at which all data of the volume is consistent and from which you can create a snapshot or clone a new cached volume from a source volume. To create a snapshot from a volume recovery point use the <a>CreateSnapshotFromVolumeRecoveryPoint</a> operation.</p>
listVolumeRecoveryPoints :: forall eff. ListVolumeRecoveryPointsInput -> Aff (exception :: EXCEPTION | eff) ListVolumeRecoveryPointsOutput
listVolumeRecoveryPoints = Request.request serviceName "listVolumeRecoveryPoints" 


-- | <p>Lists the iSCSI stored volumes of a gateway. Results are sorted by volume ARN. The response includes only the volume ARNs. If you want additional volume information, use the <a>DescribeStorediSCSIVolumes</a> or the <a>DescribeCachediSCSIVolumes</a> API.</p> <p>The operation supports pagination. By default, the operation returns a maximum of up to 100 volumes. You can optionally specify the <code>Limit</code> field in the body to limit the number of volumes in the response. If the number of volumes returned in the response is truncated, the response includes a Marker field. You can use this Marker value in your subsequent request to retrieve the next set of volumes. This operation is only supported in the cached volume and stored volume gateway types.</p>
listVolumes :: forall eff. ListVolumesInput -> Aff (exception :: EXCEPTION | eff) ListVolumesOutput
listVolumes = Request.request serviceName "listVolumes" 


-- | <p>Sends you notification when all file data written to the NFS file share has been uploaded to Amazon S3.</p> <p>AWS Storage Gateway can send a notification through Amazon CloudWatch Events when all files written to your file share up to that point in time have been uploaded to Amazon S3. These files include files written to the NFS file share up to the time that you make a request for notification. When the upload is done, Storage Gateway sends you notification through an Amazon CloudWatch event. You can configure CloudWatch Events to sent the notification through event targets such as email, SNS or a Lambda function. text or Lambda functions. This operation is only supported in the file gateway type.</p>
notifyWhenUploaded :: forall eff. NotifyWhenUploadedInput -> Aff (exception :: EXCEPTION | eff) NotifyWhenUploadedOutput
notifyWhenUploaded = Request.request serviceName "notifyWhenUploaded" 


-- | <p>Refreshes the cache for the specified file share. This operation finds objects in the Amazon S3 bucket that were added, removed or replaced since the gateway last listed the bucket's contents and cached the results. This operation is only supported in the file gateway type.</p>
refreshCache :: forall eff. RefreshCacheInput -> Aff (exception :: EXCEPTION | eff) RefreshCacheOutput
refreshCache = Request.request serviceName "refreshCache" 


-- | <p>Removes one or more tags from the specified resource. This operation is only supported in the cached volume, stored volume and tape gateway types.</p>
removeTagsFromResource :: forall eff. RemoveTagsFromResourceInput -> Aff (exception :: EXCEPTION | eff) RemoveTagsFromResourceOutput
removeTagsFromResource = Request.request serviceName "removeTagsFromResource" 


-- | <p>Resets all cache disks that have encountered a error and makes the disks available for reconfiguration as cache storage. If your cache disk encounters a error, the gateway prevents read and write operations on virtual tapes in the gateway. For example, an error can occur when a disk is corrupted or removed from the gateway. When a cache is reset, the gateway loses its cache storage. At this point you can reconfigure the disks as cache disks. This operation is only supported in the cached volume, tape and file gateway types.</p> <important> <p>If the cache disk you are resetting contains data that has not been uploaded to Amazon S3 yet, that data can be lost. After you reset cache disks, there will be no configured cache disks left in the gateway, so you must configure at least one new cache disk for your gateway to function properly.</p> </important>
resetCache :: forall eff. ResetCacheInput -> Aff (exception :: EXCEPTION | eff) ResetCacheOutput
resetCache = Request.request serviceName "resetCache" 


-- | <p>Retrieves an archived virtual tape from the virtual tape shelf (VTS) to a tape gateway. Virtual tapes archived in the VTS are not associated with any gateway. However after a tape is retrieved, it is associated with a gateway, even though it is also listed in the VTS, that is, archive. This operation is only supported in the tape gateway type.</p> <p>Once a tape is successfully retrieved to a gateway, it cannot be retrieved again to another gateway. You must archive the tape again before you can retrieve it to another gateway. This operation is only supported in the tape gateway type.</p>
retrieveTapeArchive :: forall eff. RetrieveTapeArchiveInput -> Aff (exception :: EXCEPTION | eff) RetrieveTapeArchiveOutput
retrieveTapeArchive = Request.request serviceName "retrieveTapeArchive" 


-- | <p>Retrieves the recovery point for the specified virtual tape. This operation is only supported in the tape gateway type.</p> <p>A recovery point is a point in time view of a virtual tape at which all the data on the tape is consistent. If your gateway crashes, virtual tapes that have recovery points can be recovered to a new gateway.</p> <note> <p>The virtual tape can be retrieved to only one gateway. The retrieved tape is read-only. The virtual tape can be retrieved to only a tape gateway. There is no charge for retrieving recovery points.</p> </note>
retrieveTapeRecoveryPoint :: forall eff. RetrieveTapeRecoveryPointInput -> Aff (exception :: EXCEPTION | eff) RetrieveTapeRecoveryPointOutput
retrieveTapeRecoveryPoint = Request.request serviceName "retrieveTapeRecoveryPoint" 


-- | <p>Sets the password for your VM local console. When you log in to the local console for the first time, you log in to the VM with the default credentials. We recommend that you set a new password. You don't need to know the default password to set a new password.</p>
setLocalConsolePassword :: forall eff. SetLocalConsolePasswordInput -> Aff (exception :: EXCEPTION | eff) SetLocalConsolePasswordOutput
setLocalConsolePassword = Request.request serviceName "setLocalConsolePassword" 


-- | <p>Shuts down a gateway. To specify which gateway to shut down, use the Amazon Resource Name (ARN) of the gateway in the body of your request.</p> <p>The operation shuts down the gateway service component running in the gateway's virtual machine (VM) and not the host VM.</p> <note> <p>If you want to shut down the VM, it is recommended that you first shut down the gateway component in the VM to avoid unpredictable conditions.</p> </note> <p>After the gateway is shutdown, you cannot call any other API except <a>StartGateway</a>, <a>DescribeGatewayInformation</a>, and <a>ListGateways</a>. For more information, see <a>ActivateGateway</a>. Your applications cannot read from or write to the gateway's storage volumes, and there are no snapshots taken.</p> <note> <p>When you make a shutdown request, you will get a <code>200 OK</code> success response immediately. However, it might take some time for the gateway to shut down. You can call the <a>DescribeGatewayInformation</a> API to check the status. For more information, see <a>ActivateGateway</a>.</p> </note> <p>If do not intend to use the gateway again, you must delete the gateway (using <a>DeleteGateway</a>) to no longer pay software charges associated with the gateway.</p>
shutdownGateway :: forall eff. ShutdownGatewayInput -> Aff (exception :: EXCEPTION | eff) ShutdownGatewayOutput
shutdownGateway = Request.request serviceName "shutdownGateway" 


-- | <p>Starts a gateway that you previously shut down (see <a>ShutdownGateway</a>). After the gateway starts, you can then make other API calls, your applications can read from or write to the gateway's storage volumes and you will be able to take snapshot backups.</p> <note> <p>When you make a request, you will get a 200 OK success response immediately. However, it might take some time for the gateway to be ready. You should call <a>DescribeGatewayInformation</a> and check the status before making any additional API calls. For more information, see <a>ActivateGateway</a>.</p> </note> <p>To specify which gateway to start, use the Amazon Resource Name (ARN) of the gateway in your request.</p>
startGateway :: forall eff. StartGatewayInput -> Aff (exception :: EXCEPTION | eff) StartGatewayOutput
startGateway = Request.request serviceName "startGateway" 


-- | <p>Updates the bandwidth rate limits of a gateway. You can update both the upload and download bandwidth rate limit or specify only one of the two. If you don't set a bandwidth rate limit, the existing rate limit remains.</p> <p>By default, a gateway's bandwidth rate limits are not set. If you don't set any limit, the gateway does not have any limitations on its bandwidth usage and could potentially use the maximum available bandwidth.</p> <p>To specify which gateway to update, use the Amazon Resource Name (ARN) of the gateway in your request.</p>
updateBandwidthRateLimit :: forall eff. UpdateBandwidthRateLimitInput -> Aff (exception :: EXCEPTION | eff) UpdateBandwidthRateLimitOutput
updateBandwidthRateLimit = Request.request serviceName "updateBandwidthRateLimit" 


-- | <p>Updates the Challenge-Handshake Authentication Protocol (CHAP) credentials for a specified iSCSI target. By default, a gateway does not have CHAP enabled; however, for added security, you might use it.</p> <important> <p>When you update CHAP credentials, all existing connections on the target are closed and initiators must reconnect with the new credentials.</p> </important>
updateChapCredentials :: forall eff. UpdateChapCredentialsInput -> Aff (exception :: EXCEPTION | eff) UpdateChapCredentialsOutput
updateChapCredentials = Request.request serviceName "updateChapCredentials" 


-- | <p>Updates a gateway's metadata, which includes the gateway's name and time zone. To specify which gateway to update, use the Amazon Resource Name (ARN) of the gateway in your request.</p> <note> <p>For Gateways activated after September 2, 2015, the gateway's ARN contains the gateway ID rather than the gateway name. However, changing the name of the gateway has no effect on the gateway's ARN.</p> </note>
updateGatewayInformation :: forall eff. UpdateGatewayInformationInput -> Aff (exception :: EXCEPTION | eff) UpdateGatewayInformationOutput
updateGatewayInformation = Request.request serviceName "updateGatewayInformation" 


-- | <p>Updates the gateway virtual machine (VM) software. The request immediately triggers the software update.</p> <note> <p>When you make this request, you get a <code>200 OK</code> success response immediately. However, it might take some time for the update to complete. You can call <a>DescribeGatewayInformation</a> to verify the gateway is in the <code>STATE_RUNNING</code> state.</p> </note> <important> <p>A software update forces a system restart of your gateway. You can minimize the chance of any disruption to your applications by increasing your iSCSI Initiators' timeouts. For more information about increasing iSCSI Initiator timeouts for Windows and Linux, see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorWindowsClient.html#CustomizeWindowsiSCSISettings">Customizing Your Windows iSCSI Settings</a> and <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorRedHatClient.html#CustomizeLinuxiSCSISettings">Customizing Your Linux iSCSI Settings</a>, respectively.</p> </important>
updateGatewaySoftwareNow :: forall eff. UpdateGatewaySoftwareNowInput -> Aff (exception :: EXCEPTION | eff) UpdateGatewaySoftwareNowOutput
updateGatewaySoftwareNow = Request.request serviceName "updateGatewaySoftwareNow" 


-- | <p>Updates a gateway's weekly maintenance start time information, including day and time of the week. The maintenance time is the time in your gateway's time zone.</p>
updateMaintenanceStartTime :: forall eff. UpdateMaintenanceStartTimeInput -> Aff (exception :: EXCEPTION | eff) UpdateMaintenanceStartTimeOutput
updateMaintenanceStartTime = Request.request serviceName "updateMaintenanceStartTime" 


-- | <p>Updates a file share. This operation is only supported in the file gateway type.</p> <note> <p>To leave a file share field unchanged, set the corresponding input field to null.</p> </note> <p>Updates the following file share setting:</p> <ul> <li> <p>Default storage class for your S3 bucket</p> </li> <li> <p>Metadata defaults for your S3 bucket</p> </li> <li> <p>Allowed NFS clients for your file share</p> </li> <li> <p>Squash settings</p> </li> <li> <p>Write status of your file share</p> </li> </ul> <note> <p>To leave a file share field unchanged, set the corresponding input field to null. This operation is only supported in file gateways.</p> </note>
updateNFSFileShare :: forall eff. UpdateNFSFileShareInput -> Aff (exception :: EXCEPTION | eff) UpdateNFSFileShareOutput
updateNFSFileShare = Request.request serviceName "updateNFSFileShare" 


-- | <p>Updates a snapshot schedule configured for a gateway volume. This operation is only supported in the cached volume and stored volume gateway types.</p> <p>The default snapshot schedule for volume is once every 24 hours, starting at the creation time of the volume. You can use this API to change the snapshot schedule configured for the volume.</p> <p>In the request you must identify the gateway volume whose snapshot schedule you want to update, and the schedule information, including when you want the snapshot to begin on a day and the frequency (in hours) of snapshots.</p>
updateSnapshotSchedule :: forall eff. UpdateSnapshotScheduleInput -> Aff (exception :: EXCEPTION | eff) UpdateSnapshotScheduleOutput
updateSnapshotSchedule = Request.request serviceName "updateSnapshotSchedule" 


-- | <p>Updates the type of medium changer in a tape gateway. When you activate a tape gateway, you select a medium changer type for the tape gateway. This operation enables you to select a different type of medium changer after a tape gateway is activated. This operation is only supported in the tape gateway type.</p>
updateVTLDeviceType :: forall eff. UpdateVTLDeviceTypeInput -> Aff (exception :: EXCEPTION | eff) UpdateVTLDeviceTypeOutput
updateVTLDeviceType = Request.request serviceName "updateVTLDeviceType" 


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>ActivateGatewayInput$ActivationKey</a> </p> </li> <li> <p> <a>ActivateGatewayInput$GatewayName</a> </p> </li> <li> <p> <a>ActivateGatewayInput$GatewayRegion</a> </p> </li> <li> <p> <a>ActivateGatewayInput$GatewayTimezone</a> </p> </li> <li> <p> <a>ActivateGatewayInput$GatewayType</a> </p> </li> <li> <p> <a>ActivateGatewayInput$TapeDriveType</a> </p> </li> <li> <p> <a>ActivateGatewayInput$MediumChangerType</a> </p> </li> </ul>
newtype ActivateGatewayInput = ActivateGatewayInput 
  { "ActivationKey" :: (ActivationKey)
  , "GatewayName" :: (GatewayName)
  , "GatewayTimezone" :: (GatewayTimezone)
  , "GatewayRegion" :: (RegionId)
  , "GatewayType" :: NullOrUndefined.NullOrUndefined (GatewayType)
  , "TapeDriveType" :: NullOrUndefined.NullOrUndefined (TapeDriveType)
  , "MediumChangerType" :: NullOrUndefined.NullOrUndefined (MediumChangerType)
  }
derive instance newtypeActivateGatewayInput :: Newtype ActivateGatewayInput _
derive instance repGenericActivateGatewayInput :: Generic ActivateGatewayInput _
instance showActivateGatewayInput :: Show ActivateGatewayInput where
  show = genericShow
instance decodeActivateGatewayInput :: Decode ActivateGatewayInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivateGatewayInput :: Encode ActivateGatewayInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>AWS Storage Gateway returns the Amazon Resource Name (ARN) of the activated gateway. It is a string made of information such as your account, gateway name, and region. This ARN is used to reference the gateway in other API operations as well as resource-based authorization.</p> <note> <p>For gateways activated prior to September 02, 2015, the gateway ARN contains the gateway name rather than the gateway ID. Changing the name of the gateway has no effect on the gateway ARN.</p> </note>
newtype ActivateGatewayOutput = ActivateGatewayOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeActivateGatewayOutput :: Newtype ActivateGatewayOutput _
derive instance repGenericActivateGatewayOutput :: Generic ActivateGatewayOutput _
instance showActivateGatewayOutput :: Show ActivateGatewayOutput where
  show = genericShow
instance decodeActivateGatewayOutput :: Decode ActivateGatewayOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivateGatewayOutput :: Encode ActivateGatewayOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActivationKey = ActivationKey String
derive instance newtypeActivationKey :: Newtype ActivationKey _
derive instance repGenericActivationKey :: Generic ActivationKey _
instance showActivationKey :: Show ActivationKey where
  show = genericShow
instance decodeActivationKey :: Decode ActivationKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivationKey :: Encode ActivationKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddCacheInput = AddCacheInput 
  { "GatewayARN" :: (GatewayARN)
  , "DiskIds" :: (DiskIds)
  }
derive instance newtypeAddCacheInput :: Newtype AddCacheInput _
derive instance repGenericAddCacheInput :: Generic AddCacheInput _
instance showAddCacheInput :: Show AddCacheInput where
  show = genericShow
instance decodeAddCacheInput :: Decode AddCacheInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddCacheInput :: Encode AddCacheInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddCacheOutput = AddCacheOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeAddCacheOutput :: Newtype AddCacheOutput _
derive instance repGenericAddCacheOutput :: Generic AddCacheOutput _
instance showAddCacheOutput :: Show AddCacheOutput where
  show = genericShow
instance decodeAddCacheOutput :: Decode AddCacheOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddCacheOutput :: Encode AddCacheOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>AddTagsToResourceInput</p>
newtype AddTagsToResourceInput = AddTagsToResourceInput 
  { "ResourceARN" :: (ResourceARN)
  , "Tags" :: (Tags)
  }
derive instance newtypeAddTagsToResourceInput :: Newtype AddTagsToResourceInput _
derive instance repGenericAddTagsToResourceInput :: Generic AddTagsToResourceInput _
instance showAddTagsToResourceInput :: Show AddTagsToResourceInput where
  show = genericShow
instance decodeAddTagsToResourceInput :: Decode AddTagsToResourceInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsToResourceInput :: Encode AddTagsToResourceInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>AddTagsToResourceOutput</p>
newtype AddTagsToResourceOutput = AddTagsToResourceOutput 
  { "ResourceARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  }
derive instance newtypeAddTagsToResourceOutput :: Newtype AddTagsToResourceOutput _
derive instance repGenericAddTagsToResourceOutput :: Generic AddTagsToResourceOutput _
instance showAddTagsToResourceOutput :: Show AddTagsToResourceOutput where
  show = genericShow
instance decodeAddTagsToResourceOutput :: Decode AddTagsToResourceOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsToResourceOutput :: Encode AddTagsToResourceOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddUploadBufferInput = AddUploadBufferInput 
  { "GatewayARN" :: (GatewayARN)
  , "DiskIds" :: (DiskIds)
  }
derive instance newtypeAddUploadBufferInput :: Newtype AddUploadBufferInput _
derive instance repGenericAddUploadBufferInput :: Generic AddUploadBufferInput _
instance showAddUploadBufferInput :: Show AddUploadBufferInput where
  show = genericShow
instance decodeAddUploadBufferInput :: Decode AddUploadBufferInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddUploadBufferInput :: Encode AddUploadBufferInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddUploadBufferOutput = AddUploadBufferOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeAddUploadBufferOutput :: Newtype AddUploadBufferOutput _
derive instance repGenericAddUploadBufferOutput :: Generic AddUploadBufferOutput _
instance showAddUploadBufferOutput :: Show AddUploadBufferOutput where
  show = genericShow
instance decodeAddUploadBufferOutput :: Decode AddUploadBufferOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddUploadBufferOutput :: Encode AddUploadBufferOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>AddWorkingStorageInput$DiskIds</a> </p> </li> </ul>
newtype AddWorkingStorageInput = AddWorkingStorageInput 
  { "GatewayARN" :: (GatewayARN)
  , "DiskIds" :: (DiskIds)
  }
derive instance newtypeAddWorkingStorageInput :: Newtype AddWorkingStorageInput _
derive instance repGenericAddWorkingStorageInput :: Generic AddWorkingStorageInput _
instance showAddWorkingStorageInput :: Show AddWorkingStorageInput where
  show = genericShow
instance decodeAddWorkingStorageInput :: Decode AddWorkingStorageInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddWorkingStorageInput :: Encode AddWorkingStorageInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway for which working storage was configured.</p>
newtype AddWorkingStorageOutput = AddWorkingStorageOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeAddWorkingStorageOutput :: Newtype AddWorkingStorageOutput _
derive instance repGenericAddWorkingStorageOutput :: Generic AddWorkingStorageOutput _
instance showAddWorkingStorageOutput :: Show AddWorkingStorageOutput where
  show = genericShow
instance decodeAddWorkingStorageOutput :: Decode AddWorkingStorageOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddWorkingStorageOutput :: Encode AddWorkingStorageOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BandwidthDownloadRateLimit = BandwidthDownloadRateLimit Number
derive instance newtypeBandwidthDownloadRateLimit :: Newtype BandwidthDownloadRateLimit _
derive instance repGenericBandwidthDownloadRateLimit :: Generic BandwidthDownloadRateLimit _
instance showBandwidthDownloadRateLimit :: Show BandwidthDownloadRateLimit where
  show = genericShow
instance decodeBandwidthDownloadRateLimit :: Decode BandwidthDownloadRateLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBandwidthDownloadRateLimit :: Encode BandwidthDownloadRateLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BandwidthType = BandwidthType String
derive instance newtypeBandwidthType :: Newtype BandwidthType _
derive instance repGenericBandwidthType :: Generic BandwidthType _
instance showBandwidthType :: Show BandwidthType where
  show = genericShow
instance decodeBandwidthType :: Decode BandwidthType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBandwidthType :: Encode BandwidthType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BandwidthUploadRateLimit = BandwidthUploadRateLimit Number
derive instance newtypeBandwidthUploadRateLimit :: Newtype BandwidthUploadRateLimit _
derive instance repGenericBandwidthUploadRateLimit :: Generic BandwidthUploadRateLimit _
instance showBandwidthUploadRateLimit :: Show BandwidthUploadRateLimit where
  show = genericShow
instance decodeBandwidthUploadRateLimit :: Decode BandwidthUploadRateLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBandwidthUploadRateLimit :: Encode BandwidthUploadRateLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an iSCSI cached volume.</p>
newtype CachediSCSIVolume = CachediSCSIVolume 
  { "VolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  , "VolumeId" :: NullOrUndefined.NullOrUndefined (VolumeId)
  , "VolumeType" :: NullOrUndefined.NullOrUndefined (VolumeType)
  , "VolumeStatus" :: NullOrUndefined.NullOrUndefined (VolumeStatus)
  , "VolumeSizeInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "VolumeProgress" :: NullOrUndefined.NullOrUndefined (DoubleObject)
  , "SourceSnapshotId" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  , "VolumeiSCSIAttributes" :: NullOrUndefined.NullOrUndefined (VolumeiSCSIAttributes)
  , "CreatedDate" :: NullOrUndefined.NullOrUndefined (CreatedDate)
  , "VolumeUsedInBytes" :: NullOrUndefined.NullOrUndefined (VolumeUsedInBytes)
  }
derive instance newtypeCachediSCSIVolume :: Newtype CachediSCSIVolume _
derive instance repGenericCachediSCSIVolume :: Generic CachediSCSIVolume _
instance showCachediSCSIVolume :: Show CachediSCSIVolume where
  show = genericShow
instance decodeCachediSCSIVolume :: Decode CachediSCSIVolume where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCachediSCSIVolume :: Encode CachediSCSIVolume where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CachediSCSIVolumes = CachediSCSIVolumes (Array CachediSCSIVolume)
derive instance newtypeCachediSCSIVolumes :: Newtype CachediSCSIVolumes _
derive instance repGenericCachediSCSIVolumes :: Generic CachediSCSIVolumes _
instance showCachediSCSIVolumes :: Show CachediSCSIVolumes where
  show = genericShow
instance decodeCachediSCSIVolumes :: Decode CachediSCSIVolumes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCachediSCSIVolumes :: Encode CachediSCSIVolumes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CancelArchivalInput</p>
newtype CancelArchivalInput = CancelArchivalInput 
  { "GatewayARN" :: (GatewayARN)
  , "TapeARN" :: (TapeARN)
  }
derive instance newtypeCancelArchivalInput :: Newtype CancelArchivalInput _
derive instance repGenericCancelArchivalInput :: Generic CancelArchivalInput _
instance showCancelArchivalInput :: Show CancelArchivalInput where
  show = genericShow
instance decodeCancelArchivalInput :: Decode CancelArchivalInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelArchivalInput :: Encode CancelArchivalInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CancelArchivalOutput</p>
newtype CancelArchivalOutput = CancelArchivalOutput 
  { "TapeARN" :: NullOrUndefined.NullOrUndefined (TapeARN)
  }
derive instance newtypeCancelArchivalOutput :: Newtype CancelArchivalOutput _
derive instance repGenericCancelArchivalOutput :: Generic CancelArchivalOutput _
instance showCancelArchivalOutput :: Show CancelArchivalOutput where
  show = genericShow
instance decodeCancelArchivalOutput :: Decode CancelArchivalOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelArchivalOutput :: Encode CancelArchivalOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CancelRetrievalInput</p>
newtype CancelRetrievalInput = CancelRetrievalInput 
  { "GatewayARN" :: (GatewayARN)
  , "TapeARN" :: (TapeARN)
  }
derive instance newtypeCancelRetrievalInput :: Newtype CancelRetrievalInput _
derive instance repGenericCancelRetrievalInput :: Generic CancelRetrievalInput _
instance showCancelRetrievalInput :: Show CancelRetrievalInput where
  show = genericShow
instance decodeCancelRetrievalInput :: Decode CancelRetrievalInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelRetrievalInput :: Encode CancelRetrievalInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CancelRetrievalOutput</p>
newtype CancelRetrievalOutput = CancelRetrievalOutput 
  { "TapeARN" :: NullOrUndefined.NullOrUndefined (TapeARN)
  }
derive instance newtypeCancelRetrievalOutput :: Newtype CancelRetrievalOutput _
derive instance repGenericCancelRetrievalOutput :: Generic CancelRetrievalOutput _
instance showCancelRetrievalOutput :: Show CancelRetrievalOutput where
  show = genericShow
instance decodeCancelRetrievalOutput :: Decode CancelRetrievalOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelRetrievalOutput :: Encode CancelRetrievalOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChapCredentials = ChapCredentials (Array ChapInfo)
derive instance newtypeChapCredentials :: Newtype ChapCredentials _
derive instance repGenericChapCredentials :: Generic ChapCredentials _
instance showChapCredentials :: Show ChapCredentials where
  show = genericShow
instance decodeChapCredentials :: Decode ChapCredentials where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChapCredentials :: Encode ChapCredentials where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes Challenge-Handshake Authentication Protocol (CHAP) information that supports authentication between your gateway and iSCSI initiators.</p>
newtype ChapInfo = ChapInfo 
  { "TargetARN" :: NullOrUndefined.NullOrUndefined (TargetARN)
  , "SecretToAuthenticateInitiator" :: NullOrUndefined.NullOrUndefined (ChapSecret)
  , "InitiatorName" :: NullOrUndefined.NullOrUndefined (IqnName)
  , "SecretToAuthenticateTarget" :: NullOrUndefined.NullOrUndefined (ChapSecret)
  }
derive instance newtypeChapInfo :: Newtype ChapInfo _
derive instance repGenericChapInfo :: Generic ChapInfo _
instance showChapInfo :: Show ChapInfo where
  show = genericShow
instance decodeChapInfo :: Decode ChapInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChapInfo :: Encode ChapInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChapSecret = ChapSecret String
derive instance newtypeChapSecret :: Newtype ChapSecret _
derive instance repGenericChapSecret :: Generic ChapSecret _
instance showChapSecret :: Show ChapSecret where
  show = genericShow
instance decodeChapSecret :: Decode ChapSecret where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChapSecret :: Encode ChapSecret where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientToken = ClientToken String
derive instance newtypeClientToken :: Newtype ClientToken _
derive instance repGenericClientToken :: Generic ClientToken _
instance showClientToken :: Show ClientToken where
  show = genericShow
instance decodeClientToken :: Decode ClientToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientToken :: Encode ClientToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCachediSCSIVolumeInput = CreateCachediSCSIVolumeInput 
  { "GatewayARN" :: (GatewayARN)
  , "VolumeSizeInBytes" :: (Number)
  , "SnapshotId" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  , "TargetName" :: (TargetName)
  , "SourceVolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  , "NetworkInterfaceId" :: (NetworkInterfaceId)
  , "ClientToken" :: (ClientToken)
  }
derive instance newtypeCreateCachediSCSIVolumeInput :: Newtype CreateCachediSCSIVolumeInput _
derive instance repGenericCreateCachediSCSIVolumeInput :: Generic CreateCachediSCSIVolumeInput _
instance showCreateCachediSCSIVolumeInput :: Show CreateCachediSCSIVolumeInput where
  show = genericShow
instance decodeCreateCachediSCSIVolumeInput :: Decode CreateCachediSCSIVolumeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCachediSCSIVolumeInput :: Encode CreateCachediSCSIVolumeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCachediSCSIVolumeOutput = CreateCachediSCSIVolumeOutput 
  { "VolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  , "TargetARN" :: NullOrUndefined.NullOrUndefined (TargetARN)
  }
derive instance newtypeCreateCachediSCSIVolumeOutput :: Newtype CreateCachediSCSIVolumeOutput _
derive instance repGenericCreateCachediSCSIVolumeOutput :: Generic CreateCachediSCSIVolumeOutput _
instance showCreateCachediSCSIVolumeOutput :: Show CreateCachediSCSIVolumeOutput where
  show = genericShow
instance decodeCreateCachediSCSIVolumeOutput :: Decode CreateCachediSCSIVolumeOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCachediSCSIVolumeOutput :: Encode CreateCachediSCSIVolumeOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CreateNFSFileShareInput</p>
newtype CreateNFSFileShareInput = CreateNFSFileShareInput 
  { "ClientToken" :: (ClientToken)
  , "NFSFileShareDefaults" :: NullOrUndefined.NullOrUndefined (NFSFileShareDefaults)
  , "GatewayARN" :: (GatewayARN)
  , "KMSEncrypted" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "KMSKey" :: NullOrUndefined.NullOrUndefined (KMSKey)
  , "Role" :: (Role)
  , "LocationARN" :: (LocationARN)
  , "DefaultStorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  , "ClientList" :: NullOrUndefined.NullOrUndefined (FileShareClientList)
  , "Squash" :: NullOrUndefined.NullOrUndefined (Squash)
  , "ReadOnly" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "GuessMIMETypeEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeCreateNFSFileShareInput :: Newtype CreateNFSFileShareInput _
derive instance repGenericCreateNFSFileShareInput :: Generic CreateNFSFileShareInput _
instance showCreateNFSFileShareInput :: Show CreateNFSFileShareInput where
  show = genericShow
instance decodeCreateNFSFileShareInput :: Decode CreateNFSFileShareInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateNFSFileShareInput :: Encode CreateNFSFileShareInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CreateNFSFileShareOutput</p>
newtype CreateNFSFileShareOutput = CreateNFSFileShareOutput 
  { "FileShareARN" :: NullOrUndefined.NullOrUndefined (FileShareARN)
  }
derive instance newtypeCreateNFSFileShareOutput :: Newtype CreateNFSFileShareOutput _
derive instance repGenericCreateNFSFileShareOutput :: Generic CreateNFSFileShareOutput _
instance showCreateNFSFileShareOutput :: Show CreateNFSFileShareOutput where
  show = genericShow
instance decodeCreateNFSFileShareOutput :: Decode CreateNFSFileShareOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateNFSFileShareOutput :: Encode CreateNFSFileShareOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSnapshotFromVolumeRecoveryPointInput = CreateSnapshotFromVolumeRecoveryPointInput 
  { "VolumeARN" :: (VolumeARN)
  , "SnapshotDescription" :: (SnapshotDescription)
  }
derive instance newtypeCreateSnapshotFromVolumeRecoveryPointInput :: Newtype CreateSnapshotFromVolumeRecoveryPointInput _
derive instance repGenericCreateSnapshotFromVolumeRecoveryPointInput :: Generic CreateSnapshotFromVolumeRecoveryPointInput _
instance showCreateSnapshotFromVolumeRecoveryPointInput :: Show CreateSnapshotFromVolumeRecoveryPointInput where
  show = genericShow
instance decodeCreateSnapshotFromVolumeRecoveryPointInput :: Decode CreateSnapshotFromVolumeRecoveryPointInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSnapshotFromVolumeRecoveryPointInput :: Encode CreateSnapshotFromVolumeRecoveryPointInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSnapshotFromVolumeRecoveryPointOutput = CreateSnapshotFromVolumeRecoveryPointOutput 
  { "SnapshotId" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  , "VolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  , "VolumeRecoveryPointTime" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateSnapshotFromVolumeRecoveryPointOutput :: Newtype CreateSnapshotFromVolumeRecoveryPointOutput _
derive instance repGenericCreateSnapshotFromVolumeRecoveryPointOutput :: Generic CreateSnapshotFromVolumeRecoveryPointOutput _
instance showCreateSnapshotFromVolumeRecoveryPointOutput :: Show CreateSnapshotFromVolumeRecoveryPointOutput where
  show = genericShow
instance decodeCreateSnapshotFromVolumeRecoveryPointOutput :: Decode CreateSnapshotFromVolumeRecoveryPointOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSnapshotFromVolumeRecoveryPointOutput :: Encode CreateSnapshotFromVolumeRecoveryPointOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>CreateSnapshotInput$SnapshotDescription</a> </p> </li> <li> <p> <a>CreateSnapshotInput$VolumeARN</a> </p> </li> </ul>
newtype CreateSnapshotInput = CreateSnapshotInput 
  { "VolumeARN" :: (VolumeARN)
  , "SnapshotDescription" :: (SnapshotDescription)
  }
derive instance newtypeCreateSnapshotInput :: Newtype CreateSnapshotInput _
derive instance repGenericCreateSnapshotInput :: Generic CreateSnapshotInput _
instance showCreateSnapshotInput :: Show CreateSnapshotInput where
  show = genericShow
instance decodeCreateSnapshotInput :: Decode CreateSnapshotInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSnapshotInput :: Encode CreateSnapshotInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the following fields:</p>
newtype CreateSnapshotOutput = CreateSnapshotOutput 
  { "VolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  , "SnapshotId" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  }
derive instance newtypeCreateSnapshotOutput :: Newtype CreateSnapshotOutput _
derive instance repGenericCreateSnapshotOutput :: Generic CreateSnapshotOutput _
instance showCreateSnapshotOutput :: Show CreateSnapshotOutput where
  show = genericShow
instance decodeCreateSnapshotOutput :: Decode CreateSnapshotOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSnapshotOutput :: Encode CreateSnapshotOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>CreateStorediSCSIVolumeInput$DiskId</a> </p> </li> <li> <p> <a>CreateStorediSCSIVolumeInput$NetworkInterfaceId</a> </p> </li> <li> <p> <a>CreateStorediSCSIVolumeInput$PreserveExistingData</a> </p> </li> <li> <p> <a>CreateStorediSCSIVolumeInput$SnapshotId</a> </p> </li> <li> <p> <a>CreateStorediSCSIVolumeInput$TargetName</a> </p> </li> </ul>
newtype CreateStorediSCSIVolumeInput = CreateStorediSCSIVolumeInput 
  { "GatewayARN" :: (GatewayARN)
  , "DiskId" :: (DiskId)
  , "SnapshotId" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  , "PreserveExistingData" :: (Boolean)
  , "TargetName" :: (TargetName)
  , "NetworkInterfaceId" :: (NetworkInterfaceId)
  }
derive instance newtypeCreateStorediSCSIVolumeInput :: Newtype CreateStorediSCSIVolumeInput _
derive instance repGenericCreateStorediSCSIVolumeInput :: Generic CreateStorediSCSIVolumeInput _
instance showCreateStorediSCSIVolumeInput :: Show CreateStorediSCSIVolumeInput where
  show = genericShow
instance decodeCreateStorediSCSIVolumeInput :: Decode CreateStorediSCSIVolumeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStorediSCSIVolumeInput :: Encode CreateStorediSCSIVolumeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the following fields:</p>
newtype CreateStorediSCSIVolumeOutput = CreateStorediSCSIVolumeOutput 
  { "VolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  , "VolumeSizeInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "TargetARN" :: NullOrUndefined.NullOrUndefined (TargetARN)
  }
derive instance newtypeCreateStorediSCSIVolumeOutput :: Newtype CreateStorediSCSIVolumeOutput _
derive instance repGenericCreateStorediSCSIVolumeOutput :: Generic CreateStorediSCSIVolumeOutput _
instance showCreateStorediSCSIVolumeOutput :: Show CreateStorediSCSIVolumeOutput where
  show = genericShow
instance decodeCreateStorediSCSIVolumeOutput :: Decode CreateStorediSCSIVolumeOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStorediSCSIVolumeOutput :: Encode CreateStorediSCSIVolumeOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CreateTapeWithBarcodeInput</p>
newtype CreateTapeWithBarcodeInput = CreateTapeWithBarcodeInput 
  { "GatewayARN" :: (GatewayARN)
  , "TapeSizeInBytes" :: (TapeSize)
  , "TapeBarcode" :: (TapeBarcode)
  }
derive instance newtypeCreateTapeWithBarcodeInput :: Newtype CreateTapeWithBarcodeInput _
derive instance repGenericCreateTapeWithBarcodeInput :: Generic CreateTapeWithBarcodeInput _
instance showCreateTapeWithBarcodeInput :: Show CreateTapeWithBarcodeInput where
  show = genericShow
instance decodeCreateTapeWithBarcodeInput :: Decode CreateTapeWithBarcodeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTapeWithBarcodeInput :: Encode CreateTapeWithBarcodeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CreateTapeOutput</p>
newtype CreateTapeWithBarcodeOutput = CreateTapeWithBarcodeOutput 
  { "TapeARN" :: NullOrUndefined.NullOrUndefined (TapeARN)
  }
derive instance newtypeCreateTapeWithBarcodeOutput :: Newtype CreateTapeWithBarcodeOutput _
derive instance repGenericCreateTapeWithBarcodeOutput :: Generic CreateTapeWithBarcodeOutput _
instance showCreateTapeWithBarcodeOutput :: Show CreateTapeWithBarcodeOutput where
  show = genericShow
instance decodeCreateTapeWithBarcodeOutput :: Decode CreateTapeWithBarcodeOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTapeWithBarcodeOutput :: Encode CreateTapeWithBarcodeOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CreateTapesInput</p>
newtype CreateTapesInput = CreateTapesInput 
  { "GatewayARN" :: (GatewayARN)
  , "TapeSizeInBytes" :: (TapeSize)
  , "ClientToken" :: (ClientToken)
  , "NumTapesToCreate" :: (NumTapesToCreate)
  , "TapeBarcodePrefix" :: (TapeBarcodePrefix)
  }
derive instance newtypeCreateTapesInput :: Newtype CreateTapesInput _
derive instance repGenericCreateTapesInput :: Generic CreateTapesInput _
instance showCreateTapesInput :: Show CreateTapesInput where
  show = genericShow
instance decodeCreateTapesInput :: Decode CreateTapesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTapesInput :: Encode CreateTapesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CreateTapeOutput</p>
newtype CreateTapesOutput = CreateTapesOutput 
  { "TapeARNs" :: NullOrUndefined.NullOrUndefined (TapeARNs)
  }
derive instance newtypeCreateTapesOutput :: Newtype CreateTapesOutput _
derive instance repGenericCreateTapesOutput :: Generic CreateTapesOutput _
instance showCreateTapesOutput :: Show CreateTapesOutput where
  show = genericShow
instance decodeCreateTapesOutput :: Decode CreateTapesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTapesOutput :: Encode CreateTapesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatedDate = CreatedDate Number
derive instance newtypeCreatedDate :: Newtype CreatedDate _
derive instance repGenericCreatedDate :: Generic CreatedDate _
instance showCreatedDate :: Show CreatedDate where
  show = genericShow
instance decodeCreatedDate :: Decode CreatedDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatedDate :: Encode CreatedDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DayOfWeek = DayOfWeek Int
derive instance newtypeDayOfWeek :: Newtype DayOfWeek _
derive instance repGenericDayOfWeek :: Generic DayOfWeek _
instance showDayOfWeek :: Show DayOfWeek where
  show = genericShow
instance decodeDayOfWeek :: Decode DayOfWeek where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDayOfWeek :: Encode DayOfWeek where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the following fields:</p> <ul> <li> <p> <a>DeleteBandwidthRateLimitInput$BandwidthType</a> </p> </li> </ul>
newtype DeleteBandwidthRateLimitInput = DeleteBandwidthRateLimitInput 
  { "GatewayARN" :: (GatewayARN)
  , "BandwidthType" :: (BandwidthType)
  }
derive instance newtypeDeleteBandwidthRateLimitInput :: Newtype DeleteBandwidthRateLimitInput _
derive instance repGenericDeleteBandwidthRateLimitInput :: Generic DeleteBandwidthRateLimitInput _
instance showDeleteBandwidthRateLimitInput :: Show DeleteBandwidthRateLimitInput where
  show = genericShow
instance decodeDeleteBandwidthRateLimitInput :: Decode DeleteBandwidthRateLimitInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBandwidthRateLimitInput :: Encode DeleteBandwidthRateLimitInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway whose bandwidth rate information was deleted.</p>
newtype DeleteBandwidthRateLimitOutput = DeleteBandwidthRateLimitOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeDeleteBandwidthRateLimitOutput :: Newtype DeleteBandwidthRateLimitOutput _
derive instance repGenericDeleteBandwidthRateLimitOutput :: Generic DeleteBandwidthRateLimitOutput _
instance showDeleteBandwidthRateLimitOutput :: Show DeleteBandwidthRateLimitOutput where
  show = genericShow
instance decodeDeleteBandwidthRateLimitOutput :: Decode DeleteBandwidthRateLimitOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBandwidthRateLimitOutput :: Encode DeleteBandwidthRateLimitOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>DeleteChapCredentialsInput$InitiatorName</a> </p> </li> <li> <p> <a>DeleteChapCredentialsInput$TargetARN</a> </p> </li> </ul>
newtype DeleteChapCredentialsInput = DeleteChapCredentialsInput 
  { "TargetARN" :: (TargetARN)
  , "InitiatorName" :: (IqnName)
  }
derive instance newtypeDeleteChapCredentialsInput :: Newtype DeleteChapCredentialsInput _
derive instance repGenericDeleteChapCredentialsInput :: Generic DeleteChapCredentialsInput _
instance showDeleteChapCredentialsInput :: Show DeleteChapCredentialsInput where
  show = genericShow
instance decodeDeleteChapCredentialsInput :: Decode DeleteChapCredentialsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteChapCredentialsInput :: Encode DeleteChapCredentialsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the following fields:</p>
newtype DeleteChapCredentialsOutput = DeleteChapCredentialsOutput 
  { "TargetARN" :: NullOrUndefined.NullOrUndefined (TargetARN)
  , "InitiatorName" :: NullOrUndefined.NullOrUndefined (IqnName)
  }
derive instance newtypeDeleteChapCredentialsOutput :: Newtype DeleteChapCredentialsOutput _
derive instance repGenericDeleteChapCredentialsOutput :: Generic DeleteChapCredentialsOutput _
instance showDeleteChapCredentialsOutput :: Show DeleteChapCredentialsOutput where
  show = genericShow
instance decodeDeleteChapCredentialsOutput :: Decode DeleteChapCredentialsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteChapCredentialsOutput :: Encode DeleteChapCredentialsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DeleteFileShareInput</p>
newtype DeleteFileShareInput = DeleteFileShareInput 
  { "FileShareARN" :: (FileShareARN)
  , "ForceDelete" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeDeleteFileShareInput :: Newtype DeleteFileShareInput _
derive instance repGenericDeleteFileShareInput :: Generic DeleteFileShareInput _
instance showDeleteFileShareInput :: Show DeleteFileShareInput where
  show = genericShow
instance decodeDeleteFileShareInput :: Decode DeleteFileShareInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteFileShareInput :: Encode DeleteFileShareInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DeleteFileShareOutput</p>
newtype DeleteFileShareOutput = DeleteFileShareOutput 
  { "FileShareARN" :: NullOrUndefined.NullOrUndefined (FileShareARN)
  }
derive instance newtypeDeleteFileShareOutput :: Newtype DeleteFileShareOutput _
derive instance repGenericDeleteFileShareOutput :: Generic DeleteFileShareOutput _
instance showDeleteFileShareOutput :: Show DeleteFileShareOutput where
  show = genericShow
instance decodeDeleteFileShareOutput :: Decode DeleteFileShareOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteFileShareOutput :: Encode DeleteFileShareOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the ID of the gateway to delete.</p>
newtype DeleteGatewayInput = DeleteGatewayInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDeleteGatewayInput :: Newtype DeleteGatewayInput _
derive instance repGenericDeleteGatewayInput :: Generic DeleteGatewayInput _
instance showDeleteGatewayInput :: Show DeleteGatewayInput where
  show = genericShow
instance decodeDeleteGatewayInput :: Decode DeleteGatewayInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGatewayInput :: Encode DeleteGatewayInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the ID of the deleted gateway.</p>
newtype DeleteGatewayOutput = DeleteGatewayOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeDeleteGatewayOutput :: Newtype DeleteGatewayOutput _
derive instance repGenericDeleteGatewayOutput :: Generic DeleteGatewayOutput _
instance showDeleteGatewayOutput :: Show DeleteGatewayOutput where
  show = genericShow
instance decodeDeleteGatewayOutput :: Decode DeleteGatewayOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGatewayOutput :: Encode DeleteGatewayOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSnapshotScheduleInput = DeleteSnapshotScheduleInput 
  { "VolumeARN" :: (VolumeARN)
  }
derive instance newtypeDeleteSnapshotScheduleInput :: Newtype DeleteSnapshotScheduleInput _
derive instance repGenericDeleteSnapshotScheduleInput :: Generic DeleteSnapshotScheduleInput _
instance showDeleteSnapshotScheduleInput :: Show DeleteSnapshotScheduleInput where
  show = genericShow
instance decodeDeleteSnapshotScheduleInput :: Decode DeleteSnapshotScheduleInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSnapshotScheduleInput :: Encode DeleteSnapshotScheduleInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSnapshotScheduleOutput = DeleteSnapshotScheduleOutput 
  { "VolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  }
derive instance newtypeDeleteSnapshotScheduleOutput :: Newtype DeleteSnapshotScheduleOutput _
derive instance repGenericDeleteSnapshotScheduleOutput :: Generic DeleteSnapshotScheduleOutput _
instance showDeleteSnapshotScheduleOutput :: Show DeleteSnapshotScheduleOutput where
  show = genericShow
instance decodeDeleteSnapshotScheduleOutput :: Decode DeleteSnapshotScheduleOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSnapshotScheduleOutput :: Encode DeleteSnapshotScheduleOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DeleteTapeArchiveInput</p>
newtype DeleteTapeArchiveInput = DeleteTapeArchiveInput 
  { "TapeARN" :: (TapeARN)
  }
derive instance newtypeDeleteTapeArchiveInput :: Newtype DeleteTapeArchiveInput _
derive instance repGenericDeleteTapeArchiveInput :: Generic DeleteTapeArchiveInput _
instance showDeleteTapeArchiveInput :: Show DeleteTapeArchiveInput where
  show = genericShow
instance decodeDeleteTapeArchiveInput :: Decode DeleteTapeArchiveInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTapeArchiveInput :: Encode DeleteTapeArchiveInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DeleteTapeArchiveOutput</p>
newtype DeleteTapeArchiveOutput = DeleteTapeArchiveOutput 
  { "TapeARN" :: NullOrUndefined.NullOrUndefined (TapeARN)
  }
derive instance newtypeDeleteTapeArchiveOutput :: Newtype DeleteTapeArchiveOutput _
derive instance repGenericDeleteTapeArchiveOutput :: Generic DeleteTapeArchiveOutput _
instance showDeleteTapeArchiveOutput :: Show DeleteTapeArchiveOutput where
  show = genericShow
instance decodeDeleteTapeArchiveOutput :: Decode DeleteTapeArchiveOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTapeArchiveOutput :: Encode DeleteTapeArchiveOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DeleteTapeInput</p>
newtype DeleteTapeInput = DeleteTapeInput 
  { "GatewayARN" :: (GatewayARN)
  , "TapeARN" :: (TapeARN)
  }
derive instance newtypeDeleteTapeInput :: Newtype DeleteTapeInput _
derive instance repGenericDeleteTapeInput :: Generic DeleteTapeInput _
instance showDeleteTapeInput :: Show DeleteTapeInput where
  show = genericShow
instance decodeDeleteTapeInput :: Decode DeleteTapeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTapeInput :: Encode DeleteTapeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DeleteTapeOutput</p>
newtype DeleteTapeOutput = DeleteTapeOutput 
  { "TapeARN" :: NullOrUndefined.NullOrUndefined (TapeARN)
  }
derive instance newtypeDeleteTapeOutput :: Newtype DeleteTapeOutput _
derive instance repGenericDeleteTapeOutput :: Generic DeleteTapeOutput _
instance showDeleteTapeOutput :: Show DeleteTapeOutput where
  show = genericShow
instance decodeDeleteTapeOutput :: Decode DeleteTapeOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTapeOutput :: Encode DeleteTapeOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the <a>DeleteVolumeInput$VolumeARN</a> to delete.</p>
newtype DeleteVolumeInput = DeleteVolumeInput 
  { "VolumeARN" :: (VolumeARN)
  }
derive instance newtypeDeleteVolumeInput :: Newtype DeleteVolumeInput _
derive instance repGenericDeleteVolumeInput :: Generic DeleteVolumeInput _
instance showDeleteVolumeInput :: Show DeleteVolumeInput where
  show = genericShow
instance decodeDeleteVolumeInput :: Decode DeleteVolumeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVolumeInput :: Encode DeleteVolumeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the storage volume that was deleted</p>
newtype DeleteVolumeOutput = DeleteVolumeOutput 
  { "VolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  }
derive instance newtypeDeleteVolumeOutput :: Newtype DeleteVolumeOutput _
derive instance repGenericDeleteVolumeOutput :: Generic DeleteVolumeOutput _
instance showDeleteVolumeOutput :: Show DeleteVolumeOutput where
  show = genericShow
instance decodeDeleteVolumeOutput :: Decode DeleteVolumeOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVolumeOutput :: Encode DeleteVolumeOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway.</p>
newtype DescribeBandwidthRateLimitInput = DescribeBandwidthRateLimitInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDescribeBandwidthRateLimitInput :: Newtype DescribeBandwidthRateLimitInput _
derive instance repGenericDescribeBandwidthRateLimitInput :: Generic DescribeBandwidthRateLimitInput _
instance showDescribeBandwidthRateLimitInput :: Show DescribeBandwidthRateLimitInput where
  show = genericShow
instance decodeDescribeBandwidthRateLimitInput :: Decode DescribeBandwidthRateLimitInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBandwidthRateLimitInput :: Encode DescribeBandwidthRateLimitInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the following fields:</p>
newtype DescribeBandwidthRateLimitOutput = DescribeBandwidthRateLimitOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "AverageUploadRateLimitInBitsPerSec" :: NullOrUndefined.NullOrUndefined (BandwidthUploadRateLimit)
  , "AverageDownloadRateLimitInBitsPerSec" :: NullOrUndefined.NullOrUndefined (BandwidthDownloadRateLimit)
  }
derive instance newtypeDescribeBandwidthRateLimitOutput :: Newtype DescribeBandwidthRateLimitOutput _
derive instance repGenericDescribeBandwidthRateLimitOutput :: Generic DescribeBandwidthRateLimitOutput _
instance showDescribeBandwidthRateLimitOutput :: Show DescribeBandwidthRateLimitOutput where
  show = genericShow
instance decodeDescribeBandwidthRateLimitOutput :: Decode DescribeBandwidthRateLimitOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBandwidthRateLimitOutput :: Encode DescribeBandwidthRateLimitOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeCacheInput = DescribeCacheInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDescribeCacheInput :: Newtype DescribeCacheInput _
derive instance repGenericDescribeCacheInput :: Generic DescribeCacheInput _
instance showDescribeCacheInput :: Show DescribeCacheInput where
  show = genericShow
instance decodeDescribeCacheInput :: Decode DescribeCacheInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCacheInput :: Encode DescribeCacheInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeCacheOutput = DescribeCacheOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "DiskIds" :: NullOrUndefined.NullOrUndefined (DiskIds)
  , "CacheAllocatedInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "CacheUsedPercentage" :: NullOrUndefined.NullOrUndefined (Number)
  , "CacheDirtyPercentage" :: NullOrUndefined.NullOrUndefined (Number)
  , "CacheHitPercentage" :: NullOrUndefined.NullOrUndefined (Number)
  , "CacheMissPercentage" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeDescribeCacheOutput :: Newtype DescribeCacheOutput _
derive instance repGenericDescribeCacheOutput :: Generic DescribeCacheOutput _
instance showDescribeCacheOutput :: Show DescribeCacheOutput where
  show = genericShow
instance decodeDescribeCacheOutput :: Decode DescribeCacheOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCacheOutput :: Encode DescribeCacheOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeCachediSCSIVolumesInput = DescribeCachediSCSIVolumesInput 
  { "VolumeARNs" :: (VolumeARNs)
  }
derive instance newtypeDescribeCachediSCSIVolumesInput :: Newtype DescribeCachediSCSIVolumesInput _
derive instance repGenericDescribeCachediSCSIVolumesInput :: Generic DescribeCachediSCSIVolumesInput _
instance showDescribeCachediSCSIVolumesInput :: Show DescribeCachediSCSIVolumesInput where
  show = genericShow
instance decodeDescribeCachediSCSIVolumesInput :: Decode DescribeCachediSCSIVolumesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCachediSCSIVolumesInput :: Encode DescribeCachediSCSIVolumesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the following fields:</p>
newtype DescribeCachediSCSIVolumesOutput = DescribeCachediSCSIVolumesOutput 
  { "CachediSCSIVolumes" :: NullOrUndefined.NullOrUndefined (CachediSCSIVolumes)
  }
derive instance newtypeDescribeCachediSCSIVolumesOutput :: Newtype DescribeCachediSCSIVolumesOutput _
derive instance repGenericDescribeCachediSCSIVolumesOutput :: Generic DescribeCachediSCSIVolumesOutput _
instance showDescribeCachediSCSIVolumesOutput :: Show DescribeCachediSCSIVolumesOutput where
  show = genericShow
instance decodeDescribeCachediSCSIVolumesOutput :: Decode DescribeCachediSCSIVolumesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCachediSCSIVolumesOutput :: Encode DescribeCachediSCSIVolumesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the Amazon Resource Name (ARN) of the iSCSI volume target.</p>
newtype DescribeChapCredentialsInput = DescribeChapCredentialsInput 
  { "TargetARN" :: (TargetARN)
  }
derive instance newtypeDescribeChapCredentialsInput :: Newtype DescribeChapCredentialsInput _
derive instance repGenericDescribeChapCredentialsInput :: Generic DescribeChapCredentialsInput _
instance showDescribeChapCredentialsInput :: Show DescribeChapCredentialsInput where
  show = genericShow
instance decodeDescribeChapCredentialsInput :: Decode DescribeChapCredentialsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeChapCredentialsInput :: Encode DescribeChapCredentialsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing a .</p>
newtype DescribeChapCredentialsOutput = DescribeChapCredentialsOutput 
  { "ChapCredentials" :: NullOrUndefined.NullOrUndefined (ChapCredentials)
  }
derive instance newtypeDescribeChapCredentialsOutput :: Newtype DescribeChapCredentialsOutput _
derive instance repGenericDescribeChapCredentialsOutput :: Generic DescribeChapCredentialsOutput _
instance showDescribeChapCredentialsOutput :: Show DescribeChapCredentialsOutput where
  show = genericShow
instance decodeDescribeChapCredentialsOutput :: Decode DescribeChapCredentialsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeChapCredentialsOutput :: Encode DescribeChapCredentialsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the ID of the gateway.</p>
newtype DescribeGatewayInformationInput = DescribeGatewayInformationInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDescribeGatewayInformationInput :: Newtype DescribeGatewayInformationInput _
derive instance repGenericDescribeGatewayInformationInput :: Generic DescribeGatewayInformationInput _
instance showDescribeGatewayInformationInput :: Show DescribeGatewayInformationInput where
  show = genericShow
instance decodeDescribeGatewayInformationInput :: Decode DescribeGatewayInformationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGatewayInformationInput :: Encode DescribeGatewayInformationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the following fields:</p>
newtype DescribeGatewayInformationOutput = DescribeGatewayInformationOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "GatewayId" :: NullOrUndefined.NullOrUndefined (GatewayId)
  , "GatewayName" :: NullOrUndefined.NullOrUndefined (String)
  , "GatewayTimezone" :: NullOrUndefined.NullOrUndefined (GatewayTimezone)
  , "GatewayState" :: NullOrUndefined.NullOrUndefined (GatewayState)
  , "GatewayNetworkInterfaces" :: NullOrUndefined.NullOrUndefined (GatewayNetworkInterfaces)
  , "GatewayType" :: NullOrUndefined.NullOrUndefined (GatewayType)
  , "NextUpdateAvailabilityDate" :: NullOrUndefined.NullOrUndefined (NextUpdateAvailabilityDate)
  , "LastSoftwareUpdate" :: NullOrUndefined.NullOrUndefined (LastSoftwareUpdate)
  }
derive instance newtypeDescribeGatewayInformationOutput :: Newtype DescribeGatewayInformationOutput _
derive instance repGenericDescribeGatewayInformationOutput :: Generic DescribeGatewayInformationOutput _
instance showDescribeGatewayInformationOutput :: Show DescribeGatewayInformationOutput where
  show = genericShow
instance decodeDescribeGatewayInformationOutput :: Decode DescribeGatewayInformationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGatewayInformationOutput :: Encode DescribeGatewayInformationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway.</p>
newtype DescribeMaintenanceStartTimeInput = DescribeMaintenanceStartTimeInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDescribeMaintenanceStartTimeInput :: Newtype DescribeMaintenanceStartTimeInput _
derive instance repGenericDescribeMaintenanceStartTimeInput :: Generic DescribeMaintenanceStartTimeInput _
instance showDescribeMaintenanceStartTimeInput :: Show DescribeMaintenanceStartTimeInput where
  show = genericShow
instance decodeDescribeMaintenanceStartTimeInput :: Decode DescribeMaintenanceStartTimeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceStartTimeInput :: Encode DescribeMaintenanceStartTimeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the following fields:</p> <ul> <li> <p> <a>DescribeMaintenanceStartTimeOutput$DayOfWeek</a> </p> </li> <li> <p> <a>DescribeMaintenanceStartTimeOutput$HourOfDay</a> </p> </li> <li> <p> <a>DescribeMaintenanceStartTimeOutput$MinuteOfHour</a> </p> </li> <li> <p> <a>DescribeMaintenanceStartTimeOutput$Timezone</a> </p> </li> </ul>
newtype DescribeMaintenanceStartTimeOutput = DescribeMaintenanceStartTimeOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "HourOfDay" :: NullOrUndefined.NullOrUndefined (HourOfDay)
  , "MinuteOfHour" :: NullOrUndefined.NullOrUndefined (MinuteOfHour)
  , "DayOfWeek" :: NullOrUndefined.NullOrUndefined (DayOfWeek)
  , "Timezone" :: NullOrUndefined.NullOrUndefined (GatewayTimezone)
  }
derive instance newtypeDescribeMaintenanceStartTimeOutput :: Newtype DescribeMaintenanceStartTimeOutput _
derive instance repGenericDescribeMaintenanceStartTimeOutput :: Generic DescribeMaintenanceStartTimeOutput _
instance showDescribeMaintenanceStartTimeOutput :: Show DescribeMaintenanceStartTimeOutput where
  show = genericShow
instance decodeDescribeMaintenanceStartTimeOutput :: Decode DescribeMaintenanceStartTimeOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceStartTimeOutput :: Encode DescribeMaintenanceStartTimeOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DescribeNFSFileSharesInput</p>
newtype DescribeNFSFileSharesInput = DescribeNFSFileSharesInput 
  { "FileShareARNList" :: (FileShareARNList)
  }
derive instance newtypeDescribeNFSFileSharesInput :: Newtype DescribeNFSFileSharesInput _
derive instance repGenericDescribeNFSFileSharesInput :: Generic DescribeNFSFileSharesInput _
instance showDescribeNFSFileSharesInput :: Show DescribeNFSFileSharesInput where
  show = genericShow
instance decodeDescribeNFSFileSharesInput :: Decode DescribeNFSFileSharesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeNFSFileSharesInput :: Encode DescribeNFSFileSharesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DescribeNFSFileSharesOutput</p>
newtype DescribeNFSFileSharesOutput = DescribeNFSFileSharesOutput 
  { "NFSFileShareInfoList" :: NullOrUndefined.NullOrUndefined (NFSFileShareInfoList)
  }
derive instance newtypeDescribeNFSFileSharesOutput :: Newtype DescribeNFSFileSharesOutput _
derive instance repGenericDescribeNFSFileSharesOutput :: Generic DescribeNFSFileSharesOutput _
instance showDescribeNFSFileSharesOutput :: Show DescribeNFSFileSharesOutput where
  show = genericShow
instance decodeDescribeNFSFileSharesOutput :: Decode DescribeNFSFileSharesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeNFSFileSharesOutput :: Encode DescribeNFSFileSharesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the <a>DescribeSnapshotScheduleInput$VolumeARN</a> of the volume.</p>
newtype DescribeSnapshotScheduleInput = DescribeSnapshotScheduleInput 
  { "VolumeARN" :: (VolumeARN)
  }
derive instance newtypeDescribeSnapshotScheduleInput :: Newtype DescribeSnapshotScheduleInput _
derive instance repGenericDescribeSnapshotScheduleInput :: Generic DescribeSnapshotScheduleInput _
instance showDescribeSnapshotScheduleInput :: Show DescribeSnapshotScheduleInput where
  show = genericShow
instance decodeDescribeSnapshotScheduleInput :: Decode DescribeSnapshotScheduleInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSnapshotScheduleInput :: Encode DescribeSnapshotScheduleInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeSnapshotScheduleOutput = DescribeSnapshotScheduleOutput 
  { "VolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  , "StartAt" :: NullOrUndefined.NullOrUndefined (HourOfDay)
  , "RecurrenceInHours" :: NullOrUndefined.NullOrUndefined (RecurrenceInHours)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Timezone" :: NullOrUndefined.NullOrUndefined (GatewayTimezone)
  }
derive instance newtypeDescribeSnapshotScheduleOutput :: Newtype DescribeSnapshotScheduleOutput _
derive instance repGenericDescribeSnapshotScheduleOutput :: Generic DescribeSnapshotScheduleOutput _
instance showDescribeSnapshotScheduleOutput :: Show DescribeSnapshotScheduleOutput where
  show = genericShow
instance decodeDescribeSnapshotScheduleOutput :: Decode DescribeSnapshotScheduleOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSnapshotScheduleOutput :: Encode DescribeSnapshotScheduleOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing a list of <a>DescribeStorediSCSIVolumesInput$VolumeARNs</a>.</p>
newtype DescribeStorediSCSIVolumesInput = DescribeStorediSCSIVolumesInput 
  { "VolumeARNs" :: (VolumeARNs)
  }
derive instance newtypeDescribeStorediSCSIVolumesInput :: Newtype DescribeStorediSCSIVolumesInput _
derive instance repGenericDescribeStorediSCSIVolumesInput :: Generic DescribeStorediSCSIVolumesInput _
instance showDescribeStorediSCSIVolumesInput :: Show DescribeStorediSCSIVolumesInput where
  show = genericShow
instance decodeDescribeStorediSCSIVolumesInput :: Decode DescribeStorediSCSIVolumesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStorediSCSIVolumesInput :: Encode DescribeStorediSCSIVolumesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStorediSCSIVolumesOutput = DescribeStorediSCSIVolumesOutput 
  { "StorediSCSIVolumes" :: NullOrUndefined.NullOrUndefined (StorediSCSIVolumes)
  }
derive instance newtypeDescribeStorediSCSIVolumesOutput :: Newtype DescribeStorediSCSIVolumesOutput _
derive instance repGenericDescribeStorediSCSIVolumesOutput :: Generic DescribeStorediSCSIVolumesOutput _
instance showDescribeStorediSCSIVolumesOutput :: Show DescribeStorediSCSIVolumesOutput where
  show = genericShow
instance decodeDescribeStorediSCSIVolumesOutput :: Decode DescribeStorediSCSIVolumesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStorediSCSIVolumesOutput :: Encode DescribeStorediSCSIVolumesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DescribeTapeArchivesInput</p>
newtype DescribeTapeArchivesInput = DescribeTapeArchivesInput 
  { "TapeARNs" :: NullOrUndefined.NullOrUndefined (TapeARNs)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeDescribeTapeArchivesInput :: Newtype DescribeTapeArchivesInput _
derive instance repGenericDescribeTapeArchivesInput :: Generic DescribeTapeArchivesInput _
instance showDescribeTapeArchivesInput :: Show DescribeTapeArchivesInput where
  show = genericShow
instance decodeDescribeTapeArchivesInput :: Decode DescribeTapeArchivesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTapeArchivesInput :: Encode DescribeTapeArchivesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DescribeTapeArchivesOutput</p>
newtype DescribeTapeArchivesOutput = DescribeTapeArchivesOutput 
  { "TapeArchives" :: NullOrUndefined.NullOrUndefined (TapeArchives)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeTapeArchivesOutput :: Newtype DescribeTapeArchivesOutput _
derive instance repGenericDescribeTapeArchivesOutput :: Generic DescribeTapeArchivesOutput _
instance showDescribeTapeArchivesOutput :: Show DescribeTapeArchivesOutput where
  show = genericShow
instance decodeDescribeTapeArchivesOutput :: Decode DescribeTapeArchivesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTapeArchivesOutput :: Encode DescribeTapeArchivesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DescribeTapeRecoveryPointsInput</p>
newtype DescribeTapeRecoveryPointsInput = DescribeTapeRecoveryPointsInput 
  { "GatewayARN" :: (GatewayARN)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeDescribeTapeRecoveryPointsInput :: Newtype DescribeTapeRecoveryPointsInput _
derive instance repGenericDescribeTapeRecoveryPointsInput :: Generic DescribeTapeRecoveryPointsInput _
instance showDescribeTapeRecoveryPointsInput :: Show DescribeTapeRecoveryPointsInput where
  show = genericShow
instance decodeDescribeTapeRecoveryPointsInput :: Decode DescribeTapeRecoveryPointsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTapeRecoveryPointsInput :: Encode DescribeTapeRecoveryPointsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DescribeTapeRecoveryPointsOutput</p>
newtype DescribeTapeRecoveryPointsOutput = DescribeTapeRecoveryPointsOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "TapeRecoveryPointInfos" :: NullOrUndefined.NullOrUndefined (TapeRecoveryPointInfos)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeTapeRecoveryPointsOutput :: Newtype DescribeTapeRecoveryPointsOutput _
derive instance repGenericDescribeTapeRecoveryPointsOutput :: Generic DescribeTapeRecoveryPointsOutput _
instance showDescribeTapeRecoveryPointsOutput :: Show DescribeTapeRecoveryPointsOutput where
  show = genericShow
instance decodeDescribeTapeRecoveryPointsOutput :: Decode DescribeTapeRecoveryPointsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTapeRecoveryPointsOutput :: Encode DescribeTapeRecoveryPointsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DescribeTapesInput</p>
newtype DescribeTapesInput = DescribeTapesInput 
  { "GatewayARN" :: (GatewayARN)
  , "TapeARNs" :: NullOrUndefined.NullOrUndefined (TapeARNs)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeDescribeTapesInput :: Newtype DescribeTapesInput _
derive instance repGenericDescribeTapesInput :: Generic DescribeTapesInput _
instance showDescribeTapesInput :: Show DescribeTapesInput where
  show = genericShow
instance decodeDescribeTapesInput :: Decode DescribeTapesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTapesInput :: Encode DescribeTapesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DescribeTapesOutput</p>
newtype DescribeTapesOutput = DescribeTapesOutput 
  { "Tapes" :: NullOrUndefined.NullOrUndefined (Tapes)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeTapesOutput :: Newtype DescribeTapesOutput _
derive instance repGenericDescribeTapesOutput :: Generic DescribeTapesOutput _
instance showDescribeTapesOutput :: Show DescribeTapesOutput where
  show = genericShow
instance decodeDescribeTapesOutput :: Decode DescribeTapesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTapesOutput :: Encode DescribeTapesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeUploadBufferInput = DescribeUploadBufferInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDescribeUploadBufferInput :: Newtype DescribeUploadBufferInput _
derive instance repGenericDescribeUploadBufferInput :: Generic DescribeUploadBufferInput _
instance showDescribeUploadBufferInput :: Show DescribeUploadBufferInput where
  show = genericShow
instance decodeDescribeUploadBufferInput :: Decode DescribeUploadBufferInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUploadBufferInput :: Encode DescribeUploadBufferInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeUploadBufferOutput = DescribeUploadBufferOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "DiskIds" :: NullOrUndefined.NullOrUndefined (DiskIds)
  , "UploadBufferUsedInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "UploadBufferAllocatedInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeDescribeUploadBufferOutput :: Newtype DescribeUploadBufferOutput _
derive instance repGenericDescribeUploadBufferOutput :: Generic DescribeUploadBufferOutput _
instance showDescribeUploadBufferOutput :: Show DescribeUploadBufferOutput where
  show = genericShow
instance decodeDescribeUploadBufferOutput :: Decode DescribeUploadBufferOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUploadBufferOutput :: Encode DescribeUploadBufferOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DescribeVTLDevicesInput</p>
newtype DescribeVTLDevicesInput = DescribeVTLDevicesInput 
  { "GatewayARN" :: (GatewayARN)
  , "VTLDeviceARNs" :: NullOrUndefined.NullOrUndefined (VTLDeviceARNs)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeDescribeVTLDevicesInput :: Newtype DescribeVTLDevicesInput _
derive instance repGenericDescribeVTLDevicesInput :: Generic DescribeVTLDevicesInput _
instance showDescribeVTLDevicesInput :: Show DescribeVTLDevicesInput where
  show = genericShow
instance decodeDescribeVTLDevicesInput :: Decode DescribeVTLDevicesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeVTLDevicesInput :: Encode DescribeVTLDevicesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DescribeVTLDevicesOutput</p>
newtype DescribeVTLDevicesOutput = DescribeVTLDevicesOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "VTLDevices" :: NullOrUndefined.NullOrUndefined (VTLDevices)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeVTLDevicesOutput :: Newtype DescribeVTLDevicesOutput _
derive instance repGenericDescribeVTLDevicesOutput :: Generic DescribeVTLDevicesOutput _
instance showDescribeVTLDevicesOutput :: Show DescribeVTLDevicesOutput where
  show = genericShow
instance decodeDescribeVTLDevicesOutput :: Decode DescribeVTLDevicesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeVTLDevicesOutput :: Encode DescribeVTLDevicesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway.</p>
newtype DescribeWorkingStorageInput = DescribeWorkingStorageInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDescribeWorkingStorageInput :: Newtype DescribeWorkingStorageInput _
derive instance repGenericDescribeWorkingStorageInput :: Generic DescribeWorkingStorageInput _
instance showDescribeWorkingStorageInput :: Show DescribeWorkingStorageInput where
  show = genericShow
instance decodeDescribeWorkingStorageInput :: Decode DescribeWorkingStorageInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeWorkingStorageInput :: Encode DescribeWorkingStorageInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the following fields:</p>
newtype DescribeWorkingStorageOutput = DescribeWorkingStorageOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "DiskIds" :: NullOrUndefined.NullOrUndefined (DiskIds)
  , "WorkingStorageUsedInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "WorkingStorageAllocatedInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeDescribeWorkingStorageOutput :: Newtype DescribeWorkingStorageOutput _
derive instance repGenericDescribeWorkingStorageOutput :: Generic DescribeWorkingStorageOutput _
instance showDescribeWorkingStorageOutput :: Show DescribeWorkingStorageOutput where
  show = genericShow
instance decodeDescribeWorkingStorageOutput :: Decode DescribeWorkingStorageOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeWorkingStorageOutput :: Encode DescribeWorkingStorageOutput where
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


newtype DeviceType = DeviceType String
derive instance newtypeDeviceType :: Newtype DeviceType _
derive instance repGenericDeviceType :: Generic DeviceType _
instance showDeviceType :: Show DeviceType where
  show = genericShow
instance decodeDeviceType :: Decode DeviceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceType :: Encode DeviceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lists iSCSI information about a VTL device.</p>
newtype DeviceiSCSIAttributes = DeviceiSCSIAttributes 
  { "TargetARN" :: NullOrUndefined.NullOrUndefined (TargetARN)
  , "NetworkInterfaceId" :: NullOrUndefined.NullOrUndefined (NetworkInterfaceId)
  , "NetworkInterfacePort" :: NullOrUndefined.NullOrUndefined (Int)
  , "ChapEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeDeviceiSCSIAttributes :: Newtype DeviceiSCSIAttributes _
derive instance repGenericDeviceiSCSIAttributes :: Generic DeviceiSCSIAttributes _
instance showDeviceiSCSIAttributes :: Show DeviceiSCSIAttributes where
  show = genericShow
instance decodeDeviceiSCSIAttributes :: Decode DeviceiSCSIAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceiSCSIAttributes :: Encode DeviceiSCSIAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DisableGatewayInput</p>
newtype DisableGatewayInput = DisableGatewayInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDisableGatewayInput :: Newtype DisableGatewayInput _
derive instance repGenericDisableGatewayInput :: Generic DisableGatewayInput _
instance showDisableGatewayInput :: Show DisableGatewayInput where
  show = genericShow
instance decodeDisableGatewayInput :: Decode DisableGatewayInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableGatewayInput :: Encode DisableGatewayInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DisableGatewayOutput</p>
newtype DisableGatewayOutput = DisableGatewayOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeDisableGatewayOutput :: Newtype DisableGatewayOutput _
derive instance repGenericDisableGatewayOutput :: Generic DisableGatewayOutput _
instance showDisableGatewayOutput :: Show DisableGatewayOutput where
  show = genericShow
instance decodeDisableGatewayOutput :: Decode DisableGatewayOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableGatewayOutput :: Encode DisableGatewayOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Disk = Disk 
  { "DiskId" :: NullOrUndefined.NullOrUndefined (DiskId)
  , "DiskPath" :: NullOrUndefined.NullOrUndefined (String)
  , "DiskNode" :: NullOrUndefined.NullOrUndefined (String)
  , "DiskStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "DiskSizeInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "DiskAllocationType" :: NullOrUndefined.NullOrUndefined (DiskAllocationType)
  , "DiskAllocationResource" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDisk :: Newtype Disk _
derive instance repGenericDisk :: Generic Disk _
instance showDisk :: Show Disk where
  show = genericShow
instance decodeDisk :: Decode Disk where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisk :: Encode Disk where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DiskAllocationType = DiskAllocationType String
derive instance newtypeDiskAllocationType :: Newtype DiskAllocationType _
derive instance repGenericDiskAllocationType :: Generic DiskAllocationType _
instance showDiskAllocationType :: Show DiskAllocationType where
  show = genericShow
instance decodeDiskAllocationType :: Decode DiskAllocationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiskAllocationType :: Encode DiskAllocationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DiskId = DiskId String
derive instance newtypeDiskId :: Newtype DiskId _
derive instance repGenericDiskId :: Generic DiskId _
instance showDiskId :: Show DiskId where
  show = genericShow
instance decodeDiskId :: Decode DiskId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiskId :: Encode DiskId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DiskIds = DiskIds (Array DiskId)
derive instance newtypeDiskIds :: Newtype DiskIds _
derive instance repGenericDiskIds :: Generic DiskIds _
instance showDiskIds :: Show DiskIds where
  show = genericShow
instance decodeDiskIds :: Decode DiskIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiskIds :: Encode DiskIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Disks = Disks (Array Disk)
derive instance newtypeDisks :: Newtype Disks _
derive instance repGenericDisks :: Generic Disks _
instance showDisks :: Show Disks where
  show = genericShow
instance decodeDisks :: Decode Disks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisks :: Encode Disks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DoubleObject = DoubleObject Number
derive instance newtypeDoubleObject :: Newtype DoubleObject _
derive instance repGenericDoubleObject :: Generic DoubleObject _
instance showDoubleObject :: Show DoubleObject where
  show = genericShow
instance decodeDoubleObject :: Decode DoubleObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDoubleObject :: Encode DoubleObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _
derive instance repGenericErrorCode :: Generic ErrorCode _
instance showErrorCode :: Show ErrorCode where
  show = genericShow
instance decodeErrorCode :: Decode ErrorCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorCode :: Encode ErrorCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon Resource Name (ARN) of the file share. </p>
newtype FileShareARN = FileShareARN String
derive instance newtypeFileShareARN :: Newtype FileShareARN _
derive instance repGenericFileShareARN :: Generic FileShareARN _
instance showFileShareARN :: Show FileShareARN where
  show = genericShow
instance decodeFileShareARN :: Decode FileShareARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileShareARN :: Encode FileShareARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FileShareARNList = FileShareARNList (Array FileShareARN)
derive instance newtypeFileShareARNList :: Newtype FileShareARNList _
derive instance repGenericFileShareARNList :: Generic FileShareARNList _
instance showFileShareARNList :: Show FileShareARNList where
  show = genericShow
instance decodeFileShareARNList :: Decode FileShareARNList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileShareARNList :: Encode FileShareARNList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks. </p>
newtype FileShareClientList = FileShareClientList (Array IPV4AddressCIDR)
derive instance newtypeFileShareClientList :: Newtype FileShareClientList _
derive instance repGenericFileShareClientList :: Generic FileShareClientList _
instance showFileShareClientList :: Show FileShareClientList where
  show = genericShow
instance decodeFileShareClientList :: Decode FileShareClientList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileShareClientList :: Encode FileShareClientList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ID of the file share. </p>
newtype FileShareId = FileShareId String
derive instance newtypeFileShareId :: Newtype FileShareId _
derive instance repGenericFileShareId :: Generic FileShareId _
instance showFileShareId :: Show FileShareId where
  show = genericShow
instance decodeFileShareId :: Decode FileShareId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileShareId :: Encode FileShareId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a file share.</p>
newtype FileShareInfo = FileShareInfo 
  { "FileShareARN" :: NullOrUndefined.NullOrUndefined (FileShareARN)
  , "FileShareId" :: NullOrUndefined.NullOrUndefined (FileShareId)
  , "FileShareStatus" :: NullOrUndefined.NullOrUndefined (FileShareStatus)
  , "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeFileShareInfo :: Newtype FileShareInfo _
derive instance repGenericFileShareInfo :: Generic FileShareInfo _
instance showFileShareInfo :: Show FileShareInfo where
  show = genericShow
instance decodeFileShareInfo :: Decode FileShareInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileShareInfo :: Encode FileShareInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FileShareInfoList = FileShareInfoList (Array FileShareInfo)
derive instance newtypeFileShareInfoList :: Newtype FileShareInfoList _
derive instance repGenericFileShareInfoList :: Generic FileShareInfoList _
instance showFileShareInfoList :: Show FileShareInfoList where
  show = genericShow
instance decodeFileShareInfoList :: Decode FileShareInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileShareInfoList :: Encode FileShareInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The status of the file share. Possible values are CREATING, UPDATING, AVAILABLE and DELETING. </p>
newtype FileShareStatus = FileShareStatus String
derive instance newtypeFileShareStatus :: Newtype FileShareStatus _
derive instance repGenericFileShareStatus :: Generic FileShareStatus _
instance showFileShareStatus :: Show FileShareStatus where
  show = genericShow
instance decodeFileShareStatus :: Decode FileShareStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileShareStatus :: Encode FileShareStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon Resource Name (ARN) of the gateway. Use the <a>ListGateways</a> operation to return a list of gateways for your account and region.</p>
newtype GatewayARN = GatewayARN String
derive instance newtypeGatewayARN :: Newtype GatewayARN _
derive instance repGenericGatewayARN :: Generic GatewayARN _
instance showGatewayARN :: Show GatewayARN where
  show = genericShow
instance decodeGatewayARN :: Decode GatewayARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayARN :: Encode GatewayARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GatewayId = GatewayId String
derive instance newtypeGatewayId :: Newtype GatewayId _
derive instance repGenericGatewayId :: Generic GatewayId _
instance showGatewayId :: Show GatewayId where
  show = genericShow
instance decodeGatewayId :: Decode GatewayId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayId :: Encode GatewayId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a gateway object.</p>
newtype GatewayInfo = GatewayInfo 
  { "GatewayId" :: NullOrUndefined.NullOrUndefined (GatewayId)
  , "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "GatewayType" :: NullOrUndefined.NullOrUndefined (GatewayType)
  , "GatewayOperationalState" :: NullOrUndefined.NullOrUndefined (GatewayOperationalState)
  , "GatewayName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGatewayInfo :: Newtype GatewayInfo _
derive instance repGenericGatewayInfo :: Generic GatewayInfo _
instance showGatewayInfo :: Show GatewayInfo where
  show = genericShow
instance decodeGatewayInfo :: Decode GatewayInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayInfo :: Encode GatewayInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name you configured for your gateway.</p>
newtype GatewayName = GatewayName String
derive instance newtypeGatewayName :: Newtype GatewayName _
derive instance repGenericGatewayName :: Generic GatewayName _
instance showGatewayName :: Show GatewayName where
  show = genericShow
instance decodeGatewayName :: Decode GatewayName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayName :: Encode GatewayName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GatewayNetworkInterfaces = GatewayNetworkInterfaces (Array NetworkInterface)
derive instance newtypeGatewayNetworkInterfaces :: Newtype GatewayNetworkInterfaces _
derive instance repGenericGatewayNetworkInterfaces :: Generic GatewayNetworkInterfaces _
instance showGatewayNetworkInterfaces :: Show GatewayNetworkInterfaces where
  show = genericShow
instance decodeGatewayNetworkInterfaces :: Decode GatewayNetworkInterfaces where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayNetworkInterfaces :: Encode GatewayNetworkInterfaces where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GatewayOperationalState = GatewayOperationalState String
derive instance newtypeGatewayOperationalState :: Newtype GatewayOperationalState _
derive instance repGenericGatewayOperationalState :: Generic GatewayOperationalState _
instance showGatewayOperationalState :: Show GatewayOperationalState where
  show = genericShow
instance decodeGatewayOperationalState :: Decode GatewayOperationalState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayOperationalState :: Encode GatewayOperationalState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GatewayState = GatewayState String
derive instance newtypeGatewayState :: Newtype GatewayState _
derive instance repGenericGatewayState :: Generic GatewayState _
instance showGatewayState :: Show GatewayState where
  show = genericShow
instance decodeGatewayState :: Decode GatewayState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayState :: Encode GatewayState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GatewayTimezone = GatewayTimezone String
derive instance newtypeGatewayTimezone :: Newtype GatewayTimezone _
derive instance repGenericGatewayTimezone :: Generic GatewayTimezone _
instance showGatewayTimezone :: Show GatewayTimezone where
  show = genericShow
instance decodeGatewayTimezone :: Decode GatewayTimezone where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayTimezone :: Encode GatewayTimezone where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GatewayType = GatewayType String
derive instance newtypeGatewayType :: Newtype GatewayType _
derive instance repGenericGatewayType :: Generic GatewayType _
instance showGatewayType :: Show GatewayType where
  show = genericShow
instance decodeGatewayType :: Decode GatewayType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayType :: Encode GatewayType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Gateways = Gateways (Array GatewayInfo)
derive instance newtypeGateways :: Newtype Gateways _
derive instance repGenericGateways :: Generic Gateways _
instance showGateways :: Show Gateways where
  show = genericShow
instance decodeGateways :: Decode Gateways where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGateways :: Encode Gateways where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HourOfDay = HourOfDay Int
derive instance newtypeHourOfDay :: Newtype HourOfDay _
derive instance repGenericHourOfDay :: Generic HourOfDay _
instance showHourOfDay :: Show HourOfDay where
  show = genericShow
instance decodeHourOfDay :: Decode HourOfDay where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHourOfDay :: Encode HourOfDay where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPV4AddressCIDR = IPV4AddressCIDR String
derive instance newtypeIPV4AddressCIDR :: Newtype IPV4AddressCIDR _
derive instance repGenericIPV4AddressCIDR :: Generic IPV4AddressCIDR _
instance showIPV4AddressCIDR :: Show IPV4AddressCIDR where
  show = genericShow
instance decodeIPV4AddressCIDR :: Decode IPV4AddressCIDR where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPV4AddressCIDR :: Encode IPV4AddressCIDR where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Initiator = Initiator String
derive instance newtypeInitiator :: Newtype Initiator _
derive instance repGenericInitiator :: Generic Initiator _
instance showInitiator :: Show Initiator where
  show = genericShow
instance decodeInitiator :: Decode Initiator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiator :: Encode Initiator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Initiators = Initiators (Array Initiator)
derive instance newtypeInitiators :: Newtype Initiators _
derive instance repGenericInitiators :: Generic Initiators _
instance showInitiators :: Show Initiators where
  show = genericShow
instance decodeInitiators :: Decode Initiators where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiators :: Encode Initiators where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An internal server error has occurred during the request. For more information, see the error and message fields.</p>
newtype InternalServerError = InternalServerError 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  , "Error'" :: NullOrUndefined.NullOrUndefined (StorageGatewayError)
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _
derive instance repGenericInternalServerError :: Generic InternalServerError _
instance showInternalServerError :: Show InternalServerError where
  show = genericShow
instance decodeInternalServerError :: Decode InternalServerError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerError :: Encode InternalServerError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An exception occurred because an invalid gateway request was issued to the service. For more information, see the error and message fields.</p>
newtype InvalidGatewayRequestException = InvalidGatewayRequestException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  , "Error'" :: NullOrUndefined.NullOrUndefined (StorageGatewayError)
  }
derive instance newtypeInvalidGatewayRequestException :: Newtype InvalidGatewayRequestException _
derive instance repGenericInvalidGatewayRequestException :: Generic InvalidGatewayRequestException _
instance showInvalidGatewayRequestException :: Show InvalidGatewayRequestException where
  show = genericShow
instance decodeInvalidGatewayRequestException :: Decode InvalidGatewayRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidGatewayRequestException :: Encode InvalidGatewayRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IqnName = IqnName String
derive instance newtypeIqnName :: Newtype IqnName _
derive instance repGenericIqnName :: Generic IqnName _
instance showIqnName :: Show IqnName where
  show = genericShow
instance decodeIqnName :: Decode IqnName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIqnName :: Encode IqnName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ARN of the KMS key used for Amazon S3 server side encryption. </p>
newtype KMSKey = KMSKey String
derive instance newtypeKMSKey :: Newtype KMSKey _
derive instance repGenericKMSKey :: Generic KMSKey _
instance showKMSKey :: Show KMSKey where
  show = genericShow
instance decodeKMSKey :: Decode KMSKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKMSKey :: Encode KMSKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastSoftwareUpdate = LastSoftwareUpdate String
derive instance newtypeLastSoftwareUpdate :: Newtype LastSoftwareUpdate _
derive instance repGenericLastSoftwareUpdate :: Generic LastSoftwareUpdate _
instance showLastSoftwareUpdate :: Show LastSoftwareUpdate where
  show = genericShow
instance decodeLastSoftwareUpdate :: Decode LastSoftwareUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastSoftwareUpdate :: Encode LastSoftwareUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>ListFileShareInput</p>
newtype ListFileSharesInput = ListFileSharesInput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntObject)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListFileSharesInput :: Newtype ListFileSharesInput _
derive instance repGenericListFileSharesInput :: Generic ListFileSharesInput _
instance showListFileSharesInput :: Show ListFileSharesInput where
  show = genericShow
instance decodeListFileSharesInput :: Decode ListFileSharesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFileSharesInput :: Encode ListFileSharesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>ListFileShareOutput</p>
newtype ListFileSharesOutput = ListFileSharesOutput 
  { "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "FileShareInfoList" :: NullOrUndefined.NullOrUndefined (FileShareInfoList)
  }
derive instance newtypeListFileSharesOutput :: Newtype ListFileSharesOutput _
derive instance repGenericListFileSharesOutput :: Generic ListFileSharesOutput _
instance showListFileSharesOutput :: Show ListFileSharesOutput where
  show = genericShow
instance decodeListFileSharesOutput :: Decode ListFileSharesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFileSharesOutput :: Encode ListFileSharesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing zero or more of the following fields:</p> <ul> <li> <p> <a>ListGatewaysInput$Limit</a> </p> </li> <li> <p> <a>ListGatewaysInput$Marker</a> </p> </li> </ul>
newtype ListGatewaysInput = ListGatewaysInput 
  { "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeListGatewaysInput :: Newtype ListGatewaysInput _
derive instance repGenericListGatewaysInput :: Generic ListGatewaysInput _
instance showListGatewaysInput :: Show ListGatewaysInput where
  show = genericShow
instance decodeListGatewaysInput :: Decode ListGatewaysInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGatewaysInput :: Encode ListGatewaysInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGatewaysOutput = ListGatewaysOutput 
  { "Gateways" :: NullOrUndefined.NullOrUndefined (Gateways)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListGatewaysOutput :: Newtype ListGatewaysOutput _
derive instance repGenericListGatewaysOutput :: Generic ListGatewaysOutput _
instance showListGatewaysOutput :: Show ListGatewaysOutput where
  show = genericShow
instance decodeListGatewaysOutput :: Decode ListGatewaysOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGatewaysOutput :: Encode ListGatewaysOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway.</p>
newtype ListLocalDisksInput = ListLocalDisksInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeListLocalDisksInput :: Newtype ListLocalDisksInput _
derive instance repGenericListLocalDisksInput :: Generic ListLocalDisksInput _
instance showListLocalDisksInput :: Show ListLocalDisksInput where
  show = genericShow
instance decodeListLocalDisksInput :: Decode ListLocalDisksInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListLocalDisksInput :: Encode ListLocalDisksInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListLocalDisksOutput = ListLocalDisksOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "Disks" :: NullOrUndefined.NullOrUndefined (Disks)
  }
derive instance newtypeListLocalDisksOutput :: Newtype ListLocalDisksOutput _
derive instance repGenericListLocalDisksOutput :: Generic ListLocalDisksOutput _
instance showListLocalDisksOutput :: Show ListLocalDisksOutput where
  show = genericShow
instance decodeListLocalDisksOutput :: Decode ListLocalDisksOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListLocalDisksOutput :: Encode ListLocalDisksOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>ListTagsForResourceInput</p>
newtype ListTagsForResourceInput = ListTagsForResourceInput 
  { "ResourceARN" :: (ResourceARN)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeListTagsForResourceInput :: Newtype ListTagsForResourceInput _
derive instance repGenericListTagsForResourceInput :: Generic ListTagsForResourceInput _
instance showListTagsForResourceInput :: Show ListTagsForResourceInput where
  show = genericShow
instance decodeListTagsForResourceInput :: Decode ListTagsForResourceInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceInput :: Encode ListTagsForResourceInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>ListTagsForResourceOutput</p>
newtype ListTagsForResourceOutput = ListTagsForResourceOutput 
  { "ResourceARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  }
derive instance newtypeListTagsForResourceOutput :: Newtype ListTagsForResourceOutput _
derive instance repGenericListTagsForResourceOutput :: Generic ListTagsForResourceOutput _
instance showListTagsForResourceOutput :: Show ListTagsForResourceOutput where
  show = genericShow
instance decodeListTagsForResourceOutput :: Decode ListTagsForResourceOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceOutput :: Encode ListTagsForResourceOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object that contains one or more of the following fields:</p> <ul> <li> <p> <a>ListTapesInput$Limit</a> </p> </li> <li> <p> <a>ListTapesInput$Marker</a> </p> </li> <li> <p> <a>ListTapesInput$TapeARNs</a> </p> </li> </ul>
newtype ListTapesInput = ListTapesInput 
  { "TapeARNs" :: NullOrUndefined.NullOrUndefined (TapeARNs)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeListTapesInput :: Newtype ListTapesInput _
derive instance repGenericListTapesInput :: Generic ListTapesInput _
instance showListTapesInput :: Show ListTapesInput where
  show = genericShow
instance decodeListTapesInput :: Decode ListTapesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTapesInput :: Encode ListTapesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the following fields:</p> <ul> <li> <p> <a>ListTapesOutput$Marker</a> </p> </li> <li> <p> <a>ListTapesOutput$VolumeInfos</a> </p> </li> </ul>
newtype ListTapesOutput = ListTapesOutput 
  { "TapeInfos" :: NullOrUndefined.NullOrUndefined (TapeInfos)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListTapesOutput :: Newtype ListTapesOutput _
derive instance repGenericListTapesOutput :: Generic ListTapesOutput _
instance showListTapesOutput :: Show ListTapesOutput where
  show = genericShow
instance decodeListTapesOutput :: Decode ListTapesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTapesOutput :: Encode ListTapesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>ListVolumeInitiatorsInput</p>
newtype ListVolumeInitiatorsInput = ListVolumeInitiatorsInput 
  { "VolumeARN" :: (VolumeARN)
  }
derive instance newtypeListVolumeInitiatorsInput :: Newtype ListVolumeInitiatorsInput _
derive instance repGenericListVolumeInitiatorsInput :: Generic ListVolumeInitiatorsInput _
instance showListVolumeInitiatorsInput :: Show ListVolumeInitiatorsInput where
  show = genericShow
instance decodeListVolumeInitiatorsInput :: Decode ListVolumeInitiatorsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListVolumeInitiatorsInput :: Encode ListVolumeInitiatorsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>ListVolumeInitiatorsOutput</p>
newtype ListVolumeInitiatorsOutput = ListVolumeInitiatorsOutput 
  { "Initiators" :: NullOrUndefined.NullOrUndefined (Initiators)
  }
derive instance newtypeListVolumeInitiatorsOutput :: Newtype ListVolumeInitiatorsOutput _
derive instance repGenericListVolumeInitiatorsOutput :: Generic ListVolumeInitiatorsOutput _
instance showListVolumeInitiatorsOutput :: Show ListVolumeInitiatorsOutput where
  show = genericShow
instance decodeListVolumeInitiatorsOutput :: Decode ListVolumeInitiatorsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListVolumeInitiatorsOutput :: Encode ListVolumeInitiatorsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListVolumeRecoveryPointsInput = ListVolumeRecoveryPointsInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeListVolumeRecoveryPointsInput :: Newtype ListVolumeRecoveryPointsInput _
derive instance repGenericListVolumeRecoveryPointsInput :: Generic ListVolumeRecoveryPointsInput _
instance showListVolumeRecoveryPointsInput :: Show ListVolumeRecoveryPointsInput where
  show = genericShow
instance decodeListVolumeRecoveryPointsInput :: Decode ListVolumeRecoveryPointsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListVolumeRecoveryPointsInput :: Encode ListVolumeRecoveryPointsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListVolumeRecoveryPointsOutput = ListVolumeRecoveryPointsOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "VolumeRecoveryPointInfos" :: NullOrUndefined.NullOrUndefined (VolumeRecoveryPointInfos)
  }
derive instance newtypeListVolumeRecoveryPointsOutput :: Newtype ListVolumeRecoveryPointsOutput _
derive instance repGenericListVolumeRecoveryPointsOutput :: Generic ListVolumeRecoveryPointsOutput _
instance showListVolumeRecoveryPointsOutput :: Show ListVolumeRecoveryPointsOutput where
  show = genericShow
instance decodeListVolumeRecoveryPointsOutput :: Decode ListVolumeRecoveryPointsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListVolumeRecoveryPointsOutput :: Encode ListVolumeRecoveryPointsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object that contains one or more of the following fields:</p> <ul> <li> <p> <a>ListVolumesInput$Limit</a> </p> </li> <li> <p> <a>ListVolumesInput$Marker</a> </p> </li> </ul>
newtype ListVolumesInput = ListVolumesInput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeListVolumesInput :: Newtype ListVolumesInput _
derive instance repGenericListVolumesInput :: Generic ListVolumesInput _
instance showListVolumesInput :: Show ListVolumesInput where
  show = genericShow
instance decodeListVolumesInput :: Decode ListVolumesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListVolumesInput :: Encode ListVolumesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListVolumesOutput = ListVolumesOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "VolumeInfos" :: NullOrUndefined.NullOrUndefined (VolumeInfos)
  }
derive instance newtypeListVolumesOutput :: Newtype ListVolumesOutput _
derive instance repGenericListVolumesOutput :: Generic ListVolumesOutput _
instance showListVolumesOutput :: Show ListVolumesOutput where
  show = genericShow
instance decodeListVolumesOutput :: Decode ListVolumesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListVolumesOutput :: Encode ListVolumesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LocalConsolePassword = LocalConsolePassword String
derive instance newtypeLocalConsolePassword :: Newtype LocalConsolePassword _
derive instance repGenericLocalConsolePassword :: Generic LocalConsolePassword _
instance showLocalConsolePassword :: Show LocalConsolePassword where
  show = genericShow
instance decodeLocalConsolePassword :: Decode LocalConsolePassword where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocalConsolePassword :: Encode LocalConsolePassword where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ARN of the backend storage used for storing file data. </p>
newtype LocationARN = LocationARN String
derive instance newtypeLocationARN :: Newtype LocationARN _
derive instance repGenericLocationARN :: Generic LocationARN _
instance showLocationARN :: Show LocationARN where
  show = genericShow
instance decodeLocationARN :: Decode LocationARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocationARN :: Encode LocationARN where
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


newtype MediumChangerType = MediumChangerType String
derive instance newtypeMediumChangerType :: Newtype MediumChangerType _
derive instance repGenericMediumChangerType :: Generic MediumChangerType _
instance showMediumChangerType :: Show MediumChangerType where
  show = genericShow
instance decodeMediumChangerType :: Decode MediumChangerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMediumChangerType :: Encode MediumChangerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MinuteOfHour = MinuteOfHour Int
derive instance newtypeMinuteOfHour :: Newtype MinuteOfHour _
derive instance repGenericMinuteOfHour :: Generic MinuteOfHour _
instance showMinuteOfHour :: Show MinuteOfHour where
  show = genericShow
instance decodeMinuteOfHour :: Decode MinuteOfHour where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMinuteOfHour :: Encode MinuteOfHour where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes file share default values. Files and folders stored as Amazon S3 objects in S3 buckets don't, by default, have Unix file permissions assigned to them. Upon discovery in an S3 bucket by Storage Gateway, the S3 objects that represent files and folders are assigned these default Unix permissions. This operation is only supported in the file gateway type.</p>
newtype NFSFileShareDefaults = NFSFileShareDefaults 
  { "FileMode" :: NullOrUndefined.NullOrUndefined (PermissionMode)
  , "DirectoryMode" :: NullOrUndefined.NullOrUndefined (PermissionMode)
  , "GroupId" :: NullOrUndefined.NullOrUndefined (PermissionId)
  , "OwnerId" :: NullOrUndefined.NullOrUndefined (PermissionId)
  }
derive instance newtypeNFSFileShareDefaults :: Newtype NFSFileShareDefaults _
derive instance repGenericNFSFileShareDefaults :: Generic NFSFileShareDefaults _
instance showNFSFileShareDefaults :: Show NFSFileShareDefaults where
  show = genericShow
instance decodeNFSFileShareDefaults :: Decode NFSFileShareDefaults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNFSFileShareDefaults :: Encode NFSFileShareDefaults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Unix file permissions and ownership information assigned, by default, to native S3 objects when file gateway discovers them in S3 buckets. This operation is only supported in file gateways.</p>
newtype NFSFileShareInfo = NFSFileShareInfo 
  { "NFSFileShareDefaults" :: NullOrUndefined.NullOrUndefined (NFSFileShareDefaults)
  , "FileShareARN" :: NullOrUndefined.NullOrUndefined (FileShareARN)
  , "FileShareId" :: NullOrUndefined.NullOrUndefined (FileShareId)
  , "FileShareStatus" :: NullOrUndefined.NullOrUndefined (FileShareStatus)
  , "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "KMSEncrypted" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "KMSKey" :: NullOrUndefined.NullOrUndefined (KMSKey)
  , "Path" :: NullOrUndefined.NullOrUndefined (Path)
  , "Role" :: NullOrUndefined.NullOrUndefined (Role)
  , "LocationARN" :: NullOrUndefined.NullOrUndefined (LocationARN)
  , "DefaultStorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  , "ClientList" :: NullOrUndefined.NullOrUndefined (FileShareClientList)
  , "Squash" :: NullOrUndefined.NullOrUndefined (Squash)
  , "ReadOnly" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "GuessMIMETypeEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeNFSFileShareInfo :: Newtype NFSFileShareInfo _
derive instance repGenericNFSFileShareInfo :: Generic NFSFileShareInfo _
instance showNFSFileShareInfo :: Show NFSFileShareInfo where
  show = genericShow
instance decodeNFSFileShareInfo :: Decode NFSFileShareInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNFSFileShareInfo :: Encode NFSFileShareInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NFSFileShareInfoList = NFSFileShareInfoList (Array NFSFileShareInfo)
derive instance newtypeNFSFileShareInfoList :: Newtype NFSFileShareInfoList _
derive instance repGenericNFSFileShareInfoList :: Generic NFSFileShareInfoList _
instance showNFSFileShareInfoList :: Show NFSFileShareInfoList where
  show = genericShow
instance decodeNFSFileShareInfoList :: Decode NFSFileShareInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNFSFileShareInfoList :: Encode NFSFileShareInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a gateway's network interface.</p>
newtype NetworkInterface = NetworkInterface 
  { "Ipv4Address" :: NullOrUndefined.NullOrUndefined (String)
  , "MacAddress" :: NullOrUndefined.NullOrUndefined (String)
  , "Ipv6Address" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNetworkInterface :: Newtype NetworkInterface _
derive instance repGenericNetworkInterface :: Generic NetworkInterface _
instance showNetworkInterface :: Show NetworkInterface where
  show = genericShow
instance decodeNetworkInterface :: Decode NetworkInterface where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkInterface :: Encode NetworkInterface where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NetworkInterfaceId = NetworkInterfaceId String
derive instance newtypeNetworkInterfaceId :: Newtype NetworkInterfaceId _
derive instance repGenericNetworkInterfaceId :: Generic NetworkInterfaceId _
instance showNetworkInterfaceId :: Show NetworkInterfaceId where
  show = genericShow
instance decodeNetworkInterfaceId :: Decode NetworkInterfaceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkInterfaceId :: Encode NetworkInterfaceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextUpdateAvailabilityDate = NextUpdateAvailabilityDate String
derive instance newtypeNextUpdateAvailabilityDate :: Newtype NextUpdateAvailabilityDate _
derive instance repGenericNextUpdateAvailabilityDate :: Generic NextUpdateAvailabilityDate _
instance showNextUpdateAvailabilityDate :: Show NextUpdateAvailabilityDate where
  show = genericShow
instance decodeNextUpdateAvailabilityDate :: Decode NextUpdateAvailabilityDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextUpdateAvailabilityDate :: Encode NextUpdateAvailabilityDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The randomly generated ID of the notification that was sent. This ID is in UUID format.</p>
newtype NotificationId = NotificationId String
derive instance newtypeNotificationId :: Newtype NotificationId _
derive instance repGenericNotificationId :: Generic NotificationId _
instance showNotificationId :: Show NotificationId where
  show = genericShow
instance decodeNotificationId :: Decode NotificationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationId :: Encode NotificationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NotifyWhenUploadedInput = NotifyWhenUploadedInput 
  { "FileShareARN" :: (FileShareARN)
  }
derive instance newtypeNotifyWhenUploadedInput :: Newtype NotifyWhenUploadedInput _
derive instance repGenericNotifyWhenUploadedInput :: Generic NotifyWhenUploadedInput _
instance showNotifyWhenUploadedInput :: Show NotifyWhenUploadedInput where
  show = genericShow
instance decodeNotifyWhenUploadedInput :: Decode NotifyWhenUploadedInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotifyWhenUploadedInput :: Encode NotifyWhenUploadedInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NotifyWhenUploadedOutput = NotifyWhenUploadedOutput 
  { "FileShareARN" :: NullOrUndefined.NullOrUndefined (FileShareARN)
  , "NotificationId" :: NullOrUndefined.NullOrUndefined (NotificationId)
  }
derive instance newtypeNotifyWhenUploadedOutput :: Newtype NotifyWhenUploadedOutput _
derive instance repGenericNotifyWhenUploadedOutput :: Generic NotifyWhenUploadedOutput _
instance showNotifyWhenUploadedOutput :: Show NotifyWhenUploadedOutput where
  show = genericShow
instance decodeNotifyWhenUploadedOutput :: Decode NotifyWhenUploadedOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotifyWhenUploadedOutput :: Encode NotifyWhenUploadedOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NumTapesToCreate = NumTapesToCreate Int
derive instance newtypeNumTapesToCreate :: Newtype NumTapesToCreate _
derive instance repGenericNumTapesToCreate :: Generic NumTapesToCreate _
instance showNumTapesToCreate :: Show NumTapesToCreate where
  show = genericShow
instance decodeNumTapesToCreate :: Decode NumTapesToCreate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNumTapesToCreate :: Encode NumTapesToCreate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The file share path used by the NFS client to identify the mount point. </p>
newtype Path = Path String
derive instance newtypePath :: Newtype Path _
derive instance repGenericPath :: Generic Path _
instance showPath :: Show Path where
  show = genericShow
instance decodePath :: Decode Path where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePath :: Encode Path where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PermissionId = PermissionId Number
derive instance newtypePermissionId :: Newtype PermissionId _
derive instance repGenericPermissionId :: Generic PermissionId _
instance showPermissionId :: Show PermissionId where
  show = genericShow
instance decodePermissionId :: Decode PermissionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePermissionId :: Encode PermissionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PermissionMode = PermissionMode String
derive instance newtypePermissionMode :: Newtype PermissionMode _
derive instance repGenericPermissionMode :: Generic PermissionMode _
instance showPermissionMode :: Show PermissionMode where
  show = genericShow
instance decodePermissionMode :: Decode PermissionMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePermissionMode :: Encode PermissionMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PositiveIntObject = PositiveIntObject Int
derive instance newtypePositiveIntObject :: Newtype PositiveIntObject _
derive instance repGenericPositiveIntObject :: Generic PositiveIntObject _
instance showPositiveIntObject :: Show PositiveIntObject where
  show = genericShow
instance decodePositiveIntObject :: Decode PositiveIntObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePositiveIntObject :: Encode PositiveIntObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecurrenceInHours = RecurrenceInHours Int
derive instance newtypeRecurrenceInHours :: Newtype RecurrenceInHours _
derive instance repGenericRecurrenceInHours :: Generic RecurrenceInHours _
instance showRecurrenceInHours :: Show RecurrenceInHours where
  show = genericShow
instance decodeRecurrenceInHours :: Decode RecurrenceInHours where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecurrenceInHours :: Encode RecurrenceInHours where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RefreshCacheInput = RefreshCacheInput 
  { "FileShareARN" :: (FileShareARN)
  }
derive instance newtypeRefreshCacheInput :: Newtype RefreshCacheInput _
derive instance repGenericRefreshCacheInput :: Generic RefreshCacheInput _
instance showRefreshCacheInput :: Show RefreshCacheInput where
  show = genericShow
instance decodeRefreshCacheInput :: Decode RefreshCacheInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRefreshCacheInput :: Encode RefreshCacheInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RefreshCacheOutput = RefreshCacheOutput 
  { "FileShareARN" :: NullOrUndefined.NullOrUndefined (FileShareARN)
  }
derive instance newtypeRefreshCacheOutput :: Newtype RefreshCacheOutput _
derive instance repGenericRefreshCacheOutput :: Generic RefreshCacheOutput _
instance showRefreshCacheOutput :: Show RefreshCacheOutput where
  show = genericShow
instance decodeRefreshCacheOutput :: Decode RefreshCacheOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRefreshCacheOutput :: Encode RefreshCacheOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegionId = RegionId String
derive instance newtypeRegionId :: Newtype RegionId _
derive instance repGenericRegionId :: Generic RegionId _
instance showRegionId :: Show RegionId where
  show = genericShow
instance decodeRegionId :: Decode RegionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegionId :: Encode RegionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>RemoveTagsFromResourceInput</p>
newtype RemoveTagsFromResourceInput = RemoveTagsFromResourceInput 
  { "ResourceARN" :: (ResourceARN)
  , "TagKeys" :: (TagKeys)
  }
derive instance newtypeRemoveTagsFromResourceInput :: Newtype RemoveTagsFromResourceInput _
derive instance repGenericRemoveTagsFromResourceInput :: Generic RemoveTagsFromResourceInput _
instance showRemoveTagsFromResourceInput :: Show RemoveTagsFromResourceInput where
  show = genericShow
instance decodeRemoveTagsFromResourceInput :: Decode RemoveTagsFromResourceInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsFromResourceInput :: Encode RemoveTagsFromResourceInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>RemoveTagsFromResourceOutput</p>
newtype RemoveTagsFromResourceOutput = RemoveTagsFromResourceOutput 
  { "ResourceARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  }
derive instance newtypeRemoveTagsFromResourceOutput :: Newtype RemoveTagsFromResourceOutput _
derive instance repGenericRemoveTagsFromResourceOutput :: Generic RemoveTagsFromResourceOutput _
instance showRemoveTagsFromResourceOutput :: Show RemoveTagsFromResourceOutput where
  show = genericShow
instance decodeRemoveTagsFromResourceOutput :: Decode RemoveTagsFromResourceOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsFromResourceOutput :: Encode RemoveTagsFromResourceOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResetCacheInput = ResetCacheInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeResetCacheInput :: Newtype ResetCacheInput _
derive instance repGenericResetCacheInput :: Generic ResetCacheInput _
instance showResetCacheInput :: Show ResetCacheInput where
  show = genericShow
instance decodeResetCacheInput :: Decode ResetCacheInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResetCacheInput :: Encode ResetCacheInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResetCacheOutput = ResetCacheOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeResetCacheOutput :: Newtype ResetCacheOutput _
derive instance repGenericResetCacheOutput :: Generic ResetCacheOutput _
instance showResetCacheOutput :: Show ResetCacheOutput where
  show = genericShow
instance decodeResetCacheOutput :: Decode ResetCacheOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResetCacheOutput :: Encode ResetCacheOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceARN = ResourceARN String
derive instance newtypeResourceARN :: Newtype ResourceARN _
derive instance repGenericResourceARN :: Generic ResourceARN _
instance showResourceARN :: Show ResourceARN where
  show = genericShow
instance decodeResourceARN :: Decode ResourceARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceARN :: Encode ResourceARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>RetrieveTapeArchiveInput</p>
newtype RetrieveTapeArchiveInput = RetrieveTapeArchiveInput 
  { "TapeARN" :: (TapeARN)
  , "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeRetrieveTapeArchiveInput :: Newtype RetrieveTapeArchiveInput _
derive instance repGenericRetrieveTapeArchiveInput :: Generic RetrieveTapeArchiveInput _
instance showRetrieveTapeArchiveInput :: Show RetrieveTapeArchiveInput where
  show = genericShow
instance decodeRetrieveTapeArchiveInput :: Decode RetrieveTapeArchiveInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetrieveTapeArchiveInput :: Encode RetrieveTapeArchiveInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>RetrieveTapeArchiveOutput</p>
newtype RetrieveTapeArchiveOutput = RetrieveTapeArchiveOutput 
  { "TapeARN" :: NullOrUndefined.NullOrUndefined (TapeARN)
  }
derive instance newtypeRetrieveTapeArchiveOutput :: Newtype RetrieveTapeArchiveOutput _
derive instance repGenericRetrieveTapeArchiveOutput :: Generic RetrieveTapeArchiveOutput _
instance showRetrieveTapeArchiveOutput :: Show RetrieveTapeArchiveOutput where
  show = genericShow
instance decodeRetrieveTapeArchiveOutput :: Decode RetrieveTapeArchiveOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetrieveTapeArchiveOutput :: Encode RetrieveTapeArchiveOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>RetrieveTapeRecoveryPointInput</p>
newtype RetrieveTapeRecoveryPointInput = RetrieveTapeRecoveryPointInput 
  { "TapeARN" :: (TapeARN)
  , "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeRetrieveTapeRecoveryPointInput :: Newtype RetrieveTapeRecoveryPointInput _
derive instance repGenericRetrieveTapeRecoveryPointInput :: Generic RetrieveTapeRecoveryPointInput _
instance showRetrieveTapeRecoveryPointInput :: Show RetrieveTapeRecoveryPointInput where
  show = genericShow
instance decodeRetrieveTapeRecoveryPointInput :: Decode RetrieveTapeRecoveryPointInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetrieveTapeRecoveryPointInput :: Encode RetrieveTapeRecoveryPointInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>RetrieveTapeRecoveryPointOutput</p>
newtype RetrieveTapeRecoveryPointOutput = RetrieveTapeRecoveryPointOutput 
  { "TapeARN" :: NullOrUndefined.NullOrUndefined (TapeARN)
  }
derive instance newtypeRetrieveTapeRecoveryPointOutput :: Newtype RetrieveTapeRecoveryPointOutput _
derive instance repGenericRetrieveTapeRecoveryPointOutput :: Generic RetrieveTapeRecoveryPointOutput _
instance showRetrieveTapeRecoveryPointOutput :: Show RetrieveTapeRecoveryPointOutput where
  show = genericShow
instance decodeRetrieveTapeRecoveryPointOutput :: Decode RetrieveTapeRecoveryPointOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetrieveTapeRecoveryPointOutput :: Encode RetrieveTapeRecoveryPointOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ARN of the IAM role that file gateway assumes when it accesses the underlying storage. </p>
newtype Role = Role String
derive instance newtypeRole :: Newtype Role _
derive instance repGenericRole :: Generic Role _
instance showRole :: Show Role where
  show = genericShow
instance decodeRole :: Decode Role where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRole :: Encode Role where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An internal server error has occurred because the service is unavailable. For more information, see the error and message fields.</p>
newtype ServiceUnavailableError = ServiceUnavailableError 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  , "Error'" :: NullOrUndefined.NullOrUndefined (StorageGatewayError)
  }
derive instance newtypeServiceUnavailableError :: Newtype ServiceUnavailableError _
derive instance repGenericServiceUnavailableError :: Generic ServiceUnavailableError _
instance showServiceUnavailableError :: Show ServiceUnavailableError where
  show = genericShow
instance decodeServiceUnavailableError :: Decode ServiceUnavailableError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUnavailableError :: Encode ServiceUnavailableError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>SetLocalConsolePasswordInput</p>
newtype SetLocalConsolePasswordInput = SetLocalConsolePasswordInput 
  { "GatewayARN" :: (GatewayARN)
  , "LocalConsolePassword" :: (LocalConsolePassword)
  }
derive instance newtypeSetLocalConsolePasswordInput :: Newtype SetLocalConsolePasswordInput _
derive instance repGenericSetLocalConsolePasswordInput :: Generic SetLocalConsolePasswordInput _
instance showSetLocalConsolePasswordInput :: Show SetLocalConsolePasswordInput where
  show = genericShow
instance decodeSetLocalConsolePasswordInput :: Decode SetLocalConsolePasswordInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetLocalConsolePasswordInput :: Encode SetLocalConsolePasswordInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetLocalConsolePasswordOutput = SetLocalConsolePasswordOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeSetLocalConsolePasswordOutput :: Newtype SetLocalConsolePasswordOutput _
derive instance repGenericSetLocalConsolePasswordOutput :: Generic SetLocalConsolePasswordOutput _
instance showSetLocalConsolePasswordOutput :: Show SetLocalConsolePasswordOutput where
  show = genericShow
instance decodeSetLocalConsolePasswordOutput :: Decode SetLocalConsolePasswordOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetLocalConsolePasswordOutput :: Encode SetLocalConsolePasswordOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway to shut down.</p>
newtype ShutdownGatewayInput = ShutdownGatewayInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeShutdownGatewayInput :: Newtype ShutdownGatewayInput _
derive instance repGenericShutdownGatewayInput :: Generic ShutdownGatewayInput _
instance showShutdownGatewayInput :: Show ShutdownGatewayInput where
  show = genericShow
instance decodeShutdownGatewayInput :: Decode ShutdownGatewayInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeShutdownGatewayInput :: Encode ShutdownGatewayInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway that was shut down.</p>
newtype ShutdownGatewayOutput = ShutdownGatewayOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeShutdownGatewayOutput :: Newtype ShutdownGatewayOutput _
derive instance repGenericShutdownGatewayOutput :: Generic ShutdownGatewayOutput _
instance showShutdownGatewayOutput :: Show ShutdownGatewayOutput where
  show = genericShow
instance decodeShutdownGatewayOutput :: Decode ShutdownGatewayOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeShutdownGatewayOutput :: Encode ShutdownGatewayOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnapshotDescription = SnapshotDescription String
derive instance newtypeSnapshotDescription :: Newtype SnapshotDescription _
derive instance repGenericSnapshotDescription :: Generic SnapshotDescription _
instance showSnapshotDescription :: Show SnapshotDescription where
  show = genericShow
instance decodeSnapshotDescription :: Decode SnapshotDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotDescription :: Encode SnapshotDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnapshotId = SnapshotId String
derive instance newtypeSnapshotId :: Newtype SnapshotId _
derive instance repGenericSnapshotId :: Generic SnapshotId _
instance showSnapshotId :: Show SnapshotId where
  show = genericShow
instance decodeSnapshotId :: Decode SnapshotId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotId :: Encode SnapshotId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The user mapped to anonymous user. Valid options are the following: </p> <ul> <li> <p>"RootSquash" - Only root is mapped to anonymous user.</p> </li> <li> <p>"NoSquash" - No one is mapped to anonymous user</p> </li> <li> <p>"AllSquash" - Everyone is mapped to anonymous user.</p> </li> </ul>
newtype Squash = Squash String
derive instance newtypeSquash :: Newtype Squash _
derive instance repGenericSquash :: Generic Squash _
instance showSquash :: Show Squash where
  show = genericShow
instance decodeSquash :: Decode Squash where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSquash :: Encode Squash where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway to start.</p>
newtype StartGatewayInput = StartGatewayInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeStartGatewayInput :: Newtype StartGatewayInput _
derive instance repGenericStartGatewayInput :: Generic StartGatewayInput _
instance showStartGatewayInput :: Show StartGatewayInput where
  show = genericShow
instance decodeStartGatewayInput :: Decode StartGatewayInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartGatewayInput :: Encode StartGatewayInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway that was restarted.</p>
newtype StartGatewayOutput = StartGatewayOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeStartGatewayOutput :: Newtype StartGatewayOutput _
derive instance repGenericStartGatewayOutput :: Generic StartGatewayOutput _
instance showStartGatewayOutput :: Show StartGatewayOutput where
  show = genericShow
instance decodeStartGatewayOutput :: Decode StartGatewayOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartGatewayOutput :: Encode StartGatewayOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype StorageClass = StorageClass String
derive instance newtypeStorageClass :: Newtype StorageClass _
derive instance repGenericStorageClass :: Generic StorageClass _
instance showStorageClass :: Show StorageClass where
  show = genericShow
instance decodeStorageClass :: Decode StorageClass where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageClass :: Encode StorageClass where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides additional information about an error that was returned by the service as an or. See the <code>errorCode</code> and <code>errorDetails</code> members for more information about the error.</p>
newtype StorageGatewayError = StorageGatewayError 
  { "ErrorCode'" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  , "ErrorDetails'" :: NullOrUndefined.NullOrUndefined (ErrorDetails')
  }
derive instance newtypeStorageGatewayError :: Newtype StorageGatewayError _
derive instance repGenericStorageGatewayError :: Generic StorageGatewayError _
instance showStorageGatewayError :: Show StorageGatewayError where
  show = genericShow
instance decodeStorageGatewayError :: Decode StorageGatewayError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageGatewayError :: Encode StorageGatewayError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an iSCSI stored volume.</p>
newtype StorediSCSIVolume = StorediSCSIVolume 
  { "VolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  , "VolumeId" :: NullOrUndefined.NullOrUndefined (VolumeId)
  , "VolumeType" :: NullOrUndefined.NullOrUndefined (VolumeType)
  , "VolumeStatus" :: NullOrUndefined.NullOrUndefined (VolumeStatus)
  , "VolumeSizeInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "VolumeProgress" :: NullOrUndefined.NullOrUndefined (DoubleObject)
  , "VolumeDiskId" :: NullOrUndefined.NullOrUndefined (DiskId)
  , "SourceSnapshotId" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  , "PreservedExistingData" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "VolumeiSCSIAttributes" :: NullOrUndefined.NullOrUndefined (VolumeiSCSIAttributes)
  , "CreatedDate" :: NullOrUndefined.NullOrUndefined (CreatedDate)
  , "VolumeUsedInBytes" :: NullOrUndefined.NullOrUndefined (VolumeUsedInBytes)
  }
derive instance newtypeStorediSCSIVolume :: Newtype StorediSCSIVolume _
derive instance repGenericStorediSCSIVolume :: Generic StorediSCSIVolume _
instance showStorediSCSIVolume :: Show StorediSCSIVolume where
  show = genericShow
instance decodeStorediSCSIVolume :: Decode StorediSCSIVolume where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorediSCSIVolume :: Encode StorediSCSIVolume where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StorediSCSIVolumes = StorediSCSIVolumes (Array StorediSCSIVolume)
derive instance newtypeStorediSCSIVolumes :: Newtype StorediSCSIVolumes _
derive instance repGenericStorediSCSIVolumes :: Generic StorediSCSIVolumes _
instance showStorediSCSIVolumes :: Show StorediSCSIVolumes where
  show = genericShow
instance decodeStorediSCSIVolumes :: Decode StorediSCSIVolumes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorediSCSIVolumes :: Encode StorediSCSIVolumes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
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


newtype TagKeys = TagKeys (Array TagKey)
derive instance newtypeTagKeys :: Newtype TagKeys _
derive instance repGenericTagKeys :: Generic TagKeys _
instance showTagKeys :: Show TagKeys where
  show = genericShow
instance decodeTagKeys :: Decode TagKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeys :: Encode TagKeys where
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


newtype Tags = Tags (Array Tag)
derive instance newtypeTags :: Newtype Tags _
derive instance repGenericTags :: Generic Tags _
instance showTags :: Show Tags where
  show = genericShow
instance decodeTags :: Decode Tags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTags :: Encode Tags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a virtual tape object.</p>
newtype Tape = Tape 
  { "TapeARN" :: NullOrUndefined.NullOrUndefined (TapeARN)
  , "TapeBarcode" :: NullOrUndefined.NullOrUndefined (TapeBarcode)
  , "TapeCreatedDate" :: NullOrUndefined.NullOrUndefined (Time)
  , "TapeSizeInBytes" :: NullOrUndefined.NullOrUndefined (TapeSize)
  , "TapeStatus" :: NullOrUndefined.NullOrUndefined (TapeStatus)
  , "VTLDevice" :: NullOrUndefined.NullOrUndefined (VTLDeviceARN)
  , "Progress" :: NullOrUndefined.NullOrUndefined (DoubleObject)
  , "TapeUsedInBytes" :: NullOrUndefined.NullOrUndefined (TapeUsage)
  }
derive instance newtypeTape :: Newtype Tape _
derive instance repGenericTape :: Generic Tape _
instance showTape :: Show Tape where
  show = genericShow
instance decodeTape :: Decode Tape where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTape :: Encode Tape where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TapeARN = TapeARN String
derive instance newtypeTapeARN :: Newtype TapeARN _
derive instance repGenericTapeARN :: Generic TapeARN _
instance showTapeARN :: Show TapeARN where
  show = genericShow
instance decodeTapeARN :: Decode TapeARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeARN :: Encode TapeARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon Resource Name (ARN) of each of the tapes you want to list. If you don't specify a tape ARN, the response lists all tapes in both your VTL and VTS.</p>
newtype TapeARNs = TapeARNs (Array TapeARN)
derive instance newtypeTapeARNs :: Newtype TapeARNs _
derive instance repGenericTapeARNs :: Generic TapeARNs _
instance showTapeARNs :: Show TapeARNs where
  show = genericShow
instance decodeTapeARNs :: Decode TapeARNs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeARNs :: Encode TapeARNs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a virtual tape that is archived in the virtual tape shelf (VTS).</p>
newtype TapeArchive = TapeArchive 
  { "TapeARN" :: NullOrUndefined.NullOrUndefined (TapeARN)
  , "TapeBarcode" :: NullOrUndefined.NullOrUndefined (TapeBarcode)
  , "TapeCreatedDate" :: NullOrUndefined.NullOrUndefined (Time)
  , "TapeSizeInBytes" :: NullOrUndefined.NullOrUndefined (TapeSize)
  , "CompletionTime" :: NullOrUndefined.NullOrUndefined (Time)
  , "RetrievedTo" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "TapeStatus" :: NullOrUndefined.NullOrUndefined (TapeArchiveStatus)
  , "TapeUsedInBytes" :: NullOrUndefined.NullOrUndefined (TapeUsage)
  }
derive instance newtypeTapeArchive :: Newtype TapeArchive _
derive instance repGenericTapeArchive :: Generic TapeArchive _
instance showTapeArchive :: Show TapeArchive where
  show = genericShow
instance decodeTapeArchive :: Decode TapeArchive where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeArchive :: Encode TapeArchive where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TapeArchiveStatus = TapeArchiveStatus String
derive instance newtypeTapeArchiveStatus :: Newtype TapeArchiveStatus _
derive instance repGenericTapeArchiveStatus :: Generic TapeArchiveStatus _
instance showTapeArchiveStatus :: Show TapeArchiveStatus where
  show = genericShow
instance decodeTapeArchiveStatus :: Decode TapeArchiveStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeArchiveStatus :: Encode TapeArchiveStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TapeArchives = TapeArchives (Array TapeArchive)
derive instance newtypeTapeArchives :: Newtype TapeArchives _
derive instance repGenericTapeArchives :: Generic TapeArchives _
instance showTapeArchives :: Show TapeArchives where
  show = genericShow
instance decodeTapeArchives :: Decode TapeArchives where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeArchives :: Encode TapeArchives where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TapeBarcode = TapeBarcode String
derive instance newtypeTapeBarcode :: Newtype TapeBarcode _
derive instance repGenericTapeBarcode :: Generic TapeBarcode _
instance showTapeBarcode :: Show TapeBarcode where
  show = genericShow
instance decodeTapeBarcode :: Decode TapeBarcode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeBarcode :: Encode TapeBarcode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TapeBarcodePrefix = TapeBarcodePrefix String
derive instance newtypeTapeBarcodePrefix :: Newtype TapeBarcodePrefix _
derive instance repGenericTapeBarcodePrefix :: Generic TapeBarcodePrefix _
instance showTapeBarcodePrefix :: Show TapeBarcodePrefix where
  show = genericShow
instance decodeTapeBarcodePrefix :: Decode TapeBarcodePrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeBarcodePrefix :: Encode TapeBarcodePrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TapeDriveType = TapeDriveType String
derive instance newtypeTapeDriveType :: Newtype TapeDriveType _
derive instance repGenericTapeDriveType :: Generic TapeDriveType _
instance showTapeDriveType :: Show TapeDriveType where
  show = genericShow
instance decodeTapeDriveType :: Decode TapeDriveType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeDriveType :: Encode TapeDriveType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a virtual tape.</p>
newtype TapeInfo = TapeInfo 
  { "TapeARN" :: NullOrUndefined.NullOrUndefined (TapeARN)
  , "TapeBarcode" :: NullOrUndefined.NullOrUndefined (TapeBarcode)
  , "TapeSizeInBytes" :: NullOrUndefined.NullOrUndefined (TapeSize)
  , "TapeStatus" :: NullOrUndefined.NullOrUndefined (TapeStatus)
  , "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeTapeInfo :: Newtype TapeInfo _
derive instance repGenericTapeInfo :: Generic TapeInfo _
instance showTapeInfo :: Show TapeInfo where
  show = genericShow
instance decodeTapeInfo :: Decode TapeInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeInfo :: Encode TapeInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An array of <a>TapeInfo</a> objects, where each object describes an a single tape. If there not tapes in the tape library or VTS, then the <code>TapeInfos</code> is an empty array.</p>
newtype TapeInfos = TapeInfos (Array TapeInfo)
derive instance newtypeTapeInfos :: Newtype TapeInfos _
derive instance repGenericTapeInfos :: Generic TapeInfos _
instance showTapeInfos :: Show TapeInfos where
  show = genericShow
instance decodeTapeInfos :: Decode TapeInfos where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeInfos :: Encode TapeInfos where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a recovery point.</p>
newtype TapeRecoveryPointInfo = TapeRecoveryPointInfo 
  { "TapeARN" :: NullOrUndefined.NullOrUndefined (TapeARN)
  , "TapeRecoveryPointTime" :: NullOrUndefined.NullOrUndefined (Time)
  , "TapeSizeInBytes" :: NullOrUndefined.NullOrUndefined (TapeSize)
  , "TapeStatus" :: NullOrUndefined.NullOrUndefined (TapeRecoveryPointStatus)
  }
derive instance newtypeTapeRecoveryPointInfo :: Newtype TapeRecoveryPointInfo _
derive instance repGenericTapeRecoveryPointInfo :: Generic TapeRecoveryPointInfo _
instance showTapeRecoveryPointInfo :: Show TapeRecoveryPointInfo where
  show = genericShow
instance decodeTapeRecoveryPointInfo :: Decode TapeRecoveryPointInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeRecoveryPointInfo :: Encode TapeRecoveryPointInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TapeRecoveryPointInfos = TapeRecoveryPointInfos (Array TapeRecoveryPointInfo)
derive instance newtypeTapeRecoveryPointInfos :: Newtype TapeRecoveryPointInfos _
derive instance repGenericTapeRecoveryPointInfos :: Generic TapeRecoveryPointInfos _
instance showTapeRecoveryPointInfos :: Show TapeRecoveryPointInfos where
  show = genericShow
instance decodeTapeRecoveryPointInfos :: Decode TapeRecoveryPointInfos where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeRecoveryPointInfos :: Encode TapeRecoveryPointInfos where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TapeRecoveryPointStatus = TapeRecoveryPointStatus String
derive instance newtypeTapeRecoveryPointStatus :: Newtype TapeRecoveryPointStatus _
derive instance repGenericTapeRecoveryPointStatus :: Generic TapeRecoveryPointStatus _
instance showTapeRecoveryPointStatus :: Show TapeRecoveryPointStatus where
  show = genericShow
instance decodeTapeRecoveryPointStatus :: Decode TapeRecoveryPointStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeRecoveryPointStatus :: Encode TapeRecoveryPointStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TapeSize = TapeSize Number
derive instance newtypeTapeSize :: Newtype TapeSize _
derive instance repGenericTapeSize :: Generic TapeSize _
instance showTapeSize :: Show TapeSize where
  show = genericShow
instance decodeTapeSize :: Decode TapeSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeSize :: Encode TapeSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TapeStatus = TapeStatus String
derive instance newtypeTapeStatus :: Newtype TapeStatus _
derive instance repGenericTapeStatus :: Generic TapeStatus _
instance showTapeStatus :: Show TapeStatus where
  show = genericShow
instance decodeTapeStatus :: Decode TapeStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeStatus :: Encode TapeStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TapeUsage = TapeUsage Number
derive instance newtypeTapeUsage :: Newtype TapeUsage _
derive instance repGenericTapeUsage :: Generic TapeUsage _
instance showTapeUsage :: Show TapeUsage where
  show = genericShow
instance decodeTapeUsage :: Decode TapeUsage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapeUsage :: Encode TapeUsage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tapes = Tapes (Array Tape)
derive instance newtypeTapes :: Newtype Tapes _
derive instance repGenericTapes :: Generic Tapes _
instance showTapes :: Show Tapes where
  show = genericShow
instance decodeTapes :: Decode Tapes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTapes :: Encode Tapes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetARN = TargetARN String
derive instance newtypeTargetARN :: Newtype TargetARN _
derive instance repGenericTargetARN :: Generic TargetARN _
instance showTargetARN :: Show TargetARN where
  show = genericShow
instance decodeTargetARN :: Decode TargetARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetARN :: Encode TargetARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetName = TargetName String
derive instance newtypeTargetName :: Newtype TargetName _
derive instance repGenericTargetName :: Generic TargetName _
instance showTargetName :: Show TargetName where
  show = genericShow
instance decodeTargetName :: Decode TargetName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetName :: Encode TargetName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Time = Time Number
derive instance newtypeTime :: Newtype Time _
derive instance repGenericTime :: Generic Time _
instance showTime :: Show Time where
  show = genericShow
instance decodeTime :: Decode Time where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTime :: Encode Time where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>UpdateBandwidthRateLimitInput$AverageDownloadRateLimitInBitsPerSec</a> </p> </li> <li> <p> <a>UpdateBandwidthRateLimitInput$AverageUploadRateLimitInBitsPerSec</a> </p> </li> </ul>
newtype UpdateBandwidthRateLimitInput = UpdateBandwidthRateLimitInput 
  { "GatewayARN" :: (GatewayARN)
  , "AverageUploadRateLimitInBitsPerSec" :: NullOrUndefined.NullOrUndefined (BandwidthUploadRateLimit)
  , "AverageDownloadRateLimitInBitsPerSec" :: NullOrUndefined.NullOrUndefined (BandwidthDownloadRateLimit)
  }
derive instance newtypeUpdateBandwidthRateLimitInput :: Newtype UpdateBandwidthRateLimitInput _
derive instance repGenericUpdateBandwidthRateLimitInput :: Generic UpdateBandwidthRateLimitInput _
instance showUpdateBandwidthRateLimitInput :: Show UpdateBandwidthRateLimitInput where
  show = genericShow
instance decodeUpdateBandwidthRateLimitInput :: Decode UpdateBandwidthRateLimitInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateBandwidthRateLimitInput :: Encode UpdateBandwidthRateLimitInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway whose throttle information was updated.</p>
newtype UpdateBandwidthRateLimitOutput = UpdateBandwidthRateLimitOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeUpdateBandwidthRateLimitOutput :: Newtype UpdateBandwidthRateLimitOutput _
derive instance repGenericUpdateBandwidthRateLimitOutput :: Generic UpdateBandwidthRateLimitOutput _
instance showUpdateBandwidthRateLimitOutput :: Show UpdateBandwidthRateLimitOutput where
  show = genericShow
instance decodeUpdateBandwidthRateLimitOutput :: Decode UpdateBandwidthRateLimitOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateBandwidthRateLimitOutput :: Encode UpdateBandwidthRateLimitOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>UpdateChapCredentialsInput$InitiatorName</a> </p> </li> <li> <p> <a>UpdateChapCredentialsInput$SecretToAuthenticateInitiator</a> </p> </li> <li> <p> <a>UpdateChapCredentialsInput$SecretToAuthenticateTarget</a> </p> </li> <li> <p> <a>UpdateChapCredentialsInput$TargetARN</a> </p> </li> </ul>
newtype UpdateChapCredentialsInput = UpdateChapCredentialsInput 
  { "TargetARN" :: (TargetARN)
  , "SecretToAuthenticateInitiator" :: (ChapSecret)
  , "InitiatorName" :: (IqnName)
  , "SecretToAuthenticateTarget" :: NullOrUndefined.NullOrUndefined (ChapSecret)
  }
derive instance newtypeUpdateChapCredentialsInput :: Newtype UpdateChapCredentialsInput _
derive instance repGenericUpdateChapCredentialsInput :: Generic UpdateChapCredentialsInput _
instance showUpdateChapCredentialsInput :: Show UpdateChapCredentialsInput where
  show = genericShow
instance decodeUpdateChapCredentialsInput :: Decode UpdateChapCredentialsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateChapCredentialsInput :: Encode UpdateChapCredentialsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the following fields:</p>
newtype UpdateChapCredentialsOutput = UpdateChapCredentialsOutput 
  { "TargetARN" :: NullOrUndefined.NullOrUndefined (TargetARN)
  , "InitiatorName" :: NullOrUndefined.NullOrUndefined (IqnName)
  }
derive instance newtypeUpdateChapCredentialsOutput :: Newtype UpdateChapCredentialsOutput _
derive instance repGenericUpdateChapCredentialsOutput :: Generic UpdateChapCredentialsOutput _
instance showUpdateChapCredentialsOutput :: Show UpdateChapCredentialsOutput where
  show = genericShow
instance decodeUpdateChapCredentialsOutput :: Decode UpdateChapCredentialsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateChapCredentialsOutput :: Encode UpdateChapCredentialsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGatewayInformationInput = UpdateGatewayInformationInput 
  { "GatewayARN" :: (GatewayARN)
  , "GatewayName" :: NullOrUndefined.NullOrUndefined (GatewayName)
  , "GatewayTimezone" :: NullOrUndefined.NullOrUndefined (GatewayTimezone)
  }
derive instance newtypeUpdateGatewayInformationInput :: Newtype UpdateGatewayInformationInput _
derive instance repGenericUpdateGatewayInformationInput :: Generic UpdateGatewayInformationInput _
instance showUpdateGatewayInformationInput :: Show UpdateGatewayInformationInput where
  show = genericShow
instance decodeUpdateGatewayInformationInput :: Decode UpdateGatewayInformationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGatewayInformationInput :: Encode UpdateGatewayInformationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the ARN of the gateway that was updated.</p>
newtype UpdateGatewayInformationOutput = UpdateGatewayInformationOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "GatewayName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateGatewayInformationOutput :: Newtype UpdateGatewayInformationOutput _
derive instance repGenericUpdateGatewayInformationOutput :: Generic UpdateGatewayInformationOutput _
instance showUpdateGatewayInformationOutput :: Show UpdateGatewayInformationOutput where
  show = genericShow
instance decodeUpdateGatewayInformationOutput :: Decode UpdateGatewayInformationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGatewayInformationOutput :: Encode UpdateGatewayInformationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway to update.</p>
newtype UpdateGatewaySoftwareNowInput = UpdateGatewaySoftwareNowInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeUpdateGatewaySoftwareNowInput :: Newtype UpdateGatewaySoftwareNowInput _
derive instance repGenericUpdateGatewaySoftwareNowInput :: Generic UpdateGatewaySoftwareNowInput _
instance showUpdateGatewaySoftwareNowInput :: Show UpdateGatewaySoftwareNowInput where
  show = genericShow
instance decodeUpdateGatewaySoftwareNowInput :: Decode UpdateGatewaySoftwareNowInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGatewaySoftwareNowInput :: Encode UpdateGatewaySoftwareNowInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway that was updated.</p>
newtype UpdateGatewaySoftwareNowOutput = UpdateGatewaySoftwareNowOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeUpdateGatewaySoftwareNowOutput :: Newtype UpdateGatewaySoftwareNowOutput _
derive instance repGenericUpdateGatewaySoftwareNowOutput :: Generic UpdateGatewaySoftwareNowOutput _
instance showUpdateGatewaySoftwareNowOutput :: Show UpdateGatewaySoftwareNowOutput where
  show = genericShow
instance decodeUpdateGatewaySoftwareNowOutput :: Decode UpdateGatewaySoftwareNowOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGatewaySoftwareNowOutput :: Encode UpdateGatewaySoftwareNowOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the following fields:</p> <ul> <li> <p> <a>UpdateMaintenanceStartTimeInput$DayOfWeek</a> </p> </li> <li> <p> <a>UpdateMaintenanceStartTimeInput$HourOfDay</a> </p> </li> <li> <p> <a>UpdateMaintenanceStartTimeInput$MinuteOfHour</a> </p> </li> </ul>
newtype UpdateMaintenanceStartTimeInput = UpdateMaintenanceStartTimeInput 
  { "GatewayARN" :: (GatewayARN)
  , "HourOfDay" :: (HourOfDay)
  , "MinuteOfHour" :: (MinuteOfHour)
  , "DayOfWeek" :: (DayOfWeek)
  }
derive instance newtypeUpdateMaintenanceStartTimeInput :: Newtype UpdateMaintenanceStartTimeInput _
derive instance repGenericUpdateMaintenanceStartTimeInput :: Generic UpdateMaintenanceStartTimeInput _
instance showUpdateMaintenanceStartTimeInput :: Show UpdateMaintenanceStartTimeInput where
  show = genericShow
instance decodeUpdateMaintenanceStartTimeInput :: Decode UpdateMaintenanceStartTimeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMaintenanceStartTimeInput :: Encode UpdateMaintenanceStartTimeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the gateway whose maintenance start time is updated.</p>
newtype UpdateMaintenanceStartTimeOutput = UpdateMaintenanceStartTimeOutput 
  { "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  }
derive instance newtypeUpdateMaintenanceStartTimeOutput :: Newtype UpdateMaintenanceStartTimeOutput _
derive instance repGenericUpdateMaintenanceStartTimeOutput :: Generic UpdateMaintenanceStartTimeOutput _
instance showUpdateMaintenanceStartTimeOutput :: Show UpdateMaintenanceStartTimeOutput where
  show = genericShow
instance decodeUpdateMaintenanceStartTimeOutput :: Decode UpdateMaintenanceStartTimeOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMaintenanceStartTimeOutput :: Encode UpdateMaintenanceStartTimeOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>UpdateNFSFileShareInput</p>
newtype UpdateNFSFileShareInput = UpdateNFSFileShareInput 
  { "FileShareARN" :: (FileShareARN)
  , "KMSEncrypted" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "KMSKey" :: NullOrUndefined.NullOrUndefined (KMSKey)
  , "NFSFileShareDefaults" :: NullOrUndefined.NullOrUndefined (NFSFileShareDefaults)
  , "DefaultStorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  , "ClientList" :: NullOrUndefined.NullOrUndefined (FileShareClientList)
  , "Squash" :: NullOrUndefined.NullOrUndefined (Squash)
  , "ReadOnly" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "GuessMIMETypeEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeUpdateNFSFileShareInput :: Newtype UpdateNFSFileShareInput _
derive instance repGenericUpdateNFSFileShareInput :: Generic UpdateNFSFileShareInput _
instance showUpdateNFSFileShareInput :: Show UpdateNFSFileShareInput where
  show = genericShow
instance decodeUpdateNFSFileShareInput :: Decode UpdateNFSFileShareInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateNFSFileShareInput :: Encode UpdateNFSFileShareInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>UpdateNFSFileShareOutput</p>
newtype UpdateNFSFileShareOutput = UpdateNFSFileShareOutput 
  { "FileShareARN" :: NullOrUndefined.NullOrUndefined (FileShareARN)
  }
derive instance newtypeUpdateNFSFileShareOutput :: Newtype UpdateNFSFileShareOutput _
derive instance repGenericUpdateNFSFileShareOutput :: Generic UpdateNFSFileShareOutput _
instance showUpdateNFSFileShareOutput :: Show UpdateNFSFileShareOutput where
  show = genericShow
instance decodeUpdateNFSFileShareOutput :: Decode UpdateNFSFileShareOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateNFSFileShareOutput :: Encode UpdateNFSFileShareOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>UpdateSnapshotScheduleInput$Description</a> </p> </li> <li> <p> <a>UpdateSnapshotScheduleInput$RecurrenceInHours</a> </p> </li> <li> <p> <a>UpdateSnapshotScheduleInput$StartAt</a> </p> </li> <li> <p> <a>UpdateSnapshotScheduleInput$VolumeARN</a> </p> </li> </ul>
newtype UpdateSnapshotScheduleInput = UpdateSnapshotScheduleInput 
  { "VolumeARN" :: (VolumeARN)
  , "StartAt" :: (HourOfDay)
  , "RecurrenceInHours" :: (RecurrenceInHours)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeUpdateSnapshotScheduleInput :: Newtype UpdateSnapshotScheduleInput _
derive instance repGenericUpdateSnapshotScheduleInput :: Generic UpdateSnapshotScheduleInput _
instance showUpdateSnapshotScheduleInput :: Show UpdateSnapshotScheduleInput where
  show = genericShow
instance decodeUpdateSnapshotScheduleInput :: Decode UpdateSnapshotScheduleInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSnapshotScheduleInput :: Encode UpdateSnapshotScheduleInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object containing the of the updated storage volume.</p>
newtype UpdateSnapshotScheduleOutput = UpdateSnapshotScheduleOutput 
  { "VolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  }
derive instance newtypeUpdateSnapshotScheduleOutput :: Newtype UpdateSnapshotScheduleOutput _
derive instance repGenericUpdateSnapshotScheduleOutput :: Generic UpdateSnapshotScheduleOutput _
instance showUpdateSnapshotScheduleOutput :: Show UpdateSnapshotScheduleOutput where
  show = genericShow
instance decodeUpdateSnapshotScheduleOutput :: Decode UpdateSnapshotScheduleOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSnapshotScheduleOutput :: Encode UpdateSnapshotScheduleOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateVTLDeviceTypeInput = UpdateVTLDeviceTypeInput 
  { "VTLDeviceARN" :: (VTLDeviceARN)
  , "DeviceType" :: (DeviceType)
  }
derive instance newtypeUpdateVTLDeviceTypeInput :: Newtype UpdateVTLDeviceTypeInput _
derive instance repGenericUpdateVTLDeviceTypeInput :: Generic UpdateVTLDeviceTypeInput _
instance showUpdateVTLDeviceTypeInput :: Show UpdateVTLDeviceTypeInput where
  show = genericShow
instance decodeUpdateVTLDeviceTypeInput :: Decode UpdateVTLDeviceTypeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateVTLDeviceTypeInput :: Encode UpdateVTLDeviceTypeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>UpdateVTLDeviceTypeOutput</p>
newtype UpdateVTLDeviceTypeOutput = UpdateVTLDeviceTypeOutput 
  { "VTLDeviceARN" :: NullOrUndefined.NullOrUndefined (VTLDeviceARN)
  }
derive instance newtypeUpdateVTLDeviceTypeOutput :: Newtype UpdateVTLDeviceTypeOutput _
derive instance repGenericUpdateVTLDeviceTypeOutput :: Generic UpdateVTLDeviceTypeOutput _
instance showUpdateVTLDeviceTypeOutput :: Show UpdateVTLDeviceTypeOutput where
  show = genericShow
instance decodeUpdateVTLDeviceTypeOutput :: Decode UpdateVTLDeviceTypeOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateVTLDeviceTypeOutput :: Encode UpdateVTLDeviceTypeOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a device object associated with a tape gateway.</p>
newtype VTLDevice = VTLDevice 
  { "VTLDeviceARN" :: NullOrUndefined.NullOrUndefined (VTLDeviceARN)
  , "VTLDeviceType" :: NullOrUndefined.NullOrUndefined (VTLDeviceType)
  , "VTLDeviceVendor" :: NullOrUndefined.NullOrUndefined (VTLDeviceVendor)
  , "VTLDeviceProductIdentifier" :: NullOrUndefined.NullOrUndefined (VTLDeviceProductIdentifier)
  , "DeviceiSCSIAttributes" :: NullOrUndefined.NullOrUndefined (DeviceiSCSIAttributes)
  }
derive instance newtypeVTLDevice :: Newtype VTLDevice _
derive instance repGenericVTLDevice :: Generic VTLDevice _
instance showVTLDevice :: Show VTLDevice where
  show = genericShow
instance decodeVTLDevice :: Decode VTLDevice where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVTLDevice :: Encode VTLDevice where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VTLDeviceARN = VTLDeviceARN String
derive instance newtypeVTLDeviceARN :: Newtype VTLDeviceARN _
derive instance repGenericVTLDeviceARN :: Generic VTLDeviceARN _
instance showVTLDeviceARN :: Show VTLDeviceARN where
  show = genericShow
instance decodeVTLDeviceARN :: Decode VTLDeviceARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVTLDeviceARN :: Encode VTLDeviceARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VTLDeviceARNs = VTLDeviceARNs (Array VTLDeviceARN)
derive instance newtypeVTLDeviceARNs :: Newtype VTLDeviceARNs _
derive instance repGenericVTLDeviceARNs :: Generic VTLDeviceARNs _
instance showVTLDeviceARNs :: Show VTLDeviceARNs where
  show = genericShow
instance decodeVTLDeviceARNs :: Decode VTLDeviceARNs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVTLDeviceARNs :: Encode VTLDeviceARNs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VTLDeviceProductIdentifier = VTLDeviceProductIdentifier String
derive instance newtypeVTLDeviceProductIdentifier :: Newtype VTLDeviceProductIdentifier _
derive instance repGenericVTLDeviceProductIdentifier :: Generic VTLDeviceProductIdentifier _
instance showVTLDeviceProductIdentifier :: Show VTLDeviceProductIdentifier where
  show = genericShow
instance decodeVTLDeviceProductIdentifier :: Decode VTLDeviceProductIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVTLDeviceProductIdentifier :: Encode VTLDeviceProductIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VTLDeviceType = VTLDeviceType String
derive instance newtypeVTLDeviceType :: Newtype VTLDeviceType _
derive instance repGenericVTLDeviceType :: Generic VTLDeviceType _
instance showVTLDeviceType :: Show VTLDeviceType where
  show = genericShow
instance decodeVTLDeviceType :: Decode VTLDeviceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVTLDeviceType :: Encode VTLDeviceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VTLDeviceVendor = VTLDeviceVendor String
derive instance newtypeVTLDeviceVendor :: Newtype VTLDeviceVendor _
derive instance repGenericVTLDeviceVendor :: Generic VTLDeviceVendor _
instance showVTLDeviceVendor :: Show VTLDeviceVendor where
  show = genericShow
instance decodeVTLDeviceVendor :: Decode VTLDeviceVendor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVTLDeviceVendor :: Encode VTLDeviceVendor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VTLDevices = VTLDevices (Array VTLDevice)
derive instance newtypeVTLDevices :: Newtype VTLDevices _
derive instance repGenericVTLDevices :: Generic VTLDevices _
instance showVTLDevices :: Show VTLDevices where
  show = genericShow
instance decodeVTLDevices :: Decode VTLDevices where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVTLDevices :: Encode VTLDevices where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VolumeARN = VolumeARN String
derive instance newtypeVolumeARN :: Newtype VolumeARN _
derive instance repGenericVolumeARN :: Generic VolumeARN _
instance showVolumeARN :: Show VolumeARN where
  show = genericShow
instance decodeVolumeARN :: Decode VolumeARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeARN :: Encode VolumeARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VolumeARNs = VolumeARNs (Array VolumeARN)
derive instance newtypeVolumeARNs :: Newtype VolumeARNs _
derive instance repGenericVolumeARNs :: Generic VolumeARNs _
instance showVolumeARNs :: Show VolumeARNs where
  show = genericShow
instance decodeVolumeARNs :: Decode VolumeARNs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeARNs :: Encode VolumeARNs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VolumeId = VolumeId String
derive instance newtypeVolumeId :: Newtype VolumeId _
derive instance repGenericVolumeId :: Generic VolumeId _
instance showVolumeId :: Show VolumeId where
  show = genericShow
instance decodeVolumeId :: Decode VolumeId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeId :: Encode VolumeId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a storage volume object.</p>
newtype VolumeInfo = VolumeInfo 
  { "VolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  , "VolumeId" :: NullOrUndefined.NullOrUndefined (VolumeId)
  , "GatewayARN" :: NullOrUndefined.NullOrUndefined (GatewayARN)
  , "GatewayId" :: NullOrUndefined.NullOrUndefined (GatewayId)
  , "VolumeType" :: NullOrUndefined.NullOrUndefined (VolumeType)
  , "VolumeSizeInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeVolumeInfo :: Newtype VolumeInfo _
derive instance repGenericVolumeInfo :: Generic VolumeInfo _
instance showVolumeInfo :: Show VolumeInfo where
  show = genericShow
instance decodeVolumeInfo :: Decode VolumeInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeInfo :: Encode VolumeInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VolumeInfos = VolumeInfos (Array VolumeInfo)
derive instance newtypeVolumeInfos :: Newtype VolumeInfos _
derive instance repGenericVolumeInfos :: Generic VolumeInfos _
instance showVolumeInfos :: Show VolumeInfos where
  show = genericShow
instance decodeVolumeInfos :: Decode VolumeInfos where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeInfos :: Encode VolumeInfos where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VolumeRecoveryPointInfo = VolumeRecoveryPointInfo 
  { "VolumeARN" :: NullOrUndefined.NullOrUndefined (VolumeARN)
  , "VolumeSizeInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "VolumeUsageInBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "VolumeRecoveryPointTime" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeVolumeRecoveryPointInfo :: Newtype VolumeRecoveryPointInfo _
derive instance repGenericVolumeRecoveryPointInfo :: Generic VolumeRecoveryPointInfo _
instance showVolumeRecoveryPointInfo :: Show VolumeRecoveryPointInfo where
  show = genericShow
instance decodeVolumeRecoveryPointInfo :: Decode VolumeRecoveryPointInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeRecoveryPointInfo :: Encode VolumeRecoveryPointInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VolumeRecoveryPointInfos = VolumeRecoveryPointInfos (Array VolumeRecoveryPointInfo)
derive instance newtypeVolumeRecoveryPointInfos :: Newtype VolumeRecoveryPointInfos _
derive instance repGenericVolumeRecoveryPointInfos :: Generic VolumeRecoveryPointInfos _
instance showVolumeRecoveryPointInfos :: Show VolumeRecoveryPointInfos where
  show = genericShow
instance decodeVolumeRecoveryPointInfos :: Decode VolumeRecoveryPointInfos where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeRecoveryPointInfos :: Encode VolumeRecoveryPointInfos where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VolumeStatus = VolumeStatus String
derive instance newtypeVolumeStatus :: Newtype VolumeStatus _
derive instance repGenericVolumeStatus :: Generic VolumeStatus _
instance showVolumeStatus :: Show VolumeStatus where
  show = genericShow
instance decodeVolumeStatus :: Decode VolumeStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeStatus :: Encode VolumeStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VolumeType = VolumeType String
derive instance newtypeVolumeType :: Newtype VolumeType _
derive instance repGenericVolumeType :: Generic VolumeType _
instance showVolumeType :: Show VolumeType where
  show = genericShow
instance decodeVolumeType :: Decode VolumeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeType :: Encode VolumeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VolumeUsedInBytes = VolumeUsedInBytes Number
derive instance newtypeVolumeUsedInBytes :: Newtype VolumeUsedInBytes _
derive instance repGenericVolumeUsedInBytes :: Generic VolumeUsedInBytes _
instance showVolumeUsedInBytes :: Show VolumeUsedInBytes where
  show = genericShow
instance decodeVolumeUsedInBytes :: Decode VolumeUsedInBytes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeUsedInBytes :: Encode VolumeUsedInBytes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lists iSCSI information about a volume.</p>
newtype VolumeiSCSIAttributes = VolumeiSCSIAttributes 
  { "TargetARN" :: NullOrUndefined.NullOrUndefined (TargetARN)
  , "NetworkInterfaceId" :: NullOrUndefined.NullOrUndefined (NetworkInterfaceId)
  , "NetworkInterfacePort" :: NullOrUndefined.NullOrUndefined (Int)
  , "LunNumber" :: NullOrUndefined.NullOrUndefined (PositiveIntObject)
  , "ChapEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeVolumeiSCSIAttributes :: Newtype VolumeiSCSIAttributes _
derive instance repGenericVolumeiSCSIAttributes :: Generic VolumeiSCSIAttributes _
instance showVolumeiSCSIAttributes :: Show VolumeiSCSIAttributes where
  show = genericShow
instance decodeVolumeiSCSIAttributes :: Decode VolumeiSCSIAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeiSCSIAttributes :: Encode VolumeiSCSIAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorDetails' = ErrorDetails' (StrMap.StrMap String)
derive instance newtypeErrorDetails' :: Newtype ErrorDetails' _
derive instance repGenericErrorDetails' :: Generic ErrorDetails' _
instance showErrorDetails' :: Show ErrorDetails' where
  show = genericShow
instance decodeErrorDetails' :: Decode ErrorDetails' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorDetails' :: Encode ErrorDetails' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
