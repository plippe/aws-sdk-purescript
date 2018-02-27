

-- | <fullname>AWS Storage Gateway Service</fullname> <p>AWS Storage Gateway is the service that connects an on-premises software appliance with cloud-based storage to provide seamless and secure integration between an organization's on-premises IT environment and AWS's storage infrastructure. The service enables you to securely upload data to the AWS cloud for cost effective backup and rapid disaster recovery.</p> <p>Use the following links to get started using the <i>AWS Storage Gateway Service API Reference</i>:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewayHTTPRequestsHeaders">AWS Storage Gateway Required Request Headers</a>: Describes the required headers that you must send with every POST request to AWS Storage Gateway.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewaySigningRequests">Signing Requests</a>: AWS Storage Gateway requires that you authenticate every request you send; this topic describes how sign such a request.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#APIErrorResponses">Error Responses</a>: Provides reference information about AWS Storage Gateway errors.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_Operations.html">Operations in AWS Storage Gateway</a>: Contains detailed descriptions of all AWS Storage Gateway operations, their request parameters, response elements, possible errors, and examples of requests and responses.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#sg_region">AWS Storage Gateway Regions and Endpoints:</a> Provides a list of each region and endpoints available for use with AWS Storage Gateway. </p> </li> </ul> <note> <p>AWS Storage Gateway resource IDs are in uppercase. When you use these resource IDs with the Amazon EC2 API, EC2 expects resource IDs in lowercase. You must change your resource ID to lowercase to use it with the EC2 API. For example, in Storage Gateway the ID for a volume might be <code>vol-AA22BB012345DAF670</code>. When you use this ID with the EC2 API, you must change it to <code>vol-aa22bb012345daf670</code>. Otherwise, the EC2 API might not behave as expected.</p> </note> <important> <p>IDs for Storage Gateway volumes and Amazon EBS snapshots created from gateway volumes are changing to a longer format. Starting in December 2016, all new volumes and snapshots will be created with a 17-character string. Starting in April 2016, you will be able to use these longer IDs so you can test your systems with the new format. For more information, see <a href="https://aws.amazon.com/ec2/faqs/#longer-ids">Longer EC2 and EBS Resource IDs</a>.</p> <p> For example, a volume Amazon Resource Name (ARN) with the longer volume ID format looks like the following:</p> <p> <code>arn:aws:storagegateway:us-west-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABBCCDDEEFFG</code>.</p> <p>A snapshot ID with the longer ID format looks like the following: <code>snap-78e226633445566ee</code>.</p> <p>For more information, see <a href="https://forums.aws.amazon.com/ann.jspa?annID=3557">Announcement: Heads-up – Longer AWS Storage Gateway volume and snapshot IDs coming in 2016</a>.</p> </important>
module AWS.StorageGateway where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "StorageGateway" :: String


-- | <p>Activates the gateway you previously deployed on your host. For more information, see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedActivateGateway-common.html"> Activate the AWS Storage Gateway</a>. In the activation process, you specify information such as the region you want to use for storing snapshots or tapes, the time zone for scheduled snapshots the gateway snapshot schedule window, an activation key, and a name for your gateway. The activation process also associates your gateway with your account; for more information, see <a>UpdateGatewayInformation</a>.</p> <note> <p>You must turn on the gateway VM before you can activate your gateway.</p> </note>
activateGateway :: forall eff. ActivateGatewayInput -> Aff (err :: AWS.RequestError | eff) ActivateGatewayOutput
activateGateway = AWS.request serviceName "ActivateGateway" 


-- | <p>Configures one or more gateway local disks as cache for a gateway. This operation is only supported in the cached volume, tape and file gateway type (see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/StorageGatewayConcepts.html">Storage Gateway Concepts</a>).</p> <p>In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add cache, and one or more disk IDs that you want to configure as cache.</p>
addCache :: forall eff. AddCacheInput -> Aff (err :: AWS.RequestError | eff) AddCacheOutput
addCache = AWS.request serviceName "AddCache" 


-- | <p>Adds one or more tags to the specified resource. You use tags to add metadata to resources, which you can use to categorize these resources. For example, you can categorize resources by purpose, owner, environment, or team. Each tag consists of a key and a value, which you define. You can add tags to the following AWS Storage Gateway resources:</p> <ul> <li> <p>Storage gateways of all types</p> </li> </ul> <ul> <li> <p>Storage Volumes</p> </li> </ul> <ul> <li> <p>Virtual Tapes</p> </li> </ul> <p>You can create a maximum of 10 tags for each resource. Virtual tapes and storage volumes that are recovered to a new gateway maintain their tags.</p>
addTagsToResource :: forall eff. AddTagsToResourceInput -> Aff (err :: AWS.RequestError | eff) AddTagsToResourceOutput
addTagsToResource = AWS.request serviceName "AddTagsToResource" 


-- | <p>Configures one or more gateway local disks as upload buffer for a specified gateway. This operation is supported for the stored volume, cached volume and tape gateway types.</p> <p>In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add upload buffer, and one or more disk IDs that you want to configure as upload buffer.</p>
addUploadBuffer :: forall eff. AddUploadBufferInput -> Aff (err :: AWS.RequestError | eff) AddUploadBufferOutput
addUploadBuffer = AWS.request serviceName "AddUploadBuffer" 


-- | <p>Configures one or more gateway local disks as working storage for a gateway. This operation is only supported in the stored volume gateway type. This operation is deprecated in cached volume API version 20120630. Use <a>AddUploadBuffer</a> instead.</p> <note> <p>Working storage is also referred to as upload buffer. You can also use the <a>AddUploadBuffer</a> operation to add upload buffer to a stored volume gateway.</p> </note> <p>In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add working storage, and one or more disk IDs that you want to configure as working storage.</p>
addWorkingStorage :: forall eff. AddWorkingStorageInput -> Aff (err :: AWS.RequestError | eff) AddWorkingStorageOutput
addWorkingStorage = AWS.request serviceName "AddWorkingStorage" 


-- | <p>Cancels archiving of a virtual tape to the virtual tape shelf (VTS) after the archiving process is initiated. This operation is only supported in the tape gateway type.</p>
cancelArchival :: forall eff. CancelArchivalInput -> Aff (err :: AWS.RequestError | eff) CancelArchivalOutput
cancelArchival = AWS.request serviceName "CancelArchival" 


-- | <p>Cancels retrieval of a virtual tape from the virtual tape shelf (VTS) to a gateway after the retrieval process is initiated. The virtual tape is returned to the VTS. This operation is only supported in the tape gateway type.</p>
cancelRetrieval :: forall eff. CancelRetrievalInput -> Aff (err :: AWS.RequestError | eff) CancelRetrievalOutput
cancelRetrieval = AWS.request serviceName "CancelRetrieval" 


-- | <p>Creates a cached volume on a specified cached volume gateway. This operation is only supported in the cached volume gateway type.</p> <note> <p>Cache storage must be allocated to the gateway before you can create a cached volume. Use the <a>AddCache</a> operation to add cache storage to a gateway. </p> </note> <p>In the request, you must specify the gateway, size of the volume in bytes, the iSCSI target name, an IP address on which to expose the target, and a unique client token. In response, the gateway creates the volume and returns information about it. This information includes the volume Amazon Resource Name (ARN), its size, and the iSCSI target ARN that initiators can use to connect to the volume target.</p> <p>Optionally, you can provide the ARN for an existing volume as the <code>SourceVolumeARN</code> for this cached volume, which creates an exact copy of the existing volume’s latest recovery point. The <code>VolumeSizeInBytes</code> value must be equal to or larger than the size of the copied volume, in bytes.</p>
createCachediSCSIVolume :: forall eff. CreateCachediSCSIVolumeInput -> Aff (err :: AWS.RequestError | eff) CreateCachediSCSIVolumeOutput
createCachediSCSIVolume = AWS.request serviceName "CreateCachediSCSIVolume" 


-- | <p>Creates a file share on an existing file gateway. In Storage Gateway, a file share is a file system mount point backed by Amazon S3 cloud storage. Storage Gateway exposes file shares using a Network File System (NFS) interface. This operation is only supported in the file gateway type.</p> <important> <p>File gateway requires AWS Security Token Service (AWS STS) to be activated to enable you create a file share. Make sure AWS STS is activated in the region you are creating your file gateway in. If AWS STS is not activated in the region, activate it. For information about how to activate AWS STS, see Activating and Deactivating AWS STS in an AWS Region in the AWS Identity and Access Management User Guide. </p> <p>File gateway does not support creating hard or symbolic links on a file share.</p> </important>
createNFSFileShare :: forall eff. CreateNFSFileShareInput -> Aff (err :: AWS.RequestError | eff) CreateNFSFileShareOutput
createNFSFileShare = AWS.request serviceName "CreateNFSFileShare" 


-- | <p>Initiates a snapshot of a volume.</p> <p>AWS Storage Gateway provides the ability to back up point-in-time snapshots of your data to Amazon Simple Storage (S3) for durable off-site recovery, as well as import the data to an Amazon Elastic Block Store (EBS) volume in Amazon Elastic Compute Cloud (EC2). You can take snapshots of your gateway volume on a scheduled or ad-hoc basis. This API enables you to take ad-hoc snapshot. For more information, see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#SchedulingSnapshot">Editing a Snapshot Schedule</a>.</p> <p>In the CreateSnapshot request you identify the volume by providing its Amazon Resource Name (ARN). You must also provide description for the snapshot. When AWS Storage Gateway takes the snapshot of specified volume, the snapshot and description appears in the AWS Storage Gateway Console. In response, AWS Storage Gateway returns you a snapshot ID. You can use this snapshot ID to check the snapshot progress or later use it when you want to create a volume from a snapshot. This operation is only supported in stored and cached volume gateway type.</p> <note> <p>To list or delete a snapshot, you must use the Amazon EC2 API. For more information, see DescribeSnapshots or DeleteSnapshot in the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Operations.html">EC2 API reference</a>.</p> </note> <important> <p>Volume and snapshot IDs are changing to a longer length ID format. For more information, see the important note on the <a href="http://docs.aws.amazon.com/storagegateway/latest/APIReference/Welcome.html">Welcome</a> page.</p> </important>
createSnapshot :: forall eff. CreateSnapshotInput -> Aff (err :: AWS.RequestError | eff) CreateSnapshotOutput
createSnapshot = AWS.request serviceName "CreateSnapshot" 


-- | <p>Initiates a snapshot of a gateway from a volume recovery point. This operation is only supported in the cached volume gateway type.</p> <p>A volume recovery point is a point in time at which all data of the volume is consistent and from which you can create a snapshot. To get a list of volume recovery point for cached volume gateway, use <a>ListVolumeRecoveryPoints</a>.</p> <p>In the <code>CreateSnapshotFromVolumeRecoveryPoint</code> request, you identify the volume by providing its Amazon Resource Name (ARN). You must also provide a description for the snapshot. When the gateway takes a snapshot of the specified volume, the snapshot and its description appear in the AWS Storage Gateway console. In response, the gateway returns you a snapshot ID. You can use this snapshot ID to check the snapshot progress or later use it when you want to create a volume from a snapshot.</p> <note> <p>To list or delete a snapshot, you must use the Amazon EC2 API. For more information, in <i>Amazon Elastic Compute Cloud API Reference</i>.</p> </note>
createSnapshotFromVolumeRecoveryPoint :: forall eff. CreateSnapshotFromVolumeRecoveryPointInput -> Aff (err :: AWS.RequestError | eff) CreateSnapshotFromVolumeRecoveryPointOutput
createSnapshotFromVolumeRecoveryPoint = AWS.request serviceName "CreateSnapshotFromVolumeRecoveryPoint" 


-- | <p>Creates a volume on a specified gateway. This operation is only supported in the stored volume gateway type.</p> <p>The size of the volume to create is inferred from the disk size. You can choose to preserve existing data on the disk, create volume from an existing snapshot, or create an empty volume. If you choose to create an empty gateway volume, then any existing data on the disk is erased.</p> <p>In the request you must specify the gateway and the disk information on which you are creating the volume. In response, the gateway creates the volume and returns volume information such as the volume Amazon Resource Name (ARN), its size, and the iSCSI target ARN that initiators can use to connect to the volume target.</p>
createStorediSCSIVolume :: forall eff. CreateStorediSCSIVolumeInput -> Aff (err :: AWS.RequestError | eff) CreateStorediSCSIVolumeOutput
createStorediSCSIVolume = AWS.request serviceName "CreateStorediSCSIVolume" 


-- | <p>Creates a virtual tape by using your own barcode. You write data to the virtual tape and then archive the tape. A barcode is unique and can not be reused if it has already been used on a tape . This applies to barcodes used on deleted tapes. This operation is only supported in the tape gateway type.</p> <note> <p>Cache storage must be allocated to the gateway before you can create a virtual tape. Use the <a>AddCache</a> operation to add cache storage to a gateway.</p> </note>
createTapeWithBarcode :: forall eff. CreateTapeWithBarcodeInput -> Aff (err :: AWS.RequestError | eff) CreateTapeWithBarcodeOutput
createTapeWithBarcode = AWS.request serviceName "CreateTapeWithBarcode" 


-- | <p>Creates one or more virtual tapes. You write data to the virtual tapes and then archive the tapes. This operation is only supported in the tape gateway type.</p> <note> <p>Cache storage must be allocated to the gateway before you can create virtual tapes. Use the <a>AddCache</a> operation to add cache storage to a gateway. </p> </note>
createTapes :: forall eff. CreateTapesInput -> Aff (err :: AWS.RequestError | eff) CreateTapesOutput
createTapes = AWS.request serviceName "CreateTapes" 


-- | <p>Deletes the bandwidth rate limits of a gateway. You can delete either the upload and download bandwidth rate limit, or you can delete both. If you delete only one of the limits, the other limit remains unchanged. To specify which gateway to work with, use the Amazon Resource Name (ARN) of the gateway in your request.</p>
deleteBandwidthRateLimit :: forall eff. DeleteBandwidthRateLimitInput -> Aff (err :: AWS.RequestError | eff) DeleteBandwidthRateLimitOutput
deleteBandwidthRateLimit = AWS.request serviceName "DeleteBandwidthRateLimit" 


-- | <p>Deletes Challenge-Handshake Authentication Protocol (CHAP) credentials for a specified iSCSI target and initiator pair.</p>
deleteChapCredentials :: forall eff. DeleteChapCredentialsInput -> Aff (err :: AWS.RequestError | eff) DeleteChapCredentialsOutput
deleteChapCredentials = AWS.request serviceName "DeleteChapCredentials" 


-- | <p>Deletes a file share from a file gateway. This operation is only supported in the file gateway type.</p>
deleteFileShare :: forall eff. DeleteFileShareInput -> Aff (err :: AWS.RequestError | eff) DeleteFileShareOutput
deleteFileShare = AWS.request serviceName "DeleteFileShare" 


-- | <p>Deletes a gateway. To specify which gateway to delete, use the Amazon Resource Name (ARN) of the gateway in your request. The operation deletes the gateway; however, it does not delete the gateway virtual machine (VM) from your host computer.</p> <p>After you delete a gateway, you cannot reactivate it. Completed snapshots of the gateway volumes are not deleted upon deleting the gateway, however, pending snapshots will not complete. After you delete a gateway, your next step is to remove it from your environment.</p> <important> <p>You no longer pay software charges after the gateway is deleted; however, your existing Amazon EBS snapshots persist and you will continue to be billed for these snapshots. You can choose to remove all remaining Amazon EBS snapshots by canceling your Amazon EC2 subscription.  If you prefer not to cancel your Amazon EC2 subscription, you can delete your snapshots using the Amazon EC2 console. For more information, see the <a href="http://aws.amazon.com/storagegateway"> AWS Storage Gateway Detail Page</a>. </p> </important>
deleteGateway :: forall eff. DeleteGatewayInput -> Aff (err :: AWS.RequestError | eff) DeleteGatewayOutput
deleteGateway = AWS.request serviceName "DeleteGateway" 


-- | <p>Deletes a snapshot of a volume.</p> <p>You can take snapshots of your gateway volumes on a scheduled or ad hoc basis. This API action enables you to delete a snapshot schedule for a volume. For more information, see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/WorkingWithSnapshots.html">Working with Snapshots</a>. In the <code>DeleteSnapshotSchedule</code> request, you identify the volume by providing its Amazon Resource Name (ARN). This operation is only supported in stored and cached volume gateway types.</p> <note> <p>To list or delete a snapshot, you must use the Amazon EC2 API. in <i>Amazon Elastic Compute Cloud API Reference</i>.</p> </note>
deleteSnapshotSchedule :: forall eff. DeleteSnapshotScheduleInput -> Aff (err :: AWS.RequestError | eff) DeleteSnapshotScheduleOutput
deleteSnapshotSchedule = AWS.request serviceName "DeleteSnapshotSchedule" 


-- | <p>Deletes the specified virtual tape. This operation is only supported in the tape gateway type.</p>
deleteTape :: forall eff. DeleteTapeInput -> Aff (err :: AWS.RequestError | eff) DeleteTapeOutput
deleteTape = AWS.request serviceName "DeleteTape" 


-- | <p>Deletes the specified virtual tape from the virtual tape shelf (VTS). This operation is only supported in the tape gateway type.</p>
deleteTapeArchive :: forall eff. DeleteTapeArchiveInput -> Aff (err :: AWS.RequestError | eff) DeleteTapeArchiveOutput
deleteTapeArchive = AWS.request serviceName "DeleteTapeArchive" 


-- | <p>Deletes the specified storage volume that you previously created using the <a>CreateCachediSCSIVolume</a> or <a>CreateStorediSCSIVolume</a> API. This operation is only supported in the cached volume and stored volume types. For stored volume gateways, the local disk that was configured as the storage volume is not deleted. You can reuse the local disk to create another storage volume. </p> <p>Before you delete a volume, make sure there are no iSCSI connections to the volume you are deleting. You should also make sure there is no snapshot in progress. You can use the Amazon Elastic Compute Cloud (Amazon EC2) API to query snapshots on the volume you are deleting and check the snapshot status. For more information, go to <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html">DescribeSnapshots</a> in the <i>Amazon Elastic Compute Cloud API Reference</i>.</p> <p>In the request, you must provide the Amazon Resource Name (ARN) of the storage volume you want to delete.</p>
deleteVolume :: forall eff. DeleteVolumeInput -> Aff (err :: AWS.RequestError | eff) DeleteVolumeOutput
deleteVolume = AWS.request serviceName "DeleteVolume" 


-- | <p>Returns the bandwidth rate limits of a gateway. By default, these limits are not set, which means no bandwidth rate limiting is in effect.</p> <p>This operation only returns a value for a bandwidth rate limit only if the limit is set. If no limits are set for the gateway, then this operation returns only the gateway ARN in the response body. To specify which gateway to describe, use the Amazon Resource Name (ARN) of the gateway in your request.</p>
describeBandwidthRateLimit :: forall eff. DescribeBandwidthRateLimitInput -> Aff (err :: AWS.RequestError | eff) DescribeBandwidthRateLimitOutput
describeBandwidthRateLimit = AWS.request serviceName "DescribeBandwidthRateLimit" 


-- | <p>Returns information about the cache of a gateway. This operation is only supported in the cached volume, tape and file gateway types.</p> <p>The response includes disk IDs that are configured as cache, and it includes the amount of cache allocated and used.</p>
describeCache :: forall eff. DescribeCacheInput -> Aff (err :: AWS.RequestError | eff) DescribeCacheOutput
describeCache = AWS.request serviceName "DescribeCache" 


-- | <p>Returns a description of the gateway volumes specified in the request. This operation is only supported in the cached volume gateway types.</p> <p>The list of gateway volumes in the request must be from one gateway. In the response Amazon Storage Gateway returns volume information sorted by volume Amazon Resource Name (ARN).</p>
describeCachediSCSIVolumes :: forall eff. DescribeCachediSCSIVolumesInput -> Aff (err :: AWS.RequestError | eff) DescribeCachediSCSIVolumesOutput
describeCachediSCSIVolumes = AWS.request serviceName "DescribeCachediSCSIVolumes" 


-- | <p>Returns an array of Challenge-Handshake Authentication Protocol (CHAP) credentials information for a specified iSCSI target, one for each target-initiator pair.</p>
describeChapCredentials :: forall eff. DescribeChapCredentialsInput -> Aff (err :: AWS.RequestError | eff) DescribeChapCredentialsOutput
describeChapCredentials = AWS.request serviceName "DescribeChapCredentials" 


-- | <p>Returns metadata about a gateway such as its name, network interfaces, configured time zone, and the state (whether the gateway is running or not). To specify which gateway to describe, use the Amazon Resource Name (ARN) of the gateway in your request.</p>
describeGatewayInformation :: forall eff. DescribeGatewayInformationInput -> Aff (err :: AWS.RequestError | eff) DescribeGatewayInformationOutput
describeGatewayInformation = AWS.request serviceName "DescribeGatewayInformation" 


-- | <p>Returns your gateway's weekly maintenance start time including the day and time of the week. Note that values are in terms of the gateway's time zone.</p>
describeMaintenanceStartTime :: forall eff. DescribeMaintenanceStartTimeInput -> Aff (err :: AWS.RequestError | eff) DescribeMaintenanceStartTimeOutput
describeMaintenanceStartTime = AWS.request serviceName "DescribeMaintenanceStartTime" 


-- | <p>Gets a description for one or more file shares from a file gateway. This operation is only supported in the file gateway type.</p>
describeNFSFileShares :: forall eff. DescribeNFSFileSharesInput -> Aff (err :: AWS.RequestError | eff) DescribeNFSFileSharesOutput
describeNFSFileShares = AWS.request serviceName "DescribeNFSFileShares" 


-- | <p>Describes the snapshot schedule for the specified gateway volume. The snapshot schedule information includes intervals at which snapshots are automatically initiated on the volume. This operation is only supported in the cached volume and stored volume types.</p>
describeSnapshotSchedule :: forall eff. DescribeSnapshotScheduleInput -> Aff (err :: AWS.RequestError | eff) DescribeSnapshotScheduleOutput
describeSnapshotSchedule = AWS.request serviceName "DescribeSnapshotSchedule" 


-- | <p>Returns the description of the gateway volumes specified in the request. The list of gateway volumes in the request must be from one gateway. In the response Amazon Storage Gateway returns volume information sorted by volume ARNs. This operation is only supported in stored volume gateway type.</p>
describeStorediSCSIVolumes :: forall eff. DescribeStorediSCSIVolumesInput -> Aff (err :: AWS.RequestError | eff) DescribeStorediSCSIVolumesOutput
describeStorediSCSIVolumes = AWS.request serviceName "DescribeStorediSCSIVolumes" 


-- | <p>Returns a description of specified virtual tapes in the virtual tape shelf (VTS). This operation is only supported in the tape gateway type.</p> <p>If a specific <code>TapeARN</code> is not specified, AWS Storage Gateway returns a description of all virtual tapes found in the VTS associated with your account.</p>
describeTapeArchives :: forall eff. DescribeTapeArchivesInput -> Aff (err :: AWS.RequestError | eff) DescribeTapeArchivesOutput
describeTapeArchives = AWS.request serviceName "DescribeTapeArchives" 


-- | <p>Returns a list of virtual tape recovery points that are available for the specified tape gateway.</p> <p>A recovery point is a point-in-time view of a virtual tape at which all the data on the virtual tape is consistent. If your gateway crashes, virtual tapes that have recovery points can be recovered to a new gateway. This operation is only supported in the tape gateway type.</p>
describeTapeRecoveryPoints :: forall eff. DescribeTapeRecoveryPointsInput -> Aff (err :: AWS.RequestError | eff) DescribeTapeRecoveryPointsOutput
describeTapeRecoveryPoints = AWS.request serviceName "DescribeTapeRecoveryPoints" 


-- | <p>Returns a description of the specified Amazon Resource Name (ARN) of virtual tapes. If a <code>TapeARN</code> is not specified, returns a description of all virtual tapes associated with the specified gateway. This operation is only supported in the tape gateway type.</p>
describeTapes :: forall eff. DescribeTapesInput -> Aff (err :: AWS.RequestError | eff) DescribeTapesOutput
describeTapes = AWS.request serviceName "DescribeTapes" 


-- | <p>Returns information about the upload buffer of a gateway. This operation is supported for the stored volume, cached volume and tape gateway types.</p> <p>The response includes disk IDs that are configured as upload buffer space, and it includes the amount of upload buffer space allocated and used.</p>
describeUploadBuffer :: forall eff. DescribeUploadBufferInput -> Aff (err :: AWS.RequestError | eff) DescribeUploadBufferOutput
describeUploadBuffer = AWS.request serviceName "DescribeUploadBuffer" 


-- | <p>Returns a description of virtual tape library (VTL) devices for the specified tape gateway. In the response, AWS Storage Gateway returns VTL device information.</p> <p>This operation is only supported in the tape gateway type.</p>
describeVTLDevices :: forall eff. DescribeVTLDevicesInput -> Aff (err :: AWS.RequestError | eff) DescribeVTLDevicesOutput
describeVTLDevices = AWS.request serviceName "DescribeVTLDevices" 


-- | <p>Returns information about the working storage of a gateway. This operation is only supported in the stored volumes gateway type. This operation is deprecated in cached volumes API version (20120630). Use DescribeUploadBuffer instead.</p> <note> <p>Working storage is also referred to as upload buffer. You can also use the DescribeUploadBuffer operation to add upload buffer to a stored volume gateway.</p> </note> <p>The response includes disk IDs that are configured as working storage, and it includes the amount of working storage allocated and used.</p>
describeWorkingStorage :: forall eff. DescribeWorkingStorageInput -> Aff (err :: AWS.RequestError | eff) DescribeWorkingStorageOutput
describeWorkingStorage = AWS.request serviceName "DescribeWorkingStorage" 


-- | <p>Disables a tape gateway when the gateway is no longer functioning. For example, if your gateway VM is damaged, you can disable the gateway so you can recover virtual tapes.</p> <p>Use this operation for a tape gateway that is not reachable or not functioning. This operation is only supported in the tape gateway type.</p> <important> <p>Once a gateway is disabled it cannot be enabled.</p> </important>
disableGateway :: forall eff. DisableGatewayInput -> Aff (err :: AWS.RequestError | eff) DisableGatewayOutput
disableGateway = AWS.request serviceName "DisableGateway" 


-- | <p>Gets a list of the file shares for a specific file gateway, or the list of file shares that belong to the calling user account. This operation is only supported in the file gateway type.</p>
listFileShares :: forall eff. ListFileSharesInput -> Aff (err :: AWS.RequestError | eff) ListFileSharesOutput
listFileShares = AWS.request serviceName "ListFileShares" 


-- | <p>Lists gateways owned by an AWS account in a region specified in the request. The returned list is ordered by gateway Amazon Resource Name (ARN).</p> <p>By default, the operation returns a maximum of 100 gateways. This operation supports pagination that allows you to optionally reduce the number of gateways returned in a response.</p> <p>If you have more gateways than are returned in a response (that is, the response returns only a truncated list of your gateways), the response contains a marker that you can specify in your next request to fetch the next page of gateways.</p>
listGateways :: forall eff. ListGatewaysInput -> Aff (err :: AWS.RequestError | eff) ListGatewaysOutput
listGateways = AWS.request serviceName "ListGateways" 


-- | <p>Returns a list of the gateway's local disks. To specify which gateway to describe, you use the Amazon Resource Name (ARN) of the gateway in the body of the request.</p> <p>The request returns a list of all disks, specifying which are configured as working storage, cache storage, or stored volume or not configured at all. The response includes a <code>DiskStatus</code> field. This field can have a value of present (the disk is available to use), missing (the disk is no longer connected to the gateway), or mismatch (the disk node is occupied by a disk that has incorrect metadata or the disk content is corrupted).</p>
listLocalDisks :: forall eff. ListLocalDisksInput -> Aff (err :: AWS.RequestError | eff) ListLocalDisksOutput
listLocalDisks = AWS.request serviceName "ListLocalDisks" 


-- | <p>Lists the tags that have been added to the specified resource. This operation is only supported in the cached volume, stored volume and tape gateway type.</p>
listTagsForResource :: forall eff. ListTagsForResourceInput -> Aff (err :: AWS.RequestError | eff) ListTagsForResourceOutput
listTagsForResource = AWS.request serviceName "ListTagsForResource" 


-- | <p>Lists virtual tapes in your virtual tape library (VTL) and your virtual tape shelf (VTS). You specify the tapes to list by specifying one or more tape Amazon Resource Names (ARNs). If you don't specify a tape ARN, the operation lists all virtual tapes in both your VTL and VTS.</p> <p>This operation supports pagination. By default, the operation returns a maximum of up to 100 tapes. You can optionally specify the <code>Limit</code> parameter in the body to limit the number of tapes in the response. If the number of tapes returned in the response is truncated, the response includes a <code>Marker</code> element that you can use in your subsequent request to retrieve the next set of tapes. This operation is only supported in the tape gateway type.</p>
listTapes :: forall eff. ListTapesInput -> Aff (err :: AWS.RequestError | eff) ListTapesOutput
listTapes = AWS.request serviceName "ListTapes" 


-- | <p>Lists iSCSI initiators that are connected to a volume. You can use this operation to determine whether a volume is being used or not. This operation is only supported in the cached volume and stored volume gateway types.</p>
listVolumeInitiators :: forall eff. ListVolumeInitiatorsInput -> Aff (err :: AWS.RequestError | eff) ListVolumeInitiatorsOutput
listVolumeInitiators = AWS.request serviceName "ListVolumeInitiators" 


-- | <p>Lists the recovery points for a specified gateway. This operation is only supported in the cached volume gateway type.</p> <p>Each cache volume has one recovery point. A volume recovery point is a point in time at which all data of the volume is consistent and from which you can create a snapshot or clone a new cached volume from a source volume. To create a snapshot from a volume recovery point use the <a>CreateSnapshotFromVolumeRecoveryPoint</a> operation.</p>
listVolumeRecoveryPoints :: forall eff. ListVolumeRecoveryPointsInput -> Aff (err :: AWS.RequestError | eff) ListVolumeRecoveryPointsOutput
listVolumeRecoveryPoints = AWS.request serviceName "ListVolumeRecoveryPoints" 


-- | <p>Lists the iSCSI stored volumes of a gateway. Results are sorted by volume ARN. The response includes only the volume ARNs. If you want additional volume information, use the <a>DescribeStorediSCSIVolumes</a> or the <a>DescribeCachediSCSIVolumes</a> API.</p> <p>The operation supports pagination. By default, the operation returns a maximum of up to 100 volumes. You can optionally specify the <code>Limit</code> field in the body to limit the number of volumes in the response. If the number of volumes returned in the response is truncated, the response includes a Marker field. You can use this Marker value in your subsequent request to retrieve the next set of volumes. This operation is only supported in the cached volume and stored volume gateway types.</p>
listVolumes :: forall eff. ListVolumesInput -> Aff (err :: AWS.RequestError | eff) ListVolumesOutput
listVolumes = AWS.request serviceName "ListVolumes" 


-- | <p>Sends you notification when all file data written to the NFS file share has been uploaded to Amazon S3.</p> <p>AWS Storage Gateway can send a notification through Amazon CloudWatch Events when all files written to your file share up to that point in time have been uploaded to Amazon S3. These files include files written to the NFS file share up to the time that you make a request for notification. When the upload is done, Storage Gateway sends you notification through an Amazon CloudWatch event. You can configure CloudWatch Events to sent the notification through event targets such as email, SNS or a Lambda function. text or Lambda functions. This operation is only supported in the file gateway type.</p>
notifyWhenUploaded :: forall eff. NotifyWhenUploadedInput -> Aff (err :: AWS.RequestError | eff) NotifyWhenUploadedOutput
notifyWhenUploaded = AWS.request serviceName "NotifyWhenUploaded" 


-- | <p>Refreshes the cache for the specified file share. This operation finds objects in the Amazon S3 bucket that were added, removed or replaced since the gateway last listed the bucket's contents and cached the results. This operation is only supported in the file gateway type.</p>
refreshCache :: forall eff. RefreshCacheInput -> Aff (err :: AWS.RequestError | eff) RefreshCacheOutput
refreshCache = AWS.request serviceName "RefreshCache" 


-- | <p>Removes one or more tags from the specified resource. This operation is only supported in the cached volume, stored volume and tape gateway types.</p>
removeTagsFromResource :: forall eff. RemoveTagsFromResourceInput -> Aff (err :: AWS.RequestError | eff) RemoveTagsFromResourceOutput
removeTagsFromResource = AWS.request serviceName "RemoveTagsFromResource" 


-- | <p>Resets all cache disks that have encountered a error and makes the disks available for reconfiguration as cache storage. If your cache disk encounters a error, the gateway prevents read and write operations on virtual tapes in the gateway. For example, an error can occur when a disk is corrupted or removed from the gateway. When a cache is reset, the gateway loses its cache storage. At this point you can reconfigure the disks as cache disks. This operation is only supported in the cached volume, tape and file gateway types.</p> <important> <p>If the cache disk you are resetting contains data that has not been uploaded to Amazon S3 yet, that data can be lost. After you reset cache disks, there will be no configured cache disks left in the gateway, so you must configure at least one new cache disk for your gateway to function properly.</p> </important>
resetCache :: forall eff. ResetCacheInput -> Aff (err :: AWS.RequestError | eff) ResetCacheOutput
resetCache = AWS.request serviceName "ResetCache" 


-- | <p>Retrieves an archived virtual tape from the virtual tape shelf (VTS) to a tape gateway. Virtual tapes archived in the VTS are not associated with any gateway. However after a tape is retrieved, it is associated with a gateway, even though it is also listed in the VTS, that is, archive. This operation is only supported in the tape gateway type.</p> <p>Once a tape is successfully retrieved to a gateway, it cannot be retrieved again to another gateway. You must archive the tape again before you can retrieve it to another gateway. This operation is only supported in the tape gateway type.</p>
retrieveTapeArchive :: forall eff. RetrieveTapeArchiveInput -> Aff (err :: AWS.RequestError | eff) RetrieveTapeArchiveOutput
retrieveTapeArchive = AWS.request serviceName "RetrieveTapeArchive" 


-- | <p>Retrieves the recovery point for the specified virtual tape. This operation is only supported in the tape gateway type.</p> <p>A recovery point is a point in time view of a virtual tape at which all the data on the tape is consistent. If your gateway crashes, virtual tapes that have recovery points can be recovered to a new gateway.</p> <note> <p>The virtual tape can be retrieved to only one gateway. The retrieved tape is read-only. The virtual tape can be retrieved to only a tape gateway. There is no charge for retrieving recovery points.</p> </note>
retrieveTapeRecoveryPoint :: forall eff. RetrieveTapeRecoveryPointInput -> Aff (err :: AWS.RequestError | eff) RetrieveTapeRecoveryPointOutput
retrieveTapeRecoveryPoint = AWS.request serviceName "RetrieveTapeRecoveryPoint" 


-- | <p>Sets the password for your VM local console. When you log in to the local console for the first time, you log in to the VM with the default credentials. We recommend that you set a new password. You don't need to know the default password to set a new password.</p>
setLocalConsolePassword :: forall eff. SetLocalConsolePasswordInput -> Aff (err :: AWS.RequestError | eff) SetLocalConsolePasswordOutput
setLocalConsolePassword = AWS.request serviceName "SetLocalConsolePassword" 


-- | <p>Shuts down a gateway. To specify which gateway to shut down, use the Amazon Resource Name (ARN) of the gateway in the body of your request.</p> <p>The operation shuts down the gateway service component running in the gateway's virtual machine (VM) and not the host VM.</p> <note> <p>If you want to shut down the VM, it is recommended that you first shut down the gateway component in the VM to avoid unpredictable conditions.</p> </note> <p>After the gateway is shutdown, you cannot call any other API except <a>StartGateway</a>, <a>DescribeGatewayInformation</a>, and <a>ListGateways</a>. For more information, see <a>ActivateGateway</a>. Your applications cannot read from or write to the gateway's storage volumes, and there are no snapshots taken.</p> <note> <p>When you make a shutdown request, you will get a <code>200 OK</code> success response immediately. However, it might take some time for the gateway to shut down. You can call the <a>DescribeGatewayInformation</a> API to check the status. For more information, see <a>ActivateGateway</a>.</p> </note> <p>If do not intend to use the gateway again, you must delete the gateway (using <a>DeleteGateway</a>) to no longer pay software charges associated with the gateway.</p>
shutdownGateway :: forall eff. ShutdownGatewayInput -> Aff (err :: AWS.RequestError | eff) ShutdownGatewayOutput
shutdownGateway = AWS.request serviceName "ShutdownGateway" 


-- | <p>Starts a gateway that you previously shut down (see <a>ShutdownGateway</a>). After the gateway starts, you can then make other API calls, your applications can read from or write to the gateway's storage volumes and you will be able to take snapshot backups.</p> <note> <p>When you make a request, you will get a 200 OK success response immediately. However, it might take some time for the gateway to be ready. You should call <a>DescribeGatewayInformation</a> and check the status before making any additional API calls. For more information, see <a>ActivateGateway</a>.</p> </note> <p>To specify which gateway to start, use the Amazon Resource Name (ARN) of the gateway in your request.</p>
startGateway :: forall eff. StartGatewayInput -> Aff (err :: AWS.RequestError | eff) StartGatewayOutput
startGateway = AWS.request serviceName "StartGateway" 


-- | <p>Updates the bandwidth rate limits of a gateway. You can update both the upload and download bandwidth rate limit or specify only one of the two. If you don't set a bandwidth rate limit, the existing rate limit remains.</p> <p>By default, a gateway's bandwidth rate limits are not set. If you don't set any limit, the gateway does not have any limitations on its bandwidth usage and could potentially use the maximum available bandwidth.</p> <p>To specify which gateway to update, use the Amazon Resource Name (ARN) of the gateway in your request.</p>
updateBandwidthRateLimit :: forall eff. UpdateBandwidthRateLimitInput -> Aff (err :: AWS.RequestError | eff) UpdateBandwidthRateLimitOutput
updateBandwidthRateLimit = AWS.request serviceName "UpdateBandwidthRateLimit" 


-- | <p>Updates the Challenge-Handshake Authentication Protocol (CHAP) credentials for a specified iSCSI target. By default, a gateway does not have CHAP enabled; however, for added security, you might use it.</p> <important> <p>When you update CHAP credentials, all existing connections on the target are closed and initiators must reconnect with the new credentials.</p> </important>
updateChapCredentials :: forall eff. UpdateChapCredentialsInput -> Aff (err :: AWS.RequestError | eff) UpdateChapCredentialsOutput
updateChapCredentials = AWS.request serviceName "UpdateChapCredentials" 


-- | <p>Updates a gateway's metadata, which includes the gateway's name and time zone. To specify which gateway to update, use the Amazon Resource Name (ARN) of the gateway in your request.</p> <note> <p>For Gateways activated after September 2, 2015, the gateway's ARN contains the gateway ID rather than the gateway name. However, changing the name of the gateway has no effect on the gateway's ARN.</p> </note>
updateGatewayInformation :: forall eff. UpdateGatewayInformationInput -> Aff (err :: AWS.RequestError | eff) UpdateGatewayInformationOutput
updateGatewayInformation = AWS.request serviceName "UpdateGatewayInformation" 


-- | <p>Updates the gateway virtual machine (VM) software. The request immediately triggers the software update.</p> <note> <p>When you make this request, you get a <code>200 OK</code> success response immediately. However, it might take some time for the update to complete. You can call <a>DescribeGatewayInformation</a> to verify the gateway is in the <code>STATE_RUNNING</code> state.</p> </note> <important> <p>A software update forces a system restart of your gateway. You can minimize the chance of any disruption to your applications by increasing your iSCSI Initiators' timeouts. For more information about increasing iSCSI Initiator timeouts for Windows and Linux, see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorWindowsClient.html#CustomizeWindowsiSCSISettings">Customizing Your Windows iSCSI Settings</a> and <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorRedHatClient.html#CustomizeLinuxiSCSISettings">Customizing Your Linux iSCSI Settings</a>, respectively.</p> </important>
updateGatewaySoftwareNow :: forall eff. UpdateGatewaySoftwareNowInput -> Aff (err :: AWS.RequestError | eff) UpdateGatewaySoftwareNowOutput
updateGatewaySoftwareNow = AWS.request serviceName "UpdateGatewaySoftwareNow" 


-- | <p>Updates a gateway's weekly maintenance start time information, including day and time of the week. The maintenance time is the time in your gateway's time zone.</p>
updateMaintenanceStartTime :: forall eff. UpdateMaintenanceStartTimeInput -> Aff (err :: AWS.RequestError | eff) UpdateMaintenanceStartTimeOutput
updateMaintenanceStartTime = AWS.request serviceName "UpdateMaintenanceStartTime" 


-- | <p>Updates a file share. This operation is only supported in the file gateway type.</p> <note> <p>To leave a file share field unchanged, set the corresponding input field to null.</p> </note> <p>Updates the following file share setting:</p> <ul> <li> <p>Default storage class for your S3 bucket</p> </li> <li> <p>Metadata defaults for your S3 bucket</p> </li> <li> <p>Allowed NFS clients for your file share</p> </li> <li> <p>Squash settings</p> </li> <li> <p>Write status of your file share</p> </li> </ul> <note> <p>To leave a file share field unchanged, set the corresponding input field to null. This operation is only supported in file gateways.</p> </note>
updateNFSFileShare :: forall eff. UpdateNFSFileShareInput -> Aff (err :: AWS.RequestError | eff) UpdateNFSFileShareOutput
updateNFSFileShare = AWS.request serviceName "UpdateNFSFileShare" 


-- | <p>Updates a snapshot schedule configured for a gateway volume. This operation is only supported in the cached volume and stored volume gateway types.</p> <p>The default snapshot schedule for volume is once every 24 hours, starting at the creation time of the volume. You can use this API to change the snapshot schedule configured for the volume.</p> <p>In the request you must identify the gateway volume whose snapshot schedule you want to update, and the schedule information, including when you want the snapshot to begin on a day and the frequency (in hours) of snapshots.</p>
updateSnapshotSchedule :: forall eff. UpdateSnapshotScheduleInput -> Aff (err :: AWS.RequestError | eff) UpdateSnapshotScheduleOutput
updateSnapshotSchedule = AWS.request serviceName "UpdateSnapshotSchedule" 


-- | <p>Updates the type of medium changer in a tape gateway. When you activate a tape gateway, you select a medium changer type for the tape gateway. This operation enables you to select a different type of medium changer after a tape gateway is activated. This operation is only supported in the tape gateway type.</p>
updateVTLDeviceType :: forall eff. UpdateVTLDeviceTypeInput -> Aff (err :: AWS.RequestError | eff) UpdateVTLDeviceTypeOutput
updateVTLDeviceType = AWS.request serviceName "UpdateVTLDeviceType" 


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>ActivateGatewayInput$ActivationKey</a> </p> </li> <li> <p> <a>ActivateGatewayInput$GatewayName</a> </p> </li> <li> <p> <a>ActivateGatewayInput$GatewayRegion</a> </p> </li> <li> <p> <a>ActivateGatewayInput$GatewayTimezone</a> </p> </li> <li> <p> <a>ActivateGatewayInput$GatewayType</a> </p> </li> <li> <p> <a>ActivateGatewayInput$TapeDriveType</a> </p> </li> <li> <p> <a>ActivateGatewayInput$MediumChangerType</a> </p> </li> </ul>
newtype ActivateGatewayInput = ActivateGatewayInput 
  { "ActivationKey" :: (ActivationKey)
  , "GatewayName" :: (GatewayName)
  , "GatewayTimezone" :: (GatewayTimezone)
  , "GatewayRegion" :: (RegionId)
  , "GatewayType" :: NullOrUndefined (GatewayType)
  , "TapeDriveType" :: NullOrUndefined (TapeDriveType)
  , "MediumChangerType" :: NullOrUndefined (MediumChangerType)
  }
derive instance newtypeActivateGatewayInput :: Newtype ActivateGatewayInput _


-- | <p>AWS Storage Gateway returns the Amazon Resource Name (ARN) of the activated gateway. It is a string made of information such as your account, gateway name, and region. This ARN is used to reference the gateway in other API operations as well as resource-based authorization.</p> <note> <p>For gateways activated prior to September 02, 2015, the gateway ARN contains the gateway name rather than the gateway ID. Changing the name of the gateway has no effect on the gateway ARN.</p> </note>
newtype ActivateGatewayOutput = ActivateGatewayOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeActivateGatewayOutput :: Newtype ActivateGatewayOutput _


newtype ActivationKey = ActivationKey String
derive instance newtypeActivationKey :: Newtype ActivationKey _


newtype AddCacheInput = AddCacheInput 
  { "GatewayARN" :: (GatewayARN)
  , "DiskIds" :: (DiskIds)
  }
derive instance newtypeAddCacheInput :: Newtype AddCacheInput _


newtype AddCacheOutput = AddCacheOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeAddCacheOutput :: Newtype AddCacheOutput _


-- | <p>AddTagsToResourceInput</p>
newtype AddTagsToResourceInput = AddTagsToResourceInput 
  { "ResourceARN" :: (ResourceARN)
  , "Tags" :: (Tags)
  }
derive instance newtypeAddTagsToResourceInput :: Newtype AddTagsToResourceInput _


-- | <p>AddTagsToResourceOutput</p>
newtype AddTagsToResourceOutput = AddTagsToResourceOutput 
  { "ResourceARN" :: NullOrUndefined (ResourceARN)
  }
derive instance newtypeAddTagsToResourceOutput :: Newtype AddTagsToResourceOutput _


newtype AddUploadBufferInput = AddUploadBufferInput 
  { "GatewayARN" :: (GatewayARN)
  , "DiskIds" :: (DiskIds)
  }
derive instance newtypeAddUploadBufferInput :: Newtype AddUploadBufferInput _


newtype AddUploadBufferOutput = AddUploadBufferOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeAddUploadBufferOutput :: Newtype AddUploadBufferOutput _


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>AddWorkingStorageInput$DiskIds</a> </p> </li> </ul>
newtype AddWorkingStorageInput = AddWorkingStorageInput 
  { "GatewayARN" :: (GatewayARN)
  , "DiskIds" :: (DiskIds)
  }
derive instance newtypeAddWorkingStorageInput :: Newtype AddWorkingStorageInput _


-- | <p>A JSON object containing the of the gateway for which working storage was configured.</p>
newtype AddWorkingStorageOutput = AddWorkingStorageOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeAddWorkingStorageOutput :: Newtype AddWorkingStorageOutput _


newtype BandwidthDownloadRateLimit = BandwidthDownloadRateLimit Number
derive instance newtypeBandwidthDownloadRateLimit :: Newtype BandwidthDownloadRateLimit _


newtype BandwidthType = BandwidthType String
derive instance newtypeBandwidthType :: Newtype BandwidthType _


newtype BandwidthUploadRateLimit = BandwidthUploadRateLimit Number
derive instance newtypeBandwidthUploadRateLimit :: Newtype BandwidthUploadRateLimit _


-- | <p>Describes an iSCSI cached volume.</p>
newtype CachediSCSIVolume = CachediSCSIVolume 
  { "VolumeARN" :: NullOrUndefined (VolumeARN)
  , "VolumeId" :: NullOrUndefined (VolumeId)
  , "VolumeType" :: NullOrUndefined (VolumeType)
  , "VolumeStatus" :: NullOrUndefined (VolumeStatus)
  , "VolumeSizeInBytes" :: NullOrUndefined (Number)
  , "VolumeProgress" :: NullOrUndefined (DoubleObject)
  , "SourceSnapshotId" :: NullOrUndefined (SnapshotId)
  , "VolumeiSCSIAttributes" :: NullOrUndefined (VolumeiSCSIAttributes)
  , "CreatedDate" :: NullOrUndefined (CreatedDate)
  , "VolumeUsedInBytes" :: NullOrUndefined (VolumeUsedInBytes)
  }
derive instance newtypeCachediSCSIVolume :: Newtype CachediSCSIVolume _


newtype CachediSCSIVolumes = CachediSCSIVolumes (Array CachediSCSIVolume)
derive instance newtypeCachediSCSIVolumes :: Newtype CachediSCSIVolumes _


-- | <p>CancelArchivalInput</p>
newtype CancelArchivalInput = CancelArchivalInput 
  { "GatewayARN" :: (GatewayARN)
  , "TapeARN" :: (TapeARN)
  }
derive instance newtypeCancelArchivalInput :: Newtype CancelArchivalInput _


-- | <p>CancelArchivalOutput</p>
newtype CancelArchivalOutput = CancelArchivalOutput 
  { "TapeARN" :: NullOrUndefined (TapeARN)
  }
derive instance newtypeCancelArchivalOutput :: Newtype CancelArchivalOutput _


-- | <p>CancelRetrievalInput</p>
newtype CancelRetrievalInput = CancelRetrievalInput 
  { "GatewayARN" :: (GatewayARN)
  , "TapeARN" :: (TapeARN)
  }
derive instance newtypeCancelRetrievalInput :: Newtype CancelRetrievalInput _


-- | <p>CancelRetrievalOutput</p>
newtype CancelRetrievalOutput = CancelRetrievalOutput 
  { "TapeARN" :: NullOrUndefined (TapeARN)
  }
derive instance newtypeCancelRetrievalOutput :: Newtype CancelRetrievalOutput _


newtype ChapCredentials = ChapCredentials (Array ChapInfo)
derive instance newtypeChapCredentials :: Newtype ChapCredentials _


-- | <p>Describes Challenge-Handshake Authentication Protocol (CHAP) information that supports authentication between your gateway and iSCSI initiators.</p>
newtype ChapInfo = ChapInfo 
  { "TargetARN" :: NullOrUndefined (TargetARN)
  , "SecretToAuthenticateInitiator" :: NullOrUndefined (ChapSecret)
  , "InitiatorName" :: NullOrUndefined (IqnName)
  , "SecretToAuthenticateTarget" :: NullOrUndefined (ChapSecret)
  }
derive instance newtypeChapInfo :: Newtype ChapInfo _


newtype ChapSecret = ChapSecret String
derive instance newtypeChapSecret :: Newtype ChapSecret _


newtype ClientToken = ClientToken String
derive instance newtypeClientToken :: Newtype ClientToken _


newtype CreateCachediSCSIVolumeInput = CreateCachediSCSIVolumeInput 
  { "GatewayARN" :: (GatewayARN)
  , "VolumeSizeInBytes" :: (Number)
  , "SnapshotId" :: NullOrUndefined (SnapshotId)
  , "TargetName" :: (TargetName)
  , "SourceVolumeARN" :: NullOrUndefined (VolumeARN)
  , "NetworkInterfaceId" :: (NetworkInterfaceId)
  , "ClientToken" :: (ClientToken)
  }
derive instance newtypeCreateCachediSCSIVolumeInput :: Newtype CreateCachediSCSIVolumeInput _


newtype CreateCachediSCSIVolumeOutput = CreateCachediSCSIVolumeOutput 
  { "VolumeARN" :: NullOrUndefined (VolumeARN)
  , "TargetARN" :: NullOrUndefined (TargetARN)
  }
derive instance newtypeCreateCachediSCSIVolumeOutput :: Newtype CreateCachediSCSIVolumeOutput _


-- | <p>CreateNFSFileShareInput</p>
newtype CreateNFSFileShareInput = CreateNFSFileShareInput 
  { "ClientToken" :: (ClientToken)
  , "NFSFileShareDefaults" :: NullOrUndefined (NFSFileShareDefaults)
  , "GatewayARN" :: (GatewayARN)
  , "KMSEncrypted" :: NullOrUndefined (Boolean)
  , "KMSKey" :: NullOrUndefined (KMSKey)
  , "Role" :: (Role)
  , "LocationARN" :: (LocationARN)
  , "DefaultStorageClass" :: NullOrUndefined (StorageClass)
  , "ClientList" :: NullOrUndefined (FileShareClientList)
  , "Squash" :: NullOrUndefined (Squash)
  , "ReadOnly" :: NullOrUndefined (Boolean)
  , "GuessMIMETypeEnabled" :: NullOrUndefined (Boolean)
  }
derive instance newtypeCreateNFSFileShareInput :: Newtype CreateNFSFileShareInput _


-- | <p>CreateNFSFileShareOutput</p>
newtype CreateNFSFileShareOutput = CreateNFSFileShareOutput 
  { "FileShareARN" :: NullOrUndefined (FileShareARN)
  }
derive instance newtypeCreateNFSFileShareOutput :: Newtype CreateNFSFileShareOutput _


newtype CreateSnapshotFromVolumeRecoveryPointInput = CreateSnapshotFromVolumeRecoveryPointInput 
  { "VolumeARN" :: (VolumeARN)
  , "SnapshotDescription" :: (SnapshotDescription)
  }
derive instance newtypeCreateSnapshotFromVolumeRecoveryPointInput :: Newtype CreateSnapshotFromVolumeRecoveryPointInput _


newtype CreateSnapshotFromVolumeRecoveryPointOutput = CreateSnapshotFromVolumeRecoveryPointOutput 
  { "SnapshotId" :: NullOrUndefined (SnapshotId)
  , "VolumeARN" :: NullOrUndefined (VolumeARN)
  , "VolumeRecoveryPointTime" :: NullOrUndefined (String)
  }
derive instance newtypeCreateSnapshotFromVolumeRecoveryPointOutput :: Newtype CreateSnapshotFromVolumeRecoveryPointOutput _


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>CreateSnapshotInput$SnapshotDescription</a> </p> </li> <li> <p> <a>CreateSnapshotInput$VolumeARN</a> </p> </li> </ul>
newtype CreateSnapshotInput = CreateSnapshotInput 
  { "VolumeARN" :: (VolumeARN)
  , "SnapshotDescription" :: (SnapshotDescription)
  }
derive instance newtypeCreateSnapshotInput :: Newtype CreateSnapshotInput _


-- | <p>A JSON object containing the following fields:</p>
newtype CreateSnapshotOutput = CreateSnapshotOutput 
  { "VolumeARN" :: NullOrUndefined (VolumeARN)
  , "SnapshotId" :: NullOrUndefined (SnapshotId)
  }
derive instance newtypeCreateSnapshotOutput :: Newtype CreateSnapshotOutput _


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>CreateStorediSCSIVolumeInput$DiskId</a> </p> </li> <li> <p> <a>CreateStorediSCSIVolumeInput$NetworkInterfaceId</a> </p> </li> <li> <p> <a>CreateStorediSCSIVolumeInput$PreserveExistingData</a> </p> </li> <li> <p> <a>CreateStorediSCSIVolumeInput$SnapshotId</a> </p> </li> <li> <p> <a>CreateStorediSCSIVolumeInput$TargetName</a> </p> </li> </ul>
newtype CreateStorediSCSIVolumeInput = CreateStorediSCSIVolumeInput 
  { "GatewayARN" :: (GatewayARN)
  , "DiskId" :: (DiskId)
  , "SnapshotId" :: NullOrUndefined (SnapshotId)
  , "PreserveExistingData" :: (Boolean)
  , "TargetName" :: (TargetName)
  , "NetworkInterfaceId" :: (NetworkInterfaceId)
  }
derive instance newtypeCreateStorediSCSIVolumeInput :: Newtype CreateStorediSCSIVolumeInput _


-- | <p>A JSON object containing the following fields:</p>
newtype CreateStorediSCSIVolumeOutput = CreateStorediSCSIVolumeOutput 
  { "VolumeARN" :: NullOrUndefined (VolumeARN)
  , "VolumeSizeInBytes" :: NullOrUndefined (Number)
  , "TargetARN" :: NullOrUndefined (TargetARN)
  }
derive instance newtypeCreateStorediSCSIVolumeOutput :: Newtype CreateStorediSCSIVolumeOutput _


-- | <p>CreateTapeWithBarcodeInput</p>
newtype CreateTapeWithBarcodeInput = CreateTapeWithBarcodeInput 
  { "GatewayARN" :: (GatewayARN)
  , "TapeSizeInBytes" :: (TapeSize)
  , "TapeBarcode" :: (TapeBarcode)
  }
derive instance newtypeCreateTapeWithBarcodeInput :: Newtype CreateTapeWithBarcodeInput _


-- | <p>CreateTapeOutput</p>
newtype CreateTapeWithBarcodeOutput = CreateTapeWithBarcodeOutput 
  { "TapeARN" :: NullOrUndefined (TapeARN)
  }
derive instance newtypeCreateTapeWithBarcodeOutput :: Newtype CreateTapeWithBarcodeOutput _


-- | <p>CreateTapesInput</p>
newtype CreateTapesInput = CreateTapesInput 
  { "GatewayARN" :: (GatewayARN)
  , "TapeSizeInBytes" :: (TapeSize)
  , "ClientToken" :: (ClientToken)
  , "NumTapesToCreate" :: (NumTapesToCreate)
  , "TapeBarcodePrefix" :: (TapeBarcodePrefix)
  }
derive instance newtypeCreateTapesInput :: Newtype CreateTapesInput _


-- | <p>CreateTapeOutput</p>
newtype CreateTapesOutput = CreateTapesOutput 
  { "TapeARNs" :: NullOrUndefined (TapeARNs)
  }
derive instance newtypeCreateTapesOutput :: Newtype CreateTapesOutput _


newtype CreatedDate = CreatedDate Number
derive instance newtypeCreatedDate :: Newtype CreatedDate _


newtype DayOfWeek = DayOfWeek Int
derive instance newtypeDayOfWeek :: Newtype DayOfWeek _


-- | <p>A JSON object containing the following fields:</p> <ul> <li> <p> <a>DeleteBandwidthRateLimitInput$BandwidthType</a> </p> </li> </ul>
newtype DeleteBandwidthRateLimitInput = DeleteBandwidthRateLimitInput 
  { "GatewayARN" :: (GatewayARN)
  , "BandwidthType" :: (BandwidthType)
  }
derive instance newtypeDeleteBandwidthRateLimitInput :: Newtype DeleteBandwidthRateLimitInput _


-- | <p>A JSON object containing the of the gateway whose bandwidth rate information was deleted.</p>
newtype DeleteBandwidthRateLimitOutput = DeleteBandwidthRateLimitOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeDeleteBandwidthRateLimitOutput :: Newtype DeleteBandwidthRateLimitOutput _


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>DeleteChapCredentialsInput$InitiatorName</a> </p> </li> <li> <p> <a>DeleteChapCredentialsInput$TargetARN</a> </p> </li> </ul>
newtype DeleteChapCredentialsInput = DeleteChapCredentialsInput 
  { "TargetARN" :: (TargetARN)
  , "InitiatorName" :: (IqnName)
  }
derive instance newtypeDeleteChapCredentialsInput :: Newtype DeleteChapCredentialsInput _


-- | <p>A JSON object containing the following fields:</p>
newtype DeleteChapCredentialsOutput = DeleteChapCredentialsOutput 
  { "TargetARN" :: NullOrUndefined (TargetARN)
  , "InitiatorName" :: NullOrUndefined (IqnName)
  }
derive instance newtypeDeleteChapCredentialsOutput :: Newtype DeleteChapCredentialsOutput _


-- | <p>DeleteFileShareInput</p>
newtype DeleteFileShareInput = DeleteFileShareInput 
  { "FileShareARN" :: (FileShareARN)
  , "ForceDelete" :: NullOrUndefined (Boolean)
  }
derive instance newtypeDeleteFileShareInput :: Newtype DeleteFileShareInput _


-- | <p>DeleteFileShareOutput</p>
newtype DeleteFileShareOutput = DeleteFileShareOutput 
  { "FileShareARN" :: NullOrUndefined (FileShareARN)
  }
derive instance newtypeDeleteFileShareOutput :: Newtype DeleteFileShareOutput _


-- | <p>A JSON object containing the ID of the gateway to delete.</p>
newtype DeleteGatewayInput = DeleteGatewayInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDeleteGatewayInput :: Newtype DeleteGatewayInput _


-- | <p>A JSON object containing the ID of the deleted gateway.</p>
newtype DeleteGatewayOutput = DeleteGatewayOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeDeleteGatewayOutput :: Newtype DeleteGatewayOutput _


newtype DeleteSnapshotScheduleInput = DeleteSnapshotScheduleInput 
  { "VolumeARN" :: (VolumeARN)
  }
derive instance newtypeDeleteSnapshotScheduleInput :: Newtype DeleteSnapshotScheduleInput _


newtype DeleteSnapshotScheduleOutput = DeleteSnapshotScheduleOutput 
  { "VolumeARN" :: NullOrUndefined (VolumeARN)
  }
derive instance newtypeDeleteSnapshotScheduleOutput :: Newtype DeleteSnapshotScheduleOutput _


-- | <p>DeleteTapeArchiveInput</p>
newtype DeleteTapeArchiveInput = DeleteTapeArchiveInput 
  { "TapeARN" :: (TapeARN)
  }
derive instance newtypeDeleteTapeArchiveInput :: Newtype DeleteTapeArchiveInput _


-- | <p>DeleteTapeArchiveOutput</p>
newtype DeleteTapeArchiveOutput = DeleteTapeArchiveOutput 
  { "TapeARN" :: NullOrUndefined (TapeARN)
  }
derive instance newtypeDeleteTapeArchiveOutput :: Newtype DeleteTapeArchiveOutput _


-- | <p>DeleteTapeInput</p>
newtype DeleteTapeInput = DeleteTapeInput 
  { "GatewayARN" :: (GatewayARN)
  , "TapeARN" :: (TapeARN)
  }
derive instance newtypeDeleteTapeInput :: Newtype DeleteTapeInput _


-- | <p>DeleteTapeOutput</p>
newtype DeleteTapeOutput = DeleteTapeOutput 
  { "TapeARN" :: NullOrUndefined (TapeARN)
  }
derive instance newtypeDeleteTapeOutput :: Newtype DeleteTapeOutput _


-- | <p>A JSON object containing the <a>DeleteVolumeInput$VolumeARN</a> to delete.</p>
newtype DeleteVolumeInput = DeleteVolumeInput 
  { "VolumeARN" :: (VolumeARN)
  }
derive instance newtypeDeleteVolumeInput :: Newtype DeleteVolumeInput _


-- | <p>A JSON object containing the of the storage volume that was deleted</p>
newtype DeleteVolumeOutput = DeleteVolumeOutput 
  { "VolumeARN" :: NullOrUndefined (VolumeARN)
  }
derive instance newtypeDeleteVolumeOutput :: Newtype DeleteVolumeOutput _


-- | <p>A JSON object containing the of the gateway.</p>
newtype DescribeBandwidthRateLimitInput = DescribeBandwidthRateLimitInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDescribeBandwidthRateLimitInput :: Newtype DescribeBandwidthRateLimitInput _


-- | <p>A JSON object containing the following fields:</p>
newtype DescribeBandwidthRateLimitOutput = DescribeBandwidthRateLimitOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "AverageUploadRateLimitInBitsPerSec" :: NullOrUndefined (BandwidthUploadRateLimit)
  , "AverageDownloadRateLimitInBitsPerSec" :: NullOrUndefined (BandwidthDownloadRateLimit)
  }
derive instance newtypeDescribeBandwidthRateLimitOutput :: Newtype DescribeBandwidthRateLimitOutput _


newtype DescribeCacheInput = DescribeCacheInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDescribeCacheInput :: Newtype DescribeCacheInput _


newtype DescribeCacheOutput = DescribeCacheOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "DiskIds" :: NullOrUndefined (DiskIds)
  , "CacheAllocatedInBytes" :: NullOrUndefined (Number)
  , "CacheUsedPercentage" :: NullOrUndefined (Number)
  , "CacheDirtyPercentage" :: NullOrUndefined (Number)
  , "CacheHitPercentage" :: NullOrUndefined (Number)
  , "CacheMissPercentage" :: NullOrUndefined (Number)
  }
derive instance newtypeDescribeCacheOutput :: Newtype DescribeCacheOutput _


newtype DescribeCachediSCSIVolumesInput = DescribeCachediSCSIVolumesInput 
  { "VolumeARNs" :: (VolumeARNs)
  }
derive instance newtypeDescribeCachediSCSIVolumesInput :: Newtype DescribeCachediSCSIVolumesInput _


-- | <p>A JSON object containing the following fields:</p>
newtype DescribeCachediSCSIVolumesOutput = DescribeCachediSCSIVolumesOutput 
  { "CachediSCSIVolumes" :: NullOrUndefined (CachediSCSIVolumes)
  }
derive instance newtypeDescribeCachediSCSIVolumesOutput :: Newtype DescribeCachediSCSIVolumesOutput _


-- | <p>A JSON object containing the Amazon Resource Name (ARN) of the iSCSI volume target.</p>
newtype DescribeChapCredentialsInput = DescribeChapCredentialsInput 
  { "TargetARN" :: (TargetARN)
  }
derive instance newtypeDescribeChapCredentialsInput :: Newtype DescribeChapCredentialsInput _


-- | <p>A JSON object containing a .</p>
newtype DescribeChapCredentialsOutput = DescribeChapCredentialsOutput 
  { "ChapCredentials" :: NullOrUndefined (ChapCredentials)
  }
derive instance newtypeDescribeChapCredentialsOutput :: Newtype DescribeChapCredentialsOutput _


-- | <p>A JSON object containing the ID of the gateway.</p>
newtype DescribeGatewayInformationInput = DescribeGatewayInformationInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDescribeGatewayInformationInput :: Newtype DescribeGatewayInformationInput _


-- | <p>A JSON object containing the following fields:</p>
newtype DescribeGatewayInformationOutput = DescribeGatewayInformationOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "GatewayId" :: NullOrUndefined (GatewayId)
  , "GatewayName" :: NullOrUndefined (String)
  , "GatewayTimezone" :: NullOrUndefined (GatewayTimezone)
  , "GatewayState" :: NullOrUndefined (GatewayState)
  , "GatewayNetworkInterfaces" :: NullOrUndefined (GatewayNetworkInterfaces)
  , "GatewayType" :: NullOrUndefined (GatewayType)
  , "NextUpdateAvailabilityDate" :: NullOrUndefined (NextUpdateAvailabilityDate)
  , "LastSoftwareUpdate" :: NullOrUndefined (LastSoftwareUpdate)
  }
derive instance newtypeDescribeGatewayInformationOutput :: Newtype DescribeGatewayInformationOutput _


-- | <p>A JSON object containing the of the gateway.</p>
newtype DescribeMaintenanceStartTimeInput = DescribeMaintenanceStartTimeInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDescribeMaintenanceStartTimeInput :: Newtype DescribeMaintenanceStartTimeInput _


-- | <p>A JSON object containing the following fields:</p> <ul> <li> <p> <a>DescribeMaintenanceStartTimeOutput$DayOfWeek</a> </p> </li> <li> <p> <a>DescribeMaintenanceStartTimeOutput$HourOfDay</a> </p> </li> <li> <p> <a>DescribeMaintenanceStartTimeOutput$MinuteOfHour</a> </p> </li> <li> <p> <a>DescribeMaintenanceStartTimeOutput$Timezone</a> </p> </li> </ul>
newtype DescribeMaintenanceStartTimeOutput = DescribeMaintenanceStartTimeOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "HourOfDay" :: NullOrUndefined (HourOfDay)
  , "MinuteOfHour" :: NullOrUndefined (MinuteOfHour)
  , "DayOfWeek" :: NullOrUndefined (DayOfWeek)
  , "Timezone" :: NullOrUndefined (GatewayTimezone)
  }
derive instance newtypeDescribeMaintenanceStartTimeOutput :: Newtype DescribeMaintenanceStartTimeOutput _


-- | <p>DescribeNFSFileSharesInput</p>
newtype DescribeNFSFileSharesInput = DescribeNFSFileSharesInput 
  { "FileShareARNList" :: (FileShareARNList)
  }
derive instance newtypeDescribeNFSFileSharesInput :: Newtype DescribeNFSFileSharesInput _


-- | <p>DescribeNFSFileSharesOutput</p>
newtype DescribeNFSFileSharesOutput = DescribeNFSFileSharesOutput 
  { "NFSFileShareInfoList" :: NullOrUndefined (NFSFileShareInfoList)
  }
derive instance newtypeDescribeNFSFileSharesOutput :: Newtype DescribeNFSFileSharesOutput _


-- | <p>A JSON object containing the <a>DescribeSnapshotScheduleInput$VolumeARN</a> of the volume.</p>
newtype DescribeSnapshotScheduleInput = DescribeSnapshotScheduleInput 
  { "VolumeARN" :: (VolumeARN)
  }
derive instance newtypeDescribeSnapshotScheduleInput :: Newtype DescribeSnapshotScheduleInput _


newtype DescribeSnapshotScheduleOutput = DescribeSnapshotScheduleOutput 
  { "VolumeARN" :: NullOrUndefined (VolumeARN)
  , "StartAt" :: NullOrUndefined (HourOfDay)
  , "RecurrenceInHours" :: NullOrUndefined (RecurrenceInHours)
  , "Description" :: NullOrUndefined (Description)
  , "Timezone" :: NullOrUndefined (GatewayTimezone)
  }
derive instance newtypeDescribeSnapshotScheduleOutput :: Newtype DescribeSnapshotScheduleOutput _


-- | <p>A JSON object containing a list of <a>DescribeStorediSCSIVolumesInput$VolumeARNs</a>.</p>
newtype DescribeStorediSCSIVolumesInput = DescribeStorediSCSIVolumesInput 
  { "VolumeARNs" :: (VolumeARNs)
  }
derive instance newtypeDescribeStorediSCSIVolumesInput :: Newtype DescribeStorediSCSIVolumesInput _


newtype DescribeStorediSCSIVolumesOutput = DescribeStorediSCSIVolumesOutput 
  { "StorediSCSIVolumes" :: NullOrUndefined (StorediSCSIVolumes)
  }
derive instance newtypeDescribeStorediSCSIVolumesOutput :: Newtype DescribeStorediSCSIVolumesOutput _


-- | <p>DescribeTapeArchivesInput</p>
newtype DescribeTapeArchivesInput = DescribeTapeArchivesInput 
  { "TapeARNs" :: NullOrUndefined (TapeARNs)
  , "Marker" :: NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeDescribeTapeArchivesInput :: Newtype DescribeTapeArchivesInput _


-- | <p>DescribeTapeArchivesOutput</p>
newtype DescribeTapeArchivesOutput = DescribeTapeArchivesOutput 
  { "TapeArchives" :: NullOrUndefined (TapeArchives)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeDescribeTapeArchivesOutput :: Newtype DescribeTapeArchivesOutput _


-- | <p>DescribeTapeRecoveryPointsInput</p>
newtype DescribeTapeRecoveryPointsInput = DescribeTapeRecoveryPointsInput 
  { "GatewayARN" :: (GatewayARN)
  , "Marker" :: NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeDescribeTapeRecoveryPointsInput :: Newtype DescribeTapeRecoveryPointsInput _


-- | <p>DescribeTapeRecoveryPointsOutput</p>
newtype DescribeTapeRecoveryPointsOutput = DescribeTapeRecoveryPointsOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "TapeRecoveryPointInfos" :: NullOrUndefined (TapeRecoveryPointInfos)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeDescribeTapeRecoveryPointsOutput :: Newtype DescribeTapeRecoveryPointsOutput _


-- | <p>DescribeTapesInput</p>
newtype DescribeTapesInput = DescribeTapesInput 
  { "GatewayARN" :: (GatewayARN)
  , "TapeARNs" :: NullOrUndefined (TapeARNs)
  , "Marker" :: NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeDescribeTapesInput :: Newtype DescribeTapesInput _


-- | <p>DescribeTapesOutput</p>
newtype DescribeTapesOutput = DescribeTapesOutput 
  { "Tapes" :: NullOrUndefined (Tapes)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeDescribeTapesOutput :: Newtype DescribeTapesOutput _


newtype DescribeUploadBufferInput = DescribeUploadBufferInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDescribeUploadBufferInput :: Newtype DescribeUploadBufferInput _


newtype DescribeUploadBufferOutput = DescribeUploadBufferOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "DiskIds" :: NullOrUndefined (DiskIds)
  , "UploadBufferUsedInBytes" :: NullOrUndefined (Number)
  , "UploadBufferAllocatedInBytes" :: NullOrUndefined (Number)
  }
derive instance newtypeDescribeUploadBufferOutput :: Newtype DescribeUploadBufferOutput _


-- | <p>DescribeVTLDevicesInput</p>
newtype DescribeVTLDevicesInput = DescribeVTLDevicesInput 
  { "GatewayARN" :: (GatewayARN)
  , "VTLDeviceARNs" :: NullOrUndefined (VTLDeviceARNs)
  , "Marker" :: NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeDescribeVTLDevicesInput :: Newtype DescribeVTLDevicesInput _


-- | <p>DescribeVTLDevicesOutput</p>
newtype DescribeVTLDevicesOutput = DescribeVTLDevicesOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "VTLDevices" :: NullOrUndefined (VTLDevices)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeDescribeVTLDevicesOutput :: Newtype DescribeVTLDevicesOutput _


-- | <p>A JSON object containing the of the gateway.</p>
newtype DescribeWorkingStorageInput = DescribeWorkingStorageInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDescribeWorkingStorageInput :: Newtype DescribeWorkingStorageInput _


-- | <p>A JSON object containing the following fields:</p>
newtype DescribeWorkingStorageOutput = DescribeWorkingStorageOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "DiskIds" :: NullOrUndefined (DiskIds)
  , "WorkingStorageUsedInBytes" :: NullOrUndefined (Number)
  , "WorkingStorageAllocatedInBytes" :: NullOrUndefined (Number)
  }
derive instance newtypeDescribeWorkingStorageOutput :: Newtype DescribeWorkingStorageOutput _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


newtype DeviceType = DeviceType String
derive instance newtypeDeviceType :: Newtype DeviceType _


-- | <p>Lists iSCSI information about a VTL device.</p>
newtype DeviceiSCSIAttributes = DeviceiSCSIAttributes 
  { "TargetARN" :: NullOrUndefined (TargetARN)
  , "NetworkInterfaceId" :: NullOrUndefined (NetworkInterfaceId)
  , "NetworkInterfacePort" :: NullOrUndefined (Int)
  , "ChapEnabled" :: NullOrUndefined (Boolean)
  }
derive instance newtypeDeviceiSCSIAttributes :: Newtype DeviceiSCSIAttributes _


-- | <p>DisableGatewayInput</p>
newtype DisableGatewayInput = DisableGatewayInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeDisableGatewayInput :: Newtype DisableGatewayInput _


-- | <p>DisableGatewayOutput</p>
newtype DisableGatewayOutput = DisableGatewayOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeDisableGatewayOutput :: Newtype DisableGatewayOutput _


newtype Disk = Disk 
  { "DiskId" :: NullOrUndefined (DiskId)
  , "DiskPath" :: NullOrUndefined (String)
  , "DiskNode" :: NullOrUndefined (String)
  , "DiskStatus" :: NullOrUndefined (String)
  , "DiskSizeInBytes" :: NullOrUndefined (Number)
  , "DiskAllocationType" :: NullOrUndefined (DiskAllocationType)
  , "DiskAllocationResource" :: NullOrUndefined (String)
  }
derive instance newtypeDisk :: Newtype Disk _


newtype DiskAllocationType = DiskAllocationType String
derive instance newtypeDiskAllocationType :: Newtype DiskAllocationType _


newtype DiskId = DiskId String
derive instance newtypeDiskId :: Newtype DiskId _


newtype DiskIds = DiskIds (Array DiskId)
derive instance newtypeDiskIds :: Newtype DiskIds _


newtype Disks = Disks (Array Disk)
derive instance newtypeDisks :: Newtype Disks _


newtype DoubleObject = DoubleObject Number
derive instance newtypeDoubleObject :: Newtype DoubleObject _


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _


-- | <p>The Amazon Resource Name (ARN) of the file share. </p>
newtype FileShareARN = FileShareARN String
derive instance newtypeFileShareARN :: Newtype FileShareARN _


newtype FileShareARNList = FileShareARNList (Array FileShareARN)
derive instance newtypeFileShareARNList :: Newtype FileShareARNList _


-- | <p>The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks. </p>
newtype FileShareClientList = FileShareClientList (Array IPV4AddressCIDR)
derive instance newtypeFileShareClientList :: Newtype FileShareClientList _


-- | <p>The ID of the file share. </p>
newtype FileShareId = FileShareId String
derive instance newtypeFileShareId :: Newtype FileShareId _


-- | <p>Describes a file share.</p>
newtype FileShareInfo = FileShareInfo 
  { "FileShareARN" :: NullOrUndefined (FileShareARN)
  , "FileShareId" :: NullOrUndefined (FileShareId)
  , "FileShareStatus" :: NullOrUndefined (FileShareStatus)
  , "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeFileShareInfo :: Newtype FileShareInfo _


newtype FileShareInfoList = FileShareInfoList (Array FileShareInfo)
derive instance newtypeFileShareInfoList :: Newtype FileShareInfoList _


-- | <p>The status of the file share. Possible values are CREATING, UPDATING, AVAILABLE and DELETING. </p>
newtype FileShareStatus = FileShareStatus String
derive instance newtypeFileShareStatus :: Newtype FileShareStatus _


-- | <p>The Amazon Resource Name (ARN) of the gateway. Use the <a>ListGateways</a> operation to return a list of gateways for your account and region.</p>
newtype GatewayARN = GatewayARN String
derive instance newtypeGatewayARN :: Newtype GatewayARN _


newtype GatewayId = GatewayId String
derive instance newtypeGatewayId :: Newtype GatewayId _


-- | <p>Describes a gateway object.</p>
newtype GatewayInfo = GatewayInfo 
  { "GatewayId" :: NullOrUndefined (GatewayId)
  , "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "GatewayType" :: NullOrUndefined (GatewayType)
  , "GatewayOperationalState" :: NullOrUndefined (GatewayOperationalState)
  , "GatewayName" :: NullOrUndefined (String)
  }
derive instance newtypeGatewayInfo :: Newtype GatewayInfo _


-- | <p>The name you configured for your gateway.</p>
newtype GatewayName = GatewayName String
derive instance newtypeGatewayName :: Newtype GatewayName _


newtype GatewayNetworkInterfaces = GatewayNetworkInterfaces (Array NetworkInterface)
derive instance newtypeGatewayNetworkInterfaces :: Newtype GatewayNetworkInterfaces _


newtype GatewayOperationalState = GatewayOperationalState String
derive instance newtypeGatewayOperationalState :: Newtype GatewayOperationalState _


newtype GatewayState = GatewayState String
derive instance newtypeGatewayState :: Newtype GatewayState _


newtype GatewayTimezone = GatewayTimezone String
derive instance newtypeGatewayTimezone :: Newtype GatewayTimezone _


newtype GatewayType = GatewayType String
derive instance newtypeGatewayType :: Newtype GatewayType _


newtype Gateways = Gateways (Array GatewayInfo)
derive instance newtypeGateways :: Newtype Gateways _


newtype HourOfDay = HourOfDay Int
derive instance newtypeHourOfDay :: Newtype HourOfDay _


newtype IPV4AddressCIDR = IPV4AddressCIDR String
derive instance newtypeIPV4AddressCIDR :: Newtype IPV4AddressCIDR _


newtype Initiator = Initiator String
derive instance newtypeInitiator :: Newtype Initiator _


newtype Initiators = Initiators (Array Initiator)
derive instance newtypeInitiators :: Newtype Initiators _


-- | <p>An internal server error has occurred during the request. For more information, see the error and message fields.</p>
newtype InternalServerError = InternalServerError 
  { "Message'" :: NullOrUndefined (String)
  , "Error'" :: NullOrUndefined (StorageGatewayError)
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _


-- | <p>An exception occurred because an invalid gateway request was issued to the service. For more information, see the error and message fields.</p>
newtype InvalidGatewayRequestException = InvalidGatewayRequestException 
  { "Message'" :: NullOrUndefined (String)
  , "Error'" :: NullOrUndefined (StorageGatewayError)
  }
derive instance newtypeInvalidGatewayRequestException :: Newtype InvalidGatewayRequestException _


newtype IqnName = IqnName String
derive instance newtypeIqnName :: Newtype IqnName _


-- | <p>The ARN of the KMS key used for Amazon S3 server side encryption. </p>
newtype KMSKey = KMSKey String
derive instance newtypeKMSKey :: Newtype KMSKey _


newtype LastSoftwareUpdate = LastSoftwareUpdate String
derive instance newtypeLastSoftwareUpdate :: Newtype LastSoftwareUpdate _


-- | <p>ListFileShareInput</p>
newtype ListFileSharesInput = ListFileSharesInput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "Limit" :: NullOrUndefined (PositiveIntObject)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListFileSharesInput :: Newtype ListFileSharesInput _


-- | <p>ListFileShareOutput</p>
newtype ListFileSharesOutput = ListFileSharesOutput 
  { "Marker" :: NullOrUndefined (Marker)
  , "NextMarker" :: NullOrUndefined (Marker)
  , "FileShareInfoList" :: NullOrUndefined (FileShareInfoList)
  }
derive instance newtypeListFileSharesOutput :: Newtype ListFileSharesOutput _


-- | <p>A JSON object containing zero or more of the following fields:</p> <ul> <li> <p> <a>ListGatewaysInput$Limit</a> </p> </li> <li> <p> <a>ListGatewaysInput$Marker</a> </p> </li> </ul>
newtype ListGatewaysInput = ListGatewaysInput 
  { "Marker" :: NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeListGatewaysInput :: Newtype ListGatewaysInput _


newtype ListGatewaysOutput = ListGatewaysOutput 
  { "Gateways" :: NullOrUndefined (Gateways)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListGatewaysOutput :: Newtype ListGatewaysOutput _


-- | <p>A JSON object containing the of the gateway.</p>
newtype ListLocalDisksInput = ListLocalDisksInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeListLocalDisksInput :: Newtype ListLocalDisksInput _


newtype ListLocalDisksOutput = ListLocalDisksOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "Disks" :: NullOrUndefined (Disks)
  }
derive instance newtypeListLocalDisksOutput :: Newtype ListLocalDisksOutput _


-- | <p>ListTagsForResourceInput</p>
newtype ListTagsForResourceInput = ListTagsForResourceInput 
  { "ResourceARN" :: (ResourceARN)
  , "Marker" :: NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeListTagsForResourceInput :: Newtype ListTagsForResourceInput _


-- | <p>ListTagsForResourceOutput</p>
newtype ListTagsForResourceOutput = ListTagsForResourceOutput 
  { "ResourceARN" :: NullOrUndefined (ResourceARN)
  , "Marker" :: NullOrUndefined (Marker)
  , "Tags" :: NullOrUndefined (Tags)
  }
derive instance newtypeListTagsForResourceOutput :: Newtype ListTagsForResourceOutput _


-- | <p>A JSON object that contains one or more of the following fields:</p> <ul> <li> <p> <a>ListTapesInput$Limit</a> </p> </li> <li> <p> <a>ListTapesInput$Marker</a> </p> </li> <li> <p> <a>ListTapesInput$TapeARNs</a> </p> </li> </ul>
newtype ListTapesInput = ListTapesInput 
  { "TapeARNs" :: NullOrUndefined (TapeARNs)
  , "Marker" :: NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeListTapesInput :: Newtype ListTapesInput _


-- | <p>A JSON object containing the following fields:</p> <ul> <li> <p> <a>ListTapesOutput$Marker</a> </p> </li> <li> <p> <a>ListTapesOutput$VolumeInfos</a> </p> </li> </ul>
newtype ListTapesOutput = ListTapesOutput 
  { "TapeInfos" :: NullOrUndefined (TapeInfos)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListTapesOutput :: Newtype ListTapesOutput _


-- | <p>ListVolumeInitiatorsInput</p>
newtype ListVolumeInitiatorsInput = ListVolumeInitiatorsInput 
  { "VolumeARN" :: (VolumeARN)
  }
derive instance newtypeListVolumeInitiatorsInput :: Newtype ListVolumeInitiatorsInput _


-- | <p>ListVolumeInitiatorsOutput</p>
newtype ListVolumeInitiatorsOutput = ListVolumeInitiatorsOutput 
  { "Initiators" :: NullOrUndefined (Initiators)
  }
derive instance newtypeListVolumeInitiatorsOutput :: Newtype ListVolumeInitiatorsOutput _


newtype ListVolumeRecoveryPointsInput = ListVolumeRecoveryPointsInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeListVolumeRecoveryPointsInput :: Newtype ListVolumeRecoveryPointsInput _


newtype ListVolumeRecoveryPointsOutput = ListVolumeRecoveryPointsOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "VolumeRecoveryPointInfos" :: NullOrUndefined (VolumeRecoveryPointInfos)
  }
derive instance newtypeListVolumeRecoveryPointsOutput :: Newtype ListVolumeRecoveryPointsOutput _


-- | <p>A JSON object that contains one or more of the following fields:</p> <ul> <li> <p> <a>ListVolumesInput$Limit</a> </p> </li> <li> <p> <a>ListVolumesInput$Marker</a> </p> </li> </ul>
newtype ListVolumesInput = ListVolumesInput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "Marker" :: NullOrUndefined (Marker)
  , "Limit" :: NullOrUndefined (PositiveIntObject)
  }
derive instance newtypeListVolumesInput :: Newtype ListVolumesInput _


newtype ListVolumesOutput = ListVolumesOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "Marker" :: NullOrUndefined (Marker)
  , "VolumeInfos" :: NullOrUndefined (VolumeInfos)
  }
derive instance newtypeListVolumesOutput :: Newtype ListVolumesOutput _


newtype LocalConsolePassword = LocalConsolePassword String
derive instance newtypeLocalConsolePassword :: Newtype LocalConsolePassword _


-- | <p>The ARN of the backend storage used for storing file data. </p>
newtype LocationARN = LocationARN String
derive instance newtypeLocationARN :: Newtype LocationARN _


newtype Marker = Marker String
derive instance newtypeMarker :: Newtype Marker _


newtype MediumChangerType = MediumChangerType String
derive instance newtypeMediumChangerType :: Newtype MediumChangerType _


newtype MinuteOfHour = MinuteOfHour Int
derive instance newtypeMinuteOfHour :: Newtype MinuteOfHour _


-- | <p>Describes file share default values. Files and folders stored as Amazon S3 objects in S3 buckets don't, by default, have Unix file permissions assigned to them. Upon discovery in an S3 bucket by Storage Gateway, the S3 objects that represent files and folders are assigned these default Unix permissions. This operation is only supported in the file gateway type.</p>
newtype NFSFileShareDefaults = NFSFileShareDefaults 
  { "FileMode" :: NullOrUndefined (PermissionMode)
  , "DirectoryMode" :: NullOrUndefined (PermissionMode)
  , "GroupId" :: NullOrUndefined (PermissionId)
  , "OwnerId" :: NullOrUndefined (PermissionId)
  }
derive instance newtypeNFSFileShareDefaults :: Newtype NFSFileShareDefaults _


-- | <p>The Unix file permissions and ownership information assigned, by default, to native S3 objects when file gateway discovers them in S3 buckets. This operation is only supported in file gateways.</p>
newtype NFSFileShareInfo = NFSFileShareInfo 
  { "NFSFileShareDefaults" :: NullOrUndefined (NFSFileShareDefaults)
  , "FileShareARN" :: NullOrUndefined (FileShareARN)
  , "FileShareId" :: NullOrUndefined (FileShareId)
  , "FileShareStatus" :: NullOrUndefined (FileShareStatus)
  , "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "KMSEncrypted" :: NullOrUndefined (Boolean)
  , "KMSKey" :: NullOrUndefined (KMSKey)
  , "Path" :: NullOrUndefined (Path)
  , "Role" :: NullOrUndefined (Role)
  , "LocationARN" :: NullOrUndefined (LocationARN)
  , "DefaultStorageClass" :: NullOrUndefined (StorageClass)
  , "ClientList" :: NullOrUndefined (FileShareClientList)
  , "Squash" :: NullOrUndefined (Squash)
  , "ReadOnly" :: NullOrUndefined (Boolean)
  , "GuessMIMETypeEnabled" :: NullOrUndefined (Boolean)
  }
derive instance newtypeNFSFileShareInfo :: Newtype NFSFileShareInfo _


newtype NFSFileShareInfoList = NFSFileShareInfoList (Array NFSFileShareInfo)
derive instance newtypeNFSFileShareInfoList :: Newtype NFSFileShareInfoList _


-- | <p>Describes a gateway's network interface.</p>
newtype NetworkInterface = NetworkInterface 
  { "Ipv4Address" :: NullOrUndefined (String)
  , "MacAddress" :: NullOrUndefined (String)
  , "Ipv6Address" :: NullOrUndefined (String)
  }
derive instance newtypeNetworkInterface :: Newtype NetworkInterface _


newtype NetworkInterfaceId = NetworkInterfaceId String
derive instance newtypeNetworkInterfaceId :: Newtype NetworkInterfaceId _


newtype NextUpdateAvailabilityDate = NextUpdateAvailabilityDate String
derive instance newtypeNextUpdateAvailabilityDate :: Newtype NextUpdateAvailabilityDate _


-- | <p>The randomly generated ID of the notification that was sent. This ID is in UUID format.</p>
newtype NotificationId = NotificationId String
derive instance newtypeNotificationId :: Newtype NotificationId _


newtype NotifyWhenUploadedInput = NotifyWhenUploadedInput 
  { "FileShareARN" :: (FileShareARN)
  }
derive instance newtypeNotifyWhenUploadedInput :: Newtype NotifyWhenUploadedInput _


newtype NotifyWhenUploadedOutput = NotifyWhenUploadedOutput 
  { "FileShareARN" :: NullOrUndefined (FileShareARN)
  , "NotificationId" :: NullOrUndefined (NotificationId)
  }
derive instance newtypeNotifyWhenUploadedOutput :: Newtype NotifyWhenUploadedOutput _


newtype NumTapesToCreate = NumTapesToCreate Int
derive instance newtypeNumTapesToCreate :: Newtype NumTapesToCreate _


-- | <p>The file share path used by the NFS client to identify the mount point. </p>
newtype Path = Path String
derive instance newtypePath :: Newtype Path _


newtype PermissionId = PermissionId Number
derive instance newtypePermissionId :: Newtype PermissionId _


newtype PermissionMode = PermissionMode String
derive instance newtypePermissionMode :: Newtype PermissionMode _


newtype PositiveIntObject = PositiveIntObject Int
derive instance newtypePositiveIntObject :: Newtype PositiveIntObject _


newtype RecurrenceInHours = RecurrenceInHours Int
derive instance newtypeRecurrenceInHours :: Newtype RecurrenceInHours _


newtype RefreshCacheInput = RefreshCacheInput 
  { "FileShareARN" :: (FileShareARN)
  }
derive instance newtypeRefreshCacheInput :: Newtype RefreshCacheInput _


newtype RefreshCacheOutput = RefreshCacheOutput 
  { "FileShareARN" :: NullOrUndefined (FileShareARN)
  }
derive instance newtypeRefreshCacheOutput :: Newtype RefreshCacheOutput _


newtype RegionId = RegionId String
derive instance newtypeRegionId :: Newtype RegionId _


-- | <p>RemoveTagsFromResourceInput</p>
newtype RemoveTagsFromResourceInput = RemoveTagsFromResourceInput 
  { "ResourceARN" :: (ResourceARN)
  , "TagKeys" :: (TagKeys)
  }
derive instance newtypeRemoveTagsFromResourceInput :: Newtype RemoveTagsFromResourceInput _


-- | <p>RemoveTagsFromResourceOutput</p>
newtype RemoveTagsFromResourceOutput = RemoveTagsFromResourceOutput 
  { "ResourceARN" :: NullOrUndefined (ResourceARN)
  }
derive instance newtypeRemoveTagsFromResourceOutput :: Newtype RemoveTagsFromResourceOutput _


newtype ResetCacheInput = ResetCacheInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeResetCacheInput :: Newtype ResetCacheInput _


newtype ResetCacheOutput = ResetCacheOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeResetCacheOutput :: Newtype ResetCacheOutput _


newtype ResourceARN = ResourceARN String
derive instance newtypeResourceARN :: Newtype ResourceARN _


-- | <p>RetrieveTapeArchiveInput</p>
newtype RetrieveTapeArchiveInput = RetrieveTapeArchiveInput 
  { "TapeARN" :: (TapeARN)
  , "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeRetrieveTapeArchiveInput :: Newtype RetrieveTapeArchiveInput _


-- | <p>RetrieveTapeArchiveOutput</p>
newtype RetrieveTapeArchiveOutput = RetrieveTapeArchiveOutput 
  { "TapeARN" :: NullOrUndefined (TapeARN)
  }
derive instance newtypeRetrieveTapeArchiveOutput :: Newtype RetrieveTapeArchiveOutput _


-- | <p>RetrieveTapeRecoveryPointInput</p>
newtype RetrieveTapeRecoveryPointInput = RetrieveTapeRecoveryPointInput 
  { "TapeARN" :: (TapeARN)
  , "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeRetrieveTapeRecoveryPointInput :: Newtype RetrieveTapeRecoveryPointInput _


-- | <p>RetrieveTapeRecoveryPointOutput</p>
newtype RetrieveTapeRecoveryPointOutput = RetrieveTapeRecoveryPointOutput 
  { "TapeARN" :: NullOrUndefined (TapeARN)
  }
derive instance newtypeRetrieveTapeRecoveryPointOutput :: Newtype RetrieveTapeRecoveryPointOutput _


-- | <p>The ARN of the IAM role that file gateway assumes when it accesses the underlying storage. </p>
newtype Role = Role String
derive instance newtypeRole :: Newtype Role _


-- | <p>An internal server error has occurred because the service is unavailable. For more information, see the error and message fields.</p>
newtype ServiceUnavailableError = ServiceUnavailableError 
  { "Message'" :: NullOrUndefined (String)
  , "Error'" :: NullOrUndefined (StorageGatewayError)
  }
derive instance newtypeServiceUnavailableError :: Newtype ServiceUnavailableError _


-- | <p>SetLocalConsolePasswordInput</p>
newtype SetLocalConsolePasswordInput = SetLocalConsolePasswordInput 
  { "GatewayARN" :: (GatewayARN)
  , "LocalConsolePassword" :: (LocalConsolePassword)
  }
derive instance newtypeSetLocalConsolePasswordInput :: Newtype SetLocalConsolePasswordInput _


newtype SetLocalConsolePasswordOutput = SetLocalConsolePasswordOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeSetLocalConsolePasswordOutput :: Newtype SetLocalConsolePasswordOutput _


-- | <p>A JSON object containing the of the gateway to shut down.</p>
newtype ShutdownGatewayInput = ShutdownGatewayInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeShutdownGatewayInput :: Newtype ShutdownGatewayInput _


-- | <p>A JSON object containing the of the gateway that was shut down.</p>
newtype ShutdownGatewayOutput = ShutdownGatewayOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeShutdownGatewayOutput :: Newtype ShutdownGatewayOutput _


newtype SnapshotDescription = SnapshotDescription String
derive instance newtypeSnapshotDescription :: Newtype SnapshotDescription _


newtype SnapshotId = SnapshotId String
derive instance newtypeSnapshotId :: Newtype SnapshotId _


-- | <p>The user mapped to anonymous user. Valid options are the following: </p> <ul> <li> <p>"RootSquash" - Only root is mapped to anonymous user.</p> </li> <li> <p>"NoSquash" - No one is mapped to anonymous user</p> </li> <li> <p>"AllSquash" - Everyone is mapped to anonymous user.</p> </li> </ul>
newtype Squash = Squash String
derive instance newtypeSquash :: Newtype Squash _


-- | <p>A JSON object containing the of the gateway to start.</p>
newtype StartGatewayInput = StartGatewayInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeStartGatewayInput :: Newtype StartGatewayInput _


-- | <p>A JSON object containing the of the gateway that was restarted.</p>
newtype StartGatewayOutput = StartGatewayOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeStartGatewayOutput :: Newtype StartGatewayOutput _


-- | <p/>
newtype StorageClass = StorageClass String
derive instance newtypeStorageClass :: Newtype StorageClass _


-- | <p>Provides additional information about an error that was returned by the service as an or. See the <code>errorCode</code> and <code>errorDetails</code> members for more information about the error.</p>
newtype StorageGatewayError = StorageGatewayError 
  { "ErrorCode'" :: NullOrUndefined (ErrorCode)
  , "ErrorDetails'" :: NullOrUndefined (ErrorDetails')
  }
derive instance newtypeStorageGatewayError :: Newtype StorageGatewayError _


-- | <p>Describes an iSCSI stored volume.</p>
newtype StorediSCSIVolume = StorediSCSIVolume 
  { "VolumeARN" :: NullOrUndefined (VolumeARN)
  , "VolumeId" :: NullOrUndefined (VolumeId)
  , "VolumeType" :: NullOrUndefined (VolumeType)
  , "VolumeStatus" :: NullOrUndefined (VolumeStatus)
  , "VolumeSizeInBytes" :: NullOrUndefined (Number)
  , "VolumeProgress" :: NullOrUndefined (DoubleObject)
  , "VolumeDiskId" :: NullOrUndefined (DiskId)
  , "SourceSnapshotId" :: NullOrUndefined (SnapshotId)
  , "PreservedExistingData" :: NullOrUndefined (Boolean)
  , "VolumeiSCSIAttributes" :: NullOrUndefined (VolumeiSCSIAttributes)
  , "CreatedDate" :: NullOrUndefined (CreatedDate)
  , "VolumeUsedInBytes" :: NullOrUndefined (VolumeUsedInBytes)
  }
derive instance newtypeStorediSCSIVolume :: Newtype StorediSCSIVolume _


newtype StorediSCSIVolumes = StorediSCSIVolumes (Array StorediSCSIVolume)
derive instance newtypeStorediSCSIVolumes :: Newtype StorediSCSIVolumes _


newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeys = TagKeys (Array TagKey)
derive instance newtypeTagKeys :: Newtype TagKeys _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype Tags = Tags (Array Tag)
derive instance newtypeTags :: Newtype Tags _


-- | <p>Describes a virtual tape object.</p>
newtype Tape = Tape 
  { "TapeARN" :: NullOrUndefined (TapeARN)
  , "TapeBarcode" :: NullOrUndefined (TapeBarcode)
  , "TapeCreatedDate" :: NullOrUndefined (Time)
  , "TapeSizeInBytes" :: NullOrUndefined (TapeSize)
  , "TapeStatus" :: NullOrUndefined (TapeStatus)
  , "VTLDevice" :: NullOrUndefined (VTLDeviceARN)
  , "Progress" :: NullOrUndefined (DoubleObject)
  , "TapeUsedInBytes" :: NullOrUndefined (TapeUsage)
  }
derive instance newtypeTape :: Newtype Tape _


newtype TapeARN = TapeARN String
derive instance newtypeTapeARN :: Newtype TapeARN _


-- | <p>The Amazon Resource Name (ARN) of each of the tapes you want to list. If you don't specify a tape ARN, the response lists all tapes in both your VTL and VTS.</p>
newtype TapeARNs = TapeARNs (Array TapeARN)
derive instance newtypeTapeARNs :: Newtype TapeARNs _


-- | <p>Represents a virtual tape that is archived in the virtual tape shelf (VTS).</p>
newtype TapeArchive = TapeArchive 
  { "TapeARN" :: NullOrUndefined (TapeARN)
  , "TapeBarcode" :: NullOrUndefined (TapeBarcode)
  , "TapeCreatedDate" :: NullOrUndefined (Time)
  , "TapeSizeInBytes" :: NullOrUndefined (TapeSize)
  , "CompletionTime" :: NullOrUndefined (Time)
  , "RetrievedTo" :: NullOrUndefined (GatewayARN)
  , "TapeStatus" :: NullOrUndefined (TapeArchiveStatus)
  , "TapeUsedInBytes" :: NullOrUndefined (TapeUsage)
  }
derive instance newtypeTapeArchive :: Newtype TapeArchive _


newtype TapeArchiveStatus = TapeArchiveStatus String
derive instance newtypeTapeArchiveStatus :: Newtype TapeArchiveStatus _


newtype TapeArchives = TapeArchives (Array TapeArchive)
derive instance newtypeTapeArchives :: Newtype TapeArchives _


newtype TapeBarcode = TapeBarcode String
derive instance newtypeTapeBarcode :: Newtype TapeBarcode _


newtype TapeBarcodePrefix = TapeBarcodePrefix String
derive instance newtypeTapeBarcodePrefix :: Newtype TapeBarcodePrefix _


newtype TapeDriveType = TapeDriveType String
derive instance newtypeTapeDriveType :: Newtype TapeDriveType _


-- | <p>Describes a virtual tape.</p>
newtype TapeInfo = TapeInfo 
  { "TapeARN" :: NullOrUndefined (TapeARN)
  , "TapeBarcode" :: NullOrUndefined (TapeBarcode)
  , "TapeSizeInBytes" :: NullOrUndefined (TapeSize)
  , "TapeStatus" :: NullOrUndefined (TapeStatus)
  , "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeTapeInfo :: Newtype TapeInfo _


-- | <p>An array of <a>TapeInfo</a> objects, where each object describes an a single tape. If there not tapes in the tape library or VTS, then the <code>TapeInfos</code> is an empty array.</p>
newtype TapeInfos = TapeInfos (Array TapeInfo)
derive instance newtypeTapeInfos :: Newtype TapeInfos _


-- | <p>Describes a recovery point.</p>
newtype TapeRecoveryPointInfo = TapeRecoveryPointInfo 
  { "TapeARN" :: NullOrUndefined (TapeARN)
  , "TapeRecoveryPointTime" :: NullOrUndefined (Time)
  , "TapeSizeInBytes" :: NullOrUndefined (TapeSize)
  , "TapeStatus" :: NullOrUndefined (TapeRecoveryPointStatus)
  }
derive instance newtypeTapeRecoveryPointInfo :: Newtype TapeRecoveryPointInfo _


newtype TapeRecoveryPointInfos = TapeRecoveryPointInfos (Array TapeRecoveryPointInfo)
derive instance newtypeTapeRecoveryPointInfos :: Newtype TapeRecoveryPointInfos _


newtype TapeRecoveryPointStatus = TapeRecoveryPointStatus String
derive instance newtypeTapeRecoveryPointStatus :: Newtype TapeRecoveryPointStatus _


newtype TapeSize = TapeSize Number
derive instance newtypeTapeSize :: Newtype TapeSize _


newtype TapeStatus = TapeStatus String
derive instance newtypeTapeStatus :: Newtype TapeStatus _


newtype TapeUsage = TapeUsage Number
derive instance newtypeTapeUsage :: Newtype TapeUsage _


newtype Tapes = Tapes (Array Tape)
derive instance newtypeTapes :: Newtype Tapes _


newtype TargetARN = TargetARN String
derive instance newtypeTargetARN :: Newtype TargetARN _


newtype TargetName = TargetName String
derive instance newtypeTargetName :: Newtype TargetName _


newtype Time = Time Number
derive instance newtypeTime :: Newtype Time _


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>UpdateBandwidthRateLimitInput$AverageDownloadRateLimitInBitsPerSec</a> </p> </li> <li> <p> <a>UpdateBandwidthRateLimitInput$AverageUploadRateLimitInBitsPerSec</a> </p> </li> </ul>
newtype UpdateBandwidthRateLimitInput = UpdateBandwidthRateLimitInput 
  { "GatewayARN" :: (GatewayARN)
  , "AverageUploadRateLimitInBitsPerSec" :: NullOrUndefined (BandwidthUploadRateLimit)
  , "AverageDownloadRateLimitInBitsPerSec" :: NullOrUndefined (BandwidthDownloadRateLimit)
  }
derive instance newtypeUpdateBandwidthRateLimitInput :: Newtype UpdateBandwidthRateLimitInput _


-- | <p>A JSON object containing the of the gateway whose throttle information was updated.</p>
newtype UpdateBandwidthRateLimitOutput = UpdateBandwidthRateLimitOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeUpdateBandwidthRateLimitOutput :: Newtype UpdateBandwidthRateLimitOutput _


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>UpdateChapCredentialsInput$InitiatorName</a> </p> </li> <li> <p> <a>UpdateChapCredentialsInput$SecretToAuthenticateInitiator</a> </p> </li> <li> <p> <a>UpdateChapCredentialsInput$SecretToAuthenticateTarget</a> </p> </li> <li> <p> <a>UpdateChapCredentialsInput$TargetARN</a> </p> </li> </ul>
newtype UpdateChapCredentialsInput = UpdateChapCredentialsInput 
  { "TargetARN" :: (TargetARN)
  , "SecretToAuthenticateInitiator" :: (ChapSecret)
  , "InitiatorName" :: (IqnName)
  , "SecretToAuthenticateTarget" :: NullOrUndefined (ChapSecret)
  }
derive instance newtypeUpdateChapCredentialsInput :: Newtype UpdateChapCredentialsInput _


-- | <p>A JSON object containing the following fields:</p>
newtype UpdateChapCredentialsOutput = UpdateChapCredentialsOutput 
  { "TargetARN" :: NullOrUndefined (TargetARN)
  , "InitiatorName" :: NullOrUndefined (IqnName)
  }
derive instance newtypeUpdateChapCredentialsOutput :: Newtype UpdateChapCredentialsOutput _


newtype UpdateGatewayInformationInput = UpdateGatewayInformationInput 
  { "GatewayARN" :: (GatewayARN)
  , "GatewayName" :: NullOrUndefined (GatewayName)
  , "GatewayTimezone" :: NullOrUndefined (GatewayTimezone)
  }
derive instance newtypeUpdateGatewayInformationInput :: Newtype UpdateGatewayInformationInput _


-- | <p>A JSON object containing the ARN of the gateway that was updated.</p>
newtype UpdateGatewayInformationOutput = UpdateGatewayInformationOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "GatewayName" :: NullOrUndefined (String)
  }
derive instance newtypeUpdateGatewayInformationOutput :: Newtype UpdateGatewayInformationOutput _


-- | <p>A JSON object containing the of the gateway to update.</p>
newtype UpdateGatewaySoftwareNowInput = UpdateGatewaySoftwareNowInput 
  { "GatewayARN" :: (GatewayARN)
  }
derive instance newtypeUpdateGatewaySoftwareNowInput :: Newtype UpdateGatewaySoftwareNowInput _


-- | <p>A JSON object containing the of the gateway that was updated.</p>
newtype UpdateGatewaySoftwareNowOutput = UpdateGatewaySoftwareNowOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeUpdateGatewaySoftwareNowOutput :: Newtype UpdateGatewaySoftwareNowOutput _


-- | <p>A JSON object containing the following fields:</p> <ul> <li> <p> <a>UpdateMaintenanceStartTimeInput$DayOfWeek</a> </p> </li> <li> <p> <a>UpdateMaintenanceStartTimeInput$HourOfDay</a> </p> </li> <li> <p> <a>UpdateMaintenanceStartTimeInput$MinuteOfHour</a> </p> </li> </ul>
newtype UpdateMaintenanceStartTimeInput = UpdateMaintenanceStartTimeInput 
  { "GatewayARN" :: (GatewayARN)
  , "HourOfDay" :: (HourOfDay)
  , "MinuteOfHour" :: (MinuteOfHour)
  , "DayOfWeek" :: (DayOfWeek)
  }
derive instance newtypeUpdateMaintenanceStartTimeInput :: Newtype UpdateMaintenanceStartTimeInput _


-- | <p>A JSON object containing the of the gateway whose maintenance start time is updated.</p>
newtype UpdateMaintenanceStartTimeOutput = UpdateMaintenanceStartTimeOutput 
  { "GatewayARN" :: NullOrUndefined (GatewayARN)
  }
derive instance newtypeUpdateMaintenanceStartTimeOutput :: Newtype UpdateMaintenanceStartTimeOutput _


-- | <p>UpdateNFSFileShareInput</p>
newtype UpdateNFSFileShareInput = UpdateNFSFileShareInput 
  { "FileShareARN" :: (FileShareARN)
  , "KMSEncrypted" :: NullOrUndefined (Boolean)
  , "KMSKey" :: NullOrUndefined (KMSKey)
  , "NFSFileShareDefaults" :: NullOrUndefined (NFSFileShareDefaults)
  , "DefaultStorageClass" :: NullOrUndefined (StorageClass)
  , "ClientList" :: NullOrUndefined (FileShareClientList)
  , "Squash" :: NullOrUndefined (Squash)
  , "ReadOnly" :: NullOrUndefined (Boolean)
  , "GuessMIMETypeEnabled" :: NullOrUndefined (Boolean)
  }
derive instance newtypeUpdateNFSFileShareInput :: Newtype UpdateNFSFileShareInput _


-- | <p>UpdateNFSFileShareOutput</p>
newtype UpdateNFSFileShareOutput = UpdateNFSFileShareOutput 
  { "FileShareARN" :: NullOrUndefined (FileShareARN)
  }
derive instance newtypeUpdateNFSFileShareOutput :: Newtype UpdateNFSFileShareOutput _


-- | <p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>UpdateSnapshotScheduleInput$Description</a> </p> </li> <li> <p> <a>UpdateSnapshotScheduleInput$RecurrenceInHours</a> </p> </li> <li> <p> <a>UpdateSnapshotScheduleInput$StartAt</a> </p> </li> <li> <p> <a>UpdateSnapshotScheduleInput$VolumeARN</a> </p> </li> </ul>
newtype UpdateSnapshotScheduleInput = UpdateSnapshotScheduleInput 
  { "VolumeARN" :: (VolumeARN)
  , "StartAt" :: (HourOfDay)
  , "RecurrenceInHours" :: (RecurrenceInHours)
  , "Description" :: NullOrUndefined (Description)
  }
derive instance newtypeUpdateSnapshotScheduleInput :: Newtype UpdateSnapshotScheduleInput _


-- | <p>A JSON object containing the of the updated storage volume.</p>
newtype UpdateSnapshotScheduleOutput = UpdateSnapshotScheduleOutput 
  { "VolumeARN" :: NullOrUndefined (VolumeARN)
  }
derive instance newtypeUpdateSnapshotScheduleOutput :: Newtype UpdateSnapshotScheduleOutput _


newtype UpdateVTLDeviceTypeInput = UpdateVTLDeviceTypeInput 
  { "VTLDeviceARN" :: (VTLDeviceARN)
  , "DeviceType" :: (DeviceType)
  }
derive instance newtypeUpdateVTLDeviceTypeInput :: Newtype UpdateVTLDeviceTypeInput _


-- | <p>UpdateVTLDeviceTypeOutput</p>
newtype UpdateVTLDeviceTypeOutput = UpdateVTLDeviceTypeOutput 
  { "VTLDeviceARN" :: NullOrUndefined (VTLDeviceARN)
  }
derive instance newtypeUpdateVTLDeviceTypeOutput :: Newtype UpdateVTLDeviceTypeOutput _


-- | <p>Represents a device object associated with a tape gateway.</p>
newtype VTLDevice = VTLDevice 
  { "VTLDeviceARN" :: NullOrUndefined (VTLDeviceARN)
  , "VTLDeviceType" :: NullOrUndefined (VTLDeviceType)
  , "VTLDeviceVendor" :: NullOrUndefined (VTLDeviceVendor)
  , "VTLDeviceProductIdentifier" :: NullOrUndefined (VTLDeviceProductIdentifier)
  , "DeviceiSCSIAttributes" :: NullOrUndefined (DeviceiSCSIAttributes)
  }
derive instance newtypeVTLDevice :: Newtype VTLDevice _


newtype VTLDeviceARN = VTLDeviceARN String
derive instance newtypeVTLDeviceARN :: Newtype VTLDeviceARN _


newtype VTLDeviceARNs = VTLDeviceARNs (Array VTLDeviceARN)
derive instance newtypeVTLDeviceARNs :: Newtype VTLDeviceARNs _


newtype VTLDeviceProductIdentifier = VTLDeviceProductIdentifier String
derive instance newtypeVTLDeviceProductIdentifier :: Newtype VTLDeviceProductIdentifier _


newtype VTLDeviceType = VTLDeviceType String
derive instance newtypeVTLDeviceType :: Newtype VTLDeviceType _


newtype VTLDeviceVendor = VTLDeviceVendor String
derive instance newtypeVTLDeviceVendor :: Newtype VTLDeviceVendor _


newtype VTLDevices = VTLDevices (Array VTLDevice)
derive instance newtypeVTLDevices :: Newtype VTLDevices _


newtype VolumeARN = VolumeARN String
derive instance newtypeVolumeARN :: Newtype VolumeARN _


newtype VolumeARNs = VolumeARNs (Array VolumeARN)
derive instance newtypeVolumeARNs :: Newtype VolumeARNs _


newtype VolumeId = VolumeId String
derive instance newtypeVolumeId :: Newtype VolumeId _


-- | <p>Describes a storage volume object.</p>
newtype VolumeInfo = VolumeInfo 
  { "VolumeARN" :: NullOrUndefined (VolumeARN)
  , "VolumeId" :: NullOrUndefined (VolumeId)
  , "GatewayARN" :: NullOrUndefined (GatewayARN)
  , "GatewayId" :: NullOrUndefined (GatewayId)
  , "VolumeType" :: NullOrUndefined (VolumeType)
  , "VolumeSizeInBytes" :: NullOrUndefined (Number)
  }
derive instance newtypeVolumeInfo :: Newtype VolumeInfo _


newtype VolumeInfos = VolumeInfos (Array VolumeInfo)
derive instance newtypeVolumeInfos :: Newtype VolumeInfos _


newtype VolumeRecoveryPointInfo = VolumeRecoveryPointInfo 
  { "VolumeARN" :: NullOrUndefined (VolumeARN)
  , "VolumeSizeInBytes" :: NullOrUndefined (Number)
  , "VolumeUsageInBytes" :: NullOrUndefined (Number)
  , "VolumeRecoveryPointTime" :: NullOrUndefined (String)
  }
derive instance newtypeVolumeRecoveryPointInfo :: Newtype VolumeRecoveryPointInfo _


newtype VolumeRecoveryPointInfos = VolumeRecoveryPointInfos (Array VolumeRecoveryPointInfo)
derive instance newtypeVolumeRecoveryPointInfos :: Newtype VolumeRecoveryPointInfos _


newtype VolumeStatus = VolumeStatus String
derive instance newtypeVolumeStatus :: Newtype VolumeStatus _


newtype VolumeType = VolumeType String
derive instance newtypeVolumeType :: Newtype VolumeType _


newtype VolumeUsedInBytes = VolumeUsedInBytes Number
derive instance newtypeVolumeUsedInBytes :: Newtype VolumeUsedInBytes _


-- | <p>Lists iSCSI information about a volume.</p>
newtype VolumeiSCSIAttributes = VolumeiSCSIAttributes 
  { "TargetARN" :: NullOrUndefined (TargetARN)
  , "NetworkInterfaceId" :: NullOrUndefined (NetworkInterfaceId)
  , "NetworkInterfacePort" :: NullOrUndefined (Int)
  , "LunNumber" :: NullOrUndefined (PositiveIntObject)
  , "ChapEnabled" :: NullOrUndefined (Boolean)
  }
derive instance newtypeVolumeiSCSIAttributes :: Newtype VolumeiSCSIAttributes _


newtype ErrorDetails' = ErrorDetails' (Map String String)
derive instance newtypeErrorDetails' :: Newtype ErrorDetails' _
