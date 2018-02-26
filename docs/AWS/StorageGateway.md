## Module AWS.StorageGateway

<fullname>AWS Storage Gateway Service</fullname> <p>AWS Storage Gateway is the service that connects an on-premises software appliance with cloud-based storage to provide seamless and secure integration between an organization's on-premises IT environment and AWS's storage infrastructure. The service enables you to securely upload data to the AWS cloud for cost effective backup and rapid disaster recovery.</p> <p>Use the following links to get started using the <i>AWS Storage Gateway Service API Reference</i>:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewayHTTPRequestsHeaders">AWS Storage Gateway Required Request Headers</a>: Describes the required headers that you must send with every POST request to AWS Storage Gateway.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewaySigningRequests">Signing Requests</a>: AWS Storage Gateway requires that you authenticate every request you send; this topic describes how sign such a request.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#APIErrorResponses">Error Responses</a>: Provides reference information about AWS Storage Gateway errors.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_Operations.html">Operations in AWS Storage Gateway</a>: Contains detailed descriptions of all AWS Storage Gateway operations, their request parameters, response elements, possible errors, and examples of requests and responses.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#sg_region">AWS Storage Gateway Regions and Endpoints:</a> Provides a list of each region and endpoints available for use with AWS Storage Gateway. </p> </li> </ul> <note> <p>AWS Storage Gateway resource IDs are in uppercase. When you use these resource IDs with the Amazon EC2 API, EC2 expects resource IDs in lowercase. You must change your resource ID to lowercase to use it with the EC2 API. For example, in Storage Gateway the ID for a volume might be <code>vol-AA22BB012345DAF670</code>. When you use this ID with the EC2 API, you must change it to <code>vol-aa22bb012345daf670</code>. Otherwise, the EC2 API might not behave as expected.</p> </note> <important> <p>IDs for Storage Gateway volumes and Amazon EBS snapshots created from gateway volumes are changing to a longer format. Starting in December 2016, all new volumes and snapshots will be created with a 17-character string. Starting in April 2016, you will be able to use these longer IDs so you can test your systems with the new format. For more information, see <a href="https://aws.amazon.com/ec2/faqs/#longer-ids">Longer EC2 and EBS Resource IDs</a>.</p> <p> For example, a volume Amazon Resource Name (ARN) with the longer volume ID format looks like the following:</p> <p> <code>arn:aws:storagegateway:us-west-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABBCCDDEEFFG</code>.</p> <p>A snapshot ID with the longer ID format looks like the following: <code>snap-78e226633445566ee</code>.</p> <p>For more information, see <a href="https://forums.aws.amazon.com/ann.jspa?annID=3557">Announcement: Heads-up – Longer AWS Storage Gateway volume and snapshot IDs coming in 2016</a>.</p> </important>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `activateGateway`

``` purescript
activateGateway :: forall eff. ActivateGatewayInput -> Aff (err :: RequestError | eff) ActivateGatewayOutput
```

<p>Activates the gateway you previously deployed on your host. For more information, see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedActivateGateway-common.html"> Activate the AWS Storage Gateway</a>. In the activation process, you specify information such as the region you want to use for storing snapshots or tapes, the time zone for scheduled snapshots the gateway snapshot schedule window, an activation key, and a name for your gateway. The activation process also associates your gateway with your account; for more information, see <a>UpdateGatewayInformation</a>.</p> <note> <p>You must turn on the gateway VM before you can activate your gateway.</p> </note>

#### `addCache`

``` purescript
addCache :: forall eff. AddCacheInput -> Aff (err :: RequestError | eff) AddCacheOutput
```

<p>Configures one or more gateway local disks as cache for a gateway. This operation is only supported in the cached volume, tape and file gateway type (see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/StorageGatewayConcepts.html">Storage Gateway Concepts</a>).</p> <p>In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add cache, and one or more disk IDs that you want to configure as cache.</p>

#### `addTagsToResource`

``` purescript
addTagsToResource :: forall eff. AddTagsToResourceInput -> Aff (err :: RequestError | eff) AddTagsToResourceOutput
```

<p>Adds one or more tags to the specified resource. You use tags to add metadata to resources, which you can use to categorize these resources. For example, you can categorize resources by purpose, owner, environment, or team. Each tag consists of a key and a value, which you define. You can add tags to the following AWS Storage Gateway resources:</p> <ul> <li> <p>Storage gateways of all types</p> </li> </ul> <ul> <li> <p>Storage Volumes</p> </li> </ul> <ul> <li> <p>Virtual Tapes</p> </li> </ul> <p>You can create a maximum of 10 tags for each resource. Virtual tapes and storage volumes that are recovered to a new gateway maintain their tags.</p>

#### `addUploadBuffer`

``` purescript
addUploadBuffer :: forall eff. AddUploadBufferInput -> Aff (err :: RequestError | eff) AddUploadBufferOutput
```

<p>Configures one or more gateway local disks as upload buffer for a specified gateway. This operation is supported for the stored volume, cached volume and tape gateway types.</p> <p>In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add upload buffer, and one or more disk IDs that you want to configure as upload buffer.</p>

#### `addWorkingStorage`

``` purescript
addWorkingStorage :: forall eff. AddWorkingStorageInput -> Aff (err :: RequestError | eff) AddWorkingStorageOutput
```

<p>Configures one or more gateway local disks as working storage for a gateway. This operation is only supported in the stored volume gateway type. This operation is deprecated in cached volume API version 20120630. Use <a>AddUploadBuffer</a> instead.</p> <note> <p>Working storage is also referred to as upload buffer. You can also use the <a>AddUploadBuffer</a> operation to add upload buffer to a stored volume gateway.</p> </note> <p>In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add working storage, and one or more disk IDs that you want to configure as working storage.</p>

#### `cancelArchival`

``` purescript
cancelArchival :: forall eff. CancelArchivalInput -> Aff (err :: RequestError | eff) CancelArchivalOutput
```

<p>Cancels archiving of a virtual tape to the virtual tape shelf (VTS) after the archiving process is initiated. This operation is only supported in the tape gateway type.</p>

#### `cancelRetrieval`

``` purescript
cancelRetrieval :: forall eff. CancelRetrievalInput -> Aff (err :: RequestError | eff) CancelRetrievalOutput
```

<p>Cancels retrieval of a virtual tape from the virtual tape shelf (VTS) to a gateway after the retrieval process is initiated. The virtual tape is returned to the VTS. This operation is only supported in the tape gateway type.</p>

#### `createCachediSCSIVolume`

``` purescript
createCachediSCSIVolume :: forall eff. CreateCachediSCSIVolumeInput -> Aff (err :: RequestError | eff) CreateCachediSCSIVolumeOutput
```

<p>Creates a cached volume on a specified cached volume gateway. This operation is only supported in the cached volume gateway type.</p> <note> <p>Cache storage must be allocated to the gateway before you can create a cached volume. Use the <a>AddCache</a> operation to add cache storage to a gateway. </p> </note> <p>In the request, you must specify the gateway, size of the volume in bytes, the iSCSI target name, an IP address on which to expose the target, and a unique client token. In response, the gateway creates the volume and returns information about it. This information includes the volume Amazon Resource Name (ARN), its size, and the iSCSI target ARN that initiators can use to connect to the volume target.</p> <p>Optionally, you can provide the ARN for an existing volume as the <code>SourceVolumeARN</code> for this cached volume, which creates an exact copy of the existing volume’s latest recovery point. The <code>VolumeSizeInBytes</code> value must be equal to or larger than the size of the copied volume, in bytes.</p>

#### `createNFSFileShare`

``` purescript
createNFSFileShare :: forall eff. CreateNFSFileShareInput -> Aff (err :: RequestError | eff) CreateNFSFileShareOutput
```

<p>Creates a file share on an existing file gateway. In Storage Gateway, a file share is a file system mount point backed by Amazon S3 cloud storage. Storage Gateway exposes file shares using a Network File System (NFS) interface. This operation is only supported in the file gateway type.</p> <important> <p>File gateway requires AWS Security Token Service (AWS STS) to be activated to enable you create a file share. Make sure AWS STS is activated in the region you are creating your file gateway in. If AWS STS is not activated in the region, activate it. For information about how to activate AWS STS, see Activating and Deactivating AWS STS in an AWS Region in the AWS Identity and Access Management User Guide. </p> <p>File gateway does not support creating hard or symbolic links on a file share.</p> </important>

#### `createSnapshot`

``` purescript
createSnapshot :: forall eff. CreateSnapshotInput -> Aff (err :: RequestError | eff) CreateSnapshotOutput
```

<p>Initiates a snapshot of a volume.</p> <p>AWS Storage Gateway provides the ability to back up point-in-time snapshots of your data to Amazon Simple Storage (S3) for durable off-site recovery, as well as import the data to an Amazon Elastic Block Store (EBS) volume in Amazon Elastic Compute Cloud (EC2). You can take snapshots of your gateway volume on a scheduled or ad-hoc basis. This API enables you to take ad-hoc snapshot. For more information, see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#SchedulingSnapshot">Editing a Snapshot Schedule</a>.</p> <p>In the CreateSnapshot request you identify the volume by providing its Amazon Resource Name (ARN). You must also provide description for the snapshot. When AWS Storage Gateway takes the snapshot of specified volume, the snapshot and description appears in the AWS Storage Gateway Console. In response, AWS Storage Gateway returns you a snapshot ID. You can use this snapshot ID to check the snapshot progress or later use it when you want to create a volume from a snapshot. This operation is only supported in stored and cached volume gateway type.</p> <note> <p>To list or delete a snapshot, you must use the Amazon EC2 API. For more information, see DescribeSnapshots or DeleteSnapshot in the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Operations.html">EC2 API reference</a>.</p> </note> <important> <p>Volume and snapshot IDs are changing to a longer length ID format. For more information, see the important note on the <a href="http://docs.aws.amazon.com/storagegateway/latest/APIReference/Welcome.html">Welcome</a> page.</p> </important>

#### `createSnapshotFromVolumeRecoveryPoint`

``` purescript
createSnapshotFromVolumeRecoveryPoint :: forall eff. CreateSnapshotFromVolumeRecoveryPointInput -> Aff (err :: RequestError | eff) CreateSnapshotFromVolumeRecoveryPointOutput
```

<p>Initiates a snapshot of a gateway from a volume recovery point. This operation is only supported in the cached volume gateway type.</p> <p>A volume recovery point is a point in time at which all data of the volume is consistent and from which you can create a snapshot. To get a list of volume recovery point for cached volume gateway, use <a>ListVolumeRecoveryPoints</a>.</p> <p>In the <code>CreateSnapshotFromVolumeRecoveryPoint</code> request, you identify the volume by providing its Amazon Resource Name (ARN). You must also provide a description for the snapshot. When the gateway takes a snapshot of the specified volume, the snapshot and its description appear in the AWS Storage Gateway console. In response, the gateway returns you a snapshot ID. You can use this snapshot ID to check the snapshot progress or later use it when you want to create a volume from a snapshot.</p> <note> <p>To list or delete a snapshot, you must use the Amazon EC2 API. For more information, in <i>Amazon Elastic Compute Cloud API Reference</i>.</p> </note>

#### `createStorediSCSIVolume`

``` purescript
createStorediSCSIVolume :: forall eff. CreateStorediSCSIVolumeInput -> Aff (err :: RequestError | eff) CreateStorediSCSIVolumeOutput
```

<p>Creates a volume on a specified gateway. This operation is only supported in the stored volume gateway type.</p> <p>The size of the volume to create is inferred from the disk size. You can choose to preserve existing data on the disk, create volume from an existing snapshot, or create an empty volume. If you choose to create an empty gateway volume, then any existing data on the disk is erased.</p> <p>In the request you must specify the gateway and the disk information on which you are creating the volume. In response, the gateway creates the volume and returns volume information such as the volume Amazon Resource Name (ARN), its size, and the iSCSI target ARN that initiators can use to connect to the volume target.</p>

#### `createTapeWithBarcode`

``` purescript
createTapeWithBarcode :: forall eff. CreateTapeWithBarcodeInput -> Aff (err :: RequestError | eff) CreateTapeWithBarcodeOutput
```

<p>Creates a virtual tape by using your own barcode. You write data to the virtual tape and then archive the tape. A barcode is unique and can not be reused if it has already been used on a tape . This applies to barcodes used on deleted tapes. This operation is only supported in the tape gateway type.</p> <note> <p>Cache storage must be allocated to the gateway before you can create a virtual tape. Use the <a>AddCache</a> operation to add cache storage to a gateway.</p> </note>

#### `createTapes`

``` purescript
createTapes :: forall eff. CreateTapesInput -> Aff (err :: RequestError | eff) CreateTapesOutput
```

<p>Creates one or more virtual tapes. You write data to the virtual tapes and then archive the tapes. This operation is only supported in the tape gateway type.</p> <note> <p>Cache storage must be allocated to the gateway before you can create virtual tapes. Use the <a>AddCache</a> operation to add cache storage to a gateway. </p> </note>

#### `deleteBandwidthRateLimit`

``` purescript
deleteBandwidthRateLimit :: forall eff. DeleteBandwidthRateLimitInput -> Aff (err :: RequestError | eff) DeleteBandwidthRateLimitOutput
```

<p>Deletes the bandwidth rate limits of a gateway. You can delete either the upload and download bandwidth rate limit, or you can delete both. If you delete only one of the limits, the other limit remains unchanged. To specify which gateway to work with, use the Amazon Resource Name (ARN) of the gateway in your request.</p>

#### `deleteChapCredentials`

``` purescript
deleteChapCredentials :: forall eff. DeleteChapCredentialsInput -> Aff (err :: RequestError | eff) DeleteChapCredentialsOutput
```

<p>Deletes Challenge-Handshake Authentication Protocol (CHAP) credentials for a specified iSCSI target and initiator pair.</p>

#### `deleteFileShare`

``` purescript
deleteFileShare :: forall eff. DeleteFileShareInput -> Aff (err :: RequestError | eff) DeleteFileShareOutput
```

<p>Deletes a file share from a file gateway. This operation is only supported in the file gateway type.</p>

#### `deleteGateway`

``` purescript
deleteGateway :: forall eff. DeleteGatewayInput -> Aff (err :: RequestError | eff) DeleteGatewayOutput
```

<p>Deletes a gateway. To specify which gateway to delete, use the Amazon Resource Name (ARN) of the gateway in your request. The operation deletes the gateway; however, it does not delete the gateway virtual machine (VM) from your host computer.</p> <p>After you delete a gateway, you cannot reactivate it. Completed snapshots of the gateway volumes are not deleted upon deleting the gateway, however, pending snapshots will not complete. After you delete a gateway, your next step is to remove it from your environment.</p> <important> <p>You no longer pay software charges after the gateway is deleted; however, your existing Amazon EBS snapshots persist and you will continue to be billed for these snapshots. You can choose to remove all remaining Amazon EBS snapshots by canceling your Amazon EC2 subscription.  If you prefer not to cancel your Amazon EC2 subscription, you can delete your snapshots using the Amazon EC2 console. For more information, see the <a href="http://aws.amazon.com/storagegateway"> AWS Storage Gateway Detail Page</a>. </p> </important>

#### `deleteSnapshotSchedule`

``` purescript
deleteSnapshotSchedule :: forall eff. DeleteSnapshotScheduleInput -> Aff (err :: RequestError | eff) DeleteSnapshotScheduleOutput
```

<p>Deletes a snapshot of a volume.</p> <p>You can take snapshots of your gateway volumes on a scheduled or ad hoc basis. This API action enables you to delete a snapshot schedule for a volume. For more information, see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/WorkingWithSnapshots.html">Working with Snapshots</a>. In the <code>DeleteSnapshotSchedule</code> request, you identify the volume by providing its Amazon Resource Name (ARN). This operation is only supported in stored and cached volume gateway types.</p> <note> <p>To list or delete a snapshot, you must use the Amazon EC2 API. in <i>Amazon Elastic Compute Cloud API Reference</i>.</p> </note>

#### `deleteTape`

``` purescript
deleteTape :: forall eff. DeleteTapeInput -> Aff (err :: RequestError | eff) DeleteTapeOutput
```

<p>Deletes the specified virtual tape. This operation is only supported in the tape gateway type.</p>

#### `deleteTapeArchive`

``` purescript
deleteTapeArchive :: forall eff. DeleteTapeArchiveInput -> Aff (err :: RequestError | eff) DeleteTapeArchiveOutput
```

<p>Deletes the specified virtual tape from the virtual tape shelf (VTS). This operation is only supported in the tape gateway type.</p>

#### `deleteVolume`

``` purescript
deleteVolume :: forall eff. DeleteVolumeInput -> Aff (err :: RequestError | eff) DeleteVolumeOutput
```

<p>Deletes the specified storage volume that you previously created using the <a>CreateCachediSCSIVolume</a> or <a>CreateStorediSCSIVolume</a> API. This operation is only supported in the cached volume and stored volume types. For stored volume gateways, the local disk that was configured as the storage volume is not deleted. You can reuse the local disk to create another storage volume. </p> <p>Before you delete a volume, make sure there are no iSCSI connections to the volume you are deleting. You should also make sure there is no snapshot in progress. You can use the Amazon Elastic Compute Cloud (Amazon EC2) API to query snapshots on the volume you are deleting and check the snapshot status. For more information, go to <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html">DescribeSnapshots</a> in the <i>Amazon Elastic Compute Cloud API Reference</i>.</p> <p>In the request, you must provide the Amazon Resource Name (ARN) of the storage volume you want to delete.</p>

#### `describeBandwidthRateLimit`

``` purescript
describeBandwidthRateLimit :: forall eff. DescribeBandwidthRateLimitInput -> Aff (err :: RequestError | eff) DescribeBandwidthRateLimitOutput
```

<p>Returns the bandwidth rate limits of a gateway. By default, these limits are not set, which means no bandwidth rate limiting is in effect.</p> <p>This operation only returns a value for a bandwidth rate limit only if the limit is set. If no limits are set for the gateway, then this operation returns only the gateway ARN in the response body. To specify which gateway to describe, use the Amazon Resource Name (ARN) of the gateway in your request.</p>

#### `describeCache`

``` purescript
describeCache :: forall eff. DescribeCacheInput -> Aff (err :: RequestError | eff) DescribeCacheOutput
```

<p>Returns information about the cache of a gateway. This operation is only supported in the cached volume, tape and file gateway types.</p> <p>The response includes disk IDs that are configured as cache, and it includes the amount of cache allocated and used.</p>

#### `describeCachediSCSIVolumes`

``` purescript
describeCachediSCSIVolumes :: forall eff. DescribeCachediSCSIVolumesInput -> Aff (err :: RequestError | eff) DescribeCachediSCSIVolumesOutput
```

<p>Returns a description of the gateway volumes specified in the request. This operation is only supported in the cached volume gateway types.</p> <p>The list of gateway volumes in the request must be from one gateway. In the response Amazon Storage Gateway returns volume information sorted by volume Amazon Resource Name (ARN).</p>

#### `describeChapCredentials`

``` purescript
describeChapCredentials :: forall eff. DescribeChapCredentialsInput -> Aff (err :: RequestError | eff) DescribeChapCredentialsOutput
```

<p>Returns an array of Challenge-Handshake Authentication Protocol (CHAP) credentials information for a specified iSCSI target, one for each target-initiator pair.</p>

#### `describeGatewayInformation`

``` purescript
describeGatewayInformation :: forall eff. DescribeGatewayInformationInput -> Aff (err :: RequestError | eff) DescribeGatewayInformationOutput
```

<p>Returns metadata about a gateway such as its name, network interfaces, configured time zone, and the state (whether the gateway is running or not). To specify which gateway to describe, use the Amazon Resource Name (ARN) of the gateway in your request.</p>

#### `describeMaintenanceStartTime`

``` purescript
describeMaintenanceStartTime :: forall eff. DescribeMaintenanceStartTimeInput -> Aff (err :: RequestError | eff) DescribeMaintenanceStartTimeOutput
```

<p>Returns your gateway's weekly maintenance start time including the day and time of the week. Note that values are in terms of the gateway's time zone.</p>

#### `describeNFSFileShares`

``` purescript
describeNFSFileShares :: forall eff. DescribeNFSFileSharesInput -> Aff (err :: RequestError | eff) DescribeNFSFileSharesOutput
```

<p>Gets a description for one or more file shares from a file gateway. This operation is only supported in the file gateway type.</p>

#### `describeSnapshotSchedule`

``` purescript
describeSnapshotSchedule :: forall eff. DescribeSnapshotScheduleInput -> Aff (err :: RequestError | eff) DescribeSnapshotScheduleOutput
```

<p>Describes the snapshot schedule for the specified gateway volume. The snapshot schedule information includes intervals at which snapshots are automatically initiated on the volume. This operation is only supported in the cached volume and stored volume types.</p>

#### `describeStorediSCSIVolumes`

``` purescript
describeStorediSCSIVolumes :: forall eff. DescribeStorediSCSIVolumesInput -> Aff (err :: RequestError | eff) DescribeStorediSCSIVolumesOutput
```

<p>Returns the description of the gateway volumes specified in the request. The list of gateway volumes in the request must be from one gateway. In the response Amazon Storage Gateway returns volume information sorted by volume ARNs. This operation is only supported in stored volume gateway type.</p>

#### `describeTapeArchives`

``` purescript
describeTapeArchives :: forall eff. DescribeTapeArchivesInput -> Aff (err :: RequestError | eff) DescribeTapeArchivesOutput
```

<p>Returns a description of specified virtual tapes in the virtual tape shelf (VTS). This operation is only supported in the tape gateway type.</p> <p>If a specific <code>TapeARN</code> is not specified, AWS Storage Gateway returns a description of all virtual tapes found in the VTS associated with your account.</p>

#### `describeTapeRecoveryPoints`

``` purescript
describeTapeRecoveryPoints :: forall eff. DescribeTapeRecoveryPointsInput -> Aff (err :: RequestError | eff) DescribeTapeRecoveryPointsOutput
```

<p>Returns a list of virtual tape recovery points that are available for the specified tape gateway.</p> <p>A recovery point is a point-in-time view of a virtual tape at which all the data on the virtual tape is consistent. If your gateway crashes, virtual tapes that have recovery points can be recovered to a new gateway. This operation is only supported in the tape gateway type.</p>

#### `describeTapes`

``` purescript
describeTapes :: forall eff. DescribeTapesInput -> Aff (err :: RequestError | eff) DescribeTapesOutput
```

<p>Returns a description of the specified Amazon Resource Name (ARN) of virtual tapes. If a <code>TapeARN</code> is not specified, returns a description of all virtual tapes associated with the specified gateway. This operation is only supported in the tape gateway type.</p>

#### `describeUploadBuffer`

``` purescript
describeUploadBuffer :: forall eff. DescribeUploadBufferInput -> Aff (err :: RequestError | eff) DescribeUploadBufferOutput
```

<p>Returns information about the upload buffer of a gateway. This operation is supported for the stored volume, cached volume and tape gateway types.</p> <p>The response includes disk IDs that are configured as upload buffer space, and it includes the amount of upload buffer space allocated and used.</p>

#### `describeVTLDevices`

``` purescript
describeVTLDevices :: forall eff. DescribeVTLDevicesInput -> Aff (err :: RequestError | eff) DescribeVTLDevicesOutput
```

<p>Returns a description of virtual tape library (VTL) devices for the specified tape gateway. In the response, AWS Storage Gateway returns VTL device information.</p> <p>This operation is only supported in the tape gateway type.</p>

#### `describeWorkingStorage`

``` purescript
describeWorkingStorage :: forall eff. DescribeWorkingStorageInput -> Aff (err :: RequestError | eff) DescribeWorkingStorageOutput
```

<p>Returns information about the working storage of a gateway. This operation is only supported in the stored volumes gateway type. This operation is deprecated in cached volumes API version (20120630). Use DescribeUploadBuffer instead.</p> <note> <p>Working storage is also referred to as upload buffer. You can also use the DescribeUploadBuffer operation to add upload buffer to a stored volume gateway.</p> </note> <p>The response includes disk IDs that are configured as working storage, and it includes the amount of working storage allocated and used.</p>

#### `disableGateway`

``` purescript
disableGateway :: forall eff. DisableGatewayInput -> Aff (err :: RequestError | eff) DisableGatewayOutput
```

<p>Disables a tape gateway when the gateway is no longer functioning. For example, if your gateway VM is damaged, you can disable the gateway so you can recover virtual tapes.</p> <p>Use this operation for a tape gateway that is not reachable or not functioning. This operation is only supported in the tape gateway type.</p> <important> <p>Once a gateway is disabled it cannot be enabled.</p> </important>

#### `listFileShares`

``` purescript
listFileShares :: forall eff. ListFileSharesInput -> Aff (err :: RequestError | eff) ListFileSharesOutput
```

<p>Gets a list of the file shares for a specific file gateway, or the list of file shares that belong to the calling user account. This operation is only supported in the file gateway type.</p>

#### `listGateways`

``` purescript
listGateways :: forall eff. ListGatewaysInput -> Aff (err :: RequestError | eff) ListGatewaysOutput
```

<p>Lists gateways owned by an AWS account in a region specified in the request. The returned list is ordered by gateway Amazon Resource Name (ARN).</p> <p>By default, the operation returns a maximum of 100 gateways. This operation supports pagination that allows you to optionally reduce the number of gateways returned in a response.</p> <p>If you have more gateways than are returned in a response (that is, the response returns only a truncated list of your gateways), the response contains a marker that you can specify in your next request to fetch the next page of gateways.</p>

#### `listLocalDisks`

``` purescript
listLocalDisks :: forall eff. ListLocalDisksInput -> Aff (err :: RequestError | eff) ListLocalDisksOutput
```

<p>Returns a list of the gateway's local disks. To specify which gateway to describe, you use the Amazon Resource Name (ARN) of the gateway in the body of the request.</p> <p>The request returns a list of all disks, specifying which are configured as working storage, cache storage, or stored volume or not configured at all. The response includes a <code>DiskStatus</code> field. This field can have a value of present (the disk is available to use), missing (the disk is no longer connected to the gateway), or mismatch (the disk node is occupied by a disk that has incorrect metadata or the disk content is corrupted).</p>

#### `listTagsForResource`

``` purescript
listTagsForResource :: forall eff. ListTagsForResourceInput -> Aff (err :: RequestError | eff) ListTagsForResourceOutput
```

<p>Lists the tags that have been added to the specified resource. This operation is only supported in the cached volume, stored volume and tape gateway type.</p>

#### `listTapes`

``` purescript
listTapes :: forall eff. ListTapesInput -> Aff (err :: RequestError | eff) ListTapesOutput
```

<p>Lists virtual tapes in your virtual tape library (VTL) and your virtual tape shelf (VTS). You specify the tapes to list by specifying one or more tape Amazon Resource Names (ARNs). If you don't specify a tape ARN, the operation lists all virtual tapes in both your VTL and VTS.</p> <p>This operation supports pagination. By default, the operation returns a maximum of up to 100 tapes. You can optionally specify the <code>Limit</code> parameter in the body to limit the number of tapes in the response. If the number of tapes returned in the response is truncated, the response includes a <code>Marker</code> element that you can use in your subsequent request to retrieve the next set of tapes. This operation is only supported in the tape gateway type.</p>

#### `listVolumeInitiators`

``` purescript
listVolumeInitiators :: forall eff. ListVolumeInitiatorsInput -> Aff (err :: RequestError | eff) ListVolumeInitiatorsOutput
```

<p>Lists iSCSI initiators that are connected to a volume. You can use this operation to determine whether a volume is being used or not. This operation is only supported in the cached volume and stored volume gateway types.</p>

#### `listVolumeRecoveryPoints`

``` purescript
listVolumeRecoveryPoints :: forall eff. ListVolumeRecoveryPointsInput -> Aff (err :: RequestError | eff) ListVolumeRecoveryPointsOutput
```

<p>Lists the recovery points for a specified gateway. This operation is only supported in the cached volume gateway type.</p> <p>Each cache volume has one recovery point. A volume recovery point is a point in time at which all data of the volume is consistent and from which you can create a snapshot or clone a new cached volume from a source volume. To create a snapshot from a volume recovery point use the <a>CreateSnapshotFromVolumeRecoveryPoint</a> operation.</p>

#### `listVolumes`

``` purescript
listVolumes :: forall eff. ListVolumesInput -> Aff (err :: RequestError | eff) ListVolumesOutput
```

<p>Lists the iSCSI stored volumes of a gateway. Results are sorted by volume ARN. The response includes only the volume ARNs. If you want additional volume information, use the <a>DescribeStorediSCSIVolumes</a> or the <a>DescribeCachediSCSIVolumes</a> API.</p> <p>The operation supports pagination. By default, the operation returns a maximum of up to 100 volumes. You can optionally specify the <code>Limit</code> field in the body to limit the number of volumes in the response. If the number of volumes returned in the response is truncated, the response includes a Marker field. You can use this Marker value in your subsequent request to retrieve the next set of volumes. This operation is only supported in the cached volume and stored volume gateway types.</p>

#### `notifyWhenUploaded`

``` purescript
notifyWhenUploaded :: forall eff. NotifyWhenUploadedInput -> Aff (err :: RequestError | eff) NotifyWhenUploadedOutput
```

<p>Sends you notification when all file data written to the NFS file share has been uploaded to Amazon S3.</p> <p>AWS Storage Gateway can send a notification through Amazon CloudWatch Events when all files written to your file share up to that point in time have been uploaded to Amazon S3. These files include files written to the NFS file share up to the time that you make a request for notification. When the upload is done, Storage Gateway sends you notification through an Amazon CloudWatch event. You can configure CloudWatch Events to sent the notification through event targets such as email, SNS or a Lambda function. text or Lambda functions. This operation is only supported in the file gateway type.</p>

#### `refreshCache`

``` purescript
refreshCache :: forall eff. RefreshCacheInput -> Aff (err :: RequestError | eff) RefreshCacheOutput
```

<p>Refreshes the cache for the specified file share. This operation finds objects in the Amazon S3 bucket that were added, removed or replaced since the gateway last listed the bucket's contents and cached the results. This operation is only supported in the file gateway type.</p>

#### `removeTagsFromResource`

``` purescript
removeTagsFromResource :: forall eff. RemoveTagsFromResourceInput -> Aff (err :: RequestError | eff) RemoveTagsFromResourceOutput
```

<p>Removes one or more tags from the specified resource. This operation is only supported in the cached volume, stored volume and tape gateway types.</p>

#### `resetCache`

``` purescript
resetCache :: forall eff. ResetCacheInput -> Aff (err :: RequestError | eff) ResetCacheOutput
```

<p>Resets all cache disks that have encountered a error and makes the disks available for reconfiguration as cache storage. If your cache disk encounters a error, the gateway prevents read and write operations on virtual tapes in the gateway. For example, an error can occur when a disk is corrupted or removed from the gateway. When a cache is reset, the gateway loses its cache storage. At this point you can reconfigure the disks as cache disks. This operation is only supported in the cached volume, tape and file gateway types.</p> <important> <p>If the cache disk you are resetting contains data that has not been uploaded to Amazon S3 yet, that data can be lost. After you reset cache disks, there will be no configured cache disks left in the gateway, so you must configure at least one new cache disk for your gateway to function properly.</p> </important>

#### `retrieveTapeArchive`

``` purescript
retrieveTapeArchive :: forall eff. RetrieveTapeArchiveInput -> Aff (err :: RequestError | eff) RetrieveTapeArchiveOutput
```

<p>Retrieves an archived virtual tape from the virtual tape shelf (VTS) to a tape gateway. Virtual tapes archived in the VTS are not associated with any gateway. However after a tape is retrieved, it is associated with a gateway, even though it is also listed in the VTS, that is, archive. This operation is only supported in the tape gateway type.</p> <p>Once a tape is successfully retrieved to a gateway, it cannot be retrieved again to another gateway. You must archive the tape again before you can retrieve it to another gateway. This operation is only supported in the tape gateway type.</p>

#### `retrieveTapeRecoveryPoint`

``` purescript
retrieveTapeRecoveryPoint :: forall eff. RetrieveTapeRecoveryPointInput -> Aff (err :: RequestError | eff) RetrieveTapeRecoveryPointOutput
```

<p>Retrieves the recovery point for the specified virtual tape. This operation is only supported in the tape gateway type.</p> <p>A recovery point is a point in time view of a virtual tape at which all the data on the tape is consistent. If your gateway crashes, virtual tapes that have recovery points can be recovered to a new gateway.</p> <note> <p>The virtual tape can be retrieved to only one gateway. The retrieved tape is read-only. The virtual tape can be retrieved to only a tape gateway. There is no charge for retrieving recovery points.</p> </note>

#### `setLocalConsolePassword`

``` purescript
setLocalConsolePassword :: forall eff. SetLocalConsolePasswordInput -> Aff (err :: RequestError | eff) SetLocalConsolePasswordOutput
```

<p>Sets the password for your VM local console. When you log in to the local console for the first time, you log in to the VM with the default credentials. We recommend that you set a new password. You don't need to know the default password to set a new password.</p>

#### `shutdownGateway`

``` purescript
shutdownGateway :: forall eff. ShutdownGatewayInput -> Aff (err :: RequestError | eff) ShutdownGatewayOutput
```

<p>Shuts down a gateway. To specify which gateway to shut down, use the Amazon Resource Name (ARN) of the gateway in the body of your request.</p> <p>The operation shuts down the gateway service component running in the gateway's virtual machine (VM) and not the host VM.</p> <note> <p>If you want to shut down the VM, it is recommended that you first shut down the gateway component in the VM to avoid unpredictable conditions.</p> </note> <p>After the gateway is shutdown, you cannot call any other API except <a>StartGateway</a>, <a>DescribeGatewayInformation</a>, and <a>ListGateways</a>. For more information, see <a>ActivateGateway</a>. Your applications cannot read from or write to the gateway's storage volumes, and there are no snapshots taken.</p> <note> <p>When you make a shutdown request, you will get a <code>200 OK</code> success response immediately. However, it might take some time for the gateway to shut down. You can call the <a>DescribeGatewayInformation</a> API to check the status. For more information, see <a>ActivateGateway</a>.</p> </note> <p>If do not intend to use the gateway again, you must delete the gateway (using <a>DeleteGateway</a>) to no longer pay software charges associated with the gateway.</p>

#### `startGateway`

``` purescript
startGateway :: forall eff. StartGatewayInput -> Aff (err :: RequestError | eff) StartGatewayOutput
```

<p>Starts a gateway that you previously shut down (see <a>ShutdownGateway</a>). After the gateway starts, you can then make other API calls, your applications can read from or write to the gateway's storage volumes and you will be able to take snapshot backups.</p> <note> <p>When you make a request, you will get a 200 OK success response immediately. However, it might take some time for the gateway to be ready. You should call <a>DescribeGatewayInformation</a> and check the status before making any additional API calls. For more information, see <a>ActivateGateway</a>.</p> </note> <p>To specify which gateway to start, use the Amazon Resource Name (ARN) of the gateway in your request.</p>

#### `updateBandwidthRateLimit`

``` purescript
updateBandwidthRateLimit :: forall eff. UpdateBandwidthRateLimitInput -> Aff (err :: RequestError | eff) UpdateBandwidthRateLimitOutput
```

<p>Updates the bandwidth rate limits of a gateway. You can update both the upload and download bandwidth rate limit or specify only one of the two. If you don't set a bandwidth rate limit, the existing rate limit remains.</p> <p>By default, a gateway's bandwidth rate limits are not set. If you don't set any limit, the gateway does not have any limitations on its bandwidth usage and could potentially use the maximum available bandwidth.</p> <p>To specify which gateway to update, use the Amazon Resource Name (ARN) of the gateway in your request.</p>

#### `updateChapCredentials`

``` purescript
updateChapCredentials :: forall eff. UpdateChapCredentialsInput -> Aff (err :: RequestError | eff) UpdateChapCredentialsOutput
```

<p>Updates the Challenge-Handshake Authentication Protocol (CHAP) credentials for a specified iSCSI target. By default, a gateway does not have CHAP enabled; however, for added security, you might use it.</p> <important> <p>When you update CHAP credentials, all existing connections on the target are closed and initiators must reconnect with the new credentials.</p> </important>

#### `updateGatewayInformation`

``` purescript
updateGatewayInformation :: forall eff. UpdateGatewayInformationInput -> Aff (err :: RequestError | eff) UpdateGatewayInformationOutput
```

<p>Updates a gateway's metadata, which includes the gateway's name and time zone. To specify which gateway to update, use the Amazon Resource Name (ARN) of the gateway in your request.</p> <note> <p>For Gateways activated after September 2, 2015, the gateway's ARN contains the gateway ID rather than the gateway name. However, changing the name of the gateway has no effect on the gateway's ARN.</p> </note>

#### `updateGatewaySoftwareNow`

``` purescript
updateGatewaySoftwareNow :: forall eff. UpdateGatewaySoftwareNowInput -> Aff (err :: RequestError | eff) UpdateGatewaySoftwareNowOutput
```

<p>Updates the gateway virtual machine (VM) software. The request immediately triggers the software update.</p> <note> <p>When you make this request, you get a <code>200 OK</code> success response immediately. However, it might take some time for the update to complete. You can call <a>DescribeGatewayInformation</a> to verify the gateway is in the <code>STATE_RUNNING</code> state.</p> </note> <important> <p>A software update forces a system restart of your gateway. You can minimize the chance of any disruption to your applications by increasing your iSCSI Initiators' timeouts. For more information about increasing iSCSI Initiator timeouts for Windows and Linux, see <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorWindowsClient.html#CustomizeWindowsiSCSISettings">Customizing Your Windows iSCSI Settings</a> and <a href="http://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorRedHatClient.html#CustomizeLinuxiSCSISettings">Customizing Your Linux iSCSI Settings</a>, respectively.</p> </important>

#### `updateMaintenanceStartTime`

``` purescript
updateMaintenanceStartTime :: forall eff. UpdateMaintenanceStartTimeInput -> Aff (err :: RequestError | eff) UpdateMaintenanceStartTimeOutput
```

<p>Updates a gateway's weekly maintenance start time information, including day and time of the week. The maintenance time is the time in your gateway's time zone.</p>

#### `updateNFSFileShare`

``` purescript
updateNFSFileShare :: forall eff. UpdateNFSFileShareInput -> Aff (err :: RequestError | eff) UpdateNFSFileShareOutput
```

<p>Updates a file share. This operation is only supported in the file gateway type.</p> <note> <p>To leave a file share field unchanged, set the corresponding input field to null.</p> </note> <p>Updates the following file share setting:</p> <ul> <li> <p>Default storage class for your S3 bucket</p> </li> <li> <p>Metadata defaults for your S3 bucket</p> </li> <li> <p>Allowed NFS clients for your file share</p> </li> <li> <p>Squash settings</p> </li> <li> <p>Write status of your file share</p> </li> </ul> <note> <p>To leave a file share field unchanged, set the corresponding input field to null. This operation is only supported in file gateways.</p> </note>

#### `updateSnapshotSchedule`

``` purescript
updateSnapshotSchedule :: forall eff. UpdateSnapshotScheduleInput -> Aff (err :: RequestError | eff) UpdateSnapshotScheduleOutput
```

<p>Updates a snapshot schedule configured for a gateway volume. This operation is only supported in the cached volume and stored volume gateway types.</p> <p>The default snapshot schedule for volume is once every 24 hours, starting at the creation time of the volume. You can use this API to change the snapshot schedule configured for the volume.</p> <p>In the request you must identify the gateway volume whose snapshot schedule you want to update, and the schedule information, including when you want the snapshot to begin on a day and the frequency (in hours) of snapshots.</p>

#### `updateVTLDeviceType`

``` purescript
updateVTLDeviceType :: forall eff. UpdateVTLDeviceTypeInput -> Aff (err :: RequestError | eff) UpdateVTLDeviceTypeOutput
```

<p>Updates the type of medium changer in a tape gateway. When you activate a tape gateway, you select a medium changer type for the tape gateway. This operation enables you to select a different type of medium changer after a tape gateway is activated. This operation is only supported in the tape gateway type.</p>

#### `ActivateGatewayInput`

``` purescript
newtype ActivateGatewayInput
  = ActivateGatewayInput { "ActivationKey" :: ActivationKey, "GatewayName" :: GatewayName, "GatewayTimezone" :: GatewayTimezone, "GatewayRegion" :: RegionId, "GatewayType" :: NullOrUndefined (GatewayType), "TapeDriveType" :: NullOrUndefined (TapeDriveType), "MediumChangerType" :: NullOrUndefined (MediumChangerType) }
```

<p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>ActivateGatewayInput$ActivationKey</a> </p> </li> <li> <p> <a>ActivateGatewayInput$GatewayName</a> </p> </li> <li> <p> <a>ActivateGatewayInput$GatewayRegion</a> </p> </li> <li> <p> <a>ActivateGatewayInput$GatewayTimezone</a> </p> </li> <li> <p> <a>ActivateGatewayInput$GatewayType</a> </p> </li> <li> <p> <a>ActivateGatewayInput$TapeDriveType</a> </p> </li> <li> <p> <a>ActivateGatewayInput$MediumChangerType</a> </p> </li> </ul>

#### `ActivateGatewayOutput`

``` purescript
newtype ActivateGatewayOutput
  = ActivateGatewayOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

<p>AWS Storage Gateway returns the Amazon Resource Name (ARN) of the activated gateway. It is a string made of information such as your account, gateway name, and region. This ARN is used to reference the gateway in other API operations as well as resource-based authorization.</p> <note> <p>For gateways activated prior to September 02, 2015, the gateway ARN contains the gateway name rather than the gateway ID. Changing the name of the gateway has no effect on the gateway ARN.</p> </note>

#### `ActivationKey`

``` purescript
newtype ActivationKey
  = ActivationKey String
```

#### `AddCacheInput`

``` purescript
newtype AddCacheInput
  = AddCacheInput { "GatewayARN" :: GatewayARN, "DiskIds" :: DiskIds }
```

#### `AddCacheOutput`

``` purescript
newtype AddCacheOutput
  = AddCacheOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

#### `AddTagsToResourceInput`

``` purescript
newtype AddTagsToResourceInput
  = AddTagsToResourceInput { "ResourceARN" :: ResourceARN, "Tags" :: Tags }
```

<p>AddTagsToResourceInput</p>

#### `AddTagsToResourceOutput`

``` purescript
newtype AddTagsToResourceOutput
  = AddTagsToResourceOutput { "ResourceARN" :: NullOrUndefined (ResourceARN) }
```

<p>AddTagsToResourceOutput</p>

#### `AddUploadBufferInput`

``` purescript
newtype AddUploadBufferInput
  = AddUploadBufferInput { "GatewayARN" :: GatewayARN, "DiskIds" :: DiskIds }
```

#### `AddUploadBufferOutput`

``` purescript
newtype AddUploadBufferOutput
  = AddUploadBufferOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

#### `AddWorkingStorageInput`

``` purescript
newtype AddWorkingStorageInput
  = AddWorkingStorageInput { "GatewayARN" :: GatewayARN, "DiskIds" :: DiskIds }
```

<p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>AddWorkingStorageInput$DiskIds</a> </p> </li> </ul>

#### `AddWorkingStorageOutput`

``` purescript
newtype AddWorkingStorageOutput
  = AddWorkingStorageOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

<p>A JSON object containing the of the gateway for which working storage was configured.</p>

#### `BandwidthDownloadRateLimit`

``` purescript
newtype BandwidthDownloadRateLimit
  = BandwidthDownloadRateLimit Number
```

#### `BandwidthType`

``` purescript
newtype BandwidthType
  = BandwidthType String
```

#### `BandwidthUploadRateLimit`

``` purescript
newtype BandwidthUploadRateLimit
  = BandwidthUploadRateLimit Number
```

#### `CachediSCSIVolume`

``` purescript
newtype CachediSCSIVolume
  = CachediSCSIVolume { "VolumeARN" :: NullOrUndefined (VolumeARN), "VolumeId" :: NullOrUndefined (VolumeId), "VolumeType" :: NullOrUndefined (VolumeType), "VolumeStatus" :: NullOrUndefined (VolumeStatus), "VolumeSizeInBytes" :: NullOrUndefined (Number), "VolumeProgress" :: NullOrUndefined (DoubleObject), "SourceSnapshotId" :: NullOrUndefined (SnapshotId), "VolumeiSCSIAttributes" :: NullOrUndefined (VolumeiSCSIAttributes), "CreatedDate" :: NullOrUndefined (CreatedDate), "VolumeUsedInBytes" :: NullOrUndefined (VolumeUsedInBytes) }
```

<p>Describes an iSCSI cached volume.</p>

#### `CachediSCSIVolumes`

``` purescript
newtype CachediSCSIVolumes
  = CachediSCSIVolumes (Array CachediSCSIVolume)
```

#### `CancelArchivalInput`

``` purescript
newtype CancelArchivalInput
  = CancelArchivalInput { "GatewayARN" :: GatewayARN, "TapeARN" :: TapeARN }
```

<p>CancelArchivalInput</p>

#### `CancelArchivalOutput`

``` purescript
newtype CancelArchivalOutput
  = CancelArchivalOutput { "TapeARN" :: NullOrUndefined (TapeARN) }
```

<p>CancelArchivalOutput</p>

#### `CancelRetrievalInput`

``` purescript
newtype CancelRetrievalInput
  = CancelRetrievalInput { "GatewayARN" :: GatewayARN, "TapeARN" :: TapeARN }
```

<p>CancelRetrievalInput</p>

#### `CancelRetrievalOutput`

``` purescript
newtype CancelRetrievalOutput
  = CancelRetrievalOutput { "TapeARN" :: NullOrUndefined (TapeARN) }
```

<p>CancelRetrievalOutput</p>

#### `ChapCredentials`

``` purescript
newtype ChapCredentials
  = ChapCredentials (Array ChapInfo)
```

#### `ChapInfo`

``` purescript
newtype ChapInfo
  = ChapInfo { "TargetARN" :: NullOrUndefined (TargetARN), "SecretToAuthenticateInitiator" :: NullOrUndefined (ChapSecret), "InitiatorName" :: NullOrUndefined (IqnName), "SecretToAuthenticateTarget" :: NullOrUndefined (ChapSecret) }
```

<p>Describes Challenge-Handshake Authentication Protocol (CHAP) information that supports authentication between your gateway and iSCSI initiators.</p>

#### `ChapSecret`

``` purescript
newtype ChapSecret
  = ChapSecret String
```

#### `ClientToken`

``` purescript
newtype ClientToken
  = ClientToken String
```

#### `CreateCachediSCSIVolumeInput`

``` purescript
newtype CreateCachediSCSIVolumeInput
  = CreateCachediSCSIVolumeInput { "GatewayARN" :: GatewayARN, "VolumeSizeInBytes" :: Number, "SnapshotId" :: NullOrUndefined (SnapshotId), "TargetName" :: TargetName, "SourceVolumeARN" :: NullOrUndefined (VolumeARN), "NetworkInterfaceId" :: NetworkInterfaceId, "ClientToken" :: ClientToken }
```

#### `CreateCachediSCSIVolumeOutput`

``` purescript
newtype CreateCachediSCSIVolumeOutput
  = CreateCachediSCSIVolumeOutput { "VolumeARN" :: NullOrUndefined (VolumeARN), "TargetARN" :: NullOrUndefined (TargetARN) }
```

#### `CreateNFSFileShareInput`

``` purescript
newtype CreateNFSFileShareInput
  = CreateNFSFileShareInput { "ClientToken" :: ClientToken, "NFSFileShareDefaults" :: NullOrUndefined (NFSFileShareDefaults), "GatewayARN" :: GatewayARN, "KMSEncrypted" :: NullOrUndefined (Boolean), "KMSKey" :: NullOrUndefined (KMSKey), "Role" :: Role, "LocationARN" :: LocationARN, "DefaultStorageClass" :: NullOrUndefined (StorageClass), "ClientList" :: NullOrUndefined (FileShareClientList), "Squash" :: NullOrUndefined (Squash), "ReadOnly" :: NullOrUndefined (Boolean), "GuessMIMETypeEnabled" :: NullOrUndefined (Boolean) }
```

<p>CreateNFSFileShareInput</p>

#### `CreateNFSFileShareOutput`

``` purescript
newtype CreateNFSFileShareOutput
  = CreateNFSFileShareOutput { "FileShareARN" :: NullOrUndefined (FileShareARN) }
```

<p>CreateNFSFileShareOutput</p>

#### `CreateSnapshotFromVolumeRecoveryPointInput`

``` purescript
newtype CreateSnapshotFromVolumeRecoveryPointInput
  = CreateSnapshotFromVolumeRecoveryPointInput { "VolumeARN" :: VolumeARN, "SnapshotDescription" :: SnapshotDescription }
```

#### `CreateSnapshotFromVolumeRecoveryPointOutput`

``` purescript
newtype CreateSnapshotFromVolumeRecoveryPointOutput
  = CreateSnapshotFromVolumeRecoveryPointOutput { "SnapshotId" :: NullOrUndefined (SnapshotId), "VolumeARN" :: NullOrUndefined (VolumeARN), "VolumeRecoveryPointTime" :: NullOrUndefined (String) }
```

#### `CreateSnapshotInput`

``` purescript
newtype CreateSnapshotInput
  = CreateSnapshotInput { "VolumeARN" :: VolumeARN, "SnapshotDescription" :: SnapshotDescription }
```

<p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>CreateSnapshotInput$SnapshotDescription</a> </p> </li> <li> <p> <a>CreateSnapshotInput$VolumeARN</a> </p> </li> </ul>

#### `CreateSnapshotOutput`

``` purescript
newtype CreateSnapshotOutput
  = CreateSnapshotOutput { "VolumeARN" :: NullOrUndefined (VolumeARN), "SnapshotId" :: NullOrUndefined (SnapshotId) }
```

<p>A JSON object containing the following fields:</p>

#### `CreateStorediSCSIVolumeInput`

``` purescript
newtype CreateStorediSCSIVolumeInput
  = CreateStorediSCSIVolumeInput { "GatewayARN" :: GatewayARN, "DiskId" :: DiskId, "SnapshotId" :: NullOrUndefined (SnapshotId), "PreserveExistingData" :: Boolean, "TargetName" :: TargetName, "NetworkInterfaceId" :: NetworkInterfaceId }
```

<p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>CreateStorediSCSIVolumeInput$DiskId</a> </p> </li> <li> <p> <a>CreateStorediSCSIVolumeInput$NetworkInterfaceId</a> </p> </li> <li> <p> <a>CreateStorediSCSIVolumeInput$PreserveExistingData</a> </p> </li> <li> <p> <a>CreateStorediSCSIVolumeInput$SnapshotId</a> </p> </li> <li> <p> <a>CreateStorediSCSIVolumeInput$TargetName</a> </p> </li> </ul>

#### `CreateStorediSCSIVolumeOutput`

``` purescript
newtype CreateStorediSCSIVolumeOutput
  = CreateStorediSCSIVolumeOutput { "VolumeARN" :: NullOrUndefined (VolumeARN), "VolumeSizeInBytes" :: NullOrUndefined (Number), "TargetARN" :: NullOrUndefined (TargetARN) }
```

<p>A JSON object containing the following fields:</p>

#### `CreateTapeWithBarcodeInput`

``` purescript
newtype CreateTapeWithBarcodeInput
  = CreateTapeWithBarcodeInput { "GatewayARN" :: GatewayARN, "TapeSizeInBytes" :: TapeSize, "TapeBarcode" :: TapeBarcode }
```

<p>CreateTapeWithBarcodeInput</p>

#### `CreateTapeWithBarcodeOutput`

``` purescript
newtype CreateTapeWithBarcodeOutput
  = CreateTapeWithBarcodeOutput { "TapeARN" :: NullOrUndefined (TapeARN) }
```

<p>CreateTapeOutput</p>

#### `CreateTapesInput`

``` purescript
newtype CreateTapesInput
  = CreateTapesInput { "GatewayARN" :: GatewayARN, "TapeSizeInBytes" :: TapeSize, "ClientToken" :: ClientToken, "NumTapesToCreate" :: NumTapesToCreate, "TapeBarcodePrefix" :: TapeBarcodePrefix }
```

<p>CreateTapesInput</p>

#### `CreateTapesOutput`

``` purescript
newtype CreateTapesOutput
  = CreateTapesOutput { "TapeARNs" :: NullOrUndefined (TapeARNs) }
```

<p>CreateTapeOutput</p>

#### `CreatedDate`

``` purescript
newtype CreatedDate
  = CreatedDate Number
```

#### `DayOfWeek`

``` purescript
newtype DayOfWeek
  = DayOfWeek Int
```

#### `DeleteBandwidthRateLimitInput`

``` purescript
newtype DeleteBandwidthRateLimitInput
  = DeleteBandwidthRateLimitInput { "GatewayARN" :: GatewayARN, "BandwidthType" :: BandwidthType }
```

<p>A JSON object containing the following fields:</p> <ul> <li> <p> <a>DeleteBandwidthRateLimitInput$BandwidthType</a> </p> </li> </ul>

#### `DeleteBandwidthRateLimitOutput`

``` purescript
newtype DeleteBandwidthRateLimitOutput
  = DeleteBandwidthRateLimitOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

<p>A JSON object containing the of the gateway whose bandwidth rate information was deleted.</p>

#### `DeleteChapCredentialsInput`

``` purescript
newtype DeleteChapCredentialsInput
  = DeleteChapCredentialsInput { "TargetARN" :: TargetARN, "InitiatorName" :: IqnName }
```

<p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>DeleteChapCredentialsInput$InitiatorName</a> </p> </li> <li> <p> <a>DeleteChapCredentialsInput$TargetARN</a> </p> </li> </ul>

#### `DeleteChapCredentialsOutput`

``` purescript
newtype DeleteChapCredentialsOutput
  = DeleteChapCredentialsOutput { "TargetARN" :: NullOrUndefined (TargetARN), "InitiatorName" :: NullOrUndefined (IqnName) }
```

<p>A JSON object containing the following fields:</p>

#### `DeleteFileShareInput`

``` purescript
newtype DeleteFileShareInput
  = DeleteFileShareInput { "FileShareARN" :: FileShareARN, "ForceDelete" :: NullOrUndefined (Boolean) }
```

<p>DeleteFileShareInput</p>

#### `DeleteFileShareOutput`

``` purescript
newtype DeleteFileShareOutput
  = DeleteFileShareOutput { "FileShareARN" :: NullOrUndefined (FileShareARN) }
```

<p>DeleteFileShareOutput</p>

#### `DeleteGatewayInput`

``` purescript
newtype DeleteGatewayInput
  = DeleteGatewayInput { "GatewayARN" :: GatewayARN }
```

<p>A JSON object containing the ID of the gateway to delete.</p>

#### `DeleteGatewayOutput`

``` purescript
newtype DeleteGatewayOutput
  = DeleteGatewayOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

<p>A JSON object containing the ID of the deleted gateway.</p>

#### `DeleteSnapshotScheduleInput`

``` purescript
newtype DeleteSnapshotScheduleInput
  = DeleteSnapshotScheduleInput { "VolumeARN" :: VolumeARN }
```

#### `DeleteSnapshotScheduleOutput`

``` purescript
newtype DeleteSnapshotScheduleOutput
  = DeleteSnapshotScheduleOutput { "VolumeARN" :: NullOrUndefined (VolumeARN) }
```

#### `DeleteTapeArchiveInput`

``` purescript
newtype DeleteTapeArchiveInput
  = DeleteTapeArchiveInput { "TapeARN" :: TapeARN }
```

<p>DeleteTapeArchiveInput</p>

#### `DeleteTapeArchiveOutput`

``` purescript
newtype DeleteTapeArchiveOutput
  = DeleteTapeArchiveOutput { "TapeARN" :: NullOrUndefined (TapeARN) }
```

<p>DeleteTapeArchiveOutput</p>

#### `DeleteTapeInput`

``` purescript
newtype DeleteTapeInput
  = DeleteTapeInput { "GatewayARN" :: GatewayARN, "TapeARN" :: TapeARN }
```

<p>DeleteTapeInput</p>

#### `DeleteTapeOutput`

``` purescript
newtype DeleteTapeOutput
  = DeleteTapeOutput { "TapeARN" :: NullOrUndefined (TapeARN) }
```

<p>DeleteTapeOutput</p>

#### `DeleteVolumeInput`

``` purescript
newtype DeleteVolumeInput
  = DeleteVolumeInput { "VolumeARN" :: VolumeARN }
```

<p>A JSON object containing the <a>DeleteVolumeInput$VolumeARN</a> to delete.</p>

#### `DeleteVolumeOutput`

``` purescript
newtype DeleteVolumeOutput
  = DeleteVolumeOutput { "VolumeARN" :: NullOrUndefined (VolumeARN) }
```

<p>A JSON object containing the of the storage volume that was deleted</p>

#### `DescribeBandwidthRateLimitInput`

``` purescript
newtype DescribeBandwidthRateLimitInput
  = DescribeBandwidthRateLimitInput { "GatewayARN" :: GatewayARN }
```

<p>A JSON object containing the of the gateway.</p>

#### `DescribeBandwidthRateLimitOutput`

``` purescript
newtype DescribeBandwidthRateLimitOutput
  = DescribeBandwidthRateLimitOutput { "GatewayARN" :: NullOrUndefined (GatewayARN), "AverageUploadRateLimitInBitsPerSec" :: NullOrUndefined (BandwidthUploadRateLimit), "AverageDownloadRateLimitInBitsPerSec" :: NullOrUndefined (BandwidthDownloadRateLimit) }
```

<p>A JSON object containing the following fields:</p>

#### `DescribeCacheInput`

``` purescript
newtype DescribeCacheInput
  = DescribeCacheInput { "GatewayARN" :: GatewayARN }
```

#### `DescribeCacheOutput`

``` purescript
newtype DescribeCacheOutput
  = DescribeCacheOutput { "GatewayARN" :: NullOrUndefined (GatewayARN), "DiskIds" :: NullOrUndefined (DiskIds), "CacheAllocatedInBytes" :: NullOrUndefined (Number), "CacheUsedPercentage" :: NullOrUndefined (Number), "CacheDirtyPercentage" :: NullOrUndefined (Number), "CacheHitPercentage" :: NullOrUndefined (Number), "CacheMissPercentage" :: NullOrUndefined (Number) }
```

#### `DescribeCachediSCSIVolumesInput`

``` purescript
newtype DescribeCachediSCSIVolumesInput
  = DescribeCachediSCSIVolumesInput { "VolumeARNs" :: VolumeARNs }
```

#### `DescribeCachediSCSIVolumesOutput`

``` purescript
newtype DescribeCachediSCSIVolumesOutput
  = DescribeCachediSCSIVolumesOutput { "CachediSCSIVolumes" :: NullOrUndefined (CachediSCSIVolumes) }
```

<p>A JSON object containing the following fields:</p>

#### `DescribeChapCredentialsInput`

``` purescript
newtype DescribeChapCredentialsInput
  = DescribeChapCredentialsInput { "TargetARN" :: TargetARN }
```

<p>A JSON object containing the Amazon Resource Name (ARN) of the iSCSI volume target.</p>

#### `DescribeChapCredentialsOutput`

``` purescript
newtype DescribeChapCredentialsOutput
  = DescribeChapCredentialsOutput { "ChapCredentials" :: NullOrUndefined (ChapCredentials) }
```

<p>A JSON object containing a .</p>

#### `DescribeGatewayInformationInput`

``` purescript
newtype DescribeGatewayInformationInput
  = DescribeGatewayInformationInput { "GatewayARN" :: GatewayARN }
```

<p>A JSON object containing the ID of the gateway.</p>

#### `DescribeGatewayInformationOutput`

``` purescript
newtype DescribeGatewayInformationOutput
  = DescribeGatewayInformationOutput { "GatewayARN" :: NullOrUndefined (GatewayARN), "GatewayId" :: NullOrUndefined (GatewayId), "GatewayName" :: NullOrUndefined (String), "GatewayTimezone" :: NullOrUndefined (GatewayTimezone), "GatewayState" :: NullOrUndefined (GatewayState), "GatewayNetworkInterfaces" :: NullOrUndefined (GatewayNetworkInterfaces), "GatewayType" :: NullOrUndefined (GatewayType), "NextUpdateAvailabilityDate" :: NullOrUndefined (NextUpdateAvailabilityDate), "LastSoftwareUpdate" :: NullOrUndefined (LastSoftwareUpdate) }
```

<p>A JSON object containing the following fields:</p>

#### `DescribeMaintenanceStartTimeInput`

``` purescript
newtype DescribeMaintenanceStartTimeInput
  = DescribeMaintenanceStartTimeInput { "GatewayARN" :: GatewayARN }
```

<p>A JSON object containing the of the gateway.</p>

#### `DescribeMaintenanceStartTimeOutput`

``` purescript
newtype DescribeMaintenanceStartTimeOutput
  = DescribeMaintenanceStartTimeOutput { "GatewayARN" :: NullOrUndefined (GatewayARN), "HourOfDay" :: NullOrUndefined (HourOfDay), "MinuteOfHour" :: NullOrUndefined (MinuteOfHour), "DayOfWeek" :: NullOrUndefined (DayOfWeek), "Timezone" :: NullOrUndefined (GatewayTimezone) }
```

<p>A JSON object containing the following fields:</p> <ul> <li> <p> <a>DescribeMaintenanceStartTimeOutput$DayOfWeek</a> </p> </li> <li> <p> <a>DescribeMaintenanceStartTimeOutput$HourOfDay</a> </p> </li> <li> <p> <a>DescribeMaintenanceStartTimeOutput$MinuteOfHour</a> </p> </li> <li> <p> <a>DescribeMaintenanceStartTimeOutput$Timezone</a> </p> </li> </ul>

#### `DescribeNFSFileSharesInput`

``` purescript
newtype DescribeNFSFileSharesInput
  = DescribeNFSFileSharesInput { "FileShareARNList" :: FileShareARNList }
```

<p>DescribeNFSFileSharesInput</p>

#### `DescribeNFSFileSharesOutput`

``` purescript
newtype DescribeNFSFileSharesOutput
  = DescribeNFSFileSharesOutput { "NFSFileShareInfoList" :: NullOrUndefined (NFSFileShareInfoList) }
```

<p>DescribeNFSFileSharesOutput</p>

#### `DescribeSnapshotScheduleInput`

``` purescript
newtype DescribeSnapshotScheduleInput
  = DescribeSnapshotScheduleInput { "VolumeARN" :: VolumeARN }
```

<p>A JSON object containing the <a>DescribeSnapshotScheduleInput$VolumeARN</a> of the volume.</p>

#### `DescribeSnapshotScheduleOutput`

``` purescript
newtype DescribeSnapshotScheduleOutput
  = DescribeSnapshotScheduleOutput { "VolumeARN" :: NullOrUndefined (VolumeARN), "StartAt" :: NullOrUndefined (HourOfDay), "RecurrenceInHours" :: NullOrUndefined (RecurrenceInHours), "Description" :: NullOrUndefined (Description), "Timezone" :: NullOrUndefined (GatewayTimezone) }
```

#### `DescribeStorediSCSIVolumesInput`

``` purescript
newtype DescribeStorediSCSIVolumesInput
  = DescribeStorediSCSIVolumesInput { "VolumeARNs" :: VolumeARNs }
```

<p>A JSON object containing a list of <a>DescribeStorediSCSIVolumesInput$VolumeARNs</a>.</p>

#### `DescribeStorediSCSIVolumesOutput`

``` purescript
newtype DescribeStorediSCSIVolumesOutput
  = DescribeStorediSCSIVolumesOutput { "StorediSCSIVolumes" :: NullOrUndefined (StorediSCSIVolumes) }
```

#### `DescribeTapeArchivesInput`

``` purescript
newtype DescribeTapeArchivesInput
  = DescribeTapeArchivesInput { "TapeARNs" :: NullOrUndefined (TapeARNs), "Marker" :: NullOrUndefined (Marker), "Limit" :: NullOrUndefined (PositiveIntObject) }
```

<p>DescribeTapeArchivesInput</p>

#### `DescribeTapeArchivesOutput`

``` purescript
newtype DescribeTapeArchivesOutput
  = DescribeTapeArchivesOutput { "TapeArchives" :: NullOrUndefined (TapeArchives), "Marker" :: NullOrUndefined (Marker) }
```

<p>DescribeTapeArchivesOutput</p>

#### `DescribeTapeRecoveryPointsInput`

``` purescript
newtype DescribeTapeRecoveryPointsInput
  = DescribeTapeRecoveryPointsInput { "GatewayARN" :: GatewayARN, "Marker" :: NullOrUndefined (Marker), "Limit" :: NullOrUndefined (PositiveIntObject) }
```

<p>DescribeTapeRecoveryPointsInput</p>

#### `DescribeTapeRecoveryPointsOutput`

``` purescript
newtype DescribeTapeRecoveryPointsOutput
  = DescribeTapeRecoveryPointsOutput { "GatewayARN" :: NullOrUndefined (GatewayARN), "TapeRecoveryPointInfos" :: NullOrUndefined (TapeRecoveryPointInfos), "Marker" :: NullOrUndefined (Marker) }
```

<p>DescribeTapeRecoveryPointsOutput</p>

#### `DescribeTapesInput`

``` purescript
newtype DescribeTapesInput
  = DescribeTapesInput { "GatewayARN" :: GatewayARN, "TapeARNs" :: NullOrUndefined (TapeARNs), "Marker" :: NullOrUndefined (Marker), "Limit" :: NullOrUndefined (PositiveIntObject) }
```

<p>DescribeTapesInput</p>

#### `DescribeTapesOutput`

``` purescript
newtype DescribeTapesOutput
  = DescribeTapesOutput { "Tapes" :: NullOrUndefined (Tapes), "Marker" :: NullOrUndefined (Marker) }
```

<p>DescribeTapesOutput</p>

#### `DescribeUploadBufferInput`

``` purescript
newtype DescribeUploadBufferInput
  = DescribeUploadBufferInput { "GatewayARN" :: GatewayARN }
```

#### `DescribeUploadBufferOutput`

``` purescript
newtype DescribeUploadBufferOutput
  = DescribeUploadBufferOutput { "GatewayARN" :: NullOrUndefined (GatewayARN), "DiskIds" :: NullOrUndefined (DiskIds), "UploadBufferUsedInBytes" :: NullOrUndefined (Number), "UploadBufferAllocatedInBytes" :: NullOrUndefined (Number) }
```

#### `DescribeVTLDevicesInput`

``` purescript
newtype DescribeVTLDevicesInput
  = DescribeVTLDevicesInput { "GatewayARN" :: GatewayARN, "VTLDeviceARNs" :: NullOrUndefined (VTLDeviceARNs), "Marker" :: NullOrUndefined (Marker), "Limit" :: NullOrUndefined (PositiveIntObject) }
```

<p>DescribeVTLDevicesInput</p>

#### `DescribeVTLDevicesOutput`

``` purescript
newtype DescribeVTLDevicesOutput
  = DescribeVTLDevicesOutput { "GatewayARN" :: NullOrUndefined (GatewayARN), "VTLDevices" :: NullOrUndefined (VTLDevices), "Marker" :: NullOrUndefined (Marker) }
```

<p>DescribeVTLDevicesOutput</p>

#### `DescribeWorkingStorageInput`

``` purescript
newtype DescribeWorkingStorageInput
  = DescribeWorkingStorageInput { "GatewayARN" :: GatewayARN }
```

<p>A JSON object containing the of the gateway.</p>

#### `DescribeWorkingStorageOutput`

``` purescript
newtype DescribeWorkingStorageOutput
  = DescribeWorkingStorageOutput { "GatewayARN" :: NullOrUndefined (GatewayARN), "DiskIds" :: NullOrUndefined (DiskIds), "WorkingStorageUsedInBytes" :: NullOrUndefined (Number), "WorkingStorageAllocatedInBytes" :: NullOrUndefined (Number) }
```

<p>A JSON object containing the following fields:</p>

#### `Description`

``` purescript
newtype Description
  = Description String
```

#### `DeviceType`

``` purescript
newtype DeviceType
  = DeviceType String
```

#### `DeviceiSCSIAttributes`

``` purescript
newtype DeviceiSCSIAttributes
  = DeviceiSCSIAttributes { "TargetARN" :: NullOrUndefined (TargetARN), "NetworkInterfaceId" :: NullOrUndefined (NetworkInterfaceId), "NetworkInterfacePort" :: NullOrUndefined (Int), "ChapEnabled" :: NullOrUndefined (Boolean) }
```

<p>Lists iSCSI information about a VTL device.</p>

#### `DisableGatewayInput`

``` purescript
newtype DisableGatewayInput
  = DisableGatewayInput { "GatewayARN" :: GatewayARN }
```

<p>DisableGatewayInput</p>

#### `DisableGatewayOutput`

``` purescript
newtype DisableGatewayOutput
  = DisableGatewayOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

<p>DisableGatewayOutput</p>

#### `Disk`

``` purescript
newtype Disk
  = Disk { "DiskId" :: NullOrUndefined (DiskId), "DiskPath" :: NullOrUndefined (String), "DiskNode" :: NullOrUndefined (String), "DiskStatus" :: NullOrUndefined (String), "DiskSizeInBytes" :: NullOrUndefined (Number), "DiskAllocationType" :: NullOrUndefined (DiskAllocationType), "DiskAllocationResource" :: NullOrUndefined (String) }
```

#### `DiskAllocationType`

``` purescript
newtype DiskAllocationType
  = DiskAllocationType String
```

#### `DiskId`

``` purescript
newtype DiskId
  = DiskId String
```

#### `DiskIds`

``` purescript
newtype DiskIds
  = DiskIds (Array DiskId)
```

#### `Disks`

``` purescript
newtype Disks
  = Disks (Array Disk)
```

#### `DoubleObject`

``` purescript
newtype DoubleObject
  = DoubleObject Number
```

#### `ErrorCode`

``` purescript
newtype ErrorCode
  = ErrorCode String
```

#### `FileShareARN`

``` purescript
newtype FileShareARN
  = FileShareARN String
```

<p>The Amazon Resource Name (ARN) of the file share. </p>

#### `FileShareARNList`

``` purescript
newtype FileShareARNList
  = FileShareARNList (Array FileShareARN)
```

#### `FileShareClientList`

``` purescript
newtype FileShareClientList
  = FileShareClientList (Array IPV4AddressCIDR)
```

<p>The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks. </p>

#### `FileShareId`

``` purescript
newtype FileShareId
  = FileShareId String
```

<p>The ID of the file share. </p>

#### `FileShareInfo`

``` purescript
newtype FileShareInfo
  = FileShareInfo { "FileShareARN" :: NullOrUndefined (FileShareARN), "FileShareId" :: NullOrUndefined (FileShareId), "FileShareStatus" :: NullOrUndefined (FileShareStatus), "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

<p>Describes a file share.</p>

#### `FileShareInfoList`

``` purescript
newtype FileShareInfoList
  = FileShareInfoList (Array FileShareInfo)
```

#### `FileShareStatus`

``` purescript
newtype FileShareStatus
  = FileShareStatus String
```

<p>The status of the file share. Possible values are CREATING, UPDATING, AVAILABLE and DELETING. </p>

#### `GatewayARN`

``` purescript
newtype GatewayARN
  = GatewayARN String
```

<p>The Amazon Resource Name (ARN) of the gateway. Use the <a>ListGateways</a> operation to return a list of gateways for your account and region.</p>

#### `GatewayId`

``` purescript
newtype GatewayId
  = GatewayId String
```

#### `GatewayInfo`

``` purescript
newtype GatewayInfo
  = GatewayInfo { "GatewayId" :: NullOrUndefined (GatewayId), "GatewayARN" :: NullOrUndefined (GatewayARN), "GatewayType" :: NullOrUndefined (GatewayType), "GatewayOperationalState" :: NullOrUndefined (GatewayOperationalState), "GatewayName" :: NullOrUndefined (String) }
```

<p>Describes a gateway object.</p>

#### `GatewayName`

``` purescript
newtype GatewayName
  = GatewayName String
```

<p>The name you configured for your gateway.</p>

#### `GatewayNetworkInterfaces`

``` purescript
newtype GatewayNetworkInterfaces
  = GatewayNetworkInterfaces (Array NetworkInterface)
```

#### `GatewayOperationalState`

``` purescript
newtype GatewayOperationalState
  = GatewayOperationalState String
```

#### `GatewayState`

``` purescript
newtype GatewayState
  = GatewayState String
```

#### `GatewayTimezone`

``` purescript
newtype GatewayTimezone
  = GatewayTimezone String
```

#### `GatewayType`

``` purescript
newtype GatewayType
  = GatewayType String
```

#### `Gateways`

``` purescript
newtype Gateways
  = Gateways (Array GatewayInfo)
```

#### `HourOfDay`

``` purescript
newtype HourOfDay
  = HourOfDay Int
```

#### `IPV4AddressCIDR`

``` purescript
newtype IPV4AddressCIDR
  = IPV4AddressCIDR String
```

#### `Initiator`

``` purescript
newtype Initiator
  = Initiator String
```

#### `Initiators`

``` purescript
newtype Initiators
  = Initiators (Array Initiator)
```

#### `InternalServerError`

``` purescript
newtype InternalServerError
  = InternalServerError { "Message'" :: NullOrUndefined (String), "Error'" :: NullOrUndefined (StorageGatewayError) }
```

<p>An internal server error has occurred during the request. For more information, see the error and message fields.</p>

#### `InvalidGatewayRequestException`

``` purescript
newtype InvalidGatewayRequestException
  = InvalidGatewayRequestException { "Message'" :: NullOrUndefined (String), "Error'" :: NullOrUndefined (StorageGatewayError) }
```

<p>An exception occurred because an invalid gateway request was issued to the service. For more information, see the error and message fields.</p>

#### `IqnName`

``` purescript
newtype IqnName
  = IqnName String
```

#### `KMSKey`

``` purescript
newtype KMSKey
  = KMSKey String
```

<p>The ARN of the KMS key used for Amazon S3 server side encryption. </p>

#### `LastSoftwareUpdate`

``` purescript
newtype LastSoftwareUpdate
  = LastSoftwareUpdate String
```

#### `ListFileSharesInput`

``` purescript
newtype ListFileSharesInput
  = ListFileSharesInput { "GatewayARN" :: NullOrUndefined (GatewayARN), "Limit" :: NullOrUndefined (PositiveIntObject), "Marker" :: NullOrUndefined (Marker) }
```

<p>ListFileShareInput</p>

#### `ListFileSharesOutput`

``` purescript
newtype ListFileSharesOutput
  = ListFileSharesOutput { "Marker" :: NullOrUndefined (Marker), "NextMarker" :: NullOrUndefined (Marker), "FileShareInfoList" :: NullOrUndefined (FileShareInfoList) }
```

<p>ListFileShareOutput</p>

#### `ListGatewaysInput`

``` purescript
newtype ListGatewaysInput
  = ListGatewaysInput { "Marker" :: NullOrUndefined (Marker), "Limit" :: NullOrUndefined (PositiveIntObject) }
```

<p>A JSON object containing zero or more of the following fields:</p> <ul> <li> <p> <a>ListGatewaysInput$Limit</a> </p> </li> <li> <p> <a>ListGatewaysInput$Marker</a> </p> </li> </ul>

#### `ListGatewaysOutput`

``` purescript
newtype ListGatewaysOutput
  = ListGatewaysOutput { "Gateways" :: NullOrUndefined (Gateways), "Marker" :: NullOrUndefined (Marker) }
```

#### `ListLocalDisksInput`

``` purescript
newtype ListLocalDisksInput
  = ListLocalDisksInput { "GatewayARN" :: GatewayARN }
```

<p>A JSON object containing the of the gateway.</p>

#### `ListLocalDisksOutput`

``` purescript
newtype ListLocalDisksOutput
  = ListLocalDisksOutput { "GatewayARN" :: NullOrUndefined (GatewayARN), "Disks" :: NullOrUndefined (Disks) }
```

#### `ListTagsForResourceInput`

``` purescript
newtype ListTagsForResourceInput
  = ListTagsForResourceInput { "ResourceARN" :: ResourceARN, "Marker" :: NullOrUndefined (Marker), "Limit" :: NullOrUndefined (PositiveIntObject) }
```

<p>ListTagsForResourceInput</p>

#### `ListTagsForResourceOutput`

``` purescript
newtype ListTagsForResourceOutput
  = ListTagsForResourceOutput { "ResourceARN" :: NullOrUndefined (ResourceARN), "Marker" :: NullOrUndefined (Marker), "Tags" :: NullOrUndefined (Tags) }
```

<p>ListTagsForResourceOutput</p>

#### `ListTapesInput`

``` purescript
newtype ListTapesInput
  = ListTapesInput { "TapeARNs" :: NullOrUndefined (TapeARNs), "Marker" :: NullOrUndefined (Marker), "Limit" :: NullOrUndefined (PositiveIntObject) }
```

<p>A JSON object that contains one or more of the following fields:</p> <ul> <li> <p> <a>ListTapesInput$Limit</a> </p> </li> <li> <p> <a>ListTapesInput$Marker</a> </p> </li> <li> <p> <a>ListTapesInput$TapeARNs</a> </p> </li> </ul>

#### `ListTapesOutput`

``` purescript
newtype ListTapesOutput
  = ListTapesOutput { "TapeInfos" :: NullOrUndefined (TapeInfos), "Marker" :: NullOrUndefined (Marker) }
```

<p>A JSON object containing the following fields:</p> <ul> <li> <p> <a>ListTapesOutput$Marker</a> </p> </li> <li> <p> <a>ListTapesOutput$VolumeInfos</a> </p> </li> </ul>

#### `ListVolumeInitiatorsInput`

``` purescript
newtype ListVolumeInitiatorsInput
  = ListVolumeInitiatorsInput { "VolumeARN" :: VolumeARN }
```

<p>ListVolumeInitiatorsInput</p>

#### `ListVolumeInitiatorsOutput`

``` purescript
newtype ListVolumeInitiatorsOutput
  = ListVolumeInitiatorsOutput { "Initiators" :: NullOrUndefined (Initiators) }
```

<p>ListVolumeInitiatorsOutput</p>

#### `ListVolumeRecoveryPointsInput`

``` purescript
newtype ListVolumeRecoveryPointsInput
  = ListVolumeRecoveryPointsInput { "GatewayARN" :: GatewayARN }
```

#### `ListVolumeRecoveryPointsOutput`

``` purescript
newtype ListVolumeRecoveryPointsOutput
  = ListVolumeRecoveryPointsOutput { "GatewayARN" :: NullOrUndefined (GatewayARN), "VolumeRecoveryPointInfos" :: NullOrUndefined (VolumeRecoveryPointInfos) }
```

#### `ListVolumesInput`

``` purescript
newtype ListVolumesInput
  = ListVolumesInput { "GatewayARN" :: NullOrUndefined (GatewayARN), "Marker" :: NullOrUndefined (Marker), "Limit" :: NullOrUndefined (PositiveIntObject) }
```

<p>A JSON object that contains one or more of the following fields:</p> <ul> <li> <p> <a>ListVolumesInput$Limit</a> </p> </li> <li> <p> <a>ListVolumesInput$Marker</a> </p> </li> </ul>

#### `ListVolumesOutput`

``` purescript
newtype ListVolumesOutput
  = ListVolumesOutput { "GatewayARN" :: NullOrUndefined (GatewayARN), "Marker" :: NullOrUndefined (Marker), "VolumeInfos" :: NullOrUndefined (VolumeInfos) }
```

#### `LocalConsolePassword`

``` purescript
newtype LocalConsolePassword
  = LocalConsolePassword String
```

#### `LocationARN`

``` purescript
newtype LocationARN
  = LocationARN String
```

<p>The ARN of the backend storage used for storing file data. </p>

#### `Marker`

``` purescript
newtype Marker
  = Marker String
```

#### `MediumChangerType`

``` purescript
newtype MediumChangerType
  = MediumChangerType String
```

#### `MinuteOfHour`

``` purescript
newtype MinuteOfHour
  = MinuteOfHour Int
```

#### `NFSFileShareDefaults`

``` purescript
newtype NFSFileShareDefaults
  = NFSFileShareDefaults { "FileMode" :: NullOrUndefined (PermissionMode), "DirectoryMode" :: NullOrUndefined (PermissionMode), "GroupId" :: NullOrUndefined (PermissionId), "OwnerId" :: NullOrUndefined (PermissionId) }
```

<p>Describes file share default values. Files and folders stored as Amazon S3 objects in S3 buckets don't, by default, have Unix file permissions assigned to them. Upon discovery in an S3 bucket by Storage Gateway, the S3 objects that represent files and folders are assigned these default Unix permissions. This operation is only supported in the file gateway type.</p>

#### `NFSFileShareInfo`

``` purescript
newtype NFSFileShareInfo
  = NFSFileShareInfo { "NFSFileShareDefaults" :: NullOrUndefined (NFSFileShareDefaults), "FileShareARN" :: NullOrUndefined (FileShareARN), "FileShareId" :: NullOrUndefined (FileShareId), "FileShareStatus" :: NullOrUndefined (FileShareStatus), "GatewayARN" :: NullOrUndefined (GatewayARN), "KMSEncrypted" :: NullOrUndefined (Boolean), "KMSKey" :: NullOrUndefined (KMSKey), "Path" :: NullOrUndefined (Path), "Role" :: NullOrUndefined (Role), "LocationARN" :: NullOrUndefined (LocationARN), "DefaultStorageClass" :: NullOrUndefined (StorageClass), "ClientList" :: NullOrUndefined (FileShareClientList), "Squash" :: NullOrUndefined (Squash), "ReadOnly" :: NullOrUndefined (Boolean), "GuessMIMETypeEnabled" :: NullOrUndefined (Boolean) }
```

<p>The Unix file permissions and ownership information assigned, by default, to native S3 objects when file gateway discovers them in S3 buckets. This operation is only supported in file gateways.</p>

#### `NFSFileShareInfoList`

``` purescript
newtype NFSFileShareInfoList
  = NFSFileShareInfoList (Array NFSFileShareInfo)
```

#### `NetworkInterface`

``` purescript
newtype NetworkInterface
  = NetworkInterface { "Ipv4Address" :: NullOrUndefined (String), "MacAddress" :: NullOrUndefined (String), "Ipv6Address" :: NullOrUndefined (String) }
```

<p>Describes a gateway's network interface.</p>

#### `NetworkInterfaceId`

``` purescript
newtype NetworkInterfaceId
  = NetworkInterfaceId String
```

#### `NextUpdateAvailabilityDate`

``` purescript
newtype NextUpdateAvailabilityDate
  = NextUpdateAvailabilityDate String
```

#### `NotificationId`

``` purescript
newtype NotificationId
  = NotificationId String
```

<p>The randomly generated ID of the notification that was sent. This ID is in UUID format.</p>

#### `NotifyWhenUploadedInput`

``` purescript
newtype NotifyWhenUploadedInput
  = NotifyWhenUploadedInput { "FileShareARN" :: FileShareARN }
```

#### `NotifyWhenUploadedOutput`

``` purescript
newtype NotifyWhenUploadedOutput
  = NotifyWhenUploadedOutput { "FileShareARN" :: NullOrUndefined (FileShareARN), "NotificationId" :: NullOrUndefined (NotificationId) }
```

#### `NumTapesToCreate`

``` purescript
newtype NumTapesToCreate
  = NumTapesToCreate Int
```

#### `Path`

``` purescript
newtype Path
  = Path String
```

<p>The file share path used by the NFS client to identify the mount point. </p>

#### `PermissionId`

``` purescript
newtype PermissionId
  = PermissionId Number
```

#### `PermissionMode`

``` purescript
newtype PermissionMode
  = PermissionMode String
```

#### `PositiveIntObject`

``` purescript
newtype PositiveIntObject
  = PositiveIntObject Int
```

#### `RecurrenceInHours`

``` purescript
newtype RecurrenceInHours
  = RecurrenceInHours Int
```

#### `RefreshCacheInput`

``` purescript
newtype RefreshCacheInput
  = RefreshCacheInput { "FileShareARN" :: FileShareARN }
```

#### `RefreshCacheOutput`

``` purescript
newtype RefreshCacheOutput
  = RefreshCacheOutput { "FileShareARN" :: NullOrUndefined (FileShareARN) }
```

#### `RegionId`

``` purescript
newtype RegionId
  = RegionId String
```

#### `RemoveTagsFromResourceInput`

``` purescript
newtype RemoveTagsFromResourceInput
  = RemoveTagsFromResourceInput { "ResourceARN" :: ResourceARN, "TagKeys" :: TagKeys }
```

<p>RemoveTagsFromResourceInput</p>

#### `RemoveTagsFromResourceOutput`

``` purescript
newtype RemoveTagsFromResourceOutput
  = RemoveTagsFromResourceOutput { "ResourceARN" :: NullOrUndefined (ResourceARN) }
```

<p>RemoveTagsFromResourceOutput</p>

#### `ResetCacheInput`

``` purescript
newtype ResetCacheInput
  = ResetCacheInput { "GatewayARN" :: GatewayARN }
```

#### `ResetCacheOutput`

``` purescript
newtype ResetCacheOutput
  = ResetCacheOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

#### `ResourceARN`

``` purescript
newtype ResourceARN
  = ResourceARN String
```

#### `RetrieveTapeArchiveInput`

``` purescript
newtype RetrieveTapeArchiveInput
  = RetrieveTapeArchiveInput { "TapeARN" :: TapeARN, "GatewayARN" :: GatewayARN }
```

<p>RetrieveTapeArchiveInput</p>

#### `RetrieveTapeArchiveOutput`

``` purescript
newtype RetrieveTapeArchiveOutput
  = RetrieveTapeArchiveOutput { "TapeARN" :: NullOrUndefined (TapeARN) }
```

<p>RetrieveTapeArchiveOutput</p>

#### `RetrieveTapeRecoveryPointInput`

``` purescript
newtype RetrieveTapeRecoveryPointInput
  = RetrieveTapeRecoveryPointInput { "TapeARN" :: TapeARN, "GatewayARN" :: GatewayARN }
```

<p>RetrieveTapeRecoveryPointInput</p>

#### `RetrieveTapeRecoveryPointOutput`

``` purescript
newtype RetrieveTapeRecoveryPointOutput
  = RetrieveTapeRecoveryPointOutput { "TapeARN" :: NullOrUndefined (TapeARN) }
```

<p>RetrieveTapeRecoveryPointOutput</p>

#### `Role`

``` purescript
newtype Role
  = Role String
```

<p>The ARN of the IAM role that file gateway assumes when it accesses the underlying storage. </p>

#### `ServiceUnavailableError`

``` purescript
newtype ServiceUnavailableError
  = ServiceUnavailableError { "Message'" :: NullOrUndefined (String), "Error'" :: NullOrUndefined (StorageGatewayError) }
```

<p>An internal server error has occurred because the service is unavailable. For more information, see the error and message fields.</p>

#### `SetLocalConsolePasswordInput`

``` purescript
newtype SetLocalConsolePasswordInput
  = SetLocalConsolePasswordInput { "GatewayARN" :: GatewayARN, "LocalConsolePassword" :: LocalConsolePassword }
```

<p>SetLocalConsolePasswordInput</p>

#### `SetLocalConsolePasswordOutput`

``` purescript
newtype SetLocalConsolePasswordOutput
  = SetLocalConsolePasswordOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

#### `ShutdownGatewayInput`

``` purescript
newtype ShutdownGatewayInput
  = ShutdownGatewayInput { "GatewayARN" :: GatewayARN }
```

<p>A JSON object containing the of the gateway to shut down.</p>

#### `ShutdownGatewayOutput`

``` purescript
newtype ShutdownGatewayOutput
  = ShutdownGatewayOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

<p>A JSON object containing the of the gateway that was shut down.</p>

#### `SnapshotDescription`

``` purescript
newtype SnapshotDescription
  = SnapshotDescription String
```

#### `SnapshotId`

``` purescript
newtype SnapshotId
  = SnapshotId String
```

#### `Squash`

``` purescript
newtype Squash
  = Squash String
```

<p>The user mapped to anonymous user. Valid options are the following: </p> <ul> <li> <p>"RootSquash" - Only root is mapped to anonymous user.</p> </li> <li> <p>"NoSquash" - No one is mapped to anonymous user</p> </li> <li> <p>"AllSquash" - Everyone is mapped to anonymous user.</p> </li> </ul>

#### `StartGatewayInput`

``` purescript
newtype StartGatewayInput
  = StartGatewayInput { "GatewayARN" :: GatewayARN }
```

<p>A JSON object containing the of the gateway to start.</p>

#### `StartGatewayOutput`

``` purescript
newtype StartGatewayOutput
  = StartGatewayOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

<p>A JSON object containing the of the gateway that was restarted.</p>

#### `StorageClass`

``` purescript
newtype StorageClass
  = StorageClass String
```

<p/>

#### `StorageGatewayError`

``` purescript
newtype StorageGatewayError
  = StorageGatewayError { "ErrorCode'" :: NullOrUndefined (ErrorCode), "ErrorDetails'" :: NullOrUndefined (ErrorDetails') }
```

<p>Provides additional information about an error that was returned by the service as an or. See the <code>errorCode</code> and <code>errorDetails</code> members for more information about the error.</p>

#### `StorediSCSIVolume`

``` purescript
newtype StorediSCSIVolume
  = StorediSCSIVolume { "VolumeARN" :: NullOrUndefined (VolumeARN), "VolumeId" :: NullOrUndefined (VolumeId), "VolumeType" :: NullOrUndefined (VolumeType), "VolumeStatus" :: NullOrUndefined (VolumeStatus), "VolumeSizeInBytes" :: NullOrUndefined (Number), "VolumeProgress" :: NullOrUndefined (DoubleObject), "VolumeDiskId" :: NullOrUndefined (DiskId), "SourceSnapshotId" :: NullOrUndefined (SnapshotId), "PreservedExistingData" :: NullOrUndefined (Boolean), "VolumeiSCSIAttributes" :: NullOrUndefined (VolumeiSCSIAttributes), "CreatedDate" :: NullOrUndefined (CreatedDate), "VolumeUsedInBytes" :: NullOrUndefined (VolumeUsedInBytes) }
```

<p>Describes an iSCSI stored volume.</p>

#### `StorediSCSIVolumes`

``` purescript
newtype StorediSCSIVolumes
  = StorediSCSIVolumes (Array StorediSCSIVolume)
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: TagValue }
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagKeys`

``` purescript
newtype TagKeys
  = TagKeys (Array TagKey)
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `Tags`

``` purescript
newtype Tags
  = Tags (Array Tag)
```

#### `Tape`

``` purescript
newtype Tape
  = Tape { "TapeARN" :: NullOrUndefined (TapeARN), "TapeBarcode" :: NullOrUndefined (TapeBarcode), "TapeCreatedDate" :: NullOrUndefined (Time), "TapeSizeInBytes" :: NullOrUndefined (TapeSize), "TapeStatus" :: NullOrUndefined (TapeStatus), "VTLDevice" :: NullOrUndefined (VTLDeviceARN), "Progress" :: NullOrUndefined (DoubleObject), "TapeUsedInBytes" :: NullOrUndefined (TapeUsage) }
```

<p>Describes a virtual tape object.</p>

#### `TapeARN`

``` purescript
newtype TapeARN
  = TapeARN String
```

#### `TapeARNs`

``` purescript
newtype TapeARNs
  = TapeARNs (Array TapeARN)
```

<p>The Amazon Resource Name (ARN) of each of the tapes you want to list. If you don't specify a tape ARN, the response lists all tapes in both your VTL and VTS.</p>

#### `TapeArchive`

``` purescript
newtype TapeArchive
  = TapeArchive { "TapeARN" :: NullOrUndefined (TapeARN), "TapeBarcode" :: NullOrUndefined (TapeBarcode), "TapeCreatedDate" :: NullOrUndefined (Time), "TapeSizeInBytes" :: NullOrUndefined (TapeSize), "CompletionTime" :: NullOrUndefined (Time), "RetrievedTo" :: NullOrUndefined (GatewayARN), "TapeStatus" :: NullOrUndefined (TapeArchiveStatus), "TapeUsedInBytes" :: NullOrUndefined (TapeUsage) }
```

<p>Represents a virtual tape that is archived in the virtual tape shelf (VTS).</p>

#### `TapeArchiveStatus`

``` purescript
newtype TapeArchiveStatus
  = TapeArchiveStatus String
```

#### `TapeArchives`

``` purescript
newtype TapeArchives
  = TapeArchives (Array TapeArchive)
```

#### `TapeBarcode`

``` purescript
newtype TapeBarcode
  = TapeBarcode String
```

#### `TapeBarcodePrefix`

``` purescript
newtype TapeBarcodePrefix
  = TapeBarcodePrefix String
```

#### `TapeDriveType`

``` purescript
newtype TapeDriveType
  = TapeDriveType String
```

#### `TapeInfo`

``` purescript
newtype TapeInfo
  = TapeInfo { "TapeARN" :: NullOrUndefined (TapeARN), "TapeBarcode" :: NullOrUndefined (TapeBarcode), "TapeSizeInBytes" :: NullOrUndefined (TapeSize), "TapeStatus" :: NullOrUndefined (TapeStatus), "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

<p>Describes a virtual tape.</p>

#### `TapeInfos`

``` purescript
newtype TapeInfos
  = TapeInfos (Array TapeInfo)
```

<p>An array of <a>TapeInfo</a> objects, where each object describes an a single tape. If there not tapes in the tape library or VTS, then the <code>TapeInfos</code> is an empty array.</p>

#### `TapeRecoveryPointInfo`

``` purescript
newtype TapeRecoveryPointInfo
  = TapeRecoveryPointInfo { "TapeARN" :: NullOrUndefined (TapeARN), "TapeRecoveryPointTime" :: NullOrUndefined (Time), "TapeSizeInBytes" :: NullOrUndefined (TapeSize), "TapeStatus" :: NullOrUndefined (TapeRecoveryPointStatus) }
```

<p>Describes a recovery point.</p>

#### `TapeRecoveryPointInfos`

``` purescript
newtype TapeRecoveryPointInfos
  = TapeRecoveryPointInfos (Array TapeRecoveryPointInfo)
```

#### `TapeRecoveryPointStatus`

``` purescript
newtype TapeRecoveryPointStatus
  = TapeRecoveryPointStatus String
```

#### `TapeSize`

``` purescript
newtype TapeSize
  = TapeSize Number
```

#### `TapeStatus`

``` purescript
newtype TapeStatus
  = TapeStatus String
```

#### `TapeUsage`

``` purescript
newtype TapeUsage
  = TapeUsage Number
```

#### `Tapes`

``` purescript
newtype Tapes
  = Tapes (Array Tape)
```

#### `TargetARN`

``` purescript
newtype TargetARN
  = TargetARN String
```

#### `TargetName`

``` purescript
newtype TargetName
  = TargetName String
```

#### `Time`

``` purescript
newtype Time
  = Time Number
```

#### `UpdateBandwidthRateLimitInput`

``` purescript
newtype UpdateBandwidthRateLimitInput
  = UpdateBandwidthRateLimitInput { "GatewayARN" :: GatewayARN, "AverageUploadRateLimitInBitsPerSec" :: NullOrUndefined (BandwidthUploadRateLimit), "AverageDownloadRateLimitInBitsPerSec" :: NullOrUndefined (BandwidthDownloadRateLimit) }
```

<p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>UpdateBandwidthRateLimitInput$AverageDownloadRateLimitInBitsPerSec</a> </p> </li> <li> <p> <a>UpdateBandwidthRateLimitInput$AverageUploadRateLimitInBitsPerSec</a> </p> </li> </ul>

#### `UpdateBandwidthRateLimitOutput`

``` purescript
newtype UpdateBandwidthRateLimitOutput
  = UpdateBandwidthRateLimitOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

<p>A JSON object containing the of the gateway whose throttle information was updated.</p>

#### `UpdateChapCredentialsInput`

``` purescript
newtype UpdateChapCredentialsInput
  = UpdateChapCredentialsInput { "TargetARN" :: TargetARN, "SecretToAuthenticateInitiator" :: ChapSecret, "InitiatorName" :: IqnName, "SecretToAuthenticateTarget" :: NullOrUndefined (ChapSecret) }
```

<p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>UpdateChapCredentialsInput$InitiatorName</a> </p> </li> <li> <p> <a>UpdateChapCredentialsInput$SecretToAuthenticateInitiator</a> </p> </li> <li> <p> <a>UpdateChapCredentialsInput$SecretToAuthenticateTarget</a> </p> </li> <li> <p> <a>UpdateChapCredentialsInput$TargetARN</a> </p> </li> </ul>

#### `UpdateChapCredentialsOutput`

``` purescript
newtype UpdateChapCredentialsOutput
  = UpdateChapCredentialsOutput { "TargetARN" :: NullOrUndefined (TargetARN), "InitiatorName" :: NullOrUndefined (IqnName) }
```

<p>A JSON object containing the following fields:</p>

#### `UpdateGatewayInformationInput`

``` purescript
newtype UpdateGatewayInformationInput
  = UpdateGatewayInformationInput { "GatewayARN" :: GatewayARN, "GatewayName" :: NullOrUndefined (GatewayName), "GatewayTimezone" :: NullOrUndefined (GatewayTimezone) }
```

#### `UpdateGatewayInformationOutput`

``` purescript
newtype UpdateGatewayInformationOutput
  = UpdateGatewayInformationOutput { "GatewayARN" :: NullOrUndefined (GatewayARN), "GatewayName" :: NullOrUndefined (String) }
```

<p>A JSON object containing the ARN of the gateway that was updated.</p>

#### `UpdateGatewaySoftwareNowInput`

``` purescript
newtype UpdateGatewaySoftwareNowInput
  = UpdateGatewaySoftwareNowInput { "GatewayARN" :: GatewayARN }
```

<p>A JSON object containing the of the gateway to update.</p>

#### `UpdateGatewaySoftwareNowOutput`

``` purescript
newtype UpdateGatewaySoftwareNowOutput
  = UpdateGatewaySoftwareNowOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

<p>A JSON object containing the of the gateway that was updated.</p>

#### `UpdateMaintenanceStartTimeInput`

``` purescript
newtype UpdateMaintenanceStartTimeInput
  = UpdateMaintenanceStartTimeInput { "GatewayARN" :: GatewayARN, "HourOfDay" :: HourOfDay, "MinuteOfHour" :: MinuteOfHour, "DayOfWeek" :: DayOfWeek }
```

<p>A JSON object containing the following fields:</p> <ul> <li> <p> <a>UpdateMaintenanceStartTimeInput$DayOfWeek</a> </p> </li> <li> <p> <a>UpdateMaintenanceStartTimeInput$HourOfDay</a> </p> </li> <li> <p> <a>UpdateMaintenanceStartTimeInput$MinuteOfHour</a> </p> </li> </ul>

#### `UpdateMaintenanceStartTimeOutput`

``` purescript
newtype UpdateMaintenanceStartTimeOutput
  = UpdateMaintenanceStartTimeOutput { "GatewayARN" :: NullOrUndefined (GatewayARN) }
```

<p>A JSON object containing the of the gateway whose maintenance start time is updated.</p>

#### `UpdateNFSFileShareInput`

``` purescript
newtype UpdateNFSFileShareInput
  = UpdateNFSFileShareInput { "FileShareARN" :: FileShareARN, "KMSEncrypted" :: NullOrUndefined (Boolean), "KMSKey" :: NullOrUndefined (KMSKey), "NFSFileShareDefaults" :: NullOrUndefined (NFSFileShareDefaults), "DefaultStorageClass" :: NullOrUndefined (StorageClass), "ClientList" :: NullOrUndefined (FileShareClientList), "Squash" :: NullOrUndefined (Squash), "ReadOnly" :: NullOrUndefined (Boolean), "GuessMIMETypeEnabled" :: NullOrUndefined (Boolean) }
```

<p>UpdateNFSFileShareInput</p>

#### `UpdateNFSFileShareOutput`

``` purescript
newtype UpdateNFSFileShareOutput
  = UpdateNFSFileShareOutput { "FileShareARN" :: NullOrUndefined (FileShareARN) }
```

<p>UpdateNFSFileShareOutput</p>

#### `UpdateSnapshotScheduleInput`

``` purescript
newtype UpdateSnapshotScheduleInput
  = UpdateSnapshotScheduleInput { "VolumeARN" :: VolumeARN, "StartAt" :: HourOfDay, "RecurrenceInHours" :: RecurrenceInHours, "Description" :: NullOrUndefined (Description) }
```

<p>A JSON object containing one or more of the following fields:</p> <ul> <li> <p> <a>UpdateSnapshotScheduleInput$Description</a> </p> </li> <li> <p> <a>UpdateSnapshotScheduleInput$RecurrenceInHours</a> </p> </li> <li> <p> <a>UpdateSnapshotScheduleInput$StartAt</a> </p> </li> <li> <p> <a>UpdateSnapshotScheduleInput$VolumeARN</a> </p> </li> </ul>

#### `UpdateSnapshotScheduleOutput`

``` purescript
newtype UpdateSnapshotScheduleOutput
  = UpdateSnapshotScheduleOutput { "VolumeARN" :: NullOrUndefined (VolumeARN) }
```

<p>A JSON object containing the of the updated storage volume.</p>

#### `UpdateVTLDeviceTypeInput`

``` purescript
newtype UpdateVTLDeviceTypeInput
  = UpdateVTLDeviceTypeInput { "VTLDeviceARN" :: VTLDeviceARN, "DeviceType" :: DeviceType }
```

#### `UpdateVTLDeviceTypeOutput`

``` purescript
newtype UpdateVTLDeviceTypeOutput
  = UpdateVTLDeviceTypeOutput { "VTLDeviceARN" :: NullOrUndefined (VTLDeviceARN) }
```

<p>UpdateVTLDeviceTypeOutput</p>

#### `VTLDevice`

``` purescript
newtype VTLDevice
  = VTLDevice { "VTLDeviceARN" :: NullOrUndefined (VTLDeviceARN), "VTLDeviceType" :: NullOrUndefined (VTLDeviceType), "VTLDeviceVendor" :: NullOrUndefined (VTLDeviceVendor), "VTLDeviceProductIdentifier" :: NullOrUndefined (VTLDeviceProductIdentifier), "DeviceiSCSIAttributes" :: NullOrUndefined (DeviceiSCSIAttributes) }
```

<p>Represents a device object associated with a tape gateway.</p>

#### `VTLDeviceARN`

``` purescript
newtype VTLDeviceARN
  = VTLDeviceARN String
```

#### `VTLDeviceARNs`

``` purescript
newtype VTLDeviceARNs
  = VTLDeviceARNs (Array VTLDeviceARN)
```

#### `VTLDeviceProductIdentifier`

``` purescript
newtype VTLDeviceProductIdentifier
  = VTLDeviceProductIdentifier String
```

#### `VTLDeviceType`

``` purescript
newtype VTLDeviceType
  = VTLDeviceType String
```

#### `VTLDeviceVendor`

``` purescript
newtype VTLDeviceVendor
  = VTLDeviceVendor String
```

#### `VTLDevices`

``` purescript
newtype VTLDevices
  = VTLDevices (Array VTLDevice)
```

#### `VolumeARN`

``` purescript
newtype VolumeARN
  = VolumeARN String
```

#### `VolumeARNs`

``` purescript
newtype VolumeARNs
  = VolumeARNs (Array VolumeARN)
```

#### `VolumeId`

``` purescript
newtype VolumeId
  = VolumeId String
```

#### `VolumeInfo`

``` purescript
newtype VolumeInfo
  = VolumeInfo { "VolumeARN" :: NullOrUndefined (VolumeARN), "VolumeId" :: NullOrUndefined (VolumeId), "GatewayARN" :: NullOrUndefined (GatewayARN), "GatewayId" :: NullOrUndefined (GatewayId), "VolumeType" :: NullOrUndefined (VolumeType), "VolumeSizeInBytes" :: NullOrUndefined (Number) }
```

<p>Describes a storage volume object.</p>

#### `VolumeInfos`

``` purescript
newtype VolumeInfos
  = VolumeInfos (Array VolumeInfo)
```

#### `VolumeRecoveryPointInfo`

``` purescript
newtype VolumeRecoveryPointInfo
  = VolumeRecoveryPointInfo { "VolumeARN" :: NullOrUndefined (VolumeARN), "VolumeSizeInBytes" :: NullOrUndefined (Number), "VolumeUsageInBytes" :: NullOrUndefined (Number), "VolumeRecoveryPointTime" :: NullOrUndefined (String) }
```

#### `VolumeRecoveryPointInfos`

``` purescript
newtype VolumeRecoveryPointInfos
  = VolumeRecoveryPointInfos (Array VolumeRecoveryPointInfo)
```

#### `VolumeStatus`

``` purescript
newtype VolumeStatus
  = VolumeStatus String
```

#### `VolumeType`

``` purescript
newtype VolumeType
  = VolumeType String
```

#### `VolumeUsedInBytes`

``` purescript
newtype VolumeUsedInBytes
  = VolumeUsedInBytes Number
```

#### `VolumeiSCSIAttributes`

``` purescript
newtype VolumeiSCSIAttributes
  = VolumeiSCSIAttributes { "TargetARN" :: NullOrUndefined (TargetARN), "NetworkInterfaceId" :: NullOrUndefined (NetworkInterfaceId), "NetworkInterfacePort" :: NullOrUndefined (Int), "LunNumber" :: NullOrUndefined (PositiveIntObject), "ChapEnabled" :: NullOrUndefined (Boolean) }
```

<p>Lists iSCSI information about a volume.</p>

#### `ErrorDetails'`

``` purescript
newtype ErrorDetails'
  = ErrorDetails' (Map String String)
```


