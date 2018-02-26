## Module AWS.Lightsail

<p>Amazon Lightsail is the easiest way to get started with AWS for developers who just need virtual private servers. Lightsail includes everything you need to launch your project quickly - a virtual machine, SSD-based storage, data transfer, DNS management, and a static IP - for a low, predictable price. You manage those Lightsail servers through the Lightsail console or by using the API or command-line interface (CLI).</p> <p>For more information about Lightsail concepts and tasks, see the <a href="https://lightsail.aws.amazon.com/ls/docs/all">Lightsail Dev Guide</a>.</p> <p>To use the Lightsail API or the CLI, you will need to use AWS Identity and Access Management (IAM) to generate access keys. For details about how to set this up, see the <a href="http://lightsail.aws.amazon.com/ls/docs/how-to/article/lightsail-how-to-set-up-access-keys-to-use-sdk-api-cli">Lightsail Dev Guide</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `allocateStaticIp`

``` purescript
allocateStaticIp :: forall eff. AllocateStaticIpRequest -> Aff (err :: RequestError | eff) AllocateStaticIpResult
```

<p>Allocates a static IP address.</p>

#### `attachDisk`

``` purescript
attachDisk :: forall eff. AttachDiskRequest -> Aff (err :: RequestError | eff) AttachDiskResult
```

<p>Attaches a block storage disk to a running or stopped Lightsail instance and exposes it to the instance with the specified disk name.</p>

#### `attachInstancesToLoadBalancer`

``` purescript
attachInstancesToLoadBalancer :: forall eff. AttachInstancesToLoadBalancerRequest -> Aff (err :: RequestError | eff) AttachInstancesToLoadBalancerResult
```

<p>Attaches one or more Lightsail instances to a load balancer.</p>

#### `attachLoadBalancerTlsCertificate`

``` purescript
attachLoadBalancerTlsCertificate :: forall eff. AttachLoadBalancerTlsCertificateRequest -> Aff (err :: RequestError | eff) AttachLoadBalancerTlsCertificateResult
```

<p>Attaches a Transport Layer Security (TLS) certificate to your load balancer.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>

#### `attachStaticIp`

``` purescript
attachStaticIp :: forall eff. AttachStaticIpRequest -> Aff (err :: RequestError | eff) AttachStaticIpResult
```

<p>Attaches a static IP address to a specific Amazon Lightsail instance.</p>

#### `closeInstancePublicPorts`

``` purescript
closeInstancePublicPorts :: forall eff. CloseInstancePublicPortsRequest -> Aff (err :: RequestError | eff) CloseInstancePublicPortsResult
```

<p>Closes the public ports on a specific Amazon Lightsail instance.</p>

#### `createDisk`

``` purescript
createDisk :: forall eff. CreateDiskRequest -> Aff (err :: RequestError | eff) CreateDiskResult
```

<p>Creates a block storage disk that can be attached to a Lightsail instance in the same Availability Zone (e.g., <code>us-east-2a</code>). The disk is created in the regional endpoint that you send the HTTP request to. For more information, see <a href="https://lightsail.aws.amazon.com/ls/docs/overview/article/understanding-regions-and-availability-zones-in-amazon-lightsail">Regions and Availability Zones in Lightsail</a>.</p>

#### `createDiskFromSnapshot`

``` purescript
createDiskFromSnapshot :: forall eff. CreateDiskFromSnapshotRequest -> Aff (err :: RequestError | eff) CreateDiskFromSnapshotResult
```

<p>Creates a block storage disk from a disk snapshot that can be attached to a Lightsail instance in the same Availability Zone (e.g., <code>us-east-2a</code>). The disk is created in the regional endpoint that you send the HTTP request to. For more information, see <a href="https://lightsail.aws.amazon.com/ls/docs/overview/article/understanding-regions-and-availability-zones-in-amazon-lightsail">Regions and Availability Zones in Lightsail</a>.</p>

#### `createDiskSnapshot`

``` purescript
createDiskSnapshot :: forall eff. CreateDiskSnapshotRequest -> Aff (err :: RequestError | eff) CreateDiskSnapshotResult
```

<p>Creates a snapshot of a block storage disk. You can use snapshots for backups, to make copies of disks, and to save data before shutting down a Lightsail instance.</p> <p>You can take a snapshot of an attached disk that is in use; however, snapshots only capture data that has been written to your disk at the time the snapshot command is issued. This may exclude any data that has been cached by any applications or the operating system. If you can pause any file systems on the disk long enough to take a snapshot, your snapshot should be complete. Nevertheless, if you cannot pause all file writes to the disk, you should unmount the disk from within the Lightsail instance, issue the create disk snapshot command, and then remount the disk to ensure a consistent and complete snapshot. You may remount and use your disk while the snapshot status is pending.</p>

#### `createDomain`

``` purescript
createDomain :: forall eff. CreateDomainRequest -> Aff (err :: RequestError | eff) CreateDomainResult
```

<p>Creates a domain resource for the specified domain (e.g., example.com).</p>

#### `createDomainEntry`

``` purescript
createDomainEntry :: forall eff. CreateDomainEntryRequest -> Aff (err :: RequestError | eff) CreateDomainEntryResult
```

<p>Creates one of the following entry records associated with the domain: A record, CNAME record, TXT record, or MX record.</p>

#### `createInstanceSnapshot`

``` purescript
createInstanceSnapshot :: forall eff. CreateInstanceSnapshotRequest -> Aff (err :: RequestError | eff) CreateInstanceSnapshotResult
```

<p>Creates a snapshot of a specific virtual private server, or <i>instance</i>. You can use a snapshot to create a new instance that is based on that snapshot.</p>

#### `createInstances`

``` purescript
createInstances :: forall eff. CreateInstancesRequest -> Aff (err :: RequestError | eff) CreateInstancesResult
```

<p>Creates one or more Amazon Lightsail virtual private servers, or <i>instances</i>.</p>

#### `createInstancesFromSnapshot`

``` purescript
createInstancesFromSnapshot :: forall eff. CreateInstancesFromSnapshotRequest -> Aff (err :: RequestError | eff) CreateInstancesFromSnapshotResult
```

<p>Uses a specific snapshot as a blueprint for creating one or more new instances that are based on that identical configuration.</p>

#### `createKeyPair`

``` purescript
createKeyPair :: forall eff. CreateKeyPairRequest -> Aff (err :: RequestError | eff) CreateKeyPairResult
```

<p>Creates sn SSH key pair.</p>

#### `createLoadBalancer`

``` purescript
createLoadBalancer :: forall eff. CreateLoadBalancerRequest -> Aff (err :: RequestError | eff) CreateLoadBalancerResult
```

<p>Creates a Lightsail load balancer.</p> <p>When you create a load balancer, you can specify certificates and port settings. You can create up to 5 load balancers per AWS Region in your account.</p>

#### `createLoadBalancerTlsCertificate`

``` purescript
createLoadBalancerTlsCertificate :: forall eff. CreateLoadBalancerTlsCertificateRequest -> Aff (err :: RequestError | eff) CreateLoadBalancerTlsCertificateResult
```

<p>Creates a Lightsail load balancer TLS certificate.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>

#### `deleteDisk`

``` purescript
deleteDisk :: forall eff. DeleteDiskRequest -> Aff (err :: RequestError | eff) DeleteDiskResult
```

<p>Deletes the specified block storage disk. The disk must be in the <code>available</code> state (not attached to a Lightsail instance).</p> <note> <p>The disk may remain in the <code>deleting</code> state for several minutes.</p> </note>

#### `deleteDiskSnapshot`

``` purescript
deleteDiskSnapshot :: forall eff. DeleteDiskSnapshotRequest -> Aff (err :: RequestError | eff) DeleteDiskSnapshotResult
```

<p>Deletes the specified disk snapshot.</p> <p>When you make periodic snapshots of a disk, the snapshots are incremental, and only the blocks on the device that have changed since your last snapshot are saved in the new snapshot. When you delete a snapshot, only the data not needed for any other snapshot is removed. So regardless of which prior snapshots have been deleted, all active snapshots will have access to all the information needed to restore the disk.</p>

#### `deleteDomain`

``` purescript
deleteDomain :: forall eff. DeleteDomainRequest -> Aff (err :: RequestError | eff) DeleteDomainResult
```

<p>Deletes the specified domain recordset and all of its domain records.</p>

#### `deleteDomainEntry`

``` purescript
deleteDomainEntry :: forall eff. DeleteDomainEntryRequest -> Aff (err :: RequestError | eff) DeleteDomainEntryResult
```

<p>Deletes a specific domain entry.</p>

#### `deleteInstance`

``` purescript
deleteInstance :: forall eff. DeleteInstanceRequest -> Aff (err :: RequestError | eff) DeleteInstanceResult
```

<p>Deletes a specific Amazon Lightsail virtual private server, or <i>instance</i>.</p>

#### `deleteInstanceSnapshot`

``` purescript
deleteInstanceSnapshot :: forall eff. DeleteInstanceSnapshotRequest -> Aff (err :: RequestError | eff) DeleteInstanceSnapshotResult
```

<p>Deletes a specific snapshot of a virtual private server (or <i>instance</i>).</p>

#### `deleteKeyPair`

``` purescript
deleteKeyPair :: forall eff. DeleteKeyPairRequest -> Aff (err :: RequestError | eff) DeleteKeyPairResult
```

<p>Deletes a specific SSH key pair.</p>

#### `deleteLoadBalancer`

``` purescript
deleteLoadBalancer :: forall eff. DeleteLoadBalancerRequest -> Aff (err :: RequestError | eff) DeleteLoadBalancerResult
```

<p>Deletes a Lightsail load balancer.</p>

#### `deleteLoadBalancerTlsCertificate`

``` purescript
deleteLoadBalancerTlsCertificate :: forall eff. DeleteLoadBalancerTlsCertificateRequest -> Aff (err :: RequestError | eff) DeleteLoadBalancerTlsCertificateResult
```

<p>Deletes a TLS/SSL certificate associated with a Lightsail load balancer.</p>

#### `detachDisk`

``` purescript
detachDisk :: forall eff. DetachDiskRequest -> Aff (err :: RequestError | eff) DetachDiskResult
```

<p>Detaches a stopped block storage disk from a Lightsail instance. Make sure to unmount any file systems on the device within your operating system before stopping the instance and detaching the disk.</p>

#### `detachInstancesFromLoadBalancer`

``` purescript
detachInstancesFromLoadBalancer :: forall eff. DetachInstancesFromLoadBalancerRequest -> Aff (err :: RequestError | eff) DetachInstancesFromLoadBalancerResult
```

<p>Detaches the specified instances from a Lightsail load balancer.</p>

#### `detachStaticIp`

``` purescript
detachStaticIp :: forall eff. DetachStaticIpRequest -> Aff (err :: RequestError | eff) DetachStaticIpResult
```

<p>Detaches a static IP from the Amazon Lightsail instance to which it is attached.</p>

#### `downloadDefaultKeyPair`

``` purescript
downloadDefaultKeyPair :: forall eff. DownloadDefaultKeyPairRequest -> Aff (err :: RequestError | eff) DownloadDefaultKeyPairResult
```

<p>Downloads the default SSH key pair from the user's account.</p>

#### `getActiveNames`

``` purescript
getActiveNames :: forall eff. GetActiveNamesRequest -> Aff (err :: RequestError | eff) GetActiveNamesResult
```

<p>Returns the names of all active (not deleted) resources.</p>

#### `getBlueprints`

``` purescript
getBlueprints :: forall eff. GetBlueprintsRequest -> Aff (err :: RequestError | eff) GetBlueprintsResult
```

<p>Returns the list of available instance images, or <i>blueprints</i>. You can use a blueprint to create a new virtual private server already running a specific operating system, as well as a preinstalled app or development stack. The software each instance is running depends on the blueprint image you choose.</p>

#### `getBundles`

``` purescript
getBundles :: forall eff. GetBundlesRequest -> Aff (err :: RequestError | eff) GetBundlesResult
```

<p>Returns the list of bundles that are available for purchase. A bundle describes the specs for your virtual private server (or <i>instance</i>).</p>

#### `getDisk`

``` purescript
getDisk :: forall eff. GetDiskRequest -> Aff (err :: RequestError | eff) GetDiskResult
```

<p>Returns information about a specific block storage disk.</p>

#### `getDiskSnapshot`

``` purescript
getDiskSnapshot :: forall eff. GetDiskSnapshotRequest -> Aff (err :: RequestError | eff) GetDiskSnapshotResult
```

<p>Returns information about a specific block storage disk snapshot.</p>

#### `getDiskSnapshots`

``` purescript
getDiskSnapshots :: forall eff. GetDiskSnapshotsRequest -> Aff (err :: RequestError | eff) GetDiskSnapshotsResult
```

<p>Returns information about all block storage disk snapshots in your AWS account and region.</p> <p>If you are describing a long list of disk snapshots, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.</p>

#### `getDisks`

``` purescript
getDisks :: forall eff. GetDisksRequest -> Aff (err :: RequestError | eff) GetDisksResult
```

<p>Returns information about all block storage disks in your AWS account and region.</p> <p>If you are describing a long list of disks, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.</p>

#### `getDomain`

``` purescript
getDomain :: forall eff. GetDomainRequest -> Aff (err :: RequestError | eff) GetDomainResult
```

<p>Returns information about a specific domain recordset.</p>

#### `getDomains`

``` purescript
getDomains :: forall eff. GetDomainsRequest -> Aff (err :: RequestError | eff) GetDomainsResult
```

<p>Returns a list of all domains in the user's account.</p>

#### `getInstance`

``` purescript
getInstance :: forall eff. GetInstanceRequest -> Aff (err :: RequestError | eff) GetInstanceResult
```

<p>Returns information about a specific Amazon Lightsail instance, which is a virtual private server.</p>

#### `getInstanceAccessDetails`

``` purescript
getInstanceAccessDetails :: forall eff. GetInstanceAccessDetailsRequest -> Aff (err :: RequestError | eff) GetInstanceAccessDetailsResult
```

<p>Returns temporary SSH keys you can use to connect to a specific virtual private server, or <i>instance</i>.</p>

#### `getInstanceMetricData`

``` purescript
getInstanceMetricData :: forall eff. GetInstanceMetricDataRequest -> Aff (err :: RequestError | eff) GetInstanceMetricDataResult
```

<p>Returns the data points for the specified Amazon Lightsail instance metric, given an instance name.</p>

#### `getInstancePortStates`

``` purescript
getInstancePortStates :: forall eff. GetInstancePortStatesRequest -> Aff (err :: RequestError | eff) GetInstancePortStatesResult
```

<p>Returns the port states for a specific virtual private server, or <i>instance</i>.</p>

#### `getInstanceSnapshot`

``` purescript
getInstanceSnapshot :: forall eff. GetInstanceSnapshotRequest -> Aff (err :: RequestError | eff) GetInstanceSnapshotResult
```

<p>Returns information about a specific instance snapshot.</p>

#### `getInstanceSnapshots`

``` purescript
getInstanceSnapshots :: forall eff. GetInstanceSnapshotsRequest -> Aff (err :: RequestError | eff) GetInstanceSnapshotsResult
```

<p>Returns all instance snapshots for the user's account.</p>

#### `getInstanceState`

``` purescript
getInstanceState :: forall eff. GetInstanceStateRequest -> Aff (err :: RequestError | eff) GetInstanceStateResult
```

<p>Returns the state of a specific instance. Works on one instance at a time.</p>

#### `getInstances`

``` purescript
getInstances :: forall eff. GetInstancesRequest -> Aff (err :: RequestError | eff) GetInstancesResult
```

<p>Returns information about all Amazon Lightsail virtual private servers, or <i>instances</i>.</p>

#### `getKeyPair`

``` purescript
getKeyPair :: forall eff. GetKeyPairRequest -> Aff (err :: RequestError | eff) GetKeyPairResult
```

<p>Returns information about a specific key pair.</p>

#### `getKeyPairs`

``` purescript
getKeyPairs :: forall eff. GetKeyPairsRequest -> Aff (err :: RequestError | eff) GetKeyPairsResult
```

<p>Returns information about all key pairs in the user's account.</p>

#### `getLoadBalancer`

``` purescript
getLoadBalancer :: forall eff. GetLoadBalancerRequest -> Aff (err :: RequestError | eff) GetLoadBalancerResult
```

<p>Returns information about the specified Lightsail load balancer.</p>

#### `getLoadBalancerMetricData`

``` purescript
getLoadBalancerMetricData :: forall eff. GetLoadBalancerMetricDataRequest -> Aff (err :: RequestError | eff) GetLoadBalancerMetricDataResult
```

<p>Returns information about health metrics for your Lightsail load balancer.</p>

#### `getLoadBalancerTlsCertificates`

``` purescript
getLoadBalancerTlsCertificates :: forall eff. GetLoadBalancerTlsCertificatesRequest -> Aff (err :: RequestError | eff) GetLoadBalancerTlsCertificatesResult
```

<p>Returns information about the TLS certificates that are associated with the specified Lightsail load balancer.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>

#### `getLoadBalancers`

``` purescript
getLoadBalancers :: forall eff. GetLoadBalancersRequest -> Aff (err :: RequestError | eff) GetLoadBalancersResult
```

<p>Returns information about all load balancers in an account.</p> <p>If you are describing a long list of load balancers, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.</p>

#### `getOperation`

``` purescript
getOperation :: forall eff. GetOperationRequest -> Aff (err :: RequestError | eff) GetOperationResult
```

<p>Returns information about a specific operation. Operations include events such as when you create an instance, allocate a static IP, attach a static IP, and so on.</p>

#### `getOperations`

``` purescript
getOperations :: forall eff. GetOperationsRequest -> Aff (err :: RequestError | eff) GetOperationsResult
```

<p>Returns information about all operations.</p> <p>Results are returned from oldest to newest, up to a maximum of 200. Results can be paged by making each subsequent call to <code>GetOperations</code> use the maximum (last) <code>statusChangedAt</code> value from the previous request.</p>

#### `getOperationsForResource`

``` purescript
getOperationsForResource :: forall eff. GetOperationsForResourceRequest -> Aff (err :: RequestError | eff) GetOperationsForResourceResult
```

<p>Gets operations for a specific resource (e.g., an instance or a static IP).</p>

#### `getRegions`

``` purescript
getRegions :: forall eff. GetRegionsRequest -> Aff (err :: RequestError | eff) GetRegionsResult
```

<p>Returns a list of all valid regions for Amazon Lightsail. Use the <code>include availability zones</code> parameter to also return the availability zones in a region.</p>

#### `getStaticIp`

``` purescript
getStaticIp :: forall eff. GetStaticIpRequest -> Aff (err :: RequestError | eff) GetStaticIpResult
```

<p>Returns information about a specific static IP.</p>

#### `getStaticIps`

``` purescript
getStaticIps :: forall eff. GetStaticIpsRequest -> Aff (err :: RequestError | eff) GetStaticIpsResult
```

<p>Returns information about all static IPs in the user's account.</p>

#### `importKeyPair`

``` purescript
importKeyPair :: forall eff. ImportKeyPairRequest -> Aff (err :: RequestError | eff) ImportKeyPairResult
```

<p>Imports a public SSH key from a specific key pair.</p>

#### `isVpcPeered`

``` purescript
isVpcPeered :: forall eff. IsVpcPeeredRequest -> Aff (err :: RequestError | eff) IsVpcPeeredResult
```

<p>Returns a Boolean value indicating whether your Lightsail VPC is peered.</p>

#### `openInstancePublicPorts`

``` purescript
openInstancePublicPorts :: forall eff. OpenInstancePublicPortsRequest -> Aff (err :: RequestError | eff) OpenInstancePublicPortsResult
```

<p>Adds public ports to an Amazon Lightsail instance.</p>

#### `peerVpc`

``` purescript
peerVpc :: forall eff. PeerVpcRequest -> Aff (err :: RequestError | eff) PeerVpcResult
```

<p>Tries to peer the Lightsail VPC with the user's default VPC.</p>

#### `putInstancePublicPorts`

``` purescript
putInstancePublicPorts :: forall eff. PutInstancePublicPortsRequest -> Aff (err :: RequestError | eff) PutInstancePublicPortsResult
```

<p>Sets the specified open ports for an Amazon Lightsail instance, and closes all ports for every protocol not included in the current request.</p>

#### `rebootInstance`

``` purescript
rebootInstance :: forall eff. RebootInstanceRequest -> Aff (err :: RequestError | eff) RebootInstanceResult
```

<p>Restarts a specific instance. When your Amazon Lightsail instance is finished rebooting, Lightsail assigns a new public IP address. To use the same IP address after restarting, create a static IP address and attach it to the instance.</p>

#### `releaseStaticIp`

``` purescript
releaseStaticIp :: forall eff. ReleaseStaticIpRequest -> Aff (err :: RequestError | eff) ReleaseStaticIpResult
```

<p>Deletes a specific static IP from your account.</p>

#### `startInstance`

``` purescript
startInstance :: forall eff. StartInstanceRequest -> Aff (err :: RequestError | eff) StartInstanceResult
```

<p>Starts a specific Amazon Lightsail instance from a stopped state. To restart an instance, use the reboot instance operation.</p>

#### `stopInstance`

``` purescript
stopInstance :: forall eff. StopInstanceRequest -> Aff (err :: RequestError | eff) StopInstanceResult
```

<p>Stops a specific Amazon Lightsail instance that is currently running.</p>

#### `unpeerVpc`

``` purescript
unpeerVpc :: forall eff. UnpeerVpcRequest -> Aff (err :: RequestError | eff) UnpeerVpcResult
```

<p>Attempts to unpeer the Lightsail VPC from the user's default VPC.</p>

#### `updateDomainEntry`

``` purescript
updateDomainEntry :: forall eff. UpdateDomainEntryRequest -> Aff (err :: RequestError | eff) UpdateDomainEntryResult
```

<p>Updates a domain recordset after it is created.</p>

#### `updateLoadBalancerAttribute`

``` purescript
updateLoadBalancerAttribute :: forall eff. UpdateLoadBalancerAttributeRequest -> Aff (err :: RequestError | eff) UpdateLoadBalancerAttributeResult
```

<p>Updates the specified attribute for a load balancer.</p>

#### `AccessDeniedException`

``` purescript
newtype AccessDeniedException
  = AccessDeniedException { "Code'" :: NullOrUndefined (String), "Docs'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String), "Tip'" :: NullOrUndefined (String) }
```

<p>Lightsail throws this exception when the user cannot be authenticated or uses invalid credentials to access a resource.</p>

#### `AccessDirection`

``` purescript
newtype AccessDirection
  = AccessDirection String
```

#### `AccountSetupInProgressException`

``` purescript
newtype AccountSetupInProgressException
  = AccountSetupInProgressException { "Code'" :: NullOrUndefined (String), "Docs'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String), "Tip'" :: NullOrUndefined (String) }
```

<p>Lightsail throws this exception when an account is still in the setup in progress state.</p>

#### `AllocateStaticIpRequest`

``` purescript
newtype AllocateStaticIpRequest
  = AllocateStaticIpRequest { "StaticIpName'" :: ResourceName }
```

#### `AllocateStaticIpResult`

``` purescript
newtype AllocateStaticIpResult
  = AllocateStaticIpResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `AttachDiskRequest`

``` purescript
newtype AttachDiskRequest
  = AttachDiskRequest { "DiskName'" :: ResourceName, "InstanceName'" :: ResourceName, "DiskPath'" :: NonEmptyString }
```

#### `AttachDiskResult`

``` purescript
newtype AttachDiskResult
  = AttachDiskResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `AttachInstancesToLoadBalancerRequest`

``` purescript
newtype AttachInstancesToLoadBalancerRequest
  = AttachInstancesToLoadBalancerRequest { "LoadBalancerName'" :: ResourceName, "InstanceNames'" :: ResourceNameList }
```

#### `AttachInstancesToLoadBalancerResult`

``` purescript
newtype AttachInstancesToLoadBalancerResult
  = AttachInstancesToLoadBalancerResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `AttachLoadBalancerTlsCertificateRequest`

``` purescript
newtype AttachLoadBalancerTlsCertificateRequest
  = AttachLoadBalancerTlsCertificateRequest { "LoadBalancerName'" :: ResourceName, "CertificateName'" :: ResourceName }
```

#### `AttachLoadBalancerTlsCertificateResult`

``` purescript
newtype AttachLoadBalancerTlsCertificateResult
  = AttachLoadBalancerTlsCertificateResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `AttachStaticIpRequest`

``` purescript
newtype AttachStaticIpRequest
  = AttachStaticIpRequest { "StaticIpName'" :: ResourceName, "InstanceName'" :: ResourceName }
```

#### `AttachStaticIpResult`

``` purescript
newtype AttachStaticIpResult
  = AttachStaticIpResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `AttachedDiskMap`

``` purescript
newtype AttachedDiskMap
  = AttachedDiskMap (Map ResourceName DiskMapList)
```

#### `AvailabilityZone`

``` purescript
newtype AvailabilityZone
  = AvailabilityZone { "ZoneName'" :: NullOrUndefined (NonEmptyString), "State'" :: NullOrUndefined (NonEmptyString) }
```

<p>Describes an Availability Zone.</p>

#### `AvailabilityZoneList`

``` purescript
newtype AvailabilityZoneList
  = AvailabilityZoneList (Array AvailabilityZone)
```

#### `Base64`

``` purescript
newtype Base64
  = Base64 String
```

#### `Blueprint`

``` purescript
newtype Blueprint
  = Blueprint { "BlueprintId'" :: NullOrUndefined (NonEmptyString), "Name'" :: NullOrUndefined (ResourceName), "Group'" :: NullOrUndefined (NonEmptyString), "Type'" :: NullOrUndefined (BlueprintType), "Description'" :: NullOrUndefined (String), "IsActive'" :: NullOrUndefined (Boolean), "MinPower'" :: NullOrUndefined (Int), "Version'" :: NullOrUndefined (String), "VersionCode'" :: NullOrUndefined (String), "ProductUrl'" :: NullOrUndefined (String), "LicenseUrl'" :: NullOrUndefined (String), "Platform'" :: NullOrUndefined (InstancePlatform) }
```

<p>Describes a blueprint (a virtual private server image).</p>

#### `BlueprintList`

``` purescript
newtype BlueprintList
  = BlueprintList (Array Blueprint)
```

#### `BlueprintType`

``` purescript
newtype BlueprintType
  = BlueprintType String
```

#### `Bundle`

``` purescript
newtype Bundle
  = Bundle { "Price'" :: NullOrUndefined (Number), "CpuCount'" :: NullOrUndefined (Int), "DiskSizeInGb'" :: NullOrUndefined (Int), "BundleId'" :: NullOrUndefined (NonEmptyString), "InstanceType'" :: NullOrUndefined (String), "IsActive'" :: NullOrUndefined (Boolean), "Name'" :: NullOrUndefined (String), "Power'" :: NullOrUndefined (Int), "RamSizeInGb'" :: NullOrUndefined (Number), "TransferPerMonthInGb'" :: NullOrUndefined (Int), "SupportedPlatforms'" :: NullOrUndefined (InstancePlatformList) }
```

<p>Describes a bundle, which is a set of specs describing your virtual private server (or <i>instance</i>).</p>

#### `BundleList`

``` purescript
newtype BundleList
  = BundleList (Array Bundle)
```

#### `CloseInstancePublicPortsRequest`

``` purescript
newtype CloseInstancePublicPortsRequest
  = CloseInstancePublicPortsRequest { "PortInfo'" :: PortInfo, "InstanceName'" :: ResourceName }
```

#### `CloseInstancePublicPortsResult`

``` purescript
newtype CloseInstancePublicPortsResult
  = CloseInstancePublicPortsResult { "Operation'" :: NullOrUndefined (Operation) }
```

#### `CreateDiskFromSnapshotRequest`

``` purescript
newtype CreateDiskFromSnapshotRequest
  = CreateDiskFromSnapshotRequest { "DiskName'" :: ResourceName, "DiskSnapshotName'" :: ResourceName, "AvailabilityZone'" :: NonEmptyString, "SizeInGb'" :: Int }
```

#### `CreateDiskFromSnapshotResult`

``` purescript
newtype CreateDiskFromSnapshotResult
  = CreateDiskFromSnapshotResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `CreateDiskRequest`

``` purescript
newtype CreateDiskRequest
  = CreateDiskRequest { "DiskName'" :: ResourceName, "AvailabilityZone'" :: NonEmptyString, "SizeInGb'" :: Int }
```

#### `CreateDiskResult`

``` purescript
newtype CreateDiskResult
  = CreateDiskResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `CreateDiskSnapshotRequest`

``` purescript
newtype CreateDiskSnapshotRequest
  = CreateDiskSnapshotRequest { "DiskName'" :: ResourceName, "DiskSnapshotName'" :: ResourceName }
```

#### `CreateDiskSnapshotResult`

``` purescript
newtype CreateDiskSnapshotResult
  = CreateDiskSnapshotResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `CreateDomainEntryRequest`

``` purescript
newtype CreateDomainEntryRequest
  = CreateDomainEntryRequest { "DomainName'" :: DomainName, "DomainEntry'" :: DomainEntry }
```

#### `CreateDomainEntryResult`

``` purescript
newtype CreateDomainEntryResult
  = CreateDomainEntryResult { "Operation'" :: NullOrUndefined (Operation) }
```

#### `CreateDomainRequest`

``` purescript
newtype CreateDomainRequest
  = CreateDomainRequest { "DomainName'" :: DomainName }
```

#### `CreateDomainResult`

``` purescript
newtype CreateDomainResult
  = CreateDomainResult { "Operation'" :: NullOrUndefined (Operation) }
```

#### `CreateInstanceSnapshotRequest`

``` purescript
newtype CreateInstanceSnapshotRequest
  = CreateInstanceSnapshotRequest { "InstanceSnapshotName'" :: ResourceName, "InstanceName'" :: ResourceName }
```

#### `CreateInstanceSnapshotResult`

``` purescript
newtype CreateInstanceSnapshotResult
  = CreateInstanceSnapshotResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `CreateInstancesFromSnapshotRequest`

``` purescript
newtype CreateInstancesFromSnapshotRequest
  = CreateInstancesFromSnapshotRequest { "InstanceNames'" :: StringList, "AttachedDiskMapping'" :: NullOrUndefined (AttachedDiskMap), "AvailabilityZone'" :: String, "InstanceSnapshotName'" :: ResourceName, "BundleId'" :: NonEmptyString, "UserData'" :: NullOrUndefined (String), "KeyPairName'" :: NullOrUndefined (ResourceName) }
```

#### `CreateInstancesFromSnapshotResult`

``` purescript
newtype CreateInstancesFromSnapshotResult
  = CreateInstancesFromSnapshotResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `CreateInstancesRequest`

``` purescript
newtype CreateInstancesRequest
  = CreateInstancesRequest { "InstanceNames'" :: StringList, "AvailabilityZone'" :: String, "CustomImageName'" :: NullOrUndefined (ResourceName), "BlueprintId'" :: NonEmptyString, "BundleId'" :: NonEmptyString, "UserData'" :: NullOrUndefined (String), "KeyPairName'" :: NullOrUndefined (ResourceName) }
```

#### `CreateInstancesResult`

``` purescript
newtype CreateInstancesResult
  = CreateInstancesResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `CreateKeyPairRequest`

``` purescript
newtype CreateKeyPairRequest
  = CreateKeyPairRequest { "KeyPairName'" :: ResourceName }
```

#### `CreateKeyPairResult`

``` purescript
newtype CreateKeyPairResult
  = CreateKeyPairResult { "KeyPair'" :: NullOrUndefined (KeyPair), "PublicKeyBase64'" :: NullOrUndefined (Base64), "PrivateKeyBase64'" :: NullOrUndefined (Base64), "Operation'" :: NullOrUndefined (Operation) }
```

#### `CreateLoadBalancerRequest`

``` purescript
newtype CreateLoadBalancerRequest
  = CreateLoadBalancerRequest { "LoadBalancerName'" :: ResourceName, "InstancePort'" :: Port, "HealthCheckPath'" :: NullOrUndefined (String), "CertificateName'" :: NullOrUndefined (ResourceName), "CertificateDomainName'" :: NullOrUndefined (DomainName), "CertificateAlternativeNames'" :: NullOrUndefined (DomainNameList) }
```

#### `CreateLoadBalancerResult`

``` purescript
newtype CreateLoadBalancerResult
  = CreateLoadBalancerResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `CreateLoadBalancerTlsCertificateRequest`

``` purescript
newtype CreateLoadBalancerTlsCertificateRequest
  = CreateLoadBalancerTlsCertificateRequest { "LoadBalancerName'" :: ResourceName, "CertificateName'" :: ResourceName, "CertificateDomainName'" :: DomainName, "CertificateAlternativeNames'" :: NullOrUndefined (DomainNameList) }
```

#### `CreateLoadBalancerTlsCertificateResult`

``` purescript
newtype CreateLoadBalancerTlsCertificateResult
  = CreateLoadBalancerTlsCertificateResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `DeleteDiskRequest`

``` purescript
newtype DeleteDiskRequest
  = DeleteDiskRequest { "DiskName'" :: ResourceName }
```

#### `DeleteDiskResult`

``` purescript
newtype DeleteDiskResult
  = DeleteDiskResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `DeleteDiskSnapshotRequest`

``` purescript
newtype DeleteDiskSnapshotRequest
  = DeleteDiskSnapshotRequest { "DiskSnapshotName'" :: ResourceName }
```

#### `DeleteDiskSnapshotResult`

``` purescript
newtype DeleteDiskSnapshotResult
  = DeleteDiskSnapshotResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `DeleteDomainEntryRequest`

``` purescript
newtype DeleteDomainEntryRequest
  = DeleteDomainEntryRequest { "DomainName'" :: DomainName, "DomainEntry'" :: DomainEntry }
```

#### `DeleteDomainEntryResult`

``` purescript
newtype DeleteDomainEntryResult
  = DeleteDomainEntryResult { "Operation'" :: NullOrUndefined (Operation) }
```

#### `DeleteDomainRequest`

``` purescript
newtype DeleteDomainRequest
  = DeleteDomainRequest { "DomainName'" :: DomainName }
```

#### `DeleteDomainResult`

``` purescript
newtype DeleteDomainResult
  = DeleteDomainResult { "Operation'" :: NullOrUndefined (Operation) }
```

#### `DeleteInstanceRequest`

``` purescript
newtype DeleteInstanceRequest
  = DeleteInstanceRequest { "InstanceName'" :: ResourceName }
```

#### `DeleteInstanceResult`

``` purescript
newtype DeleteInstanceResult
  = DeleteInstanceResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `DeleteInstanceSnapshotRequest`

``` purescript
newtype DeleteInstanceSnapshotRequest
  = DeleteInstanceSnapshotRequest { "InstanceSnapshotName'" :: ResourceName }
```

#### `DeleteInstanceSnapshotResult`

``` purescript
newtype DeleteInstanceSnapshotResult
  = DeleteInstanceSnapshotResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `DeleteKeyPairRequest`

``` purescript
newtype DeleteKeyPairRequest
  = DeleteKeyPairRequest { "KeyPairName'" :: ResourceName }
```

#### `DeleteKeyPairResult`

``` purescript
newtype DeleteKeyPairResult
  = DeleteKeyPairResult { "Operation'" :: NullOrUndefined (Operation) }
```

#### `DeleteLoadBalancerRequest`

``` purescript
newtype DeleteLoadBalancerRequest
  = DeleteLoadBalancerRequest { "LoadBalancerName'" :: ResourceName }
```

#### `DeleteLoadBalancerResult`

``` purescript
newtype DeleteLoadBalancerResult
  = DeleteLoadBalancerResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `DeleteLoadBalancerTlsCertificateRequest`

``` purescript
newtype DeleteLoadBalancerTlsCertificateRequest
  = DeleteLoadBalancerTlsCertificateRequest { "LoadBalancerName'" :: ResourceName, "CertificateName'" :: ResourceName, "Force'" :: NullOrUndefined (Boolean) }
```

#### `DeleteLoadBalancerTlsCertificateResult`

``` purescript
newtype DeleteLoadBalancerTlsCertificateResult
  = DeleteLoadBalancerTlsCertificateResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `DetachDiskRequest`

``` purescript
newtype DetachDiskRequest
  = DetachDiskRequest { "DiskName'" :: ResourceName }
```

#### `DetachDiskResult`

``` purescript
newtype DetachDiskResult
  = DetachDiskResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `DetachInstancesFromLoadBalancerRequest`

``` purescript
newtype DetachInstancesFromLoadBalancerRequest
  = DetachInstancesFromLoadBalancerRequest { "LoadBalancerName'" :: ResourceName, "InstanceNames'" :: ResourceNameList }
```

#### `DetachInstancesFromLoadBalancerResult`

``` purescript
newtype DetachInstancesFromLoadBalancerResult
  = DetachInstancesFromLoadBalancerResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `DetachStaticIpRequest`

``` purescript
newtype DetachStaticIpRequest
  = DetachStaticIpRequest { "StaticIpName'" :: ResourceName }
```

#### `DetachStaticIpResult`

``` purescript
newtype DetachStaticIpResult
  = DetachStaticIpResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `Disk`

``` purescript
newtype Disk
  = Disk { "Name'" :: NullOrUndefined (ResourceName), "Arn'" :: NullOrUndefined (NonEmptyString), "SupportCode'" :: NullOrUndefined (String), "CreatedAt'" :: NullOrUndefined (IsoDate), "Location'" :: NullOrUndefined (ResourceLocation), "ResourceType'" :: NullOrUndefined (ResourceType), "SizeInGb'" :: NullOrUndefined (Int), "IsSystemDisk'" :: NullOrUndefined (Boolean), "Iops'" :: NullOrUndefined (Int), "Path'" :: NullOrUndefined (String), "State'" :: NullOrUndefined (DiskState), "AttachedTo'" :: NullOrUndefined (ResourceName), "IsAttached'" :: NullOrUndefined (Boolean), "AttachmentState'" :: NullOrUndefined (String), "GbInUse'" :: NullOrUndefined (Int) }
```

<p>Describes a system disk or an block storage disk.</p>

#### `DiskList`

``` purescript
newtype DiskList
  = DiskList (Array Disk)
```

#### `DiskMap`

``` purescript
newtype DiskMap
  = DiskMap { "OriginalDiskPath'" :: NullOrUndefined (NonEmptyString), "NewDiskName'" :: NullOrUndefined (ResourceName) }
```

<p>Describes a block storage disk mapping.</p>

#### `DiskMapList`

``` purescript
newtype DiskMapList
  = DiskMapList (Array DiskMap)
```

#### `DiskSnapshot`

``` purescript
newtype DiskSnapshot
  = DiskSnapshot { "Name'" :: NullOrUndefined (ResourceName), "Arn'" :: NullOrUndefined (NonEmptyString), "SupportCode'" :: NullOrUndefined (String), "CreatedAt'" :: NullOrUndefined (IsoDate), "Location'" :: NullOrUndefined (ResourceLocation), "ResourceType'" :: NullOrUndefined (ResourceType), "SizeInGb'" :: NullOrUndefined (Int), "State'" :: NullOrUndefined (DiskSnapshotState), "Progress'" :: NullOrUndefined (String), "FromDiskName'" :: NullOrUndefined (ResourceName), "FromDiskArn'" :: NullOrUndefined (NonEmptyString) }
```

<p>Describes a block storage disk snapshot.</p>

#### `DiskSnapshotList`

``` purescript
newtype DiskSnapshotList
  = DiskSnapshotList (Array DiskSnapshot)
```

#### `DiskSnapshotState`

``` purescript
newtype DiskSnapshotState
  = DiskSnapshotState String
```

#### `DiskState`

``` purescript
newtype DiskState
  = DiskState String
```

#### `Domain`

``` purescript
newtype Domain
  = Domain { "Name'" :: NullOrUndefined (ResourceName), "Arn'" :: NullOrUndefined (NonEmptyString), "SupportCode'" :: NullOrUndefined (String), "CreatedAt'" :: NullOrUndefined (IsoDate), "Location'" :: NullOrUndefined (ResourceLocation), "ResourceType'" :: NullOrUndefined (ResourceType), "DomainEntries'" :: NullOrUndefined (DomainEntryList) }
```

<p>Describes a domain where you are storing recordsets in Lightsail.</p>

#### `DomainEntry`

``` purescript
newtype DomainEntry
  = DomainEntry { "Id'" :: NullOrUndefined (NonEmptyString), "Name'" :: NullOrUndefined (DomainName), "Target'" :: NullOrUndefined (String), "IsAlias'" :: NullOrUndefined (Boolean), "Type'" :: NullOrUndefined (DomainEntryType), "Options'" :: NullOrUndefined (DomainEntryOptions) }
```

<p>Describes a domain recordset entry.</p>

#### `DomainEntryList`

``` purescript
newtype DomainEntryList
  = DomainEntryList (Array DomainEntry)
```

#### `DomainEntryOptions`

``` purescript
newtype DomainEntryOptions
  = DomainEntryOptions (Map DomainEntryOptionsKeys String)
```

#### `DomainEntryOptionsKeys`

``` purescript
newtype DomainEntryOptionsKeys
  = DomainEntryOptionsKeys String
```

#### `DomainEntryType`

``` purescript
newtype DomainEntryType
  = DomainEntryType String
```

#### `DomainList`

``` purescript
newtype DomainList
  = DomainList (Array Domain)
```

#### `DomainName`

``` purescript
newtype DomainName
  = DomainName String
```

#### `DomainNameList`

``` purescript
newtype DomainNameList
  = DomainNameList (Array DomainName)
```

#### `DownloadDefaultKeyPairRequest`

``` purescript
newtype DownloadDefaultKeyPairRequest
  = DownloadDefaultKeyPairRequest {  }
```

#### `DownloadDefaultKeyPairResult`

``` purescript
newtype DownloadDefaultKeyPairResult
  = DownloadDefaultKeyPairResult { "PublicKeyBase64'" :: NullOrUndefined (Base64), "PrivateKeyBase64'" :: NullOrUndefined (Base64) }
```

#### `GetActiveNamesRequest`

``` purescript
newtype GetActiveNamesRequest
  = GetActiveNamesRequest { "PageToken'" :: NullOrUndefined (String) }
```

#### `GetActiveNamesResult`

``` purescript
newtype GetActiveNamesResult
  = GetActiveNamesResult { "ActiveNames'" :: NullOrUndefined (StringList), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `GetBlueprintsRequest`

``` purescript
newtype GetBlueprintsRequest
  = GetBlueprintsRequest { "IncludeInactive'" :: NullOrUndefined (Boolean), "PageToken'" :: NullOrUndefined (String) }
```

#### `GetBlueprintsResult`

``` purescript
newtype GetBlueprintsResult
  = GetBlueprintsResult { "Blueprints'" :: NullOrUndefined (BlueprintList), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `GetBundlesRequest`

``` purescript
newtype GetBundlesRequest
  = GetBundlesRequest { "IncludeInactive'" :: NullOrUndefined (Boolean), "PageToken'" :: NullOrUndefined (String) }
```

#### `GetBundlesResult`

``` purescript
newtype GetBundlesResult
  = GetBundlesResult { "Bundles'" :: NullOrUndefined (BundleList), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `GetDiskRequest`

``` purescript
newtype GetDiskRequest
  = GetDiskRequest { "DiskName'" :: ResourceName }
```

#### `GetDiskResult`

``` purescript
newtype GetDiskResult
  = GetDiskResult { "Disk'" :: NullOrUndefined (Disk) }
```

#### `GetDiskSnapshotRequest`

``` purescript
newtype GetDiskSnapshotRequest
  = GetDiskSnapshotRequest { "DiskSnapshotName'" :: ResourceName }
```

#### `GetDiskSnapshotResult`

``` purescript
newtype GetDiskSnapshotResult
  = GetDiskSnapshotResult { "DiskSnapshot'" :: NullOrUndefined (DiskSnapshot) }
```

#### `GetDiskSnapshotsRequest`

``` purescript
newtype GetDiskSnapshotsRequest
  = GetDiskSnapshotsRequest { "PageToken'" :: NullOrUndefined (String) }
```

#### `GetDiskSnapshotsResult`

``` purescript
newtype GetDiskSnapshotsResult
  = GetDiskSnapshotsResult { "DiskSnapshots'" :: NullOrUndefined (DiskSnapshotList), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `GetDisksRequest`

``` purescript
newtype GetDisksRequest
  = GetDisksRequest { "PageToken'" :: NullOrUndefined (String) }
```

#### `GetDisksResult`

``` purescript
newtype GetDisksResult
  = GetDisksResult { "Disks'" :: NullOrUndefined (DiskList), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `GetDomainRequest`

``` purescript
newtype GetDomainRequest
  = GetDomainRequest { "DomainName'" :: DomainName }
```

#### `GetDomainResult`

``` purescript
newtype GetDomainResult
  = GetDomainResult { "Domain'" :: NullOrUndefined (Domain) }
```

#### `GetDomainsRequest`

``` purescript
newtype GetDomainsRequest
  = GetDomainsRequest { "PageToken'" :: NullOrUndefined (String) }
```

#### `GetDomainsResult`

``` purescript
newtype GetDomainsResult
  = GetDomainsResult { "Domains'" :: NullOrUndefined (DomainList), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `GetInstanceAccessDetailsRequest`

``` purescript
newtype GetInstanceAccessDetailsRequest
  = GetInstanceAccessDetailsRequest { "InstanceName'" :: ResourceName, "Protocol'" :: NullOrUndefined (InstanceAccessProtocol) }
```

#### `GetInstanceAccessDetailsResult`

``` purescript
newtype GetInstanceAccessDetailsResult
  = GetInstanceAccessDetailsResult { "AccessDetails'" :: NullOrUndefined (InstanceAccessDetails) }
```

#### `GetInstanceMetricDataRequest`

``` purescript
newtype GetInstanceMetricDataRequest
  = GetInstanceMetricDataRequest { "InstanceName'" :: ResourceName, "MetricName'" :: InstanceMetricName, "Period'" :: MetricPeriod, "StartTime'" :: Number, "EndTime'" :: Number, "Unit''" :: MetricUnit, "Statistics'" :: MetricStatisticList }
```

#### `GetInstanceMetricDataResult`

``` purescript
newtype GetInstanceMetricDataResult
  = GetInstanceMetricDataResult { "MetricName'" :: NullOrUndefined (InstanceMetricName), "MetricData'" :: NullOrUndefined (MetricDatapointList) }
```

#### `GetInstancePortStatesRequest`

``` purescript
newtype GetInstancePortStatesRequest
  = GetInstancePortStatesRequest { "InstanceName'" :: ResourceName }
```

#### `GetInstancePortStatesResult`

``` purescript
newtype GetInstancePortStatesResult
  = GetInstancePortStatesResult { "PortStates'" :: NullOrUndefined (InstancePortStateList) }
```

#### `GetInstanceRequest`

``` purescript
newtype GetInstanceRequest
  = GetInstanceRequest { "InstanceName'" :: ResourceName }
```

#### `GetInstanceResult`

``` purescript
newtype GetInstanceResult
  = GetInstanceResult { "Instance'" :: NullOrUndefined (Instance) }
```

#### `GetInstanceSnapshotRequest`

``` purescript
newtype GetInstanceSnapshotRequest
  = GetInstanceSnapshotRequest { "InstanceSnapshotName'" :: ResourceName }
```

#### `GetInstanceSnapshotResult`

``` purescript
newtype GetInstanceSnapshotResult
  = GetInstanceSnapshotResult { "InstanceSnapshot'" :: NullOrUndefined (InstanceSnapshot) }
```

#### `GetInstanceSnapshotsRequest`

``` purescript
newtype GetInstanceSnapshotsRequest
  = GetInstanceSnapshotsRequest { "PageToken'" :: NullOrUndefined (String) }
```

#### `GetInstanceSnapshotsResult`

``` purescript
newtype GetInstanceSnapshotsResult
  = GetInstanceSnapshotsResult { "InstanceSnapshots'" :: NullOrUndefined (InstanceSnapshotList), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `GetInstanceStateRequest`

``` purescript
newtype GetInstanceStateRequest
  = GetInstanceStateRequest { "InstanceName'" :: ResourceName }
```

#### `GetInstanceStateResult`

``` purescript
newtype GetInstanceStateResult
  = GetInstanceStateResult { "State'" :: NullOrUndefined (InstanceState) }
```

#### `GetInstancesRequest`

``` purescript
newtype GetInstancesRequest
  = GetInstancesRequest { "PageToken'" :: NullOrUndefined (String) }
```

#### `GetInstancesResult`

``` purescript
newtype GetInstancesResult
  = GetInstancesResult { "Instances'" :: NullOrUndefined (InstanceList), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `GetKeyPairRequest`

``` purescript
newtype GetKeyPairRequest
  = GetKeyPairRequest { "KeyPairName'" :: ResourceName }
```

#### `GetKeyPairResult`

``` purescript
newtype GetKeyPairResult
  = GetKeyPairResult { "KeyPair'" :: NullOrUndefined (KeyPair) }
```

#### `GetKeyPairsRequest`

``` purescript
newtype GetKeyPairsRequest
  = GetKeyPairsRequest { "PageToken'" :: NullOrUndefined (String) }
```

#### `GetKeyPairsResult`

``` purescript
newtype GetKeyPairsResult
  = GetKeyPairsResult { "KeyPairs'" :: NullOrUndefined (KeyPairList), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `GetLoadBalancerMetricDataRequest`

``` purescript
newtype GetLoadBalancerMetricDataRequest
  = GetLoadBalancerMetricDataRequest { "LoadBalancerName'" :: ResourceName, "MetricName'" :: LoadBalancerMetricName, "Period'" :: MetricPeriod, "StartTime'" :: Number, "EndTime'" :: Number, "Unit''" :: MetricUnit, "Statistics'" :: MetricStatisticList }
```

#### `GetLoadBalancerMetricDataResult`

``` purescript
newtype GetLoadBalancerMetricDataResult
  = GetLoadBalancerMetricDataResult { "MetricName'" :: NullOrUndefined (LoadBalancerMetricName), "MetricData'" :: NullOrUndefined (MetricDatapointList) }
```

#### `GetLoadBalancerRequest`

``` purescript
newtype GetLoadBalancerRequest
  = GetLoadBalancerRequest { "LoadBalancerName'" :: ResourceName }
```

#### `GetLoadBalancerResult`

``` purescript
newtype GetLoadBalancerResult
  = GetLoadBalancerResult { "LoadBalancer'" :: NullOrUndefined (LoadBalancer) }
```

#### `GetLoadBalancerTlsCertificatesRequest`

``` purescript
newtype GetLoadBalancerTlsCertificatesRequest
  = GetLoadBalancerTlsCertificatesRequest { "LoadBalancerName'" :: ResourceName }
```

#### `GetLoadBalancerTlsCertificatesResult`

``` purescript
newtype GetLoadBalancerTlsCertificatesResult
  = GetLoadBalancerTlsCertificatesResult { "TlsCertificates'" :: NullOrUndefined (LoadBalancerTlsCertificateList) }
```

#### `GetLoadBalancersRequest`

``` purescript
newtype GetLoadBalancersRequest
  = GetLoadBalancersRequest { "PageToken'" :: NullOrUndefined (String) }
```

#### `GetLoadBalancersResult`

``` purescript
newtype GetLoadBalancersResult
  = GetLoadBalancersResult { "LoadBalancers'" :: NullOrUndefined (LoadBalancerList), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `GetOperationRequest`

``` purescript
newtype GetOperationRequest
  = GetOperationRequest { "OperationId'" :: NonEmptyString }
```

#### `GetOperationResult`

``` purescript
newtype GetOperationResult
  = GetOperationResult { "Operation'" :: NullOrUndefined (Operation) }
```

#### `GetOperationsForResourceRequest`

``` purescript
newtype GetOperationsForResourceRequest
  = GetOperationsForResourceRequest { "ResourceName'" :: ResourceName, "PageToken'" :: NullOrUndefined (String) }
```

#### `GetOperationsForResourceResult`

``` purescript
newtype GetOperationsForResourceResult
  = GetOperationsForResourceResult { "Operations'" :: NullOrUndefined (OperationList), "NextPageCount'" :: NullOrUndefined (String), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `GetOperationsRequest`

``` purescript
newtype GetOperationsRequest
  = GetOperationsRequest { "PageToken'" :: NullOrUndefined (String) }
```

#### `GetOperationsResult`

``` purescript
newtype GetOperationsResult
  = GetOperationsResult { "Operations'" :: NullOrUndefined (OperationList), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `GetRegionsRequest`

``` purescript
newtype GetRegionsRequest
  = GetRegionsRequest { "IncludeAvailabilityZones'" :: NullOrUndefined (Boolean) }
```

#### `GetRegionsResult`

``` purescript
newtype GetRegionsResult
  = GetRegionsResult { "Regions'" :: NullOrUndefined (RegionList) }
```

#### `GetStaticIpRequest`

``` purescript
newtype GetStaticIpRequest
  = GetStaticIpRequest { "StaticIpName'" :: ResourceName }
```

#### `GetStaticIpResult`

``` purescript
newtype GetStaticIpResult
  = GetStaticIpResult { "StaticIp'" :: NullOrUndefined (StaticIp) }
```

#### `GetStaticIpsRequest`

``` purescript
newtype GetStaticIpsRequest
  = GetStaticIpsRequest { "PageToken'" :: NullOrUndefined (String) }
```

#### `GetStaticIpsResult`

``` purescript
newtype GetStaticIpsResult
  = GetStaticIpsResult { "StaticIps'" :: NullOrUndefined (StaticIpList), "NextPageToken'" :: NullOrUndefined (String) }
```

#### `ImportKeyPairRequest`

``` purescript
newtype ImportKeyPairRequest
  = ImportKeyPairRequest { "KeyPairName'" :: ResourceName, "PublicKeyBase64'" :: Base64 }
```

#### `ImportKeyPairResult`

``` purescript
newtype ImportKeyPairResult
  = ImportKeyPairResult { "Operation'" :: NullOrUndefined (Operation) }
```

#### `Instance`

``` purescript
newtype Instance
  = Instance { "Name'" :: NullOrUndefined (ResourceName), "Arn'" :: NullOrUndefined (NonEmptyString), "SupportCode'" :: NullOrUndefined (String), "CreatedAt'" :: NullOrUndefined (IsoDate), "Location'" :: NullOrUndefined (ResourceLocation), "ResourceType'" :: NullOrUndefined (ResourceType), "BlueprintId'" :: NullOrUndefined (NonEmptyString), "BlueprintName'" :: NullOrUndefined (NonEmptyString), "BundleId'" :: NullOrUndefined (NonEmptyString), "IsStaticIp'" :: NullOrUndefined (Boolean), "PrivateIpAddress'" :: NullOrUndefined (IpAddress), "PublicIpAddress'" :: NullOrUndefined (IpAddress), "Ipv6Address'" :: NullOrUndefined (IpV6Address), "Hardware'" :: NullOrUndefined (InstanceHardware), "Networking'" :: NullOrUndefined (InstanceNetworking), "State'" :: NullOrUndefined (InstanceState), "Username'" :: NullOrUndefined (NonEmptyString), "SshKeyName'" :: NullOrUndefined (ResourceName) }
```

<p>Describes an instance (a virtual private server).</p>

#### `InstanceAccessDetails`

``` purescript
newtype InstanceAccessDetails
  = InstanceAccessDetails { "CertKey'" :: NullOrUndefined (String), "ExpiresAt'" :: NullOrUndefined (IsoDate), "IpAddress'" :: NullOrUndefined (IpAddress), "Password'" :: NullOrUndefined (String), "PasswordData'" :: NullOrUndefined (PasswordData), "PrivateKey'" :: NullOrUndefined (String), "Protocol'" :: NullOrUndefined (InstanceAccessProtocol), "InstanceName'" :: NullOrUndefined (ResourceName), "Username'" :: NullOrUndefined (String) }
```

<p>The parameters for gaining temporary access to one of your Amazon Lightsail instances.</p>

#### `InstanceAccessProtocol`

``` purescript
newtype InstanceAccessProtocol
  = InstanceAccessProtocol String
```

#### `InstanceHardware`

``` purescript
newtype InstanceHardware
  = InstanceHardware { "CpuCount'" :: NullOrUndefined (Int), "Disks'" :: NullOrUndefined (DiskList), "RamSizeInGb'" :: NullOrUndefined (Number) }
```

<p>Describes the hardware for the instance.</p>

#### `InstanceHealthReason`

``` purescript
newtype InstanceHealthReason
  = InstanceHealthReason String
```

#### `InstanceHealthState`

``` purescript
newtype InstanceHealthState
  = InstanceHealthState String
```

#### `InstanceHealthSummary`

``` purescript
newtype InstanceHealthSummary
  = InstanceHealthSummary { "InstanceName'" :: NullOrUndefined (ResourceName), "InstanceHealth'" :: NullOrUndefined (InstanceHealthState), "InstanceHealthReason'" :: NullOrUndefined (InstanceHealthReason) }
```

<p>Describes information about the health of the instance.</p>

#### `InstanceHealthSummaryList`

``` purescript
newtype InstanceHealthSummaryList
  = InstanceHealthSummaryList (Array InstanceHealthSummary)
```

#### `InstanceList`

``` purescript
newtype InstanceList
  = InstanceList (Array Instance)
```

#### `InstanceMetricName`

``` purescript
newtype InstanceMetricName
  = InstanceMetricName String
```

#### `InstanceNetworking`

``` purescript
newtype InstanceNetworking
  = InstanceNetworking { "MonthlyTransfer'" :: NullOrUndefined (MonthlyTransfer), "Ports'" :: NullOrUndefined (InstancePortInfoList) }
```

<p>Describes monthly data transfer rates and port information for an instance.</p>

#### `InstancePlatform`

``` purescript
newtype InstancePlatform
  = InstancePlatform String
```

#### `InstancePlatformList`

``` purescript
newtype InstancePlatformList
  = InstancePlatformList (Array InstancePlatform)
```

#### `InstancePortInfo`

``` purescript
newtype InstancePortInfo
  = InstancePortInfo { "FromPort'" :: NullOrUndefined (Port), "ToPort'" :: NullOrUndefined (Port), "Protocol'" :: NullOrUndefined (NetworkProtocol), "AccessFrom'" :: NullOrUndefined (String), "AccessType'" :: NullOrUndefined (PortAccessType), "CommonName'" :: NullOrUndefined (String), "AccessDirection'" :: NullOrUndefined (AccessDirection) }
```

<p>Describes information about the instance ports.</p>

#### `InstancePortInfoList`

``` purescript
newtype InstancePortInfoList
  = InstancePortInfoList (Array InstancePortInfo)
```

#### `InstancePortState`

``` purescript
newtype InstancePortState
  = InstancePortState { "FromPort'" :: NullOrUndefined (Port), "ToPort'" :: NullOrUndefined (Port), "Protocol'" :: NullOrUndefined (NetworkProtocol), "State'" :: NullOrUndefined (PortState) }
```

<p>Describes the port state.</p>

#### `InstancePortStateList`

``` purescript
newtype InstancePortStateList
  = InstancePortStateList (Array InstancePortState)
```

#### `InstanceSnapshot`

``` purescript
newtype InstanceSnapshot
  = InstanceSnapshot { "Name'" :: NullOrUndefined (ResourceName), "Arn'" :: NullOrUndefined (NonEmptyString), "SupportCode'" :: NullOrUndefined (String), "CreatedAt'" :: NullOrUndefined (IsoDate), "Location'" :: NullOrUndefined (ResourceLocation), "ResourceType'" :: NullOrUndefined (ResourceType), "State'" :: NullOrUndefined (InstanceSnapshotState), "Progress'" :: NullOrUndefined (String), "FromAttachedDisks'" :: NullOrUndefined (DiskList), "FromInstanceName'" :: NullOrUndefined (ResourceName), "FromInstanceArn'" :: NullOrUndefined (NonEmptyString), "FromBlueprintId'" :: NullOrUndefined (String), "FromBundleId'" :: NullOrUndefined (String), "SizeInGb'" :: NullOrUndefined (Int) }
```

<p>Describes the snapshot of the virtual private server, or <i>instance</i>.</p>

#### `InstanceSnapshotList`

``` purescript
newtype InstanceSnapshotList
  = InstanceSnapshotList (Array InstanceSnapshot)
```

#### `InstanceSnapshotState`

``` purescript
newtype InstanceSnapshotState
  = InstanceSnapshotState String
```

#### `InstanceState`

``` purescript
newtype InstanceState
  = InstanceState { "Code'" :: NullOrUndefined (Int), "Name'" :: NullOrUndefined (String) }
```

<p>Describes the virtual private server (or <i>instance</i>) status.</p>

#### `InvalidInputException`

``` purescript
newtype InvalidInputException
  = InvalidInputException { "Code'" :: NullOrUndefined (String), "Docs'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String), "Tip'" :: NullOrUndefined (String) }
```

<p>Lightsail throws this exception when user input does not conform to the validation rules of an input field.</p> <note> <p>Domain-related APIs are only available in the N. Virginia (us-east-1) Region. Please set your Region configuration to us-east-1 to create, view, or edit these resources.</p> </note>

#### `IpAddress`

``` purescript
newtype IpAddress
  = IpAddress String
```

#### `IpV6Address`

``` purescript
newtype IpV6Address
  = IpV6Address String
```

#### `IsVpcPeeredRequest`

``` purescript
newtype IsVpcPeeredRequest
  = IsVpcPeeredRequest {  }
```

#### `IsVpcPeeredResult`

``` purescript
newtype IsVpcPeeredResult
  = IsVpcPeeredResult { "IsPeered'" :: NullOrUndefined (Boolean) }
```

#### `IsoDate`

``` purescript
newtype IsoDate
  = IsoDate Number
```

#### `KeyPair`

``` purescript
newtype KeyPair
  = KeyPair { "Name'" :: NullOrUndefined (ResourceName), "Arn'" :: NullOrUndefined (NonEmptyString), "SupportCode'" :: NullOrUndefined (String), "CreatedAt'" :: NullOrUndefined (IsoDate), "Location'" :: NullOrUndefined (ResourceLocation), "ResourceType'" :: NullOrUndefined (ResourceType), "Fingerprint'" :: NullOrUndefined (Base64) }
```

<p>Describes the SSH key pair.</p>

#### `KeyPairList`

``` purescript
newtype KeyPairList
  = KeyPairList (Array KeyPair)
```

#### `LoadBalancer`

``` purescript
newtype LoadBalancer
  = LoadBalancer { "Name'" :: NullOrUndefined (ResourceName), "Arn'" :: NullOrUndefined (NonEmptyString), "SupportCode'" :: NullOrUndefined (String), "CreatedAt'" :: NullOrUndefined (IsoDate), "Location'" :: NullOrUndefined (ResourceLocation), "ResourceType'" :: NullOrUndefined (ResourceType), "DnsName'" :: NullOrUndefined (NonEmptyString), "State'" :: NullOrUndefined (LoadBalancerState), "Protocol'" :: NullOrUndefined (LoadBalancerProtocol), "PublicPorts'" :: NullOrUndefined (PortList), "HealthCheckPath'" :: NullOrUndefined (NonEmptyString), "InstancePort'" :: NullOrUndefined (Int), "InstanceHealthSummary'" :: NullOrUndefined (InstanceHealthSummaryList), "TlsCertificateSummaries'" :: NullOrUndefined (LoadBalancerTlsCertificateSummaryList), "ConfigurationOptions'" :: NullOrUndefined (LoadBalancerConfigurationOptions) }
```

<p>Describes the Lightsail load balancer.</p>

#### `LoadBalancerAttributeName`

``` purescript
newtype LoadBalancerAttributeName
  = LoadBalancerAttributeName String
```

#### `LoadBalancerConfigurationOptions`

``` purescript
newtype LoadBalancerConfigurationOptions
  = LoadBalancerConfigurationOptions (Map LoadBalancerAttributeName String)
```

#### `LoadBalancerList`

``` purescript
newtype LoadBalancerList
  = LoadBalancerList (Array LoadBalancer)
```

#### `LoadBalancerMetricName`

``` purescript
newtype LoadBalancerMetricName
  = LoadBalancerMetricName String
```

#### `LoadBalancerProtocol`

``` purescript
newtype LoadBalancerProtocol
  = LoadBalancerProtocol String
```

#### `LoadBalancerState`

``` purescript
newtype LoadBalancerState
  = LoadBalancerState String
```

#### `LoadBalancerTlsCertificate`

``` purescript
newtype LoadBalancerTlsCertificate
  = LoadBalancerTlsCertificate { "Name'" :: NullOrUndefined (ResourceName), "Arn'" :: NullOrUndefined (NonEmptyString), "SupportCode'" :: NullOrUndefined (String), "CreatedAt'" :: NullOrUndefined (IsoDate), "Location'" :: NullOrUndefined (ResourceLocation), "ResourceType'" :: NullOrUndefined (ResourceType), "LoadBalancerName'" :: NullOrUndefined (ResourceName), "IsAttached'" :: NullOrUndefined (Boolean), "Status'" :: NullOrUndefined (LoadBalancerTlsCertificateStatus), "DomainName'" :: NullOrUndefined (DomainName), "DomainValidationRecords'" :: NullOrUndefined (LoadBalancerTlsCertificateDomainValidationRecordList), "FailureReason'" :: NullOrUndefined (LoadBalancerTlsCertificateFailureReason), "IssuedAt'" :: NullOrUndefined (IsoDate), "Issuer'" :: NullOrUndefined (NonEmptyString), "KeyAlgorithm'" :: NullOrUndefined (NonEmptyString), "NotAfter'" :: NullOrUndefined (IsoDate), "NotBefore'" :: NullOrUndefined (IsoDate), "RenewalSummary'" :: NullOrUndefined (LoadBalancerTlsCertificateRenewalSummary), "RevocationReason'" :: NullOrUndefined (LoadBalancerTlsCertificateRevocationReason), "RevokedAt'" :: NullOrUndefined (IsoDate), "Serial'" :: NullOrUndefined (NonEmptyString), "SignatureAlgorithm'" :: NullOrUndefined (NonEmptyString), "Subject'" :: NullOrUndefined (NonEmptyString), "SubjectAlternativeNames'" :: NullOrUndefined (StringList) }
```

<p>Describes a load balancer TLS/SSL certificate.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>

#### `LoadBalancerTlsCertificateDomainStatus`

``` purescript
newtype LoadBalancerTlsCertificateDomainStatus
  = LoadBalancerTlsCertificateDomainStatus String
```

#### `LoadBalancerTlsCertificateDomainValidationOption`

``` purescript
newtype LoadBalancerTlsCertificateDomainValidationOption
  = LoadBalancerTlsCertificateDomainValidationOption { "DomainName'" :: NullOrUndefined (DomainName), "ValidationStatus'" :: NullOrUndefined (LoadBalancerTlsCertificateDomainStatus) }
```

<p>Contains information about the domain names on a TLS/SSL certificate that you will use to validate domain ownership.</p>

#### `LoadBalancerTlsCertificateDomainValidationOptionList`

``` purescript
newtype LoadBalancerTlsCertificateDomainValidationOptionList
  = LoadBalancerTlsCertificateDomainValidationOptionList (Array LoadBalancerTlsCertificateDomainValidationOption)
```

#### `LoadBalancerTlsCertificateDomainValidationRecord`

``` purescript
newtype LoadBalancerTlsCertificateDomainValidationRecord
  = LoadBalancerTlsCertificateDomainValidationRecord { "Name'" :: NullOrUndefined (NonEmptyString), "Type'" :: NullOrUndefined (NonEmptyString), "Value'" :: NullOrUndefined (NonEmptyString), "ValidationStatus'" :: NullOrUndefined (LoadBalancerTlsCertificateDomainStatus), "DomainName'" :: NullOrUndefined (DomainName) }
```

<p>Describes the validation record of each domain name in the TLS/SSL certificate.</p>

#### `LoadBalancerTlsCertificateDomainValidationRecordList`

``` purescript
newtype LoadBalancerTlsCertificateDomainValidationRecordList
  = LoadBalancerTlsCertificateDomainValidationRecordList (Array LoadBalancerTlsCertificateDomainValidationRecord)
```

#### `LoadBalancerTlsCertificateFailureReason`

``` purescript
newtype LoadBalancerTlsCertificateFailureReason
  = LoadBalancerTlsCertificateFailureReason String
```

#### `LoadBalancerTlsCertificateList`

``` purescript
newtype LoadBalancerTlsCertificateList
  = LoadBalancerTlsCertificateList (Array LoadBalancerTlsCertificate)
```

#### `LoadBalancerTlsCertificateRenewalStatus`

``` purescript
newtype LoadBalancerTlsCertificateRenewalStatus
  = LoadBalancerTlsCertificateRenewalStatus String
```

#### `LoadBalancerTlsCertificateRenewalSummary`

``` purescript
newtype LoadBalancerTlsCertificateRenewalSummary
  = LoadBalancerTlsCertificateRenewalSummary { "RenewalStatus'" :: NullOrUndefined (LoadBalancerTlsCertificateRenewalStatus), "DomainValidationOptions'" :: NullOrUndefined (LoadBalancerTlsCertificateDomainValidationOptionList) }
```

<p>Contains information about the status of Lightsail's managed renewal for the certificate.</p>

#### `LoadBalancerTlsCertificateRevocationReason`

``` purescript
newtype LoadBalancerTlsCertificateRevocationReason
  = LoadBalancerTlsCertificateRevocationReason String
```

#### `LoadBalancerTlsCertificateStatus`

``` purescript
newtype LoadBalancerTlsCertificateStatus
  = LoadBalancerTlsCertificateStatus String
```

#### `LoadBalancerTlsCertificateSummary`

``` purescript
newtype LoadBalancerTlsCertificateSummary
  = LoadBalancerTlsCertificateSummary { "Name'" :: NullOrUndefined (ResourceName), "IsAttached'" :: NullOrUndefined (Boolean) }
```

<p>Provides a summary of TLS/SSL certificate metadata.</p>

#### `LoadBalancerTlsCertificateSummaryList`

``` purescript
newtype LoadBalancerTlsCertificateSummaryList
  = LoadBalancerTlsCertificateSummaryList (Array LoadBalancerTlsCertificateSummary)
```

#### `MetricDatapoint`

``` purescript
newtype MetricDatapoint
  = MetricDatapoint { "Average'" :: NullOrUndefined (Number), "Maximum'" :: NullOrUndefined (Number), "Minimum'" :: NullOrUndefined (Number), "SampleCount'" :: NullOrUndefined (Number), "Sum'" :: NullOrUndefined (Number), "Number" :: NullOrUndefined (Number), "Unit''" :: NullOrUndefined (MetricUnit) }
```

<p>Describes the metric data point.</p>

#### `MetricDatapointList`

``` purescript
newtype MetricDatapointList
  = MetricDatapointList (Array MetricDatapoint)
```

#### `MetricPeriod`

``` purescript
newtype MetricPeriod
  = MetricPeriod Int
```

#### `MetricStatistic`

``` purescript
newtype MetricStatistic
  = MetricStatistic String
```

#### `MetricStatisticList`

``` purescript
newtype MetricStatisticList
  = MetricStatisticList (Array MetricStatistic)
```

#### `MetricUnit`

``` purescript
newtype MetricUnit
  = MetricUnit String
```

#### `MonthlyTransfer`

``` purescript
newtype MonthlyTransfer
  = MonthlyTransfer { "GbPerMonthAllocated'" :: NullOrUndefined (Int) }
```

<p>Describes the monthly data transfer in and out of your virtual private server (or <i>instance</i>).</p>

#### `NetworkProtocol`

``` purescript
newtype NetworkProtocol
  = NetworkProtocol String
```

#### `NonEmptyString`

``` purescript
newtype NonEmptyString
  = NonEmptyString String
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Code'" :: NullOrUndefined (String), "Docs'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String), "Tip'" :: NullOrUndefined (String) }
```

<p>Lightsail throws this exception when it cannot find a resource.</p>

#### `OpenInstancePublicPortsRequest`

``` purescript
newtype OpenInstancePublicPortsRequest
  = OpenInstancePublicPortsRequest { "PortInfo'" :: PortInfo, "InstanceName'" :: ResourceName }
```

#### `OpenInstancePublicPortsResult`

``` purescript
newtype OpenInstancePublicPortsResult
  = OpenInstancePublicPortsResult { "Operation'" :: NullOrUndefined (Operation) }
```

#### `Operation`

``` purescript
newtype Operation
  = Operation { "Id'" :: NullOrUndefined (NonEmptyString), "ResourceName'" :: NullOrUndefined (ResourceName), "ResourceType'" :: NullOrUndefined (ResourceType), "CreatedAt'" :: NullOrUndefined (IsoDate), "Location'" :: NullOrUndefined (ResourceLocation), "IsTerminal'" :: NullOrUndefined (Boolean), "OperationDetails'" :: NullOrUndefined (String), "OperationType'" :: NullOrUndefined (OperationType), "Status'" :: NullOrUndefined (OperationStatus), "StatusChangedAt'" :: NullOrUndefined (IsoDate), "ErrorCode'" :: NullOrUndefined (String), "ErrorDetails'" :: NullOrUndefined (String) }
```

<p>Describes the API operation.</p>

#### `OperationFailureException`

``` purescript
newtype OperationFailureException
  = OperationFailureException { "Code'" :: NullOrUndefined (String), "Docs'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String), "Tip'" :: NullOrUndefined (String) }
```

<p>Lightsail throws this exception when an operation fails to execute.</p>

#### `OperationList`

``` purescript
newtype OperationList
  = OperationList (Array Operation)
```

#### `OperationStatus`

``` purescript
newtype OperationStatus
  = OperationStatus String
```

#### `OperationType`

``` purescript
newtype OperationType
  = OperationType String
```

#### `PasswordData`

``` purescript
newtype PasswordData
  = PasswordData { "Ciphertext'" :: NullOrUndefined (String), "KeyPairName'" :: NullOrUndefined (ResourceName) }
```

<p>The password data for the Windows Server-based instance, including the ciphertext and the key pair name.</p>

#### `PeerVpcRequest`

``` purescript
newtype PeerVpcRequest
  = PeerVpcRequest {  }
```

#### `PeerVpcResult`

``` purescript
newtype PeerVpcResult
  = PeerVpcResult { "Operation'" :: NullOrUndefined (Operation) }
```

#### `Port`

``` purescript
newtype Port
  = Port Int
```

#### `PortAccessType`

``` purescript
newtype PortAccessType
  = PortAccessType String
```

#### `PortInfo`

``` purescript
newtype PortInfo
  = PortInfo { "FromPort'" :: NullOrUndefined (Port), "ToPort'" :: NullOrUndefined (Port), "Protocol'" :: NullOrUndefined (NetworkProtocol) }
```

<p>Describes information about the ports on your virtual private server (or <i>instance</i>).</p>

#### `PortInfoList`

``` purescript
newtype PortInfoList
  = PortInfoList (Array PortInfo)
```

#### `PortList`

``` purescript
newtype PortList
  = PortList (Array Port)
```

#### `PortState`

``` purescript
newtype PortState
  = PortState String
```

#### `PutInstancePublicPortsRequest`

``` purescript
newtype PutInstancePublicPortsRequest
  = PutInstancePublicPortsRequest { "PortInfos'" :: PortInfoList, "InstanceName'" :: ResourceName }
```

#### `PutInstancePublicPortsResult`

``` purescript
newtype PutInstancePublicPortsResult
  = PutInstancePublicPortsResult { "Operation'" :: NullOrUndefined (Operation) }
```

#### `RebootInstanceRequest`

``` purescript
newtype RebootInstanceRequest
  = RebootInstanceRequest { "InstanceName'" :: ResourceName }
```

#### `RebootInstanceResult`

``` purescript
newtype RebootInstanceResult
  = RebootInstanceResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `Region`

``` purescript
newtype Region
  = Region { "ContinentCode'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "DisplayName'" :: NullOrUndefined (String), "Name'" :: NullOrUndefined (RegionName), "AvailabilityZones'" :: NullOrUndefined (AvailabilityZoneList) }
```

<p>Describes the AWS Region.</p>

#### `RegionList`

``` purescript
newtype RegionList
  = RegionList (Array Region)
```

#### `RegionName`

``` purescript
newtype RegionName
  = RegionName String
```

#### `ReleaseStaticIpRequest`

``` purescript
newtype ReleaseStaticIpRequest
  = ReleaseStaticIpRequest { "StaticIpName'" :: ResourceName }
```

#### `ReleaseStaticIpResult`

``` purescript
newtype ReleaseStaticIpResult
  = ReleaseStaticIpResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `ResourceLocation`

``` purescript
newtype ResourceLocation
  = ResourceLocation { "AvailabilityZone'" :: NullOrUndefined (String), "RegionName'" :: NullOrUndefined (RegionName) }
```

<p>Describes the resource location.</p>

#### `ResourceName`

``` purescript
newtype ResourceName
  = ResourceName String
```

#### `ResourceNameList`

``` purescript
newtype ResourceNameList
  = ResourceNameList (Array ResourceName)
```

#### `ResourceType`

``` purescript
newtype ResourceType
  = ResourceType String
```

#### `ServiceException`

``` purescript
newtype ServiceException
  = ServiceException { "Code'" :: NullOrUndefined (String), "Docs'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String), "Tip'" :: NullOrUndefined (String) }
```

<p>A general service exception.</p>

#### `StartInstanceRequest`

``` purescript
newtype StartInstanceRequest
  = StartInstanceRequest { "InstanceName'" :: ResourceName }
```

#### `StartInstanceResult`

``` purescript
newtype StartInstanceResult
  = StartInstanceResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `StaticIp`

``` purescript
newtype StaticIp
  = StaticIp { "Name'" :: NullOrUndefined (ResourceName), "Arn'" :: NullOrUndefined (NonEmptyString), "SupportCode'" :: NullOrUndefined (String), "CreatedAt'" :: NullOrUndefined (IsoDate), "Location'" :: NullOrUndefined (ResourceLocation), "ResourceType'" :: NullOrUndefined (ResourceType), "IpAddress'" :: NullOrUndefined (IpAddress), "AttachedTo'" :: NullOrUndefined (ResourceName), "IsAttached'" :: NullOrUndefined (Boolean) }
```

<p>Describes the static IP.</p>

#### `StaticIpList`

``` purescript
newtype StaticIpList
  = StaticIpList (Array StaticIp)
```

#### `StopInstanceRequest`

``` purescript
newtype StopInstanceRequest
  = StopInstanceRequest { "InstanceName'" :: ResourceName, "Force'" :: NullOrUndefined (Boolean) }
```

#### `StopInstanceResult`

``` purescript
newtype StopInstanceResult
  = StopInstanceResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array String)
```

#### `StringMax256`

``` purescript
newtype StringMax256
  = StringMax256 String
```

#### `UnauthenticatedException`

``` purescript
newtype UnauthenticatedException
  = UnauthenticatedException { "Code'" :: NullOrUndefined (String), "Docs'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String), "Tip'" :: NullOrUndefined (String) }
```

<p>Lightsail throws this exception when the user has not been authenticated.</p>

#### `UnpeerVpcRequest`

``` purescript
newtype UnpeerVpcRequest
  = UnpeerVpcRequest {  }
```

#### `UnpeerVpcResult`

``` purescript
newtype UnpeerVpcResult
  = UnpeerVpcResult { "Operation'" :: NullOrUndefined (Operation) }
```

#### `UpdateDomainEntryRequest`

``` purescript
newtype UpdateDomainEntryRequest
  = UpdateDomainEntryRequest { "DomainName'" :: DomainName, "DomainEntry'" :: DomainEntry }
```

#### `UpdateDomainEntryResult`

``` purescript
newtype UpdateDomainEntryResult
  = UpdateDomainEntryResult { "Operations'" :: NullOrUndefined (OperationList) }
```

#### `UpdateLoadBalancerAttributeRequest`

``` purescript
newtype UpdateLoadBalancerAttributeRequest
  = UpdateLoadBalancerAttributeRequest { "LoadBalancerName'" :: ResourceName, "AttributeName'" :: LoadBalancerAttributeName, "AttributeValue'" :: StringMax256 }
```

#### `UpdateLoadBalancerAttributeResult`

``` purescript
newtype UpdateLoadBalancerAttributeResult
  = UpdateLoadBalancerAttributeResult { "Operations'" :: NullOrUndefined (OperationList) }
```


