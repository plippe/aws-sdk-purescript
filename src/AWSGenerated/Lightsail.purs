

-- | <p>Amazon Lightsail is the easiest way to get started with AWS for developers who just need virtual private servers. Lightsail includes everything you need to launch your project quickly - a virtual machine, SSD-based storage, data transfer, DNS management, and a static IP - for a low, predictable price. You manage those Lightsail servers through the Lightsail console or by using the API or command-line interface (CLI).</p> <p>For more information about Lightsail concepts and tasks, see the <a href="https://lightsail.aws.amazon.com/ls/docs/all">Lightsail Dev Guide</a>.</p> <p>To use the Lightsail API or the CLI, you will need to use AWS Identity and Access Management (IAM) to generate access keys. For details about how to set this up, see the <a href="http://lightsail.aws.amazon.com/ls/docs/how-to/article/lightsail-how-to-set-up-access-keys-to-use-sdk-api-cli">Lightsail Dev Guide</a>.</p>
module AWS.Lightsail where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Lightsail" :: String


-- | <p>Allocates a static IP address.</p>
allocateStaticIp :: forall eff. AllocateStaticIpRequest -> Aff (err :: AWS.RequestError | eff) AllocateStaticIpResult
allocateStaticIp = AWS.request serviceName "AllocateStaticIp" 


-- | <p>Attaches a block storage disk to a running or stopped Lightsail instance and exposes it to the instance with the specified disk name.</p>
attachDisk :: forall eff. AttachDiskRequest -> Aff (err :: AWS.RequestError | eff) AttachDiskResult
attachDisk = AWS.request serviceName "AttachDisk" 


-- | <p>Attaches one or more Lightsail instances to a load balancer.</p>
attachInstancesToLoadBalancer :: forall eff. AttachInstancesToLoadBalancerRequest -> Aff (err :: AWS.RequestError | eff) AttachInstancesToLoadBalancerResult
attachInstancesToLoadBalancer = AWS.request serviceName "AttachInstancesToLoadBalancer" 


-- | <p>Attaches a Transport Layer Security (TLS) certificate to your load balancer.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>
attachLoadBalancerTlsCertificate :: forall eff. AttachLoadBalancerTlsCertificateRequest -> Aff (err :: AWS.RequestError | eff) AttachLoadBalancerTlsCertificateResult
attachLoadBalancerTlsCertificate = AWS.request serviceName "AttachLoadBalancerTlsCertificate" 


-- | <p>Attaches a static IP address to a specific Amazon Lightsail instance.</p>
attachStaticIp :: forall eff. AttachStaticIpRequest -> Aff (err :: AWS.RequestError | eff) AttachStaticIpResult
attachStaticIp = AWS.request serviceName "AttachStaticIp" 


-- | <p>Closes the public ports on a specific Amazon Lightsail instance.</p>
closeInstancePublicPorts :: forall eff. CloseInstancePublicPortsRequest -> Aff (err :: AWS.RequestError | eff) CloseInstancePublicPortsResult
closeInstancePublicPorts = AWS.request serviceName "CloseInstancePublicPorts" 


-- | <p>Creates a block storage disk that can be attached to a Lightsail instance in the same Availability Zone (e.g., <code>us-east-2a</code>). The disk is created in the regional endpoint that you send the HTTP request to. For more information, see <a href="https://lightsail.aws.amazon.com/ls/docs/overview/article/understanding-regions-and-availability-zones-in-amazon-lightsail">Regions and Availability Zones in Lightsail</a>.</p>
createDisk :: forall eff. CreateDiskRequest -> Aff (err :: AWS.RequestError | eff) CreateDiskResult
createDisk = AWS.request serviceName "CreateDisk" 


-- | <p>Creates a block storage disk from a disk snapshot that can be attached to a Lightsail instance in the same Availability Zone (e.g., <code>us-east-2a</code>). The disk is created in the regional endpoint that you send the HTTP request to. For more information, see <a href="https://lightsail.aws.amazon.com/ls/docs/overview/article/understanding-regions-and-availability-zones-in-amazon-lightsail">Regions and Availability Zones in Lightsail</a>.</p>
createDiskFromSnapshot :: forall eff. CreateDiskFromSnapshotRequest -> Aff (err :: AWS.RequestError | eff) CreateDiskFromSnapshotResult
createDiskFromSnapshot = AWS.request serviceName "CreateDiskFromSnapshot" 


-- | <p>Creates a snapshot of a block storage disk. You can use snapshots for backups, to make copies of disks, and to save data before shutting down a Lightsail instance.</p> <p>You can take a snapshot of an attached disk that is in use; however, snapshots only capture data that has been written to your disk at the time the snapshot command is issued. This may exclude any data that has been cached by any applications or the operating system. If you can pause any file systems on the disk long enough to take a snapshot, your snapshot should be complete. Nevertheless, if you cannot pause all file writes to the disk, you should unmount the disk from within the Lightsail instance, issue the create disk snapshot command, and then remount the disk to ensure a consistent and complete snapshot. You may remount and use your disk while the snapshot status is pending.</p>
createDiskSnapshot :: forall eff. CreateDiskSnapshotRequest -> Aff (err :: AWS.RequestError | eff) CreateDiskSnapshotResult
createDiskSnapshot = AWS.request serviceName "CreateDiskSnapshot" 


-- | <p>Creates a domain resource for the specified domain (e.g., example.com).</p>
createDomain :: forall eff. CreateDomainRequest -> Aff (err :: AWS.RequestError | eff) CreateDomainResult
createDomain = AWS.request serviceName "CreateDomain" 


-- | <p>Creates one of the following entry records associated with the domain: A record, CNAME record, TXT record, or MX record.</p>
createDomainEntry :: forall eff. CreateDomainEntryRequest -> Aff (err :: AWS.RequestError | eff) CreateDomainEntryResult
createDomainEntry = AWS.request serviceName "CreateDomainEntry" 


-- | <p>Creates a snapshot of a specific virtual private server, or <i>instance</i>. You can use a snapshot to create a new instance that is based on that snapshot.</p>
createInstanceSnapshot :: forall eff. CreateInstanceSnapshotRequest -> Aff (err :: AWS.RequestError | eff) CreateInstanceSnapshotResult
createInstanceSnapshot = AWS.request serviceName "CreateInstanceSnapshot" 


-- | <p>Creates one or more Amazon Lightsail virtual private servers, or <i>instances</i>.</p>
createInstances :: forall eff. CreateInstancesRequest -> Aff (err :: AWS.RequestError | eff) CreateInstancesResult
createInstances = AWS.request serviceName "CreateInstances" 


-- | <p>Uses a specific snapshot as a blueprint for creating one or more new instances that are based on that identical configuration.</p>
createInstancesFromSnapshot :: forall eff. CreateInstancesFromSnapshotRequest -> Aff (err :: AWS.RequestError | eff) CreateInstancesFromSnapshotResult
createInstancesFromSnapshot = AWS.request serviceName "CreateInstancesFromSnapshot" 


-- | <p>Creates sn SSH key pair.</p>
createKeyPair :: forall eff. CreateKeyPairRequest -> Aff (err :: AWS.RequestError | eff) CreateKeyPairResult
createKeyPair = AWS.request serviceName "CreateKeyPair" 


-- | <p>Creates a Lightsail load balancer.</p> <p>When you create a load balancer, you can specify certificates and port settings. You can create up to 5 load balancers per AWS Region in your account.</p>
createLoadBalancer :: forall eff. CreateLoadBalancerRequest -> Aff (err :: AWS.RequestError | eff) CreateLoadBalancerResult
createLoadBalancer = AWS.request serviceName "CreateLoadBalancer" 


-- | <p>Creates a Lightsail load balancer TLS certificate.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>
createLoadBalancerTlsCertificate :: forall eff. CreateLoadBalancerTlsCertificateRequest -> Aff (err :: AWS.RequestError | eff) CreateLoadBalancerTlsCertificateResult
createLoadBalancerTlsCertificate = AWS.request serviceName "CreateLoadBalancerTlsCertificate" 


-- | <p>Deletes the specified block storage disk. The disk must be in the <code>available</code> state (not attached to a Lightsail instance).</p> <note> <p>The disk may remain in the <code>deleting</code> state for several minutes.</p> </note>
deleteDisk :: forall eff. DeleteDiskRequest -> Aff (err :: AWS.RequestError | eff) DeleteDiskResult
deleteDisk = AWS.request serviceName "DeleteDisk" 


-- | <p>Deletes the specified disk snapshot.</p> <p>When you make periodic snapshots of a disk, the snapshots are incremental, and only the blocks on the device that have changed since your last snapshot are saved in the new snapshot. When you delete a snapshot, only the data not needed for any other snapshot is removed. So regardless of which prior snapshots have been deleted, all active snapshots will have access to all the information needed to restore the disk.</p>
deleteDiskSnapshot :: forall eff. DeleteDiskSnapshotRequest -> Aff (err :: AWS.RequestError | eff) DeleteDiskSnapshotResult
deleteDiskSnapshot = AWS.request serviceName "DeleteDiskSnapshot" 


-- | <p>Deletes the specified domain recordset and all of its domain records.</p>
deleteDomain :: forall eff. DeleteDomainRequest -> Aff (err :: AWS.RequestError | eff) DeleteDomainResult
deleteDomain = AWS.request serviceName "DeleteDomain" 


-- | <p>Deletes a specific domain entry.</p>
deleteDomainEntry :: forall eff. DeleteDomainEntryRequest -> Aff (err :: AWS.RequestError | eff) DeleteDomainEntryResult
deleteDomainEntry = AWS.request serviceName "DeleteDomainEntry" 


-- | <p>Deletes a specific Amazon Lightsail virtual private server, or <i>instance</i>.</p>
deleteInstance :: forall eff. DeleteInstanceRequest -> Aff (err :: AWS.RequestError | eff) DeleteInstanceResult
deleteInstance = AWS.request serviceName "DeleteInstance" 


-- | <p>Deletes a specific snapshot of a virtual private server (or <i>instance</i>).</p>
deleteInstanceSnapshot :: forall eff. DeleteInstanceSnapshotRequest -> Aff (err :: AWS.RequestError | eff) DeleteInstanceSnapshotResult
deleteInstanceSnapshot = AWS.request serviceName "DeleteInstanceSnapshot" 


-- | <p>Deletes a specific SSH key pair.</p>
deleteKeyPair :: forall eff. DeleteKeyPairRequest -> Aff (err :: AWS.RequestError | eff) DeleteKeyPairResult
deleteKeyPair = AWS.request serviceName "DeleteKeyPair" 


-- | <p>Deletes a Lightsail load balancer.</p>
deleteLoadBalancer :: forall eff. DeleteLoadBalancerRequest -> Aff (err :: AWS.RequestError | eff) DeleteLoadBalancerResult
deleteLoadBalancer = AWS.request serviceName "DeleteLoadBalancer" 


-- | <p>Deletes a TLS/SSL certificate associated with a Lightsail load balancer.</p>
deleteLoadBalancerTlsCertificate :: forall eff. DeleteLoadBalancerTlsCertificateRequest -> Aff (err :: AWS.RequestError | eff) DeleteLoadBalancerTlsCertificateResult
deleteLoadBalancerTlsCertificate = AWS.request serviceName "DeleteLoadBalancerTlsCertificate" 


-- | <p>Detaches a stopped block storage disk from a Lightsail instance. Make sure to unmount any file systems on the device within your operating system before stopping the instance and detaching the disk.</p>
detachDisk :: forall eff. DetachDiskRequest -> Aff (err :: AWS.RequestError | eff) DetachDiskResult
detachDisk = AWS.request serviceName "DetachDisk" 


-- | <p>Detaches the specified instances from a Lightsail load balancer.</p>
detachInstancesFromLoadBalancer :: forall eff. DetachInstancesFromLoadBalancerRequest -> Aff (err :: AWS.RequestError | eff) DetachInstancesFromLoadBalancerResult
detachInstancesFromLoadBalancer = AWS.request serviceName "DetachInstancesFromLoadBalancer" 


-- | <p>Detaches a static IP from the Amazon Lightsail instance to which it is attached.</p>
detachStaticIp :: forall eff. DetachStaticIpRequest -> Aff (err :: AWS.RequestError | eff) DetachStaticIpResult
detachStaticIp = AWS.request serviceName "DetachStaticIp" 


-- | <p>Downloads the default SSH key pair from the user's account.</p>
downloadDefaultKeyPair :: forall eff. DownloadDefaultKeyPairRequest -> Aff (err :: AWS.RequestError | eff) DownloadDefaultKeyPairResult
downloadDefaultKeyPair = AWS.request serviceName "DownloadDefaultKeyPair" 


-- | <p>Returns the names of all active (not deleted) resources.</p>
getActiveNames :: forall eff. GetActiveNamesRequest -> Aff (err :: AWS.RequestError | eff) GetActiveNamesResult
getActiveNames = AWS.request serviceName "GetActiveNames" 


-- | <p>Returns the list of available instance images, or <i>blueprints</i>. You can use a blueprint to create a new virtual private server already running a specific operating system, as well as a preinstalled app or development stack. The software each instance is running depends on the blueprint image you choose.</p>
getBlueprints :: forall eff. GetBlueprintsRequest -> Aff (err :: AWS.RequestError | eff) GetBlueprintsResult
getBlueprints = AWS.request serviceName "GetBlueprints" 


-- | <p>Returns the list of bundles that are available for purchase. A bundle describes the specs for your virtual private server (or <i>instance</i>).</p>
getBundles :: forall eff. GetBundlesRequest -> Aff (err :: AWS.RequestError | eff) GetBundlesResult
getBundles = AWS.request serviceName "GetBundles" 


-- | <p>Returns information about a specific block storage disk.</p>
getDisk :: forall eff. GetDiskRequest -> Aff (err :: AWS.RequestError | eff) GetDiskResult
getDisk = AWS.request serviceName "GetDisk" 


-- | <p>Returns information about a specific block storage disk snapshot.</p>
getDiskSnapshot :: forall eff. GetDiskSnapshotRequest -> Aff (err :: AWS.RequestError | eff) GetDiskSnapshotResult
getDiskSnapshot = AWS.request serviceName "GetDiskSnapshot" 


-- | <p>Returns information about all block storage disk snapshots in your AWS account and region.</p> <p>If you are describing a long list of disk snapshots, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.</p>
getDiskSnapshots :: forall eff. GetDiskSnapshotsRequest -> Aff (err :: AWS.RequestError | eff) GetDiskSnapshotsResult
getDiskSnapshots = AWS.request serviceName "GetDiskSnapshots" 


-- | <p>Returns information about all block storage disks in your AWS account and region.</p> <p>If you are describing a long list of disks, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.</p>
getDisks :: forall eff. GetDisksRequest -> Aff (err :: AWS.RequestError | eff) GetDisksResult
getDisks = AWS.request serviceName "GetDisks" 


-- | <p>Returns information about a specific domain recordset.</p>
getDomain :: forall eff. GetDomainRequest -> Aff (err :: AWS.RequestError | eff) GetDomainResult
getDomain = AWS.request serviceName "GetDomain" 


-- | <p>Returns a list of all domains in the user's account.</p>
getDomains :: forall eff. GetDomainsRequest -> Aff (err :: AWS.RequestError | eff) GetDomainsResult
getDomains = AWS.request serviceName "GetDomains" 


-- | <p>Returns information about a specific Amazon Lightsail instance, which is a virtual private server.</p>
getInstance :: forall eff. GetInstanceRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceResult
getInstance = AWS.request serviceName "GetInstance" 


-- | <p>Returns temporary SSH keys you can use to connect to a specific virtual private server, or <i>instance</i>.</p>
getInstanceAccessDetails :: forall eff. GetInstanceAccessDetailsRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceAccessDetailsResult
getInstanceAccessDetails = AWS.request serviceName "GetInstanceAccessDetails" 


-- | <p>Returns the data points for the specified Amazon Lightsail instance metric, given an instance name.</p>
getInstanceMetricData :: forall eff. GetInstanceMetricDataRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceMetricDataResult
getInstanceMetricData = AWS.request serviceName "GetInstanceMetricData" 


-- | <p>Returns the port states for a specific virtual private server, or <i>instance</i>.</p>
getInstancePortStates :: forall eff. GetInstancePortStatesRequest -> Aff (err :: AWS.RequestError | eff) GetInstancePortStatesResult
getInstancePortStates = AWS.request serviceName "GetInstancePortStates" 


-- | <p>Returns information about a specific instance snapshot.</p>
getInstanceSnapshot :: forall eff. GetInstanceSnapshotRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceSnapshotResult
getInstanceSnapshot = AWS.request serviceName "GetInstanceSnapshot" 


-- | <p>Returns all instance snapshots for the user's account.</p>
getInstanceSnapshots :: forall eff. GetInstanceSnapshotsRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceSnapshotsResult
getInstanceSnapshots = AWS.request serviceName "GetInstanceSnapshots" 


-- | <p>Returns the state of a specific instance. Works on one instance at a time.</p>
getInstanceState :: forall eff. GetInstanceStateRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceStateResult
getInstanceState = AWS.request serviceName "GetInstanceState" 


-- | <p>Returns information about all Amazon Lightsail virtual private servers, or <i>instances</i>.</p>
getInstances :: forall eff. GetInstancesRequest -> Aff (err :: AWS.RequestError | eff) GetInstancesResult
getInstances = AWS.request serviceName "GetInstances" 


-- | <p>Returns information about a specific key pair.</p>
getKeyPair :: forall eff. GetKeyPairRequest -> Aff (err :: AWS.RequestError | eff) GetKeyPairResult
getKeyPair = AWS.request serviceName "GetKeyPair" 


-- | <p>Returns information about all key pairs in the user's account.</p>
getKeyPairs :: forall eff. GetKeyPairsRequest -> Aff (err :: AWS.RequestError | eff) GetKeyPairsResult
getKeyPairs = AWS.request serviceName "GetKeyPairs" 


-- | <p>Returns information about the specified Lightsail load balancer.</p>
getLoadBalancer :: forall eff. GetLoadBalancerRequest -> Aff (err :: AWS.RequestError | eff) GetLoadBalancerResult
getLoadBalancer = AWS.request serviceName "GetLoadBalancer" 


-- | <p>Returns information about health metrics for your Lightsail load balancer.</p>
getLoadBalancerMetricData :: forall eff. GetLoadBalancerMetricDataRequest -> Aff (err :: AWS.RequestError | eff) GetLoadBalancerMetricDataResult
getLoadBalancerMetricData = AWS.request serviceName "GetLoadBalancerMetricData" 


-- | <p>Returns information about the TLS certificates that are associated with the specified Lightsail load balancer.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>
getLoadBalancerTlsCertificates :: forall eff. GetLoadBalancerTlsCertificatesRequest -> Aff (err :: AWS.RequestError | eff) GetLoadBalancerTlsCertificatesResult
getLoadBalancerTlsCertificates = AWS.request serviceName "GetLoadBalancerTlsCertificates" 


-- | <p>Returns information about all load balancers in an account.</p> <p>If you are describing a long list of load balancers, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.</p>
getLoadBalancers :: forall eff. GetLoadBalancersRequest -> Aff (err :: AWS.RequestError | eff) GetLoadBalancersResult
getLoadBalancers = AWS.request serviceName "GetLoadBalancers" 


-- | <p>Returns information about a specific operation. Operations include events such as when you create an instance, allocate a static IP, attach a static IP, and so on.</p>
getOperation :: forall eff. GetOperationRequest -> Aff (err :: AWS.RequestError | eff) GetOperationResult
getOperation = AWS.request serviceName "GetOperation" 


-- | <p>Returns information about all operations.</p> <p>Results are returned from oldest to newest, up to a maximum of 200. Results can be paged by making each subsequent call to <code>GetOperations</code> use the maximum (last) <code>statusChangedAt</code> value from the previous request.</p>
getOperations :: forall eff. GetOperationsRequest -> Aff (err :: AWS.RequestError | eff) GetOperationsResult
getOperations = AWS.request serviceName "GetOperations" 


-- | <p>Gets operations for a specific resource (e.g., an instance or a static IP).</p>
getOperationsForResource :: forall eff. GetOperationsForResourceRequest -> Aff (err :: AWS.RequestError | eff) GetOperationsForResourceResult
getOperationsForResource = AWS.request serviceName "GetOperationsForResource" 


-- | <p>Returns a list of all valid regions for Amazon Lightsail. Use the <code>include availability zones</code> parameter to also return the availability zones in a region.</p>
getRegions :: forall eff. GetRegionsRequest -> Aff (err :: AWS.RequestError | eff) GetRegionsResult
getRegions = AWS.request serviceName "GetRegions" 


-- | <p>Returns information about a specific static IP.</p>
getStaticIp :: forall eff. GetStaticIpRequest -> Aff (err :: AWS.RequestError | eff) GetStaticIpResult
getStaticIp = AWS.request serviceName "GetStaticIp" 


-- | <p>Returns information about all static IPs in the user's account.</p>
getStaticIps :: forall eff. GetStaticIpsRequest -> Aff (err :: AWS.RequestError | eff) GetStaticIpsResult
getStaticIps = AWS.request serviceName "GetStaticIps" 


-- | <p>Imports a public SSH key from a specific key pair.</p>
importKeyPair :: forall eff. ImportKeyPairRequest -> Aff (err :: AWS.RequestError | eff) ImportKeyPairResult
importKeyPair = AWS.request serviceName "ImportKeyPair" 


-- | <p>Returns a Boolean value indicating whether your Lightsail VPC is peered.</p>
isVpcPeered :: forall eff. IsVpcPeeredRequest -> Aff (err :: AWS.RequestError | eff) IsVpcPeeredResult
isVpcPeered = AWS.request serviceName "IsVpcPeered" 


-- | <p>Adds public ports to an Amazon Lightsail instance.</p>
openInstancePublicPorts :: forall eff. OpenInstancePublicPortsRequest -> Aff (err :: AWS.RequestError | eff) OpenInstancePublicPortsResult
openInstancePublicPorts = AWS.request serviceName "OpenInstancePublicPorts" 


-- | <p>Tries to peer the Lightsail VPC with the user's default VPC.</p>
peerVpc :: forall eff. PeerVpcRequest -> Aff (err :: AWS.RequestError | eff) PeerVpcResult
peerVpc = AWS.request serviceName "PeerVpc" 


-- | <p>Sets the specified open ports for an Amazon Lightsail instance, and closes all ports for every protocol not included in the current request.</p>
putInstancePublicPorts :: forall eff. PutInstancePublicPortsRequest -> Aff (err :: AWS.RequestError | eff) PutInstancePublicPortsResult
putInstancePublicPorts = AWS.request serviceName "PutInstancePublicPorts" 


-- | <p>Restarts a specific instance. When your Amazon Lightsail instance is finished rebooting, Lightsail assigns a new public IP address. To use the same IP address after restarting, create a static IP address and attach it to the instance.</p>
rebootInstance :: forall eff. RebootInstanceRequest -> Aff (err :: AWS.RequestError | eff) RebootInstanceResult
rebootInstance = AWS.request serviceName "RebootInstance" 


-- | <p>Deletes a specific static IP from your account.</p>
releaseStaticIp :: forall eff. ReleaseStaticIpRequest -> Aff (err :: AWS.RequestError | eff) ReleaseStaticIpResult
releaseStaticIp = AWS.request serviceName "ReleaseStaticIp" 


-- | <p>Starts a specific Amazon Lightsail instance from a stopped state. To restart an instance, use the reboot instance operation.</p>
startInstance :: forall eff. StartInstanceRequest -> Aff (err :: AWS.RequestError | eff) StartInstanceResult
startInstance = AWS.request serviceName "StartInstance" 


-- | <p>Stops a specific Amazon Lightsail instance that is currently running.</p>
stopInstance :: forall eff. StopInstanceRequest -> Aff (err :: AWS.RequestError | eff) StopInstanceResult
stopInstance = AWS.request serviceName "StopInstance" 


-- | <p>Attempts to unpeer the Lightsail VPC from the user's default VPC.</p>
unpeerVpc :: forall eff. UnpeerVpcRequest -> Aff (err :: AWS.RequestError | eff) UnpeerVpcResult
unpeerVpc = AWS.request serviceName "UnpeerVpc" 


-- | <p>Updates a domain recordset after it is created.</p>
updateDomainEntry :: forall eff. UpdateDomainEntryRequest -> Aff (err :: AWS.RequestError | eff) UpdateDomainEntryResult
updateDomainEntry = AWS.request serviceName "UpdateDomainEntry" 


-- | <p>Updates the specified attribute for a load balancer.</p>
updateLoadBalancerAttribute :: forall eff. UpdateLoadBalancerAttributeRequest -> Aff (err :: AWS.RequestError | eff) UpdateLoadBalancerAttributeResult
updateLoadBalancerAttribute = AWS.request serviceName "UpdateLoadBalancerAttribute" 


-- | <p>Lightsail throws this exception when the user cannot be authenticated or uses invalid credentials to access a resource.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }


newtype AccessDirection = AccessDirection String


-- | <p>Lightsail throws this exception when an account is still in the setup in progress state.</p>
newtype AccountSetupInProgressException = AccountSetupInProgressException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }


newtype AllocateStaticIpRequest = AllocateStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  }


newtype AllocateStaticIpResult = AllocateStaticIpResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype AttachDiskRequest = AttachDiskRequest 
  { "DiskName'" :: (ResourceName)
  , "InstanceName'" :: (ResourceName)
  , "DiskPath'" :: (NonEmptyString)
  }


newtype AttachDiskResult = AttachDiskResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype AttachInstancesToLoadBalancerRequest = AttachInstancesToLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "InstanceNames'" :: (ResourceNameList)
  }


newtype AttachInstancesToLoadBalancerResult = AttachInstancesToLoadBalancerResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype AttachLoadBalancerTlsCertificateRequest = AttachLoadBalancerTlsCertificateRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "CertificateName'" :: (ResourceName)
  }


newtype AttachLoadBalancerTlsCertificateResult = AttachLoadBalancerTlsCertificateResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype AttachStaticIpRequest = AttachStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  , "InstanceName'" :: (ResourceName)
  }


newtype AttachStaticIpResult = AttachStaticIpResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype AttachedDiskMap = AttachedDiskMap (Map ResourceName DiskMapList)


-- | <p>Describes an Availability Zone.</p>
newtype AvailabilityZone = AvailabilityZone 
  { "ZoneName'" :: NullOrUndefined (NonEmptyString)
  , "State'" :: NullOrUndefined (NonEmptyString)
  }


newtype AvailabilityZoneList = AvailabilityZoneList (Array AvailabilityZone)


newtype Base64 = Base64 String


-- | <p>Describes a blueprint (a virtual private server image).</p>
newtype Blueprint = Blueprint 
  { "BlueprintId'" :: NullOrUndefined (NonEmptyString)
  , "Name'" :: NullOrUndefined (ResourceName)
  , "Group'" :: NullOrUndefined (NonEmptyString)
  , "Type'" :: NullOrUndefined (BlueprintType)
  , "Description'" :: NullOrUndefined (String)
  , "IsActive'" :: NullOrUndefined (Boolean)
  , "MinPower'" :: NullOrUndefined (Int)
  , "Version'" :: NullOrUndefined (String)
  , "VersionCode'" :: NullOrUndefined (String)
  , "ProductUrl'" :: NullOrUndefined (String)
  , "LicenseUrl'" :: NullOrUndefined (String)
  , "Platform'" :: NullOrUndefined (InstancePlatform)
  }


newtype BlueprintList = BlueprintList (Array Blueprint)


newtype BlueprintType = BlueprintType String


-- | <p>Describes a bundle, which is a set of specs describing your virtual private server (or <i>instance</i>).</p>
newtype Bundle = Bundle 
  { "Price'" :: NullOrUndefined (Number)
  , "CpuCount'" :: NullOrUndefined (Int)
  , "DiskSizeInGb'" :: NullOrUndefined (Int)
  , "BundleId'" :: NullOrUndefined (NonEmptyString)
  , "InstanceType'" :: NullOrUndefined (String)
  , "IsActive'" :: NullOrUndefined (Boolean)
  , "Name'" :: NullOrUndefined (String)
  , "Power'" :: NullOrUndefined (Int)
  , "RamSizeInGb'" :: NullOrUndefined (Number)
  , "TransferPerMonthInGb'" :: NullOrUndefined (Int)
  , "SupportedPlatforms'" :: NullOrUndefined (InstancePlatformList)
  }


newtype BundleList = BundleList (Array Bundle)


newtype CloseInstancePublicPortsRequest = CloseInstancePublicPortsRequest 
  { "PortInfo'" :: (PortInfo)
  , "InstanceName'" :: (ResourceName)
  }


newtype CloseInstancePublicPortsResult = CloseInstancePublicPortsResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }


newtype CreateDiskFromSnapshotRequest = CreateDiskFromSnapshotRequest 
  { "DiskName'" :: (ResourceName)
  , "DiskSnapshotName'" :: (ResourceName)
  , "AvailabilityZone'" :: (NonEmptyString)
  , "SizeInGb'" :: (Int)
  }


newtype CreateDiskFromSnapshotResult = CreateDiskFromSnapshotResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype CreateDiskRequest = CreateDiskRequest 
  { "DiskName'" :: (ResourceName)
  , "AvailabilityZone'" :: (NonEmptyString)
  , "SizeInGb'" :: (Int)
  }


newtype CreateDiskResult = CreateDiskResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype CreateDiskSnapshotRequest = CreateDiskSnapshotRequest 
  { "DiskName'" :: (ResourceName)
  , "DiskSnapshotName'" :: (ResourceName)
  }


newtype CreateDiskSnapshotResult = CreateDiskSnapshotResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype CreateDomainEntryRequest = CreateDomainEntryRequest 
  { "DomainName'" :: (DomainName)
  , "DomainEntry'" :: (DomainEntry)
  }


newtype CreateDomainEntryResult = CreateDomainEntryResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }


newtype CreateDomainRequest = CreateDomainRequest 
  { "DomainName'" :: (DomainName)
  }


newtype CreateDomainResult = CreateDomainResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }


newtype CreateInstanceSnapshotRequest = CreateInstanceSnapshotRequest 
  { "InstanceSnapshotName'" :: (ResourceName)
  , "InstanceName'" :: (ResourceName)
  }


newtype CreateInstanceSnapshotResult = CreateInstanceSnapshotResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype CreateInstancesFromSnapshotRequest = CreateInstancesFromSnapshotRequest 
  { "InstanceNames'" :: (StringList)
  , "AttachedDiskMapping'" :: NullOrUndefined (AttachedDiskMap)
  , "AvailabilityZone'" :: (String)
  , "InstanceSnapshotName'" :: (ResourceName)
  , "BundleId'" :: (NonEmptyString)
  , "UserData'" :: NullOrUndefined (String)
  , "KeyPairName'" :: NullOrUndefined (ResourceName)
  }


newtype CreateInstancesFromSnapshotResult = CreateInstancesFromSnapshotResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype CreateInstancesRequest = CreateInstancesRequest 
  { "InstanceNames'" :: (StringList)
  , "AvailabilityZone'" :: (String)
  , "CustomImageName'" :: NullOrUndefined (ResourceName)
  , "BlueprintId'" :: (NonEmptyString)
  , "BundleId'" :: (NonEmptyString)
  , "UserData'" :: NullOrUndefined (String)
  , "KeyPairName'" :: NullOrUndefined (ResourceName)
  }


newtype CreateInstancesResult = CreateInstancesResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype CreateKeyPairRequest = CreateKeyPairRequest 
  { "KeyPairName'" :: (ResourceName)
  }


newtype CreateKeyPairResult = CreateKeyPairResult 
  { "KeyPair'" :: NullOrUndefined (KeyPair)
  , "PublicKeyBase64'" :: NullOrUndefined (Base64)
  , "PrivateKeyBase64'" :: NullOrUndefined (Base64)
  , "Operation'" :: NullOrUndefined (Operation)
  }


newtype CreateLoadBalancerRequest = CreateLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "InstancePort'" :: (Port)
  , "HealthCheckPath'" :: NullOrUndefined (String)
  , "CertificateName'" :: NullOrUndefined (ResourceName)
  , "CertificateDomainName'" :: NullOrUndefined (DomainName)
  , "CertificateAlternativeNames'" :: NullOrUndefined (DomainNameList)
  }


newtype CreateLoadBalancerResult = CreateLoadBalancerResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype CreateLoadBalancerTlsCertificateRequest = CreateLoadBalancerTlsCertificateRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "CertificateName'" :: (ResourceName)
  , "CertificateDomainName'" :: (DomainName)
  , "CertificateAlternativeNames'" :: NullOrUndefined (DomainNameList)
  }


newtype CreateLoadBalancerTlsCertificateResult = CreateLoadBalancerTlsCertificateResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype DeleteDiskRequest = DeleteDiskRequest 
  { "DiskName'" :: (ResourceName)
  }


newtype DeleteDiskResult = DeleteDiskResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype DeleteDiskSnapshotRequest = DeleteDiskSnapshotRequest 
  { "DiskSnapshotName'" :: (ResourceName)
  }


newtype DeleteDiskSnapshotResult = DeleteDiskSnapshotResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype DeleteDomainEntryRequest = DeleteDomainEntryRequest 
  { "DomainName'" :: (DomainName)
  , "DomainEntry'" :: (DomainEntry)
  }


newtype DeleteDomainEntryResult = DeleteDomainEntryResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }


newtype DeleteDomainRequest = DeleteDomainRequest 
  { "DomainName'" :: (DomainName)
  }


newtype DeleteDomainResult = DeleteDomainResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }


newtype DeleteInstanceRequest = DeleteInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  }


newtype DeleteInstanceResult = DeleteInstanceResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype DeleteInstanceSnapshotRequest = DeleteInstanceSnapshotRequest 
  { "InstanceSnapshotName'" :: (ResourceName)
  }


newtype DeleteInstanceSnapshotResult = DeleteInstanceSnapshotResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype DeleteKeyPairRequest = DeleteKeyPairRequest 
  { "KeyPairName'" :: (ResourceName)
  }


newtype DeleteKeyPairResult = DeleteKeyPairResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }


newtype DeleteLoadBalancerRequest = DeleteLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  }


newtype DeleteLoadBalancerResult = DeleteLoadBalancerResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype DeleteLoadBalancerTlsCertificateRequest = DeleteLoadBalancerTlsCertificateRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "CertificateName'" :: (ResourceName)
  , "Force'" :: NullOrUndefined (Boolean)
  }


newtype DeleteLoadBalancerTlsCertificateResult = DeleteLoadBalancerTlsCertificateResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype DetachDiskRequest = DetachDiskRequest 
  { "DiskName'" :: (ResourceName)
  }


newtype DetachDiskResult = DetachDiskResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype DetachInstancesFromLoadBalancerRequest = DetachInstancesFromLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "InstanceNames'" :: (ResourceNameList)
  }


newtype DetachInstancesFromLoadBalancerResult = DetachInstancesFromLoadBalancerResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype DetachStaticIpRequest = DetachStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  }


newtype DetachStaticIpResult = DetachStaticIpResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


-- | <p>Describes a system disk or an block storage disk.</p>
newtype Disk = Disk 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined (ResourceType)
  , "SizeInGb'" :: NullOrUndefined (Int)
  , "IsSystemDisk'" :: NullOrUndefined (Boolean)
  , "Iops'" :: NullOrUndefined (Int)
  , "Path'" :: NullOrUndefined (String)
  , "State'" :: NullOrUndefined (DiskState)
  , "AttachedTo'" :: NullOrUndefined (ResourceName)
  , "IsAttached'" :: NullOrUndefined (Boolean)
  , "AttachmentState'" :: NullOrUndefined (String)
  , "GbInUse'" :: NullOrUndefined (Int)
  }


newtype DiskList = DiskList (Array Disk)


-- | <p>Describes a block storage disk mapping.</p>
newtype DiskMap = DiskMap 
  { "OriginalDiskPath'" :: NullOrUndefined (NonEmptyString)
  , "NewDiskName'" :: NullOrUndefined (ResourceName)
  }


newtype DiskMapList = DiskMapList (Array DiskMap)


-- | <p>Describes a block storage disk snapshot.</p>
newtype DiskSnapshot = DiskSnapshot 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined (ResourceType)
  , "SizeInGb'" :: NullOrUndefined (Int)
  , "State'" :: NullOrUndefined (DiskSnapshotState)
  , "Progress'" :: NullOrUndefined (String)
  , "FromDiskName'" :: NullOrUndefined (ResourceName)
  , "FromDiskArn'" :: NullOrUndefined (NonEmptyString)
  }


newtype DiskSnapshotList = DiskSnapshotList (Array DiskSnapshot)


newtype DiskSnapshotState = DiskSnapshotState String


newtype DiskState = DiskState String


-- | <p>Describes a domain where you are storing recordsets in Lightsail.</p>
newtype Domain = Domain 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined (ResourceType)
  , "DomainEntries'" :: NullOrUndefined (DomainEntryList)
  }


-- | <p>Describes a domain recordset entry.</p>
newtype DomainEntry = DomainEntry 
  { "Id'" :: NullOrUndefined (NonEmptyString)
  , "Name'" :: NullOrUndefined (DomainName)
  , "Target'" :: NullOrUndefined (String)
  , "IsAlias'" :: NullOrUndefined (Boolean)
  , "Type'" :: NullOrUndefined (DomainEntryType)
  , "Options'" :: NullOrUndefined (DomainEntryOptions)
  }


newtype DomainEntryList = DomainEntryList (Array DomainEntry)


newtype DomainEntryOptions = DomainEntryOptions (Map DomainEntryOptionsKeys String)


newtype DomainEntryOptionsKeys = DomainEntryOptionsKeys String


newtype DomainEntryType = DomainEntryType String


newtype DomainList = DomainList (Array Domain)


newtype DomainName = DomainName String


newtype DomainNameList = DomainNameList (Array DomainName)


newtype DownloadDefaultKeyPairRequest = DownloadDefaultKeyPairRequest 
  { 
  }


newtype DownloadDefaultKeyPairResult = DownloadDefaultKeyPairResult 
  { "PublicKeyBase64'" :: NullOrUndefined (Base64)
  , "PrivateKeyBase64'" :: NullOrUndefined (Base64)
  }


newtype GetActiveNamesRequest = GetActiveNamesRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }


newtype GetActiveNamesResult = GetActiveNamesResult 
  { "ActiveNames'" :: NullOrUndefined (StringList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype GetBlueprintsRequest = GetBlueprintsRequest 
  { "IncludeInactive'" :: NullOrUndefined (Boolean)
  , "PageToken'" :: NullOrUndefined (String)
  }


newtype GetBlueprintsResult = GetBlueprintsResult 
  { "Blueprints'" :: NullOrUndefined (BlueprintList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype GetBundlesRequest = GetBundlesRequest 
  { "IncludeInactive'" :: NullOrUndefined (Boolean)
  , "PageToken'" :: NullOrUndefined (String)
  }


newtype GetBundlesResult = GetBundlesResult 
  { "Bundles'" :: NullOrUndefined (BundleList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype GetDiskRequest = GetDiskRequest 
  { "DiskName'" :: (ResourceName)
  }


newtype GetDiskResult = GetDiskResult 
  { "Disk'" :: NullOrUndefined (Disk)
  }


newtype GetDiskSnapshotRequest = GetDiskSnapshotRequest 
  { "DiskSnapshotName'" :: (ResourceName)
  }


newtype GetDiskSnapshotResult = GetDiskSnapshotResult 
  { "DiskSnapshot'" :: NullOrUndefined (DiskSnapshot)
  }


newtype GetDiskSnapshotsRequest = GetDiskSnapshotsRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }


newtype GetDiskSnapshotsResult = GetDiskSnapshotsResult 
  { "DiskSnapshots'" :: NullOrUndefined (DiskSnapshotList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype GetDisksRequest = GetDisksRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }


newtype GetDisksResult = GetDisksResult 
  { "Disks'" :: NullOrUndefined (DiskList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype GetDomainRequest = GetDomainRequest 
  { "DomainName'" :: (DomainName)
  }


newtype GetDomainResult = GetDomainResult 
  { "Domain'" :: NullOrUndefined (Domain)
  }


newtype GetDomainsRequest = GetDomainsRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }


newtype GetDomainsResult = GetDomainsResult 
  { "Domains'" :: NullOrUndefined (DomainList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype GetInstanceAccessDetailsRequest = GetInstanceAccessDetailsRequest 
  { "InstanceName'" :: (ResourceName)
  , "Protocol'" :: NullOrUndefined (InstanceAccessProtocol)
  }


newtype GetInstanceAccessDetailsResult = GetInstanceAccessDetailsResult 
  { "AccessDetails'" :: NullOrUndefined (InstanceAccessDetails)
  }


newtype GetInstanceMetricDataRequest = GetInstanceMetricDataRequest 
  { "InstanceName'" :: (ResourceName)
  , "MetricName'" :: (InstanceMetricName)
  , "Period'" :: (MetricPeriod)
  , "StartTime'" :: (Number)
  , "EndTime'" :: (Number)
  , "Unit''" :: (MetricUnit)
  , "Statistics'" :: (MetricStatisticList)
  }


newtype GetInstanceMetricDataResult = GetInstanceMetricDataResult 
  { "MetricName'" :: NullOrUndefined (InstanceMetricName)
  , "MetricData'" :: NullOrUndefined (MetricDatapointList)
  }


newtype GetInstancePortStatesRequest = GetInstancePortStatesRequest 
  { "InstanceName'" :: (ResourceName)
  }


newtype GetInstancePortStatesResult = GetInstancePortStatesResult 
  { "PortStates'" :: NullOrUndefined (InstancePortStateList)
  }


newtype GetInstanceRequest = GetInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  }


newtype GetInstanceResult = GetInstanceResult 
  { "Instance'" :: NullOrUndefined (Instance)
  }


newtype GetInstanceSnapshotRequest = GetInstanceSnapshotRequest 
  { "InstanceSnapshotName'" :: (ResourceName)
  }


newtype GetInstanceSnapshotResult = GetInstanceSnapshotResult 
  { "InstanceSnapshot'" :: NullOrUndefined (InstanceSnapshot)
  }


newtype GetInstanceSnapshotsRequest = GetInstanceSnapshotsRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }


newtype GetInstanceSnapshotsResult = GetInstanceSnapshotsResult 
  { "InstanceSnapshots'" :: NullOrUndefined (InstanceSnapshotList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype GetInstanceStateRequest = GetInstanceStateRequest 
  { "InstanceName'" :: (ResourceName)
  }


newtype GetInstanceStateResult = GetInstanceStateResult 
  { "State'" :: NullOrUndefined (InstanceState)
  }


newtype GetInstancesRequest = GetInstancesRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }


newtype GetInstancesResult = GetInstancesResult 
  { "Instances'" :: NullOrUndefined (InstanceList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype GetKeyPairRequest = GetKeyPairRequest 
  { "KeyPairName'" :: (ResourceName)
  }


newtype GetKeyPairResult = GetKeyPairResult 
  { "KeyPair'" :: NullOrUndefined (KeyPair)
  }


newtype GetKeyPairsRequest = GetKeyPairsRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }


newtype GetKeyPairsResult = GetKeyPairsResult 
  { "KeyPairs'" :: NullOrUndefined (KeyPairList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype GetLoadBalancerMetricDataRequest = GetLoadBalancerMetricDataRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "MetricName'" :: (LoadBalancerMetricName)
  , "Period'" :: (MetricPeriod)
  , "StartTime'" :: (Number)
  , "EndTime'" :: (Number)
  , "Unit''" :: (MetricUnit)
  , "Statistics'" :: (MetricStatisticList)
  }


newtype GetLoadBalancerMetricDataResult = GetLoadBalancerMetricDataResult 
  { "MetricName'" :: NullOrUndefined (LoadBalancerMetricName)
  , "MetricData'" :: NullOrUndefined (MetricDatapointList)
  }


newtype GetLoadBalancerRequest = GetLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  }


newtype GetLoadBalancerResult = GetLoadBalancerResult 
  { "LoadBalancer'" :: NullOrUndefined (LoadBalancer)
  }


newtype GetLoadBalancerTlsCertificatesRequest = GetLoadBalancerTlsCertificatesRequest 
  { "LoadBalancerName'" :: (ResourceName)
  }


newtype GetLoadBalancerTlsCertificatesResult = GetLoadBalancerTlsCertificatesResult 
  { "TlsCertificates'" :: NullOrUndefined (LoadBalancerTlsCertificateList)
  }


newtype GetLoadBalancersRequest = GetLoadBalancersRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }


newtype GetLoadBalancersResult = GetLoadBalancersResult 
  { "LoadBalancers'" :: NullOrUndefined (LoadBalancerList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype GetOperationRequest = GetOperationRequest 
  { "OperationId'" :: (NonEmptyString)
  }


newtype GetOperationResult = GetOperationResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }


newtype GetOperationsForResourceRequest = GetOperationsForResourceRequest 
  { "ResourceName'" :: (ResourceName)
  , "PageToken'" :: NullOrUndefined (String)
  }


newtype GetOperationsForResourceResult = GetOperationsForResourceResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  , "NextPageCount'" :: NullOrUndefined (String)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype GetOperationsRequest = GetOperationsRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }


newtype GetOperationsResult = GetOperationsResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype GetRegionsRequest = GetRegionsRequest 
  { "IncludeAvailabilityZones'" :: NullOrUndefined (Boolean)
  }


newtype GetRegionsResult = GetRegionsResult 
  { "Regions'" :: NullOrUndefined (RegionList)
  }


newtype GetStaticIpRequest = GetStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  }


newtype GetStaticIpResult = GetStaticIpResult 
  { "StaticIp'" :: NullOrUndefined (StaticIp)
  }


newtype GetStaticIpsRequest = GetStaticIpsRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }


newtype GetStaticIpsResult = GetStaticIpsResult 
  { "StaticIps'" :: NullOrUndefined (StaticIpList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }


newtype ImportKeyPairRequest = ImportKeyPairRequest 
  { "KeyPairName'" :: (ResourceName)
  , "PublicKeyBase64'" :: (Base64)
  }


newtype ImportKeyPairResult = ImportKeyPairResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }


-- | <p>Describes an instance (a virtual private server).</p>
newtype Instance = Instance 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined (ResourceType)
  , "BlueprintId'" :: NullOrUndefined (NonEmptyString)
  , "BlueprintName'" :: NullOrUndefined (NonEmptyString)
  , "BundleId'" :: NullOrUndefined (NonEmptyString)
  , "IsStaticIp'" :: NullOrUndefined (Boolean)
  , "PrivateIpAddress'" :: NullOrUndefined (IpAddress)
  , "PublicIpAddress'" :: NullOrUndefined (IpAddress)
  , "Ipv6Address'" :: NullOrUndefined (IpV6Address)
  , "Hardware'" :: NullOrUndefined (InstanceHardware)
  , "Networking'" :: NullOrUndefined (InstanceNetworking)
  , "State'" :: NullOrUndefined (InstanceState)
  , "Username'" :: NullOrUndefined (NonEmptyString)
  , "SshKeyName'" :: NullOrUndefined (ResourceName)
  }


-- | <p>The parameters for gaining temporary access to one of your Amazon Lightsail instances.</p>
newtype InstanceAccessDetails = InstanceAccessDetails 
  { "CertKey'" :: NullOrUndefined (String)
  , "ExpiresAt'" :: NullOrUndefined (IsoDate)
  , "IpAddress'" :: NullOrUndefined (IpAddress)
  , "Password'" :: NullOrUndefined (String)
  , "PasswordData'" :: NullOrUndefined (PasswordData)
  , "PrivateKey'" :: NullOrUndefined (String)
  , "Protocol'" :: NullOrUndefined (InstanceAccessProtocol)
  , "InstanceName'" :: NullOrUndefined (ResourceName)
  , "Username'" :: NullOrUndefined (String)
  }


newtype InstanceAccessProtocol = InstanceAccessProtocol String


-- | <p>Describes the hardware for the instance.</p>
newtype InstanceHardware = InstanceHardware 
  { "CpuCount'" :: NullOrUndefined (Int)
  , "Disks'" :: NullOrUndefined (DiskList)
  , "RamSizeInGb'" :: NullOrUndefined (Number)
  }


newtype InstanceHealthReason = InstanceHealthReason String


newtype InstanceHealthState = InstanceHealthState String


-- | <p>Describes information about the health of the instance.</p>
newtype InstanceHealthSummary = InstanceHealthSummary 
  { "InstanceName'" :: NullOrUndefined (ResourceName)
  , "InstanceHealth'" :: NullOrUndefined (InstanceHealthState)
  , "InstanceHealthReason'" :: NullOrUndefined (InstanceHealthReason)
  }


newtype InstanceHealthSummaryList = InstanceHealthSummaryList (Array InstanceHealthSummary)


newtype InstanceList = InstanceList (Array Instance)


newtype InstanceMetricName = InstanceMetricName String


-- | <p>Describes monthly data transfer rates and port information for an instance.</p>
newtype InstanceNetworking = InstanceNetworking 
  { "MonthlyTransfer'" :: NullOrUndefined (MonthlyTransfer)
  , "Ports'" :: NullOrUndefined (InstancePortInfoList)
  }


newtype InstancePlatform = InstancePlatform String


newtype InstancePlatformList = InstancePlatformList (Array InstancePlatform)


-- | <p>Describes information about the instance ports.</p>
newtype InstancePortInfo = InstancePortInfo 
  { "FromPort'" :: NullOrUndefined (Port)
  , "ToPort'" :: NullOrUndefined (Port)
  , "Protocol'" :: NullOrUndefined (NetworkProtocol)
  , "AccessFrom'" :: NullOrUndefined (String)
  , "AccessType'" :: NullOrUndefined (PortAccessType)
  , "CommonName'" :: NullOrUndefined (String)
  , "AccessDirection'" :: NullOrUndefined (AccessDirection)
  }


newtype InstancePortInfoList = InstancePortInfoList (Array InstancePortInfo)


-- | <p>Describes the port state.</p>
newtype InstancePortState = InstancePortState 
  { "FromPort'" :: NullOrUndefined (Port)
  , "ToPort'" :: NullOrUndefined (Port)
  , "Protocol'" :: NullOrUndefined (NetworkProtocol)
  , "State'" :: NullOrUndefined (PortState)
  }


newtype InstancePortStateList = InstancePortStateList (Array InstancePortState)


-- | <p>Describes the snapshot of the virtual private server, or <i>instance</i>.</p>
newtype InstanceSnapshot = InstanceSnapshot 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined (ResourceType)
  , "State'" :: NullOrUndefined (InstanceSnapshotState)
  , "Progress'" :: NullOrUndefined (String)
  , "FromAttachedDisks'" :: NullOrUndefined (DiskList)
  , "FromInstanceName'" :: NullOrUndefined (ResourceName)
  , "FromInstanceArn'" :: NullOrUndefined (NonEmptyString)
  , "FromBlueprintId'" :: NullOrUndefined (String)
  , "FromBundleId'" :: NullOrUndefined (String)
  , "SizeInGb'" :: NullOrUndefined (Int)
  }


newtype InstanceSnapshotList = InstanceSnapshotList (Array InstanceSnapshot)


newtype InstanceSnapshotState = InstanceSnapshotState String


-- | <p>Describes the virtual private server (or <i>instance</i>) status.</p>
newtype InstanceState = InstanceState 
  { "Code'" :: NullOrUndefined (Int)
  , "Name'" :: NullOrUndefined (String)
  }


-- | <p>Lightsail throws this exception when user input does not conform to the validation rules of an input field.</p> <note> <p>Domain-related APIs are only available in the N. Virginia (us-east-1) Region. Please set your Region configuration to us-east-1 to create, view, or edit these resources.</p> </note>
newtype InvalidInputException = InvalidInputException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }


newtype IpAddress = IpAddress String


newtype IpV6Address = IpV6Address String


newtype IsVpcPeeredRequest = IsVpcPeeredRequest 
  { 
  }


newtype IsVpcPeeredResult = IsVpcPeeredResult 
  { "IsPeered'" :: NullOrUndefined (Boolean)
  }


newtype IsoDate = IsoDate Number


-- | <p>Describes the SSH key pair.</p>
newtype KeyPair = KeyPair 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined (ResourceType)
  , "Fingerprint'" :: NullOrUndefined (Base64)
  }


newtype KeyPairList = KeyPairList (Array KeyPair)


-- | <p>Describes the Lightsail load balancer.</p>
newtype LoadBalancer = LoadBalancer 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined (ResourceType)
  , "DnsName'" :: NullOrUndefined (NonEmptyString)
  , "State'" :: NullOrUndefined (LoadBalancerState)
  , "Protocol'" :: NullOrUndefined (LoadBalancerProtocol)
  , "PublicPorts'" :: NullOrUndefined (PortList)
  , "HealthCheckPath'" :: NullOrUndefined (NonEmptyString)
  , "InstancePort'" :: NullOrUndefined (Int)
  , "InstanceHealthSummary'" :: NullOrUndefined (InstanceHealthSummaryList)
  , "TlsCertificateSummaries'" :: NullOrUndefined (LoadBalancerTlsCertificateSummaryList)
  , "ConfigurationOptions'" :: NullOrUndefined (LoadBalancerConfigurationOptions)
  }


newtype LoadBalancerAttributeName = LoadBalancerAttributeName String


newtype LoadBalancerConfigurationOptions = LoadBalancerConfigurationOptions (Map LoadBalancerAttributeName String)


newtype LoadBalancerList = LoadBalancerList (Array LoadBalancer)


newtype LoadBalancerMetricName = LoadBalancerMetricName String


newtype LoadBalancerProtocol = LoadBalancerProtocol String


newtype LoadBalancerState = LoadBalancerState String


-- | <p>Describes a load balancer TLS/SSL certificate.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>
newtype LoadBalancerTlsCertificate = LoadBalancerTlsCertificate 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined (ResourceType)
  , "LoadBalancerName'" :: NullOrUndefined (ResourceName)
  , "IsAttached'" :: NullOrUndefined (Boolean)
  , "Status'" :: NullOrUndefined (LoadBalancerTlsCertificateStatus)
  , "DomainName'" :: NullOrUndefined (DomainName)
  , "DomainValidationRecords'" :: NullOrUndefined (LoadBalancerTlsCertificateDomainValidationRecordList)
  , "FailureReason'" :: NullOrUndefined (LoadBalancerTlsCertificateFailureReason)
  , "IssuedAt'" :: NullOrUndefined (IsoDate)
  , "Issuer'" :: NullOrUndefined (NonEmptyString)
  , "KeyAlgorithm'" :: NullOrUndefined (NonEmptyString)
  , "NotAfter'" :: NullOrUndefined (IsoDate)
  , "NotBefore'" :: NullOrUndefined (IsoDate)
  , "RenewalSummary'" :: NullOrUndefined (LoadBalancerTlsCertificateRenewalSummary)
  , "RevocationReason'" :: NullOrUndefined (LoadBalancerTlsCertificateRevocationReason)
  , "RevokedAt'" :: NullOrUndefined (IsoDate)
  , "Serial'" :: NullOrUndefined (NonEmptyString)
  , "SignatureAlgorithm'" :: NullOrUndefined (NonEmptyString)
  , "Subject'" :: NullOrUndefined (NonEmptyString)
  , "SubjectAlternativeNames'" :: NullOrUndefined (StringList)
  }


newtype LoadBalancerTlsCertificateDomainStatus = LoadBalancerTlsCertificateDomainStatus String


-- | <p>Contains information about the domain names on a TLS/SSL certificate that you will use to validate domain ownership.</p>
newtype LoadBalancerTlsCertificateDomainValidationOption = LoadBalancerTlsCertificateDomainValidationOption 
  { "DomainName'" :: NullOrUndefined (DomainName)
  , "ValidationStatus'" :: NullOrUndefined (LoadBalancerTlsCertificateDomainStatus)
  }


newtype LoadBalancerTlsCertificateDomainValidationOptionList = LoadBalancerTlsCertificateDomainValidationOptionList (Array LoadBalancerTlsCertificateDomainValidationOption)


-- | <p>Describes the validation record of each domain name in the TLS/SSL certificate.</p>
newtype LoadBalancerTlsCertificateDomainValidationRecord = LoadBalancerTlsCertificateDomainValidationRecord 
  { "Name'" :: NullOrUndefined (NonEmptyString)
  , "Type'" :: NullOrUndefined (NonEmptyString)
  , "Value'" :: NullOrUndefined (NonEmptyString)
  , "ValidationStatus'" :: NullOrUndefined (LoadBalancerTlsCertificateDomainStatus)
  , "DomainName'" :: NullOrUndefined (DomainName)
  }


newtype LoadBalancerTlsCertificateDomainValidationRecordList = LoadBalancerTlsCertificateDomainValidationRecordList (Array LoadBalancerTlsCertificateDomainValidationRecord)


newtype LoadBalancerTlsCertificateFailureReason = LoadBalancerTlsCertificateFailureReason String


newtype LoadBalancerTlsCertificateList = LoadBalancerTlsCertificateList (Array LoadBalancerTlsCertificate)


newtype LoadBalancerTlsCertificateRenewalStatus = LoadBalancerTlsCertificateRenewalStatus String


-- | <p>Contains information about the status of Lightsail's managed renewal for the certificate.</p>
newtype LoadBalancerTlsCertificateRenewalSummary = LoadBalancerTlsCertificateRenewalSummary 
  { "RenewalStatus'" :: NullOrUndefined (LoadBalancerTlsCertificateRenewalStatus)
  , "DomainValidationOptions'" :: NullOrUndefined (LoadBalancerTlsCertificateDomainValidationOptionList)
  }


newtype LoadBalancerTlsCertificateRevocationReason = LoadBalancerTlsCertificateRevocationReason String


newtype LoadBalancerTlsCertificateStatus = LoadBalancerTlsCertificateStatus String


-- | <p>Provides a summary of TLS/SSL certificate metadata.</p>
newtype LoadBalancerTlsCertificateSummary = LoadBalancerTlsCertificateSummary 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "IsAttached'" :: NullOrUndefined (Boolean)
  }


newtype LoadBalancerTlsCertificateSummaryList = LoadBalancerTlsCertificateSummaryList (Array LoadBalancerTlsCertificateSummary)


-- | <p>Describes the metric data point.</p>
newtype MetricDatapoint = MetricDatapoint 
  { "Average'" :: NullOrUndefined (Number)
  , "Maximum'" :: NullOrUndefined (Number)
  , "Minimum'" :: NullOrUndefined (Number)
  , "SampleCount'" :: NullOrUndefined (Number)
  , "Sum'" :: NullOrUndefined (Number)
  , "Number" :: NullOrUndefined (Number)
  , "Unit''" :: NullOrUndefined (MetricUnit)
  }


newtype MetricDatapointList = MetricDatapointList (Array MetricDatapoint)


newtype MetricPeriod = MetricPeriod Int


newtype MetricStatistic = MetricStatistic String


newtype MetricStatisticList = MetricStatisticList (Array MetricStatistic)


newtype MetricUnit = MetricUnit String


-- | <p>Describes the monthly data transfer in and out of your virtual private server (or <i>instance</i>).</p>
newtype MonthlyTransfer = MonthlyTransfer 
  { "GbPerMonthAllocated'" :: NullOrUndefined (Int)
  }


newtype NetworkProtocol = NetworkProtocol String


newtype NonEmptyString = NonEmptyString String


-- | <p>Lightsail throws this exception when it cannot find a resource.</p>
newtype NotFoundException = NotFoundException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }


newtype OpenInstancePublicPortsRequest = OpenInstancePublicPortsRequest 
  { "PortInfo'" :: (PortInfo)
  , "InstanceName'" :: (ResourceName)
  }


newtype OpenInstancePublicPortsResult = OpenInstancePublicPortsResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }


-- | <p>Describes the API operation.</p>
newtype Operation = Operation 
  { "Id'" :: NullOrUndefined (NonEmptyString)
  , "ResourceName'" :: NullOrUndefined (ResourceName)
  , "ResourceType'" :: NullOrUndefined (ResourceType)
  , "CreatedAt'" :: NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined (ResourceLocation)
  , "IsTerminal'" :: NullOrUndefined (Boolean)
  , "OperationDetails'" :: NullOrUndefined (String)
  , "OperationType'" :: NullOrUndefined (OperationType)
  , "Status'" :: NullOrUndefined (OperationStatus)
  , "StatusChangedAt'" :: NullOrUndefined (IsoDate)
  , "ErrorCode'" :: NullOrUndefined (String)
  , "ErrorDetails'" :: NullOrUndefined (String)
  }


-- | <p>Lightsail throws this exception when an operation fails to execute.</p>
newtype OperationFailureException = OperationFailureException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }


newtype OperationList = OperationList (Array Operation)


newtype OperationStatus = OperationStatus String


newtype OperationType = OperationType String


-- | <p>The password data for the Windows Server-based instance, including the ciphertext and the key pair name.</p>
newtype PasswordData = PasswordData 
  { "Ciphertext'" :: NullOrUndefined (String)
  , "KeyPairName'" :: NullOrUndefined (ResourceName)
  }


newtype PeerVpcRequest = PeerVpcRequest 
  { 
  }


newtype PeerVpcResult = PeerVpcResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }


newtype Port = Port Int


newtype PortAccessType = PortAccessType String


-- | <p>Describes information about the ports on your virtual private server (or <i>instance</i>).</p>
newtype PortInfo = PortInfo 
  { "FromPort'" :: NullOrUndefined (Port)
  , "ToPort'" :: NullOrUndefined (Port)
  , "Protocol'" :: NullOrUndefined (NetworkProtocol)
  }


newtype PortInfoList = PortInfoList (Array PortInfo)


newtype PortList = PortList (Array Port)


newtype PortState = PortState String


newtype PutInstancePublicPortsRequest = PutInstancePublicPortsRequest 
  { "PortInfos'" :: (PortInfoList)
  , "InstanceName'" :: (ResourceName)
  }


newtype PutInstancePublicPortsResult = PutInstancePublicPortsResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }


newtype RebootInstanceRequest = RebootInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  }


newtype RebootInstanceResult = RebootInstanceResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


-- | <p>Describes the AWS Region.</p>
newtype Region = Region 
  { "ContinentCode'" :: NullOrUndefined (String)
  , "Description'" :: NullOrUndefined (String)
  , "DisplayName'" :: NullOrUndefined (String)
  , "Name'" :: NullOrUndefined (RegionName)
  , "AvailabilityZones'" :: NullOrUndefined (AvailabilityZoneList)
  }


newtype RegionList = RegionList (Array Region)


newtype RegionName = RegionName String


newtype ReleaseStaticIpRequest = ReleaseStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  }


newtype ReleaseStaticIpResult = ReleaseStaticIpResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


-- | <p>Describes the resource location.</p>
newtype ResourceLocation = ResourceLocation 
  { "AvailabilityZone'" :: NullOrUndefined (String)
  , "RegionName'" :: NullOrUndefined (RegionName)
  }


newtype ResourceName = ResourceName String


newtype ResourceNameList = ResourceNameList (Array ResourceName)


newtype ResourceType = ResourceType String


-- | <p>A general service exception.</p>
newtype ServiceException = ServiceException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }


newtype StartInstanceRequest = StartInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  }


newtype StartInstanceResult = StartInstanceResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


-- | <p>Describes the static IP.</p>
newtype StaticIp = StaticIp 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined (ResourceType)
  , "IpAddress'" :: NullOrUndefined (IpAddress)
  , "AttachedTo'" :: NullOrUndefined (ResourceName)
  , "IsAttached'" :: NullOrUndefined (Boolean)
  }


newtype StaticIpList = StaticIpList (Array StaticIp)


newtype StopInstanceRequest = StopInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  , "Force'" :: NullOrUndefined (Boolean)
  }


newtype StopInstanceResult = StopInstanceResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype StringList = StringList (Array String)


newtype StringMax256 = StringMax256 String


-- | <p>Lightsail throws this exception when the user has not been authenticated.</p>
newtype UnauthenticatedException = UnauthenticatedException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }


newtype UnpeerVpcRequest = UnpeerVpcRequest 
  { 
  }


newtype UnpeerVpcResult = UnpeerVpcResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }


newtype UpdateDomainEntryRequest = UpdateDomainEntryRequest 
  { "DomainName'" :: (DomainName)
  , "DomainEntry'" :: (DomainEntry)
  }


newtype UpdateDomainEntryResult = UpdateDomainEntryResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }


newtype UpdateLoadBalancerAttributeRequest = UpdateLoadBalancerAttributeRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "AttributeName'" :: (LoadBalancerAttributeName)
  , "AttributeValue'" :: (StringMax256)
  }


newtype UpdateLoadBalancerAttributeResult = UpdateLoadBalancerAttributeResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
