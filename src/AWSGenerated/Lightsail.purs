

-- | <p>Amazon Lightsail is the easiest way to get started with AWS for developers who just need virtual private servers. Lightsail includes everything you need to launch your project quickly - a virtual machine, SSD-based storage, data transfer, DNS management, and a static IP - for a low, predictable price. You manage those Lightsail servers through the Lightsail console or by using the API or command-line interface (CLI).</p> <p>For more information about Lightsail concepts and tasks, see the <a href="https://lightsail.aws.amazon.com/ls/docs/all">Lightsail Dev Guide</a>.</p> <p>To use the Lightsail API or the CLI, you will need to use AWS Identity and Access Management (IAM) to generate access keys. For details about how to set this up, see the <a href="http://lightsail.aws.amazon.com/ls/docs/how-to/article/lightsail-how-to-set-up-access-keys-to-use-sdk-api-cli">Lightsail Dev Guide</a>.</p>
module AWS.Lightsail where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Lightsail" :: String


-- | <p>Allocates a static IP address.</p>
allocateStaticIp :: forall eff. AllocateStaticIpRequest -> Aff (err :: AWS.RequestError | eff) AllocateStaticIpResult
allocateStaticIp = AWS.request serviceName "allocateStaticIp" 


-- | <p>Attaches a block storage disk to a running or stopped Lightsail instance and exposes it to the instance with the specified disk name.</p>
attachDisk :: forall eff. AttachDiskRequest -> Aff (err :: AWS.RequestError | eff) AttachDiskResult
attachDisk = AWS.request serviceName "attachDisk" 


-- | <p>Attaches one or more Lightsail instances to a load balancer.</p>
attachInstancesToLoadBalancer :: forall eff. AttachInstancesToLoadBalancerRequest -> Aff (err :: AWS.RequestError | eff) AttachInstancesToLoadBalancerResult
attachInstancesToLoadBalancer = AWS.request serviceName "attachInstancesToLoadBalancer" 


-- | <p>Attaches a Transport Layer Security (TLS) certificate to your load balancer.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>
attachLoadBalancerTlsCertificate :: forall eff. AttachLoadBalancerTlsCertificateRequest -> Aff (err :: AWS.RequestError | eff) AttachLoadBalancerTlsCertificateResult
attachLoadBalancerTlsCertificate = AWS.request serviceName "attachLoadBalancerTlsCertificate" 


-- | <p>Attaches a static IP address to a specific Amazon Lightsail instance.</p>
attachStaticIp :: forall eff. AttachStaticIpRequest -> Aff (err :: AWS.RequestError | eff) AttachStaticIpResult
attachStaticIp = AWS.request serviceName "attachStaticIp" 


-- | <p>Closes the public ports on a specific Amazon Lightsail instance.</p>
closeInstancePublicPorts :: forall eff. CloseInstancePublicPortsRequest -> Aff (err :: AWS.RequestError | eff) CloseInstancePublicPortsResult
closeInstancePublicPorts = AWS.request serviceName "closeInstancePublicPorts" 


-- | <p>Creates a block storage disk that can be attached to a Lightsail instance in the same Availability Zone (e.g., <code>us-east-2a</code>). The disk is created in the regional endpoint that you send the HTTP request to. For more information, see <a href="https://lightsail.aws.amazon.com/ls/docs/overview/article/understanding-regions-and-availability-zones-in-amazon-lightsail">Regions and Availability Zones in Lightsail</a>.</p>
createDisk :: forall eff. CreateDiskRequest -> Aff (err :: AWS.RequestError | eff) CreateDiskResult
createDisk = AWS.request serviceName "createDisk" 


-- | <p>Creates a block storage disk from a disk snapshot that can be attached to a Lightsail instance in the same Availability Zone (e.g., <code>us-east-2a</code>). The disk is created in the regional endpoint that you send the HTTP request to. For more information, see <a href="https://lightsail.aws.amazon.com/ls/docs/overview/article/understanding-regions-and-availability-zones-in-amazon-lightsail">Regions and Availability Zones in Lightsail</a>.</p>
createDiskFromSnapshot :: forall eff. CreateDiskFromSnapshotRequest -> Aff (err :: AWS.RequestError | eff) CreateDiskFromSnapshotResult
createDiskFromSnapshot = AWS.request serviceName "createDiskFromSnapshot" 


-- | <p>Creates a snapshot of a block storage disk. You can use snapshots for backups, to make copies of disks, and to save data before shutting down a Lightsail instance.</p> <p>You can take a snapshot of an attached disk that is in use; however, snapshots only capture data that has been written to your disk at the time the snapshot command is issued. This may exclude any data that has been cached by any applications or the operating system. If you can pause any file systems on the disk long enough to take a snapshot, your snapshot should be complete. Nevertheless, if you cannot pause all file writes to the disk, you should unmount the disk from within the Lightsail instance, issue the create disk snapshot command, and then remount the disk to ensure a consistent and complete snapshot. You may remount and use your disk while the snapshot status is pending.</p>
createDiskSnapshot :: forall eff. CreateDiskSnapshotRequest -> Aff (err :: AWS.RequestError | eff) CreateDiskSnapshotResult
createDiskSnapshot = AWS.request serviceName "createDiskSnapshot" 


-- | <p>Creates a domain resource for the specified domain (e.g., example.com).</p>
createDomain :: forall eff. CreateDomainRequest -> Aff (err :: AWS.RequestError | eff) CreateDomainResult
createDomain = AWS.request serviceName "createDomain" 


-- | <p>Creates one of the following entry records associated with the domain: A record, CNAME record, TXT record, or MX record.</p>
createDomainEntry :: forall eff. CreateDomainEntryRequest -> Aff (err :: AWS.RequestError | eff) CreateDomainEntryResult
createDomainEntry = AWS.request serviceName "createDomainEntry" 


-- | <p>Creates a snapshot of a specific virtual private server, or <i>instance</i>. You can use a snapshot to create a new instance that is based on that snapshot.</p>
createInstanceSnapshot :: forall eff. CreateInstanceSnapshotRequest -> Aff (err :: AWS.RequestError | eff) CreateInstanceSnapshotResult
createInstanceSnapshot = AWS.request serviceName "createInstanceSnapshot" 


-- | <p>Creates one or more Amazon Lightsail virtual private servers, or <i>instances</i>.</p>
createInstances :: forall eff. CreateInstancesRequest -> Aff (err :: AWS.RequestError | eff) CreateInstancesResult
createInstances = AWS.request serviceName "createInstances" 


-- | <p>Uses a specific snapshot as a blueprint for creating one or more new instances that are based on that identical configuration.</p>
createInstancesFromSnapshot :: forall eff. CreateInstancesFromSnapshotRequest -> Aff (err :: AWS.RequestError | eff) CreateInstancesFromSnapshotResult
createInstancesFromSnapshot = AWS.request serviceName "createInstancesFromSnapshot" 


-- | <p>Creates sn SSH key pair.</p>
createKeyPair :: forall eff. CreateKeyPairRequest -> Aff (err :: AWS.RequestError | eff) CreateKeyPairResult
createKeyPair = AWS.request serviceName "createKeyPair" 


-- | <p>Creates a Lightsail load balancer.</p> <p>When you create a load balancer, you can specify certificates and port settings. You can create up to 5 load balancers per AWS Region in your account.</p>
createLoadBalancer :: forall eff. CreateLoadBalancerRequest -> Aff (err :: AWS.RequestError | eff) CreateLoadBalancerResult
createLoadBalancer = AWS.request serviceName "createLoadBalancer" 


-- | <p>Creates a Lightsail load balancer TLS certificate.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>
createLoadBalancerTlsCertificate :: forall eff. CreateLoadBalancerTlsCertificateRequest -> Aff (err :: AWS.RequestError | eff) CreateLoadBalancerTlsCertificateResult
createLoadBalancerTlsCertificate = AWS.request serviceName "createLoadBalancerTlsCertificate" 


-- | <p>Deletes the specified block storage disk. The disk must be in the <code>available</code> state (not attached to a Lightsail instance).</p> <note> <p>The disk may remain in the <code>deleting</code> state for several minutes.</p> </note>
deleteDisk :: forall eff. DeleteDiskRequest -> Aff (err :: AWS.RequestError | eff) DeleteDiskResult
deleteDisk = AWS.request serviceName "deleteDisk" 


-- | <p>Deletes the specified disk snapshot.</p> <p>When you make periodic snapshots of a disk, the snapshots are incremental, and only the blocks on the device that have changed since your last snapshot are saved in the new snapshot. When you delete a snapshot, only the data not needed for any other snapshot is removed. So regardless of which prior snapshots have been deleted, all active snapshots will have access to all the information needed to restore the disk.</p>
deleteDiskSnapshot :: forall eff. DeleteDiskSnapshotRequest -> Aff (err :: AWS.RequestError | eff) DeleteDiskSnapshotResult
deleteDiskSnapshot = AWS.request serviceName "deleteDiskSnapshot" 


-- | <p>Deletes the specified domain recordset and all of its domain records.</p>
deleteDomain :: forall eff. DeleteDomainRequest -> Aff (err :: AWS.RequestError | eff) DeleteDomainResult
deleteDomain = AWS.request serviceName "deleteDomain" 


-- | <p>Deletes a specific domain entry.</p>
deleteDomainEntry :: forall eff. DeleteDomainEntryRequest -> Aff (err :: AWS.RequestError | eff) DeleteDomainEntryResult
deleteDomainEntry = AWS.request serviceName "deleteDomainEntry" 


-- | <p>Deletes a specific Amazon Lightsail virtual private server, or <i>instance</i>.</p>
deleteInstance :: forall eff. DeleteInstanceRequest -> Aff (err :: AWS.RequestError | eff) DeleteInstanceResult
deleteInstance = AWS.request serviceName "deleteInstance" 


-- | <p>Deletes a specific snapshot of a virtual private server (or <i>instance</i>).</p>
deleteInstanceSnapshot :: forall eff. DeleteInstanceSnapshotRequest -> Aff (err :: AWS.RequestError | eff) DeleteInstanceSnapshotResult
deleteInstanceSnapshot = AWS.request serviceName "deleteInstanceSnapshot" 


-- | <p>Deletes a specific SSH key pair.</p>
deleteKeyPair :: forall eff. DeleteKeyPairRequest -> Aff (err :: AWS.RequestError | eff) DeleteKeyPairResult
deleteKeyPair = AWS.request serviceName "deleteKeyPair" 


-- | <p>Deletes a Lightsail load balancer.</p>
deleteLoadBalancer :: forall eff. DeleteLoadBalancerRequest -> Aff (err :: AWS.RequestError | eff) DeleteLoadBalancerResult
deleteLoadBalancer = AWS.request serviceName "deleteLoadBalancer" 


-- | <p>Deletes a TLS/SSL certificate associated with a Lightsail load balancer.</p>
deleteLoadBalancerTlsCertificate :: forall eff. DeleteLoadBalancerTlsCertificateRequest -> Aff (err :: AWS.RequestError | eff) DeleteLoadBalancerTlsCertificateResult
deleteLoadBalancerTlsCertificate = AWS.request serviceName "deleteLoadBalancerTlsCertificate" 


-- | <p>Detaches a stopped block storage disk from a Lightsail instance. Make sure to unmount any file systems on the device within your operating system before stopping the instance and detaching the disk.</p>
detachDisk :: forall eff. DetachDiskRequest -> Aff (err :: AWS.RequestError | eff) DetachDiskResult
detachDisk = AWS.request serviceName "detachDisk" 


-- | <p>Detaches the specified instances from a Lightsail load balancer.</p>
detachInstancesFromLoadBalancer :: forall eff. DetachInstancesFromLoadBalancerRequest -> Aff (err :: AWS.RequestError | eff) DetachInstancesFromLoadBalancerResult
detachInstancesFromLoadBalancer = AWS.request serviceName "detachInstancesFromLoadBalancer" 


-- | <p>Detaches a static IP from the Amazon Lightsail instance to which it is attached.</p>
detachStaticIp :: forall eff. DetachStaticIpRequest -> Aff (err :: AWS.RequestError | eff) DetachStaticIpResult
detachStaticIp = AWS.request serviceName "detachStaticIp" 


-- | <p>Downloads the default SSH key pair from the user's account.</p>
downloadDefaultKeyPair :: forall eff. DownloadDefaultKeyPairRequest -> Aff (err :: AWS.RequestError | eff) DownloadDefaultKeyPairResult
downloadDefaultKeyPair = AWS.request serviceName "downloadDefaultKeyPair" 


-- | <p>Returns the names of all active (not deleted) resources.</p>
getActiveNames :: forall eff. GetActiveNamesRequest -> Aff (err :: AWS.RequestError | eff) GetActiveNamesResult
getActiveNames = AWS.request serviceName "getActiveNames" 


-- | <p>Returns the list of available instance images, or <i>blueprints</i>. You can use a blueprint to create a new virtual private server already running a specific operating system, as well as a preinstalled app or development stack. The software each instance is running depends on the blueprint image you choose.</p>
getBlueprints :: forall eff. GetBlueprintsRequest -> Aff (err :: AWS.RequestError | eff) GetBlueprintsResult
getBlueprints = AWS.request serviceName "getBlueprints" 


-- | <p>Returns the list of bundles that are available for purchase. A bundle describes the specs for your virtual private server (or <i>instance</i>).</p>
getBundles :: forall eff. GetBundlesRequest -> Aff (err :: AWS.RequestError | eff) GetBundlesResult
getBundles = AWS.request serviceName "getBundles" 


-- | <p>Returns information about a specific block storage disk.</p>
getDisk :: forall eff. GetDiskRequest -> Aff (err :: AWS.RequestError | eff) GetDiskResult
getDisk = AWS.request serviceName "getDisk" 


-- | <p>Returns information about a specific block storage disk snapshot.</p>
getDiskSnapshot :: forall eff. GetDiskSnapshotRequest -> Aff (err :: AWS.RequestError | eff) GetDiskSnapshotResult
getDiskSnapshot = AWS.request serviceName "getDiskSnapshot" 


-- | <p>Returns information about all block storage disk snapshots in your AWS account and region.</p> <p>If you are describing a long list of disk snapshots, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.</p>
getDiskSnapshots :: forall eff. GetDiskSnapshotsRequest -> Aff (err :: AWS.RequestError | eff) GetDiskSnapshotsResult
getDiskSnapshots = AWS.request serviceName "getDiskSnapshots" 


-- | <p>Returns information about all block storage disks in your AWS account and region.</p> <p>If you are describing a long list of disks, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.</p>
getDisks :: forall eff. GetDisksRequest -> Aff (err :: AWS.RequestError | eff) GetDisksResult
getDisks = AWS.request serviceName "getDisks" 


-- | <p>Returns information about a specific domain recordset.</p>
getDomain :: forall eff. GetDomainRequest -> Aff (err :: AWS.RequestError | eff) GetDomainResult
getDomain = AWS.request serviceName "getDomain" 


-- | <p>Returns a list of all domains in the user's account.</p>
getDomains :: forall eff. GetDomainsRequest -> Aff (err :: AWS.RequestError | eff) GetDomainsResult
getDomains = AWS.request serviceName "getDomains" 


-- | <p>Returns information about a specific Amazon Lightsail instance, which is a virtual private server.</p>
getInstance :: forall eff. GetInstanceRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceResult
getInstance = AWS.request serviceName "getInstance" 


-- | <p>Returns temporary SSH keys you can use to connect to a specific virtual private server, or <i>instance</i>.</p>
getInstanceAccessDetails :: forall eff. GetInstanceAccessDetailsRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceAccessDetailsResult
getInstanceAccessDetails = AWS.request serviceName "getInstanceAccessDetails" 


-- | <p>Returns the data points for the specified Amazon Lightsail instance metric, given an instance name.</p>
getInstanceMetricData :: forall eff. GetInstanceMetricDataRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceMetricDataResult
getInstanceMetricData = AWS.request serviceName "getInstanceMetricData" 


-- | <p>Returns the port states for a specific virtual private server, or <i>instance</i>.</p>
getInstancePortStates :: forall eff. GetInstancePortStatesRequest -> Aff (err :: AWS.RequestError | eff) GetInstancePortStatesResult
getInstancePortStates = AWS.request serviceName "getInstancePortStates" 


-- | <p>Returns information about a specific instance snapshot.</p>
getInstanceSnapshot :: forall eff. GetInstanceSnapshotRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceSnapshotResult
getInstanceSnapshot = AWS.request serviceName "getInstanceSnapshot" 


-- | <p>Returns all instance snapshots for the user's account.</p>
getInstanceSnapshots :: forall eff. GetInstanceSnapshotsRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceSnapshotsResult
getInstanceSnapshots = AWS.request serviceName "getInstanceSnapshots" 


-- | <p>Returns the state of a specific instance. Works on one instance at a time.</p>
getInstanceState :: forall eff. GetInstanceStateRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceStateResult
getInstanceState = AWS.request serviceName "getInstanceState" 


-- | <p>Returns information about all Amazon Lightsail virtual private servers, or <i>instances</i>.</p>
getInstances :: forall eff. GetInstancesRequest -> Aff (err :: AWS.RequestError | eff) GetInstancesResult
getInstances = AWS.request serviceName "getInstances" 


-- | <p>Returns information about a specific key pair.</p>
getKeyPair :: forall eff. GetKeyPairRequest -> Aff (err :: AWS.RequestError | eff) GetKeyPairResult
getKeyPair = AWS.request serviceName "getKeyPair" 


-- | <p>Returns information about all key pairs in the user's account.</p>
getKeyPairs :: forall eff. GetKeyPairsRequest -> Aff (err :: AWS.RequestError | eff) GetKeyPairsResult
getKeyPairs = AWS.request serviceName "getKeyPairs" 


-- | <p>Returns information about the specified Lightsail load balancer.</p>
getLoadBalancer :: forall eff. GetLoadBalancerRequest -> Aff (err :: AWS.RequestError | eff) GetLoadBalancerResult
getLoadBalancer = AWS.request serviceName "getLoadBalancer" 


-- | <p>Returns information about health metrics for your Lightsail load balancer.</p>
getLoadBalancerMetricData :: forall eff. GetLoadBalancerMetricDataRequest -> Aff (err :: AWS.RequestError | eff) GetLoadBalancerMetricDataResult
getLoadBalancerMetricData = AWS.request serviceName "getLoadBalancerMetricData" 


-- | <p>Returns information about the TLS certificates that are associated with the specified Lightsail load balancer.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>
getLoadBalancerTlsCertificates :: forall eff. GetLoadBalancerTlsCertificatesRequest -> Aff (err :: AWS.RequestError | eff) GetLoadBalancerTlsCertificatesResult
getLoadBalancerTlsCertificates = AWS.request serviceName "getLoadBalancerTlsCertificates" 


-- | <p>Returns information about all load balancers in an account.</p> <p>If you are describing a long list of load balancers, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.</p>
getLoadBalancers :: forall eff. GetLoadBalancersRequest -> Aff (err :: AWS.RequestError | eff) GetLoadBalancersResult
getLoadBalancers = AWS.request serviceName "getLoadBalancers" 


-- | <p>Returns information about a specific operation. Operations include events such as when you create an instance, allocate a static IP, attach a static IP, and so on.</p>
getOperation :: forall eff. GetOperationRequest -> Aff (err :: AWS.RequestError | eff) GetOperationResult
getOperation = AWS.request serviceName "getOperation" 


-- | <p>Returns information about all operations.</p> <p>Results are returned from oldest to newest, up to a maximum of 200. Results can be paged by making each subsequent call to <code>GetOperations</code> use the maximum (last) <code>statusChangedAt</code> value from the previous request.</p>
getOperations :: forall eff. GetOperationsRequest -> Aff (err :: AWS.RequestError | eff) GetOperationsResult
getOperations = AWS.request serviceName "getOperations" 


-- | <p>Gets operations for a specific resource (e.g., an instance or a static IP).</p>
getOperationsForResource :: forall eff. GetOperationsForResourceRequest -> Aff (err :: AWS.RequestError | eff) GetOperationsForResourceResult
getOperationsForResource = AWS.request serviceName "getOperationsForResource" 


-- | <p>Returns a list of all valid regions for Amazon Lightsail. Use the <code>include availability zones</code> parameter to also return the availability zones in a region.</p>
getRegions :: forall eff. GetRegionsRequest -> Aff (err :: AWS.RequestError | eff) GetRegionsResult
getRegions = AWS.request serviceName "getRegions" 


-- | <p>Returns information about a specific static IP.</p>
getStaticIp :: forall eff. GetStaticIpRequest -> Aff (err :: AWS.RequestError | eff) GetStaticIpResult
getStaticIp = AWS.request serviceName "getStaticIp" 


-- | <p>Returns information about all static IPs in the user's account.</p>
getStaticIps :: forall eff. GetStaticIpsRequest -> Aff (err :: AWS.RequestError | eff) GetStaticIpsResult
getStaticIps = AWS.request serviceName "getStaticIps" 


-- | <p>Imports a public SSH key from a specific key pair.</p>
importKeyPair :: forall eff. ImportKeyPairRequest -> Aff (err :: AWS.RequestError | eff) ImportKeyPairResult
importKeyPair = AWS.request serviceName "importKeyPair" 


-- | <p>Returns a Boolean value indicating whether your Lightsail VPC is peered.</p>
isVpcPeered :: forall eff. IsVpcPeeredRequest -> Aff (err :: AWS.RequestError | eff) IsVpcPeeredResult
isVpcPeered = AWS.request serviceName "isVpcPeered" 


-- | <p>Adds public ports to an Amazon Lightsail instance.</p>
openInstancePublicPorts :: forall eff. OpenInstancePublicPortsRequest -> Aff (err :: AWS.RequestError | eff) OpenInstancePublicPortsResult
openInstancePublicPorts = AWS.request serviceName "openInstancePublicPorts" 


-- | <p>Tries to peer the Lightsail VPC with the user's default VPC.</p>
peerVpc :: forall eff. PeerVpcRequest -> Aff (err :: AWS.RequestError | eff) PeerVpcResult
peerVpc = AWS.request serviceName "peerVpc" 


-- | <p>Sets the specified open ports for an Amazon Lightsail instance, and closes all ports for every protocol not included in the current request.</p>
putInstancePublicPorts :: forall eff. PutInstancePublicPortsRequest -> Aff (err :: AWS.RequestError | eff) PutInstancePublicPortsResult
putInstancePublicPorts = AWS.request serviceName "putInstancePublicPorts" 


-- | <p>Restarts a specific instance. When your Amazon Lightsail instance is finished rebooting, Lightsail assigns a new public IP address. To use the same IP address after restarting, create a static IP address and attach it to the instance.</p>
rebootInstance :: forall eff. RebootInstanceRequest -> Aff (err :: AWS.RequestError | eff) RebootInstanceResult
rebootInstance = AWS.request serviceName "rebootInstance" 


-- | <p>Deletes a specific static IP from your account.</p>
releaseStaticIp :: forall eff. ReleaseStaticIpRequest -> Aff (err :: AWS.RequestError | eff) ReleaseStaticIpResult
releaseStaticIp = AWS.request serviceName "releaseStaticIp" 


-- | <p>Starts a specific Amazon Lightsail instance from a stopped state. To restart an instance, use the reboot instance operation.</p>
startInstance :: forall eff. StartInstanceRequest -> Aff (err :: AWS.RequestError | eff) StartInstanceResult
startInstance = AWS.request serviceName "startInstance" 


-- | <p>Stops a specific Amazon Lightsail instance that is currently running.</p>
stopInstance :: forall eff. StopInstanceRequest -> Aff (err :: AWS.RequestError | eff) StopInstanceResult
stopInstance = AWS.request serviceName "stopInstance" 


-- | <p>Attempts to unpeer the Lightsail VPC from the user's default VPC.</p>
unpeerVpc :: forall eff. UnpeerVpcRequest -> Aff (err :: AWS.RequestError | eff) UnpeerVpcResult
unpeerVpc = AWS.request serviceName "unpeerVpc" 


-- | <p>Updates a domain recordset after it is created.</p>
updateDomainEntry :: forall eff. UpdateDomainEntryRequest -> Aff (err :: AWS.RequestError | eff) UpdateDomainEntryResult
updateDomainEntry = AWS.request serviceName "updateDomainEntry" 


-- | <p>Updates the specified attribute for a load balancer.</p>
updateLoadBalancerAttribute :: forall eff. UpdateLoadBalancerAttributeRequest -> Aff (err :: AWS.RequestError | eff) UpdateLoadBalancerAttributeResult
updateLoadBalancerAttribute = AWS.request serviceName "updateLoadBalancerAttribute" 


-- | <p>Lightsail throws this exception when the user cannot be authenticated or uses invalid credentials to access a resource.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _


newtype AccessDirection = AccessDirection String
derive instance newtypeAccessDirection :: Newtype AccessDirection _


-- | <p>Lightsail throws this exception when an account is still in the setup in progress state.</p>
newtype AccountSetupInProgressException = AccountSetupInProgressException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }
derive instance newtypeAccountSetupInProgressException :: Newtype AccountSetupInProgressException _


newtype AllocateStaticIpRequest = AllocateStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  }
derive instance newtypeAllocateStaticIpRequest :: Newtype AllocateStaticIpRequest _


newtype AllocateStaticIpResult = AllocateStaticIpResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeAllocateStaticIpResult :: Newtype AllocateStaticIpResult _


newtype AttachDiskRequest = AttachDiskRequest 
  { "DiskName'" :: (ResourceName)
  , "InstanceName'" :: (ResourceName)
  , "DiskPath'" :: (NonEmptyString)
  }
derive instance newtypeAttachDiskRequest :: Newtype AttachDiskRequest _


newtype AttachDiskResult = AttachDiskResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeAttachDiskResult :: Newtype AttachDiskResult _


newtype AttachInstancesToLoadBalancerRequest = AttachInstancesToLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "InstanceNames'" :: (ResourceNameList)
  }
derive instance newtypeAttachInstancesToLoadBalancerRequest :: Newtype AttachInstancesToLoadBalancerRequest _


newtype AttachInstancesToLoadBalancerResult = AttachInstancesToLoadBalancerResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeAttachInstancesToLoadBalancerResult :: Newtype AttachInstancesToLoadBalancerResult _


newtype AttachLoadBalancerTlsCertificateRequest = AttachLoadBalancerTlsCertificateRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "CertificateName'" :: (ResourceName)
  }
derive instance newtypeAttachLoadBalancerTlsCertificateRequest :: Newtype AttachLoadBalancerTlsCertificateRequest _


newtype AttachLoadBalancerTlsCertificateResult = AttachLoadBalancerTlsCertificateResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeAttachLoadBalancerTlsCertificateResult :: Newtype AttachLoadBalancerTlsCertificateResult _


newtype AttachStaticIpRequest = AttachStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  , "InstanceName'" :: (ResourceName)
  }
derive instance newtypeAttachStaticIpRequest :: Newtype AttachStaticIpRequest _


newtype AttachStaticIpResult = AttachStaticIpResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeAttachStaticIpResult :: Newtype AttachStaticIpResult _


newtype AttachedDiskMap = AttachedDiskMap (Map ResourceName DiskMapList)
derive instance newtypeAttachedDiskMap :: Newtype AttachedDiskMap _


-- | <p>Describes an Availability Zone.</p>
newtype AvailabilityZone = AvailabilityZone 
  { "ZoneName'" :: NullOrUndefined (NonEmptyString)
  , "State'" :: NullOrUndefined (NonEmptyString)
  }
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _


newtype AvailabilityZoneList = AvailabilityZoneList (Array AvailabilityZone)
derive instance newtypeAvailabilityZoneList :: Newtype AvailabilityZoneList _


newtype Base64 = Base64 String
derive instance newtypeBase64 :: Newtype Base64 _


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
derive instance newtypeBlueprint :: Newtype Blueprint _


newtype BlueprintList = BlueprintList (Array Blueprint)
derive instance newtypeBlueprintList :: Newtype BlueprintList _


newtype BlueprintType = BlueprintType String
derive instance newtypeBlueprintType :: Newtype BlueprintType _


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
derive instance newtypeBundle :: Newtype Bundle _


newtype BundleList = BundleList (Array Bundle)
derive instance newtypeBundleList :: Newtype BundleList _


newtype CloseInstancePublicPortsRequest = CloseInstancePublicPortsRequest 
  { "PortInfo'" :: (PortInfo)
  , "InstanceName'" :: (ResourceName)
  }
derive instance newtypeCloseInstancePublicPortsRequest :: Newtype CloseInstancePublicPortsRequest _


newtype CloseInstancePublicPortsResult = CloseInstancePublicPortsResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypeCloseInstancePublicPortsResult :: Newtype CloseInstancePublicPortsResult _


newtype CreateDiskFromSnapshotRequest = CreateDiskFromSnapshotRequest 
  { "DiskName'" :: (ResourceName)
  , "DiskSnapshotName'" :: (ResourceName)
  , "AvailabilityZone'" :: (NonEmptyString)
  , "SizeInGb'" :: (Int)
  }
derive instance newtypeCreateDiskFromSnapshotRequest :: Newtype CreateDiskFromSnapshotRequest _


newtype CreateDiskFromSnapshotResult = CreateDiskFromSnapshotResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeCreateDiskFromSnapshotResult :: Newtype CreateDiskFromSnapshotResult _


newtype CreateDiskRequest = CreateDiskRequest 
  { "DiskName'" :: (ResourceName)
  , "AvailabilityZone'" :: (NonEmptyString)
  , "SizeInGb'" :: (Int)
  }
derive instance newtypeCreateDiskRequest :: Newtype CreateDiskRequest _


newtype CreateDiskResult = CreateDiskResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeCreateDiskResult :: Newtype CreateDiskResult _


newtype CreateDiskSnapshotRequest = CreateDiskSnapshotRequest 
  { "DiskName'" :: (ResourceName)
  , "DiskSnapshotName'" :: (ResourceName)
  }
derive instance newtypeCreateDiskSnapshotRequest :: Newtype CreateDiskSnapshotRequest _


newtype CreateDiskSnapshotResult = CreateDiskSnapshotResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeCreateDiskSnapshotResult :: Newtype CreateDiskSnapshotResult _


newtype CreateDomainEntryRequest = CreateDomainEntryRequest 
  { "DomainName'" :: (DomainName)
  , "DomainEntry'" :: (DomainEntry)
  }
derive instance newtypeCreateDomainEntryRequest :: Newtype CreateDomainEntryRequest _


newtype CreateDomainEntryResult = CreateDomainEntryResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypeCreateDomainEntryResult :: Newtype CreateDomainEntryResult _


newtype CreateDomainRequest = CreateDomainRequest 
  { "DomainName'" :: (DomainName)
  }
derive instance newtypeCreateDomainRequest :: Newtype CreateDomainRequest _


newtype CreateDomainResult = CreateDomainResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypeCreateDomainResult :: Newtype CreateDomainResult _


newtype CreateInstanceSnapshotRequest = CreateInstanceSnapshotRequest 
  { "InstanceSnapshotName'" :: (ResourceName)
  , "InstanceName'" :: (ResourceName)
  }
derive instance newtypeCreateInstanceSnapshotRequest :: Newtype CreateInstanceSnapshotRequest _


newtype CreateInstanceSnapshotResult = CreateInstanceSnapshotResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeCreateInstanceSnapshotResult :: Newtype CreateInstanceSnapshotResult _


newtype CreateInstancesFromSnapshotRequest = CreateInstancesFromSnapshotRequest 
  { "InstanceNames'" :: (StringList)
  , "AttachedDiskMapping'" :: NullOrUndefined (AttachedDiskMap)
  , "AvailabilityZone'" :: (String)
  , "InstanceSnapshotName'" :: (ResourceName)
  , "BundleId'" :: (NonEmptyString)
  , "UserData'" :: NullOrUndefined (String)
  , "KeyPairName'" :: NullOrUndefined (ResourceName)
  }
derive instance newtypeCreateInstancesFromSnapshotRequest :: Newtype CreateInstancesFromSnapshotRequest _


newtype CreateInstancesFromSnapshotResult = CreateInstancesFromSnapshotResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeCreateInstancesFromSnapshotResult :: Newtype CreateInstancesFromSnapshotResult _


newtype CreateInstancesRequest = CreateInstancesRequest 
  { "InstanceNames'" :: (StringList)
  , "AvailabilityZone'" :: (String)
  , "CustomImageName'" :: NullOrUndefined (ResourceName)
  , "BlueprintId'" :: (NonEmptyString)
  , "BundleId'" :: (NonEmptyString)
  , "UserData'" :: NullOrUndefined (String)
  , "KeyPairName'" :: NullOrUndefined (ResourceName)
  }
derive instance newtypeCreateInstancesRequest :: Newtype CreateInstancesRequest _


newtype CreateInstancesResult = CreateInstancesResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeCreateInstancesResult :: Newtype CreateInstancesResult _


newtype CreateKeyPairRequest = CreateKeyPairRequest 
  { "KeyPairName'" :: (ResourceName)
  }
derive instance newtypeCreateKeyPairRequest :: Newtype CreateKeyPairRequest _


newtype CreateKeyPairResult = CreateKeyPairResult 
  { "KeyPair'" :: NullOrUndefined (KeyPair)
  , "PublicKeyBase64'" :: NullOrUndefined (Base64)
  , "PrivateKeyBase64'" :: NullOrUndefined (Base64)
  , "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypeCreateKeyPairResult :: Newtype CreateKeyPairResult _


newtype CreateLoadBalancerRequest = CreateLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "InstancePort'" :: (Port)
  , "HealthCheckPath'" :: NullOrUndefined (String)
  , "CertificateName'" :: NullOrUndefined (ResourceName)
  , "CertificateDomainName'" :: NullOrUndefined (DomainName)
  , "CertificateAlternativeNames'" :: NullOrUndefined (DomainNameList)
  }
derive instance newtypeCreateLoadBalancerRequest :: Newtype CreateLoadBalancerRequest _


newtype CreateLoadBalancerResult = CreateLoadBalancerResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeCreateLoadBalancerResult :: Newtype CreateLoadBalancerResult _


newtype CreateLoadBalancerTlsCertificateRequest = CreateLoadBalancerTlsCertificateRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "CertificateName'" :: (ResourceName)
  , "CertificateDomainName'" :: (DomainName)
  , "CertificateAlternativeNames'" :: NullOrUndefined (DomainNameList)
  }
derive instance newtypeCreateLoadBalancerTlsCertificateRequest :: Newtype CreateLoadBalancerTlsCertificateRequest _


newtype CreateLoadBalancerTlsCertificateResult = CreateLoadBalancerTlsCertificateResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeCreateLoadBalancerTlsCertificateResult :: Newtype CreateLoadBalancerTlsCertificateResult _


newtype DeleteDiskRequest = DeleteDiskRequest 
  { "DiskName'" :: (ResourceName)
  }
derive instance newtypeDeleteDiskRequest :: Newtype DeleteDiskRequest _


newtype DeleteDiskResult = DeleteDiskResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeDeleteDiskResult :: Newtype DeleteDiskResult _


newtype DeleteDiskSnapshotRequest = DeleteDiskSnapshotRequest 
  { "DiskSnapshotName'" :: (ResourceName)
  }
derive instance newtypeDeleteDiskSnapshotRequest :: Newtype DeleteDiskSnapshotRequest _


newtype DeleteDiskSnapshotResult = DeleteDiskSnapshotResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeDeleteDiskSnapshotResult :: Newtype DeleteDiskSnapshotResult _


newtype DeleteDomainEntryRequest = DeleteDomainEntryRequest 
  { "DomainName'" :: (DomainName)
  , "DomainEntry'" :: (DomainEntry)
  }
derive instance newtypeDeleteDomainEntryRequest :: Newtype DeleteDomainEntryRequest _


newtype DeleteDomainEntryResult = DeleteDomainEntryResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypeDeleteDomainEntryResult :: Newtype DeleteDomainEntryResult _


newtype DeleteDomainRequest = DeleteDomainRequest 
  { "DomainName'" :: (DomainName)
  }
derive instance newtypeDeleteDomainRequest :: Newtype DeleteDomainRequest _


newtype DeleteDomainResult = DeleteDomainResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypeDeleteDomainResult :: Newtype DeleteDomainResult _


newtype DeleteInstanceRequest = DeleteInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  }
derive instance newtypeDeleteInstanceRequest :: Newtype DeleteInstanceRequest _


newtype DeleteInstanceResult = DeleteInstanceResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeDeleteInstanceResult :: Newtype DeleteInstanceResult _


newtype DeleteInstanceSnapshotRequest = DeleteInstanceSnapshotRequest 
  { "InstanceSnapshotName'" :: (ResourceName)
  }
derive instance newtypeDeleteInstanceSnapshotRequest :: Newtype DeleteInstanceSnapshotRequest _


newtype DeleteInstanceSnapshotResult = DeleteInstanceSnapshotResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeDeleteInstanceSnapshotResult :: Newtype DeleteInstanceSnapshotResult _


newtype DeleteKeyPairRequest = DeleteKeyPairRequest 
  { "KeyPairName'" :: (ResourceName)
  }
derive instance newtypeDeleteKeyPairRequest :: Newtype DeleteKeyPairRequest _


newtype DeleteKeyPairResult = DeleteKeyPairResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypeDeleteKeyPairResult :: Newtype DeleteKeyPairResult _


newtype DeleteLoadBalancerRequest = DeleteLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  }
derive instance newtypeDeleteLoadBalancerRequest :: Newtype DeleteLoadBalancerRequest _


newtype DeleteLoadBalancerResult = DeleteLoadBalancerResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeDeleteLoadBalancerResult :: Newtype DeleteLoadBalancerResult _


newtype DeleteLoadBalancerTlsCertificateRequest = DeleteLoadBalancerTlsCertificateRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "CertificateName'" :: (ResourceName)
  , "Force'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeDeleteLoadBalancerTlsCertificateRequest :: Newtype DeleteLoadBalancerTlsCertificateRequest _


newtype DeleteLoadBalancerTlsCertificateResult = DeleteLoadBalancerTlsCertificateResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeDeleteLoadBalancerTlsCertificateResult :: Newtype DeleteLoadBalancerTlsCertificateResult _


newtype DetachDiskRequest = DetachDiskRequest 
  { "DiskName'" :: (ResourceName)
  }
derive instance newtypeDetachDiskRequest :: Newtype DetachDiskRequest _


newtype DetachDiskResult = DetachDiskResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeDetachDiskResult :: Newtype DetachDiskResult _


newtype DetachInstancesFromLoadBalancerRequest = DetachInstancesFromLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "InstanceNames'" :: (ResourceNameList)
  }
derive instance newtypeDetachInstancesFromLoadBalancerRequest :: Newtype DetachInstancesFromLoadBalancerRequest _


newtype DetachInstancesFromLoadBalancerResult = DetachInstancesFromLoadBalancerResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeDetachInstancesFromLoadBalancerResult :: Newtype DetachInstancesFromLoadBalancerResult _


newtype DetachStaticIpRequest = DetachStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  }
derive instance newtypeDetachStaticIpRequest :: Newtype DetachStaticIpRequest _


newtype DetachStaticIpResult = DetachStaticIpResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeDetachStaticIpResult :: Newtype DetachStaticIpResult _


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
derive instance newtypeDisk :: Newtype Disk _


newtype DiskList = DiskList (Array Disk)
derive instance newtypeDiskList :: Newtype DiskList _


-- | <p>Describes a block storage disk mapping.</p>
newtype DiskMap = DiskMap 
  { "OriginalDiskPath'" :: NullOrUndefined (NonEmptyString)
  , "NewDiskName'" :: NullOrUndefined (ResourceName)
  }
derive instance newtypeDiskMap :: Newtype DiskMap _


newtype DiskMapList = DiskMapList (Array DiskMap)
derive instance newtypeDiskMapList :: Newtype DiskMapList _


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
derive instance newtypeDiskSnapshot :: Newtype DiskSnapshot _


newtype DiskSnapshotList = DiskSnapshotList (Array DiskSnapshot)
derive instance newtypeDiskSnapshotList :: Newtype DiskSnapshotList _


newtype DiskSnapshotState = DiskSnapshotState String
derive instance newtypeDiskSnapshotState :: Newtype DiskSnapshotState _


newtype DiskState = DiskState String
derive instance newtypeDiskState :: Newtype DiskState _


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
derive instance newtypeDomain :: Newtype Domain _


-- | <p>Describes a domain recordset entry.</p>
newtype DomainEntry = DomainEntry 
  { "Id'" :: NullOrUndefined (NonEmptyString)
  , "Name'" :: NullOrUndefined (DomainName)
  , "Target'" :: NullOrUndefined (String)
  , "IsAlias'" :: NullOrUndefined (Boolean)
  , "Type'" :: NullOrUndefined (DomainEntryType)
  , "Options'" :: NullOrUndefined (DomainEntryOptions)
  }
derive instance newtypeDomainEntry :: Newtype DomainEntry _


newtype DomainEntryList = DomainEntryList (Array DomainEntry)
derive instance newtypeDomainEntryList :: Newtype DomainEntryList _


newtype DomainEntryOptions = DomainEntryOptions (Map DomainEntryOptionsKeys String)
derive instance newtypeDomainEntryOptions :: Newtype DomainEntryOptions _


newtype DomainEntryOptionsKeys = DomainEntryOptionsKeys String
derive instance newtypeDomainEntryOptionsKeys :: Newtype DomainEntryOptionsKeys _


newtype DomainEntryType = DomainEntryType String
derive instance newtypeDomainEntryType :: Newtype DomainEntryType _


newtype DomainList = DomainList (Array Domain)
derive instance newtypeDomainList :: Newtype DomainList _


newtype DomainName = DomainName String
derive instance newtypeDomainName :: Newtype DomainName _


newtype DomainNameList = DomainNameList (Array DomainName)
derive instance newtypeDomainNameList :: Newtype DomainNameList _


newtype DownloadDefaultKeyPairRequest = DownloadDefaultKeyPairRequest 
  { 
  }
derive instance newtypeDownloadDefaultKeyPairRequest :: Newtype DownloadDefaultKeyPairRequest _


newtype DownloadDefaultKeyPairResult = DownloadDefaultKeyPairResult 
  { "PublicKeyBase64'" :: NullOrUndefined (Base64)
  , "PrivateKeyBase64'" :: NullOrUndefined (Base64)
  }
derive instance newtypeDownloadDefaultKeyPairResult :: Newtype DownloadDefaultKeyPairResult _


newtype GetActiveNamesRequest = GetActiveNamesRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetActiveNamesRequest :: Newtype GetActiveNamesRequest _


newtype GetActiveNamesResult = GetActiveNamesResult 
  { "ActiveNames'" :: NullOrUndefined (StringList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetActiveNamesResult :: Newtype GetActiveNamesResult _


newtype GetBlueprintsRequest = GetBlueprintsRequest 
  { "IncludeInactive'" :: NullOrUndefined (Boolean)
  , "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetBlueprintsRequest :: Newtype GetBlueprintsRequest _


newtype GetBlueprintsResult = GetBlueprintsResult 
  { "Blueprints'" :: NullOrUndefined (BlueprintList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetBlueprintsResult :: Newtype GetBlueprintsResult _


newtype GetBundlesRequest = GetBundlesRequest 
  { "IncludeInactive'" :: NullOrUndefined (Boolean)
  , "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetBundlesRequest :: Newtype GetBundlesRequest _


newtype GetBundlesResult = GetBundlesResult 
  { "Bundles'" :: NullOrUndefined (BundleList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetBundlesResult :: Newtype GetBundlesResult _


newtype GetDiskRequest = GetDiskRequest 
  { "DiskName'" :: (ResourceName)
  }
derive instance newtypeGetDiskRequest :: Newtype GetDiskRequest _


newtype GetDiskResult = GetDiskResult 
  { "Disk'" :: NullOrUndefined (Disk)
  }
derive instance newtypeGetDiskResult :: Newtype GetDiskResult _


newtype GetDiskSnapshotRequest = GetDiskSnapshotRequest 
  { "DiskSnapshotName'" :: (ResourceName)
  }
derive instance newtypeGetDiskSnapshotRequest :: Newtype GetDiskSnapshotRequest _


newtype GetDiskSnapshotResult = GetDiskSnapshotResult 
  { "DiskSnapshot'" :: NullOrUndefined (DiskSnapshot)
  }
derive instance newtypeGetDiskSnapshotResult :: Newtype GetDiskSnapshotResult _


newtype GetDiskSnapshotsRequest = GetDiskSnapshotsRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetDiskSnapshotsRequest :: Newtype GetDiskSnapshotsRequest _


newtype GetDiskSnapshotsResult = GetDiskSnapshotsResult 
  { "DiskSnapshots'" :: NullOrUndefined (DiskSnapshotList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetDiskSnapshotsResult :: Newtype GetDiskSnapshotsResult _


newtype GetDisksRequest = GetDisksRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetDisksRequest :: Newtype GetDisksRequest _


newtype GetDisksResult = GetDisksResult 
  { "Disks'" :: NullOrUndefined (DiskList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetDisksResult :: Newtype GetDisksResult _


newtype GetDomainRequest = GetDomainRequest 
  { "DomainName'" :: (DomainName)
  }
derive instance newtypeGetDomainRequest :: Newtype GetDomainRequest _


newtype GetDomainResult = GetDomainResult 
  { "Domain'" :: NullOrUndefined (Domain)
  }
derive instance newtypeGetDomainResult :: Newtype GetDomainResult _


newtype GetDomainsRequest = GetDomainsRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetDomainsRequest :: Newtype GetDomainsRequest _


newtype GetDomainsResult = GetDomainsResult 
  { "Domains'" :: NullOrUndefined (DomainList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetDomainsResult :: Newtype GetDomainsResult _


newtype GetInstanceAccessDetailsRequest = GetInstanceAccessDetailsRequest 
  { "InstanceName'" :: (ResourceName)
  , "Protocol'" :: NullOrUndefined (InstanceAccessProtocol)
  }
derive instance newtypeGetInstanceAccessDetailsRequest :: Newtype GetInstanceAccessDetailsRequest _


newtype GetInstanceAccessDetailsResult = GetInstanceAccessDetailsResult 
  { "AccessDetails'" :: NullOrUndefined (InstanceAccessDetails)
  }
derive instance newtypeGetInstanceAccessDetailsResult :: Newtype GetInstanceAccessDetailsResult _


newtype GetInstanceMetricDataRequest = GetInstanceMetricDataRequest 
  { "InstanceName'" :: (ResourceName)
  , "MetricName'" :: (InstanceMetricName)
  , "Period'" :: (MetricPeriod)
  , "StartTime'" :: (Number)
  , "EndTime'" :: (Number)
  , "Unit''" :: (MetricUnit)
  , "Statistics'" :: (MetricStatisticList)
  }
derive instance newtypeGetInstanceMetricDataRequest :: Newtype GetInstanceMetricDataRequest _


newtype GetInstanceMetricDataResult = GetInstanceMetricDataResult 
  { "MetricName'" :: NullOrUndefined (InstanceMetricName)
  , "MetricData'" :: NullOrUndefined (MetricDatapointList)
  }
derive instance newtypeGetInstanceMetricDataResult :: Newtype GetInstanceMetricDataResult _


newtype GetInstancePortStatesRequest = GetInstancePortStatesRequest 
  { "InstanceName'" :: (ResourceName)
  }
derive instance newtypeGetInstancePortStatesRequest :: Newtype GetInstancePortStatesRequest _


newtype GetInstancePortStatesResult = GetInstancePortStatesResult 
  { "PortStates'" :: NullOrUndefined (InstancePortStateList)
  }
derive instance newtypeGetInstancePortStatesResult :: Newtype GetInstancePortStatesResult _


newtype GetInstanceRequest = GetInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  }
derive instance newtypeGetInstanceRequest :: Newtype GetInstanceRequest _


newtype GetInstanceResult = GetInstanceResult 
  { "Instance'" :: NullOrUndefined (Instance)
  }
derive instance newtypeGetInstanceResult :: Newtype GetInstanceResult _


newtype GetInstanceSnapshotRequest = GetInstanceSnapshotRequest 
  { "InstanceSnapshotName'" :: (ResourceName)
  }
derive instance newtypeGetInstanceSnapshotRequest :: Newtype GetInstanceSnapshotRequest _


newtype GetInstanceSnapshotResult = GetInstanceSnapshotResult 
  { "InstanceSnapshot'" :: NullOrUndefined (InstanceSnapshot)
  }
derive instance newtypeGetInstanceSnapshotResult :: Newtype GetInstanceSnapshotResult _


newtype GetInstanceSnapshotsRequest = GetInstanceSnapshotsRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetInstanceSnapshotsRequest :: Newtype GetInstanceSnapshotsRequest _


newtype GetInstanceSnapshotsResult = GetInstanceSnapshotsResult 
  { "InstanceSnapshots'" :: NullOrUndefined (InstanceSnapshotList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetInstanceSnapshotsResult :: Newtype GetInstanceSnapshotsResult _


newtype GetInstanceStateRequest = GetInstanceStateRequest 
  { "InstanceName'" :: (ResourceName)
  }
derive instance newtypeGetInstanceStateRequest :: Newtype GetInstanceStateRequest _


newtype GetInstanceStateResult = GetInstanceStateResult 
  { "State'" :: NullOrUndefined (InstanceState)
  }
derive instance newtypeGetInstanceStateResult :: Newtype GetInstanceStateResult _


newtype GetInstancesRequest = GetInstancesRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetInstancesRequest :: Newtype GetInstancesRequest _


newtype GetInstancesResult = GetInstancesResult 
  { "Instances'" :: NullOrUndefined (InstanceList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetInstancesResult :: Newtype GetInstancesResult _


newtype GetKeyPairRequest = GetKeyPairRequest 
  { "KeyPairName'" :: (ResourceName)
  }
derive instance newtypeGetKeyPairRequest :: Newtype GetKeyPairRequest _


newtype GetKeyPairResult = GetKeyPairResult 
  { "KeyPair'" :: NullOrUndefined (KeyPair)
  }
derive instance newtypeGetKeyPairResult :: Newtype GetKeyPairResult _


newtype GetKeyPairsRequest = GetKeyPairsRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetKeyPairsRequest :: Newtype GetKeyPairsRequest _


newtype GetKeyPairsResult = GetKeyPairsResult 
  { "KeyPairs'" :: NullOrUndefined (KeyPairList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetKeyPairsResult :: Newtype GetKeyPairsResult _


newtype GetLoadBalancerMetricDataRequest = GetLoadBalancerMetricDataRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "MetricName'" :: (LoadBalancerMetricName)
  , "Period'" :: (MetricPeriod)
  , "StartTime'" :: (Number)
  , "EndTime'" :: (Number)
  , "Unit''" :: (MetricUnit)
  , "Statistics'" :: (MetricStatisticList)
  }
derive instance newtypeGetLoadBalancerMetricDataRequest :: Newtype GetLoadBalancerMetricDataRequest _


newtype GetLoadBalancerMetricDataResult = GetLoadBalancerMetricDataResult 
  { "MetricName'" :: NullOrUndefined (LoadBalancerMetricName)
  , "MetricData'" :: NullOrUndefined (MetricDatapointList)
  }
derive instance newtypeGetLoadBalancerMetricDataResult :: Newtype GetLoadBalancerMetricDataResult _


newtype GetLoadBalancerRequest = GetLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  }
derive instance newtypeGetLoadBalancerRequest :: Newtype GetLoadBalancerRequest _


newtype GetLoadBalancerResult = GetLoadBalancerResult 
  { "LoadBalancer'" :: NullOrUndefined (LoadBalancer)
  }
derive instance newtypeGetLoadBalancerResult :: Newtype GetLoadBalancerResult _


newtype GetLoadBalancerTlsCertificatesRequest = GetLoadBalancerTlsCertificatesRequest 
  { "LoadBalancerName'" :: (ResourceName)
  }
derive instance newtypeGetLoadBalancerTlsCertificatesRequest :: Newtype GetLoadBalancerTlsCertificatesRequest _


newtype GetLoadBalancerTlsCertificatesResult = GetLoadBalancerTlsCertificatesResult 
  { "TlsCertificates'" :: NullOrUndefined (LoadBalancerTlsCertificateList)
  }
derive instance newtypeGetLoadBalancerTlsCertificatesResult :: Newtype GetLoadBalancerTlsCertificatesResult _


newtype GetLoadBalancersRequest = GetLoadBalancersRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetLoadBalancersRequest :: Newtype GetLoadBalancersRequest _


newtype GetLoadBalancersResult = GetLoadBalancersResult 
  { "LoadBalancers'" :: NullOrUndefined (LoadBalancerList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetLoadBalancersResult :: Newtype GetLoadBalancersResult _


newtype GetOperationRequest = GetOperationRequest 
  { "OperationId'" :: (NonEmptyString)
  }
derive instance newtypeGetOperationRequest :: Newtype GetOperationRequest _


newtype GetOperationResult = GetOperationResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypeGetOperationResult :: Newtype GetOperationResult _


newtype GetOperationsForResourceRequest = GetOperationsForResourceRequest 
  { "ResourceName'" :: (ResourceName)
  , "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetOperationsForResourceRequest :: Newtype GetOperationsForResourceRequest _


newtype GetOperationsForResourceResult = GetOperationsForResourceResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  , "NextPageCount'" :: NullOrUndefined (String)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetOperationsForResourceResult :: Newtype GetOperationsForResourceResult _


newtype GetOperationsRequest = GetOperationsRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetOperationsRequest :: Newtype GetOperationsRequest _


newtype GetOperationsResult = GetOperationsResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetOperationsResult :: Newtype GetOperationsResult _


newtype GetRegionsRequest = GetRegionsRequest 
  { "IncludeAvailabilityZones'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeGetRegionsRequest :: Newtype GetRegionsRequest _


newtype GetRegionsResult = GetRegionsResult 
  { "Regions'" :: NullOrUndefined (RegionList)
  }
derive instance newtypeGetRegionsResult :: Newtype GetRegionsResult _


newtype GetStaticIpRequest = GetStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  }
derive instance newtypeGetStaticIpRequest :: Newtype GetStaticIpRequest _


newtype GetStaticIpResult = GetStaticIpResult 
  { "StaticIp'" :: NullOrUndefined (StaticIp)
  }
derive instance newtypeGetStaticIpResult :: Newtype GetStaticIpResult _


newtype GetStaticIpsRequest = GetStaticIpsRequest 
  { "PageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetStaticIpsRequest :: Newtype GetStaticIpsRequest _


newtype GetStaticIpsResult = GetStaticIpsResult 
  { "StaticIps'" :: NullOrUndefined (StaticIpList)
  , "NextPageToken'" :: NullOrUndefined (String)
  }
derive instance newtypeGetStaticIpsResult :: Newtype GetStaticIpsResult _


newtype ImportKeyPairRequest = ImportKeyPairRequest 
  { "KeyPairName'" :: (ResourceName)
  , "PublicKeyBase64'" :: (Base64)
  }
derive instance newtypeImportKeyPairRequest :: Newtype ImportKeyPairRequest _


newtype ImportKeyPairResult = ImportKeyPairResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypeImportKeyPairResult :: Newtype ImportKeyPairResult _


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
derive instance newtypeInstance :: Newtype Instance _


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
derive instance newtypeInstanceAccessDetails :: Newtype InstanceAccessDetails _


newtype InstanceAccessProtocol = InstanceAccessProtocol String
derive instance newtypeInstanceAccessProtocol :: Newtype InstanceAccessProtocol _


-- | <p>Describes the hardware for the instance.</p>
newtype InstanceHardware = InstanceHardware 
  { "CpuCount'" :: NullOrUndefined (Int)
  , "Disks'" :: NullOrUndefined (DiskList)
  , "RamSizeInGb'" :: NullOrUndefined (Number)
  }
derive instance newtypeInstanceHardware :: Newtype InstanceHardware _


newtype InstanceHealthReason = InstanceHealthReason String
derive instance newtypeInstanceHealthReason :: Newtype InstanceHealthReason _


newtype InstanceHealthState = InstanceHealthState String
derive instance newtypeInstanceHealthState :: Newtype InstanceHealthState _


-- | <p>Describes information about the health of the instance.</p>
newtype InstanceHealthSummary = InstanceHealthSummary 
  { "InstanceName'" :: NullOrUndefined (ResourceName)
  , "InstanceHealth'" :: NullOrUndefined (InstanceHealthState)
  , "InstanceHealthReason'" :: NullOrUndefined (InstanceHealthReason)
  }
derive instance newtypeInstanceHealthSummary :: Newtype InstanceHealthSummary _


newtype InstanceHealthSummaryList = InstanceHealthSummaryList (Array InstanceHealthSummary)
derive instance newtypeInstanceHealthSummaryList :: Newtype InstanceHealthSummaryList _


newtype InstanceList = InstanceList (Array Instance)
derive instance newtypeInstanceList :: Newtype InstanceList _


newtype InstanceMetricName = InstanceMetricName String
derive instance newtypeInstanceMetricName :: Newtype InstanceMetricName _


-- | <p>Describes monthly data transfer rates and port information for an instance.</p>
newtype InstanceNetworking = InstanceNetworking 
  { "MonthlyTransfer'" :: NullOrUndefined (MonthlyTransfer)
  , "Ports'" :: NullOrUndefined (InstancePortInfoList)
  }
derive instance newtypeInstanceNetworking :: Newtype InstanceNetworking _


newtype InstancePlatform = InstancePlatform String
derive instance newtypeInstancePlatform :: Newtype InstancePlatform _


newtype InstancePlatformList = InstancePlatformList (Array InstancePlatform)
derive instance newtypeInstancePlatformList :: Newtype InstancePlatformList _


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
derive instance newtypeInstancePortInfo :: Newtype InstancePortInfo _


newtype InstancePortInfoList = InstancePortInfoList (Array InstancePortInfo)
derive instance newtypeInstancePortInfoList :: Newtype InstancePortInfoList _


-- | <p>Describes the port state.</p>
newtype InstancePortState = InstancePortState 
  { "FromPort'" :: NullOrUndefined (Port)
  , "ToPort'" :: NullOrUndefined (Port)
  , "Protocol'" :: NullOrUndefined (NetworkProtocol)
  , "State'" :: NullOrUndefined (PortState)
  }
derive instance newtypeInstancePortState :: Newtype InstancePortState _


newtype InstancePortStateList = InstancePortStateList (Array InstancePortState)
derive instance newtypeInstancePortStateList :: Newtype InstancePortStateList _


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
derive instance newtypeInstanceSnapshot :: Newtype InstanceSnapshot _


newtype InstanceSnapshotList = InstanceSnapshotList (Array InstanceSnapshot)
derive instance newtypeInstanceSnapshotList :: Newtype InstanceSnapshotList _


newtype InstanceSnapshotState = InstanceSnapshotState String
derive instance newtypeInstanceSnapshotState :: Newtype InstanceSnapshotState _


-- | <p>Describes the virtual private server (or <i>instance</i>) status.</p>
newtype InstanceState = InstanceState 
  { "Code'" :: NullOrUndefined (Int)
  , "Name'" :: NullOrUndefined (String)
  }
derive instance newtypeInstanceState :: Newtype InstanceState _


-- | <p>Lightsail throws this exception when user input does not conform to the validation rules of an input field.</p> <note> <p>Domain-related APIs are only available in the N. Virginia (us-east-1) Region. Please set your Region configuration to us-east-1 to create, view, or edit these resources.</p> </note>
newtype InvalidInputException = InvalidInputException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidInputException :: Newtype InvalidInputException _


newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _


newtype IpV6Address = IpV6Address String
derive instance newtypeIpV6Address :: Newtype IpV6Address _


newtype IsVpcPeeredRequest = IsVpcPeeredRequest 
  { 
  }
derive instance newtypeIsVpcPeeredRequest :: Newtype IsVpcPeeredRequest _


newtype IsVpcPeeredResult = IsVpcPeeredResult 
  { "IsPeered'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeIsVpcPeeredResult :: Newtype IsVpcPeeredResult _


newtype IsoDate = IsoDate Number
derive instance newtypeIsoDate :: Newtype IsoDate _


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
derive instance newtypeKeyPair :: Newtype KeyPair _


newtype KeyPairList = KeyPairList (Array KeyPair)
derive instance newtypeKeyPairList :: Newtype KeyPairList _


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
derive instance newtypeLoadBalancer :: Newtype LoadBalancer _


newtype LoadBalancerAttributeName = LoadBalancerAttributeName String
derive instance newtypeLoadBalancerAttributeName :: Newtype LoadBalancerAttributeName _


newtype LoadBalancerConfigurationOptions = LoadBalancerConfigurationOptions (Map LoadBalancerAttributeName String)
derive instance newtypeLoadBalancerConfigurationOptions :: Newtype LoadBalancerConfigurationOptions _


newtype LoadBalancerList = LoadBalancerList (Array LoadBalancer)
derive instance newtypeLoadBalancerList :: Newtype LoadBalancerList _


newtype LoadBalancerMetricName = LoadBalancerMetricName String
derive instance newtypeLoadBalancerMetricName :: Newtype LoadBalancerMetricName _


newtype LoadBalancerProtocol = LoadBalancerProtocol String
derive instance newtypeLoadBalancerProtocol :: Newtype LoadBalancerProtocol _


newtype LoadBalancerState = LoadBalancerState String
derive instance newtypeLoadBalancerState :: Newtype LoadBalancerState _


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
derive instance newtypeLoadBalancerTlsCertificate :: Newtype LoadBalancerTlsCertificate _


newtype LoadBalancerTlsCertificateDomainStatus = LoadBalancerTlsCertificateDomainStatus String
derive instance newtypeLoadBalancerTlsCertificateDomainStatus :: Newtype LoadBalancerTlsCertificateDomainStatus _


-- | <p>Contains information about the domain names on a TLS/SSL certificate that you will use to validate domain ownership.</p>
newtype LoadBalancerTlsCertificateDomainValidationOption = LoadBalancerTlsCertificateDomainValidationOption 
  { "DomainName'" :: NullOrUndefined (DomainName)
  , "ValidationStatus'" :: NullOrUndefined (LoadBalancerTlsCertificateDomainStatus)
  }
derive instance newtypeLoadBalancerTlsCertificateDomainValidationOption :: Newtype LoadBalancerTlsCertificateDomainValidationOption _


newtype LoadBalancerTlsCertificateDomainValidationOptionList = LoadBalancerTlsCertificateDomainValidationOptionList (Array LoadBalancerTlsCertificateDomainValidationOption)
derive instance newtypeLoadBalancerTlsCertificateDomainValidationOptionList :: Newtype LoadBalancerTlsCertificateDomainValidationOptionList _


-- | <p>Describes the validation record of each domain name in the TLS/SSL certificate.</p>
newtype LoadBalancerTlsCertificateDomainValidationRecord = LoadBalancerTlsCertificateDomainValidationRecord 
  { "Name'" :: NullOrUndefined (NonEmptyString)
  , "Type'" :: NullOrUndefined (NonEmptyString)
  , "Value'" :: NullOrUndefined (NonEmptyString)
  , "ValidationStatus'" :: NullOrUndefined (LoadBalancerTlsCertificateDomainStatus)
  , "DomainName'" :: NullOrUndefined (DomainName)
  }
derive instance newtypeLoadBalancerTlsCertificateDomainValidationRecord :: Newtype LoadBalancerTlsCertificateDomainValidationRecord _


newtype LoadBalancerTlsCertificateDomainValidationRecordList = LoadBalancerTlsCertificateDomainValidationRecordList (Array LoadBalancerTlsCertificateDomainValidationRecord)
derive instance newtypeLoadBalancerTlsCertificateDomainValidationRecordList :: Newtype LoadBalancerTlsCertificateDomainValidationRecordList _


newtype LoadBalancerTlsCertificateFailureReason = LoadBalancerTlsCertificateFailureReason String
derive instance newtypeLoadBalancerTlsCertificateFailureReason :: Newtype LoadBalancerTlsCertificateFailureReason _


newtype LoadBalancerTlsCertificateList = LoadBalancerTlsCertificateList (Array LoadBalancerTlsCertificate)
derive instance newtypeLoadBalancerTlsCertificateList :: Newtype LoadBalancerTlsCertificateList _


newtype LoadBalancerTlsCertificateRenewalStatus = LoadBalancerTlsCertificateRenewalStatus String
derive instance newtypeLoadBalancerTlsCertificateRenewalStatus :: Newtype LoadBalancerTlsCertificateRenewalStatus _


-- | <p>Contains information about the status of Lightsail's managed renewal for the certificate.</p>
newtype LoadBalancerTlsCertificateRenewalSummary = LoadBalancerTlsCertificateRenewalSummary 
  { "RenewalStatus'" :: NullOrUndefined (LoadBalancerTlsCertificateRenewalStatus)
  , "DomainValidationOptions'" :: NullOrUndefined (LoadBalancerTlsCertificateDomainValidationOptionList)
  }
derive instance newtypeLoadBalancerTlsCertificateRenewalSummary :: Newtype LoadBalancerTlsCertificateRenewalSummary _


newtype LoadBalancerTlsCertificateRevocationReason = LoadBalancerTlsCertificateRevocationReason String
derive instance newtypeLoadBalancerTlsCertificateRevocationReason :: Newtype LoadBalancerTlsCertificateRevocationReason _


newtype LoadBalancerTlsCertificateStatus = LoadBalancerTlsCertificateStatus String
derive instance newtypeLoadBalancerTlsCertificateStatus :: Newtype LoadBalancerTlsCertificateStatus _


-- | <p>Provides a summary of TLS/SSL certificate metadata.</p>
newtype LoadBalancerTlsCertificateSummary = LoadBalancerTlsCertificateSummary 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "IsAttached'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeLoadBalancerTlsCertificateSummary :: Newtype LoadBalancerTlsCertificateSummary _


newtype LoadBalancerTlsCertificateSummaryList = LoadBalancerTlsCertificateSummaryList (Array LoadBalancerTlsCertificateSummary)
derive instance newtypeLoadBalancerTlsCertificateSummaryList :: Newtype LoadBalancerTlsCertificateSummaryList _


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
derive instance newtypeMetricDatapoint :: Newtype MetricDatapoint _


newtype MetricDatapointList = MetricDatapointList (Array MetricDatapoint)
derive instance newtypeMetricDatapointList :: Newtype MetricDatapointList _


newtype MetricPeriod = MetricPeriod Int
derive instance newtypeMetricPeriod :: Newtype MetricPeriod _


newtype MetricStatistic = MetricStatistic String
derive instance newtypeMetricStatistic :: Newtype MetricStatistic _


newtype MetricStatisticList = MetricStatisticList (Array MetricStatistic)
derive instance newtypeMetricStatisticList :: Newtype MetricStatisticList _


newtype MetricUnit = MetricUnit String
derive instance newtypeMetricUnit :: Newtype MetricUnit _


-- | <p>Describes the monthly data transfer in and out of your virtual private server (or <i>instance</i>).</p>
newtype MonthlyTransfer = MonthlyTransfer 
  { "GbPerMonthAllocated'" :: NullOrUndefined (Int)
  }
derive instance newtypeMonthlyTransfer :: Newtype MonthlyTransfer _


newtype NetworkProtocol = NetworkProtocol String
derive instance newtypeNetworkProtocol :: Newtype NetworkProtocol _


newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString :: Newtype NonEmptyString _


-- | <p>Lightsail throws this exception when it cannot find a resource.</p>
newtype NotFoundException = NotFoundException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


newtype OpenInstancePublicPortsRequest = OpenInstancePublicPortsRequest 
  { "PortInfo'" :: (PortInfo)
  , "InstanceName'" :: (ResourceName)
  }
derive instance newtypeOpenInstancePublicPortsRequest :: Newtype OpenInstancePublicPortsRequest _


newtype OpenInstancePublicPortsResult = OpenInstancePublicPortsResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypeOpenInstancePublicPortsResult :: Newtype OpenInstancePublicPortsResult _


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
derive instance newtypeOperation :: Newtype Operation _


-- | <p>Lightsail throws this exception when an operation fails to execute.</p>
newtype OperationFailureException = OperationFailureException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }
derive instance newtypeOperationFailureException :: Newtype OperationFailureException _


newtype OperationList = OperationList (Array Operation)
derive instance newtypeOperationList :: Newtype OperationList _


newtype OperationStatus = OperationStatus String
derive instance newtypeOperationStatus :: Newtype OperationStatus _


newtype OperationType = OperationType String
derive instance newtypeOperationType :: Newtype OperationType _


-- | <p>The password data for the Windows Server-based instance, including the ciphertext and the key pair name.</p>
newtype PasswordData = PasswordData 
  { "Ciphertext'" :: NullOrUndefined (String)
  , "KeyPairName'" :: NullOrUndefined (ResourceName)
  }
derive instance newtypePasswordData :: Newtype PasswordData _


newtype PeerVpcRequest = PeerVpcRequest 
  { 
  }
derive instance newtypePeerVpcRequest :: Newtype PeerVpcRequest _


newtype PeerVpcResult = PeerVpcResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypePeerVpcResult :: Newtype PeerVpcResult _


newtype Port = Port Int
derive instance newtypePort :: Newtype Port _


newtype PortAccessType = PortAccessType String
derive instance newtypePortAccessType :: Newtype PortAccessType _


-- | <p>Describes information about the ports on your virtual private server (or <i>instance</i>).</p>
newtype PortInfo = PortInfo 
  { "FromPort'" :: NullOrUndefined (Port)
  , "ToPort'" :: NullOrUndefined (Port)
  , "Protocol'" :: NullOrUndefined (NetworkProtocol)
  }
derive instance newtypePortInfo :: Newtype PortInfo _


newtype PortInfoList = PortInfoList (Array PortInfo)
derive instance newtypePortInfoList :: Newtype PortInfoList _


newtype PortList = PortList (Array Port)
derive instance newtypePortList :: Newtype PortList _


newtype PortState = PortState String
derive instance newtypePortState :: Newtype PortState _


newtype PutInstancePublicPortsRequest = PutInstancePublicPortsRequest 
  { "PortInfos'" :: (PortInfoList)
  , "InstanceName'" :: (ResourceName)
  }
derive instance newtypePutInstancePublicPortsRequest :: Newtype PutInstancePublicPortsRequest _


newtype PutInstancePublicPortsResult = PutInstancePublicPortsResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypePutInstancePublicPortsResult :: Newtype PutInstancePublicPortsResult _


newtype RebootInstanceRequest = RebootInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  }
derive instance newtypeRebootInstanceRequest :: Newtype RebootInstanceRequest _


newtype RebootInstanceResult = RebootInstanceResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeRebootInstanceResult :: Newtype RebootInstanceResult _


-- | <p>Describes the AWS Region.</p>
newtype Region = Region 
  { "ContinentCode'" :: NullOrUndefined (String)
  , "Description'" :: NullOrUndefined (String)
  , "DisplayName'" :: NullOrUndefined (String)
  , "Name'" :: NullOrUndefined (RegionName)
  , "AvailabilityZones'" :: NullOrUndefined (AvailabilityZoneList)
  }
derive instance newtypeRegion :: Newtype Region _


newtype RegionList = RegionList (Array Region)
derive instance newtypeRegionList :: Newtype RegionList _


newtype RegionName = RegionName String
derive instance newtypeRegionName :: Newtype RegionName _


newtype ReleaseStaticIpRequest = ReleaseStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  }
derive instance newtypeReleaseStaticIpRequest :: Newtype ReleaseStaticIpRequest _


newtype ReleaseStaticIpResult = ReleaseStaticIpResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeReleaseStaticIpResult :: Newtype ReleaseStaticIpResult _


-- | <p>Describes the resource location.</p>
newtype ResourceLocation = ResourceLocation 
  { "AvailabilityZone'" :: NullOrUndefined (String)
  , "RegionName'" :: NullOrUndefined (RegionName)
  }
derive instance newtypeResourceLocation :: Newtype ResourceLocation _


newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _


newtype ResourceNameList = ResourceNameList (Array ResourceName)
derive instance newtypeResourceNameList :: Newtype ResourceNameList _


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _


-- | <p>A general service exception.</p>
newtype ServiceException = ServiceException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }
derive instance newtypeServiceException :: Newtype ServiceException _


newtype StartInstanceRequest = StartInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  }
derive instance newtypeStartInstanceRequest :: Newtype StartInstanceRequest _


newtype StartInstanceResult = StartInstanceResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeStartInstanceResult :: Newtype StartInstanceResult _


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
derive instance newtypeStaticIp :: Newtype StaticIp _


newtype StaticIpList = StaticIpList (Array StaticIp)
derive instance newtypeStaticIpList :: Newtype StaticIpList _


newtype StopInstanceRequest = StopInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  , "Force'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeStopInstanceRequest :: Newtype StopInstanceRequest _


newtype StopInstanceResult = StopInstanceResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeStopInstanceResult :: Newtype StopInstanceResult _


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _


newtype StringMax256 = StringMax256 String
derive instance newtypeStringMax256 :: Newtype StringMax256 _


-- | <p>Lightsail throws this exception when the user has not been authenticated.</p>
newtype UnauthenticatedException = UnauthenticatedException 
  { "Code'" :: NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined (String)
  }
derive instance newtypeUnauthenticatedException :: Newtype UnauthenticatedException _


newtype UnpeerVpcRequest = UnpeerVpcRequest 
  { 
  }
derive instance newtypeUnpeerVpcRequest :: Newtype UnpeerVpcRequest _


newtype UnpeerVpcResult = UnpeerVpcResult 
  { "Operation'" :: NullOrUndefined (Operation)
  }
derive instance newtypeUnpeerVpcResult :: Newtype UnpeerVpcResult _


newtype UpdateDomainEntryRequest = UpdateDomainEntryRequest 
  { "DomainName'" :: (DomainName)
  , "DomainEntry'" :: (DomainEntry)
  }
derive instance newtypeUpdateDomainEntryRequest :: Newtype UpdateDomainEntryRequest _


newtype UpdateDomainEntryResult = UpdateDomainEntryResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeUpdateDomainEntryResult :: Newtype UpdateDomainEntryResult _


newtype UpdateLoadBalancerAttributeRequest = UpdateLoadBalancerAttributeRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "AttributeName'" :: (LoadBalancerAttributeName)
  , "AttributeValue'" :: (StringMax256)
  }
derive instance newtypeUpdateLoadBalancerAttributeRequest :: Newtype UpdateLoadBalancerAttributeRequest _


newtype UpdateLoadBalancerAttributeResult = UpdateLoadBalancerAttributeResult 
  { "Operations'" :: NullOrUndefined (OperationList)
  }
derive instance newtypeUpdateLoadBalancerAttributeResult :: Newtype UpdateLoadBalancerAttributeResult _
