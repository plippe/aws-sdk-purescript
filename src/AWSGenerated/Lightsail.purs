

-- | <p>Amazon Lightsail is the easiest way to get started with AWS for developers who just need virtual private servers. Lightsail includes everything you need to launch your project quickly - a virtual machine, SSD-based storage, data transfer, DNS management, and a static IP - for a low, predictable price. You manage those Lightsail servers through the Lightsail console or by using the API or command-line interface (CLI).</p> <p>For more information about Lightsail concepts and tasks, see the <a href="https://lightsail.aws.amazon.com/ls/docs/all">Lightsail Dev Guide</a>.</p> <p>To use the Lightsail API or the CLI, you will need to use AWS Identity and Access Management (IAM) to generate access keys. For details about how to set this up, see the <a href="http://lightsail.aws.amazon.com/ls/docs/how-to/article/lightsail-how-to-set-up-access-keys-to-use-sdk-api-cli">Lightsail Dev Guide</a>.</p>
module AWS.Lightsail where

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

serviceName = "Lightsail" :: String


-- | <p>Allocates a static IP address.</p>
allocateStaticIp :: forall eff. AllocateStaticIpRequest -> Aff (exception :: EXCEPTION | eff) AllocateStaticIpResult
allocateStaticIp = Request.request serviceName "allocateStaticIp" 


-- | <p>Attaches a block storage disk to a running or stopped Lightsail instance and exposes it to the instance with the specified disk name.</p>
attachDisk :: forall eff. AttachDiskRequest -> Aff (exception :: EXCEPTION | eff) AttachDiskResult
attachDisk = Request.request serviceName "attachDisk" 


-- | <p>Attaches one or more Lightsail instances to a load balancer.</p>
attachInstancesToLoadBalancer :: forall eff. AttachInstancesToLoadBalancerRequest -> Aff (exception :: EXCEPTION | eff) AttachInstancesToLoadBalancerResult
attachInstancesToLoadBalancer = Request.request serviceName "attachInstancesToLoadBalancer" 


-- | <p>Attaches a Transport Layer Security (TLS) certificate to your load balancer.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>
attachLoadBalancerTlsCertificate :: forall eff. AttachLoadBalancerTlsCertificateRequest -> Aff (exception :: EXCEPTION | eff) AttachLoadBalancerTlsCertificateResult
attachLoadBalancerTlsCertificate = Request.request serviceName "attachLoadBalancerTlsCertificate" 


-- | <p>Attaches a static IP address to a specific Amazon Lightsail instance.</p>
attachStaticIp :: forall eff. AttachStaticIpRequest -> Aff (exception :: EXCEPTION | eff) AttachStaticIpResult
attachStaticIp = Request.request serviceName "attachStaticIp" 


-- | <p>Closes the public ports on a specific Amazon Lightsail instance.</p>
closeInstancePublicPorts :: forall eff. CloseInstancePublicPortsRequest -> Aff (exception :: EXCEPTION | eff) CloseInstancePublicPortsResult
closeInstancePublicPorts = Request.request serviceName "closeInstancePublicPorts" 


-- | <p>Creates a block storage disk that can be attached to a Lightsail instance in the same Availability Zone (e.g., <code>us-east-2a</code>). The disk is created in the regional endpoint that you send the HTTP request to. For more information, see <a href="https://lightsail.aws.amazon.com/ls/docs/overview/article/understanding-regions-and-availability-zones-in-amazon-lightsail">Regions and Availability Zones in Lightsail</a>.</p>
createDisk :: forall eff. CreateDiskRequest -> Aff (exception :: EXCEPTION | eff) CreateDiskResult
createDisk = Request.request serviceName "createDisk" 


-- | <p>Creates a block storage disk from a disk snapshot that can be attached to a Lightsail instance in the same Availability Zone (e.g., <code>us-east-2a</code>). The disk is created in the regional endpoint that you send the HTTP request to. For more information, see <a href="https://lightsail.aws.amazon.com/ls/docs/overview/article/understanding-regions-and-availability-zones-in-amazon-lightsail">Regions and Availability Zones in Lightsail</a>.</p>
createDiskFromSnapshot :: forall eff. CreateDiskFromSnapshotRequest -> Aff (exception :: EXCEPTION | eff) CreateDiskFromSnapshotResult
createDiskFromSnapshot = Request.request serviceName "createDiskFromSnapshot" 


-- | <p>Creates a snapshot of a block storage disk. You can use snapshots for backups, to make copies of disks, and to save data before shutting down a Lightsail instance.</p> <p>You can take a snapshot of an attached disk that is in use; however, snapshots only capture data that has been written to your disk at the time the snapshot command is issued. This may exclude any data that has been cached by any applications or the operating system. If you can pause any file systems on the disk long enough to take a snapshot, your snapshot should be complete. Nevertheless, if you cannot pause all file writes to the disk, you should unmount the disk from within the Lightsail instance, issue the create disk snapshot command, and then remount the disk to ensure a consistent and complete snapshot. You may remount and use your disk while the snapshot status is pending.</p>
createDiskSnapshot :: forall eff. CreateDiskSnapshotRequest -> Aff (exception :: EXCEPTION | eff) CreateDiskSnapshotResult
createDiskSnapshot = Request.request serviceName "createDiskSnapshot" 


-- | <p>Creates a domain resource for the specified domain (e.g., example.com).</p>
createDomain :: forall eff. CreateDomainRequest -> Aff (exception :: EXCEPTION | eff) CreateDomainResult
createDomain = Request.request serviceName "createDomain" 


-- | <p>Creates one of the following entry records associated with the domain: A record, CNAME record, TXT record, or MX record.</p>
createDomainEntry :: forall eff. CreateDomainEntryRequest -> Aff (exception :: EXCEPTION | eff) CreateDomainEntryResult
createDomainEntry = Request.request serviceName "createDomainEntry" 


-- | <p>Creates a snapshot of a specific virtual private server, or <i>instance</i>. You can use a snapshot to create a new instance that is based on that snapshot.</p>
createInstanceSnapshot :: forall eff. CreateInstanceSnapshotRequest -> Aff (exception :: EXCEPTION | eff) CreateInstanceSnapshotResult
createInstanceSnapshot = Request.request serviceName "createInstanceSnapshot" 


-- | <p>Creates one or more Amazon Lightsail virtual private servers, or <i>instances</i>.</p>
createInstances :: forall eff. CreateInstancesRequest -> Aff (exception :: EXCEPTION | eff) CreateInstancesResult
createInstances = Request.request serviceName "createInstances" 


-- | <p>Uses a specific snapshot as a blueprint for creating one or more new instances that are based on that identical configuration.</p>
createInstancesFromSnapshot :: forall eff. CreateInstancesFromSnapshotRequest -> Aff (exception :: EXCEPTION | eff) CreateInstancesFromSnapshotResult
createInstancesFromSnapshot = Request.request serviceName "createInstancesFromSnapshot" 


-- | <p>Creates sn SSH key pair.</p>
createKeyPair :: forall eff. CreateKeyPairRequest -> Aff (exception :: EXCEPTION | eff) CreateKeyPairResult
createKeyPair = Request.request serviceName "createKeyPair" 


-- | <p>Creates a Lightsail load balancer.</p> <p>When you create a load balancer, you can specify certificates and port settings. You can create up to 5 load balancers per AWS Region in your account.</p>
createLoadBalancer :: forall eff. CreateLoadBalancerRequest -> Aff (exception :: EXCEPTION | eff) CreateLoadBalancerResult
createLoadBalancer = Request.request serviceName "createLoadBalancer" 


-- | <p>Creates a Lightsail load balancer TLS certificate.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>
createLoadBalancerTlsCertificate :: forall eff. CreateLoadBalancerTlsCertificateRequest -> Aff (exception :: EXCEPTION | eff) CreateLoadBalancerTlsCertificateResult
createLoadBalancerTlsCertificate = Request.request serviceName "createLoadBalancerTlsCertificate" 


-- | <p>Deletes the specified block storage disk. The disk must be in the <code>available</code> state (not attached to a Lightsail instance).</p> <note> <p>The disk may remain in the <code>deleting</code> state for several minutes.</p> </note>
deleteDisk :: forall eff. DeleteDiskRequest -> Aff (exception :: EXCEPTION | eff) DeleteDiskResult
deleteDisk = Request.request serviceName "deleteDisk" 


-- | <p>Deletes the specified disk snapshot.</p> <p>When you make periodic snapshots of a disk, the snapshots are incremental, and only the blocks on the device that have changed since your last snapshot are saved in the new snapshot. When you delete a snapshot, only the data not needed for any other snapshot is removed. So regardless of which prior snapshots have been deleted, all active snapshots will have access to all the information needed to restore the disk.</p>
deleteDiskSnapshot :: forall eff. DeleteDiskSnapshotRequest -> Aff (exception :: EXCEPTION | eff) DeleteDiskSnapshotResult
deleteDiskSnapshot = Request.request serviceName "deleteDiskSnapshot" 


-- | <p>Deletes the specified domain recordset and all of its domain records.</p>
deleteDomain :: forall eff. DeleteDomainRequest -> Aff (exception :: EXCEPTION | eff) DeleteDomainResult
deleteDomain = Request.request serviceName "deleteDomain" 


-- | <p>Deletes a specific domain entry.</p>
deleteDomainEntry :: forall eff. DeleteDomainEntryRequest -> Aff (exception :: EXCEPTION | eff) DeleteDomainEntryResult
deleteDomainEntry = Request.request serviceName "deleteDomainEntry" 


-- | <p>Deletes a specific Amazon Lightsail virtual private server, or <i>instance</i>.</p>
deleteInstance :: forall eff. DeleteInstanceRequest -> Aff (exception :: EXCEPTION | eff) DeleteInstanceResult
deleteInstance = Request.request serviceName "deleteInstance" 


-- | <p>Deletes a specific snapshot of a virtual private server (or <i>instance</i>).</p>
deleteInstanceSnapshot :: forall eff. DeleteInstanceSnapshotRequest -> Aff (exception :: EXCEPTION | eff) DeleteInstanceSnapshotResult
deleteInstanceSnapshot = Request.request serviceName "deleteInstanceSnapshot" 


-- | <p>Deletes a specific SSH key pair.</p>
deleteKeyPair :: forall eff. DeleteKeyPairRequest -> Aff (exception :: EXCEPTION | eff) DeleteKeyPairResult
deleteKeyPair = Request.request serviceName "deleteKeyPair" 


-- | <p>Deletes a Lightsail load balancer.</p>
deleteLoadBalancer :: forall eff. DeleteLoadBalancerRequest -> Aff (exception :: EXCEPTION | eff) DeleteLoadBalancerResult
deleteLoadBalancer = Request.request serviceName "deleteLoadBalancer" 


-- | <p>Deletes a TLS/SSL certificate associated with a Lightsail load balancer.</p>
deleteLoadBalancerTlsCertificate :: forall eff. DeleteLoadBalancerTlsCertificateRequest -> Aff (exception :: EXCEPTION | eff) DeleteLoadBalancerTlsCertificateResult
deleteLoadBalancerTlsCertificate = Request.request serviceName "deleteLoadBalancerTlsCertificate" 


-- | <p>Detaches a stopped block storage disk from a Lightsail instance. Make sure to unmount any file systems on the device within your operating system before stopping the instance and detaching the disk.</p>
detachDisk :: forall eff. DetachDiskRequest -> Aff (exception :: EXCEPTION | eff) DetachDiskResult
detachDisk = Request.request serviceName "detachDisk" 


-- | <p>Detaches the specified instances from a Lightsail load balancer.</p>
detachInstancesFromLoadBalancer :: forall eff. DetachInstancesFromLoadBalancerRequest -> Aff (exception :: EXCEPTION | eff) DetachInstancesFromLoadBalancerResult
detachInstancesFromLoadBalancer = Request.request serviceName "detachInstancesFromLoadBalancer" 


-- | <p>Detaches a static IP from the Amazon Lightsail instance to which it is attached.</p>
detachStaticIp :: forall eff. DetachStaticIpRequest -> Aff (exception :: EXCEPTION | eff) DetachStaticIpResult
detachStaticIp = Request.request serviceName "detachStaticIp" 


-- | <p>Downloads the default SSH key pair from the user's account.</p>
downloadDefaultKeyPair :: forall eff. DownloadDefaultKeyPairRequest -> Aff (exception :: EXCEPTION | eff) DownloadDefaultKeyPairResult
downloadDefaultKeyPair = Request.request serviceName "downloadDefaultKeyPair" 


-- | <p>Returns the names of all active (not deleted) resources.</p>
getActiveNames :: forall eff. GetActiveNamesRequest -> Aff (exception :: EXCEPTION | eff) GetActiveNamesResult
getActiveNames = Request.request serviceName "getActiveNames" 


-- | <p>Returns the list of available instance images, or <i>blueprints</i>. You can use a blueprint to create a new virtual private server already running a specific operating system, as well as a preinstalled app or development stack. The software each instance is running depends on the blueprint image you choose.</p>
getBlueprints :: forall eff. GetBlueprintsRequest -> Aff (exception :: EXCEPTION | eff) GetBlueprintsResult
getBlueprints = Request.request serviceName "getBlueprints" 


-- | <p>Returns the list of bundles that are available for purchase. A bundle describes the specs for your virtual private server (or <i>instance</i>).</p>
getBundles :: forall eff. GetBundlesRequest -> Aff (exception :: EXCEPTION | eff) GetBundlesResult
getBundles = Request.request serviceName "getBundles" 


-- | <p>Returns information about a specific block storage disk.</p>
getDisk :: forall eff. GetDiskRequest -> Aff (exception :: EXCEPTION | eff) GetDiskResult
getDisk = Request.request serviceName "getDisk" 


-- | <p>Returns information about a specific block storage disk snapshot.</p>
getDiskSnapshot :: forall eff. GetDiskSnapshotRequest -> Aff (exception :: EXCEPTION | eff) GetDiskSnapshotResult
getDiskSnapshot = Request.request serviceName "getDiskSnapshot" 


-- | <p>Returns information about all block storage disk snapshots in your AWS account and region.</p> <p>If you are describing a long list of disk snapshots, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.</p>
getDiskSnapshots :: forall eff. GetDiskSnapshotsRequest -> Aff (exception :: EXCEPTION | eff) GetDiskSnapshotsResult
getDiskSnapshots = Request.request serviceName "getDiskSnapshots" 


-- | <p>Returns information about all block storage disks in your AWS account and region.</p> <p>If you are describing a long list of disks, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.</p>
getDisks :: forall eff. GetDisksRequest -> Aff (exception :: EXCEPTION | eff) GetDisksResult
getDisks = Request.request serviceName "getDisks" 


-- | <p>Returns information about a specific domain recordset.</p>
getDomain :: forall eff. GetDomainRequest -> Aff (exception :: EXCEPTION | eff) GetDomainResult
getDomain = Request.request serviceName "getDomain" 


-- | <p>Returns a list of all domains in the user's account.</p>
getDomains :: forall eff. GetDomainsRequest -> Aff (exception :: EXCEPTION | eff) GetDomainsResult
getDomains = Request.request serviceName "getDomains" 


-- | <p>Returns information about a specific Amazon Lightsail instance, which is a virtual private server.</p>
getInstance :: forall eff. GetInstanceRequest -> Aff (exception :: EXCEPTION | eff) GetInstanceResult
getInstance = Request.request serviceName "getInstance" 


-- | <p>Returns temporary SSH keys you can use to connect to a specific virtual private server, or <i>instance</i>.</p>
getInstanceAccessDetails :: forall eff. GetInstanceAccessDetailsRequest -> Aff (exception :: EXCEPTION | eff) GetInstanceAccessDetailsResult
getInstanceAccessDetails = Request.request serviceName "getInstanceAccessDetails" 


-- | <p>Returns the data points for the specified Amazon Lightsail instance metric, given an instance name.</p>
getInstanceMetricData :: forall eff. GetInstanceMetricDataRequest -> Aff (exception :: EXCEPTION | eff) GetInstanceMetricDataResult
getInstanceMetricData = Request.request serviceName "getInstanceMetricData" 


-- | <p>Returns the port states for a specific virtual private server, or <i>instance</i>.</p>
getInstancePortStates :: forall eff. GetInstancePortStatesRequest -> Aff (exception :: EXCEPTION | eff) GetInstancePortStatesResult
getInstancePortStates = Request.request serviceName "getInstancePortStates" 


-- | <p>Returns information about a specific instance snapshot.</p>
getInstanceSnapshot :: forall eff. GetInstanceSnapshotRequest -> Aff (exception :: EXCEPTION | eff) GetInstanceSnapshotResult
getInstanceSnapshot = Request.request serviceName "getInstanceSnapshot" 


-- | <p>Returns all instance snapshots for the user's account.</p>
getInstanceSnapshots :: forall eff. GetInstanceSnapshotsRequest -> Aff (exception :: EXCEPTION | eff) GetInstanceSnapshotsResult
getInstanceSnapshots = Request.request serviceName "getInstanceSnapshots" 


-- | <p>Returns the state of a specific instance. Works on one instance at a time.</p>
getInstanceState :: forall eff. GetInstanceStateRequest -> Aff (exception :: EXCEPTION | eff) GetInstanceStateResult
getInstanceState = Request.request serviceName "getInstanceState" 


-- | <p>Returns information about all Amazon Lightsail virtual private servers, or <i>instances</i>.</p>
getInstances :: forall eff. GetInstancesRequest -> Aff (exception :: EXCEPTION | eff) GetInstancesResult
getInstances = Request.request serviceName "getInstances" 


-- | <p>Returns information about a specific key pair.</p>
getKeyPair :: forall eff. GetKeyPairRequest -> Aff (exception :: EXCEPTION | eff) GetKeyPairResult
getKeyPair = Request.request serviceName "getKeyPair" 


-- | <p>Returns information about all key pairs in the user's account.</p>
getKeyPairs :: forall eff. GetKeyPairsRequest -> Aff (exception :: EXCEPTION | eff) GetKeyPairsResult
getKeyPairs = Request.request serviceName "getKeyPairs" 


-- | <p>Returns information about the specified Lightsail load balancer.</p>
getLoadBalancer :: forall eff. GetLoadBalancerRequest -> Aff (exception :: EXCEPTION | eff) GetLoadBalancerResult
getLoadBalancer = Request.request serviceName "getLoadBalancer" 


-- | <p>Returns information about health metrics for your Lightsail load balancer.</p>
getLoadBalancerMetricData :: forall eff. GetLoadBalancerMetricDataRequest -> Aff (exception :: EXCEPTION | eff) GetLoadBalancerMetricDataResult
getLoadBalancerMetricData = Request.request serviceName "getLoadBalancerMetricData" 


-- | <p>Returns information about the TLS certificates that are associated with the specified Lightsail load balancer.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>
getLoadBalancerTlsCertificates :: forall eff. GetLoadBalancerTlsCertificatesRequest -> Aff (exception :: EXCEPTION | eff) GetLoadBalancerTlsCertificatesResult
getLoadBalancerTlsCertificates = Request.request serviceName "getLoadBalancerTlsCertificates" 


-- | <p>Returns information about all load balancers in an account.</p> <p>If you are describing a long list of load balancers, you can paginate the output to make the list more manageable. You can use the pageToken and nextPageToken values to retrieve the next items in the list.</p>
getLoadBalancers :: forall eff. GetLoadBalancersRequest -> Aff (exception :: EXCEPTION | eff) GetLoadBalancersResult
getLoadBalancers = Request.request serviceName "getLoadBalancers" 


-- | <p>Returns information about a specific operation. Operations include events such as when you create an instance, allocate a static IP, attach a static IP, and so on.</p>
getOperation :: forall eff. GetOperationRequest -> Aff (exception :: EXCEPTION | eff) GetOperationResult
getOperation = Request.request serviceName "getOperation" 


-- | <p>Returns information about all operations.</p> <p>Results are returned from oldest to newest, up to a maximum of 200. Results can be paged by making each subsequent call to <code>GetOperations</code> use the maximum (last) <code>statusChangedAt</code> value from the previous request.</p>
getOperations :: forall eff. GetOperationsRequest -> Aff (exception :: EXCEPTION | eff) GetOperationsResult
getOperations = Request.request serviceName "getOperations" 


-- | <p>Gets operations for a specific resource (e.g., an instance or a static IP).</p>
getOperationsForResource :: forall eff. GetOperationsForResourceRequest -> Aff (exception :: EXCEPTION | eff) GetOperationsForResourceResult
getOperationsForResource = Request.request serviceName "getOperationsForResource" 


-- | <p>Returns a list of all valid regions for Amazon Lightsail. Use the <code>include availability zones</code> parameter to also return the availability zones in a region.</p>
getRegions :: forall eff. GetRegionsRequest -> Aff (exception :: EXCEPTION | eff) GetRegionsResult
getRegions = Request.request serviceName "getRegions" 


-- | <p>Returns information about a specific static IP.</p>
getStaticIp :: forall eff. GetStaticIpRequest -> Aff (exception :: EXCEPTION | eff) GetStaticIpResult
getStaticIp = Request.request serviceName "getStaticIp" 


-- | <p>Returns information about all static IPs in the user's account.</p>
getStaticIps :: forall eff. GetStaticIpsRequest -> Aff (exception :: EXCEPTION | eff) GetStaticIpsResult
getStaticIps = Request.request serviceName "getStaticIps" 


-- | <p>Imports a public SSH key from a specific key pair.</p>
importKeyPair :: forall eff. ImportKeyPairRequest -> Aff (exception :: EXCEPTION | eff) ImportKeyPairResult
importKeyPair = Request.request serviceName "importKeyPair" 


-- | <p>Returns a Boolean value indicating whether your Lightsail VPC is peered.</p>
isVpcPeered :: forall eff. IsVpcPeeredRequest -> Aff (exception :: EXCEPTION | eff) IsVpcPeeredResult
isVpcPeered = Request.request serviceName "isVpcPeered" 


-- | <p>Adds public ports to an Amazon Lightsail instance.</p>
openInstancePublicPorts :: forall eff. OpenInstancePublicPortsRequest -> Aff (exception :: EXCEPTION | eff) OpenInstancePublicPortsResult
openInstancePublicPorts = Request.request serviceName "openInstancePublicPorts" 


-- | <p>Tries to peer the Lightsail VPC with the user's default VPC.</p>
peerVpc :: forall eff. PeerVpcRequest -> Aff (exception :: EXCEPTION | eff) PeerVpcResult
peerVpc = Request.request serviceName "peerVpc" 


-- | <p>Sets the specified open ports for an Amazon Lightsail instance, and closes all ports for every protocol not included in the current request.</p>
putInstancePublicPorts :: forall eff. PutInstancePublicPortsRequest -> Aff (exception :: EXCEPTION | eff) PutInstancePublicPortsResult
putInstancePublicPorts = Request.request serviceName "putInstancePublicPorts" 


-- | <p>Restarts a specific instance. When your Amazon Lightsail instance is finished rebooting, Lightsail assigns a new public IP address. To use the same IP address after restarting, create a static IP address and attach it to the instance.</p>
rebootInstance :: forall eff. RebootInstanceRequest -> Aff (exception :: EXCEPTION | eff) RebootInstanceResult
rebootInstance = Request.request serviceName "rebootInstance" 


-- | <p>Deletes a specific static IP from your account.</p>
releaseStaticIp :: forall eff. ReleaseStaticIpRequest -> Aff (exception :: EXCEPTION | eff) ReleaseStaticIpResult
releaseStaticIp = Request.request serviceName "releaseStaticIp" 


-- | <p>Starts a specific Amazon Lightsail instance from a stopped state. To restart an instance, use the reboot instance operation.</p>
startInstance :: forall eff. StartInstanceRequest -> Aff (exception :: EXCEPTION | eff) StartInstanceResult
startInstance = Request.request serviceName "startInstance" 


-- | <p>Stops a specific Amazon Lightsail instance that is currently running.</p>
stopInstance :: forall eff. StopInstanceRequest -> Aff (exception :: EXCEPTION | eff) StopInstanceResult
stopInstance = Request.request serviceName "stopInstance" 


-- | <p>Attempts to unpeer the Lightsail VPC from the user's default VPC.</p>
unpeerVpc :: forall eff. UnpeerVpcRequest -> Aff (exception :: EXCEPTION | eff) UnpeerVpcResult
unpeerVpc = Request.request serviceName "unpeerVpc" 


-- | <p>Updates a domain recordset after it is created.</p>
updateDomainEntry :: forall eff. UpdateDomainEntryRequest -> Aff (exception :: EXCEPTION | eff) UpdateDomainEntryResult
updateDomainEntry = Request.request serviceName "updateDomainEntry" 


-- | <p>Updates the specified attribute for a load balancer.</p>
updateLoadBalancerAttribute :: forall eff. UpdateLoadBalancerAttributeRequest -> Aff (exception :: EXCEPTION | eff) UpdateLoadBalancerAttributeResult
updateLoadBalancerAttribute = Request.request serviceName "updateLoadBalancerAttribute" 


-- | <p>Lightsail throws this exception when the user cannot be authenticated or uses invalid credentials to access a resource.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _
derive instance repGenericAccessDeniedException :: Generic AccessDeniedException _
instance showAccessDeniedException :: Show AccessDeniedException where
  show = genericShow
instance decodeAccessDeniedException :: Decode AccessDeniedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessDeniedException :: Encode AccessDeniedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccessDirection = AccessDirection String
derive instance newtypeAccessDirection :: Newtype AccessDirection _
derive instance repGenericAccessDirection :: Generic AccessDirection _
instance showAccessDirection :: Show AccessDirection where
  show = genericShow
instance decodeAccessDirection :: Decode AccessDirection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessDirection :: Encode AccessDirection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lightsail throws this exception when an account is still in the setup in progress state.</p>
newtype AccountSetupInProgressException = AccountSetupInProgressException 
  { "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAccountSetupInProgressException :: Newtype AccountSetupInProgressException _
derive instance repGenericAccountSetupInProgressException :: Generic AccountSetupInProgressException _
instance showAccountSetupInProgressException :: Show AccountSetupInProgressException where
  show = genericShow
instance decodeAccountSetupInProgressException :: Decode AccountSetupInProgressException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountSetupInProgressException :: Encode AccountSetupInProgressException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllocateStaticIpRequest = AllocateStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  }
derive instance newtypeAllocateStaticIpRequest :: Newtype AllocateStaticIpRequest _
derive instance repGenericAllocateStaticIpRequest :: Generic AllocateStaticIpRequest _
instance showAllocateStaticIpRequest :: Show AllocateStaticIpRequest where
  show = genericShow
instance decodeAllocateStaticIpRequest :: Decode AllocateStaticIpRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllocateStaticIpRequest :: Encode AllocateStaticIpRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllocateStaticIpResult = AllocateStaticIpResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeAllocateStaticIpResult :: Newtype AllocateStaticIpResult _
derive instance repGenericAllocateStaticIpResult :: Generic AllocateStaticIpResult _
instance showAllocateStaticIpResult :: Show AllocateStaticIpResult where
  show = genericShow
instance decodeAllocateStaticIpResult :: Decode AllocateStaticIpResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllocateStaticIpResult :: Encode AllocateStaticIpResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachDiskRequest = AttachDiskRequest 
  { "DiskName'" :: (ResourceName)
  , "InstanceName'" :: (ResourceName)
  , "DiskPath'" :: (NonEmptyString)
  }
derive instance newtypeAttachDiskRequest :: Newtype AttachDiskRequest _
derive instance repGenericAttachDiskRequest :: Generic AttachDiskRequest _
instance showAttachDiskRequest :: Show AttachDiskRequest where
  show = genericShow
instance decodeAttachDiskRequest :: Decode AttachDiskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachDiskRequest :: Encode AttachDiskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachDiskResult = AttachDiskResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeAttachDiskResult :: Newtype AttachDiskResult _
derive instance repGenericAttachDiskResult :: Generic AttachDiskResult _
instance showAttachDiskResult :: Show AttachDiskResult where
  show = genericShow
instance decodeAttachDiskResult :: Decode AttachDiskResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachDiskResult :: Encode AttachDiskResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachInstancesToLoadBalancerRequest = AttachInstancesToLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "InstanceNames'" :: (ResourceNameList)
  }
derive instance newtypeAttachInstancesToLoadBalancerRequest :: Newtype AttachInstancesToLoadBalancerRequest _
derive instance repGenericAttachInstancesToLoadBalancerRequest :: Generic AttachInstancesToLoadBalancerRequest _
instance showAttachInstancesToLoadBalancerRequest :: Show AttachInstancesToLoadBalancerRequest where
  show = genericShow
instance decodeAttachInstancesToLoadBalancerRequest :: Decode AttachInstancesToLoadBalancerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachInstancesToLoadBalancerRequest :: Encode AttachInstancesToLoadBalancerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachInstancesToLoadBalancerResult = AttachInstancesToLoadBalancerResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeAttachInstancesToLoadBalancerResult :: Newtype AttachInstancesToLoadBalancerResult _
derive instance repGenericAttachInstancesToLoadBalancerResult :: Generic AttachInstancesToLoadBalancerResult _
instance showAttachInstancesToLoadBalancerResult :: Show AttachInstancesToLoadBalancerResult where
  show = genericShow
instance decodeAttachInstancesToLoadBalancerResult :: Decode AttachInstancesToLoadBalancerResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachInstancesToLoadBalancerResult :: Encode AttachInstancesToLoadBalancerResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachLoadBalancerTlsCertificateRequest = AttachLoadBalancerTlsCertificateRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "CertificateName'" :: (ResourceName)
  }
derive instance newtypeAttachLoadBalancerTlsCertificateRequest :: Newtype AttachLoadBalancerTlsCertificateRequest _
derive instance repGenericAttachLoadBalancerTlsCertificateRequest :: Generic AttachLoadBalancerTlsCertificateRequest _
instance showAttachLoadBalancerTlsCertificateRequest :: Show AttachLoadBalancerTlsCertificateRequest where
  show = genericShow
instance decodeAttachLoadBalancerTlsCertificateRequest :: Decode AttachLoadBalancerTlsCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachLoadBalancerTlsCertificateRequest :: Encode AttachLoadBalancerTlsCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachLoadBalancerTlsCertificateResult = AttachLoadBalancerTlsCertificateResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeAttachLoadBalancerTlsCertificateResult :: Newtype AttachLoadBalancerTlsCertificateResult _
derive instance repGenericAttachLoadBalancerTlsCertificateResult :: Generic AttachLoadBalancerTlsCertificateResult _
instance showAttachLoadBalancerTlsCertificateResult :: Show AttachLoadBalancerTlsCertificateResult where
  show = genericShow
instance decodeAttachLoadBalancerTlsCertificateResult :: Decode AttachLoadBalancerTlsCertificateResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachLoadBalancerTlsCertificateResult :: Encode AttachLoadBalancerTlsCertificateResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachStaticIpRequest = AttachStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  , "InstanceName'" :: (ResourceName)
  }
derive instance newtypeAttachStaticIpRequest :: Newtype AttachStaticIpRequest _
derive instance repGenericAttachStaticIpRequest :: Generic AttachStaticIpRequest _
instance showAttachStaticIpRequest :: Show AttachStaticIpRequest where
  show = genericShow
instance decodeAttachStaticIpRequest :: Decode AttachStaticIpRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachStaticIpRequest :: Encode AttachStaticIpRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachStaticIpResult = AttachStaticIpResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeAttachStaticIpResult :: Newtype AttachStaticIpResult _
derive instance repGenericAttachStaticIpResult :: Generic AttachStaticIpResult _
instance showAttachStaticIpResult :: Show AttachStaticIpResult where
  show = genericShow
instance decodeAttachStaticIpResult :: Decode AttachStaticIpResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachStaticIpResult :: Encode AttachStaticIpResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachedDiskMap = AttachedDiskMap (StrMap.StrMap DiskMapList)
derive instance newtypeAttachedDiskMap :: Newtype AttachedDiskMap _
derive instance repGenericAttachedDiskMap :: Generic AttachedDiskMap _
instance showAttachedDiskMap :: Show AttachedDiskMap where
  show = genericShow
instance decodeAttachedDiskMap :: Decode AttachedDiskMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachedDiskMap :: Encode AttachedDiskMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an Availability Zone.</p>
newtype AvailabilityZone = AvailabilityZone 
  { "ZoneName'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "State'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _
derive instance repGenericAvailabilityZone :: Generic AvailabilityZone _
instance showAvailabilityZone :: Show AvailabilityZone where
  show = genericShow
instance decodeAvailabilityZone :: Decode AvailabilityZone where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZone :: Encode AvailabilityZone where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailabilityZoneList = AvailabilityZoneList (Array AvailabilityZone)
derive instance newtypeAvailabilityZoneList :: Newtype AvailabilityZoneList _
derive instance repGenericAvailabilityZoneList :: Generic AvailabilityZoneList _
instance showAvailabilityZoneList :: Show AvailabilityZoneList where
  show = genericShow
instance decodeAvailabilityZoneList :: Decode AvailabilityZoneList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZoneList :: Encode AvailabilityZoneList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Base64 = Base64 String
derive instance newtypeBase64 :: Newtype Base64 _
derive instance repGenericBase64 :: Generic Base64 _
instance showBase64 :: Show Base64 where
  show = genericShow
instance decodeBase64 :: Decode Base64 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBase64 :: Encode Base64 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a blueprint (a virtual private server image).</p>
newtype Blueprint = Blueprint 
  { "BlueprintId'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Group'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Type'" :: NullOrUndefined.NullOrUndefined (BlueprintType)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "IsActive'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "MinPower'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Version'" :: NullOrUndefined.NullOrUndefined (String)
  , "VersionCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "ProductUrl'" :: NullOrUndefined.NullOrUndefined (String)
  , "LicenseUrl'" :: NullOrUndefined.NullOrUndefined (String)
  , "Platform'" :: NullOrUndefined.NullOrUndefined (InstancePlatform)
  }
derive instance newtypeBlueprint :: Newtype Blueprint _
derive instance repGenericBlueprint :: Generic Blueprint _
instance showBlueprint :: Show Blueprint where
  show = genericShow
instance decodeBlueprint :: Decode Blueprint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlueprint :: Encode Blueprint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BlueprintList = BlueprintList (Array Blueprint)
derive instance newtypeBlueprintList :: Newtype BlueprintList _
derive instance repGenericBlueprintList :: Generic BlueprintList _
instance showBlueprintList :: Show BlueprintList where
  show = genericShow
instance decodeBlueprintList :: Decode BlueprintList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlueprintList :: Encode BlueprintList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BlueprintType = BlueprintType String
derive instance newtypeBlueprintType :: Newtype BlueprintType _
derive instance repGenericBlueprintType :: Generic BlueprintType _
instance showBlueprintType :: Show BlueprintType where
  show = genericShow
instance decodeBlueprintType :: Decode BlueprintType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlueprintType :: Encode BlueprintType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a bundle, which is a set of specs describing your virtual private server (or <i>instance</i>).</p>
newtype Bundle = Bundle 
  { "Price'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CpuCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "DiskSizeInGb'" :: NullOrUndefined.NullOrUndefined (Int)
  , "BundleId'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "InstanceType'" :: NullOrUndefined.NullOrUndefined (String)
  , "IsActive'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Power'" :: NullOrUndefined.NullOrUndefined (Int)
  , "RamSizeInGb'" :: NullOrUndefined.NullOrUndefined (Number)
  , "TransferPerMonthInGb'" :: NullOrUndefined.NullOrUndefined (Int)
  , "SupportedPlatforms'" :: NullOrUndefined.NullOrUndefined (InstancePlatformList)
  }
derive instance newtypeBundle :: Newtype Bundle _
derive instance repGenericBundle :: Generic Bundle _
instance showBundle :: Show Bundle where
  show = genericShow
instance decodeBundle :: Decode Bundle where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBundle :: Encode Bundle where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BundleList = BundleList (Array Bundle)
derive instance newtypeBundleList :: Newtype BundleList _
derive instance repGenericBundleList :: Generic BundleList _
instance showBundleList :: Show BundleList where
  show = genericShow
instance decodeBundleList :: Decode BundleList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBundleList :: Encode BundleList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloseInstancePublicPortsRequest = CloseInstancePublicPortsRequest 
  { "PortInfo'" :: (PortInfo)
  , "InstanceName'" :: (ResourceName)
  }
derive instance newtypeCloseInstancePublicPortsRequest :: Newtype CloseInstancePublicPortsRequest _
derive instance repGenericCloseInstancePublicPortsRequest :: Generic CloseInstancePublicPortsRequest _
instance showCloseInstancePublicPortsRequest :: Show CloseInstancePublicPortsRequest where
  show = genericShow
instance decodeCloseInstancePublicPortsRequest :: Decode CloseInstancePublicPortsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloseInstancePublicPortsRequest :: Encode CloseInstancePublicPortsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloseInstancePublicPortsResult = CloseInstancePublicPortsResult 
  { "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypeCloseInstancePublicPortsResult :: Newtype CloseInstancePublicPortsResult _
derive instance repGenericCloseInstancePublicPortsResult :: Generic CloseInstancePublicPortsResult _
instance showCloseInstancePublicPortsResult :: Show CloseInstancePublicPortsResult where
  show = genericShow
instance decodeCloseInstancePublicPortsResult :: Decode CloseInstancePublicPortsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloseInstancePublicPortsResult :: Encode CloseInstancePublicPortsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDiskFromSnapshotRequest = CreateDiskFromSnapshotRequest 
  { "DiskName'" :: (ResourceName)
  , "DiskSnapshotName'" :: (ResourceName)
  , "AvailabilityZone'" :: (NonEmptyString)
  , "SizeInGb'" :: (Int)
  }
derive instance newtypeCreateDiskFromSnapshotRequest :: Newtype CreateDiskFromSnapshotRequest _
derive instance repGenericCreateDiskFromSnapshotRequest :: Generic CreateDiskFromSnapshotRequest _
instance showCreateDiskFromSnapshotRequest :: Show CreateDiskFromSnapshotRequest where
  show = genericShow
instance decodeCreateDiskFromSnapshotRequest :: Decode CreateDiskFromSnapshotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDiskFromSnapshotRequest :: Encode CreateDiskFromSnapshotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDiskFromSnapshotResult = CreateDiskFromSnapshotResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeCreateDiskFromSnapshotResult :: Newtype CreateDiskFromSnapshotResult _
derive instance repGenericCreateDiskFromSnapshotResult :: Generic CreateDiskFromSnapshotResult _
instance showCreateDiskFromSnapshotResult :: Show CreateDiskFromSnapshotResult where
  show = genericShow
instance decodeCreateDiskFromSnapshotResult :: Decode CreateDiskFromSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDiskFromSnapshotResult :: Encode CreateDiskFromSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDiskRequest = CreateDiskRequest 
  { "DiskName'" :: (ResourceName)
  , "AvailabilityZone'" :: (NonEmptyString)
  , "SizeInGb'" :: (Int)
  }
derive instance newtypeCreateDiskRequest :: Newtype CreateDiskRequest _
derive instance repGenericCreateDiskRequest :: Generic CreateDiskRequest _
instance showCreateDiskRequest :: Show CreateDiskRequest where
  show = genericShow
instance decodeCreateDiskRequest :: Decode CreateDiskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDiskRequest :: Encode CreateDiskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDiskResult = CreateDiskResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeCreateDiskResult :: Newtype CreateDiskResult _
derive instance repGenericCreateDiskResult :: Generic CreateDiskResult _
instance showCreateDiskResult :: Show CreateDiskResult where
  show = genericShow
instance decodeCreateDiskResult :: Decode CreateDiskResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDiskResult :: Encode CreateDiskResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDiskSnapshotRequest = CreateDiskSnapshotRequest 
  { "DiskName'" :: (ResourceName)
  , "DiskSnapshotName'" :: (ResourceName)
  }
derive instance newtypeCreateDiskSnapshotRequest :: Newtype CreateDiskSnapshotRequest _
derive instance repGenericCreateDiskSnapshotRequest :: Generic CreateDiskSnapshotRequest _
instance showCreateDiskSnapshotRequest :: Show CreateDiskSnapshotRequest where
  show = genericShow
instance decodeCreateDiskSnapshotRequest :: Decode CreateDiskSnapshotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDiskSnapshotRequest :: Encode CreateDiskSnapshotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDiskSnapshotResult = CreateDiskSnapshotResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeCreateDiskSnapshotResult :: Newtype CreateDiskSnapshotResult _
derive instance repGenericCreateDiskSnapshotResult :: Generic CreateDiskSnapshotResult _
instance showCreateDiskSnapshotResult :: Show CreateDiskSnapshotResult where
  show = genericShow
instance decodeCreateDiskSnapshotResult :: Decode CreateDiskSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDiskSnapshotResult :: Encode CreateDiskSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDomainEntryRequest = CreateDomainEntryRequest 
  { "DomainName'" :: (DomainName)
  , "DomainEntry'" :: (DomainEntry)
  }
derive instance newtypeCreateDomainEntryRequest :: Newtype CreateDomainEntryRequest _
derive instance repGenericCreateDomainEntryRequest :: Generic CreateDomainEntryRequest _
instance showCreateDomainEntryRequest :: Show CreateDomainEntryRequest where
  show = genericShow
instance decodeCreateDomainEntryRequest :: Decode CreateDomainEntryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDomainEntryRequest :: Encode CreateDomainEntryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDomainEntryResult = CreateDomainEntryResult 
  { "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypeCreateDomainEntryResult :: Newtype CreateDomainEntryResult _
derive instance repGenericCreateDomainEntryResult :: Generic CreateDomainEntryResult _
instance showCreateDomainEntryResult :: Show CreateDomainEntryResult where
  show = genericShow
instance decodeCreateDomainEntryResult :: Decode CreateDomainEntryResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDomainEntryResult :: Encode CreateDomainEntryResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDomainRequest = CreateDomainRequest 
  { "DomainName'" :: (DomainName)
  }
derive instance newtypeCreateDomainRequest :: Newtype CreateDomainRequest _
derive instance repGenericCreateDomainRequest :: Generic CreateDomainRequest _
instance showCreateDomainRequest :: Show CreateDomainRequest where
  show = genericShow
instance decodeCreateDomainRequest :: Decode CreateDomainRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDomainRequest :: Encode CreateDomainRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDomainResult = CreateDomainResult 
  { "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypeCreateDomainResult :: Newtype CreateDomainResult _
derive instance repGenericCreateDomainResult :: Generic CreateDomainResult _
instance showCreateDomainResult :: Show CreateDomainResult where
  show = genericShow
instance decodeCreateDomainResult :: Decode CreateDomainResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDomainResult :: Encode CreateDomainResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateInstanceSnapshotRequest = CreateInstanceSnapshotRequest 
  { "InstanceSnapshotName'" :: (ResourceName)
  , "InstanceName'" :: (ResourceName)
  }
derive instance newtypeCreateInstanceSnapshotRequest :: Newtype CreateInstanceSnapshotRequest _
derive instance repGenericCreateInstanceSnapshotRequest :: Generic CreateInstanceSnapshotRequest _
instance showCreateInstanceSnapshotRequest :: Show CreateInstanceSnapshotRequest where
  show = genericShow
instance decodeCreateInstanceSnapshotRequest :: Decode CreateInstanceSnapshotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInstanceSnapshotRequest :: Encode CreateInstanceSnapshotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateInstanceSnapshotResult = CreateInstanceSnapshotResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeCreateInstanceSnapshotResult :: Newtype CreateInstanceSnapshotResult _
derive instance repGenericCreateInstanceSnapshotResult :: Generic CreateInstanceSnapshotResult _
instance showCreateInstanceSnapshotResult :: Show CreateInstanceSnapshotResult where
  show = genericShow
instance decodeCreateInstanceSnapshotResult :: Decode CreateInstanceSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInstanceSnapshotResult :: Encode CreateInstanceSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateInstancesFromSnapshotRequest = CreateInstancesFromSnapshotRequest 
  { "InstanceNames'" :: (StringList)
  , "AttachedDiskMapping'" :: NullOrUndefined.NullOrUndefined (AttachedDiskMap)
  , "AvailabilityZone'" :: (String)
  , "InstanceSnapshotName'" :: (ResourceName)
  , "BundleId'" :: (NonEmptyString)
  , "UserData'" :: NullOrUndefined.NullOrUndefined (String)
  , "KeyPairName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  }
derive instance newtypeCreateInstancesFromSnapshotRequest :: Newtype CreateInstancesFromSnapshotRequest _
derive instance repGenericCreateInstancesFromSnapshotRequest :: Generic CreateInstancesFromSnapshotRequest _
instance showCreateInstancesFromSnapshotRequest :: Show CreateInstancesFromSnapshotRequest where
  show = genericShow
instance decodeCreateInstancesFromSnapshotRequest :: Decode CreateInstancesFromSnapshotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInstancesFromSnapshotRequest :: Encode CreateInstancesFromSnapshotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateInstancesFromSnapshotResult = CreateInstancesFromSnapshotResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeCreateInstancesFromSnapshotResult :: Newtype CreateInstancesFromSnapshotResult _
derive instance repGenericCreateInstancesFromSnapshotResult :: Generic CreateInstancesFromSnapshotResult _
instance showCreateInstancesFromSnapshotResult :: Show CreateInstancesFromSnapshotResult where
  show = genericShow
instance decodeCreateInstancesFromSnapshotResult :: Decode CreateInstancesFromSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInstancesFromSnapshotResult :: Encode CreateInstancesFromSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateInstancesRequest = CreateInstancesRequest 
  { "InstanceNames'" :: (StringList)
  , "AvailabilityZone'" :: (String)
  , "CustomImageName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "BlueprintId'" :: (NonEmptyString)
  , "BundleId'" :: (NonEmptyString)
  , "UserData'" :: NullOrUndefined.NullOrUndefined (String)
  , "KeyPairName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  }
derive instance newtypeCreateInstancesRequest :: Newtype CreateInstancesRequest _
derive instance repGenericCreateInstancesRequest :: Generic CreateInstancesRequest _
instance showCreateInstancesRequest :: Show CreateInstancesRequest where
  show = genericShow
instance decodeCreateInstancesRequest :: Decode CreateInstancesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInstancesRequest :: Encode CreateInstancesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateInstancesResult = CreateInstancesResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeCreateInstancesResult :: Newtype CreateInstancesResult _
derive instance repGenericCreateInstancesResult :: Generic CreateInstancesResult _
instance showCreateInstancesResult :: Show CreateInstancesResult where
  show = genericShow
instance decodeCreateInstancesResult :: Decode CreateInstancesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInstancesResult :: Encode CreateInstancesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateKeyPairRequest = CreateKeyPairRequest 
  { "KeyPairName'" :: (ResourceName)
  }
derive instance newtypeCreateKeyPairRequest :: Newtype CreateKeyPairRequest _
derive instance repGenericCreateKeyPairRequest :: Generic CreateKeyPairRequest _
instance showCreateKeyPairRequest :: Show CreateKeyPairRequest where
  show = genericShow
instance decodeCreateKeyPairRequest :: Decode CreateKeyPairRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateKeyPairRequest :: Encode CreateKeyPairRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateKeyPairResult = CreateKeyPairResult 
  { "KeyPair'" :: NullOrUndefined.NullOrUndefined (KeyPair)
  , "PublicKeyBase64'" :: NullOrUndefined.NullOrUndefined (Base64)
  , "PrivateKeyBase64'" :: NullOrUndefined.NullOrUndefined (Base64)
  , "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypeCreateKeyPairResult :: Newtype CreateKeyPairResult _
derive instance repGenericCreateKeyPairResult :: Generic CreateKeyPairResult _
instance showCreateKeyPairResult :: Show CreateKeyPairResult where
  show = genericShow
instance decodeCreateKeyPairResult :: Decode CreateKeyPairResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateKeyPairResult :: Encode CreateKeyPairResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLoadBalancerRequest = CreateLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "InstancePort'" :: (Port)
  , "HealthCheckPath'" :: NullOrUndefined.NullOrUndefined (String)
  , "CertificateName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "CertificateDomainName'" :: NullOrUndefined.NullOrUndefined (DomainName)
  , "CertificateAlternativeNames'" :: NullOrUndefined.NullOrUndefined (DomainNameList)
  }
derive instance newtypeCreateLoadBalancerRequest :: Newtype CreateLoadBalancerRequest _
derive instance repGenericCreateLoadBalancerRequest :: Generic CreateLoadBalancerRequest _
instance showCreateLoadBalancerRequest :: Show CreateLoadBalancerRequest where
  show = genericShow
instance decodeCreateLoadBalancerRequest :: Decode CreateLoadBalancerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoadBalancerRequest :: Encode CreateLoadBalancerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLoadBalancerResult = CreateLoadBalancerResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeCreateLoadBalancerResult :: Newtype CreateLoadBalancerResult _
derive instance repGenericCreateLoadBalancerResult :: Generic CreateLoadBalancerResult _
instance showCreateLoadBalancerResult :: Show CreateLoadBalancerResult where
  show = genericShow
instance decodeCreateLoadBalancerResult :: Decode CreateLoadBalancerResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoadBalancerResult :: Encode CreateLoadBalancerResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLoadBalancerTlsCertificateRequest = CreateLoadBalancerTlsCertificateRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "CertificateName'" :: (ResourceName)
  , "CertificateDomainName'" :: (DomainName)
  , "CertificateAlternativeNames'" :: NullOrUndefined.NullOrUndefined (DomainNameList)
  }
derive instance newtypeCreateLoadBalancerTlsCertificateRequest :: Newtype CreateLoadBalancerTlsCertificateRequest _
derive instance repGenericCreateLoadBalancerTlsCertificateRequest :: Generic CreateLoadBalancerTlsCertificateRequest _
instance showCreateLoadBalancerTlsCertificateRequest :: Show CreateLoadBalancerTlsCertificateRequest where
  show = genericShow
instance decodeCreateLoadBalancerTlsCertificateRequest :: Decode CreateLoadBalancerTlsCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoadBalancerTlsCertificateRequest :: Encode CreateLoadBalancerTlsCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLoadBalancerTlsCertificateResult = CreateLoadBalancerTlsCertificateResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeCreateLoadBalancerTlsCertificateResult :: Newtype CreateLoadBalancerTlsCertificateResult _
derive instance repGenericCreateLoadBalancerTlsCertificateResult :: Generic CreateLoadBalancerTlsCertificateResult _
instance showCreateLoadBalancerTlsCertificateResult :: Show CreateLoadBalancerTlsCertificateResult where
  show = genericShow
instance decodeCreateLoadBalancerTlsCertificateResult :: Decode CreateLoadBalancerTlsCertificateResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoadBalancerTlsCertificateResult :: Encode CreateLoadBalancerTlsCertificateResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDiskRequest = DeleteDiskRequest 
  { "DiskName'" :: (ResourceName)
  }
derive instance newtypeDeleteDiskRequest :: Newtype DeleteDiskRequest _
derive instance repGenericDeleteDiskRequest :: Generic DeleteDiskRequest _
instance showDeleteDiskRequest :: Show DeleteDiskRequest where
  show = genericShow
instance decodeDeleteDiskRequest :: Decode DeleteDiskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDiskRequest :: Encode DeleteDiskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDiskResult = DeleteDiskResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeDeleteDiskResult :: Newtype DeleteDiskResult _
derive instance repGenericDeleteDiskResult :: Generic DeleteDiskResult _
instance showDeleteDiskResult :: Show DeleteDiskResult where
  show = genericShow
instance decodeDeleteDiskResult :: Decode DeleteDiskResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDiskResult :: Encode DeleteDiskResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDiskSnapshotRequest = DeleteDiskSnapshotRequest 
  { "DiskSnapshotName'" :: (ResourceName)
  }
derive instance newtypeDeleteDiskSnapshotRequest :: Newtype DeleteDiskSnapshotRequest _
derive instance repGenericDeleteDiskSnapshotRequest :: Generic DeleteDiskSnapshotRequest _
instance showDeleteDiskSnapshotRequest :: Show DeleteDiskSnapshotRequest where
  show = genericShow
instance decodeDeleteDiskSnapshotRequest :: Decode DeleteDiskSnapshotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDiskSnapshotRequest :: Encode DeleteDiskSnapshotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDiskSnapshotResult = DeleteDiskSnapshotResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeDeleteDiskSnapshotResult :: Newtype DeleteDiskSnapshotResult _
derive instance repGenericDeleteDiskSnapshotResult :: Generic DeleteDiskSnapshotResult _
instance showDeleteDiskSnapshotResult :: Show DeleteDiskSnapshotResult where
  show = genericShow
instance decodeDeleteDiskSnapshotResult :: Decode DeleteDiskSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDiskSnapshotResult :: Encode DeleteDiskSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDomainEntryRequest = DeleteDomainEntryRequest 
  { "DomainName'" :: (DomainName)
  , "DomainEntry'" :: (DomainEntry)
  }
derive instance newtypeDeleteDomainEntryRequest :: Newtype DeleteDomainEntryRequest _
derive instance repGenericDeleteDomainEntryRequest :: Generic DeleteDomainEntryRequest _
instance showDeleteDomainEntryRequest :: Show DeleteDomainEntryRequest where
  show = genericShow
instance decodeDeleteDomainEntryRequest :: Decode DeleteDomainEntryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDomainEntryRequest :: Encode DeleteDomainEntryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDomainEntryResult = DeleteDomainEntryResult 
  { "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypeDeleteDomainEntryResult :: Newtype DeleteDomainEntryResult _
derive instance repGenericDeleteDomainEntryResult :: Generic DeleteDomainEntryResult _
instance showDeleteDomainEntryResult :: Show DeleteDomainEntryResult where
  show = genericShow
instance decodeDeleteDomainEntryResult :: Decode DeleteDomainEntryResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDomainEntryResult :: Encode DeleteDomainEntryResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDomainRequest = DeleteDomainRequest 
  { "DomainName'" :: (DomainName)
  }
derive instance newtypeDeleteDomainRequest :: Newtype DeleteDomainRequest _
derive instance repGenericDeleteDomainRequest :: Generic DeleteDomainRequest _
instance showDeleteDomainRequest :: Show DeleteDomainRequest where
  show = genericShow
instance decodeDeleteDomainRequest :: Decode DeleteDomainRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDomainRequest :: Encode DeleteDomainRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDomainResult = DeleteDomainResult 
  { "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypeDeleteDomainResult :: Newtype DeleteDomainResult _
derive instance repGenericDeleteDomainResult :: Generic DeleteDomainResult _
instance showDeleteDomainResult :: Show DeleteDomainResult where
  show = genericShow
instance decodeDeleteDomainResult :: Decode DeleteDomainResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDomainResult :: Encode DeleteDomainResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteInstanceRequest = DeleteInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  }
derive instance newtypeDeleteInstanceRequest :: Newtype DeleteInstanceRequest _
derive instance repGenericDeleteInstanceRequest :: Generic DeleteInstanceRequest _
instance showDeleteInstanceRequest :: Show DeleteInstanceRequest where
  show = genericShow
instance decodeDeleteInstanceRequest :: Decode DeleteInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteInstanceRequest :: Encode DeleteInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteInstanceResult = DeleteInstanceResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeDeleteInstanceResult :: Newtype DeleteInstanceResult _
derive instance repGenericDeleteInstanceResult :: Generic DeleteInstanceResult _
instance showDeleteInstanceResult :: Show DeleteInstanceResult where
  show = genericShow
instance decodeDeleteInstanceResult :: Decode DeleteInstanceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteInstanceResult :: Encode DeleteInstanceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteInstanceSnapshotRequest = DeleteInstanceSnapshotRequest 
  { "InstanceSnapshotName'" :: (ResourceName)
  }
derive instance newtypeDeleteInstanceSnapshotRequest :: Newtype DeleteInstanceSnapshotRequest _
derive instance repGenericDeleteInstanceSnapshotRequest :: Generic DeleteInstanceSnapshotRequest _
instance showDeleteInstanceSnapshotRequest :: Show DeleteInstanceSnapshotRequest where
  show = genericShow
instance decodeDeleteInstanceSnapshotRequest :: Decode DeleteInstanceSnapshotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteInstanceSnapshotRequest :: Encode DeleteInstanceSnapshotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteInstanceSnapshotResult = DeleteInstanceSnapshotResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeDeleteInstanceSnapshotResult :: Newtype DeleteInstanceSnapshotResult _
derive instance repGenericDeleteInstanceSnapshotResult :: Generic DeleteInstanceSnapshotResult _
instance showDeleteInstanceSnapshotResult :: Show DeleteInstanceSnapshotResult where
  show = genericShow
instance decodeDeleteInstanceSnapshotResult :: Decode DeleteInstanceSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteInstanceSnapshotResult :: Encode DeleteInstanceSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteKeyPairRequest = DeleteKeyPairRequest 
  { "KeyPairName'" :: (ResourceName)
  }
derive instance newtypeDeleteKeyPairRequest :: Newtype DeleteKeyPairRequest _
derive instance repGenericDeleteKeyPairRequest :: Generic DeleteKeyPairRequest _
instance showDeleteKeyPairRequest :: Show DeleteKeyPairRequest where
  show = genericShow
instance decodeDeleteKeyPairRequest :: Decode DeleteKeyPairRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteKeyPairRequest :: Encode DeleteKeyPairRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteKeyPairResult = DeleteKeyPairResult 
  { "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypeDeleteKeyPairResult :: Newtype DeleteKeyPairResult _
derive instance repGenericDeleteKeyPairResult :: Generic DeleteKeyPairResult _
instance showDeleteKeyPairResult :: Show DeleteKeyPairResult where
  show = genericShow
instance decodeDeleteKeyPairResult :: Decode DeleteKeyPairResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteKeyPairResult :: Encode DeleteKeyPairResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLoadBalancerRequest = DeleteLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  }
derive instance newtypeDeleteLoadBalancerRequest :: Newtype DeleteLoadBalancerRequest _
derive instance repGenericDeleteLoadBalancerRequest :: Generic DeleteLoadBalancerRequest _
instance showDeleteLoadBalancerRequest :: Show DeleteLoadBalancerRequest where
  show = genericShow
instance decodeDeleteLoadBalancerRequest :: Decode DeleteLoadBalancerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLoadBalancerRequest :: Encode DeleteLoadBalancerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLoadBalancerResult = DeleteLoadBalancerResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeDeleteLoadBalancerResult :: Newtype DeleteLoadBalancerResult _
derive instance repGenericDeleteLoadBalancerResult :: Generic DeleteLoadBalancerResult _
instance showDeleteLoadBalancerResult :: Show DeleteLoadBalancerResult where
  show = genericShow
instance decodeDeleteLoadBalancerResult :: Decode DeleteLoadBalancerResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLoadBalancerResult :: Encode DeleteLoadBalancerResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLoadBalancerTlsCertificateRequest = DeleteLoadBalancerTlsCertificateRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "CertificateName'" :: (ResourceName)
  , "Force'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeDeleteLoadBalancerTlsCertificateRequest :: Newtype DeleteLoadBalancerTlsCertificateRequest _
derive instance repGenericDeleteLoadBalancerTlsCertificateRequest :: Generic DeleteLoadBalancerTlsCertificateRequest _
instance showDeleteLoadBalancerTlsCertificateRequest :: Show DeleteLoadBalancerTlsCertificateRequest where
  show = genericShow
instance decodeDeleteLoadBalancerTlsCertificateRequest :: Decode DeleteLoadBalancerTlsCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLoadBalancerTlsCertificateRequest :: Encode DeleteLoadBalancerTlsCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLoadBalancerTlsCertificateResult = DeleteLoadBalancerTlsCertificateResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeDeleteLoadBalancerTlsCertificateResult :: Newtype DeleteLoadBalancerTlsCertificateResult _
derive instance repGenericDeleteLoadBalancerTlsCertificateResult :: Generic DeleteLoadBalancerTlsCertificateResult _
instance showDeleteLoadBalancerTlsCertificateResult :: Show DeleteLoadBalancerTlsCertificateResult where
  show = genericShow
instance decodeDeleteLoadBalancerTlsCertificateResult :: Decode DeleteLoadBalancerTlsCertificateResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLoadBalancerTlsCertificateResult :: Encode DeleteLoadBalancerTlsCertificateResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachDiskRequest = DetachDiskRequest 
  { "DiskName'" :: (ResourceName)
  }
derive instance newtypeDetachDiskRequest :: Newtype DetachDiskRequest _
derive instance repGenericDetachDiskRequest :: Generic DetachDiskRequest _
instance showDetachDiskRequest :: Show DetachDiskRequest where
  show = genericShow
instance decodeDetachDiskRequest :: Decode DetachDiskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachDiskRequest :: Encode DetachDiskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachDiskResult = DetachDiskResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeDetachDiskResult :: Newtype DetachDiskResult _
derive instance repGenericDetachDiskResult :: Generic DetachDiskResult _
instance showDetachDiskResult :: Show DetachDiskResult where
  show = genericShow
instance decodeDetachDiskResult :: Decode DetachDiskResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachDiskResult :: Encode DetachDiskResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachInstancesFromLoadBalancerRequest = DetachInstancesFromLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "InstanceNames'" :: (ResourceNameList)
  }
derive instance newtypeDetachInstancesFromLoadBalancerRequest :: Newtype DetachInstancesFromLoadBalancerRequest _
derive instance repGenericDetachInstancesFromLoadBalancerRequest :: Generic DetachInstancesFromLoadBalancerRequest _
instance showDetachInstancesFromLoadBalancerRequest :: Show DetachInstancesFromLoadBalancerRequest where
  show = genericShow
instance decodeDetachInstancesFromLoadBalancerRequest :: Decode DetachInstancesFromLoadBalancerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachInstancesFromLoadBalancerRequest :: Encode DetachInstancesFromLoadBalancerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachInstancesFromLoadBalancerResult = DetachInstancesFromLoadBalancerResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeDetachInstancesFromLoadBalancerResult :: Newtype DetachInstancesFromLoadBalancerResult _
derive instance repGenericDetachInstancesFromLoadBalancerResult :: Generic DetachInstancesFromLoadBalancerResult _
instance showDetachInstancesFromLoadBalancerResult :: Show DetachInstancesFromLoadBalancerResult where
  show = genericShow
instance decodeDetachInstancesFromLoadBalancerResult :: Decode DetachInstancesFromLoadBalancerResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachInstancesFromLoadBalancerResult :: Encode DetachInstancesFromLoadBalancerResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachStaticIpRequest = DetachStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  }
derive instance newtypeDetachStaticIpRequest :: Newtype DetachStaticIpRequest _
derive instance repGenericDetachStaticIpRequest :: Generic DetachStaticIpRequest _
instance showDetachStaticIpRequest :: Show DetachStaticIpRequest where
  show = genericShow
instance decodeDetachStaticIpRequest :: Decode DetachStaticIpRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachStaticIpRequest :: Encode DetachStaticIpRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachStaticIpResult = DetachStaticIpResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeDetachStaticIpResult :: Newtype DetachStaticIpResult _
derive instance repGenericDetachStaticIpResult :: Generic DetachStaticIpResult _
instance showDetachStaticIpResult :: Show DetachStaticIpResult where
  show = genericShow
instance decodeDetachStaticIpResult :: Decode DetachStaticIpResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachStaticIpResult :: Encode DetachStaticIpResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a system disk or an block storage disk.</p>
newtype Disk = Disk 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined.NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "SizeInGb'" :: NullOrUndefined.NullOrUndefined (Int)
  , "IsSystemDisk'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Iops'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Path'" :: NullOrUndefined.NullOrUndefined (String)
  , "State'" :: NullOrUndefined.NullOrUndefined (DiskState)
  , "AttachedTo'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "IsAttached'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "AttachmentState'" :: NullOrUndefined.NullOrUndefined (String)
  , "GbInUse'" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeDisk :: Newtype Disk _
derive instance repGenericDisk :: Generic Disk _
instance showDisk :: Show Disk where
  show = genericShow
instance decodeDisk :: Decode Disk where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisk :: Encode Disk where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DiskList = DiskList (Array Disk)
derive instance newtypeDiskList :: Newtype DiskList _
derive instance repGenericDiskList :: Generic DiskList _
instance showDiskList :: Show DiskList where
  show = genericShow
instance decodeDiskList :: Decode DiskList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiskList :: Encode DiskList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a block storage disk mapping.</p>
newtype DiskMap = DiskMap 
  { "OriginalDiskPath'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "NewDiskName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  }
derive instance newtypeDiskMap :: Newtype DiskMap _
derive instance repGenericDiskMap :: Generic DiskMap _
instance showDiskMap :: Show DiskMap where
  show = genericShow
instance decodeDiskMap :: Decode DiskMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiskMap :: Encode DiskMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DiskMapList = DiskMapList (Array DiskMap)
derive instance newtypeDiskMapList :: Newtype DiskMapList _
derive instance repGenericDiskMapList :: Generic DiskMapList _
instance showDiskMapList :: Show DiskMapList where
  show = genericShow
instance decodeDiskMapList :: Decode DiskMapList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiskMapList :: Encode DiskMapList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a block storage disk snapshot.</p>
newtype DiskSnapshot = DiskSnapshot 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined.NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "SizeInGb'" :: NullOrUndefined.NullOrUndefined (Int)
  , "State'" :: NullOrUndefined.NullOrUndefined (DiskSnapshotState)
  , "Progress'" :: NullOrUndefined.NullOrUndefined (String)
  , "FromDiskName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "FromDiskArn'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeDiskSnapshot :: Newtype DiskSnapshot _
derive instance repGenericDiskSnapshot :: Generic DiskSnapshot _
instance showDiskSnapshot :: Show DiskSnapshot where
  show = genericShow
instance decodeDiskSnapshot :: Decode DiskSnapshot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiskSnapshot :: Encode DiskSnapshot where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DiskSnapshotList = DiskSnapshotList (Array DiskSnapshot)
derive instance newtypeDiskSnapshotList :: Newtype DiskSnapshotList _
derive instance repGenericDiskSnapshotList :: Generic DiskSnapshotList _
instance showDiskSnapshotList :: Show DiskSnapshotList where
  show = genericShow
instance decodeDiskSnapshotList :: Decode DiskSnapshotList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiskSnapshotList :: Encode DiskSnapshotList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DiskSnapshotState = DiskSnapshotState String
derive instance newtypeDiskSnapshotState :: Newtype DiskSnapshotState _
derive instance repGenericDiskSnapshotState :: Generic DiskSnapshotState _
instance showDiskSnapshotState :: Show DiskSnapshotState where
  show = genericShow
instance decodeDiskSnapshotState :: Decode DiskSnapshotState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiskSnapshotState :: Encode DiskSnapshotState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DiskState = DiskState String
derive instance newtypeDiskState :: Newtype DiskState _
derive instance repGenericDiskState :: Generic DiskState _
instance showDiskState :: Show DiskState where
  show = genericShow
instance decodeDiskState :: Decode DiskState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiskState :: Encode DiskState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a domain where you are storing recordsets in Lightsail.</p>
newtype Domain = Domain 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined.NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "DomainEntries'" :: NullOrUndefined.NullOrUndefined (DomainEntryList)
  }
derive instance newtypeDomain :: Newtype Domain _
derive instance repGenericDomain :: Generic Domain _
instance showDomain :: Show Domain where
  show = genericShow
instance decodeDomain :: Decode Domain where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomain :: Encode Domain where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a domain recordset entry.</p>
newtype DomainEntry = DomainEntry 
  { "Id'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Name'" :: NullOrUndefined.NullOrUndefined (DomainName)
  , "Target'" :: NullOrUndefined.NullOrUndefined (String)
  , "IsAlias'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Type'" :: NullOrUndefined.NullOrUndefined (DomainEntryType)
  , "Options'" :: NullOrUndefined.NullOrUndefined (DomainEntryOptions)
  }
derive instance newtypeDomainEntry :: Newtype DomainEntry _
derive instance repGenericDomainEntry :: Generic DomainEntry _
instance showDomainEntry :: Show DomainEntry where
  show = genericShow
instance decodeDomainEntry :: Decode DomainEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainEntry :: Encode DomainEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainEntryList = DomainEntryList (Array DomainEntry)
derive instance newtypeDomainEntryList :: Newtype DomainEntryList _
derive instance repGenericDomainEntryList :: Generic DomainEntryList _
instance showDomainEntryList :: Show DomainEntryList where
  show = genericShow
instance decodeDomainEntryList :: Decode DomainEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainEntryList :: Encode DomainEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainEntryOptions = DomainEntryOptions (StrMap.StrMap String)
derive instance newtypeDomainEntryOptions :: Newtype DomainEntryOptions _
derive instance repGenericDomainEntryOptions :: Generic DomainEntryOptions _
instance showDomainEntryOptions :: Show DomainEntryOptions where
  show = genericShow
instance decodeDomainEntryOptions :: Decode DomainEntryOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainEntryOptions :: Encode DomainEntryOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainEntryOptionsKeys = DomainEntryOptionsKeys String
derive instance newtypeDomainEntryOptionsKeys :: Newtype DomainEntryOptionsKeys _
derive instance repGenericDomainEntryOptionsKeys :: Generic DomainEntryOptionsKeys _
instance showDomainEntryOptionsKeys :: Show DomainEntryOptionsKeys where
  show = genericShow
instance decodeDomainEntryOptionsKeys :: Decode DomainEntryOptionsKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainEntryOptionsKeys :: Encode DomainEntryOptionsKeys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainEntryType = DomainEntryType String
derive instance newtypeDomainEntryType :: Newtype DomainEntryType _
derive instance repGenericDomainEntryType :: Generic DomainEntryType _
instance showDomainEntryType :: Show DomainEntryType where
  show = genericShow
instance decodeDomainEntryType :: Decode DomainEntryType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainEntryType :: Encode DomainEntryType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainList = DomainList (Array Domain)
derive instance newtypeDomainList :: Newtype DomainList _
derive instance repGenericDomainList :: Generic DomainList _
instance showDomainList :: Show DomainList where
  show = genericShow
instance decodeDomainList :: Decode DomainList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainList :: Encode DomainList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainName = DomainName String
derive instance newtypeDomainName :: Newtype DomainName _
derive instance repGenericDomainName :: Generic DomainName _
instance showDomainName :: Show DomainName where
  show = genericShow
instance decodeDomainName :: Decode DomainName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainName :: Encode DomainName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainNameList = DomainNameList (Array DomainName)
derive instance newtypeDomainNameList :: Newtype DomainNameList _
derive instance repGenericDomainNameList :: Generic DomainNameList _
instance showDomainNameList :: Show DomainNameList where
  show = genericShow
instance decodeDomainNameList :: Decode DomainNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainNameList :: Encode DomainNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DownloadDefaultKeyPairRequest = DownloadDefaultKeyPairRequest Types.NoArguments
derive instance newtypeDownloadDefaultKeyPairRequest :: Newtype DownloadDefaultKeyPairRequest _
derive instance repGenericDownloadDefaultKeyPairRequest :: Generic DownloadDefaultKeyPairRequest _
instance showDownloadDefaultKeyPairRequest :: Show DownloadDefaultKeyPairRequest where
  show = genericShow
instance decodeDownloadDefaultKeyPairRequest :: Decode DownloadDefaultKeyPairRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDownloadDefaultKeyPairRequest :: Encode DownloadDefaultKeyPairRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DownloadDefaultKeyPairResult = DownloadDefaultKeyPairResult 
  { "PublicKeyBase64'" :: NullOrUndefined.NullOrUndefined (Base64)
  , "PrivateKeyBase64'" :: NullOrUndefined.NullOrUndefined (Base64)
  }
derive instance newtypeDownloadDefaultKeyPairResult :: Newtype DownloadDefaultKeyPairResult _
derive instance repGenericDownloadDefaultKeyPairResult :: Generic DownloadDefaultKeyPairResult _
instance showDownloadDefaultKeyPairResult :: Show DownloadDefaultKeyPairResult where
  show = genericShow
instance decodeDownloadDefaultKeyPairResult :: Decode DownloadDefaultKeyPairResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDownloadDefaultKeyPairResult :: Encode DownloadDefaultKeyPairResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetActiveNamesRequest = GetActiveNamesRequest 
  { "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetActiveNamesRequest :: Newtype GetActiveNamesRequest _
derive instance repGenericGetActiveNamesRequest :: Generic GetActiveNamesRequest _
instance showGetActiveNamesRequest :: Show GetActiveNamesRequest where
  show = genericShow
instance decodeGetActiveNamesRequest :: Decode GetActiveNamesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetActiveNamesRequest :: Encode GetActiveNamesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetActiveNamesResult = GetActiveNamesResult 
  { "ActiveNames'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetActiveNamesResult :: Newtype GetActiveNamesResult _
derive instance repGenericGetActiveNamesResult :: Generic GetActiveNamesResult _
instance showGetActiveNamesResult :: Show GetActiveNamesResult where
  show = genericShow
instance decodeGetActiveNamesResult :: Decode GetActiveNamesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetActiveNamesResult :: Encode GetActiveNamesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBlueprintsRequest = GetBlueprintsRequest 
  { "IncludeInactive'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetBlueprintsRequest :: Newtype GetBlueprintsRequest _
derive instance repGenericGetBlueprintsRequest :: Generic GetBlueprintsRequest _
instance showGetBlueprintsRequest :: Show GetBlueprintsRequest where
  show = genericShow
instance decodeGetBlueprintsRequest :: Decode GetBlueprintsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBlueprintsRequest :: Encode GetBlueprintsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBlueprintsResult = GetBlueprintsResult 
  { "Blueprints'" :: NullOrUndefined.NullOrUndefined (BlueprintList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetBlueprintsResult :: Newtype GetBlueprintsResult _
derive instance repGenericGetBlueprintsResult :: Generic GetBlueprintsResult _
instance showGetBlueprintsResult :: Show GetBlueprintsResult where
  show = genericShow
instance decodeGetBlueprintsResult :: Decode GetBlueprintsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBlueprintsResult :: Encode GetBlueprintsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBundlesRequest = GetBundlesRequest 
  { "IncludeInactive'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetBundlesRequest :: Newtype GetBundlesRequest _
derive instance repGenericGetBundlesRequest :: Generic GetBundlesRequest _
instance showGetBundlesRequest :: Show GetBundlesRequest where
  show = genericShow
instance decodeGetBundlesRequest :: Decode GetBundlesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBundlesRequest :: Encode GetBundlesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBundlesResult = GetBundlesResult 
  { "Bundles'" :: NullOrUndefined.NullOrUndefined (BundleList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetBundlesResult :: Newtype GetBundlesResult _
derive instance repGenericGetBundlesResult :: Generic GetBundlesResult _
instance showGetBundlesResult :: Show GetBundlesResult where
  show = genericShow
instance decodeGetBundlesResult :: Decode GetBundlesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBundlesResult :: Encode GetBundlesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDiskRequest = GetDiskRequest 
  { "DiskName'" :: (ResourceName)
  }
derive instance newtypeGetDiskRequest :: Newtype GetDiskRequest _
derive instance repGenericGetDiskRequest :: Generic GetDiskRequest _
instance showGetDiskRequest :: Show GetDiskRequest where
  show = genericShow
instance decodeGetDiskRequest :: Decode GetDiskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDiskRequest :: Encode GetDiskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDiskResult = GetDiskResult 
  { "Disk'" :: NullOrUndefined.NullOrUndefined (Disk)
  }
derive instance newtypeGetDiskResult :: Newtype GetDiskResult _
derive instance repGenericGetDiskResult :: Generic GetDiskResult _
instance showGetDiskResult :: Show GetDiskResult where
  show = genericShow
instance decodeGetDiskResult :: Decode GetDiskResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDiskResult :: Encode GetDiskResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDiskSnapshotRequest = GetDiskSnapshotRequest 
  { "DiskSnapshotName'" :: (ResourceName)
  }
derive instance newtypeGetDiskSnapshotRequest :: Newtype GetDiskSnapshotRequest _
derive instance repGenericGetDiskSnapshotRequest :: Generic GetDiskSnapshotRequest _
instance showGetDiskSnapshotRequest :: Show GetDiskSnapshotRequest where
  show = genericShow
instance decodeGetDiskSnapshotRequest :: Decode GetDiskSnapshotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDiskSnapshotRequest :: Encode GetDiskSnapshotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDiskSnapshotResult = GetDiskSnapshotResult 
  { "DiskSnapshot'" :: NullOrUndefined.NullOrUndefined (DiskSnapshot)
  }
derive instance newtypeGetDiskSnapshotResult :: Newtype GetDiskSnapshotResult _
derive instance repGenericGetDiskSnapshotResult :: Generic GetDiskSnapshotResult _
instance showGetDiskSnapshotResult :: Show GetDiskSnapshotResult where
  show = genericShow
instance decodeGetDiskSnapshotResult :: Decode GetDiskSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDiskSnapshotResult :: Encode GetDiskSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDiskSnapshotsRequest = GetDiskSnapshotsRequest 
  { "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetDiskSnapshotsRequest :: Newtype GetDiskSnapshotsRequest _
derive instance repGenericGetDiskSnapshotsRequest :: Generic GetDiskSnapshotsRequest _
instance showGetDiskSnapshotsRequest :: Show GetDiskSnapshotsRequest where
  show = genericShow
instance decodeGetDiskSnapshotsRequest :: Decode GetDiskSnapshotsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDiskSnapshotsRequest :: Encode GetDiskSnapshotsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDiskSnapshotsResult = GetDiskSnapshotsResult 
  { "DiskSnapshots'" :: NullOrUndefined.NullOrUndefined (DiskSnapshotList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetDiskSnapshotsResult :: Newtype GetDiskSnapshotsResult _
derive instance repGenericGetDiskSnapshotsResult :: Generic GetDiskSnapshotsResult _
instance showGetDiskSnapshotsResult :: Show GetDiskSnapshotsResult where
  show = genericShow
instance decodeGetDiskSnapshotsResult :: Decode GetDiskSnapshotsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDiskSnapshotsResult :: Encode GetDiskSnapshotsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDisksRequest = GetDisksRequest 
  { "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetDisksRequest :: Newtype GetDisksRequest _
derive instance repGenericGetDisksRequest :: Generic GetDisksRequest _
instance showGetDisksRequest :: Show GetDisksRequest where
  show = genericShow
instance decodeGetDisksRequest :: Decode GetDisksRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDisksRequest :: Encode GetDisksRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDisksResult = GetDisksResult 
  { "Disks'" :: NullOrUndefined.NullOrUndefined (DiskList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetDisksResult :: Newtype GetDisksResult _
derive instance repGenericGetDisksResult :: Generic GetDisksResult _
instance showGetDisksResult :: Show GetDisksResult where
  show = genericShow
instance decodeGetDisksResult :: Decode GetDisksResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDisksResult :: Encode GetDisksResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDomainRequest = GetDomainRequest 
  { "DomainName'" :: (DomainName)
  }
derive instance newtypeGetDomainRequest :: Newtype GetDomainRequest _
derive instance repGenericGetDomainRequest :: Generic GetDomainRequest _
instance showGetDomainRequest :: Show GetDomainRequest where
  show = genericShow
instance decodeGetDomainRequest :: Decode GetDomainRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDomainRequest :: Encode GetDomainRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDomainResult = GetDomainResult 
  { "Domain'" :: NullOrUndefined.NullOrUndefined (Domain)
  }
derive instance newtypeGetDomainResult :: Newtype GetDomainResult _
derive instance repGenericGetDomainResult :: Generic GetDomainResult _
instance showGetDomainResult :: Show GetDomainResult where
  show = genericShow
instance decodeGetDomainResult :: Decode GetDomainResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDomainResult :: Encode GetDomainResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDomainsRequest = GetDomainsRequest 
  { "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetDomainsRequest :: Newtype GetDomainsRequest _
derive instance repGenericGetDomainsRequest :: Generic GetDomainsRequest _
instance showGetDomainsRequest :: Show GetDomainsRequest where
  show = genericShow
instance decodeGetDomainsRequest :: Decode GetDomainsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDomainsRequest :: Encode GetDomainsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDomainsResult = GetDomainsResult 
  { "Domains'" :: NullOrUndefined.NullOrUndefined (DomainList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetDomainsResult :: Newtype GetDomainsResult _
derive instance repGenericGetDomainsResult :: Generic GetDomainsResult _
instance showGetDomainsResult :: Show GetDomainsResult where
  show = genericShow
instance decodeGetDomainsResult :: Decode GetDomainsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDomainsResult :: Encode GetDomainsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstanceAccessDetailsRequest = GetInstanceAccessDetailsRequest 
  { "InstanceName'" :: (ResourceName)
  , "Protocol'" :: NullOrUndefined.NullOrUndefined (InstanceAccessProtocol)
  }
derive instance newtypeGetInstanceAccessDetailsRequest :: Newtype GetInstanceAccessDetailsRequest _
derive instance repGenericGetInstanceAccessDetailsRequest :: Generic GetInstanceAccessDetailsRequest _
instance showGetInstanceAccessDetailsRequest :: Show GetInstanceAccessDetailsRequest where
  show = genericShow
instance decodeGetInstanceAccessDetailsRequest :: Decode GetInstanceAccessDetailsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceAccessDetailsRequest :: Encode GetInstanceAccessDetailsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstanceAccessDetailsResult = GetInstanceAccessDetailsResult 
  { "AccessDetails'" :: NullOrUndefined.NullOrUndefined (InstanceAccessDetails)
  }
derive instance newtypeGetInstanceAccessDetailsResult :: Newtype GetInstanceAccessDetailsResult _
derive instance repGenericGetInstanceAccessDetailsResult :: Generic GetInstanceAccessDetailsResult _
instance showGetInstanceAccessDetailsResult :: Show GetInstanceAccessDetailsResult where
  show = genericShow
instance decodeGetInstanceAccessDetailsResult :: Decode GetInstanceAccessDetailsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceAccessDetailsResult :: Encode GetInstanceAccessDetailsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


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
derive instance repGenericGetInstanceMetricDataRequest :: Generic GetInstanceMetricDataRequest _
instance showGetInstanceMetricDataRequest :: Show GetInstanceMetricDataRequest where
  show = genericShow
instance decodeGetInstanceMetricDataRequest :: Decode GetInstanceMetricDataRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceMetricDataRequest :: Encode GetInstanceMetricDataRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstanceMetricDataResult = GetInstanceMetricDataResult 
  { "MetricName'" :: NullOrUndefined.NullOrUndefined (InstanceMetricName)
  , "MetricData'" :: NullOrUndefined.NullOrUndefined (MetricDatapointList)
  }
derive instance newtypeGetInstanceMetricDataResult :: Newtype GetInstanceMetricDataResult _
derive instance repGenericGetInstanceMetricDataResult :: Generic GetInstanceMetricDataResult _
instance showGetInstanceMetricDataResult :: Show GetInstanceMetricDataResult where
  show = genericShow
instance decodeGetInstanceMetricDataResult :: Decode GetInstanceMetricDataResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceMetricDataResult :: Encode GetInstanceMetricDataResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstancePortStatesRequest = GetInstancePortStatesRequest 
  { "InstanceName'" :: (ResourceName)
  }
derive instance newtypeGetInstancePortStatesRequest :: Newtype GetInstancePortStatesRequest _
derive instance repGenericGetInstancePortStatesRequest :: Generic GetInstancePortStatesRequest _
instance showGetInstancePortStatesRequest :: Show GetInstancePortStatesRequest where
  show = genericShow
instance decodeGetInstancePortStatesRequest :: Decode GetInstancePortStatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstancePortStatesRequest :: Encode GetInstancePortStatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstancePortStatesResult = GetInstancePortStatesResult 
  { "PortStates'" :: NullOrUndefined.NullOrUndefined (InstancePortStateList)
  }
derive instance newtypeGetInstancePortStatesResult :: Newtype GetInstancePortStatesResult _
derive instance repGenericGetInstancePortStatesResult :: Generic GetInstancePortStatesResult _
instance showGetInstancePortStatesResult :: Show GetInstancePortStatesResult where
  show = genericShow
instance decodeGetInstancePortStatesResult :: Decode GetInstancePortStatesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstancePortStatesResult :: Encode GetInstancePortStatesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstanceRequest = GetInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  }
derive instance newtypeGetInstanceRequest :: Newtype GetInstanceRequest _
derive instance repGenericGetInstanceRequest :: Generic GetInstanceRequest _
instance showGetInstanceRequest :: Show GetInstanceRequest where
  show = genericShow
instance decodeGetInstanceRequest :: Decode GetInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceRequest :: Encode GetInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstanceResult = GetInstanceResult 
  { "Instance'" :: NullOrUndefined.NullOrUndefined (Instance)
  }
derive instance newtypeGetInstanceResult :: Newtype GetInstanceResult _
derive instance repGenericGetInstanceResult :: Generic GetInstanceResult _
instance showGetInstanceResult :: Show GetInstanceResult where
  show = genericShow
instance decodeGetInstanceResult :: Decode GetInstanceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceResult :: Encode GetInstanceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstanceSnapshotRequest = GetInstanceSnapshotRequest 
  { "InstanceSnapshotName'" :: (ResourceName)
  }
derive instance newtypeGetInstanceSnapshotRequest :: Newtype GetInstanceSnapshotRequest _
derive instance repGenericGetInstanceSnapshotRequest :: Generic GetInstanceSnapshotRequest _
instance showGetInstanceSnapshotRequest :: Show GetInstanceSnapshotRequest where
  show = genericShow
instance decodeGetInstanceSnapshotRequest :: Decode GetInstanceSnapshotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceSnapshotRequest :: Encode GetInstanceSnapshotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstanceSnapshotResult = GetInstanceSnapshotResult 
  { "InstanceSnapshot'" :: NullOrUndefined.NullOrUndefined (InstanceSnapshot)
  }
derive instance newtypeGetInstanceSnapshotResult :: Newtype GetInstanceSnapshotResult _
derive instance repGenericGetInstanceSnapshotResult :: Generic GetInstanceSnapshotResult _
instance showGetInstanceSnapshotResult :: Show GetInstanceSnapshotResult where
  show = genericShow
instance decodeGetInstanceSnapshotResult :: Decode GetInstanceSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceSnapshotResult :: Encode GetInstanceSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstanceSnapshotsRequest = GetInstanceSnapshotsRequest 
  { "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetInstanceSnapshotsRequest :: Newtype GetInstanceSnapshotsRequest _
derive instance repGenericGetInstanceSnapshotsRequest :: Generic GetInstanceSnapshotsRequest _
instance showGetInstanceSnapshotsRequest :: Show GetInstanceSnapshotsRequest where
  show = genericShow
instance decodeGetInstanceSnapshotsRequest :: Decode GetInstanceSnapshotsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceSnapshotsRequest :: Encode GetInstanceSnapshotsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstanceSnapshotsResult = GetInstanceSnapshotsResult 
  { "InstanceSnapshots'" :: NullOrUndefined.NullOrUndefined (InstanceSnapshotList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetInstanceSnapshotsResult :: Newtype GetInstanceSnapshotsResult _
derive instance repGenericGetInstanceSnapshotsResult :: Generic GetInstanceSnapshotsResult _
instance showGetInstanceSnapshotsResult :: Show GetInstanceSnapshotsResult where
  show = genericShow
instance decodeGetInstanceSnapshotsResult :: Decode GetInstanceSnapshotsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceSnapshotsResult :: Encode GetInstanceSnapshotsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstanceStateRequest = GetInstanceStateRequest 
  { "InstanceName'" :: (ResourceName)
  }
derive instance newtypeGetInstanceStateRequest :: Newtype GetInstanceStateRequest _
derive instance repGenericGetInstanceStateRequest :: Generic GetInstanceStateRequest _
instance showGetInstanceStateRequest :: Show GetInstanceStateRequest where
  show = genericShow
instance decodeGetInstanceStateRequest :: Decode GetInstanceStateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceStateRequest :: Encode GetInstanceStateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstanceStateResult = GetInstanceStateResult 
  { "State'" :: NullOrUndefined.NullOrUndefined (InstanceState)
  }
derive instance newtypeGetInstanceStateResult :: Newtype GetInstanceStateResult _
derive instance repGenericGetInstanceStateResult :: Generic GetInstanceStateResult _
instance showGetInstanceStateResult :: Show GetInstanceStateResult where
  show = genericShow
instance decodeGetInstanceStateResult :: Decode GetInstanceStateResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceStateResult :: Encode GetInstanceStateResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstancesRequest = GetInstancesRequest 
  { "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetInstancesRequest :: Newtype GetInstancesRequest _
derive instance repGenericGetInstancesRequest :: Generic GetInstancesRequest _
instance showGetInstancesRequest :: Show GetInstancesRequest where
  show = genericShow
instance decodeGetInstancesRequest :: Decode GetInstancesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstancesRequest :: Encode GetInstancesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInstancesResult = GetInstancesResult 
  { "Instances'" :: NullOrUndefined.NullOrUndefined (InstanceList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetInstancesResult :: Newtype GetInstancesResult _
derive instance repGenericGetInstancesResult :: Generic GetInstancesResult _
instance showGetInstancesResult :: Show GetInstancesResult where
  show = genericShow
instance decodeGetInstancesResult :: Decode GetInstancesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstancesResult :: Encode GetInstancesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetKeyPairRequest = GetKeyPairRequest 
  { "KeyPairName'" :: (ResourceName)
  }
derive instance newtypeGetKeyPairRequest :: Newtype GetKeyPairRequest _
derive instance repGenericGetKeyPairRequest :: Generic GetKeyPairRequest _
instance showGetKeyPairRequest :: Show GetKeyPairRequest where
  show = genericShow
instance decodeGetKeyPairRequest :: Decode GetKeyPairRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetKeyPairRequest :: Encode GetKeyPairRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetKeyPairResult = GetKeyPairResult 
  { "KeyPair'" :: NullOrUndefined.NullOrUndefined (KeyPair)
  }
derive instance newtypeGetKeyPairResult :: Newtype GetKeyPairResult _
derive instance repGenericGetKeyPairResult :: Generic GetKeyPairResult _
instance showGetKeyPairResult :: Show GetKeyPairResult where
  show = genericShow
instance decodeGetKeyPairResult :: Decode GetKeyPairResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetKeyPairResult :: Encode GetKeyPairResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetKeyPairsRequest = GetKeyPairsRequest 
  { "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetKeyPairsRequest :: Newtype GetKeyPairsRequest _
derive instance repGenericGetKeyPairsRequest :: Generic GetKeyPairsRequest _
instance showGetKeyPairsRequest :: Show GetKeyPairsRequest where
  show = genericShow
instance decodeGetKeyPairsRequest :: Decode GetKeyPairsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetKeyPairsRequest :: Encode GetKeyPairsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetKeyPairsResult = GetKeyPairsResult 
  { "KeyPairs'" :: NullOrUndefined.NullOrUndefined (KeyPairList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetKeyPairsResult :: Newtype GetKeyPairsResult _
derive instance repGenericGetKeyPairsResult :: Generic GetKeyPairsResult _
instance showGetKeyPairsResult :: Show GetKeyPairsResult where
  show = genericShow
instance decodeGetKeyPairsResult :: Decode GetKeyPairsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetKeyPairsResult :: Encode GetKeyPairsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


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
derive instance repGenericGetLoadBalancerMetricDataRequest :: Generic GetLoadBalancerMetricDataRequest _
instance showGetLoadBalancerMetricDataRequest :: Show GetLoadBalancerMetricDataRequest where
  show = genericShow
instance decodeGetLoadBalancerMetricDataRequest :: Decode GetLoadBalancerMetricDataRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoadBalancerMetricDataRequest :: Encode GetLoadBalancerMetricDataRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLoadBalancerMetricDataResult = GetLoadBalancerMetricDataResult 
  { "MetricName'" :: NullOrUndefined.NullOrUndefined (LoadBalancerMetricName)
  , "MetricData'" :: NullOrUndefined.NullOrUndefined (MetricDatapointList)
  }
derive instance newtypeGetLoadBalancerMetricDataResult :: Newtype GetLoadBalancerMetricDataResult _
derive instance repGenericGetLoadBalancerMetricDataResult :: Generic GetLoadBalancerMetricDataResult _
instance showGetLoadBalancerMetricDataResult :: Show GetLoadBalancerMetricDataResult where
  show = genericShow
instance decodeGetLoadBalancerMetricDataResult :: Decode GetLoadBalancerMetricDataResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoadBalancerMetricDataResult :: Encode GetLoadBalancerMetricDataResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLoadBalancerRequest = GetLoadBalancerRequest 
  { "LoadBalancerName'" :: (ResourceName)
  }
derive instance newtypeGetLoadBalancerRequest :: Newtype GetLoadBalancerRequest _
derive instance repGenericGetLoadBalancerRequest :: Generic GetLoadBalancerRequest _
instance showGetLoadBalancerRequest :: Show GetLoadBalancerRequest where
  show = genericShow
instance decodeGetLoadBalancerRequest :: Decode GetLoadBalancerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoadBalancerRequest :: Encode GetLoadBalancerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLoadBalancerResult = GetLoadBalancerResult 
  { "LoadBalancer'" :: NullOrUndefined.NullOrUndefined (LoadBalancer)
  }
derive instance newtypeGetLoadBalancerResult :: Newtype GetLoadBalancerResult _
derive instance repGenericGetLoadBalancerResult :: Generic GetLoadBalancerResult _
instance showGetLoadBalancerResult :: Show GetLoadBalancerResult where
  show = genericShow
instance decodeGetLoadBalancerResult :: Decode GetLoadBalancerResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoadBalancerResult :: Encode GetLoadBalancerResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLoadBalancerTlsCertificatesRequest = GetLoadBalancerTlsCertificatesRequest 
  { "LoadBalancerName'" :: (ResourceName)
  }
derive instance newtypeGetLoadBalancerTlsCertificatesRequest :: Newtype GetLoadBalancerTlsCertificatesRequest _
derive instance repGenericGetLoadBalancerTlsCertificatesRequest :: Generic GetLoadBalancerTlsCertificatesRequest _
instance showGetLoadBalancerTlsCertificatesRequest :: Show GetLoadBalancerTlsCertificatesRequest where
  show = genericShow
instance decodeGetLoadBalancerTlsCertificatesRequest :: Decode GetLoadBalancerTlsCertificatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoadBalancerTlsCertificatesRequest :: Encode GetLoadBalancerTlsCertificatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLoadBalancerTlsCertificatesResult = GetLoadBalancerTlsCertificatesResult 
  { "TlsCertificates'" :: NullOrUndefined.NullOrUndefined (LoadBalancerTlsCertificateList)
  }
derive instance newtypeGetLoadBalancerTlsCertificatesResult :: Newtype GetLoadBalancerTlsCertificatesResult _
derive instance repGenericGetLoadBalancerTlsCertificatesResult :: Generic GetLoadBalancerTlsCertificatesResult _
instance showGetLoadBalancerTlsCertificatesResult :: Show GetLoadBalancerTlsCertificatesResult where
  show = genericShow
instance decodeGetLoadBalancerTlsCertificatesResult :: Decode GetLoadBalancerTlsCertificatesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoadBalancerTlsCertificatesResult :: Encode GetLoadBalancerTlsCertificatesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLoadBalancersRequest = GetLoadBalancersRequest 
  { "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetLoadBalancersRequest :: Newtype GetLoadBalancersRequest _
derive instance repGenericGetLoadBalancersRequest :: Generic GetLoadBalancersRequest _
instance showGetLoadBalancersRequest :: Show GetLoadBalancersRequest where
  show = genericShow
instance decodeGetLoadBalancersRequest :: Decode GetLoadBalancersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoadBalancersRequest :: Encode GetLoadBalancersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLoadBalancersResult = GetLoadBalancersResult 
  { "LoadBalancers'" :: NullOrUndefined.NullOrUndefined (LoadBalancerList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetLoadBalancersResult :: Newtype GetLoadBalancersResult _
derive instance repGenericGetLoadBalancersResult :: Generic GetLoadBalancersResult _
instance showGetLoadBalancersResult :: Show GetLoadBalancersResult where
  show = genericShow
instance decodeGetLoadBalancersResult :: Decode GetLoadBalancersResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoadBalancersResult :: Encode GetLoadBalancersResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetOperationRequest = GetOperationRequest 
  { "OperationId'" :: (NonEmptyString)
  }
derive instance newtypeGetOperationRequest :: Newtype GetOperationRequest _
derive instance repGenericGetOperationRequest :: Generic GetOperationRequest _
instance showGetOperationRequest :: Show GetOperationRequest where
  show = genericShow
instance decodeGetOperationRequest :: Decode GetOperationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOperationRequest :: Encode GetOperationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetOperationResult = GetOperationResult 
  { "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypeGetOperationResult :: Newtype GetOperationResult _
derive instance repGenericGetOperationResult :: Generic GetOperationResult _
instance showGetOperationResult :: Show GetOperationResult where
  show = genericShow
instance decodeGetOperationResult :: Decode GetOperationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOperationResult :: Encode GetOperationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetOperationsForResourceRequest = GetOperationsForResourceRequest 
  { "ResourceName'" :: (ResourceName)
  , "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetOperationsForResourceRequest :: Newtype GetOperationsForResourceRequest _
derive instance repGenericGetOperationsForResourceRequest :: Generic GetOperationsForResourceRequest _
instance showGetOperationsForResourceRequest :: Show GetOperationsForResourceRequest where
  show = genericShow
instance decodeGetOperationsForResourceRequest :: Decode GetOperationsForResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOperationsForResourceRequest :: Encode GetOperationsForResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetOperationsForResourceResult = GetOperationsForResourceResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  , "NextPageCount'" :: NullOrUndefined.NullOrUndefined (String)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetOperationsForResourceResult :: Newtype GetOperationsForResourceResult _
derive instance repGenericGetOperationsForResourceResult :: Generic GetOperationsForResourceResult _
instance showGetOperationsForResourceResult :: Show GetOperationsForResourceResult where
  show = genericShow
instance decodeGetOperationsForResourceResult :: Decode GetOperationsForResourceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOperationsForResourceResult :: Encode GetOperationsForResourceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetOperationsRequest = GetOperationsRequest 
  { "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetOperationsRequest :: Newtype GetOperationsRequest _
derive instance repGenericGetOperationsRequest :: Generic GetOperationsRequest _
instance showGetOperationsRequest :: Show GetOperationsRequest where
  show = genericShow
instance decodeGetOperationsRequest :: Decode GetOperationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOperationsRequest :: Encode GetOperationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetOperationsResult = GetOperationsResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetOperationsResult :: Newtype GetOperationsResult _
derive instance repGenericGetOperationsResult :: Generic GetOperationsResult _
instance showGetOperationsResult :: Show GetOperationsResult where
  show = genericShow
instance decodeGetOperationsResult :: Decode GetOperationsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOperationsResult :: Encode GetOperationsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRegionsRequest = GetRegionsRequest 
  { "IncludeAvailabilityZones'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeGetRegionsRequest :: Newtype GetRegionsRequest _
derive instance repGenericGetRegionsRequest :: Generic GetRegionsRequest _
instance showGetRegionsRequest :: Show GetRegionsRequest where
  show = genericShow
instance decodeGetRegionsRequest :: Decode GetRegionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRegionsRequest :: Encode GetRegionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRegionsResult = GetRegionsResult 
  { "Regions'" :: NullOrUndefined.NullOrUndefined (RegionList)
  }
derive instance newtypeGetRegionsResult :: Newtype GetRegionsResult _
derive instance repGenericGetRegionsResult :: Generic GetRegionsResult _
instance showGetRegionsResult :: Show GetRegionsResult where
  show = genericShow
instance decodeGetRegionsResult :: Decode GetRegionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRegionsResult :: Encode GetRegionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetStaticIpRequest = GetStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  }
derive instance newtypeGetStaticIpRequest :: Newtype GetStaticIpRequest _
derive instance repGenericGetStaticIpRequest :: Generic GetStaticIpRequest _
instance showGetStaticIpRequest :: Show GetStaticIpRequest where
  show = genericShow
instance decodeGetStaticIpRequest :: Decode GetStaticIpRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStaticIpRequest :: Encode GetStaticIpRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetStaticIpResult = GetStaticIpResult 
  { "StaticIp'" :: NullOrUndefined.NullOrUndefined (StaticIp)
  }
derive instance newtypeGetStaticIpResult :: Newtype GetStaticIpResult _
derive instance repGenericGetStaticIpResult :: Generic GetStaticIpResult _
instance showGetStaticIpResult :: Show GetStaticIpResult where
  show = genericShow
instance decodeGetStaticIpResult :: Decode GetStaticIpResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStaticIpResult :: Encode GetStaticIpResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetStaticIpsRequest = GetStaticIpsRequest 
  { "PageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetStaticIpsRequest :: Newtype GetStaticIpsRequest _
derive instance repGenericGetStaticIpsRequest :: Generic GetStaticIpsRequest _
instance showGetStaticIpsRequest :: Show GetStaticIpsRequest where
  show = genericShow
instance decodeGetStaticIpsRequest :: Decode GetStaticIpsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStaticIpsRequest :: Encode GetStaticIpsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetStaticIpsResult = GetStaticIpsResult 
  { "StaticIps'" :: NullOrUndefined.NullOrUndefined (StaticIpList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetStaticIpsResult :: Newtype GetStaticIpsResult _
derive instance repGenericGetStaticIpsResult :: Generic GetStaticIpsResult _
instance showGetStaticIpsResult :: Show GetStaticIpsResult where
  show = genericShow
instance decodeGetStaticIpsResult :: Decode GetStaticIpsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStaticIpsResult :: Encode GetStaticIpsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportKeyPairRequest = ImportKeyPairRequest 
  { "KeyPairName'" :: (ResourceName)
  , "PublicKeyBase64'" :: (Base64)
  }
derive instance newtypeImportKeyPairRequest :: Newtype ImportKeyPairRequest _
derive instance repGenericImportKeyPairRequest :: Generic ImportKeyPairRequest _
instance showImportKeyPairRequest :: Show ImportKeyPairRequest where
  show = genericShow
instance decodeImportKeyPairRequest :: Decode ImportKeyPairRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportKeyPairRequest :: Encode ImportKeyPairRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportKeyPairResult = ImportKeyPairResult 
  { "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypeImportKeyPairResult :: Newtype ImportKeyPairResult _
derive instance repGenericImportKeyPairResult :: Generic ImportKeyPairResult _
instance showImportKeyPairResult :: Show ImportKeyPairResult where
  show = genericShow
instance decodeImportKeyPairResult :: Decode ImportKeyPairResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportKeyPairResult :: Encode ImportKeyPairResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an instance (a virtual private server).</p>
newtype Instance = Instance 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined.NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "BlueprintId'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "BlueprintName'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "BundleId'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "IsStaticIp'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "PrivateIpAddress'" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "PublicIpAddress'" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "Ipv6Address'" :: NullOrUndefined.NullOrUndefined (IpV6Address)
  , "Hardware'" :: NullOrUndefined.NullOrUndefined (InstanceHardware)
  , "Networking'" :: NullOrUndefined.NullOrUndefined (InstanceNetworking)
  , "State'" :: NullOrUndefined.NullOrUndefined (InstanceState)
  , "Username'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SshKeyName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  }
derive instance newtypeInstance :: Newtype Instance _
derive instance repGenericInstance :: Generic Instance _
instance showInstance :: Show Instance where
  show = genericShow
instance decodeInstance :: Decode Instance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstance :: Encode Instance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameters for gaining temporary access to one of your Amazon Lightsail instances.</p>
newtype InstanceAccessDetails = InstanceAccessDetails 
  { "CertKey'" :: NullOrUndefined.NullOrUndefined (String)
  , "ExpiresAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "IpAddress'" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "Password'" :: NullOrUndefined.NullOrUndefined (String)
  , "PasswordData'" :: NullOrUndefined.NullOrUndefined (PasswordData)
  , "PrivateKey'" :: NullOrUndefined.NullOrUndefined (String)
  , "Protocol'" :: NullOrUndefined.NullOrUndefined (InstanceAccessProtocol)
  , "InstanceName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Username'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInstanceAccessDetails :: Newtype InstanceAccessDetails _
derive instance repGenericInstanceAccessDetails :: Generic InstanceAccessDetails _
instance showInstanceAccessDetails :: Show InstanceAccessDetails where
  show = genericShow
instance decodeInstanceAccessDetails :: Decode InstanceAccessDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceAccessDetails :: Encode InstanceAccessDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceAccessProtocol = InstanceAccessProtocol String
derive instance newtypeInstanceAccessProtocol :: Newtype InstanceAccessProtocol _
derive instance repGenericInstanceAccessProtocol :: Generic InstanceAccessProtocol _
instance showInstanceAccessProtocol :: Show InstanceAccessProtocol where
  show = genericShow
instance decodeInstanceAccessProtocol :: Decode InstanceAccessProtocol where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceAccessProtocol :: Encode InstanceAccessProtocol where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the hardware for the instance.</p>
newtype InstanceHardware = InstanceHardware 
  { "CpuCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Disks'" :: NullOrUndefined.NullOrUndefined (DiskList)
  , "RamSizeInGb'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeInstanceHardware :: Newtype InstanceHardware _
derive instance repGenericInstanceHardware :: Generic InstanceHardware _
instance showInstanceHardware :: Show InstanceHardware where
  show = genericShow
instance decodeInstanceHardware :: Decode InstanceHardware where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceHardware :: Encode InstanceHardware where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceHealthReason = InstanceHealthReason String
derive instance newtypeInstanceHealthReason :: Newtype InstanceHealthReason _
derive instance repGenericInstanceHealthReason :: Generic InstanceHealthReason _
instance showInstanceHealthReason :: Show InstanceHealthReason where
  show = genericShow
instance decodeInstanceHealthReason :: Decode InstanceHealthReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceHealthReason :: Encode InstanceHealthReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceHealthState = InstanceHealthState String
derive instance newtypeInstanceHealthState :: Newtype InstanceHealthState _
derive instance repGenericInstanceHealthState :: Generic InstanceHealthState _
instance showInstanceHealthState :: Show InstanceHealthState where
  show = genericShow
instance decodeInstanceHealthState :: Decode InstanceHealthState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceHealthState :: Encode InstanceHealthState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes information about the health of the instance.</p>
newtype InstanceHealthSummary = InstanceHealthSummary 
  { "InstanceName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "InstanceHealth'" :: NullOrUndefined.NullOrUndefined (InstanceHealthState)
  , "InstanceHealthReason'" :: NullOrUndefined.NullOrUndefined (InstanceHealthReason)
  }
derive instance newtypeInstanceHealthSummary :: Newtype InstanceHealthSummary _
derive instance repGenericInstanceHealthSummary :: Generic InstanceHealthSummary _
instance showInstanceHealthSummary :: Show InstanceHealthSummary where
  show = genericShow
instance decodeInstanceHealthSummary :: Decode InstanceHealthSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceHealthSummary :: Encode InstanceHealthSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceHealthSummaryList = InstanceHealthSummaryList (Array InstanceHealthSummary)
derive instance newtypeInstanceHealthSummaryList :: Newtype InstanceHealthSummaryList _
derive instance repGenericInstanceHealthSummaryList :: Generic InstanceHealthSummaryList _
instance showInstanceHealthSummaryList :: Show InstanceHealthSummaryList where
  show = genericShow
instance decodeInstanceHealthSummaryList :: Decode InstanceHealthSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceHealthSummaryList :: Encode InstanceHealthSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceList = InstanceList (Array Instance)
derive instance newtypeInstanceList :: Newtype InstanceList _
derive instance repGenericInstanceList :: Generic InstanceList _
instance showInstanceList :: Show InstanceList where
  show = genericShow
instance decodeInstanceList :: Decode InstanceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceList :: Encode InstanceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceMetricName = InstanceMetricName String
derive instance newtypeInstanceMetricName :: Newtype InstanceMetricName _
derive instance repGenericInstanceMetricName :: Generic InstanceMetricName _
instance showInstanceMetricName :: Show InstanceMetricName where
  show = genericShow
instance decodeInstanceMetricName :: Decode InstanceMetricName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceMetricName :: Encode InstanceMetricName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes monthly data transfer rates and port information for an instance.</p>
newtype InstanceNetworking = InstanceNetworking 
  { "MonthlyTransfer'" :: NullOrUndefined.NullOrUndefined (MonthlyTransfer)
  , "Ports'" :: NullOrUndefined.NullOrUndefined (InstancePortInfoList)
  }
derive instance newtypeInstanceNetworking :: Newtype InstanceNetworking _
derive instance repGenericInstanceNetworking :: Generic InstanceNetworking _
instance showInstanceNetworking :: Show InstanceNetworking where
  show = genericShow
instance decodeInstanceNetworking :: Decode InstanceNetworking where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceNetworking :: Encode InstanceNetworking where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancePlatform = InstancePlatform String
derive instance newtypeInstancePlatform :: Newtype InstancePlatform _
derive instance repGenericInstancePlatform :: Generic InstancePlatform _
instance showInstancePlatform :: Show InstancePlatform where
  show = genericShow
instance decodeInstancePlatform :: Decode InstancePlatform where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePlatform :: Encode InstancePlatform where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancePlatformList = InstancePlatformList (Array InstancePlatform)
derive instance newtypeInstancePlatformList :: Newtype InstancePlatformList _
derive instance repGenericInstancePlatformList :: Generic InstancePlatformList _
instance showInstancePlatformList :: Show InstancePlatformList where
  show = genericShow
instance decodeInstancePlatformList :: Decode InstancePlatformList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePlatformList :: Encode InstancePlatformList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes information about the instance ports.</p>
newtype InstancePortInfo = InstancePortInfo 
  { "FromPort'" :: NullOrUndefined.NullOrUndefined (Port)
  , "ToPort'" :: NullOrUndefined.NullOrUndefined (Port)
  , "Protocol'" :: NullOrUndefined.NullOrUndefined (NetworkProtocol)
  , "AccessFrom'" :: NullOrUndefined.NullOrUndefined (String)
  , "AccessType'" :: NullOrUndefined.NullOrUndefined (PortAccessType)
  , "CommonName'" :: NullOrUndefined.NullOrUndefined (String)
  , "AccessDirection'" :: NullOrUndefined.NullOrUndefined (AccessDirection)
  }
derive instance newtypeInstancePortInfo :: Newtype InstancePortInfo _
derive instance repGenericInstancePortInfo :: Generic InstancePortInfo _
instance showInstancePortInfo :: Show InstancePortInfo where
  show = genericShow
instance decodeInstancePortInfo :: Decode InstancePortInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePortInfo :: Encode InstancePortInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancePortInfoList = InstancePortInfoList (Array InstancePortInfo)
derive instance newtypeInstancePortInfoList :: Newtype InstancePortInfoList _
derive instance repGenericInstancePortInfoList :: Generic InstancePortInfoList _
instance showInstancePortInfoList :: Show InstancePortInfoList where
  show = genericShow
instance decodeInstancePortInfoList :: Decode InstancePortInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePortInfoList :: Encode InstancePortInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the port state.</p>
newtype InstancePortState = InstancePortState 
  { "FromPort'" :: NullOrUndefined.NullOrUndefined (Port)
  , "ToPort'" :: NullOrUndefined.NullOrUndefined (Port)
  , "Protocol'" :: NullOrUndefined.NullOrUndefined (NetworkProtocol)
  , "State'" :: NullOrUndefined.NullOrUndefined (PortState)
  }
derive instance newtypeInstancePortState :: Newtype InstancePortState _
derive instance repGenericInstancePortState :: Generic InstancePortState _
instance showInstancePortState :: Show InstancePortState where
  show = genericShow
instance decodeInstancePortState :: Decode InstancePortState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePortState :: Encode InstancePortState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancePortStateList = InstancePortStateList (Array InstancePortState)
derive instance newtypeInstancePortStateList :: Newtype InstancePortStateList _
derive instance repGenericInstancePortStateList :: Generic InstancePortStateList _
instance showInstancePortStateList :: Show InstancePortStateList where
  show = genericShow
instance decodeInstancePortStateList :: Decode InstancePortStateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePortStateList :: Encode InstancePortStateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the snapshot of the virtual private server, or <i>instance</i>.</p>
newtype InstanceSnapshot = InstanceSnapshot 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined.NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "State'" :: NullOrUndefined.NullOrUndefined (InstanceSnapshotState)
  , "Progress'" :: NullOrUndefined.NullOrUndefined (String)
  , "FromAttachedDisks'" :: NullOrUndefined.NullOrUndefined (DiskList)
  , "FromInstanceName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "FromInstanceArn'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "FromBlueprintId'" :: NullOrUndefined.NullOrUndefined (String)
  , "FromBundleId'" :: NullOrUndefined.NullOrUndefined (String)
  , "SizeInGb'" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeInstanceSnapshot :: Newtype InstanceSnapshot _
derive instance repGenericInstanceSnapshot :: Generic InstanceSnapshot _
instance showInstanceSnapshot :: Show InstanceSnapshot where
  show = genericShow
instance decodeInstanceSnapshot :: Decode InstanceSnapshot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceSnapshot :: Encode InstanceSnapshot where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceSnapshotList = InstanceSnapshotList (Array InstanceSnapshot)
derive instance newtypeInstanceSnapshotList :: Newtype InstanceSnapshotList _
derive instance repGenericInstanceSnapshotList :: Generic InstanceSnapshotList _
instance showInstanceSnapshotList :: Show InstanceSnapshotList where
  show = genericShow
instance decodeInstanceSnapshotList :: Decode InstanceSnapshotList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceSnapshotList :: Encode InstanceSnapshotList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceSnapshotState = InstanceSnapshotState String
derive instance newtypeInstanceSnapshotState :: Newtype InstanceSnapshotState _
derive instance repGenericInstanceSnapshotState :: Generic InstanceSnapshotState _
instance showInstanceSnapshotState :: Show InstanceSnapshotState where
  show = genericShow
instance decodeInstanceSnapshotState :: Decode InstanceSnapshotState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceSnapshotState :: Encode InstanceSnapshotState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the virtual private server (or <i>instance</i>) status.</p>
newtype InstanceState = InstanceState 
  { "Code'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInstanceState :: Newtype InstanceState _
derive instance repGenericInstanceState :: Generic InstanceState _
instance showInstanceState :: Show InstanceState where
  show = genericShow
instance decodeInstanceState :: Decode InstanceState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceState :: Encode InstanceState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lightsail throws this exception when user input does not conform to the validation rules of an input field.</p> <note> <p>Domain-related APIs are only available in the N. Virginia (us-east-1) Region. Please set your Region configuration to us-east-1 to create, view, or edit these resources.</p> </note>
newtype InvalidInputException = InvalidInputException 
  { "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidInputException :: Newtype InvalidInputException _
derive instance repGenericInvalidInputException :: Generic InvalidInputException _
instance showInvalidInputException :: Show InvalidInputException where
  show = genericShow
instance decodeInvalidInputException :: Decode InvalidInputException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidInputException :: Encode InvalidInputException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _
derive instance repGenericIpAddress :: Generic IpAddress _
instance showIpAddress :: Show IpAddress where
  show = genericShow
instance decodeIpAddress :: Decode IpAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpAddress :: Encode IpAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpV6Address = IpV6Address String
derive instance newtypeIpV6Address :: Newtype IpV6Address _
derive instance repGenericIpV6Address :: Generic IpV6Address _
instance showIpV6Address :: Show IpV6Address where
  show = genericShow
instance decodeIpV6Address :: Decode IpV6Address where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpV6Address :: Encode IpV6Address where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsVpcPeeredRequest = IsVpcPeeredRequest Types.NoArguments
derive instance newtypeIsVpcPeeredRequest :: Newtype IsVpcPeeredRequest _
derive instance repGenericIsVpcPeeredRequest :: Generic IsVpcPeeredRequest _
instance showIsVpcPeeredRequest :: Show IsVpcPeeredRequest where
  show = genericShow
instance decodeIsVpcPeeredRequest :: Decode IsVpcPeeredRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsVpcPeeredRequest :: Encode IsVpcPeeredRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsVpcPeeredResult = IsVpcPeeredResult 
  { "IsPeered'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeIsVpcPeeredResult :: Newtype IsVpcPeeredResult _
derive instance repGenericIsVpcPeeredResult :: Generic IsVpcPeeredResult _
instance showIsVpcPeeredResult :: Show IsVpcPeeredResult where
  show = genericShow
instance decodeIsVpcPeeredResult :: Decode IsVpcPeeredResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsVpcPeeredResult :: Encode IsVpcPeeredResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsoDate = IsoDate Number
derive instance newtypeIsoDate :: Newtype IsoDate _
derive instance repGenericIsoDate :: Generic IsoDate _
instance showIsoDate :: Show IsoDate where
  show = genericShow
instance decodeIsoDate :: Decode IsoDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsoDate :: Encode IsoDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the SSH key pair.</p>
newtype KeyPair = KeyPair 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined.NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "Fingerprint'" :: NullOrUndefined.NullOrUndefined (Base64)
  }
derive instance newtypeKeyPair :: Newtype KeyPair _
derive instance repGenericKeyPair :: Generic KeyPair _
instance showKeyPair :: Show KeyPair where
  show = genericShow
instance decodeKeyPair :: Decode KeyPair where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyPair :: Encode KeyPair where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyPairList = KeyPairList (Array KeyPair)
derive instance newtypeKeyPairList :: Newtype KeyPairList _
derive instance repGenericKeyPairList :: Generic KeyPairList _
instance showKeyPairList :: Show KeyPairList where
  show = genericShow
instance decodeKeyPairList :: Decode KeyPairList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyPairList :: Encode KeyPairList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the Lightsail load balancer.</p>
newtype LoadBalancer = LoadBalancer 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined.NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "DnsName'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "State'" :: NullOrUndefined.NullOrUndefined (LoadBalancerState)
  , "Protocol'" :: NullOrUndefined.NullOrUndefined (LoadBalancerProtocol)
  , "PublicPorts'" :: NullOrUndefined.NullOrUndefined (PortList)
  , "HealthCheckPath'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "InstancePort'" :: NullOrUndefined.NullOrUndefined (Int)
  , "InstanceHealthSummary'" :: NullOrUndefined.NullOrUndefined (InstanceHealthSummaryList)
  , "TlsCertificateSummaries'" :: NullOrUndefined.NullOrUndefined (LoadBalancerTlsCertificateSummaryList)
  , "ConfigurationOptions'" :: NullOrUndefined.NullOrUndefined (LoadBalancerConfigurationOptions)
  }
derive instance newtypeLoadBalancer :: Newtype LoadBalancer _
derive instance repGenericLoadBalancer :: Generic LoadBalancer _
instance showLoadBalancer :: Show LoadBalancer where
  show = genericShow
instance decodeLoadBalancer :: Decode LoadBalancer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancer :: Encode LoadBalancer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerAttributeName = LoadBalancerAttributeName String
derive instance newtypeLoadBalancerAttributeName :: Newtype LoadBalancerAttributeName _
derive instance repGenericLoadBalancerAttributeName :: Generic LoadBalancerAttributeName _
instance showLoadBalancerAttributeName :: Show LoadBalancerAttributeName where
  show = genericShow
instance decodeLoadBalancerAttributeName :: Decode LoadBalancerAttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerAttributeName :: Encode LoadBalancerAttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerConfigurationOptions = LoadBalancerConfigurationOptions (StrMap.StrMap String)
derive instance newtypeLoadBalancerConfigurationOptions :: Newtype LoadBalancerConfigurationOptions _
derive instance repGenericLoadBalancerConfigurationOptions :: Generic LoadBalancerConfigurationOptions _
instance showLoadBalancerConfigurationOptions :: Show LoadBalancerConfigurationOptions where
  show = genericShow
instance decodeLoadBalancerConfigurationOptions :: Decode LoadBalancerConfigurationOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerConfigurationOptions :: Encode LoadBalancerConfigurationOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerList = LoadBalancerList (Array LoadBalancer)
derive instance newtypeLoadBalancerList :: Newtype LoadBalancerList _
derive instance repGenericLoadBalancerList :: Generic LoadBalancerList _
instance showLoadBalancerList :: Show LoadBalancerList where
  show = genericShow
instance decodeLoadBalancerList :: Decode LoadBalancerList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerList :: Encode LoadBalancerList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerMetricName = LoadBalancerMetricName String
derive instance newtypeLoadBalancerMetricName :: Newtype LoadBalancerMetricName _
derive instance repGenericLoadBalancerMetricName :: Generic LoadBalancerMetricName _
instance showLoadBalancerMetricName :: Show LoadBalancerMetricName where
  show = genericShow
instance decodeLoadBalancerMetricName :: Decode LoadBalancerMetricName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerMetricName :: Encode LoadBalancerMetricName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerProtocol = LoadBalancerProtocol String
derive instance newtypeLoadBalancerProtocol :: Newtype LoadBalancerProtocol _
derive instance repGenericLoadBalancerProtocol :: Generic LoadBalancerProtocol _
instance showLoadBalancerProtocol :: Show LoadBalancerProtocol where
  show = genericShow
instance decodeLoadBalancerProtocol :: Decode LoadBalancerProtocol where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerProtocol :: Encode LoadBalancerProtocol where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerState = LoadBalancerState String
derive instance newtypeLoadBalancerState :: Newtype LoadBalancerState _
derive instance repGenericLoadBalancerState :: Generic LoadBalancerState _
instance showLoadBalancerState :: Show LoadBalancerState where
  show = genericShow
instance decodeLoadBalancerState :: Decode LoadBalancerState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerState :: Encode LoadBalancerState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a load balancer TLS/SSL certificate.</p> <p>TLS is just an updated, more secure version of Secure Socket Layer (SSL).</p>
newtype LoadBalancerTlsCertificate = LoadBalancerTlsCertificate 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined.NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "LoadBalancerName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "IsAttached'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Status'" :: NullOrUndefined.NullOrUndefined (LoadBalancerTlsCertificateStatus)
  , "DomainName'" :: NullOrUndefined.NullOrUndefined (DomainName)
  , "DomainValidationRecords'" :: NullOrUndefined.NullOrUndefined (LoadBalancerTlsCertificateDomainValidationRecordList)
  , "FailureReason'" :: NullOrUndefined.NullOrUndefined (LoadBalancerTlsCertificateFailureReason)
  , "IssuedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "Issuer'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "KeyAlgorithm'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "NotAfter'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "NotBefore'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "RenewalSummary'" :: NullOrUndefined.NullOrUndefined (LoadBalancerTlsCertificateRenewalSummary)
  , "RevocationReason'" :: NullOrUndefined.NullOrUndefined (LoadBalancerTlsCertificateRevocationReason)
  , "RevokedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "Serial'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SignatureAlgorithm'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Subject'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SubjectAlternativeNames'" :: NullOrUndefined.NullOrUndefined (StringList)
  }
derive instance newtypeLoadBalancerTlsCertificate :: Newtype LoadBalancerTlsCertificate _
derive instance repGenericLoadBalancerTlsCertificate :: Generic LoadBalancerTlsCertificate _
instance showLoadBalancerTlsCertificate :: Show LoadBalancerTlsCertificate where
  show = genericShow
instance decodeLoadBalancerTlsCertificate :: Decode LoadBalancerTlsCertificate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificate :: Encode LoadBalancerTlsCertificate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerTlsCertificateDomainStatus = LoadBalancerTlsCertificateDomainStatus String
derive instance newtypeLoadBalancerTlsCertificateDomainStatus :: Newtype LoadBalancerTlsCertificateDomainStatus _
derive instance repGenericLoadBalancerTlsCertificateDomainStatus :: Generic LoadBalancerTlsCertificateDomainStatus _
instance showLoadBalancerTlsCertificateDomainStatus :: Show LoadBalancerTlsCertificateDomainStatus where
  show = genericShow
instance decodeLoadBalancerTlsCertificateDomainStatus :: Decode LoadBalancerTlsCertificateDomainStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateDomainStatus :: Encode LoadBalancerTlsCertificateDomainStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the domain names on a TLS/SSL certificate that you will use to validate domain ownership.</p>
newtype LoadBalancerTlsCertificateDomainValidationOption = LoadBalancerTlsCertificateDomainValidationOption 
  { "DomainName'" :: NullOrUndefined.NullOrUndefined (DomainName)
  , "ValidationStatus'" :: NullOrUndefined.NullOrUndefined (LoadBalancerTlsCertificateDomainStatus)
  }
derive instance newtypeLoadBalancerTlsCertificateDomainValidationOption :: Newtype LoadBalancerTlsCertificateDomainValidationOption _
derive instance repGenericLoadBalancerTlsCertificateDomainValidationOption :: Generic LoadBalancerTlsCertificateDomainValidationOption _
instance showLoadBalancerTlsCertificateDomainValidationOption :: Show LoadBalancerTlsCertificateDomainValidationOption where
  show = genericShow
instance decodeLoadBalancerTlsCertificateDomainValidationOption :: Decode LoadBalancerTlsCertificateDomainValidationOption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateDomainValidationOption :: Encode LoadBalancerTlsCertificateDomainValidationOption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerTlsCertificateDomainValidationOptionList = LoadBalancerTlsCertificateDomainValidationOptionList (Array LoadBalancerTlsCertificateDomainValidationOption)
derive instance newtypeLoadBalancerTlsCertificateDomainValidationOptionList :: Newtype LoadBalancerTlsCertificateDomainValidationOptionList _
derive instance repGenericLoadBalancerTlsCertificateDomainValidationOptionList :: Generic LoadBalancerTlsCertificateDomainValidationOptionList _
instance showLoadBalancerTlsCertificateDomainValidationOptionList :: Show LoadBalancerTlsCertificateDomainValidationOptionList where
  show = genericShow
instance decodeLoadBalancerTlsCertificateDomainValidationOptionList :: Decode LoadBalancerTlsCertificateDomainValidationOptionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateDomainValidationOptionList :: Encode LoadBalancerTlsCertificateDomainValidationOptionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the validation record of each domain name in the TLS/SSL certificate.</p>
newtype LoadBalancerTlsCertificateDomainValidationRecord = LoadBalancerTlsCertificateDomainValidationRecord 
  { "Name'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Type'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Value'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "ValidationStatus'" :: NullOrUndefined.NullOrUndefined (LoadBalancerTlsCertificateDomainStatus)
  , "DomainName'" :: NullOrUndefined.NullOrUndefined (DomainName)
  }
derive instance newtypeLoadBalancerTlsCertificateDomainValidationRecord :: Newtype LoadBalancerTlsCertificateDomainValidationRecord _
derive instance repGenericLoadBalancerTlsCertificateDomainValidationRecord :: Generic LoadBalancerTlsCertificateDomainValidationRecord _
instance showLoadBalancerTlsCertificateDomainValidationRecord :: Show LoadBalancerTlsCertificateDomainValidationRecord where
  show = genericShow
instance decodeLoadBalancerTlsCertificateDomainValidationRecord :: Decode LoadBalancerTlsCertificateDomainValidationRecord where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateDomainValidationRecord :: Encode LoadBalancerTlsCertificateDomainValidationRecord where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerTlsCertificateDomainValidationRecordList = LoadBalancerTlsCertificateDomainValidationRecordList (Array LoadBalancerTlsCertificateDomainValidationRecord)
derive instance newtypeLoadBalancerTlsCertificateDomainValidationRecordList :: Newtype LoadBalancerTlsCertificateDomainValidationRecordList _
derive instance repGenericLoadBalancerTlsCertificateDomainValidationRecordList :: Generic LoadBalancerTlsCertificateDomainValidationRecordList _
instance showLoadBalancerTlsCertificateDomainValidationRecordList :: Show LoadBalancerTlsCertificateDomainValidationRecordList where
  show = genericShow
instance decodeLoadBalancerTlsCertificateDomainValidationRecordList :: Decode LoadBalancerTlsCertificateDomainValidationRecordList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateDomainValidationRecordList :: Encode LoadBalancerTlsCertificateDomainValidationRecordList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerTlsCertificateFailureReason = LoadBalancerTlsCertificateFailureReason String
derive instance newtypeLoadBalancerTlsCertificateFailureReason :: Newtype LoadBalancerTlsCertificateFailureReason _
derive instance repGenericLoadBalancerTlsCertificateFailureReason :: Generic LoadBalancerTlsCertificateFailureReason _
instance showLoadBalancerTlsCertificateFailureReason :: Show LoadBalancerTlsCertificateFailureReason where
  show = genericShow
instance decodeLoadBalancerTlsCertificateFailureReason :: Decode LoadBalancerTlsCertificateFailureReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateFailureReason :: Encode LoadBalancerTlsCertificateFailureReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerTlsCertificateList = LoadBalancerTlsCertificateList (Array LoadBalancerTlsCertificate)
derive instance newtypeLoadBalancerTlsCertificateList :: Newtype LoadBalancerTlsCertificateList _
derive instance repGenericLoadBalancerTlsCertificateList :: Generic LoadBalancerTlsCertificateList _
instance showLoadBalancerTlsCertificateList :: Show LoadBalancerTlsCertificateList where
  show = genericShow
instance decodeLoadBalancerTlsCertificateList :: Decode LoadBalancerTlsCertificateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateList :: Encode LoadBalancerTlsCertificateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerTlsCertificateRenewalStatus = LoadBalancerTlsCertificateRenewalStatus String
derive instance newtypeLoadBalancerTlsCertificateRenewalStatus :: Newtype LoadBalancerTlsCertificateRenewalStatus _
derive instance repGenericLoadBalancerTlsCertificateRenewalStatus :: Generic LoadBalancerTlsCertificateRenewalStatus _
instance showLoadBalancerTlsCertificateRenewalStatus :: Show LoadBalancerTlsCertificateRenewalStatus where
  show = genericShow
instance decodeLoadBalancerTlsCertificateRenewalStatus :: Decode LoadBalancerTlsCertificateRenewalStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateRenewalStatus :: Encode LoadBalancerTlsCertificateRenewalStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the status of Lightsail's managed renewal for the certificate.</p>
newtype LoadBalancerTlsCertificateRenewalSummary = LoadBalancerTlsCertificateRenewalSummary 
  { "RenewalStatus'" :: NullOrUndefined.NullOrUndefined (LoadBalancerTlsCertificateRenewalStatus)
  , "DomainValidationOptions'" :: NullOrUndefined.NullOrUndefined (LoadBalancerTlsCertificateDomainValidationOptionList)
  }
derive instance newtypeLoadBalancerTlsCertificateRenewalSummary :: Newtype LoadBalancerTlsCertificateRenewalSummary _
derive instance repGenericLoadBalancerTlsCertificateRenewalSummary :: Generic LoadBalancerTlsCertificateRenewalSummary _
instance showLoadBalancerTlsCertificateRenewalSummary :: Show LoadBalancerTlsCertificateRenewalSummary where
  show = genericShow
instance decodeLoadBalancerTlsCertificateRenewalSummary :: Decode LoadBalancerTlsCertificateRenewalSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateRenewalSummary :: Encode LoadBalancerTlsCertificateRenewalSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerTlsCertificateRevocationReason = LoadBalancerTlsCertificateRevocationReason String
derive instance newtypeLoadBalancerTlsCertificateRevocationReason :: Newtype LoadBalancerTlsCertificateRevocationReason _
derive instance repGenericLoadBalancerTlsCertificateRevocationReason :: Generic LoadBalancerTlsCertificateRevocationReason _
instance showLoadBalancerTlsCertificateRevocationReason :: Show LoadBalancerTlsCertificateRevocationReason where
  show = genericShow
instance decodeLoadBalancerTlsCertificateRevocationReason :: Decode LoadBalancerTlsCertificateRevocationReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateRevocationReason :: Encode LoadBalancerTlsCertificateRevocationReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerTlsCertificateStatus = LoadBalancerTlsCertificateStatus String
derive instance newtypeLoadBalancerTlsCertificateStatus :: Newtype LoadBalancerTlsCertificateStatus _
derive instance repGenericLoadBalancerTlsCertificateStatus :: Generic LoadBalancerTlsCertificateStatus _
instance showLoadBalancerTlsCertificateStatus :: Show LoadBalancerTlsCertificateStatus where
  show = genericShow
instance decodeLoadBalancerTlsCertificateStatus :: Decode LoadBalancerTlsCertificateStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateStatus :: Encode LoadBalancerTlsCertificateStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides a summary of TLS/SSL certificate metadata.</p>
newtype LoadBalancerTlsCertificateSummary = LoadBalancerTlsCertificateSummary 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "IsAttached'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeLoadBalancerTlsCertificateSummary :: Newtype LoadBalancerTlsCertificateSummary _
derive instance repGenericLoadBalancerTlsCertificateSummary :: Generic LoadBalancerTlsCertificateSummary _
instance showLoadBalancerTlsCertificateSummary :: Show LoadBalancerTlsCertificateSummary where
  show = genericShow
instance decodeLoadBalancerTlsCertificateSummary :: Decode LoadBalancerTlsCertificateSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateSummary :: Encode LoadBalancerTlsCertificateSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerTlsCertificateSummaryList = LoadBalancerTlsCertificateSummaryList (Array LoadBalancerTlsCertificateSummary)
derive instance newtypeLoadBalancerTlsCertificateSummaryList :: Newtype LoadBalancerTlsCertificateSummaryList _
derive instance repGenericLoadBalancerTlsCertificateSummaryList :: Generic LoadBalancerTlsCertificateSummaryList _
instance showLoadBalancerTlsCertificateSummaryList :: Show LoadBalancerTlsCertificateSummaryList where
  show = genericShow
instance decodeLoadBalancerTlsCertificateSummaryList :: Decode LoadBalancerTlsCertificateSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTlsCertificateSummaryList :: Encode LoadBalancerTlsCertificateSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the metric data point.</p>
newtype MetricDatapoint = MetricDatapoint 
  { "Average'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Maximum'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Minimum'" :: NullOrUndefined.NullOrUndefined (Number)
  , "SampleCount'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Sum'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "Unit''" :: NullOrUndefined.NullOrUndefined (MetricUnit)
  }
derive instance newtypeMetricDatapoint :: Newtype MetricDatapoint _
derive instance repGenericMetricDatapoint :: Generic MetricDatapoint _
instance showMetricDatapoint :: Show MetricDatapoint where
  show = genericShow
instance decodeMetricDatapoint :: Decode MetricDatapoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricDatapoint :: Encode MetricDatapoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricDatapointList = MetricDatapointList (Array MetricDatapoint)
derive instance newtypeMetricDatapointList :: Newtype MetricDatapointList _
derive instance repGenericMetricDatapointList :: Generic MetricDatapointList _
instance showMetricDatapointList :: Show MetricDatapointList where
  show = genericShow
instance decodeMetricDatapointList :: Decode MetricDatapointList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricDatapointList :: Encode MetricDatapointList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricPeriod = MetricPeriod Int
derive instance newtypeMetricPeriod :: Newtype MetricPeriod _
derive instance repGenericMetricPeriod :: Generic MetricPeriod _
instance showMetricPeriod :: Show MetricPeriod where
  show = genericShow
instance decodeMetricPeriod :: Decode MetricPeriod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricPeriod :: Encode MetricPeriod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricStatistic = MetricStatistic String
derive instance newtypeMetricStatistic :: Newtype MetricStatistic _
derive instance repGenericMetricStatistic :: Generic MetricStatistic _
instance showMetricStatistic :: Show MetricStatistic where
  show = genericShow
instance decodeMetricStatistic :: Decode MetricStatistic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricStatistic :: Encode MetricStatistic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricStatisticList = MetricStatisticList (Array MetricStatistic)
derive instance newtypeMetricStatisticList :: Newtype MetricStatisticList _
derive instance repGenericMetricStatisticList :: Generic MetricStatisticList _
instance showMetricStatisticList :: Show MetricStatisticList where
  show = genericShow
instance decodeMetricStatisticList :: Decode MetricStatisticList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricStatisticList :: Encode MetricStatisticList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricUnit = MetricUnit String
derive instance newtypeMetricUnit :: Newtype MetricUnit _
derive instance repGenericMetricUnit :: Generic MetricUnit _
instance showMetricUnit :: Show MetricUnit where
  show = genericShow
instance decodeMetricUnit :: Decode MetricUnit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricUnit :: Encode MetricUnit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the monthly data transfer in and out of your virtual private server (or <i>instance</i>).</p>
newtype MonthlyTransfer = MonthlyTransfer 
  { "GbPerMonthAllocated'" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeMonthlyTransfer :: Newtype MonthlyTransfer _
derive instance repGenericMonthlyTransfer :: Generic MonthlyTransfer _
instance showMonthlyTransfer :: Show MonthlyTransfer where
  show = genericShow
instance decodeMonthlyTransfer :: Decode MonthlyTransfer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMonthlyTransfer :: Encode MonthlyTransfer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NetworkProtocol = NetworkProtocol String
derive instance newtypeNetworkProtocol :: Newtype NetworkProtocol _
derive instance repGenericNetworkProtocol :: Generic NetworkProtocol _
instance showNetworkProtocol :: Show NetworkProtocol where
  show = genericShow
instance decodeNetworkProtocol :: Decode NetworkProtocol where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkProtocol :: Encode NetworkProtocol where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString :: Newtype NonEmptyString _
derive instance repGenericNonEmptyString :: Generic NonEmptyString _
instance showNonEmptyString :: Show NonEmptyString where
  show = genericShow
instance decodeNonEmptyString :: Decode NonEmptyString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonEmptyString :: Encode NonEmptyString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lightsail throws this exception when it cannot find a resource.</p>
newtype NotFoundException = NotFoundException 
  { "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OpenInstancePublicPortsRequest = OpenInstancePublicPortsRequest 
  { "PortInfo'" :: (PortInfo)
  , "InstanceName'" :: (ResourceName)
  }
derive instance newtypeOpenInstancePublicPortsRequest :: Newtype OpenInstancePublicPortsRequest _
derive instance repGenericOpenInstancePublicPortsRequest :: Generic OpenInstancePublicPortsRequest _
instance showOpenInstancePublicPortsRequest :: Show OpenInstancePublicPortsRequest where
  show = genericShow
instance decodeOpenInstancePublicPortsRequest :: Decode OpenInstancePublicPortsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOpenInstancePublicPortsRequest :: Encode OpenInstancePublicPortsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OpenInstancePublicPortsResult = OpenInstancePublicPortsResult 
  { "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypeOpenInstancePublicPortsResult :: Newtype OpenInstancePublicPortsResult _
derive instance repGenericOpenInstancePublicPortsResult :: Generic OpenInstancePublicPortsResult _
instance showOpenInstancePublicPortsResult :: Show OpenInstancePublicPortsResult where
  show = genericShow
instance decodeOpenInstancePublicPortsResult :: Decode OpenInstancePublicPortsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOpenInstancePublicPortsResult :: Encode OpenInstancePublicPortsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the API operation.</p>
newtype Operation = Operation 
  { "Id'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "ResourceName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined.NullOrUndefined (ResourceLocation)
  , "IsTerminal'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "OperationDetails'" :: NullOrUndefined.NullOrUndefined (String)
  , "OperationType'" :: NullOrUndefined.NullOrUndefined (OperationType)
  , "Status'" :: NullOrUndefined.NullOrUndefined (OperationStatus)
  , "StatusChangedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "ErrorCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "ErrorDetails'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOperation :: Newtype Operation _
derive instance repGenericOperation :: Generic Operation _
instance showOperation :: Show Operation where
  show = genericShow
instance decodeOperation :: Decode Operation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperation :: Encode Operation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lightsail throws this exception when an operation fails to execute.</p>
newtype OperationFailureException = OperationFailureException 
  { "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOperationFailureException :: Newtype OperationFailureException _
derive instance repGenericOperationFailureException :: Generic OperationFailureException _
instance showOperationFailureException :: Show OperationFailureException where
  show = genericShow
instance decodeOperationFailureException :: Decode OperationFailureException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationFailureException :: Encode OperationFailureException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OperationList = OperationList (Array Operation)
derive instance newtypeOperationList :: Newtype OperationList _
derive instance repGenericOperationList :: Generic OperationList _
instance showOperationList :: Show OperationList where
  show = genericShow
instance decodeOperationList :: Decode OperationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationList :: Encode OperationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OperationStatus = OperationStatus String
derive instance newtypeOperationStatus :: Newtype OperationStatus _
derive instance repGenericOperationStatus :: Generic OperationStatus _
instance showOperationStatus :: Show OperationStatus where
  show = genericShow
instance decodeOperationStatus :: Decode OperationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationStatus :: Encode OperationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OperationType = OperationType String
derive instance newtypeOperationType :: Newtype OperationType _
derive instance repGenericOperationType :: Generic OperationType _
instance showOperationType :: Show OperationType where
  show = genericShow
instance decodeOperationType :: Decode OperationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationType :: Encode OperationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The password data for the Windows Server-based instance, including the ciphertext and the key pair name.</p>
newtype PasswordData = PasswordData 
  { "Ciphertext'" :: NullOrUndefined.NullOrUndefined (String)
  , "KeyPairName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  }
derive instance newtypePasswordData :: Newtype PasswordData _
derive instance repGenericPasswordData :: Generic PasswordData _
instance showPasswordData :: Show PasswordData where
  show = genericShow
instance decodePasswordData :: Decode PasswordData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePasswordData :: Encode PasswordData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PeerVpcRequest = PeerVpcRequest Types.NoArguments
derive instance newtypePeerVpcRequest :: Newtype PeerVpcRequest _
derive instance repGenericPeerVpcRequest :: Generic PeerVpcRequest _
instance showPeerVpcRequest :: Show PeerVpcRequest where
  show = genericShow
instance decodePeerVpcRequest :: Decode PeerVpcRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePeerVpcRequest :: Encode PeerVpcRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PeerVpcResult = PeerVpcResult 
  { "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypePeerVpcResult :: Newtype PeerVpcResult _
derive instance repGenericPeerVpcResult :: Generic PeerVpcResult _
instance showPeerVpcResult :: Show PeerVpcResult where
  show = genericShow
instance decodePeerVpcResult :: Decode PeerVpcResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePeerVpcResult :: Encode PeerVpcResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Port = Port Int
derive instance newtypePort :: Newtype Port _
derive instance repGenericPort :: Generic Port _
instance showPort :: Show Port where
  show = genericShow
instance decodePort :: Decode Port where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePort :: Encode Port where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PortAccessType = PortAccessType String
derive instance newtypePortAccessType :: Newtype PortAccessType _
derive instance repGenericPortAccessType :: Generic PortAccessType _
instance showPortAccessType :: Show PortAccessType where
  show = genericShow
instance decodePortAccessType :: Decode PortAccessType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePortAccessType :: Encode PortAccessType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes information about the ports on your virtual private server (or <i>instance</i>).</p>
newtype PortInfo = PortInfo 
  { "FromPort'" :: NullOrUndefined.NullOrUndefined (Port)
  , "ToPort'" :: NullOrUndefined.NullOrUndefined (Port)
  , "Protocol'" :: NullOrUndefined.NullOrUndefined (NetworkProtocol)
  }
derive instance newtypePortInfo :: Newtype PortInfo _
derive instance repGenericPortInfo :: Generic PortInfo _
instance showPortInfo :: Show PortInfo where
  show = genericShow
instance decodePortInfo :: Decode PortInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePortInfo :: Encode PortInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PortInfoList = PortInfoList (Array PortInfo)
derive instance newtypePortInfoList :: Newtype PortInfoList _
derive instance repGenericPortInfoList :: Generic PortInfoList _
instance showPortInfoList :: Show PortInfoList where
  show = genericShow
instance decodePortInfoList :: Decode PortInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePortInfoList :: Encode PortInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PortList = PortList (Array Port)
derive instance newtypePortList :: Newtype PortList _
derive instance repGenericPortList :: Generic PortList _
instance showPortList :: Show PortList where
  show = genericShow
instance decodePortList :: Decode PortList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePortList :: Encode PortList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PortState = PortState String
derive instance newtypePortState :: Newtype PortState _
derive instance repGenericPortState :: Generic PortState _
instance showPortState :: Show PortState where
  show = genericShow
instance decodePortState :: Decode PortState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePortState :: Encode PortState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutInstancePublicPortsRequest = PutInstancePublicPortsRequest 
  { "PortInfos'" :: (PortInfoList)
  , "InstanceName'" :: (ResourceName)
  }
derive instance newtypePutInstancePublicPortsRequest :: Newtype PutInstancePublicPortsRequest _
derive instance repGenericPutInstancePublicPortsRequest :: Generic PutInstancePublicPortsRequest _
instance showPutInstancePublicPortsRequest :: Show PutInstancePublicPortsRequest where
  show = genericShow
instance decodePutInstancePublicPortsRequest :: Decode PutInstancePublicPortsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutInstancePublicPortsRequest :: Encode PutInstancePublicPortsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutInstancePublicPortsResult = PutInstancePublicPortsResult 
  { "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypePutInstancePublicPortsResult :: Newtype PutInstancePublicPortsResult _
derive instance repGenericPutInstancePublicPortsResult :: Generic PutInstancePublicPortsResult _
instance showPutInstancePublicPortsResult :: Show PutInstancePublicPortsResult where
  show = genericShow
instance decodePutInstancePublicPortsResult :: Decode PutInstancePublicPortsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutInstancePublicPortsResult :: Encode PutInstancePublicPortsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RebootInstanceRequest = RebootInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  }
derive instance newtypeRebootInstanceRequest :: Newtype RebootInstanceRequest _
derive instance repGenericRebootInstanceRequest :: Generic RebootInstanceRequest _
instance showRebootInstanceRequest :: Show RebootInstanceRequest where
  show = genericShow
instance decodeRebootInstanceRequest :: Decode RebootInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebootInstanceRequest :: Encode RebootInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RebootInstanceResult = RebootInstanceResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeRebootInstanceResult :: Newtype RebootInstanceResult _
derive instance repGenericRebootInstanceResult :: Generic RebootInstanceResult _
instance showRebootInstanceResult :: Show RebootInstanceResult where
  show = genericShow
instance decodeRebootInstanceResult :: Decode RebootInstanceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebootInstanceResult :: Encode RebootInstanceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the AWS Region.</p>
newtype Region = Region 
  { "ContinentCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "DisplayName'" :: NullOrUndefined.NullOrUndefined (String)
  , "Name'" :: NullOrUndefined.NullOrUndefined (RegionName)
  , "AvailabilityZones'" :: NullOrUndefined.NullOrUndefined (AvailabilityZoneList)
  }
derive instance newtypeRegion :: Newtype Region _
derive instance repGenericRegion :: Generic Region _
instance showRegion :: Show Region where
  show = genericShow
instance decodeRegion :: Decode Region where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegion :: Encode Region where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegionList = RegionList (Array Region)
derive instance newtypeRegionList :: Newtype RegionList _
derive instance repGenericRegionList :: Generic RegionList _
instance showRegionList :: Show RegionList where
  show = genericShow
instance decodeRegionList :: Decode RegionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegionList :: Encode RegionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegionName = RegionName String
derive instance newtypeRegionName :: Newtype RegionName _
derive instance repGenericRegionName :: Generic RegionName _
instance showRegionName :: Show RegionName where
  show = genericShow
instance decodeRegionName :: Decode RegionName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegionName :: Encode RegionName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReleaseStaticIpRequest = ReleaseStaticIpRequest 
  { "StaticIpName'" :: (ResourceName)
  }
derive instance newtypeReleaseStaticIpRequest :: Newtype ReleaseStaticIpRequest _
derive instance repGenericReleaseStaticIpRequest :: Generic ReleaseStaticIpRequest _
instance showReleaseStaticIpRequest :: Show ReleaseStaticIpRequest where
  show = genericShow
instance decodeReleaseStaticIpRequest :: Decode ReleaseStaticIpRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReleaseStaticIpRequest :: Encode ReleaseStaticIpRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReleaseStaticIpResult = ReleaseStaticIpResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeReleaseStaticIpResult :: Newtype ReleaseStaticIpResult _
derive instance repGenericReleaseStaticIpResult :: Generic ReleaseStaticIpResult _
instance showReleaseStaticIpResult :: Show ReleaseStaticIpResult where
  show = genericShow
instance decodeReleaseStaticIpResult :: Decode ReleaseStaticIpResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReleaseStaticIpResult :: Encode ReleaseStaticIpResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the resource location.</p>
newtype ResourceLocation = ResourceLocation 
  { "AvailabilityZone'" :: NullOrUndefined.NullOrUndefined (String)
  , "RegionName'" :: NullOrUndefined.NullOrUndefined (RegionName)
  }
derive instance newtypeResourceLocation :: Newtype ResourceLocation _
derive instance repGenericResourceLocation :: Generic ResourceLocation _
instance showResourceLocation :: Show ResourceLocation where
  show = genericShow
instance decodeResourceLocation :: Decode ResourceLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceLocation :: Encode ResourceLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _
derive instance repGenericResourceName :: Generic ResourceName _
instance showResourceName :: Show ResourceName where
  show = genericShow
instance decodeResourceName :: Decode ResourceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceName :: Encode ResourceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceNameList = ResourceNameList (Array ResourceName)
derive instance newtypeResourceNameList :: Newtype ResourceNameList _
derive instance repGenericResourceNameList :: Generic ResourceNameList _
instance showResourceNameList :: Show ResourceNameList where
  show = genericShow
instance decodeResourceNameList :: Decode ResourceNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNameList :: Encode ResourceNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _
derive instance repGenericResourceType :: Generic ResourceType _
instance showResourceType :: Show ResourceType where
  show = genericShow
instance decodeResourceType :: Decode ResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceType :: Encode ResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A general service exception.</p>
newtype ServiceException = ServiceException 
  { "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeServiceException :: Newtype ServiceException _
derive instance repGenericServiceException :: Generic ServiceException _
instance showServiceException :: Show ServiceException where
  show = genericShow
instance decodeServiceException :: Decode ServiceException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceException :: Encode ServiceException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartInstanceRequest = StartInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  }
derive instance newtypeStartInstanceRequest :: Newtype StartInstanceRequest _
derive instance repGenericStartInstanceRequest :: Generic StartInstanceRequest _
instance showStartInstanceRequest :: Show StartInstanceRequest where
  show = genericShow
instance decodeStartInstanceRequest :: Decode StartInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartInstanceRequest :: Encode StartInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartInstanceResult = StartInstanceResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeStartInstanceResult :: Newtype StartInstanceResult _
derive instance repGenericStartInstanceResult :: Generic StartInstanceResult _
instance showStartInstanceResult :: Show StartInstanceResult where
  show = genericShow
instance decodeStartInstanceResult :: Decode StartInstanceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartInstanceResult :: Encode StartInstanceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the static IP.</p>
newtype StaticIp = StaticIp 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SupportCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (IsoDate)
  , "Location'" :: NullOrUndefined.NullOrUndefined (ResourceLocation)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "IpAddress'" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "AttachedTo'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "IsAttached'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeStaticIp :: Newtype StaticIp _
derive instance repGenericStaticIp :: Generic StaticIp _
instance showStaticIp :: Show StaticIp where
  show = genericShow
instance decodeStaticIp :: Decode StaticIp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStaticIp :: Encode StaticIp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StaticIpList = StaticIpList (Array StaticIp)
derive instance newtypeStaticIpList :: Newtype StaticIpList _
derive instance repGenericStaticIpList :: Generic StaticIpList _
instance showStaticIpList :: Show StaticIpList where
  show = genericShow
instance decodeStaticIpList :: Decode StaticIpList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStaticIpList :: Encode StaticIpList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopInstanceRequest = StopInstanceRequest 
  { "InstanceName'" :: (ResourceName)
  , "Force'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeStopInstanceRequest :: Newtype StopInstanceRequest _
derive instance repGenericStopInstanceRequest :: Generic StopInstanceRequest _
instance showStopInstanceRequest :: Show StopInstanceRequest where
  show = genericShow
instance decodeStopInstanceRequest :: Decode StopInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopInstanceRequest :: Encode StopInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopInstanceResult = StopInstanceResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeStopInstanceResult :: Newtype StopInstanceResult _
derive instance repGenericStopInstanceResult :: Generic StopInstanceResult _
instance showStopInstanceResult :: Show StopInstanceResult where
  show = genericShow
instance decodeStopInstanceResult :: Decode StopInstanceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopInstanceResult :: Encode StopInstanceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _
derive instance repGenericStringList :: Generic StringList _
instance showStringList :: Show StringList where
  show = genericShow
instance decodeStringList :: Decode StringList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringList :: Encode StringList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringMax256 = StringMax256 String
derive instance newtypeStringMax256 :: Newtype StringMax256 _
derive instance repGenericStringMax256 :: Generic StringMax256 _
instance showStringMax256 :: Show StringMax256 where
  show = genericShow
instance decodeStringMax256 :: Decode StringMax256 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringMax256 :: Encode StringMax256 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lightsail throws this exception when the user has not been authenticated.</p>
newtype UnauthenticatedException = UnauthenticatedException 
  { "Code'" :: NullOrUndefined.NullOrUndefined (String)
  , "Docs'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  , "Tip'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnauthenticatedException :: Newtype UnauthenticatedException _
derive instance repGenericUnauthenticatedException :: Generic UnauthenticatedException _
instance showUnauthenticatedException :: Show UnauthenticatedException where
  show = genericShow
instance decodeUnauthenticatedException :: Decode UnauthenticatedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnauthenticatedException :: Encode UnauthenticatedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UnpeerVpcRequest = UnpeerVpcRequest Types.NoArguments
derive instance newtypeUnpeerVpcRequest :: Newtype UnpeerVpcRequest _
derive instance repGenericUnpeerVpcRequest :: Generic UnpeerVpcRequest _
instance showUnpeerVpcRequest :: Show UnpeerVpcRequest where
  show = genericShow
instance decodeUnpeerVpcRequest :: Decode UnpeerVpcRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnpeerVpcRequest :: Encode UnpeerVpcRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UnpeerVpcResult = UnpeerVpcResult 
  { "Operation'" :: NullOrUndefined.NullOrUndefined (Operation)
  }
derive instance newtypeUnpeerVpcResult :: Newtype UnpeerVpcResult _
derive instance repGenericUnpeerVpcResult :: Generic UnpeerVpcResult _
instance showUnpeerVpcResult :: Show UnpeerVpcResult where
  show = genericShow
instance decodeUnpeerVpcResult :: Decode UnpeerVpcResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnpeerVpcResult :: Encode UnpeerVpcResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDomainEntryRequest = UpdateDomainEntryRequest 
  { "DomainName'" :: (DomainName)
  , "DomainEntry'" :: (DomainEntry)
  }
derive instance newtypeUpdateDomainEntryRequest :: Newtype UpdateDomainEntryRequest _
derive instance repGenericUpdateDomainEntryRequest :: Generic UpdateDomainEntryRequest _
instance showUpdateDomainEntryRequest :: Show UpdateDomainEntryRequest where
  show = genericShow
instance decodeUpdateDomainEntryRequest :: Decode UpdateDomainEntryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDomainEntryRequest :: Encode UpdateDomainEntryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDomainEntryResult = UpdateDomainEntryResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeUpdateDomainEntryResult :: Newtype UpdateDomainEntryResult _
derive instance repGenericUpdateDomainEntryResult :: Generic UpdateDomainEntryResult _
instance showUpdateDomainEntryResult :: Show UpdateDomainEntryResult where
  show = genericShow
instance decodeUpdateDomainEntryResult :: Decode UpdateDomainEntryResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDomainEntryResult :: Encode UpdateDomainEntryResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateLoadBalancerAttributeRequest = UpdateLoadBalancerAttributeRequest 
  { "LoadBalancerName'" :: (ResourceName)
  , "AttributeName'" :: (LoadBalancerAttributeName)
  , "AttributeValue'" :: (StringMax256)
  }
derive instance newtypeUpdateLoadBalancerAttributeRequest :: Newtype UpdateLoadBalancerAttributeRequest _
derive instance repGenericUpdateLoadBalancerAttributeRequest :: Generic UpdateLoadBalancerAttributeRequest _
instance showUpdateLoadBalancerAttributeRequest :: Show UpdateLoadBalancerAttributeRequest where
  show = genericShow
instance decodeUpdateLoadBalancerAttributeRequest :: Decode UpdateLoadBalancerAttributeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateLoadBalancerAttributeRequest :: Encode UpdateLoadBalancerAttributeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateLoadBalancerAttributeResult = UpdateLoadBalancerAttributeResult 
  { "Operations'" :: NullOrUndefined.NullOrUndefined (OperationList)
  }
derive instance newtypeUpdateLoadBalancerAttributeResult :: Newtype UpdateLoadBalancerAttributeResult _
derive instance repGenericUpdateLoadBalancerAttributeResult :: Generic UpdateLoadBalancerAttributeResult _
instance showUpdateLoadBalancerAttributeResult :: Show UpdateLoadBalancerAttributeResult where
  show = genericShow
instance decodeUpdateLoadBalancerAttributeResult :: Decode UpdateLoadBalancerAttributeResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateLoadBalancerAttributeResult :: Encode UpdateLoadBalancerAttributeResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
