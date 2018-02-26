

-- | <p>For more information about AWS CloudHSM, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a> and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>.</p>
module AWS.CloudHSMV2 where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudHSMV2" :: String


-- | <p>Creates a new AWS CloudHSM cluster.</p>
createCluster :: forall eff. CreateClusterRequest -> Aff (err :: AWS.RequestError | eff) CreateClusterResponse
createCluster = AWS.request serviceName "CreateCluster" 


-- | <p>Creates a new hardware security module (HSM) in the specified AWS CloudHSM cluster.</p>
createHsm :: forall eff. CreateHsmRequest -> Aff (err :: AWS.RequestError | eff) CreateHsmResponse
createHsm = AWS.request serviceName "CreateHsm" 


-- | <p>Deletes the specified AWS CloudHSM cluster. Before you can delete a cluster, you must delete all HSMs in the cluster. To see if the cluster contains any HSMs, use <a>DescribeClusters</a>. To delete an HSM, use <a>DeleteHsm</a>.</p>
deleteCluster :: forall eff. DeleteClusterRequest -> Aff (err :: AWS.RequestError | eff) DeleteClusterResponse
deleteCluster = AWS.request serviceName "DeleteCluster" 


-- | <p>Deletes the specified HSM. To specify an HSM, you can use its identifier (ID), the IP address of the HSM's elastic network interface (ENI), or the ID of the HSM's ENI. You need to specify only one of these values. To find these values, use <a>DescribeClusters</a>.</p>
deleteHsm :: forall eff. DeleteHsmRequest -> Aff (err :: AWS.RequestError | eff) DeleteHsmResponse
deleteHsm = AWS.request serviceName "DeleteHsm" 


-- | <p>Gets information about backups of AWS CloudHSM clusters.</p> <p>This is a paginated operation, which means that each response might contain only a subset of all the backups. When the response contains only a subset of backups, it includes a <code>NextToken</code> value. Use this value in a subsequent <code>DescribeBackups</code> request to get more backups. When you receive a response with no <code>NextToken</code> (or an empty or null value), that means there are no more backups to get.</p>
describeBackups :: forall eff. DescribeBackupsRequest -> Aff (err :: AWS.RequestError | eff) DescribeBackupsResponse
describeBackups = AWS.request serviceName "DescribeBackups" 


-- | <p>Gets information about AWS CloudHSM clusters.</p> <p>This is a paginated operation, which means that each response might contain only a subset of all the clusters. When the response contains only a subset of clusters, it includes a <code>NextToken</code> value. Use this value in a subsequent <code>DescribeClusters</code> request to get more clusters. When you receive a response with no <code>NextToken</code> (or an empty or null value), that means there are no more clusters to get.</p>
describeClusters :: forall eff. DescribeClustersRequest -> Aff (err :: AWS.RequestError | eff) DescribeClustersResponse
describeClusters = AWS.request serviceName "DescribeClusters" 


-- | <p>Claims an AWS CloudHSM cluster by submitting the cluster certificate issued by your issuing certificate authority (CA) and the CA's root certificate. Before you can claim a cluster, you must sign the cluster's certificate signing request (CSR) with your issuing CA. To get the cluster's CSR, use <a>DescribeClusters</a>.</p>
initializeCluster :: forall eff. InitializeClusterRequest -> Aff (err :: AWS.RequestError | eff) InitializeClusterResponse
initializeCluster = AWS.request serviceName "InitializeCluster" 


-- | <p>Gets a list of tags for the specified AWS CloudHSM cluster.</p> <p>This is a paginated operation, which means that each response might contain only a subset of all the tags. When the response contains only a subset of tags, it includes a <code>NextToken</code> value. Use this value in a subsequent <code>ListTags</code> request to get more tags. When you receive a response with no <code>NextToken</code> (or an empty or null value), that means there are no more tags to get.</p>
listTags :: forall eff. ListTagsRequest -> Aff (err :: AWS.RequestError | eff) ListTagsResponse
listTags = AWS.request serviceName "ListTags" 


-- | <p>Adds or overwrites one or more tags for the specified AWS CloudHSM cluster.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (err :: AWS.RequestError | eff) TagResourceResponse
tagResource = AWS.request serviceName "TagResource" 


-- | <p>Removes the specified tag or tags from the specified AWS CloudHSM cluster.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: AWS.RequestError | eff) UntagResourceResponse
untagResource = AWS.request serviceName "UntagResource" 


-- | <p>Contains information about a backup of an AWS CloudHSM cluster.</p>
newtype Backup = Backup 
  { "BackupId" :: (BackupId)
  , "BackupState" :: NullOrUndefined (BackupState)
  , "ClusterId" :: NullOrUndefined (ClusterId)
  , "CreateTimestamp" :: NullOrUndefined (Number)
  }


newtype BackupId = BackupId String


newtype BackupPolicy = BackupPolicy String


newtype BackupState = BackupState String


newtype Backups = Backups (Array Backup)


newtype Cert = Cert String


-- | <p>Contains one or more certificates or a certificate signing request (CSR).</p>
newtype Certificates = Certificates 
  { "ClusterCsr" :: NullOrUndefined (Cert)
  , "HsmCertificate" :: NullOrUndefined (Cert)
  , "AwsHardwareCertificate" :: NullOrUndefined (Cert)
  , "ManufacturerHardwareCertificate" :: NullOrUndefined (Cert)
  , "ClusterCertificate" :: NullOrUndefined (Cert)
  }


-- | <p>The request was rejected because the requester does not have permission to perform the requested operation.</p>
newtype CloudHsmAccessDeniedException = CloudHsmAccessDeniedException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The request was rejected because of an AWS CloudHSM internal failure. The request can be retried.</p>
newtype CloudHsmInternalFailureException = CloudHsmInternalFailureException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The request was rejected because it is not a valid request.</p>
newtype CloudHsmInvalidRequestException = CloudHsmInvalidRequestException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The request was rejected because it refers to a resource that cannot be found.</p>
newtype CloudHsmResourceNotFoundException = CloudHsmResourceNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The request was rejected because an error occurred.</p>
newtype CloudHsmServiceException = CloudHsmServiceException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>Contains information about an AWS CloudHSM cluster.</p>
newtype Cluster = Cluster 
  { "BackupPolicy" :: NullOrUndefined (BackupPolicy)
  , "ClusterId" :: NullOrUndefined (ClusterId)
  , "CreateTimestamp" :: NullOrUndefined (Number)
  , "Hsms" :: NullOrUndefined (Hsms)
  , "HsmType" :: NullOrUndefined (HsmType)
  , "PreCoPassword" :: NullOrUndefined (PreCoPassword)
  , "SecurityGroup" :: NullOrUndefined (SecurityGroup)
  , "SourceBackupId" :: NullOrUndefined (BackupId)
  , "State" :: NullOrUndefined (ClusterState)
  , "StateMessage" :: NullOrUndefined (StateMessage)
  , "SubnetMapping" :: NullOrUndefined (ExternalSubnetMapping)
  , "VpcId" :: NullOrUndefined (VpcId)
  , "Certificates" :: NullOrUndefined (Certificates)
  }


newtype ClusterId = ClusterId String


newtype ClusterState = ClusterState String


newtype Clusters = Clusters (Array Cluster)


newtype CreateClusterRequest = CreateClusterRequest 
  { "SubnetIds" :: (SubnetIds)
  , "HsmType" :: (HsmType)
  , "SourceBackupId" :: NullOrUndefined (BackupId)
  }


newtype CreateClusterResponse = CreateClusterResponse 
  { "Cluster" :: NullOrUndefined (Cluster)
  }


newtype CreateHsmRequest = CreateHsmRequest 
  { "ClusterId" :: (ClusterId)
  , "AvailabilityZone" :: (ExternalAz)
  , "IpAddress" :: NullOrUndefined (IpAddress)
  }


newtype CreateHsmResponse = CreateHsmResponse 
  { "Hsm" :: NullOrUndefined (Hsm)
  }


newtype DeleteClusterRequest = DeleteClusterRequest 
  { "ClusterId" :: (ClusterId)
  }


newtype DeleteClusterResponse = DeleteClusterResponse 
  { "Cluster" :: NullOrUndefined (Cluster)
  }


newtype DeleteHsmRequest = DeleteHsmRequest 
  { "ClusterId" :: (ClusterId)
  , "HsmId" :: NullOrUndefined (HsmId)
  , "EniId" :: NullOrUndefined (EniId)
  , "EniIp" :: NullOrUndefined (IpAddress)
  }


newtype DeleteHsmResponse = DeleteHsmResponse 
  { "HsmId" :: NullOrUndefined (HsmId)
  }


newtype DescribeBackupsRequest = DescribeBackupsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxSize)
  , "Filters" :: NullOrUndefined (Filters)
  }


newtype DescribeBackupsResponse = DescribeBackupsResponse 
  { "Backups" :: NullOrUndefined (Backups)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeClustersRequest = DescribeClustersRequest 
  { "Filters" :: NullOrUndefined (Filters)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxSize)
  }


newtype DescribeClustersResponse = DescribeClustersResponse 
  { "Clusters" :: NullOrUndefined (Clusters)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype EniId = EniId String


newtype ExternalAz = ExternalAz String


newtype ExternalSubnetMapping = ExternalSubnetMapping (Map ExternalAz SubnetId)


newtype Field = Field String


newtype Filters = Filters (Map Field Strings)


-- | <p>Contains information about a hardware security module (HSM) in an AWS CloudHSM cluster.</p>
newtype Hsm = Hsm 
  { "AvailabilityZone" :: NullOrUndefined (ExternalAz)
  , "ClusterId" :: NullOrUndefined (ClusterId)
  , "SubnetId" :: NullOrUndefined (SubnetId)
  , "EniId" :: NullOrUndefined (EniId)
  , "EniIp" :: NullOrUndefined (IpAddress)
  , "HsmId" :: (HsmId)
  , "State" :: NullOrUndefined (HsmState)
  , "StateMessage" :: NullOrUndefined (String)
  }


newtype HsmId = HsmId String


newtype HsmState = HsmState String


newtype HsmType = HsmType String


newtype Hsms = Hsms (Array Hsm)


newtype InitializeClusterRequest = InitializeClusterRequest 
  { "ClusterId" :: (ClusterId)
  , "SignedCert" :: (Cert)
  , "TrustAnchor" :: (Cert)
  }


newtype InitializeClusterResponse = InitializeClusterResponse 
  { "State" :: NullOrUndefined (ClusterState)
  , "StateMessage" :: NullOrUndefined (StateMessage)
  }


newtype IpAddress = IpAddress String


newtype ListTagsRequest = ListTagsRequest 
  { "ResourceId" :: (ClusterId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxSize)
  }


newtype ListTagsResponse = ListTagsResponse 
  { "TagList" :: (TagList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype MaxSize = MaxSize Int


newtype NextToken = NextToken String


newtype PreCoPassword = PreCoPassword String


newtype SecurityGroup = SecurityGroup String


newtype StateMessage = StateMessage String


newtype Strings = Strings (Array String)


newtype SubnetId = SubnetId String


newtype SubnetIds = SubnetIds (Array SubnetId)


-- | <p>Contains a tag. A tag is a key-value pair.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array TagKey)


newtype TagList = TagList (Array Tag)


newtype TagResourceRequest = TagResourceRequest 
  { "ResourceId" :: (ClusterId)
  , "TagList" :: (TagList)
  }


newtype TagResourceResponse = TagResourceResponse 
  { 
  }


newtype TagValue = TagValue String


newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceId" :: (ClusterId)
  , "TagKeyList" :: (TagKeyList)
  }


newtype UntagResourceResponse = UntagResourceResponse 
  { 
  }


newtype VpcId = VpcId String


newtype ErrorMessage' = ErrorMessage' String
