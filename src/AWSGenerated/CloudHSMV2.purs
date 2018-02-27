

-- | <p>For more information about AWS CloudHSM, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a> and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>.</p>
module AWS.CloudHSMV2 where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
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
derive instance newtypeBackup :: Newtype Backup _


newtype BackupId = BackupId String
derive instance newtypeBackupId :: Newtype BackupId _


newtype BackupPolicy = BackupPolicy String
derive instance newtypeBackupPolicy :: Newtype BackupPolicy _


newtype BackupState = BackupState String
derive instance newtypeBackupState :: Newtype BackupState _


newtype Backups = Backups (Array Backup)
derive instance newtypeBackups :: Newtype Backups _


newtype Cert = Cert String
derive instance newtypeCert :: Newtype Cert _


-- | <p>Contains one or more certificates or a certificate signing request (CSR).</p>
newtype Certificates = Certificates 
  { "ClusterCsr" :: NullOrUndefined (Cert)
  , "HsmCertificate" :: NullOrUndefined (Cert)
  , "AwsHardwareCertificate" :: NullOrUndefined (Cert)
  , "ManufacturerHardwareCertificate" :: NullOrUndefined (Cert)
  , "ClusterCertificate" :: NullOrUndefined (Cert)
  }
derive instance newtypeCertificates :: Newtype Certificates _


-- | <p>The request was rejected because the requester does not have permission to perform the requested operation.</p>
newtype CloudHsmAccessDeniedException = CloudHsmAccessDeniedException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCloudHsmAccessDeniedException :: Newtype CloudHsmAccessDeniedException _


-- | <p>The request was rejected because of an AWS CloudHSM internal failure. The request can be retried.</p>
newtype CloudHsmInternalFailureException = CloudHsmInternalFailureException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCloudHsmInternalFailureException :: Newtype CloudHsmInternalFailureException _


-- | <p>The request was rejected because it is not a valid request.</p>
newtype CloudHsmInvalidRequestException = CloudHsmInvalidRequestException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCloudHsmInvalidRequestException :: Newtype CloudHsmInvalidRequestException _


-- | <p>The request was rejected because it refers to a resource that cannot be found.</p>
newtype CloudHsmResourceNotFoundException = CloudHsmResourceNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCloudHsmResourceNotFoundException :: Newtype CloudHsmResourceNotFoundException _


-- | <p>The request was rejected because an error occurred.</p>
newtype CloudHsmServiceException = CloudHsmServiceException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCloudHsmServiceException :: Newtype CloudHsmServiceException _


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
derive instance newtypeCluster :: Newtype Cluster _


newtype ClusterId = ClusterId String
derive instance newtypeClusterId :: Newtype ClusterId _


newtype ClusterState = ClusterState String
derive instance newtypeClusterState :: Newtype ClusterState _


newtype Clusters = Clusters (Array Cluster)
derive instance newtypeClusters :: Newtype Clusters _


newtype CreateClusterRequest = CreateClusterRequest 
  { "SubnetIds" :: (SubnetIds)
  , "HsmType" :: (HsmType)
  , "SourceBackupId" :: NullOrUndefined (BackupId)
  }
derive instance newtypeCreateClusterRequest :: Newtype CreateClusterRequest _


newtype CreateClusterResponse = CreateClusterResponse 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeCreateClusterResponse :: Newtype CreateClusterResponse _


newtype CreateHsmRequest = CreateHsmRequest 
  { "ClusterId" :: (ClusterId)
  , "AvailabilityZone" :: (ExternalAz)
  , "IpAddress" :: NullOrUndefined (IpAddress)
  }
derive instance newtypeCreateHsmRequest :: Newtype CreateHsmRequest _


newtype CreateHsmResponse = CreateHsmResponse 
  { "Hsm" :: NullOrUndefined (Hsm)
  }
derive instance newtypeCreateHsmResponse :: Newtype CreateHsmResponse _


newtype DeleteClusterRequest = DeleteClusterRequest 
  { "ClusterId" :: (ClusterId)
  }
derive instance newtypeDeleteClusterRequest :: Newtype DeleteClusterRequest _


newtype DeleteClusterResponse = DeleteClusterResponse 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeDeleteClusterResponse :: Newtype DeleteClusterResponse _


newtype DeleteHsmRequest = DeleteHsmRequest 
  { "ClusterId" :: (ClusterId)
  , "HsmId" :: NullOrUndefined (HsmId)
  , "EniId" :: NullOrUndefined (EniId)
  , "EniIp" :: NullOrUndefined (IpAddress)
  }
derive instance newtypeDeleteHsmRequest :: Newtype DeleteHsmRequest _


newtype DeleteHsmResponse = DeleteHsmResponse 
  { "HsmId" :: NullOrUndefined (HsmId)
  }
derive instance newtypeDeleteHsmResponse :: Newtype DeleteHsmResponse _


newtype DescribeBackupsRequest = DescribeBackupsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxSize)
  , "Filters" :: NullOrUndefined (Filters)
  }
derive instance newtypeDescribeBackupsRequest :: Newtype DescribeBackupsRequest _


newtype DescribeBackupsResponse = DescribeBackupsResponse 
  { "Backups" :: NullOrUndefined (Backups)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeBackupsResponse :: Newtype DescribeBackupsResponse _


newtype DescribeClustersRequest = DescribeClustersRequest 
  { "Filters" :: NullOrUndefined (Filters)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxSize)
  }
derive instance newtypeDescribeClustersRequest :: Newtype DescribeClustersRequest _


newtype DescribeClustersResponse = DescribeClustersResponse 
  { "Clusters" :: NullOrUndefined (Clusters)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeClustersResponse :: Newtype DescribeClustersResponse _


newtype EniId = EniId String
derive instance newtypeEniId :: Newtype EniId _


newtype ExternalAz = ExternalAz String
derive instance newtypeExternalAz :: Newtype ExternalAz _


newtype ExternalSubnetMapping = ExternalSubnetMapping (Map ExternalAz SubnetId)
derive instance newtypeExternalSubnetMapping :: Newtype ExternalSubnetMapping _


newtype Field = Field String
derive instance newtypeField :: Newtype Field _


newtype Filters = Filters (Map Field Strings)
derive instance newtypeFilters :: Newtype Filters _


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
derive instance newtypeHsm :: Newtype Hsm _


newtype HsmId = HsmId String
derive instance newtypeHsmId :: Newtype HsmId _


newtype HsmState = HsmState String
derive instance newtypeHsmState :: Newtype HsmState _


newtype HsmType = HsmType String
derive instance newtypeHsmType :: Newtype HsmType _


newtype Hsms = Hsms (Array Hsm)
derive instance newtypeHsms :: Newtype Hsms _


newtype InitializeClusterRequest = InitializeClusterRequest 
  { "ClusterId" :: (ClusterId)
  , "SignedCert" :: (Cert)
  , "TrustAnchor" :: (Cert)
  }
derive instance newtypeInitializeClusterRequest :: Newtype InitializeClusterRequest _


newtype InitializeClusterResponse = InitializeClusterResponse 
  { "State" :: NullOrUndefined (ClusterState)
  , "StateMessage" :: NullOrUndefined (StateMessage)
  }
derive instance newtypeInitializeClusterResponse :: Newtype InitializeClusterResponse _


newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _


newtype ListTagsRequest = ListTagsRequest 
  { "ResourceId" :: (ClusterId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxSize)
  }
derive instance newtypeListTagsRequest :: Newtype ListTagsRequest _


newtype ListTagsResponse = ListTagsResponse 
  { "TagList" :: (TagList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListTagsResponse :: Newtype ListTagsResponse _


newtype MaxSize = MaxSize Int
derive instance newtypeMaxSize :: Newtype MaxSize _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


newtype PreCoPassword = PreCoPassword String
derive instance newtypePreCoPassword :: Newtype PreCoPassword _


newtype SecurityGroup = SecurityGroup String
derive instance newtypeSecurityGroup :: Newtype SecurityGroup _


newtype StateMessage = StateMessage String
derive instance newtypeStateMessage :: Newtype StateMessage _


newtype Strings = Strings (Array String)
derive instance newtypeStrings :: Newtype Strings _


newtype SubnetId = SubnetId String
derive instance newtypeSubnetId :: Newtype SubnetId _


newtype SubnetIds = SubnetIds (Array SubnetId)
derive instance newtypeSubnetIds :: Newtype SubnetIds _


-- | <p>Contains a tag. A tag is a key-value pair.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagResourceRequest = TagResourceRequest 
  { "ResourceId" :: (ClusterId)
  , "TagList" :: (TagList)
  }
derive instance newtypeTagResourceRequest :: Newtype TagResourceRequest _


newtype TagResourceResponse = TagResourceResponse 
  { 
  }
derive instance newtypeTagResourceResponse :: Newtype TagResourceResponse _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceId" :: (ClusterId)
  , "TagKeyList" :: (TagKeyList)
  }
derive instance newtypeUntagResourceRequest :: Newtype UntagResourceRequest _


newtype UntagResourceResponse = UntagResourceResponse 
  { 
  }
derive instance newtypeUntagResourceResponse :: Newtype UntagResourceResponse _


newtype VpcId = VpcId String
derive instance newtypeVpcId :: Newtype VpcId _


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
