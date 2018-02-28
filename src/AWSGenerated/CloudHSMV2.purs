

-- | <p>For more information about AWS CloudHSM, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a> and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>.</p>
module AWS.CloudHSMV2 where

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

serviceName = "CloudHSMV2" :: String


-- | <p>Creates a new AWS CloudHSM cluster.</p>
createCluster :: forall eff. CreateClusterRequest -> Aff (exception :: EXCEPTION | eff) CreateClusterResponse
createCluster = Request.request serviceName "createCluster" 


-- | <p>Creates a new hardware security module (HSM) in the specified AWS CloudHSM cluster.</p>
createHsm :: forall eff. CreateHsmRequest -> Aff (exception :: EXCEPTION | eff) CreateHsmResponse
createHsm = Request.request serviceName "createHsm" 


-- | <p>Deletes the specified AWS CloudHSM cluster. Before you can delete a cluster, you must delete all HSMs in the cluster. To see if the cluster contains any HSMs, use <a>DescribeClusters</a>. To delete an HSM, use <a>DeleteHsm</a>.</p>
deleteCluster :: forall eff. DeleteClusterRequest -> Aff (exception :: EXCEPTION | eff) DeleteClusterResponse
deleteCluster = Request.request serviceName "deleteCluster" 


-- | <p>Deletes the specified HSM. To specify an HSM, you can use its identifier (ID), the IP address of the HSM's elastic network interface (ENI), or the ID of the HSM's ENI. You need to specify only one of these values. To find these values, use <a>DescribeClusters</a>.</p>
deleteHsm :: forall eff. DeleteHsmRequest -> Aff (exception :: EXCEPTION | eff) DeleteHsmResponse
deleteHsm = Request.request serviceName "deleteHsm" 


-- | <p>Gets information about backups of AWS CloudHSM clusters.</p> <p>This is a paginated operation, which means that each response might contain only a subset of all the backups. When the response contains only a subset of backups, it includes a <code>NextToken</code> value. Use this value in a subsequent <code>DescribeBackups</code> request to get more backups. When you receive a response with no <code>NextToken</code> (or an empty or null value), that means there are no more backups to get.</p>
describeBackups :: forall eff. DescribeBackupsRequest -> Aff (exception :: EXCEPTION | eff) DescribeBackupsResponse
describeBackups = Request.request serviceName "describeBackups" 


-- | <p>Gets information about AWS CloudHSM clusters.</p> <p>This is a paginated operation, which means that each response might contain only a subset of all the clusters. When the response contains only a subset of clusters, it includes a <code>NextToken</code> value. Use this value in a subsequent <code>DescribeClusters</code> request to get more clusters. When you receive a response with no <code>NextToken</code> (or an empty or null value), that means there are no more clusters to get.</p>
describeClusters :: forall eff. DescribeClustersRequest -> Aff (exception :: EXCEPTION | eff) DescribeClustersResponse
describeClusters = Request.request serviceName "describeClusters" 


-- | <p>Claims an AWS CloudHSM cluster by submitting the cluster certificate issued by your issuing certificate authority (CA) and the CA's root certificate. Before you can claim a cluster, you must sign the cluster's certificate signing request (CSR) with your issuing CA. To get the cluster's CSR, use <a>DescribeClusters</a>.</p>
initializeCluster :: forall eff. InitializeClusterRequest -> Aff (exception :: EXCEPTION | eff) InitializeClusterResponse
initializeCluster = Request.request serviceName "initializeCluster" 


-- | <p>Gets a list of tags for the specified AWS CloudHSM cluster.</p> <p>This is a paginated operation, which means that each response might contain only a subset of all the tags. When the response contains only a subset of tags, it includes a <code>NextToken</code> value. Use this value in a subsequent <code>ListTags</code> request to get more tags. When you receive a response with no <code>NextToken</code> (or an empty or null value), that means there are no more tags to get.</p>
listTags :: forall eff. ListTagsRequest -> Aff (exception :: EXCEPTION | eff) ListTagsResponse
listTags = Request.request serviceName "listTags" 


-- | <p>Adds or overwrites one or more tags for the specified AWS CloudHSM cluster.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (exception :: EXCEPTION | eff) TagResourceResponse
tagResource = Request.request serviceName "tagResource" 


-- | <p>Removes the specified tag or tags from the specified AWS CloudHSM cluster.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (exception :: EXCEPTION | eff) UntagResourceResponse
untagResource = Request.request serviceName "untagResource" 


-- | <p>Contains information about a backup of an AWS CloudHSM cluster.</p>
newtype Backup = Backup 
  { "BackupId" :: (BackupId)
  , "BackupState" :: NullOrUndefined.NullOrUndefined (BackupState)
  , "ClusterId" :: NullOrUndefined.NullOrUndefined (ClusterId)
  , "CreateTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeBackup :: Newtype Backup _
derive instance repGenericBackup :: Generic Backup _
instance showBackup :: Show Backup where
  show = genericShow
instance decodeBackup :: Decode Backup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackup :: Encode Backup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BackupId = BackupId String
derive instance newtypeBackupId :: Newtype BackupId _
derive instance repGenericBackupId :: Generic BackupId _
instance showBackupId :: Show BackupId where
  show = genericShow
instance decodeBackupId :: Decode BackupId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackupId :: Encode BackupId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BackupPolicy = BackupPolicy String
derive instance newtypeBackupPolicy :: Newtype BackupPolicy _
derive instance repGenericBackupPolicy :: Generic BackupPolicy _
instance showBackupPolicy :: Show BackupPolicy where
  show = genericShow
instance decodeBackupPolicy :: Decode BackupPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackupPolicy :: Encode BackupPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BackupState = BackupState String
derive instance newtypeBackupState :: Newtype BackupState _
derive instance repGenericBackupState :: Generic BackupState _
instance showBackupState :: Show BackupState where
  show = genericShow
instance decodeBackupState :: Decode BackupState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackupState :: Encode BackupState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Backups = Backups (Array Backup)
derive instance newtypeBackups :: Newtype Backups _
derive instance repGenericBackups :: Generic Backups _
instance showBackups :: Show Backups where
  show = genericShow
instance decodeBackups :: Decode Backups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackups :: Encode Backups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Cert = Cert String
derive instance newtypeCert :: Newtype Cert _
derive instance repGenericCert :: Generic Cert _
instance showCert :: Show Cert where
  show = genericShow
instance decodeCert :: Decode Cert where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCert :: Encode Cert where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains one or more certificates or a certificate signing request (CSR).</p>
newtype Certificates = Certificates 
  { "ClusterCsr" :: NullOrUndefined.NullOrUndefined (Cert)
  , "HsmCertificate" :: NullOrUndefined.NullOrUndefined (Cert)
  , "AwsHardwareCertificate" :: NullOrUndefined.NullOrUndefined (Cert)
  , "ManufacturerHardwareCertificate" :: NullOrUndefined.NullOrUndefined (Cert)
  , "ClusterCertificate" :: NullOrUndefined.NullOrUndefined (Cert)
  }
derive instance newtypeCertificates :: Newtype Certificates _
derive instance repGenericCertificates :: Generic Certificates _
instance showCertificates :: Show Certificates where
  show = genericShow
instance decodeCertificates :: Decode Certificates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificates :: Encode Certificates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the requester does not have permission to perform the requested operation.</p>
newtype CloudHsmAccessDeniedException = CloudHsmAccessDeniedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCloudHsmAccessDeniedException :: Newtype CloudHsmAccessDeniedException _
derive instance repGenericCloudHsmAccessDeniedException :: Generic CloudHsmAccessDeniedException _
instance showCloudHsmAccessDeniedException :: Show CloudHsmAccessDeniedException where
  show = genericShow
instance decodeCloudHsmAccessDeniedException :: Decode CloudHsmAccessDeniedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudHsmAccessDeniedException :: Encode CloudHsmAccessDeniedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because of an AWS CloudHSM internal failure. The request can be retried.</p>
newtype CloudHsmInternalFailureException = CloudHsmInternalFailureException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCloudHsmInternalFailureException :: Newtype CloudHsmInternalFailureException _
derive instance repGenericCloudHsmInternalFailureException :: Generic CloudHsmInternalFailureException _
instance showCloudHsmInternalFailureException :: Show CloudHsmInternalFailureException where
  show = genericShow
instance decodeCloudHsmInternalFailureException :: Decode CloudHsmInternalFailureException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudHsmInternalFailureException :: Encode CloudHsmInternalFailureException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because it is not a valid request.</p>
newtype CloudHsmInvalidRequestException = CloudHsmInvalidRequestException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCloudHsmInvalidRequestException :: Newtype CloudHsmInvalidRequestException _
derive instance repGenericCloudHsmInvalidRequestException :: Generic CloudHsmInvalidRequestException _
instance showCloudHsmInvalidRequestException :: Show CloudHsmInvalidRequestException where
  show = genericShow
instance decodeCloudHsmInvalidRequestException :: Decode CloudHsmInvalidRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudHsmInvalidRequestException :: Encode CloudHsmInvalidRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because it refers to a resource that cannot be found.</p>
newtype CloudHsmResourceNotFoundException = CloudHsmResourceNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCloudHsmResourceNotFoundException :: Newtype CloudHsmResourceNotFoundException _
derive instance repGenericCloudHsmResourceNotFoundException :: Generic CloudHsmResourceNotFoundException _
instance showCloudHsmResourceNotFoundException :: Show CloudHsmResourceNotFoundException where
  show = genericShow
instance decodeCloudHsmResourceNotFoundException :: Decode CloudHsmResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudHsmResourceNotFoundException :: Encode CloudHsmResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because an error occurred.</p>
newtype CloudHsmServiceException = CloudHsmServiceException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCloudHsmServiceException :: Newtype CloudHsmServiceException _
derive instance repGenericCloudHsmServiceException :: Generic CloudHsmServiceException _
instance showCloudHsmServiceException :: Show CloudHsmServiceException where
  show = genericShow
instance decodeCloudHsmServiceException :: Decode CloudHsmServiceException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudHsmServiceException :: Encode CloudHsmServiceException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about an AWS CloudHSM cluster.</p>
newtype Cluster = Cluster 
  { "BackupPolicy" :: NullOrUndefined.NullOrUndefined (BackupPolicy)
  , "ClusterId" :: NullOrUndefined.NullOrUndefined (ClusterId)
  , "CreateTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "Hsms" :: NullOrUndefined.NullOrUndefined (Hsms)
  , "HsmType" :: NullOrUndefined.NullOrUndefined (HsmType)
  , "PreCoPassword" :: NullOrUndefined.NullOrUndefined (PreCoPassword)
  , "SecurityGroup" :: NullOrUndefined.NullOrUndefined (SecurityGroup)
  , "SourceBackupId" :: NullOrUndefined.NullOrUndefined (BackupId)
  , "State" :: NullOrUndefined.NullOrUndefined (ClusterState)
  , "StateMessage" :: NullOrUndefined.NullOrUndefined (StateMessage)
  , "SubnetMapping" :: NullOrUndefined.NullOrUndefined (ExternalSubnetMapping)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (VpcId)
  , "Certificates" :: NullOrUndefined.NullOrUndefined (Certificates)
  }
derive instance newtypeCluster :: Newtype Cluster _
derive instance repGenericCluster :: Generic Cluster _
instance showCluster :: Show Cluster where
  show = genericShow
instance decodeCluster :: Decode Cluster where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCluster :: Encode Cluster where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterId = ClusterId String
derive instance newtypeClusterId :: Newtype ClusterId _
derive instance repGenericClusterId :: Generic ClusterId _
instance showClusterId :: Show ClusterId where
  show = genericShow
instance decodeClusterId :: Decode ClusterId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterId :: Encode ClusterId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterState = ClusterState String
derive instance newtypeClusterState :: Newtype ClusterState _
derive instance repGenericClusterState :: Generic ClusterState _
instance showClusterState :: Show ClusterState where
  show = genericShow
instance decodeClusterState :: Decode ClusterState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterState :: Encode ClusterState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Clusters = Clusters (Array Cluster)
derive instance newtypeClusters :: Newtype Clusters _
derive instance repGenericClusters :: Generic Clusters _
instance showClusters :: Show Clusters where
  show = genericShow
instance decodeClusters :: Decode Clusters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusters :: Encode Clusters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateClusterRequest = CreateClusterRequest 
  { "SubnetIds" :: (SubnetIds)
  , "HsmType" :: (HsmType)
  , "SourceBackupId" :: NullOrUndefined.NullOrUndefined (BackupId)
  }
derive instance newtypeCreateClusterRequest :: Newtype CreateClusterRequest _
derive instance repGenericCreateClusterRequest :: Generic CreateClusterRequest _
instance showCreateClusterRequest :: Show CreateClusterRequest where
  show = genericShow
instance decodeCreateClusterRequest :: Decode CreateClusterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterRequest :: Encode CreateClusterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateClusterResponse = CreateClusterResponse 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeCreateClusterResponse :: Newtype CreateClusterResponse _
derive instance repGenericCreateClusterResponse :: Generic CreateClusterResponse _
instance showCreateClusterResponse :: Show CreateClusterResponse where
  show = genericShow
instance decodeCreateClusterResponse :: Decode CreateClusterResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterResponse :: Encode CreateClusterResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateHsmRequest = CreateHsmRequest 
  { "ClusterId" :: (ClusterId)
  , "AvailabilityZone" :: (ExternalAz)
  , "IpAddress" :: NullOrUndefined.NullOrUndefined (IpAddress)
  }
derive instance newtypeCreateHsmRequest :: Newtype CreateHsmRequest _
derive instance repGenericCreateHsmRequest :: Generic CreateHsmRequest _
instance showCreateHsmRequest :: Show CreateHsmRequest where
  show = genericShow
instance decodeCreateHsmRequest :: Decode CreateHsmRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHsmRequest :: Encode CreateHsmRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateHsmResponse = CreateHsmResponse 
  { "Hsm" :: NullOrUndefined.NullOrUndefined (Hsm)
  }
derive instance newtypeCreateHsmResponse :: Newtype CreateHsmResponse _
derive instance repGenericCreateHsmResponse :: Generic CreateHsmResponse _
instance showCreateHsmResponse :: Show CreateHsmResponse where
  show = genericShow
instance decodeCreateHsmResponse :: Decode CreateHsmResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHsmResponse :: Encode CreateHsmResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteClusterRequest = DeleteClusterRequest 
  { "ClusterId" :: (ClusterId)
  }
derive instance newtypeDeleteClusterRequest :: Newtype DeleteClusterRequest _
derive instance repGenericDeleteClusterRequest :: Generic DeleteClusterRequest _
instance showDeleteClusterRequest :: Show DeleteClusterRequest where
  show = genericShow
instance decodeDeleteClusterRequest :: Decode DeleteClusterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClusterRequest :: Encode DeleteClusterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteClusterResponse = DeleteClusterResponse 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeDeleteClusterResponse :: Newtype DeleteClusterResponse _
derive instance repGenericDeleteClusterResponse :: Generic DeleteClusterResponse _
instance showDeleteClusterResponse :: Show DeleteClusterResponse where
  show = genericShow
instance decodeDeleteClusterResponse :: Decode DeleteClusterResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClusterResponse :: Encode DeleteClusterResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteHsmRequest = DeleteHsmRequest 
  { "ClusterId" :: (ClusterId)
  , "HsmId" :: NullOrUndefined.NullOrUndefined (HsmId)
  , "EniId" :: NullOrUndefined.NullOrUndefined (EniId)
  , "EniIp" :: NullOrUndefined.NullOrUndefined (IpAddress)
  }
derive instance newtypeDeleteHsmRequest :: Newtype DeleteHsmRequest _
derive instance repGenericDeleteHsmRequest :: Generic DeleteHsmRequest _
instance showDeleteHsmRequest :: Show DeleteHsmRequest where
  show = genericShow
instance decodeDeleteHsmRequest :: Decode DeleteHsmRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteHsmRequest :: Encode DeleteHsmRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteHsmResponse = DeleteHsmResponse 
  { "HsmId" :: NullOrUndefined.NullOrUndefined (HsmId)
  }
derive instance newtypeDeleteHsmResponse :: Newtype DeleteHsmResponse _
derive instance repGenericDeleteHsmResponse :: Generic DeleteHsmResponse _
instance showDeleteHsmResponse :: Show DeleteHsmResponse where
  show = genericShow
instance decodeDeleteHsmResponse :: Decode DeleteHsmResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteHsmResponse :: Encode DeleteHsmResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeBackupsRequest = DescribeBackupsRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxSize)
  , "Filters" :: NullOrUndefined.NullOrUndefined (Filters)
  }
derive instance newtypeDescribeBackupsRequest :: Newtype DescribeBackupsRequest _
derive instance repGenericDescribeBackupsRequest :: Generic DescribeBackupsRequest _
instance showDescribeBackupsRequest :: Show DescribeBackupsRequest where
  show = genericShow
instance decodeDescribeBackupsRequest :: Decode DescribeBackupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBackupsRequest :: Encode DescribeBackupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeBackupsResponse = DescribeBackupsResponse 
  { "Backups" :: NullOrUndefined.NullOrUndefined (Backups)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeBackupsResponse :: Newtype DescribeBackupsResponse _
derive instance repGenericDescribeBackupsResponse :: Generic DescribeBackupsResponse _
instance showDescribeBackupsResponse :: Show DescribeBackupsResponse where
  show = genericShow
instance decodeDescribeBackupsResponse :: Decode DescribeBackupsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBackupsResponse :: Encode DescribeBackupsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeClustersRequest = DescribeClustersRequest 
  { "Filters" :: NullOrUndefined.NullOrUndefined (Filters)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxSize)
  }
derive instance newtypeDescribeClustersRequest :: Newtype DescribeClustersRequest _
derive instance repGenericDescribeClustersRequest :: Generic DescribeClustersRequest _
instance showDescribeClustersRequest :: Show DescribeClustersRequest where
  show = genericShow
instance decodeDescribeClustersRequest :: Decode DescribeClustersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClustersRequest :: Encode DescribeClustersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeClustersResponse = DescribeClustersResponse 
  { "Clusters" :: NullOrUndefined.NullOrUndefined (Clusters)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeClustersResponse :: Newtype DescribeClustersResponse _
derive instance repGenericDescribeClustersResponse :: Generic DescribeClustersResponse _
instance showDescribeClustersResponse :: Show DescribeClustersResponse where
  show = genericShow
instance decodeDescribeClustersResponse :: Decode DescribeClustersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClustersResponse :: Encode DescribeClustersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EniId = EniId String
derive instance newtypeEniId :: Newtype EniId _
derive instance repGenericEniId :: Generic EniId _
instance showEniId :: Show EniId where
  show = genericShow
instance decodeEniId :: Decode EniId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEniId :: Encode EniId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExternalAz = ExternalAz String
derive instance newtypeExternalAz :: Newtype ExternalAz _
derive instance repGenericExternalAz :: Generic ExternalAz _
instance showExternalAz :: Show ExternalAz where
  show = genericShow
instance decodeExternalAz :: Decode ExternalAz where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExternalAz :: Encode ExternalAz where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExternalSubnetMapping = ExternalSubnetMapping (StrMap.StrMap SubnetId)
derive instance newtypeExternalSubnetMapping :: Newtype ExternalSubnetMapping _
derive instance repGenericExternalSubnetMapping :: Generic ExternalSubnetMapping _
instance showExternalSubnetMapping :: Show ExternalSubnetMapping where
  show = genericShow
instance decodeExternalSubnetMapping :: Decode ExternalSubnetMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExternalSubnetMapping :: Encode ExternalSubnetMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Field = Field String
derive instance newtypeField :: Newtype Field _
derive instance repGenericField :: Generic Field _
instance showField :: Show Field where
  show = genericShow
instance decodeField :: Decode Field where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeField :: Encode Field where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Filters = Filters (StrMap.StrMap Strings)
derive instance newtypeFilters :: Newtype Filters _
derive instance repGenericFilters :: Generic Filters _
instance showFilters :: Show Filters where
  show = genericShow
instance decodeFilters :: Decode Filters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilters :: Encode Filters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about a hardware security module (HSM) in an AWS CloudHSM cluster.</p>
newtype Hsm = Hsm 
  { "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (ExternalAz)
  , "ClusterId" :: NullOrUndefined.NullOrUndefined (ClusterId)
  , "SubnetId" :: NullOrUndefined.NullOrUndefined (SubnetId)
  , "EniId" :: NullOrUndefined.NullOrUndefined (EniId)
  , "EniIp" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "HsmId" :: (HsmId)
  , "State" :: NullOrUndefined.NullOrUndefined (HsmState)
  , "StateMessage" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeHsm :: Newtype Hsm _
derive instance repGenericHsm :: Generic Hsm _
instance showHsm :: Show Hsm where
  show = genericShow
instance decodeHsm :: Decode Hsm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsm :: Encode Hsm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HsmId = HsmId String
derive instance newtypeHsmId :: Newtype HsmId _
derive instance repGenericHsmId :: Generic HsmId _
instance showHsmId :: Show HsmId where
  show = genericShow
instance decodeHsmId :: Decode HsmId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmId :: Encode HsmId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HsmState = HsmState String
derive instance newtypeHsmState :: Newtype HsmState _
derive instance repGenericHsmState :: Generic HsmState _
instance showHsmState :: Show HsmState where
  show = genericShow
instance decodeHsmState :: Decode HsmState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmState :: Encode HsmState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HsmType = HsmType String
derive instance newtypeHsmType :: Newtype HsmType _
derive instance repGenericHsmType :: Generic HsmType _
instance showHsmType :: Show HsmType where
  show = genericShow
instance decodeHsmType :: Decode HsmType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmType :: Encode HsmType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Hsms = Hsms (Array Hsm)
derive instance newtypeHsms :: Newtype Hsms _
derive instance repGenericHsms :: Generic Hsms _
instance showHsms :: Show Hsms where
  show = genericShow
instance decodeHsms :: Decode Hsms where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsms :: Encode Hsms where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InitializeClusterRequest = InitializeClusterRequest 
  { "ClusterId" :: (ClusterId)
  , "SignedCert" :: (Cert)
  , "TrustAnchor" :: (Cert)
  }
derive instance newtypeInitializeClusterRequest :: Newtype InitializeClusterRequest _
derive instance repGenericInitializeClusterRequest :: Generic InitializeClusterRequest _
instance showInitializeClusterRequest :: Show InitializeClusterRequest where
  show = genericShow
instance decodeInitializeClusterRequest :: Decode InitializeClusterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitializeClusterRequest :: Encode InitializeClusterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InitializeClusterResponse = InitializeClusterResponse 
  { "State" :: NullOrUndefined.NullOrUndefined (ClusterState)
  , "StateMessage" :: NullOrUndefined.NullOrUndefined (StateMessage)
  }
derive instance newtypeInitializeClusterResponse :: Newtype InitializeClusterResponse _
derive instance repGenericInitializeClusterResponse :: Generic InitializeClusterResponse _
instance showInitializeClusterResponse :: Show InitializeClusterResponse where
  show = genericShow
instance decodeInitializeClusterResponse :: Decode InitializeClusterResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitializeClusterResponse :: Encode InitializeClusterResponse where
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


newtype ListTagsRequest = ListTagsRequest 
  { "ResourceId" :: (ClusterId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxSize)
  }
derive instance newtypeListTagsRequest :: Newtype ListTagsRequest _
derive instance repGenericListTagsRequest :: Generic ListTagsRequest _
instance showListTagsRequest :: Show ListTagsRequest where
  show = genericShow
instance decodeListTagsRequest :: Decode ListTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsRequest :: Encode ListTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsResponse = ListTagsResponse 
  { "TagList" :: (TagList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListTagsResponse :: Newtype ListTagsResponse _
derive instance repGenericListTagsResponse :: Generic ListTagsResponse _
instance showListTagsResponse :: Show ListTagsResponse where
  show = genericShow
instance decodeListTagsResponse :: Decode ListTagsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsResponse :: Encode ListTagsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxSize = MaxSize Int
derive instance newtypeMaxSize :: Newtype MaxSize _
derive instance repGenericMaxSize :: Generic MaxSize _
instance showMaxSize :: Show MaxSize where
  show = genericShow
instance decodeMaxSize :: Decode MaxSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxSize :: Encode MaxSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PreCoPassword = PreCoPassword String
derive instance newtypePreCoPassword :: Newtype PreCoPassword _
derive instance repGenericPreCoPassword :: Generic PreCoPassword _
instance showPreCoPassword :: Show PreCoPassword where
  show = genericShow
instance decodePreCoPassword :: Decode PreCoPassword where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePreCoPassword :: Encode PreCoPassword where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityGroup = SecurityGroup String
derive instance newtypeSecurityGroup :: Newtype SecurityGroup _
derive instance repGenericSecurityGroup :: Generic SecurityGroup _
instance showSecurityGroup :: Show SecurityGroup where
  show = genericShow
instance decodeSecurityGroup :: Decode SecurityGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroup :: Encode SecurityGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StateMessage = StateMessage String
derive instance newtypeStateMessage :: Newtype StateMessage _
derive instance repGenericStateMessage :: Generic StateMessage _
instance showStateMessage :: Show StateMessage where
  show = genericShow
instance decodeStateMessage :: Decode StateMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateMessage :: Encode StateMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Strings = Strings (Array String)
derive instance newtypeStrings :: Newtype Strings _
derive instance repGenericStrings :: Generic Strings _
instance showStrings :: Show Strings where
  show = genericShow
instance decodeStrings :: Decode Strings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStrings :: Encode Strings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetId = SubnetId String
derive instance newtypeSubnetId :: Newtype SubnetId _
derive instance repGenericSubnetId :: Generic SubnetId _
instance showSubnetId :: Show SubnetId where
  show = genericShow
instance decodeSubnetId :: Decode SubnetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetId :: Encode SubnetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetIds = SubnetIds (Array SubnetId)
derive instance newtypeSubnetIds :: Newtype SubnetIds _
derive instance repGenericSubnetIds :: Generic SubnetIds _
instance showSubnetIds :: Show SubnetIds where
  show = genericShow
instance decodeSubnetIds :: Decode SubnetIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetIds :: Encode SubnetIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains a tag. A tag is a key-value pair.</p>
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


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagResourceRequest = TagResourceRequest 
  { "ResourceId" :: (ClusterId)
  , "TagList" :: (TagList)
  }
derive instance newtypeTagResourceRequest :: Newtype TagResourceRequest _
derive instance repGenericTagResourceRequest :: Generic TagResourceRequest _
instance showTagResourceRequest :: Show TagResourceRequest where
  show = genericShow
instance decodeTagResourceRequest :: Decode TagResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourceRequest :: Encode TagResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagResourceResponse = TagResourceResponse Types.NoArguments
derive instance newtypeTagResourceResponse :: Newtype TagResourceResponse _
derive instance repGenericTagResourceResponse :: Generic TagResourceResponse _
instance showTagResourceResponse :: Show TagResourceResponse where
  show = genericShow
instance decodeTagResourceResponse :: Decode TagResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourceResponse :: Encode TagResourceResponse where
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


newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceId" :: (ClusterId)
  , "TagKeyList" :: (TagKeyList)
  }
derive instance newtypeUntagResourceRequest :: Newtype UntagResourceRequest _
derive instance repGenericUntagResourceRequest :: Generic UntagResourceRequest _
instance showUntagResourceRequest :: Show UntagResourceRequest where
  show = genericShow
instance decodeUntagResourceRequest :: Decode UntagResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagResourceRequest :: Encode UntagResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UntagResourceResponse = UntagResourceResponse Types.NoArguments
derive instance newtypeUntagResourceResponse :: Newtype UntagResourceResponse _
derive instance repGenericUntagResourceResponse :: Generic UntagResourceResponse _
instance showUntagResourceResponse :: Show UntagResourceResponse where
  show = genericShow
instance decodeUntagResourceResponse :: Decode UntagResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagResourceResponse :: Encode UntagResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VpcId = VpcId String
derive instance newtypeVpcId :: Newtype VpcId _
derive instance repGenericVpcId :: Generic VpcId _
instance showVpcId :: Show VpcId where
  show = genericShow
instance decodeVpcId :: Decode VpcId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcId :: Encode VpcId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
derive instance repGenericErrorMessage' :: Generic ErrorMessage' _
instance showErrorMessage' :: Show ErrorMessage' where
  show = genericShow
instance decodeErrorMessage' :: Decode ErrorMessage' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage' :: Encode ErrorMessage' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
