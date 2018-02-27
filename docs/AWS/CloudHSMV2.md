## Module AWS.CloudHSMV2

<p>For more information about AWS CloudHSM, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a> and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createCluster`

``` purescript
createCluster :: forall eff. CreateClusterRequest -> Aff (err :: RequestError | eff) CreateClusterResponse
```

<p>Creates a new AWS CloudHSM cluster.</p>

#### `createHsm`

``` purescript
createHsm :: forall eff. CreateHsmRequest -> Aff (err :: RequestError | eff) CreateHsmResponse
```

<p>Creates a new hardware security module (HSM) in the specified AWS CloudHSM cluster.</p>

#### `deleteCluster`

``` purescript
deleteCluster :: forall eff. DeleteClusterRequest -> Aff (err :: RequestError | eff) DeleteClusterResponse
```

<p>Deletes the specified AWS CloudHSM cluster. Before you can delete a cluster, you must delete all HSMs in the cluster. To see if the cluster contains any HSMs, use <a>DescribeClusters</a>. To delete an HSM, use <a>DeleteHsm</a>.</p>

#### `deleteHsm`

``` purescript
deleteHsm :: forall eff. DeleteHsmRequest -> Aff (err :: RequestError | eff) DeleteHsmResponse
```

<p>Deletes the specified HSM. To specify an HSM, you can use its identifier (ID), the IP address of the HSM's elastic network interface (ENI), or the ID of the HSM's ENI. You need to specify only one of these values. To find these values, use <a>DescribeClusters</a>.</p>

#### `describeBackups`

``` purescript
describeBackups :: forall eff. DescribeBackupsRequest -> Aff (err :: RequestError | eff) DescribeBackupsResponse
```

<p>Gets information about backups of AWS CloudHSM clusters.</p> <p>This is a paginated operation, which means that each response might contain only a subset of all the backups. When the response contains only a subset of backups, it includes a <code>NextToken</code> value. Use this value in a subsequent <code>DescribeBackups</code> request to get more backups. When you receive a response with no <code>NextToken</code> (or an empty or null value), that means there are no more backups to get.</p>

#### `describeClusters`

``` purescript
describeClusters :: forall eff. DescribeClustersRequest -> Aff (err :: RequestError | eff) DescribeClustersResponse
```

<p>Gets information about AWS CloudHSM clusters.</p> <p>This is a paginated operation, which means that each response might contain only a subset of all the clusters. When the response contains only a subset of clusters, it includes a <code>NextToken</code> value. Use this value in a subsequent <code>DescribeClusters</code> request to get more clusters. When you receive a response with no <code>NextToken</code> (or an empty or null value), that means there are no more clusters to get.</p>

#### `initializeCluster`

``` purescript
initializeCluster :: forall eff. InitializeClusterRequest -> Aff (err :: RequestError | eff) InitializeClusterResponse
```

<p>Claims an AWS CloudHSM cluster by submitting the cluster certificate issued by your issuing certificate authority (CA) and the CA's root certificate. Before you can claim a cluster, you must sign the cluster's certificate signing request (CSR) with your issuing CA. To get the cluster's CSR, use <a>DescribeClusters</a>.</p>

#### `listTags`

``` purescript
listTags :: forall eff. ListTagsRequest -> Aff (err :: RequestError | eff) ListTagsResponse
```

<p>Gets a list of tags for the specified AWS CloudHSM cluster.</p> <p>This is a paginated operation, which means that each response might contain only a subset of all the tags. When the response contains only a subset of tags, it includes a <code>NextToken</code> value. Use this value in a subsequent <code>ListTags</code> request to get more tags. When you receive a response with no <code>NextToken</code> (or an empty or null value), that means there are no more tags to get.</p>

#### `tagResource`

``` purescript
tagResource :: forall eff. TagResourceRequest -> Aff (err :: RequestError | eff) TagResourceResponse
```

<p>Adds or overwrites one or more tags for the specified AWS CloudHSM cluster.</p>

#### `untagResource`

``` purescript
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: RequestError | eff) UntagResourceResponse
```

<p>Removes the specified tag or tags from the specified AWS CloudHSM cluster.</p>

#### `Backup`

``` purescript
newtype Backup
  = Backup { "BackupId" :: BackupId, "BackupState" :: NullOrUndefined (BackupState), "ClusterId" :: NullOrUndefined (ClusterId), "CreateTimestamp" :: NullOrUndefined (Number) }
```

<p>Contains information about a backup of an AWS CloudHSM cluster.</p>

##### Instances
``` purescript
Newtype Backup _
```

#### `BackupId`

``` purescript
newtype BackupId
  = BackupId String
```

##### Instances
``` purescript
Newtype BackupId _
```

#### `BackupPolicy`

``` purescript
newtype BackupPolicy
  = BackupPolicy String
```

##### Instances
``` purescript
Newtype BackupPolicy _
```

#### `BackupState`

``` purescript
newtype BackupState
  = BackupState String
```

##### Instances
``` purescript
Newtype BackupState _
```

#### `Backups`

``` purescript
newtype Backups
  = Backups (Array Backup)
```

##### Instances
``` purescript
Newtype Backups _
```

#### `Cert`

``` purescript
newtype Cert
  = Cert String
```

##### Instances
``` purescript
Newtype Cert _
```

#### `Certificates`

``` purescript
newtype Certificates
  = Certificates { "ClusterCsr" :: NullOrUndefined (Cert), "HsmCertificate" :: NullOrUndefined (Cert), "AwsHardwareCertificate" :: NullOrUndefined (Cert), "ManufacturerHardwareCertificate" :: NullOrUndefined (Cert), "ClusterCertificate" :: NullOrUndefined (Cert) }
```

<p>Contains one or more certificates or a certificate signing request (CSR).</p>

##### Instances
``` purescript
Newtype Certificates _
```

#### `CloudHsmAccessDeniedException`

``` purescript
newtype CloudHsmAccessDeniedException
  = CloudHsmAccessDeniedException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>The request was rejected because the requester does not have permission to perform the requested operation.</p>

##### Instances
``` purescript
Newtype CloudHsmAccessDeniedException _
```

#### `CloudHsmInternalFailureException`

``` purescript
newtype CloudHsmInternalFailureException
  = CloudHsmInternalFailureException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>The request was rejected because of an AWS CloudHSM internal failure. The request can be retried.</p>

##### Instances
``` purescript
Newtype CloudHsmInternalFailureException _
```

#### `CloudHsmInvalidRequestException`

``` purescript
newtype CloudHsmInvalidRequestException
  = CloudHsmInvalidRequestException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>The request was rejected because it is not a valid request.</p>

##### Instances
``` purescript
Newtype CloudHsmInvalidRequestException _
```

#### `CloudHsmResourceNotFoundException`

``` purescript
newtype CloudHsmResourceNotFoundException
  = CloudHsmResourceNotFoundException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>The request was rejected because it refers to a resource that cannot be found.</p>

##### Instances
``` purescript
Newtype CloudHsmResourceNotFoundException _
```

#### `CloudHsmServiceException`

``` purescript
newtype CloudHsmServiceException
  = CloudHsmServiceException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>The request was rejected because an error occurred.</p>

##### Instances
``` purescript
Newtype CloudHsmServiceException _
```

#### `Cluster`

``` purescript
newtype Cluster
  = Cluster { "BackupPolicy" :: NullOrUndefined (BackupPolicy), "ClusterId" :: NullOrUndefined (ClusterId), "CreateTimestamp" :: NullOrUndefined (Number), "Hsms" :: NullOrUndefined (Hsms), "HsmType" :: NullOrUndefined (HsmType), "PreCoPassword" :: NullOrUndefined (PreCoPassword), "SecurityGroup" :: NullOrUndefined (SecurityGroup), "SourceBackupId" :: NullOrUndefined (BackupId), "State" :: NullOrUndefined (ClusterState), "StateMessage" :: NullOrUndefined (StateMessage), "SubnetMapping" :: NullOrUndefined (ExternalSubnetMapping), "VpcId" :: NullOrUndefined (VpcId), "Certificates" :: NullOrUndefined (Certificates) }
```

<p>Contains information about an AWS CloudHSM cluster.</p>

##### Instances
``` purescript
Newtype Cluster _
```

#### `ClusterId`

``` purescript
newtype ClusterId
  = ClusterId String
```

##### Instances
``` purescript
Newtype ClusterId _
```

#### `ClusterState`

``` purescript
newtype ClusterState
  = ClusterState String
```

##### Instances
``` purescript
Newtype ClusterState _
```

#### `Clusters`

``` purescript
newtype Clusters
  = Clusters (Array Cluster)
```

##### Instances
``` purescript
Newtype Clusters _
```

#### `CreateClusterRequest`

``` purescript
newtype CreateClusterRequest
  = CreateClusterRequest { "SubnetIds" :: SubnetIds, "HsmType" :: HsmType, "SourceBackupId" :: NullOrUndefined (BackupId) }
```

##### Instances
``` purescript
Newtype CreateClusterRequest _
```

#### `CreateClusterResponse`

``` purescript
newtype CreateClusterResponse
  = CreateClusterResponse { "Cluster" :: NullOrUndefined (Cluster) }
```

##### Instances
``` purescript
Newtype CreateClusterResponse _
```

#### `CreateHsmRequest`

``` purescript
newtype CreateHsmRequest
  = CreateHsmRequest { "ClusterId" :: ClusterId, "AvailabilityZone" :: ExternalAz, "IpAddress" :: NullOrUndefined (IpAddress) }
```

##### Instances
``` purescript
Newtype CreateHsmRequest _
```

#### `CreateHsmResponse`

``` purescript
newtype CreateHsmResponse
  = CreateHsmResponse { "Hsm" :: NullOrUndefined (Hsm) }
```

##### Instances
``` purescript
Newtype CreateHsmResponse _
```

#### `DeleteClusterRequest`

``` purescript
newtype DeleteClusterRequest
  = DeleteClusterRequest { "ClusterId" :: ClusterId }
```

##### Instances
``` purescript
Newtype DeleteClusterRequest _
```

#### `DeleteClusterResponse`

``` purescript
newtype DeleteClusterResponse
  = DeleteClusterResponse { "Cluster" :: NullOrUndefined (Cluster) }
```

##### Instances
``` purescript
Newtype DeleteClusterResponse _
```

#### `DeleteHsmRequest`

``` purescript
newtype DeleteHsmRequest
  = DeleteHsmRequest { "ClusterId" :: ClusterId, "HsmId" :: NullOrUndefined (HsmId), "EniId" :: NullOrUndefined (EniId), "EniIp" :: NullOrUndefined (IpAddress) }
```

##### Instances
``` purescript
Newtype DeleteHsmRequest _
```

#### `DeleteHsmResponse`

``` purescript
newtype DeleteHsmResponse
  = DeleteHsmResponse { "HsmId" :: NullOrUndefined (HsmId) }
```

##### Instances
``` purescript
Newtype DeleteHsmResponse _
```

#### `DescribeBackupsRequest`

``` purescript
newtype DescribeBackupsRequest
  = DescribeBackupsRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxSize), "Filters" :: NullOrUndefined (Filters) }
```

##### Instances
``` purescript
Newtype DescribeBackupsRequest _
```

#### `DescribeBackupsResponse`

``` purescript
newtype DescribeBackupsResponse
  = DescribeBackupsResponse { "Backups" :: NullOrUndefined (Backups), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeBackupsResponse _
```

#### `DescribeClustersRequest`

``` purescript
newtype DescribeClustersRequest
  = DescribeClustersRequest { "Filters" :: NullOrUndefined (Filters), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxSize) }
```

##### Instances
``` purescript
Newtype DescribeClustersRequest _
```

#### `DescribeClustersResponse`

``` purescript
newtype DescribeClustersResponse
  = DescribeClustersResponse { "Clusters" :: NullOrUndefined (Clusters), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeClustersResponse _
```

#### `EniId`

``` purescript
newtype EniId
  = EniId String
```

##### Instances
``` purescript
Newtype EniId _
```

#### `ExternalAz`

``` purescript
newtype ExternalAz
  = ExternalAz String
```

##### Instances
``` purescript
Newtype ExternalAz _
```

#### `ExternalSubnetMapping`

``` purescript
newtype ExternalSubnetMapping
  = ExternalSubnetMapping (Map ExternalAz SubnetId)
```

##### Instances
``` purescript
Newtype ExternalSubnetMapping _
```

#### `Field`

``` purescript
newtype Field
  = Field String
```

##### Instances
``` purescript
Newtype Field _
```

#### `Filters`

``` purescript
newtype Filters
  = Filters (Map Field Strings)
```

##### Instances
``` purescript
Newtype Filters _
```

#### `Hsm`

``` purescript
newtype Hsm
  = Hsm { "AvailabilityZone" :: NullOrUndefined (ExternalAz), "ClusterId" :: NullOrUndefined (ClusterId), "SubnetId" :: NullOrUndefined (SubnetId), "EniId" :: NullOrUndefined (EniId), "EniIp" :: NullOrUndefined (IpAddress), "HsmId" :: HsmId, "State" :: NullOrUndefined (HsmState), "StateMessage" :: NullOrUndefined (String) }
```

<p>Contains information about a hardware security module (HSM) in an AWS CloudHSM cluster.</p>

##### Instances
``` purescript
Newtype Hsm _
```

#### `HsmId`

``` purescript
newtype HsmId
  = HsmId String
```

##### Instances
``` purescript
Newtype HsmId _
```

#### `HsmState`

``` purescript
newtype HsmState
  = HsmState String
```

##### Instances
``` purescript
Newtype HsmState _
```

#### `HsmType`

``` purescript
newtype HsmType
  = HsmType String
```

##### Instances
``` purescript
Newtype HsmType _
```

#### `Hsms`

``` purescript
newtype Hsms
  = Hsms (Array Hsm)
```

##### Instances
``` purescript
Newtype Hsms _
```

#### `InitializeClusterRequest`

``` purescript
newtype InitializeClusterRequest
  = InitializeClusterRequest { "ClusterId" :: ClusterId, "SignedCert" :: Cert, "TrustAnchor" :: Cert }
```

##### Instances
``` purescript
Newtype InitializeClusterRequest _
```

#### `InitializeClusterResponse`

``` purescript
newtype InitializeClusterResponse
  = InitializeClusterResponse { "State" :: NullOrUndefined (ClusterState), "StateMessage" :: NullOrUndefined (StateMessage) }
```

##### Instances
``` purescript
Newtype InitializeClusterResponse _
```

#### `IpAddress`

``` purescript
newtype IpAddress
  = IpAddress String
```

##### Instances
``` purescript
Newtype IpAddress _
```

#### `ListTagsRequest`

``` purescript
newtype ListTagsRequest
  = ListTagsRequest { "ResourceId" :: ClusterId, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxSize) }
```

##### Instances
``` purescript
Newtype ListTagsRequest _
```

#### `ListTagsResponse`

``` purescript
newtype ListTagsResponse
  = ListTagsResponse { "TagList" :: TagList, "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListTagsResponse _
```

#### `MaxSize`

``` purescript
newtype MaxSize
  = MaxSize Int
```

##### Instances
``` purescript
Newtype MaxSize _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

##### Instances
``` purescript
Newtype NextToken _
```

#### `PreCoPassword`

``` purescript
newtype PreCoPassword
  = PreCoPassword String
```

##### Instances
``` purescript
Newtype PreCoPassword _
```

#### `SecurityGroup`

``` purescript
newtype SecurityGroup
  = SecurityGroup String
```

##### Instances
``` purescript
Newtype SecurityGroup _
```

#### `StateMessage`

``` purescript
newtype StateMessage
  = StateMessage String
```

##### Instances
``` purescript
Newtype StateMessage _
```

#### `Strings`

``` purescript
newtype Strings
  = Strings (Array String)
```

##### Instances
``` purescript
Newtype Strings _
```

#### `SubnetId`

``` purescript
newtype SubnetId
  = SubnetId String
```

##### Instances
``` purescript
Newtype SubnetId _
```

#### `SubnetIds`

``` purescript
newtype SubnetIds
  = SubnetIds (Array SubnetId)
```

##### Instances
``` purescript
Newtype SubnetIds _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: TagValue }
```

<p>Contains a tag. A tag is a key-value pair.</p>

##### Instances
``` purescript
Newtype Tag _
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

##### Instances
``` purescript
Newtype TagKey _
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

##### Instances
``` purescript
Newtype TagKeyList _
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

##### Instances
``` purescript
Newtype TagList _
```

#### `TagResourceRequest`

``` purescript
newtype TagResourceRequest
  = TagResourceRequest { "ResourceId" :: ClusterId, "TagList" :: TagList }
```

##### Instances
``` purescript
Newtype TagResourceRequest _
```

#### `TagResourceResponse`

``` purescript
newtype TagResourceResponse
  = TagResourceResponse {  }
```

##### Instances
``` purescript
Newtype TagResourceResponse _
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

##### Instances
``` purescript
Newtype TagValue _
```

#### `UntagResourceRequest`

``` purescript
newtype UntagResourceRequest
  = UntagResourceRequest { "ResourceId" :: ClusterId, "TagKeyList" :: TagKeyList }
```

##### Instances
``` purescript
Newtype UntagResourceRequest _
```

#### `UntagResourceResponse`

``` purescript
newtype UntagResourceResponse
  = UntagResourceResponse {  }
```

##### Instances
``` purescript
Newtype UntagResourceResponse _
```

#### `VpcId`

``` purescript
newtype VpcId
  = VpcId String
```

##### Instances
``` purescript
Newtype VpcId _
```

#### `ErrorMessage'`

``` purescript
newtype ErrorMessage'
  = ErrorMessage' String
```

##### Instances
``` purescript
Newtype ErrorMessage' _
```


