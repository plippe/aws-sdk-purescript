## Module AWS.ElastiCache

<fullname>Amazon ElastiCache</fullname> <p>Amazon ElastiCache is a web service that makes it easier to set up, operate, and scale a distributed cache in the cloud.</p> <p>With ElastiCache, customers get all of the benefits of a high-performance, in-memory cache with less of the administrative burden involved in launching and managing a distributed cache. The service makes setup, scaling, and cluster failure handling much simpler than in a self-managed cache deployment.</p> <p>In addition, through integration with Amazon CloudWatch, customers get enhanced visibility into the key performance statistics associated with their cache and can receive alarms if a part of their cache runs hot.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addTagsToResource`

``` purescript
addTagsToResource :: forall eff. AddTagsToResourceMessage -> Aff (err :: RequestError | eff) TagListMessage
```

<p>Adds up to 50 cost allocation tags to the named resource. A cost allocation tag is a key-value pair where the key and value are case-sensitive. You can use cost allocation tags to categorize and track your AWS costs.</p> <p> When you apply tags to your ElastiCache resources, AWS generates a cost allocation report as a comma-separated value (CSV) file with your usage and costs aggregated by your tags. You can apply tags that represent business categories (such as cost centers, application names, or owners) to organize your costs across multiple services. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Tagging.html">Using Cost Allocation Tags in Amazon ElastiCache</a> in the <i>ElastiCache User Guide</i>.</p>

#### `authorizeCacheSecurityGroupIngress`

``` purescript
authorizeCacheSecurityGroupIngress :: forall eff. AuthorizeCacheSecurityGroupIngressMessage -> Aff (err :: RequestError | eff) AuthorizeCacheSecurityGroupIngressResult
```

<p>Allows network ingress to a cache security group. Applications using ElastiCache must be running on Amazon EC2, and Amazon EC2 security groups are used as the authorization mechanism.</p> <note> <p>You cannot authorize ingress from an Amazon EC2 security group in one region to an ElastiCache cluster in another region.</p> </note>

#### `copySnapshot`

``` purescript
copySnapshot :: forall eff. CopySnapshotMessage -> Aff (err :: RequestError | eff) CopySnapshotResult
```

<p>Makes a copy of an existing snapshot.</p> <note> <p>This operation is valid for Redis only.</p> </note> <important> <p>Users or groups that have permissions to use the <code>CopySnapshot</code> operation can create their own Amazon S3 buckets and copy snapshots to it. To control access to your snapshots, use an IAM policy to control who has the ability to use the <code>CopySnapshot</code> operation. For more information about using IAM to control the use of ElastiCache operations, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html">Exporting Snapshots</a> and <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/IAM.html">Authentication &amp; Access Control</a>.</p> </important> <p>You could receive the following error messages.</p> <p class="title"> <b>Error Messages</b> </p> <ul> <li> <p> <b>Error Message:</b> The S3 bucket %s is outside of the region.</p> <p> <b>Solution:</b> Create an Amazon S3 bucket in the same region as your snapshot. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.CreateBucket">Step 1: Create an Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message:</b> The S3 bucket %s does not exist.</p> <p> <b>Solution:</b> Create an Amazon S3 bucket in the same region as your snapshot. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.CreateBucket">Step 1: Create an Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message:</b> The S3 bucket %s is not owned by the authenticated user.</p> <p> <b>Solution:</b> Create an Amazon S3 bucket in the same region as your snapshot. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.CreateBucket">Step 1: Create an Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message:</b> The authenticated user does not have sufficient permissions to perform the desired activity.</p> <p> <b>Solution:</b> Contact your system administrator to get the needed permissions.</p> </li> <li> <p> <b>Error Message:</b> The S3 bucket %s already contains an object with key %s.</p> <p> <b>Solution:</b> Give the <code>TargetSnapshotName</code> a new and unique value. If exporting a snapshot, you could alternatively create a new Amazon S3 bucket and use this same value for <code>TargetSnapshotName</code>.</p> </li> <li> <p> <b>Error Message: </b> ElastiCache has not been granted READ permissions %s on the S3 Bucket.</p> <p> <b>Solution:</b> Add List and Read permissions on the bucket. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.GrantAccess">Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message: </b> ElastiCache has not been granted WRITE permissions %s on the S3 Bucket.</p> <p> <b>Solution:</b> Add Upload/Delete permissions on the bucket. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.GrantAccess">Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message: </b> ElastiCache has not been granted READ_ACP permissions %s on the S3 Bucket.</p> <p> <b>Solution:</b> Add View Permissions on the bucket. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.GrantAccess">Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> </ul>

#### `createCacheCluster`

``` purescript
createCacheCluster :: forall eff. CreateCacheClusterMessage -> Aff (err :: RequestError | eff) CreateCacheClusterResult
```

<p>Creates a cluster. All nodes in the cluster run the same protocol-compliant cache engine software, either Memcached or Redis.</p> <important> <p>Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups.</p> </important>

#### `createCacheParameterGroup`

``` purescript
createCacheParameterGroup :: forall eff. CreateCacheParameterGroupMessage -> Aff (err :: RequestError | eff) CreateCacheParameterGroupResult
```

<p>Creates a new Amazon ElastiCache cache parameter group. An ElastiCache cache parameter group is a collection of parameters and their values that are applied to all of the nodes in any cluster or replication group using the CacheParameterGroup.</p> <p>A newly created CacheParameterGroup is an exact duplicate of the default parameter group for the CacheParameterGroupFamily. To customize the newly created CacheParameterGroup you can change the values of specific parameters. For more information, see:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyCacheParameterGroup.html">ModifyCacheParameterGroup</a> in the ElastiCache API Reference.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/ParameterGroups.html">Parameters and Parameter Groups</a> in the ElastiCache User Guide.</p> </li> </ul>

#### `createCacheSecurityGroup`

``` purescript
createCacheSecurityGroup :: forall eff. CreateCacheSecurityGroupMessage -> Aff (err :: RequestError | eff) CreateCacheSecurityGroupResult
```

<p>Creates a new cache security group. Use a cache security group to control access to one or more clusters.</p> <p>Cache security groups are only used when you are creating a cluster outside of an Amazon Virtual Private Cloud (Amazon VPC). If you are creating a cluster inside of a VPC, use a cache subnet group instead. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheSubnetGroup.html">CreateCacheSubnetGroup</a>.</p>

#### `createCacheSubnetGroup`

``` purescript
createCacheSubnetGroup :: forall eff. CreateCacheSubnetGroupMessage -> Aff (err :: RequestError | eff) CreateCacheSubnetGroupResult
```

<p>Creates a new cache subnet group.</p> <p>Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).</p>

#### `createReplicationGroup`

``` purescript
createReplicationGroup :: forall eff. CreateReplicationGroupMessage -> Aff (err :: RequestError | eff) CreateReplicationGroupResult
```

<p>Creates a Redis (cluster mode disabled) or a Redis (cluster mode enabled) replication group.</p> <p>A Redis (cluster mode disabled) replication group is a collection of clusters, where one of the clusters is a read/write primary and the others are read-only replicas. Writes to the primary are asynchronously propagated to the replicas.</p> <p>A Redis (cluster mode enabled) replication group is a collection of 1 to 15 node groups (shards). Each node group (shard) has one read/write primary node and up to 5 read-only replica nodes. Writes to the primary are asynchronously propagated to the replicas. Redis (cluster mode enabled) replication groups partition the data across node groups (shards).</p> <p>When a Redis (cluster mode disabled) replication group has been successfully created, you can add one or more read replicas to it, up to a total of 5 read replicas. You cannot alter a Redis (cluster mode enabled) replication group after it has been created. However, if you need to increase or decrease the number of node groups (console: shards), you can avail yourself of ElastiCache for Redis' enhanced backup and restore. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/backups-restoring.html">Restoring From a Backup with Cluster Resizing</a> in the <i>ElastiCache User Guide</i>.</p> <note> <p>This operation is valid for Redis only.</p> </note>

#### `createSnapshot`

``` purescript
createSnapshot :: forall eff. CreateSnapshotMessage -> Aff (err :: RequestError | eff) CreateSnapshotResult
```

<p>Creates a copy of an entire cluster or replication group at a specific moment in time.</p> <note> <p>This operation is valid for Redis only.</p> </note>

#### `deleteCacheCluster`

``` purescript
deleteCacheCluster :: forall eff. DeleteCacheClusterMessage -> Aff (err :: RequestError | eff) DeleteCacheClusterResult
```

<p>Deletes a previously provisioned cluster. <code>DeleteCacheCluster</code> deletes all associated cache nodes, node endpoints and the cluster itself. When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the cluster; you cannot cancel or revert this operation.</p> <p>This operation cannot be used to delete a cluster that is the last read replica of a replication group or node group (shard) that has Multi-AZ mode enabled or a cluster from a Redis (cluster mode enabled) replication group.</p> <important> <p>Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups.</p> </important>

#### `deleteCacheParameterGroup`

``` purescript
deleteCacheParameterGroup :: forall eff. DeleteCacheParameterGroupMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified cache parameter group. You cannot delete a cache parameter group if it is associated with any cache clusters.</p>

#### `deleteCacheSecurityGroup`

``` purescript
deleteCacheSecurityGroup :: forall eff. DeleteCacheSecurityGroupMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a cache security group.</p> <note> <p>You cannot delete a cache security group if it is associated with any clusters.</p> </note>

#### `deleteCacheSubnetGroup`

``` purescript
deleteCacheSubnetGroup :: forall eff. DeleteCacheSubnetGroupMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a cache subnet group.</p> <note> <p>You cannot delete a cache subnet group if it is associated with any clusters.</p> </note>

#### `deleteReplicationGroup`

``` purescript
deleteReplicationGroup :: forall eff. DeleteReplicationGroupMessage -> Aff (err :: RequestError | eff) DeleteReplicationGroupResult
```

<p>Deletes an existing replication group. By default, this operation deletes the entire replication group, including the primary/primaries and all of the read replicas. If the replication group has only one primary, you can optionally delete only the read replicas, while retaining the primary by setting <code>RetainPrimaryCluster=true</code>.</p> <p>When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the selected resources; you cannot cancel or revert this operation.</p> <note> <p>This operation is valid for Redis only.</p> </note>

#### `deleteSnapshot`

``` purescript
deleteSnapshot :: forall eff. DeleteSnapshotMessage -> Aff (err :: RequestError | eff) DeleteSnapshotResult
```

<p>Deletes an existing snapshot. When you receive a successful response from this operation, ElastiCache immediately begins deleting the snapshot; you cannot cancel or revert this operation.</p> <note> <p>This operation is valid for Redis only.</p> </note>

#### `describeCacheClusters`

``` purescript
describeCacheClusters :: forall eff. DescribeCacheClustersMessage -> Aff (err :: RequestError | eff) CacheClusterMessage
```

<p>Returns information about all provisioned clusters if no cluster identifier is specified, or about a specific cache cluster if a cluster identifier is supplied.</p> <p>By default, abbreviated information about the clusters is returned. You can use the optional <i>ShowCacheNodeInfo</i> flag to retrieve detailed information about the cache nodes associated with the clusters. These details include the DNS address and port for the cache node endpoint.</p> <p>If the cluster is in the <i>creating</i> state, only cluster-level information is displayed until all of the nodes are successfully provisioned.</p> <p>If the cluster is in the <i>deleting</i> state, only cluster-level information is displayed.</p> <p>If cache nodes are currently being added to the cluster, node endpoint information and creation time for the additional nodes are not displayed until they are completely provisioned. When the cluster state is <i>available</i>, the cluster is ready for use.</p> <p>If cache nodes are currently being removed from the cluster, no endpoint information for the removed nodes is displayed.</p>

#### `describeCacheEngineVersions`

``` purescript
describeCacheEngineVersions :: forall eff. DescribeCacheEngineVersionsMessage -> Aff (err :: RequestError | eff) CacheEngineVersionMessage
```

<p>Returns a list of the available cache engines and their versions.</p>

#### `describeCacheParameterGroups`

``` purescript
describeCacheParameterGroups :: forall eff. DescribeCacheParameterGroupsMessage -> Aff (err :: RequestError | eff) CacheParameterGroupsMessage
```

<p>Returns a list of cache parameter group descriptions. If a cache parameter group name is specified, the list contains only the descriptions for that group.</p>

#### `describeCacheParameters`

``` purescript
describeCacheParameters :: forall eff. DescribeCacheParametersMessage -> Aff (err :: RequestError | eff) CacheParameterGroupDetails
```

<p>Returns the detailed parameter list for a particular cache parameter group.</p>

#### `describeCacheSecurityGroups`

``` purescript
describeCacheSecurityGroups :: forall eff. DescribeCacheSecurityGroupsMessage -> Aff (err :: RequestError | eff) CacheSecurityGroupMessage
```

<p>Returns a list of cache security group descriptions. If a cache security group name is specified, the list contains only the description of that group.</p>

#### `describeCacheSubnetGroups`

``` purescript
describeCacheSubnetGroups :: forall eff. DescribeCacheSubnetGroupsMessage -> Aff (err :: RequestError | eff) CacheSubnetGroupMessage
```

<p>Returns a list of cache subnet group descriptions. If a subnet group name is specified, the list contains only the description of that group.</p>

#### `describeEngineDefaultParameters`

``` purescript
describeEngineDefaultParameters :: forall eff. DescribeEngineDefaultParametersMessage -> Aff (err :: RequestError | eff) DescribeEngineDefaultParametersResult
```

<p>Returns the default engine and system parameter information for the specified cache engine.</p>

#### `describeEvents`

``` purescript
describeEvents :: forall eff. DescribeEventsMessage -> Aff (err :: RequestError | eff) EventsMessage
```

<p>Returns events related to clusters, cache security groups, and cache parameter groups. You can obtain events specific to a particular cluster, cache security group, or cache parameter group by providing the name as a parameter.</p> <p>By default, only the events occurring within the last hour are returned; however, you can retrieve up to 14 days' worth of events if necessary.</p>

#### `describeReplicationGroups`

``` purescript
describeReplicationGroups :: forall eff. DescribeReplicationGroupsMessage -> Aff (err :: RequestError | eff) ReplicationGroupMessage
```

<p>Returns information about a particular replication group. If no identifier is specified, <code>DescribeReplicationGroups</code> returns information about all replication groups.</p> <note> <p>This operation is valid for Redis only.</p> </note>

#### `describeReservedCacheNodes`

``` purescript
describeReservedCacheNodes :: forall eff. DescribeReservedCacheNodesMessage -> Aff (err :: RequestError | eff) ReservedCacheNodeMessage
```

<p>Returns information about reserved cache nodes for this account, or about a specified reserved cache node.</p>

#### `describeReservedCacheNodesOfferings`

``` purescript
describeReservedCacheNodesOfferings :: forall eff. DescribeReservedCacheNodesOfferingsMessage -> Aff (err :: RequestError | eff) ReservedCacheNodesOfferingMessage
```

<p>Lists available reserved cache node offerings.</p>

#### `describeSnapshots`

``` purescript
describeSnapshots :: forall eff. DescribeSnapshotsMessage -> Aff (err :: RequestError | eff) DescribeSnapshotsListMessage
```

<p>Returns information about cluster or replication group snapshots. By default, <code>DescribeSnapshots</code> lists all of your snapshots; it can optionally describe a single snapshot, or just the snapshots associated with a particular cache cluster.</p> <note> <p>This operation is valid for Redis only.</p> </note>

#### `listAllowedNodeTypeModifications`

``` purescript
listAllowedNodeTypeModifications :: forall eff. ListAllowedNodeTypeModificationsMessage -> Aff (err :: RequestError | eff) AllowedNodeTypeModificationsMessage
```

<p>Lists all available node types that you can scale your Redis cluster's or replication group's current node type up to.</p> <p>When you use the <code>ModifyCacheCluster</code> or <code>ModifyReplicationGroup</code> operations to scale up your cluster or replication group, the value of the <code>CacheNodeType</code> parameter must be one of the node types returned by this operation.</p>

#### `listTagsForResource`

``` purescript
listTagsForResource :: forall eff. ListTagsForResourceMessage -> Aff (err :: RequestError | eff) TagListMessage
```

<p>Lists all cost allocation tags currently on the named resource. A <code>cost allocation tag</code> is a key-value pair where the key is case-sensitive and the value is optional. You can use cost allocation tags to categorize and track your AWS costs.</p> <p>You can have a maximum of 50 cost allocation tags on an ElastiCache resource. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/BestPractices.html">Using Cost Allocation Tags in Amazon ElastiCache</a>.</p>

#### `modifyCacheCluster`

``` purescript
modifyCacheCluster :: forall eff. ModifyCacheClusterMessage -> Aff (err :: RequestError | eff) ModifyCacheClusterResult
```

<p>Modifies the settings for a cluster. You can use this operation to change one or more cluster configuration parameters by specifying the parameters and the new values.</p>

#### `modifyCacheParameterGroup`

``` purescript
modifyCacheParameterGroup :: forall eff. ModifyCacheParameterGroupMessage -> Aff (err :: RequestError | eff) CacheParameterGroupNameMessage
```

<p>Modifies the parameters of a cache parameter group. You can modify up to 20 parameters in a single request by submitting a list parameter name and value pairs.</p>

#### `modifyCacheSubnetGroup`

``` purescript
modifyCacheSubnetGroup :: forall eff. ModifyCacheSubnetGroupMessage -> Aff (err :: RequestError | eff) ModifyCacheSubnetGroupResult
```

<p>Modifies an existing cache subnet group.</p>

#### `modifyReplicationGroup`

``` purescript
modifyReplicationGroup :: forall eff. ModifyReplicationGroupMessage -> Aff (err :: RequestError | eff) ModifyReplicationGroupResult
```

<p>Modifies the settings for a replication group.</p> <important> <p>Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups.</p> </important> <note> <p>This operation is valid for Redis only.</p> </note>

#### `modifyReplicationGroupShardConfiguration`

``` purescript
modifyReplicationGroupShardConfiguration :: forall eff. ModifyReplicationGroupShardConfigurationMessage -> Aff (err :: RequestError | eff) ModifyReplicationGroupShardConfigurationResult
```

<p>Performs horizontal scaling on a Redis (cluster mode enabled) cluster with no downtime. Requires Redis engine version 3.2.10 or newer. For information on upgrading your engine to a newer version, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/VersionManagement.html">Upgrading Engine Versions</a> in the Amazon ElastiCache User Guide.</p> <p>For more information on ElastiCache for Redis online horizontal scaling, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/redis-cluster-resharding-online.html">ElastiCache for Redis Horizontal Scaling</a> </p>

#### `purchaseReservedCacheNodesOffering`

``` purescript
purchaseReservedCacheNodesOffering :: forall eff. PurchaseReservedCacheNodesOfferingMessage -> Aff (err :: RequestError | eff) PurchaseReservedCacheNodesOfferingResult
```

<p>Allows you to purchase a reserved cache node offering.</p>

#### `rebootCacheCluster`

``` purescript
rebootCacheCluster :: forall eff. RebootCacheClusterMessage -> Aff (err :: RequestError | eff) RebootCacheClusterResult
```

<p>Reboots some, or all, of the cache nodes within a provisioned cluster. This operation applies any modified cache parameter groups to the cluster. The reboot operation takes place as soon as possible, and results in a momentary outage to the cluster. During the reboot, the cluster status is set to REBOOTING.</p> <p>The reboot causes the contents of the cache (for each cache node being rebooted) to be lost.</p> <p>When the reboot is complete, a cluster event is created.</p> <p>Rebooting a cluster is currently supported on Memcached and Redis (cluster mode disabled) clusters. Rebooting is not supported on Redis (cluster mode enabled) clusters.</p> <p>If you make changes to parameters that require a Redis (cluster mode enabled) cluster reboot for the changes to be applied, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Clusters.Rebooting.htm">Rebooting a Cluster</a> for an alternate process.</p>

#### `removeTagsFromResource`

``` purescript
removeTagsFromResource :: forall eff. RemoveTagsFromResourceMessage -> Aff (err :: RequestError | eff) TagListMessage
```

<p>Removes the tags identified by the <code>TagKeys</code> list from the named resource.</p>

#### `resetCacheParameterGroup`

``` purescript
resetCacheParameterGroup :: forall eff. ResetCacheParameterGroupMessage -> Aff (err :: RequestError | eff) CacheParameterGroupNameMessage
```

<p>Modifies the parameters of a cache parameter group to the engine or system default value. You can reset specific parameters by submitting a list of parameter names. To reset the entire cache parameter group, specify the <code>ResetAllParameters</code> and <code>CacheParameterGroupName</code> parameters.</p>

#### `revokeCacheSecurityGroupIngress`

``` purescript
revokeCacheSecurityGroupIngress :: forall eff. RevokeCacheSecurityGroupIngressMessage -> Aff (err :: RequestError | eff) RevokeCacheSecurityGroupIngressResult
```

<p>Revokes ingress from a cache security group. Use this operation to disallow access from an Amazon EC2 security group that had been previously authorized.</p>

#### `testFailover`

``` purescript
testFailover :: forall eff. TestFailoverMessage -> Aff (err :: RequestError | eff) TestFailoverResult
```

<p>Represents the input of a <code>TestFailover</code> operation which test automatic failover on a specified node group (called shard in the console) in a replication group (called cluster in the console).</p> <p class="title"> <b>Note the following</b> </p> <ul> <li> <p>A customer can use this operation to test automatic failover on up to 5 shards (called node groups in the ElastiCache API and AWS CLI) in any rolling 24-hour period.</p> </li> <li> <p>If calling this operation on shards in different clusters (called replication groups in the API and CLI), the calls can be made concurrently.</p> <p> </p> </li> <li> <p>If calling this operation multiple times on different shards in the same Redis (cluster mode enabled) replication group, the first node replacement must complete before a subsequent call can be made.</p> </li> <li> <p>To determine whether the node replacement is complete you can check Events using the Amazon ElastiCache console, the AWS CLI, or the ElastiCache API. Look for the following automatic failover related events, listed here in order of occurrance:</p> <ol> <li> <p>Replication group message: <code>Test Failover API called for node group &lt;node-group-id&gt;</code> </p> </li> <li> <p>Cache cluster message: <code>Failover from master node &lt;primary-node-id&gt; to replica node &lt;node-id&gt; completed</code> </p> </li> <li> <p>Replication group message: <code>Failover from master node &lt;primary-node-id&gt; to replica node &lt;node-id&gt; completed</code> </p> </li> <li> <p>Cache cluster message: <code>Recovering cache nodes &lt;node-id&gt;</code> </p> </li> <li> <p>Cache cluster message: <code>Finished recovery for cache nodes &lt;node-id&gt;</code> </p> </li> </ol> <p>For more information see:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/ECEvents.Viewing.html">Viewing ElastiCache Events</a> in the <i>ElastiCache User Guide</i> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeEvents.html">DescribeEvents</a> in the ElastiCache API Reference</p> </li> </ul> </li> </ul> <p>Also see, <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/AutoFailover.html#auto-failover-test">Testing Multi-AZ with Automatic Failover</a> in the <i>ElastiCache User Guide</i>.</p>

#### `APICallRateForCustomerExceededFault`

``` purescript
newtype APICallRateForCustomerExceededFault
  = APICallRateForCustomerExceededFault {  }
```

<p>The customer has exceeded the allowed rate of API calls.</p>

##### Instances
``` purescript
Newtype APICallRateForCustomerExceededFault _
```

#### `AZMode`

``` purescript
newtype AZMode
  = AZMode String
```

##### Instances
``` purescript
Newtype AZMode _
```

#### `AddTagsToResourceMessage`

``` purescript
newtype AddTagsToResourceMessage
  = AddTagsToResourceMessage { "ResourceName" :: String, "Tags" :: TagList }
```

<p>Represents the input of an AddTagsToResource operation.</p>

##### Instances
``` purescript
Newtype AddTagsToResourceMessage _
```

#### `AllowedNodeTypeModificationsMessage`

``` purescript
newtype AllowedNodeTypeModificationsMessage
  = AllowedNodeTypeModificationsMessage { "ScaleUpModifications" :: NullOrUndefined (NodeTypeList) }
```

<p>Represents the allowed node types you can use to modify your cluster or replication group.</p>

##### Instances
``` purescript
Newtype AllowedNodeTypeModificationsMessage _
```

#### `AuthorizationAlreadyExistsFault`

``` purescript
newtype AuthorizationAlreadyExistsFault
  = AuthorizationAlreadyExistsFault {  }
```

<p>The specified Amazon EC2 security group is already authorized for the specified cache security group.</p>

##### Instances
``` purescript
Newtype AuthorizationAlreadyExistsFault _
```

#### `AuthorizationNotFoundFault`

``` purescript
newtype AuthorizationNotFoundFault
  = AuthorizationNotFoundFault {  }
```

<p>The specified Amazon EC2 security group is not authorized for the specified cache security group.</p>

##### Instances
``` purescript
Newtype AuthorizationNotFoundFault _
```

#### `AuthorizeCacheSecurityGroupIngressMessage`

``` purescript
newtype AuthorizeCacheSecurityGroupIngressMessage
  = AuthorizeCacheSecurityGroupIngressMessage { "CacheSecurityGroupName" :: String, "EC2SecurityGroupName" :: String, "EC2SecurityGroupOwnerId" :: String }
```

<p>Represents the input of an AuthorizeCacheSecurityGroupIngress operation.</p>

##### Instances
``` purescript
Newtype AuthorizeCacheSecurityGroupIngressMessage _
```

#### `AuthorizeCacheSecurityGroupIngressResult`

``` purescript
newtype AuthorizeCacheSecurityGroupIngressResult
  = AuthorizeCacheSecurityGroupIngressResult { "CacheSecurityGroup" :: NullOrUndefined (CacheSecurityGroup) }
```

##### Instances
``` purescript
Newtype AuthorizeCacheSecurityGroupIngressResult _
```

#### `AutomaticFailoverStatus`

``` purescript
newtype AutomaticFailoverStatus
  = AutomaticFailoverStatus String
```

##### Instances
``` purescript
Newtype AutomaticFailoverStatus _
```

#### `AvailabilityZone`

``` purescript
newtype AvailabilityZone
  = AvailabilityZone { "Name" :: NullOrUndefined (String) }
```

<p>Describes an Availability Zone in which the cluster is launched.</p>

##### Instances
``` purescript
Newtype AvailabilityZone _
```

#### `AvailabilityZonesList`

``` purescript
newtype AvailabilityZonesList
  = AvailabilityZonesList (Array String)
```

##### Instances
``` purescript
Newtype AvailabilityZonesList _
```

#### `AwsQueryErrorMessage`

``` purescript
newtype AwsQueryErrorMessage
  = AwsQueryErrorMessage String
```

##### Instances
``` purescript
Newtype AwsQueryErrorMessage _
```

#### `BooleanOptional`

``` purescript
newtype BooleanOptional
  = BooleanOptional Boolean
```

##### Instances
``` purescript
Newtype BooleanOptional _
```

#### `CacheCluster`

``` purescript
newtype CacheCluster
  = CacheCluster { "CacheClusterId" :: NullOrUndefined (String), "ConfigurationEndpoint" :: NullOrUndefined (Endpoint), "ClientDownloadLandingPage" :: NullOrUndefined (String), "CacheNodeType" :: NullOrUndefined (String), "Engine" :: NullOrUndefined (String), "EngineVersion" :: NullOrUndefined (String), "CacheClusterStatus" :: NullOrUndefined (String), "NumCacheNodes" :: NullOrUndefined (IntegerOptional), "PreferredAvailabilityZone" :: NullOrUndefined (String), "CacheClusterCreateTime" :: NullOrUndefined (TStamp), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "PendingModifiedValues" :: NullOrUndefined (PendingModifiedValues), "NotificationConfiguration" :: NullOrUndefined (NotificationConfiguration), "CacheSecurityGroups" :: NullOrUndefined (CacheSecurityGroupMembershipList), "CacheParameterGroup" :: NullOrUndefined (CacheParameterGroupStatus), "CacheSubnetGroupName" :: NullOrUndefined (String), "CacheNodes" :: NullOrUndefined (CacheNodeList), "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean), "SecurityGroups" :: NullOrUndefined (SecurityGroupMembershipList), "ReplicationGroupId" :: NullOrUndefined (String), "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional), "SnapshotWindow" :: NullOrUndefined (String), "AuthTokenEnabled" :: NullOrUndefined (BooleanOptional), "TransitEncryptionEnabled" :: NullOrUndefined (BooleanOptional), "AtRestEncryptionEnabled" :: NullOrUndefined (BooleanOptional) }
```

<p>Contains all of the attributes of a specific cluster.</p>

##### Instances
``` purescript
Newtype CacheCluster _
```

#### `CacheClusterAlreadyExistsFault`

``` purescript
newtype CacheClusterAlreadyExistsFault
  = CacheClusterAlreadyExistsFault {  }
```

<p>You already have a cluster with the given identifier.</p>

##### Instances
``` purescript
Newtype CacheClusterAlreadyExistsFault _
```

#### `CacheClusterList`

``` purescript
newtype CacheClusterList
  = CacheClusterList (Array CacheCluster)
```

##### Instances
``` purescript
Newtype CacheClusterList _
```

#### `CacheClusterMessage`

``` purescript
newtype CacheClusterMessage
  = CacheClusterMessage { "Marker" :: NullOrUndefined (String), "CacheClusters" :: NullOrUndefined (CacheClusterList) }
```

<p>Represents the output of a <code>DescribeCacheClusters</code> operation.</p>

##### Instances
``` purescript
Newtype CacheClusterMessage _
```

#### `CacheClusterNotFoundFault`

``` purescript
newtype CacheClusterNotFoundFault
  = CacheClusterNotFoundFault {  }
```

<p>The requested cluster ID does not refer to an existing cluster.</p>

##### Instances
``` purescript
Newtype CacheClusterNotFoundFault _
```

#### `CacheEngineVersion`

``` purescript
newtype CacheEngineVersion
  = CacheEngineVersion { "Engine" :: NullOrUndefined (String), "EngineVersion" :: NullOrUndefined (String), "CacheParameterGroupFamily" :: NullOrUndefined (String), "CacheEngineDescription" :: NullOrUndefined (String), "CacheEngineVersionDescription" :: NullOrUndefined (String) }
```

<p>Provides all of the details about a particular cache engine version.</p>

##### Instances
``` purescript
Newtype CacheEngineVersion _
```

#### `CacheEngineVersionList`

``` purescript
newtype CacheEngineVersionList
  = CacheEngineVersionList (Array CacheEngineVersion)
```

##### Instances
``` purescript
Newtype CacheEngineVersionList _
```

#### `CacheEngineVersionMessage`

``` purescript
newtype CacheEngineVersionMessage
  = CacheEngineVersionMessage { "Marker" :: NullOrUndefined (String), "CacheEngineVersions" :: NullOrUndefined (CacheEngineVersionList) }
```

<p>Represents the output of a <a>DescribeCacheEngineVersions</a> operation.</p>

##### Instances
``` purescript
Newtype CacheEngineVersionMessage _
```

#### `CacheNode`

``` purescript
newtype CacheNode
  = CacheNode { "CacheNodeId" :: NullOrUndefined (String), "CacheNodeStatus" :: NullOrUndefined (String), "CacheNodeCreateTime" :: NullOrUndefined (TStamp), "Endpoint" :: NullOrUndefined (Endpoint), "ParameterGroupStatus" :: NullOrUndefined (String), "SourceCacheNodeId" :: NullOrUndefined (String), "CustomerAvailabilityZone" :: NullOrUndefined (String) }
```

<p>Represents an individual cache node within a cluster. Each cache node runs its own instance of the cluster's protocol-compliant caching software - either Memcached or Redis.</p> <p>The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.</p> <ul> <li> <p>General purpose:</p> <ul> <li> <p>Current generation: </p> <p> <b>T2 node types:</b> <code>cache.t2.micro</code>, <code>cache.t2.small</code>, <code>cache.t2.medium</code> </p> <p> <b>M3 node types:</b> <code>cache.m3.medium</code>, <code>cache.m3.large</code>, <code>cache.m3.xlarge</code>, <code>cache.m3.2xlarge</code> </p> <p> <b>M4 node types:</b> <code>cache.m4.large</code>, <code>cache.m4.xlarge</code>, <code>cache.m4.2xlarge</code>, <code>cache.m4.4xlarge</code>, <code>cache.m4.10xlarge</code> </p> </li> <li> <p>Previous generation: (not recommended)</p> <p> <b>T1 node types:</b> <code>cache.t1.micro</code> </p> <p> <b>M1 node types:</b> <code>cache.m1.small</code>, <code>cache.m1.medium</code>, <code>cache.m1.large</code>, <code>cache.m1.xlarge</code> </p> </li> </ul> </li> <li> <p>Compute optimized:</p> <ul> <li> <p>Previous generation: (not recommended)</p> <p> <b>C1 node types:</b> <code>cache.c1.xlarge</code> </p> </li> </ul> </li> <li> <p>Memory optimized:</p> <ul> <li> <p>Current generation: </p> <p> <b>R3 node types:</b> <code>cache.r3.large</code>, <code>cache.r3.xlarge</code>, <code>cache.r3.2xlarge</code>, <code>cache.r3.4xlarge</code>, <code>cache.r3.8xlarge</code> </p> </li> <li> <p>Previous generation: (not recommended)</p> <p> <b>M2 node types:</b> <code>cache.m2.xlarge</code>, <code>cache.m2.2xlarge</code>, <code>cache.m2.4xlarge</code> </p> </li> </ul> </li> </ul> <p> <b>Notes:</b> </p> <ul> <li> <p>All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).</p> </li> <li> <p>Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances. </p> </li> <li> <p>Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.</p> </li> <li> <p>Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances.</p> </li> </ul> <p>For a complete listing of node types and specifications, see <a href="http://aws.amazon.com/elasticache/details">Amazon ElastiCache Product Features and Details</a> and either <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific">Cache Node Type-Specific Parameters for Memcached</a> or <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific">Cache Node Type-Specific Parameters for Redis</a>.</p>

##### Instances
``` purescript
Newtype CacheNode _
```

#### `CacheNodeIdsList`

``` purescript
newtype CacheNodeIdsList
  = CacheNodeIdsList (Array String)
```

##### Instances
``` purescript
Newtype CacheNodeIdsList _
```

#### `CacheNodeList`

``` purescript
newtype CacheNodeList
  = CacheNodeList (Array CacheNode)
```

##### Instances
``` purescript
Newtype CacheNodeList _
```

#### `CacheNodeTypeSpecificParameter`

``` purescript
newtype CacheNodeTypeSpecificParameter
  = CacheNodeTypeSpecificParameter { "ParameterName" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Source" :: NullOrUndefined (String), "DataType" :: NullOrUndefined (String), "AllowedValues" :: NullOrUndefined (String), "IsModifiable" :: NullOrUndefined (Boolean), "MinimumEngineVersion" :: NullOrUndefined (String), "CacheNodeTypeSpecificValues" :: NullOrUndefined (CacheNodeTypeSpecificValueList), "ChangeType" :: NullOrUndefined (ChangeType) }
```

<p>A parameter that has a different value for each cache node type it is applied to. For example, in a Redis cluster, a <code>cache.m1.large</code> cache node type would have a larger <code>maxmemory</code> value than a <code>cache.m1.small</code> type.</p>

##### Instances
``` purescript
Newtype CacheNodeTypeSpecificParameter _
```

#### `CacheNodeTypeSpecificParametersList`

``` purescript
newtype CacheNodeTypeSpecificParametersList
  = CacheNodeTypeSpecificParametersList (Array CacheNodeTypeSpecificParameter)
```

##### Instances
``` purescript
Newtype CacheNodeTypeSpecificParametersList _
```

#### `CacheNodeTypeSpecificValue`

``` purescript
newtype CacheNodeTypeSpecificValue
  = CacheNodeTypeSpecificValue { "CacheNodeType" :: NullOrUndefined (String), "Value" :: NullOrUndefined (String) }
```

<p>A value that applies only to a certain cache node type.</p>

##### Instances
``` purescript
Newtype CacheNodeTypeSpecificValue _
```

#### `CacheNodeTypeSpecificValueList`

``` purescript
newtype CacheNodeTypeSpecificValueList
  = CacheNodeTypeSpecificValueList (Array CacheNodeTypeSpecificValue)
```

##### Instances
``` purescript
Newtype CacheNodeTypeSpecificValueList _
```

#### `CacheParameterGroup`

``` purescript
newtype CacheParameterGroup
  = CacheParameterGroup { "CacheParameterGroupName" :: NullOrUndefined (String), "CacheParameterGroupFamily" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

<p>Represents the output of a <code>CreateCacheParameterGroup</code> operation.</p>

##### Instances
``` purescript
Newtype CacheParameterGroup _
```

#### `CacheParameterGroupAlreadyExistsFault`

``` purescript
newtype CacheParameterGroupAlreadyExistsFault
  = CacheParameterGroupAlreadyExistsFault {  }
```

<p>A cache parameter group with the requested name already exists.</p>

##### Instances
``` purescript
Newtype CacheParameterGroupAlreadyExistsFault _
```

#### `CacheParameterGroupDetails`

``` purescript
newtype CacheParameterGroupDetails
  = CacheParameterGroupDetails { "Marker" :: NullOrUndefined (String), "Parameters" :: NullOrUndefined (ParametersList), "CacheNodeTypeSpecificParameters" :: NullOrUndefined (CacheNodeTypeSpecificParametersList) }
```

<p>Represents the output of a <code>DescribeCacheParameters</code> operation.</p>

##### Instances
``` purescript
Newtype CacheParameterGroupDetails _
```

#### `CacheParameterGroupList`

``` purescript
newtype CacheParameterGroupList
  = CacheParameterGroupList (Array CacheParameterGroup)
```

##### Instances
``` purescript
Newtype CacheParameterGroupList _
```

#### `CacheParameterGroupNameMessage`

``` purescript
newtype CacheParameterGroupNameMessage
  = CacheParameterGroupNameMessage { "CacheParameterGroupName" :: NullOrUndefined (String) }
```

<p>Represents the output of one of the following operations:</p> <ul> <li> <p> <code>ModifyCacheParameterGroup</code> </p> </li> <li> <p> <code>ResetCacheParameterGroup</code> </p> </li> </ul>

##### Instances
``` purescript
Newtype CacheParameterGroupNameMessage _
```

#### `CacheParameterGroupNotFoundFault`

``` purescript
newtype CacheParameterGroupNotFoundFault
  = CacheParameterGroupNotFoundFault {  }
```

<p>The requested cache parameter group name does not refer to an existing cache parameter group.</p>

##### Instances
``` purescript
Newtype CacheParameterGroupNotFoundFault _
```

#### `CacheParameterGroupQuotaExceededFault`

``` purescript
newtype CacheParameterGroupQuotaExceededFault
  = CacheParameterGroupQuotaExceededFault {  }
```

<p>The request cannot be processed because it would exceed the maximum number of cache security groups.</p>

##### Instances
``` purescript
Newtype CacheParameterGroupQuotaExceededFault _
```

#### `CacheParameterGroupStatus`

``` purescript
newtype CacheParameterGroupStatus
  = CacheParameterGroupStatus { "CacheParameterGroupName" :: NullOrUndefined (String), "ParameterApplyStatus" :: NullOrUndefined (String), "CacheNodeIdsToReboot" :: NullOrUndefined (CacheNodeIdsList) }
```

<p>Status of the cache parameter group.</p>

##### Instances
``` purescript
Newtype CacheParameterGroupStatus _
```

#### `CacheParameterGroupsMessage`

``` purescript
newtype CacheParameterGroupsMessage
  = CacheParameterGroupsMessage { "Marker" :: NullOrUndefined (String), "CacheParameterGroups" :: NullOrUndefined (CacheParameterGroupList) }
```

<p>Represents the output of a <code>DescribeCacheParameterGroups</code> operation.</p>

##### Instances
``` purescript
Newtype CacheParameterGroupsMessage _
```

#### `CacheSecurityGroup`

``` purescript
newtype CacheSecurityGroup
  = CacheSecurityGroup { "OwnerId" :: NullOrUndefined (String), "CacheSecurityGroupName" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "EC2SecurityGroups" :: NullOrUndefined (EC2SecurityGroupList) }
```

<p>Represents the output of one of the following operations:</p> <ul> <li> <p> <code>AuthorizeCacheSecurityGroupIngress</code> </p> </li> <li> <p> <code>CreateCacheSecurityGroup</code> </p> </li> <li> <p> <code>RevokeCacheSecurityGroupIngress</code> </p> </li> </ul>

##### Instances
``` purescript
Newtype CacheSecurityGroup _
```

#### `CacheSecurityGroupAlreadyExistsFault`

``` purescript
newtype CacheSecurityGroupAlreadyExistsFault
  = CacheSecurityGroupAlreadyExistsFault {  }
```

<p>A cache security group with the specified name already exists.</p>

##### Instances
``` purescript
Newtype CacheSecurityGroupAlreadyExistsFault _
```

#### `CacheSecurityGroupMembership`

``` purescript
newtype CacheSecurityGroupMembership
  = CacheSecurityGroupMembership { "CacheSecurityGroupName" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String) }
```

<p>Represents a cluster's status within a particular cache security group.</p>

##### Instances
``` purescript
Newtype CacheSecurityGroupMembership _
```

#### `CacheSecurityGroupMembershipList`

``` purescript
newtype CacheSecurityGroupMembershipList
  = CacheSecurityGroupMembershipList (Array CacheSecurityGroupMembership)
```

##### Instances
``` purescript
Newtype CacheSecurityGroupMembershipList _
```

#### `CacheSecurityGroupMessage`

``` purescript
newtype CacheSecurityGroupMessage
  = CacheSecurityGroupMessage { "Marker" :: NullOrUndefined (String), "CacheSecurityGroups" :: NullOrUndefined (CacheSecurityGroups) }
```

<p>Represents the output of a <code>DescribeCacheSecurityGroups</code> operation.</p>

##### Instances
``` purescript
Newtype CacheSecurityGroupMessage _
```

#### `CacheSecurityGroupNameList`

``` purescript
newtype CacheSecurityGroupNameList
  = CacheSecurityGroupNameList (Array String)
```

##### Instances
``` purescript
Newtype CacheSecurityGroupNameList _
```

#### `CacheSecurityGroupNotFoundFault`

``` purescript
newtype CacheSecurityGroupNotFoundFault
  = CacheSecurityGroupNotFoundFault {  }
```

<p>The requested cache security group name does not refer to an existing cache security group.</p>

##### Instances
``` purescript
Newtype CacheSecurityGroupNotFoundFault _
```

#### `CacheSecurityGroupQuotaExceededFault`

``` purescript
newtype CacheSecurityGroupQuotaExceededFault
  = CacheSecurityGroupQuotaExceededFault {  }
```

<p>The request cannot be processed because it would exceed the allowed number of cache security groups.</p>

##### Instances
``` purescript
Newtype CacheSecurityGroupQuotaExceededFault _
```

#### `CacheSecurityGroups`

``` purescript
newtype CacheSecurityGroups
  = CacheSecurityGroups (Array CacheSecurityGroup)
```

##### Instances
``` purescript
Newtype CacheSecurityGroups _
```

#### `CacheSubnetGroup`

``` purescript
newtype CacheSubnetGroup
  = CacheSubnetGroup { "CacheSubnetGroupName" :: NullOrUndefined (String), "CacheSubnetGroupDescription" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "Subnets" :: NullOrUndefined (SubnetList) }
```

<p>Represents the output of one of the following operations:</p> <ul> <li> <p> <code>CreateCacheSubnetGroup</code> </p> </li> <li> <p> <code>ModifyCacheSubnetGroup</code> </p> </li> </ul>

##### Instances
``` purescript
Newtype CacheSubnetGroup _
```

#### `CacheSubnetGroupAlreadyExistsFault`

``` purescript
newtype CacheSubnetGroupAlreadyExistsFault
  = CacheSubnetGroupAlreadyExistsFault {  }
```

<p>The requested cache subnet group name is already in use by an existing cache subnet group.</p>

##### Instances
``` purescript
Newtype CacheSubnetGroupAlreadyExistsFault _
```

#### `CacheSubnetGroupInUse`

``` purescript
newtype CacheSubnetGroupInUse
  = CacheSubnetGroupInUse {  }
```

<p>The requested cache subnet group is currently in use.</p>

##### Instances
``` purescript
Newtype CacheSubnetGroupInUse _
```

#### `CacheSubnetGroupMessage`

``` purescript
newtype CacheSubnetGroupMessage
  = CacheSubnetGroupMessage { "Marker" :: NullOrUndefined (String), "CacheSubnetGroups" :: NullOrUndefined (CacheSubnetGroups) }
```

<p>Represents the output of a <code>DescribeCacheSubnetGroups</code> operation.</p>

##### Instances
``` purescript
Newtype CacheSubnetGroupMessage _
```

#### `CacheSubnetGroupNotFoundFault`

``` purescript
newtype CacheSubnetGroupNotFoundFault
  = CacheSubnetGroupNotFoundFault {  }
```

<p>The requested cache subnet group name does not refer to an existing cache subnet group.</p>

##### Instances
``` purescript
Newtype CacheSubnetGroupNotFoundFault _
```

#### `CacheSubnetGroupQuotaExceededFault`

``` purescript
newtype CacheSubnetGroupQuotaExceededFault
  = CacheSubnetGroupQuotaExceededFault {  }
```

<p>The request cannot be processed because it would exceed the allowed number of cache subnet groups.</p>

##### Instances
``` purescript
Newtype CacheSubnetGroupQuotaExceededFault _
```

#### `CacheSubnetGroups`

``` purescript
newtype CacheSubnetGroups
  = CacheSubnetGroups (Array CacheSubnetGroup)
```

##### Instances
``` purescript
Newtype CacheSubnetGroups _
```

#### `CacheSubnetQuotaExceededFault`

``` purescript
newtype CacheSubnetQuotaExceededFault
  = CacheSubnetQuotaExceededFault {  }
```

<p>The request cannot be processed because it would exceed the allowed number of subnets in a cache subnet group.</p>

##### Instances
``` purescript
Newtype CacheSubnetQuotaExceededFault _
```

#### `ChangeType`

``` purescript
newtype ChangeType
  = ChangeType String
```

##### Instances
``` purescript
Newtype ChangeType _
```

#### `ClusterIdList`

``` purescript
newtype ClusterIdList
  = ClusterIdList (Array String)
```

##### Instances
``` purescript
Newtype ClusterIdList _
```

#### `ClusterQuotaForCustomerExceededFault`

``` purescript
newtype ClusterQuotaForCustomerExceededFault
  = ClusterQuotaForCustomerExceededFault {  }
```

<p>The request cannot be processed because it would exceed the allowed number of clusters per customer.</p>

##### Instances
``` purescript
Newtype ClusterQuotaForCustomerExceededFault _
```

#### `CopySnapshotMessage`

``` purescript
newtype CopySnapshotMessage
  = CopySnapshotMessage { "SourceSnapshotName" :: String, "TargetSnapshotName" :: String, "TargetBucket" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>CopySnapshotMessage</code> operation.</p>

##### Instances
``` purescript
Newtype CopySnapshotMessage _
```

#### `CopySnapshotResult`

``` purescript
newtype CopySnapshotResult
  = CopySnapshotResult { "Snapshot" :: NullOrUndefined (Snapshot) }
```

##### Instances
``` purescript
Newtype CopySnapshotResult _
```

#### `CreateCacheClusterMessage`

``` purescript
newtype CreateCacheClusterMessage
  = CreateCacheClusterMessage { "CacheClusterId" :: String, "ReplicationGroupId" :: NullOrUndefined (String), "AZMode" :: NullOrUndefined (AZMode), "PreferredAvailabilityZone" :: NullOrUndefined (String), "PreferredAvailabilityZones" :: NullOrUndefined (PreferredAvailabilityZoneList), "NumCacheNodes" :: NullOrUndefined (IntegerOptional), "CacheNodeType" :: NullOrUndefined (String), "Engine" :: NullOrUndefined (String), "EngineVersion" :: NullOrUndefined (String), "CacheParameterGroupName" :: NullOrUndefined (String), "CacheSubnetGroupName" :: NullOrUndefined (String), "CacheSecurityGroupNames" :: NullOrUndefined (CacheSecurityGroupNameList), "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdsList), "Tags" :: NullOrUndefined (TagList), "SnapshotArns" :: NullOrUndefined (SnapshotArnsList), "SnapshotName" :: NullOrUndefined (String), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "NotificationTopicArn" :: NullOrUndefined (String), "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional), "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional), "SnapshotWindow" :: NullOrUndefined (String), "AuthToken" :: NullOrUndefined (String) }
```

<p>Represents the input of a CreateCacheCluster operation.</p>

##### Instances
``` purescript
Newtype CreateCacheClusterMessage _
```

#### `CreateCacheClusterResult`

``` purescript
newtype CreateCacheClusterResult
  = CreateCacheClusterResult { "CacheCluster" :: NullOrUndefined (CacheCluster) }
```

##### Instances
``` purescript
Newtype CreateCacheClusterResult _
```

#### `CreateCacheParameterGroupMessage`

``` purescript
newtype CreateCacheParameterGroupMessage
  = CreateCacheParameterGroupMessage { "CacheParameterGroupName" :: String, "CacheParameterGroupFamily" :: String, "Description" :: String }
```

<p>Represents the input of a <code>CreateCacheParameterGroup</code> operation.</p>

##### Instances
``` purescript
Newtype CreateCacheParameterGroupMessage _
```

#### `CreateCacheParameterGroupResult`

``` purescript
newtype CreateCacheParameterGroupResult
  = CreateCacheParameterGroupResult { "CacheParameterGroup" :: NullOrUndefined (CacheParameterGroup) }
```

##### Instances
``` purescript
Newtype CreateCacheParameterGroupResult _
```

#### `CreateCacheSecurityGroupMessage`

``` purescript
newtype CreateCacheSecurityGroupMessage
  = CreateCacheSecurityGroupMessage { "CacheSecurityGroupName" :: String, "Description" :: String }
```

<p>Represents the input of a <code>CreateCacheSecurityGroup</code> operation.</p>

##### Instances
``` purescript
Newtype CreateCacheSecurityGroupMessage _
```

#### `CreateCacheSecurityGroupResult`

``` purescript
newtype CreateCacheSecurityGroupResult
  = CreateCacheSecurityGroupResult { "CacheSecurityGroup" :: NullOrUndefined (CacheSecurityGroup) }
```

##### Instances
``` purescript
Newtype CreateCacheSecurityGroupResult _
```

#### `CreateCacheSubnetGroupMessage`

``` purescript
newtype CreateCacheSubnetGroupMessage
  = CreateCacheSubnetGroupMessage { "CacheSubnetGroupName" :: String, "CacheSubnetGroupDescription" :: String, "SubnetIds" :: SubnetIdentifierList }
```

<p>Represents the input of a <code>CreateCacheSubnetGroup</code> operation.</p>

##### Instances
``` purescript
Newtype CreateCacheSubnetGroupMessage _
```

#### `CreateCacheSubnetGroupResult`

``` purescript
newtype CreateCacheSubnetGroupResult
  = CreateCacheSubnetGroupResult { "CacheSubnetGroup" :: NullOrUndefined (CacheSubnetGroup) }
```

##### Instances
``` purescript
Newtype CreateCacheSubnetGroupResult _
```

#### `CreateReplicationGroupMessage`

``` purescript
newtype CreateReplicationGroupMessage
  = CreateReplicationGroupMessage { "ReplicationGroupId" :: String, "ReplicationGroupDescription" :: String, "PrimaryClusterId" :: NullOrUndefined (String), "AutomaticFailoverEnabled" :: NullOrUndefined (BooleanOptional), "NumCacheClusters" :: NullOrUndefined (IntegerOptional), "PreferredCacheClusterAZs" :: NullOrUndefined (AvailabilityZonesList), "NumNodeGroups" :: NullOrUndefined (IntegerOptional), "ReplicasPerNodeGroup" :: NullOrUndefined (IntegerOptional), "NodeGroupConfiguration" :: NullOrUndefined (NodeGroupConfigurationList), "CacheNodeType" :: NullOrUndefined (String), "Engine" :: NullOrUndefined (String), "EngineVersion" :: NullOrUndefined (String), "CacheParameterGroupName" :: NullOrUndefined (String), "CacheSubnetGroupName" :: NullOrUndefined (String), "CacheSecurityGroupNames" :: NullOrUndefined (CacheSecurityGroupNameList), "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdsList), "Tags" :: NullOrUndefined (TagList), "SnapshotArns" :: NullOrUndefined (SnapshotArnsList), "SnapshotName" :: NullOrUndefined (String), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "NotificationTopicArn" :: NullOrUndefined (String), "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional), "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional), "SnapshotWindow" :: NullOrUndefined (String), "AuthToken" :: NullOrUndefined (String), "TransitEncryptionEnabled" :: NullOrUndefined (BooleanOptional), "AtRestEncryptionEnabled" :: NullOrUndefined (BooleanOptional) }
```

<p>Represents the input of a <code>CreateReplicationGroup</code> operation.</p>

##### Instances
``` purescript
Newtype CreateReplicationGroupMessage _
```

#### `CreateReplicationGroupResult`

``` purescript
newtype CreateReplicationGroupResult
  = CreateReplicationGroupResult { "ReplicationGroup" :: NullOrUndefined (ReplicationGroup) }
```

##### Instances
``` purescript
Newtype CreateReplicationGroupResult _
```

#### `CreateSnapshotMessage`

``` purescript
newtype CreateSnapshotMessage
  = CreateSnapshotMessage { "ReplicationGroupId" :: NullOrUndefined (String), "CacheClusterId" :: NullOrUndefined (String), "SnapshotName" :: String }
```

<p>Represents the input of a <code>CreateSnapshot</code> operation.</p>

##### Instances
``` purescript
Newtype CreateSnapshotMessage _
```

#### `CreateSnapshotResult`

``` purescript
newtype CreateSnapshotResult
  = CreateSnapshotResult { "Snapshot" :: NullOrUndefined (Snapshot) }
```

##### Instances
``` purescript
Newtype CreateSnapshotResult _
```

#### `DeleteCacheClusterMessage`

``` purescript
newtype DeleteCacheClusterMessage
  = DeleteCacheClusterMessage { "CacheClusterId" :: String, "FinalSnapshotIdentifier" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>DeleteCacheCluster</code> operation.</p>

##### Instances
``` purescript
Newtype DeleteCacheClusterMessage _
```

#### `DeleteCacheClusterResult`

``` purescript
newtype DeleteCacheClusterResult
  = DeleteCacheClusterResult { "CacheCluster" :: NullOrUndefined (CacheCluster) }
```

##### Instances
``` purescript
Newtype DeleteCacheClusterResult _
```

#### `DeleteCacheParameterGroupMessage`

``` purescript
newtype DeleteCacheParameterGroupMessage
  = DeleteCacheParameterGroupMessage { "CacheParameterGroupName" :: String }
```

<p>Represents the input of a <code>DeleteCacheParameterGroup</code> operation.</p>

##### Instances
``` purescript
Newtype DeleteCacheParameterGroupMessage _
```

#### `DeleteCacheSecurityGroupMessage`

``` purescript
newtype DeleteCacheSecurityGroupMessage
  = DeleteCacheSecurityGroupMessage { "CacheSecurityGroupName" :: String }
```

<p>Represents the input of a <code>DeleteCacheSecurityGroup</code> operation.</p>

##### Instances
``` purescript
Newtype DeleteCacheSecurityGroupMessage _
```

#### `DeleteCacheSubnetGroupMessage`

``` purescript
newtype DeleteCacheSubnetGroupMessage
  = DeleteCacheSubnetGroupMessage { "CacheSubnetGroupName" :: String }
```

<p>Represents the input of a <code>DeleteCacheSubnetGroup</code> operation.</p>

##### Instances
``` purescript
Newtype DeleteCacheSubnetGroupMessage _
```

#### `DeleteReplicationGroupMessage`

``` purescript
newtype DeleteReplicationGroupMessage
  = DeleteReplicationGroupMessage { "ReplicationGroupId" :: String, "RetainPrimaryCluster" :: NullOrUndefined (BooleanOptional), "FinalSnapshotIdentifier" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>DeleteReplicationGroup</code> operation.</p>

##### Instances
``` purescript
Newtype DeleteReplicationGroupMessage _
```

#### `DeleteReplicationGroupResult`

``` purescript
newtype DeleteReplicationGroupResult
  = DeleteReplicationGroupResult { "ReplicationGroup" :: NullOrUndefined (ReplicationGroup) }
```

##### Instances
``` purescript
Newtype DeleteReplicationGroupResult _
```

#### `DeleteSnapshotMessage`

``` purescript
newtype DeleteSnapshotMessage
  = DeleteSnapshotMessage { "SnapshotName" :: String }
```

<p>Represents the input of a <code>DeleteSnapshot</code> operation.</p>

##### Instances
``` purescript
Newtype DeleteSnapshotMessage _
```

#### `DeleteSnapshotResult`

``` purescript
newtype DeleteSnapshotResult
  = DeleteSnapshotResult { "Snapshot" :: NullOrUndefined (Snapshot) }
```

##### Instances
``` purescript
Newtype DeleteSnapshotResult _
```

#### `DescribeCacheClustersMessage`

``` purescript
newtype DescribeCacheClustersMessage
  = DescribeCacheClustersMessage { "CacheClusterId" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "ShowCacheNodeInfo" :: NullOrUndefined (BooleanOptional), "ShowCacheClustersNotInReplicationGroups" :: NullOrUndefined (BooleanOptional) }
```

<p>Represents the input of a <code>DescribeCacheClusters</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeCacheClustersMessage _
```

#### `DescribeCacheEngineVersionsMessage`

``` purescript
newtype DescribeCacheEngineVersionsMessage
  = DescribeCacheEngineVersionsMessage { "Engine" :: NullOrUndefined (String), "EngineVersion" :: NullOrUndefined (String), "CacheParameterGroupFamily" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "DefaultOnly" :: NullOrUndefined (Boolean) }
```

<p>Represents the input of a <code>DescribeCacheEngineVersions</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeCacheEngineVersionsMessage _
```

#### `DescribeCacheParameterGroupsMessage`

``` purescript
newtype DescribeCacheParameterGroupsMessage
  = DescribeCacheParameterGroupsMessage { "CacheParameterGroupName" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>DescribeCacheParameterGroups</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeCacheParameterGroupsMessage _
```

#### `DescribeCacheParametersMessage`

``` purescript
newtype DescribeCacheParametersMessage
  = DescribeCacheParametersMessage { "CacheParameterGroupName" :: String, "Source" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>DescribeCacheParameters</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeCacheParametersMessage _
```

#### `DescribeCacheSecurityGroupsMessage`

``` purescript
newtype DescribeCacheSecurityGroupsMessage
  = DescribeCacheSecurityGroupsMessage { "CacheSecurityGroupName" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>DescribeCacheSecurityGroups</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeCacheSecurityGroupsMessage _
```

#### `DescribeCacheSubnetGroupsMessage`

``` purescript
newtype DescribeCacheSubnetGroupsMessage
  = DescribeCacheSubnetGroupsMessage { "CacheSubnetGroupName" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>DescribeCacheSubnetGroups</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeCacheSubnetGroupsMessage _
```

#### `DescribeEngineDefaultParametersMessage`

``` purescript
newtype DescribeEngineDefaultParametersMessage
  = DescribeEngineDefaultParametersMessage { "CacheParameterGroupFamily" :: String, "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>DescribeEngineDefaultParameters</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeEngineDefaultParametersMessage _
```

#### `DescribeEngineDefaultParametersResult`

``` purescript
newtype DescribeEngineDefaultParametersResult
  = DescribeEngineDefaultParametersResult { "EngineDefaults" :: NullOrUndefined (EngineDefaults) }
```

##### Instances
``` purescript
Newtype DescribeEngineDefaultParametersResult _
```

#### `DescribeEventsMessage`

``` purescript
newtype DescribeEventsMessage
  = DescribeEventsMessage { "SourceIdentifier" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (SourceType), "StartTime" :: NullOrUndefined (TStamp), "EndTime" :: NullOrUndefined (TStamp), "Duration" :: NullOrUndefined (IntegerOptional), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>DescribeEvents</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeEventsMessage _
```

#### `DescribeReplicationGroupsMessage`

``` purescript
newtype DescribeReplicationGroupsMessage
  = DescribeReplicationGroupsMessage { "ReplicationGroupId" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>DescribeReplicationGroups</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeReplicationGroupsMessage _
```

#### `DescribeReservedCacheNodesMessage`

``` purescript
newtype DescribeReservedCacheNodesMessage
  = DescribeReservedCacheNodesMessage { "ReservedCacheNodeId" :: NullOrUndefined (String), "ReservedCacheNodesOfferingId" :: NullOrUndefined (String), "CacheNodeType" :: NullOrUndefined (String), "Duration" :: NullOrUndefined (String), "ProductDescription" :: NullOrUndefined (String), "OfferingType" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>DescribeReservedCacheNodes</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeReservedCacheNodesMessage _
```

#### `DescribeReservedCacheNodesOfferingsMessage`

``` purescript
newtype DescribeReservedCacheNodesOfferingsMessage
  = DescribeReservedCacheNodesOfferingsMessage { "ReservedCacheNodesOfferingId" :: NullOrUndefined (String), "CacheNodeType" :: NullOrUndefined (String), "Duration" :: NullOrUndefined (String), "ProductDescription" :: NullOrUndefined (String), "OfferingType" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>DescribeReservedCacheNodesOfferings</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeReservedCacheNodesOfferingsMessage _
```

#### `DescribeSnapshotsListMessage`

``` purescript
newtype DescribeSnapshotsListMessage
  = DescribeSnapshotsListMessage { "Marker" :: NullOrUndefined (String), "Snapshots" :: NullOrUndefined (SnapshotList) }
```

<p>Represents the output of a <code>DescribeSnapshots</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeSnapshotsListMessage _
```

#### `DescribeSnapshotsMessage`

``` purescript
newtype DescribeSnapshotsMessage
  = DescribeSnapshotsMessage { "ReplicationGroupId" :: NullOrUndefined (String), "CacheClusterId" :: NullOrUndefined (String), "SnapshotName" :: NullOrUndefined (String), "SnapshotSource" :: NullOrUndefined (String), "Marker" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "ShowNodeGroupConfig" :: NullOrUndefined (BooleanOptional) }
```

<p>Represents the input of a <code>DescribeSnapshotsMessage</code> operation.</p>

##### Instances
``` purescript
Newtype DescribeSnapshotsMessage _
```

#### `EC2SecurityGroup`

``` purescript
newtype EC2SecurityGroup
  = EC2SecurityGroup { "Status" :: NullOrUndefined (String), "EC2SecurityGroupName" :: NullOrUndefined (String), "EC2SecurityGroupOwnerId" :: NullOrUndefined (String) }
```

<p>Provides ownership and status information for an Amazon EC2 security group.</p>

##### Instances
``` purescript
Newtype EC2SecurityGroup _
```

#### `EC2SecurityGroupList`

``` purescript
newtype EC2SecurityGroupList
  = EC2SecurityGroupList (Array EC2SecurityGroup)
```

##### Instances
``` purescript
Newtype EC2SecurityGroupList _
```

#### `Endpoint`

``` purescript
newtype Endpoint
  = Endpoint { "Address" :: NullOrUndefined (String), "Port" :: NullOrUndefined (Int) }
```

<p>Represents the information required for client programs to connect to a cache node.</p>

##### Instances
``` purescript
Newtype Endpoint _
```

#### `EngineDefaults`

``` purescript
newtype EngineDefaults
  = EngineDefaults { "CacheParameterGroupFamily" :: NullOrUndefined (String), "Marker" :: NullOrUndefined (String), "Parameters" :: NullOrUndefined (ParametersList), "CacheNodeTypeSpecificParameters" :: NullOrUndefined (CacheNodeTypeSpecificParametersList) }
```

<p>Represents the output of a <code>DescribeEngineDefaultParameters</code> operation.</p>

##### Instances
``` purescript
Newtype EngineDefaults _
```

#### `Event`

``` purescript
newtype Event
  = Event { "SourceIdentifier" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (SourceType), "Message" :: NullOrUndefined (String), "Date" :: NullOrUndefined (TStamp) }
```

<p>Represents a single occurrence of something interesting within the system. Some examples of events are creating a cluster, adding or removing a cache node, or rebooting a node.</p>

##### Instances
``` purescript
Newtype Event _
```

#### `EventList`

``` purescript
newtype EventList
  = EventList (Array Event)
```

##### Instances
``` purescript
Newtype EventList _
```

#### `EventsMessage`

``` purescript
newtype EventsMessage
  = EventsMessage { "Marker" :: NullOrUndefined (String), "Events" :: NullOrUndefined (EventList) }
```

<p>Represents the output of a <code>DescribeEvents</code> operation.</p>

##### Instances
``` purescript
Newtype EventsMessage _
```

#### `InsufficientCacheClusterCapacityFault`

``` purescript
newtype InsufficientCacheClusterCapacityFault
  = InsufficientCacheClusterCapacityFault {  }
```

<p>The requested cache node type is not available in the specified Availability Zone.</p>

##### Instances
``` purescript
Newtype InsufficientCacheClusterCapacityFault _
```

#### `IntegerOptional`

``` purescript
newtype IntegerOptional
  = IntegerOptional Int
```

##### Instances
``` purescript
Newtype IntegerOptional _
```

#### `InvalidARNFault`

``` purescript
newtype InvalidARNFault
  = InvalidARNFault {  }
```

<p>The requested Amazon Resource Name (ARN) does not refer to an existing resource.</p>

##### Instances
``` purescript
Newtype InvalidARNFault _
```

#### `InvalidCacheClusterStateFault`

``` purescript
newtype InvalidCacheClusterStateFault
  = InvalidCacheClusterStateFault {  }
```

<p>The requested cluster is not in the <code>available</code> state.</p>

##### Instances
``` purescript
Newtype InvalidCacheClusterStateFault _
```

#### `InvalidCacheParameterGroupStateFault`

``` purescript
newtype InvalidCacheParameterGroupStateFault
  = InvalidCacheParameterGroupStateFault {  }
```

<p>The current state of the cache parameter group does not allow the requested operation to occur.</p>

##### Instances
``` purescript
Newtype InvalidCacheParameterGroupStateFault _
```

#### `InvalidCacheSecurityGroupStateFault`

``` purescript
newtype InvalidCacheSecurityGroupStateFault
  = InvalidCacheSecurityGroupStateFault {  }
```

<p>The current state of the cache security group does not allow deletion.</p>

##### Instances
``` purescript
Newtype InvalidCacheSecurityGroupStateFault _
```

#### `InvalidParameterCombinationException`

``` purescript
newtype InvalidParameterCombinationException
  = InvalidParameterCombinationException { "Message'" :: NullOrUndefined (AwsQueryErrorMessage) }
```

<p>Two or more incompatible parameters were specified.</p>

##### Instances
``` purescript
Newtype InvalidParameterCombinationException _
```

#### `InvalidParameterValueException`

``` purescript
newtype InvalidParameterValueException
  = InvalidParameterValueException { "Message'" :: NullOrUndefined (AwsQueryErrorMessage) }
```

<p>The value for a parameter is invalid.</p>

##### Instances
``` purescript
Newtype InvalidParameterValueException _
```

#### `InvalidReplicationGroupStateFault`

``` purescript
newtype InvalidReplicationGroupStateFault
  = InvalidReplicationGroupStateFault {  }
```

<p>The requested replication group is not in the <code>available</code> state.</p>

##### Instances
``` purescript
Newtype InvalidReplicationGroupStateFault _
```

#### `InvalidSnapshotStateFault`

``` purescript
newtype InvalidSnapshotStateFault
  = InvalidSnapshotStateFault {  }
```

<p>The current state of the snapshot does not allow the requested operation to occur.</p>

##### Instances
``` purescript
Newtype InvalidSnapshotStateFault _
```

#### `InvalidSubnet`

``` purescript
newtype InvalidSubnet
  = InvalidSubnet {  }
```

<p>An invalid subnet identifier was specified.</p>

##### Instances
``` purescript
Newtype InvalidSubnet _
```

#### `InvalidVPCNetworkStateFault`

``` purescript
newtype InvalidVPCNetworkStateFault
  = InvalidVPCNetworkStateFault {  }
```

<p>The VPC network is in an invalid state.</p>

##### Instances
``` purescript
Newtype InvalidVPCNetworkStateFault _
```

#### `KeyList`

``` purescript
newtype KeyList
  = KeyList (Array String)
```

##### Instances
``` purescript
Newtype KeyList _
```

#### `ListAllowedNodeTypeModificationsMessage`

``` purescript
newtype ListAllowedNodeTypeModificationsMessage
  = ListAllowedNodeTypeModificationsMessage { "CacheClusterId" :: NullOrUndefined (String), "ReplicationGroupId" :: NullOrUndefined (String) }
```

<p>The input parameters for the <code>ListAllowedNodeTypeModifications</code> operation.</p>

##### Instances
``` purescript
Newtype ListAllowedNodeTypeModificationsMessage _
```

#### `ListTagsForResourceMessage`

``` purescript
newtype ListTagsForResourceMessage
  = ListTagsForResourceMessage { "ResourceName" :: String }
```

<p>The input parameters for the <code>ListTagsForResource</code> operation.</p>

##### Instances
``` purescript
Newtype ListTagsForResourceMessage _
```

#### `ModifyCacheClusterMessage`

``` purescript
newtype ModifyCacheClusterMessage
  = ModifyCacheClusterMessage { "CacheClusterId" :: String, "NumCacheNodes" :: NullOrUndefined (IntegerOptional), "CacheNodeIdsToRemove" :: NullOrUndefined (CacheNodeIdsList), "AZMode" :: NullOrUndefined (AZMode), "NewAvailabilityZones" :: NullOrUndefined (PreferredAvailabilityZoneList), "CacheSecurityGroupNames" :: NullOrUndefined (CacheSecurityGroupNameList), "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdsList), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "NotificationTopicArn" :: NullOrUndefined (String), "CacheParameterGroupName" :: NullOrUndefined (String), "NotificationTopicStatus" :: NullOrUndefined (String), "ApplyImmediately" :: NullOrUndefined (Boolean), "EngineVersion" :: NullOrUndefined (String), "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional), "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional), "SnapshotWindow" :: NullOrUndefined (String), "CacheNodeType" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>ModifyCacheCluster</code> operation.</p>

##### Instances
``` purescript
Newtype ModifyCacheClusterMessage _
```

#### `ModifyCacheClusterResult`

``` purescript
newtype ModifyCacheClusterResult
  = ModifyCacheClusterResult { "CacheCluster" :: NullOrUndefined (CacheCluster) }
```

##### Instances
``` purescript
Newtype ModifyCacheClusterResult _
```

#### `ModifyCacheParameterGroupMessage`

``` purescript
newtype ModifyCacheParameterGroupMessage
  = ModifyCacheParameterGroupMessage { "CacheParameterGroupName" :: String, "ParameterNameValues" :: ParameterNameValueList }
```

<p>Represents the input of a <code>ModifyCacheParameterGroup</code> operation.</p>

##### Instances
``` purescript
Newtype ModifyCacheParameterGroupMessage _
```

#### `ModifyCacheSubnetGroupMessage`

``` purescript
newtype ModifyCacheSubnetGroupMessage
  = ModifyCacheSubnetGroupMessage { "CacheSubnetGroupName" :: String, "CacheSubnetGroupDescription" :: NullOrUndefined (String), "SubnetIds" :: NullOrUndefined (SubnetIdentifierList) }
```

<p>Represents the input of a <code>ModifyCacheSubnetGroup</code> operation.</p>

##### Instances
``` purescript
Newtype ModifyCacheSubnetGroupMessage _
```

#### `ModifyCacheSubnetGroupResult`

``` purescript
newtype ModifyCacheSubnetGroupResult
  = ModifyCacheSubnetGroupResult { "CacheSubnetGroup" :: NullOrUndefined (CacheSubnetGroup) }
```

##### Instances
``` purescript
Newtype ModifyCacheSubnetGroupResult _
```

#### `ModifyReplicationGroupMessage`

``` purescript
newtype ModifyReplicationGroupMessage
  = ModifyReplicationGroupMessage { "ReplicationGroupId" :: String, "ReplicationGroupDescription" :: NullOrUndefined (String), "PrimaryClusterId" :: NullOrUndefined (String), "SnapshottingClusterId" :: NullOrUndefined (String), "AutomaticFailoverEnabled" :: NullOrUndefined (BooleanOptional), "CacheSecurityGroupNames" :: NullOrUndefined (CacheSecurityGroupNameList), "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdsList), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "NotificationTopicArn" :: NullOrUndefined (String), "CacheParameterGroupName" :: NullOrUndefined (String), "NotificationTopicStatus" :: NullOrUndefined (String), "ApplyImmediately" :: NullOrUndefined (Boolean), "EngineVersion" :: NullOrUndefined (String), "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional), "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional), "SnapshotWindow" :: NullOrUndefined (String), "CacheNodeType" :: NullOrUndefined (String), "NodeGroupId" :: NullOrUndefined (String) }
```

<p>Represents the input of a <code>ModifyReplicationGroups</code> operation.</p>

##### Instances
``` purescript
Newtype ModifyReplicationGroupMessage _
```

#### `ModifyReplicationGroupResult`

``` purescript
newtype ModifyReplicationGroupResult
  = ModifyReplicationGroupResult { "ReplicationGroup" :: NullOrUndefined (ReplicationGroup) }
```

##### Instances
``` purescript
Newtype ModifyReplicationGroupResult _
```

#### `ModifyReplicationGroupShardConfigurationMessage`

``` purescript
newtype ModifyReplicationGroupShardConfigurationMessage
  = ModifyReplicationGroupShardConfigurationMessage { "ReplicationGroupId" :: String, "NodeGroupCount" :: Int, "ApplyImmediately" :: Boolean, "ReshardingConfiguration" :: NullOrUndefined (ReshardingConfigurationList), "NodeGroupsToRemove" :: NullOrUndefined (NodeGroupsToRemoveList) }
```

<p>Represents the input for a <code>ModifyReplicationGroupShardConfiguration</code> operation.</p>

##### Instances
``` purescript
Newtype ModifyReplicationGroupShardConfigurationMessage _
```

#### `ModifyReplicationGroupShardConfigurationResult`

``` purescript
newtype ModifyReplicationGroupShardConfigurationResult
  = ModifyReplicationGroupShardConfigurationResult { "ReplicationGroup" :: NullOrUndefined (ReplicationGroup) }
```

##### Instances
``` purescript
Newtype ModifyReplicationGroupShardConfigurationResult _
```

#### `NodeGroup`

``` purescript
newtype NodeGroup
  = NodeGroup { "NodeGroupId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "PrimaryEndpoint" :: NullOrUndefined (Endpoint), "Slots" :: NullOrUndefined (String), "NodeGroupMembers" :: NullOrUndefined (NodeGroupMemberList) }
```

<p>Represents a collection of cache nodes in a replication group. One node in the node group is the read/write primary node. All the other nodes are read-only Replica nodes.</p>

##### Instances
``` purescript
Newtype NodeGroup _
```

#### `NodeGroupConfiguration`

``` purescript
newtype NodeGroupConfiguration
  = NodeGroupConfiguration { "Slots" :: NullOrUndefined (String), "ReplicaCount" :: NullOrUndefined (IntegerOptional), "PrimaryAvailabilityZone" :: NullOrUndefined (String), "ReplicaAvailabilityZones" :: NullOrUndefined (AvailabilityZonesList) }
```

<p>Node group (shard) configuration options. Each node group (shard) configuration has the following: <code>Slots</code>, <code>PrimaryAvailabilityZone</code>, <code>ReplicaAvailabilityZones</code>, <code>ReplicaCount</code>.</p>

##### Instances
``` purescript
Newtype NodeGroupConfiguration _
```

#### `NodeGroupConfigurationList`

``` purescript
newtype NodeGroupConfigurationList
  = NodeGroupConfigurationList (Array NodeGroupConfiguration)
```

##### Instances
``` purescript
Newtype NodeGroupConfigurationList _
```

#### `NodeGroupList`

``` purescript
newtype NodeGroupList
  = NodeGroupList (Array NodeGroup)
```

##### Instances
``` purescript
Newtype NodeGroupList _
```

#### `NodeGroupMember`

``` purescript
newtype NodeGroupMember
  = NodeGroupMember { "CacheClusterId" :: NullOrUndefined (String), "CacheNodeId" :: NullOrUndefined (String), "ReadEndpoint" :: NullOrUndefined (Endpoint), "PreferredAvailabilityZone" :: NullOrUndefined (String), "CurrentRole" :: NullOrUndefined (String) }
```

<p>Represents a single node within a node group (shard).</p>

##### Instances
``` purescript
Newtype NodeGroupMember _
```

#### `NodeGroupMemberList`

``` purescript
newtype NodeGroupMemberList
  = NodeGroupMemberList (Array NodeGroupMember)
```

##### Instances
``` purescript
Newtype NodeGroupMemberList _
```

#### `NodeGroupNotFoundFault`

``` purescript
newtype NodeGroupNotFoundFault
  = NodeGroupNotFoundFault {  }
```

<p>The node group specified by the <code>NodeGroupId</code> parameter could not be found. Please verify that the node group exists and that you spelled the <code>NodeGroupId</code> value correctly.</p>

##### Instances
``` purescript
Newtype NodeGroupNotFoundFault _
```

#### `NodeGroupsPerReplicationGroupQuotaExceededFault`

``` purescript
newtype NodeGroupsPerReplicationGroupQuotaExceededFault
  = NodeGroupsPerReplicationGroupQuotaExceededFault {  }
```

<p>The request cannot be processed because it would exceed the maximum allowed number of node groups (shards) in a single replication group. The default maximum is 15</p>

##### Instances
``` purescript
Newtype NodeGroupsPerReplicationGroupQuotaExceededFault _
```

#### `NodeGroupsToRemoveList`

``` purescript
newtype NodeGroupsToRemoveList
  = NodeGroupsToRemoveList (Array String)
```

##### Instances
``` purescript
Newtype NodeGroupsToRemoveList _
```

#### `NodeQuotaForClusterExceededFault`

``` purescript
newtype NodeQuotaForClusterExceededFault
  = NodeQuotaForClusterExceededFault {  }
```

<p>The request cannot be processed because it would exceed the allowed number of cache nodes in a single cluster.</p>

##### Instances
``` purescript
Newtype NodeQuotaForClusterExceededFault _
```

#### `NodeQuotaForCustomerExceededFault`

``` purescript
newtype NodeQuotaForCustomerExceededFault
  = NodeQuotaForCustomerExceededFault {  }
```

<p>The request cannot be processed because it would exceed the allowed number of cache nodes per customer.</p>

##### Instances
``` purescript
Newtype NodeQuotaForCustomerExceededFault _
```

#### `NodeSnapshot`

``` purescript
newtype NodeSnapshot
  = NodeSnapshot { "CacheClusterId" :: NullOrUndefined (String), "NodeGroupId" :: NullOrUndefined (String), "CacheNodeId" :: NullOrUndefined (String), "NodeGroupConfiguration" :: NullOrUndefined (NodeGroupConfiguration), "CacheSize" :: NullOrUndefined (String), "CacheNodeCreateTime" :: NullOrUndefined (TStamp), "SnapshotCreateTime" :: NullOrUndefined (TStamp) }
```

<p>Represents an individual cache node in a snapshot of a cluster.</p>

##### Instances
``` purescript
Newtype NodeSnapshot _
```

#### `NodeSnapshotList`

``` purescript
newtype NodeSnapshotList
  = NodeSnapshotList (Array NodeSnapshot)
```

##### Instances
``` purescript
Newtype NodeSnapshotList _
```

#### `NodeTypeList`

``` purescript
newtype NodeTypeList
  = NodeTypeList (Array String)
```

##### Instances
``` purescript
Newtype NodeTypeList _
```

#### `NotificationConfiguration`

``` purescript
newtype NotificationConfiguration
  = NotificationConfiguration { "TopicArn" :: NullOrUndefined (String), "TopicStatus" :: NullOrUndefined (String) }
```

<p>Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS).</p>

##### Instances
``` purescript
Newtype NotificationConfiguration _
```

#### `Parameter`

``` purescript
newtype Parameter
  = Parameter { "ParameterName" :: NullOrUndefined (String), "ParameterValue" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Source" :: NullOrUndefined (String), "DataType" :: NullOrUndefined (String), "AllowedValues" :: NullOrUndefined (String), "IsModifiable" :: NullOrUndefined (Boolean), "MinimumEngineVersion" :: NullOrUndefined (String), "ChangeType" :: NullOrUndefined (ChangeType) }
```

<p>Describes an individual setting that controls some aspect of ElastiCache behavior.</p>

##### Instances
``` purescript
Newtype Parameter _
```

#### `ParameterNameValue`

``` purescript
newtype ParameterNameValue
  = ParameterNameValue { "ParameterName" :: NullOrUndefined (String), "ParameterValue" :: NullOrUndefined (String) }
```

<p>Describes a name-value pair that is used to update the value of a parameter.</p>

##### Instances
``` purescript
Newtype ParameterNameValue _
```

#### `ParameterNameValueList`

``` purescript
newtype ParameterNameValueList
  = ParameterNameValueList (Array ParameterNameValue)
```

##### Instances
``` purescript
Newtype ParameterNameValueList _
```

#### `ParametersList`

``` purescript
newtype ParametersList
  = ParametersList (Array Parameter)
```

##### Instances
``` purescript
Newtype ParametersList _
```

#### `PendingAutomaticFailoverStatus`

``` purescript
newtype PendingAutomaticFailoverStatus
  = PendingAutomaticFailoverStatus String
```

##### Instances
``` purescript
Newtype PendingAutomaticFailoverStatus _
```

#### `PendingModifiedValues`

``` purescript
newtype PendingModifiedValues
  = PendingModifiedValues { "NumCacheNodes" :: NullOrUndefined (IntegerOptional), "CacheNodeIdsToRemove" :: NullOrUndefined (CacheNodeIdsList), "EngineVersion" :: NullOrUndefined (String), "CacheNodeType" :: NullOrUndefined (String) }
```

<p>A group of settings that are applied to the cluster in the future, or that are currently being applied.</p>

##### Instances
``` purescript
Newtype PendingModifiedValues _
```

#### `PreferredAvailabilityZoneList`

``` purescript
newtype PreferredAvailabilityZoneList
  = PreferredAvailabilityZoneList (Array String)
```

##### Instances
``` purescript
Newtype PreferredAvailabilityZoneList _
```

#### `PurchaseReservedCacheNodesOfferingMessage`

``` purescript
newtype PurchaseReservedCacheNodesOfferingMessage
  = PurchaseReservedCacheNodesOfferingMessage { "ReservedCacheNodesOfferingId" :: String, "ReservedCacheNodeId" :: NullOrUndefined (String), "CacheNodeCount" :: NullOrUndefined (IntegerOptional) }
```

<p>Represents the input of a <code>PurchaseReservedCacheNodesOffering</code> operation.</p>

##### Instances
``` purescript
Newtype PurchaseReservedCacheNodesOfferingMessage _
```

#### `PurchaseReservedCacheNodesOfferingResult`

``` purescript
newtype PurchaseReservedCacheNodesOfferingResult
  = PurchaseReservedCacheNodesOfferingResult { "ReservedCacheNode" :: NullOrUndefined (ReservedCacheNode) }
```

##### Instances
``` purescript
Newtype PurchaseReservedCacheNodesOfferingResult _
```

#### `RebootCacheClusterMessage`

``` purescript
newtype RebootCacheClusterMessage
  = RebootCacheClusterMessage { "CacheClusterId" :: String, "CacheNodeIdsToReboot" :: CacheNodeIdsList }
```

<p>Represents the input of a <code>RebootCacheCluster</code> operation.</p>

##### Instances
``` purescript
Newtype RebootCacheClusterMessage _
```

#### `RebootCacheClusterResult`

``` purescript
newtype RebootCacheClusterResult
  = RebootCacheClusterResult { "CacheCluster" :: NullOrUndefined (CacheCluster) }
```

##### Instances
``` purescript
Newtype RebootCacheClusterResult _
```

#### `RecurringCharge`

``` purescript
newtype RecurringCharge
  = RecurringCharge { "RecurringChargeAmount" :: NullOrUndefined (Number), "RecurringChargeFrequency" :: NullOrUndefined (String) }
```

<p>Contains the specific price and frequency of a recurring charges for a reserved cache node, or for a reserved cache node offering.</p>

##### Instances
``` purescript
Newtype RecurringCharge _
```

#### `RecurringChargeList`

``` purescript
newtype RecurringChargeList
  = RecurringChargeList (Array RecurringCharge)
```

##### Instances
``` purescript
Newtype RecurringChargeList _
```

#### `RemoveTagsFromResourceMessage`

``` purescript
newtype RemoveTagsFromResourceMessage
  = RemoveTagsFromResourceMessage { "ResourceName" :: String, "TagKeys" :: KeyList }
```

<p>Represents the input of a <code>RemoveTagsFromResource</code> operation.</p>

##### Instances
``` purescript
Newtype RemoveTagsFromResourceMessage _
```

#### `ReplicationGroup`

``` purescript
newtype ReplicationGroup
  = ReplicationGroup { "ReplicationGroupId" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "PendingModifiedValues" :: NullOrUndefined (ReplicationGroupPendingModifiedValues), "MemberClusters" :: NullOrUndefined (ClusterIdList), "NodeGroups" :: NullOrUndefined (NodeGroupList), "SnapshottingClusterId" :: NullOrUndefined (String), "AutomaticFailover" :: NullOrUndefined (AutomaticFailoverStatus), "ConfigurationEndpoint" :: NullOrUndefined (Endpoint), "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional), "SnapshotWindow" :: NullOrUndefined (String), "ClusterEnabled" :: NullOrUndefined (BooleanOptional), "CacheNodeType" :: NullOrUndefined (String), "AuthTokenEnabled" :: NullOrUndefined (BooleanOptional), "TransitEncryptionEnabled" :: NullOrUndefined (BooleanOptional), "AtRestEncryptionEnabled" :: NullOrUndefined (BooleanOptional) }
```

<p>Contains all of the attributes of a specific Redis replication group.</p>

##### Instances
``` purescript
Newtype ReplicationGroup _
```

#### `ReplicationGroupAlreadyExistsFault`

``` purescript
newtype ReplicationGroupAlreadyExistsFault
  = ReplicationGroupAlreadyExistsFault {  }
```

<p>The specified replication group already exists.</p>

##### Instances
``` purescript
Newtype ReplicationGroupAlreadyExistsFault _
```

#### `ReplicationGroupList`

``` purescript
newtype ReplicationGroupList
  = ReplicationGroupList (Array ReplicationGroup)
```

##### Instances
``` purescript
Newtype ReplicationGroupList _
```

#### `ReplicationGroupMessage`

``` purescript
newtype ReplicationGroupMessage
  = ReplicationGroupMessage { "Marker" :: NullOrUndefined (String), "ReplicationGroups" :: NullOrUndefined (ReplicationGroupList) }
```

<p>Represents the output of a <code>DescribeReplicationGroups</code> operation.</p>

##### Instances
``` purescript
Newtype ReplicationGroupMessage _
```

#### `ReplicationGroupNotFoundFault`

``` purescript
newtype ReplicationGroupNotFoundFault
  = ReplicationGroupNotFoundFault {  }
```

<p>The specified replication group does not exist.</p>

##### Instances
``` purescript
Newtype ReplicationGroupNotFoundFault _
```

#### `ReplicationGroupPendingModifiedValues`

``` purescript
newtype ReplicationGroupPendingModifiedValues
  = ReplicationGroupPendingModifiedValues { "PrimaryClusterId" :: NullOrUndefined (String), "AutomaticFailoverStatus" :: NullOrUndefined (PendingAutomaticFailoverStatus), "Resharding" :: NullOrUndefined (ReshardingStatus) }
```

<p>The settings to be applied to the Redis replication group, either immediately or during the next maintenance window.</p>

##### Instances
``` purescript
Newtype ReplicationGroupPendingModifiedValues _
```

#### `ReservedCacheNode`

``` purescript
newtype ReservedCacheNode
  = ReservedCacheNode { "ReservedCacheNodeId" :: NullOrUndefined (String), "ReservedCacheNodesOfferingId" :: NullOrUndefined (String), "CacheNodeType" :: NullOrUndefined (String), "StartTime" :: NullOrUndefined (TStamp), "Duration" :: NullOrUndefined (Int), "FixedPrice" :: NullOrUndefined (Number), "UsagePrice" :: NullOrUndefined (Number), "CacheNodeCount" :: NullOrUndefined (Int), "ProductDescription" :: NullOrUndefined (String), "OfferingType" :: NullOrUndefined (String), "State" :: NullOrUndefined (String), "RecurringCharges" :: NullOrUndefined (RecurringChargeList) }
```

<p>Represents the output of a <code>PurchaseReservedCacheNodesOffering</code> operation.</p>

##### Instances
``` purescript
Newtype ReservedCacheNode _
```

#### `ReservedCacheNodeAlreadyExistsFault`

``` purescript
newtype ReservedCacheNodeAlreadyExistsFault
  = ReservedCacheNodeAlreadyExistsFault {  }
```

<p>You already have a reservation with the given identifier.</p>

##### Instances
``` purescript
Newtype ReservedCacheNodeAlreadyExistsFault _
```

#### `ReservedCacheNodeList`

``` purescript
newtype ReservedCacheNodeList
  = ReservedCacheNodeList (Array ReservedCacheNode)
```

##### Instances
``` purescript
Newtype ReservedCacheNodeList _
```

#### `ReservedCacheNodeMessage`

``` purescript
newtype ReservedCacheNodeMessage
  = ReservedCacheNodeMessage { "Marker" :: NullOrUndefined (String), "ReservedCacheNodes" :: NullOrUndefined (ReservedCacheNodeList) }
```

<p>Represents the output of a <code>DescribeReservedCacheNodes</code> operation.</p>

##### Instances
``` purescript
Newtype ReservedCacheNodeMessage _
```

#### `ReservedCacheNodeNotFoundFault`

``` purescript
newtype ReservedCacheNodeNotFoundFault
  = ReservedCacheNodeNotFoundFault {  }
```

<p>The requested reserved cache node was not found.</p>

##### Instances
``` purescript
Newtype ReservedCacheNodeNotFoundFault _
```

#### `ReservedCacheNodeQuotaExceededFault`

``` purescript
newtype ReservedCacheNodeQuotaExceededFault
  = ReservedCacheNodeQuotaExceededFault {  }
```

<p>The request cannot be processed because it would exceed the user's cache node quota.</p>

##### Instances
``` purescript
Newtype ReservedCacheNodeQuotaExceededFault _
```

#### `ReservedCacheNodesOffering`

``` purescript
newtype ReservedCacheNodesOffering
  = ReservedCacheNodesOffering { "ReservedCacheNodesOfferingId" :: NullOrUndefined (String), "CacheNodeType" :: NullOrUndefined (String), "Duration" :: NullOrUndefined (Int), "FixedPrice" :: NullOrUndefined (Number), "UsagePrice" :: NullOrUndefined (Number), "ProductDescription" :: NullOrUndefined (String), "OfferingType" :: NullOrUndefined (String), "RecurringCharges" :: NullOrUndefined (RecurringChargeList) }
```

<p>Describes all of the attributes of a reserved cache node offering.</p>

##### Instances
``` purescript
Newtype ReservedCacheNodesOffering _
```

#### `ReservedCacheNodesOfferingList`

``` purescript
newtype ReservedCacheNodesOfferingList
  = ReservedCacheNodesOfferingList (Array ReservedCacheNodesOffering)
```

##### Instances
``` purescript
Newtype ReservedCacheNodesOfferingList _
```

#### `ReservedCacheNodesOfferingMessage`

``` purescript
newtype ReservedCacheNodesOfferingMessage
  = ReservedCacheNodesOfferingMessage { "Marker" :: NullOrUndefined (String), "ReservedCacheNodesOfferings" :: NullOrUndefined (ReservedCacheNodesOfferingList) }
```

<p>Represents the output of a <code>DescribeReservedCacheNodesOfferings</code> operation.</p>

##### Instances
``` purescript
Newtype ReservedCacheNodesOfferingMessage _
```

#### `ReservedCacheNodesOfferingNotFoundFault`

``` purescript
newtype ReservedCacheNodesOfferingNotFoundFault
  = ReservedCacheNodesOfferingNotFoundFault {  }
```

<p>The requested cache node offering does not exist.</p>

##### Instances
``` purescript
Newtype ReservedCacheNodesOfferingNotFoundFault _
```

#### `ResetCacheParameterGroupMessage`

``` purescript
newtype ResetCacheParameterGroupMessage
  = ResetCacheParameterGroupMessage { "CacheParameterGroupName" :: String, "ResetAllParameters" :: NullOrUndefined (Boolean), "ParameterNameValues" :: NullOrUndefined (ParameterNameValueList) }
```

<p>Represents the input of a <code>ResetCacheParameterGroup</code> operation.</p>

##### Instances
``` purescript
Newtype ResetCacheParameterGroupMessage _
```

#### `ReshardingConfiguration`

``` purescript
newtype ReshardingConfiguration
  = ReshardingConfiguration { "PreferredAvailabilityZones" :: NullOrUndefined (AvailabilityZonesList) }
```

<p>A list of <code>PreferredAvailabilityZones</code> objects that specifies the configuration of a node group in the resharded cluster.</p>

##### Instances
``` purescript
Newtype ReshardingConfiguration _
```

#### `ReshardingConfigurationList`

``` purescript
newtype ReshardingConfigurationList
  = ReshardingConfigurationList (Array ReshardingConfiguration)
```

##### Instances
``` purescript
Newtype ReshardingConfigurationList _
```

#### `ReshardingStatus`

``` purescript
newtype ReshardingStatus
  = ReshardingStatus { "SlotMigration" :: NullOrUndefined (SlotMigration) }
```

<p>The status of an online resharding operation.</p>

##### Instances
``` purescript
Newtype ReshardingStatus _
```

#### `RevokeCacheSecurityGroupIngressMessage`

``` purescript
newtype RevokeCacheSecurityGroupIngressMessage
  = RevokeCacheSecurityGroupIngressMessage { "CacheSecurityGroupName" :: String, "EC2SecurityGroupName" :: String, "EC2SecurityGroupOwnerId" :: String }
```

<p>Represents the input of a <code>RevokeCacheSecurityGroupIngress</code> operation.</p>

##### Instances
``` purescript
Newtype RevokeCacheSecurityGroupIngressMessage _
```

#### `RevokeCacheSecurityGroupIngressResult`

``` purescript
newtype RevokeCacheSecurityGroupIngressResult
  = RevokeCacheSecurityGroupIngressResult { "CacheSecurityGroup" :: NullOrUndefined (CacheSecurityGroup) }
```

##### Instances
``` purescript
Newtype RevokeCacheSecurityGroupIngressResult _
```

#### `SecurityGroupIdsList`

``` purescript
newtype SecurityGroupIdsList
  = SecurityGroupIdsList (Array String)
```

##### Instances
``` purescript
Newtype SecurityGroupIdsList _
```

#### `SecurityGroupMembership`

``` purescript
newtype SecurityGroupMembership
  = SecurityGroupMembership { "SecurityGroupId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String) }
```

<p>Represents a single cache security group and its status.</p>

##### Instances
``` purescript
Newtype SecurityGroupMembership _
```

#### `SecurityGroupMembershipList`

``` purescript
newtype SecurityGroupMembershipList
  = SecurityGroupMembershipList (Array SecurityGroupMembership)
```

##### Instances
``` purescript
Newtype SecurityGroupMembershipList _
```

#### `SlotMigration`

``` purescript
newtype SlotMigration
  = SlotMigration { "ProgressPercentage" :: NullOrUndefined (Number) }
```

<p>Represents the progress of an online resharding operation.</p>

##### Instances
``` purescript
Newtype SlotMigration _
```

#### `Snapshot`

``` purescript
newtype Snapshot
  = Snapshot { "SnapshotName" :: NullOrUndefined (String), "ReplicationGroupId" :: NullOrUndefined (String), "ReplicationGroupDescription" :: NullOrUndefined (String), "CacheClusterId" :: NullOrUndefined (String), "SnapshotStatus" :: NullOrUndefined (String), "SnapshotSource" :: NullOrUndefined (String), "CacheNodeType" :: NullOrUndefined (String), "Engine" :: NullOrUndefined (String), "EngineVersion" :: NullOrUndefined (String), "NumCacheNodes" :: NullOrUndefined (IntegerOptional), "PreferredAvailabilityZone" :: NullOrUndefined (String), "CacheClusterCreateTime" :: NullOrUndefined (TStamp), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "TopicArn" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "CacheParameterGroupName" :: NullOrUndefined (String), "CacheSubnetGroupName" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean), "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional), "SnapshotWindow" :: NullOrUndefined (String), "NumNodeGroups" :: NullOrUndefined (IntegerOptional), "AutomaticFailover" :: NullOrUndefined (AutomaticFailoverStatus), "NodeSnapshots" :: NullOrUndefined (NodeSnapshotList) }
```

<p>Represents a copy of an entire Redis cluster as of the time when the snapshot was taken.</p>

##### Instances
``` purescript
Newtype Snapshot _
```

#### `SnapshotAlreadyExistsFault`

``` purescript
newtype SnapshotAlreadyExistsFault
  = SnapshotAlreadyExistsFault {  }
```

<p>You already have a snapshot with the given name.</p>

##### Instances
``` purescript
Newtype SnapshotAlreadyExistsFault _
```

#### `SnapshotArnsList`

``` purescript
newtype SnapshotArnsList
  = SnapshotArnsList (Array String)
```

##### Instances
``` purescript
Newtype SnapshotArnsList _
```

#### `SnapshotFeatureNotSupportedFault`

``` purescript
newtype SnapshotFeatureNotSupportedFault
  = SnapshotFeatureNotSupportedFault {  }
```

<p>You attempted one of the following operations:</p> <ul> <li> <p>Creating a snapshot of a Redis cluster running on a <code>cache.t1.micro</code> cache node.</p> </li> <li> <p>Creating a snapshot of a cluster that is running Memcached rather than Redis.</p> </li> </ul> <p>Neither of these are supported by ElastiCache.</p>

##### Instances
``` purescript
Newtype SnapshotFeatureNotSupportedFault _
```

#### `SnapshotList`

``` purescript
newtype SnapshotList
  = SnapshotList (Array Snapshot)
```

##### Instances
``` purescript
Newtype SnapshotList _
```

#### `SnapshotNotFoundFault`

``` purescript
newtype SnapshotNotFoundFault
  = SnapshotNotFoundFault {  }
```

<p>The requested snapshot name does not refer to an existing snapshot.</p>

##### Instances
``` purescript
Newtype SnapshotNotFoundFault _
```

#### `SnapshotQuotaExceededFault`

``` purescript
newtype SnapshotQuotaExceededFault
  = SnapshotQuotaExceededFault {  }
```

<p>The request cannot be processed because it would exceed the maximum number of snapshots.</p>

##### Instances
``` purescript
Newtype SnapshotQuotaExceededFault _
```

#### `SourceType`

``` purescript
newtype SourceType
  = SourceType String
```

##### Instances
``` purescript
Newtype SourceType _
```

#### `Subnet`

``` purescript
newtype Subnet
  = Subnet { "SubnetIdentifier" :: NullOrUndefined (String), "SubnetAvailabilityZone" :: NullOrUndefined (AvailabilityZone) }
```

<p>Represents the subnet associated with a cluster. This parameter refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used with ElastiCache.</p>

##### Instances
``` purescript
Newtype Subnet _
```

#### `SubnetIdentifierList`

``` purescript
newtype SubnetIdentifierList
  = SubnetIdentifierList (Array String)
```

##### Instances
``` purescript
Newtype SubnetIdentifierList _
```

#### `SubnetInUse`

``` purescript
newtype SubnetInUse
  = SubnetInUse {  }
```

<p>The requested subnet is being used by another cache subnet group.</p>

##### Instances
``` purescript
Newtype SubnetInUse _
```

#### `SubnetList`

``` purescript
newtype SubnetList
  = SubnetList (Array Subnet)
```

##### Instances
``` purescript
Newtype SubnetList _
```

#### `TStamp`

``` purescript
newtype TStamp
  = TStamp Number
```

##### Instances
``` purescript
Newtype TStamp _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (String), "Value" :: NullOrUndefined (String) }
```

<p>A cost allocation Tag that can be added to an ElastiCache cluster or replication group. Tags are composed of a Key/Value pair. A tag with a null Value is permitted.</p>

##### Instances
``` purescript
Newtype Tag _
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

#### `TagListMessage`

``` purescript
newtype TagListMessage
  = TagListMessage { "TagList" :: NullOrUndefined (TagList) }
```

<p>Represents the output from the <code>AddTagsToResource</code>, <code>ListTagsForResource</code>, and <code>RemoveTagsFromResource</code> operations.</p>

##### Instances
``` purescript
Newtype TagListMessage _
```

#### `TagNotFoundFault`

``` purescript
newtype TagNotFoundFault
  = TagNotFoundFault {  }
```

<p>The requested tag was not found on this resource.</p>

##### Instances
``` purescript
Newtype TagNotFoundFault _
```

#### `TagQuotaPerResourceExceeded`

``` purescript
newtype TagQuotaPerResourceExceeded
  = TagQuotaPerResourceExceeded {  }
```

<p>The request cannot be processed because it would cause the resource to have more than the allowed number of tags. The maximum number of tags permitted on a resource is 50.</p>

##### Instances
``` purescript
Newtype TagQuotaPerResourceExceeded _
```

#### `TestFailoverMessage`

``` purescript
newtype TestFailoverMessage
  = TestFailoverMessage { "ReplicationGroupId" :: String, "NodeGroupId" :: String }
```

##### Instances
``` purescript
Newtype TestFailoverMessage _
```

#### `TestFailoverNotAvailableFault`

``` purescript
newtype TestFailoverNotAvailableFault
  = TestFailoverNotAvailableFault {  }
```

##### Instances
``` purescript
Newtype TestFailoverNotAvailableFault _
```

#### `TestFailoverResult`

``` purescript
newtype TestFailoverResult
  = TestFailoverResult { "ReplicationGroup" :: NullOrUndefined (ReplicationGroup) }
```

##### Instances
``` purescript
Newtype TestFailoverResult _
```


