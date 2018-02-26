

-- | <fullname>Amazon ElastiCache</fullname> <p>Amazon ElastiCache is a web service that makes it easier to set up, operate, and scale a distributed cache in the cloud.</p> <p>With ElastiCache, customers get all of the benefits of a high-performance, in-memory cache with less of the administrative burden involved in launching and managing a distributed cache. The service makes setup, scaling, and cluster failure handling much simpler than in a self-managed cache deployment.</p> <p>In addition, through integration with Amazon CloudWatch, customers get enhanced visibility into the key performance statistics associated with their cache and can receive alarms if a part of their cache runs hot.</p>
module AWS.ElastiCache where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ElastiCache" :: String


-- | <p>Adds up to 50 cost allocation tags to the named resource. A cost allocation tag is a key-value pair where the key and value are case-sensitive. You can use cost allocation tags to categorize and track your AWS costs.</p> <p> When you apply tags to your ElastiCache resources, AWS generates a cost allocation report as a comma-separated value (CSV) file with your usage and costs aggregated by your tags. You can apply tags that represent business categories (such as cost centers, application names, or owners) to organize your costs across multiple services. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Tagging.html">Using Cost Allocation Tags in Amazon ElastiCache</a> in the <i>ElastiCache User Guide</i>.</p>
addTagsToResource :: forall eff. AddTagsToResourceMessage -> Aff (err :: AWS.RequestError | eff) TagListMessage
addTagsToResource = AWS.request serviceName "AddTagsToResource" 


-- | <p>Allows network ingress to a cache security group. Applications using ElastiCache must be running on Amazon EC2, and Amazon EC2 security groups are used as the authorization mechanism.</p> <note> <p>You cannot authorize ingress from an Amazon EC2 security group in one region to an ElastiCache cluster in another region.</p> </note>
authorizeCacheSecurityGroupIngress :: forall eff. AuthorizeCacheSecurityGroupIngressMessage -> Aff (err :: AWS.RequestError | eff) AuthorizeCacheSecurityGroupIngressResult
authorizeCacheSecurityGroupIngress = AWS.request serviceName "AuthorizeCacheSecurityGroupIngress" 


-- | <p>Makes a copy of an existing snapshot.</p> <note> <p>This operation is valid for Redis only.</p> </note> <important> <p>Users or groups that have permissions to use the <code>CopySnapshot</code> operation can create their own Amazon S3 buckets and copy snapshots to it. To control access to your snapshots, use an IAM policy to control who has the ability to use the <code>CopySnapshot</code> operation. For more information about using IAM to control the use of ElastiCache operations, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html">Exporting Snapshots</a> and <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/IAM.html">Authentication &amp; Access Control</a>.</p> </important> <p>You could receive the following error messages.</p> <p class="title"> <b>Error Messages</b> </p> <ul> <li> <p> <b>Error Message:</b> The S3 bucket %s is outside of the region.</p> <p> <b>Solution:</b> Create an Amazon S3 bucket in the same region as your snapshot. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.CreateBucket">Step 1: Create an Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message:</b> The S3 bucket %s does not exist.</p> <p> <b>Solution:</b> Create an Amazon S3 bucket in the same region as your snapshot. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.CreateBucket">Step 1: Create an Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message:</b> The S3 bucket %s is not owned by the authenticated user.</p> <p> <b>Solution:</b> Create an Amazon S3 bucket in the same region as your snapshot. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.CreateBucket">Step 1: Create an Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message:</b> The authenticated user does not have sufficient permissions to perform the desired activity.</p> <p> <b>Solution:</b> Contact your system administrator to get the needed permissions.</p> </li> <li> <p> <b>Error Message:</b> The S3 bucket %s already contains an object with key %s.</p> <p> <b>Solution:</b> Give the <code>TargetSnapshotName</code> a new and unique value. If exporting a snapshot, you could alternatively create a new Amazon S3 bucket and use this same value for <code>TargetSnapshotName</code>.</p> </li> <li> <p> <b>Error Message: </b> ElastiCache has not been granted READ permissions %s on the S3 Bucket.</p> <p> <b>Solution:</b> Add List and Read permissions on the bucket. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.GrantAccess">Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message: </b> ElastiCache has not been granted WRITE permissions %s on the S3 Bucket.</p> <p> <b>Solution:</b> Add Upload/Delete permissions on the bucket. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.GrantAccess">Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message: </b> ElastiCache has not been granted READ_ACP permissions %s on the S3 Bucket.</p> <p> <b>Solution:</b> Add View Permissions on the bucket. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.GrantAccess">Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> </ul>
copySnapshot :: forall eff. CopySnapshotMessage -> Aff (err :: AWS.RequestError | eff) CopySnapshotResult
copySnapshot = AWS.request serviceName "CopySnapshot" 


-- | <p>Creates a cluster. All nodes in the cluster run the same protocol-compliant cache engine software, either Memcached or Redis.</p> <important> <p>Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups.</p> </important>
createCacheCluster :: forall eff. CreateCacheClusterMessage -> Aff (err :: AWS.RequestError | eff) CreateCacheClusterResult
createCacheCluster = AWS.request serviceName "CreateCacheCluster" 


-- | <p>Creates a new Amazon ElastiCache cache parameter group. An ElastiCache cache parameter group is a collection of parameters and their values that are applied to all of the nodes in any cluster or replication group using the CacheParameterGroup.</p> <p>A newly created CacheParameterGroup is an exact duplicate of the default parameter group for the CacheParameterGroupFamily. To customize the newly created CacheParameterGroup you can change the values of specific parameters. For more information, see:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyCacheParameterGroup.html">ModifyCacheParameterGroup</a> in the ElastiCache API Reference.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/ParameterGroups.html">Parameters and Parameter Groups</a> in the ElastiCache User Guide.</p> </li> </ul>
createCacheParameterGroup :: forall eff. CreateCacheParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateCacheParameterGroupResult
createCacheParameterGroup = AWS.request serviceName "CreateCacheParameterGroup" 


-- | <p>Creates a new cache security group. Use a cache security group to control access to one or more clusters.</p> <p>Cache security groups are only used when you are creating a cluster outside of an Amazon Virtual Private Cloud (Amazon VPC). If you are creating a cluster inside of a VPC, use a cache subnet group instead. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheSubnetGroup.html">CreateCacheSubnetGroup</a>.</p>
createCacheSecurityGroup :: forall eff. CreateCacheSecurityGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateCacheSecurityGroupResult
createCacheSecurityGroup = AWS.request serviceName "CreateCacheSecurityGroup" 


-- | <p>Creates a new cache subnet group.</p> <p>Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).</p>
createCacheSubnetGroup :: forall eff. CreateCacheSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateCacheSubnetGroupResult
createCacheSubnetGroup = AWS.request serviceName "CreateCacheSubnetGroup" 


-- | <p>Creates a Redis (cluster mode disabled) or a Redis (cluster mode enabled) replication group.</p> <p>A Redis (cluster mode disabled) replication group is a collection of clusters, where one of the clusters is a read/write primary and the others are read-only replicas. Writes to the primary are asynchronously propagated to the replicas.</p> <p>A Redis (cluster mode enabled) replication group is a collection of 1 to 15 node groups (shards). Each node group (shard) has one read/write primary node and up to 5 read-only replica nodes. Writes to the primary are asynchronously propagated to the replicas. Redis (cluster mode enabled) replication groups partition the data across node groups (shards).</p> <p>When a Redis (cluster mode disabled) replication group has been successfully created, you can add one or more read replicas to it, up to a total of 5 read replicas. You cannot alter a Redis (cluster mode enabled) replication group after it has been created. However, if you need to increase or decrease the number of node groups (console: shards), you can avail yourself of ElastiCache for Redis' enhanced backup and restore. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/backups-restoring.html">Restoring From a Backup with Cluster Resizing</a> in the <i>ElastiCache User Guide</i>.</p> <note> <p>This operation is valid for Redis only.</p> </note>
createReplicationGroup :: forall eff. CreateReplicationGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateReplicationGroupResult
createReplicationGroup = AWS.request serviceName "CreateReplicationGroup" 


-- | <p>Creates a copy of an entire cluster or replication group at a specific moment in time.</p> <note> <p>This operation is valid for Redis only.</p> </note>
createSnapshot :: forall eff. CreateSnapshotMessage -> Aff (err :: AWS.RequestError | eff) CreateSnapshotResult
createSnapshot = AWS.request serviceName "CreateSnapshot" 


-- | <p>Deletes a previously provisioned cluster. <code>DeleteCacheCluster</code> deletes all associated cache nodes, node endpoints and the cluster itself. When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the cluster; you cannot cancel or revert this operation.</p> <p>This operation cannot be used to delete a cluster that is the last read replica of a replication group or node group (shard) that has Multi-AZ mode enabled or a cluster from a Redis (cluster mode enabled) replication group.</p> <important> <p>Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups.</p> </important>
deleteCacheCluster :: forall eff. DeleteCacheClusterMessage -> Aff (err :: AWS.RequestError | eff) DeleteCacheClusterResult
deleteCacheCluster = AWS.request serviceName "DeleteCacheCluster" 


-- | <p>Deletes the specified cache parameter group. You cannot delete a cache parameter group if it is associated with any cache clusters.</p>
deleteCacheParameterGroup :: forall eff. DeleteCacheParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteCacheParameterGroup = AWS.request serviceName "DeleteCacheParameterGroup" 


-- | <p>Deletes a cache security group.</p> <note> <p>You cannot delete a cache security group if it is associated with any clusters.</p> </note>
deleteCacheSecurityGroup :: forall eff. DeleteCacheSecurityGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteCacheSecurityGroup = AWS.request serviceName "DeleteCacheSecurityGroup" 


-- | <p>Deletes a cache subnet group.</p> <note> <p>You cannot delete a cache subnet group if it is associated with any clusters.</p> </note>
deleteCacheSubnetGroup :: forall eff. DeleteCacheSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteCacheSubnetGroup = AWS.request serviceName "DeleteCacheSubnetGroup" 


-- | <p>Deletes an existing replication group. By default, this operation deletes the entire replication group, including the primary/primaries and all of the read replicas. If the replication group has only one primary, you can optionally delete only the read replicas, while retaining the primary by setting <code>RetainPrimaryCluster=true</code>.</p> <p>When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the selected resources; you cannot cancel or revert this operation.</p> <note> <p>This operation is valid for Redis only.</p> </note>
deleteReplicationGroup :: forall eff. DeleteReplicationGroupMessage -> Aff (err :: AWS.RequestError | eff) DeleteReplicationGroupResult
deleteReplicationGroup = AWS.request serviceName "DeleteReplicationGroup" 


-- | <p>Deletes an existing snapshot. When you receive a successful response from this operation, ElastiCache immediately begins deleting the snapshot; you cannot cancel or revert this operation.</p> <note> <p>This operation is valid for Redis only.</p> </note>
deleteSnapshot :: forall eff. DeleteSnapshotMessage -> Aff (err :: AWS.RequestError | eff) DeleteSnapshotResult
deleteSnapshot = AWS.request serviceName "DeleteSnapshot" 


-- | <p>Returns information about all provisioned clusters if no cluster identifier is specified, or about a specific cache cluster if a cluster identifier is supplied.</p> <p>By default, abbreviated information about the clusters is returned. You can use the optional <i>ShowCacheNodeInfo</i> flag to retrieve detailed information about the cache nodes associated with the clusters. These details include the DNS address and port for the cache node endpoint.</p> <p>If the cluster is in the <i>creating</i> state, only cluster-level information is displayed until all of the nodes are successfully provisioned.</p> <p>If the cluster is in the <i>deleting</i> state, only cluster-level information is displayed.</p> <p>If cache nodes are currently being added to the cluster, node endpoint information and creation time for the additional nodes are not displayed until they are completely provisioned. When the cluster state is <i>available</i>, the cluster is ready for use.</p> <p>If cache nodes are currently being removed from the cluster, no endpoint information for the removed nodes is displayed.</p>
describeCacheClusters :: forall eff. DescribeCacheClustersMessage -> Aff (err :: AWS.RequestError | eff) CacheClusterMessage
describeCacheClusters = AWS.request serviceName "DescribeCacheClusters" 


-- | <p>Returns a list of the available cache engines and their versions.</p>
describeCacheEngineVersions :: forall eff. DescribeCacheEngineVersionsMessage -> Aff (err :: AWS.RequestError | eff) CacheEngineVersionMessage
describeCacheEngineVersions = AWS.request serviceName "DescribeCacheEngineVersions" 


-- | <p>Returns a list of cache parameter group descriptions. If a cache parameter group name is specified, the list contains only the descriptions for that group.</p>
describeCacheParameterGroups :: forall eff. DescribeCacheParameterGroupsMessage -> Aff (err :: AWS.RequestError | eff) CacheParameterGroupsMessage
describeCacheParameterGroups = AWS.request serviceName "DescribeCacheParameterGroups" 


-- | <p>Returns the detailed parameter list for a particular cache parameter group.</p>
describeCacheParameters :: forall eff. DescribeCacheParametersMessage -> Aff (err :: AWS.RequestError | eff) CacheParameterGroupDetails
describeCacheParameters = AWS.request serviceName "DescribeCacheParameters" 


-- | <p>Returns a list of cache security group descriptions. If a cache security group name is specified, the list contains only the description of that group.</p>
describeCacheSecurityGroups :: forall eff. DescribeCacheSecurityGroupsMessage -> Aff (err :: AWS.RequestError | eff) CacheSecurityGroupMessage
describeCacheSecurityGroups = AWS.request serviceName "DescribeCacheSecurityGroups" 


-- | <p>Returns a list of cache subnet group descriptions. If a subnet group name is specified, the list contains only the description of that group.</p>
describeCacheSubnetGroups :: forall eff. DescribeCacheSubnetGroupsMessage -> Aff (err :: AWS.RequestError | eff) CacheSubnetGroupMessage
describeCacheSubnetGroups = AWS.request serviceName "DescribeCacheSubnetGroups" 


-- | <p>Returns the default engine and system parameter information for the specified cache engine.</p>
describeEngineDefaultParameters :: forall eff. DescribeEngineDefaultParametersMessage -> Aff (err :: AWS.RequestError | eff) DescribeEngineDefaultParametersResult
describeEngineDefaultParameters = AWS.request serviceName "DescribeEngineDefaultParameters" 


-- | <p>Returns events related to clusters, cache security groups, and cache parameter groups. You can obtain events specific to a particular cluster, cache security group, or cache parameter group by providing the name as a parameter.</p> <p>By default, only the events occurring within the last hour are returned; however, you can retrieve up to 14 days' worth of events if necessary.</p>
describeEvents :: forall eff. DescribeEventsMessage -> Aff (err :: AWS.RequestError | eff) EventsMessage
describeEvents = AWS.request serviceName "DescribeEvents" 


-- | <p>Returns information about a particular replication group. If no identifier is specified, <code>DescribeReplicationGroups</code> returns information about all replication groups.</p> <note> <p>This operation is valid for Redis only.</p> </note>
describeReplicationGroups :: forall eff. DescribeReplicationGroupsMessage -> Aff (err :: AWS.RequestError | eff) ReplicationGroupMessage
describeReplicationGroups = AWS.request serviceName "DescribeReplicationGroups" 


-- | <p>Returns information about reserved cache nodes for this account, or about a specified reserved cache node.</p>
describeReservedCacheNodes :: forall eff. DescribeReservedCacheNodesMessage -> Aff (err :: AWS.RequestError | eff) ReservedCacheNodeMessage
describeReservedCacheNodes = AWS.request serviceName "DescribeReservedCacheNodes" 


-- | <p>Lists available reserved cache node offerings.</p>
describeReservedCacheNodesOfferings :: forall eff. DescribeReservedCacheNodesOfferingsMessage -> Aff (err :: AWS.RequestError | eff) ReservedCacheNodesOfferingMessage
describeReservedCacheNodesOfferings = AWS.request serviceName "DescribeReservedCacheNodesOfferings" 


-- | <p>Returns information about cluster or replication group snapshots. By default, <code>DescribeSnapshots</code> lists all of your snapshots; it can optionally describe a single snapshot, or just the snapshots associated with a particular cache cluster.</p> <note> <p>This operation is valid for Redis only.</p> </note>
describeSnapshots :: forall eff. DescribeSnapshotsMessage -> Aff (err :: AWS.RequestError | eff) DescribeSnapshotsListMessage
describeSnapshots = AWS.request serviceName "DescribeSnapshots" 


-- | <p>Lists all available node types that you can scale your Redis cluster's or replication group's current node type up to.</p> <p>When you use the <code>ModifyCacheCluster</code> or <code>ModifyReplicationGroup</code> operations to scale up your cluster or replication group, the value of the <code>CacheNodeType</code> parameter must be one of the node types returned by this operation.</p>
listAllowedNodeTypeModifications :: forall eff. ListAllowedNodeTypeModificationsMessage -> Aff (err :: AWS.RequestError | eff) AllowedNodeTypeModificationsMessage
listAllowedNodeTypeModifications = AWS.request serviceName "ListAllowedNodeTypeModifications" 


-- | <p>Lists all cost allocation tags currently on the named resource. A <code>cost allocation tag</code> is a key-value pair where the key is case-sensitive and the value is optional. You can use cost allocation tags to categorize and track your AWS costs.</p> <p>You can have a maximum of 50 cost allocation tags on an ElastiCache resource. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/BestPractices.html">Using Cost Allocation Tags in Amazon ElastiCache</a>.</p>
listTagsForResource :: forall eff. ListTagsForResourceMessage -> Aff (err :: AWS.RequestError | eff) TagListMessage
listTagsForResource = AWS.request serviceName "ListTagsForResource" 


-- | <p>Modifies the settings for a cluster. You can use this operation to change one or more cluster configuration parameters by specifying the parameters and the new values.</p>
modifyCacheCluster :: forall eff. ModifyCacheClusterMessage -> Aff (err :: AWS.RequestError | eff) ModifyCacheClusterResult
modifyCacheCluster = AWS.request serviceName "ModifyCacheCluster" 


-- | <p>Modifies the parameters of a cache parameter group. You can modify up to 20 parameters in a single request by submitting a list parameter name and value pairs.</p>
modifyCacheParameterGroup :: forall eff. ModifyCacheParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) CacheParameterGroupNameMessage
modifyCacheParameterGroup = AWS.request serviceName "ModifyCacheParameterGroup" 


-- | <p>Modifies an existing cache subnet group.</p>
modifyCacheSubnetGroup :: forall eff. ModifyCacheSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) ModifyCacheSubnetGroupResult
modifyCacheSubnetGroup = AWS.request serviceName "ModifyCacheSubnetGroup" 


-- | <p>Modifies the settings for a replication group.</p> <important> <p>Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups.</p> </important> <note> <p>This operation is valid for Redis only.</p> </note>
modifyReplicationGroup :: forall eff. ModifyReplicationGroupMessage -> Aff (err :: AWS.RequestError | eff) ModifyReplicationGroupResult
modifyReplicationGroup = AWS.request serviceName "ModifyReplicationGroup" 


-- | <p>Performs horizontal scaling on a Redis (cluster mode enabled) cluster with no downtime. Requires Redis engine version 3.2.10 or newer. For information on upgrading your engine to a newer version, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/VersionManagement.html">Upgrading Engine Versions</a> in the Amazon ElastiCache User Guide.</p> <p>For more information on ElastiCache for Redis online horizontal scaling, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/redis-cluster-resharding-online.html">ElastiCache for Redis Horizontal Scaling</a> </p>
modifyReplicationGroupShardConfiguration :: forall eff. ModifyReplicationGroupShardConfigurationMessage -> Aff (err :: AWS.RequestError | eff) ModifyReplicationGroupShardConfigurationResult
modifyReplicationGroupShardConfiguration = AWS.request serviceName "ModifyReplicationGroupShardConfiguration" 


-- | <p>Allows you to purchase a reserved cache node offering.</p>
purchaseReservedCacheNodesOffering :: forall eff. PurchaseReservedCacheNodesOfferingMessage -> Aff (err :: AWS.RequestError | eff) PurchaseReservedCacheNodesOfferingResult
purchaseReservedCacheNodesOffering = AWS.request serviceName "PurchaseReservedCacheNodesOffering" 


-- | <p>Reboots some, or all, of the cache nodes within a provisioned cluster. This operation applies any modified cache parameter groups to the cluster. The reboot operation takes place as soon as possible, and results in a momentary outage to the cluster. During the reboot, the cluster status is set to REBOOTING.</p> <p>The reboot causes the contents of the cache (for each cache node being rebooted) to be lost.</p> <p>When the reboot is complete, a cluster event is created.</p> <p>Rebooting a cluster is currently supported on Memcached and Redis (cluster mode disabled) clusters. Rebooting is not supported on Redis (cluster mode enabled) clusters.</p> <p>If you make changes to parameters that require a Redis (cluster mode enabled) cluster reboot for the changes to be applied, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Clusters.Rebooting.htm">Rebooting a Cluster</a> for an alternate process.</p>
rebootCacheCluster :: forall eff. RebootCacheClusterMessage -> Aff (err :: AWS.RequestError | eff) RebootCacheClusterResult
rebootCacheCluster = AWS.request serviceName "RebootCacheCluster" 


-- | <p>Removes the tags identified by the <code>TagKeys</code> list from the named resource.</p>
removeTagsFromResource :: forall eff. RemoveTagsFromResourceMessage -> Aff (err :: AWS.RequestError | eff) TagListMessage
removeTagsFromResource = AWS.request serviceName "RemoveTagsFromResource" 


-- | <p>Modifies the parameters of a cache parameter group to the engine or system default value. You can reset specific parameters by submitting a list of parameter names. To reset the entire cache parameter group, specify the <code>ResetAllParameters</code> and <code>CacheParameterGroupName</code> parameters.</p>
resetCacheParameterGroup :: forall eff. ResetCacheParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) CacheParameterGroupNameMessage
resetCacheParameterGroup = AWS.request serviceName "ResetCacheParameterGroup" 


-- | <p>Revokes ingress from a cache security group. Use this operation to disallow access from an Amazon EC2 security group that had been previously authorized.</p>
revokeCacheSecurityGroupIngress :: forall eff. RevokeCacheSecurityGroupIngressMessage -> Aff (err :: AWS.RequestError | eff) RevokeCacheSecurityGroupIngressResult
revokeCacheSecurityGroupIngress = AWS.request serviceName "RevokeCacheSecurityGroupIngress" 


-- | <p>Represents the input of a <code>TestFailover</code> operation which test automatic failover on a specified node group (called shard in the console) in a replication group (called cluster in the console).</p> <p class="title"> <b>Note the following</b> </p> <ul> <li> <p>A customer can use this operation to test automatic failover on up to 5 shards (called node groups in the ElastiCache API and AWS CLI) in any rolling 24-hour period.</p> </li> <li> <p>If calling this operation on shards in different clusters (called replication groups in the API and CLI), the calls can be made concurrently.</p> <p> </p> </li> <li> <p>If calling this operation multiple times on different shards in the same Redis (cluster mode enabled) replication group, the first node replacement must complete before a subsequent call can be made.</p> </li> <li> <p>To determine whether the node replacement is complete you can check Events using the Amazon ElastiCache console, the AWS CLI, or the ElastiCache API. Look for the following automatic failover related events, listed here in order of occurrance:</p> <ol> <li> <p>Replication group message: <code>Test Failover API called for node group &lt;node-group-id&gt;</code> </p> </li> <li> <p>Cache cluster message: <code>Failover from master node &lt;primary-node-id&gt; to replica node &lt;node-id&gt; completed</code> </p> </li> <li> <p>Replication group message: <code>Failover from master node &lt;primary-node-id&gt; to replica node &lt;node-id&gt; completed</code> </p> </li> <li> <p>Cache cluster message: <code>Recovering cache nodes &lt;node-id&gt;</code> </p> </li> <li> <p>Cache cluster message: <code>Finished recovery for cache nodes &lt;node-id&gt;</code> </p> </li> </ol> <p>For more information see:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/ECEvents.Viewing.html">Viewing ElastiCache Events</a> in the <i>ElastiCache User Guide</i> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeEvents.html">DescribeEvents</a> in the ElastiCache API Reference</p> </li> </ul> </li> </ul> <p>Also see, <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/AutoFailover.html#auto-failover-test">Testing Multi-AZ with Automatic Failover</a> in the <i>ElastiCache User Guide</i>.</p>
testFailover :: forall eff. TestFailoverMessage -> Aff (err :: AWS.RequestError | eff) TestFailoverResult
testFailover = AWS.request serviceName "TestFailover" 


-- | <p>The customer has exceeded the allowed rate of API calls.</p>
newtype APICallRateForCustomerExceededFault = APICallRateForCustomerExceededFault 
  { 
  }


newtype AZMode = AZMode String


-- | <p>Represents the input of an AddTagsToResource operation.</p>
newtype AddTagsToResourceMessage = AddTagsToResourceMessage 
  { "ResourceName" :: (String)
  , "Tags" :: (TagList)
  }


-- | <p>Represents the allowed node types you can use to modify your cluster or replication group.</p>
newtype AllowedNodeTypeModificationsMessage = AllowedNodeTypeModificationsMessage 
  { "ScaleUpModifications" :: NullOrUndefined (NodeTypeList)
  }


-- | <p>The specified Amazon EC2 security group is already authorized for the specified cache security group.</p>
newtype AuthorizationAlreadyExistsFault = AuthorizationAlreadyExistsFault 
  { 
  }


-- | <p>The specified Amazon EC2 security group is not authorized for the specified cache security group.</p>
newtype AuthorizationNotFoundFault = AuthorizationNotFoundFault 
  { 
  }


-- | <p>Represents the input of an AuthorizeCacheSecurityGroupIngress operation.</p>
newtype AuthorizeCacheSecurityGroupIngressMessage = AuthorizeCacheSecurityGroupIngressMessage 
  { "CacheSecurityGroupName" :: (String)
  , "EC2SecurityGroupName" :: (String)
  , "EC2SecurityGroupOwnerId" :: (String)
  }


newtype AuthorizeCacheSecurityGroupIngressResult = AuthorizeCacheSecurityGroupIngressResult 
  { "CacheSecurityGroup" :: NullOrUndefined (CacheSecurityGroup)
  }


newtype AutomaticFailoverStatus = AutomaticFailoverStatus String


-- | <p>Describes an Availability Zone in which the cluster is launched.</p>
newtype AvailabilityZone = AvailabilityZone 
  { "Name" :: NullOrUndefined (String)
  }


newtype AvailabilityZonesList = AvailabilityZonesList (Array String)


newtype AwsQueryErrorMessage = AwsQueryErrorMessage String


newtype BooleanOptional = BooleanOptional Boolean


-- | <p>Contains all of the attributes of a specific cluster.</p>
newtype CacheCluster = CacheCluster 
  { "CacheClusterId" :: NullOrUndefined (String)
  , "ConfigurationEndpoint" :: NullOrUndefined (Endpoint)
  , "ClientDownloadLandingPage" :: NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined (String)
  , "Engine" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "CacheClusterStatus" :: NullOrUndefined (String)
  , "NumCacheNodes" :: NullOrUndefined (IntegerOptional)
  , "PreferredAvailabilityZone" :: NullOrUndefined (String)
  , "CacheClusterCreateTime" :: NullOrUndefined (TStamp)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "PendingModifiedValues" :: NullOrUndefined (PendingModifiedValues)
  , "NotificationConfiguration" :: NullOrUndefined (NotificationConfiguration)
  , "CacheSecurityGroups" :: NullOrUndefined (CacheSecurityGroupMembershipList)
  , "CacheParameterGroup" :: NullOrUndefined (CacheParameterGroupStatus)
  , "CacheSubnetGroupName" :: NullOrUndefined (String)
  , "CacheNodes" :: NullOrUndefined (CacheNodeList)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean)
  , "SecurityGroups" :: NullOrUndefined (SecurityGroupMembershipList)
  , "ReplicationGroupId" :: NullOrUndefined (String)
  , "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined (String)
  , "AuthTokenEnabled" :: NullOrUndefined (BooleanOptional)
  , "TransitEncryptionEnabled" :: NullOrUndefined (BooleanOptional)
  , "AtRestEncryptionEnabled" :: NullOrUndefined (BooleanOptional)
  }


-- | <p>You already have a cluster with the given identifier.</p>
newtype CacheClusterAlreadyExistsFault = CacheClusterAlreadyExistsFault 
  { 
  }


newtype CacheClusterList = CacheClusterList (Array CacheCluster)


-- | <p>Represents the output of a <code>DescribeCacheClusters</code> operation.</p>
newtype CacheClusterMessage = CacheClusterMessage 
  { "Marker" :: NullOrUndefined (String)
  , "CacheClusters" :: NullOrUndefined (CacheClusterList)
  }


-- | <p>The requested cluster ID does not refer to an existing cluster.</p>
newtype CacheClusterNotFoundFault = CacheClusterNotFoundFault 
  { 
  }


-- | <p>Provides all of the details about a particular cache engine version.</p>
newtype CacheEngineVersion = CacheEngineVersion 
  { "Engine" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "CacheParameterGroupFamily" :: NullOrUndefined (String)
  , "CacheEngineDescription" :: NullOrUndefined (String)
  , "CacheEngineVersionDescription" :: NullOrUndefined (String)
  }


newtype CacheEngineVersionList = CacheEngineVersionList (Array CacheEngineVersion)


-- | <p>Represents the output of a <a>DescribeCacheEngineVersions</a> operation.</p>
newtype CacheEngineVersionMessage = CacheEngineVersionMessage 
  { "Marker" :: NullOrUndefined (String)
  , "CacheEngineVersions" :: NullOrUndefined (CacheEngineVersionList)
  }


-- | <p>Represents an individual cache node within a cluster. Each cache node runs its own instance of the cluster's protocol-compliant caching software - either Memcached or Redis.</p> <p>The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.</p> <ul> <li> <p>General purpose:</p> <ul> <li> <p>Current generation: </p> <p> <b>T2 node types:</b> <code>cache.t2.micro</code>, <code>cache.t2.small</code>, <code>cache.t2.medium</code> </p> <p> <b>M3 node types:</b> <code>cache.m3.medium</code>, <code>cache.m3.large</code>, <code>cache.m3.xlarge</code>, <code>cache.m3.2xlarge</code> </p> <p> <b>M4 node types:</b> <code>cache.m4.large</code>, <code>cache.m4.xlarge</code>, <code>cache.m4.2xlarge</code>, <code>cache.m4.4xlarge</code>, <code>cache.m4.10xlarge</code> </p> </li> <li> <p>Previous generation: (not recommended)</p> <p> <b>T1 node types:</b> <code>cache.t1.micro</code> </p> <p> <b>M1 node types:</b> <code>cache.m1.small</code>, <code>cache.m1.medium</code>, <code>cache.m1.large</code>, <code>cache.m1.xlarge</code> </p> </li> </ul> </li> <li> <p>Compute optimized:</p> <ul> <li> <p>Previous generation: (not recommended)</p> <p> <b>C1 node types:</b> <code>cache.c1.xlarge</code> </p> </li> </ul> </li> <li> <p>Memory optimized:</p> <ul> <li> <p>Current generation: </p> <p> <b>R3 node types:</b> <code>cache.r3.large</code>, <code>cache.r3.xlarge</code>, <code>cache.r3.2xlarge</code>, <code>cache.r3.4xlarge</code>, <code>cache.r3.8xlarge</code> </p> </li> <li> <p>Previous generation: (not recommended)</p> <p> <b>M2 node types:</b> <code>cache.m2.xlarge</code>, <code>cache.m2.2xlarge</code>, <code>cache.m2.4xlarge</code> </p> </li> </ul> </li> </ul> <p> <b>Notes:</b> </p> <ul> <li> <p>All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).</p> </li> <li> <p>Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances. </p> </li> <li> <p>Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.</p> </li> <li> <p>Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances.</p> </li> </ul> <p>For a complete listing of node types and specifications, see <a href="http://aws.amazon.com/elasticache/details">Amazon ElastiCache Product Features and Details</a> and either <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific">Cache Node Type-Specific Parameters for Memcached</a> or <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific">Cache Node Type-Specific Parameters for Redis</a>.</p>
newtype CacheNode = CacheNode 
  { "CacheNodeId" :: NullOrUndefined (String)
  , "CacheNodeStatus" :: NullOrUndefined (String)
  , "CacheNodeCreateTime" :: NullOrUndefined (TStamp)
  , "Endpoint" :: NullOrUndefined (Endpoint)
  , "ParameterGroupStatus" :: NullOrUndefined (String)
  , "SourceCacheNodeId" :: NullOrUndefined (String)
  , "CustomerAvailabilityZone" :: NullOrUndefined (String)
  }


newtype CacheNodeIdsList = CacheNodeIdsList (Array String)


newtype CacheNodeList = CacheNodeList (Array CacheNode)


-- | <p>A parameter that has a different value for each cache node type it is applied to. For example, in a Redis cluster, a <code>cache.m1.large</code> cache node type would have a larger <code>maxmemory</code> value than a <code>cache.m1.small</code> type.</p>
newtype CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter 
  { "ParameterName" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "Source" :: NullOrUndefined (String)
  , "DataType" :: NullOrUndefined (String)
  , "AllowedValues" :: NullOrUndefined (String)
  , "IsModifiable" :: NullOrUndefined (Boolean)
  , "MinimumEngineVersion" :: NullOrUndefined (String)
  , "CacheNodeTypeSpecificValues" :: NullOrUndefined (CacheNodeTypeSpecificValueList)
  , "ChangeType" :: NullOrUndefined (ChangeType)
  }


newtype CacheNodeTypeSpecificParametersList = CacheNodeTypeSpecificParametersList (Array CacheNodeTypeSpecificParameter)


-- | <p>A value that applies only to a certain cache node type.</p>
newtype CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue 
  { "CacheNodeType" :: NullOrUndefined (String)
  , "Value" :: NullOrUndefined (String)
  }


newtype CacheNodeTypeSpecificValueList = CacheNodeTypeSpecificValueList (Array CacheNodeTypeSpecificValue)


-- | <p>Represents the output of a <code>CreateCacheParameterGroup</code> operation.</p>
newtype CacheParameterGroup = CacheParameterGroup 
  { "CacheParameterGroupName" :: NullOrUndefined (String)
  , "CacheParameterGroupFamily" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  }


-- | <p>A cache parameter group with the requested name already exists.</p>
newtype CacheParameterGroupAlreadyExistsFault = CacheParameterGroupAlreadyExistsFault 
  { 
  }


-- | <p>Represents the output of a <code>DescribeCacheParameters</code> operation.</p>
newtype CacheParameterGroupDetails = CacheParameterGroupDetails 
  { "Marker" :: NullOrUndefined (String)
  , "Parameters" :: NullOrUndefined (ParametersList)
  , "CacheNodeTypeSpecificParameters" :: NullOrUndefined (CacheNodeTypeSpecificParametersList)
  }


newtype CacheParameterGroupList = CacheParameterGroupList (Array CacheParameterGroup)


-- | <p>Represents the output of one of the following operations:</p> <ul> <li> <p> <code>ModifyCacheParameterGroup</code> </p> </li> <li> <p> <code>ResetCacheParameterGroup</code> </p> </li> </ul>
newtype CacheParameterGroupNameMessage = CacheParameterGroupNameMessage 
  { "CacheParameterGroupName" :: NullOrUndefined (String)
  }


-- | <p>The requested cache parameter group name does not refer to an existing cache parameter group.</p>
newtype CacheParameterGroupNotFoundFault = CacheParameterGroupNotFoundFault 
  { 
  }


-- | <p>The request cannot be processed because it would exceed the maximum number of cache security groups.</p>
newtype CacheParameterGroupQuotaExceededFault = CacheParameterGroupQuotaExceededFault 
  { 
  }


-- | <p>Status of the cache parameter group.</p>
newtype CacheParameterGroupStatus = CacheParameterGroupStatus 
  { "CacheParameterGroupName" :: NullOrUndefined (String)
  , "ParameterApplyStatus" :: NullOrUndefined (String)
  , "CacheNodeIdsToReboot" :: NullOrUndefined (CacheNodeIdsList)
  }


-- | <p>Represents the output of a <code>DescribeCacheParameterGroups</code> operation.</p>
newtype CacheParameterGroupsMessage = CacheParameterGroupsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "CacheParameterGroups" :: NullOrUndefined (CacheParameterGroupList)
  }


-- | <p>Represents the output of one of the following operations:</p> <ul> <li> <p> <code>AuthorizeCacheSecurityGroupIngress</code> </p> </li> <li> <p> <code>CreateCacheSecurityGroup</code> </p> </li> <li> <p> <code>RevokeCacheSecurityGroupIngress</code> </p> </li> </ul>
newtype CacheSecurityGroup = CacheSecurityGroup 
  { "OwnerId" :: NullOrUndefined (String)
  , "CacheSecurityGroupName" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "EC2SecurityGroups" :: NullOrUndefined (EC2SecurityGroupList)
  }


-- | <p>A cache security group with the specified name already exists.</p>
newtype CacheSecurityGroupAlreadyExistsFault = CacheSecurityGroupAlreadyExistsFault 
  { 
  }


-- | <p>Represents a cluster's status within a particular cache security group.</p>
newtype CacheSecurityGroupMembership = CacheSecurityGroupMembership 
  { "CacheSecurityGroupName" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }


newtype CacheSecurityGroupMembershipList = CacheSecurityGroupMembershipList (Array CacheSecurityGroupMembership)


-- | <p>Represents the output of a <code>DescribeCacheSecurityGroups</code> operation.</p>
newtype CacheSecurityGroupMessage = CacheSecurityGroupMessage 
  { "Marker" :: NullOrUndefined (String)
  , "CacheSecurityGroups" :: NullOrUndefined (CacheSecurityGroups)
  }


newtype CacheSecurityGroupNameList = CacheSecurityGroupNameList (Array String)


-- | <p>The requested cache security group name does not refer to an existing cache security group.</p>
newtype CacheSecurityGroupNotFoundFault = CacheSecurityGroupNotFoundFault 
  { 
  }


-- | <p>The request cannot be processed because it would exceed the allowed number of cache security groups.</p>
newtype CacheSecurityGroupQuotaExceededFault = CacheSecurityGroupQuotaExceededFault 
  { 
  }


newtype CacheSecurityGroups = CacheSecurityGroups (Array CacheSecurityGroup)


-- | <p>Represents the output of one of the following operations:</p> <ul> <li> <p> <code>CreateCacheSubnetGroup</code> </p> </li> <li> <p> <code>ModifyCacheSubnetGroup</code> </p> </li> </ul>
newtype CacheSubnetGroup = CacheSubnetGroup 
  { "CacheSubnetGroupName" :: NullOrUndefined (String)
  , "CacheSubnetGroupDescription" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "Subnets" :: NullOrUndefined (SubnetList)
  }


-- | <p>The requested cache subnet group name is already in use by an existing cache subnet group.</p>
newtype CacheSubnetGroupAlreadyExistsFault = CacheSubnetGroupAlreadyExistsFault 
  { 
  }


-- | <p>The requested cache subnet group is currently in use.</p>
newtype CacheSubnetGroupInUse = CacheSubnetGroupInUse 
  { 
  }


-- | <p>Represents the output of a <code>DescribeCacheSubnetGroups</code> operation.</p>
newtype CacheSubnetGroupMessage = CacheSubnetGroupMessage 
  { "Marker" :: NullOrUndefined (String)
  , "CacheSubnetGroups" :: NullOrUndefined (CacheSubnetGroups)
  }


-- | <p>The requested cache subnet group name does not refer to an existing cache subnet group.</p>
newtype CacheSubnetGroupNotFoundFault = CacheSubnetGroupNotFoundFault 
  { 
  }


-- | <p>The request cannot be processed because it would exceed the allowed number of cache subnet groups.</p>
newtype CacheSubnetGroupQuotaExceededFault = CacheSubnetGroupQuotaExceededFault 
  { 
  }


newtype CacheSubnetGroups = CacheSubnetGroups (Array CacheSubnetGroup)


-- | <p>The request cannot be processed because it would exceed the allowed number of subnets in a cache subnet group.</p>
newtype CacheSubnetQuotaExceededFault = CacheSubnetQuotaExceededFault 
  { 
  }


newtype ChangeType = ChangeType String


newtype ClusterIdList = ClusterIdList (Array String)


-- | <p>The request cannot be processed because it would exceed the allowed number of clusters per customer.</p>
newtype ClusterQuotaForCustomerExceededFault = ClusterQuotaForCustomerExceededFault 
  { 
  }


-- | <p>Represents the input of a <code>CopySnapshotMessage</code> operation.</p>
newtype CopySnapshotMessage = CopySnapshotMessage 
  { "SourceSnapshotName" :: (String)
  , "TargetSnapshotName" :: (String)
  , "TargetBucket" :: NullOrUndefined (String)
  }


newtype CopySnapshotResult = CopySnapshotResult 
  { "Snapshot" :: NullOrUndefined (Snapshot)
  }


-- | <p>Represents the input of a CreateCacheCluster operation.</p>
newtype CreateCacheClusterMessage = CreateCacheClusterMessage 
  { "CacheClusterId" :: (String)
  , "ReplicationGroupId" :: NullOrUndefined (String)
  , "AZMode" :: NullOrUndefined (AZMode)
  , "PreferredAvailabilityZone" :: NullOrUndefined (String)
  , "PreferredAvailabilityZones" :: NullOrUndefined (PreferredAvailabilityZoneList)
  , "NumCacheNodes" :: NullOrUndefined (IntegerOptional)
  , "CacheNodeType" :: NullOrUndefined (String)
  , "Engine" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "CacheParameterGroupName" :: NullOrUndefined (String)
  , "CacheSubnetGroupName" :: NullOrUndefined (String)
  , "CacheSecurityGroupNames" :: NullOrUndefined (CacheSecurityGroupNameList)
  , "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdsList)
  , "Tags" :: NullOrUndefined (TagList)
  , "SnapshotArns" :: NullOrUndefined (SnapshotArnsList)
  , "SnapshotName" :: NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "NotificationTopicArn" :: NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined (String)
  , "AuthToken" :: NullOrUndefined (String)
  }


newtype CreateCacheClusterResult = CreateCacheClusterResult 
  { "CacheCluster" :: NullOrUndefined (CacheCluster)
  }


-- | <p>Represents the input of a <code>CreateCacheParameterGroup</code> operation.</p>
newtype CreateCacheParameterGroupMessage = CreateCacheParameterGroupMessage 
  { "CacheParameterGroupName" :: (String)
  , "CacheParameterGroupFamily" :: (String)
  , "Description" :: (String)
  }


newtype CreateCacheParameterGroupResult = CreateCacheParameterGroupResult 
  { "CacheParameterGroup" :: NullOrUndefined (CacheParameterGroup)
  }


-- | <p>Represents the input of a <code>CreateCacheSecurityGroup</code> operation.</p>
newtype CreateCacheSecurityGroupMessage = CreateCacheSecurityGroupMessage 
  { "CacheSecurityGroupName" :: (String)
  , "Description" :: (String)
  }


newtype CreateCacheSecurityGroupResult = CreateCacheSecurityGroupResult 
  { "CacheSecurityGroup" :: NullOrUndefined (CacheSecurityGroup)
  }


-- | <p>Represents the input of a <code>CreateCacheSubnetGroup</code> operation.</p>
newtype CreateCacheSubnetGroupMessage = CreateCacheSubnetGroupMessage 
  { "CacheSubnetGroupName" :: (String)
  , "CacheSubnetGroupDescription" :: (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  }


newtype CreateCacheSubnetGroupResult = CreateCacheSubnetGroupResult 
  { "CacheSubnetGroup" :: NullOrUndefined (CacheSubnetGroup)
  }


-- | <p>Represents the input of a <code>CreateReplicationGroup</code> operation.</p>
newtype CreateReplicationGroupMessage = CreateReplicationGroupMessage 
  { "ReplicationGroupId" :: (String)
  , "ReplicationGroupDescription" :: (String)
  , "PrimaryClusterId" :: NullOrUndefined (String)
  , "AutomaticFailoverEnabled" :: NullOrUndefined (BooleanOptional)
  , "NumCacheClusters" :: NullOrUndefined (IntegerOptional)
  , "PreferredCacheClusterAZs" :: NullOrUndefined (AvailabilityZonesList)
  , "NumNodeGroups" :: NullOrUndefined (IntegerOptional)
  , "ReplicasPerNodeGroup" :: NullOrUndefined (IntegerOptional)
  , "NodeGroupConfiguration" :: NullOrUndefined (NodeGroupConfigurationList)
  , "CacheNodeType" :: NullOrUndefined (String)
  , "Engine" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "CacheParameterGroupName" :: NullOrUndefined (String)
  , "CacheSubnetGroupName" :: NullOrUndefined (String)
  , "CacheSecurityGroupNames" :: NullOrUndefined (CacheSecurityGroupNameList)
  , "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdsList)
  , "Tags" :: NullOrUndefined (TagList)
  , "SnapshotArns" :: NullOrUndefined (SnapshotArnsList)
  , "SnapshotName" :: NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "NotificationTopicArn" :: NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined (String)
  , "AuthToken" :: NullOrUndefined (String)
  , "TransitEncryptionEnabled" :: NullOrUndefined (BooleanOptional)
  , "AtRestEncryptionEnabled" :: NullOrUndefined (BooleanOptional)
  }


newtype CreateReplicationGroupResult = CreateReplicationGroupResult 
  { "ReplicationGroup" :: NullOrUndefined (ReplicationGroup)
  }


-- | <p>Represents the input of a <code>CreateSnapshot</code> operation.</p>
newtype CreateSnapshotMessage = CreateSnapshotMessage 
  { "ReplicationGroupId" :: NullOrUndefined (String)
  , "CacheClusterId" :: NullOrUndefined (String)
  , "SnapshotName" :: (String)
  }


newtype CreateSnapshotResult = CreateSnapshotResult 
  { "Snapshot" :: NullOrUndefined (Snapshot)
  }


-- | <p>Represents the input of a <code>DeleteCacheCluster</code> operation.</p>
newtype DeleteCacheClusterMessage = DeleteCacheClusterMessage 
  { "CacheClusterId" :: (String)
  , "FinalSnapshotIdentifier" :: NullOrUndefined (String)
  }


newtype DeleteCacheClusterResult = DeleteCacheClusterResult 
  { "CacheCluster" :: NullOrUndefined (CacheCluster)
  }


-- | <p>Represents the input of a <code>DeleteCacheParameterGroup</code> operation.</p>
newtype DeleteCacheParameterGroupMessage = DeleteCacheParameterGroupMessage 
  { "CacheParameterGroupName" :: (String)
  }


-- | <p>Represents the input of a <code>DeleteCacheSecurityGroup</code> operation.</p>
newtype DeleteCacheSecurityGroupMessage = DeleteCacheSecurityGroupMessage 
  { "CacheSecurityGroupName" :: (String)
  }


-- | <p>Represents the input of a <code>DeleteCacheSubnetGroup</code> operation.</p>
newtype DeleteCacheSubnetGroupMessage = DeleteCacheSubnetGroupMessage 
  { "CacheSubnetGroupName" :: (String)
  }


-- | <p>Represents the input of a <code>DeleteReplicationGroup</code> operation.</p>
newtype DeleteReplicationGroupMessage = DeleteReplicationGroupMessage 
  { "ReplicationGroupId" :: (String)
  , "RetainPrimaryCluster" :: NullOrUndefined (BooleanOptional)
  , "FinalSnapshotIdentifier" :: NullOrUndefined (String)
  }


newtype DeleteReplicationGroupResult = DeleteReplicationGroupResult 
  { "ReplicationGroup" :: NullOrUndefined (ReplicationGroup)
  }


-- | <p>Represents the input of a <code>DeleteSnapshot</code> operation.</p>
newtype DeleteSnapshotMessage = DeleteSnapshotMessage 
  { "SnapshotName" :: (String)
  }


newtype DeleteSnapshotResult = DeleteSnapshotResult 
  { "Snapshot" :: NullOrUndefined (Snapshot)
  }


-- | <p>Represents the input of a <code>DescribeCacheClusters</code> operation.</p>
newtype DescribeCacheClustersMessage = DescribeCacheClustersMessage 
  { "CacheClusterId" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "ShowCacheNodeInfo" :: NullOrUndefined (BooleanOptional)
  , "ShowCacheClustersNotInReplicationGroups" :: NullOrUndefined (BooleanOptional)
  }


-- | <p>Represents the input of a <code>DescribeCacheEngineVersions</code> operation.</p>
newtype DescribeCacheEngineVersionsMessage = DescribeCacheEngineVersionsMessage 
  { "Engine" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "CacheParameterGroupFamily" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "DefaultOnly" :: NullOrUndefined (Boolean)
  }


-- | <p>Represents the input of a <code>DescribeCacheParameterGroups</code> operation.</p>
newtype DescribeCacheParameterGroupsMessage = DescribeCacheParameterGroupsMessage 
  { "CacheParameterGroupName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>Represents the input of a <code>DescribeCacheParameters</code> operation.</p>
newtype DescribeCacheParametersMessage = DescribeCacheParametersMessage 
  { "CacheParameterGroupName" :: (String)
  , "Source" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>Represents the input of a <code>DescribeCacheSecurityGroups</code> operation.</p>
newtype DescribeCacheSecurityGroupsMessage = DescribeCacheSecurityGroupsMessage 
  { "CacheSecurityGroupName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>Represents the input of a <code>DescribeCacheSubnetGroups</code> operation.</p>
newtype DescribeCacheSubnetGroupsMessage = DescribeCacheSubnetGroupsMessage 
  { "CacheSubnetGroupName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>Represents the input of a <code>DescribeEngineDefaultParameters</code> operation.</p>
newtype DescribeEngineDefaultParametersMessage = DescribeEngineDefaultParametersMessage 
  { "CacheParameterGroupFamily" :: (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeEngineDefaultParametersResult = DescribeEngineDefaultParametersResult 
  { "EngineDefaults" :: NullOrUndefined (EngineDefaults)
  }


-- | <p>Represents the input of a <code>DescribeEvents</code> operation.</p>
newtype DescribeEventsMessage = DescribeEventsMessage 
  { "SourceIdentifier" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (SourceType)
  , "StartTime" :: NullOrUndefined (TStamp)
  , "EndTime" :: NullOrUndefined (TStamp)
  , "Duration" :: NullOrUndefined (IntegerOptional)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>Represents the input of a <code>DescribeReplicationGroups</code> operation.</p>
newtype DescribeReplicationGroupsMessage = DescribeReplicationGroupsMessage 
  { "ReplicationGroupId" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>Represents the input of a <code>DescribeReservedCacheNodes</code> operation.</p>
newtype DescribeReservedCacheNodesMessage = DescribeReservedCacheNodesMessage 
  { "ReservedCacheNodeId" :: NullOrUndefined (String)
  , "ReservedCacheNodesOfferingId" :: NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined (String)
  , "Duration" :: NullOrUndefined (String)
  , "ProductDescription" :: NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>Represents the input of a <code>DescribeReservedCacheNodesOfferings</code> operation.</p>
newtype DescribeReservedCacheNodesOfferingsMessage = DescribeReservedCacheNodesOfferingsMessage 
  { "ReservedCacheNodesOfferingId" :: NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined (String)
  , "Duration" :: NullOrUndefined (String)
  , "ProductDescription" :: NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>Represents the output of a <code>DescribeSnapshots</code> operation.</p>
newtype DescribeSnapshotsListMessage = DescribeSnapshotsListMessage 
  { "Marker" :: NullOrUndefined (String)
  , "Snapshots" :: NullOrUndefined (SnapshotList)
  }


-- | <p>Represents the input of a <code>DescribeSnapshotsMessage</code> operation.</p>
newtype DescribeSnapshotsMessage = DescribeSnapshotsMessage 
  { "ReplicationGroupId" :: NullOrUndefined (String)
  , "CacheClusterId" :: NullOrUndefined (String)
  , "SnapshotName" :: NullOrUndefined (String)
  , "SnapshotSource" :: NullOrUndefined (String)
  , "Marker" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "ShowNodeGroupConfig" :: NullOrUndefined (BooleanOptional)
  }


-- | <p>Provides ownership and status information for an Amazon EC2 security group.</p>
newtype EC2SecurityGroup = EC2SecurityGroup 
  { "Status" :: NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined (String)
  }


newtype EC2SecurityGroupList = EC2SecurityGroupList (Array EC2SecurityGroup)


-- | <p>Represents the information required for client programs to connect to a cache node.</p>
newtype Endpoint = Endpoint 
  { "Address" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (Int)
  }


-- | <p>Represents the output of a <code>DescribeEngineDefaultParameters</code> operation.</p>
newtype EngineDefaults = EngineDefaults 
  { "CacheParameterGroupFamily" :: NullOrUndefined (String)
  , "Marker" :: NullOrUndefined (String)
  , "Parameters" :: NullOrUndefined (ParametersList)
  , "CacheNodeTypeSpecificParameters" :: NullOrUndefined (CacheNodeTypeSpecificParametersList)
  }


-- | <p>Represents a single occurrence of something interesting within the system. Some examples of events are creating a cluster, adding or removing a cache node, or rebooting a node.</p>
newtype Event = Event 
  { "SourceIdentifier" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (SourceType)
  , "Message" :: NullOrUndefined (String)
  , "Date" :: NullOrUndefined (TStamp)
  }


newtype EventList = EventList (Array Event)


-- | <p>Represents the output of a <code>DescribeEvents</code> operation.</p>
newtype EventsMessage = EventsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "Events" :: NullOrUndefined (EventList)
  }


-- | <p>The requested cache node type is not available in the specified Availability Zone.</p>
newtype InsufficientCacheClusterCapacityFault = InsufficientCacheClusterCapacityFault 
  { 
  }


newtype IntegerOptional = IntegerOptional Int


-- | <p>The requested Amazon Resource Name (ARN) does not refer to an existing resource.</p>
newtype InvalidARNFault = InvalidARNFault 
  { 
  }


-- | <p>The requested cluster is not in the <code>available</code> state.</p>
newtype InvalidCacheClusterStateFault = InvalidCacheClusterStateFault 
  { 
  }


-- | <p>The current state of the cache parameter group does not allow the requested operation to occur.</p>
newtype InvalidCacheParameterGroupStateFault = InvalidCacheParameterGroupStateFault 
  { 
  }


-- | <p>The current state of the cache security group does not allow deletion.</p>
newtype InvalidCacheSecurityGroupStateFault = InvalidCacheSecurityGroupStateFault 
  { 
  }


-- | <p>Two or more incompatible parameters were specified.</p>
newtype InvalidParameterCombinationException = InvalidParameterCombinationException 
  { "Message'" :: NullOrUndefined (AwsQueryErrorMessage)
  }


-- | <p>The value for a parameter is invalid.</p>
newtype InvalidParameterValueException = InvalidParameterValueException 
  { "Message'" :: NullOrUndefined (AwsQueryErrorMessage)
  }


-- | <p>The requested replication group is not in the <code>available</code> state.</p>
newtype InvalidReplicationGroupStateFault = InvalidReplicationGroupStateFault 
  { 
  }


-- | <p>The current state of the snapshot does not allow the requested operation to occur.</p>
newtype InvalidSnapshotStateFault = InvalidSnapshotStateFault 
  { 
  }


-- | <p>An invalid subnet identifier was specified.</p>
newtype InvalidSubnet = InvalidSubnet 
  { 
  }


-- | <p>The VPC network is in an invalid state.</p>
newtype InvalidVPCNetworkStateFault = InvalidVPCNetworkStateFault 
  { 
  }


newtype KeyList = KeyList (Array String)


-- | <p>The input parameters for the <code>ListAllowedNodeTypeModifications</code> operation.</p>
newtype ListAllowedNodeTypeModificationsMessage = ListAllowedNodeTypeModificationsMessage 
  { "CacheClusterId" :: NullOrUndefined (String)
  , "ReplicationGroupId" :: NullOrUndefined (String)
  }


-- | <p>The input parameters for the <code>ListTagsForResource</code> operation.</p>
newtype ListTagsForResourceMessage = ListTagsForResourceMessage 
  { "ResourceName" :: (String)
  }


-- | <p>Represents the input of a <code>ModifyCacheCluster</code> operation.</p>
newtype ModifyCacheClusterMessage = ModifyCacheClusterMessage 
  { "CacheClusterId" :: (String)
  , "NumCacheNodes" :: NullOrUndefined (IntegerOptional)
  , "CacheNodeIdsToRemove" :: NullOrUndefined (CacheNodeIdsList)
  , "AZMode" :: NullOrUndefined (AZMode)
  , "NewAvailabilityZones" :: NullOrUndefined (PreferredAvailabilityZoneList)
  , "CacheSecurityGroupNames" :: NullOrUndefined (CacheSecurityGroupNameList)
  , "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdsList)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "NotificationTopicArn" :: NullOrUndefined (String)
  , "CacheParameterGroupName" :: NullOrUndefined (String)
  , "NotificationTopicStatus" :: NullOrUndefined (String)
  , "ApplyImmediately" :: NullOrUndefined (Boolean)
  , "EngineVersion" :: NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined (String)
  }


newtype ModifyCacheClusterResult = ModifyCacheClusterResult 
  { "CacheCluster" :: NullOrUndefined (CacheCluster)
  }


-- | <p>Represents the input of a <code>ModifyCacheParameterGroup</code> operation.</p>
newtype ModifyCacheParameterGroupMessage = ModifyCacheParameterGroupMessage 
  { "CacheParameterGroupName" :: (String)
  , "ParameterNameValues" :: (ParameterNameValueList)
  }


-- | <p>Represents the input of a <code>ModifyCacheSubnetGroup</code> operation.</p>
newtype ModifyCacheSubnetGroupMessage = ModifyCacheSubnetGroupMessage 
  { "CacheSubnetGroupName" :: (String)
  , "CacheSubnetGroupDescription" :: NullOrUndefined (String)
  , "SubnetIds" :: NullOrUndefined (SubnetIdentifierList)
  }


newtype ModifyCacheSubnetGroupResult = ModifyCacheSubnetGroupResult 
  { "CacheSubnetGroup" :: NullOrUndefined (CacheSubnetGroup)
  }


-- | <p>Represents the input of a <code>ModifyReplicationGroups</code> operation.</p>
newtype ModifyReplicationGroupMessage = ModifyReplicationGroupMessage 
  { "ReplicationGroupId" :: (String)
  , "ReplicationGroupDescription" :: NullOrUndefined (String)
  , "PrimaryClusterId" :: NullOrUndefined (String)
  , "SnapshottingClusterId" :: NullOrUndefined (String)
  , "AutomaticFailoverEnabled" :: NullOrUndefined (BooleanOptional)
  , "CacheSecurityGroupNames" :: NullOrUndefined (CacheSecurityGroupNameList)
  , "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdsList)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "NotificationTopicArn" :: NullOrUndefined (String)
  , "CacheParameterGroupName" :: NullOrUndefined (String)
  , "NotificationTopicStatus" :: NullOrUndefined (String)
  , "ApplyImmediately" :: NullOrUndefined (Boolean)
  , "EngineVersion" :: NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined (String)
  , "NodeGroupId" :: NullOrUndefined (String)
  }


newtype ModifyReplicationGroupResult = ModifyReplicationGroupResult 
  { "ReplicationGroup" :: NullOrUndefined (ReplicationGroup)
  }


-- | <p>Represents the input for a <code>ModifyReplicationGroupShardConfiguration</code> operation.</p>
newtype ModifyReplicationGroupShardConfigurationMessage = ModifyReplicationGroupShardConfigurationMessage 
  { "ReplicationGroupId" :: (String)
  , "NodeGroupCount" :: (Int)
  , "ApplyImmediately" :: (Boolean)
  , "ReshardingConfiguration" :: NullOrUndefined (ReshardingConfigurationList)
  , "NodeGroupsToRemove" :: NullOrUndefined (NodeGroupsToRemoveList)
  }


newtype ModifyReplicationGroupShardConfigurationResult = ModifyReplicationGroupShardConfigurationResult 
  { "ReplicationGroup" :: NullOrUndefined (ReplicationGroup)
  }


-- | <p>Represents a collection of cache nodes in a replication group. One node in the node group is the read/write primary node. All the other nodes are read-only Replica nodes.</p>
newtype NodeGroup = NodeGroup 
  { "NodeGroupId" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  , "PrimaryEndpoint" :: NullOrUndefined (Endpoint)
  , "Slots" :: NullOrUndefined (String)
  , "NodeGroupMembers" :: NullOrUndefined (NodeGroupMemberList)
  }


-- | <p>Node group (shard) configuration options. Each node group (shard) configuration has the following: <code>Slots</code>, <code>PrimaryAvailabilityZone</code>, <code>ReplicaAvailabilityZones</code>, <code>ReplicaCount</code>.</p>
newtype NodeGroupConfiguration = NodeGroupConfiguration 
  { "Slots" :: NullOrUndefined (String)
  , "ReplicaCount" :: NullOrUndefined (IntegerOptional)
  , "PrimaryAvailabilityZone" :: NullOrUndefined (String)
  , "ReplicaAvailabilityZones" :: NullOrUndefined (AvailabilityZonesList)
  }


newtype NodeGroupConfigurationList = NodeGroupConfigurationList (Array NodeGroupConfiguration)


newtype NodeGroupList = NodeGroupList (Array NodeGroup)


-- | <p>Represents a single node within a node group (shard).</p>
newtype NodeGroupMember = NodeGroupMember 
  { "CacheClusterId" :: NullOrUndefined (String)
  , "CacheNodeId" :: NullOrUndefined (String)
  , "ReadEndpoint" :: NullOrUndefined (Endpoint)
  , "PreferredAvailabilityZone" :: NullOrUndefined (String)
  , "CurrentRole" :: NullOrUndefined (String)
  }


newtype NodeGroupMemberList = NodeGroupMemberList (Array NodeGroupMember)


-- | <p>The node group specified by the <code>NodeGroupId</code> parameter could not be found. Please verify that the node group exists and that you spelled the <code>NodeGroupId</code> value correctly.</p>
newtype NodeGroupNotFoundFault = NodeGroupNotFoundFault 
  { 
  }


-- | <p>The request cannot be processed because it would exceed the maximum allowed number of node groups (shards) in a single replication group. The default maximum is 15</p>
newtype NodeGroupsPerReplicationGroupQuotaExceededFault = NodeGroupsPerReplicationGroupQuotaExceededFault 
  { 
  }


newtype NodeGroupsToRemoveList = NodeGroupsToRemoveList (Array String)


-- | <p>The request cannot be processed because it would exceed the allowed number of cache nodes in a single cluster.</p>
newtype NodeQuotaForClusterExceededFault = NodeQuotaForClusterExceededFault 
  { 
  }


-- | <p>The request cannot be processed because it would exceed the allowed number of cache nodes per customer.</p>
newtype NodeQuotaForCustomerExceededFault = NodeQuotaForCustomerExceededFault 
  { 
  }


-- | <p>Represents an individual cache node in a snapshot of a cluster.</p>
newtype NodeSnapshot = NodeSnapshot 
  { "CacheClusterId" :: NullOrUndefined (String)
  , "NodeGroupId" :: NullOrUndefined (String)
  , "CacheNodeId" :: NullOrUndefined (String)
  , "NodeGroupConfiguration" :: NullOrUndefined (NodeGroupConfiguration)
  , "CacheSize" :: NullOrUndefined (String)
  , "CacheNodeCreateTime" :: NullOrUndefined (TStamp)
  , "SnapshotCreateTime" :: NullOrUndefined (TStamp)
  }


newtype NodeSnapshotList = NodeSnapshotList (Array NodeSnapshot)


newtype NodeTypeList = NodeTypeList (Array String)


-- | <p>Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS).</p>
newtype NotificationConfiguration = NotificationConfiguration 
  { "TopicArn" :: NullOrUndefined (String)
  , "TopicStatus" :: NullOrUndefined (String)
  }


-- | <p>Describes an individual setting that controls some aspect of ElastiCache behavior.</p>
newtype Parameter = Parameter 
  { "ParameterName" :: NullOrUndefined (String)
  , "ParameterValue" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "Source" :: NullOrUndefined (String)
  , "DataType" :: NullOrUndefined (String)
  , "AllowedValues" :: NullOrUndefined (String)
  , "IsModifiable" :: NullOrUndefined (Boolean)
  , "MinimumEngineVersion" :: NullOrUndefined (String)
  , "ChangeType" :: NullOrUndefined (ChangeType)
  }


-- | <p>Describes a name-value pair that is used to update the value of a parameter.</p>
newtype ParameterNameValue = ParameterNameValue 
  { "ParameterName" :: NullOrUndefined (String)
  , "ParameterValue" :: NullOrUndefined (String)
  }


newtype ParameterNameValueList = ParameterNameValueList (Array ParameterNameValue)


newtype ParametersList = ParametersList (Array Parameter)


newtype PendingAutomaticFailoverStatus = PendingAutomaticFailoverStatus String


-- | <p>A group of settings that are applied to the cluster in the future, or that are currently being applied.</p>
newtype PendingModifiedValues = PendingModifiedValues 
  { "NumCacheNodes" :: NullOrUndefined (IntegerOptional)
  , "CacheNodeIdsToRemove" :: NullOrUndefined (CacheNodeIdsList)
  , "EngineVersion" :: NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined (String)
  }


newtype PreferredAvailabilityZoneList = PreferredAvailabilityZoneList (Array String)


-- | <p>Represents the input of a <code>PurchaseReservedCacheNodesOffering</code> operation.</p>
newtype PurchaseReservedCacheNodesOfferingMessage = PurchaseReservedCacheNodesOfferingMessage 
  { "ReservedCacheNodesOfferingId" :: (String)
  , "ReservedCacheNodeId" :: NullOrUndefined (String)
  , "CacheNodeCount" :: NullOrUndefined (IntegerOptional)
  }


newtype PurchaseReservedCacheNodesOfferingResult = PurchaseReservedCacheNodesOfferingResult 
  { "ReservedCacheNode" :: NullOrUndefined (ReservedCacheNode)
  }


-- | <p>Represents the input of a <code>RebootCacheCluster</code> operation.</p>
newtype RebootCacheClusterMessage = RebootCacheClusterMessage 
  { "CacheClusterId" :: (String)
  , "CacheNodeIdsToReboot" :: (CacheNodeIdsList)
  }


newtype RebootCacheClusterResult = RebootCacheClusterResult 
  { "CacheCluster" :: NullOrUndefined (CacheCluster)
  }


-- | <p>Contains the specific price and frequency of a recurring charges for a reserved cache node, or for a reserved cache node offering.</p>
newtype RecurringCharge = RecurringCharge 
  { "RecurringChargeAmount" :: NullOrUndefined (Number)
  , "RecurringChargeFrequency" :: NullOrUndefined (String)
  }


newtype RecurringChargeList = RecurringChargeList (Array RecurringCharge)


-- | <p>Represents the input of a <code>RemoveTagsFromResource</code> operation.</p>
newtype RemoveTagsFromResourceMessage = RemoveTagsFromResourceMessage 
  { "ResourceName" :: (String)
  , "TagKeys" :: (KeyList)
  }


-- | <p>Contains all of the attributes of a specific Redis replication group.</p>
newtype ReplicationGroup = ReplicationGroup 
  { "ReplicationGroupId" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  , "PendingModifiedValues" :: NullOrUndefined (ReplicationGroupPendingModifiedValues)
  , "MemberClusters" :: NullOrUndefined (ClusterIdList)
  , "NodeGroups" :: NullOrUndefined (NodeGroupList)
  , "SnapshottingClusterId" :: NullOrUndefined (String)
  , "AutomaticFailover" :: NullOrUndefined (AutomaticFailoverStatus)
  , "ConfigurationEndpoint" :: NullOrUndefined (Endpoint)
  , "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined (String)
  , "ClusterEnabled" :: NullOrUndefined (BooleanOptional)
  , "CacheNodeType" :: NullOrUndefined (String)
  , "AuthTokenEnabled" :: NullOrUndefined (BooleanOptional)
  , "TransitEncryptionEnabled" :: NullOrUndefined (BooleanOptional)
  , "AtRestEncryptionEnabled" :: NullOrUndefined (BooleanOptional)
  }


-- | <p>The specified replication group already exists.</p>
newtype ReplicationGroupAlreadyExistsFault = ReplicationGroupAlreadyExistsFault 
  { 
  }


newtype ReplicationGroupList = ReplicationGroupList (Array ReplicationGroup)


-- | <p>Represents the output of a <code>DescribeReplicationGroups</code> operation.</p>
newtype ReplicationGroupMessage = ReplicationGroupMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ReplicationGroups" :: NullOrUndefined (ReplicationGroupList)
  }


-- | <p>The specified replication group does not exist.</p>
newtype ReplicationGroupNotFoundFault = ReplicationGroupNotFoundFault 
  { 
  }


-- | <p>The settings to be applied to the Redis replication group, either immediately or during the next maintenance window.</p>
newtype ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues 
  { "PrimaryClusterId" :: NullOrUndefined (String)
  , "AutomaticFailoverStatus" :: NullOrUndefined (PendingAutomaticFailoverStatus)
  , "Resharding" :: NullOrUndefined (ReshardingStatus)
  }


-- | <p>Represents the output of a <code>PurchaseReservedCacheNodesOffering</code> operation.</p>
newtype ReservedCacheNode = ReservedCacheNode 
  { "ReservedCacheNodeId" :: NullOrUndefined (String)
  , "ReservedCacheNodesOfferingId" :: NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined (String)
  , "StartTime" :: NullOrUndefined (TStamp)
  , "Duration" :: NullOrUndefined (Int)
  , "FixedPrice" :: NullOrUndefined (Number)
  , "UsagePrice" :: NullOrUndefined (Number)
  , "CacheNodeCount" :: NullOrUndefined (Int)
  , "ProductDescription" :: NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (String)
  , "RecurringCharges" :: NullOrUndefined (RecurringChargeList)
  }


-- | <p>You already have a reservation with the given identifier.</p>
newtype ReservedCacheNodeAlreadyExistsFault = ReservedCacheNodeAlreadyExistsFault 
  { 
  }


newtype ReservedCacheNodeList = ReservedCacheNodeList (Array ReservedCacheNode)


-- | <p>Represents the output of a <code>DescribeReservedCacheNodes</code> operation.</p>
newtype ReservedCacheNodeMessage = ReservedCacheNodeMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ReservedCacheNodes" :: NullOrUndefined (ReservedCacheNodeList)
  }


-- | <p>The requested reserved cache node was not found.</p>
newtype ReservedCacheNodeNotFoundFault = ReservedCacheNodeNotFoundFault 
  { 
  }


-- | <p>The request cannot be processed because it would exceed the user's cache node quota.</p>
newtype ReservedCacheNodeQuotaExceededFault = ReservedCacheNodeQuotaExceededFault 
  { 
  }


-- | <p>Describes all of the attributes of a reserved cache node offering.</p>
newtype ReservedCacheNodesOffering = ReservedCacheNodesOffering 
  { "ReservedCacheNodesOfferingId" :: NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined (String)
  , "Duration" :: NullOrUndefined (Int)
  , "FixedPrice" :: NullOrUndefined (Number)
  , "UsagePrice" :: NullOrUndefined (Number)
  , "ProductDescription" :: NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined (String)
  , "RecurringCharges" :: NullOrUndefined (RecurringChargeList)
  }


newtype ReservedCacheNodesOfferingList = ReservedCacheNodesOfferingList (Array ReservedCacheNodesOffering)


-- | <p>Represents the output of a <code>DescribeReservedCacheNodesOfferings</code> operation.</p>
newtype ReservedCacheNodesOfferingMessage = ReservedCacheNodesOfferingMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ReservedCacheNodesOfferings" :: NullOrUndefined (ReservedCacheNodesOfferingList)
  }


-- | <p>The requested cache node offering does not exist.</p>
newtype ReservedCacheNodesOfferingNotFoundFault = ReservedCacheNodesOfferingNotFoundFault 
  { 
  }


-- | <p>Represents the input of a <code>ResetCacheParameterGroup</code> operation.</p>
newtype ResetCacheParameterGroupMessage = ResetCacheParameterGroupMessage 
  { "CacheParameterGroupName" :: (String)
  , "ResetAllParameters" :: NullOrUndefined (Boolean)
  , "ParameterNameValues" :: NullOrUndefined (ParameterNameValueList)
  }


-- | <p>A list of <code>PreferredAvailabilityZones</code> objects that specifies the configuration of a node group in the resharded cluster.</p>
newtype ReshardingConfiguration = ReshardingConfiguration 
  { "PreferredAvailabilityZones" :: NullOrUndefined (AvailabilityZonesList)
  }


newtype ReshardingConfigurationList = ReshardingConfigurationList (Array ReshardingConfiguration)


-- | <p>The status of an online resharding operation.</p>
newtype ReshardingStatus = ReshardingStatus 
  { "SlotMigration" :: NullOrUndefined (SlotMigration)
  }


-- | <p>Represents the input of a <code>RevokeCacheSecurityGroupIngress</code> operation.</p>
newtype RevokeCacheSecurityGroupIngressMessage = RevokeCacheSecurityGroupIngressMessage 
  { "CacheSecurityGroupName" :: (String)
  , "EC2SecurityGroupName" :: (String)
  , "EC2SecurityGroupOwnerId" :: (String)
  }


newtype RevokeCacheSecurityGroupIngressResult = RevokeCacheSecurityGroupIngressResult 
  { "CacheSecurityGroup" :: NullOrUndefined (CacheSecurityGroup)
  }


newtype SecurityGroupIdsList = SecurityGroupIdsList (Array String)


-- | <p>Represents a single cache security group and its status.</p>
newtype SecurityGroupMembership = SecurityGroupMembership 
  { "SecurityGroupId" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }


newtype SecurityGroupMembershipList = SecurityGroupMembershipList (Array SecurityGroupMembership)


-- | <p>Represents the progress of an online resharding operation.</p>
newtype SlotMigration = SlotMigration 
  { "ProgressPercentage" :: NullOrUndefined (Number)
  }


-- | <p>Represents a copy of an entire Redis cluster as of the time when the snapshot was taken.</p>
newtype Snapshot = Snapshot 
  { "SnapshotName" :: NullOrUndefined (String)
  , "ReplicationGroupId" :: NullOrUndefined (String)
  , "ReplicationGroupDescription" :: NullOrUndefined (String)
  , "CacheClusterId" :: NullOrUndefined (String)
  , "SnapshotStatus" :: NullOrUndefined (String)
  , "SnapshotSource" :: NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined (String)
  , "Engine" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "NumCacheNodes" :: NullOrUndefined (IntegerOptional)
  , "PreferredAvailabilityZone" :: NullOrUndefined (String)
  , "CacheClusterCreateTime" :: NullOrUndefined (TStamp)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "TopicArn" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "CacheParameterGroupName" :: NullOrUndefined (String)
  , "CacheSubnetGroupName" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean)
  , "SnapshotRetentionLimit" :: NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined (String)
  , "NumNodeGroups" :: NullOrUndefined (IntegerOptional)
  , "AutomaticFailover" :: NullOrUndefined (AutomaticFailoverStatus)
  , "NodeSnapshots" :: NullOrUndefined (NodeSnapshotList)
  }


-- | <p>You already have a snapshot with the given name.</p>
newtype SnapshotAlreadyExistsFault = SnapshotAlreadyExistsFault 
  { 
  }


newtype SnapshotArnsList = SnapshotArnsList (Array String)


-- | <p>You attempted one of the following operations:</p> <ul> <li> <p>Creating a snapshot of a Redis cluster running on a <code>cache.t1.micro</code> cache node.</p> </li> <li> <p>Creating a snapshot of a cluster that is running Memcached rather than Redis.</p> </li> </ul> <p>Neither of these are supported by ElastiCache.</p>
newtype SnapshotFeatureNotSupportedFault = SnapshotFeatureNotSupportedFault 
  { 
  }


newtype SnapshotList = SnapshotList (Array Snapshot)


-- | <p>The requested snapshot name does not refer to an existing snapshot.</p>
newtype SnapshotNotFoundFault = SnapshotNotFoundFault 
  { 
  }


-- | <p>The request cannot be processed because it would exceed the maximum number of snapshots.</p>
newtype SnapshotQuotaExceededFault = SnapshotQuotaExceededFault 
  { 
  }


newtype SourceType = SourceType String


-- | <p>Represents the subnet associated with a cluster. This parameter refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used with ElastiCache.</p>
newtype Subnet = Subnet 
  { "SubnetIdentifier" :: NullOrUndefined (String)
  , "SubnetAvailabilityZone" :: NullOrUndefined (AvailabilityZone)
  }


newtype SubnetIdentifierList = SubnetIdentifierList (Array String)


-- | <p>The requested subnet is being used by another cache subnet group.</p>
newtype SubnetInUse = SubnetInUse 
  { 
  }


newtype SubnetList = SubnetList (Array Subnet)


newtype TStamp = TStamp Number


-- | <p>A cost allocation Tag that can be added to an ElastiCache cluster or replication group. Tags are composed of a Key/Value pair. A tag with a null Value is permitted.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (String)
  , "Value" :: NullOrUndefined (String)
  }


newtype TagList = TagList (Array Tag)


-- | <p>Represents the output from the <code>AddTagsToResource</code>, <code>ListTagsForResource</code>, and <code>RemoveTagsFromResource</code> operations.</p>
newtype TagListMessage = TagListMessage 
  { "TagList" :: NullOrUndefined (TagList)
  }


-- | <p>The requested tag was not found on this resource.</p>
newtype TagNotFoundFault = TagNotFoundFault 
  { 
  }


-- | <p>The request cannot be processed because it would cause the resource to have more than the allowed number of tags. The maximum number of tags permitted on a resource is 50.</p>
newtype TagQuotaPerResourceExceeded = TagQuotaPerResourceExceeded 
  { 
  }


newtype TestFailoverMessage = TestFailoverMessage 
  { "ReplicationGroupId" :: (String)
  , "NodeGroupId" :: (String)
  }


newtype TestFailoverNotAvailableFault = TestFailoverNotAvailableFault 
  { 
  }


newtype TestFailoverResult = TestFailoverResult 
  { "ReplicationGroup" :: NullOrUndefined (ReplicationGroup)
  }
