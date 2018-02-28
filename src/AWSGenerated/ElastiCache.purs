

-- | <fullname>Amazon ElastiCache</fullname> <p>Amazon ElastiCache is a web service that makes it easier to set up, operate, and scale a distributed cache in the cloud.</p> <p>With ElastiCache, customers get all of the benefits of a high-performance, in-memory cache with less of the administrative burden involved in launching and managing a distributed cache. The service makes setup, scaling, and cluster failure handling much simpler than in a self-managed cache deployment.</p> <p>In addition, through integration with Amazon CloudWatch, customers get enhanced visibility into the key performance statistics associated with their cache and can receive alarms if a part of their cache runs hot.</p>
module AWS.ElastiCache where

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

serviceName = "ElastiCache" :: String


-- | <p>Adds up to 50 cost allocation tags to the named resource. A cost allocation tag is a key-value pair where the key and value are case-sensitive. You can use cost allocation tags to categorize and track your AWS costs.</p> <p> When you apply tags to your ElastiCache resources, AWS generates a cost allocation report as a comma-separated value (CSV) file with your usage and costs aggregated by your tags. You can apply tags that represent business categories (such as cost centers, application names, or owners) to organize your costs across multiple services. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Tagging.html">Using Cost Allocation Tags in Amazon ElastiCache</a> in the <i>ElastiCache User Guide</i>.</p>
addTagsToResource :: forall eff. AddTagsToResourceMessage -> Aff (exception :: EXCEPTION | eff) TagListMessage
addTagsToResource = Request.request serviceName "addTagsToResource" 


-- | <p>Allows network ingress to a cache security group. Applications using ElastiCache must be running on Amazon EC2, and Amazon EC2 security groups are used as the authorization mechanism.</p> <note> <p>You cannot authorize ingress from an Amazon EC2 security group in one region to an ElastiCache cluster in another region.</p> </note>
authorizeCacheSecurityGroupIngress :: forall eff. AuthorizeCacheSecurityGroupIngressMessage -> Aff (exception :: EXCEPTION | eff) AuthorizeCacheSecurityGroupIngressResult
authorizeCacheSecurityGroupIngress = Request.request serviceName "authorizeCacheSecurityGroupIngress" 


-- | <p>Makes a copy of an existing snapshot.</p> <note> <p>This operation is valid for Redis only.</p> </note> <important> <p>Users or groups that have permissions to use the <code>CopySnapshot</code> operation can create their own Amazon S3 buckets and copy snapshots to it. To control access to your snapshots, use an IAM policy to control who has the ability to use the <code>CopySnapshot</code> operation. For more information about using IAM to control the use of ElastiCache operations, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html">Exporting Snapshots</a> and <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/IAM.html">Authentication &amp; Access Control</a>.</p> </important> <p>You could receive the following error messages.</p> <p class="title"> <b>Error Messages</b> </p> <ul> <li> <p> <b>Error Message:</b> The S3 bucket %s is outside of the region.</p> <p> <b>Solution:</b> Create an Amazon S3 bucket in the same region as your snapshot. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.CreateBucket">Step 1: Create an Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message:</b> The S3 bucket %s does not exist.</p> <p> <b>Solution:</b> Create an Amazon S3 bucket in the same region as your snapshot. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.CreateBucket">Step 1: Create an Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message:</b> The S3 bucket %s is not owned by the authenticated user.</p> <p> <b>Solution:</b> Create an Amazon S3 bucket in the same region as your snapshot. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.CreateBucket">Step 1: Create an Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message:</b> The authenticated user does not have sufficient permissions to perform the desired activity.</p> <p> <b>Solution:</b> Contact your system administrator to get the needed permissions.</p> </li> <li> <p> <b>Error Message:</b> The S3 bucket %s already contains an object with key %s.</p> <p> <b>Solution:</b> Give the <code>TargetSnapshotName</code> a new and unique value. If exporting a snapshot, you could alternatively create a new Amazon S3 bucket and use this same value for <code>TargetSnapshotName</code>.</p> </li> <li> <p> <b>Error Message: </b> ElastiCache has not been granted READ permissions %s on the S3 Bucket.</p> <p> <b>Solution:</b> Add List and Read permissions on the bucket. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.GrantAccess">Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message: </b> ElastiCache has not been granted WRITE permissions %s on the S3 Bucket.</p> <p> <b>Solution:</b> Add Upload/Delete permissions on the bucket. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.GrantAccess">Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> <li> <p> <b>Error Message: </b> ElastiCache has not been granted READ_ACP permissions %s on the S3 Bucket.</p> <p> <b>Solution:</b> Add View Permissions on the bucket. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Snapshots.Exporting.html#Snapshots.Exporting.GrantAccess">Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket</a> in the ElastiCache User Guide.</p> </li> </ul>
copySnapshot :: forall eff. CopySnapshotMessage -> Aff (exception :: EXCEPTION | eff) CopySnapshotResult
copySnapshot = Request.request serviceName "copySnapshot" 


-- | <p>Creates a cluster. All nodes in the cluster run the same protocol-compliant cache engine software, either Memcached or Redis.</p> <important> <p>Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups.</p> </important>
createCacheCluster :: forall eff. CreateCacheClusterMessage -> Aff (exception :: EXCEPTION | eff) CreateCacheClusterResult
createCacheCluster = Request.request serviceName "createCacheCluster" 


-- | <p>Creates a new Amazon ElastiCache cache parameter group. An ElastiCache cache parameter group is a collection of parameters and their values that are applied to all of the nodes in any cluster or replication group using the CacheParameterGroup.</p> <p>A newly created CacheParameterGroup is an exact duplicate of the default parameter group for the CacheParameterGroupFamily. To customize the newly created CacheParameterGroup you can change the values of specific parameters. For more information, see:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyCacheParameterGroup.html">ModifyCacheParameterGroup</a> in the ElastiCache API Reference.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/ParameterGroups.html">Parameters and Parameter Groups</a> in the ElastiCache User Guide.</p> </li> </ul>
createCacheParameterGroup :: forall eff. CreateCacheParameterGroupMessage -> Aff (exception :: EXCEPTION | eff) CreateCacheParameterGroupResult
createCacheParameterGroup = Request.request serviceName "createCacheParameterGroup" 


-- | <p>Creates a new cache security group. Use a cache security group to control access to one or more clusters.</p> <p>Cache security groups are only used when you are creating a cluster outside of an Amazon Virtual Private Cloud (Amazon VPC). If you are creating a cluster inside of a VPC, use a cache subnet group instead. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheSubnetGroup.html">CreateCacheSubnetGroup</a>.</p>
createCacheSecurityGroup :: forall eff. CreateCacheSecurityGroupMessage -> Aff (exception :: EXCEPTION | eff) CreateCacheSecurityGroupResult
createCacheSecurityGroup = Request.request serviceName "createCacheSecurityGroup" 


-- | <p>Creates a new cache subnet group.</p> <p>Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).</p>
createCacheSubnetGroup :: forall eff. CreateCacheSubnetGroupMessage -> Aff (exception :: EXCEPTION | eff) CreateCacheSubnetGroupResult
createCacheSubnetGroup = Request.request serviceName "createCacheSubnetGroup" 


-- | <p>Creates a Redis (cluster mode disabled) or a Redis (cluster mode enabled) replication group.</p> <p>A Redis (cluster mode disabled) replication group is a collection of clusters, where one of the clusters is a read/write primary and the others are read-only replicas. Writes to the primary are asynchronously propagated to the replicas.</p> <p>A Redis (cluster mode enabled) replication group is a collection of 1 to 15 node groups (shards). Each node group (shard) has one read/write primary node and up to 5 read-only replica nodes. Writes to the primary are asynchronously propagated to the replicas. Redis (cluster mode enabled) replication groups partition the data across node groups (shards).</p> <p>When a Redis (cluster mode disabled) replication group has been successfully created, you can add one or more read replicas to it, up to a total of 5 read replicas. You cannot alter a Redis (cluster mode enabled) replication group after it has been created. However, if you need to increase or decrease the number of node groups (console: shards), you can avail yourself of ElastiCache for Redis' enhanced backup and restore. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/backups-restoring.html">Restoring From a Backup with Cluster Resizing</a> in the <i>ElastiCache User Guide</i>.</p> <note> <p>This operation is valid for Redis only.</p> </note>
createReplicationGroup :: forall eff. CreateReplicationGroupMessage -> Aff (exception :: EXCEPTION | eff) CreateReplicationGroupResult
createReplicationGroup = Request.request serviceName "createReplicationGroup" 


-- | <p>Creates a copy of an entire cluster or replication group at a specific moment in time.</p> <note> <p>This operation is valid for Redis only.</p> </note>
createSnapshot :: forall eff. CreateSnapshotMessage -> Aff (exception :: EXCEPTION | eff) CreateSnapshotResult
createSnapshot = Request.request serviceName "createSnapshot" 


-- | <p>Deletes a previously provisioned cluster. <code>DeleteCacheCluster</code> deletes all associated cache nodes, node endpoints and the cluster itself. When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the cluster; you cannot cancel or revert this operation.</p> <p>This operation cannot be used to delete a cluster that is the last read replica of a replication group or node group (shard) that has Multi-AZ mode enabled or a cluster from a Redis (cluster mode enabled) replication group.</p> <important> <p>Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups.</p> </important>
deleteCacheCluster :: forall eff. DeleteCacheClusterMessage -> Aff (exception :: EXCEPTION | eff) DeleteCacheClusterResult
deleteCacheCluster = Request.request serviceName "deleteCacheCluster" 


-- | <p>Deletes the specified cache parameter group. You cannot delete a cache parameter group if it is associated with any cache clusters.</p>
deleteCacheParameterGroup :: forall eff. DeleteCacheParameterGroupMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteCacheParameterGroup = Request.request serviceName "deleteCacheParameterGroup" 


-- | <p>Deletes a cache security group.</p> <note> <p>You cannot delete a cache security group if it is associated with any clusters.</p> </note>
deleteCacheSecurityGroup :: forall eff. DeleteCacheSecurityGroupMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteCacheSecurityGroup = Request.request serviceName "deleteCacheSecurityGroup" 


-- | <p>Deletes a cache subnet group.</p> <note> <p>You cannot delete a cache subnet group if it is associated with any clusters.</p> </note>
deleteCacheSubnetGroup :: forall eff. DeleteCacheSubnetGroupMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteCacheSubnetGroup = Request.request serviceName "deleteCacheSubnetGroup" 


-- | <p>Deletes an existing replication group. By default, this operation deletes the entire replication group, including the primary/primaries and all of the read replicas. If the replication group has only one primary, you can optionally delete only the read replicas, while retaining the primary by setting <code>RetainPrimaryCluster=true</code>.</p> <p>When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the selected resources; you cannot cancel or revert this operation.</p> <note> <p>This operation is valid for Redis only.</p> </note>
deleteReplicationGroup :: forall eff. DeleteReplicationGroupMessage -> Aff (exception :: EXCEPTION | eff) DeleteReplicationGroupResult
deleteReplicationGroup = Request.request serviceName "deleteReplicationGroup" 


-- | <p>Deletes an existing snapshot. When you receive a successful response from this operation, ElastiCache immediately begins deleting the snapshot; you cannot cancel or revert this operation.</p> <note> <p>This operation is valid for Redis only.</p> </note>
deleteSnapshot :: forall eff. DeleteSnapshotMessage -> Aff (exception :: EXCEPTION | eff) DeleteSnapshotResult
deleteSnapshot = Request.request serviceName "deleteSnapshot" 


-- | <p>Returns information about all provisioned clusters if no cluster identifier is specified, or about a specific cache cluster if a cluster identifier is supplied.</p> <p>By default, abbreviated information about the clusters is returned. You can use the optional <i>ShowCacheNodeInfo</i> flag to retrieve detailed information about the cache nodes associated with the clusters. These details include the DNS address and port for the cache node endpoint.</p> <p>If the cluster is in the <i>creating</i> state, only cluster-level information is displayed until all of the nodes are successfully provisioned.</p> <p>If the cluster is in the <i>deleting</i> state, only cluster-level information is displayed.</p> <p>If cache nodes are currently being added to the cluster, node endpoint information and creation time for the additional nodes are not displayed until they are completely provisioned. When the cluster state is <i>available</i>, the cluster is ready for use.</p> <p>If cache nodes are currently being removed from the cluster, no endpoint information for the removed nodes is displayed.</p>
describeCacheClusters :: forall eff. DescribeCacheClustersMessage -> Aff (exception :: EXCEPTION | eff) CacheClusterMessage
describeCacheClusters = Request.request serviceName "describeCacheClusters" 


-- | <p>Returns a list of the available cache engines and their versions.</p>
describeCacheEngineVersions :: forall eff. DescribeCacheEngineVersionsMessage -> Aff (exception :: EXCEPTION | eff) CacheEngineVersionMessage
describeCacheEngineVersions = Request.request serviceName "describeCacheEngineVersions" 


-- | <p>Returns a list of cache parameter group descriptions. If a cache parameter group name is specified, the list contains only the descriptions for that group.</p>
describeCacheParameterGroups :: forall eff. DescribeCacheParameterGroupsMessage -> Aff (exception :: EXCEPTION | eff) CacheParameterGroupsMessage
describeCacheParameterGroups = Request.request serviceName "describeCacheParameterGroups" 


-- | <p>Returns the detailed parameter list for a particular cache parameter group.</p>
describeCacheParameters :: forall eff. DescribeCacheParametersMessage -> Aff (exception :: EXCEPTION | eff) CacheParameterGroupDetails
describeCacheParameters = Request.request serviceName "describeCacheParameters" 


-- | <p>Returns a list of cache security group descriptions. If a cache security group name is specified, the list contains only the description of that group.</p>
describeCacheSecurityGroups :: forall eff. DescribeCacheSecurityGroupsMessage -> Aff (exception :: EXCEPTION | eff) CacheSecurityGroupMessage
describeCacheSecurityGroups = Request.request serviceName "describeCacheSecurityGroups" 


-- | <p>Returns a list of cache subnet group descriptions. If a subnet group name is specified, the list contains only the description of that group.</p>
describeCacheSubnetGroups :: forall eff. DescribeCacheSubnetGroupsMessage -> Aff (exception :: EXCEPTION | eff) CacheSubnetGroupMessage
describeCacheSubnetGroups = Request.request serviceName "describeCacheSubnetGroups" 


-- | <p>Returns the default engine and system parameter information for the specified cache engine.</p>
describeEngineDefaultParameters :: forall eff. DescribeEngineDefaultParametersMessage -> Aff (exception :: EXCEPTION | eff) DescribeEngineDefaultParametersResult
describeEngineDefaultParameters = Request.request serviceName "describeEngineDefaultParameters" 


-- | <p>Returns events related to clusters, cache security groups, and cache parameter groups. You can obtain events specific to a particular cluster, cache security group, or cache parameter group by providing the name as a parameter.</p> <p>By default, only the events occurring within the last hour are returned; however, you can retrieve up to 14 days' worth of events if necessary.</p>
describeEvents :: forall eff. DescribeEventsMessage -> Aff (exception :: EXCEPTION | eff) EventsMessage
describeEvents = Request.request serviceName "describeEvents" 


-- | <p>Returns information about a particular replication group. If no identifier is specified, <code>DescribeReplicationGroups</code> returns information about all replication groups.</p> <note> <p>This operation is valid for Redis only.</p> </note>
describeReplicationGroups :: forall eff. DescribeReplicationGroupsMessage -> Aff (exception :: EXCEPTION | eff) ReplicationGroupMessage
describeReplicationGroups = Request.request serviceName "describeReplicationGroups" 


-- | <p>Returns information about reserved cache nodes for this account, or about a specified reserved cache node.</p>
describeReservedCacheNodes :: forall eff. DescribeReservedCacheNodesMessage -> Aff (exception :: EXCEPTION | eff) ReservedCacheNodeMessage
describeReservedCacheNodes = Request.request serviceName "describeReservedCacheNodes" 


-- | <p>Lists available reserved cache node offerings.</p>
describeReservedCacheNodesOfferings :: forall eff. DescribeReservedCacheNodesOfferingsMessage -> Aff (exception :: EXCEPTION | eff) ReservedCacheNodesOfferingMessage
describeReservedCacheNodesOfferings = Request.request serviceName "describeReservedCacheNodesOfferings" 


-- | <p>Returns information about cluster or replication group snapshots. By default, <code>DescribeSnapshots</code> lists all of your snapshots; it can optionally describe a single snapshot, or just the snapshots associated with a particular cache cluster.</p> <note> <p>This operation is valid for Redis only.</p> </note>
describeSnapshots :: forall eff. DescribeSnapshotsMessage -> Aff (exception :: EXCEPTION | eff) DescribeSnapshotsListMessage
describeSnapshots = Request.request serviceName "describeSnapshots" 


-- | <p>Lists all available node types that you can scale your Redis cluster's or replication group's current node type up to.</p> <p>When you use the <code>ModifyCacheCluster</code> or <code>ModifyReplicationGroup</code> operations to scale up your cluster or replication group, the value of the <code>CacheNodeType</code> parameter must be one of the node types returned by this operation.</p>
listAllowedNodeTypeModifications :: forall eff. ListAllowedNodeTypeModificationsMessage -> Aff (exception :: EXCEPTION | eff) AllowedNodeTypeModificationsMessage
listAllowedNodeTypeModifications = Request.request serviceName "listAllowedNodeTypeModifications" 


-- | <p>Lists all cost allocation tags currently on the named resource. A <code>cost allocation tag</code> is a key-value pair where the key is case-sensitive and the value is optional. You can use cost allocation tags to categorize and track your AWS costs.</p> <p>You can have a maximum of 50 cost allocation tags on an ElastiCache resource. For more information, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/BestPractices.html">Using Cost Allocation Tags in Amazon ElastiCache</a>.</p>
listTagsForResource :: forall eff. ListTagsForResourceMessage -> Aff (exception :: EXCEPTION | eff) TagListMessage
listTagsForResource = Request.request serviceName "listTagsForResource" 


-- | <p>Modifies the settings for a cluster. You can use this operation to change one or more cluster configuration parameters by specifying the parameters and the new values.</p>
modifyCacheCluster :: forall eff. ModifyCacheClusterMessage -> Aff (exception :: EXCEPTION | eff) ModifyCacheClusterResult
modifyCacheCluster = Request.request serviceName "modifyCacheCluster" 


-- | <p>Modifies the parameters of a cache parameter group. You can modify up to 20 parameters in a single request by submitting a list parameter name and value pairs.</p>
modifyCacheParameterGroup :: forall eff. ModifyCacheParameterGroupMessage -> Aff (exception :: EXCEPTION | eff) CacheParameterGroupNameMessage
modifyCacheParameterGroup = Request.request serviceName "modifyCacheParameterGroup" 


-- | <p>Modifies an existing cache subnet group.</p>
modifyCacheSubnetGroup :: forall eff. ModifyCacheSubnetGroupMessage -> Aff (exception :: EXCEPTION | eff) ModifyCacheSubnetGroupResult
modifyCacheSubnetGroup = Request.request serviceName "modifyCacheSubnetGroup" 


-- | <p>Modifies the settings for a replication group.</p> <important> <p>Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups.</p> </important> <note> <p>This operation is valid for Redis only.</p> </note>
modifyReplicationGroup :: forall eff. ModifyReplicationGroupMessage -> Aff (exception :: EXCEPTION | eff) ModifyReplicationGroupResult
modifyReplicationGroup = Request.request serviceName "modifyReplicationGroup" 


-- | <p>Performs horizontal scaling on a Redis (cluster mode enabled) cluster with no downtime. Requires Redis engine version 3.2.10 or newer. For information on upgrading your engine to a newer version, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/VersionManagement.html">Upgrading Engine Versions</a> in the Amazon ElastiCache User Guide.</p> <p>For more information on ElastiCache for Redis online horizontal scaling, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/redis-cluster-resharding-online.html">ElastiCache for Redis Horizontal Scaling</a> </p>
modifyReplicationGroupShardConfiguration :: forall eff. ModifyReplicationGroupShardConfigurationMessage -> Aff (exception :: EXCEPTION | eff) ModifyReplicationGroupShardConfigurationResult
modifyReplicationGroupShardConfiguration = Request.request serviceName "modifyReplicationGroupShardConfiguration" 


-- | <p>Allows you to purchase a reserved cache node offering.</p>
purchaseReservedCacheNodesOffering :: forall eff. PurchaseReservedCacheNodesOfferingMessage -> Aff (exception :: EXCEPTION | eff) PurchaseReservedCacheNodesOfferingResult
purchaseReservedCacheNodesOffering = Request.request serviceName "purchaseReservedCacheNodesOffering" 


-- | <p>Reboots some, or all, of the cache nodes within a provisioned cluster. This operation applies any modified cache parameter groups to the cluster. The reboot operation takes place as soon as possible, and results in a momentary outage to the cluster. During the reboot, the cluster status is set to REBOOTING.</p> <p>The reboot causes the contents of the cache (for each cache node being rebooted) to be lost.</p> <p>When the reboot is complete, a cluster event is created.</p> <p>Rebooting a cluster is currently supported on Memcached and Redis (cluster mode disabled) clusters. Rebooting is not supported on Redis (cluster mode enabled) clusters.</p> <p>If you make changes to parameters that require a Redis (cluster mode enabled) cluster reboot for the changes to be applied, see <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Clusters.Rebooting.htm">Rebooting a Cluster</a> for an alternate process.</p>
rebootCacheCluster :: forall eff. RebootCacheClusterMessage -> Aff (exception :: EXCEPTION | eff) RebootCacheClusterResult
rebootCacheCluster = Request.request serviceName "rebootCacheCluster" 


-- | <p>Removes the tags identified by the <code>TagKeys</code> list from the named resource.</p>
removeTagsFromResource :: forall eff. RemoveTagsFromResourceMessage -> Aff (exception :: EXCEPTION | eff) TagListMessage
removeTagsFromResource = Request.request serviceName "removeTagsFromResource" 


-- | <p>Modifies the parameters of a cache parameter group to the engine or system default value. You can reset specific parameters by submitting a list of parameter names. To reset the entire cache parameter group, specify the <code>ResetAllParameters</code> and <code>CacheParameterGroupName</code> parameters.</p>
resetCacheParameterGroup :: forall eff. ResetCacheParameterGroupMessage -> Aff (exception :: EXCEPTION | eff) CacheParameterGroupNameMessage
resetCacheParameterGroup = Request.request serviceName "resetCacheParameterGroup" 


-- | <p>Revokes ingress from a cache security group. Use this operation to disallow access from an Amazon EC2 security group that had been previously authorized.</p>
revokeCacheSecurityGroupIngress :: forall eff. RevokeCacheSecurityGroupIngressMessage -> Aff (exception :: EXCEPTION | eff) RevokeCacheSecurityGroupIngressResult
revokeCacheSecurityGroupIngress = Request.request serviceName "revokeCacheSecurityGroupIngress" 


-- | <p>Represents the input of a <code>TestFailover</code> operation which test automatic failover on a specified node group (called shard in the console) in a replication group (called cluster in the console).</p> <p class="title"> <b>Note the following</b> </p> <ul> <li> <p>A customer can use this operation to test automatic failover on up to 5 shards (called node groups in the ElastiCache API and AWS CLI) in any rolling 24-hour period.</p> </li> <li> <p>If calling this operation on shards in different clusters (called replication groups in the API and CLI), the calls can be made concurrently.</p> <p> </p> </li> <li> <p>If calling this operation multiple times on different shards in the same Redis (cluster mode enabled) replication group, the first node replacement must complete before a subsequent call can be made.</p> </li> <li> <p>To determine whether the node replacement is complete you can check Events using the Amazon ElastiCache console, the AWS CLI, or the ElastiCache API. Look for the following automatic failover related events, listed here in order of occurrance:</p> <ol> <li> <p>Replication group message: <code>Test Failover API called for node group &lt;node-group-id&gt;</code> </p> </li> <li> <p>Cache cluster message: <code>Failover from master node &lt;primary-node-id&gt; to replica node &lt;node-id&gt; completed</code> </p> </li> <li> <p>Replication group message: <code>Failover from master node &lt;primary-node-id&gt; to replica node &lt;node-id&gt; completed</code> </p> </li> <li> <p>Cache cluster message: <code>Recovering cache nodes &lt;node-id&gt;</code> </p> </li> <li> <p>Cache cluster message: <code>Finished recovery for cache nodes &lt;node-id&gt;</code> </p> </li> </ol> <p>For more information see:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/ECEvents.Viewing.html">Viewing ElastiCache Events</a> in the <i>ElastiCache User Guide</i> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeEvents.html">DescribeEvents</a> in the ElastiCache API Reference</p> </li> </ul> </li> </ul> <p>Also see, <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/AutoFailover.html#auto-failover-test">Testing Multi-AZ with Automatic Failover</a> in the <i>ElastiCache User Guide</i>.</p>
testFailover :: forall eff. TestFailoverMessage -> Aff (exception :: EXCEPTION | eff) TestFailoverResult
testFailover = Request.request serviceName "testFailover" 


-- | <p>The customer has exceeded the allowed rate of API calls.</p>
newtype APICallRateForCustomerExceededFault = APICallRateForCustomerExceededFault Types.NoArguments
derive instance newtypeAPICallRateForCustomerExceededFault :: Newtype APICallRateForCustomerExceededFault _
derive instance repGenericAPICallRateForCustomerExceededFault :: Generic APICallRateForCustomerExceededFault _
instance showAPICallRateForCustomerExceededFault :: Show APICallRateForCustomerExceededFault where
  show = genericShow
instance decodeAPICallRateForCustomerExceededFault :: Decode APICallRateForCustomerExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAPICallRateForCustomerExceededFault :: Encode APICallRateForCustomerExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AZMode = AZMode String
derive instance newtypeAZMode :: Newtype AZMode _
derive instance repGenericAZMode :: Generic AZMode _
instance showAZMode :: Show AZMode where
  show = genericShow
instance decodeAZMode :: Decode AZMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAZMode :: Encode AZMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of an AddTagsToResource operation.</p>
newtype AddTagsToResourceMessage = AddTagsToResourceMessage 
  { "ResourceName" :: (String)
  , "Tags" :: (TagList)
  }
derive instance newtypeAddTagsToResourceMessage :: Newtype AddTagsToResourceMessage _
derive instance repGenericAddTagsToResourceMessage :: Generic AddTagsToResourceMessage _
instance showAddTagsToResourceMessage :: Show AddTagsToResourceMessage where
  show = genericShow
instance decodeAddTagsToResourceMessage :: Decode AddTagsToResourceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsToResourceMessage :: Encode AddTagsToResourceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the allowed node types you can use to modify your cluster or replication group.</p>
newtype AllowedNodeTypeModificationsMessage = AllowedNodeTypeModificationsMessage 
  { "ScaleUpModifications" :: NullOrUndefined.NullOrUndefined (NodeTypeList)
  }
derive instance newtypeAllowedNodeTypeModificationsMessage :: Newtype AllowedNodeTypeModificationsMessage _
derive instance repGenericAllowedNodeTypeModificationsMessage :: Generic AllowedNodeTypeModificationsMessage _
instance showAllowedNodeTypeModificationsMessage :: Show AllowedNodeTypeModificationsMessage where
  show = genericShow
instance decodeAllowedNodeTypeModificationsMessage :: Decode AllowedNodeTypeModificationsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedNodeTypeModificationsMessage :: Encode AllowedNodeTypeModificationsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified Amazon EC2 security group is already authorized for the specified cache security group.</p>
newtype AuthorizationAlreadyExistsFault = AuthorizationAlreadyExistsFault Types.NoArguments
derive instance newtypeAuthorizationAlreadyExistsFault :: Newtype AuthorizationAlreadyExistsFault _
derive instance repGenericAuthorizationAlreadyExistsFault :: Generic AuthorizationAlreadyExistsFault _
instance showAuthorizationAlreadyExistsFault :: Show AuthorizationAlreadyExistsFault where
  show = genericShow
instance decodeAuthorizationAlreadyExistsFault :: Decode AuthorizationAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizationAlreadyExistsFault :: Encode AuthorizationAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified Amazon EC2 security group is not authorized for the specified cache security group.</p>
newtype AuthorizationNotFoundFault = AuthorizationNotFoundFault Types.NoArguments
derive instance newtypeAuthorizationNotFoundFault :: Newtype AuthorizationNotFoundFault _
derive instance repGenericAuthorizationNotFoundFault :: Generic AuthorizationNotFoundFault _
instance showAuthorizationNotFoundFault :: Show AuthorizationNotFoundFault where
  show = genericShow
instance decodeAuthorizationNotFoundFault :: Decode AuthorizationNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizationNotFoundFault :: Encode AuthorizationNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of an AuthorizeCacheSecurityGroupIngress operation.</p>
newtype AuthorizeCacheSecurityGroupIngressMessage = AuthorizeCacheSecurityGroupIngressMessage 
  { "CacheSecurityGroupName" :: (String)
  , "EC2SecurityGroupName" :: (String)
  , "EC2SecurityGroupOwnerId" :: (String)
  }
derive instance newtypeAuthorizeCacheSecurityGroupIngressMessage :: Newtype AuthorizeCacheSecurityGroupIngressMessage _
derive instance repGenericAuthorizeCacheSecurityGroupIngressMessage :: Generic AuthorizeCacheSecurityGroupIngressMessage _
instance showAuthorizeCacheSecurityGroupIngressMessage :: Show AuthorizeCacheSecurityGroupIngressMessage where
  show = genericShow
instance decodeAuthorizeCacheSecurityGroupIngressMessage :: Decode AuthorizeCacheSecurityGroupIngressMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizeCacheSecurityGroupIngressMessage :: Encode AuthorizeCacheSecurityGroupIngressMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthorizeCacheSecurityGroupIngressResult = AuthorizeCacheSecurityGroupIngressResult 
  { "CacheSecurityGroup" :: NullOrUndefined.NullOrUndefined (CacheSecurityGroup)
  }
derive instance newtypeAuthorizeCacheSecurityGroupIngressResult :: Newtype AuthorizeCacheSecurityGroupIngressResult _
derive instance repGenericAuthorizeCacheSecurityGroupIngressResult :: Generic AuthorizeCacheSecurityGroupIngressResult _
instance showAuthorizeCacheSecurityGroupIngressResult :: Show AuthorizeCacheSecurityGroupIngressResult where
  show = genericShow
instance decodeAuthorizeCacheSecurityGroupIngressResult :: Decode AuthorizeCacheSecurityGroupIngressResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizeCacheSecurityGroupIngressResult :: Encode AuthorizeCacheSecurityGroupIngressResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomaticFailoverStatus = AutomaticFailoverStatus String
derive instance newtypeAutomaticFailoverStatus :: Newtype AutomaticFailoverStatus _
derive instance repGenericAutomaticFailoverStatus :: Generic AutomaticFailoverStatus _
instance showAutomaticFailoverStatus :: Show AutomaticFailoverStatus where
  show = genericShow
instance decodeAutomaticFailoverStatus :: Decode AutomaticFailoverStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomaticFailoverStatus :: Encode AutomaticFailoverStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an Availability Zone in which the cluster is launched.</p>
newtype AvailabilityZone = AvailabilityZone 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _
derive instance repGenericAvailabilityZone :: Generic AvailabilityZone _
instance showAvailabilityZone :: Show AvailabilityZone where
  show = genericShow
instance decodeAvailabilityZone :: Decode AvailabilityZone where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZone :: Encode AvailabilityZone where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailabilityZonesList = AvailabilityZonesList (Array String)
derive instance newtypeAvailabilityZonesList :: Newtype AvailabilityZonesList _
derive instance repGenericAvailabilityZonesList :: Generic AvailabilityZonesList _
instance showAvailabilityZonesList :: Show AvailabilityZonesList where
  show = genericShow
instance decodeAvailabilityZonesList :: Decode AvailabilityZonesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZonesList :: Encode AvailabilityZonesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AwsQueryErrorMessage = AwsQueryErrorMessage String
derive instance newtypeAwsQueryErrorMessage :: Newtype AwsQueryErrorMessage _
derive instance repGenericAwsQueryErrorMessage :: Generic AwsQueryErrorMessage _
instance showAwsQueryErrorMessage :: Show AwsQueryErrorMessage where
  show = genericShow
instance decodeAwsQueryErrorMessage :: Decode AwsQueryErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsQueryErrorMessage :: Encode AwsQueryErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanOptional = BooleanOptional Boolean
derive instance newtypeBooleanOptional :: Newtype BooleanOptional _
derive instance repGenericBooleanOptional :: Generic BooleanOptional _
instance showBooleanOptional :: Show BooleanOptional where
  show = genericShow
instance decodeBooleanOptional :: Decode BooleanOptional where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanOptional :: Encode BooleanOptional where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains all of the attributes of a specific cluster.</p>
newtype CacheCluster = CacheCluster 
  { "CacheClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "ConfigurationEndpoint" :: NullOrUndefined.NullOrUndefined (Endpoint)
  , "ClientDownloadLandingPage" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheClusterStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "NumCacheNodes" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "PreferredAvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheClusterCreateTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "PendingModifiedValues" :: NullOrUndefined.NullOrUndefined (PendingModifiedValues)
  , "NotificationConfiguration" :: NullOrUndefined.NullOrUndefined (NotificationConfiguration)
  , "CacheSecurityGroups" :: NullOrUndefined.NullOrUndefined (CacheSecurityGroupMembershipList)
  , "CacheParameterGroup" :: NullOrUndefined.NullOrUndefined (CacheParameterGroupStatus)
  , "CacheSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodes" :: NullOrUndefined.NullOrUndefined (CacheNodeList)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "SecurityGroups" :: NullOrUndefined.NullOrUndefined (SecurityGroupMembershipList)
  , "ReplicationGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotRetentionLimit" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthTokenEnabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "TransitEncryptionEnabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "AtRestEncryptionEnabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeCacheCluster :: Newtype CacheCluster _
derive instance repGenericCacheCluster :: Generic CacheCluster _
instance showCacheCluster :: Show CacheCluster where
  show = genericShow
instance decodeCacheCluster :: Decode CacheCluster where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheCluster :: Encode CacheCluster where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You already have a cluster with the given identifier.</p>
newtype CacheClusterAlreadyExistsFault = CacheClusterAlreadyExistsFault Types.NoArguments
derive instance newtypeCacheClusterAlreadyExistsFault :: Newtype CacheClusterAlreadyExistsFault _
derive instance repGenericCacheClusterAlreadyExistsFault :: Generic CacheClusterAlreadyExistsFault _
instance showCacheClusterAlreadyExistsFault :: Show CacheClusterAlreadyExistsFault where
  show = genericShow
instance decodeCacheClusterAlreadyExistsFault :: Decode CacheClusterAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheClusterAlreadyExistsFault :: Encode CacheClusterAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheClusterList = CacheClusterList (Array CacheCluster)
derive instance newtypeCacheClusterList :: Newtype CacheClusterList _
derive instance repGenericCacheClusterList :: Generic CacheClusterList _
instance showCacheClusterList :: Show CacheClusterList where
  show = genericShow
instance decodeCacheClusterList :: Decode CacheClusterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheClusterList :: Encode CacheClusterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeCacheClusters</code> operation.</p>
newtype CacheClusterMessage = CacheClusterMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheClusters" :: NullOrUndefined.NullOrUndefined (CacheClusterList)
  }
derive instance newtypeCacheClusterMessage :: Newtype CacheClusterMessage _
derive instance repGenericCacheClusterMessage :: Generic CacheClusterMessage _
instance showCacheClusterMessage :: Show CacheClusterMessage where
  show = genericShow
instance decodeCacheClusterMessage :: Decode CacheClusterMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheClusterMessage :: Encode CacheClusterMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested cluster ID does not refer to an existing cluster.</p>
newtype CacheClusterNotFoundFault = CacheClusterNotFoundFault Types.NoArguments
derive instance newtypeCacheClusterNotFoundFault :: Newtype CacheClusterNotFoundFault _
derive instance repGenericCacheClusterNotFoundFault :: Generic CacheClusterNotFoundFault _
instance showCacheClusterNotFoundFault :: Show CacheClusterNotFoundFault where
  show = genericShow
instance decodeCacheClusterNotFoundFault :: Decode CacheClusterNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheClusterNotFoundFault :: Encode CacheClusterNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides all of the details about a particular cache engine version.</p>
newtype CacheEngineVersion = CacheEngineVersion 
  { "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheParameterGroupFamily" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheEngineDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheEngineVersionDescription" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCacheEngineVersion :: Newtype CacheEngineVersion _
derive instance repGenericCacheEngineVersion :: Generic CacheEngineVersion _
instance showCacheEngineVersion :: Show CacheEngineVersion where
  show = genericShow
instance decodeCacheEngineVersion :: Decode CacheEngineVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheEngineVersion :: Encode CacheEngineVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheEngineVersionList = CacheEngineVersionList (Array CacheEngineVersion)
derive instance newtypeCacheEngineVersionList :: Newtype CacheEngineVersionList _
derive instance repGenericCacheEngineVersionList :: Generic CacheEngineVersionList _
instance showCacheEngineVersionList :: Show CacheEngineVersionList where
  show = genericShow
instance decodeCacheEngineVersionList :: Decode CacheEngineVersionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheEngineVersionList :: Encode CacheEngineVersionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>DescribeCacheEngineVersions</a> operation.</p>
newtype CacheEngineVersionMessage = CacheEngineVersionMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheEngineVersions" :: NullOrUndefined.NullOrUndefined (CacheEngineVersionList)
  }
derive instance newtypeCacheEngineVersionMessage :: Newtype CacheEngineVersionMessage _
derive instance repGenericCacheEngineVersionMessage :: Generic CacheEngineVersionMessage _
instance showCacheEngineVersionMessage :: Show CacheEngineVersionMessage where
  show = genericShow
instance decodeCacheEngineVersionMessage :: Decode CacheEngineVersionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheEngineVersionMessage :: Encode CacheEngineVersionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an individual cache node within a cluster. Each cache node runs its own instance of the cluster's protocol-compliant caching software - either Memcached or Redis.</p> <p>The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.</p> <ul> <li> <p>General purpose:</p> <ul> <li> <p>Current generation: </p> <p> <b>T2 node types:</b> <code>cache.t2.micro</code>, <code>cache.t2.small</code>, <code>cache.t2.medium</code> </p> <p> <b>M3 node types:</b> <code>cache.m3.medium</code>, <code>cache.m3.large</code>, <code>cache.m3.xlarge</code>, <code>cache.m3.2xlarge</code> </p> <p> <b>M4 node types:</b> <code>cache.m4.large</code>, <code>cache.m4.xlarge</code>, <code>cache.m4.2xlarge</code>, <code>cache.m4.4xlarge</code>, <code>cache.m4.10xlarge</code> </p> </li> <li> <p>Previous generation: (not recommended)</p> <p> <b>T1 node types:</b> <code>cache.t1.micro</code> </p> <p> <b>M1 node types:</b> <code>cache.m1.small</code>, <code>cache.m1.medium</code>, <code>cache.m1.large</code>, <code>cache.m1.xlarge</code> </p> </li> </ul> </li> <li> <p>Compute optimized:</p> <ul> <li> <p>Previous generation: (not recommended)</p> <p> <b>C1 node types:</b> <code>cache.c1.xlarge</code> </p> </li> </ul> </li> <li> <p>Memory optimized:</p> <ul> <li> <p>Current generation: </p> <p> <b>R3 node types:</b> <code>cache.r3.large</code>, <code>cache.r3.xlarge</code>, <code>cache.r3.2xlarge</code>, <code>cache.r3.4xlarge</code>, <code>cache.r3.8xlarge</code> </p> </li> <li> <p>Previous generation: (not recommended)</p> <p> <b>M2 node types:</b> <code>cache.m2.xlarge</code>, <code>cache.m2.2xlarge</code>, <code>cache.m2.4xlarge</code> </p> </li> </ul> </li> </ul> <p> <b>Notes:</b> </p> <ul> <li> <p>All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).</p> </li> <li> <p>Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances. </p> </li> <li> <p>Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.</p> </li> <li> <p>Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances.</p> </li> </ul> <p>For a complete listing of node types and specifications, see <a href="http://aws.amazon.com/elasticache/details">Amazon ElastiCache Product Features and Details</a> and either <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific">Cache Node Type-Specific Parameters for Memcached</a> or <a href="http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific">Cache Node Type-Specific Parameters for Redis</a>.</p>
newtype CacheNode = CacheNode 
  { "CacheNodeId" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeCreateTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "Endpoint" :: NullOrUndefined.NullOrUndefined (Endpoint)
  , "ParameterGroupStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceCacheNodeId" :: NullOrUndefined.NullOrUndefined (String)
  , "CustomerAvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCacheNode :: Newtype CacheNode _
derive instance repGenericCacheNode :: Generic CacheNode _
instance showCacheNode :: Show CacheNode where
  show = genericShow
instance decodeCacheNode :: Decode CacheNode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheNode :: Encode CacheNode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheNodeIdsList = CacheNodeIdsList (Array String)
derive instance newtypeCacheNodeIdsList :: Newtype CacheNodeIdsList _
derive instance repGenericCacheNodeIdsList :: Generic CacheNodeIdsList _
instance showCacheNodeIdsList :: Show CacheNodeIdsList where
  show = genericShow
instance decodeCacheNodeIdsList :: Decode CacheNodeIdsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheNodeIdsList :: Encode CacheNodeIdsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheNodeList = CacheNodeList (Array CacheNode)
derive instance newtypeCacheNodeList :: Newtype CacheNodeList _
derive instance repGenericCacheNodeList :: Generic CacheNodeList _
instance showCacheNodeList :: Show CacheNodeList where
  show = genericShow
instance decodeCacheNodeList :: Decode CacheNodeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheNodeList :: Encode CacheNodeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A parameter that has a different value for each cache node type it is applied to. For example, in a Redis cluster, a <code>cache.m1.large</code> cache node type would have a larger <code>maxmemory</code> value than a <code>cache.m1.small</code> type.</p>
newtype CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter 
  { "ParameterName" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Source" :: NullOrUndefined.NullOrUndefined (String)
  , "DataType" :: NullOrUndefined.NullOrUndefined (String)
  , "AllowedValues" :: NullOrUndefined.NullOrUndefined (String)
  , "IsModifiable" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "MinimumEngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeTypeSpecificValues" :: NullOrUndefined.NullOrUndefined (CacheNodeTypeSpecificValueList)
  , "ChangeType" :: NullOrUndefined.NullOrUndefined (ChangeType)
  }
derive instance newtypeCacheNodeTypeSpecificParameter :: Newtype CacheNodeTypeSpecificParameter _
derive instance repGenericCacheNodeTypeSpecificParameter :: Generic CacheNodeTypeSpecificParameter _
instance showCacheNodeTypeSpecificParameter :: Show CacheNodeTypeSpecificParameter where
  show = genericShow
instance decodeCacheNodeTypeSpecificParameter :: Decode CacheNodeTypeSpecificParameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheNodeTypeSpecificParameter :: Encode CacheNodeTypeSpecificParameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheNodeTypeSpecificParametersList = CacheNodeTypeSpecificParametersList (Array CacheNodeTypeSpecificParameter)
derive instance newtypeCacheNodeTypeSpecificParametersList :: Newtype CacheNodeTypeSpecificParametersList _
derive instance repGenericCacheNodeTypeSpecificParametersList :: Generic CacheNodeTypeSpecificParametersList _
instance showCacheNodeTypeSpecificParametersList :: Show CacheNodeTypeSpecificParametersList where
  show = genericShow
instance decodeCacheNodeTypeSpecificParametersList :: Decode CacheNodeTypeSpecificParametersList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheNodeTypeSpecificParametersList :: Encode CacheNodeTypeSpecificParametersList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A value that applies only to a certain cache node type.</p>
newtype CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue 
  { "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "Value" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCacheNodeTypeSpecificValue :: Newtype CacheNodeTypeSpecificValue _
derive instance repGenericCacheNodeTypeSpecificValue :: Generic CacheNodeTypeSpecificValue _
instance showCacheNodeTypeSpecificValue :: Show CacheNodeTypeSpecificValue where
  show = genericShow
instance decodeCacheNodeTypeSpecificValue :: Decode CacheNodeTypeSpecificValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheNodeTypeSpecificValue :: Encode CacheNodeTypeSpecificValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheNodeTypeSpecificValueList = CacheNodeTypeSpecificValueList (Array CacheNodeTypeSpecificValue)
derive instance newtypeCacheNodeTypeSpecificValueList :: Newtype CacheNodeTypeSpecificValueList _
derive instance repGenericCacheNodeTypeSpecificValueList :: Generic CacheNodeTypeSpecificValueList _
instance showCacheNodeTypeSpecificValueList :: Show CacheNodeTypeSpecificValueList where
  show = genericShow
instance decodeCacheNodeTypeSpecificValueList :: Decode CacheNodeTypeSpecificValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheNodeTypeSpecificValueList :: Encode CacheNodeTypeSpecificValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>CreateCacheParameterGroup</code> operation.</p>
newtype CacheParameterGroup = CacheParameterGroup 
  { "CacheParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheParameterGroupFamily" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCacheParameterGroup :: Newtype CacheParameterGroup _
derive instance repGenericCacheParameterGroup :: Generic CacheParameterGroup _
instance showCacheParameterGroup :: Show CacheParameterGroup where
  show = genericShow
instance decodeCacheParameterGroup :: Decode CacheParameterGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheParameterGroup :: Encode CacheParameterGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A cache parameter group with the requested name already exists.</p>
newtype CacheParameterGroupAlreadyExistsFault = CacheParameterGroupAlreadyExistsFault Types.NoArguments
derive instance newtypeCacheParameterGroupAlreadyExistsFault :: Newtype CacheParameterGroupAlreadyExistsFault _
derive instance repGenericCacheParameterGroupAlreadyExistsFault :: Generic CacheParameterGroupAlreadyExistsFault _
instance showCacheParameterGroupAlreadyExistsFault :: Show CacheParameterGroupAlreadyExistsFault where
  show = genericShow
instance decodeCacheParameterGroupAlreadyExistsFault :: Decode CacheParameterGroupAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheParameterGroupAlreadyExistsFault :: Encode CacheParameterGroupAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeCacheParameters</code> operation.</p>
newtype CacheParameterGroupDetails = CacheParameterGroupDetails 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersList)
  , "CacheNodeTypeSpecificParameters" :: NullOrUndefined.NullOrUndefined (CacheNodeTypeSpecificParametersList)
  }
derive instance newtypeCacheParameterGroupDetails :: Newtype CacheParameterGroupDetails _
derive instance repGenericCacheParameterGroupDetails :: Generic CacheParameterGroupDetails _
instance showCacheParameterGroupDetails :: Show CacheParameterGroupDetails where
  show = genericShow
instance decodeCacheParameterGroupDetails :: Decode CacheParameterGroupDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheParameterGroupDetails :: Encode CacheParameterGroupDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheParameterGroupList = CacheParameterGroupList (Array CacheParameterGroup)
derive instance newtypeCacheParameterGroupList :: Newtype CacheParameterGroupList _
derive instance repGenericCacheParameterGroupList :: Generic CacheParameterGroupList _
instance showCacheParameterGroupList :: Show CacheParameterGroupList where
  show = genericShow
instance decodeCacheParameterGroupList :: Decode CacheParameterGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheParameterGroupList :: Encode CacheParameterGroupList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of one of the following operations:</p> <ul> <li> <p> <code>ModifyCacheParameterGroup</code> </p> </li> <li> <p> <code>ResetCacheParameterGroup</code> </p> </li> </ul>
newtype CacheParameterGroupNameMessage = CacheParameterGroupNameMessage 
  { "CacheParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCacheParameterGroupNameMessage :: Newtype CacheParameterGroupNameMessage _
derive instance repGenericCacheParameterGroupNameMessage :: Generic CacheParameterGroupNameMessage _
instance showCacheParameterGroupNameMessage :: Show CacheParameterGroupNameMessage where
  show = genericShow
instance decodeCacheParameterGroupNameMessage :: Decode CacheParameterGroupNameMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheParameterGroupNameMessage :: Encode CacheParameterGroupNameMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested cache parameter group name does not refer to an existing cache parameter group.</p>
newtype CacheParameterGroupNotFoundFault = CacheParameterGroupNotFoundFault Types.NoArguments
derive instance newtypeCacheParameterGroupNotFoundFault :: Newtype CacheParameterGroupNotFoundFault _
derive instance repGenericCacheParameterGroupNotFoundFault :: Generic CacheParameterGroupNotFoundFault _
instance showCacheParameterGroupNotFoundFault :: Show CacheParameterGroupNotFoundFault where
  show = genericShow
instance decodeCacheParameterGroupNotFoundFault :: Decode CacheParameterGroupNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheParameterGroupNotFoundFault :: Encode CacheParameterGroupNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request cannot be processed because it would exceed the maximum number of cache security groups.</p>
newtype CacheParameterGroupQuotaExceededFault = CacheParameterGroupQuotaExceededFault Types.NoArguments
derive instance newtypeCacheParameterGroupQuotaExceededFault :: Newtype CacheParameterGroupQuotaExceededFault _
derive instance repGenericCacheParameterGroupQuotaExceededFault :: Generic CacheParameterGroupQuotaExceededFault _
instance showCacheParameterGroupQuotaExceededFault :: Show CacheParameterGroupQuotaExceededFault where
  show = genericShow
instance decodeCacheParameterGroupQuotaExceededFault :: Decode CacheParameterGroupQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheParameterGroupQuotaExceededFault :: Encode CacheParameterGroupQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Status of the cache parameter group.</p>
newtype CacheParameterGroupStatus = CacheParameterGroupStatus 
  { "CacheParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterApplyStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeIdsToReboot" :: NullOrUndefined.NullOrUndefined (CacheNodeIdsList)
  }
derive instance newtypeCacheParameterGroupStatus :: Newtype CacheParameterGroupStatus _
derive instance repGenericCacheParameterGroupStatus :: Generic CacheParameterGroupStatus _
instance showCacheParameterGroupStatus :: Show CacheParameterGroupStatus where
  show = genericShow
instance decodeCacheParameterGroupStatus :: Decode CacheParameterGroupStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheParameterGroupStatus :: Encode CacheParameterGroupStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeCacheParameterGroups</code> operation.</p>
newtype CacheParameterGroupsMessage = CacheParameterGroupsMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheParameterGroups" :: NullOrUndefined.NullOrUndefined (CacheParameterGroupList)
  }
derive instance newtypeCacheParameterGroupsMessage :: Newtype CacheParameterGroupsMessage _
derive instance repGenericCacheParameterGroupsMessage :: Generic CacheParameterGroupsMessage _
instance showCacheParameterGroupsMessage :: Show CacheParameterGroupsMessage where
  show = genericShow
instance decodeCacheParameterGroupsMessage :: Decode CacheParameterGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheParameterGroupsMessage :: Encode CacheParameterGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of one of the following operations:</p> <ul> <li> <p> <code>AuthorizeCacheSecurityGroupIngress</code> </p> </li> <li> <p> <code>CreateCacheSecurityGroup</code> </p> </li> <li> <p> <code>RevokeCacheSecurityGroupIngress</code> </p> </li> </ul>
newtype CacheSecurityGroup = CacheSecurityGroup 
  { "OwnerId" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheSecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroups" :: NullOrUndefined.NullOrUndefined (EC2SecurityGroupList)
  }
derive instance newtypeCacheSecurityGroup :: Newtype CacheSecurityGroup _
derive instance repGenericCacheSecurityGroup :: Generic CacheSecurityGroup _
instance showCacheSecurityGroup :: Show CacheSecurityGroup where
  show = genericShow
instance decodeCacheSecurityGroup :: Decode CacheSecurityGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSecurityGroup :: Encode CacheSecurityGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A cache security group with the specified name already exists.</p>
newtype CacheSecurityGroupAlreadyExistsFault = CacheSecurityGroupAlreadyExistsFault Types.NoArguments
derive instance newtypeCacheSecurityGroupAlreadyExistsFault :: Newtype CacheSecurityGroupAlreadyExistsFault _
derive instance repGenericCacheSecurityGroupAlreadyExistsFault :: Generic CacheSecurityGroupAlreadyExistsFault _
instance showCacheSecurityGroupAlreadyExistsFault :: Show CacheSecurityGroupAlreadyExistsFault where
  show = genericShow
instance decodeCacheSecurityGroupAlreadyExistsFault :: Decode CacheSecurityGroupAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSecurityGroupAlreadyExistsFault :: Encode CacheSecurityGroupAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a cluster's status within a particular cache security group.</p>
newtype CacheSecurityGroupMembership = CacheSecurityGroupMembership 
  { "CacheSecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCacheSecurityGroupMembership :: Newtype CacheSecurityGroupMembership _
derive instance repGenericCacheSecurityGroupMembership :: Generic CacheSecurityGroupMembership _
instance showCacheSecurityGroupMembership :: Show CacheSecurityGroupMembership where
  show = genericShow
instance decodeCacheSecurityGroupMembership :: Decode CacheSecurityGroupMembership where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSecurityGroupMembership :: Encode CacheSecurityGroupMembership where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheSecurityGroupMembershipList = CacheSecurityGroupMembershipList (Array CacheSecurityGroupMembership)
derive instance newtypeCacheSecurityGroupMembershipList :: Newtype CacheSecurityGroupMembershipList _
derive instance repGenericCacheSecurityGroupMembershipList :: Generic CacheSecurityGroupMembershipList _
instance showCacheSecurityGroupMembershipList :: Show CacheSecurityGroupMembershipList where
  show = genericShow
instance decodeCacheSecurityGroupMembershipList :: Decode CacheSecurityGroupMembershipList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSecurityGroupMembershipList :: Encode CacheSecurityGroupMembershipList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeCacheSecurityGroups</code> operation.</p>
newtype CacheSecurityGroupMessage = CacheSecurityGroupMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheSecurityGroups" :: NullOrUndefined.NullOrUndefined (CacheSecurityGroups)
  }
derive instance newtypeCacheSecurityGroupMessage :: Newtype CacheSecurityGroupMessage _
derive instance repGenericCacheSecurityGroupMessage :: Generic CacheSecurityGroupMessage _
instance showCacheSecurityGroupMessage :: Show CacheSecurityGroupMessage where
  show = genericShow
instance decodeCacheSecurityGroupMessage :: Decode CacheSecurityGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSecurityGroupMessage :: Encode CacheSecurityGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheSecurityGroupNameList = CacheSecurityGroupNameList (Array String)
derive instance newtypeCacheSecurityGroupNameList :: Newtype CacheSecurityGroupNameList _
derive instance repGenericCacheSecurityGroupNameList :: Generic CacheSecurityGroupNameList _
instance showCacheSecurityGroupNameList :: Show CacheSecurityGroupNameList where
  show = genericShow
instance decodeCacheSecurityGroupNameList :: Decode CacheSecurityGroupNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSecurityGroupNameList :: Encode CacheSecurityGroupNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested cache security group name does not refer to an existing cache security group.</p>
newtype CacheSecurityGroupNotFoundFault = CacheSecurityGroupNotFoundFault Types.NoArguments
derive instance newtypeCacheSecurityGroupNotFoundFault :: Newtype CacheSecurityGroupNotFoundFault _
derive instance repGenericCacheSecurityGroupNotFoundFault :: Generic CacheSecurityGroupNotFoundFault _
instance showCacheSecurityGroupNotFoundFault :: Show CacheSecurityGroupNotFoundFault where
  show = genericShow
instance decodeCacheSecurityGroupNotFoundFault :: Decode CacheSecurityGroupNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSecurityGroupNotFoundFault :: Encode CacheSecurityGroupNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request cannot be processed because it would exceed the allowed number of cache security groups.</p>
newtype CacheSecurityGroupQuotaExceededFault = CacheSecurityGroupQuotaExceededFault Types.NoArguments
derive instance newtypeCacheSecurityGroupQuotaExceededFault :: Newtype CacheSecurityGroupQuotaExceededFault _
derive instance repGenericCacheSecurityGroupQuotaExceededFault :: Generic CacheSecurityGroupQuotaExceededFault _
instance showCacheSecurityGroupQuotaExceededFault :: Show CacheSecurityGroupQuotaExceededFault where
  show = genericShow
instance decodeCacheSecurityGroupQuotaExceededFault :: Decode CacheSecurityGroupQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSecurityGroupQuotaExceededFault :: Encode CacheSecurityGroupQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheSecurityGroups = CacheSecurityGroups (Array CacheSecurityGroup)
derive instance newtypeCacheSecurityGroups :: Newtype CacheSecurityGroups _
derive instance repGenericCacheSecurityGroups :: Generic CacheSecurityGroups _
instance showCacheSecurityGroups :: Show CacheSecurityGroups where
  show = genericShow
instance decodeCacheSecurityGroups :: Decode CacheSecurityGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSecurityGroups :: Encode CacheSecurityGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of one of the following operations:</p> <ul> <li> <p> <code>CreateCacheSubnetGroup</code> </p> </li> <li> <p> <code>ModifyCacheSubnetGroup</code> </p> </li> </ul>
newtype CacheSubnetGroup = CacheSubnetGroup 
  { "CacheSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheSubnetGroupDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (String)
  , "Subnets" :: NullOrUndefined.NullOrUndefined (SubnetList)
  }
derive instance newtypeCacheSubnetGroup :: Newtype CacheSubnetGroup _
derive instance repGenericCacheSubnetGroup :: Generic CacheSubnetGroup _
instance showCacheSubnetGroup :: Show CacheSubnetGroup where
  show = genericShow
instance decodeCacheSubnetGroup :: Decode CacheSubnetGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSubnetGroup :: Encode CacheSubnetGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested cache subnet group name is already in use by an existing cache subnet group.</p>
newtype CacheSubnetGroupAlreadyExistsFault = CacheSubnetGroupAlreadyExistsFault Types.NoArguments
derive instance newtypeCacheSubnetGroupAlreadyExistsFault :: Newtype CacheSubnetGroupAlreadyExistsFault _
derive instance repGenericCacheSubnetGroupAlreadyExistsFault :: Generic CacheSubnetGroupAlreadyExistsFault _
instance showCacheSubnetGroupAlreadyExistsFault :: Show CacheSubnetGroupAlreadyExistsFault where
  show = genericShow
instance decodeCacheSubnetGroupAlreadyExistsFault :: Decode CacheSubnetGroupAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSubnetGroupAlreadyExistsFault :: Encode CacheSubnetGroupAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested cache subnet group is currently in use.</p>
newtype CacheSubnetGroupInUse = CacheSubnetGroupInUse Types.NoArguments
derive instance newtypeCacheSubnetGroupInUse :: Newtype CacheSubnetGroupInUse _
derive instance repGenericCacheSubnetGroupInUse :: Generic CacheSubnetGroupInUse _
instance showCacheSubnetGroupInUse :: Show CacheSubnetGroupInUse where
  show = genericShow
instance decodeCacheSubnetGroupInUse :: Decode CacheSubnetGroupInUse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSubnetGroupInUse :: Encode CacheSubnetGroupInUse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeCacheSubnetGroups</code> operation.</p>
newtype CacheSubnetGroupMessage = CacheSubnetGroupMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheSubnetGroups" :: NullOrUndefined.NullOrUndefined (CacheSubnetGroups)
  }
derive instance newtypeCacheSubnetGroupMessage :: Newtype CacheSubnetGroupMessage _
derive instance repGenericCacheSubnetGroupMessage :: Generic CacheSubnetGroupMessage _
instance showCacheSubnetGroupMessage :: Show CacheSubnetGroupMessage where
  show = genericShow
instance decodeCacheSubnetGroupMessage :: Decode CacheSubnetGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSubnetGroupMessage :: Encode CacheSubnetGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested cache subnet group name does not refer to an existing cache subnet group.</p>
newtype CacheSubnetGroupNotFoundFault = CacheSubnetGroupNotFoundFault Types.NoArguments
derive instance newtypeCacheSubnetGroupNotFoundFault :: Newtype CacheSubnetGroupNotFoundFault _
derive instance repGenericCacheSubnetGroupNotFoundFault :: Generic CacheSubnetGroupNotFoundFault _
instance showCacheSubnetGroupNotFoundFault :: Show CacheSubnetGroupNotFoundFault where
  show = genericShow
instance decodeCacheSubnetGroupNotFoundFault :: Decode CacheSubnetGroupNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSubnetGroupNotFoundFault :: Encode CacheSubnetGroupNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request cannot be processed because it would exceed the allowed number of cache subnet groups.</p>
newtype CacheSubnetGroupQuotaExceededFault = CacheSubnetGroupQuotaExceededFault Types.NoArguments
derive instance newtypeCacheSubnetGroupQuotaExceededFault :: Newtype CacheSubnetGroupQuotaExceededFault _
derive instance repGenericCacheSubnetGroupQuotaExceededFault :: Generic CacheSubnetGroupQuotaExceededFault _
instance showCacheSubnetGroupQuotaExceededFault :: Show CacheSubnetGroupQuotaExceededFault where
  show = genericShow
instance decodeCacheSubnetGroupQuotaExceededFault :: Decode CacheSubnetGroupQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSubnetGroupQuotaExceededFault :: Encode CacheSubnetGroupQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheSubnetGroups = CacheSubnetGroups (Array CacheSubnetGroup)
derive instance newtypeCacheSubnetGroups :: Newtype CacheSubnetGroups _
derive instance repGenericCacheSubnetGroups :: Generic CacheSubnetGroups _
instance showCacheSubnetGroups :: Show CacheSubnetGroups where
  show = genericShow
instance decodeCacheSubnetGroups :: Decode CacheSubnetGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSubnetGroups :: Encode CacheSubnetGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request cannot be processed because it would exceed the allowed number of subnets in a cache subnet group.</p>
newtype CacheSubnetQuotaExceededFault = CacheSubnetQuotaExceededFault Types.NoArguments
derive instance newtypeCacheSubnetQuotaExceededFault :: Newtype CacheSubnetQuotaExceededFault _
derive instance repGenericCacheSubnetQuotaExceededFault :: Generic CacheSubnetQuotaExceededFault _
instance showCacheSubnetQuotaExceededFault :: Show CacheSubnetQuotaExceededFault where
  show = genericShow
instance decodeCacheSubnetQuotaExceededFault :: Decode CacheSubnetQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheSubnetQuotaExceededFault :: Encode CacheSubnetQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeType = ChangeType String
derive instance newtypeChangeType :: Newtype ChangeType _
derive instance repGenericChangeType :: Generic ChangeType _
instance showChangeType :: Show ChangeType where
  show = genericShow
instance decodeChangeType :: Decode ChangeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeType :: Encode ChangeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterIdList = ClusterIdList (Array String)
derive instance newtypeClusterIdList :: Newtype ClusterIdList _
derive instance repGenericClusterIdList :: Generic ClusterIdList _
instance showClusterIdList :: Show ClusterIdList where
  show = genericShow
instance decodeClusterIdList :: Decode ClusterIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterIdList :: Encode ClusterIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request cannot be processed because it would exceed the allowed number of clusters per customer.</p>
newtype ClusterQuotaForCustomerExceededFault = ClusterQuotaForCustomerExceededFault Types.NoArguments
derive instance newtypeClusterQuotaForCustomerExceededFault :: Newtype ClusterQuotaForCustomerExceededFault _
derive instance repGenericClusterQuotaForCustomerExceededFault :: Generic ClusterQuotaForCustomerExceededFault _
instance showClusterQuotaForCustomerExceededFault :: Show ClusterQuotaForCustomerExceededFault where
  show = genericShow
instance decodeClusterQuotaForCustomerExceededFault :: Decode ClusterQuotaForCustomerExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterQuotaForCustomerExceededFault :: Encode ClusterQuotaForCustomerExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>CopySnapshotMessage</code> operation.</p>
newtype CopySnapshotMessage = CopySnapshotMessage 
  { "SourceSnapshotName" :: (String)
  , "TargetSnapshotName" :: (String)
  , "TargetBucket" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCopySnapshotMessage :: Newtype CopySnapshotMessage _
derive instance repGenericCopySnapshotMessage :: Generic CopySnapshotMessage _
instance showCopySnapshotMessage :: Show CopySnapshotMessage where
  show = genericShow
instance decodeCopySnapshotMessage :: Decode CopySnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopySnapshotMessage :: Encode CopySnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopySnapshotResult = CopySnapshotResult 
  { "Snapshot" :: NullOrUndefined.NullOrUndefined (Snapshot)
  }
derive instance newtypeCopySnapshotResult :: Newtype CopySnapshotResult _
derive instance repGenericCopySnapshotResult :: Generic CopySnapshotResult _
instance showCopySnapshotResult :: Show CopySnapshotResult where
  show = genericShow
instance decodeCopySnapshotResult :: Decode CopySnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopySnapshotResult :: Encode CopySnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a CreateCacheCluster operation.</p>
newtype CreateCacheClusterMessage = CreateCacheClusterMessage 
  { "CacheClusterId" :: (String)
  , "ReplicationGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "AZMode" :: NullOrUndefined.NullOrUndefined (AZMode)
  , "PreferredAvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "PreferredAvailabilityZones" :: NullOrUndefined.NullOrUndefined (PreferredAvailabilityZoneList)
  , "NumCacheNodes" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheSecurityGroupNames" :: NullOrUndefined.NullOrUndefined (CacheSecurityGroupNameList)
  , "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (SecurityGroupIdsList)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  , "SnapshotArns" :: NullOrUndefined.NullOrUndefined (SnapshotArnsList)
  , "SnapshotName" :: NullOrUndefined.NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "NotificationTopicArn" :: NullOrUndefined.NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "SnapshotRetentionLimit" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateCacheClusterMessage :: Newtype CreateCacheClusterMessage _
derive instance repGenericCreateCacheClusterMessage :: Generic CreateCacheClusterMessage _
instance showCreateCacheClusterMessage :: Show CreateCacheClusterMessage where
  show = genericShow
instance decodeCreateCacheClusterMessage :: Decode CreateCacheClusterMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCacheClusterMessage :: Encode CreateCacheClusterMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCacheClusterResult = CreateCacheClusterResult 
  { "CacheCluster" :: NullOrUndefined.NullOrUndefined (CacheCluster)
  }
derive instance newtypeCreateCacheClusterResult :: Newtype CreateCacheClusterResult _
derive instance repGenericCreateCacheClusterResult :: Generic CreateCacheClusterResult _
instance showCreateCacheClusterResult :: Show CreateCacheClusterResult where
  show = genericShow
instance decodeCreateCacheClusterResult :: Decode CreateCacheClusterResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCacheClusterResult :: Encode CreateCacheClusterResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>CreateCacheParameterGroup</code> operation.</p>
newtype CreateCacheParameterGroupMessage = CreateCacheParameterGroupMessage 
  { "CacheParameterGroupName" :: (String)
  , "CacheParameterGroupFamily" :: (String)
  , "Description" :: (String)
  }
derive instance newtypeCreateCacheParameterGroupMessage :: Newtype CreateCacheParameterGroupMessage _
derive instance repGenericCreateCacheParameterGroupMessage :: Generic CreateCacheParameterGroupMessage _
instance showCreateCacheParameterGroupMessage :: Show CreateCacheParameterGroupMessage where
  show = genericShow
instance decodeCreateCacheParameterGroupMessage :: Decode CreateCacheParameterGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCacheParameterGroupMessage :: Encode CreateCacheParameterGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCacheParameterGroupResult = CreateCacheParameterGroupResult 
  { "CacheParameterGroup" :: NullOrUndefined.NullOrUndefined (CacheParameterGroup)
  }
derive instance newtypeCreateCacheParameterGroupResult :: Newtype CreateCacheParameterGroupResult _
derive instance repGenericCreateCacheParameterGroupResult :: Generic CreateCacheParameterGroupResult _
instance showCreateCacheParameterGroupResult :: Show CreateCacheParameterGroupResult where
  show = genericShow
instance decodeCreateCacheParameterGroupResult :: Decode CreateCacheParameterGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCacheParameterGroupResult :: Encode CreateCacheParameterGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>CreateCacheSecurityGroup</code> operation.</p>
newtype CreateCacheSecurityGroupMessage = CreateCacheSecurityGroupMessage 
  { "CacheSecurityGroupName" :: (String)
  , "Description" :: (String)
  }
derive instance newtypeCreateCacheSecurityGroupMessage :: Newtype CreateCacheSecurityGroupMessage _
derive instance repGenericCreateCacheSecurityGroupMessage :: Generic CreateCacheSecurityGroupMessage _
instance showCreateCacheSecurityGroupMessage :: Show CreateCacheSecurityGroupMessage where
  show = genericShow
instance decodeCreateCacheSecurityGroupMessage :: Decode CreateCacheSecurityGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCacheSecurityGroupMessage :: Encode CreateCacheSecurityGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCacheSecurityGroupResult = CreateCacheSecurityGroupResult 
  { "CacheSecurityGroup" :: NullOrUndefined.NullOrUndefined (CacheSecurityGroup)
  }
derive instance newtypeCreateCacheSecurityGroupResult :: Newtype CreateCacheSecurityGroupResult _
derive instance repGenericCreateCacheSecurityGroupResult :: Generic CreateCacheSecurityGroupResult _
instance showCreateCacheSecurityGroupResult :: Show CreateCacheSecurityGroupResult where
  show = genericShow
instance decodeCreateCacheSecurityGroupResult :: Decode CreateCacheSecurityGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCacheSecurityGroupResult :: Encode CreateCacheSecurityGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>CreateCacheSubnetGroup</code> operation.</p>
newtype CreateCacheSubnetGroupMessage = CreateCacheSubnetGroupMessage 
  { "CacheSubnetGroupName" :: (String)
  , "CacheSubnetGroupDescription" :: (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  }
derive instance newtypeCreateCacheSubnetGroupMessage :: Newtype CreateCacheSubnetGroupMessage _
derive instance repGenericCreateCacheSubnetGroupMessage :: Generic CreateCacheSubnetGroupMessage _
instance showCreateCacheSubnetGroupMessage :: Show CreateCacheSubnetGroupMessage where
  show = genericShow
instance decodeCreateCacheSubnetGroupMessage :: Decode CreateCacheSubnetGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCacheSubnetGroupMessage :: Encode CreateCacheSubnetGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCacheSubnetGroupResult = CreateCacheSubnetGroupResult 
  { "CacheSubnetGroup" :: NullOrUndefined.NullOrUndefined (CacheSubnetGroup)
  }
derive instance newtypeCreateCacheSubnetGroupResult :: Newtype CreateCacheSubnetGroupResult _
derive instance repGenericCreateCacheSubnetGroupResult :: Generic CreateCacheSubnetGroupResult _
instance showCreateCacheSubnetGroupResult :: Show CreateCacheSubnetGroupResult where
  show = genericShow
instance decodeCreateCacheSubnetGroupResult :: Decode CreateCacheSubnetGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCacheSubnetGroupResult :: Encode CreateCacheSubnetGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>CreateReplicationGroup</code> operation.</p>
newtype CreateReplicationGroupMessage = CreateReplicationGroupMessage 
  { "ReplicationGroupId" :: (String)
  , "ReplicationGroupDescription" :: (String)
  , "PrimaryClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "AutomaticFailoverEnabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "NumCacheClusters" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "PreferredCacheClusterAZs" :: NullOrUndefined.NullOrUndefined (AvailabilityZonesList)
  , "NumNodeGroups" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "ReplicasPerNodeGroup" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "NodeGroupConfiguration" :: NullOrUndefined.NullOrUndefined (NodeGroupConfigurationList)
  , "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheSecurityGroupNames" :: NullOrUndefined.NullOrUndefined (CacheSecurityGroupNameList)
  , "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (SecurityGroupIdsList)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  , "SnapshotArns" :: NullOrUndefined.NullOrUndefined (SnapshotArnsList)
  , "SnapshotName" :: NullOrUndefined.NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "NotificationTopicArn" :: NullOrUndefined.NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "SnapshotRetentionLimit" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthToken" :: NullOrUndefined.NullOrUndefined (String)
  , "TransitEncryptionEnabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "AtRestEncryptionEnabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeCreateReplicationGroupMessage :: Newtype CreateReplicationGroupMessage _
derive instance repGenericCreateReplicationGroupMessage :: Generic CreateReplicationGroupMessage _
instance showCreateReplicationGroupMessage :: Show CreateReplicationGroupMessage where
  show = genericShow
instance decodeCreateReplicationGroupMessage :: Decode CreateReplicationGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateReplicationGroupMessage :: Encode CreateReplicationGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateReplicationGroupResult = CreateReplicationGroupResult 
  { "ReplicationGroup" :: NullOrUndefined.NullOrUndefined (ReplicationGroup)
  }
derive instance newtypeCreateReplicationGroupResult :: Newtype CreateReplicationGroupResult _
derive instance repGenericCreateReplicationGroupResult :: Generic CreateReplicationGroupResult _
instance showCreateReplicationGroupResult :: Show CreateReplicationGroupResult where
  show = genericShow
instance decodeCreateReplicationGroupResult :: Decode CreateReplicationGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateReplicationGroupResult :: Encode CreateReplicationGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>CreateSnapshot</code> operation.</p>
newtype CreateSnapshotMessage = CreateSnapshotMessage 
  { "ReplicationGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotName" :: (String)
  }
derive instance newtypeCreateSnapshotMessage :: Newtype CreateSnapshotMessage _
derive instance repGenericCreateSnapshotMessage :: Generic CreateSnapshotMessage _
instance showCreateSnapshotMessage :: Show CreateSnapshotMessage where
  show = genericShow
instance decodeCreateSnapshotMessage :: Decode CreateSnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSnapshotMessage :: Encode CreateSnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSnapshotResult = CreateSnapshotResult 
  { "Snapshot" :: NullOrUndefined.NullOrUndefined (Snapshot)
  }
derive instance newtypeCreateSnapshotResult :: Newtype CreateSnapshotResult _
derive instance repGenericCreateSnapshotResult :: Generic CreateSnapshotResult _
instance showCreateSnapshotResult :: Show CreateSnapshotResult where
  show = genericShow
instance decodeCreateSnapshotResult :: Decode CreateSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSnapshotResult :: Encode CreateSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DeleteCacheCluster</code> operation.</p>
newtype DeleteCacheClusterMessage = DeleteCacheClusterMessage 
  { "CacheClusterId" :: (String)
  , "FinalSnapshotIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDeleteCacheClusterMessage :: Newtype DeleteCacheClusterMessage _
derive instance repGenericDeleteCacheClusterMessage :: Generic DeleteCacheClusterMessage _
instance showDeleteCacheClusterMessage :: Show DeleteCacheClusterMessage where
  show = genericShow
instance decodeDeleteCacheClusterMessage :: Decode DeleteCacheClusterMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCacheClusterMessage :: Encode DeleteCacheClusterMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCacheClusterResult = DeleteCacheClusterResult 
  { "CacheCluster" :: NullOrUndefined.NullOrUndefined (CacheCluster)
  }
derive instance newtypeDeleteCacheClusterResult :: Newtype DeleteCacheClusterResult _
derive instance repGenericDeleteCacheClusterResult :: Generic DeleteCacheClusterResult _
instance showDeleteCacheClusterResult :: Show DeleteCacheClusterResult where
  show = genericShow
instance decodeDeleteCacheClusterResult :: Decode DeleteCacheClusterResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCacheClusterResult :: Encode DeleteCacheClusterResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DeleteCacheParameterGroup</code> operation.</p>
newtype DeleteCacheParameterGroupMessage = DeleteCacheParameterGroupMessage 
  { "CacheParameterGroupName" :: (String)
  }
derive instance newtypeDeleteCacheParameterGroupMessage :: Newtype DeleteCacheParameterGroupMessage _
derive instance repGenericDeleteCacheParameterGroupMessage :: Generic DeleteCacheParameterGroupMessage _
instance showDeleteCacheParameterGroupMessage :: Show DeleteCacheParameterGroupMessage where
  show = genericShow
instance decodeDeleteCacheParameterGroupMessage :: Decode DeleteCacheParameterGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCacheParameterGroupMessage :: Encode DeleteCacheParameterGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DeleteCacheSecurityGroup</code> operation.</p>
newtype DeleteCacheSecurityGroupMessage = DeleteCacheSecurityGroupMessage 
  { "CacheSecurityGroupName" :: (String)
  }
derive instance newtypeDeleteCacheSecurityGroupMessage :: Newtype DeleteCacheSecurityGroupMessage _
derive instance repGenericDeleteCacheSecurityGroupMessage :: Generic DeleteCacheSecurityGroupMessage _
instance showDeleteCacheSecurityGroupMessage :: Show DeleteCacheSecurityGroupMessage where
  show = genericShow
instance decodeDeleteCacheSecurityGroupMessage :: Decode DeleteCacheSecurityGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCacheSecurityGroupMessage :: Encode DeleteCacheSecurityGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DeleteCacheSubnetGroup</code> operation.</p>
newtype DeleteCacheSubnetGroupMessage = DeleteCacheSubnetGroupMessage 
  { "CacheSubnetGroupName" :: (String)
  }
derive instance newtypeDeleteCacheSubnetGroupMessage :: Newtype DeleteCacheSubnetGroupMessage _
derive instance repGenericDeleteCacheSubnetGroupMessage :: Generic DeleteCacheSubnetGroupMessage _
instance showDeleteCacheSubnetGroupMessage :: Show DeleteCacheSubnetGroupMessage where
  show = genericShow
instance decodeDeleteCacheSubnetGroupMessage :: Decode DeleteCacheSubnetGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCacheSubnetGroupMessage :: Encode DeleteCacheSubnetGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DeleteReplicationGroup</code> operation.</p>
newtype DeleteReplicationGroupMessage = DeleteReplicationGroupMessage 
  { "ReplicationGroupId" :: (String)
  , "RetainPrimaryCluster" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "FinalSnapshotIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDeleteReplicationGroupMessage :: Newtype DeleteReplicationGroupMessage _
derive instance repGenericDeleteReplicationGroupMessage :: Generic DeleteReplicationGroupMessage _
instance showDeleteReplicationGroupMessage :: Show DeleteReplicationGroupMessage where
  show = genericShow
instance decodeDeleteReplicationGroupMessage :: Decode DeleteReplicationGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReplicationGroupMessage :: Encode DeleteReplicationGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteReplicationGroupResult = DeleteReplicationGroupResult 
  { "ReplicationGroup" :: NullOrUndefined.NullOrUndefined (ReplicationGroup)
  }
derive instance newtypeDeleteReplicationGroupResult :: Newtype DeleteReplicationGroupResult _
derive instance repGenericDeleteReplicationGroupResult :: Generic DeleteReplicationGroupResult _
instance showDeleteReplicationGroupResult :: Show DeleteReplicationGroupResult where
  show = genericShow
instance decodeDeleteReplicationGroupResult :: Decode DeleteReplicationGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReplicationGroupResult :: Encode DeleteReplicationGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DeleteSnapshot</code> operation.</p>
newtype DeleteSnapshotMessage = DeleteSnapshotMessage 
  { "SnapshotName" :: (String)
  }
derive instance newtypeDeleteSnapshotMessage :: Newtype DeleteSnapshotMessage _
derive instance repGenericDeleteSnapshotMessage :: Generic DeleteSnapshotMessage _
instance showDeleteSnapshotMessage :: Show DeleteSnapshotMessage where
  show = genericShow
instance decodeDeleteSnapshotMessage :: Decode DeleteSnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSnapshotMessage :: Encode DeleteSnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSnapshotResult = DeleteSnapshotResult 
  { "Snapshot" :: NullOrUndefined.NullOrUndefined (Snapshot)
  }
derive instance newtypeDeleteSnapshotResult :: Newtype DeleteSnapshotResult _
derive instance repGenericDeleteSnapshotResult :: Generic DeleteSnapshotResult _
instance showDeleteSnapshotResult :: Show DeleteSnapshotResult where
  show = genericShow
instance decodeDeleteSnapshotResult :: Decode DeleteSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSnapshotResult :: Encode DeleteSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeCacheClusters</code> operation.</p>
newtype DescribeCacheClustersMessage = DescribeCacheClustersMessage 
  { "CacheClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "ShowCacheNodeInfo" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "ShowCacheClustersNotInReplicationGroups" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeDescribeCacheClustersMessage :: Newtype DescribeCacheClustersMessage _
derive instance repGenericDescribeCacheClustersMessage :: Generic DescribeCacheClustersMessage _
instance showDescribeCacheClustersMessage :: Show DescribeCacheClustersMessage where
  show = genericShow
instance decodeDescribeCacheClustersMessage :: Decode DescribeCacheClustersMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCacheClustersMessage :: Encode DescribeCacheClustersMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeCacheEngineVersions</code> operation.</p>
newtype DescribeCacheEngineVersionsMessage = DescribeCacheEngineVersionsMessage 
  { "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheParameterGroupFamily" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultOnly" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeDescribeCacheEngineVersionsMessage :: Newtype DescribeCacheEngineVersionsMessage _
derive instance repGenericDescribeCacheEngineVersionsMessage :: Generic DescribeCacheEngineVersionsMessage _
instance showDescribeCacheEngineVersionsMessage :: Show DescribeCacheEngineVersionsMessage where
  show = genericShow
instance decodeDescribeCacheEngineVersionsMessage :: Decode DescribeCacheEngineVersionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCacheEngineVersionsMessage :: Encode DescribeCacheEngineVersionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeCacheParameterGroups</code> operation.</p>
newtype DescribeCacheParameterGroupsMessage = DescribeCacheParameterGroupsMessage 
  { "CacheParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeCacheParameterGroupsMessage :: Newtype DescribeCacheParameterGroupsMessage _
derive instance repGenericDescribeCacheParameterGroupsMessage :: Generic DescribeCacheParameterGroupsMessage _
instance showDescribeCacheParameterGroupsMessage :: Show DescribeCacheParameterGroupsMessage where
  show = genericShow
instance decodeDescribeCacheParameterGroupsMessage :: Decode DescribeCacheParameterGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCacheParameterGroupsMessage :: Encode DescribeCacheParameterGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeCacheParameters</code> operation.</p>
newtype DescribeCacheParametersMessage = DescribeCacheParametersMessage 
  { "CacheParameterGroupName" :: (String)
  , "Source" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeCacheParametersMessage :: Newtype DescribeCacheParametersMessage _
derive instance repGenericDescribeCacheParametersMessage :: Generic DescribeCacheParametersMessage _
instance showDescribeCacheParametersMessage :: Show DescribeCacheParametersMessage where
  show = genericShow
instance decodeDescribeCacheParametersMessage :: Decode DescribeCacheParametersMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCacheParametersMessage :: Encode DescribeCacheParametersMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeCacheSecurityGroups</code> operation.</p>
newtype DescribeCacheSecurityGroupsMessage = DescribeCacheSecurityGroupsMessage 
  { "CacheSecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeCacheSecurityGroupsMessage :: Newtype DescribeCacheSecurityGroupsMessage _
derive instance repGenericDescribeCacheSecurityGroupsMessage :: Generic DescribeCacheSecurityGroupsMessage _
instance showDescribeCacheSecurityGroupsMessage :: Show DescribeCacheSecurityGroupsMessage where
  show = genericShow
instance decodeDescribeCacheSecurityGroupsMessage :: Decode DescribeCacheSecurityGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCacheSecurityGroupsMessage :: Encode DescribeCacheSecurityGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeCacheSubnetGroups</code> operation.</p>
newtype DescribeCacheSubnetGroupsMessage = DescribeCacheSubnetGroupsMessage 
  { "CacheSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeCacheSubnetGroupsMessage :: Newtype DescribeCacheSubnetGroupsMessage _
derive instance repGenericDescribeCacheSubnetGroupsMessage :: Generic DescribeCacheSubnetGroupsMessage _
instance showDescribeCacheSubnetGroupsMessage :: Show DescribeCacheSubnetGroupsMessage where
  show = genericShow
instance decodeDescribeCacheSubnetGroupsMessage :: Decode DescribeCacheSubnetGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCacheSubnetGroupsMessage :: Encode DescribeCacheSubnetGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeEngineDefaultParameters</code> operation.</p>
newtype DescribeEngineDefaultParametersMessage = DescribeEngineDefaultParametersMessage 
  { "CacheParameterGroupFamily" :: (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeEngineDefaultParametersMessage :: Newtype DescribeEngineDefaultParametersMessage _
derive instance repGenericDescribeEngineDefaultParametersMessage :: Generic DescribeEngineDefaultParametersMessage _
instance showDescribeEngineDefaultParametersMessage :: Show DescribeEngineDefaultParametersMessage where
  show = genericShow
instance decodeDescribeEngineDefaultParametersMessage :: Decode DescribeEngineDefaultParametersMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEngineDefaultParametersMessage :: Encode DescribeEngineDefaultParametersMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEngineDefaultParametersResult = DescribeEngineDefaultParametersResult 
  { "EngineDefaults" :: NullOrUndefined.NullOrUndefined (EngineDefaults)
  }
derive instance newtypeDescribeEngineDefaultParametersResult :: Newtype DescribeEngineDefaultParametersResult _
derive instance repGenericDescribeEngineDefaultParametersResult :: Generic DescribeEngineDefaultParametersResult _
instance showDescribeEngineDefaultParametersResult :: Show DescribeEngineDefaultParametersResult where
  show = genericShow
instance decodeDescribeEngineDefaultParametersResult :: Decode DescribeEngineDefaultParametersResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEngineDefaultParametersResult :: Encode DescribeEngineDefaultParametersResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeEvents</code> operation.</p>
newtype DescribeEventsMessage = DescribeEventsMessage 
  { "SourceIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (SourceType)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "Duration" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeEventsMessage :: Newtype DescribeEventsMessage _
derive instance repGenericDescribeEventsMessage :: Generic DescribeEventsMessage _
instance showDescribeEventsMessage :: Show DescribeEventsMessage where
  show = genericShow
instance decodeDescribeEventsMessage :: Decode DescribeEventsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventsMessage :: Encode DescribeEventsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeReplicationGroups</code> operation.</p>
newtype DescribeReplicationGroupsMessage = DescribeReplicationGroupsMessage 
  { "ReplicationGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeReplicationGroupsMessage :: Newtype DescribeReplicationGroupsMessage _
derive instance repGenericDescribeReplicationGroupsMessage :: Generic DescribeReplicationGroupsMessage _
instance showDescribeReplicationGroupsMessage :: Show DescribeReplicationGroupsMessage where
  show = genericShow
instance decodeDescribeReplicationGroupsMessage :: Decode DescribeReplicationGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReplicationGroupsMessage :: Encode DescribeReplicationGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeReservedCacheNodes</code> operation.</p>
newtype DescribeReservedCacheNodesMessage = DescribeReservedCacheNodesMessage 
  { "ReservedCacheNodeId" :: NullOrUndefined.NullOrUndefined (String)
  , "ReservedCacheNodesOfferingId" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "Duration" :: NullOrUndefined.NullOrUndefined (String)
  , "ProductDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeReservedCacheNodesMessage :: Newtype DescribeReservedCacheNodesMessage _
derive instance repGenericDescribeReservedCacheNodesMessage :: Generic DescribeReservedCacheNodesMessage _
instance showDescribeReservedCacheNodesMessage :: Show DescribeReservedCacheNodesMessage where
  show = genericShow
instance decodeDescribeReservedCacheNodesMessage :: Decode DescribeReservedCacheNodesMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReservedCacheNodesMessage :: Encode DescribeReservedCacheNodesMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeReservedCacheNodesOfferings</code> operation.</p>
newtype DescribeReservedCacheNodesOfferingsMessage = DescribeReservedCacheNodesOfferingsMessage 
  { "ReservedCacheNodesOfferingId" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "Duration" :: NullOrUndefined.NullOrUndefined (String)
  , "ProductDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeReservedCacheNodesOfferingsMessage :: Newtype DescribeReservedCacheNodesOfferingsMessage _
derive instance repGenericDescribeReservedCacheNodesOfferingsMessage :: Generic DescribeReservedCacheNodesOfferingsMessage _
instance showDescribeReservedCacheNodesOfferingsMessage :: Show DescribeReservedCacheNodesOfferingsMessage where
  show = genericShow
instance decodeDescribeReservedCacheNodesOfferingsMessage :: Decode DescribeReservedCacheNodesOfferingsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReservedCacheNodesOfferingsMessage :: Encode DescribeReservedCacheNodesOfferingsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeSnapshots</code> operation.</p>
newtype DescribeSnapshotsListMessage = DescribeSnapshotsListMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "Snapshots" :: NullOrUndefined.NullOrUndefined (SnapshotList)
  }
derive instance newtypeDescribeSnapshotsListMessage :: Newtype DescribeSnapshotsListMessage _
derive instance repGenericDescribeSnapshotsListMessage :: Generic DescribeSnapshotsListMessage _
instance showDescribeSnapshotsListMessage :: Show DescribeSnapshotsListMessage where
  show = genericShow
instance decodeDescribeSnapshotsListMessage :: Decode DescribeSnapshotsListMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSnapshotsListMessage :: Encode DescribeSnapshotsListMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeSnapshotsMessage</code> operation.</p>
newtype DescribeSnapshotsMessage = DescribeSnapshotsMessage 
  { "ReplicationGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotName" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotSource" :: NullOrUndefined.NullOrUndefined (String)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "ShowNodeGroupConfig" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeDescribeSnapshotsMessage :: Newtype DescribeSnapshotsMessage _
derive instance repGenericDescribeSnapshotsMessage :: Generic DescribeSnapshotsMessage _
instance showDescribeSnapshotsMessage :: Show DescribeSnapshotsMessage where
  show = genericShow
instance decodeDescribeSnapshotsMessage :: Decode DescribeSnapshotsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSnapshotsMessage :: Encode DescribeSnapshotsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides ownership and status information for an Amazon EC2 security group.</p>
newtype EC2SecurityGroup = EC2SecurityGroup 
  { "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEC2SecurityGroup :: Newtype EC2SecurityGroup _
derive instance repGenericEC2SecurityGroup :: Generic EC2SecurityGroup _
instance showEC2SecurityGroup :: Show EC2SecurityGroup where
  show = genericShow
instance decodeEC2SecurityGroup :: Decode EC2SecurityGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEC2SecurityGroup :: Encode EC2SecurityGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EC2SecurityGroupList = EC2SecurityGroupList (Array EC2SecurityGroup)
derive instance newtypeEC2SecurityGroupList :: Newtype EC2SecurityGroupList _
derive instance repGenericEC2SecurityGroupList :: Generic EC2SecurityGroupList _
instance showEC2SecurityGroupList :: Show EC2SecurityGroupList where
  show = genericShow
instance decodeEC2SecurityGroupList :: Decode EC2SecurityGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEC2SecurityGroupList :: Encode EC2SecurityGroupList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the information required for client programs to connect to a cache node.</p>
newtype Endpoint = Endpoint 
  { "Address" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeEndpoint :: Newtype Endpoint _
derive instance repGenericEndpoint :: Generic Endpoint _
instance showEndpoint :: Show Endpoint where
  show = genericShow
instance decodeEndpoint :: Decode Endpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpoint :: Encode Endpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeEngineDefaultParameters</code> operation.</p>
newtype EngineDefaults = EngineDefaults 
  { "CacheParameterGroupFamily" :: NullOrUndefined.NullOrUndefined (String)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersList)
  , "CacheNodeTypeSpecificParameters" :: NullOrUndefined.NullOrUndefined (CacheNodeTypeSpecificParametersList)
  }
derive instance newtypeEngineDefaults :: Newtype EngineDefaults _
derive instance repGenericEngineDefaults :: Generic EngineDefaults _
instance showEngineDefaults :: Show EngineDefaults where
  show = genericShow
instance decodeEngineDefaults :: Decode EngineDefaults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEngineDefaults :: Encode EngineDefaults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a single occurrence of something interesting within the system. Some examples of events are creating a cluster, adding or removing a cache node, or rebooting a node.</p>
newtype Event = Event 
  { "SourceIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (SourceType)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "Date" :: NullOrUndefined.NullOrUndefined (TStamp)
  }
derive instance newtypeEvent :: Newtype Event _
derive instance repGenericEvent :: Generic Event _
instance showEvent :: Show Event where
  show = genericShow
instance decodeEvent :: Decode Event where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvent :: Encode Event where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventList = EventList (Array Event)
derive instance newtypeEventList :: Newtype EventList _
derive instance repGenericEventList :: Generic EventList _
instance showEventList :: Show EventList where
  show = genericShow
instance decodeEventList :: Decode EventList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventList :: Encode EventList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeEvents</code> operation.</p>
newtype EventsMessage = EventsMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "Events" :: NullOrUndefined.NullOrUndefined (EventList)
  }
derive instance newtypeEventsMessage :: Newtype EventsMessage _
derive instance repGenericEventsMessage :: Generic EventsMessage _
instance showEventsMessage :: Show EventsMessage where
  show = genericShow
instance decodeEventsMessage :: Decode EventsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventsMessage :: Encode EventsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested cache node type is not available in the specified Availability Zone.</p>
newtype InsufficientCacheClusterCapacityFault = InsufficientCacheClusterCapacityFault Types.NoArguments
derive instance newtypeInsufficientCacheClusterCapacityFault :: Newtype InsufficientCacheClusterCapacityFault _
derive instance repGenericInsufficientCacheClusterCapacityFault :: Generic InsufficientCacheClusterCapacityFault _
instance showInsufficientCacheClusterCapacityFault :: Show InsufficientCacheClusterCapacityFault where
  show = genericShow
instance decodeInsufficientCacheClusterCapacityFault :: Decode InsufficientCacheClusterCapacityFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientCacheClusterCapacityFault :: Encode InsufficientCacheClusterCapacityFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntegerOptional = IntegerOptional Int
derive instance newtypeIntegerOptional :: Newtype IntegerOptional _
derive instance repGenericIntegerOptional :: Generic IntegerOptional _
instance showIntegerOptional :: Show IntegerOptional where
  show = genericShow
instance decodeIntegerOptional :: Decode IntegerOptional where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntegerOptional :: Encode IntegerOptional where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested Amazon Resource Name (ARN) does not refer to an existing resource.</p>
newtype InvalidARNFault = InvalidARNFault Types.NoArguments
derive instance newtypeInvalidARNFault :: Newtype InvalidARNFault _
derive instance repGenericInvalidARNFault :: Generic InvalidARNFault _
instance showInvalidARNFault :: Show InvalidARNFault where
  show = genericShow
instance decodeInvalidARNFault :: Decode InvalidARNFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidARNFault :: Encode InvalidARNFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested cluster is not in the <code>available</code> state.</p>
newtype InvalidCacheClusterStateFault = InvalidCacheClusterStateFault Types.NoArguments
derive instance newtypeInvalidCacheClusterStateFault :: Newtype InvalidCacheClusterStateFault _
derive instance repGenericInvalidCacheClusterStateFault :: Generic InvalidCacheClusterStateFault _
instance showInvalidCacheClusterStateFault :: Show InvalidCacheClusterStateFault where
  show = genericShow
instance decodeInvalidCacheClusterStateFault :: Decode InvalidCacheClusterStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCacheClusterStateFault :: Encode InvalidCacheClusterStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The current state of the cache parameter group does not allow the requested operation to occur.</p>
newtype InvalidCacheParameterGroupStateFault = InvalidCacheParameterGroupStateFault Types.NoArguments
derive instance newtypeInvalidCacheParameterGroupStateFault :: Newtype InvalidCacheParameterGroupStateFault _
derive instance repGenericInvalidCacheParameterGroupStateFault :: Generic InvalidCacheParameterGroupStateFault _
instance showInvalidCacheParameterGroupStateFault :: Show InvalidCacheParameterGroupStateFault where
  show = genericShow
instance decodeInvalidCacheParameterGroupStateFault :: Decode InvalidCacheParameterGroupStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCacheParameterGroupStateFault :: Encode InvalidCacheParameterGroupStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The current state of the cache security group does not allow deletion.</p>
newtype InvalidCacheSecurityGroupStateFault = InvalidCacheSecurityGroupStateFault Types.NoArguments
derive instance newtypeInvalidCacheSecurityGroupStateFault :: Newtype InvalidCacheSecurityGroupStateFault _
derive instance repGenericInvalidCacheSecurityGroupStateFault :: Generic InvalidCacheSecurityGroupStateFault _
instance showInvalidCacheSecurityGroupStateFault :: Show InvalidCacheSecurityGroupStateFault where
  show = genericShow
instance decodeInvalidCacheSecurityGroupStateFault :: Decode InvalidCacheSecurityGroupStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCacheSecurityGroupStateFault :: Encode InvalidCacheSecurityGroupStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Two or more incompatible parameters were specified.</p>
newtype InvalidParameterCombinationException = InvalidParameterCombinationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (AwsQueryErrorMessage)
  }
derive instance newtypeInvalidParameterCombinationException :: Newtype InvalidParameterCombinationException _
derive instance repGenericInvalidParameterCombinationException :: Generic InvalidParameterCombinationException _
instance showInvalidParameterCombinationException :: Show InvalidParameterCombinationException where
  show = genericShow
instance decodeInvalidParameterCombinationException :: Decode InvalidParameterCombinationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterCombinationException :: Encode InvalidParameterCombinationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value for a parameter is invalid.</p>
newtype InvalidParameterValueException = InvalidParameterValueException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (AwsQueryErrorMessage)
  }
derive instance newtypeInvalidParameterValueException :: Newtype InvalidParameterValueException _
derive instance repGenericInvalidParameterValueException :: Generic InvalidParameterValueException _
instance showInvalidParameterValueException :: Show InvalidParameterValueException where
  show = genericShow
instance decodeInvalidParameterValueException :: Decode InvalidParameterValueException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterValueException :: Encode InvalidParameterValueException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested replication group is not in the <code>available</code> state.</p>
newtype InvalidReplicationGroupStateFault = InvalidReplicationGroupStateFault Types.NoArguments
derive instance newtypeInvalidReplicationGroupStateFault :: Newtype InvalidReplicationGroupStateFault _
derive instance repGenericInvalidReplicationGroupStateFault :: Generic InvalidReplicationGroupStateFault _
instance showInvalidReplicationGroupStateFault :: Show InvalidReplicationGroupStateFault where
  show = genericShow
instance decodeInvalidReplicationGroupStateFault :: Decode InvalidReplicationGroupStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidReplicationGroupStateFault :: Encode InvalidReplicationGroupStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The current state of the snapshot does not allow the requested operation to occur.</p>
newtype InvalidSnapshotStateFault = InvalidSnapshotStateFault Types.NoArguments
derive instance newtypeInvalidSnapshotStateFault :: Newtype InvalidSnapshotStateFault _
derive instance repGenericInvalidSnapshotStateFault :: Generic InvalidSnapshotStateFault _
instance showInvalidSnapshotStateFault :: Show InvalidSnapshotStateFault where
  show = genericShow
instance decodeInvalidSnapshotStateFault :: Decode InvalidSnapshotStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSnapshotStateFault :: Encode InvalidSnapshotStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An invalid subnet identifier was specified.</p>
newtype InvalidSubnet = InvalidSubnet Types.NoArguments
derive instance newtypeInvalidSubnet :: Newtype InvalidSubnet _
derive instance repGenericInvalidSubnet :: Generic InvalidSubnet _
instance showInvalidSubnet :: Show InvalidSubnet where
  show = genericShow
instance decodeInvalidSubnet :: Decode InvalidSubnet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSubnet :: Encode InvalidSubnet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The VPC network is in an invalid state.</p>
newtype InvalidVPCNetworkStateFault = InvalidVPCNetworkStateFault Types.NoArguments
derive instance newtypeInvalidVPCNetworkStateFault :: Newtype InvalidVPCNetworkStateFault _
derive instance repGenericInvalidVPCNetworkStateFault :: Generic InvalidVPCNetworkStateFault _
instance showInvalidVPCNetworkStateFault :: Show InvalidVPCNetworkStateFault where
  show = genericShow
instance decodeInvalidVPCNetworkStateFault :: Decode InvalidVPCNetworkStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidVPCNetworkStateFault :: Encode InvalidVPCNetworkStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyList = KeyList (Array String)
derive instance newtypeKeyList :: Newtype KeyList _
derive instance repGenericKeyList :: Generic KeyList _
instance showKeyList :: Show KeyList where
  show = genericShow
instance decodeKeyList :: Decode KeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyList :: Encode KeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input parameters for the <code>ListAllowedNodeTypeModifications</code> operation.</p>
newtype ListAllowedNodeTypeModificationsMessage = ListAllowedNodeTypeModificationsMessage 
  { "CacheClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "ReplicationGroupId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListAllowedNodeTypeModificationsMessage :: Newtype ListAllowedNodeTypeModificationsMessage _
derive instance repGenericListAllowedNodeTypeModificationsMessage :: Generic ListAllowedNodeTypeModificationsMessage _
instance showListAllowedNodeTypeModificationsMessage :: Show ListAllowedNodeTypeModificationsMessage where
  show = genericShow
instance decodeListAllowedNodeTypeModificationsMessage :: Decode ListAllowedNodeTypeModificationsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAllowedNodeTypeModificationsMessage :: Encode ListAllowedNodeTypeModificationsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input parameters for the <code>ListTagsForResource</code> operation.</p>
newtype ListTagsForResourceMessage = ListTagsForResourceMessage 
  { "ResourceName" :: (String)
  }
derive instance newtypeListTagsForResourceMessage :: Newtype ListTagsForResourceMessage _
derive instance repGenericListTagsForResourceMessage :: Generic ListTagsForResourceMessage _
instance showListTagsForResourceMessage :: Show ListTagsForResourceMessage where
  show = genericShow
instance decodeListTagsForResourceMessage :: Decode ListTagsForResourceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceMessage :: Encode ListTagsForResourceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>ModifyCacheCluster</code> operation.</p>
newtype ModifyCacheClusterMessage = ModifyCacheClusterMessage 
  { "CacheClusterId" :: (String)
  , "NumCacheNodes" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "CacheNodeIdsToRemove" :: NullOrUndefined.NullOrUndefined (CacheNodeIdsList)
  , "AZMode" :: NullOrUndefined.NullOrUndefined (AZMode)
  , "NewAvailabilityZones" :: NullOrUndefined.NullOrUndefined (PreferredAvailabilityZoneList)
  , "CacheSecurityGroupNames" :: NullOrUndefined.NullOrUndefined (CacheSecurityGroupNameList)
  , "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (SecurityGroupIdsList)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "NotificationTopicArn" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "NotificationTopicStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "ApplyImmediately" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "SnapshotRetentionLimit" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeModifyCacheClusterMessage :: Newtype ModifyCacheClusterMessage _
derive instance repGenericModifyCacheClusterMessage :: Generic ModifyCacheClusterMessage _
instance showModifyCacheClusterMessage :: Show ModifyCacheClusterMessage where
  show = genericShow
instance decodeModifyCacheClusterMessage :: Decode ModifyCacheClusterMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyCacheClusterMessage :: Encode ModifyCacheClusterMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyCacheClusterResult = ModifyCacheClusterResult 
  { "CacheCluster" :: NullOrUndefined.NullOrUndefined (CacheCluster)
  }
derive instance newtypeModifyCacheClusterResult :: Newtype ModifyCacheClusterResult _
derive instance repGenericModifyCacheClusterResult :: Generic ModifyCacheClusterResult _
instance showModifyCacheClusterResult :: Show ModifyCacheClusterResult where
  show = genericShow
instance decodeModifyCacheClusterResult :: Decode ModifyCacheClusterResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyCacheClusterResult :: Encode ModifyCacheClusterResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>ModifyCacheParameterGroup</code> operation.</p>
newtype ModifyCacheParameterGroupMessage = ModifyCacheParameterGroupMessage 
  { "CacheParameterGroupName" :: (String)
  , "ParameterNameValues" :: (ParameterNameValueList)
  }
derive instance newtypeModifyCacheParameterGroupMessage :: Newtype ModifyCacheParameterGroupMessage _
derive instance repGenericModifyCacheParameterGroupMessage :: Generic ModifyCacheParameterGroupMessage _
instance showModifyCacheParameterGroupMessage :: Show ModifyCacheParameterGroupMessage where
  show = genericShow
instance decodeModifyCacheParameterGroupMessage :: Decode ModifyCacheParameterGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyCacheParameterGroupMessage :: Encode ModifyCacheParameterGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>ModifyCacheSubnetGroup</code> operation.</p>
newtype ModifyCacheSubnetGroupMessage = ModifyCacheSubnetGroupMessage 
  { "CacheSubnetGroupName" :: (String)
  , "CacheSubnetGroupDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "SubnetIds" :: NullOrUndefined.NullOrUndefined (SubnetIdentifierList)
  }
derive instance newtypeModifyCacheSubnetGroupMessage :: Newtype ModifyCacheSubnetGroupMessage _
derive instance repGenericModifyCacheSubnetGroupMessage :: Generic ModifyCacheSubnetGroupMessage _
instance showModifyCacheSubnetGroupMessage :: Show ModifyCacheSubnetGroupMessage where
  show = genericShow
instance decodeModifyCacheSubnetGroupMessage :: Decode ModifyCacheSubnetGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyCacheSubnetGroupMessage :: Encode ModifyCacheSubnetGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyCacheSubnetGroupResult = ModifyCacheSubnetGroupResult 
  { "CacheSubnetGroup" :: NullOrUndefined.NullOrUndefined (CacheSubnetGroup)
  }
derive instance newtypeModifyCacheSubnetGroupResult :: Newtype ModifyCacheSubnetGroupResult _
derive instance repGenericModifyCacheSubnetGroupResult :: Generic ModifyCacheSubnetGroupResult _
instance showModifyCacheSubnetGroupResult :: Show ModifyCacheSubnetGroupResult where
  show = genericShow
instance decodeModifyCacheSubnetGroupResult :: Decode ModifyCacheSubnetGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyCacheSubnetGroupResult :: Encode ModifyCacheSubnetGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>ModifyReplicationGroups</code> operation.</p>
newtype ModifyReplicationGroupMessage = ModifyReplicationGroupMessage 
  { "ReplicationGroupId" :: (String)
  , "ReplicationGroupDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "PrimaryClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshottingClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "AutomaticFailoverEnabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "CacheSecurityGroupNames" :: NullOrUndefined.NullOrUndefined (CacheSecurityGroupNameList)
  , "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (SecurityGroupIdsList)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "NotificationTopicArn" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "NotificationTopicStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "ApplyImmediately" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "SnapshotRetentionLimit" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeGroupId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeModifyReplicationGroupMessage :: Newtype ModifyReplicationGroupMessage _
derive instance repGenericModifyReplicationGroupMessage :: Generic ModifyReplicationGroupMessage _
instance showModifyReplicationGroupMessage :: Show ModifyReplicationGroupMessage where
  show = genericShow
instance decodeModifyReplicationGroupMessage :: Decode ModifyReplicationGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyReplicationGroupMessage :: Encode ModifyReplicationGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyReplicationGroupResult = ModifyReplicationGroupResult 
  { "ReplicationGroup" :: NullOrUndefined.NullOrUndefined (ReplicationGroup)
  }
derive instance newtypeModifyReplicationGroupResult :: Newtype ModifyReplicationGroupResult _
derive instance repGenericModifyReplicationGroupResult :: Generic ModifyReplicationGroupResult _
instance showModifyReplicationGroupResult :: Show ModifyReplicationGroupResult where
  show = genericShow
instance decodeModifyReplicationGroupResult :: Decode ModifyReplicationGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyReplicationGroupResult :: Encode ModifyReplicationGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a <code>ModifyReplicationGroupShardConfiguration</code> operation.</p>
newtype ModifyReplicationGroupShardConfigurationMessage = ModifyReplicationGroupShardConfigurationMessage 
  { "ReplicationGroupId" :: (String)
  , "NodeGroupCount" :: (Int)
  , "ApplyImmediately" :: (Boolean)
  , "ReshardingConfiguration" :: NullOrUndefined.NullOrUndefined (ReshardingConfigurationList)
  , "NodeGroupsToRemove" :: NullOrUndefined.NullOrUndefined (NodeGroupsToRemoveList)
  }
derive instance newtypeModifyReplicationGroupShardConfigurationMessage :: Newtype ModifyReplicationGroupShardConfigurationMessage _
derive instance repGenericModifyReplicationGroupShardConfigurationMessage :: Generic ModifyReplicationGroupShardConfigurationMessage _
instance showModifyReplicationGroupShardConfigurationMessage :: Show ModifyReplicationGroupShardConfigurationMessage where
  show = genericShow
instance decodeModifyReplicationGroupShardConfigurationMessage :: Decode ModifyReplicationGroupShardConfigurationMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyReplicationGroupShardConfigurationMessage :: Encode ModifyReplicationGroupShardConfigurationMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyReplicationGroupShardConfigurationResult = ModifyReplicationGroupShardConfigurationResult 
  { "ReplicationGroup" :: NullOrUndefined.NullOrUndefined (ReplicationGroup)
  }
derive instance newtypeModifyReplicationGroupShardConfigurationResult :: Newtype ModifyReplicationGroupShardConfigurationResult _
derive instance repGenericModifyReplicationGroupShardConfigurationResult :: Generic ModifyReplicationGroupShardConfigurationResult _
instance showModifyReplicationGroupShardConfigurationResult :: Show ModifyReplicationGroupShardConfigurationResult where
  show = genericShow
instance decodeModifyReplicationGroupShardConfigurationResult :: Decode ModifyReplicationGroupShardConfigurationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyReplicationGroupShardConfigurationResult :: Encode ModifyReplicationGroupShardConfigurationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection of cache nodes in a replication group. One node in the node group is the read/write primary node. All the other nodes are read-only Replica nodes.</p>
newtype NodeGroup = NodeGroup 
  { "NodeGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "PrimaryEndpoint" :: NullOrUndefined.NullOrUndefined (Endpoint)
  , "Slots" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeGroupMembers" :: NullOrUndefined.NullOrUndefined (NodeGroupMemberList)
  }
derive instance newtypeNodeGroup :: Newtype NodeGroup _
derive instance repGenericNodeGroup :: Generic NodeGroup _
instance showNodeGroup :: Show NodeGroup where
  show = genericShow
instance decodeNodeGroup :: Decode NodeGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeGroup :: Encode NodeGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Node group (shard) configuration options. Each node group (shard) configuration has the following: <code>Slots</code>, <code>PrimaryAvailabilityZone</code>, <code>ReplicaAvailabilityZones</code>, <code>ReplicaCount</code>.</p>
newtype NodeGroupConfiguration = NodeGroupConfiguration 
  { "Slots" :: NullOrUndefined.NullOrUndefined (String)
  , "ReplicaCount" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "PrimaryAvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "ReplicaAvailabilityZones" :: NullOrUndefined.NullOrUndefined (AvailabilityZonesList)
  }
derive instance newtypeNodeGroupConfiguration :: Newtype NodeGroupConfiguration _
derive instance repGenericNodeGroupConfiguration :: Generic NodeGroupConfiguration _
instance showNodeGroupConfiguration :: Show NodeGroupConfiguration where
  show = genericShow
instance decodeNodeGroupConfiguration :: Decode NodeGroupConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeGroupConfiguration :: Encode NodeGroupConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NodeGroupConfigurationList = NodeGroupConfigurationList (Array NodeGroupConfiguration)
derive instance newtypeNodeGroupConfigurationList :: Newtype NodeGroupConfigurationList _
derive instance repGenericNodeGroupConfigurationList :: Generic NodeGroupConfigurationList _
instance showNodeGroupConfigurationList :: Show NodeGroupConfigurationList where
  show = genericShow
instance decodeNodeGroupConfigurationList :: Decode NodeGroupConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeGroupConfigurationList :: Encode NodeGroupConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NodeGroupList = NodeGroupList (Array NodeGroup)
derive instance newtypeNodeGroupList :: Newtype NodeGroupList _
derive instance repGenericNodeGroupList :: Generic NodeGroupList _
instance showNodeGroupList :: Show NodeGroupList where
  show = genericShow
instance decodeNodeGroupList :: Decode NodeGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeGroupList :: Encode NodeGroupList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a single node within a node group (shard).</p>
newtype NodeGroupMember = NodeGroupMember 
  { "CacheClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeId" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadEndpoint" :: NullOrUndefined.NullOrUndefined (Endpoint)
  , "PreferredAvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "CurrentRole" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNodeGroupMember :: Newtype NodeGroupMember _
derive instance repGenericNodeGroupMember :: Generic NodeGroupMember _
instance showNodeGroupMember :: Show NodeGroupMember where
  show = genericShow
instance decodeNodeGroupMember :: Decode NodeGroupMember where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeGroupMember :: Encode NodeGroupMember where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NodeGroupMemberList = NodeGroupMemberList (Array NodeGroupMember)
derive instance newtypeNodeGroupMemberList :: Newtype NodeGroupMemberList _
derive instance repGenericNodeGroupMemberList :: Generic NodeGroupMemberList _
instance showNodeGroupMemberList :: Show NodeGroupMemberList where
  show = genericShow
instance decodeNodeGroupMemberList :: Decode NodeGroupMemberList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeGroupMemberList :: Encode NodeGroupMemberList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The node group specified by the <code>NodeGroupId</code> parameter could not be found. Please verify that the node group exists and that you spelled the <code>NodeGroupId</code> value correctly.</p>
newtype NodeGroupNotFoundFault = NodeGroupNotFoundFault Types.NoArguments
derive instance newtypeNodeGroupNotFoundFault :: Newtype NodeGroupNotFoundFault _
derive instance repGenericNodeGroupNotFoundFault :: Generic NodeGroupNotFoundFault _
instance showNodeGroupNotFoundFault :: Show NodeGroupNotFoundFault where
  show = genericShow
instance decodeNodeGroupNotFoundFault :: Decode NodeGroupNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeGroupNotFoundFault :: Encode NodeGroupNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request cannot be processed because it would exceed the maximum allowed number of node groups (shards) in a single replication group. The default maximum is 15</p>
newtype NodeGroupsPerReplicationGroupQuotaExceededFault = NodeGroupsPerReplicationGroupQuotaExceededFault Types.NoArguments
derive instance newtypeNodeGroupsPerReplicationGroupQuotaExceededFault :: Newtype NodeGroupsPerReplicationGroupQuotaExceededFault _
derive instance repGenericNodeGroupsPerReplicationGroupQuotaExceededFault :: Generic NodeGroupsPerReplicationGroupQuotaExceededFault _
instance showNodeGroupsPerReplicationGroupQuotaExceededFault :: Show NodeGroupsPerReplicationGroupQuotaExceededFault where
  show = genericShow
instance decodeNodeGroupsPerReplicationGroupQuotaExceededFault :: Decode NodeGroupsPerReplicationGroupQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeGroupsPerReplicationGroupQuotaExceededFault :: Encode NodeGroupsPerReplicationGroupQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NodeGroupsToRemoveList = NodeGroupsToRemoveList (Array String)
derive instance newtypeNodeGroupsToRemoveList :: Newtype NodeGroupsToRemoveList _
derive instance repGenericNodeGroupsToRemoveList :: Generic NodeGroupsToRemoveList _
instance showNodeGroupsToRemoveList :: Show NodeGroupsToRemoveList where
  show = genericShow
instance decodeNodeGroupsToRemoveList :: Decode NodeGroupsToRemoveList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeGroupsToRemoveList :: Encode NodeGroupsToRemoveList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request cannot be processed because it would exceed the allowed number of cache nodes in a single cluster.</p>
newtype NodeQuotaForClusterExceededFault = NodeQuotaForClusterExceededFault Types.NoArguments
derive instance newtypeNodeQuotaForClusterExceededFault :: Newtype NodeQuotaForClusterExceededFault _
derive instance repGenericNodeQuotaForClusterExceededFault :: Generic NodeQuotaForClusterExceededFault _
instance showNodeQuotaForClusterExceededFault :: Show NodeQuotaForClusterExceededFault where
  show = genericShow
instance decodeNodeQuotaForClusterExceededFault :: Decode NodeQuotaForClusterExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeQuotaForClusterExceededFault :: Encode NodeQuotaForClusterExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request cannot be processed because it would exceed the allowed number of cache nodes per customer.</p>
newtype NodeQuotaForCustomerExceededFault = NodeQuotaForCustomerExceededFault Types.NoArguments
derive instance newtypeNodeQuotaForCustomerExceededFault :: Newtype NodeQuotaForCustomerExceededFault _
derive instance repGenericNodeQuotaForCustomerExceededFault :: Generic NodeQuotaForCustomerExceededFault _
instance showNodeQuotaForCustomerExceededFault :: Show NodeQuotaForCustomerExceededFault where
  show = genericShow
instance decodeNodeQuotaForCustomerExceededFault :: Decode NodeQuotaForCustomerExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeQuotaForCustomerExceededFault :: Encode NodeQuotaForCustomerExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an individual cache node in a snapshot of a cluster.</p>
newtype NodeSnapshot = NodeSnapshot 
  { "CacheClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeId" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeGroupConfiguration" :: NullOrUndefined.NullOrUndefined (NodeGroupConfiguration)
  , "CacheSize" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeCreateTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "SnapshotCreateTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  }
derive instance newtypeNodeSnapshot :: Newtype NodeSnapshot _
derive instance repGenericNodeSnapshot :: Generic NodeSnapshot _
instance showNodeSnapshot :: Show NodeSnapshot where
  show = genericShow
instance decodeNodeSnapshot :: Decode NodeSnapshot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeSnapshot :: Encode NodeSnapshot where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NodeSnapshotList = NodeSnapshotList (Array NodeSnapshot)
derive instance newtypeNodeSnapshotList :: Newtype NodeSnapshotList _
derive instance repGenericNodeSnapshotList :: Generic NodeSnapshotList _
instance showNodeSnapshotList :: Show NodeSnapshotList where
  show = genericShow
instance decodeNodeSnapshotList :: Decode NodeSnapshotList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeSnapshotList :: Encode NodeSnapshotList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NodeTypeList = NodeTypeList (Array String)
derive instance newtypeNodeTypeList :: Newtype NodeTypeList _
derive instance repGenericNodeTypeList :: Generic NodeTypeList _
instance showNodeTypeList :: Show NodeTypeList where
  show = genericShow
instance decodeNodeTypeList :: Decode NodeTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeTypeList :: Encode NodeTypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS).</p>
newtype NotificationConfiguration = NotificationConfiguration 
  { "TopicArn" :: NullOrUndefined.NullOrUndefined (String)
  , "TopicStatus" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNotificationConfiguration :: Newtype NotificationConfiguration _
derive instance repGenericNotificationConfiguration :: Generic NotificationConfiguration _
instance showNotificationConfiguration :: Show NotificationConfiguration where
  show = genericShow
instance decodeNotificationConfiguration :: Decode NotificationConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationConfiguration :: Encode NotificationConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an individual setting that controls some aspect of ElastiCache behavior.</p>
newtype Parameter = Parameter 
  { "ParameterName" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterValue" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Source" :: NullOrUndefined.NullOrUndefined (String)
  , "DataType" :: NullOrUndefined.NullOrUndefined (String)
  , "AllowedValues" :: NullOrUndefined.NullOrUndefined (String)
  , "IsModifiable" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "MinimumEngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "ChangeType" :: NullOrUndefined.NullOrUndefined (ChangeType)
  }
derive instance newtypeParameter :: Newtype Parameter _
derive instance repGenericParameter :: Generic Parameter _
instance showParameter :: Show Parameter where
  show = genericShow
instance decodeParameter :: Decode Parameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameter :: Encode Parameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a name-value pair that is used to update the value of a parameter.</p>
newtype ParameterNameValue = ParameterNameValue 
  { "ParameterName" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterValue" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeParameterNameValue :: Newtype ParameterNameValue _
derive instance repGenericParameterNameValue :: Generic ParameterNameValue _
instance showParameterNameValue :: Show ParameterNameValue where
  show = genericShow
instance decodeParameterNameValue :: Decode ParameterNameValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterNameValue :: Encode ParameterNameValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterNameValueList = ParameterNameValueList (Array ParameterNameValue)
derive instance newtypeParameterNameValueList :: Newtype ParameterNameValueList _
derive instance repGenericParameterNameValueList :: Generic ParameterNameValueList _
instance showParameterNameValueList :: Show ParameterNameValueList where
  show = genericShow
instance decodeParameterNameValueList :: Decode ParameterNameValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterNameValueList :: Encode ParameterNameValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParametersList = ParametersList (Array Parameter)
derive instance newtypeParametersList :: Newtype ParametersList _
derive instance repGenericParametersList :: Generic ParametersList _
instance showParametersList :: Show ParametersList where
  show = genericShow
instance decodeParametersList :: Decode ParametersList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParametersList :: Encode ParametersList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PendingAutomaticFailoverStatus = PendingAutomaticFailoverStatus String
derive instance newtypePendingAutomaticFailoverStatus :: Newtype PendingAutomaticFailoverStatus _
derive instance repGenericPendingAutomaticFailoverStatus :: Generic PendingAutomaticFailoverStatus _
instance showPendingAutomaticFailoverStatus :: Show PendingAutomaticFailoverStatus where
  show = genericShow
instance decodePendingAutomaticFailoverStatus :: Decode PendingAutomaticFailoverStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePendingAutomaticFailoverStatus :: Encode PendingAutomaticFailoverStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A group of settings that are applied to the cluster in the future, or that are currently being applied.</p>
newtype PendingModifiedValues = PendingModifiedValues 
  { "NumCacheNodes" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "CacheNodeIdsToRemove" :: NullOrUndefined.NullOrUndefined (CacheNodeIdsList)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePendingModifiedValues :: Newtype PendingModifiedValues _
derive instance repGenericPendingModifiedValues :: Generic PendingModifiedValues _
instance showPendingModifiedValues :: Show PendingModifiedValues where
  show = genericShow
instance decodePendingModifiedValues :: Decode PendingModifiedValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePendingModifiedValues :: Encode PendingModifiedValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PreferredAvailabilityZoneList = PreferredAvailabilityZoneList (Array String)
derive instance newtypePreferredAvailabilityZoneList :: Newtype PreferredAvailabilityZoneList _
derive instance repGenericPreferredAvailabilityZoneList :: Generic PreferredAvailabilityZoneList _
instance showPreferredAvailabilityZoneList :: Show PreferredAvailabilityZoneList where
  show = genericShow
instance decodePreferredAvailabilityZoneList :: Decode PreferredAvailabilityZoneList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePreferredAvailabilityZoneList :: Encode PreferredAvailabilityZoneList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>PurchaseReservedCacheNodesOffering</code> operation.</p>
newtype PurchaseReservedCacheNodesOfferingMessage = PurchaseReservedCacheNodesOfferingMessage 
  { "ReservedCacheNodesOfferingId" :: (String)
  , "ReservedCacheNodeId" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeCount" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  }
derive instance newtypePurchaseReservedCacheNodesOfferingMessage :: Newtype PurchaseReservedCacheNodesOfferingMessage _
derive instance repGenericPurchaseReservedCacheNodesOfferingMessage :: Generic PurchaseReservedCacheNodesOfferingMessage _
instance showPurchaseReservedCacheNodesOfferingMessage :: Show PurchaseReservedCacheNodesOfferingMessage where
  show = genericShow
instance decodePurchaseReservedCacheNodesOfferingMessage :: Decode PurchaseReservedCacheNodesOfferingMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurchaseReservedCacheNodesOfferingMessage :: Encode PurchaseReservedCacheNodesOfferingMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PurchaseReservedCacheNodesOfferingResult = PurchaseReservedCacheNodesOfferingResult 
  { "ReservedCacheNode" :: NullOrUndefined.NullOrUndefined (ReservedCacheNode)
  }
derive instance newtypePurchaseReservedCacheNodesOfferingResult :: Newtype PurchaseReservedCacheNodesOfferingResult _
derive instance repGenericPurchaseReservedCacheNodesOfferingResult :: Generic PurchaseReservedCacheNodesOfferingResult _
instance showPurchaseReservedCacheNodesOfferingResult :: Show PurchaseReservedCacheNodesOfferingResult where
  show = genericShow
instance decodePurchaseReservedCacheNodesOfferingResult :: Decode PurchaseReservedCacheNodesOfferingResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurchaseReservedCacheNodesOfferingResult :: Encode PurchaseReservedCacheNodesOfferingResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>RebootCacheCluster</code> operation.</p>
newtype RebootCacheClusterMessage = RebootCacheClusterMessage 
  { "CacheClusterId" :: (String)
  , "CacheNodeIdsToReboot" :: (CacheNodeIdsList)
  }
derive instance newtypeRebootCacheClusterMessage :: Newtype RebootCacheClusterMessage _
derive instance repGenericRebootCacheClusterMessage :: Generic RebootCacheClusterMessage _
instance showRebootCacheClusterMessage :: Show RebootCacheClusterMessage where
  show = genericShow
instance decodeRebootCacheClusterMessage :: Decode RebootCacheClusterMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebootCacheClusterMessage :: Encode RebootCacheClusterMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RebootCacheClusterResult = RebootCacheClusterResult 
  { "CacheCluster" :: NullOrUndefined.NullOrUndefined (CacheCluster)
  }
derive instance newtypeRebootCacheClusterResult :: Newtype RebootCacheClusterResult _
derive instance repGenericRebootCacheClusterResult :: Generic RebootCacheClusterResult _
instance showRebootCacheClusterResult :: Show RebootCacheClusterResult where
  show = genericShow
instance decodeRebootCacheClusterResult :: Decode RebootCacheClusterResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebootCacheClusterResult :: Encode RebootCacheClusterResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the specific price and frequency of a recurring charges for a reserved cache node, or for a reserved cache node offering.</p>
newtype RecurringCharge = RecurringCharge 
  { "RecurringChargeAmount" :: NullOrUndefined.NullOrUndefined (Number)
  , "RecurringChargeFrequency" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRecurringCharge :: Newtype RecurringCharge _
derive instance repGenericRecurringCharge :: Generic RecurringCharge _
instance showRecurringCharge :: Show RecurringCharge where
  show = genericShow
instance decodeRecurringCharge :: Decode RecurringCharge where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecurringCharge :: Encode RecurringCharge where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecurringChargeList = RecurringChargeList (Array RecurringCharge)
derive instance newtypeRecurringChargeList :: Newtype RecurringChargeList _
derive instance repGenericRecurringChargeList :: Generic RecurringChargeList _
instance showRecurringChargeList :: Show RecurringChargeList where
  show = genericShow
instance decodeRecurringChargeList :: Decode RecurringChargeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecurringChargeList :: Encode RecurringChargeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>RemoveTagsFromResource</code> operation.</p>
newtype RemoveTagsFromResourceMessage = RemoveTagsFromResourceMessage 
  { "ResourceName" :: (String)
  , "TagKeys" :: (KeyList)
  }
derive instance newtypeRemoveTagsFromResourceMessage :: Newtype RemoveTagsFromResourceMessage _
derive instance repGenericRemoveTagsFromResourceMessage :: Generic RemoveTagsFromResourceMessage _
instance showRemoveTagsFromResourceMessage :: Show RemoveTagsFromResourceMessage where
  show = genericShow
instance decodeRemoveTagsFromResourceMessage :: Decode RemoveTagsFromResourceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsFromResourceMessage :: Encode RemoveTagsFromResourceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains all of the attributes of a specific Redis replication group.</p>
newtype ReplicationGroup = ReplicationGroup 
  { "ReplicationGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "PendingModifiedValues" :: NullOrUndefined.NullOrUndefined (ReplicationGroupPendingModifiedValues)
  , "MemberClusters" :: NullOrUndefined.NullOrUndefined (ClusterIdList)
  , "NodeGroups" :: NullOrUndefined.NullOrUndefined (NodeGroupList)
  , "SnapshottingClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "AutomaticFailover" :: NullOrUndefined.NullOrUndefined (AutomaticFailoverStatus)
  , "ConfigurationEndpoint" :: NullOrUndefined.NullOrUndefined (Endpoint)
  , "SnapshotRetentionLimit" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterEnabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthTokenEnabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "TransitEncryptionEnabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "AtRestEncryptionEnabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeReplicationGroup :: Newtype ReplicationGroup _
derive instance repGenericReplicationGroup :: Generic ReplicationGroup _
instance showReplicationGroup :: Show ReplicationGroup where
  show = genericShow
instance decodeReplicationGroup :: Decode ReplicationGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationGroup :: Encode ReplicationGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified replication group already exists.</p>
newtype ReplicationGroupAlreadyExistsFault = ReplicationGroupAlreadyExistsFault Types.NoArguments
derive instance newtypeReplicationGroupAlreadyExistsFault :: Newtype ReplicationGroupAlreadyExistsFault _
derive instance repGenericReplicationGroupAlreadyExistsFault :: Generic ReplicationGroupAlreadyExistsFault _
instance showReplicationGroupAlreadyExistsFault :: Show ReplicationGroupAlreadyExistsFault where
  show = genericShow
instance decodeReplicationGroupAlreadyExistsFault :: Decode ReplicationGroupAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationGroupAlreadyExistsFault :: Encode ReplicationGroupAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReplicationGroupList = ReplicationGroupList (Array ReplicationGroup)
derive instance newtypeReplicationGroupList :: Newtype ReplicationGroupList _
derive instance repGenericReplicationGroupList :: Generic ReplicationGroupList _
instance showReplicationGroupList :: Show ReplicationGroupList where
  show = genericShow
instance decodeReplicationGroupList :: Decode ReplicationGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationGroupList :: Encode ReplicationGroupList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeReplicationGroups</code> operation.</p>
newtype ReplicationGroupMessage = ReplicationGroupMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "ReplicationGroups" :: NullOrUndefined.NullOrUndefined (ReplicationGroupList)
  }
derive instance newtypeReplicationGroupMessage :: Newtype ReplicationGroupMessage _
derive instance repGenericReplicationGroupMessage :: Generic ReplicationGroupMessage _
instance showReplicationGroupMessage :: Show ReplicationGroupMessage where
  show = genericShow
instance decodeReplicationGroupMessage :: Decode ReplicationGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationGroupMessage :: Encode ReplicationGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified replication group does not exist.</p>
newtype ReplicationGroupNotFoundFault = ReplicationGroupNotFoundFault Types.NoArguments
derive instance newtypeReplicationGroupNotFoundFault :: Newtype ReplicationGroupNotFoundFault _
derive instance repGenericReplicationGroupNotFoundFault :: Generic ReplicationGroupNotFoundFault _
instance showReplicationGroupNotFoundFault :: Show ReplicationGroupNotFoundFault where
  show = genericShow
instance decodeReplicationGroupNotFoundFault :: Decode ReplicationGroupNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationGroupNotFoundFault :: Encode ReplicationGroupNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The settings to be applied to the Redis replication group, either immediately or during the next maintenance window.</p>
newtype ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues 
  { "PrimaryClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "AutomaticFailoverStatus" :: NullOrUndefined.NullOrUndefined (PendingAutomaticFailoverStatus)
  , "Resharding" :: NullOrUndefined.NullOrUndefined (ReshardingStatus)
  }
derive instance newtypeReplicationGroupPendingModifiedValues :: Newtype ReplicationGroupPendingModifiedValues _
derive instance repGenericReplicationGroupPendingModifiedValues :: Generic ReplicationGroupPendingModifiedValues _
instance showReplicationGroupPendingModifiedValues :: Show ReplicationGroupPendingModifiedValues where
  show = genericShow
instance decodeReplicationGroupPendingModifiedValues :: Decode ReplicationGroupPendingModifiedValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationGroupPendingModifiedValues :: Encode ReplicationGroupPendingModifiedValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>PurchaseReservedCacheNodesOffering</code> operation.</p>
newtype ReservedCacheNode = ReservedCacheNode 
  { "ReservedCacheNodeId" :: NullOrUndefined.NullOrUndefined (String)
  , "ReservedCacheNodesOfferingId" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "Duration" :: NullOrUndefined.NullOrUndefined (Int)
  , "FixedPrice" :: NullOrUndefined.NullOrUndefined (Number)
  , "UsagePrice" :: NullOrUndefined.NullOrUndefined (Number)
  , "CacheNodeCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "ProductDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (String)
  , "RecurringCharges" :: NullOrUndefined.NullOrUndefined (RecurringChargeList)
  }
derive instance newtypeReservedCacheNode :: Newtype ReservedCacheNode _
derive instance repGenericReservedCacheNode :: Generic ReservedCacheNode _
instance showReservedCacheNode :: Show ReservedCacheNode where
  show = genericShow
instance decodeReservedCacheNode :: Decode ReservedCacheNode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedCacheNode :: Encode ReservedCacheNode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You already have a reservation with the given identifier.</p>
newtype ReservedCacheNodeAlreadyExistsFault = ReservedCacheNodeAlreadyExistsFault Types.NoArguments
derive instance newtypeReservedCacheNodeAlreadyExistsFault :: Newtype ReservedCacheNodeAlreadyExistsFault _
derive instance repGenericReservedCacheNodeAlreadyExistsFault :: Generic ReservedCacheNodeAlreadyExistsFault _
instance showReservedCacheNodeAlreadyExistsFault :: Show ReservedCacheNodeAlreadyExistsFault where
  show = genericShow
instance decodeReservedCacheNodeAlreadyExistsFault :: Decode ReservedCacheNodeAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedCacheNodeAlreadyExistsFault :: Encode ReservedCacheNodeAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservedCacheNodeList = ReservedCacheNodeList (Array ReservedCacheNode)
derive instance newtypeReservedCacheNodeList :: Newtype ReservedCacheNodeList _
derive instance repGenericReservedCacheNodeList :: Generic ReservedCacheNodeList _
instance showReservedCacheNodeList :: Show ReservedCacheNodeList where
  show = genericShow
instance decodeReservedCacheNodeList :: Decode ReservedCacheNodeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedCacheNodeList :: Encode ReservedCacheNodeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeReservedCacheNodes</code> operation.</p>
newtype ReservedCacheNodeMessage = ReservedCacheNodeMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "ReservedCacheNodes" :: NullOrUndefined.NullOrUndefined (ReservedCacheNodeList)
  }
derive instance newtypeReservedCacheNodeMessage :: Newtype ReservedCacheNodeMessage _
derive instance repGenericReservedCacheNodeMessage :: Generic ReservedCacheNodeMessage _
instance showReservedCacheNodeMessage :: Show ReservedCacheNodeMessage where
  show = genericShow
instance decodeReservedCacheNodeMessage :: Decode ReservedCacheNodeMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedCacheNodeMessage :: Encode ReservedCacheNodeMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested reserved cache node was not found.</p>
newtype ReservedCacheNodeNotFoundFault = ReservedCacheNodeNotFoundFault Types.NoArguments
derive instance newtypeReservedCacheNodeNotFoundFault :: Newtype ReservedCacheNodeNotFoundFault _
derive instance repGenericReservedCacheNodeNotFoundFault :: Generic ReservedCacheNodeNotFoundFault _
instance showReservedCacheNodeNotFoundFault :: Show ReservedCacheNodeNotFoundFault where
  show = genericShow
instance decodeReservedCacheNodeNotFoundFault :: Decode ReservedCacheNodeNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedCacheNodeNotFoundFault :: Encode ReservedCacheNodeNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request cannot be processed because it would exceed the user's cache node quota.</p>
newtype ReservedCacheNodeQuotaExceededFault = ReservedCacheNodeQuotaExceededFault Types.NoArguments
derive instance newtypeReservedCacheNodeQuotaExceededFault :: Newtype ReservedCacheNodeQuotaExceededFault _
derive instance repGenericReservedCacheNodeQuotaExceededFault :: Generic ReservedCacheNodeQuotaExceededFault _
instance showReservedCacheNodeQuotaExceededFault :: Show ReservedCacheNodeQuotaExceededFault where
  show = genericShow
instance decodeReservedCacheNodeQuotaExceededFault :: Decode ReservedCacheNodeQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedCacheNodeQuotaExceededFault :: Encode ReservedCacheNodeQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes all of the attributes of a reserved cache node offering.</p>
newtype ReservedCacheNodesOffering = ReservedCacheNodesOffering 
  { "ReservedCacheNodesOfferingId" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "Duration" :: NullOrUndefined.NullOrUndefined (Int)
  , "FixedPrice" :: NullOrUndefined.NullOrUndefined (Number)
  , "UsagePrice" :: NullOrUndefined.NullOrUndefined (Number)
  , "ProductDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined.NullOrUndefined (String)
  , "RecurringCharges" :: NullOrUndefined.NullOrUndefined (RecurringChargeList)
  }
derive instance newtypeReservedCacheNodesOffering :: Newtype ReservedCacheNodesOffering _
derive instance repGenericReservedCacheNodesOffering :: Generic ReservedCacheNodesOffering _
instance showReservedCacheNodesOffering :: Show ReservedCacheNodesOffering where
  show = genericShow
instance decodeReservedCacheNodesOffering :: Decode ReservedCacheNodesOffering where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedCacheNodesOffering :: Encode ReservedCacheNodesOffering where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservedCacheNodesOfferingList = ReservedCacheNodesOfferingList (Array ReservedCacheNodesOffering)
derive instance newtypeReservedCacheNodesOfferingList :: Newtype ReservedCacheNodesOfferingList _
derive instance repGenericReservedCacheNodesOfferingList :: Generic ReservedCacheNodesOfferingList _
instance showReservedCacheNodesOfferingList :: Show ReservedCacheNodesOfferingList where
  show = genericShow
instance decodeReservedCacheNodesOfferingList :: Decode ReservedCacheNodesOfferingList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedCacheNodesOfferingList :: Encode ReservedCacheNodesOfferingList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeReservedCacheNodesOfferings</code> operation.</p>
newtype ReservedCacheNodesOfferingMessage = ReservedCacheNodesOfferingMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "ReservedCacheNodesOfferings" :: NullOrUndefined.NullOrUndefined (ReservedCacheNodesOfferingList)
  }
derive instance newtypeReservedCacheNodesOfferingMessage :: Newtype ReservedCacheNodesOfferingMessage _
derive instance repGenericReservedCacheNodesOfferingMessage :: Generic ReservedCacheNodesOfferingMessage _
instance showReservedCacheNodesOfferingMessage :: Show ReservedCacheNodesOfferingMessage where
  show = genericShow
instance decodeReservedCacheNodesOfferingMessage :: Decode ReservedCacheNodesOfferingMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedCacheNodesOfferingMessage :: Encode ReservedCacheNodesOfferingMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested cache node offering does not exist.</p>
newtype ReservedCacheNodesOfferingNotFoundFault = ReservedCacheNodesOfferingNotFoundFault Types.NoArguments
derive instance newtypeReservedCacheNodesOfferingNotFoundFault :: Newtype ReservedCacheNodesOfferingNotFoundFault _
derive instance repGenericReservedCacheNodesOfferingNotFoundFault :: Generic ReservedCacheNodesOfferingNotFoundFault _
instance showReservedCacheNodesOfferingNotFoundFault :: Show ReservedCacheNodesOfferingNotFoundFault where
  show = genericShow
instance decodeReservedCacheNodesOfferingNotFoundFault :: Decode ReservedCacheNodesOfferingNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedCacheNodesOfferingNotFoundFault :: Encode ReservedCacheNodesOfferingNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>ResetCacheParameterGroup</code> operation.</p>
newtype ResetCacheParameterGroupMessage = ResetCacheParameterGroupMessage 
  { "CacheParameterGroupName" :: (String)
  , "ResetAllParameters" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ParameterNameValues" :: NullOrUndefined.NullOrUndefined (ParameterNameValueList)
  }
derive instance newtypeResetCacheParameterGroupMessage :: Newtype ResetCacheParameterGroupMessage _
derive instance repGenericResetCacheParameterGroupMessage :: Generic ResetCacheParameterGroupMessage _
instance showResetCacheParameterGroupMessage :: Show ResetCacheParameterGroupMessage where
  show = genericShow
instance decodeResetCacheParameterGroupMessage :: Decode ResetCacheParameterGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResetCacheParameterGroupMessage :: Encode ResetCacheParameterGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of <code>PreferredAvailabilityZones</code> objects that specifies the configuration of a node group in the resharded cluster.</p>
newtype ReshardingConfiguration = ReshardingConfiguration 
  { "PreferredAvailabilityZones" :: NullOrUndefined.NullOrUndefined (AvailabilityZonesList)
  }
derive instance newtypeReshardingConfiguration :: Newtype ReshardingConfiguration _
derive instance repGenericReshardingConfiguration :: Generic ReshardingConfiguration _
instance showReshardingConfiguration :: Show ReshardingConfiguration where
  show = genericShow
instance decodeReshardingConfiguration :: Decode ReshardingConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReshardingConfiguration :: Encode ReshardingConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReshardingConfigurationList = ReshardingConfigurationList (Array ReshardingConfiguration)
derive instance newtypeReshardingConfigurationList :: Newtype ReshardingConfigurationList _
derive instance repGenericReshardingConfigurationList :: Generic ReshardingConfigurationList _
instance showReshardingConfigurationList :: Show ReshardingConfigurationList where
  show = genericShow
instance decodeReshardingConfigurationList :: Decode ReshardingConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReshardingConfigurationList :: Encode ReshardingConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The status of an online resharding operation.</p>
newtype ReshardingStatus = ReshardingStatus 
  { "SlotMigration" :: NullOrUndefined.NullOrUndefined (SlotMigration)
  }
derive instance newtypeReshardingStatus :: Newtype ReshardingStatus _
derive instance repGenericReshardingStatus :: Generic ReshardingStatus _
instance showReshardingStatus :: Show ReshardingStatus where
  show = genericShow
instance decodeReshardingStatus :: Decode ReshardingStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReshardingStatus :: Encode ReshardingStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>RevokeCacheSecurityGroupIngress</code> operation.</p>
newtype RevokeCacheSecurityGroupIngressMessage = RevokeCacheSecurityGroupIngressMessage 
  { "CacheSecurityGroupName" :: (String)
  , "EC2SecurityGroupName" :: (String)
  , "EC2SecurityGroupOwnerId" :: (String)
  }
derive instance newtypeRevokeCacheSecurityGroupIngressMessage :: Newtype RevokeCacheSecurityGroupIngressMessage _
derive instance repGenericRevokeCacheSecurityGroupIngressMessage :: Generic RevokeCacheSecurityGroupIngressMessage _
instance showRevokeCacheSecurityGroupIngressMessage :: Show RevokeCacheSecurityGroupIngressMessage where
  show = genericShow
instance decodeRevokeCacheSecurityGroupIngressMessage :: Decode RevokeCacheSecurityGroupIngressMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevokeCacheSecurityGroupIngressMessage :: Encode RevokeCacheSecurityGroupIngressMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RevokeCacheSecurityGroupIngressResult = RevokeCacheSecurityGroupIngressResult 
  { "CacheSecurityGroup" :: NullOrUndefined.NullOrUndefined (CacheSecurityGroup)
  }
derive instance newtypeRevokeCacheSecurityGroupIngressResult :: Newtype RevokeCacheSecurityGroupIngressResult _
derive instance repGenericRevokeCacheSecurityGroupIngressResult :: Generic RevokeCacheSecurityGroupIngressResult _
instance showRevokeCacheSecurityGroupIngressResult :: Show RevokeCacheSecurityGroupIngressResult where
  show = genericShow
instance decodeRevokeCacheSecurityGroupIngressResult :: Decode RevokeCacheSecurityGroupIngressResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevokeCacheSecurityGroupIngressResult :: Encode RevokeCacheSecurityGroupIngressResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityGroupIdsList = SecurityGroupIdsList (Array String)
derive instance newtypeSecurityGroupIdsList :: Newtype SecurityGroupIdsList _
derive instance repGenericSecurityGroupIdsList :: Generic SecurityGroupIdsList _
instance showSecurityGroupIdsList :: Show SecurityGroupIdsList where
  show = genericShow
instance decodeSecurityGroupIdsList :: Decode SecurityGroupIdsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroupIdsList :: Encode SecurityGroupIdsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a single cache security group and its status.</p>
newtype SecurityGroupMembership = SecurityGroupMembership 
  { "SecurityGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSecurityGroupMembership :: Newtype SecurityGroupMembership _
derive instance repGenericSecurityGroupMembership :: Generic SecurityGroupMembership _
instance showSecurityGroupMembership :: Show SecurityGroupMembership where
  show = genericShow
instance decodeSecurityGroupMembership :: Decode SecurityGroupMembership where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroupMembership :: Encode SecurityGroupMembership where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityGroupMembershipList = SecurityGroupMembershipList (Array SecurityGroupMembership)
derive instance newtypeSecurityGroupMembershipList :: Newtype SecurityGroupMembershipList _
derive instance repGenericSecurityGroupMembershipList :: Generic SecurityGroupMembershipList _
instance showSecurityGroupMembershipList :: Show SecurityGroupMembershipList where
  show = genericShow
instance decodeSecurityGroupMembershipList :: Decode SecurityGroupMembershipList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroupMembershipList :: Encode SecurityGroupMembershipList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the progress of an online resharding operation.</p>
newtype SlotMigration = SlotMigration 
  { "ProgressPercentage" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeSlotMigration :: Newtype SlotMigration _
derive instance repGenericSlotMigration :: Generic SlotMigration _
instance showSlotMigration :: Show SlotMigration where
  show = genericShow
instance decodeSlotMigration :: Decode SlotMigration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSlotMigration :: Encode SlotMigration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a copy of an entire Redis cluster as of the time when the snapshot was taken.</p>
newtype Snapshot = Snapshot 
  { "SnapshotName" :: NullOrUndefined.NullOrUndefined (String)
  , "ReplicationGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "ReplicationGroupDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheClusterId" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotSource" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "NumCacheNodes" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "PreferredAvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheClusterCreateTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "TopicArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "CacheParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "SnapshotRetentionLimit" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "SnapshotWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "NumNodeGroups" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "AutomaticFailover" :: NullOrUndefined.NullOrUndefined (AutomaticFailoverStatus)
  , "NodeSnapshots" :: NullOrUndefined.NullOrUndefined (NodeSnapshotList)
  }
derive instance newtypeSnapshot :: Newtype Snapshot _
derive instance repGenericSnapshot :: Generic Snapshot _
instance showSnapshot :: Show Snapshot where
  show = genericShow
instance decodeSnapshot :: Decode Snapshot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshot :: Encode Snapshot where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You already have a snapshot with the given name.</p>
newtype SnapshotAlreadyExistsFault = SnapshotAlreadyExistsFault Types.NoArguments
derive instance newtypeSnapshotAlreadyExistsFault :: Newtype SnapshotAlreadyExistsFault _
derive instance repGenericSnapshotAlreadyExistsFault :: Generic SnapshotAlreadyExistsFault _
instance showSnapshotAlreadyExistsFault :: Show SnapshotAlreadyExistsFault where
  show = genericShow
instance decodeSnapshotAlreadyExistsFault :: Decode SnapshotAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotAlreadyExistsFault :: Encode SnapshotAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnapshotArnsList = SnapshotArnsList (Array String)
derive instance newtypeSnapshotArnsList :: Newtype SnapshotArnsList _
derive instance repGenericSnapshotArnsList :: Generic SnapshotArnsList _
instance showSnapshotArnsList :: Show SnapshotArnsList where
  show = genericShow
instance decodeSnapshotArnsList :: Decode SnapshotArnsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotArnsList :: Encode SnapshotArnsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You attempted one of the following operations:</p> <ul> <li> <p>Creating a snapshot of a Redis cluster running on a <code>cache.t1.micro</code> cache node.</p> </li> <li> <p>Creating a snapshot of a cluster that is running Memcached rather than Redis.</p> </li> </ul> <p>Neither of these are supported by ElastiCache.</p>
newtype SnapshotFeatureNotSupportedFault = SnapshotFeatureNotSupportedFault Types.NoArguments
derive instance newtypeSnapshotFeatureNotSupportedFault :: Newtype SnapshotFeatureNotSupportedFault _
derive instance repGenericSnapshotFeatureNotSupportedFault :: Generic SnapshotFeatureNotSupportedFault _
instance showSnapshotFeatureNotSupportedFault :: Show SnapshotFeatureNotSupportedFault where
  show = genericShow
instance decodeSnapshotFeatureNotSupportedFault :: Decode SnapshotFeatureNotSupportedFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotFeatureNotSupportedFault :: Encode SnapshotFeatureNotSupportedFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnapshotList = SnapshotList (Array Snapshot)
derive instance newtypeSnapshotList :: Newtype SnapshotList _
derive instance repGenericSnapshotList :: Generic SnapshotList _
instance showSnapshotList :: Show SnapshotList where
  show = genericShow
instance decodeSnapshotList :: Decode SnapshotList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotList :: Encode SnapshotList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested snapshot name does not refer to an existing snapshot.</p>
newtype SnapshotNotFoundFault = SnapshotNotFoundFault Types.NoArguments
derive instance newtypeSnapshotNotFoundFault :: Newtype SnapshotNotFoundFault _
derive instance repGenericSnapshotNotFoundFault :: Generic SnapshotNotFoundFault _
instance showSnapshotNotFoundFault :: Show SnapshotNotFoundFault where
  show = genericShow
instance decodeSnapshotNotFoundFault :: Decode SnapshotNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotNotFoundFault :: Encode SnapshotNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request cannot be processed because it would exceed the maximum number of snapshots.</p>
newtype SnapshotQuotaExceededFault = SnapshotQuotaExceededFault Types.NoArguments
derive instance newtypeSnapshotQuotaExceededFault :: Newtype SnapshotQuotaExceededFault _
derive instance repGenericSnapshotQuotaExceededFault :: Generic SnapshotQuotaExceededFault _
instance showSnapshotQuotaExceededFault :: Show SnapshotQuotaExceededFault where
  show = genericShow
instance decodeSnapshotQuotaExceededFault :: Decode SnapshotQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotQuotaExceededFault :: Encode SnapshotQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SourceType = SourceType String
derive instance newtypeSourceType :: Newtype SourceType _
derive instance repGenericSourceType :: Generic SourceType _
instance showSourceType :: Show SourceType where
  show = genericShow
instance decodeSourceType :: Decode SourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceType :: Encode SourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the subnet associated with a cluster. This parameter refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used with ElastiCache.</p>
newtype Subnet = Subnet 
  { "SubnetIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SubnetAvailabilityZone" :: NullOrUndefined.NullOrUndefined (AvailabilityZone)
  }
derive instance newtypeSubnet :: Newtype Subnet _
derive instance repGenericSubnet :: Generic Subnet _
instance showSubnet :: Show Subnet where
  show = genericShow
instance decodeSubnet :: Decode Subnet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnet :: Encode Subnet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetIdentifierList = SubnetIdentifierList (Array String)
derive instance newtypeSubnetIdentifierList :: Newtype SubnetIdentifierList _
derive instance repGenericSubnetIdentifierList :: Generic SubnetIdentifierList _
instance showSubnetIdentifierList :: Show SubnetIdentifierList where
  show = genericShow
instance decodeSubnetIdentifierList :: Decode SubnetIdentifierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetIdentifierList :: Encode SubnetIdentifierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested subnet is being used by another cache subnet group.</p>
newtype SubnetInUse = SubnetInUse Types.NoArguments
derive instance newtypeSubnetInUse :: Newtype SubnetInUse _
derive instance repGenericSubnetInUse :: Generic SubnetInUse _
instance showSubnetInUse :: Show SubnetInUse where
  show = genericShow
instance decodeSubnetInUse :: Decode SubnetInUse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetInUse :: Encode SubnetInUse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetList = SubnetList (Array Subnet)
derive instance newtypeSubnetList :: Newtype SubnetList _
derive instance repGenericSubnetList :: Generic SubnetList _
instance showSubnetList :: Show SubnetList where
  show = genericShow
instance decodeSubnetList :: Decode SubnetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetList :: Encode SubnetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TStamp = TStamp Number
derive instance newtypeTStamp :: Newtype TStamp _
derive instance repGenericTStamp :: Generic TStamp _
instance showTStamp :: Show TStamp where
  show = genericShow
instance decodeTStamp :: Decode TStamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTStamp :: Encode TStamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A cost allocation Tag that can be added to an ElastiCache cluster or replication group. Tags are composed of a Key/Value pair. A tag with a null Value is permitted.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined.NullOrUndefined (String)
  , "Value" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
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


-- | <p>Represents the output from the <code>AddTagsToResource</code>, <code>ListTagsForResource</code>, and <code>RemoveTagsFromResource</code> operations.</p>
newtype TagListMessage = TagListMessage 
  { "TagList" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeTagListMessage :: Newtype TagListMessage _
derive instance repGenericTagListMessage :: Generic TagListMessage _
instance showTagListMessage :: Show TagListMessage where
  show = genericShow
instance decodeTagListMessage :: Decode TagListMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagListMessage :: Encode TagListMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested tag was not found on this resource.</p>
newtype TagNotFoundFault = TagNotFoundFault Types.NoArguments
derive instance newtypeTagNotFoundFault :: Newtype TagNotFoundFault _
derive instance repGenericTagNotFoundFault :: Generic TagNotFoundFault _
instance showTagNotFoundFault :: Show TagNotFoundFault where
  show = genericShow
instance decodeTagNotFoundFault :: Decode TagNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagNotFoundFault :: Encode TagNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request cannot be processed because it would cause the resource to have more than the allowed number of tags. The maximum number of tags permitted on a resource is 50.</p>
newtype TagQuotaPerResourceExceeded = TagQuotaPerResourceExceeded Types.NoArguments
derive instance newtypeTagQuotaPerResourceExceeded :: Newtype TagQuotaPerResourceExceeded _
derive instance repGenericTagQuotaPerResourceExceeded :: Generic TagQuotaPerResourceExceeded _
instance showTagQuotaPerResourceExceeded :: Show TagQuotaPerResourceExceeded where
  show = genericShow
instance decodeTagQuotaPerResourceExceeded :: Decode TagQuotaPerResourceExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagQuotaPerResourceExceeded :: Encode TagQuotaPerResourceExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestFailoverMessage = TestFailoverMessage 
  { "ReplicationGroupId" :: (String)
  , "NodeGroupId" :: (String)
  }
derive instance newtypeTestFailoverMessage :: Newtype TestFailoverMessage _
derive instance repGenericTestFailoverMessage :: Generic TestFailoverMessage _
instance showTestFailoverMessage :: Show TestFailoverMessage where
  show = genericShow
instance decodeTestFailoverMessage :: Decode TestFailoverMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestFailoverMessage :: Encode TestFailoverMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestFailoverNotAvailableFault = TestFailoverNotAvailableFault Types.NoArguments
derive instance newtypeTestFailoverNotAvailableFault :: Newtype TestFailoverNotAvailableFault _
derive instance repGenericTestFailoverNotAvailableFault :: Generic TestFailoverNotAvailableFault _
instance showTestFailoverNotAvailableFault :: Show TestFailoverNotAvailableFault where
  show = genericShow
instance decodeTestFailoverNotAvailableFault :: Decode TestFailoverNotAvailableFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestFailoverNotAvailableFault :: Encode TestFailoverNotAvailableFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestFailoverResult = TestFailoverResult 
  { "ReplicationGroup" :: NullOrUndefined.NullOrUndefined (ReplicationGroup)
  }
derive instance newtypeTestFailoverResult :: Newtype TestFailoverResult _
derive instance repGenericTestFailoverResult :: Generic TestFailoverResult _
instance showTestFailoverResult :: Show TestFailoverResult where
  show = genericShow
instance decodeTestFailoverResult :: Decode TestFailoverResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestFailoverResult :: Encode TestFailoverResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
