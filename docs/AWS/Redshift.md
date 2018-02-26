## Module AWS.Redshift

<fullname>Amazon Redshift</fullname> <p> <b>Overview</b> </p> <p>This is an interface reference for Amazon Redshift. It contains documentation for one of the programming or command line interfaces you can use to manage Amazon Redshift clusters. Note that Amazon Redshift is asynchronous, which means that some interfaces may require techniques, such as polling or asynchronous callback handlers, to determine when a command has been applied. In this reference, the parameter descriptions indicate whether a change is applied immediately, on the next instance reboot, or during the next maintenance window. For a summary of the Amazon Redshift cluster management interfaces, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/using-aws-sdk.html">Using the Amazon Redshift Management Interfaces</a>.</p> <p>Amazon Redshift manages all the work of setting up, operating, and scaling a data warehouse: provisioning capacity, monitoring and backing up the cluster, and applying patches and upgrades to the Amazon Redshift engine. You can focus on using your data to acquire new insights for your business and customers.</p> <p>If you are a first-time user of Amazon Redshift, we recommend that you begin by reading the <a href="http://docs.aws.amazon.com/redshift/latest/gsg/getting-started.html">Amazon Redshift Getting Started Guide</a>.</p> <p>If you are a database developer, the <a href="http://docs.aws.amazon.com/redshift/latest/dg/welcome.html">Amazon Redshift Database Developer Guide</a> explains how to design, build, query, and maintain the databases that make up your data warehouse. </p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `authorizeClusterSecurityGroupIngress`

``` purescript
authorizeClusterSecurityGroupIngress :: forall eff. AuthorizeClusterSecurityGroupIngressMessage -> Aff (err :: RequestError | eff) AuthorizeClusterSecurityGroupIngressResult
```

<p>Adds an inbound (ingress) rule to an Amazon Redshift security group. Depending on whether the application accessing your cluster is running on the Internet or an Amazon EC2 instance, you can authorize inbound access to either a Classless Interdomain Routing (CIDR)/Internet Protocol (IP) range or to an Amazon EC2 security group. You can add as many as 20 ingress rules to an Amazon Redshift security group.</p> <p>If you authorize access to an Amazon EC2 security group, specify <i>EC2SecurityGroupName</i> and <i>EC2SecurityGroupOwnerId</i>. The Amazon EC2 security group and Amazon Redshift cluster must be in the same AWS region. </p> <p>If you authorize access to a CIDR/IP address range, specify <i>CIDRIP</i>. For an overview of CIDR blocks, see the Wikipedia article on <a href="http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing">Classless Inter-Domain Routing</a>. </p> <p>You must also associate the security group with a cluster so that clients running on these IP addresses or the EC2 instance are authorized to connect to the cluster. For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Working with Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `authorizeSnapshotAccess`

``` purescript
authorizeSnapshotAccess :: forall eff. AuthorizeSnapshotAccessMessage -> Aff (err :: RequestError | eff) AuthorizeSnapshotAccessResult
```

<p>Authorizes the specified AWS customer account to restore the specified snapshot.</p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `copyClusterSnapshot`

``` purescript
copyClusterSnapshot :: forall eff. CopyClusterSnapshotMessage -> Aff (err :: RequestError | eff) CopyClusterSnapshotResult
```

<p>Copies the specified automated cluster snapshot to a new manual cluster snapshot. The source must be an automated snapshot and it must be in the available state.</p> <p>When you delete a cluster, Amazon Redshift deletes any automated snapshots of the cluster. Also, when the retention period of the snapshot expires, Amazon Redshift automatically deletes it. If you want to keep an automated snapshot for a longer period, you can make a manual copy of the snapshot. Manual snapshots are retained until you delete them.</p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `createCluster`

``` purescript
createCluster :: forall eff. CreateClusterMessage -> Aff (err :: RequestError | eff) CreateClusterResult
```

<p>Creates a new cluster.</p> <p>To create the cluster in Virtual Private Cloud (VPC), you must provide a cluster subnet group name. The cluster subnet group identifies the subnets of your VPC that Amazon Redshift uses when creating the cluster. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `createClusterParameterGroup`

``` purescript
createClusterParameterGroup :: forall eff. CreateClusterParameterGroupMessage -> Aff (err :: RequestError | eff) CreateClusterParameterGroupResult
```

<p>Creates an Amazon Redshift parameter group.</p> <p>Creating parameter groups is independent of creating clusters. You can associate a cluster with a parameter group when you create the cluster. You can also associate an existing cluster with a parameter group after the cluster is created by using <a>ModifyCluster</a>. </p> <p>Parameters in the parameter group define specific behavior that applies to the databases you create on the cluster. For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `createClusterSecurityGroup`

``` purescript
createClusterSecurityGroup :: forall eff. CreateClusterSecurityGroupMessage -> Aff (err :: RequestError | eff) CreateClusterSecurityGroupResult
```

<p>Creates a new Amazon Redshift security group. You use security groups to control access to non-VPC clusters.</p> <p> For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Amazon Redshift Cluster Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `createClusterSnapshot`

``` purescript
createClusterSnapshot :: forall eff. CreateClusterSnapshotMessage -> Aff (err :: RequestError | eff) CreateClusterSnapshotResult
```

<p>Creates a manual snapshot of the specified cluster. The cluster must be in the <code>available</code> state. </p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `createClusterSubnetGroup`

``` purescript
createClusterSubnetGroup :: forall eff. CreateClusterSubnetGroupMessage -> Aff (err :: RequestError | eff) CreateClusterSubnetGroupResult
```

<p>Creates a new Amazon Redshift subnet group. You must provide a list of one or more subnets in your existing Amazon Virtual Private Cloud (Amazon VPC) when creating Amazon Redshift subnet group.</p> <p> For information about subnet groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-cluster-subnet-groups.html">Amazon Redshift Cluster Subnet Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `createEventSubscription`

``` purescript
createEventSubscription :: forall eff. CreateEventSubscriptionMessage -> Aff (err :: RequestError | eff) CreateEventSubscriptionResult
```

<p>Creates an Amazon Redshift event notification subscription. This action requires an ARN (Amazon Resource Name) of an Amazon SNS topic created by either the Amazon Redshift console, the Amazon SNS console, or the Amazon SNS API. To obtain an ARN with Amazon SNS, you must create a topic in Amazon SNS and subscribe to the topic. The ARN is displayed in the SNS console.</p> <p>You can specify the source type, and lists of Amazon Redshift source IDs, event categories, and event severities. Notifications will be sent for all events you want that match those criteria. For example, you can specify source type = cluster, source ID = my-cluster-1 and mycluster2, event categories = Availability, Backup, and severity = ERROR. The subscription will only send notifications for those ERROR events in the Availability and Backup categories for the specified clusters.</p> <p>If you specify both the source type and source IDs, such as source type = cluster and source identifier = my-cluster-1, notifications will be sent for all the cluster events for my-cluster-1. If you specify a source type but do not specify a source identifier, you will receive notice of the events for the objects of that type in your AWS account. If you do not specify either the SourceType nor the SourceIdentifier, you will be notified of events generated from all Amazon Redshift sources belonging to your AWS account. You must specify a source type if you specify a source ID.</p>

#### `createHsmClientCertificate`

``` purescript
createHsmClientCertificate :: forall eff. CreateHsmClientCertificateMessage -> Aff (err :: RequestError | eff) CreateHsmClientCertificateResult
```

<p>Creates an HSM client certificate that an Amazon Redshift cluster will use to connect to the client's HSM in order to store and retrieve the keys used to encrypt the cluster databases.</p> <p>The command returns a public key, which you must store in the HSM. In addition to creating the HSM certificate, you must create an Amazon Redshift HSM configuration that provides a cluster the information needed to store and use encryption keys in the HSM. For more information, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html">Hardware Security Modules</a> in the Amazon Redshift Cluster Management Guide.</p>

#### `createHsmConfiguration`

``` purescript
createHsmConfiguration :: forall eff. CreateHsmConfigurationMessage -> Aff (err :: RequestError | eff) CreateHsmConfigurationResult
```

<p>Creates an HSM configuration that contains the information required by an Amazon Redshift cluster to store and use database encryption keys in a Hardware Security Module (HSM). After creating the HSM configuration, you can specify it as a parameter when creating a cluster. The cluster will then store its encryption keys in the HSM.</p> <p>In addition to creating an HSM configuration, you must also create an HSM client certificate. For more information, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html">Hardware Security Modules</a> in the Amazon Redshift Cluster Management Guide.</p>

#### `createSnapshotCopyGrant`

``` purescript
createSnapshotCopyGrant :: forall eff. CreateSnapshotCopyGrantMessage -> Aff (err :: RequestError | eff) CreateSnapshotCopyGrantResult
```

<p>Creates a snapshot copy grant that permits Amazon Redshift to use a customer master key (CMK) from AWS Key Management Service (AWS KMS) to encrypt copied snapshots in a destination region.</p> <p> For more information about managing snapshot copy grants, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html">Amazon Redshift Database Encryption</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `createTags`

``` purescript
createTags :: forall eff. CreateTagsMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Adds one or more tags to a specified resource.</p> <p>A resource can have up to 10 tags. If you try to create more than 10 tags for a resource, you will receive an error and the attempt will fail.</p> <p>If you specify a key that already exists for the resource, the value for that key will be updated with the new value.</p>

#### `deleteCluster`

``` purescript
deleteCluster :: forall eff. DeleteClusterMessage -> Aff (err :: RequestError | eff) DeleteClusterResult
```

<p>Deletes a previously provisioned cluster. A successful response from the web service indicates that the request was received correctly. Use <a>DescribeClusters</a> to monitor the status of the deletion. The delete operation cannot be canceled or reverted once submitted. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>If you want to shut down the cluster and retain it for future use, set <i>SkipFinalClusterSnapshot</i> to <code>false</code> and specify a name for <i>FinalClusterSnapshotIdentifier</i>. You can later restore this snapshot to resume using the cluster. If a final cluster snapshot is requested, the status of the cluster will be "final-snapshot" while the snapshot is being taken, then it's "deleting" once Amazon Redshift begins deleting the cluster. </p> <p> For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `deleteClusterParameterGroup`

``` purescript
deleteClusterParameterGroup :: forall eff. DeleteClusterParameterGroupMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a specified Amazon Redshift parameter group.</p> <note> <p>You cannot delete a parameter group if it is associated with a cluster.</p> </note>

#### `deleteClusterSecurityGroup`

``` purescript
deleteClusterSecurityGroup :: forall eff. DeleteClusterSecurityGroupMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes an Amazon Redshift security group.</p> <note> <p>You cannot delete a security group that is associated with any clusters. You cannot delete the default security group.</p> </note> <p> For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Amazon Redshift Cluster Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `deleteClusterSnapshot`

``` purescript
deleteClusterSnapshot :: forall eff. DeleteClusterSnapshotMessage -> Aff (err :: RequestError | eff) DeleteClusterSnapshotResult
```

<p>Deletes the specified manual snapshot. The snapshot must be in the <code>available</code> state, with no other users authorized to access the snapshot. </p> <p>Unlike automated snapshots, manual snapshots are retained even after you delete your cluster. Amazon Redshift does not delete your manual snapshots. You must delete manual snapshot explicitly to avoid getting charged. If other accounts are authorized to access the snapshot, you must revoke all of the authorizations before you can delete the snapshot.</p>

#### `deleteClusterSubnetGroup`

``` purescript
deleteClusterSubnetGroup :: forall eff. DeleteClusterSubnetGroupMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified cluster subnet group.</p>

#### `deleteEventSubscription`

``` purescript
deleteEventSubscription :: forall eff. DeleteEventSubscriptionMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes an Amazon Redshift event notification subscription.</p>

#### `deleteHsmClientCertificate`

``` purescript
deleteHsmClientCertificate :: forall eff. DeleteHsmClientCertificateMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified HSM client certificate.</p>

#### `deleteHsmConfiguration`

``` purescript
deleteHsmConfiguration :: forall eff. DeleteHsmConfigurationMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified Amazon Redshift HSM configuration.</p>

#### `deleteSnapshotCopyGrant`

``` purescript
deleteSnapshotCopyGrant :: forall eff. DeleteSnapshotCopyGrantMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified snapshot copy grant.</p>

#### `deleteTags`

``` purescript
deleteTags :: forall eff. DeleteTagsMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a tag or tags from a resource. You must provide the ARN of the resource from which you want to delete the tag or tags.</p>

#### `describeClusterParameterGroups`

``` purescript
describeClusterParameterGroups :: forall eff. DescribeClusterParameterGroupsMessage -> Aff (err :: RequestError | eff) ClusterParameterGroupsMessage
```

<p>Returns a list of Amazon Redshift parameter groups, including parameter groups you created and the default parameter group. For each parameter group, the response includes the parameter group name, description, and parameter group family name. You can optionally specify a name to retrieve the description of a specific parameter group.</p> <p> For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all parameter groups that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all parameter groups that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, parameter groups are returned regardless of whether they have tag keys or values associated with them.</p>

#### `describeClusterParameters`

``` purescript
describeClusterParameters :: forall eff. DescribeClusterParametersMessage -> Aff (err :: RequestError | eff) ClusterParameterGroupDetails
```

<p>Returns a detailed list of parameters contained within the specified Amazon Redshift parameter group. For each parameter the response includes information such as parameter name, description, data type, value, whether the parameter value is modifiable, and so on.</p> <p>You can specify <i>source</i> filter to retrieve parameters of only specific type. For example, to retrieve parameters that were modified by a user action such as from <a>ModifyClusterParameterGroup</a>, you can specify <i>source</i> equal to <i>user</i>.</p> <p> For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `describeClusterSecurityGroups`

``` purescript
describeClusterSecurityGroups :: forall eff. DescribeClusterSecurityGroupsMessage -> Aff (err :: RequestError | eff) ClusterSecurityGroupMessage
```

<p>Returns information about Amazon Redshift security groups. If the name of a security group is specified, the response will contain only information about only that security group.</p> <p> For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Amazon Redshift Cluster Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all security groups that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all security groups that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, security groups are returned regardless of whether they have tag keys or values associated with them.</p>

#### `describeClusterSnapshots`

``` purescript
describeClusterSnapshots :: forall eff. DescribeClusterSnapshotsMessage -> Aff (err :: RequestError | eff) SnapshotMessage
```

<p>Returns one or more snapshot objects, which contain metadata about your cluster snapshots. By default, this operation returns information about all snapshots of all clusters that are owned by you AWS customer account. No information is returned for snapshots owned by inactive AWS customer accounts.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all snapshots that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all snapshots that have any combination of those values are returned. Only snapshots that you own are returned in the response; shared snapshots are not returned with the tag key and tag value request parameters.</p> <p>If both tag keys and values are omitted from the request, snapshots are returned regardless of whether they have tag keys or values associated with them.</p>

#### `describeClusterSubnetGroups`

``` purescript
describeClusterSubnetGroups :: forall eff. DescribeClusterSubnetGroupsMessage -> Aff (err :: RequestError | eff) ClusterSubnetGroupMessage
```

<p>Returns one or more cluster subnet group objects, which contain metadata about your cluster subnet groups. By default, this operation returns information about all cluster subnet groups that are defined in you AWS account.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all subnet groups that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all subnet groups that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, subnet groups are returned regardless of whether they have tag keys or values associated with them.</p>

#### `describeClusterVersions`

``` purescript
describeClusterVersions :: forall eff. DescribeClusterVersionsMessage -> Aff (err :: RequestError | eff) ClusterVersionsMessage
```

<p>Returns descriptions of the available Amazon Redshift cluster versions. You can call this operation even before creating any clusters to learn more about the Amazon Redshift versions. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `describeClusters`

``` purescript
describeClusters :: forall eff. DescribeClustersMessage -> Aff (err :: RequestError | eff) ClustersMessage
```

<p>Returns properties of provisioned clusters including general cluster properties, cluster database properties, maintenance and backup properties, and security and access properties. This operation supports pagination. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all clusters that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all clusters that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, clusters are returned regardless of whether they have tag keys or values associated with them.</p>

#### `describeDefaultClusterParameters`

``` purescript
describeDefaultClusterParameters :: forall eff. DescribeDefaultClusterParametersMessage -> Aff (err :: RequestError | eff) DescribeDefaultClusterParametersResult
```

<p>Returns a list of parameter settings for the specified parameter group family.</p> <p> For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `describeEventCategories`

``` purescript
describeEventCategories :: forall eff. DescribeEventCategoriesMessage -> Aff (err :: RequestError | eff) EventCategoriesMessage
```

<p>Displays a list of event categories for all event source types, or for a specified source type. For a list of the event categories and source types, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-event-notifications.html">Amazon Redshift Event Notifications</a>.</p>

#### `describeEventSubscriptions`

``` purescript
describeEventSubscriptions :: forall eff. DescribeEventSubscriptionsMessage -> Aff (err :: RequestError | eff) EventSubscriptionsMessage
```

<p>Lists descriptions of all the Amazon Redshift event notification subscriptions for a customer account. If you specify a subscription name, lists the description for that subscription.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all event notification subscriptions that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all subscriptions that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, subscriptions are returned regardless of whether they have tag keys or values associated with them.</p>

#### `describeEvents`

``` purescript
describeEvents :: forall eff. DescribeEventsMessage -> Aff (err :: RequestError | eff) EventsMessage
```

<p>Returns events related to clusters, security groups, snapshots, and parameter groups for the past 14 days. Events specific to a particular cluster, security group, snapshot or parameter group can be obtained by providing the name as a parameter. By default, the past hour of events are returned.</p>

#### `describeHsmClientCertificates`

``` purescript
describeHsmClientCertificates :: forall eff. DescribeHsmClientCertificatesMessage -> Aff (err :: RequestError | eff) HsmClientCertificateMessage
```

<p>Returns information about the specified HSM client certificate. If no certificate ID is specified, returns information about all the HSM certificates owned by your AWS customer account.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all HSM client certificates that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all HSM client certificates that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, HSM client certificates are returned regardless of whether they have tag keys or values associated with them.</p>

#### `describeHsmConfigurations`

``` purescript
describeHsmConfigurations :: forall eff. DescribeHsmConfigurationsMessage -> Aff (err :: RequestError | eff) HsmConfigurationMessage
```

<p>Returns information about the specified Amazon Redshift HSM configuration. If no configuration ID is specified, returns information about all the HSM configurations owned by your AWS customer account.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all HSM connections that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all HSM connections that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, HSM connections are returned regardless of whether they have tag keys or values associated with them.</p>

#### `describeLoggingStatus`

``` purescript
describeLoggingStatus :: forall eff. DescribeLoggingStatusMessage -> Aff (err :: RequestError | eff) LoggingStatus
```

<p>Describes whether information, such as queries and connection attempts, is being logged for the specified Amazon Redshift cluster.</p>

#### `describeOrderableClusterOptions`

``` purescript
describeOrderableClusterOptions :: forall eff. DescribeOrderableClusterOptionsMessage -> Aff (err :: RequestError | eff) OrderableClusterOptionsMessage
```

<p>Returns a list of orderable cluster options. Before you create a new cluster you can use this operation to find what options are available, such as the EC2 Availability Zones (AZ) in the specific AWS region that you can specify, and the node types you can request. The node types differ by available storage, memory, CPU and price. With the cost involved you might want to obtain a list of cluster options in the specific region and specify values when creating a cluster. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `describeReservedNodeOfferings`

``` purescript
describeReservedNodeOfferings :: forall eff. DescribeReservedNodeOfferingsMessage -> Aff (err :: RequestError | eff) ReservedNodeOfferingsMessage
```

<p>Returns a list of the available reserved node offerings by Amazon Redshift with their descriptions including the node type, the fixed and recurring costs of reserving the node and duration the node will be reserved for you. These descriptions help you determine which reserve node offering you want to purchase. You then use the unique offering ID in you call to <a>PurchaseReservedNodeOffering</a> to reserve one or more nodes for your Amazon Redshift cluster. </p> <p> For more information about reserved node offerings, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html">Purchasing Reserved Nodes</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `describeReservedNodes`

``` purescript
describeReservedNodes :: forall eff. DescribeReservedNodesMessage -> Aff (err :: RequestError | eff) ReservedNodesMessage
```

<p>Returns the descriptions of the reserved nodes.</p>

#### `describeResize`

``` purescript
describeResize :: forall eff. DescribeResizeMessage -> Aff (err :: RequestError | eff) ResizeProgressMessage
```

<p>Returns information about the last resize operation for the specified cluster. If no resize operation has ever been initiated for the specified cluster, a <code>HTTP 404</code> error is returned. If a resize operation was initiated and completed, the status of the resize remains as <code>SUCCEEDED</code> until the next resize. </p> <p>A resize operation can be requested using <a>ModifyCluster</a> and specifying a different number or type of nodes for the cluster. </p>

#### `describeSnapshotCopyGrants`

``` purescript
describeSnapshotCopyGrants :: forall eff. DescribeSnapshotCopyGrantsMessage -> Aff (err :: RequestError | eff) SnapshotCopyGrantMessage
```

<p>Returns a list of snapshot copy grants owned by the AWS account in the destination region.</p> <p> For more information about managing snapshot copy grants, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html">Amazon Redshift Database Encryption</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `describeTableRestoreStatus`

``` purescript
describeTableRestoreStatus :: forall eff. DescribeTableRestoreStatusMessage -> Aff (err :: RequestError | eff) TableRestoreStatusMessage
```

<p>Lists the status of one or more table restore requests made using the <a>RestoreTableFromClusterSnapshot</a> API action. If you don't specify a value for the <code>TableRestoreRequestId</code> parameter, then <code>DescribeTableRestoreStatus</code> returns the status of all table restore requests ordered by the date and time of the request in ascending order. Otherwise <code>DescribeTableRestoreStatus</code> returns the status of the table specified by <code>TableRestoreRequestId</code>.</p>

#### `describeTags`

``` purescript
describeTags :: forall eff. DescribeTagsMessage -> Aff (err :: RequestError | eff) TaggedResourceListMessage
```

<p>Returns a list of tags. You can return tags from a specific resource by specifying an ARN, or you can return all tags for a given type of resource, such as clusters, snapshots, and so on.</p> <p>The following are limitations for <code>DescribeTags</code>: </p> <ul> <li> <p>You cannot specify an ARN and a resource-type value together in the same request.</p> </li> <li> <p>You cannot use the <code>MaxRecords</code> and <code>Marker</code> parameters together with the ARN parameter.</p> </li> <li> <p>The <code>MaxRecords</code> parameter can be a range from 10 to 50 results to return in a request.</p> </li> </ul> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all resources that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all resources that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, resources are returned regardless of whether they have tag keys or values associated with them.</p>

#### `disableLogging`

``` purescript
disableLogging :: forall eff. DisableLoggingMessage -> Aff (err :: RequestError | eff) LoggingStatus
```

<p>Stops logging information, such as queries and connection attempts, for the specified Amazon Redshift cluster.</p>

#### `disableSnapshotCopy`

``` purescript
disableSnapshotCopy :: forall eff. DisableSnapshotCopyMessage -> Aff (err :: RequestError | eff) DisableSnapshotCopyResult
```

<p>Disables the automatic copying of snapshots from one region to another region for a specified cluster.</p> <p>If your cluster and its snapshots are encrypted using a customer master key (CMK) from AWS KMS, use <a>DeleteSnapshotCopyGrant</a> to delete the grant that grants Amazon Redshift permission to the CMK in the destination region. </p>

#### `enableLogging`

``` purescript
enableLogging :: forall eff. EnableLoggingMessage -> Aff (err :: RequestError | eff) LoggingStatus
```

<p>Starts logging information, such as queries and connection attempts, for the specified Amazon Redshift cluster.</p>

#### `enableSnapshotCopy`

``` purescript
enableSnapshotCopy :: forall eff. EnableSnapshotCopyMessage -> Aff (err :: RequestError | eff) EnableSnapshotCopyResult
```

<p>Enables the automatic copy of snapshots from one region to another region for a specified cluster.</p>

#### `getClusterCredentials`

``` purescript
getClusterCredentials :: forall eff. GetClusterCredentialsMessage -> Aff (err :: RequestError | eff) ClusterCredentials
```

<p>Returns a database user name and temporary password with temporary authorization to log on to an Amazon Redshift database. The action returns the database user name prefixed with <code>IAM:</code> if <code>AutoCreate</code> is <code>False</code> or <code>IAMA:</code> if <code>AutoCreate</code> is <code>True</code>. You can optionally specify one or more database user groups that the user will join at log on. By default, the temporary credentials expire in 900 seconds. You can optionally specify a duration between 900 seconds (15 minutes) and 3600 seconds (60 minutes). For more information, see <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/generating-user-credentials.html">Using IAM Authentication to Generate Database User Credentials</a> in the Amazon Redshift Cluster Management Guide.</p> <p>The AWS Identity and Access Management (IAM)user or role that executes GetClusterCredentials must have an IAM policy attached that allows access to all necessary actions and resources. For more information about permissions, see <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html#redshift-policy-resources.getclustercredentials-resources">Resource Policies for GetClusterCredentials</a> in the Amazon Redshift Cluster Management Guide.</p> <p>If the <code>DbGroups</code> parameter is specified, the IAM policy must allow the <code>redshift:JoinGroup</code> action with access to the listed <code>dbgroups</code>. </p> <p>In addition, if the <code>AutoCreate</code> parameter is set to <code>True</code>, then the policy must include the <code>redshift:CreateClusterUser</code> privilege.</p> <p>If the <code>DbName</code> parameter is specified, the IAM policy must allow access to the resource <code>dbname</code> for the specified database name. </p>

#### `modifyCluster`

``` purescript
modifyCluster :: forall eff. ModifyClusterMessage -> Aff (err :: RequestError | eff) ModifyClusterResult
```

<p>Modifies the settings for a cluster. For example, you can add another security or parameter group, update the preferred maintenance window, or change the master user password. Resetting a cluster password or modifying the security groups associated with a cluster do not need a reboot. However, modifying a parameter group requires a reboot for parameters to take effect. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>You can also change node type and the number of nodes to scale up or down the cluster. When resizing a cluster, you must specify both the number of nodes and the node type even if one of the parameters does not change.</p>

#### `modifyClusterIamRoles`

``` purescript
modifyClusterIamRoles :: forall eff. ModifyClusterIamRolesMessage -> Aff (err :: RequestError | eff) ModifyClusterIamRolesResult
```

<p>Modifies the list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.</p> <p>A cluster can have up to 10 IAM roles associated at any time.</p>

#### `modifyClusterParameterGroup`

``` purescript
modifyClusterParameterGroup :: forall eff. ModifyClusterParameterGroupMessage -> Aff (err :: RequestError | eff) ClusterParameterGroupNameMessage
```

<p>Modifies the parameters of a parameter group.</p> <p> For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `modifyClusterSubnetGroup`

``` purescript
modifyClusterSubnetGroup :: forall eff. ModifyClusterSubnetGroupMessage -> Aff (err :: RequestError | eff) ModifyClusterSubnetGroupResult
```

<p>Modifies a cluster subnet group to include the specified list of VPC subnets. The operation replaces the existing list of subnets with the new list of subnets.</p>

#### `modifyEventSubscription`

``` purescript
modifyEventSubscription :: forall eff. ModifyEventSubscriptionMessage -> Aff (err :: RequestError | eff) ModifyEventSubscriptionResult
```

<p>Modifies an existing Amazon Redshift event notification subscription.</p>

#### `modifySnapshotCopyRetentionPeriod`

``` purescript
modifySnapshotCopyRetentionPeriod :: forall eff. ModifySnapshotCopyRetentionPeriodMessage -> Aff (err :: RequestError | eff) ModifySnapshotCopyRetentionPeriodResult
```

<p>Modifies the number of days to retain automated snapshots in the destination region after they are copied from the source region.</p>

#### `purchaseReservedNodeOffering`

``` purescript
purchaseReservedNodeOffering :: forall eff. PurchaseReservedNodeOfferingMessage -> Aff (err :: RequestError | eff) PurchaseReservedNodeOfferingResult
```

<p>Allows you to purchase reserved nodes. Amazon Redshift offers a predefined set of reserved node offerings. You can purchase one or more of the offerings. You can call the <a>DescribeReservedNodeOfferings</a> API to obtain the available reserved node offerings. You can call this API by providing a specific reserved node offering and the number of nodes you want to reserve. </p> <p> For more information about reserved node offerings, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html">Purchasing Reserved Nodes</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `rebootCluster`

``` purescript
rebootCluster :: forall eff. RebootClusterMessage -> Aff (err :: RequestError | eff) RebootClusterResult
```

<p>Reboots a cluster. This action is taken as soon as possible. It results in a momentary outage to the cluster, during which the cluster status is set to <code>rebooting</code>. A cluster event is created when the reboot is completed. Any pending cluster modifications (see <a>ModifyCluster</a>) are applied at this reboot. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `resetClusterParameterGroup`

``` purescript
resetClusterParameterGroup :: forall eff. ResetClusterParameterGroupMessage -> Aff (err :: RequestError | eff) ClusterParameterGroupNameMessage
```

<p>Sets one or more parameters of the specified parameter group to their default values and sets the source values of the parameters to "engine-default". To reset the entire parameter group specify the <i>ResetAllParameters</i> parameter. For parameter changes to take effect you must reboot any associated clusters. </p>

#### `restoreFromClusterSnapshot`

``` purescript
restoreFromClusterSnapshot :: forall eff. RestoreFromClusterSnapshotMessage -> Aff (err :: RequestError | eff) RestoreFromClusterSnapshotResult
```

<p>Creates a new cluster from a snapshot. By default, Amazon Redshift creates the resulting cluster with the same configuration as the original cluster from which the snapshot was created, except that the new cluster is created with the default cluster security and parameter groups. After Amazon Redshift creates the cluster, you can use the <a>ModifyCluster</a> API to associate a different security group and different parameter group with the restored cluster. If you are using a DS node type, you can also choose to change to another DS node type of the same size during restore.</p> <p>If you restore a cluster into a VPC, you must provide a cluster subnet group where you want the cluster restored.</p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `restoreTableFromClusterSnapshot`

``` purescript
restoreTableFromClusterSnapshot :: forall eff. RestoreTableFromClusterSnapshotMessage -> Aff (err :: RequestError | eff) RestoreTableFromClusterSnapshotResult
```

<p>Creates a new table from a table in an Amazon Redshift cluster snapshot. You must create the new table within the Amazon Redshift cluster that the snapshot was taken from.</p> <p>You cannot use <code>RestoreTableFromClusterSnapshot</code> to restore a table with the same name as an existing table in an Amazon Redshift cluster. That is, you cannot overwrite an existing table in a cluster with a restored table. If you want to replace your original table with a new, restored table, then rename or drop your original table before you call <code>RestoreTableFromClusterSnapshot</code>. When you have renamed your original table, then you can pass the original name of the table as the <code>NewTableName</code> parameter value in the call to <code>RestoreTableFromClusterSnapshot</code>. This way, you can replace the original table with the table created from the snapshot.</p>

#### `revokeClusterSecurityGroupIngress`

``` purescript
revokeClusterSecurityGroupIngress :: forall eff. RevokeClusterSecurityGroupIngressMessage -> Aff (err :: RequestError | eff) RevokeClusterSecurityGroupIngressResult
```

<p>Revokes an ingress rule in an Amazon Redshift security group for a previously authorized IP range or Amazon EC2 security group. To add an ingress rule, see <a>AuthorizeClusterSecurityGroupIngress</a>. For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Amazon Redshift Cluster Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `revokeSnapshotAccess`

``` purescript
revokeSnapshotAccess :: forall eff. RevokeSnapshotAccessMessage -> Aff (err :: RequestError | eff) RevokeSnapshotAccessResult
```

<p>Removes the ability of the specified AWS customer account to restore the specified snapshot. If the account is currently restoring the snapshot, the restore will run to completion.</p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>

#### `rotateEncryptionKey`

``` purescript
rotateEncryptionKey :: forall eff. RotateEncryptionKeyMessage -> Aff (err :: RequestError | eff) RotateEncryptionKeyResult
```

<p>Rotates the encryption keys for a cluster.</p>

#### `AccessToSnapshotDeniedFault`

``` purescript
newtype AccessToSnapshotDeniedFault
  = AccessToSnapshotDeniedFault {  }
```

<p>The owner of the specified snapshot has not authorized your account to access the snapshot.</p>

#### `AccountWithRestoreAccess`

``` purescript
newtype AccountWithRestoreAccess
  = AccountWithRestoreAccess { "AccountId" :: NullOrUndefined (String), "AccountAlias" :: NullOrUndefined (String) }
```

<p>Describes an AWS customer account authorized to restore a snapshot.</p>

#### `AccountsWithRestoreAccessList`

``` purescript
newtype AccountsWithRestoreAccessList
  = AccountsWithRestoreAccessList (Array AccountWithRestoreAccess)
```

#### `AuthorizationAlreadyExistsFault`

``` purescript
newtype AuthorizationAlreadyExistsFault
  = AuthorizationAlreadyExistsFault {  }
```

<p>The specified CIDR block or EC2 security group is already authorized for the specified cluster security group.</p>

#### `AuthorizationNotFoundFault`

``` purescript
newtype AuthorizationNotFoundFault
  = AuthorizationNotFoundFault {  }
```

<p>The specified CIDR IP range or EC2 security group is not authorized for the specified cluster security group.</p>

#### `AuthorizationQuotaExceededFault`

``` purescript
newtype AuthorizationQuotaExceededFault
  = AuthorizationQuotaExceededFault {  }
```

<p>The authorization quota for the cluster security group has been reached.</p>

#### `AuthorizeClusterSecurityGroupIngressMessage`

``` purescript
newtype AuthorizeClusterSecurityGroupIngressMessage
  = AuthorizeClusterSecurityGroupIngressMessage { "ClusterSecurityGroupName" :: String, "CIDRIP" :: NullOrUndefined (String), "EC2SecurityGroupName" :: NullOrUndefined (String), "EC2SecurityGroupOwnerId" :: NullOrUndefined (String) }
```

<p/>

#### `AuthorizeClusterSecurityGroupIngressResult`

``` purescript
newtype AuthorizeClusterSecurityGroupIngressResult
  = AuthorizeClusterSecurityGroupIngressResult { "ClusterSecurityGroup" :: NullOrUndefined (ClusterSecurityGroup) }
```

#### `AuthorizeSnapshotAccessMessage`

``` purescript
newtype AuthorizeSnapshotAccessMessage
  = AuthorizeSnapshotAccessMessage { "SnapshotIdentifier" :: String, "SnapshotClusterIdentifier" :: NullOrUndefined (String), "AccountWithRestoreAccess" :: String }
```

<p/>

#### `AuthorizeSnapshotAccessResult`

``` purescript
newtype AuthorizeSnapshotAccessResult
  = AuthorizeSnapshotAccessResult { "Snapshot" :: NullOrUndefined (Snapshot) }
```

#### `AvailabilityZone`

``` purescript
newtype AvailabilityZone
  = AvailabilityZone { "Name" :: NullOrUndefined (String) }
```

<p>Describes an availability zone.</p>

#### `AvailabilityZoneList`

``` purescript
newtype AvailabilityZoneList
  = AvailabilityZoneList (Array AvailabilityZone)
```

#### `BooleanOptional`

``` purescript
newtype BooleanOptional
  = BooleanOptional Boolean
```

#### `BucketNotFoundFault`

``` purescript
newtype BucketNotFoundFault
  = BucketNotFoundFault {  }
```

<p>Could not find the specified S3 bucket.</p>

#### `Cluster`

``` purescript
newtype Cluster
  = Cluster { "ClusterIdentifier" :: NullOrUndefined (String), "NodeType" :: NullOrUndefined (String), "ClusterStatus" :: NullOrUndefined (String), "ModifyStatus" :: NullOrUndefined (String), "MasterUsername" :: NullOrUndefined (String), "DBName" :: NullOrUndefined (String), "Endpoint" :: NullOrUndefined (Endpoint), "ClusterCreateTime" :: NullOrUndefined (TStamp), "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined (Int), "ClusterSecurityGroups" :: NullOrUndefined (ClusterSecurityGroupMembershipList), "VpcSecurityGroups" :: NullOrUndefined (VpcSecurityGroupMembershipList), "ClusterParameterGroups" :: NullOrUndefined (ClusterParameterGroupStatusList), "ClusterSubnetGroupName" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "AvailabilityZone" :: NullOrUndefined (String), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "PendingModifiedValues" :: NullOrUndefined (PendingModifiedValues), "ClusterVersion" :: NullOrUndefined (String), "AllowVersionUpgrade" :: NullOrUndefined (Boolean), "NumberOfNodes" :: NullOrUndefined (Int), "PubliclyAccessible" :: NullOrUndefined (Boolean), "Encrypted" :: NullOrUndefined (Boolean), "RestoreStatus" :: NullOrUndefined (RestoreStatus), "HsmStatus" :: NullOrUndefined (HsmStatus), "ClusterSnapshotCopyStatus" :: NullOrUndefined (ClusterSnapshotCopyStatus), "ClusterPublicKey" :: NullOrUndefined (String), "ClusterNodes" :: NullOrUndefined (ClusterNodesList), "ElasticIpStatus" :: NullOrUndefined (ElasticIpStatus), "ClusterRevisionNumber" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList), "KmsKeyId" :: NullOrUndefined (String), "EnhancedVpcRouting" :: NullOrUndefined (Boolean), "IamRoles" :: NullOrUndefined (ClusterIamRoleList) }
```

<p>Describes a cluster.</p>

#### `ClusterAlreadyExistsFault`

``` purescript
newtype ClusterAlreadyExistsFault
  = ClusterAlreadyExistsFault {  }
```

<p>The account already has a cluster with the given identifier.</p>

#### `ClusterCredentials`

``` purescript
newtype ClusterCredentials
  = ClusterCredentials { "DbUser" :: NullOrUndefined (String), "DbPassword" :: NullOrUndefined (SensitiveString), "Expiration" :: NullOrUndefined (TStamp) }
```

<p>Temporary credentials with authorization to log on to an Amazon Redshift database. </p>

#### `ClusterIamRole`

``` purescript
newtype ClusterIamRole
  = ClusterIamRole { "IamRoleArn" :: NullOrUndefined (String), "ApplyStatus" :: NullOrUndefined (String) }
```

<p>An AWS Identity and Access Management (IAM) role that can be used by the associated Amazon Redshift cluster to access other AWS services.</p>

#### `ClusterIamRoleList`

``` purescript
newtype ClusterIamRoleList
  = ClusterIamRoleList (Array ClusterIamRole)
```

#### `ClusterList`

``` purescript
newtype ClusterList
  = ClusterList (Array Cluster)
```

#### `ClusterNode`

``` purescript
newtype ClusterNode
  = ClusterNode { "NodeRole" :: NullOrUndefined (String), "PrivateIPAddress" :: NullOrUndefined (String), "PublicIPAddress" :: NullOrUndefined (String) }
```

<p>The identifier of a node in a cluster.</p>

#### `ClusterNodesList`

``` purescript
newtype ClusterNodesList
  = ClusterNodesList (Array ClusterNode)
```

#### `ClusterNotFoundFault`

``` purescript
newtype ClusterNotFoundFault
  = ClusterNotFoundFault {  }
```

<p>The <code>ClusterIdentifier</code> parameter does not refer to an existing cluster. </p>

#### `ClusterParameterGroup`

``` purescript
newtype ClusterParameterGroup
  = ClusterParameterGroup { "ParameterGroupName" :: NullOrUndefined (String), "ParameterGroupFamily" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a parameter group.</p>

#### `ClusterParameterGroupAlreadyExistsFault`

``` purescript
newtype ClusterParameterGroupAlreadyExistsFault
  = ClusterParameterGroupAlreadyExistsFault {  }
```

<p>A cluster parameter group with the same name already exists.</p>

#### `ClusterParameterGroupDetails`

``` purescript
newtype ClusterParameterGroupDetails
  = ClusterParameterGroupDetails { "Parameters" :: NullOrUndefined (ParametersList), "Marker" :: NullOrUndefined (String) }
```

<p>Contains the output from the <a>DescribeClusterParameters</a> action. </p>

#### `ClusterParameterGroupNameMessage`

``` purescript
newtype ClusterParameterGroupNameMessage
  = ClusterParameterGroupNameMessage { "ParameterGroupName" :: NullOrUndefined (String), "ParameterGroupStatus" :: NullOrUndefined (String) }
```

<p/>

#### `ClusterParameterGroupNotFoundFault`

``` purescript
newtype ClusterParameterGroupNotFoundFault
  = ClusterParameterGroupNotFoundFault {  }
```

<p>The parameter group name does not refer to an existing parameter group.</p>

#### `ClusterParameterGroupQuotaExceededFault`

``` purescript
newtype ClusterParameterGroupQuotaExceededFault
  = ClusterParameterGroupQuotaExceededFault {  }
```

<p>The request would result in the user exceeding the allowed number of cluster parameter groups. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `ClusterParameterGroupStatus`

``` purescript
newtype ClusterParameterGroupStatus
  = ClusterParameterGroupStatus { "ParameterGroupName" :: NullOrUndefined (String), "ParameterApplyStatus" :: NullOrUndefined (String), "ClusterParameterStatusList" :: NullOrUndefined (ClusterParameterStatusList) }
```

<p>Describes the status of a parameter group.</p>

#### `ClusterParameterGroupStatusList`

``` purescript
newtype ClusterParameterGroupStatusList
  = ClusterParameterGroupStatusList (Array ClusterParameterGroupStatus)
```

#### `ClusterParameterGroupsMessage`

``` purescript
newtype ClusterParameterGroupsMessage
  = ClusterParameterGroupsMessage { "Marker" :: NullOrUndefined (String), "ParameterGroups" :: NullOrUndefined (ParameterGroupList) }
```

<p>Contains the output from the <a>DescribeClusterParameterGroups</a> action. </p>

#### `ClusterParameterStatus`

``` purescript
newtype ClusterParameterStatus
  = ClusterParameterStatus { "ParameterName" :: NullOrUndefined (String), "ParameterApplyStatus" :: NullOrUndefined (String), "ParameterApplyErrorDescription" :: NullOrUndefined (String) }
```

<p>Describes the status of a parameter group.</p>

#### `ClusterParameterStatusList`

``` purescript
newtype ClusterParameterStatusList
  = ClusterParameterStatusList (Array ClusterParameterStatus)
```

#### `ClusterQuotaExceededFault`

``` purescript
newtype ClusterQuotaExceededFault
  = ClusterQuotaExceededFault {  }
```

<p>The request would exceed the allowed number of cluster instances for this account. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `ClusterSecurityGroup`

``` purescript
newtype ClusterSecurityGroup
  = ClusterSecurityGroup { "ClusterSecurityGroupName" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "EC2SecurityGroups" :: NullOrUndefined (EC2SecurityGroupList), "IPRanges" :: NullOrUndefined (IPRangeList), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a security group.</p>

#### `ClusterSecurityGroupAlreadyExistsFault`

``` purescript
newtype ClusterSecurityGroupAlreadyExistsFault
  = ClusterSecurityGroupAlreadyExistsFault {  }
```

<p>A cluster security group with the same name already exists.</p>

#### `ClusterSecurityGroupMembership`

``` purescript
newtype ClusterSecurityGroupMembership
  = ClusterSecurityGroupMembership { "ClusterSecurityGroupName" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String) }
```

<p>Describes a cluster security group.</p>

#### `ClusterSecurityGroupMembershipList`

``` purescript
newtype ClusterSecurityGroupMembershipList
  = ClusterSecurityGroupMembershipList (Array ClusterSecurityGroupMembership)
```

#### `ClusterSecurityGroupMessage`

``` purescript
newtype ClusterSecurityGroupMessage
  = ClusterSecurityGroupMessage { "Marker" :: NullOrUndefined (String), "ClusterSecurityGroups" :: NullOrUndefined (ClusterSecurityGroups) }
```

<p/>

#### `ClusterSecurityGroupNameList`

``` purescript
newtype ClusterSecurityGroupNameList
  = ClusterSecurityGroupNameList (Array String)
```

#### `ClusterSecurityGroupNotFoundFault`

``` purescript
newtype ClusterSecurityGroupNotFoundFault
  = ClusterSecurityGroupNotFoundFault {  }
```

<p>The cluster security group name does not refer to an existing cluster security group.</p>

#### `ClusterSecurityGroupQuotaExceededFault`

``` purescript
newtype ClusterSecurityGroupQuotaExceededFault
  = ClusterSecurityGroupQuotaExceededFault {  }
```

<p>The request would result in the user exceeding the allowed number of cluster security groups. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `ClusterSecurityGroups`

``` purescript
newtype ClusterSecurityGroups
  = ClusterSecurityGroups (Array ClusterSecurityGroup)
```

#### `ClusterSnapshotAlreadyExistsFault`

``` purescript
newtype ClusterSnapshotAlreadyExistsFault
  = ClusterSnapshotAlreadyExistsFault {  }
```

<p>The value specified as a snapshot identifier is already used by an existing snapshot.</p>

#### `ClusterSnapshotCopyStatus`

``` purescript
newtype ClusterSnapshotCopyStatus
  = ClusterSnapshotCopyStatus { "DestinationRegion" :: NullOrUndefined (String), "RetentionPeriod" :: NullOrUndefined (Number), "SnapshotCopyGrantName" :: NullOrUndefined (String) }
```

<p>Returns the destination region and retention period that are configured for cross-region snapshot copy.</p>

#### `ClusterSnapshotNotFoundFault`

``` purescript
newtype ClusterSnapshotNotFoundFault
  = ClusterSnapshotNotFoundFault {  }
```

<p>The snapshot identifier does not refer to an existing cluster snapshot.</p>

#### `ClusterSnapshotQuotaExceededFault`

``` purescript
newtype ClusterSnapshotQuotaExceededFault
  = ClusterSnapshotQuotaExceededFault {  }
```

<p>The request would result in the user exceeding the allowed number of cluster snapshots.</p>

#### `ClusterSubnetGroup`

``` purescript
newtype ClusterSubnetGroup
  = ClusterSubnetGroup { "ClusterSubnetGroupName" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "SubnetGroupStatus" :: NullOrUndefined (String), "Subnets" :: NullOrUndefined (SubnetList), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a subnet group.</p>

#### `ClusterSubnetGroupAlreadyExistsFault`

``` purescript
newtype ClusterSubnetGroupAlreadyExistsFault
  = ClusterSubnetGroupAlreadyExistsFault {  }
```

<p>A <i>ClusterSubnetGroupName</i> is already used by an existing cluster subnet group. </p>

#### `ClusterSubnetGroupMessage`

``` purescript
newtype ClusterSubnetGroupMessage
  = ClusterSubnetGroupMessage { "Marker" :: NullOrUndefined (String), "ClusterSubnetGroups" :: NullOrUndefined (ClusterSubnetGroups) }
```

<p>Contains the output from the <a>DescribeClusterSubnetGroups</a> action. </p>

#### `ClusterSubnetGroupNotFoundFault`

``` purescript
newtype ClusterSubnetGroupNotFoundFault
  = ClusterSubnetGroupNotFoundFault {  }
```

<p>The cluster subnet group name does not refer to an existing cluster subnet group.</p>

#### `ClusterSubnetGroupQuotaExceededFault`

``` purescript
newtype ClusterSubnetGroupQuotaExceededFault
  = ClusterSubnetGroupQuotaExceededFault {  }
```

<p>The request would result in user exceeding the allowed number of cluster subnet groups. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `ClusterSubnetGroups`

``` purescript
newtype ClusterSubnetGroups
  = ClusterSubnetGroups (Array ClusterSubnetGroup)
```

#### `ClusterSubnetQuotaExceededFault`

``` purescript
newtype ClusterSubnetQuotaExceededFault
  = ClusterSubnetQuotaExceededFault {  }
```

<p>The request would result in user exceeding the allowed number of subnets in a cluster subnet groups. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `ClusterVersion`

``` purescript
newtype ClusterVersion
  = ClusterVersion { "ClusterVersion" :: NullOrUndefined (String), "ClusterParameterGroupFamily" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

<p>Describes a cluster version, including the parameter group family and description of the version.</p>

#### `ClusterVersionList`

``` purescript
newtype ClusterVersionList
  = ClusterVersionList (Array ClusterVersion)
```

#### `ClusterVersionsMessage`

``` purescript
newtype ClusterVersionsMessage
  = ClusterVersionsMessage { "Marker" :: NullOrUndefined (String), "ClusterVersions" :: NullOrUndefined (ClusterVersionList) }
```

<p>Contains the output from the <a>DescribeClusterVersions</a> action. </p>

#### `ClustersMessage`

``` purescript
newtype ClustersMessage
  = ClustersMessage { "Marker" :: NullOrUndefined (String), "Clusters" :: NullOrUndefined (ClusterList) }
```

<p>Contains the output from the <a>DescribeClusters</a> action. </p>

#### `CopyClusterSnapshotMessage`

``` purescript
newtype CopyClusterSnapshotMessage
  = CopyClusterSnapshotMessage { "SourceSnapshotIdentifier" :: String, "SourceSnapshotClusterIdentifier" :: NullOrUndefined (String), "TargetSnapshotIdentifier" :: String }
```

<p/>

#### `CopyClusterSnapshotResult`

``` purescript
newtype CopyClusterSnapshotResult
  = CopyClusterSnapshotResult { "Snapshot" :: NullOrUndefined (Snapshot) }
```

#### `CopyToRegionDisabledFault`

``` purescript
newtype CopyToRegionDisabledFault
  = CopyToRegionDisabledFault {  }
```

<p>Cross-region snapshot copy was temporarily disabled. Try your request again.</p>

#### `CreateClusterMessage`

``` purescript
newtype CreateClusterMessage
  = CreateClusterMessage { "DBName" :: NullOrUndefined (String), "ClusterIdentifier" :: String, "ClusterType" :: NullOrUndefined (String), "NodeType" :: String, "MasterUsername" :: String, "MasterUserPassword" :: String, "ClusterSecurityGroups" :: NullOrUndefined (ClusterSecurityGroupNameList), "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList), "ClusterSubnetGroupName" :: NullOrUndefined (String), "AvailabilityZone" :: NullOrUndefined (String), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "ClusterParameterGroupName" :: NullOrUndefined (String), "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined (IntegerOptional), "Port" :: NullOrUndefined (IntegerOptional), "ClusterVersion" :: NullOrUndefined (String), "AllowVersionUpgrade" :: NullOrUndefined (BooleanOptional), "NumberOfNodes" :: NullOrUndefined (IntegerOptional), "PubliclyAccessible" :: NullOrUndefined (BooleanOptional), "Encrypted" :: NullOrUndefined (BooleanOptional), "HsmClientCertificateIdentifier" :: NullOrUndefined (String), "HsmConfigurationIdentifier" :: NullOrUndefined (String), "ElasticIp" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList), "KmsKeyId" :: NullOrUndefined (String), "EnhancedVpcRouting" :: NullOrUndefined (BooleanOptional), "AdditionalInfo" :: NullOrUndefined (String), "IamRoles" :: NullOrUndefined (IamRoleArnList) }
```

<p/>

#### `CreateClusterParameterGroupMessage`

``` purescript
newtype CreateClusterParameterGroupMessage
  = CreateClusterParameterGroupMessage { "ParameterGroupName" :: String, "ParameterGroupFamily" :: String, "Description" :: String, "Tags" :: NullOrUndefined (TagList) }
```

<p/>

#### `CreateClusterParameterGroupResult`

``` purescript
newtype CreateClusterParameterGroupResult
  = CreateClusterParameterGroupResult { "ClusterParameterGroup" :: NullOrUndefined (ClusterParameterGroup) }
```

#### `CreateClusterResult`

``` purescript
newtype CreateClusterResult
  = CreateClusterResult { "Cluster" :: NullOrUndefined (Cluster) }
```

#### `CreateClusterSecurityGroupMessage`

``` purescript
newtype CreateClusterSecurityGroupMessage
  = CreateClusterSecurityGroupMessage { "ClusterSecurityGroupName" :: String, "Description" :: String, "Tags" :: NullOrUndefined (TagList) }
```

<p/>

#### `CreateClusterSecurityGroupResult`

``` purescript
newtype CreateClusterSecurityGroupResult
  = CreateClusterSecurityGroupResult { "ClusterSecurityGroup" :: NullOrUndefined (ClusterSecurityGroup) }
```

#### `CreateClusterSnapshotMessage`

``` purescript
newtype CreateClusterSnapshotMessage
  = CreateClusterSnapshotMessage { "SnapshotIdentifier" :: String, "ClusterIdentifier" :: String, "Tags" :: NullOrUndefined (TagList) }
```

<p/>

#### `CreateClusterSnapshotResult`

``` purescript
newtype CreateClusterSnapshotResult
  = CreateClusterSnapshotResult { "Snapshot" :: NullOrUndefined (Snapshot) }
```

#### `CreateClusterSubnetGroupMessage`

``` purescript
newtype CreateClusterSubnetGroupMessage
  = CreateClusterSubnetGroupMessage { "ClusterSubnetGroupName" :: String, "Description" :: String, "SubnetIds" :: SubnetIdentifierList, "Tags" :: NullOrUndefined (TagList) }
```

<p/>

#### `CreateClusterSubnetGroupResult`

``` purescript
newtype CreateClusterSubnetGroupResult
  = CreateClusterSubnetGroupResult { "ClusterSubnetGroup" :: NullOrUndefined (ClusterSubnetGroup) }
```

#### `CreateEventSubscriptionMessage`

``` purescript
newtype CreateEventSubscriptionMessage
  = CreateEventSubscriptionMessage { "SubscriptionName" :: String, "SnsTopicArn" :: String, "SourceType" :: NullOrUndefined (String), "SourceIds" :: NullOrUndefined (SourceIdsList), "EventCategories" :: NullOrUndefined (EventCategoriesList), "Severity" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (BooleanOptional), "Tags" :: NullOrUndefined (TagList) }
```

<p/>

#### `CreateEventSubscriptionResult`

``` purescript
newtype CreateEventSubscriptionResult
  = CreateEventSubscriptionResult { "EventSubscription" :: NullOrUndefined (EventSubscription) }
```

#### `CreateHsmClientCertificateMessage`

``` purescript
newtype CreateHsmClientCertificateMessage
  = CreateHsmClientCertificateMessage { "HsmClientCertificateIdentifier" :: String, "Tags" :: NullOrUndefined (TagList) }
```

<p/>

#### `CreateHsmClientCertificateResult`

``` purescript
newtype CreateHsmClientCertificateResult
  = CreateHsmClientCertificateResult { "HsmClientCertificate" :: NullOrUndefined (HsmClientCertificate) }
```

#### `CreateHsmConfigurationMessage`

``` purescript
newtype CreateHsmConfigurationMessage
  = CreateHsmConfigurationMessage { "HsmConfigurationIdentifier" :: String, "Description" :: String, "HsmIpAddress" :: String, "HsmPartitionName" :: String, "HsmPartitionPassword" :: String, "HsmServerPublicCertificate" :: String, "Tags" :: NullOrUndefined (TagList) }
```

<p/>

#### `CreateHsmConfigurationResult`

``` purescript
newtype CreateHsmConfigurationResult
  = CreateHsmConfigurationResult { "HsmConfiguration" :: NullOrUndefined (HsmConfiguration) }
```

#### `CreateSnapshotCopyGrantMessage`

``` purescript
newtype CreateSnapshotCopyGrantMessage
  = CreateSnapshotCopyGrantMessage { "SnapshotCopyGrantName" :: String, "KmsKeyId" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>The result of the <code>CreateSnapshotCopyGrant</code> action.</p>

#### `CreateSnapshotCopyGrantResult`

``` purescript
newtype CreateSnapshotCopyGrantResult
  = CreateSnapshotCopyGrantResult { "SnapshotCopyGrant" :: NullOrUndefined (SnapshotCopyGrant) }
```

#### `CreateTagsMessage`

``` purescript
newtype CreateTagsMessage
  = CreateTagsMessage { "ResourceName" :: String, "Tags" :: TagList }
```

<p>Contains the output from the <code>CreateTags</code> action. </p>

#### `DbGroupList`

``` purescript
newtype DbGroupList
  = DbGroupList (Array String)
```

#### `DefaultClusterParameters`

``` purescript
newtype DefaultClusterParameters
  = DefaultClusterParameters { "ParameterGroupFamily" :: NullOrUndefined (String), "Marker" :: NullOrUndefined (String), "Parameters" :: NullOrUndefined (ParametersList) }
```

<p>Describes the default cluster parameters for a parameter group family.</p>

#### `DeleteClusterMessage`

``` purescript
newtype DeleteClusterMessage
  = DeleteClusterMessage { "ClusterIdentifier" :: String, "SkipFinalClusterSnapshot" :: NullOrUndefined (Boolean), "FinalClusterSnapshotIdentifier" :: NullOrUndefined (String) }
```

<p/>

#### `DeleteClusterParameterGroupMessage`

``` purescript
newtype DeleteClusterParameterGroupMessage
  = DeleteClusterParameterGroupMessage { "ParameterGroupName" :: String }
```

<p/>

#### `DeleteClusterResult`

``` purescript
newtype DeleteClusterResult
  = DeleteClusterResult { "Cluster" :: NullOrUndefined (Cluster) }
```

#### `DeleteClusterSecurityGroupMessage`

``` purescript
newtype DeleteClusterSecurityGroupMessage
  = DeleteClusterSecurityGroupMessage { "ClusterSecurityGroupName" :: String }
```

<p/>

#### `DeleteClusterSnapshotMessage`

``` purescript
newtype DeleteClusterSnapshotMessage
  = DeleteClusterSnapshotMessage { "SnapshotIdentifier" :: String, "SnapshotClusterIdentifier" :: NullOrUndefined (String) }
```

<p/>

#### `DeleteClusterSnapshotResult`

``` purescript
newtype DeleteClusterSnapshotResult
  = DeleteClusterSnapshotResult { "Snapshot" :: NullOrUndefined (Snapshot) }
```

#### `DeleteClusterSubnetGroupMessage`

``` purescript
newtype DeleteClusterSubnetGroupMessage
  = DeleteClusterSubnetGroupMessage { "ClusterSubnetGroupName" :: String }
```

<p/>

#### `DeleteEventSubscriptionMessage`

``` purescript
newtype DeleteEventSubscriptionMessage
  = DeleteEventSubscriptionMessage { "SubscriptionName" :: String }
```

<p/>

#### `DeleteHsmClientCertificateMessage`

``` purescript
newtype DeleteHsmClientCertificateMessage
  = DeleteHsmClientCertificateMessage { "HsmClientCertificateIdentifier" :: String }
```

<p/>

#### `DeleteHsmConfigurationMessage`

``` purescript
newtype DeleteHsmConfigurationMessage
  = DeleteHsmConfigurationMessage { "HsmConfigurationIdentifier" :: String }
```

<p/>

#### `DeleteSnapshotCopyGrantMessage`

``` purescript
newtype DeleteSnapshotCopyGrantMessage
  = DeleteSnapshotCopyGrantMessage { "SnapshotCopyGrantName" :: String }
```

<p>The result of the <code>DeleteSnapshotCopyGrant</code> action.</p>

#### `DeleteTagsMessage`

``` purescript
newtype DeleteTagsMessage
  = DeleteTagsMessage { "ResourceName" :: String, "TagKeys" :: TagKeyList }
```

<p>Contains the output from the <code>DeleteTags</code> action. </p>

#### `DependentServiceRequestThrottlingFault`

``` purescript
newtype DependentServiceRequestThrottlingFault
  = DependentServiceRequestThrottlingFault {  }
```

<p>The request cannot be completed because a dependent service is throttling requests made by Amazon Redshift on your behalf. Wait and retry the request.</p>

#### `DependentServiceUnavailableFault`

``` purescript
newtype DependentServiceUnavailableFault
  = DependentServiceUnavailableFault {  }
```

<p>Your request cannot be completed because a dependent internal service is temporarily unavailable. Wait 30 to 60 seconds and try again.</p>

#### `DescribeClusterParameterGroupsMessage`

``` purescript
newtype DescribeClusterParameterGroupsMessage
  = DescribeClusterParameterGroupsMessage { "ParameterGroupName" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "TagKeys" :: NullOrUndefined (TagKeyList), "TagValues" :: NullOrUndefined (TagValueList) }
```

<p/>

#### `DescribeClusterParametersMessage`

``` purescript
newtype DescribeClusterParametersMessage
  = DescribeClusterParametersMessage { "ParameterGroupName" :: String, "Source" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeClusterSecurityGroupsMessage`

``` purescript
newtype DescribeClusterSecurityGroupsMessage
  = DescribeClusterSecurityGroupsMessage { "ClusterSecurityGroupName" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "TagKeys" :: NullOrUndefined (TagKeyList), "TagValues" :: NullOrUndefined (TagValueList) }
```

<p/>

#### `DescribeClusterSnapshotsMessage`

``` purescript
newtype DescribeClusterSnapshotsMessage
  = DescribeClusterSnapshotsMessage { "ClusterIdentifier" :: NullOrUndefined (String), "SnapshotIdentifier" :: NullOrUndefined (String), "SnapshotType" :: NullOrUndefined (String), "StartTime" :: NullOrUndefined (TStamp), "EndTime" :: NullOrUndefined (TStamp), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "OwnerAccount" :: NullOrUndefined (String), "TagKeys" :: NullOrUndefined (TagKeyList), "TagValues" :: NullOrUndefined (TagValueList) }
```

<p/>

#### `DescribeClusterSubnetGroupsMessage`

``` purescript
newtype DescribeClusterSubnetGroupsMessage
  = DescribeClusterSubnetGroupsMessage { "ClusterSubnetGroupName" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "TagKeys" :: NullOrUndefined (TagKeyList), "TagValues" :: NullOrUndefined (TagValueList) }
```

<p/>

#### `DescribeClusterVersionsMessage`

``` purescript
newtype DescribeClusterVersionsMessage
  = DescribeClusterVersionsMessage { "ClusterVersion" :: NullOrUndefined (String), "ClusterParameterGroupFamily" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeClustersMessage`

``` purescript
newtype DescribeClustersMessage
  = DescribeClustersMessage { "ClusterIdentifier" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "TagKeys" :: NullOrUndefined (TagKeyList), "TagValues" :: NullOrUndefined (TagValueList) }
```

<p/>

#### `DescribeDefaultClusterParametersMessage`

``` purescript
newtype DescribeDefaultClusterParametersMessage
  = DescribeDefaultClusterParametersMessage { "ParameterGroupFamily" :: String, "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeDefaultClusterParametersResult`

``` purescript
newtype DescribeDefaultClusterParametersResult
  = DescribeDefaultClusterParametersResult { "DefaultClusterParameters" :: NullOrUndefined (DefaultClusterParameters) }
```

#### `DescribeEventCategoriesMessage`

``` purescript
newtype DescribeEventCategoriesMessage
  = DescribeEventCategoriesMessage { "SourceType" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeEventSubscriptionsMessage`

``` purescript
newtype DescribeEventSubscriptionsMessage
  = DescribeEventSubscriptionsMessage { "SubscriptionName" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "TagKeys" :: NullOrUndefined (TagKeyList), "TagValues" :: NullOrUndefined (TagValueList) }
```

<p/>

#### `DescribeEventsMessage`

``` purescript
newtype DescribeEventsMessage
  = DescribeEventsMessage { "SourceIdentifier" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (SourceType), "StartTime" :: NullOrUndefined (TStamp), "EndTime" :: NullOrUndefined (TStamp), "Duration" :: NullOrUndefined (IntegerOptional), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeHsmClientCertificatesMessage`

``` purescript
newtype DescribeHsmClientCertificatesMessage
  = DescribeHsmClientCertificatesMessage { "HsmClientCertificateIdentifier" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "TagKeys" :: NullOrUndefined (TagKeyList), "TagValues" :: NullOrUndefined (TagValueList) }
```

<p/>

#### `DescribeHsmConfigurationsMessage`

``` purescript
newtype DescribeHsmConfigurationsMessage
  = DescribeHsmConfigurationsMessage { "HsmConfigurationIdentifier" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "TagKeys" :: NullOrUndefined (TagKeyList), "TagValues" :: NullOrUndefined (TagValueList) }
```

<p/>

#### `DescribeLoggingStatusMessage`

``` purescript
newtype DescribeLoggingStatusMessage
  = DescribeLoggingStatusMessage { "ClusterIdentifier" :: String }
```

<p/>

#### `DescribeOrderableClusterOptionsMessage`

``` purescript
newtype DescribeOrderableClusterOptionsMessage
  = DescribeOrderableClusterOptionsMessage { "ClusterVersion" :: NullOrUndefined (String), "NodeType" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeReservedNodeOfferingsMessage`

``` purescript
newtype DescribeReservedNodeOfferingsMessage
  = DescribeReservedNodeOfferingsMessage { "ReservedNodeOfferingId" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeReservedNodesMessage`

``` purescript
newtype DescribeReservedNodesMessage
  = DescribeReservedNodesMessage { "ReservedNodeId" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeResizeMessage`

``` purescript
newtype DescribeResizeMessage
  = DescribeResizeMessage { "ClusterIdentifier" :: String }
```

<p/>

#### `DescribeSnapshotCopyGrantsMessage`

``` purescript
newtype DescribeSnapshotCopyGrantsMessage
  = DescribeSnapshotCopyGrantsMessage { "SnapshotCopyGrantName" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "TagKeys" :: NullOrUndefined (TagKeyList), "TagValues" :: NullOrUndefined (TagValueList) }
```

<p>The result of the <code>DescribeSnapshotCopyGrants</code> action.</p>

#### `DescribeTableRestoreStatusMessage`

``` purescript
newtype DescribeTableRestoreStatusMessage
  = DescribeTableRestoreStatusMessage { "ClusterIdentifier" :: NullOrUndefined (String), "TableRestoreRequestId" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeTagsMessage`

``` purescript
newtype DescribeTagsMessage
  = DescribeTagsMessage { "ResourceName" :: NullOrUndefined (String), "ResourceType" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "TagKeys" :: NullOrUndefined (TagKeyList), "TagValues" :: NullOrUndefined (TagValueList) }
```

<p/>

#### `DisableLoggingMessage`

``` purescript
newtype DisableLoggingMessage
  = DisableLoggingMessage { "ClusterIdentifier" :: String }
```

<p/>

#### `DisableSnapshotCopyMessage`

``` purescript
newtype DisableSnapshotCopyMessage
  = DisableSnapshotCopyMessage { "ClusterIdentifier" :: String }
```

<p/>

#### `DisableSnapshotCopyResult`

``` purescript
newtype DisableSnapshotCopyResult
  = DisableSnapshotCopyResult { "Cluster" :: NullOrUndefined (Cluster) }
```

#### `DoubleOptional`

``` purescript
newtype DoubleOptional
  = DoubleOptional Number
```

#### `EC2SecurityGroup`

``` purescript
newtype EC2SecurityGroup
  = EC2SecurityGroup { "Status" :: NullOrUndefined (String), "EC2SecurityGroupName" :: NullOrUndefined (String), "EC2SecurityGroupOwnerId" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes an Amazon EC2 security group.</p>

#### `EC2SecurityGroupList`

``` purescript
newtype EC2SecurityGroupList
  = EC2SecurityGroupList (Array EC2SecurityGroup)
```

#### `ElasticIpStatus`

``` purescript
newtype ElasticIpStatus
  = ElasticIpStatus { "ElasticIp" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String) }
```

<p>Describes the status of the elastic IP (EIP) address.</p>

#### `EnableLoggingMessage`

``` purescript
newtype EnableLoggingMessage
  = EnableLoggingMessage { "ClusterIdentifier" :: String, "BucketName" :: String, "S3KeyPrefix" :: NullOrUndefined (String) }
```

<p/>

#### `EnableSnapshotCopyMessage`

``` purescript
newtype EnableSnapshotCopyMessage
  = EnableSnapshotCopyMessage { "ClusterIdentifier" :: String, "DestinationRegion" :: String, "RetentionPeriod" :: NullOrUndefined (IntegerOptional), "SnapshotCopyGrantName" :: NullOrUndefined (String) }
```

<p/>

#### `EnableSnapshotCopyResult`

``` purescript
newtype EnableSnapshotCopyResult
  = EnableSnapshotCopyResult { "Cluster" :: NullOrUndefined (Cluster) }
```

#### `Endpoint`

``` purescript
newtype Endpoint
  = Endpoint { "Address" :: NullOrUndefined (String), "Port" :: NullOrUndefined (Int) }
```

<p>Describes a connection endpoint.</p>

#### `Event`

``` purescript
newtype Event
  = Event { "SourceIdentifier" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (SourceType), "Message" :: NullOrUndefined (String), "EventCategories" :: NullOrUndefined (EventCategoriesList), "Severity" :: NullOrUndefined (String), "Date" :: NullOrUndefined (TStamp), "EventId" :: NullOrUndefined (String) }
```

<p>Describes an event.</p>

#### `EventCategoriesList`

``` purescript
newtype EventCategoriesList
  = EventCategoriesList (Array String)
```

#### `EventCategoriesMap`

``` purescript
newtype EventCategoriesMap
  = EventCategoriesMap { "SourceType" :: NullOrUndefined (String), "Events" :: NullOrUndefined (EventInfoMapList) }
```

<p>Describes event categories.</p>

#### `EventCategoriesMapList`

``` purescript
newtype EventCategoriesMapList
  = EventCategoriesMapList (Array EventCategoriesMap)
```

#### `EventCategoriesMessage`

``` purescript
newtype EventCategoriesMessage
  = EventCategoriesMessage { "EventCategoriesMapList" :: NullOrUndefined (EventCategoriesMapList) }
```

<p/>

#### `EventInfoMap`

``` purescript
newtype EventInfoMap
  = EventInfoMap { "EventId" :: NullOrUndefined (String), "EventCategories" :: NullOrUndefined (EventCategoriesList), "EventDescription" :: NullOrUndefined (String), "Severity" :: NullOrUndefined (String) }
```

<p>Describes event information.</p>

#### `EventInfoMapList`

``` purescript
newtype EventInfoMapList
  = EventInfoMapList (Array EventInfoMap)
```

#### `EventList`

``` purescript
newtype EventList
  = EventList (Array Event)
```

#### `EventSubscription`

``` purescript
newtype EventSubscription
  = EventSubscription { "CustomerAwsId" :: NullOrUndefined (String), "CustSubscriptionId" :: NullOrUndefined (String), "SnsTopicArn" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "SubscriptionCreationTime" :: NullOrUndefined (TStamp), "SourceType" :: NullOrUndefined (String), "SourceIdsList" :: NullOrUndefined (SourceIdsList), "EventCategoriesList" :: NullOrUndefined (EventCategoriesList), "Severity" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes event subscriptions.</p>

#### `EventSubscriptionQuotaExceededFault`

``` purescript
newtype EventSubscriptionQuotaExceededFault
  = EventSubscriptionQuotaExceededFault {  }
```

<p>The request would exceed the allowed number of event subscriptions for this account. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `EventSubscriptionsList`

``` purescript
newtype EventSubscriptionsList
  = EventSubscriptionsList (Array EventSubscription)
```

#### `EventSubscriptionsMessage`

``` purescript
newtype EventSubscriptionsMessage
  = EventSubscriptionsMessage { "Marker" :: NullOrUndefined (String), "EventSubscriptionsList" :: NullOrUndefined (EventSubscriptionsList) }
```

<p/>

#### `EventsMessage`

``` purescript
newtype EventsMessage
  = EventsMessage { "Marker" :: NullOrUndefined (String), "Events" :: NullOrUndefined (EventList) }
```

<p/>

#### `GetClusterCredentialsMessage`

``` purescript
newtype GetClusterCredentialsMessage
  = GetClusterCredentialsMessage { "DbUser" :: String, "DbName" :: NullOrUndefined (String), "ClusterIdentifier" :: String, "DurationSeconds" :: NullOrUndefined (IntegerOptional), "AutoCreate" :: NullOrUndefined (BooleanOptional), "DbGroups" :: NullOrUndefined (DbGroupList) }
```

<p>The request parameters to get cluster credentials.</p>

#### `HsmClientCertificate`

``` purescript
newtype HsmClientCertificate
  = HsmClientCertificate { "HsmClientCertificateIdentifier" :: NullOrUndefined (String), "HsmClientCertificatePublicKey" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>Returns information about an HSM client certificate. The certificate is stored in a secure Hardware Storage Module (HSM), and used by the Amazon Redshift cluster to encrypt data files.</p>

#### `HsmClientCertificateAlreadyExistsFault`

``` purescript
newtype HsmClientCertificateAlreadyExistsFault
  = HsmClientCertificateAlreadyExistsFault {  }
```

<p>There is already an existing Amazon Redshift HSM client certificate with the specified identifier.</p>

#### `HsmClientCertificateList`

``` purescript
newtype HsmClientCertificateList
  = HsmClientCertificateList (Array HsmClientCertificate)
```

#### `HsmClientCertificateMessage`

``` purescript
newtype HsmClientCertificateMessage
  = HsmClientCertificateMessage { "Marker" :: NullOrUndefined (String), "HsmClientCertificates" :: NullOrUndefined (HsmClientCertificateList) }
```

<p/>

#### `HsmClientCertificateNotFoundFault`

``` purescript
newtype HsmClientCertificateNotFoundFault
  = HsmClientCertificateNotFoundFault {  }
```

<p>There is no Amazon Redshift HSM client certificate with the specified identifier.</p>

#### `HsmClientCertificateQuotaExceededFault`

``` purescript
newtype HsmClientCertificateQuotaExceededFault
  = HsmClientCertificateQuotaExceededFault {  }
```

<p>The quota for HSM client certificates has been reached. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `HsmConfiguration`

``` purescript
newtype HsmConfiguration
  = HsmConfiguration { "HsmConfigurationIdentifier" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HsmIpAddress" :: NullOrUndefined (String), "HsmPartitionName" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>Returns information about an HSM configuration, which is an object that describes to Amazon Redshift clusters the information they require to connect to an HSM where they can store database encryption keys.</p>

#### `HsmConfigurationAlreadyExistsFault`

``` purescript
newtype HsmConfigurationAlreadyExistsFault
  = HsmConfigurationAlreadyExistsFault {  }
```

<p>There is already an existing Amazon Redshift HSM configuration with the specified identifier.</p>

#### `HsmConfigurationList`

``` purescript
newtype HsmConfigurationList
  = HsmConfigurationList (Array HsmConfiguration)
```

#### `HsmConfigurationMessage`

``` purescript
newtype HsmConfigurationMessage
  = HsmConfigurationMessage { "Marker" :: NullOrUndefined (String), "HsmConfigurations" :: NullOrUndefined (HsmConfigurationList) }
```

<p/>

#### `HsmConfigurationNotFoundFault`

``` purescript
newtype HsmConfigurationNotFoundFault
  = HsmConfigurationNotFoundFault {  }
```

<p>There is no Amazon Redshift HSM configuration with the specified identifier.</p>

#### `HsmConfigurationQuotaExceededFault`

``` purescript
newtype HsmConfigurationQuotaExceededFault
  = HsmConfigurationQuotaExceededFault {  }
```

<p>The quota for HSM configurations has been reached. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `HsmStatus`

``` purescript
newtype HsmStatus
  = HsmStatus { "HsmClientCertificateIdentifier" :: NullOrUndefined (String), "HsmConfigurationIdentifier" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String) }
```

<p>Describes the status of changes to HSM settings.</p>

#### `IPRange`

``` purescript
newtype IPRange
  = IPRange { "Status" :: NullOrUndefined (String), "CIDRIP" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes an IP range used in a security group.</p>

#### `IPRangeList`

``` purescript
newtype IPRangeList
  = IPRangeList (Array IPRange)
```

#### `IamRoleArnList`

``` purescript
newtype IamRoleArnList
  = IamRoleArnList (Array String)
```

#### `ImportTablesCompleted`

``` purescript
newtype ImportTablesCompleted
  = ImportTablesCompleted (Array String)
```

#### `ImportTablesInProgress`

``` purescript
newtype ImportTablesInProgress
  = ImportTablesInProgress (Array String)
```

#### `ImportTablesNotStarted`

``` purescript
newtype ImportTablesNotStarted
  = ImportTablesNotStarted (Array String)
```

#### `InProgressTableRestoreQuotaExceededFault`

``` purescript
newtype InProgressTableRestoreQuotaExceededFault
  = InProgressTableRestoreQuotaExceededFault {  }
```

<p>You have exceeded the allowed number of table restore requests. Wait for your current table restore requests to complete before making a new request.</p>

#### `IncompatibleOrderableOptions`

``` purescript
newtype IncompatibleOrderableOptions
  = IncompatibleOrderableOptions {  }
```

<p>The specified options are incompatible.</p>

#### `InsufficientClusterCapacityFault`

``` purescript
newtype InsufficientClusterCapacityFault
  = InsufficientClusterCapacityFault {  }
```

<p>The number of nodes specified exceeds the allotted capacity of the cluster.</p>

#### `InsufficientS3BucketPolicyFault`

``` purescript
newtype InsufficientS3BucketPolicyFault
  = InsufficientS3BucketPolicyFault {  }
```

<p>The cluster does not have read bucket or put object permissions on the S3 bucket specified when enabling logging.</p>

#### `IntegerOptional`

``` purescript
newtype IntegerOptional
  = IntegerOptional Int
```

#### `InvalidClusterParameterGroupStateFault`

``` purescript
newtype InvalidClusterParameterGroupStateFault
  = InvalidClusterParameterGroupStateFault {  }
```

<p>The cluster parameter group action can not be completed because another task is in progress that involves the parameter group. Wait a few moments and try the operation again.</p>

#### `InvalidClusterSecurityGroupStateFault`

``` purescript
newtype InvalidClusterSecurityGroupStateFault
  = InvalidClusterSecurityGroupStateFault {  }
```

<p>The state of the cluster security group is not <code>available</code>. </p>

#### `InvalidClusterSnapshotStateFault`

``` purescript
newtype InvalidClusterSnapshotStateFault
  = InvalidClusterSnapshotStateFault {  }
```

<p>The specified cluster snapshot is not in the <code>available</code> state, or other accounts are authorized to access the snapshot. </p>

#### `InvalidClusterStateFault`

``` purescript
newtype InvalidClusterStateFault
  = InvalidClusterStateFault {  }
```

<p>The specified cluster is not in the <code>available</code> state. </p>

#### `InvalidClusterSubnetGroupStateFault`

``` purescript
newtype InvalidClusterSubnetGroupStateFault
  = InvalidClusterSubnetGroupStateFault {  }
```

<p>The cluster subnet group cannot be deleted because it is in use.</p>

#### `InvalidClusterSubnetStateFault`

``` purescript
newtype InvalidClusterSubnetStateFault
  = InvalidClusterSubnetStateFault {  }
```

<p>The state of the subnet is invalid.</p>

#### `InvalidElasticIpFault`

``` purescript
newtype InvalidElasticIpFault
  = InvalidElasticIpFault {  }
```

<p>The Elastic IP (EIP) is invalid or cannot be found.</p>

#### `InvalidHsmClientCertificateStateFault`

``` purescript
newtype InvalidHsmClientCertificateStateFault
  = InvalidHsmClientCertificateStateFault {  }
```

<p>The specified HSM client certificate is not in the <code>available</code> state, or it is still in use by one or more Amazon Redshift clusters.</p>

#### `InvalidHsmConfigurationStateFault`

``` purescript
newtype InvalidHsmConfigurationStateFault
  = InvalidHsmConfigurationStateFault {  }
```

<p>The specified HSM configuration is not in the <code>available</code> state, or it is still in use by one or more Amazon Redshift clusters.</p>

#### `InvalidRestoreFault`

``` purescript
newtype InvalidRestoreFault
  = InvalidRestoreFault {  }
```

<p>The restore is invalid.</p>

#### `InvalidS3BucketNameFault`

``` purescript
newtype InvalidS3BucketNameFault
  = InvalidS3BucketNameFault {  }
```

<p>The S3 bucket name is invalid. For more information about naming rules, go to <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html">Bucket Restrictions and Limitations</a> in the Amazon Simple Storage Service (S3) Developer Guide.</p>

#### `InvalidS3KeyPrefixFault`

``` purescript
newtype InvalidS3KeyPrefixFault
  = InvalidS3KeyPrefixFault {  }
```

<p>The string specified for the logging S3 key prefix does not comply with the documented constraints.</p>

#### `InvalidSnapshotCopyGrantStateFault`

``` purescript
newtype InvalidSnapshotCopyGrantStateFault
  = InvalidSnapshotCopyGrantStateFault {  }
```

<p>The snapshot copy grant can't be deleted because it is used by one or more clusters.</p>

#### `InvalidSubnet`

``` purescript
newtype InvalidSubnet
  = InvalidSubnet {  }
```

<p>The requested subnet is not valid, or not all of the subnets are in the same VPC.</p>

#### `InvalidSubscriptionStateFault`

``` purescript
newtype InvalidSubscriptionStateFault
  = InvalidSubscriptionStateFault {  }
```

<p>The subscription request is invalid because it is a duplicate request. This subscription request is already in progress.</p>

#### `InvalidTableRestoreArgumentFault`

``` purescript
newtype InvalidTableRestoreArgumentFault
  = InvalidTableRestoreArgumentFault {  }
```

<p>The value specified for the <code>sourceDatabaseName</code>, <code>sourceSchemaName</code>, or <code>sourceTableName</code> parameter, or a combination of these, doesn't exist in the snapshot.</p>

#### `InvalidTagFault`

``` purescript
newtype InvalidTagFault
  = InvalidTagFault {  }
```

<p>The tag is invalid.</p>

#### `InvalidVPCNetworkStateFault`

``` purescript
newtype InvalidVPCNetworkStateFault
  = InvalidVPCNetworkStateFault {  }
```

<p>The cluster subnet group does not cover all Availability Zones.</p>

#### `LimitExceededFault`

``` purescript
newtype LimitExceededFault
  = LimitExceededFault {  }
```

<p>The encryption key has exceeded its grant limit in AWS KMS.</p>

#### `LoggingStatus`

``` purescript
newtype LoggingStatus
  = LoggingStatus { "LoggingEnabled" :: NullOrUndefined (Boolean), "BucketName" :: NullOrUndefined (String), "S3KeyPrefix" :: NullOrUndefined (String), "LastSuccessfulDeliveryTime" :: NullOrUndefined (TStamp), "LastFailureTime" :: NullOrUndefined (TStamp), "LastFailureMessage" :: NullOrUndefined (String) }
```

<p>Describes the status of logging for a cluster.</p>

#### `LongOptional`

``` purescript
newtype LongOptional
  = LongOptional Number
```

#### `ModifyClusterIamRolesMessage`

``` purescript
newtype ModifyClusterIamRolesMessage
  = ModifyClusterIamRolesMessage { "ClusterIdentifier" :: String, "AddIamRoles" :: NullOrUndefined (IamRoleArnList), "RemoveIamRoles" :: NullOrUndefined (IamRoleArnList) }
```

<p/>

#### `ModifyClusterIamRolesResult`

``` purescript
newtype ModifyClusterIamRolesResult
  = ModifyClusterIamRolesResult { "Cluster" :: NullOrUndefined (Cluster) }
```

#### `ModifyClusterMessage`

``` purescript
newtype ModifyClusterMessage
  = ModifyClusterMessage { "ClusterIdentifier" :: String, "ClusterType" :: NullOrUndefined (String), "NodeType" :: NullOrUndefined (String), "NumberOfNodes" :: NullOrUndefined (IntegerOptional), "ClusterSecurityGroups" :: NullOrUndefined (ClusterSecurityGroupNameList), "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList), "MasterUserPassword" :: NullOrUndefined (String), "ClusterParameterGroupName" :: NullOrUndefined (String), "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined (IntegerOptional), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "ClusterVersion" :: NullOrUndefined (String), "AllowVersionUpgrade" :: NullOrUndefined (BooleanOptional), "HsmClientCertificateIdentifier" :: NullOrUndefined (String), "HsmConfigurationIdentifier" :: NullOrUndefined (String), "NewClusterIdentifier" :: NullOrUndefined (String), "PubliclyAccessible" :: NullOrUndefined (BooleanOptional), "ElasticIp" :: NullOrUndefined (String), "EnhancedVpcRouting" :: NullOrUndefined (BooleanOptional) }
```

<p/>

#### `ModifyClusterParameterGroupMessage`

``` purescript
newtype ModifyClusterParameterGroupMessage
  = ModifyClusterParameterGroupMessage { "ParameterGroupName" :: String, "Parameters" :: ParametersList }
```

<p/>

#### `ModifyClusterResult`

``` purescript
newtype ModifyClusterResult
  = ModifyClusterResult { "Cluster" :: NullOrUndefined (Cluster) }
```

#### `ModifyClusterSubnetGroupMessage`

``` purescript
newtype ModifyClusterSubnetGroupMessage
  = ModifyClusterSubnetGroupMessage { "ClusterSubnetGroupName" :: String, "Description" :: NullOrUndefined (String), "SubnetIds" :: SubnetIdentifierList }
```

<p/>

#### `ModifyClusterSubnetGroupResult`

``` purescript
newtype ModifyClusterSubnetGroupResult
  = ModifyClusterSubnetGroupResult { "ClusterSubnetGroup" :: NullOrUndefined (ClusterSubnetGroup) }
```

#### `ModifyEventSubscriptionMessage`

``` purescript
newtype ModifyEventSubscriptionMessage
  = ModifyEventSubscriptionMessage { "SubscriptionName" :: String, "SnsTopicArn" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (String), "SourceIds" :: NullOrUndefined (SourceIdsList), "EventCategories" :: NullOrUndefined (EventCategoriesList), "Severity" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (BooleanOptional) }
```

<p/>

#### `ModifyEventSubscriptionResult`

``` purescript
newtype ModifyEventSubscriptionResult
  = ModifyEventSubscriptionResult { "EventSubscription" :: NullOrUndefined (EventSubscription) }
```

#### `ModifySnapshotCopyRetentionPeriodMessage`

``` purescript
newtype ModifySnapshotCopyRetentionPeriodMessage
  = ModifySnapshotCopyRetentionPeriodMessage { "ClusterIdentifier" :: String, "RetentionPeriod" :: Int }
```

<p/>

#### `ModifySnapshotCopyRetentionPeriodResult`

``` purescript
newtype ModifySnapshotCopyRetentionPeriodResult
  = ModifySnapshotCopyRetentionPeriodResult { "Cluster" :: NullOrUndefined (Cluster) }
```

#### `NumberOfNodesPerClusterLimitExceededFault`

``` purescript
newtype NumberOfNodesPerClusterLimitExceededFault
  = NumberOfNodesPerClusterLimitExceededFault {  }
```

<p>The operation would exceed the number of nodes allowed for a cluster.</p>

#### `NumberOfNodesQuotaExceededFault`

``` purescript
newtype NumberOfNodesQuotaExceededFault
  = NumberOfNodesQuotaExceededFault {  }
```

<p>The operation would exceed the number of nodes allotted to the account. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `OrderableClusterOption`

``` purescript
newtype OrderableClusterOption
  = OrderableClusterOption { "ClusterVersion" :: NullOrUndefined (String), "ClusterType" :: NullOrUndefined (String), "NodeType" :: NullOrUndefined (String), "AvailabilityZones" :: NullOrUndefined (AvailabilityZoneList) }
```

<p>Describes an orderable cluster option.</p>

#### `OrderableClusterOptionsList`

``` purescript
newtype OrderableClusterOptionsList
  = OrderableClusterOptionsList (Array OrderableClusterOption)
```

#### `OrderableClusterOptionsMessage`

``` purescript
newtype OrderableClusterOptionsMessage
  = OrderableClusterOptionsMessage { "OrderableClusterOptions" :: NullOrUndefined (OrderableClusterOptionsList), "Marker" :: NullOrUndefined (String) }
```

<p>Contains the output from the <a>DescribeOrderableClusterOptions</a> action. </p>

#### `Parameter`

``` purescript
newtype Parameter
  = Parameter { "ParameterName" :: NullOrUndefined (String), "ParameterValue" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Source" :: NullOrUndefined (String), "DataType" :: NullOrUndefined (String), "AllowedValues" :: NullOrUndefined (String), "ApplyType" :: NullOrUndefined (ParameterApplyType), "IsModifiable" :: NullOrUndefined (Boolean), "MinimumEngineVersion" :: NullOrUndefined (String) }
```

<p>Describes a parameter in a cluster parameter group.</p>

#### `ParameterApplyType`

``` purescript
newtype ParameterApplyType
  = ParameterApplyType String
```

#### `ParameterGroupList`

``` purescript
newtype ParameterGroupList
  = ParameterGroupList (Array ClusterParameterGroup)
```

#### `ParametersList`

``` purescript
newtype ParametersList
  = ParametersList (Array Parameter)
```

#### `PendingModifiedValues`

``` purescript
newtype PendingModifiedValues
  = PendingModifiedValues { "MasterUserPassword" :: NullOrUndefined (String), "NodeType" :: NullOrUndefined (String), "NumberOfNodes" :: NullOrUndefined (IntegerOptional), "ClusterType" :: NullOrUndefined (String), "ClusterVersion" :: NullOrUndefined (String), "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined (IntegerOptional), "ClusterIdentifier" :: NullOrUndefined (String), "PubliclyAccessible" :: NullOrUndefined (BooleanOptional), "EnhancedVpcRouting" :: NullOrUndefined (BooleanOptional) }
```

<p>Describes cluster attributes that are in a pending state. A change to one or more the attributes was requested and is in progress or will be applied.</p>

#### `PurchaseReservedNodeOfferingMessage`

``` purescript
newtype PurchaseReservedNodeOfferingMessage
  = PurchaseReservedNodeOfferingMessage { "ReservedNodeOfferingId" :: String, "NodeCount" :: NullOrUndefined (IntegerOptional) }
```

<p/>

#### `PurchaseReservedNodeOfferingResult`

``` purescript
newtype PurchaseReservedNodeOfferingResult
  = PurchaseReservedNodeOfferingResult { "ReservedNode" :: NullOrUndefined (ReservedNode) }
```

#### `RebootClusterMessage`

``` purescript
newtype RebootClusterMessage
  = RebootClusterMessage { "ClusterIdentifier" :: String }
```

<p/>

#### `RebootClusterResult`

``` purescript
newtype RebootClusterResult
  = RebootClusterResult { "Cluster" :: NullOrUndefined (Cluster) }
```

#### `RecurringCharge`

``` purescript
newtype RecurringCharge
  = RecurringCharge { "RecurringChargeAmount" :: NullOrUndefined (Number), "RecurringChargeFrequency" :: NullOrUndefined (String) }
```

<p>Describes a recurring charge.</p>

#### `RecurringChargeList`

``` purescript
newtype RecurringChargeList
  = RecurringChargeList (Array RecurringCharge)
```

#### `ReservedNode`

``` purescript
newtype ReservedNode
  = ReservedNode { "ReservedNodeId" :: NullOrUndefined (String), "ReservedNodeOfferingId" :: NullOrUndefined (String), "NodeType" :: NullOrUndefined (String), "StartTime" :: NullOrUndefined (TStamp), "Duration" :: NullOrUndefined (Int), "FixedPrice" :: NullOrUndefined (Number), "UsagePrice" :: NullOrUndefined (Number), "CurrencyCode" :: NullOrUndefined (String), "NodeCount" :: NullOrUndefined (Int), "State" :: NullOrUndefined (String), "OfferingType" :: NullOrUndefined (String), "RecurringCharges" :: NullOrUndefined (RecurringChargeList) }
```

<p>Describes a reserved node. You can call the <a>DescribeReservedNodeOfferings</a> API to obtain the available reserved node offerings. </p>

#### `ReservedNodeAlreadyExistsFault`

``` purescript
newtype ReservedNodeAlreadyExistsFault
  = ReservedNodeAlreadyExistsFault {  }
```

<p>User already has a reservation with the given identifier.</p>

#### `ReservedNodeList`

``` purescript
newtype ReservedNodeList
  = ReservedNodeList (Array ReservedNode)
```

#### `ReservedNodeNotFoundFault`

``` purescript
newtype ReservedNodeNotFoundFault
  = ReservedNodeNotFoundFault {  }
```

<p>The specified reserved compute node not found.</p>

#### `ReservedNodeOffering`

``` purescript
newtype ReservedNodeOffering
  = ReservedNodeOffering { "ReservedNodeOfferingId" :: NullOrUndefined (String), "NodeType" :: NullOrUndefined (String), "Duration" :: NullOrUndefined (Int), "FixedPrice" :: NullOrUndefined (Number), "UsagePrice" :: NullOrUndefined (Number), "CurrencyCode" :: NullOrUndefined (String), "OfferingType" :: NullOrUndefined (String), "RecurringCharges" :: NullOrUndefined (RecurringChargeList) }
```

<p>Describes a reserved node offering.</p>

#### `ReservedNodeOfferingList`

``` purescript
newtype ReservedNodeOfferingList
  = ReservedNodeOfferingList (Array ReservedNodeOffering)
```

#### `ReservedNodeOfferingNotFoundFault`

``` purescript
newtype ReservedNodeOfferingNotFoundFault
  = ReservedNodeOfferingNotFoundFault {  }
```

<p>Specified offering does not exist.</p>

#### `ReservedNodeOfferingsMessage`

``` purescript
newtype ReservedNodeOfferingsMessage
  = ReservedNodeOfferingsMessage { "Marker" :: NullOrUndefined (String), "ReservedNodeOfferings" :: NullOrUndefined (ReservedNodeOfferingList) }
```

<p/>

#### `ReservedNodeQuotaExceededFault`

``` purescript
newtype ReservedNodeQuotaExceededFault
  = ReservedNodeQuotaExceededFault {  }
```

<p>Request would exceed the user's compute node quota. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `ReservedNodesMessage`

``` purescript
newtype ReservedNodesMessage
  = ReservedNodesMessage { "Marker" :: NullOrUndefined (String), "ReservedNodes" :: NullOrUndefined (ReservedNodeList) }
```

<p/>

#### `ResetClusterParameterGroupMessage`

``` purescript
newtype ResetClusterParameterGroupMessage
  = ResetClusterParameterGroupMessage { "ParameterGroupName" :: String, "ResetAllParameters" :: NullOrUndefined (Boolean), "Parameters" :: NullOrUndefined (ParametersList) }
```

<p/>

#### `ResizeNotFoundFault`

``` purescript
newtype ResizeNotFoundFault
  = ResizeNotFoundFault {  }
```

<p>A resize operation for the specified cluster is not found.</p>

#### `ResizeProgressMessage`

``` purescript
newtype ResizeProgressMessage
  = ResizeProgressMessage { "TargetNodeType" :: NullOrUndefined (String), "TargetNumberOfNodes" :: NullOrUndefined (IntegerOptional), "TargetClusterType" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "ImportTablesCompleted" :: NullOrUndefined (ImportTablesCompleted), "ImportTablesInProgress" :: NullOrUndefined (ImportTablesInProgress), "ImportTablesNotStarted" :: NullOrUndefined (ImportTablesNotStarted), "AvgResizeRateInMegaBytesPerSecond" :: NullOrUndefined (DoubleOptional), "TotalResizeDataInMegaBytes" :: NullOrUndefined (LongOptional), "ProgressInMegaBytes" :: NullOrUndefined (LongOptional), "ElapsedTimeInSeconds" :: NullOrUndefined (LongOptional), "EstimatedTimeToCompletionInSeconds" :: NullOrUndefined (LongOptional) }
```

<p>Describes the result of a cluster resize operation.</p>

#### `ResourceNotFoundFault`

``` purescript
newtype ResourceNotFoundFault
  = ResourceNotFoundFault {  }
```

<p>The resource could not be found.</p>

#### `RestorableNodeTypeList`

``` purescript
newtype RestorableNodeTypeList
  = RestorableNodeTypeList (Array String)
```

#### `RestoreFromClusterSnapshotMessage`

``` purescript
newtype RestoreFromClusterSnapshotMessage
  = RestoreFromClusterSnapshotMessage { "ClusterIdentifier" :: String, "SnapshotIdentifier" :: String, "SnapshotClusterIdentifier" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "AvailabilityZone" :: NullOrUndefined (String), "AllowVersionUpgrade" :: NullOrUndefined (BooleanOptional), "ClusterSubnetGroupName" :: NullOrUndefined (String), "PubliclyAccessible" :: NullOrUndefined (BooleanOptional), "OwnerAccount" :: NullOrUndefined (String), "HsmClientCertificateIdentifier" :: NullOrUndefined (String), "HsmConfigurationIdentifier" :: NullOrUndefined (String), "ElasticIp" :: NullOrUndefined (String), "ClusterParameterGroupName" :: NullOrUndefined (String), "ClusterSecurityGroups" :: NullOrUndefined (ClusterSecurityGroupNameList), "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined (IntegerOptional), "KmsKeyId" :: NullOrUndefined (String), "NodeType" :: NullOrUndefined (String), "EnhancedVpcRouting" :: NullOrUndefined (BooleanOptional), "AdditionalInfo" :: NullOrUndefined (String), "IamRoles" :: NullOrUndefined (IamRoleArnList) }
```

<p/>

#### `RestoreFromClusterSnapshotResult`

``` purescript
newtype RestoreFromClusterSnapshotResult
  = RestoreFromClusterSnapshotResult { "Cluster" :: NullOrUndefined (Cluster) }
```

#### `RestoreStatus`

``` purescript
newtype RestoreStatus
  = RestoreStatus { "Status" :: NullOrUndefined (String), "CurrentRestoreRateInMegaBytesPerSecond" :: NullOrUndefined (Number), "SnapshotSizeInMegaBytes" :: NullOrUndefined (Number), "ProgressInMegaBytes" :: NullOrUndefined (Number), "ElapsedTimeInSeconds" :: NullOrUndefined (Number), "EstimatedTimeToCompletionInSeconds" :: NullOrUndefined (Number) }
```

<p>Describes the status of a cluster restore action. Returns null if the cluster was not created by restoring a snapshot.</p>

#### `RestoreTableFromClusterSnapshotMessage`

``` purescript
newtype RestoreTableFromClusterSnapshotMessage
  = RestoreTableFromClusterSnapshotMessage { "ClusterIdentifier" :: String, "SnapshotIdentifier" :: String, "SourceDatabaseName" :: String, "SourceSchemaName" :: NullOrUndefined (String), "SourceTableName" :: String, "TargetDatabaseName" :: NullOrUndefined (String), "TargetSchemaName" :: NullOrUndefined (String), "NewTableName" :: String }
```

<p/>

#### `RestoreTableFromClusterSnapshotResult`

``` purescript
newtype RestoreTableFromClusterSnapshotResult
  = RestoreTableFromClusterSnapshotResult { "TableRestoreStatus" :: NullOrUndefined (TableRestoreStatus) }
```

#### `RevokeClusterSecurityGroupIngressMessage`

``` purescript
newtype RevokeClusterSecurityGroupIngressMessage
  = RevokeClusterSecurityGroupIngressMessage { "ClusterSecurityGroupName" :: String, "CIDRIP" :: NullOrUndefined (String), "EC2SecurityGroupName" :: NullOrUndefined (String), "EC2SecurityGroupOwnerId" :: NullOrUndefined (String) }
```

<p/>

#### `RevokeClusterSecurityGroupIngressResult`

``` purescript
newtype RevokeClusterSecurityGroupIngressResult
  = RevokeClusterSecurityGroupIngressResult { "ClusterSecurityGroup" :: NullOrUndefined (ClusterSecurityGroup) }
```

#### `RevokeSnapshotAccessMessage`

``` purescript
newtype RevokeSnapshotAccessMessage
  = RevokeSnapshotAccessMessage { "SnapshotIdentifier" :: String, "SnapshotClusterIdentifier" :: NullOrUndefined (String), "AccountWithRestoreAccess" :: String }
```

<p/>

#### `RevokeSnapshotAccessResult`

``` purescript
newtype RevokeSnapshotAccessResult
  = RevokeSnapshotAccessResult { "Snapshot" :: NullOrUndefined (Snapshot) }
```

#### `RotateEncryptionKeyMessage`

``` purescript
newtype RotateEncryptionKeyMessage
  = RotateEncryptionKeyMessage { "ClusterIdentifier" :: String }
```

<p/>

#### `RotateEncryptionKeyResult`

``` purescript
newtype RotateEncryptionKeyResult
  = RotateEncryptionKeyResult { "Cluster" :: NullOrUndefined (Cluster) }
```

#### `SNSInvalidTopicFault`

``` purescript
newtype SNSInvalidTopicFault
  = SNSInvalidTopicFault {  }
```

<p>Amazon SNS has responded that there is a problem with the specified Amazon SNS topic.</p>

#### `SNSNoAuthorizationFault`

``` purescript
newtype SNSNoAuthorizationFault
  = SNSNoAuthorizationFault {  }
```

<p>You do not have permission to publish to the specified Amazon SNS topic.</p>

#### `SNSTopicArnNotFoundFault`

``` purescript
newtype SNSTopicArnNotFoundFault
  = SNSTopicArnNotFoundFault {  }
```

<p>An Amazon SNS topic with the specified Amazon Resource Name (ARN) does not exist.</p>

#### `SensitiveString`

``` purescript
newtype SensitiveString
  = SensitiveString String
```

#### `Snapshot`

``` purescript
newtype Snapshot
  = Snapshot { "SnapshotIdentifier" :: NullOrUndefined (String), "ClusterIdentifier" :: NullOrUndefined (String), "SnapshotCreateTime" :: NullOrUndefined (TStamp), "Status" :: NullOrUndefined (String), "Port" :: NullOrUndefined (Int), "AvailabilityZone" :: NullOrUndefined (String), "ClusterCreateTime" :: NullOrUndefined (TStamp), "MasterUsername" :: NullOrUndefined (String), "ClusterVersion" :: NullOrUndefined (String), "SnapshotType" :: NullOrUndefined (String), "NodeType" :: NullOrUndefined (String), "NumberOfNodes" :: NullOrUndefined (Int), "DBName" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "Encrypted" :: NullOrUndefined (Boolean), "KmsKeyId" :: NullOrUndefined (String), "EncryptedWithHSM" :: NullOrUndefined (Boolean), "AccountsWithRestoreAccess" :: NullOrUndefined (AccountsWithRestoreAccessList), "OwnerAccount" :: NullOrUndefined (String), "TotalBackupSizeInMegaBytes" :: NullOrUndefined (Number), "ActualIncrementalBackupSizeInMegaBytes" :: NullOrUndefined (Number), "BackupProgressInMegaBytes" :: NullOrUndefined (Number), "CurrentBackupRateInMegaBytesPerSecond" :: NullOrUndefined (Number), "EstimatedSecondsToCompletion" :: NullOrUndefined (Number), "ElapsedTimeInSeconds" :: NullOrUndefined (Number), "SourceRegion" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList), "RestorableNodeTypes" :: NullOrUndefined (RestorableNodeTypeList), "EnhancedVpcRouting" :: NullOrUndefined (Boolean) }
```

<p>Describes a snapshot.</p>

#### `SnapshotCopyAlreadyDisabledFault`

``` purescript
newtype SnapshotCopyAlreadyDisabledFault
  = SnapshotCopyAlreadyDisabledFault {  }
```

<p>The cluster already has cross-region snapshot copy disabled.</p>

#### `SnapshotCopyAlreadyEnabledFault`

``` purescript
newtype SnapshotCopyAlreadyEnabledFault
  = SnapshotCopyAlreadyEnabledFault {  }
```

<p>The cluster already has cross-region snapshot copy enabled.</p>

#### `SnapshotCopyDisabledFault`

``` purescript
newtype SnapshotCopyDisabledFault
  = SnapshotCopyDisabledFault {  }
```

<p>Cross-region snapshot copy was temporarily disabled. Try your request again.</p>

#### `SnapshotCopyGrant`

``` purescript
newtype SnapshotCopyGrant
  = SnapshotCopyGrant { "SnapshotCopyGrantName" :: NullOrUndefined (String), "KmsKeyId" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>The snapshot copy grant that grants Amazon Redshift permission to encrypt copied snapshots with the specified customer master key (CMK) from AWS KMS in the destination region.</p> <p> For more information about managing snapshot copy grants, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html">Amazon Redshift Database Encryption</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>

#### `SnapshotCopyGrantAlreadyExistsFault`

``` purescript
newtype SnapshotCopyGrantAlreadyExistsFault
  = SnapshotCopyGrantAlreadyExistsFault {  }
```

<p>The snapshot copy grant can't be created because a grant with the same name already exists.</p>

#### `SnapshotCopyGrantList`

``` purescript
newtype SnapshotCopyGrantList
  = SnapshotCopyGrantList (Array SnapshotCopyGrant)
```

#### `SnapshotCopyGrantMessage`

``` purescript
newtype SnapshotCopyGrantMessage
  = SnapshotCopyGrantMessage { "Marker" :: NullOrUndefined (String), "SnapshotCopyGrants" :: NullOrUndefined (SnapshotCopyGrantList) }
```

<p/>

#### `SnapshotCopyGrantNotFoundFault`

``` purescript
newtype SnapshotCopyGrantNotFoundFault
  = SnapshotCopyGrantNotFoundFault {  }
```

<p>The specified snapshot copy grant can't be found. Make sure that the name is typed correctly and that the grant exists in the destination region.</p>

#### `SnapshotCopyGrantQuotaExceededFault`

``` purescript
newtype SnapshotCopyGrantQuotaExceededFault
  = SnapshotCopyGrantQuotaExceededFault {  }
```

<p>The AWS account has exceeded the maximum number of snapshot copy grants in this region.</p>

#### `SnapshotList`

``` purescript
newtype SnapshotList
  = SnapshotList (Array Snapshot)
```

#### `SnapshotMessage`

``` purescript
newtype SnapshotMessage
  = SnapshotMessage { "Marker" :: NullOrUndefined (String), "Snapshots" :: NullOrUndefined (SnapshotList) }
```

<p>Contains the output from the <a>DescribeClusterSnapshots</a> action. </p>

#### `SourceIdsList`

``` purescript
newtype SourceIdsList
  = SourceIdsList (Array String)
```

#### `SourceNotFoundFault`

``` purescript
newtype SourceNotFoundFault
  = SourceNotFoundFault {  }
```

<p>The specified Amazon Redshift event source could not be found.</p>

#### `SourceType`

``` purescript
newtype SourceType
  = SourceType String
```

#### `Subnet`

``` purescript
newtype Subnet
  = Subnet { "SubnetIdentifier" :: NullOrUndefined (String), "SubnetAvailabilityZone" :: NullOrUndefined (AvailabilityZone), "SubnetStatus" :: NullOrUndefined (String) }
```

<p>Describes a subnet.</p>

#### `SubnetAlreadyInUse`

``` purescript
newtype SubnetAlreadyInUse
  = SubnetAlreadyInUse {  }
```

<p>A specified subnet is already in use by another cluster.</p>

#### `SubnetIdentifierList`

``` purescript
newtype SubnetIdentifierList
  = SubnetIdentifierList (Array String)
```

#### `SubnetList`

``` purescript
newtype SubnetList
  = SubnetList (Array Subnet)
```

#### `SubscriptionAlreadyExistFault`

``` purescript
newtype SubscriptionAlreadyExistFault
  = SubscriptionAlreadyExistFault {  }
```

<p>There is already an existing event notification subscription with the specified name.</p>

#### `SubscriptionCategoryNotFoundFault`

``` purescript
newtype SubscriptionCategoryNotFoundFault
  = SubscriptionCategoryNotFoundFault {  }
```

<p>The value specified for the event category was not one of the allowed values, or it specified a category that does not apply to the specified source type. The allowed values are Configuration, Management, Monitoring, and Security.</p>

#### `SubscriptionEventIdNotFoundFault`

``` purescript
newtype SubscriptionEventIdNotFoundFault
  = SubscriptionEventIdNotFoundFault {  }
```

<p>An Amazon Redshift event with the specified event ID does not exist.</p>

#### `SubscriptionNotFoundFault`

``` purescript
newtype SubscriptionNotFoundFault
  = SubscriptionNotFoundFault {  }
```

<p>An Amazon Redshift event notification subscription with the specified name does not exist.</p>

#### `SubscriptionSeverityNotFoundFault`

``` purescript
newtype SubscriptionSeverityNotFoundFault
  = SubscriptionSeverityNotFoundFault {  }
```

<p>The value specified for the event severity was not one of the allowed values, or it specified a severity that does not apply to the specified source type. The allowed values are ERROR and INFO.</p>

#### `TStamp`

``` purescript
newtype TStamp
  = TStamp Number
```

#### `TableRestoreNotFoundFault`

``` purescript
newtype TableRestoreNotFoundFault
  = TableRestoreNotFoundFault {  }
```

<p>The specified <code>TableRestoreRequestId</code> value was not found.</p>

#### `TableRestoreStatus`

``` purescript
newtype TableRestoreStatus
  = TableRestoreStatus { "TableRestoreRequestId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (TableRestoreStatusType), "Message" :: NullOrUndefined (String), "RequestTime" :: NullOrUndefined (TStamp), "ProgressInMegaBytes" :: NullOrUndefined (LongOptional), "TotalDataInMegaBytes" :: NullOrUndefined (LongOptional), "ClusterIdentifier" :: NullOrUndefined (String), "SnapshotIdentifier" :: NullOrUndefined (String), "SourceDatabaseName" :: NullOrUndefined (String), "SourceSchemaName" :: NullOrUndefined (String), "SourceTableName" :: NullOrUndefined (String), "TargetDatabaseName" :: NullOrUndefined (String), "TargetSchemaName" :: NullOrUndefined (String), "NewTableName" :: NullOrUndefined (String) }
```

<p>Describes the status of a <a>RestoreTableFromClusterSnapshot</a> operation.</p>

#### `TableRestoreStatusList`

``` purescript
newtype TableRestoreStatusList
  = TableRestoreStatusList (Array TableRestoreStatus)
```

#### `TableRestoreStatusMessage`

``` purescript
newtype TableRestoreStatusMessage
  = TableRestoreStatusMessage { "TableRestoreStatusDetails" :: NullOrUndefined (TableRestoreStatusList), "Marker" :: NullOrUndefined (String) }
```

<p/>

#### `TableRestoreStatusType`

``` purescript
newtype TableRestoreStatusType
  = TableRestoreStatusType String
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (String), "Value" :: NullOrUndefined (String) }
```

<p>A tag consisting of a name/value pair for a resource.</p>

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array String)
```

#### `TagLimitExceededFault`

``` purescript
newtype TagLimitExceededFault
  = TagLimitExceededFault {  }
```

<p>The request exceeds the limit of 10 tags for the resource.</p>

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

#### `TagValueList`

``` purescript
newtype TagValueList
  = TagValueList (Array String)
```

#### `TaggedResource`

``` purescript
newtype TaggedResource
  = TaggedResource { "Tag" :: NullOrUndefined (Tag), "ResourceName" :: NullOrUndefined (String), "ResourceType" :: NullOrUndefined (String) }
```

<p>A tag and its associated resource.</p>

#### `TaggedResourceList`

``` purescript
newtype TaggedResourceList
  = TaggedResourceList (Array TaggedResource)
```

#### `TaggedResourceListMessage`

``` purescript
newtype TaggedResourceListMessage
  = TaggedResourceListMessage { "TaggedResources" :: NullOrUndefined (TaggedResourceList), "Marker" :: NullOrUndefined (String) }
```

<p/>

#### `UnauthorizedOperation`

``` purescript
newtype UnauthorizedOperation
  = UnauthorizedOperation {  }
```

<p>Your account is not authorized to perform the requested operation.</p>

#### `UnknownSnapshotCopyRegionFault`

``` purescript
newtype UnknownSnapshotCopyRegionFault
  = UnknownSnapshotCopyRegionFault {  }
```

<p>The specified region is incorrect or does not exist.</p>

#### `UnsupportedOperationFault`

``` purescript
newtype UnsupportedOperationFault
  = UnsupportedOperationFault {  }
```

<p>The requested operation isn't supported.</p>

#### `UnsupportedOptionFault`

``` purescript
newtype UnsupportedOptionFault
  = UnsupportedOptionFault {  }
```

<p>A request option was specified that is not supported.</p>

#### `VpcSecurityGroupIdList`

``` purescript
newtype VpcSecurityGroupIdList
  = VpcSecurityGroupIdList (Array String)
```

#### `VpcSecurityGroupMembership`

``` purescript
newtype VpcSecurityGroupMembership
  = VpcSecurityGroupMembership { "VpcSecurityGroupId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String) }
```

<p>Describes the members of a VPC security group.</p>

#### `VpcSecurityGroupMembershipList`

``` purescript
newtype VpcSecurityGroupMembershipList
  = VpcSecurityGroupMembershipList (Array VpcSecurityGroupMembership)
```


