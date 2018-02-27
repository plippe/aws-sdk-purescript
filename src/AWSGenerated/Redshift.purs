

-- | <fullname>Amazon Redshift</fullname> <p> <b>Overview</b> </p> <p>This is an interface reference for Amazon Redshift. It contains documentation for one of the programming or command line interfaces you can use to manage Amazon Redshift clusters. Note that Amazon Redshift is asynchronous, which means that some interfaces may require techniques, such as polling or asynchronous callback handlers, to determine when a command has been applied. In this reference, the parameter descriptions indicate whether a change is applied immediately, on the next instance reboot, or during the next maintenance window. For a summary of the Amazon Redshift cluster management interfaces, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/using-aws-sdk.html">Using the Amazon Redshift Management Interfaces</a>.</p> <p>Amazon Redshift manages all the work of setting up, operating, and scaling a data warehouse: provisioning capacity, monitoring and backing up the cluster, and applying patches and upgrades to the Amazon Redshift engine. You can focus on using your data to acquire new insights for your business and customers.</p> <p>If you are a first-time user of Amazon Redshift, we recommend that you begin by reading the <a href="http://docs.aws.amazon.com/redshift/latest/gsg/getting-started.html">Amazon Redshift Getting Started Guide</a>.</p> <p>If you are a database developer, the <a href="http://docs.aws.amazon.com/redshift/latest/dg/welcome.html">Amazon Redshift Database Developer Guide</a> explains how to design, build, query, and maintain the databases that make up your data warehouse. </p>
module AWS.Redshift where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Redshift" :: String


-- | <p>Adds an inbound (ingress) rule to an Amazon Redshift security group. Depending on whether the application accessing your cluster is running on the Internet or an Amazon EC2 instance, you can authorize inbound access to either a Classless Interdomain Routing (CIDR)/Internet Protocol (IP) range or to an Amazon EC2 security group. You can add as many as 20 ingress rules to an Amazon Redshift security group.</p> <p>If you authorize access to an Amazon EC2 security group, specify <i>EC2SecurityGroupName</i> and <i>EC2SecurityGroupOwnerId</i>. The Amazon EC2 security group and Amazon Redshift cluster must be in the same AWS region. </p> <p>If you authorize access to a CIDR/IP address range, specify <i>CIDRIP</i>. For an overview of CIDR blocks, see the Wikipedia article on <a href="http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing">Classless Inter-Domain Routing</a>. </p> <p>You must also associate the security group with a cluster so that clients running on these IP addresses or the EC2 instance are authorized to connect to the cluster. For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Working with Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
authorizeClusterSecurityGroupIngress :: forall eff. AuthorizeClusterSecurityGroupIngressMessage -> Aff (err :: AWS.RequestError | eff) AuthorizeClusterSecurityGroupIngressResult
authorizeClusterSecurityGroupIngress = AWS.request serviceName "authorizeClusterSecurityGroupIngress" 


-- | <p>Authorizes the specified AWS customer account to restore the specified snapshot.</p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
authorizeSnapshotAccess :: forall eff. AuthorizeSnapshotAccessMessage -> Aff (err :: AWS.RequestError | eff) AuthorizeSnapshotAccessResult
authorizeSnapshotAccess = AWS.request serviceName "authorizeSnapshotAccess" 


-- | <p>Copies the specified automated cluster snapshot to a new manual cluster snapshot. The source must be an automated snapshot and it must be in the available state.</p> <p>When you delete a cluster, Amazon Redshift deletes any automated snapshots of the cluster. Also, when the retention period of the snapshot expires, Amazon Redshift automatically deletes it. If you want to keep an automated snapshot for a longer period, you can make a manual copy of the snapshot. Manual snapshots are retained until you delete them.</p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
copyClusterSnapshot :: forall eff. CopyClusterSnapshotMessage -> Aff (err :: AWS.RequestError | eff) CopyClusterSnapshotResult
copyClusterSnapshot = AWS.request serviceName "copyClusterSnapshot" 


-- | <p>Creates a new cluster.</p> <p>To create the cluster in Virtual Private Cloud (VPC), you must provide a cluster subnet group name. The cluster subnet group identifies the subnets of your VPC that Amazon Redshift uses when creating the cluster. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
createCluster :: forall eff. CreateClusterMessage -> Aff (err :: AWS.RequestError | eff) CreateClusterResult
createCluster = AWS.request serviceName "createCluster" 


-- | <p>Creates an Amazon Redshift parameter group.</p> <p>Creating parameter groups is independent of creating clusters. You can associate a cluster with a parameter group when you create the cluster. You can also associate an existing cluster with a parameter group after the cluster is created by using <a>ModifyCluster</a>. </p> <p>Parameters in the parameter group define specific behavior that applies to the databases you create on the cluster. For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
createClusterParameterGroup :: forall eff. CreateClusterParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateClusterParameterGroupResult
createClusterParameterGroup = AWS.request serviceName "createClusterParameterGroup" 


-- | <p>Creates a new Amazon Redshift security group. You use security groups to control access to non-VPC clusters.</p> <p> For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Amazon Redshift Cluster Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
createClusterSecurityGroup :: forall eff. CreateClusterSecurityGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateClusterSecurityGroupResult
createClusterSecurityGroup = AWS.request serviceName "createClusterSecurityGroup" 


-- | <p>Creates a manual snapshot of the specified cluster. The cluster must be in the <code>available</code> state. </p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
createClusterSnapshot :: forall eff. CreateClusterSnapshotMessage -> Aff (err :: AWS.RequestError | eff) CreateClusterSnapshotResult
createClusterSnapshot = AWS.request serviceName "createClusterSnapshot" 


-- | <p>Creates a new Amazon Redshift subnet group. You must provide a list of one or more subnets in your existing Amazon Virtual Private Cloud (Amazon VPC) when creating Amazon Redshift subnet group.</p> <p> For information about subnet groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-cluster-subnet-groups.html">Amazon Redshift Cluster Subnet Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
createClusterSubnetGroup :: forall eff. CreateClusterSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateClusterSubnetGroupResult
createClusterSubnetGroup = AWS.request serviceName "createClusterSubnetGroup" 


-- | <p>Creates an Amazon Redshift event notification subscription. This action requires an ARN (Amazon Resource Name) of an Amazon SNS topic created by either the Amazon Redshift console, the Amazon SNS console, or the Amazon SNS API. To obtain an ARN with Amazon SNS, you must create a topic in Amazon SNS and subscribe to the topic. The ARN is displayed in the SNS console.</p> <p>You can specify the source type, and lists of Amazon Redshift source IDs, event categories, and event severities. Notifications will be sent for all events you want that match those criteria. For example, you can specify source type = cluster, source ID = my-cluster-1 and mycluster2, event categories = Availability, Backup, and severity = ERROR. The subscription will only send notifications for those ERROR events in the Availability and Backup categories for the specified clusters.</p> <p>If you specify both the source type and source IDs, such as source type = cluster and source identifier = my-cluster-1, notifications will be sent for all the cluster events for my-cluster-1. If you specify a source type but do not specify a source identifier, you will receive notice of the events for the objects of that type in your AWS account. If you do not specify either the SourceType nor the SourceIdentifier, you will be notified of events generated from all Amazon Redshift sources belonging to your AWS account. You must specify a source type if you specify a source ID.</p>
createEventSubscription :: forall eff. CreateEventSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) CreateEventSubscriptionResult
createEventSubscription = AWS.request serviceName "createEventSubscription" 


-- | <p>Creates an HSM client certificate that an Amazon Redshift cluster will use to connect to the client's HSM in order to store and retrieve the keys used to encrypt the cluster databases.</p> <p>The command returns a public key, which you must store in the HSM. In addition to creating the HSM certificate, you must create an Amazon Redshift HSM configuration that provides a cluster the information needed to store and use encryption keys in the HSM. For more information, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html">Hardware Security Modules</a> in the Amazon Redshift Cluster Management Guide.</p>
createHsmClientCertificate :: forall eff. CreateHsmClientCertificateMessage -> Aff (err :: AWS.RequestError | eff) CreateHsmClientCertificateResult
createHsmClientCertificate = AWS.request serviceName "createHsmClientCertificate" 


-- | <p>Creates an HSM configuration that contains the information required by an Amazon Redshift cluster to store and use database encryption keys in a Hardware Security Module (HSM). After creating the HSM configuration, you can specify it as a parameter when creating a cluster. The cluster will then store its encryption keys in the HSM.</p> <p>In addition to creating an HSM configuration, you must also create an HSM client certificate. For more information, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html">Hardware Security Modules</a> in the Amazon Redshift Cluster Management Guide.</p>
createHsmConfiguration :: forall eff. CreateHsmConfigurationMessage -> Aff (err :: AWS.RequestError | eff) CreateHsmConfigurationResult
createHsmConfiguration = AWS.request serviceName "createHsmConfiguration" 


-- | <p>Creates a snapshot copy grant that permits Amazon Redshift to use a customer master key (CMK) from AWS Key Management Service (AWS KMS) to encrypt copied snapshots in a destination region.</p> <p> For more information about managing snapshot copy grants, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html">Amazon Redshift Database Encryption</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
createSnapshotCopyGrant :: forall eff. CreateSnapshotCopyGrantMessage -> Aff (err :: AWS.RequestError | eff) CreateSnapshotCopyGrantResult
createSnapshotCopyGrant = AWS.request serviceName "createSnapshotCopyGrant" 


-- | <p>Adds one or more tags to a specified resource.</p> <p>A resource can have up to 10 tags. If you try to create more than 10 tags for a resource, you will receive an error and the attempt will fail.</p> <p>If you specify a key that already exists for the resource, the value for that key will be updated with the new value.</p>
createTags :: forall eff. CreateTagsMessage -> Aff (err :: AWS.RequestError | eff) Unit
createTags = AWS.request serviceName "createTags" 


-- | <p>Deletes a previously provisioned cluster. A successful response from the web service indicates that the request was received correctly. Use <a>DescribeClusters</a> to monitor the status of the deletion. The delete operation cannot be canceled or reverted once submitted. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>If you want to shut down the cluster and retain it for future use, set <i>SkipFinalClusterSnapshot</i> to <code>false</code> and specify a name for <i>FinalClusterSnapshotIdentifier</i>. You can later restore this snapshot to resume using the cluster. If a final cluster snapshot is requested, the status of the cluster will be "final-snapshot" while the snapshot is being taken, then it's "deleting" once Amazon Redshift begins deleting the cluster. </p> <p> For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
deleteCluster :: forall eff. DeleteClusterMessage -> Aff (err :: AWS.RequestError | eff) DeleteClusterResult
deleteCluster = AWS.request serviceName "deleteCluster" 


-- | <p>Deletes a specified Amazon Redshift parameter group.</p> <note> <p>You cannot delete a parameter group if it is associated with a cluster.</p> </note>
deleteClusterParameterGroup :: forall eff. DeleteClusterParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteClusterParameterGroup = AWS.request serviceName "deleteClusterParameterGroup" 


-- | <p>Deletes an Amazon Redshift security group.</p> <note> <p>You cannot delete a security group that is associated with any clusters. You cannot delete the default security group.</p> </note> <p> For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Amazon Redshift Cluster Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
deleteClusterSecurityGroup :: forall eff. DeleteClusterSecurityGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteClusterSecurityGroup = AWS.request serviceName "deleteClusterSecurityGroup" 


-- | <p>Deletes the specified manual snapshot. The snapshot must be in the <code>available</code> state, with no other users authorized to access the snapshot. </p> <p>Unlike automated snapshots, manual snapshots are retained even after you delete your cluster. Amazon Redshift does not delete your manual snapshots. You must delete manual snapshot explicitly to avoid getting charged. If other accounts are authorized to access the snapshot, you must revoke all of the authorizations before you can delete the snapshot.</p>
deleteClusterSnapshot :: forall eff. DeleteClusterSnapshotMessage -> Aff (err :: AWS.RequestError | eff) DeleteClusterSnapshotResult
deleteClusterSnapshot = AWS.request serviceName "deleteClusterSnapshot" 


-- | <p>Deletes the specified cluster subnet group.</p>
deleteClusterSubnetGroup :: forall eff. DeleteClusterSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteClusterSubnetGroup = AWS.request serviceName "deleteClusterSubnetGroup" 


-- | <p>Deletes an Amazon Redshift event notification subscription.</p>
deleteEventSubscription :: forall eff. DeleteEventSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteEventSubscription = AWS.request serviceName "deleteEventSubscription" 


-- | <p>Deletes the specified HSM client certificate.</p>
deleteHsmClientCertificate :: forall eff. DeleteHsmClientCertificateMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteHsmClientCertificate = AWS.request serviceName "deleteHsmClientCertificate" 


-- | <p>Deletes the specified Amazon Redshift HSM configuration.</p>
deleteHsmConfiguration :: forall eff. DeleteHsmConfigurationMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteHsmConfiguration = AWS.request serviceName "deleteHsmConfiguration" 


-- | <p>Deletes the specified snapshot copy grant.</p>
deleteSnapshotCopyGrant :: forall eff. DeleteSnapshotCopyGrantMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteSnapshotCopyGrant = AWS.request serviceName "deleteSnapshotCopyGrant" 


-- | <p>Deletes a tag or tags from a resource. You must provide the ARN of the resource from which you want to delete the tag or tags.</p>
deleteTags :: forall eff. DeleteTagsMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteTags = AWS.request serviceName "deleteTags" 


-- | <p>Returns a list of Amazon Redshift parameter groups, including parameter groups you created and the default parameter group. For each parameter group, the response includes the parameter group name, description, and parameter group family name. You can optionally specify a name to retrieve the description of a specific parameter group.</p> <p> For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all parameter groups that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all parameter groups that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, parameter groups are returned regardless of whether they have tag keys or values associated with them.</p>
describeClusterParameterGroups :: forall eff. DescribeClusterParameterGroupsMessage -> Aff (err :: AWS.RequestError | eff) ClusterParameterGroupsMessage
describeClusterParameterGroups = AWS.request serviceName "describeClusterParameterGroups" 


-- | <p>Returns a detailed list of parameters contained within the specified Amazon Redshift parameter group. For each parameter the response includes information such as parameter name, description, data type, value, whether the parameter value is modifiable, and so on.</p> <p>You can specify <i>source</i> filter to retrieve parameters of only specific type. For example, to retrieve parameters that were modified by a user action such as from <a>ModifyClusterParameterGroup</a>, you can specify <i>source</i> equal to <i>user</i>.</p> <p> For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
describeClusterParameters :: forall eff. DescribeClusterParametersMessage -> Aff (err :: AWS.RequestError | eff) ClusterParameterGroupDetails
describeClusterParameters = AWS.request serviceName "describeClusterParameters" 


-- | <p>Returns information about Amazon Redshift security groups. If the name of a security group is specified, the response will contain only information about only that security group.</p> <p> For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Amazon Redshift Cluster Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all security groups that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all security groups that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, security groups are returned regardless of whether they have tag keys or values associated with them.</p>
describeClusterSecurityGroups :: forall eff. DescribeClusterSecurityGroupsMessage -> Aff (err :: AWS.RequestError | eff) ClusterSecurityGroupMessage
describeClusterSecurityGroups = AWS.request serviceName "describeClusterSecurityGroups" 


-- | <p>Returns one or more snapshot objects, which contain metadata about your cluster snapshots. By default, this operation returns information about all snapshots of all clusters that are owned by you AWS customer account. No information is returned for snapshots owned by inactive AWS customer accounts.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all snapshots that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all snapshots that have any combination of those values are returned. Only snapshots that you own are returned in the response; shared snapshots are not returned with the tag key and tag value request parameters.</p> <p>If both tag keys and values are omitted from the request, snapshots are returned regardless of whether they have tag keys or values associated with them.</p>
describeClusterSnapshots :: forall eff. DescribeClusterSnapshotsMessage -> Aff (err :: AWS.RequestError | eff) SnapshotMessage
describeClusterSnapshots = AWS.request serviceName "describeClusterSnapshots" 


-- | <p>Returns one or more cluster subnet group objects, which contain metadata about your cluster subnet groups. By default, this operation returns information about all cluster subnet groups that are defined in you AWS account.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all subnet groups that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all subnet groups that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, subnet groups are returned regardless of whether they have tag keys or values associated with them.</p>
describeClusterSubnetGroups :: forall eff. DescribeClusterSubnetGroupsMessage -> Aff (err :: AWS.RequestError | eff) ClusterSubnetGroupMessage
describeClusterSubnetGroups = AWS.request serviceName "describeClusterSubnetGroups" 


-- | <p>Returns descriptions of the available Amazon Redshift cluster versions. You can call this operation even before creating any clusters to learn more about the Amazon Redshift versions. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
describeClusterVersions :: forall eff. DescribeClusterVersionsMessage -> Aff (err :: AWS.RequestError | eff) ClusterVersionsMessage
describeClusterVersions = AWS.request serviceName "describeClusterVersions" 


-- | <p>Returns properties of provisioned clusters including general cluster properties, cluster database properties, maintenance and backup properties, and security and access properties. This operation supports pagination. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all clusters that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all clusters that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, clusters are returned regardless of whether they have tag keys or values associated with them.</p>
describeClusters :: forall eff. DescribeClustersMessage -> Aff (err :: AWS.RequestError | eff) ClustersMessage
describeClusters = AWS.request serviceName "describeClusters" 


-- | <p>Returns a list of parameter settings for the specified parameter group family.</p> <p> For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
describeDefaultClusterParameters :: forall eff. DescribeDefaultClusterParametersMessage -> Aff (err :: AWS.RequestError | eff) DescribeDefaultClusterParametersResult
describeDefaultClusterParameters = AWS.request serviceName "describeDefaultClusterParameters" 


-- | <p>Displays a list of event categories for all event source types, or for a specified source type. For a list of the event categories and source types, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-event-notifications.html">Amazon Redshift Event Notifications</a>.</p>
describeEventCategories :: forall eff. DescribeEventCategoriesMessage -> Aff (err :: AWS.RequestError | eff) EventCategoriesMessage
describeEventCategories = AWS.request serviceName "describeEventCategories" 


-- | <p>Lists descriptions of all the Amazon Redshift event notification subscriptions for a customer account. If you specify a subscription name, lists the description for that subscription.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all event notification subscriptions that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all subscriptions that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, subscriptions are returned regardless of whether they have tag keys or values associated with them.</p>
describeEventSubscriptions :: forall eff. DescribeEventSubscriptionsMessage -> Aff (err :: AWS.RequestError | eff) EventSubscriptionsMessage
describeEventSubscriptions = AWS.request serviceName "describeEventSubscriptions" 


-- | <p>Returns events related to clusters, security groups, snapshots, and parameter groups for the past 14 days. Events specific to a particular cluster, security group, snapshot or parameter group can be obtained by providing the name as a parameter. By default, the past hour of events are returned.</p>
describeEvents :: forall eff. DescribeEventsMessage -> Aff (err :: AWS.RequestError | eff) EventsMessage
describeEvents = AWS.request serviceName "describeEvents" 


-- | <p>Returns information about the specified HSM client certificate. If no certificate ID is specified, returns information about all the HSM certificates owned by your AWS customer account.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all HSM client certificates that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all HSM client certificates that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, HSM client certificates are returned regardless of whether they have tag keys or values associated with them.</p>
describeHsmClientCertificates :: forall eff. DescribeHsmClientCertificatesMessage -> Aff (err :: AWS.RequestError | eff) HsmClientCertificateMessage
describeHsmClientCertificates = AWS.request serviceName "describeHsmClientCertificates" 


-- | <p>Returns information about the specified Amazon Redshift HSM configuration. If no configuration ID is specified, returns information about all the HSM configurations owned by your AWS customer account.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all HSM connections that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all HSM connections that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, HSM connections are returned regardless of whether they have tag keys or values associated with them.</p>
describeHsmConfigurations :: forall eff. DescribeHsmConfigurationsMessage -> Aff (err :: AWS.RequestError | eff) HsmConfigurationMessage
describeHsmConfigurations = AWS.request serviceName "describeHsmConfigurations" 


-- | <p>Describes whether information, such as queries and connection attempts, is being logged for the specified Amazon Redshift cluster.</p>
describeLoggingStatus :: forall eff. DescribeLoggingStatusMessage -> Aff (err :: AWS.RequestError | eff) LoggingStatus
describeLoggingStatus = AWS.request serviceName "describeLoggingStatus" 


-- | <p>Returns a list of orderable cluster options. Before you create a new cluster you can use this operation to find what options are available, such as the EC2 Availability Zones (AZ) in the specific AWS region that you can specify, and the node types you can request. The node types differ by available storage, memory, CPU and price. With the cost involved you might want to obtain a list of cluster options in the specific region and specify values when creating a cluster. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
describeOrderableClusterOptions :: forall eff. DescribeOrderableClusterOptionsMessage -> Aff (err :: AWS.RequestError | eff) OrderableClusterOptionsMessage
describeOrderableClusterOptions = AWS.request serviceName "describeOrderableClusterOptions" 


-- | <p>Returns a list of the available reserved node offerings by Amazon Redshift with their descriptions including the node type, the fixed and recurring costs of reserving the node and duration the node will be reserved for you. These descriptions help you determine which reserve node offering you want to purchase. You then use the unique offering ID in you call to <a>PurchaseReservedNodeOffering</a> to reserve one or more nodes for your Amazon Redshift cluster. </p> <p> For more information about reserved node offerings, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html">Purchasing Reserved Nodes</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
describeReservedNodeOfferings :: forall eff. DescribeReservedNodeOfferingsMessage -> Aff (err :: AWS.RequestError | eff) ReservedNodeOfferingsMessage
describeReservedNodeOfferings = AWS.request serviceName "describeReservedNodeOfferings" 


-- | <p>Returns the descriptions of the reserved nodes.</p>
describeReservedNodes :: forall eff. DescribeReservedNodesMessage -> Aff (err :: AWS.RequestError | eff) ReservedNodesMessage
describeReservedNodes = AWS.request serviceName "describeReservedNodes" 


-- | <p>Returns information about the last resize operation for the specified cluster. If no resize operation has ever been initiated for the specified cluster, a <code>HTTP 404</code> error is returned. If a resize operation was initiated and completed, the status of the resize remains as <code>SUCCEEDED</code> until the next resize. </p> <p>A resize operation can be requested using <a>ModifyCluster</a> and specifying a different number or type of nodes for the cluster. </p>
describeResize :: forall eff. DescribeResizeMessage -> Aff (err :: AWS.RequestError | eff) ResizeProgressMessage
describeResize = AWS.request serviceName "describeResize" 


-- | <p>Returns a list of snapshot copy grants owned by the AWS account in the destination region.</p> <p> For more information about managing snapshot copy grants, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html">Amazon Redshift Database Encryption</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
describeSnapshotCopyGrants :: forall eff. DescribeSnapshotCopyGrantsMessage -> Aff (err :: AWS.RequestError | eff) SnapshotCopyGrantMessage
describeSnapshotCopyGrants = AWS.request serviceName "describeSnapshotCopyGrants" 


-- | <p>Lists the status of one or more table restore requests made using the <a>RestoreTableFromClusterSnapshot</a> API action. If you don't specify a value for the <code>TableRestoreRequestId</code> parameter, then <code>DescribeTableRestoreStatus</code> returns the status of all table restore requests ordered by the date and time of the request in ascending order. Otherwise <code>DescribeTableRestoreStatus</code> returns the status of the table specified by <code>TableRestoreRequestId</code>.</p>
describeTableRestoreStatus :: forall eff. DescribeTableRestoreStatusMessage -> Aff (err :: AWS.RequestError | eff) TableRestoreStatusMessage
describeTableRestoreStatus = AWS.request serviceName "describeTableRestoreStatus" 


-- | <p>Returns a list of tags. You can return tags from a specific resource by specifying an ARN, or you can return all tags for a given type of resource, such as clusters, snapshots, and so on.</p> <p>The following are limitations for <code>DescribeTags</code>: </p> <ul> <li> <p>You cannot specify an ARN and a resource-type value together in the same request.</p> </li> <li> <p>You cannot use the <code>MaxRecords</code> and <code>Marker</code> parameters together with the ARN parameter.</p> </li> <li> <p>The <code>MaxRecords</code> parameter can be a range from 10 to 50 results to return in a request.</p> </li> </ul> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all resources that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all resources that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, resources are returned regardless of whether they have tag keys or values associated with them.</p>
describeTags :: forall eff. DescribeTagsMessage -> Aff (err :: AWS.RequestError | eff) TaggedResourceListMessage
describeTags = AWS.request serviceName "describeTags" 


-- | <p>Stops logging information, such as queries and connection attempts, for the specified Amazon Redshift cluster.</p>
disableLogging :: forall eff. DisableLoggingMessage -> Aff (err :: AWS.RequestError | eff) LoggingStatus
disableLogging = AWS.request serviceName "disableLogging" 


-- | <p>Disables the automatic copying of snapshots from one region to another region for a specified cluster.</p> <p>If your cluster and its snapshots are encrypted using a customer master key (CMK) from AWS KMS, use <a>DeleteSnapshotCopyGrant</a> to delete the grant that grants Amazon Redshift permission to the CMK in the destination region. </p>
disableSnapshotCopy :: forall eff. DisableSnapshotCopyMessage -> Aff (err :: AWS.RequestError | eff) DisableSnapshotCopyResult
disableSnapshotCopy = AWS.request serviceName "disableSnapshotCopy" 


-- | <p>Starts logging information, such as queries and connection attempts, for the specified Amazon Redshift cluster.</p>
enableLogging :: forall eff. EnableLoggingMessage -> Aff (err :: AWS.RequestError | eff) LoggingStatus
enableLogging = AWS.request serviceName "enableLogging" 


-- | <p>Enables the automatic copy of snapshots from one region to another region for a specified cluster.</p>
enableSnapshotCopy :: forall eff. EnableSnapshotCopyMessage -> Aff (err :: AWS.RequestError | eff) EnableSnapshotCopyResult
enableSnapshotCopy = AWS.request serviceName "enableSnapshotCopy" 


-- | <p>Returns a database user name and temporary password with temporary authorization to log on to an Amazon Redshift database. The action returns the database user name prefixed with <code>IAM:</code> if <code>AutoCreate</code> is <code>False</code> or <code>IAMA:</code> if <code>AutoCreate</code> is <code>True</code>. You can optionally specify one or more database user groups that the user will join at log on. By default, the temporary credentials expire in 900 seconds. You can optionally specify a duration between 900 seconds (15 minutes) and 3600 seconds (60 minutes). For more information, see <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/generating-user-credentials.html">Using IAM Authentication to Generate Database User Credentials</a> in the Amazon Redshift Cluster Management Guide.</p> <p>The AWS Identity and Access Management (IAM)user or role that executes GetClusterCredentials must have an IAM policy attached that allows access to all necessary actions and resources. For more information about permissions, see <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html#redshift-policy-resources.getclustercredentials-resources">Resource Policies for GetClusterCredentials</a> in the Amazon Redshift Cluster Management Guide.</p> <p>If the <code>DbGroups</code> parameter is specified, the IAM policy must allow the <code>redshift:JoinGroup</code> action with access to the listed <code>dbgroups</code>. </p> <p>In addition, if the <code>AutoCreate</code> parameter is set to <code>True</code>, then the policy must include the <code>redshift:CreateClusterUser</code> privilege.</p> <p>If the <code>DbName</code> parameter is specified, the IAM policy must allow access to the resource <code>dbname</code> for the specified database name. </p>
getClusterCredentials :: forall eff. GetClusterCredentialsMessage -> Aff (err :: AWS.RequestError | eff) ClusterCredentials
getClusterCredentials = AWS.request serviceName "getClusterCredentials" 


-- | <p>Modifies the settings for a cluster. For example, you can add another security or parameter group, update the preferred maintenance window, or change the master user password. Resetting a cluster password or modifying the security groups associated with a cluster do not need a reboot. However, modifying a parameter group requires a reboot for parameters to take effect. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>You can also change node type and the number of nodes to scale up or down the cluster. When resizing a cluster, you must specify both the number of nodes and the node type even if one of the parameters does not change.</p>
modifyCluster :: forall eff. ModifyClusterMessage -> Aff (err :: AWS.RequestError | eff) ModifyClusterResult
modifyCluster = AWS.request serviceName "modifyCluster" 


-- | <p>Modifies the list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.</p> <p>A cluster can have up to 10 IAM roles associated at any time.</p>
modifyClusterIamRoles :: forall eff. ModifyClusterIamRolesMessage -> Aff (err :: AWS.RequestError | eff) ModifyClusterIamRolesResult
modifyClusterIamRoles = AWS.request serviceName "modifyClusterIamRoles" 


-- | <p>Modifies the parameters of a parameter group.</p> <p> For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
modifyClusterParameterGroup :: forall eff. ModifyClusterParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) ClusterParameterGroupNameMessage
modifyClusterParameterGroup = AWS.request serviceName "modifyClusterParameterGroup" 


-- | <p>Modifies a cluster subnet group to include the specified list of VPC subnets. The operation replaces the existing list of subnets with the new list of subnets.</p>
modifyClusterSubnetGroup :: forall eff. ModifyClusterSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) ModifyClusterSubnetGroupResult
modifyClusterSubnetGroup = AWS.request serviceName "modifyClusterSubnetGroup" 


-- | <p>Modifies an existing Amazon Redshift event notification subscription.</p>
modifyEventSubscription :: forall eff. ModifyEventSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) ModifyEventSubscriptionResult
modifyEventSubscription = AWS.request serviceName "modifyEventSubscription" 


-- | <p>Modifies the number of days to retain automated snapshots in the destination region after they are copied from the source region.</p>
modifySnapshotCopyRetentionPeriod :: forall eff. ModifySnapshotCopyRetentionPeriodMessage -> Aff (err :: AWS.RequestError | eff) ModifySnapshotCopyRetentionPeriodResult
modifySnapshotCopyRetentionPeriod = AWS.request serviceName "modifySnapshotCopyRetentionPeriod" 


-- | <p>Allows you to purchase reserved nodes. Amazon Redshift offers a predefined set of reserved node offerings. You can purchase one or more of the offerings. You can call the <a>DescribeReservedNodeOfferings</a> API to obtain the available reserved node offerings. You can call this API by providing a specific reserved node offering and the number of nodes you want to reserve. </p> <p> For more information about reserved node offerings, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html">Purchasing Reserved Nodes</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
purchaseReservedNodeOffering :: forall eff. PurchaseReservedNodeOfferingMessage -> Aff (err :: AWS.RequestError | eff) PurchaseReservedNodeOfferingResult
purchaseReservedNodeOffering = AWS.request serviceName "purchaseReservedNodeOffering" 


-- | <p>Reboots a cluster. This action is taken as soon as possible. It results in a momentary outage to the cluster, during which the cluster status is set to <code>rebooting</code>. A cluster event is created when the reboot is completed. Any pending cluster modifications (see <a>ModifyCluster</a>) are applied at this reboot. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
rebootCluster :: forall eff. RebootClusterMessage -> Aff (err :: AWS.RequestError | eff) RebootClusterResult
rebootCluster = AWS.request serviceName "rebootCluster" 


-- | <p>Sets one or more parameters of the specified parameter group to their default values and sets the source values of the parameters to "engine-default". To reset the entire parameter group specify the <i>ResetAllParameters</i> parameter. For parameter changes to take effect you must reboot any associated clusters. </p>
resetClusterParameterGroup :: forall eff. ResetClusterParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) ClusterParameterGroupNameMessage
resetClusterParameterGroup = AWS.request serviceName "resetClusterParameterGroup" 


-- | <p>Creates a new cluster from a snapshot. By default, Amazon Redshift creates the resulting cluster with the same configuration as the original cluster from which the snapshot was created, except that the new cluster is created with the default cluster security and parameter groups. After Amazon Redshift creates the cluster, you can use the <a>ModifyCluster</a> API to associate a different security group and different parameter group with the restored cluster. If you are using a DS node type, you can also choose to change to another DS node type of the same size during restore.</p> <p>If you restore a cluster into a VPC, you must provide a cluster subnet group where you want the cluster restored.</p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
restoreFromClusterSnapshot :: forall eff. RestoreFromClusterSnapshotMessage -> Aff (err :: AWS.RequestError | eff) RestoreFromClusterSnapshotResult
restoreFromClusterSnapshot = AWS.request serviceName "restoreFromClusterSnapshot" 


-- | <p>Creates a new table from a table in an Amazon Redshift cluster snapshot. You must create the new table within the Amazon Redshift cluster that the snapshot was taken from.</p> <p>You cannot use <code>RestoreTableFromClusterSnapshot</code> to restore a table with the same name as an existing table in an Amazon Redshift cluster. That is, you cannot overwrite an existing table in a cluster with a restored table. If you want to replace your original table with a new, restored table, then rename or drop your original table before you call <code>RestoreTableFromClusterSnapshot</code>. When you have renamed your original table, then you can pass the original name of the table as the <code>NewTableName</code> parameter value in the call to <code>RestoreTableFromClusterSnapshot</code>. This way, you can replace the original table with the table created from the snapshot.</p>
restoreTableFromClusterSnapshot :: forall eff. RestoreTableFromClusterSnapshotMessage -> Aff (err :: AWS.RequestError | eff) RestoreTableFromClusterSnapshotResult
restoreTableFromClusterSnapshot = AWS.request serviceName "restoreTableFromClusterSnapshot" 


-- | <p>Revokes an ingress rule in an Amazon Redshift security group for a previously authorized IP range or Amazon EC2 security group. To add an ingress rule, see <a>AuthorizeClusterSecurityGroupIngress</a>. For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Amazon Redshift Cluster Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
revokeClusterSecurityGroupIngress :: forall eff. RevokeClusterSecurityGroupIngressMessage -> Aff (err :: AWS.RequestError | eff) RevokeClusterSecurityGroupIngressResult
revokeClusterSecurityGroupIngress = AWS.request serviceName "revokeClusterSecurityGroupIngress" 


-- | <p>Removes the ability of the specified AWS customer account to restore the specified snapshot. If the account is currently restoring the snapshot, the restore will run to completion.</p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
revokeSnapshotAccess :: forall eff. RevokeSnapshotAccessMessage -> Aff (err :: AWS.RequestError | eff) RevokeSnapshotAccessResult
revokeSnapshotAccess = AWS.request serviceName "revokeSnapshotAccess" 


-- | <p>Rotates the encryption keys for a cluster.</p>
rotateEncryptionKey :: forall eff. RotateEncryptionKeyMessage -> Aff (err :: AWS.RequestError | eff) RotateEncryptionKeyResult
rotateEncryptionKey = AWS.request serviceName "rotateEncryptionKey" 


-- | <p>The owner of the specified snapshot has not authorized your account to access the snapshot.</p>
newtype AccessToSnapshotDeniedFault = AccessToSnapshotDeniedFault 
  { 
  }
derive instance newtypeAccessToSnapshotDeniedFault :: Newtype AccessToSnapshotDeniedFault _


-- | <p>Describes an AWS customer account authorized to restore a snapshot.</p>
newtype AccountWithRestoreAccess = AccountWithRestoreAccess 
  { "AccountId" :: NullOrUndefined (String)
  , "AccountAlias" :: NullOrUndefined (String)
  }
derive instance newtypeAccountWithRestoreAccess :: Newtype AccountWithRestoreAccess _


newtype AccountsWithRestoreAccessList = AccountsWithRestoreAccessList (Array AccountWithRestoreAccess)
derive instance newtypeAccountsWithRestoreAccessList :: Newtype AccountsWithRestoreAccessList _


-- | <p>The specified CIDR block or EC2 security group is already authorized for the specified cluster security group.</p>
newtype AuthorizationAlreadyExistsFault = AuthorizationAlreadyExistsFault 
  { 
  }
derive instance newtypeAuthorizationAlreadyExistsFault :: Newtype AuthorizationAlreadyExistsFault _


-- | <p>The specified CIDR IP range or EC2 security group is not authorized for the specified cluster security group.</p>
newtype AuthorizationNotFoundFault = AuthorizationNotFoundFault 
  { 
  }
derive instance newtypeAuthorizationNotFoundFault :: Newtype AuthorizationNotFoundFault _


-- | <p>The authorization quota for the cluster security group has been reached.</p>
newtype AuthorizationQuotaExceededFault = AuthorizationQuotaExceededFault 
  { 
  }
derive instance newtypeAuthorizationQuotaExceededFault :: Newtype AuthorizationQuotaExceededFault _


-- | <p/>
newtype AuthorizeClusterSecurityGroupIngressMessage = AuthorizeClusterSecurityGroupIngressMessage 
  { "ClusterSecurityGroupName" :: (String)
  , "CIDRIP" :: NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined (String)
  }
derive instance newtypeAuthorizeClusterSecurityGroupIngressMessage :: Newtype AuthorizeClusterSecurityGroupIngressMessage _


newtype AuthorizeClusterSecurityGroupIngressResult = AuthorizeClusterSecurityGroupIngressResult 
  { "ClusterSecurityGroup" :: NullOrUndefined (ClusterSecurityGroup)
  }
derive instance newtypeAuthorizeClusterSecurityGroupIngressResult :: Newtype AuthorizeClusterSecurityGroupIngressResult _


-- | <p/>
newtype AuthorizeSnapshotAccessMessage = AuthorizeSnapshotAccessMessage 
  { "SnapshotIdentifier" :: (String)
  , "SnapshotClusterIdentifier" :: NullOrUndefined (String)
  , "AccountWithRestoreAccess" :: (String)
  }
derive instance newtypeAuthorizeSnapshotAccessMessage :: Newtype AuthorizeSnapshotAccessMessage _


newtype AuthorizeSnapshotAccessResult = AuthorizeSnapshotAccessResult 
  { "Snapshot" :: NullOrUndefined (Snapshot)
  }
derive instance newtypeAuthorizeSnapshotAccessResult :: Newtype AuthorizeSnapshotAccessResult _


-- | <p>Describes an availability zone.</p>
newtype AvailabilityZone = AvailabilityZone 
  { "Name" :: NullOrUndefined (String)
  }
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _


newtype AvailabilityZoneList = AvailabilityZoneList (Array AvailabilityZone)
derive instance newtypeAvailabilityZoneList :: Newtype AvailabilityZoneList _


newtype BooleanOptional = BooleanOptional Boolean
derive instance newtypeBooleanOptional :: Newtype BooleanOptional _


-- | <p>Could not find the specified S3 bucket.</p>
newtype BucketNotFoundFault = BucketNotFoundFault 
  { 
  }
derive instance newtypeBucketNotFoundFault :: Newtype BucketNotFoundFault _


-- | <p>Describes a cluster.</p>
newtype Cluster = Cluster 
  { "ClusterIdentifier" :: NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined (String)
  , "ClusterStatus" :: NullOrUndefined (String)
  , "ModifyStatus" :: NullOrUndefined (String)
  , "MasterUsername" :: NullOrUndefined (String)
  , "DBName" :: NullOrUndefined (String)
  , "Endpoint" :: NullOrUndefined (Endpoint)
  , "ClusterCreateTime" :: NullOrUndefined (TStamp)
  , "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined (Int)
  , "ClusterSecurityGroups" :: NullOrUndefined (ClusterSecurityGroupMembershipList)
  , "VpcSecurityGroups" :: NullOrUndefined (VpcSecurityGroupMembershipList)
  , "ClusterParameterGroups" :: NullOrUndefined (ClusterParameterGroupStatusList)
  , "ClusterSubnetGroupName" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "PendingModifiedValues" :: NullOrUndefined (PendingModifiedValues)
  , "ClusterVersion" :: NullOrUndefined (String)
  , "AllowVersionUpgrade" :: NullOrUndefined (Boolean)
  , "NumberOfNodes" :: NullOrUndefined (Int)
  , "PubliclyAccessible" :: NullOrUndefined (Boolean)
  , "Encrypted" :: NullOrUndefined (Boolean)
  , "RestoreStatus" :: NullOrUndefined (RestoreStatus)
  , "HsmStatus" :: NullOrUndefined (HsmStatus)
  , "ClusterSnapshotCopyStatus" :: NullOrUndefined (ClusterSnapshotCopyStatus)
  , "ClusterPublicKey" :: NullOrUndefined (String)
  , "ClusterNodes" :: NullOrUndefined (ClusterNodesList)
  , "ElasticIpStatus" :: NullOrUndefined (ElasticIpStatus)
  , "ClusterRevisionNumber" :: NullOrUndefined (String)
  , "Tags" :: NullOrUndefined (TagList)
  , "KmsKeyId" :: NullOrUndefined (String)
  , "EnhancedVpcRouting" :: NullOrUndefined (Boolean)
  , "IamRoles" :: NullOrUndefined (ClusterIamRoleList)
  }
derive instance newtypeCluster :: Newtype Cluster _


-- | <p>The account already has a cluster with the given identifier.</p>
newtype ClusterAlreadyExistsFault = ClusterAlreadyExistsFault 
  { 
  }
derive instance newtypeClusterAlreadyExistsFault :: Newtype ClusterAlreadyExistsFault _


-- | <p>Temporary credentials with authorization to log on to an Amazon Redshift database. </p>
newtype ClusterCredentials = ClusterCredentials 
  { "DbUser" :: NullOrUndefined (String)
  , "DbPassword" :: NullOrUndefined (SensitiveString)
  , "Expiration" :: NullOrUndefined (TStamp)
  }
derive instance newtypeClusterCredentials :: Newtype ClusterCredentials _


-- | <p>An AWS Identity and Access Management (IAM) role that can be used by the associated Amazon Redshift cluster to access other AWS services.</p>
newtype ClusterIamRole = ClusterIamRole 
  { "IamRoleArn" :: NullOrUndefined (String)
  , "ApplyStatus" :: NullOrUndefined (String)
  }
derive instance newtypeClusterIamRole :: Newtype ClusterIamRole _


newtype ClusterIamRoleList = ClusterIamRoleList (Array ClusterIamRole)
derive instance newtypeClusterIamRoleList :: Newtype ClusterIamRoleList _


newtype ClusterList = ClusterList (Array Cluster)
derive instance newtypeClusterList :: Newtype ClusterList _


-- | <p>The identifier of a node in a cluster.</p>
newtype ClusterNode = ClusterNode 
  { "NodeRole" :: NullOrUndefined (String)
  , "PrivateIPAddress" :: NullOrUndefined (String)
  , "PublicIPAddress" :: NullOrUndefined (String)
  }
derive instance newtypeClusterNode :: Newtype ClusterNode _


newtype ClusterNodesList = ClusterNodesList (Array ClusterNode)
derive instance newtypeClusterNodesList :: Newtype ClusterNodesList _


-- | <p>The <code>ClusterIdentifier</code> parameter does not refer to an existing cluster. </p>
newtype ClusterNotFoundFault = ClusterNotFoundFault 
  { 
  }
derive instance newtypeClusterNotFoundFault :: Newtype ClusterNotFoundFault _


-- | <p>Describes a parameter group.</p>
newtype ClusterParameterGroup = ClusterParameterGroup 
  { "ParameterGroupName" :: NullOrUndefined (String)
  , "ParameterGroupFamily" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeClusterParameterGroup :: Newtype ClusterParameterGroup _


-- | <p>A cluster parameter group with the same name already exists.</p>
newtype ClusterParameterGroupAlreadyExistsFault = ClusterParameterGroupAlreadyExistsFault 
  { 
  }
derive instance newtypeClusterParameterGroupAlreadyExistsFault :: Newtype ClusterParameterGroupAlreadyExistsFault _


-- | <p>Contains the output from the <a>DescribeClusterParameters</a> action. </p>
newtype ClusterParameterGroupDetails = ClusterParameterGroupDetails 
  { "Parameters" :: NullOrUndefined (ParametersList)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeClusterParameterGroupDetails :: Newtype ClusterParameterGroupDetails _


-- | <p/>
newtype ClusterParameterGroupNameMessage = ClusterParameterGroupNameMessage 
  { "ParameterGroupName" :: NullOrUndefined (String)
  , "ParameterGroupStatus" :: NullOrUndefined (String)
  }
derive instance newtypeClusterParameterGroupNameMessage :: Newtype ClusterParameterGroupNameMessage _


-- | <p>The parameter group name does not refer to an existing parameter group.</p>
newtype ClusterParameterGroupNotFoundFault = ClusterParameterGroupNotFoundFault 
  { 
  }
derive instance newtypeClusterParameterGroupNotFoundFault :: Newtype ClusterParameterGroupNotFoundFault _


-- | <p>The request would result in the user exceeding the allowed number of cluster parameter groups. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype ClusterParameterGroupQuotaExceededFault = ClusterParameterGroupQuotaExceededFault 
  { 
  }
derive instance newtypeClusterParameterGroupQuotaExceededFault :: Newtype ClusterParameterGroupQuotaExceededFault _


-- | <p>Describes the status of a parameter group.</p>
newtype ClusterParameterGroupStatus = ClusterParameterGroupStatus 
  { "ParameterGroupName" :: NullOrUndefined (String)
  , "ParameterApplyStatus" :: NullOrUndefined (String)
  , "ClusterParameterStatusList" :: NullOrUndefined (ClusterParameterStatusList)
  }
derive instance newtypeClusterParameterGroupStatus :: Newtype ClusterParameterGroupStatus _


newtype ClusterParameterGroupStatusList = ClusterParameterGroupStatusList (Array ClusterParameterGroupStatus)
derive instance newtypeClusterParameterGroupStatusList :: Newtype ClusterParameterGroupStatusList _


-- | <p>Contains the output from the <a>DescribeClusterParameterGroups</a> action. </p>
newtype ClusterParameterGroupsMessage = ClusterParameterGroupsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ParameterGroups" :: NullOrUndefined (ParameterGroupList)
  }
derive instance newtypeClusterParameterGroupsMessage :: Newtype ClusterParameterGroupsMessage _


-- | <p>Describes the status of a parameter group.</p>
newtype ClusterParameterStatus = ClusterParameterStatus 
  { "ParameterName" :: NullOrUndefined (String)
  , "ParameterApplyStatus" :: NullOrUndefined (String)
  , "ParameterApplyErrorDescription" :: NullOrUndefined (String)
  }
derive instance newtypeClusterParameterStatus :: Newtype ClusterParameterStatus _


newtype ClusterParameterStatusList = ClusterParameterStatusList (Array ClusterParameterStatus)
derive instance newtypeClusterParameterStatusList :: Newtype ClusterParameterStatusList _


-- | <p>The request would exceed the allowed number of cluster instances for this account. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype ClusterQuotaExceededFault = ClusterQuotaExceededFault 
  { 
  }
derive instance newtypeClusterQuotaExceededFault :: Newtype ClusterQuotaExceededFault _


-- | <p>Describes a security group.</p>
newtype ClusterSecurityGroup = ClusterSecurityGroup 
  { "ClusterSecurityGroupName" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "EC2SecurityGroups" :: NullOrUndefined (EC2SecurityGroupList)
  , "IPRanges" :: NullOrUndefined (IPRangeList)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeClusterSecurityGroup :: Newtype ClusterSecurityGroup _


-- | <p>A cluster security group with the same name already exists.</p>
newtype ClusterSecurityGroupAlreadyExistsFault = ClusterSecurityGroupAlreadyExistsFault 
  { 
  }
derive instance newtypeClusterSecurityGroupAlreadyExistsFault :: Newtype ClusterSecurityGroupAlreadyExistsFault _


-- | <p>Describes a cluster security group.</p>
newtype ClusterSecurityGroupMembership = ClusterSecurityGroupMembership 
  { "ClusterSecurityGroupName" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }
derive instance newtypeClusterSecurityGroupMembership :: Newtype ClusterSecurityGroupMembership _


newtype ClusterSecurityGroupMembershipList = ClusterSecurityGroupMembershipList (Array ClusterSecurityGroupMembership)
derive instance newtypeClusterSecurityGroupMembershipList :: Newtype ClusterSecurityGroupMembershipList _


-- | <p/>
newtype ClusterSecurityGroupMessage = ClusterSecurityGroupMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ClusterSecurityGroups" :: NullOrUndefined (ClusterSecurityGroups)
  }
derive instance newtypeClusterSecurityGroupMessage :: Newtype ClusterSecurityGroupMessage _


newtype ClusterSecurityGroupNameList = ClusterSecurityGroupNameList (Array String)
derive instance newtypeClusterSecurityGroupNameList :: Newtype ClusterSecurityGroupNameList _


-- | <p>The cluster security group name does not refer to an existing cluster security group.</p>
newtype ClusterSecurityGroupNotFoundFault = ClusterSecurityGroupNotFoundFault 
  { 
  }
derive instance newtypeClusterSecurityGroupNotFoundFault :: Newtype ClusterSecurityGroupNotFoundFault _


-- | <p>The request would result in the user exceeding the allowed number of cluster security groups. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype ClusterSecurityGroupQuotaExceededFault = ClusterSecurityGroupQuotaExceededFault 
  { 
  }
derive instance newtypeClusterSecurityGroupQuotaExceededFault :: Newtype ClusterSecurityGroupQuotaExceededFault _


newtype ClusterSecurityGroups = ClusterSecurityGroups (Array ClusterSecurityGroup)
derive instance newtypeClusterSecurityGroups :: Newtype ClusterSecurityGroups _


-- | <p>The value specified as a snapshot identifier is already used by an existing snapshot.</p>
newtype ClusterSnapshotAlreadyExistsFault = ClusterSnapshotAlreadyExistsFault 
  { 
  }
derive instance newtypeClusterSnapshotAlreadyExistsFault :: Newtype ClusterSnapshotAlreadyExistsFault _


-- | <p>Returns the destination region and retention period that are configured for cross-region snapshot copy.</p>
newtype ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus 
  { "DestinationRegion" :: NullOrUndefined (String)
  , "RetentionPeriod" :: NullOrUndefined (Number)
  , "SnapshotCopyGrantName" :: NullOrUndefined (String)
  }
derive instance newtypeClusterSnapshotCopyStatus :: Newtype ClusterSnapshotCopyStatus _


-- | <p>The snapshot identifier does not refer to an existing cluster snapshot.</p>
newtype ClusterSnapshotNotFoundFault = ClusterSnapshotNotFoundFault 
  { 
  }
derive instance newtypeClusterSnapshotNotFoundFault :: Newtype ClusterSnapshotNotFoundFault _


-- | <p>The request would result in the user exceeding the allowed number of cluster snapshots.</p>
newtype ClusterSnapshotQuotaExceededFault = ClusterSnapshotQuotaExceededFault 
  { 
  }
derive instance newtypeClusterSnapshotQuotaExceededFault :: Newtype ClusterSnapshotQuotaExceededFault _


-- | <p>Describes a subnet group.</p>
newtype ClusterSubnetGroup = ClusterSubnetGroup 
  { "ClusterSubnetGroupName" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "SubnetGroupStatus" :: NullOrUndefined (String)
  , "Subnets" :: NullOrUndefined (SubnetList)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeClusterSubnetGroup :: Newtype ClusterSubnetGroup _


-- | <p>A <i>ClusterSubnetGroupName</i> is already used by an existing cluster subnet group. </p>
newtype ClusterSubnetGroupAlreadyExistsFault = ClusterSubnetGroupAlreadyExistsFault 
  { 
  }
derive instance newtypeClusterSubnetGroupAlreadyExistsFault :: Newtype ClusterSubnetGroupAlreadyExistsFault _


-- | <p>Contains the output from the <a>DescribeClusterSubnetGroups</a> action. </p>
newtype ClusterSubnetGroupMessage = ClusterSubnetGroupMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ClusterSubnetGroups" :: NullOrUndefined (ClusterSubnetGroups)
  }
derive instance newtypeClusterSubnetGroupMessage :: Newtype ClusterSubnetGroupMessage _


-- | <p>The cluster subnet group name does not refer to an existing cluster subnet group.</p>
newtype ClusterSubnetGroupNotFoundFault = ClusterSubnetGroupNotFoundFault 
  { 
  }
derive instance newtypeClusterSubnetGroupNotFoundFault :: Newtype ClusterSubnetGroupNotFoundFault _


-- | <p>The request would result in user exceeding the allowed number of cluster subnet groups. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype ClusterSubnetGroupQuotaExceededFault = ClusterSubnetGroupQuotaExceededFault 
  { 
  }
derive instance newtypeClusterSubnetGroupQuotaExceededFault :: Newtype ClusterSubnetGroupQuotaExceededFault _


newtype ClusterSubnetGroups = ClusterSubnetGroups (Array ClusterSubnetGroup)
derive instance newtypeClusterSubnetGroups :: Newtype ClusterSubnetGroups _


-- | <p>The request would result in user exceeding the allowed number of subnets in a cluster subnet groups. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype ClusterSubnetQuotaExceededFault = ClusterSubnetQuotaExceededFault 
  { 
  }
derive instance newtypeClusterSubnetQuotaExceededFault :: Newtype ClusterSubnetQuotaExceededFault _


-- | <p>Describes a cluster version, including the parameter group family and description of the version.</p>
newtype ClusterVersion = ClusterVersion 
  { "ClusterVersion" :: NullOrUndefined (String)
  , "ClusterParameterGroupFamily" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  }
derive instance newtypeClusterVersion :: Newtype ClusterVersion _


newtype ClusterVersionList = ClusterVersionList (Array ClusterVersion)
derive instance newtypeClusterVersionList :: Newtype ClusterVersionList _


-- | <p>Contains the output from the <a>DescribeClusterVersions</a> action. </p>
newtype ClusterVersionsMessage = ClusterVersionsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ClusterVersions" :: NullOrUndefined (ClusterVersionList)
  }
derive instance newtypeClusterVersionsMessage :: Newtype ClusterVersionsMessage _


-- | <p>Contains the output from the <a>DescribeClusters</a> action. </p>
newtype ClustersMessage = ClustersMessage 
  { "Marker" :: NullOrUndefined (String)
  , "Clusters" :: NullOrUndefined (ClusterList)
  }
derive instance newtypeClustersMessage :: Newtype ClustersMessage _


-- | <p/>
newtype CopyClusterSnapshotMessage = CopyClusterSnapshotMessage 
  { "SourceSnapshotIdentifier" :: (String)
  , "SourceSnapshotClusterIdentifier" :: NullOrUndefined (String)
  , "TargetSnapshotIdentifier" :: (String)
  }
derive instance newtypeCopyClusterSnapshotMessage :: Newtype CopyClusterSnapshotMessage _


newtype CopyClusterSnapshotResult = CopyClusterSnapshotResult 
  { "Snapshot" :: NullOrUndefined (Snapshot)
  }
derive instance newtypeCopyClusterSnapshotResult :: Newtype CopyClusterSnapshotResult _


-- | <p>Cross-region snapshot copy was temporarily disabled. Try your request again.</p>
newtype CopyToRegionDisabledFault = CopyToRegionDisabledFault 
  { 
  }
derive instance newtypeCopyToRegionDisabledFault :: Newtype CopyToRegionDisabledFault _


-- | <p/>
newtype CreateClusterMessage = CreateClusterMessage 
  { "DBName" :: NullOrUndefined (String)
  , "ClusterIdentifier" :: (String)
  , "ClusterType" :: NullOrUndefined (String)
  , "NodeType" :: (String)
  , "MasterUsername" :: (String)
  , "MasterUserPassword" :: (String)
  , "ClusterSecurityGroups" :: NullOrUndefined (ClusterSecurityGroupNameList)
  , "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList)
  , "ClusterSubnetGroupName" :: NullOrUndefined (String)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "ClusterParameterGroupName" :: NullOrUndefined (String)
  , "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined (IntegerOptional)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "ClusterVersion" :: NullOrUndefined (String)
  , "AllowVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "NumberOfNodes" :: NullOrUndefined (IntegerOptional)
  , "PubliclyAccessible" :: NullOrUndefined (BooleanOptional)
  , "Encrypted" :: NullOrUndefined (BooleanOptional)
  , "HsmClientCertificateIdentifier" :: NullOrUndefined (String)
  , "HsmConfigurationIdentifier" :: NullOrUndefined (String)
  , "ElasticIp" :: NullOrUndefined (String)
  , "Tags" :: NullOrUndefined (TagList)
  , "KmsKeyId" :: NullOrUndefined (String)
  , "EnhancedVpcRouting" :: NullOrUndefined (BooleanOptional)
  , "AdditionalInfo" :: NullOrUndefined (String)
  , "IamRoles" :: NullOrUndefined (IamRoleArnList)
  }
derive instance newtypeCreateClusterMessage :: Newtype CreateClusterMessage _


-- | <p/>
newtype CreateClusterParameterGroupMessage = CreateClusterParameterGroupMessage 
  { "ParameterGroupName" :: (String)
  , "ParameterGroupFamily" :: (String)
  , "Description" :: (String)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateClusterParameterGroupMessage :: Newtype CreateClusterParameterGroupMessage _


newtype CreateClusterParameterGroupResult = CreateClusterParameterGroupResult 
  { "ClusterParameterGroup" :: NullOrUndefined (ClusterParameterGroup)
  }
derive instance newtypeCreateClusterParameterGroupResult :: Newtype CreateClusterParameterGroupResult _


newtype CreateClusterResult = CreateClusterResult 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeCreateClusterResult :: Newtype CreateClusterResult _


-- | <p/>
newtype CreateClusterSecurityGroupMessage = CreateClusterSecurityGroupMessage 
  { "ClusterSecurityGroupName" :: (String)
  , "Description" :: (String)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateClusterSecurityGroupMessage :: Newtype CreateClusterSecurityGroupMessage _


newtype CreateClusterSecurityGroupResult = CreateClusterSecurityGroupResult 
  { "ClusterSecurityGroup" :: NullOrUndefined (ClusterSecurityGroup)
  }
derive instance newtypeCreateClusterSecurityGroupResult :: Newtype CreateClusterSecurityGroupResult _


-- | <p/>
newtype CreateClusterSnapshotMessage = CreateClusterSnapshotMessage 
  { "SnapshotIdentifier" :: (String)
  , "ClusterIdentifier" :: (String)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateClusterSnapshotMessage :: Newtype CreateClusterSnapshotMessage _


newtype CreateClusterSnapshotResult = CreateClusterSnapshotResult 
  { "Snapshot" :: NullOrUndefined (Snapshot)
  }
derive instance newtypeCreateClusterSnapshotResult :: Newtype CreateClusterSnapshotResult _


-- | <p/>
newtype CreateClusterSubnetGroupMessage = CreateClusterSubnetGroupMessage 
  { "ClusterSubnetGroupName" :: (String)
  , "Description" :: (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateClusterSubnetGroupMessage :: Newtype CreateClusterSubnetGroupMessage _


newtype CreateClusterSubnetGroupResult = CreateClusterSubnetGroupResult 
  { "ClusterSubnetGroup" :: NullOrUndefined (ClusterSubnetGroup)
  }
derive instance newtypeCreateClusterSubnetGroupResult :: Newtype CreateClusterSubnetGroupResult _


-- | <p/>
newtype CreateEventSubscriptionMessage = CreateEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SnsTopicArn" :: (String)
  , "SourceType" :: NullOrUndefined (String)
  , "SourceIds" :: NullOrUndefined (SourceIdsList)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "Severity" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (BooleanOptional)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateEventSubscriptionMessage :: Newtype CreateEventSubscriptionMessage _


newtype CreateEventSubscriptionResult = CreateEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }
derive instance newtypeCreateEventSubscriptionResult :: Newtype CreateEventSubscriptionResult _


-- | <p/>
newtype CreateHsmClientCertificateMessage = CreateHsmClientCertificateMessage 
  { "HsmClientCertificateIdentifier" :: (String)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateHsmClientCertificateMessage :: Newtype CreateHsmClientCertificateMessage _


newtype CreateHsmClientCertificateResult = CreateHsmClientCertificateResult 
  { "HsmClientCertificate" :: NullOrUndefined (HsmClientCertificate)
  }
derive instance newtypeCreateHsmClientCertificateResult :: Newtype CreateHsmClientCertificateResult _


-- | <p/>
newtype CreateHsmConfigurationMessage = CreateHsmConfigurationMessage 
  { "HsmConfigurationIdentifier" :: (String)
  , "Description" :: (String)
  , "HsmIpAddress" :: (String)
  , "HsmPartitionName" :: (String)
  , "HsmPartitionPassword" :: (String)
  , "HsmServerPublicCertificate" :: (String)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateHsmConfigurationMessage :: Newtype CreateHsmConfigurationMessage _


newtype CreateHsmConfigurationResult = CreateHsmConfigurationResult 
  { "HsmConfiguration" :: NullOrUndefined (HsmConfiguration)
  }
derive instance newtypeCreateHsmConfigurationResult :: Newtype CreateHsmConfigurationResult _


-- | <p>The result of the <code>CreateSnapshotCopyGrant</code> action.</p>
newtype CreateSnapshotCopyGrantMessage = CreateSnapshotCopyGrantMessage 
  { "SnapshotCopyGrantName" :: (String)
  , "KmsKeyId" :: NullOrUndefined (String)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateSnapshotCopyGrantMessage :: Newtype CreateSnapshotCopyGrantMessage _


newtype CreateSnapshotCopyGrantResult = CreateSnapshotCopyGrantResult 
  { "SnapshotCopyGrant" :: NullOrUndefined (SnapshotCopyGrant)
  }
derive instance newtypeCreateSnapshotCopyGrantResult :: Newtype CreateSnapshotCopyGrantResult _


-- | <p>Contains the output from the <code>CreateTags</code> action. </p>
newtype CreateTagsMessage = CreateTagsMessage 
  { "ResourceName" :: (String)
  , "Tags" :: (TagList)
  }
derive instance newtypeCreateTagsMessage :: Newtype CreateTagsMessage _


newtype DbGroupList = DbGroupList (Array String)
derive instance newtypeDbGroupList :: Newtype DbGroupList _


-- | <p>Describes the default cluster parameters for a parameter group family.</p>
newtype DefaultClusterParameters = DefaultClusterParameters 
  { "ParameterGroupFamily" :: NullOrUndefined (String)
  , "Marker" :: NullOrUndefined (String)
  , "Parameters" :: NullOrUndefined (ParametersList)
  }
derive instance newtypeDefaultClusterParameters :: Newtype DefaultClusterParameters _


-- | <p/>
newtype DeleteClusterMessage = DeleteClusterMessage 
  { "ClusterIdentifier" :: (String)
  , "SkipFinalClusterSnapshot" :: NullOrUndefined (Boolean)
  , "FinalClusterSnapshotIdentifier" :: NullOrUndefined (String)
  }
derive instance newtypeDeleteClusterMessage :: Newtype DeleteClusterMessage _


-- | <p/>
newtype DeleteClusterParameterGroupMessage = DeleteClusterParameterGroupMessage 
  { "ParameterGroupName" :: (String)
  }
derive instance newtypeDeleteClusterParameterGroupMessage :: Newtype DeleteClusterParameterGroupMessage _


newtype DeleteClusterResult = DeleteClusterResult 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeDeleteClusterResult :: Newtype DeleteClusterResult _


-- | <p/>
newtype DeleteClusterSecurityGroupMessage = DeleteClusterSecurityGroupMessage 
  { "ClusterSecurityGroupName" :: (String)
  }
derive instance newtypeDeleteClusterSecurityGroupMessage :: Newtype DeleteClusterSecurityGroupMessage _


-- | <p/>
newtype DeleteClusterSnapshotMessage = DeleteClusterSnapshotMessage 
  { "SnapshotIdentifier" :: (String)
  , "SnapshotClusterIdentifier" :: NullOrUndefined (String)
  }
derive instance newtypeDeleteClusterSnapshotMessage :: Newtype DeleteClusterSnapshotMessage _


newtype DeleteClusterSnapshotResult = DeleteClusterSnapshotResult 
  { "Snapshot" :: NullOrUndefined (Snapshot)
  }
derive instance newtypeDeleteClusterSnapshotResult :: Newtype DeleteClusterSnapshotResult _


-- | <p/>
newtype DeleteClusterSubnetGroupMessage = DeleteClusterSubnetGroupMessage 
  { "ClusterSubnetGroupName" :: (String)
  }
derive instance newtypeDeleteClusterSubnetGroupMessage :: Newtype DeleteClusterSubnetGroupMessage _


-- | <p/>
newtype DeleteEventSubscriptionMessage = DeleteEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  }
derive instance newtypeDeleteEventSubscriptionMessage :: Newtype DeleteEventSubscriptionMessage _


-- | <p/>
newtype DeleteHsmClientCertificateMessage = DeleteHsmClientCertificateMessage 
  { "HsmClientCertificateIdentifier" :: (String)
  }
derive instance newtypeDeleteHsmClientCertificateMessage :: Newtype DeleteHsmClientCertificateMessage _


-- | <p/>
newtype DeleteHsmConfigurationMessage = DeleteHsmConfigurationMessage 
  { "HsmConfigurationIdentifier" :: (String)
  }
derive instance newtypeDeleteHsmConfigurationMessage :: Newtype DeleteHsmConfigurationMessage _


-- | <p>The result of the <code>DeleteSnapshotCopyGrant</code> action.</p>
newtype DeleteSnapshotCopyGrantMessage = DeleteSnapshotCopyGrantMessage 
  { "SnapshotCopyGrantName" :: (String)
  }
derive instance newtypeDeleteSnapshotCopyGrantMessage :: Newtype DeleteSnapshotCopyGrantMessage _


-- | <p>Contains the output from the <code>DeleteTags</code> action. </p>
newtype DeleteTagsMessage = DeleteTagsMessage 
  { "ResourceName" :: (String)
  , "TagKeys" :: (TagKeyList)
  }
derive instance newtypeDeleteTagsMessage :: Newtype DeleteTagsMessage _


-- | <p>The request cannot be completed because a dependent service is throttling requests made by Amazon Redshift on your behalf. Wait and retry the request.</p>
newtype DependentServiceRequestThrottlingFault = DependentServiceRequestThrottlingFault 
  { 
  }
derive instance newtypeDependentServiceRequestThrottlingFault :: Newtype DependentServiceRequestThrottlingFault _


-- | <p>Your request cannot be completed because a dependent internal service is temporarily unavailable. Wait 30 to 60 seconds and try again.</p>
newtype DependentServiceUnavailableFault = DependentServiceUnavailableFault 
  { 
  }
derive instance newtypeDependentServiceUnavailableFault :: Newtype DependentServiceUnavailableFault _


-- | <p/>
newtype DescribeClusterParameterGroupsMessage = DescribeClusterParameterGroupsMessage 
  { "ParameterGroupName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeClusterParameterGroupsMessage :: Newtype DescribeClusterParameterGroupsMessage _


-- | <p/>
newtype DescribeClusterParametersMessage = DescribeClusterParametersMessage 
  { "ParameterGroupName" :: (String)
  , "Source" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeClusterParametersMessage :: Newtype DescribeClusterParametersMessage _


-- | <p/>
newtype DescribeClusterSecurityGroupsMessage = DescribeClusterSecurityGroupsMessage 
  { "ClusterSecurityGroupName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeClusterSecurityGroupsMessage :: Newtype DescribeClusterSecurityGroupsMessage _


-- | <p/>
newtype DescribeClusterSnapshotsMessage = DescribeClusterSnapshotsMessage 
  { "ClusterIdentifier" :: NullOrUndefined (String)
  , "SnapshotIdentifier" :: NullOrUndefined (String)
  , "SnapshotType" :: NullOrUndefined (String)
  , "StartTime" :: NullOrUndefined (TStamp)
  , "EndTime" :: NullOrUndefined (TStamp)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "OwnerAccount" :: NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeClusterSnapshotsMessage :: Newtype DescribeClusterSnapshotsMessage _


-- | <p/>
newtype DescribeClusterSubnetGroupsMessage = DescribeClusterSubnetGroupsMessage 
  { "ClusterSubnetGroupName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeClusterSubnetGroupsMessage :: Newtype DescribeClusterSubnetGroupsMessage _


-- | <p/>
newtype DescribeClusterVersionsMessage = DescribeClusterVersionsMessage 
  { "ClusterVersion" :: NullOrUndefined (String)
  , "ClusterParameterGroupFamily" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeClusterVersionsMessage :: Newtype DescribeClusterVersionsMessage _


-- | <p/>
newtype DescribeClustersMessage = DescribeClustersMessage 
  { "ClusterIdentifier" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeClustersMessage :: Newtype DescribeClustersMessage _


-- | <p/>
newtype DescribeDefaultClusterParametersMessage = DescribeDefaultClusterParametersMessage 
  { "ParameterGroupFamily" :: (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeDefaultClusterParametersMessage :: Newtype DescribeDefaultClusterParametersMessage _


newtype DescribeDefaultClusterParametersResult = DescribeDefaultClusterParametersResult 
  { "DefaultClusterParameters" :: NullOrUndefined (DefaultClusterParameters)
  }
derive instance newtypeDescribeDefaultClusterParametersResult :: Newtype DescribeDefaultClusterParametersResult _


-- | <p/>
newtype DescribeEventCategoriesMessage = DescribeEventCategoriesMessage 
  { "SourceType" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEventCategoriesMessage :: Newtype DescribeEventCategoriesMessage _


-- | <p/>
newtype DescribeEventSubscriptionsMessage = DescribeEventSubscriptionsMessage 
  { "SubscriptionName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeEventSubscriptionsMessage :: Newtype DescribeEventSubscriptionsMessage _


-- | <p/>
newtype DescribeEventsMessage = DescribeEventsMessage 
  { "SourceIdentifier" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (SourceType)
  , "StartTime" :: NullOrUndefined (TStamp)
  , "EndTime" :: NullOrUndefined (TStamp)
  , "Duration" :: NullOrUndefined (IntegerOptional)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEventsMessage :: Newtype DescribeEventsMessage _


-- | <p/>
newtype DescribeHsmClientCertificatesMessage = DescribeHsmClientCertificatesMessage 
  { "HsmClientCertificateIdentifier" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeHsmClientCertificatesMessage :: Newtype DescribeHsmClientCertificatesMessage _


-- | <p/>
newtype DescribeHsmConfigurationsMessage = DescribeHsmConfigurationsMessage 
  { "HsmConfigurationIdentifier" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeHsmConfigurationsMessage :: Newtype DescribeHsmConfigurationsMessage _


-- | <p/>
newtype DescribeLoggingStatusMessage = DescribeLoggingStatusMessage 
  { "ClusterIdentifier" :: (String)
  }
derive instance newtypeDescribeLoggingStatusMessage :: Newtype DescribeLoggingStatusMessage _


-- | <p/>
newtype DescribeOrderableClusterOptionsMessage = DescribeOrderableClusterOptionsMessage 
  { "ClusterVersion" :: NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeOrderableClusterOptionsMessage :: Newtype DescribeOrderableClusterOptionsMessage _


-- | <p/>
newtype DescribeReservedNodeOfferingsMessage = DescribeReservedNodeOfferingsMessage 
  { "ReservedNodeOfferingId" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeReservedNodeOfferingsMessage :: Newtype DescribeReservedNodeOfferingsMessage _


-- | <p/>
newtype DescribeReservedNodesMessage = DescribeReservedNodesMessage 
  { "ReservedNodeId" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeReservedNodesMessage :: Newtype DescribeReservedNodesMessage _


-- | <p/>
newtype DescribeResizeMessage = DescribeResizeMessage 
  { "ClusterIdentifier" :: (String)
  }
derive instance newtypeDescribeResizeMessage :: Newtype DescribeResizeMessage _


-- | <p>The result of the <code>DescribeSnapshotCopyGrants</code> action.</p>
newtype DescribeSnapshotCopyGrantsMessage = DescribeSnapshotCopyGrantsMessage 
  { "SnapshotCopyGrantName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeSnapshotCopyGrantsMessage :: Newtype DescribeSnapshotCopyGrantsMessage _


-- | <p/>
newtype DescribeTableRestoreStatusMessage = DescribeTableRestoreStatusMessage 
  { "ClusterIdentifier" :: NullOrUndefined (String)
  , "TableRestoreRequestId" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeTableRestoreStatusMessage :: Newtype DescribeTableRestoreStatusMessage _


-- | <p/>
newtype DescribeTagsMessage = DescribeTagsMessage 
  { "ResourceName" :: NullOrUndefined (String)
  , "ResourceType" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeTagsMessage :: Newtype DescribeTagsMessage _


-- | <p/>
newtype DisableLoggingMessage = DisableLoggingMessage 
  { "ClusterIdentifier" :: (String)
  }
derive instance newtypeDisableLoggingMessage :: Newtype DisableLoggingMessage _


-- | <p/>
newtype DisableSnapshotCopyMessage = DisableSnapshotCopyMessage 
  { "ClusterIdentifier" :: (String)
  }
derive instance newtypeDisableSnapshotCopyMessage :: Newtype DisableSnapshotCopyMessage _


newtype DisableSnapshotCopyResult = DisableSnapshotCopyResult 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeDisableSnapshotCopyResult :: Newtype DisableSnapshotCopyResult _


newtype DoubleOptional = DoubleOptional Number
derive instance newtypeDoubleOptional :: Newtype DoubleOptional _


-- | <p>Describes an Amazon EC2 security group.</p>
newtype EC2SecurityGroup = EC2SecurityGroup 
  { "Status" :: NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined (String)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeEC2SecurityGroup :: Newtype EC2SecurityGroup _


newtype EC2SecurityGroupList = EC2SecurityGroupList (Array EC2SecurityGroup)
derive instance newtypeEC2SecurityGroupList :: Newtype EC2SecurityGroupList _


-- | <p>Describes the status of the elastic IP (EIP) address.</p>
newtype ElasticIpStatus = ElasticIpStatus 
  { "ElasticIp" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }
derive instance newtypeElasticIpStatus :: Newtype ElasticIpStatus _


-- | <p/>
newtype EnableLoggingMessage = EnableLoggingMessage 
  { "ClusterIdentifier" :: (String)
  , "BucketName" :: (String)
  , "S3KeyPrefix" :: NullOrUndefined (String)
  }
derive instance newtypeEnableLoggingMessage :: Newtype EnableLoggingMessage _


-- | <p/>
newtype EnableSnapshotCopyMessage = EnableSnapshotCopyMessage 
  { "ClusterIdentifier" :: (String)
  , "DestinationRegion" :: (String)
  , "RetentionPeriod" :: NullOrUndefined (IntegerOptional)
  , "SnapshotCopyGrantName" :: NullOrUndefined (String)
  }
derive instance newtypeEnableSnapshotCopyMessage :: Newtype EnableSnapshotCopyMessage _


newtype EnableSnapshotCopyResult = EnableSnapshotCopyResult 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeEnableSnapshotCopyResult :: Newtype EnableSnapshotCopyResult _


-- | <p>Describes a connection endpoint.</p>
newtype Endpoint = Endpoint 
  { "Address" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (Int)
  }
derive instance newtypeEndpoint :: Newtype Endpoint _


-- | <p>Describes an event.</p>
newtype Event = Event 
  { "SourceIdentifier" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (SourceType)
  , "Message" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "Severity" :: NullOrUndefined (String)
  , "Date" :: NullOrUndefined (TStamp)
  , "EventId" :: NullOrUndefined (String)
  }
derive instance newtypeEvent :: Newtype Event _


newtype EventCategoriesList = EventCategoriesList (Array String)
derive instance newtypeEventCategoriesList :: Newtype EventCategoriesList _


-- | <p>Describes event categories.</p>
newtype EventCategoriesMap = EventCategoriesMap 
  { "SourceType" :: NullOrUndefined (String)
  , "Events" :: NullOrUndefined (EventInfoMapList)
  }
derive instance newtypeEventCategoriesMap :: Newtype EventCategoriesMap _


newtype EventCategoriesMapList = EventCategoriesMapList (Array EventCategoriesMap)
derive instance newtypeEventCategoriesMapList :: Newtype EventCategoriesMapList _


-- | <p/>
newtype EventCategoriesMessage = EventCategoriesMessage 
  { "EventCategoriesMapList" :: NullOrUndefined (EventCategoriesMapList)
  }
derive instance newtypeEventCategoriesMessage :: Newtype EventCategoriesMessage _


-- | <p>Describes event information.</p>
newtype EventInfoMap = EventInfoMap 
  { "EventId" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "EventDescription" :: NullOrUndefined (String)
  , "Severity" :: NullOrUndefined (String)
  }
derive instance newtypeEventInfoMap :: Newtype EventInfoMap _


newtype EventInfoMapList = EventInfoMapList (Array EventInfoMap)
derive instance newtypeEventInfoMapList :: Newtype EventInfoMapList _


newtype EventList = EventList (Array Event)
derive instance newtypeEventList :: Newtype EventList _


-- | <p>Describes event subscriptions.</p>
newtype EventSubscription = EventSubscription 
  { "CustomerAwsId" :: NullOrUndefined (String)
  , "CustSubscriptionId" :: NullOrUndefined (String)
  , "SnsTopicArn" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  , "SubscriptionCreationTime" :: NullOrUndefined (TStamp)
  , "SourceType" :: NullOrUndefined (String)
  , "SourceIdsList" :: NullOrUndefined (SourceIdsList)
  , "EventCategoriesList" :: NullOrUndefined (EventCategoriesList)
  , "Severity" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeEventSubscription :: Newtype EventSubscription _


-- | <p>The request would exceed the allowed number of event subscriptions for this account. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype EventSubscriptionQuotaExceededFault = EventSubscriptionQuotaExceededFault 
  { 
  }
derive instance newtypeEventSubscriptionQuotaExceededFault :: Newtype EventSubscriptionQuotaExceededFault _


newtype EventSubscriptionsList = EventSubscriptionsList (Array EventSubscription)
derive instance newtypeEventSubscriptionsList :: Newtype EventSubscriptionsList _


-- | <p/>
newtype EventSubscriptionsMessage = EventSubscriptionsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "EventSubscriptionsList" :: NullOrUndefined (EventSubscriptionsList)
  }
derive instance newtypeEventSubscriptionsMessage :: Newtype EventSubscriptionsMessage _


-- | <p/>
newtype EventsMessage = EventsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "Events" :: NullOrUndefined (EventList)
  }
derive instance newtypeEventsMessage :: Newtype EventsMessage _


-- | <p>The request parameters to get cluster credentials.</p>
newtype GetClusterCredentialsMessage = GetClusterCredentialsMessage 
  { "DbUser" :: (String)
  , "DbName" :: NullOrUndefined (String)
  , "ClusterIdentifier" :: (String)
  , "DurationSeconds" :: NullOrUndefined (IntegerOptional)
  , "AutoCreate" :: NullOrUndefined (BooleanOptional)
  , "DbGroups" :: NullOrUndefined (DbGroupList)
  }
derive instance newtypeGetClusterCredentialsMessage :: Newtype GetClusterCredentialsMessage _


-- | <p>Returns information about an HSM client certificate. The certificate is stored in a secure Hardware Storage Module (HSM), and used by the Amazon Redshift cluster to encrypt data files.</p>
newtype HsmClientCertificate = HsmClientCertificate 
  { "HsmClientCertificateIdentifier" :: NullOrUndefined (String)
  , "HsmClientCertificatePublicKey" :: NullOrUndefined (String)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeHsmClientCertificate :: Newtype HsmClientCertificate _


-- | <p>There is already an existing Amazon Redshift HSM client certificate with the specified identifier.</p>
newtype HsmClientCertificateAlreadyExistsFault = HsmClientCertificateAlreadyExistsFault 
  { 
  }
derive instance newtypeHsmClientCertificateAlreadyExistsFault :: Newtype HsmClientCertificateAlreadyExistsFault _


newtype HsmClientCertificateList = HsmClientCertificateList (Array HsmClientCertificate)
derive instance newtypeHsmClientCertificateList :: Newtype HsmClientCertificateList _


-- | <p/>
newtype HsmClientCertificateMessage = HsmClientCertificateMessage 
  { "Marker" :: NullOrUndefined (String)
  , "HsmClientCertificates" :: NullOrUndefined (HsmClientCertificateList)
  }
derive instance newtypeHsmClientCertificateMessage :: Newtype HsmClientCertificateMessage _


-- | <p>There is no Amazon Redshift HSM client certificate with the specified identifier.</p>
newtype HsmClientCertificateNotFoundFault = HsmClientCertificateNotFoundFault 
  { 
  }
derive instance newtypeHsmClientCertificateNotFoundFault :: Newtype HsmClientCertificateNotFoundFault _


-- | <p>The quota for HSM client certificates has been reached. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype HsmClientCertificateQuotaExceededFault = HsmClientCertificateQuotaExceededFault 
  { 
  }
derive instance newtypeHsmClientCertificateQuotaExceededFault :: Newtype HsmClientCertificateQuotaExceededFault _


-- | <p>Returns information about an HSM configuration, which is an object that describes to Amazon Redshift clusters the information they require to connect to an HSM where they can store database encryption keys.</p>
newtype HsmConfiguration = HsmConfiguration 
  { "HsmConfigurationIdentifier" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HsmIpAddress" :: NullOrUndefined (String)
  , "HsmPartitionName" :: NullOrUndefined (String)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeHsmConfiguration :: Newtype HsmConfiguration _


-- | <p>There is already an existing Amazon Redshift HSM configuration with the specified identifier.</p>
newtype HsmConfigurationAlreadyExistsFault = HsmConfigurationAlreadyExistsFault 
  { 
  }
derive instance newtypeHsmConfigurationAlreadyExistsFault :: Newtype HsmConfigurationAlreadyExistsFault _


newtype HsmConfigurationList = HsmConfigurationList (Array HsmConfiguration)
derive instance newtypeHsmConfigurationList :: Newtype HsmConfigurationList _


-- | <p/>
newtype HsmConfigurationMessage = HsmConfigurationMessage 
  { "Marker" :: NullOrUndefined (String)
  , "HsmConfigurations" :: NullOrUndefined (HsmConfigurationList)
  }
derive instance newtypeHsmConfigurationMessage :: Newtype HsmConfigurationMessage _


-- | <p>There is no Amazon Redshift HSM configuration with the specified identifier.</p>
newtype HsmConfigurationNotFoundFault = HsmConfigurationNotFoundFault 
  { 
  }
derive instance newtypeHsmConfigurationNotFoundFault :: Newtype HsmConfigurationNotFoundFault _


-- | <p>The quota for HSM configurations has been reached. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype HsmConfigurationQuotaExceededFault = HsmConfigurationQuotaExceededFault 
  { 
  }
derive instance newtypeHsmConfigurationQuotaExceededFault :: Newtype HsmConfigurationQuotaExceededFault _


-- | <p>Describes the status of changes to HSM settings.</p>
newtype HsmStatus = HsmStatus 
  { "HsmClientCertificateIdentifier" :: NullOrUndefined (String)
  , "HsmConfigurationIdentifier" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }
derive instance newtypeHsmStatus :: Newtype HsmStatus _


-- | <p>Describes an IP range used in a security group.</p>
newtype IPRange = IPRange 
  { "Status" :: NullOrUndefined (String)
  , "CIDRIP" :: NullOrUndefined (String)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeIPRange :: Newtype IPRange _


newtype IPRangeList = IPRangeList (Array IPRange)
derive instance newtypeIPRangeList :: Newtype IPRangeList _


newtype IamRoleArnList = IamRoleArnList (Array String)
derive instance newtypeIamRoleArnList :: Newtype IamRoleArnList _


newtype ImportTablesCompleted = ImportTablesCompleted (Array String)
derive instance newtypeImportTablesCompleted :: Newtype ImportTablesCompleted _


newtype ImportTablesInProgress = ImportTablesInProgress (Array String)
derive instance newtypeImportTablesInProgress :: Newtype ImportTablesInProgress _


newtype ImportTablesNotStarted = ImportTablesNotStarted (Array String)
derive instance newtypeImportTablesNotStarted :: Newtype ImportTablesNotStarted _


-- | <p>You have exceeded the allowed number of table restore requests. Wait for your current table restore requests to complete before making a new request.</p>
newtype InProgressTableRestoreQuotaExceededFault = InProgressTableRestoreQuotaExceededFault 
  { 
  }
derive instance newtypeInProgressTableRestoreQuotaExceededFault :: Newtype InProgressTableRestoreQuotaExceededFault _


-- | <p>The specified options are incompatible.</p>
newtype IncompatibleOrderableOptions = IncompatibleOrderableOptions 
  { 
  }
derive instance newtypeIncompatibleOrderableOptions :: Newtype IncompatibleOrderableOptions _


-- | <p>The number of nodes specified exceeds the allotted capacity of the cluster.</p>
newtype InsufficientClusterCapacityFault = InsufficientClusterCapacityFault 
  { 
  }
derive instance newtypeInsufficientClusterCapacityFault :: Newtype InsufficientClusterCapacityFault _


-- | <p>The cluster does not have read bucket or put object permissions on the S3 bucket specified when enabling logging.</p>
newtype InsufficientS3BucketPolicyFault = InsufficientS3BucketPolicyFault 
  { 
  }
derive instance newtypeInsufficientS3BucketPolicyFault :: Newtype InsufficientS3BucketPolicyFault _


newtype IntegerOptional = IntegerOptional Int
derive instance newtypeIntegerOptional :: Newtype IntegerOptional _


-- | <p>The cluster parameter group action can not be completed because another task is in progress that involves the parameter group. Wait a few moments and try the operation again.</p>
newtype InvalidClusterParameterGroupStateFault = InvalidClusterParameterGroupStateFault 
  { 
  }
derive instance newtypeInvalidClusterParameterGroupStateFault :: Newtype InvalidClusterParameterGroupStateFault _


-- | <p>The state of the cluster security group is not <code>available</code>. </p>
newtype InvalidClusterSecurityGroupStateFault = InvalidClusterSecurityGroupStateFault 
  { 
  }
derive instance newtypeInvalidClusterSecurityGroupStateFault :: Newtype InvalidClusterSecurityGroupStateFault _


-- | <p>The specified cluster snapshot is not in the <code>available</code> state, or other accounts are authorized to access the snapshot. </p>
newtype InvalidClusterSnapshotStateFault = InvalidClusterSnapshotStateFault 
  { 
  }
derive instance newtypeInvalidClusterSnapshotStateFault :: Newtype InvalidClusterSnapshotStateFault _


-- | <p>The specified cluster is not in the <code>available</code> state. </p>
newtype InvalidClusterStateFault = InvalidClusterStateFault 
  { 
  }
derive instance newtypeInvalidClusterStateFault :: Newtype InvalidClusterStateFault _


-- | <p>The cluster subnet group cannot be deleted because it is in use.</p>
newtype InvalidClusterSubnetGroupStateFault = InvalidClusterSubnetGroupStateFault 
  { 
  }
derive instance newtypeInvalidClusterSubnetGroupStateFault :: Newtype InvalidClusterSubnetGroupStateFault _


-- | <p>The state of the subnet is invalid.</p>
newtype InvalidClusterSubnetStateFault = InvalidClusterSubnetStateFault 
  { 
  }
derive instance newtypeInvalidClusterSubnetStateFault :: Newtype InvalidClusterSubnetStateFault _


-- | <p>The Elastic IP (EIP) is invalid or cannot be found.</p>
newtype InvalidElasticIpFault = InvalidElasticIpFault 
  { 
  }
derive instance newtypeInvalidElasticIpFault :: Newtype InvalidElasticIpFault _


-- | <p>The specified HSM client certificate is not in the <code>available</code> state, or it is still in use by one or more Amazon Redshift clusters.</p>
newtype InvalidHsmClientCertificateStateFault = InvalidHsmClientCertificateStateFault 
  { 
  }
derive instance newtypeInvalidHsmClientCertificateStateFault :: Newtype InvalidHsmClientCertificateStateFault _


-- | <p>The specified HSM configuration is not in the <code>available</code> state, or it is still in use by one or more Amazon Redshift clusters.</p>
newtype InvalidHsmConfigurationStateFault = InvalidHsmConfigurationStateFault 
  { 
  }
derive instance newtypeInvalidHsmConfigurationStateFault :: Newtype InvalidHsmConfigurationStateFault _


-- | <p>The restore is invalid.</p>
newtype InvalidRestoreFault = InvalidRestoreFault 
  { 
  }
derive instance newtypeInvalidRestoreFault :: Newtype InvalidRestoreFault _


-- | <p>The S3 bucket name is invalid. For more information about naming rules, go to <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html">Bucket Restrictions and Limitations</a> in the Amazon Simple Storage Service (S3) Developer Guide.</p>
newtype InvalidS3BucketNameFault = InvalidS3BucketNameFault 
  { 
  }
derive instance newtypeInvalidS3BucketNameFault :: Newtype InvalidS3BucketNameFault _


-- | <p>The string specified for the logging S3 key prefix does not comply with the documented constraints.</p>
newtype InvalidS3KeyPrefixFault = InvalidS3KeyPrefixFault 
  { 
  }
derive instance newtypeInvalidS3KeyPrefixFault :: Newtype InvalidS3KeyPrefixFault _


-- | <p>The snapshot copy grant can't be deleted because it is used by one or more clusters.</p>
newtype InvalidSnapshotCopyGrantStateFault = InvalidSnapshotCopyGrantStateFault 
  { 
  }
derive instance newtypeInvalidSnapshotCopyGrantStateFault :: Newtype InvalidSnapshotCopyGrantStateFault _


-- | <p>The requested subnet is not valid, or not all of the subnets are in the same VPC.</p>
newtype InvalidSubnet = InvalidSubnet 
  { 
  }
derive instance newtypeInvalidSubnet :: Newtype InvalidSubnet _


-- | <p>The subscription request is invalid because it is a duplicate request. This subscription request is already in progress.</p>
newtype InvalidSubscriptionStateFault = InvalidSubscriptionStateFault 
  { 
  }
derive instance newtypeInvalidSubscriptionStateFault :: Newtype InvalidSubscriptionStateFault _


-- | <p>The value specified for the <code>sourceDatabaseName</code>, <code>sourceSchemaName</code>, or <code>sourceTableName</code> parameter, or a combination of these, doesn't exist in the snapshot.</p>
newtype InvalidTableRestoreArgumentFault = InvalidTableRestoreArgumentFault 
  { 
  }
derive instance newtypeInvalidTableRestoreArgumentFault :: Newtype InvalidTableRestoreArgumentFault _


-- | <p>The tag is invalid.</p>
newtype InvalidTagFault = InvalidTagFault 
  { 
  }
derive instance newtypeInvalidTagFault :: Newtype InvalidTagFault _


-- | <p>The cluster subnet group does not cover all Availability Zones.</p>
newtype InvalidVPCNetworkStateFault = InvalidVPCNetworkStateFault 
  { 
  }
derive instance newtypeInvalidVPCNetworkStateFault :: Newtype InvalidVPCNetworkStateFault _


-- | <p>The encryption key has exceeded its grant limit in AWS KMS.</p>
newtype LimitExceededFault = LimitExceededFault 
  { 
  }
derive instance newtypeLimitExceededFault :: Newtype LimitExceededFault _


-- | <p>Describes the status of logging for a cluster.</p>
newtype LoggingStatus = LoggingStatus 
  { "LoggingEnabled" :: NullOrUndefined (Boolean)
  , "BucketName" :: NullOrUndefined (String)
  , "S3KeyPrefix" :: NullOrUndefined (String)
  , "LastSuccessfulDeliveryTime" :: NullOrUndefined (TStamp)
  , "LastFailureTime" :: NullOrUndefined (TStamp)
  , "LastFailureMessage" :: NullOrUndefined (String)
  }
derive instance newtypeLoggingStatus :: Newtype LoggingStatus _


newtype LongOptional = LongOptional Number
derive instance newtypeLongOptional :: Newtype LongOptional _


-- | <p/>
newtype ModifyClusterIamRolesMessage = ModifyClusterIamRolesMessage 
  { "ClusterIdentifier" :: (String)
  , "AddIamRoles" :: NullOrUndefined (IamRoleArnList)
  , "RemoveIamRoles" :: NullOrUndefined (IamRoleArnList)
  }
derive instance newtypeModifyClusterIamRolesMessage :: Newtype ModifyClusterIamRolesMessage _


newtype ModifyClusterIamRolesResult = ModifyClusterIamRolesResult 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeModifyClusterIamRolesResult :: Newtype ModifyClusterIamRolesResult _


-- | <p/>
newtype ModifyClusterMessage = ModifyClusterMessage 
  { "ClusterIdentifier" :: (String)
  , "ClusterType" :: NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined (String)
  , "NumberOfNodes" :: NullOrUndefined (IntegerOptional)
  , "ClusterSecurityGroups" :: NullOrUndefined (ClusterSecurityGroupNameList)
  , "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList)
  , "MasterUserPassword" :: NullOrUndefined (String)
  , "ClusterParameterGroupName" :: NullOrUndefined (String)
  , "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined (IntegerOptional)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "ClusterVersion" :: NullOrUndefined (String)
  , "AllowVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "HsmClientCertificateIdentifier" :: NullOrUndefined (String)
  , "HsmConfigurationIdentifier" :: NullOrUndefined (String)
  , "NewClusterIdentifier" :: NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined (BooleanOptional)
  , "ElasticIp" :: NullOrUndefined (String)
  , "EnhancedVpcRouting" :: NullOrUndefined (BooleanOptional)
  }
derive instance newtypeModifyClusterMessage :: Newtype ModifyClusterMessage _


-- | <p/>
newtype ModifyClusterParameterGroupMessage = ModifyClusterParameterGroupMessage 
  { "ParameterGroupName" :: (String)
  , "Parameters" :: (ParametersList)
  }
derive instance newtypeModifyClusterParameterGroupMessage :: Newtype ModifyClusterParameterGroupMessage _


newtype ModifyClusterResult = ModifyClusterResult 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeModifyClusterResult :: Newtype ModifyClusterResult _


-- | <p/>
newtype ModifyClusterSubnetGroupMessage = ModifyClusterSubnetGroupMessage 
  { "ClusterSubnetGroupName" :: (String)
  , "Description" :: NullOrUndefined (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  }
derive instance newtypeModifyClusterSubnetGroupMessage :: Newtype ModifyClusterSubnetGroupMessage _


newtype ModifyClusterSubnetGroupResult = ModifyClusterSubnetGroupResult 
  { "ClusterSubnetGroup" :: NullOrUndefined (ClusterSubnetGroup)
  }
derive instance newtypeModifyClusterSubnetGroupResult :: Newtype ModifyClusterSubnetGroupResult _


-- | <p/>
newtype ModifyEventSubscriptionMessage = ModifyEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SnsTopicArn" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (String)
  , "SourceIds" :: NullOrUndefined (SourceIdsList)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "Severity" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (BooleanOptional)
  }
derive instance newtypeModifyEventSubscriptionMessage :: Newtype ModifyEventSubscriptionMessage _


newtype ModifyEventSubscriptionResult = ModifyEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }
derive instance newtypeModifyEventSubscriptionResult :: Newtype ModifyEventSubscriptionResult _


-- | <p/>
newtype ModifySnapshotCopyRetentionPeriodMessage = ModifySnapshotCopyRetentionPeriodMessage 
  { "ClusterIdentifier" :: (String)
  , "RetentionPeriod" :: (Int)
  }
derive instance newtypeModifySnapshotCopyRetentionPeriodMessage :: Newtype ModifySnapshotCopyRetentionPeriodMessage _


newtype ModifySnapshotCopyRetentionPeriodResult = ModifySnapshotCopyRetentionPeriodResult 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeModifySnapshotCopyRetentionPeriodResult :: Newtype ModifySnapshotCopyRetentionPeriodResult _


-- | <p>The operation would exceed the number of nodes allowed for a cluster.</p>
newtype NumberOfNodesPerClusterLimitExceededFault = NumberOfNodesPerClusterLimitExceededFault 
  { 
  }
derive instance newtypeNumberOfNodesPerClusterLimitExceededFault :: Newtype NumberOfNodesPerClusterLimitExceededFault _


-- | <p>The operation would exceed the number of nodes allotted to the account. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype NumberOfNodesQuotaExceededFault = NumberOfNodesQuotaExceededFault 
  { 
  }
derive instance newtypeNumberOfNodesQuotaExceededFault :: Newtype NumberOfNodesQuotaExceededFault _


-- | <p>Describes an orderable cluster option.</p>
newtype OrderableClusterOption = OrderableClusterOption 
  { "ClusterVersion" :: NullOrUndefined (String)
  , "ClusterType" :: NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined (String)
  , "AvailabilityZones" :: NullOrUndefined (AvailabilityZoneList)
  }
derive instance newtypeOrderableClusterOption :: Newtype OrderableClusterOption _


newtype OrderableClusterOptionsList = OrderableClusterOptionsList (Array OrderableClusterOption)
derive instance newtypeOrderableClusterOptionsList :: Newtype OrderableClusterOptionsList _


-- | <p>Contains the output from the <a>DescribeOrderableClusterOptions</a> action. </p>
newtype OrderableClusterOptionsMessage = OrderableClusterOptionsMessage 
  { "OrderableClusterOptions" :: NullOrUndefined (OrderableClusterOptionsList)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeOrderableClusterOptionsMessage :: Newtype OrderableClusterOptionsMessage _


-- | <p>Describes a parameter in a cluster parameter group.</p>
newtype Parameter = Parameter 
  { "ParameterName" :: NullOrUndefined (String)
  , "ParameterValue" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "Source" :: NullOrUndefined (String)
  , "DataType" :: NullOrUndefined (String)
  , "AllowedValues" :: NullOrUndefined (String)
  , "ApplyType" :: NullOrUndefined (ParameterApplyType)
  , "IsModifiable" :: NullOrUndefined (Boolean)
  , "MinimumEngineVersion" :: NullOrUndefined (String)
  }
derive instance newtypeParameter :: Newtype Parameter _


newtype ParameterApplyType = ParameterApplyType String
derive instance newtypeParameterApplyType :: Newtype ParameterApplyType _


newtype ParameterGroupList = ParameterGroupList (Array ClusterParameterGroup)
derive instance newtypeParameterGroupList :: Newtype ParameterGroupList _


newtype ParametersList = ParametersList (Array Parameter)
derive instance newtypeParametersList :: Newtype ParametersList _


-- | <p>Describes cluster attributes that are in a pending state. A change to one or more the attributes was requested and is in progress or will be applied.</p>
newtype PendingModifiedValues = PendingModifiedValues 
  { "MasterUserPassword" :: NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined (String)
  , "NumberOfNodes" :: NullOrUndefined (IntegerOptional)
  , "ClusterType" :: NullOrUndefined (String)
  , "ClusterVersion" :: NullOrUndefined (String)
  , "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined (IntegerOptional)
  , "ClusterIdentifier" :: NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined (BooleanOptional)
  , "EnhancedVpcRouting" :: NullOrUndefined (BooleanOptional)
  }
derive instance newtypePendingModifiedValues :: Newtype PendingModifiedValues _


-- | <p/>
newtype PurchaseReservedNodeOfferingMessage = PurchaseReservedNodeOfferingMessage 
  { "ReservedNodeOfferingId" :: (String)
  , "NodeCount" :: NullOrUndefined (IntegerOptional)
  }
derive instance newtypePurchaseReservedNodeOfferingMessage :: Newtype PurchaseReservedNodeOfferingMessage _


newtype PurchaseReservedNodeOfferingResult = PurchaseReservedNodeOfferingResult 
  { "ReservedNode" :: NullOrUndefined (ReservedNode)
  }
derive instance newtypePurchaseReservedNodeOfferingResult :: Newtype PurchaseReservedNodeOfferingResult _


-- | <p/>
newtype RebootClusterMessage = RebootClusterMessage 
  { "ClusterIdentifier" :: (String)
  }
derive instance newtypeRebootClusterMessage :: Newtype RebootClusterMessage _


newtype RebootClusterResult = RebootClusterResult 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeRebootClusterResult :: Newtype RebootClusterResult _


-- | <p>Describes a recurring charge.</p>
newtype RecurringCharge = RecurringCharge 
  { "RecurringChargeAmount" :: NullOrUndefined (Number)
  , "RecurringChargeFrequency" :: NullOrUndefined (String)
  }
derive instance newtypeRecurringCharge :: Newtype RecurringCharge _


newtype RecurringChargeList = RecurringChargeList (Array RecurringCharge)
derive instance newtypeRecurringChargeList :: Newtype RecurringChargeList _


-- | <p>Describes a reserved node. You can call the <a>DescribeReservedNodeOfferings</a> API to obtain the available reserved node offerings. </p>
newtype ReservedNode = ReservedNode 
  { "ReservedNodeId" :: NullOrUndefined (String)
  , "ReservedNodeOfferingId" :: NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined (String)
  , "StartTime" :: NullOrUndefined (TStamp)
  , "Duration" :: NullOrUndefined (Int)
  , "FixedPrice" :: NullOrUndefined (Number)
  , "UsagePrice" :: NullOrUndefined (Number)
  , "CurrencyCode" :: NullOrUndefined (String)
  , "NodeCount" :: NullOrUndefined (Int)
  , "State" :: NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined (String)
  , "RecurringCharges" :: NullOrUndefined (RecurringChargeList)
  }
derive instance newtypeReservedNode :: Newtype ReservedNode _


-- | <p>User already has a reservation with the given identifier.</p>
newtype ReservedNodeAlreadyExistsFault = ReservedNodeAlreadyExistsFault 
  { 
  }
derive instance newtypeReservedNodeAlreadyExistsFault :: Newtype ReservedNodeAlreadyExistsFault _


newtype ReservedNodeList = ReservedNodeList (Array ReservedNode)
derive instance newtypeReservedNodeList :: Newtype ReservedNodeList _


-- | <p>The specified reserved compute node not found.</p>
newtype ReservedNodeNotFoundFault = ReservedNodeNotFoundFault 
  { 
  }
derive instance newtypeReservedNodeNotFoundFault :: Newtype ReservedNodeNotFoundFault _


-- | <p>Describes a reserved node offering.</p>
newtype ReservedNodeOffering = ReservedNodeOffering 
  { "ReservedNodeOfferingId" :: NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined (String)
  , "Duration" :: NullOrUndefined (Int)
  , "FixedPrice" :: NullOrUndefined (Number)
  , "UsagePrice" :: NullOrUndefined (Number)
  , "CurrencyCode" :: NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined (String)
  , "RecurringCharges" :: NullOrUndefined (RecurringChargeList)
  }
derive instance newtypeReservedNodeOffering :: Newtype ReservedNodeOffering _


newtype ReservedNodeOfferingList = ReservedNodeOfferingList (Array ReservedNodeOffering)
derive instance newtypeReservedNodeOfferingList :: Newtype ReservedNodeOfferingList _


-- | <p>Specified offering does not exist.</p>
newtype ReservedNodeOfferingNotFoundFault = ReservedNodeOfferingNotFoundFault 
  { 
  }
derive instance newtypeReservedNodeOfferingNotFoundFault :: Newtype ReservedNodeOfferingNotFoundFault _


-- | <p/>
newtype ReservedNodeOfferingsMessage = ReservedNodeOfferingsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ReservedNodeOfferings" :: NullOrUndefined (ReservedNodeOfferingList)
  }
derive instance newtypeReservedNodeOfferingsMessage :: Newtype ReservedNodeOfferingsMessage _


-- | <p>Request would exceed the user's compute node quota. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype ReservedNodeQuotaExceededFault = ReservedNodeQuotaExceededFault 
  { 
  }
derive instance newtypeReservedNodeQuotaExceededFault :: Newtype ReservedNodeQuotaExceededFault _


-- | <p/>
newtype ReservedNodesMessage = ReservedNodesMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ReservedNodes" :: NullOrUndefined (ReservedNodeList)
  }
derive instance newtypeReservedNodesMessage :: Newtype ReservedNodesMessage _


-- | <p/>
newtype ResetClusterParameterGroupMessage = ResetClusterParameterGroupMessage 
  { "ParameterGroupName" :: (String)
  , "ResetAllParameters" :: NullOrUndefined (Boolean)
  , "Parameters" :: NullOrUndefined (ParametersList)
  }
derive instance newtypeResetClusterParameterGroupMessage :: Newtype ResetClusterParameterGroupMessage _


-- | <p>A resize operation for the specified cluster is not found.</p>
newtype ResizeNotFoundFault = ResizeNotFoundFault 
  { 
  }
derive instance newtypeResizeNotFoundFault :: Newtype ResizeNotFoundFault _


-- | <p>Describes the result of a cluster resize operation.</p>
newtype ResizeProgressMessage = ResizeProgressMessage 
  { "TargetNodeType" :: NullOrUndefined (String)
  , "TargetNumberOfNodes" :: NullOrUndefined (IntegerOptional)
  , "TargetClusterType" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  , "ImportTablesCompleted" :: NullOrUndefined (ImportTablesCompleted)
  , "ImportTablesInProgress" :: NullOrUndefined (ImportTablesInProgress)
  , "ImportTablesNotStarted" :: NullOrUndefined (ImportTablesNotStarted)
  , "AvgResizeRateInMegaBytesPerSecond" :: NullOrUndefined (DoubleOptional)
  , "TotalResizeDataInMegaBytes" :: NullOrUndefined (LongOptional)
  , "ProgressInMegaBytes" :: NullOrUndefined (LongOptional)
  , "ElapsedTimeInSeconds" :: NullOrUndefined (LongOptional)
  , "EstimatedTimeToCompletionInSeconds" :: NullOrUndefined (LongOptional)
  }
derive instance newtypeResizeProgressMessage :: Newtype ResizeProgressMessage _


-- | <p>The resource could not be found.</p>
newtype ResourceNotFoundFault = ResourceNotFoundFault 
  { 
  }
derive instance newtypeResourceNotFoundFault :: Newtype ResourceNotFoundFault _


newtype RestorableNodeTypeList = RestorableNodeTypeList (Array String)
derive instance newtypeRestorableNodeTypeList :: Newtype RestorableNodeTypeList _


-- | <p/>
newtype RestoreFromClusterSnapshotMessage = RestoreFromClusterSnapshotMessage 
  { "ClusterIdentifier" :: (String)
  , "SnapshotIdentifier" :: (String)
  , "SnapshotClusterIdentifier" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "AllowVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "ClusterSubnetGroupName" :: NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined (BooleanOptional)
  , "OwnerAccount" :: NullOrUndefined (String)
  , "HsmClientCertificateIdentifier" :: NullOrUndefined (String)
  , "HsmConfigurationIdentifier" :: NullOrUndefined (String)
  , "ElasticIp" :: NullOrUndefined (String)
  , "ClusterParameterGroupName" :: NullOrUndefined (String)
  , "ClusterSecurityGroups" :: NullOrUndefined (ClusterSecurityGroupNameList)
  , "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined (IntegerOptional)
  , "KmsKeyId" :: NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined (String)
  , "EnhancedVpcRouting" :: NullOrUndefined (BooleanOptional)
  , "AdditionalInfo" :: NullOrUndefined (String)
  , "IamRoles" :: NullOrUndefined (IamRoleArnList)
  }
derive instance newtypeRestoreFromClusterSnapshotMessage :: Newtype RestoreFromClusterSnapshotMessage _


newtype RestoreFromClusterSnapshotResult = RestoreFromClusterSnapshotResult 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeRestoreFromClusterSnapshotResult :: Newtype RestoreFromClusterSnapshotResult _


-- | <p>Describes the status of a cluster restore action. Returns null if the cluster was not created by restoring a snapshot.</p>
newtype RestoreStatus = RestoreStatus 
  { "Status" :: NullOrUndefined (String)
  , "CurrentRestoreRateInMegaBytesPerSecond" :: NullOrUndefined (Number)
  , "SnapshotSizeInMegaBytes" :: NullOrUndefined (Number)
  , "ProgressInMegaBytes" :: NullOrUndefined (Number)
  , "ElapsedTimeInSeconds" :: NullOrUndefined (Number)
  , "EstimatedTimeToCompletionInSeconds" :: NullOrUndefined (Number)
  }
derive instance newtypeRestoreStatus :: Newtype RestoreStatus _


-- | <p/>
newtype RestoreTableFromClusterSnapshotMessage = RestoreTableFromClusterSnapshotMessage 
  { "ClusterIdentifier" :: (String)
  , "SnapshotIdentifier" :: (String)
  , "SourceDatabaseName" :: (String)
  , "SourceSchemaName" :: NullOrUndefined (String)
  , "SourceTableName" :: (String)
  , "TargetDatabaseName" :: NullOrUndefined (String)
  , "TargetSchemaName" :: NullOrUndefined (String)
  , "NewTableName" :: (String)
  }
derive instance newtypeRestoreTableFromClusterSnapshotMessage :: Newtype RestoreTableFromClusterSnapshotMessage _


newtype RestoreTableFromClusterSnapshotResult = RestoreTableFromClusterSnapshotResult 
  { "TableRestoreStatus" :: NullOrUndefined (TableRestoreStatus)
  }
derive instance newtypeRestoreTableFromClusterSnapshotResult :: Newtype RestoreTableFromClusterSnapshotResult _


-- | <p/>
newtype RevokeClusterSecurityGroupIngressMessage = RevokeClusterSecurityGroupIngressMessage 
  { "ClusterSecurityGroupName" :: (String)
  , "CIDRIP" :: NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined (String)
  }
derive instance newtypeRevokeClusterSecurityGroupIngressMessage :: Newtype RevokeClusterSecurityGroupIngressMessage _


newtype RevokeClusterSecurityGroupIngressResult = RevokeClusterSecurityGroupIngressResult 
  { "ClusterSecurityGroup" :: NullOrUndefined (ClusterSecurityGroup)
  }
derive instance newtypeRevokeClusterSecurityGroupIngressResult :: Newtype RevokeClusterSecurityGroupIngressResult _


-- | <p/>
newtype RevokeSnapshotAccessMessage = RevokeSnapshotAccessMessage 
  { "SnapshotIdentifier" :: (String)
  , "SnapshotClusterIdentifier" :: NullOrUndefined (String)
  , "AccountWithRestoreAccess" :: (String)
  }
derive instance newtypeRevokeSnapshotAccessMessage :: Newtype RevokeSnapshotAccessMessage _


newtype RevokeSnapshotAccessResult = RevokeSnapshotAccessResult 
  { "Snapshot" :: NullOrUndefined (Snapshot)
  }
derive instance newtypeRevokeSnapshotAccessResult :: Newtype RevokeSnapshotAccessResult _


-- | <p/>
newtype RotateEncryptionKeyMessage = RotateEncryptionKeyMessage 
  { "ClusterIdentifier" :: (String)
  }
derive instance newtypeRotateEncryptionKeyMessage :: Newtype RotateEncryptionKeyMessage _


newtype RotateEncryptionKeyResult = RotateEncryptionKeyResult 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeRotateEncryptionKeyResult :: Newtype RotateEncryptionKeyResult _


-- | <p>Amazon SNS has responded that there is a problem with the specified Amazon SNS topic.</p>
newtype SNSInvalidTopicFault = SNSInvalidTopicFault 
  { 
  }
derive instance newtypeSNSInvalidTopicFault :: Newtype SNSInvalidTopicFault _


-- | <p>You do not have permission to publish to the specified Amazon SNS topic.</p>
newtype SNSNoAuthorizationFault = SNSNoAuthorizationFault 
  { 
  }
derive instance newtypeSNSNoAuthorizationFault :: Newtype SNSNoAuthorizationFault _


-- | <p>An Amazon SNS topic with the specified Amazon Resource Name (ARN) does not exist.</p>
newtype SNSTopicArnNotFoundFault = SNSTopicArnNotFoundFault 
  { 
  }
derive instance newtypeSNSTopicArnNotFoundFault :: Newtype SNSTopicArnNotFoundFault _


newtype SensitiveString = SensitiveString String
derive instance newtypeSensitiveString :: Newtype SensitiveString _


-- | <p>Describes a snapshot.</p>
newtype Snapshot = Snapshot 
  { "SnapshotIdentifier" :: NullOrUndefined (String)
  , "ClusterIdentifier" :: NullOrUndefined (String)
  , "SnapshotCreateTime" :: NullOrUndefined (TStamp)
  , "Status" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (Int)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "ClusterCreateTime" :: NullOrUndefined (TStamp)
  , "MasterUsername" :: NullOrUndefined (String)
  , "ClusterVersion" :: NullOrUndefined (String)
  , "SnapshotType" :: NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined (String)
  , "NumberOfNodes" :: NullOrUndefined (Int)
  , "DBName" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "Encrypted" :: NullOrUndefined (Boolean)
  , "KmsKeyId" :: NullOrUndefined (String)
  , "EncryptedWithHSM" :: NullOrUndefined (Boolean)
  , "AccountsWithRestoreAccess" :: NullOrUndefined (AccountsWithRestoreAccessList)
  , "OwnerAccount" :: NullOrUndefined (String)
  , "TotalBackupSizeInMegaBytes" :: NullOrUndefined (Number)
  , "ActualIncrementalBackupSizeInMegaBytes" :: NullOrUndefined (Number)
  , "BackupProgressInMegaBytes" :: NullOrUndefined (Number)
  , "CurrentBackupRateInMegaBytesPerSecond" :: NullOrUndefined (Number)
  , "EstimatedSecondsToCompletion" :: NullOrUndefined (Number)
  , "ElapsedTimeInSeconds" :: NullOrUndefined (Number)
  , "SourceRegion" :: NullOrUndefined (String)
  , "Tags" :: NullOrUndefined (TagList)
  , "RestorableNodeTypes" :: NullOrUndefined (RestorableNodeTypeList)
  , "EnhancedVpcRouting" :: NullOrUndefined (Boolean)
  }
derive instance newtypeSnapshot :: Newtype Snapshot _


-- | <p>The cluster already has cross-region snapshot copy disabled.</p>
newtype SnapshotCopyAlreadyDisabledFault = SnapshotCopyAlreadyDisabledFault 
  { 
  }
derive instance newtypeSnapshotCopyAlreadyDisabledFault :: Newtype SnapshotCopyAlreadyDisabledFault _


-- | <p>The cluster already has cross-region snapshot copy enabled.</p>
newtype SnapshotCopyAlreadyEnabledFault = SnapshotCopyAlreadyEnabledFault 
  { 
  }
derive instance newtypeSnapshotCopyAlreadyEnabledFault :: Newtype SnapshotCopyAlreadyEnabledFault _


-- | <p>Cross-region snapshot copy was temporarily disabled. Try your request again.</p>
newtype SnapshotCopyDisabledFault = SnapshotCopyDisabledFault 
  { 
  }
derive instance newtypeSnapshotCopyDisabledFault :: Newtype SnapshotCopyDisabledFault _


-- | <p>The snapshot copy grant that grants Amazon Redshift permission to encrypt copied snapshots with the specified customer master key (CMK) from AWS KMS in the destination region.</p> <p> For more information about managing snapshot copy grants, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html">Amazon Redshift Database Encryption</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype SnapshotCopyGrant = SnapshotCopyGrant 
  { "SnapshotCopyGrantName" :: NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined (String)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeSnapshotCopyGrant :: Newtype SnapshotCopyGrant _


-- | <p>The snapshot copy grant can't be created because a grant with the same name already exists.</p>
newtype SnapshotCopyGrantAlreadyExistsFault = SnapshotCopyGrantAlreadyExistsFault 
  { 
  }
derive instance newtypeSnapshotCopyGrantAlreadyExistsFault :: Newtype SnapshotCopyGrantAlreadyExistsFault _


newtype SnapshotCopyGrantList = SnapshotCopyGrantList (Array SnapshotCopyGrant)
derive instance newtypeSnapshotCopyGrantList :: Newtype SnapshotCopyGrantList _


-- | <p/>
newtype SnapshotCopyGrantMessage = SnapshotCopyGrantMessage 
  { "Marker" :: NullOrUndefined (String)
  , "SnapshotCopyGrants" :: NullOrUndefined (SnapshotCopyGrantList)
  }
derive instance newtypeSnapshotCopyGrantMessage :: Newtype SnapshotCopyGrantMessage _


-- | <p>The specified snapshot copy grant can't be found. Make sure that the name is typed correctly and that the grant exists in the destination region.</p>
newtype SnapshotCopyGrantNotFoundFault = SnapshotCopyGrantNotFoundFault 
  { 
  }
derive instance newtypeSnapshotCopyGrantNotFoundFault :: Newtype SnapshotCopyGrantNotFoundFault _


-- | <p>The AWS account has exceeded the maximum number of snapshot copy grants in this region.</p>
newtype SnapshotCopyGrantQuotaExceededFault = SnapshotCopyGrantQuotaExceededFault 
  { 
  }
derive instance newtypeSnapshotCopyGrantQuotaExceededFault :: Newtype SnapshotCopyGrantQuotaExceededFault _


newtype SnapshotList = SnapshotList (Array Snapshot)
derive instance newtypeSnapshotList :: Newtype SnapshotList _


-- | <p>Contains the output from the <a>DescribeClusterSnapshots</a> action. </p>
newtype SnapshotMessage = SnapshotMessage 
  { "Marker" :: NullOrUndefined (String)
  , "Snapshots" :: NullOrUndefined (SnapshotList)
  }
derive instance newtypeSnapshotMessage :: Newtype SnapshotMessage _


newtype SourceIdsList = SourceIdsList (Array String)
derive instance newtypeSourceIdsList :: Newtype SourceIdsList _


-- | <p>The specified Amazon Redshift event source could not be found.</p>
newtype SourceNotFoundFault = SourceNotFoundFault 
  { 
  }
derive instance newtypeSourceNotFoundFault :: Newtype SourceNotFoundFault _


newtype SourceType = SourceType String
derive instance newtypeSourceType :: Newtype SourceType _


-- | <p>Describes a subnet.</p>
newtype Subnet = Subnet 
  { "SubnetIdentifier" :: NullOrUndefined (String)
  , "SubnetAvailabilityZone" :: NullOrUndefined (AvailabilityZone)
  , "SubnetStatus" :: NullOrUndefined (String)
  }
derive instance newtypeSubnet :: Newtype Subnet _


-- | <p>A specified subnet is already in use by another cluster.</p>
newtype SubnetAlreadyInUse = SubnetAlreadyInUse 
  { 
  }
derive instance newtypeSubnetAlreadyInUse :: Newtype SubnetAlreadyInUse _


newtype SubnetIdentifierList = SubnetIdentifierList (Array String)
derive instance newtypeSubnetIdentifierList :: Newtype SubnetIdentifierList _


newtype SubnetList = SubnetList (Array Subnet)
derive instance newtypeSubnetList :: Newtype SubnetList _


-- | <p>There is already an existing event notification subscription with the specified name.</p>
newtype SubscriptionAlreadyExistFault = SubscriptionAlreadyExistFault 
  { 
  }
derive instance newtypeSubscriptionAlreadyExistFault :: Newtype SubscriptionAlreadyExistFault _


-- | <p>The value specified for the event category was not one of the allowed values, or it specified a category that does not apply to the specified source type. The allowed values are Configuration, Management, Monitoring, and Security.</p>
newtype SubscriptionCategoryNotFoundFault = SubscriptionCategoryNotFoundFault 
  { 
  }
derive instance newtypeSubscriptionCategoryNotFoundFault :: Newtype SubscriptionCategoryNotFoundFault _


-- | <p>An Amazon Redshift event with the specified event ID does not exist.</p>
newtype SubscriptionEventIdNotFoundFault = SubscriptionEventIdNotFoundFault 
  { 
  }
derive instance newtypeSubscriptionEventIdNotFoundFault :: Newtype SubscriptionEventIdNotFoundFault _


-- | <p>An Amazon Redshift event notification subscription with the specified name does not exist.</p>
newtype SubscriptionNotFoundFault = SubscriptionNotFoundFault 
  { 
  }
derive instance newtypeSubscriptionNotFoundFault :: Newtype SubscriptionNotFoundFault _


-- | <p>The value specified for the event severity was not one of the allowed values, or it specified a severity that does not apply to the specified source type. The allowed values are ERROR and INFO.</p>
newtype SubscriptionSeverityNotFoundFault = SubscriptionSeverityNotFoundFault 
  { 
  }
derive instance newtypeSubscriptionSeverityNotFoundFault :: Newtype SubscriptionSeverityNotFoundFault _


newtype TStamp = TStamp Number
derive instance newtypeTStamp :: Newtype TStamp _


-- | <p>The specified <code>TableRestoreRequestId</code> value was not found.</p>
newtype TableRestoreNotFoundFault = TableRestoreNotFoundFault 
  { 
  }
derive instance newtypeTableRestoreNotFoundFault :: Newtype TableRestoreNotFoundFault _


-- | <p>Describes the status of a <a>RestoreTableFromClusterSnapshot</a> operation.</p>
newtype TableRestoreStatus = TableRestoreStatus 
  { "TableRestoreRequestId" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (TableRestoreStatusType)
  , "Message" :: NullOrUndefined (String)
  , "RequestTime" :: NullOrUndefined (TStamp)
  , "ProgressInMegaBytes" :: NullOrUndefined (LongOptional)
  , "TotalDataInMegaBytes" :: NullOrUndefined (LongOptional)
  , "ClusterIdentifier" :: NullOrUndefined (String)
  , "SnapshotIdentifier" :: NullOrUndefined (String)
  , "SourceDatabaseName" :: NullOrUndefined (String)
  , "SourceSchemaName" :: NullOrUndefined (String)
  , "SourceTableName" :: NullOrUndefined (String)
  , "TargetDatabaseName" :: NullOrUndefined (String)
  , "TargetSchemaName" :: NullOrUndefined (String)
  , "NewTableName" :: NullOrUndefined (String)
  }
derive instance newtypeTableRestoreStatus :: Newtype TableRestoreStatus _


newtype TableRestoreStatusList = TableRestoreStatusList (Array TableRestoreStatus)
derive instance newtypeTableRestoreStatusList :: Newtype TableRestoreStatusList _


-- | <p/>
newtype TableRestoreStatusMessage = TableRestoreStatusMessage 
  { "TableRestoreStatusDetails" :: NullOrUndefined (TableRestoreStatusList)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeTableRestoreStatusMessage :: Newtype TableRestoreStatusMessage _


newtype TableRestoreStatusType = TableRestoreStatusType String
derive instance newtypeTableRestoreStatusType :: Newtype TableRestoreStatusType _


-- | <p>A tag consisting of a name/value pair for a resource.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (String)
  , "Value" :: NullOrUndefined (String)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKeyList = TagKeyList (Array String)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


-- | <p>The request exceeds the limit of 10 tags for the resource.</p>
newtype TagLimitExceededFault = TagLimitExceededFault 
  { 
  }
derive instance newtypeTagLimitExceededFault :: Newtype TagLimitExceededFault _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagValueList = TagValueList (Array String)
derive instance newtypeTagValueList :: Newtype TagValueList _


-- | <p>A tag and its associated resource.</p>
newtype TaggedResource = TaggedResource 
  { "Tag" :: NullOrUndefined (Tag)
  , "ResourceName" :: NullOrUndefined (String)
  , "ResourceType" :: NullOrUndefined (String)
  }
derive instance newtypeTaggedResource :: Newtype TaggedResource _


newtype TaggedResourceList = TaggedResourceList (Array TaggedResource)
derive instance newtypeTaggedResourceList :: Newtype TaggedResourceList _


-- | <p/>
newtype TaggedResourceListMessage = TaggedResourceListMessage 
  { "TaggedResources" :: NullOrUndefined (TaggedResourceList)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeTaggedResourceListMessage :: Newtype TaggedResourceListMessage _


-- | <p>Your account is not authorized to perform the requested operation.</p>
newtype UnauthorizedOperation = UnauthorizedOperation 
  { 
  }
derive instance newtypeUnauthorizedOperation :: Newtype UnauthorizedOperation _


-- | <p>The specified region is incorrect or does not exist.</p>
newtype UnknownSnapshotCopyRegionFault = UnknownSnapshotCopyRegionFault 
  { 
  }
derive instance newtypeUnknownSnapshotCopyRegionFault :: Newtype UnknownSnapshotCopyRegionFault _


-- | <p>The requested operation isn't supported.</p>
newtype UnsupportedOperationFault = UnsupportedOperationFault 
  { 
  }
derive instance newtypeUnsupportedOperationFault :: Newtype UnsupportedOperationFault _


-- | <p>A request option was specified that is not supported.</p>
newtype UnsupportedOptionFault = UnsupportedOptionFault 
  { 
  }
derive instance newtypeUnsupportedOptionFault :: Newtype UnsupportedOptionFault _


newtype VpcSecurityGroupIdList = VpcSecurityGroupIdList (Array String)
derive instance newtypeVpcSecurityGroupIdList :: Newtype VpcSecurityGroupIdList _


-- | <p>Describes the members of a VPC security group.</p>
newtype VpcSecurityGroupMembership = VpcSecurityGroupMembership 
  { "VpcSecurityGroupId" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }
derive instance newtypeVpcSecurityGroupMembership :: Newtype VpcSecurityGroupMembership _


newtype VpcSecurityGroupMembershipList = VpcSecurityGroupMembershipList (Array VpcSecurityGroupMembership)
derive instance newtypeVpcSecurityGroupMembershipList :: Newtype VpcSecurityGroupMembershipList _
