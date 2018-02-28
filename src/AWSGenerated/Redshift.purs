

-- | <fullname>Amazon Redshift</fullname> <p> <b>Overview</b> </p> <p>This is an interface reference for Amazon Redshift. It contains documentation for one of the programming or command line interfaces you can use to manage Amazon Redshift clusters. Note that Amazon Redshift is asynchronous, which means that some interfaces may require techniques, such as polling or asynchronous callback handlers, to determine when a command has been applied. In this reference, the parameter descriptions indicate whether a change is applied immediately, on the next instance reboot, or during the next maintenance window. For a summary of the Amazon Redshift cluster management interfaces, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/using-aws-sdk.html">Using the Amazon Redshift Management Interfaces</a>.</p> <p>Amazon Redshift manages all the work of setting up, operating, and scaling a data warehouse: provisioning capacity, monitoring and backing up the cluster, and applying patches and upgrades to the Amazon Redshift engine. You can focus on using your data to acquire new insights for your business and customers.</p> <p>If you are a first-time user of Amazon Redshift, we recommend that you begin by reading the <a href="http://docs.aws.amazon.com/redshift/latest/gsg/getting-started.html">Amazon Redshift Getting Started Guide</a>.</p> <p>If you are a database developer, the <a href="http://docs.aws.amazon.com/redshift/latest/dg/welcome.html">Amazon Redshift Database Developer Guide</a> explains how to design, build, query, and maintain the databases that make up your data warehouse. </p>
module AWS.Redshift where

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

serviceName = "Redshift" :: String


-- | <p>Adds an inbound (ingress) rule to an Amazon Redshift security group. Depending on whether the application accessing your cluster is running on the Internet or an Amazon EC2 instance, you can authorize inbound access to either a Classless Interdomain Routing (CIDR)/Internet Protocol (IP) range or to an Amazon EC2 security group. You can add as many as 20 ingress rules to an Amazon Redshift security group.</p> <p>If you authorize access to an Amazon EC2 security group, specify <i>EC2SecurityGroupName</i> and <i>EC2SecurityGroupOwnerId</i>. The Amazon EC2 security group and Amazon Redshift cluster must be in the same AWS region. </p> <p>If you authorize access to a CIDR/IP address range, specify <i>CIDRIP</i>. For an overview of CIDR blocks, see the Wikipedia article on <a href="http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing">Classless Inter-Domain Routing</a>. </p> <p>You must also associate the security group with a cluster so that clients running on these IP addresses or the EC2 instance are authorized to connect to the cluster. For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Working with Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
authorizeClusterSecurityGroupIngress :: forall eff. AuthorizeClusterSecurityGroupIngressMessage -> Aff (exception :: EXCEPTION | eff) AuthorizeClusterSecurityGroupIngressResult
authorizeClusterSecurityGroupIngress = Request.request serviceName "authorizeClusterSecurityGroupIngress" 


-- | <p>Authorizes the specified AWS customer account to restore the specified snapshot.</p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
authorizeSnapshotAccess :: forall eff. AuthorizeSnapshotAccessMessage -> Aff (exception :: EXCEPTION | eff) AuthorizeSnapshotAccessResult
authorizeSnapshotAccess = Request.request serviceName "authorizeSnapshotAccess" 


-- | <p>Copies the specified automated cluster snapshot to a new manual cluster snapshot. The source must be an automated snapshot and it must be in the available state.</p> <p>When you delete a cluster, Amazon Redshift deletes any automated snapshots of the cluster. Also, when the retention period of the snapshot expires, Amazon Redshift automatically deletes it. If you want to keep an automated snapshot for a longer period, you can make a manual copy of the snapshot. Manual snapshots are retained until you delete them.</p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
copyClusterSnapshot :: forall eff. CopyClusterSnapshotMessage -> Aff (exception :: EXCEPTION | eff) CopyClusterSnapshotResult
copyClusterSnapshot = Request.request serviceName "copyClusterSnapshot" 


-- | <p>Creates a new cluster.</p> <p>To create the cluster in Virtual Private Cloud (VPC), you must provide a cluster subnet group name. The cluster subnet group identifies the subnets of your VPC that Amazon Redshift uses when creating the cluster. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
createCluster :: forall eff. CreateClusterMessage -> Aff (exception :: EXCEPTION | eff) CreateClusterResult
createCluster = Request.request serviceName "createCluster" 


-- | <p>Creates an Amazon Redshift parameter group.</p> <p>Creating parameter groups is independent of creating clusters. You can associate a cluster with a parameter group when you create the cluster. You can also associate an existing cluster with a parameter group after the cluster is created by using <a>ModifyCluster</a>. </p> <p>Parameters in the parameter group define specific behavior that applies to the databases you create on the cluster. For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
createClusterParameterGroup :: forall eff. CreateClusterParameterGroupMessage -> Aff (exception :: EXCEPTION | eff) CreateClusterParameterGroupResult
createClusterParameterGroup = Request.request serviceName "createClusterParameterGroup" 


-- | <p>Creates a new Amazon Redshift security group. You use security groups to control access to non-VPC clusters.</p> <p> For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Amazon Redshift Cluster Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
createClusterSecurityGroup :: forall eff. CreateClusterSecurityGroupMessage -> Aff (exception :: EXCEPTION | eff) CreateClusterSecurityGroupResult
createClusterSecurityGroup = Request.request serviceName "createClusterSecurityGroup" 


-- | <p>Creates a manual snapshot of the specified cluster. The cluster must be in the <code>available</code> state. </p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
createClusterSnapshot :: forall eff. CreateClusterSnapshotMessage -> Aff (exception :: EXCEPTION | eff) CreateClusterSnapshotResult
createClusterSnapshot = Request.request serviceName "createClusterSnapshot" 


-- | <p>Creates a new Amazon Redshift subnet group. You must provide a list of one or more subnets in your existing Amazon Virtual Private Cloud (Amazon VPC) when creating Amazon Redshift subnet group.</p> <p> For information about subnet groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-cluster-subnet-groups.html">Amazon Redshift Cluster Subnet Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
createClusterSubnetGroup :: forall eff. CreateClusterSubnetGroupMessage -> Aff (exception :: EXCEPTION | eff) CreateClusterSubnetGroupResult
createClusterSubnetGroup = Request.request serviceName "createClusterSubnetGroup" 


-- | <p>Creates an Amazon Redshift event notification subscription. This action requires an ARN (Amazon Resource Name) of an Amazon SNS topic created by either the Amazon Redshift console, the Amazon SNS console, or the Amazon SNS API. To obtain an ARN with Amazon SNS, you must create a topic in Amazon SNS and subscribe to the topic. The ARN is displayed in the SNS console.</p> <p>You can specify the source type, and lists of Amazon Redshift source IDs, event categories, and event severities. Notifications will be sent for all events you want that match those criteria. For example, you can specify source type = cluster, source ID = my-cluster-1 and mycluster2, event categories = Availability, Backup, and severity = ERROR. The subscription will only send notifications for those ERROR events in the Availability and Backup categories for the specified clusters.</p> <p>If you specify both the source type and source IDs, such as source type = cluster and source identifier = my-cluster-1, notifications will be sent for all the cluster events for my-cluster-1. If you specify a source type but do not specify a source identifier, you will receive notice of the events for the objects of that type in your AWS account. If you do not specify either the SourceType nor the SourceIdentifier, you will be notified of events generated from all Amazon Redshift sources belonging to your AWS account. You must specify a source type if you specify a source ID.</p>
createEventSubscription :: forall eff. CreateEventSubscriptionMessage -> Aff (exception :: EXCEPTION | eff) CreateEventSubscriptionResult
createEventSubscription = Request.request serviceName "createEventSubscription" 


-- | <p>Creates an HSM client certificate that an Amazon Redshift cluster will use to connect to the client's HSM in order to store and retrieve the keys used to encrypt the cluster databases.</p> <p>The command returns a public key, which you must store in the HSM. In addition to creating the HSM certificate, you must create an Amazon Redshift HSM configuration that provides a cluster the information needed to store and use encryption keys in the HSM. For more information, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html">Hardware Security Modules</a> in the Amazon Redshift Cluster Management Guide.</p>
createHsmClientCertificate :: forall eff. CreateHsmClientCertificateMessage -> Aff (exception :: EXCEPTION | eff) CreateHsmClientCertificateResult
createHsmClientCertificate = Request.request serviceName "createHsmClientCertificate" 


-- | <p>Creates an HSM configuration that contains the information required by an Amazon Redshift cluster to store and use database encryption keys in a Hardware Security Module (HSM). After creating the HSM configuration, you can specify it as a parameter when creating a cluster. The cluster will then store its encryption keys in the HSM.</p> <p>In addition to creating an HSM configuration, you must also create an HSM client certificate. For more information, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html">Hardware Security Modules</a> in the Amazon Redshift Cluster Management Guide.</p>
createHsmConfiguration :: forall eff. CreateHsmConfigurationMessage -> Aff (exception :: EXCEPTION | eff) CreateHsmConfigurationResult
createHsmConfiguration = Request.request serviceName "createHsmConfiguration" 


-- | <p>Creates a snapshot copy grant that permits Amazon Redshift to use a customer master key (CMK) from AWS Key Management Service (AWS KMS) to encrypt copied snapshots in a destination region.</p> <p> For more information about managing snapshot copy grants, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html">Amazon Redshift Database Encryption</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
createSnapshotCopyGrant :: forall eff. CreateSnapshotCopyGrantMessage -> Aff (exception :: EXCEPTION | eff) CreateSnapshotCopyGrantResult
createSnapshotCopyGrant = Request.request serviceName "createSnapshotCopyGrant" 


-- | <p>Adds one or more tags to a specified resource.</p> <p>A resource can have up to 10 tags. If you try to create more than 10 tags for a resource, you will receive an error and the attempt will fail.</p> <p>If you specify a key that already exists for the resource, the value for that key will be updated with the new value.</p>
createTags :: forall eff. CreateTagsMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
createTags = Request.request serviceName "createTags" 


-- | <p>Deletes a previously provisioned cluster. A successful response from the web service indicates that the request was received correctly. Use <a>DescribeClusters</a> to monitor the status of the deletion. The delete operation cannot be canceled or reverted once submitted. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>If you want to shut down the cluster and retain it for future use, set <i>SkipFinalClusterSnapshot</i> to <code>false</code> and specify a name for <i>FinalClusterSnapshotIdentifier</i>. You can later restore this snapshot to resume using the cluster. If a final cluster snapshot is requested, the status of the cluster will be "final-snapshot" while the snapshot is being taken, then it's "deleting" once Amazon Redshift begins deleting the cluster. </p> <p> For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
deleteCluster :: forall eff. DeleteClusterMessage -> Aff (exception :: EXCEPTION | eff) DeleteClusterResult
deleteCluster = Request.request serviceName "deleteCluster" 


-- | <p>Deletes a specified Amazon Redshift parameter group.</p> <note> <p>You cannot delete a parameter group if it is associated with a cluster.</p> </note>
deleteClusterParameterGroup :: forall eff. DeleteClusterParameterGroupMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteClusterParameterGroup = Request.request serviceName "deleteClusterParameterGroup" 


-- | <p>Deletes an Amazon Redshift security group.</p> <note> <p>You cannot delete a security group that is associated with any clusters. You cannot delete the default security group.</p> </note> <p> For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Amazon Redshift Cluster Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
deleteClusterSecurityGroup :: forall eff. DeleteClusterSecurityGroupMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteClusterSecurityGroup = Request.request serviceName "deleteClusterSecurityGroup" 


-- | <p>Deletes the specified manual snapshot. The snapshot must be in the <code>available</code> state, with no other users authorized to access the snapshot. </p> <p>Unlike automated snapshots, manual snapshots are retained even after you delete your cluster. Amazon Redshift does not delete your manual snapshots. You must delete manual snapshot explicitly to avoid getting charged. If other accounts are authorized to access the snapshot, you must revoke all of the authorizations before you can delete the snapshot.</p>
deleteClusterSnapshot :: forall eff. DeleteClusterSnapshotMessage -> Aff (exception :: EXCEPTION | eff) DeleteClusterSnapshotResult
deleteClusterSnapshot = Request.request serviceName "deleteClusterSnapshot" 


-- | <p>Deletes the specified cluster subnet group.</p>
deleteClusterSubnetGroup :: forall eff. DeleteClusterSubnetGroupMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteClusterSubnetGroup = Request.request serviceName "deleteClusterSubnetGroup" 


-- | <p>Deletes an Amazon Redshift event notification subscription.</p>
deleteEventSubscription :: forall eff. DeleteEventSubscriptionMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteEventSubscription = Request.request serviceName "deleteEventSubscription" 


-- | <p>Deletes the specified HSM client certificate.</p>
deleteHsmClientCertificate :: forall eff. DeleteHsmClientCertificateMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteHsmClientCertificate = Request.request serviceName "deleteHsmClientCertificate" 


-- | <p>Deletes the specified Amazon Redshift HSM configuration.</p>
deleteHsmConfiguration :: forall eff. DeleteHsmConfigurationMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteHsmConfiguration = Request.request serviceName "deleteHsmConfiguration" 


-- | <p>Deletes the specified snapshot copy grant.</p>
deleteSnapshotCopyGrant :: forall eff. DeleteSnapshotCopyGrantMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteSnapshotCopyGrant = Request.request serviceName "deleteSnapshotCopyGrant" 


-- | <p>Deletes a tag or tags from a resource. You must provide the ARN of the resource from which you want to delete the tag or tags.</p>
deleteTags :: forall eff. DeleteTagsMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteTags = Request.request serviceName "deleteTags" 


-- | <p>Returns a list of Amazon Redshift parameter groups, including parameter groups you created and the default parameter group. For each parameter group, the response includes the parameter group name, description, and parameter group family name. You can optionally specify a name to retrieve the description of a specific parameter group.</p> <p> For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all parameter groups that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all parameter groups that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, parameter groups are returned regardless of whether they have tag keys or values associated with them.</p>
describeClusterParameterGroups :: forall eff. DescribeClusterParameterGroupsMessage -> Aff (exception :: EXCEPTION | eff) ClusterParameterGroupsMessage
describeClusterParameterGroups = Request.request serviceName "describeClusterParameterGroups" 


-- | <p>Returns a detailed list of parameters contained within the specified Amazon Redshift parameter group. For each parameter the response includes information such as parameter name, description, data type, value, whether the parameter value is modifiable, and so on.</p> <p>You can specify <i>source</i> filter to retrieve parameters of only specific type. For example, to retrieve parameters that were modified by a user action such as from <a>ModifyClusterParameterGroup</a>, you can specify <i>source</i> equal to <i>user</i>.</p> <p> For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
describeClusterParameters :: forall eff. DescribeClusterParametersMessage -> Aff (exception :: EXCEPTION | eff) ClusterParameterGroupDetails
describeClusterParameters = Request.request serviceName "describeClusterParameters" 


-- | <p>Returns information about Amazon Redshift security groups. If the name of a security group is specified, the response will contain only information about only that security group.</p> <p> For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Amazon Redshift Cluster Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all security groups that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all security groups that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, security groups are returned regardless of whether they have tag keys or values associated with them.</p>
describeClusterSecurityGroups :: forall eff. DescribeClusterSecurityGroupsMessage -> Aff (exception :: EXCEPTION | eff) ClusterSecurityGroupMessage
describeClusterSecurityGroups = Request.request serviceName "describeClusterSecurityGroups" 


-- | <p>Returns one or more snapshot objects, which contain metadata about your cluster snapshots. By default, this operation returns information about all snapshots of all clusters that are owned by you AWS customer account. No information is returned for snapshots owned by inactive AWS customer accounts.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all snapshots that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all snapshots that have any combination of those values are returned. Only snapshots that you own are returned in the response; shared snapshots are not returned with the tag key and tag value request parameters.</p> <p>If both tag keys and values are omitted from the request, snapshots are returned regardless of whether they have tag keys or values associated with them.</p>
describeClusterSnapshots :: forall eff. DescribeClusterSnapshotsMessage -> Aff (exception :: EXCEPTION | eff) SnapshotMessage
describeClusterSnapshots = Request.request serviceName "describeClusterSnapshots" 


-- | <p>Returns one or more cluster subnet group objects, which contain metadata about your cluster subnet groups. By default, this operation returns information about all cluster subnet groups that are defined in you AWS account.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all subnet groups that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all subnet groups that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, subnet groups are returned regardless of whether they have tag keys or values associated with them.</p>
describeClusterSubnetGroups :: forall eff. DescribeClusterSubnetGroupsMessage -> Aff (exception :: EXCEPTION | eff) ClusterSubnetGroupMessage
describeClusterSubnetGroups = Request.request serviceName "describeClusterSubnetGroups" 


-- | <p>Returns descriptions of the available Amazon Redshift cluster versions. You can call this operation even before creating any clusters to learn more about the Amazon Redshift versions. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
describeClusterVersions :: forall eff. DescribeClusterVersionsMessage -> Aff (exception :: EXCEPTION | eff) ClusterVersionsMessage
describeClusterVersions = Request.request serviceName "describeClusterVersions" 


-- | <p>Returns properties of provisioned clusters including general cluster properties, cluster database properties, maintenance and backup properties, and security and access properties. This operation supports pagination. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all clusters that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all clusters that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, clusters are returned regardless of whether they have tag keys or values associated with them.</p>
describeClusters :: forall eff. DescribeClustersMessage -> Aff (exception :: EXCEPTION | eff) ClustersMessage
describeClusters = Request.request serviceName "describeClusters" 


-- | <p>Returns a list of parameter settings for the specified parameter group family.</p> <p> For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
describeDefaultClusterParameters :: forall eff. DescribeDefaultClusterParametersMessage -> Aff (exception :: EXCEPTION | eff) DescribeDefaultClusterParametersResult
describeDefaultClusterParameters = Request.request serviceName "describeDefaultClusterParameters" 


-- | <p>Displays a list of event categories for all event source types, or for a specified source type. For a list of the event categories and source types, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-event-notifications.html">Amazon Redshift Event Notifications</a>.</p>
describeEventCategories :: forall eff. DescribeEventCategoriesMessage -> Aff (exception :: EXCEPTION | eff) EventCategoriesMessage
describeEventCategories = Request.request serviceName "describeEventCategories" 


-- | <p>Lists descriptions of all the Amazon Redshift event notification subscriptions for a customer account. If you specify a subscription name, lists the description for that subscription.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all event notification subscriptions that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all subscriptions that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, subscriptions are returned regardless of whether they have tag keys or values associated with them.</p>
describeEventSubscriptions :: forall eff. DescribeEventSubscriptionsMessage -> Aff (exception :: EXCEPTION | eff) EventSubscriptionsMessage
describeEventSubscriptions = Request.request serviceName "describeEventSubscriptions" 


-- | <p>Returns events related to clusters, security groups, snapshots, and parameter groups for the past 14 days. Events specific to a particular cluster, security group, snapshot or parameter group can be obtained by providing the name as a parameter. By default, the past hour of events are returned.</p>
describeEvents :: forall eff. DescribeEventsMessage -> Aff (exception :: EXCEPTION | eff) EventsMessage
describeEvents = Request.request serviceName "describeEvents" 


-- | <p>Returns information about the specified HSM client certificate. If no certificate ID is specified, returns information about all the HSM certificates owned by your AWS customer account.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all HSM client certificates that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all HSM client certificates that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, HSM client certificates are returned regardless of whether they have tag keys or values associated with them.</p>
describeHsmClientCertificates :: forall eff. DescribeHsmClientCertificatesMessage -> Aff (exception :: EXCEPTION | eff) HsmClientCertificateMessage
describeHsmClientCertificates = Request.request serviceName "describeHsmClientCertificates" 


-- | <p>Returns information about the specified Amazon Redshift HSM configuration. If no configuration ID is specified, returns information about all the HSM configurations owned by your AWS customer account.</p> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all HSM connections that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all HSM connections that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, HSM connections are returned regardless of whether they have tag keys or values associated with them.</p>
describeHsmConfigurations :: forall eff. DescribeHsmConfigurationsMessage -> Aff (exception :: EXCEPTION | eff) HsmConfigurationMessage
describeHsmConfigurations = Request.request serviceName "describeHsmConfigurations" 


-- | <p>Describes whether information, such as queries and connection attempts, is being logged for the specified Amazon Redshift cluster.</p>
describeLoggingStatus :: forall eff. DescribeLoggingStatusMessage -> Aff (exception :: EXCEPTION | eff) LoggingStatus
describeLoggingStatus = Request.request serviceName "describeLoggingStatus" 


-- | <p>Returns a list of orderable cluster options. Before you create a new cluster you can use this operation to find what options are available, such as the EC2 Availability Zones (AZ) in the specific AWS region that you can specify, and the node types you can request. The node types differ by available storage, memory, CPU and price. With the cost involved you might want to obtain a list of cluster options in the specific region and specify values when creating a cluster. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
describeOrderableClusterOptions :: forall eff. DescribeOrderableClusterOptionsMessage -> Aff (exception :: EXCEPTION | eff) OrderableClusterOptionsMessage
describeOrderableClusterOptions = Request.request serviceName "describeOrderableClusterOptions" 


-- | <p>Returns a list of the available reserved node offerings by Amazon Redshift with their descriptions including the node type, the fixed and recurring costs of reserving the node and duration the node will be reserved for you. These descriptions help you determine which reserve node offering you want to purchase. You then use the unique offering ID in you call to <a>PurchaseReservedNodeOffering</a> to reserve one or more nodes for your Amazon Redshift cluster. </p> <p> For more information about reserved node offerings, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html">Purchasing Reserved Nodes</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
describeReservedNodeOfferings :: forall eff. DescribeReservedNodeOfferingsMessage -> Aff (exception :: EXCEPTION | eff) ReservedNodeOfferingsMessage
describeReservedNodeOfferings = Request.request serviceName "describeReservedNodeOfferings" 


-- | <p>Returns the descriptions of the reserved nodes.</p>
describeReservedNodes :: forall eff. DescribeReservedNodesMessage -> Aff (exception :: EXCEPTION | eff) ReservedNodesMessage
describeReservedNodes = Request.request serviceName "describeReservedNodes" 


-- | <p>Returns information about the last resize operation for the specified cluster. If no resize operation has ever been initiated for the specified cluster, a <code>HTTP 404</code> error is returned. If a resize operation was initiated and completed, the status of the resize remains as <code>SUCCEEDED</code> until the next resize. </p> <p>A resize operation can be requested using <a>ModifyCluster</a> and specifying a different number or type of nodes for the cluster. </p>
describeResize :: forall eff. DescribeResizeMessage -> Aff (exception :: EXCEPTION | eff) ResizeProgressMessage
describeResize = Request.request serviceName "describeResize" 


-- | <p>Returns a list of snapshot copy grants owned by the AWS account in the destination region.</p> <p> For more information about managing snapshot copy grants, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html">Amazon Redshift Database Encryption</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
describeSnapshotCopyGrants :: forall eff. DescribeSnapshotCopyGrantsMessage -> Aff (exception :: EXCEPTION | eff) SnapshotCopyGrantMessage
describeSnapshotCopyGrants = Request.request serviceName "describeSnapshotCopyGrants" 


-- | <p>Lists the status of one or more table restore requests made using the <a>RestoreTableFromClusterSnapshot</a> API action. If you don't specify a value for the <code>TableRestoreRequestId</code> parameter, then <code>DescribeTableRestoreStatus</code> returns the status of all table restore requests ordered by the date and time of the request in ascending order. Otherwise <code>DescribeTableRestoreStatus</code> returns the status of the table specified by <code>TableRestoreRequestId</code>.</p>
describeTableRestoreStatus :: forall eff. DescribeTableRestoreStatusMessage -> Aff (exception :: EXCEPTION | eff) TableRestoreStatusMessage
describeTableRestoreStatus = Request.request serviceName "describeTableRestoreStatus" 


-- | <p>Returns a list of tags. You can return tags from a specific resource by specifying an ARN, or you can return all tags for a given type of resource, such as clusters, snapshots, and so on.</p> <p>The following are limitations for <code>DescribeTags</code>: </p> <ul> <li> <p>You cannot specify an ARN and a resource-type value together in the same request.</p> </li> <li> <p>You cannot use the <code>MaxRecords</code> and <code>Marker</code> parameters together with the ARN parameter.</p> </li> <li> <p>The <code>MaxRecords</code> parameter can be a range from 10 to 50 results to return in a request.</p> </li> </ul> <p>If you specify both tag keys and tag values in the same request, Amazon Redshift returns all resources that match any combination of the specified keys and values. For example, if you have <code>owner</code> and <code>environment</code> for tag keys, and <code>admin</code> and <code>test</code> for tag values, all resources that have any combination of those values are returned.</p> <p>If both tag keys and values are omitted from the request, resources are returned regardless of whether they have tag keys or values associated with them.</p>
describeTags :: forall eff. DescribeTagsMessage -> Aff (exception :: EXCEPTION | eff) TaggedResourceListMessage
describeTags = Request.request serviceName "describeTags" 


-- | <p>Stops logging information, such as queries and connection attempts, for the specified Amazon Redshift cluster.</p>
disableLogging :: forall eff. DisableLoggingMessage -> Aff (exception :: EXCEPTION | eff) LoggingStatus
disableLogging = Request.request serviceName "disableLogging" 


-- | <p>Disables the automatic copying of snapshots from one region to another region for a specified cluster.</p> <p>If your cluster and its snapshots are encrypted using a customer master key (CMK) from AWS KMS, use <a>DeleteSnapshotCopyGrant</a> to delete the grant that grants Amazon Redshift permission to the CMK in the destination region. </p>
disableSnapshotCopy :: forall eff. DisableSnapshotCopyMessage -> Aff (exception :: EXCEPTION | eff) DisableSnapshotCopyResult
disableSnapshotCopy = Request.request serviceName "disableSnapshotCopy" 


-- | <p>Starts logging information, such as queries and connection attempts, for the specified Amazon Redshift cluster.</p>
enableLogging :: forall eff. EnableLoggingMessage -> Aff (exception :: EXCEPTION | eff) LoggingStatus
enableLogging = Request.request serviceName "enableLogging" 


-- | <p>Enables the automatic copy of snapshots from one region to another region for a specified cluster.</p>
enableSnapshotCopy :: forall eff. EnableSnapshotCopyMessage -> Aff (exception :: EXCEPTION | eff) EnableSnapshotCopyResult
enableSnapshotCopy = Request.request serviceName "enableSnapshotCopy" 


-- | <p>Returns a database user name and temporary password with temporary authorization to log on to an Amazon Redshift database. The action returns the database user name prefixed with <code>IAM:</code> if <code>AutoCreate</code> is <code>False</code> or <code>IAMA:</code> if <code>AutoCreate</code> is <code>True</code>. You can optionally specify one or more database user groups that the user will join at log on. By default, the temporary credentials expire in 900 seconds. You can optionally specify a duration between 900 seconds (15 minutes) and 3600 seconds (60 minutes). For more information, see <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/generating-user-credentials.html">Using IAM Authentication to Generate Database User Credentials</a> in the Amazon Redshift Cluster Management Guide.</p> <p>The AWS Identity and Access Management (IAM)user or role that executes GetClusterCredentials must have an IAM policy attached that allows access to all necessary actions and resources. For more information about permissions, see <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html#redshift-policy-resources.getclustercredentials-resources">Resource Policies for GetClusterCredentials</a> in the Amazon Redshift Cluster Management Guide.</p> <p>If the <code>DbGroups</code> parameter is specified, the IAM policy must allow the <code>redshift:JoinGroup</code> action with access to the listed <code>dbgroups</code>. </p> <p>In addition, if the <code>AutoCreate</code> parameter is set to <code>True</code>, then the policy must include the <code>redshift:CreateClusterUser</code> privilege.</p> <p>If the <code>DbName</code> parameter is specified, the IAM policy must allow access to the resource <code>dbname</code> for the specified database name. </p>
getClusterCredentials :: forall eff. GetClusterCredentialsMessage -> Aff (exception :: EXCEPTION | eff) ClusterCredentials
getClusterCredentials = Request.request serviceName "getClusterCredentials" 


-- | <p>Modifies the settings for a cluster. For example, you can add another security or parameter group, update the preferred maintenance window, or change the master user password. Resetting a cluster password or modifying the security groups associated with a cluster do not need a reboot. However, modifying a parameter group requires a reboot for parameters to take effect. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p> <p>You can also change node type and the number of nodes to scale up or down the cluster. When resizing a cluster, you must specify both the number of nodes and the node type even if one of the parameters does not change.</p>
modifyCluster :: forall eff. ModifyClusterMessage -> Aff (exception :: EXCEPTION | eff) ModifyClusterResult
modifyCluster = Request.request serviceName "modifyCluster" 


-- | <p>Modifies the list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.</p> <p>A cluster can have up to 10 IAM roles associated at any time.</p>
modifyClusterIamRoles :: forall eff. ModifyClusterIamRolesMessage -> Aff (exception :: EXCEPTION | eff) ModifyClusterIamRolesResult
modifyClusterIamRoles = Request.request serviceName "modifyClusterIamRoles" 


-- | <p>Modifies the parameters of a parameter group.</p> <p> For more information about parameters and parameter groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html">Amazon Redshift Parameter Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
modifyClusterParameterGroup :: forall eff. ModifyClusterParameterGroupMessage -> Aff (exception :: EXCEPTION | eff) ClusterParameterGroupNameMessage
modifyClusterParameterGroup = Request.request serviceName "modifyClusterParameterGroup" 


-- | <p>Modifies a cluster subnet group to include the specified list of VPC subnets. The operation replaces the existing list of subnets with the new list of subnets.</p>
modifyClusterSubnetGroup :: forall eff. ModifyClusterSubnetGroupMessage -> Aff (exception :: EXCEPTION | eff) ModifyClusterSubnetGroupResult
modifyClusterSubnetGroup = Request.request serviceName "modifyClusterSubnetGroup" 


-- | <p>Modifies an existing Amazon Redshift event notification subscription.</p>
modifyEventSubscription :: forall eff. ModifyEventSubscriptionMessage -> Aff (exception :: EXCEPTION | eff) ModifyEventSubscriptionResult
modifyEventSubscription = Request.request serviceName "modifyEventSubscription" 


-- | <p>Modifies the number of days to retain automated snapshots in the destination region after they are copied from the source region.</p>
modifySnapshotCopyRetentionPeriod :: forall eff. ModifySnapshotCopyRetentionPeriodMessage -> Aff (exception :: EXCEPTION | eff) ModifySnapshotCopyRetentionPeriodResult
modifySnapshotCopyRetentionPeriod = Request.request serviceName "modifySnapshotCopyRetentionPeriod" 


-- | <p>Allows you to purchase reserved nodes. Amazon Redshift offers a predefined set of reserved node offerings. You can purchase one or more of the offerings. You can call the <a>DescribeReservedNodeOfferings</a> API to obtain the available reserved node offerings. You can call this API by providing a specific reserved node offering and the number of nodes you want to reserve. </p> <p> For more information about reserved node offerings, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html">Purchasing Reserved Nodes</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
purchaseReservedNodeOffering :: forall eff. PurchaseReservedNodeOfferingMessage -> Aff (exception :: EXCEPTION | eff) PurchaseReservedNodeOfferingResult
purchaseReservedNodeOffering = Request.request serviceName "purchaseReservedNodeOffering" 


-- | <p>Reboots a cluster. This action is taken as soon as possible. It results in a momentary outage to the cluster, during which the cluster status is set to <code>rebooting</code>. A cluster event is created when the reboot is completed. Any pending cluster modifications (see <a>ModifyCluster</a>) are applied at this reboot. For more information about managing clusters, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html">Amazon Redshift Clusters</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
rebootCluster :: forall eff. RebootClusterMessage -> Aff (exception :: EXCEPTION | eff) RebootClusterResult
rebootCluster = Request.request serviceName "rebootCluster" 


-- | <p>Sets one or more parameters of the specified parameter group to their default values and sets the source values of the parameters to "engine-default". To reset the entire parameter group specify the <i>ResetAllParameters</i> parameter. For parameter changes to take effect you must reboot any associated clusters. </p>
resetClusterParameterGroup :: forall eff. ResetClusterParameterGroupMessage -> Aff (exception :: EXCEPTION | eff) ClusterParameterGroupNameMessage
resetClusterParameterGroup = Request.request serviceName "resetClusterParameterGroup" 


-- | <p>Creates a new cluster from a snapshot. By default, Amazon Redshift creates the resulting cluster with the same configuration as the original cluster from which the snapshot was created, except that the new cluster is created with the default cluster security and parameter groups. After Amazon Redshift creates the cluster, you can use the <a>ModifyCluster</a> API to associate a different security group and different parameter group with the restored cluster. If you are using a DS node type, you can also choose to change to another DS node type of the same size during restore.</p> <p>If you restore a cluster into a VPC, you must provide a cluster subnet group where you want the cluster restored.</p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
restoreFromClusterSnapshot :: forall eff. RestoreFromClusterSnapshotMessage -> Aff (exception :: EXCEPTION | eff) RestoreFromClusterSnapshotResult
restoreFromClusterSnapshot = Request.request serviceName "restoreFromClusterSnapshot" 


-- | <p>Creates a new table from a table in an Amazon Redshift cluster snapshot. You must create the new table within the Amazon Redshift cluster that the snapshot was taken from.</p> <p>You cannot use <code>RestoreTableFromClusterSnapshot</code> to restore a table with the same name as an existing table in an Amazon Redshift cluster. That is, you cannot overwrite an existing table in a cluster with a restored table. If you want to replace your original table with a new, restored table, then rename or drop your original table before you call <code>RestoreTableFromClusterSnapshot</code>. When you have renamed your original table, then you can pass the original name of the table as the <code>NewTableName</code> parameter value in the call to <code>RestoreTableFromClusterSnapshot</code>. This way, you can replace the original table with the table created from the snapshot.</p>
restoreTableFromClusterSnapshot :: forall eff. RestoreTableFromClusterSnapshotMessage -> Aff (exception :: EXCEPTION | eff) RestoreTableFromClusterSnapshotResult
restoreTableFromClusterSnapshot = Request.request serviceName "restoreTableFromClusterSnapshot" 


-- | <p>Revokes an ingress rule in an Amazon Redshift security group for a previously authorized IP range or Amazon EC2 security group. To add an ingress rule, see <a>AuthorizeClusterSecurityGroupIngress</a>. For information about managing security groups, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html">Amazon Redshift Cluster Security Groups</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
revokeClusterSecurityGroupIngress :: forall eff. RevokeClusterSecurityGroupIngressMessage -> Aff (exception :: EXCEPTION | eff) RevokeClusterSecurityGroupIngressResult
revokeClusterSecurityGroupIngress = Request.request serviceName "revokeClusterSecurityGroupIngress" 


-- | <p>Removes the ability of the specified AWS customer account to restore the specified snapshot. If the account is currently restoring the snapshot, the restore will run to completion.</p> <p> For more information about working with snapshots, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html">Amazon Redshift Snapshots</a> in the <i>Amazon Redshift Cluster Management Guide</i>.</p>
revokeSnapshotAccess :: forall eff. RevokeSnapshotAccessMessage -> Aff (exception :: EXCEPTION | eff) RevokeSnapshotAccessResult
revokeSnapshotAccess = Request.request serviceName "revokeSnapshotAccess" 


-- | <p>Rotates the encryption keys for a cluster.</p>
rotateEncryptionKey :: forall eff. RotateEncryptionKeyMessage -> Aff (exception :: EXCEPTION | eff) RotateEncryptionKeyResult
rotateEncryptionKey = Request.request serviceName "rotateEncryptionKey" 


-- | <p>The owner of the specified snapshot has not authorized your account to access the snapshot.</p>
newtype AccessToSnapshotDeniedFault = AccessToSnapshotDeniedFault Types.NoArguments
derive instance newtypeAccessToSnapshotDeniedFault :: Newtype AccessToSnapshotDeniedFault _
derive instance repGenericAccessToSnapshotDeniedFault :: Generic AccessToSnapshotDeniedFault _
instance showAccessToSnapshotDeniedFault :: Show AccessToSnapshotDeniedFault where
  show = genericShow
instance decodeAccessToSnapshotDeniedFault :: Decode AccessToSnapshotDeniedFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessToSnapshotDeniedFault :: Encode AccessToSnapshotDeniedFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an AWS customer account authorized to restore a snapshot.</p>
newtype AccountWithRestoreAccess = AccountWithRestoreAccess 
  { "AccountId" :: NullOrUndefined.NullOrUndefined (String)
  , "AccountAlias" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAccountWithRestoreAccess :: Newtype AccountWithRestoreAccess _
derive instance repGenericAccountWithRestoreAccess :: Generic AccountWithRestoreAccess _
instance showAccountWithRestoreAccess :: Show AccountWithRestoreAccess where
  show = genericShow
instance decodeAccountWithRestoreAccess :: Decode AccountWithRestoreAccess where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountWithRestoreAccess :: Encode AccountWithRestoreAccess where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountsWithRestoreAccessList = AccountsWithRestoreAccessList (Array AccountWithRestoreAccess)
derive instance newtypeAccountsWithRestoreAccessList :: Newtype AccountsWithRestoreAccessList _
derive instance repGenericAccountsWithRestoreAccessList :: Generic AccountsWithRestoreAccessList _
instance showAccountsWithRestoreAccessList :: Show AccountsWithRestoreAccessList where
  show = genericShow
instance decodeAccountsWithRestoreAccessList :: Decode AccountsWithRestoreAccessList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountsWithRestoreAccessList :: Encode AccountsWithRestoreAccessList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified CIDR block or EC2 security group is already authorized for the specified cluster security group.</p>
newtype AuthorizationAlreadyExistsFault = AuthorizationAlreadyExistsFault Types.NoArguments
derive instance newtypeAuthorizationAlreadyExistsFault :: Newtype AuthorizationAlreadyExistsFault _
derive instance repGenericAuthorizationAlreadyExistsFault :: Generic AuthorizationAlreadyExistsFault _
instance showAuthorizationAlreadyExistsFault :: Show AuthorizationAlreadyExistsFault where
  show = genericShow
instance decodeAuthorizationAlreadyExistsFault :: Decode AuthorizationAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizationAlreadyExistsFault :: Encode AuthorizationAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified CIDR IP range or EC2 security group is not authorized for the specified cluster security group.</p>
newtype AuthorizationNotFoundFault = AuthorizationNotFoundFault Types.NoArguments
derive instance newtypeAuthorizationNotFoundFault :: Newtype AuthorizationNotFoundFault _
derive instance repGenericAuthorizationNotFoundFault :: Generic AuthorizationNotFoundFault _
instance showAuthorizationNotFoundFault :: Show AuthorizationNotFoundFault where
  show = genericShow
instance decodeAuthorizationNotFoundFault :: Decode AuthorizationNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizationNotFoundFault :: Encode AuthorizationNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The authorization quota for the cluster security group has been reached.</p>
newtype AuthorizationQuotaExceededFault = AuthorizationQuotaExceededFault Types.NoArguments
derive instance newtypeAuthorizationQuotaExceededFault :: Newtype AuthorizationQuotaExceededFault _
derive instance repGenericAuthorizationQuotaExceededFault :: Generic AuthorizationQuotaExceededFault _
instance showAuthorizationQuotaExceededFault :: Show AuthorizationQuotaExceededFault where
  show = genericShow
instance decodeAuthorizationQuotaExceededFault :: Decode AuthorizationQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizationQuotaExceededFault :: Encode AuthorizationQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype AuthorizeClusterSecurityGroupIngressMessage = AuthorizeClusterSecurityGroupIngressMessage 
  { "ClusterSecurityGroupName" :: (String)
  , "CIDRIP" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAuthorizeClusterSecurityGroupIngressMessage :: Newtype AuthorizeClusterSecurityGroupIngressMessage _
derive instance repGenericAuthorizeClusterSecurityGroupIngressMessage :: Generic AuthorizeClusterSecurityGroupIngressMessage _
instance showAuthorizeClusterSecurityGroupIngressMessage :: Show AuthorizeClusterSecurityGroupIngressMessage where
  show = genericShow
instance decodeAuthorizeClusterSecurityGroupIngressMessage :: Decode AuthorizeClusterSecurityGroupIngressMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizeClusterSecurityGroupIngressMessage :: Encode AuthorizeClusterSecurityGroupIngressMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthorizeClusterSecurityGroupIngressResult = AuthorizeClusterSecurityGroupIngressResult 
  { "ClusterSecurityGroup" :: NullOrUndefined.NullOrUndefined (ClusterSecurityGroup)
  }
derive instance newtypeAuthorizeClusterSecurityGroupIngressResult :: Newtype AuthorizeClusterSecurityGroupIngressResult _
derive instance repGenericAuthorizeClusterSecurityGroupIngressResult :: Generic AuthorizeClusterSecurityGroupIngressResult _
instance showAuthorizeClusterSecurityGroupIngressResult :: Show AuthorizeClusterSecurityGroupIngressResult where
  show = genericShow
instance decodeAuthorizeClusterSecurityGroupIngressResult :: Decode AuthorizeClusterSecurityGroupIngressResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizeClusterSecurityGroupIngressResult :: Encode AuthorizeClusterSecurityGroupIngressResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype AuthorizeSnapshotAccessMessage = AuthorizeSnapshotAccessMessage 
  { "SnapshotIdentifier" :: (String)
  , "SnapshotClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "AccountWithRestoreAccess" :: (String)
  }
derive instance newtypeAuthorizeSnapshotAccessMessage :: Newtype AuthorizeSnapshotAccessMessage _
derive instance repGenericAuthorizeSnapshotAccessMessage :: Generic AuthorizeSnapshotAccessMessage _
instance showAuthorizeSnapshotAccessMessage :: Show AuthorizeSnapshotAccessMessage where
  show = genericShow
instance decodeAuthorizeSnapshotAccessMessage :: Decode AuthorizeSnapshotAccessMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizeSnapshotAccessMessage :: Encode AuthorizeSnapshotAccessMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthorizeSnapshotAccessResult = AuthorizeSnapshotAccessResult 
  { "Snapshot" :: NullOrUndefined.NullOrUndefined (Snapshot)
  }
derive instance newtypeAuthorizeSnapshotAccessResult :: Newtype AuthorizeSnapshotAccessResult _
derive instance repGenericAuthorizeSnapshotAccessResult :: Generic AuthorizeSnapshotAccessResult _
instance showAuthorizeSnapshotAccessResult :: Show AuthorizeSnapshotAccessResult where
  show = genericShow
instance decodeAuthorizeSnapshotAccessResult :: Decode AuthorizeSnapshotAccessResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizeSnapshotAccessResult :: Encode AuthorizeSnapshotAccessResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an availability zone.</p>
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


newtype AvailabilityZoneList = AvailabilityZoneList (Array AvailabilityZone)
derive instance newtypeAvailabilityZoneList :: Newtype AvailabilityZoneList _
derive instance repGenericAvailabilityZoneList :: Generic AvailabilityZoneList _
instance showAvailabilityZoneList :: Show AvailabilityZoneList where
  show = genericShow
instance decodeAvailabilityZoneList :: Decode AvailabilityZoneList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZoneList :: Encode AvailabilityZoneList where
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


-- | <p>Could not find the specified S3 bucket.</p>
newtype BucketNotFoundFault = BucketNotFoundFault Types.NoArguments
derive instance newtypeBucketNotFoundFault :: Newtype BucketNotFoundFault _
derive instance repGenericBucketNotFoundFault :: Generic BucketNotFoundFault _
instance showBucketNotFoundFault :: Show BucketNotFoundFault where
  show = genericShow
instance decodeBucketNotFoundFault :: Decode BucketNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketNotFoundFault :: Encode BucketNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a cluster.</p>
newtype Cluster = Cluster 
  { "ClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "ModifyStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "MasterUsername" :: NullOrUndefined.NullOrUndefined (String)
  , "DBName" :: NullOrUndefined.NullOrUndefined (String)
  , "Endpoint" :: NullOrUndefined.NullOrUndefined (Endpoint)
  , "ClusterCreateTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined.NullOrUndefined (Int)
  , "ClusterSecurityGroups" :: NullOrUndefined.NullOrUndefined (ClusterSecurityGroupMembershipList)
  , "VpcSecurityGroups" :: NullOrUndefined.NullOrUndefined (VpcSecurityGroupMembershipList)
  , "ClusterParameterGroups" :: NullOrUndefined.NullOrUndefined (ClusterParameterGroupStatusList)
  , "ClusterSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (String)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "PendingModifiedValues" :: NullOrUndefined.NullOrUndefined (PendingModifiedValues)
  , "ClusterVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "AllowVersionUpgrade" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "NumberOfNodes" :: NullOrUndefined.NullOrUndefined (Int)
  , "PubliclyAccessible" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Encrypted" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RestoreStatus" :: NullOrUndefined.NullOrUndefined (RestoreStatus)
  , "HsmStatus" :: NullOrUndefined.NullOrUndefined (HsmStatus)
  , "ClusterSnapshotCopyStatus" :: NullOrUndefined.NullOrUndefined (ClusterSnapshotCopyStatus)
  , "ClusterPublicKey" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterNodes" :: NullOrUndefined.NullOrUndefined (ClusterNodesList)
  , "ElasticIpStatus" :: NullOrUndefined.NullOrUndefined (ElasticIpStatus)
  , "ClusterRevisionNumber" :: NullOrUndefined.NullOrUndefined (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (String)
  , "EnhancedVpcRouting" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "IamRoles" :: NullOrUndefined.NullOrUndefined (ClusterIamRoleList)
  }
derive instance newtypeCluster :: Newtype Cluster _
derive instance repGenericCluster :: Generic Cluster _
instance showCluster :: Show Cluster where
  show = genericShow
instance decodeCluster :: Decode Cluster where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCluster :: Encode Cluster where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The account already has a cluster with the given identifier.</p>
newtype ClusterAlreadyExistsFault = ClusterAlreadyExistsFault Types.NoArguments
derive instance newtypeClusterAlreadyExistsFault :: Newtype ClusterAlreadyExistsFault _
derive instance repGenericClusterAlreadyExistsFault :: Generic ClusterAlreadyExistsFault _
instance showClusterAlreadyExistsFault :: Show ClusterAlreadyExistsFault where
  show = genericShow
instance decodeClusterAlreadyExistsFault :: Decode ClusterAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterAlreadyExistsFault :: Encode ClusterAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Temporary credentials with authorization to log on to an Amazon Redshift database. </p>
newtype ClusterCredentials = ClusterCredentials 
  { "DbUser" :: NullOrUndefined.NullOrUndefined (String)
  , "DbPassword" :: NullOrUndefined.NullOrUndefined (SensitiveString)
  , "Expiration" :: NullOrUndefined.NullOrUndefined (TStamp)
  }
derive instance newtypeClusterCredentials :: Newtype ClusterCredentials _
derive instance repGenericClusterCredentials :: Generic ClusterCredentials _
instance showClusterCredentials :: Show ClusterCredentials where
  show = genericShow
instance decodeClusterCredentials :: Decode ClusterCredentials where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterCredentials :: Encode ClusterCredentials where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An AWS Identity and Access Management (IAM) role that can be used by the associated Amazon Redshift cluster to access other AWS services.</p>
newtype ClusterIamRole = ClusterIamRole 
  { "IamRoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "ApplyStatus" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeClusterIamRole :: Newtype ClusterIamRole _
derive instance repGenericClusterIamRole :: Generic ClusterIamRole _
instance showClusterIamRole :: Show ClusterIamRole where
  show = genericShow
instance decodeClusterIamRole :: Decode ClusterIamRole where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterIamRole :: Encode ClusterIamRole where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterIamRoleList = ClusterIamRoleList (Array ClusterIamRole)
derive instance newtypeClusterIamRoleList :: Newtype ClusterIamRoleList _
derive instance repGenericClusterIamRoleList :: Generic ClusterIamRoleList _
instance showClusterIamRoleList :: Show ClusterIamRoleList where
  show = genericShow
instance decodeClusterIamRoleList :: Decode ClusterIamRoleList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterIamRoleList :: Encode ClusterIamRoleList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterList = ClusterList (Array Cluster)
derive instance newtypeClusterList :: Newtype ClusterList _
derive instance repGenericClusterList :: Generic ClusterList _
instance showClusterList :: Show ClusterList where
  show = genericShow
instance decodeClusterList :: Decode ClusterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterList :: Encode ClusterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The identifier of a node in a cluster.</p>
newtype ClusterNode = ClusterNode 
  { "NodeRole" :: NullOrUndefined.NullOrUndefined (String)
  , "PrivateIPAddress" :: NullOrUndefined.NullOrUndefined (String)
  , "PublicIPAddress" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeClusterNode :: Newtype ClusterNode _
derive instance repGenericClusterNode :: Generic ClusterNode _
instance showClusterNode :: Show ClusterNode where
  show = genericShow
instance decodeClusterNode :: Decode ClusterNode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterNode :: Encode ClusterNode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterNodesList = ClusterNodesList (Array ClusterNode)
derive instance newtypeClusterNodesList :: Newtype ClusterNodesList _
derive instance repGenericClusterNodesList :: Generic ClusterNodesList _
instance showClusterNodesList :: Show ClusterNodesList where
  show = genericShow
instance decodeClusterNodesList :: Decode ClusterNodesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterNodesList :: Encode ClusterNodesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ClusterIdentifier</code> parameter does not refer to an existing cluster. </p>
newtype ClusterNotFoundFault = ClusterNotFoundFault Types.NoArguments
derive instance newtypeClusterNotFoundFault :: Newtype ClusterNotFoundFault _
derive instance repGenericClusterNotFoundFault :: Generic ClusterNotFoundFault _
instance showClusterNotFoundFault :: Show ClusterNotFoundFault where
  show = genericShow
instance decodeClusterNotFoundFault :: Decode ClusterNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterNotFoundFault :: Encode ClusterNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a parameter group.</p>
newtype ClusterParameterGroup = ClusterParameterGroup 
  { "ParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterGroupFamily" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeClusterParameterGroup :: Newtype ClusterParameterGroup _
derive instance repGenericClusterParameterGroup :: Generic ClusterParameterGroup _
instance showClusterParameterGroup :: Show ClusterParameterGroup where
  show = genericShow
instance decodeClusterParameterGroup :: Decode ClusterParameterGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterParameterGroup :: Encode ClusterParameterGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A cluster parameter group with the same name already exists.</p>
newtype ClusterParameterGroupAlreadyExistsFault = ClusterParameterGroupAlreadyExistsFault Types.NoArguments
derive instance newtypeClusterParameterGroupAlreadyExistsFault :: Newtype ClusterParameterGroupAlreadyExistsFault _
derive instance repGenericClusterParameterGroupAlreadyExistsFault :: Generic ClusterParameterGroupAlreadyExistsFault _
instance showClusterParameterGroupAlreadyExistsFault :: Show ClusterParameterGroupAlreadyExistsFault where
  show = genericShow
instance decodeClusterParameterGroupAlreadyExistsFault :: Decode ClusterParameterGroupAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterParameterGroupAlreadyExistsFault :: Encode ClusterParameterGroupAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output from the <a>DescribeClusterParameters</a> action. </p>
newtype ClusterParameterGroupDetails = ClusterParameterGroupDetails 
  { "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeClusterParameterGroupDetails :: Newtype ClusterParameterGroupDetails _
derive instance repGenericClusterParameterGroupDetails :: Generic ClusterParameterGroupDetails _
instance showClusterParameterGroupDetails :: Show ClusterParameterGroupDetails where
  show = genericShow
instance decodeClusterParameterGroupDetails :: Decode ClusterParameterGroupDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterParameterGroupDetails :: Encode ClusterParameterGroupDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ClusterParameterGroupNameMessage = ClusterParameterGroupNameMessage 
  { "ParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterGroupStatus" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeClusterParameterGroupNameMessage :: Newtype ClusterParameterGroupNameMessage _
derive instance repGenericClusterParameterGroupNameMessage :: Generic ClusterParameterGroupNameMessage _
instance showClusterParameterGroupNameMessage :: Show ClusterParameterGroupNameMessage where
  show = genericShow
instance decodeClusterParameterGroupNameMessage :: Decode ClusterParameterGroupNameMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterParameterGroupNameMessage :: Encode ClusterParameterGroupNameMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameter group name does not refer to an existing parameter group.</p>
newtype ClusterParameterGroupNotFoundFault = ClusterParameterGroupNotFoundFault Types.NoArguments
derive instance newtypeClusterParameterGroupNotFoundFault :: Newtype ClusterParameterGroupNotFoundFault _
derive instance repGenericClusterParameterGroupNotFoundFault :: Generic ClusterParameterGroupNotFoundFault _
instance showClusterParameterGroupNotFoundFault :: Show ClusterParameterGroupNotFoundFault where
  show = genericShow
instance decodeClusterParameterGroupNotFoundFault :: Decode ClusterParameterGroupNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterParameterGroupNotFoundFault :: Encode ClusterParameterGroupNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request would result in the user exceeding the allowed number of cluster parameter groups. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype ClusterParameterGroupQuotaExceededFault = ClusterParameterGroupQuotaExceededFault Types.NoArguments
derive instance newtypeClusterParameterGroupQuotaExceededFault :: Newtype ClusterParameterGroupQuotaExceededFault _
derive instance repGenericClusterParameterGroupQuotaExceededFault :: Generic ClusterParameterGroupQuotaExceededFault _
instance showClusterParameterGroupQuotaExceededFault :: Show ClusterParameterGroupQuotaExceededFault where
  show = genericShow
instance decodeClusterParameterGroupQuotaExceededFault :: Decode ClusterParameterGroupQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterParameterGroupQuotaExceededFault :: Encode ClusterParameterGroupQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the status of a parameter group.</p>
newtype ClusterParameterGroupStatus = ClusterParameterGroupStatus 
  { "ParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterApplyStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterParameterStatusList" :: NullOrUndefined.NullOrUndefined (ClusterParameterStatusList)
  }
derive instance newtypeClusterParameterGroupStatus :: Newtype ClusterParameterGroupStatus _
derive instance repGenericClusterParameterGroupStatus :: Generic ClusterParameterGroupStatus _
instance showClusterParameterGroupStatus :: Show ClusterParameterGroupStatus where
  show = genericShow
instance decodeClusterParameterGroupStatus :: Decode ClusterParameterGroupStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterParameterGroupStatus :: Encode ClusterParameterGroupStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterParameterGroupStatusList = ClusterParameterGroupStatusList (Array ClusterParameterGroupStatus)
derive instance newtypeClusterParameterGroupStatusList :: Newtype ClusterParameterGroupStatusList _
derive instance repGenericClusterParameterGroupStatusList :: Generic ClusterParameterGroupStatusList _
instance showClusterParameterGroupStatusList :: Show ClusterParameterGroupStatusList where
  show = genericShow
instance decodeClusterParameterGroupStatusList :: Decode ClusterParameterGroupStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterParameterGroupStatusList :: Encode ClusterParameterGroupStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output from the <a>DescribeClusterParameterGroups</a> action. </p>
newtype ClusterParameterGroupsMessage = ClusterParameterGroupsMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterGroups" :: NullOrUndefined.NullOrUndefined (ParameterGroupList)
  }
derive instance newtypeClusterParameterGroupsMessage :: Newtype ClusterParameterGroupsMessage _
derive instance repGenericClusterParameterGroupsMessage :: Generic ClusterParameterGroupsMessage _
instance showClusterParameterGroupsMessage :: Show ClusterParameterGroupsMessage where
  show = genericShow
instance decodeClusterParameterGroupsMessage :: Decode ClusterParameterGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterParameterGroupsMessage :: Encode ClusterParameterGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the status of a parameter group.</p>
newtype ClusterParameterStatus = ClusterParameterStatus 
  { "ParameterName" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterApplyStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterApplyErrorDescription" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeClusterParameterStatus :: Newtype ClusterParameterStatus _
derive instance repGenericClusterParameterStatus :: Generic ClusterParameterStatus _
instance showClusterParameterStatus :: Show ClusterParameterStatus where
  show = genericShow
instance decodeClusterParameterStatus :: Decode ClusterParameterStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterParameterStatus :: Encode ClusterParameterStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterParameterStatusList = ClusterParameterStatusList (Array ClusterParameterStatus)
derive instance newtypeClusterParameterStatusList :: Newtype ClusterParameterStatusList _
derive instance repGenericClusterParameterStatusList :: Generic ClusterParameterStatusList _
instance showClusterParameterStatusList :: Show ClusterParameterStatusList where
  show = genericShow
instance decodeClusterParameterStatusList :: Decode ClusterParameterStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterParameterStatusList :: Encode ClusterParameterStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request would exceed the allowed number of cluster instances for this account. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype ClusterQuotaExceededFault = ClusterQuotaExceededFault Types.NoArguments
derive instance newtypeClusterQuotaExceededFault :: Newtype ClusterQuotaExceededFault _
derive instance repGenericClusterQuotaExceededFault :: Generic ClusterQuotaExceededFault _
instance showClusterQuotaExceededFault :: Show ClusterQuotaExceededFault where
  show = genericShow
instance decodeClusterQuotaExceededFault :: Decode ClusterQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterQuotaExceededFault :: Encode ClusterQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a security group.</p>
newtype ClusterSecurityGroup = ClusterSecurityGroup 
  { "ClusterSecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroups" :: NullOrUndefined.NullOrUndefined (EC2SecurityGroupList)
  , "IPRanges" :: NullOrUndefined.NullOrUndefined (IPRangeList)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeClusterSecurityGroup :: Newtype ClusterSecurityGroup _
derive instance repGenericClusterSecurityGroup :: Generic ClusterSecurityGroup _
instance showClusterSecurityGroup :: Show ClusterSecurityGroup where
  show = genericShow
instance decodeClusterSecurityGroup :: Decode ClusterSecurityGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSecurityGroup :: Encode ClusterSecurityGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A cluster security group with the same name already exists.</p>
newtype ClusterSecurityGroupAlreadyExistsFault = ClusterSecurityGroupAlreadyExistsFault Types.NoArguments
derive instance newtypeClusterSecurityGroupAlreadyExistsFault :: Newtype ClusterSecurityGroupAlreadyExistsFault _
derive instance repGenericClusterSecurityGroupAlreadyExistsFault :: Generic ClusterSecurityGroupAlreadyExistsFault _
instance showClusterSecurityGroupAlreadyExistsFault :: Show ClusterSecurityGroupAlreadyExistsFault where
  show = genericShow
instance decodeClusterSecurityGroupAlreadyExistsFault :: Decode ClusterSecurityGroupAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSecurityGroupAlreadyExistsFault :: Encode ClusterSecurityGroupAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a cluster security group.</p>
newtype ClusterSecurityGroupMembership = ClusterSecurityGroupMembership 
  { "ClusterSecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeClusterSecurityGroupMembership :: Newtype ClusterSecurityGroupMembership _
derive instance repGenericClusterSecurityGroupMembership :: Generic ClusterSecurityGroupMembership _
instance showClusterSecurityGroupMembership :: Show ClusterSecurityGroupMembership where
  show = genericShow
instance decodeClusterSecurityGroupMembership :: Decode ClusterSecurityGroupMembership where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSecurityGroupMembership :: Encode ClusterSecurityGroupMembership where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterSecurityGroupMembershipList = ClusterSecurityGroupMembershipList (Array ClusterSecurityGroupMembership)
derive instance newtypeClusterSecurityGroupMembershipList :: Newtype ClusterSecurityGroupMembershipList _
derive instance repGenericClusterSecurityGroupMembershipList :: Generic ClusterSecurityGroupMembershipList _
instance showClusterSecurityGroupMembershipList :: Show ClusterSecurityGroupMembershipList where
  show = genericShow
instance decodeClusterSecurityGroupMembershipList :: Decode ClusterSecurityGroupMembershipList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSecurityGroupMembershipList :: Encode ClusterSecurityGroupMembershipList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ClusterSecurityGroupMessage = ClusterSecurityGroupMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterSecurityGroups" :: NullOrUndefined.NullOrUndefined (ClusterSecurityGroups)
  }
derive instance newtypeClusterSecurityGroupMessage :: Newtype ClusterSecurityGroupMessage _
derive instance repGenericClusterSecurityGroupMessage :: Generic ClusterSecurityGroupMessage _
instance showClusterSecurityGroupMessage :: Show ClusterSecurityGroupMessage where
  show = genericShow
instance decodeClusterSecurityGroupMessage :: Decode ClusterSecurityGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSecurityGroupMessage :: Encode ClusterSecurityGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterSecurityGroupNameList = ClusterSecurityGroupNameList (Array String)
derive instance newtypeClusterSecurityGroupNameList :: Newtype ClusterSecurityGroupNameList _
derive instance repGenericClusterSecurityGroupNameList :: Generic ClusterSecurityGroupNameList _
instance showClusterSecurityGroupNameList :: Show ClusterSecurityGroupNameList where
  show = genericShow
instance decodeClusterSecurityGroupNameList :: Decode ClusterSecurityGroupNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSecurityGroupNameList :: Encode ClusterSecurityGroupNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The cluster security group name does not refer to an existing cluster security group.</p>
newtype ClusterSecurityGroupNotFoundFault = ClusterSecurityGroupNotFoundFault Types.NoArguments
derive instance newtypeClusterSecurityGroupNotFoundFault :: Newtype ClusterSecurityGroupNotFoundFault _
derive instance repGenericClusterSecurityGroupNotFoundFault :: Generic ClusterSecurityGroupNotFoundFault _
instance showClusterSecurityGroupNotFoundFault :: Show ClusterSecurityGroupNotFoundFault where
  show = genericShow
instance decodeClusterSecurityGroupNotFoundFault :: Decode ClusterSecurityGroupNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSecurityGroupNotFoundFault :: Encode ClusterSecurityGroupNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request would result in the user exceeding the allowed number of cluster security groups. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype ClusterSecurityGroupQuotaExceededFault = ClusterSecurityGroupQuotaExceededFault Types.NoArguments
derive instance newtypeClusterSecurityGroupQuotaExceededFault :: Newtype ClusterSecurityGroupQuotaExceededFault _
derive instance repGenericClusterSecurityGroupQuotaExceededFault :: Generic ClusterSecurityGroupQuotaExceededFault _
instance showClusterSecurityGroupQuotaExceededFault :: Show ClusterSecurityGroupQuotaExceededFault where
  show = genericShow
instance decodeClusterSecurityGroupQuotaExceededFault :: Decode ClusterSecurityGroupQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSecurityGroupQuotaExceededFault :: Encode ClusterSecurityGroupQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterSecurityGroups = ClusterSecurityGroups (Array ClusterSecurityGroup)
derive instance newtypeClusterSecurityGroups :: Newtype ClusterSecurityGroups _
derive instance repGenericClusterSecurityGroups :: Generic ClusterSecurityGroups _
instance showClusterSecurityGroups :: Show ClusterSecurityGroups where
  show = genericShow
instance decodeClusterSecurityGroups :: Decode ClusterSecurityGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSecurityGroups :: Encode ClusterSecurityGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value specified as a snapshot identifier is already used by an existing snapshot.</p>
newtype ClusterSnapshotAlreadyExistsFault = ClusterSnapshotAlreadyExistsFault Types.NoArguments
derive instance newtypeClusterSnapshotAlreadyExistsFault :: Newtype ClusterSnapshotAlreadyExistsFault _
derive instance repGenericClusterSnapshotAlreadyExistsFault :: Generic ClusterSnapshotAlreadyExistsFault _
instance showClusterSnapshotAlreadyExistsFault :: Show ClusterSnapshotAlreadyExistsFault where
  show = genericShow
instance decodeClusterSnapshotAlreadyExistsFault :: Decode ClusterSnapshotAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSnapshotAlreadyExistsFault :: Encode ClusterSnapshotAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the destination region and retention period that are configured for cross-region snapshot copy.</p>
newtype ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus 
  { "DestinationRegion" :: NullOrUndefined.NullOrUndefined (String)
  , "RetentionPeriod" :: NullOrUndefined.NullOrUndefined (Number)
  , "SnapshotCopyGrantName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeClusterSnapshotCopyStatus :: Newtype ClusterSnapshotCopyStatus _
derive instance repGenericClusterSnapshotCopyStatus :: Generic ClusterSnapshotCopyStatus _
instance showClusterSnapshotCopyStatus :: Show ClusterSnapshotCopyStatus where
  show = genericShow
instance decodeClusterSnapshotCopyStatus :: Decode ClusterSnapshotCopyStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSnapshotCopyStatus :: Encode ClusterSnapshotCopyStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The snapshot identifier does not refer to an existing cluster snapshot.</p>
newtype ClusterSnapshotNotFoundFault = ClusterSnapshotNotFoundFault Types.NoArguments
derive instance newtypeClusterSnapshotNotFoundFault :: Newtype ClusterSnapshotNotFoundFault _
derive instance repGenericClusterSnapshotNotFoundFault :: Generic ClusterSnapshotNotFoundFault _
instance showClusterSnapshotNotFoundFault :: Show ClusterSnapshotNotFoundFault where
  show = genericShow
instance decodeClusterSnapshotNotFoundFault :: Decode ClusterSnapshotNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSnapshotNotFoundFault :: Encode ClusterSnapshotNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request would result in the user exceeding the allowed number of cluster snapshots.</p>
newtype ClusterSnapshotQuotaExceededFault = ClusterSnapshotQuotaExceededFault Types.NoArguments
derive instance newtypeClusterSnapshotQuotaExceededFault :: Newtype ClusterSnapshotQuotaExceededFault _
derive instance repGenericClusterSnapshotQuotaExceededFault :: Generic ClusterSnapshotQuotaExceededFault _
instance showClusterSnapshotQuotaExceededFault :: Show ClusterSnapshotQuotaExceededFault where
  show = genericShow
instance decodeClusterSnapshotQuotaExceededFault :: Decode ClusterSnapshotQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSnapshotQuotaExceededFault :: Encode ClusterSnapshotQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a subnet group.</p>
newtype ClusterSubnetGroup = ClusterSubnetGroup 
  { "ClusterSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (String)
  , "SubnetGroupStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "Subnets" :: NullOrUndefined.NullOrUndefined (SubnetList)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeClusterSubnetGroup :: Newtype ClusterSubnetGroup _
derive instance repGenericClusterSubnetGroup :: Generic ClusterSubnetGroup _
instance showClusterSubnetGroup :: Show ClusterSubnetGroup where
  show = genericShow
instance decodeClusterSubnetGroup :: Decode ClusterSubnetGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSubnetGroup :: Encode ClusterSubnetGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A <i>ClusterSubnetGroupName</i> is already used by an existing cluster subnet group. </p>
newtype ClusterSubnetGroupAlreadyExistsFault = ClusterSubnetGroupAlreadyExistsFault Types.NoArguments
derive instance newtypeClusterSubnetGroupAlreadyExistsFault :: Newtype ClusterSubnetGroupAlreadyExistsFault _
derive instance repGenericClusterSubnetGroupAlreadyExistsFault :: Generic ClusterSubnetGroupAlreadyExistsFault _
instance showClusterSubnetGroupAlreadyExistsFault :: Show ClusterSubnetGroupAlreadyExistsFault where
  show = genericShow
instance decodeClusterSubnetGroupAlreadyExistsFault :: Decode ClusterSubnetGroupAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSubnetGroupAlreadyExistsFault :: Encode ClusterSubnetGroupAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output from the <a>DescribeClusterSubnetGroups</a> action. </p>
newtype ClusterSubnetGroupMessage = ClusterSubnetGroupMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterSubnetGroups" :: NullOrUndefined.NullOrUndefined (ClusterSubnetGroups)
  }
derive instance newtypeClusterSubnetGroupMessage :: Newtype ClusterSubnetGroupMessage _
derive instance repGenericClusterSubnetGroupMessage :: Generic ClusterSubnetGroupMessage _
instance showClusterSubnetGroupMessage :: Show ClusterSubnetGroupMessage where
  show = genericShow
instance decodeClusterSubnetGroupMessage :: Decode ClusterSubnetGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSubnetGroupMessage :: Encode ClusterSubnetGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The cluster subnet group name does not refer to an existing cluster subnet group.</p>
newtype ClusterSubnetGroupNotFoundFault = ClusterSubnetGroupNotFoundFault Types.NoArguments
derive instance newtypeClusterSubnetGroupNotFoundFault :: Newtype ClusterSubnetGroupNotFoundFault _
derive instance repGenericClusterSubnetGroupNotFoundFault :: Generic ClusterSubnetGroupNotFoundFault _
instance showClusterSubnetGroupNotFoundFault :: Show ClusterSubnetGroupNotFoundFault where
  show = genericShow
instance decodeClusterSubnetGroupNotFoundFault :: Decode ClusterSubnetGroupNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSubnetGroupNotFoundFault :: Encode ClusterSubnetGroupNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request would result in user exceeding the allowed number of cluster subnet groups. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype ClusterSubnetGroupQuotaExceededFault = ClusterSubnetGroupQuotaExceededFault Types.NoArguments
derive instance newtypeClusterSubnetGroupQuotaExceededFault :: Newtype ClusterSubnetGroupQuotaExceededFault _
derive instance repGenericClusterSubnetGroupQuotaExceededFault :: Generic ClusterSubnetGroupQuotaExceededFault _
instance showClusterSubnetGroupQuotaExceededFault :: Show ClusterSubnetGroupQuotaExceededFault where
  show = genericShow
instance decodeClusterSubnetGroupQuotaExceededFault :: Decode ClusterSubnetGroupQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSubnetGroupQuotaExceededFault :: Encode ClusterSubnetGroupQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterSubnetGroups = ClusterSubnetGroups (Array ClusterSubnetGroup)
derive instance newtypeClusterSubnetGroups :: Newtype ClusterSubnetGroups _
derive instance repGenericClusterSubnetGroups :: Generic ClusterSubnetGroups _
instance showClusterSubnetGroups :: Show ClusterSubnetGroups where
  show = genericShow
instance decodeClusterSubnetGroups :: Decode ClusterSubnetGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSubnetGroups :: Encode ClusterSubnetGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request would result in user exceeding the allowed number of subnets in a cluster subnet groups. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype ClusterSubnetQuotaExceededFault = ClusterSubnetQuotaExceededFault Types.NoArguments
derive instance newtypeClusterSubnetQuotaExceededFault :: Newtype ClusterSubnetQuotaExceededFault _
derive instance repGenericClusterSubnetQuotaExceededFault :: Generic ClusterSubnetQuotaExceededFault _
instance showClusterSubnetQuotaExceededFault :: Show ClusterSubnetQuotaExceededFault where
  show = genericShow
instance decodeClusterSubnetQuotaExceededFault :: Decode ClusterSubnetQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSubnetQuotaExceededFault :: Encode ClusterSubnetQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a cluster version, including the parameter group family and description of the version.</p>
newtype ClusterVersion = ClusterVersion 
  { "ClusterVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterParameterGroupFamily" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeClusterVersion :: Newtype ClusterVersion _
derive instance repGenericClusterVersion :: Generic ClusterVersion _
instance showClusterVersion :: Show ClusterVersion where
  show = genericShow
instance decodeClusterVersion :: Decode ClusterVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterVersion :: Encode ClusterVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterVersionList = ClusterVersionList (Array ClusterVersion)
derive instance newtypeClusterVersionList :: Newtype ClusterVersionList _
derive instance repGenericClusterVersionList :: Generic ClusterVersionList _
instance showClusterVersionList :: Show ClusterVersionList where
  show = genericShow
instance decodeClusterVersionList :: Decode ClusterVersionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterVersionList :: Encode ClusterVersionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output from the <a>DescribeClusterVersions</a> action. </p>
newtype ClusterVersionsMessage = ClusterVersionsMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterVersions" :: NullOrUndefined.NullOrUndefined (ClusterVersionList)
  }
derive instance newtypeClusterVersionsMessage :: Newtype ClusterVersionsMessage _
derive instance repGenericClusterVersionsMessage :: Generic ClusterVersionsMessage _
instance showClusterVersionsMessage :: Show ClusterVersionsMessage where
  show = genericShow
instance decodeClusterVersionsMessage :: Decode ClusterVersionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterVersionsMessage :: Encode ClusterVersionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output from the <a>DescribeClusters</a> action. </p>
newtype ClustersMessage = ClustersMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "Clusters" :: NullOrUndefined.NullOrUndefined (ClusterList)
  }
derive instance newtypeClustersMessage :: Newtype ClustersMessage _
derive instance repGenericClustersMessage :: Generic ClustersMessage _
instance showClustersMessage :: Show ClustersMessage where
  show = genericShow
instance decodeClustersMessage :: Decode ClustersMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClustersMessage :: Encode ClustersMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CopyClusterSnapshotMessage = CopyClusterSnapshotMessage 
  { "SourceSnapshotIdentifier" :: (String)
  , "SourceSnapshotClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "TargetSnapshotIdentifier" :: (String)
  }
derive instance newtypeCopyClusterSnapshotMessage :: Newtype CopyClusterSnapshotMessage _
derive instance repGenericCopyClusterSnapshotMessage :: Generic CopyClusterSnapshotMessage _
instance showCopyClusterSnapshotMessage :: Show CopyClusterSnapshotMessage where
  show = genericShow
instance decodeCopyClusterSnapshotMessage :: Decode CopyClusterSnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopyClusterSnapshotMessage :: Encode CopyClusterSnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopyClusterSnapshotResult = CopyClusterSnapshotResult 
  { "Snapshot" :: NullOrUndefined.NullOrUndefined (Snapshot)
  }
derive instance newtypeCopyClusterSnapshotResult :: Newtype CopyClusterSnapshotResult _
derive instance repGenericCopyClusterSnapshotResult :: Generic CopyClusterSnapshotResult _
instance showCopyClusterSnapshotResult :: Show CopyClusterSnapshotResult where
  show = genericShow
instance decodeCopyClusterSnapshotResult :: Decode CopyClusterSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopyClusterSnapshotResult :: Encode CopyClusterSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Cross-region snapshot copy was temporarily disabled. Try your request again.</p>
newtype CopyToRegionDisabledFault = CopyToRegionDisabledFault Types.NoArguments
derive instance newtypeCopyToRegionDisabledFault :: Newtype CopyToRegionDisabledFault _
derive instance repGenericCopyToRegionDisabledFault :: Generic CopyToRegionDisabledFault _
instance showCopyToRegionDisabledFault :: Show CopyToRegionDisabledFault where
  show = genericShow
instance decodeCopyToRegionDisabledFault :: Decode CopyToRegionDisabledFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopyToRegionDisabledFault :: Encode CopyToRegionDisabledFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateClusterMessage = CreateClusterMessage 
  { "DBName" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterIdentifier" :: (String)
  , "ClusterType" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeType" :: (String)
  , "MasterUsername" :: (String)
  , "MasterUserPassword" :: (String)
  , "ClusterSecurityGroups" :: NullOrUndefined.NullOrUndefined (ClusterSecurityGroupNameList)
  , "VpcSecurityGroupIds" :: NullOrUndefined.NullOrUndefined (VpcSecurityGroupIdList)
  , "ClusterSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Port" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "ClusterVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "AllowVersionUpgrade" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "NumberOfNodes" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "PubliclyAccessible" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "Encrypted" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "HsmClientCertificateIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "HsmConfigurationIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "ElasticIp" :: NullOrUndefined.NullOrUndefined (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (String)
  , "EnhancedVpcRouting" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "AdditionalInfo" :: NullOrUndefined.NullOrUndefined (String)
  , "IamRoles" :: NullOrUndefined.NullOrUndefined (IamRoleArnList)
  }
derive instance newtypeCreateClusterMessage :: Newtype CreateClusterMessage _
derive instance repGenericCreateClusterMessage :: Generic CreateClusterMessage _
instance showCreateClusterMessage :: Show CreateClusterMessage where
  show = genericShow
instance decodeCreateClusterMessage :: Decode CreateClusterMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterMessage :: Encode CreateClusterMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateClusterParameterGroupMessage = CreateClusterParameterGroupMessage 
  { "ParameterGroupName" :: (String)
  , "ParameterGroupFamily" :: (String)
  , "Description" :: (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeCreateClusterParameterGroupMessage :: Newtype CreateClusterParameterGroupMessage _
derive instance repGenericCreateClusterParameterGroupMessage :: Generic CreateClusterParameterGroupMessage _
instance showCreateClusterParameterGroupMessage :: Show CreateClusterParameterGroupMessage where
  show = genericShow
instance decodeCreateClusterParameterGroupMessage :: Decode CreateClusterParameterGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterParameterGroupMessage :: Encode CreateClusterParameterGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateClusterParameterGroupResult = CreateClusterParameterGroupResult 
  { "ClusterParameterGroup" :: NullOrUndefined.NullOrUndefined (ClusterParameterGroup)
  }
derive instance newtypeCreateClusterParameterGroupResult :: Newtype CreateClusterParameterGroupResult _
derive instance repGenericCreateClusterParameterGroupResult :: Generic CreateClusterParameterGroupResult _
instance showCreateClusterParameterGroupResult :: Show CreateClusterParameterGroupResult where
  show = genericShow
instance decodeCreateClusterParameterGroupResult :: Decode CreateClusterParameterGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterParameterGroupResult :: Encode CreateClusterParameterGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateClusterResult = CreateClusterResult 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeCreateClusterResult :: Newtype CreateClusterResult _
derive instance repGenericCreateClusterResult :: Generic CreateClusterResult _
instance showCreateClusterResult :: Show CreateClusterResult where
  show = genericShow
instance decodeCreateClusterResult :: Decode CreateClusterResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterResult :: Encode CreateClusterResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateClusterSecurityGroupMessage = CreateClusterSecurityGroupMessage 
  { "ClusterSecurityGroupName" :: (String)
  , "Description" :: (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeCreateClusterSecurityGroupMessage :: Newtype CreateClusterSecurityGroupMessage _
derive instance repGenericCreateClusterSecurityGroupMessage :: Generic CreateClusterSecurityGroupMessage _
instance showCreateClusterSecurityGroupMessage :: Show CreateClusterSecurityGroupMessage where
  show = genericShow
instance decodeCreateClusterSecurityGroupMessage :: Decode CreateClusterSecurityGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterSecurityGroupMessage :: Encode CreateClusterSecurityGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateClusterSecurityGroupResult = CreateClusterSecurityGroupResult 
  { "ClusterSecurityGroup" :: NullOrUndefined.NullOrUndefined (ClusterSecurityGroup)
  }
derive instance newtypeCreateClusterSecurityGroupResult :: Newtype CreateClusterSecurityGroupResult _
derive instance repGenericCreateClusterSecurityGroupResult :: Generic CreateClusterSecurityGroupResult _
instance showCreateClusterSecurityGroupResult :: Show CreateClusterSecurityGroupResult where
  show = genericShow
instance decodeCreateClusterSecurityGroupResult :: Decode CreateClusterSecurityGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterSecurityGroupResult :: Encode CreateClusterSecurityGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateClusterSnapshotMessage = CreateClusterSnapshotMessage 
  { "SnapshotIdentifier" :: (String)
  , "ClusterIdentifier" :: (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeCreateClusterSnapshotMessage :: Newtype CreateClusterSnapshotMessage _
derive instance repGenericCreateClusterSnapshotMessage :: Generic CreateClusterSnapshotMessage _
instance showCreateClusterSnapshotMessage :: Show CreateClusterSnapshotMessage where
  show = genericShow
instance decodeCreateClusterSnapshotMessage :: Decode CreateClusterSnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterSnapshotMessage :: Encode CreateClusterSnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateClusterSnapshotResult = CreateClusterSnapshotResult 
  { "Snapshot" :: NullOrUndefined.NullOrUndefined (Snapshot)
  }
derive instance newtypeCreateClusterSnapshotResult :: Newtype CreateClusterSnapshotResult _
derive instance repGenericCreateClusterSnapshotResult :: Generic CreateClusterSnapshotResult _
instance showCreateClusterSnapshotResult :: Show CreateClusterSnapshotResult where
  show = genericShow
instance decodeCreateClusterSnapshotResult :: Decode CreateClusterSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterSnapshotResult :: Encode CreateClusterSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateClusterSubnetGroupMessage = CreateClusterSubnetGroupMessage 
  { "ClusterSubnetGroupName" :: (String)
  , "Description" :: (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeCreateClusterSubnetGroupMessage :: Newtype CreateClusterSubnetGroupMessage _
derive instance repGenericCreateClusterSubnetGroupMessage :: Generic CreateClusterSubnetGroupMessage _
instance showCreateClusterSubnetGroupMessage :: Show CreateClusterSubnetGroupMessage where
  show = genericShow
instance decodeCreateClusterSubnetGroupMessage :: Decode CreateClusterSubnetGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterSubnetGroupMessage :: Encode CreateClusterSubnetGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateClusterSubnetGroupResult = CreateClusterSubnetGroupResult 
  { "ClusterSubnetGroup" :: NullOrUndefined.NullOrUndefined (ClusterSubnetGroup)
  }
derive instance newtypeCreateClusterSubnetGroupResult :: Newtype CreateClusterSubnetGroupResult _
derive instance repGenericCreateClusterSubnetGroupResult :: Generic CreateClusterSubnetGroupResult _
instance showCreateClusterSubnetGroupResult :: Show CreateClusterSubnetGroupResult where
  show = genericShow
instance decodeCreateClusterSubnetGroupResult :: Decode CreateClusterSubnetGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterSubnetGroupResult :: Encode CreateClusterSubnetGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateEventSubscriptionMessage = CreateEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SnsTopicArn" :: (String)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceIds" :: NullOrUndefined.NullOrUndefined (SourceIdsList)
  , "EventCategories" :: NullOrUndefined.NullOrUndefined (EventCategoriesList)
  , "Severity" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeCreateEventSubscriptionMessage :: Newtype CreateEventSubscriptionMessage _
derive instance repGenericCreateEventSubscriptionMessage :: Generic CreateEventSubscriptionMessage _
instance showCreateEventSubscriptionMessage :: Show CreateEventSubscriptionMessage where
  show = genericShow
instance decodeCreateEventSubscriptionMessage :: Decode CreateEventSubscriptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateEventSubscriptionMessage :: Encode CreateEventSubscriptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateEventSubscriptionResult = CreateEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined.NullOrUndefined (EventSubscription)
  }
derive instance newtypeCreateEventSubscriptionResult :: Newtype CreateEventSubscriptionResult _
derive instance repGenericCreateEventSubscriptionResult :: Generic CreateEventSubscriptionResult _
instance showCreateEventSubscriptionResult :: Show CreateEventSubscriptionResult where
  show = genericShow
instance decodeCreateEventSubscriptionResult :: Decode CreateEventSubscriptionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateEventSubscriptionResult :: Encode CreateEventSubscriptionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateHsmClientCertificateMessage = CreateHsmClientCertificateMessage 
  { "HsmClientCertificateIdentifier" :: (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeCreateHsmClientCertificateMessage :: Newtype CreateHsmClientCertificateMessage _
derive instance repGenericCreateHsmClientCertificateMessage :: Generic CreateHsmClientCertificateMessage _
instance showCreateHsmClientCertificateMessage :: Show CreateHsmClientCertificateMessage where
  show = genericShow
instance decodeCreateHsmClientCertificateMessage :: Decode CreateHsmClientCertificateMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHsmClientCertificateMessage :: Encode CreateHsmClientCertificateMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateHsmClientCertificateResult = CreateHsmClientCertificateResult 
  { "HsmClientCertificate" :: NullOrUndefined.NullOrUndefined (HsmClientCertificate)
  }
derive instance newtypeCreateHsmClientCertificateResult :: Newtype CreateHsmClientCertificateResult _
derive instance repGenericCreateHsmClientCertificateResult :: Generic CreateHsmClientCertificateResult _
instance showCreateHsmClientCertificateResult :: Show CreateHsmClientCertificateResult where
  show = genericShow
instance decodeCreateHsmClientCertificateResult :: Decode CreateHsmClientCertificateResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHsmClientCertificateResult :: Encode CreateHsmClientCertificateResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateHsmConfigurationMessage = CreateHsmConfigurationMessage 
  { "HsmConfigurationIdentifier" :: (String)
  , "Description" :: (String)
  , "HsmIpAddress" :: (String)
  , "HsmPartitionName" :: (String)
  , "HsmPartitionPassword" :: (String)
  , "HsmServerPublicCertificate" :: (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeCreateHsmConfigurationMessage :: Newtype CreateHsmConfigurationMessage _
derive instance repGenericCreateHsmConfigurationMessage :: Generic CreateHsmConfigurationMessage _
instance showCreateHsmConfigurationMessage :: Show CreateHsmConfigurationMessage where
  show = genericShow
instance decodeCreateHsmConfigurationMessage :: Decode CreateHsmConfigurationMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHsmConfigurationMessage :: Encode CreateHsmConfigurationMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateHsmConfigurationResult = CreateHsmConfigurationResult 
  { "HsmConfiguration" :: NullOrUndefined.NullOrUndefined (HsmConfiguration)
  }
derive instance newtypeCreateHsmConfigurationResult :: Newtype CreateHsmConfigurationResult _
derive instance repGenericCreateHsmConfigurationResult :: Generic CreateHsmConfigurationResult _
instance showCreateHsmConfigurationResult :: Show CreateHsmConfigurationResult where
  show = genericShow
instance decodeCreateHsmConfigurationResult :: Decode CreateHsmConfigurationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHsmConfigurationResult :: Encode CreateHsmConfigurationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of the <code>CreateSnapshotCopyGrant</code> action.</p>
newtype CreateSnapshotCopyGrantMessage = CreateSnapshotCopyGrantMessage 
  { "SnapshotCopyGrantName" :: (String)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeCreateSnapshotCopyGrantMessage :: Newtype CreateSnapshotCopyGrantMessage _
derive instance repGenericCreateSnapshotCopyGrantMessage :: Generic CreateSnapshotCopyGrantMessage _
instance showCreateSnapshotCopyGrantMessage :: Show CreateSnapshotCopyGrantMessage where
  show = genericShow
instance decodeCreateSnapshotCopyGrantMessage :: Decode CreateSnapshotCopyGrantMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSnapshotCopyGrantMessage :: Encode CreateSnapshotCopyGrantMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSnapshotCopyGrantResult = CreateSnapshotCopyGrantResult 
  { "SnapshotCopyGrant" :: NullOrUndefined.NullOrUndefined (SnapshotCopyGrant)
  }
derive instance newtypeCreateSnapshotCopyGrantResult :: Newtype CreateSnapshotCopyGrantResult _
derive instance repGenericCreateSnapshotCopyGrantResult :: Generic CreateSnapshotCopyGrantResult _
instance showCreateSnapshotCopyGrantResult :: Show CreateSnapshotCopyGrantResult where
  show = genericShow
instance decodeCreateSnapshotCopyGrantResult :: Decode CreateSnapshotCopyGrantResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSnapshotCopyGrantResult :: Encode CreateSnapshotCopyGrantResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output from the <code>CreateTags</code> action. </p>
newtype CreateTagsMessage = CreateTagsMessage 
  { "ResourceName" :: (String)
  , "Tags" :: (TagList)
  }
derive instance newtypeCreateTagsMessage :: Newtype CreateTagsMessage _
derive instance repGenericCreateTagsMessage :: Generic CreateTagsMessage _
instance showCreateTagsMessage :: Show CreateTagsMessage where
  show = genericShow
instance decodeCreateTagsMessage :: Decode CreateTagsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTagsMessage :: Encode CreateTagsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DbGroupList = DbGroupList (Array String)
derive instance newtypeDbGroupList :: Newtype DbGroupList _
derive instance repGenericDbGroupList :: Generic DbGroupList _
instance showDbGroupList :: Show DbGroupList where
  show = genericShow
instance decodeDbGroupList :: Decode DbGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDbGroupList :: Encode DbGroupList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the default cluster parameters for a parameter group family.</p>
newtype DefaultClusterParameters = DefaultClusterParameters 
  { "ParameterGroupFamily" :: NullOrUndefined.NullOrUndefined (String)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersList)
  }
derive instance newtypeDefaultClusterParameters :: Newtype DefaultClusterParameters _
derive instance repGenericDefaultClusterParameters :: Generic DefaultClusterParameters _
instance showDefaultClusterParameters :: Show DefaultClusterParameters where
  show = genericShow
instance decodeDefaultClusterParameters :: Decode DefaultClusterParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultClusterParameters :: Encode DefaultClusterParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteClusterMessage = DeleteClusterMessage 
  { "ClusterIdentifier" :: (String)
  , "SkipFinalClusterSnapshot" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "FinalClusterSnapshotIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDeleteClusterMessage :: Newtype DeleteClusterMessage _
derive instance repGenericDeleteClusterMessage :: Generic DeleteClusterMessage _
instance showDeleteClusterMessage :: Show DeleteClusterMessage where
  show = genericShow
instance decodeDeleteClusterMessage :: Decode DeleteClusterMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClusterMessage :: Encode DeleteClusterMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteClusterParameterGroupMessage = DeleteClusterParameterGroupMessage 
  { "ParameterGroupName" :: (String)
  }
derive instance newtypeDeleteClusterParameterGroupMessage :: Newtype DeleteClusterParameterGroupMessage _
derive instance repGenericDeleteClusterParameterGroupMessage :: Generic DeleteClusterParameterGroupMessage _
instance showDeleteClusterParameterGroupMessage :: Show DeleteClusterParameterGroupMessage where
  show = genericShow
instance decodeDeleteClusterParameterGroupMessage :: Decode DeleteClusterParameterGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClusterParameterGroupMessage :: Encode DeleteClusterParameterGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteClusterResult = DeleteClusterResult 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeDeleteClusterResult :: Newtype DeleteClusterResult _
derive instance repGenericDeleteClusterResult :: Generic DeleteClusterResult _
instance showDeleteClusterResult :: Show DeleteClusterResult where
  show = genericShow
instance decodeDeleteClusterResult :: Decode DeleteClusterResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClusterResult :: Encode DeleteClusterResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteClusterSecurityGroupMessage = DeleteClusterSecurityGroupMessage 
  { "ClusterSecurityGroupName" :: (String)
  }
derive instance newtypeDeleteClusterSecurityGroupMessage :: Newtype DeleteClusterSecurityGroupMessage _
derive instance repGenericDeleteClusterSecurityGroupMessage :: Generic DeleteClusterSecurityGroupMessage _
instance showDeleteClusterSecurityGroupMessage :: Show DeleteClusterSecurityGroupMessage where
  show = genericShow
instance decodeDeleteClusterSecurityGroupMessage :: Decode DeleteClusterSecurityGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClusterSecurityGroupMessage :: Encode DeleteClusterSecurityGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteClusterSnapshotMessage = DeleteClusterSnapshotMessage 
  { "SnapshotIdentifier" :: (String)
  , "SnapshotClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDeleteClusterSnapshotMessage :: Newtype DeleteClusterSnapshotMessage _
derive instance repGenericDeleteClusterSnapshotMessage :: Generic DeleteClusterSnapshotMessage _
instance showDeleteClusterSnapshotMessage :: Show DeleteClusterSnapshotMessage where
  show = genericShow
instance decodeDeleteClusterSnapshotMessage :: Decode DeleteClusterSnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClusterSnapshotMessage :: Encode DeleteClusterSnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteClusterSnapshotResult = DeleteClusterSnapshotResult 
  { "Snapshot" :: NullOrUndefined.NullOrUndefined (Snapshot)
  }
derive instance newtypeDeleteClusterSnapshotResult :: Newtype DeleteClusterSnapshotResult _
derive instance repGenericDeleteClusterSnapshotResult :: Generic DeleteClusterSnapshotResult _
instance showDeleteClusterSnapshotResult :: Show DeleteClusterSnapshotResult where
  show = genericShow
instance decodeDeleteClusterSnapshotResult :: Decode DeleteClusterSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClusterSnapshotResult :: Encode DeleteClusterSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteClusterSubnetGroupMessage = DeleteClusterSubnetGroupMessage 
  { "ClusterSubnetGroupName" :: (String)
  }
derive instance newtypeDeleteClusterSubnetGroupMessage :: Newtype DeleteClusterSubnetGroupMessage _
derive instance repGenericDeleteClusterSubnetGroupMessage :: Generic DeleteClusterSubnetGroupMessage _
instance showDeleteClusterSubnetGroupMessage :: Show DeleteClusterSubnetGroupMessage where
  show = genericShow
instance decodeDeleteClusterSubnetGroupMessage :: Decode DeleteClusterSubnetGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClusterSubnetGroupMessage :: Encode DeleteClusterSubnetGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteEventSubscriptionMessage = DeleteEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  }
derive instance newtypeDeleteEventSubscriptionMessage :: Newtype DeleteEventSubscriptionMessage _
derive instance repGenericDeleteEventSubscriptionMessage :: Generic DeleteEventSubscriptionMessage _
instance showDeleteEventSubscriptionMessage :: Show DeleteEventSubscriptionMessage where
  show = genericShow
instance decodeDeleteEventSubscriptionMessage :: Decode DeleteEventSubscriptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEventSubscriptionMessage :: Encode DeleteEventSubscriptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteHsmClientCertificateMessage = DeleteHsmClientCertificateMessage 
  { "HsmClientCertificateIdentifier" :: (String)
  }
derive instance newtypeDeleteHsmClientCertificateMessage :: Newtype DeleteHsmClientCertificateMessage _
derive instance repGenericDeleteHsmClientCertificateMessage :: Generic DeleteHsmClientCertificateMessage _
instance showDeleteHsmClientCertificateMessage :: Show DeleteHsmClientCertificateMessage where
  show = genericShow
instance decodeDeleteHsmClientCertificateMessage :: Decode DeleteHsmClientCertificateMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteHsmClientCertificateMessage :: Encode DeleteHsmClientCertificateMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteHsmConfigurationMessage = DeleteHsmConfigurationMessage 
  { "HsmConfigurationIdentifier" :: (String)
  }
derive instance newtypeDeleteHsmConfigurationMessage :: Newtype DeleteHsmConfigurationMessage _
derive instance repGenericDeleteHsmConfigurationMessage :: Generic DeleteHsmConfigurationMessage _
instance showDeleteHsmConfigurationMessage :: Show DeleteHsmConfigurationMessage where
  show = genericShow
instance decodeDeleteHsmConfigurationMessage :: Decode DeleteHsmConfigurationMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteHsmConfigurationMessage :: Encode DeleteHsmConfigurationMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of the <code>DeleteSnapshotCopyGrant</code> action.</p>
newtype DeleteSnapshotCopyGrantMessage = DeleteSnapshotCopyGrantMessage 
  { "SnapshotCopyGrantName" :: (String)
  }
derive instance newtypeDeleteSnapshotCopyGrantMessage :: Newtype DeleteSnapshotCopyGrantMessage _
derive instance repGenericDeleteSnapshotCopyGrantMessage :: Generic DeleteSnapshotCopyGrantMessage _
instance showDeleteSnapshotCopyGrantMessage :: Show DeleteSnapshotCopyGrantMessage where
  show = genericShow
instance decodeDeleteSnapshotCopyGrantMessage :: Decode DeleteSnapshotCopyGrantMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSnapshotCopyGrantMessage :: Encode DeleteSnapshotCopyGrantMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output from the <code>DeleteTags</code> action. </p>
newtype DeleteTagsMessage = DeleteTagsMessage 
  { "ResourceName" :: (String)
  , "TagKeys" :: (TagKeyList)
  }
derive instance newtypeDeleteTagsMessage :: Newtype DeleteTagsMessage _
derive instance repGenericDeleteTagsMessage :: Generic DeleteTagsMessage _
instance showDeleteTagsMessage :: Show DeleteTagsMessage where
  show = genericShow
instance decodeDeleteTagsMessage :: Decode DeleteTagsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTagsMessage :: Encode DeleteTagsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request cannot be completed because a dependent service is throttling requests made by Amazon Redshift on your behalf. Wait and retry the request.</p>
newtype DependentServiceRequestThrottlingFault = DependentServiceRequestThrottlingFault Types.NoArguments
derive instance newtypeDependentServiceRequestThrottlingFault :: Newtype DependentServiceRequestThrottlingFault _
derive instance repGenericDependentServiceRequestThrottlingFault :: Generic DependentServiceRequestThrottlingFault _
instance showDependentServiceRequestThrottlingFault :: Show DependentServiceRequestThrottlingFault where
  show = genericShow
instance decodeDependentServiceRequestThrottlingFault :: Decode DependentServiceRequestThrottlingFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDependentServiceRequestThrottlingFault :: Encode DependentServiceRequestThrottlingFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Your request cannot be completed because a dependent internal service is temporarily unavailable. Wait 30 to 60 seconds and try again.</p>
newtype DependentServiceUnavailableFault = DependentServiceUnavailableFault Types.NoArguments
derive instance newtypeDependentServiceUnavailableFault :: Newtype DependentServiceUnavailableFault _
derive instance repGenericDependentServiceUnavailableFault :: Generic DependentServiceUnavailableFault _
instance showDependentServiceUnavailableFault :: Show DependentServiceUnavailableFault where
  show = genericShow
instance decodeDependentServiceUnavailableFault :: Decode DependentServiceUnavailableFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDependentServiceUnavailableFault :: Encode DependentServiceUnavailableFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeClusterParameterGroupsMessage = DescribeClusterParameterGroupsMessage 
  { "ParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined.NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeClusterParameterGroupsMessage :: Newtype DescribeClusterParameterGroupsMessage _
derive instance repGenericDescribeClusterParameterGroupsMessage :: Generic DescribeClusterParameterGroupsMessage _
instance showDescribeClusterParameterGroupsMessage :: Show DescribeClusterParameterGroupsMessage where
  show = genericShow
instance decodeDescribeClusterParameterGroupsMessage :: Decode DescribeClusterParameterGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClusterParameterGroupsMessage :: Encode DescribeClusterParameterGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeClusterParametersMessage = DescribeClusterParametersMessage 
  { "ParameterGroupName" :: (String)
  , "Source" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeClusterParametersMessage :: Newtype DescribeClusterParametersMessage _
derive instance repGenericDescribeClusterParametersMessage :: Generic DescribeClusterParametersMessage _
instance showDescribeClusterParametersMessage :: Show DescribeClusterParametersMessage where
  show = genericShow
instance decodeDescribeClusterParametersMessage :: Decode DescribeClusterParametersMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClusterParametersMessage :: Encode DescribeClusterParametersMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeClusterSecurityGroupsMessage = DescribeClusterSecurityGroupsMessage 
  { "ClusterSecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined.NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeClusterSecurityGroupsMessage :: Newtype DescribeClusterSecurityGroupsMessage _
derive instance repGenericDescribeClusterSecurityGroupsMessage :: Generic DescribeClusterSecurityGroupsMessage _
instance showDescribeClusterSecurityGroupsMessage :: Show DescribeClusterSecurityGroupsMessage where
  show = genericShow
instance decodeDescribeClusterSecurityGroupsMessage :: Decode DescribeClusterSecurityGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClusterSecurityGroupsMessage :: Encode DescribeClusterSecurityGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeClusterSnapshotsMessage = DescribeClusterSnapshotsMessage 
  { "ClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotType" :: NullOrUndefined.NullOrUndefined (String)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "OwnerAccount" :: NullOrUndefined.NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined.NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeClusterSnapshotsMessage :: Newtype DescribeClusterSnapshotsMessage _
derive instance repGenericDescribeClusterSnapshotsMessage :: Generic DescribeClusterSnapshotsMessage _
instance showDescribeClusterSnapshotsMessage :: Show DescribeClusterSnapshotsMessage where
  show = genericShow
instance decodeDescribeClusterSnapshotsMessage :: Decode DescribeClusterSnapshotsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClusterSnapshotsMessage :: Encode DescribeClusterSnapshotsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeClusterSubnetGroupsMessage = DescribeClusterSubnetGroupsMessage 
  { "ClusterSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined.NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeClusterSubnetGroupsMessage :: Newtype DescribeClusterSubnetGroupsMessage _
derive instance repGenericDescribeClusterSubnetGroupsMessage :: Generic DescribeClusterSubnetGroupsMessage _
instance showDescribeClusterSubnetGroupsMessage :: Show DescribeClusterSubnetGroupsMessage where
  show = genericShow
instance decodeDescribeClusterSubnetGroupsMessage :: Decode DescribeClusterSubnetGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClusterSubnetGroupsMessage :: Encode DescribeClusterSubnetGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeClusterVersionsMessage = DescribeClusterVersionsMessage 
  { "ClusterVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterParameterGroupFamily" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeClusterVersionsMessage :: Newtype DescribeClusterVersionsMessage _
derive instance repGenericDescribeClusterVersionsMessage :: Generic DescribeClusterVersionsMessage _
instance showDescribeClusterVersionsMessage :: Show DescribeClusterVersionsMessage where
  show = genericShow
instance decodeDescribeClusterVersionsMessage :: Decode DescribeClusterVersionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClusterVersionsMessage :: Encode DescribeClusterVersionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeClustersMessage = DescribeClustersMessage 
  { "ClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined.NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeClustersMessage :: Newtype DescribeClustersMessage _
derive instance repGenericDescribeClustersMessage :: Generic DescribeClustersMessage _
instance showDescribeClustersMessage :: Show DescribeClustersMessage where
  show = genericShow
instance decodeDescribeClustersMessage :: Decode DescribeClustersMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClustersMessage :: Encode DescribeClustersMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeDefaultClusterParametersMessage = DescribeDefaultClusterParametersMessage 
  { "ParameterGroupFamily" :: (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeDefaultClusterParametersMessage :: Newtype DescribeDefaultClusterParametersMessage _
derive instance repGenericDescribeDefaultClusterParametersMessage :: Generic DescribeDefaultClusterParametersMessage _
instance showDescribeDefaultClusterParametersMessage :: Show DescribeDefaultClusterParametersMessage where
  show = genericShow
instance decodeDescribeDefaultClusterParametersMessage :: Decode DescribeDefaultClusterParametersMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDefaultClusterParametersMessage :: Encode DescribeDefaultClusterParametersMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDefaultClusterParametersResult = DescribeDefaultClusterParametersResult 
  { "DefaultClusterParameters" :: NullOrUndefined.NullOrUndefined (DefaultClusterParameters)
  }
derive instance newtypeDescribeDefaultClusterParametersResult :: Newtype DescribeDefaultClusterParametersResult _
derive instance repGenericDescribeDefaultClusterParametersResult :: Generic DescribeDefaultClusterParametersResult _
instance showDescribeDefaultClusterParametersResult :: Show DescribeDefaultClusterParametersResult where
  show = genericShow
instance decodeDescribeDefaultClusterParametersResult :: Decode DescribeDefaultClusterParametersResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDefaultClusterParametersResult :: Encode DescribeDefaultClusterParametersResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeEventCategoriesMessage = DescribeEventCategoriesMessage 
  { "SourceType" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeEventCategoriesMessage :: Newtype DescribeEventCategoriesMessage _
derive instance repGenericDescribeEventCategoriesMessage :: Generic DescribeEventCategoriesMessage _
instance showDescribeEventCategoriesMessage :: Show DescribeEventCategoriesMessage where
  show = genericShow
instance decodeDescribeEventCategoriesMessage :: Decode DescribeEventCategoriesMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventCategoriesMessage :: Encode DescribeEventCategoriesMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeEventSubscriptionsMessage = DescribeEventSubscriptionsMessage 
  { "SubscriptionName" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined.NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeEventSubscriptionsMessage :: Newtype DescribeEventSubscriptionsMessage _
derive instance repGenericDescribeEventSubscriptionsMessage :: Generic DescribeEventSubscriptionsMessage _
instance showDescribeEventSubscriptionsMessage :: Show DescribeEventSubscriptionsMessage where
  show = genericShow
instance decodeDescribeEventSubscriptionsMessage :: Decode DescribeEventSubscriptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventSubscriptionsMessage :: Encode DescribeEventSubscriptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
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


-- | <p/>
newtype DescribeHsmClientCertificatesMessage = DescribeHsmClientCertificatesMessage 
  { "HsmClientCertificateIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined.NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeHsmClientCertificatesMessage :: Newtype DescribeHsmClientCertificatesMessage _
derive instance repGenericDescribeHsmClientCertificatesMessage :: Generic DescribeHsmClientCertificatesMessage _
instance showDescribeHsmClientCertificatesMessage :: Show DescribeHsmClientCertificatesMessage where
  show = genericShow
instance decodeDescribeHsmClientCertificatesMessage :: Decode DescribeHsmClientCertificatesMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeHsmClientCertificatesMessage :: Encode DescribeHsmClientCertificatesMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeHsmConfigurationsMessage = DescribeHsmConfigurationsMessage 
  { "HsmConfigurationIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined.NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeHsmConfigurationsMessage :: Newtype DescribeHsmConfigurationsMessage _
derive instance repGenericDescribeHsmConfigurationsMessage :: Generic DescribeHsmConfigurationsMessage _
instance showDescribeHsmConfigurationsMessage :: Show DescribeHsmConfigurationsMessage where
  show = genericShow
instance decodeDescribeHsmConfigurationsMessage :: Decode DescribeHsmConfigurationsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeHsmConfigurationsMessage :: Encode DescribeHsmConfigurationsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeLoggingStatusMessage = DescribeLoggingStatusMessage 
  { "ClusterIdentifier" :: (String)
  }
derive instance newtypeDescribeLoggingStatusMessage :: Newtype DescribeLoggingStatusMessage _
derive instance repGenericDescribeLoggingStatusMessage :: Generic DescribeLoggingStatusMessage _
instance showDescribeLoggingStatusMessage :: Show DescribeLoggingStatusMessage where
  show = genericShow
instance decodeDescribeLoggingStatusMessage :: Decode DescribeLoggingStatusMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLoggingStatusMessage :: Encode DescribeLoggingStatusMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeOrderableClusterOptionsMessage = DescribeOrderableClusterOptionsMessage 
  { "ClusterVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeOrderableClusterOptionsMessage :: Newtype DescribeOrderableClusterOptionsMessage _
derive instance repGenericDescribeOrderableClusterOptionsMessage :: Generic DescribeOrderableClusterOptionsMessage _
instance showDescribeOrderableClusterOptionsMessage :: Show DescribeOrderableClusterOptionsMessage where
  show = genericShow
instance decodeDescribeOrderableClusterOptionsMessage :: Decode DescribeOrderableClusterOptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeOrderableClusterOptionsMessage :: Encode DescribeOrderableClusterOptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeReservedNodeOfferingsMessage = DescribeReservedNodeOfferingsMessage 
  { "ReservedNodeOfferingId" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeReservedNodeOfferingsMessage :: Newtype DescribeReservedNodeOfferingsMessage _
derive instance repGenericDescribeReservedNodeOfferingsMessage :: Generic DescribeReservedNodeOfferingsMessage _
instance showDescribeReservedNodeOfferingsMessage :: Show DescribeReservedNodeOfferingsMessage where
  show = genericShow
instance decodeDescribeReservedNodeOfferingsMessage :: Decode DescribeReservedNodeOfferingsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReservedNodeOfferingsMessage :: Encode DescribeReservedNodeOfferingsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeReservedNodesMessage = DescribeReservedNodesMessage 
  { "ReservedNodeId" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeReservedNodesMessage :: Newtype DescribeReservedNodesMessage _
derive instance repGenericDescribeReservedNodesMessage :: Generic DescribeReservedNodesMessage _
instance showDescribeReservedNodesMessage :: Show DescribeReservedNodesMessage where
  show = genericShow
instance decodeDescribeReservedNodesMessage :: Decode DescribeReservedNodesMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReservedNodesMessage :: Encode DescribeReservedNodesMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeResizeMessage = DescribeResizeMessage 
  { "ClusterIdentifier" :: (String)
  }
derive instance newtypeDescribeResizeMessage :: Newtype DescribeResizeMessage _
derive instance repGenericDescribeResizeMessage :: Generic DescribeResizeMessage _
instance showDescribeResizeMessage :: Show DescribeResizeMessage where
  show = genericShow
instance decodeDescribeResizeMessage :: Decode DescribeResizeMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeResizeMessage :: Encode DescribeResizeMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of the <code>DescribeSnapshotCopyGrants</code> action.</p>
newtype DescribeSnapshotCopyGrantsMessage = DescribeSnapshotCopyGrantsMessage 
  { "SnapshotCopyGrantName" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined.NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeSnapshotCopyGrantsMessage :: Newtype DescribeSnapshotCopyGrantsMessage _
derive instance repGenericDescribeSnapshotCopyGrantsMessage :: Generic DescribeSnapshotCopyGrantsMessage _
instance showDescribeSnapshotCopyGrantsMessage :: Show DescribeSnapshotCopyGrantsMessage where
  show = genericShow
instance decodeDescribeSnapshotCopyGrantsMessage :: Decode DescribeSnapshotCopyGrantsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSnapshotCopyGrantsMessage :: Encode DescribeSnapshotCopyGrantsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeTableRestoreStatusMessage = DescribeTableRestoreStatusMessage 
  { "ClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "TableRestoreRequestId" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeTableRestoreStatusMessage :: Newtype DescribeTableRestoreStatusMessage _
derive instance repGenericDescribeTableRestoreStatusMessage :: Generic DescribeTableRestoreStatusMessage _
instance showDescribeTableRestoreStatusMessage :: Show DescribeTableRestoreStatusMessage where
  show = genericShow
instance decodeDescribeTableRestoreStatusMessage :: Decode DescribeTableRestoreStatusMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTableRestoreStatusMessage :: Encode DescribeTableRestoreStatusMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeTagsMessage = DescribeTagsMessage 
  { "ResourceName" :: NullOrUndefined.NullOrUndefined (String)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "TagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  , "TagValues" :: NullOrUndefined.NullOrUndefined (TagValueList)
  }
derive instance newtypeDescribeTagsMessage :: Newtype DescribeTagsMessage _
derive instance repGenericDescribeTagsMessage :: Generic DescribeTagsMessage _
instance showDescribeTagsMessage :: Show DescribeTagsMessage where
  show = genericShow
instance decodeDescribeTagsMessage :: Decode DescribeTagsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsMessage :: Encode DescribeTagsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DisableLoggingMessage = DisableLoggingMessage 
  { "ClusterIdentifier" :: (String)
  }
derive instance newtypeDisableLoggingMessage :: Newtype DisableLoggingMessage _
derive instance repGenericDisableLoggingMessage :: Generic DisableLoggingMessage _
instance showDisableLoggingMessage :: Show DisableLoggingMessage where
  show = genericShow
instance decodeDisableLoggingMessage :: Decode DisableLoggingMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableLoggingMessage :: Encode DisableLoggingMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DisableSnapshotCopyMessage = DisableSnapshotCopyMessage 
  { "ClusterIdentifier" :: (String)
  }
derive instance newtypeDisableSnapshotCopyMessage :: Newtype DisableSnapshotCopyMessage _
derive instance repGenericDisableSnapshotCopyMessage :: Generic DisableSnapshotCopyMessage _
instance showDisableSnapshotCopyMessage :: Show DisableSnapshotCopyMessage where
  show = genericShow
instance decodeDisableSnapshotCopyMessage :: Decode DisableSnapshotCopyMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableSnapshotCopyMessage :: Encode DisableSnapshotCopyMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisableSnapshotCopyResult = DisableSnapshotCopyResult 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeDisableSnapshotCopyResult :: Newtype DisableSnapshotCopyResult _
derive instance repGenericDisableSnapshotCopyResult :: Generic DisableSnapshotCopyResult _
instance showDisableSnapshotCopyResult :: Show DisableSnapshotCopyResult where
  show = genericShow
instance decodeDisableSnapshotCopyResult :: Decode DisableSnapshotCopyResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableSnapshotCopyResult :: Encode DisableSnapshotCopyResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DoubleOptional = DoubleOptional Number
derive instance newtypeDoubleOptional :: Newtype DoubleOptional _
derive instance repGenericDoubleOptional :: Generic DoubleOptional _
instance showDoubleOptional :: Show DoubleOptional where
  show = genericShow
instance decodeDoubleOptional :: Decode DoubleOptional where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDoubleOptional :: Encode DoubleOptional where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an Amazon EC2 security group.</p>
newtype EC2SecurityGroup = EC2SecurityGroup 
  { "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined.NullOrUndefined (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
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


-- | <p>Describes the status of the elastic IP (EIP) address.</p>
newtype ElasticIpStatus = ElasticIpStatus 
  { "ElasticIp" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeElasticIpStatus :: Newtype ElasticIpStatus _
derive instance repGenericElasticIpStatus :: Generic ElasticIpStatus _
instance showElasticIpStatus :: Show ElasticIpStatus where
  show = genericShow
instance decodeElasticIpStatus :: Decode ElasticIpStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticIpStatus :: Encode ElasticIpStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype EnableLoggingMessage = EnableLoggingMessage 
  { "ClusterIdentifier" :: (String)
  , "BucketName" :: (String)
  , "S3KeyPrefix" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEnableLoggingMessage :: Newtype EnableLoggingMessage _
derive instance repGenericEnableLoggingMessage :: Generic EnableLoggingMessage _
instance showEnableLoggingMessage :: Show EnableLoggingMessage where
  show = genericShow
instance decodeEnableLoggingMessage :: Decode EnableLoggingMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableLoggingMessage :: Encode EnableLoggingMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype EnableSnapshotCopyMessage = EnableSnapshotCopyMessage 
  { "ClusterIdentifier" :: (String)
  , "DestinationRegion" :: (String)
  , "RetentionPeriod" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "SnapshotCopyGrantName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEnableSnapshotCopyMessage :: Newtype EnableSnapshotCopyMessage _
derive instance repGenericEnableSnapshotCopyMessage :: Generic EnableSnapshotCopyMessage _
instance showEnableSnapshotCopyMessage :: Show EnableSnapshotCopyMessage where
  show = genericShow
instance decodeEnableSnapshotCopyMessage :: Decode EnableSnapshotCopyMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableSnapshotCopyMessage :: Encode EnableSnapshotCopyMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnableSnapshotCopyResult = EnableSnapshotCopyResult 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeEnableSnapshotCopyResult :: Newtype EnableSnapshotCopyResult _
derive instance repGenericEnableSnapshotCopyResult :: Generic EnableSnapshotCopyResult _
instance showEnableSnapshotCopyResult :: Show EnableSnapshotCopyResult where
  show = genericShow
instance decodeEnableSnapshotCopyResult :: Decode EnableSnapshotCopyResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableSnapshotCopyResult :: Encode EnableSnapshotCopyResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a connection endpoint.</p>
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


-- | <p>Describes an event.</p>
newtype Event = Event 
  { "SourceIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (SourceType)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined.NullOrUndefined (EventCategoriesList)
  , "Severity" :: NullOrUndefined.NullOrUndefined (String)
  , "Date" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "EventId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEvent :: Newtype Event _
derive instance repGenericEvent :: Generic Event _
instance showEvent :: Show Event where
  show = genericShow
instance decodeEvent :: Decode Event where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvent :: Encode Event where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventCategoriesList = EventCategoriesList (Array String)
derive instance newtypeEventCategoriesList :: Newtype EventCategoriesList _
derive instance repGenericEventCategoriesList :: Generic EventCategoriesList _
instance showEventCategoriesList :: Show EventCategoriesList where
  show = genericShow
instance decodeEventCategoriesList :: Decode EventCategoriesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventCategoriesList :: Encode EventCategoriesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes event categories.</p>
newtype EventCategoriesMap = EventCategoriesMap 
  { "SourceType" :: NullOrUndefined.NullOrUndefined (String)
  , "Events" :: NullOrUndefined.NullOrUndefined (EventInfoMapList)
  }
derive instance newtypeEventCategoriesMap :: Newtype EventCategoriesMap _
derive instance repGenericEventCategoriesMap :: Generic EventCategoriesMap _
instance showEventCategoriesMap :: Show EventCategoriesMap where
  show = genericShow
instance decodeEventCategoriesMap :: Decode EventCategoriesMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventCategoriesMap :: Encode EventCategoriesMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventCategoriesMapList = EventCategoriesMapList (Array EventCategoriesMap)
derive instance newtypeEventCategoriesMapList :: Newtype EventCategoriesMapList _
derive instance repGenericEventCategoriesMapList :: Generic EventCategoriesMapList _
instance showEventCategoriesMapList :: Show EventCategoriesMapList where
  show = genericShow
instance decodeEventCategoriesMapList :: Decode EventCategoriesMapList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventCategoriesMapList :: Encode EventCategoriesMapList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype EventCategoriesMessage = EventCategoriesMessage 
  { "EventCategoriesMapList" :: NullOrUndefined.NullOrUndefined (EventCategoriesMapList)
  }
derive instance newtypeEventCategoriesMessage :: Newtype EventCategoriesMessage _
derive instance repGenericEventCategoriesMessage :: Generic EventCategoriesMessage _
instance showEventCategoriesMessage :: Show EventCategoriesMessage where
  show = genericShow
instance decodeEventCategoriesMessage :: Decode EventCategoriesMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventCategoriesMessage :: Encode EventCategoriesMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes event information.</p>
newtype EventInfoMap = EventInfoMap 
  { "EventId" :: NullOrUndefined.NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined.NullOrUndefined (EventCategoriesList)
  , "EventDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "Severity" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEventInfoMap :: Newtype EventInfoMap _
derive instance repGenericEventInfoMap :: Generic EventInfoMap _
instance showEventInfoMap :: Show EventInfoMap where
  show = genericShow
instance decodeEventInfoMap :: Decode EventInfoMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventInfoMap :: Encode EventInfoMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventInfoMapList = EventInfoMapList (Array EventInfoMap)
derive instance newtypeEventInfoMapList :: Newtype EventInfoMapList _
derive instance repGenericEventInfoMapList :: Generic EventInfoMapList _
instance showEventInfoMapList :: Show EventInfoMapList where
  show = genericShow
instance decodeEventInfoMapList :: Decode EventInfoMapList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventInfoMapList :: Encode EventInfoMapList where
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


-- | <p>Describes event subscriptions.</p>
newtype EventSubscription = EventSubscription 
  { "CustomerAwsId" :: NullOrUndefined.NullOrUndefined (String)
  , "CustSubscriptionId" :: NullOrUndefined.NullOrUndefined (String)
  , "SnsTopicArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "SubscriptionCreationTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceIdsList" :: NullOrUndefined.NullOrUndefined (SourceIdsList)
  , "EventCategoriesList" :: NullOrUndefined.NullOrUndefined (EventCategoriesList)
  , "Severity" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeEventSubscription :: Newtype EventSubscription _
derive instance repGenericEventSubscription :: Generic EventSubscription _
instance showEventSubscription :: Show EventSubscription where
  show = genericShow
instance decodeEventSubscription :: Decode EventSubscription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventSubscription :: Encode EventSubscription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request would exceed the allowed number of event subscriptions for this account. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype EventSubscriptionQuotaExceededFault = EventSubscriptionQuotaExceededFault Types.NoArguments
derive instance newtypeEventSubscriptionQuotaExceededFault :: Newtype EventSubscriptionQuotaExceededFault _
derive instance repGenericEventSubscriptionQuotaExceededFault :: Generic EventSubscriptionQuotaExceededFault _
instance showEventSubscriptionQuotaExceededFault :: Show EventSubscriptionQuotaExceededFault where
  show = genericShow
instance decodeEventSubscriptionQuotaExceededFault :: Decode EventSubscriptionQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventSubscriptionQuotaExceededFault :: Encode EventSubscriptionQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventSubscriptionsList = EventSubscriptionsList (Array EventSubscription)
derive instance newtypeEventSubscriptionsList :: Newtype EventSubscriptionsList _
derive instance repGenericEventSubscriptionsList :: Generic EventSubscriptionsList _
instance showEventSubscriptionsList :: Show EventSubscriptionsList where
  show = genericShow
instance decodeEventSubscriptionsList :: Decode EventSubscriptionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventSubscriptionsList :: Encode EventSubscriptionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype EventSubscriptionsMessage = EventSubscriptionsMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "EventSubscriptionsList" :: NullOrUndefined.NullOrUndefined (EventSubscriptionsList)
  }
derive instance newtypeEventSubscriptionsMessage :: Newtype EventSubscriptionsMessage _
derive instance repGenericEventSubscriptionsMessage :: Generic EventSubscriptionsMessage _
instance showEventSubscriptionsMessage :: Show EventSubscriptionsMessage where
  show = genericShow
instance decodeEventSubscriptionsMessage :: Decode EventSubscriptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventSubscriptionsMessage :: Encode EventSubscriptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
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


-- | <p>The request parameters to get cluster credentials.</p>
newtype GetClusterCredentialsMessage = GetClusterCredentialsMessage 
  { "DbUser" :: (String)
  , "DbName" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterIdentifier" :: (String)
  , "DurationSeconds" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "AutoCreate" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "DbGroups" :: NullOrUndefined.NullOrUndefined (DbGroupList)
  }
derive instance newtypeGetClusterCredentialsMessage :: Newtype GetClusterCredentialsMessage _
derive instance repGenericGetClusterCredentialsMessage :: Generic GetClusterCredentialsMessage _
instance showGetClusterCredentialsMessage :: Show GetClusterCredentialsMessage where
  show = genericShow
instance decodeGetClusterCredentialsMessage :: Decode GetClusterCredentialsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetClusterCredentialsMessage :: Encode GetClusterCredentialsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about an HSM client certificate. The certificate is stored in a secure Hardware Storage Module (HSM), and used by the Amazon Redshift cluster to encrypt data files.</p>
newtype HsmClientCertificate = HsmClientCertificate 
  { "HsmClientCertificateIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "HsmClientCertificatePublicKey" :: NullOrUndefined.NullOrUndefined (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeHsmClientCertificate :: Newtype HsmClientCertificate _
derive instance repGenericHsmClientCertificate :: Generic HsmClientCertificate _
instance showHsmClientCertificate :: Show HsmClientCertificate where
  show = genericShow
instance decodeHsmClientCertificate :: Decode HsmClientCertificate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmClientCertificate :: Encode HsmClientCertificate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is already an existing Amazon Redshift HSM client certificate with the specified identifier.</p>
newtype HsmClientCertificateAlreadyExistsFault = HsmClientCertificateAlreadyExistsFault Types.NoArguments
derive instance newtypeHsmClientCertificateAlreadyExistsFault :: Newtype HsmClientCertificateAlreadyExistsFault _
derive instance repGenericHsmClientCertificateAlreadyExistsFault :: Generic HsmClientCertificateAlreadyExistsFault _
instance showHsmClientCertificateAlreadyExistsFault :: Show HsmClientCertificateAlreadyExistsFault where
  show = genericShow
instance decodeHsmClientCertificateAlreadyExistsFault :: Decode HsmClientCertificateAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmClientCertificateAlreadyExistsFault :: Encode HsmClientCertificateAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HsmClientCertificateList = HsmClientCertificateList (Array HsmClientCertificate)
derive instance newtypeHsmClientCertificateList :: Newtype HsmClientCertificateList _
derive instance repGenericHsmClientCertificateList :: Generic HsmClientCertificateList _
instance showHsmClientCertificateList :: Show HsmClientCertificateList where
  show = genericShow
instance decodeHsmClientCertificateList :: Decode HsmClientCertificateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmClientCertificateList :: Encode HsmClientCertificateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype HsmClientCertificateMessage = HsmClientCertificateMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "HsmClientCertificates" :: NullOrUndefined.NullOrUndefined (HsmClientCertificateList)
  }
derive instance newtypeHsmClientCertificateMessage :: Newtype HsmClientCertificateMessage _
derive instance repGenericHsmClientCertificateMessage :: Generic HsmClientCertificateMessage _
instance showHsmClientCertificateMessage :: Show HsmClientCertificateMessage where
  show = genericShow
instance decodeHsmClientCertificateMessage :: Decode HsmClientCertificateMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmClientCertificateMessage :: Encode HsmClientCertificateMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is no Amazon Redshift HSM client certificate with the specified identifier.</p>
newtype HsmClientCertificateNotFoundFault = HsmClientCertificateNotFoundFault Types.NoArguments
derive instance newtypeHsmClientCertificateNotFoundFault :: Newtype HsmClientCertificateNotFoundFault _
derive instance repGenericHsmClientCertificateNotFoundFault :: Generic HsmClientCertificateNotFoundFault _
instance showHsmClientCertificateNotFoundFault :: Show HsmClientCertificateNotFoundFault where
  show = genericShow
instance decodeHsmClientCertificateNotFoundFault :: Decode HsmClientCertificateNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmClientCertificateNotFoundFault :: Encode HsmClientCertificateNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The quota for HSM client certificates has been reached. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype HsmClientCertificateQuotaExceededFault = HsmClientCertificateQuotaExceededFault Types.NoArguments
derive instance newtypeHsmClientCertificateQuotaExceededFault :: Newtype HsmClientCertificateQuotaExceededFault _
derive instance repGenericHsmClientCertificateQuotaExceededFault :: Generic HsmClientCertificateQuotaExceededFault _
instance showHsmClientCertificateQuotaExceededFault :: Show HsmClientCertificateQuotaExceededFault where
  show = genericShow
instance decodeHsmClientCertificateQuotaExceededFault :: Decode HsmClientCertificateQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmClientCertificateQuotaExceededFault :: Encode HsmClientCertificateQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about an HSM configuration, which is an object that describes to Amazon Redshift clusters the information they require to connect to an HSM where they can store database encryption keys.</p>
newtype HsmConfiguration = HsmConfiguration 
  { "HsmConfigurationIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HsmIpAddress" :: NullOrUndefined.NullOrUndefined (String)
  , "HsmPartitionName" :: NullOrUndefined.NullOrUndefined (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeHsmConfiguration :: Newtype HsmConfiguration _
derive instance repGenericHsmConfiguration :: Generic HsmConfiguration _
instance showHsmConfiguration :: Show HsmConfiguration where
  show = genericShow
instance decodeHsmConfiguration :: Decode HsmConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmConfiguration :: Encode HsmConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is already an existing Amazon Redshift HSM configuration with the specified identifier.</p>
newtype HsmConfigurationAlreadyExistsFault = HsmConfigurationAlreadyExistsFault Types.NoArguments
derive instance newtypeHsmConfigurationAlreadyExistsFault :: Newtype HsmConfigurationAlreadyExistsFault _
derive instance repGenericHsmConfigurationAlreadyExistsFault :: Generic HsmConfigurationAlreadyExistsFault _
instance showHsmConfigurationAlreadyExistsFault :: Show HsmConfigurationAlreadyExistsFault where
  show = genericShow
instance decodeHsmConfigurationAlreadyExistsFault :: Decode HsmConfigurationAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmConfigurationAlreadyExistsFault :: Encode HsmConfigurationAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HsmConfigurationList = HsmConfigurationList (Array HsmConfiguration)
derive instance newtypeHsmConfigurationList :: Newtype HsmConfigurationList _
derive instance repGenericHsmConfigurationList :: Generic HsmConfigurationList _
instance showHsmConfigurationList :: Show HsmConfigurationList where
  show = genericShow
instance decodeHsmConfigurationList :: Decode HsmConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmConfigurationList :: Encode HsmConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype HsmConfigurationMessage = HsmConfigurationMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "HsmConfigurations" :: NullOrUndefined.NullOrUndefined (HsmConfigurationList)
  }
derive instance newtypeHsmConfigurationMessage :: Newtype HsmConfigurationMessage _
derive instance repGenericHsmConfigurationMessage :: Generic HsmConfigurationMessage _
instance showHsmConfigurationMessage :: Show HsmConfigurationMessage where
  show = genericShow
instance decodeHsmConfigurationMessage :: Decode HsmConfigurationMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmConfigurationMessage :: Encode HsmConfigurationMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is no Amazon Redshift HSM configuration with the specified identifier.</p>
newtype HsmConfigurationNotFoundFault = HsmConfigurationNotFoundFault Types.NoArguments
derive instance newtypeHsmConfigurationNotFoundFault :: Newtype HsmConfigurationNotFoundFault _
derive instance repGenericHsmConfigurationNotFoundFault :: Generic HsmConfigurationNotFoundFault _
instance showHsmConfigurationNotFoundFault :: Show HsmConfigurationNotFoundFault where
  show = genericShow
instance decodeHsmConfigurationNotFoundFault :: Decode HsmConfigurationNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmConfigurationNotFoundFault :: Encode HsmConfigurationNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The quota for HSM configurations has been reached. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype HsmConfigurationQuotaExceededFault = HsmConfigurationQuotaExceededFault Types.NoArguments
derive instance newtypeHsmConfigurationQuotaExceededFault :: Newtype HsmConfigurationQuotaExceededFault _
derive instance repGenericHsmConfigurationQuotaExceededFault :: Generic HsmConfigurationQuotaExceededFault _
instance showHsmConfigurationQuotaExceededFault :: Show HsmConfigurationQuotaExceededFault where
  show = genericShow
instance decodeHsmConfigurationQuotaExceededFault :: Decode HsmConfigurationQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmConfigurationQuotaExceededFault :: Encode HsmConfigurationQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the status of changes to HSM settings.</p>
newtype HsmStatus = HsmStatus 
  { "HsmClientCertificateIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "HsmConfigurationIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeHsmStatus :: Newtype HsmStatus _
derive instance repGenericHsmStatus :: Generic HsmStatus _
instance showHsmStatus :: Show HsmStatus where
  show = genericShow
instance decodeHsmStatus :: Decode HsmStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmStatus :: Encode HsmStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an IP range used in a security group.</p>
newtype IPRange = IPRange 
  { "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "CIDRIP" :: NullOrUndefined.NullOrUndefined (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeIPRange :: Newtype IPRange _
derive instance repGenericIPRange :: Generic IPRange _
instance showIPRange :: Show IPRange where
  show = genericShow
instance decodeIPRange :: Decode IPRange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPRange :: Encode IPRange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPRangeList = IPRangeList (Array IPRange)
derive instance newtypeIPRangeList :: Newtype IPRangeList _
derive instance repGenericIPRangeList :: Generic IPRangeList _
instance showIPRangeList :: Show IPRangeList where
  show = genericShow
instance decodeIPRangeList :: Decode IPRangeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPRangeList :: Encode IPRangeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IamRoleArnList = IamRoleArnList (Array String)
derive instance newtypeIamRoleArnList :: Newtype IamRoleArnList _
derive instance repGenericIamRoleArnList :: Generic IamRoleArnList _
instance showIamRoleArnList :: Show IamRoleArnList where
  show = genericShow
instance decodeIamRoleArnList :: Decode IamRoleArnList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIamRoleArnList :: Encode IamRoleArnList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportTablesCompleted = ImportTablesCompleted (Array String)
derive instance newtypeImportTablesCompleted :: Newtype ImportTablesCompleted _
derive instance repGenericImportTablesCompleted :: Generic ImportTablesCompleted _
instance showImportTablesCompleted :: Show ImportTablesCompleted where
  show = genericShow
instance decodeImportTablesCompleted :: Decode ImportTablesCompleted where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportTablesCompleted :: Encode ImportTablesCompleted where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportTablesInProgress = ImportTablesInProgress (Array String)
derive instance newtypeImportTablesInProgress :: Newtype ImportTablesInProgress _
derive instance repGenericImportTablesInProgress :: Generic ImportTablesInProgress _
instance showImportTablesInProgress :: Show ImportTablesInProgress where
  show = genericShow
instance decodeImportTablesInProgress :: Decode ImportTablesInProgress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportTablesInProgress :: Encode ImportTablesInProgress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportTablesNotStarted = ImportTablesNotStarted (Array String)
derive instance newtypeImportTablesNotStarted :: Newtype ImportTablesNotStarted _
derive instance repGenericImportTablesNotStarted :: Generic ImportTablesNotStarted _
instance showImportTablesNotStarted :: Show ImportTablesNotStarted where
  show = genericShow
instance decodeImportTablesNotStarted :: Decode ImportTablesNotStarted where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportTablesNotStarted :: Encode ImportTablesNotStarted where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have exceeded the allowed number of table restore requests. Wait for your current table restore requests to complete before making a new request.</p>
newtype InProgressTableRestoreQuotaExceededFault = InProgressTableRestoreQuotaExceededFault Types.NoArguments
derive instance newtypeInProgressTableRestoreQuotaExceededFault :: Newtype InProgressTableRestoreQuotaExceededFault _
derive instance repGenericInProgressTableRestoreQuotaExceededFault :: Generic InProgressTableRestoreQuotaExceededFault _
instance showInProgressTableRestoreQuotaExceededFault :: Show InProgressTableRestoreQuotaExceededFault where
  show = genericShow
instance decodeInProgressTableRestoreQuotaExceededFault :: Decode InProgressTableRestoreQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInProgressTableRestoreQuotaExceededFault :: Encode InProgressTableRestoreQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified options are incompatible.</p>
newtype IncompatibleOrderableOptions = IncompatibleOrderableOptions Types.NoArguments
derive instance newtypeIncompatibleOrderableOptions :: Newtype IncompatibleOrderableOptions _
derive instance repGenericIncompatibleOrderableOptions :: Generic IncompatibleOrderableOptions _
instance showIncompatibleOrderableOptions :: Show IncompatibleOrderableOptions where
  show = genericShow
instance decodeIncompatibleOrderableOptions :: Decode IncompatibleOrderableOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncompatibleOrderableOptions :: Encode IncompatibleOrderableOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of nodes specified exceeds the allotted capacity of the cluster.</p>
newtype InsufficientClusterCapacityFault = InsufficientClusterCapacityFault Types.NoArguments
derive instance newtypeInsufficientClusterCapacityFault :: Newtype InsufficientClusterCapacityFault _
derive instance repGenericInsufficientClusterCapacityFault :: Generic InsufficientClusterCapacityFault _
instance showInsufficientClusterCapacityFault :: Show InsufficientClusterCapacityFault where
  show = genericShow
instance decodeInsufficientClusterCapacityFault :: Decode InsufficientClusterCapacityFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientClusterCapacityFault :: Encode InsufficientClusterCapacityFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The cluster does not have read bucket or put object permissions on the S3 bucket specified when enabling logging.</p>
newtype InsufficientS3BucketPolicyFault = InsufficientS3BucketPolicyFault Types.NoArguments
derive instance newtypeInsufficientS3BucketPolicyFault :: Newtype InsufficientS3BucketPolicyFault _
derive instance repGenericInsufficientS3BucketPolicyFault :: Generic InsufficientS3BucketPolicyFault _
instance showInsufficientS3BucketPolicyFault :: Show InsufficientS3BucketPolicyFault where
  show = genericShow
instance decodeInsufficientS3BucketPolicyFault :: Decode InsufficientS3BucketPolicyFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientS3BucketPolicyFault :: Encode InsufficientS3BucketPolicyFault where
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


-- | <p>The cluster parameter group action can not be completed because another task is in progress that involves the parameter group. Wait a few moments and try the operation again.</p>
newtype InvalidClusterParameterGroupStateFault = InvalidClusterParameterGroupStateFault Types.NoArguments
derive instance newtypeInvalidClusterParameterGroupStateFault :: Newtype InvalidClusterParameterGroupStateFault _
derive instance repGenericInvalidClusterParameterGroupStateFault :: Generic InvalidClusterParameterGroupStateFault _
instance showInvalidClusterParameterGroupStateFault :: Show InvalidClusterParameterGroupStateFault where
  show = genericShow
instance decodeInvalidClusterParameterGroupStateFault :: Decode InvalidClusterParameterGroupStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidClusterParameterGroupStateFault :: Encode InvalidClusterParameterGroupStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The state of the cluster security group is not <code>available</code>. </p>
newtype InvalidClusterSecurityGroupStateFault = InvalidClusterSecurityGroupStateFault Types.NoArguments
derive instance newtypeInvalidClusterSecurityGroupStateFault :: Newtype InvalidClusterSecurityGroupStateFault _
derive instance repGenericInvalidClusterSecurityGroupStateFault :: Generic InvalidClusterSecurityGroupStateFault _
instance showInvalidClusterSecurityGroupStateFault :: Show InvalidClusterSecurityGroupStateFault where
  show = genericShow
instance decodeInvalidClusterSecurityGroupStateFault :: Decode InvalidClusterSecurityGroupStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidClusterSecurityGroupStateFault :: Encode InvalidClusterSecurityGroupStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified cluster snapshot is not in the <code>available</code> state, or other accounts are authorized to access the snapshot. </p>
newtype InvalidClusterSnapshotStateFault = InvalidClusterSnapshotStateFault Types.NoArguments
derive instance newtypeInvalidClusterSnapshotStateFault :: Newtype InvalidClusterSnapshotStateFault _
derive instance repGenericInvalidClusterSnapshotStateFault :: Generic InvalidClusterSnapshotStateFault _
instance showInvalidClusterSnapshotStateFault :: Show InvalidClusterSnapshotStateFault where
  show = genericShow
instance decodeInvalidClusterSnapshotStateFault :: Decode InvalidClusterSnapshotStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidClusterSnapshotStateFault :: Encode InvalidClusterSnapshotStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified cluster is not in the <code>available</code> state. </p>
newtype InvalidClusterStateFault = InvalidClusterStateFault Types.NoArguments
derive instance newtypeInvalidClusterStateFault :: Newtype InvalidClusterStateFault _
derive instance repGenericInvalidClusterStateFault :: Generic InvalidClusterStateFault _
instance showInvalidClusterStateFault :: Show InvalidClusterStateFault where
  show = genericShow
instance decodeInvalidClusterStateFault :: Decode InvalidClusterStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidClusterStateFault :: Encode InvalidClusterStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The cluster subnet group cannot be deleted because it is in use.</p>
newtype InvalidClusterSubnetGroupStateFault = InvalidClusterSubnetGroupStateFault Types.NoArguments
derive instance newtypeInvalidClusterSubnetGroupStateFault :: Newtype InvalidClusterSubnetGroupStateFault _
derive instance repGenericInvalidClusterSubnetGroupStateFault :: Generic InvalidClusterSubnetGroupStateFault _
instance showInvalidClusterSubnetGroupStateFault :: Show InvalidClusterSubnetGroupStateFault where
  show = genericShow
instance decodeInvalidClusterSubnetGroupStateFault :: Decode InvalidClusterSubnetGroupStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidClusterSubnetGroupStateFault :: Encode InvalidClusterSubnetGroupStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The state of the subnet is invalid.</p>
newtype InvalidClusterSubnetStateFault = InvalidClusterSubnetStateFault Types.NoArguments
derive instance newtypeInvalidClusterSubnetStateFault :: Newtype InvalidClusterSubnetStateFault _
derive instance repGenericInvalidClusterSubnetStateFault :: Generic InvalidClusterSubnetStateFault _
instance showInvalidClusterSubnetStateFault :: Show InvalidClusterSubnetStateFault where
  show = genericShow
instance decodeInvalidClusterSubnetStateFault :: Decode InvalidClusterSubnetStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidClusterSubnetStateFault :: Encode InvalidClusterSubnetStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Elastic IP (EIP) is invalid or cannot be found.</p>
newtype InvalidElasticIpFault = InvalidElasticIpFault Types.NoArguments
derive instance newtypeInvalidElasticIpFault :: Newtype InvalidElasticIpFault _
derive instance repGenericInvalidElasticIpFault :: Generic InvalidElasticIpFault _
instance showInvalidElasticIpFault :: Show InvalidElasticIpFault where
  show = genericShow
instance decodeInvalidElasticIpFault :: Decode InvalidElasticIpFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidElasticIpFault :: Encode InvalidElasticIpFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified HSM client certificate is not in the <code>available</code> state, or it is still in use by one or more Amazon Redshift clusters.</p>
newtype InvalidHsmClientCertificateStateFault = InvalidHsmClientCertificateStateFault Types.NoArguments
derive instance newtypeInvalidHsmClientCertificateStateFault :: Newtype InvalidHsmClientCertificateStateFault _
derive instance repGenericInvalidHsmClientCertificateStateFault :: Generic InvalidHsmClientCertificateStateFault _
instance showInvalidHsmClientCertificateStateFault :: Show InvalidHsmClientCertificateStateFault where
  show = genericShow
instance decodeInvalidHsmClientCertificateStateFault :: Decode InvalidHsmClientCertificateStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidHsmClientCertificateStateFault :: Encode InvalidHsmClientCertificateStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified HSM configuration is not in the <code>available</code> state, or it is still in use by one or more Amazon Redshift clusters.</p>
newtype InvalidHsmConfigurationStateFault = InvalidHsmConfigurationStateFault Types.NoArguments
derive instance newtypeInvalidHsmConfigurationStateFault :: Newtype InvalidHsmConfigurationStateFault _
derive instance repGenericInvalidHsmConfigurationStateFault :: Generic InvalidHsmConfigurationStateFault _
instance showInvalidHsmConfigurationStateFault :: Show InvalidHsmConfigurationStateFault where
  show = genericShow
instance decodeInvalidHsmConfigurationStateFault :: Decode InvalidHsmConfigurationStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidHsmConfigurationStateFault :: Encode InvalidHsmConfigurationStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The restore is invalid.</p>
newtype InvalidRestoreFault = InvalidRestoreFault Types.NoArguments
derive instance newtypeInvalidRestoreFault :: Newtype InvalidRestoreFault _
derive instance repGenericInvalidRestoreFault :: Generic InvalidRestoreFault _
instance showInvalidRestoreFault :: Show InvalidRestoreFault where
  show = genericShow
instance decodeInvalidRestoreFault :: Decode InvalidRestoreFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRestoreFault :: Encode InvalidRestoreFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The S3 bucket name is invalid. For more information about naming rules, go to <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html">Bucket Restrictions and Limitations</a> in the Amazon Simple Storage Service (S3) Developer Guide.</p>
newtype InvalidS3BucketNameFault = InvalidS3BucketNameFault Types.NoArguments
derive instance newtypeInvalidS3BucketNameFault :: Newtype InvalidS3BucketNameFault _
derive instance repGenericInvalidS3BucketNameFault :: Generic InvalidS3BucketNameFault _
instance showInvalidS3BucketNameFault :: Show InvalidS3BucketNameFault where
  show = genericShow
instance decodeInvalidS3BucketNameFault :: Decode InvalidS3BucketNameFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidS3BucketNameFault :: Encode InvalidS3BucketNameFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The string specified for the logging S3 key prefix does not comply with the documented constraints.</p>
newtype InvalidS3KeyPrefixFault = InvalidS3KeyPrefixFault Types.NoArguments
derive instance newtypeInvalidS3KeyPrefixFault :: Newtype InvalidS3KeyPrefixFault _
derive instance repGenericInvalidS3KeyPrefixFault :: Generic InvalidS3KeyPrefixFault _
instance showInvalidS3KeyPrefixFault :: Show InvalidS3KeyPrefixFault where
  show = genericShow
instance decodeInvalidS3KeyPrefixFault :: Decode InvalidS3KeyPrefixFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidS3KeyPrefixFault :: Encode InvalidS3KeyPrefixFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The snapshot copy grant can't be deleted because it is used by one or more clusters.</p>
newtype InvalidSnapshotCopyGrantStateFault = InvalidSnapshotCopyGrantStateFault Types.NoArguments
derive instance newtypeInvalidSnapshotCopyGrantStateFault :: Newtype InvalidSnapshotCopyGrantStateFault _
derive instance repGenericInvalidSnapshotCopyGrantStateFault :: Generic InvalidSnapshotCopyGrantStateFault _
instance showInvalidSnapshotCopyGrantStateFault :: Show InvalidSnapshotCopyGrantStateFault where
  show = genericShow
instance decodeInvalidSnapshotCopyGrantStateFault :: Decode InvalidSnapshotCopyGrantStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSnapshotCopyGrantStateFault :: Encode InvalidSnapshotCopyGrantStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested subnet is not valid, or not all of the subnets are in the same VPC.</p>
newtype InvalidSubnet = InvalidSubnet Types.NoArguments
derive instance newtypeInvalidSubnet :: Newtype InvalidSubnet _
derive instance repGenericInvalidSubnet :: Generic InvalidSubnet _
instance showInvalidSubnet :: Show InvalidSubnet where
  show = genericShow
instance decodeInvalidSubnet :: Decode InvalidSubnet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSubnet :: Encode InvalidSubnet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The subscription request is invalid because it is a duplicate request. This subscription request is already in progress.</p>
newtype InvalidSubscriptionStateFault = InvalidSubscriptionStateFault Types.NoArguments
derive instance newtypeInvalidSubscriptionStateFault :: Newtype InvalidSubscriptionStateFault _
derive instance repGenericInvalidSubscriptionStateFault :: Generic InvalidSubscriptionStateFault _
instance showInvalidSubscriptionStateFault :: Show InvalidSubscriptionStateFault where
  show = genericShow
instance decodeInvalidSubscriptionStateFault :: Decode InvalidSubscriptionStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSubscriptionStateFault :: Encode InvalidSubscriptionStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value specified for the <code>sourceDatabaseName</code>, <code>sourceSchemaName</code>, or <code>sourceTableName</code> parameter, or a combination of these, doesn't exist in the snapshot.</p>
newtype InvalidTableRestoreArgumentFault = InvalidTableRestoreArgumentFault Types.NoArguments
derive instance newtypeInvalidTableRestoreArgumentFault :: Newtype InvalidTableRestoreArgumentFault _
derive instance repGenericInvalidTableRestoreArgumentFault :: Generic InvalidTableRestoreArgumentFault _
instance showInvalidTableRestoreArgumentFault :: Show InvalidTableRestoreArgumentFault where
  show = genericShow
instance decodeInvalidTableRestoreArgumentFault :: Decode InvalidTableRestoreArgumentFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTableRestoreArgumentFault :: Encode InvalidTableRestoreArgumentFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The tag is invalid.</p>
newtype InvalidTagFault = InvalidTagFault Types.NoArguments
derive instance newtypeInvalidTagFault :: Newtype InvalidTagFault _
derive instance repGenericInvalidTagFault :: Generic InvalidTagFault _
instance showInvalidTagFault :: Show InvalidTagFault where
  show = genericShow
instance decodeInvalidTagFault :: Decode InvalidTagFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTagFault :: Encode InvalidTagFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The cluster subnet group does not cover all Availability Zones.</p>
newtype InvalidVPCNetworkStateFault = InvalidVPCNetworkStateFault Types.NoArguments
derive instance newtypeInvalidVPCNetworkStateFault :: Newtype InvalidVPCNetworkStateFault _
derive instance repGenericInvalidVPCNetworkStateFault :: Generic InvalidVPCNetworkStateFault _
instance showInvalidVPCNetworkStateFault :: Show InvalidVPCNetworkStateFault where
  show = genericShow
instance decodeInvalidVPCNetworkStateFault :: Decode InvalidVPCNetworkStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidVPCNetworkStateFault :: Encode InvalidVPCNetworkStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The encryption key has exceeded its grant limit in AWS KMS.</p>
newtype LimitExceededFault = LimitExceededFault Types.NoArguments
derive instance newtypeLimitExceededFault :: Newtype LimitExceededFault _
derive instance repGenericLimitExceededFault :: Generic LimitExceededFault _
instance showLimitExceededFault :: Show LimitExceededFault where
  show = genericShow
instance decodeLimitExceededFault :: Decode LimitExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededFault :: Encode LimitExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the status of logging for a cluster.</p>
newtype LoggingStatus = LoggingStatus 
  { "LoggingEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "BucketName" :: NullOrUndefined.NullOrUndefined (String)
  , "S3KeyPrefix" :: NullOrUndefined.NullOrUndefined (String)
  , "LastSuccessfulDeliveryTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "LastFailureTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "LastFailureMessage" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLoggingStatus :: Newtype LoggingStatus _
derive instance repGenericLoggingStatus :: Generic LoggingStatus _
instance showLoggingStatus :: Show LoggingStatus where
  show = genericShow
instance decodeLoggingStatus :: Decode LoggingStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoggingStatus :: Encode LoggingStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LongOptional = LongOptional Number
derive instance newtypeLongOptional :: Newtype LongOptional _
derive instance repGenericLongOptional :: Generic LongOptional _
instance showLongOptional :: Show LongOptional where
  show = genericShow
instance decodeLongOptional :: Decode LongOptional where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLongOptional :: Encode LongOptional where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ModifyClusterIamRolesMessage = ModifyClusterIamRolesMessage 
  { "ClusterIdentifier" :: (String)
  , "AddIamRoles" :: NullOrUndefined.NullOrUndefined (IamRoleArnList)
  , "RemoveIamRoles" :: NullOrUndefined.NullOrUndefined (IamRoleArnList)
  }
derive instance newtypeModifyClusterIamRolesMessage :: Newtype ModifyClusterIamRolesMessage _
derive instance repGenericModifyClusterIamRolesMessage :: Generic ModifyClusterIamRolesMessage _
instance showModifyClusterIamRolesMessage :: Show ModifyClusterIamRolesMessage where
  show = genericShow
instance decodeModifyClusterIamRolesMessage :: Decode ModifyClusterIamRolesMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyClusterIamRolesMessage :: Encode ModifyClusterIamRolesMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyClusterIamRolesResult = ModifyClusterIamRolesResult 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeModifyClusterIamRolesResult :: Newtype ModifyClusterIamRolesResult _
derive instance repGenericModifyClusterIamRolesResult :: Generic ModifyClusterIamRolesResult _
instance showModifyClusterIamRolesResult :: Show ModifyClusterIamRolesResult where
  show = genericShow
instance decodeModifyClusterIamRolesResult :: Decode ModifyClusterIamRolesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyClusterIamRolesResult :: Encode ModifyClusterIamRolesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ModifyClusterMessage = ModifyClusterMessage 
  { "ClusterIdentifier" :: (String)
  , "ClusterType" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "NumberOfNodes" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "ClusterSecurityGroups" :: NullOrUndefined.NullOrUndefined (ClusterSecurityGroupNameList)
  , "VpcSecurityGroupIds" :: NullOrUndefined.NullOrUndefined (VpcSecurityGroupIdList)
  , "MasterUserPassword" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "AllowVersionUpgrade" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "HsmClientCertificateIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "HsmConfigurationIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "NewClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "ElasticIp" :: NullOrUndefined.NullOrUndefined (String)
  , "EnhancedVpcRouting" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeModifyClusterMessage :: Newtype ModifyClusterMessage _
derive instance repGenericModifyClusterMessage :: Generic ModifyClusterMessage _
instance showModifyClusterMessage :: Show ModifyClusterMessage where
  show = genericShow
instance decodeModifyClusterMessage :: Decode ModifyClusterMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyClusterMessage :: Encode ModifyClusterMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ModifyClusterParameterGroupMessage = ModifyClusterParameterGroupMessage 
  { "ParameterGroupName" :: (String)
  , "Parameters" :: (ParametersList)
  }
derive instance newtypeModifyClusterParameterGroupMessage :: Newtype ModifyClusterParameterGroupMessage _
derive instance repGenericModifyClusterParameterGroupMessage :: Generic ModifyClusterParameterGroupMessage _
instance showModifyClusterParameterGroupMessage :: Show ModifyClusterParameterGroupMessage where
  show = genericShow
instance decodeModifyClusterParameterGroupMessage :: Decode ModifyClusterParameterGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyClusterParameterGroupMessage :: Encode ModifyClusterParameterGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyClusterResult = ModifyClusterResult 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeModifyClusterResult :: Newtype ModifyClusterResult _
derive instance repGenericModifyClusterResult :: Generic ModifyClusterResult _
instance showModifyClusterResult :: Show ModifyClusterResult where
  show = genericShow
instance decodeModifyClusterResult :: Decode ModifyClusterResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyClusterResult :: Encode ModifyClusterResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ModifyClusterSubnetGroupMessage = ModifyClusterSubnetGroupMessage 
  { "ClusterSubnetGroupName" :: (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  }
derive instance newtypeModifyClusterSubnetGroupMessage :: Newtype ModifyClusterSubnetGroupMessage _
derive instance repGenericModifyClusterSubnetGroupMessage :: Generic ModifyClusterSubnetGroupMessage _
instance showModifyClusterSubnetGroupMessage :: Show ModifyClusterSubnetGroupMessage where
  show = genericShow
instance decodeModifyClusterSubnetGroupMessage :: Decode ModifyClusterSubnetGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyClusterSubnetGroupMessage :: Encode ModifyClusterSubnetGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyClusterSubnetGroupResult = ModifyClusterSubnetGroupResult 
  { "ClusterSubnetGroup" :: NullOrUndefined.NullOrUndefined (ClusterSubnetGroup)
  }
derive instance newtypeModifyClusterSubnetGroupResult :: Newtype ModifyClusterSubnetGroupResult _
derive instance repGenericModifyClusterSubnetGroupResult :: Generic ModifyClusterSubnetGroupResult _
instance showModifyClusterSubnetGroupResult :: Show ModifyClusterSubnetGroupResult where
  show = genericShow
instance decodeModifyClusterSubnetGroupResult :: Decode ModifyClusterSubnetGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyClusterSubnetGroupResult :: Encode ModifyClusterSubnetGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ModifyEventSubscriptionMessage = ModifyEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SnsTopicArn" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceIds" :: NullOrUndefined.NullOrUndefined (SourceIdsList)
  , "EventCategories" :: NullOrUndefined.NullOrUndefined (EventCategoriesList)
  , "Severity" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeModifyEventSubscriptionMessage :: Newtype ModifyEventSubscriptionMessage _
derive instance repGenericModifyEventSubscriptionMessage :: Generic ModifyEventSubscriptionMessage _
instance showModifyEventSubscriptionMessage :: Show ModifyEventSubscriptionMessage where
  show = genericShow
instance decodeModifyEventSubscriptionMessage :: Decode ModifyEventSubscriptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyEventSubscriptionMessage :: Encode ModifyEventSubscriptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyEventSubscriptionResult = ModifyEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined.NullOrUndefined (EventSubscription)
  }
derive instance newtypeModifyEventSubscriptionResult :: Newtype ModifyEventSubscriptionResult _
derive instance repGenericModifyEventSubscriptionResult :: Generic ModifyEventSubscriptionResult _
instance showModifyEventSubscriptionResult :: Show ModifyEventSubscriptionResult where
  show = genericShow
instance decodeModifyEventSubscriptionResult :: Decode ModifyEventSubscriptionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyEventSubscriptionResult :: Encode ModifyEventSubscriptionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ModifySnapshotCopyRetentionPeriodMessage = ModifySnapshotCopyRetentionPeriodMessage 
  { "ClusterIdentifier" :: (String)
  , "RetentionPeriod" :: (Int)
  }
derive instance newtypeModifySnapshotCopyRetentionPeriodMessage :: Newtype ModifySnapshotCopyRetentionPeriodMessage _
derive instance repGenericModifySnapshotCopyRetentionPeriodMessage :: Generic ModifySnapshotCopyRetentionPeriodMessage _
instance showModifySnapshotCopyRetentionPeriodMessage :: Show ModifySnapshotCopyRetentionPeriodMessage where
  show = genericShow
instance decodeModifySnapshotCopyRetentionPeriodMessage :: Decode ModifySnapshotCopyRetentionPeriodMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifySnapshotCopyRetentionPeriodMessage :: Encode ModifySnapshotCopyRetentionPeriodMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifySnapshotCopyRetentionPeriodResult = ModifySnapshotCopyRetentionPeriodResult 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeModifySnapshotCopyRetentionPeriodResult :: Newtype ModifySnapshotCopyRetentionPeriodResult _
derive instance repGenericModifySnapshotCopyRetentionPeriodResult :: Generic ModifySnapshotCopyRetentionPeriodResult _
instance showModifySnapshotCopyRetentionPeriodResult :: Show ModifySnapshotCopyRetentionPeriodResult where
  show = genericShow
instance decodeModifySnapshotCopyRetentionPeriodResult :: Decode ModifySnapshotCopyRetentionPeriodResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifySnapshotCopyRetentionPeriodResult :: Encode ModifySnapshotCopyRetentionPeriodResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation would exceed the number of nodes allowed for a cluster.</p>
newtype NumberOfNodesPerClusterLimitExceededFault = NumberOfNodesPerClusterLimitExceededFault Types.NoArguments
derive instance newtypeNumberOfNodesPerClusterLimitExceededFault :: Newtype NumberOfNodesPerClusterLimitExceededFault _
derive instance repGenericNumberOfNodesPerClusterLimitExceededFault :: Generic NumberOfNodesPerClusterLimitExceededFault _
instance showNumberOfNodesPerClusterLimitExceededFault :: Show NumberOfNodesPerClusterLimitExceededFault where
  show = genericShow
instance decodeNumberOfNodesPerClusterLimitExceededFault :: Decode NumberOfNodesPerClusterLimitExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNumberOfNodesPerClusterLimitExceededFault :: Encode NumberOfNodesPerClusterLimitExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation would exceed the number of nodes allotted to the account. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype NumberOfNodesQuotaExceededFault = NumberOfNodesQuotaExceededFault Types.NoArguments
derive instance newtypeNumberOfNodesQuotaExceededFault :: Newtype NumberOfNodesQuotaExceededFault _
derive instance repGenericNumberOfNodesQuotaExceededFault :: Generic NumberOfNodesQuotaExceededFault _
instance showNumberOfNodesQuotaExceededFault :: Show NumberOfNodesQuotaExceededFault where
  show = genericShow
instance decodeNumberOfNodesQuotaExceededFault :: Decode NumberOfNodesQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNumberOfNodesQuotaExceededFault :: Encode NumberOfNodesQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an orderable cluster option.</p>
newtype OrderableClusterOption = OrderableClusterOption 
  { "ClusterVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterType" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "AvailabilityZones" :: NullOrUndefined.NullOrUndefined (AvailabilityZoneList)
  }
derive instance newtypeOrderableClusterOption :: Newtype OrderableClusterOption _
derive instance repGenericOrderableClusterOption :: Generic OrderableClusterOption _
instance showOrderableClusterOption :: Show OrderableClusterOption where
  show = genericShow
instance decodeOrderableClusterOption :: Decode OrderableClusterOption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderableClusterOption :: Encode OrderableClusterOption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrderableClusterOptionsList = OrderableClusterOptionsList (Array OrderableClusterOption)
derive instance newtypeOrderableClusterOptionsList :: Newtype OrderableClusterOptionsList _
derive instance repGenericOrderableClusterOptionsList :: Generic OrderableClusterOptionsList _
instance showOrderableClusterOptionsList :: Show OrderableClusterOptionsList where
  show = genericShow
instance decodeOrderableClusterOptionsList :: Decode OrderableClusterOptionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderableClusterOptionsList :: Encode OrderableClusterOptionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output from the <a>DescribeOrderableClusterOptions</a> action. </p>
newtype OrderableClusterOptionsMessage = OrderableClusterOptionsMessage 
  { "OrderableClusterOptions" :: NullOrUndefined.NullOrUndefined (OrderableClusterOptionsList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOrderableClusterOptionsMessage :: Newtype OrderableClusterOptionsMessage _
derive instance repGenericOrderableClusterOptionsMessage :: Generic OrderableClusterOptionsMessage _
instance showOrderableClusterOptionsMessage :: Show OrderableClusterOptionsMessage where
  show = genericShow
instance decodeOrderableClusterOptionsMessage :: Decode OrderableClusterOptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderableClusterOptionsMessage :: Encode OrderableClusterOptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a parameter in a cluster parameter group.</p>
newtype Parameter = Parameter 
  { "ParameterName" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterValue" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Source" :: NullOrUndefined.NullOrUndefined (String)
  , "DataType" :: NullOrUndefined.NullOrUndefined (String)
  , "AllowedValues" :: NullOrUndefined.NullOrUndefined (String)
  , "ApplyType" :: NullOrUndefined.NullOrUndefined (ParameterApplyType)
  , "IsModifiable" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "MinimumEngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeParameter :: Newtype Parameter _
derive instance repGenericParameter :: Generic Parameter _
instance showParameter :: Show Parameter where
  show = genericShow
instance decodeParameter :: Decode Parameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameter :: Encode Parameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterApplyType = ParameterApplyType String
derive instance newtypeParameterApplyType :: Newtype ParameterApplyType _
derive instance repGenericParameterApplyType :: Generic ParameterApplyType _
instance showParameterApplyType :: Show ParameterApplyType where
  show = genericShow
instance decodeParameterApplyType :: Decode ParameterApplyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterApplyType :: Encode ParameterApplyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterGroupList = ParameterGroupList (Array ClusterParameterGroup)
derive instance newtypeParameterGroupList :: Newtype ParameterGroupList _
derive instance repGenericParameterGroupList :: Generic ParameterGroupList _
instance showParameterGroupList :: Show ParameterGroupList where
  show = genericShow
instance decodeParameterGroupList :: Decode ParameterGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterGroupList :: Encode ParameterGroupList where
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


-- | <p>Describes cluster attributes that are in a pending state. A change to one or more the attributes was requested and is in progress or will be applied.</p>
newtype PendingModifiedValues = PendingModifiedValues 
  { "MasterUserPassword" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "NumberOfNodes" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "ClusterType" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "ClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "EnhancedVpcRouting" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypePendingModifiedValues :: Newtype PendingModifiedValues _
derive instance repGenericPendingModifiedValues :: Generic PendingModifiedValues _
instance showPendingModifiedValues :: Show PendingModifiedValues where
  show = genericShow
instance decodePendingModifiedValues :: Decode PendingModifiedValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePendingModifiedValues :: Encode PendingModifiedValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype PurchaseReservedNodeOfferingMessage = PurchaseReservedNodeOfferingMessage 
  { "ReservedNodeOfferingId" :: (String)
  , "NodeCount" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  }
derive instance newtypePurchaseReservedNodeOfferingMessage :: Newtype PurchaseReservedNodeOfferingMessage _
derive instance repGenericPurchaseReservedNodeOfferingMessage :: Generic PurchaseReservedNodeOfferingMessage _
instance showPurchaseReservedNodeOfferingMessage :: Show PurchaseReservedNodeOfferingMessage where
  show = genericShow
instance decodePurchaseReservedNodeOfferingMessage :: Decode PurchaseReservedNodeOfferingMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurchaseReservedNodeOfferingMessage :: Encode PurchaseReservedNodeOfferingMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PurchaseReservedNodeOfferingResult = PurchaseReservedNodeOfferingResult 
  { "ReservedNode" :: NullOrUndefined.NullOrUndefined (ReservedNode)
  }
derive instance newtypePurchaseReservedNodeOfferingResult :: Newtype PurchaseReservedNodeOfferingResult _
derive instance repGenericPurchaseReservedNodeOfferingResult :: Generic PurchaseReservedNodeOfferingResult _
instance showPurchaseReservedNodeOfferingResult :: Show PurchaseReservedNodeOfferingResult where
  show = genericShow
instance decodePurchaseReservedNodeOfferingResult :: Decode PurchaseReservedNodeOfferingResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurchaseReservedNodeOfferingResult :: Encode PurchaseReservedNodeOfferingResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype RebootClusterMessage = RebootClusterMessage 
  { "ClusterIdentifier" :: (String)
  }
derive instance newtypeRebootClusterMessage :: Newtype RebootClusterMessage _
derive instance repGenericRebootClusterMessage :: Generic RebootClusterMessage _
instance showRebootClusterMessage :: Show RebootClusterMessage where
  show = genericShow
instance decodeRebootClusterMessage :: Decode RebootClusterMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebootClusterMessage :: Encode RebootClusterMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RebootClusterResult = RebootClusterResult 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeRebootClusterResult :: Newtype RebootClusterResult _
derive instance repGenericRebootClusterResult :: Generic RebootClusterResult _
instance showRebootClusterResult :: Show RebootClusterResult where
  show = genericShow
instance decodeRebootClusterResult :: Decode RebootClusterResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebootClusterResult :: Encode RebootClusterResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a recurring charge.</p>
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


-- | <p>Describes a reserved node. You can call the <a>DescribeReservedNodeOfferings</a> API to obtain the available reserved node offerings. </p>
newtype ReservedNode = ReservedNode 
  { "ReservedNodeId" :: NullOrUndefined.NullOrUndefined (String)
  , "ReservedNodeOfferingId" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "Duration" :: NullOrUndefined.NullOrUndefined (Int)
  , "FixedPrice" :: NullOrUndefined.NullOrUndefined (Number)
  , "UsagePrice" :: NullOrUndefined.NullOrUndefined (Number)
  , "CurrencyCode" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "State" :: NullOrUndefined.NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined.NullOrUndefined (String)
  , "RecurringCharges" :: NullOrUndefined.NullOrUndefined (RecurringChargeList)
  }
derive instance newtypeReservedNode :: Newtype ReservedNode _
derive instance repGenericReservedNode :: Generic ReservedNode _
instance showReservedNode :: Show ReservedNode where
  show = genericShow
instance decodeReservedNode :: Decode ReservedNode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedNode :: Encode ReservedNode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>User already has a reservation with the given identifier.</p>
newtype ReservedNodeAlreadyExistsFault = ReservedNodeAlreadyExistsFault Types.NoArguments
derive instance newtypeReservedNodeAlreadyExistsFault :: Newtype ReservedNodeAlreadyExistsFault _
derive instance repGenericReservedNodeAlreadyExistsFault :: Generic ReservedNodeAlreadyExistsFault _
instance showReservedNodeAlreadyExistsFault :: Show ReservedNodeAlreadyExistsFault where
  show = genericShow
instance decodeReservedNodeAlreadyExistsFault :: Decode ReservedNodeAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedNodeAlreadyExistsFault :: Encode ReservedNodeAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservedNodeList = ReservedNodeList (Array ReservedNode)
derive instance newtypeReservedNodeList :: Newtype ReservedNodeList _
derive instance repGenericReservedNodeList :: Generic ReservedNodeList _
instance showReservedNodeList :: Show ReservedNodeList where
  show = genericShow
instance decodeReservedNodeList :: Decode ReservedNodeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedNodeList :: Encode ReservedNodeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified reserved compute node not found.</p>
newtype ReservedNodeNotFoundFault = ReservedNodeNotFoundFault Types.NoArguments
derive instance newtypeReservedNodeNotFoundFault :: Newtype ReservedNodeNotFoundFault _
derive instance repGenericReservedNodeNotFoundFault :: Generic ReservedNodeNotFoundFault _
instance showReservedNodeNotFoundFault :: Show ReservedNodeNotFoundFault where
  show = genericShow
instance decodeReservedNodeNotFoundFault :: Decode ReservedNodeNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedNodeNotFoundFault :: Encode ReservedNodeNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a reserved node offering.</p>
newtype ReservedNodeOffering = ReservedNodeOffering 
  { "ReservedNodeOfferingId" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "Duration" :: NullOrUndefined.NullOrUndefined (Int)
  , "FixedPrice" :: NullOrUndefined.NullOrUndefined (Number)
  , "UsagePrice" :: NullOrUndefined.NullOrUndefined (Number)
  , "CurrencyCode" :: NullOrUndefined.NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined.NullOrUndefined (String)
  , "RecurringCharges" :: NullOrUndefined.NullOrUndefined (RecurringChargeList)
  }
derive instance newtypeReservedNodeOffering :: Newtype ReservedNodeOffering _
derive instance repGenericReservedNodeOffering :: Generic ReservedNodeOffering _
instance showReservedNodeOffering :: Show ReservedNodeOffering where
  show = genericShow
instance decodeReservedNodeOffering :: Decode ReservedNodeOffering where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedNodeOffering :: Encode ReservedNodeOffering where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservedNodeOfferingList = ReservedNodeOfferingList (Array ReservedNodeOffering)
derive instance newtypeReservedNodeOfferingList :: Newtype ReservedNodeOfferingList _
derive instance repGenericReservedNodeOfferingList :: Generic ReservedNodeOfferingList _
instance showReservedNodeOfferingList :: Show ReservedNodeOfferingList where
  show = genericShow
instance decodeReservedNodeOfferingList :: Decode ReservedNodeOfferingList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedNodeOfferingList :: Encode ReservedNodeOfferingList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specified offering does not exist.</p>
newtype ReservedNodeOfferingNotFoundFault = ReservedNodeOfferingNotFoundFault Types.NoArguments
derive instance newtypeReservedNodeOfferingNotFoundFault :: Newtype ReservedNodeOfferingNotFoundFault _
derive instance repGenericReservedNodeOfferingNotFoundFault :: Generic ReservedNodeOfferingNotFoundFault _
instance showReservedNodeOfferingNotFoundFault :: Show ReservedNodeOfferingNotFoundFault where
  show = genericShow
instance decodeReservedNodeOfferingNotFoundFault :: Decode ReservedNodeOfferingNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedNodeOfferingNotFoundFault :: Encode ReservedNodeOfferingNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ReservedNodeOfferingsMessage = ReservedNodeOfferingsMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "ReservedNodeOfferings" :: NullOrUndefined.NullOrUndefined (ReservedNodeOfferingList)
  }
derive instance newtypeReservedNodeOfferingsMessage :: Newtype ReservedNodeOfferingsMessage _
derive instance repGenericReservedNodeOfferingsMessage :: Generic ReservedNodeOfferingsMessage _
instance showReservedNodeOfferingsMessage :: Show ReservedNodeOfferingsMessage where
  show = genericShow
instance decodeReservedNodeOfferingsMessage :: Decode ReservedNodeOfferingsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedNodeOfferingsMessage :: Encode ReservedNodeOfferingsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request would exceed the user's compute node quota. For information about increasing your quota, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html">Limits in Amazon Redshift</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype ReservedNodeQuotaExceededFault = ReservedNodeQuotaExceededFault Types.NoArguments
derive instance newtypeReservedNodeQuotaExceededFault :: Newtype ReservedNodeQuotaExceededFault _
derive instance repGenericReservedNodeQuotaExceededFault :: Generic ReservedNodeQuotaExceededFault _
instance showReservedNodeQuotaExceededFault :: Show ReservedNodeQuotaExceededFault where
  show = genericShow
instance decodeReservedNodeQuotaExceededFault :: Decode ReservedNodeQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedNodeQuotaExceededFault :: Encode ReservedNodeQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ReservedNodesMessage = ReservedNodesMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "ReservedNodes" :: NullOrUndefined.NullOrUndefined (ReservedNodeList)
  }
derive instance newtypeReservedNodesMessage :: Newtype ReservedNodesMessage _
derive instance repGenericReservedNodesMessage :: Generic ReservedNodesMessage _
instance showReservedNodesMessage :: Show ReservedNodesMessage where
  show = genericShow
instance decodeReservedNodesMessage :: Decode ReservedNodesMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedNodesMessage :: Encode ReservedNodesMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ResetClusterParameterGroupMessage = ResetClusterParameterGroupMessage 
  { "ParameterGroupName" :: (String)
  , "ResetAllParameters" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersList)
  }
derive instance newtypeResetClusterParameterGroupMessage :: Newtype ResetClusterParameterGroupMessage _
derive instance repGenericResetClusterParameterGroupMessage :: Generic ResetClusterParameterGroupMessage _
instance showResetClusterParameterGroupMessage :: Show ResetClusterParameterGroupMessage where
  show = genericShow
instance decodeResetClusterParameterGroupMessage :: Decode ResetClusterParameterGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResetClusterParameterGroupMessage :: Encode ResetClusterParameterGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A resize operation for the specified cluster is not found.</p>
newtype ResizeNotFoundFault = ResizeNotFoundFault Types.NoArguments
derive instance newtypeResizeNotFoundFault :: Newtype ResizeNotFoundFault _
derive instance repGenericResizeNotFoundFault :: Generic ResizeNotFoundFault _
instance showResizeNotFoundFault :: Show ResizeNotFoundFault where
  show = genericShow
instance decodeResizeNotFoundFault :: Decode ResizeNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResizeNotFoundFault :: Encode ResizeNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the result of a cluster resize operation.</p>
newtype ResizeProgressMessage = ResizeProgressMessage 
  { "TargetNodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "TargetNumberOfNodes" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "TargetClusterType" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "ImportTablesCompleted" :: NullOrUndefined.NullOrUndefined (ImportTablesCompleted)
  , "ImportTablesInProgress" :: NullOrUndefined.NullOrUndefined (ImportTablesInProgress)
  , "ImportTablesNotStarted" :: NullOrUndefined.NullOrUndefined (ImportTablesNotStarted)
  , "AvgResizeRateInMegaBytesPerSecond" :: NullOrUndefined.NullOrUndefined (DoubleOptional)
  , "TotalResizeDataInMegaBytes" :: NullOrUndefined.NullOrUndefined (LongOptional)
  , "ProgressInMegaBytes" :: NullOrUndefined.NullOrUndefined (LongOptional)
  , "ElapsedTimeInSeconds" :: NullOrUndefined.NullOrUndefined (LongOptional)
  , "EstimatedTimeToCompletionInSeconds" :: NullOrUndefined.NullOrUndefined (LongOptional)
  }
derive instance newtypeResizeProgressMessage :: Newtype ResizeProgressMessage _
derive instance repGenericResizeProgressMessage :: Generic ResizeProgressMessage _
instance showResizeProgressMessage :: Show ResizeProgressMessage where
  show = genericShow
instance decodeResizeProgressMessage :: Decode ResizeProgressMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResizeProgressMessage :: Encode ResizeProgressMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource could not be found.</p>
newtype ResourceNotFoundFault = ResourceNotFoundFault Types.NoArguments
derive instance newtypeResourceNotFoundFault :: Newtype ResourceNotFoundFault _
derive instance repGenericResourceNotFoundFault :: Generic ResourceNotFoundFault _
instance showResourceNotFoundFault :: Show ResourceNotFoundFault where
  show = genericShow
instance decodeResourceNotFoundFault :: Decode ResourceNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundFault :: Encode ResourceNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestorableNodeTypeList = RestorableNodeTypeList (Array String)
derive instance newtypeRestorableNodeTypeList :: Newtype RestorableNodeTypeList _
derive instance repGenericRestorableNodeTypeList :: Generic RestorableNodeTypeList _
instance showRestorableNodeTypeList :: Show RestorableNodeTypeList where
  show = genericShow
instance decodeRestorableNodeTypeList :: Decode RestorableNodeTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestorableNodeTypeList :: Encode RestorableNodeTypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype RestoreFromClusterSnapshotMessage = RestoreFromClusterSnapshotMessage 
  { "ClusterIdentifier" :: (String)
  , "SnapshotIdentifier" :: (String)
  , "SnapshotClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "AllowVersionUpgrade" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "ClusterSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "OwnerAccount" :: NullOrUndefined.NullOrUndefined (String)
  , "HsmClientCertificateIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "HsmConfigurationIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "ElasticIp" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterSecurityGroups" :: NullOrUndefined.NullOrUndefined (ClusterSecurityGroupNameList)
  , "VpcSecurityGroupIds" :: NullOrUndefined.NullOrUndefined (VpcSecurityGroupIdList)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "AutomatedSnapshotRetentionPeriod" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "EnhancedVpcRouting" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "AdditionalInfo" :: NullOrUndefined.NullOrUndefined (String)
  , "IamRoles" :: NullOrUndefined.NullOrUndefined (IamRoleArnList)
  }
derive instance newtypeRestoreFromClusterSnapshotMessage :: Newtype RestoreFromClusterSnapshotMessage _
derive instance repGenericRestoreFromClusterSnapshotMessage :: Generic RestoreFromClusterSnapshotMessage _
instance showRestoreFromClusterSnapshotMessage :: Show RestoreFromClusterSnapshotMessage where
  show = genericShow
instance decodeRestoreFromClusterSnapshotMessage :: Decode RestoreFromClusterSnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreFromClusterSnapshotMessage :: Encode RestoreFromClusterSnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestoreFromClusterSnapshotResult = RestoreFromClusterSnapshotResult 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeRestoreFromClusterSnapshotResult :: Newtype RestoreFromClusterSnapshotResult _
derive instance repGenericRestoreFromClusterSnapshotResult :: Generic RestoreFromClusterSnapshotResult _
instance showRestoreFromClusterSnapshotResult :: Show RestoreFromClusterSnapshotResult where
  show = genericShow
instance decodeRestoreFromClusterSnapshotResult :: Decode RestoreFromClusterSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreFromClusterSnapshotResult :: Encode RestoreFromClusterSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the status of a cluster restore action. Returns null if the cluster was not created by restoring a snapshot.</p>
newtype RestoreStatus = RestoreStatus 
  { "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "CurrentRestoreRateInMegaBytesPerSecond" :: NullOrUndefined.NullOrUndefined (Number)
  , "SnapshotSizeInMegaBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "ProgressInMegaBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "ElapsedTimeInSeconds" :: NullOrUndefined.NullOrUndefined (Number)
  , "EstimatedTimeToCompletionInSeconds" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeRestoreStatus :: Newtype RestoreStatus _
derive instance repGenericRestoreStatus :: Generic RestoreStatus _
instance showRestoreStatus :: Show RestoreStatus where
  show = genericShow
instance decodeRestoreStatus :: Decode RestoreStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreStatus :: Encode RestoreStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype RestoreTableFromClusterSnapshotMessage = RestoreTableFromClusterSnapshotMessage 
  { "ClusterIdentifier" :: (String)
  , "SnapshotIdentifier" :: (String)
  , "SourceDatabaseName" :: (String)
  , "SourceSchemaName" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceTableName" :: (String)
  , "TargetDatabaseName" :: NullOrUndefined.NullOrUndefined (String)
  , "TargetSchemaName" :: NullOrUndefined.NullOrUndefined (String)
  , "NewTableName" :: (String)
  }
derive instance newtypeRestoreTableFromClusterSnapshotMessage :: Newtype RestoreTableFromClusterSnapshotMessage _
derive instance repGenericRestoreTableFromClusterSnapshotMessage :: Generic RestoreTableFromClusterSnapshotMessage _
instance showRestoreTableFromClusterSnapshotMessage :: Show RestoreTableFromClusterSnapshotMessage where
  show = genericShow
instance decodeRestoreTableFromClusterSnapshotMessage :: Decode RestoreTableFromClusterSnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreTableFromClusterSnapshotMessage :: Encode RestoreTableFromClusterSnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestoreTableFromClusterSnapshotResult = RestoreTableFromClusterSnapshotResult 
  { "TableRestoreStatus" :: NullOrUndefined.NullOrUndefined (TableRestoreStatus)
  }
derive instance newtypeRestoreTableFromClusterSnapshotResult :: Newtype RestoreTableFromClusterSnapshotResult _
derive instance repGenericRestoreTableFromClusterSnapshotResult :: Generic RestoreTableFromClusterSnapshotResult _
instance showRestoreTableFromClusterSnapshotResult :: Show RestoreTableFromClusterSnapshotResult where
  show = genericShow
instance decodeRestoreTableFromClusterSnapshotResult :: Decode RestoreTableFromClusterSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreTableFromClusterSnapshotResult :: Encode RestoreTableFromClusterSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype RevokeClusterSecurityGroupIngressMessage = RevokeClusterSecurityGroupIngressMessage 
  { "ClusterSecurityGroupName" :: (String)
  , "CIDRIP" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRevokeClusterSecurityGroupIngressMessage :: Newtype RevokeClusterSecurityGroupIngressMessage _
derive instance repGenericRevokeClusterSecurityGroupIngressMessage :: Generic RevokeClusterSecurityGroupIngressMessage _
instance showRevokeClusterSecurityGroupIngressMessage :: Show RevokeClusterSecurityGroupIngressMessage where
  show = genericShow
instance decodeRevokeClusterSecurityGroupIngressMessage :: Decode RevokeClusterSecurityGroupIngressMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevokeClusterSecurityGroupIngressMessage :: Encode RevokeClusterSecurityGroupIngressMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RevokeClusterSecurityGroupIngressResult = RevokeClusterSecurityGroupIngressResult 
  { "ClusterSecurityGroup" :: NullOrUndefined.NullOrUndefined (ClusterSecurityGroup)
  }
derive instance newtypeRevokeClusterSecurityGroupIngressResult :: Newtype RevokeClusterSecurityGroupIngressResult _
derive instance repGenericRevokeClusterSecurityGroupIngressResult :: Generic RevokeClusterSecurityGroupIngressResult _
instance showRevokeClusterSecurityGroupIngressResult :: Show RevokeClusterSecurityGroupIngressResult where
  show = genericShow
instance decodeRevokeClusterSecurityGroupIngressResult :: Decode RevokeClusterSecurityGroupIngressResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevokeClusterSecurityGroupIngressResult :: Encode RevokeClusterSecurityGroupIngressResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype RevokeSnapshotAccessMessage = RevokeSnapshotAccessMessage 
  { "SnapshotIdentifier" :: (String)
  , "SnapshotClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "AccountWithRestoreAccess" :: (String)
  }
derive instance newtypeRevokeSnapshotAccessMessage :: Newtype RevokeSnapshotAccessMessage _
derive instance repGenericRevokeSnapshotAccessMessage :: Generic RevokeSnapshotAccessMessage _
instance showRevokeSnapshotAccessMessage :: Show RevokeSnapshotAccessMessage where
  show = genericShow
instance decodeRevokeSnapshotAccessMessage :: Decode RevokeSnapshotAccessMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevokeSnapshotAccessMessage :: Encode RevokeSnapshotAccessMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RevokeSnapshotAccessResult = RevokeSnapshotAccessResult 
  { "Snapshot" :: NullOrUndefined.NullOrUndefined (Snapshot)
  }
derive instance newtypeRevokeSnapshotAccessResult :: Newtype RevokeSnapshotAccessResult _
derive instance repGenericRevokeSnapshotAccessResult :: Generic RevokeSnapshotAccessResult _
instance showRevokeSnapshotAccessResult :: Show RevokeSnapshotAccessResult where
  show = genericShow
instance decodeRevokeSnapshotAccessResult :: Decode RevokeSnapshotAccessResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevokeSnapshotAccessResult :: Encode RevokeSnapshotAccessResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype RotateEncryptionKeyMessage = RotateEncryptionKeyMessage 
  { "ClusterIdentifier" :: (String)
  }
derive instance newtypeRotateEncryptionKeyMessage :: Newtype RotateEncryptionKeyMessage _
derive instance repGenericRotateEncryptionKeyMessage :: Generic RotateEncryptionKeyMessage _
instance showRotateEncryptionKeyMessage :: Show RotateEncryptionKeyMessage where
  show = genericShow
instance decodeRotateEncryptionKeyMessage :: Decode RotateEncryptionKeyMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRotateEncryptionKeyMessage :: Encode RotateEncryptionKeyMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RotateEncryptionKeyResult = RotateEncryptionKeyResult 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeRotateEncryptionKeyResult :: Newtype RotateEncryptionKeyResult _
derive instance repGenericRotateEncryptionKeyResult :: Generic RotateEncryptionKeyResult _
instance showRotateEncryptionKeyResult :: Show RotateEncryptionKeyResult where
  show = genericShow
instance decodeRotateEncryptionKeyResult :: Decode RotateEncryptionKeyResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRotateEncryptionKeyResult :: Encode RotateEncryptionKeyResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon SNS has responded that there is a problem with the specified Amazon SNS topic.</p>
newtype SNSInvalidTopicFault = SNSInvalidTopicFault Types.NoArguments
derive instance newtypeSNSInvalidTopicFault :: Newtype SNSInvalidTopicFault _
derive instance repGenericSNSInvalidTopicFault :: Generic SNSInvalidTopicFault _
instance showSNSInvalidTopicFault :: Show SNSInvalidTopicFault where
  show = genericShow
instance decodeSNSInvalidTopicFault :: Decode SNSInvalidTopicFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSNSInvalidTopicFault :: Encode SNSInvalidTopicFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You do not have permission to publish to the specified Amazon SNS topic.</p>
newtype SNSNoAuthorizationFault = SNSNoAuthorizationFault Types.NoArguments
derive instance newtypeSNSNoAuthorizationFault :: Newtype SNSNoAuthorizationFault _
derive instance repGenericSNSNoAuthorizationFault :: Generic SNSNoAuthorizationFault _
instance showSNSNoAuthorizationFault :: Show SNSNoAuthorizationFault where
  show = genericShow
instance decodeSNSNoAuthorizationFault :: Decode SNSNoAuthorizationFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSNSNoAuthorizationFault :: Encode SNSNoAuthorizationFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An Amazon SNS topic with the specified Amazon Resource Name (ARN) does not exist.</p>
newtype SNSTopicArnNotFoundFault = SNSTopicArnNotFoundFault Types.NoArguments
derive instance newtypeSNSTopicArnNotFoundFault :: Newtype SNSTopicArnNotFoundFault _
derive instance repGenericSNSTopicArnNotFoundFault :: Generic SNSTopicArnNotFoundFault _
instance showSNSTopicArnNotFoundFault :: Show SNSTopicArnNotFoundFault where
  show = genericShow
instance decodeSNSTopicArnNotFoundFault :: Decode SNSTopicArnNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSNSTopicArnNotFoundFault :: Encode SNSTopicArnNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SensitiveString = SensitiveString String
derive instance newtypeSensitiveString :: Newtype SensitiveString _
derive instance repGenericSensitiveString :: Generic SensitiveString _
instance showSensitiveString :: Show SensitiveString where
  show = genericShow
instance decodeSensitiveString :: Decode SensitiveString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSensitiveString :: Encode SensitiveString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a snapshot.</p>
newtype Snapshot = Snapshot 
  { "SnapshotIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotCreateTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (Int)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterCreateTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "MasterUsername" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotType" :: NullOrUndefined.NullOrUndefined (String)
  , "NodeType" :: NullOrUndefined.NullOrUndefined (String)
  , "NumberOfNodes" :: NullOrUndefined.NullOrUndefined (Int)
  , "DBName" :: NullOrUndefined.NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (String)
  , "Encrypted" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (String)
  , "EncryptedWithHSM" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "AccountsWithRestoreAccess" :: NullOrUndefined.NullOrUndefined (AccountsWithRestoreAccessList)
  , "OwnerAccount" :: NullOrUndefined.NullOrUndefined (String)
  , "TotalBackupSizeInMegaBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "ActualIncrementalBackupSizeInMegaBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "BackupProgressInMegaBytes" :: NullOrUndefined.NullOrUndefined (Number)
  , "CurrentBackupRateInMegaBytesPerSecond" :: NullOrUndefined.NullOrUndefined (Number)
  , "EstimatedSecondsToCompletion" :: NullOrUndefined.NullOrUndefined (Number)
  , "ElapsedTimeInSeconds" :: NullOrUndefined.NullOrUndefined (Number)
  , "SourceRegion" :: NullOrUndefined.NullOrUndefined (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  , "RestorableNodeTypes" :: NullOrUndefined.NullOrUndefined (RestorableNodeTypeList)
  , "EnhancedVpcRouting" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeSnapshot :: Newtype Snapshot _
derive instance repGenericSnapshot :: Generic Snapshot _
instance showSnapshot :: Show Snapshot where
  show = genericShow
instance decodeSnapshot :: Decode Snapshot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshot :: Encode Snapshot where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The cluster already has cross-region snapshot copy disabled.</p>
newtype SnapshotCopyAlreadyDisabledFault = SnapshotCopyAlreadyDisabledFault Types.NoArguments
derive instance newtypeSnapshotCopyAlreadyDisabledFault :: Newtype SnapshotCopyAlreadyDisabledFault _
derive instance repGenericSnapshotCopyAlreadyDisabledFault :: Generic SnapshotCopyAlreadyDisabledFault _
instance showSnapshotCopyAlreadyDisabledFault :: Show SnapshotCopyAlreadyDisabledFault where
  show = genericShow
instance decodeSnapshotCopyAlreadyDisabledFault :: Decode SnapshotCopyAlreadyDisabledFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotCopyAlreadyDisabledFault :: Encode SnapshotCopyAlreadyDisabledFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The cluster already has cross-region snapshot copy enabled.</p>
newtype SnapshotCopyAlreadyEnabledFault = SnapshotCopyAlreadyEnabledFault Types.NoArguments
derive instance newtypeSnapshotCopyAlreadyEnabledFault :: Newtype SnapshotCopyAlreadyEnabledFault _
derive instance repGenericSnapshotCopyAlreadyEnabledFault :: Generic SnapshotCopyAlreadyEnabledFault _
instance showSnapshotCopyAlreadyEnabledFault :: Show SnapshotCopyAlreadyEnabledFault where
  show = genericShow
instance decodeSnapshotCopyAlreadyEnabledFault :: Decode SnapshotCopyAlreadyEnabledFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotCopyAlreadyEnabledFault :: Encode SnapshotCopyAlreadyEnabledFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Cross-region snapshot copy was temporarily disabled. Try your request again.</p>
newtype SnapshotCopyDisabledFault = SnapshotCopyDisabledFault Types.NoArguments
derive instance newtypeSnapshotCopyDisabledFault :: Newtype SnapshotCopyDisabledFault _
derive instance repGenericSnapshotCopyDisabledFault :: Generic SnapshotCopyDisabledFault _
instance showSnapshotCopyDisabledFault :: Show SnapshotCopyDisabledFault where
  show = genericShow
instance decodeSnapshotCopyDisabledFault :: Decode SnapshotCopyDisabledFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotCopyDisabledFault :: Encode SnapshotCopyDisabledFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The snapshot copy grant that grants Amazon Redshift permission to encrypt copied snapshots with the specified customer master key (CMK) from AWS KMS in the destination region.</p> <p> For more information about managing snapshot copy grants, go to <a href="http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html">Amazon Redshift Database Encryption</a> in the <i>Amazon Redshift Cluster Management Guide</i>. </p>
newtype SnapshotCopyGrant = SnapshotCopyGrant 
  { "SnapshotCopyGrantName" :: NullOrUndefined.NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (String)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeSnapshotCopyGrant :: Newtype SnapshotCopyGrant _
derive instance repGenericSnapshotCopyGrant :: Generic SnapshotCopyGrant _
instance showSnapshotCopyGrant :: Show SnapshotCopyGrant where
  show = genericShow
instance decodeSnapshotCopyGrant :: Decode SnapshotCopyGrant where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotCopyGrant :: Encode SnapshotCopyGrant where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The snapshot copy grant can't be created because a grant with the same name already exists.</p>
newtype SnapshotCopyGrantAlreadyExistsFault = SnapshotCopyGrantAlreadyExistsFault Types.NoArguments
derive instance newtypeSnapshotCopyGrantAlreadyExistsFault :: Newtype SnapshotCopyGrantAlreadyExistsFault _
derive instance repGenericSnapshotCopyGrantAlreadyExistsFault :: Generic SnapshotCopyGrantAlreadyExistsFault _
instance showSnapshotCopyGrantAlreadyExistsFault :: Show SnapshotCopyGrantAlreadyExistsFault where
  show = genericShow
instance decodeSnapshotCopyGrantAlreadyExistsFault :: Decode SnapshotCopyGrantAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotCopyGrantAlreadyExistsFault :: Encode SnapshotCopyGrantAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnapshotCopyGrantList = SnapshotCopyGrantList (Array SnapshotCopyGrant)
derive instance newtypeSnapshotCopyGrantList :: Newtype SnapshotCopyGrantList _
derive instance repGenericSnapshotCopyGrantList :: Generic SnapshotCopyGrantList _
instance showSnapshotCopyGrantList :: Show SnapshotCopyGrantList where
  show = genericShow
instance decodeSnapshotCopyGrantList :: Decode SnapshotCopyGrantList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotCopyGrantList :: Encode SnapshotCopyGrantList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype SnapshotCopyGrantMessage = SnapshotCopyGrantMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotCopyGrants" :: NullOrUndefined.NullOrUndefined (SnapshotCopyGrantList)
  }
derive instance newtypeSnapshotCopyGrantMessage :: Newtype SnapshotCopyGrantMessage _
derive instance repGenericSnapshotCopyGrantMessage :: Generic SnapshotCopyGrantMessage _
instance showSnapshotCopyGrantMessage :: Show SnapshotCopyGrantMessage where
  show = genericShow
instance decodeSnapshotCopyGrantMessage :: Decode SnapshotCopyGrantMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotCopyGrantMessage :: Encode SnapshotCopyGrantMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified snapshot copy grant can't be found. Make sure that the name is typed correctly and that the grant exists in the destination region.</p>
newtype SnapshotCopyGrantNotFoundFault = SnapshotCopyGrantNotFoundFault Types.NoArguments
derive instance newtypeSnapshotCopyGrantNotFoundFault :: Newtype SnapshotCopyGrantNotFoundFault _
derive instance repGenericSnapshotCopyGrantNotFoundFault :: Generic SnapshotCopyGrantNotFoundFault _
instance showSnapshotCopyGrantNotFoundFault :: Show SnapshotCopyGrantNotFoundFault where
  show = genericShow
instance decodeSnapshotCopyGrantNotFoundFault :: Decode SnapshotCopyGrantNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotCopyGrantNotFoundFault :: Encode SnapshotCopyGrantNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The AWS account has exceeded the maximum number of snapshot copy grants in this region.</p>
newtype SnapshotCopyGrantQuotaExceededFault = SnapshotCopyGrantQuotaExceededFault Types.NoArguments
derive instance newtypeSnapshotCopyGrantQuotaExceededFault :: Newtype SnapshotCopyGrantQuotaExceededFault _
derive instance repGenericSnapshotCopyGrantQuotaExceededFault :: Generic SnapshotCopyGrantQuotaExceededFault _
instance showSnapshotCopyGrantQuotaExceededFault :: Show SnapshotCopyGrantQuotaExceededFault where
  show = genericShow
instance decodeSnapshotCopyGrantQuotaExceededFault :: Decode SnapshotCopyGrantQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotCopyGrantQuotaExceededFault :: Encode SnapshotCopyGrantQuotaExceededFault where
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


-- | <p>Contains the output from the <a>DescribeClusterSnapshots</a> action. </p>
newtype SnapshotMessage = SnapshotMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "Snapshots" :: NullOrUndefined.NullOrUndefined (SnapshotList)
  }
derive instance newtypeSnapshotMessage :: Newtype SnapshotMessage _
derive instance repGenericSnapshotMessage :: Generic SnapshotMessage _
instance showSnapshotMessage :: Show SnapshotMessage where
  show = genericShow
instance decodeSnapshotMessage :: Decode SnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotMessage :: Encode SnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SourceIdsList = SourceIdsList (Array String)
derive instance newtypeSourceIdsList :: Newtype SourceIdsList _
derive instance repGenericSourceIdsList :: Generic SourceIdsList _
instance showSourceIdsList :: Show SourceIdsList where
  show = genericShow
instance decodeSourceIdsList :: Decode SourceIdsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceIdsList :: Encode SourceIdsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified Amazon Redshift event source could not be found.</p>
newtype SourceNotFoundFault = SourceNotFoundFault Types.NoArguments
derive instance newtypeSourceNotFoundFault :: Newtype SourceNotFoundFault _
derive instance repGenericSourceNotFoundFault :: Generic SourceNotFoundFault _
instance showSourceNotFoundFault :: Show SourceNotFoundFault where
  show = genericShow
instance decodeSourceNotFoundFault :: Decode SourceNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceNotFoundFault :: Encode SourceNotFoundFault where
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


-- | <p>Describes a subnet.</p>
newtype Subnet = Subnet 
  { "SubnetIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SubnetAvailabilityZone" :: NullOrUndefined.NullOrUndefined (AvailabilityZone)
  , "SubnetStatus" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSubnet :: Newtype Subnet _
derive instance repGenericSubnet :: Generic Subnet _
instance showSubnet :: Show Subnet where
  show = genericShow
instance decodeSubnet :: Decode Subnet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnet :: Encode Subnet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A specified subnet is already in use by another cluster.</p>
newtype SubnetAlreadyInUse = SubnetAlreadyInUse Types.NoArguments
derive instance newtypeSubnetAlreadyInUse :: Newtype SubnetAlreadyInUse _
derive instance repGenericSubnetAlreadyInUse :: Generic SubnetAlreadyInUse _
instance showSubnetAlreadyInUse :: Show SubnetAlreadyInUse where
  show = genericShow
instance decodeSubnetAlreadyInUse :: Decode SubnetAlreadyInUse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetAlreadyInUse :: Encode SubnetAlreadyInUse where
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


newtype SubnetList = SubnetList (Array Subnet)
derive instance newtypeSubnetList :: Newtype SubnetList _
derive instance repGenericSubnetList :: Generic SubnetList _
instance showSubnetList :: Show SubnetList where
  show = genericShow
instance decodeSubnetList :: Decode SubnetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetList :: Encode SubnetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is already an existing event notification subscription with the specified name.</p>
newtype SubscriptionAlreadyExistFault = SubscriptionAlreadyExistFault Types.NoArguments
derive instance newtypeSubscriptionAlreadyExistFault :: Newtype SubscriptionAlreadyExistFault _
derive instance repGenericSubscriptionAlreadyExistFault :: Generic SubscriptionAlreadyExistFault _
instance showSubscriptionAlreadyExistFault :: Show SubscriptionAlreadyExistFault where
  show = genericShow
instance decodeSubscriptionAlreadyExistFault :: Decode SubscriptionAlreadyExistFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionAlreadyExistFault :: Encode SubscriptionAlreadyExistFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value specified for the event category was not one of the allowed values, or it specified a category that does not apply to the specified source type. The allowed values are Configuration, Management, Monitoring, and Security.</p>
newtype SubscriptionCategoryNotFoundFault = SubscriptionCategoryNotFoundFault Types.NoArguments
derive instance newtypeSubscriptionCategoryNotFoundFault :: Newtype SubscriptionCategoryNotFoundFault _
derive instance repGenericSubscriptionCategoryNotFoundFault :: Generic SubscriptionCategoryNotFoundFault _
instance showSubscriptionCategoryNotFoundFault :: Show SubscriptionCategoryNotFoundFault where
  show = genericShow
instance decodeSubscriptionCategoryNotFoundFault :: Decode SubscriptionCategoryNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionCategoryNotFoundFault :: Encode SubscriptionCategoryNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An Amazon Redshift event with the specified event ID does not exist.</p>
newtype SubscriptionEventIdNotFoundFault = SubscriptionEventIdNotFoundFault Types.NoArguments
derive instance newtypeSubscriptionEventIdNotFoundFault :: Newtype SubscriptionEventIdNotFoundFault _
derive instance repGenericSubscriptionEventIdNotFoundFault :: Generic SubscriptionEventIdNotFoundFault _
instance showSubscriptionEventIdNotFoundFault :: Show SubscriptionEventIdNotFoundFault where
  show = genericShow
instance decodeSubscriptionEventIdNotFoundFault :: Decode SubscriptionEventIdNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionEventIdNotFoundFault :: Encode SubscriptionEventIdNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An Amazon Redshift event notification subscription with the specified name does not exist.</p>
newtype SubscriptionNotFoundFault = SubscriptionNotFoundFault Types.NoArguments
derive instance newtypeSubscriptionNotFoundFault :: Newtype SubscriptionNotFoundFault _
derive instance repGenericSubscriptionNotFoundFault :: Generic SubscriptionNotFoundFault _
instance showSubscriptionNotFoundFault :: Show SubscriptionNotFoundFault where
  show = genericShow
instance decodeSubscriptionNotFoundFault :: Decode SubscriptionNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionNotFoundFault :: Encode SubscriptionNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value specified for the event severity was not one of the allowed values, or it specified a severity that does not apply to the specified source type. The allowed values are ERROR and INFO.</p>
newtype SubscriptionSeverityNotFoundFault = SubscriptionSeverityNotFoundFault Types.NoArguments
derive instance newtypeSubscriptionSeverityNotFoundFault :: Newtype SubscriptionSeverityNotFoundFault _
derive instance repGenericSubscriptionSeverityNotFoundFault :: Generic SubscriptionSeverityNotFoundFault _
instance showSubscriptionSeverityNotFoundFault :: Show SubscriptionSeverityNotFoundFault where
  show = genericShow
instance decodeSubscriptionSeverityNotFoundFault :: Decode SubscriptionSeverityNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionSeverityNotFoundFault :: Encode SubscriptionSeverityNotFoundFault where
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


-- | <p>The specified <code>TableRestoreRequestId</code> value was not found.</p>
newtype TableRestoreNotFoundFault = TableRestoreNotFoundFault Types.NoArguments
derive instance newtypeTableRestoreNotFoundFault :: Newtype TableRestoreNotFoundFault _
derive instance repGenericTableRestoreNotFoundFault :: Generic TableRestoreNotFoundFault _
instance showTableRestoreNotFoundFault :: Show TableRestoreNotFoundFault where
  show = genericShow
instance decodeTableRestoreNotFoundFault :: Decode TableRestoreNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableRestoreNotFoundFault :: Encode TableRestoreNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the status of a <a>RestoreTableFromClusterSnapshot</a> operation.</p>
newtype TableRestoreStatus = TableRestoreStatus 
  { "TableRestoreRequestId" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (TableRestoreStatusType)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "ProgressInMegaBytes" :: NullOrUndefined.NullOrUndefined (LongOptional)
  , "TotalDataInMegaBytes" :: NullOrUndefined.NullOrUndefined (LongOptional)
  , "ClusterIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceDatabaseName" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceSchemaName" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceTableName" :: NullOrUndefined.NullOrUndefined (String)
  , "TargetDatabaseName" :: NullOrUndefined.NullOrUndefined (String)
  , "TargetSchemaName" :: NullOrUndefined.NullOrUndefined (String)
  , "NewTableName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTableRestoreStatus :: Newtype TableRestoreStatus _
derive instance repGenericTableRestoreStatus :: Generic TableRestoreStatus _
instance showTableRestoreStatus :: Show TableRestoreStatus where
  show = genericShow
instance decodeTableRestoreStatus :: Decode TableRestoreStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableRestoreStatus :: Encode TableRestoreStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TableRestoreStatusList = TableRestoreStatusList (Array TableRestoreStatus)
derive instance newtypeTableRestoreStatusList :: Newtype TableRestoreStatusList _
derive instance repGenericTableRestoreStatusList :: Generic TableRestoreStatusList _
instance showTableRestoreStatusList :: Show TableRestoreStatusList where
  show = genericShow
instance decodeTableRestoreStatusList :: Decode TableRestoreStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableRestoreStatusList :: Encode TableRestoreStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype TableRestoreStatusMessage = TableRestoreStatusMessage 
  { "TableRestoreStatusDetails" :: NullOrUndefined.NullOrUndefined (TableRestoreStatusList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTableRestoreStatusMessage :: Newtype TableRestoreStatusMessage _
derive instance repGenericTableRestoreStatusMessage :: Generic TableRestoreStatusMessage _
instance showTableRestoreStatusMessage :: Show TableRestoreStatusMessage where
  show = genericShow
instance decodeTableRestoreStatusMessage :: Decode TableRestoreStatusMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableRestoreStatusMessage :: Encode TableRestoreStatusMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TableRestoreStatusType = TableRestoreStatusType String
derive instance newtypeTableRestoreStatusType :: Newtype TableRestoreStatusType _
derive instance repGenericTableRestoreStatusType :: Generic TableRestoreStatusType _
instance showTableRestoreStatusType :: Show TableRestoreStatusType where
  show = genericShow
instance decodeTableRestoreStatusType :: Decode TableRestoreStatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableRestoreStatusType :: Encode TableRestoreStatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A tag consisting of a name/value pair for a resource.</p>
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


newtype TagKeyList = TagKeyList (Array String)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request exceeds the limit of 10 tags for the resource.</p>
newtype TagLimitExceededFault = TagLimitExceededFault Types.NoArguments
derive instance newtypeTagLimitExceededFault :: Newtype TagLimitExceededFault _
derive instance repGenericTagLimitExceededFault :: Generic TagLimitExceededFault _
instance showTagLimitExceededFault :: Show TagLimitExceededFault where
  show = genericShow
instance decodeTagLimitExceededFault :: Decode TagLimitExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagLimitExceededFault :: Encode TagLimitExceededFault where
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


newtype TagValueList = TagValueList (Array String)
derive instance newtypeTagValueList :: Newtype TagValueList _
derive instance repGenericTagValueList :: Generic TagValueList _
instance showTagValueList :: Show TagValueList where
  show = genericShow
instance decodeTagValueList :: Decode TagValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValueList :: Encode TagValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A tag and its associated resource.</p>
newtype TaggedResource = TaggedResource 
  { "Tag" :: NullOrUndefined.NullOrUndefined (Tag)
  , "ResourceName" :: NullOrUndefined.NullOrUndefined (String)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTaggedResource :: Newtype TaggedResource _
derive instance repGenericTaggedResource :: Generic TaggedResource _
instance showTaggedResource :: Show TaggedResource where
  show = genericShow
instance decodeTaggedResource :: Decode TaggedResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaggedResource :: Encode TaggedResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaggedResourceList = TaggedResourceList (Array TaggedResource)
derive instance newtypeTaggedResourceList :: Newtype TaggedResourceList _
derive instance repGenericTaggedResourceList :: Generic TaggedResourceList _
instance showTaggedResourceList :: Show TaggedResourceList where
  show = genericShow
instance decodeTaggedResourceList :: Decode TaggedResourceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaggedResourceList :: Encode TaggedResourceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype TaggedResourceListMessage = TaggedResourceListMessage 
  { "TaggedResources" :: NullOrUndefined.NullOrUndefined (TaggedResourceList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTaggedResourceListMessage :: Newtype TaggedResourceListMessage _
derive instance repGenericTaggedResourceListMessage :: Generic TaggedResourceListMessage _
instance showTaggedResourceListMessage :: Show TaggedResourceListMessage where
  show = genericShow
instance decodeTaggedResourceListMessage :: Decode TaggedResourceListMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaggedResourceListMessage :: Encode TaggedResourceListMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Your account is not authorized to perform the requested operation.</p>
newtype UnauthorizedOperation = UnauthorizedOperation Types.NoArguments
derive instance newtypeUnauthorizedOperation :: Newtype UnauthorizedOperation _
derive instance repGenericUnauthorizedOperation :: Generic UnauthorizedOperation _
instance showUnauthorizedOperation :: Show UnauthorizedOperation where
  show = genericShow
instance decodeUnauthorizedOperation :: Decode UnauthorizedOperation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnauthorizedOperation :: Encode UnauthorizedOperation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified region is incorrect or does not exist.</p>
newtype UnknownSnapshotCopyRegionFault = UnknownSnapshotCopyRegionFault Types.NoArguments
derive instance newtypeUnknownSnapshotCopyRegionFault :: Newtype UnknownSnapshotCopyRegionFault _
derive instance repGenericUnknownSnapshotCopyRegionFault :: Generic UnknownSnapshotCopyRegionFault _
instance showUnknownSnapshotCopyRegionFault :: Show UnknownSnapshotCopyRegionFault where
  show = genericShow
instance decodeUnknownSnapshotCopyRegionFault :: Decode UnknownSnapshotCopyRegionFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnknownSnapshotCopyRegionFault :: Encode UnknownSnapshotCopyRegionFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested operation isn't supported.</p>
newtype UnsupportedOperationFault = UnsupportedOperationFault Types.NoArguments
derive instance newtypeUnsupportedOperationFault :: Newtype UnsupportedOperationFault _
derive instance repGenericUnsupportedOperationFault :: Generic UnsupportedOperationFault _
instance showUnsupportedOperationFault :: Show UnsupportedOperationFault where
  show = genericShow
instance decodeUnsupportedOperationFault :: Decode UnsupportedOperationFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedOperationFault :: Encode UnsupportedOperationFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request option was specified that is not supported.</p>
newtype UnsupportedOptionFault = UnsupportedOptionFault Types.NoArguments
derive instance newtypeUnsupportedOptionFault :: Newtype UnsupportedOptionFault _
derive instance repGenericUnsupportedOptionFault :: Generic UnsupportedOptionFault _
instance showUnsupportedOptionFault :: Show UnsupportedOptionFault where
  show = genericShow
instance decodeUnsupportedOptionFault :: Decode UnsupportedOptionFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedOptionFault :: Encode UnsupportedOptionFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VpcSecurityGroupIdList = VpcSecurityGroupIdList (Array String)
derive instance newtypeVpcSecurityGroupIdList :: Newtype VpcSecurityGroupIdList _
derive instance repGenericVpcSecurityGroupIdList :: Generic VpcSecurityGroupIdList _
instance showVpcSecurityGroupIdList :: Show VpcSecurityGroupIdList where
  show = genericShow
instance decodeVpcSecurityGroupIdList :: Decode VpcSecurityGroupIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcSecurityGroupIdList :: Encode VpcSecurityGroupIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the members of a VPC security group.</p>
newtype VpcSecurityGroupMembership = VpcSecurityGroupMembership 
  { "VpcSecurityGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeVpcSecurityGroupMembership :: Newtype VpcSecurityGroupMembership _
derive instance repGenericVpcSecurityGroupMembership :: Generic VpcSecurityGroupMembership _
instance showVpcSecurityGroupMembership :: Show VpcSecurityGroupMembership where
  show = genericShow
instance decodeVpcSecurityGroupMembership :: Decode VpcSecurityGroupMembership where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcSecurityGroupMembership :: Encode VpcSecurityGroupMembership where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VpcSecurityGroupMembershipList = VpcSecurityGroupMembershipList (Array VpcSecurityGroupMembership)
derive instance newtypeVpcSecurityGroupMembershipList :: Newtype VpcSecurityGroupMembershipList _
derive instance repGenericVpcSecurityGroupMembershipList :: Generic VpcSecurityGroupMembershipList _
instance showVpcSecurityGroupMembershipList :: Show VpcSecurityGroupMembershipList where
  show = genericShow
instance decodeVpcSecurityGroupMembershipList :: Decode VpcSecurityGroupMembershipList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcSecurityGroupMembershipList :: Encode VpcSecurityGroupMembershipList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
