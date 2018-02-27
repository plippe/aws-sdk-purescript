## Module AWS.DAX

<p>DAX is a managed caching service engineered for Amazon DynamoDB. DAX dramatically speeds up database reads by caching frequently-accessed data from DynamoDB, so applications can access that data with sub-millisecond latency. You can create a DAX cluster easily, using the AWS Management Console. With a few simple modifications to your code, your application can begin taking advantage of the DAX cluster and realize significant improvements in read performance.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createCluster`

``` purescript
createCluster :: forall eff. CreateClusterRequest -> Aff (err :: RequestError | eff) CreateClusterResponse
```

<p>Creates a DAX cluster. All nodes in the cluster run the same DAX caching software.</p>

#### `createParameterGroup`

``` purescript
createParameterGroup :: forall eff. CreateParameterGroupRequest -> Aff (err :: RequestError | eff) CreateParameterGroupResponse
```

<p>Creates a new parameter group. A parameter group is a collection of parameters that you apply to all of the nodes in a DAX cluster.</p>

#### `createSubnetGroup`

``` purescript
createSubnetGroup :: forall eff. CreateSubnetGroupRequest -> Aff (err :: RequestError | eff) CreateSubnetGroupResponse
```

<p>Creates a new subnet group.</p>

#### `decreaseReplicationFactor`

``` purescript
decreaseReplicationFactor :: forall eff. DecreaseReplicationFactorRequest -> Aff (err :: RequestError | eff) DecreaseReplicationFactorResponse
```

<p>Removes one or more nodes from a DAX cluster.</p> <note> <p>You cannot use <code>DecreaseReplicationFactor</code> to remove the last node in a DAX cluster. If you need to do this, use <code>DeleteCluster</code> instead.</p> </note>

#### `deleteCluster`

``` purescript
deleteCluster :: forall eff. DeleteClusterRequest -> Aff (err :: RequestError | eff) DeleteClusterResponse
```

<p>Deletes a previously provisioned DAX cluster. <i>DeleteCluster</i> deletes all associated nodes, node endpoints and the DAX cluster itself. When you receive a successful response from this action, DAX immediately begins deleting the cluster; you cannot cancel or revert this action.</p>

#### `deleteParameterGroup`

``` purescript
deleteParameterGroup :: forall eff. DeleteParameterGroupRequest -> Aff (err :: RequestError | eff) DeleteParameterGroupResponse
```

<p>Deletes the specified parameter group. You cannot delete a parameter group if it is associated with any DAX clusters.</p>

#### `deleteSubnetGroup`

``` purescript
deleteSubnetGroup :: forall eff. DeleteSubnetGroupRequest -> Aff (err :: RequestError | eff) DeleteSubnetGroupResponse
```

<p>Deletes a subnet group.</p> <note> <p>You cannot delete a subnet group if it is associated with any DAX clusters.</p> </note>

#### `describeClusters`

``` purescript
describeClusters :: forall eff. DescribeClustersRequest -> Aff (err :: RequestError | eff) DescribeClustersResponse
```

<p>Returns information about all provisioned DAX clusters if no cluster identifier is specified, or about a specific DAX cluster if a cluster identifier is supplied.</p> <p>If the cluster is in the CREATING state, only cluster level information will be displayed until all of the nodes are successfully provisioned.</p> <p>If the cluster is in the DELETING state, only cluster level information will be displayed.</p> <p>If nodes are currently being added to the DAX cluster, node endpoint information and creation time for the additional nodes will not be displayed until they are completely provisioned. When the DAX cluster state is <i>available</i>, the cluster is ready for use.</p> <p>If nodes are currently being removed from the DAX cluster, no endpoint information for the removed nodes is displayed.</p>

#### `describeDefaultParameters`

``` purescript
describeDefaultParameters :: forall eff. DescribeDefaultParametersRequest -> Aff (err :: RequestError | eff) DescribeDefaultParametersResponse
```

<p>Returns the default system parameter information for the DAX caching software.</p>

#### `describeEvents`

``` purescript
describeEvents :: forall eff. DescribeEventsRequest -> Aff (err :: RequestError | eff) DescribeEventsResponse
```

<p>Returns events related to DAX clusters and parameter groups. You can obtain events specific to a particular DAX cluster or parameter group by providing the name as a parameter.</p> <p>By default, only the events occurring within the last hour are returned; however, you can retrieve up to 14 days' worth of events if necessary.</p>

#### `describeParameterGroups`

``` purescript
describeParameterGroups :: forall eff. DescribeParameterGroupsRequest -> Aff (err :: RequestError | eff) DescribeParameterGroupsResponse
```

<p>Returns a list of parameter group descriptions. If a parameter group name is specified, the list will contain only the descriptions for that group.</p>

#### `describeParameters`

``` purescript
describeParameters :: forall eff. DescribeParametersRequest -> Aff (err :: RequestError | eff) DescribeParametersResponse
```

<p>Returns the detailed parameter list for a particular parameter group.</p>

#### `describeSubnetGroups`

``` purescript
describeSubnetGroups :: forall eff. DescribeSubnetGroupsRequest -> Aff (err :: RequestError | eff) DescribeSubnetGroupsResponse
```

<p>Returns a list of subnet group descriptions. If a subnet group name is specified, the list will contain only the description of that group.</p>

#### `increaseReplicationFactor`

``` purescript
increaseReplicationFactor :: forall eff. IncreaseReplicationFactorRequest -> Aff (err :: RequestError | eff) IncreaseReplicationFactorResponse
```

<p>Adds one or more nodes to a DAX cluster.</p>

#### `listTags`

``` purescript
listTags :: forall eff. ListTagsRequest -> Aff (err :: RequestError | eff) ListTagsResponse
```

<p>List all of the tags for a DAX cluster. You can call <code>ListTags</code> up to 10 times per second, per account.</p>

#### `rebootNode`

``` purescript
rebootNode :: forall eff. RebootNodeRequest -> Aff (err :: RequestError | eff) RebootNodeResponse
```

<p>Reboots a single node of a DAX cluster. The reboot action takes place as soon as possible. During the reboot, the node status is set to REBOOTING.</p>

#### `tagResource`

``` purescript
tagResource :: forall eff. TagResourceRequest -> Aff (err :: RequestError | eff) TagResourceResponse
```

<p>Associates a set of tags with a DAX resource. You can call <code>TagResource</code> up to 5 times per second, per account. </p>

#### `untagResource`

``` purescript
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: RequestError | eff) UntagResourceResponse
```

<p>Removes the association of tags from a DAX resource. You can call <code>UntagResource</code> up to 5 times per second, per account. </p>

#### `updateCluster`

``` purescript
updateCluster :: forall eff. UpdateClusterRequest -> Aff (err :: RequestError | eff) UpdateClusterResponse
```

<p>Modifies the settings for a DAX cluster. You can use this action to change one or more cluster configuration parameters by specifying the parameters and the new values.</p>

#### `updateParameterGroup`

``` purescript
updateParameterGroup :: forall eff. UpdateParameterGroupRequest -> Aff (err :: RequestError | eff) UpdateParameterGroupResponse
```

<p>Modifies the parameters of a parameter group. You can modify up to 20 parameters in a single request by submitting a list parameter name and value pairs.</p>

#### `updateSubnetGroup`

``` purescript
updateSubnetGroup :: forall eff. UpdateSubnetGroupRequest -> Aff (err :: RequestError | eff) UpdateSubnetGroupResponse
```

<p>Modifies an existing subnet group.</p>

#### `AvailabilityZoneList`

``` purescript
newtype AvailabilityZoneList
  = AvailabilityZoneList (Array String)
```

##### Instances
``` purescript
Newtype AvailabilityZoneList _
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

#### `ChangeType`

``` purescript
newtype ChangeType
  = ChangeType String
```

##### Instances
``` purescript
Newtype ChangeType _
```

#### `Cluster`

``` purescript
newtype Cluster
  = Cluster { "ClusterName" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "ClusterArn" :: NullOrUndefined (String), "TotalNodes" :: NullOrUndefined (IntegerOptional), "ActiveNodes" :: NullOrUndefined (IntegerOptional), "NodeType" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "ClusterDiscoveryEndpoint" :: NullOrUndefined (Endpoint), "NodeIdsToRemove" :: NullOrUndefined (NodeIdentifierList), "Nodes" :: NullOrUndefined (NodeList), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "NotificationConfiguration" :: NullOrUndefined (NotificationConfiguration), "SubnetGroup" :: NullOrUndefined (String), "SecurityGroups" :: NullOrUndefined (SecurityGroupMembershipList), "IamRoleArn" :: NullOrUndefined (String), "ParameterGroup" :: NullOrUndefined (ParameterGroupStatus) }
```

<p>Contains all of the attributes of a specific DAX cluster.</p>

##### Instances
``` purescript
Newtype Cluster _
```

#### `ClusterAlreadyExistsFault`

``` purescript
newtype ClusterAlreadyExistsFault
  = ClusterAlreadyExistsFault {  }
```

<p>You already have a DAX cluster with the given identifier.</p>

##### Instances
``` purescript
Newtype ClusterAlreadyExistsFault _
```

#### `ClusterList`

``` purescript
newtype ClusterList
  = ClusterList (Array Cluster)
```

##### Instances
``` purescript
Newtype ClusterList _
```

#### `ClusterNameList`

``` purescript
newtype ClusterNameList
  = ClusterNameList (Array String)
```

##### Instances
``` purescript
Newtype ClusterNameList _
```

#### `ClusterNotFoundFault`

``` purescript
newtype ClusterNotFoundFault
  = ClusterNotFoundFault {  }
```

<p>The requested cluster ID does not refer to an existing DAX cluster.</p>

##### Instances
``` purescript
Newtype ClusterNotFoundFault _
```

#### `ClusterQuotaForCustomerExceededFault`

``` purescript
newtype ClusterQuotaForCustomerExceededFault
  = ClusterQuotaForCustomerExceededFault {  }
```

<p>You have attempted to exceed the maximum number of DAX clusters for your AWS account.</p>

##### Instances
``` purescript
Newtype ClusterQuotaForCustomerExceededFault _
```

#### `CreateClusterRequest`

``` purescript
newtype CreateClusterRequest
  = CreateClusterRequest { "ClusterName" :: String, "NodeType" :: String, "Description" :: NullOrUndefined (String), "ReplicationFactor" :: Int, "AvailabilityZones" :: NullOrUndefined (AvailabilityZoneList), "SubnetGroupName" :: NullOrUndefined (String), "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdentifierList), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "NotificationTopicArn" :: NullOrUndefined (String), "IamRoleArn" :: String, "ParameterGroupName" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
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

#### `CreateParameterGroupRequest`

``` purescript
newtype CreateParameterGroupRequest
  = CreateParameterGroupRequest { "ParameterGroupName" :: String, "Description" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateParameterGroupRequest _
```

#### `CreateParameterGroupResponse`

``` purescript
newtype CreateParameterGroupResponse
  = CreateParameterGroupResponse { "ParameterGroup" :: NullOrUndefined (ParameterGroup) }
```

##### Instances
``` purescript
Newtype CreateParameterGroupResponse _
```

#### `CreateSubnetGroupRequest`

``` purescript
newtype CreateSubnetGroupRequest
  = CreateSubnetGroupRequest { "SubnetGroupName" :: String, "Description" :: NullOrUndefined (String), "SubnetIds" :: SubnetIdentifierList }
```

##### Instances
``` purescript
Newtype CreateSubnetGroupRequest _
```

#### `CreateSubnetGroupResponse`

``` purescript
newtype CreateSubnetGroupResponse
  = CreateSubnetGroupResponse { "SubnetGroup" :: NullOrUndefined (SubnetGroup) }
```

##### Instances
``` purescript
Newtype CreateSubnetGroupResponse _
```

#### `DecreaseReplicationFactorRequest`

``` purescript
newtype DecreaseReplicationFactorRequest
  = DecreaseReplicationFactorRequest { "ClusterName" :: String, "NewReplicationFactor" :: Int, "AvailabilityZones" :: NullOrUndefined (AvailabilityZoneList), "NodeIdsToRemove" :: NullOrUndefined (NodeIdentifierList) }
```

##### Instances
``` purescript
Newtype DecreaseReplicationFactorRequest _
```

#### `DecreaseReplicationFactorResponse`

``` purescript
newtype DecreaseReplicationFactorResponse
  = DecreaseReplicationFactorResponse { "Cluster" :: NullOrUndefined (Cluster) }
```

##### Instances
``` purescript
Newtype DecreaseReplicationFactorResponse _
```

#### `DeleteClusterRequest`

``` purescript
newtype DeleteClusterRequest
  = DeleteClusterRequest { "ClusterName" :: String }
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

#### `DeleteParameterGroupRequest`

``` purescript
newtype DeleteParameterGroupRequest
  = DeleteParameterGroupRequest { "ParameterGroupName" :: String }
```

##### Instances
``` purescript
Newtype DeleteParameterGroupRequest _
```

#### `DeleteParameterGroupResponse`

``` purescript
newtype DeleteParameterGroupResponse
  = DeleteParameterGroupResponse { "DeletionMessage" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DeleteParameterGroupResponse _
```

#### `DeleteSubnetGroupRequest`

``` purescript
newtype DeleteSubnetGroupRequest
  = DeleteSubnetGroupRequest { "SubnetGroupName" :: String }
```

##### Instances
``` purescript
Newtype DeleteSubnetGroupRequest _
```

#### `DeleteSubnetGroupResponse`

``` purescript
newtype DeleteSubnetGroupResponse
  = DeleteSubnetGroupResponse { "DeletionMessage" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DeleteSubnetGroupResponse _
```

#### `DescribeClustersRequest`

``` purescript
newtype DescribeClustersRequest
  = DescribeClustersRequest { "ClusterNames" :: NullOrUndefined (ClusterNameList), "MaxResults" :: NullOrUndefined (IntegerOptional), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeClustersRequest _
```

#### `DescribeClustersResponse`

``` purescript
newtype DescribeClustersResponse
  = DescribeClustersResponse { "NextToken" :: NullOrUndefined (String), "Clusters" :: NullOrUndefined (ClusterList) }
```

##### Instances
``` purescript
Newtype DescribeClustersResponse _
```

#### `DescribeDefaultParametersRequest`

``` purescript
newtype DescribeDefaultParametersRequest
  = DescribeDefaultParametersRequest { "MaxResults" :: NullOrUndefined (IntegerOptional), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeDefaultParametersRequest _
```

#### `DescribeDefaultParametersResponse`

``` purescript
newtype DescribeDefaultParametersResponse
  = DescribeDefaultParametersResponse { "NextToken" :: NullOrUndefined (String), "Parameters" :: NullOrUndefined (ParameterList) }
```

##### Instances
``` purescript
Newtype DescribeDefaultParametersResponse _
```

#### `DescribeEventsRequest`

``` purescript
newtype DescribeEventsRequest
  = DescribeEventsRequest { "SourceName" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (SourceType), "StartTime" :: NullOrUndefined (TStamp), "EndTime" :: NullOrUndefined (TStamp), "Duration" :: NullOrUndefined (IntegerOptional), "MaxResults" :: NullOrUndefined (IntegerOptional), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeEventsRequest _
```

#### `DescribeEventsResponse`

``` purescript
newtype DescribeEventsResponse
  = DescribeEventsResponse { "NextToken" :: NullOrUndefined (String), "Events" :: NullOrUndefined (EventList) }
```

##### Instances
``` purescript
Newtype DescribeEventsResponse _
```

#### `DescribeParameterGroupsRequest`

``` purescript
newtype DescribeParameterGroupsRequest
  = DescribeParameterGroupsRequest { "ParameterGroupNames" :: NullOrUndefined (ParameterGroupNameList), "MaxResults" :: NullOrUndefined (IntegerOptional), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeParameterGroupsRequest _
```

#### `DescribeParameterGroupsResponse`

``` purescript
newtype DescribeParameterGroupsResponse
  = DescribeParameterGroupsResponse { "NextToken" :: NullOrUndefined (String), "ParameterGroups" :: NullOrUndefined (ParameterGroupList) }
```

##### Instances
``` purescript
Newtype DescribeParameterGroupsResponse _
```

#### `DescribeParametersRequest`

``` purescript
newtype DescribeParametersRequest
  = DescribeParametersRequest { "ParameterGroupName" :: String, "Source" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (IntegerOptional), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeParametersRequest _
```

#### `DescribeParametersResponse`

``` purescript
newtype DescribeParametersResponse
  = DescribeParametersResponse { "NextToken" :: NullOrUndefined (String), "Parameters" :: NullOrUndefined (ParameterList) }
```

##### Instances
``` purescript
Newtype DescribeParametersResponse _
```

#### `DescribeSubnetGroupsRequest`

``` purescript
newtype DescribeSubnetGroupsRequest
  = DescribeSubnetGroupsRequest { "SubnetGroupNames" :: NullOrUndefined (SubnetGroupNameList), "MaxResults" :: NullOrUndefined (IntegerOptional), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeSubnetGroupsRequest _
```

#### `DescribeSubnetGroupsResponse`

``` purescript
newtype DescribeSubnetGroupsResponse
  = DescribeSubnetGroupsResponse { "NextToken" :: NullOrUndefined (String), "SubnetGroups" :: NullOrUndefined (SubnetGroupList) }
```

##### Instances
``` purescript
Newtype DescribeSubnetGroupsResponse _
```

#### `Endpoint`

``` purescript
newtype Endpoint
  = Endpoint { "Address" :: NullOrUndefined (String), "Port" :: NullOrUndefined (Int) }
```

<p>Represents the information required for client programs to connect to the configuration endpoint for a DAX cluster, or to an individual node within the cluster.</p>

##### Instances
``` purescript
Newtype Endpoint _
```

#### `Event`

``` purescript
newtype Event
  = Event { "SourceName" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (SourceType), "Message" :: NullOrUndefined (String), "Date" :: NullOrUndefined (TStamp) }
```

<p>Represents a single occurrence of something interesting within the system. Some examples of events are creating a DAX cluster, adding or removing a node, or rebooting a node.</p>

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

#### `IncreaseReplicationFactorRequest`

``` purescript
newtype IncreaseReplicationFactorRequest
  = IncreaseReplicationFactorRequest { "ClusterName" :: String, "NewReplicationFactor" :: Int, "AvailabilityZones" :: NullOrUndefined (AvailabilityZoneList) }
```

##### Instances
``` purescript
Newtype IncreaseReplicationFactorRequest _
```

#### `IncreaseReplicationFactorResponse`

``` purescript
newtype IncreaseReplicationFactorResponse
  = IncreaseReplicationFactorResponse { "Cluster" :: NullOrUndefined (Cluster) }
```

##### Instances
``` purescript
Newtype IncreaseReplicationFactorResponse _
```

#### `InsufficientClusterCapacityFault`

``` purescript
newtype InsufficientClusterCapacityFault
  = InsufficientClusterCapacityFault {  }
```

<p>There are not enough system resources to create the cluster you requested (or to resize an already-existing cluster). </p>

##### Instances
``` purescript
Newtype InsufficientClusterCapacityFault _
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

<p>The Amazon Resource Name (ARN) supplied in the request is not valid.</p>

##### Instances
``` purescript
Newtype InvalidARNFault _
```

#### `InvalidClusterStateFault`

``` purescript
newtype InvalidClusterStateFault
  = InvalidClusterStateFault {  }
```

<p>The requested DAX cluster is not in the <i>available</i> state.</p>

##### Instances
``` purescript
Newtype InvalidClusterStateFault _
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

#### `InvalidParameterGroupStateFault`

``` purescript
newtype InvalidParameterGroupStateFault
  = InvalidParameterGroupStateFault {  }
```

<p>One or more parameters in a parameter group are in an invalid state.</p>

##### Instances
``` purescript
Newtype InvalidParameterGroupStateFault _
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

#### `IsModifiable`

``` purescript
newtype IsModifiable
  = IsModifiable String
```

##### Instances
``` purescript
Newtype IsModifiable _
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

#### `ListTagsRequest`

``` purescript
newtype ListTagsRequest
  = ListTagsRequest { "ResourceName" :: String, "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListTagsRequest _
```

#### `ListTagsResponse`

``` purescript
newtype ListTagsResponse
  = ListTagsResponse { "Tags" :: NullOrUndefined (TagList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListTagsResponse _
```

#### `Node`

``` purescript
newtype Node
  = Node { "NodeId" :: NullOrUndefined (String), "Endpoint" :: NullOrUndefined (Endpoint), "NodeCreateTime" :: NullOrUndefined (TStamp), "AvailabilityZone" :: NullOrUndefined (String), "NodeStatus" :: NullOrUndefined (String), "ParameterGroupStatus" :: NullOrUndefined (String) }
```

<p>Represents an individual node within a DAX cluster.</p>

##### Instances
``` purescript
Newtype Node _
```

#### `NodeIdentifierList`

``` purescript
newtype NodeIdentifierList
  = NodeIdentifierList (Array String)
```

##### Instances
``` purescript
Newtype NodeIdentifierList _
```

#### `NodeList`

``` purescript
newtype NodeList
  = NodeList (Array Node)
```

##### Instances
``` purescript
Newtype NodeList _
```

#### `NodeNotFoundFault`

``` purescript
newtype NodeNotFoundFault
  = NodeNotFoundFault {  }
```

<p>None of the nodes in the cluster have the given node ID.</p>

##### Instances
``` purescript
Newtype NodeNotFoundFault _
```

#### `NodeQuotaForClusterExceededFault`

``` purescript
newtype NodeQuotaForClusterExceededFault
  = NodeQuotaForClusterExceededFault {  }
```

<p>You have attempted to exceed the maximum number of nodes for a DAX cluster.</p>

##### Instances
``` purescript
Newtype NodeQuotaForClusterExceededFault _
```

#### `NodeQuotaForCustomerExceededFault`

``` purescript
newtype NodeQuotaForCustomerExceededFault
  = NodeQuotaForCustomerExceededFault {  }
```

<p>You have attempted to exceed the maximum number of nodes for your AWS account.</p>

##### Instances
``` purescript
Newtype NodeQuotaForCustomerExceededFault _
```

#### `NodeTypeSpecificValue`

``` purescript
newtype NodeTypeSpecificValue
  = NodeTypeSpecificValue { "NodeType" :: NullOrUndefined (String), "Value" :: NullOrUndefined (String) }
```

<p>Represents a parameter value that is applicable to a particular node type.</p>

##### Instances
``` purescript
Newtype NodeTypeSpecificValue _
```

#### `NodeTypeSpecificValueList`

``` purescript
newtype NodeTypeSpecificValueList
  = NodeTypeSpecificValueList (Array NodeTypeSpecificValue)
```

##### Instances
``` purescript
Newtype NodeTypeSpecificValueList _
```

#### `NotificationConfiguration`

``` purescript
newtype NotificationConfiguration
  = NotificationConfiguration { "TopicArn" :: NullOrUndefined (String), "TopicStatus" :: NullOrUndefined (String) }
```

<p>Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).</p>

##### Instances
``` purescript
Newtype NotificationConfiguration _
```

#### `Parameter`

``` purescript
newtype Parameter
  = Parameter { "ParameterName" :: NullOrUndefined (String), "ParameterType" :: NullOrUndefined (ParameterType), "ParameterValue" :: NullOrUndefined (String), "NodeTypeSpecificValues" :: NullOrUndefined (NodeTypeSpecificValueList), "Description" :: NullOrUndefined (String), "Source" :: NullOrUndefined (String), "DataType" :: NullOrUndefined (String), "AllowedValues" :: NullOrUndefined (String), "IsModifiable" :: NullOrUndefined (IsModifiable), "ChangeType" :: NullOrUndefined (ChangeType) }
```

<p>Describes an individual setting that controls some aspect of DAX behavior.</p>

##### Instances
``` purescript
Newtype Parameter _
```

#### `ParameterGroup`

``` purescript
newtype ParameterGroup
  = ParameterGroup { "ParameterGroupName" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

<p>A named set of parameters that are applied to all of the nodes in a DAX cluster.</p>

##### Instances
``` purescript
Newtype ParameterGroup _
```

#### `ParameterGroupAlreadyExistsFault`

``` purescript
newtype ParameterGroupAlreadyExistsFault
  = ParameterGroupAlreadyExistsFault {  }
```

<p>The specified parameter group already exists.</p>

##### Instances
``` purescript
Newtype ParameterGroupAlreadyExistsFault _
```

#### `ParameterGroupList`

``` purescript
newtype ParameterGroupList
  = ParameterGroupList (Array ParameterGroup)
```

##### Instances
``` purescript
Newtype ParameterGroupList _
```

#### `ParameterGroupNameList`

``` purescript
newtype ParameterGroupNameList
  = ParameterGroupNameList (Array String)
```

##### Instances
``` purescript
Newtype ParameterGroupNameList _
```

#### `ParameterGroupNotFoundFault`

``` purescript
newtype ParameterGroupNotFoundFault
  = ParameterGroupNotFoundFault {  }
```

<p>The specified parameter group does not exist.</p>

##### Instances
``` purescript
Newtype ParameterGroupNotFoundFault _
```

#### `ParameterGroupQuotaExceededFault`

``` purescript
newtype ParameterGroupQuotaExceededFault
  = ParameterGroupQuotaExceededFault {  }
```

<p>You have attempted to exceed the maximum number of parameter groups.</p>

##### Instances
``` purescript
Newtype ParameterGroupQuotaExceededFault _
```

#### `ParameterGroupStatus`

``` purescript
newtype ParameterGroupStatus
  = ParameterGroupStatus { "ParameterGroupName" :: NullOrUndefined (String), "ParameterApplyStatus" :: NullOrUndefined (String), "NodeIdsToReboot" :: NullOrUndefined (NodeIdentifierList) }
```

<p>The status of a parameter group.</p>

##### Instances
``` purescript
Newtype ParameterGroupStatus _
```

#### `ParameterList`

``` purescript
newtype ParameterList
  = ParameterList (Array Parameter)
```

##### Instances
``` purescript
Newtype ParameterList _
```

#### `ParameterNameValue`

``` purescript
newtype ParameterNameValue
  = ParameterNameValue { "ParameterName" :: NullOrUndefined (String), "ParameterValue" :: NullOrUndefined (String) }
```

<p>An individual DAX parameter.</p>

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

#### `ParameterType`

``` purescript
newtype ParameterType
  = ParameterType String
```

##### Instances
``` purescript
Newtype ParameterType _
```

#### `RebootNodeRequest`

``` purescript
newtype RebootNodeRequest
  = RebootNodeRequest { "ClusterName" :: String, "NodeId" :: String }
```

##### Instances
``` purescript
Newtype RebootNodeRequest _
```

#### `RebootNodeResponse`

``` purescript
newtype RebootNodeResponse
  = RebootNodeResponse { "Cluster" :: NullOrUndefined (Cluster) }
```

##### Instances
``` purescript
Newtype RebootNodeResponse _
```

#### `SecurityGroupIdentifierList`

``` purescript
newtype SecurityGroupIdentifierList
  = SecurityGroupIdentifierList (Array String)
```

##### Instances
``` purescript
Newtype SecurityGroupIdentifierList _
```

#### `SecurityGroupMembership`

``` purescript
newtype SecurityGroupMembership
  = SecurityGroupMembership { "SecurityGroupIdentifier" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String) }
```

<p>An individual VPC security group and its status.</p>

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
  = Subnet { "SubnetIdentifier" :: NullOrUndefined (String), "SubnetAvailabilityZone" :: NullOrUndefined (String) }
```

<p>Represents the subnet associated with a DAX cluster. This parameter refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used with DAX.</p>

##### Instances
``` purescript
Newtype Subnet _
```

#### `SubnetGroup`

``` purescript
newtype SubnetGroup
  = SubnetGroup { "SubnetGroupName" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "Subnets" :: NullOrUndefined (SubnetList) }
```

<p>Represents the output of one of the following actions:</p> <ul> <li> <p> <i>CreateSubnetGroup</i> </p> </li> <li> <p> <i>ModifySubnetGroup</i> </p> </li> </ul>

##### Instances
``` purescript
Newtype SubnetGroup _
```

#### `SubnetGroupAlreadyExistsFault`

``` purescript
newtype SubnetGroupAlreadyExistsFault
  = SubnetGroupAlreadyExistsFault {  }
```

<p>The specified subnet group already exists.</p>

##### Instances
``` purescript
Newtype SubnetGroupAlreadyExistsFault _
```

#### `SubnetGroupInUseFault`

``` purescript
newtype SubnetGroupInUseFault
  = SubnetGroupInUseFault {  }
```

<p>The specified subnet group is currently in use.</p>

##### Instances
``` purescript
Newtype SubnetGroupInUseFault _
```

#### `SubnetGroupList`

``` purescript
newtype SubnetGroupList
  = SubnetGroupList (Array SubnetGroup)
```

##### Instances
``` purescript
Newtype SubnetGroupList _
```

#### `SubnetGroupNameList`

``` purescript
newtype SubnetGroupNameList
  = SubnetGroupNameList (Array String)
```

##### Instances
``` purescript
Newtype SubnetGroupNameList _
```

#### `SubnetGroupNotFoundFault`

``` purescript
newtype SubnetGroupNotFoundFault
  = SubnetGroupNotFoundFault {  }
```

<p>The requested subnet group name does not refer to an existing subnet group.</p>

##### Instances
``` purescript
Newtype SubnetGroupNotFoundFault _
```

#### `SubnetGroupQuotaExceededFault`

``` purescript
newtype SubnetGroupQuotaExceededFault
  = SubnetGroupQuotaExceededFault {  }
```

<p>The request cannot be processed because it would exceed the allowed number of subnets in a subnet group.</p>

##### Instances
``` purescript
Newtype SubnetGroupQuotaExceededFault _
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

<p>The requested subnet is being used by another subnet group.</p>

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

#### `SubnetQuotaExceededFault`

``` purescript
newtype SubnetQuotaExceededFault
  = SubnetQuotaExceededFault {  }
```

<p>The request cannot be processed because it would exceed the allowed number of subnets in a subnet group.</p>

##### Instances
``` purescript
Newtype SubnetQuotaExceededFault _
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

<p>A description of a tag. Every tag is a key-value pair. You can add up to 50 tags to a single DAX cluster.</p> <p>AWS-assigned tag names and values are automatically assigned the <code>aws:</code> prefix, which the user cannot assign. AWS-assigned tag names do not count towards the tag limit of 50. User-assigned tag names have the prefix <code>user:</code>.</p> <p>You cannot backdate the application of a tag.</p>

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

#### `TagNotFoundFault`

``` purescript
newtype TagNotFoundFault
  = TagNotFoundFault {  }
```

<p>The tag does not exist.</p>

##### Instances
``` purescript
Newtype TagNotFoundFault _
```

#### `TagQuotaPerResourceExceeded`

``` purescript
newtype TagQuotaPerResourceExceeded
  = TagQuotaPerResourceExceeded {  }
```

<p>You have exceeded the maximum number of tags for this DAX cluster.</p>

##### Instances
``` purescript
Newtype TagQuotaPerResourceExceeded _
```

#### `TagResourceRequest`

``` purescript
newtype TagResourceRequest
  = TagResourceRequest { "ResourceName" :: String, "Tags" :: TagList }
```

##### Instances
``` purescript
Newtype TagResourceRequest _
```

#### `TagResourceResponse`

``` purescript
newtype TagResourceResponse
  = TagResourceResponse { "Tags" :: NullOrUndefined (TagList) }
```

##### Instances
``` purescript
Newtype TagResourceResponse _
```

#### `UntagResourceRequest`

``` purescript
newtype UntagResourceRequest
  = UntagResourceRequest { "ResourceName" :: String, "TagKeys" :: KeyList }
```

##### Instances
``` purescript
Newtype UntagResourceRequest _
```

#### `UntagResourceResponse`

``` purescript
newtype UntagResourceResponse
  = UntagResourceResponse { "Tags" :: NullOrUndefined (TagList) }
```

##### Instances
``` purescript
Newtype UntagResourceResponse _
```

#### `UpdateClusterRequest`

``` purescript
newtype UpdateClusterRequest
  = UpdateClusterRequest { "ClusterName" :: String, "Description" :: NullOrUndefined (String), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "NotificationTopicArn" :: NullOrUndefined (String), "NotificationTopicStatus" :: NullOrUndefined (String), "ParameterGroupName" :: NullOrUndefined (String), "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdentifierList) }
```

##### Instances
``` purescript
Newtype UpdateClusterRequest _
```

#### `UpdateClusterResponse`

``` purescript
newtype UpdateClusterResponse
  = UpdateClusterResponse { "Cluster" :: NullOrUndefined (Cluster) }
```

##### Instances
``` purescript
Newtype UpdateClusterResponse _
```

#### `UpdateParameterGroupRequest`

``` purescript
newtype UpdateParameterGroupRequest
  = UpdateParameterGroupRequest { "ParameterGroupName" :: String, "ParameterNameValues" :: ParameterNameValueList }
```

##### Instances
``` purescript
Newtype UpdateParameterGroupRequest _
```

#### `UpdateParameterGroupResponse`

``` purescript
newtype UpdateParameterGroupResponse
  = UpdateParameterGroupResponse { "ParameterGroup" :: NullOrUndefined (ParameterGroup) }
```

##### Instances
``` purescript
Newtype UpdateParameterGroupResponse _
```

#### `UpdateSubnetGroupRequest`

``` purescript
newtype UpdateSubnetGroupRequest
  = UpdateSubnetGroupRequest { "SubnetGroupName" :: String, "Description" :: NullOrUndefined (String), "SubnetIds" :: NullOrUndefined (SubnetIdentifierList) }
```

##### Instances
``` purescript
Newtype UpdateSubnetGroupRequest _
```

#### `UpdateSubnetGroupResponse`

``` purescript
newtype UpdateSubnetGroupResponse
  = UpdateSubnetGroupResponse { "SubnetGroup" :: NullOrUndefined (SubnetGroup) }
```

##### Instances
``` purescript
Newtype UpdateSubnetGroupResponse _
```


