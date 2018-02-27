## Module AWS.DirectConnect

<p>AWS Direct Connect links your internal network to an AWS Direct Connect location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic cable. One end of the cable is connected to your router, the other to an AWS Direct Connect router. With this connection in place, you can create virtual interfaces directly to the AWS cloud (for example, to Amazon Elastic Compute Cloud (Amazon EC2) and Amazon Simple Storage Service (Amazon S3)) and to Amazon Virtual Private Cloud (Amazon VPC), bypassing Internet service providers in your network path. An AWS Direct Connect location provides access to AWS in the region it is associated with, as well as access to other US regions. For example, you can provision a single connection to any AWS Direct Connect location in the US and use it to access public AWS services in all US Regions and AWS GovCloud (US).</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `allocateConnectionOnInterconnect`

``` purescript
allocateConnectionOnInterconnect :: forall eff. AllocateConnectionOnInterconnectRequest -> Aff (err :: RequestError | eff) Connection
```

<p>Deprecated in favor of <a>AllocateHostedConnection</a>.</p> <p>Creates a hosted connection on an interconnect.</p> <p>Allocates a VLAN number and a specified amount of bandwidth for use by a hosted connection on the given interconnect.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>

#### `allocateHostedConnection`

``` purescript
allocateHostedConnection :: forall eff. AllocateHostedConnectionRequest -> Aff (err :: RequestError | eff) Connection
```

<p>Creates a hosted connection on an interconnect or a link aggregation group (LAG).</p> <p>Allocates a VLAN number and a specified amount of bandwidth for use by a hosted connection on the given interconnect or LAG.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>

#### `allocatePrivateVirtualInterface`

``` purescript
allocatePrivateVirtualInterface :: forall eff. AllocatePrivateVirtualInterfaceRequest -> Aff (err :: RequestError | eff) VirtualInterface
```

<p>Provisions a private virtual interface to be owned by another AWS customer.</p> <p>Virtual interfaces created using this action must be confirmed by the virtual interface owner by using the <a>ConfirmPrivateVirtualInterface</a> action. Until then, the virtual interface will be in 'Confirming' state, and will not be available for handling traffic.</p>

#### `allocatePublicVirtualInterface`

``` purescript
allocatePublicVirtualInterface :: forall eff. AllocatePublicVirtualInterfaceRequest -> Aff (err :: RequestError | eff) VirtualInterface
```

<p>Provisions a public virtual interface to be owned by a different customer.</p> <p>The owner of a connection calls this function to provision a public virtual interface which will be owned by another AWS customer.</p> <p>Virtual interfaces created using this function must be confirmed by the virtual interface owner by calling ConfirmPublicVirtualInterface. Until this step has been completed, the virtual interface will be in 'Confirming' state, and will not be available for handling traffic.</p> <p>When creating an IPv6 public virtual interface (addressFamily is 'ipv6'), the customer and amazon address fields should be left blank to use auto-assigned IPv6 space. Custom IPv6 Addresses are currently not supported.</p>

#### `associateConnectionWithLag`

``` purescript
associateConnectionWithLag :: forall eff. AssociateConnectionWithLagRequest -> Aff (err :: RequestError | eff) Connection
```

<p>Associates an existing connection with a link aggregation group (LAG). The connection is interrupted and re-established as a member of the LAG (connectivity to AWS will be interrupted). The connection must be hosted on the same AWS Direct Connect endpoint as the LAG, and its bandwidth must match the bandwidth for the LAG. You can reassociate a connection that's currently associated with a different LAG; however, if removing the connection will cause the original LAG to fall below its setting for minimum number of operational connections, the request fails.</p> <p>Any virtual interfaces that are directly associated with the connection are automatically re-associated with the LAG. If the connection was originally associated with a different LAG, the virtual interfaces remain associated with the original LAG.</p> <p>For interconnects, any hosted connections are automatically re-associated with the LAG. If the interconnect was originally associated with a different LAG, the hosted connections remain associated with the original LAG.</p>

#### `associateHostedConnection`

``` purescript
associateHostedConnection :: forall eff. AssociateHostedConnectionRequest -> Aff (err :: RequestError | eff) Connection
```

<p>Associates a hosted connection and its virtual interfaces with a link aggregation group (LAG) or interconnect. If the target interconnect or LAG has an existing hosted connection with a conflicting VLAN number or IP address, the operation fails. This action temporarily interrupts the hosted connection's connectivity to AWS as it is being migrated.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>

#### `associateVirtualInterface`

``` purescript
associateVirtualInterface :: forall eff. AssociateVirtualInterfaceRequest -> Aff (err :: RequestError | eff) VirtualInterface
```

<p>Associates a virtual interface with a specified link aggregation group (LAG) or connection. Connectivity to AWS is temporarily interrupted as the virtual interface is being migrated. If the target connection or LAG has an associated virtual interface with a conflicting VLAN number or a conflicting IP address, the operation fails. </p> <p>Virtual interfaces associated with a hosted connection cannot be associated with a LAG; hosted connections must be migrated along with their virtual interfaces using <a>AssociateHostedConnection</a>.</p> <p>In order to reassociate a virtual interface to a new connection or LAG, the requester must own either the virtual interface itself or the connection to which the virtual interface is currently associated. Additionally, the requester must own the connection or LAG to which the virtual interface will be newly associated.</p>

#### `confirmConnection`

``` purescript
confirmConnection :: forall eff. ConfirmConnectionRequest -> Aff (err :: RequestError | eff) ConfirmConnectionResponse
```

<p>Confirm the creation of a hosted connection on an interconnect.</p> <p>Upon creation, the hosted connection is initially in the 'Ordering' state, and will remain in this state until the owner calls ConfirmConnection to confirm creation of the hosted connection.</p>

#### `confirmPrivateVirtualInterface`

``` purescript
confirmPrivateVirtualInterface :: forall eff. ConfirmPrivateVirtualInterfaceRequest -> Aff (err :: RequestError | eff) ConfirmPrivateVirtualInterfaceResponse
```

<p>Accept ownership of a private virtual interface created by another customer.</p> <p>After the virtual interface owner calls this function, the virtual interface will be created and attached to the given virtual private gateway or direct connect gateway, and will be available for handling traffic.</p>

#### `confirmPublicVirtualInterface`

``` purescript
confirmPublicVirtualInterface :: forall eff. ConfirmPublicVirtualInterfaceRequest -> Aff (err :: RequestError | eff) ConfirmPublicVirtualInterfaceResponse
```

<p>Accept ownership of a public virtual interface created by another customer.</p> <p>After the virtual interface owner calls this function, the specified virtual interface will be created and made available for handling traffic.</p>

#### `createBGPPeer`

``` purescript
createBGPPeer :: forall eff. CreateBGPPeerRequest -> Aff (err :: RequestError | eff) CreateBGPPeerResponse
```

<p>Creates a new BGP peer on a specified virtual interface. The BGP peer cannot be in the same address family (IPv4/IPv6) of an existing BGP peer on the virtual interface.</p> <p>You must create a BGP peer for the corresponding address family in order to access AWS resources that also use that address family.</p> <p>When creating a IPv6 BGP peer, the Amazon address and customer address fields must be left blank. IPv6 addresses are automatically assigned from Amazon's pool of IPv6 addresses; you cannot specify custom IPv6 addresses.</p> <p>For a public virtual interface, the Autonomous System Number (ASN) must be private or already whitelisted for the virtual interface.</p>

#### `createConnection`

``` purescript
createConnection :: forall eff. CreateConnectionRequest -> Aff (err :: RequestError | eff) Connection
```

<p>Creates a new connection between the customer network and a specific AWS Direct Connect location.</p> <p>A connection links your internal network to an AWS Direct Connect location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic cable. One end of the cable is connected to your router, the other to an AWS Direct Connect router. An AWS Direct Connect location provides access to Amazon Web Services in the region it is associated with. You can establish connections with AWS Direct Connect locations in multiple regions, but a connection in one region does not provide connectivity to other regions.</p> <p>To find the locations for your region, use <a>DescribeLocations</a>.</p> <p>You can automatically add the new connection to a link aggregation group (LAG) by specifying a LAG ID in the request. This ensures that the new connection is allocated on the same AWS Direct Connect endpoint that hosts the specified LAG. If there are no available ports on the endpoint, the request fails and no connection will be created.</p>

#### `createDirectConnectGateway`

``` purescript
createDirectConnectGateway :: forall eff. CreateDirectConnectGatewayRequest -> Aff (err :: RequestError | eff) CreateDirectConnectGatewayResult
```

<p>Creates a new direct connect gateway. A direct connect gateway is an intermediate object that enables you to connect a set of virtual interfaces and virtual private gateways. direct connect gateways are global and visible in any AWS region after they are created. The virtual interfaces and virtual private gateways that are connected through a direct connect gateway can be in different regions. This enables you to connect to a VPC in any region, regardless of the region in which the virtual interfaces are located, and pass traffic between them.</p>

#### `createDirectConnectGatewayAssociation`

``` purescript
createDirectConnectGatewayAssociation :: forall eff. CreateDirectConnectGatewayAssociationRequest -> Aff (err :: RequestError | eff) CreateDirectConnectGatewayAssociationResult
```

<p>Creates an association between a direct connect gateway and a virtual private gateway (VGW). The VGW must be attached to a VPC and must not be associated with another direct connect gateway.</p>

#### `createInterconnect`

``` purescript
createInterconnect :: forall eff. CreateInterconnectRequest -> Aff (err :: RequestError | eff) Interconnect
```

<p>Creates a new interconnect between a AWS Direct Connect partner's network and a specific AWS Direct Connect location.</p> <p>An interconnect is a connection which is capable of hosting other connections. The AWS Direct Connect partner can use an interconnect to provide sub-1Gbps AWS Direct Connect service to tier 2 customers who do not have their own connections. Like a standard connection, an interconnect links the AWS Direct Connect partner's network to an AWS Direct Connect location over a standard 1 Gbps or 10 Gbps Ethernet fiber-optic cable. One end is connected to the partner's router, the other to an AWS Direct Connect router.</p> <p>You can automatically add the new interconnect to a link aggregation group (LAG) by specifying a LAG ID in the request. This ensures that the new interconnect is allocated on the same AWS Direct Connect endpoint that hosts the specified LAG. If there are no available ports on the endpoint, the request fails and no interconnect will be created.</p> <p>For each end customer, the AWS Direct Connect partner provisions a connection on their interconnect by calling AllocateConnectionOnInterconnect. The end customer can then connect to AWS resources by creating a virtual interface on their connection, using the VLAN assigned to them by the AWS Direct Connect partner.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>

#### `createLag`

``` purescript
createLag :: forall eff. CreateLagRequest -> Aff (err :: RequestError | eff) Lag
```

<p>Creates a new link aggregation group (LAG) with the specified number of bundled physical connections between the customer network and a specific AWS Direct Connect location. A LAG is a logical interface that uses the Link Aggregation Control Protocol (LACP) to aggregate multiple 1 gigabit or 10 gigabit interfaces, allowing you to treat them as a single interface.</p> <p>All connections in a LAG must use the same bandwidth (for example, 10 Gbps), and must terminate at the same AWS Direct Connect endpoint.</p> <p>You can have up to 10 connections per LAG. Regardless of this limit, if you request more connections for the LAG than AWS Direct Connect can allocate on a single endpoint, no LAG is created.</p> <p>You can specify an existing physical connection or interconnect to include in the LAG (which counts towards the total number of connections). Doing so interrupts the current physical connection or hosted connections, and re-establishes them as a member of the LAG. The LAG will be created on the same AWS Direct Connect endpoint to which the connection terminates. Any virtual interfaces associated with the connection are automatically disassociated and re-associated with the LAG. The connection ID does not change.</p> <p>If the AWS account used to create a LAG is a registered AWS Direct Connect partner, the LAG is automatically enabled to host sub-connections. For a LAG owned by a partner, any associated virtual interfaces cannot be directly configured.</p>

#### `createPrivateVirtualInterface`

``` purescript
createPrivateVirtualInterface :: forall eff. CreatePrivateVirtualInterfaceRequest -> Aff (err :: RequestError | eff) VirtualInterface
```

<p>Creates a new private virtual interface. A virtual interface is the VLAN that transports AWS Direct Connect traffic. A private virtual interface supports sending traffic to a single virtual private cloud (VPC).</p>

#### `createPublicVirtualInterface`

``` purescript
createPublicVirtualInterface :: forall eff. CreatePublicVirtualInterfaceRequest -> Aff (err :: RequestError | eff) VirtualInterface
```

<p>Creates a new public virtual interface. A virtual interface is the VLAN that transports AWS Direct Connect traffic. A public virtual interface supports sending traffic to public services of AWS such as Amazon Simple Storage Service (Amazon S3).</p> <p>When creating an IPv6 public virtual interface (addressFamily is 'ipv6'), the customer and amazon address fields should be left blank to use auto-assigned IPv6 space. Custom IPv6 Addresses are currently not supported.</p>

#### `deleteBGPPeer`

``` purescript
deleteBGPPeer :: forall eff. DeleteBGPPeerRequest -> Aff (err :: RequestError | eff) DeleteBGPPeerResponse
```

<p>Deletes a BGP peer on the specified virtual interface that matches the specified customer address and ASN. You cannot delete the last BGP peer from a virtual interface.</p>

#### `deleteConnection`

``` purescript
deleteConnection :: forall eff. DeleteConnectionRequest -> Aff (err :: RequestError | eff) Connection
```

<p>Deletes the connection.</p> <p>Deleting a connection only stops the AWS Direct Connect port hour and data transfer charges. You need to cancel separately with the providers any services or charges for cross-connects or network circuits that connect you to the AWS Direct Connect location.</p>

#### `deleteDirectConnectGateway`

``` purescript
deleteDirectConnectGateway :: forall eff. DeleteDirectConnectGatewayRequest -> Aff (err :: RequestError | eff) DeleteDirectConnectGatewayResult
```

<p>Deletes a direct connect gateway. You must first delete all virtual interfaces that are attached to the direct connect gateway and disassociate all virtual private gateways that are associated with the direct connect gateway.</p>

#### `deleteDirectConnectGatewayAssociation`

``` purescript
deleteDirectConnectGatewayAssociation :: forall eff. DeleteDirectConnectGatewayAssociationRequest -> Aff (err :: RequestError | eff) DeleteDirectConnectGatewayAssociationResult
```

<p>Deletes the association between a direct connect gateway and a virtual private gateway.</p>

#### `deleteInterconnect`

``` purescript
deleteInterconnect :: forall eff. DeleteInterconnectRequest -> Aff (err :: RequestError | eff) DeleteInterconnectResponse
```

<p>Deletes the specified interconnect.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>

#### `deleteLag`

``` purescript
deleteLag :: forall eff. DeleteLagRequest -> Aff (err :: RequestError | eff) Lag
```

<p>Deletes a link aggregation group (LAG). You cannot delete a LAG if it has active virtual interfaces or hosted connections.</p>

#### `deleteVirtualInterface`

``` purescript
deleteVirtualInterface :: forall eff. DeleteVirtualInterfaceRequest -> Aff (err :: RequestError | eff) DeleteVirtualInterfaceResponse
```

<p>Deletes a virtual interface.</p>

#### `describeConnectionLoa`

``` purescript
describeConnectionLoa :: forall eff. DescribeConnectionLoaRequest -> Aff (err :: RequestError | eff) DescribeConnectionLoaResponse
```

<p>Deprecated in favor of <a>DescribeLoa</a>.</p> <p>Returns the LOA-CFA for a Connection.</p> <p>The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that your APN partner or service provider uses when establishing your cross connect to AWS at the colocation facility. For more information, see <a href="http://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html">Requesting Cross Connects at AWS Direct Connect Locations</a> in the AWS Direct Connect user guide.</p>

#### `describeConnections`

``` purescript
describeConnections :: forall eff. DescribeConnectionsRequest -> Aff (err :: RequestError | eff) Connections
```

<p>Displays all connections in this region.</p> <p>If a connection ID is provided, the call returns only that particular connection.</p>

#### `describeConnectionsOnInterconnect`

``` purescript
describeConnectionsOnInterconnect :: forall eff. DescribeConnectionsOnInterconnectRequest -> Aff (err :: RequestError | eff) Connections
```

<p>Deprecated in favor of <a>DescribeHostedConnections</a>.</p> <p>Returns a list of connections that have been provisioned on the given interconnect.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>

#### `describeDirectConnectGatewayAssociations`

``` purescript
describeDirectConnectGatewayAssociations :: forall eff. DescribeDirectConnectGatewayAssociationsRequest -> Aff (err :: RequestError | eff) DescribeDirectConnectGatewayAssociationsResult
```

<p>Returns a list of all direct connect gateway and virtual private gateway (VGW) associations. Either a direct connect gateway ID or a VGW ID must be provided in the request. If a direct connect gateway ID is provided, the response returns all VGWs associated with the direct connect gateway. If a VGW ID is provided, the response returns all direct connect gateways associated with the VGW. If both are provided, the response only returns the association that matches both the direct connect gateway and the VGW.</p>

#### `describeDirectConnectGatewayAttachments`

``` purescript
describeDirectConnectGatewayAttachments :: forall eff. DescribeDirectConnectGatewayAttachmentsRequest -> Aff (err :: RequestError | eff) DescribeDirectConnectGatewayAttachmentsResult
```

<p>Returns a list of all direct connect gateway and virtual interface (VIF) attachments. Either a direct connect gateway ID or a VIF ID must be provided in the request. If a direct connect gateway ID is provided, the response returns all VIFs attached to the direct connect gateway. If a VIF ID is provided, the response returns all direct connect gateways attached to the VIF. If both are provided, the response only returns the attachment that matches both the direct connect gateway and the VIF.</p>

#### `describeDirectConnectGateways`

``` purescript
describeDirectConnectGateways :: forall eff. DescribeDirectConnectGatewaysRequest -> Aff (err :: RequestError | eff) DescribeDirectConnectGatewaysResult
```

<p>Returns a list of direct connect gateways in your account. Deleted direct connect gateways are not returned. You can provide a direct connect gateway ID in the request to return information about the specific direct connect gateway only. Otherwise, if a direct connect gateway ID is not provided, information about all of your direct connect gateways is returned. </p>

#### `describeHostedConnections`

``` purescript
describeHostedConnections :: forall eff. DescribeHostedConnectionsRequest -> Aff (err :: RequestError | eff) Connections
```

<p>Returns a list of hosted connections that have been provisioned on the given interconnect or link aggregation group (LAG).</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>

#### `describeInterconnectLoa`

``` purescript
describeInterconnectLoa :: forall eff. DescribeInterconnectLoaRequest -> Aff (err :: RequestError | eff) DescribeInterconnectLoaResponse
```

<p>Deprecated in favor of <a>DescribeLoa</a>.</p> <p>Returns the LOA-CFA for an Interconnect.</p> <p>The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that is used when establishing your cross connect to AWS at the colocation facility. For more information, see <a href="http://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html">Requesting Cross Connects at AWS Direct Connect Locations</a> in the AWS Direct Connect user guide.</p>

#### `describeInterconnects`

``` purescript
describeInterconnects :: forall eff. DescribeInterconnectsRequest -> Aff (err :: RequestError | eff) Interconnects
```

<p>Returns a list of interconnects owned by the AWS account.</p> <p>If an interconnect ID is provided, it will only return this particular interconnect.</p>

#### `describeLags`

``` purescript
describeLags :: forall eff. DescribeLagsRequest -> Aff (err :: RequestError | eff) Lags
```

<p>Describes the link aggregation groups (LAGs) in your account. </p> <p>If a LAG ID is provided, only information about the specified LAG is returned.</p>

#### `describeLoa`

``` purescript
describeLoa :: forall eff. DescribeLoaRequest -> Aff (err :: RequestError | eff) Loa
```

<p>Returns the LOA-CFA for a connection, interconnect, or link aggregation group (LAG).</p> <p>The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that is used when establishing your cross connect to AWS at the colocation facility. For more information, see <a href="http://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html">Requesting Cross Connects at AWS Direct Connect Locations</a> in the AWS Direct Connect user guide.</p>

#### `describeLocations`

``` purescript
describeLocations :: forall eff. Aff (err :: RequestError | eff) Locations
```

<p>Returns the list of AWS Direct Connect locations in the current AWS region. These are the locations that may be selected when calling <a>CreateConnection</a> or <a>CreateInterconnect</a>.</p>

#### `describeTags`

``` purescript
describeTags :: forall eff. DescribeTagsRequest -> Aff (err :: RequestError | eff) DescribeTagsResponse
```

<p>Describes the tags associated with the specified Direct Connect resources.</p>

#### `describeVirtualGateways`

``` purescript
describeVirtualGateways :: forall eff. Aff (err :: RequestError | eff) VirtualGateways
```

<p>Returns a list of virtual private gateways owned by the AWS account.</p> <p>You can create one or more AWS Direct Connect private virtual interfaces linking to a virtual private gateway. A virtual private gateway can be managed via Amazon Virtual Private Cloud (VPC) console or the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html">EC2 CreateVpnGateway</a> action.</p>

#### `describeVirtualInterfaces`

``` purescript
describeVirtualInterfaces :: forall eff. DescribeVirtualInterfacesRequest -> Aff (err :: RequestError | eff) VirtualInterfaces
```

<p>Displays all virtual interfaces for an AWS account. Virtual interfaces deleted fewer than 15 minutes before you make the request are also returned. If you specify a connection ID, only the virtual interfaces associated with the connection are returned. If you specify a virtual interface ID, then only a single virtual interface is returned.</p> <p>A virtual interface (VLAN) transmits the traffic between the AWS Direct Connect location and the customer.</p>

#### `disassociateConnectionFromLag`

``` purescript
disassociateConnectionFromLag :: forall eff. DisassociateConnectionFromLagRequest -> Aff (err :: RequestError | eff) Connection
```

<p>Disassociates a connection from a link aggregation group (LAG). The connection is interrupted and re-established as a standalone connection (the connection is not deleted; to delete the connection, use the <a>DeleteConnection</a> request). If the LAG has associated virtual interfaces or hosted connections, they remain associated with the LAG. A disassociated connection owned by an AWS Direct Connect partner is automatically converted to an interconnect.</p> <p>If disassociating the connection will cause the LAG to fall below its setting for minimum number of operational connections, the request fails, except when it's the last member of the LAG. If all connections are disassociated, the LAG continues to exist as an empty LAG with no physical connections. </p>

#### `tagResource`

``` purescript
tagResource :: forall eff. TagResourceRequest -> Aff (err :: RequestError | eff) TagResourceResponse
```

<p>Adds the specified tags to the specified Direct Connect resource. Each Direct Connect resource can have a maximum of 50 tags.</p> <p>Each tag consists of a key and an optional value. If a tag with the same key is already associated with the Direct Connect resource, this action updates its value.</p>

#### `untagResource`

``` purescript
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: RequestError | eff) UntagResourceResponse
```

<p>Removes one or more tags from the specified Direct Connect resource.</p>

#### `updateLag`

``` purescript
updateLag :: forall eff. UpdateLagRequest -> Aff (err :: RequestError | eff) Lag
```

<p>Updates the attributes of a link aggregation group (LAG). </p> <p>You can update the following attributes: </p> <ul> <li> <p>The name of the LAG.</p> </li> <li> <p>The value for the minimum number of connections that must be operational for the LAG itself to be operational. </p> </li> </ul> <p>When you create a LAG, the default value for the minimum number of operational connections is zero (0). If you update this value, and the number of operational connections falls below the specified value, the LAG will automatically go down to avoid overutilization of the remaining connections. Adjusting this value should be done with care as it could force the LAG down if the value is set higher than the current number of operational connections.</p>

#### `ASN`

``` purescript
newtype ASN
  = ASN Int
```

<p>The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.</p> <p>Example: 65000</p>

##### Instances
``` purescript
Newtype ASN _
```

#### `AddressFamily`

``` purescript
newtype AddressFamily
  = AddressFamily String
```

<p>Indicates the address family for the BGP peer.</p> <ul> <li> <p> <b>ipv4</b>: IPv4 address family</p> </li> <li> <p> <b>ipv6</b>: IPv6 address family</p> </li> </ul>

##### Instances
``` purescript
Newtype AddressFamily _
```

#### `AllocateConnectionOnInterconnectRequest`

``` purescript
newtype AllocateConnectionOnInterconnectRequest
  = AllocateConnectionOnInterconnectRequest { "Bandwidth'" :: Bandwidth, "ConnectionName'" :: ConnectionName, "OwnerAccount'" :: OwnerAccount, "InterconnectId'" :: InterconnectId, "Vlan'" :: VLAN }
```

<p>Container for the parameters to the AllocateConnectionOnInterconnect operation.</p>

##### Instances
``` purescript
Newtype AllocateConnectionOnInterconnectRequest _
```

#### `AllocateHostedConnectionRequest`

``` purescript
newtype AllocateHostedConnectionRequest
  = AllocateHostedConnectionRequest { "ConnectionId'" :: ConnectionId, "OwnerAccount'" :: OwnerAccount, "Bandwidth'" :: Bandwidth, "ConnectionName'" :: ConnectionName, "Vlan'" :: VLAN }
```

<p>Container for the parameters to theHostedConnection operation.</p>

##### Instances
``` purescript
Newtype AllocateHostedConnectionRequest _
```

#### `AllocatePrivateVirtualInterfaceRequest`

``` purescript
newtype AllocatePrivateVirtualInterfaceRequest
  = AllocatePrivateVirtualInterfaceRequest { "ConnectionId'" :: ConnectionId, "OwnerAccount'" :: OwnerAccount, "NewPrivateVirtualInterfaceAllocation'" :: NewPrivateVirtualInterfaceAllocation }
```

<p>Container for the parameters to the AllocatePrivateVirtualInterface operation.</p>

##### Instances
``` purescript
Newtype AllocatePrivateVirtualInterfaceRequest _
```

#### `AllocatePublicVirtualInterfaceRequest`

``` purescript
newtype AllocatePublicVirtualInterfaceRequest
  = AllocatePublicVirtualInterfaceRequest { "ConnectionId'" :: ConnectionId, "OwnerAccount'" :: OwnerAccount, "NewPublicVirtualInterfaceAllocation'" :: NewPublicVirtualInterfaceAllocation }
```

<p>Container for the parameters to the AllocatePublicVirtualInterface operation.</p>

##### Instances
``` purescript
Newtype AllocatePublicVirtualInterfaceRequest _
```

#### `AmazonAddress`

``` purescript
newtype AmazonAddress
  = AmazonAddress String
```

<p>IP address assigned to the Amazon interface.</p> <p>Example: 192.168.1.1/30 or 2001:db8::1/125</p>

##### Instances
``` purescript
Newtype AmazonAddress _
```

#### `AssociateConnectionWithLagRequest`

``` purescript
newtype AssociateConnectionWithLagRequest
  = AssociateConnectionWithLagRequest { "ConnectionId'" :: ConnectionId, "LagId'" :: LagId }
```

<p>Container for the parameters to the AssociateConnectionWithLag operation.</p>

##### Instances
``` purescript
Newtype AssociateConnectionWithLagRequest _
```

#### `AssociateHostedConnectionRequest`

``` purescript
newtype AssociateHostedConnectionRequest
  = AssociateHostedConnectionRequest { "ConnectionId'" :: ConnectionId, "ParentConnectionId'" :: ConnectionId }
```

<p>Container for the parameters to the AssociateHostedConnection operation.</p>

##### Instances
``` purescript
Newtype AssociateHostedConnectionRequest _
```

#### `AssociateVirtualInterfaceRequest`

``` purescript
newtype AssociateVirtualInterfaceRequest
  = AssociateVirtualInterfaceRequest { "VirtualInterfaceId'" :: VirtualInterfaceId, "ConnectionId'" :: ConnectionId }
```

<p>Container for the parameters to the AssociateVirtualInterface operation.</p>

##### Instances
``` purescript
Newtype AssociateVirtualInterfaceRequest _
```

#### `AwsDevice`

``` purescript
newtype AwsDevice
  = AwsDevice String
```

<p>An abstract ID for the physical Direct Connect endpoint.</p> <p>Example: EQC50-abcdef123456</p>

##### Instances
``` purescript
Newtype AwsDevice _
```

#### `BGPAuthKey`

``` purescript
newtype BGPAuthKey
  = BGPAuthKey String
```

<p>The authentication key for BGP configuration.</p> <p>Example: asdf34example</p>

##### Instances
``` purescript
Newtype BGPAuthKey _
```

#### `BGPPeer`

``` purescript
newtype BGPPeer
  = BGPPeer { "Asn'" :: NullOrUndefined (ASN), "AuthKey'" :: NullOrUndefined (BGPAuthKey), "AddressFamily'" :: NullOrUndefined (AddressFamily), "AmazonAddress'" :: NullOrUndefined (AmazonAddress), "CustomerAddress'" :: NullOrUndefined (CustomerAddress), "BgpPeerState'" :: NullOrUndefined (BGPPeerState), "BgpStatus'" :: NullOrUndefined (BGPStatus) }
```

<p>A structure containing information about a BGP peer.</p>

##### Instances
``` purescript
Newtype BGPPeer _
```

#### `BGPPeerList`

``` purescript
newtype BGPPeerList
  = BGPPeerList (Array BGPPeer)
```

<p>A list of the BGP peers configured on this virtual interface.</p>

##### Instances
``` purescript
Newtype BGPPeerList _
```

#### `BGPPeerState`

``` purescript
newtype BGPPeerState
  = BGPPeerState String
```

<p>The state of the BGP peer.</p> <ul> <li> <p> <b>Verifying</b>: The BGP peering addresses or ASN require validation before the BGP peer can be created. This state only applies to BGP peers on a public virtual interface. </p> </li> <li> <p> <b>Pending</b>: The BGP peer has been created, and is in this state until it is ready to be established.</p> </li> <li> <p> <b>Available</b>: The BGP peer can be established.</p> </li> <li> <p> <b>Deleting</b>: The BGP peer is in the process of being deleted.</p> </li> <li> <p> <b>Deleted</b>: The BGP peer has been deleted and cannot be established.</p> </li> </ul>

##### Instances
``` purescript
Newtype BGPPeerState _
```

#### `BGPStatus`

``` purescript
newtype BGPStatus
  = BGPStatus String
```

<p>The Up/Down state of the BGP peer.</p> <ul> <li> <p> <b>Up</b>: The BGP peer is established.</p> </li> <li> <p> <b>Down</b>: The BGP peer is down.</p> </li> </ul>

##### Instances
``` purescript
Newtype BGPStatus _
```

#### `Bandwidth`

``` purescript
newtype Bandwidth
  = Bandwidth String
```

<p>Bandwidth of the connection.</p> <p>Example: 1Gbps</p> <p>Default: None</p>

##### Instances
``` purescript
Newtype Bandwidth _
```

#### `BooleanFlag`

``` purescript
newtype BooleanFlag
  = BooleanFlag Boolean
```

##### Instances
``` purescript
Newtype BooleanFlag _
```

#### `CIDR`

``` purescript
newtype CIDR
  = CIDR String
```

##### Instances
``` purescript
Newtype CIDR _
```

#### `ConfirmConnectionRequest`

``` purescript
newtype ConfirmConnectionRequest
  = ConfirmConnectionRequest { "ConnectionId'" :: ConnectionId }
```

<p>Container for the parameters to the ConfirmConnection operation.</p>

##### Instances
``` purescript
Newtype ConfirmConnectionRequest _
```

#### `ConfirmConnectionResponse`

``` purescript
newtype ConfirmConnectionResponse
  = ConfirmConnectionResponse { "ConnectionState'" :: NullOrUndefined (ConnectionState) }
```

<p>The response received when ConfirmConnection is called.</p>

##### Instances
``` purescript
Newtype ConfirmConnectionResponse _
```

#### `ConfirmPrivateVirtualInterfaceRequest`

``` purescript
newtype ConfirmPrivateVirtualInterfaceRequest
  = ConfirmPrivateVirtualInterfaceRequest { "VirtualInterfaceId'" :: VirtualInterfaceId, "VirtualGatewayId'" :: NullOrUndefined (VirtualGatewayId), "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId) }
```

<p>Container for the parameters to the ConfirmPrivateVirtualInterface operation.</p>

##### Instances
``` purescript
Newtype ConfirmPrivateVirtualInterfaceRequest _
```

#### `ConfirmPrivateVirtualInterfaceResponse`

``` purescript
newtype ConfirmPrivateVirtualInterfaceResponse
  = ConfirmPrivateVirtualInterfaceResponse { "VirtualInterfaceState'" :: NullOrUndefined (VirtualInterfaceState) }
```

<p>The response received when ConfirmPrivateVirtualInterface is called.</p>

##### Instances
``` purescript
Newtype ConfirmPrivateVirtualInterfaceResponse _
```

#### `ConfirmPublicVirtualInterfaceRequest`

``` purescript
newtype ConfirmPublicVirtualInterfaceRequest
  = ConfirmPublicVirtualInterfaceRequest { "VirtualInterfaceId'" :: VirtualInterfaceId }
```

<p>Container for the parameters to the ConfirmPublicVirtualInterface operation.</p>

##### Instances
``` purescript
Newtype ConfirmPublicVirtualInterfaceRequest _
```

#### `ConfirmPublicVirtualInterfaceResponse`

``` purescript
newtype ConfirmPublicVirtualInterfaceResponse
  = ConfirmPublicVirtualInterfaceResponse { "VirtualInterfaceState'" :: NullOrUndefined (VirtualInterfaceState) }
```

<p>The response received when ConfirmPublicVirtualInterface is called.</p>

##### Instances
``` purescript
Newtype ConfirmPublicVirtualInterfaceResponse _
```

#### `Connection`

``` purescript
newtype Connection
  = Connection { "OwnerAccount'" :: NullOrUndefined (OwnerAccount), "ConnectionId'" :: NullOrUndefined (ConnectionId), "ConnectionName'" :: NullOrUndefined (ConnectionName), "ConnectionState'" :: NullOrUndefined (ConnectionState), "Region'" :: NullOrUndefined (Region), "Location'" :: NullOrUndefined (LocationCode), "Bandwidth'" :: NullOrUndefined (Bandwidth), "Vlan'" :: NullOrUndefined (VLAN), "PartnerName'" :: NullOrUndefined (PartnerName), "LoaIssueTime'" :: NullOrUndefined (LoaIssueTime), "LagId'" :: NullOrUndefined (LagId), "AwsDevice'" :: NullOrUndefined (AwsDevice) }
```

<p>A connection represents the physical network connection between the AWS Direct Connect location and the customer.</p>

##### Instances
``` purescript
Newtype Connection _
```

#### `ConnectionId`

``` purescript
newtype ConnectionId
  = ConnectionId String
```

<p>The ID of the connection. This field is also used as the ID type for operations that use multiple connection types (LAG, interconnect, and/or connection).</p> <p>Example: dxcon-fg5678gh</p> <p>Default: None</p>

##### Instances
``` purescript
Newtype ConnectionId _
```

#### `ConnectionList`

``` purescript
newtype ConnectionList
  = ConnectionList (Array Connection)
```

<p>A list of connections.</p>

##### Instances
``` purescript
Newtype ConnectionList _
```

#### `ConnectionName`

``` purescript
newtype ConnectionName
  = ConnectionName String
```

<p>The name of the connection.</p> <p>Example: "<i>My Connection to AWS</i>"</p> <p>Default: None</p>

##### Instances
``` purescript
Newtype ConnectionName _
```

#### `ConnectionState`

``` purescript
newtype ConnectionState
  = ConnectionState String
```

<p>State of the connection.</p> <ul> <li> <p> <b>Ordering</b>: The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.</p> </li> <li> <p> <b>Requested</b>: The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.</p> </li> <li> <p> <b>Pending</b>: The connection has been approved, and is being initialized.</p> </li> <li> <p> <b>Available</b>: The network link is up, and the connection is ready for use.</p> </li> <li> <p> <b>Down</b>: The network link is down.</p> </li> <li> <p> <b>Deleting</b>: The connection is in the process of being deleted.</p> </li> <li> <p> <b>Deleted</b>: The connection has been deleted.</p> </li> <li> <p> <b>Rejected</b>: A hosted connection in the 'Ordering' state will enter the 'Rejected' state if it is deleted by the end customer.</p> </li> </ul>

##### Instances
``` purescript
Newtype ConnectionState _
```

#### `Connections`

``` purescript
newtype Connections
  = Connections { "Connections'" :: NullOrUndefined (ConnectionList) }
```

<p>A structure containing a list of connections.</p>

##### Instances
``` purescript
Newtype Connections _
```

#### `Count`

``` purescript
newtype Count
  = Count Int
```

##### Instances
``` purescript
Newtype Count _
```

#### `CreateBGPPeerRequest`

``` purescript
newtype CreateBGPPeerRequest
  = CreateBGPPeerRequest { "VirtualInterfaceId'" :: NullOrUndefined (VirtualInterfaceId), "NewBGPPeer'" :: NullOrUndefined (NewBGPPeer) }
```

<p>Container for the parameters to the CreateBGPPeer operation.</p>

##### Instances
``` purescript
Newtype CreateBGPPeerRequest _
```

#### `CreateBGPPeerResponse`

``` purescript
newtype CreateBGPPeerResponse
  = CreateBGPPeerResponse { "VirtualInterface'" :: NullOrUndefined (VirtualInterface) }
```

<p>The response received when CreateBGPPeer is called.</p>

##### Instances
``` purescript
Newtype CreateBGPPeerResponse _
```

#### `CreateConnectionRequest`

``` purescript
newtype CreateConnectionRequest
  = CreateConnectionRequest { "Location'" :: LocationCode, "Bandwidth'" :: Bandwidth, "ConnectionName'" :: ConnectionName, "LagId'" :: NullOrUndefined (LagId) }
```

<p>Container for the parameters to the CreateConnection operation.</p>

##### Instances
``` purescript
Newtype CreateConnectionRequest _
```

#### `CreateDirectConnectGatewayAssociationRequest`

``` purescript
newtype CreateDirectConnectGatewayAssociationRequest
  = CreateDirectConnectGatewayAssociationRequest { "DirectConnectGatewayId'" :: DirectConnectGatewayId, "VirtualGatewayId'" :: VirtualGatewayId }
```

<p>Container for the parameters to the CreateDirectConnectGatewayAssociation operation.</p>

##### Instances
``` purescript
Newtype CreateDirectConnectGatewayAssociationRequest _
```

#### `CreateDirectConnectGatewayAssociationResult`

``` purescript
newtype CreateDirectConnectGatewayAssociationResult
  = CreateDirectConnectGatewayAssociationResult { "DirectConnectGatewayAssociation'" :: NullOrUndefined (DirectConnectGatewayAssociation) }
```

<p>Container for the response from the CreateDirectConnectGatewayAssociation API call</p>

##### Instances
``` purescript
Newtype CreateDirectConnectGatewayAssociationResult _
```

#### `CreateDirectConnectGatewayRequest`

``` purescript
newtype CreateDirectConnectGatewayRequest
  = CreateDirectConnectGatewayRequest { "DirectConnectGatewayName'" :: DirectConnectGatewayName, "AmazonSideAsn'" :: NullOrUndefined (LongAsn) }
```

<p>Container for the parameters to the CreateDirectConnectGateway operation.</p>

##### Instances
``` purescript
Newtype CreateDirectConnectGatewayRequest _
```

#### `CreateDirectConnectGatewayResult`

``` purescript
newtype CreateDirectConnectGatewayResult
  = CreateDirectConnectGatewayResult { "DirectConnectGateway'" :: NullOrUndefined (DirectConnectGateway) }
```

<p>Container for the response from the CreateDirectConnectGateway API call</p>

##### Instances
``` purescript
Newtype CreateDirectConnectGatewayResult _
```

#### `CreateInterconnectRequest`

``` purescript
newtype CreateInterconnectRequest
  = CreateInterconnectRequest { "InterconnectName'" :: InterconnectName, "Bandwidth'" :: Bandwidth, "Location'" :: LocationCode, "LagId'" :: NullOrUndefined (LagId) }
```

<p>Container for the parameters to the CreateInterconnect operation.</p>

##### Instances
``` purescript
Newtype CreateInterconnectRequest _
```

#### `CreateLagRequest`

``` purescript
newtype CreateLagRequest
  = CreateLagRequest { "NumberOfConnections'" :: Count, "Location'" :: LocationCode, "ConnectionsBandwidth'" :: Bandwidth, "LagName'" :: LagName, "ConnectionId'" :: NullOrUndefined (ConnectionId) }
```

<p>Container for the parameters to the CreateLag operation.</p>

##### Instances
``` purescript
Newtype CreateLagRequest _
```

#### `CreatePrivateVirtualInterfaceRequest`

``` purescript
newtype CreatePrivateVirtualInterfaceRequest
  = CreatePrivateVirtualInterfaceRequest { "ConnectionId'" :: ConnectionId, "NewPrivateVirtualInterface'" :: NewPrivateVirtualInterface }
```

<p>Container for the parameters to the CreatePrivateVirtualInterface operation.</p>

##### Instances
``` purescript
Newtype CreatePrivateVirtualInterfaceRequest _
```

#### `CreatePublicVirtualInterfaceRequest`

``` purescript
newtype CreatePublicVirtualInterfaceRequest
  = CreatePublicVirtualInterfaceRequest { "ConnectionId'" :: ConnectionId, "NewPublicVirtualInterface'" :: NewPublicVirtualInterface }
```

<p>Container for the parameters to the CreatePublicVirtualInterface operation.</p>

##### Instances
``` purescript
Newtype CreatePublicVirtualInterfaceRequest _
```

#### `CustomerAddress`

``` purescript
newtype CustomerAddress
  = CustomerAddress String
```

<p>IP address assigned to the customer interface.</p> <p>Example: 192.168.1.2/30 or 2001:db8::2/125</p>

##### Instances
``` purescript
Newtype CustomerAddress _
```

#### `DeleteBGPPeerRequest`

``` purescript
newtype DeleteBGPPeerRequest
  = DeleteBGPPeerRequest { "VirtualInterfaceId'" :: NullOrUndefined (VirtualInterfaceId), "Asn'" :: NullOrUndefined (ASN), "CustomerAddress'" :: NullOrUndefined (CustomerAddress) }
```

<p>Container for the parameters to the DeleteBGPPeer operation.</p>

##### Instances
``` purescript
Newtype DeleteBGPPeerRequest _
```

#### `DeleteBGPPeerResponse`

``` purescript
newtype DeleteBGPPeerResponse
  = DeleteBGPPeerResponse { "VirtualInterface'" :: NullOrUndefined (VirtualInterface) }
```

<p>The response received when DeleteBGPPeer is called.</p>

##### Instances
``` purescript
Newtype DeleteBGPPeerResponse _
```

#### `DeleteConnectionRequest`

``` purescript
newtype DeleteConnectionRequest
  = DeleteConnectionRequest { "ConnectionId'" :: ConnectionId }
```

<p>Container for the parameters to the DeleteConnection operation.</p>

##### Instances
``` purescript
Newtype DeleteConnectionRequest _
```

#### `DeleteDirectConnectGatewayAssociationRequest`

``` purescript
newtype DeleteDirectConnectGatewayAssociationRequest
  = DeleteDirectConnectGatewayAssociationRequest { "DirectConnectGatewayId'" :: DirectConnectGatewayId, "VirtualGatewayId'" :: VirtualGatewayId }
```

<p>Container for the parameters to the DeleteDirectConnectGatewayAssociation operation.</p>

##### Instances
``` purescript
Newtype DeleteDirectConnectGatewayAssociationRequest _
```

#### `DeleteDirectConnectGatewayAssociationResult`

``` purescript
newtype DeleteDirectConnectGatewayAssociationResult
  = DeleteDirectConnectGatewayAssociationResult { "DirectConnectGatewayAssociation'" :: NullOrUndefined (DirectConnectGatewayAssociation) }
```

<p>Container for the response from the DeleteDirectConnectGatewayAssociation API call</p>

##### Instances
``` purescript
Newtype DeleteDirectConnectGatewayAssociationResult _
```

#### `DeleteDirectConnectGatewayRequest`

``` purescript
newtype DeleteDirectConnectGatewayRequest
  = DeleteDirectConnectGatewayRequest { "DirectConnectGatewayId'" :: DirectConnectGatewayId }
```

<p>Container for the parameters to the DeleteDirectConnectGateway operation.</p>

##### Instances
``` purescript
Newtype DeleteDirectConnectGatewayRequest _
```

#### `DeleteDirectConnectGatewayResult`

``` purescript
newtype DeleteDirectConnectGatewayResult
  = DeleteDirectConnectGatewayResult { "DirectConnectGateway'" :: NullOrUndefined (DirectConnectGateway) }
```

<p>Container for the response from the DeleteDirectConnectGateway API call</p>

##### Instances
``` purescript
Newtype DeleteDirectConnectGatewayResult _
```

#### `DeleteInterconnectRequest`

``` purescript
newtype DeleteInterconnectRequest
  = DeleteInterconnectRequest { "InterconnectId'" :: InterconnectId }
```

<p>Container for the parameters to the DeleteInterconnect operation.</p>

##### Instances
``` purescript
Newtype DeleteInterconnectRequest _
```

#### `DeleteInterconnectResponse`

``` purescript
newtype DeleteInterconnectResponse
  = DeleteInterconnectResponse { "InterconnectState'" :: NullOrUndefined (InterconnectState) }
```

<p>The response received when DeleteInterconnect is called.</p>

##### Instances
``` purescript
Newtype DeleteInterconnectResponse _
```

#### `DeleteLagRequest`

``` purescript
newtype DeleteLagRequest
  = DeleteLagRequest { "LagId'" :: LagId }
```

<p>Container for the parameters to the DeleteLag operation.</p>

##### Instances
``` purescript
Newtype DeleteLagRequest _
```

#### `DeleteVirtualInterfaceRequest`

``` purescript
newtype DeleteVirtualInterfaceRequest
  = DeleteVirtualInterfaceRequest { "VirtualInterfaceId'" :: VirtualInterfaceId }
```

<p>Container for the parameters to the DeleteVirtualInterface operation.</p>

##### Instances
``` purescript
Newtype DeleteVirtualInterfaceRequest _
```

#### `DeleteVirtualInterfaceResponse`

``` purescript
newtype DeleteVirtualInterfaceResponse
  = DeleteVirtualInterfaceResponse { "VirtualInterfaceState'" :: NullOrUndefined (VirtualInterfaceState) }
```

<p>The response received when DeleteVirtualInterface is called.</p>

##### Instances
``` purescript
Newtype DeleteVirtualInterfaceResponse _
```

#### `DescribeConnectionLoaRequest`

``` purescript
newtype DescribeConnectionLoaRequest
  = DescribeConnectionLoaRequest { "ConnectionId'" :: ConnectionId, "ProviderName'" :: NullOrUndefined (ProviderName), "LoaContentType'" :: NullOrUndefined (LoaContentType) }
```

<p>Container for the parameters to the DescribeConnectionLoa operation.</p>

##### Instances
``` purescript
Newtype DescribeConnectionLoaRequest _
```

#### `DescribeConnectionLoaResponse`

``` purescript
newtype DescribeConnectionLoaResponse
  = DescribeConnectionLoaResponse { "Loa'" :: NullOrUndefined (Loa) }
```

<p>The response received when DescribeConnectionLoa is called.</p>

##### Instances
``` purescript
Newtype DescribeConnectionLoaResponse _
```

#### `DescribeConnectionsOnInterconnectRequest`

``` purescript
newtype DescribeConnectionsOnInterconnectRequest
  = DescribeConnectionsOnInterconnectRequest { "InterconnectId'" :: InterconnectId }
```

<p>Container for the parameters to the DescribeConnectionsOnInterconnect operation.</p>

##### Instances
``` purescript
Newtype DescribeConnectionsOnInterconnectRequest _
```

#### `DescribeConnectionsRequest`

``` purescript
newtype DescribeConnectionsRequest
  = DescribeConnectionsRequest { "ConnectionId'" :: NullOrUndefined (ConnectionId) }
```

<p>Container for the parameters to the DescribeConnections operation.</p>

##### Instances
``` purescript
Newtype DescribeConnectionsRequest _
```

#### `DescribeDirectConnectGatewayAssociationsRequest`

``` purescript
newtype DescribeDirectConnectGatewayAssociationsRequest
  = DescribeDirectConnectGatewayAssociationsRequest { "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId), "VirtualGatewayId'" :: NullOrUndefined (VirtualGatewayId), "MaxResults'" :: NullOrUndefined (MaxResultSetSize), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Container for the parameters to the DescribeDirectConnectGatewayAssociations operation.</p>

##### Instances
``` purescript
Newtype DescribeDirectConnectGatewayAssociationsRequest _
```

#### `DescribeDirectConnectGatewayAssociationsResult`

``` purescript
newtype DescribeDirectConnectGatewayAssociationsResult
  = DescribeDirectConnectGatewayAssociationsResult { "DirectConnectGatewayAssociations'" :: NullOrUndefined (DirectConnectGatewayAssociationList), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Container for the response from the DescribeDirectConnectGatewayAssociations API call</p>

##### Instances
``` purescript
Newtype DescribeDirectConnectGatewayAssociationsResult _
```

#### `DescribeDirectConnectGatewayAttachmentsRequest`

``` purescript
newtype DescribeDirectConnectGatewayAttachmentsRequest
  = DescribeDirectConnectGatewayAttachmentsRequest { "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId), "VirtualInterfaceId'" :: NullOrUndefined (VirtualInterfaceId), "MaxResults'" :: NullOrUndefined (MaxResultSetSize), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Container for the parameters to the DescribeDirectConnectGatewayAttachments operation.</p>

##### Instances
``` purescript
Newtype DescribeDirectConnectGatewayAttachmentsRequest _
```

#### `DescribeDirectConnectGatewayAttachmentsResult`

``` purescript
newtype DescribeDirectConnectGatewayAttachmentsResult
  = DescribeDirectConnectGatewayAttachmentsResult { "DirectConnectGatewayAttachments'" :: NullOrUndefined (DirectConnectGatewayAttachmentList), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Container for the response from the DescribeDirectConnectGatewayAttachments API call</p>

##### Instances
``` purescript
Newtype DescribeDirectConnectGatewayAttachmentsResult _
```

#### `DescribeDirectConnectGatewaysRequest`

``` purescript
newtype DescribeDirectConnectGatewaysRequest
  = DescribeDirectConnectGatewaysRequest { "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId), "MaxResults'" :: NullOrUndefined (MaxResultSetSize), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Container for the parameters to the DescribeDirectConnectGateways operation.</p>

##### Instances
``` purescript
Newtype DescribeDirectConnectGatewaysRequest _
```

#### `DescribeDirectConnectGatewaysResult`

``` purescript
newtype DescribeDirectConnectGatewaysResult
  = DescribeDirectConnectGatewaysResult { "DirectConnectGateways'" :: NullOrUndefined (DirectConnectGatewayList), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Container for the response from the DescribeDirectConnectGateways API call</p>

##### Instances
``` purescript
Newtype DescribeDirectConnectGatewaysResult _
```

#### `DescribeHostedConnectionsRequest`

``` purescript
newtype DescribeHostedConnectionsRequest
  = DescribeHostedConnectionsRequest { "ConnectionId'" :: ConnectionId }
```

<p>Container for the parameters to the DescribeHostedConnections operation.</p>

##### Instances
``` purescript
Newtype DescribeHostedConnectionsRequest _
```

#### `DescribeInterconnectLoaRequest`

``` purescript
newtype DescribeInterconnectLoaRequest
  = DescribeInterconnectLoaRequest { "InterconnectId'" :: InterconnectId, "ProviderName'" :: NullOrUndefined (ProviderName), "LoaContentType'" :: NullOrUndefined (LoaContentType) }
```

<p>Container for the parameters to the DescribeInterconnectLoa operation.</p>

##### Instances
``` purescript
Newtype DescribeInterconnectLoaRequest _
```

#### `DescribeInterconnectLoaResponse`

``` purescript
newtype DescribeInterconnectLoaResponse
  = DescribeInterconnectLoaResponse { "Loa'" :: NullOrUndefined (Loa) }
```

<p>The response received when DescribeInterconnectLoa is called.</p>

##### Instances
``` purescript
Newtype DescribeInterconnectLoaResponse _
```

#### `DescribeInterconnectsRequest`

``` purescript
newtype DescribeInterconnectsRequest
  = DescribeInterconnectsRequest { "InterconnectId'" :: NullOrUndefined (InterconnectId) }
```

<p>Container for the parameters to the DescribeInterconnects operation.</p>

##### Instances
``` purescript
Newtype DescribeInterconnectsRequest _
```

#### `DescribeLagsRequest`

``` purescript
newtype DescribeLagsRequest
  = DescribeLagsRequest { "LagId'" :: NullOrUndefined (LagId) }
```

<p>Container for the parameters to the DescribeLags operation.</p>

##### Instances
``` purescript
Newtype DescribeLagsRequest _
```

#### `DescribeLoaRequest`

``` purescript
newtype DescribeLoaRequest
  = DescribeLoaRequest { "ConnectionId'" :: ConnectionId, "ProviderName'" :: NullOrUndefined (ProviderName), "LoaContentType'" :: NullOrUndefined (LoaContentType) }
```

<p>Container for the parameters to the DescribeLoa operation.</p>

##### Instances
``` purescript
Newtype DescribeLoaRequest _
```

#### `DescribeTagsRequest`

``` purescript
newtype DescribeTagsRequest
  = DescribeTagsRequest { "ResourceArns'" :: ResourceArnList }
```

<p>Container for the parameters to the DescribeTags operation.</p>

##### Instances
``` purescript
Newtype DescribeTagsRequest _
```

#### `DescribeTagsResponse`

``` purescript
newtype DescribeTagsResponse
  = DescribeTagsResponse { "ResourceTags'" :: NullOrUndefined (ResourceTagList) }
```

<p>The response received when DescribeTags is called.</p>

##### Instances
``` purescript
Newtype DescribeTagsResponse _
```

#### `DescribeVirtualInterfacesRequest`

``` purescript
newtype DescribeVirtualInterfacesRequest
  = DescribeVirtualInterfacesRequest { "ConnectionId'" :: NullOrUndefined (ConnectionId), "VirtualInterfaceId'" :: NullOrUndefined (VirtualInterfaceId) }
```

<p>Container for the parameters to the DescribeVirtualInterfaces operation.</p>

##### Instances
``` purescript
Newtype DescribeVirtualInterfacesRequest _
```

#### `DirectConnectClientException`

``` purescript
newtype DirectConnectClientException
  = DirectConnectClientException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The API was called with invalid parameters. The error message will contain additional details about the cause.</p>

##### Instances
``` purescript
Newtype DirectConnectClientException _
```

#### `DirectConnectGateway`

``` purescript
newtype DirectConnectGateway
  = DirectConnectGateway { "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId), "DirectConnectGatewayName'" :: NullOrUndefined (DirectConnectGatewayName), "AmazonSideAsn'" :: NullOrUndefined (LongAsn), "OwnerAccount'" :: NullOrUndefined (OwnerAccount), "DirectConnectGatewayState'" :: NullOrUndefined (DirectConnectGatewayState), "StateChangeError'" :: NullOrUndefined (StateChangeError) }
```

<p>A direct connect gateway is an intermediate object that enables you to connect virtual interfaces and virtual private gateways.</p>

##### Instances
``` purescript
Newtype DirectConnectGateway _
```

#### `DirectConnectGatewayAssociation`

``` purescript
newtype DirectConnectGatewayAssociation
  = DirectConnectGatewayAssociation { "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId), "VirtualGatewayId'" :: NullOrUndefined (VirtualGatewayId), "VirtualGatewayRegion'" :: NullOrUndefined (VirtualGatewayRegion), "VirtualGatewayOwnerAccount'" :: NullOrUndefined (OwnerAccount), "AssociationState'" :: NullOrUndefined (DirectConnectGatewayAssociationState), "StateChangeError'" :: NullOrUndefined (StateChangeError) }
```

<p>The association between a direct connect gateway and virtual private gateway.</p>

##### Instances
``` purescript
Newtype DirectConnectGatewayAssociation _
```

#### `DirectConnectGatewayAssociationList`

``` purescript
newtype DirectConnectGatewayAssociationList
  = DirectConnectGatewayAssociationList (Array DirectConnectGatewayAssociation)
```

<p>A list of direct connect gateway associations.</p>

##### Instances
``` purescript
Newtype DirectConnectGatewayAssociationList _
```

#### `DirectConnectGatewayAssociationState`

``` purescript
newtype DirectConnectGatewayAssociationState
  = DirectConnectGatewayAssociationState String
```

<p>State of the direct connect gateway association.</p> <ul> <li> <p> <b>Associating</b>: The initial state after calling <a>CreateDirectConnectGatewayAssociation</a>.</p> </li> <li> <p> <b>Associated</b>: The direct connect gateway and virtual private gateway are successfully associated and ready to pass traffic.</p> </li> <li> <p> <b>Disassociating</b>: The initial state after calling <a>DeleteDirectConnectGatewayAssociation</a>.</p> </li> <li> <p> <b>Disassociated</b>: The virtual private gateway is successfully disassociated from the direct connect gateway. Traffic flow between the direct connect gateway and virtual private gateway stops.</p> </li> </ul>

##### Instances
``` purescript
Newtype DirectConnectGatewayAssociationState _
```

#### `DirectConnectGatewayAttachment`

``` purescript
newtype DirectConnectGatewayAttachment
  = DirectConnectGatewayAttachment { "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId), "VirtualInterfaceId'" :: NullOrUndefined (VirtualInterfaceId), "VirtualInterfaceRegion'" :: NullOrUndefined (VirtualInterfaceRegion), "VirtualInterfaceOwnerAccount'" :: NullOrUndefined (OwnerAccount), "AttachmentState'" :: NullOrUndefined (DirectConnectGatewayAttachmentState), "StateChangeError'" :: NullOrUndefined (StateChangeError) }
```

<p>The association between a direct connect gateway and virtual interface.</p>

##### Instances
``` purescript
Newtype DirectConnectGatewayAttachment _
```

#### `DirectConnectGatewayAttachmentList`

``` purescript
newtype DirectConnectGatewayAttachmentList
  = DirectConnectGatewayAttachmentList (Array DirectConnectGatewayAttachment)
```

<p>A list of direct connect gateway attachments.</p>

##### Instances
``` purescript
Newtype DirectConnectGatewayAttachmentList _
```

#### `DirectConnectGatewayAttachmentState`

``` purescript
newtype DirectConnectGatewayAttachmentState
  = DirectConnectGatewayAttachmentState String
```

<p>State of the direct connect gateway attachment.</p> <ul> <li> <p> <b>Attaching</b>: The initial state after a virtual interface is created using the direct connect gateway.</p> </li> <li> <p> <b>Attached</b>: The direct connect gateway and virtual interface are successfully attached and ready to pass traffic.</p> </li> <li> <p> <b>Detaching</b>: The initial state after calling <a>DeleteVirtualInterface</a> on a virtual interface that is attached to a direct connect gateway.</p> </li> <li> <p> <b>Detached</b>: The virtual interface is successfully detached from the direct connect gateway. Traffic flow between the direct connect gateway and virtual interface stops.</p> </li> </ul>

##### Instances
``` purescript
Newtype DirectConnectGatewayAttachmentState _
```

#### `DirectConnectGatewayId`

``` purescript
newtype DirectConnectGatewayId
  = DirectConnectGatewayId String
```

<p>The ID of the direct connect gateway.</p> <p>Example: "abcd1234-dcba-5678-be23-cdef9876ab45"</p>

##### Instances
``` purescript
Newtype DirectConnectGatewayId _
```

#### `DirectConnectGatewayList`

``` purescript
newtype DirectConnectGatewayList
  = DirectConnectGatewayList (Array DirectConnectGateway)
```

<p>A list of direct connect gateways.</p>

##### Instances
``` purescript
Newtype DirectConnectGatewayList _
```

#### `DirectConnectGatewayName`

``` purescript
newtype DirectConnectGatewayName
  = DirectConnectGatewayName String
```

<p>The name of the direct connect gateway.</p> <p>Example: "My direct connect gateway"</p> <p>Default: None</p>

##### Instances
``` purescript
Newtype DirectConnectGatewayName _
```

#### `DirectConnectGatewayState`

``` purescript
newtype DirectConnectGatewayState
  = DirectConnectGatewayState String
```

<p>State of the direct connect gateway.</p> <ul> <li> <p> <b>Pending</b>: The initial state after calling <a>CreateDirectConnectGateway</a>.</p> </li> <li> <p> <b>Available</b>: The direct connect gateway is ready for use.</p> </li> <li> <p> <b>Deleting</b>: The initial state after calling <a>DeleteDirectConnectGateway</a>.</p> </li> <li> <p> <b>Deleted</b>: The direct connect gateway is deleted and cannot pass traffic.</p> </li> </ul>

##### Instances
``` purescript
Newtype DirectConnectGatewayState _
```

#### `DirectConnectServerException`

``` purescript
newtype DirectConnectServerException
  = DirectConnectServerException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>A server-side error occurred during the API call. The error message will contain additional details about the cause.</p>

##### Instances
``` purescript
Newtype DirectConnectServerException _
```

#### `DisassociateConnectionFromLagRequest`

``` purescript
newtype DisassociateConnectionFromLagRequest
  = DisassociateConnectionFromLagRequest { "ConnectionId'" :: ConnectionId, "LagId'" :: LagId }
```

<p>Container for the parameters to the DisassociateConnectionFromLag operation.</p>

##### Instances
``` purescript
Newtype DisassociateConnectionFromLagRequest _
```

#### `DuplicateTagKeysException`

``` purescript
newtype DuplicateTagKeysException
  = DuplicateTagKeysException {  }
```

<p>A tag key was specified more than once.</p>

##### Instances
``` purescript
Newtype DuplicateTagKeysException _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `Interconnect`

``` purescript
newtype Interconnect
  = Interconnect { "InterconnectId'" :: NullOrUndefined (InterconnectId), "InterconnectName'" :: NullOrUndefined (InterconnectName), "InterconnectState'" :: NullOrUndefined (InterconnectState), "Region'" :: NullOrUndefined (Region), "Location'" :: NullOrUndefined (LocationCode), "Bandwidth'" :: NullOrUndefined (Bandwidth), "LoaIssueTime'" :: NullOrUndefined (LoaIssueTime), "LagId'" :: NullOrUndefined (LagId), "AwsDevice'" :: NullOrUndefined (AwsDevice) }
```

<p>An interconnect is a connection that can host other connections.</p> <p>Like a standard AWS Direct Connect connection, an interconnect represents the physical connection between an AWS Direct Connect partner's network and a specific Direct Connect location. An AWS Direct Connect partner who owns an interconnect can provision hosted connections on the interconnect for their end customers, thereby providing the end customers with connectivity to AWS services.</p> <p>The resources of the interconnect, including bandwidth and VLAN numbers, are shared by all of the hosted connections on the interconnect, and the owner of the interconnect determines how these resources are assigned.</p>

##### Instances
``` purescript
Newtype Interconnect _
```

#### `InterconnectId`

``` purescript
newtype InterconnectId
  = InterconnectId String
```

<p>The ID of the interconnect.</p> <p>Example: dxcon-abc123</p>

##### Instances
``` purescript
Newtype InterconnectId _
```

#### `InterconnectList`

``` purescript
newtype InterconnectList
  = InterconnectList (Array Interconnect)
```

<p>A list of interconnects.</p>

##### Instances
``` purescript
Newtype InterconnectList _
```

#### `InterconnectName`

``` purescript
newtype InterconnectName
  = InterconnectName String
```

<p>The name of the interconnect.</p> <p>Example: "<i>1G Interconnect to AWS</i>"</p>

##### Instances
``` purescript
Newtype InterconnectName _
```

#### `InterconnectState`

``` purescript
newtype InterconnectState
  = InterconnectState String
```

<p>State of the interconnect.</p> <ul> <li> <p> <b>Requested</b>: The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.</p> </li> <li> <p> <b>Pending</b>: The interconnect has been approved, and is being initialized.</p> </li> <li> <p> <b>Available</b>: The network link is up, and the interconnect is ready for use.</p> </li> <li> <p> <b>Down</b>: The network link is down.</p> </li> <li> <p> <b>Deleting</b>: The interconnect is in the process of being deleted.</p> </li> <li> <p> <b>Deleted</b>: The interconnect has been deleted.</p> </li> </ul>

##### Instances
``` purescript
Newtype InterconnectState _
```

#### `Interconnects`

``` purescript
newtype Interconnects
  = Interconnects { "Interconnects'" :: NullOrUndefined (InterconnectList) }
```

<p>A structure containing a list of interconnects.</p>

##### Instances
``` purescript
Newtype Interconnects _
```

#### `Lag`

``` purescript
newtype Lag
  = Lag { "ConnectionsBandwidth'" :: NullOrUndefined (Bandwidth), "NumberOfConnections'" :: NullOrUndefined (Count), "LagId'" :: NullOrUndefined (LagId), "OwnerAccount'" :: NullOrUndefined (OwnerAccount), "LagName'" :: NullOrUndefined (LagName), "LagState'" :: NullOrUndefined (LagState), "Location'" :: NullOrUndefined (LocationCode), "Region'" :: NullOrUndefined (Region), "MinimumLinks'" :: NullOrUndefined (Count), "AwsDevice'" :: NullOrUndefined (AwsDevice), "Connections'" :: NullOrUndefined (ConnectionList), "AllowsHostedConnections'" :: NullOrUndefined (BooleanFlag) }
```

<p>Describes a link aggregation group (LAG). A LAG is a connection that uses the Link Aggregation Control Protocol (LACP) to logically aggregate a bundle of physical connections. Like an interconnect, it can host other connections. All connections in a LAG must terminate on the same physical AWS Direct Connect endpoint, and must be the same bandwidth.</p>

##### Instances
``` purescript
Newtype Lag _
```

#### `LagId`

``` purescript
newtype LagId
  = LagId String
```

<p>The ID of the LAG.</p> <p>Example: dxlag-fg5678gh</p>

##### Instances
``` purescript
Newtype LagId _
```

#### `LagList`

``` purescript
newtype LagList
  = LagList (Array Lag)
```

<p>A list of LAGs.</p>

##### Instances
``` purescript
Newtype LagList _
```

#### `LagName`

``` purescript
newtype LagName
  = LagName String
```

##### Instances
``` purescript
Newtype LagName _
```

#### `LagState`

``` purescript
newtype LagState
  = LagState String
```

<p>The state of the LAG.</p> <ul> <li> <p> <b>Requested</b>: The initial state of a LAG. The LAG stays in the requested state until the Letter of Authorization (LOA) is available.</p> </li> <li> <p> <b>Pending</b>: The LAG has been approved, and is being initialized.</p> </li> <li> <p> <b>Available</b>: The network link is established, and the LAG is ready for use.</p> </li> <li> <p> <b>Down</b>: The network link is down.</p> </li> <li> <p> <b>Deleting</b>: The LAG is in the process of being deleted.</p> </li> <li> <p> <b>Deleted</b>: The LAG has been deleted.</p> </li> </ul>

##### Instances
``` purescript
Newtype LagState _
```

#### `Lags`

``` purescript
newtype Lags
  = Lags { "Lags'" :: NullOrUndefined (LagList) }
```

<p>A structure containing a list of LAGs.</p>

##### Instances
``` purescript
Newtype Lags _
```

#### `Loa`

``` purescript
newtype Loa
  = Loa { "LoaContent'" :: NullOrUndefined (LoaContent), "LoaContentType'" :: NullOrUndefined (LoaContentType) }
```

<p>A structure containing the Letter of Authorization - Connecting Facility Assignment (LOA-CFA) for a connection.</p>

##### Instances
``` purescript
Newtype Loa _
```

#### `LoaContent`

``` purescript
newtype LoaContent
  = LoaContent String
```

<p>The binary contents of the LOA-CFA document.</p>

##### Instances
``` purescript
Newtype LoaContent _
```

#### `LoaContentType`

``` purescript
newtype LoaContentType
  = LoaContentType String
```

<p>A standard media type indicating the content type of the LOA-CFA document. Currently, the only supported value is "application/pdf".</p> <p>Default: application/pdf</p>

##### Instances
``` purescript
Newtype LoaContentType _
```

#### `LoaIssueTime`

``` purescript
newtype LoaIssueTime
  = LoaIssueTime Number
```

##### Instances
``` purescript
Newtype LoaIssueTime _
```

#### `Location`

``` purescript
newtype Location
  = Location { "LocationCode'" :: NullOrUndefined (LocationCode), "LocationName'" :: NullOrUndefined (LocationName) }
```

<p>An AWS Direct Connect location where connections and interconnects can be requested.</p>

##### Instances
``` purescript
Newtype Location _
```

#### `LocationCode`

``` purescript
newtype LocationCode
  = LocationCode String
```

<p>Where the connection is located.</p> <p>Example: EqSV5</p> <p>Default: None</p>

##### Instances
``` purescript
Newtype LocationCode _
```

#### `LocationList`

``` purescript
newtype LocationList
  = LocationList (Array Location)
```

##### Instances
``` purescript
Newtype LocationList _
```

#### `LocationName`

``` purescript
newtype LocationName
  = LocationName String
```

##### Instances
``` purescript
Newtype LocationName _
```

#### `Locations`

``` purescript
newtype Locations
  = Locations { "Locations'" :: NullOrUndefined (LocationList) }
```

<p>A location is a network facility where AWS Direct Connect routers are available to be connected. Generally, these are colocation hubs where many network providers have equipment, and where cross connects can be delivered. Locations include a name and facility code, and must be provided when creating a connection.</p>

##### Instances
``` purescript
Newtype Locations _
```

#### `LongAsn`

``` purescript
newtype LongAsn
  = LongAsn Number
```

##### Instances
``` purescript
Newtype LongAsn _
```

#### `MaxResultSetSize`

``` purescript
newtype MaxResultSetSize
  = MaxResultSetSize Int
```

<p>Maximum number of objects to return per page.</p>

##### Instances
``` purescript
Newtype MaxResultSetSize _
```

#### `NewBGPPeer`

``` purescript
newtype NewBGPPeer
  = NewBGPPeer { "Asn'" :: NullOrUndefined (ASN), "AuthKey'" :: NullOrUndefined (BGPAuthKey), "AddressFamily'" :: NullOrUndefined (AddressFamily), "AmazonAddress'" :: NullOrUndefined (AmazonAddress), "CustomerAddress'" :: NullOrUndefined (CustomerAddress) }
```

<p>A structure containing information about a new BGP peer.</p>

##### Instances
``` purescript
Newtype NewBGPPeer _
```

#### `NewPrivateVirtualInterface`

``` purescript
newtype NewPrivateVirtualInterface
  = NewPrivateVirtualInterface { "VirtualInterfaceName'" :: VirtualInterfaceName, "Vlan'" :: VLAN, "Asn'" :: ASN, "AuthKey'" :: NullOrUndefined (BGPAuthKey), "AmazonAddress'" :: NullOrUndefined (AmazonAddress), "CustomerAddress'" :: NullOrUndefined (CustomerAddress), "AddressFamily'" :: NullOrUndefined (AddressFamily), "VirtualGatewayId'" :: NullOrUndefined (VirtualGatewayId), "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId) }
```

<p>A structure containing information about a new private virtual interface.</p>

##### Instances
``` purescript
Newtype NewPrivateVirtualInterface _
```

#### `NewPrivateVirtualInterfaceAllocation`

``` purescript
newtype NewPrivateVirtualInterfaceAllocation
  = NewPrivateVirtualInterfaceAllocation { "VirtualInterfaceName'" :: VirtualInterfaceName, "Vlan'" :: VLAN, "Asn'" :: ASN, "AuthKey'" :: NullOrUndefined (BGPAuthKey), "AmazonAddress'" :: NullOrUndefined (AmazonAddress), "AddressFamily'" :: NullOrUndefined (AddressFamily), "CustomerAddress'" :: NullOrUndefined (CustomerAddress) }
```

<p>A structure containing information about a private virtual interface that will be provisioned on a connection.</p>

##### Instances
``` purescript
Newtype NewPrivateVirtualInterfaceAllocation _
```

#### `NewPublicVirtualInterface`

``` purescript
newtype NewPublicVirtualInterface
  = NewPublicVirtualInterface { "VirtualInterfaceName'" :: VirtualInterfaceName, "Vlan'" :: VLAN, "Asn'" :: ASN, "AuthKey'" :: NullOrUndefined (BGPAuthKey), "AmazonAddress'" :: NullOrUndefined (AmazonAddress), "CustomerAddress'" :: NullOrUndefined (CustomerAddress), "AddressFamily'" :: NullOrUndefined (AddressFamily), "RouteFilterPrefixes'" :: NullOrUndefined (RouteFilterPrefixList) }
```

<p>A structure containing information about a new public virtual interface.</p>

##### Instances
``` purescript
Newtype NewPublicVirtualInterface _
```

#### `NewPublicVirtualInterfaceAllocation`

``` purescript
newtype NewPublicVirtualInterfaceAllocation
  = NewPublicVirtualInterfaceAllocation { "VirtualInterfaceName'" :: VirtualInterfaceName, "Vlan'" :: VLAN, "Asn'" :: ASN, "AuthKey'" :: NullOrUndefined (BGPAuthKey), "AmazonAddress'" :: NullOrUndefined (AmazonAddress), "CustomerAddress'" :: NullOrUndefined (CustomerAddress), "AddressFamily'" :: NullOrUndefined (AddressFamily), "RouteFilterPrefixes'" :: NullOrUndefined (RouteFilterPrefixList) }
```

<p>A structure containing information about a public virtual interface that will be provisioned on a connection.</p>

##### Instances
``` purescript
Newtype NewPublicVirtualInterfaceAllocation _
```

#### `OwnerAccount`

``` purescript
newtype OwnerAccount
  = OwnerAccount String
```

##### Instances
``` purescript
Newtype OwnerAccount _
```

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

<p>Token to retrieve the next page of the result.</p>

##### Instances
``` purescript
Newtype PaginationToken _
```

#### `PartnerName`

``` purescript
newtype PartnerName
  = PartnerName String
```

##### Instances
``` purescript
Newtype PartnerName _
```

#### `ProviderName`

``` purescript
newtype ProviderName
  = ProviderName String
```

##### Instances
``` purescript
Newtype ProviderName _
```

#### `Region`

``` purescript
newtype Region
  = Region String
```

<p>The AWS region where the connection is located.</p> <p>Example: us-east-1</p> <p>Default: None</p>

##### Instances
``` purescript
Newtype Region _
```

#### `ResourceArn`

``` purescript
newtype ResourceArn
  = ResourceArn String
```

##### Instances
``` purescript
Newtype ResourceArn _
```

#### `ResourceArnList`

``` purescript
newtype ResourceArnList
  = ResourceArnList (Array ResourceArn)
```

##### Instances
``` purescript
Newtype ResourceArnList _
```

#### `ResourceTag`

``` purescript
newtype ResourceTag
  = ResourceTag { "ResourceArn'" :: NullOrUndefined (ResourceArn), "Tags'" :: NullOrUndefined (TagList) }
```

<p>The tags associated with a Direct Connect resource.</p>

##### Instances
``` purescript
Newtype ResourceTag _
```

#### `ResourceTagList`

``` purescript
newtype ResourceTagList
  = ResourceTagList (Array ResourceTag)
```

##### Instances
``` purescript
Newtype ResourceTagList _
```

#### `RouteFilterPrefix`

``` purescript
newtype RouteFilterPrefix
  = RouteFilterPrefix { "Cidr'" :: NullOrUndefined (CIDR) }
```

<p>A route filter prefix that the customer can advertise through Border Gateway Protocol (BGP) over a public virtual interface.</p>

##### Instances
``` purescript
Newtype RouteFilterPrefix _
```

#### `RouteFilterPrefixList`

``` purescript
newtype RouteFilterPrefixList
  = RouteFilterPrefixList (Array RouteFilterPrefix)
```

<p>A list of routes to be advertised to the AWS network in this region (public virtual interface).</p>

##### Instances
``` purescript
Newtype RouteFilterPrefixList _
```

#### `RouterConfig`

``` purescript
newtype RouterConfig
  = RouterConfig String
```

##### Instances
``` purescript
Newtype RouterConfig _
```

#### `StateChangeError`

``` purescript
newtype StateChangeError
  = StateChangeError String
```

<p>Error message when the state of an object fails to advance.</p>

##### Instances
``` purescript
Newtype StateChangeError _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key'" :: TagKey, "Value'" :: NullOrUndefined (TagValue) }
```

<p>Information about a tag.</p>

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
  = TagResourceRequest { "ResourceArn'" :: ResourceArn, "Tags'" :: TagList }
```

<p>Container for the parameters to the TagResource operation.</p>

##### Instances
``` purescript
Newtype TagResourceRequest _
```

#### `TagResourceResponse`

``` purescript
newtype TagResourceResponse
  = TagResourceResponse {  }
```

<p>The response received when TagResource is called.</p>

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

#### `TooManyTagsException`

``` purescript
newtype TooManyTagsException
  = TooManyTagsException {  }
```

<p>You have reached the limit on the number of tags that can be assigned to a Direct Connect resource.</p>

##### Instances
``` purescript
Newtype TooManyTagsException _
```

#### `UntagResourceRequest`

``` purescript
newtype UntagResourceRequest
  = UntagResourceRequest { "ResourceArn'" :: ResourceArn, "TagKeys'" :: TagKeyList }
```

<p>Container for the parameters to the UntagResource operation.</p>

##### Instances
``` purescript
Newtype UntagResourceRequest _
```

#### `UntagResourceResponse`

``` purescript
newtype UntagResourceResponse
  = UntagResourceResponse {  }
```

<p>The response received when UntagResource is called.</p>

##### Instances
``` purescript
Newtype UntagResourceResponse _
```

#### `UpdateLagRequest`

``` purescript
newtype UpdateLagRequest
  = UpdateLagRequest { "LagId'" :: LagId, "LagName'" :: NullOrUndefined (LagName), "MinimumLinks'" :: NullOrUndefined (Count) }
```

<p>Container for the parameters to the UpdateLag operation.</p>

##### Instances
``` purescript
Newtype UpdateLagRequest _
```

#### `VLAN`

``` purescript
newtype VLAN
  = VLAN Int
```

<p>The VLAN ID.</p> <p>Example: 101</p>

##### Instances
``` purescript
Newtype VLAN _
```

#### `VirtualGateway`

``` purescript
newtype VirtualGateway
  = VirtualGateway { "VirtualGatewayId'" :: NullOrUndefined (VirtualGatewayId), "VirtualGatewayState'" :: NullOrUndefined (VirtualGatewayState) }
```

<p>You can create one or more AWS Direct Connect private virtual interfaces linking to your virtual private gateway.</p> <p>Virtual private gateways can be managed using the Amazon Virtual Private Cloud (Amazon VPC) console or the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html">Amazon EC2 CreateVpnGateway action</a>.</p>

##### Instances
``` purescript
Newtype VirtualGateway _
```

#### `VirtualGatewayId`

``` purescript
newtype VirtualGatewayId
  = VirtualGatewayId String
```

<p>The ID of the virtual private gateway to a VPC. This only applies to private virtual interfaces.</p> <p>Example: vgw-123er56</p>

##### Instances
``` purescript
Newtype VirtualGatewayId _
```

#### `VirtualGatewayList`

``` purescript
newtype VirtualGatewayList
  = VirtualGatewayList (Array VirtualGateway)
```

<p>A list of virtual private gateways.</p>

##### Instances
``` purescript
Newtype VirtualGatewayList _
```

#### `VirtualGatewayRegion`

``` purescript
newtype VirtualGatewayRegion
  = VirtualGatewayRegion String
```

<p>The region in which the virtual private gateway is located.</p> <p>Example: us-east-1</p>

##### Instances
``` purescript
Newtype VirtualGatewayRegion _
```

#### `VirtualGatewayState`

``` purescript
newtype VirtualGatewayState
  = VirtualGatewayState String
```

<p>State of the virtual private gateway.</p> <ul> <li> <p> <b>Pending</b>: This is the initial state after calling <i>CreateVpnGateway</i>.</p> </li> <li> <p> <b>Available</b>: Ready for use by a private virtual interface.</p> </li> <li> <p> <b>Deleting</b>: This is the initial state after calling <i>DeleteVpnGateway</i>.</p> </li> <li> <p> <b>Deleted</b>: In this state, a private virtual interface is unable to send traffic over this gateway.</p> </li> </ul>

##### Instances
``` purescript
Newtype VirtualGatewayState _
```

#### `VirtualGateways`

``` purescript
newtype VirtualGateways
  = VirtualGateways { "VirtualGateways'" :: NullOrUndefined (VirtualGatewayList) }
```

<p>A structure containing a list of virtual private gateways.</p>

##### Instances
``` purescript
Newtype VirtualGateways _
```

#### `VirtualInterface`

``` purescript
newtype VirtualInterface
  = VirtualInterface { "OwnerAccount'" :: NullOrUndefined (OwnerAccount), "VirtualInterfaceId'" :: NullOrUndefined (VirtualInterfaceId), "Location'" :: NullOrUndefined (LocationCode), "ConnectionId'" :: NullOrUndefined (ConnectionId), "VirtualInterfaceType'" :: NullOrUndefined (VirtualInterfaceType), "VirtualInterfaceName'" :: NullOrUndefined (VirtualInterfaceName), "Vlan'" :: NullOrUndefined (VLAN), "Asn'" :: NullOrUndefined (ASN), "AmazonSideAsn'" :: NullOrUndefined (LongAsn), "AuthKey'" :: NullOrUndefined (BGPAuthKey), "AmazonAddress'" :: NullOrUndefined (AmazonAddress), "CustomerAddress'" :: NullOrUndefined (CustomerAddress), "AddressFamily'" :: NullOrUndefined (AddressFamily), "VirtualInterfaceState'" :: NullOrUndefined (VirtualInterfaceState), "CustomerRouterConfig'" :: NullOrUndefined (RouterConfig), "VirtualGatewayId'" :: NullOrUndefined (VirtualGatewayId), "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId), "RouteFilterPrefixes'" :: NullOrUndefined (RouteFilterPrefixList), "BgpPeers'" :: NullOrUndefined (BGPPeerList) }
```

<p>A virtual interface (VLAN) transmits the traffic between the AWS Direct Connect location and the customer.</p>

##### Instances
``` purescript
Newtype VirtualInterface _
```

#### `VirtualInterfaceId`

``` purescript
newtype VirtualInterfaceId
  = VirtualInterfaceId String
```

<p>The ID of the virtual interface.</p> <p>Example: dxvif-123dfg56</p> <p>Default: None</p>

##### Instances
``` purescript
Newtype VirtualInterfaceId _
```

#### `VirtualInterfaceList`

``` purescript
newtype VirtualInterfaceList
  = VirtualInterfaceList (Array VirtualInterface)
```

<p>A list of virtual interfaces.</p>

##### Instances
``` purescript
Newtype VirtualInterfaceList _
```

#### `VirtualInterfaceName`

``` purescript
newtype VirtualInterfaceName
  = VirtualInterfaceName String
```

<p>The name of the virtual interface assigned by the customer.</p> <p>Example: "My VPC"</p>

##### Instances
``` purescript
Newtype VirtualInterfaceName _
```

#### `VirtualInterfaceRegion`

``` purescript
newtype VirtualInterfaceRegion
  = VirtualInterfaceRegion String
```

<p>The region in which the virtual interface is located.</p> <p>Example: us-east-1</p>

##### Instances
``` purescript
Newtype VirtualInterfaceRegion _
```

#### `VirtualInterfaceState`

``` purescript
newtype VirtualInterfaceState
  = VirtualInterfaceState String
```

<p>State of the virtual interface.</p> <ul> <li> <p> <b>Confirming</b>: The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.</p> </li> <li> <p> <b>Verifying</b>: This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.</p> </li> <li> <p> <b>Pending</b>: A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.</p> </li> <li> <p> <b>Available</b>: A virtual interface that is able to forward traffic.</p> </li> <li> <p> <b>Down</b>: A virtual interface that is BGP down.</p> </li> <li> <p> <b>Deleting</b>: A virtual interface is in this state immediately after calling <a>DeleteVirtualInterface</a> until it can no longer forward traffic.</p> </li> <li> <p> <b>Deleted</b>: A virtual interface that cannot forward traffic.</p> </li> <li> <p> <b>Rejected</b>: The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the 'Confirming' state is deleted by the virtual interface owner, the virtual interface will enter the 'Rejected' state.</p> </li> </ul>

##### Instances
``` purescript
Newtype VirtualInterfaceState _
```

#### `VirtualInterfaceType`

``` purescript
newtype VirtualInterfaceType
  = VirtualInterfaceType String
```

<p>The type of virtual interface.</p> <p>Example: private (Amazon VPC) or public (Amazon S3, Amazon DynamoDB, and so on.)</p>

##### Instances
``` purescript
Newtype VirtualInterfaceType _
```

#### `VirtualInterfaces`

``` purescript
newtype VirtualInterfaces
  = VirtualInterfaces { "VirtualInterfaces'" :: NullOrUndefined (VirtualInterfaceList) }
```

<p>A structure containing a list of virtual interfaces.</p>

##### Instances
``` purescript
Newtype VirtualInterfaces _
```


