

-- | <p>AWS Direct Connect links your internal network to an AWS Direct Connect location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic cable. One end of the cable is connected to your router, the other to an AWS Direct Connect router. With this connection in place, you can create virtual interfaces directly to the AWS cloud (for example, to Amazon Elastic Compute Cloud (Amazon EC2) and Amazon Simple Storage Service (Amazon S3)) and to Amazon Virtual Private Cloud (Amazon VPC), bypassing Internet service providers in your network path. An AWS Direct Connect location provides access to AWS in the region it is associated with, as well as access to other US regions. For example, you can provision a single connection to any AWS Direct Connect location in the US and use it to access public AWS services in all US Regions and AWS GovCloud (US).</p>
module AWS.DirectConnect where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "DirectConnect" :: String


-- | <p>Deprecated in favor of <a>AllocateHostedConnection</a>.</p> <p>Creates a hosted connection on an interconnect.</p> <p>Allocates a VLAN number and a specified amount of bandwidth for use by a hosted connection on the given interconnect.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
allocateConnectionOnInterconnect :: forall eff. AllocateConnectionOnInterconnectRequest -> Aff (err :: AWS.RequestError | eff) Connection
allocateConnectionOnInterconnect = AWS.request serviceName "AllocateConnectionOnInterconnect" 


-- | <p>Creates a hosted connection on an interconnect or a link aggregation group (LAG).</p> <p>Allocates a VLAN number and a specified amount of bandwidth for use by a hosted connection on the given interconnect or LAG.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
allocateHostedConnection :: forall eff. AllocateHostedConnectionRequest -> Aff (err :: AWS.RequestError | eff) Connection
allocateHostedConnection = AWS.request serviceName "AllocateHostedConnection" 


-- | <p>Provisions a private virtual interface to be owned by another AWS customer.</p> <p>Virtual interfaces created using this action must be confirmed by the virtual interface owner by using the <a>ConfirmPrivateVirtualInterface</a> action. Until then, the virtual interface will be in 'Confirming' state, and will not be available for handling traffic.</p>
allocatePrivateVirtualInterface :: forall eff. AllocatePrivateVirtualInterfaceRequest -> Aff (err :: AWS.RequestError | eff) VirtualInterface
allocatePrivateVirtualInterface = AWS.request serviceName "AllocatePrivateVirtualInterface" 


-- | <p>Provisions a public virtual interface to be owned by a different customer.</p> <p>The owner of a connection calls this function to provision a public virtual interface which will be owned by another AWS customer.</p> <p>Virtual interfaces created using this function must be confirmed by the virtual interface owner by calling ConfirmPublicVirtualInterface. Until this step has been completed, the virtual interface will be in 'Confirming' state, and will not be available for handling traffic.</p> <p>When creating an IPv6 public virtual interface (addressFamily is 'ipv6'), the customer and amazon address fields should be left blank to use auto-assigned IPv6 space. Custom IPv6 Addresses are currently not supported.</p>
allocatePublicVirtualInterface :: forall eff. AllocatePublicVirtualInterfaceRequest -> Aff (err :: AWS.RequestError | eff) VirtualInterface
allocatePublicVirtualInterface = AWS.request serviceName "AllocatePublicVirtualInterface" 


-- | <p>Associates an existing connection with a link aggregation group (LAG). The connection is interrupted and re-established as a member of the LAG (connectivity to AWS will be interrupted). The connection must be hosted on the same AWS Direct Connect endpoint as the LAG, and its bandwidth must match the bandwidth for the LAG. You can reassociate a connection that's currently associated with a different LAG; however, if removing the connection will cause the original LAG to fall below its setting for minimum number of operational connections, the request fails.</p> <p>Any virtual interfaces that are directly associated with the connection are automatically re-associated with the LAG. If the connection was originally associated with a different LAG, the virtual interfaces remain associated with the original LAG.</p> <p>For interconnects, any hosted connections are automatically re-associated with the LAG. If the interconnect was originally associated with a different LAG, the hosted connections remain associated with the original LAG.</p>
associateConnectionWithLag :: forall eff. AssociateConnectionWithLagRequest -> Aff (err :: AWS.RequestError | eff) Connection
associateConnectionWithLag = AWS.request serviceName "AssociateConnectionWithLag" 


-- | <p>Associates a hosted connection and its virtual interfaces with a link aggregation group (LAG) or interconnect. If the target interconnect or LAG has an existing hosted connection with a conflicting VLAN number or IP address, the operation fails. This action temporarily interrupts the hosted connection's connectivity to AWS as it is being migrated.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
associateHostedConnection :: forall eff. AssociateHostedConnectionRequest -> Aff (err :: AWS.RequestError | eff) Connection
associateHostedConnection = AWS.request serviceName "AssociateHostedConnection" 


-- | <p>Associates a virtual interface with a specified link aggregation group (LAG) or connection. Connectivity to AWS is temporarily interrupted as the virtual interface is being migrated. If the target connection or LAG has an associated virtual interface with a conflicting VLAN number or a conflicting IP address, the operation fails. </p> <p>Virtual interfaces associated with a hosted connection cannot be associated with a LAG; hosted connections must be migrated along with their virtual interfaces using <a>AssociateHostedConnection</a>.</p> <p>In order to reassociate a virtual interface to a new connection or LAG, the requester must own either the virtual interface itself or the connection to which the virtual interface is currently associated. Additionally, the requester must own the connection or LAG to which the virtual interface will be newly associated.</p>
associateVirtualInterface :: forall eff. AssociateVirtualInterfaceRequest -> Aff (err :: AWS.RequestError | eff) VirtualInterface
associateVirtualInterface = AWS.request serviceName "AssociateVirtualInterface" 


-- | <p>Confirm the creation of a hosted connection on an interconnect.</p> <p>Upon creation, the hosted connection is initially in the 'Ordering' state, and will remain in this state until the owner calls ConfirmConnection to confirm creation of the hosted connection.</p>
confirmConnection :: forall eff. ConfirmConnectionRequest -> Aff (err :: AWS.RequestError | eff) ConfirmConnectionResponse
confirmConnection = AWS.request serviceName "ConfirmConnection" 


-- | <p>Accept ownership of a private virtual interface created by another customer.</p> <p>After the virtual interface owner calls this function, the virtual interface will be created and attached to the given virtual private gateway or direct connect gateway, and will be available for handling traffic.</p>
confirmPrivateVirtualInterface :: forall eff. ConfirmPrivateVirtualInterfaceRequest -> Aff (err :: AWS.RequestError | eff) ConfirmPrivateVirtualInterfaceResponse
confirmPrivateVirtualInterface = AWS.request serviceName "ConfirmPrivateVirtualInterface" 


-- | <p>Accept ownership of a public virtual interface created by another customer.</p> <p>After the virtual interface owner calls this function, the specified virtual interface will be created and made available for handling traffic.</p>
confirmPublicVirtualInterface :: forall eff. ConfirmPublicVirtualInterfaceRequest -> Aff (err :: AWS.RequestError | eff) ConfirmPublicVirtualInterfaceResponse
confirmPublicVirtualInterface = AWS.request serviceName "ConfirmPublicVirtualInterface" 


-- | <p>Creates a new BGP peer on a specified virtual interface. The BGP peer cannot be in the same address family (IPv4/IPv6) of an existing BGP peer on the virtual interface.</p> <p>You must create a BGP peer for the corresponding address family in order to access AWS resources that also use that address family.</p> <p>When creating a IPv6 BGP peer, the Amazon address and customer address fields must be left blank. IPv6 addresses are automatically assigned from Amazon's pool of IPv6 addresses; you cannot specify custom IPv6 addresses.</p> <p>For a public virtual interface, the Autonomous System Number (ASN) must be private or already whitelisted for the virtual interface.</p>
createBGPPeer :: forall eff. CreateBGPPeerRequest -> Aff (err :: AWS.RequestError | eff) CreateBGPPeerResponse
createBGPPeer = AWS.request serviceName "CreateBGPPeer" 


-- | <p>Creates a new connection between the customer network and a specific AWS Direct Connect location.</p> <p>A connection links your internal network to an AWS Direct Connect location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic cable. One end of the cable is connected to your router, the other to an AWS Direct Connect router. An AWS Direct Connect location provides access to Amazon Web Services in the region it is associated with. You can establish connections with AWS Direct Connect locations in multiple regions, but a connection in one region does not provide connectivity to other regions.</p> <p>To find the locations for your region, use <a>DescribeLocations</a>.</p> <p>You can automatically add the new connection to a link aggregation group (LAG) by specifying a LAG ID in the request. This ensures that the new connection is allocated on the same AWS Direct Connect endpoint that hosts the specified LAG. If there are no available ports on the endpoint, the request fails and no connection will be created.</p>
createConnection :: forall eff. CreateConnectionRequest -> Aff (err :: AWS.RequestError | eff) Connection
createConnection = AWS.request serviceName "CreateConnection" 


-- | <p>Creates a new direct connect gateway. A direct connect gateway is an intermediate object that enables you to connect a set of virtual interfaces and virtual private gateways. direct connect gateways are global and visible in any AWS region after they are created. The virtual interfaces and virtual private gateways that are connected through a direct connect gateway can be in different regions. This enables you to connect to a VPC in any region, regardless of the region in which the virtual interfaces are located, and pass traffic between them.</p>
createDirectConnectGateway :: forall eff. CreateDirectConnectGatewayRequest -> Aff (err :: AWS.RequestError | eff) CreateDirectConnectGatewayResult
createDirectConnectGateway = AWS.request serviceName "CreateDirectConnectGateway" 


-- | <p>Creates an association between a direct connect gateway and a virtual private gateway (VGW). The VGW must be attached to a VPC and must not be associated with another direct connect gateway.</p>
createDirectConnectGatewayAssociation :: forall eff. CreateDirectConnectGatewayAssociationRequest -> Aff (err :: AWS.RequestError | eff) CreateDirectConnectGatewayAssociationResult
createDirectConnectGatewayAssociation = AWS.request serviceName "CreateDirectConnectGatewayAssociation" 


-- | <p>Creates a new interconnect between a AWS Direct Connect partner's network and a specific AWS Direct Connect location.</p> <p>An interconnect is a connection which is capable of hosting other connections. The AWS Direct Connect partner can use an interconnect to provide sub-1Gbps AWS Direct Connect service to tier 2 customers who do not have their own connections. Like a standard connection, an interconnect links the AWS Direct Connect partner's network to an AWS Direct Connect location over a standard 1 Gbps or 10 Gbps Ethernet fiber-optic cable. One end is connected to the partner's router, the other to an AWS Direct Connect router.</p> <p>You can automatically add the new interconnect to a link aggregation group (LAG) by specifying a LAG ID in the request. This ensures that the new interconnect is allocated on the same AWS Direct Connect endpoint that hosts the specified LAG. If there are no available ports on the endpoint, the request fails and no interconnect will be created.</p> <p>For each end customer, the AWS Direct Connect partner provisions a connection on their interconnect by calling AllocateConnectionOnInterconnect. The end customer can then connect to AWS resources by creating a virtual interface on their connection, using the VLAN assigned to them by the AWS Direct Connect partner.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
createInterconnect :: forall eff. CreateInterconnectRequest -> Aff (err :: AWS.RequestError | eff) Interconnect
createInterconnect = AWS.request serviceName "CreateInterconnect" 


-- | <p>Creates a new link aggregation group (LAG) with the specified number of bundled physical connections between the customer network and a specific AWS Direct Connect location. A LAG is a logical interface that uses the Link Aggregation Control Protocol (LACP) to aggregate multiple 1 gigabit or 10 gigabit interfaces, allowing you to treat them as a single interface.</p> <p>All connections in a LAG must use the same bandwidth (for example, 10 Gbps), and must terminate at the same AWS Direct Connect endpoint.</p> <p>You can have up to 10 connections per LAG. Regardless of this limit, if you request more connections for the LAG than AWS Direct Connect can allocate on a single endpoint, no LAG is created.</p> <p>You can specify an existing physical connection or interconnect to include in the LAG (which counts towards the total number of connections). Doing so interrupts the current physical connection or hosted connections, and re-establishes them as a member of the LAG. The LAG will be created on the same AWS Direct Connect endpoint to which the connection terminates. Any virtual interfaces associated with the connection are automatically disassociated and re-associated with the LAG. The connection ID does not change.</p> <p>If the AWS account used to create a LAG is a registered AWS Direct Connect partner, the LAG is automatically enabled to host sub-connections. For a LAG owned by a partner, any associated virtual interfaces cannot be directly configured.</p>
createLag :: forall eff. CreateLagRequest -> Aff (err :: AWS.RequestError | eff) Lag
createLag = AWS.request serviceName "CreateLag" 


-- | <p>Creates a new private virtual interface. A virtual interface is the VLAN that transports AWS Direct Connect traffic. A private virtual interface supports sending traffic to a single virtual private cloud (VPC).</p>
createPrivateVirtualInterface :: forall eff. CreatePrivateVirtualInterfaceRequest -> Aff (err :: AWS.RequestError | eff) VirtualInterface
createPrivateVirtualInterface = AWS.request serviceName "CreatePrivateVirtualInterface" 


-- | <p>Creates a new public virtual interface. A virtual interface is the VLAN that transports AWS Direct Connect traffic. A public virtual interface supports sending traffic to public services of AWS such as Amazon Simple Storage Service (Amazon S3).</p> <p>When creating an IPv6 public virtual interface (addressFamily is 'ipv6'), the customer and amazon address fields should be left blank to use auto-assigned IPv6 space. Custom IPv6 Addresses are currently not supported.</p>
createPublicVirtualInterface :: forall eff. CreatePublicVirtualInterfaceRequest -> Aff (err :: AWS.RequestError | eff) VirtualInterface
createPublicVirtualInterface = AWS.request serviceName "CreatePublicVirtualInterface" 


-- | <p>Deletes a BGP peer on the specified virtual interface that matches the specified customer address and ASN. You cannot delete the last BGP peer from a virtual interface.</p>
deleteBGPPeer :: forall eff. DeleteBGPPeerRequest -> Aff (err :: AWS.RequestError | eff) DeleteBGPPeerResponse
deleteBGPPeer = AWS.request serviceName "DeleteBGPPeer" 


-- | <p>Deletes the connection.</p> <p>Deleting a connection only stops the AWS Direct Connect port hour and data transfer charges. You need to cancel separately with the providers any services or charges for cross-connects or network circuits that connect you to the AWS Direct Connect location.</p>
deleteConnection :: forall eff. DeleteConnectionRequest -> Aff (err :: AWS.RequestError | eff) Connection
deleteConnection = AWS.request serviceName "DeleteConnection" 


-- | <p>Deletes a direct connect gateway. You must first delete all virtual interfaces that are attached to the direct connect gateway and disassociate all virtual private gateways that are associated with the direct connect gateway.</p>
deleteDirectConnectGateway :: forall eff. DeleteDirectConnectGatewayRequest -> Aff (err :: AWS.RequestError | eff) DeleteDirectConnectGatewayResult
deleteDirectConnectGateway = AWS.request serviceName "DeleteDirectConnectGateway" 


-- | <p>Deletes the association between a direct connect gateway and a virtual private gateway.</p>
deleteDirectConnectGatewayAssociation :: forall eff. DeleteDirectConnectGatewayAssociationRequest -> Aff (err :: AWS.RequestError | eff) DeleteDirectConnectGatewayAssociationResult
deleteDirectConnectGatewayAssociation = AWS.request serviceName "DeleteDirectConnectGatewayAssociation" 


-- | <p>Deletes the specified interconnect.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
deleteInterconnect :: forall eff. DeleteInterconnectRequest -> Aff (err :: AWS.RequestError | eff) DeleteInterconnectResponse
deleteInterconnect = AWS.request serviceName "DeleteInterconnect" 


-- | <p>Deletes a link aggregation group (LAG). You cannot delete a LAG if it has active virtual interfaces or hosted connections.</p>
deleteLag :: forall eff. DeleteLagRequest -> Aff (err :: AWS.RequestError | eff) Lag
deleteLag = AWS.request serviceName "DeleteLag" 


-- | <p>Deletes a virtual interface.</p>
deleteVirtualInterface :: forall eff. DeleteVirtualInterfaceRequest -> Aff (err :: AWS.RequestError | eff) DeleteVirtualInterfaceResponse
deleteVirtualInterface = AWS.request serviceName "DeleteVirtualInterface" 


-- | <p>Deprecated in favor of <a>DescribeLoa</a>.</p> <p>Returns the LOA-CFA for a Connection.</p> <p>The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that your APN partner or service provider uses when establishing your cross connect to AWS at the colocation facility. For more information, see <a href="http://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html">Requesting Cross Connects at AWS Direct Connect Locations</a> in the AWS Direct Connect user guide.</p>
describeConnectionLoa :: forall eff. DescribeConnectionLoaRequest -> Aff (err :: AWS.RequestError | eff) DescribeConnectionLoaResponse
describeConnectionLoa = AWS.request serviceName "DescribeConnectionLoa" 


-- | <p>Displays all connections in this region.</p> <p>If a connection ID is provided, the call returns only that particular connection.</p>
describeConnections :: forall eff. DescribeConnectionsRequest -> Aff (err :: AWS.RequestError | eff) Connections
describeConnections = AWS.request serviceName "DescribeConnections" 


-- | <p>Deprecated in favor of <a>DescribeHostedConnections</a>.</p> <p>Returns a list of connections that have been provisioned on the given interconnect.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
describeConnectionsOnInterconnect :: forall eff. DescribeConnectionsOnInterconnectRequest -> Aff (err :: AWS.RequestError | eff) Connections
describeConnectionsOnInterconnect = AWS.request serviceName "DescribeConnectionsOnInterconnect" 


-- | <p>Returns a list of all direct connect gateway and virtual private gateway (VGW) associations. Either a direct connect gateway ID or a VGW ID must be provided in the request. If a direct connect gateway ID is provided, the response returns all VGWs associated with the direct connect gateway. If a VGW ID is provided, the response returns all direct connect gateways associated with the VGW. If both are provided, the response only returns the association that matches both the direct connect gateway and the VGW.</p>
describeDirectConnectGatewayAssociations :: forall eff. DescribeDirectConnectGatewayAssociationsRequest -> Aff (err :: AWS.RequestError | eff) DescribeDirectConnectGatewayAssociationsResult
describeDirectConnectGatewayAssociations = AWS.request serviceName "DescribeDirectConnectGatewayAssociations" 


-- | <p>Returns a list of all direct connect gateway and virtual interface (VIF) attachments. Either a direct connect gateway ID or a VIF ID must be provided in the request. If a direct connect gateway ID is provided, the response returns all VIFs attached to the direct connect gateway. If a VIF ID is provided, the response returns all direct connect gateways attached to the VIF. If both are provided, the response only returns the attachment that matches both the direct connect gateway and the VIF.</p>
describeDirectConnectGatewayAttachments :: forall eff. DescribeDirectConnectGatewayAttachmentsRequest -> Aff (err :: AWS.RequestError | eff) DescribeDirectConnectGatewayAttachmentsResult
describeDirectConnectGatewayAttachments = AWS.request serviceName "DescribeDirectConnectGatewayAttachments" 


-- | <p>Returns a list of direct connect gateways in your account. Deleted direct connect gateways are not returned. You can provide a direct connect gateway ID in the request to return information about the specific direct connect gateway only. Otherwise, if a direct connect gateway ID is not provided, information about all of your direct connect gateways is returned. </p>
describeDirectConnectGateways :: forall eff. DescribeDirectConnectGatewaysRequest -> Aff (err :: AWS.RequestError | eff) DescribeDirectConnectGatewaysResult
describeDirectConnectGateways = AWS.request serviceName "DescribeDirectConnectGateways" 


-- | <p>Returns a list of hosted connections that have been provisioned on the given interconnect or link aggregation group (LAG).</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
describeHostedConnections :: forall eff. DescribeHostedConnectionsRequest -> Aff (err :: AWS.RequestError | eff) Connections
describeHostedConnections = AWS.request serviceName "DescribeHostedConnections" 


-- | <p>Deprecated in favor of <a>DescribeLoa</a>.</p> <p>Returns the LOA-CFA for an Interconnect.</p> <p>The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that is used when establishing your cross connect to AWS at the colocation facility. For more information, see <a href="http://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html">Requesting Cross Connects at AWS Direct Connect Locations</a> in the AWS Direct Connect user guide.</p>
describeInterconnectLoa :: forall eff. DescribeInterconnectLoaRequest -> Aff (err :: AWS.RequestError | eff) DescribeInterconnectLoaResponse
describeInterconnectLoa = AWS.request serviceName "DescribeInterconnectLoa" 


-- | <p>Returns a list of interconnects owned by the AWS account.</p> <p>If an interconnect ID is provided, it will only return this particular interconnect.</p>
describeInterconnects :: forall eff. DescribeInterconnectsRequest -> Aff (err :: AWS.RequestError | eff) Interconnects
describeInterconnects = AWS.request serviceName "DescribeInterconnects" 


-- | <p>Describes the link aggregation groups (LAGs) in your account. </p> <p>If a LAG ID is provided, only information about the specified LAG is returned.</p>
describeLags :: forall eff. DescribeLagsRequest -> Aff (err :: AWS.RequestError | eff) Lags
describeLags = AWS.request serviceName "DescribeLags" 


-- | <p>Returns the LOA-CFA for a connection, interconnect, or link aggregation group (LAG).</p> <p>The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that is used when establishing your cross connect to AWS at the colocation facility. For more information, see <a href="http://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html">Requesting Cross Connects at AWS Direct Connect Locations</a> in the AWS Direct Connect user guide.</p>
describeLoa :: forall eff. DescribeLoaRequest -> Aff (err :: AWS.RequestError | eff) Loa
describeLoa = AWS.request serviceName "DescribeLoa" 


-- | <p>Returns the list of AWS Direct Connect locations in the current AWS region. These are the locations that may be selected when calling <a>CreateConnection</a> or <a>CreateInterconnect</a>.</p>
describeLocations :: forall eff.  Aff (err :: AWS.RequestError | eff) Locations
describeLocations = AWS.request serviceName "DescribeLocations" unit


-- | <p>Describes the tags associated with the specified Direct Connect resources.</p>
describeTags :: forall eff. DescribeTagsRequest -> Aff (err :: AWS.RequestError | eff) DescribeTagsResponse
describeTags = AWS.request serviceName "DescribeTags" 


-- | <p>Returns a list of virtual private gateways owned by the AWS account.</p> <p>You can create one or more AWS Direct Connect private virtual interfaces linking to a virtual private gateway. A virtual private gateway can be managed via Amazon Virtual Private Cloud (VPC) console or the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html">EC2 CreateVpnGateway</a> action.</p>
describeVirtualGateways :: forall eff.  Aff (err :: AWS.RequestError | eff) VirtualGateways
describeVirtualGateways = AWS.request serviceName "DescribeVirtualGateways" unit


-- | <p>Displays all virtual interfaces for an AWS account. Virtual interfaces deleted fewer than 15 minutes before you make the request are also returned. If you specify a connection ID, only the virtual interfaces associated with the connection are returned. If you specify a virtual interface ID, then only a single virtual interface is returned.</p> <p>A virtual interface (VLAN) transmits the traffic between the AWS Direct Connect location and the customer.</p>
describeVirtualInterfaces :: forall eff. DescribeVirtualInterfacesRequest -> Aff (err :: AWS.RequestError | eff) VirtualInterfaces
describeVirtualInterfaces = AWS.request serviceName "DescribeVirtualInterfaces" 


-- | <p>Disassociates a connection from a link aggregation group (LAG). The connection is interrupted and re-established as a standalone connection (the connection is not deleted; to delete the connection, use the <a>DeleteConnection</a> request). If the LAG has associated virtual interfaces or hosted connections, they remain associated with the LAG. A disassociated connection owned by an AWS Direct Connect partner is automatically converted to an interconnect.</p> <p>If disassociating the connection will cause the LAG to fall below its setting for minimum number of operational connections, the request fails, except when it's the last member of the LAG. If all connections are disassociated, the LAG continues to exist as an empty LAG with no physical connections. </p>
disassociateConnectionFromLag :: forall eff. DisassociateConnectionFromLagRequest -> Aff (err :: AWS.RequestError | eff) Connection
disassociateConnectionFromLag = AWS.request serviceName "DisassociateConnectionFromLag" 


-- | <p>Adds the specified tags to the specified Direct Connect resource. Each Direct Connect resource can have a maximum of 50 tags.</p> <p>Each tag consists of a key and an optional value. If a tag with the same key is already associated with the Direct Connect resource, this action updates its value.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (err :: AWS.RequestError | eff) TagResourceResponse
tagResource = AWS.request serviceName "TagResource" 


-- | <p>Removes one or more tags from the specified Direct Connect resource.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: AWS.RequestError | eff) UntagResourceResponse
untagResource = AWS.request serviceName "UntagResource" 


-- | <p>Updates the attributes of a link aggregation group (LAG). </p> <p>You can update the following attributes: </p> <ul> <li> <p>The name of the LAG.</p> </li> <li> <p>The value for the minimum number of connections that must be operational for the LAG itself to be operational. </p> </li> </ul> <p>When you create a LAG, the default value for the minimum number of operational connections is zero (0). If you update this value, and the number of operational connections falls below the specified value, the LAG will automatically go down to avoid overutilization of the remaining connections. Adjusting this value should be done with care as it could force the LAG down if the value is set higher than the current number of operational connections.</p>
updateLag :: forall eff. UpdateLagRequest -> Aff (err :: AWS.RequestError | eff) Lag
updateLag = AWS.request serviceName "UpdateLag" 


-- | <p>The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.</p> <p>Example: 65000</p>
newtype ASN = ASN Int


-- | <p>Indicates the address family for the BGP peer.</p> <ul> <li> <p> <b>ipv4</b>: IPv4 address family</p> </li> <li> <p> <b>ipv6</b>: IPv6 address family</p> </li> </ul>
newtype AddressFamily = AddressFamily String


-- | <p>Container for the parameters to the AllocateConnectionOnInterconnect operation.</p>
newtype AllocateConnectionOnInterconnectRequest = AllocateConnectionOnInterconnectRequest 
  { "Bandwidth'" :: (Bandwidth)
  , "ConnectionName'" :: (ConnectionName)
  , "OwnerAccount'" :: (OwnerAccount)
  , "InterconnectId'" :: (InterconnectId)
  , "Vlan'" :: (VLAN)
  }


-- | <p>Container for the parameters to theHostedConnection operation.</p>
newtype AllocateHostedConnectionRequest = AllocateHostedConnectionRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "OwnerAccount'" :: (OwnerAccount)
  , "Bandwidth'" :: (Bandwidth)
  , "ConnectionName'" :: (ConnectionName)
  , "Vlan'" :: (VLAN)
  }


-- | <p>Container for the parameters to the AllocatePrivateVirtualInterface operation.</p>
newtype AllocatePrivateVirtualInterfaceRequest = AllocatePrivateVirtualInterfaceRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "OwnerAccount'" :: (OwnerAccount)
  , "NewPrivateVirtualInterfaceAllocation'" :: (NewPrivateVirtualInterfaceAllocation)
  }


-- | <p>Container for the parameters to the AllocatePublicVirtualInterface operation.</p>
newtype AllocatePublicVirtualInterfaceRequest = AllocatePublicVirtualInterfaceRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "OwnerAccount'" :: (OwnerAccount)
  , "NewPublicVirtualInterfaceAllocation'" :: (NewPublicVirtualInterfaceAllocation)
  }


-- | <p>IP address assigned to the Amazon interface.</p> <p>Example: 192.168.1.1/30 or 2001:db8::1/125</p>
newtype AmazonAddress = AmazonAddress String


-- | <p>Container for the parameters to the AssociateConnectionWithLag operation.</p>
newtype AssociateConnectionWithLagRequest = AssociateConnectionWithLagRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "LagId'" :: (LagId)
  }


-- | <p>Container for the parameters to the AssociateHostedConnection operation.</p>
newtype AssociateHostedConnectionRequest = AssociateHostedConnectionRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "ParentConnectionId'" :: (ConnectionId)
  }


-- | <p>Container for the parameters to the AssociateVirtualInterface operation.</p>
newtype AssociateVirtualInterfaceRequest = AssociateVirtualInterfaceRequest 
  { "VirtualInterfaceId'" :: (VirtualInterfaceId)
  , "ConnectionId'" :: (ConnectionId)
  }


-- | <p>An abstract ID for the physical Direct Connect endpoint.</p> <p>Example: EQC50-abcdef123456</p>
newtype AwsDevice = AwsDevice String


-- | <p>The authentication key for BGP configuration.</p> <p>Example: asdf34example</p>
newtype BGPAuthKey = BGPAuthKey String


-- | <p>A structure containing information about a BGP peer.</p>
newtype BGPPeer = BGPPeer 
  { "Asn'" :: NullOrUndefined (ASN)
  , "AuthKey'" :: NullOrUndefined (BGPAuthKey)
  , "AddressFamily'" :: NullOrUndefined (AddressFamily)
  , "AmazonAddress'" :: NullOrUndefined (AmazonAddress)
  , "CustomerAddress'" :: NullOrUndefined (CustomerAddress)
  , "BgpPeerState'" :: NullOrUndefined (BGPPeerState)
  , "BgpStatus'" :: NullOrUndefined (BGPStatus)
  }


-- | <p>A list of the BGP peers configured on this virtual interface.</p>
newtype BGPPeerList = BGPPeerList (Array BGPPeer)


-- | <p>The state of the BGP peer.</p> <ul> <li> <p> <b>Verifying</b>: The BGP peering addresses or ASN require validation before the BGP peer can be created. This state only applies to BGP peers on a public virtual interface. </p> </li> <li> <p> <b>Pending</b>: The BGP peer has been created, and is in this state until it is ready to be established.</p> </li> <li> <p> <b>Available</b>: The BGP peer can be established.</p> </li> <li> <p> <b>Deleting</b>: The BGP peer is in the process of being deleted.</p> </li> <li> <p> <b>Deleted</b>: The BGP peer has been deleted and cannot be established.</p> </li> </ul>
newtype BGPPeerState = BGPPeerState String


-- | <p>The Up/Down state of the BGP peer.</p> <ul> <li> <p> <b>Up</b>: The BGP peer is established.</p> </li> <li> <p> <b>Down</b>: The BGP peer is down.</p> </li> </ul>
newtype BGPStatus = BGPStatus String


-- | <p>Bandwidth of the connection.</p> <p>Example: 1Gbps</p> <p>Default: None</p>
newtype Bandwidth = Bandwidth String


newtype BooleanFlag = BooleanFlag Boolean


newtype CIDR = CIDR String


-- | <p>Container for the parameters to the ConfirmConnection operation.</p>
newtype ConfirmConnectionRequest = ConfirmConnectionRequest 
  { "ConnectionId'" :: (ConnectionId)
  }


-- | <p>The response received when ConfirmConnection is called.</p>
newtype ConfirmConnectionResponse = ConfirmConnectionResponse 
  { "ConnectionState'" :: NullOrUndefined (ConnectionState)
  }


-- | <p>Container for the parameters to the ConfirmPrivateVirtualInterface operation.</p>
newtype ConfirmPrivateVirtualInterfaceRequest = ConfirmPrivateVirtualInterfaceRequest 
  { "VirtualInterfaceId'" :: (VirtualInterfaceId)
  , "VirtualGatewayId'" :: NullOrUndefined (VirtualGatewayId)
  , "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId)
  }


-- | <p>The response received when ConfirmPrivateVirtualInterface is called.</p>
newtype ConfirmPrivateVirtualInterfaceResponse = ConfirmPrivateVirtualInterfaceResponse 
  { "VirtualInterfaceState'" :: NullOrUndefined (VirtualInterfaceState)
  }


-- | <p>Container for the parameters to the ConfirmPublicVirtualInterface operation.</p>
newtype ConfirmPublicVirtualInterfaceRequest = ConfirmPublicVirtualInterfaceRequest 
  { "VirtualInterfaceId'" :: (VirtualInterfaceId)
  }


-- | <p>The response received when ConfirmPublicVirtualInterface is called.</p>
newtype ConfirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse 
  { "VirtualInterfaceState'" :: NullOrUndefined (VirtualInterfaceState)
  }


-- | <p>A connection represents the physical network connection between the AWS Direct Connect location and the customer.</p>
newtype Connection = Connection 
  { "OwnerAccount'" :: NullOrUndefined (OwnerAccount)
  , "ConnectionId'" :: NullOrUndefined (ConnectionId)
  , "ConnectionName'" :: NullOrUndefined (ConnectionName)
  , "ConnectionState'" :: NullOrUndefined (ConnectionState)
  , "Region'" :: NullOrUndefined (Region)
  , "Location'" :: NullOrUndefined (LocationCode)
  , "Bandwidth'" :: NullOrUndefined (Bandwidth)
  , "Vlan'" :: NullOrUndefined (VLAN)
  , "PartnerName'" :: NullOrUndefined (PartnerName)
  , "LoaIssueTime'" :: NullOrUndefined (LoaIssueTime)
  , "LagId'" :: NullOrUndefined (LagId)
  , "AwsDevice'" :: NullOrUndefined (AwsDevice)
  }


-- | <p>The ID of the connection. This field is also used as the ID type for operations that use multiple connection types (LAG, interconnect, and/or connection).</p> <p>Example: dxcon-fg5678gh</p> <p>Default: None</p>
newtype ConnectionId = ConnectionId String


-- | <p>A list of connections.</p>
newtype ConnectionList = ConnectionList (Array Connection)


-- | <p>The name of the connection.</p> <p>Example: "<i>My Connection to AWS</i>"</p> <p>Default: None</p>
newtype ConnectionName = ConnectionName String


-- | <p>State of the connection.</p> <ul> <li> <p> <b>Ordering</b>: The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.</p> </li> <li> <p> <b>Requested</b>: The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.</p> </li> <li> <p> <b>Pending</b>: The connection has been approved, and is being initialized.</p> </li> <li> <p> <b>Available</b>: The network link is up, and the connection is ready for use.</p> </li> <li> <p> <b>Down</b>: The network link is down.</p> </li> <li> <p> <b>Deleting</b>: The connection is in the process of being deleted.</p> </li> <li> <p> <b>Deleted</b>: The connection has been deleted.</p> </li> <li> <p> <b>Rejected</b>: A hosted connection in the 'Ordering' state will enter the 'Rejected' state if it is deleted by the end customer.</p> </li> </ul>
newtype ConnectionState = ConnectionState String


-- | <p>A structure containing a list of connections.</p>
newtype Connections = Connections 
  { "Connections'" :: NullOrUndefined (ConnectionList)
  }


newtype Count = Count Int


-- | <p>Container for the parameters to the CreateBGPPeer operation.</p>
newtype CreateBGPPeerRequest = CreateBGPPeerRequest 
  { "VirtualInterfaceId'" :: NullOrUndefined (VirtualInterfaceId)
  , "NewBGPPeer'" :: NullOrUndefined (NewBGPPeer)
  }


-- | <p>The response received when CreateBGPPeer is called.</p>
newtype CreateBGPPeerResponse = CreateBGPPeerResponse 
  { "VirtualInterface'" :: NullOrUndefined (VirtualInterface)
  }


-- | <p>Container for the parameters to the CreateConnection operation.</p>
newtype CreateConnectionRequest = CreateConnectionRequest 
  { "Location'" :: (LocationCode)
  , "Bandwidth'" :: (Bandwidth)
  , "ConnectionName'" :: (ConnectionName)
  , "LagId'" :: NullOrUndefined (LagId)
  }


-- | <p>Container for the parameters to the CreateDirectConnectGatewayAssociation operation.</p>
newtype CreateDirectConnectGatewayAssociationRequest = CreateDirectConnectGatewayAssociationRequest 
  { "DirectConnectGatewayId'" :: (DirectConnectGatewayId)
  , "VirtualGatewayId'" :: (VirtualGatewayId)
  }


-- | <p>Container for the response from the CreateDirectConnectGatewayAssociation API call</p>
newtype CreateDirectConnectGatewayAssociationResult = CreateDirectConnectGatewayAssociationResult 
  { "DirectConnectGatewayAssociation'" :: NullOrUndefined (DirectConnectGatewayAssociation)
  }


-- | <p>Container for the parameters to the CreateDirectConnectGateway operation.</p>
newtype CreateDirectConnectGatewayRequest = CreateDirectConnectGatewayRequest 
  { "DirectConnectGatewayName'" :: (DirectConnectGatewayName)
  , "AmazonSideAsn'" :: NullOrUndefined (LongAsn)
  }


-- | <p>Container for the response from the CreateDirectConnectGateway API call</p>
newtype CreateDirectConnectGatewayResult = CreateDirectConnectGatewayResult 
  { "DirectConnectGateway'" :: NullOrUndefined (DirectConnectGateway)
  }


-- | <p>Container for the parameters to the CreateInterconnect operation.</p>
newtype CreateInterconnectRequest = CreateInterconnectRequest 
  { "InterconnectName'" :: (InterconnectName)
  , "Bandwidth'" :: (Bandwidth)
  , "Location'" :: (LocationCode)
  , "LagId'" :: NullOrUndefined (LagId)
  }


-- | <p>Container for the parameters to the CreateLag operation.</p>
newtype CreateLagRequest = CreateLagRequest 
  { "NumberOfConnections'" :: (Count)
  , "Location'" :: (LocationCode)
  , "ConnectionsBandwidth'" :: (Bandwidth)
  , "LagName'" :: (LagName)
  , "ConnectionId'" :: NullOrUndefined (ConnectionId)
  }


-- | <p>Container for the parameters to the CreatePrivateVirtualInterface operation.</p>
newtype CreatePrivateVirtualInterfaceRequest = CreatePrivateVirtualInterfaceRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "NewPrivateVirtualInterface'" :: (NewPrivateVirtualInterface)
  }


-- | <p>Container for the parameters to the CreatePublicVirtualInterface operation.</p>
newtype CreatePublicVirtualInterfaceRequest = CreatePublicVirtualInterfaceRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "NewPublicVirtualInterface'" :: (NewPublicVirtualInterface)
  }


-- | <p>IP address assigned to the customer interface.</p> <p>Example: 192.168.1.2/30 or 2001:db8::2/125</p>
newtype CustomerAddress = CustomerAddress String


-- | <p>Container for the parameters to the DeleteBGPPeer operation.</p>
newtype DeleteBGPPeerRequest = DeleteBGPPeerRequest 
  { "VirtualInterfaceId'" :: NullOrUndefined (VirtualInterfaceId)
  , "Asn'" :: NullOrUndefined (ASN)
  , "CustomerAddress'" :: NullOrUndefined (CustomerAddress)
  }


-- | <p>The response received when DeleteBGPPeer is called.</p>
newtype DeleteBGPPeerResponse = DeleteBGPPeerResponse 
  { "VirtualInterface'" :: NullOrUndefined (VirtualInterface)
  }


-- | <p>Container for the parameters to the DeleteConnection operation.</p>
newtype DeleteConnectionRequest = DeleteConnectionRequest 
  { "ConnectionId'" :: (ConnectionId)
  }


-- | <p>Container for the parameters to the DeleteDirectConnectGatewayAssociation operation.</p>
newtype DeleteDirectConnectGatewayAssociationRequest = DeleteDirectConnectGatewayAssociationRequest 
  { "DirectConnectGatewayId'" :: (DirectConnectGatewayId)
  , "VirtualGatewayId'" :: (VirtualGatewayId)
  }


-- | <p>Container for the response from the DeleteDirectConnectGatewayAssociation API call</p>
newtype DeleteDirectConnectGatewayAssociationResult = DeleteDirectConnectGatewayAssociationResult 
  { "DirectConnectGatewayAssociation'" :: NullOrUndefined (DirectConnectGatewayAssociation)
  }


-- | <p>Container for the parameters to the DeleteDirectConnectGateway operation.</p>
newtype DeleteDirectConnectGatewayRequest = DeleteDirectConnectGatewayRequest 
  { "DirectConnectGatewayId'" :: (DirectConnectGatewayId)
  }


-- | <p>Container for the response from the DeleteDirectConnectGateway API call</p>
newtype DeleteDirectConnectGatewayResult = DeleteDirectConnectGatewayResult 
  { "DirectConnectGateway'" :: NullOrUndefined (DirectConnectGateway)
  }


-- | <p>Container for the parameters to the DeleteInterconnect operation.</p>
newtype DeleteInterconnectRequest = DeleteInterconnectRequest 
  { "InterconnectId'" :: (InterconnectId)
  }


-- | <p>The response received when DeleteInterconnect is called.</p>
newtype DeleteInterconnectResponse = DeleteInterconnectResponse 
  { "InterconnectState'" :: NullOrUndefined (InterconnectState)
  }


-- | <p>Container for the parameters to the DeleteLag operation.</p>
newtype DeleteLagRequest = DeleteLagRequest 
  { "LagId'" :: (LagId)
  }


-- | <p>Container for the parameters to the DeleteVirtualInterface operation.</p>
newtype DeleteVirtualInterfaceRequest = DeleteVirtualInterfaceRequest 
  { "VirtualInterfaceId'" :: (VirtualInterfaceId)
  }


-- | <p>The response received when DeleteVirtualInterface is called.</p>
newtype DeleteVirtualInterfaceResponse = DeleteVirtualInterfaceResponse 
  { "VirtualInterfaceState'" :: NullOrUndefined (VirtualInterfaceState)
  }


-- | <p>Container for the parameters to the DescribeConnectionLoa operation.</p>
newtype DescribeConnectionLoaRequest = DescribeConnectionLoaRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "ProviderName'" :: NullOrUndefined (ProviderName)
  , "LoaContentType'" :: NullOrUndefined (LoaContentType)
  }


-- | <p>The response received when DescribeConnectionLoa is called.</p>
newtype DescribeConnectionLoaResponse = DescribeConnectionLoaResponse 
  { "Loa'" :: NullOrUndefined (Loa)
  }


-- | <p>Container for the parameters to the DescribeConnectionsOnInterconnect operation.</p>
newtype DescribeConnectionsOnInterconnectRequest = DescribeConnectionsOnInterconnectRequest 
  { "InterconnectId'" :: (InterconnectId)
  }


-- | <p>Container for the parameters to the DescribeConnections operation.</p>
newtype DescribeConnectionsRequest = DescribeConnectionsRequest 
  { "ConnectionId'" :: NullOrUndefined (ConnectionId)
  }


-- | <p>Container for the parameters to the DescribeDirectConnectGatewayAssociations operation.</p>
newtype DescribeDirectConnectGatewayAssociationsRequest = DescribeDirectConnectGatewayAssociationsRequest 
  { "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId)
  , "VirtualGatewayId'" :: NullOrUndefined (VirtualGatewayId)
  , "MaxResults'" :: NullOrUndefined (MaxResultSetSize)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Container for the response from the DescribeDirectConnectGatewayAssociations API call</p>
newtype DescribeDirectConnectGatewayAssociationsResult = DescribeDirectConnectGatewayAssociationsResult 
  { "DirectConnectGatewayAssociations'" :: NullOrUndefined (DirectConnectGatewayAssociationList)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Container for the parameters to the DescribeDirectConnectGatewayAttachments operation.</p>
newtype DescribeDirectConnectGatewayAttachmentsRequest = DescribeDirectConnectGatewayAttachmentsRequest 
  { "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId)
  , "VirtualInterfaceId'" :: NullOrUndefined (VirtualInterfaceId)
  , "MaxResults'" :: NullOrUndefined (MaxResultSetSize)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Container for the response from the DescribeDirectConnectGatewayAttachments API call</p>
newtype DescribeDirectConnectGatewayAttachmentsResult = DescribeDirectConnectGatewayAttachmentsResult 
  { "DirectConnectGatewayAttachments'" :: NullOrUndefined (DirectConnectGatewayAttachmentList)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Container for the parameters to the DescribeDirectConnectGateways operation.</p>
newtype DescribeDirectConnectGatewaysRequest = DescribeDirectConnectGatewaysRequest 
  { "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId)
  , "MaxResults'" :: NullOrUndefined (MaxResultSetSize)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Container for the response from the DescribeDirectConnectGateways API call</p>
newtype DescribeDirectConnectGatewaysResult = DescribeDirectConnectGatewaysResult 
  { "DirectConnectGateways'" :: NullOrUndefined (DirectConnectGatewayList)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Container for the parameters to the DescribeHostedConnections operation.</p>
newtype DescribeHostedConnectionsRequest = DescribeHostedConnectionsRequest 
  { "ConnectionId'" :: (ConnectionId)
  }


-- | <p>Container for the parameters to the DescribeInterconnectLoa operation.</p>
newtype DescribeInterconnectLoaRequest = DescribeInterconnectLoaRequest 
  { "InterconnectId'" :: (InterconnectId)
  , "ProviderName'" :: NullOrUndefined (ProviderName)
  , "LoaContentType'" :: NullOrUndefined (LoaContentType)
  }


-- | <p>The response received when DescribeInterconnectLoa is called.</p>
newtype DescribeInterconnectLoaResponse = DescribeInterconnectLoaResponse 
  { "Loa'" :: NullOrUndefined (Loa)
  }


-- | <p>Container for the parameters to the DescribeInterconnects operation.</p>
newtype DescribeInterconnectsRequest = DescribeInterconnectsRequest 
  { "InterconnectId'" :: NullOrUndefined (InterconnectId)
  }


-- | <p>Container for the parameters to the DescribeLags operation.</p>
newtype DescribeLagsRequest = DescribeLagsRequest 
  { "LagId'" :: NullOrUndefined (LagId)
  }


-- | <p>Container for the parameters to the DescribeLoa operation.</p>
newtype DescribeLoaRequest = DescribeLoaRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "ProviderName'" :: NullOrUndefined (ProviderName)
  , "LoaContentType'" :: NullOrUndefined (LoaContentType)
  }


-- | <p>Container for the parameters to the DescribeTags operation.</p>
newtype DescribeTagsRequest = DescribeTagsRequest 
  { "ResourceArns'" :: (ResourceArnList)
  }


-- | <p>The response received when DescribeTags is called.</p>
newtype DescribeTagsResponse = DescribeTagsResponse 
  { "ResourceTags'" :: NullOrUndefined (ResourceTagList)
  }


-- | <p>Container for the parameters to the DescribeVirtualInterfaces operation.</p>
newtype DescribeVirtualInterfacesRequest = DescribeVirtualInterfacesRequest 
  { "ConnectionId'" :: NullOrUndefined (ConnectionId)
  , "VirtualInterfaceId'" :: NullOrUndefined (VirtualInterfaceId)
  }


-- | <p>The API was called with invalid parameters. The error message will contain additional details about the cause.</p>
newtype DirectConnectClientException = DirectConnectClientException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>A direct connect gateway is an intermediate object that enables you to connect virtual interfaces and virtual private gateways.</p>
newtype DirectConnectGateway = DirectConnectGateway 
  { "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId)
  , "DirectConnectGatewayName'" :: NullOrUndefined (DirectConnectGatewayName)
  , "AmazonSideAsn'" :: NullOrUndefined (LongAsn)
  , "OwnerAccount'" :: NullOrUndefined (OwnerAccount)
  , "DirectConnectGatewayState'" :: NullOrUndefined (DirectConnectGatewayState)
  , "StateChangeError'" :: NullOrUndefined (StateChangeError)
  }


-- | <p>The association between a direct connect gateway and virtual private gateway.</p>
newtype DirectConnectGatewayAssociation = DirectConnectGatewayAssociation 
  { "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId)
  , "VirtualGatewayId'" :: NullOrUndefined (VirtualGatewayId)
  , "VirtualGatewayRegion'" :: NullOrUndefined (VirtualGatewayRegion)
  , "VirtualGatewayOwnerAccount'" :: NullOrUndefined (OwnerAccount)
  , "AssociationState'" :: NullOrUndefined (DirectConnectGatewayAssociationState)
  , "StateChangeError'" :: NullOrUndefined (StateChangeError)
  }


-- | <p>A list of direct connect gateway associations.</p>
newtype DirectConnectGatewayAssociationList = DirectConnectGatewayAssociationList (Array DirectConnectGatewayAssociation)


-- | <p>State of the direct connect gateway association.</p> <ul> <li> <p> <b>Associating</b>: The initial state after calling <a>CreateDirectConnectGatewayAssociation</a>.</p> </li> <li> <p> <b>Associated</b>: The direct connect gateway and virtual private gateway are successfully associated and ready to pass traffic.</p> </li> <li> <p> <b>Disassociating</b>: The initial state after calling <a>DeleteDirectConnectGatewayAssociation</a>.</p> </li> <li> <p> <b>Disassociated</b>: The virtual private gateway is successfully disassociated from the direct connect gateway. Traffic flow between the direct connect gateway and virtual private gateway stops.</p> </li> </ul>
newtype DirectConnectGatewayAssociationState = DirectConnectGatewayAssociationState String


-- | <p>The association between a direct connect gateway and virtual interface.</p>
newtype DirectConnectGatewayAttachment = DirectConnectGatewayAttachment 
  { "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId)
  , "VirtualInterfaceId'" :: NullOrUndefined (VirtualInterfaceId)
  , "VirtualInterfaceRegion'" :: NullOrUndefined (VirtualInterfaceRegion)
  , "VirtualInterfaceOwnerAccount'" :: NullOrUndefined (OwnerAccount)
  , "AttachmentState'" :: NullOrUndefined (DirectConnectGatewayAttachmentState)
  , "StateChangeError'" :: NullOrUndefined (StateChangeError)
  }


-- | <p>A list of direct connect gateway attachments.</p>
newtype DirectConnectGatewayAttachmentList = DirectConnectGatewayAttachmentList (Array DirectConnectGatewayAttachment)


-- | <p>State of the direct connect gateway attachment.</p> <ul> <li> <p> <b>Attaching</b>: The initial state after a virtual interface is created using the direct connect gateway.</p> </li> <li> <p> <b>Attached</b>: The direct connect gateway and virtual interface are successfully attached and ready to pass traffic.</p> </li> <li> <p> <b>Detaching</b>: The initial state after calling <a>DeleteVirtualInterface</a> on a virtual interface that is attached to a direct connect gateway.</p> </li> <li> <p> <b>Detached</b>: The virtual interface is successfully detached from the direct connect gateway. Traffic flow between the direct connect gateway and virtual interface stops.</p> </li> </ul>
newtype DirectConnectGatewayAttachmentState = DirectConnectGatewayAttachmentState String


-- | <p>The ID of the direct connect gateway.</p> <p>Example: "abcd1234-dcba-5678-be23-cdef9876ab45"</p>
newtype DirectConnectGatewayId = DirectConnectGatewayId String


-- | <p>A list of direct connect gateways.</p>
newtype DirectConnectGatewayList = DirectConnectGatewayList (Array DirectConnectGateway)


-- | <p>The name of the direct connect gateway.</p> <p>Example: "My direct connect gateway"</p> <p>Default: None</p>
newtype DirectConnectGatewayName = DirectConnectGatewayName String


-- | <p>State of the direct connect gateway.</p> <ul> <li> <p> <b>Pending</b>: The initial state after calling <a>CreateDirectConnectGateway</a>.</p> </li> <li> <p> <b>Available</b>: The direct connect gateway is ready for use.</p> </li> <li> <p> <b>Deleting</b>: The initial state after calling <a>DeleteDirectConnectGateway</a>.</p> </li> <li> <p> <b>Deleted</b>: The direct connect gateway is deleted and cannot pass traffic.</p> </li> </ul>
newtype DirectConnectGatewayState = DirectConnectGatewayState String


-- | <p>A server-side error occurred during the API call. The error message will contain additional details about the cause.</p>
newtype DirectConnectServerException = DirectConnectServerException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Container for the parameters to the DisassociateConnectionFromLag operation.</p>
newtype DisassociateConnectionFromLagRequest = DisassociateConnectionFromLagRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "LagId'" :: (LagId)
  }


-- | <p>A tag key was specified more than once.</p>
newtype DuplicateTagKeysException = DuplicateTagKeysException 
  { 
  }


newtype ErrorMessage = ErrorMessage String


-- | <p>An interconnect is a connection that can host other connections.</p> <p>Like a standard AWS Direct Connect connection, an interconnect represents the physical connection between an AWS Direct Connect partner's network and a specific Direct Connect location. An AWS Direct Connect partner who owns an interconnect can provision hosted connections on the interconnect for their end customers, thereby providing the end customers with connectivity to AWS services.</p> <p>The resources of the interconnect, including bandwidth and VLAN numbers, are shared by all of the hosted connections on the interconnect, and the owner of the interconnect determines how these resources are assigned.</p>
newtype Interconnect = Interconnect 
  { "InterconnectId'" :: NullOrUndefined (InterconnectId)
  , "InterconnectName'" :: NullOrUndefined (InterconnectName)
  , "InterconnectState'" :: NullOrUndefined (InterconnectState)
  , "Region'" :: NullOrUndefined (Region)
  , "Location'" :: NullOrUndefined (LocationCode)
  , "Bandwidth'" :: NullOrUndefined (Bandwidth)
  , "LoaIssueTime'" :: NullOrUndefined (LoaIssueTime)
  , "LagId'" :: NullOrUndefined (LagId)
  , "AwsDevice'" :: NullOrUndefined (AwsDevice)
  }


-- | <p>The ID of the interconnect.</p> <p>Example: dxcon-abc123</p>
newtype InterconnectId = InterconnectId String


-- | <p>A list of interconnects.</p>
newtype InterconnectList = InterconnectList (Array Interconnect)


-- | <p>The name of the interconnect.</p> <p>Example: "<i>1G Interconnect to AWS</i>"</p>
newtype InterconnectName = InterconnectName String


-- | <p>State of the interconnect.</p> <ul> <li> <p> <b>Requested</b>: The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.</p> </li> <li> <p> <b>Pending</b>: The interconnect has been approved, and is being initialized.</p> </li> <li> <p> <b>Available</b>: The network link is up, and the interconnect is ready for use.</p> </li> <li> <p> <b>Down</b>: The network link is down.</p> </li> <li> <p> <b>Deleting</b>: The interconnect is in the process of being deleted.</p> </li> <li> <p> <b>Deleted</b>: The interconnect has been deleted.</p> </li> </ul>
newtype InterconnectState = InterconnectState String


-- | <p>A structure containing a list of interconnects.</p>
newtype Interconnects = Interconnects 
  { "Interconnects'" :: NullOrUndefined (InterconnectList)
  }


-- | <p>Describes a link aggregation group (LAG). A LAG is a connection that uses the Link Aggregation Control Protocol (LACP) to logically aggregate a bundle of physical connections. Like an interconnect, it can host other connections. All connections in a LAG must terminate on the same physical AWS Direct Connect endpoint, and must be the same bandwidth.</p>
newtype Lag = Lag 
  { "ConnectionsBandwidth'" :: NullOrUndefined (Bandwidth)
  , "NumberOfConnections'" :: NullOrUndefined (Count)
  , "LagId'" :: NullOrUndefined (LagId)
  , "OwnerAccount'" :: NullOrUndefined (OwnerAccount)
  , "LagName'" :: NullOrUndefined (LagName)
  , "LagState'" :: NullOrUndefined (LagState)
  , "Location'" :: NullOrUndefined (LocationCode)
  , "Region'" :: NullOrUndefined (Region)
  , "MinimumLinks'" :: NullOrUndefined (Count)
  , "AwsDevice'" :: NullOrUndefined (AwsDevice)
  , "Connections'" :: NullOrUndefined (ConnectionList)
  , "AllowsHostedConnections'" :: NullOrUndefined (BooleanFlag)
  }


-- | <p>The ID of the LAG.</p> <p>Example: dxlag-fg5678gh</p>
newtype LagId = LagId String


-- | <p>A list of LAGs.</p>
newtype LagList = LagList (Array Lag)


newtype LagName = LagName String


-- | <p>The state of the LAG.</p> <ul> <li> <p> <b>Requested</b>: The initial state of a LAG. The LAG stays in the requested state until the Letter of Authorization (LOA) is available.</p> </li> <li> <p> <b>Pending</b>: The LAG has been approved, and is being initialized.</p> </li> <li> <p> <b>Available</b>: The network link is established, and the LAG is ready for use.</p> </li> <li> <p> <b>Down</b>: The network link is down.</p> </li> <li> <p> <b>Deleting</b>: The LAG is in the process of being deleted.</p> </li> <li> <p> <b>Deleted</b>: The LAG has been deleted.</p> </li> </ul>
newtype LagState = LagState String


-- | <p>A structure containing a list of LAGs.</p>
newtype Lags = Lags 
  { "Lags'" :: NullOrUndefined (LagList)
  }


-- | <p>A structure containing the Letter of Authorization - Connecting Facility Assignment (LOA-CFA) for a connection.</p>
newtype Loa = Loa 
  { "LoaContent'" :: NullOrUndefined (LoaContent)
  , "LoaContentType'" :: NullOrUndefined (LoaContentType)
  }


-- | <p>The binary contents of the LOA-CFA document.</p>
newtype LoaContent = LoaContent String


-- | <p>A standard media type indicating the content type of the LOA-CFA document. Currently, the only supported value is "application/pdf".</p> <p>Default: application/pdf</p>
newtype LoaContentType = LoaContentType String


newtype LoaIssueTime = LoaIssueTime Number


-- | <p>An AWS Direct Connect location where connections and interconnects can be requested.</p>
newtype Location = Location 
  { "LocationCode'" :: NullOrUndefined (LocationCode)
  , "LocationName'" :: NullOrUndefined (LocationName)
  }


-- | <p>Where the connection is located.</p> <p>Example: EqSV5</p> <p>Default: None</p>
newtype LocationCode = LocationCode String


newtype LocationList = LocationList (Array Location)


newtype LocationName = LocationName String


-- | <p>A location is a network facility where AWS Direct Connect routers are available to be connected. Generally, these are colocation hubs where many network providers have equipment, and where cross connects can be delivered. Locations include a name and facility code, and must be provided when creating a connection.</p>
newtype Locations = Locations 
  { "Locations'" :: NullOrUndefined (LocationList)
  }


newtype LongAsn = LongAsn Number


-- | <p>Maximum number of objects to return per page.</p>
newtype MaxResultSetSize = MaxResultSetSize Int


-- | <p>A structure containing information about a new BGP peer.</p>
newtype NewBGPPeer = NewBGPPeer 
  { "Asn'" :: NullOrUndefined (ASN)
  , "AuthKey'" :: NullOrUndefined (BGPAuthKey)
  , "AddressFamily'" :: NullOrUndefined (AddressFamily)
  , "AmazonAddress'" :: NullOrUndefined (AmazonAddress)
  , "CustomerAddress'" :: NullOrUndefined (CustomerAddress)
  }


-- | <p>A structure containing information about a new private virtual interface.</p>
newtype NewPrivateVirtualInterface = NewPrivateVirtualInterface 
  { "VirtualInterfaceName'" :: (VirtualInterfaceName)
  , "Vlan'" :: (VLAN)
  , "Asn'" :: (ASN)
  , "AuthKey'" :: NullOrUndefined (BGPAuthKey)
  , "AmazonAddress'" :: NullOrUndefined (AmazonAddress)
  , "CustomerAddress'" :: NullOrUndefined (CustomerAddress)
  , "AddressFamily'" :: NullOrUndefined (AddressFamily)
  , "VirtualGatewayId'" :: NullOrUndefined (VirtualGatewayId)
  , "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId)
  }


-- | <p>A structure containing information about a private virtual interface that will be provisioned on a connection.</p>
newtype NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation 
  { "VirtualInterfaceName'" :: (VirtualInterfaceName)
  , "Vlan'" :: (VLAN)
  , "Asn'" :: (ASN)
  , "AuthKey'" :: NullOrUndefined (BGPAuthKey)
  , "AmazonAddress'" :: NullOrUndefined (AmazonAddress)
  , "AddressFamily'" :: NullOrUndefined (AddressFamily)
  , "CustomerAddress'" :: NullOrUndefined (CustomerAddress)
  }


-- | <p>A structure containing information about a new public virtual interface.</p>
newtype NewPublicVirtualInterface = NewPublicVirtualInterface 
  { "VirtualInterfaceName'" :: (VirtualInterfaceName)
  , "Vlan'" :: (VLAN)
  , "Asn'" :: (ASN)
  , "AuthKey'" :: NullOrUndefined (BGPAuthKey)
  , "AmazonAddress'" :: NullOrUndefined (AmazonAddress)
  , "CustomerAddress'" :: NullOrUndefined (CustomerAddress)
  , "AddressFamily'" :: NullOrUndefined (AddressFamily)
  , "RouteFilterPrefixes'" :: NullOrUndefined (RouteFilterPrefixList)
  }


-- | <p>A structure containing information about a public virtual interface that will be provisioned on a connection.</p>
newtype NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation 
  { "VirtualInterfaceName'" :: (VirtualInterfaceName)
  , "Vlan'" :: (VLAN)
  , "Asn'" :: (ASN)
  , "AuthKey'" :: NullOrUndefined (BGPAuthKey)
  , "AmazonAddress'" :: NullOrUndefined (AmazonAddress)
  , "CustomerAddress'" :: NullOrUndefined (CustomerAddress)
  , "AddressFamily'" :: NullOrUndefined (AddressFamily)
  , "RouteFilterPrefixes'" :: NullOrUndefined (RouteFilterPrefixList)
  }


newtype OwnerAccount = OwnerAccount String


-- | <p>Token to retrieve the next page of the result.</p>
newtype PaginationToken = PaginationToken String


newtype PartnerName = PartnerName String


newtype ProviderName = ProviderName String


-- | <p>The AWS region where the connection is located.</p> <p>Example: us-east-1</p> <p>Default: None</p>
newtype Region = Region String


newtype ResourceArn = ResourceArn String


newtype ResourceArnList = ResourceArnList (Array ResourceArn)


-- | <p>The tags associated with a Direct Connect resource.</p>
newtype ResourceTag = ResourceTag 
  { "ResourceArn'" :: NullOrUndefined (ResourceArn)
  , "Tags'" :: NullOrUndefined (TagList)
  }


newtype ResourceTagList = ResourceTagList (Array ResourceTag)


-- | <p>A route filter prefix that the customer can advertise through Border Gateway Protocol (BGP) over a public virtual interface.</p>
newtype RouteFilterPrefix = RouteFilterPrefix 
  { "Cidr'" :: NullOrUndefined (CIDR)
  }


-- | <p>A list of routes to be advertised to the AWS network in this region (public virtual interface).</p>
newtype RouteFilterPrefixList = RouteFilterPrefixList (Array RouteFilterPrefix)


newtype RouterConfig = RouterConfig String


-- | <p>Error message when the state of an object fails to advance.</p>
newtype StateChangeError = StateChangeError String


-- | <p>Information about a tag.</p>
newtype Tag = Tag 
  { "Key'" :: (TagKey)
  , "Value'" :: NullOrUndefined (TagValue)
  }


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array TagKey)


newtype TagList = TagList (Array Tag)


-- | <p>Container for the parameters to the TagResource operation.</p>
newtype TagResourceRequest = TagResourceRequest 
  { "ResourceArn'" :: (ResourceArn)
  , "Tags'" :: (TagList)
  }


-- | <p>The response received when TagResource is called.</p>
newtype TagResourceResponse = TagResourceResponse 
  { 
  }


newtype TagValue = TagValue String


-- | <p>You have reached the limit on the number of tags that can be assigned to a Direct Connect resource.</p>
newtype TooManyTagsException = TooManyTagsException 
  { 
  }


-- | <p>Container for the parameters to the UntagResource operation.</p>
newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceArn'" :: (ResourceArn)
  , "TagKeys'" :: (TagKeyList)
  }


-- | <p>The response received when UntagResource is called.</p>
newtype UntagResourceResponse = UntagResourceResponse 
  { 
  }


-- | <p>Container for the parameters to the UpdateLag operation.</p>
newtype UpdateLagRequest = UpdateLagRequest 
  { "LagId'" :: (LagId)
  , "LagName'" :: NullOrUndefined (LagName)
  , "MinimumLinks'" :: NullOrUndefined (Count)
  }


-- | <p>The VLAN ID.</p> <p>Example: 101</p>
newtype VLAN = VLAN Int


-- | <p>You can create one or more AWS Direct Connect private virtual interfaces linking to your virtual private gateway.</p> <p>Virtual private gateways can be managed using the Amazon Virtual Private Cloud (Amazon VPC) console or the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html">Amazon EC2 CreateVpnGateway action</a>.</p>
newtype VirtualGateway = VirtualGateway 
  { "VirtualGatewayId'" :: NullOrUndefined (VirtualGatewayId)
  , "VirtualGatewayState'" :: NullOrUndefined (VirtualGatewayState)
  }


-- | <p>The ID of the virtual private gateway to a VPC. This only applies to private virtual interfaces.</p> <p>Example: vgw-123er56</p>
newtype VirtualGatewayId = VirtualGatewayId String


-- | <p>A list of virtual private gateways.</p>
newtype VirtualGatewayList = VirtualGatewayList (Array VirtualGateway)


-- | <p>The region in which the virtual private gateway is located.</p> <p>Example: us-east-1</p>
newtype VirtualGatewayRegion = VirtualGatewayRegion String


-- | <p>State of the virtual private gateway.</p> <ul> <li> <p> <b>Pending</b>: This is the initial state after calling <i>CreateVpnGateway</i>.</p> </li> <li> <p> <b>Available</b>: Ready for use by a private virtual interface.</p> </li> <li> <p> <b>Deleting</b>: This is the initial state after calling <i>DeleteVpnGateway</i>.</p> </li> <li> <p> <b>Deleted</b>: In this state, a private virtual interface is unable to send traffic over this gateway.</p> </li> </ul>
newtype VirtualGatewayState = VirtualGatewayState String


-- | <p>A structure containing a list of virtual private gateways.</p>
newtype VirtualGateways = VirtualGateways 
  { "VirtualGateways'" :: NullOrUndefined (VirtualGatewayList)
  }


-- | <p>A virtual interface (VLAN) transmits the traffic between the AWS Direct Connect location and the customer.</p>
newtype VirtualInterface = VirtualInterface 
  { "OwnerAccount'" :: NullOrUndefined (OwnerAccount)
  , "VirtualInterfaceId'" :: NullOrUndefined (VirtualInterfaceId)
  , "Location'" :: NullOrUndefined (LocationCode)
  , "ConnectionId'" :: NullOrUndefined (ConnectionId)
  , "VirtualInterfaceType'" :: NullOrUndefined (VirtualInterfaceType)
  , "VirtualInterfaceName'" :: NullOrUndefined (VirtualInterfaceName)
  , "Vlan'" :: NullOrUndefined (VLAN)
  , "Asn'" :: NullOrUndefined (ASN)
  , "AmazonSideAsn'" :: NullOrUndefined (LongAsn)
  , "AuthKey'" :: NullOrUndefined (BGPAuthKey)
  , "AmazonAddress'" :: NullOrUndefined (AmazonAddress)
  , "CustomerAddress'" :: NullOrUndefined (CustomerAddress)
  , "AddressFamily'" :: NullOrUndefined (AddressFamily)
  , "VirtualInterfaceState'" :: NullOrUndefined (VirtualInterfaceState)
  , "CustomerRouterConfig'" :: NullOrUndefined (RouterConfig)
  , "VirtualGatewayId'" :: NullOrUndefined (VirtualGatewayId)
  , "DirectConnectGatewayId'" :: NullOrUndefined (DirectConnectGatewayId)
  , "RouteFilterPrefixes'" :: NullOrUndefined (RouteFilterPrefixList)
  , "BgpPeers'" :: NullOrUndefined (BGPPeerList)
  }


-- | <p>The ID of the virtual interface.</p> <p>Example: dxvif-123dfg56</p> <p>Default: None</p>
newtype VirtualInterfaceId = VirtualInterfaceId String


-- | <p>A list of virtual interfaces.</p>
newtype VirtualInterfaceList = VirtualInterfaceList (Array VirtualInterface)


-- | <p>The name of the virtual interface assigned by the customer.</p> <p>Example: "My VPC"</p>
newtype VirtualInterfaceName = VirtualInterfaceName String


-- | <p>The region in which the virtual interface is located.</p> <p>Example: us-east-1</p>
newtype VirtualInterfaceRegion = VirtualInterfaceRegion String


-- | <p>State of the virtual interface.</p> <ul> <li> <p> <b>Confirming</b>: The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.</p> </li> <li> <p> <b>Verifying</b>: This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.</p> </li> <li> <p> <b>Pending</b>: A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.</p> </li> <li> <p> <b>Available</b>: A virtual interface that is able to forward traffic.</p> </li> <li> <p> <b>Down</b>: A virtual interface that is BGP down.</p> </li> <li> <p> <b>Deleting</b>: A virtual interface is in this state immediately after calling <a>DeleteVirtualInterface</a> until it can no longer forward traffic.</p> </li> <li> <p> <b>Deleted</b>: A virtual interface that cannot forward traffic.</p> </li> <li> <p> <b>Rejected</b>: The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the 'Confirming' state is deleted by the virtual interface owner, the virtual interface will enter the 'Rejected' state.</p> </li> </ul>
newtype VirtualInterfaceState = VirtualInterfaceState String


-- | <p>The type of virtual interface.</p> <p>Example: private (Amazon VPC) or public (Amazon S3, Amazon DynamoDB, and so on.)</p>
newtype VirtualInterfaceType = VirtualInterfaceType String


-- | <p>A structure containing a list of virtual interfaces.</p>
newtype VirtualInterfaces = VirtualInterfaces 
  { "VirtualInterfaces'" :: NullOrUndefined (VirtualInterfaceList)
  }
