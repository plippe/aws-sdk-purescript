

-- | <p>AWS Direct Connect links your internal network to an AWS Direct Connect location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic cable. One end of the cable is connected to your router, the other to an AWS Direct Connect router. With this connection in place, you can create virtual interfaces directly to the AWS cloud (for example, to Amazon Elastic Compute Cloud (Amazon EC2) and Amazon Simple Storage Service (Amazon S3)) and to Amazon Virtual Private Cloud (Amazon VPC), bypassing Internet service providers in your network path. An AWS Direct Connect location provides access to AWS in the region it is associated with, as well as access to other US regions. For example, you can provision a single connection to any AWS Direct Connect location in the US and use it to access public AWS services in all US Regions and AWS GovCloud (US).</p>
module AWS.DirectConnect where

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

serviceName = "DirectConnect" :: String


-- | <p>Deprecated in favor of <a>AllocateHostedConnection</a>.</p> <p>Creates a hosted connection on an interconnect.</p> <p>Allocates a VLAN number and a specified amount of bandwidth for use by a hosted connection on the given interconnect.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
allocateConnectionOnInterconnect :: forall eff. AllocateConnectionOnInterconnectRequest -> Aff (exception :: EXCEPTION | eff) Connection
allocateConnectionOnInterconnect = Request.request serviceName "allocateConnectionOnInterconnect" 


-- | <p>Creates a hosted connection on an interconnect or a link aggregation group (LAG).</p> <p>Allocates a VLAN number and a specified amount of bandwidth for use by a hosted connection on the given interconnect or LAG.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
allocateHostedConnection :: forall eff. AllocateHostedConnectionRequest -> Aff (exception :: EXCEPTION | eff) Connection
allocateHostedConnection = Request.request serviceName "allocateHostedConnection" 


-- | <p>Provisions a private virtual interface to be owned by another AWS customer.</p> <p>Virtual interfaces created using this action must be confirmed by the virtual interface owner by using the <a>ConfirmPrivateVirtualInterface</a> action. Until then, the virtual interface will be in 'Confirming' state, and will not be available for handling traffic.</p>
allocatePrivateVirtualInterface :: forall eff. AllocatePrivateVirtualInterfaceRequest -> Aff (exception :: EXCEPTION | eff) VirtualInterface
allocatePrivateVirtualInterface = Request.request serviceName "allocatePrivateVirtualInterface" 


-- | <p>Provisions a public virtual interface to be owned by a different customer.</p> <p>The owner of a connection calls this function to provision a public virtual interface which will be owned by another AWS customer.</p> <p>Virtual interfaces created using this function must be confirmed by the virtual interface owner by calling ConfirmPublicVirtualInterface. Until this step has been completed, the virtual interface will be in 'Confirming' state, and will not be available for handling traffic.</p> <p>When creating an IPv6 public virtual interface (addressFamily is 'ipv6'), the customer and amazon address fields should be left blank to use auto-assigned IPv6 space. Custom IPv6 Addresses are currently not supported.</p>
allocatePublicVirtualInterface :: forall eff. AllocatePublicVirtualInterfaceRequest -> Aff (exception :: EXCEPTION | eff) VirtualInterface
allocatePublicVirtualInterface = Request.request serviceName "allocatePublicVirtualInterface" 


-- | <p>Associates an existing connection with a link aggregation group (LAG). The connection is interrupted and re-established as a member of the LAG (connectivity to AWS will be interrupted). The connection must be hosted on the same AWS Direct Connect endpoint as the LAG, and its bandwidth must match the bandwidth for the LAG. You can reassociate a connection that's currently associated with a different LAG; however, if removing the connection will cause the original LAG to fall below its setting for minimum number of operational connections, the request fails.</p> <p>Any virtual interfaces that are directly associated with the connection are automatically re-associated with the LAG. If the connection was originally associated with a different LAG, the virtual interfaces remain associated with the original LAG.</p> <p>For interconnects, any hosted connections are automatically re-associated with the LAG. If the interconnect was originally associated with a different LAG, the hosted connections remain associated with the original LAG.</p>
associateConnectionWithLag :: forall eff. AssociateConnectionWithLagRequest -> Aff (exception :: EXCEPTION | eff) Connection
associateConnectionWithLag = Request.request serviceName "associateConnectionWithLag" 


-- | <p>Associates a hosted connection and its virtual interfaces with a link aggregation group (LAG) or interconnect. If the target interconnect or LAG has an existing hosted connection with a conflicting VLAN number or IP address, the operation fails. This action temporarily interrupts the hosted connection's connectivity to AWS as it is being migrated.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
associateHostedConnection :: forall eff. AssociateHostedConnectionRequest -> Aff (exception :: EXCEPTION | eff) Connection
associateHostedConnection = Request.request serviceName "associateHostedConnection" 


-- | <p>Associates a virtual interface with a specified link aggregation group (LAG) or connection. Connectivity to AWS is temporarily interrupted as the virtual interface is being migrated. If the target connection or LAG has an associated virtual interface with a conflicting VLAN number or a conflicting IP address, the operation fails. </p> <p>Virtual interfaces associated with a hosted connection cannot be associated with a LAG; hosted connections must be migrated along with their virtual interfaces using <a>AssociateHostedConnection</a>.</p> <p>In order to reassociate a virtual interface to a new connection or LAG, the requester must own either the virtual interface itself or the connection to which the virtual interface is currently associated. Additionally, the requester must own the connection or LAG to which the virtual interface will be newly associated.</p>
associateVirtualInterface :: forall eff. AssociateVirtualInterfaceRequest -> Aff (exception :: EXCEPTION | eff) VirtualInterface
associateVirtualInterface = Request.request serviceName "associateVirtualInterface" 


-- | <p>Confirm the creation of a hosted connection on an interconnect.</p> <p>Upon creation, the hosted connection is initially in the 'Ordering' state, and will remain in this state until the owner calls ConfirmConnection to confirm creation of the hosted connection.</p>
confirmConnection :: forall eff. ConfirmConnectionRequest -> Aff (exception :: EXCEPTION | eff) ConfirmConnectionResponse
confirmConnection = Request.request serviceName "confirmConnection" 


-- | <p>Accept ownership of a private virtual interface created by another customer.</p> <p>After the virtual interface owner calls this function, the virtual interface will be created and attached to the given virtual private gateway or direct connect gateway, and will be available for handling traffic.</p>
confirmPrivateVirtualInterface :: forall eff. ConfirmPrivateVirtualInterfaceRequest -> Aff (exception :: EXCEPTION | eff) ConfirmPrivateVirtualInterfaceResponse
confirmPrivateVirtualInterface = Request.request serviceName "confirmPrivateVirtualInterface" 


-- | <p>Accept ownership of a public virtual interface created by another customer.</p> <p>After the virtual interface owner calls this function, the specified virtual interface will be created and made available for handling traffic.</p>
confirmPublicVirtualInterface :: forall eff. ConfirmPublicVirtualInterfaceRequest -> Aff (exception :: EXCEPTION | eff) ConfirmPublicVirtualInterfaceResponse
confirmPublicVirtualInterface = Request.request serviceName "confirmPublicVirtualInterface" 


-- | <p>Creates a new BGP peer on a specified virtual interface. The BGP peer cannot be in the same address family (IPv4/IPv6) of an existing BGP peer on the virtual interface.</p> <p>You must create a BGP peer for the corresponding address family in order to access AWS resources that also use that address family.</p> <p>When creating a IPv6 BGP peer, the Amazon address and customer address fields must be left blank. IPv6 addresses are automatically assigned from Amazon's pool of IPv6 addresses; you cannot specify custom IPv6 addresses.</p> <p>For a public virtual interface, the Autonomous System Number (ASN) must be private or already whitelisted for the virtual interface.</p>
createBGPPeer :: forall eff. CreateBGPPeerRequest -> Aff (exception :: EXCEPTION | eff) CreateBGPPeerResponse
createBGPPeer = Request.request serviceName "createBGPPeer" 


-- | <p>Creates a new connection between the customer network and a specific AWS Direct Connect location.</p> <p>A connection links your internal network to an AWS Direct Connect location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic cable. One end of the cable is connected to your router, the other to an AWS Direct Connect router. An AWS Direct Connect location provides access to Amazon Web Services in the region it is associated with. You can establish connections with AWS Direct Connect locations in multiple regions, but a connection in one region does not provide connectivity to other regions.</p> <p>To find the locations for your region, use <a>DescribeLocations</a>.</p> <p>You can automatically add the new connection to a link aggregation group (LAG) by specifying a LAG ID in the request. This ensures that the new connection is allocated on the same AWS Direct Connect endpoint that hosts the specified LAG. If there are no available ports on the endpoint, the request fails and no connection will be created.</p>
createConnection :: forall eff. CreateConnectionRequest -> Aff (exception :: EXCEPTION | eff) Connection
createConnection = Request.request serviceName "createConnection" 


-- | <p>Creates a new direct connect gateway. A direct connect gateway is an intermediate object that enables you to connect a set of virtual interfaces and virtual private gateways. direct connect gateways are global and visible in any AWS region after they are created. The virtual interfaces and virtual private gateways that are connected through a direct connect gateway can be in different regions. This enables you to connect to a VPC in any region, regardless of the region in which the virtual interfaces are located, and pass traffic between them.</p>
createDirectConnectGateway :: forall eff. CreateDirectConnectGatewayRequest -> Aff (exception :: EXCEPTION | eff) CreateDirectConnectGatewayResult
createDirectConnectGateway = Request.request serviceName "createDirectConnectGateway" 


-- | <p>Creates an association between a direct connect gateway and a virtual private gateway (VGW). The VGW must be attached to a VPC and must not be associated with another direct connect gateway.</p>
createDirectConnectGatewayAssociation :: forall eff. CreateDirectConnectGatewayAssociationRequest -> Aff (exception :: EXCEPTION | eff) CreateDirectConnectGatewayAssociationResult
createDirectConnectGatewayAssociation = Request.request serviceName "createDirectConnectGatewayAssociation" 


-- | <p>Creates a new interconnect between a AWS Direct Connect partner's network and a specific AWS Direct Connect location.</p> <p>An interconnect is a connection which is capable of hosting other connections. The AWS Direct Connect partner can use an interconnect to provide sub-1Gbps AWS Direct Connect service to tier 2 customers who do not have their own connections. Like a standard connection, an interconnect links the AWS Direct Connect partner's network to an AWS Direct Connect location over a standard 1 Gbps or 10 Gbps Ethernet fiber-optic cable. One end is connected to the partner's router, the other to an AWS Direct Connect router.</p> <p>You can automatically add the new interconnect to a link aggregation group (LAG) by specifying a LAG ID in the request. This ensures that the new interconnect is allocated on the same AWS Direct Connect endpoint that hosts the specified LAG. If there are no available ports on the endpoint, the request fails and no interconnect will be created.</p> <p>For each end customer, the AWS Direct Connect partner provisions a connection on their interconnect by calling AllocateConnectionOnInterconnect. The end customer can then connect to AWS resources by creating a virtual interface on their connection, using the VLAN assigned to them by the AWS Direct Connect partner.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
createInterconnect :: forall eff. CreateInterconnectRequest -> Aff (exception :: EXCEPTION | eff) Interconnect
createInterconnect = Request.request serviceName "createInterconnect" 


-- | <p>Creates a new link aggregation group (LAG) with the specified number of bundled physical connections between the customer network and a specific AWS Direct Connect location. A LAG is a logical interface that uses the Link Aggregation Control Protocol (LACP) to aggregate multiple 1 gigabit or 10 gigabit interfaces, allowing you to treat them as a single interface.</p> <p>All connections in a LAG must use the same bandwidth (for example, 10 Gbps), and must terminate at the same AWS Direct Connect endpoint.</p> <p>You can have up to 10 connections per LAG. Regardless of this limit, if you request more connections for the LAG than AWS Direct Connect can allocate on a single endpoint, no LAG is created.</p> <p>You can specify an existing physical connection or interconnect to include in the LAG (which counts towards the total number of connections). Doing so interrupts the current physical connection or hosted connections, and re-establishes them as a member of the LAG. The LAG will be created on the same AWS Direct Connect endpoint to which the connection terminates. Any virtual interfaces associated with the connection are automatically disassociated and re-associated with the LAG. The connection ID does not change.</p> <p>If the AWS account used to create a LAG is a registered AWS Direct Connect partner, the LAG is automatically enabled to host sub-connections. For a LAG owned by a partner, any associated virtual interfaces cannot be directly configured.</p>
createLag :: forall eff. CreateLagRequest -> Aff (exception :: EXCEPTION | eff) Lag
createLag = Request.request serviceName "createLag" 


-- | <p>Creates a new private virtual interface. A virtual interface is the VLAN that transports AWS Direct Connect traffic. A private virtual interface supports sending traffic to a single virtual private cloud (VPC).</p>
createPrivateVirtualInterface :: forall eff. CreatePrivateVirtualInterfaceRequest -> Aff (exception :: EXCEPTION | eff) VirtualInterface
createPrivateVirtualInterface = Request.request serviceName "createPrivateVirtualInterface" 


-- | <p>Creates a new public virtual interface. A virtual interface is the VLAN that transports AWS Direct Connect traffic. A public virtual interface supports sending traffic to public services of AWS such as Amazon Simple Storage Service (Amazon S3).</p> <p>When creating an IPv6 public virtual interface (addressFamily is 'ipv6'), the customer and amazon address fields should be left blank to use auto-assigned IPv6 space. Custom IPv6 Addresses are currently not supported.</p>
createPublicVirtualInterface :: forall eff. CreatePublicVirtualInterfaceRequest -> Aff (exception :: EXCEPTION | eff) VirtualInterface
createPublicVirtualInterface = Request.request serviceName "createPublicVirtualInterface" 


-- | <p>Deletes a BGP peer on the specified virtual interface that matches the specified customer address and ASN. You cannot delete the last BGP peer from a virtual interface.</p>
deleteBGPPeer :: forall eff. DeleteBGPPeerRequest -> Aff (exception :: EXCEPTION | eff) DeleteBGPPeerResponse
deleteBGPPeer = Request.request serviceName "deleteBGPPeer" 


-- | <p>Deletes the connection.</p> <p>Deleting a connection only stops the AWS Direct Connect port hour and data transfer charges. You need to cancel separately with the providers any services or charges for cross-connects or network circuits that connect you to the AWS Direct Connect location.</p>
deleteConnection :: forall eff. DeleteConnectionRequest -> Aff (exception :: EXCEPTION | eff) Connection
deleteConnection = Request.request serviceName "deleteConnection" 


-- | <p>Deletes a direct connect gateway. You must first delete all virtual interfaces that are attached to the direct connect gateway and disassociate all virtual private gateways that are associated with the direct connect gateway.</p>
deleteDirectConnectGateway :: forall eff. DeleteDirectConnectGatewayRequest -> Aff (exception :: EXCEPTION | eff) DeleteDirectConnectGatewayResult
deleteDirectConnectGateway = Request.request serviceName "deleteDirectConnectGateway" 


-- | <p>Deletes the association between a direct connect gateway and a virtual private gateway.</p>
deleteDirectConnectGatewayAssociation :: forall eff. DeleteDirectConnectGatewayAssociationRequest -> Aff (exception :: EXCEPTION | eff) DeleteDirectConnectGatewayAssociationResult
deleteDirectConnectGatewayAssociation = Request.request serviceName "deleteDirectConnectGatewayAssociation" 


-- | <p>Deletes the specified interconnect.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
deleteInterconnect :: forall eff. DeleteInterconnectRequest -> Aff (exception :: EXCEPTION | eff) DeleteInterconnectResponse
deleteInterconnect = Request.request serviceName "deleteInterconnect" 


-- | <p>Deletes a link aggregation group (LAG). You cannot delete a LAG if it has active virtual interfaces or hosted connections.</p>
deleteLag :: forall eff. DeleteLagRequest -> Aff (exception :: EXCEPTION | eff) Lag
deleteLag = Request.request serviceName "deleteLag" 


-- | <p>Deletes a virtual interface.</p>
deleteVirtualInterface :: forall eff. DeleteVirtualInterfaceRequest -> Aff (exception :: EXCEPTION | eff) DeleteVirtualInterfaceResponse
deleteVirtualInterface = Request.request serviceName "deleteVirtualInterface" 


-- | <p>Deprecated in favor of <a>DescribeLoa</a>.</p> <p>Returns the LOA-CFA for a Connection.</p> <p>The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that your APN partner or service provider uses when establishing your cross connect to AWS at the colocation facility. For more information, see <a href="http://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html">Requesting Cross Connects at AWS Direct Connect Locations</a> in the AWS Direct Connect user guide.</p>
describeConnectionLoa :: forall eff. DescribeConnectionLoaRequest -> Aff (exception :: EXCEPTION | eff) DescribeConnectionLoaResponse
describeConnectionLoa = Request.request serviceName "describeConnectionLoa" 


-- | <p>Displays all connections in this region.</p> <p>If a connection ID is provided, the call returns only that particular connection.</p>
describeConnections :: forall eff. DescribeConnectionsRequest -> Aff (exception :: EXCEPTION | eff) Connections
describeConnections = Request.request serviceName "describeConnections" 


-- | <p>Deprecated in favor of <a>DescribeHostedConnections</a>.</p> <p>Returns a list of connections that have been provisioned on the given interconnect.</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
describeConnectionsOnInterconnect :: forall eff. DescribeConnectionsOnInterconnectRequest -> Aff (exception :: EXCEPTION | eff) Connections
describeConnectionsOnInterconnect = Request.request serviceName "describeConnectionsOnInterconnect" 


-- | <p>Returns a list of all direct connect gateway and virtual private gateway (VGW) associations. Either a direct connect gateway ID or a VGW ID must be provided in the request. If a direct connect gateway ID is provided, the response returns all VGWs associated with the direct connect gateway. If a VGW ID is provided, the response returns all direct connect gateways associated with the VGW. If both are provided, the response only returns the association that matches both the direct connect gateway and the VGW.</p>
describeDirectConnectGatewayAssociations :: forall eff. DescribeDirectConnectGatewayAssociationsRequest -> Aff (exception :: EXCEPTION | eff) DescribeDirectConnectGatewayAssociationsResult
describeDirectConnectGatewayAssociations = Request.request serviceName "describeDirectConnectGatewayAssociations" 


-- | <p>Returns a list of all direct connect gateway and virtual interface (VIF) attachments. Either a direct connect gateway ID or a VIF ID must be provided in the request. If a direct connect gateway ID is provided, the response returns all VIFs attached to the direct connect gateway. If a VIF ID is provided, the response returns all direct connect gateways attached to the VIF. If both are provided, the response only returns the attachment that matches both the direct connect gateway and the VIF.</p>
describeDirectConnectGatewayAttachments :: forall eff. DescribeDirectConnectGatewayAttachmentsRequest -> Aff (exception :: EXCEPTION | eff) DescribeDirectConnectGatewayAttachmentsResult
describeDirectConnectGatewayAttachments = Request.request serviceName "describeDirectConnectGatewayAttachments" 


-- | <p>Returns a list of direct connect gateways in your account. Deleted direct connect gateways are not returned. You can provide a direct connect gateway ID in the request to return information about the specific direct connect gateway only. Otherwise, if a direct connect gateway ID is not provided, information about all of your direct connect gateways is returned. </p>
describeDirectConnectGateways :: forall eff. DescribeDirectConnectGatewaysRequest -> Aff (exception :: EXCEPTION | eff) DescribeDirectConnectGatewaysResult
describeDirectConnectGateways = Request.request serviceName "describeDirectConnectGateways" 


-- | <p>Returns a list of hosted connections that have been provisioned on the given interconnect or link aggregation group (LAG).</p> <note> <p>This is intended for use by AWS Direct Connect partners only.</p> </note>
describeHostedConnections :: forall eff. DescribeHostedConnectionsRequest -> Aff (exception :: EXCEPTION | eff) Connections
describeHostedConnections = Request.request serviceName "describeHostedConnections" 


-- | <p>Deprecated in favor of <a>DescribeLoa</a>.</p> <p>Returns the LOA-CFA for an Interconnect.</p> <p>The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that is used when establishing your cross connect to AWS at the colocation facility. For more information, see <a href="http://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html">Requesting Cross Connects at AWS Direct Connect Locations</a> in the AWS Direct Connect user guide.</p>
describeInterconnectLoa :: forall eff. DescribeInterconnectLoaRequest -> Aff (exception :: EXCEPTION | eff) DescribeInterconnectLoaResponse
describeInterconnectLoa = Request.request serviceName "describeInterconnectLoa" 


-- | <p>Returns a list of interconnects owned by the AWS account.</p> <p>If an interconnect ID is provided, it will only return this particular interconnect.</p>
describeInterconnects :: forall eff. DescribeInterconnectsRequest -> Aff (exception :: EXCEPTION | eff) Interconnects
describeInterconnects = Request.request serviceName "describeInterconnects" 


-- | <p>Describes the link aggregation groups (LAGs) in your account. </p> <p>If a LAG ID is provided, only information about the specified LAG is returned.</p>
describeLags :: forall eff. DescribeLagsRequest -> Aff (exception :: EXCEPTION | eff) Lags
describeLags = Request.request serviceName "describeLags" 


-- | <p>Returns the LOA-CFA for a connection, interconnect, or link aggregation group (LAG).</p> <p>The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that is used when establishing your cross connect to AWS at the colocation facility. For more information, see <a href="http://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html">Requesting Cross Connects at AWS Direct Connect Locations</a> in the AWS Direct Connect user guide.</p>
describeLoa :: forall eff. DescribeLoaRequest -> Aff (exception :: EXCEPTION | eff) Loa
describeLoa = Request.request serviceName "describeLoa" 


-- | <p>Returns the list of AWS Direct Connect locations in the current AWS region. These are the locations that may be selected when calling <a>CreateConnection</a> or <a>CreateInterconnect</a>.</p>
describeLocations :: forall eff.  Aff (exception :: EXCEPTION | eff) Locations
describeLocations = Request.request serviceName "describeLocations" (Types.NoInput unit)


-- | <p>Describes the tags associated with the specified Direct Connect resources.</p>
describeTags :: forall eff. DescribeTagsRequest -> Aff (exception :: EXCEPTION | eff) DescribeTagsResponse
describeTags = Request.request serviceName "describeTags" 


-- | <p>Returns a list of virtual private gateways owned by the AWS account.</p> <p>You can create one or more AWS Direct Connect private virtual interfaces linking to a virtual private gateway. A virtual private gateway can be managed via Amazon Virtual Private Cloud (VPC) console or the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html">EC2 CreateVpnGateway</a> action.</p>
describeVirtualGateways :: forall eff.  Aff (exception :: EXCEPTION | eff) VirtualGateways
describeVirtualGateways = Request.request serviceName "describeVirtualGateways" (Types.NoInput unit)


-- | <p>Displays all virtual interfaces for an AWS account. Virtual interfaces deleted fewer than 15 minutes before you make the request are also returned. If you specify a connection ID, only the virtual interfaces associated with the connection are returned. If you specify a virtual interface ID, then only a single virtual interface is returned.</p> <p>A virtual interface (VLAN) transmits the traffic between the AWS Direct Connect location and the customer.</p>
describeVirtualInterfaces :: forall eff. DescribeVirtualInterfacesRequest -> Aff (exception :: EXCEPTION | eff) VirtualInterfaces
describeVirtualInterfaces = Request.request serviceName "describeVirtualInterfaces" 


-- | <p>Disassociates a connection from a link aggregation group (LAG). The connection is interrupted and re-established as a standalone connection (the connection is not deleted; to delete the connection, use the <a>DeleteConnection</a> request). If the LAG has associated virtual interfaces or hosted connections, they remain associated with the LAG. A disassociated connection owned by an AWS Direct Connect partner is automatically converted to an interconnect.</p> <p>If disassociating the connection will cause the LAG to fall below its setting for minimum number of operational connections, the request fails, except when it's the last member of the LAG. If all connections are disassociated, the LAG continues to exist as an empty LAG with no physical connections. </p>
disassociateConnectionFromLag :: forall eff. DisassociateConnectionFromLagRequest -> Aff (exception :: EXCEPTION | eff) Connection
disassociateConnectionFromLag = Request.request serviceName "disassociateConnectionFromLag" 


-- | <p>Adds the specified tags to the specified Direct Connect resource. Each Direct Connect resource can have a maximum of 50 tags.</p> <p>Each tag consists of a key and an optional value. If a tag with the same key is already associated with the Direct Connect resource, this action updates its value.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (exception :: EXCEPTION | eff) TagResourceResponse
tagResource = Request.request serviceName "tagResource" 


-- | <p>Removes one or more tags from the specified Direct Connect resource.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (exception :: EXCEPTION | eff) UntagResourceResponse
untagResource = Request.request serviceName "untagResource" 


-- | <p>Updates the attributes of a link aggregation group (LAG). </p> <p>You can update the following attributes: </p> <ul> <li> <p>The name of the LAG.</p> </li> <li> <p>The value for the minimum number of connections that must be operational for the LAG itself to be operational. </p> </li> </ul> <p>When you create a LAG, the default value for the minimum number of operational connections is zero (0). If you update this value, and the number of operational connections falls below the specified value, the LAG will automatically go down to avoid overutilization of the remaining connections. Adjusting this value should be done with care as it could force the LAG down if the value is set higher than the current number of operational connections.</p>
updateLag :: forall eff. UpdateLagRequest -> Aff (exception :: EXCEPTION | eff) Lag
updateLag = Request.request serviceName "updateLag" 


-- | <p>The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.</p> <p>Example: 65000</p>
newtype ASN = ASN Int
derive instance newtypeASN :: Newtype ASN _
derive instance repGenericASN :: Generic ASN _
instance showASN :: Show ASN where
  show = genericShow
instance decodeASN :: Decode ASN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeASN :: Encode ASN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates the address family for the BGP peer.</p> <ul> <li> <p> <b>ipv4</b>: IPv4 address family</p> </li> <li> <p> <b>ipv6</b>: IPv6 address family</p> </li> </ul>
newtype AddressFamily = AddressFamily String
derive instance newtypeAddressFamily :: Newtype AddressFamily _
derive instance repGenericAddressFamily :: Generic AddressFamily _
instance showAddressFamily :: Show AddressFamily where
  show = genericShow
instance decodeAddressFamily :: Decode AddressFamily where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddressFamily :: Encode AddressFamily where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the AllocateConnectionOnInterconnect operation.</p>
newtype AllocateConnectionOnInterconnectRequest = AllocateConnectionOnInterconnectRequest 
  { "Bandwidth'" :: (Bandwidth)
  , "ConnectionName'" :: (ConnectionName)
  , "OwnerAccount'" :: (OwnerAccount)
  , "InterconnectId'" :: (InterconnectId)
  , "Vlan'" :: (VLAN)
  }
derive instance newtypeAllocateConnectionOnInterconnectRequest :: Newtype AllocateConnectionOnInterconnectRequest _
derive instance repGenericAllocateConnectionOnInterconnectRequest :: Generic AllocateConnectionOnInterconnectRequest _
instance showAllocateConnectionOnInterconnectRequest :: Show AllocateConnectionOnInterconnectRequest where
  show = genericShow
instance decodeAllocateConnectionOnInterconnectRequest :: Decode AllocateConnectionOnInterconnectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllocateConnectionOnInterconnectRequest :: Encode AllocateConnectionOnInterconnectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to theHostedConnection operation.</p>
newtype AllocateHostedConnectionRequest = AllocateHostedConnectionRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "OwnerAccount'" :: (OwnerAccount)
  , "Bandwidth'" :: (Bandwidth)
  , "ConnectionName'" :: (ConnectionName)
  , "Vlan'" :: (VLAN)
  }
derive instance newtypeAllocateHostedConnectionRequest :: Newtype AllocateHostedConnectionRequest _
derive instance repGenericAllocateHostedConnectionRequest :: Generic AllocateHostedConnectionRequest _
instance showAllocateHostedConnectionRequest :: Show AllocateHostedConnectionRequest where
  show = genericShow
instance decodeAllocateHostedConnectionRequest :: Decode AllocateHostedConnectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllocateHostedConnectionRequest :: Encode AllocateHostedConnectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the AllocatePrivateVirtualInterface operation.</p>
newtype AllocatePrivateVirtualInterfaceRequest = AllocatePrivateVirtualInterfaceRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "OwnerAccount'" :: (OwnerAccount)
  , "NewPrivateVirtualInterfaceAllocation'" :: (NewPrivateVirtualInterfaceAllocation)
  }
derive instance newtypeAllocatePrivateVirtualInterfaceRequest :: Newtype AllocatePrivateVirtualInterfaceRequest _
derive instance repGenericAllocatePrivateVirtualInterfaceRequest :: Generic AllocatePrivateVirtualInterfaceRequest _
instance showAllocatePrivateVirtualInterfaceRequest :: Show AllocatePrivateVirtualInterfaceRequest where
  show = genericShow
instance decodeAllocatePrivateVirtualInterfaceRequest :: Decode AllocatePrivateVirtualInterfaceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllocatePrivateVirtualInterfaceRequest :: Encode AllocatePrivateVirtualInterfaceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the AllocatePublicVirtualInterface operation.</p>
newtype AllocatePublicVirtualInterfaceRequest = AllocatePublicVirtualInterfaceRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "OwnerAccount'" :: (OwnerAccount)
  , "NewPublicVirtualInterfaceAllocation'" :: (NewPublicVirtualInterfaceAllocation)
  }
derive instance newtypeAllocatePublicVirtualInterfaceRequest :: Newtype AllocatePublicVirtualInterfaceRequest _
derive instance repGenericAllocatePublicVirtualInterfaceRequest :: Generic AllocatePublicVirtualInterfaceRequest _
instance showAllocatePublicVirtualInterfaceRequest :: Show AllocatePublicVirtualInterfaceRequest where
  show = genericShow
instance decodeAllocatePublicVirtualInterfaceRequest :: Decode AllocatePublicVirtualInterfaceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllocatePublicVirtualInterfaceRequest :: Encode AllocatePublicVirtualInterfaceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>IP address assigned to the Amazon interface.</p> <p>Example: 192.168.1.1/30 or 2001:db8::1/125</p>
newtype AmazonAddress = AmazonAddress String
derive instance newtypeAmazonAddress :: Newtype AmazonAddress _
derive instance repGenericAmazonAddress :: Generic AmazonAddress _
instance showAmazonAddress :: Show AmazonAddress where
  show = genericShow
instance decodeAmazonAddress :: Decode AmazonAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAmazonAddress :: Encode AmazonAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the AssociateConnectionWithLag operation.</p>
newtype AssociateConnectionWithLagRequest = AssociateConnectionWithLagRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "LagId'" :: (LagId)
  }
derive instance newtypeAssociateConnectionWithLagRequest :: Newtype AssociateConnectionWithLagRequest _
derive instance repGenericAssociateConnectionWithLagRequest :: Generic AssociateConnectionWithLagRequest _
instance showAssociateConnectionWithLagRequest :: Show AssociateConnectionWithLagRequest where
  show = genericShow
instance decodeAssociateConnectionWithLagRequest :: Decode AssociateConnectionWithLagRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateConnectionWithLagRequest :: Encode AssociateConnectionWithLagRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the AssociateHostedConnection operation.</p>
newtype AssociateHostedConnectionRequest = AssociateHostedConnectionRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "ParentConnectionId'" :: (ConnectionId)
  }
derive instance newtypeAssociateHostedConnectionRequest :: Newtype AssociateHostedConnectionRequest _
derive instance repGenericAssociateHostedConnectionRequest :: Generic AssociateHostedConnectionRequest _
instance showAssociateHostedConnectionRequest :: Show AssociateHostedConnectionRequest where
  show = genericShow
instance decodeAssociateHostedConnectionRequest :: Decode AssociateHostedConnectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateHostedConnectionRequest :: Encode AssociateHostedConnectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the AssociateVirtualInterface operation.</p>
newtype AssociateVirtualInterfaceRequest = AssociateVirtualInterfaceRequest 
  { "VirtualInterfaceId'" :: (VirtualInterfaceId)
  , "ConnectionId'" :: (ConnectionId)
  }
derive instance newtypeAssociateVirtualInterfaceRequest :: Newtype AssociateVirtualInterfaceRequest _
derive instance repGenericAssociateVirtualInterfaceRequest :: Generic AssociateVirtualInterfaceRequest _
instance showAssociateVirtualInterfaceRequest :: Show AssociateVirtualInterfaceRequest where
  show = genericShow
instance decodeAssociateVirtualInterfaceRequest :: Decode AssociateVirtualInterfaceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateVirtualInterfaceRequest :: Encode AssociateVirtualInterfaceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An abstract ID for the physical Direct Connect endpoint.</p> <p>Example: EQC50-abcdef123456</p>
newtype AwsDevice = AwsDevice String
derive instance newtypeAwsDevice :: Newtype AwsDevice _
derive instance repGenericAwsDevice :: Generic AwsDevice _
instance showAwsDevice :: Show AwsDevice where
  show = genericShow
instance decodeAwsDevice :: Decode AwsDevice where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsDevice :: Encode AwsDevice where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The authentication key for BGP configuration.</p> <p>Example: asdf34example</p>
newtype BGPAuthKey = BGPAuthKey String
derive instance newtypeBGPAuthKey :: Newtype BGPAuthKey _
derive instance repGenericBGPAuthKey :: Generic BGPAuthKey _
instance showBGPAuthKey :: Show BGPAuthKey where
  show = genericShow
instance decodeBGPAuthKey :: Decode BGPAuthKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBGPAuthKey :: Encode BGPAuthKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing information about a BGP peer.</p>
newtype BGPPeer = BGPPeer 
  { "Asn'" :: NullOrUndefined.NullOrUndefined (ASN)
  , "AuthKey'" :: NullOrUndefined.NullOrUndefined (BGPAuthKey)
  , "AddressFamily'" :: NullOrUndefined.NullOrUndefined (AddressFamily)
  , "AmazonAddress'" :: NullOrUndefined.NullOrUndefined (AmazonAddress)
  , "CustomerAddress'" :: NullOrUndefined.NullOrUndefined (CustomerAddress)
  , "BgpPeerState'" :: NullOrUndefined.NullOrUndefined (BGPPeerState)
  , "BgpStatus'" :: NullOrUndefined.NullOrUndefined (BGPStatus)
  }
derive instance newtypeBGPPeer :: Newtype BGPPeer _
derive instance repGenericBGPPeer :: Generic BGPPeer _
instance showBGPPeer :: Show BGPPeer where
  show = genericShow
instance decodeBGPPeer :: Decode BGPPeer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBGPPeer :: Encode BGPPeer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of the BGP peers configured on this virtual interface.</p>
newtype BGPPeerList = BGPPeerList (Array BGPPeer)
derive instance newtypeBGPPeerList :: Newtype BGPPeerList _
derive instance repGenericBGPPeerList :: Generic BGPPeerList _
instance showBGPPeerList :: Show BGPPeerList where
  show = genericShow
instance decodeBGPPeerList :: Decode BGPPeerList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBGPPeerList :: Encode BGPPeerList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The state of the BGP peer.</p> <ul> <li> <p> <b>Verifying</b>: The BGP peering addresses or ASN require validation before the BGP peer can be created. This state only applies to BGP peers on a public virtual interface. </p> </li> <li> <p> <b>Pending</b>: The BGP peer has been created, and is in this state until it is ready to be established.</p> </li> <li> <p> <b>Available</b>: The BGP peer can be established.</p> </li> <li> <p> <b>Deleting</b>: The BGP peer is in the process of being deleted.</p> </li> <li> <p> <b>Deleted</b>: The BGP peer has been deleted and cannot be established.</p> </li> </ul>
newtype BGPPeerState = BGPPeerState String
derive instance newtypeBGPPeerState :: Newtype BGPPeerState _
derive instance repGenericBGPPeerState :: Generic BGPPeerState _
instance showBGPPeerState :: Show BGPPeerState where
  show = genericShow
instance decodeBGPPeerState :: Decode BGPPeerState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBGPPeerState :: Encode BGPPeerState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Up/Down state of the BGP peer.</p> <ul> <li> <p> <b>Up</b>: The BGP peer is established.</p> </li> <li> <p> <b>Down</b>: The BGP peer is down.</p> </li> </ul>
newtype BGPStatus = BGPStatus String
derive instance newtypeBGPStatus :: Newtype BGPStatus _
derive instance repGenericBGPStatus :: Generic BGPStatus _
instance showBGPStatus :: Show BGPStatus where
  show = genericShow
instance decodeBGPStatus :: Decode BGPStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBGPStatus :: Encode BGPStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Bandwidth of the connection.</p> <p>Example: 1Gbps</p> <p>Default: None</p>
newtype Bandwidth = Bandwidth String
derive instance newtypeBandwidth :: Newtype Bandwidth _
derive instance repGenericBandwidth :: Generic Bandwidth _
instance showBandwidth :: Show Bandwidth where
  show = genericShow
instance decodeBandwidth :: Decode Bandwidth where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBandwidth :: Encode Bandwidth where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanFlag = BooleanFlag Boolean
derive instance newtypeBooleanFlag :: Newtype BooleanFlag _
derive instance repGenericBooleanFlag :: Generic BooleanFlag _
instance showBooleanFlag :: Show BooleanFlag where
  show = genericShow
instance decodeBooleanFlag :: Decode BooleanFlag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanFlag :: Encode BooleanFlag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CIDR = CIDR String
derive instance newtypeCIDR :: Newtype CIDR _
derive instance repGenericCIDR :: Generic CIDR _
instance showCIDR :: Show CIDR where
  show = genericShow
instance decodeCIDR :: Decode CIDR where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCIDR :: Encode CIDR where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the ConfirmConnection operation.</p>
newtype ConfirmConnectionRequest = ConfirmConnectionRequest 
  { "ConnectionId'" :: (ConnectionId)
  }
derive instance newtypeConfirmConnectionRequest :: Newtype ConfirmConnectionRequest _
derive instance repGenericConfirmConnectionRequest :: Generic ConfirmConnectionRequest _
instance showConfirmConnectionRequest :: Show ConfirmConnectionRequest where
  show = genericShow
instance decodeConfirmConnectionRequest :: Decode ConfirmConnectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmConnectionRequest :: Encode ConfirmConnectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when ConfirmConnection is called.</p>
newtype ConfirmConnectionResponse = ConfirmConnectionResponse 
  { "ConnectionState'" :: NullOrUndefined.NullOrUndefined (ConnectionState)
  }
derive instance newtypeConfirmConnectionResponse :: Newtype ConfirmConnectionResponse _
derive instance repGenericConfirmConnectionResponse :: Generic ConfirmConnectionResponse _
instance showConfirmConnectionResponse :: Show ConfirmConnectionResponse where
  show = genericShow
instance decodeConfirmConnectionResponse :: Decode ConfirmConnectionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmConnectionResponse :: Encode ConfirmConnectionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the ConfirmPrivateVirtualInterface operation.</p>
newtype ConfirmPrivateVirtualInterfaceRequest = ConfirmPrivateVirtualInterfaceRequest 
  { "VirtualInterfaceId'" :: (VirtualInterfaceId)
  , "VirtualGatewayId'" :: NullOrUndefined.NullOrUndefined (VirtualGatewayId)
  , "DirectConnectGatewayId'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayId)
  }
derive instance newtypeConfirmPrivateVirtualInterfaceRequest :: Newtype ConfirmPrivateVirtualInterfaceRequest _
derive instance repGenericConfirmPrivateVirtualInterfaceRequest :: Generic ConfirmPrivateVirtualInterfaceRequest _
instance showConfirmPrivateVirtualInterfaceRequest :: Show ConfirmPrivateVirtualInterfaceRequest where
  show = genericShow
instance decodeConfirmPrivateVirtualInterfaceRequest :: Decode ConfirmPrivateVirtualInterfaceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmPrivateVirtualInterfaceRequest :: Encode ConfirmPrivateVirtualInterfaceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when ConfirmPrivateVirtualInterface is called.</p>
newtype ConfirmPrivateVirtualInterfaceResponse = ConfirmPrivateVirtualInterfaceResponse 
  { "VirtualInterfaceState'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceState)
  }
derive instance newtypeConfirmPrivateVirtualInterfaceResponse :: Newtype ConfirmPrivateVirtualInterfaceResponse _
derive instance repGenericConfirmPrivateVirtualInterfaceResponse :: Generic ConfirmPrivateVirtualInterfaceResponse _
instance showConfirmPrivateVirtualInterfaceResponse :: Show ConfirmPrivateVirtualInterfaceResponse where
  show = genericShow
instance decodeConfirmPrivateVirtualInterfaceResponse :: Decode ConfirmPrivateVirtualInterfaceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmPrivateVirtualInterfaceResponse :: Encode ConfirmPrivateVirtualInterfaceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the ConfirmPublicVirtualInterface operation.</p>
newtype ConfirmPublicVirtualInterfaceRequest = ConfirmPublicVirtualInterfaceRequest 
  { "VirtualInterfaceId'" :: (VirtualInterfaceId)
  }
derive instance newtypeConfirmPublicVirtualInterfaceRequest :: Newtype ConfirmPublicVirtualInterfaceRequest _
derive instance repGenericConfirmPublicVirtualInterfaceRequest :: Generic ConfirmPublicVirtualInterfaceRequest _
instance showConfirmPublicVirtualInterfaceRequest :: Show ConfirmPublicVirtualInterfaceRequest where
  show = genericShow
instance decodeConfirmPublicVirtualInterfaceRequest :: Decode ConfirmPublicVirtualInterfaceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmPublicVirtualInterfaceRequest :: Encode ConfirmPublicVirtualInterfaceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when ConfirmPublicVirtualInterface is called.</p>
newtype ConfirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse 
  { "VirtualInterfaceState'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceState)
  }
derive instance newtypeConfirmPublicVirtualInterfaceResponse :: Newtype ConfirmPublicVirtualInterfaceResponse _
derive instance repGenericConfirmPublicVirtualInterfaceResponse :: Generic ConfirmPublicVirtualInterfaceResponse _
instance showConfirmPublicVirtualInterfaceResponse :: Show ConfirmPublicVirtualInterfaceResponse where
  show = genericShow
instance decodeConfirmPublicVirtualInterfaceResponse :: Decode ConfirmPublicVirtualInterfaceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmPublicVirtualInterfaceResponse :: Encode ConfirmPublicVirtualInterfaceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A connection represents the physical network connection between the AWS Direct Connect location and the customer.</p>
newtype Connection = Connection 
  { "OwnerAccount'" :: NullOrUndefined.NullOrUndefined (OwnerAccount)
  , "ConnectionId'" :: NullOrUndefined.NullOrUndefined (ConnectionId)
  , "ConnectionName'" :: NullOrUndefined.NullOrUndefined (ConnectionName)
  , "ConnectionState'" :: NullOrUndefined.NullOrUndefined (ConnectionState)
  , "Region'" :: NullOrUndefined.NullOrUndefined (Region)
  , "Location'" :: NullOrUndefined.NullOrUndefined (LocationCode)
  , "Bandwidth'" :: NullOrUndefined.NullOrUndefined (Bandwidth)
  , "Vlan'" :: NullOrUndefined.NullOrUndefined (VLAN)
  , "PartnerName'" :: NullOrUndefined.NullOrUndefined (PartnerName)
  , "LoaIssueTime'" :: NullOrUndefined.NullOrUndefined (LoaIssueTime)
  , "LagId'" :: NullOrUndefined.NullOrUndefined (LagId)
  , "AwsDevice'" :: NullOrUndefined.NullOrUndefined (AwsDevice)
  }
derive instance newtypeConnection :: Newtype Connection _
derive instance repGenericConnection :: Generic Connection _
instance showConnection :: Show Connection where
  show = genericShow
instance decodeConnection :: Decode Connection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnection :: Encode Connection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ID of the connection. This field is also used as the ID type for operations that use multiple connection types (LAG, interconnect, and/or connection).</p> <p>Example: dxcon-fg5678gh</p> <p>Default: None</p>
newtype ConnectionId = ConnectionId String
derive instance newtypeConnectionId :: Newtype ConnectionId _
derive instance repGenericConnectionId :: Generic ConnectionId _
instance showConnectionId :: Show ConnectionId where
  show = genericShow
instance decodeConnectionId :: Decode ConnectionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionId :: Encode ConnectionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of connections.</p>
newtype ConnectionList = ConnectionList (Array Connection)
derive instance newtypeConnectionList :: Newtype ConnectionList _
derive instance repGenericConnectionList :: Generic ConnectionList _
instance showConnectionList :: Show ConnectionList where
  show = genericShow
instance decodeConnectionList :: Decode ConnectionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionList :: Encode ConnectionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of the connection.</p> <p>Example: "<i>My Connection to AWS</i>"</p> <p>Default: None</p>
newtype ConnectionName = ConnectionName String
derive instance newtypeConnectionName :: Newtype ConnectionName _
derive instance repGenericConnectionName :: Generic ConnectionName _
instance showConnectionName :: Show ConnectionName where
  show = genericShow
instance decodeConnectionName :: Decode ConnectionName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionName :: Encode ConnectionName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>State of the connection.</p> <ul> <li> <p> <b>Ordering</b>: The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.</p> </li> <li> <p> <b>Requested</b>: The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.</p> </li> <li> <p> <b>Pending</b>: The connection has been approved, and is being initialized.</p> </li> <li> <p> <b>Available</b>: The network link is up, and the connection is ready for use.</p> </li> <li> <p> <b>Down</b>: The network link is down.</p> </li> <li> <p> <b>Deleting</b>: The connection is in the process of being deleted.</p> </li> <li> <p> <b>Deleted</b>: The connection has been deleted.</p> </li> <li> <p> <b>Rejected</b>: A hosted connection in the 'Ordering' state will enter the 'Rejected' state if it is deleted by the end customer.</p> </li> </ul>
newtype ConnectionState = ConnectionState String
derive instance newtypeConnectionState :: Newtype ConnectionState _
derive instance repGenericConnectionState :: Generic ConnectionState _
instance showConnectionState :: Show ConnectionState where
  show = genericShow
instance decodeConnectionState :: Decode ConnectionState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionState :: Encode ConnectionState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing a list of connections.</p>
newtype Connections = Connections 
  { "Connections'" :: NullOrUndefined.NullOrUndefined (ConnectionList)
  }
derive instance newtypeConnections :: Newtype Connections _
derive instance repGenericConnections :: Generic Connections _
instance showConnections :: Show Connections where
  show = genericShow
instance decodeConnections :: Decode Connections where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnections :: Encode Connections where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Count = Count Int
derive instance newtypeCount :: Newtype Count _
derive instance repGenericCount :: Generic Count _
instance showCount :: Show Count where
  show = genericShow
instance decodeCount :: Decode Count where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCount :: Encode Count where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the CreateBGPPeer operation.</p>
newtype CreateBGPPeerRequest = CreateBGPPeerRequest 
  { "VirtualInterfaceId'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceId)
  , "NewBGPPeer'" :: NullOrUndefined.NullOrUndefined (NewBGPPeer)
  }
derive instance newtypeCreateBGPPeerRequest :: Newtype CreateBGPPeerRequest _
derive instance repGenericCreateBGPPeerRequest :: Generic CreateBGPPeerRequest _
instance showCreateBGPPeerRequest :: Show CreateBGPPeerRequest where
  show = genericShow
instance decodeCreateBGPPeerRequest :: Decode CreateBGPPeerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBGPPeerRequest :: Encode CreateBGPPeerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when CreateBGPPeer is called.</p>
newtype CreateBGPPeerResponse = CreateBGPPeerResponse 
  { "VirtualInterface'" :: NullOrUndefined.NullOrUndefined (VirtualInterface)
  }
derive instance newtypeCreateBGPPeerResponse :: Newtype CreateBGPPeerResponse _
derive instance repGenericCreateBGPPeerResponse :: Generic CreateBGPPeerResponse _
instance showCreateBGPPeerResponse :: Show CreateBGPPeerResponse where
  show = genericShow
instance decodeCreateBGPPeerResponse :: Decode CreateBGPPeerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBGPPeerResponse :: Encode CreateBGPPeerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the CreateConnection operation.</p>
newtype CreateConnectionRequest = CreateConnectionRequest 
  { "Location'" :: (LocationCode)
  , "Bandwidth'" :: (Bandwidth)
  , "ConnectionName'" :: (ConnectionName)
  , "LagId'" :: NullOrUndefined.NullOrUndefined (LagId)
  }
derive instance newtypeCreateConnectionRequest :: Newtype CreateConnectionRequest _
derive instance repGenericCreateConnectionRequest :: Generic CreateConnectionRequest _
instance showCreateConnectionRequest :: Show CreateConnectionRequest where
  show = genericShow
instance decodeCreateConnectionRequest :: Decode CreateConnectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateConnectionRequest :: Encode CreateConnectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the CreateDirectConnectGatewayAssociation operation.</p>
newtype CreateDirectConnectGatewayAssociationRequest = CreateDirectConnectGatewayAssociationRequest 
  { "DirectConnectGatewayId'" :: (DirectConnectGatewayId)
  , "VirtualGatewayId'" :: (VirtualGatewayId)
  }
derive instance newtypeCreateDirectConnectGatewayAssociationRequest :: Newtype CreateDirectConnectGatewayAssociationRequest _
derive instance repGenericCreateDirectConnectGatewayAssociationRequest :: Generic CreateDirectConnectGatewayAssociationRequest _
instance showCreateDirectConnectGatewayAssociationRequest :: Show CreateDirectConnectGatewayAssociationRequest where
  show = genericShow
instance decodeCreateDirectConnectGatewayAssociationRequest :: Decode CreateDirectConnectGatewayAssociationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDirectConnectGatewayAssociationRequest :: Encode CreateDirectConnectGatewayAssociationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the response from the CreateDirectConnectGatewayAssociation API call</p>
newtype CreateDirectConnectGatewayAssociationResult = CreateDirectConnectGatewayAssociationResult 
  { "DirectConnectGatewayAssociation'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayAssociation)
  }
derive instance newtypeCreateDirectConnectGatewayAssociationResult :: Newtype CreateDirectConnectGatewayAssociationResult _
derive instance repGenericCreateDirectConnectGatewayAssociationResult :: Generic CreateDirectConnectGatewayAssociationResult _
instance showCreateDirectConnectGatewayAssociationResult :: Show CreateDirectConnectGatewayAssociationResult where
  show = genericShow
instance decodeCreateDirectConnectGatewayAssociationResult :: Decode CreateDirectConnectGatewayAssociationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDirectConnectGatewayAssociationResult :: Encode CreateDirectConnectGatewayAssociationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the CreateDirectConnectGateway operation.</p>
newtype CreateDirectConnectGatewayRequest = CreateDirectConnectGatewayRequest 
  { "DirectConnectGatewayName'" :: (DirectConnectGatewayName)
  , "AmazonSideAsn'" :: NullOrUndefined.NullOrUndefined (LongAsn)
  }
derive instance newtypeCreateDirectConnectGatewayRequest :: Newtype CreateDirectConnectGatewayRequest _
derive instance repGenericCreateDirectConnectGatewayRequest :: Generic CreateDirectConnectGatewayRequest _
instance showCreateDirectConnectGatewayRequest :: Show CreateDirectConnectGatewayRequest where
  show = genericShow
instance decodeCreateDirectConnectGatewayRequest :: Decode CreateDirectConnectGatewayRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDirectConnectGatewayRequest :: Encode CreateDirectConnectGatewayRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the response from the CreateDirectConnectGateway API call</p>
newtype CreateDirectConnectGatewayResult = CreateDirectConnectGatewayResult 
  { "DirectConnectGateway'" :: NullOrUndefined.NullOrUndefined (DirectConnectGateway)
  }
derive instance newtypeCreateDirectConnectGatewayResult :: Newtype CreateDirectConnectGatewayResult _
derive instance repGenericCreateDirectConnectGatewayResult :: Generic CreateDirectConnectGatewayResult _
instance showCreateDirectConnectGatewayResult :: Show CreateDirectConnectGatewayResult where
  show = genericShow
instance decodeCreateDirectConnectGatewayResult :: Decode CreateDirectConnectGatewayResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDirectConnectGatewayResult :: Encode CreateDirectConnectGatewayResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the CreateInterconnect operation.</p>
newtype CreateInterconnectRequest = CreateInterconnectRequest 
  { "InterconnectName'" :: (InterconnectName)
  , "Bandwidth'" :: (Bandwidth)
  , "Location'" :: (LocationCode)
  , "LagId'" :: NullOrUndefined.NullOrUndefined (LagId)
  }
derive instance newtypeCreateInterconnectRequest :: Newtype CreateInterconnectRequest _
derive instance repGenericCreateInterconnectRequest :: Generic CreateInterconnectRequest _
instance showCreateInterconnectRequest :: Show CreateInterconnectRequest where
  show = genericShow
instance decodeCreateInterconnectRequest :: Decode CreateInterconnectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInterconnectRequest :: Encode CreateInterconnectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the CreateLag operation.</p>
newtype CreateLagRequest = CreateLagRequest 
  { "NumberOfConnections'" :: (Count)
  , "Location'" :: (LocationCode)
  , "ConnectionsBandwidth'" :: (Bandwidth)
  , "LagName'" :: (LagName)
  , "ConnectionId'" :: NullOrUndefined.NullOrUndefined (ConnectionId)
  }
derive instance newtypeCreateLagRequest :: Newtype CreateLagRequest _
derive instance repGenericCreateLagRequest :: Generic CreateLagRequest _
instance showCreateLagRequest :: Show CreateLagRequest where
  show = genericShow
instance decodeCreateLagRequest :: Decode CreateLagRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLagRequest :: Encode CreateLagRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the CreatePrivateVirtualInterface operation.</p>
newtype CreatePrivateVirtualInterfaceRequest = CreatePrivateVirtualInterfaceRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "NewPrivateVirtualInterface'" :: (NewPrivateVirtualInterface)
  }
derive instance newtypeCreatePrivateVirtualInterfaceRequest :: Newtype CreatePrivateVirtualInterfaceRequest _
derive instance repGenericCreatePrivateVirtualInterfaceRequest :: Generic CreatePrivateVirtualInterfaceRequest _
instance showCreatePrivateVirtualInterfaceRequest :: Show CreatePrivateVirtualInterfaceRequest where
  show = genericShow
instance decodeCreatePrivateVirtualInterfaceRequest :: Decode CreatePrivateVirtualInterfaceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePrivateVirtualInterfaceRequest :: Encode CreatePrivateVirtualInterfaceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the CreatePublicVirtualInterface operation.</p>
newtype CreatePublicVirtualInterfaceRequest = CreatePublicVirtualInterfaceRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "NewPublicVirtualInterface'" :: (NewPublicVirtualInterface)
  }
derive instance newtypeCreatePublicVirtualInterfaceRequest :: Newtype CreatePublicVirtualInterfaceRequest _
derive instance repGenericCreatePublicVirtualInterfaceRequest :: Generic CreatePublicVirtualInterfaceRequest _
instance showCreatePublicVirtualInterfaceRequest :: Show CreatePublicVirtualInterfaceRequest where
  show = genericShow
instance decodeCreatePublicVirtualInterfaceRequest :: Decode CreatePublicVirtualInterfaceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePublicVirtualInterfaceRequest :: Encode CreatePublicVirtualInterfaceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>IP address assigned to the customer interface.</p> <p>Example: 192.168.1.2/30 or 2001:db8::2/125</p>
newtype CustomerAddress = CustomerAddress String
derive instance newtypeCustomerAddress :: Newtype CustomerAddress _
derive instance repGenericCustomerAddress :: Generic CustomerAddress _
instance showCustomerAddress :: Show CustomerAddress where
  show = genericShow
instance decodeCustomerAddress :: Decode CustomerAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomerAddress :: Encode CustomerAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DeleteBGPPeer operation.</p>
newtype DeleteBGPPeerRequest = DeleteBGPPeerRequest 
  { "VirtualInterfaceId'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceId)
  , "Asn'" :: NullOrUndefined.NullOrUndefined (ASN)
  , "CustomerAddress'" :: NullOrUndefined.NullOrUndefined (CustomerAddress)
  }
derive instance newtypeDeleteBGPPeerRequest :: Newtype DeleteBGPPeerRequest _
derive instance repGenericDeleteBGPPeerRequest :: Generic DeleteBGPPeerRequest _
instance showDeleteBGPPeerRequest :: Show DeleteBGPPeerRequest where
  show = genericShow
instance decodeDeleteBGPPeerRequest :: Decode DeleteBGPPeerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBGPPeerRequest :: Encode DeleteBGPPeerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when DeleteBGPPeer is called.</p>
newtype DeleteBGPPeerResponse = DeleteBGPPeerResponse 
  { "VirtualInterface'" :: NullOrUndefined.NullOrUndefined (VirtualInterface)
  }
derive instance newtypeDeleteBGPPeerResponse :: Newtype DeleteBGPPeerResponse _
derive instance repGenericDeleteBGPPeerResponse :: Generic DeleteBGPPeerResponse _
instance showDeleteBGPPeerResponse :: Show DeleteBGPPeerResponse where
  show = genericShow
instance decodeDeleteBGPPeerResponse :: Decode DeleteBGPPeerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBGPPeerResponse :: Encode DeleteBGPPeerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DeleteConnection operation.</p>
newtype DeleteConnectionRequest = DeleteConnectionRequest 
  { "ConnectionId'" :: (ConnectionId)
  }
derive instance newtypeDeleteConnectionRequest :: Newtype DeleteConnectionRequest _
derive instance repGenericDeleteConnectionRequest :: Generic DeleteConnectionRequest _
instance showDeleteConnectionRequest :: Show DeleteConnectionRequest where
  show = genericShow
instance decodeDeleteConnectionRequest :: Decode DeleteConnectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConnectionRequest :: Encode DeleteConnectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DeleteDirectConnectGatewayAssociation operation.</p>
newtype DeleteDirectConnectGatewayAssociationRequest = DeleteDirectConnectGatewayAssociationRequest 
  { "DirectConnectGatewayId'" :: (DirectConnectGatewayId)
  , "VirtualGatewayId'" :: (VirtualGatewayId)
  }
derive instance newtypeDeleteDirectConnectGatewayAssociationRequest :: Newtype DeleteDirectConnectGatewayAssociationRequest _
derive instance repGenericDeleteDirectConnectGatewayAssociationRequest :: Generic DeleteDirectConnectGatewayAssociationRequest _
instance showDeleteDirectConnectGatewayAssociationRequest :: Show DeleteDirectConnectGatewayAssociationRequest where
  show = genericShow
instance decodeDeleteDirectConnectGatewayAssociationRequest :: Decode DeleteDirectConnectGatewayAssociationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDirectConnectGatewayAssociationRequest :: Encode DeleteDirectConnectGatewayAssociationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the response from the DeleteDirectConnectGatewayAssociation API call</p>
newtype DeleteDirectConnectGatewayAssociationResult = DeleteDirectConnectGatewayAssociationResult 
  { "DirectConnectGatewayAssociation'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayAssociation)
  }
derive instance newtypeDeleteDirectConnectGatewayAssociationResult :: Newtype DeleteDirectConnectGatewayAssociationResult _
derive instance repGenericDeleteDirectConnectGatewayAssociationResult :: Generic DeleteDirectConnectGatewayAssociationResult _
instance showDeleteDirectConnectGatewayAssociationResult :: Show DeleteDirectConnectGatewayAssociationResult where
  show = genericShow
instance decodeDeleteDirectConnectGatewayAssociationResult :: Decode DeleteDirectConnectGatewayAssociationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDirectConnectGatewayAssociationResult :: Encode DeleteDirectConnectGatewayAssociationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DeleteDirectConnectGateway operation.</p>
newtype DeleteDirectConnectGatewayRequest = DeleteDirectConnectGatewayRequest 
  { "DirectConnectGatewayId'" :: (DirectConnectGatewayId)
  }
derive instance newtypeDeleteDirectConnectGatewayRequest :: Newtype DeleteDirectConnectGatewayRequest _
derive instance repGenericDeleteDirectConnectGatewayRequest :: Generic DeleteDirectConnectGatewayRequest _
instance showDeleteDirectConnectGatewayRequest :: Show DeleteDirectConnectGatewayRequest where
  show = genericShow
instance decodeDeleteDirectConnectGatewayRequest :: Decode DeleteDirectConnectGatewayRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDirectConnectGatewayRequest :: Encode DeleteDirectConnectGatewayRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the response from the DeleteDirectConnectGateway API call</p>
newtype DeleteDirectConnectGatewayResult = DeleteDirectConnectGatewayResult 
  { "DirectConnectGateway'" :: NullOrUndefined.NullOrUndefined (DirectConnectGateway)
  }
derive instance newtypeDeleteDirectConnectGatewayResult :: Newtype DeleteDirectConnectGatewayResult _
derive instance repGenericDeleteDirectConnectGatewayResult :: Generic DeleteDirectConnectGatewayResult _
instance showDeleteDirectConnectGatewayResult :: Show DeleteDirectConnectGatewayResult where
  show = genericShow
instance decodeDeleteDirectConnectGatewayResult :: Decode DeleteDirectConnectGatewayResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDirectConnectGatewayResult :: Encode DeleteDirectConnectGatewayResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DeleteInterconnect operation.</p>
newtype DeleteInterconnectRequest = DeleteInterconnectRequest 
  { "InterconnectId'" :: (InterconnectId)
  }
derive instance newtypeDeleteInterconnectRequest :: Newtype DeleteInterconnectRequest _
derive instance repGenericDeleteInterconnectRequest :: Generic DeleteInterconnectRequest _
instance showDeleteInterconnectRequest :: Show DeleteInterconnectRequest where
  show = genericShow
instance decodeDeleteInterconnectRequest :: Decode DeleteInterconnectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteInterconnectRequest :: Encode DeleteInterconnectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when DeleteInterconnect is called.</p>
newtype DeleteInterconnectResponse = DeleteInterconnectResponse 
  { "InterconnectState'" :: NullOrUndefined.NullOrUndefined (InterconnectState)
  }
derive instance newtypeDeleteInterconnectResponse :: Newtype DeleteInterconnectResponse _
derive instance repGenericDeleteInterconnectResponse :: Generic DeleteInterconnectResponse _
instance showDeleteInterconnectResponse :: Show DeleteInterconnectResponse where
  show = genericShow
instance decodeDeleteInterconnectResponse :: Decode DeleteInterconnectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteInterconnectResponse :: Encode DeleteInterconnectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DeleteLag operation.</p>
newtype DeleteLagRequest = DeleteLagRequest 
  { "LagId'" :: (LagId)
  }
derive instance newtypeDeleteLagRequest :: Newtype DeleteLagRequest _
derive instance repGenericDeleteLagRequest :: Generic DeleteLagRequest _
instance showDeleteLagRequest :: Show DeleteLagRequest where
  show = genericShow
instance decodeDeleteLagRequest :: Decode DeleteLagRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLagRequest :: Encode DeleteLagRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DeleteVirtualInterface operation.</p>
newtype DeleteVirtualInterfaceRequest = DeleteVirtualInterfaceRequest 
  { "VirtualInterfaceId'" :: (VirtualInterfaceId)
  }
derive instance newtypeDeleteVirtualInterfaceRequest :: Newtype DeleteVirtualInterfaceRequest _
derive instance repGenericDeleteVirtualInterfaceRequest :: Generic DeleteVirtualInterfaceRequest _
instance showDeleteVirtualInterfaceRequest :: Show DeleteVirtualInterfaceRequest where
  show = genericShow
instance decodeDeleteVirtualInterfaceRequest :: Decode DeleteVirtualInterfaceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVirtualInterfaceRequest :: Encode DeleteVirtualInterfaceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when DeleteVirtualInterface is called.</p>
newtype DeleteVirtualInterfaceResponse = DeleteVirtualInterfaceResponse 
  { "VirtualInterfaceState'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceState)
  }
derive instance newtypeDeleteVirtualInterfaceResponse :: Newtype DeleteVirtualInterfaceResponse _
derive instance repGenericDeleteVirtualInterfaceResponse :: Generic DeleteVirtualInterfaceResponse _
instance showDeleteVirtualInterfaceResponse :: Show DeleteVirtualInterfaceResponse where
  show = genericShow
instance decodeDeleteVirtualInterfaceResponse :: Decode DeleteVirtualInterfaceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVirtualInterfaceResponse :: Encode DeleteVirtualInterfaceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeConnectionLoa operation.</p>
newtype DescribeConnectionLoaRequest = DescribeConnectionLoaRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "ProviderName'" :: NullOrUndefined.NullOrUndefined (ProviderName)
  , "LoaContentType'" :: NullOrUndefined.NullOrUndefined (LoaContentType)
  }
derive instance newtypeDescribeConnectionLoaRequest :: Newtype DescribeConnectionLoaRequest _
derive instance repGenericDescribeConnectionLoaRequest :: Generic DescribeConnectionLoaRequest _
instance showDescribeConnectionLoaRequest :: Show DescribeConnectionLoaRequest where
  show = genericShow
instance decodeDescribeConnectionLoaRequest :: Decode DescribeConnectionLoaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConnectionLoaRequest :: Encode DescribeConnectionLoaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when DescribeConnectionLoa is called.</p>
newtype DescribeConnectionLoaResponse = DescribeConnectionLoaResponse 
  { "Loa'" :: NullOrUndefined.NullOrUndefined (Loa)
  }
derive instance newtypeDescribeConnectionLoaResponse :: Newtype DescribeConnectionLoaResponse _
derive instance repGenericDescribeConnectionLoaResponse :: Generic DescribeConnectionLoaResponse _
instance showDescribeConnectionLoaResponse :: Show DescribeConnectionLoaResponse where
  show = genericShow
instance decodeDescribeConnectionLoaResponse :: Decode DescribeConnectionLoaResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConnectionLoaResponse :: Encode DescribeConnectionLoaResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeConnectionsOnInterconnect operation.</p>
newtype DescribeConnectionsOnInterconnectRequest = DescribeConnectionsOnInterconnectRequest 
  { "InterconnectId'" :: (InterconnectId)
  }
derive instance newtypeDescribeConnectionsOnInterconnectRequest :: Newtype DescribeConnectionsOnInterconnectRequest _
derive instance repGenericDescribeConnectionsOnInterconnectRequest :: Generic DescribeConnectionsOnInterconnectRequest _
instance showDescribeConnectionsOnInterconnectRequest :: Show DescribeConnectionsOnInterconnectRequest where
  show = genericShow
instance decodeDescribeConnectionsOnInterconnectRequest :: Decode DescribeConnectionsOnInterconnectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConnectionsOnInterconnectRequest :: Encode DescribeConnectionsOnInterconnectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeConnections operation.</p>
newtype DescribeConnectionsRequest = DescribeConnectionsRequest 
  { "ConnectionId'" :: NullOrUndefined.NullOrUndefined (ConnectionId)
  }
derive instance newtypeDescribeConnectionsRequest :: Newtype DescribeConnectionsRequest _
derive instance repGenericDescribeConnectionsRequest :: Generic DescribeConnectionsRequest _
instance showDescribeConnectionsRequest :: Show DescribeConnectionsRequest where
  show = genericShow
instance decodeDescribeConnectionsRequest :: Decode DescribeConnectionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConnectionsRequest :: Encode DescribeConnectionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeDirectConnectGatewayAssociations operation.</p>
newtype DescribeDirectConnectGatewayAssociationsRequest = DescribeDirectConnectGatewayAssociationsRequest 
  { "DirectConnectGatewayId'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayId)
  , "VirtualGatewayId'" :: NullOrUndefined.NullOrUndefined (VirtualGatewayId)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResultSetSize)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeDirectConnectGatewayAssociationsRequest :: Newtype DescribeDirectConnectGatewayAssociationsRequest _
derive instance repGenericDescribeDirectConnectGatewayAssociationsRequest :: Generic DescribeDirectConnectGatewayAssociationsRequest _
instance showDescribeDirectConnectGatewayAssociationsRequest :: Show DescribeDirectConnectGatewayAssociationsRequest where
  show = genericShow
instance decodeDescribeDirectConnectGatewayAssociationsRequest :: Decode DescribeDirectConnectGatewayAssociationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDirectConnectGatewayAssociationsRequest :: Encode DescribeDirectConnectGatewayAssociationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the response from the DescribeDirectConnectGatewayAssociations API call</p>
newtype DescribeDirectConnectGatewayAssociationsResult = DescribeDirectConnectGatewayAssociationsResult 
  { "DirectConnectGatewayAssociations'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayAssociationList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeDirectConnectGatewayAssociationsResult :: Newtype DescribeDirectConnectGatewayAssociationsResult _
derive instance repGenericDescribeDirectConnectGatewayAssociationsResult :: Generic DescribeDirectConnectGatewayAssociationsResult _
instance showDescribeDirectConnectGatewayAssociationsResult :: Show DescribeDirectConnectGatewayAssociationsResult where
  show = genericShow
instance decodeDescribeDirectConnectGatewayAssociationsResult :: Decode DescribeDirectConnectGatewayAssociationsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDirectConnectGatewayAssociationsResult :: Encode DescribeDirectConnectGatewayAssociationsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeDirectConnectGatewayAttachments operation.</p>
newtype DescribeDirectConnectGatewayAttachmentsRequest = DescribeDirectConnectGatewayAttachmentsRequest 
  { "DirectConnectGatewayId'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayId)
  , "VirtualInterfaceId'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceId)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResultSetSize)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeDirectConnectGatewayAttachmentsRequest :: Newtype DescribeDirectConnectGatewayAttachmentsRequest _
derive instance repGenericDescribeDirectConnectGatewayAttachmentsRequest :: Generic DescribeDirectConnectGatewayAttachmentsRequest _
instance showDescribeDirectConnectGatewayAttachmentsRequest :: Show DescribeDirectConnectGatewayAttachmentsRequest where
  show = genericShow
instance decodeDescribeDirectConnectGatewayAttachmentsRequest :: Decode DescribeDirectConnectGatewayAttachmentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDirectConnectGatewayAttachmentsRequest :: Encode DescribeDirectConnectGatewayAttachmentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the response from the DescribeDirectConnectGatewayAttachments API call</p>
newtype DescribeDirectConnectGatewayAttachmentsResult = DescribeDirectConnectGatewayAttachmentsResult 
  { "DirectConnectGatewayAttachments'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayAttachmentList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeDirectConnectGatewayAttachmentsResult :: Newtype DescribeDirectConnectGatewayAttachmentsResult _
derive instance repGenericDescribeDirectConnectGatewayAttachmentsResult :: Generic DescribeDirectConnectGatewayAttachmentsResult _
instance showDescribeDirectConnectGatewayAttachmentsResult :: Show DescribeDirectConnectGatewayAttachmentsResult where
  show = genericShow
instance decodeDescribeDirectConnectGatewayAttachmentsResult :: Decode DescribeDirectConnectGatewayAttachmentsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDirectConnectGatewayAttachmentsResult :: Encode DescribeDirectConnectGatewayAttachmentsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeDirectConnectGateways operation.</p>
newtype DescribeDirectConnectGatewaysRequest = DescribeDirectConnectGatewaysRequest 
  { "DirectConnectGatewayId'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayId)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResultSetSize)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeDirectConnectGatewaysRequest :: Newtype DescribeDirectConnectGatewaysRequest _
derive instance repGenericDescribeDirectConnectGatewaysRequest :: Generic DescribeDirectConnectGatewaysRequest _
instance showDescribeDirectConnectGatewaysRequest :: Show DescribeDirectConnectGatewaysRequest where
  show = genericShow
instance decodeDescribeDirectConnectGatewaysRequest :: Decode DescribeDirectConnectGatewaysRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDirectConnectGatewaysRequest :: Encode DescribeDirectConnectGatewaysRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the response from the DescribeDirectConnectGateways API call</p>
newtype DescribeDirectConnectGatewaysResult = DescribeDirectConnectGatewaysResult 
  { "DirectConnectGateways'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeDirectConnectGatewaysResult :: Newtype DescribeDirectConnectGatewaysResult _
derive instance repGenericDescribeDirectConnectGatewaysResult :: Generic DescribeDirectConnectGatewaysResult _
instance showDescribeDirectConnectGatewaysResult :: Show DescribeDirectConnectGatewaysResult where
  show = genericShow
instance decodeDescribeDirectConnectGatewaysResult :: Decode DescribeDirectConnectGatewaysResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDirectConnectGatewaysResult :: Encode DescribeDirectConnectGatewaysResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeHostedConnections operation.</p>
newtype DescribeHostedConnectionsRequest = DescribeHostedConnectionsRequest 
  { "ConnectionId'" :: (ConnectionId)
  }
derive instance newtypeDescribeHostedConnectionsRequest :: Newtype DescribeHostedConnectionsRequest _
derive instance repGenericDescribeHostedConnectionsRequest :: Generic DescribeHostedConnectionsRequest _
instance showDescribeHostedConnectionsRequest :: Show DescribeHostedConnectionsRequest where
  show = genericShow
instance decodeDescribeHostedConnectionsRequest :: Decode DescribeHostedConnectionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeHostedConnectionsRequest :: Encode DescribeHostedConnectionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeInterconnectLoa operation.</p>
newtype DescribeInterconnectLoaRequest = DescribeInterconnectLoaRequest 
  { "InterconnectId'" :: (InterconnectId)
  , "ProviderName'" :: NullOrUndefined.NullOrUndefined (ProviderName)
  , "LoaContentType'" :: NullOrUndefined.NullOrUndefined (LoaContentType)
  }
derive instance newtypeDescribeInterconnectLoaRequest :: Newtype DescribeInterconnectLoaRequest _
derive instance repGenericDescribeInterconnectLoaRequest :: Generic DescribeInterconnectLoaRequest _
instance showDescribeInterconnectLoaRequest :: Show DescribeInterconnectLoaRequest where
  show = genericShow
instance decodeDescribeInterconnectLoaRequest :: Decode DescribeInterconnectLoaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInterconnectLoaRequest :: Encode DescribeInterconnectLoaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when DescribeInterconnectLoa is called.</p>
newtype DescribeInterconnectLoaResponse = DescribeInterconnectLoaResponse 
  { "Loa'" :: NullOrUndefined.NullOrUndefined (Loa)
  }
derive instance newtypeDescribeInterconnectLoaResponse :: Newtype DescribeInterconnectLoaResponse _
derive instance repGenericDescribeInterconnectLoaResponse :: Generic DescribeInterconnectLoaResponse _
instance showDescribeInterconnectLoaResponse :: Show DescribeInterconnectLoaResponse where
  show = genericShow
instance decodeDescribeInterconnectLoaResponse :: Decode DescribeInterconnectLoaResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInterconnectLoaResponse :: Encode DescribeInterconnectLoaResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeInterconnects operation.</p>
newtype DescribeInterconnectsRequest = DescribeInterconnectsRequest 
  { "InterconnectId'" :: NullOrUndefined.NullOrUndefined (InterconnectId)
  }
derive instance newtypeDescribeInterconnectsRequest :: Newtype DescribeInterconnectsRequest _
derive instance repGenericDescribeInterconnectsRequest :: Generic DescribeInterconnectsRequest _
instance showDescribeInterconnectsRequest :: Show DescribeInterconnectsRequest where
  show = genericShow
instance decodeDescribeInterconnectsRequest :: Decode DescribeInterconnectsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInterconnectsRequest :: Encode DescribeInterconnectsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeLags operation.</p>
newtype DescribeLagsRequest = DescribeLagsRequest 
  { "LagId'" :: NullOrUndefined.NullOrUndefined (LagId)
  }
derive instance newtypeDescribeLagsRequest :: Newtype DescribeLagsRequest _
derive instance repGenericDescribeLagsRequest :: Generic DescribeLagsRequest _
instance showDescribeLagsRequest :: Show DescribeLagsRequest where
  show = genericShow
instance decodeDescribeLagsRequest :: Decode DescribeLagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLagsRequest :: Encode DescribeLagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeLoa operation.</p>
newtype DescribeLoaRequest = DescribeLoaRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "ProviderName'" :: NullOrUndefined.NullOrUndefined (ProviderName)
  , "LoaContentType'" :: NullOrUndefined.NullOrUndefined (LoaContentType)
  }
derive instance newtypeDescribeLoaRequest :: Newtype DescribeLoaRequest _
derive instance repGenericDescribeLoaRequest :: Generic DescribeLoaRequest _
instance showDescribeLoaRequest :: Show DescribeLoaRequest where
  show = genericShow
instance decodeDescribeLoaRequest :: Decode DescribeLoaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLoaRequest :: Encode DescribeLoaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeTags operation.</p>
newtype DescribeTagsRequest = DescribeTagsRequest 
  { "ResourceArns'" :: (ResourceArnList)
  }
derive instance newtypeDescribeTagsRequest :: Newtype DescribeTagsRequest _
derive instance repGenericDescribeTagsRequest :: Generic DescribeTagsRequest _
instance showDescribeTagsRequest :: Show DescribeTagsRequest where
  show = genericShow
instance decodeDescribeTagsRequest :: Decode DescribeTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsRequest :: Encode DescribeTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when DescribeTags is called.</p>
newtype DescribeTagsResponse = DescribeTagsResponse 
  { "ResourceTags'" :: NullOrUndefined.NullOrUndefined (ResourceTagList)
  }
derive instance newtypeDescribeTagsResponse :: Newtype DescribeTagsResponse _
derive instance repGenericDescribeTagsResponse :: Generic DescribeTagsResponse _
instance showDescribeTagsResponse :: Show DescribeTagsResponse where
  show = genericShow
instance decodeDescribeTagsResponse :: Decode DescribeTagsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsResponse :: Encode DescribeTagsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DescribeVirtualInterfaces operation.</p>
newtype DescribeVirtualInterfacesRequest = DescribeVirtualInterfacesRequest 
  { "ConnectionId'" :: NullOrUndefined.NullOrUndefined (ConnectionId)
  , "VirtualInterfaceId'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceId)
  }
derive instance newtypeDescribeVirtualInterfacesRequest :: Newtype DescribeVirtualInterfacesRequest _
derive instance repGenericDescribeVirtualInterfacesRequest :: Generic DescribeVirtualInterfacesRequest _
instance showDescribeVirtualInterfacesRequest :: Show DescribeVirtualInterfacesRequest where
  show = genericShow
instance decodeDescribeVirtualInterfacesRequest :: Decode DescribeVirtualInterfacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeVirtualInterfacesRequest :: Encode DescribeVirtualInterfacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The API was called with invalid parameters. The error message will contain additional details about the cause.</p>
newtype DirectConnectClientException = DirectConnectClientException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDirectConnectClientException :: Newtype DirectConnectClientException _
derive instance repGenericDirectConnectClientException :: Generic DirectConnectClientException _
instance showDirectConnectClientException :: Show DirectConnectClientException where
  show = genericShow
instance decodeDirectConnectClientException :: Decode DirectConnectClientException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectClientException :: Encode DirectConnectClientException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A direct connect gateway is an intermediate object that enables you to connect virtual interfaces and virtual private gateways.</p>
newtype DirectConnectGateway = DirectConnectGateway 
  { "DirectConnectGatewayId'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayId)
  , "DirectConnectGatewayName'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayName)
  , "AmazonSideAsn'" :: NullOrUndefined.NullOrUndefined (LongAsn)
  , "OwnerAccount'" :: NullOrUndefined.NullOrUndefined (OwnerAccount)
  , "DirectConnectGatewayState'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayState)
  , "StateChangeError'" :: NullOrUndefined.NullOrUndefined (StateChangeError)
  }
derive instance newtypeDirectConnectGateway :: Newtype DirectConnectGateway _
derive instance repGenericDirectConnectGateway :: Generic DirectConnectGateway _
instance showDirectConnectGateway :: Show DirectConnectGateway where
  show = genericShow
instance decodeDirectConnectGateway :: Decode DirectConnectGateway where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectGateway :: Encode DirectConnectGateway where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The association between a direct connect gateway and virtual private gateway.</p>
newtype DirectConnectGatewayAssociation = DirectConnectGatewayAssociation 
  { "DirectConnectGatewayId'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayId)
  , "VirtualGatewayId'" :: NullOrUndefined.NullOrUndefined (VirtualGatewayId)
  , "VirtualGatewayRegion'" :: NullOrUndefined.NullOrUndefined (VirtualGatewayRegion)
  , "VirtualGatewayOwnerAccount'" :: NullOrUndefined.NullOrUndefined (OwnerAccount)
  , "AssociationState'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayAssociationState)
  , "StateChangeError'" :: NullOrUndefined.NullOrUndefined (StateChangeError)
  }
derive instance newtypeDirectConnectGatewayAssociation :: Newtype DirectConnectGatewayAssociation _
derive instance repGenericDirectConnectGatewayAssociation :: Generic DirectConnectGatewayAssociation _
instance showDirectConnectGatewayAssociation :: Show DirectConnectGatewayAssociation where
  show = genericShow
instance decodeDirectConnectGatewayAssociation :: Decode DirectConnectGatewayAssociation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectGatewayAssociation :: Encode DirectConnectGatewayAssociation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of direct connect gateway associations.</p>
newtype DirectConnectGatewayAssociationList = DirectConnectGatewayAssociationList (Array DirectConnectGatewayAssociation)
derive instance newtypeDirectConnectGatewayAssociationList :: Newtype DirectConnectGatewayAssociationList _
derive instance repGenericDirectConnectGatewayAssociationList :: Generic DirectConnectGatewayAssociationList _
instance showDirectConnectGatewayAssociationList :: Show DirectConnectGatewayAssociationList where
  show = genericShow
instance decodeDirectConnectGatewayAssociationList :: Decode DirectConnectGatewayAssociationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectGatewayAssociationList :: Encode DirectConnectGatewayAssociationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>State of the direct connect gateway association.</p> <ul> <li> <p> <b>Associating</b>: The initial state after calling <a>CreateDirectConnectGatewayAssociation</a>.</p> </li> <li> <p> <b>Associated</b>: The direct connect gateway and virtual private gateway are successfully associated and ready to pass traffic.</p> </li> <li> <p> <b>Disassociating</b>: The initial state after calling <a>DeleteDirectConnectGatewayAssociation</a>.</p> </li> <li> <p> <b>Disassociated</b>: The virtual private gateway is successfully disassociated from the direct connect gateway. Traffic flow between the direct connect gateway and virtual private gateway stops.</p> </li> </ul>
newtype DirectConnectGatewayAssociationState = DirectConnectGatewayAssociationState String
derive instance newtypeDirectConnectGatewayAssociationState :: Newtype DirectConnectGatewayAssociationState _
derive instance repGenericDirectConnectGatewayAssociationState :: Generic DirectConnectGatewayAssociationState _
instance showDirectConnectGatewayAssociationState :: Show DirectConnectGatewayAssociationState where
  show = genericShow
instance decodeDirectConnectGatewayAssociationState :: Decode DirectConnectGatewayAssociationState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectGatewayAssociationState :: Encode DirectConnectGatewayAssociationState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The association between a direct connect gateway and virtual interface.</p>
newtype DirectConnectGatewayAttachment = DirectConnectGatewayAttachment 
  { "DirectConnectGatewayId'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayId)
  , "VirtualInterfaceId'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceId)
  , "VirtualInterfaceRegion'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceRegion)
  , "VirtualInterfaceOwnerAccount'" :: NullOrUndefined.NullOrUndefined (OwnerAccount)
  , "AttachmentState'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayAttachmentState)
  , "StateChangeError'" :: NullOrUndefined.NullOrUndefined (StateChangeError)
  }
derive instance newtypeDirectConnectGatewayAttachment :: Newtype DirectConnectGatewayAttachment _
derive instance repGenericDirectConnectGatewayAttachment :: Generic DirectConnectGatewayAttachment _
instance showDirectConnectGatewayAttachment :: Show DirectConnectGatewayAttachment where
  show = genericShow
instance decodeDirectConnectGatewayAttachment :: Decode DirectConnectGatewayAttachment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectGatewayAttachment :: Encode DirectConnectGatewayAttachment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of direct connect gateway attachments.</p>
newtype DirectConnectGatewayAttachmentList = DirectConnectGatewayAttachmentList (Array DirectConnectGatewayAttachment)
derive instance newtypeDirectConnectGatewayAttachmentList :: Newtype DirectConnectGatewayAttachmentList _
derive instance repGenericDirectConnectGatewayAttachmentList :: Generic DirectConnectGatewayAttachmentList _
instance showDirectConnectGatewayAttachmentList :: Show DirectConnectGatewayAttachmentList where
  show = genericShow
instance decodeDirectConnectGatewayAttachmentList :: Decode DirectConnectGatewayAttachmentList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectGatewayAttachmentList :: Encode DirectConnectGatewayAttachmentList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>State of the direct connect gateway attachment.</p> <ul> <li> <p> <b>Attaching</b>: The initial state after a virtual interface is created using the direct connect gateway.</p> </li> <li> <p> <b>Attached</b>: The direct connect gateway and virtual interface are successfully attached and ready to pass traffic.</p> </li> <li> <p> <b>Detaching</b>: The initial state after calling <a>DeleteVirtualInterface</a> on a virtual interface that is attached to a direct connect gateway.</p> </li> <li> <p> <b>Detached</b>: The virtual interface is successfully detached from the direct connect gateway. Traffic flow between the direct connect gateway and virtual interface stops.</p> </li> </ul>
newtype DirectConnectGatewayAttachmentState = DirectConnectGatewayAttachmentState String
derive instance newtypeDirectConnectGatewayAttachmentState :: Newtype DirectConnectGatewayAttachmentState _
derive instance repGenericDirectConnectGatewayAttachmentState :: Generic DirectConnectGatewayAttachmentState _
instance showDirectConnectGatewayAttachmentState :: Show DirectConnectGatewayAttachmentState where
  show = genericShow
instance decodeDirectConnectGatewayAttachmentState :: Decode DirectConnectGatewayAttachmentState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectGatewayAttachmentState :: Encode DirectConnectGatewayAttachmentState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ID of the direct connect gateway.</p> <p>Example: "abcd1234-dcba-5678-be23-cdef9876ab45"</p>
newtype DirectConnectGatewayId = DirectConnectGatewayId String
derive instance newtypeDirectConnectGatewayId :: Newtype DirectConnectGatewayId _
derive instance repGenericDirectConnectGatewayId :: Generic DirectConnectGatewayId _
instance showDirectConnectGatewayId :: Show DirectConnectGatewayId where
  show = genericShow
instance decodeDirectConnectGatewayId :: Decode DirectConnectGatewayId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectGatewayId :: Encode DirectConnectGatewayId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of direct connect gateways.</p>
newtype DirectConnectGatewayList = DirectConnectGatewayList (Array DirectConnectGateway)
derive instance newtypeDirectConnectGatewayList :: Newtype DirectConnectGatewayList _
derive instance repGenericDirectConnectGatewayList :: Generic DirectConnectGatewayList _
instance showDirectConnectGatewayList :: Show DirectConnectGatewayList where
  show = genericShow
instance decodeDirectConnectGatewayList :: Decode DirectConnectGatewayList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectGatewayList :: Encode DirectConnectGatewayList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of the direct connect gateway.</p> <p>Example: "My direct connect gateway"</p> <p>Default: None</p>
newtype DirectConnectGatewayName = DirectConnectGatewayName String
derive instance newtypeDirectConnectGatewayName :: Newtype DirectConnectGatewayName _
derive instance repGenericDirectConnectGatewayName :: Generic DirectConnectGatewayName _
instance showDirectConnectGatewayName :: Show DirectConnectGatewayName where
  show = genericShow
instance decodeDirectConnectGatewayName :: Decode DirectConnectGatewayName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectGatewayName :: Encode DirectConnectGatewayName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>State of the direct connect gateway.</p> <ul> <li> <p> <b>Pending</b>: The initial state after calling <a>CreateDirectConnectGateway</a>.</p> </li> <li> <p> <b>Available</b>: The direct connect gateway is ready for use.</p> </li> <li> <p> <b>Deleting</b>: The initial state after calling <a>DeleteDirectConnectGateway</a>.</p> </li> <li> <p> <b>Deleted</b>: The direct connect gateway is deleted and cannot pass traffic.</p> </li> </ul>
newtype DirectConnectGatewayState = DirectConnectGatewayState String
derive instance newtypeDirectConnectGatewayState :: Newtype DirectConnectGatewayState _
derive instance repGenericDirectConnectGatewayState :: Generic DirectConnectGatewayState _
instance showDirectConnectGatewayState :: Show DirectConnectGatewayState where
  show = genericShow
instance decodeDirectConnectGatewayState :: Decode DirectConnectGatewayState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectGatewayState :: Encode DirectConnectGatewayState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A server-side error occurred during the API call. The error message will contain additional details about the cause.</p>
newtype DirectConnectServerException = DirectConnectServerException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDirectConnectServerException :: Newtype DirectConnectServerException _
derive instance repGenericDirectConnectServerException :: Generic DirectConnectServerException _
instance showDirectConnectServerException :: Show DirectConnectServerException where
  show = genericShow
instance decodeDirectConnectServerException :: Decode DirectConnectServerException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectConnectServerException :: Encode DirectConnectServerException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the DisassociateConnectionFromLag operation.</p>
newtype DisassociateConnectionFromLagRequest = DisassociateConnectionFromLagRequest 
  { "ConnectionId'" :: (ConnectionId)
  , "LagId'" :: (LagId)
  }
derive instance newtypeDisassociateConnectionFromLagRequest :: Newtype DisassociateConnectionFromLagRequest _
derive instance repGenericDisassociateConnectionFromLagRequest :: Generic DisassociateConnectionFromLagRequest _
instance showDisassociateConnectionFromLagRequest :: Show DisassociateConnectionFromLagRequest where
  show = genericShow
instance decodeDisassociateConnectionFromLagRequest :: Decode DisassociateConnectionFromLagRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateConnectionFromLagRequest :: Encode DisassociateConnectionFromLagRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A tag key was specified more than once.</p>
newtype DuplicateTagKeysException = DuplicateTagKeysException Types.NoArguments
derive instance newtypeDuplicateTagKeysException :: Newtype DuplicateTagKeysException _
derive instance repGenericDuplicateTagKeysException :: Generic DuplicateTagKeysException _
instance showDuplicateTagKeysException :: Show DuplicateTagKeysException where
  show = genericShow
instance decodeDuplicateTagKeysException :: Decode DuplicateTagKeysException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateTagKeysException :: Encode DuplicateTagKeysException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An interconnect is a connection that can host other connections.</p> <p>Like a standard AWS Direct Connect connection, an interconnect represents the physical connection between an AWS Direct Connect partner's network and a specific Direct Connect location. An AWS Direct Connect partner who owns an interconnect can provision hosted connections on the interconnect for their end customers, thereby providing the end customers with connectivity to AWS services.</p> <p>The resources of the interconnect, including bandwidth and VLAN numbers, are shared by all of the hosted connections on the interconnect, and the owner of the interconnect determines how these resources are assigned.</p>
newtype Interconnect = Interconnect 
  { "InterconnectId'" :: NullOrUndefined.NullOrUndefined (InterconnectId)
  , "InterconnectName'" :: NullOrUndefined.NullOrUndefined (InterconnectName)
  , "InterconnectState'" :: NullOrUndefined.NullOrUndefined (InterconnectState)
  , "Region'" :: NullOrUndefined.NullOrUndefined (Region)
  , "Location'" :: NullOrUndefined.NullOrUndefined (LocationCode)
  , "Bandwidth'" :: NullOrUndefined.NullOrUndefined (Bandwidth)
  , "LoaIssueTime'" :: NullOrUndefined.NullOrUndefined (LoaIssueTime)
  , "LagId'" :: NullOrUndefined.NullOrUndefined (LagId)
  , "AwsDevice'" :: NullOrUndefined.NullOrUndefined (AwsDevice)
  }
derive instance newtypeInterconnect :: Newtype Interconnect _
derive instance repGenericInterconnect :: Generic Interconnect _
instance showInterconnect :: Show Interconnect where
  show = genericShow
instance decodeInterconnect :: Decode Interconnect where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInterconnect :: Encode Interconnect where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ID of the interconnect.</p> <p>Example: dxcon-abc123</p>
newtype InterconnectId = InterconnectId String
derive instance newtypeInterconnectId :: Newtype InterconnectId _
derive instance repGenericInterconnectId :: Generic InterconnectId _
instance showInterconnectId :: Show InterconnectId where
  show = genericShow
instance decodeInterconnectId :: Decode InterconnectId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInterconnectId :: Encode InterconnectId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of interconnects.</p>
newtype InterconnectList = InterconnectList (Array Interconnect)
derive instance newtypeInterconnectList :: Newtype InterconnectList _
derive instance repGenericInterconnectList :: Generic InterconnectList _
instance showInterconnectList :: Show InterconnectList where
  show = genericShow
instance decodeInterconnectList :: Decode InterconnectList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInterconnectList :: Encode InterconnectList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of the interconnect.</p> <p>Example: "<i>1G Interconnect to AWS</i>"</p>
newtype InterconnectName = InterconnectName String
derive instance newtypeInterconnectName :: Newtype InterconnectName _
derive instance repGenericInterconnectName :: Generic InterconnectName _
instance showInterconnectName :: Show InterconnectName where
  show = genericShow
instance decodeInterconnectName :: Decode InterconnectName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInterconnectName :: Encode InterconnectName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>State of the interconnect.</p> <ul> <li> <p> <b>Requested</b>: The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.</p> </li> <li> <p> <b>Pending</b>: The interconnect has been approved, and is being initialized.</p> </li> <li> <p> <b>Available</b>: The network link is up, and the interconnect is ready for use.</p> </li> <li> <p> <b>Down</b>: The network link is down.</p> </li> <li> <p> <b>Deleting</b>: The interconnect is in the process of being deleted.</p> </li> <li> <p> <b>Deleted</b>: The interconnect has been deleted.</p> </li> </ul>
newtype InterconnectState = InterconnectState String
derive instance newtypeInterconnectState :: Newtype InterconnectState _
derive instance repGenericInterconnectState :: Generic InterconnectState _
instance showInterconnectState :: Show InterconnectState where
  show = genericShow
instance decodeInterconnectState :: Decode InterconnectState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInterconnectState :: Encode InterconnectState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing a list of interconnects.</p>
newtype Interconnects = Interconnects 
  { "Interconnects'" :: NullOrUndefined.NullOrUndefined (InterconnectList)
  }
derive instance newtypeInterconnects :: Newtype Interconnects _
derive instance repGenericInterconnects :: Generic Interconnects _
instance showInterconnects :: Show Interconnects where
  show = genericShow
instance decodeInterconnects :: Decode Interconnects where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInterconnects :: Encode Interconnects where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a link aggregation group (LAG). A LAG is a connection that uses the Link Aggregation Control Protocol (LACP) to logically aggregate a bundle of physical connections. Like an interconnect, it can host other connections. All connections in a LAG must terminate on the same physical AWS Direct Connect endpoint, and must be the same bandwidth.</p>
newtype Lag = Lag 
  { "ConnectionsBandwidth'" :: NullOrUndefined.NullOrUndefined (Bandwidth)
  , "NumberOfConnections'" :: NullOrUndefined.NullOrUndefined (Count)
  , "LagId'" :: NullOrUndefined.NullOrUndefined (LagId)
  , "OwnerAccount'" :: NullOrUndefined.NullOrUndefined (OwnerAccount)
  , "LagName'" :: NullOrUndefined.NullOrUndefined (LagName)
  , "LagState'" :: NullOrUndefined.NullOrUndefined (LagState)
  , "Location'" :: NullOrUndefined.NullOrUndefined (LocationCode)
  , "Region'" :: NullOrUndefined.NullOrUndefined (Region)
  , "MinimumLinks'" :: NullOrUndefined.NullOrUndefined (Count)
  , "AwsDevice'" :: NullOrUndefined.NullOrUndefined (AwsDevice)
  , "Connections'" :: NullOrUndefined.NullOrUndefined (ConnectionList)
  , "AllowsHostedConnections'" :: NullOrUndefined.NullOrUndefined (BooleanFlag)
  }
derive instance newtypeLag :: Newtype Lag _
derive instance repGenericLag :: Generic Lag _
instance showLag :: Show Lag where
  show = genericShow
instance decodeLag :: Decode Lag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLag :: Encode Lag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ID of the LAG.</p> <p>Example: dxlag-fg5678gh</p>
newtype LagId = LagId String
derive instance newtypeLagId :: Newtype LagId _
derive instance repGenericLagId :: Generic LagId _
instance showLagId :: Show LagId where
  show = genericShow
instance decodeLagId :: Decode LagId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLagId :: Encode LagId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of LAGs.</p>
newtype LagList = LagList (Array Lag)
derive instance newtypeLagList :: Newtype LagList _
derive instance repGenericLagList :: Generic LagList _
instance showLagList :: Show LagList where
  show = genericShow
instance decodeLagList :: Decode LagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLagList :: Encode LagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LagName = LagName String
derive instance newtypeLagName :: Newtype LagName _
derive instance repGenericLagName :: Generic LagName _
instance showLagName :: Show LagName where
  show = genericShow
instance decodeLagName :: Decode LagName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLagName :: Encode LagName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The state of the LAG.</p> <ul> <li> <p> <b>Requested</b>: The initial state of a LAG. The LAG stays in the requested state until the Letter of Authorization (LOA) is available.</p> </li> <li> <p> <b>Pending</b>: The LAG has been approved, and is being initialized.</p> </li> <li> <p> <b>Available</b>: The network link is established, and the LAG is ready for use.</p> </li> <li> <p> <b>Down</b>: The network link is down.</p> </li> <li> <p> <b>Deleting</b>: The LAG is in the process of being deleted.</p> </li> <li> <p> <b>Deleted</b>: The LAG has been deleted.</p> </li> </ul>
newtype LagState = LagState String
derive instance newtypeLagState :: Newtype LagState _
derive instance repGenericLagState :: Generic LagState _
instance showLagState :: Show LagState where
  show = genericShow
instance decodeLagState :: Decode LagState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLagState :: Encode LagState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing a list of LAGs.</p>
newtype Lags = Lags 
  { "Lags'" :: NullOrUndefined.NullOrUndefined (LagList)
  }
derive instance newtypeLags :: Newtype Lags _
derive instance repGenericLags :: Generic Lags _
instance showLags :: Show Lags where
  show = genericShow
instance decodeLags :: Decode Lags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLags :: Encode Lags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing the Letter of Authorization - Connecting Facility Assignment (LOA-CFA) for a connection.</p>
newtype Loa = Loa 
  { "LoaContent'" :: NullOrUndefined.NullOrUndefined (LoaContent)
  , "LoaContentType'" :: NullOrUndefined.NullOrUndefined (LoaContentType)
  }
derive instance newtypeLoa :: Newtype Loa _
derive instance repGenericLoa :: Generic Loa _
instance showLoa :: Show Loa where
  show = genericShow
instance decodeLoa :: Decode Loa where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoa :: Encode Loa where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The binary contents of the LOA-CFA document.</p>
newtype LoaContent = LoaContent String
derive instance newtypeLoaContent :: Newtype LoaContent _
derive instance repGenericLoaContent :: Generic LoaContent _
instance showLoaContent :: Show LoaContent where
  show = genericShow
instance decodeLoaContent :: Decode LoaContent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoaContent :: Encode LoaContent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A standard media type indicating the content type of the LOA-CFA document. Currently, the only supported value is "application/pdf".</p> <p>Default: application/pdf</p>
newtype LoaContentType = LoaContentType String
derive instance newtypeLoaContentType :: Newtype LoaContentType _
derive instance repGenericLoaContentType :: Generic LoaContentType _
instance showLoaContentType :: Show LoaContentType where
  show = genericShow
instance decodeLoaContentType :: Decode LoaContentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoaContentType :: Encode LoaContentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoaIssueTime = LoaIssueTime Number
derive instance newtypeLoaIssueTime :: Newtype LoaIssueTime _
derive instance repGenericLoaIssueTime :: Generic LoaIssueTime _
instance showLoaIssueTime :: Show LoaIssueTime where
  show = genericShow
instance decodeLoaIssueTime :: Decode LoaIssueTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoaIssueTime :: Encode LoaIssueTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An AWS Direct Connect location where connections and interconnects can be requested.</p>
newtype Location = Location 
  { "LocationCode'" :: NullOrUndefined.NullOrUndefined (LocationCode)
  , "LocationName'" :: NullOrUndefined.NullOrUndefined (LocationName)
  }
derive instance newtypeLocation :: Newtype Location _
derive instance repGenericLocation :: Generic Location _
instance showLocation :: Show Location where
  show = genericShow
instance decodeLocation :: Decode Location where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocation :: Encode Location where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Where the connection is located.</p> <p>Example: EqSV5</p> <p>Default: None</p>
newtype LocationCode = LocationCode String
derive instance newtypeLocationCode :: Newtype LocationCode _
derive instance repGenericLocationCode :: Generic LocationCode _
instance showLocationCode :: Show LocationCode where
  show = genericShow
instance decodeLocationCode :: Decode LocationCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocationCode :: Encode LocationCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LocationList = LocationList (Array Location)
derive instance newtypeLocationList :: Newtype LocationList _
derive instance repGenericLocationList :: Generic LocationList _
instance showLocationList :: Show LocationList where
  show = genericShow
instance decodeLocationList :: Decode LocationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocationList :: Encode LocationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LocationName = LocationName String
derive instance newtypeLocationName :: Newtype LocationName _
derive instance repGenericLocationName :: Generic LocationName _
instance showLocationName :: Show LocationName where
  show = genericShow
instance decodeLocationName :: Decode LocationName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocationName :: Encode LocationName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A location is a network facility where AWS Direct Connect routers are available to be connected. Generally, these are colocation hubs where many network providers have equipment, and where cross connects can be delivered. Locations include a name and facility code, and must be provided when creating a connection.</p>
newtype Locations = Locations 
  { "Locations'" :: NullOrUndefined.NullOrUndefined (LocationList)
  }
derive instance newtypeLocations :: Newtype Locations _
derive instance repGenericLocations :: Generic Locations _
instance showLocations :: Show Locations where
  show = genericShow
instance decodeLocations :: Decode Locations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocations :: Encode Locations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LongAsn = LongAsn Number
derive instance newtypeLongAsn :: Newtype LongAsn _
derive instance repGenericLongAsn :: Generic LongAsn _
instance showLongAsn :: Show LongAsn where
  show = genericShow
instance decodeLongAsn :: Decode LongAsn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLongAsn :: Encode LongAsn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Maximum number of objects to return per page.</p>
newtype MaxResultSetSize = MaxResultSetSize Int
derive instance newtypeMaxResultSetSize :: Newtype MaxResultSetSize _
derive instance repGenericMaxResultSetSize :: Generic MaxResultSetSize _
instance showMaxResultSetSize :: Show MaxResultSetSize where
  show = genericShow
instance decodeMaxResultSetSize :: Decode MaxResultSetSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResultSetSize :: Encode MaxResultSetSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing information about a new BGP peer.</p>
newtype NewBGPPeer = NewBGPPeer 
  { "Asn'" :: NullOrUndefined.NullOrUndefined (ASN)
  , "AuthKey'" :: NullOrUndefined.NullOrUndefined (BGPAuthKey)
  , "AddressFamily'" :: NullOrUndefined.NullOrUndefined (AddressFamily)
  , "AmazonAddress'" :: NullOrUndefined.NullOrUndefined (AmazonAddress)
  , "CustomerAddress'" :: NullOrUndefined.NullOrUndefined (CustomerAddress)
  }
derive instance newtypeNewBGPPeer :: Newtype NewBGPPeer _
derive instance repGenericNewBGPPeer :: Generic NewBGPPeer _
instance showNewBGPPeer :: Show NewBGPPeer where
  show = genericShow
instance decodeNewBGPPeer :: Decode NewBGPPeer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNewBGPPeer :: Encode NewBGPPeer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing information about a new private virtual interface.</p>
newtype NewPrivateVirtualInterface = NewPrivateVirtualInterface 
  { "VirtualInterfaceName'" :: (VirtualInterfaceName)
  , "Vlan'" :: (VLAN)
  , "Asn'" :: (ASN)
  , "AuthKey'" :: NullOrUndefined.NullOrUndefined (BGPAuthKey)
  , "AmazonAddress'" :: NullOrUndefined.NullOrUndefined (AmazonAddress)
  , "CustomerAddress'" :: NullOrUndefined.NullOrUndefined (CustomerAddress)
  , "AddressFamily'" :: NullOrUndefined.NullOrUndefined (AddressFamily)
  , "VirtualGatewayId'" :: NullOrUndefined.NullOrUndefined (VirtualGatewayId)
  , "DirectConnectGatewayId'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayId)
  }
derive instance newtypeNewPrivateVirtualInterface :: Newtype NewPrivateVirtualInterface _
derive instance repGenericNewPrivateVirtualInterface :: Generic NewPrivateVirtualInterface _
instance showNewPrivateVirtualInterface :: Show NewPrivateVirtualInterface where
  show = genericShow
instance decodeNewPrivateVirtualInterface :: Decode NewPrivateVirtualInterface where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNewPrivateVirtualInterface :: Encode NewPrivateVirtualInterface where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing information about a private virtual interface that will be provisioned on a connection.</p>
newtype NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation 
  { "VirtualInterfaceName'" :: (VirtualInterfaceName)
  , "Vlan'" :: (VLAN)
  , "Asn'" :: (ASN)
  , "AuthKey'" :: NullOrUndefined.NullOrUndefined (BGPAuthKey)
  , "AmazonAddress'" :: NullOrUndefined.NullOrUndefined (AmazonAddress)
  , "AddressFamily'" :: NullOrUndefined.NullOrUndefined (AddressFamily)
  , "CustomerAddress'" :: NullOrUndefined.NullOrUndefined (CustomerAddress)
  }
derive instance newtypeNewPrivateVirtualInterfaceAllocation :: Newtype NewPrivateVirtualInterfaceAllocation _
derive instance repGenericNewPrivateVirtualInterfaceAllocation :: Generic NewPrivateVirtualInterfaceAllocation _
instance showNewPrivateVirtualInterfaceAllocation :: Show NewPrivateVirtualInterfaceAllocation where
  show = genericShow
instance decodeNewPrivateVirtualInterfaceAllocation :: Decode NewPrivateVirtualInterfaceAllocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNewPrivateVirtualInterfaceAllocation :: Encode NewPrivateVirtualInterfaceAllocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing information about a new public virtual interface.</p>
newtype NewPublicVirtualInterface = NewPublicVirtualInterface 
  { "VirtualInterfaceName'" :: (VirtualInterfaceName)
  , "Vlan'" :: (VLAN)
  , "Asn'" :: (ASN)
  , "AuthKey'" :: NullOrUndefined.NullOrUndefined (BGPAuthKey)
  , "AmazonAddress'" :: NullOrUndefined.NullOrUndefined (AmazonAddress)
  , "CustomerAddress'" :: NullOrUndefined.NullOrUndefined (CustomerAddress)
  , "AddressFamily'" :: NullOrUndefined.NullOrUndefined (AddressFamily)
  , "RouteFilterPrefixes'" :: NullOrUndefined.NullOrUndefined (RouteFilterPrefixList)
  }
derive instance newtypeNewPublicVirtualInterface :: Newtype NewPublicVirtualInterface _
derive instance repGenericNewPublicVirtualInterface :: Generic NewPublicVirtualInterface _
instance showNewPublicVirtualInterface :: Show NewPublicVirtualInterface where
  show = genericShow
instance decodeNewPublicVirtualInterface :: Decode NewPublicVirtualInterface where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNewPublicVirtualInterface :: Encode NewPublicVirtualInterface where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing information about a public virtual interface that will be provisioned on a connection.</p>
newtype NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation 
  { "VirtualInterfaceName'" :: (VirtualInterfaceName)
  , "Vlan'" :: (VLAN)
  , "Asn'" :: (ASN)
  , "AuthKey'" :: NullOrUndefined.NullOrUndefined (BGPAuthKey)
  , "AmazonAddress'" :: NullOrUndefined.NullOrUndefined (AmazonAddress)
  , "CustomerAddress'" :: NullOrUndefined.NullOrUndefined (CustomerAddress)
  , "AddressFamily'" :: NullOrUndefined.NullOrUndefined (AddressFamily)
  , "RouteFilterPrefixes'" :: NullOrUndefined.NullOrUndefined (RouteFilterPrefixList)
  }
derive instance newtypeNewPublicVirtualInterfaceAllocation :: Newtype NewPublicVirtualInterfaceAllocation _
derive instance repGenericNewPublicVirtualInterfaceAllocation :: Generic NewPublicVirtualInterfaceAllocation _
instance showNewPublicVirtualInterfaceAllocation :: Show NewPublicVirtualInterfaceAllocation where
  show = genericShow
instance decodeNewPublicVirtualInterfaceAllocation :: Decode NewPublicVirtualInterfaceAllocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNewPublicVirtualInterfaceAllocation :: Encode NewPublicVirtualInterfaceAllocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OwnerAccount = OwnerAccount String
derive instance newtypeOwnerAccount :: Newtype OwnerAccount _
derive instance repGenericOwnerAccount :: Generic OwnerAccount _
instance showOwnerAccount :: Show OwnerAccount where
  show = genericShow
instance decodeOwnerAccount :: Decode OwnerAccount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOwnerAccount :: Encode OwnerAccount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Token to retrieve the next page of the result.</p>
newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _
derive instance repGenericPaginationToken :: Generic PaginationToken _
instance showPaginationToken :: Show PaginationToken where
  show = genericShow
instance decodePaginationToken :: Decode PaginationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaginationToken :: Encode PaginationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartnerName = PartnerName String
derive instance newtypePartnerName :: Newtype PartnerName _
derive instance repGenericPartnerName :: Generic PartnerName _
instance showPartnerName :: Show PartnerName where
  show = genericShow
instance decodePartnerName :: Decode PartnerName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartnerName :: Encode PartnerName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProviderName = ProviderName String
derive instance newtypeProviderName :: Newtype ProviderName _
derive instance repGenericProviderName :: Generic ProviderName _
instance showProviderName :: Show ProviderName where
  show = genericShow
instance decodeProviderName :: Decode ProviderName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProviderName :: Encode ProviderName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The AWS region where the connection is located.</p> <p>Example: us-east-1</p> <p>Default: None</p>
newtype Region = Region String
derive instance newtypeRegion :: Newtype Region _
derive instance repGenericRegion :: Generic Region _
instance showRegion :: Show Region where
  show = genericShow
instance decodeRegion :: Decode Region where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegion :: Encode Region where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _
derive instance repGenericResourceArn :: Generic ResourceArn _
instance showResourceArn :: Show ResourceArn where
  show = genericShow
instance decodeResourceArn :: Decode ResourceArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceArn :: Encode ResourceArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceArnList = ResourceArnList (Array ResourceArn)
derive instance newtypeResourceArnList :: Newtype ResourceArnList _
derive instance repGenericResourceArnList :: Generic ResourceArnList _
instance showResourceArnList :: Show ResourceArnList where
  show = genericShow
instance decodeResourceArnList :: Decode ResourceArnList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceArnList :: Encode ResourceArnList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The tags associated with a Direct Connect resource.</p>
newtype ResourceTag = ResourceTag 
  { "ResourceArn'" :: NullOrUndefined.NullOrUndefined (ResourceArn)
  , "Tags'" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeResourceTag :: Newtype ResourceTag _
derive instance repGenericResourceTag :: Generic ResourceTag _
instance showResourceTag :: Show ResourceTag where
  show = genericShow
instance decodeResourceTag :: Decode ResourceTag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTag :: Encode ResourceTag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceTagList = ResourceTagList (Array ResourceTag)
derive instance newtypeResourceTagList :: Newtype ResourceTagList _
derive instance repGenericResourceTagList :: Generic ResourceTagList _
instance showResourceTagList :: Show ResourceTagList where
  show = genericShow
instance decodeResourceTagList :: Decode ResourceTagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTagList :: Encode ResourceTagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A route filter prefix that the customer can advertise through Border Gateway Protocol (BGP) over a public virtual interface.</p>
newtype RouteFilterPrefix = RouteFilterPrefix 
  { "Cidr'" :: NullOrUndefined.NullOrUndefined (CIDR)
  }
derive instance newtypeRouteFilterPrefix :: Newtype RouteFilterPrefix _
derive instance repGenericRouteFilterPrefix :: Generic RouteFilterPrefix _
instance showRouteFilterPrefix :: Show RouteFilterPrefix where
  show = genericShow
instance decodeRouteFilterPrefix :: Decode RouteFilterPrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRouteFilterPrefix :: Encode RouteFilterPrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of routes to be advertised to the AWS network in this region (public virtual interface).</p>
newtype RouteFilterPrefixList = RouteFilterPrefixList (Array RouteFilterPrefix)
derive instance newtypeRouteFilterPrefixList :: Newtype RouteFilterPrefixList _
derive instance repGenericRouteFilterPrefixList :: Generic RouteFilterPrefixList _
instance showRouteFilterPrefixList :: Show RouteFilterPrefixList where
  show = genericShow
instance decodeRouteFilterPrefixList :: Decode RouteFilterPrefixList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRouteFilterPrefixList :: Encode RouteFilterPrefixList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RouterConfig = RouterConfig String
derive instance newtypeRouterConfig :: Newtype RouterConfig _
derive instance repGenericRouterConfig :: Generic RouterConfig _
instance showRouterConfig :: Show RouterConfig where
  show = genericShow
instance decodeRouterConfig :: Decode RouterConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRouterConfig :: Encode RouterConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Error message when the state of an object fails to advance.</p>
newtype StateChangeError = StateChangeError String
derive instance newtypeStateChangeError :: Newtype StateChangeError _
derive instance repGenericStateChangeError :: Generic StateChangeError _
instance showStateChangeError :: Show StateChangeError where
  show = genericShow
instance decodeStateChangeError :: Decode StateChangeError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateChangeError :: Encode StateChangeError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a tag.</p>
newtype Tag = Tag 
  { "Key'" :: (TagKey)
  , "Value'" :: NullOrUndefined.NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
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


-- | <p>Container for the parameters to the TagResource operation.</p>
newtype TagResourceRequest = TagResourceRequest 
  { "ResourceArn'" :: (ResourceArn)
  , "Tags'" :: (TagList)
  }
derive instance newtypeTagResourceRequest :: Newtype TagResourceRequest _
derive instance repGenericTagResourceRequest :: Generic TagResourceRequest _
instance showTagResourceRequest :: Show TagResourceRequest where
  show = genericShow
instance decodeTagResourceRequest :: Decode TagResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourceRequest :: Encode TagResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when TagResource is called.</p>
newtype TagResourceResponse = TagResourceResponse Types.NoArguments
derive instance newtypeTagResourceResponse :: Newtype TagResourceResponse _
derive instance repGenericTagResourceResponse :: Generic TagResourceResponse _
instance showTagResourceResponse :: Show TagResourceResponse where
  show = genericShow
instance decodeTagResourceResponse :: Decode TagResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourceResponse :: Encode TagResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have reached the limit on the number of tags that can be assigned to a Direct Connect resource.</p>
newtype TooManyTagsException = TooManyTagsException Types.NoArguments
derive instance newtypeTooManyTagsException :: Newtype TooManyTagsException _
derive instance repGenericTooManyTagsException :: Generic TooManyTagsException _
instance showTooManyTagsException :: Show TooManyTagsException where
  show = genericShow
instance decodeTooManyTagsException :: Decode TooManyTagsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyTagsException :: Encode TooManyTagsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the UntagResource operation.</p>
newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceArn'" :: (ResourceArn)
  , "TagKeys'" :: (TagKeyList)
  }
derive instance newtypeUntagResourceRequest :: Newtype UntagResourceRequest _
derive instance repGenericUntagResourceRequest :: Generic UntagResourceRequest _
instance showUntagResourceRequest :: Show UntagResourceRequest where
  show = genericShow
instance decodeUntagResourceRequest :: Decode UntagResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagResourceRequest :: Encode UntagResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when UntagResource is called.</p>
newtype UntagResourceResponse = UntagResourceResponse Types.NoArguments
derive instance newtypeUntagResourceResponse :: Newtype UntagResourceResponse _
derive instance repGenericUntagResourceResponse :: Generic UntagResourceResponse _
instance showUntagResourceResponse :: Show UntagResourceResponse where
  show = genericShow
instance decodeUntagResourceResponse :: Decode UntagResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagResourceResponse :: Encode UntagResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container for the parameters to the UpdateLag operation.</p>
newtype UpdateLagRequest = UpdateLagRequest 
  { "LagId'" :: (LagId)
  , "LagName'" :: NullOrUndefined.NullOrUndefined (LagName)
  , "MinimumLinks'" :: NullOrUndefined.NullOrUndefined (Count)
  }
derive instance newtypeUpdateLagRequest :: Newtype UpdateLagRequest _
derive instance repGenericUpdateLagRequest :: Generic UpdateLagRequest _
instance showUpdateLagRequest :: Show UpdateLagRequest where
  show = genericShow
instance decodeUpdateLagRequest :: Decode UpdateLagRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateLagRequest :: Encode UpdateLagRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The VLAN ID.</p> <p>Example: 101</p>
newtype VLAN = VLAN Int
derive instance newtypeVLAN :: Newtype VLAN _
derive instance repGenericVLAN :: Generic VLAN _
instance showVLAN :: Show VLAN where
  show = genericShow
instance decodeVLAN :: Decode VLAN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVLAN :: Encode VLAN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You can create one or more AWS Direct Connect private virtual interfaces linking to your virtual private gateway.</p> <p>Virtual private gateways can be managed using the Amazon Virtual Private Cloud (Amazon VPC) console or the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html">Amazon EC2 CreateVpnGateway action</a>.</p>
newtype VirtualGateway = VirtualGateway 
  { "VirtualGatewayId'" :: NullOrUndefined.NullOrUndefined (VirtualGatewayId)
  , "VirtualGatewayState'" :: NullOrUndefined.NullOrUndefined (VirtualGatewayState)
  }
derive instance newtypeVirtualGateway :: Newtype VirtualGateway _
derive instance repGenericVirtualGateway :: Generic VirtualGateway _
instance showVirtualGateway :: Show VirtualGateway where
  show = genericShow
instance decodeVirtualGateway :: Decode VirtualGateway where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualGateway :: Encode VirtualGateway where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ID of the virtual private gateway to a VPC. This only applies to private virtual interfaces.</p> <p>Example: vgw-123er56</p>
newtype VirtualGatewayId = VirtualGatewayId String
derive instance newtypeVirtualGatewayId :: Newtype VirtualGatewayId _
derive instance repGenericVirtualGatewayId :: Generic VirtualGatewayId _
instance showVirtualGatewayId :: Show VirtualGatewayId where
  show = genericShow
instance decodeVirtualGatewayId :: Decode VirtualGatewayId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualGatewayId :: Encode VirtualGatewayId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of virtual private gateways.</p>
newtype VirtualGatewayList = VirtualGatewayList (Array VirtualGateway)
derive instance newtypeVirtualGatewayList :: Newtype VirtualGatewayList _
derive instance repGenericVirtualGatewayList :: Generic VirtualGatewayList _
instance showVirtualGatewayList :: Show VirtualGatewayList where
  show = genericShow
instance decodeVirtualGatewayList :: Decode VirtualGatewayList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualGatewayList :: Encode VirtualGatewayList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The region in which the virtual private gateway is located.</p> <p>Example: us-east-1</p>
newtype VirtualGatewayRegion = VirtualGatewayRegion String
derive instance newtypeVirtualGatewayRegion :: Newtype VirtualGatewayRegion _
derive instance repGenericVirtualGatewayRegion :: Generic VirtualGatewayRegion _
instance showVirtualGatewayRegion :: Show VirtualGatewayRegion where
  show = genericShow
instance decodeVirtualGatewayRegion :: Decode VirtualGatewayRegion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualGatewayRegion :: Encode VirtualGatewayRegion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>State of the virtual private gateway.</p> <ul> <li> <p> <b>Pending</b>: This is the initial state after calling <i>CreateVpnGateway</i>.</p> </li> <li> <p> <b>Available</b>: Ready for use by a private virtual interface.</p> </li> <li> <p> <b>Deleting</b>: This is the initial state after calling <i>DeleteVpnGateway</i>.</p> </li> <li> <p> <b>Deleted</b>: In this state, a private virtual interface is unable to send traffic over this gateway.</p> </li> </ul>
newtype VirtualGatewayState = VirtualGatewayState String
derive instance newtypeVirtualGatewayState :: Newtype VirtualGatewayState _
derive instance repGenericVirtualGatewayState :: Generic VirtualGatewayState _
instance showVirtualGatewayState :: Show VirtualGatewayState where
  show = genericShow
instance decodeVirtualGatewayState :: Decode VirtualGatewayState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualGatewayState :: Encode VirtualGatewayState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing a list of virtual private gateways.</p>
newtype VirtualGateways = VirtualGateways 
  { "VirtualGateways'" :: NullOrUndefined.NullOrUndefined (VirtualGatewayList)
  }
derive instance newtypeVirtualGateways :: Newtype VirtualGateways _
derive instance repGenericVirtualGateways :: Generic VirtualGateways _
instance showVirtualGateways :: Show VirtualGateways where
  show = genericShow
instance decodeVirtualGateways :: Decode VirtualGateways where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualGateways :: Encode VirtualGateways where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A virtual interface (VLAN) transmits the traffic between the AWS Direct Connect location and the customer.</p>
newtype VirtualInterface = VirtualInterface 
  { "OwnerAccount'" :: NullOrUndefined.NullOrUndefined (OwnerAccount)
  , "VirtualInterfaceId'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceId)
  , "Location'" :: NullOrUndefined.NullOrUndefined (LocationCode)
  , "ConnectionId'" :: NullOrUndefined.NullOrUndefined (ConnectionId)
  , "VirtualInterfaceType'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceType)
  , "VirtualInterfaceName'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceName)
  , "Vlan'" :: NullOrUndefined.NullOrUndefined (VLAN)
  , "Asn'" :: NullOrUndefined.NullOrUndefined (ASN)
  , "AmazonSideAsn'" :: NullOrUndefined.NullOrUndefined (LongAsn)
  , "AuthKey'" :: NullOrUndefined.NullOrUndefined (BGPAuthKey)
  , "AmazonAddress'" :: NullOrUndefined.NullOrUndefined (AmazonAddress)
  , "CustomerAddress'" :: NullOrUndefined.NullOrUndefined (CustomerAddress)
  , "AddressFamily'" :: NullOrUndefined.NullOrUndefined (AddressFamily)
  , "VirtualInterfaceState'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceState)
  , "CustomerRouterConfig'" :: NullOrUndefined.NullOrUndefined (RouterConfig)
  , "VirtualGatewayId'" :: NullOrUndefined.NullOrUndefined (VirtualGatewayId)
  , "DirectConnectGatewayId'" :: NullOrUndefined.NullOrUndefined (DirectConnectGatewayId)
  , "RouteFilterPrefixes'" :: NullOrUndefined.NullOrUndefined (RouteFilterPrefixList)
  , "BgpPeers'" :: NullOrUndefined.NullOrUndefined (BGPPeerList)
  }
derive instance newtypeVirtualInterface :: Newtype VirtualInterface _
derive instance repGenericVirtualInterface :: Generic VirtualInterface _
instance showVirtualInterface :: Show VirtualInterface where
  show = genericShow
instance decodeVirtualInterface :: Decode VirtualInterface where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualInterface :: Encode VirtualInterface where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ID of the virtual interface.</p> <p>Example: dxvif-123dfg56</p> <p>Default: None</p>
newtype VirtualInterfaceId = VirtualInterfaceId String
derive instance newtypeVirtualInterfaceId :: Newtype VirtualInterfaceId _
derive instance repGenericVirtualInterfaceId :: Generic VirtualInterfaceId _
instance showVirtualInterfaceId :: Show VirtualInterfaceId where
  show = genericShow
instance decodeVirtualInterfaceId :: Decode VirtualInterfaceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualInterfaceId :: Encode VirtualInterfaceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of virtual interfaces.</p>
newtype VirtualInterfaceList = VirtualInterfaceList (Array VirtualInterface)
derive instance newtypeVirtualInterfaceList :: Newtype VirtualInterfaceList _
derive instance repGenericVirtualInterfaceList :: Generic VirtualInterfaceList _
instance showVirtualInterfaceList :: Show VirtualInterfaceList where
  show = genericShow
instance decodeVirtualInterfaceList :: Decode VirtualInterfaceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualInterfaceList :: Encode VirtualInterfaceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of the virtual interface assigned by the customer.</p> <p>Example: "My VPC"</p>
newtype VirtualInterfaceName = VirtualInterfaceName String
derive instance newtypeVirtualInterfaceName :: Newtype VirtualInterfaceName _
derive instance repGenericVirtualInterfaceName :: Generic VirtualInterfaceName _
instance showVirtualInterfaceName :: Show VirtualInterfaceName where
  show = genericShow
instance decodeVirtualInterfaceName :: Decode VirtualInterfaceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualInterfaceName :: Encode VirtualInterfaceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The region in which the virtual interface is located.</p> <p>Example: us-east-1</p>
newtype VirtualInterfaceRegion = VirtualInterfaceRegion String
derive instance newtypeVirtualInterfaceRegion :: Newtype VirtualInterfaceRegion _
derive instance repGenericVirtualInterfaceRegion :: Generic VirtualInterfaceRegion _
instance showVirtualInterfaceRegion :: Show VirtualInterfaceRegion where
  show = genericShow
instance decodeVirtualInterfaceRegion :: Decode VirtualInterfaceRegion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualInterfaceRegion :: Encode VirtualInterfaceRegion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>State of the virtual interface.</p> <ul> <li> <p> <b>Confirming</b>: The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.</p> </li> <li> <p> <b>Verifying</b>: This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.</p> </li> <li> <p> <b>Pending</b>: A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.</p> </li> <li> <p> <b>Available</b>: A virtual interface that is able to forward traffic.</p> </li> <li> <p> <b>Down</b>: A virtual interface that is BGP down.</p> </li> <li> <p> <b>Deleting</b>: A virtual interface is in this state immediately after calling <a>DeleteVirtualInterface</a> until it can no longer forward traffic.</p> </li> <li> <p> <b>Deleted</b>: A virtual interface that cannot forward traffic.</p> </li> <li> <p> <b>Rejected</b>: The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the 'Confirming' state is deleted by the virtual interface owner, the virtual interface will enter the 'Rejected' state.</p> </li> </ul>
newtype VirtualInterfaceState = VirtualInterfaceState String
derive instance newtypeVirtualInterfaceState :: Newtype VirtualInterfaceState _
derive instance repGenericVirtualInterfaceState :: Generic VirtualInterfaceState _
instance showVirtualInterfaceState :: Show VirtualInterfaceState where
  show = genericShow
instance decodeVirtualInterfaceState :: Decode VirtualInterfaceState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualInterfaceState :: Encode VirtualInterfaceState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The type of virtual interface.</p> <p>Example: private (Amazon VPC) or public (Amazon S3, Amazon DynamoDB, and so on.)</p>
newtype VirtualInterfaceType = VirtualInterfaceType String
derive instance newtypeVirtualInterfaceType :: Newtype VirtualInterfaceType _
derive instance repGenericVirtualInterfaceType :: Generic VirtualInterfaceType _
instance showVirtualInterfaceType :: Show VirtualInterfaceType where
  show = genericShow
instance decodeVirtualInterfaceType :: Decode VirtualInterfaceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualInterfaceType :: Encode VirtualInterfaceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing a list of virtual interfaces.</p>
newtype VirtualInterfaces = VirtualInterfaces 
  { "VirtualInterfaces'" :: NullOrUndefined.NullOrUndefined (VirtualInterfaceList)
  }
derive instance newtypeVirtualInterfaces :: Newtype VirtualInterfaces _
derive instance repGenericVirtualInterfaces :: Generic VirtualInterfaces _
instance showVirtualInterfaces :: Show VirtualInterfaces where
  show = genericShow
instance decodeVirtualInterfaces :: Decode VirtualInterfaces where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualInterfaces :: Encode VirtualInterfaces where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
