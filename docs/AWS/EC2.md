## Module AWS.EC2

<fullname>Amazon Elastic Compute Cloud</fullname> <p>Amazon Elastic Compute Cloud (Amazon EC2) provides resizable computing capacity in the AWS Cloud. Using Amazon EC2 eliminates the need to invest in hardware up front, so you can develop and deploy applications faster.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `acceptReservedInstancesExchangeQuote`

``` purescript
acceptReservedInstancesExchangeQuote :: forall eff. AcceptReservedInstancesExchangeQuoteRequest -> Aff (err :: RequestError | eff) AcceptReservedInstancesExchangeQuoteResult
```

<p>Accepts the Convertible Reserved Instance exchange quote described in the <a>GetReservedInstancesExchangeQuote</a> call.</p>

#### `acceptVpcEndpointConnections`

``` purescript
acceptVpcEndpointConnections :: forall eff. AcceptVpcEndpointConnectionsRequest -> Aff (err :: RequestError | eff) AcceptVpcEndpointConnectionsResult
```

<p>Accepts one or more interface VPC endpoint connection requests to your VPC endpoint service.</p>

#### `acceptVpcPeeringConnection`

``` purescript
acceptVpcPeeringConnection :: forall eff. AcceptVpcPeeringConnectionRequest -> Aff (err :: RequestError | eff) AcceptVpcPeeringConnectionResult
```

<p>Accept a VPC peering connection request. To accept a request, the VPC peering connection must be in the <code>pending-acceptance</code> state, and you must be the owner of the peer VPC. Use <a>DescribeVpcPeeringConnections</a> to view your outstanding VPC peering connection requests.</p> <p>For an inter-region VPC peering connection request, you must accept the VPC peering connection in the region of the accepter VPC.</p>

#### `allocateAddress`

``` purescript
allocateAddress :: forall eff. AllocateAddressRequest -> Aff (err :: RequestError | eff) AllocateAddressResult
```

<p>Allocates an Elastic IP address.</p> <p>An Elastic IP address is for use either in the EC2-Classic platform or in a VPC. By default, you can allocate 5 Elastic IP addresses for EC2-Classic per region and 5 Elastic IP addresses for EC2-VPC per region.</p> <p>If you release an Elastic IP address for use in a VPC, you might be able to recover it. To recover an Elastic IP address that you released, specify it in the <code>Address</code> parameter. Note that you cannot recover an Elastic IP address that you released after it is allocated to another AWS account.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html">Elastic IP Addresses</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `allocateHosts`

``` purescript
allocateHosts :: forall eff. AllocateHostsRequest -> Aff (err :: RequestError | eff) AllocateHostsResult
```

<p>Allocates a Dedicated Host to your account. At minimum you need to specify the instance size type, Availability Zone, and quantity of hosts you want to allocate.</p>

#### `assignIpv6Addresses`

``` purescript
assignIpv6Addresses :: forall eff. AssignIpv6AddressesRequest -> Aff (err :: RequestError | eff) AssignIpv6AddressesResult
```

<p>Assigns one or more IPv6 addresses to the specified network interface. You can specify one or more specific IPv6 addresses, or you can specify the number of IPv6 addresses to be automatically assigned from within the subnet's IPv6 CIDR block range. You can assign as many IPv6 addresses to a network interface as you can assign private IPv4 addresses, and the limit varies per instance type. For information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI">IP Addresses Per Network Interface Per Instance Type</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `assignPrivateIpAddresses`

``` purescript
assignPrivateIpAddresses :: forall eff. AssignPrivateIpAddressesRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Assigns one or more secondary private IP addresses to the specified network interface. You can specify one or more specific secondary IP addresses, or you can specify the number of secondary IP addresses to be automatically assigned within the subnet's CIDR block range. The number of secondary IP addresses that you can assign to an instance varies by instance type. For information about instance types, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html">Instance Types</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>. For more information about Elastic IP addresses, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html">Elastic IP Addresses</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>AssignPrivateIpAddresses is available only in EC2-VPC.</p>

#### `associateAddress`

``` purescript
associateAddress :: forall eff. AssociateAddressRequest -> Aff (err :: RequestError | eff) AssociateAddressResult
```

<p>Associates an Elastic IP address with an instance or a network interface.</p> <p>An Elastic IP address is for use in either the EC2-Classic platform or in a VPC. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html">Elastic IP Addresses</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>[EC2-Classic, VPC in an EC2-VPC-only account] If the Elastic IP address is already associated with a different instance, it is disassociated from that instance and associated with the specified instance. If you associate an Elastic IP address with an instance that has an existing Elastic IP address, the existing address is disassociated from the instance, but remains allocated to your account.</p> <p>[VPC in an EC2-Classic account] If you don't specify a private IP address, the Elastic IP address is associated with the primary IP address. If the Elastic IP address is already associated with a different instance or a network interface, you get an error unless you allow reassociation. You cannot associate an Elastic IP address with an instance or network interface that has an existing Elastic IP address.</p> <important> <p>This is an idempotent operation. If you perform the operation more than once, Amazon EC2 doesn't return an error, and you may be charged for each time the Elastic IP address is remapped to the same instance. For more information, see the <i>Elastic IP Addresses</i> section of <a href="http://aws.amazon.com/ec2/pricing/">Amazon EC2 Pricing</a>.</p> </important>

#### `associateDhcpOptions`

``` purescript
associateDhcpOptions :: forall eff. AssociateDhcpOptionsRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Associates a set of DHCP options (that you've previously created) with the specified VPC, or associates no DHCP options with the VPC.</p> <p>After you associate the options with the VPC, any existing instances and all new instances that you launch in that VPC use the options. You don't need to restart or relaunch the instances. They automatically pick up the changes within a few hours, depending on how frequently the instance renews its DHCP lease. You can explicitly renew the lease using the operating system on the instance.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html">DHCP Options Sets</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `associateIamInstanceProfile`

``` purescript
associateIamInstanceProfile :: forall eff. AssociateIamInstanceProfileRequest -> Aff (err :: RequestError | eff) AssociateIamInstanceProfileResult
```

<p>Associates an IAM instance profile with a running or stopped instance. You cannot associate more than one IAM instance profile with an instance.</p>

#### `associateRouteTable`

``` purescript
associateRouteTable :: forall eff. AssociateRouteTableRequest -> Aff (err :: RequestError | eff) AssociateRouteTableResult
```

<p>Associates a subnet with a route table. The subnet and route table must be in the same VPC. This association causes traffic originating from the subnet to be routed according to the routes in the route table. The action returns an association ID, which you need in order to disassociate the route table from the subnet later. A route table can be associated with multiple subnets.</p> <p>For more information about route tables, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html">Route Tables</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `associateSubnetCidrBlock`

``` purescript
associateSubnetCidrBlock :: forall eff. AssociateSubnetCidrBlockRequest -> Aff (err :: RequestError | eff) AssociateSubnetCidrBlockResult
```

<p>Associates a CIDR block with your subnet. You can only associate a single IPv6 CIDR block with your subnet. An IPv6 CIDR block must have a prefix length of /64.</p>

#### `associateVpcCidrBlock`

``` purescript
associateVpcCidrBlock :: forall eff. AssociateVpcCidrBlockRequest -> Aff (err :: RequestError | eff) AssociateVpcCidrBlockResult
```

<p>Associates a CIDR block with your VPC. You can associate a secondary IPv4 CIDR block, or you can associate an Amazon-provided IPv6 CIDR block. The IPv6 CIDR block size is fixed at /56.</p> <p>For more information about associating CIDR blocks with your VPC and applicable restrictions, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html#VPC_Sizing">VPC and Subnet Sizing</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `attachClassicLinkVpc`

``` purescript
attachClassicLinkVpc :: forall eff. AttachClassicLinkVpcRequest -> Aff (err :: RequestError | eff) AttachClassicLinkVpcResult
```

<p>Links an EC2-Classic instance to a ClassicLink-enabled VPC through one or more of the VPC's security groups. You cannot link an EC2-Classic instance to more than one VPC at a time. You can only link an instance that's in the <code>running</code> state. An instance is automatically unlinked from a VPC when it's stopped - you can link it to the VPC again when you restart it.</p> <p>After you've linked an instance, you cannot change the VPC security groups that are associated with it. To change the security groups, you must first unlink the instance, and then link it again.</p> <p>Linking your instance to a VPC is sometimes referred to as <i>attaching</i> your instance.</p>

#### `attachInternetGateway`

``` purescript
attachInternetGateway :: forall eff. AttachInternetGatewayRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Attaches an Internet gateway to a VPC, enabling connectivity between the Internet and the VPC. For more information about your VPC and Internet gateway, see the <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/">Amazon Virtual Private Cloud User Guide</a>.</p>

#### `attachNetworkInterface`

``` purescript
attachNetworkInterface :: forall eff. AttachNetworkInterfaceRequest -> Aff (err :: RequestError | eff) AttachNetworkInterfaceResult
```

<p>Attaches a network interface to an instance.</p>

#### `attachVolume`

``` purescript
attachVolume :: forall eff. AttachVolumeRequest -> Aff (err :: RequestError | eff) VolumeAttachment
```

<p>Attaches an EBS volume to a running or stopped instance and exposes it to the instance with the specified device name.</p> <p>Encrypted EBS volumes may only be attached to instances that support Amazon EBS encryption. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html">Amazon EBS Encryption</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>For a list of supported device names, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-attaching-volume.html">Attaching an EBS Volume to an Instance</a>. Any device names that aren't reserved for instance store volumes can be used for EBS volumes. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html">Amazon EC2 Instance Store</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>If a volume has an AWS Marketplace product code:</p> <ul> <li> <p>The volume can be attached only to a stopped instance.</p> </li> <li> <p>AWS Marketplace product codes are copied from the volume to the instance.</p> </li> <li> <p>You must be subscribed to the product.</p> </li> <li> <p>The instance type and operating system of the instance must support the product. For example, you can't detach a volume from a Windows instance and attach it to a Linux instance.</p> </li> </ul> <p>For an overview of the AWS Marketplace, see <a href="https://aws.amazon.com/marketplace/help/200900000">Introducing AWS Marketplace</a>.</p> <p>For more information about EBS volumes, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-attaching-volume.html">Attaching Amazon EBS Volumes</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `attachVpnGateway`

``` purescript
attachVpnGateway :: forall eff. AttachVpnGatewayRequest -> Aff (err :: RequestError | eff) AttachVpnGatewayResult
```

<p>Attaches a virtual private gateway to a VPC. You can attach one virtual private gateway to one VPC at a time.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html">AWS Managed VPN Connections</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `authorizeSecurityGroupEgress`

``` purescript
authorizeSecurityGroupEgress :: forall eff. AuthorizeSecurityGroupEgressRequest -> Aff (err :: RequestError | eff) Unit
```

<p>[EC2-VPC only] Adds one or more egress rules to a security group for use with a VPC. Specifically, this action permits instances to send traffic to one or more destination IPv4 or IPv6 CIDR address ranges, or to one or more destination security groups for the same VPC. This action doesn't apply to security groups for use in EC2-Classic. For more information, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html">Security Groups for Your VPC</a> in the <i>Amazon Virtual Private Cloud User Guide</i>. For more information about security group limits, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Appendix_Limits.html">Amazon VPC Limits</a>.</p> <p>Each rule consists of the protocol (for example, TCP), plus either a CIDR range or a source group. For the TCP and UDP protocols, you must also specify the destination port or port range. For the ICMP protocol, you must also specify the ICMP type and code. You can use -1 for the type or code to mean all types or all codes. You can optionally specify a description for the rule.</p> <p>Rule changes are propagated to affected instances as quickly as possible. However, a small delay might occur.</p>

#### `authorizeSecurityGroupIngress`

``` purescript
authorizeSecurityGroupIngress :: forall eff. AuthorizeSecurityGroupIngressRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds one or more ingress rules to a security group.</p> <p>Rule changes are propagated to instances within the security group as quickly as possible. However, a small delay might occur.</p> <p>[EC2-Classic] This action gives one or more IPv4 CIDR address ranges permission to access a security group in your account, or gives one or more security groups (called the <i>source groups</i>) permission to access a security group for your account. A source group can be for your own AWS account, or another. You can have up to 100 rules per group.</p> <p>[EC2-VPC] This action gives one or more IPv4 or IPv6 CIDR address ranges permission to access a security group in your VPC, or gives one or more other security groups (called the <i>source groups</i>) permission to access a security group for your VPC. The security groups must all be for the same VPC or a peer VPC in a VPC peering connection. For more information about VPC security group limits, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Appendix_Limits.html">Amazon VPC Limits</a>.</p> <p>You can optionally specify a description for the security group rule.</p>

#### `bundleInstance`

``` purescript
bundleInstance :: forall eff. BundleInstanceRequest -> Aff (err :: RequestError | eff) BundleInstanceResult
```

<p>Bundles an Amazon instance store-backed Windows instance.</p> <p>During bundling, only the root device volume (C:\) is bundled. Data on other instance store volumes is not preserved.</p> <note> <p>This action is not applicable for Linux/Unix instances or Windows instances that are backed by Amazon EBS.</p> </note> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/Creating_InstanceStoreBacked_WinAMI.html">Creating an Instance Store-Backed Windows AMI</a>.</p>

#### `cancelBundleTask`

``` purescript
cancelBundleTask :: forall eff. CancelBundleTaskRequest -> Aff (err :: RequestError | eff) CancelBundleTaskResult
```

<p>Cancels a bundling operation for an instance store-backed Windows instance.</p>

#### `cancelConversionTask`

``` purescript
cancelConversionTask :: forall eff. CancelConversionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Cancels an active conversion task. The task can be the import of an instance or volume. The action removes all artifacts of the conversion, including a partially uploaded volume or instance. If the conversion is complete or is in the process of transferring the final disk image, the command fails and returns an exception.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/ec2-cli-vmimport-export.html">Importing a Virtual Machine Using the Amazon EC2 CLI</a>.</p>

#### `cancelExportTask`

``` purescript
cancelExportTask :: forall eff. CancelExportTaskRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Cancels an active export task. The request removes all artifacts of the export, including any partially-created Amazon S3 objects. If the export task is complete or is in the process of transferring the final disk image, the command fails and returns an error.</p>

#### `cancelImportTask`

``` purescript
cancelImportTask :: forall eff. CancelImportTaskRequest -> Aff (err :: RequestError | eff) CancelImportTaskResult
```

<p>Cancels an in-process import virtual machine or import snapshot task.</p>

#### `cancelReservedInstancesListing`

``` purescript
cancelReservedInstancesListing :: forall eff. CancelReservedInstancesListingRequest -> Aff (err :: RequestError | eff) CancelReservedInstancesListingResult
```

<p>Cancels the specified Reserved Instance listing in the Reserved Instance Marketplace.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html">Reserved Instance Marketplace</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `cancelSpotFleetRequests`

``` purescript
cancelSpotFleetRequests :: forall eff. CancelSpotFleetRequestsRequest -> Aff (err :: RequestError | eff) CancelSpotFleetRequestsResponse
```

<p>Cancels the specified Spot Fleet requests.</p> <p>After you cancel a Spot Fleet request, the Spot Fleet launches no new Spot Instances. You must specify whether the Spot Fleet should also terminate its Spot Instances. If you terminate the instances, the Spot Fleet request enters the <code>cancelled_terminating</code> state. Otherwise, the Spot Fleet request enters the <code>cancelled_running</code> state and the instances continue to run until they are interrupted or you terminate them manually.</p>

#### `cancelSpotInstanceRequests`

``` purescript
cancelSpotInstanceRequests :: forall eff. CancelSpotInstanceRequestsRequest -> Aff (err :: RequestError | eff) CancelSpotInstanceRequestsResult
```

<p>Cancels one or more Spot Instance requests. Spot Instances are instances that Amazon EC2 starts on your behalf when the maximum price that you specify exceeds the current Spot price. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html">Spot Instance Requests</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <important> <p>Canceling a Spot Instance request does not terminate running Spot Instances associated with the request.</p> </important>

#### `confirmProductInstance`

``` purescript
confirmProductInstance :: forall eff. ConfirmProductInstanceRequest -> Aff (err :: RequestError | eff) ConfirmProductInstanceResult
```

<p>Determines whether a product code is associated with an instance. This action can only be used by the owner of the product code. It is useful when a product code owner must verify whether another user's instance is eligible for support.</p>

#### `copyFpgaImage`

``` purescript
copyFpgaImage :: forall eff. CopyFpgaImageRequest -> Aff (err :: RequestError | eff) CopyFpgaImageResult
```

<p>Copies the specified Amazon FPGA Image (AFI) to the current region.</p>

#### `copyImage`

``` purescript
copyImage :: forall eff. CopyImageRequest -> Aff (err :: RequestError | eff) CopyImageResult
```

<p>Initiates the copy of an AMI from the specified source region to the current region. You specify the destination region by using its endpoint when making the request.</p> <p>For more information about the prerequisites and limits when copying an AMI, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/CopyingAMIs.html">Copying an AMI</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `copySnapshot`

``` purescript
copySnapshot :: forall eff. CopySnapshotRequest -> Aff (err :: RequestError | eff) CopySnapshotResult
```

<p>Copies a point-in-time snapshot of an EBS volume and stores it in Amazon S3. You can copy the snapshot within the same region or from one region to another. You can use the snapshot to create EBS volumes or Amazon Machine Images (AMIs). The snapshot is copied to the regional endpoint that you send the HTTP request to.</p> <p>Copies of encrypted EBS snapshots remain encrypted. Copies of unencrypted snapshots remain unencrypted, unless the <code>Encrypted</code> flag is specified during the snapshot copy operation. By default, encrypted snapshot copies use the default AWS Key Management Service (AWS KMS) customer master key (CMK); however, you can specify a non-default CMK with the <code>KmsKeyId</code> parameter. </p> <note> <p>To copy an encrypted snapshot that has been shared from another account, you must have permissions for the CMK used to encrypt the snapshot.</p> </note> <note> <p>Snapshots created by the CopySnapshot action have an arbitrary volume ID that should not be used for any purpose.</p> </note> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-copy-snapshot.html">Copying an Amazon EBS Snapshot</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `createCustomerGateway`

``` purescript
createCustomerGateway :: forall eff. CreateCustomerGatewayRequest -> Aff (err :: RequestError | eff) CreateCustomerGatewayResult
```

<p>Provides information to AWS about your VPN customer gateway device. The customer gateway is the appliance at your end of the VPN connection. (The device on the AWS side of the VPN connection is the virtual private gateway.) You must provide the Internet-routable IP address of the customer gateway's external interface. The IP address must be static and may be behind a device performing network address translation (NAT).</p> <p>For devices that use Border Gateway Protocol (BGP), you can also provide the device's BGP Autonomous System Number (ASN). You can use an existing ASN assigned to your network. If you don't have an ASN already, you can use a private ASN (in the 64512 - 65534 range).</p> <note> <p>Amazon EC2 supports all 2-byte ASN numbers in the range of 1 - 65534, with the exception of 7224, which is reserved in the <code>us-east-1</code> region, and 9059, which is reserved in the <code>eu-west-1</code> region.</p> </note> <p>For more information about VPN customer gateways, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html">AWS Managed VPN Connections</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p> <important> <p>You cannot create more than one customer gateway with the same VPN type, IP address, and BGP ASN parameter values. If you run an identical request more than one time, the first request creates the customer gateway, and subsequent requests return information about the existing customer gateway. The subsequent requests do not create new customer gateway resources.</p> </important>

#### `createDefaultSubnet`

``` purescript
createDefaultSubnet :: forall eff. CreateDefaultSubnetRequest -> Aff (err :: RequestError | eff) CreateDefaultSubnetResult
```

<p>Creates a default subnet with a size <code>/20</code> IPv4 CIDR block in the specified Availability Zone in your default VPC. You can have only one default subnet per Availability Zone. For more information, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/default-vpc.html#create-default-subnet">Creating a Default Subnet</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `createDefaultVpc`

``` purescript
createDefaultVpc :: forall eff. CreateDefaultVpcRequest -> Aff (err :: RequestError | eff) CreateDefaultVpcResult
```

<p>Creates a default VPC with a size <code>/16</code> IPv4 CIDR block and a default subnet in each Availability Zone. For more information about the components of a default VPC, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/default-vpc.html">Default VPC and Default Subnets</a> in the <i>Amazon Virtual Private Cloud User Guide</i>. You cannot specify the components of the default VPC yourself.</p> <p>You can create a default VPC if you deleted your previous default VPC. You cannot have more than one default VPC per region. </p> <p>If your account supports EC2-Classic, you cannot use this action to create a default VPC in a region that supports EC2-Classic. If you want a default VPC in a region that supports EC2-Classic, see "I really want a default VPC for my existing EC2 account. Is that possible?" in the <a href="http://aws.amazon.com/vpc/faqs/#Default_VPCs">Default VPCs FAQ</a>.</p>

#### `createDhcpOptions`

``` purescript
createDhcpOptions :: forall eff. CreateDhcpOptionsRequest -> Aff (err :: RequestError | eff) CreateDhcpOptionsResult
```

<p>Creates a set of DHCP options for your VPC. After creating the set, you must associate it with the VPC, causing all existing and new instances that you launch in the VPC to use this set of DHCP options. The following are the individual DHCP options you can specify. For more information about the options, see <a href="http://www.ietf.org/rfc/rfc2132.txt">RFC 2132</a>.</p> <ul> <li> <p> <code>domain-name-servers</code> - The IP addresses of up to four domain name servers, or AmazonProvidedDNS. The default DHCP option set specifies AmazonProvidedDNS. If specifying more than one domain name server, specify the IP addresses in a single parameter, separated by commas. If you want your instance to receive a custom DNS hostname as specified in <code>domain-name</code>, you must set <code>domain-name-servers</code> to a custom DNS server.</p> </li> <li> <p> <code>domain-name</code> - If you're using AmazonProvidedDNS in <code>us-east-1</code>, specify <code>ec2.internal</code>. If you're using AmazonProvidedDNS in another region, specify <code>region.compute.internal</code> (for example, <code>ap-northeast-1.compute.internal</code>). Otherwise, specify a domain name (for example, <code>MyCompany.com</code>). This value is used to complete unqualified DNS hostnames. <b>Important</b>: Some Linux operating systems accept multiple domain names separated by spaces. However, Windows and other Linux operating systems treat the value as a single domain, which results in unexpected behavior. If your DHCP options set is associated with a VPC that has instances with multiple operating systems, specify only one domain name.</p> </li> <li> <p> <code>ntp-servers</code> - The IP addresses of up to four Network Time Protocol (NTP) servers.</p> </li> <li> <p> <code>netbios-name-servers</code> - The IP addresses of up to four NetBIOS name servers.</p> </li> <li> <p> <code>netbios-node-type</code> - The NetBIOS node type (1, 2, 4, or 8). We recommend that you specify 2 (broadcast and multicast are not currently supported). For more information about these node types, see <a href="http://www.ietf.org/rfc/rfc2132.txt">RFC 2132</a>.</p> </li> </ul> <p>Your VPC automatically starts out with a set of DHCP options that includes only a DNS server that we provide (AmazonProvidedDNS). If you create a set of options, and if your VPC has an Internet gateway, make sure to set the <code>domain-name-servers</code> option either to <code>AmazonProvidedDNS</code> or to a domain name server of your choice. For more information about DHCP options, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html">DHCP Options Sets</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `createEgressOnlyInternetGateway`

``` purescript
createEgressOnlyInternetGateway :: forall eff. CreateEgressOnlyInternetGatewayRequest -> Aff (err :: RequestError | eff) CreateEgressOnlyInternetGatewayResult
```

<p>[IPv6 only] Creates an egress-only Internet gateway for your VPC. An egress-only Internet gateway is used to enable outbound communication over IPv6 from instances in your VPC to the Internet, and prevents hosts outside of your VPC from initiating an IPv6 connection with your instance.</p>

#### `createFlowLogs`

``` purescript
createFlowLogs :: forall eff. CreateFlowLogsRequest -> Aff (err :: RequestError | eff) CreateFlowLogsResult
```

<p>Creates one or more flow logs to capture IP traffic for a specific network interface, subnet, or VPC. Flow logs are delivered to a specified log group in Amazon CloudWatch Logs. If you specify a VPC or subnet in the request, a log stream is created in CloudWatch Logs for each network interface in the subnet or VPC. Log streams can include information about accepted and rejected traffic to a network interface. You can view the data in your log streams using Amazon CloudWatch Logs.</p> <p>In your request, you must also specify an IAM role that has permission to publish logs to CloudWatch Logs.</p>

#### `createFpgaImage`

``` purescript
createFpgaImage :: forall eff. CreateFpgaImageRequest -> Aff (err :: RequestError | eff) CreateFpgaImageResult
```

<p>Creates an Amazon FPGA Image (AFI) from the specified design checkpoint (DCP).</p> <p>The create operation is asynchronous. To verify that the AFI is ready for use, check the output logs.</p> <p>An AFI contains the FPGA bitstream that is ready to download to an FPGA. You can securely deploy an AFI on one or more FPGA-accelerated instances. For more information, see the <a href="https://github.com/aws/aws-fpga/">AWS FPGA Hardware Development Kit</a>.</p>

#### `createImage`

``` purescript
createImage :: forall eff. CreateImageRequest -> Aff (err :: RequestError | eff) CreateImageResult
```

<p>Creates an Amazon EBS-backed AMI from an Amazon EBS-backed instance that is either running or stopped.</p> <p>If you customized your instance with instance store volumes or EBS volumes in addition to the root device volume, the new AMI contains block device mapping information for those volumes. When you launch an instance from this new AMI, the instance automatically launches with those additional volumes.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html">Creating Amazon EBS-Backed Linux AMIs</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `createInstanceExportTask`

``` purescript
createInstanceExportTask :: forall eff. CreateInstanceExportTaskRequest -> Aff (err :: RequestError | eff) CreateInstanceExportTaskResult
```

<p>Exports a running or stopped instance to an S3 bucket.</p> <p>For information about the supported operating systems, image formats, and known limitations for the types of instances you can export, see <a href="http://docs.aws.amazon.com/vm-import/latest/userguide/vmexport.html">Exporting an Instance as a VM Using VM Import/Export</a> in the <i>VM Import/Export User Guide</i>.</p>

#### `createInternetGateway`

``` purescript
createInternetGateway :: forall eff. CreateInternetGatewayRequest -> Aff (err :: RequestError | eff) CreateInternetGatewayResult
```

<p>Creates an Internet gateway for use with a VPC. After creating the Internet gateway, you attach it to a VPC using <a>AttachInternetGateway</a>.</p> <p>For more information about your VPC and Internet gateway, see the <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/">Amazon Virtual Private Cloud User Guide</a>.</p>

#### `createKeyPair`

``` purescript
createKeyPair :: forall eff. CreateKeyPairRequest -> Aff (err :: RequestError | eff) KeyPair
```

<p>Creates a 2048-bit RSA key pair with the specified name. Amazon EC2 stores the public key and displays the private key for you to save to a file. The private key is returned as an unencrypted PEM encoded PKCS#1 private key. If a key with the specified name already exists, Amazon EC2 returns an error.</p> <p>You can have up to five thousand key pairs per region.</p> <p>The key pair returned to you is available only in the region in which you create it. If you prefer, you can create your own key pair using a third-party tool and upload it to any region using <a>ImportKeyPair</a>.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html">Key Pairs</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `createLaunchTemplate`

``` purescript
createLaunchTemplate :: forall eff. CreateLaunchTemplateRequest -> Aff (err :: RequestError | eff) CreateLaunchTemplateResult
```

<p>Creates a launch template. A launch template contains the parameters to launch an instance. When you launch an instance using <a>RunInstances</a>, you can specify a launch template instead of providing the launch parameters in the request.</p>

#### `createLaunchTemplateVersion`

``` purescript
createLaunchTemplateVersion :: forall eff. CreateLaunchTemplateVersionRequest -> Aff (err :: RequestError | eff) CreateLaunchTemplateVersionResult
```

<p>Creates a new version for a launch template. You can specify an existing version of launch template from which to base the new version.</p> <p>Launch template versions are numbered in the order in which they are created. You cannot specify, change, or replace the numbering of launch template versions.</p>

#### `createNatGateway`

``` purescript
createNatGateway :: forall eff. CreateNatGatewayRequest -> Aff (err :: RequestError | eff) CreateNatGatewayResult
```

<p>Creates a NAT gateway in the specified subnet. A NAT gateway can be used to enable instances in a private subnet to connect to the Internet. This action creates a network interface in the specified subnet with a private IP address from the IP address range of the subnet. For more information, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html">NAT Gateways</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `createNetworkAcl`

``` purescript
createNetworkAcl :: forall eff. CreateNetworkAclRequest -> Aff (err :: RequestError | eff) CreateNetworkAclResult
```

<p>Creates a network ACL in a VPC. Network ACLs provide an optional layer of security (in addition to security groups) for the instances in your VPC.</p> <p>For more information about network ACLs, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html">Network ACLs</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `createNetworkAclEntry`

``` purescript
createNetworkAclEntry :: forall eff. CreateNetworkAclEntryRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates an entry (a rule) in a network ACL with the specified rule number. Each network ACL has a set of numbered ingress rules and a separate set of numbered egress rules. When determining whether a packet should be allowed in or out of a subnet associated with the ACL, we process the entries in the ACL according to the rule numbers, in ascending order. Each network ACL has a set of ingress rules and a separate set of egress rules.</p> <p>We recommend that you leave room between the rule numbers (for example, 100, 110, 120, ...), and not number them one right after the other (for example, 101, 102, 103, ...). This makes it easier to add a rule between existing ones without having to renumber the rules.</p> <p>After you add an entry, you can't modify it; you must either replace it, or create an entry and delete the old one.</p> <p>For more information about network ACLs, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html">Network ACLs</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `createNetworkInterface`

``` purescript
createNetworkInterface :: forall eff. CreateNetworkInterfaceRequest -> Aff (err :: RequestError | eff) CreateNetworkInterfaceResult
```

<p>Creates a network interface in the specified subnet.</p> <p>For more information about network interfaces, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html">Elastic Network Interfaces</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `createNetworkInterfacePermission`

``` purescript
createNetworkInterfacePermission :: forall eff. CreateNetworkInterfacePermissionRequest -> Aff (err :: RequestError | eff) CreateNetworkInterfacePermissionResult
```

<p>Grants an AWS authorized partner account permission to attach the specified network interface to an instance in their account.</p> <p>You can grant permission to a single AWS account only, and only one account at a time.</p>

#### `createPlacementGroup`

``` purescript
createPlacementGroup :: forall eff. CreatePlacementGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates a placement group in which to launch instances. The strategy of the placement group determines how the instances are organized within the group. </p> <p>A <code>cluster</code> placement group is a logical grouping of instances within a single Availability Zone that benefit from low network latency, high network throughput. A <code>spread</code> placement group places instances on distinct hardware.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html">Placement Groups</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `createReservedInstancesListing`

``` purescript
createReservedInstancesListing :: forall eff. CreateReservedInstancesListingRequest -> Aff (err :: RequestError | eff) CreateReservedInstancesListingResult
```

<p>Creates a listing for Amazon EC2 Standard Reserved Instances to be sold in the Reserved Instance Marketplace. You can submit one Standard Reserved Instance listing at a time. To get a list of your Standard Reserved Instances, you can use the <a>DescribeReservedInstances</a> operation.</p> <note> <p>Only Standard Reserved Instances with a capacity reservation can be sold in the Reserved Instance Marketplace. Convertible Reserved Instances and Standard Reserved Instances with a regional benefit cannot be sold.</p> </note> <p>The Reserved Instance Marketplace matches sellers who want to resell Standard Reserved Instance capacity that they no longer need with buyers who want to purchase additional capacity. Reserved Instances bought and sold through the Reserved Instance Marketplace work like any other Reserved Instances.</p> <p>To sell your Standard Reserved Instances, you must first register as a seller in the Reserved Instance Marketplace. After completing the registration process, you can create a Reserved Instance Marketplace listing of some or all of your Standard Reserved Instances, and specify the upfront price to receive for them. Your Standard Reserved Instance listings then become available for purchase. To view the details of your Standard Reserved Instance listing, you can use the <a>DescribeReservedInstancesListings</a> operation.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html">Reserved Instance Marketplace</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `createRoute`

``` purescript
createRoute :: forall eff. CreateRouteRequest -> Aff (err :: RequestError | eff) CreateRouteResult
```

<p>Creates a route in a route table within a VPC.</p> <p>You must specify one of the following targets: Internet gateway or virtual private gateway, NAT instance, NAT gateway, VPC peering connection, network interface, or egress-only Internet gateway.</p> <p>When determining how to route traffic, we use the route with the most specific match. For example, traffic is destined for the IPv4 address <code>192.0.2.3</code>, and the route table includes the following two IPv4 routes:</p> <ul> <li> <p> <code>192.0.2.0/24</code> (goes to some target A)</p> </li> <li> <p> <code>192.0.2.0/28</code> (goes to some target B)</p> </li> </ul> <p>Both routes apply to the traffic destined for <code>192.0.2.3</code>. However, the second route in the list covers a smaller number of IP addresses and is therefore more specific, so we use that route to determine where to target the traffic.</p> <p>For more information about route tables, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html">Route Tables</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `createRouteTable`

``` purescript
createRouteTable :: forall eff. CreateRouteTableRequest -> Aff (err :: RequestError | eff) CreateRouteTableResult
```

<p>Creates a route table for the specified VPC. After you create a route table, you can add routes and associate the table with a subnet.</p> <p>For more information about route tables, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html">Route Tables</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `createSecurityGroup`

``` purescript
createSecurityGroup :: forall eff. CreateSecurityGroupRequest -> Aff (err :: RequestError | eff) CreateSecurityGroupResult
```

<p>Creates a security group.</p> <p>A security group is for use with instances either in the EC2-Classic platform or in a specific VPC. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html">Amazon EC2 Security Groups</a> in the <i>Amazon Elastic Compute Cloud User Guide</i> and <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html">Security Groups for Your VPC</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p> <important> <p>EC2-Classic: You can have up to 500 security groups.</p> <p>EC2-VPC: You can create up to 500 security groups per VPC.</p> </important> <p>When you create a security group, you specify a friendly name of your choice. You can have a security group for use in EC2-Classic with the same name as a security group for use in a VPC. However, you can't have two security groups for use in EC2-Classic with the same name or two security groups for use in a VPC with the same name.</p> <p>You have a default security group for use in EC2-Classic and a default security group for use in your VPC. If you don't specify a security group when you launch an instance, the instance is launched into the appropriate default security group. A default security group includes a default rule that grants instances unrestricted network access to each other.</p> <p>You can add or remove rules from your security groups using <a>AuthorizeSecurityGroupIngress</a>, <a>AuthorizeSecurityGroupEgress</a>, <a>RevokeSecurityGroupIngress</a>, and <a>RevokeSecurityGroupEgress</a>.</p>

#### `createSnapshot`

``` purescript
createSnapshot :: forall eff. CreateSnapshotRequest -> Aff (err :: RequestError | eff) Snapshot
```

<p>Creates a snapshot of an EBS volume and stores it in Amazon S3. You can use snapshots for backups, to make copies of EBS volumes, and to save data before shutting down an instance.</p> <p>When a snapshot is created, any AWS Marketplace product codes that are associated with the source volume are propagated to the snapshot.</p> <p>You can take a snapshot of an attached volume that is in use. However, snapshots only capture data that has been written to your EBS volume at the time the snapshot command is issued; this may exclude any data that has been cached by any applications or the operating system. If you can pause any file systems on the volume long enough to take a snapshot, your snapshot should be complete. However, if you cannot pause all file writes to the volume, you should unmount the volume from within the instance, issue the snapshot command, and then remount the volume to ensure a consistent and complete snapshot. You may remount and use your volume while the snapshot status is <code>pending</code>.</p> <p>To create a snapshot for EBS volumes that serve as root devices, you should stop the instance before taking the snapshot.</p> <p>Snapshots that are taken from encrypted volumes are automatically encrypted. Volumes that are created from encrypted snapshots are also automatically encrypted. Your encrypted volumes and any associated snapshots always remain protected.</p> <p>You can tag your snapshots during creation. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html">Tagging Your Amazon EC2 Resources</a>.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AmazonEBS.html">Amazon Elastic Block Store</a> and <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html">Amazon EBS Encryption</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `createSpotDatafeedSubscription`

``` purescript
createSpotDatafeedSubscription :: forall eff. CreateSpotDatafeedSubscriptionRequest -> Aff (err :: RequestError | eff) CreateSpotDatafeedSubscriptionResult
```

<p>Creates a data feed for Spot Instances, enabling you to view Spot Instance usage logs. You can create one data feed per AWS account. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html">Spot Instance Data Feed</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `createSubnet`

``` purescript
createSubnet :: forall eff. CreateSubnetRequest -> Aff (err :: RequestError | eff) CreateSubnetResult
```

<p>Creates a subnet in an existing VPC.</p> <p>When you create each subnet, you provide the VPC ID and the IPv4 CIDR block you want for the subnet. After you create a subnet, you can't change its CIDR block. The size of the subnet's IPv4 CIDR block can be the same as a VPC's IPv4 CIDR block, or a subset of a VPC's IPv4 CIDR block. If you create more than one subnet in a VPC, the subnets' CIDR blocks must not overlap. The smallest IPv4 subnet (and VPC) you can create uses a /28 netmask (16 IPv4 addresses), and the largest uses a /16 netmask (65,536 IPv4 addresses).</p> <p>If you've associated an IPv6 CIDR block with your VPC, you can create a subnet with an IPv6 CIDR block that uses a /64 prefix length. </p> <important> <p>AWS reserves both the first four and the last IPv4 address in each subnet's CIDR block. They're not available for use.</p> </important> <p>If you add more than one subnet to a VPC, they're set up in a star topology with a logical router in the middle.</p> <p>If you launch an instance in a VPC using an Amazon EBS-backed AMI, the IP address doesn't change if you stop and restart the instance (unlike a similar instance launched outside a VPC, which gets a new IP address when restarted). It's therefore possible to have a subnet with no running instances (they're all stopped), but no remaining IP addresses available.</p> <p>For more information about subnets, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html">Your VPC and Subnets</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `createTags`

``` purescript
createTags :: forall eff. CreateTagsRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds or overwrites one or more tags for the specified Amazon EC2 resource or resources. Each resource can have a maximum of 50 tags. Each tag consists of a key and optional value. Tag keys must be unique per resource.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html">Tagging Your Resources</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>. For more information about creating IAM policies that control users' access to resources based on tags, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-iam-actions-resources.html">Supported Resource-Level Permissions for Amazon EC2 API Actions</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `createVolume`

``` purescript
createVolume :: forall eff. CreateVolumeRequest -> Aff (err :: RequestError | eff) Volume
```

<p>Creates an EBS volume that can be attached to an instance in the same Availability Zone. The volume is created in the regional endpoint that you send the HTTP request to. For more information see <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html">Regions and Endpoints</a>.</p> <p>You can create a new empty volume or restore a volume from an EBS snapshot. Any AWS Marketplace product codes from the snapshot are propagated to the volume.</p> <p>You can create encrypted volumes with the <code>Encrypted</code> parameter. Encrypted volumes may only be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are also automatically encrypted. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html">Amazon EBS Encryption</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>You can tag your volumes during creation. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html">Tagging Your Amazon EC2 Resources</a>.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-creating-volume.html">Creating an Amazon EBS Volume</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `createVpc`

``` purescript
createVpc :: forall eff. CreateVpcRequest -> Aff (err :: RequestError | eff) CreateVpcResult
```

<p>Creates a VPC with the specified IPv4 CIDR block. The smallest VPC you can create uses a /28 netmask (16 IPv4 addresses), and the largest uses a /16 netmask (65,536 IPv4 addresses). To help you decide how big to make your VPC, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html">Your VPC and Subnets</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p> <p>You can optionally request an Amazon-provided IPv6 CIDR block for the VPC. The IPv6 CIDR block uses a /56 prefix length, and is allocated from Amazon's pool of IPv6 addresses. You cannot choose the IPv6 range for your VPC.</p> <p>By default, each instance you launch in the VPC has the default DHCP options, which includes only a default DNS server that we provide (AmazonProvidedDNS). For more information about DHCP options, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html">DHCP Options Sets</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p> <p>You can specify the instance tenancy value for the VPC when you create it. You can't change this value for the VPC after you create it. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-instance.html">Dedicated Instances</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `createVpcEndpoint`

``` purescript
createVpcEndpoint :: forall eff. CreateVpcEndpointRequest -> Aff (err :: RequestError | eff) CreateVpcEndpointResult
```

<p>Creates a VPC endpoint for a specified service. An endpoint enables you to create a private connection between your VPC and the service. The service may be provided by AWS, an AWS Marketplace partner, or another AWS account. For more information, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-endpoints.html">VPC Endpoints</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p> <p>A <code>gateway</code> endpoint serves as a target for a route in your route table for traffic destined for the AWS service. You can specify an endpoint policy to attach to the endpoint that will control access to the service from your VPC. You can also specify the VPC route tables that use the endpoint.</p> <p>An <code>interface</code> endpoint is a network interface in your subnet that serves as an endpoint for communicating with the specified service. You can specify the subnets in which to create an endpoint, and the security groups to associate with the endpoint network interface.</p> <p>Use <a>DescribeVpcEndpointServices</a> to get a list of supported services.</p>

#### `createVpcEndpointConnectionNotification`

``` purescript
createVpcEndpointConnectionNotification :: forall eff. CreateVpcEndpointConnectionNotificationRequest -> Aff (err :: RequestError | eff) CreateVpcEndpointConnectionNotificationResult
```

<p>Creates a connection notification for a specified VPC endpoint or VPC endpoint service. A connection notification notifies you of specific endpoint events. You must create an SNS topic to receive notifications. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html">Create a Topic</a> in the <i>Amazon Simple Notification Service Developer Guide</i>.</p> <p>You can create a connection notification for interface endpoints only.</p>

#### `createVpcEndpointServiceConfiguration`

``` purescript
createVpcEndpointServiceConfiguration :: forall eff. CreateVpcEndpointServiceConfigurationRequest -> Aff (err :: RequestError | eff) CreateVpcEndpointServiceConfigurationResult
```

<p>Creates a VPC endpoint service configuration to which service consumers (AWS accounts, IAM users, and IAM roles) can connect. Service consumers can create an interface VPC endpoint to connect to your service.</p> <p>To create an endpoint service configuration, you must first create a Network Load Balancer for your service. For more information, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/endpoint-service.html">VPC Endpoint Services</a> in the <i>Amazon Virtual Private Cloud User Guide</i>. </p>

#### `createVpcPeeringConnection`

``` purescript
createVpcPeeringConnection :: forall eff. CreateVpcPeeringConnectionRequest -> Aff (err :: RequestError | eff) CreateVpcPeeringConnectionResult
```

<p>Requests a VPC peering connection between two VPCs: a requester VPC that you own and an accepter VPC with which to create the connection. The accepter VPC can belong to another AWS account and can be in a different region to the requester VPC. The requester VPC and accepter VPC cannot have overlapping CIDR blocks.</p> <note> <p>Limitations and rules apply to a VPC peering connection. For more information, see the <a href="http://docs.aws.amazon.com/AmazonVPC/latest/PeeringGuide/vpc-peering-basics.html#vpc-peering-limitations">limitations</a> section in the <i>VPC Peering Guide</i>.</p> </note> <p>The owner of the accepter VPC must accept the peering request to activate the peering connection. The VPC peering connection request expires after 7 days, after which it cannot be accepted or rejected.</p> <p>If you create a VPC peering connection request between VPCs with overlapping CIDR blocks, the VPC peering connection has a status of <code>failed</code>.</p>

#### `createVpnConnection`

``` purescript
createVpnConnection :: forall eff. CreateVpnConnectionRequest -> Aff (err :: RequestError | eff) CreateVpnConnectionResult
```

<p>Creates a VPN connection between an existing virtual private gateway and a VPN customer gateway. The only supported connection type is <code>ipsec.1</code>.</p> <p>The response includes information that you need to give to your network administrator to configure your customer gateway.</p> <important> <p>We strongly recommend that you use HTTPS when calling this operation because the response contains sensitive cryptographic information for configuring your customer gateway.</p> </important> <p>If you decide to shut down your VPN connection for any reason and later create a new VPN connection, you must reconfigure your customer gateway with the new information returned from this call.</p> <p>This is an idempotent operation. If you perform the operation more than once, Amazon EC2 doesn't return an error.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html">AWS Managed VPN Connections</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `createVpnConnectionRoute`

``` purescript
createVpnConnectionRoute :: forall eff. CreateVpnConnectionRouteRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates a static route associated with a VPN connection between an existing virtual private gateway and a VPN customer gateway. The static route allows traffic to be routed from the virtual private gateway to the VPN customer gateway.</p> <p>For more information about VPN connections, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html">AWS Managed VPN Connections</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `createVpnGateway`

``` purescript
createVpnGateway :: forall eff. CreateVpnGatewayRequest -> Aff (err :: RequestError | eff) CreateVpnGatewayResult
```

<p>Creates a virtual private gateway. A virtual private gateway is the endpoint on the VPC side of your VPN connection. You can create a virtual private gateway before creating the VPC itself.</p> <p>For more information about virtual private gateways, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html">AWS Managed VPN Connections</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `deleteCustomerGateway`

``` purescript
deleteCustomerGateway :: forall eff. DeleteCustomerGatewayRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified customer gateway. You must delete the VPN connection before you can delete the customer gateway.</p>

#### `deleteDhcpOptions`

``` purescript
deleteDhcpOptions :: forall eff. DeleteDhcpOptionsRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified set of DHCP options. You must disassociate the set of DHCP options before you can delete it. You can disassociate the set of DHCP options by associating either a new set of options or the default set of options with the VPC.</p>

#### `deleteEgressOnlyInternetGateway`

``` purescript
deleteEgressOnlyInternetGateway :: forall eff. DeleteEgressOnlyInternetGatewayRequest -> Aff (err :: RequestError | eff) DeleteEgressOnlyInternetGatewayResult
```

<p>Deletes an egress-only Internet gateway.</p>

#### `deleteFlowLogs`

``` purescript
deleteFlowLogs :: forall eff. DeleteFlowLogsRequest -> Aff (err :: RequestError | eff) DeleteFlowLogsResult
```

<p>Deletes one or more flow logs.</p>

#### `deleteFpgaImage`

``` purescript
deleteFpgaImage :: forall eff. DeleteFpgaImageRequest -> Aff (err :: RequestError | eff) DeleteFpgaImageResult
```

<p>Deletes the specified Amazon FPGA Image (AFI).</p>

#### `deleteInternetGateway`

``` purescript
deleteInternetGateway :: forall eff. DeleteInternetGatewayRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified Internet gateway. You must detach the Internet gateway from the VPC before you can delete it.</p>

#### `deleteKeyPair`

``` purescript
deleteKeyPair :: forall eff. DeleteKeyPairRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified key pair, by removing the public key from Amazon EC2.</p>

#### `deleteLaunchTemplate`

``` purescript
deleteLaunchTemplate :: forall eff. DeleteLaunchTemplateRequest -> Aff (err :: RequestError | eff) DeleteLaunchTemplateResult
```

<p>Deletes a launch template. Deleting a launch template deletes all of its versions.</p>

#### `deleteLaunchTemplateVersions`

``` purescript
deleteLaunchTemplateVersions :: forall eff. DeleteLaunchTemplateVersionsRequest -> Aff (err :: RequestError | eff) DeleteLaunchTemplateVersionsResult
```

<p>Deletes one or more versions of a launch template. You cannot delete the default version of a launch template; you must first assign a different version as the default. If the default version is the only version for the launch template, you must delete the entire launch template using <a>DeleteLaunchTemplate</a>.</p>

#### `deleteNatGateway`

``` purescript
deleteNatGateway :: forall eff. DeleteNatGatewayRequest -> Aff (err :: RequestError | eff) DeleteNatGatewayResult
```

<p>Deletes the specified NAT gateway. Deleting a NAT gateway disassociates its Elastic IP address, but does not release the address from your account. Deleting a NAT gateway does not delete any NAT gateway routes in your route tables.</p>

#### `deleteNetworkAcl`

``` purescript
deleteNetworkAcl :: forall eff. DeleteNetworkAclRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified network ACL. You can't delete the ACL if it's associated with any subnets. You can't delete the default network ACL.</p>

#### `deleteNetworkAclEntry`

``` purescript
deleteNetworkAclEntry :: forall eff. DeleteNetworkAclEntryRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified ingress or egress entry (rule) from the specified network ACL.</p>

#### `deleteNetworkInterface`

``` purescript
deleteNetworkInterface :: forall eff. DeleteNetworkInterfaceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified network interface. You must detach the network interface before you can delete it.</p>

#### `deleteNetworkInterfacePermission`

``` purescript
deleteNetworkInterfacePermission :: forall eff. DeleteNetworkInterfacePermissionRequest -> Aff (err :: RequestError | eff) DeleteNetworkInterfacePermissionResult
```

<p>Deletes a permission for a network interface. By default, you cannot delete the permission if the account for which you're removing the permission has attached the network interface to an instance. However, you can force delete the permission, regardless of any attachment.</p>

#### `deletePlacementGroup`

``` purescript
deletePlacementGroup :: forall eff. DeletePlacementGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified placement group. You must terminate all instances in the placement group before you can delete the placement group. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html">Placement Groups</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `deleteRoute`

``` purescript
deleteRoute :: forall eff. DeleteRouteRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified route from the specified route table.</p>

#### `deleteRouteTable`

``` purescript
deleteRouteTable :: forall eff. DeleteRouteTableRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified route table. You must disassociate the route table from any subnets before you can delete it. You can't delete the main route table.</p>

#### `deleteSecurityGroup`

``` purescript
deleteSecurityGroup :: forall eff. DeleteSecurityGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a security group.</p> <p>If you attempt to delete a security group that is associated with an instance, or is referenced by another security group, the operation fails with <code>InvalidGroup.InUse</code> in EC2-Classic or <code>DependencyViolation</code> in EC2-VPC.</p>

#### `deleteSnapshot`

``` purescript
deleteSnapshot :: forall eff. DeleteSnapshotRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified snapshot.</p> <p>When you make periodic snapshots of a volume, the snapshots are incremental, and only the blocks on the device that have changed since your last snapshot are saved in the new snapshot. When you delete a snapshot, only the data not needed for any other snapshot is removed. So regardless of which prior snapshots have been deleted, all active snapshots will have access to all the information needed to restore the volume.</p> <p>You cannot delete a snapshot of the root device of an EBS volume used by a registered AMI. You must first de-register the AMI before you can delete the snapshot.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-snapshot.html">Deleting an Amazon EBS Snapshot</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `deleteSpotDatafeedSubscription`

``` purescript
deleteSpotDatafeedSubscription :: forall eff. DeleteSpotDatafeedSubscriptionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the data feed for Spot Instances.</p>

#### `deleteSubnet`

``` purescript
deleteSubnet :: forall eff. DeleteSubnetRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified subnet. You must terminate all running instances in the subnet before you can delete the subnet.</p>

#### `deleteTags`

``` purescript
deleteTags :: forall eff. DeleteTagsRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified set of tags from the specified set of resources.</p> <p>To list the current tags, use <a>DescribeTags</a>. For more information about tags, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html">Tagging Your Resources</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `deleteVolume`

``` purescript
deleteVolume :: forall eff. DeleteVolumeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified EBS volume. The volume must be in the <code>available</code> state (not attached to an instance).</p> <note> <p>The volume may remain in the <code>deleting</code> state for several minutes.</p> </note> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-volume.html">Deleting an Amazon EBS Volume</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `deleteVpc`

``` purescript
deleteVpc :: forall eff. DeleteVpcRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified VPC. You must detach or delete all gateways and resources that are associated with the VPC before you can delete it. For example, you must terminate all instances running in the VPC, delete all security groups associated with the VPC (except the default one), delete all route tables associated with the VPC (except the default one), and so on.</p>

#### `deleteVpcEndpointConnectionNotifications`

``` purescript
deleteVpcEndpointConnectionNotifications :: forall eff. DeleteVpcEndpointConnectionNotificationsRequest -> Aff (err :: RequestError | eff) DeleteVpcEndpointConnectionNotificationsResult
```

<p>Deletes one or more VPC endpoint connection notifications.</p>

#### `deleteVpcEndpointServiceConfigurations`

``` purescript
deleteVpcEndpointServiceConfigurations :: forall eff. DeleteVpcEndpointServiceConfigurationsRequest -> Aff (err :: RequestError | eff) DeleteVpcEndpointServiceConfigurationsResult
```

<p>Deletes one or more VPC endpoint service configurations in your account. Before you delete the endpoint service configuration, you must reject any <code>Available</code> or <code>PendingAcceptance</code> interface endpoint connections that are attached to the service.</p>

#### `deleteVpcEndpoints`

``` purescript
deleteVpcEndpoints :: forall eff. DeleteVpcEndpointsRequest -> Aff (err :: RequestError | eff) DeleteVpcEndpointsResult
```

<p>Deletes one or more specified VPC endpoints. Deleting a gateway endpoint also deletes the endpoint routes in the route tables that were associated with the endpoint. Deleting an interface endpoint deletes the endpoint network interfaces.</p>

#### `deleteVpcPeeringConnection`

``` purescript
deleteVpcPeeringConnection :: forall eff. DeleteVpcPeeringConnectionRequest -> Aff (err :: RequestError | eff) DeleteVpcPeeringConnectionResult
```

<p>Deletes a VPC peering connection. Either the owner of the requester VPC or the owner of the accepter VPC can delete the VPC peering connection if it's in the <code>active</code> state. The owner of the requester VPC can delete a VPC peering connection in the <code>pending-acceptance</code> state. </p>

#### `deleteVpnConnection`

``` purescript
deleteVpnConnection :: forall eff. DeleteVpnConnectionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified VPN connection.</p> <p>If you're deleting the VPC and its associated components, we recommend that you detach the virtual private gateway from the VPC and delete the VPC before deleting the VPN connection. If you believe that the tunnel credentials for your VPN connection have been compromised, you can delete the VPN connection and create a new one that has new keys, without needing to delete the VPC or virtual private gateway. If you create a new VPN connection, you must reconfigure the customer gateway using the new configuration information returned with the new VPN connection ID.</p>

#### `deleteVpnConnectionRoute`

``` purescript
deleteVpnConnectionRoute :: forall eff. DeleteVpnConnectionRouteRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified static route associated with a VPN connection between an existing virtual private gateway and a VPN customer gateway. The static route allows traffic to be routed from the virtual private gateway to the VPN customer gateway.</p>

#### `deleteVpnGateway`

``` purescript
deleteVpnGateway :: forall eff. DeleteVpnGatewayRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified virtual private gateway. We recommend that before you delete a virtual private gateway, you detach it from the VPC and delete the VPN connection. Note that you don't need to delete the virtual private gateway if you plan to delete and recreate the VPN connection between your VPC and your network.</p>

#### `deregisterImage`

``` purescript
deregisterImage :: forall eff. DeregisterImageRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deregisters the specified AMI. After you deregister an AMI, it can't be used to launch new instances; however, it doesn't affect any instances that you've already launched from the AMI. You'll continue to incur usage costs for those instances until you terminate them.</p> <p>When you deregister an Amazon EBS-backed AMI, it doesn't affect the snapshot that was created for the root volume of the instance during the AMI creation process. When you deregister an instance store-backed AMI, it doesn't affect the files that you uploaded to Amazon S3 when you created the AMI.</p>

#### `describeAccountAttributes`

``` purescript
describeAccountAttributes :: forall eff. DescribeAccountAttributesRequest -> Aff (err :: RequestError | eff) DescribeAccountAttributesResult
```

<p>Describes attributes of your AWS account. The following are the supported account attributes:</p> <ul> <li> <p> <code>supported-platforms</code>: Indicates whether your account can launch instances into EC2-Classic and EC2-VPC, or only into EC2-VPC.</p> </li> <li> <p> <code>default-vpc</code>: The ID of the default VPC for your account, or <code>none</code>.</p> </li> <li> <p> <code>max-instances</code>: The maximum number of On-Demand Instances that you can run.</p> </li> <li> <p> <code>vpc-max-security-groups-per-interface</code>: The maximum number of security groups that you can assign to a network interface.</p> </li> <li> <p> <code>max-elastic-ips</code>: The maximum number of Elastic IP addresses that you can allocate for use with EC2-Classic. </p> </li> <li> <p> <code>vpc-max-elastic-ips</code>: The maximum number of Elastic IP addresses that you can allocate for use with EC2-VPC.</p> </li> </ul>

#### `describeAddresses`

``` purescript
describeAddresses :: forall eff. DescribeAddressesRequest -> Aff (err :: RequestError | eff) DescribeAddressesResult
```

<p>Describes one or more of your Elastic IP addresses.</p> <p>An Elastic IP address is for use in either the EC2-Classic platform or in a VPC. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html">Elastic IP Addresses</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeAggregateIdFormat`

``` purescript
describeAggregateIdFormat :: forall eff. DescribeAggregateIdFormatRequest -> Aff (err :: RequestError | eff) DescribeAggregateIdFormatResult
```

<p>Describes the longer ID format settings for all resource types in a specific region. This request is useful for performing a quick audit to determine whether a specific region is fully opted in for longer IDs (17-character IDs).</p> <p>This request only returns information about resource types that support longer IDs.</p> <p>The following resource types support longer IDs: <code>bundle</code> | <code>conversion-task</code> | <code>dhcp-options</code> | <code>elastic-ip-allocation</code> | <code>elastic-ip-association</code> | <code>export-task</code> | <code>flow-log</code> | <code>image</code> | <code>import-task</code> | <code>instance</code> | <code>internet-gateway</code> | <code>network-acl</code> | <code>network-acl-association</code> | <code>network-interface</code> | <code>network-interface-attachment</code> | <code>prefix-list</code> | <code>reservation</code> | <code>route-table</code> | <code>route-table-association</code> | <code>security-group</code> | <code>snapshot</code> | <code>subnet</code> | <code>subnet-cidr-block-association</code> | <code>volume</code> | <code>vpc</code> | <code>vpc-cidr-block-association</code> | <code>vpc-peering-connection</code>.</p>

#### `describeAvailabilityZones`

``` purescript
describeAvailabilityZones :: forall eff. DescribeAvailabilityZonesRequest -> Aff (err :: RequestError | eff) DescribeAvailabilityZonesResult
```

<p>Describes one or more of the Availability Zones that are available to you. The results include zones only for the region you're currently using. If there is an event impacting an Availability Zone, you can use this request to view the state and any provided message for that Availability Zone.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html">Regions and Availability Zones</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeBundleTasks`

``` purescript
describeBundleTasks :: forall eff. DescribeBundleTasksRequest -> Aff (err :: RequestError | eff) DescribeBundleTasksResult
```

<p>Describes one or more of your bundling tasks.</p> <note> <p>Completed bundle tasks are listed for only a limited time. If your bundle task is no longer in the list, you can still register an AMI from it. Just use <code>RegisterImage</code> with the Amazon S3 bucket name and image manifest name you provided to the bundle task.</p> </note>

#### `describeClassicLinkInstances`

``` purescript
describeClassicLinkInstances :: forall eff. DescribeClassicLinkInstancesRequest -> Aff (err :: RequestError | eff) DescribeClassicLinkInstancesResult
```

<p>Describes one or more of your linked EC2-Classic instances. This request only returns information about EC2-Classic instances linked to a VPC through ClassicLink; you cannot use this request to return information about other instances.</p>

#### `describeConversionTasks`

``` purescript
describeConversionTasks :: forall eff. DescribeConversionTasksRequest -> Aff (err :: RequestError | eff) DescribeConversionTasksResult
```

<p>Describes one or more of your conversion tasks. For more information, see the <a href="http://docs.aws.amazon.com/vm-import/latest/userguide/">VM Import/Export User Guide</a>.</p> <p>For information about the import manifest referenced by this API action, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html">VM Import Manifest</a>.</p>

#### `describeCustomerGateways`

``` purescript
describeCustomerGateways :: forall eff. DescribeCustomerGatewaysRequest -> Aff (err :: RequestError | eff) DescribeCustomerGatewaysResult
```

<p>Describes one or more of your VPN customer gateways.</p> <p>For more information about VPN customer gateways, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html">AWS Managed VPN Connections</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `describeDhcpOptions`

``` purescript
describeDhcpOptions :: forall eff. DescribeDhcpOptionsRequest -> Aff (err :: RequestError | eff) DescribeDhcpOptionsResult
```

<p>Describes one or more of your DHCP options sets.</p> <p>For more information about DHCP options sets, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html">DHCP Options Sets</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `describeEgressOnlyInternetGateways`

``` purescript
describeEgressOnlyInternetGateways :: forall eff. DescribeEgressOnlyInternetGatewaysRequest -> Aff (err :: RequestError | eff) DescribeEgressOnlyInternetGatewaysResult
```

<p>Describes one or more of your egress-only Internet gateways.</p>

#### `describeElasticGpus`

``` purescript
describeElasticGpus :: forall eff. DescribeElasticGpusRequest -> Aff (err :: RequestError | eff) DescribeElasticGpusResult
```

<p>Describes the Elastic GPUs associated with your instances. For more information about Elastic GPUs, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-gpus.html">Amazon EC2 Elastic GPUs</a>.</p>

#### `describeExportTasks`

``` purescript
describeExportTasks :: forall eff. DescribeExportTasksRequest -> Aff (err :: RequestError | eff) DescribeExportTasksResult
```

<p>Describes one or more of your export tasks.</p>

#### `describeFlowLogs`

``` purescript
describeFlowLogs :: forall eff. DescribeFlowLogsRequest -> Aff (err :: RequestError | eff) DescribeFlowLogsResult
```

<p>Describes one or more flow logs. To view the information in your flow logs (the log streams for the network interfaces), you must use the CloudWatch Logs console or the CloudWatch Logs API.</p>

#### `describeFpgaImageAttribute`

``` purescript
describeFpgaImageAttribute :: forall eff. DescribeFpgaImageAttributeRequest -> Aff (err :: RequestError | eff) DescribeFpgaImageAttributeResult
```

<p>Describes the specified attribute of the specified Amazon FPGA Image (AFI).</p>

#### `describeFpgaImages`

``` purescript
describeFpgaImages :: forall eff. DescribeFpgaImagesRequest -> Aff (err :: RequestError | eff) DescribeFpgaImagesResult
```

<p>Describes one or more available Amazon FPGA Images (AFIs). These include public AFIs, private AFIs that you own, and AFIs owned by other AWS accounts for which you have load permissions.</p>

#### `describeHostReservationOfferings`

``` purescript
describeHostReservationOfferings :: forall eff. DescribeHostReservationOfferingsRequest -> Aff (err :: RequestError | eff) DescribeHostReservationOfferingsResult
```

<p>Describes the Dedicated Host Reservations that are available to purchase.</p> <p>The results describe all the Dedicated Host Reservation offerings, including offerings that may not match the instance family and region of your Dedicated Hosts. When purchasing an offering, ensure that the the instance family and region of the offering matches that of the Dedicated Host/s it will be associated with. For an overview of supported instance types, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-overview.html">Dedicated Hosts Overview</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>. </p>

#### `describeHostReservations`

``` purescript
describeHostReservations :: forall eff. DescribeHostReservationsRequest -> Aff (err :: RequestError | eff) DescribeHostReservationsResult
```

<p>Describes Dedicated Host Reservations which are associated with Dedicated Hosts in your account.</p>

#### `describeHosts`

``` purescript
describeHosts :: forall eff. DescribeHostsRequest -> Aff (err :: RequestError | eff) DescribeHostsResult
```

<p>Describes one or more of your Dedicated Hosts.</p> <p>The results describe only the Dedicated Hosts in the region you're currently using. All listed instances consume capacity on your Dedicated Host. Dedicated Hosts that have recently been released will be listed with the state <code>released</code>.</p>

#### `describeIamInstanceProfileAssociations`

``` purescript
describeIamInstanceProfileAssociations :: forall eff. DescribeIamInstanceProfileAssociationsRequest -> Aff (err :: RequestError | eff) DescribeIamInstanceProfileAssociationsResult
```

<p>Describes your IAM instance profile associations.</p>

#### `describeIdFormat`

``` purescript
describeIdFormat :: forall eff. DescribeIdFormatRequest -> Aff (err :: RequestError | eff) DescribeIdFormatResult
```

<p>Describes the ID format settings for your resources on a per-region basis, for example, to view which resource types are enabled for longer IDs. This request only returns information about resource types whose ID formats can be modified; it does not return information about other resource types.</p> <p>The following resource types support longer IDs: <code>bundle</code> | <code>conversion-task</code> | <code>dhcp-options</code> | <code>elastic-ip-allocation</code> | <code>elastic-ip-association</code> | <code>export-task</code> | <code>flow-log</code> | <code>image</code> | <code>import-task</code> | <code>instance</code> | <code>internet-gateway</code> | <code>network-acl</code> | <code>network-acl-association</code> | <code>network-interface</code> | <code>network-interface-attachment</code> | <code>prefix-list</code> | <code>reservation</code> | <code>route-table</code> | <code>route-table-association</code> | <code>security-group</code> | <code>snapshot</code> | <code>subnet</code> | <code>subnet-cidr-block-association</code> | <code>volume</code> | <code>vpc</code> | <code>vpc-cidr-block-association</code> | <code>vpc-peering-connection</code>. </p> <p>These settings apply to the IAM user who makes the request; they do not apply to the entire AWS account. By default, an IAM user defaults to the same settings as the root user, unless they explicitly override the settings by running the <a>ModifyIdFormat</a> command. Resources created with longer IDs are visible to all IAM users, regardless of these settings and provided that they have permission to use the relevant <code>Describe</code> command for the resource type.</p>

#### `describeIdentityIdFormat`

``` purescript
describeIdentityIdFormat :: forall eff. DescribeIdentityIdFormatRequest -> Aff (err :: RequestError | eff) DescribeIdentityIdFormatResult
```

<p>Describes the ID format settings for resources for the specified IAM user, IAM role, or root user. For example, you can view the resource types that are enabled for longer IDs. This request only returns information about resource types whose ID formats can be modified; it does not return information about other resource types. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html">Resource IDs</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>. </p> <p>The following resource types support longer IDs: <code>bundle</code> | <code>conversion-task</code> | <code>dhcp-options</code> | <code>elastic-ip-allocation</code> | <code>elastic-ip-association</code> | <code>export-task</code> | <code>flow-log</code> | <code>image</code> | <code>import-task</code> | <code>instance</code> | <code>internet-gateway</code> | <code>network-acl</code> | <code>network-acl-association</code> | <code>network-interface</code> | <code>network-interface-attachment</code> | <code>prefix-list</code> | <code>reservation</code> | <code>route-table</code> | <code>route-table-association</code> | <code>security-group</code> | <code>snapshot</code> | <code>subnet</code> | <code>subnet-cidr-block-association</code> | <code>volume</code> | <code>vpc</code> | <code>vpc-cidr-block-association</code> | <code>vpc-peering-connection</code>. </p> <p>These settings apply to the principal specified in the request. They do not apply to the principal that makes the request.</p>

#### `describeImageAttribute`

``` purescript
describeImageAttribute :: forall eff. DescribeImageAttributeRequest -> Aff (err :: RequestError | eff) ImageAttribute
```

<p>Describes the specified attribute of the specified AMI. You can specify only one attribute at a time.</p>

#### `describeImages`

``` purescript
describeImages :: forall eff. DescribeImagesRequest -> Aff (err :: RequestError | eff) DescribeImagesResult
```

<p>Describes one or more of the images (AMIs, AKIs, and ARIs) available to you. Images available to you include public images, private images that you own, and private images owned by other AWS accounts but for which you have explicit launch permissions.</p> <note> <p>Deregistered images are included in the returned results for an unspecified interval after deregistration.</p> </note>

#### `describeImportImageTasks`

``` purescript
describeImportImageTasks :: forall eff. DescribeImportImageTasksRequest -> Aff (err :: RequestError | eff) DescribeImportImageTasksResult
```

<p>Displays details about an import virtual machine or import snapshot tasks that are already created.</p>

#### `describeImportSnapshotTasks`

``` purescript
describeImportSnapshotTasks :: forall eff. DescribeImportSnapshotTasksRequest -> Aff (err :: RequestError | eff) DescribeImportSnapshotTasksResult
```

<p>Describes your import snapshot tasks.</p>

#### `describeInstanceAttribute`

``` purescript
describeInstanceAttribute :: forall eff. DescribeInstanceAttributeRequest -> Aff (err :: RequestError | eff) InstanceAttribute
```

<p>Describes the specified attribute of the specified instance. You can specify only one attribute at a time. Valid attribute values are: <code>instanceType</code> | <code>kernel</code> | <code>ramdisk</code> | <code>userData</code> | <code>disableApiTermination</code> | <code>instanceInitiatedShutdownBehavior</code> | <code>rootDeviceName</code> | <code>blockDeviceMapping</code> | <code>productCodes</code> | <code>sourceDestCheck</code> | <code>groupSet</code> | <code>ebsOptimized</code> | <code>sriovNetSupport</code> </p>

#### `describeInstanceCreditSpecifications`

``` purescript
describeInstanceCreditSpecifications :: forall eff. DescribeInstanceCreditSpecificationsRequest -> Aff (err :: RequestError | eff) DescribeInstanceCreditSpecificationsResult
```

<p>Describes the credit option for CPU usage of one or more of your T2 instances. The credit options are <code>standard</code> and <code>unlimited</code>.</p> <p>If you do not specify an instance ID, Amazon EC2 returns only the T2 instances with the <code>unlimited</code> credit option. If you specify one or more instance IDs, Amazon EC2 returns the credit option (<code>standard</code> or <code>unlimited</code>) of those instances. If you specify an instance ID that is not valid, such as an instance that is not a T2 instance, an error is returned.</p> <p>Recently terminated instances might appear in the returned results. This interval is usually less than one hour.</p> <p>If an Availability Zone is experiencing a service disruption and you specify instance IDs in the affected zone, or do not specify any instance IDs at all, the call fails. If you specify only instance IDs in an unaffected zone, the call works normally.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/t2-instances.html">T2 Instances</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeInstanceStatus`

``` purescript
describeInstanceStatus :: forall eff. DescribeInstanceStatusRequest -> Aff (err :: RequestError | eff) DescribeInstanceStatusResult
```

<p>Describes the status of one or more instances. By default, only running instances are described, unless you specifically indicate to return the status of all instances.</p> <p>Instance status includes the following components:</p> <ul> <li> <p> <b>Status checks</b> - Amazon EC2 performs status checks on running EC2 instances to identify hardware and software issues. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-system-instance-status-check.html">Status Checks for Your Instances</a> and <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstances.html">Troubleshooting Instances with Failed Status Checks</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> </li> <li> <p> <b>Scheduled events</b> - Amazon EC2 can schedule events (such as reboot, stop, or terminate) for your instances related to hardware issues, software updates, or system maintenance. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-instances-status-check_sched.html">Scheduled Events for Your Instances</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> </li> <li> <p> <b>Instance state</b> - You can manage your instances from the moment you launch them through their termination. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html">Instance Lifecycle</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> </li> </ul>

#### `describeInstances`

``` purescript
describeInstances :: forall eff. DescribeInstancesRequest -> Aff (err :: RequestError | eff) DescribeInstancesResult
```

<p>Describes one or more of your instances.</p> <p>If you specify one or more instance IDs, Amazon EC2 returns information for those instances. If you do not specify instance IDs, Amazon EC2 returns information for all relevant instances. If you specify an instance ID that is not valid, an error is returned. If you specify an instance that you do not own, it is not included in the returned results.</p> <p>Recently terminated instances might appear in the returned results. This interval is usually less than one hour.</p> <p>If you describe instances in the rare case where an Availability Zone is experiencing a service disruption and you specify instance IDs that are in the affected zone, or do not specify any instance IDs at all, the call fails. If you describe instances and specify only instance IDs that are in an unaffected zone, the call works normally.</p>

#### `describeInternetGateways`

``` purescript
describeInternetGateways :: forall eff. DescribeInternetGatewaysRequest -> Aff (err :: RequestError | eff) DescribeInternetGatewaysResult
```

<p>Describes one or more of your Internet gateways.</p>

#### `describeKeyPairs`

``` purescript
describeKeyPairs :: forall eff. DescribeKeyPairsRequest -> Aff (err :: RequestError | eff) DescribeKeyPairsResult
```

<p>Describes one or more of your key pairs.</p> <p>For more information about key pairs, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html">Key Pairs</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeLaunchTemplateVersions`

``` purescript
describeLaunchTemplateVersions :: forall eff. DescribeLaunchTemplateVersionsRequest -> Aff (err :: RequestError | eff) DescribeLaunchTemplateVersionsResult
```

<p>Describes one or more versions of a specified launch template. You can describe all versions, individual versions, or a range of versions.</p>

#### `describeLaunchTemplates`

``` purescript
describeLaunchTemplates :: forall eff. DescribeLaunchTemplatesRequest -> Aff (err :: RequestError | eff) DescribeLaunchTemplatesResult
```

<p>Describes one or more launch templates.</p>

#### `describeMovingAddresses`

``` purescript
describeMovingAddresses :: forall eff. DescribeMovingAddressesRequest -> Aff (err :: RequestError | eff) DescribeMovingAddressesResult
```

<p>Describes your Elastic IP addresses that are being moved to the EC2-VPC platform, or that are being restored to the EC2-Classic platform. This request does not return information about any other Elastic IP addresses in your account.</p>

#### `describeNatGateways`

``` purescript
describeNatGateways :: forall eff. DescribeNatGatewaysRequest -> Aff (err :: RequestError | eff) DescribeNatGatewaysResult
```

<p>Describes one or more of the your NAT gateways.</p>

#### `describeNetworkAcls`

``` purescript
describeNetworkAcls :: forall eff. DescribeNetworkAclsRequest -> Aff (err :: RequestError | eff) DescribeNetworkAclsResult
```

<p>Describes one or more of your network ACLs.</p> <p>For more information about network ACLs, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html">Network ACLs</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `describeNetworkInterfaceAttribute`

``` purescript
describeNetworkInterfaceAttribute :: forall eff. DescribeNetworkInterfaceAttributeRequest -> Aff (err :: RequestError | eff) DescribeNetworkInterfaceAttributeResult
```

<p>Describes a network interface attribute. You can specify only one attribute at a time.</p>

#### `describeNetworkInterfacePermissions`

``` purescript
describeNetworkInterfacePermissions :: forall eff. DescribeNetworkInterfacePermissionsRequest -> Aff (err :: RequestError | eff) DescribeNetworkInterfacePermissionsResult
```

<p>Describes the permissions for your network interfaces. </p>

#### `describeNetworkInterfaces`

``` purescript
describeNetworkInterfaces :: forall eff. DescribeNetworkInterfacesRequest -> Aff (err :: RequestError | eff) DescribeNetworkInterfacesResult
```

<p>Describes one or more of your network interfaces.</p>

#### `describePlacementGroups`

``` purescript
describePlacementGroups :: forall eff. DescribePlacementGroupsRequest -> Aff (err :: RequestError | eff) DescribePlacementGroupsResult
```

<p>Describes one or more of your placement groups. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html">Placement Groups</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describePrefixLists`

``` purescript
describePrefixLists :: forall eff. DescribePrefixListsRequest -> Aff (err :: RequestError | eff) DescribePrefixListsResult
```

<p>Describes available AWS services in a prefix list format, which includes the prefix list name and prefix list ID of the service and the IP address range for the service. A prefix list ID is required for creating an outbound security group rule that allows traffic from a VPC to access an AWS service through a gateway VPC endpoint.</p>

#### `describePrincipalIdFormat`

``` purescript
describePrincipalIdFormat :: forall eff. DescribePrincipalIdFormatRequest -> Aff (err :: RequestError | eff) DescribePrincipalIdFormatResult
```

<p>Describes the ID format settings for the root user and all IAM roles and IAM users that have explicitly specified a longer ID (17-character ID) preference. </p> <p>By default, all IAM roles and IAM users default to the same ID settings as the root user, unless they explicitly override the settings. This request is useful for identifying those IAM users and IAM roles that have overridden the default ID settings.</p> <p>The following resource types support longer IDs: <code>bundle</code> | <code>conversion-task</code> | <code>dhcp-options</code> | <code>elastic-ip-allocation</code> | <code>elastic-ip-association</code> | <code>export-task</code> | <code>flow-log</code> | <code>image</code> | <code>import-task</code> | <code>instance</code> | <code>internet-gateway</code> | <code>network-acl</code> | <code>network-acl-association</code> | <code>network-interface</code> | <code>network-interface-attachment</code> | <code>prefix-list</code> | <code>reservation</code> | <code>route-table</code> | <code>route-table-association</code> | <code>security-group</code> | <code>snapshot</code> | <code>subnet</code> | <code>subnet-cidr-block-association</code> | <code>volume</code> | <code>vpc</code> | <code>vpc-cidr-block-association</code> | <code>vpc-peering-connection</code>. </p>

#### `describeRegions`

``` purescript
describeRegions :: forall eff. DescribeRegionsRequest -> Aff (err :: RequestError | eff) DescribeRegionsResult
```

<p>Describes one or more regions that are currently available to you.</p> <p>For a list of the regions supported by Amazon EC2, see <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#ec2_region">Regions and Endpoints</a>.</p>

#### `describeReservedInstances`

``` purescript
describeReservedInstances :: forall eff. DescribeReservedInstancesRequest -> Aff (err :: RequestError | eff) DescribeReservedInstancesResult
```

<p>Describes one or more of the Reserved Instances that you purchased.</p> <p>For more information about Reserved Instances, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-on-demand-reserved-instances.html">Reserved Instances</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeReservedInstancesListings`

``` purescript
describeReservedInstancesListings :: forall eff. DescribeReservedInstancesListingsRequest -> Aff (err :: RequestError | eff) DescribeReservedInstancesListingsResult
```

<p>Describes your account's Reserved Instance listings in the Reserved Instance Marketplace.</p> <p>The Reserved Instance Marketplace matches sellers who want to resell Reserved Instance capacity that they no longer need with buyers who want to purchase additional capacity. Reserved Instances bought and sold through the Reserved Instance Marketplace work like any other Reserved Instances.</p> <p>As a seller, you choose to list some or all of your Reserved Instances, and you specify the upfront price to receive for them. Your Reserved Instances are then listed in the Reserved Instance Marketplace and are available for purchase.</p> <p>As a buyer, you specify the configuration of the Reserved Instance to purchase, and the Marketplace matches what you're searching for with what's available. The Marketplace first sells the lowest priced Reserved Instances to you, and continues to sell available Reserved Instance listings to you until your demand is met. You are charged based on the total price of all of the listings that you purchase.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html">Reserved Instance Marketplace</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeReservedInstancesModifications`

``` purescript
describeReservedInstancesModifications :: forall eff. DescribeReservedInstancesModificationsRequest -> Aff (err :: RequestError | eff) DescribeReservedInstancesModificationsResult
```

<p>Describes the modifications made to your Reserved Instances. If no parameter is specified, information about all your Reserved Instances modification requests is returned. If a modification ID is specified, only information about the specific modification is returned.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html">Modifying Reserved Instances</a> in the Amazon Elastic Compute Cloud User Guide.</p>

#### `describeReservedInstancesOfferings`

``` purescript
describeReservedInstancesOfferings :: forall eff. DescribeReservedInstancesOfferingsRequest -> Aff (err :: RequestError | eff) DescribeReservedInstancesOfferingsResult
```

<p>Describes Reserved Instance offerings that are available for purchase. With Reserved Instances, you purchase the right to launch instances for a period of time. During that time period, you do not receive insufficient capacity errors, and you pay a lower usage rate than the rate charged for On-Demand instances for the actual time used.</p> <p>If you have listed your own Reserved Instances for sale in the Reserved Instance Marketplace, they will be excluded from these results. This is to ensure that you do not purchase your own Reserved Instances.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html">Reserved Instance Marketplace</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeRouteTables`

``` purescript
describeRouteTables :: forall eff. DescribeRouteTablesRequest -> Aff (err :: RequestError | eff) DescribeRouteTablesResult
```

<p>Describes one or more of your route tables.</p> <p>Each subnet in your VPC must be associated with a route table. If a subnet is not explicitly associated with any route table, it is implicitly associated with the main route table. This command does not return the subnet ID for implicit associations.</p> <p>For more information about route tables, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html">Route Tables</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `describeScheduledInstanceAvailability`

``` purescript
describeScheduledInstanceAvailability :: forall eff. DescribeScheduledInstanceAvailabilityRequest -> Aff (err :: RequestError | eff) DescribeScheduledInstanceAvailabilityResult
```

<p>Finds available schedules that meet the specified criteria.</p> <p>You can search for an available schedule no more than 3 months in advance. You must meet the minimum required duration of 1,200 hours per year. For example, the minimum daily schedule is 4 hours, the minimum weekly schedule is 24 hours, and the minimum monthly schedule is 100 hours.</p> <p>After you find a schedule that meets your needs, call <a>PurchaseScheduledInstances</a> to purchase Scheduled Instances with that schedule.</p>

#### `describeScheduledInstances`

``` purescript
describeScheduledInstances :: forall eff. DescribeScheduledInstancesRequest -> Aff (err :: RequestError | eff) DescribeScheduledInstancesResult
```

<p>Describes one or more of your Scheduled Instances.</p>

#### `describeSecurityGroupReferences`

``` purescript
describeSecurityGroupReferences :: forall eff. DescribeSecurityGroupReferencesRequest -> Aff (err :: RequestError | eff) DescribeSecurityGroupReferencesResult
```

<p>[EC2-VPC only] Describes the VPCs on the other side of a VPC peering connection that are referencing the security groups you've specified in this request.</p>

#### `describeSecurityGroups`

``` purescript
describeSecurityGroups :: forall eff. DescribeSecurityGroupsRequest -> Aff (err :: RequestError | eff) DescribeSecurityGroupsResult
```

<p>Describes one or more of your security groups.</p> <p>A security group is for use with instances either in the EC2-Classic platform or in a specific VPC. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html">Amazon EC2 Security Groups</a> in the <i>Amazon Elastic Compute Cloud User Guide</i> and <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html">Security Groups for Your VPC</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `describeSnapshotAttribute`

``` purescript
describeSnapshotAttribute :: forall eff. DescribeSnapshotAttributeRequest -> Aff (err :: RequestError | eff) DescribeSnapshotAttributeResult
```

<p>Describes the specified attribute of the specified snapshot. You can specify only one attribute at a time.</p> <p>For more information about EBS snapshots, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSSnapshots.html">Amazon EBS Snapshots</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeSnapshots`

``` purescript
describeSnapshots :: forall eff. DescribeSnapshotsRequest -> Aff (err :: RequestError | eff) DescribeSnapshotsResult
```

<p>Describes one or more of the EBS snapshots available to you. Available snapshots include public snapshots available for any AWS account to launch, private snapshots that you own, and private snapshots owned by another AWS account but for which you've been given explicit create volume permissions.</p> <p>The create volume permissions fall into the following categories:</p> <ul> <li> <p> <i>public</i>: The owner of the snapshot granted create volume permissions for the snapshot to the <code>all</code> group. All AWS accounts have create volume permissions for these snapshots.</p> </li> <li> <p> <i>explicit</i>: The owner of the snapshot granted create volume permissions to a specific AWS account.</p> </li> <li> <p> <i>implicit</i>: An AWS account has implicit create volume permissions for all snapshots it owns.</p> </li> </ul> <p>The list of snapshots returned can be modified by specifying snapshot IDs, snapshot owners, or AWS accounts with create volume permissions. If no options are specified, Amazon EC2 returns all snapshots for which you have create volume permissions.</p> <p>If you specify one or more snapshot IDs, only snapshots that have the specified IDs are returned. If you specify an invalid snapshot ID, an error is returned. If you specify a snapshot ID for which you do not have access, it is not included in the returned results.</p> <p>If you specify one or more snapshot owners using the <code>OwnerIds</code> option, only snapshots from the specified owners and for which you have access are returned. The results can include the AWS account IDs of the specified owners, <code>amazon</code> for snapshots owned by Amazon, or <code>self</code> for snapshots that you own.</p> <p>If you specify a list of restorable users, only snapshots with create snapshot permissions for those users are returned. You can specify AWS account IDs (if you own the snapshots), <code>self</code> for snapshots for which you own or have explicit permissions, or <code>all</code> for public snapshots.</p> <p>If you are describing a long list of snapshots, you can paginate the output to make the list more manageable. The <code>MaxResults</code> parameter sets the maximum number of results returned in a single page. If the list of results exceeds your <code>MaxResults</code> value, then that number of results is returned along with a <code>NextToken</code> value that can be passed to a subsequent <code>DescribeSnapshots</code> request to retrieve the remaining results.</p> <p>For more information about EBS snapshots, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSSnapshots.html">Amazon EBS Snapshots</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeSpotDatafeedSubscription`

``` purescript
describeSpotDatafeedSubscription :: forall eff. DescribeSpotDatafeedSubscriptionRequest -> Aff (err :: RequestError | eff) DescribeSpotDatafeedSubscriptionResult
```

<p>Describes the data feed for Spot Instances. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html">Spot Instance Data Feed</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeSpotFleetInstances`

``` purescript
describeSpotFleetInstances :: forall eff. DescribeSpotFleetInstancesRequest -> Aff (err :: RequestError | eff) DescribeSpotFleetInstancesResponse
```

<p>Describes the running instances for the specified Spot Fleet.</p>

#### `describeSpotFleetRequestHistory`

``` purescript
describeSpotFleetRequestHistory :: forall eff. DescribeSpotFleetRequestHistoryRequest -> Aff (err :: RequestError | eff) DescribeSpotFleetRequestHistoryResponse
```

<p>Describes the events for the specified Spot Fleet request during the specified time.</p> <p>Spot Fleet events are delayed by up to 30 seconds before they can be described. This ensures that you can query by the last evaluated time and not miss a recorded event.</p>

#### `describeSpotFleetRequests`

``` purescript
describeSpotFleetRequests :: forall eff. DescribeSpotFleetRequestsRequest -> Aff (err :: RequestError | eff) DescribeSpotFleetRequestsResponse
```

<p>Describes your Spot Fleet requests.</p> <p>Spot Fleet requests are deleted 48 hours after they are canceled and their instances are terminated.</p>

#### `describeSpotInstanceRequests`

``` purescript
describeSpotInstanceRequests :: forall eff. DescribeSpotInstanceRequestsRequest -> Aff (err :: RequestError | eff) DescribeSpotInstanceRequestsResult
```

<p>Describes the Spot Instance requests that belong to your account. Spot Instances are instances that Amazon EC2 launches when the Spot price that you specify exceeds the current Spot price. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html">Spot Instance Requests</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>You can use <code>DescribeSpotInstanceRequests</code> to find a running Spot Instance by examining the response. If the status of the Spot Instance is <code>fulfilled</code>, the instance ID appears in the response and contains the identifier of the instance. Alternatively, you can use <a>DescribeInstances</a> with a filter to look for instances where the instance lifecycle is <code>spot</code>.</p> <p>Spot Instance requests are deleted 4 hours after they are canceled and their instances are terminated.</p>

#### `describeSpotPriceHistory`

``` purescript
describeSpotPriceHistory :: forall eff. DescribeSpotPriceHistoryRequest -> Aff (err :: RequestError | eff) DescribeSpotPriceHistoryResult
```

<p>Describes the Spot price history. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-spot-instances-history.html">Spot Instance Pricing History</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>When you specify a start and end time, this operation returns the prices of the instance types within the time range that you specified and the time when the price changed. The price is valid within the time period that you specified; the response merely indicates the last time that the price changed.</p>

#### `describeStaleSecurityGroups`

``` purescript
describeStaleSecurityGroups :: forall eff. DescribeStaleSecurityGroupsRequest -> Aff (err :: RequestError | eff) DescribeStaleSecurityGroupsResult
```

<p>[EC2-VPC only] Describes the stale security group rules for security groups in a specified VPC. Rules are stale when they reference a deleted security group in a peer VPC, or a security group in a peer VPC for which the VPC peering connection has been deleted.</p>

#### `describeSubnets`

``` purescript
describeSubnets :: forall eff. DescribeSubnetsRequest -> Aff (err :: RequestError | eff) DescribeSubnetsResult
```

<p>Describes one or more of your subnets.</p> <p>For more information about subnets, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html">Your VPC and Subnets</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `describeTags`

``` purescript
describeTags :: forall eff. DescribeTagsRequest -> Aff (err :: RequestError | eff) DescribeTagsResult
```

<p>Describes one or more of the tags for your EC2 resources.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html">Tagging Your Resources</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeVolumeAttribute`

``` purescript
describeVolumeAttribute :: forall eff. DescribeVolumeAttributeRequest -> Aff (err :: RequestError | eff) DescribeVolumeAttributeResult
```

<p>Describes the specified attribute of the specified volume. You can specify only one attribute at a time.</p> <p>For more information about EBS volumes, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html">Amazon EBS Volumes</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeVolumeStatus`

``` purescript
describeVolumeStatus :: forall eff. DescribeVolumeStatusRequest -> Aff (err :: RequestError | eff) DescribeVolumeStatusResult
```

<p>Describes the status of the specified volumes. Volume status provides the result of the checks performed on your volumes to determine events that can impair the performance of your volumes. The performance of a volume can be affected if an issue occurs on the volume's underlying host. If the volume's underlying host experiences a power outage or system issue, after the system is restored, there could be data inconsistencies on the volume. Volume events notify you if this occurs. Volume actions notify you if any action needs to be taken in response to the event.</p> <p>The <code>DescribeVolumeStatus</code> operation provides the following information about the specified volumes:</p> <p> <i>Status</i>: Reflects the current status of the volume. The possible values are <code>ok</code>, <code>impaired</code> , <code>warning</code>, or <code>insufficient-data</code>. If all checks pass, the overall status of the volume is <code>ok</code>. If the check fails, the overall status is <code>impaired</code>. If the status is <code>insufficient-data</code>, then the checks may still be taking place on your volume at the time. We recommend that you retry the request. For more information on volume status, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-volume-status.html">Monitoring the Status of Your Volumes</a>.</p> <p> <i>Events</i>: Reflect the cause of a volume status and may require you to take action. For example, if your volume returns an <code>impaired</code> status, then the volume event might be <code>potential-data-inconsistency</code>. This means that your volume has been affected by an issue with the underlying host, has all I/O operations disabled, and may have inconsistent data.</p> <p> <i>Actions</i>: Reflect the actions you may have to take in response to an event. For example, if the status of the volume is <code>impaired</code> and the volume event shows <code>potential-data-inconsistency</code>, then the action shows <code>enable-volume-io</code>. This means that you may want to enable the I/O operations for the volume by calling the <a>EnableVolumeIO</a> action and then check the volume for data consistency.</p> <note> <p>Volume status is based on the volume status checks, and does not reflect the volume state. Therefore, volume status does not indicate volumes in the <code>error</code> state (for example, when a volume is incapable of accepting I/O.)</p> </note>

#### `describeVolumes`

``` purescript
describeVolumes :: forall eff. DescribeVolumesRequest -> Aff (err :: RequestError | eff) DescribeVolumesResult
```

<p>Describes the specified EBS volumes.</p> <p>If you are describing a long list of volumes, you can paginate the output to make the list more manageable. The <code>MaxResults</code> parameter sets the maximum number of results returned in a single page. If the list of results exceeds your <code>MaxResults</code> value, then that number of results is returned along with a <code>NextToken</code> value that can be passed to a subsequent <code>DescribeVolumes</code> request to retrieve the remaining results.</p> <p>For more information about EBS volumes, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html">Amazon EBS Volumes</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeVolumesModifications`

``` purescript
describeVolumesModifications :: forall eff. DescribeVolumesModificationsRequest -> Aff (err :: RequestError | eff) DescribeVolumesModificationsResult
```

<p>Reports the current modification status of EBS volumes.</p> <p>Current-generation EBS volumes support modification of attributes including type, size, and (for <code>io1</code> volumes) IOPS provisioning while either attached to or detached from an instance. Following an action from the API or the console to modify a volume, the status of the modification may be <code>modifying</code>, <code>optimizing</code>, <code>completed</code>, or <code>failed</code>. If a volume has never been modified, then certain elements of the returned <code>VolumeModification</code> objects are null. </p> <p> You can also use CloudWatch Events to check the status of a modification to an EBS volume. For information about CloudWatch Events, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/">Amazon CloudWatch Events User Guide</a>. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#monitoring_mods">Monitoring Volume Modifications"</a>. </p>

#### `describeVpcAttribute`

``` purescript
describeVpcAttribute :: forall eff. DescribeVpcAttributeRequest -> Aff (err :: RequestError | eff) DescribeVpcAttributeResult
```

<p>Describes the specified attribute of the specified VPC. You can specify only one attribute at a time.</p>

#### `describeVpcClassicLink`

``` purescript
describeVpcClassicLink :: forall eff. DescribeVpcClassicLinkRequest -> Aff (err :: RequestError | eff) DescribeVpcClassicLinkResult
```

<p>Describes the ClassicLink status of one or more VPCs.</p>

#### `describeVpcClassicLinkDnsSupport`

``` purescript
describeVpcClassicLinkDnsSupport :: forall eff. DescribeVpcClassicLinkDnsSupportRequest -> Aff (err :: RequestError | eff) DescribeVpcClassicLinkDnsSupportResult
```

<p>Describes the ClassicLink DNS support status of one or more VPCs. If enabled, the DNS hostname of a linked EC2-Classic instance resolves to its private IP address when addressed from an instance in the VPC to which it's linked. Similarly, the DNS hostname of an instance in a VPC resolves to its private IP address when addressed from a linked EC2-Classic instance. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html">ClassicLink</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `describeVpcEndpointConnectionNotifications`

``` purescript
describeVpcEndpointConnectionNotifications :: forall eff. DescribeVpcEndpointConnectionNotificationsRequest -> Aff (err :: RequestError | eff) DescribeVpcEndpointConnectionNotificationsResult
```

<p>Describes the connection notifications for VPC endpoints and VPC endpoint services.</p>

#### `describeVpcEndpointConnections`

``` purescript
describeVpcEndpointConnections :: forall eff. DescribeVpcEndpointConnectionsRequest -> Aff (err :: RequestError | eff) DescribeVpcEndpointConnectionsResult
```

<p>Describes the VPC endpoint connections to your VPC endpoint services, including any endpoints that are pending your acceptance.</p>

#### `describeVpcEndpointServiceConfigurations`

``` purescript
describeVpcEndpointServiceConfigurations :: forall eff. DescribeVpcEndpointServiceConfigurationsRequest -> Aff (err :: RequestError | eff) DescribeVpcEndpointServiceConfigurationsResult
```

<p>Describes the VPC endpoint service configurations in your account (your services).</p>

#### `describeVpcEndpointServicePermissions`

``` purescript
describeVpcEndpointServicePermissions :: forall eff. DescribeVpcEndpointServicePermissionsRequest -> Aff (err :: RequestError | eff) DescribeVpcEndpointServicePermissionsResult
```

<p>Describes the principals (service consumers) that are permitted to discover your VPC endpoint service.</p>

#### `describeVpcEndpointServices`

``` purescript
describeVpcEndpointServices :: forall eff. DescribeVpcEndpointServicesRequest -> Aff (err :: RequestError | eff) DescribeVpcEndpointServicesResult
```

<p>Describes available services to which you can create a VPC endpoint.</p>

#### `describeVpcEndpoints`

``` purescript
describeVpcEndpoints :: forall eff. DescribeVpcEndpointsRequest -> Aff (err :: RequestError | eff) DescribeVpcEndpointsResult
```

<p>Describes one or more of your VPC endpoints.</p>

#### `describeVpcPeeringConnections`

``` purescript
describeVpcPeeringConnections :: forall eff. DescribeVpcPeeringConnectionsRequest -> Aff (err :: RequestError | eff) DescribeVpcPeeringConnectionsResult
```

<p>Describes one or more of your VPC peering connections.</p>

#### `describeVpcs`

``` purescript
describeVpcs :: forall eff. DescribeVpcsRequest -> Aff (err :: RequestError | eff) DescribeVpcsResult
```

<p>Describes one or more of your VPCs.</p>

#### `describeVpnConnections`

``` purescript
describeVpnConnections :: forall eff. DescribeVpnConnectionsRequest -> Aff (err :: RequestError | eff) DescribeVpnConnectionsResult
```

<p>Describes one or more of your VPN connections.</p> <p>For more information about VPN connections, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html">AWS Managed VPN Connections</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `describeVpnGateways`

``` purescript
describeVpnGateways :: forall eff. DescribeVpnGatewaysRequest -> Aff (err :: RequestError | eff) DescribeVpnGatewaysResult
```

<p>Describes one or more of your virtual private gateways.</p> <p>For more information about virtual private gateways, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html">AWS Managed VPN Connections</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `detachClassicLinkVpc`

``` purescript
detachClassicLinkVpc :: forall eff. DetachClassicLinkVpcRequest -> Aff (err :: RequestError | eff) DetachClassicLinkVpcResult
```

<p>Unlinks (detaches) a linked EC2-Classic instance from a VPC. After the instance has been unlinked, the VPC security groups are no longer associated with it. An instance is automatically unlinked from a VPC when it's stopped.</p>

#### `detachInternetGateway`

``` purescript
detachInternetGateway :: forall eff. DetachInternetGatewayRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Detaches an Internet gateway from a VPC, disabling connectivity between the Internet and the VPC. The VPC must not contain any running instances with Elastic IP addresses or public IPv4 addresses.</p>

#### `detachNetworkInterface`

``` purescript
detachNetworkInterface :: forall eff. DetachNetworkInterfaceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Detaches a network interface from an instance.</p>

#### `detachVolume`

``` purescript
detachVolume :: forall eff. DetachVolumeRequest -> Aff (err :: RequestError | eff) VolumeAttachment
```

<p>Detaches an EBS volume from an instance. Make sure to unmount any file systems on the device within your operating system before detaching the volume. Failure to do so can result in the volume becoming stuck in the <code>busy</code> state while detaching. If this happens, detachment can be delayed indefinitely until you unmount the volume, force detachment, reboot the instance, or all three. If an EBS volume is the root device of an instance, it can't be detached while the instance is running. To detach the root volume, stop the instance first.</p> <p>When a volume with an AWS Marketplace product code is detached from an instance, the product code is no longer associated with the instance.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-detaching-volume.html">Detaching an Amazon EBS Volume</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `detachVpnGateway`

``` purescript
detachVpnGateway :: forall eff. DetachVpnGatewayRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Detaches a virtual private gateway from a VPC. You do this if you're planning to turn off the VPC and not use it anymore. You can confirm a virtual private gateway has been completely detached from a VPC by describing the virtual private gateway (any attachments to the virtual private gateway are also described).</p> <p>You must wait for the attachment's state to switch to <code>detached</code> before you can delete the VPC or attach a different VPC to the virtual private gateway.</p>

#### `disableVgwRoutePropagation`

``` purescript
disableVgwRoutePropagation :: forall eff. DisableVgwRoutePropagationRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Disables a virtual private gateway (VGW) from propagating routes to a specified route table of a VPC.</p>

#### `disableVpcClassicLink`

``` purescript
disableVpcClassicLink :: forall eff. DisableVpcClassicLinkRequest -> Aff (err :: RequestError | eff) DisableVpcClassicLinkResult
```

<p>Disables ClassicLink for a VPC. You cannot disable ClassicLink for a VPC that has EC2-Classic instances linked to it.</p>

#### `disableVpcClassicLinkDnsSupport`

``` purescript
disableVpcClassicLinkDnsSupport :: forall eff. DisableVpcClassicLinkDnsSupportRequest -> Aff (err :: RequestError | eff) DisableVpcClassicLinkDnsSupportResult
```

<p>Disables ClassicLink DNS support for a VPC. If disabled, DNS hostnames resolve to public IP addresses when addressed between a linked EC2-Classic instance and instances in the VPC to which it's linked. For more information about ClassicLink, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html">ClassicLink</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `disassociateAddress`

``` purescript
disassociateAddress :: forall eff. DisassociateAddressRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Disassociates an Elastic IP address from the instance or network interface it's associated with.</p> <p>An Elastic IP address is for use in either the EC2-Classic platform or in a VPC. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html">Elastic IP Addresses</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>This is an idempotent operation. If you perform the operation more than once, Amazon EC2 doesn't return an error.</p>

#### `disassociateIamInstanceProfile`

``` purescript
disassociateIamInstanceProfile :: forall eff. DisassociateIamInstanceProfileRequest -> Aff (err :: RequestError | eff) DisassociateIamInstanceProfileResult
```

<p>Disassociates an IAM instance profile from a running or stopped instance.</p> <p>Use <a>DescribeIamInstanceProfileAssociations</a> to get the association ID.</p>

#### `disassociateRouteTable`

``` purescript
disassociateRouteTable :: forall eff. DisassociateRouteTableRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Disassociates a subnet from a route table.</p> <p>After you perform this action, the subnet no longer uses the routes in the route table. Instead, it uses the routes in the VPC's main route table. For more information about route tables, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html">Route Tables</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `disassociateSubnetCidrBlock`

``` purescript
disassociateSubnetCidrBlock :: forall eff. DisassociateSubnetCidrBlockRequest -> Aff (err :: RequestError | eff) DisassociateSubnetCidrBlockResult
```

<p>Disassociates a CIDR block from a subnet. Currently, you can disassociate an IPv6 CIDR block only. You must detach or delete all gateways and resources that are associated with the CIDR block before you can disassociate it. </p>

#### `disassociateVpcCidrBlock`

``` purescript
disassociateVpcCidrBlock :: forall eff. DisassociateVpcCidrBlockRequest -> Aff (err :: RequestError | eff) DisassociateVpcCidrBlockResult
```

<p>Disassociates a CIDR block from a VPC. To disassociate the CIDR block, you must specify its association ID. You can get the association ID by using <a>DescribeVpcs</a>. You must detach or delete all gateways and resources that are associated with the CIDR block before you can disassociate it. </p> <p>You cannot disassociate the CIDR block with which you originally created the VPC (the primary CIDR block).</p>

#### `enableVgwRoutePropagation`

``` purescript
enableVgwRoutePropagation :: forall eff. EnableVgwRoutePropagationRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Enables a virtual private gateway (VGW) to propagate routes to the specified route table of a VPC.</p>

#### `enableVolumeIO`

``` purescript
enableVolumeIO :: forall eff. EnableVolumeIORequest -> Aff (err :: RequestError | eff) Unit
```

<p>Enables I/O operations for a volume that had I/O operations disabled because the data on the volume was potentially inconsistent.</p>

#### `enableVpcClassicLink`

``` purescript
enableVpcClassicLink :: forall eff. EnableVpcClassicLinkRequest -> Aff (err :: RequestError | eff) EnableVpcClassicLinkResult
```

<p>Enables a VPC for ClassicLink. You can then link EC2-Classic instances to your ClassicLink-enabled VPC to allow communication over private IP addresses. You cannot enable your VPC for ClassicLink if any of your VPC's route tables have existing routes for address ranges within the <code>10.0.0.0/8</code> IP address range, excluding local routes for VPCs in the <code>10.0.0.0/16</code> and <code>10.1.0.0/16</code> IP address ranges. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html">ClassicLink</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `enableVpcClassicLinkDnsSupport`

``` purescript
enableVpcClassicLinkDnsSupport :: forall eff. EnableVpcClassicLinkDnsSupportRequest -> Aff (err :: RequestError | eff) EnableVpcClassicLinkDnsSupportResult
```

<p>Enables a VPC to support DNS hostname resolution for ClassicLink. If enabled, the DNS hostname of a linked EC2-Classic instance resolves to its private IP address when addressed from an instance in the VPC to which it's linked. Similarly, the DNS hostname of an instance in a VPC resolves to its private IP address when addressed from a linked EC2-Classic instance. For more information about ClassicLink, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html">ClassicLink</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `getConsoleOutput`

``` purescript
getConsoleOutput :: forall eff. GetConsoleOutputRequest -> Aff (err :: RequestError | eff) GetConsoleOutputResult
```

<p>Gets the console output for the specified instance.</p> <p>Instances do not have a physical monitor through which you can view their console output. They also lack physical controls that allow you to power up, reboot, or shut them down. To allow these actions, we provide them through the Amazon EC2 API and command line interface.</p> <p>Instance console output is buffered and posted shortly after instance boot, reboot, and termination. Amazon EC2 preserves the most recent 64 KB output, which is available for at least one hour after the most recent post.</p> <p>For Linux instances, the instance console output displays the exact console output that would normally be displayed on a physical monitor attached to a computer. This output is buffered because the instance produces it and then posts it to a store where the instance's owner can retrieve it.</p> <p>For Windows instances, the instance console output includes output from the EC2Config service.</p>

#### `getConsoleScreenshot`

``` purescript
getConsoleScreenshot :: forall eff. GetConsoleScreenshotRequest -> Aff (err :: RequestError | eff) GetConsoleScreenshotResult
```

<p>Retrieve a JPG-format screenshot of a running instance to help with troubleshooting.</p> <p>The returned content is Base64-encoded.</p>

#### `getHostReservationPurchasePreview`

``` purescript
getHostReservationPurchasePreview :: forall eff. GetHostReservationPurchasePreviewRequest -> Aff (err :: RequestError | eff) GetHostReservationPurchasePreviewResult
```

<p>Preview a reservation purchase with configurations that match those of your Dedicated Host. You must have active Dedicated Hosts in your account before you purchase a reservation.</p> <p>This is a preview of the <a>PurchaseHostReservation</a> action and does not result in the offering being purchased.</p>

#### `getLaunchTemplateData`

``` purescript
getLaunchTemplateData :: forall eff. GetLaunchTemplateDataRequest -> Aff (err :: RequestError | eff) GetLaunchTemplateDataResult
```

<p>Retrieves the configuration data of the specified instance. You can use this data to create a launch template.</p>

#### `getPasswordData`

``` purescript
getPasswordData :: forall eff. GetPasswordDataRequest -> Aff (err :: RequestError | eff) GetPasswordDataResult
```

<p>Retrieves the encrypted administrator password for a running Windows instance.</p> <p>The Windows password is generated at boot by the <code>EC2Config</code> service or <code>EC2Launch</code> scripts (Windows Server 2016 and later). This usually only happens the first time an instance is launched. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/UsingConfig_WinAMI.html">EC2Config</a> and <a href="http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2launch.html">EC2Launch</a> in the Amazon Elastic Compute Cloud User Guide.</p> <p>For the <code>EC2Config</code> service, the password is not generated for rebundled AMIs unless <code>Ec2SetPassword</code> is enabled before bundling.</p> <p>The password is encrypted using the key pair that you specified when you launched the instance. You must provide the corresponding key pair file.</p> <p>When you launch an instance, password generation and encryption may take a few minutes. If you try to retrieve the password before it's available, the output returns an empty string. We recommend that you wait up to 15 minutes after launching an instance before trying to retrieve the generated password.</p>

#### `getReservedInstancesExchangeQuote`

``` purescript
getReservedInstancesExchangeQuote :: forall eff. GetReservedInstancesExchangeQuoteRequest -> Aff (err :: RequestError | eff) GetReservedInstancesExchangeQuoteResult
```

<p>Returns a quote and exchange information for exchanging one or more specified Convertible Reserved Instances for a new Convertible Reserved Instance. If the exchange cannot be performed, the reason is returned in the response. Use <a>AcceptReservedInstancesExchangeQuote</a> to perform the exchange.</p>

#### `importImage`

``` purescript
importImage :: forall eff. ImportImageRequest -> Aff (err :: RequestError | eff) ImportImageResult
```

<p>Import single or multi-volume disk images or EBS snapshots into an Amazon Machine Image (AMI). For more information, see <a href="http://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html">Importing a VM as an Image Using VM Import/Export</a> in the <i>VM Import/Export User Guide</i>.</p>

#### `importInstance`

``` purescript
importInstance :: forall eff. ImportInstanceRequest -> Aff (err :: RequestError | eff) ImportInstanceResult
```

<p>Creates an import instance task using metadata from the specified disk image. <code>ImportInstance</code> only supports single-volume VMs. To import multi-volume VMs, use <a>ImportImage</a>. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/ec2-cli-vmimport-export.html">Importing a Virtual Machine Using the Amazon EC2 CLI</a>.</p> <p>For information about the import manifest referenced by this API action, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html">VM Import Manifest</a>.</p>

#### `importKeyPair`

``` purescript
importKeyPair :: forall eff. ImportKeyPairRequest -> Aff (err :: RequestError | eff) ImportKeyPairResult
```

<p>Imports the public key from an RSA key pair that you created with a third-party tool. Compare this with <a>CreateKeyPair</a>, in which AWS creates the key pair and gives the keys to you (AWS keeps a copy of the public key). With ImportKeyPair, you create the key pair and give AWS just the public key. The private key is never transferred between you and AWS.</p> <p>For more information about key pairs, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html">Key Pairs</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `importSnapshot`

``` purescript
importSnapshot :: forall eff. ImportSnapshotRequest -> Aff (err :: RequestError | eff) ImportSnapshotResult
```

<p>Imports a disk into an EBS snapshot.</p>

#### `importVolume`

``` purescript
importVolume :: forall eff. ImportVolumeRequest -> Aff (err :: RequestError | eff) ImportVolumeResult
```

<p>Creates an import volume task using metadata from the specified disk image.For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/importing-your-volumes-into-amazon-ebs.html">Importing Disks to Amazon EBS</a>.</p> <p>For information about the import manifest referenced by this API action, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html">VM Import Manifest</a>.</p>

#### `modifyFpgaImageAttribute`

``` purescript
modifyFpgaImageAttribute :: forall eff. ModifyFpgaImageAttributeRequest -> Aff (err :: RequestError | eff) ModifyFpgaImageAttributeResult
```

<p>Modifies the specified attribute of the specified Amazon FPGA Image (AFI).</p>

#### `modifyHosts`

``` purescript
modifyHosts :: forall eff. ModifyHostsRequest -> Aff (err :: RequestError | eff) ModifyHostsResult
```

<p>Modify the auto-placement setting of a Dedicated Host. When auto-placement is enabled, AWS will place instances that you launch with a tenancy of <code>host</code>, but without targeting a specific host ID, onto any available Dedicated Host in your account which has auto-placement enabled. When auto-placement is disabled, you need to provide a host ID if you want the instance to launch onto a specific host. If no host ID is provided, the instance will be launched onto a suitable host which has auto-placement enabled.</p>

#### `modifyIdFormat`

``` purescript
modifyIdFormat :: forall eff. ModifyIdFormatRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Modifies the ID format for the specified resource on a per-region basis. You can specify that resources should receive longer IDs (17-character IDs) when they are created.</p> <p>This request can only be used to modify longer ID settings for resource types that are within the opt-in period. Resources currently in their opt-in period include: <code>bundle</code> | <code>conversion-task</code> | <code>dhcp-options</code> | <code>elastic-ip-allocation</code> | <code>elastic-ip-association</code> | <code>export-task</code> | <code>flow-log</code> | <code>image</code> | <code>import-task</code> | <code>internet-gateway</code> | <code>network-acl</code> | <code>network-acl-association</code> | <code>network-interface</code> | <code>network-interface-attachment</code> | <code>prefix-list</code> | <code>route-table</code> | <code>route-table-association</code> | <code>security-group</code> | <code>subnet</code> | <code>subnet-cidr-block-association</code> | <code>vpc</code> | <code>vpc-cidr-block-association</code> | <code>vpc-peering-connection</code>.</p> <p>This setting applies to the IAM user who makes the request; it does not apply to the entire AWS account. By default, an IAM user defaults to the same settings as the root user. If you're using this action as the root user, then these settings apply to the entire account, unless an IAM user explicitly overrides these settings for themselves. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html">Resource IDs</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>. </p> <p>Resources created with longer IDs are visible to all IAM roles and users, regardless of these settings and provided that they have permission to use the relevant <code>Describe</code> command for the resource type.</p>

#### `modifyIdentityIdFormat`

``` purescript
modifyIdentityIdFormat :: forall eff. ModifyIdentityIdFormatRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Modifies the ID format of a resource for a specified IAM user, IAM role, or the root user for an account; or all IAM users, IAM roles, and the root user for an account. You can specify that resources should receive longer IDs (17-character IDs) when they are created. </p> <p>This request can only be used to modify longer ID settings for resource types that are within the opt-in period. Resources currently in their opt-in period include: <code>bundle</code> | <code>conversion-task</code> | <code>dhcp-options</code> | <code>elastic-ip-allocation</code> | <code>elastic-ip-association</code> | <code>export-task</code> | <code>flow-log</code> | <code>image</code> | <code>import-task</code> | <code>internet-gateway</code> | <code>network-acl</code> | <code>network-acl-association</code> | <code>network-interface</code> | <code>network-interface-attachment</code> | <code>prefix-list</code> | <code>route-table</code> | <code>route-table-association</code> | <code>security-group</code> | <code>subnet</code> | <code>subnet-cidr-block-association</code> | <code>vpc</code> | <code>vpc-cidr-block-association</code> | <code>vpc-peering-connection</code>.. </p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html">Resource IDs</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>. </p> <p>This setting applies to the principal specified in the request; it does not apply to the principal that makes the request. </p> <p>Resources created with longer IDs are visible to all IAM roles and users, regardless of these settings and provided that they have permission to use the relevant <code>Describe</code> command for the resource type.</p>

#### `modifyImageAttribute`

``` purescript
modifyImageAttribute :: forall eff. ModifyImageAttributeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Modifies the specified attribute of the specified AMI. You can specify only one attribute at a time. You can use the <code>Attribute</code> parameter to specify the attribute or one of the following parameters: <code>Description</code>, <code>LaunchPermission</code>, or <code>ProductCode</code>.</p> <p>AWS Marketplace product codes cannot be modified. Images with an AWS Marketplace product code cannot be made public.</p> <p>To enable the SriovNetSupport enhanced networking attribute of an image, enable SriovNetSupport on an instance and create an AMI from the instance.</p>

#### `modifyInstanceAttribute`

``` purescript
modifyInstanceAttribute :: forall eff. ModifyInstanceAttributeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Modifies the specified attribute of the specified instance. You can specify only one attribute at a time.</p> <p>To modify some attributes, the instance must be stopped. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_ChangingAttributesWhileInstanceStopped.html">Modifying Attributes of a Stopped Instance</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `modifyInstanceCreditSpecification`

``` purescript
modifyInstanceCreditSpecification :: forall eff. ModifyInstanceCreditSpecificationRequest -> Aff (err :: RequestError | eff) ModifyInstanceCreditSpecificationResult
```

<p>Modifies the credit option for CPU usage on a running or stopped T2 instance. The credit options are <code>standard</code> and <code>unlimited</code>.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/t2-instances.html">T2 Instances</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `modifyInstancePlacement`

``` purescript
modifyInstancePlacement :: forall eff. ModifyInstancePlacementRequest -> Aff (err :: RequestError | eff) ModifyInstancePlacementResult
```

<p>Set the instance affinity value for a specific stopped instance and modify the instance tenancy setting.</p> <p>Instance affinity is disabled by default. When instance affinity is <code>host</code> and it is not associated with a specific Dedicated Host, the next time it is launched it will automatically be associated with the host it lands on. This relationship will persist if the instance is stopped/started, or rebooted.</p> <p>You can modify the host ID associated with a stopped instance. If a stopped instance has a new host ID association, the instance will target that host when restarted.</p> <p>You can modify the tenancy of a stopped instance with a tenancy of <code>host</code> or <code>dedicated</code>.</p> <p>Affinity, hostID, and tenancy are not required parameters, but at least one of them must be specified in the request. Affinity and tenancy can be modified in the same request, but tenancy can only be modified on instances that are stopped.</p>

#### `modifyLaunchTemplate`

``` purescript
modifyLaunchTemplate :: forall eff. ModifyLaunchTemplateRequest -> Aff (err :: RequestError | eff) ModifyLaunchTemplateResult
```

<p>Modifies a launch template. You can specify which version of the launch template to set as the default version. When launching an instance, the default version applies when a launch template version is not specified.</p>

#### `modifyNetworkInterfaceAttribute`

``` purescript
modifyNetworkInterfaceAttribute :: forall eff. ModifyNetworkInterfaceAttributeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Modifies the specified network interface attribute. You can specify only one attribute at a time.</p>

#### `modifyReservedInstances`

``` purescript
modifyReservedInstances :: forall eff. ModifyReservedInstancesRequest -> Aff (err :: RequestError | eff) ModifyReservedInstancesResult
```

<p>Modifies the Availability Zone, instance count, instance type, or network platform (EC2-Classic or EC2-VPC) of your Reserved Instances. The Reserved Instances to be modified must be identical, except for Availability Zone, network platform, and instance type.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html">Modifying Reserved Instances</a> in the Amazon Elastic Compute Cloud User Guide.</p>

#### `modifySnapshotAttribute`

``` purescript
modifySnapshotAttribute :: forall eff. ModifySnapshotAttributeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds or removes permission settings for the specified snapshot. You may add or remove specified AWS account IDs from a snapshot's list of create volume permissions, but you cannot do both in a single API call. If you need to both add and remove account IDs for a snapshot, you must use multiple API calls.</p> <note> <p>Encrypted snapshots and snapshots with AWS Marketplace product codes cannot be made public. Snapshots encrypted with your default CMK cannot be shared with other accounts.</p> </note> <p>For more information on modifying snapshot permissions, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html">Sharing Snapshots</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `modifySpotFleetRequest`

``` purescript
modifySpotFleetRequest :: forall eff. ModifySpotFleetRequestRequest -> Aff (err :: RequestError | eff) ModifySpotFleetRequestResponse
```

<p>Modifies the specified Spot Fleet request.</p> <p>While the Spot Fleet request is being modified, it is in the <code>modifying</code> state.</p> <p>To scale up your Spot Fleet, increase its target capacity. The Spot Fleet launches the additional Spot Instances according to the allocation strategy for the Spot Fleet request. If the allocation strategy is <code>lowestPrice</code>, the Spot Fleet launches instances using the Spot pool with the lowest price. If the allocation strategy is <code>diversified</code>, the Spot Fleet distributes the instances across the Spot pools.</p> <p>To scale down your Spot Fleet, decrease its target capacity. First, the Spot Fleet cancels any open requests that exceed the new target capacity. You can request that the Spot Fleet terminate Spot Instances until the size of the fleet no longer exceeds the new target capacity. If the allocation strategy is <code>lowestPrice</code>, the Spot Fleet terminates the instances with the highest price per unit. If the allocation strategy is <code>diversified</code>, the Spot Fleet terminates instances across the Spot pools. Alternatively, you can request that the Spot Fleet keep the fleet at its current size, but not replace any Spot Instances that are interrupted or that you terminate manually.</p> <p>If you are finished with your Spot Fleet for now, but will use it again later, you can set the target capacity to 0.</p>

#### `modifySubnetAttribute`

``` purescript
modifySubnetAttribute :: forall eff. ModifySubnetAttributeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Modifies a subnet attribute. You can only modify one attribute at a time.</p>

#### `modifyVolume`

``` purescript
modifyVolume :: forall eff. ModifyVolumeRequest -> Aff (err :: RequestError | eff) ModifyVolumeResult
```

<p>You can modify several parameters of an existing EBS volume, including volume size, volume type, and IOPS capacity. If your EBS volume is attached to a current-generation EC2 instance type, you may be able to apply these changes without stopping the instance or detaching the volume from it. For more information about modifying an EBS volume running Linux, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html">Modifying the Size, IOPS, or Type of an EBS Volume on Linux</a>. For more information about modifying an EBS volume running Windows, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html">Modifying the Size, IOPS, or Type of an EBS Volume on Windows</a>. </p> <p> When you complete a resize operation on your volume, you need to extend the volume's file-system size to take advantage of the new storage capacity. For information about extending a Linux file system, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#recognize-expanded-volume-linux">Extending a Linux File System</a>. For information about extending a Windows file system, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html#recognize-expanded-volume-windows">Extending a Windows File System</a>. </p> <p> You can use CloudWatch Events to check the status of a modification to an EBS volume. For information about CloudWatch Events, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/">Amazon CloudWatch Events User Guide</a>. You can also track the status of a modification using the <a>DescribeVolumesModifications</a> API. For information about tracking status changes using either method, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#monitoring_mods">Monitoring Volume Modifications</a>. </p> <note> <p>With previous-generation instance types, resizing an EBS volume may require detaching and reattaching the volume or stopping and restarting the instance. For more information about modifying an EBS volume running Linux, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html">Modifying the Size, IOPS, or Type of an EBS Volume on Linux</a>. For more information about modifying an EBS volume running Windows, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html">Modifying the Size, IOPS, or Type of an EBS Volume on Windows</a>.</p> </note> <note> <p>If you reach the maximum volume modification rate per volume limit, you will need to wait at least six hours before applying further modifications to the affected EBS volume.</p> </note>

#### `modifyVolumeAttribute`

``` purescript
modifyVolumeAttribute :: forall eff. ModifyVolumeAttributeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Modifies a volume attribute.</p> <p>By default, all I/O operations for the volume are suspended when the data on the volume is determined to be potentially inconsistent, to prevent undetectable, latent data corruption. The I/O access to the volume can be resumed by first enabling I/O access and then checking the data consistency on your volume.</p> <p>You can change the default behavior to resume I/O operations. We recommend that you change this only for boot volumes or for volumes that are stateless or disposable.</p>

#### `modifyVpcAttribute`

``` purescript
modifyVpcAttribute :: forall eff. ModifyVpcAttributeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Modifies the specified attribute of the specified VPC.</p>

#### `modifyVpcEndpoint`

``` purescript
modifyVpcEndpoint :: forall eff. ModifyVpcEndpointRequest -> Aff (err :: RequestError | eff) ModifyVpcEndpointResult
```

<p>Modifies attributes of a specified VPC endpoint. The attributes that you can modify depend on the type of VPC endpoint (interface or gateway). For more information, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-endpoints.html">VPC Endpoints</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `modifyVpcEndpointConnectionNotification`

``` purescript
modifyVpcEndpointConnectionNotification :: forall eff. ModifyVpcEndpointConnectionNotificationRequest -> Aff (err :: RequestError | eff) ModifyVpcEndpointConnectionNotificationResult
```

<p>Modifies a connection notification for VPC endpoint or VPC endpoint service. You can change the SNS topic for the notification, or the events for which to be notified. </p>

#### `modifyVpcEndpointServiceConfiguration`

``` purescript
modifyVpcEndpointServiceConfiguration :: forall eff. ModifyVpcEndpointServiceConfigurationRequest -> Aff (err :: RequestError | eff) ModifyVpcEndpointServiceConfigurationResult
```

<p>Modifies the attributes of your VPC endpoint service configuration. You can change the Network Load Balancers for your service, and you can specify whether acceptance is required for requests to connect to your endpoint service through an interface VPC endpoint.</p>

#### `modifyVpcEndpointServicePermissions`

``` purescript
modifyVpcEndpointServicePermissions :: forall eff. ModifyVpcEndpointServicePermissionsRequest -> Aff (err :: RequestError | eff) ModifyVpcEndpointServicePermissionsResult
```

<p>Modifies the permissions for your <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/endpoint-service.html">VPC endpoint service</a>. You can add or remove permissions for service consumers (IAM users, IAM roles, and AWS accounts) to connect to your endpoint service.</p>

#### `modifyVpcPeeringConnectionOptions`

``` purescript
modifyVpcPeeringConnectionOptions :: forall eff. ModifyVpcPeeringConnectionOptionsRequest -> Aff (err :: RequestError | eff) ModifyVpcPeeringConnectionOptionsResult
```

<p>Modifies the VPC peering connection options on one side of a VPC peering connection. You can do the following:</p> <ul> <li> <p>Enable/disable communication over the peering connection between an EC2-Classic instance that's linked to your VPC (using ClassicLink) and instances in the peer VPC.</p> </li> <li> <p>Enable/disable communication over the peering connection between instances in your VPC and an EC2-Classic instance that's linked to the peer VPC.</p> </li> <li> <p>Enable/disable a local VPC to resolve public DNS hostnames to private IP addresses when queried from instances in the peer VPC.</p> </li> </ul> <p>If the peered VPCs are in different accounts, each owner must initiate a separate request to modify the peering connection options, depending on whether their VPC was the requester or accepter for the VPC peering connection. If the peered VPCs are in the same account, you can modify the requester and accepter options in the same request. To confirm which VPC is the accepter and requester for a VPC peering connection, use the <a>DescribeVpcPeeringConnections</a> command.</p>

#### `modifyVpcTenancy`

``` purescript
modifyVpcTenancy :: forall eff. ModifyVpcTenancyRequest -> Aff (err :: RequestError | eff) ModifyVpcTenancyResult
```

<p>Modifies the instance tenancy attribute of the specified VPC. You can change the instance tenancy attribute of a VPC to <code>default</code> only. You cannot change the instance tenancy attribute to <code>dedicated</code>.</p> <p>After you modify the tenancy of the VPC, any new instances that you launch into the VPC have a tenancy of <code>default</code>, unless you specify otherwise during launch. The tenancy of any existing instances in the VPC is not affected.</p> <p>For more information about Dedicated Instances, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-instance.html">Dedicated Instances</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `monitorInstances`

``` purescript
monitorInstances :: forall eff. MonitorInstancesRequest -> Aff (err :: RequestError | eff) MonitorInstancesResult
```

<p>Enables detailed monitoring for a running instance. Otherwise, basic monitoring is enabled. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html">Monitoring Your Instances and Volumes</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>To disable detailed monitoring, see .</p>

#### `moveAddressToVpc`

``` purescript
moveAddressToVpc :: forall eff. MoveAddressToVpcRequest -> Aff (err :: RequestError | eff) MoveAddressToVpcResult
```

<p>Moves an Elastic IP address from the EC2-Classic platform to the EC2-VPC platform. The Elastic IP address must be allocated to your account for more than 24 hours, and it must not be associated with an instance. After the Elastic IP address is moved, it is no longer available for use in the EC2-Classic platform, unless you move it back using the <a>RestoreAddressToClassic</a> request. You cannot move an Elastic IP address that was originally allocated for use in the EC2-VPC platform to the EC2-Classic platform. </p>

#### `purchaseHostReservation`

``` purescript
purchaseHostReservation :: forall eff. PurchaseHostReservationRequest -> Aff (err :: RequestError | eff) PurchaseHostReservationResult
```

<p>Purchase a reservation with configurations that match those of your Dedicated Host. You must have active Dedicated Hosts in your account before you purchase a reservation. This action results in the specified reservation being purchased and charged to your account.</p>

#### `purchaseReservedInstancesOffering`

``` purescript
purchaseReservedInstancesOffering :: forall eff. PurchaseReservedInstancesOfferingRequest -> Aff (err :: RequestError | eff) PurchaseReservedInstancesOfferingResult
```

<p>Purchases a Reserved Instance for use with your account. With Reserved Instances, you pay a lower hourly rate compared to On-Demand instance pricing.</p> <p>Use <a>DescribeReservedInstancesOfferings</a> to get a list of Reserved Instance offerings that match your specifications. After you've purchased a Reserved Instance, you can check for your new Reserved Instance with <a>DescribeReservedInstances</a>.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-on-demand-reserved-instances.html">Reserved Instances</a> and <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html">Reserved Instance Marketplace</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `purchaseScheduledInstances`

``` purescript
purchaseScheduledInstances :: forall eff. PurchaseScheduledInstancesRequest -> Aff (err :: RequestError | eff) PurchaseScheduledInstancesResult
```

<p>Purchases one or more Scheduled Instances with the specified schedule.</p> <p>Scheduled Instances enable you to purchase Amazon EC2 compute capacity by the hour for a one-year term. Before you can purchase a Scheduled Instance, you must call <a>DescribeScheduledInstanceAvailability</a> to check for available schedules and obtain a purchase token. After you purchase a Scheduled Instance, you must call <a>RunScheduledInstances</a> during each scheduled time period.</p> <p>After you purchase a Scheduled Instance, you can't cancel, modify, or resell your purchase.</p>

#### `rebootInstances`

``` purescript
rebootInstances :: forall eff. RebootInstancesRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Requests a reboot of one or more instances. This operation is asynchronous; it only queues a request to reboot the specified instances. The operation succeeds if the instances are valid and belong to you. Requests to reboot terminated instances are ignored.</p> <p>If an instance does not cleanly shut down within four minutes, Amazon EC2 performs a hard reboot.</p> <p>For more information about troubleshooting, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-console.html">Getting Console Output and Rebooting Instances</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `registerImage`

``` purescript
registerImage :: forall eff. RegisterImageRequest -> Aff (err :: RequestError | eff) RegisterImageResult
```

<p>Registers an AMI. When you're creating an AMI, this is the final step you must complete before you can launch an instance from the AMI. For more information about creating AMIs, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami.html">Creating Your Own AMIs</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <note> <p>For Amazon EBS-backed instances, <a>CreateImage</a> creates and registers the AMI in a single request, so you don't have to register the AMI yourself.</p> </note> <p>You can also use <code>RegisterImage</code> to create an Amazon EBS-backed Linux AMI from a snapshot of a root device volume. You specify the snapshot using the block device mapping. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-launch-snapshot.html">Launching a Linux Instance from a Backup</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>You can't register an image where a secondary (non-root) snapshot has AWS Marketplace product codes.</p> <p>Some Linux distributions, such as Red Hat Enterprise Linux (RHEL) and SUSE Linux Enterprise Server (SLES), use the EC2 billing product code associated with an AMI to verify the subscription status for package updates. Creating an AMI from an EBS snapshot does not maintain this billing code, and subsequent instances launched from such an AMI will not be able to connect to package update infrastructure. To create an AMI that must retain billing codes, see <a>CreateImage</a>.</p> <p>If needed, you can deregister an AMI at any time. Any modifications you make to an AMI backed by an instance store volume invalidates its registration. If you make changes to an image, deregister the previous image and register the new image.</p>

#### `rejectVpcEndpointConnections`

``` purescript
rejectVpcEndpointConnections :: forall eff. RejectVpcEndpointConnectionsRequest -> Aff (err :: RequestError | eff) RejectVpcEndpointConnectionsResult
```

<p>Rejects one or more VPC endpoint connection requests to your VPC endpoint service.</p>

#### `rejectVpcPeeringConnection`

``` purescript
rejectVpcPeeringConnection :: forall eff. RejectVpcPeeringConnectionRequest -> Aff (err :: RequestError | eff) RejectVpcPeeringConnectionResult
```

<p>Rejects a VPC peering connection request. The VPC peering connection must be in the <code>pending-acceptance</code> state. Use the <a>DescribeVpcPeeringConnections</a> request to view your outstanding VPC peering connection requests. To delete an active VPC peering connection, or to delete a VPC peering connection request that you initiated, use <a>DeleteVpcPeeringConnection</a>.</p>

#### `releaseAddress`

``` purescript
releaseAddress :: forall eff. ReleaseAddressRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Releases the specified Elastic IP address.</p> <p>[EC2-Classic, default VPC] Releasing an Elastic IP address automatically disassociates it from any instance that it's associated with. To disassociate an Elastic IP address without releasing it, use <a>DisassociateAddress</a>.</p> <p>[Nondefault VPC] You must use <a>DisassociateAddress</a> to disassociate the Elastic IP address before you can release it. Otherwise, Amazon EC2 returns an error (<code>InvalidIPAddress.InUse</code>).</p> <p>After releasing an Elastic IP address, it is released to the IP address pool. Be sure to update your DNS records and any servers or devices that communicate with the address. If you attempt to release an Elastic IP address that you already released, you'll get an <code>AuthFailure</code> error if the address is already allocated to another AWS account.</p> <p>[EC2-VPC] After you release an Elastic IP address for use in a VPC, you might be able to recover it. For more information, see <a>AllocateAddress</a>.</p>

#### `releaseHosts`

``` purescript
releaseHosts :: forall eff. ReleaseHostsRequest -> Aff (err :: RequestError | eff) ReleaseHostsResult
```

<p>When you no longer want to use an On-Demand Dedicated Host it can be released. On-Demand billing is stopped and the host goes into <code>released</code> state. The host ID of Dedicated Hosts that have been released can no longer be specified in another request, e.g., ModifyHosts. You must stop or terminate all instances on a host before it can be released.</p> <p>When Dedicated Hosts are released, it make take some time for them to stop counting toward your limit and you may receive capacity errors when trying to allocate new Dedicated hosts. Try waiting a few minutes, and then try again.</p> <p>Released hosts will still appear in a <a>DescribeHosts</a> response.</p>

#### `replaceIamInstanceProfileAssociation`

``` purescript
replaceIamInstanceProfileAssociation :: forall eff. ReplaceIamInstanceProfileAssociationRequest -> Aff (err :: RequestError | eff) ReplaceIamInstanceProfileAssociationResult
```

<p>Replaces an IAM instance profile for the specified running instance. You can use this action to change the IAM instance profile that's associated with an instance without having to disassociate the existing IAM instance profile first.</p> <p>Use <a>DescribeIamInstanceProfileAssociations</a> to get the association ID.</p>

#### `replaceNetworkAclAssociation`

``` purescript
replaceNetworkAclAssociation :: forall eff. ReplaceNetworkAclAssociationRequest -> Aff (err :: RequestError | eff) ReplaceNetworkAclAssociationResult
```

<p>Changes which network ACL a subnet is associated with. By default when you create a subnet, it's automatically associated with the default network ACL. For more information about network ACLs, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html">Network ACLs</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p> <p>This is an idempotent operation.</p>

#### `replaceNetworkAclEntry`

``` purescript
replaceNetworkAclEntry :: forall eff. ReplaceNetworkAclEntryRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Replaces an entry (rule) in a network ACL. For more information about network ACLs, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html">Network ACLs</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `replaceRoute`

``` purescript
replaceRoute :: forall eff. ReplaceRouteRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Replaces an existing route within a route table in a VPC. You must provide only one of the following: Internet gateway or virtual private gateway, NAT instance, NAT gateway, VPC peering connection, network interface, or egress-only Internet gateway.</p> <p>For more information about route tables, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html">Route Tables</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `replaceRouteTableAssociation`

``` purescript
replaceRouteTableAssociation :: forall eff. ReplaceRouteTableAssociationRequest -> Aff (err :: RequestError | eff) ReplaceRouteTableAssociationResult
```

<p>Changes the route table associated with a given subnet in a VPC. After the operation completes, the subnet uses the routes in the new route table it's associated with. For more information about route tables, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html">Route Tables</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p> <p>You can also use ReplaceRouteTableAssociation to change which table is the main route table in the VPC. You just specify the main route table's association ID and the route table to be the new main route table.</p>

#### `reportInstanceStatus`

``` purescript
reportInstanceStatus :: forall eff. ReportInstanceStatusRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Submits feedback about the status of an instance. The instance must be in the <code>running</code> state. If your experience with the instance differs from the instance status returned by <a>DescribeInstanceStatus</a>, use <a>ReportInstanceStatus</a> to report your experience with the instance. Amazon EC2 collects this information to improve the accuracy of status checks.</p> <p>Use of this action does not change the value returned by <a>DescribeInstanceStatus</a>.</p>

#### `requestSpotFleet`

``` purescript
requestSpotFleet :: forall eff. RequestSpotFleetRequest -> Aff (err :: RequestError | eff) RequestSpotFleetResponse
```

<p>Creates a Spot Fleet request.</p> <p>You can submit a single request that includes multiple launch specifications that vary by instance type, AMI, Availability Zone, or subnet.</p> <p>By default, the Spot Fleet requests Spot Instances in the Spot pool where the price per unit is the lowest. Each launch specification can include its own instance weighting that reflects the value of the instance type to your application workload.</p> <p>Alternatively, you can specify that the Spot Fleet distribute the target capacity across the Spot pools included in its launch specifications. By ensuring that the Spot Instances in your Spot Fleet are in different Spot pools, you can improve the availability of your fleet.</p> <p>You can specify tags for the Spot Instances. You cannot tag other resource types in a Spot Fleet request; only the <code>instance</code> resource type is supported.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html">Spot Fleet Requests</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `requestSpotInstances`

``` purescript
requestSpotInstances :: forall eff. RequestSpotInstancesRequest -> Aff (err :: RequestError | eff) RequestSpotInstancesResult
```

<p>Creates a Spot Instance request. Spot Instances are instances that Amazon EC2 launches when the maximum price that you specify exceeds the current Spot price. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html">Spot Instance Requests</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `resetFpgaImageAttribute`

``` purescript
resetFpgaImageAttribute :: forall eff. ResetFpgaImageAttributeRequest -> Aff (err :: RequestError | eff) ResetFpgaImageAttributeResult
```

<p>Resets the specified attribute of the specified Amazon FPGA Image (AFI) to its default value. You can only reset the load permission attribute.</p>

#### `resetImageAttribute`

``` purescript
resetImageAttribute :: forall eff. ResetImageAttributeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Resets an attribute of an AMI to its default value.</p> <note> <p>The productCodes attribute can't be reset.</p> </note>

#### `resetInstanceAttribute`

``` purescript
resetInstanceAttribute :: forall eff. ResetInstanceAttributeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Resets an attribute of an instance to its default value. To reset the <code>kernel</code> or <code>ramdisk</code>, the instance must be in a stopped state. To reset the <code>sourceDestCheck</code>, the instance can be either running or stopped.</p> <p>The <code>sourceDestCheck</code> attribute controls whether source/destination checking is enabled. The default value is <code>true</code>, which means checking is enabled. This value must be <code>false</code> for a NAT instance to perform NAT. For more information, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html">NAT Instances</a> in the <i>Amazon Virtual Private Cloud User Guide</i>.</p>

#### `resetNetworkInterfaceAttribute`

``` purescript
resetNetworkInterfaceAttribute :: forall eff. ResetNetworkInterfaceAttributeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Resets a network interface attribute. You can specify only one attribute at a time.</p>

#### `resetSnapshotAttribute`

``` purescript
resetSnapshotAttribute :: forall eff. ResetSnapshotAttributeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Resets permission settings for the specified snapshot.</p> <p>For more information on modifying snapshot permissions, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html">Sharing Snapshots</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `restoreAddressToClassic`

``` purescript
restoreAddressToClassic :: forall eff. RestoreAddressToClassicRequest -> Aff (err :: RequestError | eff) RestoreAddressToClassicResult
```

<p>Restores an Elastic IP address that was previously moved to the EC2-VPC platform back to the EC2-Classic platform. You cannot move an Elastic IP address that was originally allocated for use in EC2-VPC. The Elastic IP address must not be associated with an instance or network interface.</p>

#### `revokeSecurityGroupEgress`

``` purescript
revokeSecurityGroupEgress :: forall eff. RevokeSecurityGroupEgressRequest -> Aff (err :: RequestError | eff) Unit
```

<p>[EC2-VPC only] Removes one or more egress rules from a security group for EC2-VPC. This action doesn't apply to security groups for use in EC2-Classic. To remove a rule, the values that you specify (for example, ports) must match the existing rule's values exactly.</p> <p>Each rule consists of the protocol and the IPv4 or IPv6 CIDR range or source security group. For the TCP and UDP protocols, you must also specify the destination port or range of ports. For the ICMP protocol, you must also specify the ICMP type and code. If the security group rule has a description, you do not have to specify the description to revoke the rule.</p> <p>Rule changes are propagated to instances within the security group as quickly as possible. However, a small delay might occur.</p>

#### `revokeSecurityGroupIngress`

``` purescript
revokeSecurityGroupIngress :: forall eff. RevokeSecurityGroupIngressRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes one or more ingress rules from a security group. To remove a rule, the values that you specify (for example, ports) must match the existing rule's values exactly.</p> <note> <p>[EC2-Classic security groups only] If the values you specify do not match the existing rule's values, no error is returned. Use <a>DescribeSecurityGroups</a> to verify that the rule has been removed.</p> </note> <p>Each rule consists of the protocol and the CIDR range or source security group. For the TCP and UDP protocols, you must also specify the destination port or range of ports. For the ICMP protocol, you must also specify the ICMP type and code. If the security group rule has a description, you do not have to specify the description to revoke the rule.</p> <p>Rule changes are propagated to instances within the security group as quickly as possible. However, a small delay might occur.</p>

#### `runInstances`

``` purescript
runInstances :: forall eff. RunInstancesRequest -> Aff (err :: RequestError | eff) Reservation
```

<p>Launches the specified number of instances using an AMI for which you have permissions. </p> <p>You can specify a number of options, or leave the default options. The following rules apply:</p> <ul> <li> <p>[EC2-VPC] If you don't specify a subnet ID, we choose a default subnet from your default VPC for you. If you don't have a default VPC, you must specify a subnet ID in the request.</p> </li> <li> <p>[EC2-Classic] If don't specify an Availability Zone, we choose one for you.</p> </li> <li> <p>Some instance types must be launched into a VPC. If you do not have a default VPC, or if you do not specify a subnet ID, the request fails. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-vpc.html#vpc-only-instance-types">Instance Types Available Only in a VPC</a>.</p> </li> <li> <p>[EC2-VPC] All instances have a network interface with a primary private IPv4 address. If you don't specify this address, we choose one from the IPv4 range of your subnet.</p> </li> <li> <p>Not all instance types support IPv6 addresses. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html">Instance Types</a>.</p> </li> <li> <p>If you don't specify a security group ID, we use the default security group. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html">Security Groups</a>.</p> </li> <li> <p>If any of the AMIs have a product code attached for which the user has not subscribed, the request fails.</p> </li> </ul> <p>You can create a <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html">launch template</a>, which is a resource that contains the parameters to launch an instance. When you launch an instance using <a>RunInstances</a>, you can specify the launch template instead of specifying the launch parameters.</p> <p>To ensure faster instance launches, break up large requests into smaller batches. For example, create five separate launch requests for 100 instances each instead of one launch request for 500 instances.</p> <p>An instance is ready for you to use when it's in the <code>running</code> state. You can check the state of your instance using <a>DescribeInstances</a>. You can tag instances and EBS volumes during launch, after launch, or both. For more information, see <a>CreateTags</a> and <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html">Tagging Your Amazon EC2 Resources</a>.</p> <p>Linux instances have access to the public key of the key pair at boot. You can use this key to provide secure access to the instance. Amazon EC2 public images use this feature to provide secure access without passwords. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html">Key Pairs</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>For troubleshooting, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_InstanceStraightToTerminated.html">What To Do If An Instance Immediately Terminates</a>, and <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesConnecting.html">Troubleshooting Connecting to Your Instance</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `runScheduledInstances`

``` purescript
runScheduledInstances :: forall eff. RunScheduledInstancesRequest -> Aff (err :: RequestError | eff) RunScheduledInstancesResult
```

<p>Launches the specified Scheduled Instances.</p> <p>Before you can launch a Scheduled Instance, you must purchase it and obtain an identifier using <a>PurchaseScheduledInstances</a>.</p> <p>You must launch a Scheduled Instance during its scheduled time period. You can't stop or reboot a Scheduled Instance, but you can terminate it as needed. If you terminate a Scheduled Instance before the current scheduled time period ends, you can launch it again after a few minutes. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-scheduled-instances.html">Scheduled Instances</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `startInstances`

``` purescript
startInstances :: forall eff. StartInstancesRequest -> Aff (err :: RequestError | eff) StartInstancesResult
```

<p>Starts an Amazon EBS-backed instance that you've previously stopped.</p> <p>Instances that use Amazon EBS volumes as their root devices can be quickly stopped and started. When an instance is stopped, the compute resources are released and you are not billed for instance usage. However, your root partition Amazon EBS volume remains and continues to persist your data, and you are charged for Amazon EBS volume usage. You can restart your instance at any time. Every time you start your Windows instance, Amazon EC2 charges you for a full instance hour. If you stop and restart your Windows instance, a new instance hour begins and Amazon EC2 charges you for another full instance hour even if you are still within the same 60-minute period when it was stopped. Every time you start your Linux instance, Amazon EC2 charges a one-minute minimum for instance usage, and thereafter charges per second for instance usage.</p> <p>Before stopping an instance, make sure it is in a state from which it can be restarted. Stopping an instance does not preserve data stored in RAM.</p> <p>Performing this operation on an instance that uses an instance store as its root device returns an error.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html">Stopping Instances</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `stopInstances`

``` purescript
stopInstances :: forall eff. StopInstancesRequest -> Aff (err :: RequestError | eff) StopInstancesResult
```

<p>Stops an Amazon EBS-backed instance.</p> <p>We don't charge usage for a stopped instance, or data transfer fees; however, your root partition Amazon EBS volume remains and continues to persist your data, and you are charged for Amazon EBS volume usage. Every time you start your Windows instance, Amazon EC2 charges you for a full instance hour. If you stop and restart your Windows instance, a new instance hour begins and Amazon EC2 charges you for another full instance hour even if you are still within the same 60-minute period when it was stopped. Every time you start your Linux instance, Amazon EC2 charges a one-minute minimum for instance usage, and thereafter charges per second for instance usage.</p> <p>You can't start or stop Spot Instances, and you can't stop instance store-backed instances.</p> <p>When you stop an instance, we shut it down. You can restart your instance at any time. Before stopping an instance, make sure it is in a state from which it can be restarted. Stopping an instance does not preserve data stored in RAM.</p> <p>Stopping an instance is different to rebooting or terminating it. For example, when you stop an instance, the root device and any other devices attached to the instance persist. When you terminate an instance, the root device and any other devices attached during the instance launch are automatically deleted. For more information about the differences between rebooting, stopping, and terminating instances, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html">Instance Lifecycle</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>When you stop an instance, we attempt to shut it down forcibly after a short while. If your instance appears stuck in the stopping state after a period of time, there may be an issue with the underlying host computer. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesStopping.html">Troubleshooting Stopping Your Instance</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `terminateInstances`

``` purescript
terminateInstances :: forall eff. TerminateInstancesRequest -> Aff (err :: RequestError | eff) TerminateInstancesResult
```

<p>Shuts down one or more instances. This operation is idempotent; if you terminate an instance more than once, each call succeeds. </p> <p>If you specify multiple instances and the request fails (for example, because of a single incorrect instance ID), none of the instances are terminated.</p> <p>Terminated instances remain visible after termination (for approximately one hour).</p> <p>By default, Amazon EC2 deletes all EBS volumes that were attached when the instance launched. Volumes attached after instance launch continue running.</p> <p>You can stop, start, and terminate EBS-backed instances. You can only terminate instance store-backed instances. What happens to an instance differs if you stop it or terminate it. For example, when you stop an instance, the root device and any other devices attached to the instance persist. When you terminate an instance, any attached EBS volumes with the <code>DeleteOnTermination</code> block device mapping parameter set to <code>true</code> are automatically deleted. For more information about the differences between stopping and terminating instances, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html">Instance Lifecycle</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p> <p>For more information about troubleshooting, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesShuttingDown.html">Troubleshooting Terminating Your Instance</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `unassignIpv6Addresses`

``` purescript
unassignIpv6Addresses :: forall eff. UnassignIpv6AddressesRequest -> Aff (err :: RequestError | eff) UnassignIpv6AddressesResult
```

<p>Unassigns one or more IPv6 addresses from a network interface.</p>

#### `unassignPrivateIpAddresses`

``` purescript
unassignPrivateIpAddresses :: forall eff. UnassignPrivateIpAddressesRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Unassigns one or more secondary private IP addresses from a network interface.</p>

#### `unmonitorInstances`

``` purescript
unmonitorInstances :: forall eff. UnmonitorInstancesRequest -> Aff (err :: RequestError | eff) UnmonitorInstancesResult
```

<p>Disables detailed monitoring for a running instance. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html">Monitoring Your Instances and Volumes</a> in the <i>Amazon Elastic Compute Cloud User Guide</i>.</p>

#### `updateSecurityGroupRuleDescriptionsEgress`

``` purescript
updateSecurityGroupRuleDescriptionsEgress :: forall eff. UpdateSecurityGroupRuleDescriptionsEgressRequest -> Aff (err :: RequestError | eff) UpdateSecurityGroupRuleDescriptionsEgressResult
```

<p>[EC2-VPC only] Updates the description of an egress (outbound) security group rule. You can replace an existing description, or add a description to a rule that did not have one previously.</p> <p>You specify the description as part of the IP permissions structure. You can remove a description for a security group rule by omitting the description parameter in the request.</p>

#### `updateSecurityGroupRuleDescriptionsIngress`

``` purescript
updateSecurityGroupRuleDescriptionsIngress :: forall eff. UpdateSecurityGroupRuleDescriptionsIngressRequest -> Aff (err :: RequestError | eff) UpdateSecurityGroupRuleDescriptionsIngressResult
```

<p>Updates the description of an ingress (inbound) security group rule. You can replace an existing description, or add a description to a rule that did not have one previously.</p> <p>You specify the description as part of the IP permissions structure. You can remove a description for a security group rule by omitting the description parameter in the request.</p>

#### `AcceptReservedInstancesExchangeQuoteRequest`

``` purescript
newtype AcceptReservedInstancesExchangeQuoteRequest
  = AcceptReservedInstancesExchangeQuoteRequest { "DryRun" :: NullOrUndefined (Boolean), "ReservedInstanceIds" :: ReservedInstanceIdSet, "TargetConfigurations" :: NullOrUndefined (TargetConfigurationRequestSet) }
```

<p>Contains the parameters for accepting the quote.</p>

#### `AcceptReservedInstancesExchangeQuoteResult`

``` purescript
newtype AcceptReservedInstancesExchangeQuoteResult
  = AcceptReservedInstancesExchangeQuoteResult { "ExchangeId" :: NullOrUndefined (String) }
```

<p>The result of the exchange and whether it was <code>successful</code>.</p>

#### `AcceptVpcEndpointConnectionsRequest`

``` purescript
newtype AcceptVpcEndpointConnectionsRequest
  = AcceptVpcEndpointConnectionsRequest { "DryRun" :: NullOrUndefined (Boolean), "ServiceId" :: String, "VpcEndpointIds" :: ValueStringList }
```

#### `AcceptVpcEndpointConnectionsResult`

``` purescript
newtype AcceptVpcEndpointConnectionsResult
  = AcceptVpcEndpointConnectionsResult { "Unsuccessful" :: NullOrUndefined (UnsuccessfulItemSet) }
```

#### `AcceptVpcPeeringConnectionRequest`

``` purescript
newtype AcceptVpcPeeringConnectionRequest
  = AcceptVpcPeeringConnectionRequest { "DryRun" :: NullOrUndefined (Boolean), "VpcPeeringConnectionId" :: NullOrUndefined (String) }
```

<p>Contains the parameters for AcceptVpcPeeringConnection.</p>

#### `AcceptVpcPeeringConnectionResult`

``` purescript
newtype AcceptVpcPeeringConnectionResult
  = AcceptVpcPeeringConnectionResult { "VpcPeeringConnection" :: NullOrUndefined (VpcPeeringConnection) }
```

<p>Contains the output of AcceptVpcPeeringConnection.</p>

#### `AccountAttribute`

``` purescript
newtype AccountAttribute
  = AccountAttribute { "AttributeName" :: NullOrUndefined (String), "AttributeValues" :: NullOrUndefined (AccountAttributeValueList) }
```

<p>Describes an account attribute.</p>

#### `AccountAttributeList`

``` purescript
newtype AccountAttributeList
  = AccountAttributeList (Array AccountAttribute)
```

#### `AccountAttributeName`

``` purescript
newtype AccountAttributeName
  = AccountAttributeName String
```

#### `AccountAttributeNameStringList`

``` purescript
newtype AccountAttributeNameStringList
  = AccountAttributeNameStringList (Array AccountAttributeName)
```

#### `AccountAttributeValue`

``` purescript
newtype AccountAttributeValue
  = AccountAttributeValue { "AttributeValue" :: NullOrUndefined (String) }
```

<p>Describes a value of an account attribute.</p>

#### `AccountAttributeValueList`

``` purescript
newtype AccountAttributeValueList
  = AccountAttributeValueList (Array AccountAttributeValue)
```

#### `ActiveInstance`

``` purescript
newtype ActiveInstance
  = ActiveInstance { "InstanceId" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (String), "SpotInstanceRequestId" :: NullOrUndefined (String), "InstanceHealth" :: NullOrUndefined (InstanceHealthStatus) }
```

<p>Describes a running instance in a Spot Fleet.</p>

#### `ActiveInstanceSet`

``` purescript
newtype ActiveInstanceSet
  = ActiveInstanceSet (Array ActiveInstance)
```

#### `ActivityStatus`

``` purescript
newtype ActivityStatus
  = ActivityStatus String
```

#### `Address`

``` purescript
newtype Address
  = Address { "InstanceId" :: NullOrUndefined (String), "PublicIp" :: NullOrUndefined (String), "AllocationId" :: NullOrUndefined (String), "AssociationId" :: NullOrUndefined (String), "Domain" :: NullOrUndefined (DomainType), "NetworkInterfaceId" :: NullOrUndefined (String), "NetworkInterfaceOwnerId" :: NullOrUndefined (String), "PrivateIpAddress" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes an Elastic IP address.</p>

#### `AddressList`

``` purescript
newtype AddressList
  = AddressList (Array Address)
```

#### `Affinity`

``` purescript
newtype Affinity
  = Affinity String
```

#### `AllocateAddressRequest`

``` purescript
newtype AllocateAddressRequest
  = AllocateAddressRequest { "Domain" :: NullOrUndefined (DomainType), "Address" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for AllocateAddress.</p>

#### `AllocateAddressResult`

``` purescript
newtype AllocateAddressResult
  = AllocateAddressResult { "PublicIp" :: NullOrUndefined (String), "AllocationId" :: NullOrUndefined (String), "Domain" :: NullOrUndefined (DomainType) }
```

<p>Contains the output of AllocateAddress.</p>

#### `AllocateHostsRequest`

``` purescript
newtype AllocateHostsRequest
  = AllocateHostsRequest { "AutoPlacement" :: NullOrUndefined (AutoPlacement), "AvailabilityZone" :: String, "ClientToken" :: NullOrUndefined (String), "InstanceType" :: String, "Quantity" :: Int }
```

<p>Contains the parameters for AllocateHosts.</p>

#### `AllocateHostsResult`

``` purescript
newtype AllocateHostsResult
  = AllocateHostsResult { "HostIds" :: NullOrUndefined (ResponseHostIdList) }
```

<p>Contains the output of AllocateHosts.</p>

#### `AllocationIdList`

``` purescript
newtype AllocationIdList
  = AllocationIdList (Array String)
```

#### `AllocationState`

``` purescript
newtype AllocationState
  = AllocationState String
```

#### `AllocationStrategy`

``` purescript
newtype AllocationStrategy
  = AllocationStrategy String
```

#### `AllowedPrincipal`

``` purescript
newtype AllowedPrincipal
  = AllowedPrincipal { "PrincipalType" :: NullOrUndefined (PrincipalType), "Principal" :: NullOrUndefined (String) }
```

<p>Describes a principal.</p>

#### `AllowedPrincipalSet`

``` purescript
newtype AllowedPrincipalSet
  = AllowedPrincipalSet (Array AllowedPrincipal)
```

#### `ArchitectureValues`

``` purescript
newtype ArchitectureValues
  = ArchitectureValues String
```

#### `AssignIpv6AddressesRequest`

``` purescript
newtype AssignIpv6AddressesRequest
  = AssignIpv6AddressesRequest { "Ipv6AddressCount" :: NullOrUndefined (Int), "Ipv6Addresses" :: NullOrUndefined (Ipv6AddressList), "NetworkInterfaceId" :: String }
```

#### `AssignIpv6AddressesResult`

``` purescript
newtype AssignIpv6AddressesResult
  = AssignIpv6AddressesResult { "AssignedIpv6Addresses" :: NullOrUndefined (Ipv6AddressList), "NetworkInterfaceId" :: NullOrUndefined (String) }
```

#### `AssignPrivateIpAddressesRequest`

``` purescript
newtype AssignPrivateIpAddressesRequest
  = AssignPrivateIpAddressesRequest { "AllowReassignment" :: NullOrUndefined (Boolean), "NetworkInterfaceId" :: String, "PrivateIpAddresses" :: NullOrUndefined (PrivateIpAddressStringList), "SecondaryPrivateIpAddressCount" :: NullOrUndefined (Int) }
```

<p>Contains the parameters for AssignPrivateIpAddresses.</p>

#### `AssociateAddressRequest`

``` purescript
newtype AssociateAddressRequest
  = AssociateAddressRequest { "AllocationId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "PublicIp" :: NullOrUndefined (String), "AllowReassociation" :: NullOrUndefined (Boolean), "DryRun" :: NullOrUndefined (Boolean), "NetworkInterfaceId" :: NullOrUndefined (String), "PrivateIpAddress" :: NullOrUndefined (String) }
```

<p>Contains the parameters for AssociateAddress.</p>

#### `AssociateAddressResult`

``` purescript
newtype AssociateAddressResult
  = AssociateAddressResult { "AssociationId" :: NullOrUndefined (String) }
```

<p>Contains the output of AssociateAddress.</p>

#### `AssociateDhcpOptionsRequest`

``` purescript
newtype AssociateDhcpOptionsRequest
  = AssociateDhcpOptionsRequest { "DhcpOptionsId" :: String, "VpcId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for AssociateDhcpOptions.</p>

#### `AssociateIamInstanceProfileRequest`

``` purescript
newtype AssociateIamInstanceProfileRequest
  = AssociateIamInstanceProfileRequest { "IamInstanceProfile" :: IamInstanceProfileSpecification, "InstanceId" :: String }
```

#### `AssociateIamInstanceProfileResult`

``` purescript
newtype AssociateIamInstanceProfileResult
  = AssociateIamInstanceProfileResult { "IamInstanceProfileAssociation" :: NullOrUndefined (IamInstanceProfileAssociation) }
```

#### `AssociateRouteTableRequest`

``` purescript
newtype AssociateRouteTableRequest
  = AssociateRouteTableRequest { "DryRun" :: NullOrUndefined (Boolean), "RouteTableId" :: String, "SubnetId" :: String }
```

<p>Contains the parameters for AssociateRouteTable.</p>

#### `AssociateRouteTableResult`

``` purescript
newtype AssociateRouteTableResult
  = AssociateRouteTableResult { "AssociationId" :: NullOrUndefined (String) }
```

<p>Contains the output of AssociateRouteTable.</p>

#### `AssociateSubnetCidrBlockRequest`

``` purescript
newtype AssociateSubnetCidrBlockRequest
  = AssociateSubnetCidrBlockRequest { "Ipv6CidrBlock" :: String, "SubnetId" :: String }
```

#### `AssociateSubnetCidrBlockResult`

``` purescript
newtype AssociateSubnetCidrBlockResult
  = AssociateSubnetCidrBlockResult { "Ipv6CidrBlockAssociation" :: NullOrUndefined (SubnetIpv6CidrBlockAssociation), "SubnetId" :: NullOrUndefined (String) }
```

#### `AssociateVpcCidrBlockRequest`

``` purescript
newtype AssociateVpcCidrBlockRequest
  = AssociateVpcCidrBlockRequest { "AmazonProvidedIpv6CidrBlock" :: NullOrUndefined (Boolean), "CidrBlock" :: NullOrUndefined (String), "VpcId" :: String }
```

#### `AssociateVpcCidrBlockResult`

``` purescript
newtype AssociateVpcCidrBlockResult
  = AssociateVpcCidrBlockResult { "Ipv6CidrBlockAssociation" :: NullOrUndefined (VpcIpv6CidrBlockAssociation), "CidrBlockAssociation" :: NullOrUndefined (VpcCidrBlockAssociation), "VpcId" :: NullOrUndefined (String) }
```

#### `AssociationIdList`

``` purescript
newtype AssociationIdList
  = AssociationIdList (Array String)
```

#### `AttachClassicLinkVpcRequest`

``` purescript
newtype AttachClassicLinkVpcRequest
  = AttachClassicLinkVpcRequest { "DryRun" :: NullOrUndefined (Boolean), "Groups" :: GroupIdStringList, "InstanceId" :: String, "VpcId" :: String }
```

<p>Contains the parameters for AttachClassicLinkVpc.</p>

#### `AttachClassicLinkVpcResult`

``` purescript
newtype AttachClassicLinkVpcResult
  = AttachClassicLinkVpcResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of AttachClassicLinkVpc.</p>

#### `AttachInternetGatewayRequest`

``` purescript
newtype AttachInternetGatewayRequest
  = AttachInternetGatewayRequest { "DryRun" :: NullOrUndefined (Boolean), "InternetGatewayId" :: String, "VpcId" :: String }
```

<p>Contains the parameters for AttachInternetGateway.</p>

#### `AttachNetworkInterfaceRequest`

``` purescript
newtype AttachNetworkInterfaceRequest
  = AttachNetworkInterfaceRequest { "DeviceIndex" :: Int, "DryRun" :: NullOrUndefined (Boolean), "InstanceId" :: String, "NetworkInterfaceId" :: String }
```

<p>Contains the parameters for AttachNetworkInterface.</p>

#### `AttachNetworkInterfaceResult`

``` purescript
newtype AttachNetworkInterfaceResult
  = AttachNetworkInterfaceResult { "AttachmentId" :: NullOrUndefined (String) }
```

<p>Contains the output of AttachNetworkInterface.</p>

#### `AttachVolumeRequest`

``` purescript
newtype AttachVolumeRequest
  = AttachVolumeRequest { "Device" :: String, "InstanceId" :: String, "VolumeId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for AttachVolume.</p>

#### `AttachVpnGatewayRequest`

``` purescript
newtype AttachVpnGatewayRequest
  = AttachVpnGatewayRequest { "VpcId" :: String, "VpnGatewayId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for AttachVpnGateway.</p>

#### `AttachVpnGatewayResult`

``` purescript
newtype AttachVpnGatewayResult
  = AttachVpnGatewayResult { "VpcAttachment" :: NullOrUndefined (VpcAttachment) }
```

<p>Contains the output of AttachVpnGateway.</p>

#### `AttachmentStatus`

``` purescript
newtype AttachmentStatus
  = AttachmentStatus String
```

#### `AttributeBooleanValue`

``` purescript
newtype AttributeBooleanValue
  = AttributeBooleanValue { "Value" :: NullOrUndefined (Boolean) }
```

<p>Describes a value for a resource attribute that is a Boolean value.</p>

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue { "Value" :: NullOrUndefined (String) }
```

<p>Describes a value for a resource attribute that is a String.</p>

#### `AuthorizeSecurityGroupEgressRequest`

``` purescript
newtype AuthorizeSecurityGroupEgressRequest
  = AuthorizeSecurityGroupEgressRequest { "DryRun" :: NullOrUndefined (Boolean), "GroupId" :: String, "IpPermissions" :: NullOrUndefined (IpPermissionList), "CidrIp" :: NullOrUndefined (String), "FromPort" :: NullOrUndefined (Int), "IpProtocol" :: NullOrUndefined (String), "ToPort" :: NullOrUndefined (Int), "SourceSecurityGroupName" :: NullOrUndefined (String), "SourceSecurityGroupOwnerId" :: NullOrUndefined (String) }
```

<p>Contains the parameters for AuthorizeSecurityGroupEgress.</p>

#### `AuthorizeSecurityGroupIngressRequest`

``` purescript
newtype AuthorizeSecurityGroupIngressRequest
  = AuthorizeSecurityGroupIngressRequest { "CidrIp" :: NullOrUndefined (String), "FromPort" :: NullOrUndefined (Int), "GroupId" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String), "IpPermissions" :: NullOrUndefined (IpPermissionList), "IpProtocol" :: NullOrUndefined (String), "SourceSecurityGroupName" :: NullOrUndefined (String), "SourceSecurityGroupOwnerId" :: NullOrUndefined (String), "ToPort" :: NullOrUndefined (Int), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for AuthorizeSecurityGroupIngress.</p>

#### `AutoPlacement`

``` purescript
newtype AutoPlacement
  = AutoPlacement String
```

#### `AvailabilityZone`

``` purescript
newtype AvailabilityZone
  = AvailabilityZone { "State" :: NullOrUndefined (AvailabilityZoneState), "Messages" :: NullOrUndefined (AvailabilityZoneMessageList), "RegionName" :: NullOrUndefined (String), "ZoneName" :: NullOrUndefined (String) }
```

<p>Describes an Availability Zone.</p>

#### `AvailabilityZoneList`

``` purescript
newtype AvailabilityZoneList
  = AvailabilityZoneList (Array AvailabilityZone)
```

#### `AvailabilityZoneMessage`

``` purescript
newtype AvailabilityZoneMessage
  = AvailabilityZoneMessage { "Message" :: NullOrUndefined (String) }
```

<p>Describes a message about an Availability Zone.</p>

#### `AvailabilityZoneMessageList`

``` purescript
newtype AvailabilityZoneMessageList
  = AvailabilityZoneMessageList (Array AvailabilityZoneMessage)
```

#### `AvailabilityZoneState`

``` purescript
newtype AvailabilityZoneState
  = AvailabilityZoneState String
```

#### `AvailableCapacity`

``` purescript
newtype AvailableCapacity
  = AvailableCapacity { "AvailableInstanceCapacity" :: NullOrUndefined (AvailableInstanceCapacityList), "AvailableVCpus" :: NullOrUndefined (Int) }
```

<p>The capacity information for instances launched onto the Dedicated Host.</p>

#### `AvailableInstanceCapacityList`

``` purescript
newtype AvailableInstanceCapacityList
  = AvailableInstanceCapacityList (Array InstanceCapacity)
```

#### `BatchState`

``` purescript
newtype BatchState
  = BatchState String
```

#### `BillingProductList`

``` purescript
newtype BillingProductList
  = BillingProductList (Array String)
```

#### `BlobAttributeValue`

``` purescript
newtype BlobAttributeValue
  = BlobAttributeValue { "Value" :: NullOrUndefined (String) }
```

#### `BlockDeviceMapping`

``` purescript
newtype BlockDeviceMapping
  = BlockDeviceMapping { "DeviceName" :: NullOrUndefined (String), "VirtualName" :: NullOrUndefined (String), "Ebs" :: NullOrUndefined (EbsBlockDevice), "NoDevice" :: NullOrUndefined (String) }
```

<p>Describes a block device mapping.</p>

#### `BlockDeviceMappingList`

``` purescript
newtype BlockDeviceMappingList
  = BlockDeviceMappingList (Array BlockDeviceMapping)
```

#### `BlockDeviceMappingRequestList`

``` purescript
newtype BlockDeviceMappingRequestList
  = BlockDeviceMappingRequestList (Array BlockDeviceMapping)
```

#### `BundleIdStringList`

``` purescript
newtype BundleIdStringList
  = BundleIdStringList (Array String)
```

#### `BundleInstanceRequest`

``` purescript
newtype BundleInstanceRequest
  = BundleInstanceRequest { "InstanceId" :: String, "Storage" :: Storage, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for BundleInstance.</p>

#### `BundleInstanceResult`

``` purescript
newtype BundleInstanceResult
  = BundleInstanceResult { "BundleTask" :: NullOrUndefined (BundleTask) }
```

<p>Contains the output of BundleInstance.</p>

#### `BundleTask`

``` purescript
newtype BundleTask
  = BundleTask { "BundleId" :: NullOrUndefined (String), "BundleTaskError" :: NullOrUndefined (BundleTaskError), "InstanceId" :: NullOrUndefined (String), "Progress" :: NullOrUndefined (String), "StartTime" :: NullOrUndefined (DateTime), "State" :: NullOrUndefined (BundleTaskState), "Storage" :: NullOrUndefined (Storage), "UpdateTime" :: NullOrUndefined (DateTime) }
```

<p>Describes a bundle task.</p>

#### `BundleTaskError`

``` purescript
newtype BundleTaskError
  = BundleTaskError { "Code" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

<p>Describes an error for <a>BundleInstance</a>.</p>

#### `BundleTaskList`

``` purescript
newtype BundleTaskList
  = BundleTaskList (Array BundleTask)
```

#### `BundleTaskState`

``` purescript
newtype BundleTaskState
  = BundleTaskState String
```

#### `CancelBatchErrorCode`

``` purescript
newtype CancelBatchErrorCode
  = CancelBatchErrorCode String
```

#### `CancelBundleTaskRequest`

``` purescript
newtype CancelBundleTaskRequest
  = CancelBundleTaskRequest { "BundleId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CancelBundleTask.</p>

#### `CancelBundleTaskResult`

``` purescript
newtype CancelBundleTaskResult
  = CancelBundleTaskResult { "BundleTask" :: NullOrUndefined (BundleTask) }
```

<p>Contains the output of CancelBundleTask.</p>

#### `CancelConversionRequest`

``` purescript
newtype CancelConversionRequest
  = CancelConversionRequest { "ConversionTaskId" :: String, "DryRun" :: NullOrUndefined (Boolean), "ReasonMessage" :: NullOrUndefined (String) }
```

<p>Contains the parameters for CancelConversionTask.</p>

#### `CancelExportTaskRequest`

``` purescript
newtype CancelExportTaskRequest
  = CancelExportTaskRequest { "ExportTaskId" :: String }
```

<p>Contains the parameters for CancelExportTask.</p>

#### `CancelImportTaskRequest`

``` purescript
newtype CancelImportTaskRequest
  = CancelImportTaskRequest { "CancelReason" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "ImportTaskId" :: NullOrUndefined (String) }
```

<p>Contains the parameters for CancelImportTask.</p>

#### `CancelImportTaskResult`

``` purescript
newtype CancelImportTaskResult
  = CancelImportTaskResult { "ImportTaskId" :: NullOrUndefined (String), "PreviousState" :: NullOrUndefined (String), "State" :: NullOrUndefined (String) }
```

<p>Contains the output for CancelImportTask.</p>

#### `CancelReservedInstancesListingRequest`

``` purescript
newtype CancelReservedInstancesListingRequest
  = CancelReservedInstancesListingRequest { "ReservedInstancesListingId" :: String }
```

<p>Contains the parameters for CancelReservedInstancesListing.</p>

#### `CancelReservedInstancesListingResult`

``` purescript
newtype CancelReservedInstancesListingResult
  = CancelReservedInstancesListingResult { "ReservedInstancesListings" :: NullOrUndefined (ReservedInstancesListingList) }
```

<p>Contains the output of CancelReservedInstancesListing.</p>

#### `CancelSpotFleetRequestsError`

``` purescript
newtype CancelSpotFleetRequestsError
  = CancelSpotFleetRequestsError { "Code" :: CancelBatchErrorCode, "Message" :: String }
```

<p>Describes a Spot Fleet error.</p>

#### `CancelSpotFleetRequestsErrorItem`

``` purescript
newtype CancelSpotFleetRequestsErrorItem
  = CancelSpotFleetRequestsErrorItem { "Error" :: CancelSpotFleetRequestsError, "SpotFleetRequestId" :: String }
```

<p>Describes a Spot Fleet request that was not successfully canceled.</p>

#### `CancelSpotFleetRequestsErrorSet`

``` purescript
newtype CancelSpotFleetRequestsErrorSet
  = CancelSpotFleetRequestsErrorSet (Array CancelSpotFleetRequestsErrorItem)
```

#### `CancelSpotFleetRequestsRequest`

``` purescript
newtype CancelSpotFleetRequestsRequest
  = CancelSpotFleetRequestsRequest { "DryRun" :: NullOrUndefined (Boolean), "SpotFleetRequestIds" :: ValueStringList, "TerminateInstances" :: Boolean }
```

<p>Contains the parameters for CancelSpotFleetRequests.</p>

#### `CancelSpotFleetRequestsResponse`

``` purescript
newtype CancelSpotFleetRequestsResponse
  = CancelSpotFleetRequestsResponse { "SuccessfulFleetRequests" :: NullOrUndefined (CancelSpotFleetRequestsSuccessSet), "UnsuccessfulFleetRequests" :: NullOrUndefined (CancelSpotFleetRequestsErrorSet) }
```

<p>Contains the output of CancelSpotFleetRequests.</p>

#### `CancelSpotFleetRequestsSuccessItem`

``` purescript
newtype CancelSpotFleetRequestsSuccessItem
  = CancelSpotFleetRequestsSuccessItem { "CurrentSpotFleetRequestState" :: BatchState, "PreviousSpotFleetRequestState" :: BatchState, "SpotFleetRequestId" :: String }
```

<p>Describes a Spot Fleet request that was successfully canceled.</p>

#### `CancelSpotFleetRequestsSuccessSet`

``` purescript
newtype CancelSpotFleetRequestsSuccessSet
  = CancelSpotFleetRequestsSuccessSet (Array CancelSpotFleetRequestsSuccessItem)
```

#### `CancelSpotInstanceRequestState`

``` purescript
newtype CancelSpotInstanceRequestState
  = CancelSpotInstanceRequestState String
```

#### `CancelSpotInstanceRequestsRequest`

``` purescript
newtype CancelSpotInstanceRequestsRequest
  = CancelSpotInstanceRequestsRequest { "DryRun" :: NullOrUndefined (Boolean), "SpotInstanceRequestIds" :: SpotInstanceRequestIdList }
```

<p>Contains the parameters for CancelSpotInstanceRequests.</p>

#### `CancelSpotInstanceRequestsResult`

``` purescript
newtype CancelSpotInstanceRequestsResult
  = CancelSpotInstanceRequestsResult { "CancelledSpotInstanceRequests" :: NullOrUndefined (CancelledSpotInstanceRequestList) }
```

<p>Contains the output of CancelSpotInstanceRequests.</p>

#### `CancelledSpotInstanceRequest`

``` purescript
newtype CancelledSpotInstanceRequest
  = CancelledSpotInstanceRequest { "SpotInstanceRequestId" :: NullOrUndefined (String), "State" :: NullOrUndefined (CancelSpotInstanceRequestState) }
```

<p>Describes a request to cancel a Spot Instance.</p>

#### `CancelledSpotInstanceRequestList`

``` purescript
newtype CancelledSpotInstanceRequestList
  = CancelledSpotInstanceRequestList (Array CancelledSpotInstanceRequest)
```

#### `CidrBlock`

``` purescript
newtype CidrBlock
  = CidrBlock { "CidrBlock" :: NullOrUndefined (String) }
```

<p>Describes an IPv4 CIDR block.</p>

#### `CidrBlockSet`

``` purescript
newtype CidrBlockSet
  = CidrBlockSet (Array CidrBlock)
```

#### `ClassicLinkDnsSupport`

``` purescript
newtype ClassicLinkDnsSupport
  = ClassicLinkDnsSupport { "ClassicLinkDnsSupported" :: NullOrUndefined (Boolean), "VpcId" :: NullOrUndefined (String) }
```

<p>Describes the ClassicLink DNS support status of a VPC.</p>

#### `ClassicLinkDnsSupportList`

``` purescript
newtype ClassicLinkDnsSupportList
  = ClassicLinkDnsSupportList (Array ClassicLinkDnsSupport)
```

#### `ClassicLinkInstance`

``` purescript
newtype ClassicLinkInstance
  = ClassicLinkInstance { "Groups" :: NullOrUndefined (GroupIdentifierList), "InstanceId" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList), "VpcId" :: NullOrUndefined (String) }
```

<p>Describes a linked EC2-Classic instance.</p>

#### `ClassicLinkInstanceList`

``` purescript
newtype ClassicLinkInstanceList
  = ClassicLinkInstanceList (Array ClassicLinkInstance)
```

#### `ClassicLoadBalancer`

``` purescript
newtype ClassicLoadBalancer
  = ClassicLoadBalancer { "Name" :: String }
```

<p>Describes a Classic Load Balancer.</p>

#### `ClassicLoadBalancers`

``` purescript
newtype ClassicLoadBalancers
  = ClassicLoadBalancers (Array ClassicLoadBalancer)
```

#### `ClassicLoadBalancersConfig`

``` purescript
newtype ClassicLoadBalancersConfig
  = ClassicLoadBalancersConfig { "ClassicLoadBalancers" :: ClassicLoadBalancers }
```

<p>Describes the Classic Load Balancers to attach to a Spot Fleet. Spot Fleet registers the running Spot Instances with these Classic Load Balancers.</p>

#### `ClientData`

``` purescript
newtype ClientData
  = ClientData { "Comment" :: NullOrUndefined (String), "UploadEnd" :: NullOrUndefined (DateTime), "UploadSize" :: NullOrUndefined (Number), "UploadStart" :: NullOrUndefined (DateTime) }
```

<p>Describes the client-specific data.</p>

#### `ConfirmProductInstanceRequest`

``` purescript
newtype ConfirmProductInstanceRequest
  = ConfirmProductInstanceRequest { "InstanceId" :: String, "ProductCode" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for ConfirmProductInstance.</p>

#### `ConfirmProductInstanceResult`

``` purescript
newtype ConfirmProductInstanceResult
  = ConfirmProductInstanceResult { "OwnerId" :: NullOrUndefined (String), "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of ConfirmProductInstance.</p>

#### `ConnectionNotification`

``` purescript
newtype ConnectionNotification
  = ConnectionNotification { "ConnectionNotificationId" :: NullOrUndefined (String), "ServiceId" :: NullOrUndefined (String), "VpcEndpointId" :: NullOrUndefined (String), "ConnectionNotificationType" :: NullOrUndefined (ConnectionNotificationType), "ConnectionNotificationArn" :: NullOrUndefined (String), "ConnectionEvents" :: NullOrUndefined (ValueStringList), "ConnectionNotificationState" :: NullOrUndefined (ConnectionNotificationState) }
```

<p>Describes a connection notification for a VPC endpoint or VPC endpoint service.</p>

#### `ConnectionNotificationSet`

``` purescript
newtype ConnectionNotificationSet
  = ConnectionNotificationSet (Array ConnectionNotification)
```

#### `ConnectionNotificationState`

``` purescript
newtype ConnectionNotificationState
  = ConnectionNotificationState String
```

#### `ConnectionNotificationType`

``` purescript
newtype ConnectionNotificationType
  = ConnectionNotificationType String
```

#### `ContainerFormat`

``` purescript
newtype ContainerFormat
  = ContainerFormat String
```

#### `ConversionIdStringList`

``` purescript
newtype ConversionIdStringList
  = ConversionIdStringList (Array String)
```

#### `ConversionTask`

``` purescript
newtype ConversionTask
  = ConversionTask { "ConversionTaskId" :: String, "ExpirationTime" :: NullOrUndefined (String), "ImportInstance" :: NullOrUndefined (ImportInstanceTaskDetails), "ImportVolume" :: NullOrUndefined (ImportVolumeTaskDetails), "State" :: ConversionTaskState, "StatusMessage" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a conversion task.</p>

#### `ConversionTaskState`

``` purescript
newtype ConversionTaskState
  = ConversionTaskState String
```

#### `CopyFpgaImageRequest`

``` purescript
newtype CopyFpgaImageRequest
  = CopyFpgaImageRequest { "DryRun" :: NullOrUndefined (Boolean), "SourceFpgaImageId" :: String, "Description" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "SourceRegion" :: String, "ClientToken" :: NullOrUndefined (String) }
```

#### `CopyFpgaImageResult`

``` purescript
newtype CopyFpgaImageResult
  = CopyFpgaImageResult { "FpgaImageId" :: NullOrUndefined (String) }
```

#### `CopyImageRequest`

``` purescript
newtype CopyImageRequest
  = CopyImageRequest { "ClientToken" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Encrypted" :: NullOrUndefined (Boolean), "KmsKeyId" :: NullOrUndefined (String), "Name" :: String, "SourceImageId" :: String, "SourceRegion" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CopyImage.</p>

#### `CopyImageResult`

``` purescript
newtype CopyImageResult
  = CopyImageResult { "ImageId" :: NullOrUndefined (String) }
```

<p>Contains the output of CopyImage.</p>

#### `CopySnapshotRequest`

``` purescript
newtype CopySnapshotRequest
  = CopySnapshotRequest { "Description" :: NullOrUndefined (String), "DestinationRegion" :: NullOrUndefined (String), "Encrypted" :: NullOrUndefined (Boolean), "KmsKeyId" :: NullOrUndefined (String), "PresignedUrl" :: NullOrUndefined (String), "SourceRegion" :: String, "SourceSnapshotId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CopySnapshot.</p>

#### `CopySnapshotResult`

``` purescript
newtype CopySnapshotResult
  = CopySnapshotResult { "SnapshotId" :: NullOrUndefined (String) }
```

<p>Contains the output of CopySnapshot.</p>

#### `CreateCustomerGatewayRequest`

``` purescript
newtype CreateCustomerGatewayRequest
  = CreateCustomerGatewayRequest { "BgpAsn" :: Int, "PublicIp" :: String, "Type" :: GatewayType, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CreateCustomerGateway.</p>

#### `CreateCustomerGatewayResult`

``` purescript
newtype CreateCustomerGatewayResult
  = CreateCustomerGatewayResult { "CustomerGateway" :: NullOrUndefined (CustomerGateway) }
```

<p>Contains the output of CreateCustomerGateway.</p>

#### `CreateDefaultSubnetRequest`

``` purescript
newtype CreateDefaultSubnetRequest
  = CreateDefaultSubnetRequest { "AvailabilityZone" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

#### `CreateDefaultSubnetResult`

``` purescript
newtype CreateDefaultSubnetResult
  = CreateDefaultSubnetResult { "Subnet" :: NullOrUndefined (Subnet) }
```

#### `CreateDefaultVpcRequest`

``` purescript
newtype CreateDefaultVpcRequest
  = CreateDefaultVpcRequest { "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CreateDefaultVpc.</p>

#### `CreateDefaultVpcResult`

``` purescript
newtype CreateDefaultVpcResult
  = CreateDefaultVpcResult { "Vpc" :: NullOrUndefined (Vpc) }
```

<p>Contains the output of CreateDefaultVpc.</p>

#### `CreateDhcpOptionsRequest`

``` purescript
newtype CreateDhcpOptionsRequest
  = CreateDhcpOptionsRequest { "DhcpConfigurations" :: NewDhcpConfigurationList, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CreateDhcpOptions.</p>

#### `CreateDhcpOptionsResult`

``` purescript
newtype CreateDhcpOptionsResult
  = CreateDhcpOptionsResult { "DhcpOptions" :: NullOrUndefined (DhcpOptions) }
```

<p>Contains the output of CreateDhcpOptions.</p>

#### `CreateEgressOnlyInternetGatewayRequest`

``` purescript
newtype CreateEgressOnlyInternetGatewayRequest
  = CreateEgressOnlyInternetGatewayRequest { "ClientToken" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "VpcId" :: String }
```

#### `CreateEgressOnlyInternetGatewayResult`

``` purescript
newtype CreateEgressOnlyInternetGatewayResult
  = CreateEgressOnlyInternetGatewayResult { "ClientToken" :: NullOrUndefined (String), "EgressOnlyInternetGateway" :: NullOrUndefined (EgressOnlyInternetGateway) }
```

#### `CreateFlowLogsRequest`

``` purescript
newtype CreateFlowLogsRequest
  = CreateFlowLogsRequest { "ClientToken" :: NullOrUndefined (String), "DeliverLogsPermissionArn" :: String, "LogGroupName" :: String, "ResourceIds" :: ValueStringList, "ResourceType" :: FlowLogsResourceType, "TrafficType" :: TrafficType }
```

<p>Contains the parameters for CreateFlowLogs.</p>

#### `CreateFlowLogsResult`

``` purescript
newtype CreateFlowLogsResult
  = CreateFlowLogsResult { "ClientToken" :: NullOrUndefined (String), "FlowLogIds" :: NullOrUndefined (ValueStringList), "Unsuccessful" :: NullOrUndefined (UnsuccessfulItemSet) }
```

<p>Contains the output of CreateFlowLogs.</p>

#### `CreateFpgaImageRequest`

``` purescript
newtype CreateFpgaImageRequest
  = CreateFpgaImageRequest { "DryRun" :: NullOrUndefined (Boolean), "InputStorageLocation" :: StorageLocation, "LogsStorageLocation" :: NullOrUndefined (StorageLocation), "Description" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "ClientToken" :: NullOrUndefined (String) }
```

#### `CreateFpgaImageResult`

``` purescript
newtype CreateFpgaImageResult
  = CreateFpgaImageResult { "FpgaImageId" :: NullOrUndefined (String), "FpgaImageGlobalId" :: NullOrUndefined (String) }
```

#### `CreateImageRequest`

``` purescript
newtype CreateImageRequest
  = CreateImageRequest { "BlockDeviceMappings" :: NullOrUndefined (BlockDeviceMappingRequestList), "Description" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "InstanceId" :: String, "Name" :: String, "NoReboot" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CreateImage.</p>

#### `CreateImageResult`

``` purescript
newtype CreateImageResult
  = CreateImageResult { "ImageId" :: NullOrUndefined (String) }
```

<p>Contains the output of CreateImage.</p>

#### `CreateInstanceExportTaskRequest`

``` purescript
newtype CreateInstanceExportTaskRequest
  = CreateInstanceExportTaskRequest { "Description" :: NullOrUndefined (String), "ExportToS3Task" :: NullOrUndefined (ExportToS3TaskSpecification), "InstanceId" :: String, "TargetEnvironment" :: NullOrUndefined (ExportEnvironment) }
```

<p>Contains the parameters for CreateInstanceExportTask.</p>

#### `CreateInstanceExportTaskResult`

``` purescript
newtype CreateInstanceExportTaskResult
  = CreateInstanceExportTaskResult { "ExportTask" :: NullOrUndefined (ExportTask) }
```

<p>Contains the output for CreateInstanceExportTask.</p>

#### `CreateInternetGatewayRequest`

``` purescript
newtype CreateInternetGatewayRequest
  = CreateInternetGatewayRequest { "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CreateInternetGateway.</p>

#### `CreateInternetGatewayResult`

``` purescript
newtype CreateInternetGatewayResult
  = CreateInternetGatewayResult { "InternetGateway" :: NullOrUndefined (InternetGateway) }
```

<p>Contains the output of CreateInternetGateway.</p>

#### `CreateKeyPairRequest`

``` purescript
newtype CreateKeyPairRequest
  = CreateKeyPairRequest { "KeyName" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CreateKeyPair.</p>

#### `CreateLaunchTemplateRequest`

``` purescript
newtype CreateLaunchTemplateRequest
  = CreateLaunchTemplateRequest { "DryRun" :: NullOrUndefined (Boolean), "ClientToken" :: NullOrUndefined (String), "LaunchTemplateName" :: LaunchTemplateName, "VersionDescription" :: NullOrUndefined (VersionDescription), "LaunchTemplateData" :: RequestLaunchTemplateData }
```

#### `CreateLaunchTemplateResult`

``` purescript
newtype CreateLaunchTemplateResult
  = CreateLaunchTemplateResult { "LaunchTemplate" :: NullOrUndefined (LaunchTemplate) }
```

#### `CreateLaunchTemplateVersionRequest`

``` purescript
newtype CreateLaunchTemplateVersionRequest
  = CreateLaunchTemplateVersionRequest { "DryRun" :: NullOrUndefined (Boolean), "ClientToken" :: NullOrUndefined (String), "LaunchTemplateId" :: NullOrUndefined (String), "LaunchTemplateName" :: NullOrUndefined (LaunchTemplateName), "SourceVersion" :: NullOrUndefined (String), "VersionDescription" :: NullOrUndefined (VersionDescription), "LaunchTemplateData" :: RequestLaunchTemplateData }
```

#### `CreateLaunchTemplateVersionResult`

``` purescript
newtype CreateLaunchTemplateVersionResult
  = CreateLaunchTemplateVersionResult { "LaunchTemplateVersion" :: NullOrUndefined (LaunchTemplateVersion) }
```

#### `CreateNatGatewayRequest`

``` purescript
newtype CreateNatGatewayRequest
  = CreateNatGatewayRequest { "AllocationId" :: String, "ClientToken" :: NullOrUndefined (String), "SubnetId" :: String }
```

<p>Contains the parameters for CreateNatGateway.</p>

#### `CreateNatGatewayResult`

``` purescript
newtype CreateNatGatewayResult
  = CreateNatGatewayResult { "ClientToken" :: NullOrUndefined (String), "NatGateway" :: NullOrUndefined (NatGateway) }
```

<p>Contains the output of CreateNatGateway.</p>

#### `CreateNetworkAclEntryRequest`

``` purescript
newtype CreateNetworkAclEntryRequest
  = CreateNetworkAclEntryRequest { "CidrBlock" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "Egress" :: Boolean, "IcmpTypeCode" :: NullOrUndefined (IcmpTypeCode), "Ipv6CidrBlock" :: NullOrUndefined (String), "NetworkAclId" :: String, "PortRange" :: NullOrUndefined (PortRange), "Protocol" :: String, "RuleAction" :: RuleAction, "RuleNumber" :: Int }
```

<p>Contains the parameters for CreateNetworkAclEntry.</p>

#### `CreateNetworkAclRequest`

``` purescript
newtype CreateNetworkAclRequest
  = CreateNetworkAclRequest { "DryRun" :: NullOrUndefined (Boolean), "VpcId" :: String }
```

<p>Contains the parameters for CreateNetworkAcl.</p>

#### `CreateNetworkAclResult`

``` purescript
newtype CreateNetworkAclResult
  = CreateNetworkAclResult { "NetworkAcl" :: NullOrUndefined (NetworkAcl) }
```

<p>Contains the output of CreateNetworkAcl.</p>

#### `CreateNetworkInterfacePermissionRequest`

``` purescript
newtype CreateNetworkInterfacePermissionRequest
  = CreateNetworkInterfacePermissionRequest { "NetworkInterfaceId" :: String, "AwsAccountId" :: NullOrUndefined (String), "AwsService" :: NullOrUndefined (String), "Permission" :: InterfacePermissionType, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CreateNetworkInterfacePermission.</p>

#### `CreateNetworkInterfacePermissionResult`

``` purescript
newtype CreateNetworkInterfacePermissionResult
  = CreateNetworkInterfacePermissionResult { "InterfacePermission" :: NullOrUndefined (NetworkInterfacePermission) }
```

<p>Contains the output of CreateNetworkInterfacePermission.</p>

#### `CreateNetworkInterfaceRequest`

``` purescript
newtype CreateNetworkInterfaceRequest
  = CreateNetworkInterfaceRequest { "Description" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (SecurityGroupIdStringList), "Ipv6AddressCount" :: NullOrUndefined (Int), "Ipv6Addresses" :: NullOrUndefined (InstanceIpv6AddressList), "PrivateIpAddress" :: NullOrUndefined (String), "PrivateIpAddresses" :: NullOrUndefined (PrivateIpAddressSpecificationList), "SecondaryPrivateIpAddressCount" :: NullOrUndefined (Int), "SubnetId" :: String }
```

<p>Contains the parameters for CreateNetworkInterface.</p>

#### `CreateNetworkInterfaceResult`

``` purescript
newtype CreateNetworkInterfaceResult
  = CreateNetworkInterfaceResult { "NetworkInterface" :: NullOrUndefined (NetworkInterface) }
```

<p>Contains the output of CreateNetworkInterface.</p>

#### `CreatePlacementGroupRequest`

``` purescript
newtype CreatePlacementGroupRequest
  = CreatePlacementGroupRequest { "DryRun" :: NullOrUndefined (Boolean), "GroupName" :: String, "Strategy" :: PlacementStrategy }
```

<p>Contains the parameters for CreatePlacementGroup.</p>

#### `CreateReservedInstancesListingRequest`

``` purescript
newtype CreateReservedInstancesListingRequest
  = CreateReservedInstancesListingRequest { "ClientToken" :: String, "InstanceCount" :: Int, "PriceSchedules" :: PriceScheduleSpecificationList, "ReservedInstancesId" :: String }
```

<p>Contains the parameters for CreateReservedInstancesListing.</p>

#### `CreateReservedInstancesListingResult`

``` purescript
newtype CreateReservedInstancesListingResult
  = CreateReservedInstancesListingResult { "ReservedInstancesListings" :: NullOrUndefined (ReservedInstancesListingList) }
```

<p>Contains the output of CreateReservedInstancesListing.</p>

#### `CreateRouteRequest`

``` purescript
newtype CreateRouteRequest
  = CreateRouteRequest { "DestinationCidrBlock" :: NullOrUndefined (String), "DestinationIpv6CidrBlock" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "EgressOnlyInternetGatewayId" :: NullOrUndefined (String), "GatewayId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "NatGatewayId" :: NullOrUndefined (String), "NetworkInterfaceId" :: NullOrUndefined (String), "RouteTableId" :: String, "VpcPeeringConnectionId" :: NullOrUndefined (String) }
```

<p>Contains the parameters for CreateRoute.</p>

#### `CreateRouteResult`

``` purescript
newtype CreateRouteResult
  = CreateRouteResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of CreateRoute.</p>

#### `CreateRouteTableRequest`

``` purescript
newtype CreateRouteTableRequest
  = CreateRouteTableRequest { "DryRun" :: NullOrUndefined (Boolean), "VpcId" :: String }
```

<p>Contains the parameters for CreateRouteTable.</p>

#### `CreateRouteTableResult`

``` purescript
newtype CreateRouteTableResult
  = CreateRouteTableResult { "RouteTable" :: NullOrUndefined (RouteTable) }
```

<p>Contains the output of CreateRouteTable.</p>

#### `CreateSecurityGroupRequest`

``` purescript
newtype CreateSecurityGroupRequest
  = CreateSecurityGroupRequest { "Description" :: String, "GroupName" :: String, "VpcId" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CreateSecurityGroup.</p>

#### `CreateSecurityGroupResult`

``` purescript
newtype CreateSecurityGroupResult
  = CreateSecurityGroupResult { "GroupId" :: NullOrUndefined (String) }
```

<p>Contains the output of CreateSecurityGroup.</p>

#### `CreateSnapshotRequest`

``` purescript
newtype CreateSnapshotRequest
  = CreateSnapshotRequest { "Description" :: NullOrUndefined (String), "VolumeId" :: String, "TagSpecifications" :: NullOrUndefined (TagSpecificationList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CreateSnapshot.</p>

#### `CreateSpotDatafeedSubscriptionRequest`

``` purescript
newtype CreateSpotDatafeedSubscriptionRequest
  = CreateSpotDatafeedSubscriptionRequest { "Bucket" :: String, "DryRun" :: NullOrUndefined (Boolean), "Prefix" :: NullOrUndefined (String) }
```

<p>Contains the parameters for CreateSpotDatafeedSubscription.</p>

#### `CreateSpotDatafeedSubscriptionResult`

``` purescript
newtype CreateSpotDatafeedSubscriptionResult
  = CreateSpotDatafeedSubscriptionResult { "SpotDatafeedSubscription" :: NullOrUndefined (SpotDatafeedSubscription) }
```

<p>Contains the output of CreateSpotDatafeedSubscription.</p>

#### `CreateSubnetRequest`

``` purescript
newtype CreateSubnetRequest
  = CreateSubnetRequest { "AvailabilityZone" :: NullOrUndefined (String), "CidrBlock" :: String, "Ipv6CidrBlock" :: NullOrUndefined (String), "VpcId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CreateSubnet.</p>

#### `CreateSubnetResult`

``` purescript
newtype CreateSubnetResult
  = CreateSubnetResult { "Subnet" :: NullOrUndefined (Subnet) }
```

<p>Contains the output of CreateSubnet.</p>

#### `CreateTagsRequest`

``` purescript
newtype CreateTagsRequest
  = CreateTagsRequest { "DryRun" :: NullOrUndefined (Boolean), "Resources" :: ResourceIdList, "Tags" :: TagList }
```

<p>Contains the parameters for CreateTags.</p>

#### `CreateVolumePermission`

``` purescript
newtype CreateVolumePermission
  = CreateVolumePermission { "Group" :: NullOrUndefined (PermissionGroup), "UserId" :: NullOrUndefined (String) }
```

<p>Describes the user or group to be added or removed from the permissions for a volume.</p>

#### `CreateVolumePermissionList`

``` purescript
newtype CreateVolumePermissionList
  = CreateVolumePermissionList (Array CreateVolumePermission)
```

#### `CreateVolumePermissionModifications`

``` purescript
newtype CreateVolumePermissionModifications
  = CreateVolumePermissionModifications { "Add" :: NullOrUndefined (CreateVolumePermissionList), "Remove" :: NullOrUndefined (CreateVolumePermissionList) }
```

<p>Describes modifications to the permissions for a volume.</p>

#### `CreateVolumeRequest`

``` purescript
newtype CreateVolumeRequest
  = CreateVolumeRequest { "AvailabilityZone" :: String, "Encrypted" :: NullOrUndefined (Boolean), "Iops" :: NullOrUndefined (Int), "KmsKeyId" :: NullOrUndefined (String), "Size" :: NullOrUndefined (Int), "SnapshotId" :: NullOrUndefined (String), "VolumeType" :: NullOrUndefined (VolumeType), "DryRun" :: NullOrUndefined (Boolean), "TagSpecifications" :: NullOrUndefined (TagSpecificationList) }
```

<p>Contains the parameters for CreateVolume.</p>

#### `CreateVpcEndpointConnectionNotificationRequest`

``` purescript
newtype CreateVpcEndpointConnectionNotificationRequest
  = CreateVpcEndpointConnectionNotificationRequest { "DryRun" :: NullOrUndefined (Boolean), "ServiceId" :: NullOrUndefined (String), "VpcEndpointId" :: NullOrUndefined (String), "ConnectionNotificationArn" :: String, "ConnectionEvents" :: ValueStringList, "ClientToken" :: NullOrUndefined (String) }
```

#### `CreateVpcEndpointConnectionNotificationResult`

``` purescript
newtype CreateVpcEndpointConnectionNotificationResult
  = CreateVpcEndpointConnectionNotificationResult { "ConnectionNotification" :: NullOrUndefined (ConnectionNotification), "ClientToken" :: NullOrUndefined (String) }
```

#### `CreateVpcEndpointRequest`

``` purescript
newtype CreateVpcEndpointRequest
  = CreateVpcEndpointRequest { "DryRun" :: NullOrUndefined (Boolean), "VpcEndpointType" :: NullOrUndefined (VpcEndpointType), "VpcId" :: String, "ServiceName" :: String, "PolicyDocument" :: NullOrUndefined (String), "RouteTableIds" :: NullOrUndefined (ValueStringList), "SubnetIds" :: NullOrUndefined (ValueStringList), "SecurityGroupIds" :: NullOrUndefined (ValueStringList), "ClientToken" :: NullOrUndefined (String), "PrivateDnsEnabled" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CreateVpcEndpoint.</p>

#### `CreateVpcEndpointResult`

``` purescript
newtype CreateVpcEndpointResult
  = CreateVpcEndpointResult { "VpcEndpoint" :: NullOrUndefined (VpcEndpoint), "ClientToken" :: NullOrUndefined (String) }
```

<p>Contains the output of CreateVpcEndpoint.</p>

#### `CreateVpcEndpointServiceConfigurationRequest`

``` purescript
newtype CreateVpcEndpointServiceConfigurationRequest
  = CreateVpcEndpointServiceConfigurationRequest { "DryRun" :: NullOrUndefined (Boolean), "AcceptanceRequired" :: NullOrUndefined (Boolean), "NetworkLoadBalancerArns" :: ValueStringList, "ClientToken" :: NullOrUndefined (String) }
```

#### `CreateVpcEndpointServiceConfigurationResult`

``` purescript
newtype CreateVpcEndpointServiceConfigurationResult
  = CreateVpcEndpointServiceConfigurationResult { "ServiceConfiguration" :: NullOrUndefined (ServiceConfiguration), "ClientToken" :: NullOrUndefined (String) }
```

#### `CreateVpcPeeringConnectionRequest`

``` purescript
newtype CreateVpcPeeringConnectionRequest
  = CreateVpcPeeringConnectionRequest { "DryRun" :: NullOrUndefined (Boolean), "PeerOwnerId" :: NullOrUndefined (String), "PeerVpcId" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "PeerRegion" :: NullOrUndefined (String) }
```

<p>Contains the parameters for CreateVpcPeeringConnection.</p>

#### `CreateVpcPeeringConnectionResult`

``` purescript
newtype CreateVpcPeeringConnectionResult
  = CreateVpcPeeringConnectionResult { "VpcPeeringConnection" :: NullOrUndefined (VpcPeeringConnection) }
```

<p>Contains the output of CreateVpcPeeringConnection.</p>

#### `CreateVpcRequest`

``` purescript
newtype CreateVpcRequest
  = CreateVpcRequest { "CidrBlock" :: String, "AmazonProvidedIpv6CidrBlock" :: NullOrUndefined (Boolean), "DryRun" :: NullOrUndefined (Boolean), "InstanceTenancy" :: NullOrUndefined (Tenancy) }
```

<p>Contains the parameters for CreateVpc.</p>

#### `CreateVpcResult`

``` purescript
newtype CreateVpcResult
  = CreateVpcResult { "Vpc" :: NullOrUndefined (Vpc) }
```

<p>Contains the output of CreateVpc.</p>

#### `CreateVpnConnectionRequest`

``` purescript
newtype CreateVpnConnectionRequest
  = CreateVpnConnectionRequest { "CustomerGatewayId" :: String, "Type" :: String, "VpnGatewayId" :: String, "DryRun" :: NullOrUndefined (Boolean), "Options" :: NullOrUndefined (VpnConnectionOptionsSpecification) }
```

<p>Contains the parameters for CreateVpnConnection.</p>

#### `CreateVpnConnectionResult`

``` purescript
newtype CreateVpnConnectionResult
  = CreateVpnConnectionResult { "VpnConnection" :: NullOrUndefined (VpnConnection) }
```

<p>Contains the output of CreateVpnConnection.</p>

#### `CreateVpnConnectionRouteRequest`

``` purescript
newtype CreateVpnConnectionRouteRequest
  = CreateVpnConnectionRouteRequest { "DestinationCidrBlock" :: String, "VpnConnectionId" :: String }
```

<p>Contains the parameters for CreateVpnConnectionRoute.</p>

#### `CreateVpnGatewayRequest`

``` purescript
newtype CreateVpnGatewayRequest
  = CreateVpnGatewayRequest { "AvailabilityZone" :: NullOrUndefined (String), "Type" :: GatewayType, "AmazonSideAsn" :: NullOrUndefined (Number), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for CreateVpnGateway.</p>

#### `CreateVpnGatewayResult`

``` purescript
newtype CreateVpnGatewayResult
  = CreateVpnGatewayResult { "VpnGateway" :: NullOrUndefined (VpnGateway) }
```

<p>Contains the output of CreateVpnGateway.</p>

#### `CreditSpecification`

``` purescript
newtype CreditSpecification
  = CreditSpecification { "CpuCredits" :: NullOrUndefined (String) }
```

<p>Describes the credit option for CPU usage of a T2 instance.</p>

#### `CreditSpecificationRequest`

``` purescript
newtype CreditSpecificationRequest
  = CreditSpecificationRequest { "CpuCredits" :: String }
```

<p>The credit option for CPU usage of a T2 instance.</p>

#### `CurrencyCodeValues`

``` purescript
newtype CurrencyCodeValues
  = CurrencyCodeValues String
```

#### `CustomerGateway`

``` purescript
newtype CustomerGateway
  = CustomerGateway { "BgpAsn" :: NullOrUndefined (String), "CustomerGatewayId" :: NullOrUndefined (String), "IpAddress" :: NullOrUndefined (String), "State" :: NullOrUndefined (String), "Type" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a customer gateway.</p>

#### `CustomerGatewayIdStringList`

``` purescript
newtype CustomerGatewayIdStringList
  = CustomerGatewayIdStringList (Array String)
```

#### `CustomerGatewayList`

``` purescript
newtype CustomerGatewayList
  = CustomerGatewayList (Array CustomerGateway)
```

#### `DatafeedSubscriptionState`

``` purescript
newtype DatafeedSubscriptionState
  = DatafeedSubscriptionState String
```

#### `DateTime`

``` purescript
newtype DateTime
  = DateTime Number
```

#### `DeleteCustomerGatewayRequest`

``` purescript
newtype DeleteCustomerGatewayRequest
  = DeleteCustomerGatewayRequest { "CustomerGatewayId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeleteCustomerGateway.</p>

#### `DeleteDhcpOptionsRequest`

``` purescript
newtype DeleteDhcpOptionsRequest
  = DeleteDhcpOptionsRequest { "DhcpOptionsId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeleteDhcpOptions.</p>

#### `DeleteEgressOnlyInternetGatewayRequest`

``` purescript
newtype DeleteEgressOnlyInternetGatewayRequest
  = DeleteEgressOnlyInternetGatewayRequest { "DryRun" :: NullOrUndefined (Boolean), "EgressOnlyInternetGatewayId" :: EgressOnlyInternetGatewayId }
```

#### `DeleteEgressOnlyInternetGatewayResult`

``` purescript
newtype DeleteEgressOnlyInternetGatewayResult
  = DeleteEgressOnlyInternetGatewayResult { "ReturnCode" :: NullOrUndefined (Boolean) }
```

#### `DeleteFlowLogsRequest`

``` purescript
newtype DeleteFlowLogsRequest
  = DeleteFlowLogsRequest { "FlowLogIds" :: ValueStringList }
```

<p>Contains the parameters for DeleteFlowLogs.</p>

#### `DeleteFlowLogsResult`

``` purescript
newtype DeleteFlowLogsResult
  = DeleteFlowLogsResult { "Unsuccessful" :: NullOrUndefined (UnsuccessfulItemSet) }
```

<p>Contains the output of DeleteFlowLogs.</p>

#### `DeleteFpgaImageRequest`

``` purescript
newtype DeleteFpgaImageRequest
  = DeleteFpgaImageRequest { "DryRun" :: NullOrUndefined (Boolean), "FpgaImageId" :: String }
```

#### `DeleteFpgaImageResult`

``` purescript
newtype DeleteFpgaImageResult
  = DeleteFpgaImageResult { "Return" :: NullOrUndefined (Boolean) }
```

#### `DeleteInternetGatewayRequest`

``` purescript
newtype DeleteInternetGatewayRequest
  = DeleteInternetGatewayRequest { "DryRun" :: NullOrUndefined (Boolean), "InternetGatewayId" :: String }
```

<p>Contains the parameters for DeleteInternetGateway.</p>

#### `DeleteKeyPairRequest`

``` purescript
newtype DeleteKeyPairRequest
  = DeleteKeyPairRequest { "KeyName" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeleteKeyPair.</p>

#### `DeleteLaunchTemplateRequest`

``` purescript
newtype DeleteLaunchTemplateRequest
  = DeleteLaunchTemplateRequest { "DryRun" :: NullOrUndefined (Boolean), "LaunchTemplateId" :: NullOrUndefined (String), "LaunchTemplateName" :: NullOrUndefined (LaunchTemplateName) }
```

#### `DeleteLaunchTemplateResult`

``` purescript
newtype DeleteLaunchTemplateResult
  = DeleteLaunchTemplateResult { "LaunchTemplate" :: NullOrUndefined (LaunchTemplate) }
```

#### `DeleteLaunchTemplateVersionsRequest`

``` purescript
newtype DeleteLaunchTemplateVersionsRequest
  = DeleteLaunchTemplateVersionsRequest { "DryRun" :: NullOrUndefined (Boolean), "LaunchTemplateId" :: NullOrUndefined (String), "LaunchTemplateName" :: NullOrUndefined (LaunchTemplateName), "Versions" :: VersionStringList }
```

#### `DeleteLaunchTemplateVersionsResponseErrorItem`

``` purescript
newtype DeleteLaunchTemplateVersionsResponseErrorItem
  = DeleteLaunchTemplateVersionsResponseErrorItem { "LaunchTemplateId" :: NullOrUndefined (String), "LaunchTemplateName" :: NullOrUndefined (String), "VersionNumber" :: NullOrUndefined (Number), "ResponseError" :: NullOrUndefined (ResponseError) }
```

<p>Describes a launch template version that could not be deleted.</p>

#### `DeleteLaunchTemplateVersionsResponseErrorSet`

``` purescript
newtype DeleteLaunchTemplateVersionsResponseErrorSet
  = DeleteLaunchTemplateVersionsResponseErrorSet (Array DeleteLaunchTemplateVersionsResponseErrorItem)
```

#### `DeleteLaunchTemplateVersionsResponseSuccessItem`

``` purescript
newtype DeleteLaunchTemplateVersionsResponseSuccessItem
  = DeleteLaunchTemplateVersionsResponseSuccessItem { "LaunchTemplateId" :: NullOrUndefined (String), "LaunchTemplateName" :: NullOrUndefined (String), "VersionNumber" :: NullOrUndefined (Number) }
```

<p>Describes a launch template version that was successfully deleted.</p>

#### `DeleteLaunchTemplateVersionsResponseSuccessSet`

``` purescript
newtype DeleteLaunchTemplateVersionsResponseSuccessSet
  = DeleteLaunchTemplateVersionsResponseSuccessSet (Array DeleteLaunchTemplateVersionsResponseSuccessItem)
```

#### `DeleteLaunchTemplateVersionsResult`

``` purescript
newtype DeleteLaunchTemplateVersionsResult
  = DeleteLaunchTemplateVersionsResult { "SuccessfullyDeletedLaunchTemplateVersions" :: NullOrUndefined (DeleteLaunchTemplateVersionsResponseSuccessSet), "UnsuccessfullyDeletedLaunchTemplateVersions" :: NullOrUndefined (DeleteLaunchTemplateVersionsResponseErrorSet) }
```

#### `DeleteNatGatewayRequest`

``` purescript
newtype DeleteNatGatewayRequest
  = DeleteNatGatewayRequest { "NatGatewayId" :: String }
```

<p>Contains the parameters for DeleteNatGateway.</p>

#### `DeleteNatGatewayResult`

``` purescript
newtype DeleteNatGatewayResult
  = DeleteNatGatewayResult { "NatGatewayId" :: NullOrUndefined (String) }
```

<p>Contains the output of DeleteNatGateway.</p>

#### `DeleteNetworkAclEntryRequest`

``` purescript
newtype DeleteNetworkAclEntryRequest
  = DeleteNetworkAclEntryRequest { "DryRun" :: NullOrUndefined (Boolean), "Egress" :: Boolean, "NetworkAclId" :: String, "RuleNumber" :: Int }
```

<p>Contains the parameters for DeleteNetworkAclEntry.</p>

#### `DeleteNetworkAclRequest`

``` purescript
newtype DeleteNetworkAclRequest
  = DeleteNetworkAclRequest { "DryRun" :: NullOrUndefined (Boolean), "NetworkAclId" :: String }
```

<p>Contains the parameters for DeleteNetworkAcl.</p>

#### `DeleteNetworkInterfacePermissionRequest`

``` purescript
newtype DeleteNetworkInterfacePermissionRequest
  = DeleteNetworkInterfacePermissionRequest { "NetworkInterfacePermissionId" :: String, "Force" :: NullOrUndefined (Boolean), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeleteNetworkInterfacePermission.</p>

#### `DeleteNetworkInterfacePermissionResult`

``` purescript
newtype DeleteNetworkInterfacePermissionResult
  = DeleteNetworkInterfacePermissionResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output for DeleteNetworkInterfacePermission.</p>

#### `DeleteNetworkInterfaceRequest`

``` purescript
newtype DeleteNetworkInterfaceRequest
  = DeleteNetworkInterfaceRequest { "DryRun" :: NullOrUndefined (Boolean), "NetworkInterfaceId" :: String }
```

<p>Contains the parameters for DeleteNetworkInterface.</p>

#### `DeletePlacementGroupRequest`

``` purescript
newtype DeletePlacementGroupRequest
  = DeletePlacementGroupRequest { "DryRun" :: NullOrUndefined (Boolean), "GroupName" :: String }
```

<p>Contains the parameters for DeletePlacementGroup.</p>

#### `DeleteRouteRequest`

``` purescript
newtype DeleteRouteRequest
  = DeleteRouteRequest { "DestinationCidrBlock" :: NullOrUndefined (String), "DestinationIpv6CidrBlock" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "RouteTableId" :: String }
```

<p>Contains the parameters for DeleteRoute.</p>

#### `DeleteRouteTableRequest`

``` purescript
newtype DeleteRouteTableRequest
  = DeleteRouteTableRequest { "DryRun" :: NullOrUndefined (Boolean), "RouteTableId" :: String }
```

<p>Contains the parameters for DeleteRouteTable.</p>

#### `DeleteSecurityGroupRequest`

``` purescript
newtype DeleteSecurityGroupRequest
  = DeleteSecurityGroupRequest { "GroupId" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeleteSecurityGroup.</p>

#### `DeleteSnapshotRequest`

``` purescript
newtype DeleteSnapshotRequest
  = DeleteSnapshotRequest { "SnapshotId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeleteSnapshot.</p>

#### `DeleteSpotDatafeedSubscriptionRequest`

``` purescript
newtype DeleteSpotDatafeedSubscriptionRequest
  = DeleteSpotDatafeedSubscriptionRequest { "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeleteSpotDatafeedSubscription.</p>

#### `DeleteSubnetRequest`

``` purescript
newtype DeleteSubnetRequest
  = DeleteSubnetRequest { "SubnetId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeleteSubnet.</p>

#### `DeleteTagsRequest`

``` purescript
newtype DeleteTagsRequest
  = DeleteTagsRequest { "DryRun" :: NullOrUndefined (Boolean), "Resources" :: ResourceIdList, "Tags" :: NullOrUndefined (TagList) }
```

<p>Contains the parameters for DeleteTags.</p>

#### `DeleteVolumeRequest`

``` purescript
newtype DeleteVolumeRequest
  = DeleteVolumeRequest { "VolumeId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeleteVolume.</p>

#### `DeleteVpcEndpointConnectionNotificationsRequest`

``` purescript
newtype DeleteVpcEndpointConnectionNotificationsRequest
  = DeleteVpcEndpointConnectionNotificationsRequest { "DryRun" :: NullOrUndefined (Boolean), "ConnectionNotificationIds" :: ValueStringList }
```

#### `DeleteVpcEndpointConnectionNotificationsResult`

``` purescript
newtype DeleteVpcEndpointConnectionNotificationsResult
  = DeleteVpcEndpointConnectionNotificationsResult { "Unsuccessful" :: NullOrUndefined (UnsuccessfulItemSet) }
```

#### `DeleteVpcEndpointServiceConfigurationsRequest`

``` purescript
newtype DeleteVpcEndpointServiceConfigurationsRequest
  = DeleteVpcEndpointServiceConfigurationsRequest { "DryRun" :: NullOrUndefined (Boolean), "ServiceIds" :: ValueStringList }
```

#### `DeleteVpcEndpointServiceConfigurationsResult`

``` purescript
newtype DeleteVpcEndpointServiceConfigurationsResult
  = DeleteVpcEndpointServiceConfigurationsResult { "Unsuccessful" :: NullOrUndefined (UnsuccessfulItemSet) }
```

#### `DeleteVpcEndpointsRequest`

``` purescript
newtype DeleteVpcEndpointsRequest
  = DeleteVpcEndpointsRequest { "DryRun" :: NullOrUndefined (Boolean), "VpcEndpointIds" :: ValueStringList }
```

<p>Contains the parameters for DeleteVpcEndpoints.</p>

#### `DeleteVpcEndpointsResult`

``` purescript
newtype DeleteVpcEndpointsResult
  = DeleteVpcEndpointsResult { "Unsuccessful" :: NullOrUndefined (UnsuccessfulItemSet) }
```

<p>Contains the output of DeleteVpcEndpoints.</p>

#### `DeleteVpcPeeringConnectionRequest`

``` purescript
newtype DeleteVpcPeeringConnectionRequest
  = DeleteVpcPeeringConnectionRequest { "DryRun" :: NullOrUndefined (Boolean), "VpcPeeringConnectionId" :: String }
```

<p>Contains the parameters for DeleteVpcPeeringConnection.</p>

#### `DeleteVpcPeeringConnectionResult`

``` purescript
newtype DeleteVpcPeeringConnectionResult
  = DeleteVpcPeeringConnectionResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of DeleteVpcPeeringConnection.</p>

#### `DeleteVpcRequest`

``` purescript
newtype DeleteVpcRequest
  = DeleteVpcRequest { "VpcId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeleteVpc.</p>

#### `DeleteVpnConnectionRequest`

``` purescript
newtype DeleteVpnConnectionRequest
  = DeleteVpnConnectionRequest { "VpnConnectionId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeleteVpnConnection.</p>

#### `DeleteVpnConnectionRouteRequest`

``` purescript
newtype DeleteVpnConnectionRouteRequest
  = DeleteVpnConnectionRouteRequest { "DestinationCidrBlock" :: String, "VpnConnectionId" :: String }
```

<p>Contains the parameters for DeleteVpnConnectionRoute.</p>

#### `DeleteVpnGatewayRequest`

``` purescript
newtype DeleteVpnGatewayRequest
  = DeleteVpnGatewayRequest { "VpnGatewayId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeleteVpnGateway.</p>

#### `DeregisterImageRequest`

``` purescript
newtype DeregisterImageRequest
  = DeregisterImageRequest { "ImageId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DeregisterImage.</p>

#### `DescribeAccountAttributesRequest`

``` purescript
newtype DescribeAccountAttributesRequest
  = DescribeAccountAttributesRequest { "AttributeNames" :: NullOrUndefined (AccountAttributeNameStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeAccountAttributes.</p>

#### `DescribeAccountAttributesResult`

``` purescript
newtype DescribeAccountAttributesResult
  = DescribeAccountAttributesResult { "AccountAttributes" :: NullOrUndefined (AccountAttributeList) }
```

<p>Contains the output of DescribeAccountAttributes.</p>

#### `DescribeAddressesRequest`

``` purescript
newtype DescribeAddressesRequest
  = DescribeAddressesRequest { "Filters" :: NullOrUndefined (FilterList), "PublicIps" :: NullOrUndefined (PublicIpStringList), "AllocationIds" :: NullOrUndefined (AllocationIdList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeAddresses.</p>

#### `DescribeAddressesResult`

``` purescript
newtype DescribeAddressesResult
  = DescribeAddressesResult { "Addresses" :: NullOrUndefined (AddressList) }
```

<p>Contains the output of DescribeAddresses.</p>

#### `DescribeAggregateIdFormatRequest`

``` purescript
newtype DescribeAggregateIdFormatRequest
  = DescribeAggregateIdFormatRequest { "DryRun" :: NullOrUndefined (Boolean) }
```

#### `DescribeAggregateIdFormatResult`

``` purescript
newtype DescribeAggregateIdFormatResult
  = DescribeAggregateIdFormatResult { "UseLongIdsAggregated" :: NullOrUndefined (Boolean), "Statuses" :: NullOrUndefined (IdFormatList) }
```

#### `DescribeAvailabilityZonesRequest`

``` purescript
newtype DescribeAvailabilityZonesRequest
  = DescribeAvailabilityZonesRequest { "Filters" :: NullOrUndefined (FilterList), "ZoneNames" :: NullOrUndefined (ZoneNameStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeAvailabilityZones.</p>

#### `DescribeAvailabilityZonesResult`

``` purescript
newtype DescribeAvailabilityZonesResult
  = DescribeAvailabilityZonesResult { "AvailabilityZones" :: NullOrUndefined (AvailabilityZoneList) }
```

<p>Contains the output of DescribeAvailabiltyZones.</p>

#### `DescribeBundleTasksRequest`

``` purescript
newtype DescribeBundleTasksRequest
  = DescribeBundleTasksRequest { "BundleIds" :: NullOrUndefined (BundleIdStringList), "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeBundleTasks.</p>

#### `DescribeBundleTasksResult`

``` purescript
newtype DescribeBundleTasksResult
  = DescribeBundleTasksResult { "BundleTasks" :: NullOrUndefined (BundleTaskList) }
```

<p>Contains the output of DescribeBundleTasks.</p>

#### `DescribeClassicLinkInstancesRequest`

``` purescript
newtype DescribeClassicLinkInstancesRequest
  = DescribeClassicLinkInstancesRequest { "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean), "InstanceIds" :: NullOrUndefined (InstanceIdStringList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeClassicLinkInstances.</p>

#### `DescribeClassicLinkInstancesResult`

``` purescript
newtype DescribeClassicLinkInstancesResult
  = DescribeClassicLinkInstancesResult { "Instances" :: NullOrUndefined (ClassicLinkInstanceList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeClassicLinkInstances.</p>

#### `DescribeConversionTaskList`

``` purescript
newtype DescribeConversionTaskList
  = DescribeConversionTaskList (Array ConversionTask)
```

#### `DescribeConversionTasksRequest`

``` purescript
newtype DescribeConversionTasksRequest
  = DescribeConversionTasksRequest { "ConversionTaskIds" :: NullOrUndefined (ConversionIdStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeConversionTasks.</p>

#### `DescribeConversionTasksResult`

``` purescript
newtype DescribeConversionTasksResult
  = DescribeConversionTasksResult { "ConversionTasks" :: NullOrUndefined (DescribeConversionTaskList) }
```

<p>Contains the output for DescribeConversionTasks.</p>

#### `DescribeCustomerGatewaysRequest`

``` purescript
newtype DescribeCustomerGatewaysRequest
  = DescribeCustomerGatewaysRequest { "CustomerGatewayIds" :: NullOrUndefined (CustomerGatewayIdStringList), "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeCustomerGateways.</p>

#### `DescribeCustomerGatewaysResult`

``` purescript
newtype DescribeCustomerGatewaysResult
  = DescribeCustomerGatewaysResult { "CustomerGateways" :: NullOrUndefined (CustomerGatewayList) }
```

<p>Contains the output of DescribeCustomerGateways.</p>

#### `DescribeDhcpOptionsRequest`

``` purescript
newtype DescribeDhcpOptionsRequest
  = DescribeDhcpOptionsRequest { "DhcpOptionsIds" :: NullOrUndefined (DhcpOptionsIdStringList), "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeDhcpOptions.</p>

#### `DescribeDhcpOptionsResult`

``` purescript
newtype DescribeDhcpOptionsResult
  = DescribeDhcpOptionsResult { "DhcpOptions" :: NullOrUndefined (DhcpOptionsList) }
```

<p>Contains the output of DescribeDhcpOptions.</p>

#### `DescribeEgressOnlyInternetGatewaysRequest`

``` purescript
newtype DescribeEgressOnlyInternetGatewaysRequest
  = DescribeEgressOnlyInternetGatewaysRequest { "DryRun" :: NullOrUndefined (Boolean), "EgressOnlyInternetGatewayIds" :: NullOrUndefined (EgressOnlyInternetGatewayIdList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeEgressOnlyInternetGatewaysResult`

``` purescript
newtype DescribeEgressOnlyInternetGatewaysResult
  = DescribeEgressOnlyInternetGatewaysResult { "EgressOnlyInternetGateways" :: NullOrUndefined (EgressOnlyInternetGatewayList), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeElasticGpusRequest`

``` purescript
newtype DescribeElasticGpusRequest
  = DescribeElasticGpusRequest { "ElasticGpuIds" :: NullOrUndefined (ElasticGpuIdSet), "DryRun" :: NullOrUndefined (Boolean), "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeElasticGpusResult`

``` purescript
newtype DescribeElasticGpusResult
  = DescribeElasticGpusResult { "ElasticGpuSet" :: NullOrUndefined (ElasticGpuSet), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeExportTasksRequest`

``` purescript
newtype DescribeExportTasksRequest
  = DescribeExportTasksRequest { "ExportTaskIds" :: NullOrUndefined (ExportTaskIdStringList) }
```

<p>Contains the parameters for DescribeExportTasks.</p>

#### `DescribeExportTasksResult`

``` purescript
newtype DescribeExportTasksResult
  = DescribeExportTasksResult { "ExportTasks" :: NullOrUndefined (ExportTaskList) }
```

<p>Contains the output for DescribeExportTasks.</p>

#### `DescribeFlowLogsRequest`

``` purescript
newtype DescribeFlowLogsRequest
  = DescribeFlowLogsRequest { "Filter" :: NullOrUndefined (FilterList), "FlowLogIds" :: NullOrUndefined (ValueStringList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeFlowLogs.</p>

#### `DescribeFlowLogsResult`

``` purescript
newtype DescribeFlowLogsResult
  = DescribeFlowLogsResult { "FlowLogs" :: NullOrUndefined (FlowLogSet), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeFlowLogs.</p>

#### `DescribeFpgaImageAttributeRequest`

``` purescript
newtype DescribeFpgaImageAttributeRequest
  = DescribeFpgaImageAttributeRequest { "DryRun" :: NullOrUndefined (Boolean), "FpgaImageId" :: String, "Attribute" :: FpgaImageAttributeName }
```

#### `DescribeFpgaImageAttributeResult`

``` purescript
newtype DescribeFpgaImageAttributeResult
  = DescribeFpgaImageAttributeResult { "FpgaImageAttribute" :: NullOrUndefined (FpgaImageAttribute) }
```

#### `DescribeFpgaImagesRequest`

``` purescript
newtype DescribeFpgaImagesRequest
  = DescribeFpgaImagesRequest { "DryRun" :: NullOrUndefined (Boolean), "FpgaImageIds" :: NullOrUndefined (FpgaImageIdList), "Owners" :: NullOrUndefined (OwnerStringList), "Filters" :: NullOrUndefined (FilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

#### `DescribeFpgaImagesResult`

``` purescript
newtype DescribeFpgaImagesResult
  = DescribeFpgaImagesResult { "FpgaImages" :: NullOrUndefined (FpgaImageList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeHostReservationOfferingsRequest`

``` purescript
newtype DescribeHostReservationOfferingsRequest
  = DescribeHostReservationOfferingsRequest { "Filter" :: NullOrUndefined (FilterList), "MaxDuration" :: NullOrUndefined (Int), "MaxResults" :: NullOrUndefined (Int), "MinDuration" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "OfferingId" :: NullOrUndefined (String) }
```

#### `DescribeHostReservationOfferingsResult`

``` purescript
newtype DescribeHostReservationOfferingsResult
  = DescribeHostReservationOfferingsResult { "NextToken" :: NullOrUndefined (String), "OfferingSet" :: NullOrUndefined (HostOfferingSet) }
```

#### `DescribeHostReservationsRequest`

``` purescript
newtype DescribeHostReservationsRequest
  = DescribeHostReservationsRequest { "Filter" :: NullOrUndefined (FilterList), "HostReservationIdSet" :: NullOrUndefined (HostReservationIdSet), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeHostReservationsResult`

``` purescript
newtype DescribeHostReservationsResult
  = DescribeHostReservationsResult { "HostReservationSet" :: NullOrUndefined (HostReservationSet), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeHostsRequest`

``` purescript
newtype DescribeHostsRequest
  = DescribeHostsRequest { "Filter" :: NullOrUndefined (FilterList), "HostIds" :: NullOrUndefined (RequestHostIdList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeHosts.</p>

#### `DescribeHostsResult`

``` purescript
newtype DescribeHostsResult
  = DescribeHostsResult { "Hosts" :: NullOrUndefined (HostList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeHosts.</p>

#### `DescribeIamInstanceProfileAssociationsRequest`

``` purescript
newtype DescribeIamInstanceProfileAssociationsRequest
  = DescribeIamInstanceProfileAssociationsRequest { "AssociationIds" :: NullOrUndefined (AssociationIdList), "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeIamInstanceProfileAssociationsResult`

``` purescript
newtype DescribeIamInstanceProfileAssociationsResult
  = DescribeIamInstanceProfileAssociationsResult { "IamInstanceProfileAssociations" :: NullOrUndefined (IamInstanceProfileAssociationSet), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeIdFormatRequest`

``` purescript
newtype DescribeIdFormatRequest
  = DescribeIdFormatRequest { "Resource" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeIdFormat.</p>

#### `DescribeIdFormatResult`

``` purescript
newtype DescribeIdFormatResult
  = DescribeIdFormatResult { "Statuses" :: NullOrUndefined (IdFormatList) }
```

<p>Contains the output of DescribeIdFormat.</p>

#### `DescribeIdentityIdFormatRequest`

``` purescript
newtype DescribeIdentityIdFormatRequest
  = DescribeIdentityIdFormatRequest { "PrincipalArn" :: String, "Resource" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeIdentityIdFormat.</p>

#### `DescribeIdentityIdFormatResult`

``` purescript
newtype DescribeIdentityIdFormatResult
  = DescribeIdentityIdFormatResult { "Statuses" :: NullOrUndefined (IdFormatList) }
```

<p>Contains the output of DescribeIdentityIdFormat.</p>

#### `DescribeImageAttributeRequest`

``` purescript
newtype DescribeImageAttributeRequest
  = DescribeImageAttributeRequest { "Attribute" :: ImageAttributeName, "ImageId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeImageAttribute.</p>

#### `DescribeImagesRequest`

``` purescript
newtype DescribeImagesRequest
  = DescribeImagesRequest { "ExecutableUsers" :: NullOrUndefined (ExecutableByStringList), "Filters" :: NullOrUndefined (FilterList), "ImageIds" :: NullOrUndefined (ImageIdStringList), "Owners" :: NullOrUndefined (OwnerStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeImages.</p>

#### `DescribeImagesResult`

``` purescript
newtype DescribeImagesResult
  = DescribeImagesResult { "Images" :: NullOrUndefined (ImageList) }
```

<p>Contains the output of DescribeImages.</p>

#### `DescribeImportImageTasksRequest`

``` purescript
newtype DescribeImportImageTasksRequest
  = DescribeImportImageTasksRequest { "DryRun" :: NullOrUndefined (Boolean), "Filters" :: NullOrUndefined (FilterList), "ImportTaskIds" :: NullOrUndefined (ImportTaskIdList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeImportImageTasks.</p>

#### `DescribeImportImageTasksResult`

``` purescript
newtype DescribeImportImageTasksResult
  = DescribeImportImageTasksResult { "ImportImageTasks" :: NullOrUndefined (ImportImageTaskList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output for DescribeImportImageTasks.</p>

#### `DescribeImportSnapshotTasksRequest`

``` purescript
newtype DescribeImportSnapshotTasksRequest
  = DescribeImportSnapshotTasksRequest { "DryRun" :: NullOrUndefined (Boolean), "Filters" :: NullOrUndefined (FilterList), "ImportTaskIds" :: NullOrUndefined (ImportTaskIdList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeImportSnapshotTasks.</p>

#### `DescribeImportSnapshotTasksResult`

``` purescript
newtype DescribeImportSnapshotTasksResult
  = DescribeImportSnapshotTasksResult { "ImportSnapshotTasks" :: NullOrUndefined (ImportSnapshotTaskList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output for DescribeImportSnapshotTasks.</p>

#### `DescribeInstanceAttributeRequest`

``` purescript
newtype DescribeInstanceAttributeRequest
  = DescribeInstanceAttributeRequest { "Attribute" :: InstanceAttributeName, "DryRun" :: NullOrUndefined (Boolean), "InstanceId" :: String }
```

<p>Contains the parameters for DescribeInstanceAttribute.</p>

#### `DescribeInstanceCreditSpecificationsRequest`

``` purescript
newtype DescribeInstanceCreditSpecificationsRequest
  = DescribeInstanceCreditSpecificationsRequest { "DryRun" :: NullOrUndefined (Boolean), "Filters" :: NullOrUndefined (FilterList), "InstanceIds" :: NullOrUndefined (InstanceIdStringList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeInstanceCreditSpecificationsResult`

``` purescript
newtype DescribeInstanceCreditSpecificationsResult
  = DescribeInstanceCreditSpecificationsResult { "InstanceCreditSpecifications" :: NullOrUndefined (InstanceCreditSpecificationList), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeInstanceStatusRequest`

``` purescript
newtype DescribeInstanceStatusRequest
  = DescribeInstanceStatusRequest { "Filters" :: NullOrUndefined (FilterList), "InstanceIds" :: NullOrUndefined (InstanceIdStringList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "IncludeAllInstances" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeInstanceStatus.</p>

#### `DescribeInstanceStatusResult`

``` purescript
newtype DescribeInstanceStatusResult
  = DescribeInstanceStatusResult { "InstanceStatuses" :: NullOrUndefined (InstanceStatusList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeInstanceStatus.</p>

#### `DescribeInstancesRequest`

``` purescript
newtype DescribeInstancesRequest
  = DescribeInstancesRequest { "Filters" :: NullOrUndefined (FilterList), "InstanceIds" :: NullOrUndefined (InstanceIdStringList), "DryRun" :: NullOrUndefined (Boolean), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeInstances.</p>

#### `DescribeInstancesResult`

``` purescript
newtype DescribeInstancesResult
  = DescribeInstancesResult { "Reservations" :: NullOrUndefined (ReservationList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeInstances.</p>

#### `DescribeInternetGatewaysRequest`

``` purescript
newtype DescribeInternetGatewaysRequest
  = DescribeInternetGatewaysRequest { "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean), "InternetGatewayIds" :: NullOrUndefined (ValueStringList) }
```

<p>Contains the parameters for DescribeInternetGateways.</p>

#### `DescribeInternetGatewaysResult`

``` purescript
newtype DescribeInternetGatewaysResult
  = DescribeInternetGatewaysResult { "InternetGateways" :: NullOrUndefined (InternetGatewayList) }
```

<p>Contains the output of DescribeInternetGateways.</p>

#### `DescribeKeyPairsRequest`

``` purescript
newtype DescribeKeyPairsRequest
  = DescribeKeyPairsRequest { "Filters" :: NullOrUndefined (FilterList), "KeyNames" :: NullOrUndefined (KeyNameStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeKeyPairs.</p>

#### `DescribeKeyPairsResult`

``` purescript
newtype DescribeKeyPairsResult
  = DescribeKeyPairsResult { "KeyPairs" :: NullOrUndefined (KeyPairList) }
```

<p>Contains the output of DescribeKeyPairs.</p>

#### `DescribeLaunchTemplateVersionsRequest`

``` purescript
newtype DescribeLaunchTemplateVersionsRequest
  = DescribeLaunchTemplateVersionsRequest { "DryRun" :: NullOrUndefined (Boolean), "LaunchTemplateId" :: NullOrUndefined (String), "LaunchTemplateName" :: NullOrUndefined (LaunchTemplateName), "Versions" :: NullOrUndefined (VersionStringList), "MinVersion" :: NullOrUndefined (String), "MaxVersion" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int), "Filters" :: NullOrUndefined (FilterList) }
```

#### `DescribeLaunchTemplateVersionsResult`

``` purescript
newtype DescribeLaunchTemplateVersionsResult
  = DescribeLaunchTemplateVersionsResult { "LaunchTemplateVersions" :: NullOrUndefined (LaunchTemplateVersionSet), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeLaunchTemplatesRequest`

``` purescript
newtype DescribeLaunchTemplatesRequest
  = DescribeLaunchTemplatesRequest { "DryRun" :: NullOrUndefined (Boolean), "LaunchTemplateIds" :: NullOrUndefined (ValueStringList), "LaunchTemplateNames" :: NullOrUndefined (LaunchTemplateNameStringList), "Filters" :: NullOrUndefined (FilterList), "NextToken" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int) }
```

#### `DescribeLaunchTemplatesResult`

``` purescript
newtype DescribeLaunchTemplatesResult
  = DescribeLaunchTemplatesResult { "LaunchTemplates" :: NullOrUndefined (LaunchTemplateSet), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeMovingAddressesRequest`

``` purescript
newtype DescribeMovingAddressesRequest
  = DescribeMovingAddressesRequest { "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "PublicIps" :: NullOrUndefined (ValueStringList) }
```

<p>Contains the parameters for DescribeMovingAddresses.</p>

#### `DescribeMovingAddressesResult`

``` purescript
newtype DescribeMovingAddressesResult
  = DescribeMovingAddressesResult { "MovingAddressStatuses" :: NullOrUndefined (MovingAddressStatusSet), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeMovingAddresses.</p>

#### `DescribeNatGatewaysRequest`

``` purescript
newtype DescribeNatGatewaysRequest
  = DescribeNatGatewaysRequest { "Filter" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NatGatewayIds" :: NullOrUndefined (ValueStringList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeNatGateways.</p>

#### `DescribeNatGatewaysResult`

``` purescript
newtype DescribeNatGatewaysResult
  = DescribeNatGatewaysResult { "NatGateways" :: NullOrUndefined (NatGatewayList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeNatGateways.</p>

#### `DescribeNetworkAclsRequest`

``` purescript
newtype DescribeNetworkAclsRequest
  = DescribeNetworkAclsRequest { "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean), "NetworkAclIds" :: NullOrUndefined (ValueStringList) }
```

<p>Contains the parameters for DescribeNetworkAcls.</p>

#### `DescribeNetworkAclsResult`

``` purescript
newtype DescribeNetworkAclsResult
  = DescribeNetworkAclsResult { "NetworkAcls" :: NullOrUndefined (NetworkAclList) }
```

<p>Contains the output of DescribeNetworkAcls.</p>

#### `DescribeNetworkInterfaceAttributeRequest`

``` purescript
newtype DescribeNetworkInterfaceAttributeRequest
  = DescribeNetworkInterfaceAttributeRequest { "Attribute" :: NullOrUndefined (NetworkInterfaceAttribute), "DryRun" :: NullOrUndefined (Boolean), "NetworkInterfaceId" :: String }
```

<p>Contains the parameters for DescribeNetworkInterfaceAttribute.</p>

#### `DescribeNetworkInterfaceAttributeResult`

``` purescript
newtype DescribeNetworkInterfaceAttributeResult
  = DescribeNetworkInterfaceAttributeResult { "Attachment" :: NullOrUndefined (NetworkInterfaceAttachment), "Description" :: NullOrUndefined (AttributeValue), "Groups" :: NullOrUndefined (GroupIdentifierList), "NetworkInterfaceId" :: NullOrUndefined (String), "SourceDestCheck" :: NullOrUndefined (AttributeBooleanValue) }
```

<p>Contains the output of DescribeNetworkInterfaceAttribute.</p>

#### `DescribeNetworkInterfacePermissionsRequest`

``` purescript
newtype DescribeNetworkInterfacePermissionsRequest
  = DescribeNetworkInterfacePermissionsRequest { "NetworkInterfacePermissionIds" :: NullOrUndefined (NetworkInterfacePermissionIdList), "Filters" :: NullOrUndefined (FilterList), "NextToken" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int) }
```

<p>Contains the parameters for DescribeNetworkInterfacePermissions.</p>

#### `DescribeNetworkInterfacePermissionsResult`

``` purescript
newtype DescribeNetworkInterfacePermissionsResult
  = DescribeNetworkInterfacePermissionsResult { "NetworkInterfacePermissions" :: NullOrUndefined (NetworkInterfacePermissionList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output for DescribeNetworkInterfacePermissions.</p>

#### `DescribeNetworkInterfacesRequest`

``` purescript
newtype DescribeNetworkInterfacesRequest
  = DescribeNetworkInterfacesRequest { "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean), "NetworkInterfaceIds" :: NullOrUndefined (NetworkInterfaceIdList) }
```

<p>Contains the parameters for DescribeNetworkInterfaces.</p>

#### `DescribeNetworkInterfacesResult`

``` purescript
newtype DescribeNetworkInterfacesResult
  = DescribeNetworkInterfacesResult { "NetworkInterfaces" :: NullOrUndefined (NetworkInterfaceList) }
```

<p>Contains the output of DescribeNetworkInterfaces.</p>

#### `DescribePlacementGroupsRequest`

``` purescript
newtype DescribePlacementGroupsRequest
  = DescribePlacementGroupsRequest { "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean), "GroupNames" :: NullOrUndefined (PlacementGroupStringList) }
```

<p>Contains the parameters for DescribePlacementGroups.</p>

#### `DescribePlacementGroupsResult`

``` purescript
newtype DescribePlacementGroupsResult
  = DescribePlacementGroupsResult { "PlacementGroups" :: NullOrUndefined (PlacementGroupList) }
```

<p>Contains the output of DescribePlacementGroups.</p>

#### `DescribePrefixListsRequest`

``` purescript
newtype DescribePrefixListsRequest
  = DescribePrefixListsRequest { "DryRun" :: NullOrUndefined (Boolean), "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "PrefixListIds" :: NullOrUndefined (ValueStringList) }
```

<p>Contains the parameters for DescribePrefixLists.</p>

#### `DescribePrefixListsResult`

``` purescript
newtype DescribePrefixListsResult
  = DescribePrefixListsResult { "NextToken" :: NullOrUndefined (String), "PrefixLists" :: NullOrUndefined (PrefixListSet) }
```

<p>Contains the output of DescribePrefixLists.</p>

#### `DescribePrincipalIdFormatRequest`

``` purescript
newtype DescribePrincipalIdFormatRequest
  = DescribePrincipalIdFormatRequest { "DryRun" :: NullOrUndefined (Boolean), "Resources" :: NullOrUndefined (ResourceList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribePrincipalIdFormatResult`

``` purescript
newtype DescribePrincipalIdFormatResult
  = DescribePrincipalIdFormatResult { "Principals" :: NullOrUndefined (PrincipalIdFormatList), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeRegionsRequest`

``` purescript
newtype DescribeRegionsRequest
  = DescribeRegionsRequest { "Filters" :: NullOrUndefined (FilterList), "RegionNames" :: NullOrUndefined (RegionNameStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeRegions.</p>

#### `DescribeRegionsResult`

``` purescript
newtype DescribeRegionsResult
  = DescribeRegionsResult { "Regions" :: NullOrUndefined (RegionList) }
```

<p>Contains the output of DescribeRegions.</p>

#### `DescribeReservedInstancesListingsRequest`

``` purescript
newtype DescribeReservedInstancesListingsRequest
  = DescribeReservedInstancesListingsRequest { "Filters" :: NullOrUndefined (FilterList), "ReservedInstancesId" :: NullOrUndefined (String), "ReservedInstancesListingId" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeReservedInstancesListings.</p>

#### `DescribeReservedInstancesListingsResult`

``` purescript
newtype DescribeReservedInstancesListingsResult
  = DescribeReservedInstancesListingsResult { "ReservedInstancesListings" :: NullOrUndefined (ReservedInstancesListingList) }
```

<p>Contains the output of DescribeReservedInstancesListings.</p>

#### `DescribeReservedInstancesModificationsRequest`

``` purescript
newtype DescribeReservedInstancesModificationsRequest
  = DescribeReservedInstancesModificationsRequest { "Filters" :: NullOrUndefined (FilterList), "ReservedInstancesModificationIds" :: NullOrUndefined (ReservedInstancesModificationIdStringList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeReservedInstancesModifications.</p>

#### `DescribeReservedInstancesModificationsResult`

``` purescript
newtype DescribeReservedInstancesModificationsResult
  = DescribeReservedInstancesModificationsResult { "NextToken" :: NullOrUndefined (String), "ReservedInstancesModifications" :: NullOrUndefined (ReservedInstancesModificationList) }
```

<p>Contains the output of DescribeReservedInstancesModifications.</p>

#### `DescribeReservedInstancesOfferingsRequest`

``` purescript
newtype DescribeReservedInstancesOfferingsRequest
  = DescribeReservedInstancesOfferingsRequest { "AvailabilityZone" :: NullOrUndefined (String), "Filters" :: NullOrUndefined (FilterList), "IncludeMarketplace" :: NullOrUndefined (Boolean), "InstanceType" :: NullOrUndefined (InstanceType), "MaxDuration" :: NullOrUndefined (Number), "MaxInstanceCount" :: NullOrUndefined (Int), "MinDuration" :: NullOrUndefined (Number), "OfferingClass" :: NullOrUndefined (OfferingClassType), "ProductDescription" :: NullOrUndefined (RIProductDescription), "ReservedInstancesOfferingIds" :: NullOrUndefined (ReservedInstancesOfferingIdStringList), "DryRun" :: NullOrUndefined (Boolean), "InstanceTenancy" :: NullOrUndefined (Tenancy), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "OfferingType" :: NullOrUndefined (OfferingTypeValues) }
```

<p>Contains the parameters for DescribeReservedInstancesOfferings.</p>

#### `DescribeReservedInstancesOfferingsResult`

``` purescript
newtype DescribeReservedInstancesOfferingsResult
  = DescribeReservedInstancesOfferingsResult { "ReservedInstancesOfferings" :: NullOrUndefined (ReservedInstancesOfferingList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeReservedInstancesOfferings.</p>

#### `DescribeReservedInstancesRequest`

``` purescript
newtype DescribeReservedInstancesRequest
  = DescribeReservedInstancesRequest { "Filters" :: NullOrUndefined (FilterList), "OfferingClass" :: NullOrUndefined (OfferingClassType), "ReservedInstancesIds" :: NullOrUndefined (ReservedInstancesIdStringList), "DryRun" :: NullOrUndefined (Boolean), "OfferingType" :: NullOrUndefined (OfferingTypeValues) }
```

<p>Contains the parameters for DescribeReservedInstances.</p>

#### `DescribeReservedInstancesResult`

``` purescript
newtype DescribeReservedInstancesResult
  = DescribeReservedInstancesResult { "ReservedInstances" :: NullOrUndefined (ReservedInstancesList) }
```

<p>Contains the output for DescribeReservedInstances.</p>

#### `DescribeRouteTablesRequest`

``` purescript
newtype DescribeRouteTablesRequest
  = DescribeRouteTablesRequest { "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean), "RouteTableIds" :: NullOrUndefined (ValueStringList) }
```

<p>Contains the parameters for DescribeRouteTables.</p>

#### `DescribeRouteTablesResult`

``` purescript
newtype DescribeRouteTablesResult
  = DescribeRouteTablesResult { "RouteTables" :: NullOrUndefined (RouteTableList) }
```

<p>Contains the output of DescribeRouteTables.</p>

#### `DescribeScheduledInstanceAvailabilityRequest`

``` purescript
newtype DescribeScheduledInstanceAvailabilityRequest
  = DescribeScheduledInstanceAvailabilityRequest { "DryRun" :: NullOrUndefined (Boolean), "Filters" :: NullOrUndefined (FilterList), "FirstSlotStartTimeRange" :: SlotDateTimeRangeRequest, "MaxResults" :: NullOrUndefined (Int), "MaxSlotDurationInHours" :: NullOrUndefined (Int), "MinSlotDurationInHours" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Recurrence" :: ScheduledInstanceRecurrenceRequest }
```

<p>Contains the parameters for DescribeScheduledInstanceAvailability.</p>

#### `DescribeScheduledInstanceAvailabilityResult`

``` purescript
newtype DescribeScheduledInstanceAvailabilityResult
  = DescribeScheduledInstanceAvailabilityResult { "NextToken" :: NullOrUndefined (String), "ScheduledInstanceAvailabilitySet" :: NullOrUndefined (ScheduledInstanceAvailabilitySet) }
```

<p>Contains the output of DescribeScheduledInstanceAvailability.</p>

#### `DescribeScheduledInstancesRequest`

``` purescript
newtype DescribeScheduledInstancesRequest
  = DescribeScheduledInstancesRequest { "DryRun" :: NullOrUndefined (Boolean), "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "ScheduledInstanceIds" :: NullOrUndefined (ScheduledInstanceIdRequestSet), "SlotStartTimeRange" :: NullOrUndefined (SlotStartTimeRangeRequest) }
```

<p>Contains the parameters for DescribeScheduledInstances.</p>

#### `DescribeScheduledInstancesResult`

``` purescript
newtype DescribeScheduledInstancesResult
  = DescribeScheduledInstancesResult { "NextToken" :: NullOrUndefined (String), "ScheduledInstanceSet" :: NullOrUndefined (ScheduledInstanceSet) }
```

<p>Contains the output of DescribeScheduledInstances.</p>

#### `DescribeSecurityGroupReferencesRequest`

``` purescript
newtype DescribeSecurityGroupReferencesRequest
  = DescribeSecurityGroupReferencesRequest { "DryRun" :: NullOrUndefined (Boolean), "GroupId" :: GroupIds }
```

#### `DescribeSecurityGroupReferencesResult`

``` purescript
newtype DescribeSecurityGroupReferencesResult
  = DescribeSecurityGroupReferencesResult { "SecurityGroupReferenceSet" :: NullOrUndefined (SecurityGroupReferences) }
```

#### `DescribeSecurityGroupsRequest`

``` purescript
newtype DescribeSecurityGroupsRequest
  = DescribeSecurityGroupsRequest { "Filters" :: NullOrUndefined (FilterList), "GroupIds" :: NullOrUndefined (GroupIdStringList), "GroupNames" :: NullOrUndefined (GroupNameStringList), "DryRun" :: NullOrUndefined (Boolean), "NextToken" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int) }
```

<p>Contains the parameters for DescribeSecurityGroups.</p>

#### `DescribeSecurityGroupsResult`

``` purescript
newtype DescribeSecurityGroupsResult
  = DescribeSecurityGroupsResult { "SecurityGroups" :: NullOrUndefined (SecurityGroupList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeSecurityGroups.</p>

#### `DescribeSnapshotAttributeRequest`

``` purescript
newtype DescribeSnapshotAttributeRequest
  = DescribeSnapshotAttributeRequest { "Attribute" :: SnapshotAttributeName, "SnapshotId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeSnapshotAttribute.</p>

#### `DescribeSnapshotAttributeResult`

``` purescript
newtype DescribeSnapshotAttributeResult
  = DescribeSnapshotAttributeResult { "CreateVolumePermissions" :: NullOrUndefined (CreateVolumePermissionList), "ProductCodes" :: NullOrUndefined (ProductCodeList), "SnapshotId" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeSnapshotAttribute.</p>

#### `DescribeSnapshotsRequest`

``` purescript
newtype DescribeSnapshotsRequest
  = DescribeSnapshotsRequest { "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "OwnerIds" :: NullOrUndefined (OwnerStringList), "RestorableByUserIds" :: NullOrUndefined (RestorableByStringList), "SnapshotIds" :: NullOrUndefined (SnapshotIdStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeSnapshots.</p>

#### `DescribeSnapshotsResult`

``` purescript
newtype DescribeSnapshotsResult
  = DescribeSnapshotsResult { "Snapshots" :: NullOrUndefined (SnapshotList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeSnapshots.</p>

#### `DescribeSpotDatafeedSubscriptionRequest`

``` purescript
newtype DescribeSpotDatafeedSubscriptionRequest
  = DescribeSpotDatafeedSubscriptionRequest { "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeSpotDatafeedSubscription.</p>

#### `DescribeSpotDatafeedSubscriptionResult`

``` purescript
newtype DescribeSpotDatafeedSubscriptionResult
  = DescribeSpotDatafeedSubscriptionResult { "SpotDatafeedSubscription" :: NullOrUndefined (SpotDatafeedSubscription) }
```

<p>Contains the output of DescribeSpotDatafeedSubscription.</p>

#### `DescribeSpotFleetInstancesRequest`

``` purescript
newtype DescribeSpotFleetInstancesRequest
  = DescribeSpotFleetInstancesRequest { "DryRun" :: NullOrUndefined (Boolean), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "SpotFleetRequestId" :: String }
```

<p>Contains the parameters for DescribeSpotFleetInstances.</p>

#### `DescribeSpotFleetInstancesResponse`

``` purescript
newtype DescribeSpotFleetInstancesResponse
  = DescribeSpotFleetInstancesResponse { "ActiveInstances" :: ActiveInstanceSet, "NextToken" :: NullOrUndefined (String), "SpotFleetRequestId" :: String }
```

<p>Contains the output of DescribeSpotFleetInstances.</p>

#### `DescribeSpotFleetRequestHistoryRequest`

``` purescript
newtype DescribeSpotFleetRequestHistoryRequest
  = DescribeSpotFleetRequestHistoryRequest { "DryRun" :: NullOrUndefined (Boolean), "EventType" :: NullOrUndefined (EventType), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "SpotFleetRequestId" :: String, "StartTime" :: DateTime }
```

<p>Contains the parameters for DescribeSpotFleetRequestHistory.</p>

#### `DescribeSpotFleetRequestHistoryResponse`

``` purescript
newtype DescribeSpotFleetRequestHistoryResponse
  = DescribeSpotFleetRequestHistoryResponse { "HistoryRecords" :: HistoryRecords, "LastEvaluatedTime" :: DateTime, "NextToken" :: NullOrUndefined (String), "SpotFleetRequestId" :: String, "StartTime" :: DateTime }
```

<p>Contains the output of DescribeSpotFleetRequestHistory.</p>

#### `DescribeSpotFleetRequestsRequest`

``` purescript
newtype DescribeSpotFleetRequestsRequest
  = DescribeSpotFleetRequestsRequest { "DryRun" :: NullOrUndefined (Boolean), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "SpotFleetRequestIds" :: NullOrUndefined (ValueStringList) }
```

<p>Contains the parameters for DescribeSpotFleetRequests.</p>

#### `DescribeSpotFleetRequestsResponse`

``` purescript
newtype DescribeSpotFleetRequestsResponse
  = DescribeSpotFleetRequestsResponse { "NextToken" :: NullOrUndefined (String), "SpotFleetRequestConfigs" :: SpotFleetRequestConfigSet }
```

<p>Contains the output of DescribeSpotFleetRequests.</p>

#### `DescribeSpotInstanceRequestsRequest`

``` purescript
newtype DescribeSpotInstanceRequestsRequest
  = DescribeSpotInstanceRequestsRequest { "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean), "SpotInstanceRequestIds" :: NullOrUndefined (SpotInstanceRequestIdList) }
```

<p>Contains the parameters for DescribeSpotInstanceRequests.</p>

#### `DescribeSpotInstanceRequestsResult`

``` purescript
newtype DescribeSpotInstanceRequestsResult
  = DescribeSpotInstanceRequestsResult { "SpotInstanceRequests" :: NullOrUndefined (SpotInstanceRequestList) }
```

<p>Contains the output of DescribeSpotInstanceRequests.</p>

#### `DescribeSpotPriceHistoryRequest`

``` purescript
newtype DescribeSpotPriceHistoryRequest
  = DescribeSpotPriceHistoryRequest { "Filters" :: NullOrUndefined (FilterList), "AvailabilityZone" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "EndTime" :: NullOrUndefined (DateTime), "InstanceTypes" :: NullOrUndefined (InstanceTypeList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "ProductDescriptions" :: NullOrUndefined (ProductDescriptionList), "StartTime" :: NullOrUndefined (DateTime) }
```

<p>Contains the parameters for DescribeSpotPriceHistory.</p>

#### `DescribeSpotPriceHistoryResult`

``` purescript
newtype DescribeSpotPriceHistoryResult
  = DescribeSpotPriceHistoryResult { "NextToken" :: NullOrUndefined (String), "SpotPriceHistory" :: NullOrUndefined (SpotPriceHistoryList) }
```

<p>Contains the output of DescribeSpotPriceHistory.</p>

#### `DescribeStaleSecurityGroupsRequest`

``` purescript
newtype DescribeStaleSecurityGroupsRequest
  = DescribeStaleSecurityGroupsRequest { "DryRun" :: NullOrUndefined (Boolean), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken), "VpcId" :: String }
```

#### `DescribeStaleSecurityGroupsResult`

``` purescript
newtype DescribeStaleSecurityGroupsResult
  = DescribeStaleSecurityGroupsResult { "NextToken" :: NullOrUndefined (String), "StaleSecurityGroupSet" :: NullOrUndefined (StaleSecurityGroupSet) }
```

#### `DescribeSubnetsRequest`

``` purescript
newtype DescribeSubnetsRequest
  = DescribeSubnetsRequest { "Filters" :: NullOrUndefined (FilterList), "SubnetIds" :: NullOrUndefined (SubnetIdStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeSubnets.</p>

#### `DescribeSubnetsResult`

``` purescript
newtype DescribeSubnetsResult
  = DescribeSubnetsResult { "Subnets" :: NullOrUndefined (SubnetList) }
```

<p>Contains the output of DescribeSubnets.</p>

#### `DescribeTagsRequest`

``` purescript
newtype DescribeTagsRequest
  = DescribeTagsRequest { "DryRun" :: NullOrUndefined (Boolean), "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeTags.</p>

#### `DescribeTagsResult`

``` purescript
newtype DescribeTagsResult
  = DescribeTagsResult { "NextToken" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagDescriptionList) }
```

<p>Contains the output of DescribeTags.</p>

#### `DescribeVolumeAttributeRequest`

``` purescript
newtype DescribeVolumeAttributeRequest
  = DescribeVolumeAttributeRequest { "Attribute" :: NullOrUndefined (VolumeAttributeName), "VolumeId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeVolumeAttribute.</p>

#### `DescribeVolumeAttributeResult`

``` purescript
newtype DescribeVolumeAttributeResult
  = DescribeVolumeAttributeResult { "AutoEnableIO" :: NullOrUndefined (AttributeBooleanValue), "ProductCodes" :: NullOrUndefined (ProductCodeList), "VolumeId" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeVolumeAttribute.</p>

#### `DescribeVolumeStatusRequest`

``` purescript
newtype DescribeVolumeStatusRequest
  = DescribeVolumeStatusRequest { "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "VolumeIds" :: NullOrUndefined (VolumeIdStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeVolumeStatus.</p>

#### `DescribeVolumeStatusResult`

``` purescript
newtype DescribeVolumeStatusResult
  = DescribeVolumeStatusResult { "NextToken" :: NullOrUndefined (String), "VolumeStatuses" :: NullOrUndefined (VolumeStatusList) }
```

<p>Contains the output of DescribeVolumeStatus.</p>

#### `DescribeVolumesModificationsRequest`

``` purescript
newtype DescribeVolumesModificationsRequest
  = DescribeVolumesModificationsRequest { "DryRun" :: NullOrUndefined (Boolean), "VolumeIds" :: NullOrUndefined (VolumeIdStringList), "Filters" :: NullOrUndefined (FilterList), "NextToken" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int) }
```

#### `DescribeVolumesModificationsResult`

``` purescript
newtype DescribeVolumesModificationsResult
  = DescribeVolumesModificationsResult { "VolumesModifications" :: NullOrUndefined (VolumeModificationList), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeVolumesRequest`

``` purescript
newtype DescribeVolumesRequest
  = DescribeVolumesRequest { "Filters" :: NullOrUndefined (FilterList), "VolumeIds" :: NullOrUndefined (VolumeIdStringList), "DryRun" :: NullOrUndefined (Boolean), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeVolumes.</p>

#### `DescribeVolumesResult`

``` purescript
newtype DescribeVolumesResult
  = DescribeVolumesResult { "Volumes" :: NullOrUndefined (VolumeList), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeVolumes.</p>

#### `DescribeVpcAttributeRequest`

``` purescript
newtype DescribeVpcAttributeRequest
  = DescribeVpcAttributeRequest { "Attribute" :: VpcAttributeName, "VpcId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeVpcAttribute.</p>

#### `DescribeVpcAttributeResult`

``` purescript
newtype DescribeVpcAttributeResult
  = DescribeVpcAttributeResult { "VpcId" :: NullOrUndefined (String), "EnableDnsHostnames" :: NullOrUndefined (AttributeBooleanValue), "EnableDnsSupport" :: NullOrUndefined (AttributeBooleanValue) }
```

<p>Contains the output of DescribeVpcAttribute.</p>

#### `DescribeVpcClassicLinkDnsSupportRequest`

``` purescript
newtype DescribeVpcClassicLinkDnsSupportRequest
  = DescribeVpcClassicLinkDnsSupportRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken), "VpcIds" :: NullOrUndefined (VpcClassicLinkIdList) }
```

<p>Contains the parameters for DescribeVpcClassicLinkDnsSupport.</p>

#### `DescribeVpcClassicLinkDnsSupportResult`

``` purescript
newtype DescribeVpcClassicLinkDnsSupportResult
  = DescribeVpcClassicLinkDnsSupportResult { "NextToken" :: NullOrUndefined (NextToken), "Vpcs" :: NullOrUndefined (ClassicLinkDnsSupportList) }
```

<p>Contains the output of DescribeVpcClassicLinkDnsSupport.</p>

#### `DescribeVpcClassicLinkRequest`

``` purescript
newtype DescribeVpcClassicLinkRequest
  = DescribeVpcClassicLinkRequest { "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean), "VpcIds" :: NullOrUndefined (VpcClassicLinkIdList) }
```

<p>Contains the parameters for DescribeVpcClassicLink.</p>

#### `DescribeVpcClassicLinkResult`

``` purescript
newtype DescribeVpcClassicLinkResult
  = DescribeVpcClassicLinkResult { "Vpcs" :: NullOrUndefined (VpcClassicLinkList) }
```

<p>Contains the output of DescribeVpcClassicLink.</p>

#### `DescribeVpcEndpointConnectionNotificationsRequest`

``` purescript
newtype DescribeVpcEndpointConnectionNotificationsRequest
  = DescribeVpcEndpointConnectionNotificationsRequest { "DryRun" :: NullOrUndefined (Boolean), "ConnectionNotificationId" :: NullOrUndefined (String), "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeVpcEndpointConnectionNotificationsResult`

``` purescript
newtype DescribeVpcEndpointConnectionNotificationsResult
  = DescribeVpcEndpointConnectionNotificationsResult { "ConnectionNotificationSet" :: NullOrUndefined (ConnectionNotificationSet), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeVpcEndpointConnectionsRequest`

``` purescript
newtype DescribeVpcEndpointConnectionsRequest
  = DescribeVpcEndpointConnectionsRequest { "DryRun" :: NullOrUndefined (Boolean), "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeVpcEndpointConnectionsResult`

``` purescript
newtype DescribeVpcEndpointConnectionsResult
  = DescribeVpcEndpointConnectionsResult { "VpcEndpointConnections" :: NullOrUndefined (VpcEndpointConnectionSet), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeVpcEndpointServiceConfigurationsRequest`

``` purescript
newtype DescribeVpcEndpointServiceConfigurationsRequest
  = DescribeVpcEndpointServiceConfigurationsRequest { "DryRun" :: NullOrUndefined (Boolean), "ServiceIds" :: NullOrUndefined (ValueStringList), "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeVpcEndpointServiceConfigurationsResult`

``` purescript
newtype DescribeVpcEndpointServiceConfigurationsResult
  = DescribeVpcEndpointServiceConfigurationsResult { "ServiceConfigurations" :: NullOrUndefined (ServiceConfigurationSet), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeVpcEndpointServicePermissionsRequest`

``` purescript
newtype DescribeVpcEndpointServicePermissionsRequest
  = DescribeVpcEndpointServicePermissionsRequest { "DryRun" :: NullOrUndefined (Boolean), "ServiceId" :: String, "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeVpcEndpointServicePermissionsResult`

``` purescript
newtype DescribeVpcEndpointServicePermissionsResult
  = DescribeVpcEndpointServicePermissionsResult { "AllowedPrincipals" :: NullOrUndefined (AllowedPrincipalSet), "NextToken" :: NullOrUndefined (String) }
```

#### `DescribeVpcEndpointServicesRequest`

``` purescript
newtype DescribeVpcEndpointServicesRequest
  = DescribeVpcEndpointServicesRequest { "DryRun" :: NullOrUndefined (Boolean), "ServiceNames" :: NullOrUndefined (ValueStringList), "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeVpcEndpointServices.</p>

#### `DescribeVpcEndpointServicesResult`

``` purescript
newtype DescribeVpcEndpointServicesResult
  = DescribeVpcEndpointServicesResult { "ServiceNames" :: NullOrUndefined (ValueStringList), "ServiceDetails" :: NullOrUndefined (ServiceDetailSet), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeVpcEndpointServices.</p>

#### `DescribeVpcEndpointsRequest`

``` purescript
newtype DescribeVpcEndpointsRequest
  = DescribeVpcEndpointsRequest { "DryRun" :: NullOrUndefined (Boolean), "VpcEndpointIds" :: NullOrUndefined (ValueStringList), "Filters" :: NullOrUndefined (FilterList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DescribeVpcEndpoints.</p>

#### `DescribeVpcEndpointsResult`

``` purescript
newtype DescribeVpcEndpointsResult
  = DescribeVpcEndpointsResult { "VpcEndpoints" :: NullOrUndefined (VpcEndpointSet), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the output of DescribeVpcEndpoints.</p>

#### `DescribeVpcPeeringConnectionsRequest`

``` purescript
newtype DescribeVpcPeeringConnectionsRequest
  = DescribeVpcPeeringConnectionsRequest { "Filters" :: NullOrUndefined (FilterList), "DryRun" :: NullOrUndefined (Boolean), "VpcPeeringConnectionIds" :: NullOrUndefined (ValueStringList) }
```

<p>Contains the parameters for DescribeVpcPeeringConnections.</p>

#### `DescribeVpcPeeringConnectionsResult`

``` purescript
newtype DescribeVpcPeeringConnectionsResult
  = DescribeVpcPeeringConnectionsResult { "VpcPeeringConnections" :: NullOrUndefined (VpcPeeringConnectionList) }
```

<p>Contains the output of DescribeVpcPeeringConnections.</p>

#### `DescribeVpcsRequest`

``` purescript
newtype DescribeVpcsRequest
  = DescribeVpcsRequest { "Filters" :: NullOrUndefined (FilterList), "VpcIds" :: NullOrUndefined (VpcIdStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeVpcs.</p>

#### `DescribeVpcsResult`

``` purescript
newtype DescribeVpcsResult
  = DescribeVpcsResult { "Vpcs" :: NullOrUndefined (VpcList) }
```

<p>Contains the output of DescribeVpcs.</p>

#### `DescribeVpnConnectionsRequest`

``` purescript
newtype DescribeVpnConnectionsRequest
  = DescribeVpnConnectionsRequest { "Filters" :: NullOrUndefined (FilterList), "VpnConnectionIds" :: NullOrUndefined (VpnConnectionIdStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeVpnConnections.</p>

#### `DescribeVpnConnectionsResult`

``` purescript
newtype DescribeVpnConnectionsResult
  = DescribeVpnConnectionsResult { "VpnConnections" :: NullOrUndefined (VpnConnectionList) }
```

<p>Contains the output of DescribeVpnConnections.</p>

#### `DescribeVpnGatewaysRequest`

``` purescript
newtype DescribeVpnGatewaysRequest
  = DescribeVpnGatewaysRequest { "Filters" :: NullOrUndefined (FilterList), "VpnGatewayIds" :: NullOrUndefined (VpnGatewayIdStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DescribeVpnGateways.</p>

#### `DescribeVpnGatewaysResult`

``` purescript
newtype DescribeVpnGatewaysResult
  = DescribeVpnGatewaysResult { "VpnGateways" :: NullOrUndefined (VpnGatewayList) }
```

<p>Contains the output of DescribeVpnGateways.</p>

#### `DetachClassicLinkVpcRequest`

``` purescript
newtype DetachClassicLinkVpcRequest
  = DetachClassicLinkVpcRequest { "DryRun" :: NullOrUndefined (Boolean), "InstanceId" :: String, "VpcId" :: String }
```

<p>Contains the parameters for DetachClassicLinkVpc.</p>

#### `DetachClassicLinkVpcResult`

``` purescript
newtype DetachClassicLinkVpcResult
  = DetachClassicLinkVpcResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of DetachClassicLinkVpc.</p>

#### `DetachInternetGatewayRequest`

``` purescript
newtype DetachInternetGatewayRequest
  = DetachInternetGatewayRequest { "DryRun" :: NullOrUndefined (Boolean), "InternetGatewayId" :: String, "VpcId" :: String }
```

<p>Contains the parameters for DetachInternetGateway.</p>

#### `DetachNetworkInterfaceRequest`

``` purescript
newtype DetachNetworkInterfaceRequest
  = DetachNetworkInterfaceRequest { "AttachmentId" :: String, "DryRun" :: NullOrUndefined (Boolean), "Force" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DetachNetworkInterface.</p>

#### `DetachVolumeRequest`

``` purescript
newtype DetachVolumeRequest
  = DetachVolumeRequest { "Device" :: NullOrUndefined (String), "Force" :: NullOrUndefined (Boolean), "InstanceId" :: NullOrUndefined (String), "VolumeId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DetachVolume.</p>

#### `DetachVpnGatewayRequest`

``` purescript
newtype DetachVpnGatewayRequest
  = DetachVpnGatewayRequest { "VpcId" :: String, "VpnGatewayId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DetachVpnGateway.</p>

#### `DeviceType`

``` purescript
newtype DeviceType
  = DeviceType String
```

#### `DhcpConfiguration`

``` purescript
newtype DhcpConfiguration
  = DhcpConfiguration { "Key" :: NullOrUndefined (String), "Values" :: NullOrUndefined (DhcpConfigurationValueList) }
```

<p>Describes a DHCP configuration option.</p>

#### `DhcpConfigurationList`

``` purescript
newtype DhcpConfigurationList
  = DhcpConfigurationList (Array DhcpConfiguration)
```

#### `DhcpConfigurationValueList`

``` purescript
newtype DhcpConfigurationValueList
  = DhcpConfigurationValueList (Array AttributeValue)
```

#### `DhcpOptions`

``` purescript
newtype DhcpOptions
  = DhcpOptions { "DhcpConfigurations" :: NullOrUndefined (DhcpConfigurationList), "DhcpOptionsId" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a set of DHCP options.</p>

#### `DhcpOptionsIdStringList`

``` purescript
newtype DhcpOptionsIdStringList
  = DhcpOptionsIdStringList (Array String)
```

#### `DhcpOptionsList`

``` purescript
newtype DhcpOptionsList
  = DhcpOptionsList (Array DhcpOptions)
```

#### `DisableVgwRoutePropagationRequest`

``` purescript
newtype DisableVgwRoutePropagationRequest
  = DisableVgwRoutePropagationRequest { "GatewayId" :: String, "RouteTableId" :: String }
```

<p>Contains the parameters for DisableVgwRoutePropagation.</p>

#### `DisableVpcClassicLinkDnsSupportRequest`

``` purescript
newtype DisableVpcClassicLinkDnsSupportRequest
  = DisableVpcClassicLinkDnsSupportRequest { "VpcId" :: NullOrUndefined (String) }
```

<p>Contains the parameters for DisableVpcClassicLinkDnsSupport.</p>

#### `DisableVpcClassicLinkDnsSupportResult`

``` purescript
newtype DisableVpcClassicLinkDnsSupportResult
  = DisableVpcClassicLinkDnsSupportResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of DisableVpcClassicLinkDnsSupport.</p>

#### `DisableVpcClassicLinkRequest`

``` purescript
newtype DisableVpcClassicLinkRequest
  = DisableVpcClassicLinkRequest { "DryRun" :: NullOrUndefined (Boolean), "VpcId" :: String }
```

<p>Contains the parameters for DisableVpcClassicLink.</p>

#### `DisableVpcClassicLinkResult`

``` purescript
newtype DisableVpcClassicLinkResult
  = DisableVpcClassicLinkResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of DisableVpcClassicLink.</p>

#### `DisassociateAddressRequest`

``` purescript
newtype DisassociateAddressRequest
  = DisassociateAddressRequest { "AssociationId" :: NullOrUndefined (String), "PublicIp" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DisassociateAddress.</p>

#### `DisassociateIamInstanceProfileRequest`

``` purescript
newtype DisassociateIamInstanceProfileRequest
  = DisassociateIamInstanceProfileRequest { "AssociationId" :: String }
```

#### `DisassociateIamInstanceProfileResult`

``` purescript
newtype DisassociateIamInstanceProfileResult
  = DisassociateIamInstanceProfileResult { "IamInstanceProfileAssociation" :: NullOrUndefined (IamInstanceProfileAssociation) }
```

#### `DisassociateRouteTableRequest`

``` purescript
newtype DisassociateRouteTableRequest
  = DisassociateRouteTableRequest { "AssociationId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for DisassociateRouteTable.</p>

#### `DisassociateSubnetCidrBlockRequest`

``` purescript
newtype DisassociateSubnetCidrBlockRequest
  = DisassociateSubnetCidrBlockRequest { "AssociationId" :: String }
```

#### `DisassociateSubnetCidrBlockResult`

``` purescript
newtype DisassociateSubnetCidrBlockResult
  = DisassociateSubnetCidrBlockResult { "Ipv6CidrBlockAssociation" :: NullOrUndefined (SubnetIpv6CidrBlockAssociation), "SubnetId" :: NullOrUndefined (String) }
```

#### `DisassociateVpcCidrBlockRequest`

``` purescript
newtype DisassociateVpcCidrBlockRequest
  = DisassociateVpcCidrBlockRequest { "AssociationId" :: String }
```

#### `DisassociateVpcCidrBlockResult`

``` purescript
newtype DisassociateVpcCidrBlockResult
  = DisassociateVpcCidrBlockResult { "Ipv6CidrBlockAssociation" :: NullOrUndefined (VpcIpv6CidrBlockAssociation), "CidrBlockAssociation" :: NullOrUndefined (VpcCidrBlockAssociation), "VpcId" :: NullOrUndefined (String) }
```

#### `DiskImage`

``` purescript
newtype DiskImage
  = DiskImage { "Description" :: NullOrUndefined (String), "Image" :: NullOrUndefined (DiskImageDetail), "Volume" :: NullOrUndefined (VolumeDetail) }
```

<p>Describes a disk image.</p>

#### `DiskImageDescription`

``` purescript
newtype DiskImageDescription
  = DiskImageDescription { "Checksum" :: NullOrUndefined (String), "Format" :: DiskImageFormat, "ImportManifestUrl" :: String, "Size" :: Number }
```

<p>Describes a disk image.</p>

#### `DiskImageDetail`

``` purescript
newtype DiskImageDetail
  = DiskImageDetail { "Bytes" :: Number, "Format" :: DiskImageFormat, "ImportManifestUrl" :: String }
```

<p>Describes a disk image.</p>

#### `DiskImageFormat`

``` purescript
newtype DiskImageFormat
  = DiskImageFormat String
```

#### `DiskImageList`

``` purescript
newtype DiskImageList
  = DiskImageList (Array DiskImage)
```

#### `DiskImageVolumeDescription`

``` purescript
newtype DiskImageVolumeDescription
  = DiskImageVolumeDescription { "Id" :: String, "Size" :: NullOrUndefined (Number) }
```

<p>Describes a disk image volume.</p>

#### `DnsEntry`

``` purescript
newtype DnsEntry
  = DnsEntry { "DnsName" :: NullOrUndefined (String), "HostedZoneId" :: NullOrUndefined (String) }
```

<p>Describes a DNS entry.</p>

#### `DnsEntrySet`

``` purescript
newtype DnsEntrySet
  = DnsEntrySet (Array DnsEntry)
```

#### `DomainType`

``` purescript
newtype DomainType
  = DomainType String
```

#### `EbsBlockDevice`

``` purescript
newtype EbsBlockDevice
  = EbsBlockDevice { "Encrypted" :: NullOrUndefined (Boolean), "DeleteOnTermination" :: NullOrUndefined (Boolean), "Iops" :: NullOrUndefined (Int), "KmsKeyId" :: NullOrUndefined (String), "SnapshotId" :: NullOrUndefined (String), "VolumeSize" :: NullOrUndefined (Int), "VolumeType" :: NullOrUndefined (VolumeType) }
```

<p>Describes a block device for an EBS volume.</p>

#### `EbsInstanceBlockDevice`

``` purescript
newtype EbsInstanceBlockDevice
  = EbsInstanceBlockDevice { "AttachTime" :: NullOrUndefined (DateTime), "DeleteOnTermination" :: NullOrUndefined (Boolean), "Status" :: NullOrUndefined (AttachmentStatus), "VolumeId" :: NullOrUndefined (String) }
```

<p>Describes a parameter used to set up an EBS volume in a block device mapping.</p>

#### `EbsInstanceBlockDeviceSpecification`

``` purescript
newtype EbsInstanceBlockDeviceSpecification
  = EbsInstanceBlockDeviceSpecification { "DeleteOnTermination" :: NullOrUndefined (Boolean), "VolumeId" :: NullOrUndefined (String) }
```

<p>Describes information used to set up an EBS volume specified in a block device mapping.</p>

#### `EgressOnlyInternetGateway`

``` purescript
newtype EgressOnlyInternetGateway
  = EgressOnlyInternetGateway { "Attachments" :: NullOrUndefined (InternetGatewayAttachmentList), "EgressOnlyInternetGatewayId" :: NullOrUndefined (EgressOnlyInternetGatewayId) }
```

<p>Describes an egress-only Internet gateway.</p>

#### `EgressOnlyInternetGatewayId`

``` purescript
newtype EgressOnlyInternetGatewayId
  = EgressOnlyInternetGatewayId String
```

#### `EgressOnlyInternetGatewayIdList`

``` purescript
newtype EgressOnlyInternetGatewayIdList
  = EgressOnlyInternetGatewayIdList (Array EgressOnlyInternetGatewayId)
```

#### `EgressOnlyInternetGatewayList`

``` purescript
newtype EgressOnlyInternetGatewayList
  = EgressOnlyInternetGatewayList (Array EgressOnlyInternetGateway)
```

#### `ElasticGpuAssociation`

``` purescript
newtype ElasticGpuAssociation
  = ElasticGpuAssociation { "ElasticGpuId" :: NullOrUndefined (String), "ElasticGpuAssociationId" :: NullOrUndefined (String), "ElasticGpuAssociationState" :: NullOrUndefined (String), "ElasticGpuAssociationTime" :: NullOrUndefined (String) }
```

<p>Describes the association between an instance and an Elastic GPU.</p>

#### `ElasticGpuAssociationList`

``` purescript
newtype ElasticGpuAssociationList
  = ElasticGpuAssociationList (Array ElasticGpuAssociation)
```

#### `ElasticGpuHealth`

``` purescript
newtype ElasticGpuHealth
  = ElasticGpuHealth { "Status" :: NullOrUndefined (ElasticGpuStatus) }
```

<p>Describes the status of an Elastic GPU.</p>

#### `ElasticGpuIdSet`

``` purescript
newtype ElasticGpuIdSet
  = ElasticGpuIdSet (Array String)
```

#### `ElasticGpuSet`

``` purescript
newtype ElasticGpuSet
  = ElasticGpuSet (Array ElasticGpus)
```

#### `ElasticGpuSpecification`

``` purescript
newtype ElasticGpuSpecification
  = ElasticGpuSpecification { "Type" :: String }
```

<p>A specification for an Elastic GPU.</p>

#### `ElasticGpuSpecificationList`

``` purescript
newtype ElasticGpuSpecificationList
  = ElasticGpuSpecificationList (Array ElasticGpuSpecification)
```

#### `ElasticGpuSpecificationResponse`

``` purescript
newtype ElasticGpuSpecificationResponse
  = ElasticGpuSpecificationResponse { "Type" :: NullOrUndefined (String) }
```

<p>Describes an elastic GPU.</p>

#### `ElasticGpuSpecificationResponseList`

``` purescript
newtype ElasticGpuSpecificationResponseList
  = ElasticGpuSpecificationResponseList (Array ElasticGpuSpecificationResponse)
```

#### `ElasticGpuSpecifications`

``` purescript
newtype ElasticGpuSpecifications
  = ElasticGpuSpecifications (Array ElasticGpuSpecification)
```

#### `ElasticGpuState`

``` purescript
newtype ElasticGpuState
  = ElasticGpuState String
```

#### `ElasticGpuStatus`

``` purescript
newtype ElasticGpuStatus
  = ElasticGpuStatus String
```

#### `ElasticGpus`

``` purescript
newtype ElasticGpus
  = ElasticGpus { "ElasticGpuId" :: NullOrUndefined (String), "AvailabilityZone" :: NullOrUndefined (String), "ElasticGpuType" :: NullOrUndefined (String), "ElasticGpuHealth" :: NullOrUndefined (ElasticGpuHealth), "ElasticGpuState" :: NullOrUndefined (ElasticGpuState), "InstanceId" :: NullOrUndefined (String) }
```

<p>Describes an Elastic GPU.</p>

#### `EnableVgwRoutePropagationRequest`

``` purescript
newtype EnableVgwRoutePropagationRequest
  = EnableVgwRoutePropagationRequest { "GatewayId" :: String, "RouteTableId" :: String }
```

<p>Contains the parameters for EnableVgwRoutePropagation.</p>

#### `EnableVolumeIORequest`

``` purescript
newtype EnableVolumeIORequest
  = EnableVolumeIORequest { "DryRun" :: NullOrUndefined (Boolean), "VolumeId" :: String }
```

<p>Contains the parameters for EnableVolumeIO.</p>

#### `EnableVpcClassicLinkDnsSupportRequest`

``` purescript
newtype EnableVpcClassicLinkDnsSupportRequest
  = EnableVpcClassicLinkDnsSupportRequest { "VpcId" :: NullOrUndefined (String) }
```

<p>Contains the parameters for EnableVpcClassicLinkDnsSupport.</p>

#### `EnableVpcClassicLinkDnsSupportResult`

``` purescript
newtype EnableVpcClassicLinkDnsSupportResult
  = EnableVpcClassicLinkDnsSupportResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of EnableVpcClassicLinkDnsSupport.</p>

#### `EnableVpcClassicLinkRequest`

``` purescript
newtype EnableVpcClassicLinkRequest
  = EnableVpcClassicLinkRequest { "DryRun" :: NullOrUndefined (Boolean), "VpcId" :: String }
```

<p>Contains the parameters for EnableVpcClassicLink.</p>

#### `EnableVpcClassicLinkResult`

``` purescript
newtype EnableVpcClassicLinkResult
  = EnableVpcClassicLinkResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of EnableVpcClassicLink.</p>

#### `EventCode`

``` purescript
newtype EventCode
  = EventCode String
```

#### `EventInformation`

``` purescript
newtype EventInformation
  = EventInformation { "EventDescription" :: NullOrUndefined (String), "EventSubType" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String) }
```

<p>Describes a Spot Fleet event.</p>

#### `EventType`

``` purescript
newtype EventType
  = EventType String
```

#### `ExcessCapacityTerminationPolicy`

``` purescript
newtype ExcessCapacityTerminationPolicy
  = ExcessCapacityTerminationPolicy String
```

#### `ExecutableByStringList`

``` purescript
newtype ExecutableByStringList
  = ExecutableByStringList (Array String)
```

#### `ExportEnvironment`

``` purescript
newtype ExportEnvironment
  = ExportEnvironment String
```

#### `ExportTask`

``` purescript
newtype ExportTask
  = ExportTask { "Description" :: NullOrUndefined (String), "ExportTaskId" :: NullOrUndefined (String), "ExportToS3Task" :: NullOrUndefined (ExportToS3Task), "InstanceExportDetails" :: NullOrUndefined (InstanceExportDetails), "State" :: NullOrUndefined (ExportTaskState), "StatusMessage" :: NullOrUndefined (String) }
```

<p>Describes an instance export task.</p>

#### `ExportTaskIdStringList`

``` purescript
newtype ExportTaskIdStringList
  = ExportTaskIdStringList (Array String)
```

#### `ExportTaskList`

``` purescript
newtype ExportTaskList
  = ExportTaskList (Array ExportTask)
```

#### `ExportTaskState`

``` purescript
newtype ExportTaskState
  = ExportTaskState String
```

#### `ExportToS3Task`

``` purescript
newtype ExportToS3Task
  = ExportToS3Task { "ContainerFormat" :: NullOrUndefined (ContainerFormat), "DiskImageFormat" :: NullOrUndefined (DiskImageFormat), "S3Bucket" :: NullOrUndefined (String), "S3Key" :: NullOrUndefined (String) }
```

<p>Describes the format and location for an instance export task.</p>

#### `ExportToS3TaskSpecification`

``` purescript
newtype ExportToS3TaskSpecification
  = ExportToS3TaskSpecification { "ContainerFormat" :: NullOrUndefined (ContainerFormat), "DiskImageFormat" :: NullOrUndefined (DiskImageFormat), "S3Bucket" :: NullOrUndefined (String), "S3Prefix" :: NullOrUndefined (String) }
```

<p>Describes an instance export task.</p>

#### `Filter`

``` purescript
newtype Filter
  = Filter { "Name" :: NullOrUndefined (String), "Values" :: NullOrUndefined (ValueStringList) }
```

<p>A filter name and value pair that is used to return a more specific list of results. Filters can be used to match a set of resources by various criteria, such as tags, attributes, or IDs.</p>

#### `FilterList`

``` purescript
newtype FilterList
  = FilterList (Array Filter)
```

#### `FleetLaunchTemplateSpecification`

``` purescript
newtype FleetLaunchTemplateSpecification
  = FleetLaunchTemplateSpecification { "LaunchTemplateId" :: NullOrUndefined (String), "LaunchTemplateName" :: NullOrUndefined (LaunchTemplateName), "Version" :: NullOrUndefined (String) }
```

<p>Describes a launch template.</p>

#### `FleetType`

``` purescript
newtype FleetType
  = FleetType String
```

#### `FlowLog`

``` purescript
newtype FlowLog
  = FlowLog { "CreationTime" :: NullOrUndefined (DateTime), "DeliverLogsErrorMessage" :: NullOrUndefined (String), "DeliverLogsPermissionArn" :: NullOrUndefined (String), "DeliverLogsStatus" :: NullOrUndefined (String), "FlowLogId" :: NullOrUndefined (String), "FlowLogStatus" :: NullOrUndefined (String), "LogGroupName" :: NullOrUndefined (String), "ResourceId" :: NullOrUndefined (String), "TrafficType" :: NullOrUndefined (TrafficType) }
```

<p>Describes a flow log.</p>

#### `FlowLogSet`

``` purescript
newtype FlowLogSet
  = FlowLogSet (Array FlowLog)
```

#### `FlowLogsResourceType`

``` purescript
newtype FlowLogsResourceType
  = FlowLogsResourceType String
```

#### `FpgaImage`

``` purescript
newtype FpgaImage
  = FpgaImage { "FpgaImageId" :: NullOrUndefined (String), "FpgaImageGlobalId" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "ShellVersion" :: NullOrUndefined (String), "PciId" :: NullOrUndefined (PciId), "State" :: NullOrUndefined (FpgaImageState), "CreateTime" :: NullOrUndefined (DateTime), "UpdateTime" :: NullOrUndefined (DateTime), "OwnerId" :: NullOrUndefined (String), "OwnerAlias" :: NullOrUndefined (String), "ProductCodes" :: NullOrUndefined (ProductCodeList), "Tags" :: NullOrUndefined (TagList), "Public" :: NullOrUndefined (Boolean) }
```

<p>Describes an Amazon FPGA image (AFI).</p>

#### `FpgaImageAttribute`

``` purescript
newtype FpgaImageAttribute
  = FpgaImageAttribute { "FpgaImageId" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "LoadPermissions" :: NullOrUndefined (LoadPermissionList), "ProductCodes" :: NullOrUndefined (ProductCodeList) }
```

<p>Describes an Amazon FPGA image (AFI) attribute.</p>

#### `FpgaImageAttributeName`

``` purescript
newtype FpgaImageAttributeName
  = FpgaImageAttributeName String
```

#### `FpgaImageIdList`

``` purescript
newtype FpgaImageIdList
  = FpgaImageIdList (Array String)
```

#### `FpgaImageList`

``` purescript
newtype FpgaImageList
  = FpgaImageList (Array FpgaImage)
```

#### `FpgaImageState`

``` purescript
newtype FpgaImageState
  = FpgaImageState { "Code" :: NullOrUndefined (FpgaImageStateCode), "Message" :: NullOrUndefined (String) }
```

<p>Describes the state of the bitstream generation process for an Amazon FPGA image (AFI).</p>

#### `FpgaImageStateCode`

``` purescript
newtype FpgaImageStateCode
  = FpgaImageStateCode String
```

#### `GatewayType`

``` purescript
newtype GatewayType
  = GatewayType String
```

#### `GetConsoleOutputRequest`

``` purescript
newtype GetConsoleOutputRequest
  = GetConsoleOutputRequest { "InstanceId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for GetConsoleOutput.</p>

#### `GetConsoleOutputResult`

``` purescript
newtype GetConsoleOutputResult
  = GetConsoleOutputResult { "InstanceId" :: NullOrUndefined (String), "Output" :: NullOrUndefined (String), "Number" :: NullOrUndefined (DateTime) }
```

<p>Contains the output of GetConsoleOutput.</p>

#### `GetConsoleScreenshotRequest`

``` purescript
newtype GetConsoleScreenshotRequest
  = GetConsoleScreenshotRequest { "DryRun" :: NullOrUndefined (Boolean), "InstanceId" :: String, "WakeUp" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for the request.</p>

#### `GetConsoleScreenshotResult`

``` purescript
newtype GetConsoleScreenshotResult
  = GetConsoleScreenshotResult { "ImageData" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String) }
```

<p>Contains the output of the request.</p>

#### `GetHostReservationPurchasePreviewRequest`

``` purescript
newtype GetHostReservationPurchasePreviewRequest
  = GetHostReservationPurchasePreviewRequest { "HostIdSet" :: RequestHostIdSet, "OfferingId" :: String }
```

#### `GetHostReservationPurchasePreviewResult`

``` purescript
newtype GetHostReservationPurchasePreviewResult
  = GetHostReservationPurchasePreviewResult { "CurrencyCode" :: NullOrUndefined (CurrencyCodeValues), "Purchase" :: NullOrUndefined (PurchaseSet), "TotalHourlyPrice" :: NullOrUndefined (String), "TotalUpfrontPrice" :: NullOrUndefined (String) }
```

#### `GetLaunchTemplateDataRequest`

``` purescript
newtype GetLaunchTemplateDataRequest
  = GetLaunchTemplateDataRequest { "DryRun" :: NullOrUndefined (Boolean), "InstanceId" :: String }
```

#### `GetLaunchTemplateDataResult`

``` purescript
newtype GetLaunchTemplateDataResult
  = GetLaunchTemplateDataResult { "LaunchTemplateData" :: NullOrUndefined (ResponseLaunchTemplateData) }
```

#### `GetPasswordDataRequest`

``` purescript
newtype GetPasswordDataRequest
  = GetPasswordDataRequest { "InstanceId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for GetPasswordData.</p>

#### `GetPasswordDataResult`

``` purescript
newtype GetPasswordDataResult
  = GetPasswordDataResult { "InstanceId" :: NullOrUndefined (String), "PasswordData" :: NullOrUndefined (String), "Number" :: NullOrUndefined (DateTime) }
```

<p>Contains the output of GetPasswordData.</p>

#### `GetReservedInstancesExchangeQuoteRequest`

``` purescript
newtype GetReservedInstancesExchangeQuoteRequest
  = GetReservedInstancesExchangeQuoteRequest { "DryRun" :: NullOrUndefined (Boolean), "ReservedInstanceIds" :: ReservedInstanceIdSet, "TargetConfigurations" :: NullOrUndefined (TargetConfigurationRequestSet) }
```

<p>Contains the parameters for GetReservedInstanceExchangeQuote.</p>

#### `GetReservedInstancesExchangeQuoteResult`

``` purescript
newtype GetReservedInstancesExchangeQuoteResult
  = GetReservedInstancesExchangeQuoteResult { "CurrencyCode" :: NullOrUndefined (String), "IsValidExchange" :: NullOrUndefined (Boolean), "OutputReservedInstancesWillExpireAt" :: NullOrUndefined (DateTime), "PaymentDue" :: NullOrUndefined (String), "ReservedInstanceValueRollup" :: NullOrUndefined (ReservationValue), "ReservedInstanceValueSet" :: NullOrUndefined (ReservedInstanceReservationValueSet), "TargetConfigurationValueRollup" :: NullOrUndefined (ReservationValue), "TargetConfigurationValueSet" :: NullOrUndefined (TargetReservationValueSet), "ValidationFailureReason" :: NullOrUndefined (String) }
```

<p>Contains the output of GetReservedInstancesExchangeQuote.</p>

#### `GroupIdStringList`

``` purescript
newtype GroupIdStringList
  = GroupIdStringList (Array String)
```

#### `GroupIdentifier`

``` purescript
newtype GroupIdentifier
  = GroupIdentifier { "GroupName" :: NullOrUndefined (String), "GroupId" :: NullOrUndefined (String) }
```

<p>Describes a security group.</p>

#### `GroupIdentifierList`

``` purescript
newtype GroupIdentifierList
  = GroupIdentifierList (Array GroupIdentifier)
```

#### `GroupIdentifierSet`

``` purescript
newtype GroupIdentifierSet
  = GroupIdentifierSet (Array SecurityGroupIdentifier)
```

#### `GroupIds`

``` purescript
newtype GroupIds
  = GroupIds (Array String)
```

#### `GroupNameStringList`

``` purescript
newtype GroupNameStringList
  = GroupNameStringList (Array String)
```

#### `HistoryRecord`

``` purescript
newtype HistoryRecord
  = HistoryRecord { "EventInformation" :: EventInformation, "EventType" :: EventType, "Number" :: DateTime }
```

<p>Describes an event in the history of the Spot Fleet request.</p>

#### `HistoryRecords`

``` purescript
newtype HistoryRecords
  = HistoryRecords (Array HistoryRecord)
```

#### `Host`

``` purescript
newtype Host
  = Host { "AutoPlacement" :: NullOrUndefined (AutoPlacement), "AvailabilityZone" :: NullOrUndefined (String), "AvailableCapacity" :: NullOrUndefined (AvailableCapacity), "ClientToken" :: NullOrUndefined (String), "HostId" :: NullOrUndefined (String), "HostProperties" :: NullOrUndefined (HostProperties), "HostReservationId" :: NullOrUndefined (String), "Instances" :: NullOrUndefined (HostInstanceList), "State" :: NullOrUndefined (AllocationState) }
```

<p>Describes the properties of the Dedicated Host.</p>

#### `HostInstance`

``` purescript
newtype HostInstance
  = HostInstance { "InstanceId" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (String) }
```

<p>Describes an instance running on a Dedicated Host.</p>

#### `HostInstanceList`

``` purescript
newtype HostInstanceList
  = HostInstanceList (Array HostInstance)
```

#### `HostList`

``` purescript
newtype HostList
  = HostList (Array Host)
```

#### `HostOffering`

``` purescript
newtype HostOffering
  = HostOffering { "CurrencyCode" :: NullOrUndefined (CurrencyCodeValues), "Duration" :: NullOrUndefined (Int), "HourlyPrice" :: NullOrUndefined (String), "InstanceFamily" :: NullOrUndefined (String), "OfferingId" :: NullOrUndefined (String), "PaymentOption" :: NullOrUndefined (PaymentOption), "UpfrontPrice" :: NullOrUndefined (String) }
```

<p>Details about the Dedicated Host Reservation offering.</p>

#### `HostOfferingSet`

``` purescript
newtype HostOfferingSet
  = HostOfferingSet (Array HostOffering)
```

#### `HostProperties`

``` purescript
newtype HostProperties
  = HostProperties { "Cores" :: NullOrUndefined (Int), "InstanceType" :: NullOrUndefined (String), "Sockets" :: NullOrUndefined (Int), "TotalVCpus" :: NullOrUndefined (Int) }
```

<p>Describes properties of a Dedicated Host.</p>

#### `HostReservation`

``` purescript
newtype HostReservation
  = HostReservation { "Count" :: NullOrUndefined (Int), "CurrencyCode" :: NullOrUndefined (CurrencyCodeValues), "Duration" :: NullOrUndefined (Int), "End" :: NullOrUndefined (DateTime), "HostIdSet" :: NullOrUndefined (ResponseHostIdSet), "HostReservationId" :: NullOrUndefined (String), "HourlyPrice" :: NullOrUndefined (String), "InstanceFamily" :: NullOrUndefined (String), "OfferingId" :: NullOrUndefined (String), "PaymentOption" :: NullOrUndefined (PaymentOption), "Start" :: NullOrUndefined (DateTime), "State" :: NullOrUndefined (ReservationState), "UpfrontPrice" :: NullOrUndefined (String) }
```

<p>Details about the Dedicated Host Reservation and associated Dedicated Hosts.</p>

#### `HostReservationIdSet`

``` purescript
newtype HostReservationIdSet
  = HostReservationIdSet (Array String)
```

#### `HostReservationSet`

``` purescript
newtype HostReservationSet
  = HostReservationSet (Array HostReservation)
```

#### `HostTenancy`

``` purescript
newtype HostTenancy
  = HostTenancy String
```

#### `HypervisorType`

``` purescript
newtype HypervisorType
  = HypervisorType String
```

#### `IamInstanceProfile`

``` purescript
newtype IamInstanceProfile
  = IamInstanceProfile { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String) }
```

<p>Describes an IAM instance profile.</p>

#### `IamInstanceProfileAssociation`

``` purescript
newtype IamInstanceProfileAssociation
  = IamInstanceProfileAssociation { "AssociationId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "IamInstanceProfile" :: NullOrUndefined (IamInstanceProfile), "State" :: NullOrUndefined (IamInstanceProfileAssociationState), "Number" :: NullOrUndefined (DateTime) }
```

<p>Describes an association between an IAM instance profile and an instance.</p>

#### `IamInstanceProfileAssociationSet`

``` purescript
newtype IamInstanceProfileAssociationSet
  = IamInstanceProfileAssociationSet (Array IamInstanceProfileAssociation)
```

#### `IamInstanceProfileAssociationState`

``` purescript
newtype IamInstanceProfileAssociationState
  = IamInstanceProfileAssociationState String
```

#### `IamInstanceProfileSpecification`

``` purescript
newtype IamInstanceProfileSpecification
  = IamInstanceProfileSpecification { "Arn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

<p>Describes an IAM instance profile.</p>

#### `IcmpTypeCode`

``` purescript
newtype IcmpTypeCode
  = IcmpTypeCode { "Code" :: NullOrUndefined (Int), "Type" :: NullOrUndefined (Int) }
```

<p>Describes the ICMP type and code.</p>

#### `IdFormat`

``` purescript
newtype IdFormat
  = IdFormat { "Deadline" :: NullOrUndefined (DateTime), "Resource" :: NullOrUndefined (String), "UseLongIds" :: NullOrUndefined (Boolean) }
```

<p>Describes the ID format for a resource.</p>

#### `IdFormatList`

``` purescript
newtype IdFormatList
  = IdFormatList (Array IdFormat)
```

#### `Image`

``` purescript
newtype Image
  = Image { "Architecture" :: NullOrUndefined (ArchitectureValues), "CreationDate" :: NullOrUndefined (String), "ImageId" :: NullOrUndefined (String), "ImageLocation" :: NullOrUndefined (String), "ImageType" :: NullOrUndefined (ImageTypeValues), "Public" :: NullOrUndefined (Boolean), "KernelId" :: NullOrUndefined (String), "OwnerId" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (PlatformValues), "ProductCodes" :: NullOrUndefined (ProductCodeList), "RamdiskId" :: NullOrUndefined (String), "State" :: NullOrUndefined (ImageState), "BlockDeviceMappings" :: NullOrUndefined (BlockDeviceMappingList), "Description" :: NullOrUndefined (String), "EnaSupport" :: NullOrUndefined (Boolean), "Hypervisor" :: NullOrUndefined (HypervisorType), "ImageOwnerAlias" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "RootDeviceName" :: NullOrUndefined (String), "RootDeviceType" :: NullOrUndefined (DeviceType), "SriovNetSupport" :: NullOrUndefined (String), "StateReason" :: NullOrUndefined (StateReason), "Tags" :: NullOrUndefined (TagList), "VirtualizationType" :: NullOrUndefined (VirtualizationType) }
```

<p>Describes an image.</p>

#### `ImageAttribute`

``` purescript
newtype ImageAttribute
  = ImageAttribute { "BlockDeviceMappings" :: NullOrUndefined (BlockDeviceMappingList), "ImageId" :: NullOrUndefined (String), "LaunchPermissions" :: NullOrUndefined (LaunchPermissionList), "ProductCodes" :: NullOrUndefined (ProductCodeList), "Description" :: NullOrUndefined (AttributeValue), "KernelId" :: NullOrUndefined (AttributeValue), "RamdiskId" :: NullOrUndefined (AttributeValue), "SriovNetSupport" :: NullOrUndefined (AttributeValue) }
```

<p>Describes an image attribute.</p>

#### `ImageAttributeName`

``` purescript
newtype ImageAttributeName
  = ImageAttributeName String
```

#### `ImageDiskContainer`

``` purescript
newtype ImageDiskContainer
  = ImageDiskContainer { "Description" :: NullOrUndefined (String), "DeviceName" :: NullOrUndefined (String), "Format" :: NullOrUndefined (String), "SnapshotId" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String), "UserBucket" :: NullOrUndefined (UserBucket) }
```

<p>Describes the disk container object for an import image task.</p>

#### `ImageDiskContainerList`

``` purescript
newtype ImageDiskContainerList
  = ImageDiskContainerList (Array ImageDiskContainer)
```

#### `ImageIdStringList`

``` purescript
newtype ImageIdStringList
  = ImageIdStringList (Array String)
```

#### `ImageList`

``` purescript
newtype ImageList
  = ImageList (Array Image)
```

#### `ImageState`

``` purescript
newtype ImageState
  = ImageState String
```

#### `ImageTypeValues`

``` purescript
newtype ImageTypeValues
  = ImageTypeValues String
```

#### `ImportImageRequest`

``` purescript
newtype ImportImageRequest
  = ImportImageRequest { "Architecture" :: NullOrUndefined (String), "ClientData" :: NullOrUndefined (ClientData), "ClientToken" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "DiskContainers" :: NullOrUndefined (ImageDiskContainerList), "DryRun" :: NullOrUndefined (Boolean), "Hypervisor" :: NullOrUndefined (String), "LicenseType" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "RoleName" :: NullOrUndefined (String) }
```

<p>Contains the parameters for ImportImage.</p>

#### `ImportImageResult`

``` purescript
newtype ImportImageResult
  = ImportImageResult { "Architecture" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Hypervisor" :: NullOrUndefined (String), "ImageId" :: NullOrUndefined (String), "ImportTaskId" :: NullOrUndefined (String), "LicenseType" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Progress" :: NullOrUndefined (String), "SnapshotDetails" :: NullOrUndefined (SnapshotDetailList), "Status" :: NullOrUndefined (String), "StatusMessage" :: NullOrUndefined (String) }
```

<p>Contains the output for ImportImage.</p>

#### `ImportImageTask`

``` purescript
newtype ImportImageTask
  = ImportImageTask { "Architecture" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Hypervisor" :: NullOrUndefined (String), "ImageId" :: NullOrUndefined (String), "ImportTaskId" :: NullOrUndefined (String), "LicenseType" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Progress" :: NullOrUndefined (String), "SnapshotDetails" :: NullOrUndefined (SnapshotDetailList), "Status" :: NullOrUndefined (String), "StatusMessage" :: NullOrUndefined (String) }
```

<p>Describes an import image task.</p>

#### `ImportImageTaskList`

``` purescript
newtype ImportImageTaskList
  = ImportImageTaskList (Array ImportImageTask)
```

#### `ImportInstanceLaunchSpecification`

``` purescript
newtype ImportInstanceLaunchSpecification
  = ImportInstanceLaunchSpecification { "AdditionalInfo" :: NullOrUndefined (String), "Architecture" :: NullOrUndefined (ArchitectureValues), "GroupIds" :: NullOrUndefined (SecurityGroupIdStringList), "GroupNames" :: NullOrUndefined (SecurityGroupStringList), "InstanceInitiatedShutdownBehavior" :: NullOrUndefined (ShutdownBehavior), "InstanceType" :: NullOrUndefined (InstanceType), "Monitoring" :: NullOrUndefined (Boolean), "Placement" :: NullOrUndefined (Placement), "PrivateIpAddress" :: NullOrUndefined (String), "SubnetId" :: NullOrUndefined (String), "UserData" :: NullOrUndefined (UserData) }
```

<p>Describes the launch specification for VM import.</p>

#### `ImportInstanceRequest`

``` purescript
newtype ImportInstanceRequest
  = ImportInstanceRequest { "Description" :: NullOrUndefined (String), "DiskImages" :: NullOrUndefined (DiskImageList), "DryRun" :: NullOrUndefined (Boolean), "LaunchSpecification" :: NullOrUndefined (ImportInstanceLaunchSpecification), "Platform" :: PlatformValues }
```

<p>Contains the parameters for ImportInstance.</p>

#### `ImportInstanceResult`

``` purescript
newtype ImportInstanceResult
  = ImportInstanceResult { "ConversionTask" :: NullOrUndefined (ConversionTask) }
```

<p>Contains the output for ImportInstance.</p>

#### `ImportInstanceTaskDetails`

``` purescript
newtype ImportInstanceTaskDetails
  = ImportInstanceTaskDetails { "Description" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (PlatformValues), "Volumes" :: ImportInstanceVolumeDetailSet }
```

<p>Describes an import instance task.</p>

#### `ImportInstanceVolumeDetailItem`

``` purescript
newtype ImportInstanceVolumeDetailItem
  = ImportInstanceVolumeDetailItem { "AvailabilityZone" :: String, "BytesConverted" :: Number, "Description" :: NullOrUndefined (String), "Image" :: DiskImageDescription, "Status" :: String, "StatusMessage" :: NullOrUndefined (String), "Volume" :: DiskImageVolumeDescription }
```

<p>Describes an import volume task.</p>

#### `ImportInstanceVolumeDetailSet`

``` purescript
newtype ImportInstanceVolumeDetailSet
  = ImportInstanceVolumeDetailSet (Array ImportInstanceVolumeDetailItem)
```

#### `ImportKeyPairRequest`

``` purescript
newtype ImportKeyPairRequest
  = ImportKeyPairRequest { "DryRun" :: NullOrUndefined (Boolean), "KeyName" :: String, "PublicKeyMaterial" :: String }
```

<p>Contains the parameters for ImportKeyPair.</p>

#### `ImportKeyPairResult`

``` purescript
newtype ImportKeyPairResult
  = ImportKeyPairResult { "KeyFingerprint" :: NullOrUndefined (String), "KeyName" :: NullOrUndefined (String) }
```

<p>Contains the output of ImportKeyPair.</p>

#### `ImportSnapshotRequest`

``` purescript
newtype ImportSnapshotRequest
  = ImportSnapshotRequest { "ClientData" :: NullOrUndefined (ClientData), "ClientToken" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "DiskContainer" :: NullOrUndefined (SnapshotDiskContainer), "DryRun" :: NullOrUndefined (Boolean), "RoleName" :: NullOrUndefined (String) }
```

<p>Contains the parameters for ImportSnapshot.</p>

#### `ImportSnapshotResult`

``` purescript
newtype ImportSnapshotResult
  = ImportSnapshotResult { "Description" :: NullOrUndefined (String), "ImportTaskId" :: NullOrUndefined (String), "SnapshotTaskDetail" :: NullOrUndefined (SnapshotTaskDetail) }
```

<p>Contains the output for ImportSnapshot.</p>

#### `ImportSnapshotTask`

``` purescript
newtype ImportSnapshotTask
  = ImportSnapshotTask { "Description" :: NullOrUndefined (String), "ImportTaskId" :: NullOrUndefined (String), "SnapshotTaskDetail" :: NullOrUndefined (SnapshotTaskDetail) }
```

<p>Describes an import snapshot task.</p>

#### `ImportSnapshotTaskList`

``` purescript
newtype ImportSnapshotTaskList
  = ImportSnapshotTaskList (Array ImportSnapshotTask)
```

#### `ImportTaskIdList`

``` purescript
newtype ImportTaskIdList
  = ImportTaskIdList (Array String)
```

#### `ImportVolumeRequest`

``` purescript
newtype ImportVolumeRequest
  = ImportVolumeRequest { "AvailabilityZone" :: String, "Description" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "Image" :: DiskImageDetail, "Volume" :: VolumeDetail }
```

<p>Contains the parameters for ImportVolume.</p>

#### `ImportVolumeResult`

``` purescript
newtype ImportVolumeResult
  = ImportVolumeResult { "ConversionTask" :: NullOrUndefined (ConversionTask) }
```

<p>Contains the output for ImportVolume.</p>

#### `ImportVolumeTaskDetails`

``` purescript
newtype ImportVolumeTaskDetails
  = ImportVolumeTaskDetails { "AvailabilityZone" :: String, "BytesConverted" :: Number, "Description" :: NullOrUndefined (String), "Image" :: DiskImageDescription, "Volume" :: DiskImageVolumeDescription }
```

<p>Describes an import volume task.</p>

#### `Instance`

``` purescript
newtype Instance
  = Instance { "AmiLaunchIndex" :: NullOrUndefined (Int), "ImageId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (InstanceType), "KernelId" :: NullOrUndefined (String), "KeyName" :: NullOrUndefined (String), "LaunchTime" :: NullOrUndefined (DateTime), "Monitoring" :: NullOrUndefined (Monitoring), "Placement" :: NullOrUndefined (Placement), "Platform" :: NullOrUndefined (PlatformValues), "PrivateDnsName" :: NullOrUndefined (String), "PrivateIpAddress" :: NullOrUndefined (String), "ProductCodes" :: NullOrUndefined (ProductCodeList), "PublicDnsName" :: NullOrUndefined (String), "PublicIpAddress" :: NullOrUndefined (String), "RamdiskId" :: NullOrUndefined (String), "State" :: NullOrUndefined (InstanceState), "StateTransitionReason" :: NullOrUndefined (String), "SubnetId" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "Architecture" :: NullOrUndefined (ArchitectureValues), "BlockDeviceMappings" :: NullOrUndefined (InstanceBlockDeviceMappingList), "ClientToken" :: NullOrUndefined (String), "EbsOptimized" :: NullOrUndefined (Boolean), "EnaSupport" :: NullOrUndefined (Boolean), "Hypervisor" :: NullOrUndefined (HypervisorType), "IamInstanceProfile" :: NullOrUndefined (IamInstanceProfile), "InstanceLifecycle" :: NullOrUndefined (InstanceLifecycleType), "ElasticGpuAssociations" :: NullOrUndefined (ElasticGpuAssociationList), "NetworkInterfaces" :: NullOrUndefined (InstanceNetworkInterfaceList), "RootDeviceName" :: NullOrUndefined (String), "RootDeviceType" :: NullOrUndefined (DeviceType), "SecurityGroups" :: NullOrUndefined (GroupIdentifierList), "SourceDestCheck" :: NullOrUndefined (Boolean), "SpotInstanceRequestId" :: NullOrUndefined (String), "SriovNetSupport" :: NullOrUndefined (String), "StateReason" :: NullOrUndefined (StateReason), "Tags" :: NullOrUndefined (TagList), "VirtualizationType" :: NullOrUndefined (VirtualizationType) }
```

<p>Describes an instance.</p>

#### `InstanceAttribute`

``` purescript
newtype InstanceAttribute
  = InstanceAttribute { "Groups" :: NullOrUndefined (GroupIdentifierList), "BlockDeviceMappings" :: NullOrUndefined (InstanceBlockDeviceMappingList), "DisableApiTermination" :: NullOrUndefined (AttributeBooleanValue), "EnaSupport" :: NullOrUndefined (AttributeBooleanValue), "EbsOptimized" :: NullOrUndefined (AttributeBooleanValue), "InstanceId" :: NullOrUndefined (String), "InstanceInitiatedShutdownBehavior" :: NullOrUndefined (AttributeValue), "InstanceType" :: NullOrUndefined (AttributeValue), "KernelId" :: NullOrUndefined (AttributeValue), "ProductCodes" :: NullOrUndefined (ProductCodeList), "RamdiskId" :: NullOrUndefined (AttributeValue), "RootDeviceName" :: NullOrUndefined (AttributeValue), "SourceDestCheck" :: NullOrUndefined (AttributeBooleanValue), "SriovNetSupport" :: NullOrUndefined (AttributeValue), "UserData" :: NullOrUndefined (AttributeValue) }
```

<p>Describes an instance attribute.</p>

#### `InstanceAttributeName`

``` purescript
newtype InstanceAttributeName
  = InstanceAttributeName String
```

#### `InstanceBlockDeviceMapping`

``` purescript
newtype InstanceBlockDeviceMapping
  = InstanceBlockDeviceMapping { "DeviceName" :: NullOrUndefined (String), "Ebs" :: NullOrUndefined (EbsInstanceBlockDevice) }
```

<p>Describes a block device mapping.</p>

#### `InstanceBlockDeviceMappingList`

``` purescript
newtype InstanceBlockDeviceMappingList
  = InstanceBlockDeviceMappingList (Array InstanceBlockDeviceMapping)
```

#### `InstanceBlockDeviceMappingSpecification`

``` purescript
newtype InstanceBlockDeviceMappingSpecification
  = InstanceBlockDeviceMappingSpecification { "DeviceName" :: NullOrUndefined (String), "Ebs" :: NullOrUndefined (EbsInstanceBlockDeviceSpecification), "NoDevice" :: NullOrUndefined (String), "VirtualName" :: NullOrUndefined (String) }
```

<p>Describes a block device mapping entry.</p>

#### `InstanceBlockDeviceMappingSpecificationList`

``` purescript
newtype InstanceBlockDeviceMappingSpecificationList
  = InstanceBlockDeviceMappingSpecificationList (Array InstanceBlockDeviceMappingSpecification)
```

#### `InstanceCapacity`

``` purescript
newtype InstanceCapacity
  = InstanceCapacity { "AvailableCapacity" :: NullOrUndefined (Int), "InstanceType" :: NullOrUndefined (String), "TotalCapacity" :: NullOrUndefined (Int) }
```

<p>Information about the instance type that the Dedicated Host supports.</p>

#### `InstanceCount`

``` purescript
newtype InstanceCount
  = InstanceCount { "InstanceCount" :: NullOrUndefined (Int), "State" :: NullOrUndefined (ListingState) }
```

<p>Describes a Reserved Instance listing state.</p>

#### `InstanceCountList`

``` purescript
newtype InstanceCountList
  = InstanceCountList (Array InstanceCount)
```

#### `InstanceCreditSpecification`

``` purescript
newtype InstanceCreditSpecification
  = InstanceCreditSpecification { "InstanceId" :: NullOrUndefined (String), "CpuCredits" :: NullOrUndefined (String) }
```

<p>Describes the credit option for CPU usage of a T2 instance. </p>

#### `InstanceCreditSpecificationList`

``` purescript
newtype InstanceCreditSpecificationList
  = InstanceCreditSpecificationList (Array InstanceCreditSpecification)
```

#### `InstanceCreditSpecificationListRequest`

``` purescript
newtype InstanceCreditSpecificationListRequest
  = InstanceCreditSpecificationListRequest (Array InstanceCreditSpecificationRequest)
```

#### `InstanceCreditSpecificationRequest`

``` purescript
newtype InstanceCreditSpecificationRequest
  = InstanceCreditSpecificationRequest { "InstanceId" :: NullOrUndefined (String), "CpuCredits" :: NullOrUndefined (String) }
```

<p>Describes the credit option for CPU usage of a T2 instance.</p>

#### `InstanceExportDetails`

``` purescript
newtype InstanceExportDetails
  = InstanceExportDetails { "InstanceId" :: NullOrUndefined (String), "TargetEnvironment" :: NullOrUndefined (ExportEnvironment) }
```

<p>Describes an instance to export.</p>

#### `InstanceHealthStatus`

``` purescript
newtype InstanceHealthStatus
  = InstanceHealthStatus String
```

#### `InstanceIdSet`

``` purescript
newtype InstanceIdSet
  = InstanceIdSet (Array String)
```

#### `InstanceIdStringList`

``` purescript
newtype InstanceIdStringList
  = InstanceIdStringList (Array String)
```

#### `InstanceInterruptionBehavior`

``` purescript
newtype InstanceInterruptionBehavior
  = InstanceInterruptionBehavior String
```

#### `InstanceIpv6Address`

``` purescript
newtype InstanceIpv6Address
  = InstanceIpv6Address { "Ipv6Address" :: NullOrUndefined (String) }
```

<p>Describes an IPv6 address.</p>

#### `InstanceIpv6AddressList`

``` purescript
newtype InstanceIpv6AddressList
  = InstanceIpv6AddressList (Array InstanceIpv6Address)
```

#### `InstanceIpv6AddressListRequest`

``` purescript
newtype InstanceIpv6AddressListRequest
  = InstanceIpv6AddressListRequest (Array InstanceIpv6AddressRequest)
```

#### `InstanceIpv6AddressRequest`

``` purescript
newtype InstanceIpv6AddressRequest
  = InstanceIpv6AddressRequest { "Ipv6Address" :: NullOrUndefined (String) }
```

<p>Describes an IPv6 address.</p>

#### `InstanceLifecycleType`

``` purescript
newtype InstanceLifecycleType
  = InstanceLifecycleType String
```

#### `InstanceList`

``` purescript
newtype InstanceList
  = InstanceList (Array Instance)
```

#### `InstanceMarketOptionsRequest`

``` purescript
newtype InstanceMarketOptionsRequest
  = InstanceMarketOptionsRequest { "MarketType" :: NullOrUndefined (MarketType), "SpotOptions" :: NullOrUndefined (SpotMarketOptions) }
```

<p>Describes the market (purchasing) option for the instances.</p>

#### `InstanceMonitoring`

``` purescript
newtype InstanceMonitoring
  = InstanceMonitoring { "InstanceId" :: NullOrUndefined (String), "Monitoring" :: NullOrUndefined (Monitoring) }
```

<p>Describes the monitoring of an instance.</p>

#### `InstanceMonitoringList`

``` purescript
newtype InstanceMonitoringList
  = InstanceMonitoringList (Array InstanceMonitoring)
```

#### `InstanceNetworkInterface`

``` purescript
newtype InstanceNetworkInterface
  = InstanceNetworkInterface { "Association" :: NullOrUndefined (InstanceNetworkInterfaceAssociation), "Attachment" :: NullOrUndefined (InstanceNetworkInterfaceAttachment), "Description" :: NullOrUndefined (String), "Groups" :: NullOrUndefined (GroupIdentifierList), "Ipv6Addresses" :: NullOrUndefined (InstanceIpv6AddressList), "MacAddress" :: NullOrUndefined (String), "NetworkInterfaceId" :: NullOrUndefined (String), "OwnerId" :: NullOrUndefined (String), "PrivateDnsName" :: NullOrUndefined (String), "PrivateIpAddress" :: NullOrUndefined (String), "PrivateIpAddresses" :: NullOrUndefined (InstancePrivateIpAddressList), "SourceDestCheck" :: NullOrUndefined (Boolean), "Status" :: NullOrUndefined (NetworkInterfaceStatus), "SubnetId" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String) }
```

<p>Describes a network interface.</p>

#### `InstanceNetworkInterfaceAssociation`

``` purescript
newtype InstanceNetworkInterfaceAssociation
  = InstanceNetworkInterfaceAssociation { "IpOwnerId" :: NullOrUndefined (String), "PublicDnsName" :: NullOrUndefined (String), "PublicIp" :: NullOrUndefined (String) }
```

<p>Describes association information for an Elastic IP address (IPv4).</p>

#### `InstanceNetworkInterfaceAttachment`

``` purescript
newtype InstanceNetworkInterfaceAttachment
  = InstanceNetworkInterfaceAttachment { "AttachTime" :: NullOrUndefined (DateTime), "AttachmentId" :: NullOrUndefined (String), "DeleteOnTermination" :: NullOrUndefined (Boolean), "DeviceIndex" :: NullOrUndefined (Int), "Status" :: NullOrUndefined (AttachmentStatus) }
```

<p>Describes a network interface attachment.</p>

#### `InstanceNetworkInterfaceList`

``` purescript
newtype InstanceNetworkInterfaceList
  = InstanceNetworkInterfaceList (Array InstanceNetworkInterface)
```

#### `InstanceNetworkInterfaceSpecification`

``` purescript
newtype InstanceNetworkInterfaceSpecification
  = InstanceNetworkInterfaceSpecification { "AssociatePublicIpAddress" :: NullOrUndefined (Boolean), "DeleteOnTermination" :: NullOrUndefined (Boolean), "Description" :: NullOrUndefined (String), "DeviceIndex" :: NullOrUndefined (Int), "Groups" :: NullOrUndefined (SecurityGroupIdStringList), "Ipv6AddressCount" :: NullOrUndefined (Int), "Ipv6Addresses" :: NullOrUndefined (InstanceIpv6AddressList), "NetworkInterfaceId" :: NullOrUndefined (String), "PrivateIpAddress" :: NullOrUndefined (String), "PrivateIpAddresses" :: NullOrUndefined (PrivateIpAddressSpecificationList), "SecondaryPrivateIpAddressCount" :: NullOrUndefined (Int), "SubnetId" :: NullOrUndefined (String) }
```

<p>Describes a network interface.</p>

#### `InstanceNetworkInterfaceSpecificationList`

``` purescript
newtype InstanceNetworkInterfaceSpecificationList
  = InstanceNetworkInterfaceSpecificationList (Array InstanceNetworkInterfaceSpecification)
```

#### `InstancePrivateIpAddress`

``` purescript
newtype InstancePrivateIpAddress
  = InstancePrivateIpAddress { "Association" :: NullOrUndefined (InstanceNetworkInterfaceAssociation), "Primary" :: NullOrUndefined (Boolean), "PrivateDnsName" :: NullOrUndefined (String), "PrivateIpAddress" :: NullOrUndefined (String) }
```

<p>Describes a private IPv4 address.</p>

#### `InstancePrivateIpAddressList`

``` purescript
newtype InstancePrivateIpAddressList
  = InstancePrivateIpAddressList (Array InstancePrivateIpAddress)
```

#### `InstanceState`

``` purescript
newtype InstanceState
  = InstanceState { "Code" :: NullOrUndefined (Int), "Name" :: NullOrUndefined (InstanceStateName) }
```

<p>Describes the current state of an instance.</p>

#### `InstanceStateChange`

``` purescript
newtype InstanceStateChange
  = InstanceStateChange { "CurrentState" :: NullOrUndefined (InstanceState), "InstanceId" :: NullOrUndefined (String), "PreviousState" :: NullOrUndefined (InstanceState) }
```

<p>Describes an instance state change.</p>

#### `InstanceStateChangeList`

``` purescript
newtype InstanceStateChangeList
  = InstanceStateChangeList (Array InstanceStateChange)
```

#### `InstanceStateName`

``` purescript
newtype InstanceStateName
  = InstanceStateName String
```

#### `InstanceStatus`

``` purescript
newtype InstanceStatus
  = InstanceStatus { "AvailabilityZone" :: NullOrUndefined (String), "Events" :: NullOrUndefined (InstanceStatusEventList), "InstanceId" :: NullOrUndefined (String), "InstanceState" :: NullOrUndefined (InstanceState), "InstanceStatus" :: NullOrUndefined (InstanceStatusSummary), "SystemStatus" :: NullOrUndefined (InstanceStatusSummary) }
```

<p>Describes the status of an instance.</p>

#### `InstanceStatusDetails`

``` purescript
newtype InstanceStatusDetails
  = InstanceStatusDetails { "ImpairedSince" :: NullOrUndefined (DateTime), "Name" :: NullOrUndefined (StatusName), "Status" :: NullOrUndefined (StatusType) }
```

<p>Describes the instance status.</p>

#### `InstanceStatusDetailsList`

``` purescript
newtype InstanceStatusDetailsList
  = InstanceStatusDetailsList (Array InstanceStatusDetails)
```

#### `InstanceStatusEvent`

``` purescript
newtype InstanceStatusEvent
  = InstanceStatusEvent { "Code" :: NullOrUndefined (EventCode), "Description" :: NullOrUndefined (String), "NotAfter" :: NullOrUndefined (DateTime), "NotBefore" :: NullOrUndefined (DateTime) }
```

<p>Describes a scheduled event for an instance.</p>

#### `InstanceStatusEventList`

``` purescript
newtype InstanceStatusEventList
  = InstanceStatusEventList (Array InstanceStatusEvent)
```

#### `InstanceStatusList`

``` purescript
newtype InstanceStatusList
  = InstanceStatusList (Array InstanceStatus)
```

#### `InstanceStatusSummary`

``` purescript
newtype InstanceStatusSummary
  = InstanceStatusSummary { "Details" :: NullOrUndefined (InstanceStatusDetailsList), "Status" :: NullOrUndefined (SummaryStatus) }
```

<p>Describes the status of an instance.</p>

#### `InstanceType`

``` purescript
newtype InstanceType
  = InstanceType String
```

#### `InstanceTypeList`

``` purescript
newtype InstanceTypeList
  = InstanceTypeList (Array InstanceType)
```

#### `InterfacePermissionType`

``` purescript
newtype InterfacePermissionType
  = InterfacePermissionType String
```

#### `InternetGateway`

``` purescript
newtype InternetGateway
  = InternetGateway { "Attachments" :: NullOrUndefined (InternetGatewayAttachmentList), "InternetGatewayId" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes an Internet gateway.</p>

#### `InternetGatewayAttachment`

``` purescript
newtype InternetGatewayAttachment
  = InternetGatewayAttachment { "State" :: NullOrUndefined (AttachmentStatus), "VpcId" :: NullOrUndefined (String) }
```

<p>Describes the attachment of a VPC to an Internet gateway or an egress-only Internet gateway.</p>

#### `InternetGatewayAttachmentList`

``` purescript
newtype InternetGatewayAttachmentList
  = InternetGatewayAttachmentList (Array InternetGatewayAttachment)
```

#### `InternetGatewayList`

``` purescript
newtype InternetGatewayList
  = InternetGatewayList (Array InternetGateway)
```

#### `IpPermission`

``` purescript
newtype IpPermission
  = IpPermission { "FromPort" :: NullOrUndefined (Int), "IpProtocol" :: NullOrUndefined (String), "IpRanges" :: NullOrUndefined (IpRangeList), "Ipv6Ranges" :: NullOrUndefined (Ipv6RangeList), "PrefixListIds" :: NullOrUndefined (PrefixListIdList), "ToPort" :: NullOrUndefined (Int), "UserIdGroupPairs" :: NullOrUndefined (UserIdGroupPairList) }
```

<p>Describes a set of permissions for a security group rule.</p>

#### `IpPermissionList`

``` purescript
newtype IpPermissionList
  = IpPermissionList (Array IpPermission)
```

#### `IpRange`

``` purescript
newtype IpRange
  = IpRange { "CidrIp" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

<p>Describes an IPv4 range.</p>

#### `IpRangeList`

``` purescript
newtype IpRangeList
  = IpRangeList (Array IpRange)
```

#### `IpRanges`

``` purescript
newtype IpRanges
  = IpRanges (Array String)
```

#### `Ipv6Address`

``` purescript
newtype Ipv6Address
  = Ipv6Address String
```

#### `Ipv6AddressList`

``` purescript
newtype Ipv6AddressList
  = Ipv6AddressList (Array String)
```

#### `Ipv6CidrBlock`

``` purescript
newtype Ipv6CidrBlock
  = Ipv6CidrBlock { "Ipv6CidrBlock" :: NullOrUndefined (String) }
```

<p>Describes an IPv6 CIDR block.</p>

#### `Ipv6CidrBlockSet`

``` purescript
newtype Ipv6CidrBlockSet
  = Ipv6CidrBlockSet (Array Ipv6CidrBlock)
```

#### `Ipv6Range`

``` purescript
newtype Ipv6Range
  = Ipv6Range { "CidrIpv6" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

<p>[EC2-VPC only] Describes an IPv6 range.</p>

#### `Ipv6RangeList`

``` purescript
newtype Ipv6RangeList
  = Ipv6RangeList (Array Ipv6Range)
```

#### `KeyNameStringList`

``` purescript
newtype KeyNameStringList
  = KeyNameStringList (Array String)
```

#### `KeyPair`

``` purescript
newtype KeyPair
  = KeyPair { "KeyFingerprint" :: NullOrUndefined (String), "KeyMaterial" :: NullOrUndefined (String), "KeyName" :: NullOrUndefined (String) }
```

<p>Describes a key pair.</p>

#### `KeyPairInfo`

``` purescript
newtype KeyPairInfo
  = KeyPairInfo { "KeyFingerprint" :: NullOrUndefined (String), "KeyName" :: NullOrUndefined (String) }
```

<p>Describes a key pair.</p>

#### `KeyPairList`

``` purescript
newtype KeyPairList
  = KeyPairList (Array KeyPairInfo)
```

#### `LaunchPermission`

``` purescript
newtype LaunchPermission
  = LaunchPermission { "Group" :: NullOrUndefined (PermissionGroup), "UserId" :: NullOrUndefined (String) }
```

<p>Describes a launch permission.</p>

#### `LaunchPermissionList`

``` purescript
newtype LaunchPermissionList
  = LaunchPermissionList (Array LaunchPermission)
```

#### `LaunchPermissionModifications`

``` purescript
newtype LaunchPermissionModifications
  = LaunchPermissionModifications { "Add" :: NullOrUndefined (LaunchPermissionList), "Remove" :: NullOrUndefined (LaunchPermissionList) }
```

<p>Describes a launch permission modification.</p>

#### `LaunchSpecification`

``` purescript
newtype LaunchSpecification
  = LaunchSpecification { "UserData" :: NullOrUndefined (String), "SecurityGroups" :: NullOrUndefined (GroupIdentifierList), "AddressingType" :: NullOrUndefined (String), "BlockDeviceMappings" :: NullOrUndefined (BlockDeviceMappingList), "EbsOptimized" :: NullOrUndefined (Boolean), "IamInstanceProfile" :: NullOrUndefined (IamInstanceProfileSpecification), "ImageId" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (InstanceType), "KernelId" :: NullOrUndefined (String), "KeyName" :: NullOrUndefined (String), "NetworkInterfaces" :: NullOrUndefined (InstanceNetworkInterfaceSpecificationList), "Placement" :: NullOrUndefined (SpotPlacement), "RamdiskId" :: NullOrUndefined (String), "SubnetId" :: NullOrUndefined (String), "Monitoring" :: NullOrUndefined (RunInstancesMonitoringEnabled) }
```

<p>Describes the launch specification for an instance.</p>

#### `LaunchSpecsList`

``` purescript
newtype LaunchSpecsList
  = LaunchSpecsList (Array SpotFleetLaunchSpecification)
```

#### `LaunchTemplate`

``` purescript
newtype LaunchTemplate
  = LaunchTemplate { "LaunchTemplateId" :: NullOrUndefined (String), "LaunchTemplateName" :: NullOrUndefined (LaunchTemplateName), "CreateTime" :: NullOrUndefined (DateTime), "CreatedBy" :: NullOrUndefined (String), "DefaultVersionNumber" :: NullOrUndefined (Number), "LatestVersionNumber" :: NullOrUndefined (Number), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a launch template.</p>

#### `LaunchTemplateBlockDeviceMapping`

``` purescript
newtype LaunchTemplateBlockDeviceMapping
  = LaunchTemplateBlockDeviceMapping { "DeviceName" :: NullOrUndefined (String), "VirtualName" :: NullOrUndefined (String), "Ebs" :: NullOrUndefined (LaunchTemplateEbsBlockDevice), "NoDevice" :: NullOrUndefined (String) }
```

<p>Describes a block device mapping.</p>

#### `LaunchTemplateBlockDeviceMappingList`

``` purescript
newtype LaunchTemplateBlockDeviceMappingList
  = LaunchTemplateBlockDeviceMappingList (Array LaunchTemplateBlockDeviceMapping)
```

#### `LaunchTemplateBlockDeviceMappingRequest`

``` purescript
newtype LaunchTemplateBlockDeviceMappingRequest
  = LaunchTemplateBlockDeviceMappingRequest { "DeviceName" :: NullOrUndefined (String), "VirtualName" :: NullOrUndefined (String), "Ebs" :: NullOrUndefined (LaunchTemplateEbsBlockDeviceRequest), "NoDevice" :: NullOrUndefined (String) }
```

<p>Describes a block device mapping.</p>

#### `LaunchTemplateBlockDeviceMappingRequestList`

``` purescript
newtype LaunchTemplateBlockDeviceMappingRequestList
  = LaunchTemplateBlockDeviceMappingRequestList (Array LaunchTemplateBlockDeviceMappingRequest)
```

#### `LaunchTemplateConfig`

``` purescript
newtype LaunchTemplateConfig
  = LaunchTemplateConfig { "LaunchTemplateSpecification" :: NullOrUndefined (FleetLaunchTemplateSpecification), "Overrides" :: NullOrUndefined (LaunchTemplateOverridesList) }
```

<p>Describes a launch template and overrides.</p>

#### `LaunchTemplateConfigList`

``` purescript
newtype LaunchTemplateConfigList
  = LaunchTemplateConfigList (Array LaunchTemplateConfig)
```

#### `LaunchTemplateEbsBlockDevice`

``` purescript
newtype LaunchTemplateEbsBlockDevice
  = LaunchTemplateEbsBlockDevice { "Encrypted" :: NullOrUndefined (Boolean), "DeleteOnTermination" :: NullOrUndefined (Boolean), "Iops" :: NullOrUndefined (Int), "KmsKeyId" :: NullOrUndefined (String), "SnapshotId" :: NullOrUndefined (String), "VolumeSize" :: NullOrUndefined (Int), "VolumeType" :: NullOrUndefined (VolumeType) }
```

<p>Describes a block device for an EBS volume.</p>

#### `LaunchTemplateEbsBlockDeviceRequest`

``` purescript
newtype LaunchTemplateEbsBlockDeviceRequest
  = LaunchTemplateEbsBlockDeviceRequest { "Encrypted" :: NullOrUndefined (Boolean), "DeleteOnTermination" :: NullOrUndefined (Boolean), "Iops" :: NullOrUndefined (Int), "KmsKeyId" :: NullOrUndefined (String), "SnapshotId" :: NullOrUndefined (String), "VolumeSize" :: NullOrUndefined (Int), "VolumeType" :: NullOrUndefined (VolumeType) }
```

<p>The parameters for a block device for an EBS volume.</p>

#### `LaunchTemplateErrorCode`

``` purescript
newtype LaunchTemplateErrorCode
  = LaunchTemplateErrorCode String
```

#### `LaunchTemplateIamInstanceProfileSpecification`

``` purescript
newtype LaunchTemplateIamInstanceProfileSpecification
  = LaunchTemplateIamInstanceProfileSpecification { "Arn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

<p>Describes an IAM instance profile.</p>

#### `LaunchTemplateIamInstanceProfileSpecificationRequest`

``` purescript
newtype LaunchTemplateIamInstanceProfileSpecificationRequest
  = LaunchTemplateIamInstanceProfileSpecificationRequest { "Arn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

<p>An IAM instance profile.</p>

#### `LaunchTemplateInstanceMarketOptions`

``` purescript
newtype LaunchTemplateInstanceMarketOptions
  = LaunchTemplateInstanceMarketOptions { "MarketType" :: NullOrUndefined (MarketType), "SpotOptions" :: NullOrUndefined (LaunchTemplateSpotMarketOptions) }
```

<p>The market (purchasing) option for the instances.</p>

#### `LaunchTemplateInstanceMarketOptionsRequest`

``` purescript
newtype LaunchTemplateInstanceMarketOptionsRequest
  = LaunchTemplateInstanceMarketOptionsRequest { "MarketType" :: NullOrUndefined (MarketType), "SpotOptions" :: NullOrUndefined (LaunchTemplateSpotMarketOptionsRequest) }
```

<p>The market (purchasing) option for the instances.</p>

#### `LaunchTemplateInstanceNetworkInterfaceSpecification`

``` purescript
newtype LaunchTemplateInstanceNetworkInterfaceSpecification
  = LaunchTemplateInstanceNetworkInterfaceSpecification { "AssociatePublicIpAddress" :: NullOrUndefined (Boolean), "DeleteOnTermination" :: NullOrUndefined (Boolean), "Description" :: NullOrUndefined (String), "DeviceIndex" :: NullOrUndefined (Int), "Groups" :: NullOrUndefined (GroupIdStringList), "Ipv6AddressCount" :: NullOrUndefined (Int), "Ipv6Addresses" :: NullOrUndefined (InstanceIpv6AddressList), "NetworkInterfaceId" :: NullOrUndefined (String), "PrivateIpAddress" :: NullOrUndefined (String), "PrivateIpAddresses" :: NullOrUndefined (PrivateIpAddressSpecificationList), "SecondaryPrivateIpAddressCount" :: NullOrUndefined (Int), "SubnetId" :: NullOrUndefined (String) }
```

<p>Describes a network interface.</p>

#### `LaunchTemplateInstanceNetworkInterfaceSpecificationList`

``` purescript
newtype LaunchTemplateInstanceNetworkInterfaceSpecificationList
  = LaunchTemplateInstanceNetworkInterfaceSpecificationList (Array LaunchTemplateInstanceNetworkInterfaceSpecification)
```

#### `LaunchTemplateInstanceNetworkInterfaceSpecificationRequest`

``` purescript
newtype LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  = LaunchTemplateInstanceNetworkInterfaceSpecificationRequest { "AssociatePublicIpAddress" :: NullOrUndefined (Boolean), "DeleteOnTermination" :: NullOrUndefined (Boolean), "Description" :: NullOrUndefined (String), "DeviceIndex" :: NullOrUndefined (Int), "Groups" :: NullOrUndefined (SecurityGroupIdStringList), "Ipv6AddressCount" :: NullOrUndefined (Int), "Ipv6Addresses" :: NullOrUndefined (InstanceIpv6AddressListRequest), "NetworkInterfaceId" :: NullOrUndefined (String), "PrivateIpAddress" :: NullOrUndefined (String), "PrivateIpAddresses" :: NullOrUndefined (PrivateIpAddressSpecificationList), "SecondaryPrivateIpAddressCount" :: NullOrUndefined (Int), "SubnetId" :: NullOrUndefined (String) }
```

<p>The parameters for a network interface.</p>

#### `LaunchTemplateInstanceNetworkInterfaceSpecificationRequestList`

``` purescript
newtype LaunchTemplateInstanceNetworkInterfaceSpecificationRequestList
  = LaunchTemplateInstanceNetworkInterfaceSpecificationRequestList (Array LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
```

#### `LaunchTemplateName`

``` purescript
newtype LaunchTemplateName
  = LaunchTemplateName String
```

#### `LaunchTemplateNameStringList`

``` purescript
newtype LaunchTemplateNameStringList
  = LaunchTemplateNameStringList (Array LaunchTemplateName)
```

#### `LaunchTemplateOverrides`

``` purescript
newtype LaunchTemplateOverrides
  = LaunchTemplateOverrides { "InstanceType" :: NullOrUndefined (InstanceType), "SpotPrice" :: NullOrUndefined (String), "SubnetId" :: NullOrUndefined (String), "AvailabilityZone" :: NullOrUndefined (String), "WeightedCapacity" :: NullOrUndefined (Number) }
```

<p>Describes overrides for a launch template.</p>

#### `LaunchTemplateOverridesList`

``` purescript
newtype LaunchTemplateOverridesList
  = LaunchTemplateOverridesList (Array LaunchTemplateOverrides)
```

#### `LaunchTemplatePlacement`

``` purescript
newtype LaunchTemplatePlacement
  = LaunchTemplatePlacement { "AvailabilityZone" :: NullOrUndefined (String), "Affinity" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String), "HostId" :: NullOrUndefined (String), "Tenancy" :: NullOrUndefined (Tenancy), "SpreadDomain" :: NullOrUndefined (String) }
```

<p>Describes the placement of an instance.</p>

#### `LaunchTemplatePlacementRequest`

``` purescript
newtype LaunchTemplatePlacementRequest
  = LaunchTemplatePlacementRequest { "AvailabilityZone" :: NullOrUndefined (String), "Affinity" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String), "HostId" :: NullOrUndefined (String), "Tenancy" :: NullOrUndefined (Tenancy), "SpreadDomain" :: NullOrUndefined (String) }
```

<p>The placement for the instance.</p>

#### `LaunchTemplateSet`

``` purescript
newtype LaunchTemplateSet
  = LaunchTemplateSet (Array LaunchTemplate)
```

#### `LaunchTemplateSpecification`

``` purescript
newtype LaunchTemplateSpecification
  = LaunchTemplateSpecification { "LaunchTemplateId" :: NullOrUndefined (String), "LaunchTemplateName" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

<p>The launch template to use. You must specify either the launch template ID or launch template name in the request.</p>

#### `LaunchTemplateSpotMarketOptions`

``` purescript
newtype LaunchTemplateSpotMarketOptions
  = LaunchTemplateSpotMarketOptions { "MaxPrice" :: NullOrUndefined (String), "SpotInstanceType" :: NullOrUndefined (SpotInstanceType), "BlockDurationMinutes" :: NullOrUndefined (Int), "ValidUntil" :: NullOrUndefined (DateTime), "InstanceInterruptionBehavior" :: NullOrUndefined (InstanceInterruptionBehavior) }
```

<p>The options for Spot Instances.</p>

#### `LaunchTemplateSpotMarketOptionsRequest`

``` purescript
newtype LaunchTemplateSpotMarketOptionsRequest
  = LaunchTemplateSpotMarketOptionsRequest { "MaxPrice" :: NullOrUndefined (String), "SpotInstanceType" :: NullOrUndefined (SpotInstanceType), "BlockDurationMinutes" :: NullOrUndefined (Int), "ValidUntil" :: NullOrUndefined (DateTime), "InstanceInterruptionBehavior" :: NullOrUndefined (InstanceInterruptionBehavior) }
```

<p>The options for Spot Instances.</p>

#### `LaunchTemplateTagSpecification`

``` purescript
newtype LaunchTemplateTagSpecification
  = LaunchTemplateTagSpecification { "ResourceType" :: NullOrUndefined (ResourceType), "Tags" :: NullOrUndefined (TagList) }
```

<p>The tag specification for the launch template.</p>

#### `LaunchTemplateTagSpecificationList`

``` purescript
newtype LaunchTemplateTagSpecificationList
  = LaunchTemplateTagSpecificationList (Array LaunchTemplateTagSpecification)
```

#### `LaunchTemplateTagSpecificationRequest`

``` purescript
newtype LaunchTemplateTagSpecificationRequest
  = LaunchTemplateTagSpecificationRequest { "ResourceType" :: NullOrUndefined (ResourceType), "Tags" :: NullOrUndefined (TagList) }
```

<p>The tags specification for the launch template.</p>

#### `LaunchTemplateTagSpecificationRequestList`

``` purescript
newtype LaunchTemplateTagSpecificationRequestList
  = LaunchTemplateTagSpecificationRequestList (Array LaunchTemplateTagSpecificationRequest)
```

#### `LaunchTemplateVersion`

``` purescript
newtype LaunchTemplateVersion
  = LaunchTemplateVersion { "LaunchTemplateId" :: NullOrUndefined (String), "LaunchTemplateName" :: NullOrUndefined (LaunchTemplateName), "VersionNumber" :: NullOrUndefined (Number), "VersionDescription" :: NullOrUndefined (VersionDescription), "CreateTime" :: NullOrUndefined (DateTime), "CreatedBy" :: NullOrUndefined (String), "DefaultVersion" :: NullOrUndefined (Boolean), "LaunchTemplateData" :: NullOrUndefined (ResponseLaunchTemplateData) }
```

<p>Describes a launch template version.</p>

#### `LaunchTemplateVersionSet`

``` purescript
newtype LaunchTemplateVersionSet
  = LaunchTemplateVersionSet (Array LaunchTemplateVersion)
```

#### `LaunchTemplatesMonitoring`

``` purescript
newtype LaunchTemplatesMonitoring
  = LaunchTemplatesMonitoring { "Enabled" :: NullOrUndefined (Boolean) }
```

<p>Describes the monitoring for the instance.</p>

#### `LaunchTemplatesMonitoringRequest`

``` purescript
newtype LaunchTemplatesMonitoringRequest
  = LaunchTemplatesMonitoringRequest { "Enabled" :: NullOrUndefined (Boolean) }
```

<p>Describes the monitoring for the instance.</p>

#### `ListingState`

``` purescript
newtype ListingState
  = ListingState String
```

#### `ListingStatus`

``` purescript
newtype ListingStatus
  = ListingStatus String
```

#### `LoadBalancersConfig`

``` purescript
newtype LoadBalancersConfig
  = LoadBalancersConfig { "ClassicLoadBalancersConfig" :: NullOrUndefined (ClassicLoadBalancersConfig), "TargetGroupsConfig" :: NullOrUndefined (TargetGroupsConfig) }
```

<p>Describes the Classic Load Balancers and target groups to attach to a Spot Fleet request.</p>

#### `LoadPermission`

``` purescript
newtype LoadPermission
  = LoadPermission { "UserId" :: NullOrUndefined (String), "Group" :: NullOrUndefined (PermissionGroup) }
```

<p>Describes a load permission.</p>

#### `LoadPermissionList`

``` purescript
newtype LoadPermissionList
  = LoadPermissionList (Array LoadPermission)
```

#### `LoadPermissionListRequest`

``` purescript
newtype LoadPermissionListRequest
  = LoadPermissionListRequest (Array LoadPermissionRequest)
```

#### `LoadPermissionModifications`

``` purescript
newtype LoadPermissionModifications
  = LoadPermissionModifications { "Add" :: NullOrUndefined (LoadPermissionListRequest), "Remove" :: NullOrUndefined (LoadPermissionListRequest) }
```

<p>Describes modifications to the load permissions of an Amazon FPGA image (AFI).</p>

#### `LoadPermissionRequest`

``` purescript
newtype LoadPermissionRequest
  = LoadPermissionRequest { "Group" :: NullOrUndefined (PermissionGroup), "UserId" :: NullOrUndefined (String) }
```

<p>Describes a load permission.</p>

#### `MarketType`

``` purescript
newtype MarketType
  = MarketType String
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `ModifyFpgaImageAttributeRequest`

``` purescript
newtype ModifyFpgaImageAttributeRequest
  = ModifyFpgaImageAttributeRequest { "DryRun" :: NullOrUndefined (Boolean), "FpgaImageId" :: String, "Attribute" :: NullOrUndefined (FpgaImageAttributeName), "OperationType" :: NullOrUndefined (OperationType), "UserIds" :: NullOrUndefined (UserIdStringList), "UserGroups" :: NullOrUndefined (UserGroupStringList), "ProductCodes" :: NullOrUndefined (ProductCodeStringList), "LoadPermission" :: NullOrUndefined (LoadPermissionModifications), "Description" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `ModifyFpgaImageAttributeResult`

``` purescript
newtype ModifyFpgaImageAttributeResult
  = ModifyFpgaImageAttributeResult { "FpgaImageAttribute" :: NullOrUndefined (FpgaImageAttribute) }
```

#### `ModifyHostsRequest`

``` purescript
newtype ModifyHostsRequest
  = ModifyHostsRequest { "AutoPlacement" :: AutoPlacement, "HostIds" :: RequestHostIdList }
```

<p>Contains the parameters for ModifyHosts.</p>

#### `ModifyHostsResult`

``` purescript
newtype ModifyHostsResult
  = ModifyHostsResult { "Successful" :: NullOrUndefined (ResponseHostIdList), "Unsuccessful" :: NullOrUndefined (UnsuccessfulItemList) }
```

<p>Contains the output of ModifyHosts.</p>

#### `ModifyIdFormatRequest`

``` purescript
newtype ModifyIdFormatRequest
  = ModifyIdFormatRequest { "Resource" :: String, "UseLongIds" :: Boolean }
```

<p>Contains the parameters of ModifyIdFormat.</p>

#### `ModifyIdentityIdFormatRequest`

``` purescript
newtype ModifyIdentityIdFormatRequest
  = ModifyIdentityIdFormatRequest { "PrincipalArn" :: String, "Resource" :: String, "UseLongIds" :: Boolean }
```

<p>Contains the parameters of ModifyIdentityIdFormat.</p>

#### `ModifyImageAttributeRequest`

``` purescript
newtype ModifyImageAttributeRequest
  = ModifyImageAttributeRequest { "Attribute" :: NullOrUndefined (String), "Description" :: NullOrUndefined (AttributeValue), "ImageId" :: String, "LaunchPermission" :: NullOrUndefined (LaunchPermissionModifications), "OperationType" :: NullOrUndefined (OperationType), "ProductCodes" :: NullOrUndefined (ProductCodeStringList), "UserGroups" :: NullOrUndefined (UserGroupStringList), "UserIds" :: NullOrUndefined (UserIdStringList), "Value" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for ModifyImageAttribute.</p>

#### `ModifyInstanceAttributeRequest`

``` purescript
newtype ModifyInstanceAttributeRequest
  = ModifyInstanceAttributeRequest { "SourceDestCheck" :: NullOrUndefined (AttributeBooleanValue), "Attribute" :: NullOrUndefined (InstanceAttributeName), "BlockDeviceMappings" :: NullOrUndefined (InstanceBlockDeviceMappingSpecificationList), "DisableApiTermination" :: NullOrUndefined (AttributeBooleanValue), "DryRun" :: NullOrUndefined (Boolean), "EbsOptimized" :: NullOrUndefined (AttributeBooleanValue), "EnaSupport" :: NullOrUndefined (AttributeBooleanValue), "Groups" :: NullOrUndefined (GroupIdStringList), "InstanceId" :: String, "InstanceInitiatedShutdownBehavior" :: NullOrUndefined (AttributeValue), "InstanceType" :: NullOrUndefined (AttributeValue), "Kernel" :: NullOrUndefined (AttributeValue), "Ramdisk" :: NullOrUndefined (AttributeValue), "SriovNetSupport" :: NullOrUndefined (AttributeValue), "UserData" :: NullOrUndefined (BlobAttributeValue), "Value" :: NullOrUndefined (String) }
```

<p>Contains the parameters for ModifyInstanceAttribute.</p>

#### `ModifyInstanceCreditSpecificationRequest`

``` purescript
newtype ModifyInstanceCreditSpecificationRequest
  = ModifyInstanceCreditSpecificationRequest { "DryRun" :: NullOrUndefined (Boolean), "ClientToken" :: NullOrUndefined (String), "InstanceCreditSpecifications" :: InstanceCreditSpecificationListRequest }
```

#### `ModifyInstanceCreditSpecificationResult`

``` purescript
newtype ModifyInstanceCreditSpecificationResult
  = ModifyInstanceCreditSpecificationResult { "SuccessfulInstanceCreditSpecifications" :: NullOrUndefined (SuccessfulInstanceCreditSpecificationSet), "UnsuccessfulInstanceCreditSpecifications" :: NullOrUndefined (UnsuccessfulInstanceCreditSpecificationSet) }
```

#### `ModifyInstancePlacementRequest`

``` purescript
newtype ModifyInstancePlacementRequest
  = ModifyInstancePlacementRequest { "Affinity" :: NullOrUndefined (Affinity), "HostId" :: NullOrUndefined (String), "InstanceId" :: String, "Tenancy" :: NullOrUndefined (HostTenancy) }
```

<p>Contains the parameters for ModifyInstancePlacement.</p>

#### `ModifyInstancePlacementResult`

``` purescript
newtype ModifyInstancePlacementResult
  = ModifyInstancePlacementResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of ModifyInstancePlacement.</p>

#### `ModifyLaunchTemplateRequest`

``` purescript
newtype ModifyLaunchTemplateRequest
  = ModifyLaunchTemplateRequest { "DryRun" :: NullOrUndefined (Boolean), "ClientToken" :: NullOrUndefined (String), "LaunchTemplateId" :: NullOrUndefined (String), "LaunchTemplateName" :: NullOrUndefined (LaunchTemplateName), "DefaultVersion" :: NullOrUndefined (String) }
```

#### `ModifyLaunchTemplateResult`

``` purescript
newtype ModifyLaunchTemplateResult
  = ModifyLaunchTemplateResult { "LaunchTemplate" :: NullOrUndefined (LaunchTemplate) }
```

#### `ModifyNetworkInterfaceAttributeRequest`

``` purescript
newtype ModifyNetworkInterfaceAttributeRequest
  = ModifyNetworkInterfaceAttributeRequest { "Attachment" :: NullOrUndefined (NetworkInterfaceAttachmentChanges), "Description" :: NullOrUndefined (AttributeValue), "DryRun" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (SecurityGroupIdStringList), "NetworkInterfaceId" :: String, "SourceDestCheck" :: NullOrUndefined (AttributeBooleanValue) }
```

<p>Contains the parameters for ModifyNetworkInterfaceAttribute.</p>

#### `ModifyReservedInstancesRequest`

``` purescript
newtype ModifyReservedInstancesRequest
  = ModifyReservedInstancesRequest { "ReservedInstancesIds" :: ReservedInstancesIdStringList, "ClientToken" :: NullOrUndefined (String), "TargetConfigurations" :: ReservedInstancesConfigurationList }
```

<p>Contains the parameters for ModifyReservedInstances.</p>

#### `ModifyReservedInstancesResult`

``` purescript
newtype ModifyReservedInstancesResult
  = ModifyReservedInstancesResult { "ReservedInstancesModificationId" :: NullOrUndefined (String) }
```

<p>Contains the output of ModifyReservedInstances.</p>

#### `ModifySnapshotAttributeRequest`

``` purescript
newtype ModifySnapshotAttributeRequest
  = ModifySnapshotAttributeRequest { "Attribute" :: NullOrUndefined (SnapshotAttributeName), "CreateVolumePermission" :: NullOrUndefined (CreateVolumePermissionModifications), "GroupNames" :: NullOrUndefined (GroupNameStringList), "OperationType" :: NullOrUndefined (OperationType), "SnapshotId" :: String, "UserIds" :: NullOrUndefined (UserIdStringList), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for ModifySnapshotAttribute.</p>

#### `ModifySpotFleetRequestRequest`

``` purescript
newtype ModifySpotFleetRequestRequest
  = ModifySpotFleetRequestRequest { "ExcessCapacityTerminationPolicy" :: NullOrUndefined (ExcessCapacityTerminationPolicy), "SpotFleetRequestId" :: String, "TargetCapacity" :: NullOrUndefined (Int) }
```

<p>Contains the parameters for ModifySpotFleetRequest.</p>

#### `ModifySpotFleetRequestResponse`

``` purescript
newtype ModifySpotFleetRequestResponse
  = ModifySpotFleetRequestResponse { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of ModifySpotFleetRequest.</p>

#### `ModifySubnetAttributeRequest`

``` purescript
newtype ModifySubnetAttributeRequest
  = ModifySubnetAttributeRequest { "AssignIpv6AddressOnCreation" :: NullOrUndefined (AttributeBooleanValue), "MapPublicIpOnLaunch" :: NullOrUndefined (AttributeBooleanValue), "SubnetId" :: String }
```

<p>Contains the parameters for ModifySubnetAttribute.</p>

#### `ModifyVolumeAttributeRequest`

``` purescript
newtype ModifyVolumeAttributeRequest
  = ModifyVolumeAttributeRequest { "AutoEnableIO" :: NullOrUndefined (AttributeBooleanValue), "VolumeId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for ModifyVolumeAttribute.</p>

#### `ModifyVolumeRequest`

``` purescript
newtype ModifyVolumeRequest
  = ModifyVolumeRequest { "DryRun" :: NullOrUndefined (Boolean), "VolumeId" :: String, "Size" :: NullOrUndefined (Int), "VolumeType" :: NullOrUndefined (VolumeType), "Iops" :: NullOrUndefined (Int) }
```

#### `ModifyVolumeResult`

``` purescript
newtype ModifyVolumeResult
  = ModifyVolumeResult { "VolumeModification" :: NullOrUndefined (VolumeModification) }
```

#### `ModifyVpcAttributeRequest`

``` purescript
newtype ModifyVpcAttributeRequest
  = ModifyVpcAttributeRequest { "EnableDnsHostnames" :: NullOrUndefined (AttributeBooleanValue), "EnableDnsSupport" :: NullOrUndefined (AttributeBooleanValue), "VpcId" :: String }
```

<p>Contains the parameters for ModifyVpcAttribute.</p>

#### `ModifyVpcEndpointConnectionNotificationRequest`

``` purescript
newtype ModifyVpcEndpointConnectionNotificationRequest
  = ModifyVpcEndpointConnectionNotificationRequest { "DryRun" :: NullOrUndefined (Boolean), "ConnectionNotificationId" :: String, "ConnectionNotificationArn" :: NullOrUndefined (String), "ConnectionEvents" :: NullOrUndefined (ValueStringList) }
```

#### `ModifyVpcEndpointConnectionNotificationResult`

``` purescript
newtype ModifyVpcEndpointConnectionNotificationResult
  = ModifyVpcEndpointConnectionNotificationResult { "ReturnValue" :: NullOrUndefined (Boolean) }
```

#### `ModifyVpcEndpointRequest`

``` purescript
newtype ModifyVpcEndpointRequest
  = ModifyVpcEndpointRequest { "DryRun" :: NullOrUndefined (Boolean), "VpcEndpointId" :: String, "ResetPolicy" :: NullOrUndefined (Boolean), "PolicyDocument" :: NullOrUndefined (String), "AddRouteTableIds" :: NullOrUndefined (ValueStringList), "RemoveRouteTableIds" :: NullOrUndefined (ValueStringList), "AddSubnetIds" :: NullOrUndefined (ValueStringList), "RemoveSubnetIds" :: NullOrUndefined (ValueStringList), "AddSecurityGroupIds" :: NullOrUndefined (ValueStringList), "RemoveSecurityGroupIds" :: NullOrUndefined (ValueStringList), "PrivateDnsEnabled" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for ModifyVpcEndpoint.</p>

#### `ModifyVpcEndpointResult`

``` purescript
newtype ModifyVpcEndpointResult
  = ModifyVpcEndpointResult { "Return" :: NullOrUndefined (Boolean) }
```

#### `ModifyVpcEndpointServiceConfigurationRequest`

``` purescript
newtype ModifyVpcEndpointServiceConfigurationRequest
  = ModifyVpcEndpointServiceConfigurationRequest { "DryRun" :: NullOrUndefined (Boolean), "ServiceId" :: String, "AcceptanceRequired" :: NullOrUndefined (Boolean), "AddNetworkLoadBalancerArns" :: NullOrUndefined (ValueStringList), "RemoveNetworkLoadBalancerArns" :: NullOrUndefined (ValueStringList) }
```

#### `ModifyVpcEndpointServiceConfigurationResult`

``` purescript
newtype ModifyVpcEndpointServiceConfigurationResult
  = ModifyVpcEndpointServiceConfigurationResult { "Return" :: NullOrUndefined (Boolean) }
```

#### `ModifyVpcEndpointServicePermissionsRequest`

``` purescript
newtype ModifyVpcEndpointServicePermissionsRequest
  = ModifyVpcEndpointServicePermissionsRequest { "DryRun" :: NullOrUndefined (Boolean), "ServiceId" :: String, "AddAllowedPrincipals" :: NullOrUndefined (ValueStringList), "RemoveAllowedPrincipals" :: NullOrUndefined (ValueStringList) }
```

#### `ModifyVpcEndpointServicePermissionsResult`

``` purescript
newtype ModifyVpcEndpointServicePermissionsResult
  = ModifyVpcEndpointServicePermissionsResult { "ReturnValue" :: NullOrUndefined (Boolean) }
```

#### `ModifyVpcPeeringConnectionOptionsRequest`

``` purescript
newtype ModifyVpcPeeringConnectionOptionsRequest
  = ModifyVpcPeeringConnectionOptionsRequest { "AccepterPeeringConnectionOptions" :: NullOrUndefined (PeeringConnectionOptionsRequest), "DryRun" :: NullOrUndefined (Boolean), "RequesterPeeringConnectionOptions" :: NullOrUndefined (PeeringConnectionOptionsRequest), "VpcPeeringConnectionId" :: String }
```

#### `ModifyVpcPeeringConnectionOptionsResult`

``` purescript
newtype ModifyVpcPeeringConnectionOptionsResult
  = ModifyVpcPeeringConnectionOptionsResult { "AccepterPeeringConnectionOptions" :: NullOrUndefined (PeeringConnectionOptions), "RequesterPeeringConnectionOptions" :: NullOrUndefined (PeeringConnectionOptions) }
```

#### `ModifyVpcTenancyRequest`

``` purescript
newtype ModifyVpcTenancyRequest
  = ModifyVpcTenancyRequest { "VpcId" :: String, "InstanceTenancy" :: VpcTenancy, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for ModifyVpcTenancy.</p>

#### `ModifyVpcTenancyResult`

``` purescript
newtype ModifyVpcTenancyResult
  = ModifyVpcTenancyResult { "ReturnValue" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of ModifyVpcTenancy.</p>

#### `MonitorInstancesRequest`

``` purescript
newtype MonitorInstancesRequest
  = MonitorInstancesRequest { "InstanceIds" :: InstanceIdStringList, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for MonitorInstances.</p>

#### `MonitorInstancesResult`

``` purescript
newtype MonitorInstancesResult
  = MonitorInstancesResult { "InstanceMonitorings" :: NullOrUndefined (InstanceMonitoringList) }
```

<p>Contains the output of MonitorInstances.</p>

#### `Monitoring`

``` purescript
newtype Monitoring
  = Monitoring { "State" :: NullOrUndefined (MonitoringState) }
```

<p>Describes the monitoring of an instance.</p>

#### `MonitoringState`

``` purescript
newtype MonitoringState
  = MonitoringState String
```

#### `MoveAddressToVpcRequest`

``` purescript
newtype MoveAddressToVpcRequest
  = MoveAddressToVpcRequest { "DryRun" :: NullOrUndefined (Boolean), "PublicIp" :: String }
```

<p>Contains the parameters for MoveAddressToVpc.</p>

#### `MoveAddressToVpcResult`

``` purescript
newtype MoveAddressToVpcResult
  = MoveAddressToVpcResult { "AllocationId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (Status) }
```

<p>Contains the output of MoveAddressToVpc.</p>

#### `MoveStatus`

``` purescript
newtype MoveStatus
  = MoveStatus String
```

#### `MovingAddressStatus`

``` purescript
newtype MovingAddressStatus
  = MovingAddressStatus { "MoveStatus" :: NullOrUndefined (MoveStatus), "PublicIp" :: NullOrUndefined (String) }
```

<p>Describes the status of a moving Elastic IP address.</p>

#### `MovingAddressStatusSet`

``` purescript
newtype MovingAddressStatusSet
  = MovingAddressStatusSet (Array MovingAddressStatus)
```

#### `NatGateway`

``` purescript
newtype NatGateway
  = NatGateway { "CreateTime" :: NullOrUndefined (DateTime), "DeleteTime" :: NullOrUndefined (DateTime), "FailureCode" :: NullOrUndefined (String), "FailureMessage" :: NullOrUndefined (String), "NatGatewayAddresses" :: NullOrUndefined (NatGatewayAddressList), "NatGatewayId" :: NullOrUndefined (String), "ProvisionedBandwidth" :: NullOrUndefined (ProvisionedBandwidth), "State" :: NullOrUndefined (NatGatewayState), "SubnetId" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a NAT gateway.</p>

#### `NatGatewayAddress`

``` purescript
newtype NatGatewayAddress
  = NatGatewayAddress { "AllocationId" :: NullOrUndefined (String), "NetworkInterfaceId" :: NullOrUndefined (String), "PrivateIp" :: NullOrUndefined (String), "PublicIp" :: NullOrUndefined (String) }
```

<p>Describes the IP addresses and network interface associated with a NAT gateway.</p>

#### `NatGatewayAddressList`

``` purescript
newtype NatGatewayAddressList
  = NatGatewayAddressList (Array NatGatewayAddress)
```

#### `NatGatewayList`

``` purescript
newtype NatGatewayList
  = NatGatewayList (Array NatGateway)
```

#### `NatGatewayState`

``` purescript
newtype NatGatewayState
  = NatGatewayState String
```

#### `NetworkAcl`

``` purescript
newtype NetworkAcl
  = NetworkAcl { "Associations" :: NullOrUndefined (NetworkAclAssociationList), "Entries" :: NullOrUndefined (NetworkAclEntryList), "IsDefault" :: NullOrUndefined (Boolean), "NetworkAclId" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList), "VpcId" :: NullOrUndefined (String) }
```

<p>Describes a network ACL.</p>

#### `NetworkAclAssociation`

``` purescript
newtype NetworkAclAssociation
  = NetworkAclAssociation { "NetworkAclAssociationId" :: NullOrUndefined (String), "NetworkAclId" :: NullOrUndefined (String), "SubnetId" :: NullOrUndefined (String) }
```

<p>Describes an association between a network ACL and a subnet.</p>

#### `NetworkAclAssociationList`

``` purescript
newtype NetworkAclAssociationList
  = NetworkAclAssociationList (Array NetworkAclAssociation)
```

#### `NetworkAclEntry`

``` purescript
newtype NetworkAclEntry
  = NetworkAclEntry { "CidrBlock" :: NullOrUndefined (String), "Egress" :: NullOrUndefined (Boolean), "IcmpTypeCode" :: NullOrUndefined (IcmpTypeCode), "Ipv6CidrBlock" :: NullOrUndefined (String), "PortRange" :: NullOrUndefined (PortRange), "Protocol" :: NullOrUndefined (String), "RuleAction" :: NullOrUndefined (RuleAction), "RuleNumber" :: NullOrUndefined (Int) }
```

<p>Describes an entry in a network ACL.</p>

#### `NetworkAclEntryList`

``` purescript
newtype NetworkAclEntryList
  = NetworkAclEntryList (Array NetworkAclEntry)
```

#### `NetworkAclList`

``` purescript
newtype NetworkAclList
  = NetworkAclList (Array NetworkAcl)
```

#### `NetworkInterface`

``` purescript
newtype NetworkInterface
  = NetworkInterface { "Association" :: NullOrUndefined (NetworkInterfaceAssociation), "Attachment" :: NullOrUndefined (NetworkInterfaceAttachment), "AvailabilityZone" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Groups" :: NullOrUndefined (GroupIdentifierList), "InterfaceType" :: NullOrUndefined (NetworkInterfaceType), "Ipv6Addresses" :: NullOrUndefined (NetworkInterfaceIpv6AddressesList), "MacAddress" :: NullOrUndefined (String), "NetworkInterfaceId" :: NullOrUndefined (String), "OwnerId" :: NullOrUndefined (String), "PrivateDnsName" :: NullOrUndefined (String), "PrivateIpAddress" :: NullOrUndefined (String), "PrivateIpAddresses" :: NullOrUndefined (NetworkInterfacePrivateIpAddressList), "RequesterId" :: NullOrUndefined (String), "RequesterManaged" :: NullOrUndefined (Boolean), "SourceDestCheck" :: NullOrUndefined (Boolean), "Status" :: NullOrUndefined (NetworkInterfaceStatus), "SubnetId" :: NullOrUndefined (String), "TagSet" :: NullOrUndefined (TagList), "VpcId" :: NullOrUndefined (String) }
```

<p>Describes a network interface.</p>

#### `NetworkInterfaceAssociation`

``` purescript
newtype NetworkInterfaceAssociation
  = NetworkInterfaceAssociation { "AllocationId" :: NullOrUndefined (String), "AssociationId" :: NullOrUndefined (String), "IpOwnerId" :: NullOrUndefined (String), "PublicDnsName" :: NullOrUndefined (String), "PublicIp" :: NullOrUndefined (String) }
```

<p>Describes association information for an Elastic IP address (IPv4 only).</p>

#### `NetworkInterfaceAttachment`

``` purescript
newtype NetworkInterfaceAttachment
  = NetworkInterfaceAttachment { "AttachTime" :: NullOrUndefined (DateTime), "AttachmentId" :: NullOrUndefined (String), "DeleteOnTermination" :: NullOrUndefined (Boolean), "DeviceIndex" :: NullOrUndefined (Int), "InstanceId" :: NullOrUndefined (String), "InstanceOwnerId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (AttachmentStatus) }
```

<p>Describes a network interface attachment.</p>

#### `NetworkInterfaceAttachmentChanges`

``` purescript
newtype NetworkInterfaceAttachmentChanges
  = NetworkInterfaceAttachmentChanges { "AttachmentId" :: NullOrUndefined (String), "DeleteOnTermination" :: NullOrUndefined (Boolean) }
```

<p>Describes an attachment change.</p>

#### `NetworkInterfaceAttribute`

``` purescript
newtype NetworkInterfaceAttribute
  = NetworkInterfaceAttribute String
```

#### `NetworkInterfaceIdList`

``` purescript
newtype NetworkInterfaceIdList
  = NetworkInterfaceIdList (Array String)
```

#### `NetworkInterfaceIpv6Address`

``` purescript
newtype NetworkInterfaceIpv6Address
  = NetworkInterfaceIpv6Address { "Ipv6Address" :: NullOrUndefined (String) }
```

<p>Describes an IPv6 address associated with a network interface.</p>

#### `NetworkInterfaceIpv6AddressesList`

``` purescript
newtype NetworkInterfaceIpv6AddressesList
  = NetworkInterfaceIpv6AddressesList (Array NetworkInterfaceIpv6Address)
```

#### `NetworkInterfaceList`

``` purescript
newtype NetworkInterfaceList
  = NetworkInterfaceList (Array NetworkInterface)
```

#### `NetworkInterfacePermission`

``` purescript
newtype NetworkInterfacePermission
  = NetworkInterfacePermission { "NetworkInterfacePermissionId" :: NullOrUndefined (String), "NetworkInterfaceId" :: NullOrUndefined (String), "AwsAccountId" :: NullOrUndefined (String), "AwsService" :: NullOrUndefined (String), "Permission" :: NullOrUndefined (InterfacePermissionType), "PermissionState" :: NullOrUndefined (NetworkInterfacePermissionState) }
```

<p>Describes a permission for a network interface.</p>

#### `NetworkInterfacePermissionIdList`

``` purescript
newtype NetworkInterfacePermissionIdList
  = NetworkInterfacePermissionIdList (Array String)
```

#### `NetworkInterfacePermissionList`

``` purescript
newtype NetworkInterfacePermissionList
  = NetworkInterfacePermissionList (Array NetworkInterfacePermission)
```

#### `NetworkInterfacePermissionState`

``` purescript
newtype NetworkInterfacePermissionState
  = NetworkInterfacePermissionState { "State" :: NullOrUndefined (NetworkInterfacePermissionStateCode), "StatusMessage" :: NullOrUndefined (String) }
```

<p>Describes the state of a network interface permission.</p>

#### `NetworkInterfacePermissionStateCode`

``` purescript
newtype NetworkInterfacePermissionStateCode
  = NetworkInterfacePermissionStateCode String
```

#### `NetworkInterfacePrivateIpAddress`

``` purescript
newtype NetworkInterfacePrivateIpAddress
  = NetworkInterfacePrivateIpAddress { "Association" :: NullOrUndefined (NetworkInterfaceAssociation), "Primary" :: NullOrUndefined (Boolean), "PrivateDnsName" :: NullOrUndefined (String), "PrivateIpAddress" :: NullOrUndefined (String) }
```

<p>Describes the private IPv4 address of a network interface.</p>

#### `NetworkInterfacePrivateIpAddressList`

``` purescript
newtype NetworkInterfacePrivateIpAddressList
  = NetworkInterfacePrivateIpAddressList (Array NetworkInterfacePrivateIpAddress)
```

#### `NetworkInterfaceStatus`

``` purescript
newtype NetworkInterfaceStatus
  = NetworkInterfaceStatus String
```

#### `NetworkInterfaceType`

``` purescript
newtype NetworkInterfaceType
  = NetworkInterfaceType String
```

#### `NewDhcpConfiguration`

``` purescript
newtype NewDhcpConfiguration
  = NewDhcpConfiguration { "Key" :: NullOrUndefined (String), "Values" :: NullOrUndefined (ValueStringList) }
```

#### `NewDhcpConfigurationList`

``` purescript
newtype NewDhcpConfigurationList
  = NewDhcpConfigurationList (Array NewDhcpConfiguration)
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `OccurrenceDayRequestSet`

``` purescript
newtype OccurrenceDayRequestSet
  = OccurrenceDayRequestSet (Array Int)
```

#### `OccurrenceDaySet`

``` purescript
newtype OccurrenceDaySet
  = OccurrenceDaySet (Array Int)
```

#### `OfferingClassType`

``` purescript
newtype OfferingClassType
  = OfferingClassType String
```

#### `OfferingTypeValues`

``` purescript
newtype OfferingTypeValues
  = OfferingTypeValues String
```

#### `OperationType`

``` purescript
newtype OperationType
  = OperationType String
```

#### `OwnerStringList`

``` purescript
newtype OwnerStringList
  = OwnerStringList (Array String)
```

#### `PaymentOption`

``` purescript
newtype PaymentOption
  = PaymentOption String
```

#### `PciId`

``` purescript
newtype PciId
  = PciId { "DeviceId" :: NullOrUndefined (String), "VendorId" :: NullOrUndefined (String), "SubsystemId" :: NullOrUndefined (String), "SubsystemVendorId" :: NullOrUndefined (String) }
```

<p>Describes the data that identifies an Amazon FPGA image (AFI) on the PCI bus.</p>

#### `PeeringConnectionOptions`

``` purescript
newtype PeeringConnectionOptions
  = PeeringConnectionOptions { "AllowDnsResolutionFromRemoteVpc" :: NullOrUndefined (Boolean), "AllowEgressFromLocalClassicLinkToRemoteVpc" :: NullOrUndefined (Boolean), "AllowEgressFromLocalVpcToRemoteClassicLink" :: NullOrUndefined (Boolean) }
```

<p>Describes the VPC peering connection options.</p>

#### `PeeringConnectionOptionsRequest`

``` purescript
newtype PeeringConnectionOptionsRequest
  = PeeringConnectionOptionsRequest { "AllowDnsResolutionFromRemoteVpc" :: NullOrUndefined (Boolean), "AllowEgressFromLocalClassicLinkToRemoteVpc" :: NullOrUndefined (Boolean), "AllowEgressFromLocalVpcToRemoteClassicLink" :: NullOrUndefined (Boolean) }
```

<p>The VPC peering connection options.</p>

#### `PermissionGroup`

``` purescript
newtype PermissionGroup
  = PermissionGroup String
```

#### `Placement`

``` purescript
newtype Placement
  = Placement { "AvailabilityZone" :: NullOrUndefined (String), "Affinity" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String), "HostId" :: NullOrUndefined (String), "Tenancy" :: NullOrUndefined (Tenancy), "SpreadDomain" :: NullOrUndefined (String) }
```

<p>Describes the placement of an instance.</p>

#### `PlacementGroup`

``` purescript
newtype PlacementGroup
  = PlacementGroup { "GroupName" :: NullOrUndefined (String), "State" :: NullOrUndefined (PlacementGroupState), "Strategy" :: NullOrUndefined (PlacementStrategy) }
```

<p>Describes a placement group.</p>

#### `PlacementGroupList`

``` purescript
newtype PlacementGroupList
  = PlacementGroupList (Array PlacementGroup)
```

#### `PlacementGroupState`

``` purescript
newtype PlacementGroupState
  = PlacementGroupState String
```

#### `PlacementGroupStringList`

``` purescript
newtype PlacementGroupStringList
  = PlacementGroupStringList (Array String)
```

#### `PlacementStrategy`

``` purescript
newtype PlacementStrategy
  = PlacementStrategy String
```

#### `PlatformValues`

``` purescript
newtype PlatformValues
  = PlatformValues String
```

#### `PortRange`

``` purescript
newtype PortRange
  = PortRange { "From" :: NullOrUndefined (Int), "To" :: NullOrUndefined (Int) }
```

<p>Describes a range of ports.</p>

#### `PrefixList`

``` purescript
newtype PrefixList
  = PrefixList { "Cidrs" :: NullOrUndefined (ValueStringList), "PrefixListId" :: NullOrUndefined (String), "PrefixListName" :: NullOrUndefined (String) }
```

<p>Describes prefixes for AWS services.</p>

#### `PrefixListId`

``` purescript
newtype PrefixListId
  = PrefixListId { "Description" :: NullOrUndefined (String), "PrefixListId" :: NullOrUndefined (String) }
```

<p>[EC2-VPC only] The ID of the prefix.</p>

#### `PrefixListIdList`

``` purescript
newtype PrefixListIdList
  = PrefixListIdList (Array PrefixListId)
```

#### `PrefixListIdSet`

``` purescript
newtype PrefixListIdSet
  = PrefixListIdSet (Array String)
```

#### `PrefixListSet`

``` purescript
newtype PrefixListSet
  = PrefixListSet (Array PrefixList)
```

#### `PriceSchedule`

``` purescript
newtype PriceSchedule
  = PriceSchedule { "Active" :: NullOrUndefined (Boolean), "CurrencyCode" :: NullOrUndefined (CurrencyCodeValues), "Price" :: NullOrUndefined (Number), "Term" :: NullOrUndefined (Number) }
```

<p>Describes the price for a Reserved Instance.</p>

#### `PriceScheduleList`

``` purescript
newtype PriceScheduleList
  = PriceScheduleList (Array PriceSchedule)
```

#### `PriceScheduleSpecification`

``` purescript
newtype PriceScheduleSpecification
  = PriceScheduleSpecification { "CurrencyCode" :: NullOrUndefined (CurrencyCodeValues), "Price" :: NullOrUndefined (Number), "Term" :: NullOrUndefined (Number) }
```

<p>Describes the price for a Reserved Instance.</p>

#### `PriceScheduleSpecificationList`

``` purescript
newtype PriceScheduleSpecificationList
  = PriceScheduleSpecificationList (Array PriceScheduleSpecification)
```

#### `PricingDetail`

``` purescript
newtype PricingDetail
  = PricingDetail { "Count" :: NullOrUndefined (Int), "Price" :: NullOrUndefined (Number) }
```

<p>Describes a Reserved Instance offering.</p>

#### `PricingDetailsList`

``` purescript
newtype PricingDetailsList
  = PricingDetailsList (Array PricingDetail)
```

#### `PrincipalIdFormat`

``` purescript
newtype PrincipalIdFormat
  = PrincipalIdFormat { "Arn" :: NullOrUndefined (String), "Statuses" :: NullOrUndefined (IdFormatList) }
```

<p>PrincipalIdFormat description</p>

#### `PrincipalIdFormatList`

``` purescript
newtype PrincipalIdFormatList
  = PrincipalIdFormatList (Array PrincipalIdFormat)
```

#### `PrincipalType`

``` purescript
newtype PrincipalType
  = PrincipalType String
```

#### `PrivateIpAddressConfigSet`

``` purescript
newtype PrivateIpAddressConfigSet
  = PrivateIpAddressConfigSet (Array ScheduledInstancesPrivateIpAddressConfig)
```

#### `PrivateIpAddressSpecification`

``` purescript
newtype PrivateIpAddressSpecification
  = PrivateIpAddressSpecification { "Primary" :: NullOrUndefined (Boolean), "PrivateIpAddress" :: String }
```

<p>Describes a secondary private IPv4 address for a network interface.</p>

#### `PrivateIpAddressSpecificationList`

``` purescript
newtype PrivateIpAddressSpecificationList
  = PrivateIpAddressSpecificationList (Array PrivateIpAddressSpecification)
```

#### `PrivateIpAddressStringList`

``` purescript
newtype PrivateIpAddressStringList
  = PrivateIpAddressStringList (Array String)
```

#### `ProductCode`

``` purescript
newtype ProductCode
  = ProductCode { "ProductCodeId" :: NullOrUndefined (String), "ProductCodeType" :: NullOrUndefined (ProductCodeValues) }
```

<p>Describes a product code.</p>

#### `ProductCodeList`

``` purescript
newtype ProductCodeList
  = ProductCodeList (Array ProductCode)
```

#### `ProductCodeStringList`

``` purescript
newtype ProductCodeStringList
  = ProductCodeStringList (Array String)
```

#### `ProductCodeValues`

``` purescript
newtype ProductCodeValues
  = ProductCodeValues String
```

#### `ProductDescriptionList`

``` purescript
newtype ProductDescriptionList
  = ProductDescriptionList (Array String)
```

#### `PropagatingVgw`

``` purescript
newtype PropagatingVgw
  = PropagatingVgw { "GatewayId" :: NullOrUndefined (String) }
```

<p>Describes a virtual private gateway propagating route.</p>

#### `PropagatingVgwList`

``` purescript
newtype PropagatingVgwList
  = PropagatingVgwList (Array PropagatingVgw)
```

#### `ProvisionedBandwidth`

``` purescript
newtype ProvisionedBandwidth
  = ProvisionedBandwidth { "ProvisionTime" :: NullOrUndefined (DateTime), "Provisioned" :: NullOrUndefined (String), "RequestTime" :: NullOrUndefined (DateTime), "Requested" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String) }
```

<p>Reserved. If you need to sustain traffic greater than the <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html">documented limits</a>, contact us through the <a href="https://console.aws.amazon.com/support/home?">Support Center</a>.</p>

#### `PublicIpStringList`

``` purescript
newtype PublicIpStringList
  = PublicIpStringList (Array String)
```

#### `Purchase`

``` purescript
newtype Purchase
  = Purchase { "CurrencyCode" :: NullOrUndefined (CurrencyCodeValues), "Duration" :: NullOrUndefined (Int), "HostIdSet" :: NullOrUndefined (ResponseHostIdSet), "HostReservationId" :: NullOrUndefined (String), "HourlyPrice" :: NullOrUndefined (String), "InstanceFamily" :: NullOrUndefined (String), "PaymentOption" :: NullOrUndefined (PaymentOption), "UpfrontPrice" :: NullOrUndefined (String) }
```

<p>Describes the result of the purchase.</p>

#### `PurchaseHostReservationRequest`

``` purescript
newtype PurchaseHostReservationRequest
  = PurchaseHostReservationRequest { "ClientToken" :: NullOrUndefined (String), "CurrencyCode" :: NullOrUndefined (CurrencyCodeValues), "HostIdSet" :: RequestHostIdSet, "LimitPrice" :: NullOrUndefined (String), "OfferingId" :: String }
```

#### `PurchaseHostReservationResult`

``` purescript
newtype PurchaseHostReservationResult
  = PurchaseHostReservationResult { "ClientToken" :: NullOrUndefined (String), "CurrencyCode" :: NullOrUndefined (CurrencyCodeValues), "Purchase" :: NullOrUndefined (PurchaseSet), "TotalHourlyPrice" :: NullOrUndefined (String), "TotalUpfrontPrice" :: NullOrUndefined (String) }
```

#### `PurchaseRequest`

``` purescript
newtype PurchaseRequest
  = PurchaseRequest { "InstanceCount" :: Int, "PurchaseToken" :: String }
```

<p>Describes a request to purchase Scheduled Instances.</p>

#### `PurchaseRequestSet`

``` purescript
newtype PurchaseRequestSet
  = PurchaseRequestSet (Array PurchaseRequest)
```

#### `PurchaseReservedInstancesOfferingRequest`

``` purescript
newtype PurchaseReservedInstancesOfferingRequest
  = PurchaseReservedInstancesOfferingRequest { "InstanceCount" :: Int, "ReservedInstancesOfferingId" :: String, "DryRun" :: NullOrUndefined (Boolean), "LimitPrice" :: NullOrUndefined (ReservedInstanceLimitPrice) }
```

<p>Contains the parameters for PurchaseReservedInstancesOffering.</p>

#### `PurchaseReservedInstancesOfferingResult`

``` purescript
newtype PurchaseReservedInstancesOfferingResult
  = PurchaseReservedInstancesOfferingResult { "ReservedInstancesId" :: NullOrUndefined (String) }
```

<p>Contains the output of PurchaseReservedInstancesOffering.</p>

#### `PurchaseScheduledInstancesRequest`

``` purescript
newtype PurchaseScheduledInstancesRequest
  = PurchaseScheduledInstancesRequest { "ClientToken" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "PurchaseRequests" :: PurchaseRequestSet }
```

<p>Contains the parameters for PurchaseScheduledInstances.</p>

#### `PurchaseScheduledInstancesResult`

``` purescript
newtype PurchaseScheduledInstancesResult
  = PurchaseScheduledInstancesResult { "ScheduledInstanceSet" :: NullOrUndefined (PurchasedScheduledInstanceSet) }
```

<p>Contains the output of PurchaseScheduledInstances.</p>

#### `PurchaseSet`

``` purescript
newtype PurchaseSet
  = PurchaseSet (Array Purchase)
```

#### `PurchasedScheduledInstanceSet`

``` purescript
newtype PurchasedScheduledInstanceSet
  = PurchasedScheduledInstanceSet (Array ScheduledInstance)
```

#### `RIProductDescription`

``` purescript
newtype RIProductDescription
  = RIProductDescription String
```

#### `ReasonCodesList`

``` purescript
newtype ReasonCodesList
  = ReasonCodesList (Array ReportInstanceReasonCodes)
```

#### `RebootInstancesRequest`

``` purescript
newtype RebootInstancesRequest
  = RebootInstancesRequest { "InstanceIds" :: InstanceIdStringList, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for RebootInstances.</p>

#### `RecurringCharge`

``` purescript
newtype RecurringCharge
  = RecurringCharge { "Amount" :: NullOrUndefined (Number), "Frequency" :: NullOrUndefined (RecurringChargeFrequency) }
```

<p>Describes a recurring charge.</p>

#### `RecurringChargeFrequency`

``` purescript
newtype RecurringChargeFrequency
  = RecurringChargeFrequency String
```

#### `RecurringChargesList`

``` purescript
newtype RecurringChargesList
  = RecurringChargesList (Array RecurringCharge)
```

#### `Region`

``` purescript
newtype Region
  = Region { "Endpoint" :: NullOrUndefined (String), "RegionName" :: NullOrUndefined (String) }
```

<p>Describes a region.</p>

#### `RegionList`

``` purescript
newtype RegionList
  = RegionList (Array Region)
```

#### `RegionNameStringList`

``` purescript
newtype RegionNameStringList
  = RegionNameStringList (Array String)
```

#### `RegisterImageRequest`

``` purescript
newtype RegisterImageRequest
  = RegisterImageRequest { "ImageLocation" :: NullOrUndefined (String), "Architecture" :: NullOrUndefined (ArchitectureValues), "BlockDeviceMappings" :: NullOrUndefined (BlockDeviceMappingRequestList), "Description" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "EnaSupport" :: NullOrUndefined (Boolean), "KernelId" :: NullOrUndefined (String), "Name" :: String, "BillingProducts" :: NullOrUndefined (BillingProductList), "RamdiskId" :: NullOrUndefined (String), "RootDeviceName" :: NullOrUndefined (String), "SriovNetSupport" :: NullOrUndefined (String), "VirtualizationType" :: NullOrUndefined (String) }
```

<p>Contains the parameters for RegisterImage.</p>

#### `RegisterImageResult`

``` purescript
newtype RegisterImageResult
  = RegisterImageResult { "ImageId" :: NullOrUndefined (String) }
```

<p>Contains the output of RegisterImage.</p>

#### `RejectVpcEndpointConnectionsRequest`

``` purescript
newtype RejectVpcEndpointConnectionsRequest
  = RejectVpcEndpointConnectionsRequest { "DryRun" :: NullOrUndefined (Boolean), "ServiceId" :: String, "VpcEndpointIds" :: ValueStringList }
```

#### `RejectVpcEndpointConnectionsResult`

``` purescript
newtype RejectVpcEndpointConnectionsResult
  = RejectVpcEndpointConnectionsResult { "Unsuccessful" :: NullOrUndefined (UnsuccessfulItemSet) }
```

#### `RejectVpcPeeringConnectionRequest`

``` purescript
newtype RejectVpcPeeringConnectionRequest
  = RejectVpcPeeringConnectionRequest { "DryRun" :: NullOrUndefined (Boolean), "VpcPeeringConnectionId" :: String }
```

<p>Contains the parameters for RejectVpcPeeringConnection.</p>

#### `RejectVpcPeeringConnectionResult`

``` purescript
newtype RejectVpcPeeringConnectionResult
  = RejectVpcPeeringConnectionResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of RejectVpcPeeringConnection.</p>

#### `ReleaseAddressRequest`

``` purescript
newtype ReleaseAddressRequest
  = ReleaseAddressRequest { "AllocationId" :: NullOrUndefined (String), "PublicIp" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for ReleaseAddress.</p>

#### `ReleaseHostsRequest`

``` purescript
newtype ReleaseHostsRequest
  = ReleaseHostsRequest { "HostIds" :: RequestHostIdList }
```

<p>Contains the parameters for ReleaseHosts.</p>

#### `ReleaseHostsResult`

``` purescript
newtype ReleaseHostsResult
  = ReleaseHostsResult { "Successful" :: NullOrUndefined (ResponseHostIdList), "Unsuccessful" :: NullOrUndefined (UnsuccessfulItemList) }
```

<p>Contains the output of ReleaseHosts.</p>

#### `ReplaceIamInstanceProfileAssociationRequest`

``` purescript
newtype ReplaceIamInstanceProfileAssociationRequest
  = ReplaceIamInstanceProfileAssociationRequest { "IamInstanceProfile" :: IamInstanceProfileSpecification, "AssociationId" :: String }
```

#### `ReplaceIamInstanceProfileAssociationResult`

``` purescript
newtype ReplaceIamInstanceProfileAssociationResult
  = ReplaceIamInstanceProfileAssociationResult { "IamInstanceProfileAssociation" :: NullOrUndefined (IamInstanceProfileAssociation) }
```

#### `ReplaceNetworkAclAssociationRequest`

``` purescript
newtype ReplaceNetworkAclAssociationRequest
  = ReplaceNetworkAclAssociationRequest { "AssociationId" :: String, "DryRun" :: NullOrUndefined (Boolean), "NetworkAclId" :: String }
```

<p>Contains the parameters for ReplaceNetworkAclAssociation.</p>

#### `ReplaceNetworkAclAssociationResult`

``` purescript
newtype ReplaceNetworkAclAssociationResult
  = ReplaceNetworkAclAssociationResult { "NewAssociationId" :: NullOrUndefined (String) }
```

<p>Contains the output of ReplaceNetworkAclAssociation.</p>

#### `ReplaceNetworkAclEntryRequest`

``` purescript
newtype ReplaceNetworkAclEntryRequest
  = ReplaceNetworkAclEntryRequest { "CidrBlock" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "Egress" :: Boolean, "IcmpTypeCode" :: NullOrUndefined (IcmpTypeCode), "Ipv6CidrBlock" :: NullOrUndefined (String), "NetworkAclId" :: String, "PortRange" :: NullOrUndefined (PortRange), "Protocol" :: String, "RuleAction" :: RuleAction, "RuleNumber" :: Int }
```

<p>Contains the parameters for ReplaceNetworkAclEntry.</p>

#### `ReplaceRouteRequest`

``` purescript
newtype ReplaceRouteRequest
  = ReplaceRouteRequest { "DestinationCidrBlock" :: NullOrUndefined (String), "DestinationIpv6CidrBlock" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "EgressOnlyInternetGatewayId" :: NullOrUndefined (String), "GatewayId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "NatGatewayId" :: NullOrUndefined (String), "NetworkInterfaceId" :: NullOrUndefined (String), "RouteTableId" :: String, "VpcPeeringConnectionId" :: NullOrUndefined (String) }
```

<p>Contains the parameters for ReplaceRoute.</p>

#### `ReplaceRouteTableAssociationRequest`

``` purescript
newtype ReplaceRouteTableAssociationRequest
  = ReplaceRouteTableAssociationRequest { "AssociationId" :: String, "DryRun" :: NullOrUndefined (Boolean), "RouteTableId" :: String }
```

<p>Contains the parameters for ReplaceRouteTableAssociation.</p>

#### `ReplaceRouteTableAssociationResult`

``` purescript
newtype ReplaceRouteTableAssociationResult
  = ReplaceRouteTableAssociationResult { "NewAssociationId" :: NullOrUndefined (String) }
```

<p>Contains the output of ReplaceRouteTableAssociation.</p>

#### `ReportInstanceReasonCodes`

``` purescript
newtype ReportInstanceReasonCodes
  = ReportInstanceReasonCodes String
```

#### `ReportInstanceStatusRequest`

``` purescript
newtype ReportInstanceStatusRequest
  = ReportInstanceStatusRequest { "Description" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "EndTime" :: NullOrUndefined (DateTime), "Instances" :: InstanceIdStringList, "ReasonCodes" :: ReasonCodesList, "StartTime" :: NullOrUndefined (DateTime), "Status" :: ReportStatusType }
```

<p>Contains the parameters for ReportInstanceStatus.</p>

#### `ReportStatusType`

``` purescript
newtype ReportStatusType
  = ReportStatusType String
```

#### `RequestHostIdList`

``` purescript
newtype RequestHostIdList
  = RequestHostIdList (Array String)
```

#### `RequestHostIdSet`

``` purescript
newtype RequestHostIdSet
  = RequestHostIdSet (Array String)
```

#### `RequestLaunchTemplateData`

``` purescript
newtype RequestLaunchTemplateData
  = RequestLaunchTemplateData { "KernelId" :: NullOrUndefined (String), "EbsOptimized" :: NullOrUndefined (Boolean), "IamInstanceProfile" :: NullOrUndefined (LaunchTemplateIamInstanceProfileSpecificationRequest), "BlockDeviceMappings" :: NullOrUndefined (LaunchTemplateBlockDeviceMappingRequestList), "NetworkInterfaces" :: NullOrUndefined (LaunchTemplateInstanceNetworkInterfaceSpecificationRequestList), "ImageId" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (InstanceType), "KeyName" :: NullOrUndefined (String), "Monitoring" :: NullOrUndefined (LaunchTemplatesMonitoringRequest), "Placement" :: NullOrUndefined (LaunchTemplatePlacementRequest), "RamDiskId" :: NullOrUndefined (String), "DisableApiTermination" :: NullOrUndefined (Boolean), "InstanceInitiatedShutdownBehavior" :: NullOrUndefined (ShutdownBehavior), "UserData" :: NullOrUndefined (String), "TagSpecifications" :: NullOrUndefined (LaunchTemplateTagSpecificationRequestList), "ElasticGpuSpecifications" :: NullOrUndefined (ElasticGpuSpecificationList), "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdStringList), "SecurityGroups" :: NullOrUndefined (SecurityGroupStringList), "InstanceMarketOptions" :: NullOrUndefined (LaunchTemplateInstanceMarketOptionsRequest), "CreditSpecification" :: NullOrUndefined (CreditSpecificationRequest) }
```

<p>The information to include in the launch template.</p>

#### `RequestSpotFleetRequest`

``` purescript
newtype RequestSpotFleetRequest
  = RequestSpotFleetRequest { "DryRun" :: NullOrUndefined (Boolean), "SpotFleetRequestConfig" :: SpotFleetRequestConfigData }
```

<p>Contains the parameters for RequestSpotFleet.</p>

#### `RequestSpotFleetResponse`

``` purescript
newtype RequestSpotFleetResponse
  = RequestSpotFleetResponse { "SpotFleetRequestId" :: String }
```

<p>Contains the output of RequestSpotFleet.</p>

#### `RequestSpotInstancesRequest`

``` purescript
newtype RequestSpotInstancesRequest
  = RequestSpotInstancesRequest { "AvailabilityZoneGroup" :: NullOrUndefined (String), "BlockDurationMinutes" :: NullOrUndefined (Int), "ClientToken" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "InstanceCount" :: NullOrUndefined (Int), "LaunchGroup" :: NullOrUndefined (String), "LaunchSpecification" :: NullOrUndefined (RequestSpotLaunchSpecification), "SpotPrice" :: NullOrUndefined (String), "Type" :: NullOrUndefined (SpotInstanceType), "ValidFrom" :: NullOrUndefined (DateTime), "ValidUntil" :: NullOrUndefined (DateTime), "InstanceInterruptionBehavior" :: NullOrUndefined (InstanceInterruptionBehavior) }
```

<p>Contains the parameters for RequestSpotInstances.</p>

#### `RequestSpotInstancesResult`

``` purescript
newtype RequestSpotInstancesResult
  = RequestSpotInstancesResult { "SpotInstanceRequests" :: NullOrUndefined (SpotInstanceRequestList) }
```

<p>Contains the output of RequestSpotInstances.</p>

#### `RequestSpotLaunchSpecification`

``` purescript
newtype RequestSpotLaunchSpecification
  = RequestSpotLaunchSpecification { "SecurityGroupIds" :: NullOrUndefined (ValueStringList), "SecurityGroups" :: NullOrUndefined (ValueStringList), "AddressingType" :: NullOrUndefined (String), "BlockDeviceMappings" :: NullOrUndefined (BlockDeviceMappingList), "EbsOptimized" :: NullOrUndefined (Boolean), "IamInstanceProfile" :: NullOrUndefined (IamInstanceProfileSpecification), "ImageId" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (InstanceType), "KernelId" :: NullOrUndefined (String), "KeyName" :: NullOrUndefined (String), "Monitoring" :: NullOrUndefined (RunInstancesMonitoringEnabled), "NetworkInterfaces" :: NullOrUndefined (InstanceNetworkInterfaceSpecificationList), "Placement" :: NullOrUndefined (SpotPlacement), "RamdiskId" :: NullOrUndefined (String), "SubnetId" :: NullOrUndefined (String), "UserData" :: NullOrUndefined (String) }
```

<p>Describes the launch specification for an instance.</p>

#### `Reservation`

``` purescript
newtype Reservation
  = Reservation { "Groups" :: NullOrUndefined (GroupIdentifierList), "Instances" :: NullOrUndefined (InstanceList), "OwnerId" :: NullOrUndefined (String), "RequesterId" :: NullOrUndefined (String), "ReservationId" :: NullOrUndefined (String) }
```

<p>Describes a reservation.</p>

#### `ReservationList`

``` purescript
newtype ReservationList
  = ReservationList (Array Reservation)
```

#### `ReservationState`

``` purescript
newtype ReservationState
  = ReservationState String
```

#### `ReservationValue`

``` purescript
newtype ReservationValue
  = ReservationValue { "HourlyPrice" :: NullOrUndefined (String), "RemainingTotalValue" :: NullOrUndefined (String), "RemainingUpfrontValue" :: NullOrUndefined (String) }
```

<p>The cost associated with the Reserved Instance.</p>

#### `ReservedInstanceIdSet`

``` purescript
newtype ReservedInstanceIdSet
  = ReservedInstanceIdSet (Array String)
```

#### `ReservedInstanceLimitPrice`

``` purescript
newtype ReservedInstanceLimitPrice
  = ReservedInstanceLimitPrice { "Amount" :: NullOrUndefined (Number), "CurrencyCode" :: NullOrUndefined (CurrencyCodeValues) }
```

<p>Describes the limit price of a Reserved Instance offering.</p>

#### `ReservedInstanceReservationValue`

``` purescript
newtype ReservedInstanceReservationValue
  = ReservedInstanceReservationValue { "ReservationValue" :: NullOrUndefined (ReservationValue), "ReservedInstanceId" :: NullOrUndefined (String) }
```

<p>The total value of the Convertible Reserved Instance.</p>

#### `ReservedInstanceReservationValueSet`

``` purescript
newtype ReservedInstanceReservationValueSet
  = ReservedInstanceReservationValueSet (Array ReservedInstanceReservationValue)
```

#### `ReservedInstanceState`

``` purescript
newtype ReservedInstanceState
  = ReservedInstanceState String
```

#### `ReservedInstances`

``` purescript
newtype ReservedInstances
  = ReservedInstances { "AvailabilityZone" :: NullOrUndefined (String), "Duration" :: NullOrUndefined (Number), "End" :: NullOrUndefined (DateTime), "FixedPrice" :: NullOrUndefined (Number), "InstanceCount" :: NullOrUndefined (Int), "InstanceType" :: NullOrUndefined (InstanceType), "ProductDescription" :: NullOrUndefined (RIProductDescription), "ReservedInstancesId" :: NullOrUndefined (String), "Start" :: NullOrUndefined (DateTime), "State" :: NullOrUndefined (ReservedInstanceState), "UsagePrice" :: NullOrUndefined (Number), "CurrencyCode" :: NullOrUndefined (CurrencyCodeValues), "InstanceTenancy" :: NullOrUndefined (Tenancy), "OfferingClass" :: NullOrUndefined (OfferingClassType), "OfferingType" :: NullOrUndefined (OfferingTypeValues), "RecurringCharges" :: NullOrUndefined (RecurringChargesList), "Scope" :: NullOrUndefined (Scope'), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a Reserved Instance.</p>

#### `ReservedInstancesConfiguration`

``` purescript
newtype ReservedInstancesConfiguration
  = ReservedInstancesConfiguration { "AvailabilityZone" :: NullOrUndefined (String), "InstanceCount" :: NullOrUndefined (Int), "InstanceType" :: NullOrUndefined (InstanceType), "Platform" :: NullOrUndefined (String), "Scope" :: NullOrUndefined (Scope') }
```

<p>Describes the configuration settings for the modified Reserved Instances.</p>

#### `ReservedInstancesConfigurationList`

``` purescript
newtype ReservedInstancesConfigurationList
  = ReservedInstancesConfigurationList (Array ReservedInstancesConfiguration)
```

#### `ReservedInstancesId`

``` purescript
newtype ReservedInstancesId
  = ReservedInstancesId { "ReservedInstancesId" :: NullOrUndefined (String) }
```

<p>Describes the ID of a Reserved Instance.</p>

#### `ReservedInstancesIdStringList`

``` purescript
newtype ReservedInstancesIdStringList
  = ReservedInstancesIdStringList (Array String)
```

#### `ReservedInstancesList`

``` purescript
newtype ReservedInstancesList
  = ReservedInstancesList (Array ReservedInstances)
```

#### `ReservedInstancesListing`

``` purescript
newtype ReservedInstancesListing
  = ReservedInstancesListing { "ClientToken" :: NullOrUndefined (String), "CreateDate" :: NullOrUndefined (DateTime), "InstanceCounts" :: NullOrUndefined (InstanceCountList), "PriceSchedules" :: NullOrUndefined (PriceScheduleList), "ReservedInstancesId" :: NullOrUndefined (String), "ReservedInstancesListingId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (ListingStatus), "StatusMessage" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList), "UpdateDate" :: NullOrUndefined (DateTime) }
```

<p>Describes a Reserved Instance listing.</p>

#### `ReservedInstancesListingList`

``` purescript
newtype ReservedInstancesListingList
  = ReservedInstancesListingList (Array ReservedInstancesListing)
```

#### `ReservedInstancesModification`

``` purescript
newtype ReservedInstancesModification
  = ReservedInstancesModification { "ClientToken" :: NullOrUndefined (String), "CreateDate" :: NullOrUndefined (DateTime), "EffectiveDate" :: NullOrUndefined (DateTime), "ModificationResults" :: NullOrUndefined (ReservedInstancesModificationResultList), "ReservedInstancesIds" :: NullOrUndefined (ReservedIntancesIds), "ReservedInstancesModificationId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "StatusMessage" :: NullOrUndefined (String), "UpdateDate" :: NullOrUndefined (DateTime) }
```

<p>Describes a Reserved Instance modification.</p>

#### `ReservedInstancesModificationIdStringList`

``` purescript
newtype ReservedInstancesModificationIdStringList
  = ReservedInstancesModificationIdStringList (Array String)
```

#### `ReservedInstancesModificationList`

``` purescript
newtype ReservedInstancesModificationList
  = ReservedInstancesModificationList (Array ReservedInstancesModification)
```

#### `ReservedInstancesModificationResult`

``` purescript
newtype ReservedInstancesModificationResult
  = ReservedInstancesModificationResult { "ReservedInstancesId" :: NullOrUndefined (String), "TargetConfiguration" :: NullOrUndefined (ReservedInstancesConfiguration) }
```

<p>Describes the modification request/s.</p>

#### `ReservedInstancesModificationResultList`

``` purescript
newtype ReservedInstancesModificationResultList
  = ReservedInstancesModificationResultList (Array ReservedInstancesModificationResult)
```

#### `ReservedInstancesOffering`

``` purescript
newtype ReservedInstancesOffering
  = ReservedInstancesOffering { "AvailabilityZone" :: NullOrUndefined (String), "Duration" :: NullOrUndefined (Number), "FixedPrice" :: NullOrUndefined (Number), "InstanceType" :: NullOrUndefined (InstanceType), "ProductDescription" :: NullOrUndefined (RIProductDescription), "ReservedInstancesOfferingId" :: NullOrUndefined (String), "UsagePrice" :: NullOrUndefined (Number), "CurrencyCode" :: NullOrUndefined (CurrencyCodeValues), "InstanceTenancy" :: NullOrUndefined (Tenancy), "Marketplace" :: NullOrUndefined (Boolean), "OfferingClass" :: NullOrUndefined (OfferingClassType), "OfferingType" :: NullOrUndefined (OfferingTypeValues), "PricingDetails" :: NullOrUndefined (PricingDetailsList), "RecurringCharges" :: NullOrUndefined (RecurringChargesList), "Scope" :: NullOrUndefined (Scope') }
```

<p>Describes a Reserved Instance offering.</p>

#### `ReservedInstancesOfferingIdStringList`

``` purescript
newtype ReservedInstancesOfferingIdStringList
  = ReservedInstancesOfferingIdStringList (Array String)
```

#### `ReservedInstancesOfferingList`

``` purescript
newtype ReservedInstancesOfferingList
  = ReservedInstancesOfferingList (Array ReservedInstancesOffering)
```

#### `ReservedIntancesIds`

``` purescript
newtype ReservedIntancesIds
  = ReservedIntancesIds (Array ReservedInstancesId)
```

#### `ResetFpgaImageAttributeName`

``` purescript
newtype ResetFpgaImageAttributeName
  = ResetFpgaImageAttributeName String
```

#### `ResetFpgaImageAttributeRequest`

``` purescript
newtype ResetFpgaImageAttributeRequest
  = ResetFpgaImageAttributeRequest { "DryRun" :: NullOrUndefined (Boolean), "FpgaImageId" :: String, "Attribute" :: NullOrUndefined (ResetFpgaImageAttributeName) }
```

#### `ResetFpgaImageAttributeResult`

``` purescript
newtype ResetFpgaImageAttributeResult
  = ResetFpgaImageAttributeResult { "Return" :: NullOrUndefined (Boolean) }
```

#### `ResetImageAttributeName`

``` purescript
newtype ResetImageAttributeName
  = ResetImageAttributeName String
```

#### `ResetImageAttributeRequest`

``` purescript
newtype ResetImageAttributeRequest
  = ResetImageAttributeRequest { "Attribute" :: ResetImageAttributeName, "ImageId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for ResetImageAttribute.</p>

#### `ResetInstanceAttributeRequest`

``` purescript
newtype ResetInstanceAttributeRequest
  = ResetInstanceAttributeRequest { "Attribute" :: InstanceAttributeName, "DryRun" :: NullOrUndefined (Boolean), "InstanceId" :: String }
```

<p>Contains the parameters for ResetInstanceAttribute.</p>

#### `ResetNetworkInterfaceAttributeRequest`

``` purescript
newtype ResetNetworkInterfaceAttributeRequest
  = ResetNetworkInterfaceAttributeRequest { "DryRun" :: NullOrUndefined (Boolean), "NetworkInterfaceId" :: String, "SourceDestCheck" :: NullOrUndefined (String) }
```

<p>Contains the parameters for ResetNetworkInterfaceAttribute.</p>

#### `ResetSnapshotAttributeRequest`

``` purescript
newtype ResetSnapshotAttributeRequest
  = ResetSnapshotAttributeRequest { "Attribute" :: SnapshotAttributeName, "SnapshotId" :: String, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for ResetSnapshotAttribute.</p>

#### `ResourceIdList`

``` purescript
newtype ResourceIdList
  = ResourceIdList (Array String)
```

#### `ResourceList`

``` purescript
newtype ResourceList
  = ResourceList (Array String)
```

#### `ResourceType`

``` purescript
newtype ResourceType
  = ResourceType String
```

#### `ResponseError`

``` purescript
newtype ResponseError
  = ResponseError { "Code" :: NullOrUndefined (LaunchTemplateErrorCode), "Message" :: NullOrUndefined (String) }
```

<p>Describes the error that's returned when you cannot delete a launch template version.</p>

#### `ResponseHostIdList`

``` purescript
newtype ResponseHostIdList
  = ResponseHostIdList (Array String)
```

#### `ResponseHostIdSet`

``` purescript
newtype ResponseHostIdSet
  = ResponseHostIdSet (Array String)
```

#### `ResponseLaunchTemplateData`

``` purescript
newtype ResponseLaunchTemplateData
  = ResponseLaunchTemplateData { "KernelId" :: NullOrUndefined (String), "EbsOptimized" :: NullOrUndefined (Boolean), "IamInstanceProfile" :: NullOrUndefined (LaunchTemplateIamInstanceProfileSpecification), "BlockDeviceMappings" :: NullOrUndefined (LaunchTemplateBlockDeviceMappingList), "NetworkInterfaces" :: NullOrUndefined (LaunchTemplateInstanceNetworkInterfaceSpecificationList), "ImageId" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (InstanceType), "KeyName" :: NullOrUndefined (String), "Monitoring" :: NullOrUndefined (LaunchTemplatesMonitoring), "Placement" :: NullOrUndefined (LaunchTemplatePlacement), "RamDiskId" :: NullOrUndefined (String), "DisableApiTermination" :: NullOrUndefined (Boolean), "InstanceInitiatedShutdownBehavior" :: NullOrUndefined (ShutdownBehavior), "UserData" :: NullOrUndefined (String), "TagSpecifications" :: NullOrUndefined (LaunchTemplateTagSpecificationList), "ElasticGpuSpecifications" :: NullOrUndefined (ElasticGpuSpecificationResponseList), "SecurityGroupIds" :: NullOrUndefined (ValueStringList), "SecurityGroups" :: NullOrUndefined (ValueStringList), "InstanceMarketOptions" :: NullOrUndefined (LaunchTemplateInstanceMarketOptions), "CreditSpecification" :: NullOrUndefined (CreditSpecification) }
```

<p>The information for a launch template. </p>

#### `RestorableByStringList`

``` purescript
newtype RestorableByStringList
  = RestorableByStringList (Array String)
```

#### `RestoreAddressToClassicRequest`

``` purescript
newtype RestoreAddressToClassicRequest
  = RestoreAddressToClassicRequest { "DryRun" :: NullOrUndefined (Boolean), "PublicIp" :: String }
```

<p>Contains the parameters for RestoreAddressToClassic.</p>

#### `RestoreAddressToClassicResult`

``` purescript
newtype RestoreAddressToClassicResult
  = RestoreAddressToClassicResult { "PublicIp" :: NullOrUndefined (String), "Status" :: NullOrUndefined (Status) }
```

<p>Contains the output of RestoreAddressToClassic.</p>

#### `RevokeSecurityGroupEgressRequest`

``` purescript
newtype RevokeSecurityGroupEgressRequest
  = RevokeSecurityGroupEgressRequest { "DryRun" :: NullOrUndefined (Boolean), "GroupId" :: String, "IpPermissions" :: NullOrUndefined (IpPermissionList), "CidrIp" :: NullOrUndefined (String), "FromPort" :: NullOrUndefined (Int), "IpProtocol" :: NullOrUndefined (String), "ToPort" :: NullOrUndefined (Int), "SourceSecurityGroupName" :: NullOrUndefined (String), "SourceSecurityGroupOwnerId" :: NullOrUndefined (String) }
```

<p>Contains the parameters for RevokeSecurityGroupEgress.</p>

#### `RevokeSecurityGroupIngressRequest`

``` purescript
newtype RevokeSecurityGroupIngressRequest
  = RevokeSecurityGroupIngressRequest { "CidrIp" :: NullOrUndefined (String), "FromPort" :: NullOrUndefined (Int), "GroupId" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String), "IpPermissions" :: NullOrUndefined (IpPermissionList), "IpProtocol" :: NullOrUndefined (String), "SourceSecurityGroupName" :: NullOrUndefined (String), "SourceSecurityGroupOwnerId" :: NullOrUndefined (String), "ToPort" :: NullOrUndefined (Int), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for RevokeSecurityGroupIngress.</p>

#### `Route`

``` purescript
newtype Route
  = Route { "DestinationCidrBlock" :: NullOrUndefined (String), "DestinationIpv6CidrBlock" :: NullOrUndefined (String), "DestinationPrefixListId" :: NullOrUndefined (String), "EgressOnlyInternetGatewayId" :: NullOrUndefined (String), "GatewayId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "InstanceOwnerId" :: NullOrUndefined (String), "NatGatewayId" :: NullOrUndefined (String), "NetworkInterfaceId" :: NullOrUndefined (String), "Origin" :: NullOrUndefined (RouteOrigin), "State" :: NullOrUndefined (RouteState), "VpcPeeringConnectionId" :: NullOrUndefined (String) }
```

<p>Describes a route in a route table.</p>

#### `RouteList`

``` purescript
newtype RouteList
  = RouteList (Array Route)
```

#### `RouteOrigin`

``` purescript
newtype RouteOrigin
  = RouteOrigin String
```

#### `RouteState`

``` purescript
newtype RouteState
  = RouteState String
```

#### `RouteTable`

``` purescript
newtype RouteTable
  = RouteTable { "Associations" :: NullOrUndefined (RouteTableAssociationList), "PropagatingVgws" :: NullOrUndefined (PropagatingVgwList), "RouteTableId" :: NullOrUndefined (String), "Routes" :: NullOrUndefined (RouteList), "Tags" :: NullOrUndefined (TagList), "VpcId" :: NullOrUndefined (String) }
```

<p>Describes a route table.</p>

#### `RouteTableAssociation`

``` purescript
newtype RouteTableAssociation
  = RouteTableAssociation { "Main" :: NullOrUndefined (Boolean), "RouteTableAssociationId" :: NullOrUndefined (String), "RouteTableId" :: NullOrUndefined (String), "SubnetId" :: NullOrUndefined (String) }
```

<p>Describes an association between a route table and a subnet.</p>

#### `RouteTableAssociationList`

``` purescript
newtype RouteTableAssociationList
  = RouteTableAssociationList (Array RouteTableAssociation)
```

#### `RouteTableList`

``` purescript
newtype RouteTableList
  = RouteTableList (Array RouteTable)
```

#### `RuleAction`

``` purescript
newtype RuleAction
  = RuleAction String
```

#### `RunInstancesMonitoringEnabled`

``` purescript
newtype RunInstancesMonitoringEnabled
  = RunInstancesMonitoringEnabled { "Enabled" :: Boolean }
```

<p>Describes the monitoring of an instance.</p>

#### `RunInstancesRequest`

``` purescript
newtype RunInstancesRequest
  = RunInstancesRequest { "BlockDeviceMappings" :: NullOrUndefined (BlockDeviceMappingRequestList), "ImageId" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (InstanceType), "Ipv6AddressCount" :: NullOrUndefined (Int), "Ipv6Addresses" :: NullOrUndefined (InstanceIpv6AddressList), "KernelId" :: NullOrUndefined (String), "KeyName" :: NullOrUndefined (String), "MaxCount" :: Int, "MinCount" :: Int, "Monitoring" :: NullOrUndefined (RunInstancesMonitoringEnabled), "Placement" :: NullOrUndefined (Placement), "RamdiskId" :: NullOrUndefined (String), "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdStringList), "SecurityGroups" :: NullOrUndefined (SecurityGroupStringList), "SubnetId" :: NullOrUndefined (String), "UserData" :: NullOrUndefined (String), "AdditionalInfo" :: NullOrUndefined (String), "ClientToken" :: NullOrUndefined (String), "DisableApiTermination" :: NullOrUndefined (Boolean), "DryRun" :: NullOrUndefined (Boolean), "EbsOptimized" :: NullOrUndefined (Boolean), "IamInstanceProfile" :: NullOrUndefined (IamInstanceProfileSpecification), "InstanceInitiatedShutdownBehavior" :: NullOrUndefined (ShutdownBehavior), "NetworkInterfaces" :: NullOrUndefined (InstanceNetworkInterfaceSpecificationList), "PrivateIpAddress" :: NullOrUndefined (String), "ElasticGpuSpecification" :: NullOrUndefined (ElasticGpuSpecifications), "TagSpecifications" :: NullOrUndefined (TagSpecificationList), "LaunchTemplate" :: NullOrUndefined (LaunchTemplateSpecification), "InstanceMarketOptions" :: NullOrUndefined (InstanceMarketOptionsRequest), "CreditSpecification" :: NullOrUndefined (CreditSpecificationRequest) }
```

<p>Contains the parameters for RunInstances.</p>

#### `RunScheduledInstancesRequest`

``` purescript
newtype RunScheduledInstancesRequest
  = RunScheduledInstancesRequest { "ClientToken" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean), "InstanceCount" :: NullOrUndefined (Int), "LaunchSpecification" :: ScheduledInstancesLaunchSpecification, "ScheduledInstanceId" :: String }
```

<p>Contains the parameters for RunScheduledInstances.</p>

#### `RunScheduledInstancesResult`

``` purescript
newtype RunScheduledInstancesResult
  = RunScheduledInstancesResult { "InstanceIdSet" :: NullOrUndefined (InstanceIdSet) }
```

<p>Contains the output of RunScheduledInstances.</p>

#### `S3Storage`

``` purescript
newtype S3Storage
  = S3Storage { "AWSAccessKeyId" :: NullOrUndefined (String), "Bucket" :: NullOrUndefined (String), "Prefix" :: NullOrUndefined (String), "UploadPolicy" :: NullOrUndefined (String), "UploadPolicySignature" :: NullOrUndefined (String) }
```

<p>Describes the storage parameters for S3 and S3 buckets for an instance store-backed AMI.</p>

#### `ScheduledInstance`

``` purescript
newtype ScheduledInstance
  = ScheduledInstance { "AvailabilityZone" :: NullOrUndefined (String), "CreateDate" :: NullOrUndefined (DateTime), "HourlyPrice" :: NullOrUndefined (String), "InstanceCount" :: NullOrUndefined (Int), "InstanceType" :: NullOrUndefined (String), "NetworkPlatform" :: NullOrUndefined (String), "NextSlotStartTime" :: NullOrUndefined (DateTime), "Platform" :: NullOrUndefined (String), "PreviousSlotEndTime" :: NullOrUndefined (DateTime), "Recurrence" :: NullOrUndefined (ScheduledInstanceRecurrence), "ScheduledInstanceId" :: NullOrUndefined (String), "SlotDurationInHours" :: NullOrUndefined (Int), "TermEndDate" :: NullOrUndefined (DateTime), "TermStartDate" :: NullOrUndefined (DateTime), "TotalScheduledInstanceHours" :: NullOrUndefined (Int) }
```

<p>Describes a Scheduled Instance.</p>

#### `ScheduledInstanceAvailability`

``` purescript
newtype ScheduledInstanceAvailability
  = ScheduledInstanceAvailability { "AvailabilityZone" :: NullOrUndefined (String), "AvailableInstanceCount" :: NullOrUndefined (Int), "FirstSlotStartTime" :: NullOrUndefined (DateTime), "HourlyPrice" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (String), "MaxTermDurationInDays" :: NullOrUndefined (Int), "MinTermDurationInDays" :: NullOrUndefined (Int), "NetworkPlatform" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "PurchaseToken" :: NullOrUndefined (String), "Recurrence" :: NullOrUndefined (ScheduledInstanceRecurrence), "SlotDurationInHours" :: NullOrUndefined (Int), "TotalScheduledInstanceHours" :: NullOrUndefined (Int) }
```

<p>Describes a schedule that is available for your Scheduled Instances.</p>

#### `ScheduledInstanceAvailabilitySet`

``` purescript
newtype ScheduledInstanceAvailabilitySet
  = ScheduledInstanceAvailabilitySet (Array ScheduledInstanceAvailability)
```

#### `ScheduledInstanceIdRequestSet`

``` purescript
newtype ScheduledInstanceIdRequestSet
  = ScheduledInstanceIdRequestSet (Array String)
```

#### `ScheduledInstanceRecurrence`

``` purescript
newtype ScheduledInstanceRecurrence
  = ScheduledInstanceRecurrence { "Frequency" :: NullOrUndefined (String), "Interval" :: NullOrUndefined (Int), "OccurrenceDaySet" :: NullOrUndefined (OccurrenceDaySet), "OccurrenceRelativeToEnd" :: NullOrUndefined (Boolean), "OccurrenceUnit" :: NullOrUndefined (String) }
```

<p>Describes the recurring schedule for a Scheduled Instance.</p>

#### `ScheduledInstanceRecurrenceRequest`

``` purescript
newtype ScheduledInstanceRecurrenceRequest
  = ScheduledInstanceRecurrenceRequest { "Frequency" :: NullOrUndefined (String), "Interval" :: NullOrUndefined (Int), "OccurrenceDays" :: NullOrUndefined (OccurrenceDayRequestSet), "OccurrenceRelativeToEnd" :: NullOrUndefined (Boolean), "OccurrenceUnit" :: NullOrUndefined (String) }
```

<p>Describes the recurring schedule for a Scheduled Instance.</p>

#### `ScheduledInstanceSet`

``` purescript
newtype ScheduledInstanceSet
  = ScheduledInstanceSet (Array ScheduledInstance)
```

#### `ScheduledInstancesBlockDeviceMapping`

``` purescript
newtype ScheduledInstancesBlockDeviceMapping
  = ScheduledInstancesBlockDeviceMapping { "DeviceName" :: NullOrUndefined (String), "Ebs" :: NullOrUndefined (ScheduledInstancesEbs), "NoDevice" :: NullOrUndefined (String), "VirtualName" :: NullOrUndefined (String) }
```

<p>Describes a block device mapping for a Scheduled Instance.</p>

#### `ScheduledInstancesBlockDeviceMappingSet`

``` purescript
newtype ScheduledInstancesBlockDeviceMappingSet
  = ScheduledInstancesBlockDeviceMappingSet (Array ScheduledInstancesBlockDeviceMapping)
```

#### `ScheduledInstancesEbs`

``` purescript
newtype ScheduledInstancesEbs
  = ScheduledInstancesEbs { "DeleteOnTermination" :: NullOrUndefined (Boolean), "Encrypted" :: NullOrUndefined (Boolean), "Iops" :: NullOrUndefined (Int), "SnapshotId" :: NullOrUndefined (String), "VolumeSize" :: NullOrUndefined (Int), "VolumeType" :: NullOrUndefined (String) }
```

<p>Describes an EBS volume for a Scheduled Instance.</p>

#### `ScheduledInstancesIamInstanceProfile`

``` purescript
newtype ScheduledInstancesIamInstanceProfile
  = ScheduledInstancesIamInstanceProfile { "Arn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

<p>Describes an IAM instance profile for a Scheduled Instance.</p>

#### `ScheduledInstancesIpv6Address`

``` purescript
newtype ScheduledInstancesIpv6Address
  = ScheduledInstancesIpv6Address { "Ipv6Address" :: NullOrUndefined (Ipv6Address) }
```

<p>Describes an IPv6 address.</p>

#### `ScheduledInstancesIpv6AddressList`

``` purescript
newtype ScheduledInstancesIpv6AddressList
  = ScheduledInstancesIpv6AddressList (Array ScheduledInstancesIpv6Address)
```

#### `ScheduledInstancesLaunchSpecification`

``` purescript
newtype ScheduledInstancesLaunchSpecification
  = ScheduledInstancesLaunchSpecification { "BlockDeviceMappings" :: NullOrUndefined (ScheduledInstancesBlockDeviceMappingSet), "EbsOptimized" :: NullOrUndefined (Boolean), "IamInstanceProfile" :: NullOrUndefined (ScheduledInstancesIamInstanceProfile), "ImageId" :: String, "InstanceType" :: NullOrUndefined (String), "KernelId" :: NullOrUndefined (String), "KeyName" :: NullOrUndefined (String), "Monitoring" :: NullOrUndefined (ScheduledInstancesMonitoring), "NetworkInterfaces" :: NullOrUndefined (ScheduledInstancesNetworkInterfaceSet), "Placement" :: NullOrUndefined (ScheduledInstancesPlacement), "RamdiskId" :: NullOrUndefined (String), "SecurityGroupIds" :: NullOrUndefined (ScheduledInstancesSecurityGroupIdSet), "SubnetId" :: NullOrUndefined (String), "UserData" :: NullOrUndefined (String) }
```

<p>Describes the launch specification for a Scheduled Instance.</p> <p>If you are launching the Scheduled Instance in EC2-VPC, you must specify the ID of the subnet. You can specify the subnet using either <code>SubnetId</code> or <code>NetworkInterface</code>.</p>

#### `ScheduledInstancesMonitoring`

``` purescript
newtype ScheduledInstancesMonitoring
  = ScheduledInstancesMonitoring { "Enabled" :: NullOrUndefined (Boolean) }
```

<p>Describes whether monitoring is enabled for a Scheduled Instance.</p>

#### `ScheduledInstancesNetworkInterface`

``` purescript
newtype ScheduledInstancesNetworkInterface
  = ScheduledInstancesNetworkInterface { "AssociatePublicIpAddress" :: NullOrUndefined (Boolean), "DeleteOnTermination" :: NullOrUndefined (Boolean), "Description" :: NullOrUndefined (String), "DeviceIndex" :: NullOrUndefined (Int), "Groups" :: NullOrUndefined (ScheduledInstancesSecurityGroupIdSet), "Ipv6AddressCount" :: NullOrUndefined (Int), "Ipv6Addresses" :: NullOrUndefined (ScheduledInstancesIpv6AddressList), "NetworkInterfaceId" :: NullOrUndefined (String), "PrivateIpAddress" :: NullOrUndefined (String), "PrivateIpAddressConfigs" :: NullOrUndefined (PrivateIpAddressConfigSet), "SecondaryPrivateIpAddressCount" :: NullOrUndefined (Int), "SubnetId" :: NullOrUndefined (String) }
```

<p>Describes a network interface for a Scheduled Instance.</p>

#### `ScheduledInstancesNetworkInterfaceSet`

``` purescript
newtype ScheduledInstancesNetworkInterfaceSet
  = ScheduledInstancesNetworkInterfaceSet (Array ScheduledInstancesNetworkInterface)
```

#### `ScheduledInstancesPlacement`

``` purescript
newtype ScheduledInstancesPlacement
  = ScheduledInstancesPlacement { "AvailabilityZone" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String) }
```

<p>Describes the placement for a Scheduled Instance.</p>

#### `ScheduledInstancesPrivateIpAddressConfig`

``` purescript
newtype ScheduledInstancesPrivateIpAddressConfig
  = ScheduledInstancesPrivateIpAddressConfig { "Primary" :: NullOrUndefined (Boolean), "PrivateIpAddress" :: NullOrUndefined (String) }
```

<p>Describes a private IPv4 address for a Scheduled Instance.</p>

#### `ScheduledInstancesSecurityGroupIdSet`

``` purescript
newtype ScheduledInstancesSecurityGroupIdSet
  = ScheduledInstancesSecurityGroupIdSet (Array String)
```

#### `SecurityGroup`

``` purescript
newtype SecurityGroup
  = SecurityGroup { "Description" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String), "IpPermissions" :: NullOrUndefined (IpPermissionList), "OwnerId" :: NullOrUndefined (String), "GroupId" :: NullOrUndefined (String), "IpPermissionsEgress" :: NullOrUndefined (IpPermissionList), "Tags" :: NullOrUndefined (TagList), "VpcId" :: NullOrUndefined (String) }
```

<p>Describes a security group</p>

#### `SecurityGroupIdStringList`

``` purescript
newtype SecurityGroupIdStringList
  = SecurityGroupIdStringList (Array String)
```

#### `SecurityGroupIdentifier`

``` purescript
newtype SecurityGroupIdentifier
  = SecurityGroupIdentifier { "GroupId" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String) }
```

<p>Describes a security group.</p>

#### `SecurityGroupList`

``` purescript
newtype SecurityGroupList
  = SecurityGroupList (Array SecurityGroup)
```

#### `SecurityGroupReference`

``` purescript
newtype SecurityGroupReference
  = SecurityGroupReference { "GroupId" :: String, "ReferencingVpcId" :: String, "VpcPeeringConnectionId" :: NullOrUndefined (String) }
```

<p>Describes a VPC with a security group that references your security group.</p>

#### `SecurityGroupReferences`

``` purescript
newtype SecurityGroupReferences
  = SecurityGroupReferences (Array SecurityGroupReference)
```

#### `SecurityGroupStringList`

``` purescript
newtype SecurityGroupStringList
  = SecurityGroupStringList (Array String)
```

#### `ServiceConfiguration`

``` purescript
newtype ServiceConfiguration
  = ServiceConfiguration { "ServiceType" :: NullOrUndefined (ServiceTypeDetailSet), "ServiceId" :: NullOrUndefined (String), "ServiceName" :: NullOrUndefined (String), "ServiceState" :: NullOrUndefined (ServiceState), "AvailabilityZones" :: NullOrUndefined (ValueStringList), "AcceptanceRequired" :: NullOrUndefined (Boolean), "NetworkLoadBalancerArns" :: NullOrUndefined (ValueStringList), "BaseEndpointDnsNames" :: NullOrUndefined (ValueStringList), "PrivateDnsName" :: NullOrUndefined (String) }
```

<p>Describes a service configuration for a VPC endpoint service.</p>

#### `ServiceConfigurationSet`

``` purescript
newtype ServiceConfigurationSet
  = ServiceConfigurationSet (Array ServiceConfiguration)
```

#### `ServiceDetail`

``` purescript
newtype ServiceDetail
  = ServiceDetail { "ServiceName" :: NullOrUndefined (String), "ServiceType" :: NullOrUndefined (ServiceTypeDetailSet), "AvailabilityZones" :: NullOrUndefined (ValueStringList), "Owner" :: NullOrUndefined (String), "BaseEndpointDnsNames" :: NullOrUndefined (ValueStringList), "PrivateDnsName" :: NullOrUndefined (String), "VpcEndpointPolicySupported" :: NullOrUndefined (Boolean), "AcceptanceRequired" :: NullOrUndefined (Boolean) }
```

<p>Describes a VPC endpoint service.</p>

#### `ServiceDetailSet`

``` purescript
newtype ServiceDetailSet
  = ServiceDetailSet (Array ServiceDetail)
```

#### `ServiceState`

``` purescript
newtype ServiceState
  = ServiceState String
```

#### `ServiceType`

``` purescript
newtype ServiceType
  = ServiceType String
```

#### `ServiceTypeDetail`

``` purescript
newtype ServiceTypeDetail
  = ServiceTypeDetail { "ServiceType" :: NullOrUndefined (ServiceType) }
```

<p>Describes the type of service for a VPC endpoint.</p>

#### `ServiceTypeDetailSet`

``` purescript
newtype ServiceTypeDetailSet
  = ServiceTypeDetailSet (Array ServiceTypeDetail)
```

#### `ShutdownBehavior`

``` purescript
newtype ShutdownBehavior
  = ShutdownBehavior String
```

#### `SlotDateTimeRangeRequest`

``` purescript
newtype SlotDateTimeRangeRequest
  = SlotDateTimeRangeRequest { "EarliestTime" :: DateTime, "LatestTime" :: DateTime }
```

<p>Describes the time period for a Scheduled Instance to start its first schedule. The time period must span less than one day.</p>

#### `SlotStartTimeRangeRequest`

``` purescript
newtype SlotStartTimeRangeRequest
  = SlotStartTimeRangeRequest { "EarliestTime" :: NullOrUndefined (DateTime), "LatestTime" :: NullOrUndefined (DateTime) }
```

<p>Describes the time period for a Scheduled Instance to start its first schedule.</p>

#### `Snapshot`

``` purescript
newtype Snapshot
  = Snapshot { "DataEncryptionKeyId" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Encrypted" :: NullOrUndefined (Boolean), "KmsKeyId" :: NullOrUndefined (String), "OwnerId" :: NullOrUndefined (String), "Progress" :: NullOrUndefined (String), "SnapshotId" :: NullOrUndefined (String), "StartTime" :: NullOrUndefined (DateTime), "State" :: NullOrUndefined (SnapshotState), "StateMessage" :: NullOrUndefined (String), "VolumeId" :: NullOrUndefined (String), "VolumeSize" :: NullOrUndefined (Int), "OwnerAlias" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a snapshot.</p>

#### `SnapshotAttributeName`

``` purescript
newtype SnapshotAttributeName
  = SnapshotAttributeName String
```

#### `SnapshotDetail`

``` purescript
newtype SnapshotDetail
  = SnapshotDetail { "Description" :: NullOrUndefined (String), "DeviceName" :: NullOrUndefined (String), "DiskImageSize" :: NullOrUndefined (Number), "Format" :: NullOrUndefined (String), "Progress" :: NullOrUndefined (String), "SnapshotId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "StatusMessage" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String), "UserBucket" :: NullOrUndefined (UserBucketDetails) }
```

<p>Describes the snapshot created from the imported disk.</p>

#### `SnapshotDetailList`

``` purescript
newtype SnapshotDetailList
  = SnapshotDetailList (Array SnapshotDetail)
```

#### `SnapshotDiskContainer`

``` purescript
newtype SnapshotDiskContainer
  = SnapshotDiskContainer { "Description" :: NullOrUndefined (String), "Format" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String), "UserBucket" :: NullOrUndefined (UserBucket) }
```

<p>The disk container object for the import snapshot request.</p>

#### `SnapshotIdStringList`

``` purescript
newtype SnapshotIdStringList
  = SnapshotIdStringList (Array String)
```

#### `SnapshotList`

``` purescript
newtype SnapshotList
  = SnapshotList (Array Snapshot)
```

#### `SnapshotState`

``` purescript
newtype SnapshotState
  = SnapshotState String
```

#### `SnapshotTaskDetail`

``` purescript
newtype SnapshotTaskDetail
  = SnapshotTaskDetail { "Description" :: NullOrUndefined (String), "DiskImageSize" :: NullOrUndefined (Number), "Format" :: NullOrUndefined (String), "Progress" :: NullOrUndefined (String), "SnapshotId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "StatusMessage" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String), "UserBucket" :: NullOrUndefined (UserBucketDetails) }
```

<p>Details about the import snapshot task.</p>

#### `SpotDatafeedSubscription`

``` purescript
newtype SpotDatafeedSubscription
  = SpotDatafeedSubscription { "Bucket" :: NullOrUndefined (String), "Fault" :: NullOrUndefined (SpotInstanceStateFault), "OwnerId" :: NullOrUndefined (String), "Prefix" :: NullOrUndefined (String), "State" :: NullOrUndefined (DatafeedSubscriptionState) }
```

<p>Describes the data feed for a Spot Instance.</p>

#### `SpotFleetLaunchSpecification`

``` purescript
newtype SpotFleetLaunchSpecification
  = SpotFleetLaunchSpecification { "SecurityGroups" :: NullOrUndefined (GroupIdentifierList), "AddressingType" :: NullOrUndefined (String), "BlockDeviceMappings" :: NullOrUndefined (BlockDeviceMappingList), "EbsOptimized" :: NullOrUndefined (Boolean), "IamInstanceProfile" :: NullOrUndefined (IamInstanceProfileSpecification), "ImageId" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (InstanceType), "KernelId" :: NullOrUndefined (String), "KeyName" :: NullOrUndefined (String), "Monitoring" :: NullOrUndefined (SpotFleetMonitoring), "NetworkInterfaces" :: NullOrUndefined (InstanceNetworkInterfaceSpecificationList), "Placement" :: NullOrUndefined (SpotPlacement), "RamdiskId" :: NullOrUndefined (String), "SpotPrice" :: NullOrUndefined (String), "SubnetId" :: NullOrUndefined (String), "UserData" :: NullOrUndefined (String), "WeightedCapacity" :: NullOrUndefined (Number), "TagSpecifications" :: NullOrUndefined (SpotFleetTagSpecificationList) }
```

<p>Describes the launch specification for one or more Spot Instances.</p>

#### `SpotFleetMonitoring`

``` purescript
newtype SpotFleetMonitoring
  = SpotFleetMonitoring { "Enabled" :: NullOrUndefined (Boolean) }
```

<p>Describes whether monitoring is enabled.</p>

#### `SpotFleetRequestConfig`

``` purescript
newtype SpotFleetRequestConfig
  = SpotFleetRequestConfig { "ActivityStatus" :: NullOrUndefined (ActivityStatus), "CreateTime" :: DateTime, "SpotFleetRequestConfig" :: SpotFleetRequestConfigData, "SpotFleetRequestId" :: String, "SpotFleetRequestState" :: BatchState }
```

<p>Describes a Spot Fleet request.</p>

#### `SpotFleetRequestConfigData`

``` purescript
newtype SpotFleetRequestConfigData
  = SpotFleetRequestConfigData { "AllocationStrategy" :: NullOrUndefined (AllocationStrategy), "ClientToken" :: NullOrUndefined (String), "ExcessCapacityTerminationPolicy" :: NullOrUndefined (ExcessCapacityTerminationPolicy), "FulfilledCapacity" :: NullOrUndefined (Number), "IamFleetRole" :: String, "LaunchSpecifications" :: NullOrUndefined (LaunchSpecsList), "LaunchTemplateConfigs" :: NullOrUndefined (LaunchTemplateConfigList), "SpotPrice" :: NullOrUndefined (String), "TargetCapacity" :: Int, "TerminateInstancesWithExpiration" :: NullOrUndefined (Boolean), "Type" :: NullOrUndefined (FleetType), "ValidFrom" :: NullOrUndefined (DateTime), "ValidUntil" :: NullOrUndefined (DateTime), "ReplaceUnhealthyInstances" :: NullOrUndefined (Boolean), "InstanceInterruptionBehavior" :: NullOrUndefined (InstanceInterruptionBehavior), "LoadBalancersConfig" :: NullOrUndefined (LoadBalancersConfig) }
```

<p>Describes the configuration of a Spot Fleet request.</p>

#### `SpotFleetRequestConfigSet`

``` purescript
newtype SpotFleetRequestConfigSet
  = SpotFleetRequestConfigSet (Array SpotFleetRequestConfig)
```

#### `SpotFleetTagSpecification`

``` purescript
newtype SpotFleetTagSpecification
  = SpotFleetTagSpecification { "ResourceType" :: NullOrUndefined (ResourceType), "Tags" :: NullOrUndefined (TagList) }
```

<p>The tags for a Spot Fleet resource.</p>

#### `SpotFleetTagSpecificationList`

``` purescript
newtype SpotFleetTagSpecificationList
  = SpotFleetTagSpecificationList (Array SpotFleetTagSpecification)
```

#### `SpotInstanceRequest`

``` purescript
newtype SpotInstanceRequest
  = SpotInstanceRequest { "ActualBlockHourlyPrice" :: NullOrUndefined (String), "AvailabilityZoneGroup" :: NullOrUndefined (String), "BlockDurationMinutes" :: NullOrUndefined (Int), "CreateTime" :: NullOrUndefined (DateTime), "Fault" :: NullOrUndefined (SpotInstanceStateFault), "InstanceId" :: NullOrUndefined (String), "LaunchGroup" :: NullOrUndefined (String), "LaunchSpecification" :: NullOrUndefined (LaunchSpecification), "LaunchedAvailabilityZone" :: NullOrUndefined (String), "ProductDescription" :: NullOrUndefined (RIProductDescription), "SpotInstanceRequestId" :: NullOrUndefined (String), "SpotPrice" :: NullOrUndefined (String), "State" :: NullOrUndefined (SpotInstanceState), "Status" :: NullOrUndefined (SpotInstanceStatus), "Tags" :: NullOrUndefined (TagList), "Type" :: NullOrUndefined (SpotInstanceType), "ValidFrom" :: NullOrUndefined (DateTime), "ValidUntil" :: NullOrUndefined (DateTime), "InstanceInterruptionBehavior" :: NullOrUndefined (InstanceInterruptionBehavior) }
```

<p>Describes a Spot Instance request.</p>

#### `SpotInstanceRequestIdList`

``` purescript
newtype SpotInstanceRequestIdList
  = SpotInstanceRequestIdList (Array String)
```

#### `SpotInstanceRequestList`

``` purescript
newtype SpotInstanceRequestList
  = SpotInstanceRequestList (Array SpotInstanceRequest)
```

#### `SpotInstanceState`

``` purescript
newtype SpotInstanceState
  = SpotInstanceState String
```

#### `SpotInstanceStateFault`

``` purescript
newtype SpotInstanceStateFault
  = SpotInstanceStateFault { "Code" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

<p>Describes a Spot Instance state change.</p>

#### `SpotInstanceStatus`

``` purescript
newtype SpotInstanceStatus
  = SpotInstanceStatus { "Code" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String), "UpdateTime" :: NullOrUndefined (DateTime) }
```

<p>Describes the status of a Spot Instance request.</p>

#### `SpotInstanceType`

``` purescript
newtype SpotInstanceType
  = SpotInstanceType String
```

#### `SpotMarketOptions`

``` purescript
newtype SpotMarketOptions
  = SpotMarketOptions { "MaxPrice" :: NullOrUndefined (String), "SpotInstanceType" :: NullOrUndefined (SpotInstanceType), "BlockDurationMinutes" :: NullOrUndefined (Int), "ValidUntil" :: NullOrUndefined (DateTime), "InstanceInterruptionBehavior" :: NullOrUndefined (InstanceInterruptionBehavior) }
```

<p>The options for Spot Instances.</p>

#### `SpotPlacement`

``` purescript
newtype SpotPlacement
  = SpotPlacement { "AvailabilityZone" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String), "Tenancy" :: NullOrUndefined (Tenancy) }
```

<p>Describes Spot Instance placement.</p>

#### `SpotPrice`

``` purescript
newtype SpotPrice
  = SpotPrice { "AvailabilityZone" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (InstanceType), "ProductDescription" :: NullOrUndefined (RIProductDescription), "SpotPrice" :: NullOrUndefined (String), "Number" :: NullOrUndefined (DateTime) }
```

<p>Describes the maximum price per hour that you are willing to pay for a Spot Instance.</p>

#### `SpotPriceHistoryList`

``` purescript
newtype SpotPriceHistoryList
  = SpotPriceHistoryList (Array SpotPrice)
```

#### `StaleIpPermission`

``` purescript
newtype StaleIpPermission
  = StaleIpPermission { "FromPort" :: NullOrUndefined (Int), "IpProtocol" :: NullOrUndefined (String), "IpRanges" :: NullOrUndefined (IpRanges), "PrefixListIds" :: NullOrUndefined (PrefixListIdSet), "ToPort" :: NullOrUndefined (Int), "UserIdGroupPairs" :: NullOrUndefined (UserIdGroupPairSet) }
```

<p>Describes a stale rule in a security group.</p>

#### `StaleIpPermissionSet`

``` purescript
newtype StaleIpPermissionSet
  = StaleIpPermissionSet (Array StaleIpPermission)
```

#### `StaleSecurityGroup`

``` purescript
newtype StaleSecurityGroup
  = StaleSecurityGroup { "Description" :: NullOrUndefined (String), "GroupId" :: String, "GroupName" :: NullOrUndefined (String), "StaleIpPermissions" :: NullOrUndefined (StaleIpPermissionSet), "StaleIpPermissionsEgress" :: NullOrUndefined (StaleIpPermissionSet), "VpcId" :: NullOrUndefined (String) }
```

<p>Describes a stale security group (a security group that contains stale rules).</p>

#### `StaleSecurityGroupSet`

``` purescript
newtype StaleSecurityGroupSet
  = StaleSecurityGroupSet (Array StaleSecurityGroup)
```

#### `StartInstancesRequest`

``` purescript
newtype StartInstancesRequest
  = StartInstancesRequest { "InstanceIds" :: InstanceIdStringList, "AdditionalInfo" :: NullOrUndefined (String), "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for StartInstances.</p>

#### `StartInstancesResult`

``` purescript
newtype StartInstancesResult
  = StartInstancesResult { "StartingInstances" :: NullOrUndefined (InstanceStateChangeList) }
```

<p>Contains the output of StartInstances.</p>

#### `State`

``` purescript
newtype State
  = State String
```

#### `StateReason`

``` purescript
newtype StateReason
  = StateReason { "Code" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

<p>Describes a state change.</p>

#### `Status`

``` purescript
newtype Status
  = Status String
```

#### `StatusName`

``` purescript
newtype StatusName
  = StatusName String
```

#### `StatusType`

``` purescript
newtype StatusType
  = StatusType String
```

#### `StopInstancesRequest`

``` purescript
newtype StopInstancesRequest
  = StopInstancesRequest { "InstanceIds" :: InstanceIdStringList, "DryRun" :: NullOrUndefined (Boolean), "Force" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for StopInstances.</p>

#### `StopInstancesResult`

``` purescript
newtype StopInstancesResult
  = StopInstancesResult { "StoppingInstances" :: NullOrUndefined (InstanceStateChangeList) }
```

<p>Contains the output of StopInstances.</p>

#### `Storage`

``` purescript
newtype Storage
  = Storage { "S3" :: NullOrUndefined (S3Storage) }
```

<p>Describes the storage location for an instance store-backed AMI.</p>

#### `StorageLocation`

``` purescript
newtype StorageLocation
  = StorageLocation { "Bucket" :: NullOrUndefined (String), "Key" :: NullOrUndefined (String) }
```

<p>Describes a storage location in Amazon S3.</p>

#### `Subnet`

``` purescript
newtype Subnet
  = Subnet { "AvailabilityZone" :: NullOrUndefined (String), "AvailableIpAddressCount" :: NullOrUndefined (Int), "CidrBlock" :: NullOrUndefined (String), "DefaultForAz" :: NullOrUndefined (Boolean), "MapPublicIpOnLaunch" :: NullOrUndefined (Boolean), "State" :: NullOrUndefined (SubnetState), "SubnetId" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "AssignIpv6AddressOnCreation" :: NullOrUndefined (Boolean), "Ipv6CidrBlockAssociationSet" :: NullOrUndefined (SubnetIpv6CidrBlockAssociationSet), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a subnet.</p>

#### `SubnetCidrBlockState`

``` purescript
newtype SubnetCidrBlockState
  = SubnetCidrBlockState { "State" :: NullOrUndefined (SubnetCidrBlockStateCode), "StatusMessage" :: NullOrUndefined (String) }
```

<p>Describes the state of a CIDR block.</p>

#### `SubnetCidrBlockStateCode`

``` purescript
newtype SubnetCidrBlockStateCode
  = SubnetCidrBlockStateCode String
```

#### `SubnetIdStringList`

``` purescript
newtype SubnetIdStringList
  = SubnetIdStringList (Array String)
```

#### `SubnetIpv6CidrBlockAssociation`

``` purescript
newtype SubnetIpv6CidrBlockAssociation
  = SubnetIpv6CidrBlockAssociation { "AssociationId" :: NullOrUndefined (String), "Ipv6CidrBlock" :: NullOrUndefined (String), "Ipv6CidrBlockState" :: NullOrUndefined (SubnetCidrBlockState) }
```

<p>Describes an IPv6 CIDR block associated with a subnet.</p>

#### `SubnetIpv6CidrBlockAssociationSet`

``` purescript
newtype SubnetIpv6CidrBlockAssociationSet
  = SubnetIpv6CidrBlockAssociationSet (Array SubnetIpv6CidrBlockAssociation)
```

#### `SubnetList`

``` purescript
newtype SubnetList
  = SubnetList (Array Subnet)
```

#### `SubnetState`

``` purescript
newtype SubnetState
  = SubnetState String
```

#### `SuccessfulInstanceCreditSpecificationItem`

``` purescript
newtype SuccessfulInstanceCreditSpecificationItem
  = SuccessfulInstanceCreditSpecificationItem { "InstanceId" :: NullOrUndefined (String) }
```

<p>Describes the T2 instance whose credit option for CPU usage was successfully modified.</p>

#### `SuccessfulInstanceCreditSpecificationSet`

``` purescript
newtype SuccessfulInstanceCreditSpecificationSet
  = SuccessfulInstanceCreditSpecificationSet (Array SuccessfulInstanceCreditSpecificationItem)
```

#### `SummaryStatus`

``` purescript
newtype SummaryStatus
  = SummaryStatus String
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (String), "Value" :: NullOrUndefined (String) }
```

<p>Describes a tag.</p>

#### `TagDescription`

``` purescript
newtype TagDescription
  = TagDescription { "Key" :: NullOrUndefined (String), "ResourceId" :: NullOrUndefined (String), "ResourceType" :: NullOrUndefined (ResourceType), "Value" :: NullOrUndefined (String) }
```

<p>Describes a tag.</p>

#### `TagDescriptionList`

``` purescript
newtype TagDescriptionList
  = TagDescriptionList (Array TagDescription)
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

#### `TagSpecification`

``` purescript
newtype TagSpecification
  = TagSpecification { "ResourceType" :: NullOrUndefined (ResourceType), "Tags" :: NullOrUndefined (TagList) }
```

<p>The tags to apply to a resource when the resource is being created.</p>

#### `TagSpecificationList`

``` purescript
newtype TagSpecificationList
  = TagSpecificationList (Array TagSpecification)
```

#### `TargetConfiguration`

``` purescript
newtype TargetConfiguration
  = TargetConfiguration { "InstanceCount" :: NullOrUndefined (Int), "OfferingId" :: NullOrUndefined (String) }
```

<p>Information about the Convertible Reserved Instance offering.</p>

#### `TargetConfigurationRequest`

``` purescript
newtype TargetConfigurationRequest
  = TargetConfigurationRequest { "InstanceCount" :: NullOrUndefined (Int), "OfferingId" :: String }
```

<p>Details about the target configuration.</p>

#### `TargetConfigurationRequestSet`

``` purescript
newtype TargetConfigurationRequestSet
  = TargetConfigurationRequestSet (Array TargetConfigurationRequest)
```

#### `TargetGroup`

``` purescript
newtype TargetGroup
  = TargetGroup { "Arn" :: String }
```

<p>Describes a load balancer target group.</p>

#### `TargetGroups`

``` purescript
newtype TargetGroups
  = TargetGroups (Array TargetGroup)
```

#### `TargetGroupsConfig`

``` purescript
newtype TargetGroupsConfig
  = TargetGroupsConfig { "TargetGroups" :: TargetGroups }
```

<p>Describes the target groups to attach to a Spot Fleet. Spot Fleet registers the running Spot Instances with these target groups.</p>

#### `TargetReservationValue`

``` purescript
newtype TargetReservationValue
  = TargetReservationValue { "ReservationValue" :: NullOrUndefined (ReservationValue), "TargetConfiguration" :: NullOrUndefined (TargetConfiguration) }
```

<p>The total value of the new Convertible Reserved Instances.</p>

#### `TargetReservationValueSet`

``` purescript
newtype TargetReservationValueSet
  = TargetReservationValueSet (Array TargetReservationValue)
```

#### `TelemetryStatus`

``` purescript
newtype TelemetryStatus
  = TelemetryStatus String
```

#### `Tenancy`

``` purescript
newtype Tenancy
  = Tenancy String
```

#### `TerminateInstancesRequest`

``` purescript
newtype TerminateInstancesRequest
  = TerminateInstancesRequest { "InstanceIds" :: InstanceIdStringList, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for TerminateInstances.</p>

#### `TerminateInstancesResult`

``` purescript
newtype TerminateInstancesResult
  = TerminateInstancesResult { "TerminatingInstances" :: NullOrUndefined (InstanceStateChangeList) }
```

<p>Contains the output of TerminateInstances.</p>

#### `TrafficType`

``` purescript
newtype TrafficType
  = TrafficType String
```

#### `TunnelOptionsList`

``` purescript
newtype TunnelOptionsList
  = TunnelOptionsList (Array VpnTunnelOptionsSpecification)
```

#### `UnassignIpv6AddressesRequest`

``` purescript
newtype UnassignIpv6AddressesRequest
  = UnassignIpv6AddressesRequest { "Ipv6Addresses" :: Ipv6AddressList, "NetworkInterfaceId" :: String }
```

#### `UnassignIpv6AddressesResult`

``` purescript
newtype UnassignIpv6AddressesResult
  = UnassignIpv6AddressesResult { "NetworkInterfaceId" :: NullOrUndefined (String), "UnassignedIpv6Addresses" :: NullOrUndefined (Ipv6AddressList) }
```

#### `UnassignPrivateIpAddressesRequest`

``` purescript
newtype UnassignPrivateIpAddressesRequest
  = UnassignPrivateIpAddressesRequest { "NetworkInterfaceId" :: String, "PrivateIpAddresses" :: PrivateIpAddressStringList }
```

<p>Contains the parameters for UnassignPrivateIpAddresses.</p>

#### `UnmonitorInstancesRequest`

``` purescript
newtype UnmonitorInstancesRequest
  = UnmonitorInstancesRequest { "InstanceIds" :: InstanceIdStringList, "DryRun" :: NullOrUndefined (Boolean) }
```

<p>Contains the parameters for UnmonitorInstances.</p>

#### `UnmonitorInstancesResult`

``` purescript
newtype UnmonitorInstancesResult
  = UnmonitorInstancesResult { "InstanceMonitorings" :: NullOrUndefined (InstanceMonitoringList) }
```

<p>Contains the output of UnmonitorInstances.</p>

#### `UnsuccessfulInstanceCreditSpecificationErrorCode`

``` purescript
newtype UnsuccessfulInstanceCreditSpecificationErrorCode
  = UnsuccessfulInstanceCreditSpecificationErrorCode String
```

#### `UnsuccessfulInstanceCreditSpecificationItem`

``` purescript
newtype UnsuccessfulInstanceCreditSpecificationItem
  = UnsuccessfulInstanceCreditSpecificationItem { "InstanceId" :: NullOrUndefined (String), "Error" :: NullOrUndefined (UnsuccessfulInstanceCreditSpecificationItemError) }
```

<p>Describes the T2 instance whose credit option for CPU usage was not modified.</p>

#### `UnsuccessfulInstanceCreditSpecificationItemError`

``` purescript
newtype UnsuccessfulInstanceCreditSpecificationItemError
  = UnsuccessfulInstanceCreditSpecificationItemError { "Code" :: NullOrUndefined (UnsuccessfulInstanceCreditSpecificationErrorCode), "Message" :: NullOrUndefined (String) }
```

<p>Information about the error for the T2 instance whose credit option for CPU usage was not modified.</p>

#### `UnsuccessfulInstanceCreditSpecificationSet`

``` purescript
newtype UnsuccessfulInstanceCreditSpecificationSet
  = UnsuccessfulInstanceCreditSpecificationSet (Array UnsuccessfulInstanceCreditSpecificationItem)
```

#### `UnsuccessfulItem`

``` purescript
newtype UnsuccessfulItem
  = UnsuccessfulItem { "Error" :: UnsuccessfulItemError, "ResourceId" :: NullOrUndefined (String) }
```

<p>Information about items that were not successfully processed in a batch call.</p>

#### `UnsuccessfulItemError`

``` purescript
newtype UnsuccessfulItemError
  = UnsuccessfulItemError { "Code" :: String, "Message" :: String }
```

<p>Information about the error that occurred. For more information about errors, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html">Error Codes</a>.</p>

#### `UnsuccessfulItemList`

``` purescript
newtype UnsuccessfulItemList
  = UnsuccessfulItemList (Array UnsuccessfulItem)
```

#### `UnsuccessfulItemSet`

``` purescript
newtype UnsuccessfulItemSet
  = UnsuccessfulItemSet (Array UnsuccessfulItem)
```

#### `UpdateSecurityGroupRuleDescriptionsEgressRequest`

``` purescript
newtype UpdateSecurityGroupRuleDescriptionsEgressRequest
  = UpdateSecurityGroupRuleDescriptionsEgressRequest { "DryRun" :: NullOrUndefined (Boolean), "GroupId" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String), "IpPermissions" :: IpPermissionList }
```

<p>Contains the parameters for UpdateSecurityGroupRuleDescriptionsEgress.</p>

#### `UpdateSecurityGroupRuleDescriptionsEgressResult`

``` purescript
newtype UpdateSecurityGroupRuleDescriptionsEgressResult
  = UpdateSecurityGroupRuleDescriptionsEgressResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of UpdateSecurityGroupRuleDescriptionsEgress.</p>

#### `UpdateSecurityGroupRuleDescriptionsIngressRequest`

``` purescript
newtype UpdateSecurityGroupRuleDescriptionsIngressRequest
  = UpdateSecurityGroupRuleDescriptionsIngressRequest { "DryRun" :: NullOrUndefined (Boolean), "GroupId" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String), "IpPermissions" :: IpPermissionList }
```

<p>Contains the parameters for UpdateSecurityGroupRuleDescriptionsIngress.</p>

#### `UpdateSecurityGroupRuleDescriptionsIngressResult`

``` purescript
newtype UpdateSecurityGroupRuleDescriptionsIngressResult
  = UpdateSecurityGroupRuleDescriptionsIngressResult { "Return" :: NullOrUndefined (Boolean) }
```

<p>Contains the output of UpdateSecurityGroupRuleDescriptionsIngress.</p>

#### `UserBucket`

``` purescript
newtype UserBucket
  = UserBucket { "S3Bucket" :: NullOrUndefined (String), "S3Key" :: NullOrUndefined (String) }
```

<p>Describes the S3 bucket for the disk image.</p>

#### `UserBucketDetails`

``` purescript
newtype UserBucketDetails
  = UserBucketDetails { "S3Bucket" :: NullOrUndefined (String), "S3Key" :: NullOrUndefined (String) }
```

<p>Describes the S3 bucket for the disk image.</p>

#### `UserData`

``` purescript
newtype UserData
  = UserData { "Data" :: NullOrUndefined (String) }
```

<p>Describes the user data for an instance.</p>

#### `UserGroupStringList`

``` purescript
newtype UserGroupStringList
  = UserGroupStringList (Array String)
```

#### `UserIdGroupPair`

``` purescript
newtype UserIdGroupPair
  = UserIdGroupPair { "Description" :: NullOrUndefined (String), "GroupId" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String), "PeeringStatus" :: NullOrUndefined (String), "UserId" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "VpcPeeringConnectionId" :: NullOrUndefined (String) }
```

<p>Describes a security group and AWS account ID pair.</p>

#### `UserIdGroupPairList`

``` purescript
newtype UserIdGroupPairList
  = UserIdGroupPairList (Array UserIdGroupPair)
```

#### `UserIdGroupPairSet`

``` purescript
newtype UserIdGroupPairSet
  = UserIdGroupPairSet (Array UserIdGroupPair)
```

#### `UserIdStringList`

``` purescript
newtype UserIdStringList
  = UserIdStringList (Array String)
```

#### `ValueStringList`

``` purescript
newtype ValueStringList
  = ValueStringList (Array String)
```

#### `VersionDescription`

``` purescript
newtype VersionDescription
  = VersionDescription String
```

#### `VersionStringList`

``` purescript
newtype VersionStringList
  = VersionStringList (Array String)
```

#### `VgwTelemetry`

``` purescript
newtype VgwTelemetry
  = VgwTelemetry { "AcceptedRouteCount" :: NullOrUndefined (Int), "LastStatusChange" :: NullOrUndefined (DateTime), "OutsideIpAddress" :: NullOrUndefined (String), "Status" :: NullOrUndefined (TelemetryStatus), "StatusMessage" :: NullOrUndefined (String) }
```

<p>Describes telemetry for a VPN tunnel.</p>

#### `VgwTelemetryList`

``` purescript
newtype VgwTelemetryList
  = VgwTelemetryList (Array VgwTelemetry)
```

#### `VirtualizationType`

``` purescript
newtype VirtualizationType
  = VirtualizationType String
```

#### `Volume`

``` purescript
newtype Volume
  = Volume { "Attachments" :: NullOrUndefined (VolumeAttachmentList), "AvailabilityZone" :: NullOrUndefined (String), "CreateTime" :: NullOrUndefined (DateTime), "Encrypted" :: NullOrUndefined (Boolean), "KmsKeyId" :: NullOrUndefined (String), "Size" :: NullOrUndefined (Int), "SnapshotId" :: NullOrUndefined (String), "State" :: NullOrUndefined (VolumeState), "VolumeId" :: NullOrUndefined (String), "Iops" :: NullOrUndefined (Int), "Tags" :: NullOrUndefined (TagList), "VolumeType" :: NullOrUndefined (VolumeType) }
```

<p>Describes a volume.</p>

#### `VolumeAttachment`

``` purescript
newtype VolumeAttachment
  = VolumeAttachment { "AttachTime" :: NullOrUndefined (DateTime), "Device" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "State" :: NullOrUndefined (VolumeAttachmentState), "VolumeId" :: NullOrUndefined (String), "DeleteOnTermination" :: NullOrUndefined (Boolean) }
```

<p>Describes volume attachment details.</p>

#### `VolumeAttachmentList`

``` purescript
newtype VolumeAttachmentList
  = VolumeAttachmentList (Array VolumeAttachment)
```

#### `VolumeAttachmentState`

``` purescript
newtype VolumeAttachmentState
  = VolumeAttachmentState String
```

#### `VolumeAttributeName`

``` purescript
newtype VolumeAttributeName
  = VolumeAttributeName String
```

#### `VolumeDetail`

``` purescript
newtype VolumeDetail
  = VolumeDetail { "Size" :: Number }
```

<p>Describes an EBS volume.</p>

#### `VolumeIdStringList`

``` purescript
newtype VolumeIdStringList
  = VolumeIdStringList (Array String)
```

#### `VolumeList`

``` purescript
newtype VolumeList
  = VolumeList (Array Volume)
```

#### `VolumeModification`

``` purescript
newtype VolumeModification
  = VolumeModification { "VolumeId" :: NullOrUndefined (String), "ModificationState" :: NullOrUndefined (VolumeModificationState), "StatusMessage" :: NullOrUndefined (String), "TargetSize" :: NullOrUndefined (Int), "TargetIops" :: NullOrUndefined (Int), "TargetVolumeType" :: NullOrUndefined (VolumeType), "OriginalSize" :: NullOrUndefined (Int), "OriginalIops" :: NullOrUndefined (Int), "OriginalVolumeType" :: NullOrUndefined (VolumeType), "Progress" :: NullOrUndefined (Number), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime) }
```

<p>Describes the modification status of an EBS volume.</p> <p>If the volume has never been modified, some element values will be null.</p>

#### `VolumeModificationList`

``` purescript
newtype VolumeModificationList
  = VolumeModificationList (Array VolumeModification)
```

#### `VolumeModificationState`

``` purescript
newtype VolumeModificationState
  = VolumeModificationState String
```

#### `VolumeState`

``` purescript
newtype VolumeState
  = VolumeState String
```

#### `VolumeStatusAction`

``` purescript
newtype VolumeStatusAction
  = VolumeStatusAction { "Code" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "EventId" :: NullOrUndefined (String), "EventType" :: NullOrUndefined (String) }
```

<p>Describes a volume status operation code.</p>

#### `VolumeStatusActionsList`

``` purescript
newtype VolumeStatusActionsList
  = VolumeStatusActionsList (Array VolumeStatusAction)
```

#### `VolumeStatusDetails`

``` purescript
newtype VolumeStatusDetails
  = VolumeStatusDetails { "Name" :: NullOrUndefined (VolumeStatusName), "Status" :: NullOrUndefined (String) }
```

<p>Describes a volume status.</p>

#### `VolumeStatusDetailsList`

``` purescript
newtype VolumeStatusDetailsList
  = VolumeStatusDetailsList (Array VolumeStatusDetails)
```

#### `VolumeStatusEvent`

``` purescript
newtype VolumeStatusEvent
  = VolumeStatusEvent { "Description" :: NullOrUndefined (String), "EventId" :: NullOrUndefined (String), "EventType" :: NullOrUndefined (String), "NotAfter" :: NullOrUndefined (DateTime), "NotBefore" :: NullOrUndefined (DateTime) }
```

<p>Describes a volume status event.</p>

#### `VolumeStatusEventsList`

``` purescript
newtype VolumeStatusEventsList
  = VolumeStatusEventsList (Array VolumeStatusEvent)
```

#### `VolumeStatusInfo`

``` purescript
newtype VolumeStatusInfo
  = VolumeStatusInfo { "Details" :: NullOrUndefined (VolumeStatusDetailsList), "Status" :: NullOrUndefined (VolumeStatusInfoStatus) }
```

<p>Describes the status of a volume.</p>

#### `VolumeStatusInfoStatus`

``` purescript
newtype VolumeStatusInfoStatus
  = VolumeStatusInfoStatus String
```

#### `VolumeStatusItem`

``` purescript
newtype VolumeStatusItem
  = VolumeStatusItem { "Actions" :: NullOrUndefined (VolumeStatusActionsList), "AvailabilityZone" :: NullOrUndefined (String), "Events" :: NullOrUndefined (VolumeStatusEventsList), "VolumeId" :: NullOrUndefined (String), "VolumeStatus" :: NullOrUndefined (VolumeStatusInfo) }
```

<p>Describes the volume status.</p>

#### `VolumeStatusList`

``` purescript
newtype VolumeStatusList
  = VolumeStatusList (Array VolumeStatusItem)
```

#### `VolumeStatusName`

``` purescript
newtype VolumeStatusName
  = VolumeStatusName String
```

#### `VolumeType`

``` purescript
newtype VolumeType
  = VolumeType String
```

#### `Vpc`

``` purescript
newtype Vpc
  = Vpc { "CidrBlock" :: NullOrUndefined (String), "DhcpOptionsId" :: NullOrUndefined (String), "State" :: NullOrUndefined (VpcState), "VpcId" :: NullOrUndefined (String), "InstanceTenancy" :: NullOrUndefined (Tenancy), "Ipv6CidrBlockAssociationSet" :: NullOrUndefined (VpcIpv6CidrBlockAssociationSet), "CidrBlockAssociationSet" :: NullOrUndefined (VpcCidrBlockAssociationSet), "IsDefault" :: NullOrUndefined (Boolean), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a VPC.</p>

#### `VpcAttachment`

``` purescript
newtype VpcAttachment
  = VpcAttachment { "State" :: NullOrUndefined (AttachmentStatus), "VpcId" :: NullOrUndefined (String) }
```

<p>Describes an attachment between a virtual private gateway and a VPC.</p>

#### `VpcAttachmentList`

``` purescript
newtype VpcAttachmentList
  = VpcAttachmentList (Array VpcAttachment)
```

#### `VpcAttributeName`

``` purescript
newtype VpcAttributeName
  = VpcAttributeName String
```

#### `VpcCidrBlockAssociation`

``` purescript
newtype VpcCidrBlockAssociation
  = VpcCidrBlockAssociation { "AssociationId" :: NullOrUndefined (String), "CidrBlock" :: NullOrUndefined (String), "CidrBlockState" :: NullOrUndefined (VpcCidrBlockState) }
```

<p>Describes an IPv4 CIDR block associated with a VPC.</p>

#### `VpcCidrBlockAssociationSet`

``` purescript
newtype VpcCidrBlockAssociationSet
  = VpcCidrBlockAssociationSet (Array VpcCidrBlockAssociation)
```

#### `VpcCidrBlockState`

``` purescript
newtype VpcCidrBlockState
  = VpcCidrBlockState { "State" :: NullOrUndefined (VpcCidrBlockStateCode), "StatusMessage" :: NullOrUndefined (String) }
```

<p>Describes the state of a CIDR block.</p>

#### `VpcCidrBlockStateCode`

``` purescript
newtype VpcCidrBlockStateCode
  = VpcCidrBlockStateCode String
```

#### `VpcClassicLink`

``` purescript
newtype VpcClassicLink
  = VpcClassicLink { "ClassicLinkEnabled" :: NullOrUndefined (Boolean), "Tags" :: NullOrUndefined (TagList), "VpcId" :: NullOrUndefined (String) }
```

<p>Describes whether a VPC is enabled for ClassicLink.</p>

#### `VpcClassicLinkIdList`

``` purescript
newtype VpcClassicLinkIdList
  = VpcClassicLinkIdList (Array String)
```

#### `VpcClassicLinkList`

``` purescript
newtype VpcClassicLinkList
  = VpcClassicLinkList (Array VpcClassicLink)
```

#### `VpcEndpoint`

``` purescript
newtype VpcEndpoint
  = VpcEndpoint { "VpcEndpointId" :: NullOrUndefined (String), "VpcEndpointType" :: NullOrUndefined (VpcEndpointType), "VpcId" :: NullOrUndefined (String), "ServiceName" :: NullOrUndefined (String), "State" :: NullOrUndefined (State), "PolicyDocument" :: NullOrUndefined (String), "RouteTableIds" :: NullOrUndefined (ValueStringList), "SubnetIds" :: NullOrUndefined (ValueStringList), "Groups" :: NullOrUndefined (GroupIdentifierSet), "PrivateDnsEnabled" :: NullOrUndefined (Boolean), "NetworkInterfaceIds" :: NullOrUndefined (ValueStringList), "DnsEntries" :: NullOrUndefined (DnsEntrySet), "CreationTimestamp" :: NullOrUndefined (DateTime) }
```

<p>Describes a VPC endpoint.</p>

#### `VpcEndpointConnection`

``` purescript
newtype VpcEndpointConnection
  = VpcEndpointConnection { "ServiceId" :: NullOrUndefined (String), "VpcEndpointId" :: NullOrUndefined (String), "VpcEndpointOwner" :: NullOrUndefined (String), "VpcEndpointState" :: NullOrUndefined (State), "CreationTimestamp" :: NullOrUndefined (DateTime) }
```

<p>Describes a VPC endpoint connection to a service.</p>

#### `VpcEndpointConnectionSet`

``` purescript
newtype VpcEndpointConnectionSet
  = VpcEndpointConnectionSet (Array VpcEndpointConnection)
```

#### `VpcEndpointSet`

``` purescript
newtype VpcEndpointSet
  = VpcEndpointSet (Array VpcEndpoint)
```

#### `VpcEndpointType`

``` purescript
newtype VpcEndpointType
  = VpcEndpointType String
```

#### `VpcIdStringList`

``` purescript
newtype VpcIdStringList
  = VpcIdStringList (Array String)
```

#### `VpcIpv6CidrBlockAssociation`

``` purescript
newtype VpcIpv6CidrBlockAssociation
  = VpcIpv6CidrBlockAssociation { "AssociationId" :: NullOrUndefined (String), "Ipv6CidrBlock" :: NullOrUndefined (String), "Ipv6CidrBlockState" :: NullOrUndefined (VpcCidrBlockState) }
```

<p>Describes an IPv6 CIDR block associated with a VPC.</p>

#### `VpcIpv6CidrBlockAssociationSet`

``` purescript
newtype VpcIpv6CidrBlockAssociationSet
  = VpcIpv6CidrBlockAssociationSet (Array VpcIpv6CidrBlockAssociation)
```

#### `VpcList`

``` purescript
newtype VpcList
  = VpcList (Array Vpc)
```

#### `VpcPeeringConnection`

``` purescript
newtype VpcPeeringConnection
  = VpcPeeringConnection { "AccepterVpcInfo" :: NullOrUndefined (VpcPeeringConnectionVpcInfo), "ExpirationTime" :: NullOrUndefined (DateTime), "RequesterVpcInfo" :: NullOrUndefined (VpcPeeringConnectionVpcInfo), "Status" :: NullOrUndefined (VpcPeeringConnectionStateReason), "Tags" :: NullOrUndefined (TagList), "VpcPeeringConnectionId" :: NullOrUndefined (String) }
```

<p>Describes a VPC peering connection.</p>

#### `VpcPeeringConnectionList`

``` purescript
newtype VpcPeeringConnectionList
  = VpcPeeringConnectionList (Array VpcPeeringConnection)
```

#### `VpcPeeringConnectionOptionsDescription`

``` purescript
newtype VpcPeeringConnectionOptionsDescription
  = VpcPeeringConnectionOptionsDescription { "AllowDnsResolutionFromRemoteVpc" :: NullOrUndefined (Boolean), "AllowEgressFromLocalClassicLinkToRemoteVpc" :: NullOrUndefined (Boolean), "AllowEgressFromLocalVpcToRemoteClassicLink" :: NullOrUndefined (Boolean) }
```

<p>Describes the VPC peering connection options.</p>

#### `VpcPeeringConnectionStateReason`

``` purescript
newtype VpcPeeringConnectionStateReason
  = VpcPeeringConnectionStateReason { "Code" :: NullOrUndefined (VpcPeeringConnectionStateReasonCode), "Message" :: NullOrUndefined (String) }
```

<p>Describes the status of a VPC peering connection.</p>

#### `VpcPeeringConnectionStateReasonCode`

``` purescript
newtype VpcPeeringConnectionStateReasonCode
  = VpcPeeringConnectionStateReasonCode String
```

#### `VpcPeeringConnectionVpcInfo`

``` purescript
newtype VpcPeeringConnectionVpcInfo
  = VpcPeeringConnectionVpcInfo { "CidrBlock" :: NullOrUndefined (String), "Ipv6CidrBlockSet" :: NullOrUndefined (Ipv6CidrBlockSet), "CidrBlockSet" :: NullOrUndefined (CidrBlockSet), "OwnerId" :: NullOrUndefined (String), "PeeringOptions" :: NullOrUndefined (VpcPeeringConnectionOptionsDescription), "VpcId" :: NullOrUndefined (String), "Region" :: NullOrUndefined (String) }
```

<p>Describes a VPC in a VPC peering connection.</p>

#### `VpcState`

``` purescript
newtype VpcState
  = VpcState String
```

#### `VpcTenancy`

``` purescript
newtype VpcTenancy
  = VpcTenancy String
```

#### `VpnConnection`

``` purescript
newtype VpnConnection
  = VpnConnection { "CustomerGatewayConfiguration" :: NullOrUndefined (String), "CustomerGatewayId" :: NullOrUndefined (String), "Category" :: NullOrUndefined (String), "State" :: NullOrUndefined (VpnState), "Type" :: NullOrUndefined (GatewayType), "VpnConnectionId" :: NullOrUndefined (String), "VpnGatewayId" :: NullOrUndefined (String), "Options" :: NullOrUndefined (VpnConnectionOptions), "Routes" :: NullOrUndefined (VpnStaticRouteList), "Tags" :: NullOrUndefined (TagList), "VgwTelemetry" :: NullOrUndefined (VgwTelemetryList) }
```

<p>Describes a VPN connection.</p>

#### `VpnConnectionIdStringList`

``` purescript
newtype VpnConnectionIdStringList
  = VpnConnectionIdStringList (Array String)
```

#### `VpnConnectionList`

``` purescript
newtype VpnConnectionList
  = VpnConnectionList (Array VpnConnection)
```

#### `VpnConnectionOptions`

``` purescript
newtype VpnConnectionOptions
  = VpnConnectionOptions { "StaticRoutesOnly" :: NullOrUndefined (Boolean) }
```

<p>Describes VPN connection options.</p>

#### `VpnConnectionOptionsSpecification`

``` purescript
newtype VpnConnectionOptionsSpecification
  = VpnConnectionOptionsSpecification { "StaticRoutesOnly" :: NullOrUndefined (Boolean), "TunnelOptions" :: NullOrUndefined (TunnelOptionsList) }
```

<p>Describes VPN connection options.</p>

#### `VpnGateway`

``` purescript
newtype VpnGateway
  = VpnGateway { "AvailabilityZone" :: NullOrUndefined (String), "State" :: NullOrUndefined (VpnState), "Type" :: NullOrUndefined (GatewayType), "VpcAttachments" :: NullOrUndefined (VpcAttachmentList), "VpnGatewayId" :: NullOrUndefined (String), "AmazonSideAsn" :: NullOrUndefined (Number), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a virtual private gateway.</p>

#### `VpnGatewayIdStringList`

``` purescript
newtype VpnGatewayIdStringList
  = VpnGatewayIdStringList (Array String)
```

#### `VpnGatewayList`

``` purescript
newtype VpnGatewayList
  = VpnGatewayList (Array VpnGateway)
```

#### `VpnState`

``` purescript
newtype VpnState
  = VpnState String
```

#### `VpnStaticRoute`

``` purescript
newtype VpnStaticRoute
  = VpnStaticRoute { "DestinationCidrBlock" :: NullOrUndefined (String), "Source" :: NullOrUndefined (VpnStaticRouteSource), "State" :: NullOrUndefined (VpnState) }
```

<p>Describes a static route for a VPN connection.</p>

#### `VpnStaticRouteList`

``` purescript
newtype VpnStaticRouteList
  = VpnStaticRouteList (Array VpnStaticRoute)
```

#### `VpnStaticRouteSource`

``` purescript
newtype VpnStaticRouteSource
  = VpnStaticRouteSource String
```

#### `VpnTunnelOptionsSpecification`

``` purescript
newtype VpnTunnelOptionsSpecification
  = VpnTunnelOptionsSpecification { "TunnelInsideCidr" :: NullOrUndefined (String), "PreSharedKey" :: NullOrUndefined (String) }
```

<p>The tunnel options for a VPN connection.</p>

#### `ZoneNameStringList`

``` purescript
newtype ZoneNameStringList
  = ZoneNameStringList (Array String)
```

#### `Scope'`

``` purescript
newtype Scope'
  = Scope' String
```


