

-- | <fullname>Amazon Elastic File System</fullname> <p>Amazon Elastic File System (Amazon EFS) provides simple, scalable file storage for use with Amazon EC2 instances in the AWS Cloud. With Amazon EFS, storage capacity is elastic, growing and shrinking automatically as you add and remove files, so your applications have the storage they need, when they need it. For more information, see the <a href="http://docs.aws.amazon.com/efs/latest/ug/api-reference.html">User Guide</a>.</p>
module AWS.EFS where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "EFS" :: String


-- | <p>Creates a new, empty file system. The operation requires a creation token in the request that Amazon EFS uses to ensure idempotent creation (calling the operation with same creation token has no effect). If a file system does not currently exist that is owned by the caller's AWS account with the specified creation token, this operation does the following:</p> <ul> <li> <p>Creates a new, empty file system. The file system will have an Amazon EFS assigned ID, and an initial lifecycle state <code>creating</code>.</p> </li> <li> <p>Returns with the description of the created file system.</p> </li> </ul> <p>Otherwise, this operation returns a <code>FileSystemAlreadyExists</code> error with the ID of the existing file system.</p> <note> <p>For basic use cases, you can use a randomly generated UUID for the creation token.</p> </note> <p> The idempotent operation allows you to retry a <code>CreateFileSystem</code> call without risk of creating an extra file system. This can happen when an initial call fails in a way that leaves it uncertain whether or not a file system was actually created. An example might be that a transport level timeout occurred or your connection was reset. As long as you use the same creation token, if the initial call had succeeded in creating a file system, the client can learn of its existence from the <code>FileSystemAlreadyExists</code> error.</p> <note> <p>The <code>CreateFileSystem</code> call returns while the file system's lifecycle state is still <code>creating</code>. You can check the file system creation status by calling the <a>DescribeFileSystems</a> operation, which among other things returns the file system state.</p> </note> <p>This operation also takes an optional <code>PerformanceMode</code> parameter that you choose for your file system. We recommend <code>generalPurpose</code> performance mode for most file systems. File systems using the <code>maxIO</code> performance mode can scale to higher levels of aggregate throughput and operations per second with a tradeoff of slightly higher latencies for most file operations. The performance mode can't be changed after the file system has been created. For more information, see <a href="http://docs.aws.amazon.com/efs/latest/ug/performance.html#performancemodes.html">Amazon EFS: Performance Modes</a>.</p> <p>After the file system is fully created, Amazon EFS sets its lifecycle state to <code>available</code>, at which point you can create one or more mount targets for the file system in your VPC. For more information, see <a>CreateMountTarget</a>. You mount your Amazon EFS file system on an EC2 instances in your VPC via the mount target. For more information, see <a href="http://docs.aws.amazon.com/efs/latest/ug/how-it-works.html">Amazon EFS: How it Works</a>. </p> <p> This operation requires permissions for the <code>elasticfilesystem:CreateFileSystem</code> action. </p>
createFileSystem :: forall eff. CreateFileSystemRequest -> Aff (err :: AWS.RequestError | eff) FileSystemDescription
createFileSystem = AWS.request serviceName "CreateFileSystem" 


-- | <p>Creates a mount target for a file system. You can then mount the file system on EC2 instances via the mount target.</p> <p>You can create one mount target in each Availability Zone in your VPC. All EC2 instances in a VPC within a given Availability Zone share a single mount target for a given file system. If you have multiple subnets in an Availability Zone, you create a mount target in one of the subnets. EC2 instances do not need to be in the same subnet as the mount target in order to access their file system. For more information, see <a href="http://docs.aws.amazon.com/efs/latest/ug/how-it-works.html">Amazon EFS: How it Works</a>. </p> <p>In the request, you also specify a file system ID for which you are creating the mount target and the file system's lifecycle state must be <code>available</code>. For more information, see <a>DescribeFileSystems</a>.</p> <p>In the request, you also provide a subnet ID, which determines the following:</p> <ul> <li> <p>VPC in which Amazon EFS creates the mount target</p> </li> <li> <p>Availability Zone in which Amazon EFS creates the mount target</p> </li> <li> <p>IP address range from which Amazon EFS selects the IP address of the mount target (if you don't specify an IP address in the request)</p> </li> </ul> <p>After creating the mount target, Amazon EFS returns a response that includes, a <code>MountTargetId</code> and an <code>IpAddress</code>. You use this IP address when mounting the file system in an EC2 instance. You can also use the mount target's DNS name when mounting the file system. The EC2 instance on which you mount the file system via the mount target can resolve the mount target's DNS name to its IP address. For more information, see <a href="http://docs.aws.amazon.com/efs/latest/ug/how-it-works.html#how-it-works-implementation">How it Works: Implementation Overview</a>. </p> <p>Note that you can create mount targets for a file system in only one VPC, and there can be only one mount target per Availability Zone. That is, if the file system already has one or more mount targets created for it, the subnet specified in the request to add another mount target must meet the following requirements:</p> <ul> <li> <p>Must belong to the same VPC as the subnets of the existing mount targets</p> </li> <li> <p>Must not be in the same Availability Zone as any of the subnets of the existing mount targets</p> </li> </ul> <p>If the request satisfies the requirements, Amazon EFS does the following:</p> <ul> <li> <p>Creates a new mount target in the specified subnet.</p> </li> <li> <p>Also creates a new network interface in the subnet as follows:</p> <ul> <li> <p>If the request provides an <code>IpAddress</code>, Amazon EFS assigns that IP address to the network interface. Otherwise, Amazon EFS assigns a free address in the subnet (in the same way that the Amazon EC2 <code>CreateNetworkInterface</code> call does when a request does not specify a primary private IP address).</p> </li> <li> <p>If the request provides <code>SecurityGroups</code>, this network interface is associated with those security groups. Otherwise, it belongs to the default security group for the subnet's VPC.</p> </li> <li> <p>Assigns the description <code>Mount target <i>fsmt-id</i> for file system <i>fs-id</i> </code> where <code> <i>fsmt-id</i> </code> is the mount target ID, and <code> <i>fs-id</i> </code> is the <code>FileSystemId</code>.</p> </li> <li> <p>Sets the <code>requesterManaged</code> property of the network interface to <code>true</code>, and the <code>requesterId</code> value to <code>EFS</code>.</p> </li> </ul> <p>Each Amazon EFS mount target has one corresponding requester-managed EC2 network interface. After the network interface is created, Amazon EFS sets the <code>NetworkInterfaceId</code> field in the mount target's description to the network interface ID, and the <code>IpAddress</code> field to its address. If network interface creation fails, the entire <code>CreateMountTarget</code> operation fails.</p> </li> </ul> <note> <p>The <code>CreateMountTarget</code> call returns only after creating the network interface, but while the mount target state is still <code>creating</code>, you can check the mount target creation status by calling the <a>DescribeMountTargets</a> operation, which among other things returns the mount target state.</p> </note> <p>We recommend you create a mount target in each of the Availability Zones. There are cost considerations for using a file system in an Availability Zone through a mount target created in another Availability Zone. For more information, see <a href="http://aws.amazon.com/efs/">Amazon EFS</a>. In addition, by always using a mount target local to the instance's Availability Zone, you eliminate a partial failure scenario. If the Availability Zone in which your mount target is created goes down, then you won't be able to access your file system through that mount target. </p> <p>This operation requires permissions for the following action on the file system:</p> <ul> <li> <p> <code>elasticfilesystem:CreateMountTarget</code> </p> </li> </ul> <p>This operation also requires permissions for the following Amazon EC2 actions:</p> <ul> <li> <p> <code>ec2:DescribeSubnets</code> </p> </li> <li> <p> <code>ec2:DescribeNetworkInterfaces</code> </p> </li> <li> <p> <code>ec2:CreateNetworkInterface</code> </p> </li> </ul>
createMountTarget :: forall eff. CreateMountTargetRequest -> Aff (err :: AWS.RequestError | eff) MountTargetDescription
createMountTarget = AWS.request serviceName "CreateMountTarget" 


-- | <p>Creates or overwrites tags associated with a file system. Each tag is a key-value pair. If a tag key specified in the request already exists on the file system, this operation overwrites its value with the value provided in the request. If you add the <code>Name</code> tag to your file system, Amazon EFS returns it in the response to the <a>DescribeFileSystems</a> operation. </p> <p>This operation requires permission for the <code>elasticfilesystem:CreateTags</code> action.</p>
createTags :: forall eff. CreateTagsRequest -> Aff (err :: AWS.RequestError | eff) Unit
createTags = AWS.request serviceName "CreateTags" 


-- | <p>Deletes a file system, permanently severing access to its contents. Upon return, the file system no longer exists and you can't access any contents of the deleted file system.</p> <p> You can't delete a file system that is in use. That is, if the file system has any mount targets, you must first delete them. For more information, see <a>DescribeMountTargets</a> and <a>DeleteMountTarget</a>. </p> <note> <p>The <code>DeleteFileSystem</code> call returns while the file system state is still <code>deleting</code>. You can check the file system deletion status by calling the <a>DescribeFileSystems</a> operation, which returns a list of file systems in your account. If you pass file system ID or creation token for the deleted file system, the <a>DescribeFileSystems</a> returns a <code>404 FileSystemNotFound</code> error.</p> </note> <p>This operation requires permissions for the <code>elasticfilesystem:DeleteFileSystem</code> action.</p>
deleteFileSystem :: forall eff. DeleteFileSystemRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteFileSystem = AWS.request serviceName "DeleteFileSystem" 


-- | <p>Deletes the specified mount target.</p> <p>This operation forcibly breaks any mounts of the file system via the mount target that is being deleted, which might disrupt instances or applications using those mounts. To avoid applications getting cut off abruptly, you might consider unmounting any mounts of the mount target, if feasible. The operation also deletes the associated network interface. Uncommitted writes may be lost, but breaking a mount target using this operation does not corrupt the file system itself. The file system you created remains. You can mount an EC2 instance in your VPC via another mount target.</p> <p>This operation requires permissions for the following action on the file system:</p> <ul> <li> <p> <code>elasticfilesystem:DeleteMountTarget</code> </p> </li> </ul> <note> <p>The <code>DeleteMountTarget</code> call returns while the mount target state is still <code>deleting</code>. You can check the mount target deletion by calling the <a>DescribeMountTargets</a> operation, which returns a list of mount target descriptions for the given file system. </p> </note> <p>The operation also requires permissions for the following Amazon EC2 action on the mount target's network interface:</p> <ul> <li> <p> <code>ec2:DeleteNetworkInterface</code> </p> </li> </ul>
deleteMountTarget :: forall eff. DeleteMountTargetRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteMountTarget = AWS.request serviceName "DeleteMountTarget" 


-- | <p>Deletes the specified tags from a file system. If the <code>DeleteTags</code> request includes a tag key that does not exist, Amazon EFS ignores it and doesn't cause an error. For more information about tags and related restrictions, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Tag Restrictions</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p> <p>This operation requires permissions for the <code>elasticfilesystem:DeleteTags</code> action.</p>
deleteTags :: forall eff. DeleteTagsRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteTags = AWS.request serviceName "DeleteTags" 


-- | <p>Returns the description of a specific Amazon EFS file system if either the file system <code>CreationToken</code> or the <code>FileSystemId</code> is provided. Otherwise, it returns descriptions of all file systems owned by the caller's AWS account in the AWS Region of the endpoint that you're calling.</p> <p> When retrieving all file system descriptions, you can optionally specify the <code>MaxItems</code> parameter to limit the number of descriptions in a response. If more file system descriptions remain, Amazon EFS returns a <code>NextMarker</code>, an opaque token, in the response. In this case, you should send a subsequent request with the <code>Marker</code> request parameter set to the value of <code>NextMarker</code>. </p> <p>To retrieve a list of your file system descriptions, this operation is used in an iterative process, where <code>DescribeFileSystems</code> is called first without the <code>Marker</code> and then the operation continues to call it with the <code>Marker</code> parameter set to the value of the <code>NextMarker</code> from the previous response until the response has no <code>NextMarker</code>. </p> <p>The implementation may return fewer than <code>MaxItems</code> file system descriptions while still including a <code>NextMarker</code> value. </p> <p> The order of file systems returned in the response of one <code>DescribeFileSystems</code> call and the order of file systems returned across the responses of a multi-call iteration is unspecified. </p> <p> This operation requires permissions for the <code>elasticfilesystem:DescribeFileSystems</code> action. </p>
describeFileSystems :: forall eff. DescribeFileSystemsRequest -> Aff (err :: AWS.RequestError | eff) DescribeFileSystemsResponse
describeFileSystems = AWS.request serviceName "DescribeFileSystems" 


-- | <p>Returns the security groups currently in effect for a mount target. This operation requires that the network interface of the mount target has been created and the lifecycle state of the mount target is not <code>deleted</code>.</p> <p>This operation requires permissions for the following actions:</p> <ul> <li> <p> <code>elasticfilesystem:DescribeMountTargetSecurityGroups</code> action on the mount target's file system. </p> </li> <li> <p> <code>ec2:DescribeNetworkInterfaceAttribute</code> action on the mount target's network interface. </p> </li> </ul>
describeMountTargetSecurityGroups :: forall eff. DescribeMountTargetSecurityGroupsRequest -> Aff (err :: AWS.RequestError | eff) DescribeMountTargetSecurityGroupsResponse
describeMountTargetSecurityGroups = AWS.request serviceName "DescribeMountTargetSecurityGroups" 


-- | <p>Returns the descriptions of all the current mount targets, or a specific mount target, for a file system. When requesting all of the current mount targets, the order of mount targets returned in the response is unspecified.</p> <p>This operation requires permissions for the <code>elasticfilesystem:DescribeMountTargets</code> action, on either the file system ID that you specify in <code>FileSystemId</code>, or on the file system of the mount target that you specify in <code>MountTargetId</code>.</p>
describeMountTargets :: forall eff. DescribeMountTargetsRequest -> Aff (err :: AWS.RequestError | eff) DescribeMountTargetsResponse
describeMountTargets = AWS.request serviceName "DescribeMountTargets" 


-- | <p>Returns the tags associated with a file system. The order of tags returned in the response of one <code>DescribeTags</code> call and the order of tags returned across the responses of a multi-call iteration (when using pagination) is unspecified. </p> <p> This operation requires permissions for the <code>elasticfilesystem:DescribeTags</code> action. </p>
describeTags :: forall eff. DescribeTagsRequest -> Aff (err :: AWS.RequestError | eff) DescribeTagsResponse
describeTags = AWS.request serviceName "DescribeTags" 


-- | <p>Modifies the set of security groups in effect for a mount target.</p> <p>When you create a mount target, Amazon EFS also creates a new network interface. For more information, see <a>CreateMountTarget</a>. This operation replaces the security groups in effect for the network interface associated with a mount target, with the <code>SecurityGroups</code> provided in the request. This operation requires that the network interface of the mount target has been created and the lifecycle state of the mount target is not <code>deleted</code>. </p> <p>The operation requires permissions for the following actions:</p> <ul> <li> <p> <code>elasticfilesystem:ModifyMountTargetSecurityGroups</code> action on the mount target's file system. </p> </li> <li> <p> <code>ec2:ModifyNetworkInterfaceAttribute</code> action on the mount target's network interface. </p> </li> </ul>
modifyMountTargetSecurityGroups :: forall eff. ModifyMountTargetSecurityGroupsRequest -> Aff (err :: AWS.RequestError | eff) Unit
modifyMountTargetSecurityGroups = AWS.request serviceName "ModifyMountTargetSecurityGroups" 


newtype AwsAccountId = AwsAccountId String


-- | <p>Returned if the request is malformed or contains an error such as an invalid parameter value or a missing required parameter.</p>
newtype BadRequest = BadRequest 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype CreateFileSystemRequest = CreateFileSystemRequest 
  { "CreationToken" :: (CreationToken)
  , "PerformanceMode" :: NullOrUndefined (PerformanceMode)
  , "Encrypted" :: NullOrUndefined (Encrypted)
  , "KmsKeyId" :: NullOrUndefined (KmsKeyId)
  }


-- | <p/>
newtype CreateMountTargetRequest = CreateMountTargetRequest 
  { "FileSystemId" :: (FileSystemId)
  , "SubnetId" :: (SubnetId)
  , "IpAddress" :: NullOrUndefined (IpAddress)
  , "SecurityGroups" :: NullOrUndefined (SecurityGroups)
  }


-- | <p/>
newtype CreateTagsRequest = CreateTagsRequest 
  { "FileSystemId" :: (FileSystemId)
  , "Tags" :: (Tags)
  }


newtype CreationToken = CreationToken String


-- | <p/>
newtype DeleteFileSystemRequest = DeleteFileSystemRequest 
  { "FileSystemId" :: (FileSystemId)
  }


-- | <p/>
newtype DeleteMountTargetRequest = DeleteMountTargetRequest 
  { "MountTargetId" :: (MountTargetId)
  }


-- | <p/>
newtype DeleteTagsRequest = DeleteTagsRequest 
  { "FileSystemId" :: (FileSystemId)
  , "TagKeys" :: (TagKeys)
  }


-- | <p>The service timed out trying to fulfill the request, and the client should try the call again.</p>
newtype DependencyTimeout = DependencyTimeout 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p/>
newtype DescribeFileSystemsRequest = DescribeFileSystemsRequest 
  { "MaxItems" :: NullOrUndefined (MaxItems)
  , "Marker" :: NullOrUndefined (Marker)
  , "CreationToken" :: NullOrUndefined (CreationToken)
  , "FileSystemId" :: NullOrUndefined (FileSystemId)
  }


newtype DescribeFileSystemsResponse = DescribeFileSystemsResponse 
  { "Marker" :: NullOrUndefined (Marker)
  , "FileSystems" :: NullOrUndefined (FileSystemDescriptions)
  , "NextMarker" :: NullOrUndefined (Marker)
  }


-- | <p/>
newtype DescribeMountTargetSecurityGroupsRequest = DescribeMountTargetSecurityGroupsRequest 
  { "MountTargetId" :: (MountTargetId)
  }


newtype DescribeMountTargetSecurityGroupsResponse = DescribeMountTargetSecurityGroupsResponse 
  { "SecurityGroups" :: (SecurityGroups)
  }


-- | <p/>
newtype DescribeMountTargetsRequest = DescribeMountTargetsRequest 
  { "MaxItems" :: NullOrUndefined (MaxItems)
  , "Marker" :: NullOrUndefined (Marker)
  , "FileSystemId" :: NullOrUndefined (FileSystemId)
  , "MountTargetId" :: NullOrUndefined (MountTargetId)
  }


-- | <p/>
newtype DescribeMountTargetsResponse = DescribeMountTargetsResponse 
  { "Marker" :: NullOrUndefined (Marker)
  , "MountTargets" :: NullOrUndefined (MountTargetDescriptions)
  , "NextMarker" :: NullOrUndefined (Marker)
  }


-- | <p/>
newtype DescribeTagsRequest = DescribeTagsRequest 
  { "MaxItems" :: NullOrUndefined (MaxItems)
  , "Marker" :: NullOrUndefined (Marker)
  , "FileSystemId" :: (FileSystemId)
  }


-- | <p/>
newtype DescribeTagsResponse = DescribeTagsResponse 
  { "Marker" :: NullOrUndefined (Marker)
  , "Tags" :: (Tags)
  , "NextMarker" :: NullOrUndefined (Marker)
  }


newtype Encrypted = Encrypted Boolean


newtype ErrorCode = ErrorCode String


newtype ErrorMessage = ErrorMessage String


-- | <p>Returned if the file system you are trying to create already exists, with the creation token you provided.</p>
newtype FileSystemAlreadyExists = FileSystemAlreadyExists 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  , "FileSystemId" :: (FileSystemId)
  }


-- | <p>Description of the file system.</p>
newtype FileSystemDescription = FileSystemDescription 
  { "OwnerId" :: (AwsAccountId)
  , "CreationToken" :: (CreationToken)
  , "FileSystemId" :: (FileSystemId)
  , "CreationTime" :: (Number)
  , "LifeCycleState" :: (LifeCycleState)
  , "Name" :: NullOrUndefined (TagValue)
  , "NumberOfMountTargets" :: (MountTargetCount)
  , "SizeInBytes" :: (FileSystemSize)
  , "PerformanceMode" :: (PerformanceMode)
  , "Encrypted" :: NullOrUndefined (Encrypted)
  , "KmsKeyId" :: NullOrUndefined (KmsKeyId)
  }


newtype FileSystemDescriptions = FileSystemDescriptions (Array FileSystemDescription)


newtype FileSystemId = FileSystemId String


-- | <p>Returned if a file system has mount targets.</p>
newtype FileSystemInUse = FileSystemInUse 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Returned if the AWS account has already created maximum number of file systems allowed per account.</p>
newtype FileSystemLimitExceeded = FileSystemLimitExceeded 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Returned if the specified <code>FileSystemId</code> does not exist in the requester's AWS account.</p>
newtype FileSystemNotFound = FileSystemNotFound 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Latest known metered size (in bytes) of data stored in the file system, in its <code>Value</code> field, and the time at which that size was determined in its <code>Timestamp</code> field. Note that the value does not represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, the value will represent the actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not necessarily the exact size the file system was at any instant in time.</p>
newtype FileSystemSize = FileSystemSize 
  { "Value" :: (FileSystemSizeValue)
  , "Number" :: NullOrUndefined (Number)
  }


newtype FileSystemSizeValue = FileSystemSizeValue Number


-- | <p>Returned if the file system's life cycle state is not "created".</p>
newtype IncorrectFileSystemLifeCycleState = IncorrectFileSystemLifeCycleState 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Returned if the mount target is not in the correct state for the operation.</p>
newtype IncorrectMountTargetState = IncorrectMountTargetState 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Returned if an error occurred on the server side.</p>
newtype InternalServerError = InternalServerError 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype IpAddress = IpAddress String


-- | <p>Returned if the request specified an <code>IpAddress</code> that is already in use in the subnet.</p>
newtype IpAddressInUse = IpAddressInUse 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype KmsKeyId = KmsKeyId String


newtype LifeCycleState = LifeCycleState String


newtype Marker = Marker String


newtype MaxItems = MaxItems Int


-- | <p/>
newtype ModifyMountTargetSecurityGroupsRequest = ModifyMountTargetSecurityGroupsRequest 
  { "MountTargetId" :: (MountTargetId)
  , "SecurityGroups" :: NullOrUndefined (SecurityGroups)
  }


-- | <p>Returned if the mount target would violate one of the specified restrictions based on the file system's existing mount targets.</p>
newtype MountTargetConflict = MountTargetConflict 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype MountTargetCount = MountTargetCount Int


-- | <p>Provides a description of a mount target.</p>
newtype MountTargetDescription = MountTargetDescription 
  { "OwnerId" :: NullOrUndefined (AwsAccountId)
  , "MountTargetId" :: (MountTargetId)
  , "FileSystemId" :: (FileSystemId)
  , "SubnetId" :: (SubnetId)
  , "LifeCycleState" :: (LifeCycleState)
  , "IpAddress" :: NullOrUndefined (IpAddress)
  , "NetworkInterfaceId" :: NullOrUndefined (NetworkInterfaceId)
  }


newtype MountTargetDescriptions = MountTargetDescriptions (Array MountTargetDescription)


newtype MountTargetId = MountTargetId String


-- | <p>Returned if there is no mount target with the specified ID found in the caller's account.</p>
newtype MountTargetNotFound = MountTargetNotFound 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype NetworkInterfaceId = NetworkInterfaceId String


-- | <p> The calling account has reached the ENI limit for the specific AWS region. Client should try to delete some ENIs or get its account limit raised. For more information, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Appendix_Limits.html">Amazon VPC Limits</a> in the Amazon Virtual Private Cloud User Guide (see the Network interfaces per VPC entry in the table). </p>
newtype NetworkInterfaceLimitExceeded = NetworkInterfaceLimitExceeded 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Returned if <code>IpAddress</code> was not specified in the request and there are no free IP addresses in the subnet.</p>
newtype NoFreeAddressesInSubnet = NoFreeAddressesInSubnet 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype PerformanceMode = PerformanceMode String


newtype SecurityGroup = SecurityGroup String


-- | <p>Returned if the size of <code>SecurityGroups</code> specified in the request is greater than five.</p>
newtype SecurityGroupLimitExceeded = SecurityGroupLimitExceeded 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Returned if one of the specified security groups does not exist in the subnet's VPC.</p>
newtype SecurityGroupNotFound = SecurityGroupNotFound 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype SecurityGroups = SecurityGroups (Array SecurityGroup)


newtype SubnetId = SubnetId String


-- | <p>Returned if there is no subnet with ID <code>SubnetId</code> provided in the request.</p>
newtype SubnetNotFound = SubnetNotFound 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>A tag is a key-value pair. Allowed characters: letters, whitespace, and numbers, representable in UTF-8, and the following characters:<code> + - = . _ : /</code> </p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }


newtype TagKey = TagKey String


newtype TagKeys = TagKeys (Array TagKey)


newtype TagValue = TagValue String


newtype Tags = Tags (Array Tag)


-- | <p/>
newtype UnsupportedAvailabilityZone = UnsupportedAvailabilityZone 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }
