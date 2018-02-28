

-- | <fullname>Amazon Elastic File System</fullname> <p>Amazon Elastic File System (Amazon EFS) provides simple, scalable file storage for use with Amazon EC2 instances in the AWS Cloud. With Amazon EFS, storage capacity is elastic, growing and shrinking automatically as you add and remove files, so your applications have the storage they need, when they need it. For more information, see the <a href="http://docs.aws.amazon.com/efs/latest/ug/api-reference.html">User Guide</a>.</p>
module AWS.EFS where

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

serviceName = "EFS" :: String


-- | <p>Creates a new, empty file system. The operation requires a creation token in the request that Amazon EFS uses to ensure idempotent creation (calling the operation with same creation token has no effect). If a file system does not currently exist that is owned by the caller's AWS account with the specified creation token, this operation does the following:</p> <ul> <li> <p>Creates a new, empty file system. The file system will have an Amazon EFS assigned ID, and an initial lifecycle state <code>creating</code>.</p> </li> <li> <p>Returns with the description of the created file system.</p> </li> </ul> <p>Otherwise, this operation returns a <code>FileSystemAlreadyExists</code> error with the ID of the existing file system.</p> <note> <p>For basic use cases, you can use a randomly generated UUID for the creation token.</p> </note> <p> The idempotent operation allows you to retry a <code>CreateFileSystem</code> call without risk of creating an extra file system. This can happen when an initial call fails in a way that leaves it uncertain whether or not a file system was actually created. An example might be that a transport level timeout occurred or your connection was reset. As long as you use the same creation token, if the initial call had succeeded in creating a file system, the client can learn of its existence from the <code>FileSystemAlreadyExists</code> error.</p> <note> <p>The <code>CreateFileSystem</code> call returns while the file system's lifecycle state is still <code>creating</code>. You can check the file system creation status by calling the <a>DescribeFileSystems</a> operation, which among other things returns the file system state.</p> </note> <p>This operation also takes an optional <code>PerformanceMode</code> parameter that you choose for your file system. We recommend <code>generalPurpose</code> performance mode for most file systems. File systems using the <code>maxIO</code> performance mode can scale to higher levels of aggregate throughput and operations per second with a tradeoff of slightly higher latencies for most file operations. The performance mode can't be changed after the file system has been created. For more information, see <a href="http://docs.aws.amazon.com/efs/latest/ug/performance.html#performancemodes.html">Amazon EFS: Performance Modes</a>.</p> <p>After the file system is fully created, Amazon EFS sets its lifecycle state to <code>available</code>, at which point you can create one or more mount targets for the file system in your VPC. For more information, see <a>CreateMountTarget</a>. You mount your Amazon EFS file system on an EC2 instances in your VPC via the mount target. For more information, see <a href="http://docs.aws.amazon.com/efs/latest/ug/how-it-works.html">Amazon EFS: How it Works</a>. </p> <p> This operation requires permissions for the <code>elasticfilesystem:CreateFileSystem</code> action. </p>
createFileSystem :: forall eff. CreateFileSystemRequest -> Aff (exception :: EXCEPTION | eff) FileSystemDescription
createFileSystem = Request.request serviceName "createFileSystem" 


-- | <p>Creates a mount target for a file system. You can then mount the file system on EC2 instances via the mount target.</p> <p>You can create one mount target in each Availability Zone in your VPC. All EC2 instances in a VPC within a given Availability Zone share a single mount target for a given file system. If you have multiple subnets in an Availability Zone, you create a mount target in one of the subnets. EC2 instances do not need to be in the same subnet as the mount target in order to access their file system. For more information, see <a href="http://docs.aws.amazon.com/efs/latest/ug/how-it-works.html">Amazon EFS: How it Works</a>. </p> <p>In the request, you also specify a file system ID for which you are creating the mount target and the file system's lifecycle state must be <code>available</code>. For more information, see <a>DescribeFileSystems</a>.</p> <p>In the request, you also provide a subnet ID, which determines the following:</p> <ul> <li> <p>VPC in which Amazon EFS creates the mount target</p> </li> <li> <p>Availability Zone in which Amazon EFS creates the mount target</p> </li> <li> <p>IP address range from which Amazon EFS selects the IP address of the mount target (if you don't specify an IP address in the request)</p> </li> </ul> <p>After creating the mount target, Amazon EFS returns a response that includes, a <code>MountTargetId</code> and an <code>IpAddress</code>. You use this IP address when mounting the file system in an EC2 instance. You can also use the mount target's DNS name when mounting the file system. The EC2 instance on which you mount the file system via the mount target can resolve the mount target's DNS name to its IP address. For more information, see <a href="http://docs.aws.amazon.com/efs/latest/ug/how-it-works.html#how-it-works-implementation">How it Works: Implementation Overview</a>. </p> <p>Note that you can create mount targets for a file system in only one VPC, and there can be only one mount target per Availability Zone. That is, if the file system already has one or more mount targets created for it, the subnet specified in the request to add another mount target must meet the following requirements:</p> <ul> <li> <p>Must belong to the same VPC as the subnets of the existing mount targets</p> </li> <li> <p>Must not be in the same Availability Zone as any of the subnets of the existing mount targets</p> </li> </ul> <p>If the request satisfies the requirements, Amazon EFS does the following:</p> <ul> <li> <p>Creates a new mount target in the specified subnet.</p> </li> <li> <p>Also creates a new network interface in the subnet as follows:</p> <ul> <li> <p>If the request provides an <code>IpAddress</code>, Amazon EFS assigns that IP address to the network interface. Otherwise, Amazon EFS assigns a free address in the subnet (in the same way that the Amazon EC2 <code>CreateNetworkInterface</code> call does when a request does not specify a primary private IP address).</p> </li> <li> <p>If the request provides <code>SecurityGroups</code>, this network interface is associated with those security groups. Otherwise, it belongs to the default security group for the subnet's VPC.</p> </li> <li> <p>Assigns the description <code>Mount target <i>fsmt-id</i> for file system <i>fs-id</i> </code> where <code> <i>fsmt-id</i> </code> is the mount target ID, and <code> <i>fs-id</i> </code> is the <code>FileSystemId</code>.</p> </li> <li> <p>Sets the <code>requesterManaged</code> property of the network interface to <code>true</code>, and the <code>requesterId</code> value to <code>EFS</code>.</p> </li> </ul> <p>Each Amazon EFS mount target has one corresponding requester-managed EC2 network interface. After the network interface is created, Amazon EFS sets the <code>NetworkInterfaceId</code> field in the mount target's description to the network interface ID, and the <code>IpAddress</code> field to its address. If network interface creation fails, the entire <code>CreateMountTarget</code> operation fails.</p> </li> </ul> <note> <p>The <code>CreateMountTarget</code> call returns only after creating the network interface, but while the mount target state is still <code>creating</code>, you can check the mount target creation status by calling the <a>DescribeMountTargets</a> operation, which among other things returns the mount target state.</p> </note> <p>We recommend you create a mount target in each of the Availability Zones. There are cost considerations for using a file system in an Availability Zone through a mount target created in another Availability Zone. For more information, see <a href="http://aws.amazon.com/efs/">Amazon EFS</a>. In addition, by always using a mount target local to the instance's Availability Zone, you eliminate a partial failure scenario. If the Availability Zone in which your mount target is created goes down, then you won't be able to access your file system through that mount target. </p> <p>This operation requires permissions for the following action on the file system:</p> <ul> <li> <p> <code>elasticfilesystem:CreateMountTarget</code> </p> </li> </ul> <p>This operation also requires permissions for the following Amazon EC2 actions:</p> <ul> <li> <p> <code>ec2:DescribeSubnets</code> </p> </li> <li> <p> <code>ec2:DescribeNetworkInterfaces</code> </p> </li> <li> <p> <code>ec2:CreateNetworkInterface</code> </p> </li> </ul>
createMountTarget :: forall eff. CreateMountTargetRequest -> Aff (exception :: EXCEPTION | eff) MountTargetDescription
createMountTarget = Request.request serviceName "createMountTarget" 


-- | <p>Creates or overwrites tags associated with a file system. Each tag is a key-value pair. If a tag key specified in the request already exists on the file system, this operation overwrites its value with the value provided in the request. If you add the <code>Name</code> tag to your file system, Amazon EFS returns it in the response to the <a>DescribeFileSystems</a> operation. </p> <p>This operation requires permission for the <code>elasticfilesystem:CreateTags</code> action.</p>
createTags :: forall eff. CreateTagsRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
createTags = Request.request serviceName "createTags" 


-- | <p>Deletes a file system, permanently severing access to its contents. Upon return, the file system no longer exists and you can't access any contents of the deleted file system.</p> <p> You can't delete a file system that is in use. That is, if the file system has any mount targets, you must first delete them. For more information, see <a>DescribeMountTargets</a> and <a>DeleteMountTarget</a>. </p> <note> <p>The <code>DeleteFileSystem</code> call returns while the file system state is still <code>deleting</code>. You can check the file system deletion status by calling the <a>DescribeFileSystems</a> operation, which returns a list of file systems in your account. If you pass file system ID or creation token for the deleted file system, the <a>DescribeFileSystems</a> returns a <code>404 FileSystemNotFound</code> error.</p> </note> <p>This operation requires permissions for the <code>elasticfilesystem:DeleteFileSystem</code> action.</p>
deleteFileSystem :: forall eff. DeleteFileSystemRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteFileSystem = Request.request serviceName "deleteFileSystem" 


-- | <p>Deletes the specified mount target.</p> <p>This operation forcibly breaks any mounts of the file system via the mount target that is being deleted, which might disrupt instances or applications using those mounts. To avoid applications getting cut off abruptly, you might consider unmounting any mounts of the mount target, if feasible. The operation also deletes the associated network interface. Uncommitted writes may be lost, but breaking a mount target using this operation does not corrupt the file system itself. The file system you created remains. You can mount an EC2 instance in your VPC via another mount target.</p> <p>This operation requires permissions for the following action on the file system:</p> <ul> <li> <p> <code>elasticfilesystem:DeleteMountTarget</code> </p> </li> </ul> <note> <p>The <code>DeleteMountTarget</code> call returns while the mount target state is still <code>deleting</code>. You can check the mount target deletion by calling the <a>DescribeMountTargets</a> operation, which returns a list of mount target descriptions for the given file system. </p> </note> <p>The operation also requires permissions for the following Amazon EC2 action on the mount target's network interface:</p> <ul> <li> <p> <code>ec2:DeleteNetworkInterface</code> </p> </li> </ul>
deleteMountTarget :: forall eff. DeleteMountTargetRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteMountTarget = Request.request serviceName "deleteMountTarget" 


-- | <p>Deletes the specified tags from a file system. If the <code>DeleteTags</code> request includes a tag key that does not exist, Amazon EFS ignores it and doesn't cause an error. For more information about tags and related restrictions, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Tag Restrictions</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p> <p>This operation requires permissions for the <code>elasticfilesystem:DeleteTags</code> action.</p>
deleteTags :: forall eff. DeleteTagsRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteTags = Request.request serviceName "deleteTags" 


-- | <p>Returns the description of a specific Amazon EFS file system if either the file system <code>CreationToken</code> or the <code>FileSystemId</code> is provided. Otherwise, it returns descriptions of all file systems owned by the caller's AWS account in the AWS Region of the endpoint that you're calling.</p> <p> When retrieving all file system descriptions, you can optionally specify the <code>MaxItems</code> parameter to limit the number of descriptions in a response. If more file system descriptions remain, Amazon EFS returns a <code>NextMarker</code>, an opaque token, in the response. In this case, you should send a subsequent request with the <code>Marker</code> request parameter set to the value of <code>NextMarker</code>. </p> <p>To retrieve a list of your file system descriptions, this operation is used in an iterative process, where <code>DescribeFileSystems</code> is called first without the <code>Marker</code> and then the operation continues to call it with the <code>Marker</code> parameter set to the value of the <code>NextMarker</code> from the previous response until the response has no <code>NextMarker</code>. </p> <p>The implementation may return fewer than <code>MaxItems</code> file system descriptions while still including a <code>NextMarker</code> value. </p> <p> The order of file systems returned in the response of one <code>DescribeFileSystems</code> call and the order of file systems returned across the responses of a multi-call iteration is unspecified. </p> <p> This operation requires permissions for the <code>elasticfilesystem:DescribeFileSystems</code> action. </p>
describeFileSystems :: forall eff. DescribeFileSystemsRequest -> Aff (exception :: EXCEPTION | eff) DescribeFileSystemsResponse
describeFileSystems = Request.request serviceName "describeFileSystems" 


-- | <p>Returns the security groups currently in effect for a mount target. This operation requires that the network interface of the mount target has been created and the lifecycle state of the mount target is not <code>deleted</code>.</p> <p>This operation requires permissions for the following actions:</p> <ul> <li> <p> <code>elasticfilesystem:DescribeMountTargetSecurityGroups</code> action on the mount target's file system. </p> </li> <li> <p> <code>ec2:DescribeNetworkInterfaceAttribute</code> action on the mount target's network interface. </p> </li> </ul>
describeMountTargetSecurityGroups :: forall eff. DescribeMountTargetSecurityGroupsRequest -> Aff (exception :: EXCEPTION | eff) DescribeMountTargetSecurityGroupsResponse
describeMountTargetSecurityGroups = Request.request serviceName "describeMountTargetSecurityGroups" 


-- | <p>Returns the descriptions of all the current mount targets, or a specific mount target, for a file system. When requesting all of the current mount targets, the order of mount targets returned in the response is unspecified.</p> <p>This operation requires permissions for the <code>elasticfilesystem:DescribeMountTargets</code> action, on either the file system ID that you specify in <code>FileSystemId</code>, or on the file system of the mount target that you specify in <code>MountTargetId</code>.</p>
describeMountTargets :: forall eff. DescribeMountTargetsRequest -> Aff (exception :: EXCEPTION | eff) DescribeMountTargetsResponse
describeMountTargets = Request.request serviceName "describeMountTargets" 


-- | <p>Returns the tags associated with a file system. The order of tags returned in the response of one <code>DescribeTags</code> call and the order of tags returned across the responses of a multi-call iteration (when using pagination) is unspecified. </p> <p> This operation requires permissions for the <code>elasticfilesystem:DescribeTags</code> action. </p>
describeTags :: forall eff. DescribeTagsRequest -> Aff (exception :: EXCEPTION | eff) DescribeTagsResponse
describeTags = Request.request serviceName "describeTags" 


-- | <p>Modifies the set of security groups in effect for a mount target.</p> <p>When you create a mount target, Amazon EFS also creates a new network interface. For more information, see <a>CreateMountTarget</a>. This operation replaces the security groups in effect for the network interface associated with a mount target, with the <code>SecurityGroups</code> provided in the request. This operation requires that the network interface of the mount target has been created and the lifecycle state of the mount target is not <code>deleted</code>. </p> <p>The operation requires permissions for the following actions:</p> <ul> <li> <p> <code>elasticfilesystem:ModifyMountTargetSecurityGroups</code> action on the mount target's file system. </p> </li> <li> <p> <code>ec2:ModifyNetworkInterfaceAttribute</code> action on the mount target's network interface. </p> </li> </ul>
modifyMountTargetSecurityGroups :: forall eff. ModifyMountTargetSecurityGroupsRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
modifyMountTargetSecurityGroups = Request.request serviceName "modifyMountTargetSecurityGroups" 


newtype AwsAccountId = AwsAccountId String
derive instance newtypeAwsAccountId :: Newtype AwsAccountId _
derive instance repGenericAwsAccountId :: Generic AwsAccountId _
instance showAwsAccountId :: Show AwsAccountId where
  show = genericShow
instance decodeAwsAccountId :: Decode AwsAccountId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsAccountId :: Encode AwsAccountId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if the request is malformed or contains an error such as an invalid parameter value or a missing required parameter.</p>
newtype BadRequest = BadRequest 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBadRequest :: Newtype BadRequest _
derive instance repGenericBadRequest :: Generic BadRequest _
instance showBadRequest :: Show BadRequest where
  show = genericShow
instance decodeBadRequest :: Decode BadRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequest :: Encode BadRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateFileSystemRequest = CreateFileSystemRequest 
  { "CreationToken" :: (CreationToken)
  , "PerformanceMode" :: NullOrUndefined.NullOrUndefined (PerformanceMode)
  , "Encrypted" :: NullOrUndefined.NullOrUndefined (Encrypted)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (KmsKeyId)
  }
derive instance newtypeCreateFileSystemRequest :: Newtype CreateFileSystemRequest _
derive instance repGenericCreateFileSystemRequest :: Generic CreateFileSystemRequest _
instance showCreateFileSystemRequest :: Show CreateFileSystemRequest where
  show = genericShow
instance decodeCreateFileSystemRequest :: Decode CreateFileSystemRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateFileSystemRequest :: Encode CreateFileSystemRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateMountTargetRequest = CreateMountTargetRequest 
  { "FileSystemId" :: (FileSystemId)
  , "SubnetId" :: (SubnetId)
  , "IpAddress" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "SecurityGroups" :: NullOrUndefined.NullOrUndefined (SecurityGroups)
  }
derive instance newtypeCreateMountTargetRequest :: Newtype CreateMountTargetRequest _
derive instance repGenericCreateMountTargetRequest :: Generic CreateMountTargetRequest _
instance showCreateMountTargetRequest :: Show CreateMountTargetRequest where
  show = genericShow
instance decodeCreateMountTargetRequest :: Decode CreateMountTargetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMountTargetRequest :: Encode CreateMountTargetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateTagsRequest = CreateTagsRequest 
  { "FileSystemId" :: (FileSystemId)
  , "Tags" :: (Tags)
  }
derive instance newtypeCreateTagsRequest :: Newtype CreateTagsRequest _
derive instance repGenericCreateTagsRequest :: Generic CreateTagsRequest _
instance showCreateTagsRequest :: Show CreateTagsRequest where
  show = genericShow
instance decodeCreateTagsRequest :: Decode CreateTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTagsRequest :: Encode CreateTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreationToken = CreationToken String
derive instance newtypeCreationToken :: Newtype CreationToken _
derive instance repGenericCreationToken :: Generic CreationToken _
instance showCreationToken :: Show CreationToken where
  show = genericShow
instance decodeCreationToken :: Decode CreationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreationToken :: Encode CreationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteFileSystemRequest = DeleteFileSystemRequest 
  { "FileSystemId" :: (FileSystemId)
  }
derive instance newtypeDeleteFileSystemRequest :: Newtype DeleteFileSystemRequest _
derive instance repGenericDeleteFileSystemRequest :: Generic DeleteFileSystemRequest _
instance showDeleteFileSystemRequest :: Show DeleteFileSystemRequest where
  show = genericShow
instance decodeDeleteFileSystemRequest :: Decode DeleteFileSystemRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteFileSystemRequest :: Encode DeleteFileSystemRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteMountTargetRequest = DeleteMountTargetRequest 
  { "MountTargetId" :: (MountTargetId)
  }
derive instance newtypeDeleteMountTargetRequest :: Newtype DeleteMountTargetRequest _
derive instance repGenericDeleteMountTargetRequest :: Generic DeleteMountTargetRequest _
instance showDeleteMountTargetRequest :: Show DeleteMountTargetRequest where
  show = genericShow
instance decodeDeleteMountTargetRequest :: Decode DeleteMountTargetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMountTargetRequest :: Encode DeleteMountTargetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteTagsRequest = DeleteTagsRequest 
  { "FileSystemId" :: (FileSystemId)
  , "TagKeys" :: (TagKeys)
  }
derive instance newtypeDeleteTagsRequest :: Newtype DeleteTagsRequest _
derive instance repGenericDeleteTagsRequest :: Generic DeleteTagsRequest _
instance showDeleteTagsRequest :: Show DeleteTagsRequest where
  show = genericShow
instance decodeDeleteTagsRequest :: Decode DeleteTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTagsRequest :: Encode DeleteTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The service timed out trying to fulfill the request, and the client should try the call again.</p>
newtype DependencyTimeout = DependencyTimeout 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDependencyTimeout :: Newtype DependencyTimeout _
derive instance repGenericDependencyTimeout :: Generic DependencyTimeout _
instance showDependencyTimeout :: Show DependencyTimeout where
  show = genericShow
instance decodeDependencyTimeout :: Decode DependencyTimeout where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDependencyTimeout :: Encode DependencyTimeout where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeFileSystemsRequest = DescribeFileSystemsRequest 
  { "MaxItems" :: NullOrUndefined.NullOrUndefined (MaxItems)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "CreationToken" :: NullOrUndefined.NullOrUndefined (CreationToken)
  , "FileSystemId" :: NullOrUndefined.NullOrUndefined (FileSystemId)
  }
derive instance newtypeDescribeFileSystemsRequest :: Newtype DescribeFileSystemsRequest _
derive instance repGenericDescribeFileSystemsRequest :: Generic DescribeFileSystemsRequest _
instance showDescribeFileSystemsRequest :: Show DescribeFileSystemsRequest where
  show = genericShow
instance decodeDescribeFileSystemsRequest :: Decode DescribeFileSystemsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFileSystemsRequest :: Encode DescribeFileSystemsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeFileSystemsResponse = DescribeFileSystemsResponse 
  { "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "FileSystems" :: NullOrUndefined.NullOrUndefined (FileSystemDescriptions)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeFileSystemsResponse :: Newtype DescribeFileSystemsResponse _
derive instance repGenericDescribeFileSystemsResponse :: Generic DescribeFileSystemsResponse _
instance showDescribeFileSystemsResponse :: Show DescribeFileSystemsResponse where
  show = genericShow
instance decodeDescribeFileSystemsResponse :: Decode DescribeFileSystemsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFileSystemsResponse :: Encode DescribeFileSystemsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeMountTargetSecurityGroupsRequest = DescribeMountTargetSecurityGroupsRequest 
  { "MountTargetId" :: (MountTargetId)
  }
derive instance newtypeDescribeMountTargetSecurityGroupsRequest :: Newtype DescribeMountTargetSecurityGroupsRequest _
derive instance repGenericDescribeMountTargetSecurityGroupsRequest :: Generic DescribeMountTargetSecurityGroupsRequest _
instance showDescribeMountTargetSecurityGroupsRequest :: Show DescribeMountTargetSecurityGroupsRequest where
  show = genericShow
instance decodeDescribeMountTargetSecurityGroupsRequest :: Decode DescribeMountTargetSecurityGroupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMountTargetSecurityGroupsRequest :: Encode DescribeMountTargetSecurityGroupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMountTargetSecurityGroupsResponse = DescribeMountTargetSecurityGroupsResponse 
  { "SecurityGroups" :: (SecurityGroups)
  }
derive instance newtypeDescribeMountTargetSecurityGroupsResponse :: Newtype DescribeMountTargetSecurityGroupsResponse _
derive instance repGenericDescribeMountTargetSecurityGroupsResponse :: Generic DescribeMountTargetSecurityGroupsResponse _
instance showDescribeMountTargetSecurityGroupsResponse :: Show DescribeMountTargetSecurityGroupsResponse where
  show = genericShow
instance decodeDescribeMountTargetSecurityGroupsResponse :: Decode DescribeMountTargetSecurityGroupsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMountTargetSecurityGroupsResponse :: Encode DescribeMountTargetSecurityGroupsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeMountTargetsRequest = DescribeMountTargetsRequest 
  { "MaxItems" :: NullOrUndefined.NullOrUndefined (MaxItems)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "FileSystemId" :: NullOrUndefined.NullOrUndefined (FileSystemId)
  , "MountTargetId" :: NullOrUndefined.NullOrUndefined (MountTargetId)
  }
derive instance newtypeDescribeMountTargetsRequest :: Newtype DescribeMountTargetsRequest _
derive instance repGenericDescribeMountTargetsRequest :: Generic DescribeMountTargetsRequest _
instance showDescribeMountTargetsRequest :: Show DescribeMountTargetsRequest where
  show = genericShow
instance decodeDescribeMountTargetsRequest :: Decode DescribeMountTargetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMountTargetsRequest :: Encode DescribeMountTargetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeMountTargetsResponse = DescribeMountTargetsResponse 
  { "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "MountTargets" :: NullOrUndefined.NullOrUndefined (MountTargetDescriptions)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeMountTargetsResponse :: Newtype DescribeMountTargetsResponse _
derive instance repGenericDescribeMountTargetsResponse :: Generic DescribeMountTargetsResponse _
instance showDescribeMountTargetsResponse :: Show DescribeMountTargetsResponse where
  show = genericShow
instance decodeDescribeMountTargetsResponse :: Decode DescribeMountTargetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMountTargetsResponse :: Encode DescribeMountTargetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeTagsRequest = DescribeTagsRequest 
  { "MaxItems" :: NullOrUndefined.NullOrUndefined (MaxItems)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "FileSystemId" :: (FileSystemId)
  }
derive instance newtypeDescribeTagsRequest :: Newtype DescribeTagsRequest _
derive instance repGenericDescribeTagsRequest :: Generic DescribeTagsRequest _
instance showDescribeTagsRequest :: Show DescribeTagsRequest where
  show = genericShow
instance decodeDescribeTagsRequest :: Decode DescribeTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsRequest :: Encode DescribeTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeTagsResponse = DescribeTagsResponse 
  { "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "Tags" :: (Tags)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeTagsResponse :: Newtype DescribeTagsResponse _
derive instance repGenericDescribeTagsResponse :: Generic DescribeTagsResponse _
instance showDescribeTagsResponse :: Show DescribeTagsResponse where
  show = genericShow
instance decodeDescribeTagsResponse :: Decode DescribeTagsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsResponse :: Encode DescribeTagsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Encrypted = Encrypted Boolean
derive instance newtypeEncrypted :: Newtype Encrypted _
derive instance repGenericEncrypted :: Generic Encrypted _
instance showEncrypted :: Show Encrypted where
  show = genericShow
instance decodeEncrypted :: Decode Encrypted where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncrypted :: Encode Encrypted where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _
derive instance repGenericErrorCode :: Generic ErrorCode _
instance showErrorCode :: Show ErrorCode where
  show = genericShow
instance decodeErrorCode :: Decode ErrorCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorCode :: Encode ErrorCode where
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


-- | <p>Returned if the file system you are trying to create already exists, with the creation token you provided.</p>
newtype FileSystemAlreadyExists = FileSystemAlreadyExists 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  , "FileSystemId" :: (FileSystemId)
  }
derive instance newtypeFileSystemAlreadyExists :: Newtype FileSystemAlreadyExists _
derive instance repGenericFileSystemAlreadyExists :: Generic FileSystemAlreadyExists _
instance showFileSystemAlreadyExists :: Show FileSystemAlreadyExists where
  show = genericShow
instance decodeFileSystemAlreadyExists :: Decode FileSystemAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileSystemAlreadyExists :: Encode FileSystemAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Description of the file system.</p>
newtype FileSystemDescription = FileSystemDescription 
  { "OwnerId" :: (AwsAccountId)
  , "CreationToken" :: (CreationToken)
  , "FileSystemId" :: (FileSystemId)
  , "CreationTime" :: (Number)
  , "LifeCycleState" :: (LifeCycleState)
  , "Name" :: NullOrUndefined.NullOrUndefined (TagValue)
  , "NumberOfMountTargets" :: (MountTargetCount)
  , "SizeInBytes" :: (FileSystemSize)
  , "PerformanceMode" :: (PerformanceMode)
  , "Encrypted" :: NullOrUndefined.NullOrUndefined (Encrypted)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (KmsKeyId)
  }
derive instance newtypeFileSystemDescription :: Newtype FileSystemDescription _
derive instance repGenericFileSystemDescription :: Generic FileSystemDescription _
instance showFileSystemDescription :: Show FileSystemDescription where
  show = genericShow
instance decodeFileSystemDescription :: Decode FileSystemDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileSystemDescription :: Encode FileSystemDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FileSystemDescriptions = FileSystemDescriptions (Array FileSystemDescription)
derive instance newtypeFileSystemDescriptions :: Newtype FileSystemDescriptions _
derive instance repGenericFileSystemDescriptions :: Generic FileSystemDescriptions _
instance showFileSystemDescriptions :: Show FileSystemDescriptions where
  show = genericShow
instance decodeFileSystemDescriptions :: Decode FileSystemDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileSystemDescriptions :: Encode FileSystemDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FileSystemId = FileSystemId String
derive instance newtypeFileSystemId :: Newtype FileSystemId _
derive instance repGenericFileSystemId :: Generic FileSystemId _
instance showFileSystemId :: Show FileSystemId where
  show = genericShow
instance decodeFileSystemId :: Decode FileSystemId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileSystemId :: Encode FileSystemId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if a file system has mount targets.</p>
newtype FileSystemInUse = FileSystemInUse 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeFileSystemInUse :: Newtype FileSystemInUse _
derive instance repGenericFileSystemInUse :: Generic FileSystemInUse _
instance showFileSystemInUse :: Show FileSystemInUse where
  show = genericShow
instance decodeFileSystemInUse :: Decode FileSystemInUse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileSystemInUse :: Encode FileSystemInUse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if the AWS account has already created maximum number of file systems allowed per account.</p>
newtype FileSystemLimitExceeded = FileSystemLimitExceeded 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeFileSystemLimitExceeded :: Newtype FileSystemLimitExceeded _
derive instance repGenericFileSystemLimitExceeded :: Generic FileSystemLimitExceeded _
instance showFileSystemLimitExceeded :: Show FileSystemLimitExceeded where
  show = genericShow
instance decodeFileSystemLimitExceeded :: Decode FileSystemLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileSystemLimitExceeded :: Encode FileSystemLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if the specified <code>FileSystemId</code> does not exist in the requester's AWS account.</p>
newtype FileSystemNotFound = FileSystemNotFound 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeFileSystemNotFound :: Newtype FileSystemNotFound _
derive instance repGenericFileSystemNotFound :: Generic FileSystemNotFound _
instance showFileSystemNotFound :: Show FileSystemNotFound where
  show = genericShow
instance decodeFileSystemNotFound :: Decode FileSystemNotFound where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileSystemNotFound :: Encode FileSystemNotFound where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Latest known metered size (in bytes) of data stored in the file system, in its <code>Value</code> field, and the time at which that size was determined in its <code>Timestamp</code> field. Note that the value does not represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, the value will represent the actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not necessarily the exact size the file system was at any instant in time.</p>
newtype FileSystemSize = FileSystemSize 
  { "Value" :: (FileSystemSizeValue)
  , "Number" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeFileSystemSize :: Newtype FileSystemSize _
derive instance repGenericFileSystemSize :: Generic FileSystemSize _
instance showFileSystemSize :: Show FileSystemSize where
  show = genericShow
instance decodeFileSystemSize :: Decode FileSystemSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileSystemSize :: Encode FileSystemSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FileSystemSizeValue = FileSystemSizeValue Number
derive instance newtypeFileSystemSizeValue :: Newtype FileSystemSizeValue _
derive instance repGenericFileSystemSizeValue :: Generic FileSystemSizeValue _
instance showFileSystemSizeValue :: Show FileSystemSizeValue where
  show = genericShow
instance decodeFileSystemSizeValue :: Decode FileSystemSizeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileSystemSizeValue :: Encode FileSystemSizeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if the file system's life cycle state is not "created".</p>
newtype IncorrectFileSystemLifeCycleState = IncorrectFileSystemLifeCycleState 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeIncorrectFileSystemLifeCycleState :: Newtype IncorrectFileSystemLifeCycleState _
derive instance repGenericIncorrectFileSystemLifeCycleState :: Generic IncorrectFileSystemLifeCycleState _
instance showIncorrectFileSystemLifeCycleState :: Show IncorrectFileSystemLifeCycleState where
  show = genericShow
instance decodeIncorrectFileSystemLifeCycleState :: Decode IncorrectFileSystemLifeCycleState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncorrectFileSystemLifeCycleState :: Encode IncorrectFileSystemLifeCycleState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if the mount target is not in the correct state for the operation.</p>
newtype IncorrectMountTargetState = IncorrectMountTargetState 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeIncorrectMountTargetState :: Newtype IncorrectMountTargetState _
derive instance repGenericIncorrectMountTargetState :: Generic IncorrectMountTargetState _
instance showIncorrectMountTargetState :: Show IncorrectMountTargetState where
  show = genericShow
instance decodeIncorrectMountTargetState :: Decode IncorrectMountTargetState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncorrectMountTargetState :: Encode IncorrectMountTargetState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if an error occurred on the server side.</p>
newtype InternalServerError = InternalServerError 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _
derive instance repGenericInternalServerError :: Generic InternalServerError _
instance showInternalServerError :: Show InternalServerError where
  show = genericShow
instance decodeInternalServerError :: Decode InternalServerError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerError :: Encode InternalServerError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _
derive instance repGenericIpAddress :: Generic IpAddress _
instance showIpAddress :: Show IpAddress where
  show = genericShow
instance decodeIpAddress :: Decode IpAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpAddress :: Encode IpAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if the request specified an <code>IpAddress</code> that is already in use in the subnet.</p>
newtype IpAddressInUse = IpAddressInUse 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeIpAddressInUse :: Newtype IpAddressInUse _
derive instance repGenericIpAddressInUse :: Generic IpAddressInUse _
instance showIpAddressInUse :: Show IpAddressInUse where
  show = genericShow
instance decodeIpAddressInUse :: Decode IpAddressInUse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpAddressInUse :: Encode IpAddressInUse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KmsKeyId = KmsKeyId String
derive instance newtypeKmsKeyId :: Newtype KmsKeyId _
derive instance repGenericKmsKeyId :: Generic KmsKeyId _
instance showKmsKeyId :: Show KmsKeyId where
  show = genericShow
instance decodeKmsKeyId :: Decode KmsKeyId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKmsKeyId :: Encode KmsKeyId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LifeCycleState = LifeCycleState String
derive instance newtypeLifeCycleState :: Newtype LifeCycleState _
derive instance repGenericLifeCycleState :: Generic LifeCycleState _
instance showLifeCycleState :: Show LifeCycleState where
  show = genericShow
instance decodeLifeCycleState :: Decode LifeCycleState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLifeCycleState :: Encode LifeCycleState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Marker = Marker String
derive instance newtypeMarker :: Newtype Marker _
derive instance repGenericMarker :: Generic Marker _
instance showMarker :: Show Marker where
  show = genericShow
instance decodeMarker :: Decode Marker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarker :: Encode Marker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxItems = MaxItems Int
derive instance newtypeMaxItems :: Newtype MaxItems _
derive instance repGenericMaxItems :: Generic MaxItems _
instance showMaxItems :: Show MaxItems where
  show = genericShow
instance decodeMaxItems :: Decode MaxItems where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxItems :: Encode MaxItems where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ModifyMountTargetSecurityGroupsRequest = ModifyMountTargetSecurityGroupsRequest 
  { "MountTargetId" :: (MountTargetId)
  , "SecurityGroups" :: NullOrUndefined.NullOrUndefined (SecurityGroups)
  }
derive instance newtypeModifyMountTargetSecurityGroupsRequest :: Newtype ModifyMountTargetSecurityGroupsRequest _
derive instance repGenericModifyMountTargetSecurityGroupsRequest :: Generic ModifyMountTargetSecurityGroupsRequest _
instance showModifyMountTargetSecurityGroupsRequest :: Show ModifyMountTargetSecurityGroupsRequest where
  show = genericShow
instance decodeModifyMountTargetSecurityGroupsRequest :: Decode ModifyMountTargetSecurityGroupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyMountTargetSecurityGroupsRequest :: Encode ModifyMountTargetSecurityGroupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if the mount target would violate one of the specified restrictions based on the file system's existing mount targets.</p>
newtype MountTargetConflict = MountTargetConflict 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMountTargetConflict :: Newtype MountTargetConflict _
derive instance repGenericMountTargetConflict :: Generic MountTargetConflict _
instance showMountTargetConflict :: Show MountTargetConflict where
  show = genericShow
instance decodeMountTargetConflict :: Decode MountTargetConflict where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMountTargetConflict :: Encode MountTargetConflict where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MountTargetCount = MountTargetCount Int
derive instance newtypeMountTargetCount :: Newtype MountTargetCount _
derive instance repGenericMountTargetCount :: Generic MountTargetCount _
instance showMountTargetCount :: Show MountTargetCount where
  show = genericShow
instance decodeMountTargetCount :: Decode MountTargetCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMountTargetCount :: Encode MountTargetCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides a description of a mount target.</p>
newtype MountTargetDescription = MountTargetDescription 
  { "OwnerId" :: NullOrUndefined.NullOrUndefined (AwsAccountId)
  , "MountTargetId" :: (MountTargetId)
  , "FileSystemId" :: (FileSystemId)
  , "SubnetId" :: (SubnetId)
  , "LifeCycleState" :: (LifeCycleState)
  , "IpAddress" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "NetworkInterfaceId" :: NullOrUndefined.NullOrUndefined (NetworkInterfaceId)
  }
derive instance newtypeMountTargetDescription :: Newtype MountTargetDescription _
derive instance repGenericMountTargetDescription :: Generic MountTargetDescription _
instance showMountTargetDescription :: Show MountTargetDescription where
  show = genericShow
instance decodeMountTargetDescription :: Decode MountTargetDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMountTargetDescription :: Encode MountTargetDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MountTargetDescriptions = MountTargetDescriptions (Array MountTargetDescription)
derive instance newtypeMountTargetDescriptions :: Newtype MountTargetDescriptions _
derive instance repGenericMountTargetDescriptions :: Generic MountTargetDescriptions _
instance showMountTargetDescriptions :: Show MountTargetDescriptions where
  show = genericShow
instance decodeMountTargetDescriptions :: Decode MountTargetDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMountTargetDescriptions :: Encode MountTargetDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MountTargetId = MountTargetId String
derive instance newtypeMountTargetId :: Newtype MountTargetId _
derive instance repGenericMountTargetId :: Generic MountTargetId _
instance showMountTargetId :: Show MountTargetId where
  show = genericShow
instance decodeMountTargetId :: Decode MountTargetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMountTargetId :: Encode MountTargetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if there is no mount target with the specified ID found in the caller's account.</p>
newtype MountTargetNotFound = MountTargetNotFound 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMountTargetNotFound :: Newtype MountTargetNotFound _
derive instance repGenericMountTargetNotFound :: Generic MountTargetNotFound _
instance showMountTargetNotFound :: Show MountTargetNotFound where
  show = genericShow
instance decodeMountTargetNotFound :: Decode MountTargetNotFound where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMountTargetNotFound :: Encode MountTargetNotFound where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NetworkInterfaceId = NetworkInterfaceId String
derive instance newtypeNetworkInterfaceId :: Newtype NetworkInterfaceId _
derive instance repGenericNetworkInterfaceId :: Generic NetworkInterfaceId _
instance showNetworkInterfaceId :: Show NetworkInterfaceId where
  show = genericShow
instance decodeNetworkInterfaceId :: Decode NetworkInterfaceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkInterfaceId :: Encode NetworkInterfaceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The calling account has reached the ENI limit for the specific AWS region. Client should try to delete some ENIs or get its account limit raised. For more information, see <a href="http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Appendix_Limits.html">Amazon VPC Limits</a> in the Amazon Virtual Private Cloud User Guide (see the Network interfaces per VPC entry in the table). </p>
newtype NetworkInterfaceLimitExceeded = NetworkInterfaceLimitExceeded 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNetworkInterfaceLimitExceeded :: Newtype NetworkInterfaceLimitExceeded _
derive instance repGenericNetworkInterfaceLimitExceeded :: Generic NetworkInterfaceLimitExceeded _
instance showNetworkInterfaceLimitExceeded :: Show NetworkInterfaceLimitExceeded where
  show = genericShow
instance decodeNetworkInterfaceLimitExceeded :: Decode NetworkInterfaceLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkInterfaceLimitExceeded :: Encode NetworkInterfaceLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if <code>IpAddress</code> was not specified in the request and there are no free IP addresses in the subnet.</p>
newtype NoFreeAddressesInSubnet = NoFreeAddressesInSubnet 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoFreeAddressesInSubnet :: Newtype NoFreeAddressesInSubnet _
derive instance repGenericNoFreeAddressesInSubnet :: Generic NoFreeAddressesInSubnet _
instance showNoFreeAddressesInSubnet :: Show NoFreeAddressesInSubnet where
  show = genericShow
instance decodeNoFreeAddressesInSubnet :: Decode NoFreeAddressesInSubnet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoFreeAddressesInSubnet :: Encode NoFreeAddressesInSubnet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PerformanceMode = PerformanceMode String
derive instance newtypePerformanceMode :: Newtype PerformanceMode _
derive instance repGenericPerformanceMode :: Generic PerformanceMode _
instance showPerformanceMode :: Show PerformanceMode where
  show = genericShow
instance decodePerformanceMode :: Decode PerformanceMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePerformanceMode :: Encode PerformanceMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityGroup = SecurityGroup String
derive instance newtypeSecurityGroup :: Newtype SecurityGroup _
derive instance repGenericSecurityGroup :: Generic SecurityGroup _
instance showSecurityGroup :: Show SecurityGroup where
  show = genericShow
instance decodeSecurityGroup :: Decode SecurityGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroup :: Encode SecurityGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if the size of <code>SecurityGroups</code> specified in the request is greater than five.</p>
newtype SecurityGroupLimitExceeded = SecurityGroupLimitExceeded 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeSecurityGroupLimitExceeded :: Newtype SecurityGroupLimitExceeded _
derive instance repGenericSecurityGroupLimitExceeded :: Generic SecurityGroupLimitExceeded _
instance showSecurityGroupLimitExceeded :: Show SecurityGroupLimitExceeded where
  show = genericShow
instance decodeSecurityGroupLimitExceeded :: Decode SecurityGroupLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroupLimitExceeded :: Encode SecurityGroupLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if one of the specified security groups does not exist in the subnet's VPC.</p>
newtype SecurityGroupNotFound = SecurityGroupNotFound 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeSecurityGroupNotFound :: Newtype SecurityGroupNotFound _
derive instance repGenericSecurityGroupNotFound :: Generic SecurityGroupNotFound _
instance showSecurityGroupNotFound :: Show SecurityGroupNotFound where
  show = genericShow
instance decodeSecurityGroupNotFound :: Decode SecurityGroupNotFound where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroupNotFound :: Encode SecurityGroupNotFound where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityGroups = SecurityGroups (Array SecurityGroup)
derive instance newtypeSecurityGroups :: Newtype SecurityGroups _
derive instance repGenericSecurityGroups :: Generic SecurityGroups _
instance showSecurityGroups :: Show SecurityGroups where
  show = genericShow
instance decodeSecurityGroups :: Decode SecurityGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroups :: Encode SecurityGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetId = SubnetId String
derive instance newtypeSubnetId :: Newtype SubnetId _
derive instance repGenericSubnetId :: Generic SubnetId _
instance showSubnetId :: Show SubnetId where
  show = genericShow
instance decodeSubnetId :: Decode SubnetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetId :: Encode SubnetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if there is no subnet with ID <code>SubnetId</code> provided in the request.</p>
newtype SubnetNotFound = SubnetNotFound 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeSubnetNotFound :: Newtype SubnetNotFound _
derive instance repGenericSubnetNotFound :: Generic SubnetNotFound _
instance showSubnetNotFound :: Show SubnetNotFound where
  show = genericShow
instance decodeSubnetNotFound :: Decode SubnetNotFound where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetNotFound :: Encode SubnetNotFound where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A tag is a key-value pair. Allowed characters: letters, whitespace, and numbers, representable in UTF-8, and the following characters:<code> + - = . _ : /</code> </p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
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


newtype TagKeys = TagKeys (Array TagKey)
derive instance newtypeTagKeys :: Newtype TagKeys _
derive instance repGenericTagKeys :: Generic TagKeys _
instance showTagKeys :: Show TagKeys where
  show = genericShow
instance decodeTagKeys :: Decode TagKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeys :: Encode TagKeys where
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


newtype Tags = Tags (Array Tag)
derive instance newtypeTags :: Newtype Tags _
derive instance repGenericTags :: Generic Tags _
instance showTags :: Show Tags where
  show = genericShow
instance decodeTags :: Decode Tags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTags :: Encode Tags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype UnsupportedAvailabilityZone = UnsupportedAvailabilityZone 
  { "ErrorCode" :: (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnsupportedAvailabilityZone :: Newtype UnsupportedAvailabilityZone _
derive instance repGenericUnsupportedAvailabilityZone :: Generic UnsupportedAvailabilityZone _
instance showUnsupportedAvailabilityZone :: Show UnsupportedAvailabilityZone where
  show = genericShow
instance decodeUnsupportedAvailabilityZone :: Decode UnsupportedAvailabilityZone where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedAvailabilityZone :: Encode UnsupportedAvailabilityZone where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
