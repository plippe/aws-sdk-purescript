

-- | <fullname>AWS OpsWorks CM</fullname> <p> AWS OpsWorks for configuration management (CM) is a service that runs and manages configuration management servers. </p> <p> <b>Glossary of terms</b> </p> <ul> <li> <p> <b>Server</b>: A configuration management server that can be highly-available. The configuration management server runs on an Amazon Elastic Compute Cloud (EC2) instance, and may use various other AWS services, such as Amazon Relational Database Service (RDS) and Elastic Load Balancing. A server is a generic abstraction over the configuration manager that you want to use, much like Amazon RDS. In AWS OpsWorks CM, you do not start or stop servers. After you create servers, they continue to run until they are deleted.</p> </li> <li> <p> <b>Engine</b>: The engine is the specific configuration manager that you want to use. Valid values in this release include <code>Chef</code> and <code>Puppet</code>.</p> </li> <li> <p> <b>Backup</b>: This is an application-level backup of the data that the configuration manager stores. AWS OpsWorks CM creates an S3 bucket for backups when you launch the first server. A backup maintains a snapshot of a server's configuration-related attributes at the time the backup starts.</p> </li> <li> <p> <b>Events</b>: Events are always related to a server. Events are written during server creation, when health checks run, when backups are created, when system maintenance is performed, etc. When you delete a server, the server's events are also deleted.</p> </li> <li> <p> <b>Account attributes</b>: Every account has attributes that are assigned in the AWS OpsWorks CM database. These attributes store information about configuration limits (servers, backups, etc.) and your customer account. </p> </li> </ul> <p> <b>Endpoints</b> </p> <p>AWS OpsWorks CM supports the following endpoints, all HTTPS. You must connect to one of the following endpoints. Your servers can only be accessed or managed within the endpoint in which they are created.</p> <ul> <li> <p>opsworks-cm.us-east-1.amazonaws.com</p> </li> <li> <p>opsworks-cm.us-west-2.amazonaws.com</p> </li> <li> <p>opsworks-cm.eu-west-1.amazonaws.com</p> </li> </ul> <p> <b>Throttling limits</b> </p> <p>All API operations allow for five requests per second with a burst of 10 requests per second.</p>
module AWS.OpsWorksCM where

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

serviceName = "OpsWorksCM" :: String


-- | <p> Associates a new node with the server. For more information about how to disassociate a node, see <a>DisassociateNode</a>.</p> <p> On a Chef server: This command is an alternative to <code>knife bootstrap</code>.</p> <p> Example (Chef): <code>aws opsworks-cm associate-node --server-name <i>MyServer</i> --node-name <i>MyManagedNode</i> --engine-attributes "Name=<i>CHEF_ORGANIZATION</i>,Value=default" "Name=<i>CHEF_NODE_PUBLIC_KEY</i>,Value=<i>public-key-pem</i>"</code> </p> <p> On a Puppet server, this command is an alternative to the <code>puppet cert sign</code> command that signs a Puppet node CSR. </p> <p> Example (Chef): <code>aws opsworks-cm associate-node --server-name <i>MyServer</i> --node-name <i>MyManagedNode</i> --engine-attributes "Name=<i>PUPPET_NODE_CSR</i>,Value=<i>csr-pem</i>"</code> </p> <p> A node can can only be associated with servers that are in a <code>HEALTHY</code> state. Otherwise, an <code>InvalidStateException</code> is thrown. A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. The AssociateNode API call can be integrated into Auto Scaling configurations, AWS Cloudformation templates, or the user data of a server's instance. </p>
associateNode :: forall eff. AssociateNodeRequest -> Aff (exception :: EXCEPTION | eff) AssociateNodeResponse
associateNode = Request.request serviceName "associateNode" 


-- | <p> Creates an application-level backup of a server. While the server is in the <code>BACKING_UP</code> state, the server cannot be changed, and no additional backup can be created. </p> <p> Backups can be created for servers in <code>RUNNING</code>, <code>HEALTHY</code>, and <code>UNHEALTHY</code> states. By default, you can create a maximum of 50 manual backups. </p> <p> This operation is asynchronous. </p> <p> A <code>LimitExceededException</code> is thrown when the maximum number of manual backups is reached. An <code>InvalidStateException</code> is thrown when the server is not in any of the following states: RUNNING, HEALTHY, or UNHEALTHY. A <code>ResourceNotFoundException</code> is thrown when the server is not found. A <code>ValidationException</code> is thrown when parameters of the request are not valid. </p>
createBackup :: forall eff. CreateBackupRequest -> Aff (exception :: EXCEPTION | eff) CreateBackupResponse
createBackup = Request.request serviceName "createBackup" 


-- | <p> Creates and immedately starts a new server. The server is ready to use when it is in the <code>HEALTHY</code> state. By default, you can create a maximum of 10 servers. </p> <p> This operation is asynchronous. </p> <p> A <code>LimitExceededException</code> is thrown when you have created the maximum number of servers (10). A <code>ResourceAlreadyExistsException</code> is thrown when a server with the same name already exists in the account. A <code>ResourceNotFoundException</code> is thrown when you specify a backup ID that is not valid or is for a backup that does not exist. A <code>ValidationException</code> is thrown when parameters of the request are not valid. </p> <p> If you do not specify a security group by adding the <code>SecurityGroupIds</code> parameter, AWS OpsWorks creates a new security group. </p> <p> <i>Chef Automate:</i> The default security group opens the Chef server to the world on TCP port 443. If a KeyName is present, AWS OpsWorks enables SSH access. SSH is also open to the world on TCP port 22. </p> <p> <i>Puppet Enterprise:</i> The default security group opens TCP ports 22, 443, 4433, 8140, 8142, 8143, and 8170. If a KeyName is present, AWS OpsWorks enables SSH access. SSH is also open to the world on TCP port 22. </p> <p>By default, your server is accessible from any IP address. We recommend that you update your security group rules to allow access from known IP addresses and address ranges only. To edit security group rules, open Security Groups in the navigation pane of the EC2 management console. </p>
createServer :: forall eff. CreateServerRequest -> Aff (exception :: EXCEPTION | eff) CreateServerResponse
createServer = Request.request serviceName "createServer" 


-- | <p> Deletes a backup. You can delete both manual and automated backups. This operation is asynchronous. </p> <p> An <code>InvalidStateException</code> is thrown when a backup deletion is already in progress. A <code>ResourceNotFoundException</code> is thrown when the backup does not exist. A <code>ValidationException</code> is thrown when parameters of the request are not valid. </p>
deleteBackup :: forall eff. DeleteBackupRequest -> Aff (exception :: EXCEPTION | eff) DeleteBackupResponse
deleteBackup = Request.request serviceName "deleteBackup" 


-- | <p> Deletes the server and the underlying AWS CloudFormation stacks (including the server's EC2 instance). When you run this command, the server state is updated to <code>DELETING</code>. After the server is deleted, it is no longer returned by <code>DescribeServer</code> requests. If the AWS CloudFormation stack cannot be deleted, the server cannot be deleted. </p> <p> This operation is asynchronous. </p> <p> An <code>InvalidStateException</code> is thrown when a server deletion is already in progress. A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p> <p> </p>
deleteServer :: forall eff. DeleteServerRequest -> Aff (exception :: EXCEPTION | eff) DeleteServerResponse
deleteServer = Request.request serviceName "deleteServer" 


-- | <p> Describes your account attributes, and creates requests to increase limits before they are reached or exceeded. </p> <p> This operation is synchronous. </p>
describeAccountAttributes :: forall eff. DescribeAccountAttributesRequest -> Aff (exception :: EXCEPTION | eff) DescribeAccountAttributesResponse
describeAccountAttributes = Request.request serviceName "describeAccountAttributes" 


-- | <p> Describes backups. The results are ordered by time, with newest backups first. If you do not specify a BackupId or ServerName, the command returns all backups. </p> <p> This operation is synchronous. </p> <p> A <code>ResourceNotFoundException</code> is thrown when the backup does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
describeBackups :: forall eff. DescribeBackupsRequest -> Aff (exception :: EXCEPTION | eff) DescribeBackupsResponse
describeBackups = Request.request serviceName "describeBackups" 


-- | <p> Describes events for a specified server. Results are ordered by time, with newest events first. </p> <p> This operation is synchronous. </p> <p> A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
describeEvents :: forall eff. DescribeEventsRequest -> Aff (exception :: EXCEPTION | eff) DescribeEventsResponse
describeEvents = Request.request serviceName "describeEvents" 


-- | <p> Returns the current status of an existing association or disassociation request. </p> <p> A <code>ResourceNotFoundException</code> is thrown when no recent association or disassociation request with the specified token is found, or when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
describeNodeAssociationStatus :: forall eff. DescribeNodeAssociationStatusRequest -> Aff (exception :: EXCEPTION | eff) DescribeNodeAssociationStatusResponse
describeNodeAssociationStatus = Request.request serviceName "describeNodeAssociationStatus" 


-- | <p> Lists all configuration management servers that are identified with your account. Only the stored results from Amazon DynamoDB are returned. AWS OpsWorks CM does not query other services. </p> <p> This operation is synchronous. </p> <p> A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
describeServers :: forall eff. DescribeServersRequest -> Aff (exception :: EXCEPTION | eff) DescribeServersResponse
describeServers = Request.request serviceName "describeServers" 


-- | <p> Disassociates a node from an AWS OpsWorks CM server, and removes the node from the server's managed nodes. After a node is disassociated, the node key pair is no longer valid for accessing the configuration manager's API. For more information about how to associate a node, see <a>AssociateNode</a>. </p> <p>A node can can only be disassociated from a server that is in a <code>HEALTHY</code> state. Otherwise, an <code>InvalidStateException</code> is thrown. A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
disassociateNode :: forall eff. DisassociateNodeRequest -> Aff (exception :: EXCEPTION | eff) DisassociateNodeResponse
disassociateNode = Request.request serviceName "disassociateNode" 


-- | <p> Restores a backup to a server that is in a <code>CONNECTION_LOST</code>, <code>HEALTHY</code>, <code>RUNNING</code>, <code>UNHEALTHY</code>, or <code>TERMINATED</code> state. When you run RestoreServer, the server's EC2 instance is deleted, and a new EC2 instance is configured. RestoreServer maintains the existing server endpoint, so configuration management of the server's client devices (nodes) should continue to work. </p> <p> This operation is asynchronous. </p> <p> An <code>InvalidStateException</code> is thrown when the server is not in a valid state. A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
restoreServer :: forall eff. RestoreServerRequest -> Aff (exception :: EXCEPTION | eff) RestoreServerResponse
restoreServer = Request.request serviceName "restoreServer" 


-- | <p> Manually starts server maintenance. This command can be useful if an earlier maintenance attempt failed, and the underlying cause of maintenance failure has been resolved. The server is in an <code>UNDER_MAINTENANCE</code> state while maintenance is in progress. </p> <p> Maintenance can only be started on servers in <code>HEALTHY</code> and <code>UNHEALTHY</code> states. Otherwise, an <code>InvalidStateException</code> is thrown. A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
startMaintenance :: forall eff. StartMaintenanceRequest -> Aff (exception :: EXCEPTION | eff) StartMaintenanceResponse
startMaintenance = Request.request serviceName "startMaintenance" 


-- | <p> Updates settings for a server. </p> <p> This operation is synchronous. </p>
updateServer :: forall eff. UpdateServerRequest -> Aff (exception :: EXCEPTION | eff) UpdateServerResponse
updateServer = Request.request serviceName "updateServer" 


-- | <p> Updates engine-specific attributes on a specified server. The server enters the <code>MODIFYING</code> state when this operation is in progress. Only one update can occur at a time. You can use this command to reset a Chef server's private key (<code>CHEF_PIVOTAL_KEY</code>), a Chef server's admin password (<code>CHEF_DELIVERY_ADMIN_PASSWORD</code>), or a Puppet server's admin password (<code>PUPPET_ADMIN_PASSWORD</code>). </p> <p> This operation is asynchronous. </p> <p> This operation can only be called for servers in <code>HEALTHY</code> or <code>UNHEALTHY</code> states. Otherwise, an <code>InvalidStateException</code> is raised. A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
updateServerEngineAttributes :: forall eff. UpdateServerEngineAttributesRequest -> Aff (exception :: EXCEPTION | eff) UpdateServerEngineAttributesResponse
updateServerEngineAttributes = Request.request serviceName "updateServerEngineAttributes" 


-- | <p>Stores account attributes. </p>
newtype AccountAttribute = AccountAttribute 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Maximum" :: NullOrUndefined.NullOrUndefined (Int)
  , "Used" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeAccountAttribute :: Newtype AccountAttribute _
derive instance repGenericAccountAttribute :: Generic AccountAttribute _
instance showAccountAttribute :: Show AccountAttribute where
  show = genericShow
instance decodeAccountAttribute :: Decode AccountAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountAttribute :: Encode AccountAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A list of individual account attributes. </p>
newtype AccountAttributes = AccountAttributes (Array AccountAttribute)
derive instance newtypeAccountAttributes :: Newtype AccountAttributes _
derive instance repGenericAccountAttributes :: Generic AccountAttributes _
instance showAccountAttributes :: Show AccountAttributes where
  show = genericShow
instance decodeAccountAttributes :: Decode AccountAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountAttributes :: Encode AccountAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateNodeRequest = AssociateNodeRequest 
  { "ServerName" :: (ServerName)
  , "NodeName" :: (NodeName)
  , "EngineAttributes" :: (EngineAttributes)
  }
derive instance newtypeAssociateNodeRequest :: Newtype AssociateNodeRequest _
derive instance repGenericAssociateNodeRequest :: Generic AssociateNodeRequest _
instance showAssociateNodeRequest :: Show AssociateNodeRequest where
  show = genericShow
instance decodeAssociateNodeRequest :: Decode AssociateNodeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateNodeRequest :: Encode AssociateNodeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateNodeResponse = AssociateNodeResponse 
  { "NodeAssociationStatusToken" :: NullOrUndefined.NullOrUndefined (NodeAssociationStatusToken)
  }
derive instance newtypeAssociateNodeResponse :: Newtype AssociateNodeResponse _
derive instance repGenericAssociateNodeResponse :: Generic AssociateNodeResponse _
instance showAssociateNodeResponse :: Show AssociateNodeResponse where
  show = genericShow
instance decodeAssociateNodeResponse :: Decode AssociateNodeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateNodeResponse :: Encode AssociateNodeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeName = AttributeName String
derive instance newtypeAttributeName :: Newtype AttributeName _
derive instance repGenericAttributeName :: Generic AttributeName _
instance showAttributeName :: Show AttributeName where
  show = genericShow
instance decodeAttributeName :: Decode AttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeName :: Encode AttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeValue = AttributeValue String
derive instance newtypeAttributeValue :: Newtype AttributeValue _
derive instance repGenericAttributeValue :: Generic AttributeValue _
instance showAttributeValue :: Show AttributeValue where
  show = genericShow
instance decodeAttributeValue :: Decode AttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValue :: Encode AttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a single backup. </p>
newtype Backup = Backup 
  { "BackupArn" :: NullOrUndefined.NullOrUndefined (String)
  , "BackupId" :: NullOrUndefined.NullOrUndefined (BackupId)
  , "BackupType" :: NullOrUndefined.NullOrUndefined (BackupType)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (Number)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineModel" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceProfileArn" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceType" :: NullOrUndefined.NullOrUndefined (String)
  , "KeyPair" :: NullOrUndefined.NullOrUndefined (String)
  , "PreferredBackupWindow" :: NullOrUndefined.NullOrUndefined (TimeWindowDefinition)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (TimeWindowDefinition)
  , "S3DataSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "S3DataUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "S3LogUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (Strings)
  , "ServerName" :: NullOrUndefined.NullOrUndefined (ServerName)
  , "ServiceRoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (BackupStatus)
  , "StatusDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "SubnetIds" :: NullOrUndefined.NullOrUndefined (Strings)
  , "ToolsVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "UserArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBackup :: Newtype Backup _
derive instance repGenericBackup :: Generic Backup _
instance showBackup :: Show Backup where
  show = genericShow
instance decodeBackup :: Decode Backup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackup :: Encode Backup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BackupId = BackupId String
derive instance newtypeBackupId :: Newtype BackupId _
derive instance repGenericBackupId :: Generic BackupId _
instance showBackupId :: Show BackupId where
  show = genericShow
instance decodeBackupId :: Decode BackupId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackupId :: Encode BackupId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BackupRetentionCountDefinition = BackupRetentionCountDefinition Int
derive instance newtypeBackupRetentionCountDefinition :: Newtype BackupRetentionCountDefinition _
derive instance repGenericBackupRetentionCountDefinition :: Generic BackupRetentionCountDefinition _
instance showBackupRetentionCountDefinition :: Show BackupRetentionCountDefinition where
  show = genericShow
instance decodeBackupRetentionCountDefinition :: Decode BackupRetentionCountDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackupRetentionCountDefinition :: Encode BackupRetentionCountDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BackupStatus = BackupStatus String
derive instance newtypeBackupStatus :: Newtype BackupStatus _
derive instance repGenericBackupStatus :: Generic BackupStatus _
instance showBackupStatus :: Show BackupStatus where
  show = genericShow
instance decodeBackupStatus :: Decode BackupStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackupStatus :: Encode BackupStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BackupType = BackupType String
derive instance newtypeBackupType :: Newtype BackupType _
derive instance repGenericBackupType :: Generic BackupType _
instance showBackupType :: Show BackupType where
  show = genericShow
instance decodeBackupType :: Decode BackupType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackupType :: Encode BackupType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Backups = Backups (Array Backup)
derive instance newtypeBackups :: Newtype Backups _
derive instance repGenericBackups :: Generic Backups _
instance showBackups :: Show Backups where
  show = genericShow
instance decodeBackups :: Decode Backups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackups :: Encode Backups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateBackupRequest = CreateBackupRequest 
  { "ServerName" :: (ServerName)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateBackupRequest :: Newtype CreateBackupRequest _
derive instance repGenericCreateBackupRequest :: Generic CreateBackupRequest _
instance showCreateBackupRequest :: Show CreateBackupRequest where
  show = genericShow
instance decodeCreateBackupRequest :: Decode CreateBackupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBackupRequest :: Encode CreateBackupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateBackupResponse = CreateBackupResponse 
  { "Backup" :: NullOrUndefined.NullOrUndefined (Backup)
  }
derive instance newtypeCreateBackupResponse :: Newtype CreateBackupResponse _
derive instance repGenericCreateBackupResponse :: Generic CreateBackupResponse _
instance showCreateBackupResponse :: Show CreateBackupResponse where
  show = genericShow
instance decodeCreateBackupResponse :: Decode CreateBackupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBackupResponse :: Encode CreateBackupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateServerRequest = CreateServerRequest 
  { "AssociatePublicIpAddress" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "DisableAutomatedBackup" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineModel" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineAttributes" :: NullOrUndefined.NullOrUndefined (EngineAttributes)
  , "BackupRetentionCount" :: NullOrUndefined.NullOrUndefined (BackupRetentionCountDefinition)
  , "ServerName" :: (ServerName)
  , "InstanceProfileArn" :: (InstanceProfileArn)
  , "InstanceType" :: (String)
  , "KeyPair" :: NullOrUndefined.NullOrUndefined (KeyPair)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (TimeWindowDefinition)
  , "PreferredBackupWindow" :: NullOrUndefined.NullOrUndefined (TimeWindowDefinition)
  , "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (Strings)
  , "ServiceRoleArn" :: (ServiceRoleArn)
  , "SubnetIds" :: NullOrUndefined.NullOrUndefined (Strings)
  , "BackupId" :: NullOrUndefined.NullOrUndefined (BackupId)
  }
derive instance newtypeCreateServerRequest :: Newtype CreateServerRequest _
derive instance repGenericCreateServerRequest :: Generic CreateServerRequest _
instance showCreateServerRequest :: Show CreateServerRequest where
  show = genericShow
instance decodeCreateServerRequest :: Decode CreateServerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateServerRequest :: Encode CreateServerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateServerResponse = CreateServerResponse 
  { "Server" :: NullOrUndefined.NullOrUndefined (Server)
  }
derive instance newtypeCreateServerResponse :: Newtype CreateServerResponse _
derive instance repGenericCreateServerResponse :: Generic CreateServerResponse _
instance showCreateServerResponse :: Show CreateServerResponse where
  show = genericShow
instance decodeCreateServerResponse :: Decode CreateServerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateServerResponse :: Encode CreateServerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBackupRequest = DeleteBackupRequest 
  { "BackupId" :: (BackupId)
  }
derive instance newtypeDeleteBackupRequest :: Newtype DeleteBackupRequest _
derive instance repGenericDeleteBackupRequest :: Generic DeleteBackupRequest _
instance showDeleteBackupRequest :: Show DeleteBackupRequest where
  show = genericShow
instance decodeDeleteBackupRequest :: Decode DeleteBackupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBackupRequest :: Encode DeleteBackupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBackupResponse = DeleteBackupResponse Types.NoArguments
derive instance newtypeDeleteBackupResponse :: Newtype DeleteBackupResponse _
derive instance repGenericDeleteBackupResponse :: Generic DeleteBackupResponse _
instance showDeleteBackupResponse :: Show DeleteBackupResponse where
  show = genericShow
instance decodeDeleteBackupResponse :: Decode DeleteBackupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBackupResponse :: Encode DeleteBackupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteServerRequest = DeleteServerRequest 
  { "ServerName" :: (ServerName)
  }
derive instance newtypeDeleteServerRequest :: Newtype DeleteServerRequest _
derive instance repGenericDeleteServerRequest :: Generic DeleteServerRequest _
instance showDeleteServerRequest :: Show DeleteServerRequest where
  show = genericShow
instance decodeDeleteServerRequest :: Decode DeleteServerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteServerRequest :: Encode DeleteServerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteServerResponse = DeleteServerResponse Types.NoArguments
derive instance newtypeDeleteServerResponse :: Newtype DeleteServerResponse _
derive instance repGenericDeleteServerResponse :: Generic DeleteServerResponse _
instance showDeleteServerResponse :: Show DeleteServerResponse where
  show = genericShow
instance decodeDeleteServerResponse :: Decode DeleteServerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteServerResponse :: Encode DeleteServerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAccountAttributesRequest = DescribeAccountAttributesRequest Types.NoArguments
derive instance newtypeDescribeAccountAttributesRequest :: Newtype DescribeAccountAttributesRequest _
derive instance repGenericDescribeAccountAttributesRequest :: Generic DescribeAccountAttributesRequest _
instance showDescribeAccountAttributesRequest :: Show DescribeAccountAttributesRequest where
  show = genericShow
instance decodeDescribeAccountAttributesRequest :: Decode DescribeAccountAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAccountAttributesRequest :: Encode DescribeAccountAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAccountAttributesResponse = DescribeAccountAttributesResponse 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (AccountAttributes)
  }
derive instance newtypeDescribeAccountAttributesResponse :: Newtype DescribeAccountAttributesResponse _
derive instance repGenericDescribeAccountAttributesResponse :: Generic DescribeAccountAttributesResponse _
instance showDescribeAccountAttributesResponse :: Show DescribeAccountAttributesResponse where
  show = genericShow
instance decodeDescribeAccountAttributesResponse :: Decode DescribeAccountAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAccountAttributesResponse :: Encode DescribeAccountAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeBackupsRequest = DescribeBackupsRequest 
  { "BackupId" :: NullOrUndefined.NullOrUndefined (BackupId)
  , "ServerName" :: NullOrUndefined.NullOrUndefined (ServerName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeDescribeBackupsRequest :: Newtype DescribeBackupsRequest _
derive instance repGenericDescribeBackupsRequest :: Generic DescribeBackupsRequest _
instance showDescribeBackupsRequest :: Show DescribeBackupsRequest where
  show = genericShow
instance decodeDescribeBackupsRequest :: Decode DescribeBackupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBackupsRequest :: Encode DescribeBackupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeBackupsResponse = DescribeBackupsResponse 
  { "Backups" :: NullOrUndefined.NullOrUndefined (Backups)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeBackupsResponse :: Newtype DescribeBackupsResponse _
derive instance repGenericDescribeBackupsResponse :: Generic DescribeBackupsResponse _
instance showDescribeBackupsResponse :: Show DescribeBackupsResponse where
  show = genericShow
instance decodeDescribeBackupsResponse :: Decode DescribeBackupsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBackupsResponse :: Encode DescribeBackupsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventsRequest = DescribeEventsRequest 
  { "ServerName" :: (ServerName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeDescribeEventsRequest :: Newtype DescribeEventsRequest _
derive instance repGenericDescribeEventsRequest :: Generic DescribeEventsRequest _
instance showDescribeEventsRequest :: Show DescribeEventsRequest where
  show = genericShow
instance decodeDescribeEventsRequest :: Decode DescribeEventsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventsRequest :: Encode DescribeEventsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventsResponse = DescribeEventsResponse 
  { "ServerEvents" :: NullOrUndefined.NullOrUndefined (ServerEvents)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeEventsResponse :: Newtype DescribeEventsResponse _
derive instance repGenericDescribeEventsResponse :: Generic DescribeEventsResponse _
instance showDescribeEventsResponse :: Show DescribeEventsResponse where
  show = genericShow
instance decodeDescribeEventsResponse :: Decode DescribeEventsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventsResponse :: Encode DescribeEventsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeNodeAssociationStatusRequest = DescribeNodeAssociationStatusRequest 
  { "NodeAssociationStatusToken" :: (NodeAssociationStatusToken)
  , "ServerName" :: (ServerName)
  }
derive instance newtypeDescribeNodeAssociationStatusRequest :: Newtype DescribeNodeAssociationStatusRequest _
derive instance repGenericDescribeNodeAssociationStatusRequest :: Generic DescribeNodeAssociationStatusRequest _
instance showDescribeNodeAssociationStatusRequest :: Show DescribeNodeAssociationStatusRequest where
  show = genericShow
instance decodeDescribeNodeAssociationStatusRequest :: Decode DescribeNodeAssociationStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeNodeAssociationStatusRequest :: Encode DescribeNodeAssociationStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeNodeAssociationStatusResponse = DescribeNodeAssociationStatusResponse 
  { "NodeAssociationStatus" :: NullOrUndefined.NullOrUndefined (NodeAssociationStatus)
  , "EngineAttributes" :: NullOrUndefined.NullOrUndefined (EngineAttributes)
  }
derive instance newtypeDescribeNodeAssociationStatusResponse :: Newtype DescribeNodeAssociationStatusResponse _
derive instance repGenericDescribeNodeAssociationStatusResponse :: Generic DescribeNodeAssociationStatusResponse _
instance showDescribeNodeAssociationStatusResponse :: Show DescribeNodeAssociationStatusResponse where
  show = genericShow
instance decodeDescribeNodeAssociationStatusResponse :: Decode DescribeNodeAssociationStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeNodeAssociationStatusResponse :: Encode DescribeNodeAssociationStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeServersRequest = DescribeServersRequest 
  { "ServerName" :: NullOrUndefined.NullOrUndefined (ServerName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeDescribeServersRequest :: Newtype DescribeServersRequest _
derive instance repGenericDescribeServersRequest :: Generic DescribeServersRequest _
instance showDescribeServersRequest :: Show DescribeServersRequest where
  show = genericShow
instance decodeDescribeServersRequest :: Decode DescribeServersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeServersRequest :: Encode DescribeServersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeServersResponse = DescribeServersResponse 
  { "Servers" :: NullOrUndefined.NullOrUndefined (Servers)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeServersResponse :: Newtype DescribeServersResponse _
derive instance repGenericDescribeServersResponse :: Generic DescribeServersResponse _
instance showDescribeServersResponse :: Show DescribeServersResponse where
  show = genericShow
instance decodeDescribeServersResponse :: Decode DescribeServersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeServersResponse :: Encode DescribeServersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateNodeRequest = DisassociateNodeRequest 
  { "ServerName" :: (ServerName)
  , "NodeName" :: (NodeName)
  , "EngineAttributes" :: NullOrUndefined.NullOrUndefined (EngineAttributes)
  }
derive instance newtypeDisassociateNodeRequest :: Newtype DisassociateNodeRequest _
derive instance repGenericDisassociateNodeRequest :: Generic DisassociateNodeRequest _
instance showDisassociateNodeRequest :: Show DisassociateNodeRequest where
  show = genericShow
instance decodeDisassociateNodeRequest :: Decode DisassociateNodeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateNodeRequest :: Encode DisassociateNodeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateNodeResponse = DisassociateNodeResponse 
  { "NodeAssociationStatusToken" :: NullOrUndefined.NullOrUndefined (NodeAssociationStatusToken)
  }
derive instance newtypeDisassociateNodeResponse :: Newtype DisassociateNodeResponse _
derive instance repGenericDisassociateNodeResponse :: Generic DisassociateNodeResponse _
instance showDisassociateNodeResponse :: Show DisassociateNodeResponse where
  show = genericShow
instance decodeDisassociateNodeResponse :: Decode DisassociateNodeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateNodeResponse :: Encode DisassociateNodeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A name and value pair that is specific to the engine of the server. </p>
newtype EngineAttribute = EngineAttribute 
  { "Name" :: NullOrUndefined.NullOrUndefined (EngineAttributeName)
  , "Value" :: NullOrUndefined.NullOrUndefined (EngineAttributeValue)
  }
derive instance newtypeEngineAttribute :: Newtype EngineAttribute _
derive instance repGenericEngineAttribute :: Generic EngineAttribute _
instance showEngineAttribute :: Show EngineAttribute where
  show = genericShow
instance decodeEngineAttribute :: Decode EngineAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEngineAttribute :: Encode EngineAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EngineAttributeName = EngineAttributeName String
derive instance newtypeEngineAttributeName :: Newtype EngineAttributeName _
derive instance repGenericEngineAttributeName :: Generic EngineAttributeName _
instance showEngineAttributeName :: Show EngineAttributeName where
  show = genericShow
instance decodeEngineAttributeName :: Decode EngineAttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEngineAttributeName :: Encode EngineAttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EngineAttributeValue = EngineAttributeValue String
derive instance newtypeEngineAttributeValue :: Newtype EngineAttributeValue _
derive instance repGenericEngineAttributeValue :: Generic EngineAttributeValue _
instance showEngineAttributeValue :: Show EngineAttributeValue where
  show = genericShow
instance decodeEngineAttributeValue :: Decode EngineAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEngineAttributeValue :: Encode EngineAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EngineAttributes = EngineAttributes (Array EngineAttribute)
derive instance newtypeEngineAttributes :: Newtype EngineAttributes _
derive instance repGenericEngineAttributes :: Generic EngineAttributes _
instance showEngineAttributes :: Show EngineAttributes where
  show = genericShow
instance decodeEngineAttributes :: Decode EngineAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEngineAttributes :: Encode EngineAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceProfileArn = InstanceProfileArn String
derive instance newtypeInstanceProfileArn :: Newtype InstanceProfileArn _
derive instance repGenericInstanceProfileArn :: Generic InstanceProfileArn _
instance showInstanceProfileArn :: Show InstanceProfileArn where
  show = genericShow
instance decodeInstanceProfileArn :: Decode InstanceProfileArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceProfileArn :: Encode InstanceProfileArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This occurs when the provided nextToken is not valid. </p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _
derive instance repGenericInvalidNextTokenException :: Generic InvalidNextTokenException _
instance showInvalidNextTokenException :: Show InvalidNextTokenException where
  show = genericShow
instance decodeInvalidNextTokenException :: Decode InvalidNextTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNextTokenException :: Encode InvalidNextTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource is in a state that does not allow you to perform a specified action. </p>
newtype InvalidStateException = InvalidStateException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidStateException :: Newtype InvalidStateException _
derive instance repGenericInvalidStateException :: Generic InvalidStateException _
instance showInvalidStateException :: Show InvalidStateException where
  show = genericShow
instance decodeInvalidStateException :: Decode InvalidStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidStateException :: Encode InvalidStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyPair = KeyPair String
derive instance newtypeKeyPair :: Newtype KeyPair _
derive instance repGenericKeyPair :: Generic KeyPair _
instance showKeyPair :: Show KeyPair where
  show = genericShow
instance decodeKeyPair :: Decode KeyPair where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyPair :: Encode KeyPair where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The limit of servers or backups has been reached. </p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceStatus = MaintenanceStatus String
derive instance newtypeMaintenanceStatus :: Newtype MaintenanceStatus _
derive instance repGenericMaintenanceStatus :: Generic MaintenanceStatus _
instance showMaintenanceStatus :: Show MaintenanceStatus where
  show = genericShow
instance decodeMaintenanceStatus :: Decode MaintenanceStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceStatus :: Encode MaintenanceStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The status of the association or disassociation request. </p> <p class="title"> <b>Possible values:</b> </p> <ul> <li> <p> <code>SUCCESS</code>: The association or disassociation succeeded. </p> </li> <li> <p> <code>FAILED</code>: The association or disassociation failed. </p> </li> <li> <p> <code>IN_PROGRESS</code>: The association or disassociation is still in progress. </p> </li> </ul>
newtype NodeAssociationStatus = NodeAssociationStatus String
derive instance newtypeNodeAssociationStatus :: Newtype NodeAssociationStatus _
derive instance repGenericNodeAssociationStatus :: Generic NodeAssociationStatus _
instance showNodeAssociationStatus :: Show NodeAssociationStatus where
  show = genericShow
instance decodeNodeAssociationStatus :: Decode NodeAssociationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeAssociationStatus :: Encode NodeAssociationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NodeAssociationStatusToken = NodeAssociationStatusToken String
derive instance newtypeNodeAssociationStatusToken :: Newtype NodeAssociationStatusToken _
derive instance repGenericNodeAssociationStatusToken :: Generic NodeAssociationStatusToken _
instance showNodeAssociationStatusToken :: Show NodeAssociationStatusToken where
  show = genericShow
instance decodeNodeAssociationStatusToken :: Decode NodeAssociationStatusToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeAssociationStatusToken :: Encode NodeAssociationStatusToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The node name that is used by <code>chef-client</code> or <code>puppet-agent</code>for a new node. We recommend to use a unique FQDN as hostname. For more information, see the <a href="http://docs.aws.amazon.com/https:/docs.chef.io/nodes.html#about-node-names">Chef</a> or <a href="http://docs.aws.amazon.com/https:/docs.puppet.com/puppet/4.10/man/agent.html">Puppet</a> documentation. </p>
newtype NodeName = NodeName String
derive instance newtypeNodeName :: Newtype NodeName _
derive instance repGenericNodeName :: Generic NodeName _
instance showNodeName :: Show NodeName where
  show = genericShow
instance decodeNodeName :: Decode NodeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNodeName :: Encode NodeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested resource cannot be created because it already exists. </p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceAlreadyExistsException :: Newtype ResourceAlreadyExistsException _
derive instance repGenericResourceAlreadyExistsException :: Generic ResourceAlreadyExistsException _
instance showResourceAlreadyExistsException :: Show ResourceAlreadyExistsException where
  show = genericShow
instance decodeResourceAlreadyExistsException :: Decode ResourceAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceAlreadyExistsException :: Encode ResourceAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested resource does not exist, or access was denied. </p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestoreServerRequest = RestoreServerRequest 
  { "BackupId" :: (BackupId)
  , "ServerName" :: (ServerName)
  , "InstanceType" :: NullOrUndefined.NullOrUndefined (String)
  , "KeyPair" :: NullOrUndefined.NullOrUndefined (KeyPair)
  }
derive instance newtypeRestoreServerRequest :: Newtype RestoreServerRequest _
derive instance repGenericRestoreServerRequest :: Generic RestoreServerRequest _
instance showRestoreServerRequest :: Show RestoreServerRequest where
  show = genericShow
instance decodeRestoreServerRequest :: Decode RestoreServerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreServerRequest :: Encode RestoreServerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestoreServerResponse = RestoreServerResponse Types.NoArguments
derive instance newtypeRestoreServerResponse :: Newtype RestoreServerResponse _
derive instance repGenericRestoreServerResponse :: Generic RestoreServerResponse _
instance showRestoreServerResponse :: Show RestoreServerResponse where
  show = genericShow
instance decodeRestoreServerResponse :: Decode RestoreServerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreServerResponse :: Encode RestoreServerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a configuration management server. </p>
newtype Server = Server 
  { "AssociatePublicIpAddress" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "BackupRetentionCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "ServerName" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (Number)
  , "CloudFormationStackArn" :: NullOrUndefined.NullOrUndefined (String)
  , "DisableAutomatedBackup" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Endpoint" :: NullOrUndefined.NullOrUndefined (String)
  , "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineModel" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineAttributes" :: NullOrUndefined.NullOrUndefined (EngineAttributes)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceProfileArn" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceType" :: NullOrUndefined.NullOrUndefined (String)
  , "KeyPair" :: NullOrUndefined.NullOrUndefined (String)
  , "MaintenanceStatus" :: NullOrUndefined.NullOrUndefined (MaintenanceStatus)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (TimeWindowDefinition)
  , "PreferredBackupWindow" :: NullOrUndefined.NullOrUndefined (TimeWindowDefinition)
  , "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (Strings)
  , "ServiceRoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (ServerStatus)
  , "StatusReason" :: NullOrUndefined.NullOrUndefined (String)
  , "SubnetIds" :: NullOrUndefined.NullOrUndefined (Strings)
  , "ServerArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeServer :: Newtype Server _
derive instance repGenericServer :: Generic Server _
instance showServer :: Show Server where
  show = genericShow
instance decodeServer :: Decode Server where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServer :: Encode Server where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An event that is related to the server, such as the start of maintenance or backup. </p>
newtype ServerEvent = ServerEvent 
  { "CreatedAt" :: NullOrUndefined.NullOrUndefined (Number)
  , "ServerName" :: NullOrUndefined.NullOrUndefined (String)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "LogUrl" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeServerEvent :: Newtype ServerEvent _
derive instance repGenericServerEvent :: Generic ServerEvent _
instance showServerEvent :: Show ServerEvent where
  show = genericShow
instance decodeServerEvent :: Decode ServerEvent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerEvent :: Encode ServerEvent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServerEvents = ServerEvents (Array ServerEvent)
derive instance newtypeServerEvents :: Newtype ServerEvents _
derive instance repGenericServerEvents :: Generic ServerEvents _
instance showServerEvents :: Show ServerEvents where
  show = genericShow
instance decodeServerEvents :: Decode ServerEvents where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerEvents :: Encode ServerEvents where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServerName = ServerName String
derive instance newtypeServerName :: Newtype ServerName _
derive instance repGenericServerName :: Generic ServerName _
instance showServerName :: Show ServerName where
  show = genericShow
instance decodeServerName :: Decode ServerName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerName :: Encode ServerName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServerStatus = ServerStatus String
derive instance newtypeServerStatus :: Newtype ServerStatus _
derive instance repGenericServerStatus :: Generic ServerStatus _
instance showServerStatus :: Show ServerStatus where
  show = genericShow
instance decodeServerStatus :: Decode ServerStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerStatus :: Encode ServerStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Servers = Servers (Array Server)
derive instance newtypeServers :: Newtype Servers _
derive instance repGenericServers :: Generic Servers _
instance showServers :: Show Servers where
  show = genericShow
instance decodeServers :: Decode Servers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServers :: Encode Servers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServiceRoleArn = ServiceRoleArn String
derive instance newtypeServiceRoleArn :: Newtype ServiceRoleArn _
derive instance repGenericServiceRoleArn :: Generic ServiceRoleArn _
instance showServiceRoleArn :: Show ServiceRoleArn where
  show = genericShow
instance decodeServiceRoleArn :: Decode ServiceRoleArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceRoleArn :: Encode ServiceRoleArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartMaintenanceRequest = StartMaintenanceRequest 
  { "ServerName" :: (ServerName)
  , "EngineAttributes" :: NullOrUndefined.NullOrUndefined (EngineAttributes)
  }
derive instance newtypeStartMaintenanceRequest :: Newtype StartMaintenanceRequest _
derive instance repGenericStartMaintenanceRequest :: Generic StartMaintenanceRequest _
instance showStartMaintenanceRequest :: Show StartMaintenanceRequest where
  show = genericShow
instance decodeStartMaintenanceRequest :: Decode StartMaintenanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartMaintenanceRequest :: Encode StartMaintenanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartMaintenanceResponse = StartMaintenanceResponse 
  { "Server" :: NullOrUndefined.NullOrUndefined (Server)
  }
derive instance newtypeStartMaintenanceResponse :: Newtype StartMaintenanceResponse _
derive instance repGenericStartMaintenanceResponse :: Generic StartMaintenanceResponse _
instance showStartMaintenanceResponse :: Show StartMaintenanceResponse where
  show = genericShow
instance decodeStartMaintenanceResponse :: Decode StartMaintenanceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartMaintenanceResponse :: Encode StartMaintenanceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Strings = Strings (Array String)
derive instance newtypeStrings :: Newtype Strings _
derive instance repGenericStrings :: Generic Strings _
instance showStrings :: Show Strings where
  show = genericShow
instance decodeStrings :: Decode Strings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStrings :: Encode Strings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> <code>DDD:HH:MM</code> (weekly start time) or <code>HH:MM</code> (daily start time). </p> <p> Time windows always use coordinated universal time (UTC). Valid strings for day of week (<code>DDD</code>) are: <code>Mon</code>, <code>Tue</code>, <code>Wed</code>, <code>Thr</code>, <code>Fri</code>, <code>Sat</code>, or <code>Sun</code>.</p>
newtype TimeWindowDefinition = TimeWindowDefinition String
derive instance newtypeTimeWindowDefinition :: Newtype TimeWindowDefinition _
derive instance repGenericTimeWindowDefinition :: Generic TimeWindowDefinition _
instance showTimeWindowDefinition :: Show TimeWindowDefinition where
  show = genericShow
instance decodeTimeWindowDefinition :: Decode TimeWindowDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeWindowDefinition :: Encode TimeWindowDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateServerEngineAttributesRequest = UpdateServerEngineAttributesRequest 
  { "ServerName" :: (ServerName)
  , "AttributeName" :: (AttributeName)
  , "AttributeValue" :: NullOrUndefined.NullOrUndefined (AttributeValue)
  }
derive instance newtypeUpdateServerEngineAttributesRequest :: Newtype UpdateServerEngineAttributesRequest _
derive instance repGenericUpdateServerEngineAttributesRequest :: Generic UpdateServerEngineAttributesRequest _
instance showUpdateServerEngineAttributesRequest :: Show UpdateServerEngineAttributesRequest where
  show = genericShow
instance decodeUpdateServerEngineAttributesRequest :: Decode UpdateServerEngineAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateServerEngineAttributesRequest :: Encode UpdateServerEngineAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateServerEngineAttributesResponse = UpdateServerEngineAttributesResponse 
  { "Server" :: NullOrUndefined.NullOrUndefined (Server)
  }
derive instance newtypeUpdateServerEngineAttributesResponse :: Newtype UpdateServerEngineAttributesResponse _
derive instance repGenericUpdateServerEngineAttributesResponse :: Generic UpdateServerEngineAttributesResponse _
instance showUpdateServerEngineAttributesResponse :: Show UpdateServerEngineAttributesResponse where
  show = genericShow
instance decodeUpdateServerEngineAttributesResponse :: Decode UpdateServerEngineAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateServerEngineAttributesResponse :: Encode UpdateServerEngineAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateServerRequest = UpdateServerRequest 
  { "DisableAutomatedBackup" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "BackupRetentionCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "ServerName" :: (ServerName)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (TimeWindowDefinition)
  , "PreferredBackupWindow" :: NullOrUndefined.NullOrUndefined (TimeWindowDefinition)
  }
derive instance newtypeUpdateServerRequest :: Newtype UpdateServerRequest _
derive instance repGenericUpdateServerRequest :: Generic UpdateServerRequest _
instance showUpdateServerRequest :: Show UpdateServerRequest where
  show = genericShow
instance decodeUpdateServerRequest :: Decode UpdateServerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateServerRequest :: Encode UpdateServerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateServerResponse = UpdateServerResponse 
  { "Server" :: NullOrUndefined.NullOrUndefined (Server)
  }
derive instance newtypeUpdateServerResponse :: Newtype UpdateServerResponse _
derive instance repGenericUpdateServerResponse :: Generic UpdateServerResponse _
instance showUpdateServerResponse :: Show UpdateServerResponse where
  show = genericShow
instance decodeUpdateServerResponse :: Decode UpdateServerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateServerResponse :: Encode UpdateServerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more of the provided request parameters are not valid. </p>
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeValidationException :: Newtype ValidationException _
derive instance repGenericValidationException :: Generic ValidationException _
instance showValidationException :: Show ValidationException where
  show = genericShow
instance decodeValidationException :: Decode ValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationException :: Encode ValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
