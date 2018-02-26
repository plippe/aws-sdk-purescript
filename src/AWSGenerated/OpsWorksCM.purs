

-- | <fullname>AWS OpsWorks CM</fullname> <p> AWS OpsWorks for configuration management (CM) is a service that runs and manages configuration management servers. </p> <p> <b>Glossary of terms</b> </p> <ul> <li> <p> <b>Server</b>: A configuration management server that can be highly-available. The configuration management server runs on an Amazon Elastic Compute Cloud (EC2) instance, and may use various other AWS services, such as Amazon Relational Database Service (RDS) and Elastic Load Balancing. A server is a generic abstraction over the configuration manager that you want to use, much like Amazon RDS. In AWS OpsWorks CM, you do not start or stop servers. After you create servers, they continue to run until they are deleted.</p> </li> <li> <p> <b>Engine</b>: The engine is the specific configuration manager that you want to use. Valid values in this release include <code>Chef</code> and <code>Puppet</code>.</p> </li> <li> <p> <b>Backup</b>: This is an application-level backup of the data that the configuration manager stores. AWS OpsWorks CM creates an S3 bucket for backups when you launch the first server. A backup maintains a snapshot of a server's configuration-related attributes at the time the backup starts.</p> </li> <li> <p> <b>Events</b>: Events are always related to a server. Events are written during server creation, when health checks run, when backups are created, when system maintenance is performed, etc. When you delete a server, the server's events are also deleted.</p> </li> <li> <p> <b>Account attributes</b>: Every account has attributes that are assigned in the AWS OpsWorks CM database. These attributes store information about configuration limits (servers, backups, etc.) and your customer account. </p> </li> </ul> <p> <b>Endpoints</b> </p> <p>AWS OpsWorks CM supports the following endpoints, all HTTPS. You must connect to one of the following endpoints. Your servers can only be accessed or managed within the endpoint in which they are created.</p> <ul> <li> <p>opsworks-cm.us-east-1.amazonaws.com</p> </li> <li> <p>opsworks-cm.us-west-2.amazonaws.com</p> </li> <li> <p>opsworks-cm.eu-west-1.amazonaws.com</p> </li> </ul> <p> <b>Throttling limits</b> </p> <p>All API operations allow for five requests per second with a burst of 10 requests per second.</p>
module AWS.OpsWorksCM where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "OpsWorksCM" :: String


-- | <p> Associates a new node with the server. For more information about how to disassociate a node, see <a>DisassociateNode</a>.</p> <p> On a Chef server: This command is an alternative to <code>knife bootstrap</code>.</p> <p> Example (Chef): <code>aws opsworks-cm associate-node --server-name <i>MyServer</i> --node-name <i>MyManagedNode</i> --engine-attributes "Name=<i>CHEF_ORGANIZATION</i>,Value=default" "Name=<i>CHEF_NODE_PUBLIC_KEY</i>,Value=<i>public-key-pem</i>"</code> </p> <p> On a Puppet server, this command is an alternative to the <code>puppet cert sign</code> command that signs a Puppet node CSR. </p> <p> Example (Chef): <code>aws opsworks-cm associate-node --server-name <i>MyServer</i> --node-name <i>MyManagedNode</i> --engine-attributes "Name=<i>PUPPET_NODE_CSR</i>,Value=<i>csr-pem</i>"</code> </p> <p> A node can can only be associated with servers that are in a <code>HEALTHY</code> state. Otherwise, an <code>InvalidStateException</code> is thrown. A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. The AssociateNode API call can be integrated into Auto Scaling configurations, AWS Cloudformation templates, or the user data of a server's instance. </p>
associateNode :: forall eff. AssociateNodeRequest -> Aff (err :: AWS.RequestError | eff) AssociateNodeResponse
associateNode = AWS.request serviceName "AssociateNode" 


-- | <p> Creates an application-level backup of a server. While the server is in the <code>BACKING_UP</code> state, the server cannot be changed, and no additional backup can be created. </p> <p> Backups can be created for servers in <code>RUNNING</code>, <code>HEALTHY</code>, and <code>UNHEALTHY</code> states. By default, you can create a maximum of 50 manual backups. </p> <p> This operation is asynchronous. </p> <p> A <code>LimitExceededException</code> is thrown when the maximum number of manual backups is reached. An <code>InvalidStateException</code> is thrown when the server is not in any of the following states: RUNNING, HEALTHY, or UNHEALTHY. A <code>ResourceNotFoundException</code> is thrown when the server is not found. A <code>ValidationException</code> is thrown when parameters of the request are not valid. </p>
createBackup :: forall eff. CreateBackupRequest -> Aff (err :: AWS.RequestError | eff) CreateBackupResponse
createBackup = AWS.request serviceName "CreateBackup" 


-- | <p> Creates and immedately starts a new server. The server is ready to use when it is in the <code>HEALTHY</code> state. By default, you can create a maximum of 10 servers. </p> <p> This operation is asynchronous. </p> <p> A <code>LimitExceededException</code> is thrown when you have created the maximum number of servers (10). A <code>ResourceAlreadyExistsException</code> is thrown when a server with the same name already exists in the account. A <code>ResourceNotFoundException</code> is thrown when you specify a backup ID that is not valid or is for a backup that does not exist. A <code>ValidationException</code> is thrown when parameters of the request are not valid. </p> <p> If you do not specify a security group by adding the <code>SecurityGroupIds</code> parameter, AWS OpsWorks creates a new security group. </p> <p> <i>Chef Automate:</i> The default security group opens the Chef server to the world on TCP port 443. If a KeyName is present, AWS OpsWorks enables SSH access. SSH is also open to the world on TCP port 22. </p> <p> <i>Puppet Enterprise:</i> The default security group opens TCP ports 22, 443, 4433, 8140, 8142, 8143, and 8170. If a KeyName is present, AWS OpsWorks enables SSH access. SSH is also open to the world on TCP port 22. </p> <p>By default, your server is accessible from any IP address. We recommend that you update your security group rules to allow access from known IP addresses and address ranges only. To edit security group rules, open Security Groups in the navigation pane of the EC2 management console. </p>
createServer :: forall eff. CreateServerRequest -> Aff (err :: AWS.RequestError | eff) CreateServerResponse
createServer = AWS.request serviceName "CreateServer" 


-- | <p> Deletes a backup. You can delete both manual and automated backups. This operation is asynchronous. </p> <p> An <code>InvalidStateException</code> is thrown when a backup deletion is already in progress. A <code>ResourceNotFoundException</code> is thrown when the backup does not exist. A <code>ValidationException</code> is thrown when parameters of the request are not valid. </p>
deleteBackup :: forall eff. DeleteBackupRequest -> Aff (err :: AWS.RequestError | eff) DeleteBackupResponse
deleteBackup = AWS.request serviceName "DeleteBackup" 


-- | <p> Deletes the server and the underlying AWS CloudFormation stacks (including the server's EC2 instance). When you run this command, the server state is updated to <code>DELETING</code>. After the server is deleted, it is no longer returned by <code>DescribeServer</code> requests. If the AWS CloudFormation stack cannot be deleted, the server cannot be deleted. </p> <p> This operation is asynchronous. </p> <p> An <code>InvalidStateException</code> is thrown when a server deletion is already in progress. A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p> <p> </p>
deleteServer :: forall eff. DeleteServerRequest -> Aff (err :: AWS.RequestError | eff) DeleteServerResponse
deleteServer = AWS.request serviceName "DeleteServer" 


-- | <p> Describes your account attributes, and creates requests to increase limits before they are reached or exceeded. </p> <p> This operation is synchronous. </p>
describeAccountAttributes :: forall eff. DescribeAccountAttributesRequest -> Aff (err :: AWS.RequestError | eff) DescribeAccountAttributesResponse
describeAccountAttributes = AWS.request serviceName "DescribeAccountAttributes" 


-- | <p> Describes backups. The results are ordered by time, with newest backups first. If you do not specify a BackupId or ServerName, the command returns all backups. </p> <p> This operation is synchronous. </p> <p> A <code>ResourceNotFoundException</code> is thrown when the backup does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
describeBackups :: forall eff. DescribeBackupsRequest -> Aff (err :: AWS.RequestError | eff) DescribeBackupsResponse
describeBackups = AWS.request serviceName "DescribeBackups" 


-- | <p> Describes events for a specified server. Results are ordered by time, with newest events first. </p> <p> This operation is synchronous. </p> <p> A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
describeEvents :: forall eff. DescribeEventsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventsResponse
describeEvents = AWS.request serviceName "DescribeEvents" 


-- | <p> Returns the current status of an existing association or disassociation request. </p> <p> A <code>ResourceNotFoundException</code> is thrown when no recent association or disassociation request with the specified token is found, or when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
describeNodeAssociationStatus :: forall eff. DescribeNodeAssociationStatusRequest -> Aff (err :: AWS.RequestError | eff) DescribeNodeAssociationStatusResponse
describeNodeAssociationStatus = AWS.request serviceName "DescribeNodeAssociationStatus" 


-- | <p> Lists all configuration management servers that are identified with your account. Only the stored results from Amazon DynamoDB are returned. AWS OpsWorks CM does not query other services. </p> <p> This operation is synchronous. </p> <p> A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
describeServers :: forall eff. DescribeServersRequest -> Aff (err :: AWS.RequestError | eff) DescribeServersResponse
describeServers = AWS.request serviceName "DescribeServers" 


-- | <p> Disassociates a node from an AWS OpsWorks CM server, and removes the node from the server's managed nodes. After a node is disassociated, the node key pair is no longer valid for accessing the configuration manager's API. For more information about how to associate a node, see <a>AssociateNode</a>. </p> <p>A node can can only be disassociated from a server that is in a <code>HEALTHY</code> state. Otherwise, an <code>InvalidStateException</code> is thrown. A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
disassociateNode :: forall eff. DisassociateNodeRequest -> Aff (err :: AWS.RequestError | eff) DisassociateNodeResponse
disassociateNode = AWS.request serviceName "DisassociateNode" 


-- | <p> Restores a backup to a server that is in a <code>CONNECTION_LOST</code>, <code>HEALTHY</code>, <code>RUNNING</code>, <code>UNHEALTHY</code>, or <code>TERMINATED</code> state. When you run RestoreServer, the server's EC2 instance is deleted, and a new EC2 instance is configured. RestoreServer maintains the existing server endpoint, so configuration management of the server's client devices (nodes) should continue to work. </p> <p> This operation is asynchronous. </p> <p> An <code>InvalidStateException</code> is thrown when the server is not in a valid state. A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
restoreServer :: forall eff. RestoreServerRequest -> Aff (err :: AWS.RequestError | eff) RestoreServerResponse
restoreServer = AWS.request serviceName "RestoreServer" 


-- | <p> Manually starts server maintenance. This command can be useful if an earlier maintenance attempt failed, and the underlying cause of maintenance failure has been resolved. The server is in an <code>UNDER_MAINTENANCE</code> state while maintenance is in progress. </p> <p> Maintenance can only be started on servers in <code>HEALTHY</code> and <code>UNHEALTHY</code> states. Otherwise, an <code>InvalidStateException</code> is thrown. A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
startMaintenance :: forall eff. StartMaintenanceRequest -> Aff (err :: AWS.RequestError | eff) StartMaintenanceResponse
startMaintenance = AWS.request serviceName "StartMaintenance" 


-- | <p> Updates settings for a server. </p> <p> This operation is synchronous. </p>
updateServer :: forall eff. UpdateServerRequest -> Aff (err :: AWS.RequestError | eff) UpdateServerResponse
updateServer = AWS.request serviceName "UpdateServer" 


-- | <p> Updates engine-specific attributes on a specified server. The server enters the <code>MODIFYING</code> state when this operation is in progress. Only one update can occur at a time. You can use this command to reset a Chef server's private key (<code>CHEF_PIVOTAL_KEY</code>), a Chef server's admin password (<code>CHEF_DELIVERY_ADMIN_PASSWORD</code>), or a Puppet server's admin password (<code>PUPPET_ADMIN_PASSWORD</code>). </p> <p> This operation is asynchronous. </p> <p> This operation can only be called for servers in <code>HEALTHY</code> or <code>UNHEALTHY</code> states. Otherwise, an <code>InvalidStateException</code> is raised. A <code>ResourceNotFoundException</code> is thrown when the server does not exist. A <code>ValidationException</code> is raised when parameters of the request are not valid. </p>
updateServerEngineAttributes :: forall eff. UpdateServerEngineAttributesRequest -> Aff (err :: AWS.RequestError | eff) UpdateServerEngineAttributesResponse
updateServerEngineAttributes = AWS.request serviceName "UpdateServerEngineAttributes" 


-- | <p>Stores account attributes. </p>
newtype AccountAttribute = AccountAttribute 
  { "Name" :: NullOrUndefined (String)
  , "Maximum" :: NullOrUndefined (Int)
  , "Used" :: NullOrUndefined (Int)
  }


-- | <p> A list of individual account attributes. </p>
newtype AccountAttributes = AccountAttributes (Array AccountAttribute)


newtype AssociateNodeRequest = AssociateNodeRequest 
  { "ServerName" :: (ServerName)
  , "NodeName" :: (NodeName)
  , "EngineAttributes" :: (EngineAttributes)
  }


newtype AssociateNodeResponse = AssociateNodeResponse 
  { "NodeAssociationStatusToken" :: NullOrUndefined (NodeAssociationStatusToken)
  }


newtype AttributeName = AttributeName String


newtype AttributeValue = AttributeValue String


-- | <p>Describes a single backup. </p>
newtype Backup = Backup 
  { "BackupArn" :: NullOrUndefined (String)
  , "BackupId" :: NullOrUndefined (BackupId)
  , "BackupType" :: NullOrUndefined (BackupType)
  , "CreatedAt" :: NullOrUndefined (Number)
  , "Description" :: NullOrUndefined (String)
  , "Engine" :: NullOrUndefined (String)
  , "EngineModel" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "InstanceProfileArn" :: NullOrUndefined (String)
  , "InstanceType" :: NullOrUndefined (String)
  , "KeyPair" :: NullOrUndefined (String)
  , "PreferredBackupWindow" :: NullOrUndefined (TimeWindowDefinition)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (TimeWindowDefinition)
  , "S3DataSize" :: NullOrUndefined (Int)
  , "S3DataUrl" :: NullOrUndefined (String)
  , "S3LogUrl" :: NullOrUndefined (String)
  , "SecurityGroupIds" :: NullOrUndefined (Strings)
  , "ServerName" :: NullOrUndefined (ServerName)
  , "ServiceRoleArn" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (BackupStatus)
  , "StatusDescription" :: NullOrUndefined (String)
  , "SubnetIds" :: NullOrUndefined (Strings)
  , "ToolsVersion" :: NullOrUndefined (String)
  , "UserArn" :: NullOrUndefined (String)
  }


newtype BackupId = BackupId String


newtype BackupRetentionCountDefinition = BackupRetentionCountDefinition Int


newtype BackupStatus = BackupStatus String


newtype BackupType = BackupType String


newtype Backups = Backups (Array Backup)


newtype CreateBackupRequest = CreateBackupRequest 
  { "ServerName" :: (ServerName)
  , "Description" :: NullOrUndefined (String)
  }


newtype CreateBackupResponse = CreateBackupResponse 
  { "Backup" :: NullOrUndefined (Backup)
  }


newtype CreateServerRequest = CreateServerRequest 
  { "AssociatePublicIpAddress" :: NullOrUndefined (Boolean)
  , "DisableAutomatedBackup" :: NullOrUndefined (Boolean)
  , "Engine" :: NullOrUndefined (String)
  , "EngineModel" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "EngineAttributes" :: NullOrUndefined (EngineAttributes)
  , "BackupRetentionCount" :: NullOrUndefined (BackupRetentionCountDefinition)
  , "ServerName" :: (ServerName)
  , "InstanceProfileArn" :: (InstanceProfileArn)
  , "InstanceType" :: (String)
  , "KeyPair" :: NullOrUndefined (KeyPair)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (TimeWindowDefinition)
  , "PreferredBackupWindow" :: NullOrUndefined (TimeWindowDefinition)
  , "SecurityGroupIds" :: NullOrUndefined (Strings)
  , "ServiceRoleArn" :: (ServiceRoleArn)
  , "SubnetIds" :: NullOrUndefined (Strings)
  , "BackupId" :: NullOrUndefined (BackupId)
  }


newtype CreateServerResponse = CreateServerResponse 
  { "Server" :: NullOrUndefined (Server)
  }


newtype DeleteBackupRequest = DeleteBackupRequest 
  { "BackupId" :: (BackupId)
  }


newtype DeleteBackupResponse = DeleteBackupResponse 
  { 
  }


newtype DeleteServerRequest = DeleteServerRequest 
  { "ServerName" :: (ServerName)
  }


newtype DeleteServerResponse = DeleteServerResponse 
  { 
  }


newtype DescribeAccountAttributesRequest = DescribeAccountAttributesRequest 
  { 
  }


newtype DescribeAccountAttributesResponse = DescribeAccountAttributesResponse 
  { "Attributes" :: NullOrUndefined (AccountAttributes)
  }


newtype DescribeBackupsRequest = DescribeBackupsRequest 
  { "BackupId" :: NullOrUndefined (BackupId)
  , "ServerName" :: NullOrUndefined (ServerName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype DescribeBackupsResponse = DescribeBackupsResponse 
  { "Backups" :: NullOrUndefined (Backups)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype DescribeEventsRequest = DescribeEventsRequest 
  { "ServerName" :: (ServerName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype DescribeEventsResponse = DescribeEventsResponse 
  { "ServerEvents" :: NullOrUndefined (ServerEvents)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype DescribeNodeAssociationStatusRequest = DescribeNodeAssociationStatusRequest 
  { "NodeAssociationStatusToken" :: (NodeAssociationStatusToken)
  , "ServerName" :: (ServerName)
  }


newtype DescribeNodeAssociationStatusResponse = DescribeNodeAssociationStatusResponse 
  { "NodeAssociationStatus" :: NullOrUndefined (NodeAssociationStatus)
  , "EngineAttributes" :: NullOrUndefined (EngineAttributes)
  }


newtype DescribeServersRequest = DescribeServersRequest 
  { "ServerName" :: NullOrUndefined (ServerName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype DescribeServersResponse = DescribeServersResponse 
  { "Servers" :: NullOrUndefined (Servers)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype DisassociateNodeRequest = DisassociateNodeRequest 
  { "ServerName" :: (ServerName)
  , "NodeName" :: (NodeName)
  , "EngineAttributes" :: NullOrUndefined (EngineAttributes)
  }


newtype DisassociateNodeResponse = DisassociateNodeResponse 
  { "NodeAssociationStatusToken" :: NullOrUndefined (NodeAssociationStatusToken)
  }


-- | <p>A name and value pair that is specific to the engine of the server. </p>
newtype EngineAttribute = EngineAttribute 
  { "Name" :: NullOrUndefined (EngineAttributeName)
  , "Value" :: NullOrUndefined (EngineAttributeValue)
  }


newtype EngineAttributeName = EngineAttributeName String


newtype EngineAttributeValue = EngineAttributeValue String


newtype EngineAttributes = EngineAttributes (Array EngineAttribute)


newtype InstanceProfileArn = InstanceProfileArn String


-- | <p>This occurs when the provided nextToken is not valid. </p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The resource is in a state that does not allow you to perform a specified action. </p>
newtype InvalidStateException = InvalidStateException 
  { "Message" :: NullOrUndefined (String)
  }


newtype KeyPair = KeyPair String


-- | <p>The limit of servers or backups has been reached. </p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }


newtype MaintenanceStatus = MaintenanceStatus String


newtype MaxResults = MaxResults Int


newtype NextToken = NextToken String


-- | <p>The status of the association or disassociation request. </p> <p class="title"> <b>Possible values:</b> </p> <ul> <li> <p> <code>SUCCESS</code>: The association or disassociation succeeded. </p> </li> <li> <p> <code>FAILED</code>: The association or disassociation failed. </p> </li> <li> <p> <code>IN_PROGRESS</code>: The association or disassociation is still in progress. </p> </li> </ul>
newtype NodeAssociationStatus = NodeAssociationStatus String


newtype NodeAssociationStatusToken = NodeAssociationStatusToken String


-- | <p>The node name that is used by <code>chef-client</code> or <code>puppet-agent</code>for a new node. We recommend to use a unique FQDN as hostname. For more information, see the <a href="http://docs.aws.amazon.com/https:/docs.chef.io/nodes.html#about-node-names">Chef</a> or <a href="http://docs.aws.amazon.com/https:/docs.puppet.com/puppet/4.10/man/agent.html">Puppet</a> documentation. </p>
newtype NodeName = NodeName String


-- | <p>The requested resource cannot be created because it already exists. </p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The requested resource does not exist, or access was denied. </p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message" :: NullOrUndefined (String)
  }


newtype RestoreServerRequest = RestoreServerRequest 
  { "BackupId" :: (BackupId)
  , "ServerName" :: (ServerName)
  , "InstanceType" :: NullOrUndefined (String)
  , "KeyPair" :: NullOrUndefined (KeyPair)
  }


newtype RestoreServerResponse = RestoreServerResponse 
  { 
  }


-- | <p>Describes a configuration management server. </p>
newtype Server = Server 
  { "AssociatePublicIpAddress" :: NullOrUndefined (Boolean)
  , "BackupRetentionCount" :: NullOrUndefined (Int)
  , "ServerName" :: NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined (Number)
  , "CloudFormationStackArn" :: NullOrUndefined (String)
  , "DisableAutomatedBackup" :: NullOrUndefined (Boolean)
  , "Endpoint" :: NullOrUndefined (String)
  , "Engine" :: NullOrUndefined (String)
  , "EngineModel" :: NullOrUndefined (String)
  , "EngineAttributes" :: NullOrUndefined (EngineAttributes)
  , "EngineVersion" :: NullOrUndefined (String)
  , "InstanceProfileArn" :: NullOrUndefined (String)
  , "InstanceType" :: NullOrUndefined (String)
  , "KeyPair" :: NullOrUndefined (String)
  , "MaintenanceStatus" :: NullOrUndefined (MaintenanceStatus)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (TimeWindowDefinition)
  , "PreferredBackupWindow" :: NullOrUndefined (TimeWindowDefinition)
  , "SecurityGroupIds" :: NullOrUndefined (Strings)
  , "ServiceRoleArn" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (ServerStatus)
  , "StatusReason" :: NullOrUndefined (String)
  , "SubnetIds" :: NullOrUndefined (Strings)
  , "ServerArn" :: NullOrUndefined (String)
  }


-- | <p>An event that is related to the server, such as the start of maintenance or backup. </p>
newtype ServerEvent = ServerEvent 
  { "CreatedAt" :: NullOrUndefined (Number)
  , "ServerName" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  , "LogUrl" :: NullOrUndefined (String)
  }


newtype ServerEvents = ServerEvents (Array ServerEvent)


newtype ServerName = ServerName String


newtype ServerStatus = ServerStatus String


newtype Servers = Servers (Array Server)


newtype ServiceRoleArn = ServiceRoleArn String


newtype StartMaintenanceRequest = StartMaintenanceRequest 
  { "ServerName" :: (ServerName)
  , "EngineAttributes" :: NullOrUndefined (EngineAttributes)
  }


newtype StartMaintenanceResponse = StartMaintenanceResponse 
  { "Server" :: NullOrUndefined (Server)
  }


newtype Strings = Strings (Array String)


-- | <p> <code>DDD:HH:MM</code> (weekly start time) or <code>HH:MM</code> (daily start time). </p> <p> Time windows always use coordinated universal time (UTC). Valid strings for day of week (<code>DDD</code>) are: <code>Mon</code>, <code>Tue</code>, <code>Wed</code>, <code>Thr</code>, <code>Fri</code>, <code>Sat</code>, or <code>Sun</code>.</p>
newtype TimeWindowDefinition = TimeWindowDefinition String


newtype UpdateServerEngineAttributesRequest = UpdateServerEngineAttributesRequest 
  { "ServerName" :: (ServerName)
  , "AttributeName" :: (AttributeName)
  , "AttributeValue" :: NullOrUndefined (AttributeValue)
  }


newtype UpdateServerEngineAttributesResponse = UpdateServerEngineAttributesResponse 
  { "Server" :: NullOrUndefined (Server)
  }


newtype UpdateServerRequest = UpdateServerRequest 
  { "DisableAutomatedBackup" :: NullOrUndefined (Boolean)
  , "BackupRetentionCount" :: NullOrUndefined (Int)
  , "ServerName" :: (ServerName)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (TimeWindowDefinition)
  , "PreferredBackupWindow" :: NullOrUndefined (TimeWindowDefinition)
  }


newtype UpdateServerResponse = UpdateServerResponse 
  { "Server" :: NullOrUndefined (Server)
  }


-- | <p>One or more of the provided request parameters are not valid. </p>
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined (String)
  }
