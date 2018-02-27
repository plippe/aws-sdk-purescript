

-- | <fullname>AWS Database Migration Service</fullname> <p>AWS Database Migration Service (AWS DMS) can migrate your data to and from the most widely used commercial and open-source databases such as Oracle, PostgreSQL, Microsoft SQL Server, Amazon Redshift, MariaDB, Amazon Aurora, MySQL, and SAP Adaptive Server Enterprise (ASE). The service supports homogeneous migrations such as Oracle to Oracle, as well as heterogeneous migrations between different database platforms, such as Oracle to MySQL or SQL Server to PostgreSQL.</p> <p>For more information about AWS DMS, see the AWS DMS user guide at <a href="http://docs.aws.amazon.com/dms/latest/userguide/Welcome.html"> What Is AWS Database Migration Service? </a> </p>
module AWS.DMS where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "DMS" :: String


-- | <p>Adds metadata tags to a DMS resource, including replication instance, endpoint, security group, and migration task. These tags can also be used with cost allocation reporting to track cost associated with DMS resources, or used in a Condition statement in an IAM policy for DMS.</p>
addTagsToResource :: forall eff. AddTagsToResourceMessage -> Aff (err :: AWS.RequestError | eff) AddTagsToResourceResponse
addTagsToResource = AWS.request serviceName "AddTagsToResource" 


-- | <p>Creates an endpoint using the provided settings.</p>
createEndpoint :: forall eff. CreateEndpointMessage -> Aff (err :: AWS.RequestError | eff) CreateEndpointResponse
createEndpoint = AWS.request serviceName "CreateEndpoint" 


-- | <p> Creates an AWS DMS event notification subscription. </p> <p>You can specify the type of source (<code>SourceType</code>) you want to be notified of, provide a list of AWS DMS source IDs (<code>SourceIds</code>) that triggers the events, and provide a list of event categories (<code>EventCategories</code>) for events you want to be notified of. If you specify both the <code>SourceType</code> and <code>SourceIds</code>, such as <code>SourceType = replication-instance</code> and <code>SourceIdentifier = my-replinstance</code>, you will be notified of all the replication instance events for the specified source. If you specify a <code>SourceType</code> but don't specify a <code>SourceIdentifier</code>, you receive notice of the events for that source type for all your AWS DMS sources. If you don't specify either <code>SourceType</code> nor <code>SourceIdentifier</code>, you will be notified of events generated from all AWS DMS sources belonging to your customer account.</p> <p>For more information about AWS DMS events, see <a href="http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html"> Working with Events and Notifications </a> in the AWS Database MIgration Service User Guide.</p>
createEventSubscription :: forall eff. CreateEventSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) CreateEventSubscriptionResponse
createEventSubscription = AWS.request serviceName "CreateEventSubscription" 


-- | <p>Creates the replication instance using the specified parameters.</p>
createReplicationInstance :: forall eff. CreateReplicationInstanceMessage -> Aff (err :: AWS.RequestError | eff) CreateReplicationInstanceResponse
createReplicationInstance = AWS.request serviceName "CreateReplicationInstance" 


-- | <p>Creates a replication subnet group given a list of the subnet IDs in a VPC.</p>
createReplicationSubnetGroup :: forall eff. CreateReplicationSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateReplicationSubnetGroupResponse
createReplicationSubnetGroup = AWS.request serviceName "CreateReplicationSubnetGroup" 


-- | <p>Creates a replication task using the specified parameters.</p>
createReplicationTask :: forall eff. CreateReplicationTaskMessage -> Aff (err :: AWS.RequestError | eff) CreateReplicationTaskResponse
createReplicationTask = AWS.request serviceName "CreateReplicationTask" 


-- | <p>Deletes the specified certificate. </p>
deleteCertificate :: forall eff. DeleteCertificateMessage -> Aff (err :: AWS.RequestError | eff) DeleteCertificateResponse
deleteCertificate = AWS.request serviceName "DeleteCertificate" 


-- | <p>Deletes the specified endpoint.</p> <note> <p>All tasks associated with the endpoint must be deleted before you can delete the endpoint.</p> </note> <p/>
deleteEndpoint :: forall eff. DeleteEndpointMessage -> Aff (err :: AWS.RequestError | eff) DeleteEndpointResponse
deleteEndpoint = AWS.request serviceName "DeleteEndpoint" 


-- | <p> Deletes an AWS DMS event subscription. </p>
deleteEventSubscription :: forall eff. DeleteEventSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) DeleteEventSubscriptionResponse
deleteEventSubscription = AWS.request serviceName "DeleteEventSubscription" 


-- | <p>Deletes the specified replication instance.</p> <note> <p>You must delete any migration tasks that are associated with the replication instance before you can delete it.</p> </note> <p/>
deleteReplicationInstance :: forall eff. DeleteReplicationInstanceMessage -> Aff (err :: AWS.RequestError | eff) DeleteReplicationInstanceResponse
deleteReplicationInstance = AWS.request serviceName "DeleteReplicationInstance" 


-- | <p>Deletes a subnet group.</p>
deleteReplicationSubnetGroup :: forall eff. DeleteReplicationSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) DeleteReplicationSubnetGroupResponse
deleteReplicationSubnetGroup = AWS.request serviceName "DeleteReplicationSubnetGroup" 


-- | <p>Deletes the specified replication task.</p>
deleteReplicationTask :: forall eff. DeleteReplicationTaskMessage -> Aff (err :: AWS.RequestError | eff) DeleteReplicationTaskResponse
deleteReplicationTask = AWS.request serviceName "DeleteReplicationTask" 


-- | <p>Lists all of the AWS DMS attributes for a customer account. The attributes include AWS DMS quotas for the account, such as the number of replication instances allowed. The description for a quota includes the quota name, current usage toward that quota, and the quota's maximum value.</p> <p>This command does not take any parameters.</p>
describeAccountAttributes :: forall eff. DescribeAccountAttributesMessage -> Aff (err :: AWS.RequestError | eff) DescribeAccountAttributesResponse
describeAccountAttributes = AWS.request serviceName "DescribeAccountAttributes" 


-- | <p>Provides a description of the certificate.</p>
describeCertificates :: forall eff. DescribeCertificatesMessage -> Aff (err :: AWS.RequestError | eff) DescribeCertificatesResponse
describeCertificates = AWS.request serviceName "DescribeCertificates" 


-- | <p>Describes the status of the connections that have been made between the replication instance and an endpoint. Connections are created when you test an endpoint.</p>
describeConnections :: forall eff. DescribeConnectionsMessage -> Aff (err :: AWS.RequestError | eff) DescribeConnectionsResponse
describeConnections = AWS.request serviceName "DescribeConnections" 


-- | <p>Returns information about the type of endpoints available.</p>
describeEndpointTypes :: forall eff. DescribeEndpointTypesMessage -> Aff (err :: AWS.RequestError | eff) DescribeEndpointTypesResponse
describeEndpointTypes = AWS.request serviceName "DescribeEndpointTypes" 


-- | <p>Returns information about the endpoints for your account in the current region.</p>
describeEndpoints :: forall eff. DescribeEndpointsMessage -> Aff (err :: AWS.RequestError | eff) DescribeEndpointsResponse
describeEndpoints = AWS.request serviceName "DescribeEndpoints" 


-- | <p>Lists categories for all event source types, or, if specified, for a specified source type. You can see a list of the event categories and source types in <a href="http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html"> Working with Events and Notifications </a> in the AWS Database Migration Service User Guide. </p>
describeEventCategories :: forall eff. DescribeEventCategoriesMessage -> Aff (err :: AWS.RequestError | eff) DescribeEventCategoriesResponse
describeEventCategories = AWS.request serviceName "DescribeEventCategories" 


-- | <p>Lists all the event subscriptions for a customer account. The description of a subscription includes <code>SubscriptionName</code>, <code>SNSTopicARN</code>, <code>CustomerID</code>, <code>SourceType</code>, <code>SourceID</code>, <code>CreationTime</code>, and <code>Status</code>. </p> <p>If you specify <code>SubscriptionName</code>, this action lists the description for that subscription.</p>
describeEventSubscriptions :: forall eff. DescribeEventSubscriptionsMessage -> Aff (err :: AWS.RequestError | eff) DescribeEventSubscriptionsResponse
describeEventSubscriptions = AWS.request serviceName "DescribeEventSubscriptions" 


-- | <p> Lists events for a given source identifier and source type. You can also specify a start and end time. For more information on AWS DMS events, see <a href="http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html"> Working with Events and Notifications </a>. </p>
describeEvents :: forall eff. DescribeEventsMessage -> Aff (err :: AWS.RequestError | eff) DescribeEventsResponse
describeEvents = AWS.request serviceName "DescribeEvents" 


-- | <p>Returns information about the replication instance types that can be created in the specified region.</p>
describeOrderableReplicationInstances :: forall eff. DescribeOrderableReplicationInstancesMessage -> Aff (err :: AWS.RequestError | eff) DescribeOrderableReplicationInstancesResponse
describeOrderableReplicationInstances = AWS.request serviceName "DescribeOrderableReplicationInstances" 


-- | <p>Returns the status of the RefreshSchemas operation.</p>
describeRefreshSchemasStatus :: forall eff. DescribeRefreshSchemasStatusMessage -> Aff (err :: AWS.RequestError | eff) DescribeRefreshSchemasStatusResponse
describeRefreshSchemasStatus = AWS.request serviceName "DescribeRefreshSchemasStatus" 


-- | <p>Returns information about the task logs for the specified task.</p>
describeReplicationInstanceTaskLogs :: forall eff. DescribeReplicationInstanceTaskLogsMessage -> Aff (err :: AWS.RequestError | eff) DescribeReplicationInstanceTaskLogsResponse
describeReplicationInstanceTaskLogs = AWS.request serviceName "DescribeReplicationInstanceTaskLogs" 


-- | <p>Returns information about replication instances for your account in the current region.</p>
describeReplicationInstances :: forall eff. DescribeReplicationInstancesMessage -> Aff (err :: AWS.RequestError | eff) DescribeReplicationInstancesResponse
describeReplicationInstances = AWS.request serviceName "DescribeReplicationInstances" 


-- | <p>Returns information about the replication subnet groups.</p>
describeReplicationSubnetGroups :: forall eff. DescribeReplicationSubnetGroupsMessage -> Aff (err :: AWS.RequestError | eff) DescribeReplicationSubnetGroupsResponse
describeReplicationSubnetGroups = AWS.request serviceName "DescribeReplicationSubnetGroups" 


-- | <p>Returns the task assessment results from Amazon S3. This action always returns the latest results.</p>
describeReplicationTaskAssessmentResults :: forall eff. DescribeReplicationTaskAssessmentResultsMessage -> Aff (err :: AWS.RequestError | eff) DescribeReplicationTaskAssessmentResultsResponse
describeReplicationTaskAssessmentResults = AWS.request serviceName "DescribeReplicationTaskAssessmentResults" 


-- | <p>Returns information about replication tasks for your account in the current region.</p>
describeReplicationTasks :: forall eff. DescribeReplicationTasksMessage -> Aff (err :: AWS.RequestError | eff) DescribeReplicationTasksResponse
describeReplicationTasks = AWS.request serviceName "DescribeReplicationTasks" 


-- | <p>Returns information about the schema for the specified endpoint.</p> <p/>
describeSchemas :: forall eff. DescribeSchemasMessage -> Aff (err :: AWS.RequestError | eff) DescribeSchemasResponse
describeSchemas = AWS.request serviceName "DescribeSchemas" 


-- | <p>Returns table statistics on the database migration task, including table name, rows inserted, rows updated, and rows deleted.</p> <p>Note that the "last updated" column the DMS console only indicates the time that AWS DMS last updated the table statistics record for a table. It does not indicate the time of the last update to the table.</p>
describeTableStatistics :: forall eff. DescribeTableStatisticsMessage -> Aff (err :: AWS.RequestError | eff) DescribeTableStatisticsResponse
describeTableStatistics = AWS.request serviceName "DescribeTableStatistics" 


-- | <p>Uploads the specified certificate.</p>
importCertificate :: forall eff. ImportCertificateMessage -> Aff (err :: AWS.RequestError | eff) ImportCertificateResponse
importCertificate = AWS.request serviceName "ImportCertificate" 


-- | <p>Lists all tags for an AWS DMS resource.</p>
listTagsForResource :: forall eff. ListTagsForResourceMessage -> Aff (err :: AWS.RequestError | eff) ListTagsForResourceResponse
listTagsForResource = AWS.request serviceName "ListTagsForResource" 


-- | <p>Modifies the specified endpoint.</p>
modifyEndpoint :: forall eff. ModifyEndpointMessage -> Aff (err :: AWS.RequestError | eff) ModifyEndpointResponse
modifyEndpoint = AWS.request serviceName "ModifyEndpoint" 


-- | <p>Modifies an existing AWS DMS event notification subscription. </p>
modifyEventSubscription :: forall eff. ModifyEventSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) ModifyEventSubscriptionResponse
modifyEventSubscription = AWS.request serviceName "ModifyEventSubscription" 


-- | <p>Modifies the replication instance to apply new settings. You can change one or more parameters by specifying these parameters and the new values in the request.</p> <p>Some settings are applied during the maintenance window.</p> <p/>
modifyReplicationInstance :: forall eff. ModifyReplicationInstanceMessage -> Aff (err :: AWS.RequestError | eff) ModifyReplicationInstanceResponse
modifyReplicationInstance = AWS.request serviceName "ModifyReplicationInstance" 


-- | <p>Modifies the settings for the specified replication subnet group.</p>
modifyReplicationSubnetGroup :: forall eff. ModifyReplicationSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) ModifyReplicationSubnetGroupResponse
modifyReplicationSubnetGroup = AWS.request serviceName "ModifyReplicationSubnetGroup" 


-- | <p>Modifies the specified replication task.</p> <p>You can't modify the task endpoints. The task must be stopped before you can modify it. </p> <p>For more information about AWS DMS tasks, see the AWS DMS user guide at <a href="http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html"> Working with Migration Tasks </a> </p>
modifyReplicationTask :: forall eff. ModifyReplicationTaskMessage -> Aff (err :: AWS.RequestError | eff) ModifyReplicationTaskResponse
modifyReplicationTask = AWS.request serviceName "ModifyReplicationTask" 


-- | <p>Reboots a replication instance. Rebooting results in a momentary outage, until the replication instance becomes available again.</p>
rebootReplicationInstance :: forall eff. RebootReplicationInstanceMessage -> Aff (err :: AWS.RequestError | eff) RebootReplicationInstanceResponse
rebootReplicationInstance = AWS.request serviceName "RebootReplicationInstance" 


-- | <p>Populates the schema for the specified endpoint. This is an asynchronous operation and can take several minutes. You can check the status of this operation by calling the DescribeRefreshSchemasStatus operation.</p>
refreshSchemas :: forall eff. RefreshSchemasMessage -> Aff (err :: AWS.RequestError | eff) RefreshSchemasResponse
refreshSchemas = AWS.request serviceName "RefreshSchemas" 


-- | <p>Reloads the target database table with the source data. </p>
reloadTables :: forall eff. ReloadTablesMessage -> Aff (err :: AWS.RequestError | eff) ReloadTablesResponse
reloadTables = AWS.request serviceName "ReloadTables" 


-- | <p>Removes metadata tags from a DMS resource.</p>
removeTagsFromResource :: forall eff. RemoveTagsFromResourceMessage -> Aff (err :: AWS.RequestError | eff) RemoveTagsFromResourceResponse
removeTagsFromResource = AWS.request serviceName "RemoveTagsFromResource" 


-- | <p>Starts the replication task.</p> <p>For more information about AWS DMS tasks, see the AWS DMS user guide at <a href="http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html"> Working with Migration Tasks </a> </p>
startReplicationTask :: forall eff. StartReplicationTaskMessage -> Aff (err :: AWS.RequestError | eff) StartReplicationTaskResponse
startReplicationTask = AWS.request serviceName "StartReplicationTask" 


-- | <p> Starts the replication task assessment for unsupported data types in the source database. </p>
startReplicationTaskAssessment :: forall eff. StartReplicationTaskAssessmentMessage -> Aff (err :: AWS.RequestError | eff) StartReplicationTaskAssessmentResponse
startReplicationTaskAssessment = AWS.request serviceName "StartReplicationTaskAssessment" 


-- | <p>Stops the replication task.</p> <p/>
stopReplicationTask :: forall eff. StopReplicationTaskMessage -> Aff (err :: AWS.RequestError | eff) StopReplicationTaskResponse
stopReplicationTask = AWS.request serviceName "StopReplicationTask" 


-- | <p>Tests the connection between the replication instance and the endpoint.</p>
testConnection :: forall eff. TestConnectionMessage -> Aff (err :: AWS.RequestError | eff) TestConnectionResponse
testConnection = AWS.request serviceName "TestConnection" 


-- | <p>AWS DMS was denied access to the endpoint.</p>
newtype AccessDeniedFault = AccessDeniedFault 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeAccessDeniedFault :: Newtype AccessDeniedFault _


-- | <p>Describes a quota for an AWS account, for example, the number of replication instances allowed.</p>
newtype AccountQuota = AccountQuota 
  { "AccountQuotaName" :: NullOrUndefined (String)
  , "Used" :: NullOrUndefined (Number)
  , "Max" :: NullOrUndefined (Number)
  }
derive instance newtypeAccountQuota :: Newtype AccountQuota _


newtype AccountQuotaList = AccountQuotaList (Array AccountQuota)
derive instance newtypeAccountQuotaList :: Newtype AccountQuotaList _


-- | <p/>
newtype AddTagsToResourceMessage = AddTagsToResourceMessage 
  { "ResourceArn" :: (String)
  , "Tags" :: (TagList)
  }
derive instance newtypeAddTagsToResourceMessage :: Newtype AddTagsToResourceMessage _


-- | <p/>
newtype AddTagsToResourceResponse = AddTagsToResourceResponse 
  { 
  }
derive instance newtypeAddTagsToResourceResponse :: Newtype AddTagsToResourceResponse _


newtype AuthMechanismValue = AuthMechanismValue String
derive instance newtypeAuthMechanismValue :: Newtype AuthMechanismValue _


newtype AuthTypeValue = AuthTypeValue String
derive instance newtypeAuthTypeValue :: Newtype AuthTypeValue _


-- | <p/>
newtype AvailabilityZone = AvailabilityZone 
  { "Name" :: NullOrUndefined (String)
  }
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _


newtype BooleanOptional = BooleanOptional Boolean
derive instance newtypeBooleanOptional :: Newtype BooleanOptional _


-- | <p>The SSL certificate that can be used to encrypt connections between the endpoints and the replication instance.</p>
newtype Certificate = Certificate 
  { "CertificateIdentifier" :: NullOrUndefined (String)
  , "CertificateCreationDate" :: NullOrUndefined (TStamp)
  , "CertificatePem" :: NullOrUndefined (String)
  , "CertificateWallet" :: NullOrUndefined (CertificateWallet)
  , "CertificateArn" :: NullOrUndefined (String)
  , "CertificateOwner" :: NullOrUndefined (String)
  , "ValidFromDate" :: NullOrUndefined (TStamp)
  , "ValidToDate" :: NullOrUndefined (TStamp)
  , "SigningAlgorithm" :: NullOrUndefined (String)
  , "KeyLength" :: NullOrUndefined (IntegerOptional)
  }
derive instance newtypeCertificate :: Newtype Certificate _


newtype CertificateList = CertificateList (Array Certificate)
derive instance newtypeCertificateList :: Newtype CertificateList _


newtype CertificateWallet = CertificateWallet String
derive instance newtypeCertificateWallet :: Newtype CertificateWallet _


newtype CompressionTypeValue = CompressionTypeValue String
derive instance newtypeCompressionTypeValue :: Newtype CompressionTypeValue _


-- | <p/>
newtype Connection = Connection 
  { "ReplicationInstanceArn" :: NullOrUndefined (String)
  , "EndpointArn" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  , "LastFailureMessage" :: NullOrUndefined (String)
  , "EndpointIdentifier" :: NullOrUndefined (String)
  , "ReplicationInstanceIdentifier" :: NullOrUndefined (String)
  }
derive instance newtypeConnection :: Newtype Connection _


newtype ConnectionList = ConnectionList (Array Connection)
derive instance newtypeConnectionList :: Newtype ConnectionList _


-- | <p/>
newtype CreateEndpointMessage = CreateEndpointMessage 
  { "EndpointIdentifier" :: (String)
  , "EndpointType" :: (ReplicationEndpointTypeValue)
  , "EngineName" :: (String)
  , "Username" :: NullOrUndefined (String)
  , "Password" :: NullOrUndefined (SecretString)
  , "ServerName" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "DatabaseName" :: NullOrUndefined (String)
  , "ExtraConnectionAttributes" :: NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined (String)
  , "Tags" :: NullOrUndefined (TagList)
  , "CertificateArn" :: NullOrUndefined (String)
  , "SslMode" :: NullOrUndefined (DmsSslModeValue)
  , "DynamoDbSettings" :: NullOrUndefined (DynamoDbSettings)
  , "S3Settings" :: NullOrUndefined (S3Settings)
  , "MongoDbSettings" :: NullOrUndefined (MongoDbSettings)
  }
derive instance newtypeCreateEndpointMessage :: Newtype CreateEndpointMessage _


-- | <p/>
newtype CreateEndpointResponse = CreateEndpointResponse 
  { "Endpoint" :: NullOrUndefined (Endpoint)
  }
derive instance newtypeCreateEndpointResponse :: Newtype CreateEndpointResponse _


-- | <p/>
newtype CreateEventSubscriptionMessage = CreateEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SnsTopicArn" :: (String)
  , "SourceType" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "SourceIds" :: NullOrUndefined (SourceIdsList)
  , "Enabled" :: NullOrUndefined (BooleanOptional)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateEventSubscriptionMessage :: Newtype CreateEventSubscriptionMessage _


-- | <p/>
newtype CreateEventSubscriptionResponse = CreateEventSubscriptionResponse 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }
derive instance newtypeCreateEventSubscriptionResponse :: Newtype CreateEventSubscriptionResponse _


-- | <p/>
newtype CreateReplicationInstanceMessage = CreateReplicationInstanceMessage 
  { "ReplicationInstanceIdentifier" :: (String)
  , "AllocatedStorage" :: NullOrUndefined (IntegerOptional)
  , "ReplicationInstanceClass" :: (String)
  , "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "ReplicationSubnetGroupIdentifier" :: NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined (BooleanOptional)
  , "EngineVersion" :: NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "Tags" :: NullOrUndefined (TagList)
  , "KmsKeyId" :: NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined (BooleanOptional)
  }
derive instance newtypeCreateReplicationInstanceMessage :: Newtype CreateReplicationInstanceMessage _


-- | <p/>
newtype CreateReplicationInstanceResponse = CreateReplicationInstanceResponse 
  { "ReplicationInstance" :: NullOrUndefined (ReplicationInstance)
  }
derive instance newtypeCreateReplicationInstanceResponse :: Newtype CreateReplicationInstanceResponse _


-- | <p/>
newtype CreateReplicationSubnetGroupMessage = CreateReplicationSubnetGroupMessage 
  { "ReplicationSubnetGroupIdentifier" :: (String)
  , "ReplicationSubnetGroupDescription" :: (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateReplicationSubnetGroupMessage :: Newtype CreateReplicationSubnetGroupMessage _


-- | <p/>
newtype CreateReplicationSubnetGroupResponse = CreateReplicationSubnetGroupResponse 
  { "ReplicationSubnetGroup" :: NullOrUndefined (ReplicationSubnetGroup)
  }
derive instance newtypeCreateReplicationSubnetGroupResponse :: Newtype CreateReplicationSubnetGroupResponse _


-- | <p/>
newtype CreateReplicationTaskMessage = CreateReplicationTaskMessage 
  { "ReplicationTaskIdentifier" :: (String)
  , "SourceEndpointArn" :: (String)
  , "TargetEndpointArn" :: (String)
  , "ReplicationInstanceArn" :: (String)
  , "MigrationType" :: (MigrationTypeValue)
  , "TableMappings" :: (String)
  , "ReplicationTaskSettings" :: NullOrUndefined (String)
  , "CdcStartTime" :: NullOrUndefined (TStamp)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateReplicationTaskMessage :: Newtype CreateReplicationTaskMessage _


-- | <p/>
newtype CreateReplicationTaskResponse = CreateReplicationTaskResponse 
  { "ReplicationTask" :: NullOrUndefined (ReplicationTask)
  }
derive instance newtypeCreateReplicationTaskResponse :: Newtype CreateReplicationTaskResponse _


newtype DeleteCertificateMessage = DeleteCertificateMessage 
  { "CertificateArn" :: (String)
  }
derive instance newtypeDeleteCertificateMessage :: Newtype DeleteCertificateMessage _


newtype DeleteCertificateResponse = DeleteCertificateResponse 
  { "Certificate" :: NullOrUndefined (Certificate)
  }
derive instance newtypeDeleteCertificateResponse :: Newtype DeleteCertificateResponse _


-- | <p/>
newtype DeleteEndpointMessage = DeleteEndpointMessage 
  { "EndpointArn" :: (String)
  }
derive instance newtypeDeleteEndpointMessage :: Newtype DeleteEndpointMessage _


-- | <p/>
newtype DeleteEndpointResponse = DeleteEndpointResponse 
  { "Endpoint" :: NullOrUndefined (Endpoint)
  }
derive instance newtypeDeleteEndpointResponse :: Newtype DeleteEndpointResponse _


-- | <p/>
newtype DeleteEventSubscriptionMessage = DeleteEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  }
derive instance newtypeDeleteEventSubscriptionMessage :: Newtype DeleteEventSubscriptionMessage _


-- | <p/>
newtype DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }
derive instance newtypeDeleteEventSubscriptionResponse :: Newtype DeleteEventSubscriptionResponse _


-- | <p/>
newtype DeleteReplicationInstanceMessage = DeleteReplicationInstanceMessage 
  { "ReplicationInstanceArn" :: (String)
  }
derive instance newtypeDeleteReplicationInstanceMessage :: Newtype DeleteReplicationInstanceMessage _


-- | <p/>
newtype DeleteReplicationInstanceResponse = DeleteReplicationInstanceResponse 
  { "ReplicationInstance" :: NullOrUndefined (ReplicationInstance)
  }
derive instance newtypeDeleteReplicationInstanceResponse :: Newtype DeleteReplicationInstanceResponse _


-- | <p/>
newtype DeleteReplicationSubnetGroupMessage = DeleteReplicationSubnetGroupMessage 
  { "ReplicationSubnetGroupIdentifier" :: (String)
  }
derive instance newtypeDeleteReplicationSubnetGroupMessage :: Newtype DeleteReplicationSubnetGroupMessage _


-- | <p/>
newtype DeleteReplicationSubnetGroupResponse = DeleteReplicationSubnetGroupResponse 
  { 
  }
derive instance newtypeDeleteReplicationSubnetGroupResponse :: Newtype DeleteReplicationSubnetGroupResponse _


-- | <p/>
newtype DeleteReplicationTaskMessage = DeleteReplicationTaskMessage 
  { "ReplicationTaskArn" :: (String)
  }
derive instance newtypeDeleteReplicationTaskMessage :: Newtype DeleteReplicationTaskMessage _


-- | <p/>
newtype DeleteReplicationTaskResponse = DeleteReplicationTaskResponse 
  { "ReplicationTask" :: NullOrUndefined (ReplicationTask)
  }
derive instance newtypeDeleteReplicationTaskResponse :: Newtype DeleteReplicationTaskResponse _


-- | <p/>
newtype DescribeAccountAttributesMessage = DescribeAccountAttributesMessage 
  { 
  }
derive instance newtypeDescribeAccountAttributesMessage :: Newtype DescribeAccountAttributesMessage _


-- | <p/>
newtype DescribeAccountAttributesResponse = DescribeAccountAttributesResponse 
  { "AccountQuotas" :: NullOrUndefined (AccountQuotaList)
  }
derive instance newtypeDescribeAccountAttributesResponse :: Newtype DescribeAccountAttributesResponse _


newtype DescribeCertificatesMessage = DescribeCertificatesMessage 
  { "Filters" :: NullOrUndefined (FilterList)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeCertificatesMessage :: Newtype DescribeCertificatesMessage _


newtype DescribeCertificatesResponse = DescribeCertificatesResponse 
  { "Marker" :: NullOrUndefined (String)
  , "Certificates" :: NullOrUndefined (CertificateList)
  }
derive instance newtypeDescribeCertificatesResponse :: Newtype DescribeCertificatesResponse _


-- | <p/>
newtype DescribeConnectionsMessage = DescribeConnectionsMessage 
  { "Filters" :: NullOrUndefined (FilterList)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeConnectionsMessage :: Newtype DescribeConnectionsMessage _


-- | <p/>
newtype DescribeConnectionsResponse = DescribeConnectionsResponse 
  { "Marker" :: NullOrUndefined (String)
  , "Connections" :: NullOrUndefined (ConnectionList)
  }
derive instance newtypeDescribeConnectionsResponse :: Newtype DescribeConnectionsResponse _


-- | <p/>
newtype DescribeEndpointTypesMessage = DescribeEndpointTypesMessage 
  { "Filters" :: NullOrUndefined (FilterList)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEndpointTypesMessage :: Newtype DescribeEndpointTypesMessage _


-- | <p/>
newtype DescribeEndpointTypesResponse = DescribeEndpointTypesResponse 
  { "Marker" :: NullOrUndefined (String)
  , "SupportedEndpointTypes" :: NullOrUndefined (SupportedEndpointTypeList)
  }
derive instance newtypeDescribeEndpointTypesResponse :: Newtype DescribeEndpointTypesResponse _


-- | <p/>
newtype DescribeEndpointsMessage = DescribeEndpointsMessage 
  { "Filters" :: NullOrUndefined (FilterList)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEndpointsMessage :: Newtype DescribeEndpointsMessage _


-- | <p/>
newtype DescribeEndpointsResponse = DescribeEndpointsResponse 
  { "Marker" :: NullOrUndefined (String)
  , "Endpoints" :: NullOrUndefined (EndpointList)
  }
derive instance newtypeDescribeEndpointsResponse :: Newtype DescribeEndpointsResponse _


-- | <p/>
newtype DescribeEventCategoriesMessage = DescribeEventCategoriesMessage 
  { "SourceType" :: NullOrUndefined (String)
  , "Filters" :: NullOrUndefined (FilterList)
  }
derive instance newtypeDescribeEventCategoriesMessage :: Newtype DescribeEventCategoriesMessage _


-- | <p/>
newtype DescribeEventCategoriesResponse = DescribeEventCategoriesResponse 
  { "EventCategoryGroupList" :: NullOrUndefined (EventCategoryGroupList)
  }
derive instance newtypeDescribeEventCategoriesResponse :: Newtype DescribeEventCategoriesResponse _


-- | <p/>
newtype DescribeEventSubscriptionsMessage = DescribeEventSubscriptionsMessage 
  { "SubscriptionName" :: NullOrUndefined (String)
  , "Filters" :: NullOrUndefined (FilterList)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEventSubscriptionsMessage :: Newtype DescribeEventSubscriptionsMessage _


-- | <p/>
newtype DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse 
  { "Marker" :: NullOrUndefined (String)
  , "EventSubscriptionsList" :: NullOrUndefined (EventSubscriptionsList)
  }
derive instance newtypeDescribeEventSubscriptionsResponse :: Newtype DescribeEventSubscriptionsResponse _


-- | <p/>
newtype DescribeEventsMessage = DescribeEventsMessage 
  { "SourceIdentifier" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (SourceType)
  , "StartTime" :: NullOrUndefined (TStamp)
  , "EndTime" :: NullOrUndefined (TStamp)
  , "Duration" :: NullOrUndefined (IntegerOptional)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "Filters" :: NullOrUndefined (FilterList)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEventsMessage :: Newtype DescribeEventsMessage _


-- | <p/>
newtype DescribeEventsResponse = DescribeEventsResponse 
  { "Marker" :: NullOrUndefined (String)
  , "Events" :: NullOrUndefined (EventList)
  }
derive instance newtypeDescribeEventsResponse :: Newtype DescribeEventsResponse _


-- | <p/>
newtype DescribeOrderableReplicationInstancesMessage = DescribeOrderableReplicationInstancesMessage 
  { "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeOrderableReplicationInstancesMessage :: Newtype DescribeOrderableReplicationInstancesMessage _


-- | <p/>
newtype DescribeOrderableReplicationInstancesResponse = DescribeOrderableReplicationInstancesResponse 
  { "OrderableReplicationInstances" :: NullOrUndefined (OrderableReplicationInstanceList)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeOrderableReplicationInstancesResponse :: Newtype DescribeOrderableReplicationInstancesResponse _


-- | <p/>
newtype DescribeRefreshSchemasStatusMessage = DescribeRefreshSchemasStatusMessage 
  { "EndpointArn" :: (String)
  }
derive instance newtypeDescribeRefreshSchemasStatusMessage :: Newtype DescribeRefreshSchemasStatusMessage _


-- | <p/>
newtype DescribeRefreshSchemasStatusResponse = DescribeRefreshSchemasStatusResponse 
  { "RefreshSchemasStatus" :: NullOrUndefined (RefreshSchemasStatus)
  }
derive instance newtypeDescribeRefreshSchemasStatusResponse :: Newtype DescribeRefreshSchemasStatusResponse _


newtype DescribeReplicationInstanceTaskLogsMessage = DescribeReplicationInstanceTaskLogsMessage 
  { "ReplicationInstanceArn" :: (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeReplicationInstanceTaskLogsMessage :: Newtype DescribeReplicationInstanceTaskLogsMessage _


newtype DescribeReplicationInstanceTaskLogsResponse = DescribeReplicationInstanceTaskLogsResponse 
  { "ReplicationInstanceArn" :: NullOrUndefined (String)
  , "ReplicationInstanceTaskLogs" :: NullOrUndefined (ReplicationInstanceTaskLogsList)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeReplicationInstanceTaskLogsResponse :: Newtype DescribeReplicationInstanceTaskLogsResponse _


-- | <p/>
newtype DescribeReplicationInstancesMessage = DescribeReplicationInstancesMessage 
  { "Filters" :: NullOrUndefined (FilterList)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeReplicationInstancesMessage :: Newtype DescribeReplicationInstancesMessage _


-- | <p/>
newtype DescribeReplicationInstancesResponse = DescribeReplicationInstancesResponse 
  { "Marker" :: NullOrUndefined (String)
  , "ReplicationInstances" :: NullOrUndefined (ReplicationInstanceList)
  }
derive instance newtypeDescribeReplicationInstancesResponse :: Newtype DescribeReplicationInstancesResponse _


-- | <p/>
newtype DescribeReplicationSubnetGroupsMessage = DescribeReplicationSubnetGroupsMessage 
  { "Filters" :: NullOrUndefined (FilterList)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeReplicationSubnetGroupsMessage :: Newtype DescribeReplicationSubnetGroupsMessage _


-- | <p/>
newtype DescribeReplicationSubnetGroupsResponse = DescribeReplicationSubnetGroupsResponse 
  { "Marker" :: NullOrUndefined (String)
  , "ReplicationSubnetGroups" :: NullOrUndefined (ReplicationSubnetGroups)
  }
derive instance newtypeDescribeReplicationSubnetGroupsResponse :: Newtype DescribeReplicationSubnetGroupsResponse _


-- | <p/>
newtype DescribeReplicationTaskAssessmentResultsMessage = DescribeReplicationTaskAssessmentResultsMessage 
  { "ReplicationTaskArn" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeReplicationTaskAssessmentResultsMessage :: Newtype DescribeReplicationTaskAssessmentResultsMessage _


-- | <p/>
newtype DescribeReplicationTaskAssessmentResultsResponse = DescribeReplicationTaskAssessmentResultsResponse 
  { "Marker" :: NullOrUndefined (String)
  , "BucketName" :: NullOrUndefined (String)
  , "ReplicationTaskAssessmentResults" :: NullOrUndefined (ReplicationTaskAssessmentResultList)
  }
derive instance newtypeDescribeReplicationTaskAssessmentResultsResponse :: Newtype DescribeReplicationTaskAssessmentResultsResponse _


-- | <p/>
newtype DescribeReplicationTasksMessage = DescribeReplicationTasksMessage 
  { "Filters" :: NullOrUndefined (FilterList)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeReplicationTasksMessage :: Newtype DescribeReplicationTasksMessage _


-- | <p/>
newtype DescribeReplicationTasksResponse = DescribeReplicationTasksResponse 
  { "Marker" :: NullOrUndefined (String)
  , "ReplicationTasks" :: NullOrUndefined (ReplicationTaskList)
  }
derive instance newtypeDescribeReplicationTasksResponse :: Newtype DescribeReplicationTasksResponse _


-- | <p/>
newtype DescribeSchemasMessage = DescribeSchemasMessage 
  { "EndpointArn" :: (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeSchemasMessage :: Newtype DescribeSchemasMessage _


-- | <p/>
newtype DescribeSchemasResponse = DescribeSchemasResponse 
  { "Marker" :: NullOrUndefined (String)
  , "Schemas" :: NullOrUndefined (SchemaList)
  }
derive instance newtypeDescribeSchemasResponse :: Newtype DescribeSchemasResponse _


-- | <p/>
newtype DescribeTableStatisticsMessage = DescribeTableStatisticsMessage 
  { "ReplicationTaskArn" :: (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "Filters" :: NullOrUndefined (FilterList)
  }
derive instance newtypeDescribeTableStatisticsMessage :: Newtype DescribeTableStatisticsMessage _


-- | <p/>
newtype DescribeTableStatisticsResponse = DescribeTableStatisticsResponse 
  { "ReplicationTaskArn" :: NullOrUndefined (String)
  , "TableStatistics" :: NullOrUndefined (TableStatisticsList)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeTableStatisticsResponse :: Newtype DescribeTableStatisticsResponse _


newtype DmsSslModeValue = DmsSslModeValue String
derive instance newtypeDmsSslModeValue :: Newtype DmsSslModeValue _


-- | <p/>
newtype DynamoDbSettings = DynamoDbSettings 
  { "ServiceAccessRoleArn" :: (String)
  }
derive instance newtypeDynamoDbSettings :: Newtype DynamoDbSettings _


-- | <p/>
newtype Endpoint = Endpoint 
  { "EndpointIdentifier" :: NullOrUndefined (String)
  , "EndpointType" :: NullOrUndefined (ReplicationEndpointTypeValue)
  , "EngineName" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  , "ServerName" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "DatabaseName" :: NullOrUndefined (String)
  , "ExtraConnectionAttributes" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined (String)
  , "EndpointArn" :: NullOrUndefined (String)
  , "CertificateArn" :: NullOrUndefined (String)
  , "SslMode" :: NullOrUndefined (DmsSslModeValue)
  , "ExternalId" :: NullOrUndefined (String)
  , "DynamoDbSettings" :: NullOrUndefined (DynamoDbSettings)
  , "S3Settings" :: NullOrUndefined (S3Settings)
  , "MongoDbSettings" :: NullOrUndefined (MongoDbSettings)
  }
derive instance newtypeEndpoint :: Newtype Endpoint _


newtype EndpointList = EndpointList (Array Endpoint)
derive instance newtypeEndpointList :: Newtype EndpointList _


-- | <p/>
newtype Event = Event 
  { "SourceIdentifier" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (SourceType)
  , "Message" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "Date" :: NullOrUndefined (TStamp)
  }
derive instance newtypeEvent :: Newtype Event _


newtype EventCategoriesList = EventCategoriesList (Array String)
derive instance newtypeEventCategoriesList :: Newtype EventCategoriesList _


-- | <p/>
newtype EventCategoryGroup = EventCategoryGroup 
  { "SourceType" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  }
derive instance newtypeEventCategoryGroup :: Newtype EventCategoryGroup _


newtype EventCategoryGroupList = EventCategoryGroupList (Array EventCategoryGroup)
derive instance newtypeEventCategoryGroupList :: Newtype EventCategoryGroupList _


newtype EventList = EventList (Array Event)
derive instance newtypeEventList :: Newtype EventList _


-- | <p/>
newtype EventSubscription = EventSubscription 
  { "CustomerAwsId" :: NullOrUndefined (String)
  , "CustSubscriptionId" :: NullOrUndefined (String)
  , "SnsTopicArn" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  , "SubscriptionCreationTime" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (String)
  , "SourceIdsList" :: NullOrUndefined (SourceIdsList)
  , "EventCategoriesList" :: NullOrUndefined (EventCategoriesList)
  , "Enabled" :: NullOrUndefined (Boolean)
  }
derive instance newtypeEventSubscription :: Newtype EventSubscription _


newtype EventSubscriptionsList = EventSubscriptionsList (Array EventSubscription)
derive instance newtypeEventSubscriptionsList :: Newtype EventSubscriptionsList _


newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _


-- | <p/>
newtype Filter = Filter 
  { "Name" :: (String)
  , "Values" :: (FilterValueList)
  }
derive instance newtypeFilter :: Newtype Filter _


newtype FilterList = FilterList (Array Filter)
derive instance newtypeFilterList :: Newtype FilterList _


newtype FilterValueList = FilterValueList (Array String)
derive instance newtypeFilterValueList :: Newtype FilterValueList _


newtype ImportCertificateMessage = ImportCertificateMessage 
  { "CertificateIdentifier" :: (String)
  , "CertificatePem" :: NullOrUndefined (String)
  , "CertificateWallet" :: NullOrUndefined (CertificateWallet)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeImportCertificateMessage :: Newtype ImportCertificateMessage _


newtype ImportCertificateResponse = ImportCertificateResponse 
  { "Certificate" :: NullOrUndefined (Certificate)
  }
derive instance newtypeImportCertificateResponse :: Newtype ImportCertificateResponse _


-- | <p>There are not enough resources allocated to the database migration.</p>
newtype InsufficientResourceCapacityFault = InsufficientResourceCapacityFault 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInsufficientResourceCapacityFault :: Newtype InsufficientResourceCapacityFault _


newtype IntegerOptional = IntegerOptional Int
derive instance newtypeIntegerOptional :: Newtype IntegerOptional _


-- | <p>The certificate was not valid.</p>
newtype InvalidCertificateFault = InvalidCertificateFault 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidCertificateFault :: Newtype InvalidCertificateFault _


-- | <p>The resource is in a state that prevents it from being used for database migration.</p>
newtype InvalidResourceStateFault = InvalidResourceStateFault 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidResourceStateFault :: Newtype InvalidResourceStateFault _


-- | <p>The subnet provided is invalid.</p>
newtype InvalidSubnet = InvalidSubnet 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidSubnet :: Newtype InvalidSubnet _


-- | <p>AWS DMS cannot access the KMS key.</p>
newtype KMSKeyNotAccessibleFault = KMSKeyNotAccessibleFault 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeKMSKeyNotAccessibleFault :: Newtype KMSKeyNotAccessibleFault _


newtype KeyList = KeyList (Array String)
derive instance newtypeKeyList :: Newtype KeyList _


-- | <p/>
newtype ListTagsForResourceMessage = ListTagsForResourceMessage 
  { "ResourceArn" :: (String)
  }
derive instance newtypeListTagsForResourceMessage :: Newtype ListTagsForResourceMessage _


-- | <p/>
newtype ListTagsForResourceResponse = ListTagsForResourceResponse 
  { "TagList" :: NullOrUndefined (TagList)
  }
derive instance newtypeListTagsForResourceResponse :: Newtype ListTagsForResourceResponse _


newtype MigrationTypeValue = MigrationTypeValue String
derive instance newtypeMigrationTypeValue :: Newtype MigrationTypeValue _


-- | <p/>
newtype ModifyEndpointMessage = ModifyEndpointMessage 
  { "EndpointArn" :: (String)
  , "EndpointIdentifier" :: NullOrUndefined (String)
  , "EndpointType" :: NullOrUndefined (ReplicationEndpointTypeValue)
  , "EngineName" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  , "Password" :: NullOrUndefined (SecretString)
  , "ServerName" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "DatabaseName" :: NullOrUndefined (String)
  , "ExtraConnectionAttributes" :: NullOrUndefined (String)
  , "CertificateArn" :: NullOrUndefined (String)
  , "SslMode" :: NullOrUndefined (DmsSslModeValue)
  , "DynamoDbSettings" :: NullOrUndefined (DynamoDbSettings)
  , "S3Settings" :: NullOrUndefined (S3Settings)
  , "MongoDbSettings" :: NullOrUndefined (MongoDbSettings)
  }
derive instance newtypeModifyEndpointMessage :: Newtype ModifyEndpointMessage _


-- | <p/>
newtype ModifyEndpointResponse = ModifyEndpointResponse 
  { "Endpoint" :: NullOrUndefined (Endpoint)
  }
derive instance newtypeModifyEndpointResponse :: Newtype ModifyEndpointResponse _


-- | <p/>
newtype ModifyEventSubscriptionMessage = ModifyEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SnsTopicArn" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "Enabled" :: NullOrUndefined (BooleanOptional)
  }
derive instance newtypeModifyEventSubscriptionMessage :: Newtype ModifyEventSubscriptionMessage _


-- | <p/>
newtype ModifyEventSubscriptionResponse = ModifyEventSubscriptionResponse 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }
derive instance newtypeModifyEventSubscriptionResponse :: Newtype ModifyEventSubscriptionResponse _


-- | <p/>
newtype ModifyReplicationInstanceMessage = ModifyReplicationInstanceMessage 
  { "ReplicationInstanceArn" :: (String)
  , "AllocatedStorage" :: NullOrUndefined (IntegerOptional)
  , "ApplyImmediately" :: NullOrUndefined (Boolean)
  , "ReplicationInstanceClass" :: NullOrUndefined (String)
  , "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined (BooleanOptional)
  , "EngineVersion" :: NullOrUndefined (String)
  , "AllowMajorVersionUpgrade" :: NullOrUndefined (Boolean)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "ReplicationInstanceIdentifier" :: NullOrUndefined (String)
  }
derive instance newtypeModifyReplicationInstanceMessage :: Newtype ModifyReplicationInstanceMessage _


-- | <p/>
newtype ModifyReplicationInstanceResponse = ModifyReplicationInstanceResponse 
  { "ReplicationInstance" :: NullOrUndefined (ReplicationInstance)
  }
derive instance newtypeModifyReplicationInstanceResponse :: Newtype ModifyReplicationInstanceResponse _


-- | <p/>
newtype ModifyReplicationSubnetGroupMessage = ModifyReplicationSubnetGroupMessage 
  { "ReplicationSubnetGroupIdentifier" :: (String)
  , "ReplicationSubnetGroupDescription" :: NullOrUndefined (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  }
derive instance newtypeModifyReplicationSubnetGroupMessage :: Newtype ModifyReplicationSubnetGroupMessage _


-- | <p/>
newtype ModifyReplicationSubnetGroupResponse = ModifyReplicationSubnetGroupResponse 
  { "ReplicationSubnetGroup" :: NullOrUndefined (ReplicationSubnetGroup)
  }
derive instance newtypeModifyReplicationSubnetGroupResponse :: Newtype ModifyReplicationSubnetGroupResponse _


-- | <p/>
newtype ModifyReplicationTaskMessage = ModifyReplicationTaskMessage 
  { "ReplicationTaskArn" :: (String)
  , "ReplicationTaskIdentifier" :: NullOrUndefined (String)
  , "MigrationType" :: NullOrUndefined (MigrationTypeValue)
  , "TableMappings" :: NullOrUndefined (String)
  , "ReplicationTaskSettings" :: NullOrUndefined (String)
  , "CdcStartTime" :: NullOrUndefined (TStamp)
  }
derive instance newtypeModifyReplicationTaskMessage :: Newtype ModifyReplicationTaskMessage _


-- | <p/>
newtype ModifyReplicationTaskResponse = ModifyReplicationTaskResponse 
  { "ReplicationTask" :: NullOrUndefined (ReplicationTask)
  }
derive instance newtypeModifyReplicationTaskResponse :: Newtype ModifyReplicationTaskResponse _


-- | <p/>
newtype MongoDbSettings = MongoDbSettings 
  { "Username" :: NullOrUndefined (String)
  , "Password" :: NullOrUndefined (SecretString)
  , "ServerName" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "DatabaseName" :: NullOrUndefined (String)
  , "AuthType" :: NullOrUndefined (AuthTypeValue)
  , "AuthMechanism" :: NullOrUndefined (AuthMechanismValue)
  , "NestingLevel" :: NullOrUndefined (NestingLevelValue)
  , "ExtractDocId" :: NullOrUndefined (String)
  , "DocsToInvestigate" :: NullOrUndefined (String)
  , "AuthSource" :: NullOrUndefined (String)
  }
derive instance newtypeMongoDbSettings :: Newtype MongoDbSettings _


newtype NestingLevelValue = NestingLevelValue String
derive instance newtypeNestingLevelValue :: Newtype NestingLevelValue _


-- | <p/>
newtype OrderableReplicationInstance = OrderableReplicationInstance 
  { "EngineVersion" :: NullOrUndefined (String)
  , "ReplicationInstanceClass" :: NullOrUndefined (String)
  , "StorageType" :: NullOrUndefined (String)
  , "MinAllocatedStorage" :: NullOrUndefined (Int)
  , "MaxAllocatedStorage" :: NullOrUndefined (Int)
  , "DefaultAllocatedStorage" :: NullOrUndefined (Int)
  , "IncludedAllocatedStorage" :: NullOrUndefined (Int)
  }
derive instance newtypeOrderableReplicationInstance :: Newtype OrderableReplicationInstance _


newtype OrderableReplicationInstanceList = OrderableReplicationInstanceList (Array OrderableReplicationInstance)
derive instance newtypeOrderableReplicationInstanceList :: Newtype OrderableReplicationInstanceList _


newtype RebootReplicationInstanceMessage = RebootReplicationInstanceMessage 
  { "ReplicationInstanceArn" :: (String)
  , "ForceFailover" :: NullOrUndefined (BooleanOptional)
  }
derive instance newtypeRebootReplicationInstanceMessage :: Newtype RebootReplicationInstanceMessage _


newtype RebootReplicationInstanceResponse = RebootReplicationInstanceResponse 
  { "ReplicationInstance" :: NullOrUndefined (ReplicationInstance)
  }
derive instance newtypeRebootReplicationInstanceResponse :: Newtype RebootReplicationInstanceResponse _


-- | <p/>
newtype RefreshSchemasMessage = RefreshSchemasMessage 
  { "EndpointArn" :: (String)
  , "ReplicationInstanceArn" :: (String)
  }
derive instance newtypeRefreshSchemasMessage :: Newtype RefreshSchemasMessage _


-- | <p/>
newtype RefreshSchemasResponse = RefreshSchemasResponse 
  { "RefreshSchemasStatus" :: NullOrUndefined (RefreshSchemasStatus)
  }
derive instance newtypeRefreshSchemasResponse :: Newtype RefreshSchemasResponse _


-- | <p/>
newtype RefreshSchemasStatus = RefreshSchemasStatus 
  { "EndpointArn" :: NullOrUndefined (String)
  , "ReplicationInstanceArn" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (RefreshSchemasStatusTypeValue)
  , "LastRefreshDate" :: NullOrUndefined (TStamp)
  , "LastFailureMessage" :: NullOrUndefined (String)
  }
derive instance newtypeRefreshSchemasStatus :: Newtype RefreshSchemasStatus _


newtype RefreshSchemasStatusTypeValue = RefreshSchemasStatusTypeValue String
derive instance newtypeRefreshSchemasStatusTypeValue :: Newtype RefreshSchemasStatusTypeValue _


newtype ReloadTablesMessage = ReloadTablesMessage 
  { "ReplicationTaskArn" :: (String)
  , "TablesToReload" :: (TableListToReload)
  }
derive instance newtypeReloadTablesMessage :: Newtype ReloadTablesMessage _


newtype ReloadTablesResponse = ReloadTablesResponse 
  { "ReplicationTaskArn" :: NullOrUndefined (String)
  }
derive instance newtypeReloadTablesResponse :: Newtype ReloadTablesResponse _


-- | <p/>
newtype RemoveTagsFromResourceMessage = RemoveTagsFromResourceMessage 
  { "ResourceArn" :: (String)
  , "TagKeys" :: (KeyList)
  }
derive instance newtypeRemoveTagsFromResourceMessage :: Newtype RemoveTagsFromResourceMessage _


-- | <p/>
newtype RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse 
  { 
  }
derive instance newtypeRemoveTagsFromResourceResponse :: Newtype RemoveTagsFromResourceResponse _


newtype ReplicationEndpointTypeValue = ReplicationEndpointTypeValue String
derive instance newtypeReplicationEndpointTypeValue :: Newtype ReplicationEndpointTypeValue _


-- | <p/>
newtype ReplicationInstance = ReplicationInstance 
  { "ReplicationInstanceIdentifier" :: NullOrUndefined (String)
  , "ReplicationInstanceClass" :: NullOrUndefined (String)
  , "ReplicationInstanceStatus" :: NullOrUndefined (String)
  , "AllocatedStorage" :: NullOrUndefined (Int)
  , "InstanceCreateTime" :: NullOrUndefined (TStamp)
  , "VpcSecurityGroups" :: NullOrUndefined (VpcSecurityGroupMembershipList)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "ReplicationSubnetGroup" :: NullOrUndefined (ReplicationSubnetGroup)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "PendingModifiedValues" :: NullOrUndefined (ReplicationPendingModifiedValues)
  , "MultiAZ" :: NullOrUndefined (Boolean)
  , "EngineVersion" :: NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean)
  , "KmsKeyId" :: NullOrUndefined (String)
  , "ReplicationInstanceArn" :: NullOrUndefined (String)
  , "ReplicationInstancePublicIpAddress" :: NullOrUndefined (String)
  , "ReplicationInstancePrivateIpAddress" :: NullOrUndefined (String)
  , "ReplicationInstancePublicIpAddresses" :: NullOrUndefined (ReplicationInstancePublicIpAddressList)
  , "ReplicationInstancePrivateIpAddresses" :: NullOrUndefined (ReplicationInstancePrivateIpAddressList)
  , "PubliclyAccessible" :: NullOrUndefined (Boolean)
  , "SecondaryAvailabilityZone" :: NullOrUndefined (String)
  }
derive instance newtypeReplicationInstance :: Newtype ReplicationInstance _


newtype ReplicationInstanceList = ReplicationInstanceList (Array ReplicationInstance)
derive instance newtypeReplicationInstanceList :: Newtype ReplicationInstanceList _


newtype ReplicationInstancePrivateIpAddressList = ReplicationInstancePrivateIpAddressList (Array String)
derive instance newtypeReplicationInstancePrivateIpAddressList :: Newtype ReplicationInstancePrivateIpAddressList _


newtype ReplicationInstancePublicIpAddressList = ReplicationInstancePublicIpAddressList (Array String)
derive instance newtypeReplicationInstancePublicIpAddressList :: Newtype ReplicationInstancePublicIpAddressList _


-- | <p>Contains metadata for a replication instance task log.</p>
newtype ReplicationInstanceTaskLog = ReplicationInstanceTaskLog 
  { "ReplicationTaskName" :: NullOrUndefined (String)
  , "ReplicationTaskArn" :: NullOrUndefined (String)
  , "ReplicationInstanceTaskLogSize" :: NullOrUndefined (Number)
  }
derive instance newtypeReplicationInstanceTaskLog :: Newtype ReplicationInstanceTaskLog _


newtype ReplicationInstanceTaskLogsList = ReplicationInstanceTaskLogsList (Array ReplicationInstanceTaskLog)
derive instance newtypeReplicationInstanceTaskLogsList :: Newtype ReplicationInstanceTaskLogsList _


-- | <p/>
newtype ReplicationPendingModifiedValues = ReplicationPendingModifiedValues 
  { "ReplicationInstanceClass" :: NullOrUndefined (String)
  , "AllocatedStorage" :: NullOrUndefined (IntegerOptional)
  , "MultiAZ" :: NullOrUndefined (BooleanOptional)
  , "EngineVersion" :: NullOrUndefined (String)
  }
derive instance newtypeReplicationPendingModifiedValues :: Newtype ReplicationPendingModifiedValues _


-- | <p/>
newtype ReplicationSubnetGroup = ReplicationSubnetGroup 
  { "ReplicationSubnetGroupIdentifier" :: NullOrUndefined (String)
  , "ReplicationSubnetGroupDescription" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "SubnetGroupStatus" :: NullOrUndefined (String)
  , "Subnets" :: NullOrUndefined (SubnetList)
  }
derive instance newtypeReplicationSubnetGroup :: Newtype ReplicationSubnetGroup _


-- | <p>The replication subnet group does not cover enough Availability Zones (AZs). Edit the replication subnet group and add more AZs.</p>
newtype ReplicationSubnetGroupDoesNotCoverEnoughAZs = ReplicationSubnetGroupDoesNotCoverEnoughAZs 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeReplicationSubnetGroupDoesNotCoverEnoughAZs :: Newtype ReplicationSubnetGroupDoesNotCoverEnoughAZs _


newtype ReplicationSubnetGroups = ReplicationSubnetGroups (Array ReplicationSubnetGroup)
derive instance newtypeReplicationSubnetGroups :: Newtype ReplicationSubnetGroups _


-- | <p/>
newtype ReplicationTask = ReplicationTask 
  { "ReplicationTaskIdentifier" :: NullOrUndefined (String)
  , "SourceEndpointArn" :: NullOrUndefined (String)
  , "TargetEndpointArn" :: NullOrUndefined (String)
  , "ReplicationInstanceArn" :: NullOrUndefined (String)
  , "MigrationType" :: NullOrUndefined (MigrationTypeValue)
  , "TableMappings" :: NullOrUndefined (String)
  , "ReplicationTaskSettings" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  , "LastFailureMessage" :: NullOrUndefined (String)
  , "StopReason" :: NullOrUndefined (String)
  , "ReplicationTaskCreationDate" :: NullOrUndefined (TStamp)
  , "ReplicationTaskStartDate" :: NullOrUndefined (TStamp)
  , "ReplicationTaskArn" :: NullOrUndefined (String)
  , "ReplicationTaskStats" :: NullOrUndefined (ReplicationTaskStats)
  }
derive instance newtypeReplicationTask :: Newtype ReplicationTask _


-- | <p> The task assessment report in JSON format. </p>
newtype ReplicationTaskAssessmentResult = ReplicationTaskAssessmentResult 
  { "ReplicationTaskIdentifier" :: NullOrUndefined (String)
  , "ReplicationTaskArn" :: NullOrUndefined (String)
  , "ReplicationTaskLastAssessmentDate" :: NullOrUndefined (TStamp)
  , "AssessmentStatus" :: NullOrUndefined (String)
  , "AssessmentResultsFile" :: NullOrUndefined (String)
  , "AssessmentResults" :: NullOrUndefined (String)
  , "S3ObjectUrl" :: NullOrUndefined (String)
  }
derive instance newtypeReplicationTaskAssessmentResult :: Newtype ReplicationTaskAssessmentResult _


newtype ReplicationTaskAssessmentResultList = ReplicationTaskAssessmentResultList (Array ReplicationTaskAssessmentResult)
derive instance newtypeReplicationTaskAssessmentResultList :: Newtype ReplicationTaskAssessmentResultList _


newtype ReplicationTaskList = ReplicationTaskList (Array ReplicationTask)
derive instance newtypeReplicationTaskList :: Newtype ReplicationTaskList _


-- | <p/>
newtype ReplicationTaskStats = ReplicationTaskStats 
  { "FullLoadProgressPercent" :: NullOrUndefined (Int)
  , "ElapsedTimeMillis" :: NullOrUndefined (Number)
  , "TablesLoaded" :: NullOrUndefined (Int)
  , "TablesLoading" :: NullOrUndefined (Int)
  , "TablesQueued" :: NullOrUndefined (Int)
  , "TablesErrored" :: NullOrUndefined (Int)
  }
derive instance newtypeReplicationTaskStats :: Newtype ReplicationTaskStats _


-- | <p>The resource you are attempting to create already exists.</p>
newtype ResourceAlreadyExistsFault = ResourceAlreadyExistsFault 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeResourceAlreadyExistsFault :: Newtype ResourceAlreadyExistsFault _


-- | <p>The resource could not be found.</p>
newtype ResourceNotFoundFault = ResourceNotFoundFault 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeResourceNotFoundFault :: Newtype ResourceNotFoundFault _


-- | <p>The quota for this resource quota has been exceeded.</p>
newtype ResourceQuotaExceededFault = ResourceQuotaExceededFault 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeResourceQuotaExceededFault :: Newtype ResourceQuotaExceededFault _


-- | <p/>
newtype S3Settings = S3Settings 
  { "ServiceAccessRoleArn" :: NullOrUndefined (String)
  , "ExternalTableDefinition" :: NullOrUndefined (String)
  , "CsvRowDelimiter" :: NullOrUndefined (String)
  , "CsvDelimiter" :: NullOrUndefined (String)
  , "BucketFolder" :: NullOrUndefined (String)
  , "BucketName" :: NullOrUndefined (String)
  , "CompressionType" :: NullOrUndefined (CompressionTypeValue)
  }
derive instance newtypeS3Settings :: Newtype S3Settings _


-- | <p>The SNS topic is invalid.</p>
newtype SNSInvalidTopicFault = SNSInvalidTopicFault 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeSNSInvalidTopicFault :: Newtype SNSInvalidTopicFault _


-- | <p>You are not authorized for the SNS subscription.</p>
newtype SNSNoAuthorizationFault = SNSNoAuthorizationFault 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeSNSNoAuthorizationFault :: Newtype SNSNoAuthorizationFault _


newtype SchemaList = SchemaList (Array String)
derive instance newtypeSchemaList :: Newtype SchemaList _


newtype SecretString = SecretString String
derive instance newtypeSecretString :: Newtype SecretString _


newtype SourceIdsList = SourceIdsList (Array String)
derive instance newtypeSourceIdsList :: Newtype SourceIdsList _


newtype SourceType = SourceType String
derive instance newtypeSourceType :: Newtype SourceType _


-- | <p/>
newtype StartReplicationTaskAssessmentMessage = StartReplicationTaskAssessmentMessage 
  { "ReplicationTaskArn" :: (String)
  }
derive instance newtypeStartReplicationTaskAssessmentMessage :: Newtype StartReplicationTaskAssessmentMessage _


-- | <p/>
newtype StartReplicationTaskAssessmentResponse = StartReplicationTaskAssessmentResponse 
  { "ReplicationTask" :: NullOrUndefined (ReplicationTask)
  }
derive instance newtypeStartReplicationTaskAssessmentResponse :: Newtype StartReplicationTaskAssessmentResponse _


-- | <p/>
newtype StartReplicationTaskMessage = StartReplicationTaskMessage 
  { "ReplicationTaskArn" :: (String)
  , "StartReplicationTaskType" :: (StartReplicationTaskTypeValue)
  , "CdcStartTime" :: NullOrUndefined (TStamp)
  }
derive instance newtypeStartReplicationTaskMessage :: Newtype StartReplicationTaskMessage _


-- | <p/>
newtype StartReplicationTaskResponse = StartReplicationTaskResponse 
  { "ReplicationTask" :: NullOrUndefined (ReplicationTask)
  }
derive instance newtypeStartReplicationTaskResponse :: Newtype StartReplicationTaskResponse _


newtype StartReplicationTaskTypeValue = StartReplicationTaskTypeValue String
derive instance newtypeStartReplicationTaskTypeValue :: Newtype StartReplicationTaskTypeValue _


-- | <p/>
newtype StopReplicationTaskMessage = StopReplicationTaskMessage 
  { "ReplicationTaskArn" :: (String)
  }
derive instance newtypeStopReplicationTaskMessage :: Newtype StopReplicationTaskMessage _


-- | <p/>
newtype StopReplicationTaskResponse = StopReplicationTaskResponse 
  { "ReplicationTask" :: NullOrUndefined (ReplicationTask)
  }
derive instance newtypeStopReplicationTaskResponse :: Newtype StopReplicationTaskResponse _


-- | <p>The storage quota has been exceeded.</p>
newtype StorageQuotaExceededFault = StorageQuotaExceededFault 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeStorageQuotaExceededFault :: Newtype StorageQuotaExceededFault _


-- | <p/>
newtype Subnet = Subnet 
  { "SubnetIdentifier" :: NullOrUndefined (String)
  , "SubnetAvailabilityZone" :: NullOrUndefined (AvailabilityZone)
  , "SubnetStatus" :: NullOrUndefined (String)
  }
derive instance newtypeSubnet :: Newtype Subnet _


-- | <p>The specified subnet is already in use.</p>
newtype SubnetAlreadyInUse = SubnetAlreadyInUse 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeSubnetAlreadyInUse :: Newtype SubnetAlreadyInUse _


newtype SubnetIdentifierList = SubnetIdentifierList (Array String)
derive instance newtypeSubnetIdentifierList :: Newtype SubnetIdentifierList _


newtype SubnetList = SubnetList (Array Subnet)
derive instance newtypeSubnetList :: Newtype SubnetList _


-- | <p/>
newtype SupportedEndpointType = SupportedEndpointType 
  { "EngineName" :: NullOrUndefined (String)
  , "SupportsCDC" :: NullOrUndefined (Boolean)
  , "EndpointType" :: NullOrUndefined (ReplicationEndpointTypeValue)
  }
derive instance newtypeSupportedEndpointType :: Newtype SupportedEndpointType _


newtype SupportedEndpointTypeList = SupportedEndpointTypeList (Array SupportedEndpointType)
derive instance newtypeSupportedEndpointTypeList :: Newtype SupportedEndpointTypeList _


newtype TStamp = TStamp Number
derive instance newtypeTStamp :: Newtype TStamp _


newtype TableListToReload = TableListToReload (Array TableToReload)
derive instance newtypeTableListToReload :: Newtype TableListToReload _


-- | <p/>
newtype TableStatistics = TableStatistics 
  { "SchemaName" :: NullOrUndefined (String)
  , "TableName" :: NullOrUndefined (String)
  , "Inserts" :: NullOrUndefined (Number)
  , "Deletes" :: NullOrUndefined (Number)
  , "Updates" :: NullOrUndefined (Number)
  , "Ddls" :: NullOrUndefined (Number)
  , "FullLoadRows" :: NullOrUndefined (Number)
  , "FullLoadCondtnlChkFailedRows" :: NullOrUndefined (Number)
  , "FullLoadErrorRows" :: NullOrUndefined (Number)
  , "LastUpdateTime" :: NullOrUndefined (TStamp)
  , "TableState" :: NullOrUndefined (String)
  , "ValidationPendingRecords" :: NullOrUndefined (Number)
  , "ValidationFailedRecords" :: NullOrUndefined (Number)
  , "ValidationSuspendedRecords" :: NullOrUndefined (Number)
  , "ValidationState" :: NullOrUndefined (String)
  }
derive instance newtypeTableStatistics :: Newtype TableStatistics _


newtype TableStatisticsList = TableStatisticsList (Array TableStatistics)
derive instance newtypeTableStatisticsList :: Newtype TableStatisticsList _


-- | <p/>
newtype TableToReload = TableToReload 
  { "SchemaName" :: NullOrUndefined (String)
  , "TableName" :: NullOrUndefined (String)
  }
derive instance newtypeTableToReload :: Newtype TableToReload _


-- | <p/>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (String)
  , "Value" :: NullOrUndefined (String)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


-- | <p/>
newtype TestConnectionMessage = TestConnectionMessage 
  { "ReplicationInstanceArn" :: (String)
  , "EndpointArn" :: (String)
  }
derive instance newtypeTestConnectionMessage :: Newtype TestConnectionMessage _


-- | <p/>
newtype TestConnectionResponse = TestConnectionResponse 
  { "Connection" :: NullOrUndefined (Connection)
  }
derive instance newtypeTestConnectionResponse :: Newtype TestConnectionResponse _


-- | <p>An upgrade dependency is preventing the database migration.</p>
newtype UpgradeDependencyFailureFault = UpgradeDependencyFailureFault 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeUpgradeDependencyFailureFault :: Newtype UpgradeDependencyFailureFault _


newtype VpcSecurityGroupIdList = VpcSecurityGroupIdList (Array String)
derive instance newtypeVpcSecurityGroupIdList :: Newtype VpcSecurityGroupIdList _


-- | <p/>
newtype VpcSecurityGroupMembership = VpcSecurityGroupMembership 
  { "VpcSecurityGroupId" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }
derive instance newtypeVpcSecurityGroupMembership :: Newtype VpcSecurityGroupMembership _


newtype VpcSecurityGroupMembershipList = VpcSecurityGroupMembershipList (Array VpcSecurityGroupMembership)
derive instance newtypeVpcSecurityGroupMembershipList :: Newtype VpcSecurityGroupMembershipList _
