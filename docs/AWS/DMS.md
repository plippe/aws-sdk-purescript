## Module AWS.DMS

<fullname>AWS Database Migration Service</fullname> <p>AWS Database Migration Service (AWS DMS) can migrate your data to and from the most widely used commercial and open-source databases such as Oracle, PostgreSQL, Microsoft SQL Server, Amazon Redshift, MariaDB, Amazon Aurora, MySQL, and SAP Adaptive Server Enterprise (ASE). The service supports homogeneous migrations such as Oracle to Oracle, as well as heterogeneous migrations between different database platforms, such as Oracle to MySQL or SQL Server to PostgreSQL.</p> <p>For more information about AWS DMS, see the AWS DMS user guide at <a href="http://docs.aws.amazon.com/dms/latest/userguide/Welcome.html"> What Is AWS Database Migration Service? </a> </p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addTagsToResource`

``` purescript
addTagsToResource :: forall eff. AddTagsToResourceMessage -> Aff (err :: RequestError | eff) AddTagsToResourceResponse
```

<p>Adds metadata tags to a DMS resource, including replication instance, endpoint, security group, and migration task. These tags can also be used with cost allocation reporting to track cost associated with DMS resources, or used in a Condition statement in an IAM policy for DMS.</p>

#### `createEndpoint`

``` purescript
createEndpoint :: forall eff. CreateEndpointMessage -> Aff (err :: RequestError | eff) CreateEndpointResponse
```

<p>Creates an endpoint using the provided settings.</p>

#### `createEventSubscription`

``` purescript
createEventSubscription :: forall eff. CreateEventSubscriptionMessage -> Aff (err :: RequestError | eff) CreateEventSubscriptionResponse
```

<p> Creates an AWS DMS event notification subscription. </p> <p>You can specify the type of source (<code>SourceType</code>) you want to be notified of, provide a list of AWS DMS source IDs (<code>SourceIds</code>) that triggers the events, and provide a list of event categories (<code>EventCategories</code>) for events you want to be notified of. If you specify both the <code>SourceType</code> and <code>SourceIds</code>, such as <code>SourceType = replication-instance</code> and <code>SourceIdentifier = my-replinstance</code>, you will be notified of all the replication instance events for the specified source. If you specify a <code>SourceType</code> but don't specify a <code>SourceIdentifier</code>, you receive notice of the events for that source type for all your AWS DMS sources. If you don't specify either <code>SourceType</code> nor <code>SourceIdentifier</code>, you will be notified of events generated from all AWS DMS sources belonging to your customer account.</p> <p>For more information about AWS DMS events, see <a href="http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html"> Working with Events and Notifications </a> in the AWS Database MIgration Service User Guide.</p>

#### `createReplicationInstance`

``` purescript
createReplicationInstance :: forall eff. CreateReplicationInstanceMessage -> Aff (err :: RequestError | eff) CreateReplicationInstanceResponse
```

<p>Creates the replication instance using the specified parameters.</p>

#### `createReplicationSubnetGroup`

``` purescript
createReplicationSubnetGroup :: forall eff. CreateReplicationSubnetGroupMessage -> Aff (err :: RequestError | eff) CreateReplicationSubnetGroupResponse
```

<p>Creates a replication subnet group given a list of the subnet IDs in a VPC.</p>

#### `createReplicationTask`

``` purescript
createReplicationTask :: forall eff. CreateReplicationTaskMessage -> Aff (err :: RequestError | eff) CreateReplicationTaskResponse
```

<p>Creates a replication task using the specified parameters.</p>

#### `deleteCertificate`

``` purescript
deleteCertificate :: forall eff. DeleteCertificateMessage -> Aff (err :: RequestError | eff) DeleteCertificateResponse
```

<p>Deletes the specified certificate. </p>

#### `deleteEndpoint`

``` purescript
deleteEndpoint :: forall eff. DeleteEndpointMessage -> Aff (err :: RequestError | eff) DeleteEndpointResponse
```

<p>Deletes the specified endpoint.</p> <note> <p>All tasks associated with the endpoint must be deleted before you can delete the endpoint.</p> </note> <p/>

#### `deleteEventSubscription`

``` purescript
deleteEventSubscription :: forall eff. DeleteEventSubscriptionMessage -> Aff (err :: RequestError | eff) DeleteEventSubscriptionResponse
```

<p> Deletes an AWS DMS event subscription. </p>

#### `deleteReplicationInstance`

``` purescript
deleteReplicationInstance :: forall eff. DeleteReplicationInstanceMessage -> Aff (err :: RequestError | eff) DeleteReplicationInstanceResponse
```

<p>Deletes the specified replication instance.</p> <note> <p>You must delete any migration tasks that are associated with the replication instance before you can delete it.</p> </note> <p/>

#### `deleteReplicationSubnetGroup`

``` purescript
deleteReplicationSubnetGroup :: forall eff. DeleteReplicationSubnetGroupMessage -> Aff (err :: RequestError | eff) DeleteReplicationSubnetGroupResponse
```

<p>Deletes a subnet group.</p>

#### `deleteReplicationTask`

``` purescript
deleteReplicationTask :: forall eff. DeleteReplicationTaskMessage -> Aff (err :: RequestError | eff) DeleteReplicationTaskResponse
```

<p>Deletes the specified replication task.</p>

#### `describeAccountAttributes`

``` purescript
describeAccountAttributes :: forall eff. DescribeAccountAttributesMessage -> Aff (err :: RequestError | eff) DescribeAccountAttributesResponse
```

<p>Lists all of the AWS DMS attributes for a customer account. The attributes include AWS DMS quotas for the account, such as the number of replication instances allowed. The description for a quota includes the quota name, current usage toward that quota, and the quota's maximum value.</p> <p>This command does not take any parameters.</p>

#### `describeCertificates`

``` purescript
describeCertificates :: forall eff. DescribeCertificatesMessage -> Aff (err :: RequestError | eff) DescribeCertificatesResponse
```

<p>Provides a description of the certificate.</p>

#### `describeConnections`

``` purescript
describeConnections :: forall eff. DescribeConnectionsMessage -> Aff (err :: RequestError | eff) DescribeConnectionsResponse
```

<p>Describes the status of the connections that have been made between the replication instance and an endpoint. Connections are created when you test an endpoint.</p>

#### `describeEndpointTypes`

``` purescript
describeEndpointTypes :: forall eff. DescribeEndpointTypesMessage -> Aff (err :: RequestError | eff) DescribeEndpointTypesResponse
```

<p>Returns information about the type of endpoints available.</p>

#### `describeEndpoints`

``` purescript
describeEndpoints :: forall eff. DescribeEndpointsMessage -> Aff (err :: RequestError | eff) DescribeEndpointsResponse
```

<p>Returns information about the endpoints for your account in the current region.</p>

#### `describeEventCategories`

``` purescript
describeEventCategories :: forall eff. DescribeEventCategoriesMessage -> Aff (err :: RequestError | eff) DescribeEventCategoriesResponse
```

<p>Lists categories for all event source types, or, if specified, for a specified source type. You can see a list of the event categories and source types in <a href="http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html"> Working with Events and Notifications </a> in the AWS Database Migration Service User Guide. </p>

#### `describeEventSubscriptions`

``` purescript
describeEventSubscriptions :: forall eff. DescribeEventSubscriptionsMessage -> Aff (err :: RequestError | eff) DescribeEventSubscriptionsResponse
```

<p>Lists all the event subscriptions for a customer account. The description of a subscription includes <code>SubscriptionName</code>, <code>SNSTopicARN</code>, <code>CustomerID</code>, <code>SourceType</code>, <code>SourceID</code>, <code>CreationTime</code>, and <code>Status</code>. </p> <p>If you specify <code>SubscriptionName</code>, this action lists the description for that subscription.</p>

#### `describeEvents`

``` purescript
describeEvents :: forall eff. DescribeEventsMessage -> Aff (err :: RequestError | eff) DescribeEventsResponse
```

<p> Lists events for a given source identifier and source type. You can also specify a start and end time. For more information on AWS DMS events, see <a href="http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html"> Working with Events and Notifications </a>. </p>

#### `describeOrderableReplicationInstances`

``` purescript
describeOrderableReplicationInstances :: forall eff. DescribeOrderableReplicationInstancesMessage -> Aff (err :: RequestError | eff) DescribeOrderableReplicationInstancesResponse
```

<p>Returns information about the replication instance types that can be created in the specified region.</p>

#### `describeRefreshSchemasStatus`

``` purescript
describeRefreshSchemasStatus :: forall eff. DescribeRefreshSchemasStatusMessage -> Aff (err :: RequestError | eff) DescribeRefreshSchemasStatusResponse
```

<p>Returns the status of the RefreshSchemas operation.</p>

#### `describeReplicationInstanceTaskLogs`

``` purescript
describeReplicationInstanceTaskLogs :: forall eff. DescribeReplicationInstanceTaskLogsMessage -> Aff (err :: RequestError | eff) DescribeReplicationInstanceTaskLogsResponse
```

<p>Returns information about the task logs for the specified task.</p>

#### `describeReplicationInstances`

``` purescript
describeReplicationInstances :: forall eff. DescribeReplicationInstancesMessage -> Aff (err :: RequestError | eff) DescribeReplicationInstancesResponse
```

<p>Returns information about replication instances for your account in the current region.</p>

#### `describeReplicationSubnetGroups`

``` purescript
describeReplicationSubnetGroups :: forall eff. DescribeReplicationSubnetGroupsMessage -> Aff (err :: RequestError | eff) DescribeReplicationSubnetGroupsResponse
```

<p>Returns information about the replication subnet groups.</p>

#### `describeReplicationTaskAssessmentResults`

``` purescript
describeReplicationTaskAssessmentResults :: forall eff. DescribeReplicationTaskAssessmentResultsMessage -> Aff (err :: RequestError | eff) DescribeReplicationTaskAssessmentResultsResponse
```

<p>Returns the task assessment results from Amazon S3. This action always returns the latest results.</p>

#### `describeReplicationTasks`

``` purescript
describeReplicationTasks :: forall eff. DescribeReplicationTasksMessage -> Aff (err :: RequestError | eff) DescribeReplicationTasksResponse
```

<p>Returns information about replication tasks for your account in the current region.</p>

#### `describeSchemas`

``` purescript
describeSchemas :: forall eff. DescribeSchemasMessage -> Aff (err :: RequestError | eff) DescribeSchemasResponse
```

<p>Returns information about the schema for the specified endpoint.</p> <p/>

#### `describeTableStatistics`

``` purescript
describeTableStatistics :: forall eff. DescribeTableStatisticsMessage -> Aff (err :: RequestError | eff) DescribeTableStatisticsResponse
```

<p>Returns table statistics on the database migration task, including table name, rows inserted, rows updated, and rows deleted.</p> <p>Note that the "last updated" column the DMS console only indicates the time that AWS DMS last updated the table statistics record for a table. It does not indicate the time of the last update to the table.</p>

#### `importCertificate`

``` purescript
importCertificate :: forall eff. ImportCertificateMessage -> Aff (err :: RequestError | eff) ImportCertificateResponse
```

<p>Uploads the specified certificate.</p>

#### `listTagsForResource`

``` purescript
listTagsForResource :: forall eff. ListTagsForResourceMessage -> Aff (err :: RequestError | eff) ListTagsForResourceResponse
```

<p>Lists all tags for an AWS DMS resource.</p>

#### `modifyEndpoint`

``` purescript
modifyEndpoint :: forall eff. ModifyEndpointMessage -> Aff (err :: RequestError | eff) ModifyEndpointResponse
```

<p>Modifies the specified endpoint.</p>

#### `modifyEventSubscription`

``` purescript
modifyEventSubscription :: forall eff. ModifyEventSubscriptionMessage -> Aff (err :: RequestError | eff) ModifyEventSubscriptionResponse
```

<p>Modifies an existing AWS DMS event notification subscription. </p>

#### `modifyReplicationInstance`

``` purescript
modifyReplicationInstance :: forall eff. ModifyReplicationInstanceMessage -> Aff (err :: RequestError | eff) ModifyReplicationInstanceResponse
```

<p>Modifies the replication instance to apply new settings. You can change one or more parameters by specifying these parameters and the new values in the request.</p> <p>Some settings are applied during the maintenance window.</p> <p/>

#### `modifyReplicationSubnetGroup`

``` purescript
modifyReplicationSubnetGroup :: forall eff. ModifyReplicationSubnetGroupMessage -> Aff (err :: RequestError | eff) ModifyReplicationSubnetGroupResponse
```

<p>Modifies the settings for the specified replication subnet group.</p>

#### `modifyReplicationTask`

``` purescript
modifyReplicationTask :: forall eff. ModifyReplicationTaskMessage -> Aff (err :: RequestError | eff) ModifyReplicationTaskResponse
```

<p>Modifies the specified replication task.</p> <p>You can't modify the task endpoints. The task must be stopped before you can modify it. </p> <p>For more information about AWS DMS tasks, see the AWS DMS user guide at <a href="http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html"> Working with Migration Tasks </a> </p>

#### `rebootReplicationInstance`

``` purescript
rebootReplicationInstance :: forall eff. RebootReplicationInstanceMessage -> Aff (err :: RequestError | eff) RebootReplicationInstanceResponse
```

<p>Reboots a replication instance. Rebooting results in a momentary outage, until the replication instance becomes available again.</p>

#### `refreshSchemas`

``` purescript
refreshSchemas :: forall eff. RefreshSchemasMessage -> Aff (err :: RequestError | eff) RefreshSchemasResponse
```

<p>Populates the schema for the specified endpoint. This is an asynchronous operation and can take several minutes. You can check the status of this operation by calling the DescribeRefreshSchemasStatus operation.</p>

#### `reloadTables`

``` purescript
reloadTables :: forall eff. ReloadTablesMessage -> Aff (err :: RequestError | eff) ReloadTablesResponse
```

<p>Reloads the target database table with the source data. </p>

#### `removeTagsFromResource`

``` purescript
removeTagsFromResource :: forall eff. RemoveTagsFromResourceMessage -> Aff (err :: RequestError | eff) RemoveTagsFromResourceResponse
```

<p>Removes metadata tags from a DMS resource.</p>

#### `startReplicationTask`

``` purescript
startReplicationTask :: forall eff. StartReplicationTaskMessage -> Aff (err :: RequestError | eff) StartReplicationTaskResponse
```

<p>Starts the replication task.</p> <p>For more information about AWS DMS tasks, see the AWS DMS user guide at <a href="http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html"> Working with Migration Tasks </a> </p>

#### `startReplicationTaskAssessment`

``` purescript
startReplicationTaskAssessment :: forall eff. StartReplicationTaskAssessmentMessage -> Aff (err :: RequestError | eff) StartReplicationTaskAssessmentResponse
```

<p> Starts the replication task assessment for unsupported data types in the source database. </p>

#### `stopReplicationTask`

``` purescript
stopReplicationTask :: forall eff. StopReplicationTaskMessage -> Aff (err :: RequestError | eff) StopReplicationTaskResponse
```

<p>Stops the replication task.</p> <p/>

#### `testConnection`

``` purescript
testConnection :: forall eff. TestConnectionMessage -> Aff (err :: RequestError | eff) TestConnectionResponse
```

<p>Tests the connection between the replication instance and the endpoint.</p>

#### `AccessDeniedFault`

``` purescript
newtype AccessDeniedFault
  = AccessDeniedFault { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>AWS DMS was denied access to the endpoint.</p>

##### Instances
``` purescript
Newtype AccessDeniedFault _
```

#### `AccountQuota`

``` purescript
newtype AccountQuota
  = AccountQuota { "AccountQuotaName" :: NullOrUndefined (String), "Used" :: NullOrUndefined (Number), "Max" :: NullOrUndefined (Number) }
```

<p>Describes a quota for an AWS account, for example, the number of replication instances allowed.</p>

##### Instances
``` purescript
Newtype AccountQuota _
```

#### `AccountQuotaList`

``` purescript
newtype AccountQuotaList
  = AccountQuotaList (Array AccountQuota)
```

##### Instances
``` purescript
Newtype AccountQuotaList _
```

#### `AddTagsToResourceMessage`

``` purescript
newtype AddTagsToResourceMessage
  = AddTagsToResourceMessage { "ResourceArn" :: String, "Tags" :: TagList }
```

<p/>

##### Instances
``` purescript
Newtype AddTagsToResourceMessage _
```

#### `AddTagsToResourceResponse`

``` purescript
newtype AddTagsToResourceResponse
  = AddTagsToResourceResponse {  }
```

<p/>

##### Instances
``` purescript
Newtype AddTagsToResourceResponse _
```

#### `AuthMechanismValue`

``` purescript
newtype AuthMechanismValue
  = AuthMechanismValue String
```

##### Instances
``` purescript
Newtype AuthMechanismValue _
```

#### `AuthTypeValue`

``` purescript
newtype AuthTypeValue
  = AuthTypeValue String
```

##### Instances
``` purescript
Newtype AuthTypeValue _
```

#### `AvailabilityZone`

``` purescript
newtype AvailabilityZone
  = AvailabilityZone { "Name" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype AvailabilityZone _
```

#### `BooleanOptional`

``` purescript
newtype BooleanOptional
  = BooleanOptional Boolean
```

##### Instances
``` purescript
Newtype BooleanOptional _
```

#### `Certificate`

``` purescript
newtype Certificate
  = Certificate { "CertificateIdentifier" :: NullOrUndefined (String), "CertificateCreationDate" :: NullOrUndefined (TStamp), "CertificatePem" :: NullOrUndefined (String), "CertificateWallet" :: NullOrUndefined (CertificateWallet), "CertificateArn" :: NullOrUndefined (String), "CertificateOwner" :: NullOrUndefined (String), "ValidFromDate" :: NullOrUndefined (TStamp), "ValidToDate" :: NullOrUndefined (TStamp), "SigningAlgorithm" :: NullOrUndefined (String), "KeyLength" :: NullOrUndefined (IntegerOptional) }
```

<p>The SSL certificate that can be used to encrypt connections between the endpoints and the replication instance.</p>

##### Instances
``` purescript
Newtype Certificate _
```

#### `CertificateList`

``` purescript
newtype CertificateList
  = CertificateList (Array Certificate)
```

##### Instances
``` purescript
Newtype CertificateList _
```

#### `CertificateWallet`

``` purescript
newtype CertificateWallet
  = CertificateWallet String
```

##### Instances
``` purescript
Newtype CertificateWallet _
```

#### `CompressionTypeValue`

``` purescript
newtype CompressionTypeValue
  = CompressionTypeValue String
```

##### Instances
``` purescript
Newtype CompressionTypeValue _
```

#### `Connection`

``` purescript
newtype Connection
  = Connection { "ReplicationInstanceArn" :: NullOrUndefined (String), "EndpointArn" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "LastFailureMessage" :: NullOrUndefined (String), "EndpointIdentifier" :: NullOrUndefined (String), "ReplicationInstanceIdentifier" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype Connection _
```

#### `ConnectionList`

``` purescript
newtype ConnectionList
  = ConnectionList (Array Connection)
```

##### Instances
``` purescript
Newtype ConnectionList _
```

#### `CreateEndpointMessage`

``` purescript
newtype CreateEndpointMessage
  = CreateEndpointMessage { "EndpointIdentifier" :: String, "EndpointType" :: ReplicationEndpointTypeValue, "EngineName" :: String, "Username" :: NullOrUndefined (String), "Password" :: NullOrUndefined (SecretString), "ServerName" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "DatabaseName" :: NullOrUndefined (String), "ExtraConnectionAttributes" :: NullOrUndefined (String), "KmsKeyId" :: NullOrUndefined (String), "Tags" :: NullOrUndefined (TagList), "CertificateArn" :: NullOrUndefined (String), "SslMode" :: NullOrUndefined (DmsSslModeValue), "DynamoDbSettings" :: NullOrUndefined (DynamoDbSettings), "S3Settings" :: NullOrUndefined (S3Settings), "MongoDbSettings" :: NullOrUndefined (MongoDbSettings) }
```

<p/>

##### Instances
``` purescript
Newtype CreateEndpointMessage _
```

#### `CreateEndpointResponse`

``` purescript
newtype CreateEndpointResponse
  = CreateEndpointResponse { "Endpoint" :: NullOrUndefined (Endpoint) }
```

<p/>

##### Instances
``` purescript
Newtype CreateEndpointResponse _
```

#### `CreateEventSubscriptionMessage`

``` purescript
newtype CreateEventSubscriptionMessage
  = CreateEventSubscriptionMessage { "SubscriptionName" :: String, "SnsTopicArn" :: String, "SourceType" :: NullOrUndefined (String), "EventCategories" :: NullOrUndefined (EventCategoriesList), "SourceIds" :: NullOrUndefined (SourceIdsList), "Enabled" :: NullOrUndefined (BooleanOptional), "Tags" :: NullOrUndefined (TagList) }
```

<p/>

##### Instances
``` purescript
Newtype CreateEventSubscriptionMessage _
```

#### `CreateEventSubscriptionResponse`

``` purescript
newtype CreateEventSubscriptionResponse
  = CreateEventSubscriptionResponse { "EventSubscription" :: NullOrUndefined (EventSubscription) }
```

<p/>

##### Instances
``` purescript
Newtype CreateEventSubscriptionResponse _
```

#### `CreateReplicationInstanceMessage`

``` purescript
newtype CreateReplicationInstanceMessage
  = CreateReplicationInstanceMessage { "ReplicationInstanceIdentifier" :: String, "AllocatedStorage" :: NullOrUndefined (IntegerOptional), "ReplicationInstanceClass" :: String, "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList), "AvailabilityZone" :: NullOrUndefined (String), "ReplicationSubnetGroupIdentifier" :: NullOrUndefined (String), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "MultiAZ" :: NullOrUndefined (BooleanOptional), "EngineVersion" :: NullOrUndefined (String), "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional), "Tags" :: NullOrUndefined (TagList), "KmsKeyId" :: NullOrUndefined (String), "PubliclyAccessible" :: NullOrUndefined (BooleanOptional) }
```

<p/>

##### Instances
``` purescript
Newtype CreateReplicationInstanceMessage _
```

#### `CreateReplicationInstanceResponse`

``` purescript
newtype CreateReplicationInstanceResponse
  = CreateReplicationInstanceResponse { "ReplicationInstance" :: NullOrUndefined (ReplicationInstance) }
```

<p/>

##### Instances
``` purescript
Newtype CreateReplicationInstanceResponse _
```

#### `CreateReplicationSubnetGroupMessage`

``` purescript
newtype CreateReplicationSubnetGroupMessage
  = CreateReplicationSubnetGroupMessage { "ReplicationSubnetGroupIdentifier" :: String, "ReplicationSubnetGroupDescription" :: String, "SubnetIds" :: SubnetIdentifierList, "Tags" :: NullOrUndefined (TagList) }
```

<p/>

##### Instances
``` purescript
Newtype CreateReplicationSubnetGroupMessage _
```

#### `CreateReplicationSubnetGroupResponse`

``` purescript
newtype CreateReplicationSubnetGroupResponse
  = CreateReplicationSubnetGroupResponse { "ReplicationSubnetGroup" :: NullOrUndefined (ReplicationSubnetGroup) }
```

<p/>

##### Instances
``` purescript
Newtype CreateReplicationSubnetGroupResponse _
```

#### `CreateReplicationTaskMessage`

``` purescript
newtype CreateReplicationTaskMessage
  = CreateReplicationTaskMessage { "ReplicationTaskIdentifier" :: String, "SourceEndpointArn" :: String, "TargetEndpointArn" :: String, "ReplicationInstanceArn" :: String, "MigrationType" :: MigrationTypeValue, "TableMappings" :: String, "ReplicationTaskSettings" :: NullOrUndefined (String), "CdcStartTime" :: NullOrUndefined (TStamp), "Tags" :: NullOrUndefined (TagList) }
```

<p/>

##### Instances
``` purescript
Newtype CreateReplicationTaskMessage _
```

#### `CreateReplicationTaskResponse`

``` purescript
newtype CreateReplicationTaskResponse
  = CreateReplicationTaskResponse { "ReplicationTask" :: NullOrUndefined (ReplicationTask) }
```

<p/>

##### Instances
``` purescript
Newtype CreateReplicationTaskResponse _
```

#### `DeleteCertificateMessage`

``` purescript
newtype DeleteCertificateMessage
  = DeleteCertificateMessage { "CertificateArn" :: String }
```

##### Instances
``` purescript
Newtype DeleteCertificateMessage _
```

#### `DeleteCertificateResponse`

``` purescript
newtype DeleteCertificateResponse
  = DeleteCertificateResponse { "Certificate" :: NullOrUndefined (Certificate) }
```

##### Instances
``` purescript
Newtype DeleteCertificateResponse _
```

#### `DeleteEndpointMessage`

``` purescript
newtype DeleteEndpointMessage
  = DeleteEndpointMessage { "EndpointArn" :: String }
```

<p/>

##### Instances
``` purescript
Newtype DeleteEndpointMessage _
```

#### `DeleteEndpointResponse`

``` purescript
newtype DeleteEndpointResponse
  = DeleteEndpointResponse { "Endpoint" :: NullOrUndefined (Endpoint) }
```

<p/>

##### Instances
``` purescript
Newtype DeleteEndpointResponse _
```

#### `DeleteEventSubscriptionMessage`

``` purescript
newtype DeleteEventSubscriptionMessage
  = DeleteEventSubscriptionMessage { "SubscriptionName" :: String }
```

<p/>

##### Instances
``` purescript
Newtype DeleteEventSubscriptionMessage _
```

#### `DeleteEventSubscriptionResponse`

``` purescript
newtype DeleteEventSubscriptionResponse
  = DeleteEventSubscriptionResponse { "EventSubscription" :: NullOrUndefined (EventSubscription) }
```

<p/>

##### Instances
``` purescript
Newtype DeleteEventSubscriptionResponse _
```

#### `DeleteReplicationInstanceMessage`

``` purescript
newtype DeleteReplicationInstanceMessage
  = DeleteReplicationInstanceMessage { "ReplicationInstanceArn" :: String }
```

<p/>

##### Instances
``` purescript
Newtype DeleteReplicationInstanceMessage _
```

#### `DeleteReplicationInstanceResponse`

``` purescript
newtype DeleteReplicationInstanceResponse
  = DeleteReplicationInstanceResponse { "ReplicationInstance" :: NullOrUndefined (ReplicationInstance) }
```

<p/>

##### Instances
``` purescript
Newtype DeleteReplicationInstanceResponse _
```

#### `DeleteReplicationSubnetGroupMessage`

``` purescript
newtype DeleteReplicationSubnetGroupMessage
  = DeleteReplicationSubnetGroupMessage { "ReplicationSubnetGroupIdentifier" :: String }
```

<p/>

##### Instances
``` purescript
Newtype DeleteReplicationSubnetGroupMessage _
```

#### `DeleteReplicationSubnetGroupResponse`

``` purescript
newtype DeleteReplicationSubnetGroupResponse
  = DeleteReplicationSubnetGroupResponse {  }
```

<p/>

##### Instances
``` purescript
Newtype DeleteReplicationSubnetGroupResponse _
```

#### `DeleteReplicationTaskMessage`

``` purescript
newtype DeleteReplicationTaskMessage
  = DeleteReplicationTaskMessage { "ReplicationTaskArn" :: String }
```

<p/>

##### Instances
``` purescript
Newtype DeleteReplicationTaskMessage _
```

#### `DeleteReplicationTaskResponse`

``` purescript
newtype DeleteReplicationTaskResponse
  = DeleteReplicationTaskResponse { "ReplicationTask" :: NullOrUndefined (ReplicationTask) }
```

<p/>

##### Instances
``` purescript
Newtype DeleteReplicationTaskResponse _
```

#### `DescribeAccountAttributesMessage`

``` purescript
newtype DescribeAccountAttributesMessage
  = DescribeAccountAttributesMessage {  }
```

<p/>

##### Instances
``` purescript
Newtype DescribeAccountAttributesMessage _
```

#### `DescribeAccountAttributesResponse`

``` purescript
newtype DescribeAccountAttributesResponse
  = DescribeAccountAttributesResponse { "AccountQuotas" :: NullOrUndefined (AccountQuotaList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeAccountAttributesResponse _
```

#### `DescribeCertificatesMessage`

``` purescript
newtype DescribeCertificatesMessage
  = DescribeCertificatesMessage { "Filters" :: NullOrUndefined (FilterList), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeCertificatesMessage _
```

#### `DescribeCertificatesResponse`

``` purescript
newtype DescribeCertificatesResponse
  = DescribeCertificatesResponse { "Marker" :: NullOrUndefined (String), "Certificates" :: NullOrUndefined (CertificateList) }
```

##### Instances
``` purescript
Newtype DescribeCertificatesResponse _
```

#### `DescribeConnectionsMessage`

``` purescript
newtype DescribeConnectionsMessage
  = DescribeConnectionsMessage { "Filters" :: NullOrUndefined (FilterList), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeConnectionsMessage _
```

#### `DescribeConnectionsResponse`

``` purescript
newtype DescribeConnectionsResponse
  = DescribeConnectionsResponse { "Marker" :: NullOrUndefined (String), "Connections" :: NullOrUndefined (ConnectionList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeConnectionsResponse _
```

#### `DescribeEndpointTypesMessage`

``` purescript
newtype DescribeEndpointTypesMessage
  = DescribeEndpointTypesMessage { "Filters" :: NullOrUndefined (FilterList), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeEndpointTypesMessage _
```

#### `DescribeEndpointTypesResponse`

``` purescript
newtype DescribeEndpointTypesResponse
  = DescribeEndpointTypesResponse { "Marker" :: NullOrUndefined (String), "SupportedEndpointTypes" :: NullOrUndefined (SupportedEndpointTypeList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeEndpointTypesResponse _
```

#### `DescribeEndpointsMessage`

``` purescript
newtype DescribeEndpointsMessage
  = DescribeEndpointsMessage { "Filters" :: NullOrUndefined (FilterList), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeEndpointsMessage _
```

#### `DescribeEndpointsResponse`

``` purescript
newtype DescribeEndpointsResponse
  = DescribeEndpointsResponse { "Marker" :: NullOrUndefined (String), "Endpoints" :: NullOrUndefined (EndpointList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeEndpointsResponse _
```

#### `DescribeEventCategoriesMessage`

``` purescript
newtype DescribeEventCategoriesMessage
  = DescribeEventCategoriesMessage { "SourceType" :: NullOrUndefined (String), "Filters" :: NullOrUndefined (FilterList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeEventCategoriesMessage _
```

#### `DescribeEventCategoriesResponse`

``` purescript
newtype DescribeEventCategoriesResponse
  = DescribeEventCategoriesResponse { "EventCategoryGroupList" :: NullOrUndefined (EventCategoryGroupList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeEventCategoriesResponse _
```

#### `DescribeEventSubscriptionsMessage`

``` purescript
newtype DescribeEventSubscriptionsMessage
  = DescribeEventSubscriptionsMessage { "SubscriptionName" :: NullOrUndefined (String), "Filters" :: NullOrUndefined (FilterList), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeEventSubscriptionsMessage _
```

#### `DescribeEventSubscriptionsResponse`

``` purescript
newtype DescribeEventSubscriptionsResponse
  = DescribeEventSubscriptionsResponse { "Marker" :: NullOrUndefined (String), "EventSubscriptionsList" :: NullOrUndefined (EventSubscriptionsList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeEventSubscriptionsResponse _
```

#### `DescribeEventsMessage`

``` purescript
newtype DescribeEventsMessage
  = DescribeEventsMessage { "SourceIdentifier" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (SourceType), "StartTime" :: NullOrUndefined (TStamp), "EndTime" :: NullOrUndefined (TStamp), "Duration" :: NullOrUndefined (IntegerOptional), "EventCategories" :: NullOrUndefined (EventCategoriesList), "Filters" :: NullOrUndefined (FilterList), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeEventsMessage _
```

#### `DescribeEventsResponse`

``` purescript
newtype DescribeEventsResponse
  = DescribeEventsResponse { "Marker" :: NullOrUndefined (String), "Events" :: NullOrUndefined (EventList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeEventsResponse _
```

#### `DescribeOrderableReplicationInstancesMessage`

``` purescript
newtype DescribeOrderableReplicationInstancesMessage
  = DescribeOrderableReplicationInstancesMessage { "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeOrderableReplicationInstancesMessage _
```

#### `DescribeOrderableReplicationInstancesResponse`

``` purescript
newtype DescribeOrderableReplicationInstancesResponse
  = DescribeOrderableReplicationInstancesResponse { "OrderableReplicationInstances" :: NullOrUndefined (OrderableReplicationInstanceList), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeOrderableReplicationInstancesResponse _
```

#### `DescribeRefreshSchemasStatusMessage`

``` purescript
newtype DescribeRefreshSchemasStatusMessage
  = DescribeRefreshSchemasStatusMessage { "EndpointArn" :: String }
```

<p/>

##### Instances
``` purescript
Newtype DescribeRefreshSchemasStatusMessage _
```

#### `DescribeRefreshSchemasStatusResponse`

``` purescript
newtype DescribeRefreshSchemasStatusResponse
  = DescribeRefreshSchemasStatusResponse { "RefreshSchemasStatus" :: NullOrUndefined (RefreshSchemasStatus) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeRefreshSchemasStatusResponse _
```

#### `DescribeReplicationInstanceTaskLogsMessage`

``` purescript
newtype DescribeReplicationInstanceTaskLogsMessage
  = DescribeReplicationInstanceTaskLogsMessage { "ReplicationInstanceArn" :: String, "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeReplicationInstanceTaskLogsMessage _
```

#### `DescribeReplicationInstanceTaskLogsResponse`

``` purescript
newtype DescribeReplicationInstanceTaskLogsResponse
  = DescribeReplicationInstanceTaskLogsResponse { "ReplicationInstanceArn" :: NullOrUndefined (String), "ReplicationInstanceTaskLogs" :: NullOrUndefined (ReplicationInstanceTaskLogsList), "Marker" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeReplicationInstanceTaskLogsResponse _
```

#### `DescribeReplicationInstancesMessage`

``` purescript
newtype DescribeReplicationInstancesMessage
  = DescribeReplicationInstancesMessage { "Filters" :: NullOrUndefined (FilterList), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeReplicationInstancesMessage _
```

#### `DescribeReplicationInstancesResponse`

``` purescript
newtype DescribeReplicationInstancesResponse
  = DescribeReplicationInstancesResponse { "Marker" :: NullOrUndefined (String), "ReplicationInstances" :: NullOrUndefined (ReplicationInstanceList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeReplicationInstancesResponse _
```

#### `DescribeReplicationSubnetGroupsMessage`

``` purescript
newtype DescribeReplicationSubnetGroupsMessage
  = DescribeReplicationSubnetGroupsMessage { "Filters" :: NullOrUndefined (FilterList), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeReplicationSubnetGroupsMessage _
```

#### `DescribeReplicationSubnetGroupsResponse`

``` purescript
newtype DescribeReplicationSubnetGroupsResponse
  = DescribeReplicationSubnetGroupsResponse { "Marker" :: NullOrUndefined (String), "ReplicationSubnetGroups" :: NullOrUndefined (ReplicationSubnetGroups) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeReplicationSubnetGroupsResponse _
```

#### `DescribeReplicationTaskAssessmentResultsMessage`

``` purescript
newtype DescribeReplicationTaskAssessmentResultsMessage
  = DescribeReplicationTaskAssessmentResultsMessage { "ReplicationTaskArn" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeReplicationTaskAssessmentResultsMessage _
```

#### `DescribeReplicationTaskAssessmentResultsResponse`

``` purescript
newtype DescribeReplicationTaskAssessmentResultsResponse
  = DescribeReplicationTaskAssessmentResultsResponse { "Marker" :: NullOrUndefined (String), "BucketName" :: NullOrUndefined (String), "ReplicationTaskAssessmentResults" :: NullOrUndefined (ReplicationTaskAssessmentResultList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeReplicationTaskAssessmentResultsResponse _
```

#### `DescribeReplicationTasksMessage`

``` purescript
newtype DescribeReplicationTasksMessage
  = DescribeReplicationTasksMessage { "Filters" :: NullOrUndefined (FilterList), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeReplicationTasksMessage _
```

#### `DescribeReplicationTasksResponse`

``` purescript
newtype DescribeReplicationTasksResponse
  = DescribeReplicationTasksResponse { "Marker" :: NullOrUndefined (String), "ReplicationTasks" :: NullOrUndefined (ReplicationTaskList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeReplicationTasksResponse _
```

#### `DescribeSchemasMessage`

``` purescript
newtype DescribeSchemasMessage
  = DescribeSchemasMessage { "EndpointArn" :: String, "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeSchemasMessage _
```

#### `DescribeSchemasResponse`

``` purescript
newtype DescribeSchemasResponse
  = DescribeSchemasResponse { "Marker" :: NullOrUndefined (String), "Schemas" :: NullOrUndefined (SchemaList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeSchemasResponse _
```

#### `DescribeTableStatisticsMessage`

``` purescript
newtype DescribeTableStatisticsMessage
  = DescribeTableStatisticsMessage { "ReplicationTaskArn" :: String, "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "Filters" :: NullOrUndefined (FilterList) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeTableStatisticsMessage _
```

#### `DescribeTableStatisticsResponse`

``` purescript
newtype DescribeTableStatisticsResponse
  = DescribeTableStatisticsResponse { "ReplicationTaskArn" :: NullOrUndefined (String), "TableStatistics" :: NullOrUndefined (TableStatisticsList), "Marker" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype DescribeTableStatisticsResponse _
```

#### `DmsSslModeValue`

``` purescript
newtype DmsSslModeValue
  = DmsSslModeValue String
```

##### Instances
``` purescript
Newtype DmsSslModeValue _
```

#### `DynamoDbSettings`

``` purescript
newtype DynamoDbSettings
  = DynamoDbSettings { "ServiceAccessRoleArn" :: String }
```

<p/>

##### Instances
``` purescript
Newtype DynamoDbSettings _
```

#### `Endpoint`

``` purescript
newtype Endpoint
  = Endpoint { "EndpointIdentifier" :: NullOrUndefined (String), "EndpointType" :: NullOrUndefined (ReplicationEndpointTypeValue), "EngineName" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String), "ServerName" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "DatabaseName" :: NullOrUndefined (String), "ExtraConnectionAttributes" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "KmsKeyId" :: NullOrUndefined (String), "EndpointArn" :: NullOrUndefined (String), "CertificateArn" :: NullOrUndefined (String), "SslMode" :: NullOrUndefined (DmsSslModeValue), "ExternalId" :: NullOrUndefined (String), "DynamoDbSettings" :: NullOrUndefined (DynamoDbSettings), "S3Settings" :: NullOrUndefined (S3Settings), "MongoDbSettings" :: NullOrUndefined (MongoDbSettings) }
```

<p/>

##### Instances
``` purescript
Newtype Endpoint _
```

#### `EndpointList`

``` purescript
newtype EndpointList
  = EndpointList (Array Endpoint)
```

##### Instances
``` purescript
Newtype EndpointList _
```

#### `Event`

``` purescript
newtype Event
  = Event { "SourceIdentifier" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (SourceType), "Message" :: NullOrUndefined (String), "EventCategories" :: NullOrUndefined (EventCategoriesList), "Date" :: NullOrUndefined (TStamp) }
```

<p/>

##### Instances
``` purescript
Newtype Event _
```

#### `EventCategoriesList`

``` purescript
newtype EventCategoriesList
  = EventCategoriesList (Array String)
```

##### Instances
``` purescript
Newtype EventCategoriesList _
```

#### `EventCategoryGroup`

``` purescript
newtype EventCategoryGroup
  = EventCategoryGroup { "SourceType" :: NullOrUndefined (String), "EventCategories" :: NullOrUndefined (EventCategoriesList) }
```

<p/>

##### Instances
``` purescript
Newtype EventCategoryGroup _
```

#### `EventCategoryGroupList`

``` purescript
newtype EventCategoryGroupList
  = EventCategoryGroupList (Array EventCategoryGroup)
```

##### Instances
``` purescript
Newtype EventCategoryGroupList _
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

#### `EventSubscription`

``` purescript
newtype EventSubscription
  = EventSubscription { "CustomerAwsId" :: NullOrUndefined (String), "CustSubscriptionId" :: NullOrUndefined (String), "SnsTopicArn" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "SubscriptionCreationTime" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (String), "SourceIdsList" :: NullOrUndefined (SourceIdsList), "EventCategoriesList" :: NullOrUndefined (EventCategoriesList), "Enabled" :: NullOrUndefined (Boolean) }
```

<p/>

##### Instances
``` purescript
Newtype EventSubscription _
```

#### `EventSubscriptionsList`

``` purescript
newtype EventSubscriptionsList
  = EventSubscriptionsList (Array EventSubscription)
```

##### Instances
``` purescript
Newtype EventSubscriptionsList _
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

##### Instances
``` purescript
Newtype ExceptionMessage _
```

#### `Filter`

``` purescript
newtype Filter
  = Filter { "Name" :: String, "Values" :: FilterValueList }
```

<p/>

##### Instances
``` purescript
Newtype Filter _
```

#### `FilterList`

``` purescript
newtype FilterList
  = FilterList (Array Filter)
```

##### Instances
``` purescript
Newtype FilterList _
```

#### `FilterValueList`

``` purescript
newtype FilterValueList
  = FilterValueList (Array String)
```

##### Instances
``` purescript
Newtype FilterValueList _
```

#### `ImportCertificateMessage`

``` purescript
newtype ImportCertificateMessage
  = ImportCertificateMessage { "CertificateIdentifier" :: String, "CertificatePem" :: NullOrUndefined (String), "CertificateWallet" :: NullOrUndefined (CertificateWallet), "Tags" :: NullOrUndefined (TagList) }
```

##### Instances
``` purescript
Newtype ImportCertificateMessage _
```

#### `ImportCertificateResponse`

``` purescript
newtype ImportCertificateResponse
  = ImportCertificateResponse { "Certificate" :: NullOrUndefined (Certificate) }
```

##### Instances
``` purescript
Newtype ImportCertificateResponse _
```

#### `InsufficientResourceCapacityFault`

``` purescript
newtype InsufficientResourceCapacityFault
  = InsufficientResourceCapacityFault { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>There are not enough resources allocated to the database migration.</p>

##### Instances
``` purescript
Newtype InsufficientResourceCapacityFault _
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

#### `InvalidCertificateFault`

``` purescript
newtype InvalidCertificateFault
  = InvalidCertificateFault { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The certificate was not valid.</p>

##### Instances
``` purescript
Newtype InvalidCertificateFault _
```

#### `InvalidResourceStateFault`

``` purescript
newtype InvalidResourceStateFault
  = InvalidResourceStateFault { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The resource is in a state that prevents it from being used for database migration.</p>

##### Instances
``` purescript
Newtype InvalidResourceStateFault _
```

#### `InvalidSubnet`

``` purescript
newtype InvalidSubnet
  = InvalidSubnet { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The subnet provided is invalid.</p>

##### Instances
``` purescript
Newtype InvalidSubnet _
```

#### `KMSKeyNotAccessibleFault`

``` purescript
newtype KMSKeyNotAccessibleFault
  = KMSKeyNotAccessibleFault { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>AWS DMS cannot access the KMS key.</p>

##### Instances
``` purescript
Newtype KMSKeyNotAccessibleFault _
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

#### `ListTagsForResourceMessage`

``` purescript
newtype ListTagsForResourceMessage
  = ListTagsForResourceMessage { "ResourceArn" :: String }
```

<p/>

##### Instances
``` purescript
Newtype ListTagsForResourceMessage _
```

#### `ListTagsForResourceResponse`

``` purescript
newtype ListTagsForResourceResponse
  = ListTagsForResourceResponse { "TagList" :: NullOrUndefined (TagList) }
```

<p/>

##### Instances
``` purescript
Newtype ListTagsForResourceResponse _
```

#### `MigrationTypeValue`

``` purescript
newtype MigrationTypeValue
  = MigrationTypeValue String
```

##### Instances
``` purescript
Newtype MigrationTypeValue _
```

#### `ModifyEndpointMessage`

``` purescript
newtype ModifyEndpointMessage
  = ModifyEndpointMessage { "EndpointArn" :: String, "EndpointIdentifier" :: NullOrUndefined (String), "EndpointType" :: NullOrUndefined (ReplicationEndpointTypeValue), "EngineName" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String), "Password" :: NullOrUndefined (SecretString), "ServerName" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "DatabaseName" :: NullOrUndefined (String), "ExtraConnectionAttributes" :: NullOrUndefined (String), "CertificateArn" :: NullOrUndefined (String), "SslMode" :: NullOrUndefined (DmsSslModeValue), "DynamoDbSettings" :: NullOrUndefined (DynamoDbSettings), "S3Settings" :: NullOrUndefined (S3Settings), "MongoDbSettings" :: NullOrUndefined (MongoDbSettings) }
```

<p/>

##### Instances
``` purescript
Newtype ModifyEndpointMessage _
```

#### `ModifyEndpointResponse`

``` purescript
newtype ModifyEndpointResponse
  = ModifyEndpointResponse { "Endpoint" :: NullOrUndefined (Endpoint) }
```

<p/>

##### Instances
``` purescript
Newtype ModifyEndpointResponse _
```

#### `ModifyEventSubscriptionMessage`

``` purescript
newtype ModifyEventSubscriptionMessage
  = ModifyEventSubscriptionMessage { "SubscriptionName" :: String, "SnsTopicArn" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (String), "EventCategories" :: NullOrUndefined (EventCategoriesList), "Enabled" :: NullOrUndefined (BooleanOptional) }
```

<p/>

##### Instances
``` purescript
Newtype ModifyEventSubscriptionMessage _
```

#### `ModifyEventSubscriptionResponse`

``` purescript
newtype ModifyEventSubscriptionResponse
  = ModifyEventSubscriptionResponse { "EventSubscription" :: NullOrUndefined (EventSubscription) }
```

<p/>

##### Instances
``` purescript
Newtype ModifyEventSubscriptionResponse _
```

#### `ModifyReplicationInstanceMessage`

``` purescript
newtype ModifyReplicationInstanceMessage
  = ModifyReplicationInstanceMessage { "ReplicationInstanceArn" :: String, "AllocatedStorage" :: NullOrUndefined (IntegerOptional), "ApplyImmediately" :: NullOrUndefined (Boolean), "ReplicationInstanceClass" :: NullOrUndefined (String), "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "MultiAZ" :: NullOrUndefined (BooleanOptional), "EngineVersion" :: NullOrUndefined (String), "AllowMajorVersionUpgrade" :: NullOrUndefined (Boolean), "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional), "ReplicationInstanceIdentifier" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype ModifyReplicationInstanceMessage _
```

#### `ModifyReplicationInstanceResponse`

``` purescript
newtype ModifyReplicationInstanceResponse
  = ModifyReplicationInstanceResponse { "ReplicationInstance" :: NullOrUndefined (ReplicationInstance) }
```

<p/>

##### Instances
``` purescript
Newtype ModifyReplicationInstanceResponse _
```

#### `ModifyReplicationSubnetGroupMessage`

``` purescript
newtype ModifyReplicationSubnetGroupMessage
  = ModifyReplicationSubnetGroupMessage { "ReplicationSubnetGroupIdentifier" :: String, "ReplicationSubnetGroupDescription" :: NullOrUndefined (String), "SubnetIds" :: SubnetIdentifierList }
```

<p/>

##### Instances
``` purescript
Newtype ModifyReplicationSubnetGroupMessage _
```

#### `ModifyReplicationSubnetGroupResponse`

``` purescript
newtype ModifyReplicationSubnetGroupResponse
  = ModifyReplicationSubnetGroupResponse { "ReplicationSubnetGroup" :: NullOrUndefined (ReplicationSubnetGroup) }
```

<p/>

##### Instances
``` purescript
Newtype ModifyReplicationSubnetGroupResponse _
```

#### `ModifyReplicationTaskMessage`

``` purescript
newtype ModifyReplicationTaskMessage
  = ModifyReplicationTaskMessage { "ReplicationTaskArn" :: String, "ReplicationTaskIdentifier" :: NullOrUndefined (String), "MigrationType" :: NullOrUndefined (MigrationTypeValue), "TableMappings" :: NullOrUndefined (String), "ReplicationTaskSettings" :: NullOrUndefined (String), "CdcStartTime" :: NullOrUndefined (TStamp) }
```

<p/>

##### Instances
``` purescript
Newtype ModifyReplicationTaskMessage _
```

#### `ModifyReplicationTaskResponse`

``` purescript
newtype ModifyReplicationTaskResponse
  = ModifyReplicationTaskResponse { "ReplicationTask" :: NullOrUndefined (ReplicationTask) }
```

<p/>

##### Instances
``` purescript
Newtype ModifyReplicationTaskResponse _
```

#### `MongoDbSettings`

``` purescript
newtype MongoDbSettings
  = MongoDbSettings { "Username" :: NullOrUndefined (String), "Password" :: NullOrUndefined (SecretString), "ServerName" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "DatabaseName" :: NullOrUndefined (String), "AuthType" :: NullOrUndefined (AuthTypeValue), "AuthMechanism" :: NullOrUndefined (AuthMechanismValue), "NestingLevel" :: NullOrUndefined (NestingLevelValue), "ExtractDocId" :: NullOrUndefined (String), "DocsToInvestigate" :: NullOrUndefined (String), "AuthSource" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype MongoDbSettings _
```

#### `NestingLevelValue`

``` purescript
newtype NestingLevelValue
  = NestingLevelValue String
```

##### Instances
``` purescript
Newtype NestingLevelValue _
```

#### `OrderableReplicationInstance`

``` purescript
newtype OrderableReplicationInstance
  = OrderableReplicationInstance { "EngineVersion" :: NullOrUndefined (String), "ReplicationInstanceClass" :: NullOrUndefined (String), "StorageType" :: NullOrUndefined (String), "MinAllocatedStorage" :: NullOrUndefined (Int), "MaxAllocatedStorage" :: NullOrUndefined (Int), "DefaultAllocatedStorage" :: NullOrUndefined (Int), "IncludedAllocatedStorage" :: NullOrUndefined (Int) }
```

<p/>

##### Instances
``` purescript
Newtype OrderableReplicationInstance _
```

#### `OrderableReplicationInstanceList`

``` purescript
newtype OrderableReplicationInstanceList
  = OrderableReplicationInstanceList (Array OrderableReplicationInstance)
```

##### Instances
``` purescript
Newtype OrderableReplicationInstanceList _
```

#### `RebootReplicationInstanceMessage`

``` purescript
newtype RebootReplicationInstanceMessage
  = RebootReplicationInstanceMessage { "ReplicationInstanceArn" :: String, "ForceFailover" :: NullOrUndefined (BooleanOptional) }
```

##### Instances
``` purescript
Newtype RebootReplicationInstanceMessage _
```

#### `RebootReplicationInstanceResponse`

``` purescript
newtype RebootReplicationInstanceResponse
  = RebootReplicationInstanceResponse { "ReplicationInstance" :: NullOrUndefined (ReplicationInstance) }
```

##### Instances
``` purescript
Newtype RebootReplicationInstanceResponse _
```

#### `RefreshSchemasMessage`

``` purescript
newtype RefreshSchemasMessage
  = RefreshSchemasMessage { "EndpointArn" :: String, "ReplicationInstanceArn" :: String }
```

<p/>

##### Instances
``` purescript
Newtype RefreshSchemasMessage _
```

#### `RefreshSchemasResponse`

``` purescript
newtype RefreshSchemasResponse
  = RefreshSchemasResponse { "RefreshSchemasStatus" :: NullOrUndefined (RefreshSchemasStatus) }
```

<p/>

##### Instances
``` purescript
Newtype RefreshSchemasResponse _
```

#### `RefreshSchemasStatus`

``` purescript
newtype RefreshSchemasStatus
  = RefreshSchemasStatus { "EndpointArn" :: NullOrUndefined (String), "ReplicationInstanceArn" :: NullOrUndefined (String), "Status" :: NullOrUndefined (RefreshSchemasStatusTypeValue), "LastRefreshDate" :: NullOrUndefined (TStamp), "LastFailureMessage" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype RefreshSchemasStatus _
```

#### `RefreshSchemasStatusTypeValue`

``` purescript
newtype RefreshSchemasStatusTypeValue
  = RefreshSchemasStatusTypeValue String
```

##### Instances
``` purescript
Newtype RefreshSchemasStatusTypeValue _
```

#### `ReloadTablesMessage`

``` purescript
newtype ReloadTablesMessage
  = ReloadTablesMessage { "ReplicationTaskArn" :: String, "TablesToReload" :: TableListToReload }
```

##### Instances
``` purescript
Newtype ReloadTablesMessage _
```

#### `ReloadTablesResponse`

``` purescript
newtype ReloadTablesResponse
  = ReloadTablesResponse { "ReplicationTaskArn" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ReloadTablesResponse _
```

#### `RemoveTagsFromResourceMessage`

``` purescript
newtype RemoveTagsFromResourceMessage
  = RemoveTagsFromResourceMessage { "ResourceArn" :: String, "TagKeys" :: KeyList }
```

<p/>

##### Instances
``` purescript
Newtype RemoveTagsFromResourceMessage _
```

#### `RemoveTagsFromResourceResponse`

``` purescript
newtype RemoveTagsFromResourceResponse
  = RemoveTagsFromResourceResponse {  }
```

<p/>

##### Instances
``` purescript
Newtype RemoveTagsFromResourceResponse _
```

#### `ReplicationEndpointTypeValue`

``` purescript
newtype ReplicationEndpointTypeValue
  = ReplicationEndpointTypeValue String
```

##### Instances
``` purescript
Newtype ReplicationEndpointTypeValue _
```

#### `ReplicationInstance`

``` purescript
newtype ReplicationInstance
  = ReplicationInstance { "ReplicationInstanceIdentifier" :: NullOrUndefined (String), "ReplicationInstanceClass" :: NullOrUndefined (String), "ReplicationInstanceStatus" :: NullOrUndefined (String), "AllocatedStorage" :: NullOrUndefined (Int), "InstanceCreateTime" :: NullOrUndefined (TStamp), "VpcSecurityGroups" :: NullOrUndefined (VpcSecurityGroupMembershipList), "AvailabilityZone" :: NullOrUndefined (String), "ReplicationSubnetGroup" :: NullOrUndefined (ReplicationSubnetGroup), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "PendingModifiedValues" :: NullOrUndefined (ReplicationPendingModifiedValues), "MultiAZ" :: NullOrUndefined (Boolean), "EngineVersion" :: NullOrUndefined (String), "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean), "KmsKeyId" :: NullOrUndefined (String), "ReplicationInstanceArn" :: NullOrUndefined (String), "ReplicationInstancePublicIpAddress" :: NullOrUndefined (String), "ReplicationInstancePrivateIpAddress" :: NullOrUndefined (String), "ReplicationInstancePublicIpAddresses" :: NullOrUndefined (ReplicationInstancePublicIpAddressList), "ReplicationInstancePrivateIpAddresses" :: NullOrUndefined (ReplicationInstancePrivateIpAddressList), "PubliclyAccessible" :: NullOrUndefined (Boolean), "SecondaryAvailabilityZone" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype ReplicationInstance _
```

#### `ReplicationInstanceList`

``` purescript
newtype ReplicationInstanceList
  = ReplicationInstanceList (Array ReplicationInstance)
```

##### Instances
``` purescript
Newtype ReplicationInstanceList _
```

#### `ReplicationInstancePrivateIpAddressList`

``` purescript
newtype ReplicationInstancePrivateIpAddressList
  = ReplicationInstancePrivateIpAddressList (Array String)
```

##### Instances
``` purescript
Newtype ReplicationInstancePrivateIpAddressList _
```

#### `ReplicationInstancePublicIpAddressList`

``` purescript
newtype ReplicationInstancePublicIpAddressList
  = ReplicationInstancePublicIpAddressList (Array String)
```

##### Instances
``` purescript
Newtype ReplicationInstancePublicIpAddressList _
```

#### `ReplicationInstanceTaskLog`

``` purescript
newtype ReplicationInstanceTaskLog
  = ReplicationInstanceTaskLog { "ReplicationTaskName" :: NullOrUndefined (String), "ReplicationTaskArn" :: NullOrUndefined (String), "ReplicationInstanceTaskLogSize" :: NullOrUndefined (Number) }
```

<p>Contains metadata for a replication instance task log.</p>

##### Instances
``` purescript
Newtype ReplicationInstanceTaskLog _
```

#### `ReplicationInstanceTaskLogsList`

``` purescript
newtype ReplicationInstanceTaskLogsList
  = ReplicationInstanceTaskLogsList (Array ReplicationInstanceTaskLog)
```

##### Instances
``` purescript
Newtype ReplicationInstanceTaskLogsList _
```

#### `ReplicationPendingModifiedValues`

``` purescript
newtype ReplicationPendingModifiedValues
  = ReplicationPendingModifiedValues { "ReplicationInstanceClass" :: NullOrUndefined (String), "AllocatedStorage" :: NullOrUndefined (IntegerOptional), "MultiAZ" :: NullOrUndefined (BooleanOptional), "EngineVersion" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype ReplicationPendingModifiedValues _
```

#### `ReplicationSubnetGroup`

``` purescript
newtype ReplicationSubnetGroup
  = ReplicationSubnetGroup { "ReplicationSubnetGroupIdentifier" :: NullOrUndefined (String), "ReplicationSubnetGroupDescription" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "SubnetGroupStatus" :: NullOrUndefined (String), "Subnets" :: NullOrUndefined (SubnetList) }
```

<p/>

##### Instances
``` purescript
Newtype ReplicationSubnetGroup _
```

#### `ReplicationSubnetGroupDoesNotCoverEnoughAZs`

``` purescript
newtype ReplicationSubnetGroupDoesNotCoverEnoughAZs
  = ReplicationSubnetGroupDoesNotCoverEnoughAZs { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The replication subnet group does not cover enough Availability Zones (AZs). Edit the replication subnet group and add more AZs.</p>

##### Instances
``` purescript
Newtype ReplicationSubnetGroupDoesNotCoverEnoughAZs _
```

#### `ReplicationSubnetGroups`

``` purescript
newtype ReplicationSubnetGroups
  = ReplicationSubnetGroups (Array ReplicationSubnetGroup)
```

##### Instances
``` purescript
Newtype ReplicationSubnetGroups _
```

#### `ReplicationTask`

``` purescript
newtype ReplicationTask
  = ReplicationTask { "ReplicationTaskIdentifier" :: NullOrUndefined (String), "SourceEndpointArn" :: NullOrUndefined (String), "TargetEndpointArn" :: NullOrUndefined (String), "ReplicationInstanceArn" :: NullOrUndefined (String), "MigrationType" :: NullOrUndefined (MigrationTypeValue), "TableMappings" :: NullOrUndefined (String), "ReplicationTaskSettings" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "LastFailureMessage" :: NullOrUndefined (String), "StopReason" :: NullOrUndefined (String), "ReplicationTaskCreationDate" :: NullOrUndefined (TStamp), "ReplicationTaskStartDate" :: NullOrUndefined (TStamp), "ReplicationTaskArn" :: NullOrUndefined (String), "ReplicationTaskStats" :: NullOrUndefined (ReplicationTaskStats) }
```

<p/>

##### Instances
``` purescript
Newtype ReplicationTask _
```

#### `ReplicationTaskAssessmentResult`

``` purescript
newtype ReplicationTaskAssessmentResult
  = ReplicationTaskAssessmentResult { "ReplicationTaskIdentifier" :: NullOrUndefined (String), "ReplicationTaskArn" :: NullOrUndefined (String), "ReplicationTaskLastAssessmentDate" :: NullOrUndefined (TStamp), "AssessmentStatus" :: NullOrUndefined (String), "AssessmentResultsFile" :: NullOrUndefined (String), "AssessmentResults" :: NullOrUndefined (String), "S3ObjectUrl" :: NullOrUndefined (String) }
```

<p> The task assessment report in JSON format. </p>

##### Instances
``` purescript
Newtype ReplicationTaskAssessmentResult _
```

#### `ReplicationTaskAssessmentResultList`

``` purescript
newtype ReplicationTaskAssessmentResultList
  = ReplicationTaskAssessmentResultList (Array ReplicationTaskAssessmentResult)
```

##### Instances
``` purescript
Newtype ReplicationTaskAssessmentResultList _
```

#### `ReplicationTaskList`

``` purescript
newtype ReplicationTaskList
  = ReplicationTaskList (Array ReplicationTask)
```

##### Instances
``` purescript
Newtype ReplicationTaskList _
```

#### `ReplicationTaskStats`

``` purescript
newtype ReplicationTaskStats
  = ReplicationTaskStats { "FullLoadProgressPercent" :: NullOrUndefined (Int), "ElapsedTimeMillis" :: NullOrUndefined (Number), "TablesLoaded" :: NullOrUndefined (Int), "TablesLoading" :: NullOrUndefined (Int), "TablesQueued" :: NullOrUndefined (Int), "TablesErrored" :: NullOrUndefined (Int) }
```

<p/>

##### Instances
``` purescript
Newtype ReplicationTaskStats _
```

#### `ResourceAlreadyExistsFault`

``` purescript
newtype ResourceAlreadyExistsFault
  = ResourceAlreadyExistsFault { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The resource you are attempting to create already exists.</p>

##### Instances
``` purescript
Newtype ResourceAlreadyExistsFault _
```

#### `ResourceNotFoundFault`

``` purescript
newtype ResourceNotFoundFault
  = ResourceNotFoundFault { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The resource could not be found.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundFault _
```

#### `ResourceQuotaExceededFault`

``` purescript
newtype ResourceQuotaExceededFault
  = ResourceQuotaExceededFault { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The quota for this resource quota has been exceeded.</p>

##### Instances
``` purescript
Newtype ResourceQuotaExceededFault _
```

#### `S3Settings`

``` purescript
newtype S3Settings
  = S3Settings { "ServiceAccessRoleArn" :: NullOrUndefined (String), "ExternalTableDefinition" :: NullOrUndefined (String), "CsvRowDelimiter" :: NullOrUndefined (String), "CsvDelimiter" :: NullOrUndefined (String), "BucketFolder" :: NullOrUndefined (String), "BucketName" :: NullOrUndefined (String), "CompressionType" :: NullOrUndefined (CompressionTypeValue) }
```

<p/>

##### Instances
``` purescript
Newtype S3Settings _
```

#### `SNSInvalidTopicFault`

``` purescript
newtype SNSInvalidTopicFault
  = SNSInvalidTopicFault { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The SNS topic is invalid.</p>

##### Instances
``` purescript
Newtype SNSInvalidTopicFault _
```

#### `SNSNoAuthorizationFault`

``` purescript
newtype SNSNoAuthorizationFault
  = SNSNoAuthorizationFault { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>You are not authorized for the SNS subscription.</p>

##### Instances
``` purescript
Newtype SNSNoAuthorizationFault _
```

#### `SchemaList`

``` purescript
newtype SchemaList
  = SchemaList (Array String)
```

##### Instances
``` purescript
Newtype SchemaList _
```

#### `SecretString`

``` purescript
newtype SecretString
  = SecretString String
```

##### Instances
``` purescript
Newtype SecretString _
```

#### `SourceIdsList`

``` purescript
newtype SourceIdsList
  = SourceIdsList (Array String)
```

##### Instances
``` purescript
Newtype SourceIdsList _
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

#### `StartReplicationTaskAssessmentMessage`

``` purescript
newtype StartReplicationTaskAssessmentMessage
  = StartReplicationTaskAssessmentMessage { "ReplicationTaskArn" :: String }
```

<p/>

##### Instances
``` purescript
Newtype StartReplicationTaskAssessmentMessage _
```

#### `StartReplicationTaskAssessmentResponse`

``` purescript
newtype StartReplicationTaskAssessmentResponse
  = StartReplicationTaskAssessmentResponse { "ReplicationTask" :: NullOrUndefined (ReplicationTask) }
```

<p/>

##### Instances
``` purescript
Newtype StartReplicationTaskAssessmentResponse _
```

#### `StartReplicationTaskMessage`

``` purescript
newtype StartReplicationTaskMessage
  = StartReplicationTaskMessage { "ReplicationTaskArn" :: String, "StartReplicationTaskType" :: StartReplicationTaskTypeValue, "CdcStartTime" :: NullOrUndefined (TStamp) }
```

<p/>

##### Instances
``` purescript
Newtype StartReplicationTaskMessage _
```

#### `StartReplicationTaskResponse`

``` purescript
newtype StartReplicationTaskResponse
  = StartReplicationTaskResponse { "ReplicationTask" :: NullOrUndefined (ReplicationTask) }
```

<p/>

##### Instances
``` purescript
Newtype StartReplicationTaskResponse _
```

#### `StartReplicationTaskTypeValue`

``` purescript
newtype StartReplicationTaskTypeValue
  = StartReplicationTaskTypeValue String
```

##### Instances
``` purescript
Newtype StartReplicationTaskTypeValue _
```

#### `StopReplicationTaskMessage`

``` purescript
newtype StopReplicationTaskMessage
  = StopReplicationTaskMessage { "ReplicationTaskArn" :: String }
```

<p/>

##### Instances
``` purescript
Newtype StopReplicationTaskMessage _
```

#### `StopReplicationTaskResponse`

``` purescript
newtype StopReplicationTaskResponse
  = StopReplicationTaskResponse { "ReplicationTask" :: NullOrUndefined (ReplicationTask) }
```

<p/>

##### Instances
``` purescript
Newtype StopReplicationTaskResponse _
```

#### `StorageQuotaExceededFault`

``` purescript
newtype StorageQuotaExceededFault
  = StorageQuotaExceededFault { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The storage quota has been exceeded.</p>

##### Instances
``` purescript
Newtype StorageQuotaExceededFault _
```

#### `Subnet`

``` purescript
newtype Subnet
  = Subnet { "SubnetIdentifier" :: NullOrUndefined (String), "SubnetAvailabilityZone" :: NullOrUndefined (AvailabilityZone), "SubnetStatus" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype Subnet _
```

#### `SubnetAlreadyInUse`

``` purescript
newtype SubnetAlreadyInUse
  = SubnetAlreadyInUse { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified subnet is already in use.</p>

##### Instances
``` purescript
Newtype SubnetAlreadyInUse _
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

#### `SubnetList`

``` purescript
newtype SubnetList
  = SubnetList (Array Subnet)
```

##### Instances
``` purescript
Newtype SubnetList _
```

#### `SupportedEndpointType`

``` purescript
newtype SupportedEndpointType
  = SupportedEndpointType { "EngineName" :: NullOrUndefined (String), "SupportsCDC" :: NullOrUndefined (Boolean), "EndpointType" :: NullOrUndefined (ReplicationEndpointTypeValue) }
```

<p/>

##### Instances
``` purescript
Newtype SupportedEndpointType _
```

#### `SupportedEndpointTypeList`

``` purescript
newtype SupportedEndpointTypeList
  = SupportedEndpointTypeList (Array SupportedEndpointType)
```

##### Instances
``` purescript
Newtype SupportedEndpointTypeList _
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

#### `TableListToReload`

``` purescript
newtype TableListToReload
  = TableListToReload (Array TableToReload)
```

##### Instances
``` purescript
Newtype TableListToReload _
```

#### `TableStatistics`

``` purescript
newtype TableStatistics
  = TableStatistics { "SchemaName" :: NullOrUndefined (String), "TableName" :: NullOrUndefined (String), "Inserts" :: NullOrUndefined (Number), "Deletes" :: NullOrUndefined (Number), "Updates" :: NullOrUndefined (Number), "Ddls" :: NullOrUndefined (Number), "FullLoadRows" :: NullOrUndefined (Number), "FullLoadCondtnlChkFailedRows" :: NullOrUndefined (Number), "FullLoadErrorRows" :: NullOrUndefined (Number), "LastUpdateTime" :: NullOrUndefined (TStamp), "TableState" :: NullOrUndefined (String), "ValidationPendingRecords" :: NullOrUndefined (Number), "ValidationFailedRecords" :: NullOrUndefined (Number), "ValidationSuspendedRecords" :: NullOrUndefined (Number), "ValidationState" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype TableStatistics _
```

#### `TableStatisticsList`

``` purescript
newtype TableStatisticsList
  = TableStatisticsList (Array TableStatistics)
```

##### Instances
``` purescript
Newtype TableStatisticsList _
```

#### `TableToReload`

``` purescript
newtype TableToReload
  = TableToReload { "SchemaName" :: NullOrUndefined (String), "TableName" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype TableToReload _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (String), "Value" :: NullOrUndefined (String) }
```

<p/>

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

#### `TestConnectionMessage`

``` purescript
newtype TestConnectionMessage
  = TestConnectionMessage { "ReplicationInstanceArn" :: String, "EndpointArn" :: String }
```

<p/>

##### Instances
``` purescript
Newtype TestConnectionMessage _
```

#### `TestConnectionResponse`

``` purescript
newtype TestConnectionResponse
  = TestConnectionResponse { "Connection" :: NullOrUndefined (Connection) }
```

<p/>

##### Instances
``` purescript
Newtype TestConnectionResponse _
```

#### `UpgradeDependencyFailureFault`

``` purescript
newtype UpgradeDependencyFailureFault
  = UpgradeDependencyFailureFault { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>An upgrade dependency is preventing the database migration.</p>

##### Instances
``` purescript
Newtype UpgradeDependencyFailureFault _
```

#### `VpcSecurityGroupIdList`

``` purescript
newtype VpcSecurityGroupIdList
  = VpcSecurityGroupIdList (Array String)
```

##### Instances
``` purescript
Newtype VpcSecurityGroupIdList _
```

#### `VpcSecurityGroupMembership`

``` purescript
newtype VpcSecurityGroupMembership
  = VpcSecurityGroupMembership { "VpcSecurityGroupId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String) }
```

<p/>

##### Instances
``` purescript
Newtype VpcSecurityGroupMembership _
```

#### `VpcSecurityGroupMembershipList`

``` purescript
newtype VpcSecurityGroupMembershipList
  = VpcSecurityGroupMembershipList (Array VpcSecurityGroupMembership)
```

##### Instances
``` purescript
Newtype VpcSecurityGroupMembershipList _
```


