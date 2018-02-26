## Module AWS.RDS

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addSourceIdentifierToSubscription`

``` purescript
addSourceIdentifierToSubscription :: forall eff. AddSourceIdentifierToSubscriptionMessage -> Aff (err :: RequestError | eff) AddSourceIdentifierToSubscriptionResult
```

#### `addTagsToResource`

``` purescript
addTagsToResource :: forall eff. AddTagsToResourceMessage -> Aff (err :: RequestError | eff) Unit
```

#### `authorizeDBSecurityGroupIngress`

``` purescript
authorizeDBSecurityGroupIngress :: forall eff. AuthorizeDBSecurityGroupIngressMessage -> Aff (err :: RequestError | eff) AuthorizeDBSecurityGroupIngressResult
```

#### `copyDBSnapshot`

``` purescript
copyDBSnapshot :: forall eff. CopyDBSnapshotMessage -> Aff (err :: RequestError | eff) CopyDBSnapshotResult
```

#### `createDBInstance`

``` purescript
createDBInstance :: forall eff. CreateDBInstanceMessage -> Aff (err :: RequestError | eff) CreateDBInstanceResult
```

#### `createDBInstanceReadReplica`

``` purescript
createDBInstanceReadReplica :: forall eff. CreateDBInstanceReadReplicaMessage -> Aff (err :: RequestError | eff) CreateDBInstanceReadReplicaResult
```

#### `createDBParameterGroup`

``` purescript
createDBParameterGroup :: forall eff. CreateDBParameterGroupMessage -> Aff (err :: RequestError | eff) CreateDBParameterGroupResult
```

#### `createDBSecurityGroup`

``` purescript
createDBSecurityGroup :: forall eff. CreateDBSecurityGroupMessage -> Aff (err :: RequestError | eff) CreateDBSecurityGroupResult
```

#### `createDBSnapshot`

``` purescript
createDBSnapshot :: forall eff. CreateDBSnapshotMessage -> Aff (err :: RequestError | eff) CreateDBSnapshotResult
```

#### `createDBSubnetGroup`

``` purescript
createDBSubnetGroup :: forall eff. CreateDBSubnetGroupMessage -> Aff (err :: RequestError | eff) CreateDBSubnetGroupResult
```

#### `createEventSubscription`

``` purescript
createEventSubscription :: forall eff. CreateEventSubscriptionMessage -> Aff (err :: RequestError | eff) CreateEventSubscriptionResult
```

#### `createOptionGroup`

``` purescript
createOptionGroup :: forall eff. CreateOptionGroupMessage -> Aff (err :: RequestError | eff) CreateOptionGroupResult
```

#### `deleteDBInstance`

``` purescript
deleteDBInstance :: forall eff. DeleteDBInstanceMessage -> Aff (err :: RequestError | eff) DeleteDBInstanceResult
```

#### `deleteDBParameterGroup`

``` purescript
deleteDBParameterGroup :: forall eff. DeleteDBParameterGroupMessage -> Aff (err :: RequestError | eff) Unit
```

#### `deleteDBSecurityGroup`

``` purescript
deleteDBSecurityGroup :: forall eff. DeleteDBSecurityGroupMessage -> Aff (err :: RequestError | eff) Unit
```

#### `deleteDBSnapshot`

``` purescript
deleteDBSnapshot :: forall eff. DeleteDBSnapshotMessage -> Aff (err :: RequestError | eff) DeleteDBSnapshotResult
```

#### `deleteDBSubnetGroup`

``` purescript
deleteDBSubnetGroup :: forall eff. DeleteDBSubnetGroupMessage -> Aff (err :: RequestError | eff) Unit
```

#### `deleteEventSubscription`

``` purescript
deleteEventSubscription :: forall eff. DeleteEventSubscriptionMessage -> Aff (err :: RequestError | eff) DeleteEventSubscriptionResult
```

#### `deleteOptionGroup`

``` purescript
deleteOptionGroup :: forall eff. DeleteOptionGroupMessage -> Aff (err :: RequestError | eff) Unit
```

#### `describeDBEngineVersions`

``` purescript
describeDBEngineVersions :: forall eff. DescribeDBEngineVersionsMessage -> Aff (err :: RequestError | eff) DBEngineVersionMessage
```

#### `describeDBInstances`

``` purescript
describeDBInstances :: forall eff. DescribeDBInstancesMessage -> Aff (err :: RequestError | eff) DBInstanceMessage
```

#### `describeDBParameterGroups`

``` purescript
describeDBParameterGroups :: forall eff. DescribeDBParameterGroupsMessage -> Aff (err :: RequestError | eff) DBParameterGroupsMessage
```

#### `describeDBParameters`

``` purescript
describeDBParameters :: forall eff. DescribeDBParametersMessage -> Aff (err :: RequestError | eff) DBParameterGroupDetails
```

#### `describeDBSecurityGroups`

``` purescript
describeDBSecurityGroups :: forall eff. DescribeDBSecurityGroupsMessage -> Aff (err :: RequestError | eff) DBSecurityGroupMessage
```

#### `describeDBSnapshots`

``` purescript
describeDBSnapshots :: forall eff. DescribeDBSnapshotsMessage -> Aff (err :: RequestError | eff) DBSnapshotMessage
```

#### `describeDBSubnetGroups`

``` purescript
describeDBSubnetGroups :: forall eff. DescribeDBSubnetGroupsMessage -> Aff (err :: RequestError | eff) DBSubnetGroupMessage
```

#### `describeEngineDefaultParameters`

``` purescript
describeEngineDefaultParameters :: forall eff. DescribeEngineDefaultParametersMessage -> Aff (err :: RequestError | eff) DescribeEngineDefaultParametersResult
```

#### `describeEventCategories`

``` purescript
describeEventCategories :: forall eff. DescribeEventCategoriesMessage -> Aff (err :: RequestError | eff) EventCategoriesMessage
```

#### `describeEventSubscriptions`

``` purescript
describeEventSubscriptions :: forall eff. DescribeEventSubscriptionsMessage -> Aff (err :: RequestError | eff) EventSubscriptionsMessage
```

#### `describeEvents`

``` purescript
describeEvents :: forall eff. DescribeEventsMessage -> Aff (err :: RequestError | eff) EventsMessage
```

#### `describeOptionGroupOptions`

``` purescript
describeOptionGroupOptions :: forall eff. DescribeOptionGroupOptionsMessage -> Aff (err :: RequestError | eff) OptionGroupOptionsMessage
```

#### `describeOptionGroups`

``` purescript
describeOptionGroups :: forall eff. DescribeOptionGroupsMessage -> Aff (err :: RequestError | eff) OptionGroups
```

#### `describeOrderableDBInstanceOptions`

``` purescript
describeOrderableDBInstanceOptions :: forall eff. DescribeOrderableDBInstanceOptionsMessage -> Aff (err :: RequestError | eff) OrderableDBInstanceOptionsMessage
```

#### `describeReservedDBInstances`

``` purescript
describeReservedDBInstances :: forall eff. DescribeReservedDBInstancesMessage -> Aff (err :: RequestError | eff) ReservedDBInstanceMessage
```

#### `describeReservedDBInstancesOfferings`

``` purescript
describeReservedDBInstancesOfferings :: forall eff. DescribeReservedDBInstancesOfferingsMessage -> Aff (err :: RequestError | eff) ReservedDBInstancesOfferingMessage
```

#### `listTagsForResource`

``` purescript
listTagsForResource :: forall eff. ListTagsForResourceMessage -> Aff (err :: RequestError | eff) TagListMessage
```

#### `modifyDBInstance`

``` purescript
modifyDBInstance :: forall eff. ModifyDBInstanceMessage -> Aff (err :: RequestError | eff) ModifyDBInstanceResult
```

#### `modifyDBParameterGroup`

``` purescript
modifyDBParameterGroup :: forall eff. ModifyDBParameterGroupMessage -> Aff (err :: RequestError | eff) DBParameterGroupNameMessage
```

#### `modifyDBSubnetGroup`

``` purescript
modifyDBSubnetGroup :: forall eff. ModifyDBSubnetGroupMessage -> Aff (err :: RequestError | eff) ModifyDBSubnetGroupResult
```

#### `modifyEventSubscription`

``` purescript
modifyEventSubscription :: forall eff. ModifyEventSubscriptionMessage -> Aff (err :: RequestError | eff) ModifyEventSubscriptionResult
```

#### `modifyOptionGroup`

``` purescript
modifyOptionGroup :: forall eff. ModifyOptionGroupMessage -> Aff (err :: RequestError | eff) ModifyOptionGroupResult
```

#### `promoteReadReplica`

``` purescript
promoteReadReplica :: forall eff. PromoteReadReplicaMessage -> Aff (err :: RequestError | eff) PromoteReadReplicaResult
```

#### `purchaseReservedDBInstancesOffering`

``` purescript
purchaseReservedDBInstancesOffering :: forall eff. PurchaseReservedDBInstancesOfferingMessage -> Aff (err :: RequestError | eff) PurchaseReservedDBInstancesOfferingResult
```

#### `rebootDBInstance`

``` purescript
rebootDBInstance :: forall eff. RebootDBInstanceMessage -> Aff (err :: RequestError | eff) RebootDBInstanceResult
```

#### `removeSourceIdentifierFromSubscription`

``` purescript
removeSourceIdentifierFromSubscription :: forall eff. RemoveSourceIdentifierFromSubscriptionMessage -> Aff (err :: RequestError | eff) RemoveSourceIdentifierFromSubscriptionResult
```

#### `removeTagsFromResource`

``` purescript
removeTagsFromResource :: forall eff. RemoveTagsFromResourceMessage -> Aff (err :: RequestError | eff) Unit
```

#### `resetDBParameterGroup`

``` purescript
resetDBParameterGroup :: forall eff. ResetDBParameterGroupMessage -> Aff (err :: RequestError | eff) DBParameterGroupNameMessage
```

#### `restoreDBInstanceFromDBSnapshot`

``` purescript
restoreDBInstanceFromDBSnapshot :: forall eff. RestoreDBInstanceFromDBSnapshotMessage -> Aff (err :: RequestError | eff) RestoreDBInstanceFromDBSnapshotResult
```

#### `restoreDBInstanceToPointInTime`

``` purescript
restoreDBInstanceToPointInTime :: forall eff. RestoreDBInstanceToPointInTimeMessage -> Aff (err :: RequestError | eff) RestoreDBInstanceToPointInTimeResult
```

#### `revokeDBSecurityGroupIngress`

``` purescript
revokeDBSecurityGroupIngress :: forall eff. RevokeDBSecurityGroupIngressMessage -> Aff (err :: RequestError | eff) RevokeDBSecurityGroupIngressResult
```

#### `AddSourceIdentifierToSubscriptionMessage`

``` purescript
newtype AddSourceIdentifierToSubscriptionMessage
  = AddSourceIdentifierToSubscriptionMessage { "SubscriptionName" :: String, "SourceIdentifier" :: String }
```

#### `AddSourceIdentifierToSubscriptionResult`

``` purescript
newtype AddSourceIdentifierToSubscriptionResult
  = AddSourceIdentifierToSubscriptionResult { "EventSubscription" :: NullOrUndefined (EventSubscription) }
```

#### `AddTagsToResourceMessage`

``` purescript
newtype AddTagsToResourceMessage
  = AddTagsToResourceMessage { "ResourceName" :: String, "Tags" :: TagList }
```

#### `ApplyMethod`

``` purescript
newtype ApplyMethod
  = ApplyMethod String
```

#### `AuthorizationAlreadyExistsFault`

``` purescript
newtype AuthorizationAlreadyExistsFault
  = AuthorizationAlreadyExistsFault {  }
```

<p>The specified CIDRIP or EC2 security group is already authorized for the specified DB security group.</p>

#### `AuthorizationNotFoundFault`

``` purescript
newtype AuthorizationNotFoundFault
  = AuthorizationNotFoundFault {  }
```

<p>Specified CIDRIP or EC2 security group is not authorized for the specified DB security group.</p> <p>RDS may not also be authorized via IAM to perform necessary actions on your behalf.</p>

#### `AuthorizationQuotaExceededFault`

``` purescript
newtype AuthorizationQuotaExceededFault
  = AuthorizationQuotaExceededFault {  }
```

<p>DB security group authorization quota has been reached.</p>

#### `AuthorizeDBSecurityGroupIngressMessage`

``` purescript
newtype AuthorizeDBSecurityGroupIngressMessage
  = AuthorizeDBSecurityGroupIngressMessage { "DBSecurityGroupName" :: String, "CIDRIP" :: NullOrUndefined (String), "EC2SecurityGroupName" :: NullOrUndefined (String), "EC2SecurityGroupId" :: NullOrUndefined (String), "EC2SecurityGroupOwnerId" :: NullOrUndefined (String) }
```

#### `AuthorizeDBSecurityGroupIngressResult`

``` purescript
newtype AuthorizeDBSecurityGroupIngressResult
  = AuthorizeDBSecurityGroupIngressResult { "DBSecurityGroup" :: NullOrUndefined (DBSecurityGroup) }
```

#### `AvailabilityZone`

``` purescript
newtype AvailabilityZone
  = AvailabilityZone { "Name" :: NullOrUndefined (String), "ProvisionedIopsCapable" :: NullOrUndefined (Boolean) }
```

#### `AvailabilityZoneList`

``` purescript
newtype AvailabilityZoneList
  = AvailabilityZoneList (Array AvailabilityZone)
```

#### `BooleanOptional`

``` purescript
newtype BooleanOptional
  = BooleanOptional Boolean
```

#### `CharacterSet`

``` purescript
newtype CharacterSet
  = CharacterSet { "CharacterSetName" :: NullOrUndefined (String), "CharacterSetDescription" :: NullOrUndefined (String) }
```

#### `CopyDBSnapshotMessage`

``` purescript
newtype CopyDBSnapshotMessage
  = CopyDBSnapshotMessage { "SourceDBSnapshotIdentifier" :: String, "TargetDBSnapshotIdentifier" :: String }
```

#### `CopyDBSnapshotResult`

``` purescript
newtype CopyDBSnapshotResult
  = CopyDBSnapshotResult { "DBSnapshot" :: NullOrUndefined (DBSnapshot) }
```

#### `CreateDBInstanceMessage`

``` purescript
newtype CreateDBInstanceMessage
  = CreateDBInstanceMessage { "DBName" :: NullOrUndefined (String), "DBInstanceIdentifier" :: String, "AllocatedStorage" :: IntegerOptional, "DBInstanceClass" :: String, "Engine" :: String, "MasterUsername" :: String, "MasterUserPassword" :: String, "DBSecurityGroups" :: NullOrUndefined (DBSecurityGroupNameList), "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList), "AvailabilityZone" :: NullOrUndefined (String), "DBSubnetGroupName" :: NullOrUndefined (String), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "DBParameterGroupName" :: NullOrUndefined (String), "BackupRetentionPeriod" :: NullOrUndefined (IntegerOptional), "PreferredBackupWindow" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "MultiAZ" :: NullOrUndefined (BooleanOptional), "EngineVersion" :: NullOrUndefined (String), "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional), "LicenseModel" :: NullOrUndefined (String), "Iops" :: NullOrUndefined (IntegerOptional), "OptionGroupName" :: NullOrUndefined (String), "CharacterSetName" :: NullOrUndefined (String), "PubliclyAccessible" :: NullOrUndefined (BooleanOptional) }
```

#### `CreateDBInstanceReadReplicaMessage`

``` purescript
newtype CreateDBInstanceReadReplicaMessage
  = CreateDBInstanceReadReplicaMessage { "DBInstanceIdentifier" :: String, "SourceDBInstanceIdentifier" :: String, "DBInstanceClass" :: NullOrUndefined (String), "AvailabilityZone" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional), "Iops" :: NullOrUndefined (IntegerOptional), "OptionGroupName" :: NullOrUndefined (String), "PubliclyAccessible" :: NullOrUndefined (BooleanOptional) }
```

#### `CreateDBInstanceReadReplicaResult`

``` purescript
newtype CreateDBInstanceReadReplicaResult
  = CreateDBInstanceReadReplicaResult { "DBInstance" :: NullOrUndefined (DBInstance) }
```

#### `CreateDBInstanceResult`

``` purescript
newtype CreateDBInstanceResult
  = CreateDBInstanceResult { "DBInstance" :: NullOrUndefined (DBInstance) }
```

#### `CreateDBParameterGroupMessage`

``` purescript
newtype CreateDBParameterGroupMessage
  = CreateDBParameterGroupMessage { "DBParameterGroupName" :: String, "DBParameterGroupFamily" :: String, "Description" :: String }
```

#### `CreateDBParameterGroupResult`

``` purescript
newtype CreateDBParameterGroupResult
  = CreateDBParameterGroupResult { "DBParameterGroup" :: NullOrUndefined (DBParameterGroup) }
```

#### `CreateDBSecurityGroupMessage`

``` purescript
newtype CreateDBSecurityGroupMessage
  = CreateDBSecurityGroupMessage { "DBSecurityGroupName" :: String, "DBSecurityGroupDescription" :: String }
```

#### `CreateDBSecurityGroupResult`

``` purescript
newtype CreateDBSecurityGroupResult
  = CreateDBSecurityGroupResult { "DBSecurityGroup" :: NullOrUndefined (DBSecurityGroup) }
```

#### `CreateDBSnapshotMessage`

``` purescript
newtype CreateDBSnapshotMessage
  = CreateDBSnapshotMessage { "DBSnapshotIdentifier" :: String, "DBInstanceIdentifier" :: String }
```

#### `CreateDBSnapshotResult`

``` purescript
newtype CreateDBSnapshotResult
  = CreateDBSnapshotResult { "DBSnapshot" :: NullOrUndefined (DBSnapshot) }
```

#### `CreateDBSubnetGroupMessage`

``` purescript
newtype CreateDBSubnetGroupMessage
  = CreateDBSubnetGroupMessage { "DBSubnetGroupName" :: String, "DBSubnetGroupDescription" :: String, "SubnetIds" :: SubnetIdentifierList }
```

#### `CreateDBSubnetGroupResult`

``` purescript
newtype CreateDBSubnetGroupResult
  = CreateDBSubnetGroupResult { "DBSubnetGroup" :: NullOrUndefined (DBSubnetGroup) }
```

#### `CreateEventSubscriptionMessage`

``` purescript
newtype CreateEventSubscriptionMessage
  = CreateEventSubscriptionMessage { "SubscriptionName" :: String, "SnsTopicArn" :: String, "SourceType" :: NullOrUndefined (String), "EventCategories" :: NullOrUndefined (EventCategoriesList), "SourceIds" :: NullOrUndefined (SourceIdsList), "Enabled" :: NullOrUndefined (BooleanOptional) }
```

#### `CreateEventSubscriptionResult`

``` purescript
newtype CreateEventSubscriptionResult
  = CreateEventSubscriptionResult { "EventSubscription" :: NullOrUndefined (EventSubscription) }
```

#### `CreateOptionGroupMessage`

``` purescript
newtype CreateOptionGroupMessage
  = CreateOptionGroupMessage { "OptionGroupName" :: String, "EngineName" :: String, "MajorEngineVersion" :: String, "OptionGroupDescription" :: String }
```

#### `CreateOptionGroupResult`

``` purescript
newtype CreateOptionGroupResult
  = CreateOptionGroupResult { "OptionGroup" :: NullOrUndefined (OptionGroup) }
```

#### `DBEngineVersion`

``` purescript
newtype DBEngineVersion
  = DBEngineVersion { "Engine" :: NullOrUndefined (String), "EngineVersion" :: NullOrUndefined (String), "DBParameterGroupFamily" :: NullOrUndefined (String), "DBEngineDescription" :: NullOrUndefined (String), "DBEngineVersionDescription" :: NullOrUndefined (String), "DefaultCharacterSet" :: NullOrUndefined (CharacterSet), "SupportedCharacterSets" :: NullOrUndefined (SupportedCharacterSetsList) }
```

#### `DBEngineVersionList`

``` purescript
newtype DBEngineVersionList
  = DBEngineVersionList (Array DBEngineVersion)
```

#### `DBEngineVersionMessage`

``` purescript
newtype DBEngineVersionMessage
  = DBEngineVersionMessage { "Marker" :: NullOrUndefined (String), "DBEngineVersions" :: NullOrUndefined (DBEngineVersionList) }
```

#### `DBInstance`

``` purescript
newtype DBInstance
  = DBInstance { "DBInstanceIdentifier" :: NullOrUndefined (String), "DBInstanceClass" :: NullOrUndefined (String), "Engine" :: NullOrUndefined (String), "DBInstanceStatus" :: NullOrUndefined (String), "MasterUsername" :: NullOrUndefined (String), "DBName" :: NullOrUndefined (String), "Endpoint" :: NullOrUndefined (Endpoint), "AllocatedStorage" :: NullOrUndefined (Int), "InstanceCreateTime" :: NullOrUndefined (TStamp), "PreferredBackupWindow" :: NullOrUndefined (String), "BackupRetentionPeriod" :: NullOrUndefined (Int), "DBSecurityGroups" :: NullOrUndefined (DBSecurityGroupMembershipList), "VpcSecurityGroups" :: NullOrUndefined (VpcSecurityGroupMembershipList), "DBParameterGroups" :: NullOrUndefined (DBParameterGroupStatusList), "AvailabilityZone" :: NullOrUndefined (String), "DBSubnetGroup" :: NullOrUndefined (DBSubnetGroup), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "PendingModifiedValues" :: NullOrUndefined (PendingModifiedValues), "LatestRestorableTime" :: NullOrUndefined (TStamp), "MultiAZ" :: NullOrUndefined (Boolean), "EngineVersion" :: NullOrUndefined (String), "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean), "ReadReplicaSourceDBInstanceIdentifier" :: NullOrUndefined (String), "ReadReplicaDBInstanceIdentifiers" :: NullOrUndefined (ReadReplicaDBInstanceIdentifierList), "LicenseModel" :: NullOrUndefined (String), "Iops" :: NullOrUndefined (IntegerOptional), "OptionGroupMembership" :: NullOrUndefined (OptionGroupMembership), "CharacterSetName" :: NullOrUndefined (String), "SecondaryAvailabilityZone" :: NullOrUndefined (String), "PubliclyAccessible" :: NullOrUndefined (Boolean) }
```

#### `DBInstanceAlreadyExistsFault`

``` purescript
newtype DBInstanceAlreadyExistsFault
  = DBInstanceAlreadyExistsFault {  }
```

<p>User already has a DB instance with the given identifier.</p>

#### `DBInstanceList`

``` purescript
newtype DBInstanceList
  = DBInstanceList (Array DBInstance)
```

#### `DBInstanceMessage`

``` purescript
newtype DBInstanceMessage
  = DBInstanceMessage { "Marker" :: NullOrUndefined (String), "DBInstances" :: NullOrUndefined (DBInstanceList) }
```

#### `DBInstanceNotFoundFault`

``` purescript
newtype DBInstanceNotFoundFault
  = DBInstanceNotFoundFault {  }
```

<p> <i>DBInstanceIdentifier</i> does not refer to an existing DB instance. </p>

#### `DBParameterGroup`

``` purescript
newtype DBParameterGroup
  = DBParameterGroup { "DBParameterGroupName" :: NullOrUndefined (String), "DBParameterGroupFamily" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

#### `DBParameterGroupAlreadyExistsFault`

``` purescript
newtype DBParameterGroupAlreadyExistsFault
  = DBParameterGroupAlreadyExistsFault {  }
```

<p>A DB parameter group with the same name exists.</p>

#### `DBParameterGroupDetails`

``` purescript
newtype DBParameterGroupDetails
  = DBParameterGroupDetails { "Parameters" :: NullOrUndefined (ParametersList), "Marker" :: NullOrUndefined (String) }
```

#### `DBParameterGroupList`

``` purescript
newtype DBParameterGroupList
  = DBParameterGroupList (Array DBParameterGroup)
```

#### `DBParameterGroupNameMessage`

``` purescript
newtype DBParameterGroupNameMessage
  = DBParameterGroupNameMessage { "DBParameterGroupName" :: NullOrUndefined (String) }
```

#### `DBParameterGroupNotFoundFault`

``` purescript
newtype DBParameterGroupNotFoundFault
  = DBParameterGroupNotFoundFault {  }
```

<p> <i>DBParameterGroupName</i> does not refer to an existing DB parameter group. </p>

#### `DBParameterGroupQuotaExceededFault`

``` purescript
newtype DBParameterGroupQuotaExceededFault
  = DBParameterGroupQuotaExceededFault {  }
```

<p>Request would result in user exceeding the allowed number of DB parameter groups.</p>

#### `DBParameterGroupStatus`

``` purescript
newtype DBParameterGroupStatus
  = DBParameterGroupStatus { "DBParameterGroupName" :: NullOrUndefined (String), "ParameterApplyStatus" :: NullOrUndefined (String) }
```

#### `DBParameterGroupStatusList`

``` purescript
newtype DBParameterGroupStatusList
  = DBParameterGroupStatusList (Array DBParameterGroupStatus)
```

#### `DBParameterGroupsMessage`

``` purescript
newtype DBParameterGroupsMessage
  = DBParameterGroupsMessage { "Marker" :: NullOrUndefined (String), "DBParameterGroups" :: NullOrUndefined (DBParameterGroupList) }
```

#### `DBSecurityGroup`

``` purescript
newtype DBSecurityGroup
  = DBSecurityGroup { "OwnerId" :: NullOrUndefined (String), "DBSecurityGroupName" :: NullOrUndefined (String), "DBSecurityGroupDescription" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "EC2SecurityGroups" :: NullOrUndefined (EC2SecurityGroupList), "IPRanges" :: NullOrUndefined (IPRangeList) }
```

#### `DBSecurityGroupAlreadyExistsFault`

``` purescript
newtype DBSecurityGroupAlreadyExistsFault
  = DBSecurityGroupAlreadyExistsFault {  }
```

<p> A DB security group with the name specified in <i>DBSecurityGroupName</i> already exists. </p>

#### `DBSecurityGroupMembership`

``` purescript
newtype DBSecurityGroupMembership
  = DBSecurityGroupMembership { "DBSecurityGroupName" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String) }
```

#### `DBSecurityGroupMembershipList`

``` purescript
newtype DBSecurityGroupMembershipList
  = DBSecurityGroupMembershipList (Array DBSecurityGroupMembership)
```

#### `DBSecurityGroupMessage`

``` purescript
newtype DBSecurityGroupMessage
  = DBSecurityGroupMessage { "Marker" :: NullOrUndefined (String), "DBSecurityGroups" :: NullOrUndefined (DBSecurityGroups) }
```

#### `DBSecurityGroupNameList`

``` purescript
newtype DBSecurityGroupNameList
  = DBSecurityGroupNameList (Array String)
```

#### `DBSecurityGroupNotFoundFault`

``` purescript
newtype DBSecurityGroupNotFoundFault
  = DBSecurityGroupNotFoundFault {  }
```

<p> <i>DBSecurityGroupName</i> does not refer to an existing DB security group. </p>

#### `DBSecurityGroupNotSupportedFault`

``` purescript
newtype DBSecurityGroupNotSupportedFault
  = DBSecurityGroupNotSupportedFault {  }
```

<p>A DB security group is not allowed for this action.</p>

#### `DBSecurityGroupQuotaExceededFault`

``` purescript
newtype DBSecurityGroupQuotaExceededFault
  = DBSecurityGroupQuotaExceededFault {  }
```

<p>Request would result in user exceeding the allowed number of DB security groups.</p>

#### `DBSecurityGroups`

``` purescript
newtype DBSecurityGroups
  = DBSecurityGroups (Array DBSecurityGroup)
```

#### `DBSnapshot`

``` purescript
newtype DBSnapshot
  = DBSnapshot { "DBSnapshotIdentifier" :: NullOrUndefined (String), "DBInstanceIdentifier" :: NullOrUndefined (String), "SnapshotCreateTime" :: NullOrUndefined (TStamp), "Engine" :: NullOrUndefined (String), "AllocatedStorage" :: NullOrUndefined (Int), "Status" :: NullOrUndefined (String), "Port" :: NullOrUndefined (Int), "AvailabilityZone" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "InstanceCreateTime" :: NullOrUndefined (TStamp), "MasterUsername" :: NullOrUndefined (String), "EngineVersion" :: NullOrUndefined (String), "LicenseModel" :: NullOrUndefined (String), "SnapshotType" :: NullOrUndefined (String), "Iops" :: NullOrUndefined (IntegerOptional) }
```

#### `DBSnapshotAlreadyExistsFault`

``` purescript
newtype DBSnapshotAlreadyExistsFault
  = DBSnapshotAlreadyExistsFault {  }
```

<p> <i>DBSnapshotIdentifier</i> is already used by an existing snapshot. </p>

#### `DBSnapshotList`

``` purescript
newtype DBSnapshotList
  = DBSnapshotList (Array DBSnapshot)
```

#### `DBSnapshotMessage`

``` purescript
newtype DBSnapshotMessage
  = DBSnapshotMessage { "Marker" :: NullOrUndefined (String), "DBSnapshots" :: NullOrUndefined (DBSnapshotList) }
```

#### `DBSnapshotNotFoundFault`

``` purescript
newtype DBSnapshotNotFoundFault
  = DBSnapshotNotFoundFault {  }
```

<p> <i>DBSnapshotIdentifier</i> does not refer to an existing DB snapshot. </p>

#### `DBSubnetGroup`

``` purescript
newtype DBSubnetGroup
  = DBSubnetGroup { "DBSubnetGroupName" :: NullOrUndefined (String), "DBSubnetGroupDescription" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "SubnetGroupStatus" :: NullOrUndefined (String), "Subnets" :: NullOrUndefined (SubnetList) }
```

#### `DBSubnetGroupAlreadyExistsFault`

``` purescript
newtype DBSubnetGroupAlreadyExistsFault
  = DBSubnetGroupAlreadyExistsFault {  }
```

<p> <i>DBSubnetGroupName</i> is already used by an existing DB subnet group. </p>

#### `DBSubnetGroupDoesNotCoverEnoughAZs`

``` purescript
newtype DBSubnetGroupDoesNotCoverEnoughAZs
  = DBSubnetGroupDoesNotCoverEnoughAZs {  }
```

<p>Subnets in the DB subnet group should cover at least two Availability Zones unless there is only one Availability Zone.</p>

#### `DBSubnetGroupMessage`

``` purescript
newtype DBSubnetGroupMessage
  = DBSubnetGroupMessage { "Marker" :: NullOrUndefined (String), "DBSubnetGroups" :: NullOrUndefined (DBSubnetGroups) }
```

#### `DBSubnetGroupNotFoundFault`

``` purescript
newtype DBSubnetGroupNotFoundFault
  = DBSubnetGroupNotFoundFault {  }
```

<p> <i>DBSubnetGroupName</i> does not refer to an existing DB subnet group. </p>

#### `DBSubnetGroupQuotaExceededFault`

``` purescript
newtype DBSubnetGroupQuotaExceededFault
  = DBSubnetGroupQuotaExceededFault {  }
```

<p>Request would result in user exceeding the allowed number of DB subnet groups.</p>

#### `DBSubnetGroups`

``` purescript
newtype DBSubnetGroups
  = DBSubnetGroups (Array DBSubnetGroup)
```

#### `DBSubnetQuotaExceededFault`

``` purescript
newtype DBSubnetQuotaExceededFault
  = DBSubnetQuotaExceededFault {  }
```

<p>Request would result in user exceeding the allowed number of subnets in a DB subnet groups.</p>

#### `DBUpgradeDependencyFailureFault`

``` purescript
newtype DBUpgradeDependencyFailureFault
  = DBUpgradeDependencyFailureFault {  }
```

<p>The DB upgrade failed because a resource the DB depends on could not be modified.</p>

#### `DeleteDBInstanceMessage`

``` purescript
newtype DeleteDBInstanceMessage
  = DeleteDBInstanceMessage { "DBInstanceIdentifier" :: String, "SkipFinalSnapshot" :: NullOrUndefined (Boolean), "FinalDBSnapshotIdentifier" :: NullOrUndefined (String) }
```

#### `DeleteDBInstanceResult`

``` purescript
newtype DeleteDBInstanceResult
  = DeleteDBInstanceResult { "DBInstance" :: NullOrUndefined (DBInstance) }
```

#### `DeleteDBParameterGroupMessage`

``` purescript
newtype DeleteDBParameterGroupMessage
  = DeleteDBParameterGroupMessage { "DBParameterGroupName" :: String }
```

#### `DeleteDBSecurityGroupMessage`

``` purescript
newtype DeleteDBSecurityGroupMessage
  = DeleteDBSecurityGroupMessage { "DBSecurityGroupName" :: String }
```

#### `DeleteDBSnapshotMessage`

``` purescript
newtype DeleteDBSnapshotMessage
  = DeleteDBSnapshotMessage { "DBSnapshotIdentifier" :: String }
```

#### `DeleteDBSnapshotResult`

``` purescript
newtype DeleteDBSnapshotResult
  = DeleteDBSnapshotResult { "DBSnapshot" :: NullOrUndefined (DBSnapshot) }
```

#### `DeleteDBSubnetGroupMessage`

``` purescript
newtype DeleteDBSubnetGroupMessage
  = DeleteDBSubnetGroupMessage { "DBSubnetGroupName" :: String }
```

#### `DeleteEventSubscriptionMessage`

``` purescript
newtype DeleteEventSubscriptionMessage
  = DeleteEventSubscriptionMessage { "SubscriptionName" :: String }
```

#### `DeleteEventSubscriptionResult`

``` purescript
newtype DeleteEventSubscriptionResult
  = DeleteEventSubscriptionResult { "EventSubscription" :: NullOrUndefined (EventSubscription) }
```

#### `DeleteOptionGroupMessage`

``` purescript
newtype DeleteOptionGroupMessage
  = DeleteOptionGroupMessage { "OptionGroupName" :: String }
```

#### `DescribeDBEngineVersionsMessage`

``` purescript
newtype DescribeDBEngineVersionsMessage
  = DescribeDBEngineVersionsMessage { "Engine" :: NullOrUndefined (String), "EngineVersion" :: NullOrUndefined (String), "DBParameterGroupFamily" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String), "DefaultOnly" :: NullOrUndefined (Boolean), "ListSupportedCharacterSets" :: NullOrUndefined (BooleanOptional) }
```

#### `DescribeDBInstancesMessage`

``` purescript
newtype DescribeDBInstancesMessage
  = DescribeDBInstancesMessage { "DBInstanceIdentifier" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `DescribeDBParameterGroupsMessage`

``` purescript
newtype DescribeDBParameterGroupsMessage
  = DescribeDBParameterGroupsMessage { "DBParameterGroupName" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `DescribeDBParametersMessage`

``` purescript
newtype DescribeDBParametersMessage
  = DescribeDBParametersMessage { "DBParameterGroupName" :: String, "Source" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `DescribeDBSecurityGroupsMessage`

``` purescript
newtype DescribeDBSecurityGroupsMessage
  = DescribeDBSecurityGroupsMessage { "DBSecurityGroupName" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `DescribeDBSnapshotsMessage`

``` purescript
newtype DescribeDBSnapshotsMessage
  = DescribeDBSnapshotsMessage { "DBInstanceIdentifier" :: NullOrUndefined (String), "DBSnapshotIdentifier" :: NullOrUndefined (String), "SnapshotType" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `DescribeDBSubnetGroupsMessage`

``` purescript
newtype DescribeDBSubnetGroupsMessage
  = DescribeDBSubnetGroupsMessage { "DBSubnetGroupName" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `DescribeEngineDefaultParametersMessage`

``` purescript
newtype DescribeEngineDefaultParametersMessage
  = DescribeEngineDefaultParametersMessage { "DBParameterGroupFamily" :: String, "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `DescribeEngineDefaultParametersResult`

``` purescript
newtype DescribeEngineDefaultParametersResult
  = DescribeEngineDefaultParametersResult { "EngineDefaults" :: NullOrUndefined (EngineDefaults) }
```

#### `DescribeEventCategoriesMessage`

``` purescript
newtype DescribeEventCategoriesMessage
  = DescribeEventCategoriesMessage { "SourceType" :: NullOrUndefined (String) }
```

#### `DescribeEventSubscriptionsMessage`

``` purescript
newtype DescribeEventSubscriptionsMessage
  = DescribeEventSubscriptionsMessage { "SubscriptionName" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `DescribeEventsMessage`

``` purescript
newtype DescribeEventsMessage
  = DescribeEventsMessage { "SourceIdentifier" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (SourceType), "StartTime" :: NullOrUndefined (TStamp), "EndTime" :: NullOrUndefined (TStamp), "Duration" :: NullOrUndefined (IntegerOptional), "EventCategories" :: NullOrUndefined (EventCategoriesList), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `DescribeOptionGroupOptionsMessage`

``` purescript
newtype DescribeOptionGroupOptionsMessage
  = DescribeOptionGroupOptionsMessage { "EngineName" :: String, "MajorEngineVersion" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `DescribeOptionGroupsMessage`

``` purescript
newtype DescribeOptionGroupsMessage
  = DescribeOptionGroupsMessage { "OptionGroupName" :: NullOrUndefined (String), "Marker" :: NullOrUndefined (String), "MaxRecords" :: NullOrUndefined (IntegerOptional), "EngineName" :: NullOrUndefined (String), "MajorEngineVersion" :: NullOrUndefined (String) }
```

#### `DescribeOrderableDBInstanceOptionsMessage`

``` purescript
newtype DescribeOrderableDBInstanceOptionsMessage
  = DescribeOrderableDBInstanceOptionsMessage { "Engine" :: String, "EngineVersion" :: NullOrUndefined (String), "DBInstanceClass" :: NullOrUndefined (String), "LicenseModel" :: NullOrUndefined (String), "Vpc" :: NullOrUndefined (BooleanOptional), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `DescribeReservedDBInstancesMessage`

``` purescript
newtype DescribeReservedDBInstancesMessage
  = DescribeReservedDBInstancesMessage { "ReservedDBInstanceId" :: NullOrUndefined (String), "ReservedDBInstancesOfferingId" :: NullOrUndefined (String), "DBInstanceClass" :: NullOrUndefined (String), "Duration" :: NullOrUndefined (String), "ProductDescription" :: NullOrUndefined (String), "OfferingType" :: NullOrUndefined (String), "MultiAZ" :: NullOrUndefined (BooleanOptional), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `DescribeReservedDBInstancesOfferingsMessage`

``` purescript
newtype DescribeReservedDBInstancesOfferingsMessage
  = DescribeReservedDBInstancesOfferingsMessage { "ReservedDBInstancesOfferingId" :: NullOrUndefined (String), "DBInstanceClass" :: NullOrUndefined (String), "Duration" :: NullOrUndefined (String), "ProductDescription" :: NullOrUndefined (String), "OfferingType" :: NullOrUndefined (String), "MultiAZ" :: NullOrUndefined (BooleanOptional), "MaxRecords" :: NullOrUndefined (IntegerOptional), "Marker" :: NullOrUndefined (String) }
```

#### `EC2SecurityGroup`

``` purescript
newtype EC2SecurityGroup
  = EC2SecurityGroup { "Status" :: NullOrUndefined (String), "EC2SecurityGroupName" :: NullOrUndefined (String), "EC2SecurityGroupId" :: NullOrUndefined (String), "EC2SecurityGroupOwnerId" :: NullOrUndefined (String) }
```

#### `EC2SecurityGroupList`

``` purescript
newtype EC2SecurityGroupList
  = EC2SecurityGroupList (Array EC2SecurityGroup)
```

#### `Endpoint`

``` purescript
newtype Endpoint
  = Endpoint { "Address" :: NullOrUndefined (String), "Port" :: NullOrUndefined (Int) }
```

#### `EngineDefaults`

``` purescript
newtype EngineDefaults
  = EngineDefaults { "DBParameterGroupFamily" :: NullOrUndefined (String), "Marker" :: NullOrUndefined (String), "Parameters" :: NullOrUndefined (ParametersList) }
```

#### `Event`

``` purescript
newtype Event
  = Event { "SourceIdentifier" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (SourceType), "Message" :: NullOrUndefined (String), "EventCategories" :: NullOrUndefined (EventCategoriesList), "Date" :: NullOrUndefined (TStamp) }
```

#### `EventCategoriesList`

``` purescript
newtype EventCategoriesList
  = EventCategoriesList (Array String)
```

#### `EventCategoriesMap`

``` purescript
newtype EventCategoriesMap
  = EventCategoriesMap { "SourceType" :: NullOrUndefined (String), "EventCategories" :: NullOrUndefined (EventCategoriesList) }
```

#### `EventCategoriesMapList`

``` purescript
newtype EventCategoriesMapList
  = EventCategoriesMapList (Array EventCategoriesMap)
```

#### `EventCategoriesMessage`

``` purescript
newtype EventCategoriesMessage
  = EventCategoriesMessage { "EventCategoriesMapList" :: NullOrUndefined (EventCategoriesMapList) }
```

#### `EventList`

``` purescript
newtype EventList
  = EventList (Array Event)
```

#### `EventSubscription`

``` purescript
newtype EventSubscription
  = EventSubscription { "Id" :: NullOrUndefined (String), "CustomerAwsId" :: NullOrUndefined (String), "CustSubscriptionId" :: NullOrUndefined (String), "SnsTopicArn" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "SubscriptionCreationTime" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (String), "SourceIdsList" :: NullOrUndefined (SourceIdsList), "EventCategoriesList" :: NullOrUndefined (EventCategoriesList), "Enabled" :: NullOrUndefined (Boolean) }
```

#### `EventSubscriptionQuotaExceededFault`

``` purescript
newtype EventSubscriptionQuotaExceededFault
  = EventSubscriptionQuotaExceededFault {  }
```

<p>You have reached the maximum number of event subscriptions.</p>

#### `EventSubscriptionsList`

``` purescript
newtype EventSubscriptionsList
  = EventSubscriptionsList (Array EventSubscription)
```

#### `EventSubscriptionsMessage`

``` purescript
newtype EventSubscriptionsMessage
  = EventSubscriptionsMessage { "Marker" :: NullOrUndefined (String), "EventSubscriptionsList" :: NullOrUndefined (EventSubscriptionsList) }
```

#### `EventsMessage`

``` purescript
newtype EventsMessage
  = EventsMessage { "Marker" :: NullOrUndefined (String), "Events" :: NullOrUndefined (EventList) }
```

#### `IPRange`

``` purescript
newtype IPRange
  = IPRange { "Status" :: NullOrUndefined (String), "CIDRIP" :: NullOrUndefined (String) }
```

#### `IPRangeList`

``` purescript
newtype IPRangeList
  = IPRangeList (Array IPRange)
```

#### `InstanceQuotaExceededFault`

``` purescript
newtype InstanceQuotaExceededFault
  = InstanceQuotaExceededFault {  }
```

<p>Request would result in user exceeding the allowed number of DB instances.</p>

#### `InsufficientDBInstanceCapacityFault`

``` purescript
newtype InsufficientDBInstanceCapacityFault
  = InsufficientDBInstanceCapacityFault {  }
```

<p>Specified DB instance class is not available in the specified Availability Zone.</p>

#### `IntegerOptional`

``` purescript
newtype IntegerOptional
  = IntegerOptional Int
```

#### `InvalidDBInstanceStateFault`

``` purescript
newtype InvalidDBInstanceStateFault
  = InvalidDBInstanceStateFault {  }
```

<p> The specified DB instance is not in the <i>available</i> state. </p>

#### `InvalidDBParameterGroupStateFault`

``` purescript
newtype InvalidDBParameterGroupStateFault
  = InvalidDBParameterGroupStateFault {  }
```

<p>The DB parameter group is in use or is in an invalid state. If you are attempting to delete the parameter group, you cannot delete it when the parameter group is in this state.</p>

#### `InvalidDBSecurityGroupStateFault`

``` purescript
newtype InvalidDBSecurityGroupStateFault
  = InvalidDBSecurityGroupStateFault {  }
```

<p>The state of the DB security group does not allow deletion.</p>

#### `InvalidDBSnapshotStateFault`

``` purescript
newtype InvalidDBSnapshotStateFault
  = InvalidDBSnapshotStateFault {  }
```

<p>The state of the DB snapshot does not allow deletion.</p>

#### `InvalidDBSubnetGroupStateFault`

``` purescript
newtype InvalidDBSubnetGroupStateFault
  = InvalidDBSubnetGroupStateFault {  }
```

<p>The DB subnet group cannot be deleted because it is in use.</p>

#### `InvalidDBSubnetStateFault`

``` purescript
newtype InvalidDBSubnetStateFault
  = InvalidDBSubnetStateFault {  }
```

<p> The DB subnet is not in the <i>available</i> state. </p>

#### `InvalidEventSubscriptionStateFault`

``` purescript
newtype InvalidEventSubscriptionStateFault
  = InvalidEventSubscriptionStateFault {  }
```

<p>This error can occur if someone else is modifying a subscription. You should retry the action.</p>

#### `InvalidOptionGroupStateFault`

``` purescript
newtype InvalidOptionGroupStateFault
  = InvalidOptionGroupStateFault {  }
```

<p> The option group is not in the <i>available</i> state. </p>

#### `InvalidRestoreFault`

``` purescript
newtype InvalidRestoreFault
  = InvalidRestoreFault {  }
```

<p>Cannot restore from vpc backup to non-vpc DB instance.</p>

#### `InvalidSubnet`

``` purescript
newtype InvalidSubnet
  = InvalidSubnet {  }
```

<p>The requested subnet is invalid, or multiple subnets were requested that are not all in a common VPC.</p>

#### `InvalidVPCNetworkStateFault`

``` purescript
newtype InvalidVPCNetworkStateFault
  = InvalidVPCNetworkStateFault {  }
```

<p>DB subnet group does not cover all Availability Zones after it is created because users' change.</p>

#### `KeyList`

``` purescript
newtype KeyList
  = KeyList (Array String)
```

#### `ListTagsForResourceMessage`

``` purescript
newtype ListTagsForResourceMessage
  = ListTagsForResourceMessage { "ResourceName" :: String }
```

#### `ModifyDBInstanceMessage`

``` purescript
newtype ModifyDBInstanceMessage
  = ModifyDBInstanceMessage { "DBInstanceIdentifier" :: String, "AllocatedStorage" :: NullOrUndefined (IntegerOptional), "DBInstanceClass" :: NullOrUndefined (String), "DBSecurityGroups" :: NullOrUndefined (DBSecurityGroupNameList), "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList), "ApplyImmediately" :: NullOrUndefined (Boolean), "MasterUserPassword" :: NullOrUndefined (String), "DBParameterGroupName" :: NullOrUndefined (String), "BackupRetentionPeriod" :: NullOrUndefined (IntegerOptional), "PreferredBackupWindow" :: NullOrUndefined (String), "PreferredMaintenanceWindow" :: NullOrUndefined (String), "MultiAZ" :: NullOrUndefined (BooleanOptional), "EngineVersion" :: NullOrUndefined (String), "AllowMajorVersionUpgrade" :: NullOrUndefined (Boolean), "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional), "Iops" :: NullOrUndefined (IntegerOptional), "OptionGroupName" :: NullOrUndefined (String), "NewDBInstanceIdentifier" :: NullOrUndefined (String) }
```

#### `ModifyDBInstanceResult`

``` purescript
newtype ModifyDBInstanceResult
  = ModifyDBInstanceResult { "DBInstance" :: NullOrUndefined (DBInstance) }
```

#### `ModifyDBParameterGroupMessage`

``` purescript
newtype ModifyDBParameterGroupMessage
  = ModifyDBParameterGroupMessage { "DBParameterGroupName" :: String, "Parameters" :: ParametersList }
```

#### `ModifyDBSubnetGroupMessage`

``` purescript
newtype ModifyDBSubnetGroupMessage
  = ModifyDBSubnetGroupMessage { "DBSubnetGroupName" :: String, "DBSubnetGroupDescription" :: NullOrUndefined (String), "SubnetIds" :: SubnetIdentifierList }
```

#### `ModifyDBSubnetGroupResult`

``` purescript
newtype ModifyDBSubnetGroupResult
  = ModifyDBSubnetGroupResult { "DBSubnetGroup" :: NullOrUndefined (DBSubnetGroup) }
```

#### `ModifyEventSubscriptionMessage`

``` purescript
newtype ModifyEventSubscriptionMessage
  = ModifyEventSubscriptionMessage { "SubscriptionName" :: String, "SnsTopicArn" :: NullOrUndefined (String), "SourceType" :: NullOrUndefined (String), "EventCategories" :: NullOrUndefined (EventCategoriesList), "Enabled" :: NullOrUndefined (BooleanOptional) }
```

#### `ModifyEventSubscriptionResult`

``` purescript
newtype ModifyEventSubscriptionResult
  = ModifyEventSubscriptionResult { "EventSubscription" :: NullOrUndefined (EventSubscription) }
```

#### `ModifyOptionGroupMessage`

``` purescript
newtype ModifyOptionGroupMessage
  = ModifyOptionGroupMessage { "OptionGroupName" :: String, "OptionsToInclude" :: NullOrUndefined (OptionConfigurationList), "OptionsToRemove" :: NullOrUndefined (OptionNamesList), "ApplyImmediately" :: NullOrUndefined (Boolean) }
```

#### `ModifyOptionGroupResult`

``` purescript
newtype ModifyOptionGroupResult
  = ModifyOptionGroupResult { "OptionGroup" :: NullOrUndefined (OptionGroup) }
```

#### `Option`

``` purescript
newtype Option
  = Option { "OptionName" :: NullOrUndefined (String), "OptionDescription" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "DBSecurityGroupMemberships" :: NullOrUndefined (DBSecurityGroupMembershipList), "VpcSecurityGroupMemberships" :: NullOrUndefined (VpcSecurityGroupMembershipList) }
```

#### `OptionConfiguration`

``` purescript
newtype OptionConfiguration
  = OptionConfiguration { "OptionName" :: String, "Port" :: NullOrUndefined (IntegerOptional), "DBSecurityGroupMemberships" :: NullOrUndefined (DBSecurityGroupNameList), "VpcSecurityGroupMemberships" :: NullOrUndefined (VpcSecurityGroupIdList) }
```

#### `OptionConfigurationList`

``` purescript
newtype OptionConfigurationList
  = OptionConfigurationList (Array OptionConfiguration)
```

#### `OptionGroup`

``` purescript
newtype OptionGroup
  = OptionGroup { "OptionGroupName" :: NullOrUndefined (String), "OptionGroupDescription" :: NullOrUndefined (String), "EngineName" :: NullOrUndefined (String), "MajorEngineVersion" :: NullOrUndefined (String), "Options" :: NullOrUndefined (OptionsList), "AllowsVpcAndNonVpcInstanceMemberships" :: NullOrUndefined (Boolean), "VpcId" :: NullOrUndefined (String) }
```

#### `OptionGroupAlreadyExistsFault`

``` purescript
newtype OptionGroupAlreadyExistsFault
  = OptionGroupAlreadyExistsFault {  }
```

<p>The option group you are trying to create already exists.</p>

#### `OptionGroupMembership`

``` purescript
newtype OptionGroupMembership
  = OptionGroupMembership { "OptionGroupName" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String) }
```

#### `OptionGroupNotFoundFault`

``` purescript
newtype OptionGroupNotFoundFault
  = OptionGroupNotFoundFault {  }
```

<p>The specified option group could not be found.</p>

#### `OptionGroupOption`

``` purescript
newtype OptionGroupOption
  = OptionGroupOption { "Name" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "EngineName" :: NullOrUndefined (String), "MajorEngineVersion" :: NullOrUndefined (String), "MinimumRequiredMinorEngineVersion" :: NullOrUndefined (String), "PortRequired" :: NullOrUndefined (Boolean), "DefaultPort" :: NullOrUndefined (IntegerOptional), "OptionsDependedOn" :: NullOrUndefined (OptionsDependedOn) }
```

#### `OptionGroupOptionsList`

``` purescript
newtype OptionGroupOptionsList
  = OptionGroupOptionsList (Array OptionGroupOption)
```

#### `OptionGroupOptionsMessage`

``` purescript
newtype OptionGroupOptionsMessage
  = OptionGroupOptionsMessage { "OptionGroupOptions" :: NullOrUndefined (OptionGroupOptionsList), "Marker" :: NullOrUndefined (String) }
```

#### `OptionGroupQuotaExceededFault`

``` purescript
newtype OptionGroupQuotaExceededFault
  = OptionGroupQuotaExceededFault {  }
```

<p>The quota of 20 option groups was exceeded for this AWS account.</p>

#### `OptionGroups`

``` purescript
newtype OptionGroups
  = OptionGroups { "OptionGroupsList" :: NullOrUndefined (OptionGroupsList), "Marker" :: NullOrUndefined (String) }
```

#### `OptionGroupsList`

``` purescript
newtype OptionGroupsList
  = OptionGroupsList (Array OptionGroup)
```

#### `OptionNamesList`

``` purescript
newtype OptionNamesList
  = OptionNamesList (Array String)
```

#### `OptionsDependedOn`

``` purescript
newtype OptionsDependedOn
  = OptionsDependedOn (Array String)
```

#### `OptionsList`

``` purescript
newtype OptionsList
  = OptionsList (Array Option)
```

#### `OrderableDBInstanceOption`

``` purescript
newtype OrderableDBInstanceOption
  = OrderableDBInstanceOption { "Engine" :: NullOrUndefined (String), "EngineVersion" :: NullOrUndefined (String), "DBInstanceClass" :: NullOrUndefined (String), "LicenseModel" :: NullOrUndefined (String), "AvailabilityZones" :: NullOrUndefined (AvailabilityZoneList), "MultiAZCapable" :: NullOrUndefined (Boolean), "ReadReplicaCapable" :: NullOrUndefined (Boolean), "Vpc" :: NullOrUndefined (Boolean) }
```

#### `OrderableDBInstanceOptionsList`

``` purescript
newtype OrderableDBInstanceOptionsList
  = OrderableDBInstanceOptionsList (Array OrderableDBInstanceOption)
```

#### `OrderableDBInstanceOptionsMessage`

``` purescript
newtype OrderableDBInstanceOptionsMessage
  = OrderableDBInstanceOptionsMessage { "OrderableDBInstanceOptions" :: NullOrUndefined (OrderableDBInstanceOptionsList), "Marker" :: NullOrUndefined (String) }
```

#### `Parameter`

``` purescript
newtype Parameter
  = Parameter { "ParameterName" :: NullOrUndefined (String), "ParameterValue" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Source" :: NullOrUndefined (String), "ApplyType" :: NullOrUndefined (String), "DataType" :: NullOrUndefined (String), "AllowedValues" :: NullOrUndefined (String), "IsModifiable" :: NullOrUndefined (Boolean), "MinimumEngineVersion" :: NullOrUndefined (String), "ApplyMethod" :: NullOrUndefined (ApplyMethod) }
```

#### `ParametersList`

``` purescript
newtype ParametersList
  = ParametersList (Array Parameter)
```

#### `PendingModifiedValues`

``` purescript
newtype PendingModifiedValues
  = PendingModifiedValues { "DBInstanceClass" :: NullOrUndefined (String), "AllocatedStorage" :: NullOrUndefined (IntegerOptional), "MasterUserPassword" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "BackupRetentionPeriod" :: NullOrUndefined (IntegerOptional), "MultiAZ" :: NullOrUndefined (BooleanOptional), "EngineVersion" :: NullOrUndefined (String), "Iops" :: NullOrUndefined (IntegerOptional), "DBInstanceIdentifier" :: NullOrUndefined (String) }
```

#### `PointInTimeRestoreNotEnabledFault`

``` purescript
newtype PointInTimeRestoreNotEnabledFault
  = PointInTimeRestoreNotEnabledFault {  }
```

<p> <i>SourceDBInstanceIdentifier</i> refers to a DB instance with <i>BackupRetentionPeriod</i> equal to 0. </p>

#### `PromoteReadReplicaMessage`

``` purescript
newtype PromoteReadReplicaMessage
  = PromoteReadReplicaMessage { "DBInstanceIdentifier" :: String, "BackupRetentionPeriod" :: NullOrUndefined (IntegerOptional), "PreferredBackupWindow" :: NullOrUndefined (String) }
```

#### `PromoteReadReplicaResult`

``` purescript
newtype PromoteReadReplicaResult
  = PromoteReadReplicaResult { "DBInstance" :: NullOrUndefined (DBInstance) }
```

#### `ProvisionedIopsNotAvailableInAZFault`

``` purescript
newtype ProvisionedIopsNotAvailableInAZFault
  = ProvisionedIopsNotAvailableInAZFault {  }
```

<p>Provisioned IOPS not available in the specified Availability Zone.</p>

#### `PurchaseReservedDBInstancesOfferingMessage`

``` purescript
newtype PurchaseReservedDBInstancesOfferingMessage
  = PurchaseReservedDBInstancesOfferingMessage { "ReservedDBInstancesOfferingId" :: String, "ReservedDBInstanceId" :: NullOrUndefined (String), "DBInstanceCount" :: NullOrUndefined (IntegerOptional) }
```

#### `PurchaseReservedDBInstancesOfferingResult`

``` purescript
newtype PurchaseReservedDBInstancesOfferingResult
  = PurchaseReservedDBInstancesOfferingResult { "ReservedDBInstance" :: NullOrUndefined (ReservedDBInstance) }
```

#### `ReadReplicaDBInstanceIdentifierList`

``` purescript
newtype ReadReplicaDBInstanceIdentifierList
  = ReadReplicaDBInstanceIdentifierList (Array String)
```

#### `RebootDBInstanceMessage`

``` purescript
newtype RebootDBInstanceMessage
  = RebootDBInstanceMessage { "DBInstanceIdentifier" :: String, "ForceFailover" :: NullOrUndefined (BooleanOptional) }
```

#### `RebootDBInstanceResult`

``` purescript
newtype RebootDBInstanceResult
  = RebootDBInstanceResult { "DBInstance" :: NullOrUndefined (DBInstance) }
```

#### `RecurringCharge`

``` purescript
newtype RecurringCharge
  = RecurringCharge { "RecurringChargeAmount" :: NullOrUndefined (Number), "RecurringChargeFrequency" :: NullOrUndefined (String) }
```

#### `RecurringChargeList`

``` purescript
newtype RecurringChargeList
  = RecurringChargeList (Array RecurringCharge)
```

#### `RemoveSourceIdentifierFromSubscriptionMessage`

``` purescript
newtype RemoveSourceIdentifierFromSubscriptionMessage
  = RemoveSourceIdentifierFromSubscriptionMessage { "SubscriptionName" :: String, "SourceIdentifier" :: String }
```

#### `RemoveSourceIdentifierFromSubscriptionResult`

``` purescript
newtype RemoveSourceIdentifierFromSubscriptionResult
  = RemoveSourceIdentifierFromSubscriptionResult { "EventSubscription" :: NullOrUndefined (EventSubscription) }
```

#### `RemoveTagsFromResourceMessage`

``` purescript
newtype RemoveTagsFromResourceMessage
  = RemoveTagsFromResourceMessage { "ResourceName" :: String, "TagKeys" :: KeyList }
```

#### `ReservedDBInstance`

``` purescript
newtype ReservedDBInstance
  = ReservedDBInstance { "ReservedDBInstanceId" :: NullOrUndefined (String), "ReservedDBInstancesOfferingId" :: NullOrUndefined (String), "DBInstanceClass" :: NullOrUndefined (String), "StartTime" :: NullOrUndefined (TStamp), "Duration" :: NullOrUndefined (Int), "FixedPrice" :: NullOrUndefined (Number), "UsagePrice" :: NullOrUndefined (Number), "CurrencyCode" :: NullOrUndefined (String), "DBInstanceCount" :: NullOrUndefined (Int), "ProductDescription" :: NullOrUndefined (String), "OfferingType" :: NullOrUndefined (String), "MultiAZ" :: NullOrUndefined (Boolean), "State" :: NullOrUndefined (String), "RecurringCharges" :: NullOrUndefined (RecurringChargeList) }
```

#### `ReservedDBInstanceAlreadyExistsFault`

``` purescript
newtype ReservedDBInstanceAlreadyExistsFault
  = ReservedDBInstanceAlreadyExistsFault {  }
```

<p>User already has a reservation with the given identifier.</p>

#### `ReservedDBInstanceList`

``` purescript
newtype ReservedDBInstanceList
  = ReservedDBInstanceList (Array ReservedDBInstance)
```

#### `ReservedDBInstanceMessage`

``` purescript
newtype ReservedDBInstanceMessage
  = ReservedDBInstanceMessage { "Marker" :: NullOrUndefined (String), "ReservedDBInstances" :: NullOrUndefined (ReservedDBInstanceList) }
```

#### `ReservedDBInstanceNotFoundFault`

``` purescript
newtype ReservedDBInstanceNotFoundFault
  = ReservedDBInstanceNotFoundFault {  }
```

<p>The specified reserved DB Instance not found.</p>

#### `ReservedDBInstanceQuotaExceededFault`

``` purescript
newtype ReservedDBInstanceQuotaExceededFault
  = ReservedDBInstanceQuotaExceededFault {  }
```

<p>Request would exceed the user's DB Instance quota.</p>

#### `ReservedDBInstancesOffering`

``` purescript
newtype ReservedDBInstancesOffering
  = ReservedDBInstancesOffering { "ReservedDBInstancesOfferingId" :: NullOrUndefined (String), "DBInstanceClass" :: NullOrUndefined (String), "Duration" :: NullOrUndefined (Int), "FixedPrice" :: NullOrUndefined (Number), "UsagePrice" :: NullOrUndefined (Number), "CurrencyCode" :: NullOrUndefined (String), "ProductDescription" :: NullOrUndefined (String), "OfferingType" :: NullOrUndefined (String), "MultiAZ" :: NullOrUndefined (Boolean), "RecurringCharges" :: NullOrUndefined (RecurringChargeList) }
```

#### `ReservedDBInstancesOfferingList`

``` purescript
newtype ReservedDBInstancesOfferingList
  = ReservedDBInstancesOfferingList (Array ReservedDBInstancesOffering)
```

#### `ReservedDBInstancesOfferingMessage`

``` purescript
newtype ReservedDBInstancesOfferingMessage
  = ReservedDBInstancesOfferingMessage { "Marker" :: NullOrUndefined (String), "ReservedDBInstancesOfferings" :: NullOrUndefined (ReservedDBInstancesOfferingList) }
```

#### `ReservedDBInstancesOfferingNotFoundFault`

``` purescript
newtype ReservedDBInstancesOfferingNotFoundFault
  = ReservedDBInstancesOfferingNotFoundFault {  }
```

<p>Specified offering does not exist.</p>

#### `ResetDBParameterGroupMessage`

``` purescript
newtype ResetDBParameterGroupMessage
  = ResetDBParameterGroupMessage { "DBParameterGroupName" :: String, "ResetAllParameters" :: NullOrUndefined (Boolean), "Parameters" :: NullOrUndefined (ParametersList) }
```

#### `RestoreDBInstanceFromDBSnapshotMessage`

``` purescript
newtype RestoreDBInstanceFromDBSnapshotMessage
  = RestoreDBInstanceFromDBSnapshotMessage { "DBInstanceIdentifier" :: String, "DBSnapshotIdentifier" :: String, "DBInstanceClass" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "AvailabilityZone" :: NullOrUndefined (String), "DBSubnetGroupName" :: NullOrUndefined (String), "MultiAZ" :: NullOrUndefined (BooleanOptional), "PubliclyAccessible" :: NullOrUndefined (BooleanOptional), "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional), "LicenseModel" :: NullOrUndefined (String), "DBName" :: NullOrUndefined (String), "Engine" :: NullOrUndefined (String), "Iops" :: NullOrUndefined (IntegerOptional), "OptionGroupName" :: NullOrUndefined (String) }
```

#### `RestoreDBInstanceFromDBSnapshotResult`

``` purescript
newtype RestoreDBInstanceFromDBSnapshotResult
  = RestoreDBInstanceFromDBSnapshotResult { "DBInstance" :: NullOrUndefined (DBInstance) }
```

#### `RestoreDBInstanceToPointInTimeMessage`

``` purescript
newtype RestoreDBInstanceToPointInTimeMessage
  = RestoreDBInstanceToPointInTimeMessage { "SourceDBInstanceIdentifier" :: String, "TargetDBInstanceIdentifier" :: String, "RestoreTime" :: NullOrUndefined (TStamp), "UseLatestRestorableTime" :: NullOrUndefined (Boolean), "DBInstanceClass" :: NullOrUndefined (String), "Port" :: NullOrUndefined (IntegerOptional), "AvailabilityZone" :: NullOrUndefined (String), "DBSubnetGroupName" :: NullOrUndefined (String), "MultiAZ" :: NullOrUndefined (BooleanOptional), "PubliclyAccessible" :: NullOrUndefined (BooleanOptional), "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional), "LicenseModel" :: NullOrUndefined (String), "DBName" :: NullOrUndefined (String), "Engine" :: NullOrUndefined (String), "Iops" :: NullOrUndefined (IntegerOptional), "OptionGroupName" :: NullOrUndefined (String) }
```

#### `RestoreDBInstanceToPointInTimeResult`

``` purescript
newtype RestoreDBInstanceToPointInTimeResult
  = RestoreDBInstanceToPointInTimeResult { "DBInstance" :: NullOrUndefined (DBInstance) }
```

#### `RevokeDBSecurityGroupIngressMessage`

``` purescript
newtype RevokeDBSecurityGroupIngressMessage
  = RevokeDBSecurityGroupIngressMessage { "DBSecurityGroupName" :: String, "CIDRIP" :: NullOrUndefined (String), "EC2SecurityGroupName" :: NullOrUndefined (String), "EC2SecurityGroupId" :: NullOrUndefined (String), "EC2SecurityGroupOwnerId" :: NullOrUndefined (String) }
```

#### `RevokeDBSecurityGroupIngressResult`

``` purescript
newtype RevokeDBSecurityGroupIngressResult
  = RevokeDBSecurityGroupIngressResult { "DBSecurityGroup" :: NullOrUndefined (DBSecurityGroup) }
```

#### `SNSInvalidTopicFault`

``` purescript
newtype SNSInvalidTopicFault
  = SNSInvalidTopicFault {  }
```

<p>SNS has responded that there is a problem with the SND topic specified.</p>

#### `SNSNoAuthorizationFault`

``` purescript
newtype SNSNoAuthorizationFault
  = SNSNoAuthorizationFault {  }
```

<p>You do not have permission to publish to the SNS topic ARN.</p>

#### `SNSTopicArnNotFoundFault`

``` purescript
newtype SNSTopicArnNotFoundFault
  = SNSTopicArnNotFoundFault {  }
```

<p>The SNS topic ARN does not exist.</p>

#### `SnapshotQuotaExceededFault`

``` purescript
newtype SnapshotQuotaExceededFault
  = SnapshotQuotaExceededFault {  }
```

<p>Request would result in user exceeding the allowed number of DB snapshots.</p>

#### `SourceIdsList`

``` purescript
newtype SourceIdsList
  = SourceIdsList (Array String)
```

#### `SourceNotFoundFault`

``` purescript
newtype SourceNotFoundFault
  = SourceNotFoundFault {  }
```

<p>The requested source could not be found.</p>

#### `SourceType`

``` purescript
newtype SourceType
  = SourceType String
```

#### `StorageQuotaExceededFault`

``` purescript
newtype StorageQuotaExceededFault
  = StorageQuotaExceededFault {  }
```

<p>Request would result in user exceeding the allowed amount of storage available across all DB instances.</p>

#### `Subnet`

``` purescript
newtype Subnet
  = Subnet { "SubnetIdentifier" :: NullOrUndefined (String), "SubnetAvailabilityZone" :: NullOrUndefined (AvailabilityZone), "SubnetStatus" :: NullOrUndefined (String) }
```

#### `SubnetAlreadyInUse`

``` purescript
newtype SubnetAlreadyInUse
  = SubnetAlreadyInUse {  }
```

<p>The DB subnet is already in use in the Availability Zone.</p>

#### `SubnetIdentifierList`

``` purescript
newtype SubnetIdentifierList
  = SubnetIdentifierList (Array String)
```

#### `SubnetList`

``` purescript
newtype SubnetList
  = SubnetList (Array Subnet)
```

#### `SubscriptionAlreadyExistFault`

``` purescript
newtype SubscriptionAlreadyExistFault
  = SubscriptionAlreadyExistFault {  }
```

<p>The supplied subscription name already exists.</p>

#### `SubscriptionCategoryNotFoundFault`

``` purescript
newtype SubscriptionCategoryNotFoundFault
  = SubscriptionCategoryNotFoundFault {  }
```

<p>The supplied category does not exist.</p>

#### `SubscriptionNotFoundFault`

``` purescript
newtype SubscriptionNotFoundFault
  = SubscriptionNotFoundFault {  }
```

<p>The subscription name does not exist.</p>

#### `SupportedCharacterSetsList`

``` purescript
newtype SupportedCharacterSetsList
  = SupportedCharacterSetsList (Array CharacterSet)
```

#### `TStamp`

``` purescript
newtype TStamp
  = TStamp Number
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (String), "Value" :: NullOrUndefined (String) }
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

#### `TagListMessage`

``` purescript
newtype TagListMessage
  = TagListMessage { "TagList" :: NullOrUndefined (TagList) }
```

#### `VpcSecurityGroupIdList`

``` purescript
newtype VpcSecurityGroupIdList
  = VpcSecurityGroupIdList (Array String)
```

#### `VpcSecurityGroupMembership`

``` purescript
newtype VpcSecurityGroupMembership
  = VpcSecurityGroupMembership { "VpcSecurityGroupId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String) }
```

#### `VpcSecurityGroupMembershipList`

``` purescript
newtype VpcSecurityGroupMembershipList
  = VpcSecurityGroupMembershipList (Array VpcSecurityGroupMembership)
```


