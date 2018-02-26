

module AWS.RDS where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "RDS" :: String


addSourceIdentifierToSubscription :: forall eff. AddSourceIdentifierToSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) AddSourceIdentifierToSubscriptionResult
addSourceIdentifierToSubscription = AWS.request serviceName "AddSourceIdentifierToSubscription" 


addTagsToResource :: forall eff. AddTagsToResourceMessage -> Aff (err :: AWS.RequestError | eff) Unit
addTagsToResource = AWS.request serviceName "AddTagsToResource" 


authorizeDBSecurityGroupIngress :: forall eff. AuthorizeDBSecurityGroupIngressMessage -> Aff (err :: AWS.RequestError | eff) AuthorizeDBSecurityGroupIngressResult
authorizeDBSecurityGroupIngress = AWS.request serviceName "AuthorizeDBSecurityGroupIngress" 


copyDBSnapshot :: forall eff. CopyDBSnapshotMessage -> Aff (err :: AWS.RequestError | eff) CopyDBSnapshotResult
copyDBSnapshot = AWS.request serviceName "CopyDBSnapshot" 


createDBInstance :: forall eff. CreateDBInstanceMessage -> Aff (err :: AWS.RequestError | eff) CreateDBInstanceResult
createDBInstance = AWS.request serviceName "CreateDBInstance" 


createDBInstanceReadReplica :: forall eff. CreateDBInstanceReadReplicaMessage -> Aff (err :: AWS.RequestError | eff) CreateDBInstanceReadReplicaResult
createDBInstanceReadReplica = AWS.request serviceName "CreateDBInstanceReadReplica" 


createDBParameterGroup :: forall eff. CreateDBParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateDBParameterGroupResult
createDBParameterGroup = AWS.request serviceName "CreateDBParameterGroup" 


createDBSecurityGroup :: forall eff. CreateDBSecurityGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateDBSecurityGroupResult
createDBSecurityGroup = AWS.request serviceName "CreateDBSecurityGroup" 


createDBSnapshot :: forall eff. CreateDBSnapshotMessage -> Aff (err :: AWS.RequestError | eff) CreateDBSnapshotResult
createDBSnapshot = AWS.request serviceName "CreateDBSnapshot" 


createDBSubnetGroup :: forall eff. CreateDBSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateDBSubnetGroupResult
createDBSubnetGroup = AWS.request serviceName "CreateDBSubnetGroup" 


createEventSubscription :: forall eff. CreateEventSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) CreateEventSubscriptionResult
createEventSubscription = AWS.request serviceName "CreateEventSubscription" 


createOptionGroup :: forall eff. CreateOptionGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateOptionGroupResult
createOptionGroup = AWS.request serviceName "CreateOptionGroup" 


deleteDBInstance :: forall eff. DeleteDBInstanceMessage -> Aff (err :: AWS.RequestError | eff) DeleteDBInstanceResult
deleteDBInstance = AWS.request serviceName "DeleteDBInstance" 


deleteDBParameterGroup :: forall eff. DeleteDBParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteDBParameterGroup = AWS.request serviceName "DeleteDBParameterGroup" 


deleteDBSecurityGroup :: forall eff. DeleteDBSecurityGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteDBSecurityGroup = AWS.request serviceName "DeleteDBSecurityGroup" 


deleteDBSnapshot :: forall eff. DeleteDBSnapshotMessage -> Aff (err :: AWS.RequestError | eff) DeleteDBSnapshotResult
deleteDBSnapshot = AWS.request serviceName "DeleteDBSnapshot" 


deleteDBSubnetGroup :: forall eff. DeleteDBSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteDBSubnetGroup = AWS.request serviceName "DeleteDBSubnetGroup" 


deleteEventSubscription :: forall eff. DeleteEventSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) DeleteEventSubscriptionResult
deleteEventSubscription = AWS.request serviceName "DeleteEventSubscription" 


deleteOptionGroup :: forall eff. DeleteOptionGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteOptionGroup = AWS.request serviceName "DeleteOptionGroup" 


describeDBEngineVersions :: forall eff. DescribeDBEngineVersionsMessage -> Aff (err :: AWS.RequestError | eff) DBEngineVersionMessage
describeDBEngineVersions = AWS.request serviceName "DescribeDBEngineVersions" 


describeDBInstances :: forall eff. DescribeDBInstancesMessage -> Aff (err :: AWS.RequestError | eff) DBInstanceMessage
describeDBInstances = AWS.request serviceName "DescribeDBInstances" 


describeDBParameterGroups :: forall eff. DescribeDBParameterGroupsMessage -> Aff (err :: AWS.RequestError | eff) DBParameterGroupsMessage
describeDBParameterGroups = AWS.request serviceName "DescribeDBParameterGroups" 


describeDBParameters :: forall eff. DescribeDBParametersMessage -> Aff (err :: AWS.RequestError | eff) DBParameterGroupDetails
describeDBParameters = AWS.request serviceName "DescribeDBParameters" 


describeDBSecurityGroups :: forall eff. DescribeDBSecurityGroupsMessage -> Aff (err :: AWS.RequestError | eff) DBSecurityGroupMessage
describeDBSecurityGroups = AWS.request serviceName "DescribeDBSecurityGroups" 


describeDBSnapshots :: forall eff. DescribeDBSnapshotsMessage -> Aff (err :: AWS.RequestError | eff) DBSnapshotMessage
describeDBSnapshots = AWS.request serviceName "DescribeDBSnapshots" 


describeDBSubnetGroups :: forall eff. DescribeDBSubnetGroupsMessage -> Aff (err :: AWS.RequestError | eff) DBSubnetGroupMessage
describeDBSubnetGroups = AWS.request serviceName "DescribeDBSubnetGroups" 


describeEngineDefaultParameters :: forall eff. DescribeEngineDefaultParametersMessage -> Aff (err :: AWS.RequestError | eff) DescribeEngineDefaultParametersResult
describeEngineDefaultParameters = AWS.request serviceName "DescribeEngineDefaultParameters" 


describeEventCategories :: forall eff. DescribeEventCategoriesMessage -> Aff (err :: AWS.RequestError | eff) EventCategoriesMessage
describeEventCategories = AWS.request serviceName "DescribeEventCategories" 


describeEventSubscriptions :: forall eff. DescribeEventSubscriptionsMessage -> Aff (err :: AWS.RequestError | eff) EventSubscriptionsMessage
describeEventSubscriptions = AWS.request serviceName "DescribeEventSubscriptions" 


describeEvents :: forall eff. DescribeEventsMessage -> Aff (err :: AWS.RequestError | eff) EventsMessage
describeEvents = AWS.request serviceName "DescribeEvents" 


describeOptionGroupOptions :: forall eff. DescribeOptionGroupOptionsMessage -> Aff (err :: AWS.RequestError | eff) OptionGroupOptionsMessage
describeOptionGroupOptions = AWS.request serviceName "DescribeOptionGroupOptions" 


describeOptionGroups :: forall eff. DescribeOptionGroupsMessage -> Aff (err :: AWS.RequestError | eff) OptionGroups
describeOptionGroups = AWS.request serviceName "DescribeOptionGroups" 


describeOrderableDBInstanceOptions :: forall eff. DescribeOrderableDBInstanceOptionsMessage -> Aff (err :: AWS.RequestError | eff) OrderableDBInstanceOptionsMessage
describeOrderableDBInstanceOptions = AWS.request serviceName "DescribeOrderableDBInstanceOptions" 


describeReservedDBInstances :: forall eff. DescribeReservedDBInstancesMessage -> Aff (err :: AWS.RequestError | eff) ReservedDBInstanceMessage
describeReservedDBInstances = AWS.request serviceName "DescribeReservedDBInstances" 


describeReservedDBInstancesOfferings :: forall eff. DescribeReservedDBInstancesOfferingsMessage -> Aff (err :: AWS.RequestError | eff) ReservedDBInstancesOfferingMessage
describeReservedDBInstancesOfferings = AWS.request serviceName "DescribeReservedDBInstancesOfferings" 


listTagsForResource :: forall eff. ListTagsForResourceMessage -> Aff (err :: AWS.RequestError | eff) TagListMessage
listTagsForResource = AWS.request serviceName "ListTagsForResource" 


modifyDBInstance :: forall eff. ModifyDBInstanceMessage -> Aff (err :: AWS.RequestError | eff) ModifyDBInstanceResult
modifyDBInstance = AWS.request serviceName "ModifyDBInstance" 


modifyDBParameterGroup :: forall eff. ModifyDBParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) DBParameterGroupNameMessage
modifyDBParameterGroup = AWS.request serviceName "ModifyDBParameterGroup" 


modifyDBSubnetGroup :: forall eff. ModifyDBSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) ModifyDBSubnetGroupResult
modifyDBSubnetGroup = AWS.request serviceName "ModifyDBSubnetGroup" 


modifyEventSubscription :: forall eff. ModifyEventSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) ModifyEventSubscriptionResult
modifyEventSubscription = AWS.request serviceName "ModifyEventSubscription" 


modifyOptionGroup :: forall eff. ModifyOptionGroupMessage -> Aff (err :: AWS.RequestError | eff) ModifyOptionGroupResult
modifyOptionGroup = AWS.request serviceName "ModifyOptionGroup" 


promoteReadReplica :: forall eff. PromoteReadReplicaMessage -> Aff (err :: AWS.RequestError | eff) PromoteReadReplicaResult
promoteReadReplica = AWS.request serviceName "PromoteReadReplica" 


purchaseReservedDBInstancesOffering :: forall eff. PurchaseReservedDBInstancesOfferingMessage -> Aff (err :: AWS.RequestError | eff) PurchaseReservedDBInstancesOfferingResult
purchaseReservedDBInstancesOffering = AWS.request serviceName "PurchaseReservedDBInstancesOffering" 


rebootDBInstance :: forall eff. RebootDBInstanceMessage -> Aff (err :: AWS.RequestError | eff) RebootDBInstanceResult
rebootDBInstance = AWS.request serviceName "RebootDBInstance" 


removeSourceIdentifierFromSubscription :: forall eff. RemoveSourceIdentifierFromSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) RemoveSourceIdentifierFromSubscriptionResult
removeSourceIdentifierFromSubscription = AWS.request serviceName "RemoveSourceIdentifierFromSubscription" 


removeTagsFromResource :: forall eff. RemoveTagsFromResourceMessage -> Aff (err :: AWS.RequestError | eff) Unit
removeTagsFromResource = AWS.request serviceName "RemoveTagsFromResource" 


resetDBParameterGroup :: forall eff. ResetDBParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) DBParameterGroupNameMessage
resetDBParameterGroup = AWS.request serviceName "ResetDBParameterGroup" 


restoreDBInstanceFromDBSnapshot :: forall eff. RestoreDBInstanceFromDBSnapshotMessage -> Aff (err :: AWS.RequestError | eff) RestoreDBInstanceFromDBSnapshotResult
restoreDBInstanceFromDBSnapshot = AWS.request serviceName "RestoreDBInstanceFromDBSnapshot" 


restoreDBInstanceToPointInTime :: forall eff. RestoreDBInstanceToPointInTimeMessage -> Aff (err :: AWS.RequestError | eff) RestoreDBInstanceToPointInTimeResult
restoreDBInstanceToPointInTime = AWS.request serviceName "RestoreDBInstanceToPointInTime" 


revokeDBSecurityGroupIngress :: forall eff. RevokeDBSecurityGroupIngressMessage -> Aff (err :: AWS.RequestError | eff) RevokeDBSecurityGroupIngressResult
revokeDBSecurityGroupIngress = AWS.request serviceName "RevokeDBSecurityGroupIngress" 


newtype AddSourceIdentifierToSubscriptionMessage = AddSourceIdentifierToSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SourceIdentifier" :: (String)
  }


newtype AddSourceIdentifierToSubscriptionResult = AddSourceIdentifierToSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }


newtype AddTagsToResourceMessage = AddTagsToResourceMessage 
  { "ResourceName" :: (String)
  , "Tags" :: (TagList)
  }


newtype ApplyMethod = ApplyMethod String


-- | <p>The specified CIDRIP or EC2 security group is already authorized for the specified DB security group.</p>
newtype AuthorizationAlreadyExistsFault = AuthorizationAlreadyExistsFault 
  { 
  }


-- | <p>Specified CIDRIP or EC2 security group is not authorized for the specified DB security group.</p> <p>RDS may not also be authorized via IAM to perform necessary actions on your behalf.</p>
newtype AuthorizationNotFoundFault = AuthorizationNotFoundFault 
  { 
  }


-- | <p>DB security group authorization quota has been reached.</p>
newtype AuthorizationQuotaExceededFault = AuthorizationQuotaExceededFault 
  { 
  }


newtype AuthorizeDBSecurityGroupIngressMessage = AuthorizeDBSecurityGroupIngressMessage 
  { "DBSecurityGroupName" :: (String)
  , "CIDRIP" :: NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined (String)
  , "EC2SecurityGroupId" :: NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined (String)
  }


newtype AuthorizeDBSecurityGroupIngressResult = AuthorizeDBSecurityGroupIngressResult 
  { "DBSecurityGroup" :: NullOrUndefined (DBSecurityGroup)
  }


newtype AvailabilityZone = AvailabilityZone 
  { "Name" :: NullOrUndefined (String)
  , "ProvisionedIopsCapable" :: NullOrUndefined (Boolean)
  }


newtype AvailabilityZoneList = AvailabilityZoneList (Array AvailabilityZone)


newtype BooleanOptional = BooleanOptional Boolean


newtype CharacterSet = CharacterSet 
  { "CharacterSetName" :: NullOrUndefined (String)
  , "CharacterSetDescription" :: NullOrUndefined (String)
  }


newtype CopyDBSnapshotMessage = CopyDBSnapshotMessage 
  { "SourceDBSnapshotIdentifier" :: (String)
  , "TargetDBSnapshotIdentifier" :: (String)
  }


newtype CopyDBSnapshotResult = CopyDBSnapshotResult 
  { "DBSnapshot" :: NullOrUndefined (DBSnapshot)
  }


newtype CreateDBInstanceMessage = CreateDBInstanceMessage 
  { "DBName" :: NullOrUndefined (String)
  , "DBInstanceIdentifier" :: (String)
  , "AllocatedStorage" :: (IntegerOptional)
  , "DBInstanceClass" :: (String)
  , "Engine" :: (String)
  , "MasterUsername" :: (String)
  , "MasterUserPassword" :: (String)
  , "DBSecurityGroups" :: NullOrUndefined (DBSecurityGroupNameList)
  , "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "DBSubnetGroupName" :: NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "DBParameterGroupName" :: NullOrUndefined (String)
  , "BackupRetentionPeriod" :: NullOrUndefined (IntegerOptional)
  , "PreferredBackupWindow" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "MultiAZ" :: NullOrUndefined (BooleanOptional)
  , "EngineVersion" :: NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "LicenseModel" :: NullOrUndefined (String)
  , "Iops" :: NullOrUndefined (IntegerOptional)
  , "OptionGroupName" :: NullOrUndefined (String)
  , "CharacterSetName" :: NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined (BooleanOptional)
  }


newtype CreateDBInstanceReadReplicaMessage = CreateDBInstanceReadReplicaMessage 
  { "DBInstanceIdentifier" :: (String)
  , "SourceDBInstanceIdentifier" :: (String)
  , "DBInstanceClass" :: NullOrUndefined (String)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "Iops" :: NullOrUndefined (IntegerOptional)
  , "OptionGroupName" :: NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined (BooleanOptional)
  }


newtype CreateDBInstanceReadReplicaResult = CreateDBInstanceReadReplicaResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }


newtype CreateDBInstanceResult = CreateDBInstanceResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }


newtype CreateDBParameterGroupMessage = CreateDBParameterGroupMessage 
  { "DBParameterGroupName" :: (String)
  , "DBParameterGroupFamily" :: (String)
  , "Description" :: (String)
  }


newtype CreateDBParameterGroupResult = CreateDBParameterGroupResult 
  { "DBParameterGroup" :: NullOrUndefined (DBParameterGroup)
  }


newtype CreateDBSecurityGroupMessage = CreateDBSecurityGroupMessage 
  { "DBSecurityGroupName" :: (String)
  , "DBSecurityGroupDescription" :: (String)
  }


newtype CreateDBSecurityGroupResult = CreateDBSecurityGroupResult 
  { "DBSecurityGroup" :: NullOrUndefined (DBSecurityGroup)
  }


newtype CreateDBSnapshotMessage = CreateDBSnapshotMessage 
  { "DBSnapshotIdentifier" :: (String)
  , "DBInstanceIdentifier" :: (String)
  }


newtype CreateDBSnapshotResult = CreateDBSnapshotResult 
  { "DBSnapshot" :: NullOrUndefined (DBSnapshot)
  }


newtype CreateDBSubnetGroupMessage = CreateDBSubnetGroupMessage 
  { "DBSubnetGroupName" :: (String)
  , "DBSubnetGroupDescription" :: (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  }


newtype CreateDBSubnetGroupResult = CreateDBSubnetGroupResult 
  { "DBSubnetGroup" :: NullOrUndefined (DBSubnetGroup)
  }


newtype CreateEventSubscriptionMessage = CreateEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SnsTopicArn" :: (String)
  , "SourceType" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "SourceIds" :: NullOrUndefined (SourceIdsList)
  , "Enabled" :: NullOrUndefined (BooleanOptional)
  }


newtype CreateEventSubscriptionResult = CreateEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }


newtype CreateOptionGroupMessage = CreateOptionGroupMessage 
  { "OptionGroupName" :: (String)
  , "EngineName" :: (String)
  , "MajorEngineVersion" :: (String)
  , "OptionGroupDescription" :: (String)
  }


newtype CreateOptionGroupResult = CreateOptionGroupResult 
  { "OptionGroup" :: NullOrUndefined (OptionGroup)
  }


newtype DBEngineVersion = DBEngineVersion 
  { "Engine" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "DBParameterGroupFamily" :: NullOrUndefined (String)
  , "DBEngineDescription" :: NullOrUndefined (String)
  , "DBEngineVersionDescription" :: NullOrUndefined (String)
  , "DefaultCharacterSet" :: NullOrUndefined (CharacterSet)
  , "SupportedCharacterSets" :: NullOrUndefined (SupportedCharacterSetsList)
  }


newtype DBEngineVersionList = DBEngineVersionList (Array DBEngineVersion)


newtype DBEngineVersionMessage = DBEngineVersionMessage 
  { "Marker" :: NullOrUndefined (String)
  , "DBEngineVersions" :: NullOrUndefined (DBEngineVersionList)
  }


newtype DBInstance = DBInstance 
  { "DBInstanceIdentifier" :: NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined (String)
  , "Engine" :: NullOrUndefined (String)
  , "DBInstanceStatus" :: NullOrUndefined (String)
  , "MasterUsername" :: NullOrUndefined (String)
  , "DBName" :: NullOrUndefined (String)
  , "Endpoint" :: NullOrUndefined (Endpoint)
  , "AllocatedStorage" :: NullOrUndefined (Int)
  , "InstanceCreateTime" :: NullOrUndefined (TStamp)
  , "PreferredBackupWindow" :: NullOrUndefined (String)
  , "BackupRetentionPeriod" :: NullOrUndefined (Int)
  , "DBSecurityGroups" :: NullOrUndefined (DBSecurityGroupMembershipList)
  , "VpcSecurityGroups" :: NullOrUndefined (VpcSecurityGroupMembershipList)
  , "DBParameterGroups" :: NullOrUndefined (DBParameterGroupStatusList)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "DBSubnetGroup" :: NullOrUndefined (DBSubnetGroup)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "PendingModifiedValues" :: NullOrUndefined (PendingModifiedValues)
  , "LatestRestorableTime" :: NullOrUndefined (TStamp)
  , "MultiAZ" :: NullOrUndefined (Boolean)
  , "EngineVersion" :: NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean)
  , "ReadReplicaSourceDBInstanceIdentifier" :: NullOrUndefined (String)
  , "ReadReplicaDBInstanceIdentifiers" :: NullOrUndefined (ReadReplicaDBInstanceIdentifierList)
  , "LicenseModel" :: NullOrUndefined (String)
  , "Iops" :: NullOrUndefined (IntegerOptional)
  , "OptionGroupMembership" :: NullOrUndefined (OptionGroupMembership)
  , "CharacterSetName" :: NullOrUndefined (String)
  , "SecondaryAvailabilityZone" :: NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined (Boolean)
  }


-- | <p>User already has a DB instance with the given identifier.</p>
newtype DBInstanceAlreadyExistsFault = DBInstanceAlreadyExistsFault 
  { 
  }


newtype DBInstanceList = DBInstanceList (Array DBInstance)


newtype DBInstanceMessage = DBInstanceMessage 
  { "Marker" :: NullOrUndefined (String)
  , "DBInstances" :: NullOrUndefined (DBInstanceList)
  }


-- | <p> <i>DBInstanceIdentifier</i> does not refer to an existing DB instance. </p>
newtype DBInstanceNotFoundFault = DBInstanceNotFoundFault 
  { 
  }


newtype DBParameterGroup = DBParameterGroup 
  { "DBParameterGroupName" :: NullOrUndefined (String)
  , "DBParameterGroupFamily" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  }


-- | <p>A DB parameter group with the same name exists.</p>
newtype DBParameterGroupAlreadyExistsFault = DBParameterGroupAlreadyExistsFault 
  { 
  }


newtype DBParameterGroupDetails = DBParameterGroupDetails 
  { "Parameters" :: NullOrUndefined (ParametersList)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DBParameterGroupList = DBParameterGroupList (Array DBParameterGroup)


newtype DBParameterGroupNameMessage = DBParameterGroupNameMessage 
  { "DBParameterGroupName" :: NullOrUndefined (String)
  }


-- | <p> <i>DBParameterGroupName</i> does not refer to an existing DB parameter group. </p>
newtype DBParameterGroupNotFoundFault = DBParameterGroupNotFoundFault 
  { 
  }


-- | <p>Request would result in user exceeding the allowed number of DB parameter groups.</p>
newtype DBParameterGroupQuotaExceededFault = DBParameterGroupQuotaExceededFault 
  { 
  }


newtype DBParameterGroupStatus = DBParameterGroupStatus 
  { "DBParameterGroupName" :: NullOrUndefined (String)
  , "ParameterApplyStatus" :: NullOrUndefined (String)
  }


newtype DBParameterGroupStatusList = DBParameterGroupStatusList (Array DBParameterGroupStatus)


newtype DBParameterGroupsMessage = DBParameterGroupsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "DBParameterGroups" :: NullOrUndefined (DBParameterGroupList)
  }


newtype DBSecurityGroup = DBSecurityGroup 
  { "OwnerId" :: NullOrUndefined (String)
  , "DBSecurityGroupName" :: NullOrUndefined (String)
  , "DBSecurityGroupDescription" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "EC2SecurityGroups" :: NullOrUndefined (EC2SecurityGroupList)
  , "IPRanges" :: NullOrUndefined (IPRangeList)
  }


-- | <p> A DB security group with the name specified in <i>DBSecurityGroupName</i> already exists. </p>
newtype DBSecurityGroupAlreadyExistsFault = DBSecurityGroupAlreadyExistsFault 
  { 
  }


newtype DBSecurityGroupMembership = DBSecurityGroupMembership 
  { "DBSecurityGroupName" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }


newtype DBSecurityGroupMembershipList = DBSecurityGroupMembershipList (Array DBSecurityGroupMembership)


newtype DBSecurityGroupMessage = DBSecurityGroupMessage 
  { "Marker" :: NullOrUndefined (String)
  , "DBSecurityGroups" :: NullOrUndefined (DBSecurityGroups)
  }


newtype DBSecurityGroupNameList = DBSecurityGroupNameList (Array String)


-- | <p> <i>DBSecurityGroupName</i> does not refer to an existing DB security group. </p>
newtype DBSecurityGroupNotFoundFault = DBSecurityGroupNotFoundFault 
  { 
  }


-- | <p>A DB security group is not allowed for this action.</p>
newtype DBSecurityGroupNotSupportedFault = DBSecurityGroupNotSupportedFault 
  { 
  }


-- | <p>Request would result in user exceeding the allowed number of DB security groups.</p>
newtype DBSecurityGroupQuotaExceededFault = DBSecurityGroupQuotaExceededFault 
  { 
  }


newtype DBSecurityGroups = DBSecurityGroups (Array DBSecurityGroup)


newtype DBSnapshot = DBSnapshot 
  { "DBSnapshotIdentifier" :: NullOrUndefined (String)
  , "DBInstanceIdentifier" :: NullOrUndefined (String)
  , "SnapshotCreateTime" :: NullOrUndefined (TStamp)
  , "Engine" :: NullOrUndefined (String)
  , "AllocatedStorage" :: NullOrUndefined (Int)
  , "Status" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (Int)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "InstanceCreateTime" :: NullOrUndefined (TStamp)
  , "MasterUsername" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "LicenseModel" :: NullOrUndefined (String)
  , "SnapshotType" :: NullOrUndefined (String)
  , "Iops" :: NullOrUndefined (IntegerOptional)
  }


-- | <p> <i>DBSnapshotIdentifier</i> is already used by an existing snapshot. </p>
newtype DBSnapshotAlreadyExistsFault = DBSnapshotAlreadyExistsFault 
  { 
  }


newtype DBSnapshotList = DBSnapshotList (Array DBSnapshot)


newtype DBSnapshotMessage = DBSnapshotMessage 
  { "Marker" :: NullOrUndefined (String)
  , "DBSnapshots" :: NullOrUndefined (DBSnapshotList)
  }


-- | <p> <i>DBSnapshotIdentifier</i> does not refer to an existing DB snapshot. </p>
newtype DBSnapshotNotFoundFault = DBSnapshotNotFoundFault 
  { 
  }


newtype DBSubnetGroup = DBSubnetGroup 
  { "DBSubnetGroupName" :: NullOrUndefined (String)
  , "DBSubnetGroupDescription" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "SubnetGroupStatus" :: NullOrUndefined (String)
  , "Subnets" :: NullOrUndefined (SubnetList)
  }


-- | <p> <i>DBSubnetGroupName</i> is already used by an existing DB subnet group. </p>
newtype DBSubnetGroupAlreadyExistsFault = DBSubnetGroupAlreadyExistsFault 
  { 
  }


-- | <p>Subnets in the DB subnet group should cover at least two Availability Zones unless there is only one Availability Zone.</p>
newtype DBSubnetGroupDoesNotCoverEnoughAZs = DBSubnetGroupDoesNotCoverEnoughAZs 
  { 
  }


newtype DBSubnetGroupMessage = DBSubnetGroupMessage 
  { "Marker" :: NullOrUndefined (String)
  , "DBSubnetGroups" :: NullOrUndefined (DBSubnetGroups)
  }


-- | <p> <i>DBSubnetGroupName</i> does not refer to an existing DB subnet group. </p>
newtype DBSubnetGroupNotFoundFault = DBSubnetGroupNotFoundFault 
  { 
  }


-- | <p>Request would result in user exceeding the allowed number of DB subnet groups.</p>
newtype DBSubnetGroupQuotaExceededFault = DBSubnetGroupQuotaExceededFault 
  { 
  }


newtype DBSubnetGroups = DBSubnetGroups (Array DBSubnetGroup)


-- | <p>Request would result in user exceeding the allowed number of subnets in a DB subnet groups.</p>
newtype DBSubnetQuotaExceededFault = DBSubnetQuotaExceededFault 
  { 
  }


-- | <p>The DB upgrade failed because a resource the DB depends on could not be modified.</p>
newtype DBUpgradeDependencyFailureFault = DBUpgradeDependencyFailureFault 
  { 
  }


newtype DeleteDBInstanceMessage = DeleteDBInstanceMessage 
  { "DBInstanceIdentifier" :: (String)
  , "SkipFinalSnapshot" :: NullOrUndefined (Boolean)
  , "FinalDBSnapshotIdentifier" :: NullOrUndefined (String)
  }


newtype DeleteDBInstanceResult = DeleteDBInstanceResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }


newtype DeleteDBParameterGroupMessage = DeleteDBParameterGroupMessage 
  { "DBParameterGroupName" :: (String)
  }


newtype DeleteDBSecurityGroupMessage = DeleteDBSecurityGroupMessage 
  { "DBSecurityGroupName" :: (String)
  }


newtype DeleteDBSnapshotMessage = DeleteDBSnapshotMessage 
  { "DBSnapshotIdentifier" :: (String)
  }


newtype DeleteDBSnapshotResult = DeleteDBSnapshotResult 
  { "DBSnapshot" :: NullOrUndefined (DBSnapshot)
  }


newtype DeleteDBSubnetGroupMessage = DeleteDBSubnetGroupMessage 
  { "DBSubnetGroupName" :: (String)
  }


newtype DeleteEventSubscriptionMessage = DeleteEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  }


newtype DeleteEventSubscriptionResult = DeleteEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }


newtype DeleteOptionGroupMessage = DeleteOptionGroupMessage 
  { "OptionGroupName" :: (String)
  }


newtype DescribeDBEngineVersionsMessage = DescribeDBEngineVersionsMessage 
  { "Engine" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "DBParameterGroupFamily" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "DefaultOnly" :: NullOrUndefined (Boolean)
  , "ListSupportedCharacterSets" :: NullOrUndefined (BooleanOptional)
  }


newtype DescribeDBInstancesMessage = DescribeDBInstancesMessage 
  { "DBInstanceIdentifier" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeDBParameterGroupsMessage = DescribeDBParameterGroupsMessage 
  { "DBParameterGroupName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeDBParametersMessage = DescribeDBParametersMessage 
  { "DBParameterGroupName" :: (String)
  , "Source" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeDBSecurityGroupsMessage = DescribeDBSecurityGroupsMessage 
  { "DBSecurityGroupName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeDBSnapshotsMessage = DescribeDBSnapshotsMessage 
  { "DBInstanceIdentifier" :: NullOrUndefined (String)
  , "DBSnapshotIdentifier" :: NullOrUndefined (String)
  , "SnapshotType" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeDBSubnetGroupsMessage = DescribeDBSubnetGroupsMessage 
  { "DBSubnetGroupName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeEngineDefaultParametersMessage = DescribeEngineDefaultParametersMessage 
  { "DBParameterGroupFamily" :: (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeEngineDefaultParametersResult = DescribeEngineDefaultParametersResult 
  { "EngineDefaults" :: NullOrUndefined (EngineDefaults)
  }


newtype DescribeEventCategoriesMessage = DescribeEventCategoriesMessage 
  { "SourceType" :: NullOrUndefined (String)
  }


newtype DescribeEventSubscriptionsMessage = DescribeEventSubscriptionsMessage 
  { "SubscriptionName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeEventsMessage = DescribeEventsMessage 
  { "SourceIdentifier" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (SourceType)
  , "StartTime" :: NullOrUndefined (TStamp)
  , "EndTime" :: NullOrUndefined (TStamp)
  , "Duration" :: NullOrUndefined (IntegerOptional)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeOptionGroupOptionsMessage = DescribeOptionGroupOptionsMessage 
  { "EngineName" :: (String)
  , "MajorEngineVersion" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeOptionGroupsMessage = DescribeOptionGroupsMessage 
  { "OptionGroupName" :: NullOrUndefined (String)
  , "Marker" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "EngineName" :: NullOrUndefined (String)
  , "MajorEngineVersion" :: NullOrUndefined (String)
  }


newtype DescribeOrderableDBInstanceOptionsMessage = DescribeOrderableDBInstanceOptionsMessage 
  { "Engine" :: (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined (String)
  , "LicenseModel" :: NullOrUndefined (String)
  , "Vpc" :: NullOrUndefined (BooleanOptional)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeReservedDBInstancesMessage = DescribeReservedDBInstancesMessage 
  { "ReservedDBInstanceId" :: NullOrUndefined (String)
  , "ReservedDBInstancesOfferingId" :: NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined (String)
  , "Duration" :: NullOrUndefined (String)
  , "ProductDescription" :: NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined (BooleanOptional)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype DescribeReservedDBInstancesOfferingsMessage = DescribeReservedDBInstancesOfferingsMessage 
  { "ReservedDBInstancesOfferingId" :: NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined (String)
  , "Duration" :: NullOrUndefined (String)
  , "ProductDescription" :: NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined (BooleanOptional)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }


newtype EC2SecurityGroup = EC2SecurityGroup 
  { "Status" :: NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined (String)
  , "EC2SecurityGroupId" :: NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined (String)
  }


newtype EC2SecurityGroupList = EC2SecurityGroupList (Array EC2SecurityGroup)


newtype Endpoint = Endpoint 
  { "Address" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (Int)
  }


newtype EngineDefaults = EngineDefaults 
  { "DBParameterGroupFamily" :: NullOrUndefined (String)
  , "Marker" :: NullOrUndefined (String)
  , "Parameters" :: NullOrUndefined (ParametersList)
  }


newtype Event = Event 
  { "SourceIdentifier" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (SourceType)
  , "Message" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "Date" :: NullOrUndefined (TStamp)
  }


newtype EventCategoriesList = EventCategoriesList (Array String)


newtype EventCategoriesMap = EventCategoriesMap 
  { "SourceType" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  }


newtype EventCategoriesMapList = EventCategoriesMapList (Array EventCategoriesMap)


newtype EventCategoriesMessage = EventCategoriesMessage 
  { "EventCategoriesMapList" :: NullOrUndefined (EventCategoriesMapList)
  }


newtype EventList = EventList (Array Event)


newtype EventSubscription = EventSubscription 
  { "Id" :: NullOrUndefined (String)
  , "CustomerAwsId" :: NullOrUndefined (String)
  , "CustSubscriptionId" :: NullOrUndefined (String)
  , "SnsTopicArn" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  , "SubscriptionCreationTime" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (String)
  , "SourceIdsList" :: NullOrUndefined (SourceIdsList)
  , "EventCategoriesList" :: NullOrUndefined (EventCategoriesList)
  , "Enabled" :: NullOrUndefined (Boolean)
  }


-- | <p>You have reached the maximum number of event subscriptions.</p>
newtype EventSubscriptionQuotaExceededFault = EventSubscriptionQuotaExceededFault 
  { 
  }


newtype EventSubscriptionsList = EventSubscriptionsList (Array EventSubscription)


newtype EventSubscriptionsMessage = EventSubscriptionsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "EventSubscriptionsList" :: NullOrUndefined (EventSubscriptionsList)
  }


newtype EventsMessage = EventsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "Events" :: NullOrUndefined (EventList)
  }


newtype IPRange = IPRange 
  { "Status" :: NullOrUndefined (String)
  , "CIDRIP" :: NullOrUndefined (String)
  }


newtype IPRangeList = IPRangeList (Array IPRange)


-- | <p>Request would result in user exceeding the allowed number of DB instances.</p>
newtype InstanceQuotaExceededFault = InstanceQuotaExceededFault 
  { 
  }


-- | <p>Specified DB instance class is not available in the specified Availability Zone.</p>
newtype InsufficientDBInstanceCapacityFault = InsufficientDBInstanceCapacityFault 
  { 
  }


newtype IntegerOptional = IntegerOptional Int


-- | <p> The specified DB instance is not in the <i>available</i> state. </p>
newtype InvalidDBInstanceStateFault = InvalidDBInstanceStateFault 
  { 
  }


-- | <p>The DB parameter group is in use or is in an invalid state. If you are attempting to delete the parameter group, you cannot delete it when the parameter group is in this state.</p>
newtype InvalidDBParameterGroupStateFault = InvalidDBParameterGroupStateFault 
  { 
  }


-- | <p>The state of the DB security group does not allow deletion.</p>
newtype InvalidDBSecurityGroupStateFault = InvalidDBSecurityGroupStateFault 
  { 
  }


-- | <p>The state of the DB snapshot does not allow deletion.</p>
newtype InvalidDBSnapshotStateFault = InvalidDBSnapshotStateFault 
  { 
  }


-- | <p>The DB subnet group cannot be deleted because it is in use.</p>
newtype InvalidDBSubnetGroupStateFault = InvalidDBSubnetGroupStateFault 
  { 
  }


-- | <p> The DB subnet is not in the <i>available</i> state. </p>
newtype InvalidDBSubnetStateFault = InvalidDBSubnetStateFault 
  { 
  }


-- | <p>This error can occur if someone else is modifying a subscription. You should retry the action.</p>
newtype InvalidEventSubscriptionStateFault = InvalidEventSubscriptionStateFault 
  { 
  }


-- | <p> The option group is not in the <i>available</i> state. </p>
newtype InvalidOptionGroupStateFault = InvalidOptionGroupStateFault 
  { 
  }


-- | <p>Cannot restore from vpc backup to non-vpc DB instance.</p>
newtype InvalidRestoreFault = InvalidRestoreFault 
  { 
  }


-- | <p>The requested subnet is invalid, or multiple subnets were requested that are not all in a common VPC.</p>
newtype InvalidSubnet = InvalidSubnet 
  { 
  }


-- | <p>DB subnet group does not cover all Availability Zones after it is created because users' change.</p>
newtype InvalidVPCNetworkStateFault = InvalidVPCNetworkStateFault 
  { 
  }


newtype KeyList = KeyList (Array String)


newtype ListTagsForResourceMessage = ListTagsForResourceMessage 
  { "ResourceName" :: (String)
  }


newtype ModifyDBInstanceMessage = ModifyDBInstanceMessage 
  { "DBInstanceIdentifier" :: (String)
  , "AllocatedStorage" :: NullOrUndefined (IntegerOptional)
  , "DBInstanceClass" :: NullOrUndefined (String)
  , "DBSecurityGroups" :: NullOrUndefined (DBSecurityGroupNameList)
  , "VpcSecurityGroupIds" :: NullOrUndefined (VpcSecurityGroupIdList)
  , "ApplyImmediately" :: NullOrUndefined (Boolean)
  , "MasterUserPassword" :: NullOrUndefined (String)
  , "DBParameterGroupName" :: NullOrUndefined (String)
  , "BackupRetentionPeriod" :: NullOrUndefined (IntegerOptional)
  , "PreferredBackupWindow" :: NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined (BooleanOptional)
  , "EngineVersion" :: NullOrUndefined (String)
  , "AllowMajorVersionUpgrade" :: NullOrUndefined (Boolean)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "Iops" :: NullOrUndefined (IntegerOptional)
  , "OptionGroupName" :: NullOrUndefined (String)
  , "NewDBInstanceIdentifier" :: NullOrUndefined (String)
  }


newtype ModifyDBInstanceResult = ModifyDBInstanceResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }


newtype ModifyDBParameterGroupMessage = ModifyDBParameterGroupMessage 
  { "DBParameterGroupName" :: (String)
  , "Parameters" :: (ParametersList)
  }


newtype ModifyDBSubnetGroupMessage = ModifyDBSubnetGroupMessage 
  { "DBSubnetGroupName" :: (String)
  , "DBSubnetGroupDescription" :: NullOrUndefined (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  }


newtype ModifyDBSubnetGroupResult = ModifyDBSubnetGroupResult 
  { "DBSubnetGroup" :: NullOrUndefined (DBSubnetGroup)
  }


newtype ModifyEventSubscriptionMessage = ModifyEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SnsTopicArn" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "Enabled" :: NullOrUndefined (BooleanOptional)
  }


newtype ModifyEventSubscriptionResult = ModifyEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }


newtype ModifyOptionGroupMessage = ModifyOptionGroupMessage 
  { "OptionGroupName" :: (String)
  , "OptionsToInclude" :: NullOrUndefined (OptionConfigurationList)
  , "OptionsToRemove" :: NullOrUndefined (OptionNamesList)
  , "ApplyImmediately" :: NullOrUndefined (Boolean)
  }


newtype ModifyOptionGroupResult = ModifyOptionGroupResult 
  { "OptionGroup" :: NullOrUndefined (OptionGroup)
  }


newtype Option = Option 
  { "OptionName" :: NullOrUndefined (String)
  , "OptionDescription" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "DBSecurityGroupMemberships" :: NullOrUndefined (DBSecurityGroupMembershipList)
  , "VpcSecurityGroupMemberships" :: NullOrUndefined (VpcSecurityGroupMembershipList)
  }


newtype OptionConfiguration = OptionConfiguration 
  { "OptionName" :: (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "DBSecurityGroupMemberships" :: NullOrUndefined (DBSecurityGroupNameList)
  , "VpcSecurityGroupMemberships" :: NullOrUndefined (VpcSecurityGroupIdList)
  }


newtype OptionConfigurationList = OptionConfigurationList (Array OptionConfiguration)


newtype OptionGroup = OptionGroup 
  { "OptionGroupName" :: NullOrUndefined (String)
  , "OptionGroupDescription" :: NullOrUndefined (String)
  , "EngineName" :: NullOrUndefined (String)
  , "MajorEngineVersion" :: NullOrUndefined (String)
  , "Options" :: NullOrUndefined (OptionsList)
  , "AllowsVpcAndNonVpcInstanceMemberships" :: NullOrUndefined (Boolean)
  , "VpcId" :: NullOrUndefined (String)
  }


-- | <p>The option group you are trying to create already exists.</p>
newtype OptionGroupAlreadyExistsFault = OptionGroupAlreadyExistsFault 
  { 
  }


newtype OptionGroupMembership = OptionGroupMembership 
  { "OptionGroupName" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }


-- | <p>The specified option group could not be found.</p>
newtype OptionGroupNotFoundFault = OptionGroupNotFoundFault 
  { 
  }


newtype OptionGroupOption = OptionGroupOption 
  { "Name" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "EngineName" :: NullOrUndefined (String)
  , "MajorEngineVersion" :: NullOrUndefined (String)
  , "MinimumRequiredMinorEngineVersion" :: NullOrUndefined (String)
  , "PortRequired" :: NullOrUndefined (Boolean)
  , "DefaultPort" :: NullOrUndefined (IntegerOptional)
  , "OptionsDependedOn" :: NullOrUndefined (OptionsDependedOn)
  }


newtype OptionGroupOptionsList = OptionGroupOptionsList (Array OptionGroupOption)


newtype OptionGroupOptionsMessage = OptionGroupOptionsMessage 
  { "OptionGroupOptions" :: NullOrUndefined (OptionGroupOptionsList)
  , "Marker" :: NullOrUndefined (String)
  }


-- | <p>The quota of 20 option groups was exceeded for this AWS account.</p>
newtype OptionGroupQuotaExceededFault = OptionGroupQuotaExceededFault 
  { 
  }


newtype OptionGroups = OptionGroups 
  { "OptionGroupsList" :: NullOrUndefined (OptionGroupsList)
  , "Marker" :: NullOrUndefined (String)
  }


newtype OptionGroupsList = OptionGroupsList (Array OptionGroup)


newtype OptionNamesList = OptionNamesList (Array String)


newtype OptionsDependedOn = OptionsDependedOn (Array String)


newtype OptionsList = OptionsList (Array Option)


newtype OrderableDBInstanceOption = OrderableDBInstanceOption 
  { "Engine" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined (String)
  , "LicenseModel" :: NullOrUndefined (String)
  , "AvailabilityZones" :: NullOrUndefined (AvailabilityZoneList)
  , "MultiAZCapable" :: NullOrUndefined (Boolean)
  , "ReadReplicaCapable" :: NullOrUndefined (Boolean)
  , "Vpc" :: NullOrUndefined (Boolean)
  }


newtype OrderableDBInstanceOptionsList = OrderableDBInstanceOptionsList (Array OrderableDBInstanceOption)


newtype OrderableDBInstanceOptionsMessage = OrderableDBInstanceOptionsMessage 
  { "OrderableDBInstanceOptions" :: NullOrUndefined (OrderableDBInstanceOptionsList)
  , "Marker" :: NullOrUndefined (String)
  }


newtype Parameter = Parameter 
  { "ParameterName" :: NullOrUndefined (String)
  , "ParameterValue" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "Source" :: NullOrUndefined (String)
  , "ApplyType" :: NullOrUndefined (String)
  , "DataType" :: NullOrUndefined (String)
  , "AllowedValues" :: NullOrUndefined (String)
  , "IsModifiable" :: NullOrUndefined (Boolean)
  , "MinimumEngineVersion" :: NullOrUndefined (String)
  , "ApplyMethod" :: NullOrUndefined (ApplyMethod)
  }


newtype ParametersList = ParametersList (Array Parameter)


newtype PendingModifiedValues = PendingModifiedValues 
  { "DBInstanceClass" :: NullOrUndefined (String)
  , "AllocatedStorage" :: NullOrUndefined (IntegerOptional)
  , "MasterUserPassword" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "BackupRetentionPeriod" :: NullOrUndefined (IntegerOptional)
  , "MultiAZ" :: NullOrUndefined (BooleanOptional)
  , "EngineVersion" :: NullOrUndefined (String)
  , "Iops" :: NullOrUndefined (IntegerOptional)
  , "DBInstanceIdentifier" :: NullOrUndefined (String)
  }


-- | <p> <i>SourceDBInstanceIdentifier</i> refers to a DB instance with <i>BackupRetentionPeriod</i> equal to 0. </p>
newtype PointInTimeRestoreNotEnabledFault = PointInTimeRestoreNotEnabledFault 
  { 
  }


newtype PromoteReadReplicaMessage = PromoteReadReplicaMessage 
  { "DBInstanceIdentifier" :: (String)
  , "BackupRetentionPeriod" :: NullOrUndefined (IntegerOptional)
  , "PreferredBackupWindow" :: NullOrUndefined (String)
  }


newtype PromoteReadReplicaResult = PromoteReadReplicaResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }


-- | <p>Provisioned IOPS not available in the specified Availability Zone.</p>
newtype ProvisionedIopsNotAvailableInAZFault = ProvisionedIopsNotAvailableInAZFault 
  { 
  }


newtype PurchaseReservedDBInstancesOfferingMessage = PurchaseReservedDBInstancesOfferingMessage 
  { "ReservedDBInstancesOfferingId" :: (String)
  , "ReservedDBInstanceId" :: NullOrUndefined (String)
  , "DBInstanceCount" :: NullOrUndefined (IntegerOptional)
  }


newtype PurchaseReservedDBInstancesOfferingResult = PurchaseReservedDBInstancesOfferingResult 
  { "ReservedDBInstance" :: NullOrUndefined (ReservedDBInstance)
  }


newtype ReadReplicaDBInstanceIdentifierList = ReadReplicaDBInstanceIdentifierList (Array String)


newtype RebootDBInstanceMessage = RebootDBInstanceMessage 
  { "DBInstanceIdentifier" :: (String)
  , "ForceFailover" :: NullOrUndefined (BooleanOptional)
  }


newtype RebootDBInstanceResult = RebootDBInstanceResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }


newtype RecurringCharge = RecurringCharge 
  { "RecurringChargeAmount" :: NullOrUndefined (Number)
  , "RecurringChargeFrequency" :: NullOrUndefined (String)
  }


newtype RecurringChargeList = RecurringChargeList (Array RecurringCharge)


newtype RemoveSourceIdentifierFromSubscriptionMessage = RemoveSourceIdentifierFromSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SourceIdentifier" :: (String)
  }


newtype RemoveSourceIdentifierFromSubscriptionResult = RemoveSourceIdentifierFromSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }


newtype RemoveTagsFromResourceMessage = RemoveTagsFromResourceMessage 
  { "ResourceName" :: (String)
  , "TagKeys" :: (KeyList)
  }


newtype ReservedDBInstance = ReservedDBInstance 
  { "ReservedDBInstanceId" :: NullOrUndefined (String)
  , "ReservedDBInstancesOfferingId" :: NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined (String)
  , "StartTime" :: NullOrUndefined (TStamp)
  , "Duration" :: NullOrUndefined (Int)
  , "FixedPrice" :: NullOrUndefined (Number)
  , "UsagePrice" :: NullOrUndefined (Number)
  , "CurrencyCode" :: NullOrUndefined (String)
  , "DBInstanceCount" :: NullOrUndefined (Int)
  , "ProductDescription" :: NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined (Boolean)
  , "State" :: NullOrUndefined (String)
  , "RecurringCharges" :: NullOrUndefined (RecurringChargeList)
  }


-- | <p>User already has a reservation with the given identifier.</p>
newtype ReservedDBInstanceAlreadyExistsFault = ReservedDBInstanceAlreadyExistsFault 
  { 
  }


newtype ReservedDBInstanceList = ReservedDBInstanceList (Array ReservedDBInstance)


newtype ReservedDBInstanceMessage = ReservedDBInstanceMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ReservedDBInstances" :: NullOrUndefined (ReservedDBInstanceList)
  }


-- | <p>The specified reserved DB Instance not found.</p>
newtype ReservedDBInstanceNotFoundFault = ReservedDBInstanceNotFoundFault 
  { 
  }


-- | <p>Request would exceed the user's DB Instance quota.</p>
newtype ReservedDBInstanceQuotaExceededFault = ReservedDBInstanceQuotaExceededFault 
  { 
  }


newtype ReservedDBInstancesOffering = ReservedDBInstancesOffering 
  { "ReservedDBInstancesOfferingId" :: NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined (String)
  , "Duration" :: NullOrUndefined (Int)
  , "FixedPrice" :: NullOrUndefined (Number)
  , "UsagePrice" :: NullOrUndefined (Number)
  , "CurrencyCode" :: NullOrUndefined (String)
  , "ProductDescription" :: NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined (Boolean)
  , "RecurringCharges" :: NullOrUndefined (RecurringChargeList)
  }


newtype ReservedDBInstancesOfferingList = ReservedDBInstancesOfferingList (Array ReservedDBInstancesOffering)


newtype ReservedDBInstancesOfferingMessage = ReservedDBInstancesOfferingMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ReservedDBInstancesOfferings" :: NullOrUndefined (ReservedDBInstancesOfferingList)
  }


-- | <p>Specified offering does not exist.</p>
newtype ReservedDBInstancesOfferingNotFoundFault = ReservedDBInstancesOfferingNotFoundFault 
  { 
  }


newtype ResetDBParameterGroupMessage = ResetDBParameterGroupMessage 
  { "DBParameterGroupName" :: (String)
  , "ResetAllParameters" :: NullOrUndefined (Boolean)
  , "Parameters" :: NullOrUndefined (ParametersList)
  }


newtype RestoreDBInstanceFromDBSnapshotMessage = RestoreDBInstanceFromDBSnapshotMessage 
  { "DBInstanceIdentifier" :: (String)
  , "DBSnapshotIdentifier" :: (String)
  , "DBInstanceClass" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "DBSubnetGroupName" :: NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined (BooleanOptional)
  , "PubliclyAccessible" :: NullOrUndefined (BooleanOptional)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "LicenseModel" :: NullOrUndefined (String)
  , "DBName" :: NullOrUndefined (String)
  , "Engine" :: NullOrUndefined (String)
  , "Iops" :: NullOrUndefined (IntegerOptional)
  , "OptionGroupName" :: NullOrUndefined (String)
  }


newtype RestoreDBInstanceFromDBSnapshotResult = RestoreDBInstanceFromDBSnapshotResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }


newtype RestoreDBInstanceToPointInTimeMessage = RestoreDBInstanceToPointInTimeMessage 
  { "SourceDBInstanceIdentifier" :: (String)
  , "TargetDBInstanceIdentifier" :: (String)
  , "RestoreTime" :: NullOrUndefined (TStamp)
  , "UseLatestRestorableTime" :: NullOrUndefined (Boolean)
  , "DBInstanceClass" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "DBSubnetGroupName" :: NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined (BooleanOptional)
  , "PubliclyAccessible" :: NullOrUndefined (BooleanOptional)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined (BooleanOptional)
  , "LicenseModel" :: NullOrUndefined (String)
  , "DBName" :: NullOrUndefined (String)
  , "Engine" :: NullOrUndefined (String)
  , "Iops" :: NullOrUndefined (IntegerOptional)
  , "OptionGroupName" :: NullOrUndefined (String)
  }


newtype RestoreDBInstanceToPointInTimeResult = RestoreDBInstanceToPointInTimeResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }


newtype RevokeDBSecurityGroupIngressMessage = RevokeDBSecurityGroupIngressMessage 
  { "DBSecurityGroupName" :: (String)
  , "CIDRIP" :: NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined (String)
  , "EC2SecurityGroupId" :: NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined (String)
  }


newtype RevokeDBSecurityGroupIngressResult = RevokeDBSecurityGroupIngressResult 
  { "DBSecurityGroup" :: NullOrUndefined (DBSecurityGroup)
  }


-- | <p>SNS has responded that there is a problem with the SND topic specified.</p>
newtype SNSInvalidTopicFault = SNSInvalidTopicFault 
  { 
  }


-- | <p>You do not have permission to publish to the SNS topic ARN.</p>
newtype SNSNoAuthorizationFault = SNSNoAuthorizationFault 
  { 
  }


-- | <p>The SNS topic ARN does not exist.</p>
newtype SNSTopicArnNotFoundFault = SNSTopicArnNotFoundFault 
  { 
  }


-- | <p>Request would result in user exceeding the allowed number of DB snapshots.</p>
newtype SnapshotQuotaExceededFault = SnapshotQuotaExceededFault 
  { 
  }


newtype SourceIdsList = SourceIdsList (Array String)


-- | <p>The requested source could not be found.</p>
newtype SourceNotFoundFault = SourceNotFoundFault 
  { 
  }


newtype SourceType = SourceType String


-- | <p>Request would result in user exceeding the allowed amount of storage available across all DB instances.</p>
newtype StorageQuotaExceededFault = StorageQuotaExceededFault 
  { 
  }


newtype Subnet = Subnet 
  { "SubnetIdentifier" :: NullOrUndefined (String)
  , "SubnetAvailabilityZone" :: NullOrUndefined (AvailabilityZone)
  , "SubnetStatus" :: NullOrUndefined (String)
  }


-- | <p>The DB subnet is already in use in the Availability Zone.</p>
newtype SubnetAlreadyInUse = SubnetAlreadyInUse 
  { 
  }


newtype SubnetIdentifierList = SubnetIdentifierList (Array String)


newtype SubnetList = SubnetList (Array Subnet)


-- | <p>The supplied subscription name already exists.</p>
newtype SubscriptionAlreadyExistFault = SubscriptionAlreadyExistFault 
  { 
  }


-- | <p>The supplied category does not exist.</p>
newtype SubscriptionCategoryNotFoundFault = SubscriptionCategoryNotFoundFault 
  { 
  }


-- | <p>The subscription name does not exist.</p>
newtype SubscriptionNotFoundFault = SubscriptionNotFoundFault 
  { 
  }


newtype SupportedCharacterSetsList = SupportedCharacterSetsList (Array CharacterSet)


newtype TStamp = TStamp Number


newtype Tag = Tag 
  { "Key" :: NullOrUndefined (String)
  , "Value" :: NullOrUndefined (String)
  }


newtype TagList = TagList (Array Tag)


newtype TagListMessage = TagListMessage 
  { "TagList" :: NullOrUndefined (TagList)
  }


newtype VpcSecurityGroupIdList = VpcSecurityGroupIdList (Array String)


newtype VpcSecurityGroupMembership = VpcSecurityGroupMembership 
  { "VpcSecurityGroupId" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }


newtype VpcSecurityGroupMembershipList = VpcSecurityGroupMembershipList (Array VpcSecurityGroupMembership)
