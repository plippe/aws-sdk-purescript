

module AWS.RDS where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "RDS" :: String


addSourceIdentifierToSubscription :: forall eff. AddSourceIdentifierToSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) AddSourceIdentifierToSubscriptionResult
addSourceIdentifierToSubscription = AWS.request serviceName "addSourceIdentifierToSubscription" 


addTagsToResource :: forall eff. AddTagsToResourceMessage -> Aff (err :: AWS.RequestError | eff) Unit
addTagsToResource = AWS.request serviceName "addTagsToResource" 


authorizeDBSecurityGroupIngress :: forall eff. AuthorizeDBSecurityGroupIngressMessage -> Aff (err :: AWS.RequestError | eff) AuthorizeDBSecurityGroupIngressResult
authorizeDBSecurityGroupIngress = AWS.request serviceName "authorizeDBSecurityGroupIngress" 


copyDBSnapshot :: forall eff. CopyDBSnapshotMessage -> Aff (err :: AWS.RequestError | eff) CopyDBSnapshotResult
copyDBSnapshot = AWS.request serviceName "copyDBSnapshot" 


createDBInstance :: forall eff. CreateDBInstanceMessage -> Aff (err :: AWS.RequestError | eff) CreateDBInstanceResult
createDBInstance = AWS.request serviceName "createDBInstance" 


createDBInstanceReadReplica :: forall eff. CreateDBInstanceReadReplicaMessage -> Aff (err :: AWS.RequestError | eff) CreateDBInstanceReadReplicaResult
createDBInstanceReadReplica = AWS.request serviceName "createDBInstanceReadReplica" 


createDBParameterGroup :: forall eff. CreateDBParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateDBParameterGroupResult
createDBParameterGroup = AWS.request serviceName "createDBParameterGroup" 


createDBSecurityGroup :: forall eff. CreateDBSecurityGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateDBSecurityGroupResult
createDBSecurityGroup = AWS.request serviceName "createDBSecurityGroup" 


createDBSnapshot :: forall eff. CreateDBSnapshotMessage -> Aff (err :: AWS.RequestError | eff) CreateDBSnapshotResult
createDBSnapshot = AWS.request serviceName "createDBSnapshot" 


createDBSubnetGroup :: forall eff. CreateDBSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateDBSubnetGroupResult
createDBSubnetGroup = AWS.request serviceName "createDBSubnetGroup" 


createEventSubscription :: forall eff. CreateEventSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) CreateEventSubscriptionResult
createEventSubscription = AWS.request serviceName "createEventSubscription" 


createOptionGroup :: forall eff. CreateOptionGroupMessage -> Aff (err :: AWS.RequestError | eff) CreateOptionGroupResult
createOptionGroup = AWS.request serviceName "createOptionGroup" 


deleteDBInstance :: forall eff. DeleteDBInstanceMessage -> Aff (err :: AWS.RequestError | eff) DeleteDBInstanceResult
deleteDBInstance = AWS.request serviceName "deleteDBInstance" 


deleteDBParameterGroup :: forall eff. DeleteDBParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteDBParameterGroup = AWS.request serviceName "deleteDBParameterGroup" 


deleteDBSecurityGroup :: forall eff. DeleteDBSecurityGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteDBSecurityGroup = AWS.request serviceName "deleteDBSecurityGroup" 


deleteDBSnapshot :: forall eff. DeleteDBSnapshotMessage -> Aff (err :: AWS.RequestError | eff) DeleteDBSnapshotResult
deleteDBSnapshot = AWS.request serviceName "deleteDBSnapshot" 


deleteDBSubnetGroup :: forall eff. DeleteDBSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteDBSubnetGroup = AWS.request serviceName "deleteDBSubnetGroup" 


deleteEventSubscription :: forall eff. DeleteEventSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) DeleteEventSubscriptionResult
deleteEventSubscription = AWS.request serviceName "deleteEventSubscription" 


deleteOptionGroup :: forall eff. DeleteOptionGroupMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteOptionGroup = AWS.request serviceName "deleteOptionGroup" 


describeDBEngineVersions :: forall eff. DescribeDBEngineVersionsMessage -> Aff (err :: AWS.RequestError | eff) DBEngineVersionMessage
describeDBEngineVersions = AWS.request serviceName "describeDBEngineVersions" 


describeDBInstances :: forall eff. DescribeDBInstancesMessage -> Aff (err :: AWS.RequestError | eff) DBInstanceMessage
describeDBInstances = AWS.request serviceName "describeDBInstances" 


describeDBParameterGroups :: forall eff. DescribeDBParameterGroupsMessage -> Aff (err :: AWS.RequestError | eff) DBParameterGroupsMessage
describeDBParameterGroups = AWS.request serviceName "describeDBParameterGroups" 


describeDBParameters :: forall eff. DescribeDBParametersMessage -> Aff (err :: AWS.RequestError | eff) DBParameterGroupDetails
describeDBParameters = AWS.request serviceName "describeDBParameters" 


describeDBSecurityGroups :: forall eff. DescribeDBSecurityGroupsMessage -> Aff (err :: AWS.RequestError | eff) DBSecurityGroupMessage
describeDBSecurityGroups = AWS.request serviceName "describeDBSecurityGroups" 


describeDBSnapshots :: forall eff. DescribeDBSnapshotsMessage -> Aff (err :: AWS.RequestError | eff) DBSnapshotMessage
describeDBSnapshots = AWS.request serviceName "describeDBSnapshots" 


describeDBSubnetGroups :: forall eff. DescribeDBSubnetGroupsMessage -> Aff (err :: AWS.RequestError | eff) DBSubnetGroupMessage
describeDBSubnetGroups = AWS.request serviceName "describeDBSubnetGroups" 


describeEngineDefaultParameters :: forall eff. DescribeEngineDefaultParametersMessage -> Aff (err :: AWS.RequestError | eff) DescribeEngineDefaultParametersResult
describeEngineDefaultParameters = AWS.request serviceName "describeEngineDefaultParameters" 


describeEventCategories :: forall eff. DescribeEventCategoriesMessage -> Aff (err :: AWS.RequestError | eff) EventCategoriesMessage
describeEventCategories = AWS.request serviceName "describeEventCategories" 


describeEventSubscriptions :: forall eff. DescribeEventSubscriptionsMessage -> Aff (err :: AWS.RequestError | eff) EventSubscriptionsMessage
describeEventSubscriptions = AWS.request serviceName "describeEventSubscriptions" 


describeEvents :: forall eff. DescribeEventsMessage -> Aff (err :: AWS.RequestError | eff) EventsMessage
describeEvents = AWS.request serviceName "describeEvents" 


describeOptionGroupOptions :: forall eff. DescribeOptionGroupOptionsMessage -> Aff (err :: AWS.RequestError | eff) OptionGroupOptionsMessage
describeOptionGroupOptions = AWS.request serviceName "describeOptionGroupOptions" 


describeOptionGroups :: forall eff. DescribeOptionGroupsMessage -> Aff (err :: AWS.RequestError | eff) OptionGroups
describeOptionGroups = AWS.request serviceName "describeOptionGroups" 


describeOrderableDBInstanceOptions :: forall eff. DescribeOrderableDBInstanceOptionsMessage -> Aff (err :: AWS.RequestError | eff) OrderableDBInstanceOptionsMessage
describeOrderableDBInstanceOptions = AWS.request serviceName "describeOrderableDBInstanceOptions" 


describeReservedDBInstances :: forall eff. DescribeReservedDBInstancesMessage -> Aff (err :: AWS.RequestError | eff) ReservedDBInstanceMessage
describeReservedDBInstances = AWS.request serviceName "describeReservedDBInstances" 


describeReservedDBInstancesOfferings :: forall eff. DescribeReservedDBInstancesOfferingsMessage -> Aff (err :: AWS.RequestError | eff) ReservedDBInstancesOfferingMessage
describeReservedDBInstancesOfferings = AWS.request serviceName "describeReservedDBInstancesOfferings" 


listTagsForResource :: forall eff. ListTagsForResourceMessage -> Aff (err :: AWS.RequestError | eff) TagListMessage
listTagsForResource = AWS.request serviceName "listTagsForResource" 


modifyDBInstance :: forall eff. ModifyDBInstanceMessage -> Aff (err :: AWS.RequestError | eff) ModifyDBInstanceResult
modifyDBInstance = AWS.request serviceName "modifyDBInstance" 


modifyDBParameterGroup :: forall eff. ModifyDBParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) DBParameterGroupNameMessage
modifyDBParameterGroup = AWS.request serviceName "modifyDBParameterGroup" 


modifyDBSubnetGroup :: forall eff. ModifyDBSubnetGroupMessage -> Aff (err :: AWS.RequestError | eff) ModifyDBSubnetGroupResult
modifyDBSubnetGroup = AWS.request serviceName "modifyDBSubnetGroup" 


modifyEventSubscription :: forall eff. ModifyEventSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) ModifyEventSubscriptionResult
modifyEventSubscription = AWS.request serviceName "modifyEventSubscription" 


modifyOptionGroup :: forall eff. ModifyOptionGroupMessage -> Aff (err :: AWS.RequestError | eff) ModifyOptionGroupResult
modifyOptionGroup = AWS.request serviceName "modifyOptionGroup" 


promoteReadReplica :: forall eff. PromoteReadReplicaMessage -> Aff (err :: AWS.RequestError | eff) PromoteReadReplicaResult
promoteReadReplica = AWS.request serviceName "promoteReadReplica" 


purchaseReservedDBInstancesOffering :: forall eff. PurchaseReservedDBInstancesOfferingMessage -> Aff (err :: AWS.RequestError | eff) PurchaseReservedDBInstancesOfferingResult
purchaseReservedDBInstancesOffering = AWS.request serviceName "purchaseReservedDBInstancesOffering" 


rebootDBInstance :: forall eff. RebootDBInstanceMessage -> Aff (err :: AWS.RequestError | eff) RebootDBInstanceResult
rebootDBInstance = AWS.request serviceName "rebootDBInstance" 


removeSourceIdentifierFromSubscription :: forall eff. RemoveSourceIdentifierFromSubscriptionMessage -> Aff (err :: AWS.RequestError | eff) RemoveSourceIdentifierFromSubscriptionResult
removeSourceIdentifierFromSubscription = AWS.request serviceName "removeSourceIdentifierFromSubscription" 


removeTagsFromResource :: forall eff. RemoveTagsFromResourceMessage -> Aff (err :: AWS.RequestError | eff) Unit
removeTagsFromResource = AWS.request serviceName "removeTagsFromResource" 


resetDBParameterGroup :: forall eff. ResetDBParameterGroupMessage -> Aff (err :: AWS.RequestError | eff) DBParameterGroupNameMessage
resetDBParameterGroup = AWS.request serviceName "resetDBParameterGroup" 


restoreDBInstanceFromDBSnapshot :: forall eff. RestoreDBInstanceFromDBSnapshotMessage -> Aff (err :: AWS.RequestError | eff) RestoreDBInstanceFromDBSnapshotResult
restoreDBInstanceFromDBSnapshot = AWS.request serviceName "restoreDBInstanceFromDBSnapshot" 


restoreDBInstanceToPointInTime :: forall eff. RestoreDBInstanceToPointInTimeMessage -> Aff (err :: AWS.RequestError | eff) RestoreDBInstanceToPointInTimeResult
restoreDBInstanceToPointInTime = AWS.request serviceName "restoreDBInstanceToPointInTime" 


revokeDBSecurityGroupIngress :: forall eff. RevokeDBSecurityGroupIngressMessage -> Aff (err :: AWS.RequestError | eff) RevokeDBSecurityGroupIngressResult
revokeDBSecurityGroupIngress = AWS.request serviceName "revokeDBSecurityGroupIngress" 


newtype AddSourceIdentifierToSubscriptionMessage = AddSourceIdentifierToSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SourceIdentifier" :: (String)
  }
derive instance newtypeAddSourceIdentifierToSubscriptionMessage :: Newtype AddSourceIdentifierToSubscriptionMessage _


newtype AddSourceIdentifierToSubscriptionResult = AddSourceIdentifierToSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }
derive instance newtypeAddSourceIdentifierToSubscriptionResult :: Newtype AddSourceIdentifierToSubscriptionResult _


newtype AddTagsToResourceMessage = AddTagsToResourceMessage 
  { "ResourceName" :: (String)
  , "Tags" :: (TagList)
  }
derive instance newtypeAddTagsToResourceMessage :: Newtype AddTagsToResourceMessage _


newtype ApplyMethod = ApplyMethod String
derive instance newtypeApplyMethod :: Newtype ApplyMethod _


-- | <p>The specified CIDRIP or EC2 security group is already authorized for the specified DB security group.</p>
newtype AuthorizationAlreadyExistsFault = AuthorizationAlreadyExistsFault 
  { 
  }
derive instance newtypeAuthorizationAlreadyExistsFault :: Newtype AuthorizationAlreadyExistsFault _


-- | <p>Specified CIDRIP or EC2 security group is not authorized for the specified DB security group.</p> <p>RDS may not also be authorized via IAM to perform necessary actions on your behalf.</p>
newtype AuthorizationNotFoundFault = AuthorizationNotFoundFault 
  { 
  }
derive instance newtypeAuthorizationNotFoundFault :: Newtype AuthorizationNotFoundFault _


-- | <p>DB security group authorization quota has been reached.</p>
newtype AuthorizationQuotaExceededFault = AuthorizationQuotaExceededFault 
  { 
  }
derive instance newtypeAuthorizationQuotaExceededFault :: Newtype AuthorizationQuotaExceededFault _


newtype AuthorizeDBSecurityGroupIngressMessage = AuthorizeDBSecurityGroupIngressMessage 
  { "DBSecurityGroupName" :: (String)
  , "CIDRIP" :: NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined (String)
  , "EC2SecurityGroupId" :: NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined (String)
  }
derive instance newtypeAuthorizeDBSecurityGroupIngressMessage :: Newtype AuthorizeDBSecurityGroupIngressMessage _


newtype AuthorizeDBSecurityGroupIngressResult = AuthorizeDBSecurityGroupIngressResult 
  { "DBSecurityGroup" :: NullOrUndefined (DBSecurityGroup)
  }
derive instance newtypeAuthorizeDBSecurityGroupIngressResult :: Newtype AuthorizeDBSecurityGroupIngressResult _


newtype AvailabilityZone = AvailabilityZone 
  { "Name" :: NullOrUndefined (String)
  , "ProvisionedIopsCapable" :: NullOrUndefined (Boolean)
  }
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _


newtype AvailabilityZoneList = AvailabilityZoneList (Array AvailabilityZone)
derive instance newtypeAvailabilityZoneList :: Newtype AvailabilityZoneList _


newtype BooleanOptional = BooleanOptional Boolean
derive instance newtypeBooleanOptional :: Newtype BooleanOptional _


newtype CharacterSet = CharacterSet 
  { "CharacterSetName" :: NullOrUndefined (String)
  , "CharacterSetDescription" :: NullOrUndefined (String)
  }
derive instance newtypeCharacterSet :: Newtype CharacterSet _


newtype CopyDBSnapshotMessage = CopyDBSnapshotMessage 
  { "SourceDBSnapshotIdentifier" :: (String)
  , "TargetDBSnapshotIdentifier" :: (String)
  }
derive instance newtypeCopyDBSnapshotMessage :: Newtype CopyDBSnapshotMessage _


newtype CopyDBSnapshotResult = CopyDBSnapshotResult 
  { "DBSnapshot" :: NullOrUndefined (DBSnapshot)
  }
derive instance newtypeCopyDBSnapshotResult :: Newtype CopyDBSnapshotResult _


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
derive instance newtypeCreateDBInstanceMessage :: Newtype CreateDBInstanceMessage _


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
derive instance newtypeCreateDBInstanceReadReplicaMessage :: Newtype CreateDBInstanceReadReplicaMessage _


newtype CreateDBInstanceReadReplicaResult = CreateDBInstanceReadReplicaResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }
derive instance newtypeCreateDBInstanceReadReplicaResult :: Newtype CreateDBInstanceReadReplicaResult _


newtype CreateDBInstanceResult = CreateDBInstanceResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }
derive instance newtypeCreateDBInstanceResult :: Newtype CreateDBInstanceResult _


newtype CreateDBParameterGroupMessage = CreateDBParameterGroupMessage 
  { "DBParameterGroupName" :: (String)
  , "DBParameterGroupFamily" :: (String)
  , "Description" :: (String)
  }
derive instance newtypeCreateDBParameterGroupMessage :: Newtype CreateDBParameterGroupMessage _


newtype CreateDBParameterGroupResult = CreateDBParameterGroupResult 
  { "DBParameterGroup" :: NullOrUndefined (DBParameterGroup)
  }
derive instance newtypeCreateDBParameterGroupResult :: Newtype CreateDBParameterGroupResult _


newtype CreateDBSecurityGroupMessage = CreateDBSecurityGroupMessage 
  { "DBSecurityGroupName" :: (String)
  , "DBSecurityGroupDescription" :: (String)
  }
derive instance newtypeCreateDBSecurityGroupMessage :: Newtype CreateDBSecurityGroupMessage _


newtype CreateDBSecurityGroupResult = CreateDBSecurityGroupResult 
  { "DBSecurityGroup" :: NullOrUndefined (DBSecurityGroup)
  }
derive instance newtypeCreateDBSecurityGroupResult :: Newtype CreateDBSecurityGroupResult _


newtype CreateDBSnapshotMessage = CreateDBSnapshotMessage 
  { "DBSnapshotIdentifier" :: (String)
  , "DBInstanceIdentifier" :: (String)
  }
derive instance newtypeCreateDBSnapshotMessage :: Newtype CreateDBSnapshotMessage _


newtype CreateDBSnapshotResult = CreateDBSnapshotResult 
  { "DBSnapshot" :: NullOrUndefined (DBSnapshot)
  }
derive instance newtypeCreateDBSnapshotResult :: Newtype CreateDBSnapshotResult _


newtype CreateDBSubnetGroupMessage = CreateDBSubnetGroupMessage 
  { "DBSubnetGroupName" :: (String)
  , "DBSubnetGroupDescription" :: (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  }
derive instance newtypeCreateDBSubnetGroupMessage :: Newtype CreateDBSubnetGroupMessage _


newtype CreateDBSubnetGroupResult = CreateDBSubnetGroupResult 
  { "DBSubnetGroup" :: NullOrUndefined (DBSubnetGroup)
  }
derive instance newtypeCreateDBSubnetGroupResult :: Newtype CreateDBSubnetGroupResult _


newtype CreateEventSubscriptionMessage = CreateEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SnsTopicArn" :: (String)
  , "SourceType" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "SourceIds" :: NullOrUndefined (SourceIdsList)
  , "Enabled" :: NullOrUndefined (BooleanOptional)
  }
derive instance newtypeCreateEventSubscriptionMessage :: Newtype CreateEventSubscriptionMessage _


newtype CreateEventSubscriptionResult = CreateEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }
derive instance newtypeCreateEventSubscriptionResult :: Newtype CreateEventSubscriptionResult _


newtype CreateOptionGroupMessage = CreateOptionGroupMessage 
  { "OptionGroupName" :: (String)
  , "EngineName" :: (String)
  , "MajorEngineVersion" :: (String)
  , "OptionGroupDescription" :: (String)
  }
derive instance newtypeCreateOptionGroupMessage :: Newtype CreateOptionGroupMessage _


newtype CreateOptionGroupResult = CreateOptionGroupResult 
  { "OptionGroup" :: NullOrUndefined (OptionGroup)
  }
derive instance newtypeCreateOptionGroupResult :: Newtype CreateOptionGroupResult _


newtype DBEngineVersion = DBEngineVersion 
  { "Engine" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "DBParameterGroupFamily" :: NullOrUndefined (String)
  , "DBEngineDescription" :: NullOrUndefined (String)
  , "DBEngineVersionDescription" :: NullOrUndefined (String)
  , "DefaultCharacterSet" :: NullOrUndefined (CharacterSet)
  , "SupportedCharacterSets" :: NullOrUndefined (SupportedCharacterSetsList)
  }
derive instance newtypeDBEngineVersion :: Newtype DBEngineVersion _


newtype DBEngineVersionList = DBEngineVersionList (Array DBEngineVersion)
derive instance newtypeDBEngineVersionList :: Newtype DBEngineVersionList _


newtype DBEngineVersionMessage = DBEngineVersionMessage 
  { "Marker" :: NullOrUndefined (String)
  , "DBEngineVersions" :: NullOrUndefined (DBEngineVersionList)
  }
derive instance newtypeDBEngineVersionMessage :: Newtype DBEngineVersionMessage _


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
derive instance newtypeDBInstance :: Newtype DBInstance _


-- | <p>User already has a DB instance with the given identifier.</p>
newtype DBInstanceAlreadyExistsFault = DBInstanceAlreadyExistsFault 
  { 
  }
derive instance newtypeDBInstanceAlreadyExistsFault :: Newtype DBInstanceAlreadyExistsFault _


newtype DBInstanceList = DBInstanceList (Array DBInstance)
derive instance newtypeDBInstanceList :: Newtype DBInstanceList _


newtype DBInstanceMessage = DBInstanceMessage 
  { "Marker" :: NullOrUndefined (String)
  , "DBInstances" :: NullOrUndefined (DBInstanceList)
  }
derive instance newtypeDBInstanceMessage :: Newtype DBInstanceMessage _


-- | <p> <i>DBInstanceIdentifier</i> does not refer to an existing DB instance. </p>
newtype DBInstanceNotFoundFault = DBInstanceNotFoundFault 
  { 
  }
derive instance newtypeDBInstanceNotFoundFault :: Newtype DBInstanceNotFoundFault _


newtype DBParameterGroup = DBParameterGroup 
  { "DBParameterGroupName" :: NullOrUndefined (String)
  , "DBParameterGroupFamily" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  }
derive instance newtypeDBParameterGroup :: Newtype DBParameterGroup _


-- | <p>A DB parameter group with the same name exists.</p>
newtype DBParameterGroupAlreadyExistsFault = DBParameterGroupAlreadyExistsFault 
  { 
  }
derive instance newtypeDBParameterGroupAlreadyExistsFault :: Newtype DBParameterGroupAlreadyExistsFault _


newtype DBParameterGroupDetails = DBParameterGroupDetails 
  { "Parameters" :: NullOrUndefined (ParametersList)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDBParameterGroupDetails :: Newtype DBParameterGroupDetails _


newtype DBParameterGroupList = DBParameterGroupList (Array DBParameterGroup)
derive instance newtypeDBParameterGroupList :: Newtype DBParameterGroupList _


newtype DBParameterGroupNameMessage = DBParameterGroupNameMessage 
  { "DBParameterGroupName" :: NullOrUndefined (String)
  }
derive instance newtypeDBParameterGroupNameMessage :: Newtype DBParameterGroupNameMessage _


-- | <p> <i>DBParameterGroupName</i> does not refer to an existing DB parameter group. </p>
newtype DBParameterGroupNotFoundFault = DBParameterGroupNotFoundFault 
  { 
  }
derive instance newtypeDBParameterGroupNotFoundFault :: Newtype DBParameterGroupNotFoundFault _


-- | <p>Request would result in user exceeding the allowed number of DB parameter groups.</p>
newtype DBParameterGroupQuotaExceededFault = DBParameterGroupQuotaExceededFault 
  { 
  }
derive instance newtypeDBParameterGroupQuotaExceededFault :: Newtype DBParameterGroupQuotaExceededFault _


newtype DBParameterGroupStatus = DBParameterGroupStatus 
  { "DBParameterGroupName" :: NullOrUndefined (String)
  , "ParameterApplyStatus" :: NullOrUndefined (String)
  }
derive instance newtypeDBParameterGroupStatus :: Newtype DBParameterGroupStatus _


newtype DBParameterGroupStatusList = DBParameterGroupStatusList (Array DBParameterGroupStatus)
derive instance newtypeDBParameterGroupStatusList :: Newtype DBParameterGroupStatusList _


newtype DBParameterGroupsMessage = DBParameterGroupsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "DBParameterGroups" :: NullOrUndefined (DBParameterGroupList)
  }
derive instance newtypeDBParameterGroupsMessage :: Newtype DBParameterGroupsMessage _


newtype DBSecurityGroup = DBSecurityGroup 
  { "OwnerId" :: NullOrUndefined (String)
  , "DBSecurityGroupName" :: NullOrUndefined (String)
  , "DBSecurityGroupDescription" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "EC2SecurityGroups" :: NullOrUndefined (EC2SecurityGroupList)
  , "IPRanges" :: NullOrUndefined (IPRangeList)
  }
derive instance newtypeDBSecurityGroup :: Newtype DBSecurityGroup _


-- | <p> A DB security group with the name specified in <i>DBSecurityGroupName</i> already exists. </p>
newtype DBSecurityGroupAlreadyExistsFault = DBSecurityGroupAlreadyExistsFault 
  { 
  }
derive instance newtypeDBSecurityGroupAlreadyExistsFault :: Newtype DBSecurityGroupAlreadyExistsFault _


newtype DBSecurityGroupMembership = DBSecurityGroupMembership 
  { "DBSecurityGroupName" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }
derive instance newtypeDBSecurityGroupMembership :: Newtype DBSecurityGroupMembership _


newtype DBSecurityGroupMembershipList = DBSecurityGroupMembershipList (Array DBSecurityGroupMembership)
derive instance newtypeDBSecurityGroupMembershipList :: Newtype DBSecurityGroupMembershipList _


newtype DBSecurityGroupMessage = DBSecurityGroupMessage 
  { "Marker" :: NullOrUndefined (String)
  , "DBSecurityGroups" :: NullOrUndefined (DBSecurityGroups)
  }
derive instance newtypeDBSecurityGroupMessage :: Newtype DBSecurityGroupMessage _


newtype DBSecurityGroupNameList = DBSecurityGroupNameList (Array String)
derive instance newtypeDBSecurityGroupNameList :: Newtype DBSecurityGroupNameList _


-- | <p> <i>DBSecurityGroupName</i> does not refer to an existing DB security group. </p>
newtype DBSecurityGroupNotFoundFault = DBSecurityGroupNotFoundFault 
  { 
  }
derive instance newtypeDBSecurityGroupNotFoundFault :: Newtype DBSecurityGroupNotFoundFault _


-- | <p>A DB security group is not allowed for this action.</p>
newtype DBSecurityGroupNotSupportedFault = DBSecurityGroupNotSupportedFault 
  { 
  }
derive instance newtypeDBSecurityGroupNotSupportedFault :: Newtype DBSecurityGroupNotSupportedFault _


-- | <p>Request would result in user exceeding the allowed number of DB security groups.</p>
newtype DBSecurityGroupQuotaExceededFault = DBSecurityGroupQuotaExceededFault 
  { 
  }
derive instance newtypeDBSecurityGroupQuotaExceededFault :: Newtype DBSecurityGroupQuotaExceededFault _


newtype DBSecurityGroups = DBSecurityGroups (Array DBSecurityGroup)
derive instance newtypeDBSecurityGroups :: Newtype DBSecurityGroups _


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
derive instance newtypeDBSnapshot :: Newtype DBSnapshot _


-- | <p> <i>DBSnapshotIdentifier</i> is already used by an existing snapshot. </p>
newtype DBSnapshotAlreadyExistsFault = DBSnapshotAlreadyExistsFault 
  { 
  }
derive instance newtypeDBSnapshotAlreadyExistsFault :: Newtype DBSnapshotAlreadyExistsFault _


newtype DBSnapshotList = DBSnapshotList (Array DBSnapshot)
derive instance newtypeDBSnapshotList :: Newtype DBSnapshotList _


newtype DBSnapshotMessage = DBSnapshotMessage 
  { "Marker" :: NullOrUndefined (String)
  , "DBSnapshots" :: NullOrUndefined (DBSnapshotList)
  }
derive instance newtypeDBSnapshotMessage :: Newtype DBSnapshotMessage _


-- | <p> <i>DBSnapshotIdentifier</i> does not refer to an existing DB snapshot. </p>
newtype DBSnapshotNotFoundFault = DBSnapshotNotFoundFault 
  { 
  }
derive instance newtypeDBSnapshotNotFoundFault :: Newtype DBSnapshotNotFoundFault _


newtype DBSubnetGroup = DBSubnetGroup 
  { "DBSubnetGroupName" :: NullOrUndefined (String)
  , "DBSubnetGroupDescription" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "SubnetGroupStatus" :: NullOrUndefined (String)
  , "Subnets" :: NullOrUndefined (SubnetList)
  }
derive instance newtypeDBSubnetGroup :: Newtype DBSubnetGroup _


-- | <p> <i>DBSubnetGroupName</i> is already used by an existing DB subnet group. </p>
newtype DBSubnetGroupAlreadyExistsFault = DBSubnetGroupAlreadyExistsFault 
  { 
  }
derive instance newtypeDBSubnetGroupAlreadyExistsFault :: Newtype DBSubnetGroupAlreadyExistsFault _


-- | <p>Subnets in the DB subnet group should cover at least two Availability Zones unless there is only one Availability Zone.</p>
newtype DBSubnetGroupDoesNotCoverEnoughAZs = DBSubnetGroupDoesNotCoverEnoughAZs 
  { 
  }
derive instance newtypeDBSubnetGroupDoesNotCoverEnoughAZs :: Newtype DBSubnetGroupDoesNotCoverEnoughAZs _


newtype DBSubnetGroupMessage = DBSubnetGroupMessage 
  { "Marker" :: NullOrUndefined (String)
  , "DBSubnetGroups" :: NullOrUndefined (DBSubnetGroups)
  }
derive instance newtypeDBSubnetGroupMessage :: Newtype DBSubnetGroupMessage _


-- | <p> <i>DBSubnetGroupName</i> does not refer to an existing DB subnet group. </p>
newtype DBSubnetGroupNotFoundFault = DBSubnetGroupNotFoundFault 
  { 
  }
derive instance newtypeDBSubnetGroupNotFoundFault :: Newtype DBSubnetGroupNotFoundFault _


-- | <p>Request would result in user exceeding the allowed number of DB subnet groups.</p>
newtype DBSubnetGroupQuotaExceededFault = DBSubnetGroupQuotaExceededFault 
  { 
  }
derive instance newtypeDBSubnetGroupQuotaExceededFault :: Newtype DBSubnetGroupQuotaExceededFault _


newtype DBSubnetGroups = DBSubnetGroups (Array DBSubnetGroup)
derive instance newtypeDBSubnetGroups :: Newtype DBSubnetGroups _


-- | <p>Request would result in user exceeding the allowed number of subnets in a DB subnet groups.</p>
newtype DBSubnetQuotaExceededFault = DBSubnetQuotaExceededFault 
  { 
  }
derive instance newtypeDBSubnetQuotaExceededFault :: Newtype DBSubnetQuotaExceededFault _


-- | <p>The DB upgrade failed because a resource the DB depends on could not be modified.</p>
newtype DBUpgradeDependencyFailureFault = DBUpgradeDependencyFailureFault 
  { 
  }
derive instance newtypeDBUpgradeDependencyFailureFault :: Newtype DBUpgradeDependencyFailureFault _


newtype DeleteDBInstanceMessage = DeleteDBInstanceMessage 
  { "DBInstanceIdentifier" :: (String)
  , "SkipFinalSnapshot" :: NullOrUndefined (Boolean)
  , "FinalDBSnapshotIdentifier" :: NullOrUndefined (String)
  }
derive instance newtypeDeleteDBInstanceMessage :: Newtype DeleteDBInstanceMessage _


newtype DeleteDBInstanceResult = DeleteDBInstanceResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }
derive instance newtypeDeleteDBInstanceResult :: Newtype DeleteDBInstanceResult _


newtype DeleteDBParameterGroupMessage = DeleteDBParameterGroupMessage 
  { "DBParameterGroupName" :: (String)
  }
derive instance newtypeDeleteDBParameterGroupMessage :: Newtype DeleteDBParameterGroupMessage _


newtype DeleteDBSecurityGroupMessage = DeleteDBSecurityGroupMessage 
  { "DBSecurityGroupName" :: (String)
  }
derive instance newtypeDeleteDBSecurityGroupMessage :: Newtype DeleteDBSecurityGroupMessage _


newtype DeleteDBSnapshotMessage = DeleteDBSnapshotMessage 
  { "DBSnapshotIdentifier" :: (String)
  }
derive instance newtypeDeleteDBSnapshotMessage :: Newtype DeleteDBSnapshotMessage _


newtype DeleteDBSnapshotResult = DeleteDBSnapshotResult 
  { "DBSnapshot" :: NullOrUndefined (DBSnapshot)
  }
derive instance newtypeDeleteDBSnapshotResult :: Newtype DeleteDBSnapshotResult _


newtype DeleteDBSubnetGroupMessage = DeleteDBSubnetGroupMessage 
  { "DBSubnetGroupName" :: (String)
  }
derive instance newtypeDeleteDBSubnetGroupMessage :: Newtype DeleteDBSubnetGroupMessage _


newtype DeleteEventSubscriptionMessage = DeleteEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  }
derive instance newtypeDeleteEventSubscriptionMessage :: Newtype DeleteEventSubscriptionMessage _


newtype DeleteEventSubscriptionResult = DeleteEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }
derive instance newtypeDeleteEventSubscriptionResult :: Newtype DeleteEventSubscriptionResult _


newtype DeleteOptionGroupMessage = DeleteOptionGroupMessage 
  { "OptionGroupName" :: (String)
  }
derive instance newtypeDeleteOptionGroupMessage :: Newtype DeleteOptionGroupMessage _


newtype DescribeDBEngineVersionsMessage = DescribeDBEngineVersionsMessage 
  { "Engine" :: NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "DBParameterGroupFamily" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  , "DefaultOnly" :: NullOrUndefined (Boolean)
  , "ListSupportedCharacterSets" :: NullOrUndefined (BooleanOptional)
  }
derive instance newtypeDescribeDBEngineVersionsMessage :: Newtype DescribeDBEngineVersionsMessage _


newtype DescribeDBInstancesMessage = DescribeDBInstancesMessage 
  { "DBInstanceIdentifier" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeDBInstancesMessage :: Newtype DescribeDBInstancesMessage _


newtype DescribeDBParameterGroupsMessage = DescribeDBParameterGroupsMessage 
  { "DBParameterGroupName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeDBParameterGroupsMessage :: Newtype DescribeDBParameterGroupsMessage _


newtype DescribeDBParametersMessage = DescribeDBParametersMessage 
  { "DBParameterGroupName" :: (String)
  , "Source" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeDBParametersMessage :: Newtype DescribeDBParametersMessage _


newtype DescribeDBSecurityGroupsMessage = DescribeDBSecurityGroupsMessage 
  { "DBSecurityGroupName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeDBSecurityGroupsMessage :: Newtype DescribeDBSecurityGroupsMessage _


newtype DescribeDBSnapshotsMessage = DescribeDBSnapshotsMessage 
  { "DBInstanceIdentifier" :: NullOrUndefined (String)
  , "DBSnapshotIdentifier" :: NullOrUndefined (String)
  , "SnapshotType" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeDBSnapshotsMessage :: Newtype DescribeDBSnapshotsMessage _


newtype DescribeDBSubnetGroupsMessage = DescribeDBSubnetGroupsMessage 
  { "DBSubnetGroupName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeDBSubnetGroupsMessage :: Newtype DescribeDBSubnetGroupsMessage _


newtype DescribeEngineDefaultParametersMessage = DescribeEngineDefaultParametersMessage 
  { "DBParameterGroupFamily" :: (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEngineDefaultParametersMessage :: Newtype DescribeEngineDefaultParametersMessage _


newtype DescribeEngineDefaultParametersResult = DescribeEngineDefaultParametersResult 
  { "EngineDefaults" :: NullOrUndefined (EngineDefaults)
  }
derive instance newtypeDescribeEngineDefaultParametersResult :: Newtype DescribeEngineDefaultParametersResult _


newtype DescribeEventCategoriesMessage = DescribeEventCategoriesMessage 
  { "SourceType" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEventCategoriesMessage :: Newtype DescribeEventCategoriesMessage _


newtype DescribeEventSubscriptionsMessage = DescribeEventSubscriptionsMessage 
  { "SubscriptionName" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEventSubscriptionsMessage :: Newtype DescribeEventSubscriptionsMessage _


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
derive instance newtypeDescribeEventsMessage :: Newtype DescribeEventsMessage _


newtype DescribeOptionGroupOptionsMessage = DescribeOptionGroupOptionsMessage 
  { "EngineName" :: (String)
  , "MajorEngineVersion" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeOptionGroupOptionsMessage :: Newtype DescribeOptionGroupOptionsMessage _


newtype DescribeOptionGroupsMessage = DescribeOptionGroupsMessage 
  { "OptionGroupName" :: NullOrUndefined (String)
  , "Marker" :: NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "EngineName" :: NullOrUndefined (String)
  , "MajorEngineVersion" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeOptionGroupsMessage :: Newtype DescribeOptionGroupsMessage _


newtype DescribeOrderableDBInstanceOptionsMessage = DescribeOrderableDBInstanceOptionsMessage 
  { "Engine" :: (String)
  , "EngineVersion" :: NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined (String)
  , "LicenseModel" :: NullOrUndefined (String)
  , "Vpc" :: NullOrUndefined (BooleanOptional)
  , "MaxRecords" :: NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeOrderableDBInstanceOptionsMessage :: Newtype DescribeOrderableDBInstanceOptionsMessage _


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
derive instance newtypeDescribeReservedDBInstancesMessage :: Newtype DescribeReservedDBInstancesMessage _


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
derive instance newtypeDescribeReservedDBInstancesOfferingsMessage :: Newtype DescribeReservedDBInstancesOfferingsMessage _


newtype EC2SecurityGroup = EC2SecurityGroup 
  { "Status" :: NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined (String)
  , "EC2SecurityGroupId" :: NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined (String)
  }
derive instance newtypeEC2SecurityGroup :: Newtype EC2SecurityGroup _


newtype EC2SecurityGroupList = EC2SecurityGroupList (Array EC2SecurityGroup)
derive instance newtypeEC2SecurityGroupList :: Newtype EC2SecurityGroupList _


newtype Endpoint = Endpoint 
  { "Address" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (Int)
  }
derive instance newtypeEndpoint :: Newtype Endpoint _


newtype EngineDefaults = EngineDefaults 
  { "DBParameterGroupFamily" :: NullOrUndefined (String)
  , "Marker" :: NullOrUndefined (String)
  , "Parameters" :: NullOrUndefined (ParametersList)
  }
derive instance newtypeEngineDefaults :: Newtype EngineDefaults _


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


newtype EventCategoriesMap = EventCategoriesMap 
  { "SourceType" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  }
derive instance newtypeEventCategoriesMap :: Newtype EventCategoriesMap _


newtype EventCategoriesMapList = EventCategoriesMapList (Array EventCategoriesMap)
derive instance newtypeEventCategoriesMapList :: Newtype EventCategoriesMapList _


newtype EventCategoriesMessage = EventCategoriesMessage 
  { "EventCategoriesMapList" :: NullOrUndefined (EventCategoriesMapList)
  }
derive instance newtypeEventCategoriesMessage :: Newtype EventCategoriesMessage _


newtype EventList = EventList (Array Event)
derive instance newtypeEventList :: Newtype EventList _


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
derive instance newtypeEventSubscription :: Newtype EventSubscription _


-- | <p>You have reached the maximum number of event subscriptions.</p>
newtype EventSubscriptionQuotaExceededFault = EventSubscriptionQuotaExceededFault 
  { 
  }
derive instance newtypeEventSubscriptionQuotaExceededFault :: Newtype EventSubscriptionQuotaExceededFault _


newtype EventSubscriptionsList = EventSubscriptionsList (Array EventSubscription)
derive instance newtypeEventSubscriptionsList :: Newtype EventSubscriptionsList _


newtype EventSubscriptionsMessage = EventSubscriptionsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "EventSubscriptionsList" :: NullOrUndefined (EventSubscriptionsList)
  }
derive instance newtypeEventSubscriptionsMessage :: Newtype EventSubscriptionsMessage _


newtype EventsMessage = EventsMessage 
  { "Marker" :: NullOrUndefined (String)
  , "Events" :: NullOrUndefined (EventList)
  }
derive instance newtypeEventsMessage :: Newtype EventsMessage _


newtype IPRange = IPRange 
  { "Status" :: NullOrUndefined (String)
  , "CIDRIP" :: NullOrUndefined (String)
  }
derive instance newtypeIPRange :: Newtype IPRange _


newtype IPRangeList = IPRangeList (Array IPRange)
derive instance newtypeIPRangeList :: Newtype IPRangeList _


-- | <p>Request would result in user exceeding the allowed number of DB instances.</p>
newtype InstanceQuotaExceededFault = InstanceQuotaExceededFault 
  { 
  }
derive instance newtypeInstanceQuotaExceededFault :: Newtype InstanceQuotaExceededFault _


-- | <p>Specified DB instance class is not available in the specified Availability Zone.</p>
newtype InsufficientDBInstanceCapacityFault = InsufficientDBInstanceCapacityFault 
  { 
  }
derive instance newtypeInsufficientDBInstanceCapacityFault :: Newtype InsufficientDBInstanceCapacityFault _


newtype IntegerOptional = IntegerOptional Int
derive instance newtypeIntegerOptional :: Newtype IntegerOptional _


-- | <p> The specified DB instance is not in the <i>available</i> state. </p>
newtype InvalidDBInstanceStateFault = InvalidDBInstanceStateFault 
  { 
  }
derive instance newtypeInvalidDBInstanceStateFault :: Newtype InvalidDBInstanceStateFault _


-- | <p>The DB parameter group is in use or is in an invalid state. If you are attempting to delete the parameter group, you cannot delete it when the parameter group is in this state.</p>
newtype InvalidDBParameterGroupStateFault = InvalidDBParameterGroupStateFault 
  { 
  }
derive instance newtypeInvalidDBParameterGroupStateFault :: Newtype InvalidDBParameterGroupStateFault _


-- | <p>The state of the DB security group does not allow deletion.</p>
newtype InvalidDBSecurityGroupStateFault = InvalidDBSecurityGroupStateFault 
  { 
  }
derive instance newtypeInvalidDBSecurityGroupStateFault :: Newtype InvalidDBSecurityGroupStateFault _


-- | <p>The state of the DB snapshot does not allow deletion.</p>
newtype InvalidDBSnapshotStateFault = InvalidDBSnapshotStateFault 
  { 
  }
derive instance newtypeInvalidDBSnapshotStateFault :: Newtype InvalidDBSnapshotStateFault _


-- | <p>The DB subnet group cannot be deleted because it is in use.</p>
newtype InvalidDBSubnetGroupStateFault = InvalidDBSubnetGroupStateFault 
  { 
  }
derive instance newtypeInvalidDBSubnetGroupStateFault :: Newtype InvalidDBSubnetGroupStateFault _


-- | <p> The DB subnet is not in the <i>available</i> state. </p>
newtype InvalidDBSubnetStateFault = InvalidDBSubnetStateFault 
  { 
  }
derive instance newtypeInvalidDBSubnetStateFault :: Newtype InvalidDBSubnetStateFault _


-- | <p>This error can occur if someone else is modifying a subscription. You should retry the action.</p>
newtype InvalidEventSubscriptionStateFault = InvalidEventSubscriptionStateFault 
  { 
  }
derive instance newtypeInvalidEventSubscriptionStateFault :: Newtype InvalidEventSubscriptionStateFault _


-- | <p> The option group is not in the <i>available</i> state. </p>
newtype InvalidOptionGroupStateFault = InvalidOptionGroupStateFault 
  { 
  }
derive instance newtypeInvalidOptionGroupStateFault :: Newtype InvalidOptionGroupStateFault _


-- | <p>Cannot restore from vpc backup to non-vpc DB instance.</p>
newtype InvalidRestoreFault = InvalidRestoreFault 
  { 
  }
derive instance newtypeInvalidRestoreFault :: Newtype InvalidRestoreFault _


-- | <p>The requested subnet is invalid, or multiple subnets were requested that are not all in a common VPC.</p>
newtype InvalidSubnet = InvalidSubnet 
  { 
  }
derive instance newtypeInvalidSubnet :: Newtype InvalidSubnet _


-- | <p>DB subnet group does not cover all Availability Zones after it is created because users' change.</p>
newtype InvalidVPCNetworkStateFault = InvalidVPCNetworkStateFault 
  { 
  }
derive instance newtypeInvalidVPCNetworkStateFault :: Newtype InvalidVPCNetworkStateFault _


newtype KeyList = KeyList (Array String)
derive instance newtypeKeyList :: Newtype KeyList _


newtype ListTagsForResourceMessage = ListTagsForResourceMessage 
  { "ResourceName" :: (String)
  }
derive instance newtypeListTagsForResourceMessage :: Newtype ListTagsForResourceMessage _


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
derive instance newtypeModifyDBInstanceMessage :: Newtype ModifyDBInstanceMessage _


newtype ModifyDBInstanceResult = ModifyDBInstanceResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }
derive instance newtypeModifyDBInstanceResult :: Newtype ModifyDBInstanceResult _


newtype ModifyDBParameterGroupMessage = ModifyDBParameterGroupMessage 
  { "DBParameterGroupName" :: (String)
  , "Parameters" :: (ParametersList)
  }
derive instance newtypeModifyDBParameterGroupMessage :: Newtype ModifyDBParameterGroupMessage _


newtype ModifyDBSubnetGroupMessage = ModifyDBSubnetGroupMessage 
  { "DBSubnetGroupName" :: (String)
  , "DBSubnetGroupDescription" :: NullOrUndefined (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  }
derive instance newtypeModifyDBSubnetGroupMessage :: Newtype ModifyDBSubnetGroupMessage _


newtype ModifyDBSubnetGroupResult = ModifyDBSubnetGroupResult 
  { "DBSubnetGroup" :: NullOrUndefined (DBSubnetGroup)
  }
derive instance newtypeModifyDBSubnetGroupResult :: Newtype ModifyDBSubnetGroupResult _


newtype ModifyEventSubscriptionMessage = ModifyEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SnsTopicArn" :: NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined (EventCategoriesList)
  , "Enabled" :: NullOrUndefined (BooleanOptional)
  }
derive instance newtypeModifyEventSubscriptionMessage :: Newtype ModifyEventSubscriptionMessage _


newtype ModifyEventSubscriptionResult = ModifyEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }
derive instance newtypeModifyEventSubscriptionResult :: Newtype ModifyEventSubscriptionResult _


newtype ModifyOptionGroupMessage = ModifyOptionGroupMessage 
  { "OptionGroupName" :: (String)
  , "OptionsToInclude" :: NullOrUndefined (OptionConfigurationList)
  , "OptionsToRemove" :: NullOrUndefined (OptionNamesList)
  , "ApplyImmediately" :: NullOrUndefined (Boolean)
  }
derive instance newtypeModifyOptionGroupMessage :: Newtype ModifyOptionGroupMessage _


newtype ModifyOptionGroupResult = ModifyOptionGroupResult 
  { "OptionGroup" :: NullOrUndefined (OptionGroup)
  }
derive instance newtypeModifyOptionGroupResult :: Newtype ModifyOptionGroupResult _


newtype Option = Option 
  { "OptionName" :: NullOrUndefined (String)
  , "OptionDescription" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "DBSecurityGroupMemberships" :: NullOrUndefined (DBSecurityGroupMembershipList)
  , "VpcSecurityGroupMemberships" :: NullOrUndefined (VpcSecurityGroupMembershipList)
  }
derive instance newtypeOption :: Newtype Option _


newtype OptionConfiguration = OptionConfiguration 
  { "OptionName" :: (String)
  , "Port" :: NullOrUndefined (IntegerOptional)
  , "DBSecurityGroupMemberships" :: NullOrUndefined (DBSecurityGroupNameList)
  , "VpcSecurityGroupMemberships" :: NullOrUndefined (VpcSecurityGroupIdList)
  }
derive instance newtypeOptionConfiguration :: Newtype OptionConfiguration _


newtype OptionConfigurationList = OptionConfigurationList (Array OptionConfiguration)
derive instance newtypeOptionConfigurationList :: Newtype OptionConfigurationList _


newtype OptionGroup = OptionGroup 
  { "OptionGroupName" :: NullOrUndefined (String)
  , "OptionGroupDescription" :: NullOrUndefined (String)
  , "EngineName" :: NullOrUndefined (String)
  , "MajorEngineVersion" :: NullOrUndefined (String)
  , "Options" :: NullOrUndefined (OptionsList)
  , "AllowsVpcAndNonVpcInstanceMemberships" :: NullOrUndefined (Boolean)
  , "VpcId" :: NullOrUndefined (String)
  }
derive instance newtypeOptionGroup :: Newtype OptionGroup _


-- | <p>The option group you are trying to create already exists.</p>
newtype OptionGroupAlreadyExistsFault = OptionGroupAlreadyExistsFault 
  { 
  }
derive instance newtypeOptionGroupAlreadyExistsFault :: Newtype OptionGroupAlreadyExistsFault _


newtype OptionGroupMembership = OptionGroupMembership 
  { "OptionGroupName" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }
derive instance newtypeOptionGroupMembership :: Newtype OptionGroupMembership _


-- | <p>The specified option group could not be found.</p>
newtype OptionGroupNotFoundFault = OptionGroupNotFoundFault 
  { 
  }
derive instance newtypeOptionGroupNotFoundFault :: Newtype OptionGroupNotFoundFault _


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
derive instance newtypeOptionGroupOption :: Newtype OptionGroupOption _


newtype OptionGroupOptionsList = OptionGroupOptionsList (Array OptionGroupOption)
derive instance newtypeOptionGroupOptionsList :: Newtype OptionGroupOptionsList _


newtype OptionGroupOptionsMessage = OptionGroupOptionsMessage 
  { "OptionGroupOptions" :: NullOrUndefined (OptionGroupOptionsList)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeOptionGroupOptionsMessage :: Newtype OptionGroupOptionsMessage _


-- | <p>The quota of 20 option groups was exceeded for this AWS account.</p>
newtype OptionGroupQuotaExceededFault = OptionGroupQuotaExceededFault 
  { 
  }
derive instance newtypeOptionGroupQuotaExceededFault :: Newtype OptionGroupQuotaExceededFault _


newtype OptionGroups = OptionGroups 
  { "OptionGroupsList" :: NullOrUndefined (OptionGroupsList)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeOptionGroups :: Newtype OptionGroups _


newtype OptionGroupsList = OptionGroupsList (Array OptionGroup)
derive instance newtypeOptionGroupsList :: Newtype OptionGroupsList _


newtype OptionNamesList = OptionNamesList (Array String)
derive instance newtypeOptionNamesList :: Newtype OptionNamesList _


newtype OptionsDependedOn = OptionsDependedOn (Array String)
derive instance newtypeOptionsDependedOn :: Newtype OptionsDependedOn _


newtype OptionsList = OptionsList (Array Option)
derive instance newtypeOptionsList :: Newtype OptionsList _


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
derive instance newtypeOrderableDBInstanceOption :: Newtype OrderableDBInstanceOption _


newtype OrderableDBInstanceOptionsList = OrderableDBInstanceOptionsList (Array OrderableDBInstanceOption)
derive instance newtypeOrderableDBInstanceOptionsList :: Newtype OrderableDBInstanceOptionsList _


newtype OrderableDBInstanceOptionsMessage = OrderableDBInstanceOptionsMessage 
  { "OrderableDBInstanceOptions" :: NullOrUndefined (OrderableDBInstanceOptionsList)
  , "Marker" :: NullOrUndefined (String)
  }
derive instance newtypeOrderableDBInstanceOptionsMessage :: Newtype OrderableDBInstanceOptionsMessage _


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
derive instance newtypeParameter :: Newtype Parameter _


newtype ParametersList = ParametersList (Array Parameter)
derive instance newtypeParametersList :: Newtype ParametersList _


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
derive instance newtypePendingModifiedValues :: Newtype PendingModifiedValues _


-- | <p> <i>SourceDBInstanceIdentifier</i> refers to a DB instance with <i>BackupRetentionPeriod</i> equal to 0. </p>
newtype PointInTimeRestoreNotEnabledFault = PointInTimeRestoreNotEnabledFault 
  { 
  }
derive instance newtypePointInTimeRestoreNotEnabledFault :: Newtype PointInTimeRestoreNotEnabledFault _


newtype PromoteReadReplicaMessage = PromoteReadReplicaMessage 
  { "DBInstanceIdentifier" :: (String)
  , "BackupRetentionPeriod" :: NullOrUndefined (IntegerOptional)
  , "PreferredBackupWindow" :: NullOrUndefined (String)
  }
derive instance newtypePromoteReadReplicaMessage :: Newtype PromoteReadReplicaMessage _


newtype PromoteReadReplicaResult = PromoteReadReplicaResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }
derive instance newtypePromoteReadReplicaResult :: Newtype PromoteReadReplicaResult _


-- | <p>Provisioned IOPS not available in the specified Availability Zone.</p>
newtype ProvisionedIopsNotAvailableInAZFault = ProvisionedIopsNotAvailableInAZFault 
  { 
  }
derive instance newtypeProvisionedIopsNotAvailableInAZFault :: Newtype ProvisionedIopsNotAvailableInAZFault _


newtype PurchaseReservedDBInstancesOfferingMessage = PurchaseReservedDBInstancesOfferingMessage 
  { "ReservedDBInstancesOfferingId" :: (String)
  , "ReservedDBInstanceId" :: NullOrUndefined (String)
  , "DBInstanceCount" :: NullOrUndefined (IntegerOptional)
  }
derive instance newtypePurchaseReservedDBInstancesOfferingMessage :: Newtype PurchaseReservedDBInstancesOfferingMessage _


newtype PurchaseReservedDBInstancesOfferingResult = PurchaseReservedDBInstancesOfferingResult 
  { "ReservedDBInstance" :: NullOrUndefined (ReservedDBInstance)
  }
derive instance newtypePurchaseReservedDBInstancesOfferingResult :: Newtype PurchaseReservedDBInstancesOfferingResult _


newtype ReadReplicaDBInstanceIdentifierList = ReadReplicaDBInstanceIdentifierList (Array String)
derive instance newtypeReadReplicaDBInstanceIdentifierList :: Newtype ReadReplicaDBInstanceIdentifierList _


newtype RebootDBInstanceMessage = RebootDBInstanceMessage 
  { "DBInstanceIdentifier" :: (String)
  , "ForceFailover" :: NullOrUndefined (BooleanOptional)
  }
derive instance newtypeRebootDBInstanceMessage :: Newtype RebootDBInstanceMessage _


newtype RebootDBInstanceResult = RebootDBInstanceResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }
derive instance newtypeRebootDBInstanceResult :: Newtype RebootDBInstanceResult _


newtype RecurringCharge = RecurringCharge 
  { "RecurringChargeAmount" :: NullOrUndefined (Number)
  , "RecurringChargeFrequency" :: NullOrUndefined (String)
  }
derive instance newtypeRecurringCharge :: Newtype RecurringCharge _


newtype RecurringChargeList = RecurringChargeList (Array RecurringCharge)
derive instance newtypeRecurringChargeList :: Newtype RecurringChargeList _


newtype RemoveSourceIdentifierFromSubscriptionMessage = RemoveSourceIdentifierFromSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SourceIdentifier" :: (String)
  }
derive instance newtypeRemoveSourceIdentifierFromSubscriptionMessage :: Newtype RemoveSourceIdentifierFromSubscriptionMessage _


newtype RemoveSourceIdentifierFromSubscriptionResult = RemoveSourceIdentifierFromSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined (EventSubscription)
  }
derive instance newtypeRemoveSourceIdentifierFromSubscriptionResult :: Newtype RemoveSourceIdentifierFromSubscriptionResult _


newtype RemoveTagsFromResourceMessage = RemoveTagsFromResourceMessage 
  { "ResourceName" :: (String)
  , "TagKeys" :: (KeyList)
  }
derive instance newtypeRemoveTagsFromResourceMessage :: Newtype RemoveTagsFromResourceMessage _


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
derive instance newtypeReservedDBInstance :: Newtype ReservedDBInstance _


-- | <p>User already has a reservation with the given identifier.</p>
newtype ReservedDBInstanceAlreadyExistsFault = ReservedDBInstanceAlreadyExistsFault 
  { 
  }
derive instance newtypeReservedDBInstanceAlreadyExistsFault :: Newtype ReservedDBInstanceAlreadyExistsFault _


newtype ReservedDBInstanceList = ReservedDBInstanceList (Array ReservedDBInstance)
derive instance newtypeReservedDBInstanceList :: Newtype ReservedDBInstanceList _


newtype ReservedDBInstanceMessage = ReservedDBInstanceMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ReservedDBInstances" :: NullOrUndefined (ReservedDBInstanceList)
  }
derive instance newtypeReservedDBInstanceMessage :: Newtype ReservedDBInstanceMessage _


-- | <p>The specified reserved DB Instance not found.</p>
newtype ReservedDBInstanceNotFoundFault = ReservedDBInstanceNotFoundFault 
  { 
  }
derive instance newtypeReservedDBInstanceNotFoundFault :: Newtype ReservedDBInstanceNotFoundFault _


-- | <p>Request would exceed the user's DB Instance quota.</p>
newtype ReservedDBInstanceQuotaExceededFault = ReservedDBInstanceQuotaExceededFault 
  { 
  }
derive instance newtypeReservedDBInstanceQuotaExceededFault :: Newtype ReservedDBInstanceQuotaExceededFault _


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
derive instance newtypeReservedDBInstancesOffering :: Newtype ReservedDBInstancesOffering _


newtype ReservedDBInstancesOfferingList = ReservedDBInstancesOfferingList (Array ReservedDBInstancesOffering)
derive instance newtypeReservedDBInstancesOfferingList :: Newtype ReservedDBInstancesOfferingList _


newtype ReservedDBInstancesOfferingMessage = ReservedDBInstancesOfferingMessage 
  { "Marker" :: NullOrUndefined (String)
  , "ReservedDBInstancesOfferings" :: NullOrUndefined (ReservedDBInstancesOfferingList)
  }
derive instance newtypeReservedDBInstancesOfferingMessage :: Newtype ReservedDBInstancesOfferingMessage _


-- | <p>Specified offering does not exist.</p>
newtype ReservedDBInstancesOfferingNotFoundFault = ReservedDBInstancesOfferingNotFoundFault 
  { 
  }
derive instance newtypeReservedDBInstancesOfferingNotFoundFault :: Newtype ReservedDBInstancesOfferingNotFoundFault _


newtype ResetDBParameterGroupMessage = ResetDBParameterGroupMessage 
  { "DBParameterGroupName" :: (String)
  , "ResetAllParameters" :: NullOrUndefined (Boolean)
  , "Parameters" :: NullOrUndefined (ParametersList)
  }
derive instance newtypeResetDBParameterGroupMessage :: Newtype ResetDBParameterGroupMessage _


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
derive instance newtypeRestoreDBInstanceFromDBSnapshotMessage :: Newtype RestoreDBInstanceFromDBSnapshotMessage _


newtype RestoreDBInstanceFromDBSnapshotResult = RestoreDBInstanceFromDBSnapshotResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }
derive instance newtypeRestoreDBInstanceFromDBSnapshotResult :: Newtype RestoreDBInstanceFromDBSnapshotResult _


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
derive instance newtypeRestoreDBInstanceToPointInTimeMessage :: Newtype RestoreDBInstanceToPointInTimeMessage _


newtype RestoreDBInstanceToPointInTimeResult = RestoreDBInstanceToPointInTimeResult 
  { "DBInstance" :: NullOrUndefined (DBInstance)
  }
derive instance newtypeRestoreDBInstanceToPointInTimeResult :: Newtype RestoreDBInstanceToPointInTimeResult _


newtype RevokeDBSecurityGroupIngressMessage = RevokeDBSecurityGroupIngressMessage 
  { "DBSecurityGroupName" :: (String)
  , "CIDRIP" :: NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined (String)
  , "EC2SecurityGroupId" :: NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined (String)
  }
derive instance newtypeRevokeDBSecurityGroupIngressMessage :: Newtype RevokeDBSecurityGroupIngressMessage _


newtype RevokeDBSecurityGroupIngressResult = RevokeDBSecurityGroupIngressResult 
  { "DBSecurityGroup" :: NullOrUndefined (DBSecurityGroup)
  }
derive instance newtypeRevokeDBSecurityGroupIngressResult :: Newtype RevokeDBSecurityGroupIngressResult _


-- | <p>SNS has responded that there is a problem with the SND topic specified.</p>
newtype SNSInvalidTopicFault = SNSInvalidTopicFault 
  { 
  }
derive instance newtypeSNSInvalidTopicFault :: Newtype SNSInvalidTopicFault _


-- | <p>You do not have permission to publish to the SNS topic ARN.</p>
newtype SNSNoAuthorizationFault = SNSNoAuthorizationFault 
  { 
  }
derive instance newtypeSNSNoAuthorizationFault :: Newtype SNSNoAuthorizationFault _


-- | <p>The SNS topic ARN does not exist.</p>
newtype SNSTopicArnNotFoundFault = SNSTopicArnNotFoundFault 
  { 
  }
derive instance newtypeSNSTopicArnNotFoundFault :: Newtype SNSTopicArnNotFoundFault _


-- | <p>Request would result in user exceeding the allowed number of DB snapshots.</p>
newtype SnapshotQuotaExceededFault = SnapshotQuotaExceededFault 
  { 
  }
derive instance newtypeSnapshotQuotaExceededFault :: Newtype SnapshotQuotaExceededFault _


newtype SourceIdsList = SourceIdsList (Array String)
derive instance newtypeSourceIdsList :: Newtype SourceIdsList _


-- | <p>The requested source could not be found.</p>
newtype SourceNotFoundFault = SourceNotFoundFault 
  { 
  }
derive instance newtypeSourceNotFoundFault :: Newtype SourceNotFoundFault _


newtype SourceType = SourceType String
derive instance newtypeSourceType :: Newtype SourceType _


-- | <p>Request would result in user exceeding the allowed amount of storage available across all DB instances.</p>
newtype StorageQuotaExceededFault = StorageQuotaExceededFault 
  { 
  }
derive instance newtypeStorageQuotaExceededFault :: Newtype StorageQuotaExceededFault _


newtype Subnet = Subnet 
  { "SubnetIdentifier" :: NullOrUndefined (String)
  , "SubnetAvailabilityZone" :: NullOrUndefined (AvailabilityZone)
  , "SubnetStatus" :: NullOrUndefined (String)
  }
derive instance newtypeSubnet :: Newtype Subnet _


-- | <p>The DB subnet is already in use in the Availability Zone.</p>
newtype SubnetAlreadyInUse = SubnetAlreadyInUse 
  { 
  }
derive instance newtypeSubnetAlreadyInUse :: Newtype SubnetAlreadyInUse _


newtype SubnetIdentifierList = SubnetIdentifierList (Array String)
derive instance newtypeSubnetIdentifierList :: Newtype SubnetIdentifierList _


newtype SubnetList = SubnetList (Array Subnet)
derive instance newtypeSubnetList :: Newtype SubnetList _


-- | <p>The supplied subscription name already exists.</p>
newtype SubscriptionAlreadyExistFault = SubscriptionAlreadyExistFault 
  { 
  }
derive instance newtypeSubscriptionAlreadyExistFault :: Newtype SubscriptionAlreadyExistFault _


-- | <p>The supplied category does not exist.</p>
newtype SubscriptionCategoryNotFoundFault = SubscriptionCategoryNotFoundFault 
  { 
  }
derive instance newtypeSubscriptionCategoryNotFoundFault :: Newtype SubscriptionCategoryNotFoundFault _


-- | <p>The subscription name does not exist.</p>
newtype SubscriptionNotFoundFault = SubscriptionNotFoundFault 
  { 
  }
derive instance newtypeSubscriptionNotFoundFault :: Newtype SubscriptionNotFoundFault _


newtype SupportedCharacterSetsList = SupportedCharacterSetsList (Array CharacterSet)
derive instance newtypeSupportedCharacterSetsList :: Newtype SupportedCharacterSetsList _


newtype TStamp = TStamp Number
derive instance newtypeTStamp :: Newtype TStamp _


newtype Tag = Tag 
  { "Key" :: NullOrUndefined (String)
  , "Value" :: NullOrUndefined (String)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagListMessage = TagListMessage 
  { "TagList" :: NullOrUndefined (TagList)
  }
derive instance newtypeTagListMessage :: Newtype TagListMessage _


newtype VpcSecurityGroupIdList = VpcSecurityGroupIdList (Array String)
derive instance newtypeVpcSecurityGroupIdList :: Newtype VpcSecurityGroupIdList _


newtype VpcSecurityGroupMembership = VpcSecurityGroupMembership 
  { "VpcSecurityGroupId" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  }
derive instance newtypeVpcSecurityGroupMembership :: Newtype VpcSecurityGroupMembership _


newtype VpcSecurityGroupMembershipList = VpcSecurityGroupMembershipList (Array VpcSecurityGroupMembership)
derive instance newtypeVpcSecurityGroupMembershipList :: Newtype VpcSecurityGroupMembershipList _
