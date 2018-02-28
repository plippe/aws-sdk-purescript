

module AWS.RDS where

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

serviceName = "RDS" :: String


addSourceIdentifierToSubscription :: forall eff. AddSourceIdentifierToSubscriptionMessage -> Aff (exception :: EXCEPTION | eff) AddSourceIdentifierToSubscriptionResult
addSourceIdentifierToSubscription = Request.request serviceName "addSourceIdentifierToSubscription" 


addTagsToResource :: forall eff. AddTagsToResourceMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
addTagsToResource = Request.request serviceName "addTagsToResource" 


authorizeDBSecurityGroupIngress :: forall eff. AuthorizeDBSecurityGroupIngressMessage -> Aff (exception :: EXCEPTION | eff) AuthorizeDBSecurityGroupIngressResult
authorizeDBSecurityGroupIngress = Request.request serviceName "authorizeDBSecurityGroupIngress" 


copyDBSnapshot :: forall eff. CopyDBSnapshotMessage -> Aff (exception :: EXCEPTION | eff) CopyDBSnapshotResult
copyDBSnapshot = Request.request serviceName "copyDBSnapshot" 


createDBInstance :: forall eff. CreateDBInstanceMessage -> Aff (exception :: EXCEPTION | eff) CreateDBInstanceResult
createDBInstance = Request.request serviceName "createDBInstance" 


createDBInstanceReadReplica :: forall eff. CreateDBInstanceReadReplicaMessage -> Aff (exception :: EXCEPTION | eff) CreateDBInstanceReadReplicaResult
createDBInstanceReadReplica = Request.request serviceName "createDBInstanceReadReplica" 


createDBParameterGroup :: forall eff. CreateDBParameterGroupMessage -> Aff (exception :: EXCEPTION | eff) CreateDBParameterGroupResult
createDBParameterGroup = Request.request serviceName "createDBParameterGroup" 


createDBSecurityGroup :: forall eff. CreateDBSecurityGroupMessage -> Aff (exception :: EXCEPTION | eff) CreateDBSecurityGroupResult
createDBSecurityGroup = Request.request serviceName "createDBSecurityGroup" 


createDBSnapshot :: forall eff. CreateDBSnapshotMessage -> Aff (exception :: EXCEPTION | eff) CreateDBSnapshotResult
createDBSnapshot = Request.request serviceName "createDBSnapshot" 


createDBSubnetGroup :: forall eff. CreateDBSubnetGroupMessage -> Aff (exception :: EXCEPTION | eff) CreateDBSubnetGroupResult
createDBSubnetGroup = Request.request serviceName "createDBSubnetGroup" 


createEventSubscription :: forall eff. CreateEventSubscriptionMessage -> Aff (exception :: EXCEPTION | eff) CreateEventSubscriptionResult
createEventSubscription = Request.request serviceName "createEventSubscription" 


createOptionGroup :: forall eff. CreateOptionGroupMessage -> Aff (exception :: EXCEPTION | eff) CreateOptionGroupResult
createOptionGroup = Request.request serviceName "createOptionGroup" 


deleteDBInstance :: forall eff. DeleteDBInstanceMessage -> Aff (exception :: EXCEPTION | eff) DeleteDBInstanceResult
deleteDBInstance = Request.request serviceName "deleteDBInstance" 


deleteDBParameterGroup :: forall eff. DeleteDBParameterGroupMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteDBParameterGroup = Request.request serviceName "deleteDBParameterGroup" 


deleteDBSecurityGroup :: forall eff. DeleteDBSecurityGroupMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteDBSecurityGroup = Request.request serviceName "deleteDBSecurityGroup" 


deleteDBSnapshot :: forall eff. DeleteDBSnapshotMessage -> Aff (exception :: EXCEPTION | eff) DeleteDBSnapshotResult
deleteDBSnapshot = Request.request serviceName "deleteDBSnapshot" 


deleteDBSubnetGroup :: forall eff. DeleteDBSubnetGroupMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteDBSubnetGroup = Request.request serviceName "deleteDBSubnetGroup" 


deleteEventSubscription :: forall eff. DeleteEventSubscriptionMessage -> Aff (exception :: EXCEPTION | eff) DeleteEventSubscriptionResult
deleteEventSubscription = Request.request serviceName "deleteEventSubscription" 


deleteOptionGroup :: forall eff. DeleteOptionGroupMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteOptionGroup = Request.request serviceName "deleteOptionGroup" 


describeDBEngineVersions :: forall eff. DescribeDBEngineVersionsMessage -> Aff (exception :: EXCEPTION | eff) DBEngineVersionMessage
describeDBEngineVersions = Request.request serviceName "describeDBEngineVersions" 


describeDBInstances :: forall eff. DescribeDBInstancesMessage -> Aff (exception :: EXCEPTION | eff) DBInstanceMessage
describeDBInstances = Request.request serviceName "describeDBInstances" 


describeDBParameterGroups :: forall eff. DescribeDBParameterGroupsMessage -> Aff (exception :: EXCEPTION | eff) DBParameterGroupsMessage
describeDBParameterGroups = Request.request serviceName "describeDBParameterGroups" 


describeDBParameters :: forall eff. DescribeDBParametersMessage -> Aff (exception :: EXCEPTION | eff) DBParameterGroupDetails
describeDBParameters = Request.request serviceName "describeDBParameters" 


describeDBSecurityGroups :: forall eff. DescribeDBSecurityGroupsMessage -> Aff (exception :: EXCEPTION | eff) DBSecurityGroupMessage
describeDBSecurityGroups = Request.request serviceName "describeDBSecurityGroups" 


describeDBSnapshots :: forall eff. DescribeDBSnapshotsMessage -> Aff (exception :: EXCEPTION | eff) DBSnapshotMessage
describeDBSnapshots = Request.request serviceName "describeDBSnapshots" 


describeDBSubnetGroups :: forall eff. DescribeDBSubnetGroupsMessage -> Aff (exception :: EXCEPTION | eff) DBSubnetGroupMessage
describeDBSubnetGroups = Request.request serviceName "describeDBSubnetGroups" 


describeEngineDefaultParameters :: forall eff. DescribeEngineDefaultParametersMessage -> Aff (exception :: EXCEPTION | eff) DescribeEngineDefaultParametersResult
describeEngineDefaultParameters = Request.request serviceName "describeEngineDefaultParameters" 


describeEventCategories :: forall eff. DescribeEventCategoriesMessage -> Aff (exception :: EXCEPTION | eff) EventCategoriesMessage
describeEventCategories = Request.request serviceName "describeEventCategories" 


describeEventSubscriptions :: forall eff. DescribeEventSubscriptionsMessage -> Aff (exception :: EXCEPTION | eff) EventSubscriptionsMessage
describeEventSubscriptions = Request.request serviceName "describeEventSubscriptions" 


describeEvents :: forall eff. DescribeEventsMessage -> Aff (exception :: EXCEPTION | eff) EventsMessage
describeEvents = Request.request serviceName "describeEvents" 


describeOptionGroupOptions :: forall eff. DescribeOptionGroupOptionsMessage -> Aff (exception :: EXCEPTION | eff) OptionGroupOptionsMessage
describeOptionGroupOptions = Request.request serviceName "describeOptionGroupOptions" 


describeOptionGroups :: forall eff. DescribeOptionGroupsMessage -> Aff (exception :: EXCEPTION | eff) OptionGroups
describeOptionGroups = Request.request serviceName "describeOptionGroups" 


describeOrderableDBInstanceOptions :: forall eff. DescribeOrderableDBInstanceOptionsMessage -> Aff (exception :: EXCEPTION | eff) OrderableDBInstanceOptionsMessage
describeOrderableDBInstanceOptions = Request.request serviceName "describeOrderableDBInstanceOptions" 


describeReservedDBInstances :: forall eff. DescribeReservedDBInstancesMessage -> Aff (exception :: EXCEPTION | eff) ReservedDBInstanceMessage
describeReservedDBInstances = Request.request serviceName "describeReservedDBInstances" 


describeReservedDBInstancesOfferings :: forall eff. DescribeReservedDBInstancesOfferingsMessage -> Aff (exception :: EXCEPTION | eff) ReservedDBInstancesOfferingMessage
describeReservedDBInstancesOfferings = Request.request serviceName "describeReservedDBInstancesOfferings" 


listTagsForResource :: forall eff. ListTagsForResourceMessage -> Aff (exception :: EXCEPTION | eff) TagListMessage
listTagsForResource = Request.request serviceName "listTagsForResource" 


modifyDBInstance :: forall eff. ModifyDBInstanceMessage -> Aff (exception :: EXCEPTION | eff) ModifyDBInstanceResult
modifyDBInstance = Request.request serviceName "modifyDBInstance" 


modifyDBParameterGroup :: forall eff. ModifyDBParameterGroupMessage -> Aff (exception :: EXCEPTION | eff) DBParameterGroupNameMessage
modifyDBParameterGroup = Request.request serviceName "modifyDBParameterGroup" 


modifyDBSubnetGroup :: forall eff. ModifyDBSubnetGroupMessage -> Aff (exception :: EXCEPTION | eff) ModifyDBSubnetGroupResult
modifyDBSubnetGroup = Request.request serviceName "modifyDBSubnetGroup" 


modifyEventSubscription :: forall eff. ModifyEventSubscriptionMessage -> Aff (exception :: EXCEPTION | eff) ModifyEventSubscriptionResult
modifyEventSubscription = Request.request serviceName "modifyEventSubscription" 


modifyOptionGroup :: forall eff. ModifyOptionGroupMessage -> Aff (exception :: EXCEPTION | eff) ModifyOptionGroupResult
modifyOptionGroup = Request.request serviceName "modifyOptionGroup" 


promoteReadReplica :: forall eff. PromoteReadReplicaMessage -> Aff (exception :: EXCEPTION | eff) PromoteReadReplicaResult
promoteReadReplica = Request.request serviceName "promoteReadReplica" 


purchaseReservedDBInstancesOffering :: forall eff. PurchaseReservedDBInstancesOfferingMessage -> Aff (exception :: EXCEPTION | eff) PurchaseReservedDBInstancesOfferingResult
purchaseReservedDBInstancesOffering = Request.request serviceName "purchaseReservedDBInstancesOffering" 


rebootDBInstance :: forall eff. RebootDBInstanceMessage -> Aff (exception :: EXCEPTION | eff) RebootDBInstanceResult
rebootDBInstance = Request.request serviceName "rebootDBInstance" 


removeSourceIdentifierFromSubscription :: forall eff. RemoveSourceIdentifierFromSubscriptionMessage -> Aff (exception :: EXCEPTION | eff) RemoveSourceIdentifierFromSubscriptionResult
removeSourceIdentifierFromSubscription = Request.request serviceName "removeSourceIdentifierFromSubscription" 


removeTagsFromResource :: forall eff. RemoveTagsFromResourceMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
removeTagsFromResource = Request.request serviceName "removeTagsFromResource" 


resetDBParameterGroup :: forall eff. ResetDBParameterGroupMessage -> Aff (exception :: EXCEPTION | eff) DBParameterGroupNameMessage
resetDBParameterGroup = Request.request serviceName "resetDBParameterGroup" 


restoreDBInstanceFromDBSnapshot :: forall eff. RestoreDBInstanceFromDBSnapshotMessage -> Aff (exception :: EXCEPTION | eff) RestoreDBInstanceFromDBSnapshotResult
restoreDBInstanceFromDBSnapshot = Request.request serviceName "restoreDBInstanceFromDBSnapshot" 


restoreDBInstanceToPointInTime :: forall eff. RestoreDBInstanceToPointInTimeMessage -> Aff (exception :: EXCEPTION | eff) RestoreDBInstanceToPointInTimeResult
restoreDBInstanceToPointInTime = Request.request serviceName "restoreDBInstanceToPointInTime" 


revokeDBSecurityGroupIngress :: forall eff. RevokeDBSecurityGroupIngressMessage -> Aff (exception :: EXCEPTION | eff) RevokeDBSecurityGroupIngressResult
revokeDBSecurityGroupIngress = Request.request serviceName "revokeDBSecurityGroupIngress" 


newtype AddSourceIdentifierToSubscriptionMessage = AddSourceIdentifierToSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SourceIdentifier" :: (String)
  }
derive instance newtypeAddSourceIdentifierToSubscriptionMessage :: Newtype AddSourceIdentifierToSubscriptionMessage _
derive instance repGenericAddSourceIdentifierToSubscriptionMessage :: Generic AddSourceIdentifierToSubscriptionMessage _
instance showAddSourceIdentifierToSubscriptionMessage :: Show AddSourceIdentifierToSubscriptionMessage where
  show = genericShow
instance decodeAddSourceIdentifierToSubscriptionMessage :: Decode AddSourceIdentifierToSubscriptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddSourceIdentifierToSubscriptionMessage :: Encode AddSourceIdentifierToSubscriptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddSourceIdentifierToSubscriptionResult = AddSourceIdentifierToSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined.NullOrUndefined (EventSubscription)
  }
derive instance newtypeAddSourceIdentifierToSubscriptionResult :: Newtype AddSourceIdentifierToSubscriptionResult _
derive instance repGenericAddSourceIdentifierToSubscriptionResult :: Generic AddSourceIdentifierToSubscriptionResult _
instance showAddSourceIdentifierToSubscriptionResult :: Show AddSourceIdentifierToSubscriptionResult where
  show = genericShow
instance decodeAddSourceIdentifierToSubscriptionResult :: Decode AddSourceIdentifierToSubscriptionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddSourceIdentifierToSubscriptionResult :: Encode AddSourceIdentifierToSubscriptionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddTagsToResourceMessage = AddTagsToResourceMessage 
  { "ResourceName" :: (String)
  , "Tags" :: (TagList)
  }
derive instance newtypeAddTagsToResourceMessage :: Newtype AddTagsToResourceMessage _
derive instance repGenericAddTagsToResourceMessage :: Generic AddTagsToResourceMessage _
instance showAddTagsToResourceMessage :: Show AddTagsToResourceMessage where
  show = genericShow
instance decodeAddTagsToResourceMessage :: Decode AddTagsToResourceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsToResourceMessage :: Encode AddTagsToResourceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplyMethod = ApplyMethod String
derive instance newtypeApplyMethod :: Newtype ApplyMethod _
derive instance repGenericApplyMethod :: Generic ApplyMethod _
instance showApplyMethod :: Show ApplyMethod where
  show = genericShow
instance decodeApplyMethod :: Decode ApplyMethod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplyMethod :: Encode ApplyMethod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified CIDRIP or EC2 security group is already authorized for the specified DB security group.</p>
newtype AuthorizationAlreadyExistsFault = AuthorizationAlreadyExistsFault Types.NoArguments
derive instance newtypeAuthorizationAlreadyExistsFault :: Newtype AuthorizationAlreadyExistsFault _
derive instance repGenericAuthorizationAlreadyExistsFault :: Generic AuthorizationAlreadyExistsFault _
instance showAuthorizationAlreadyExistsFault :: Show AuthorizationAlreadyExistsFault where
  show = genericShow
instance decodeAuthorizationAlreadyExistsFault :: Decode AuthorizationAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizationAlreadyExistsFault :: Encode AuthorizationAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specified CIDRIP or EC2 security group is not authorized for the specified DB security group.</p> <p>RDS may not also be authorized via IAM to perform necessary actions on your behalf.</p>
newtype AuthorizationNotFoundFault = AuthorizationNotFoundFault Types.NoArguments
derive instance newtypeAuthorizationNotFoundFault :: Newtype AuthorizationNotFoundFault _
derive instance repGenericAuthorizationNotFoundFault :: Generic AuthorizationNotFoundFault _
instance showAuthorizationNotFoundFault :: Show AuthorizationNotFoundFault where
  show = genericShow
instance decodeAuthorizationNotFoundFault :: Decode AuthorizationNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizationNotFoundFault :: Encode AuthorizationNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DB security group authorization quota has been reached.</p>
newtype AuthorizationQuotaExceededFault = AuthorizationQuotaExceededFault Types.NoArguments
derive instance newtypeAuthorizationQuotaExceededFault :: Newtype AuthorizationQuotaExceededFault _
derive instance repGenericAuthorizationQuotaExceededFault :: Generic AuthorizationQuotaExceededFault _
instance showAuthorizationQuotaExceededFault :: Show AuthorizationQuotaExceededFault where
  show = genericShow
instance decodeAuthorizationQuotaExceededFault :: Decode AuthorizationQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizationQuotaExceededFault :: Encode AuthorizationQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthorizeDBSecurityGroupIngressMessage = AuthorizeDBSecurityGroupIngressMessage 
  { "DBSecurityGroupName" :: (String)
  , "CIDRIP" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAuthorizeDBSecurityGroupIngressMessage :: Newtype AuthorizeDBSecurityGroupIngressMessage _
derive instance repGenericAuthorizeDBSecurityGroupIngressMessage :: Generic AuthorizeDBSecurityGroupIngressMessage _
instance showAuthorizeDBSecurityGroupIngressMessage :: Show AuthorizeDBSecurityGroupIngressMessage where
  show = genericShow
instance decodeAuthorizeDBSecurityGroupIngressMessage :: Decode AuthorizeDBSecurityGroupIngressMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizeDBSecurityGroupIngressMessage :: Encode AuthorizeDBSecurityGroupIngressMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthorizeDBSecurityGroupIngressResult = AuthorizeDBSecurityGroupIngressResult 
  { "DBSecurityGroup" :: NullOrUndefined.NullOrUndefined (DBSecurityGroup)
  }
derive instance newtypeAuthorizeDBSecurityGroupIngressResult :: Newtype AuthorizeDBSecurityGroupIngressResult _
derive instance repGenericAuthorizeDBSecurityGroupIngressResult :: Generic AuthorizeDBSecurityGroupIngressResult _
instance showAuthorizeDBSecurityGroupIngressResult :: Show AuthorizeDBSecurityGroupIngressResult where
  show = genericShow
instance decodeAuthorizeDBSecurityGroupIngressResult :: Decode AuthorizeDBSecurityGroupIngressResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizeDBSecurityGroupIngressResult :: Encode AuthorizeDBSecurityGroupIngressResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailabilityZone = AvailabilityZone 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "ProvisionedIopsCapable" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _
derive instance repGenericAvailabilityZone :: Generic AvailabilityZone _
instance showAvailabilityZone :: Show AvailabilityZone where
  show = genericShow
instance decodeAvailabilityZone :: Decode AvailabilityZone where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZone :: Encode AvailabilityZone where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailabilityZoneList = AvailabilityZoneList (Array AvailabilityZone)
derive instance newtypeAvailabilityZoneList :: Newtype AvailabilityZoneList _
derive instance repGenericAvailabilityZoneList :: Generic AvailabilityZoneList _
instance showAvailabilityZoneList :: Show AvailabilityZoneList where
  show = genericShow
instance decodeAvailabilityZoneList :: Decode AvailabilityZoneList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZoneList :: Encode AvailabilityZoneList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanOptional = BooleanOptional Boolean
derive instance newtypeBooleanOptional :: Newtype BooleanOptional _
derive instance repGenericBooleanOptional :: Generic BooleanOptional _
instance showBooleanOptional :: Show BooleanOptional where
  show = genericShow
instance decodeBooleanOptional :: Decode BooleanOptional where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanOptional :: Encode BooleanOptional where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CharacterSet = CharacterSet 
  { "CharacterSetName" :: NullOrUndefined.NullOrUndefined (String)
  , "CharacterSetDescription" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCharacterSet :: Newtype CharacterSet _
derive instance repGenericCharacterSet :: Generic CharacterSet _
instance showCharacterSet :: Show CharacterSet where
  show = genericShow
instance decodeCharacterSet :: Decode CharacterSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCharacterSet :: Encode CharacterSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopyDBSnapshotMessage = CopyDBSnapshotMessage 
  { "SourceDBSnapshotIdentifier" :: (String)
  , "TargetDBSnapshotIdentifier" :: (String)
  }
derive instance newtypeCopyDBSnapshotMessage :: Newtype CopyDBSnapshotMessage _
derive instance repGenericCopyDBSnapshotMessage :: Generic CopyDBSnapshotMessage _
instance showCopyDBSnapshotMessage :: Show CopyDBSnapshotMessage where
  show = genericShow
instance decodeCopyDBSnapshotMessage :: Decode CopyDBSnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopyDBSnapshotMessage :: Encode CopyDBSnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopyDBSnapshotResult = CopyDBSnapshotResult 
  { "DBSnapshot" :: NullOrUndefined.NullOrUndefined (DBSnapshot)
  }
derive instance newtypeCopyDBSnapshotResult :: Newtype CopyDBSnapshotResult _
derive instance repGenericCopyDBSnapshotResult :: Generic CopyDBSnapshotResult _
instance showCopyDBSnapshotResult :: Show CopyDBSnapshotResult where
  show = genericShow
instance decodeCopyDBSnapshotResult :: Decode CopyDBSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopyDBSnapshotResult :: Encode CopyDBSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDBInstanceMessage = CreateDBInstanceMessage 
  { "DBName" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstanceIdentifier" :: (String)
  , "AllocatedStorage" :: (IntegerOptional)
  , "DBInstanceClass" :: (String)
  , "Engine" :: (String)
  , "MasterUsername" :: (String)
  , "MasterUserPassword" :: (String)
  , "DBSecurityGroups" :: NullOrUndefined.NullOrUndefined (DBSecurityGroupNameList)
  , "VpcSecurityGroupIds" :: NullOrUndefined.NullOrUndefined (VpcSecurityGroupIdList)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "DBSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "DBParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "BackupRetentionPeriod" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "PreferredBackupWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "MultiAZ" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "LicenseModel" :: NullOrUndefined.NullOrUndefined (String)
  , "Iops" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "OptionGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "CharacterSetName" :: NullOrUndefined.NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeCreateDBInstanceMessage :: Newtype CreateDBInstanceMessage _
derive instance repGenericCreateDBInstanceMessage :: Generic CreateDBInstanceMessage _
instance showCreateDBInstanceMessage :: Show CreateDBInstanceMessage where
  show = genericShow
instance decodeCreateDBInstanceMessage :: Decode CreateDBInstanceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDBInstanceMessage :: Encode CreateDBInstanceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDBInstanceReadReplicaMessage = CreateDBInstanceReadReplicaMessage 
  { "DBInstanceIdentifier" :: (String)
  , "SourceDBInstanceIdentifier" :: (String)
  , "DBInstanceClass" :: NullOrUndefined.NullOrUndefined (String)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "Iops" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "OptionGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeCreateDBInstanceReadReplicaMessage :: Newtype CreateDBInstanceReadReplicaMessage _
derive instance repGenericCreateDBInstanceReadReplicaMessage :: Generic CreateDBInstanceReadReplicaMessage _
instance showCreateDBInstanceReadReplicaMessage :: Show CreateDBInstanceReadReplicaMessage where
  show = genericShow
instance decodeCreateDBInstanceReadReplicaMessage :: Decode CreateDBInstanceReadReplicaMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDBInstanceReadReplicaMessage :: Encode CreateDBInstanceReadReplicaMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDBInstanceReadReplicaResult = CreateDBInstanceReadReplicaResult 
  { "DBInstance" :: NullOrUndefined.NullOrUndefined (DBInstance)
  }
derive instance newtypeCreateDBInstanceReadReplicaResult :: Newtype CreateDBInstanceReadReplicaResult _
derive instance repGenericCreateDBInstanceReadReplicaResult :: Generic CreateDBInstanceReadReplicaResult _
instance showCreateDBInstanceReadReplicaResult :: Show CreateDBInstanceReadReplicaResult where
  show = genericShow
instance decodeCreateDBInstanceReadReplicaResult :: Decode CreateDBInstanceReadReplicaResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDBInstanceReadReplicaResult :: Encode CreateDBInstanceReadReplicaResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDBInstanceResult = CreateDBInstanceResult 
  { "DBInstance" :: NullOrUndefined.NullOrUndefined (DBInstance)
  }
derive instance newtypeCreateDBInstanceResult :: Newtype CreateDBInstanceResult _
derive instance repGenericCreateDBInstanceResult :: Generic CreateDBInstanceResult _
instance showCreateDBInstanceResult :: Show CreateDBInstanceResult where
  show = genericShow
instance decodeCreateDBInstanceResult :: Decode CreateDBInstanceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDBInstanceResult :: Encode CreateDBInstanceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDBParameterGroupMessage = CreateDBParameterGroupMessage 
  { "DBParameterGroupName" :: (String)
  , "DBParameterGroupFamily" :: (String)
  , "Description" :: (String)
  }
derive instance newtypeCreateDBParameterGroupMessage :: Newtype CreateDBParameterGroupMessage _
derive instance repGenericCreateDBParameterGroupMessage :: Generic CreateDBParameterGroupMessage _
instance showCreateDBParameterGroupMessage :: Show CreateDBParameterGroupMessage where
  show = genericShow
instance decodeCreateDBParameterGroupMessage :: Decode CreateDBParameterGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDBParameterGroupMessage :: Encode CreateDBParameterGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDBParameterGroupResult = CreateDBParameterGroupResult 
  { "DBParameterGroup" :: NullOrUndefined.NullOrUndefined (DBParameterGroup)
  }
derive instance newtypeCreateDBParameterGroupResult :: Newtype CreateDBParameterGroupResult _
derive instance repGenericCreateDBParameterGroupResult :: Generic CreateDBParameterGroupResult _
instance showCreateDBParameterGroupResult :: Show CreateDBParameterGroupResult where
  show = genericShow
instance decodeCreateDBParameterGroupResult :: Decode CreateDBParameterGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDBParameterGroupResult :: Encode CreateDBParameterGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDBSecurityGroupMessage = CreateDBSecurityGroupMessage 
  { "DBSecurityGroupName" :: (String)
  , "DBSecurityGroupDescription" :: (String)
  }
derive instance newtypeCreateDBSecurityGroupMessage :: Newtype CreateDBSecurityGroupMessage _
derive instance repGenericCreateDBSecurityGroupMessage :: Generic CreateDBSecurityGroupMessage _
instance showCreateDBSecurityGroupMessage :: Show CreateDBSecurityGroupMessage where
  show = genericShow
instance decodeCreateDBSecurityGroupMessage :: Decode CreateDBSecurityGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDBSecurityGroupMessage :: Encode CreateDBSecurityGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDBSecurityGroupResult = CreateDBSecurityGroupResult 
  { "DBSecurityGroup" :: NullOrUndefined.NullOrUndefined (DBSecurityGroup)
  }
derive instance newtypeCreateDBSecurityGroupResult :: Newtype CreateDBSecurityGroupResult _
derive instance repGenericCreateDBSecurityGroupResult :: Generic CreateDBSecurityGroupResult _
instance showCreateDBSecurityGroupResult :: Show CreateDBSecurityGroupResult where
  show = genericShow
instance decodeCreateDBSecurityGroupResult :: Decode CreateDBSecurityGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDBSecurityGroupResult :: Encode CreateDBSecurityGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDBSnapshotMessage = CreateDBSnapshotMessage 
  { "DBSnapshotIdentifier" :: (String)
  , "DBInstanceIdentifier" :: (String)
  }
derive instance newtypeCreateDBSnapshotMessage :: Newtype CreateDBSnapshotMessage _
derive instance repGenericCreateDBSnapshotMessage :: Generic CreateDBSnapshotMessage _
instance showCreateDBSnapshotMessage :: Show CreateDBSnapshotMessage where
  show = genericShow
instance decodeCreateDBSnapshotMessage :: Decode CreateDBSnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDBSnapshotMessage :: Encode CreateDBSnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDBSnapshotResult = CreateDBSnapshotResult 
  { "DBSnapshot" :: NullOrUndefined.NullOrUndefined (DBSnapshot)
  }
derive instance newtypeCreateDBSnapshotResult :: Newtype CreateDBSnapshotResult _
derive instance repGenericCreateDBSnapshotResult :: Generic CreateDBSnapshotResult _
instance showCreateDBSnapshotResult :: Show CreateDBSnapshotResult where
  show = genericShow
instance decodeCreateDBSnapshotResult :: Decode CreateDBSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDBSnapshotResult :: Encode CreateDBSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDBSubnetGroupMessage = CreateDBSubnetGroupMessage 
  { "DBSubnetGroupName" :: (String)
  , "DBSubnetGroupDescription" :: (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  }
derive instance newtypeCreateDBSubnetGroupMessage :: Newtype CreateDBSubnetGroupMessage _
derive instance repGenericCreateDBSubnetGroupMessage :: Generic CreateDBSubnetGroupMessage _
instance showCreateDBSubnetGroupMessage :: Show CreateDBSubnetGroupMessage where
  show = genericShow
instance decodeCreateDBSubnetGroupMessage :: Decode CreateDBSubnetGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDBSubnetGroupMessage :: Encode CreateDBSubnetGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDBSubnetGroupResult = CreateDBSubnetGroupResult 
  { "DBSubnetGroup" :: NullOrUndefined.NullOrUndefined (DBSubnetGroup)
  }
derive instance newtypeCreateDBSubnetGroupResult :: Newtype CreateDBSubnetGroupResult _
derive instance repGenericCreateDBSubnetGroupResult :: Generic CreateDBSubnetGroupResult _
instance showCreateDBSubnetGroupResult :: Show CreateDBSubnetGroupResult where
  show = genericShow
instance decodeCreateDBSubnetGroupResult :: Decode CreateDBSubnetGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDBSubnetGroupResult :: Encode CreateDBSubnetGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateEventSubscriptionMessage = CreateEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SnsTopicArn" :: (String)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined.NullOrUndefined (EventCategoriesList)
  , "SourceIds" :: NullOrUndefined.NullOrUndefined (SourceIdsList)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeCreateEventSubscriptionMessage :: Newtype CreateEventSubscriptionMessage _
derive instance repGenericCreateEventSubscriptionMessage :: Generic CreateEventSubscriptionMessage _
instance showCreateEventSubscriptionMessage :: Show CreateEventSubscriptionMessage where
  show = genericShow
instance decodeCreateEventSubscriptionMessage :: Decode CreateEventSubscriptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateEventSubscriptionMessage :: Encode CreateEventSubscriptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateEventSubscriptionResult = CreateEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined.NullOrUndefined (EventSubscription)
  }
derive instance newtypeCreateEventSubscriptionResult :: Newtype CreateEventSubscriptionResult _
derive instance repGenericCreateEventSubscriptionResult :: Generic CreateEventSubscriptionResult _
instance showCreateEventSubscriptionResult :: Show CreateEventSubscriptionResult where
  show = genericShow
instance decodeCreateEventSubscriptionResult :: Decode CreateEventSubscriptionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateEventSubscriptionResult :: Encode CreateEventSubscriptionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateOptionGroupMessage = CreateOptionGroupMessage 
  { "OptionGroupName" :: (String)
  , "EngineName" :: (String)
  , "MajorEngineVersion" :: (String)
  , "OptionGroupDescription" :: (String)
  }
derive instance newtypeCreateOptionGroupMessage :: Newtype CreateOptionGroupMessage _
derive instance repGenericCreateOptionGroupMessage :: Generic CreateOptionGroupMessage _
instance showCreateOptionGroupMessage :: Show CreateOptionGroupMessage where
  show = genericShow
instance decodeCreateOptionGroupMessage :: Decode CreateOptionGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateOptionGroupMessage :: Encode CreateOptionGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateOptionGroupResult = CreateOptionGroupResult 
  { "OptionGroup" :: NullOrUndefined.NullOrUndefined (OptionGroup)
  }
derive instance newtypeCreateOptionGroupResult :: Newtype CreateOptionGroupResult _
derive instance repGenericCreateOptionGroupResult :: Generic CreateOptionGroupResult _
instance showCreateOptionGroupResult :: Show CreateOptionGroupResult where
  show = genericShow
instance decodeCreateOptionGroupResult :: Decode CreateOptionGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateOptionGroupResult :: Encode CreateOptionGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBEngineVersion = DBEngineVersion 
  { "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "DBParameterGroupFamily" :: NullOrUndefined.NullOrUndefined (String)
  , "DBEngineDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "DBEngineVersionDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultCharacterSet" :: NullOrUndefined.NullOrUndefined (CharacterSet)
  , "SupportedCharacterSets" :: NullOrUndefined.NullOrUndefined (SupportedCharacterSetsList)
  }
derive instance newtypeDBEngineVersion :: Newtype DBEngineVersion _
derive instance repGenericDBEngineVersion :: Generic DBEngineVersion _
instance showDBEngineVersion :: Show DBEngineVersion where
  show = genericShow
instance decodeDBEngineVersion :: Decode DBEngineVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBEngineVersion :: Encode DBEngineVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBEngineVersionList = DBEngineVersionList (Array DBEngineVersion)
derive instance newtypeDBEngineVersionList :: Newtype DBEngineVersionList _
derive instance repGenericDBEngineVersionList :: Generic DBEngineVersionList _
instance showDBEngineVersionList :: Show DBEngineVersionList where
  show = genericShow
instance decodeDBEngineVersionList :: Decode DBEngineVersionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBEngineVersionList :: Encode DBEngineVersionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBEngineVersionMessage = DBEngineVersionMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "DBEngineVersions" :: NullOrUndefined.NullOrUndefined (DBEngineVersionList)
  }
derive instance newtypeDBEngineVersionMessage :: Newtype DBEngineVersionMessage _
derive instance repGenericDBEngineVersionMessage :: Generic DBEngineVersionMessage _
instance showDBEngineVersionMessage :: Show DBEngineVersionMessage where
  show = genericShow
instance decodeDBEngineVersionMessage :: Decode DBEngineVersionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBEngineVersionMessage :: Encode DBEngineVersionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBInstance = DBInstance 
  { "DBInstanceIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined.NullOrUndefined (String)
  , "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstanceStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "MasterUsername" :: NullOrUndefined.NullOrUndefined (String)
  , "DBName" :: NullOrUndefined.NullOrUndefined (String)
  , "Endpoint" :: NullOrUndefined.NullOrUndefined (Endpoint)
  , "AllocatedStorage" :: NullOrUndefined.NullOrUndefined (Int)
  , "InstanceCreateTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "PreferredBackupWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "BackupRetentionPeriod" :: NullOrUndefined.NullOrUndefined (Int)
  , "DBSecurityGroups" :: NullOrUndefined.NullOrUndefined (DBSecurityGroupMembershipList)
  , "VpcSecurityGroups" :: NullOrUndefined.NullOrUndefined (VpcSecurityGroupMembershipList)
  , "DBParameterGroups" :: NullOrUndefined.NullOrUndefined (DBParameterGroupStatusList)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "DBSubnetGroup" :: NullOrUndefined.NullOrUndefined (DBSubnetGroup)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "PendingModifiedValues" :: NullOrUndefined.NullOrUndefined (PendingModifiedValues)
  , "LatestRestorableTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "MultiAZ" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ReadReplicaSourceDBInstanceIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadReplicaDBInstanceIdentifiers" :: NullOrUndefined.NullOrUndefined (ReadReplicaDBInstanceIdentifierList)
  , "LicenseModel" :: NullOrUndefined.NullOrUndefined (String)
  , "Iops" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "OptionGroupMembership" :: NullOrUndefined.NullOrUndefined (OptionGroupMembership)
  , "CharacterSetName" :: NullOrUndefined.NullOrUndefined (String)
  , "SecondaryAvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "PubliclyAccessible" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeDBInstance :: Newtype DBInstance _
derive instance repGenericDBInstance :: Generic DBInstance _
instance showDBInstance :: Show DBInstance where
  show = genericShow
instance decodeDBInstance :: Decode DBInstance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBInstance :: Encode DBInstance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>User already has a DB instance with the given identifier.</p>
newtype DBInstanceAlreadyExistsFault = DBInstanceAlreadyExistsFault Types.NoArguments
derive instance newtypeDBInstanceAlreadyExistsFault :: Newtype DBInstanceAlreadyExistsFault _
derive instance repGenericDBInstanceAlreadyExistsFault :: Generic DBInstanceAlreadyExistsFault _
instance showDBInstanceAlreadyExistsFault :: Show DBInstanceAlreadyExistsFault where
  show = genericShow
instance decodeDBInstanceAlreadyExistsFault :: Decode DBInstanceAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBInstanceAlreadyExistsFault :: Encode DBInstanceAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBInstanceList = DBInstanceList (Array DBInstance)
derive instance newtypeDBInstanceList :: Newtype DBInstanceList _
derive instance repGenericDBInstanceList :: Generic DBInstanceList _
instance showDBInstanceList :: Show DBInstanceList where
  show = genericShow
instance decodeDBInstanceList :: Decode DBInstanceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBInstanceList :: Encode DBInstanceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBInstanceMessage = DBInstanceMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstances" :: NullOrUndefined.NullOrUndefined (DBInstanceList)
  }
derive instance newtypeDBInstanceMessage :: Newtype DBInstanceMessage _
derive instance repGenericDBInstanceMessage :: Generic DBInstanceMessage _
instance showDBInstanceMessage :: Show DBInstanceMessage where
  show = genericShow
instance decodeDBInstanceMessage :: Decode DBInstanceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBInstanceMessage :: Encode DBInstanceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> <i>DBInstanceIdentifier</i> does not refer to an existing DB instance. </p>
newtype DBInstanceNotFoundFault = DBInstanceNotFoundFault Types.NoArguments
derive instance newtypeDBInstanceNotFoundFault :: Newtype DBInstanceNotFoundFault _
derive instance repGenericDBInstanceNotFoundFault :: Generic DBInstanceNotFoundFault _
instance showDBInstanceNotFoundFault :: Show DBInstanceNotFoundFault where
  show = genericShow
instance decodeDBInstanceNotFoundFault :: Decode DBInstanceNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBInstanceNotFoundFault :: Encode DBInstanceNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBParameterGroup = DBParameterGroup 
  { "DBParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "DBParameterGroupFamily" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDBParameterGroup :: Newtype DBParameterGroup _
derive instance repGenericDBParameterGroup :: Generic DBParameterGroup _
instance showDBParameterGroup :: Show DBParameterGroup where
  show = genericShow
instance decodeDBParameterGroup :: Decode DBParameterGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBParameterGroup :: Encode DBParameterGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A DB parameter group with the same name exists.</p>
newtype DBParameterGroupAlreadyExistsFault = DBParameterGroupAlreadyExistsFault Types.NoArguments
derive instance newtypeDBParameterGroupAlreadyExistsFault :: Newtype DBParameterGroupAlreadyExistsFault _
derive instance repGenericDBParameterGroupAlreadyExistsFault :: Generic DBParameterGroupAlreadyExistsFault _
instance showDBParameterGroupAlreadyExistsFault :: Show DBParameterGroupAlreadyExistsFault where
  show = genericShow
instance decodeDBParameterGroupAlreadyExistsFault :: Decode DBParameterGroupAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBParameterGroupAlreadyExistsFault :: Encode DBParameterGroupAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBParameterGroupDetails = DBParameterGroupDetails 
  { "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDBParameterGroupDetails :: Newtype DBParameterGroupDetails _
derive instance repGenericDBParameterGroupDetails :: Generic DBParameterGroupDetails _
instance showDBParameterGroupDetails :: Show DBParameterGroupDetails where
  show = genericShow
instance decodeDBParameterGroupDetails :: Decode DBParameterGroupDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBParameterGroupDetails :: Encode DBParameterGroupDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBParameterGroupList = DBParameterGroupList (Array DBParameterGroup)
derive instance newtypeDBParameterGroupList :: Newtype DBParameterGroupList _
derive instance repGenericDBParameterGroupList :: Generic DBParameterGroupList _
instance showDBParameterGroupList :: Show DBParameterGroupList where
  show = genericShow
instance decodeDBParameterGroupList :: Decode DBParameterGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBParameterGroupList :: Encode DBParameterGroupList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBParameterGroupNameMessage = DBParameterGroupNameMessage 
  { "DBParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDBParameterGroupNameMessage :: Newtype DBParameterGroupNameMessage _
derive instance repGenericDBParameterGroupNameMessage :: Generic DBParameterGroupNameMessage _
instance showDBParameterGroupNameMessage :: Show DBParameterGroupNameMessage where
  show = genericShow
instance decodeDBParameterGroupNameMessage :: Decode DBParameterGroupNameMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBParameterGroupNameMessage :: Encode DBParameterGroupNameMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> <i>DBParameterGroupName</i> does not refer to an existing DB parameter group. </p>
newtype DBParameterGroupNotFoundFault = DBParameterGroupNotFoundFault Types.NoArguments
derive instance newtypeDBParameterGroupNotFoundFault :: Newtype DBParameterGroupNotFoundFault _
derive instance repGenericDBParameterGroupNotFoundFault :: Generic DBParameterGroupNotFoundFault _
instance showDBParameterGroupNotFoundFault :: Show DBParameterGroupNotFoundFault where
  show = genericShow
instance decodeDBParameterGroupNotFoundFault :: Decode DBParameterGroupNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBParameterGroupNotFoundFault :: Encode DBParameterGroupNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request would result in user exceeding the allowed number of DB parameter groups.</p>
newtype DBParameterGroupQuotaExceededFault = DBParameterGroupQuotaExceededFault Types.NoArguments
derive instance newtypeDBParameterGroupQuotaExceededFault :: Newtype DBParameterGroupQuotaExceededFault _
derive instance repGenericDBParameterGroupQuotaExceededFault :: Generic DBParameterGroupQuotaExceededFault _
instance showDBParameterGroupQuotaExceededFault :: Show DBParameterGroupQuotaExceededFault where
  show = genericShow
instance decodeDBParameterGroupQuotaExceededFault :: Decode DBParameterGroupQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBParameterGroupQuotaExceededFault :: Encode DBParameterGroupQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBParameterGroupStatus = DBParameterGroupStatus 
  { "DBParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterApplyStatus" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDBParameterGroupStatus :: Newtype DBParameterGroupStatus _
derive instance repGenericDBParameterGroupStatus :: Generic DBParameterGroupStatus _
instance showDBParameterGroupStatus :: Show DBParameterGroupStatus where
  show = genericShow
instance decodeDBParameterGroupStatus :: Decode DBParameterGroupStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBParameterGroupStatus :: Encode DBParameterGroupStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBParameterGroupStatusList = DBParameterGroupStatusList (Array DBParameterGroupStatus)
derive instance newtypeDBParameterGroupStatusList :: Newtype DBParameterGroupStatusList _
derive instance repGenericDBParameterGroupStatusList :: Generic DBParameterGroupStatusList _
instance showDBParameterGroupStatusList :: Show DBParameterGroupStatusList where
  show = genericShow
instance decodeDBParameterGroupStatusList :: Decode DBParameterGroupStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBParameterGroupStatusList :: Encode DBParameterGroupStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBParameterGroupsMessage = DBParameterGroupsMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "DBParameterGroups" :: NullOrUndefined.NullOrUndefined (DBParameterGroupList)
  }
derive instance newtypeDBParameterGroupsMessage :: Newtype DBParameterGroupsMessage _
derive instance repGenericDBParameterGroupsMessage :: Generic DBParameterGroupsMessage _
instance showDBParameterGroupsMessage :: Show DBParameterGroupsMessage where
  show = genericShow
instance decodeDBParameterGroupsMessage :: Decode DBParameterGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBParameterGroupsMessage :: Encode DBParameterGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBSecurityGroup = DBSecurityGroup 
  { "OwnerId" :: NullOrUndefined.NullOrUndefined (String)
  , "DBSecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "DBSecurityGroupDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroups" :: NullOrUndefined.NullOrUndefined (EC2SecurityGroupList)
  , "IPRanges" :: NullOrUndefined.NullOrUndefined (IPRangeList)
  }
derive instance newtypeDBSecurityGroup :: Newtype DBSecurityGroup _
derive instance repGenericDBSecurityGroup :: Generic DBSecurityGroup _
instance showDBSecurityGroup :: Show DBSecurityGroup where
  show = genericShow
instance decodeDBSecurityGroup :: Decode DBSecurityGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSecurityGroup :: Encode DBSecurityGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A DB security group with the name specified in <i>DBSecurityGroupName</i> already exists. </p>
newtype DBSecurityGroupAlreadyExistsFault = DBSecurityGroupAlreadyExistsFault Types.NoArguments
derive instance newtypeDBSecurityGroupAlreadyExistsFault :: Newtype DBSecurityGroupAlreadyExistsFault _
derive instance repGenericDBSecurityGroupAlreadyExistsFault :: Generic DBSecurityGroupAlreadyExistsFault _
instance showDBSecurityGroupAlreadyExistsFault :: Show DBSecurityGroupAlreadyExistsFault where
  show = genericShow
instance decodeDBSecurityGroupAlreadyExistsFault :: Decode DBSecurityGroupAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSecurityGroupAlreadyExistsFault :: Encode DBSecurityGroupAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBSecurityGroupMembership = DBSecurityGroupMembership 
  { "DBSecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDBSecurityGroupMembership :: Newtype DBSecurityGroupMembership _
derive instance repGenericDBSecurityGroupMembership :: Generic DBSecurityGroupMembership _
instance showDBSecurityGroupMembership :: Show DBSecurityGroupMembership where
  show = genericShow
instance decodeDBSecurityGroupMembership :: Decode DBSecurityGroupMembership where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSecurityGroupMembership :: Encode DBSecurityGroupMembership where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBSecurityGroupMembershipList = DBSecurityGroupMembershipList (Array DBSecurityGroupMembership)
derive instance newtypeDBSecurityGroupMembershipList :: Newtype DBSecurityGroupMembershipList _
derive instance repGenericDBSecurityGroupMembershipList :: Generic DBSecurityGroupMembershipList _
instance showDBSecurityGroupMembershipList :: Show DBSecurityGroupMembershipList where
  show = genericShow
instance decodeDBSecurityGroupMembershipList :: Decode DBSecurityGroupMembershipList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSecurityGroupMembershipList :: Encode DBSecurityGroupMembershipList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBSecurityGroupMessage = DBSecurityGroupMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "DBSecurityGroups" :: NullOrUndefined.NullOrUndefined (DBSecurityGroups)
  }
derive instance newtypeDBSecurityGroupMessage :: Newtype DBSecurityGroupMessage _
derive instance repGenericDBSecurityGroupMessage :: Generic DBSecurityGroupMessage _
instance showDBSecurityGroupMessage :: Show DBSecurityGroupMessage where
  show = genericShow
instance decodeDBSecurityGroupMessage :: Decode DBSecurityGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSecurityGroupMessage :: Encode DBSecurityGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBSecurityGroupNameList = DBSecurityGroupNameList (Array String)
derive instance newtypeDBSecurityGroupNameList :: Newtype DBSecurityGroupNameList _
derive instance repGenericDBSecurityGroupNameList :: Generic DBSecurityGroupNameList _
instance showDBSecurityGroupNameList :: Show DBSecurityGroupNameList where
  show = genericShow
instance decodeDBSecurityGroupNameList :: Decode DBSecurityGroupNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSecurityGroupNameList :: Encode DBSecurityGroupNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> <i>DBSecurityGroupName</i> does not refer to an existing DB security group. </p>
newtype DBSecurityGroupNotFoundFault = DBSecurityGroupNotFoundFault Types.NoArguments
derive instance newtypeDBSecurityGroupNotFoundFault :: Newtype DBSecurityGroupNotFoundFault _
derive instance repGenericDBSecurityGroupNotFoundFault :: Generic DBSecurityGroupNotFoundFault _
instance showDBSecurityGroupNotFoundFault :: Show DBSecurityGroupNotFoundFault where
  show = genericShow
instance decodeDBSecurityGroupNotFoundFault :: Decode DBSecurityGroupNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSecurityGroupNotFoundFault :: Encode DBSecurityGroupNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A DB security group is not allowed for this action.</p>
newtype DBSecurityGroupNotSupportedFault = DBSecurityGroupNotSupportedFault Types.NoArguments
derive instance newtypeDBSecurityGroupNotSupportedFault :: Newtype DBSecurityGroupNotSupportedFault _
derive instance repGenericDBSecurityGroupNotSupportedFault :: Generic DBSecurityGroupNotSupportedFault _
instance showDBSecurityGroupNotSupportedFault :: Show DBSecurityGroupNotSupportedFault where
  show = genericShow
instance decodeDBSecurityGroupNotSupportedFault :: Decode DBSecurityGroupNotSupportedFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSecurityGroupNotSupportedFault :: Encode DBSecurityGroupNotSupportedFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request would result in user exceeding the allowed number of DB security groups.</p>
newtype DBSecurityGroupQuotaExceededFault = DBSecurityGroupQuotaExceededFault Types.NoArguments
derive instance newtypeDBSecurityGroupQuotaExceededFault :: Newtype DBSecurityGroupQuotaExceededFault _
derive instance repGenericDBSecurityGroupQuotaExceededFault :: Generic DBSecurityGroupQuotaExceededFault _
instance showDBSecurityGroupQuotaExceededFault :: Show DBSecurityGroupQuotaExceededFault where
  show = genericShow
instance decodeDBSecurityGroupQuotaExceededFault :: Decode DBSecurityGroupQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSecurityGroupQuotaExceededFault :: Encode DBSecurityGroupQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBSecurityGroups = DBSecurityGroups (Array DBSecurityGroup)
derive instance newtypeDBSecurityGroups :: Newtype DBSecurityGroups _
derive instance repGenericDBSecurityGroups :: Generic DBSecurityGroups _
instance showDBSecurityGroups :: Show DBSecurityGroups where
  show = genericShow
instance decodeDBSecurityGroups :: Decode DBSecurityGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSecurityGroups :: Encode DBSecurityGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBSnapshot = DBSnapshot 
  { "DBSnapshotIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstanceIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotCreateTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "AllocatedStorage" :: NullOrUndefined.NullOrUndefined (Int)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (Int)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceCreateTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "MasterUsername" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LicenseModel" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotType" :: NullOrUndefined.NullOrUndefined (String)
  , "Iops" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  }
derive instance newtypeDBSnapshot :: Newtype DBSnapshot _
derive instance repGenericDBSnapshot :: Generic DBSnapshot _
instance showDBSnapshot :: Show DBSnapshot where
  show = genericShow
instance decodeDBSnapshot :: Decode DBSnapshot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSnapshot :: Encode DBSnapshot where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> <i>DBSnapshotIdentifier</i> is already used by an existing snapshot. </p>
newtype DBSnapshotAlreadyExistsFault = DBSnapshotAlreadyExistsFault Types.NoArguments
derive instance newtypeDBSnapshotAlreadyExistsFault :: Newtype DBSnapshotAlreadyExistsFault _
derive instance repGenericDBSnapshotAlreadyExistsFault :: Generic DBSnapshotAlreadyExistsFault _
instance showDBSnapshotAlreadyExistsFault :: Show DBSnapshotAlreadyExistsFault where
  show = genericShow
instance decodeDBSnapshotAlreadyExistsFault :: Decode DBSnapshotAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSnapshotAlreadyExistsFault :: Encode DBSnapshotAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBSnapshotList = DBSnapshotList (Array DBSnapshot)
derive instance newtypeDBSnapshotList :: Newtype DBSnapshotList _
derive instance repGenericDBSnapshotList :: Generic DBSnapshotList _
instance showDBSnapshotList :: Show DBSnapshotList where
  show = genericShow
instance decodeDBSnapshotList :: Decode DBSnapshotList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSnapshotList :: Encode DBSnapshotList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBSnapshotMessage = DBSnapshotMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "DBSnapshots" :: NullOrUndefined.NullOrUndefined (DBSnapshotList)
  }
derive instance newtypeDBSnapshotMessage :: Newtype DBSnapshotMessage _
derive instance repGenericDBSnapshotMessage :: Generic DBSnapshotMessage _
instance showDBSnapshotMessage :: Show DBSnapshotMessage where
  show = genericShow
instance decodeDBSnapshotMessage :: Decode DBSnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSnapshotMessage :: Encode DBSnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> <i>DBSnapshotIdentifier</i> does not refer to an existing DB snapshot. </p>
newtype DBSnapshotNotFoundFault = DBSnapshotNotFoundFault Types.NoArguments
derive instance newtypeDBSnapshotNotFoundFault :: Newtype DBSnapshotNotFoundFault _
derive instance repGenericDBSnapshotNotFoundFault :: Generic DBSnapshotNotFoundFault _
instance showDBSnapshotNotFoundFault :: Show DBSnapshotNotFoundFault where
  show = genericShow
instance decodeDBSnapshotNotFoundFault :: Decode DBSnapshotNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSnapshotNotFoundFault :: Encode DBSnapshotNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBSubnetGroup = DBSubnetGroup 
  { "DBSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "DBSubnetGroupDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (String)
  , "SubnetGroupStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "Subnets" :: NullOrUndefined.NullOrUndefined (SubnetList)
  }
derive instance newtypeDBSubnetGroup :: Newtype DBSubnetGroup _
derive instance repGenericDBSubnetGroup :: Generic DBSubnetGroup _
instance showDBSubnetGroup :: Show DBSubnetGroup where
  show = genericShow
instance decodeDBSubnetGroup :: Decode DBSubnetGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSubnetGroup :: Encode DBSubnetGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> <i>DBSubnetGroupName</i> is already used by an existing DB subnet group. </p>
newtype DBSubnetGroupAlreadyExistsFault = DBSubnetGroupAlreadyExistsFault Types.NoArguments
derive instance newtypeDBSubnetGroupAlreadyExistsFault :: Newtype DBSubnetGroupAlreadyExistsFault _
derive instance repGenericDBSubnetGroupAlreadyExistsFault :: Generic DBSubnetGroupAlreadyExistsFault _
instance showDBSubnetGroupAlreadyExistsFault :: Show DBSubnetGroupAlreadyExistsFault where
  show = genericShow
instance decodeDBSubnetGroupAlreadyExistsFault :: Decode DBSubnetGroupAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSubnetGroupAlreadyExistsFault :: Encode DBSubnetGroupAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Subnets in the DB subnet group should cover at least two Availability Zones unless there is only one Availability Zone.</p>
newtype DBSubnetGroupDoesNotCoverEnoughAZs = DBSubnetGroupDoesNotCoverEnoughAZs Types.NoArguments
derive instance newtypeDBSubnetGroupDoesNotCoverEnoughAZs :: Newtype DBSubnetGroupDoesNotCoverEnoughAZs _
derive instance repGenericDBSubnetGroupDoesNotCoverEnoughAZs :: Generic DBSubnetGroupDoesNotCoverEnoughAZs _
instance showDBSubnetGroupDoesNotCoverEnoughAZs :: Show DBSubnetGroupDoesNotCoverEnoughAZs where
  show = genericShow
instance decodeDBSubnetGroupDoesNotCoverEnoughAZs :: Decode DBSubnetGroupDoesNotCoverEnoughAZs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSubnetGroupDoesNotCoverEnoughAZs :: Encode DBSubnetGroupDoesNotCoverEnoughAZs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBSubnetGroupMessage = DBSubnetGroupMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "DBSubnetGroups" :: NullOrUndefined.NullOrUndefined (DBSubnetGroups)
  }
derive instance newtypeDBSubnetGroupMessage :: Newtype DBSubnetGroupMessage _
derive instance repGenericDBSubnetGroupMessage :: Generic DBSubnetGroupMessage _
instance showDBSubnetGroupMessage :: Show DBSubnetGroupMessage where
  show = genericShow
instance decodeDBSubnetGroupMessage :: Decode DBSubnetGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSubnetGroupMessage :: Encode DBSubnetGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> <i>DBSubnetGroupName</i> does not refer to an existing DB subnet group. </p>
newtype DBSubnetGroupNotFoundFault = DBSubnetGroupNotFoundFault Types.NoArguments
derive instance newtypeDBSubnetGroupNotFoundFault :: Newtype DBSubnetGroupNotFoundFault _
derive instance repGenericDBSubnetGroupNotFoundFault :: Generic DBSubnetGroupNotFoundFault _
instance showDBSubnetGroupNotFoundFault :: Show DBSubnetGroupNotFoundFault where
  show = genericShow
instance decodeDBSubnetGroupNotFoundFault :: Decode DBSubnetGroupNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSubnetGroupNotFoundFault :: Encode DBSubnetGroupNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request would result in user exceeding the allowed number of DB subnet groups.</p>
newtype DBSubnetGroupQuotaExceededFault = DBSubnetGroupQuotaExceededFault Types.NoArguments
derive instance newtypeDBSubnetGroupQuotaExceededFault :: Newtype DBSubnetGroupQuotaExceededFault _
derive instance repGenericDBSubnetGroupQuotaExceededFault :: Generic DBSubnetGroupQuotaExceededFault _
instance showDBSubnetGroupQuotaExceededFault :: Show DBSubnetGroupQuotaExceededFault where
  show = genericShow
instance decodeDBSubnetGroupQuotaExceededFault :: Decode DBSubnetGroupQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSubnetGroupQuotaExceededFault :: Encode DBSubnetGroupQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DBSubnetGroups = DBSubnetGroups (Array DBSubnetGroup)
derive instance newtypeDBSubnetGroups :: Newtype DBSubnetGroups _
derive instance repGenericDBSubnetGroups :: Generic DBSubnetGroups _
instance showDBSubnetGroups :: Show DBSubnetGroups where
  show = genericShow
instance decodeDBSubnetGroups :: Decode DBSubnetGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSubnetGroups :: Encode DBSubnetGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request would result in user exceeding the allowed number of subnets in a DB subnet groups.</p>
newtype DBSubnetQuotaExceededFault = DBSubnetQuotaExceededFault Types.NoArguments
derive instance newtypeDBSubnetQuotaExceededFault :: Newtype DBSubnetQuotaExceededFault _
derive instance repGenericDBSubnetQuotaExceededFault :: Generic DBSubnetQuotaExceededFault _
instance showDBSubnetQuotaExceededFault :: Show DBSubnetQuotaExceededFault where
  show = genericShow
instance decodeDBSubnetQuotaExceededFault :: Decode DBSubnetQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBSubnetQuotaExceededFault :: Encode DBSubnetQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The DB upgrade failed because a resource the DB depends on could not be modified.</p>
newtype DBUpgradeDependencyFailureFault = DBUpgradeDependencyFailureFault Types.NoArguments
derive instance newtypeDBUpgradeDependencyFailureFault :: Newtype DBUpgradeDependencyFailureFault _
derive instance repGenericDBUpgradeDependencyFailureFault :: Generic DBUpgradeDependencyFailureFault _
instance showDBUpgradeDependencyFailureFault :: Show DBUpgradeDependencyFailureFault where
  show = genericShow
instance decodeDBUpgradeDependencyFailureFault :: Decode DBUpgradeDependencyFailureFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDBUpgradeDependencyFailureFault :: Encode DBUpgradeDependencyFailureFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDBInstanceMessage = DeleteDBInstanceMessage 
  { "DBInstanceIdentifier" :: (String)
  , "SkipFinalSnapshot" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "FinalDBSnapshotIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDeleteDBInstanceMessage :: Newtype DeleteDBInstanceMessage _
derive instance repGenericDeleteDBInstanceMessage :: Generic DeleteDBInstanceMessage _
instance showDeleteDBInstanceMessage :: Show DeleteDBInstanceMessage where
  show = genericShow
instance decodeDeleteDBInstanceMessage :: Decode DeleteDBInstanceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDBInstanceMessage :: Encode DeleteDBInstanceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDBInstanceResult = DeleteDBInstanceResult 
  { "DBInstance" :: NullOrUndefined.NullOrUndefined (DBInstance)
  }
derive instance newtypeDeleteDBInstanceResult :: Newtype DeleteDBInstanceResult _
derive instance repGenericDeleteDBInstanceResult :: Generic DeleteDBInstanceResult _
instance showDeleteDBInstanceResult :: Show DeleteDBInstanceResult where
  show = genericShow
instance decodeDeleteDBInstanceResult :: Decode DeleteDBInstanceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDBInstanceResult :: Encode DeleteDBInstanceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDBParameterGroupMessage = DeleteDBParameterGroupMessage 
  { "DBParameterGroupName" :: (String)
  }
derive instance newtypeDeleteDBParameterGroupMessage :: Newtype DeleteDBParameterGroupMessage _
derive instance repGenericDeleteDBParameterGroupMessage :: Generic DeleteDBParameterGroupMessage _
instance showDeleteDBParameterGroupMessage :: Show DeleteDBParameterGroupMessage where
  show = genericShow
instance decodeDeleteDBParameterGroupMessage :: Decode DeleteDBParameterGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDBParameterGroupMessage :: Encode DeleteDBParameterGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDBSecurityGroupMessage = DeleteDBSecurityGroupMessage 
  { "DBSecurityGroupName" :: (String)
  }
derive instance newtypeDeleteDBSecurityGroupMessage :: Newtype DeleteDBSecurityGroupMessage _
derive instance repGenericDeleteDBSecurityGroupMessage :: Generic DeleteDBSecurityGroupMessage _
instance showDeleteDBSecurityGroupMessage :: Show DeleteDBSecurityGroupMessage where
  show = genericShow
instance decodeDeleteDBSecurityGroupMessage :: Decode DeleteDBSecurityGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDBSecurityGroupMessage :: Encode DeleteDBSecurityGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDBSnapshotMessage = DeleteDBSnapshotMessage 
  { "DBSnapshotIdentifier" :: (String)
  }
derive instance newtypeDeleteDBSnapshotMessage :: Newtype DeleteDBSnapshotMessage _
derive instance repGenericDeleteDBSnapshotMessage :: Generic DeleteDBSnapshotMessage _
instance showDeleteDBSnapshotMessage :: Show DeleteDBSnapshotMessage where
  show = genericShow
instance decodeDeleteDBSnapshotMessage :: Decode DeleteDBSnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDBSnapshotMessage :: Encode DeleteDBSnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDBSnapshotResult = DeleteDBSnapshotResult 
  { "DBSnapshot" :: NullOrUndefined.NullOrUndefined (DBSnapshot)
  }
derive instance newtypeDeleteDBSnapshotResult :: Newtype DeleteDBSnapshotResult _
derive instance repGenericDeleteDBSnapshotResult :: Generic DeleteDBSnapshotResult _
instance showDeleteDBSnapshotResult :: Show DeleteDBSnapshotResult where
  show = genericShow
instance decodeDeleteDBSnapshotResult :: Decode DeleteDBSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDBSnapshotResult :: Encode DeleteDBSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDBSubnetGroupMessage = DeleteDBSubnetGroupMessage 
  { "DBSubnetGroupName" :: (String)
  }
derive instance newtypeDeleteDBSubnetGroupMessage :: Newtype DeleteDBSubnetGroupMessage _
derive instance repGenericDeleteDBSubnetGroupMessage :: Generic DeleteDBSubnetGroupMessage _
instance showDeleteDBSubnetGroupMessage :: Show DeleteDBSubnetGroupMessage where
  show = genericShow
instance decodeDeleteDBSubnetGroupMessage :: Decode DeleteDBSubnetGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDBSubnetGroupMessage :: Encode DeleteDBSubnetGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteEventSubscriptionMessage = DeleteEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  }
derive instance newtypeDeleteEventSubscriptionMessage :: Newtype DeleteEventSubscriptionMessage _
derive instance repGenericDeleteEventSubscriptionMessage :: Generic DeleteEventSubscriptionMessage _
instance showDeleteEventSubscriptionMessage :: Show DeleteEventSubscriptionMessage where
  show = genericShow
instance decodeDeleteEventSubscriptionMessage :: Decode DeleteEventSubscriptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEventSubscriptionMessage :: Encode DeleteEventSubscriptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteEventSubscriptionResult = DeleteEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined.NullOrUndefined (EventSubscription)
  }
derive instance newtypeDeleteEventSubscriptionResult :: Newtype DeleteEventSubscriptionResult _
derive instance repGenericDeleteEventSubscriptionResult :: Generic DeleteEventSubscriptionResult _
instance showDeleteEventSubscriptionResult :: Show DeleteEventSubscriptionResult where
  show = genericShow
instance decodeDeleteEventSubscriptionResult :: Decode DeleteEventSubscriptionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEventSubscriptionResult :: Encode DeleteEventSubscriptionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteOptionGroupMessage = DeleteOptionGroupMessage 
  { "OptionGroupName" :: (String)
  }
derive instance newtypeDeleteOptionGroupMessage :: Newtype DeleteOptionGroupMessage _
derive instance repGenericDeleteOptionGroupMessage :: Generic DeleteOptionGroupMessage _
instance showDeleteOptionGroupMessage :: Show DeleteOptionGroupMessage where
  show = genericShow
instance decodeDeleteOptionGroupMessage :: Decode DeleteOptionGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteOptionGroupMessage :: Encode DeleteOptionGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDBEngineVersionsMessage = DescribeDBEngineVersionsMessage 
  { "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "DBParameterGroupFamily" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultOnly" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ListSupportedCharacterSets" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeDescribeDBEngineVersionsMessage :: Newtype DescribeDBEngineVersionsMessage _
derive instance repGenericDescribeDBEngineVersionsMessage :: Generic DescribeDBEngineVersionsMessage _
instance showDescribeDBEngineVersionsMessage :: Show DescribeDBEngineVersionsMessage where
  show = genericShow
instance decodeDescribeDBEngineVersionsMessage :: Decode DescribeDBEngineVersionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDBEngineVersionsMessage :: Encode DescribeDBEngineVersionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDBInstancesMessage = DescribeDBInstancesMessage 
  { "DBInstanceIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeDBInstancesMessage :: Newtype DescribeDBInstancesMessage _
derive instance repGenericDescribeDBInstancesMessage :: Generic DescribeDBInstancesMessage _
instance showDescribeDBInstancesMessage :: Show DescribeDBInstancesMessage where
  show = genericShow
instance decodeDescribeDBInstancesMessage :: Decode DescribeDBInstancesMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDBInstancesMessage :: Encode DescribeDBInstancesMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDBParameterGroupsMessage = DescribeDBParameterGroupsMessage 
  { "DBParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeDBParameterGroupsMessage :: Newtype DescribeDBParameterGroupsMessage _
derive instance repGenericDescribeDBParameterGroupsMessage :: Generic DescribeDBParameterGroupsMessage _
instance showDescribeDBParameterGroupsMessage :: Show DescribeDBParameterGroupsMessage where
  show = genericShow
instance decodeDescribeDBParameterGroupsMessage :: Decode DescribeDBParameterGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDBParameterGroupsMessage :: Encode DescribeDBParameterGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDBParametersMessage = DescribeDBParametersMessage 
  { "DBParameterGroupName" :: (String)
  , "Source" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeDBParametersMessage :: Newtype DescribeDBParametersMessage _
derive instance repGenericDescribeDBParametersMessage :: Generic DescribeDBParametersMessage _
instance showDescribeDBParametersMessage :: Show DescribeDBParametersMessage where
  show = genericShow
instance decodeDescribeDBParametersMessage :: Decode DescribeDBParametersMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDBParametersMessage :: Encode DescribeDBParametersMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDBSecurityGroupsMessage = DescribeDBSecurityGroupsMessage 
  { "DBSecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeDBSecurityGroupsMessage :: Newtype DescribeDBSecurityGroupsMessage _
derive instance repGenericDescribeDBSecurityGroupsMessage :: Generic DescribeDBSecurityGroupsMessage _
instance showDescribeDBSecurityGroupsMessage :: Show DescribeDBSecurityGroupsMessage where
  show = genericShow
instance decodeDescribeDBSecurityGroupsMessage :: Decode DescribeDBSecurityGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDBSecurityGroupsMessage :: Encode DescribeDBSecurityGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDBSnapshotsMessage = DescribeDBSnapshotsMessage 
  { "DBInstanceIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "DBSnapshotIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SnapshotType" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeDBSnapshotsMessage :: Newtype DescribeDBSnapshotsMessage _
derive instance repGenericDescribeDBSnapshotsMessage :: Generic DescribeDBSnapshotsMessage _
instance showDescribeDBSnapshotsMessage :: Show DescribeDBSnapshotsMessage where
  show = genericShow
instance decodeDescribeDBSnapshotsMessage :: Decode DescribeDBSnapshotsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDBSnapshotsMessage :: Encode DescribeDBSnapshotsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDBSubnetGroupsMessage = DescribeDBSubnetGroupsMessage 
  { "DBSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeDBSubnetGroupsMessage :: Newtype DescribeDBSubnetGroupsMessage _
derive instance repGenericDescribeDBSubnetGroupsMessage :: Generic DescribeDBSubnetGroupsMessage _
instance showDescribeDBSubnetGroupsMessage :: Show DescribeDBSubnetGroupsMessage where
  show = genericShow
instance decodeDescribeDBSubnetGroupsMessage :: Decode DescribeDBSubnetGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDBSubnetGroupsMessage :: Encode DescribeDBSubnetGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEngineDefaultParametersMessage = DescribeEngineDefaultParametersMessage 
  { "DBParameterGroupFamily" :: (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeEngineDefaultParametersMessage :: Newtype DescribeEngineDefaultParametersMessage _
derive instance repGenericDescribeEngineDefaultParametersMessage :: Generic DescribeEngineDefaultParametersMessage _
instance showDescribeEngineDefaultParametersMessage :: Show DescribeEngineDefaultParametersMessage where
  show = genericShow
instance decodeDescribeEngineDefaultParametersMessage :: Decode DescribeEngineDefaultParametersMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEngineDefaultParametersMessage :: Encode DescribeEngineDefaultParametersMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEngineDefaultParametersResult = DescribeEngineDefaultParametersResult 
  { "EngineDefaults" :: NullOrUndefined.NullOrUndefined (EngineDefaults)
  }
derive instance newtypeDescribeEngineDefaultParametersResult :: Newtype DescribeEngineDefaultParametersResult _
derive instance repGenericDescribeEngineDefaultParametersResult :: Generic DescribeEngineDefaultParametersResult _
instance showDescribeEngineDefaultParametersResult :: Show DescribeEngineDefaultParametersResult where
  show = genericShow
instance decodeDescribeEngineDefaultParametersResult :: Decode DescribeEngineDefaultParametersResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEngineDefaultParametersResult :: Encode DescribeEngineDefaultParametersResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventCategoriesMessage = DescribeEventCategoriesMessage 
  { "SourceType" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeEventCategoriesMessage :: Newtype DescribeEventCategoriesMessage _
derive instance repGenericDescribeEventCategoriesMessage :: Generic DescribeEventCategoriesMessage _
instance showDescribeEventCategoriesMessage :: Show DescribeEventCategoriesMessage where
  show = genericShow
instance decodeDescribeEventCategoriesMessage :: Decode DescribeEventCategoriesMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventCategoriesMessage :: Encode DescribeEventCategoriesMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventSubscriptionsMessage = DescribeEventSubscriptionsMessage 
  { "SubscriptionName" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeEventSubscriptionsMessage :: Newtype DescribeEventSubscriptionsMessage _
derive instance repGenericDescribeEventSubscriptionsMessage :: Generic DescribeEventSubscriptionsMessage _
instance showDescribeEventSubscriptionsMessage :: Show DescribeEventSubscriptionsMessage where
  show = genericShow
instance decodeDescribeEventSubscriptionsMessage :: Decode DescribeEventSubscriptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventSubscriptionsMessage :: Encode DescribeEventSubscriptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventsMessage = DescribeEventsMessage 
  { "SourceIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (SourceType)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "Duration" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "EventCategories" :: NullOrUndefined.NullOrUndefined (EventCategoriesList)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeEventsMessage :: Newtype DescribeEventsMessage _
derive instance repGenericDescribeEventsMessage :: Generic DescribeEventsMessage _
instance showDescribeEventsMessage :: Show DescribeEventsMessage where
  show = genericShow
instance decodeDescribeEventsMessage :: Decode DescribeEventsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventsMessage :: Encode DescribeEventsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeOptionGroupOptionsMessage = DescribeOptionGroupOptionsMessage 
  { "EngineName" :: (String)
  , "MajorEngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeOptionGroupOptionsMessage :: Newtype DescribeOptionGroupOptionsMessage _
derive instance repGenericDescribeOptionGroupOptionsMessage :: Generic DescribeOptionGroupOptionsMessage _
instance showDescribeOptionGroupOptionsMessage :: Show DescribeOptionGroupOptionsMessage where
  show = genericShow
instance decodeDescribeOptionGroupOptionsMessage :: Decode DescribeOptionGroupOptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeOptionGroupOptionsMessage :: Encode DescribeOptionGroupOptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeOptionGroupsMessage = DescribeOptionGroupsMessage 
  { "OptionGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "EngineName" :: NullOrUndefined.NullOrUndefined (String)
  , "MajorEngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeOptionGroupsMessage :: Newtype DescribeOptionGroupsMessage _
derive instance repGenericDescribeOptionGroupsMessage :: Generic DescribeOptionGroupsMessage _
instance showDescribeOptionGroupsMessage :: Show DescribeOptionGroupsMessage where
  show = genericShow
instance decodeDescribeOptionGroupsMessage :: Decode DescribeOptionGroupsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeOptionGroupsMessage :: Encode DescribeOptionGroupsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeOrderableDBInstanceOptionsMessage = DescribeOrderableDBInstanceOptionsMessage 
  { "Engine" :: (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined.NullOrUndefined (String)
  , "LicenseModel" :: NullOrUndefined.NullOrUndefined (String)
  , "Vpc" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeOrderableDBInstanceOptionsMessage :: Newtype DescribeOrderableDBInstanceOptionsMessage _
derive instance repGenericDescribeOrderableDBInstanceOptionsMessage :: Generic DescribeOrderableDBInstanceOptionsMessage _
instance showDescribeOrderableDBInstanceOptionsMessage :: Show DescribeOrderableDBInstanceOptionsMessage where
  show = genericShow
instance decodeDescribeOrderableDBInstanceOptionsMessage :: Decode DescribeOrderableDBInstanceOptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeOrderableDBInstanceOptionsMessage :: Encode DescribeOrderableDBInstanceOptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeReservedDBInstancesMessage = DescribeReservedDBInstancesMessage 
  { "ReservedDBInstanceId" :: NullOrUndefined.NullOrUndefined (String)
  , "ReservedDBInstancesOfferingId" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined.NullOrUndefined (String)
  , "Duration" :: NullOrUndefined.NullOrUndefined (String)
  , "ProductDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined.NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeReservedDBInstancesMessage :: Newtype DescribeReservedDBInstancesMessage _
derive instance repGenericDescribeReservedDBInstancesMessage :: Generic DescribeReservedDBInstancesMessage _
instance showDescribeReservedDBInstancesMessage :: Show DescribeReservedDBInstancesMessage where
  show = genericShow
instance decodeDescribeReservedDBInstancesMessage :: Decode DescribeReservedDBInstancesMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReservedDBInstancesMessage :: Encode DescribeReservedDBInstancesMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeReservedDBInstancesOfferingsMessage = DescribeReservedDBInstancesOfferingsMessage 
  { "ReservedDBInstancesOfferingId" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined.NullOrUndefined (String)
  , "Duration" :: NullOrUndefined.NullOrUndefined (String)
  , "ProductDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined.NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeReservedDBInstancesOfferingsMessage :: Newtype DescribeReservedDBInstancesOfferingsMessage _
derive instance repGenericDescribeReservedDBInstancesOfferingsMessage :: Generic DescribeReservedDBInstancesOfferingsMessage _
instance showDescribeReservedDBInstancesOfferingsMessage :: Show DescribeReservedDBInstancesOfferingsMessage where
  show = genericShow
instance decodeDescribeReservedDBInstancesOfferingsMessage :: Decode DescribeReservedDBInstancesOfferingsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReservedDBInstancesOfferingsMessage :: Encode DescribeReservedDBInstancesOfferingsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EC2SecurityGroup = EC2SecurityGroup 
  { "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEC2SecurityGroup :: Newtype EC2SecurityGroup _
derive instance repGenericEC2SecurityGroup :: Generic EC2SecurityGroup _
instance showEC2SecurityGroup :: Show EC2SecurityGroup where
  show = genericShow
instance decodeEC2SecurityGroup :: Decode EC2SecurityGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEC2SecurityGroup :: Encode EC2SecurityGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EC2SecurityGroupList = EC2SecurityGroupList (Array EC2SecurityGroup)
derive instance newtypeEC2SecurityGroupList :: Newtype EC2SecurityGroupList _
derive instance repGenericEC2SecurityGroupList :: Generic EC2SecurityGroupList _
instance showEC2SecurityGroupList :: Show EC2SecurityGroupList where
  show = genericShow
instance decodeEC2SecurityGroupList :: Decode EC2SecurityGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEC2SecurityGroupList :: Encode EC2SecurityGroupList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Endpoint = Endpoint 
  { "Address" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeEndpoint :: Newtype Endpoint _
derive instance repGenericEndpoint :: Generic Endpoint _
instance showEndpoint :: Show Endpoint where
  show = genericShow
instance decodeEndpoint :: Decode Endpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpoint :: Encode Endpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EngineDefaults = EngineDefaults 
  { "DBParameterGroupFamily" :: NullOrUndefined.NullOrUndefined (String)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersList)
  }
derive instance newtypeEngineDefaults :: Newtype EngineDefaults _
derive instance repGenericEngineDefaults :: Generic EngineDefaults _
instance showEngineDefaults :: Show EngineDefaults where
  show = genericShow
instance decodeEngineDefaults :: Decode EngineDefaults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEngineDefaults :: Encode EngineDefaults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Event = Event 
  { "SourceIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (SourceType)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined.NullOrUndefined (EventCategoriesList)
  , "Date" :: NullOrUndefined.NullOrUndefined (TStamp)
  }
derive instance newtypeEvent :: Newtype Event _
derive instance repGenericEvent :: Generic Event _
instance showEvent :: Show Event where
  show = genericShow
instance decodeEvent :: Decode Event where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvent :: Encode Event where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventCategoriesList = EventCategoriesList (Array String)
derive instance newtypeEventCategoriesList :: Newtype EventCategoriesList _
derive instance repGenericEventCategoriesList :: Generic EventCategoriesList _
instance showEventCategoriesList :: Show EventCategoriesList where
  show = genericShow
instance decodeEventCategoriesList :: Decode EventCategoriesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventCategoriesList :: Encode EventCategoriesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventCategoriesMap = EventCategoriesMap 
  { "SourceType" :: NullOrUndefined.NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined.NullOrUndefined (EventCategoriesList)
  }
derive instance newtypeEventCategoriesMap :: Newtype EventCategoriesMap _
derive instance repGenericEventCategoriesMap :: Generic EventCategoriesMap _
instance showEventCategoriesMap :: Show EventCategoriesMap where
  show = genericShow
instance decodeEventCategoriesMap :: Decode EventCategoriesMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventCategoriesMap :: Encode EventCategoriesMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventCategoriesMapList = EventCategoriesMapList (Array EventCategoriesMap)
derive instance newtypeEventCategoriesMapList :: Newtype EventCategoriesMapList _
derive instance repGenericEventCategoriesMapList :: Generic EventCategoriesMapList _
instance showEventCategoriesMapList :: Show EventCategoriesMapList where
  show = genericShow
instance decodeEventCategoriesMapList :: Decode EventCategoriesMapList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventCategoriesMapList :: Encode EventCategoriesMapList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventCategoriesMessage = EventCategoriesMessage 
  { "EventCategoriesMapList" :: NullOrUndefined.NullOrUndefined (EventCategoriesMapList)
  }
derive instance newtypeEventCategoriesMessage :: Newtype EventCategoriesMessage _
derive instance repGenericEventCategoriesMessage :: Generic EventCategoriesMessage _
instance showEventCategoriesMessage :: Show EventCategoriesMessage where
  show = genericShow
instance decodeEventCategoriesMessage :: Decode EventCategoriesMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventCategoriesMessage :: Encode EventCategoriesMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventList = EventList (Array Event)
derive instance newtypeEventList :: Newtype EventList _
derive instance repGenericEventList :: Generic EventList _
instance showEventList :: Show EventList where
  show = genericShow
instance decodeEventList :: Decode EventList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventList :: Encode EventList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventSubscription = EventSubscription 
  { "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "CustomerAwsId" :: NullOrUndefined.NullOrUndefined (String)
  , "CustSubscriptionId" :: NullOrUndefined.NullOrUndefined (String)
  , "SnsTopicArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "SubscriptionCreationTime" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceIdsList" :: NullOrUndefined.NullOrUndefined (SourceIdsList)
  , "EventCategoriesList" :: NullOrUndefined.NullOrUndefined (EventCategoriesList)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeEventSubscription :: Newtype EventSubscription _
derive instance repGenericEventSubscription :: Generic EventSubscription _
instance showEventSubscription :: Show EventSubscription where
  show = genericShow
instance decodeEventSubscription :: Decode EventSubscription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventSubscription :: Encode EventSubscription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have reached the maximum number of event subscriptions.</p>
newtype EventSubscriptionQuotaExceededFault = EventSubscriptionQuotaExceededFault Types.NoArguments
derive instance newtypeEventSubscriptionQuotaExceededFault :: Newtype EventSubscriptionQuotaExceededFault _
derive instance repGenericEventSubscriptionQuotaExceededFault :: Generic EventSubscriptionQuotaExceededFault _
instance showEventSubscriptionQuotaExceededFault :: Show EventSubscriptionQuotaExceededFault where
  show = genericShow
instance decodeEventSubscriptionQuotaExceededFault :: Decode EventSubscriptionQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventSubscriptionQuotaExceededFault :: Encode EventSubscriptionQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventSubscriptionsList = EventSubscriptionsList (Array EventSubscription)
derive instance newtypeEventSubscriptionsList :: Newtype EventSubscriptionsList _
derive instance repGenericEventSubscriptionsList :: Generic EventSubscriptionsList _
instance showEventSubscriptionsList :: Show EventSubscriptionsList where
  show = genericShow
instance decodeEventSubscriptionsList :: Decode EventSubscriptionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventSubscriptionsList :: Encode EventSubscriptionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventSubscriptionsMessage = EventSubscriptionsMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "EventSubscriptionsList" :: NullOrUndefined.NullOrUndefined (EventSubscriptionsList)
  }
derive instance newtypeEventSubscriptionsMessage :: Newtype EventSubscriptionsMessage _
derive instance repGenericEventSubscriptionsMessage :: Generic EventSubscriptionsMessage _
instance showEventSubscriptionsMessage :: Show EventSubscriptionsMessage where
  show = genericShow
instance decodeEventSubscriptionsMessage :: Decode EventSubscriptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventSubscriptionsMessage :: Encode EventSubscriptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventsMessage = EventsMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "Events" :: NullOrUndefined.NullOrUndefined (EventList)
  }
derive instance newtypeEventsMessage :: Newtype EventsMessage _
derive instance repGenericEventsMessage :: Generic EventsMessage _
instance showEventsMessage :: Show EventsMessage where
  show = genericShow
instance decodeEventsMessage :: Decode EventsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventsMessage :: Encode EventsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPRange = IPRange 
  { "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "CIDRIP" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeIPRange :: Newtype IPRange _
derive instance repGenericIPRange :: Generic IPRange _
instance showIPRange :: Show IPRange where
  show = genericShow
instance decodeIPRange :: Decode IPRange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPRange :: Encode IPRange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPRangeList = IPRangeList (Array IPRange)
derive instance newtypeIPRangeList :: Newtype IPRangeList _
derive instance repGenericIPRangeList :: Generic IPRangeList _
instance showIPRangeList :: Show IPRangeList where
  show = genericShow
instance decodeIPRangeList :: Decode IPRangeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPRangeList :: Encode IPRangeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request would result in user exceeding the allowed number of DB instances.</p>
newtype InstanceQuotaExceededFault = InstanceQuotaExceededFault Types.NoArguments
derive instance newtypeInstanceQuotaExceededFault :: Newtype InstanceQuotaExceededFault _
derive instance repGenericInstanceQuotaExceededFault :: Generic InstanceQuotaExceededFault _
instance showInstanceQuotaExceededFault :: Show InstanceQuotaExceededFault where
  show = genericShow
instance decodeInstanceQuotaExceededFault :: Decode InstanceQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceQuotaExceededFault :: Encode InstanceQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specified DB instance class is not available in the specified Availability Zone.</p>
newtype InsufficientDBInstanceCapacityFault = InsufficientDBInstanceCapacityFault Types.NoArguments
derive instance newtypeInsufficientDBInstanceCapacityFault :: Newtype InsufficientDBInstanceCapacityFault _
derive instance repGenericInsufficientDBInstanceCapacityFault :: Generic InsufficientDBInstanceCapacityFault _
instance showInsufficientDBInstanceCapacityFault :: Show InsufficientDBInstanceCapacityFault where
  show = genericShow
instance decodeInsufficientDBInstanceCapacityFault :: Decode InsufficientDBInstanceCapacityFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientDBInstanceCapacityFault :: Encode InsufficientDBInstanceCapacityFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntegerOptional = IntegerOptional Int
derive instance newtypeIntegerOptional :: Newtype IntegerOptional _
derive instance repGenericIntegerOptional :: Generic IntegerOptional _
instance showIntegerOptional :: Show IntegerOptional where
  show = genericShow
instance decodeIntegerOptional :: Decode IntegerOptional where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntegerOptional :: Encode IntegerOptional where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The specified DB instance is not in the <i>available</i> state. </p>
newtype InvalidDBInstanceStateFault = InvalidDBInstanceStateFault Types.NoArguments
derive instance newtypeInvalidDBInstanceStateFault :: Newtype InvalidDBInstanceStateFault _
derive instance repGenericInvalidDBInstanceStateFault :: Generic InvalidDBInstanceStateFault _
instance showInvalidDBInstanceStateFault :: Show InvalidDBInstanceStateFault where
  show = genericShow
instance decodeInvalidDBInstanceStateFault :: Decode InvalidDBInstanceStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDBInstanceStateFault :: Encode InvalidDBInstanceStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The DB parameter group is in use or is in an invalid state. If you are attempting to delete the parameter group, you cannot delete it when the parameter group is in this state.</p>
newtype InvalidDBParameterGroupStateFault = InvalidDBParameterGroupStateFault Types.NoArguments
derive instance newtypeInvalidDBParameterGroupStateFault :: Newtype InvalidDBParameterGroupStateFault _
derive instance repGenericInvalidDBParameterGroupStateFault :: Generic InvalidDBParameterGroupStateFault _
instance showInvalidDBParameterGroupStateFault :: Show InvalidDBParameterGroupStateFault where
  show = genericShow
instance decodeInvalidDBParameterGroupStateFault :: Decode InvalidDBParameterGroupStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDBParameterGroupStateFault :: Encode InvalidDBParameterGroupStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The state of the DB security group does not allow deletion.</p>
newtype InvalidDBSecurityGroupStateFault = InvalidDBSecurityGroupStateFault Types.NoArguments
derive instance newtypeInvalidDBSecurityGroupStateFault :: Newtype InvalidDBSecurityGroupStateFault _
derive instance repGenericInvalidDBSecurityGroupStateFault :: Generic InvalidDBSecurityGroupStateFault _
instance showInvalidDBSecurityGroupStateFault :: Show InvalidDBSecurityGroupStateFault where
  show = genericShow
instance decodeInvalidDBSecurityGroupStateFault :: Decode InvalidDBSecurityGroupStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDBSecurityGroupStateFault :: Encode InvalidDBSecurityGroupStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The state of the DB snapshot does not allow deletion.</p>
newtype InvalidDBSnapshotStateFault = InvalidDBSnapshotStateFault Types.NoArguments
derive instance newtypeInvalidDBSnapshotStateFault :: Newtype InvalidDBSnapshotStateFault _
derive instance repGenericInvalidDBSnapshotStateFault :: Generic InvalidDBSnapshotStateFault _
instance showInvalidDBSnapshotStateFault :: Show InvalidDBSnapshotStateFault where
  show = genericShow
instance decodeInvalidDBSnapshotStateFault :: Decode InvalidDBSnapshotStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDBSnapshotStateFault :: Encode InvalidDBSnapshotStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The DB subnet group cannot be deleted because it is in use.</p>
newtype InvalidDBSubnetGroupStateFault = InvalidDBSubnetGroupStateFault Types.NoArguments
derive instance newtypeInvalidDBSubnetGroupStateFault :: Newtype InvalidDBSubnetGroupStateFault _
derive instance repGenericInvalidDBSubnetGroupStateFault :: Generic InvalidDBSubnetGroupStateFault _
instance showInvalidDBSubnetGroupStateFault :: Show InvalidDBSubnetGroupStateFault where
  show = genericShow
instance decodeInvalidDBSubnetGroupStateFault :: Decode InvalidDBSubnetGroupStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDBSubnetGroupStateFault :: Encode InvalidDBSubnetGroupStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The DB subnet is not in the <i>available</i> state. </p>
newtype InvalidDBSubnetStateFault = InvalidDBSubnetStateFault Types.NoArguments
derive instance newtypeInvalidDBSubnetStateFault :: Newtype InvalidDBSubnetStateFault _
derive instance repGenericInvalidDBSubnetStateFault :: Generic InvalidDBSubnetStateFault _
instance showInvalidDBSubnetStateFault :: Show InvalidDBSubnetStateFault where
  show = genericShow
instance decodeInvalidDBSubnetStateFault :: Decode InvalidDBSubnetStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDBSubnetStateFault :: Encode InvalidDBSubnetStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This error can occur if someone else is modifying a subscription. You should retry the action.</p>
newtype InvalidEventSubscriptionStateFault = InvalidEventSubscriptionStateFault Types.NoArguments
derive instance newtypeInvalidEventSubscriptionStateFault :: Newtype InvalidEventSubscriptionStateFault _
derive instance repGenericInvalidEventSubscriptionStateFault :: Generic InvalidEventSubscriptionStateFault _
instance showInvalidEventSubscriptionStateFault :: Show InvalidEventSubscriptionStateFault where
  show = genericShow
instance decodeInvalidEventSubscriptionStateFault :: Decode InvalidEventSubscriptionStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidEventSubscriptionStateFault :: Encode InvalidEventSubscriptionStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The option group is not in the <i>available</i> state. </p>
newtype InvalidOptionGroupStateFault = InvalidOptionGroupStateFault Types.NoArguments
derive instance newtypeInvalidOptionGroupStateFault :: Newtype InvalidOptionGroupStateFault _
derive instance repGenericInvalidOptionGroupStateFault :: Generic InvalidOptionGroupStateFault _
instance showInvalidOptionGroupStateFault :: Show InvalidOptionGroupStateFault where
  show = genericShow
instance decodeInvalidOptionGroupStateFault :: Decode InvalidOptionGroupStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidOptionGroupStateFault :: Encode InvalidOptionGroupStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Cannot restore from vpc backup to non-vpc DB instance.</p>
newtype InvalidRestoreFault = InvalidRestoreFault Types.NoArguments
derive instance newtypeInvalidRestoreFault :: Newtype InvalidRestoreFault _
derive instance repGenericInvalidRestoreFault :: Generic InvalidRestoreFault _
instance showInvalidRestoreFault :: Show InvalidRestoreFault where
  show = genericShow
instance decodeInvalidRestoreFault :: Decode InvalidRestoreFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRestoreFault :: Encode InvalidRestoreFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested subnet is invalid, or multiple subnets were requested that are not all in a common VPC.</p>
newtype InvalidSubnet = InvalidSubnet Types.NoArguments
derive instance newtypeInvalidSubnet :: Newtype InvalidSubnet _
derive instance repGenericInvalidSubnet :: Generic InvalidSubnet _
instance showInvalidSubnet :: Show InvalidSubnet where
  show = genericShow
instance decodeInvalidSubnet :: Decode InvalidSubnet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSubnet :: Encode InvalidSubnet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>DB subnet group does not cover all Availability Zones after it is created because users' change.</p>
newtype InvalidVPCNetworkStateFault = InvalidVPCNetworkStateFault Types.NoArguments
derive instance newtypeInvalidVPCNetworkStateFault :: Newtype InvalidVPCNetworkStateFault _
derive instance repGenericInvalidVPCNetworkStateFault :: Generic InvalidVPCNetworkStateFault _
instance showInvalidVPCNetworkStateFault :: Show InvalidVPCNetworkStateFault where
  show = genericShow
instance decodeInvalidVPCNetworkStateFault :: Decode InvalidVPCNetworkStateFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidVPCNetworkStateFault :: Encode InvalidVPCNetworkStateFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyList = KeyList (Array String)
derive instance newtypeKeyList :: Newtype KeyList _
derive instance repGenericKeyList :: Generic KeyList _
instance showKeyList :: Show KeyList where
  show = genericShow
instance decodeKeyList :: Decode KeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyList :: Encode KeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsForResourceMessage = ListTagsForResourceMessage 
  { "ResourceName" :: (String)
  }
derive instance newtypeListTagsForResourceMessage :: Newtype ListTagsForResourceMessage _
derive instance repGenericListTagsForResourceMessage :: Generic ListTagsForResourceMessage _
instance showListTagsForResourceMessage :: Show ListTagsForResourceMessage where
  show = genericShow
instance decodeListTagsForResourceMessage :: Decode ListTagsForResourceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceMessage :: Encode ListTagsForResourceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyDBInstanceMessage = ModifyDBInstanceMessage 
  { "DBInstanceIdentifier" :: (String)
  , "AllocatedStorage" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "DBInstanceClass" :: NullOrUndefined.NullOrUndefined (String)
  , "DBSecurityGroups" :: NullOrUndefined.NullOrUndefined (DBSecurityGroupNameList)
  , "VpcSecurityGroupIds" :: NullOrUndefined.NullOrUndefined (VpcSecurityGroupIdList)
  , "ApplyImmediately" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "MasterUserPassword" :: NullOrUndefined.NullOrUndefined (String)
  , "DBParameterGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "BackupRetentionPeriod" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "PreferredBackupWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "PreferredMaintenanceWindow" :: NullOrUndefined.NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "AllowMajorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "Iops" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "OptionGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "NewDBInstanceIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeModifyDBInstanceMessage :: Newtype ModifyDBInstanceMessage _
derive instance repGenericModifyDBInstanceMessage :: Generic ModifyDBInstanceMessage _
instance showModifyDBInstanceMessage :: Show ModifyDBInstanceMessage where
  show = genericShow
instance decodeModifyDBInstanceMessage :: Decode ModifyDBInstanceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyDBInstanceMessage :: Encode ModifyDBInstanceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyDBInstanceResult = ModifyDBInstanceResult 
  { "DBInstance" :: NullOrUndefined.NullOrUndefined (DBInstance)
  }
derive instance newtypeModifyDBInstanceResult :: Newtype ModifyDBInstanceResult _
derive instance repGenericModifyDBInstanceResult :: Generic ModifyDBInstanceResult _
instance showModifyDBInstanceResult :: Show ModifyDBInstanceResult where
  show = genericShow
instance decodeModifyDBInstanceResult :: Decode ModifyDBInstanceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyDBInstanceResult :: Encode ModifyDBInstanceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyDBParameterGroupMessage = ModifyDBParameterGroupMessage 
  { "DBParameterGroupName" :: (String)
  , "Parameters" :: (ParametersList)
  }
derive instance newtypeModifyDBParameterGroupMessage :: Newtype ModifyDBParameterGroupMessage _
derive instance repGenericModifyDBParameterGroupMessage :: Generic ModifyDBParameterGroupMessage _
instance showModifyDBParameterGroupMessage :: Show ModifyDBParameterGroupMessage where
  show = genericShow
instance decodeModifyDBParameterGroupMessage :: Decode ModifyDBParameterGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyDBParameterGroupMessage :: Encode ModifyDBParameterGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyDBSubnetGroupMessage = ModifyDBSubnetGroupMessage 
  { "DBSubnetGroupName" :: (String)
  , "DBSubnetGroupDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "SubnetIds" :: (SubnetIdentifierList)
  }
derive instance newtypeModifyDBSubnetGroupMessage :: Newtype ModifyDBSubnetGroupMessage _
derive instance repGenericModifyDBSubnetGroupMessage :: Generic ModifyDBSubnetGroupMessage _
instance showModifyDBSubnetGroupMessage :: Show ModifyDBSubnetGroupMessage where
  show = genericShow
instance decodeModifyDBSubnetGroupMessage :: Decode ModifyDBSubnetGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyDBSubnetGroupMessage :: Encode ModifyDBSubnetGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyDBSubnetGroupResult = ModifyDBSubnetGroupResult 
  { "DBSubnetGroup" :: NullOrUndefined.NullOrUndefined (DBSubnetGroup)
  }
derive instance newtypeModifyDBSubnetGroupResult :: Newtype ModifyDBSubnetGroupResult _
derive instance repGenericModifyDBSubnetGroupResult :: Generic ModifyDBSubnetGroupResult _
instance showModifyDBSubnetGroupResult :: Show ModifyDBSubnetGroupResult where
  show = genericShow
instance decodeModifyDBSubnetGroupResult :: Decode ModifyDBSubnetGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyDBSubnetGroupResult :: Encode ModifyDBSubnetGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyEventSubscriptionMessage = ModifyEventSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SnsTopicArn" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (String)
  , "EventCategories" :: NullOrUndefined.NullOrUndefined (EventCategoriesList)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeModifyEventSubscriptionMessage :: Newtype ModifyEventSubscriptionMessage _
derive instance repGenericModifyEventSubscriptionMessage :: Generic ModifyEventSubscriptionMessage _
instance showModifyEventSubscriptionMessage :: Show ModifyEventSubscriptionMessage where
  show = genericShow
instance decodeModifyEventSubscriptionMessage :: Decode ModifyEventSubscriptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyEventSubscriptionMessage :: Encode ModifyEventSubscriptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyEventSubscriptionResult = ModifyEventSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined.NullOrUndefined (EventSubscription)
  }
derive instance newtypeModifyEventSubscriptionResult :: Newtype ModifyEventSubscriptionResult _
derive instance repGenericModifyEventSubscriptionResult :: Generic ModifyEventSubscriptionResult _
instance showModifyEventSubscriptionResult :: Show ModifyEventSubscriptionResult where
  show = genericShow
instance decodeModifyEventSubscriptionResult :: Decode ModifyEventSubscriptionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyEventSubscriptionResult :: Encode ModifyEventSubscriptionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyOptionGroupMessage = ModifyOptionGroupMessage 
  { "OptionGroupName" :: (String)
  , "OptionsToInclude" :: NullOrUndefined.NullOrUndefined (OptionConfigurationList)
  , "OptionsToRemove" :: NullOrUndefined.NullOrUndefined (OptionNamesList)
  , "ApplyImmediately" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeModifyOptionGroupMessage :: Newtype ModifyOptionGroupMessage _
derive instance repGenericModifyOptionGroupMessage :: Generic ModifyOptionGroupMessage _
instance showModifyOptionGroupMessage :: Show ModifyOptionGroupMessage where
  show = genericShow
instance decodeModifyOptionGroupMessage :: Decode ModifyOptionGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyOptionGroupMessage :: Encode ModifyOptionGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyOptionGroupResult = ModifyOptionGroupResult 
  { "OptionGroup" :: NullOrUndefined.NullOrUndefined (OptionGroup)
  }
derive instance newtypeModifyOptionGroupResult :: Newtype ModifyOptionGroupResult _
derive instance repGenericModifyOptionGroupResult :: Generic ModifyOptionGroupResult _
instance showModifyOptionGroupResult :: Show ModifyOptionGroupResult where
  show = genericShow
instance decodeModifyOptionGroupResult :: Decode ModifyOptionGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyOptionGroupResult :: Encode ModifyOptionGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Option = Option 
  { "OptionName" :: NullOrUndefined.NullOrUndefined (String)
  , "OptionDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "DBSecurityGroupMemberships" :: NullOrUndefined.NullOrUndefined (DBSecurityGroupMembershipList)
  , "VpcSecurityGroupMemberships" :: NullOrUndefined.NullOrUndefined (VpcSecurityGroupMembershipList)
  }
derive instance newtypeOption :: Newtype Option _
derive instance repGenericOption :: Generic Option _
instance showOption :: Show Option where
  show = genericShow
instance decodeOption :: Decode Option where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOption :: Encode Option where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionConfiguration = OptionConfiguration 
  { "OptionName" :: (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "DBSecurityGroupMemberships" :: NullOrUndefined.NullOrUndefined (DBSecurityGroupNameList)
  , "VpcSecurityGroupMemberships" :: NullOrUndefined.NullOrUndefined (VpcSecurityGroupIdList)
  }
derive instance newtypeOptionConfiguration :: Newtype OptionConfiguration _
derive instance repGenericOptionConfiguration :: Generic OptionConfiguration _
instance showOptionConfiguration :: Show OptionConfiguration where
  show = genericShow
instance decodeOptionConfiguration :: Decode OptionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionConfiguration :: Encode OptionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionConfigurationList = OptionConfigurationList (Array OptionConfiguration)
derive instance newtypeOptionConfigurationList :: Newtype OptionConfigurationList _
derive instance repGenericOptionConfigurationList :: Generic OptionConfigurationList _
instance showOptionConfigurationList :: Show OptionConfigurationList where
  show = genericShow
instance decodeOptionConfigurationList :: Decode OptionConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionConfigurationList :: Encode OptionConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionGroup = OptionGroup 
  { "OptionGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "OptionGroupDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineName" :: NullOrUndefined.NullOrUndefined (String)
  , "MajorEngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "Options" :: NullOrUndefined.NullOrUndefined (OptionsList)
  , "AllowsVpcAndNonVpcInstanceMemberships" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOptionGroup :: Newtype OptionGroup _
derive instance repGenericOptionGroup :: Generic OptionGroup _
instance showOptionGroup :: Show OptionGroup where
  show = genericShow
instance decodeOptionGroup :: Decode OptionGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionGroup :: Encode OptionGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The option group you are trying to create already exists.</p>
newtype OptionGroupAlreadyExistsFault = OptionGroupAlreadyExistsFault Types.NoArguments
derive instance newtypeOptionGroupAlreadyExistsFault :: Newtype OptionGroupAlreadyExistsFault _
derive instance repGenericOptionGroupAlreadyExistsFault :: Generic OptionGroupAlreadyExistsFault _
instance showOptionGroupAlreadyExistsFault :: Show OptionGroupAlreadyExistsFault where
  show = genericShow
instance decodeOptionGroupAlreadyExistsFault :: Decode OptionGroupAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionGroupAlreadyExistsFault :: Encode OptionGroupAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionGroupMembership = OptionGroupMembership 
  { "OptionGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOptionGroupMembership :: Newtype OptionGroupMembership _
derive instance repGenericOptionGroupMembership :: Generic OptionGroupMembership _
instance showOptionGroupMembership :: Show OptionGroupMembership where
  show = genericShow
instance decodeOptionGroupMembership :: Decode OptionGroupMembership where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionGroupMembership :: Encode OptionGroupMembership where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified option group could not be found.</p>
newtype OptionGroupNotFoundFault = OptionGroupNotFoundFault Types.NoArguments
derive instance newtypeOptionGroupNotFoundFault :: Newtype OptionGroupNotFoundFault _
derive instance repGenericOptionGroupNotFoundFault :: Generic OptionGroupNotFoundFault _
instance showOptionGroupNotFoundFault :: Show OptionGroupNotFoundFault where
  show = genericShow
instance decodeOptionGroupNotFoundFault :: Decode OptionGroupNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionGroupNotFoundFault :: Encode OptionGroupNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionGroupOption = OptionGroupOption 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineName" :: NullOrUndefined.NullOrUndefined (String)
  , "MajorEngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "MinimumRequiredMinorEngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "PortRequired" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "DefaultPort" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "OptionsDependedOn" :: NullOrUndefined.NullOrUndefined (OptionsDependedOn)
  }
derive instance newtypeOptionGroupOption :: Newtype OptionGroupOption _
derive instance repGenericOptionGroupOption :: Generic OptionGroupOption _
instance showOptionGroupOption :: Show OptionGroupOption where
  show = genericShow
instance decodeOptionGroupOption :: Decode OptionGroupOption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionGroupOption :: Encode OptionGroupOption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionGroupOptionsList = OptionGroupOptionsList (Array OptionGroupOption)
derive instance newtypeOptionGroupOptionsList :: Newtype OptionGroupOptionsList _
derive instance repGenericOptionGroupOptionsList :: Generic OptionGroupOptionsList _
instance showOptionGroupOptionsList :: Show OptionGroupOptionsList where
  show = genericShow
instance decodeOptionGroupOptionsList :: Decode OptionGroupOptionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionGroupOptionsList :: Encode OptionGroupOptionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionGroupOptionsMessage = OptionGroupOptionsMessage 
  { "OptionGroupOptions" :: NullOrUndefined.NullOrUndefined (OptionGroupOptionsList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOptionGroupOptionsMessage :: Newtype OptionGroupOptionsMessage _
derive instance repGenericOptionGroupOptionsMessage :: Generic OptionGroupOptionsMessage _
instance showOptionGroupOptionsMessage :: Show OptionGroupOptionsMessage where
  show = genericShow
instance decodeOptionGroupOptionsMessage :: Decode OptionGroupOptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionGroupOptionsMessage :: Encode OptionGroupOptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The quota of 20 option groups was exceeded for this AWS account.</p>
newtype OptionGroupQuotaExceededFault = OptionGroupQuotaExceededFault Types.NoArguments
derive instance newtypeOptionGroupQuotaExceededFault :: Newtype OptionGroupQuotaExceededFault _
derive instance repGenericOptionGroupQuotaExceededFault :: Generic OptionGroupQuotaExceededFault _
instance showOptionGroupQuotaExceededFault :: Show OptionGroupQuotaExceededFault where
  show = genericShow
instance decodeOptionGroupQuotaExceededFault :: Decode OptionGroupQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionGroupQuotaExceededFault :: Encode OptionGroupQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionGroups = OptionGroups 
  { "OptionGroupsList" :: NullOrUndefined.NullOrUndefined (OptionGroupsList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOptionGroups :: Newtype OptionGroups _
derive instance repGenericOptionGroups :: Generic OptionGroups _
instance showOptionGroups :: Show OptionGroups where
  show = genericShow
instance decodeOptionGroups :: Decode OptionGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionGroups :: Encode OptionGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionGroupsList = OptionGroupsList (Array OptionGroup)
derive instance newtypeOptionGroupsList :: Newtype OptionGroupsList _
derive instance repGenericOptionGroupsList :: Generic OptionGroupsList _
instance showOptionGroupsList :: Show OptionGroupsList where
  show = genericShow
instance decodeOptionGroupsList :: Decode OptionGroupsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionGroupsList :: Encode OptionGroupsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionNamesList = OptionNamesList (Array String)
derive instance newtypeOptionNamesList :: Newtype OptionNamesList _
derive instance repGenericOptionNamesList :: Generic OptionNamesList _
instance showOptionNamesList :: Show OptionNamesList where
  show = genericShow
instance decodeOptionNamesList :: Decode OptionNamesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionNamesList :: Encode OptionNamesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionsDependedOn = OptionsDependedOn (Array String)
derive instance newtypeOptionsDependedOn :: Newtype OptionsDependedOn _
derive instance repGenericOptionsDependedOn :: Generic OptionsDependedOn _
instance showOptionsDependedOn :: Show OptionsDependedOn where
  show = genericShow
instance decodeOptionsDependedOn :: Decode OptionsDependedOn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionsDependedOn :: Encode OptionsDependedOn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionsList = OptionsList (Array Option)
derive instance newtypeOptionsList :: Newtype OptionsList _
derive instance repGenericOptionsList :: Generic OptionsList _
instance showOptionsList :: Show OptionsList where
  show = genericShow
instance decodeOptionsList :: Decode OptionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionsList :: Encode OptionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrderableDBInstanceOption = OrderableDBInstanceOption 
  { "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined.NullOrUndefined (String)
  , "LicenseModel" :: NullOrUndefined.NullOrUndefined (String)
  , "AvailabilityZones" :: NullOrUndefined.NullOrUndefined (AvailabilityZoneList)
  , "MultiAZCapable" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ReadReplicaCapable" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Vpc" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeOrderableDBInstanceOption :: Newtype OrderableDBInstanceOption _
derive instance repGenericOrderableDBInstanceOption :: Generic OrderableDBInstanceOption _
instance showOrderableDBInstanceOption :: Show OrderableDBInstanceOption where
  show = genericShow
instance decodeOrderableDBInstanceOption :: Decode OrderableDBInstanceOption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderableDBInstanceOption :: Encode OrderableDBInstanceOption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrderableDBInstanceOptionsList = OrderableDBInstanceOptionsList (Array OrderableDBInstanceOption)
derive instance newtypeOrderableDBInstanceOptionsList :: Newtype OrderableDBInstanceOptionsList _
derive instance repGenericOrderableDBInstanceOptionsList :: Generic OrderableDBInstanceOptionsList _
instance showOrderableDBInstanceOptionsList :: Show OrderableDBInstanceOptionsList where
  show = genericShow
instance decodeOrderableDBInstanceOptionsList :: Decode OrderableDBInstanceOptionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderableDBInstanceOptionsList :: Encode OrderableDBInstanceOptionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrderableDBInstanceOptionsMessage = OrderableDBInstanceOptionsMessage 
  { "OrderableDBInstanceOptions" :: NullOrUndefined.NullOrUndefined (OrderableDBInstanceOptionsList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOrderableDBInstanceOptionsMessage :: Newtype OrderableDBInstanceOptionsMessage _
derive instance repGenericOrderableDBInstanceOptionsMessage :: Generic OrderableDBInstanceOptionsMessage _
instance showOrderableDBInstanceOptionsMessage :: Show OrderableDBInstanceOptionsMessage where
  show = genericShow
instance decodeOrderableDBInstanceOptionsMessage :: Decode OrderableDBInstanceOptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderableDBInstanceOptionsMessage :: Encode OrderableDBInstanceOptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Parameter = Parameter 
  { "ParameterName" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterValue" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Source" :: NullOrUndefined.NullOrUndefined (String)
  , "ApplyType" :: NullOrUndefined.NullOrUndefined (String)
  , "DataType" :: NullOrUndefined.NullOrUndefined (String)
  , "AllowedValues" :: NullOrUndefined.NullOrUndefined (String)
  , "IsModifiable" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "MinimumEngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "ApplyMethod" :: NullOrUndefined.NullOrUndefined (ApplyMethod)
  }
derive instance newtypeParameter :: Newtype Parameter _
derive instance repGenericParameter :: Generic Parameter _
instance showParameter :: Show Parameter where
  show = genericShow
instance decodeParameter :: Decode Parameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameter :: Encode Parameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParametersList = ParametersList (Array Parameter)
derive instance newtypeParametersList :: Newtype ParametersList _
derive instance repGenericParametersList :: Generic ParametersList _
instance showParametersList :: Show ParametersList where
  show = genericShow
instance decodeParametersList :: Decode ParametersList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParametersList :: Encode ParametersList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PendingModifiedValues = PendingModifiedValues 
  { "DBInstanceClass" :: NullOrUndefined.NullOrUndefined (String)
  , "AllocatedStorage" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "MasterUserPassword" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "BackupRetentionPeriod" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "MultiAZ" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "EngineVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "Iops" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "DBInstanceIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePendingModifiedValues :: Newtype PendingModifiedValues _
derive instance repGenericPendingModifiedValues :: Generic PendingModifiedValues _
instance showPendingModifiedValues :: Show PendingModifiedValues where
  show = genericShow
instance decodePendingModifiedValues :: Decode PendingModifiedValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePendingModifiedValues :: Encode PendingModifiedValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> <i>SourceDBInstanceIdentifier</i> refers to a DB instance with <i>BackupRetentionPeriod</i> equal to 0. </p>
newtype PointInTimeRestoreNotEnabledFault = PointInTimeRestoreNotEnabledFault Types.NoArguments
derive instance newtypePointInTimeRestoreNotEnabledFault :: Newtype PointInTimeRestoreNotEnabledFault _
derive instance repGenericPointInTimeRestoreNotEnabledFault :: Generic PointInTimeRestoreNotEnabledFault _
instance showPointInTimeRestoreNotEnabledFault :: Show PointInTimeRestoreNotEnabledFault where
  show = genericShow
instance decodePointInTimeRestoreNotEnabledFault :: Decode PointInTimeRestoreNotEnabledFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePointInTimeRestoreNotEnabledFault :: Encode PointInTimeRestoreNotEnabledFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PromoteReadReplicaMessage = PromoteReadReplicaMessage 
  { "DBInstanceIdentifier" :: (String)
  , "BackupRetentionPeriod" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "PreferredBackupWindow" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePromoteReadReplicaMessage :: Newtype PromoteReadReplicaMessage _
derive instance repGenericPromoteReadReplicaMessage :: Generic PromoteReadReplicaMessage _
instance showPromoteReadReplicaMessage :: Show PromoteReadReplicaMessage where
  show = genericShow
instance decodePromoteReadReplicaMessage :: Decode PromoteReadReplicaMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePromoteReadReplicaMessage :: Encode PromoteReadReplicaMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PromoteReadReplicaResult = PromoteReadReplicaResult 
  { "DBInstance" :: NullOrUndefined.NullOrUndefined (DBInstance)
  }
derive instance newtypePromoteReadReplicaResult :: Newtype PromoteReadReplicaResult _
derive instance repGenericPromoteReadReplicaResult :: Generic PromoteReadReplicaResult _
instance showPromoteReadReplicaResult :: Show PromoteReadReplicaResult where
  show = genericShow
instance decodePromoteReadReplicaResult :: Decode PromoteReadReplicaResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePromoteReadReplicaResult :: Encode PromoteReadReplicaResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provisioned IOPS not available in the specified Availability Zone.</p>
newtype ProvisionedIopsNotAvailableInAZFault = ProvisionedIopsNotAvailableInAZFault Types.NoArguments
derive instance newtypeProvisionedIopsNotAvailableInAZFault :: Newtype ProvisionedIopsNotAvailableInAZFault _
derive instance repGenericProvisionedIopsNotAvailableInAZFault :: Generic ProvisionedIopsNotAvailableInAZFault _
instance showProvisionedIopsNotAvailableInAZFault :: Show ProvisionedIopsNotAvailableInAZFault where
  show = genericShow
instance decodeProvisionedIopsNotAvailableInAZFault :: Decode ProvisionedIopsNotAvailableInAZFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProvisionedIopsNotAvailableInAZFault :: Encode ProvisionedIopsNotAvailableInAZFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PurchaseReservedDBInstancesOfferingMessage = PurchaseReservedDBInstancesOfferingMessage 
  { "ReservedDBInstancesOfferingId" :: (String)
  , "ReservedDBInstanceId" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstanceCount" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  }
derive instance newtypePurchaseReservedDBInstancesOfferingMessage :: Newtype PurchaseReservedDBInstancesOfferingMessage _
derive instance repGenericPurchaseReservedDBInstancesOfferingMessage :: Generic PurchaseReservedDBInstancesOfferingMessage _
instance showPurchaseReservedDBInstancesOfferingMessage :: Show PurchaseReservedDBInstancesOfferingMessage where
  show = genericShow
instance decodePurchaseReservedDBInstancesOfferingMessage :: Decode PurchaseReservedDBInstancesOfferingMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurchaseReservedDBInstancesOfferingMessage :: Encode PurchaseReservedDBInstancesOfferingMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PurchaseReservedDBInstancesOfferingResult = PurchaseReservedDBInstancesOfferingResult 
  { "ReservedDBInstance" :: NullOrUndefined.NullOrUndefined (ReservedDBInstance)
  }
derive instance newtypePurchaseReservedDBInstancesOfferingResult :: Newtype PurchaseReservedDBInstancesOfferingResult _
derive instance repGenericPurchaseReservedDBInstancesOfferingResult :: Generic PurchaseReservedDBInstancesOfferingResult _
instance showPurchaseReservedDBInstancesOfferingResult :: Show PurchaseReservedDBInstancesOfferingResult where
  show = genericShow
instance decodePurchaseReservedDBInstancesOfferingResult :: Decode PurchaseReservedDBInstancesOfferingResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurchaseReservedDBInstancesOfferingResult :: Encode PurchaseReservedDBInstancesOfferingResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReadReplicaDBInstanceIdentifierList = ReadReplicaDBInstanceIdentifierList (Array String)
derive instance newtypeReadReplicaDBInstanceIdentifierList :: Newtype ReadReplicaDBInstanceIdentifierList _
derive instance repGenericReadReplicaDBInstanceIdentifierList :: Generic ReadReplicaDBInstanceIdentifierList _
instance showReadReplicaDBInstanceIdentifierList :: Show ReadReplicaDBInstanceIdentifierList where
  show = genericShow
instance decodeReadReplicaDBInstanceIdentifierList :: Decode ReadReplicaDBInstanceIdentifierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReadReplicaDBInstanceIdentifierList :: Encode ReadReplicaDBInstanceIdentifierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RebootDBInstanceMessage = RebootDBInstanceMessage 
  { "DBInstanceIdentifier" :: (String)
  , "ForceFailover" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  }
derive instance newtypeRebootDBInstanceMessage :: Newtype RebootDBInstanceMessage _
derive instance repGenericRebootDBInstanceMessage :: Generic RebootDBInstanceMessage _
instance showRebootDBInstanceMessage :: Show RebootDBInstanceMessage where
  show = genericShow
instance decodeRebootDBInstanceMessage :: Decode RebootDBInstanceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebootDBInstanceMessage :: Encode RebootDBInstanceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RebootDBInstanceResult = RebootDBInstanceResult 
  { "DBInstance" :: NullOrUndefined.NullOrUndefined (DBInstance)
  }
derive instance newtypeRebootDBInstanceResult :: Newtype RebootDBInstanceResult _
derive instance repGenericRebootDBInstanceResult :: Generic RebootDBInstanceResult _
instance showRebootDBInstanceResult :: Show RebootDBInstanceResult where
  show = genericShow
instance decodeRebootDBInstanceResult :: Decode RebootDBInstanceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebootDBInstanceResult :: Encode RebootDBInstanceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecurringCharge = RecurringCharge 
  { "RecurringChargeAmount" :: NullOrUndefined.NullOrUndefined (Number)
  , "RecurringChargeFrequency" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRecurringCharge :: Newtype RecurringCharge _
derive instance repGenericRecurringCharge :: Generic RecurringCharge _
instance showRecurringCharge :: Show RecurringCharge where
  show = genericShow
instance decodeRecurringCharge :: Decode RecurringCharge where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecurringCharge :: Encode RecurringCharge where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecurringChargeList = RecurringChargeList (Array RecurringCharge)
derive instance newtypeRecurringChargeList :: Newtype RecurringChargeList _
derive instance repGenericRecurringChargeList :: Generic RecurringChargeList _
instance showRecurringChargeList :: Show RecurringChargeList where
  show = genericShow
instance decodeRecurringChargeList :: Decode RecurringChargeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecurringChargeList :: Encode RecurringChargeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveSourceIdentifierFromSubscriptionMessage = RemoveSourceIdentifierFromSubscriptionMessage 
  { "SubscriptionName" :: (String)
  , "SourceIdentifier" :: (String)
  }
derive instance newtypeRemoveSourceIdentifierFromSubscriptionMessage :: Newtype RemoveSourceIdentifierFromSubscriptionMessage _
derive instance repGenericRemoveSourceIdentifierFromSubscriptionMessage :: Generic RemoveSourceIdentifierFromSubscriptionMessage _
instance showRemoveSourceIdentifierFromSubscriptionMessage :: Show RemoveSourceIdentifierFromSubscriptionMessage where
  show = genericShow
instance decodeRemoveSourceIdentifierFromSubscriptionMessage :: Decode RemoveSourceIdentifierFromSubscriptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveSourceIdentifierFromSubscriptionMessage :: Encode RemoveSourceIdentifierFromSubscriptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveSourceIdentifierFromSubscriptionResult = RemoveSourceIdentifierFromSubscriptionResult 
  { "EventSubscription" :: NullOrUndefined.NullOrUndefined (EventSubscription)
  }
derive instance newtypeRemoveSourceIdentifierFromSubscriptionResult :: Newtype RemoveSourceIdentifierFromSubscriptionResult _
derive instance repGenericRemoveSourceIdentifierFromSubscriptionResult :: Generic RemoveSourceIdentifierFromSubscriptionResult _
instance showRemoveSourceIdentifierFromSubscriptionResult :: Show RemoveSourceIdentifierFromSubscriptionResult where
  show = genericShow
instance decodeRemoveSourceIdentifierFromSubscriptionResult :: Decode RemoveSourceIdentifierFromSubscriptionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveSourceIdentifierFromSubscriptionResult :: Encode RemoveSourceIdentifierFromSubscriptionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTagsFromResourceMessage = RemoveTagsFromResourceMessage 
  { "ResourceName" :: (String)
  , "TagKeys" :: (KeyList)
  }
derive instance newtypeRemoveTagsFromResourceMessage :: Newtype RemoveTagsFromResourceMessage _
derive instance repGenericRemoveTagsFromResourceMessage :: Generic RemoveTagsFromResourceMessage _
instance showRemoveTagsFromResourceMessage :: Show RemoveTagsFromResourceMessage where
  show = genericShow
instance decodeRemoveTagsFromResourceMessage :: Decode RemoveTagsFromResourceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsFromResourceMessage :: Encode RemoveTagsFromResourceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservedDBInstance = ReservedDBInstance 
  { "ReservedDBInstanceId" :: NullOrUndefined.NullOrUndefined (String)
  , "ReservedDBInstancesOfferingId" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined.NullOrUndefined (String)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "Duration" :: NullOrUndefined.NullOrUndefined (Int)
  , "FixedPrice" :: NullOrUndefined.NullOrUndefined (Number)
  , "UsagePrice" :: NullOrUndefined.NullOrUndefined (Number)
  , "CurrencyCode" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstanceCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "ProductDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined.NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "State" :: NullOrUndefined.NullOrUndefined (String)
  , "RecurringCharges" :: NullOrUndefined.NullOrUndefined (RecurringChargeList)
  }
derive instance newtypeReservedDBInstance :: Newtype ReservedDBInstance _
derive instance repGenericReservedDBInstance :: Generic ReservedDBInstance _
instance showReservedDBInstance :: Show ReservedDBInstance where
  show = genericShow
instance decodeReservedDBInstance :: Decode ReservedDBInstance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedDBInstance :: Encode ReservedDBInstance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>User already has a reservation with the given identifier.</p>
newtype ReservedDBInstanceAlreadyExistsFault = ReservedDBInstanceAlreadyExistsFault Types.NoArguments
derive instance newtypeReservedDBInstanceAlreadyExistsFault :: Newtype ReservedDBInstanceAlreadyExistsFault _
derive instance repGenericReservedDBInstanceAlreadyExistsFault :: Generic ReservedDBInstanceAlreadyExistsFault _
instance showReservedDBInstanceAlreadyExistsFault :: Show ReservedDBInstanceAlreadyExistsFault where
  show = genericShow
instance decodeReservedDBInstanceAlreadyExistsFault :: Decode ReservedDBInstanceAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedDBInstanceAlreadyExistsFault :: Encode ReservedDBInstanceAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservedDBInstanceList = ReservedDBInstanceList (Array ReservedDBInstance)
derive instance newtypeReservedDBInstanceList :: Newtype ReservedDBInstanceList _
derive instance repGenericReservedDBInstanceList :: Generic ReservedDBInstanceList _
instance showReservedDBInstanceList :: Show ReservedDBInstanceList where
  show = genericShow
instance decodeReservedDBInstanceList :: Decode ReservedDBInstanceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedDBInstanceList :: Encode ReservedDBInstanceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservedDBInstanceMessage = ReservedDBInstanceMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "ReservedDBInstances" :: NullOrUndefined.NullOrUndefined (ReservedDBInstanceList)
  }
derive instance newtypeReservedDBInstanceMessage :: Newtype ReservedDBInstanceMessage _
derive instance repGenericReservedDBInstanceMessage :: Generic ReservedDBInstanceMessage _
instance showReservedDBInstanceMessage :: Show ReservedDBInstanceMessage where
  show = genericShow
instance decodeReservedDBInstanceMessage :: Decode ReservedDBInstanceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedDBInstanceMessage :: Encode ReservedDBInstanceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified reserved DB Instance not found.</p>
newtype ReservedDBInstanceNotFoundFault = ReservedDBInstanceNotFoundFault Types.NoArguments
derive instance newtypeReservedDBInstanceNotFoundFault :: Newtype ReservedDBInstanceNotFoundFault _
derive instance repGenericReservedDBInstanceNotFoundFault :: Generic ReservedDBInstanceNotFoundFault _
instance showReservedDBInstanceNotFoundFault :: Show ReservedDBInstanceNotFoundFault where
  show = genericShow
instance decodeReservedDBInstanceNotFoundFault :: Decode ReservedDBInstanceNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedDBInstanceNotFoundFault :: Encode ReservedDBInstanceNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request would exceed the user's DB Instance quota.</p>
newtype ReservedDBInstanceQuotaExceededFault = ReservedDBInstanceQuotaExceededFault Types.NoArguments
derive instance newtypeReservedDBInstanceQuotaExceededFault :: Newtype ReservedDBInstanceQuotaExceededFault _
derive instance repGenericReservedDBInstanceQuotaExceededFault :: Generic ReservedDBInstanceQuotaExceededFault _
instance showReservedDBInstanceQuotaExceededFault :: Show ReservedDBInstanceQuotaExceededFault where
  show = genericShow
instance decodeReservedDBInstanceQuotaExceededFault :: Decode ReservedDBInstanceQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedDBInstanceQuotaExceededFault :: Encode ReservedDBInstanceQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservedDBInstancesOffering = ReservedDBInstancesOffering 
  { "ReservedDBInstancesOfferingId" :: NullOrUndefined.NullOrUndefined (String)
  , "DBInstanceClass" :: NullOrUndefined.NullOrUndefined (String)
  , "Duration" :: NullOrUndefined.NullOrUndefined (Int)
  , "FixedPrice" :: NullOrUndefined.NullOrUndefined (Number)
  , "UsagePrice" :: NullOrUndefined.NullOrUndefined (Number)
  , "CurrencyCode" :: NullOrUndefined.NullOrUndefined (String)
  , "ProductDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "OfferingType" :: NullOrUndefined.NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RecurringCharges" :: NullOrUndefined.NullOrUndefined (RecurringChargeList)
  }
derive instance newtypeReservedDBInstancesOffering :: Newtype ReservedDBInstancesOffering _
derive instance repGenericReservedDBInstancesOffering :: Generic ReservedDBInstancesOffering _
instance showReservedDBInstancesOffering :: Show ReservedDBInstancesOffering where
  show = genericShow
instance decodeReservedDBInstancesOffering :: Decode ReservedDBInstancesOffering where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedDBInstancesOffering :: Encode ReservedDBInstancesOffering where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservedDBInstancesOfferingList = ReservedDBInstancesOfferingList (Array ReservedDBInstancesOffering)
derive instance newtypeReservedDBInstancesOfferingList :: Newtype ReservedDBInstancesOfferingList _
derive instance repGenericReservedDBInstancesOfferingList :: Generic ReservedDBInstancesOfferingList _
instance showReservedDBInstancesOfferingList :: Show ReservedDBInstancesOfferingList where
  show = genericShow
instance decodeReservedDBInstancesOfferingList :: Decode ReservedDBInstancesOfferingList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedDBInstancesOfferingList :: Encode ReservedDBInstancesOfferingList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservedDBInstancesOfferingMessage = ReservedDBInstancesOfferingMessage 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "ReservedDBInstancesOfferings" :: NullOrUndefined.NullOrUndefined (ReservedDBInstancesOfferingList)
  }
derive instance newtypeReservedDBInstancesOfferingMessage :: Newtype ReservedDBInstancesOfferingMessage _
derive instance repGenericReservedDBInstancesOfferingMessage :: Generic ReservedDBInstancesOfferingMessage _
instance showReservedDBInstancesOfferingMessage :: Show ReservedDBInstancesOfferingMessage where
  show = genericShow
instance decodeReservedDBInstancesOfferingMessage :: Decode ReservedDBInstancesOfferingMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedDBInstancesOfferingMessage :: Encode ReservedDBInstancesOfferingMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specified offering does not exist.</p>
newtype ReservedDBInstancesOfferingNotFoundFault = ReservedDBInstancesOfferingNotFoundFault Types.NoArguments
derive instance newtypeReservedDBInstancesOfferingNotFoundFault :: Newtype ReservedDBInstancesOfferingNotFoundFault _
derive instance repGenericReservedDBInstancesOfferingNotFoundFault :: Generic ReservedDBInstancesOfferingNotFoundFault _
instance showReservedDBInstancesOfferingNotFoundFault :: Show ReservedDBInstancesOfferingNotFoundFault where
  show = genericShow
instance decodeReservedDBInstancesOfferingNotFoundFault :: Decode ReservedDBInstancesOfferingNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedDBInstancesOfferingNotFoundFault :: Encode ReservedDBInstancesOfferingNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResetDBParameterGroupMessage = ResetDBParameterGroupMessage 
  { "DBParameterGroupName" :: (String)
  , "ResetAllParameters" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersList)
  }
derive instance newtypeResetDBParameterGroupMessage :: Newtype ResetDBParameterGroupMessage _
derive instance repGenericResetDBParameterGroupMessage :: Generic ResetDBParameterGroupMessage _
instance showResetDBParameterGroupMessage :: Show ResetDBParameterGroupMessage where
  show = genericShow
instance decodeResetDBParameterGroupMessage :: Decode ResetDBParameterGroupMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResetDBParameterGroupMessage :: Encode ResetDBParameterGroupMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestoreDBInstanceFromDBSnapshotMessage = RestoreDBInstanceFromDBSnapshotMessage 
  { "DBInstanceIdentifier" :: (String)
  , "DBSnapshotIdentifier" :: (String)
  , "DBInstanceClass" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "DBSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "PubliclyAccessible" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "LicenseModel" :: NullOrUndefined.NullOrUndefined (String)
  , "DBName" :: NullOrUndefined.NullOrUndefined (String)
  , "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "Iops" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "OptionGroupName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRestoreDBInstanceFromDBSnapshotMessage :: Newtype RestoreDBInstanceFromDBSnapshotMessage _
derive instance repGenericRestoreDBInstanceFromDBSnapshotMessage :: Generic RestoreDBInstanceFromDBSnapshotMessage _
instance showRestoreDBInstanceFromDBSnapshotMessage :: Show RestoreDBInstanceFromDBSnapshotMessage where
  show = genericShow
instance decodeRestoreDBInstanceFromDBSnapshotMessage :: Decode RestoreDBInstanceFromDBSnapshotMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreDBInstanceFromDBSnapshotMessage :: Encode RestoreDBInstanceFromDBSnapshotMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestoreDBInstanceFromDBSnapshotResult = RestoreDBInstanceFromDBSnapshotResult 
  { "DBInstance" :: NullOrUndefined.NullOrUndefined (DBInstance)
  }
derive instance newtypeRestoreDBInstanceFromDBSnapshotResult :: Newtype RestoreDBInstanceFromDBSnapshotResult _
derive instance repGenericRestoreDBInstanceFromDBSnapshotResult :: Generic RestoreDBInstanceFromDBSnapshotResult _
instance showRestoreDBInstanceFromDBSnapshotResult :: Show RestoreDBInstanceFromDBSnapshotResult where
  show = genericShow
instance decodeRestoreDBInstanceFromDBSnapshotResult :: Decode RestoreDBInstanceFromDBSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreDBInstanceFromDBSnapshotResult :: Encode RestoreDBInstanceFromDBSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestoreDBInstanceToPointInTimeMessage = RestoreDBInstanceToPointInTimeMessage 
  { "SourceDBInstanceIdentifier" :: (String)
  , "TargetDBInstanceIdentifier" :: (String)
  , "RestoreTime" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "UseLatestRestorableTime" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "DBInstanceClass" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "DBSubnetGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "MultiAZ" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "PubliclyAccessible" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "AutoMinorVersionUpgrade" :: NullOrUndefined.NullOrUndefined (BooleanOptional)
  , "LicenseModel" :: NullOrUndefined.NullOrUndefined (String)
  , "DBName" :: NullOrUndefined.NullOrUndefined (String)
  , "Engine" :: NullOrUndefined.NullOrUndefined (String)
  , "Iops" :: NullOrUndefined.NullOrUndefined (IntegerOptional)
  , "OptionGroupName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRestoreDBInstanceToPointInTimeMessage :: Newtype RestoreDBInstanceToPointInTimeMessage _
derive instance repGenericRestoreDBInstanceToPointInTimeMessage :: Generic RestoreDBInstanceToPointInTimeMessage _
instance showRestoreDBInstanceToPointInTimeMessage :: Show RestoreDBInstanceToPointInTimeMessage where
  show = genericShow
instance decodeRestoreDBInstanceToPointInTimeMessage :: Decode RestoreDBInstanceToPointInTimeMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreDBInstanceToPointInTimeMessage :: Encode RestoreDBInstanceToPointInTimeMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RestoreDBInstanceToPointInTimeResult = RestoreDBInstanceToPointInTimeResult 
  { "DBInstance" :: NullOrUndefined.NullOrUndefined (DBInstance)
  }
derive instance newtypeRestoreDBInstanceToPointInTimeResult :: Newtype RestoreDBInstanceToPointInTimeResult _
derive instance repGenericRestoreDBInstanceToPointInTimeResult :: Generic RestoreDBInstanceToPointInTimeResult _
instance showRestoreDBInstanceToPointInTimeResult :: Show RestoreDBInstanceToPointInTimeResult where
  show = genericShow
instance decodeRestoreDBInstanceToPointInTimeResult :: Decode RestoreDBInstanceToPointInTimeResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreDBInstanceToPointInTimeResult :: Encode RestoreDBInstanceToPointInTimeResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RevokeDBSecurityGroupIngressMessage = RevokeDBSecurityGroupIngressMessage 
  { "DBSecurityGroupName" :: (String)
  , "CIDRIP" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupName" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "EC2SecurityGroupOwnerId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRevokeDBSecurityGroupIngressMessage :: Newtype RevokeDBSecurityGroupIngressMessage _
derive instance repGenericRevokeDBSecurityGroupIngressMessage :: Generic RevokeDBSecurityGroupIngressMessage _
instance showRevokeDBSecurityGroupIngressMessage :: Show RevokeDBSecurityGroupIngressMessage where
  show = genericShow
instance decodeRevokeDBSecurityGroupIngressMessage :: Decode RevokeDBSecurityGroupIngressMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevokeDBSecurityGroupIngressMessage :: Encode RevokeDBSecurityGroupIngressMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RevokeDBSecurityGroupIngressResult = RevokeDBSecurityGroupIngressResult 
  { "DBSecurityGroup" :: NullOrUndefined.NullOrUndefined (DBSecurityGroup)
  }
derive instance newtypeRevokeDBSecurityGroupIngressResult :: Newtype RevokeDBSecurityGroupIngressResult _
derive instance repGenericRevokeDBSecurityGroupIngressResult :: Generic RevokeDBSecurityGroupIngressResult _
instance showRevokeDBSecurityGroupIngressResult :: Show RevokeDBSecurityGroupIngressResult where
  show = genericShow
instance decodeRevokeDBSecurityGroupIngressResult :: Decode RevokeDBSecurityGroupIngressResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevokeDBSecurityGroupIngressResult :: Encode RevokeDBSecurityGroupIngressResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>SNS has responded that there is a problem with the SND topic specified.</p>
newtype SNSInvalidTopicFault = SNSInvalidTopicFault Types.NoArguments
derive instance newtypeSNSInvalidTopicFault :: Newtype SNSInvalidTopicFault _
derive instance repGenericSNSInvalidTopicFault :: Generic SNSInvalidTopicFault _
instance showSNSInvalidTopicFault :: Show SNSInvalidTopicFault where
  show = genericShow
instance decodeSNSInvalidTopicFault :: Decode SNSInvalidTopicFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSNSInvalidTopicFault :: Encode SNSInvalidTopicFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You do not have permission to publish to the SNS topic ARN.</p>
newtype SNSNoAuthorizationFault = SNSNoAuthorizationFault Types.NoArguments
derive instance newtypeSNSNoAuthorizationFault :: Newtype SNSNoAuthorizationFault _
derive instance repGenericSNSNoAuthorizationFault :: Generic SNSNoAuthorizationFault _
instance showSNSNoAuthorizationFault :: Show SNSNoAuthorizationFault where
  show = genericShow
instance decodeSNSNoAuthorizationFault :: Decode SNSNoAuthorizationFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSNSNoAuthorizationFault :: Encode SNSNoAuthorizationFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The SNS topic ARN does not exist.</p>
newtype SNSTopicArnNotFoundFault = SNSTopicArnNotFoundFault Types.NoArguments
derive instance newtypeSNSTopicArnNotFoundFault :: Newtype SNSTopicArnNotFoundFault _
derive instance repGenericSNSTopicArnNotFoundFault :: Generic SNSTopicArnNotFoundFault _
instance showSNSTopicArnNotFoundFault :: Show SNSTopicArnNotFoundFault where
  show = genericShow
instance decodeSNSTopicArnNotFoundFault :: Decode SNSTopicArnNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSNSTopicArnNotFoundFault :: Encode SNSTopicArnNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request would result in user exceeding the allowed number of DB snapshots.</p>
newtype SnapshotQuotaExceededFault = SnapshotQuotaExceededFault Types.NoArguments
derive instance newtypeSnapshotQuotaExceededFault :: Newtype SnapshotQuotaExceededFault _
derive instance repGenericSnapshotQuotaExceededFault :: Generic SnapshotQuotaExceededFault _
instance showSnapshotQuotaExceededFault :: Show SnapshotQuotaExceededFault where
  show = genericShow
instance decodeSnapshotQuotaExceededFault :: Decode SnapshotQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotQuotaExceededFault :: Encode SnapshotQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SourceIdsList = SourceIdsList (Array String)
derive instance newtypeSourceIdsList :: Newtype SourceIdsList _
derive instance repGenericSourceIdsList :: Generic SourceIdsList _
instance showSourceIdsList :: Show SourceIdsList where
  show = genericShow
instance decodeSourceIdsList :: Decode SourceIdsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceIdsList :: Encode SourceIdsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested source could not be found.</p>
newtype SourceNotFoundFault = SourceNotFoundFault Types.NoArguments
derive instance newtypeSourceNotFoundFault :: Newtype SourceNotFoundFault _
derive instance repGenericSourceNotFoundFault :: Generic SourceNotFoundFault _
instance showSourceNotFoundFault :: Show SourceNotFoundFault where
  show = genericShow
instance decodeSourceNotFoundFault :: Decode SourceNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceNotFoundFault :: Encode SourceNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SourceType = SourceType String
derive instance newtypeSourceType :: Newtype SourceType _
derive instance repGenericSourceType :: Generic SourceType _
instance showSourceType :: Show SourceType where
  show = genericShow
instance decodeSourceType :: Decode SourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceType :: Encode SourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request would result in user exceeding the allowed amount of storage available across all DB instances.</p>
newtype StorageQuotaExceededFault = StorageQuotaExceededFault Types.NoArguments
derive instance newtypeStorageQuotaExceededFault :: Newtype StorageQuotaExceededFault _
derive instance repGenericStorageQuotaExceededFault :: Generic StorageQuotaExceededFault _
instance showStorageQuotaExceededFault :: Show StorageQuotaExceededFault where
  show = genericShow
instance decodeStorageQuotaExceededFault :: Decode StorageQuotaExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageQuotaExceededFault :: Encode StorageQuotaExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Subnet = Subnet 
  { "SubnetIdentifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SubnetAvailabilityZone" :: NullOrUndefined.NullOrUndefined (AvailabilityZone)
  , "SubnetStatus" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSubnet :: Newtype Subnet _
derive instance repGenericSubnet :: Generic Subnet _
instance showSubnet :: Show Subnet where
  show = genericShow
instance decodeSubnet :: Decode Subnet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnet :: Encode Subnet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The DB subnet is already in use in the Availability Zone.</p>
newtype SubnetAlreadyInUse = SubnetAlreadyInUse Types.NoArguments
derive instance newtypeSubnetAlreadyInUse :: Newtype SubnetAlreadyInUse _
derive instance repGenericSubnetAlreadyInUse :: Generic SubnetAlreadyInUse _
instance showSubnetAlreadyInUse :: Show SubnetAlreadyInUse where
  show = genericShow
instance decodeSubnetAlreadyInUse :: Decode SubnetAlreadyInUse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetAlreadyInUse :: Encode SubnetAlreadyInUse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetIdentifierList = SubnetIdentifierList (Array String)
derive instance newtypeSubnetIdentifierList :: Newtype SubnetIdentifierList _
derive instance repGenericSubnetIdentifierList :: Generic SubnetIdentifierList _
instance showSubnetIdentifierList :: Show SubnetIdentifierList where
  show = genericShow
instance decodeSubnetIdentifierList :: Decode SubnetIdentifierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetIdentifierList :: Encode SubnetIdentifierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetList = SubnetList (Array Subnet)
derive instance newtypeSubnetList :: Newtype SubnetList _
derive instance repGenericSubnetList :: Generic SubnetList _
instance showSubnetList :: Show SubnetList where
  show = genericShow
instance decodeSubnetList :: Decode SubnetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetList :: Encode SubnetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The supplied subscription name already exists.</p>
newtype SubscriptionAlreadyExistFault = SubscriptionAlreadyExistFault Types.NoArguments
derive instance newtypeSubscriptionAlreadyExistFault :: Newtype SubscriptionAlreadyExistFault _
derive instance repGenericSubscriptionAlreadyExistFault :: Generic SubscriptionAlreadyExistFault _
instance showSubscriptionAlreadyExistFault :: Show SubscriptionAlreadyExistFault where
  show = genericShow
instance decodeSubscriptionAlreadyExistFault :: Decode SubscriptionAlreadyExistFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionAlreadyExistFault :: Encode SubscriptionAlreadyExistFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The supplied category does not exist.</p>
newtype SubscriptionCategoryNotFoundFault = SubscriptionCategoryNotFoundFault Types.NoArguments
derive instance newtypeSubscriptionCategoryNotFoundFault :: Newtype SubscriptionCategoryNotFoundFault _
derive instance repGenericSubscriptionCategoryNotFoundFault :: Generic SubscriptionCategoryNotFoundFault _
instance showSubscriptionCategoryNotFoundFault :: Show SubscriptionCategoryNotFoundFault where
  show = genericShow
instance decodeSubscriptionCategoryNotFoundFault :: Decode SubscriptionCategoryNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionCategoryNotFoundFault :: Encode SubscriptionCategoryNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The subscription name does not exist.</p>
newtype SubscriptionNotFoundFault = SubscriptionNotFoundFault Types.NoArguments
derive instance newtypeSubscriptionNotFoundFault :: Newtype SubscriptionNotFoundFault _
derive instance repGenericSubscriptionNotFoundFault :: Generic SubscriptionNotFoundFault _
instance showSubscriptionNotFoundFault :: Show SubscriptionNotFoundFault where
  show = genericShow
instance decodeSubscriptionNotFoundFault :: Decode SubscriptionNotFoundFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionNotFoundFault :: Encode SubscriptionNotFoundFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SupportedCharacterSetsList = SupportedCharacterSetsList (Array CharacterSet)
derive instance newtypeSupportedCharacterSetsList :: Newtype SupportedCharacterSetsList _
derive instance repGenericSupportedCharacterSetsList :: Generic SupportedCharacterSetsList _
instance showSupportedCharacterSetsList :: Show SupportedCharacterSetsList where
  show = genericShow
instance decodeSupportedCharacterSetsList :: Decode SupportedCharacterSetsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSupportedCharacterSetsList :: Encode SupportedCharacterSetsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TStamp = TStamp Number
derive instance newtypeTStamp :: Newtype TStamp _
derive instance repGenericTStamp :: Generic TStamp _
instance showTStamp :: Show TStamp where
  show = genericShow
instance decodeTStamp :: Decode TStamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTStamp :: Encode TStamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tag = Tag 
  { "Key" :: NullOrUndefined.NullOrUndefined (String)
  , "Value" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagListMessage = TagListMessage 
  { "TagList" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeTagListMessage :: Newtype TagListMessage _
derive instance repGenericTagListMessage :: Generic TagListMessage _
instance showTagListMessage :: Show TagListMessage where
  show = genericShow
instance decodeTagListMessage :: Decode TagListMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagListMessage :: Encode TagListMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VpcSecurityGroupIdList = VpcSecurityGroupIdList (Array String)
derive instance newtypeVpcSecurityGroupIdList :: Newtype VpcSecurityGroupIdList _
derive instance repGenericVpcSecurityGroupIdList :: Generic VpcSecurityGroupIdList _
instance showVpcSecurityGroupIdList :: Show VpcSecurityGroupIdList where
  show = genericShow
instance decodeVpcSecurityGroupIdList :: Decode VpcSecurityGroupIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcSecurityGroupIdList :: Encode VpcSecurityGroupIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VpcSecurityGroupMembership = VpcSecurityGroupMembership 
  { "VpcSecurityGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeVpcSecurityGroupMembership :: Newtype VpcSecurityGroupMembership _
derive instance repGenericVpcSecurityGroupMembership :: Generic VpcSecurityGroupMembership _
instance showVpcSecurityGroupMembership :: Show VpcSecurityGroupMembership where
  show = genericShow
instance decodeVpcSecurityGroupMembership :: Decode VpcSecurityGroupMembership where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcSecurityGroupMembership :: Encode VpcSecurityGroupMembership where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VpcSecurityGroupMembershipList = VpcSecurityGroupMembershipList (Array VpcSecurityGroupMembership)
derive instance newtypeVpcSecurityGroupMembershipList :: Newtype VpcSecurityGroupMembershipList _
derive instance repGenericVpcSecurityGroupMembershipList :: Generic VpcSecurityGroupMembershipList _
instance showVpcSecurityGroupMembershipList :: Show VpcSecurityGroupMembershipList where
  show = genericShow
instance decodeVpcSecurityGroupMembershipList :: Decode VpcSecurityGroupMembershipList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcSecurityGroupMembershipList :: Encode VpcSecurityGroupMembershipList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
