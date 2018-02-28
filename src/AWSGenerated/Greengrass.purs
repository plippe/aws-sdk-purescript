

-- | AWS Greengrass seamlessly extends AWS onto physical devices so they can act locally on the data they generate, while still using the cloud for management, analytics, and durable storage. AWS Greengrass ensures your devices can respond quickly to local events and operate with intermittent connectivity. AWS Greengrass minimizes the cost of transmitting data to the cloud by allowing you to author AWS Lambda functions that execute locally.
module AWS.Greengrass where

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

serviceName = "Greengrass" :: String


-- | Associates a role with a group. The role will be used by the AWS Greengrass core in order to access AWS cloud services. The role's permissions will allow Greengrass core Lambda functions to perform actions against the cloud.
associateRoleToGroup :: forall eff. AssociateRoleToGroupRequest -> Aff (exception :: EXCEPTION | eff) AssociateRoleToGroupResponse
associateRoleToGroup = Request.request serviceName "associateRoleToGroup" 


-- | Associates a role which is used by AWS Greengrass. AWS Greengrass uses the role to access your Lambda functions and AWS IoT resources. This is necessary for deployments to succeed. It needs to have minimum permissions in policy ``AWSGreengrassResourceAccessRolePolicy``
associateServiceRoleToAccount :: forall eff. AssociateServiceRoleToAccountRequest -> Aff (exception :: EXCEPTION | eff) AssociateServiceRoleToAccountResponse
associateServiceRoleToAccount = Request.request serviceName "associateServiceRoleToAccount" 


-- | Creates a core definition. You may optionally provide the initial version of the core definition or use ''CreateCoreDefinitionVersion'' at a later time. AWS Greengrass Groups must each contain exactly 1 AWS Greengrass Core.
createCoreDefinition :: forall eff. CreateCoreDefinitionRequest -> Aff (exception :: EXCEPTION | eff) CreateCoreDefinitionResponse
createCoreDefinition = Request.request serviceName "createCoreDefinition" 


-- | Creates a version of a core definition that has already been defined. AWS Greengrass Groups must each contain exactly 1 AWS Greengrass Core.
createCoreDefinitionVersion :: forall eff. CreateCoreDefinitionVersionRequest -> Aff (exception :: EXCEPTION | eff) CreateCoreDefinitionVersionResponse
createCoreDefinitionVersion = Request.request serviceName "createCoreDefinitionVersion" 


-- | Creates a deployment.
createDeployment :: forall eff. CreateDeploymentRequest -> Aff (exception :: EXCEPTION | eff) CreateDeploymentResponse
createDeployment = Request.request serviceName "createDeployment" 


-- | Creates a device definition. You may optinally provide the initial version of the device definition or use ``CreateDeviceDefinitionVersion`` at a later time.
createDeviceDefinition :: forall eff. CreateDeviceDefinitionRequest -> Aff (exception :: EXCEPTION | eff) CreateDeviceDefinitionResponse
createDeviceDefinition = Request.request serviceName "createDeviceDefinition" 


-- | Creates a version of a device definition that has already been defined.
createDeviceDefinitionVersion :: forall eff. CreateDeviceDefinitionVersionRequest -> Aff (exception :: EXCEPTION | eff) CreateDeviceDefinitionVersionResponse
createDeviceDefinitionVersion = Request.request serviceName "createDeviceDefinitionVersion" 


-- | Creates a Lambda function definition which contains a list of Lambda functions and their configurations to be used in a group. You can create an initial version of the definition by providing a list of Lambda functions and their configurations now, or use ``CreateFunctionDefinitionVersion`` later.
createFunctionDefinition :: forall eff. CreateFunctionDefinitionRequest -> Aff (exception :: EXCEPTION | eff) CreateFunctionDefinitionResponse
createFunctionDefinition = Request.request serviceName "createFunctionDefinition" 


-- | Create a version of a Lambda function definition that has already been defined.
createFunctionDefinitionVersion :: forall eff. CreateFunctionDefinitionVersionRequest -> Aff (exception :: EXCEPTION | eff) CreateFunctionDefinitionVersionResponse
createFunctionDefinitionVersion = Request.request serviceName "createFunctionDefinitionVersion" 


-- | Creates a group. You may optionally provide the initial version of the group or use ''CreateGroupVersion'' at a later time.
createGroup :: forall eff. CreateGroupRequest -> Aff (exception :: EXCEPTION | eff) CreateGroupResponse
createGroup = Request.request serviceName "createGroup" 


-- | Creates a CA for the group. If a CA already exists, it will rotate the existing CA.
createGroupCertificateAuthority :: forall eff. CreateGroupCertificateAuthorityRequest -> Aff (exception :: EXCEPTION | eff) CreateGroupCertificateAuthorityResponse
createGroupCertificateAuthority = Request.request serviceName "createGroupCertificateAuthority" 


-- | Creates a version of a group which has already been defined.
createGroupVersion :: forall eff. CreateGroupVersionRequest -> Aff (exception :: EXCEPTION | eff) CreateGroupVersionResponse
createGroupVersion = Request.request serviceName "createGroupVersion" 


-- | Creates a logger definition. You may optionally provide the initial version of the logger definition or use ``CreateLoggerDefinitionVersion`` at a later time.
createLoggerDefinition :: forall eff. CreateLoggerDefinitionRequest -> Aff (exception :: EXCEPTION | eff) CreateLoggerDefinitionResponse
createLoggerDefinition = Request.request serviceName "createLoggerDefinition" 


-- | Creates a version of a logger definition that has already been defined.
createLoggerDefinitionVersion :: forall eff. CreateLoggerDefinitionVersionRequest -> Aff (exception :: EXCEPTION | eff) CreateLoggerDefinitionVersionResponse
createLoggerDefinitionVersion = Request.request serviceName "createLoggerDefinitionVersion" 


-- | Creates a resource definition which contains a list of resources to be used in a group. You can create an initial version of the definition by providing a list of resources now, or use ``CreateResourceDefinitionVersion`` later.
createResourceDefinition :: forall eff. CreateResourceDefinitionRequest -> Aff (exception :: EXCEPTION | eff) CreateResourceDefinitionResponse
createResourceDefinition = Request.request serviceName "createResourceDefinition" 


-- | Create a version of a resource definition that has already been defined.
createResourceDefinitionVersion :: forall eff. CreateResourceDefinitionVersionRequest -> Aff (exception :: EXCEPTION | eff) CreateResourceDefinitionVersionResponse
createResourceDefinitionVersion = Request.request serviceName "createResourceDefinitionVersion" 


-- | Creates an Iot Job that will trigger your Greengrass Cores to update the software they are running.
createSoftwareUpdateJob :: forall eff. CreateSoftwareUpdateJobRequest -> Aff (exception :: EXCEPTION | eff) CreateSoftwareUpdateJobResponse
createSoftwareUpdateJob = Request.request serviceName "createSoftwareUpdateJob" 


-- | Creates a subscription definition. You may optionally provide the initial version of the subscription definition or use ``CreateSubscriptionDefinitionVersion`` at a later time.
createSubscriptionDefinition :: forall eff. CreateSubscriptionDefinitionRequest -> Aff (exception :: EXCEPTION | eff) CreateSubscriptionDefinitionResponse
createSubscriptionDefinition = Request.request serviceName "createSubscriptionDefinition" 


-- | Creates a version of a subscription definition which has already been defined.
createSubscriptionDefinitionVersion :: forall eff. CreateSubscriptionDefinitionVersionRequest -> Aff (exception :: EXCEPTION | eff) CreateSubscriptionDefinitionVersionResponse
createSubscriptionDefinitionVersion = Request.request serviceName "createSubscriptionDefinitionVersion" 


-- | Deletes a core definition. The core definition must not have been used in a deployment.
deleteCoreDefinition :: forall eff. DeleteCoreDefinitionRequest -> Aff (exception :: EXCEPTION | eff) DeleteCoreDefinitionResponse
deleteCoreDefinition = Request.request serviceName "deleteCoreDefinition" 


-- | Deletes a device definition. The device definition must not have been used in a deployment.
deleteDeviceDefinition :: forall eff. DeleteDeviceDefinitionRequest -> Aff (exception :: EXCEPTION | eff) DeleteDeviceDefinitionResponse
deleteDeviceDefinition = Request.request serviceName "deleteDeviceDefinition" 


-- | Deletes a Lambda function definition. The Lambda function definition must not have been used in a deployment.
deleteFunctionDefinition :: forall eff. DeleteFunctionDefinitionRequest -> Aff (exception :: EXCEPTION | eff) DeleteFunctionDefinitionResponse
deleteFunctionDefinition = Request.request serviceName "deleteFunctionDefinition" 


-- | Deletes a group. The group must not have been used in deployment.
deleteGroup :: forall eff. DeleteGroupRequest -> Aff (exception :: EXCEPTION | eff) DeleteGroupResponse
deleteGroup = Request.request serviceName "deleteGroup" 


-- | Deletes a logger definition. The logger definition must not have been used in a deployment.
deleteLoggerDefinition :: forall eff. DeleteLoggerDefinitionRequest -> Aff (exception :: EXCEPTION | eff) DeleteLoggerDefinitionResponse
deleteLoggerDefinition = Request.request serviceName "deleteLoggerDefinition" 


-- | Deletes a resource definition.
deleteResourceDefinition :: forall eff. DeleteResourceDefinitionRequest -> Aff (exception :: EXCEPTION | eff) DeleteResourceDefinitionResponse
deleteResourceDefinition = Request.request serviceName "deleteResourceDefinition" 


-- | Deletes a subscription definition. The subscription definition must not have been used in a deployment.
deleteSubscriptionDefinition :: forall eff. DeleteSubscriptionDefinitionRequest -> Aff (exception :: EXCEPTION | eff) DeleteSubscriptionDefinitionResponse
deleteSubscriptionDefinition = Request.request serviceName "deleteSubscriptionDefinition" 


-- | Disassociates the role from a group.
disassociateRoleFromGroup :: forall eff. DisassociateRoleFromGroupRequest -> Aff (exception :: EXCEPTION | eff) DisassociateRoleFromGroupResponse
disassociateRoleFromGroup = Request.request serviceName "disassociateRoleFromGroup" 


-- | Disassociates the service role from the account. Without a service role, deployments will not work.
disassociateServiceRoleFromAccount :: forall eff. DisassociateServiceRoleFromAccountRequest -> Aff (exception :: EXCEPTION | eff) DisassociateServiceRoleFromAccountResponse
disassociateServiceRoleFromAccount = Request.request serviceName "disassociateServiceRoleFromAccount" 


-- | Retrieves the role associated with a particular group.
getAssociatedRole :: forall eff. GetAssociatedRoleRequest -> Aff (exception :: EXCEPTION | eff) GetAssociatedRoleResponse
getAssociatedRole = Request.request serviceName "getAssociatedRole" 


-- | Retrieves the connectivity information for a core.
getConnectivityInfo :: forall eff. GetConnectivityInfoRequest -> Aff (exception :: EXCEPTION | eff) GetConnectivityInfoResponse
getConnectivityInfo = Request.request serviceName "getConnectivityInfo" 


-- | Retrieves information about a core definition version.
getCoreDefinition :: forall eff. GetCoreDefinitionRequest -> Aff (exception :: EXCEPTION | eff) GetCoreDefinitionResponse
getCoreDefinition = Request.request serviceName "getCoreDefinition" 


-- | Retrieves information about a core definition version.
getCoreDefinitionVersion :: forall eff. GetCoreDefinitionVersionRequest -> Aff (exception :: EXCEPTION | eff) GetCoreDefinitionVersionResponse
getCoreDefinitionVersion = Request.request serviceName "getCoreDefinitionVersion" 


-- | Returns the status of a deployment.
getDeploymentStatus :: forall eff. GetDeploymentStatusRequest -> Aff (exception :: EXCEPTION | eff) GetDeploymentStatusResponse
getDeploymentStatus = Request.request serviceName "getDeploymentStatus" 


-- | Retrieves information about a device definition.
getDeviceDefinition :: forall eff. GetDeviceDefinitionRequest -> Aff (exception :: EXCEPTION | eff) GetDeviceDefinitionResponse
getDeviceDefinition = Request.request serviceName "getDeviceDefinition" 


-- | Retrieves information about a device definition version.
getDeviceDefinitionVersion :: forall eff. GetDeviceDefinitionVersionRequest -> Aff (exception :: EXCEPTION | eff) GetDeviceDefinitionVersionResponse
getDeviceDefinitionVersion = Request.request serviceName "getDeviceDefinitionVersion" 


-- | Retrieves information about a Lambda function definition, such as its creation time and latest version.
getFunctionDefinition :: forall eff. GetFunctionDefinitionRequest -> Aff (exception :: EXCEPTION | eff) GetFunctionDefinitionResponse
getFunctionDefinition = Request.request serviceName "getFunctionDefinition" 


-- | Retrieves information about a Lambda function definition version, such as which Lambda functions are included in the version and their configurations.
getFunctionDefinitionVersion :: forall eff. GetFunctionDefinitionVersionRequest -> Aff (exception :: EXCEPTION | eff) GetFunctionDefinitionVersionResponse
getFunctionDefinitionVersion = Request.request serviceName "getFunctionDefinitionVersion" 


-- | Retrieves information about a group.
getGroup :: forall eff. GetGroupRequest -> Aff (exception :: EXCEPTION | eff) GetGroupResponse
getGroup = Request.request serviceName "getGroup" 


-- | Retreives the CA associated with a group. Returns the public key of the CA.
getGroupCertificateAuthority :: forall eff. GetGroupCertificateAuthorityRequest -> Aff (exception :: EXCEPTION | eff) GetGroupCertificateAuthorityResponse
getGroupCertificateAuthority = Request.request serviceName "getGroupCertificateAuthority" 


-- | Retrieves the current configuration for the CA used by the group.
getGroupCertificateConfiguration :: forall eff. GetGroupCertificateConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetGroupCertificateConfigurationResponse
getGroupCertificateConfiguration = Request.request serviceName "getGroupCertificateConfiguration" 


-- | Retrieves information about a group version.
getGroupVersion :: forall eff. GetGroupVersionRequest -> Aff (exception :: EXCEPTION | eff) GetGroupVersionResponse
getGroupVersion = Request.request serviceName "getGroupVersion" 


-- | Retrieves information about a logger definition.
getLoggerDefinition :: forall eff. GetLoggerDefinitionRequest -> Aff (exception :: EXCEPTION | eff) GetLoggerDefinitionResponse
getLoggerDefinition = Request.request serviceName "getLoggerDefinition" 


-- | Retrieves information about a logger definition version.
getLoggerDefinitionVersion :: forall eff. GetLoggerDefinitionVersionRequest -> Aff (exception :: EXCEPTION | eff) GetLoggerDefinitionVersionResponse
getLoggerDefinitionVersion = Request.request serviceName "getLoggerDefinitionVersion" 


-- | Retrieves information about a resource definition, such as its creation time and latest version.
getResourceDefinition :: forall eff. GetResourceDefinitionRequest -> Aff (exception :: EXCEPTION | eff) GetResourceDefinitionResponse
getResourceDefinition = Request.request serviceName "getResourceDefinition" 


-- | Retrieves information about a resource definition version, such as which resources are included in the version.
getResourceDefinitionVersion :: forall eff. GetResourceDefinitionVersionRequest -> Aff (exception :: EXCEPTION | eff) GetResourceDefinitionVersionResponse
getResourceDefinitionVersion = Request.request serviceName "getResourceDefinitionVersion" 


-- | Retrieves the service role that is attached to the account.
getServiceRoleForAccount :: forall eff. GetServiceRoleForAccountRequest -> Aff (exception :: EXCEPTION | eff) GetServiceRoleForAccountResponse
getServiceRoleForAccount = Request.request serviceName "getServiceRoleForAccount" 


-- | Retrieves information about a subscription definition.
getSubscriptionDefinition :: forall eff. GetSubscriptionDefinitionRequest -> Aff (exception :: EXCEPTION | eff) GetSubscriptionDefinitionResponse
getSubscriptionDefinition = Request.request serviceName "getSubscriptionDefinition" 


-- | Retrieves information about a subscription definition version.
getSubscriptionDefinitionVersion :: forall eff. GetSubscriptionDefinitionVersionRequest -> Aff (exception :: EXCEPTION | eff) GetSubscriptionDefinitionVersionResponse
getSubscriptionDefinitionVersion = Request.request serviceName "getSubscriptionDefinitionVersion" 


-- | Lists versions of a core definition.
listCoreDefinitionVersions :: forall eff. ListCoreDefinitionVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListCoreDefinitionVersionsResponse
listCoreDefinitionVersions = Request.request serviceName "listCoreDefinitionVersions" 


-- | Retrieves a list of core definitions.
listCoreDefinitions :: forall eff. ListCoreDefinitionsRequest -> Aff (exception :: EXCEPTION | eff) ListCoreDefinitionsResponse
listCoreDefinitions = Request.request serviceName "listCoreDefinitions" 


-- | Returns a history of deployments for the group.
listDeployments :: forall eff. ListDeploymentsRequest -> Aff (exception :: EXCEPTION | eff) ListDeploymentsResponse
listDeployments = Request.request serviceName "listDeployments" 


-- | Lists the versions of a device definition.
listDeviceDefinitionVersions :: forall eff. ListDeviceDefinitionVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListDeviceDefinitionVersionsResponse
listDeviceDefinitionVersions = Request.request serviceName "listDeviceDefinitionVersions" 


-- | Retrieves a list of device definitions.
listDeviceDefinitions :: forall eff. ListDeviceDefinitionsRequest -> Aff (exception :: EXCEPTION | eff) ListDeviceDefinitionsResponse
listDeviceDefinitions = Request.request serviceName "listDeviceDefinitions" 


-- | Lists the versions of a Lambda function definition.
listFunctionDefinitionVersions :: forall eff. ListFunctionDefinitionVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListFunctionDefinitionVersionsResponse
listFunctionDefinitionVersions = Request.request serviceName "listFunctionDefinitionVersions" 


-- | Retrieves a list of Lambda function definitions.
listFunctionDefinitions :: forall eff. ListFunctionDefinitionsRequest -> Aff (exception :: EXCEPTION | eff) ListFunctionDefinitionsResponse
listFunctionDefinitions = Request.request serviceName "listFunctionDefinitions" 


-- | Retrieves the current CAs for a group.
listGroupCertificateAuthorities :: forall eff. ListGroupCertificateAuthoritiesRequest -> Aff (exception :: EXCEPTION | eff) ListGroupCertificateAuthoritiesResponse
listGroupCertificateAuthorities = Request.request serviceName "listGroupCertificateAuthorities" 


-- | List the versions of a group.
listGroupVersions :: forall eff. ListGroupVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListGroupVersionsResponse
listGroupVersions = Request.request serviceName "listGroupVersions" 


-- | Retrieves a list of groups.
listGroups :: forall eff. ListGroupsRequest -> Aff (exception :: EXCEPTION | eff) ListGroupsResponse
listGroups = Request.request serviceName "listGroups" 


-- | Lists the versions of a logger definition.
listLoggerDefinitionVersions :: forall eff. ListLoggerDefinitionVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListLoggerDefinitionVersionsResponse
listLoggerDefinitionVersions = Request.request serviceName "listLoggerDefinitionVersions" 


-- | Retrieves a list of logger definitions.
listLoggerDefinitions :: forall eff. ListLoggerDefinitionsRequest -> Aff (exception :: EXCEPTION | eff) ListLoggerDefinitionsResponse
listLoggerDefinitions = Request.request serviceName "listLoggerDefinitions" 


-- | Lists the versions of a resource definition.
listResourceDefinitionVersions :: forall eff. ListResourceDefinitionVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListResourceDefinitionVersionsResponse
listResourceDefinitionVersions = Request.request serviceName "listResourceDefinitionVersions" 


-- | Retrieves a list of resource definitions.
listResourceDefinitions :: forall eff. ListResourceDefinitionsRequest -> Aff (exception :: EXCEPTION | eff) ListResourceDefinitionsResponse
listResourceDefinitions = Request.request serviceName "listResourceDefinitions" 


-- | Lists the versions of a subscription definition.
listSubscriptionDefinitionVersions :: forall eff. ListSubscriptionDefinitionVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListSubscriptionDefinitionVersionsResponse
listSubscriptionDefinitionVersions = Request.request serviceName "listSubscriptionDefinitionVersions" 


-- | Retrieves a list of subscription definitions.
listSubscriptionDefinitions :: forall eff. ListSubscriptionDefinitionsRequest -> Aff (exception :: EXCEPTION | eff) ListSubscriptionDefinitionsResponse
listSubscriptionDefinitions = Request.request serviceName "listSubscriptionDefinitions" 


-- | Resets a group's deployments.
resetDeployments :: forall eff. ResetDeploymentsRequest -> Aff (exception :: EXCEPTION | eff) ResetDeploymentsResponse
resetDeployments = Request.request serviceName "resetDeployments" 


-- | Updates the connectivity information for the core. Any devices that belong to the group which has this core will receive this information in order to find the location of the core and connect to it.
updateConnectivityInfo :: forall eff. UpdateConnectivityInfoRequest -> Aff (exception :: EXCEPTION | eff) UpdateConnectivityInfoResponse
updateConnectivityInfo = Request.request serviceName "updateConnectivityInfo" 


-- | Updates a core definition.
updateCoreDefinition :: forall eff. UpdateCoreDefinitionRequest -> Aff (exception :: EXCEPTION | eff) UpdateCoreDefinitionResponse
updateCoreDefinition = Request.request serviceName "updateCoreDefinition" 


-- | Updates a device definition.
updateDeviceDefinition :: forall eff. UpdateDeviceDefinitionRequest -> Aff (exception :: EXCEPTION | eff) UpdateDeviceDefinitionResponse
updateDeviceDefinition = Request.request serviceName "updateDeviceDefinition" 


-- | Updates a Lambda function definition.
updateFunctionDefinition :: forall eff. UpdateFunctionDefinitionRequest -> Aff (exception :: EXCEPTION | eff) UpdateFunctionDefinitionResponse
updateFunctionDefinition = Request.request serviceName "updateFunctionDefinition" 


-- | Updates a group.
updateGroup :: forall eff. UpdateGroupRequest -> Aff (exception :: EXCEPTION | eff) UpdateGroupResponse
updateGroup = Request.request serviceName "updateGroup" 


-- | Updates the Cert expiry time for a group.
updateGroupCertificateConfiguration :: forall eff. UpdateGroupCertificateConfigurationRequest -> Aff (exception :: EXCEPTION | eff) UpdateGroupCertificateConfigurationResponse
updateGroupCertificateConfiguration = Request.request serviceName "updateGroupCertificateConfiguration" 


-- | Updates a logger definition.
updateLoggerDefinition :: forall eff. UpdateLoggerDefinitionRequest -> Aff (exception :: EXCEPTION | eff) UpdateLoggerDefinitionResponse
updateLoggerDefinition = Request.request serviceName "updateLoggerDefinition" 


-- | Updates a resource definition.
updateResourceDefinition :: forall eff. UpdateResourceDefinitionRequest -> Aff (exception :: EXCEPTION | eff) UpdateResourceDefinitionResponse
updateResourceDefinition = Request.request serviceName "updateResourceDefinition" 


-- | Updates a subscription definition.
updateSubscriptionDefinition :: forall eff. UpdateSubscriptionDefinitionRequest -> Aff (exception :: EXCEPTION | eff) UpdateSubscriptionDefinitionResponse
updateSubscriptionDefinition = Request.request serviceName "updateSubscriptionDefinition" 


newtype AssociateRoleToGroupRequest = AssociateRoleToGroupRequest 
  { "GroupId" :: (String)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAssociateRoleToGroupRequest :: Newtype AssociateRoleToGroupRequest _
derive instance repGenericAssociateRoleToGroupRequest :: Generic AssociateRoleToGroupRequest _
instance showAssociateRoleToGroupRequest :: Show AssociateRoleToGroupRequest where
  show = genericShow
instance decodeAssociateRoleToGroupRequest :: Decode AssociateRoleToGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateRoleToGroupRequest :: Encode AssociateRoleToGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateRoleToGroupResponse = AssociateRoleToGroupResponse 
  { "AssociatedAt" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAssociateRoleToGroupResponse :: Newtype AssociateRoleToGroupResponse _
derive instance repGenericAssociateRoleToGroupResponse :: Generic AssociateRoleToGroupResponse _
instance showAssociateRoleToGroupResponse :: Show AssociateRoleToGroupResponse where
  show = genericShow
instance decodeAssociateRoleToGroupResponse :: Decode AssociateRoleToGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateRoleToGroupResponse :: Encode AssociateRoleToGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateServiceRoleToAccountRequest = AssociateServiceRoleToAccountRequest 
  { "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAssociateServiceRoleToAccountRequest :: Newtype AssociateServiceRoleToAccountRequest _
derive instance repGenericAssociateServiceRoleToAccountRequest :: Generic AssociateServiceRoleToAccountRequest _
instance showAssociateServiceRoleToAccountRequest :: Show AssociateServiceRoleToAccountRequest where
  show = genericShow
instance decodeAssociateServiceRoleToAccountRequest :: Decode AssociateServiceRoleToAccountRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateServiceRoleToAccountRequest :: Encode AssociateServiceRoleToAccountRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateServiceRoleToAccountResponse = AssociateServiceRoleToAccountResponse 
  { "AssociatedAt" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAssociateServiceRoleToAccountResponse :: Newtype AssociateServiceRoleToAccountResponse _
derive instance repGenericAssociateServiceRoleToAccountResponse :: Generic AssociateServiceRoleToAccountResponse _
instance showAssociateServiceRoleToAccountResponse :: Show AssociateServiceRoleToAccountResponse where
  show = genericShow
instance decodeAssociateServiceRoleToAccountResponse :: Decode AssociateServiceRoleToAccountResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateServiceRoleToAccountResponse :: Encode AssociateServiceRoleToAccountResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | General Error
newtype BadRequestException = BadRequestException 
  { "ErrorDetails" :: NullOrUndefined.NullOrUndefined (ErrorDetails)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _
derive instance repGenericBadRequestException :: Generic BadRequestException _
instance showBadRequestException :: Show BadRequestException where
  show = genericShow
instance decodeBadRequestException :: Decode BadRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequestException :: Encode BadRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Connectivity Info
newtype ConnectivityInfo = ConnectivityInfo 
  { "HostAddress" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Metadata" :: NullOrUndefined.NullOrUndefined (String)
  , "PortNumber" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeConnectivityInfo :: Newtype ConnectivityInfo _
derive instance repGenericConnectivityInfo :: Generic ConnectivityInfo _
instance showConnectivityInfo :: Show ConnectivityInfo where
  show = genericShow
instance decodeConnectivityInfo :: Decode ConnectivityInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectivityInfo :: Encode ConnectivityInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on the core
newtype Core = Core 
  { "CertificateArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "SyncShadow" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ThingArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCore :: Newtype Core _
derive instance repGenericCore :: Generic Core _
instance showCore :: Show Core where
  show = genericShow
instance decodeCore :: Decode Core where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCore :: Encode Core where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on core definition version
newtype CoreDefinitionVersion = CoreDefinitionVersion 
  { "Cores" :: NullOrUndefined.NullOrUndefined (ListOfCore)
  }
derive instance newtypeCoreDefinitionVersion :: Newtype CoreDefinitionVersion _
derive instance repGenericCoreDefinitionVersion :: Generic CoreDefinitionVersion _
instance showCoreDefinitionVersion :: Show CoreDefinitionVersion where
  show = genericShow
instance decodeCoreDefinitionVersion :: Decode CoreDefinitionVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCoreDefinitionVersion :: Encode CoreDefinitionVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on the core definition request
newtype CreateCoreDefinitionRequest = CreateCoreDefinitionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined.NullOrUndefined (CoreDefinitionVersion)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateCoreDefinitionRequest :: Newtype CreateCoreDefinitionRequest _
derive instance repGenericCreateCoreDefinitionRequest :: Generic CreateCoreDefinitionRequest _
instance showCreateCoreDefinitionRequest :: Show CreateCoreDefinitionRequest where
  show = genericShow
instance decodeCreateCoreDefinitionRequest :: Decode CreateCoreDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCoreDefinitionRequest :: Encode CreateCoreDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCoreDefinitionResponse = CreateCoreDefinitionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateCoreDefinitionResponse :: Newtype CreateCoreDefinitionResponse _
derive instance repGenericCreateCoreDefinitionResponse :: Generic CreateCoreDefinitionResponse _
instance showCreateCoreDefinitionResponse :: Show CreateCoreDefinitionResponse where
  show = genericShow
instance decodeCreateCoreDefinitionResponse :: Decode CreateCoreDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCoreDefinitionResponse :: Encode CreateCoreDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCoreDefinitionVersionRequest = CreateCoreDefinitionVersionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "CoreDefinitionId" :: (String)
  , "Cores" :: NullOrUndefined.NullOrUndefined (ListOfCore)
  }
derive instance newtypeCreateCoreDefinitionVersionRequest :: Newtype CreateCoreDefinitionVersionRequest _
derive instance repGenericCreateCoreDefinitionVersionRequest :: Generic CreateCoreDefinitionVersionRequest _
instance showCreateCoreDefinitionVersionRequest :: Show CreateCoreDefinitionVersionRequest where
  show = genericShow
instance decodeCreateCoreDefinitionVersionRequest :: Decode CreateCoreDefinitionVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCoreDefinitionVersionRequest :: Encode CreateCoreDefinitionVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCoreDefinitionVersionResponse = CreateCoreDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateCoreDefinitionVersionResponse :: Newtype CreateCoreDefinitionVersionResponse _
derive instance repGenericCreateCoreDefinitionVersionResponse :: Generic CreateCoreDefinitionVersionResponse _
instance showCreateCoreDefinitionVersionResponse :: Show CreateCoreDefinitionVersionResponse where
  show = genericShow
instance decodeCreateCoreDefinitionVersionResponse :: Decode CreateCoreDefinitionVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCoreDefinitionVersionResponse :: Encode CreateCoreDefinitionVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDeploymentRequest = CreateDeploymentRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentId" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentType" :: NullOrUndefined.NullOrUndefined (DeploymentType)
  , "GroupId" :: (String)
  , "GroupVersionId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateDeploymentRequest :: Newtype CreateDeploymentRequest _
derive instance repGenericCreateDeploymentRequest :: Generic CreateDeploymentRequest _
instance showCreateDeploymentRequest :: Show CreateDeploymentRequest where
  show = genericShow
instance decodeCreateDeploymentRequest :: Decode CreateDeploymentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDeploymentRequest :: Encode CreateDeploymentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDeploymentResponse = CreateDeploymentResponse 
  { "DeploymentArn" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateDeploymentResponse :: Newtype CreateDeploymentResponse _
derive instance repGenericCreateDeploymentResponse :: Generic CreateDeploymentResponse _
instance showCreateDeploymentResponse :: Show CreateDeploymentResponse where
  show = genericShow
instance decodeCreateDeploymentResponse :: Decode CreateDeploymentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDeploymentResponse :: Encode CreateDeploymentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDeviceDefinitionRequest = CreateDeviceDefinitionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined.NullOrUndefined (DeviceDefinitionVersion)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateDeviceDefinitionRequest :: Newtype CreateDeviceDefinitionRequest _
derive instance repGenericCreateDeviceDefinitionRequest :: Generic CreateDeviceDefinitionRequest _
instance showCreateDeviceDefinitionRequest :: Show CreateDeviceDefinitionRequest where
  show = genericShow
instance decodeCreateDeviceDefinitionRequest :: Decode CreateDeviceDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDeviceDefinitionRequest :: Encode CreateDeviceDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDeviceDefinitionResponse = CreateDeviceDefinitionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateDeviceDefinitionResponse :: Newtype CreateDeviceDefinitionResponse _
derive instance repGenericCreateDeviceDefinitionResponse :: Generic CreateDeviceDefinitionResponse _
instance showCreateDeviceDefinitionResponse :: Show CreateDeviceDefinitionResponse where
  show = genericShow
instance decodeCreateDeviceDefinitionResponse :: Decode CreateDeviceDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDeviceDefinitionResponse :: Encode CreateDeviceDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDeviceDefinitionVersionRequest = CreateDeviceDefinitionVersionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "DeviceDefinitionId" :: (String)
  , "Devices" :: NullOrUndefined.NullOrUndefined (ListOfDevice)
  }
derive instance newtypeCreateDeviceDefinitionVersionRequest :: Newtype CreateDeviceDefinitionVersionRequest _
derive instance repGenericCreateDeviceDefinitionVersionRequest :: Generic CreateDeviceDefinitionVersionRequest _
instance showCreateDeviceDefinitionVersionRequest :: Show CreateDeviceDefinitionVersionRequest where
  show = genericShow
instance decodeCreateDeviceDefinitionVersionRequest :: Decode CreateDeviceDefinitionVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDeviceDefinitionVersionRequest :: Encode CreateDeviceDefinitionVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDeviceDefinitionVersionResponse = CreateDeviceDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateDeviceDefinitionVersionResponse :: Newtype CreateDeviceDefinitionVersionResponse _
derive instance repGenericCreateDeviceDefinitionVersionResponse :: Generic CreateDeviceDefinitionVersionResponse _
instance showCreateDeviceDefinitionVersionResponse :: Show CreateDeviceDefinitionVersionResponse where
  show = genericShow
instance decodeCreateDeviceDefinitionVersionResponse :: Decode CreateDeviceDefinitionVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDeviceDefinitionVersionResponse :: Encode CreateDeviceDefinitionVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateFunctionDefinitionRequest = CreateFunctionDefinitionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined.NullOrUndefined (FunctionDefinitionVersion)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateFunctionDefinitionRequest :: Newtype CreateFunctionDefinitionRequest _
derive instance repGenericCreateFunctionDefinitionRequest :: Generic CreateFunctionDefinitionRequest _
instance showCreateFunctionDefinitionRequest :: Show CreateFunctionDefinitionRequest where
  show = genericShow
instance decodeCreateFunctionDefinitionRequest :: Decode CreateFunctionDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateFunctionDefinitionRequest :: Encode CreateFunctionDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateFunctionDefinitionResponse = CreateFunctionDefinitionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateFunctionDefinitionResponse :: Newtype CreateFunctionDefinitionResponse _
derive instance repGenericCreateFunctionDefinitionResponse :: Generic CreateFunctionDefinitionResponse _
instance showCreateFunctionDefinitionResponse :: Show CreateFunctionDefinitionResponse where
  show = genericShow
instance decodeCreateFunctionDefinitionResponse :: Decode CreateFunctionDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateFunctionDefinitionResponse :: Encode CreateFunctionDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Function definition version
newtype CreateFunctionDefinitionVersionRequest = CreateFunctionDefinitionVersionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "FunctionDefinitionId" :: (String)
  , "Functions" :: NullOrUndefined.NullOrUndefined (ListOfFunction)
  }
derive instance newtypeCreateFunctionDefinitionVersionRequest :: Newtype CreateFunctionDefinitionVersionRequest _
derive instance repGenericCreateFunctionDefinitionVersionRequest :: Generic CreateFunctionDefinitionVersionRequest _
instance showCreateFunctionDefinitionVersionRequest :: Show CreateFunctionDefinitionVersionRequest where
  show = genericShow
instance decodeCreateFunctionDefinitionVersionRequest :: Decode CreateFunctionDefinitionVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateFunctionDefinitionVersionRequest :: Encode CreateFunctionDefinitionVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateFunctionDefinitionVersionResponse = CreateFunctionDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateFunctionDefinitionVersionResponse :: Newtype CreateFunctionDefinitionVersionResponse _
derive instance repGenericCreateFunctionDefinitionVersionResponse :: Generic CreateFunctionDefinitionVersionResponse _
instance showCreateFunctionDefinitionVersionResponse :: Show CreateFunctionDefinitionVersionResponse where
  show = genericShow
instance decodeCreateFunctionDefinitionVersionResponse :: Decode CreateFunctionDefinitionVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateFunctionDefinitionVersionResponse :: Encode CreateFunctionDefinitionVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGroupCertificateAuthorityRequest = CreateGroupCertificateAuthorityRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "GroupId" :: (String)
  }
derive instance newtypeCreateGroupCertificateAuthorityRequest :: Newtype CreateGroupCertificateAuthorityRequest _
derive instance repGenericCreateGroupCertificateAuthorityRequest :: Generic CreateGroupCertificateAuthorityRequest _
instance showCreateGroupCertificateAuthorityRequest :: Show CreateGroupCertificateAuthorityRequest where
  show = genericShow
instance decodeCreateGroupCertificateAuthorityRequest :: Decode CreateGroupCertificateAuthorityRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGroupCertificateAuthorityRequest :: Encode CreateGroupCertificateAuthorityRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGroupCertificateAuthorityResponse = CreateGroupCertificateAuthorityResponse 
  { "GroupCertificateAuthorityArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateGroupCertificateAuthorityResponse :: Newtype CreateGroupCertificateAuthorityResponse _
derive instance repGenericCreateGroupCertificateAuthorityResponse :: Generic CreateGroupCertificateAuthorityResponse _
instance showCreateGroupCertificateAuthorityResponse :: Show CreateGroupCertificateAuthorityResponse where
  show = genericShow
instance decodeCreateGroupCertificateAuthorityResponse :: Decode CreateGroupCertificateAuthorityResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGroupCertificateAuthorityResponse :: Encode CreateGroupCertificateAuthorityResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGroupRequest = CreateGroupRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined.NullOrUndefined (GroupVersion)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateGroupRequest :: Newtype CreateGroupRequest _
derive instance repGenericCreateGroupRequest :: Generic CreateGroupRequest _
instance showCreateGroupRequest :: Show CreateGroupRequest where
  show = genericShow
instance decodeCreateGroupRequest :: Decode CreateGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGroupRequest :: Encode CreateGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGroupResponse = CreateGroupResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateGroupResponse :: Newtype CreateGroupResponse _
derive instance repGenericCreateGroupResponse :: Generic CreateGroupResponse _
instance showCreateGroupResponse :: Show CreateGroupResponse where
  show = genericShow
instance decodeCreateGroupResponse :: Decode CreateGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGroupResponse :: Encode CreateGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGroupVersionRequest = CreateGroupVersionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "CoreDefinitionVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "DeviceDefinitionVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "FunctionDefinitionVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "GroupId" :: (String)
  , "LoggerDefinitionVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "ResourceDefinitionVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "SubscriptionDefinitionVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateGroupVersionRequest :: Newtype CreateGroupVersionRequest _
derive instance repGenericCreateGroupVersionRequest :: Generic CreateGroupVersionRequest _
instance showCreateGroupVersionRequest :: Show CreateGroupVersionRequest where
  show = genericShow
instance decodeCreateGroupVersionRequest :: Decode CreateGroupVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGroupVersionRequest :: Encode CreateGroupVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGroupVersionResponse = CreateGroupVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateGroupVersionResponse :: Newtype CreateGroupVersionResponse _
derive instance repGenericCreateGroupVersionResponse :: Generic CreateGroupVersionResponse _
instance showCreateGroupVersionResponse :: Show CreateGroupVersionResponse where
  show = genericShow
instance decodeCreateGroupVersionResponse :: Decode CreateGroupVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGroupVersionResponse :: Encode CreateGroupVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLoggerDefinitionRequest = CreateLoggerDefinitionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined.NullOrUndefined (LoggerDefinitionVersion)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateLoggerDefinitionRequest :: Newtype CreateLoggerDefinitionRequest _
derive instance repGenericCreateLoggerDefinitionRequest :: Generic CreateLoggerDefinitionRequest _
instance showCreateLoggerDefinitionRequest :: Show CreateLoggerDefinitionRequest where
  show = genericShow
instance decodeCreateLoggerDefinitionRequest :: Decode CreateLoggerDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoggerDefinitionRequest :: Encode CreateLoggerDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLoggerDefinitionResponse = CreateLoggerDefinitionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateLoggerDefinitionResponse :: Newtype CreateLoggerDefinitionResponse _
derive instance repGenericCreateLoggerDefinitionResponse :: Generic CreateLoggerDefinitionResponse _
instance showCreateLoggerDefinitionResponse :: Show CreateLoggerDefinitionResponse where
  show = genericShow
instance decodeCreateLoggerDefinitionResponse :: Decode CreateLoggerDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoggerDefinitionResponse :: Encode CreateLoggerDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLoggerDefinitionVersionRequest = CreateLoggerDefinitionVersionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "LoggerDefinitionId" :: (String)
  , "Loggers" :: NullOrUndefined.NullOrUndefined (ListOfLogger)
  }
derive instance newtypeCreateLoggerDefinitionVersionRequest :: Newtype CreateLoggerDefinitionVersionRequest _
derive instance repGenericCreateLoggerDefinitionVersionRequest :: Generic CreateLoggerDefinitionVersionRequest _
instance showCreateLoggerDefinitionVersionRequest :: Show CreateLoggerDefinitionVersionRequest where
  show = genericShow
instance decodeCreateLoggerDefinitionVersionRequest :: Decode CreateLoggerDefinitionVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoggerDefinitionVersionRequest :: Encode CreateLoggerDefinitionVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLoggerDefinitionVersionResponse = CreateLoggerDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateLoggerDefinitionVersionResponse :: Newtype CreateLoggerDefinitionVersionResponse _
derive instance repGenericCreateLoggerDefinitionVersionResponse :: Generic CreateLoggerDefinitionVersionResponse _
instance showCreateLoggerDefinitionVersionResponse :: Show CreateLoggerDefinitionVersionResponse where
  show = genericShow
instance decodeCreateLoggerDefinitionVersionResponse :: Decode CreateLoggerDefinitionVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoggerDefinitionVersionResponse :: Encode CreateLoggerDefinitionVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateResourceDefinitionRequest = CreateResourceDefinitionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined.NullOrUndefined (ResourceDefinitionVersion)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateResourceDefinitionRequest :: Newtype CreateResourceDefinitionRequest _
derive instance repGenericCreateResourceDefinitionRequest :: Generic CreateResourceDefinitionRequest _
instance showCreateResourceDefinitionRequest :: Show CreateResourceDefinitionRequest where
  show = genericShow
instance decodeCreateResourceDefinitionRequest :: Decode CreateResourceDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResourceDefinitionRequest :: Encode CreateResourceDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateResourceDefinitionResponse = CreateResourceDefinitionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateResourceDefinitionResponse :: Newtype CreateResourceDefinitionResponse _
derive instance repGenericCreateResourceDefinitionResponse :: Generic CreateResourceDefinitionResponse _
instance showCreateResourceDefinitionResponse :: Show CreateResourceDefinitionResponse where
  show = genericShow
instance decodeCreateResourceDefinitionResponse :: Decode CreateResourceDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResourceDefinitionResponse :: Encode CreateResourceDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateResourceDefinitionVersionRequest = CreateResourceDefinitionVersionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "ResourceDefinitionId" :: (String)
  , "Resources" :: NullOrUndefined.NullOrUndefined (ListOfResource)
  }
derive instance newtypeCreateResourceDefinitionVersionRequest :: Newtype CreateResourceDefinitionVersionRequest _
derive instance repGenericCreateResourceDefinitionVersionRequest :: Generic CreateResourceDefinitionVersionRequest _
instance showCreateResourceDefinitionVersionRequest :: Show CreateResourceDefinitionVersionRequest where
  show = genericShow
instance decodeCreateResourceDefinitionVersionRequest :: Decode CreateResourceDefinitionVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResourceDefinitionVersionRequest :: Encode CreateResourceDefinitionVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateResourceDefinitionVersionResponse = CreateResourceDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateResourceDefinitionVersionResponse :: Newtype CreateResourceDefinitionVersionResponse _
derive instance repGenericCreateResourceDefinitionVersionResponse :: Generic CreateResourceDefinitionVersionResponse _
instance showCreateResourceDefinitionVersionResponse :: Show CreateResourceDefinitionVersionResponse where
  show = genericShow
instance decodeCreateResourceDefinitionVersionResponse :: Decode CreateResourceDefinitionVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResourceDefinitionVersionResponse :: Encode CreateResourceDefinitionVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSoftwareUpdateJobRequest = CreateSoftwareUpdateJobRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "S3UrlSignerRole" :: NullOrUndefined.NullOrUndefined (S3UrlSignerRole)
  , "SoftwareToUpdate" :: NullOrUndefined.NullOrUndefined (SoftwareToUpdate)
  , "UpdateAgentLogLevel" :: NullOrUndefined.NullOrUndefined (UpdateAgentLogLevel)
  , "UpdateTargets" :: NullOrUndefined.NullOrUndefined (UpdateTargets)
  , "UpdateTargetsArchitecture" :: NullOrUndefined.NullOrUndefined (UpdateTargetsArchitecture)
  , "UpdateTargetsOperatingSystem" :: NullOrUndefined.NullOrUndefined (UpdateTargetsOperatingSystem)
  }
derive instance newtypeCreateSoftwareUpdateJobRequest :: Newtype CreateSoftwareUpdateJobRequest _
derive instance repGenericCreateSoftwareUpdateJobRequest :: Generic CreateSoftwareUpdateJobRequest _
instance showCreateSoftwareUpdateJobRequest :: Show CreateSoftwareUpdateJobRequest where
  show = genericShow
instance decodeCreateSoftwareUpdateJobRequest :: Decode CreateSoftwareUpdateJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSoftwareUpdateJobRequest :: Encode CreateSoftwareUpdateJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSoftwareUpdateJobResponse = CreateSoftwareUpdateJobResponse 
  { "IotJobArn" :: NullOrUndefined.NullOrUndefined (String)
  , "IotJobId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateSoftwareUpdateJobResponse :: Newtype CreateSoftwareUpdateJobResponse _
derive instance repGenericCreateSoftwareUpdateJobResponse :: Generic CreateSoftwareUpdateJobResponse _
instance showCreateSoftwareUpdateJobResponse :: Show CreateSoftwareUpdateJobResponse where
  show = genericShow
instance decodeCreateSoftwareUpdateJobResponse :: Decode CreateSoftwareUpdateJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSoftwareUpdateJobResponse :: Encode CreateSoftwareUpdateJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSubscriptionDefinitionRequest = CreateSubscriptionDefinitionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined.NullOrUndefined (SubscriptionDefinitionVersion)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateSubscriptionDefinitionRequest :: Newtype CreateSubscriptionDefinitionRequest _
derive instance repGenericCreateSubscriptionDefinitionRequest :: Generic CreateSubscriptionDefinitionRequest _
instance showCreateSubscriptionDefinitionRequest :: Show CreateSubscriptionDefinitionRequest where
  show = genericShow
instance decodeCreateSubscriptionDefinitionRequest :: Decode CreateSubscriptionDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSubscriptionDefinitionRequest :: Encode CreateSubscriptionDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSubscriptionDefinitionResponse = CreateSubscriptionDefinitionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateSubscriptionDefinitionResponse :: Newtype CreateSubscriptionDefinitionResponse _
derive instance repGenericCreateSubscriptionDefinitionResponse :: Generic CreateSubscriptionDefinitionResponse _
instance showCreateSubscriptionDefinitionResponse :: Show CreateSubscriptionDefinitionResponse where
  show = genericShow
instance decodeCreateSubscriptionDefinitionResponse :: Decode CreateSubscriptionDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSubscriptionDefinitionResponse :: Encode CreateSubscriptionDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSubscriptionDefinitionVersionRequest = CreateSubscriptionDefinitionVersionRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "SubscriptionDefinitionId" :: (String)
  , "Subscriptions" :: NullOrUndefined.NullOrUndefined (ListOfSubscription)
  }
derive instance newtypeCreateSubscriptionDefinitionVersionRequest :: Newtype CreateSubscriptionDefinitionVersionRequest _
derive instance repGenericCreateSubscriptionDefinitionVersionRequest :: Generic CreateSubscriptionDefinitionVersionRequest _
instance showCreateSubscriptionDefinitionVersionRequest :: Show CreateSubscriptionDefinitionVersionRequest where
  show = genericShow
instance decodeCreateSubscriptionDefinitionVersionRequest :: Decode CreateSubscriptionDefinitionVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSubscriptionDefinitionVersionRequest :: Encode CreateSubscriptionDefinitionVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSubscriptionDefinitionVersionResponse = CreateSubscriptionDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateSubscriptionDefinitionVersionResponse :: Newtype CreateSubscriptionDefinitionVersionResponse _
derive instance repGenericCreateSubscriptionDefinitionVersionResponse :: Generic CreateSubscriptionDefinitionVersionResponse _
instance showCreateSubscriptionDefinitionVersionResponse :: Show CreateSubscriptionDefinitionVersionResponse where
  show = genericShow
instance decodeCreateSubscriptionDefinitionVersionResponse :: Decode CreateSubscriptionDefinitionVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSubscriptionDefinitionVersionResponse :: Encode CreateSubscriptionDefinitionVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on the Definition
newtype DefinitionInformation = DefinitionInformation 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDefinitionInformation :: Newtype DefinitionInformation _
derive instance repGenericDefinitionInformation :: Generic DefinitionInformation _
instance showDefinitionInformation :: Show DefinitionInformation where
  show = genericShow
instance decodeDefinitionInformation :: Decode DefinitionInformation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefinitionInformation :: Encode DefinitionInformation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCoreDefinitionRequest = DeleteCoreDefinitionRequest 
  { "CoreDefinitionId" :: (String)
  }
derive instance newtypeDeleteCoreDefinitionRequest :: Newtype DeleteCoreDefinitionRequest _
derive instance repGenericDeleteCoreDefinitionRequest :: Generic DeleteCoreDefinitionRequest _
instance showDeleteCoreDefinitionRequest :: Show DeleteCoreDefinitionRequest where
  show = genericShow
instance decodeDeleteCoreDefinitionRequest :: Decode DeleteCoreDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCoreDefinitionRequest :: Encode DeleteCoreDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCoreDefinitionResponse = DeleteCoreDefinitionResponse Types.NoArguments
derive instance newtypeDeleteCoreDefinitionResponse :: Newtype DeleteCoreDefinitionResponse _
derive instance repGenericDeleteCoreDefinitionResponse :: Generic DeleteCoreDefinitionResponse _
instance showDeleteCoreDefinitionResponse :: Show DeleteCoreDefinitionResponse where
  show = genericShow
instance decodeDeleteCoreDefinitionResponse :: Decode DeleteCoreDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCoreDefinitionResponse :: Encode DeleteCoreDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDeviceDefinitionRequest = DeleteDeviceDefinitionRequest 
  { "DeviceDefinitionId" :: (String)
  }
derive instance newtypeDeleteDeviceDefinitionRequest :: Newtype DeleteDeviceDefinitionRequest _
derive instance repGenericDeleteDeviceDefinitionRequest :: Generic DeleteDeviceDefinitionRequest _
instance showDeleteDeviceDefinitionRequest :: Show DeleteDeviceDefinitionRequest where
  show = genericShow
instance decodeDeleteDeviceDefinitionRequest :: Decode DeleteDeviceDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDeviceDefinitionRequest :: Encode DeleteDeviceDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDeviceDefinitionResponse = DeleteDeviceDefinitionResponse Types.NoArguments
derive instance newtypeDeleteDeviceDefinitionResponse :: Newtype DeleteDeviceDefinitionResponse _
derive instance repGenericDeleteDeviceDefinitionResponse :: Generic DeleteDeviceDefinitionResponse _
instance showDeleteDeviceDefinitionResponse :: Show DeleteDeviceDefinitionResponse where
  show = genericShow
instance decodeDeleteDeviceDefinitionResponse :: Decode DeleteDeviceDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDeviceDefinitionResponse :: Encode DeleteDeviceDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteFunctionDefinitionRequest = DeleteFunctionDefinitionRequest 
  { "FunctionDefinitionId" :: (String)
  }
derive instance newtypeDeleteFunctionDefinitionRequest :: Newtype DeleteFunctionDefinitionRequest _
derive instance repGenericDeleteFunctionDefinitionRequest :: Generic DeleteFunctionDefinitionRequest _
instance showDeleteFunctionDefinitionRequest :: Show DeleteFunctionDefinitionRequest where
  show = genericShow
instance decodeDeleteFunctionDefinitionRequest :: Decode DeleteFunctionDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteFunctionDefinitionRequest :: Encode DeleteFunctionDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteFunctionDefinitionResponse = DeleteFunctionDefinitionResponse Types.NoArguments
derive instance newtypeDeleteFunctionDefinitionResponse :: Newtype DeleteFunctionDefinitionResponse _
derive instance repGenericDeleteFunctionDefinitionResponse :: Generic DeleteFunctionDefinitionResponse _
instance showDeleteFunctionDefinitionResponse :: Show DeleteFunctionDefinitionResponse where
  show = genericShow
instance decodeDeleteFunctionDefinitionResponse :: Decode DeleteFunctionDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteFunctionDefinitionResponse :: Encode DeleteFunctionDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGroupRequest = DeleteGroupRequest 
  { "GroupId" :: (String)
  }
derive instance newtypeDeleteGroupRequest :: Newtype DeleteGroupRequest _
derive instance repGenericDeleteGroupRequest :: Generic DeleteGroupRequest _
instance showDeleteGroupRequest :: Show DeleteGroupRequest where
  show = genericShow
instance decodeDeleteGroupRequest :: Decode DeleteGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGroupRequest :: Encode DeleteGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGroupResponse = DeleteGroupResponse Types.NoArguments
derive instance newtypeDeleteGroupResponse :: Newtype DeleteGroupResponse _
derive instance repGenericDeleteGroupResponse :: Generic DeleteGroupResponse _
instance showDeleteGroupResponse :: Show DeleteGroupResponse where
  show = genericShow
instance decodeDeleteGroupResponse :: Decode DeleteGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGroupResponse :: Encode DeleteGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLoggerDefinitionRequest = DeleteLoggerDefinitionRequest 
  { "LoggerDefinitionId" :: (String)
  }
derive instance newtypeDeleteLoggerDefinitionRequest :: Newtype DeleteLoggerDefinitionRequest _
derive instance repGenericDeleteLoggerDefinitionRequest :: Generic DeleteLoggerDefinitionRequest _
instance showDeleteLoggerDefinitionRequest :: Show DeleteLoggerDefinitionRequest where
  show = genericShow
instance decodeDeleteLoggerDefinitionRequest :: Decode DeleteLoggerDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLoggerDefinitionRequest :: Encode DeleteLoggerDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLoggerDefinitionResponse = DeleteLoggerDefinitionResponse Types.NoArguments
derive instance newtypeDeleteLoggerDefinitionResponse :: Newtype DeleteLoggerDefinitionResponse _
derive instance repGenericDeleteLoggerDefinitionResponse :: Generic DeleteLoggerDefinitionResponse _
instance showDeleteLoggerDefinitionResponse :: Show DeleteLoggerDefinitionResponse where
  show = genericShow
instance decodeDeleteLoggerDefinitionResponse :: Decode DeleteLoggerDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLoggerDefinitionResponse :: Encode DeleteLoggerDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteResourceDefinitionRequest = DeleteResourceDefinitionRequest 
  { "ResourceDefinitionId" :: (String)
  }
derive instance newtypeDeleteResourceDefinitionRequest :: Newtype DeleteResourceDefinitionRequest _
derive instance repGenericDeleteResourceDefinitionRequest :: Generic DeleteResourceDefinitionRequest _
instance showDeleteResourceDefinitionRequest :: Show DeleteResourceDefinitionRequest where
  show = genericShow
instance decodeDeleteResourceDefinitionRequest :: Decode DeleteResourceDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteResourceDefinitionRequest :: Encode DeleteResourceDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteResourceDefinitionResponse = DeleteResourceDefinitionResponse Types.NoArguments
derive instance newtypeDeleteResourceDefinitionResponse :: Newtype DeleteResourceDefinitionResponse _
derive instance repGenericDeleteResourceDefinitionResponse :: Generic DeleteResourceDefinitionResponse _
instance showDeleteResourceDefinitionResponse :: Show DeleteResourceDefinitionResponse where
  show = genericShow
instance decodeDeleteResourceDefinitionResponse :: Decode DeleteResourceDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteResourceDefinitionResponse :: Encode DeleteResourceDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSubscriptionDefinitionRequest = DeleteSubscriptionDefinitionRequest 
  { "SubscriptionDefinitionId" :: (String)
  }
derive instance newtypeDeleteSubscriptionDefinitionRequest :: Newtype DeleteSubscriptionDefinitionRequest _
derive instance repGenericDeleteSubscriptionDefinitionRequest :: Generic DeleteSubscriptionDefinitionRequest _
instance showDeleteSubscriptionDefinitionRequest :: Show DeleteSubscriptionDefinitionRequest where
  show = genericShow
instance decodeDeleteSubscriptionDefinitionRequest :: Decode DeleteSubscriptionDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSubscriptionDefinitionRequest :: Encode DeleteSubscriptionDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSubscriptionDefinitionResponse = DeleteSubscriptionDefinitionResponse Types.NoArguments
derive instance newtypeDeleteSubscriptionDefinitionResponse :: Newtype DeleteSubscriptionDefinitionResponse _
derive instance repGenericDeleteSubscriptionDefinitionResponse :: Generic DeleteSubscriptionDefinitionResponse _
instance showDeleteSubscriptionDefinitionResponse :: Show DeleteSubscriptionDefinitionResponse where
  show = genericShow
instance decodeDeleteSubscriptionDefinitionResponse :: Decode DeleteSubscriptionDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSubscriptionDefinitionResponse :: Encode DeleteSubscriptionDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on the deployment
newtype Deployment = Deployment 
  { "CreatedAt" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentArn" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentId" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentType" :: NullOrUndefined.NullOrUndefined (DeploymentType)
  , "GroupArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDeployment :: Newtype Deployment _
derive instance repGenericDeployment :: Generic Deployment _
instance showDeployment :: Show Deployment where
  show = genericShow
instance decodeDeployment :: Decode Deployment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeployment :: Encode Deployment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeploymentType = DeploymentType String
derive instance newtypeDeploymentType :: Newtype DeploymentType _
derive instance repGenericDeploymentType :: Generic DeploymentType _
instance showDeploymentType :: Show DeploymentType where
  show = genericShow
instance decodeDeploymentType :: Decode DeploymentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeploymentType :: Encode DeploymentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Deployments = Deployments (Array Deployment)
derive instance newtypeDeployments :: Newtype Deployments _
derive instance repGenericDeployments :: Generic Deployments _
instance showDeployments :: Show Deployments where
  show = genericShow
instance decodeDeployments :: Decode Deployments where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeployments :: Encode Deployments where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on a Device
newtype Device = Device 
  { "CertificateArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "SyncShadow" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ThingArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDevice :: Newtype Device _
derive instance repGenericDevice :: Generic Device _
instance showDevice :: Show Device where
  show = genericShow
instance decodeDevice :: Decode Device where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevice :: Encode Device where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on device definition version
newtype DeviceDefinitionVersion = DeviceDefinitionVersion 
  { "Devices" :: NullOrUndefined.NullOrUndefined (ListOfDevice)
  }
derive instance newtypeDeviceDefinitionVersion :: Newtype DeviceDefinitionVersion _
derive instance repGenericDeviceDefinitionVersion :: Generic DeviceDefinitionVersion _
instance showDeviceDefinitionVersion :: Show DeviceDefinitionVersion where
  show = genericShow
instance decodeDeviceDefinitionVersion :: Decode DeviceDefinitionVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceDefinitionVersion :: Encode DeviceDefinitionVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateRoleFromGroupRequest = DisassociateRoleFromGroupRequest 
  { "GroupId" :: (String)
  }
derive instance newtypeDisassociateRoleFromGroupRequest :: Newtype DisassociateRoleFromGroupRequest _
derive instance repGenericDisassociateRoleFromGroupRequest :: Generic DisassociateRoleFromGroupRequest _
instance showDisassociateRoleFromGroupRequest :: Show DisassociateRoleFromGroupRequest where
  show = genericShow
instance decodeDisassociateRoleFromGroupRequest :: Decode DisassociateRoleFromGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateRoleFromGroupRequest :: Encode DisassociateRoleFromGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateRoleFromGroupResponse = DisassociateRoleFromGroupResponse 
  { "DisassociatedAt" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDisassociateRoleFromGroupResponse :: Newtype DisassociateRoleFromGroupResponse _
derive instance repGenericDisassociateRoleFromGroupResponse :: Generic DisassociateRoleFromGroupResponse _
instance showDisassociateRoleFromGroupResponse :: Show DisassociateRoleFromGroupResponse where
  show = genericShow
instance decodeDisassociateRoleFromGroupResponse :: Decode DisassociateRoleFromGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateRoleFromGroupResponse :: Encode DisassociateRoleFromGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateServiceRoleFromAccountRequest = DisassociateServiceRoleFromAccountRequest Types.NoArguments
derive instance newtypeDisassociateServiceRoleFromAccountRequest :: Newtype DisassociateServiceRoleFromAccountRequest _
derive instance repGenericDisassociateServiceRoleFromAccountRequest :: Generic DisassociateServiceRoleFromAccountRequest _
instance showDisassociateServiceRoleFromAccountRequest :: Show DisassociateServiceRoleFromAccountRequest where
  show = genericShow
instance decodeDisassociateServiceRoleFromAccountRequest :: Decode DisassociateServiceRoleFromAccountRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateServiceRoleFromAccountRequest :: Encode DisassociateServiceRoleFromAccountRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateServiceRoleFromAccountResponse = DisassociateServiceRoleFromAccountResponse 
  { "DisassociatedAt" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDisassociateServiceRoleFromAccountResponse :: Newtype DisassociateServiceRoleFromAccountResponse _
derive instance repGenericDisassociateServiceRoleFromAccountResponse :: Generic DisassociateServiceRoleFromAccountResponse _
instance showDisassociateServiceRoleFromAccountResponse :: Show DisassociateServiceRoleFromAccountResponse where
  show = genericShow
instance decodeDisassociateServiceRoleFromAccountResponse :: Decode DisassociateServiceRoleFromAccountResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateServiceRoleFromAccountResponse :: Encode DisassociateServiceRoleFromAccountResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Empty
newtype Empty = Empty Types.NoArguments
derive instance newtypeEmpty :: Newtype Empty _
derive instance repGenericEmpty :: Generic Empty _
instance showEmpty :: Show Empty where
  show = genericShow
instance decodeEmpty :: Decode Empty where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmpty :: Encode Empty where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | ErrorDetail
newtype ErrorDetail = ErrorDetail 
  { "DetailedErrorCode" :: NullOrUndefined.NullOrUndefined (String)
  , "DetailedErrorMessage" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeErrorDetail :: Newtype ErrorDetail _
derive instance repGenericErrorDetail :: Generic ErrorDetail _
instance showErrorDetail :: Show ErrorDetail where
  show = genericShow
instance decodeErrorDetail :: Decode ErrorDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorDetail :: Encode ErrorDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Error Details
newtype ErrorDetails = ErrorDetails (Array ErrorDetail)
derive instance newtypeErrorDetails :: Newtype ErrorDetails _
derive instance repGenericErrorDetails :: Generic ErrorDetails _
instance showErrorDetails :: Show ErrorDetails where
  show = genericShow
instance decodeErrorDetails :: Decode ErrorDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorDetails :: Encode ErrorDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on function
newtype Function'' = Function'' 
  { "FunctionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "FunctionConfiguration" :: NullOrUndefined.NullOrUndefined (FunctionConfiguration)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeFunction'' :: Newtype Function'' _
derive instance repGenericFunction'' :: Generic Function'' _
instance showFunction'' :: Show Function'' where
  show = genericShow
instance decodeFunction'' :: Decode Function'' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFunction'' :: Encode Function'' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Configuration of the function
newtype FunctionConfiguration = FunctionConfiguration 
  { "Environment" :: NullOrUndefined.NullOrUndefined (FunctionConfigurationEnvironment)
  , "ExecArgs" :: NullOrUndefined.NullOrUndefined (String)
  , "Executable" :: NullOrUndefined.NullOrUndefined (String)
  , "MemorySize" :: NullOrUndefined.NullOrUndefined (Int)
  , "Pinned" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Timeout" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeFunctionConfiguration :: Newtype FunctionConfiguration _
derive instance repGenericFunctionConfiguration :: Generic FunctionConfiguration _
instance showFunctionConfiguration :: Show FunctionConfiguration where
  show = genericShow
instance decodeFunctionConfiguration :: Decode FunctionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFunctionConfiguration :: Encode FunctionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Environment of the function configuration
newtype FunctionConfigurationEnvironment = FunctionConfigurationEnvironment 
  { "AccessSysfs" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ResourceAccessPolicies" :: NullOrUndefined.NullOrUndefined (ListOfResourceAccessPolicy)
  , "Variables" :: NullOrUndefined.NullOrUndefined (MapOf__string)
  }
derive instance newtypeFunctionConfigurationEnvironment :: Newtype FunctionConfigurationEnvironment _
derive instance repGenericFunctionConfigurationEnvironment :: Generic FunctionConfigurationEnvironment _
instance showFunctionConfigurationEnvironment :: Show FunctionConfigurationEnvironment where
  show = genericShow
instance decodeFunctionConfigurationEnvironment :: Decode FunctionConfigurationEnvironment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFunctionConfigurationEnvironment :: Encode FunctionConfigurationEnvironment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on the function definition version
newtype FunctionDefinitionVersion = FunctionDefinitionVersion 
  { "Functions" :: NullOrUndefined.NullOrUndefined (ListOfFunction)
  }
derive instance newtypeFunctionDefinitionVersion :: Newtype FunctionDefinitionVersion _
derive instance repGenericFunctionDefinitionVersion :: Generic FunctionDefinitionVersion _
instance showFunctionDefinitionVersion :: Show FunctionDefinitionVersion where
  show = genericShow
instance decodeFunctionDefinitionVersion :: Decode FunctionDefinitionVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFunctionDefinitionVersion :: Encode FunctionDefinitionVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | General Error
newtype GeneralError = GeneralError 
  { "ErrorDetails" :: NullOrUndefined.NullOrUndefined (ErrorDetails)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGeneralError :: Newtype GeneralError _
derive instance repGenericGeneralError :: Generic GeneralError _
instance showGeneralError :: Show GeneralError where
  show = genericShow
instance decodeGeneralError :: Decode GeneralError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeneralError :: Encode GeneralError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAssociatedRoleRequest = GetAssociatedRoleRequest 
  { "GroupId" :: (String)
  }
derive instance newtypeGetAssociatedRoleRequest :: Newtype GetAssociatedRoleRequest _
derive instance repGenericGetAssociatedRoleRequest :: Generic GetAssociatedRoleRequest _
instance showGetAssociatedRoleRequest :: Show GetAssociatedRoleRequest where
  show = genericShow
instance decodeGetAssociatedRoleRequest :: Decode GetAssociatedRoleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAssociatedRoleRequest :: Encode GetAssociatedRoleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAssociatedRoleResponse = GetAssociatedRoleResponse 
  { "AssociatedAt" :: NullOrUndefined.NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetAssociatedRoleResponse :: Newtype GetAssociatedRoleResponse _
derive instance repGenericGetAssociatedRoleResponse :: Generic GetAssociatedRoleResponse _
instance showGetAssociatedRoleResponse :: Show GetAssociatedRoleResponse where
  show = genericShow
instance decodeGetAssociatedRoleResponse :: Decode GetAssociatedRoleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAssociatedRoleResponse :: Encode GetAssociatedRoleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetConnectivityInfoRequest = GetConnectivityInfoRequest 
  { "ThingName" :: (String)
  }
derive instance newtypeGetConnectivityInfoRequest :: Newtype GetConnectivityInfoRequest _
derive instance repGenericGetConnectivityInfoRequest :: Generic GetConnectivityInfoRequest _
instance showGetConnectivityInfoRequest :: Show GetConnectivityInfoRequest where
  show = genericShow
instance decodeGetConnectivityInfoRequest :: Decode GetConnectivityInfoRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetConnectivityInfoRequest :: Encode GetConnectivityInfoRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetConnectivityInfoResponse = GetConnectivityInfoResponse 
  { "ConnectivityInfo" :: NullOrUndefined.NullOrUndefined (ListOfConnectivityInfo)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetConnectivityInfoResponse :: Newtype GetConnectivityInfoResponse _
derive instance repGenericGetConnectivityInfoResponse :: Generic GetConnectivityInfoResponse _
instance showGetConnectivityInfoResponse :: Show GetConnectivityInfoResponse where
  show = genericShow
instance decodeGetConnectivityInfoResponse :: Decode GetConnectivityInfoResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetConnectivityInfoResponse :: Encode GetConnectivityInfoResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCoreDefinitionRequest = GetCoreDefinitionRequest 
  { "CoreDefinitionId" :: (String)
  }
derive instance newtypeGetCoreDefinitionRequest :: Newtype GetCoreDefinitionRequest _
derive instance repGenericGetCoreDefinitionRequest :: Generic GetCoreDefinitionRequest _
instance showGetCoreDefinitionRequest :: Show GetCoreDefinitionRequest where
  show = genericShow
instance decodeGetCoreDefinitionRequest :: Decode GetCoreDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCoreDefinitionRequest :: Encode GetCoreDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCoreDefinitionResponse = GetCoreDefinitionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetCoreDefinitionResponse :: Newtype GetCoreDefinitionResponse _
derive instance repGenericGetCoreDefinitionResponse :: Generic GetCoreDefinitionResponse _
instance showGetCoreDefinitionResponse :: Show GetCoreDefinitionResponse where
  show = genericShow
instance decodeGetCoreDefinitionResponse :: Decode GetCoreDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCoreDefinitionResponse :: Encode GetCoreDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCoreDefinitionVersionRequest = GetCoreDefinitionVersionRequest 
  { "CoreDefinitionId" :: (String)
  , "CoreDefinitionVersionId" :: (String)
  }
derive instance newtypeGetCoreDefinitionVersionRequest :: Newtype GetCoreDefinitionVersionRequest _
derive instance repGenericGetCoreDefinitionVersionRequest :: Generic GetCoreDefinitionVersionRequest _
instance showGetCoreDefinitionVersionRequest :: Show GetCoreDefinitionVersionRequest where
  show = genericShow
instance decodeGetCoreDefinitionVersionRequest :: Decode GetCoreDefinitionVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCoreDefinitionVersionRequest :: Encode GetCoreDefinitionVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCoreDefinitionVersionResponse = GetCoreDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Definition" :: NullOrUndefined.NullOrUndefined (CoreDefinitionVersion)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetCoreDefinitionVersionResponse :: Newtype GetCoreDefinitionVersionResponse _
derive instance repGenericGetCoreDefinitionVersionResponse :: Generic GetCoreDefinitionVersionResponse _
instance showGetCoreDefinitionVersionResponse :: Show GetCoreDefinitionVersionResponse where
  show = genericShow
instance decodeGetCoreDefinitionVersionResponse :: Decode GetCoreDefinitionVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCoreDefinitionVersionResponse :: Encode GetCoreDefinitionVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDeploymentStatusRequest = GetDeploymentStatusRequest 
  { "DeploymentId" :: (String)
  , "GroupId" :: (String)
  }
derive instance newtypeGetDeploymentStatusRequest :: Newtype GetDeploymentStatusRequest _
derive instance repGenericGetDeploymentStatusRequest :: Generic GetDeploymentStatusRequest _
instance showGetDeploymentStatusRequest :: Show GetDeploymentStatusRequest where
  show = genericShow
instance decodeGetDeploymentStatusRequest :: Decode GetDeploymentStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeploymentStatusRequest :: Encode GetDeploymentStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDeploymentStatusResponse = GetDeploymentStatusResponse 
  { "DeploymentStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentType" :: NullOrUndefined.NullOrUndefined (DeploymentType)
  , "ErrorDetails" :: NullOrUndefined.NullOrUndefined (ErrorDetails)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (String)
  , "UpdatedAt" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetDeploymentStatusResponse :: Newtype GetDeploymentStatusResponse _
derive instance repGenericGetDeploymentStatusResponse :: Generic GetDeploymentStatusResponse _
instance showGetDeploymentStatusResponse :: Show GetDeploymentStatusResponse where
  show = genericShow
instance decodeGetDeploymentStatusResponse :: Decode GetDeploymentStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeploymentStatusResponse :: Encode GetDeploymentStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDeviceDefinitionRequest = GetDeviceDefinitionRequest 
  { "DeviceDefinitionId" :: (String)
  }
derive instance newtypeGetDeviceDefinitionRequest :: Newtype GetDeviceDefinitionRequest _
derive instance repGenericGetDeviceDefinitionRequest :: Generic GetDeviceDefinitionRequest _
instance showGetDeviceDefinitionRequest :: Show GetDeviceDefinitionRequest where
  show = genericShow
instance decodeGetDeviceDefinitionRequest :: Decode GetDeviceDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeviceDefinitionRequest :: Encode GetDeviceDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDeviceDefinitionResponse = GetDeviceDefinitionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetDeviceDefinitionResponse :: Newtype GetDeviceDefinitionResponse _
derive instance repGenericGetDeviceDefinitionResponse :: Generic GetDeviceDefinitionResponse _
instance showGetDeviceDefinitionResponse :: Show GetDeviceDefinitionResponse where
  show = genericShow
instance decodeGetDeviceDefinitionResponse :: Decode GetDeviceDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeviceDefinitionResponse :: Encode GetDeviceDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDeviceDefinitionVersionRequest = GetDeviceDefinitionVersionRequest 
  { "DeviceDefinitionId" :: (String)
  , "DeviceDefinitionVersionId" :: (String)
  }
derive instance newtypeGetDeviceDefinitionVersionRequest :: Newtype GetDeviceDefinitionVersionRequest _
derive instance repGenericGetDeviceDefinitionVersionRequest :: Generic GetDeviceDefinitionVersionRequest _
instance showGetDeviceDefinitionVersionRequest :: Show GetDeviceDefinitionVersionRequest where
  show = genericShow
instance decodeGetDeviceDefinitionVersionRequest :: Decode GetDeviceDefinitionVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeviceDefinitionVersionRequest :: Encode GetDeviceDefinitionVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDeviceDefinitionVersionResponse = GetDeviceDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Definition" :: NullOrUndefined.NullOrUndefined (DeviceDefinitionVersion)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetDeviceDefinitionVersionResponse :: Newtype GetDeviceDefinitionVersionResponse _
derive instance repGenericGetDeviceDefinitionVersionResponse :: Generic GetDeviceDefinitionVersionResponse _
instance showGetDeviceDefinitionVersionResponse :: Show GetDeviceDefinitionVersionResponse where
  show = genericShow
instance decodeGetDeviceDefinitionVersionResponse :: Decode GetDeviceDefinitionVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeviceDefinitionVersionResponse :: Encode GetDeviceDefinitionVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFunctionDefinitionRequest = GetFunctionDefinitionRequest 
  { "FunctionDefinitionId" :: (String)
  }
derive instance newtypeGetFunctionDefinitionRequest :: Newtype GetFunctionDefinitionRequest _
derive instance repGenericGetFunctionDefinitionRequest :: Generic GetFunctionDefinitionRequest _
instance showGetFunctionDefinitionRequest :: Show GetFunctionDefinitionRequest where
  show = genericShow
instance decodeGetFunctionDefinitionRequest :: Decode GetFunctionDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFunctionDefinitionRequest :: Encode GetFunctionDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFunctionDefinitionResponse = GetFunctionDefinitionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetFunctionDefinitionResponse :: Newtype GetFunctionDefinitionResponse _
derive instance repGenericGetFunctionDefinitionResponse :: Generic GetFunctionDefinitionResponse _
instance showGetFunctionDefinitionResponse :: Show GetFunctionDefinitionResponse where
  show = genericShow
instance decodeGetFunctionDefinitionResponse :: Decode GetFunctionDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFunctionDefinitionResponse :: Encode GetFunctionDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFunctionDefinitionVersionRequest = GetFunctionDefinitionVersionRequest 
  { "FunctionDefinitionId" :: (String)
  , "FunctionDefinitionVersionId" :: (String)
  }
derive instance newtypeGetFunctionDefinitionVersionRequest :: Newtype GetFunctionDefinitionVersionRequest _
derive instance repGenericGetFunctionDefinitionVersionRequest :: Generic GetFunctionDefinitionVersionRequest _
instance showGetFunctionDefinitionVersionRequest :: Show GetFunctionDefinitionVersionRequest where
  show = genericShow
instance decodeGetFunctionDefinitionVersionRequest :: Decode GetFunctionDefinitionVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFunctionDefinitionVersionRequest :: Encode GetFunctionDefinitionVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFunctionDefinitionVersionResponse = GetFunctionDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Definition" :: NullOrUndefined.NullOrUndefined (FunctionDefinitionVersion)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetFunctionDefinitionVersionResponse :: Newtype GetFunctionDefinitionVersionResponse _
derive instance repGenericGetFunctionDefinitionVersionResponse :: Generic GetFunctionDefinitionVersionResponse _
instance showGetFunctionDefinitionVersionResponse :: Show GetFunctionDefinitionVersionResponse where
  show = genericShow
instance decodeGetFunctionDefinitionVersionResponse :: Decode GetFunctionDefinitionVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFunctionDefinitionVersionResponse :: Encode GetFunctionDefinitionVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupCertificateAuthorityRequest = GetGroupCertificateAuthorityRequest 
  { "CertificateAuthorityId" :: (String)
  , "GroupId" :: (String)
  }
derive instance newtypeGetGroupCertificateAuthorityRequest :: Newtype GetGroupCertificateAuthorityRequest _
derive instance repGenericGetGroupCertificateAuthorityRequest :: Generic GetGroupCertificateAuthorityRequest _
instance showGetGroupCertificateAuthorityRequest :: Show GetGroupCertificateAuthorityRequest where
  show = genericShow
instance decodeGetGroupCertificateAuthorityRequest :: Decode GetGroupCertificateAuthorityRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupCertificateAuthorityRequest :: Encode GetGroupCertificateAuthorityRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupCertificateAuthorityResponse = GetGroupCertificateAuthorityResponse 
  { "GroupCertificateAuthorityArn" :: NullOrUndefined.NullOrUndefined (String)
  , "GroupCertificateAuthorityId" :: NullOrUndefined.NullOrUndefined (String)
  , "PemEncodedCertificate" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetGroupCertificateAuthorityResponse :: Newtype GetGroupCertificateAuthorityResponse _
derive instance repGenericGetGroupCertificateAuthorityResponse :: Generic GetGroupCertificateAuthorityResponse _
instance showGetGroupCertificateAuthorityResponse :: Show GetGroupCertificateAuthorityResponse where
  show = genericShow
instance decodeGetGroupCertificateAuthorityResponse :: Decode GetGroupCertificateAuthorityResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupCertificateAuthorityResponse :: Encode GetGroupCertificateAuthorityResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupCertificateConfigurationRequest = GetGroupCertificateConfigurationRequest 
  { "GroupId" :: (String)
  }
derive instance newtypeGetGroupCertificateConfigurationRequest :: Newtype GetGroupCertificateConfigurationRequest _
derive instance repGenericGetGroupCertificateConfigurationRequest :: Generic GetGroupCertificateConfigurationRequest _
instance showGetGroupCertificateConfigurationRequest :: Show GetGroupCertificateConfigurationRequest where
  show = genericShow
instance decodeGetGroupCertificateConfigurationRequest :: Decode GetGroupCertificateConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupCertificateConfigurationRequest :: Encode GetGroupCertificateConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupCertificateConfigurationResponse = GetGroupCertificateConfigurationResponse 
  { "CertificateAuthorityExpiryInMilliseconds" :: NullOrUndefined.NullOrUndefined (String)
  , "CertificateExpiryInMilliseconds" :: NullOrUndefined.NullOrUndefined (String)
  , "GroupId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetGroupCertificateConfigurationResponse :: Newtype GetGroupCertificateConfigurationResponse _
derive instance repGenericGetGroupCertificateConfigurationResponse :: Generic GetGroupCertificateConfigurationResponse _
instance showGetGroupCertificateConfigurationResponse :: Show GetGroupCertificateConfigurationResponse where
  show = genericShow
instance decodeGetGroupCertificateConfigurationResponse :: Decode GetGroupCertificateConfigurationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupCertificateConfigurationResponse :: Encode GetGroupCertificateConfigurationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupRequest = GetGroupRequest 
  { "GroupId" :: (String)
  }
derive instance newtypeGetGroupRequest :: Newtype GetGroupRequest _
derive instance repGenericGetGroupRequest :: Generic GetGroupRequest _
instance showGetGroupRequest :: Show GetGroupRequest where
  show = genericShow
instance decodeGetGroupRequest :: Decode GetGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupRequest :: Encode GetGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupResponse = GetGroupResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetGroupResponse :: Newtype GetGroupResponse _
derive instance repGenericGetGroupResponse :: Generic GetGroupResponse _
instance showGetGroupResponse :: Show GetGroupResponse where
  show = genericShow
instance decodeGetGroupResponse :: Decode GetGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupResponse :: Encode GetGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupVersionRequest = GetGroupVersionRequest 
  { "GroupId" :: (String)
  , "GroupVersionId" :: (String)
  }
derive instance newtypeGetGroupVersionRequest :: Newtype GetGroupVersionRequest _
derive instance repGenericGetGroupVersionRequest :: Generic GetGroupVersionRequest _
instance showGetGroupVersionRequest :: Show GetGroupVersionRequest where
  show = genericShow
instance decodeGetGroupVersionRequest :: Decode GetGroupVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupVersionRequest :: Encode GetGroupVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupVersionResponse = GetGroupVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Definition" :: NullOrUndefined.NullOrUndefined (GroupVersion)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetGroupVersionResponse :: Newtype GetGroupVersionResponse _
derive instance repGenericGetGroupVersionResponse :: Generic GetGroupVersionResponse _
instance showGetGroupVersionResponse :: Show GetGroupVersionResponse where
  show = genericShow
instance decodeGetGroupVersionResponse :: Decode GetGroupVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupVersionResponse :: Encode GetGroupVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLoggerDefinitionRequest = GetLoggerDefinitionRequest 
  { "LoggerDefinitionId" :: (String)
  }
derive instance newtypeGetLoggerDefinitionRequest :: Newtype GetLoggerDefinitionRequest _
derive instance repGenericGetLoggerDefinitionRequest :: Generic GetLoggerDefinitionRequest _
instance showGetLoggerDefinitionRequest :: Show GetLoggerDefinitionRequest where
  show = genericShow
instance decodeGetLoggerDefinitionRequest :: Decode GetLoggerDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoggerDefinitionRequest :: Encode GetLoggerDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLoggerDefinitionResponse = GetLoggerDefinitionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetLoggerDefinitionResponse :: Newtype GetLoggerDefinitionResponse _
derive instance repGenericGetLoggerDefinitionResponse :: Generic GetLoggerDefinitionResponse _
instance showGetLoggerDefinitionResponse :: Show GetLoggerDefinitionResponse where
  show = genericShow
instance decodeGetLoggerDefinitionResponse :: Decode GetLoggerDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoggerDefinitionResponse :: Encode GetLoggerDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLoggerDefinitionVersionRequest = GetLoggerDefinitionVersionRequest 
  { "LoggerDefinitionId" :: (String)
  , "LoggerDefinitionVersionId" :: (String)
  }
derive instance newtypeGetLoggerDefinitionVersionRequest :: Newtype GetLoggerDefinitionVersionRequest _
derive instance repGenericGetLoggerDefinitionVersionRequest :: Generic GetLoggerDefinitionVersionRequest _
instance showGetLoggerDefinitionVersionRequest :: Show GetLoggerDefinitionVersionRequest where
  show = genericShow
instance decodeGetLoggerDefinitionVersionRequest :: Decode GetLoggerDefinitionVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoggerDefinitionVersionRequest :: Encode GetLoggerDefinitionVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLoggerDefinitionVersionResponse = GetLoggerDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Definition" :: NullOrUndefined.NullOrUndefined (LoggerDefinitionVersion)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetLoggerDefinitionVersionResponse :: Newtype GetLoggerDefinitionVersionResponse _
derive instance repGenericGetLoggerDefinitionVersionResponse :: Generic GetLoggerDefinitionVersionResponse _
instance showGetLoggerDefinitionVersionResponse :: Show GetLoggerDefinitionVersionResponse where
  show = genericShow
instance decodeGetLoggerDefinitionVersionResponse :: Decode GetLoggerDefinitionVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoggerDefinitionVersionResponse :: Encode GetLoggerDefinitionVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetResourceDefinitionRequest = GetResourceDefinitionRequest 
  { "ResourceDefinitionId" :: (String)
  }
derive instance newtypeGetResourceDefinitionRequest :: Newtype GetResourceDefinitionRequest _
derive instance repGenericGetResourceDefinitionRequest :: Generic GetResourceDefinitionRequest _
instance showGetResourceDefinitionRequest :: Show GetResourceDefinitionRequest where
  show = genericShow
instance decodeGetResourceDefinitionRequest :: Decode GetResourceDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetResourceDefinitionRequest :: Encode GetResourceDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetResourceDefinitionResponse = GetResourceDefinitionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetResourceDefinitionResponse :: Newtype GetResourceDefinitionResponse _
derive instance repGenericGetResourceDefinitionResponse :: Generic GetResourceDefinitionResponse _
instance showGetResourceDefinitionResponse :: Show GetResourceDefinitionResponse where
  show = genericShow
instance decodeGetResourceDefinitionResponse :: Decode GetResourceDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetResourceDefinitionResponse :: Encode GetResourceDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetResourceDefinitionVersionRequest = GetResourceDefinitionVersionRequest 
  { "ResourceDefinitionId" :: (String)
  , "ResourceDefinitionVersionId" :: (String)
  }
derive instance newtypeGetResourceDefinitionVersionRequest :: Newtype GetResourceDefinitionVersionRequest _
derive instance repGenericGetResourceDefinitionVersionRequest :: Generic GetResourceDefinitionVersionRequest _
instance showGetResourceDefinitionVersionRequest :: Show GetResourceDefinitionVersionRequest where
  show = genericShow
instance decodeGetResourceDefinitionVersionRequest :: Decode GetResourceDefinitionVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetResourceDefinitionVersionRequest :: Encode GetResourceDefinitionVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetResourceDefinitionVersionResponse = GetResourceDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Definition" :: NullOrUndefined.NullOrUndefined (ResourceDefinitionVersion)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetResourceDefinitionVersionResponse :: Newtype GetResourceDefinitionVersionResponse _
derive instance repGenericGetResourceDefinitionVersionResponse :: Generic GetResourceDefinitionVersionResponse _
instance showGetResourceDefinitionVersionResponse :: Show GetResourceDefinitionVersionResponse where
  show = genericShow
instance decodeGetResourceDefinitionVersionResponse :: Decode GetResourceDefinitionVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetResourceDefinitionVersionResponse :: Encode GetResourceDefinitionVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetServiceRoleForAccountRequest = GetServiceRoleForAccountRequest Types.NoArguments
derive instance newtypeGetServiceRoleForAccountRequest :: Newtype GetServiceRoleForAccountRequest _
derive instance repGenericGetServiceRoleForAccountRequest :: Generic GetServiceRoleForAccountRequest _
instance showGetServiceRoleForAccountRequest :: Show GetServiceRoleForAccountRequest where
  show = genericShow
instance decodeGetServiceRoleForAccountRequest :: Decode GetServiceRoleForAccountRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetServiceRoleForAccountRequest :: Encode GetServiceRoleForAccountRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetServiceRoleForAccountResponse = GetServiceRoleForAccountResponse 
  { "AssociatedAt" :: NullOrUndefined.NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetServiceRoleForAccountResponse :: Newtype GetServiceRoleForAccountResponse _
derive instance repGenericGetServiceRoleForAccountResponse :: Generic GetServiceRoleForAccountResponse _
instance showGetServiceRoleForAccountResponse :: Show GetServiceRoleForAccountResponse where
  show = genericShow
instance decodeGetServiceRoleForAccountResponse :: Decode GetServiceRoleForAccountResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetServiceRoleForAccountResponse :: Encode GetServiceRoleForAccountResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSubscriptionDefinitionRequest = GetSubscriptionDefinitionRequest 
  { "SubscriptionDefinitionId" :: (String)
  }
derive instance newtypeGetSubscriptionDefinitionRequest :: Newtype GetSubscriptionDefinitionRequest _
derive instance repGenericGetSubscriptionDefinitionRequest :: Generic GetSubscriptionDefinitionRequest _
instance showGetSubscriptionDefinitionRequest :: Show GetSubscriptionDefinitionRequest where
  show = genericShow
instance decodeGetSubscriptionDefinitionRequest :: Decode GetSubscriptionDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSubscriptionDefinitionRequest :: Encode GetSubscriptionDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSubscriptionDefinitionResponse = GetSubscriptionDefinitionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetSubscriptionDefinitionResponse :: Newtype GetSubscriptionDefinitionResponse _
derive instance repGenericGetSubscriptionDefinitionResponse :: Generic GetSubscriptionDefinitionResponse _
instance showGetSubscriptionDefinitionResponse :: Show GetSubscriptionDefinitionResponse where
  show = genericShow
instance decodeGetSubscriptionDefinitionResponse :: Decode GetSubscriptionDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSubscriptionDefinitionResponse :: Encode GetSubscriptionDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSubscriptionDefinitionVersionRequest = GetSubscriptionDefinitionVersionRequest 
  { "SubscriptionDefinitionId" :: (String)
  , "SubscriptionDefinitionVersionId" :: (String)
  }
derive instance newtypeGetSubscriptionDefinitionVersionRequest :: Newtype GetSubscriptionDefinitionVersionRequest _
derive instance repGenericGetSubscriptionDefinitionVersionRequest :: Generic GetSubscriptionDefinitionVersionRequest _
instance showGetSubscriptionDefinitionVersionRequest :: Show GetSubscriptionDefinitionVersionRequest where
  show = genericShow
instance decodeGetSubscriptionDefinitionVersionRequest :: Decode GetSubscriptionDefinitionVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSubscriptionDefinitionVersionRequest :: Encode GetSubscriptionDefinitionVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSubscriptionDefinitionVersionResponse = GetSubscriptionDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Definition" :: NullOrUndefined.NullOrUndefined (SubscriptionDefinitionVersion)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetSubscriptionDefinitionVersionResponse :: Newtype GetSubscriptionDefinitionVersionResponse _
derive instance repGenericGetSubscriptionDefinitionVersionResponse :: Generic GetSubscriptionDefinitionVersionResponse _
instance showGetSubscriptionDefinitionVersionResponse :: Show GetSubscriptionDefinitionVersionResponse where
  show = genericShow
instance decodeGetSubscriptionDefinitionVersionResponse :: Decode GetSubscriptionDefinitionVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSubscriptionDefinitionVersionResponse :: Encode GetSubscriptionDefinitionVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on group certificate authority properties
newtype GroupCertificateAuthorityProperties = GroupCertificateAuthorityProperties 
  { "GroupCertificateAuthorityArn" :: NullOrUndefined.NullOrUndefined (String)
  , "GroupCertificateAuthorityId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGroupCertificateAuthorityProperties :: Newtype GroupCertificateAuthorityProperties _
derive instance repGenericGroupCertificateAuthorityProperties :: Generic GroupCertificateAuthorityProperties _
instance showGroupCertificateAuthorityProperties :: Show GroupCertificateAuthorityProperties where
  show = genericShow
instance decodeGroupCertificateAuthorityProperties :: Decode GroupCertificateAuthorityProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupCertificateAuthorityProperties :: Encode GroupCertificateAuthorityProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on the group certificate configuration
newtype GroupCertificateConfiguration = GroupCertificateConfiguration 
  { "CertificateAuthorityExpiryInMilliseconds" :: NullOrUndefined.NullOrUndefined (String)
  , "CertificateExpiryInMilliseconds" :: NullOrUndefined.NullOrUndefined (String)
  , "GroupId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGroupCertificateConfiguration :: Newtype GroupCertificateConfiguration _
derive instance repGenericGroupCertificateConfiguration :: Generic GroupCertificateConfiguration _
instance showGroupCertificateConfiguration :: Show GroupCertificateConfiguration where
  show = genericShow
instance decodeGroupCertificateConfiguration :: Decode GroupCertificateConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupCertificateConfiguration :: Encode GroupCertificateConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on the group
newtype GroupInformation = GroupInformation 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGroupInformation :: Newtype GroupInformation _
derive instance repGenericGroupInformation :: Generic GroupInformation _
instance showGroupInformation :: Show GroupInformation where
  show = genericShow
instance decodeGroupInformation :: Decode GroupInformation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupInformation :: Encode GroupInformation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Group owner related settings for local resources.
newtype GroupOwnerSetting = GroupOwnerSetting 
  { "AutoAddGroupOwner" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "GroupOwner" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGroupOwnerSetting :: Newtype GroupOwnerSetting _
derive instance repGenericGroupOwnerSetting :: Generic GroupOwnerSetting _
instance showGroupOwnerSetting :: Show GroupOwnerSetting where
  show = genericShow
instance decodeGroupOwnerSetting :: Decode GroupOwnerSetting where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupOwnerSetting :: Encode GroupOwnerSetting where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on group version
newtype GroupVersion = GroupVersion 
  { "CoreDefinitionVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "DeviceDefinitionVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "FunctionDefinitionVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "LoggerDefinitionVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "ResourceDefinitionVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "SubscriptionDefinitionVersionArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGroupVersion :: Newtype GroupVersion _
derive instance repGenericGroupVersion :: Generic GroupVersion _
instance showGroupVersion :: Show GroupVersion where
  show = genericShow
instance decodeGroupVersion :: Decode GroupVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupVersion :: Encode GroupVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | General Error
newtype InternalServerErrorException = InternalServerErrorException 
  { "ErrorDetails" :: NullOrUndefined.NullOrUndefined (ErrorDetails)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _
derive instance repGenericInternalServerErrorException :: Generic InternalServerErrorException _
instance showInternalServerErrorException :: Show InternalServerErrorException where
  show = genericShow
instance decodeInternalServerErrorException :: Decode InternalServerErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerErrorException :: Encode InternalServerErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCoreDefinitionVersionsRequest = ListCoreDefinitionVersionsRequest 
  { "CoreDefinitionId" :: (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListCoreDefinitionVersionsRequest :: Newtype ListCoreDefinitionVersionsRequest _
derive instance repGenericListCoreDefinitionVersionsRequest :: Generic ListCoreDefinitionVersionsRequest _
instance showListCoreDefinitionVersionsRequest :: Show ListCoreDefinitionVersionsRequest where
  show = genericShow
instance decodeListCoreDefinitionVersionsRequest :: Decode ListCoreDefinitionVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCoreDefinitionVersionsRequest :: Encode ListCoreDefinitionVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCoreDefinitionVersionsResponse = ListCoreDefinitionVersionsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Versions" :: NullOrUndefined.NullOrUndefined (ListOfVersionInformation)
  }
derive instance newtypeListCoreDefinitionVersionsResponse :: Newtype ListCoreDefinitionVersionsResponse _
derive instance repGenericListCoreDefinitionVersionsResponse :: Generic ListCoreDefinitionVersionsResponse _
instance showListCoreDefinitionVersionsResponse :: Show ListCoreDefinitionVersionsResponse where
  show = genericShow
instance decodeListCoreDefinitionVersionsResponse :: Decode ListCoreDefinitionVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCoreDefinitionVersionsResponse :: Encode ListCoreDefinitionVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCoreDefinitionsRequest = ListCoreDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListCoreDefinitionsRequest :: Newtype ListCoreDefinitionsRequest _
derive instance repGenericListCoreDefinitionsRequest :: Generic ListCoreDefinitionsRequest _
instance showListCoreDefinitionsRequest :: Show ListCoreDefinitionsRequest where
  show = genericShow
instance decodeListCoreDefinitionsRequest :: Decode ListCoreDefinitionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCoreDefinitionsRequest :: Encode ListCoreDefinitionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCoreDefinitionsResponse = ListCoreDefinitionsResponse 
  { "Definitions" :: NullOrUndefined.NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListCoreDefinitionsResponse :: Newtype ListCoreDefinitionsResponse _
derive instance repGenericListCoreDefinitionsResponse :: Generic ListCoreDefinitionsResponse _
instance showListCoreDefinitionsResponse :: Show ListCoreDefinitionsResponse where
  show = genericShow
instance decodeListCoreDefinitionsResponse :: Decode ListCoreDefinitionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCoreDefinitionsResponse :: Encode ListCoreDefinitionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | List of definition responses
newtype ListDefinitionsResponse = ListDefinitionsResponse 
  { "Definitions" :: NullOrUndefined.NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListDefinitionsResponse :: Newtype ListDefinitionsResponse _
derive instance repGenericListDefinitionsResponse :: Generic ListDefinitionsResponse _
instance showListDefinitionsResponse :: Show ListDefinitionsResponse where
  show = genericShow
instance decodeListDefinitionsResponse :: Decode ListDefinitionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDefinitionsResponse :: Encode ListDefinitionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDeploymentsRequest = ListDeploymentsRequest 
  { "GroupId" :: (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListDeploymentsRequest :: Newtype ListDeploymentsRequest _
derive instance repGenericListDeploymentsRequest :: Generic ListDeploymentsRequest _
instance showListDeploymentsRequest :: Show ListDeploymentsRequest where
  show = genericShow
instance decodeListDeploymentsRequest :: Decode ListDeploymentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDeploymentsRequest :: Encode ListDeploymentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDeploymentsResponse = ListDeploymentsResponse 
  { "Deployments" :: NullOrUndefined.NullOrUndefined (Deployments)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListDeploymentsResponse :: Newtype ListDeploymentsResponse _
derive instance repGenericListDeploymentsResponse :: Generic ListDeploymentsResponse _
instance showListDeploymentsResponse :: Show ListDeploymentsResponse where
  show = genericShow
instance decodeListDeploymentsResponse :: Decode ListDeploymentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDeploymentsResponse :: Encode ListDeploymentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDeviceDefinitionVersionsRequest = ListDeviceDefinitionVersionsRequest 
  { "DeviceDefinitionId" :: (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListDeviceDefinitionVersionsRequest :: Newtype ListDeviceDefinitionVersionsRequest _
derive instance repGenericListDeviceDefinitionVersionsRequest :: Generic ListDeviceDefinitionVersionsRequest _
instance showListDeviceDefinitionVersionsRequest :: Show ListDeviceDefinitionVersionsRequest where
  show = genericShow
instance decodeListDeviceDefinitionVersionsRequest :: Decode ListDeviceDefinitionVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDeviceDefinitionVersionsRequest :: Encode ListDeviceDefinitionVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDeviceDefinitionVersionsResponse = ListDeviceDefinitionVersionsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Versions" :: NullOrUndefined.NullOrUndefined (ListOfVersionInformation)
  }
derive instance newtypeListDeviceDefinitionVersionsResponse :: Newtype ListDeviceDefinitionVersionsResponse _
derive instance repGenericListDeviceDefinitionVersionsResponse :: Generic ListDeviceDefinitionVersionsResponse _
instance showListDeviceDefinitionVersionsResponse :: Show ListDeviceDefinitionVersionsResponse where
  show = genericShow
instance decodeListDeviceDefinitionVersionsResponse :: Decode ListDeviceDefinitionVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDeviceDefinitionVersionsResponse :: Encode ListDeviceDefinitionVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDeviceDefinitionsRequest = ListDeviceDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListDeviceDefinitionsRequest :: Newtype ListDeviceDefinitionsRequest _
derive instance repGenericListDeviceDefinitionsRequest :: Generic ListDeviceDefinitionsRequest _
instance showListDeviceDefinitionsRequest :: Show ListDeviceDefinitionsRequest where
  show = genericShow
instance decodeListDeviceDefinitionsRequest :: Decode ListDeviceDefinitionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDeviceDefinitionsRequest :: Encode ListDeviceDefinitionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDeviceDefinitionsResponse = ListDeviceDefinitionsResponse 
  { "Definitions" :: NullOrUndefined.NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListDeviceDefinitionsResponse :: Newtype ListDeviceDefinitionsResponse _
derive instance repGenericListDeviceDefinitionsResponse :: Generic ListDeviceDefinitionsResponse _
instance showListDeviceDefinitionsResponse :: Show ListDeviceDefinitionsResponse where
  show = genericShow
instance decodeListDeviceDefinitionsResponse :: Decode ListDeviceDefinitionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDeviceDefinitionsResponse :: Encode ListDeviceDefinitionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListFunctionDefinitionVersionsRequest = ListFunctionDefinitionVersionsRequest 
  { "FunctionDefinitionId" :: (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListFunctionDefinitionVersionsRequest :: Newtype ListFunctionDefinitionVersionsRequest _
derive instance repGenericListFunctionDefinitionVersionsRequest :: Generic ListFunctionDefinitionVersionsRequest _
instance showListFunctionDefinitionVersionsRequest :: Show ListFunctionDefinitionVersionsRequest where
  show = genericShow
instance decodeListFunctionDefinitionVersionsRequest :: Decode ListFunctionDefinitionVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFunctionDefinitionVersionsRequest :: Encode ListFunctionDefinitionVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListFunctionDefinitionVersionsResponse = ListFunctionDefinitionVersionsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Versions" :: NullOrUndefined.NullOrUndefined (ListOfVersionInformation)
  }
derive instance newtypeListFunctionDefinitionVersionsResponse :: Newtype ListFunctionDefinitionVersionsResponse _
derive instance repGenericListFunctionDefinitionVersionsResponse :: Generic ListFunctionDefinitionVersionsResponse _
instance showListFunctionDefinitionVersionsResponse :: Show ListFunctionDefinitionVersionsResponse where
  show = genericShow
instance decodeListFunctionDefinitionVersionsResponse :: Decode ListFunctionDefinitionVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFunctionDefinitionVersionsResponse :: Encode ListFunctionDefinitionVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListFunctionDefinitionsRequest = ListFunctionDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListFunctionDefinitionsRequest :: Newtype ListFunctionDefinitionsRequest _
derive instance repGenericListFunctionDefinitionsRequest :: Generic ListFunctionDefinitionsRequest _
instance showListFunctionDefinitionsRequest :: Show ListFunctionDefinitionsRequest where
  show = genericShow
instance decodeListFunctionDefinitionsRequest :: Decode ListFunctionDefinitionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFunctionDefinitionsRequest :: Encode ListFunctionDefinitionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListFunctionDefinitionsResponse = ListFunctionDefinitionsResponse 
  { "Definitions" :: NullOrUndefined.NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListFunctionDefinitionsResponse :: Newtype ListFunctionDefinitionsResponse _
derive instance repGenericListFunctionDefinitionsResponse :: Generic ListFunctionDefinitionsResponse _
instance showListFunctionDefinitionsResponse :: Show ListFunctionDefinitionsResponse where
  show = genericShow
instance decodeListFunctionDefinitionsResponse :: Decode ListFunctionDefinitionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFunctionDefinitionsResponse :: Encode ListFunctionDefinitionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupCertificateAuthoritiesRequest = ListGroupCertificateAuthoritiesRequest 
  { "GroupId" :: (String)
  }
derive instance newtypeListGroupCertificateAuthoritiesRequest :: Newtype ListGroupCertificateAuthoritiesRequest _
derive instance repGenericListGroupCertificateAuthoritiesRequest :: Generic ListGroupCertificateAuthoritiesRequest _
instance showListGroupCertificateAuthoritiesRequest :: Show ListGroupCertificateAuthoritiesRequest where
  show = genericShow
instance decodeListGroupCertificateAuthoritiesRequest :: Decode ListGroupCertificateAuthoritiesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupCertificateAuthoritiesRequest :: Encode ListGroupCertificateAuthoritiesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupCertificateAuthoritiesResponse = ListGroupCertificateAuthoritiesResponse 
  { "GroupCertificateAuthorities" :: NullOrUndefined.NullOrUndefined (ListOfGroupCertificateAuthorityProperties)
  }
derive instance newtypeListGroupCertificateAuthoritiesResponse :: Newtype ListGroupCertificateAuthoritiesResponse _
derive instance repGenericListGroupCertificateAuthoritiesResponse :: Generic ListGroupCertificateAuthoritiesResponse _
instance showListGroupCertificateAuthoritiesResponse :: Show ListGroupCertificateAuthoritiesResponse where
  show = genericShow
instance decodeListGroupCertificateAuthoritiesResponse :: Decode ListGroupCertificateAuthoritiesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupCertificateAuthoritiesResponse :: Encode ListGroupCertificateAuthoritiesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupVersionsRequest = ListGroupVersionsRequest 
  { "GroupId" :: (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListGroupVersionsRequest :: Newtype ListGroupVersionsRequest _
derive instance repGenericListGroupVersionsRequest :: Generic ListGroupVersionsRequest _
instance showListGroupVersionsRequest :: Show ListGroupVersionsRequest where
  show = genericShow
instance decodeListGroupVersionsRequest :: Decode ListGroupVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupVersionsRequest :: Encode ListGroupVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupVersionsResponse = ListGroupVersionsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Versions" :: NullOrUndefined.NullOrUndefined (ListOfVersionInformation)
  }
derive instance newtypeListGroupVersionsResponse :: Newtype ListGroupVersionsResponse _
derive instance repGenericListGroupVersionsResponse :: Generic ListGroupVersionsResponse _
instance showListGroupVersionsResponse :: Show ListGroupVersionsResponse where
  show = genericShow
instance decodeListGroupVersionsResponse :: Decode ListGroupVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupVersionsResponse :: Encode ListGroupVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupsRequest = ListGroupsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListGroupsRequest :: Newtype ListGroupsRequest _
derive instance repGenericListGroupsRequest :: Generic ListGroupsRequest _
instance showListGroupsRequest :: Show ListGroupsRequest where
  show = genericShow
instance decodeListGroupsRequest :: Decode ListGroupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupsRequest :: Encode ListGroupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupsResponse = ListGroupsResponse 
  { "Groups" :: NullOrUndefined.NullOrUndefined (ListOfGroupInformation)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListGroupsResponse :: Newtype ListGroupsResponse _
derive instance repGenericListGroupsResponse :: Generic ListGroupsResponse _
instance showListGroupsResponse :: Show ListGroupsResponse where
  show = genericShow
instance decodeListGroupsResponse :: Decode ListGroupsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupsResponse :: Encode ListGroupsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListLoggerDefinitionVersionsRequest = ListLoggerDefinitionVersionsRequest 
  { "LoggerDefinitionId" :: (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListLoggerDefinitionVersionsRequest :: Newtype ListLoggerDefinitionVersionsRequest _
derive instance repGenericListLoggerDefinitionVersionsRequest :: Generic ListLoggerDefinitionVersionsRequest _
instance showListLoggerDefinitionVersionsRequest :: Show ListLoggerDefinitionVersionsRequest where
  show = genericShow
instance decodeListLoggerDefinitionVersionsRequest :: Decode ListLoggerDefinitionVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListLoggerDefinitionVersionsRequest :: Encode ListLoggerDefinitionVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListLoggerDefinitionVersionsResponse = ListLoggerDefinitionVersionsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Versions" :: NullOrUndefined.NullOrUndefined (ListOfVersionInformation)
  }
derive instance newtypeListLoggerDefinitionVersionsResponse :: Newtype ListLoggerDefinitionVersionsResponse _
derive instance repGenericListLoggerDefinitionVersionsResponse :: Generic ListLoggerDefinitionVersionsResponse _
instance showListLoggerDefinitionVersionsResponse :: Show ListLoggerDefinitionVersionsResponse where
  show = genericShow
instance decodeListLoggerDefinitionVersionsResponse :: Decode ListLoggerDefinitionVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListLoggerDefinitionVersionsResponse :: Encode ListLoggerDefinitionVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListLoggerDefinitionsRequest = ListLoggerDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListLoggerDefinitionsRequest :: Newtype ListLoggerDefinitionsRequest _
derive instance repGenericListLoggerDefinitionsRequest :: Generic ListLoggerDefinitionsRequest _
instance showListLoggerDefinitionsRequest :: Show ListLoggerDefinitionsRequest where
  show = genericShow
instance decodeListLoggerDefinitionsRequest :: Decode ListLoggerDefinitionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListLoggerDefinitionsRequest :: Encode ListLoggerDefinitionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListLoggerDefinitionsResponse = ListLoggerDefinitionsResponse 
  { "Definitions" :: NullOrUndefined.NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListLoggerDefinitionsResponse :: Newtype ListLoggerDefinitionsResponse _
derive instance repGenericListLoggerDefinitionsResponse :: Generic ListLoggerDefinitionsResponse _
instance showListLoggerDefinitionsResponse :: Show ListLoggerDefinitionsResponse where
  show = genericShow
instance decodeListLoggerDefinitionsResponse :: Decode ListLoggerDefinitionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListLoggerDefinitionsResponse :: Encode ListLoggerDefinitionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfConnectivityInfo = ListOfConnectivityInfo (Array ConnectivityInfo)
derive instance newtypeListOfConnectivityInfo :: Newtype ListOfConnectivityInfo _
derive instance repGenericListOfConnectivityInfo :: Generic ListOfConnectivityInfo _
instance showListOfConnectivityInfo :: Show ListOfConnectivityInfo where
  show = genericShow
instance decodeListOfConnectivityInfo :: Decode ListOfConnectivityInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfConnectivityInfo :: Encode ListOfConnectivityInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfCore = ListOfCore (Array Core)
derive instance newtypeListOfCore :: Newtype ListOfCore _
derive instance repGenericListOfCore :: Generic ListOfCore _
instance showListOfCore :: Show ListOfCore where
  show = genericShow
instance decodeListOfCore :: Decode ListOfCore where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfCore :: Encode ListOfCore where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfDefinitionInformation = ListOfDefinitionInformation (Array DefinitionInformation)
derive instance newtypeListOfDefinitionInformation :: Newtype ListOfDefinitionInformation _
derive instance repGenericListOfDefinitionInformation :: Generic ListOfDefinitionInformation _
instance showListOfDefinitionInformation :: Show ListOfDefinitionInformation where
  show = genericShow
instance decodeListOfDefinitionInformation :: Decode ListOfDefinitionInformation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfDefinitionInformation :: Encode ListOfDefinitionInformation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfDevice = ListOfDevice (Array Device)
derive instance newtypeListOfDevice :: Newtype ListOfDevice _
derive instance repGenericListOfDevice :: Generic ListOfDevice _
instance showListOfDevice :: Show ListOfDevice where
  show = genericShow
instance decodeListOfDevice :: Decode ListOfDevice where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfDevice :: Encode ListOfDevice where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfFunction = ListOfFunction (Array Function'')
derive instance newtypeListOfFunction :: Newtype ListOfFunction _
derive instance repGenericListOfFunction :: Generic ListOfFunction _
instance showListOfFunction :: Show ListOfFunction where
  show = genericShow
instance decodeListOfFunction :: Decode ListOfFunction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfFunction :: Encode ListOfFunction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfGroupCertificateAuthorityProperties = ListOfGroupCertificateAuthorityProperties (Array GroupCertificateAuthorityProperties)
derive instance newtypeListOfGroupCertificateAuthorityProperties :: Newtype ListOfGroupCertificateAuthorityProperties _
derive instance repGenericListOfGroupCertificateAuthorityProperties :: Generic ListOfGroupCertificateAuthorityProperties _
instance showListOfGroupCertificateAuthorityProperties :: Show ListOfGroupCertificateAuthorityProperties where
  show = genericShow
instance decodeListOfGroupCertificateAuthorityProperties :: Decode ListOfGroupCertificateAuthorityProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfGroupCertificateAuthorityProperties :: Encode ListOfGroupCertificateAuthorityProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfGroupInformation = ListOfGroupInformation (Array GroupInformation)
derive instance newtypeListOfGroupInformation :: Newtype ListOfGroupInformation _
derive instance repGenericListOfGroupInformation :: Generic ListOfGroupInformation _
instance showListOfGroupInformation :: Show ListOfGroupInformation where
  show = genericShow
instance decodeListOfGroupInformation :: Decode ListOfGroupInformation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfGroupInformation :: Encode ListOfGroupInformation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfLogger = ListOfLogger (Array Logger)
derive instance newtypeListOfLogger :: Newtype ListOfLogger _
derive instance repGenericListOfLogger :: Generic ListOfLogger _
instance showListOfLogger :: Show ListOfLogger where
  show = genericShow
instance decodeListOfLogger :: Decode ListOfLogger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfLogger :: Encode ListOfLogger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfResource = ListOfResource (Array Resource)
derive instance newtypeListOfResource :: Newtype ListOfResource _
derive instance repGenericListOfResource :: Generic ListOfResource _
instance showListOfResource :: Show ListOfResource where
  show = genericShow
instance decodeListOfResource :: Decode ListOfResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfResource :: Encode ListOfResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfResourceAccessPolicy = ListOfResourceAccessPolicy (Array ResourceAccessPolicy)
derive instance newtypeListOfResourceAccessPolicy :: Newtype ListOfResourceAccessPolicy _
derive instance repGenericListOfResourceAccessPolicy :: Generic ListOfResourceAccessPolicy _
instance showListOfResourceAccessPolicy :: Show ListOfResourceAccessPolicy where
  show = genericShow
instance decodeListOfResourceAccessPolicy :: Decode ListOfResourceAccessPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfResourceAccessPolicy :: Encode ListOfResourceAccessPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfSubscription = ListOfSubscription (Array Subscription)
derive instance newtypeListOfSubscription :: Newtype ListOfSubscription _
derive instance repGenericListOfSubscription :: Generic ListOfSubscription _
instance showListOfSubscription :: Show ListOfSubscription where
  show = genericShow
instance decodeListOfSubscription :: Decode ListOfSubscription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfSubscription :: Encode ListOfSubscription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfVersionInformation = ListOfVersionInformation (Array VersionInformation)
derive instance newtypeListOfVersionInformation :: Newtype ListOfVersionInformation _
derive instance repGenericListOfVersionInformation :: Generic ListOfVersionInformation _
instance showListOfVersionInformation :: Show ListOfVersionInformation where
  show = genericShow
instance decodeListOfVersionInformation :: Decode ListOfVersionInformation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfVersionInformation :: Encode ListOfVersionInformation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceDefinitionVersionsRequest = ListResourceDefinitionVersionsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "ResourceDefinitionId" :: (String)
  }
derive instance newtypeListResourceDefinitionVersionsRequest :: Newtype ListResourceDefinitionVersionsRequest _
derive instance repGenericListResourceDefinitionVersionsRequest :: Generic ListResourceDefinitionVersionsRequest _
instance showListResourceDefinitionVersionsRequest :: Show ListResourceDefinitionVersionsRequest where
  show = genericShow
instance decodeListResourceDefinitionVersionsRequest :: Decode ListResourceDefinitionVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceDefinitionVersionsRequest :: Encode ListResourceDefinitionVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceDefinitionVersionsResponse = ListResourceDefinitionVersionsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Versions" :: NullOrUndefined.NullOrUndefined (ListOfVersionInformation)
  }
derive instance newtypeListResourceDefinitionVersionsResponse :: Newtype ListResourceDefinitionVersionsResponse _
derive instance repGenericListResourceDefinitionVersionsResponse :: Generic ListResourceDefinitionVersionsResponse _
instance showListResourceDefinitionVersionsResponse :: Show ListResourceDefinitionVersionsResponse where
  show = genericShow
instance decodeListResourceDefinitionVersionsResponse :: Decode ListResourceDefinitionVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceDefinitionVersionsResponse :: Encode ListResourceDefinitionVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceDefinitionsRequest = ListResourceDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListResourceDefinitionsRequest :: Newtype ListResourceDefinitionsRequest _
derive instance repGenericListResourceDefinitionsRequest :: Generic ListResourceDefinitionsRequest _
instance showListResourceDefinitionsRequest :: Show ListResourceDefinitionsRequest where
  show = genericShow
instance decodeListResourceDefinitionsRequest :: Decode ListResourceDefinitionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceDefinitionsRequest :: Encode ListResourceDefinitionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceDefinitionsResponse = ListResourceDefinitionsResponse 
  { "Definitions" :: NullOrUndefined.NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListResourceDefinitionsResponse :: Newtype ListResourceDefinitionsResponse _
derive instance repGenericListResourceDefinitionsResponse :: Generic ListResourceDefinitionsResponse _
instance showListResourceDefinitionsResponse :: Show ListResourceDefinitionsResponse where
  show = genericShow
instance decodeListResourceDefinitionsResponse :: Decode ListResourceDefinitionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceDefinitionsResponse :: Encode ListResourceDefinitionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListSubscriptionDefinitionVersionsRequest = ListSubscriptionDefinitionVersionsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "SubscriptionDefinitionId" :: (String)
  }
derive instance newtypeListSubscriptionDefinitionVersionsRequest :: Newtype ListSubscriptionDefinitionVersionsRequest _
derive instance repGenericListSubscriptionDefinitionVersionsRequest :: Generic ListSubscriptionDefinitionVersionsRequest _
instance showListSubscriptionDefinitionVersionsRequest :: Show ListSubscriptionDefinitionVersionsRequest where
  show = genericShow
instance decodeListSubscriptionDefinitionVersionsRequest :: Decode ListSubscriptionDefinitionVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSubscriptionDefinitionVersionsRequest :: Encode ListSubscriptionDefinitionVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListSubscriptionDefinitionVersionsResponse = ListSubscriptionDefinitionVersionsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Versions" :: NullOrUndefined.NullOrUndefined (ListOfVersionInformation)
  }
derive instance newtypeListSubscriptionDefinitionVersionsResponse :: Newtype ListSubscriptionDefinitionVersionsResponse _
derive instance repGenericListSubscriptionDefinitionVersionsResponse :: Generic ListSubscriptionDefinitionVersionsResponse _
instance showListSubscriptionDefinitionVersionsResponse :: Show ListSubscriptionDefinitionVersionsResponse where
  show = genericShow
instance decodeListSubscriptionDefinitionVersionsResponse :: Decode ListSubscriptionDefinitionVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSubscriptionDefinitionVersionsResponse :: Encode ListSubscriptionDefinitionVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListSubscriptionDefinitionsRequest = ListSubscriptionDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListSubscriptionDefinitionsRequest :: Newtype ListSubscriptionDefinitionsRequest _
derive instance repGenericListSubscriptionDefinitionsRequest :: Generic ListSubscriptionDefinitionsRequest _
instance showListSubscriptionDefinitionsRequest :: Show ListSubscriptionDefinitionsRequest where
  show = genericShow
instance decodeListSubscriptionDefinitionsRequest :: Decode ListSubscriptionDefinitionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSubscriptionDefinitionsRequest :: Encode ListSubscriptionDefinitionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListSubscriptionDefinitionsResponse = ListSubscriptionDefinitionsResponse 
  { "Definitions" :: NullOrUndefined.NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListSubscriptionDefinitionsResponse :: Newtype ListSubscriptionDefinitionsResponse _
derive instance repGenericListSubscriptionDefinitionsResponse :: Generic ListSubscriptionDefinitionsResponse _
instance showListSubscriptionDefinitionsResponse :: Show ListSubscriptionDefinitionsResponse where
  show = genericShow
instance decodeListSubscriptionDefinitionsResponse :: Decode ListSubscriptionDefinitionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSubscriptionDefinitionsResponse :: Encode ListSubscriptionDefinitionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | List of versions response
newtype ListVersionsResponse = ListVersionsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Versions" :: NullOrUndefined.NullOrUndefined (ListOfVersionInformation)
  }
derive instance newtypeListVersionsResponse :: Newtype ListVersionsResponse _
derive instance repGenericListVersionsResponse :: Generic ListVersionsResponse _
instance showListVersionsResponse :: Show ListVersionsResponse where
  show = genericShow
instance decodeListVersionsResponse :: Decode ListVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListVersionsResponse :: Encode ListVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Attributes that define the Local Device Resource.
newtype LocalDeviceResourceData = LocalDeviceResourceData 
  { "GroupOwnerSetting" :: NullOrUndefined.NullOrUndefined (GroupOwnerSetting)
  , "SourcePath" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLocalDeviceResourceData :: Newtype LocalDeviceResourceData _
derive instance repGenericLocalDeviceResourceData :: Generic LocalDeviceResourceData _
instance showLocalDeviceResourceData :: Show LocalDeviceResourceData where
  show = genericShow
instance decodeLocalDeviceResourceData :: Decode LocalDeviceResourceData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocalDeviceResourceData :: Encode LocalDeviceResourceData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Attributes that define the Local Volume Resource.
newtype LocalVolumeResourceData = LocalVolumeResourceData 
  { "DestinationPath" :: NullOrUndefined.NullOrUndefined (String)
  , "GroupOwnerSetting" :: NullOrUndefined.NullOrUndefined (GroupOwnerSetting)
  , "SourcePath" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLocalVolumeResourceData :: Newtype LocalVolumeResourceData _
derive instance repGenericLocalVolumeResourceData :: Generic LocalVolumeResourceData _
instance showLocalVolumeResourceData :: Show LocalVolumeResourceData where
  show = genericShow
instance decodeLocalVolumeResourceData :: Decode LocalVolumeResourceData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocalVolumeResourceData :: Encode LocalVolumeResourceData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on the Logger
newtype Logger = Logger 
  { "Component" :: NullOrUndefined.NullOrUndefined (LoggerComponent)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Level" :: NullOrUndefined.NullOrUndefined (LoggerLevel)
  , "Space" :: NullOrUndefined.NullOrUndefined (Int)
  , "Type" :: NullOrUndefined.NullOrUndefined (LoggerType)
  }
derive instance newtypeLogger :: Newtype Logger _
derive instance repGenericLogger :: Generic Logger _
instance showLogger :: Show Logger where
  show = genericShow
instance decodeLogger :: Decode Logger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogger :: Encode Logger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoggerComponent = LoggerComponent String
derive instance newtypeLoggerComponent :: Newtype LoggerComponent _
derive instance repGenericLoggerComponent :: Generic LoggerComponent _
instance showLoggerComponent :: Show LoggerComponent where
  show = genericShow
instance decodeLoggerComponent :: Decode LoggerComponent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoggerComponent :: Encode LoggerComponent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on logger definition version
newtype LoggerDefinitionVersion = LoggerDefinitionVersion 
  { "Loggers" :: NullOrUndefined.NullOrUndefined (ListOfLogger)
  }
derive instance newtypeLoggerDefinitionVersion :: Newtype LoggerDefinitionVersion _
derive instance repGenericLoggerDefinitionVersion :: Generic LoggerDefinitionVersion _
instance showLoggerDefinitionVersion :: Show LoggerDefinitionVersion where
  show = genericShow
instance decodeLoggerDefinitionVersion :: Decode LoggerDefinitionVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoggerDefinitionVersion :: Encode LoggerDefinitionVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoggerLevel = LoggerLevel String
derive instance newtypeLoggerLevel :: Newtype LoggerLevel _
derive instance repGenericLoggerLevel :: Generic LoggerLevel _
instance showLoggerLevel :: Show LoggerLevel where
  show = genericShow
instance decodeLoggerLevel :: Decode LoggerLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoggerLevel :: Encode LoggerLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoggerType = LoggerType String
derive instance newtypeLoggerType :: Newtype LoggerType _
derive instance repGenericLoggerType :: Generic LoggerType _
instance showLoggerType :: Show LoggerType where
  show = genericShow
instance decodeLoggerType :: Decode LoggerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoggerType :: Encode LoggerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOf__string = MapOf__string (StrMap.StrMap String)
derive instance newtypeMapOf__string :: Newtype MapOf__string _
derive instance repGenericMapOf__string :: Generic MapOf__string _
instance showMapOf__string :: Show MapOf__string where
  show = genericShow
instance decodeMapOf__string :: Decode MapOf__string where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOf__string :: Encode MapOf__string where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Type of permissions a function could have to access a resource.
newtype Permission = Permission String
derive instance newtypePermission :: Newtype Permission _
derive instance repGenericPermission :: Generic Permission _
instance showPermission :: Show Permission where
  show = genericShow
instance decodePermission :: Decode Permission where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePermission :: Encode Permission where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information needed to perform a reset of a group's deployments.
newtype ResetDeploymentsRequest = ResetDeploymentsRequest 
  { "AmznClientToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Force" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "GroupId" :: (String)
  }
derive instance newtypeResetDeploymentsRequest :: Newtype ResetDeploymentsRequest _
derive instance repGenericResetDeploymentsRequest :: Generic ResetDeploymentsRequest _
instance showResetDeploymentsRequest :: Show ResetDeploymentsRequest where
  show = genericShow
instance decodeResetDeploymentsRequest :: Decode ResetDeploymentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResetDeploymentsRequest :: Encode ResetDeploymentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResetDeploymentsResponse = ResetDeploymentsResponse 
  { "DeploymentArn" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResetDeploymentsResponse :: Newtype ResetDeploymentsResponse _
derive instance repGenericResetDeploymentsResponse :: Generic ResetDeploymentsResponse _
instance showResetDeploymentsResponse :: Show ResetDeploymentsResponse where
  show = genericShow
instance decodeResetDeploymentsResponse :: Decode ResetDeploymentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResetDeploymentsResponse :: Encode ResetDeploymentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on the resource.
newtype Resource = Resource 
  { "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "ResourceDataContainer" :: NullOrUndefined.NullOrUndefined (ResourceDataContainer)
  }
derive instance newtypeResource :: Newtype Resource _
derive instance repGenericResource :: Generic Resource _
instance showResource :: Show Resource where
  show = genericShow
instance decodeResource :: Decode Resource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResource :: Encode Resource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Policy for the function to access a resource.
newtype ResourceAccessPolicy = ResourceAccessPolicy 
  { "Permission" :: NullOrUndefined.NullOrUndefined (Permission)
  , "ResourceId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceAccessPolicy :: Newtype ResourceAccessPolicy _
derive instance repGenericResourceAccessPolicy :: Generic ResourceAccessPolicy _
instance showResourceAccessPolicy :: Show ResourceAccessPolicy where
  show = genericShow
instance decodeResourceAccessPolicy :: Decode ResourceAccessPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceAccessPolicy :: Encode ResourceAccessPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A container of data for all resource types.
newtype ResourceDataContainer = ResourceDataContainer 
  { "LocalDeviceResourceData" :: NullOrUndefined.NullOrUndefined (LocalDeviceResourceData)
  , "LocalVolumeResourceData" :: NullOrUndefined.NullOrUndefined (LocalVolumeResourceData)
  }
derive instance newtypeResourceDataContainer :: Newtype ResourceDataContainer _
derive instance repGenericResourceDataContainer :: Generic ResourceDataContainer _
instance showResourceDataContainer :: Show ResourceDataContainer where
  show = genericShow
instance decodeResourceDataContainer :: Decode ResourceDataContainer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataContainer :: Encode ResourceDataContainer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on resource definition version
newtype ResourceDefinitionVersion = ResourceDefinitionVersion 
  { "Resources" :: NullOrUndefined.NullOrUndefined (ListOfResource)
  }
derive instance newtypeResourceDefinitionVersion :: Newtype ResourceDefinitionVersion _
derive instance repGenericResourceDefinitionVersion :: Generic ResourceDefinitionVersion _
instance showResourceDefinitionVersion :: Show ResourceDefinitionVersion where
  show = genericShow
instance decodeResourceDefinitionVersion :: Decode ResourceDefinitionVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDefinitionVersion :: Encode ResourceDefinitionVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The IAM Role that Greengrass will use to create pre-signed URLs pointing towards the update artifact.
newtype S3UrlSignerRole = S3UrlSignerRole String
derive instance newtypeS3UrlSignerRole :: Newtype S3UrlSignerRole _
derive instance repGenericS3UrlSignerRole :: Generic S3UrlSignerRole _
instance showS3UrlSignerRole :: Show S3UrlSignerRole where
  show = genericShow
instance decodeS3UrlSignerRole :: Decode S3UrlSignerRole where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3UrlSignerRole :: Encode S3UrlSignerRole where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The piece of software on the Greengrass Core that will be updated.
newtype SoftwareToUpdate = SoftwareToUpdate String
derive instance newtypeSoftwareToUpdate :: Newtype SoftwareToUpdate _
derive instance repGenericSoftwareToUpdate :: Generic SoftwareToUpdate _
instance showSoftwareToUpdate :: Show SoftwareToUpdate where
  show = genericShow
instance decodeSoftwareToUpdate :: Decode SoftwareToUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSoftwareToUpdate :: Encode SoftwareToUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on subscription
newtype Subscription = Subscription 
  { "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Source" :: NullOrUndefined.NullOrUndefined (String)
  , "Subject" :: NullOrUndefined.NullOrUndefined (String)
  , "Target" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSubscription :: Newtype Subscription _
derive instance repGenericSubscription :: Generic Subscription _
instance showSubscription :: Show Subscription where
  show = genericShow
instance decodeSubscription :: Decode Subscription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscription :: Encode Subscription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on subscription definition version
newtype SubscriptionDefinitionVersion = SubscriptionDefinitionVersion 
  { "Subscriptions" :: NullOrUndefined.NullOrUndefined (ListOfSubscription)
  }
derive instance newtypeSubscriptionDefinitionVersion :: Newtype SubscriptionDefinitionVersion _
derive instance repGenericSubscriptionDefinitionVersion :: Generic SubscriptionDefinitionVersion _
instance showSubscriptionDefinitionVersion :: Show SubscriptionDefinitionVersion where
  show = genericShow
instance decodeSubscriptionDefinitionVersion :: Decode SubscriptionDefinitionVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionDefinitionVersion :: Encode SubscriptionDefinitionVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The minimum level of log statements that should be logged by the OTA Agent during an update.
newtype UpdateAgentLogLevel = UpdateAgentLogLevel String
derive instance newtypeUpdateAgentLogLevel :: Newtype UpdateAgentLogLevel _
derive instance repGenericUpdateAgentLogLevel :: Generic UpdateAgentLogLevel _
instance showUpdateAgentLogLevel :: Show UpdateAgentLogLevel where
  show = genericShow
instance decodeUpdateAgentLogLevel :: Decode UpdateAgentLogLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAgentLogLevel :: Encode UpdateAgentLogLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | connectivity info request
newtype UpdateConnectivityInfoRequest = UpdateConnectivityInfoRequest 
  { "ConnectivityInfo" :: NullOrUndefined.NullOrUndefined (ListOfConnectivityInfo)
  , "ThingName" :: (String)
  }
derive instance newtypeUpdateConnectivityInfoRequest :: Newtype UpdateConnectivityInfoRequest _
derive instance repGenericUpdateConnectivityInfoRequest :: Generic UpdateConnectivityInfoRequest _
instance showUpdateConnectivityInfoRequest :: Show UpdateConnectivityInfoRequest where
  show = genericShow
instance decodeUpdateConnectivityInfoRequest :: Decode UpdateConnectivityInfoRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConnectivityInfoRequest :: Encode UpdateConnectivityInfoRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateConnectivityInfoResponse = UpdateConnectivityInfoResponse 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateConnectivityInfoResponse :: Newtype UpdateConnectivityInfoResponse _
derive instance repGenericUpdateConnectivityInfoResponse :: Generic UpdateConnectivityInfoResponse _
instance showUpdateConnectivityInfoResponse :: Show UpdateConnectivityInfoResponse where
  show = genericShow
instance decodeUpdateConnectivityInfoResponse :: Decode UpdateConnectivityInfoResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConnectivityInfoResponse :: Encode UpdateConnectivityInfoResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateCoreDefinitionRequest = UpdateCoreDefinitionRequest 
  { "CoreDefinitionId" :: (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateCoreDefinitionRequest :: Newtype UpdateCoreDefinitionRequest _
derive instance repGenericUpdateCoreDefinitionRequest :: Generic UpdateCoreDefinitionRequest _
instance showUpdateCoreDefinitionRequest :: Show UpdateCoreDefinitionRequest where
  show = genericShow
instance decodeUpdateCoreDefinitionRequest :: Decode UpdateCoreDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCoreDefinitionRequest :: Encode UpdateCoreDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateCoreDefinitionResponse = UpdateCoreDefinitionResponse Types.NoArguments
derive instance newtypeUpdateCoreDefinitionResponse :: Newtype UpdateCoreDefinitionResponse _
derive instance repGenericUpdateCoreDefinitionResponse :: Generic UpdateCoreDefinitionResponse _
instance showUpdateCoreDefinitionResponse :: Show UpdateCoreDefinitionResponse where
  show = genericShow
instance decodeUpdateCoreDefinitionResponse :: Decode UpdateCoreDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCoreDefinitionResponse :: Encode UpdateCoreDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDeviceDefinitionRequest = UpdateDeviceDefinitionRequest 
  { "DeviceDefinitionId" :: (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateDeviceDefinitionRequest :: Newtype UpdateDeviceDefinitionRequest _
derive instance repGenericUpdateDeviceDefinitionRequest :: Generic UpdateDeviceDefinitionRequest _
instance showUpdateDeviceDefinitionRequest :: Show UpdateDeviceDefinitionRequest where
  show = genericShow
instance decodeUpdateDeviceDefinitionRequest :: Decode UpdateDeviceDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDeviceDefinitionRequest :: Encode UpdateDeviceDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDeviceDefinitionResponse = UpdateDeviceDefinitionResponse Types.NoArguments
derive instance newtypeUpdateDeviceDefinitionResponse :: Newtype UpdateDeviceDefinitionResponse _
derive instance repGenericUpdateDeviceDefinitionResponse :: Generic UpdateDeviceDefinitionResponse _
instance showUpdateDeviceDefinitionResponse :: Show UpdateDeviceDefinitionResponse where
  show = genericShow
instance decodeUpdateDeviceDefinitionResponse :: Decode UpdateDeviceDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDeviceDefinitionResponse :: Encode UpdateDeviceDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateFunctionDefinitionRequest = UpdateFunctionDefinitionRequest 
  { "FunctionDefinitionId" :: (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateFunctionDefinitionRequest :: Newtype UpdateFunctionDefinitionRequest _
derive instance repGenericUpdateFunctionDefinitionRequest :: Generic UpdateFunctionDefinitionRequest _
instance showUpdateFunctionDefinitionRequest :: Show UpdateFunctionDefinitionRequest where
  show = genericShow
instance decodeUpdateFunctionDefinitionRequest :: Decode UpdateFunctionDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFunctionDefinitionRequest :: Encode UpdateFunctionDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateFunctionDefinitionResponse = UpdateFunctionDefinitionResponse Types.NoArguments
derive instance newtypeUpdateFunctionDefinitionResponse :: Newtype UpdateFunctionDefinitionResponse _
derive instance repGenericUpdateFunctionDefinitionResponse :: Generic UpdateFunctionDefinitionResponse _
instance showUpdateFunctionDefinitionResponse :: Show UpdateFunctionDefinitionResponse where
  show = genericShow
instance decodeUpdateFunctionDefinitionResponse :: Decode UpdateFunctionDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFunctionDefinitionResponse :: Encode UpdateFunctionDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGroupCertificateConfigurationRequest = UpdateGroupCertificateConfigurationRequest 
  { "CertificateExpiryInMilliseconds" :: NullOrUndefined.NullOrUndefined (String)
  , "GroupId" :: (String)
  }
derive instance newtypeUpdateGroupCertificateConfigurationRequest :: Newtype UpdateGroupCertificateConfigurationRequest _
derive instance repGenericUpdateGroupCertificateConfigurationRequest :: Generic UpdateGroupCertificateConfigurationRequest _
instance showUpdateGroupCertificateConfigurationRequest :: Show UpdateGroupCertificateConfigurationRequest where
  show = genericShow
instance decodeUpdateGroupCertificateConfigurationRequest :: Decode UpdateGroupCertificateConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGroupCertificateConfigurationRequest :: Encode UpdateGroupCertificateConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGroupCertificateConfigurationResponse = UpdateGroupCertificateConfigurationResponse 
  { "CertificateAuthorityExpiryInMilliseconds" :: NullOrUndefined.NullOrUndefined (String)
  , "CertificateExpiryInMilliseconds" :: NullOrUndefined.NullOrUndefined (String)
  , "GroupId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateGroupCertificateConfigurationResponse :: Newtype UpdateGroupCertificateConfigurationResponse _
derive instance repGenericUpdateGroupCertificateConfigurationResponse :: Generic UpdateGroupCertificateConfigurationResponse _
instance showUpdateGroupCertificateConfigurationResponse :: Show UpdateGroupCertificateConfigurationResponse where
  show = genericShow
instance decodeUpdateGroupCertificateConfigurationResponse :: Decode UpdateGroupCertificateConfigurationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGroupCertificateConfigurationResponse :: Encode UpdateGroupCertificateConfigurationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGroupRequest = UpdateGroupRequest 
  { "GroupId" :: (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateGroupRequest :: Newtype UpdateGroupRequest _
derive instance repGenericUpdateGroupRequest :: Generic UpdateGroupRequest _
instance showUpdateGroupRequest :: Show UpdateGroupRequest where
  show = genericShow
instance decodeUpdateGroupRequest :: Decode UpdateGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGroupRequest :: Encode UpdateGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGroupResponse = UpdateGroupResponse Types.NoArguments
derive instance newtypeUpdateGroupResponse :: Newtype UpdateGroupResponse _
derive instance repGenericUpdateGroupResponse :: Generic UpdateGroupResponse _
instance showUpdateGroupResponse :: Show UpdateGroupResponse where
  show = genericShow
instance decodeUpdateGroupResponse :: Decode UpdateGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGroupResponse :: Encode UpdateGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateLoggerDefinitionRequest = UpdateLoggerDefinitionRequest 
  { "LoggerDefinitionId" :: (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateLoggerDefinitionRequest :: Newtype UpdateLoggerDefinitionRequest _
derive instance repGenericUpdateLoggerDefinitionRequest :: Generic UpdateLoggerDefinitionRequest _
instance showUpdateLoggerDefinitionRequest :: Show UpdateLoggerDefinitionRequest where
  show = genericShow
instance decodeUpdateLoggerDefinitionRequest :: Decode UpdateLoggerDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateLoggerDefinitionRequest :: Encode UpdateLoggerDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateLoggerDefinitionResponse = UpdateLoggerDefinitionResponse Types.NoArguments
derive instance newtypeUpdateLoggerDefinitionResponse :: Newtype UpdateLoggerDefinitionResponse _
derive instance repGenericUpdateLoggerDefinitionResponse :: Generic UpdateLoggerDefinitionResponse _
instance showUpdateLoggerDefinitionResponse :: Show UpdateLoggerDefinitionResponse where
  show = genericShow
instance decodeUpdateLoggerDefinitionResponse :: Decode UpdateLoggerDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateLoggerDefinitionResponse :: Encode UpdateLoggerDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateResourceDefinitionRequest = UpdateResourceDefinitionRequest 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "ResourceDefinitionId" :: (String)
  }
derive instance newtypeUpdateResourceDefinitionRequest :: Newtype UpdateResourceDefinitionRequest _
derive instance repGenericUpdateResourceDefinitionRequest :: Generic UpdateResourceDefinitionRequest _
instance showUpdateResourceDefinitionRequest :: Show UpdateResourceDefinitionRequest where
  show = genericShow
instance decodeUpdateResourceDefinitionRequest :: Decode UpdateResourceDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateResourceDefinitionRequest :: Encode UpdateResourceDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateResourceDefinitionResponse = UpdateResourceDefinitionResponse Types.NoArguments
derive instance newtypeUpdateResourceDefinitionResponse :: Newtype UpdateResourceDefinitionResponse _
derive instance repGenericUpdateResourceDefinitionResponse :: Generic UpdateResourceDefinitionResponse _
instance showUpdateResourceDefinitionResponse :: Show UpdateResourceDefinitionResponse where
  show = genericShow
instance decodeUpdateResourceDefinitionResponse :: Decode UpdateResourceDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateResourceDefinitionResponse :: Encode UpdateResourceDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateSubscriptionDefinitionRequest = UpdateSubscriptionDefinitionRequest 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "SubscriptionDefinitionId" :: (String)
  }
derive instance newtypeUpdateSubscriptionDefinitionRequest :: Newtype UpdateSubscriptionDefinitionRequest _
derive instance repGenericUpdateSubscriptionDefinitionRequest :: Generic UpdateSubscriptionDefinitionRequest _
instance showUpdateSubscriptionDefinitionRequest :: Show UpdateSubscriptionDefinitionRequest where
  show = genericShow
instance decodeUpdateSubscriptionDefinitionRequest :: Decode UpdateSubscriptionDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSubscriptionDefinitionRequest :: Encode UpdateSubscriptionDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateSubscriptionDefinitionResponse = UpdateSubscriptionDefinitionResponse Types.NoArguments
derive instance newtypeUpdateSubscriptionDefinitionResponse :: Newtype UpdateSubscriptionDefinitionResponse _
derive instance repGenericUpdateSubscriptionDefinitionResponse :: Generic UpdateSubscriptionDefinitionResponse _
instance showUpdateSubscriptionDefinitionResponse :: Show UpdateSubscriptionDefinitionResponse where
  show = genericShow
instance decodeUpdateSubscriptionDefinitionResponse :: Decode UpdateSubscriptionDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSubscriptionDefinitionResponse :: Encode UpdateSubscriptionDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The target arns that this update will be applied to.
newtype UpdateTargets = UpdateTargets (Array String)
derive instance newtypeUpdateTargets :: Newtype UpdateTargets _
derive instance repGenericUpdateTargets :: Generic UpdateTargets _
instance showUpdateTargets :: Show UpdateTargets where
  show = genericShow
instance decodeUpdateTargets :: Decode UpdateTargets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTargets :: Encode UpdateTargets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The architecture of the Cores in the targets of an update
newtype UpdateTargetsArchitecture = UpdateTargetsArchitecture String
derive instance newtypeUpdateTargetsArchitecture :: Newtype UpdateTargetsArchitecture _
derive instance repGenericUpdateTargetsArchitecture :: Generic UpdateTargetsArchitecture _
instance showUpdateTargetsArchitecture :: Show UpdateTargetsArchitecture where
  show = genericShow
instance decodeUpdateTargetsArchitecture :: Decode UpdateTargetsArchitecture where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTargetsArchitecture :: Encode UpdateTargetsArchitecture where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The operating system of the Cores in the targets of an update
newtype UpdateTargetsOperatingSystem = UpdateTargetsOperatingSystem String
derive instance newtypeUpdateTargetsOperatingSystem :: Newtype UpdateTargetsOperatingSystem _
derive instance repGenericUpdateTargetsOperatingSystem :: Generic UpdateTargetsOperatingSystem _
instance showUpdateTargetsOperatingSystem :: Show UpdateTargetsOperatingSystem where
  show = genericShow
instance decodeUpdateTargetsOperatingSystem :: Decode UpdateTargetsOperatingSystem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTargetsOperatingSystem :: Encode UpdateTargetsOperatingSystem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information on the version
newtype VersionInformation = VersionInformation 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeVersionInformation :: Newtype VersionInformation _
derive instance repGenericVersionInformation :: Generic VersionInformation _
instance showVersionInformation :: Show VersionInformation where
  show = genericShow
instance decodeVersionInformation :: Decode VersionInformation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionInformation :: Encode VersionInformation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
