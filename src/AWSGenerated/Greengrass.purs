

-- | AWS Greengrass seamlessly extends AWS onto physical devices so they can act locally on the data they generate, while still using the cloud for management, analytics, and durable storage. AWS Greengrass ensures your devices can respond quickly to local events and operate with intermittent connectivity. AWS Greengrass minimizes the cost of transmitting data to the cloud by allowing you to author AWS Lambda functions that execute locally.
module AWS.Greengrass where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Greengrass" :: String


-- | Associates a role with a group. The role will be used by the AWS Greengrass core in order to access AWS cloud services. The role's permissions will allow Greengrass core Lambda functions to perform actions against the cloud.
associateRoleToGroup :: forall eff. AssociateRoleToGroupRequest -> Aff (err :: AWS.RequestError | eff) AssociateRoleToGroupResponse
associateRoleToGroup = AWS.request serviceName "AssociateRoleToGroup" 


-- | Associates a role which is used by AWS Greengrass. AWS Greengrass uses the role to access your Lambda functions and AWS IoT resources. This is necessary for deployments to succeed. It needs to have minimum permissions in policy ``AWSGreengrassResourceAccessRolePolicy``
associateServiceRoleToAccount :: forall eff. AssociateServiceRoleToAccountRequest -> Aff (err :: AWS.RequestError | eff) AssociateServiceRoleToAccountResponse
associateServiceRoleToAccount = AWS.request serviceName "AssociateServiceRoleToAccount" 


-- | Creates a core definition. You may optionally provide the initial version of the core definition or use ''CreateCoreDefinitionVersion'' at a later time. AWS Greengrass Groups must each contain exactly 1 AWS Greengrass Core.
createCoreDefinition :: forall eff. CreateCoreDefinitionRequest -> Aff (err :: AWS.RequestError | eff) CreateCoreDefinitionResponse
createCoreDefinition = AWS.request serviceName "CreateCoreDefinition" 


-- | Creates a version of a core definition that has already been defined. AWS Greengrass Groups must each contain exactly 1 AWS Greengrass Core.
createCoreDefinitionVersion :: forall eff. CreateCoreDefinitionVersionRequest -> Aff (err :: AWS.RequestError | eff) CreateCoreDefinitionVersionResponse
createCoreDefinitionVersion = AWS.request serviceName "CreateCoreDefinitionVersion" 


-- | Creates a deployment.
createDeployment :: forall eff. CreateDeploymentRequest -> Aff (err :: AWS.RequestError | eff) CreateDeploymentResponse
createDeployment = AWS.request serviceName "CreateDeployment" 


-- | Creates a device definition. You may optinally provide the initial version of the device definition or use ``CreateDeviceDefinitionVersion`` at a later time.
createDeviceDefinition :: forall eff. CreateDeviceDefinitionRequest -> Aff (err :: AWS.RequestError | eff) CreateDeviceDefinitionResponse
createDeviceDefinition = AWS.request serviceName "CreateDeviceDefinition" 


-- | Creates a version of a device definition that has already been defined.
createDeviceDefinitionVersion :: forall eff. CreateDeviceDefinitionVersionRequest -> Aff (err :: AWS.RequestError | eff) CreateDeviceDefinitionVersionResponse
createDeviceDefinitionVersion = AWS.request serviceName "CreateDeviceDefinitionVersion" 


-- | Creates a Lambda function definition which contains a list of Lambda functions and their configurations to be used in a group. You can create an initial version of the definition by providing a list of Lambda functions and their configurations now, or use ``CreateFunctionDefinitionVersion`` later.
createFunctionDefinition :: forall eff. CreateFunctionDefinitionRequest -> Aff (err :: AWS.RequestError | eff) CreateFunctionDefinitionResponse
createFunctionDefinition = AWS.request serviceName "CreateFunctionDefinition" 


-- | Create a version of a Lambda function definition that has already been defined.
createFunctionDefinitionVersion :: forall eff. CreateFunctionDefinitionVersionRequest -> Aff (err :: AWS.RequestError | eff) CreateFunctionDefinitionVersionResponse
createFunctionDefinitionVersion = AWS.request serviceName "CreateFunctionDefinitionVersion" 


-- | Creates a group. You may optionally provide the initial version of the group or use ''CreateGroupVersion'' at a later time.
createGroup :: forall eff. CreateGroupRequest -> Aff (err :: AWS.RequestError | eff) CreateGroupResponse
createGroup = AWS.request serviceName "CreateGroup" 


-- | Creates a CA for the group. If a CA already exists, it will rotate the existing CA.
createGroupCertificateAuthority :: forall eff. CreateGroupCertificateAuthorityRequest -> Aff (err :: AWS.RequestError | eff) CreateGroupCertificateAuthorityResponse
createGroupCertificateAuthority = AWS.request serviceName "CreateGroupCertificateAuthority" 


-- | Creates a version of a group which has already been defined.
createGroupVersion :: forall eff. CreateGroupVersionRequest -> Aff (err :: AWS.RequestError | eff) CreateGroupVersionResponse
createGroupVersion = AWS.request serviceName "CreateGroupVersion" 


-- | Creates a logger definition. You may optionally provide the initial version of the logger definition or use ``CreateLoggerDefinitionVersion`` at a later time.
createLoggerDefinition :: forall eff. CreateLoggerDefinitionRequest -> Aff (err :: AWS.RequestError | eff) CreateLoggerDefinitionResponse
createLoggerDefinition = AWS.request serviceName "CreateLoggerDefinition" 


-- | Creates a version of a logger definition that has already been defined.
createLoggerDefinitionVersion :: forall eff. CreateLoggerDefinitionVersionRequest -> Aff (err :: AWS.RequestError | eff) CreateLoggerDefinitionVersionResponse
createLoggerDefinitionVersion = AWS.request serviceName "CreateLoggerDefinitionVersion" 


-- | Creates a resource definition which contains a list of resources to be used in a group. You can create an initial version of the definition by providing a list of resources now, or use ``CreateResourceDefinitionVersion`` later.
createResourceDefinition :: forall eff. CreateResourceDefinitionRequest -> Aff (err :: AWS.RequestError | eff) CreateResourceDefinitionResponse
createResourceDefinition = AWS.request serviceName "CreateResourceDefinition" 


-- | Create a version of a resource definition that has already been defined.
createResourceDefinitionVersion :: forall eff. CreateResourceDefinitionVersionRequest -> Aff (err :: AWS.RequestError | eff) CreateResourceDefinitionVersionResponse
createResourceDefinitionVersion = AWS.request serviceName "CreateResourceDefinitionVersion" 


-- | Creates an Iot Job that will trigger your Greengrass Cores to update the software they are running.
createSoftwareUpdateJob :: forall eff. CreateSoftwareUpdateJobRequest -> Aff (err :: AWS.RequestError | eff) CreateSoftwareUpdateJobResponse
createSoftwareUpdateJob = AWS.request serviceName "CreateSoftwareUpdateJob" 


-- | Creates a subscription definition. You may optionally provide the initial version of the subscription definition or use ``CreateSubscriptionDefinitionVersion`` at a later time.
createSubscriptionDefinition :: forall eff. CreateSubscriptionDefinitionRequest -> Aff (err :: AWS.RequestError | eff) CreateSubscriptionDefinitionResponse
createSubscriptionDefinition = AWS.request serviceName "CreateSubscriptionDefinition" 


-- | Creates a version of a subscription definition which has already been defined.
createSubscriptionDefinitionVersion :: forall eff. CreateSubscriptionDefinitionVersionRequest -> Aff (err :: AWS.RequestError | eff) CreateSubscriptionDefinitionVersionResponse
createSubscriptionDefinitionVersion = AWS.request serviceName "CreateSubscriptionDefinitionVersion" 


-- | Deletes a core definition. The core definition must not have been used in a deployment.
deleteCoreDefinition :: forall eff. DeleteCoreDefinitionRequest -> Aff (err :: AWS.RequestError | eff) DeleteCoreDefinitionResponse
deleteCoreDefinition = AWS.request serviceName "DeleteCoreDefinition" 


-- | Deletes a device definition. The device definition must not have been used in a deployment.
deleteDeviceDefinition :: forall eff. DeleteDeviceDefinitionRequest -> Aff (err :: AWS.RequestError | eff) DeleteDeviceDefinitionResponse
deleteDeviceDefinition = AWS.request serviceName "DeleteDeviceDefinition" 


-- | Deletes a Lambda function definition. The Lambda function definition must not have been used in a deployment.
deleteFunctionDefinition :: forall eff. DeleteFunctionDefinitionRequest -> Aff (err :: AWS.RequestError | eff) DeleteFunctionDefinitionResponse
deleteFunctionDefinition = AWS.request serviceName "DeleteFunctionDefinition" 


-- | Deletes a group. The group must not have been used in deployment.
deleteGroup :: forall eff. DeleteGroupRequest -> Aff (err :: AWS.RequestError | eff) DeleteGroupResponse
deleteGroup = AWS.request serviceName "DeleteGroup" 


-- | Deletes a logger definition. The logger definition must not have been used in a deployment.
deleteLoggerDefinition :: forall eff. DeleteLoggerDefinitionRequest -> Aff (err :: AWS.RequestError | eff) DeleteLoggerDefinitionResponse
deleteLoggerDefinition = AWS.request serviceName "DeleteLoggerDefinition" 


-- | Deletes a resource definition.
deleteResourceDefinition :: forall eff. DeleteResourceDefinitionRequest -> Aff (err :: AWS.RequestError | eff) DeleteResourceDefinitionResponse
deleteResourceDefinition = AWS.request serviceName "DeleteResourceDefinition" 


-- | Deletes a subscription definition. The subscription definition must not have been used in a deployment.
deleteSubscriptionDefinition :: forall eff. DeleteSubscriptionDefinitionRequest -> Aff (err :: AWS.RequestError | eff) DeleteSubscriptionDefinitionResponse
deleteSubscriptionDefinition = AWS.request serviceName "DeleteSubscriptionDefinition" 


-- | Disassociates the role from a group.
disassociateRoleFromGroup :: forall eff. DisassociateRoleFromGroupRequest -> Aff (err :: AWS.RequestError | eff) DisassociateRoleFromGroupResponse
disassociateRoleFromGroup = AWS.request serviceName "DisassociateRoleFromGroup" 


-- | Disassociates the service role from the account. Without a service role, deployments will not work.
disassociateServiceRoleFromAccount :: forall eff. DisassociateServiceRoleFromAccountRequest -> Aff (err :: AWS.RequestError | eff) DisassociateServiceRoleFromAccountResponse
disassociateServiceRoleFromAccount = AWS.request serviceName "DisassociateServiceRoleFromAccount" 


-- | Retrieves the role associated with a particular group.
getAssociatedRole :: forall eff. GetAssociatedRoleRequest -> Aff (err :: AWS.RequestError | eff) GetAssociatedRoleResponse
getAssociatedRole = AWS.request serviceName "GetAssociatedRole" 


-- | Retrieves the connectivity information for a core.
getConnectivityInfo :: forall eff. GetConnectivityInfoRequest -> Aff (err :: AWS.RequestError | eff) GetConnectivityInfoResponse
getConnectivityInfo = AWS.request serviceName "GetConnectivityInfo" 


-- | Retrieves information about a core definition version.
getCoreDefinition :: forall eff. GetCoreDefinitionRequest -> Aff (err :: AWS.RequestError | eff) GetCoreDefinitionResponse
getCoreDefinition = AWS.request serviceName "GetCoreDefinition" 


-- | Retrieves information about a core definition version.
getCoreDefinitionVersion :: forall eff. GetCoreDefinitionVersionRequest -> Aff (err :: AWS.RequestError | eff) GetCoreDefinitionVersionResponse
getCoreDefinitionVersion = AWS.request serviceName "GetCoreDefinitionVersion" 


-- | Returns the status of a deployment.
getDeploymentStatus :: forall eff. GetDeploymentStatusRequest -> Aff (err :: AWS.RequestError | eff) GetDeploymentStatusResponse
getDeploymentStatus = AWS.request serviceName "GetDeploymentStatus" 


-- | Retrieves information about a device definition.
getDeviceDefinition :: forall eff. GetDeviceDefinitionRequest -> Aff (err :: AWS.RequestError | eff) GetDeviceDefinitionResponse
getDeviceDefinition = AWS.request serviceName "GetDeviceDefinition" 


-- | Retrieves information about a device definition version.
getDeviceDefinitionVersion :: forall eff. GetDeviceDefinitionVersionRequest -> Aff (err :: AWS.RequestError | eff) GetDeviceDefinitionVersionResponse
getDeviceDefinitionVersion = AWS.request serviceName "GetDeviceDefinitionVersion" 


-- | Retrieves information about a Lambda function definition, such as its creation time and latest version.
getFunctionDefinition :: forall eff. GetFunctionDefinitionRequest -> Aff (err :: AWS.RequestError | eff) GetFunctionDefinitionResponse
getFunctionDefinition = AWS.request serviceName "GetFunctionDefinition" 


-- | Retrieves information about a Lambda function definition version, such as which Lambda functions are included in the version and their configurations.
getFunctionDefinitionVersion :: forall eff. GetFunctionDefinitionVersionRequest -> Aff (err :: AWS.RequestError | eff) GetFunctionDefinitionVersionResponse
getFunctionDefinitionVersion = AWS.request serviceName "GetFunctionDefinitionVersion" 


-- | Retrieves information about a group.
getGroup :: forall eff. GetGroupRequest -> Aff (err :: AWS.RequestError | eff) GetGroupResponse
getGroup = AWS.request serviceName "GetGroup" 


-- | Retreives the CA associated with a group. Returns the public key of the CA.
getGroupCertificateAuthority :: forall eff. GetGroupCertificateAuthorityRequest -> Aff (err :: AWS.RequestError | eff) GetGroupCertificateAuthorityResponse
getGroupCertificateAuthority = AWS.request serviceName "GetGroupCertificateAuthority" 


-- | Retrieves the current configuration for the CA used by the group.
getGroupCertificateConfiguration :: forall eff. GetGroupCertificateConfigurationRequest -> Aff (err :: AWS.RequestError | eff) GetGroupCertificateConfigurationResponse
getGroupCertificateConfiguration = AWS.request serviceName "GetGroupCertificateConfiguration" 


-- | Retrieves information about a group version.
getGroupVersion :: forall eff. GetGroupVersionRequest -> Aff (err :: AWS.RequestError | eff) GetGroupVersionResponse
getGroupVersion = AWS.request serviceName "GetGroupVersion" 


-- | Retrieves information about a logger definition.
getLoggerDefinition :: forall eff. GetLoggerDefinitionRequest -> Aff (err :: AWS.RequestError | eff) GetLoggerDefinitionResponse
getLoggerDefinition = AWS.request serviceName "GetLoggerDefinition" 


-- | Retrieves information about a logger definition version.
getLoggerDefinitionVersion :: forall eff. GetLoggerDefinitionVersionRequest -> Aff (err :: AWS.RequestError | eff) GetLoggerDefinitionVersionResponse
getLoggerDefinitionVersion = AWS.request serviceName "GetLoggerDefinitionVersion" 


-- | Retrieves information about a resource definition, such as its creation time and latest version.
getResourceDefinition :: forall eff. GetResourceDefinitionRequest -> Aff (err :: AWS.RequestError | eff) GetResourceDefinitionResponse
getResourceDefinition = AWS.request serviceName "GetResourceDefinition" 


-- | Retrieves information about a resource definition version, such as which resources are included in the version.
getResourceDefinitionVersion :: forall eff. GetResourceDefinitionVersionRequest -> Aff (err :: AWS.RequestError | eff) GetResourceDefinitionVersionResponse
getResourceDefinitionVersion = AWS.request serviceName "GetResourceDefinitionVersion" 


-- | Retrieves the service role that is attached to the account.
getServiceRoleForAccount :: forall eff. GetServiceRoleForAccountRequest -> Aff (err :: AWS.RequestError | eff) GetServiceRoleForAccountResponse
getServiceRoleForAccount = AWS.request serviceName "GetServiceRoleForAccount" 


-- | Retrieves information about a subscription definition.
getSubscriptionDefinition :: forall eff. GetSubscriptionDefinitionRequest -> Aff (err :: AWS.RequestError | eff) GetSubscriptionDefinitionResponse
getSubscriptionDefinition = AWS.request serviceName "GetSubscriptionDefinition" 


-- | Retrieves information about a subscription definition version.
getSubscriptionDefinitionVersion :: forall eff. GetSubscriptionDefinitionVersionRequest -> Aff (err :: AWS.RequestError | eff) GetSubscriptionDefinitionVersionResponse
getSubscriptionDefinitionVersion = AWS.request serviceName "GetSubscriptionDefinitionVersion" 


-- | Lists versions of a core definition.
listCoreDefinitionVersions :: forall eff. ListCoreDefinitionVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListCoreDefinitionVersionsResponse
listCoreDefinitionVersions = AWS.request serviceName "ListCoreDefinitionVersions" 


-- | Retrieves a list of core definitions.
listCoreDefinitions :: forall eff. ListCoreDefinitionsRequest -> Aff (err :: AWS.RequestError | eff) ListCoreDefinitionsResponse
listCoreDefinitions = AWS.request serviceName "ListCoreDefinitions" 


-- | Returns a history of deployments for the group.
listDeployments :: forall eff. ListDeploymentsRequest -> Aff (err :: AWS.RequestError | eff) ListDeploymentsResponse
listDeployments = AWS.request serviceName "ListDeployments" 


-- | Lists the versions of a device definition.
listDeviceDefinitionVersions :: forall eff. ListDeviceDefinitionVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListDeviceDefinitionVersionsResponse
listDeviceDefinitionVersions = AWS.request serviceName "ListDeviceDefinitionVersions" 


-- | Retrieves a list of device definitions.
listDeviceDefinitions :: forall eff. ListDeviceDefinitionsRequest -> Aff (err :: AWS.RequestError | eff) ListDeviceDefinitionsResponse
listDeviceDefinitions = AWS.request serviceName "ListDeviceDefinitions" 


-- | Lists the versions of a Lambda function definition.
listFunctionDefinitionVersions :: forall eff. ListFunctionDefinitionVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListFunctionDefinitionVersionsResponse
listFunctionDefinitionVersions = AWS.request serviceName "ListFunctionDefinitionVersions" 


-- | Retrieves a list of Lambda function definitions.
listFunctionDefinitions :: forall eff. ListFunctionDefinitionsRequest -> Aff (err :: AWS.RequestError | eff) ListFunctionDefinitionsResponse
listFunctionDefinitions = AWS.request serviceName "ListFunctionDefinitions" 


-- | Retrieves the current CAs for a group.
listGroupCertificateAuthorities :: forall eff. ListGroupCertificateAuthoritiesRequest -> Aff (err :: AWS.RequestError | eff) ListGroupCertificateAuthoritiesResponse
listGroupCertificateAuthorities = AWS.request serviceName "ListGroupCertificateAuthorities" 


-- | List the versions of a group.
listGroupVersions :: forall eff. ListGroupVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListGroupVersionsResponse
listGroupVersions = AWS.request serviceName "ListGroupVersions" 


-- | Retrieves a list of groups.
listGroups :: forall eff. ListGroupsRequest -> Aff (err :: AWS.RequestError | eff) ListGroupsResponse
listGroups = AWS.request serviceName "ListGroups" 


-- | Lists the versions of a logger definition.
listLoggerDefinitionVersions :: forall eff. ListLoggerDefinitionVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListLoggerDefinitionVersionsResponse
listLoggerDefinitionVersions = AWS.request serviceName "ListLoggerDefinitionVersions" 


-- | Retrieves a list of logger definitions.
listLoggerDefinitions :: forall eff. ListLoggerDefinitionsRequest -> Aff (err :: AWS.RequestError | eff) ListLoggerDefinitionsResponse
listLoggerDefinitions = AWS.request serviceName "ListLoggerDefinitions" 


-- | Lists the versions of a resource definition.
listResourceDefinitionVersions :: forall eff. ListResourceDefinitionVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListResourceDefinitionVersionsResponse
listResourceDefinitionVersions = AWS.request serviceName "ListResourceDefinitionVersions" 


-- | Retrieves a list of resource definitions.
listResourceDefinitions :: forall eff. ListResourceDefinitionsRequest -> Aff (err :: AWS.RequestError | eff) ListResourceDefinitionsResponse
listResourceDefinitions = AWS.request serviceName "ListResourceDefinitions" 


-- | Lists the versions of a subscription definition.
listSubscriptionDefinitionVersions :: forall eff. ListSubscriptionDefinitionVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListSubscriptionDefinitionVersionsResponse
listSubscriptionDefinitionVersions = AWS.request serviceName "ListSubscriptionDefinitionVersions" 


-- | Retrieves a list of subscription definitions.
listSubscriptionDefinitions :: forall eff. ListSubscriptionDefinitionsRequest -> Aff (err :: AWS.RequestError | eff) ListSubscriptionDefinitionsResponse
listSubscriptionDefinitions = AWS.request serviceName "ListSubscriptionDefinitions" 


-- | Resets a group's deployments.
resetDeployments :: forall eff. ResetDeploymentsRequest -> Aff (err :: AWS.RequestError | eff) ResetDeploymentsResponse
resetDeployments = AWS.request serviceName "ResetDeployments" 


-- | Updates the connectivity information for the core. Any devices that belong to the group which has this core will receive this information in order to find the location of the core and connect to it.
updateConnectivityInfo :: forall eff. UpdateConnectivityInfoRequest -> Aff (err :: AWS.RequestError | eff) UpdateConnectivityInfoResponse
updateConnectivityInfo = AWS.request serviceName "UpdateConnectivityInfo" 


-- | Updates a core definition.
updateCoreDefinition :: forall eff. UpdateCoreDefinitionRequest -> Aff (err :: AWS.RequestError | eff) UpdateCoreDefinitionResponse
updateCoreDefinition = AWS.request serviceName "UpdateCoreDefinition" 


-- | Updates a device definition.
updateDeviceDefinition :: forall eff. UpdateDeviceDefinitionRequest -> Aff (err :: AWS.RequestError | eff) UpdateDeviceDefinitionResponse
updateDeviceDefinition = AWS.request serviceName "UpdateDeviceDefinition" 


-- | Updates a Lambda function definition.
updateFunctionDefinition :: forall eff. UpdateFunctionDefinitionRequest -> Aff (err :: AWS.RequestError | eff) UpdateFunctionDefinitionResponse
updateFunctionDefinition = AWS.request serviceName "UpdateFunctionDefinition" 


-- | Updates a group.
updateGroup :: forall eff. UpdateGroupRequest -> Aff (err :: AWS.RequestError | eff) UpdateGroupResponse
updateGroup = AWS.request serviceName "UpdateGroup" 


-- | Updates the Cert expiry time for a group.
updateGroupCertificateConfiguration :: forall eff. UpdateGroupCertificateConfigurationRequest -> Aff (err :: AWS.RequestError | eff) UpdateGroupCertificateConfigurationResponse
updateGroupCertificateConfiguration = AWS.request serviceName "UpdateGroupCertificateConfiguration" 


-- | Updates a logger definition.
updateLoggerDefinition :: forall eff. UpdateLoggerDefinitionRequest -> Aff (err :: AWS.RequestError | eff) UpdateLoggerDefinitionResponse
updateLoggerDefinition = AWS.request serviceName "UpdateLoggerDefinition" 


-- | Updates a resource definition.
updateResourceDefinition :: forall eff. UpdateResourceDefinitionRequest -> Aff (err :: AWS.RequestError | eff) UpdateResourceDefinitionResponse
updateResourceDefinition = AWS.request serviceName "UpdateResourceDefinition" 


-- | Updates a subscription definition.
updateSubscriptionDefinition :: forall eff. UpdateSubscriptionDefinitionRequest -> Aff (err :: AWS.RequestError | eff) UpdateSubscriptionDefinitionResponse
updateSubscriptionDefinition = AWS.request serviceName "UpdateSubscriptionDefinition" 


newtype AssociateRoleToGroupRequest = AssociateRoleToGroupRequest 
  { "GroupId" :: (String)
  , "RoleArn" :: NullOrUndefined (String)
  }


newtype AssociateRoleToGroupResponse = AssociateRoleToGroupResponse 
  { "AssociatedAt" :: NullOrUndefined (String)
  }


newtype AssociateServiceRoleToAccountRequest = AssociateServiceRoleToAccountRequest 
  { "RoleArn" :: NullOrUndefined (String)
  }


newtype AssociateServiceRoleToAccountResponse = AssociateServiceRoleToAccountResponse 
  { "AssociatedAt" :: NullOrUndefined (String)
  }


-- | General Error
newtype BadRequestException = BadRequestException 
  { "ErrorDetails" :: NullOrUndefined (ErrorDetails)
  , "Message" :: NullOrUndefined (String)
  }


-- | Connectivity Info
newtype ConnectivityInfo = ConnectivityInfo 
  { "HostAddress" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Metadata" :: NullOrUndefined (String)
  , "PortNumber" :: NullOrUndefined (Int)
  }


-- | Information on the core
newtype Core = Core 
  { "CertificateArn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "SyncShadow" :: NullOrUndefined (Boolean)
  , "ThingArn" :: NullOrUndefined (String)
  }


-- | Information on core definition version
newtype CoreDefinitionVersion = CoreDefinitionVersion 
  { "Cores" :: NullOrUndefined (ListOfCore)
  }


-- | Information on the core definition request
newtype CreateCoreDefinitionRequest = CreateCoreDefinitionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined (CoreDefinitionVersion)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateCoreDefinitionResponse = CreateCoreDefinitionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateCoreDefinitionVersionRequest = CreateCoreDefinitionVersionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "CoreDefinitionId" :: (String)
  , "Cores" :: NullOrUndefined (ListOfCore)
  }


newtype CreateCoreDefinitionVersionResponse = CreateCoreDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype CreateDeploymentRequest = CreateDeploymentRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "DeploymentId" :: NullOrUndefined (String)
  , "DeploymentType" :: NullOrUndefined (DeploymentType)
  , "GroupId" :: (String)
  , "GroupVersionId" :: NullOrUndefined (String)
  }


newtype CreateDeploymentResponse = CreateDeploymentResponse 
  { "DeploymentArn" :: NullOrUndefined (String)
  , "DeploymentId" :: NullOrUndefined (String)
  }


newtype CreateDeviceDefinitionRequest = CreateDeviceDefinitionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined (DeviceDefinitionVersion)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateDeviceDefinitionResponse = CreateDeviceDefinitionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateDeviceDefinitionVersionRequest = CreateDeviceDefinitionVersionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "DeviceDefinitionId" :: (String)
  , "Devices" :: NullOrUndefined (ListOfDevice)
  }


newtype CreateDeviceDefinitionVersionResponse = CreateDeviceDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype CreateFunctionDefinitionRequest = CreateFunctionDefinitionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined (FunctionDefinitionVersion)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateFunctionDefinitionResponse = CreateFunctionDefinitionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


-- | Function definition version
newtype CreateFunctionDefinitionVersionRequest = CreateFunctionDefinitionVersionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "FunctionDefinitionId" :: (String)
  , "Functions" :: NullOrUndefined (ListOfFunction)
  }


newtype CreateFunctionDefinitionVersionResponse = CreateFunctionDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype CreateGroupCertificateAuthorityRequest = CreateGroupCertificateAuthorityRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "GroupId" :: (String)
  }


newtype CreateGroupCertificateAuthorityResponse = CreateGroupCertificateAuthorityResponse 
  { "GroupCertificateAuthorityArn" :: NullOrUndefined (String)
  }


newtype CreateGroupRequest = CreateGroupRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined (GroupVersion)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateGroupResponse = CreateGroupResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateGroupVersionRequest = CreateGroupVersionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "CoreDefinitionVersionArn" :: NullOrUndefined (String)
  , "DeviceDefinitionVersionArn" :: NullOrUndefined (String)
  , "FunctionDefinitionVersionArn" :: NullOrUndefined (String)
  , "GroupId" :: (String)
  , "LoggerDefinitionVersionArn" :: NullOrUndefined (String)
  , "ResourceDefinitionVersionArn" :: NullOrUndefined (String)
  , "SubscriptionDefinitionVersionArn" :: NullOrUndefined (String)
  }


newtype CreateGroupVersionResponse = CreateGroupVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype CreateLoggerDefinitionRequest = CreateLoggerDefinitionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined (LoggerDefinitionVersion)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateLoggerDefinitionResponse = CreateLoggerDefinitionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateLoggerDefinitionVersionRequest = CreateLoggerDefinitionVersionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "LoggerDefinitionId" :: (String)
  , "Loggers" :: NullOrUndefined (ListOfLogger)
  }


newtype CreateLoggerDefinitionVersionResponse = CreateLoggerDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype CreateResourceDefinitionRequest = CreateResourceDefinitionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined (ResourceDefinitionVersion)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateResourceDefinitionResponse = CreateResourceDefinitionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateResourceDefinitionVersionRequest = CreateResourceDefinitionVersionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "ResourceDefinitionId" :: (String)
  , "Resources" :: NullOrUndefined (ListOfResource)
  }


newtype CreateResourceDefinitionVersionResponse = CreateResourceDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype CreateSoftwareUpdateJobRequest = CreateSoftwareUpdateJobRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "S3UrlSignerRole" :: NullOrUndefined (S3UrlSignerRole)
  , "SoftwareToUpdate" :: NullOrUndefined (SoftwareToUpdate)
  , "UpdateAgentLogLevel" :: NullOrUndefined (UpdateAgentLogLevel)
  , "UpdateTargets" :: NullOrUndefined (UpdateTargets)
  , "UpdateTargetsArchitecture" :: NullOrUndefined (UpdateTargetsArchitecture)
  , "UpdateTargetsOperatingSystem" :: NullOrUndefined (UpdateTargetsOperatingSystem)
  }


newtype CreateSoftwareUpdateJobResponse = CreateSoftwareUpdateJobResponse 
  { "IotJobArn" :: NullOrUndefined (String)
  , "IotJobId" :: NullOrUndefined (String)
  }


newtype CreateSubscriptionDefinitionRequest = CreateSubscriptionDefinitionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "InitialVersion" :: NullOrUndefined (SubscriptionDefinitionVersion)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateSubscriptionDefinitionResponse = CreateSubscriptionDefinitionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateSubscriptionDefinitionVersionRequest = CreateSubscriptionDefinitionVersionRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "SubscriptionDefinitionId" :: (String)
  , "Subscriptions" :: NullOrUndefined (ListOfSubscription)
  }


newtype CreateSubscriptionDefinitionVersionResponse = CreateSubscriptionDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


-- | Information on the Definition
newtype DefinitionInformation = DefinitionInformation 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype DeleteCoreDefinitionRequest = DeleteCoreDefinitionRequest 
  { "CoreDefinitionId" :: (String)
  }


newtype DeleteCoreDefinitionResponse = DeleteCoreDefinitionResponse 
  { 
  }


newtype DeleteDeviceDefinitionRequest = DeleteDeviceDefinitionRequest 
  { "DeviceDefinitionId" :: (String)
  }


newtype DeleteDeviceDefinitionResponse = DeleteDeviceDefinitionResponse 
  { 
  }


newtype DeleteFunctionDefinitionRequest = DeleteFunctionDefinitionRequest 
  { "FunctionDefinitionId" :: (String)
  }


newtype DeleteFunctionDefinitionResponse = DeleteFunctionDefinitionResponse 
  { 
  }


newtype DeleteGroupRequest = DeleteGroupRequest 
  { "GroupId" :: (String)
  }


newtype DeleteGroupResponse = DeleteGroupResponse 
  { 
  }


newtype DeleteLoggerDefinitionRequest = DeleteLoggerDefinitionRequest 
  { "LoggerDefinitionId" :: (String)
  }


newtype DeleteLoggerDefinitionResponse = DeleteLoggerDefinitionResponse 
  { 
  }


newtype DeleteResourceDefinitionRequest = DeleteResourceDefinitionRequest 
  { "ResourceDefinitionId" :: (String)
  }


newtype DeleteResourceDefinitionResponse = DeleteResourceDefinitionResponse 
  { 
  }


newtype DeleteSubscriptionDefinitionRequest = DeleteSubscriptionDefinitionRequest 
  { "SubscriptionDefinitionId" :: (String)
  }


newtype DeleteSubscriptionDefinitionResponse = DeleteSubscriptionDefinitionResponse 
  { 
  }


-- | Information on the deployment
newtype Deployment = Deployment 
  { "CreatedAt" :: NullOrUndefined (String)
  , "DeploymentArn" :: NullOrUndefined (String)
  , "DeploymentId" :: NullOrUndefined (String)
  , "DeploymentType" :: NullOrUndefined (DeploymentType)
  , "GroupArn" :: NullOrUndefined (String)
  }


newtype DeploymentType = DeploymentType String


newtype Deployments = Deployments (Array Deployment)


-- | Information on a Device
newtype Device = Device 
  { "CertificateArn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "SyncShadow" :: NullOrUndefined (Boolean)
  , "ThingArn" :: NullOrUndefined (String)
  }


-- | Information on device definition version
newtype DeviceDefinitionVersion = DeviceDefinitionVersion 
  { "Devices" :: NullOrUndefined (ListOfDevice)
  }


newtype DisassociateRoleFromGroupRequest = DisassociateRoleFromGroupRequest 
  { "GroupId" :: (String)
  }


newtype DisassociateRoleFromGroupResponse = DisassociateRoleFromGroupResponse 
  { "DisassociatedAt" :: NullOrUndefined (String)
  }


newtype DisassociateServiceRoleFromAccountRequest = DisassociateServiceRoleFromAccountRequest 
  { 
  }


newtype DisassociateServiceRoleFromAccountResponse = DisassociateServiceRoleFromAccountResponse 
  { "DisassociatedAt" :: NullOrUndefined (String)
  }


-- | Empty
newtype Empty = Empty 
  { 
  }


-- | ErrorDetail
newtype ErrorDetail = ErrorDetail 
  { "DetailedErrorCode" :: NullOrUndefined (String)
  , "DetailedErrorMessage" :: NullOrUndefined (String)
  }


-- | Error Details
newtype ErrorDetails = ErrorDetails (Array ErrorDetail)


-- | Information on function
newtype Function'' = Function'' 
  { "FunctionArn" :: NullOrUndefined (String)
  , "FunctionConfiguration" :: NullOrUndefined (FunctionConfiguration)
  , "Id" :: NullOrUndefined (String)
  }


-- | Configuration of the function
newtype FunctionConfiguration = FunctionConfiguration 
  { "Environment" :: NullOrUndefined (FunctionConfigurationEnvironment)
  , "ExecArgs" :: NullOrUndefined (String)
  , "Executable" :: NullOrUndefined (String)
  , "MemorySize" :: NullOrUndefined (Int)
  , "Pinned" :: NullOrUndefined (Boolean)
  , "Timeout" :: NullOrUndefined (Int)
  }


-- | Environment of the function configuration
newtype FunctionConfigurationEnvironment = FunctionConfigurationEnvironment 
  { "AccessSysfs" :: NullOrUndefined (Boolean)
  , "ResourceAccessPolicies" :: NullOrUndefined (ListOfResourceAccessPolicy)
  , "Variables" :: NullOrUndefined (MapOf__string)
  }


-- | Information on the function definition version
newtype FunctionDefinitionVersion = FunctionDefinitionVersion 
  { "Functions" :: NullOrUndefined (ListOfFunction)
  }


-- | General Error
newtype GeneralError = GeneralError 
  { "ErrorDetails" :: NullOrUndefined (ErrorDetails)
  , "Message" :: NullOrUndefined (String)
  }


newtype GetAssociatedRoleRequest = GetAssociatedRoleRequest 
  { "GroupId" :: (String)
  }


newtype GetAssociatedRoleResponse = GetAssociatedRoleResponse 
  { "AssociatedAt" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }


newtype GetConnectivityInfoRequest = GetConnectivityInfoRequest 
  { "ThingName" :: (String)
  }


newtype GetConnectivityInfoResponse = GetConnectivityInfoResponse 
  { "ConnectivityInfo" :: NullOrUndefined (ListOfConnectivityInfo)
  , "Message" :: NullOrUndefined (String)
  }


newtype GetCoreDefinitionRequest = GetCoreDefinitionRequest 
  { "CoreDefinitionId" :: (String)
  }


newtype GetCoreDefinitionResponse = GetCoreDefinitionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype GetCoreDefinitionVersionRequest = GetCoreDefinitionVersionRequest 
  { "CoreDefinitionId" :: (String)
  , "CoreDefinitionVersionId" :: (String)
  }


newtype GetCoreDefinitionVersionResponse = GetCoreDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Definition" :: NullOrUndefined (CoreDefinitionVersion)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype GetDeploymentStatusRequest = GetDeploymentStatusRequest 
  { "DeploymentId" :: (String)
  , "GroupId" :: (String)
  }


newtype GetDeploymentStatusResponse = GetDeploymentStatusResponse 
  { "DeploymentStatus" :: NullOrUndefined (String)
  , "DeploymentType" :: NullOrUndefined (DeploymentType)
  , "ErrorDetails" :: NullOrUndefined (ErrorDetails)
  , "ErrorMessage" :: NullOrUndefined (String)
  , "UpdatedAt" :: NullOrUndefined (String)
  }


newtype GetDeviceDefinitionRequest = GetDeviceDefinitionRequest 
  { "DeviceDefinitionId" :: (String)
  }


newtype GetDeviceDefinitionResponse = GetDeviceDefinitionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype GetDeviceDefinitionVersionRequest = GetDeviceDefinitionVersionRequest 
  { "DeviceDefinitionId" :: (String)
  , "DeviceDefinitionVersionId" :: (String)
  }


newtype GetDeviceDefinitionVersionResponse = GetDeviceDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Definition" :: NullOrUndefined (DeviceDefinitionVersion)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype GetFunctionDefinitionRequest = GetFunctionDefinitionRequest 
  { "FunctionDefinitionId" :: (String)
  }


newtype GetFunctionDefinitionResponse = GetFunctionDefinitionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype GetFunctionDefinitionVersionRequest = GetFunctionDefinitionVersionRequest 
  { "FunctionDefinitionId" :: (String)
  , "FunctionDefinitionVersionId" :: (String)
  }


newtype GetFunctionDefinitionVersionResponse = GetFunctionDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Definition" :: NullOrUndefined (FunctionDefinitionVersion)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype GetGroupCertificateAuthorityRequest = GetGroupCertificateAuthorityRequest 
  { "CertificateAuthorityId" :: (String)
  , "GroupId" :: (String)
  }


newtype GetGroupCertificateAuthorityResponse = GetGroupCertificateAuthorityResponse 
  { "GroupCertificateAuthorityArn" :: NullOrUndefined (String)
  , "GroupCertificateAuthorityId" :: NullOrUndefined (String)
  , "PemEncodedCertificate" :: NullOrUndefined (String)
  }


newtype GetGroupCertificateConfigurationRequest = GetGroupCertificateConfigurationRequest 
  { "GroupId" :: (String)
  }


newtype GetGroupCertificateConfigurationResponse = GetGroupCertificateConfigurationResponse 
  { "CertificateAuthorityExpiryInMilliseconds" :: NullOrUndefined (String)
  , "CertificateExpiryInMilliseconds" :: NullOrUndefined (String)
  , "GroupId" :: NullOrUndefined (String)
  }


newtype GetGroupRequest = GetGroupRequest 
  { "GroupId" :: (String)
  }


newtype GetGroupResponse = GetGroupResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype GetGroupVersionRequest = GetGroupVersionRequest 
  { "GroupId" :: (String)
  , "GroupVersionId" :: (String)
  }


newtype GetGroupVersionResponse = GetGroupVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Definition" :: NullOrUndefined (GroupVersion)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype GetLoggerDefinitionRequest = GetLoggerDefinitionRequest 
  { "LoggerDefinitionId" :: (String)
  }


newtype GetLoggerDefinitionResponse = GetLoggerDefinitionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype GetLoggerDefinitionVersionRequest = GetLoggerDefinitionVersionRequest 
  { "LoggerDefinitionId" :: (String)
  , "LoggerDefinitionVersionId" :: (String)
  }


newtype GetLoggerDefinitionVersionResponse = GetLoggerDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Definition" :: NullOrUndefined (LoggerDefinitionVersion)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype GetResourceDefinitionRequest = GetResourceDefinitionRequest 
  { "ResourceDefinitionId" :: (String)
  }


newtype GetResourceDefinitionResponse = GetResourceDefinitionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype GetResourceDefinitionVersionRequest = GetResourceDefinitionVersionRequest 
  { "ResourceDefinitionId" :: (String)
  , "ResourceDefinitionVersionId" :: (String)
  }


newtype GetResourceDefinitionVersionResponse = GetResourceDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Definition" :: NullOrUndefined (ResourceDefinitionVersion)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype GetServiceRoleForAccountRequest = GetServiceRoleForAccountRequest 
  { 
  }


newtype GetServiceRoleForAccountResponse = GetServiceRoleForAccountResponse 
  { "AssociatedAt" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }


newtype GetSubscriptionDefinitionRequest = GetSubscriptionDefinitionRequest 
  { "SubscriptionDefinitionId" :: (String)
  }


newtype GetSubscriptionDefinitionResponse = GetSubscriptionDefinitionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype GetSubscriptionDefinitionVersionRequest = GetSubscriptionDefinitionVersionRequest 
  { "SubscriptionDefinitionId" :: (String)
  , "SubscriptionDefinitionVersionId" :: (String)
  }


newtype GetSubscriptionDefinitionVersionResponse = GetSubscriptionDefinitionVersionResponse 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Definition" :: NullOrUndefined (SubscriptionDefinitionVersion)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


-- | Information on group certificate authority properties
newtype GroupCertificateAuthorityProperties = GroupCertificateAuthorityProperties 
  { "GroupCertificateAuthorityArn" :: NullOrUndefined (String)
  , "GroupCertificateAuthorityId" :: NullOrUndefined (String)
  }


-- | Information on the group certificate configuration
newtype GroupCertificateConfiguration = GroupCertificateConfiguration 
  { "CertificateAuthorityExpiryInMilliseconds" :: NullOrUndefined (String)
  , "CertificateExpiryInMilliseconds" :: NullOrUndefined (String)
  , "GroupId" :: NullOrUndefined (String)
  }


-- | Information on the group
newtype GroupInformation = GroupInformation 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LastUpdatedTimestamp" :: NullOrUndefined (String)
  , "LatestVersion" :: NullOrUndefined (String)
  , "LatestVersionArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


-- | Group owner related settings for local resources.
newtype GroupOwnerSetting = GroupOwnerSetting 
  { "AutoAddGroupOwner" :: NullOrUndefined (Boolean)
  , "GroupOwner" :: NullOrUndefined (String)
  }


-- | Information on group version
newtype GroupVersion = GroupVersion 
  { "CoreDefinitionVersionArn" :: NullOrUndefined (String)
  , "DeviceDefinitionVersionArn" :: NullOrUndefined (String)
  , "FunctionDefinitionVersionArn" :: NullOrUndefined (String)
  , "LoggerDefinitionVersionArn" :: NullOrUndefined (String)
  , "ResourceDefinitionVersionArn" :: NullOrUndefined (String)
  , "SubscriptionDefinitionVersionArn" :: NullOrUndefined (String)
  }


-- | General Error
newtype InternalServerErrorException = InternalServerErrorException 
  { "ErrorDetails" :: NullOrUndefined (ErrorDetails)
  , "Message" :: NullOrUndefined (String)
  }


newtype ListCoreDefinitionVersionsRequest = ListCoreDefinitionVersionsRequest 
  { "CoreDefinitionId" :: (String)
  , "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListCoreDefinitionVersionsResponse = ListCoreDefinitionVersionsResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "Versions" :: NullOrUndefined (ListOfVersionInformation)
  }


newtype ListCoreDefinitionsRequest = ListCoreDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListCoreDefinitionsResponse = ListCoreDefinitionsResponse 
  { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | List of definition responses
newtype ListDefinitionsResponse = ListDefinitionsResponse 
  { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListDeploymentsRequest = ListDeploymentsRequest 
  { "GroupId" :: (String)
  , "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListDeploymentsResponse = ListDeploymentsResponse 
  { "Deployments" :: NullOrUndefined (Deployments)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListDeviceDefinitionVersionsRequest = ListDeviceDefinitionVersionsRequest 
  { "DeviceDefinitionId" :: (String)
  , "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListDeviceDefinitionVersionsResponse = ListDeviceDefinitionVersionsResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "Versions" :: NullOrUndefined (ListOfVersionInformation)
  }


newtype ListDeviceDefinitionsRequest = ListDeviceDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListDeviceDefinitionsResponse = ListDeviceDefinitionsResponse 
  { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListFunctionDefinitionVersionsRequest = ListFunctionDefinitionVersionsRequest 
  { "FunctionDefinitionId" :: (String)
  , "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListFunctionDefinitionVersionsResponse = ListFunctionDefinitionVersionsResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "Versions" :: NullOrUndefined (ListOfVersionInformation)
  }


newtype ListFunctionDefinitionsRequest = ListFunctionDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListFunctionDefinitionsResponse = ListFunctionDefinitionsResponse 
  { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListGroupCertificateAuthoritiesRequest = ListGroupCertificateAuthoritiesRequest 
  { "GroupId" :: (String)
  }


newtype ListGroupCertificateAuthoritiesResponse = ListGroupCertificateAuthoritiesResponse 
  { "GroupCertificateAuthorities" :: NullOrUndefined (ListOfGroupCertificateAuthorityProperties)
  }


newtype ListGroupVersionsRequest = ListGroupVersionsRequest 
  { "GroupId" :: (String)
  , "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListGroupVersionsResponse = ListGroupVersionsResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "Versions" :: NullOrUndefined (ListOfVersionInformation)
  }


newtype ListGroupsRequest = ListGroupsRequest 
  { "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListGroupsResponse = ListGroupsResponse 
  { "Groups" :: NullOrUndefined (ListOfGroupInformation)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListLoggerDefinitionVersionsRequest = ListLoggerDefinitionVersionsRequest 
  { "LoggerDefinitionId" :: (String)
  , "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListLoggerDefinitionVersionsResponse = ListLoggerDefinitionVersionsResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "Versions" :: NullOrUndefined (ListOfVersionInformation)
  }


newtype ListLoggerDefinitionsRequest = ListLoggerDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListLoggerDefinitionsResponse = ListLoggerDefinitionsResponse 
  { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListOfConnectivityInfo = ListOfConnectivityInfo (Array ConnectivityInfo)


newtype ListOfCore = ListOfCore (Array Core)


newtype ListOfDefinitionInformation = ListOfDefinitionInformation (Array DefinitionInformation)


newtype ListOfDevice = ListOfDevice (Array Device)


newtype ListOfFunction = ListOfFunction (Array Function'')


newtype ListOfGroupCertificateAuthorityProperties = ListOfGroupCertificateAuthorityProperties (Array GroupCertificateAuthorityProperties)


newtype ListOfGroupInformation = ListOfGroupInformation (Array GroupInformation)


newtype ListOfLogger = ListOfLogger (Array Logger)


newtype ListOfResource = ListOfResource (Array Resource)


newtype ListOfResourceAccessPolicy = ListOfResourceAccessPolicy (Array ResourceAccessPolicy)


newtype ListOfSubscription = ListOfSubscription (Array Subscription)


newtype ListOfVersionInformation = ListOfVersionInformation (Array VersionInformation)


newtype ListResourceDefinitionVersionsRequest = ListResourceDefinitionVersionsRequest 
  { "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  , "ResourceDefinitionId" :: (String)
  }


newtype ListResourceDefinitionVersionsResponse = ListResourceDefinitionVersionsResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "Versions" :: NullOrUndefined (ListOfVersionInformation)
  }


newtype ListResourceDefinitionsRequest = ListResourceDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListResourceDefinitionsResponse = ListResourceDefinitionsResponse 
  { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListSubscriptionDefinitionVersionsRequest = ListSubscriptionDefinitionVersionsRequest 
  { "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  , "SubscriptionDefinitionId" :: (String)
  }


newtype ListSubscriptionDefinitionVersionsResponse = ListSubscriptionDefinitionVersionsResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "Versions" :: NullOrUndefined (ListOfVersionInformation)
  }


newtype ListSubscriptionDefinitionsRequest = ListSubscriptionDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListSubscriptionDefinitionsResponse = ListSubscriptionDefinitionsResponse 
  { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | List of versions response
newtype ListVersionsResponse = ListVersionsResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "Versions" :: NullOrUndefined (ListOfVersionInformation)
  }


-- | Attributes that define the Local Device Resource.
newtype LocalDeviceResourceData = LocalDeviceResourceData 
  { "GroupOwnerSetting" :: NullOrUndefined (GroupOwnerSetting)
  , "SourcePath" :: NullOrUndefined (String)
  }


-- | Attributes that define the Local Volume Resource.
newtype LocalVolumeResourceData = LocalVolumeResourceData 
  { "DestinationPath" :: NullOrUndefined (String)
  , "GroupOwnerSetting" :: NullOrUndefined (GroupOwnerSetting)
  , "SourcePath" :: NullOrUndefined (String)
  }


-- | Information on the Logger
newtype Logger = Logger 
  { "Component" :: NullOrUndefined (LoggerComponent)
  , "Id" :: NullOrUndefined (String)
  , "Level" :: NullOrUndefined (LoggerLevel)
  , "Space" :: NullOrUndefined (Int)
  , "Type" :: NullOrUndefined (LoggerType)
  }


newtype LoggerComponent = LoggerComponent String


-- | Information on logger definition version
newtype LoggerDefinitionVersion = LoggerDefinitionVersion 
  { "Loggers" :: NullOrUndefined (ListOfLogger)
  }


newtype LoggerLevel = LoggerLevel String


newtype LoggerType = LoggerType String


newtype MapOf__string = MapOf__string (Map String String)


-- | Type of permissions a function could have to access a resource.
newtype Permission = Permission String


-- | Information needed to perform a reset of a group's deployments.
newtype ResetDeploymentsRequest = ResetDeploymentsRequest 
  { "AmznClientToken" :: NullOrUndefined (String)
  , "Force" :: NullOrUndefined (Boolean)
  , "GroupId" :: (String)
  }


newtype ResetDeploymentsResponse = ResetDeploymentsResponse 
  { "DeploymentArn" :: NullOrUndefined (String)
  , "DeploymentId" :: NullOrUndefined (String)
  }


-- | Information on the resource.
newtype Resource = Resource 
  { "Id" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "ResourceDataContainer" :: NullOrUndefined (ResourceDataContainer)
  }


-- | Policy for the function to access a resource.
newtype ResourceAccessPolicy = ResourceAccessPolicy 
  { "Permission" :: NullOrUndefined (Permission)
  , "ResourceId" :: NullOrUndefined (String)
  }


-- | A container of data for all resource types.
newtype ResourceDataContainer = ResourceDataContainer 
  { "LocalDeviceResourceData" :: NullOrUndefined (LocalDeviceResourceData)
  , "LocalVolumeResourceData" :: NullOrUndefined (LocalVolumeResourceData)
  }


-- | Information on resource definition version
newtype ResourceDefinitionVersion = ResourceDefinitionVersion 
  { "Resources" :: NullOrUndefined (ListOfResource)
  }


-- | The IAM Role that Greengrass will use to create pre-signed URLs pointing towards the update artifact.
newtype S3UrlSignerRole = S3UrlSignerRole String


-- | The piece of software on the Greengrass Core that will be updated.
newtype SoftwareToUpdate = SoftwareToUpdate String


-- | Information on subscription
newtype Subscription = Subscription 
  { "Id" :: NullOrUndefined (String)
  , "Source" :: NullOrUndefined (String)
  , "Subject" :: NullOrUndefined (String)
  , "Target" :: NullOrUndefined (String)
  }


-- | Information on subscription definition version
newtype SubscriptionDefinitionVersion = SubscriptionDefinitionVersion 
  { "Subscriptions" :: NullOrUndefined (ListOfSubscription)
  }


-- | The minimum level of log statements that should be logged by the OTA Agent during an update.
newtype UpdateAgentLogLevel = UpdateAgentLogLevel String


-- | connectivity info request
newtype UpdateConnectivityInfoRequest = UpdateConnectivityInfoRequest 
  { "ConnectivityInfo" :: NullOrUndefined (ListOfConnectivityInfo)
  , "ThingName" :: (String)
  }


newtype UpdateConnectivityInfoResponse = UpdateConnectivityInfoResponse 
  { "Message" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype UpdateCoreDefinitionRequest = UpdateCoreDefinitionRequest 
  { "CoreDefinitionId" :: (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype UpdateCoreDefinitionResponse = UpdateCoreDefinitionResponse 
  { 
  }


newtype UpdateDeviceDefinitionRequest = UpdateDeviceDefinitionRequest 
  { "DeviceDefinitionId" :: (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype UpdateDeviceDefinitionResponse = UpdateDeviceDefinitionResponse 
  { 
  }


newtype UpdateFunctionDefinitionRequest = UpdateFunctionDefinitionRequest 
  { "FunctionDefinitionId" :: (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype UpdateFunctionDefinitionResponse = UpdateFunctionDefinitionResponse 
  { 
  }


newtype UpdateGroupCertificateConfigurationRequest = UpdateGroupCertificateConfigurationRequest 
  { "CertificateExpiryInMilliseconds" :: NullOrUndefined (String)
  , "GroupId" :: (String)
  }


newtype UpdateGroupCertificateConfigurationResponse = UpdateGroupCertificateConfigurationResponse 
  { "CertificateAuthorityExpiryInMilliseconds" :: NullOrUndefined (String)
  , "CertificateExpiryInMilliseconds" :: NullOrUndefined (String)
  , "GroupId" :: NullOrUndefined (String)
  }


newtype UpdateGroupRequest = UpdateGroupRequest 
  { "GroupId" :: (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype UpdateGroupResponse = UpdateGroupResponse 
  { 
  }


newtype UpdateLoggerDefinitionRequest = UpdateLoggerDefinitionRequest 
  { "LoggerDefinitionId" :: (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype UpdateLoggerDefinitionResponse = UpdateLoggerDefinitionResponse 
  { 
  }


newtype UpdateResourceDefinitionRequest = UpdateResourceDefinitionRequest 
  { "Name" :: NullOrUndefined (String)
  , "ResourceDefinitionId" :: (String)
  }


newtype UpdateResourceDefinitionResponse = UpdateResourceDefinitionResponse 
  { 
  }


newtype UpdateSubscriptionDefinitionRequest = UpdateSubscriptionDefinitionRequest 
  { "Name" :: NullOrUndefined (String)
  , "SubscriptionDefinitionId" :: (String)
  }


newtype UpdateSubscriptionDefinitionResponse = UpdateSubscriptionDefinitionResponse 
  { 
  }


-- | The target arns that this update will be applied to.
newtype UpdateTargets = UpdateTargets (Array String)


-- | The architecture of the Cores in the targets of an update
newtype UpdateTargetsArchitecture = UpdateTargetsArchitecture String


-- | The operating system of the Cores in the targets of an update
newtype UpdateTargetsOperatingSystem = UpdateTargetsOperatingSystem String


-- | Information on the version
newtype VersionInformation = VersionInformation 
  { "Arn" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }
