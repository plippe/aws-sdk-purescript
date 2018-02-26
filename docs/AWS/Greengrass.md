## Module AWS.Greengrass

AWS Greengrass seamlessly extends AWS onto physical devices so they can act locally on the data they generate, while still using the cloud for management, analytics, and durable storage. AWS Greengrass ensures your devices can respond quickly to local events and operate with intermittent connectivity. AWS Greengrass minimizes the cost of transmitting data to the cloud by allowing you to author AWS Lambda functions that execute locally.

#### `serviceName`

``` purescript
serviceName :: String
```

#### `associateRoleToGroup`

``` purescript
associateRoleToGroup :: forall eff. AssociateRoleToGroupRequest -> Aff (err :: RequestError | eff) AssociateRoleToGroupResponse
```

Associates a role with a group. The role will be used by the AWS Greengrass core in order to access AWS cloud services. The role's permissions will allow Greengrass core Lambda functions to perform actions against the cloud.

#### `associateServiceRoleToAccount`

``` purescript
associateServiceRoleToAccount :: forall eff. AssociateServiceRoleToAccountRequest -> Aff (err :: RequestError | eff) AssociateServiceRoleToAccountResponse
```

Associates a role which is used by AWS Greengrass. AWS Greengrass uses the role to access your Lambda functions and AWS IoT resources. This is necessary for deployments to succeed. It needs to have minimum permissions in policy ``AWSGreengrassResourceAccessRolePolicy``

#### `createCoreDefinition`

``` purescript
createCoreDefinition :: forall eff. CreateCoreDefinitionRequest -> Aff (err :: RequestError | eff) CreateCoreDefinitionResponse
```

Creates a core definition. You may optionally provide the initial version of the core definition or use ''CreateCoreDefinitionVersion'' at a later time. AWS Greengrass Groups must each contain exactly 1 AWS Greengrass Core.

#### `createCoreDefinitionVersion`

``` purescript
createCoreDefinitionVersion :: forall eff. CreateCoreDefinitionVersionRequest -> Aff (err :: RequestError | eff) CreateCoreDefinitionVersionResponse
```

Creates a version of a core definition that has already been defined. AWS Greengrass Groups must each contain exactly 1 AWS Greengrass Core.

#### `createDeployment`

``` purescript
createDeployment :: forall eff. CreateDeploymentRequest -> Aff (err :: RequestError | eff) CreateDeploymentResponse
```

Creates a deployment.

#### `createDeviceDefinition`

``` purescript
createDeviceDefinition :: forall eff. CreateDeviceDefinitionRequest -> Aff (err :: RequestError | eff) CreateDeviceDefinitionResponse
```

Creates a device definition. You may optinally provide the initial version of the device definition or use ``CreateDeviceDefinitionVersion`` at a later time.

#### `createDeviceDefinitionVersion`

``` purescript
createDeviceDefinitionVersion :: forall eff. CreateDeviceDefinitionVersionRequest -> Aff (err :: RequestError | eff) CreateDeviceDefinitionVersionResponse
```

Creates a version of a device definition that has already been defined.

#### `createFunctionDefinition`

``` purescript
createFunctionDefinition :: forall eff. CreateFunctionDefinitionRequest -> Aff (err :: RequestError | eff) CreateFunctionDefinitionResponse
```

Creates a Lambda function definition which contains a list of Lambda functions and their configurations to be used in a group. You can create an initial version of the definition by providing a list of Lambda functions and their configurations now, or use ``CreateFunctionDefinitionVersion`` later.

#### `createFunctionDefinitionVersion`

``` purescript
createFunctionDefinitionVersion :: forall eff. CreateFunctionDefinitionVersionRequest -> Aff (err :: RequestError | eff) CreateFunctionDefinitionVersionResponse
```

Create a version of a Lambda function definition that has already been defined.

#### `createGroup`

``` purescript
createGroup :: forall eff. CreateGroupRequest -> Aff (err :: RequestError | eff) CreateGroupResponse
```

Creates a group. You may optionally provide the initial version of the group or use ''CreateGroupVersion'' at a later time.

#### `createGroupCertificateAuthority`

``` purescript
createGroupCertificateAuthority :: forall eff. CreateGroupCertificateAuthorityRequest -> Aff (err :: RequestError | eff) CreateGroupCertificateAuthorityResponse
```

Creates a CA for the group. If a CA already exists, it will rotate the existing CA.

#### `createGroupVersion`

``` purescript
createGroupVersion :: forall eff. CreateGroupVersionRequest -> Aff (err :: RequestError | eff) CreateGroupVersionResponse
```

Creates a version of a group which has already been defined.

#### `createLoggerDefinition`

``` purescript
createLoggerDefinition :: forall eff. CreateLoggerDefinitionRequest -> Aff (err :: RequestError | eff) CreateLoggerDefinitionResponse
```

Creates a logger definition. You may optionally provide the initial version of the logger definition or use ``CreateLoggerDefinitionVersion`` at a later time.

#### `createLoggerDefinitionVersion`

``` purescript
createLoggerDefinitionVersion :: forall eff. CreateLoggerDefinitionVersionRequest -> Aff (err :: RequestError | eff) CreateLoggerDefinitionVersionResponse
```

Creates a version of a logger definition that has already been defined.

#### `createResourceDefinition`

``` purescript
createResourceDefinition :: forall eff. CreateResourceDefinitionRequest -> Aff (err :: RequestError | eff) CreateResourceDefinitionResponse
```

Creates a resource definition which contains a list of resources to be used in a group. You can create an initial version of the definition by providing a list of resources now, or use ``CreateResourceDefinitionVersion`` later.

#### `createResourceDefinitionVersion`

``` purescript
createResourceDefinitionVersion :: forall eff. CreateResourceDefinitionVersionRequest -> Aff (err :: RequestError | eff) CreateResourceDefinitionVersionResponse
```

Create a version of a resource definition that has already been defined.

#### `createSoftwareUpdateJob`

``` purescript
createSoftwareUpdateJob :: forall eff. CreateSoftwareUpdateJobRequest -> Aff (err :: RequestError | eff) CreateSoftwareUpdateJobResponse
```

Creates an Iot Job that will trigger your Greengrass Cores to update the software they are running.

#### `createSubscriptionDefinition`

``` purescript
createSubscriptionDefinition :: forall eff. CreateSubscriptionDefinitionRequest -> Aff (err :: RequestError | eff) CreateSubscriptionDefinitionResponse
```

Creates a subscription definition. You may optionally provide the initial version of the subscription definition or use ``CreateSubscriptionDefinitionVersion`` at a later time.

#### `createSubscriptionDefinitionVersion`

``` purescript
createSubscriptionDefinitionVersion :: forall eff. CreateSubscriptionDefinitionVersionRequest -> Aff (err :: RequestError | eff) CreateSubscriptionDefinitionVersionResponse
```

Creates a version of a subscription definition which has already been defined.

#### `deleteCoreDefinition`

``` purescript
deleteCoreDefinition :: forall eff. DeleteCoreDefinitionRequest -> Aff (err :: RequestError | eff) DeleteCoreDefinitionResponse
```

Deletes a core definition. The core definition must not have been used in a deployment.

#### `deleteDeviceDefinition`

``` purescript
deleteDeviceDefinition :: forall eff. DeleteDeviceDefinitionRequest -> Aff (err :: RequestError | eff) DeleteDeviceDefinitionResponse
```

Deletes a device definition. The device definition must not have been used in a deployment.

#### `deleteFunctionDefinition`

``` purescript
deleteFunctionDefinition :: forall eff. DeleteFunctionDefinitionRequest -> Aff (err :: RequestError | eff) DeleteFunctionDefinitionResponse
```

Deletes a Lambda function definition. The Lambda function definition must not have been used in a deployment.

#### `deleteGroup`

``` purescript
deleteGroup :: forall eff. DeleteGroupRequest -> Aff (err :: RequestError | eff) DeleteGroupResponse
```

Deletes a group. The group must not have been used in deployment.

#### `deleteLoggerDefinition`

``` purescript
deleteLoggerDefinition :: forall eff. DeleteLoggerDefinitionRequest -> Aff (err :: RequestError | eff) DeleteLoggerDefinitionResponse
```

Deletes a logger definition. The logger definition must not have been used in a deployment.

#### `deleteResourceDefinition`

``` purescript
deleteResourceDefinition :: forall eff. DeleteResourceDefinitionRequest -> Aff (err :: RequestError | eff) DeleteResourceDefinitionResponse
```

Deletes a resource definition.

#### `deleteSubscriptionDefinition`

``` purescript
deleteSubscriptionDefinition :: forall eff. DeleteSubscriptionDefinitionRequest -> Aff (err :: RequestError | eff) DeleteSubscriptionDefinitionResponse
```

Deletes a subscription definition. The subscription definition must not have been used in a deployment.

#### `disassociateRoleFromGroup`

``` purescript
disassociateRoleFromGroup :: forall eff. DisassociateRoleFromGroupRequest -> Aff (err :: RequestError | eff) DisassociateRoleFromGroupResponse
```

Disassociates the role from a group.

#### `disassociateServiceRoleFromAccount`

``` purescript
disassociateServiceRoleFromAccount :: forall eff. DisassociateServiceRoleFromAccountRequest -> Aff (err :: RequestError | eff) DisassociateServiceRoleFromAccountResponse
```

Disassociates the service role from the account. Without a service role, deployments will not work.

#### `getAssociatedRole`

``` purescript
getAssociatedRole :: forall eff. GetAssociatedRoleRequest -> Aff (err :: RequestError | eff) GetAssociatedRoleResponse
```

Retrieves the role associated with a particular group.

#### `getConnectivityInfo`

``` purescript
getConnectivityInfo :: forall eff. GetConnectivityInfoRequest -> Aff (err :: RequestError | eff) GetConnectivityInfoResponse
```

Retrieves the connectivity information for a core.

#### `getCoreDefinition`

``` purescript
getCoreDefinition :: forall eff. GetCoreDefinitionRequest -> Aff (err :: RequestError | eff) GetCoreDefinitionResponse
```

Retrieves information about a core definition version.

#### `getCoreDefinitionVersion`

``` purescript
getCoreDefinitionVersion :: forall eff. GetCoreDefinitionVersionRequest -> Aff (err :: RequestError | eff) GetCoreDefinitionVersionResponse
```

Retrieves information about a core definition version.

#### `getDeploymentStatus`

``` purescript
getDeploymentStatus :: forall eff. GetDeploymentStatusRequest -> Aff (err :: RequestError | eff) GetDeploymentStatusResponse
```

Returns the status of a deployment.

#### `getDeviceDefinition`

``` purescript
getDeviceDefinition :: forall eff. GetDeviceDefinitionRequest -> Aff (err :: RequestError | eff) GetDeviceDefinitionResponse
```

Retrieves information about a device definition.

#### `getDeviceDefinitionVersion`

``` purescript
getDeviceDefinitionVersion :: forall eff. GetDeviceDefinitionVersionRequest -> Aff (err :: RequestError | eff) GetDeviceDefinitionVersionResponse
```

Retrieves information about a device definition version.

#### `getFunctionDefinition`

``` purescript
getFunctionDefinition :: forall eff. GetFunctionDefinitionRequest -> Aff (err :: RequestError | eff) GetFunctionDefinitionResponse
```

Retrieves information about a Lambda function definition, such as its creation time and latest version.

#### `getFunctionDefinitionVersion`

``` purescript
getFunctionDefinitionVersion :: forall eff. GetFunctionDefinitionVersionRequest -> Aff (err :: RequestError | eff) GetFunctionDefinitionVersionResponse
```

Retrieves information about a Lambda function definition version, such as which Lambda functions are included in the version and their configurations.

#### `getGroup`

``` purescript
getGroup :: forall eff. GetGroupRequest -> Aff (err :: RequestError | eff) GetGroupResponse
```

Retrieves information about a group.

#### `getGroupCertificateAuthority`

``` purescript
getGroupCertificateAuthority :: forall eff. GetGroupCertificateAuthorityRequest -> Aff (err :: RequestError | eff) GetGroupCertificateAuthorityResponse
```

Retreives the CA associated with a group. Returns the public key of the CA.

#### `getGroupCertificateConfiguration`

``` purescript
getGroupCertificateConfiguration :: forall eff. GetGroupCertificateConfigurationRequest -> Aff (err :: RequestError | eff) GetGroupCertificateConfigurationResponse
```

Retrieves the current configuration for the CA used by the group.

#### `getGroupVersion`

``` purescript
getGroupVersion :: forall eff. GetGroupVersionRequest -> Aff (err :: RequestError | eff) GetGroupVersionResponse
```

Retrieves information about a group version.

#### `getLoggerDefinition`

``` purescript
getLoggerDefinition :: forall eff. GetLoggerDefinitionRequest -> Aff (err :: RequestError | eff) GetLoggerDefinitionResponse
```

Retrieves information about a logger definition.

#### `getLoggerDefinitionVersion`

``` purescript
getLoggerDefinitionVersion :: forall eff. GetLoggerDefinitionVersionRequest -> Aff (err :: RequestError | eff) GetLoggerDefinitionVersionResponse
```

Retrieves information about a logger definition version.

#### `getResourceDefinition`

``` purescript
getResourceDefinition :: forall eff. GetResourceDefinitionRequest -> Aff (err :: RequestError | eff) GetResourceDefinitionResponse
```

Retrieves information about a resource definition, such as its creation time and latest version.

#### `getResourceDefinitionVersion`

``` purescript
getResourceDefinitionVersion :: forall eff. GetResourceDefinitionVersionRequest -> Aff (err :: RequestError | eff) GetResourceDefinitionVersionResponse
```

Retrieves information about a resource definition version, such as which resources are included in the version.

#### `getServiceRoleForAccount`

``` purescript
getServiceRoleForAccount :: forall eff. GetServiceRoleForAccountRequest -> Aff (err :: RequestError | eff) GetServiceRoleForAccountResponse
```

Retrieves the service role that is attached to the account.

#### `getSubscriptionDefinition`

``` purescript
getSubscriptionDefinition :: forall eff. GetSubscriptionDefinitionRequest -> Aff (err :: RequestError | eff) GetSubscriptionDefinitionResponse
```

Retrieves information about a subscription definition.

#### `getSubscriptionDefinitionVersion`

``` purescript
getSubscriptionDefinitionVersion :: forall eff. GetSubscriptionDefinitionVersionRequest -> Aff (err :: RequestError | eff) GetSubscriptionDefinitionVersionResponse
```

Retrieves information about a subscription definition version.

#### `listCoreDefinitionVersions`

``` purescript
listCoreDefinitionVersions :: forall eff. ListCoreDefinitionVersionsRequest -> Aff (err :: RequestError | eff) ListCoreDefinitionVersionsResponse
```

Lists versions of a core definition.

#### `listCoreDefinitions`

``` purescript
listCoreDefinitions :: forall eff. ListCoreDefinitionsRequest -> Aff (err :: RequestError | eff) ListCoreDefinitionsResponse
```

Retrieves a list of core definitions.

#### `listDeployments`

``` purescript
listDeployments :: forall eff. ListDeploymentsRequest -> Aff (err :: RequestError | eff) ListDeploymentsResponse
```

Returns a history of deployments for the group.

#### `listDeviceDefinitionVersions`

``` purescript
listDeviceDefinitionVersions :: forall eff. ListDeviceDefinitionVersionsRequest -> Aff (err :: RequestError | eff) ListDeviceDefinitionVersionsResponse
```

Lists the versions of a device definition.

#### `listDeviceDefinitions`

``` purescript
listDeviceDefinitions :: forall eff. ListDeviceDefinitionsRequest -> Aff (err :: RequestError | eff) ListDeviceDefinitionsResponse
```

Retrieves a list of device definitions.

#### `listFunctionDefinitionVersions`

``` purescript
listFunctionDefinitionVersions :: forall eff. ListFunctionDefinitionVersionsRequest -> Aff (err :: RequestError | eff) ListFunctionDefinitionVersionsResponse
```

Lists the versions of a Lambda function definition.

#### `listFunctionDefinitions`

``` purescript
listFunctionDefinitions :: forall eff. ListFunctionDefinitionsRequest -> Aff (err :: RequestError | eff) ListFunctionDefinitionsResponse
```

Retrieves a list of Lambda function definitions.

#### `listGroupCertificateAuthorities`

``` purescript
listGroupCertificateAuthorities :: forall eff. ListGroupCertificateAuthoritiesRequest -> Aff (err :: RequestError | eff) ListGroupCertificateAuthoritiesResponse
```

Retrieves the current CAs for a group.

#### `listGroupVersions`

``` purescript
listGroupVersions :: forall eff. ListGroupVersionsRequest -> Aff (err :: RequestError | eff) ListGroupVersionsResponse
```

List the versions of a group.

#### `listGroups`

``` purescript
listGroups :: forall eff. ListGroupsRequest -> Aff (err :: RequestError | eff) ListGroupsResponse
```

Retrieves a list of groups.

#### `listLoggerDefinitionVersions`

``` purescript
listLoggerDefinitionVersions :: forall eff. ListLoggerDefinitionVersionsRequest -> Aff (err :: RequestError | eff) ListLoggerDefinitionVersionsResponse
```

Lists the versions of a logger definition.

#### `listLoggerDefinitions`

``` purescript
listLoggerDefinitions :: forall eff. ListLoggerDefinitionsRequest -> Aff (err :: RequestError | eff) ListLoggerDefinitionsResponse
```

Retrieves a list of logger definitions.

#### `listResourceDefinitionVersions`

``` purescript
listResourceDefinitionVersions :: forall eff. ListResourceDefinitionVersionsRequest -> Aff (err :: RequestError | eff) ListResourceDefinitionVersionsResponse
```

Lists the versions of a resource definition.

#### `listResourceDefinitions`

``` purescript
listResourceDefinitions :: forall eff. ListResourceDefinitionsRequest -> Aff (err :: RequestError | eff) ListResourceDefinitionsResponse
```

Retrieves a list of resource definitions.

#### `listSubscriptionDefinitionVersions`

``` purescript
listSubscriptionDefinitionVersions :: forall eff. ListSubscriptionDefinitionVersionsRequest -> Aff (err :: RequestError | eff) ListSubscriptionDefinitionVersionsResponse
```

Lists the versions of a subscription definition.

#### `listSubscriptionDefinitions`

``` purescript
listSubscriptionDefinitions :: forall eff. ListSubscriptionDefinitionsRequest -> Aff (err :: RequestError | eff) ListSubscriptionDefinitionsResponse
```

Retrieves a list of subscription definitions.

#### `resetDeployments`

``` purescript
resetDeployments :: forall eff. ResetDeploymentsRequest -> Aff (err :: RequestError | eff) ResetDeploymentsResponse
```

Resets a group's deployments.

#### `updateConnectivityInfo`

``` purescript
updateConnectivityInfo :: forall eff. UpdateConnectivityInfoRequest -> Aff (err :: RequestError | eff) UpdateConnectivityInfoResponse
```

Updates the connectivity information for the core. Any devices that belong to the group which has this core will receive this information in order to find the location of the core and connect to it.

#### `updateCoreDefinition`

``` purescript
updateCoreDefinition :: forall eff. UpdateCoreDefinitionRequest -> Aff (err :: RequestError | eff) UpdateCoreDefinitionResponse
```

Updates a core definition.

#### `updateDeviceDefinition`

``` purescript
updateDeviceDefinition :: forall eff. UpdateDeviceDefinitionRequest -> Aff (err :: RequestError | eff) UpdateDeviceDefinitionResponse
```

Updates a device definition.

#### `updateFunctionDefinition`

``` purescript
updateFunctionDefinition :: forall eff. UpdateFunctionDefinitionRequest -> Aff (err :: RequestError | eff) UpdateFunctionDefinitionResponse
```

Updates a Lambda function definition.

#### `updateGroup`

``` purescript
updateGroup :: forall eff. UpdateGroupRequest -> Aff (err :: RequestError | eff) UpdateGroupResponse
```

Updates a group.

#### `updateGroupCertificateConfiguration`

``` purescript
updateGroupCertificateConfiguration :: forall eff. UpdateGroupCertificateConfigurationRequest -> Aff (err :: RequestError | eff) UpdateGroupCertificateConfigurationResponse
```

Updates the Cert expiry time for a group.

#### `updateLoggerDefinition`

``` purescript
updateLoggerDefinition :: forall eff. UpdateLoggerDefinitionRequest -> Aff (err :: RequestError | eff) UpdateLoggerDefinitionResponse
```

Updates a logger definition.

#### `updateResourceDefinition`

``` purescript
updateResourceDefinition :: forall eff. UpdateResourceDefinitionRequest -> Aff (err :: RequestError | eff) UpdateResourceDefinitionResponse
```

Updates a resource definition.

#### `updateSubscriptionDefinition`

``` purescript
updateSubscriptionDefinition :: forall eff. UpdateSubscriptionDefinitionRequest -> Aff (err :: RequestError | eff) UpdateSubscriptionDefinitionResponse
```

Updates a subscription definition.

#### `AssociateRoleToGroupRequest`

``` purescript
newtype AssociateRoleToGroupRequest
  = AssociateRoleToGroupRequest { "GroupId" :: String, "RoleArn" :: NullOrUndefined (String) }
```

#### `AssociateRoleToGroupResponse`

``` purescript
newtype AssociateRoleToGroupResponse
  = AssociateRoleToGroupResponse { "AssociatedAt" :: NullOrUndefined (String) }
```

#### `AssociateServiceRoleToAccountRequest`

``` purescript
newtype AssociateServiceRoleToAccountRequest
  = AssociateServiceRoleToAccountRequest { "RoleArn" :: NullOrUndefined (String) }
```

#### `AssociateServiceRoleToAccountResponse`

``` purescript
newtype AssociateServiceRoleToAccountResponse
  = AssociateServiceRoleToAccountResponse { "AssociatedAt" :: NullOrUndefined (String) }
```

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "ErrorDetails" :: NullOrUndefined (ErrorDetails), "Message" :: NullOrUndefined (String) }
```

General Error

#### `ConnectivityInfo`

``` purescript
newtype ConnectivityInfo
  = ConnectivityInfo { "HostAddress" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Metadata" :: NullOrUndefined (String), "PortNumber" :: NullOrUndefined (Int) }
```

Connectivity Info

#### `Core`

``` purescript
newtype Core
  = Core { "CertificateArn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "SyncShadow" :: NullOrUndefined (Boolean), "ThingArn" :: NullOrUndefined (String) }
```

Information on the core

#### `CoreDefinitionVersion`

``` purescript
newtype CoreDefinitionVersion
  = CoreDefinitionVersion { "Cores" :: NullOrUndefined (ListOfCore) }
```

Information on core definition version

#### `CreateCoreDefinitionRequest`

``` purescript
newtype CreateCoreDefinitionRequest
  = CreateCoreDefinitionRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (CoreDefinitionVersion), "Name" :: NullOrUndefined (String) }
```

Information on the core definition request

#### `CreateCoreDefinitionResponse`

``` purescript
newtype CreateCoreDefinitionResponse
  = CreateCoreDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `CreateCoreDefinitionVersionRequest`

``` purescript
newtype CreateCoreDefinitionVersionRequest
  = CreateCoreDefinitionVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "CoreDefinitionId" :: String, "Cores" :: NullOrUndefined (ListOfCore) }
```

#### `CreateCoreDefinitionVersionResponse`

``` purescript
newtype CreateCoreDefinitionVersionResponse
  = CreateCoreDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `CreateDeploymentRequest`

``` purescript
newtype CreateDeploymentRequest
  = CreateDeploymentRequest { "AmznClientToken" :: NullOrUndefined (String), "DeploymentId" :: NullOrUndefined (String), "DeploymentType" :: NullOrUndefined (DeploymentType), "GroupId" :: String, "GroupVersionId" :: NullOrUndefined (String) }
```

#### `CreateDeploymentResponse`

``` purescript
newtype CreateDeploymentResponse
  = CreateDeploymentResponse { "DeploymentArn" :: NullOrUndefined (String), "DeploymentId" :: NullOrUndefined (String) }
```

#### `CreateDeviceDefinitionRequest`

``` purescript
newtype CreateDeviceDefinitionRequest
  = CreateDeviceDefinitionRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (DeviceDefinitionVersion), "Name" :: NullOrUndefined (String) }
```

#### `CreateDeviceDefinitionResponse`

``` purescript
newtype CreateDeviceDefinitionResponse
  = CreateDeviceDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `CreateDeviceDefinitionVersionRequest`

``` purescript
newtype CreateDeviceDefinitionVersionRequest
  = CreateDeviceDefinitionVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "DeviceDefinitionId" :: String, "Devices" :: NullOrUndefined (ListOfDevice) }
```

#### `CreateDeviceDefinitionVersionResponse`

``` purescript
newtype CreateDeviceDefinitionVersionResponse
  = CreateDeviceDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `CreateFunctionDefinitionRequest`

``` purescript
newtype CreateFunctionDefinitionRequest
  = CreateFunctionDefinitionRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (FunctionDefinitionVersion), "Name" :: NullOrUndefined (String) }
```

#### `CreateFunctionDefinitionResponse`

``` purescript
newtype CreateFunctionDefinitionResponse
  = CreateFunctionDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `CreateFunctionDefinitionVersionRequest`

``` purescript
newtype CreateFunctionDefinitionVersionRequest
  = CreateFunctionDefinitionVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "FunctionDefinitionId" :: String, "Functions" :: NullOrUndefined (ListOfFunction) }
```

Function definition version

#### `CreateFunctionDefinitionVersionResponse`

``` purescript
newtype CreateFunctionDefinitionVersionResponse
  = CreateFunctionDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `CreateGroupCertificateAuthorityRequest`

``` purescript
newtype CreateGroupCertificateAuthorityRequest
  = CreateGroupCertificateAuthorityRequest { "AmznClientToken" :: NullOrUndefined (String), "GroupId" :: String }
```

#### `CreateGroupCertificateAuthorityResponse`

``` purescript
newtype CreateGroupCertificateAuthorityResponse
  = CreateGroupCertificateAuthorityResponse { "GroupCertificateAuthorityArn" :: NullOrUndefined (String) }
```

#### `CreateGroupRequest`

``` purescript
newtype CreateGroupRequest
  = CreateGroupRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (GroupVersion), "Name" :: NullOrUndefined (String) }
```

#### `CreateGroupResponse`

``` purescript
newtype CreateGroupResponse
  = CreateGroupResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `CreateGroupVersionRequest`

``` purescript
newtype CreateGroupVersionRequest
  = CreateGroupVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "CoreDefinitionVersionArn" :: NullOrUndefined (String), "DeviceDefinitionVersionArn" :: NullOrUndefined (String), "FunctionDefinitionVersionArn" :: NullOrUndefined (String), "GroupId" :: String, "LoggerDefinitionVersionArn" :: NullOrUndefined (String), "ResourceDefinitionVersionArn" :: NullOrUndefined (String), "SubscriptionDefinitionVersionArn" :: NullOrUndefined (String) }
```

#### `CreateGroupVersionResponse`

``` purescript
newtype CreateGroupVersionResponse
  = CreateGroupVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `CreateLoggerDefinitionRequest`

``` purescript
newtype CreateLoggerDefinitionRequest
  = CreateLoggerDefinitionRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (LoggerDefinitionVersion), "Name" :: NullOrUndefined (String) }
```

#### `CreateLoggerDefinitionResponse`

``` purescript
newtype CreateLoggerDefinitionResponse
  = CreateLoggerDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `CreateLoggerDefinitionVersionRequest`

``` purescript
newtype CreateLoggerDefinitionVersionRequest
  = CreateLoggerDefinitionVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "LoggerDefinitionId" :: String, "Loggers" :: NullOrUndefined (ListOfLogger) }
```

#### `CreateLoggerDefinitionVersionResponse`

``` purescript
newtype CreateLoggerDefinitionVersionResponse
  = CreateLoggerDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `CreateResourceDefinitionRequest`

``` purescript
newtype CreateResourceDefinitionRequest
  = CreateResourceDefinitionRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (ResourceDefinitionVersion), "Name" :: NullOrUndefined (String) }
```

#### `CreateResourceDefinitionResponse`

``` purescript
newtype CreateResourceDefinitionResponse
  = CreateResourceDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `CreateResourceDefinitionVersionRequest`

``` purescript
newtype CreateResourceDefinitionVersionRequest
  = CreateResourceDefinitionVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "ResourceDefinitionId" :: String, "Resources" :: NullOrUndefined (ListOfResource) }
```

#### `CreateResourceDefinitionVersionResponse`

``` purescript
newtype CreateResourceDefinitionVersionResponse
  = CreateResourceDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `CreateSoftwareUpdateJobRequest`

``` purescript
newtype CreateSoftwareUpdateJobRequest
  = CreateSoftwareUpdateJobRequest { "AmznClientToken" :: NullOrUndefined (String), "S3UrlSignerRole" :: NullOrUndefined (S3UrlSignerRole), "SoftwareToUpdate" :: NullOrUndefined (SoftwareToUpdate), "UpdateAgentLogLevel" :: NullOrUndefined (UpdateAgentLogLevel), "UpdateTargets" :: NullOrUndefined (UpdateTargets), "UpdateTargetsArchitecture" :: NullOrUndefined (UpdateTargetsArchitecture), "UpdateTargetsOperatingSystem" :: NullOrUndefined (UpdateTargetsOperatingSystem) }
```

#### `CreateSoftwareUpdateJobResponse`

``` purescript
newtype CreateSoftwareUpdateJobResponse
  = CreateSoftwareUpdateJobResponse { "IotJobArn" :: NullOrUndefined (String), "IotJobId" :: NullOrUndefined (String) }
```

#### `CreateSubscriptionDefinitionRequest`

``` purescript
newtype CreateSubscriptionDefinitionRequest
  = CreateSubscriptionDefinitionRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (SubscriptionDefinitionVersion), "Name" :: NullOrUndefined (String) }
```

#### `CreateSubscriptionDefinitionResponse`

``` purescript
newtype CreateSubscriptionDefinitionResponse
  = CreateSubscriptionDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `CreateSubscriptionDefinitionVersionRequest`

``` purescript
newtype CreateSubscriptionDefinitionVersionRequest
  = CreateSubscriptionDefinitionVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "SubscriptionDefinitionId" :: String, "Subscriptions" :: NullOrUndefined (ListOfSubscription) }
```

#### `CreateSubscriptionDefinitionVersionResponse`

``` purescript
newtype CreateSubscriptionDefinitionVersionResponse
  = CreateSubscriptionDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `DefinitionInformation`

``` purescript
newtype DefinitionInformation
  = DefinitionInformation { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

Information on the Definition

#### `DeleteCoreDefinitionRequest`

``` purescript
newtype DeleteCoreDefinitionRequest
  = DeleteCoreDefinitionRequest { "CoreDefinitionId" :: String }
```

#### `DeleteCoreDefinitionResponse`

``` purescript
newtype DeleteCoreDefinitionResponse
  = DeleteCoreDefinitionResponse {  }
```

#### `DeleteDeviceDefinitionRequest`

``` purescript
newtype DeleteDeviceDefinitionRequest
  = DeleteDeviceDefinitionRequest { "DeviceDefinitionId" :: String }
```

#### `DeleteDeviceDefinitionResponse`

``` purescript
newtype DeleteDeviceDefinitionResponse
  = DeleteDeviceDefinitionResponse {  }
```

#### `DeleteFunctionDefinitionRequest`

``` purescript
newtype DeleteFunctionDefinitionRequest
  = DeleteFunctionDefinitionRequest { "FunctionDefinitionId" :: String }
```

#### `DeleteFunctionDefinitionResponse`

``` purescript
newtype DeleteFunctionDefinitionResponse
  = DeleteFunctionDefinitionResponse {  }
```

#### `DeleteGroupRequest`

``` purescript
newtype DeleteGroupRequest
  = DeleteGroupRequest { "GroupId" :: String }
```

#### `DeleteGroupResponse`

``` purescript
newtype DeleteGroupResponse
  = DeleteGroupResponse {  }
```

#### `DeleteLoggerDefinitionRequest`

``` purescript
newtype DeleteLoggerDefinitionRequest
  = DeleteLoggerDefinitionRequest { "LoggerDefinitionId" :: String }
```

#### `DeleteLoggerDefinitionResponse`

``` purescript
newtype DeleteLoggerDefinitionResponse
  = DeleteLoggerDefinitionResponse {  }
```

#### `DeleteResourceDefinitionRequest`

``` purescript
newtype DeleteResourceDefinitionRequest
  = DeleteResourceDefinitionRequest { "ResourceDefinitionId" :: String }
```

#### `DeleteResourceDefinitionResponse`

``` purescript
newtype DeleteResourceDefinitionResponse
  = DeleteResourceDefinitionResponse {  }
```

#### `DeleteSubscriptionDefinitionRequest`

``` purescript
newtype DeleteSubscriptionDefinitionRequest
  = DeleteSubscriptionDefinitionRequest { "SubscriptionDefinitionId" :: String }
```

#### `DeleteSubscriptionDefinitionResponse`

``` purescript
newtype DeleteSubscriptionDefinitionResponse
  = DeleteSubscriptionDefinitionResponse {  }
```

#### `Deployment`

``` purescript
newtype Deployment
  = Deployment { "CreatedAt" :: NullOrUndefined (String), "DeploymentArn" :: NullOrUndefined (String), "DeploymentId" :: NullOrUndefined (String), "DeploymentType" :: NullOrUndefined (DeploymentType), "GroupArn" :: NullOrUndefined (String) }
```

Information on the deployment

#### `DeploymentType`

``` purescript
newtype DeploymentType
  = DeploymentType String
```

#### `Deployments`

``` purescript
newtype Deployments
  = Deployments (Array Deployment)
```

#### `Device`

``` purescript
newtype Device
  = Device { "CertificateArn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "SyncShadow" :: NullOrUndefined (Boolean), "ThingArn" :: NullOrUndefined (String) }
```

Information on a Device

#### `DeviceDefinitionVersion`

``` purescript
newtype DeviceDefinitionVersion
  = DeviceDefinitionVersion { "Devices" :: NullOrUndefined (ListOfDevice) }
```

Information on device definition version

#### `DisassociateRoleFromGroupRequest`

``` purescript
newtype DisassociateRoleFromGroupRequest
  = DisassociateRoleFromGroupRequest { "GroupId" :: String }
```

#### `DisassociateRoleFromGroupResponse`

``` purescript
newtype DisassociateRoleFromGroupResponse
  = DisassociateRoleFromGroupResponse { "DisassociatedAt" :: NullOrUndefined (String) }
```

#### `DisassociateServiceRoleFromAccountRequest`

``` purescript
newtype DisassociateServiceRoleFromAccountRequest
  = DisassociateServiceRoleFromAccountRequest {  }
```

#### `DisassociateServiceRoleFromAccountResponse`

``` purescript
newtype DisassociateServiceRoleFromAccountResponse
  = DisassociateServiceRoleFromAccountResponse { "DisassociatedAt" :: NullOrUndefined (String) }
```

#### `Empty`

``` purescript
newtype Empty
  = Empty {  }
```

Empty

#### `ErrorDetail`

``` purescript
newtype ErrorDetail
  = ErrorDetail { "DetailedErrorCode" :: NullOrUndefined (String), "DetailedErrorMessage" :: NullOrUndefined (String) }
```

ErrorDetail

#### `ErrorDetails`

``` purescript
newtype ErrorDetails
  = ErrorDetails (Array ErrorDetail)
```

Error Details

#### `Function''`

``` purescript
newtype Function''
  = Function'' { "FunctionArn" :: NullOrUndefined (String), "FunctionConfiguration" :: NullOrUndefined (FunctionConfiguration), "Id" :: NullOrUndefined (String) }
```

Information on function

#### `FunctionConfiguration`

``` purescript
newtype FunctionConfiguration
  = FunctionConfiguration { "Environment" :: NullOrUndefined (FunctionConfigurationEnvironment), "ExecArgs" :: NullOrUndefined (String), "Executable" :: NullOrUndefined (String), "MemorySize" :: NullOrUndefined (Int), "Pinned" :: NullOrUndefined (Boolean), "Timeout" :: NullOrUndefined (Int) }
```

Configuration of the function

#### `FunctionConfigurationEnvironment`

``` purescript
newtype FunctionConfigurationEnvironment
  = FunctionConfigurationEnvironment { "AccessSysfs" :: NullOrUndefined (Boolean), "ResourceAccessPolicies" :: NullOrUndefined (ListOfResourceAccessPolicy), "Variables" :: NullOrUndefined (MapOf__string) }
```

Environment of the function configuration

#### `FunctionDefinitionVersion`

``` purescript
newtype FunctionDefinitionVersion
  = FunctionDefinitionVersion { "Functions" :: NullOrUndefined (ListOfFunction) }
```

Information on the function definition version

#### `GeneralError`

``` purescript
newtype GeneralError
  = GeneralError { "ErrorDetails" :: NullOrUndefined (ErrorDetails), "Message" :: NullOrUndefined (String) }
```

General Error

#### `GetAssociatedRoleRequest`

``` purescript
newtype GetAssociatedRoleRequest
  = GetAssociatedRoleRequest { "GroupId" :: String }
```

#### `GetAssociatedRoleResponse`

``` purescript
newtype GetAssociatedRoleResponse
  = GetAssociatedRoleResponse { "AssociatedAt" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

#### `GetConnectivityInfoRequest`

``` purescript
newtype GetConnectivityInfoRequest
  = GetConnectivityInfoRequest { "ThingName" :: String }
```

#### `GetConnectivityInfoResponse`

``` purescript
newtype GetConnectivityInfoResponse
  = GetConnectivityInfoResponse { "ConnectivityInfo" :: NullOrUndefined (ListOfConnectivityInfo), "Message" :: NullOrUndefined (String) }
```

#### `GetCoreDefinitionRequest`

``` purescript
newtype GetCoreDefinitionRequest
  = GetCoreDefinitionRequest { "CoreDefinitionId" :: String }
```

#### `GetCoreDefinitionResponse`

``` purescript
newtype GetCoreDefinitionResponse
  = GetCoreDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `GetCoreDefinitionVersionRequest`

``` purescript
newtype GetCoreDefinitionVersionRequest
  = GetCoreDefinitionVersionRequest { "CoreDefinitionId" :: String, "CoreDefinitionVersionId" :: String }
```

#### `GetCoreDefinitionVersionResponse`

``` purescript
newtype GetCoreDefinitionVersionResponse
  = GetCoreDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (CoreDefinitionVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `GetDeploymentStatusRequest`

``` purescript
newtype GetDeploymentStatusRequest
  = GetDeploymentStatusRequest { "DeploymentId" :: String, "GroupId" :: String }
```

#### `GetDeploymentStatusResponse`

``` purescript
newtype GetDeploymentStatusResponse
  = GetDeploymentStatusResponse { "DeploymentStatus" :: NullOrUndefined (String), "DeploymentType" :: NullOrUndefined (DeploymentType), "ErrorDetails" :: NullOrUndefined (ErrorDetails), "ErrorMessage" :: NullOrUndefined (String), "UpdatedAt" :: NullOrUndefined (String) }
```

#### `GetDeviceDefinitionRequest`

``` purescript
newtype GetDeviceDefinitionRequest
  = GetDeviceDefinitionRequest { "DeviceDefinitionId" :: String }
```

#### `GetDeviceDefinitionResponse`

``` purescript
newtype GetDeviceDefinitionResponse
  = GetDeviceDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `GetDeviceDefinitionVersionRequest`

``` purescript
newtype GetDeviceDefinitionVersionRequest
  = GetDeviceDefinitionVersionRequest { "DeviceDefinitionId" :: String, "DeviceDefinitionVersionId" :: String }
```

#### `GetDeviceDefinitionVersionResponse`

``` purescript
newtype GetDeviceDefinitionVersionResponse
  = GetDeviceDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (DeviceDefinitionVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `GetFunctionDefinitionRequest`

``` purescript
newtype GetFunctionDefinitionRequest
  = GetFunctionDefinitionRequest { "FunctionDefinitionId" :: String }
```

#### `GetFunctionDefinitionResponse`

``` purescript
newtype GetFunctionDefinitionResponse
  = GetFunctionDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `GetFunctionDefinitionVersionRequest`

``` purescript
newtype GetFunctionDefinitionVersionRequest
  = GetFunctionDefinitionVersionRequest { "FunctionDefinitionId" :: String, "FunctionDefinitionVersionId" :: String }
```

#### `GetFunctionDefinitionVersionResponse`

``` purescript
newtype GetFunctionDefinitionVersionResponse
  = GetFunctionDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (FunctionDefinitionVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `GetGroupCertificateAuthorityRequest`

``` purescript
newtype GetGroupCertificateAuthorityRequest
  = GetGroupCertificateAuthorityRequest { "CertificateAuthorityId" :: String, "GroupId" :: String }
```

#### `GetGroupCertificateAuthorityResponse`

``` purescript
newtype GetGroupCertificateAuthorityResponse
  = GetGroupCertificateAuthorityResponse { "GroupCertificateAuthorityArn" :: NullOrUndefined (String), "GroupCertificateAuthorityId" :: NullOrUndefined (String), "PemEncodedCertificate" :: NullOrUndefined (String) }
```

#### `GetGroupCertificateConfigurationRequest`

``` purescript
newtype GetGroupCertificateConfigurationRequest
  = GetGroupCertificateConfigurationRequest { "GroupId" :: String }
```

#### `GetGroupCertificateConfigurationResponse`

``` purescript
newtype GetGroupCertificateConfigurationResponse
  = GetGroupCertificateConfigurationResponse { "CertificateAuthorityExpiryInMilliseconds" :: NullOrUndefined (String), "CertificateExpiryInMilliseconds" :: NullOrUndefined (String), "GroupId" :: NullOrUndefined (String) }
```

#### `GetGroupRequest`

``` purescript
newtype GetGroupRequest
  = GetGroupRequest { "GroupId" :: String }
```

#### `GetGroupResponse`

``` purescript
newtype GetGroupResponse
  = GetGroupResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `GetGroupVersionRequest`

``` purescript
newtype GetGroupVersionRequest
  = GetGroupVersionRequest { "GroupId" :: String, "GroupVersionId" :: String }
```

#### `GetGroupVersionResponse`

``` purescript
newtype GetGroupVersionResponse
  = GetGroupVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (GroupVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `GetLoggerDefinitionRequest`

``` purescript
newtype GetLoggerDefinitionRequest
  = GetLoggerDefinitionRequest { "LoggerDefinitionId" :: String }
```

#### `GetLoggerDefinitionResponse`

``` purescript
newtype GetLoggerDefinitionResponse
  = GetLoggerDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `GetLoggerDefinitionVersionRequest`

``` purescript
newtype GetLoggerDefinitionVersionRequest
  = GetLoggerDefinitionVersionRequest { "LoggerDefinitionId" :: String, "LoggerDefinitionVersionId" :: String }
```

#### `GetLoggerDefinitionVersionResponse`

``` purescript
newtype GetLoggerDefinitionVersionResponse
  = GetLoggerDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (LoggerDefinitionVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `GetResourceDefinitionRequest`

``` purescript
newtype GetResourceDefinitionRequest
  = GetResourceDefinitionRequest { "ResourceDefinitionId" :: String }
```

#### `GetResourceDefinitionResponse`

``` purescript
newtype GetResourceDefinitionResponse
  = GetResourceDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `GetResourceDefinitionVersionRequest`

``` purescript
newtype GetResourceDefinitionVersionRequest
  = GetResourceDefinitionVersionRequest { "ResourceDefinitionId" :: String, "ResourceDefinitionVersionId" :: String }
```

#### `GetResourceDefinitionVersionResponse`

``` purescript
newtype GetResourceDefinitionVersionResponse
  = GetResourceDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (ResourceDefinitionVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `GetServiceRoleForAccountRequest`

``` purescript
newtype GetServiceRoleForAccountRequest
  = GetServiceRoleForAccountRequest {  }
```

#### `GetServiceRoleForAccountResponse`

``` purescript
newtype GetServiceRoleForAccountResponse
  = GetServiceRoleForAccountResponse { "AssociatedAt" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

#### `GetSubscriptionDefinitionRequest`

``` purescript
newtype GetSubscriptionDefinitionRequest
  = GetSubscriptionDefinitionRequest { "SubscriptionDefinitionId" :: String }
```

#### `GetSubscriptionDefinitionResponse`

``` purescript
newtype GetSubscriptionDefinitionResponse
  = GetSubscriptionDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `GetSubscriptionDefinitionVersionRequest`

``` purescript
newtype GetSubscriptionDefinitionVersionRequest
  = GetSubscriptionDefinitionVersionRequest { "SubscriptionDefinitionId" :: String, "SubscriptionDefinitionVersionId" :: String }
```

#### `GetSubscriptionDefinitionVersionResponse`

``` purescript
newtype GetSubscriptionDefinitionVersionResponse
  = GetSubscriptionDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (SubscriptionDefinitionVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `GroupCertificateAuthorityProperties`

``` purescript
newtype GroupCertificateAuthorityProperties
  = GroupCertificateAuthorityProperties { "GroupCertificateAuthorityArn" :: NullOrUndefined (String), "GroupCertificateAuthorityId" :: NullOrUndefined (String) }
```

Information on group certificate authority properties

#### `GroupCertificateConfiguration`

``` purescript
newtype GroupCertificateConfiguration
  = GroupCertificateConfiguration { "CertificateAuthorityExpiryInMilliseconds" :: NullOrUndefined (String), "CertificateExpiryInMilliseconds" :: NullOrUndefined (String), "GroupId" :: NullOrUndefined (String) }
```

Information on the group certificate configuration

#### `GroupInformation`

``` purescript
newtype GroupInformation
  = GroupInformation { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

Information on the group

#### `GroupOwnerSetting`

``` purescript
newtype GroupOwnerSetting
  = GroupOwnerSetting { "AutoAddGroupOwner" :: NullOrUndefined (Boolean), "GroupOwner" :: NullOrUndefined (String) }
```

Group owner related settings for local resources.

#### `GroupVersion`

``` purescript
newtype GroupVersion
  = GroupVersion { "CoreDefinitionVersionArn" :: NullOrUndefined (String), "DeviceDefinitionVersionArn" :: NullOrUndefined (String), "FunctionDefinitionVersionArn" :: NullOrUndefined (String), "LoggerDefinitionVersionArn" :: NullOrUndefined (String), "ResourceDefinitionVersionArn" :: NullOrUndefined (String), "SubscriptionDefinitionVersionArn" :: NullOrUndefined (String) }
```

Information on group version

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "ErrorDetails" :: NullOrUndefined (ErrorDetails), "Message" :: NullOrUndefined (String) }
```

General Error

#### `ListCoreDefinitionVersionsRequest`

``` purescript
newtype ListCoreDefinitionVersionsRequest
  = ListCoreDefinitionVersionsRequest { "CoreDefinitionId" :: String, "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListCoreDefinitionVersionsResponse`

``` purescript
newtype ListCoreDefinitionVersionsResponse
  = ListCoreDefinitionVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

#### `ListCoreDefinitionsRequest`

``` purescript
newtype ListCoreDefinitionsRequest
  = ListCoreDefinitionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListCoreDefinitionsResponse`

``` purescript
newtype ListCoreDefinitionsResponse
  = ListCoreDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

#### `ListDefinitionsResponse`

``` purescript
newtype ListDefinitionsResponse
  = ListDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

List of definition responses

#### `ListDeploymentsRequest`

``` purescript
newtype ListDeploymentsRequest
  = ListDeploymentsRequest { "GroupId" :: String, "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListDeploymentsResponse`

``` purescript
newtype ListDeploymentsResponse
  = ListDeploymentsResponse { "Deployments" :: NullOrUndefined (Deployments), "NextToken" :: NullOrUndefined (String) }
```

#### `ListDeviceDefinitionVersionsRequest`

``` purescript
newtype ListDeviceDefinitionVersionsRequest
  = ListDeviceDefinitionVersionsRequest { "DeviceDefinitionId" :: String, "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListDeviceDefinitionVersionsResponse`

``` purescript
newtype ListDeviceDefinitionVersionsResponse
  = ListDeviceDefinitionVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

#### `ListDeviceDefinitionsRequest`

``` purescript
newtype ListDeviceDefinitionsRequest
  = ListDeviceDefinitionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListDeviceDefinitionsResponse`

``` purescript
newtype ListDeviceDefinitionsResponse
  = ListDeviceDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

#### `ListFunctionDefinitionVersionsRequest`

``` purescript
newtype ListFunctionDefinitionVersionsRequest
  = ListFunctionDefinitionVersionsRequest { "FunctionDefinitionId" :: String, "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListFunctionDefinitionVersionsResponse`

``` purescript
newtype ListFunctionDefinitionVersionsResponse
  = ListFunctionDefinitionVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

#### `ListFunctionDefinitionsRequest`

``` purescript
newtype ListFunctionDefinitionsRequest
  = ListFunctionDefinitionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListFunctionDefinitionsResponse`

``` purescript
newtype ListFunctionDefinitionsResponse
  = ListFunctionDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

#### `ListGroupCertificateAuthoritiesRequest`

``` purescript
newtype ListGroupCertificateAuthoritiesRequest
  = ListGroupCertificateAuthoritiesRequest { "GroupId" :: String }
```

#### `ListGroupCertificateAuthoritiesResponse`

``` purescript
newtype ListGroupCertificateAuthoritiesResponse
  = ListGroupCertificateAuthoritiesResponse { "GroupCertificateAuthorities" :: NullOrUndefined (ListOfGroupCertificateAuthorityProperties) }
```

#### `ListGroupVersionsRequest`

``` purescript
newtype ListGroupVersionsRequest
  = ListGroupVersionsRequest { "GroupId" :: String, "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListGroupVersionsResponse`

``` purescript
newtype ListGroupVersionsResponse
  = ListGroupVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

#### `ListGroupsRequest`

``` purescript
newtype ListGroupsRequest
  = ListGroupsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListGroupsResponse`

``` purescript
newtype ListGroupsResponse
  = ListGroupsResponse { "Groups" :: NullOrUndefined (ListOfGroupInformation), "NextToken" :: NullOrUndefined (String) }
```

#### `ListLoggerDefinitionVersionsRequest`

``` purescript
newtype ListLoggerDefinitionVersionsRequest
  = ListLoggerDefinitionVersionsRequest { "LoggerDefinitionId" :: String, "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListLoggerDefinitionVersionsResponse`

``` purescript
newtype ListLoggerDefinitionVersionsResponse
  = ListLoggerDefinitionVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

#### `ListLoggerDefinitionsRequest`

``` purescript
newtype ListLoggerDefinitionsRequest
  = ListLoggerDefinitionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListLoggerDefinitionsResponse`

``` purescript
newtype ListLoggerDefinitionsResponse
  = ListLoggerDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

#### `ListOfConnectivityInfo`

``` purescript
newtype ListOfConnectivityInfo
  = ListOfConnectivityInfo (Array ConnectivityInfo)
```

#### `ListOfCore`

``` purescript
newtype ListOfCore
  = ListOfCore (Array Core)
```

#### `ListOfDefinitionInformation`

``` purescript
newtype ListOfDefinitionInformation
  = ListOfDefinitionInformation (Array DefinitionInformation)
```

#### `ListOfDevice`

``` purescript
newtype ListOfDevice
  = ListOfDevice (Array Device)
```

#### `ListOfFunction`

``` purescript
newtype ListOfFunction
  = ListOfFunction (Array Function'')
```

#### `ListOfGroupCertificateAuthorityProperties`

``` purescript
newtype ListOfGroupCertificateAuthorityProperties
  = ListOfGroupCertificateAuthorityProperties (Array GroupCertificateAuthorityProperties)
```

#### `ListOfGroupInformation`

``` purescript
newtype ListOfGroupInformation
  = ListOfGroupInformation (Array GroupInformation)
```

#### `ListOfLogger`

``` purescript
newtype ListOfLogger
  = ListOfLogger (Array Logger)
```

#### `ListOfResource`

``` purescript
newtype ListOfResource
  = ListOfResource (Array Resource)
```

#### `ListOfResourceAccessPolicy`

``` purescript
newtype ListOfResourceAccessPolicy
  = ListOfResourceAccessPolicy (Array ResourceAccessPolicy)
```

#### `ListOfSubscription`

``` purescript
newtype ListOfSubscription
  = ListOfSubscription (Array Subscription)
```

#### `ListOfVersionInformation`

``` purescript
newtype ListOfVersionInformation
  = ListOfVersionInformation (Array VersionInformation)
```

#### `ListResourceDefinitionVersionsRequest`

``` purescript
newtype ListResourceDefinitionVersionsRequest
  = ListResourceDefinitionVersionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String), "ResourceDefinitionId" :: String }
```

#### `ListResourceDefinitionVersionsResponse`

``` purescript
newtype ListResourceDefinitionVersionsResponse
  = ListResourceDefinitionVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

#### `ListResourceDefinitionsRequest`

``` purescript
newtype ListResourceDefinitionsRequest
  = ListResourceDefinitionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListResourceDefinitionsResponse`

``` purescript
newtype ListResourceDefinitionsResponse
  = ListResourceDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

#### `ListSubscriptionDefinitionVersionsRequest`

``` purescript
newtype ListSubscriptionDefinitionVersionsRequest
  = ListSubscriptionDefinitionVersionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String), "SubscriptionDefinitionId" :: String }
```

#### `ListSubscriptionDefinitionVersionsResponse`

``` purescript
newtype ListSubscriptionDefinitionVersionsResponse
  = ListSubscriptionDefinitionVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

#### `ListSubscriptionDefinitionsRequest`

``` purescript
newtype ListSubscriptionDefinitionsRequest
  = ListSubscriptionDefinitionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

#### `ListSubscriptionDefinitionsResponse`

``` purescript
newtype ListSubscriptionDefinitionsResponse
  = ListSubscriptionDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

#### `ListVersionsResponse`

``` purescript
newtype ListVersionsResponse
  = ListVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

List of versions response

#### `LocalDeviceResourceData`

``` purescript
newtype LocalDeviceResourceData
  = LocalDeviceResourceData { "GroupOwnerSetting" :: NullOrUndefined (GroupOwnerSetting), "SourcePath" :: NullOrUndefined (String) }
```

Attributes that define the Local Device Resource.

#### `LocalVolumeResourceData`

``` purescript
newtype LocalVolumeResourceData
  = LocalVolumeResourceData { "DestinationPath" :: NullOrUndefined (String), "GroupOwnerSetting" :: NullOrUndefined (GroupOwnerSetting), "SourcePath" :: NullOrUndefined (String) }
```

Attributes that define the Local Volume Resource.

#### `Logger`

``` purescript
newtype Logger
  = Logger { "Component" :: NullOrUndefined (LoggerComponent), "Id" :: NullOrUndefined (String), "Level" :: NullOrUndefined (LoggerLevel), "Space" :: NullOrUndefined (Int), "Type" :: NullOrUndefined (LoggerType) }
```

Information on the Logger

#### `LoggerComponent`

``` purescript
newtype LoggerComponent
  = LoggerComponent String
```

#### `LoggerDefinitionVersion`

``` purescript
newtype LoggerDefinitionVersion
  = LoggerDefinitionVersion { "Loggers" :: NullOrUndefined (ListOfLogger) }
```

Information on logger definition version

#### `LoggerLevel`

``` purescript
newtype LoggerLevel
  = LoggerLevel String
```

#### `LoggerType`

``` purescript
newtype LoggerType
  = LoggerType String
```

#### `MapOf__string`

``` purescript
newtype MapOf__string
  = MapOf__string (Map String String)
```

#### `Permission`

``` purescript
newtype Permission
  = Permission String
```

Type of permissions a function could have to access a resource.

#### `ResetDeploymentsRequest`

``` purescript
newtype ResetDeploymentsRequest
  = ResetDeploymentsRequest { "AmznClientToken" :: NullOrUndefined (String), "Force" :: NullOrUndefined (Boolean), "GroupId" :: String }
```

Information needed to perform a reset of a group's deployments.

#### `ResetDeploymentsResponse`

``` purescript
newtype ResetDeploymentsResponse
  = ResetDeploymentsResponse { "DeploymentArn" :: NullOrUndefined (String), "DeploymentId" :: NullOrUndefined (String) }
```

#### `Resource`

``` purescript
newtype Resource
  = Resource { "Id" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "ResourceDataContainer" :: NullOrUndefined (ResourceDataContainer) }
```

Information on the resource.

#### `ResourceAccessPolicy`

``` purescript
newtype ResourceAccessPolicy
  = ResourceAccessPolicy { "Permission" :: NullOrUndefined (Permission), "ResourceId" :: NullOrUndefined (String) }
```

Policy for the function to access a resource.

#### `ResourceDataContainer`

``` purescript
newtype ResourceDataContainer
  = ResourceDataContainer { "LocalDeviceResourceData" :: NullOrUndefined (LocalDeviceResourceData), "LocalVolumeResourceData" :: NullOrUndefined (LocalVolumeResourceData) }
```

A container of data for all resource types.

#### `ResourceDefinitionVersion`

``` purescript
newtype ResourceDefinitionVersion
  = ResourceDefinitionVersion { "Resources" :: NullOrUndefined (ListOfResource) }
```

Information on resource definition version

#### `S3UrlSignerRole`

``` purescript
newtype S3UrlSignerRole
  = S3UrlSignerRole String
```

The IAM Role that Greengrass will use to create pre-signed URLs pointing towards the update artifact.

#### `SoftwareToUpdate`

``` purescript
newtype SoftwareToUpdate
  = SoftwareToUpdate String
```

The piece of software on the Greengrass Core that will be updated.

#### `Subscription`

``` purescript
newtype Subscription
  = Subscription { "Id" :: NullOrUndefined (String), "Source" :: NullOrUndefined (String), "Subject" :: NullOrUndefined (String), "Target" :: NullOrUndefined (String) }
```

Information on subscription

#### `SubscriptionDefinitionVersion`

``` purescript
newtype SubscriptionDefinitionVersion
  = SubscriptionDefinitionVersion { "Subscriptions" :: NullOrUndefined (ListOfSubscription) }
```

Information on subscription definition version

#### `UpdateAgentLogLevel`

``` purescript
newtype UpdateAgentLogLevel
  = UpdateAgentLogLevel String
```

The minimum level of log statements that should be logged by the OTA Agent during an update.

#### `UpdateConnectivityInfoRequest`

``` purescript
newtype UpdateConnectivityInfoRequest
  = UpdateConnectivityInfoRequest { "ConnectivityInfo" :: NullOrUndefined (ListOfConnectivityInfo), "ThingName" :: String }
```

connectivity info request

#### `UpdateConnectivityInfoResponse`

``` purescript
newtype UpdateConnectivityInfoResponse
  = UpdateConnectivityInfoResponse { "Message" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

#### `UpdateCoreDefinitionRequest`

``` purescript
newtype UpdateCoreDefinitionRequest
  = UpdateCoreDefinitionRequest { "CoreDefinitionId" :: String, "Name" :: NullOrUndefined (String) }
```

#### `UpdateCoreDefinitionResponse`

``` purescript
newtype UpdateCoreDefinitionResponse
  = UpdateCoreDefinitionResponse {  }
```

#### `UpdateDeviceDefinitionRequest`

``` purescript
newtype UpdateDeviceDefinitionRequest
  = UpdateDeviceDefinitionRequest { "DeviceDefinitionId" :: String, "Name" :: NullOrUndefined (String) }
```

#### `UpdateDeviceDefinitionResponse`

``` purescript
newtype UpdateDeviceDefinitionResponse
  = UpdateDeviceDefinitionResponse {  }
```

#### `UpdateFunctionDefinitionRequest`

``` purescript
newtype UpdateFunctionDefinitionRequest
  = UpdateFunctionDefinitionRequest { "FunctionDefinitionId" :: String, "Name" :: NullOrUndefined (String) }
```

#### `UpdateFunctionDefinitionResponse`

``` purescript
newtype UpdateFunctionDefinitionResponse
  = UpdateFunctionDefinitionResponse {  }
```

#### `UpdateGroupCertificateConfigurationRequest`

``` purescript
newtype UpdateGroupCertificateConfigurationRequest
  = UpdateGroupCertificateConfigurationRequest { "CertificateExpiryInMilliseconds" :: NullOrUndefined (String), "GroupId" :: String }
```

#### `UpdateGroupCertificateConfigurationResponse`

``` purescript
newtype UpdateGroupCertificateConfigurationResponse
  = UpdateGroupCertificateConfigurationResponse { "CertificateAuthorityExpiryInMilliseconds" :: NullOrUndefined (String), "CertificateExpiryInMilliseconds" :: NullOrUndefined (String), "GroupId" :: NullOrUndefined (String) }
```

#### `UpdateGroupRequest`

``` purescript
newtype UpdateGroupRequest
  = UpdateGroupRequest { "GroupId" :: String, "Name" :: NullOrUndefined (String) }
```

#### `UpdateGroupResponse`

``` purescript
newtype UpdateGroupResponse
  = UpdateGroupResponse {  }
```

#### `UpdateLoggerDefinitionRequest`

``` purescript
newtype UpdateLoggerDefinitionRequest
  = UpdateLoggerDefinitionRequest { "LoggerDefinitionId" :: String, "Name" :: NullOrUndefined (String) }
```

#### `UpdateLoggerDefinitionResponse`

``` purescript
newtype UpdateLoggerDefinitionResponse
  = UpdateLoggerDefinitionResponse {  }
```

#### `UpdateResourceDefinitionRequest`

``` purescript
newtype UpdateResourceDefinitionRequest
  = UpdateResourceDefinitionRequest { "Name" :: NullOrUndefined (String), "ResourceDefinitionId" :: String }
```

#### `UpdateResourceDefinitionResponse`

``` purescript
newtype UpdateResourceDefinitionResponse
  = UpdateResourceDefinitionResponse {  }
```

#### `UpdateSubscriptionDefinitionRequest`

``` purescript
newtype UpdateSubscriptionDefinitionRequest
  = UpdateSubscriptionDefinitionRequest { "Name" :: NullOrUndefined (String), "SubscriptionDefinitionId" :: String }
```

#### `UpdateSubscriptionDefinitionResponse`

``` purescript
newtype UpdateSubscriptionDefinitionResponse
  = UpdateSubscriptionDefinitionResponse {  }
```

#### `UpdateTargets`

``` purescript
newtype UpdateTargets
  = UpdateTargets (Array String)
```

The target arns that this update will be applied to.

#### `UpdateTargetsArchitecture`

``` purescript
newtype UpdateTargetsArchitecture
  = UpdateTargetsArchitecture String
```

The architecture of the Cores in the targets of an update

#### `UpdateTargetsOperatingSystem`

``` purescript
newtype UpdateTargetsOperatingSystem
  = UpdateTargetsOperatingSystem String
```

The operating system of the Cores in the targets of an update

#### `VersionInformation`

``` purescript
newtype VersionInformation
  = VersionInformation { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

Information on the version


