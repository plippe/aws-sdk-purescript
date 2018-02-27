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

##### Instances
``` purescript
Newtype AssociateRoleToGroupRequest _
```

#### `AssociateRoleToGroupResponse`

``` purescript
newtype AssociateRoleToGroupResponse
  = AssociateRoleToGroupResponse { "AssociatedAt" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype AssociateRoleToGroupResponse _
```

#### `AssociateServiceRoleToAccountRequest`

``` purescript
newtype AssociateServiceRoleToAccountRequest
  = AssociateServiceRoleToAccountRequest { "RoleArn" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype AssociateServiceRoleToAccountRequest _
```

#### `AssociateServiceRoleToAccountResponse`

``` purescript
newtype AssociateServiceRoleToAccountResponse
  = AssociateServiceRoleToAccountResponse { "AssociatedAt" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype AssociateServiceRoleToAccountResponse _
```

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "ErrorDetails" :: NullOrUndefined (ErrorDetails), "Message" :: NullOrUndefined (String) }
```

General Error

##### Instances
``` purescript
Newtype BadRequestException _
```

#### `ConnectivityInfo`

``` purescript
newtype ConnectivityInfo
  = ConnectivityInfo { "HostAddress" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Metadata" :: NullOrUndefined (String), "PortNumber" :: NullOrUndefined (Int) }
```

Connectivity Info

##### Instances
``` purescript
Newtype ConnectivityInfo _
```

#### `Core`

``` purescript
newtype Core
  = Core { "CertificateArn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "SyncShadow" :: NullOrUndefined (Boolean), "ThingArn" :: NullOrUndefined (String) }
```

Information on the core

##### Instances
``` purescript
Newtype Core _
```

#### `CoreDefinitionVersion`

``` purescript
newtype CoreDefinitionVersion
  = CoreDefinitionVersion { "Cores" :: NullOrUndefined (ListOfCore) }
```

Information on core definition version

##### Instances
``` purescript
Newtype CoreDefinitionVersion _
```

#### `CreateCoreDefinitionRequest`

``` purescript
newtype CreateCoreDefinitionRequest
  = CreateCoreDefinitionRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (CoreDefinitionVersion), "Name" :: NullOrUndefined (String) }
```

Information on the core definition request

##### Instances
``` purescript
Newtype CreateCoreDefinitionRequest _
```

#### `CreateCoreDefinitionResponse`

``` purescript
newtype CreateCoreDefinitionResponse
  = CreateCoreDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateCoreDefinitionResponse _
```

#### `CreateCoreDefinitionVersionRequest`

``` purescript
newtype CreateCoreDefinitionVersionRequest
  = CreateCoreDefinitionVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "CoreDefinitionId" :: String, "Cores" :: NullOrUndefined (ListOfCore) }
```

##### Instances
``` purescript
Newtype CreateCoreDefinitionVersionRequest _
```

#### `CreateCoreDefinitionVersionResponse`

``` purescript
newtype CreateCoreDefinitionVersionResponse
  = CreateCoreDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateCoreDefinitionVersionResponse _
```

#### `CreateDeploymentRequest`

``` purescript
newtype CreateDeploymentRequest
  = CreateDeploymentRequest { "AmznClientToken" :: NullOrUndefined (String), "DeploymentId" :: NullOrUndefined (String), "DeploymentType" :: NullOrUndefined (DeploymentType), "GroupId" :: String, "GroupVersionId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateDeploymentRequest _
```

#### `CreateDeploymentResponse`

``` purescript
newtype CreateDeploymentResponse
  = CreateDeploymentResponse { "DeploymentArn" :: NullOrUndefined (String), "DeploymentId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateDeploymentResponse _
```

#### `CreateDeviceDefinitionRequest`

``` purescript
newtype CreateDeviceDefinitionRequest
  = CreateDeviceDefinitionRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (DeviceDefinitionVersion), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateDeviceDefinitionRequest _
```

#### `CreateDeviceDefinitionResponse`

``` purescript
newtype CreateDeviceDefinitionResponse
  = CreateDeviceDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateDeviceDefinitionResponse _
```

#### `CreateDeviceDefinitionVersionRequest`

``` purescript
newtype CreateDeviceDefinitionVersionRequest
  = CreateDeviceDefinitionVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "DeviceDefinitionId" :: String, "Devices" :: NullOrUndefined (ListOfDevice) }
```

##### Instances
``` purescript
Newtype CreateDeviceDefinitionVersionRequest _
```

#### `CreateDeviceDefinitionVersionResponse`

``` purescript
newtype CreateDeviceDefinitionVersionResponse
  = CreateDeviceDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateDeviceDefinitionVersionResponse _
```

#### `CreateFunctionDefinitionRequest`

``` purescript
newtype CreateFunctionDefinitionRequest
  = CreateFunctionDefinitionRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (FunctionDefinitionVersion), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateFunctionDefinitionRequest _
```

#### `CreateFunctionDefinitionResponse`

``` purescript
newtype CreateFunctionDefinitionResponse
  = CreateFunctionDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateFunctionDefinitionResponse _
```

#### `CreateFunctionDefinitionVersionRequest`

``` purescript
newtype CreateFunctionDefinitionVersionRequest
  = CreateFunctionDefinitionVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "FunctionDefinitionId" :: String, "Functions" :: NullOrUndefined (ListOfFunction) }
```

Function definition version

##### Instances
``` purescript
Newtype CreateFunctionDefinitionVersionRequest _
```

#### `CreateFunctionDefinitionVersionResponse`

``` purescript
newtype CreateFunctionDefinitionVersionResponse
  = CreateFunctionDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateFunctionDefinitionVersionResponse _
```

#### `CreateGroupCertificateAuthorityRequest`

``` purescript
newtype CreateGroupCertificateAuthorityRequest
  = CreateGroupCertificateAuthorityRequest { "AmznClientToken" :: NullOrUndefined (String), "GroupId" :: String }
```

##### Instances
``` purescript
Newtype CreateGroupCertificateAuthorityRequest _
```

#### `CreateGroupCertificateAuthorityResponse`

``` purescript
newtype CreateGroupCertificateAuthorityResponse
  = CreateGroupCertificateAuthorityResponse { "GroupCertificateAuthorityArn" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateGroupCertificateAuthorityResponse _
```

#### `CreateGroupRequest`

``` purescript
newtype CreateGroupRequest
  = CreateGroupRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (GroupVersion), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateGroupRequest _
```

#### `CreateGroupResponse`

``` purescript
newtype CreateGroupResponse
  = CreateGroupResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateGroupResponse _
```

#### `CreateGroupVersionRequest`

``` purescript
newtype CreateGroupVersionRequest
  = CreateGroupVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "CoreDefinitionVersionArn" :: NullOrUndefined (String), "DeviceDefinitionVersionArn" :: NullOrUndefined (String), "FunctionDefinitionVersionArn" :: NullOrUndefined (String), "GroupId" :: String, "LoggerDefinitionVersionArn" :: NullOrUndefined (String), "ResourceDefinitionVersionArn" :: NullOrUndefined (String), "SubscriptionDefinitionVersionArn" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateGroupVersionRequest _
```

#### `CreateGroupVersionResponse`

``` purescript
newtype CreateGroupVersionResponse
  = CreateGroupVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateGroupVersionResponse _
```

#### `CreateLoggerDefinitionRequest`

``` purescript
newtype CreateLoggerDefinitionRequest
  = CreateLoggerDefinitionRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (LoggerDefinitionVersion), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateLoggerDefinitionRequest _
```

#### `CreateLoggerDefinitionResponse`

``` purescript
newtype CreateLoggerDefinitionResponse
  = CreateLoggerDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateLoggerDefinitionResponse _
```

#### `CreateLoggerDefinitionVersionRequest`

``` purescript
newtype CreateLoggerDefinitionVersionRequest
  = CreateLoggerDefinitionVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "LoggerDefinitionId" :: String, "Loggers" :: NullOrUndefined (ListOfLogger) }
```

##### Instances
``` purescript
Newtype CreateLoggerDefinitionVersionRequest _
```

#### `CreateLoggerDefinitionVersionResponse`

``` purescript
newtype CreateLoggerDefinitionVersionResponse
  = CreateLoggerDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateLoggerDefinitionVersionResponse _
```

#### `CreateResourceDefinitionRequest`

``` purescript
newtype CreateResourceDefinitionRequest
  = CreateResourceDefinitionRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (ResourceDefinitionVersion), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateResourceDefinitionRequest _
```

#### `CreateResourceDefinitionResponse`

``` purescript
newtype CreateResourceDefinitionResponse
  = CreateResourceDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateResourceDefinitionResponse _
```

#### `CreateResourceDefinitionVersionRequest`

``` purescript
newtype CreateResourceDefinitionVersionRequest
  = CreateResourceDefinitionVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "ResourceDefinitionId" :: String, "Resources" :: NullOrUndefined (ListOfResource) }
```

##### Instances
``` purescript
Newtype CreateResourceDefinitionVersionRequest _
```

#### `CreateResourceDefinitionVersionResponse`

``` purescript
newtype CreateResourceDefinitionVersionResponse
  = CreateResourceDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateResourceDefinitionVersionResponse _
```

#### `CreateSoftwareUpdateJobRequest`

``` purescript
newtype CreateSoftwareUpdateJobRequest
  = CreateSoftwareUpdateJobRequest { "AmznClientToken" :: NullOrUndefined (String), "S3UrlSignerRole" :: NullOrUndefined (S3UrlSignerRole), "SoftwareToUpdate" :: NullOrUndefined (SoftwareToUpdate), "UpdateAgentLogLevel" :: NullOrUndefined (UpdateAgentLogLevel), "UpdateTargets" :: NullOrUndefined (UpdateTargets), "UpdateTargetsArchitecture" :: NullOrUndefined (UpdateTargetsArchitecture), "UpdateTargetsOperatingSystem" :: NullOrUndefined (UpdateTargetsOperatingSystem) }
```

##### Instances
``` purescript
Newtype CreateSoftwareUpdateJobRequest _
```

#### `CreateSoftwareUpdateJobResponse`

``` purescript
newtype CreateSoftwareUpdateJobResponse
  = CreateSoftwareUpdateJobResponse { "IotJobArn" :: NullOrUndefined (String), "IotJobId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateSoftwareUpdateJobResponse _
```

#### `CreateSubscriptionDefinitionRequest`

``` purescript
newtype CreateSubscriptionDefinitionRequest
  = CreateSubscriptionDefinitionRequest { "AmznClientToken" :: NullOrUndefined (String), "InitialVersion" :: NullOrUndefined (SubscriptionDefinitionVersion), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateSubscriptionDefinitionRequest _
```

#### `CreateSubscriptionDefinitionResponse`

``` purescript
newtype CreateSubscriptionDefinitionResponse
  = CreateSubscriptionDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateSubscriptionDefinitionResponse _
```

#### `CreateSubscriptionDefinitionVersionRequest`

``` purescript
newtype CreateSubscriptionDefinitionVersionRequest
  = CreateSubscriptionDefinitionVersionRequest { "AmznClientToken" :: NullOrUndefined (String), "SubscriptionDefinitionId" :: String, "Subscriptions" :: NullOrUndefined (ListOfSubscription) }
```

##### Instances
``` purescript
Newtype CreateSubscriptionDefinitionVersionRequest _
```

#### `CreateSubscriptionDefinitionVersionResponse`

``` purescript
newtype CreateSubscriptionDefinitionVersionResponse
  = CreateSubscriptionDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateSubscriptionDefinitionVersionResponse _
```

#### `DefinitionInformation`

``` purescript
newtype DefinitionInformation
  = DefinitionInformation { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

Information on the Definition

##### Instances
``` purescript
Newtype DefinitionInformation _
```

#### `DeleteCoreDefinitionRequest`

``` purescript
newtype DeleteCoreDefinitionRequest
  = DeleteCoreDefinitionRequest { "CoreDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype DeleteCoreDefinitionRequest _
```

#### `DeleteCoreDefinitionResponse`

``` purescript
newtype DeleteCoreDefinitionResponse
  = DeleteCoreDefinitionResponse {  }
```

##### Instances
``` purescript
Newtype DeleteCoreDefinitionResponse _
```

#### `DeleteDeviceDefinitionRequest`

``` purescript
newtype DeleteDeviceDefinitionRequest
  = DeleteDeviceDefinitionRequest { "DeviceDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype DeleteDeviceDefinitionRequest _
```

#### `DeleteDeviceDefinitionResponse`

``` purescript
newtype DeleteDeviceDefinitionResponse
  = DeleteDeviceDefinitionResponse {  }
```

##### Instances
``` purescript
Newtype DeleteDeviceDefinitionResponse _
```

#### `DeleteFunctionDefinitionRequest`

``` purescript
newtype DeleteFunctionDefinitionRequest
  = DeleteFunctionDefinitionRequest { "FunctionDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype DeleteFunctionDefinitionRequest _
```

#### `DeleteFunctionDefinitionResponse`

``` purescript
newtype DeleteFunctionDefinitionResponse
  = DeleteFunctionDefinitionResponse {  }
```

##### Instances
``` purescript
Newtype DeleteFunctionDefinitionResponse _
```

#### `DeleteGroupRequest`

``` purescript
newtype DeleteGroupRequest
  = DeleteGroupRequest { "GroupId" :: String }
```

##### Instances
``` purescript
Newtype DeleteGroupRequest _
```

#### `DeleteGroupResponse`

``` purescript
newtype DeleteGroupResponse
  = DeleteGroupResponse {  }
```

##### Instances
``` purescript
Newtype DeleteGroupResponse _
```

#### `DeleteLoggerDefinitionRequest`

``` purescript
newtype DeleteLoggerDefinitionRequest
  = DeleteLoggerDefinitionRequest { "LoggerDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype DeleteLoggerDefinitionRequest _
```

#### `DeleteLoggerDefinitionResponse`

``` purescript
newtype DeleteLoggerDefinitionResponse
  = DeleteLoggerDefinitionResponse {  }
```

##### Instances
``` purescript
Newtype DeleteLoggerDefinitionResponse _
```

#### `DeleteResourceDefinitionRequest`

``` purescript
newtype DeleteResourceDefinitionRequest
  = DeleteResourceDefinitionRequest { "ResourceDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype DeleteResourceDefinitionRequest _
```

#### `DeleteResourceDefinitionResponse`

``` purescript
newtype DeleteResourceDefinitionResponse
  = DeleteResourceDefinitionResponse {  }
```

##### Instances
``` purescript
Newtype DeleteResourceDefinitionResponse _
```

#### `DeleteSubscriptionDefinitionRequest`

``` purescript
newtype DeleteSubscriptionDefinitionRequest
  = DeleteSubscriptionDefinitionRequest { "SubscriptionDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype DeleteSubscriptionDefinitionRequest _
```

#### `DeleteSubscriptionDefinitionResponse`

``` purescript
newtype DeleteSubscriptionDefinitionResponse
  = DeleteSubscriptionDefinitionResponse {  }
```

##### Instances
``` purescript
Newtype DeleteSubscriptionDefinitionResponse _
```

#### `Deployment`

``` purescript
newtype Deployment
  = Deployment { "CreatedAt" :: NullOrUndefined (String), "DeploymentArn" :: NullOrUndefined (String), "DeploymentId" :: NullOrUndefined (String), "DeploymentType" :: NullOrUndefined (DeploymentType), "GroupArn" :: NullOrUndefined (String) }
```

Information on the deployment

##### Instances
``` purescript
Newtype Deployment _
```

#### `DeploymentType`

``` purescript
newtype DeploymentType
  = DeploymentType String
```

##### Instances
``` purescript
Newtype DeploymentType _
```

#### `Deployments`

``` purescript
newtype Deployments
  = Deployments (Array Deployment)
```

##### Instances
``` purescript
Newtype Deployments _
```

#### `Device`

``` purescript
newtype Device
  = Device { "CertificateArn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "SyncShadow" :: NullOrUndefined (Boolean), "ThingArn" :: NullOrUndefined (String) }
```

Information on a Device

##### Instances
``` purescript
Newtype Device _
```

#### `DeviceDefinitionVersion`

``` purescript
newtype DeviceDefinitionVersion
  = DeviceDefinitionVersion { "Devices" :: NullOrUndefined (ListOfDevice) }
```

Information on device definition version

##### Instances
``` purescript
Newtype DeviceDefinitionVersion _
```

#### `DisassociateRoleFromGroupRequest`

``` purescript
newtype DisassociateRoleFromGroupRequest
  = DisassociateRoleFromGroupRequest { "GroupId" :: String }
```

##### Instances
``` purescript
Newtype DisassociateRoleFromGroupRequest _
```

#### `DisassociateRoleFromGroupResponse`

``` purescript
newtype DisassociateRoleFromGroupResponse
  = DisassociateRoleFromGroupResponse { "DisassociatedAt" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DisassociateRoleFromGroupResponse _
```

#### `DisassociateServiceRoleFromAccountRequest`

``` purescript
newtype DisassociateServiceRoleFromAccountRequest
  = DisassociateServiceRoleFromAccountRequest {  }
```

##### Instances
``` purescript
Newtype DisassociateServiceRoleFromAccountRequest _
```

#### `DisassociateServiceRoleFromAccountResponse`

``` purescript
newtype DisassociateServiceRoleFromAccountResponse
  = DisassociateServiceRoleFromAccountResponse { "DisassociatedAt" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DisassociateServiceRoleFromAccountResponse _
```

#### `Empty`

``` purescript
newtype Empty
  = Empty {  }
```

Empty

##### Instances
``` purescript
Newtype Empty _
```

#### `ErrorDetail`

``` purescript
newtype ErrorDetail
  = ErrorDetail { "DetailedErrorCode" :: NullOrUndefined (String), "DetailedErrorMessage" :: NullOrUndefined (String) }
```

ErrorDetail

##### Instances
``` purescript
Newtype ErrorDetail _
```

#### `ErrorDetails`

``` purescript
newtype ErrorDetails
  = ErrorDetails (Array ErrorDetail)
```

Error Details

##### Instances
``` purescript
Newtype ErrorDetails _
```

#### `Function''`

``` purescript
newtype Function''
  = Function'' { "FunctionArn" :: NullOrUndefined (String), "FunctionConfiguration" :: NullOrUndefined (FunctionConfiguration), "Id" :: NullOrUndefined (String) }
```

Information on function

##### Instances
``` purescript
Newtype Function'' _
```

#### `FunctionConfiguration`

``` purescript
newtype FunctionConfiguration
  = FunctionConfiguration { "Environment" :: NullOrUndefined (FunctionConfigurationEnvironment), "ExecArgs" :: NullOrUndefined (String), "Executable" :: NullOrUndefined (String), "MemorySize" :: NullOrUndefined (Int), "Pinned" :: NullOrUndefined (Boolean), "Timeout" :: NullOrUndefined (Int) }
```

Configuration of the function

##### Instances
``` purescript
Newtype FunctionConfiguration _
```

#### `FunctionConfigurationEnvironment`

``` purescript
newtype FunctionConfigurationEnvironment
  = FunctionConfigurationEnvironment { "AccessSysfs" :: NullOrUndefined (Boolean), "ResourceAccessPolicies" :: NullOrUndefined (ListOfResourceAccessPolicy), "Variables" :: NullOrUndefined (MapOf__string) }
```

Environment of the function configuration

##### Instances
``` purescript
Newtype FunctionConfigurationEnvironment _
```

#### `FunctionDefinitionVersion`

``` purescript
newtype FunctionDefinitionVersion
  = FunctionDefinitionVersion { "Functions" :: NullOrUndefined (ListOfFunction) }
```

Information on the function definition version

##### Instances
``` purescript
Newtype FunctionDefinitionVersion _
```

#### `GeneralError`

``` purescript
newtype GeneralError
  = GeneralError { "ErrorDetails" :: NullOrUndefined (ErrorDetails), "Message" :: NullOrUndefined (String) }
```

General Error

##### Instances
``` purescript
Newtype GeneralError _
```

#### `GetAssociatedRoleRequest`

``` purescript
newtype GetAssociatedRoleRequest
  = GetAssociatedRoleRequest { "GroupId" :: String }
```

##### Instances
``` purescript
Newtype GetAssociatedRoleRequest _
```

#### `GetAssociatedRoleResponse`

``` purescript
newtype GetAssociatedRoleResponse
  = GetAssociatedRoleResponse { "AssociatedAt" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetAssociatedRoleResponse _
```

#### `GetConnectivityInfoRequest`

``` purescript
newtype GetConnectivityInfoRequest
  = GetConnectivityInfoRequest { "ThingName" :: String }
```

##### Instances
``` purescript
Newtype GetConnectivityInfoRequest _
```

#### `GetConnectivityInfoResponse`

``` purescript
newtype GetConnectivityInfoResponse
  = GetConnectivityInfoResponse { "ConnectivityInfo" :: NullOrUndefined (ListOfConnectivityInfo), "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetConnectivityInfoResponse _
```

#### `GetCoreDefinitionRequest`

``` purescript
newtype GetCoreDefinitionRequest
  = GetCoreDefinitionRequest { "CoreDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype GetCoreDefinitionRequest _
```

#### `GetCoreDefinitionResponse`

``` purescript
newtype GetCoreDefinitionResponse
  = GetCoreDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetCoreDefinitionResponse _
```

#### `GetCoreDefinitionVersionRequest`

``` purescript
newtype GetCoreDefinitionVersionRequest
  = GetCoreDefinitionVersionRequest { "CoreDefinitionId" :: String, "CoreDefinitionVersionId" :: String }
```

##### Instances
``` purescript
Newtype GetCoreDefinitionVersionRequest _
```

#### `GetCoreDefinitionVersionResponse`

``` purescript
newtype GetCoreDefinitionVersionResponse
  = GetCoreDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (CoreDefinitionVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetCoreDefinitionVersionResponse _
```

#### `GetDeploymentStatusRequest`

``` purescript
newtype GetDeploymentStatusRequest
  = GetDeploymentStatusRequest { "DeploymentId" :: String, "GroupId" :: String }
```

##### Instances
``` purescript
Newtype GetDeploymentStatusRequest _
```

#### `GetDeploymentStatusResponse`

``` purescript
newtype GetDeploymentStatusResponse
  = GetDeploymentStatusResponse { "DeploymentStatus" :: NullOrUndefined (String), "DeploymentType" :: NullOrUndefined (DeploymentType), "ErrorDetails" :: NullOrUndefined (ErrorDetails), "ErrorMessage" :: NullOrUndefined (String), "UpdatedAt" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetDeploymentStatusResponse _
```

#### `GetDeviceDefinitionRequest`

``` purescript
newtype GetDeviceDefinitionRequest
  = GetDeviceDefinitionRequest { "DeviceDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype GetDeviceDefinitionRequest _
```

#### `GetDeviceDefinitionResponse`

``` purescript
newtype GetDeviceDefinitionResponse
  = GetDeviceDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetDeviceDefinitionResponse _
```

#### `GetDeviceDefinitionVersionRequest`

``` purescript
newtype GetDeviceDefinitionVersionRequest
  = GetDeviceDefinitionVersionRequest { "DeviceDefinitionId" :: String, "DeviceDefinitionVersionId" :: String }
```

##### Instances
``` purescript
Newtype GetDeviceDefinitionVersionRequest _
```

#### `GetDeviceDefinitionVersionResponse`

``` purescript
newtype GetDeviceDefinitionVersionResponse
  = GetDeviceDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (DeviceDefinitionVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetDeviceDefinitionVersionResponse _
```

#### `GetFunctionDefinitionRequest`

``` purescript
newtype GetFunctionDefinitionRequest
  = GetFunctionDefinitionRequest { "FunctionDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype GetFunctionDefinitionRequest _
```

#### `GetFunctionDefinitionResponse`

``` purescript
newtype GetFunctionDefinitionResponse
  = GetFunctionDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetFunctionDefinitionResponse _
```

#### `GetFunctionDefinitionVersionRequest`

``` purescript
newtype GetFunctionDefinitionVersionRequest
  = GetFunctionDefinitionVersionRequest { "FunctionDefinitionId" :: String, "FunctionDefinitionVersionId" :: String }
```

##### Instances
``` purescript
Newtype GetFunctionDefinitionVersionRequest _
```

#### `GetFunctionDefinitionVersionResponse`

``` purescript
newtype GetFunctionDefinitionVersionResponse
  = GetFunctionDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (FunctionDefinitionVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetFunctionDefinitionVersionResponse _
```

#### `GetGroupCertificateAuthorityRequest`

``` purescript
newtype GetGroupCertificateAuthorityRequest
  = GetGroupCertificateAuthorityRequest { "CertificateAuthorityId" :: String, "GroupId" :: String }
```

##### Instances
``` purescript
Newtype GetGroupCertificateAuthorityRequest _
```

#### `GetGroupCertificateAuthorityResponse`

``` purescript
newtype GetGroupCertificateAuthorityResponse
  = GetGroupCertificateAuthorityResponse { "GroupCertificateAuthorityArn" :: NullOrUndefined (String), "GroupCertificateAuthorityId" :: NullOrUndefined (String), "PemEncodedCertificate" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetGroupCertificateAuthorityResponse _
```

#### `GetGroupCertificateConfigurationRequest`

``` purescript
newtype GetGroupCertificateConfigurationRequest
  = GetGroupCertificateConfigurationRequest { "GroupId" :: String }
```

##### Instances
``` purescript
Newtype GetGroupCertificateConfigurationRequest _
```

#### `GetGroupCertificateConfigurationResponse`

``` purescript
newtype GetGroupCertificateConfigurationResponse
  = GetGroupCertificateConfigurationResponse { "CertificateAuthorityExpiryInMilliseconds" :: NullOrUndefined (String), "CertificateExpiryInMilliseconds" :: NullOrUndefined (String), "GroupId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetGroupCertificateConfigurationResponse _
```

#### `GetGroupRequest`

``` purescript
newtype GetGroupRequest
  = GetGroupRequest { "GroupId" :: String }
```

##### Instances
``` purescript
Newtype GetGroupRequest _
```

#### `GetGroupResponse`

``` purescript
newtype GetGroupResponse
  = GetGroupResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetGroupResponse _
```

#### `GetGroupVersionRequest`

``` purescript
newtype GetGroupVersionRequest
  = GetGroupVersionRequest { "GroupId" :: String, "GroupVersionId" :: String }
```

##### Instances
``` purescript
Newtype GetGroupVersionRequest _
```

#### `GetGroupVersionResponse`

``` purescript
newtype GetGroupVersionResponse
  = GetGroupVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (GroupVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetGroupVersionResponse _
```

#### `GetLoggerDefinitionRequest`

``` purescript
newtype GetLoggerDefinitionRequest
  = GetLoggerDefinitionRequest { "LoggerDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype GetLoggerDefinitionRequest _
```

#### `GetLoggerDefinitionResponse`

``` purescript
newtype GetLoggerDefinitionResponse
  = GetLoggerDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetLoggerDefinitionResponse _
```

#### `GetLoggerDefinitionVersionRequest`

``` purescript
newtype GetLoggerDefinitionVersionRequest
  = GetLoggerDefinitionVersionRequest { "LoggerDefinitionId" :: String, "LoggerDefinitionVersionId" :: String }
```

##### Instances
``` purescript
Newtype GetLoggerDefinitionVersionRequest _
```

#### `GetLoggerDefinitionVersionResponse`

``` purescript
newtype GetLoggerDefinitionVersionResponse
  = GetLoggerDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (LoggerDefinitionVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetLoggerDefinitionVersionResponse _
```

#### `GetResourceDefinitionRequest`

``` purescript
newtype GetResourceDefinitionRequest
  = GetResourceDefinitionRequest { "ResourceDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype GetResourceDefinitionRequest _
```

#### `GetResourceDefinitionResponse`

``` purescript
newtype GetResourceDefinitionResponse
  = GetResourceDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetResourceDefinitionResponse _
```

#### `GetResourceDefinitionVersionRequest`

``` purescript
newtype GetResourceDefinitionVersionRequest
  = GetResourceDefinitionVersionRequest { "ResourceDefinitionId" :: String, "ResourceDefinitionVersionId" :: String }
```

##### Instances
``` purescript
Newtype GetResourceDefinitionVersionRequest _
```

#### `GetResourceDefinitionVersionResponse`

``` purescript
newtype GetResourceDefinitionVersionResponse
  = GetResourceDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (ResourceDefinitionVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetResourceDefinitionVersionResponse _
```

#### `GetServiceRoleForAccountRequest`

``` purescript
newtype GetServiceRoleForAccountRequest
  = GetServiceRoleForAccountRequest {  }
```

##### Instances
``` purescript
Newtype GetServiceRoleForAccountRequest _
```

#### `GetServiceRoleForAccountResponse`

``` purescript
newtype GetServiceRoleForAccountResponse
  = GetServiceRoleForAccountResponse { "AssociatedAt" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetServiceRoleForAccountResponse _
```

#### `GetSubscriptionDefinitionRequest`

``` purescript
newtype GetSubscriptionDefinitionRequest
  = GetSubscriptionDefinitionRequest { "SubscriptionDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype GetSubscriptionDefinitionRequest _
```

#### `GetSubscriptionDefinitionResponse`

``` purescript
newtype GetSubscriptionDefinitionResponse
  = GetSubscriptionDefinitionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetSubscriptionDefinitionResponse _
```

#### `GetSubscriptionDefinitionVersionRequest`

``` purescript
newtype GetSubscriptionDefinitionVersionRequest
  = GetSubscriptionDefinitionVersionRequest { "SubscriptionDefinitionId" :: String, "SubscriptionDefinitionVersionId" :: String }
```

##### Instances
``` purescript
Newtype GetSubscriptionDefinitionVersionRequest _
```

#### `GetSubscriptionDefinitionVersionResponse`

``` purescript
newtype GetSubscriptionDefinitionVersionResponse
  = GetSubscriptionDefinitionVersionResponse { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (SubscriptionDefinitionVersion), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetSubscriptionDefinitionVersionResponse _
```

#### `GroupCertificateAuthorityProperties`

``` purescript
newtype GroupCertificateAuthorityProperties
  = GroupCertificateAuthorityProperties { "GroupCertificateAuthorityArn" :: NullOrUndefined (String), "GroupCertificateAuthorityId" :: NullOrUndefined (String) }
```

Information on group certificate authority properties

##### Instances
``` purescript
Newtype GroupCertificateAuthorityProperties _
```

#### `GroupCertificateConfiguration`

``` purescript
newtype GroupCertificateConfiguration
  = GroupCertificateConfiguration { "CertificateAuthorityExpiryInMilliseconds" :: NullOrUndefined (String), "CertificateExpiryInMilliseconds" :: NullOrUndefined (String), "GroupId" :: NullOrUndefined (String) }
```

Information on the group certificate configuration

##### Instances
``` purescript
Newtype GroupCertificateConfiguration _
```

#### `GroupInformation`

``` purescript
newtype GroupInformation
  = GroupInformation { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LastUpdatedTimestamp" :: NullOrUndefined (String), "LatestVersion" :: NullOrUndefined (String), "LatestVersionArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

Information on the group

##### Instances
``` purescript
Newtype GroupInformation _
```

#### `GroupOwnerSetting`

``` purescript
newtype GroupOwnerSetting
  = GroupOwnerSetting { "AutoAddGroupOwner" :: NullOrUndefined (Boolean), "GroupOwner" :: NullOrUndefined (String) }
```

Group owner related settings for local resources.

##### Instances
``` purescript
Newtype GroupOwnerSetting _
```

#### `GroupVersion`

``` purescript
newtype GroupVersion
  = GroupVersion { "CoreDefinitionVersionArn" :: NullOrUndefined (String), "DeviceDefinitionVersionArn" :: NullOrUndefined (String), "FunctionDefinitionVersionArn" :: NullOrUndefined (String), "LoggerDefinitionVersionArn" :: NullOrUndefined (String), "ResourceDefinitionVersionArn" :: NullOrUndefined (String), "SubscriptionDefinitionVersionArn" :: NullOrUndefined (String) }
```

Information on group version

##### Instances
``` purescript
Newtype GroupVersion _
```

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "ErrorDetails" :: NullOrUndefined (ErrorDetails), "Message" :: NullOrUndefined (String) }
```

General Error

##### Instances
``` purescript
Newtype InternalServerErrorException _
```

#### `ListCoreDefinitionVersionsRequest`

``` purescript
newtype ListCoreDefinitionVersionsRequest
  = ListCoreDefinitionVersionsRequest { "CoreDefinitionId" :: String, "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListCoreDefinitionVersionsRequest _
```

#### `ListCoreDefinitionVersionsResponse`

``` purescript
newtype ListCoreDefinitionVersionsResponse
  = ListCoreDefinitionVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

##### Instances
``` purescript
Newtype ListCoreDefinitionVersionsResponse _
```

#### `ListCoreDefinitionsRequest`

``` purescript
newtype ListCoreDefinitionsRequest
  = ListCoreDefinitionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListCoreDefinitionsRequest _
```

#### `ListCoreDefinitionsResponse`

``` purescript
newtype ListCoreDefinitionsResponse
  = ListCoreDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListCoreDefinitionsResponse _
```

#### `ListDefinitionsResponse`

``` purescript
newtype ListDefinitionsResponse
  = ListDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

List of definition responses

##### Instances
``` purescript
Newtype ListDefinitionsResponse _
```

#### `ListDeploymentsRequest`

``` purescript
newtype ListDeploymentsRequest
  = ListDeploymentsRequest { "GroupId" :: String, "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListDeploymentsRequest _
```

#### `ListDeploymentsResponse`

``` purescript
newtype ListDeploymentsResponse
  = ListDeploymentsResponse { "Deployments" :: NullOrUndefined (Deployments), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListDeploymentsResponse _
```

#### `ListDeviceDefinitionVersionsRequest`

``` purescript
newtype ListDeviceDefinitionVersionsRequest
  = ListDeviceDefinitionVersionsRequest { "DeviceDefinitionId" :: String, "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListDeviceDefinitionVersionsRequest _
```

#### `ListDeviceDefinitionVersionsResponse`

``` purescript
newtype ListDeviceDefinitionVersionsResponse
  = ListDeviceDefinitionVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

##### Instances
``` purescript
Newtype ListDeviceDefinitionVersionsResponse _
```

#### `ListDeviceDefinitionsRequest`

``` purescript
newtype ListDeviceDefinitionsRequest
  = ListDeviceDefinitionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListDeviceDefinitionsRequest _
```

#### `ListDeviceDefinitionsResponse`

``` purescript
newtype ListDeviceDefinitionsResponse
  = ListDeviceDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListDeviceDefinitionsResponse _
```

#### `ListFunctionDefinitionVersionsRequest`

``` purescript
newtype ListFunctionDefinitionVersionsRequest
  = ListFunctionDefinitionVersionsRequest { "FunctionDefinitionId" :: String, "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListFunctionDefinitionVersionsRequest _
```

#### `ListFunctionDefinitionVersionsResponse`

``` purescript
newtype ListFunctionDefinitionVersionsResponse
  = ListFunctionDefinitionVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

##### Instances
``` purescript
Newtype ListFunctionDefinitionVersionsResponse _
```

#### `ListFunctionDefinitionsRequest`

``` purescript
newtype ListFunctionDefinitionsRequest
  = ListFunctionDefinitionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListFunctionDefinitionsRequest _
```

#### `ListFunctionDefinitionsResponse`

``` purescript
newtype ListFunctionDefinitionsResponse
  = ListFunctionDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListFunctionDefinitionsResponse _
```

#### `ListGroupCertificateAuthoritiesRequest`

``` purescript
newtype ListGroupCertificateAuthoritiesRequest
  = ListGroupCertificateAuthoritiesRequest { "GroupId" :: String }
```

##### Instances
``` purescript
Newtype ListGroupCertificateAuthoritiesRequest _
```

#### `ListGroupCertificateAuthoritiesResponse`

``` purescript
newtype ListGroupCertificateAuthoritiesResponse
  = ListGroupCertificateAuthoritiesResponse { "GroupCertificateAuthorities" :: NullOrUndefined (ListOfGroupCertificateAuthorityProperties) }
```

##### Instances
``` purescript
Newtype ListGroupCertificateAuthoritiesResponse _
```

#### `ListGroupVersionsRequest`

``` purescript
newtype ListGroupVersionsRequest
  = ListGroupVersionsRequest { "GroupId" :: String, "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListGroupVersionsRequest _
```

#### `ListGroupVersionsResponse`

``` purescript
newtype ListGroupVersionsResponse
  = ListGroupVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

##### Instances
``` purescript
Newtype ListGroupVersionsResponse _
```

#### `ListGroupsRequest`

``` purescript
newtype ListGroupsRequest
  = ListGroupsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListGroupsRequest _
```

#### `ListGroupsResponse`

``` purescript
newtype ListGroupsResponse
  = ListGroupsResponse { "Groups" :: NullOrUndefined (ListOfGroupInformation), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListGroupsResponse _
```

#### `ListLoggerDefinitionVersionsRequest`

``` purescript
newtype ListLoggerDefinitionVersionsRequest
  = ListLoggerDefinitionVersionsRequest { "LoggerDefinitionId" :: String, "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListLoggerDefinitionVersionsRequest _
```

#### `ListLoggerDefinitionVersionsResponse`

``` purescript
newtype ListLoggerDefinitionVersionsResponse
  = ListLoggerDefinitionVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

##### Instances
``` purescript
Newtype ListLoggerDefinitionVersionsResponse _
```

#### `ListLoggerDefinitionsRequest`

``` purescript
newtype ListLoggerDefinitionsRequest
  = ListLoggerDefinitionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListLoggerDefinitionsRequest _
```

#### `ListLoggerDefinitionsResponse`

``` purescript
newtype ListLoggerDefinitionsResponse
  = ListLoggerDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListLoggerDefinitionsResponse _
```

#### `ListOfConnectivityInfo`

``` purescript
newtype ListOfConnectivityInfo
  = ListOfConnectivityInfo (Array ConnectivityInfo)
```

##### Instances
``` purescript
Newtype ListOfConnectivityInfo _
```

#### `ListOfCore`

``` purescript
newtype ListOfCore
  = ListOfCore (Array Core)
```

##### Instances
``` purescript
Newtype ListOfCore _
```

#### `ListOfDefinitionInformation`

``` purescript
newtype ListOfDefinitionInformation
  = ListOfDefinitionInformation (Array DefinitionInformation)
```

##### Instances
``` purescript
Newtype ListOfDefinitionInformation _
```

#### `ListOfDevice`

``` purescript
newtype ListOfDevice
  = ListOfDevice (Array Device)
```

##### Instances
``` purescript
Newtype ListOfDevice _
```

#### `ListOfFunction`

``` purescript
newtype ListOfFunction
  = ListOfFunction (Array Function'')
```

##### Instances
``` purescript
Newtype ListOfFunction _
```

#### `ListOfGroupCertificateAuthorityProperties`

``` purescript
newtype ListOfGroupCertificateAuthorityProperties
  = ListOfGroupCertificateAuthorityProperties (Array GroupCertificateAuthorityProperties)
```

##### Instances
``` purescript
Newtype ListOfGroupCertificateAuthorityProperties _
```

#### `ListOfGroupInformation`

``` purescript
newtype ListOfGroupInformation
  = ListOfGroupInformation (Array GroupInformation)
```

##### Instances
``` purescript
Newtype ListOfGroupInformation _
```

#### `ListOfLogger`

``` purescript
newtype ListOfLogger
  = ListOfLogger (Array Logger)
```

##### Instances
``` purescript
Newtype ListOfLogger _
```

#### `ListOfResource`

``` purescript
newtype ListOfResource
  = ListOfResource (Array Resource)
```

##### Instances
``` purescript
Newtype ListOfResource _
```

#### `ListOfResourceAccessPolicy`

``` purescript
newtype ListOfResourceAccessPolicy
  = ListOfResourceAccessPolicy (Array ResourceAccessPolicy)
```

##### Instances
``` purescript
Newtype ListOfResourceAccessPolicy _
```

#### `ListOfSubscription`

``` purescript
newtype ListOfSubscription
  = ListOfSubscription (Array Subscription)
```

##### Instances
``` purescript
Newtype ListOfSubscription _
```

#### `ListOfVersionInformation`

``` purescript
newtype ListOfVersionInformation
  = ListOfVersionInformation (Array VersionInformation)
```

##### Instances
``` purescript
Newtype ListOfVersionInformation _
```

#### `ListResourceDefinitionVersionsRequest`

``` purescript
newtype ListResourceDefinitionVersionsRequest
  = ListResourceDefinitionVersionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String), "ResourceDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype ListResourceDefinitionVersionsRequest _
```

#### `ListResourceDefinitionVersionsResponse`

``` purescript
newtype ListResourceDefinitionVersionsResponse
  = ListResourceDefinitionVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

##### Instances
``` purescript
Newtype ListResourceDefinitionVersionsResponse _
```

#### `ListResourceDefinitionsRequest`

``` purescript
newtype ListResourceDefinitionsRequest
  = ListResourceDefinitionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListResourceDefinitionsRequest _
```

#### `ListResourceDefinitionsResponse`

``` purescript
newtype ListResourceDefinitionsResponse
  = ListResourceDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListResourceDefinitionsResponse _
```

#### `ListSubscriptionDefinitionVersionsRequest`

``` purescript
newtype ListSubscriptionDefinitionVersionsRequest
  = ListSubscriptionDefinitionVersionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String), "SubscriptionDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype ListSubscriptionDefinitionVersionsRequest _
```

#### `ListSubscriptionDefinitionVersionsResponse`

``` purescript
newtype ListSubscriptionDefinitionVersionsResponse
  = ListSubscriptionDefinitionVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

##### Instances
``` purescript
Newtype ListSubscriptionDefinitionVersionsResponse _
```

#### `ListSubscriptionDefinitionsRequest`

``` purescript
newtype ListSubscriptionDefinitionsRequest
  = ListSubscriptionDefinitionsRequest { "MaxResults" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListSubscriptionDefinitionsRequest _
```

#### `ListSubscriptionDefinitionsResponse`

``` purescript
newtype ListSubscriptionDefinitionsResponse
  = ListSubscriptionDefinitionsResponse { "Definitions" :: NullOrUndefined (ListOfDefinitionInformation), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListSubscriptionDefinitionsResponse _
```

#### `ListVersionsResponse`

``` purescript
newtype ListVersionsResponse
  = ListVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionInformation) }
```

List of versions response

##### Instances
``` purescript
Newtype ListVersionsResponse _
```

#### `LocalDeviceResourceData`

``` purescript
newtype LocalDeviceResourceData
  = LocalDeviceResourceData { "GroupOwnerSetting" :: NullOrUndefined (GroupOwnerSetting), "SourcePath" :: NullOrUndefined (String) }
```

Attributes that define the Local Device Resource.

##### Instances
``` purescript
Newtype LocalDeviceResourceData _
```

#### `LocalVolumeResourceData`

``` purescript
newtype LocalVolumeResourceData
  = LocalVolumeResourceData { "DestinationPath" :: NullOrUndefined (String), "GroupOwnerSetting" :: NullOrUndefined (GroupOwnerSetting), "SourcePath" :: NullOrUndefined (String) }
```

Attributes that define the Local Volume Resource.

##### Instances
``` purescript
Newtype LocalVolumeResourceData _
```

#### `Logger`

``` purescript
newtype Logger
  = Logger { "Component" :: NullOrUndefined (LoggerComponent), "Id" :: NullOrUndefined (String), "Level" :: NullOrUndefined (LoggerLevel), "Space" :: NullOrUndefined (Int), "Type" :: NullOrUndefined (LoggerType) }
```

Information on the Logger

##### Instances
``` purescript
Newtype Logger _
```

#### `LoggerComponent`

``` purescript
newtype LoggerComponent
  = LoggerComponent String
```

##### Instances
``` purescript
Newtype LoggerComponent _
```

#### `LoggerDefinitionVersion`

``` purescript
newtype LoggerDefinitionVersion
  = LoggerDefinitionVersion { "Loggers" :: NullOrUndefined (ListOfLogger) }
```

Information on logger definition version

##### Instances
``` purescript
Newtype LoggerDefinitionVersion _
```

#### `LoggerLevel`

``` purescript
newtype LoggerLevel
  = LoggerLevel String
```

##### Instances
``` purescript
Newtype LoggerLevel _
```

#### `LoggerType`

``` purescript
newtype LoggerType
  = LoggerType String
```

##### Instances
``` purescript
Newtype LoggerType _
```

#### `MapOf__string`

``` purescript
newtype MapOf__string
  = MapOf__string (Map String String)
```

##### Instances
``` purescript
Newtype MapOf__string _
```

#### `Permission`

``` purescript
newtype Permission
  = Permission String
```

Type of permissions a function could have to access a resource.

##### Instances
``` purescript
Newtype Permission _
```

#### `ResetDeploymentsRequest`

``` purescript
newtype ResetDeploymentsRequest
  = ResetDeploymentsRequest { "AmznClientToken" :: NullOrUndefined (String), "Force" :: NullOrUndefined (Boolean), "GroupId" :: String }
```

Information needed to perform a reset of a group's deployments.

##### Instances
``` purescript
Newtype ResetDeploymentsRequest _
```

#### `ResetDeploymentsResponse`

``` purescript
newtype ResetDeploymentsResponse
  = ResetDeploymentsResponse { "DeploymentArn" :: NullOrUndefined (String), "DeploymentId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ResetDeploymentsResponse _
```

#### `Resource`

``` purescript
newtype Resource
  = Resource { "Id" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "ResourceDataContainer" :: NullOrUndefined (ResourceDataContainer) }
```

Information on the resource.

##### Instances
``` purescript
Newtype Resource _
```

#### `ResourceAccessPolicy`

``` purescript
newtype ResourceAccessPolicy
  = ResourceAccessPolicy { "Permission" :: NullOrUndefined (Permission), "ResourceId" :: NullOrUndefined (String) }
```

Policy for the function to access a resource.

##### Instances
``` purescript
Newtype ResourceAccessPolicy _
```

#### `ResourceDataContainer`

``` purescript
newtype ResourceDataContainer
  = ResourceDataContainer { "LocalDeviceResourceData" :: NullOrUndefined (LocalDeviceResourceData), "LocalVolumeResourceData" :: NullOrUndefined (LocalVolumeResourceData) }
```

A container of data for all resource types.

##### Instances
``` purescript
Newtype ResourceDataContainer _
```

#### `ResourceDefinitionVersion`

``` purescript
newtype ResourceDefinitionVersion
  = ResourceDefinitionVersion { "Resources" :: NullOrUndefined (ListOfResource) }
```

Information on resource definition version

##### Instances
``` purescript
Newtype ResourceDefinitionVersion _
```

#### `S3UrlSignerRole`

``` purescript
newtype S3UrlSignerRole
  = S3UrlSignerRole String
```

The IAM Role that Greengrass will use to create pre-signed URLs pointing towards the update artifact.

##### Instances
``` purescript
Newtype S3UrlSignerRole _
```

#### `SoftwareToUpdate`

``` purescript
newtype SoftwareToUpdate
  = SoftwareToUpdate String
```

The piece of software on the Greengrass Core that will be updated.

##### Instances
``` purescript
Newtype SoftwareToUpdate _
```

#### `Subscription`

``` purescript
newtype Subscription
  = Subscription { "Id" :: NullOrUndefined (String), "Source" :: NullOrUndefined (String), "Subject" :: NullOrUndefined (String), "Target" :: NullOrUndefined (String) }
```

Information on subscription

##### Instances
``` purescript
Newtype Subscription _
```

#### `SubscriptionDefinitionVersion`

``` purescript
newtype SubscriptionDefinitionVersion
  = SubscriptionDefinitionVersion { "Subscriptions" :: NullOrUndefined (ListOfSubscription) }
```

Information on subscription definition version

##### Instances
``` purescript
Newtype SubscriptionDefinitionVersion _
```

#### `UpdateAgentLogLevel`

``` purescript
newtype UpdateAgentLogLevel
  = UpdateAgentLogLevel String
```

The minimum level of log statements that should be logged by the OTA Agent during an update.

##### Instances
``` purescript
Newtype UpdateAgentLogLevel _
```

#### `UpdateConnectivityInfoRequest`

``` purescript
newtype UpdateConnectivityInfoRequest
  = UpdateConnectivityInfoRequest { "ConnectivityInfo" :: NullOrUndefined (ListOfConnectivityInfo), "ThingName" :: String }
```

connectivity info request

##### Instances
``` purescript
Newtype UpdateConnectivityInfoRequest _
```

#### `UpdateConnectivityInfoResponse`

``` purescript
newtype UpdateConnectivityInfoResponse
  = UpdateConnectivityInfoResponse { "Message" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateConnectivityInfoResponse _
```

#### `UpdateCoreDefinitionRequest`

``` purescript
newtype UpdateCoreDefinitionRequest
  = UpdateCoreDefinitionRequest { "CoreDefinitionId" :: String, "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateCoreDefinitionRequest _
```

#### `UpdateCoreDefinitionResponse`

``` purescript
newtype UpdateCoreDefinitionResponse
  = UpdateCoreDefinitionResponse {  }
```

##### Instances
``` purescript
Newtype UpdateCoreDefinitionResponse _
```

#### `UpdateDeviceDefinitionRequest`

``` purescript
newtype UpdateDeviceDefinitionRequest
  = UpdateDeviceDefinitionRequest { "DeviceDefinitionId" :: String, "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateDeviceDefinitionRequest _
```

#### `UpdateDeviceDefinitionResponse`

``` purescript
newtype UpdateDeviceDefinitionResponse
  = UpdateDeviceDefinitionResponse {  }
```

##### Instances
``` purescript
Newtype UpdateDeviceDefinitionResponse _
```

#### `UpdateFunctionDefinitionRequest`

``` purescript
newtype UpdateFunctionDefinitionRequest
  = UpdateFunctionDefinitionRequest { "FunctionDefinitionId" :: String, "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateFunctionDefinitionRequest _
```

#### `UpdateFunctionDefinitionResponse`

``` purescript
newtype UpdateFunctionDefinitionResponse
  = UpdateFunctionDefinitionResponse {  }
```

##### Instances
``` purescript
Newtype UpdateFunctionDefinitionResponse _
```

#### `UpdateGroupCertificateConfigurationRequest`

``` purescript
newtype UpdateGroupCertificateConfigurationRequest
  = UpdateGroupCertificateConfigurationRequest { "CertificateExpiryInMilliseconds" :: NullOrUndefined (String), "GroupId" :: String }
```

##### Instances
``` purescript
Newtype UpdateGroupCertificateConfigurationRequest _
```

#### `UpdateGroupCertificateConfigurationResponse`

``` purescript
newtype UpdateGroupCertificateConfigurationResponse
  = UpdateGroupCertificateConfigurationResponse { "CertificateAuthorityExpiryInMilliseconds" :: NullOrUndefined (String), "CertificateExpiryInMilliseconds" :: NullOrUndefined (String), "GroupId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateGroupCertificateConfigurationResponse _
```

#### `UpdateGroupRequest`

``` purescript
newtype UpdateGroupRequest
  = UpdateGroupRequest { "GroupId" :: String, "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateGroupRequest _
```

#### `UpdateGroupResponse`

``` purescript
newtype UpdateGroupResponse
  = UpdateGroupResponse {  }
```

##### Instances
``` purescript
Newtype UpdateGroupResponse _
```

#### `UpdateLoggerDefinitionRequest`

``` purescript
newtype UpdateLoggerDefinitionRequest
  = UpdateLoggerDefinitionRequest { "LoggerDefinitionId" :: String, "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateLoggerDefinitionRequest _
```

#### `UpdateLoggerDefinitionResponse`

``` purescript
newtype UpdateLoggerDefinitionResponse
  = UpdateLoggerDefinitionResponse {  }
```

##### Instances
``` purescript
Newtype UpdateLoggerDefinitionResponse _
```

#### `UpdateResourceDefinitionRequest`

``` purescript
newtype UpdateResourceDefinitionRequest
  = UpdateResourceDefinitionRequest { "Name" :: NullOrUndefined (String), "ResourceDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype UpdateResourceDefinitionRequest _
```

#### `UpdateResourceDefinitionResponse`

``` purescript
newtype UpdateResourceDefinitionResponse
  = UpdateResourceDefinitionResponse {  }
```

##### Instances
``` purescript
Newtype UpdateResourceDefinitionResponse _
```

#### `UpdateSubscriptionDefinitionRequest`

``` purescript
newtype UpdateSubscriptionDefinitionRequest
  = UpdateSubscriptionDefinitionRequest { "Name" :: NullOrUndefined (String), "SubscriptionDefinitionId" :: String }
```

##### Instances
``` purescript
Newtype UpdateSubscriptionDefinitionRequest _
```

#### `UpdateSubscriptionDefinitionResponse`

``` purescript
newtype UpdateSubscriptionDefinitionResponse
  = UpdateSubscriptionDefinitionResponse {  }
```

##### Instances
``` purescript
Newtype UpdateSubscriptionDefinitionResponse _
```

#### `UpdateTargets`

``` purescript
newtype UpdateTargets
  = UpdateTargets (Array String)
```

The target arns that this update will be applied to.

##### Instances
``` purescript
Newtype UpdateTargets _
```

#### `UpdateTargetsArchitecture`

``` purescript
newtype UpdateTargetsArchitecture
  = UpdateTargetsArchitecture String
```

The architecture of the Cores in the targets of an update

##### Instances
``` purescript
Newtype UpdateTargetsArchitecture _
```

#### `UpdateTargetsOperatingSystem`

``` purescript
newtype UpdateTargetsOperatingSystem
  = UpdateTargetsOperatingSystem String
```

The operating system of the Cores in the targets of an update

##### Instances
``` purescript
Newtype UpdateTargetsOperatingSystem _
```

#### `VersionInformation`

``` purescript
newtype VersionInformation
  = VersionInformation { "Arn" :: NullOrUndefined (String), "CreationTimestamp" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

Information on the version

##### Instances
``` purescript
Newtype VersionInformation _
```


