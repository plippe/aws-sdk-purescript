## Module AWS.MQ

Amazon MQ is a managed message broker service for Apache ActiveMQ that makes it easy to set up and operate message brokers in the cloud. A message broker allows software applications and components to communicate using various programming languages, operating systems, and formal messaging protocols.

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createBroker`

``` purescript
createBroker :: forall eff. CreateBrokerRequest -> Aff (err :: RequestError | eff) CreateBrokerResponse
```

Creates a broker. Note: This API is asynchronous.

#### `createConfiguration`

``` purescript
createConfiguration :: forall eff. CreateConfigurationRequest -> Aff (err :: RequestError | eff) CreateConfigurationResponse
```

Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version). Note: If the configuration name already exists, Amazon MQ doesn't create a configuration.

#### `createUser`

``` purescript
createUser :: forall eff. CreateUserRequest -> Aff (err :: RequestError | eff) CreateUserResponse
```

Creates an ActiveMQ user.

#### `deleteBroker`

``` purescript
deleteBroker :: forall eff. DeleteBrokerRequest -> Aff (err :: RequestError | eff) DeleteBrokerResponse
```

Deletes a broker. Note: This API is asynchronous.

#### `deleteUser`

``` purescript
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: RequestError | eff) DeleteUserResponse
```

Deletes an ActiveMQ user.

#### `describeBroker`

``` purescript
describeBroker :: forall eff. DescribeBrokerRequest -> Aff (err :: RequestError | eff) DescribeBrokerResponse
```

Returns information about the specified broker.

#### `describeConfiguration`

``` purescript
describeConfiguration :: forall eff. DescribeConfigurationRequest -> Aff (err :: RequestError | eff) DescribeConfigurationResponse
```

Returns information about the specified configuration.

#### `describeConfigurationRevision`

``` purescript
describeConfigurationRevision :: forall eff. DescribeConfigurationRevisionRequest -> Aff (err :: RequestError | eff) DescribeConfigurationRevisionResponse
```

Returns the specified configuration revision for the specified configuration.

#### `describeUser`

``` purescript
describeUser :: forall eff. DescribeUserRequest -> Aff (err :: RequestError | eff) DescribeUserResponse
```

Returns information about an ActiveMQ user.

#### `listBrokers`

``` purescript
listBrokers :: forall eff. ListBrokersRequest -> Aff (err :: RequestError | eff) ListBrokersResponse
```

Returns a list of all brokers.

#### `listConfigurationRevisions`

``` purescript
listConfigurationRevisions :: forall eff. ListConfigurationRevisionsRequest -> Aff (err :: RequestError | eff) ListConfigurationRevisionsResponse
```

Returns a list of all revisions for the specified configuration.

#### `listConfigurations`

``` purescript
listConfigurations :: forall eff. ListConfigurationsRequest -> Aff (err :: RequestError | eff) ListConfigurationsResponse
```

Returns a list of all configurations.

#### `listUsers`

``` purescript
listUsers :: forall eff. ListUsersRequest -> Aff (err :: RequestError | eff) ListUsersResponse
```

Returns a list of all ActiveMQ users.

#### `rebootBroker`

``` purescript
rebootBroker :: forall eff. RebootBrokerRequest -> Aff (err :: RequestError | eff) RebootBrokerResponse
```

Reboots a broker. Note: This API is asynchronous.

#### `updateBroker`

``` purescript
updateBroker :: forall eff. UpdateBrokerRequest -> Aff (err :: RequestError | eff) UpdateBrokerResponse
```

Adds a pending configuration change to a broker.

#### `updateConfiguration`

``` purescript
updateConfiguration :: forall eff. UpdateConfigurationRequest -> Aff (err :: RequestError | eff) UpdateConfigurationResponse
```

Updates the specified configuration.

#### `updateUser`

``` purescript
updateUser :: forall eff. UpdateUserRequest -> Aff (err :: RequestError | eff) UpdateUserResponse
```

Updates the information for an ActiveMQ user.

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

##### Instances
``` purescript
Newtype BadRequestException _
```

#### `BrokerInstance`

``` purescript
newtype BrokerInstance
  = BrokerInstance { "ConsoleURL" :: NullOrUndefined (String), "Endpoints" :: NullOrUndefined (ListOf__string) }
```

Returns information about all brokers.

##### Instances
``` purescript
Newtype BrokerInstance _
```

#### `BrokerState`

``` purescript
newtype BrokerState
  = BrokerState String
```

The status of the broker. Possible values: CREATION_IN_PROGRESS, CREATION_FAILED, DELETION_IN_PROGRESS, RUNNING, REBOOT_IN_PROGRESS

##### Instances
``` purescript
Newtype BrokerState _
```

#### `BrokerSummary`

``` purescript
newtype BrokerSummary
  = BrokerSummary { "BrokerArn" :: NullOrUndefined (String), "BrokerId" :: NullOrUndefined (String), "BrokerName" :: NullOrUndefined (String), "BrokerState" :: NullOrUndefined (BrokerState), "DeploymentMode" :: NullOrUndefined (DeploymentMode), "HostInstanceType" :: NullOrUndefined (String) }
```

The Amazon Resource Name (ARN) of the broker.

##### Instances
``` purescript
Newtype BrokerSummary _
```

#### `ChangeType`

``` purescript
newtype ChangeType
  = ChangeType String
```

The type of change pending for the ActiveMQ user. Possible values: CREATE, UPDATE, DELETE

##### Instances
``` purescript
Newtype ChangeType _
```

#### `Configuration`

``` purescript
newtype Configuration
  = Configuration { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LatestRevision" :: NullOrUndefined (ConfigurationRevision), "Name" :: NullOrUndefined (String) }
```

Returns information about all configurations.

##### Instances
``` purescript
Newtype Configuration _
```

#### `ConfigurationId`

``` purescript
newtype ConfigurationId
  = ConfigurationId { "Id" :: NullOrUndefined (String), "Revision" :: NullOrUndefined (Int) }
```

A list of information about the configuration.

##### Instances
``` purescript
Newtype ConfigurationId _
```

#### `ConfigurationRevision`

``` purescript
newtype ConfigurationRevision
  = ConfigurationRevision { "Description" :: NullOrUndefined (String), "Revision" :: NullOrUndefined (Int) }
```

Returns information about the specified configuration revision.

##### Instances
``` purescript
Newtype ConfigurationRevision _
```

#### `Configurations`

``` purescript
newtype Configurations
  = Configurations { "Current" :: NullOrUndefined (ConfigurationId), "History" :: NullOrUndefined (ListOfConfigurationId), "Pending" :: NullOrUndefined (ConfigurationId) }
```

Broker configuration information

##### Instances
``` purescript
Newtype Configurations _
```

#### `ConflictException`

``` purescript
newtype ConflictException
  = ConflictException { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

##### Instances
``` purescript
Newtype ConflictException _
```

#### `CreateBrokerInput`

``` purescript
newtype CreateBrokerInput
  = CreateBrokerInput { "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean), "BrokerName" :: NullOrUndefined (String), "Configuration" :: NullOrUndefined (ConfigurationId), "CreatorRequestId" :: NullOrUndefined (String), "DeploymentMode" :: NullOrUndefined (DeploymentMode), "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "HostInstanceType" :: NullOrUndefined (String), "MaintenanceWindowStartTime" :: NullOrUndefined (WeeklyStartTime), "PubliclyAccessible" :: NullOrUndefined (Boolean), "SecurityGroups" :: NullOrUndefined (ListOf__string), "SubnetIds" :: NullOrUndefined (ListOf__string), "Users" :: NullOrUndefined (ListOfUser) }
```

Required. The time period during which Amazon MQ applies pending updates or patches to the broker.

##### Instances
``` purescript
Newtype CreateBrokerInput _
```

#### `CreateBrokerOutput`

``` purescript
newtype CreateBrokerOutput
  = CreateBrokerOutput { "BrokerArn" :: NullOrUndefined (String), "BrokerId" :: NullOrUndefined (String) }
```

Returns information about the created broker.

##### Instances
``` purescript
Newtype CreateBrokerOutput _
```

#### `CreateBrokerRequest`

``` purescript
newtype CreateBrokerRequest
  = CreateBrokerRequest { "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean), "BrokerName" :: NullOrUndefined (String), "Configuration" :: NullOrUndefined (ConfigurationId), "CreatorRequestId" :: NullOrUndefined (String), "DeploymentMode" :: NullOrUndefined (DeploymentMode), "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "HostInstanceType" :: NullOrUndefined (String), "MaintenanceWindowStartTime" :: NullOrUndefined (WeeklyStartTime), "PubliclyAccessible" :: NullOrUndefined (Boolean), "SecurityGroups" :: NullOrUndefined (ListOf__string), "SubnetIds" :: NullOrUndefined (ListOf__string), "Users" :: NullOrUndefined (ListOfUser) }
```

Creates a broker using the specified properties.

##### Instances
``` purescript
Newtype CreateBrokerRequest _
```

#### `CreateBrokerResponse`

``` purescript
newtype CreateBrokerResponse
  = CreateBrokerResponse { "BrokerArn" :: NullOrUndefined (String), "BrokerId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateBrokerResponse _
```

#### `CreateConfigurationInput`

``` purescript
newtype CreateConfigurationInput
  = CreateConfigurationInput { "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version). Note: If the configuration name already exists, Amazon MQ doesn't create a configuration.

##### Instances
``` purescript
Newtype CreateConfigurationInput _
```

#### `CreateConfigurationOutput`

``` purescript
newtype CreateConfigurationOutput
  = CreateConfigurationOutput { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LatestRevision" :: NullOrUndefined (ConfigurationRevision), "Name" :: NullOrUndefined (String) }
```

Returns information about the created configuration.

##### Instances
``` purescript
Newtype CreateConfigurationOutput _
```

#### `CreateConfigurationRequest`

``` purescript
newtype CreateConfigurationRequest
  = CreateConfigurationRequest { "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version). Note: If the configuration name already exists, Amazon MQ doesn't create a configuration.

##### Instances
``` purescript
Newtype CreateConfigurationRequest _
```

#### `CreateConfigurationResponse`

``` purescript
newtype CreateConfigurationResponse
  = CreateConfigurationResponse { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LatestRevision" :: NullOrUndefined (ConfigurationRevision), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateConfigurationResponse _
```

#### `CreateUserInput`

``` purescript
newtype CreateUserInput
  = CreateUserInput { "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Password" :: NullOrUndefined (String) }
```

Creates a new ActiveMQ user.

##### Instances
``` purescript
Newtype CreateUserInput _
```

#### `CreateUserRequest`

``` purescript
newtype CreateUserRequest
  = CreateUserRequest { "BrokerId" :: String, "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Password" :: NullOrUndefined (String), "Username" :: String }
```

Creates a new ActiveMQ user.

##### Instances
``` purescript
Newtype CreateUserRequest _
```

#### `CreateUserResponse`

``` purescript
newtype CreateUserResponse
  = CreateUserResponse {  }
```

##### Instances
``` purescript
Newtype CreateUserResponse _
```

#### `DayOfWeek`

``` purescript
newtype DayOfWeek
  = DayOfWeek String
```

##### Instances
``` purescript
Newtype DayOfWeek _
```

#### `DeleteBrokerOutput`

``` purescript
newtype DeleteBrokerOutput
  = DeleteBrokerOutput { "BrokerId" :: NullOrUndefined (String) }
```

Returns information about the deleted broker.

##### Instances
``` purescript
Newtype DeleteBrokerOutput _
```

#### `DeleteBrokerRequest`

``` purescript
newtype DeleteBrokerRequest
  = DeleteBrokerRequest { "BrokerId" :: String }
```

##### Instances
``` purescript
Newtype DeleteBrokerRequest _
```

#### `DeleteBrokerResponse`

``` purescript
newtype DeleteBrokerResponse
  = DeleteBrokerResponse { "BrokerId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DeleteBrokerResponse _
```

#### `DeleteUserRequest`

``` purescript
newtype DeleteUserRequest
  = DeleteUserRequest { "BrokerId" :: String, "Username" :: String }
```

##### Instances
``` purescript
Newtype DeleteUserRequest _
```

#### `DeleteUserResponse`

``` purescript
newtype DeleteUserResponse
  = DeleteUserResponse {  }
```

##### Instances
``` purescript
Newtype DeleteUserResponse _
```

#### `DeploymentMode`

``` purescript
newtype DeploymentMode
  = DeploymentMode String
```

The deployment mode of the broker. Possible values: SINGLE_INSTANCE, ACTIVE_STANDBY_MULTI_AZ SINGLE_INSTANCE creates a single-instance broker in a single Availability Zone. ACTIVE_STANDBY_MULTI_AZ creates an active/standby broker for high availability.

##### Instances
``` purescript
Newtype DeploymentMode _
```

#### `DescribeBrokerOutput`

``` purescript
newtype DescribeBrokerOutput
  = DescribeBrokerOutput { "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean), "BrokerArn" :: NullOrUndefined (String), "BrokerId" :: NullOrUndefined (String), "BrokerInstances" :: NullOrUndefined (ListOfBrokerInstance), "BrokerName" :: NullOrUndefined (String), "BrokerState" :: NullOrUndefined (BrokerState), "Configurations" :: NullOrUndefined (Configurations), "DeploymentMode" :: NullOrUndefined (DeploymentMode), "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "HostInstanceType" :: NullOrUndefined (String), "MaintenanceWindowStartTime" :: NullOrUndefined (WeeklyStartTime), "PubliclyAccessible" :: NullOrUndefined (Boolean), "SecurityGroups" :: NullOrUndefined (ListOf__string), "SubnetIds" :: NullOrUndefined (ListOf__string), "Users" :: NullOrUndefined (ListOfUserSummary) }
```

The version of the broker engine. Note: Currently, Amazon MQ supports only 5.15.0.

##### Instances
``` purescript
Newtype DescribeBrokerOutput _
```

#### `DescribeBrokerRequest`

``` purescript
newtype DescribeBrokerRequest
  = DescribeBrokerRequest { "BrokerId" :: String }
```

##### Instances
``` purescript
Newtype DescribeBrokerRequest _
```

#### `DescribeBrokerResponse`

``` purescript
newtype DescribeBrokerResponse
  = DescribeBrokerResponse { "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean), "BrokerArn" :: NullOrUndefined (String), "BrokerId" :: NullOrUndefined (String), "BrokerInstances" :: NullOrUndefined (ListOfBrokerInstance), "BrokerName" :: NullOrUndefined (String), "BrokerState" :: NullOrUndefined (BrokerState), "Configurations" :: NullOrUndefined (Configurations), "DeploymentMode" :: NullOrUndefined (DeploymentMode), "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "HostInstanceType" :: NullOrUndefined (String), "MaintenanceWindowStartTime" :: NullOrUndefined (WeeklyStartTime), "PubliclyAccessible" :: NullOrUndefined (Boolean), "SecurityGroups" :: NullOrUndefined (ListOf__string), "SubnetIds" :: NullOrUndefined (ListOf__string), "Users" :: NullOrUndefined (ListOfUserSummary) }
```

##### Instances
``` purescript
Newtype DescribeBrokerResponse _
```

#### `DescribeConfigurationRequest`

``` purescript
newtype DescribeConfigurationRequest
  = DescribeConfigurationRequest { "ConfigurationId" :: String }
```

##### Instances
``` purescript
Newtype DescribeConfigurationRequest _
```

#### `DescribeConfigurationResponse`

``` purescript
newtype DescribeConfigurationResponse
  = DescribeConfigurationResponse { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LatestRevision" :: NullOrUndefined (ConfigurationRevision), "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeConfigurationResponse _
```

#### `DescribeConfigurationRevisionOutput`

``` purescript
newtype DescribeConfigurationRevisionOutput
  = DescribeConfigurationRevisionOutput { "ConfigurationId" :: NullOrUndefined (String), "Data" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

Returns the specified configuration revision for the specified configuration.

##### Instances
``` purescript
Newtype DescribeConfigurationRevisionOutput _
```

#### `DescribeConfigurationRevisionRequest`

``` purescript
newtype DescribeConfigurationRevisionRequest
  = DescribeConfigurationRevisionRequest { "ConfigurationId" :: String, "ConfigurationRevision" :: String }
```

##### Instances
``` purescript
Newtype DescribeConfigurationRevisionRequest _
```

#### `DescribeConfigurationRevisionResponse`

``` purescript
newtype DescribeConfigurationRevisionResponse
  = DescribeConfigurationRevisionResponse { "ConfigurationId" :: NullOrUndefined (String), "Data" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeConfigurationRevisionResponse _
```

#### `DescribeUserOutput`

``` purescript
newtype DescribeUserOutput
  = DescribeUserOutput { "BrokerId" :: NullOrUndefined (String), "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Pending" :: NullOrUndefined (UserPendingChanges), "Username" :: NullOrUndefined (String) }
```

Returns information about an ActiveMQ user.

##### Instances
``` purescript
Newtype DescribeUserOutput _
```

#### `DescribeUserRequest`

``` purescript
newtype DescribeUserRequest
  = DescribeUserRequest { "BrokerId" :: String, "Username" :: String }
```

##### Instances
``` purescript
Newtype DescribeUserRequest _
```

#### `DescribeUserResponse`

``` purescript
newtype DescribeUserResponse
  = DescribeUserResponse { "BrokerId" :: NullOrUndefined (String), "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Pending" :: NullOrUndefined (UserPendingChanges), "Username" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeUserResponse _
```

#### `EngineType`

``` purescript
newtype EngineType
  = EngineType String
```

The type of broker engine. Note: Currently, Amazon MQ supports only ActiveMQ.

##### Instances
``` purescript
Newtype EngineType _
```

#### `Error`

``` purescript
newtype Error
  = Error { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

##### Instances
``` purescript
Newtype Error _
```

#### `ForbiddenException`

``` purescript
newtype ForbiddenException
  = ForbiddenException { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

##### Instances
``` purescript
Newtype ForbiddenException _
```

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

##### Instances
``` purescript
Newtype InternalServerErrorException _
```

#### `ListBrokersOutput`

``` purescript
newtype ListBrokersOutput
  = ListBrokersOutput { "BrokerSummaries" :: NullOrUndefined (ListOfBrokerSummary), "NextToken" :: NullOrUndefined (String) }
```

A list of information about all brokers.

##### Instances
``` purescript
Newtype ListBrokersOutput _
```

#### `ListBrokersRequest`

``` purescript
newtype ListBrokersRequest
  = ListBrokersRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListBrokersRequest _
```

#### `ListBrokersResponse`

``` purescript
newtype ListBrokersResponse
  = ListBrokersResponse { "BrokerSummaries" :: NullOrUndefined (ListOfBrokerSummary), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListBrokersResponse _
```

#### `ListConfigurationRevisionsOutput`

``` purescript
newtype ListConfigurationRevisionsOutput
  = ListConfigurationRevisionsOutput { "ConfigurationId" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Revisions" :: NullOrUndefined (ListOfConfigurationRevision) }
```

Returns a list of all revisions for the specified configuration.

##### Instances
``` purescript
Newtype ListConfigurationRevisionsOutput _
```

#### `ListConfigurationRevisionsRequest`

``` purescript
newtype ListConfigurationRevisionsRequest
  = ListConfigurationRevisionsRequest { "ConfigurationId" :: String, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListConfigurationRevisionsRequest _
```

#### `ListConfigurationRevisionsResponse`

``` purescript
newtype ListConfigurationRevisionsResponse
  = ListConfigurationRevisionsResponse { "ConfigurationId" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Revisions" :: NullOrUndefined (ListOfConfigurationRevision) }
```

##### Instances
``` purescript
Newtype ListConfigurationRevisionsResponse _
```

#### `ListConfigurationsOutput`

``` purescript
newtype ListConfigurationsOutput
  = ListConfigurationsOutput { "Configurations" :: NullOrUndefined (ListOfConfiguration), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

Returns a list of all configurations.

##### Instances
``` purescript
Newtype ListConfigurationsOutput _
```

#### `ListConfigurationsRequest`

``` purescript
newtype ListConfigurationsRequest
  = ListConfigurationsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListConfigurationsRequest _
```

#### `ListConfigurationsResponse`

``` purescript
newtype ListConfigurationsResponse
  = ListConfigurationsResponse { "Configurations" :: NullOrUndefined (ListOfConfiguration), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListConfigurationsResponse _
```

#### `ListOfBrokerInstance`

``` purescript
newtype ListOfBrokerInstance
  = ListOfBrokerInstance (Array BrokerInstance)
```

##### Instances
``` purescript
Newtype ListOfBrokerInstance _
```

#### `ListOfBrokerSummary`

``` purescript
newtype ListOfBrokerSummary
  = ListOfBrokerSummary (Array BrokerSummary)
```

##### Instances
``` purescript
Newtype ListOfBrokerSummary _
```

#### `ListOfConfiguration`

``` purescript
newtype ListOfConfiguration
  = ListOfConfiguration (Array Configuration)
```

##### Instances
``` purescript
Newtype ListOfConfiguration _
```

#### `ListOfConfigurationId`

``` purescript
newtype ListOfConfigurationId
  = ListOfConfigurationId (Array ConfigurationId)
```

##### Instances
``` purescript
Newtype ListOfConfigurationId _
```

#### `ListOfConfigurationRevision`

``` purescript
newtype ListOfConfigurationRevision
  = ListOfConfigurationRevision (Array ConfigurationRevision)
```

##### Instances
``` purescript
Newtype ListOfConfigurationRevision _
```

#### `ListOfSanitizationWarning`

``` purescript
newtype ListOfSanitizationWarning
  = ListOfSanitizationWarning (Array SanitizationWarning)
```

##### Instances
``` purescript
Newtype ListOfSanitizationWarning _
```

#### `ListOfUser`

``` purescript
newtype ListOfUser
  = ListOfUser (Array User)
```

##### Instances
``` purescript
Newtype ListOfUser _
```

#### `ListOfUserSummary`

``` purescript
newtype ListOfUserSummary
  = ListOfUserSummary (Array UserSummary)
```

##### Instances
``` purescript
Newtype ListOfUserSummary _
```

#### `ListOf__string`

``` purescript
newtype ListOf__string
  = ListOf__string (Array String)
```

##### Instances
``` purescript
Newtype ListOf__string _
```

#### `ListUsersOutput`

``` purescript
newtype ListUsersOutput
  = ListUsersOutput { "BrokerId" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Users" :: NullOrUndefined (ListOfUserSummary) }
```

Returns a list of all ActiveMQ users.

##### Instances
``` purescript
Newtype ListUsersOutput _
```

#### `ListUsersRequest`

``` purescript
newtype ListUsersRequest
  = ListUsersRequest { "BrokerId" :: String, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListUsersRequest _
```

#### `ListUsersResponse`

``` purescript
newtype ListUsersResponse
  = ListUsersResponse { "BrokerId" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Users" :: NullOrUndefined (ListOfUserSummary) }
```

##### Instances
``` purescript
Newtype ListUsersResponse _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

##### Instances
``` purescript
Newtype MaxResults _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `RebootBrokerRequest`

``` purescript
newtype RebootBrokerRequest
  = RebootBrokerRequest { "BrokerId" :: String }
```

##### Instances
``` purescript
Newtype RebootBrokerRequest _
```

#### `RebootBrokerResponse`

``` purescript
newtype RebootBrokerResponse
  = RebootBrokerResponse {  }
```

##### Instances
``` purescript
Newtype RebootBrokerResponse _
```

#### `SanitizationWarning`

``` purescript
newtype SanitizationWarning
  = SanitizationWarning { "AttributeName" :: NullOrUndefined (String), "ElementName" :: NullOrUndefined (String), "Reason" :: NullOrUndefined (SanitizationWarningReason) }
```

Returns information about the XML element or attribute that was sanitized in the configuration.

##### Instances
``` purescript
Newtype SanitizationWarning _
```

#### `SanitizationWarningReason`

``` purescript
newtype SanitizationWarningReason
  = SanitizationWarningReason String
```

The reason for which the XML elements or attributes were sanitized. Possible values: DISALLOWED_ELEMENT_REMOVED, DISALLOWED_ATTRIBUTE_REMOVED, INVALID_ATTRIBUTE_VALUE_REMOVED DISALLOWED_ELEMENT_REMOVED shows that the provided element isn't allowed and has been removed. DISALLOWED_ATTRIBUTE_REMOVED shows that the provided attribute isn't allowed and has been removed. INVALID_ATTRIBUTE_VALUE_REMOVED shows that the provided value for the attribute isn't allowed and has been removed.

##### Instances
``` purescript
Newtype SanitizationWarningReason _
```

#### `UnauthorizedException`

``` purescript
newtype UnauthorizedException
  = UnauthorizedException { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

##### Instances
``` purescript
Newtype UnauthorizedException _
```

#### `UpdateBrokerInput`

``` purescript
newtype UpdateBrokerInput
  = UpdateBrokerInput { "Configuration" :: NullOrUndefined (ConfigurationId) }
```

Updates the broker using the specified properties.

##### Instances
``` purescript
Newtype UpdateBrokerInput _
```

#### `UpdateBrokerOutput`

``` purescript
newtype UpdateBrokerOutput
  = UpdateBrokerOutput { "BrokerId" :: NullOrUndefined (String), "Configuration" :: NullOrUndefined (ConfigurationId) }
```

Returns information about the updated broker.

##### Instances
``` purescript
Newtype UpdateBrokerOutput _
```

#### `UpdateBrokerRequest`

``` purescript
newtype UpdateBrokerRequest
  = UpdateBrokerRequest { "BrokerId" :: String, "Configuration" :: NullOrUndefined (ConfigurationId) }
```

Updates the broker using the specified properties.

##### Instances
``` purescript
Newtype UpdateBrokerRequest _
```

#### `UpdateBrokerResponse`

``` purescript
newtype UpdateBrokerResponse
  = UpdateBrokerResponse { "BrokerId" :: NullOrUndefined (String), "Configuration" :: NullOrUndefined (ConfigurationId) }
```

##### Instances
``` purescript
Newtype UpdateBrokerResponse _
```

#### `UpdateConfigurationInput`

``` purescript
newtype UpdateConfigurationInput
  = UpdateConfigurationInput { "Data" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

Updates the specified configuration.

##### Instances
``` purescript
Newtype UpdateConfigurationInput _
```

#### `UpdateConfigurationOutput`

``` purescript
newtype UpdateConfigurationOutput
  = UpdateConfigurationOutput { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LatestRevision" :: NullOrUndefined (ConfigurationRevision), "Name" :: NullOrUndefined (String), "Warnings" :: NullOrUndefined (ListOfSanitizationWarning) }
```

Returns information about the updated configuration.

##### Instances
``` purescript
Newtype UpdateConfigurationOutput _
```

#### `UpdateConfigurationRequest`

``` purescript
newtype UpdateConfigurationRequest
  = UpdateConfigurationRequest { "ConfigurationId" :: String, "Data" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

Updates the specified configuration.

##### Instances
``` purescript
Newtype UpdateConfigurationRequest _
```

#### `UpdateConfigurationResponse`

``` purescript
newtype UpdateConfigurationResponse
  = UpdateConfigurationResponse { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LatestRevision" :: NullOrUndefined (ConfigurationRevision), "Name" :: NullOrUndefined (String), "Warnings" :: NullOrUndefined (ListOfSanitizationWarning) }
```

##### Instances
``` purescript
Newtype UpdateConfigurationResponse _
```

#### `UpdateUserInput`

``` purescript
newtype UpdateUserInput
  = UpdateUserInput { "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Password" :: NullOrUndefined (String) }
```

Updates the information for an ActiveMQ user.

##### Instances
``` purescript
Newtype UpdateUserInput _
```

#### `UpdateUserRequest`

``` purescript
newtype UpdateUserRequest
  = UpdateUserRequest { "BrokerId" :: String, "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Password" :: NullOrUndefined (String), "Username" :: String }
```

Updates the information for an ActiveMQ user.

##### Instances
``` purescript
Newtype UpdateUserRequest _
```

#### `UpdateUserResponse`

``` purescript
newtype UpdateUserResponse
  = UpdateUserResponse {  }
```

##### Instances
``` purescript
Newtype UpdateUserResponse _
```

#### `User`

``` purescript
newtype User
  = User { "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Password" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String) }
```

An ActiveMQ user associated with the broker.

##### Instances
``` purescript
Newtype User _
```

#### `UserPendingChanges`

``` purescript
newtype UserPendingChanges
  = UserPendingChanges { "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "PendingChange" :: NullOrUndefined (ChangeType) }
```

Returns information about the status of the changes pending for the ActiveMQ user.

##### Instances
``` purescript
Newtype UserPendingChanges _
```

#### `UserSummary`

``` purescript
newtype UserSummary
  = UserSummary { "PendingChange" :: NullOrUndefined (ChangeType), "Username" :: NullOrUndefined (String) }
```

Returns a list of all ActiveMQ users.

##### Instances
``` purescript
Newtype UserSummary _
```

#### `WeeklyStartTime`

``` purescript
newtype WeeklyStartTime
  = WeeklyStartTime { "DayOfWeek" :: NullOrUndefined (DayOfWeek), "TimeOfDay" :: NullOrUndefined (String), "TimeZone" :: NullOrUndefined (String) }
```

The scheduled time period relative to UTC during which Amazon MQ begins to apply pending updates or patches to the broker.

##### Instances
``` purescript
Newtype WeeklyStartTime _
```


