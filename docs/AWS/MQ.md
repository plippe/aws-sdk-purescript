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

#### `BrokerInstance`

``` purescript
newtype BrokerInstance
  = BrokerInstance { "ConsoleURL" :: NullOrUndefined (String), "Endpoints" :: NullOrUndefined (ListOf__string) }
```

Returns information about all brokers.

#### `BrokerState`

``` purescript
newtype BrokerState
  = BrokerState String
```

The status of the broker. Possible values: CREATION_IN_PROGRESS, CREATION_FAILED, DELETION_IN_PROGRESS, RUNNING, REBOOT_IN_PROGRESS

#### `BrokerSummary`

``` purescript
newtype BrokerSummary
  = BrokerSummary { "BrokerArn" :: NullOrUndefined (String), "BrokerId" :: NullOrUndefined (String), "BrokerName" :: NullOrUndefined (String), "BrokerState" :: NullOrUndefined (BrokerState), "DeploymentMode" :: NullOrUndefined (DeploymentMode), "HostInstanceType" :: NullOrUndefined (String) }
```

The Amazon Resource Name (ARN) of the broker.

#### `ChangeType`

``` purescript
newtype ChangeType
  = ChangeType String
```

The type of change pending for the ActiveMQ user. Possible values: CREATE, UPDATE, DELETE

#### `Configuration`

``` purescript
newtype Configuration
  = Configuration { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LatestRevision" :: NullOrUndefined (ConfigurationRevision), "Name" :: NullOrUndefined (String) }
```

Returns information about all configurations.

#### `ConfigurationId`

``` purescript
newtype ConfigurationId
  = ConfigurationId { "Id" :: NullOrUndefined (String), "Revision" :: NullOrUndefined (Int) }
```

A list of information about the configuration.

#### `ConfigurationRevision`

``` purescript
newtype ConfigurationRevision
  = ConfigurationRevision { "Description" :: NullOrUndefined (String), "Revision" :: NullOrUndefined (Int) }
```

Returns information about the specified configuration revision.

#### `Configurations`

``` purescript
newtype Configurations
  = Configurations { "Current" :: NullOrUndefined (ConfigurationId), "History" :: NullOrUndefined (ListOfConfigurationId), "Pending" :: NullOrUndefined (ConfigurationId) }
```

Broker configuration information

#### `ConflictException`

``` purescript
newtype ConflictException
  = ConflictException { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

#### `CreateBrokerInput`

``` purescript
newtype CreateBrokerInput
  = CreateBrokerInput { "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean), "BrokerName" :: NullOrUndefined (String), "Configuration" :: NullOrUndefined (ConfigurationId), "CreatorRequestId" :: NullOrUndefined (String), "DeploymentMode" :: NullOrUndefined (DeploymentMode), "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "HostInstanceType" :: NullOrUndefined (String), "MaintenanceWindowStartTime" :: NullOrUndefined (WeeklyStartTime), "PubliclyAccessible" :: NullOrUndefined (Boolean), "SecurityGroups" :: NullOrUndefined (ListOf__string), "SubnetIds" :: NullOrUndefined (ListOf__string), "Users" :: NullOrUndefined (ListOfUser) }
```

Required. The time period during which Amazon MQ applies pending updates or patches to the broker.

#### `CreateBrokerOutput`

``` purescript
newtype CreateBrokerOutput
  = CreateBrokerOutput { "BrokerArn" :: NullOrUndefined (String), "BrokerId" :: NullOrUndefined (String) }
```

Returns information about the created broker.

#### `CreateBrokerRequest`

``` purescript
newtype CreateBrokerRequest
  = CreateBrokerRequest { "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean), "BrokerName" :: NullOrUndefined (String), "Configuration" :: NullOrUndefined (ConfigurationId), "CreatorRequestId" :: NullOrUndefined (String), "DeploymentMode" :: NullOrUndefined (DeploymentMode), "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "HostInstanceType" :: NullOrUndefined (String), "MaintenanceWindowStartTime" :: NullOrUndefined (WeeklyStartTime), "PubliclyAccessible" :: NullOrUndefined (Boolean), "SecurityGroups" :: NullOrUndefined (ListOf__string), "SubnetIds" :: NullOrUndefined (ListOf__string), "Users" :: NullOrUndefined (ListOfUser) }
```

Creates a broker using the specified properties.

#### `CreateBrokerResponse`

``` purescript
newtype CreateBrokerResponse
  = CreateBrokerResponse { "BrokerArn" :: NullOrUndefined (String), "BrokerId" :: NullOrUndefined (String) }
```

#### `CreateConfigurationInput`

``` purescript
newtype CreateConfigurationInput
  = CreateConfigurationInput { "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version). Note: If the configuration name already exists, Amazon MQ doesn't create a configuration.

#### `CreateConfigurationOutput`

``` purescript
newtype CreateConfigurationOutput
  = CreateConfigurationOutput { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LatestRevision" :: NullOrUndefined (ConfigurationRevision), "Name" :: NullOrUndefined (String) }
```

Returns information about the created configuration.

#### `CreateConfigurationRequest`

``` purescript
newtype CreateConfigurationRequest
  = CreateConfigurationRequest { "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version). Note: If the configuration name already exists, Amazon MQ doesn't create a configuration.

#### `CreateConfigurationResponse`

``` purescript
newtype CreateConfigurationResponse
  = CreateConfigurationResponse { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LatestRevision" :: NullOrUndefined (ConfigurationRevision), "Name" :: NullOrUndefined (String) }
```

#### `CreateUserInput`

``` purescript
newtype CreateUserInput
  = CreateUserInput { "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Password" :: NullOrUndefined (String) }
```

Creates a new ActiveMQ user.

#### `CreateUserRequest`

``` purescript
newtype CreateUserRequest
  = CreateUserRequest { "BrokerId" :: String, "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Password" :: NullOrUndefined (String), "Username" :: String }
```

Creates a new ActiveMQ user.

#### `CreateUserResponse`

``` purescript
newtype CreateUserResponse
  = CreateUserResponse {  }
```

#### `DayOfWeek`

``` purescript
newtype DayOfWeek
  = DayOfWeek String
```

#### `DeleteBrokerOutput`

``` purescript
newtype DeleteBrokerOutput
  = DeleteBrokerOutput { "BrokerId" :: NullOrUndefined (String) }
```

Returns information about the deleted broker.

#### `DeleteBrokerRequest`

``` purescript
newtype DeleteBrokerRequest
  = DeleteBrokerRequest { "BrokerId" :: String }
```

#### `DeleteBrokerResponse`

``` purescript
newtype DeleteBrokerResponse
  = DeleteBrokerResponse { "BrokerId" :: NullOrUndefined (String) }
```

#### `DeleteUserRequest`

``` purescript
newtype DeleteUserRequest
  = DeleteUserRequest { "BrokerId" :: String, "Username" :: String }
```

#### `DeleteUserResponse`

``` purescript
newtype DeleteUserResponse
  = DeleteUserResponse {  }
```

#### `DeploymentMode`

``` purescript
newtype DeploymentMode
  = DeploymentMode String
```

The deployment mode of the broker. Possible values: SINGLE_INSTANCE, ACTIVE_STANDBY_MULTI_AZ SINGLE_INSTANCE creates a single-instance broker in a single Availability Zone. ACTIVE_STANDBY_MULTI_AZ creates an active/standby broker for high availability.

#### `DescribeBrokerOutput`

``` purescript
newtype DescribeBrokerOutput
  = DescribeBrokerOutput { "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean), "BrokerArn" :: NullOrUndefined (String), "BrokerId" :: NullOrUndefined (String), "BrokerInstances" :: NullOrUndefined (ListOfBrokerInstance), "BrokerName" :: NullOrUndefined (String), "BrokerState" :: NullOrUndefined (BrokerState), "Configurations" :: NullOrUndefined (Configurations), "DeploymentMode" :: NullOrUndefined (DeploymentMode), "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "HostInstanceType" :: NullOrUndefined (String), "MaintenanceWindowStartTime" :: NullOrUndefined (WeeklyStartTime), "PubliclyAccessible" :: NullOrUndefined (Boolean), "SecurityGroups" :: NullOrUndefined (ListOf__string), "SubnetIds" :: NullOrUndefined (ListOf__string), "Users" :: NullOrUndefined (ListOfUserSummary) }
```

The version of the broker engine. Note: Currently, Amazon MQ supports only 5.15.0.

#### `DescribeBrokerRequest`

``` purescript
newtype DescribeBrokerRequest
  = DescribeBrokerRequest { "BrokerId" :: String }
```

#### `DescribeBrokerResponse`

``` purescript
newtype DescribeBrokerResponse
  = DescribeBrokerResponse { "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean), "BrokerArn" :: NullOrUndefined (String), "BrokerId" :: NullOrUndefined (String), "BrokerInstances" :: NullOrUndefined (ListOfBrokerInstance), "BrokerName" :: NullOrUndefined (String), "BrokerState" :: NullOrUndefined (BrokerState), "Configurations" :: NullOrUndefined (Configurations), "DeploymentMode" :: NullOrUndefined (DeploymentMode), "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "HostInstanceType" :: NullOrUndefined (String), "MaintenanceWindowStartTime" :: NullOrUndefined (WeeklyStartTime), "PubliclyAccessible" :: NullOrUndefined (Boolean), "SecurityGroups" :: NullOrUndefined (ListOf__string), "SubnetIds" :: NullOrUndefined (ListOf__string), "Users" :: NullOrUndefined (ListOfUserSummary) }
```

#### `DescribeConfigurationRequest`

``` purescript
newtype DescribeConfigurationRequest
  = DescribeConfigurationRequest { "ConfigurationId" :: String }
```

#### `DescribeConfigurationResponse`

``` purescript
newtype DescribeConfigurationResponse
  = DescribeConfigurationResponse { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "EngineType" :: NullOrUndefined (EngineType), "EngineVersion" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LatestRevision" :: NullOrUndefined (ConfigurationRevision), "Name" :: NullOrUndefined (String) }
```

#### `DescribeConfigurationRevisionOutput`

``` purescript
newtype DescribeConfigurationRevisionOutput
  = DescribeConfigurationRevisionOutput { "ConfigurationId" :: NullOrUndefined (String), "Data" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

Returns the specified configuration revision for the specified configuration.

#### `DescribeConfigurationRevisionRequest`

``` purescript
newtype DescribeConfigurationRevisionRequest
  = DescribeConfigurationRevisionRequest { "ConfigurationId" :: String, "ConfigurationRevision" :: String }
```

#### `DescribeConfigurationRevisionResponse`

``` purescript
newtype DescribeConfigurationRevisionResponse
  = DescribeConfigurationRevisionResponse { "ConfigurationId" :: NullOrUndefined (String), "Data" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

#### `DescribeUserOutput`

``` purescript
newtype DescribeUserOutput
  = DescribeUserOutput { "BrokerId" :: NullOrUndefined (String), "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Pending" :: NullOrUndefined (UserPendingChanges), "Username" :: NullOrUndefined (String) }
```

Returns information about an ActiveMQ user.

#### `DescribeUserRequest`

``` purescript
newtype DescribeUserRequest
  = DescribeUserRequest { "BrokerId" :: String, "Username" :: String }
```

#### `DescribeUserResponse`

``` purescript
newtype DescribeUserResponse
  = DescribeUserResponse { "BrokerId" :: NullOrUndefined (String), "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Pending" :: NullOrUndefined (UserPendingChanges), "Username" :: NullOrUndefined (String) }
```

#### `EngineType`

``` purescript
newtype EngineType
  = EngineType String
```

The type of broker engine. Note: Currently, Amazon MQ supports only ActiveMQ.

#### `Error`

``` purescript
newtype Error
  = Error { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

#### `ForbiddenException`

``` purescript
newtype ForbiddenException
  = ForbiddenException { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

#### `ListBrokersOutput`

``` purescript
newtype ListBrokersOutput
  = ListBrokersOutput { "BrokerSummaries" :: NullOrUndefined (ListOfBrokerSummary), "NextToken" :: NullOrUndefined (String) }
```

A list of information about all brokers.

#### `ListBrokersRequest`

``` purescript
newtype ListBrokersRequest
  = ListBrokersRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

#### `ListBrokersResponse`

``` purescript
newtype ListBrokersResponse
  = ListBrokersResponse { "BrokerSummaries" :: NullOrUndefined (ListOfBrokerSummary), "NextToken" :: NullOrUndefined (String) }
```

#### `ListConfigurationRevisionsOutput`

``` purescript
newtype ListConfigurationRevisionsOutput
  = ListConfigurationRevisionsOutput { "ConfigurationId" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Revisions" :: NullOrUndefined (ListOfConfigurationRevision) }
```

Returns a list of all revisions for the specified configuration.

#### `ListConfigurationRevisionsRequest`

``` purescript
newtype ListConfigurationRevisionsRequest
  = ListConfigurationRevisionsRequest { "ConfigurationId" :: String, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

#### `ListConfigurationRevisionsResponse`

``` purescript
newtype ListConfigurationRevisionsResponse
  = ListConfigurationRevisionsResponse { "ConfigurationId" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Revisions" :: NullOrUndefined (ListOfConfigurationRevision) }
```

#### `ListConfigurationsOutput`

``` purescript
newtype ListConfigurationsOutput
  = ListConfigurationsOutput { "Configurations" :: NullOrUndefined (ListOfConfiguration), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

Returns a list of all configurations.

#### `ListConfigurationsRequest`

``` purescript
newtype ListConfigurationsRequest
  = ListConfigurationsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

#### `ListConfigurationsResponse`

``` purescript
newtype ListConfigurationsResponse
  = ListConfigurationsResponse { "Configurations" :: NullOrUndefined (ListOfConfiguration), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

#### `ListOfBrokerInstance`

``` purescript
newtype ListOfBrokerInstance
  = ListOfBrokerInstance (Array BrokerInstance)
```

#### `ListOfBrokerSummary`

``` purescript
newtype ListOfBrokerSummary
  = ListOfBrokerSummary (Array BrokerSummary)
```

#### `ListOfConfiguration`

``` purescript
newtype ListOfConfiguration
  = ListOfConfiguration (Array Configuration)
```

#### `ListOfConfigurationId`

``` purescript
newtype ListOfConfigurationId
  = ListOfConfigurationId (Array ConfigurationId)
```

#### `ListOfConfigurationRevision`

``` purescript
newtype ListOfConfigurationRevision
  = ListOfConfigurationRevision (Array ConfigurationRevision)
```

#### `ListOfSanitizationWarning`

``` purescript
newtype ListOfSanitizationWarning
  = ListOfSanitizationWarning (Array SanitizationWarning)
```

#### `ListOfUser`

``` purescript
newtype ListOfUser
  = ListOfUser (Array User)
```

#### `ListOfUserSummary`

``` purescript
newtype ListOfUserSummary
  = ListOfUserSummary (Array UserSummary)
```

#### `ListOf__string`

``` purescript
newtype ListOf__string
  = ListOf__string (Array String)
```

#### `ListUsersOutput`

``` purescript
newtype ListUsersOutput
  = ListUsersOutput { "BrokerId" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Users" :: NullOrUndefined (ListOfUserSummary) }
```

Returns a list of all ActiveMQ users.

#### `ListUsersRequest`

``` purescript
newtype ListUsersRequest
  = ListUsersRequest { "BrokerId" :: String, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

#### `ListUsersResponse`

``` purescript
newtype ListUsersResponse
  = ListUsersResponse { "BrokerId" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Users" :: NullOrUndefined (ListOfUserSummary) }
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

#### `RebootBrokerRequest`

``` purescript
newtype RebootBrokerRequest
  = RebootBrokerRequest { "BrokerId" :: String }
```

#### `RebootBrokerResponse`

``` purescript
newtype RebootBrokerResponse
  = RebootBrokerResponse {  }
```

#### `SanitizationWarning`

``` purescript
newtype SanitizationWarning
  = SanitizationWarning { "AttributeName" :: NullOrUndefined (String), "ElementName" :: NullOrUndefined (String), "Reason" :: NullOrUndefined (SanitizationWarningReason) }
```

Returns information about the XML element or attribute that was sanitized in the configuration.

#### `SanitizationWarningReason`

``` purescript
newtype SanitizationWarningReason
  = SanitizationWarningReason String
```

The reason for which the XML elements or attributes were sanitized. Possible values: DISALLOWED_ELEMENT_REMOVED, DISALLOWED_ATTRIBUTE_REMOVED, INVALID_ATTRIBUTE_VALUE_REMOVED DISALLOWED_ELEMENT_REMOVED shows that the provided element isn't allowed and has been removed. DISALLOWED_ATTRIBUTE_REMOVED shows that the provided attribute isn't allowed and has been removed. INVALID_ATTRIBUTE_VALUE_REMOVED shows that the provided value for the attribute isn't allowed and has been removed.

#### `UnauthorizedException`

``` purescript
newtype UnauthorizedException
  = UnauthorizedException { "ErrorAttribute" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

Returns information about an error.

#### `UpdateBrokerInput`

``` purescript
newtype UpdateBrokerInput
  = UpdateBrokerInput { "Configuration" :: NullOrUndefined (ConfigurationId) }
```

Updates the broker using the specified properties.

#### `UpdateBrokerOutput`

``` purescript
newtype UpdateBrokerOutput
  = UpdateBrokerOutput { "BrokerId" :: NullOrUndefined (String), "Configuration" :: NullOrUndefined (ConfigurationId) }
```

Returns information about the updated broker.

#### `UpdateBrokerRequest`

``` purescript
newtype UpdateBrokerRequest
  = UpdateBrokerRequest { "BrokerId" :: String, "Configuration" :: NullOrUndefined (ConfigurationId) }
```

Updates the broker using the specified properties.

#### `UpdateBrokerResponse`

``` purescript
newtype UpdateBrokerResponse
  = UpdateBrokerResponse { "BrokerId" :: NullOrUndefined (String), "Configuration" :: NullOrUndefined (ConfigurationId) }
```

#### `UpdateConfigurationInput`

``` purescript
newtype UpdateConfigurationInput
  = UpdateConfigurationInput { "Data" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

Updates the specified configuration.

#### `UpdateConfigurationOutput`

``` purescript
newtype UpdateConfigurationOutput
  = UpdateConfigurationOutput { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LatestRevision" :: NullOrUndefined (ConfigurationRevision), "Name" :: NullOrUndefined (String), "Warnings" :: NullOrUndefined (ListOfSanitizationWarning) }
```

Returns information about the updated configuration.

#### `UpdateConfigurationRequest`

``` purescript
newtype UpdateConfigurationRequest
  = UpdateConfigurationRequest { "ConfigurationId" :: String, "Data" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String) }
```

Updates the specified configuration.

#### `UpdateConfigurationResponse`

``` purescript
newtype UpdateConfigurationResponse
  = UpdateConfigurationResponse { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "LatestRevision" :: NullOrUndefined (ConfigurationRevision), "Name" :: NullOrUndefined (String), "Warnings" :: NullOrUndefined (ListOfSanitizationWarning) }
```

#### `UpdateUserInput`

``` purescript
newtype UpdateUserInput
  = UpdateUserInput { "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Password" :: NullOrUndefined (String) }
```

Updates the information for an ActiveMQ user.

#### `UpdateUserRequest`

``` purescript
newtype UpdateUserRequest
  = UpdateUserRequest { "BrokerId" :: String, "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Password" :: NullOrUndefined (String), "Username" :: String }
```

Updates the information for an ActiveMQ user.

#### `UpdateUserResponse`

``` purescript
newtype UpdateUserResponse
  = UpdateUserResponse {  }
```

#### `User`

``` purescript
newtype User
  = User { "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "Password" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String) }
```

An ActiveMQ user associated with the broker.

#### `UserPendingChanges`

``` purescript
newtype UserPendingChanges
  = UserPendingChanges { "ConsoleAccess" :: NullOrUndefined (Boolean), "Groups" :: NullOrUndefined (ListOf__string), "PendingChange" :: NullOrUndefined (ChangeType) }
```

Returns information about the status of the changes pending for the ActiveMQ user.

#### `UserSummary`

``` purescript
newtype UserSummary
  = UserSummary { "PendingChange" :: NullOrUndefined (ChangeType), "Username" :: NullOrUndefined (String) }
```

Returns a list of all ActiveMQ users.

#### `WeeklyStartTime`

``` purescript
newtype WeeklyStartTime
  = WeeklyStartTime { "DayOfWeek" :: NullOrUndefined (DayOfWeek), "TimeOfDay" :: NullOrUndefined (String), "TimeZone" :: NullOrUndefined (String) }
```

The scheduled time period relative to UTC during which Amazon MQ begins to apply pending updates or patches to the broker.


