

-- | Amazon MQ is a managed message broker service for Apache ActiveMQ that makes it easy to set up and operate message brokers in the cloud. A message broker allows software applications and components to communicate using various programming languages, operating systems, and formal messaging protocols.
module AWS.MQ where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MQ" :: String


-- | Creates a broker. Note: This API is asynchronous.
createBroker :: forall eff. CreateBrokerRequest -> Aff (err :: AWS.RequestError | eff) CreateBrokerResponse
createBroker = AWS.request serviceName "CreateBroker" 


-- | Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version). Note: If the configuration name already exists, Amazon MQ doesn't create a configuration.
createConfiguration :: forall eff. CreateConfigurationRequest -> Aff (err :: AWS.RequestError | eff) CreateConfigurationResponse
createConfiguration = AWS.request serviceName "CreateConfiguration" 


-- | Creates an ActiveMQ user.
createUser :: forall eff. CreateUserRequest -> Aff (err :: AWS.RequestError | eff) CreateUserResponse
createUser = AWS.request serviceName "CreateUser" 


-- | Deletes a broker. Note: This API is asynchronous.
deleteBroker :: forall eff. DeleteBrokerRequest -> Aff (err :: AWS.RequestError | eff) DeleteBrokerResponse
deleteBroker = AWS.request serviceName "DeleteBroker" 


-- | Deletes an ActiveMQ user.
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: AWS.RequestError | eff) DeleteUserResponse
deleteUser = AWS.request serviceName "DeleteUser" 


-- | Returns information about the specified broker.
describeBroker :: forall eff. DescribeBrokerRequest -> Aff (err :: AWS.RequestError | eff) DescribeBrokerResponse
describeBroker = AWS.request serviceName "DescribeBroker" 


-- | Returns information about the specified configuration.
describeConfiguration :: forall eff. DescribeConfigurationRequest -> Aff (err :: AWS.RequestError | eff) DescribeConfigurationResponse
describeConfiguration = AWS.request serviceName "DescribeConfiguration" 


-- | Returns the specified configuration revision for the specified configuration.
describeConfigurationRevision :: forall eff. DescribeConfigurationRevisionRequest -> Aff (err :: AWS.RequestError | eff) DescribeConfigurationRevisionResponse
describeConfigurationRevision = AWS.request serviceName "DescribeConfigurationRevision" 


-- | Returns information about an ActiveMQ user.
describeUser :: forall eff. DescribeUserRequest -> Aff (err :: AWS.RequestError | eff) DescribeUserResponse
describeUser = AWS.request serviceName "DescribeUser" 


-- | Returns a list of all brokers.
listBrokers :: forall eff. ListBrokersRequest -> Aff (err :: AWS.RequestError | eff) ListBrokersResponse
listBrokers = AWS.request serviceName "ListBrokers" 


-- | Returns a list of all revisions for the specified configuration.
listConfigurationRevisions :: forall eff. ListConfigurationRevisionsRequest -> Aff (err :: AWS.RequestError | eff) ListConfigurationRevisionsResponse
listConfigurationRevisions = AWS.request serviceName "ListConfigurationRevisions" 


-- | Returns a list of all configurations.
listConfigurations :: forall eff. ListConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) ListConfigurationsResponse
listConfigurations = AWS.request serviceName "ListConfigurations" 


-- | Returns a list of all ActiveMQ users.
listUsers :: forall eff. ListUsersRequest -> Aff (err :: AWS.RequestError | eff) ListUsersResponse
listUsers = AWS.request serviceName "ListUsers" 


-- | Reboots a broker. Note: This API is asynchronous.
rebootBroker :: forall eff. RebootBrokerRequest -> Aff (err :: AWS.RequestError | eff) RebootBrokerResponse
rebootBroker = AWS.request serviceName "RebootBroker" 


-- | Adds a pending configuration change to a broker.
updateBroker :: forall eff. UpdateBrokerRequest -> Aff (err :: AWS.RequestError | eff) UpdateBrokerResponse
updateBroker = AWS.request serviceName "UpdateBroker" 


-- | Updates the specified configuration.
updateConfiguration :: forall eff. UpdateConfigurationRequest -> Aff (err :: AWS.RequestError | eff) UpdateConfigurationResponse
updateConfiguration = AWS.request serviceName "UpdateConfiguration" 


-- | Updates the information for an ActiveMQ user.
updateUser :: forall eff. UpdateUserRequest -> Aff (err :: AWS.RequestError | eff) UpdateUserResponse
updateUser = AWS.request serviceName "UpdateUser" 


-- | Returns information about an error.
newtype BadRequestException = BadRequestException 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


-- | Returns information about all brokers.
newtype BrokerInstance = BrokerInstance 
  { "ConsoleURL" :: NullOrUndefined (String)
  , "Endpoints" :: NullOrUndefined (ListOf__string)
  }


-- | The status of the broker. Possible values: CREATION_IN_PROGRESS, CREATION_FAILED, DELETION_IN_PROGRESS, RUNNING, REBOOT_IN_PROGRESS
newtype BrokerState = BrokerState String


-- | The Amazon Resource Name (ARN) of the broker.
newtype BrokerSummary = BrokerSummary 
  { "BrokerArn" :: NullOrUndefined (String)
  , "BrokerId" :: NullOrUndefined (String)
  , "BrokerName" :: NullOrUndefined (String)
  , "BrokerState" :: NullOrUndefined (BrokerState)
  , "DeploymentMode" :: NullOrUndefined (DeploymentMode)
  , "HostInstanceType" :: NullOrUndefined (String)
  }


-- | The type of change pending for the ActiveMQ user. Possible values: CREATE, UPDATE, DELETE
newtype ChangeType = ChangeType String


-- | Returns information about all configurations.
newtype Configuration = Configuration 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "EngineType" :: NullOrUndefined (EngineType)
  , "EngineVersion" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LatestRevision" :: NullOrUndefined (ConfigurationRevision)
  , "Name" :: NullOrUndefined (String)
  }


-- | A list of information about the configuration.
newtype ConfigurationId = ConfigurationId 
  { "Id" :: NullOrUndefined (String)
  , "Revision" :: NullOrUndefined (Int)
  }


-- | Returns information about the specified configuration revision.
newtype ConfigurationRevision = ConfigurationRevision 
  { "Description" :: NullOrUndefined (String)
  , "Revision" :: NullOrUndefined (Int)
  }


-- | Broker configuration information
newtype Configurations = Configurations 
  { "Current" :: NullOrUndefined (ConfigurationId)
  , "History" :: NullOrUndefined (ListOfConfigurationId)
  , "Pending" :: NullOrUndefined (ConfigurationId)
  }


-- | Returns information about an error.
newtype ConflictException = ConflictException 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


-- | Required. The time period during which Amazon MQ applies pending updates or patches to the broker.
newtype CreateBrokerInput = CreateBrokerInput 
  { "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean)
  , "BrokerName" :: NullOrUndefined (String)
  , "Configuration" :: NullOrUndefined (ConfigurationId)
  , "CreatorRequestId" :: NullOrUndefined (String)
  , "DeploymentMode" :: NullOrUndefined (DeploymentMode)
  , "EngineType" :: NullOrUndefined (EngineType)
  , "EngineVersion" :: NullOrUndefined (String)
  , "HostInstanceType" :: NullOrUndefined (String)
  , "MaintenanceWindowStartTime" :: NullOrUndefined (WeeklyStartTime)
  , "PubliclyAccessible" :: NullOrUndefined (Boolean)
  , "SecurityGroups" :: NullOrUndefined (ListOf__string)
  , "SubnetIds" :: NullOrUndefined (ListOf__string)
  , "Users" :: NullOrUndefined (ListOfUser)
  }


-- | Returns information about the created broker.
newtype CreateBrokerOutput = CreateBrokerOutput 
  { "BrokerArn" :: NullOrUndefined (String)
  , "BrokerId" :: NullOrUndefined (String)
  }


-- | Creates a broker using the specified properties.
newtype CreateBrokerRequest = CreateBrokerRequest 
  { "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean)
  , "BrokerName" :: NullOrUndefined (String)
  , "Configuration" :: NullOrUndefined (ConfigurationId)
  , "CreatorRequestId" :: NullOrUndefined (String)
  , "DeploymentMode" :: NullOrUndefined (DeploymentMode)
  , "EngineType" :: NullOrUndefined (EngineType)
  , "EngineVersion" :: NullOrUndefined (String)
  , "HostInstanceType" :: NullOrUndefined (String)
  , "MaintenanceWindowStartTime" :: NullOrUndefined (WeeklyStartTime)
  , "PubliclyAccessible" :: NullOrUndefined (Boolean)
  , "SecurityGroups" :: NullOrUndefined (ListOf__string)
  , "SubnetIds" :: NullOrUndefined (ListOf__string)
  , "Users" :: NullOrUndefined (ListOfUser)
  }


newtype CreateBrokerResponse = CreateBrokerResponse 
  { "BrokerArn" :: NullOrUndefined (String)
  , "BrokerId" :: NullOrUndefined (String)
  }


-- | Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version). Note: If the configuration name already exists, Amazon MQ doesn't create a configuration.
newtype CreateConfigurationInput = CreateConfigurationInput 
  { "EngineType" :: NullOrUndefined (EngineType)
  , "EngineVersion" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


-- | Returns information about the created configuration.
newtype CreateConfigurationOutput = CreateConfigurationOutput 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LatestRevision" :: NullOrUndefined (ConfigurationRevision)
  , "Name" :: NullOrUndefined (String)
  }


-- | Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version). Note: If the configuration name already exists, Amazon MQ doesn't create a configuration.
newtype CreateConfigurationRequest = CreateConfigurationRequest 
  { "EngineType" :: NullOrUndefined (EngineType)
  , "EngineVersion" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype CreateConfigurationResponse = CreateConfigurationResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LatestRevision" :: NullOrUndefined (ConfigurationRevision)
  , "Name" :: NullOrUndefined (String)
  }


-- | Creates a new ActiveMQ user.
newtype CreateUserInput = CreateUserInput 
  { "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Password" :: NullOrUndefined (String)
  }


-- | Creates a new ActiveMQ user.
newtype CreateUserRequest = CreateUserRequest 
  { "BrokerId" :: (String)
  , "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Password" :: NullOrUndefined (String)
  , "Username" :: (String)
  }


newtype CreateUserResponse = CreateUserResponse 
  { 
  }


newtype DayOfWeek = DayOfWeek String


-- | Returns information about the deleted broker.
newtype DeleteBrokerOutput = DeleteBrokerOutput 
  { "BrokerId" :: NullOrUndefined (String)
  }


newtype DeleteBrokerRequest = DeleteBrokerRequest 
  { "BrokerId" :: (String)
  }


newtype DeleteBrokerResponse = DeleteBrokerResponse 
  { "BrokerId" :: NullOrUndefined (String)
  }


newtype DeleteUserRequest = DeleteUserRequest 
  { "BrokerId" :: (String)
  , "Username" :: (String)
  }


newtype DeleteUserResponse = DeleteUserResponse 
  { 
  }


-- | The deployment mode of the broker. Possible values: SINGLE_INSTANCE, ACTIVE_STANDBY_MULTI_AZ SINGLE_INSTANCE creates a single-instance broker in a single Availability Zone. ACTIVE_STANDBY_MULTI_AZ creates an active/standby broker for high availability.
newtype DeploymentMode = DeploymentMode String


-- | The version of the broker engine. Note: Currently, Amazon MQ supports only 5.15.0.
newtype DescribeBrokerOutput = DescribeBrokerOutput 
  { "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean)
  , "BrokerArn" :: NullOrUndefined (String)
  , "BrokerId" :: NullOrUndefined (String)
  , "BrokerInstances" :: NullOrUndefined (ListOfBrokerInstance)
  , "BrokerName" :: NullOrUndefined (String)
  , "BrokerState" :: NullOrUndefined (BrokerState)
  , "Configurations" :: NullOrUndefined (Configurations)
  , "DeploymentMode" :: NullOrUndefined (DeploymentMode)
  , "EngineType" :: NullOrUndefined (EngineType)
  , "EngineVersion" :: NullOrUndefined (String)
  , "HostInstanceType" :: NullOrUndefined (String)
  , "MaintenanceWindowStartTime" :: NullOrUndefined (WeeklyStartTime)
  , "PubliclyAccessible" :: NullOrUndefined (Boolean)
  , "SecurityGroups" :: NullOrUndefined (ListOf__string)
  , "SubnetIds" :: NullOrUndefined (ListOf__string)
  , "Users" :: NullOrUndefined (ListOfUserSummary)
  }


newtype DescribeBrokerRequest = DescribeBrokerRequest 
  { "BrokerId" :: (String)
  }


newtype DescribeBrokerResponse = DescribeBrokerResponse 
  { "AutoMinorVersionUpgrade" :: NullOrUndefined (Boolean)
  , "BrokerArn" :: NullOrUndefined (String)
  , "BrokerId" :: NullOrUndefined (String)
  , "BrokerInstances" :: NullOrUndefined (ListOfBrokerInstance)
  , "BrokerName" :: NullOrUndefined (String)
  , "BrokerState" :: NullOrUndefined (BrokerState)
  , "Configurations" :: NullOrUndefined (Configurations)
  , "DeploymentMode" :: NullOrUndefined (DeploymentMode)
  , "EngineType" :: NullOrUndefined (EngineType)
  , "EngineVersion" :: NullOrUndefined (String)
  , "HostInstanceType" :: NullOrUndefined (String)
  , "MaintenanceWindowStartTime" :: NullOrUndefined (WeeklyStartTime)
  , "PubliclyAccessible" :: NullOrUndefined (Boolean)
  , "SecurityGroups" :: NullOrUndefined (ListOf__string)
  , "SubnetIds" :: NullOrUndefined (ListOf__string)
  , "Users" :: NullOrUndefined (ListOfUserSummary)
  }


newtype DescribeConfigurationRequest = DescribeConfigurationRequest 
  { "ConfigurationId" :: (String)
  }


newtype DescribeConfigurationResponse = DescribeConfigurationResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "EngineType" :: NullOrUndefined (EngineType)
  , "EngineVersion" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LatestRevision" :: NullOrUndefined (ConfigurationRevision)
  , "Name" :: NullOrUndefined (String)
  }


-- | Returns the specified configuration revision for the specified configuration.
newtype DescribeConfigurationRevisionOutput = DescribeConfigurationRevisionOutput 
  { "ConfigurationId" :: NullOrUndefined (String)
  , "Data" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  }


newtype DescribeConfigurationRevisionRequest = DescribeConfigurationRevisionRequest 
  { "ConfigurationId" :: (String)
  , "ConfigurationRevision" :: (String)
  }


newtype DescribeConfigurationRevisionResponse = DescribeConfigurationRevisionResponse 
  { "ConfigurationId" :: NullOrUndefined (String)
  , "Data" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  }


-- | Returns information about an ActiveMQ user.
newtype DescribeUserOutput = DescribeUserOutput 
  { "BrokerId" :: NullOrUndefined (String)
  , "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Pending" :: NullOrUndefined (UserPendingChanges)
  , "Username" :: NullOrUndefined (String)
  }


newtype DescribeUserRequest = DescribeUserRequest 
  { "BrokerId" :: (String)
  , "Username" :: (String)
  }


newtype DescribeUserResponse = DescribeUserResponse 
  { "BrokerId" :: NullOrUndefined (String)
  , "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Pending" :: NullOrUndefined (UserPendingChanges)
  , "Username" :: NullOrUndefined (String)
  }


-- | The type of broker engine. Note: Currently, Amazon MQ supports only ActiveMQ.
newtype EngineType = EngineType String


-- | Returns information about an error.
newtype Error = Error 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


-- | Returns information about an error.
newtype ForbiddenException = ForbiddenException 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


-- | Returns information about an error.
newtype InternalServerErrorException = InternalServerErrorException 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


-- | A list of information about all brokers.
newtype ListBrokersOutput = ListBrokersOutput 
  { "BrokerSummaries" :: NullOrUndefined (ListOfBrokerSummary)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListBrokersRequest = ListBrokersRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListBrokersResponse = ListBrokersResponse 
  { "BrokerSummaries" :: NullOrUndefined (ListOfBrokerSummary)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Returns a list of all revisions for the specified configuration.
newtype ListConfigurationRevisionsOutput = ListConfigurationRevisionsOutput 
  { "ConfigurationId" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  , "Revisions" :: NullOrUndefined (ListOfConfigurationRevision)
  }


newtype ListConfigurationRevisionsRequest = ListConfigurationRevisionsRequest 
  { "ConfigurationId" :: (String)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListConfigurationRevisionsResponse = ListConfigurationRevisionsResponse 
  { "ConfigurationId" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  , "Revisions" :: NullOrUndefined (ListOfConfigurationRevision)
  }


-- | Returns a list of all configurations.
newtype ListConfigurationsOutput = ListConfigurationsOutput 
  { "Configurations" :: NullOrUndefined (ListOfConfiguration)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListConfigurationsRequest = ListConfigurationsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListConfigurationsResponse = ListConfigurationsResponse 
  { "Configurations" :: NullOrUndefined (ListOfConfiguration)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListOfBrokerInstance = ListOfBrokerInstance (Array BrokerInstance)


newtype ListOfBrokerSummary = ListOfBrokerSummary (Array BrokerSummary)


newtype ListOfConfiguration = ListOfConfiguration (Array Configuration)


newtype ListOfConfigurationId = ListOfConfigurationId (Array ConfigurationId)


newtype ListOfConfigurationRevision = ListOfConfigurationRevision (Array ConfigurationRevision)


newtype ListOfSanitizationWarning = ListOfSanitizationWarning (Array SanitizationWarning)


newtype ListOfUser = ListOfUser (Array User)


newtype ListOfUserSummary = ListOfUserSummary (Array UserSummary)


newtype ListOf__string = ListOf__string (Array String)


-- | Returns a list of all ActiveMQ users.
newtype ListUsersOutput = ListUsersOutput 
  { "BrokerId" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  , "Users" :: NullOrUndefined (ListOfUserSummary)
  }


newtype ListUsersRequest = ListUsersRequest 
  { "BrokerId" :: (String)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListUsersResponse = ListUsersResponse 
  { "BrokerId" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  , "Users" :: NullOrUndefined (ListOfUserSummary)
  }


newtype MaxResults = MaxResults Int


-- | Returns information about an error.
newtype NotFoundException = NotFoundException 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


newtype RebootBrokerRequest = RebootBrokerRequest 
  { "BrokerId" :: (String)
  }


newtype RebootBrokerResponse = RebootBrokerResponse 
  { 
  }


-- | Returns information about the XML element or attribute that was sanitized in the configuration.
newtype SanitizationWarning = SanitizationWarning 
  { "AttributeName" :: NullOrUndefined (String)
  , "ElementName" :: NullOrUndefined (String)
  , "Reason" :: NullOrUndefined (SanitizationWarningReason)
  }


-- | The reason for which the XML elements or attributes were sanitized. Possible values: DISALLOWED_ELEMENT_REMOVED, DISALLOWED_ATTRIBUTE_REMOVED, INVALID_ATTRIBUTE_VALUE_REMOVED DISALLOWED_ELEMENT_REMOVED shows that the provided element isn't allowed and has been removed. DISALLOWED_ATTRIBUTE_REMOVED shows that the provided attribute isn't allowed and has been removed. INVALID_ATTRIBUTE_VALUE_REMOVED shows that the provided value for the attribute isn't allowed and has been removed.
newtype SanitizationWarningReason = SanitizationWarningReason String


-- | Returns information about an error.
newtype UnauthorizedException = UnauthorizedException 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


-- | Updates the broker using the specified properties.
newtype UpdateBrokerInput = UpdateBrokerInput 
  { "Configuration" :: NullOrUndefined (ConfigurationId)
  }


-- | Returns information about the updated broker.
newtype UpdateBrokerOutput = UpdateBrokerOutput 
  { "BrokerId" :: NullOrUndefined (String)
  , "Configuration" :: NullOrUndefined (ConfigurationId)
  }


-- | Updates the broker using the specified properties.
newtype UpdateBrokerRequest = UpdateBrokerRequest 
  { "BrokerId" :: (String)
  , "Configuration" :: NullOrUndefined (ConfigurationId)
  }


newtype UpdateBrokerResponse = UpdateBrokerResponse 
  { "BrokerId" :: NullOrUndefined (String)
  , "Configuration" :: NullOrUndefined (ConfigurationId)
  }


-- | Updates the specified configuration.
newtype UpdateConfigurationInput = UpdateConfigurationInput 
  { "Data" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  }


-- | Returns information about the updated configuration.
newtype UpdateConfigurationOutput = UpdateConfigurationOutput 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LatestRevision" :: NullOrUndefined (ConfigurationRevision)
  , "Name" :: NullOrUndefined (String)
  , "Warnings" :: NullOrUndefined (ListOfSanitizationWarning)
  }


-- | Updates the specified configuration.
newtype UpdateConfigurationRequest = UpdateConfigurationRequest 
  { "ConfigurationId" :: (String)
  , "Data" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  }


newtype UpdateConfigurationResponse = UpdateConfigurationResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LatestRevision" :: NullOrUndefined (ConfigurationRevision)
  , "Name" :: NullOrUndefined (String)
  , "Warnings" :: NullOrUndefined (ListOfSanitizationWarning)
  }


-- | Updates the information for an ActiveMQ user.
newtype UpdateUserInput = UpdateUserInput 
  { "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Password" :: NullOrUndefined (String)
  }


-- | Updates the information for an ActiveMQ user.
newtype UpdateUserRequest = UpdateUserRequest 
  { "BrokerId" :: (String)
  , "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Password" :: NullOrUndefined (String)
  , "Username" :: (String)
  }


newtype UpdateUserResponse = UpdateUserResponse 
  { 
  }


-- | An ActiveMQ user associated with the broker.
newtype User = User 
  { "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Password" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  }


-- | Returns information about the status of the changes pending for the ActiveMQ user.
newtype UserPendingChanges = UserPendingChanges 
  { "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "PendingChange" :: NullOrUndefined (ChangeType)
  }


-- | Returns a list of all ActiveMQ users.
newtype UserSummary = UserSummary 
  { "PendingChange" :: NullOrUndefined (ChangeType)
  , "Username" :: NullOrUndefined (String)
  }


-- | The scheduled time period relative to UTC during which Amazon MQ begins to apply pending updates or patches to the broker.
newtype WeeklyStartTime = WeeklyStartTime 
  { "DayOfWeek" :: NullOrUndefined (DayOfWeek)
  , "TimeOfDay" :: NullOrUndefined (String)
  , "TimeZone" :: NullOrUndefined (String)
  }
