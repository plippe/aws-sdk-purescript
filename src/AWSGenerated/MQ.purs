

-- | Amazon MQ is a managed message broker service for Apache ActiveMQ that makes it easy to set up and operate message brokers in the cloud. A message broker allows software applications and components to communicate using various programming languages, operating systems, and formal messaging protocols.
module AWS.MQ where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MQ" :: String


-- | Creates a broker. Note: This API is asynchronous.
createBroker :: forall eff. CreateBrokerRequest -> Aff (err :: AWS.RequestError | eff) CreateBrokerResponse
createBroker = AWS.request serviceName "createBroker" 


-- | Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version). Note: If the configuration name already exists, Amazon MQ doesn't create a configuration.
createConfiguration :: forall eff. CreateConfigurationRequest -> Aff (err :: AWS.RequestError | eff) CreateConfigurationResponse
createConfiguration = AWS.request serviceName "createConfiguration" 


-- | Creates an ActiveMQ user.
createUser :: forall eff. CreateUserRequest -> Aff (err :: AWS.RequestError | eff) CreateUserResponse
createUser = AWS.request serviceName "createUser" 


-- | Deletes a broker. Note: This API is asynchronous.
deleteBroker :: forall eff. DeleteBrokerRequest -> Aff (err :: AWS.RequestError | eff) DeleteBrokerResponse
deleteBroker = AWS.request serviceName "deleteBroker" 


-- | Deletes an ActiveMQ user.
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: AWS.RequestError | eff) DeleteUserResponse
deleteUser = AWS.request serviceName "deleteUser" 


-- | Returns information about the specified broker.
describeBroker :: forall eff. DescribeBrokerRequest -> Aff (err :: AWS.RequestError | eff) DescribeBrokerResponse
describeBroker = AWS.request serviceName "describeBroker" 


-- | Returns information about the specified configuration.
describeConfiguration :: forall eff. DescribeConfigurationRequest -> Aff (err :: AWS.RequestError | eff) DescribeConfigurationResponse
describeConfiguration = AWS.request serviceName "describeConfiguration" 


-- | Returns the specified configuration revision for the specified configuration.
describeConfigurationRevision :: forall eff. DescribeConfigurationRevisionRequest -> Aff (err :: AWS.RequestError | eff) DescribeConfigurationRevisionResponse
describeConfigurationRevision = AWS.request serviceName "describeConfigurationRevision" 


-- | Returns information about an ActiveMQ user.
describeUser :: forall eff. DescribeUserRequest -> Aff (err :: AWS.RequestError | eff) DescribeUserResponse
describeUser = AWS.request serviceName "describeUser" 


-- | Returns a list of all brokers.
listBrokers :: forall eff. ListBrokersRequest -> Aff (err :: AWS.RequestError | eff) ListBrokersResponse
listBrokers = AWS.request serviceName "listBrokers" 


-- | Returns a list of all revisions for the specified configuration.
listConfigurationRevisions :: forall eff. ListConfigurationRevisionsRequest -> Aff (err :: AWS.RequestError | eff) ListConfigurationRevisionsResponse
listConfigurationRevisions = AWS.request serviceName "listConfigurationRevisions" 


-- | Returns a list of all configurations.
listConfigurations :: forall eff. ListConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) ListConfigurationsResponse
listConfigurations = AWS.request serviceName "listConfigurations" 


-- | Returns a list of all ActiveMQ users.
listUsers :: forall eff. ListUsersRequest -> Aff (err :: AWS.RequestError | eff) ListUsersResponse
listUsers = AWS.request serviceName "listUsers" 


-- | Reboots a broker. Note: This API is asynchronous.
rebootBroker :: forall eff. RebootBrokerRequest -> Aff (err :: AWS.RequestError | eff) RebootBrokerResponse
rebootBroker = AWS.request serviceName "rebootBroker" 


-- | Adds a pending configuration change to a broker.
updateBroker :: forall eff. UpdateBrokerRequest -> Aff (err :: AWS.RequestError | eff) UpdateBrokerResponse
updateBroker = AWS.request serviceName "updateBroker" 


-- | Updates the specified configuration.
updateConfiguration :: forall eff. UpdateConfigurationRequest -> Aff (err :: AWS.RequestError | eff) UpdateConfigurationResponse
updateConfiguration = AWS.request serviceName "updateConfiguration" 


-- | Updates the information for an ActiveMQ user.
updateUser :: forall eff. UpdateUserRequest -> Aff (err :: AWS.RequestError | eff) UpdateUserResponse
updateUser = AWS.request serviceName "updateUser" 


-- | Returns information about an error.
newtype BadRequestException = BadRequestException 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _


-- | Returns information about all brokers.
newtype BrokerInstance = BrokerInstance 
  { "ConsoleURL" :: NullOrUndefined (String)
  , "Endpoints" :: NullOrUndefined (ListOf__string)
  }
derive instance newtypeBrokerInstance :: Newtype BrokerInstance _


-- | The status of the broker. Possible values: CREATION_IN_PROGRESS, CREATION_FAILED, DELETION_IN_PROGRESS, RUNNING, REBOOT_IN_PROGRESS
newtype BrokerState = BrokerState String
derive instance newtypeBrokerState :: Newtype BrokerState _


-- | The Amazon Resource Name (ARN) of the broker.
newtype BrokerSummary = BrokerSummary 
  { "BrokerArn" :: NullOrUndefined (String)
  , "BrokerId" :: NullOrUndefined (String)
  , "BrokerName" :: NullOrUndefined (String)
  , "BrokerState" :: NullOrUndefined (BrokerState)
  , "DeploymentMode" :: NullOrUndefined (DeploymentMode)
  , "HostInstanceType" :: NullOrUndefined (String)
  }
derive instance newtypeBrokerSummary :: Newtype BrokerSummary _


-- | The type of change pending for the ActiveMQ user. Possible values: CREATE, UPDATE, DELETE
newtype ChangeType = ChangeType String
derive instance newtypeChangeType :: Newtype ChangeType _


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
derive instance newtypeConfiguration :: Newtype Configuration _


-- | A list of information about the configuration.
newtype ConfigurationId = ConfigurationId 
  { "Id" :: NullOrUndefined (String)
  , "Revision" :: NullOrUndefined (Int)
  }
derive instance newtypeConfigurationId :: Newtype ConfigurationId _


-- | Returns information about the specified configuration revision.
newtype ConfigurationRevision = ConfigurationRevision 
  { "Description" :: NullOrUndefined (String)
  , "Revision" :: NullOrUndefined (Int)
  }
derive instance newtypeConfigurationRevision :: Newtype ConfigurationRevision _


-- | Broker configuration information
newtype Configurations = Configurations 
  { "Current" :: NullOrUndefined (ConfigurationId)
  , "History" :: NullOrUndefined (ListOfConfigurationId)
  , "Pending" :: NullOrUndefined (ConfigurationId)
  }
derive instance newtypeConfigurations :: Newtype Configurations _


-- | Returns information about an error.
newtype ConflictException = ConflictException 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeConflictException :: Newtype ConflictException _


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
derive instance newtypeCreateBrokerInput :: Newtype CreateBrokerInput _


-- | Returns information about the created broker.
newtype CreateBrokerOutput = CreateBrokerOutput 
  { "BrokerArn" :: NullOrUndefined (String)
  , "BrokerId" :: NullOrUndefined (String)
  }
derive instance newtypeCreateBrokerOutput :: Newtype CreateBrokerOutput _


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
derive instance newtypeCreateBrokerRequest :: Newtype CreateBrokerRequest _


newtype CreateBrokerResponse = CreateBrokerResponse 
  { "BrokerArn" :: NullOrUndefined (String)
  , "BrokerId" :: NullOrUndefined (String)
  }
derive instance newtypeCreateBrokerResponse :: Newtype CreateBrokerResponse _


-- | Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version). Note: If the configuration name already exists, Amazon MQ doesn't create a configuration.
newtype CreateConfigurationInput = CreateConfigurationInput 
  { "EngineType" :: NullOrUndefined (EngineType)
  , "EngineVersion" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }
derive instance newtypeCreateConfigurationInput :: Newtype CreateConfigurationInput _


-- | Returns information about the created configuration.
newtype CreateConfigurationOutput = CreateConfigurationOutput 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LatestRevision" :: NullOrUndefined (ConfigurationRevision)
  , "Name" :: NullOrUndefined (String)
  }
derive instance newtypeCreateConfigurationOutput :: Newtype CreateConfigurationOutput _


-- | Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version). Note: If the configuration name already exists, Amazon MQ doesn't create a configuration.
newtype CreateConfigurationRequest = CreateConfigurationRequest 
  { "EngineType" :: NullOrUndefined (EngineType)
  , "EngineVersion" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }
derive instance newtypeCreateConfigurationRequest :: Newtype CreateConfigurationRequest _


newtype CreateConfigurationResponse = CreateConfigurationResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LatestRevision" :: NullOrUndefined (ConfigurationRevision)
  , "Name" :: NullOrUndefined (String)
  }
derive instance newtypeCreateConfigurationResponse :: Newtype CreateConfigurationResponse _


-- | Creates a new ActiveMQ user.
newtype CreateUserInput = CreateUserInput 
  { "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Password" :: NullOrUndefined (String)
  }
derive instance newtypeCreateUserInput :: Newtype CreateUserInput _


-- | Creates a new ActiveMQ user.
newtype CreateUserRequest = CreateUserRequest 
  { "BrokerId" :: (String)
  , "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Password" :: NullOrUndefined (String)
  , "Username" :: (String)
  }
derive instance newtypeCreateUserRequest :: Newtype CreateUserRequest _


newtype CreateUserResponse = CreateUserResponse 
  { 
  }
derive instance newtypeCreateUserResponse :: Newtype CreateUserResponse _


newtype DayOfWeek = DayOfWeek String
derive instance newtypeDayOfWeek :: Newtype DayOfWeek _


-- | Returns information about the deleted broker.
newtype DeleteBrokerOutput = DeleteBrokerOutput 
  { "BrokerId" :: NullOrUndefined (String)
  }
derive instance newtypeDeleteBrokerOutput :: Newtype DeleteBrokerOutput _


newtype DeleteBrokerRequest = DeleteBrokerRequest 
  { "BrokerId" :: (String)
  }
derive instance newtypeDeleteBrokerRequest :: Newtype DeleteBrokerRequest _


newtype DeleteBrokerResponse = DeleteBrokerResponse 
  { "BrokerId" :: NullOrUndefined (String)
  }
derive instance newtypeDeleteBrokerResponse :: Newtype DeleteBrokerResponse _


newtype DeleteUserRequest = DeleteUserRequest 
  { "BrokerId" :: (String)
  , "Username" :: (String)
  }
derive instance newtypeDeleteUserRequest :: Newtype DeleteUserRequest _


newtype DeleteUserResponse = DeleteUserResponse 
  { 
  }
derive instance newtypeDeleteUserResponse :: Newtype DeleteUserResponse _


-- | The deployment mode of the broker. Possible values: SINGLE_INSTANCE, ACTIVE_STANDBY_MULTI_AZ SINGLE_INSTANCE creates a single-instance broker in a single Availability Zone. ACTIVE_STANDBY_MULTI_AZ creates an active/standby broker for high availability.
newtype DeploymentMode = DeploymentMode String
derive instance newtypeDeploymentMode :: Newtype DeploymentMode _


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
derive instance newtypeDescribeBrokerOutput :: Newtype DescribeBrokerOutput _


newtype DescribeBrokerRequest = DescribeBrokerRequest 
  { "BrokerId" :: (String)
  }
derive instance newtypeDescribeBrokerRequest :: Newtype DescribeBrokerRequest _


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
derive instance newtypeDescribeBrokerResponse :: Newtype DescribeBrokerResponse _


newtype DescribeConfigurationRequest = DescribeConfigurationRequest 
  { "ConfigurationId" :: (String)
  }
derive instance newtypeDescribeConfigurationRequest :: Newtype DescribeConfigurationRequest _


newtype DescribeConfigurationResponse = DescribeConfigurationResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "EngineType" :: NullOrUndefined (EngineType)
  , "EngineVersion" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LatestRevision" :: NullOrUndefined (ConfigurationRevision)
  , "Name" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeConfigurationResponse :: Newtype DescribeConfigurationResponse _


-- | Returns the specified configuration revision for the specified configuration.
newtype DescribeConfigurationRevisionOutput = DescribeConfigurationRevisionOutput 
  { "ConfigurationId" :: NullOrUndefined (String)
  , "Data" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeConfigurationRevisionOutput :: Newtype DescribeConfigurationRevisionOutput _


newtype DescribeConfigurationRevisionRequest = DescribeConfigurationRevisionRequest 
  { "ConfigurationId" :: (String)
  , "ConfigurationRevision" :: (String)
  }
derive instance newtypeDescribeConfigurationRevisionRequest :: Newtype DescribeConfigurationRevisionRequest _


newtype DescribeConfigurationRevisionResponse = DescribeConfigurationRevisionResponse 
  { "ConfigurationId" :: NullOrUndefined (String)
  , "Data" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeConfigurationRevisionResponse :: Newtype DescribeConfigurationRevisionResponse _


-- | Returns information about an ActiveMQ user.
newtype DescribeUserOutput = DescribeUserOutput 
  { "BrokerId" :: NullOrUndefined (String)
  , "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Pending" :: NullOrUndefined (UserPendingChanges)
  , "Username" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeUserOutput :: Newtype DescribeUserOutput _


newtype DescribeUserRequest = DescribeUserRequest 
  { "BrokerId" :: (String)
  , "Username" :: (String)
  }
derive instance newtypeDescribeUserRequest :: Newtype DescribeUserRequest _


newtype DescribeUserResponse = DescribeUserResponse 
  { "BrokerId" :: NullOrUndefined (String)
  , "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Pending" :: NullOrUndefined (UserPendingChanges)
  , "Username" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeUserResponse :: Newtype DescribeUserResponse _


-- | The type of broker engine. Note: Currently, Amazon MQ supports only ActiveMQ.
newtype EngineType = EngineType String
derive instance newtypeEngineType :: Newtype EngineType _


-- | Returns information about an error.
newtype Error = Error 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeError :: Newtype Error _


-- | Returns information about an error.
newtype ForbiddenException = ForbiddenException 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeForbiddenException :: Newtype ForbiddenException _


-- | Returns information about an error.
newtype InternalServerErrorException = InternalServerErrorException 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _


-- | A list of information about all brokers.
newtype ListBrokersOutput = ListBrokersOutput 
  { "BrokerSummaries" :: NullOrUndefined (ListOfBrokerSummary)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListBrokersOutput :: Newtype ListBrokersOutput _


newtype ListBrokersRequest = ListBrokersRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListBrokersRequest :: Newtype ListBrokersRequest _


newtype ListBrokersResponse = ListBrokersResponse 
  { "BrokerSummaries" :: NullOrUndefined (ListOfBrokerSummary)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListBrokersResponse :: Newtype ListBrokersResponse _


-- | Returns a list of all revisions for the specified configuration.
newtype ListConfigurationRevisionsOutput = ListConfigurationRevisionsOutput 
  { "ConfigurationId" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  , "Revisions" :: NullOrUndefined (ListOfConfigurationRevision)
  }
derive instance newtypeListConfigurationRevisionsOutput :: Newtype ListConfigurationRevisionsOutput _


newtype ListConfigurationRevisionsRequest = ListConfigurationRevisionsRequest 
  { "ConfigurationId" :: (String)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListConfigurationRevisionsRequest :: Newtype ListConfigurationRevisionsRequest _


newtype ListConfigurationRevisionsResponse = ListConfigurationRevisionsResponse 
  { "ConfigurationId" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  , "Revisions" :: NullOrUndefined (ListOfConfigurationRevision)
  }
derive instance newtypeListConfigurationRevisionsResponse :: Newtype ListConfigurationRevisionsResponse _


-- | Returns a list of all configurations.
newtype ListConfigurationsOutput = ListConfigurationsOutput 
  { "Configurations" :: NullOrUndefined (ListOfConfiguration)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListConfigurationsOutput :: Newtype ListConfigurationsOutput _


newtype ListConfigurationsRequest = ListConfigurationsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListConfigurationsRequest :: Newtype ListConfigurationsRequest _


newtype ListConfigurationsResponse = ListConfigurationsResponse 
  { "Configurations" :: NullOrUndefined (ListOfConfiguration)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListConfigurationsResponse :: Newtype ListConfigurationsResponse _


newtype ListOfBrokerInstance = ListOfBrokerInstance (Array BrokerInstance)
derive instance newtypeListOfBrokerInstance :: Newtype ListOfBrokerInstance _


newtype ListOfBrokerSummary = ListOfBrokerSummary (Array BrokerSummary)
derive instance newtypeListOfBrokerSummary :: Newtype ListOfBrokerSummary _


newtype ListOfConfiguration = ListOfConfiguration (Array Configuration)
derive instance newtypeListOfConfiguration :: Newtype ListOfConfiguration _


newtype ListOfConfigurationId = ListOfConfigurationId (Array ConfigurationId)
derive instance newtypeListOfConfigurationId :: Newtype ListOfConfigurationId _


newtype ListOfConfigurationRevision = ListOfConfigurationRevision (Array ConfigurationRevision)
derive instance newtypeListOfConfigurationRevision :: Newtype ListOfConfigurationRevision _


newtype ListOfSanitizationWarning = ListOfSanitizationWarning (Array SanitizationWarning)
derive instance newtypeListOfSanitizationWarning :: Newtype ListOfSanitizationWarning _


newtype ListOfUser = ListOfUser (Array User)
derive instance newtypeListOfUser :: Newtype ListOfUser _


newtype ListOfUserSummary = ListOfUserSummary (Array UserSummary)
derive instance newtypeListOfUserSummary :: Newtype ListOfUserSummary _


newtype ListOf__string = ListOf__string (Array String)
derive instance newtypeListOf__string :: Newtype ListOf__string _


-- | Returns a list of all ActiveMQ users.
newtype ListUsersOutput = ListUsersOutput 
  { "BrokerId" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  , "Users" :: NullOrUndefined (ListOfUserSummary)
  }
derive instance newtypeListUsersOutput :: Newtype ListUsersOutput _


newtype ListUsersRequest = ListUsersRequest 
  { "BrokerId" :: (String)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListUsersRequest :: Newtype ListUsersRequest _


newtype ListUsersResponse = ListUsersResponse 
  { "BrokerId" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  , "Users" :: NullOrUndefined (ListOfUserSummary)
  }
derive instance newtypeListUsersResponse :: Newtype ListUsersResponse _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | Returns information about an error.
newtype NotFoundException = NotFoundException 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


newtype RebootBrokerRequest = RebootBrokerRequest 
  { "BrokerId" :: (String)
  }
derive instance newtypeRebootBrokerRequest :: Newtype RebootBrokerRequest _


newtype RebootBrokerResponse = RebootBrokerResponse 
  { 
  }
derive instance newtypeRebootBrokerResponse :: Newtype RebootBrokerResponse _


-- | Returns information about the XML element or attribute that was sanitized in the configuration.
newtype SanitizationWarning = SanitizationWarning 
  { "AttributeName" :: NullOrUndefined (String)
  , "ElementName" :: NullOrUndefined (String)
  , "Reason" :: NullOrUndefined (SanitizationWarningReason)
  }
derive instance newtypeSanitizationWarning :: Newtype SanitizationWarning _


-- | The reason for which the XML elements or attributes were sanitized. Possible values: DISALLOWED_ELEMENT_REMOVED, DISALLOWED_ATTRIBUTE_REMOVED, INVALID_ATTRIBUTE_VALUE_REMOVED DISALLOWED_ELEMENT_REMOVED shows that the provided element isn't allowed and has been removed. DISALLOWED_ATTRIBUTE_REMOVED shows that the provided attribute isn't allowed and has been removed. INVALID_ATTRIBUTE_VALUE_REMOVED shows that the provided value for the attribute isn't allowed and has been removed.
newtype SanitizationWarningReason = SanitizationWarningReason String
derive instance newtypeSanitizationWarningReason :: Newtype SanitizationWarningReason _


-- | Returns information about an error.
newtype UnauthorizedException = UnauthorizedException 
  { "ErrorAttribute" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeUnauthorizedException :: Newtype UnauthorizedException _


-- | Updates the broker using the specified properties.
newtype UpdateBrokerInput = UpdateBrokerInput 
  { "Configuration" :: NullOrUndefined (ConfigurationId)
  }
derive instance newtypeUpdateBrokerInput :: Newtype UpdateBrokerInput _


-- | Returns information about the updated broker.
newtype UpdateBrokerOutput = UpdateBrokerOutput 
  { "BrokerId" :: NullOrUndefined (String)
  , "Configuration" :: NullOrUndefined (ConfigurationId)
  }
derive instance newtypeUpdateBrokerOutput :: Newtype UpdateBrokerOutput _


-- | Updates the broker using the specified properties.
newtype UpdateBrokerRequest = UpdateBrokerRequest 
  { "BrokerId" :: (String)
  , "Configuration" :: NullOrUndefined (ConfigurationId)
  }
derive instance newtypeUpdateBrokerRequest :: Newtype UpdateBrokerRequest _


newtype UpdateBrokerResponse = UpdateBrokerResponse 
  { "BrokerId" :: NullOrUndefined (String)
  , "Configuration" :: NullOrUndefined (ConfigurationId)
  }
derive instance newtypeUpdateBrokerResponse :: Newtype UpdateBrokerResponse _


-- | Updates the specified configuration.
newtype UpdateConfigurationInput = UpdateConfigurationInput 
  { "Data" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  }
derive instance newtypeUpdateConfigurationInput :: Newtype UpdateConfigurationInput _


-- | Returns information about the updated configuration.
newtype UpdateConfigurationOutput = UpdateConfigurationOutput 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LatestRevision" :: NullOrUndefined (ConfigurationRevision)
  , "Name" :: NullOrUndefined (String)
  , "Warnings" :: NullOrUndefined (ListOfSanitizationWarning)
  }
derive instance newtypeUpdateConfigurationOutput :: Newtype UpdateConfigurationOutput _


-- | Updates the specified configuration.
newtype UpdateConfigurationRequest = UpdateConfigurationRequest 
  { "ConfigurationId" :: (String)
  , "Data" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  }
derive instance newtypeUpdateConfigurationRequest :: Newtype UpdateConfigurationRequest _


newtype UpdateConfigurationResponse = UpdateConfigurationResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "LatestRevision" :: NullOrUndefined (ConfigurationRevision)
  , "Name" :: NullOrUndefined (String)
  , "Warnings" :: NullOrUndefined (ListOfSanitizationWarning)
  }
derive instance newtypeUpdateConfigurationResponse :: Newtype UpdateConfigurationResponse _


-- | Updates the information for an ActiveMQ user.
newtype UpdateUserInput = UpdateUserInput 
  { "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Password" :: NullOrUndefined (String)
  }
derive instance newtypeUpdateUserInput :: Newtype UpdateUserInput _


-- | Updates the information for an ActiveMQ user.
newtype UpdateUserRequest = UpdateUserRequest 
  { "BrokerId" :: (String)
  , "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Password" :: NullOrUndefined (String)
  , "Username" :: (String)
  }
derive instance newtypeUpdateUserRequest :: Newtype UpdateUserRequest _


newtype UpdateUserResponse = UpdateUserResponse 
  { 
  }
derive instance newtypeUpdateUserResponse :: Newtype UpdateUserResponse _


-- | An ActiveMQ user associated with the broker.
newtype User = User 
  { "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "Password" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  }
derive instance newtypeUser :: Newtype User _


-- | Returns information about the status of the changes pending for the ActiveMQ user.
newtype UserPendingChanges = UserPendingChanges 
  { "ConsoleAccess" :: NullOrUndefined (Boolean)
  , "Groups" :: NullOrUndefined (ListOf__string)
  , "PendingChange" :: NullOrUndefined (ChangeType)
  }
derive instance newtypeUserPendingChanges :: Newtype UserPendingChanges _


-- | Returns a list of all ActiveMQ users.
newtype UserSummary = UserSummary 
  { "PendingChange" :: NullOrUndefined (ChangeType)
  , "Username" :: NullOrUndefined (String)
  }
derive instance newtypeUserSummary :: Newtype UserSummary _


-- | The scheduled time period relative to UTC during which Amazon MQ begins to apply pending updates or patches to the broker.
newtype WeeklyStartTime = WeeklyStartTime 
  { "DayOfWeek" :: NullOrUndefined (DayOfWeek)
  , "TimeOfDay" :: NullOrUndefined (String)
  , "TimeZone" :: NullOrUndefined (String)
  }
derive instance newtypeWeeklyStartTime :: Newtype WeeklyStartTime _
