

-- | <p/>
module AWS.MigrationHub where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MigrationHub" :: String


-- | <p>Associates a created artifact of an AWS cloud resource, the target receiving the migration, with the migration task performed by a migration tool. This API has the following traits:</p> <ul> <li> <p>Migration tools can call the <code>AssociateCreatedArtifact</code> operation to indicate which AWS artifact is associated with a migration task.</p> </li> <li> <p>The created artifact name must be provided in ARN (Amazon Resource Name) format which will contain information about type and region; for example: <code>arn:aws:ec2:us-east-1:488216288981:image/ami-6d0ba87b</code>.</p> </li> <li> <p>Examples of the AWS resource behind the created artifact are, AMI's, EC2 instance, or DMS endpoint, etc.</p> </li> </ul>
associateCreatedArtifact :: forall eff. AssociateCreatedArtifactRequest -> Aff (err :: AWS.RequestError | eff) AssociateCreatedArtifactResult
associateCreatedArtifact = AWS.request serviceName "AssociateCreatedArtifact" 


-- | <p>Associates a discovered resource ID from Application Discovery Service (ADS) with a migration task.</p>
associateDiscoveredResource :: forall eff. AssociateDiscoveredResourceRequest -> Aff (err :: AWS.RequestError | eff) AssociateDiscoveredResourceResult
associateDiscoveredResource = AWS.request serviceName "AssociateDiscoveredResource" 


-- | <p>Creates a progress update stream which is an AWS resource used for access control as well as a namespace for migration task names that is implicitly linked to your AWS account. It must uniquely identify the migration tool as it is used for all updates made by the tool; however, it does not need to be unique for each AWS account because it is scoped to the AWS account.</p>
createProgressUpdateStream :: forall eff. CreateProgressUpdateStreamRequest -> Aff (err :: AWS.RequestError | eff) CreateProgressUpdateStreamResult
createProgressUpdateStream = AWS.request serviceName "CreateProgressUpdateStream" 


-- | <p>Deletes a progress update stream, including all of its tasks, which was previously created as an AWS resource used for access control. This API has the following traits:</p> <ul> <li> <p>The only parameter needed for <code>DeleteProgressUpdateStream</code> is the stream name (same as a <code>CreateProgressUpdateStream</code> call).</p> </li> <li> <p>The call will return, and a background process will asynchronously be doing the actual delete of the stream and all of its resources (tasks, associated resources, resource attributes, created artifacts).</p> </li> <li> <p>If the stream takes time to be deleted, it might still show up on a <code>ListProgressUpdateStreams</code> call.</p> </li> <li> <p> <code>CreateProgressUpdateStream</code>, <code>ImportMigrationTask</code>, <code>NotifyMigrationTaskState</code>, and all Associate[*] APIs realted to the tasks belonging to the stream will throw "InvalidInputException" if the stream of the same name is in the process of being deleted.</p> </li> <li> <p>Once the stream and all of its resources are deleted, <code>CreateProgressUpdateStream</code> for a stream of the same name will succeed, and that stream will be an entirely new logical resource (without any resources associated with the old stream).</p> </li> </ul>
deleteProgressUpdateStream :: forall eff. DeleteProgressUpdateStreamRequest -> Aff (err :: AWS.RequestError | eff) DeleteProgressUpdateStreamResult
deleteProgressUpdateStream = AWS.request serviceName "DeleteProgressUpdateStream" 


-- | <p>Gets the migration status of an application.</p>
describeApplicationState :: forall eff. DescribeApplicationStateRequest -> Aff (err :: AWS.RequestError | eff) DescribeApplicationStateResult
describeApplicationState = AWS.request serviceName "DescribeApplicationState" 


-- | <p>Retrieves a list of all attributes associated with a specific migration task.</p>
describeMigrationTask :: forall eff. DescribeMigrationTaskRequest -> Aff (err :: AWS.RequestError | eff) DescribeMigrationTaskResult
describeMigrationTask = AWS.request serviceName "DescribeMigrationTask" 


-- | <p>Disassociates a created artifact of an AWS resource with a migration task performed by a migration tool that was previously associated. This API has the following traits:</p> <ul> <li> <p>A migration user can call the <code>DisassociateCreatedArtifacts</code> operation to disassociate a created AWS Artifact from a migration task.</p> </li> <li> <p>The created artifact name must be provided in ARN (Amazon Resource Name) format which will contain information about type and region; for example: <code>arn:aws:ec2:us-east-1:488216288981:image/ami-6d0ba87b</code>.</p> </li> <li> <p>Examples of the AWS resource behind the created artifact are, AMI's, EC2 instance, or RDS instance, etc.</p> </li> </ul>
disassociateCreatedArtifact :: forall eff. DisassociateCreatedArtifactRequest -> Aff (err :: AWS.RequestError | eff) DisassociateCreatedArtifactResult
disassociateCreatedArtifact = AWS.request serviceName "DisassociateCreatedArtifact" 


-- | <p>Disassociate an Application Discovery Service (ADS) discovered resource from a migration task.</p>
disassociateDiscoveredResource :: forall eff. DisassociateDiscoveredResourceRequest -> Aff (err :: AWS.RequestError | eff) DisassociateDiscoveredResourceResult
disassociateDiscoveredResource = AWS.request serviceName "DisassociateDiscoveredResource" 


-- | <p>Registers a new migration task which represents a server, database, etc., being migrated to AWS by a migration tool.</p> <p>This API is a prerequisite to calling the <code>NotifyMigrationTaskState</code> API as the migration tool must first register the migration task with Migration Hub.</p>
importMigrationTask :: forall eff. ImportMigrationTaskRequest -> Aff (err :: AWS.RequestError | eff) ImportMigrationTaskResult
importMigrationTask = AWS.request serviceName "ImportMigrationTask" 


-- | <p>Lists the created artifacts attached to a given migration task in an update stream. This API has the following traits:</p> <ul> <li> <p>Gets the list of the created artifacts while migration is taking place.</p> </li> <li> <p>Shows the artifacts created by the migration tool that was associated by the <code>AssociateCreatedArtifact</code> API. </p> </li> <li> <p>Lists created artifacts in a paginated interface. </p> </li> </ul>
listCreatedArtifacts :: forall eff. ListCreatedArtifactsRequest -> Aff (err :: AWS.RequestError | eff) ListCreatedArtifactsResult
listCreatedArtifacts = AWS.request serviceName "ListCreatedArtifacts" 


-- | <p>Lists discovered resources associated with the given <code>MigrationTask</code>.</p>
listDiscoveredResources :: forall eff. ListDiscoveredResourcesRequest -> Aff (err :: AWS.RequestError | eff) ListDiscoveredResourcesResult
listDiscoveredResources = AWS.request serviceName "ListDiscoveredResources" 


-- | <p>Lists all, or filtered by resource name, migration tasks associated with the user account making this call. This API has the following traits:</p> <ul> <li> <p>Can show a summary list of the most recent migration tasks.</p> </li> <li> <p>Can show a summary list of migration tasks associated with a given discovered resource.</p> </li> <li> <p>Lists migration tasks in a paginated interface.</p> </li> </ul>
listMigrationTasks :: forall eff. ListMigrationTasksRequest -> Aff (err :: AWS.RequestError | eff) ListMigrationTasksResult
listMigrationTasks = AWS.request serviceName "ListMigrationTasks" 


-- | <p>Lists progress update streams associated with the user account making this call.</p>
listProgressUpdateStreams :: forall eff. ListProgressUpdateStreamsRequest -> Aff (err :: AWS.RequestError | eff) ListProgressUpdateStreamsResult
listProgressUpdateStreams = AWS.request serviceName "ListProgressUpdateStreams" 


-- | <p>Sets the migration state of an application. For a given application identified by the value passed to <code>ApplicationId</code>, its status is set or updated by passing one of three values to <code>Status</code>: <code>NOT_STARTED | IN_PROGRESS | COMPLETED</code>.</p>
notifyApplicationState :: forall eff. NotifyApplicationStateRequest -> Aff (err :: AWS.RequestError | eff) NotifyApplicationStateResult
notifyApplicationState = AWS.request serviceName "NotifyApplicationState" 


-- | <p>Notifies Migration Hub of the current status, progress, or other detail regarding a migration task. This API has the following traits:</p> <ul> <li> <p>Migration tools will call the <code>NotifyMigrationTaskState</code> API to share the latest progress and status.</p> </li> <li> <p> <code>MigrationTaskName</code> is used for addressing updates to the correct target.</p> </li> <li> <p> <code>ProgressUpdateStream</code> is used for access control and to provide a namespace for each migration tool.</p> </li> </ul>
notifyMigrationTaskState :: forall eff. NotifyMigrationTaskStateRequest -> Aff (err :: AWS.RequestError | eff) NotifyMigrationTaskStateResult
notifyMigrationTaskState = AWS.request serviceName "NotifyMigrationTaskState" 


-- | <p>Provides identifying details of the resource being migrated so that it can be associated in the Application Discovery Service (ADS)'s repository. This association occurs asynchronously after <code>PutResourceAttributes</code> returns.</p> <important> <p>Keep in mind that subsequent calls to PutResourceAttributes will override previously stored attributes. For example, if it is first called with a MAC address, but later, it is desired to <i>add</i> an IP address, it will then be required to call it with <i>both</i> the IP and MAC addresses to prevent overiding the MAC address.</p> </important> <note> <p>Because this is an asynchronous call, it will always return 200, whether an association occurs or not. To confirm if an association was found based on the provided details, call <code>ListAssociatedResource</code>.</p> </note>
putResourceAttributes :: forall eff. PutResourceAttributesRequest -> Aff (err :: AWS.RequestError | eff) PutResourceAttributesResult
putResourceAttributes = AWS.request serviceName "PutResourceAttributes" 


-- | <p>Exception raised when the account making the call is not whitelisted or there are other authentication errors.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _


newtype ApplicationId = ApplicationId String
derive instance newtypeApplicationId :: Newtype ApplicationId _


newtype ApplicationStatus = ApplicationStatus String
derive instance newtypeApplicationStatus :: Newtype ApplicationStatus _


newtype AssociateCreatedArtifactRequest = AssociateCreatedArtifactRequest 
  { "ProgressUpdateStream" :: (ProgressUpdateStream)
  , "MigrationTaskName" :: (MigrationTaskName)
  , "CreatedArtifact" :: (CreatedArtifact)
  , "DryRun" :: NullOrUndefined (DryRun)
  }
derive instance newtypeAssociateCreatedArtifactRequest :: Newtype AssociateCreatedArtifactRequest _


newtype AssociateCreatedArtifactResult = AssociateCreatedArtifactResult 
  { 
  }
derive instance newtypeAssociateCreatedArtifactResult :: Newtype AssociateCreatedArtifactResult _


newtype AssociateDiscoveredResourceRequest = AssociateDiscoveredResourceRequest 
  { "ProgressUpdateStream" :: (ProgressUpdateStream)
  , "MigrationTaskName" :: (MigrationTaskName)
  , "DiscoveredResource" :: (DiscoveredResource)
  , "DryRun" :: NullOrUndefined (DryRun)
  }
derive instance newtypeAssociateDiscoveredResourceRequest :: Newtype AssociateDiscoveredResourceRequest _


newtype AssociateDiscoveredResourceResult = AssociateDiscoveredResourceResult 
  { 
  }
derive instance newtypeAssociateDiscoveredResourceResult :: Newtype AssociateDiscoveredResourceResult _


newtype ConfigurationId = ConfigurationId String
derive instance newtypeConfigurationId :: Newtype ConfigurationId _


newtype CreateProgressUpdateStreamRequest = CreateProgressUpdateStreamRequest 
  { "ProgressUpdateStreamName" :: (ProgressUpdateStream)
  , "DryRun" :: NullOrUndefined (DryRun)
  }
derive instance newtypeCreateProgressUpdateStreamRequest :: Newtype CreateProgressUpdateStreamRequest _


newtype CreateProgressUpdateStreamResult = CreateProgressUpdateStreamResult 
  { 
  }
derive instance newtypeCreateProgressUpdateStreamResult :: Newtype CreateProgressUpdateStreamResult _


-- | <p>An ARN of the AWS cloud resource target receiving the migration (e.g., AMI, EC2 instance, RDS instance, etc.).</p>
newtype CreatedArtifact = CreatedArtifact 
  { "Name" :: (CreatedArtifactName)
  , "Description" :: NullOrUndefined (CreatedArtifactDescription)
  }
derive instance newtypeCreatedArtifact :: Newtype CreatedArtifact _


newtype CreatedArtifactDescription = CreatedArtifactDescription String
derive instance newtypeCreatedArtifactDescription :: Newtype CreatedArtifactDescription _


newtype CreatedArtifactList = CreatedArtifactList (Array CreatedArtifact)
derive instance newtypeCreatedArtifactList :: Newtype CreatedArtifactList _


newtype CreatedArtifactName = CreatedArtifactName String
derive instance newtypeCreatedArtifactName :: Newtype CreatedArtifactName _


newtype DeleteProgressUpdateStreamRequest = DeleteProgressUpdateStreamRequest 
  { "ProgressUpdateStreamName" :: (ProgressUpdateStream)
  , "DryRun" :: NullOrUndefined (DryRun)
  }
derive instance newtypeDeleteProgressUpdateStreamRequest :: Newtype DeleteProgressUpdateStreamRequest _


newtype DeleteProgressUpdateStreamResult = DeleteProgressUpdateStreamResult 
  { 
  }
derive instance newtypeDeleteProgressUpdateStreamResult :: Newtype DeleteProgressUpdateStreamResult _


newtype DescribeApplicationStateRequest = DescribeApplicationStateRequest 
  { "ApplicationId" :: (ApplicationId)
  }
derive instance newtypeDescribeApplicationStateRequest :: Newtype DescribeApplicationStateRequest _


newtype DescribeApplicationStateResult = DescribeApplicationStateResult 
  { "ApplicationStatus" :: NullOrUndefined (ApplicationStatus)
  , "LastUpdatedTime" :: NullOrUndefined (UpdateDateTime)
  }
derive instance newtypeDescribeApplicationStateResult :: Newtype DescribeApplicationStateResult _


newtype DescribeMigrationTaskRequest = DescribeMigrationTaskRequest 
  { "ProgressUpdateStream" :: (ProgressUpdateStream)
  , "MigrationTaskName" :: (MigrationTaskName)
  }
derive instance newtypeDescribeMigrationTaskRequest :: Newtype DescribeMigrationTaskRequest _


newtype DescribeMigrationTaskResult = DescribeMigrationTaskResult 
  { "MigrationTask" :: NullOrUndefined (MigrationTask)
  }
derive instance newtypeDescribeMigrationTaskResult :: Newtype DescribeMigrationTaskResult _


newtype DisassociateCreatedArtifactRequest = DisassociateCreatedArtifactRequest 
  { "ProgressUpdateStream" :: (ProgressUpdateStream)
  , "MigrationTaskName" :: (MigrationTaskName)
  , "CreatedArtifactName" :: (CreatedArtifactName)
  , "DryRun" :: NullOrUndefined (DryRun)
  }
derive instance newtypeDisassociateCreatedArtifactRequest :: Newtype DisassociateCreatedArtifactRequest _


newtype DisassociateCreatedArtifactResult = DisassociateCreatedArtifactResult 
  { 
  }
derive instance newtypeDisassociateCreatedArtifactResult :: Newtype DisassociateCreatedArtifactResult _


newtype DisassociateDiscoveredResourceRequest = DisassociateDiscoveredResourceRequest 
  { "ProgressUpdateStream" :: (ProgressUpdateStream)
  , "MigrationTaskName" :: (MigrationTaskName)
  , "ConfigurationId" :: (ConfigurationId)
  , "DryRun" :: NullOrUndefined (DryRun)
  }
derive instance newtypeDisassociateDiscoveredResourceRequest :: Newtype DisassociateDiscoveredResourceRequest _


newtype DisassociateDiscoveredResourceResult = DisassociateDiscoveredResourceResult 
  { 
  }
derive instance newtypeDisassociateDiscoveredResourceResult :: Newtype DisassociateDiscoveredResourceResult _


-- | <p>Object representing the on-premises resource being migrated.</p>
newtype DiscoveredResource = DiscoveredResource 
  { "ConfigurationId" :: (ConfigurationId)
  , "Description" :: NullOrUndefined (DiscoveredResourceDescription)
  }
derive instance newtypeDiscoveredResource :: Newtype DiscoveredResource _


newtype DiscoveredResourceDescription = DiscoveredResourceDescription String
derive instance newtypeDiscoveredResourceDescription :: Newtype DiscoveredResourceDescription _


newtype DiscoveredResourceList = DiscoveredResourceList (Array DiscoveredResource)
derive instance newtypeDiscoveredResourceList :: Newtype DiscoveredResourceList _


newtype DryRun = DryRun Boolean
derive instance newtypeDryRun :: Newtype DryRun _


-- | <p>Exception raised to indicate a successfully authorized action when the <code>DryRun</code> flag is set to "true".</p>
newtype DryRunOperation = DryRunOperation 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDryRunOperation :: Newtype DryRunOperation _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype ImportMigrationTaskRequest = ImportMigrationTaskRequest 
  { "ProgressUpdateStream" :: (ProgressUpdateStream)
  , "MigrationTaskName" :: (MigrationTaskName)
  , "DryRun" :: NullOrUndefined (DryRun)
  }
derive instance newtypeImportMigrationTaskRequest :: Newtype ImportMigrationTaskRequest _


newtype ImportMigrationTaskResult = ImportMigrationTaskResult 
  { 
  }
derive instance newtypeImportMigrationTaskResult :: Newtype ImportMigrationTaskResult _


-- | <p>Exception raised when there is an internal, configuration, or dependency error encountered.</p>
newtype InternalServerError = InternalServerError 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _


-- | <p>Exception raised when the provided input violates a policy constraint or is entered in the wrong format or data type.</p>
newtype InvalidInputException = InvalidInputException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidInputException :: Newtype InvalidInputException _


newtype LatestResourceAttributeList = LatestResourceAttributeList (Array ResourceAttribute)
derive instance newtypeLatestResourceAttributeList :: Newtype LatestResourceAttributeList _


newtype ListCreatedArtifactsRequest = ListCreatedArtifactsRequest 
  { "ProgressUpdateStream" :: (ProgressUpdateStream)
  , "MigrationTaskName" :: (MigrationTaskName)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (MaxResultsCreatedArtifacts)
  }
derive instance newtypeListCreatedArtifactsRequest :: Newtype ListCreatedArtifactsRequest _


newtype ListCreatedArtifactsResult = ListCreatedArtifactsResult 
  { "NextToken" :: NullOrUndefined (Token)
  , "CreatedArtifactList" :: NullOrUndefined (CreatedArtifactList)
  }
derive instance newtypeListCreatedArtifactsResult :: Newtype ListCreatedArtifactsResult _


newtype ListDiscoveredResourcesRequest = ListDiscoveredResourcesRequest 
  { "ProgressUpdateStream" :: (ProgressUpdateStream)
  , "MigrationTaskName" :: (MigrationTaskName)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (MaxResultsResources)
  }
derive instance newtypeListDiscoveredResourcesRequest :: Newtype ListDiscoveredResourcesRequest _


newtype ListDiscoveredResourcesResult = ListDiscoveredResourcesResult 
  { "NextToken" :: NullOrUndefined (Token)
  , "DiscoveredResourceList" :: NullOrUndefined (DiscoveredResourceList)
  }
derive instance newtypeListDiscoveredResourcesResult :: Newtype ListDiscoveredResourcesResult _


newtype ListMigrationTasksRequest = ListMigrationTasksRequest 
  { "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "ResourceName" :: NullOrUndefined (ResourceName)
  }
derive instance newtypeListMigrationTasksRequest :: Newtype ListMigrationTasksRequest _


newtype ListMigrationTasksResult = ListMigrationTasksResult 
  { "NextToken" :: NullOrUndefined (Token)
  , "MigrationTaskSummaryList" :: NullOrUndefined (MigrationTaskSummaryList)
  }
derive instance newtypeListMigrationTasksResult :: Newtype ListMigrationTasksResult _


newtype ListProgressUpdateStreamsRequest = ListProgressUpdateStreamsRequest 
  { "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListProgressUpdateStreamsRequest :: Newtype ListProgressUpdateStreamsRequest _


newtype ListProgressUpdateStreamsResult = ListProgressUpdateStreamsResult 
  { "ProgressUpdateStreamSummaryList" :: NullOrUndefined (ProgressUpdateStreamSummaryList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeListProgressUpdateStreamsResult :: Newtype ListProgressUpdateStreamsResult _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype MaxResultsCreatedArtifacts = MaxResultsCreatedArtifacts Int
derive instance newtypeMaxResultsCreatedArtifacts :: Newtype MaxResultsCreatedArtifacts _


newtype MaxResultsResources = MaxResultsResources Int
derive instance newtypeMaxResultsResources :: Newtype MaxResultsResources _


-- | <p>Represents a migration task in a migration tool.</p>
newtype MigrationTask = MigrationTask 
  { "ProgressUpdateStream" :: NullOrUndefined (ProgressUpdateStream)
  , "MigrationTaskName" :: NullOrUndefined (MigrationTaskName)
  , "Task" :: NullOrUndefined (Task)
  , "UpdateDateTime" :: NullOrUndefined (UpdateDateTime)
  , "ResourceAttributeList" :: NullOrUndefined (LatestResourceAttributeList)
  }
derive instance newtypeMigrationTask :: Newtype MigrationTask _


newtype MigrationTaskName = MigrationTaskName String
derive instance newtypeMigrationTaskName :: Newtype MigrationTaskName _


-- | <p>MigrationTaskSummary includes <code>MigrationTaskName</code>, <code>ProgressPercent</code>, <code>ProgressUpdateStream</code>, <code>Status</code>, and <code>UpdateDateTime</code> for each task.</p>
newtype MigrationTaskSummary = MigrationTaskSummary 
  { "ProgressUpdateStream" :: NullOrUndefined (ProgressUpdateStream)
  , "MigrationTaskName" :: NullOrUndefined (MigrationTaskName)
  , "Status" :: NullOrUndefined (Status)
  , "ProgressPercent" :: NullOrUndefined (ProgressPercent)
  , "StatusDetail" :: NullOrUndefined (StatusDetail)
  , "UpdateDateTime" :: NullOrUndefined (UpdateDateTime)
  }
derive instance newtypeMigrationTaskSummary :: Newtype MigrationTaskSummary _


newtype MigrationTaskSummaryList = MigrationTaskSummaryList (Array MigrationTaskSummary)
derive instance newtypeMigrationTaskSummaryList :: Newtype MigrationTaskSummaryList _


newtype NextUpdateSeconds = NextUpdateSeconds Int
derive instance newtypeNextUpdateSeconds :: Newtype NextUpdateSeconds _


newtype NotifyApplicationStateRequest = NotifyApplicationStateRequest 
  { "ApplicationId" :: (ApplicationId)
  , "Status" :: (ApplicationStatus)
  , "DryRun" :: NullOrUndefined (DryRun)
  }
derive instance newtypeNotifyApplicationStateRequest :: Newtype NotifyApplicationStateRequest _


newtype NotifyApplicationStateResult = NotifyApplicationStateResult 
  { 
  }
derive instance newtypeNotifyApplicationStateResult :: Newtype NotifyApplicationStateResult _


newtype NotifyMigrationTaskStateRequest = NotifyMigrationTaskStateRequest 
  { "ProgressUpdateStream" :: (ProgressUpdateStream)
  , "MigrationTaskName" :: (MigrationTaskName)
  , "Task" :: (Task)
  , "UpdateDateTime" :: (UpdateDateTime)
  , "NextUpdateSeconds" :: (NextUpdateSeconds)
  , "DryRun" :: NullOrUndefined (DryRun)
  }
derive instance newtypeNotifyMigrationTaskStateRequest :: Newtype NotifyMigrationTaskStateRequest _


newtype NotifyMigrationTaskStateResult = NotifyMigrationTaskStateResult 
  { 
  }
derive instance newtypeNotifyMigrationTaskStateResult :: Newtype NotifyMigrationTaskStateResult _


-- | <p>Exception raised when there are problems accessing ADS (Application Discovery Service); most likely due to a misconfigured policy or the <code>ADSCaller</code> role is missing or not configured correctly.</p>
newtype PolicyErrorException = PolicyErrorException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypePolicyErrorException :: Newtype PolicyErrorException _


newtype ProgressPercent = ProgressPercent Int
derive instance newtypeProgressPercent :: Newtype ProgressPercent _


newtype ProgressUpdateStream = ProgressUpdateStream String
derive instance newtypeProgressUpdateStream :: Newtype ProgressUpdateStream _


-- | <p>Summary of the AWS resource used for access control that is implicitly linked to your AWS account.</p>
newtype ProgressUpdateStreamSummary = ProgressUpdateStreamSummary 
  { "ProgressUpdateStreamName" :: NullOrUndefined (ProgressUpdateStream)
  }
derive instance newtypeProgressUpdateStreamSummary :: Newtype ProgressUpdateStreamSummary _


newtype ProgressUpdateStreamSummaryList = ProgressUpdateStreamSummaryList (Array ProgressUpdateStreamSummary)
derive instance newtypeProgressUpdateStreamSummaryList :: Newtype ProgressUpdateStreamSummaryList _


newtype PutResourceAttributesRequest = PutResourceAttributesRequest 
  { "ProgressUpdateStream" :: (ProgressUpdateStream)
  , "MigrationTaskName" :: (MigrationTaskName)
  , "ResourceAttributeList" :: (ResourceAttributeList)
  , "DryRun" :: NullOrUndefined (DryRun)
  }
derive instance newtypePutResourceAttributesRequest :: Newtype PutResourceAttributesRequest _


newtype PutResourceAttributesResult = PutResourceAttributesResult 
  { 
  }
derive instance newtypePutResourceAttributesResult :: Newtype PutResourceAttributesResult _


-- | <p>Attribute associated with a resource.</p>
newtype ResourceAttribute = ResourceAttribute 
  { "Type" :: (ResourceAttributeType)
  , "Value" :: (ResourceAttributeValue)
  }
derive instance newtypeResourceAttribute :: Newtype ResourceAttribute _


newtype ResourceAttributeList = ResourceAttributeList (Array ResourceAttribute)
derive instance newtypeResourceAttributeList :: Newtype ResourceAttributeList _


newtype ResourceAttributeType = ResourceAttributeType String
derive instance newtypeResourceAttributeType :: Newtype ResourceAttributeType _


newtype ResourceAttributeValue = ResourceAttributeValue String
derive instance newtypeResourceAttributeValue :: Newtype ResourceAttributeValue _


newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _


-- | <p>Exception raised when the request references a resource (ADS configuration, update stream, migration task, etc.) that does not exist in ADS (Application Discovery Service) or in Migration Hub's repository.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>Exception raised when the service encounters throttled communication with upstream dependencies or is overloaded with requests.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _


newtype Status = Status String
derive instance newtypeStatus :: Newtype Status _


newtype StatusDetail = StatusDetail String
derive instance newtypeStatusDetail :: Newtype StatusDetail _


-- | <p>Task object encapsulating task information.</p>
newtype Task = Task 
  { "Status" :: (Status)
  , "StatusDetail" :: NullOrUndefined (StatusDetail)
  , "ProgressPercent" :: NullOrUndefined (ProgressPercent)
  }
derive instance newtypeTask :: Newtype Task _


newtype Token = Token String
derive instance newtypeToken :: Newtype Token _


-- | <p>Exception raised to indicate a request was not authorized when the <code>DryRun</code> flag is set to "true".</p>
newtype UnauthorizedOperation = UnauthorizedOperation 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnauthorizedOperation :: Newtype UnauthorizedOperation _


newtype UpdateDateTime = UpdateDateTime Number
derive instance newtypeUpdateDateTime :: Newtype UpdateDateTime _
