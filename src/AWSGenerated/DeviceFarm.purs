

-- | <p>AWS Device Farm is a service that enables mobile app developers to test Android, iOS, and Fire OS apps on physical phones, tablets, and other devices in the cloud.</p>
module AWS.DeviceFarm where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "DeviceFarm" :: String


-- | <p>Creates a device pool.</p>
createDevicePool :: forall eff. CreateDevicePoolRequest -> Aff (err :: AWS.RequestError | eff) CreateDevicePoolResult
createDevicePool = AWS.request serviceName "createDevicePool" 


-- | <p>Creates a network profile.</p>
createNetworkProfile :: forall eff. CreateNetworkProfileRequest -> Aff (err :: AWS.RequestError | eff) CreateNetworkProfileResult
createNetworkProfile = AWS.request serviceName "createNetworkProfile" 


-- | <p>Creates a new project.</p>
createProject :: forall eff. CreateProjectRequest -> Aff (err :: AWS.RequestError | eff) CreateProjectResult
createProject = AWS.request serviceName "createProject" 


-- | <p>Specifies and starts a remote access session.</p>
createRemoteAccessSession :: forall eff. CreateRemoteAccessSessionRequest -> Aff (err :: AWS.RequestError | eff) CreateRemoteAccessSessionResult
createRemoteAccessSession = AWS.request serviceName "createRemoteAccessSession" 


-- | <p>Uploads an app or test scripts.</p>
createUpload :: forall eff. CreateUploadRequest -> Aff (err :: AWS.RequestError | eff) CreateUploadResult
createUpload = AWS.request serviceName "createUpload" 


-- | <p>Deletes a device pool given the pool ARN. Does not allow deletion of curated pools owned by the system.</p>
deleteDevicePool :: forall eff. DeleteDevicePoolRequest -> Aff (err :: AWS.RequestError | eff) DeleteDevicePoolResult
deleteDevicePool = AWS.request serviceName "deleteDevicePool" 


-- | <p>Deletes a network profile.</p>
deleteNetworkProfile :: forall eff. DeleteNetworkProfileRequest -> Aff (err :: AWS.RequestError | eff) DeleteNetworkProfileResult
deleteNetworkProfile = AWS.request serviceName "deleteNetworkProfile" 


-- | <p>Deletes an AWS Device Farm project, given the project ARN.</p> <p> <b>Note</b> Deleting this resource does not stop an in-progress run.</p>
deleteProject :: forall eff. DeleteProjectRequest -> Aff (err :: AWS.RequestError | eff) DeleteProjectResult
deleteProject = AWS.request serviceName "deleteProject" 


-- | <p>Deletes a completed remote access session and its results.</p>
deleteRemoteAccessSession :: forall eff. DeleteRemoteAccessSessionRequest -> Aff (err :: AWS.RequestError | eff) DeleteRemoteAccessSessionResult
deleteRemoteAccessSession = AWS.request serviceName "deleteRemoteAccessSession" 


-- | <p>Deletes the run, given the run ARN.</p> <p> <b>Note</b> Deleting this resource does not stop an in-progress run.</p>
deleteRun :: forall eff. DeleteRunRequest -> Aff (err :: AWS.RequestError | eff) DeleteRunResult
deleteRun = AWS.request serviceName "deleteRun" 


-- | <p>Deletes an upload given the upload ARN.</p>
deleteUpload :: forall eff. DeleteUploadRequest -> Aff (err :: AWS.RequestError | eff) DeleteUploadResult
deleteUpload = AWS.request serviceName "deleteUpload" 


-- | <p>Returns the number of unmetered iOS and/or unmetered Android devices that have been purchased by the account.</p>
getAccountSettings :: forall eff. GetAccountSettingsRequest -> Aff (err :: AWS.RequestError | eff) GetAccountSettingsResult
getAccountSettings = AWS.request serviceName "getAccountSettings" 


-- | <p>Gets information about a unique device type.</p>
getDevice :: forall eff. GetDeviceRequest -> Aff (err :: AWS.RequestError | eff) GetDeviceResult
getDevice = AWS.request serviceName "getDevice" 


-- | <p>Gets information about a device pool.</p>
getDevicePool :: forall eff. GetDevicePoolRequest -> Aff (err :: AWS.RequestError | eff) GetDevicePoolResult
getDevicePool = AWS.request serviceName "getDevicePool" 


-- | <p>Gets information about compatibility with a device pool.</p>
getDevicePoolCompatibility :: forall eff. GetDevicePoolCompatibilityRequest -> Aff (err :: AWS.RequestError | eff) GetDevicePoolCompatibilityResult
getDevicePoolCompatibility = AWS.request serviceName "getDevicePoolCompatibility" 


-- | <p>Gets information about a job.</p>
getJob :: forall eff. GetJobRequest -> Aff (err :: AWS.RequestError | eff) GetJobResult
getJob = AWS.request serviceName "getJob" 


-- | <p>Returns information about a network profile.</p>
getNetworkProfile :: forall eff. GetNetworkProfileRequest -> Aff (err :: AWS.RequestError | eff) GetNetworkProfileResult
getNetworkProfile = AWS.request serviceName "getNetworkProfile" 


-- | <p>Gets the current status and future status of all offerings purchased by an AWS account. The response indicates how many offerings are currently available and the offerings that will be available in the next period. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
getOfferingStatus :: forall eff. GetOfferingStatusRequest -> Aff (err :: AWS.RequestError | eff) GetOfferingStatusResult
getOfferingStatus = AWS.request serviceName "getOfferingStatus" 


-- | <p>Gets information about a project.</p>
getProject :: forall eff. GetProjectRequest -> Aff (err :: AWS.RequestError | eff) GetProjectResult
getProject = AWS.request serviceName "getProject" 


-- | <p>Returns a link to a currently running remote access session.</p>
getRemoteAccessSession :: forall eff. GetRemoteAccessSessionRequest -> Aff (err :: AWS.RequestError | eff) GetRemoteAccessSessionResult
getRemoteAccessSession = AWS.request serviceName "getRemoteAccessSession" 


-- | <p>Gets information about a run.</p>
getRun :: forall eff. GetRunRequest -> Aff (err :: AWS.RequestError | eff) GetRunResult
getRun = AWS.request serviceName "getRun" 


-- | <p>Gets information about a suite.</p>
getSuite :: forall eff. GetSuiteRequest -> Aff (err :: AWS.RequestError | eff) GetSuiteResult
getSuite = AWS.request serviceName "getSuite" 


-- | <p>Gets information about a test.</p>
getTest :: forall eff. GetTestRequest -> Aff (err :: AWS.RequestError | eff) GetTestResult
getTest = AWS.request serviceName "getTest" 


-- | <p>Gets information about an upload.</p>
getUpload :: forall eff. GetUploadRequest -> Aff (err :: AWS.RequestError | eff) GetUploadResult
getUpload = AWS.request serviceName "getUpload" 


-- | <p>Installs an application to the device in a remote access session. For Android applications, the file must be in .apk format. For iOS applications, the file must be in .ipa format.</p>
installToRemoteAccessSession :: forall eff. InstallToRemoteAccessSessionRequest -> Aff (err :: AWS.RequestError | eff) InstallToRemoteAccessSessionResult
installToRemoteAccessSession = AWS.request serviceName "installToRemoteAccessSession" 


-- | <p>Gets information about artifacts.</p>
listArtifacts :: forall eff. ListArtifactsRequest -> Aff (err :: AWS.RequestError | eff) ListArtifactsResult
listArtifacts = AWS.request serviceName "listArtifacts" 


-- | <p>Gets information about device pools.</p>
listDevicePools :: forall eff. ListDevicePoolsRequest -> Aff (err :: AWS.RequestError | eff) ListDevicePoolsResult
listDevicePools = AWS.request serviceName "listDevicePools" 


-- | <p>Gets information about unique device types.</p>
listDevices :: forall eff. ListDevicesRequest -> Aff (err :: AWS.RequestError | eff) ListDevicesResult
listDevices = AWS.request serviceName "listDevices" 


-- | <p>Gets information about jobs for a given test run.</p>
listJobs :: forall eff. ListJobsRequest -> Aff (err :: AWS.RequestError | eff) ListJobsResult
listJobs = AWS.request serviceName "listJobs" 


-- | <p>Returns the list of available network profiles.</p>
listNetworkProfiles :: forall eff. ListNetworkProfilesRequest -> Aff (err :: AWS.RequestError | eff) ListNetworkProfilesResult
listNetworkProfiles = AWS.request serviceName "listNetworkProfiles" 


-- | <p>Returns a list of offering promotions. Each offering promotion record contains the ID and description of the promotion. The API returns a <code>NotEligible</code> error if the caller is not permitted to invoke the operation. Contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
listOfferingPromotions :: forall eff. ListOfferingPromotionsRequest -> Aff (err :: AWS.RequestError | eff) ListOfferingPromotionsResult
listOfferingPromotions = AWS.request serviceName "listOfferingPromotions" 


-- | <p>Returns a list of all historical purchases, renewals, and system renewal transactions for an AWS account. The list is paginated and ordered by a descending timestamp (most recent transactions are first). The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
listOfferingTransactions :: forall eff. ListOfferingTransactionsRequest -> Aff (err :: AWS.RequestError | eff) ListOfferingTransactionsResult
listOfferingTransactions = AWS.request serviceName "listOfferingTransactions" 


-- | <p>Returns a list of products or offerings that the user can manage through the API. Each offering record indicates the recurring price per unit and the frequency for that offering. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
listOfferings :: forall eff. ListOfferingsRequest -> Aff (err :: AWS.RequestError | eff) ListOfferingsResult
listOfferings = AWS.request serviceName "listOfferings" 


-- | <p>Gets information about projects.</p>
listProjects :: forall eff. ListProjectsRequest -> Aff (err :: AWS.RequestError | eff) ListProjectsResult
listProjects = AWS.request serviceName "listProjects" 


-- | <p>Returns a list of all currently running remote access sessions.</p>
listRemoteAccessSessions :: forall eff. ListRemoteAccessSessionsRequest -> Aff (err :: AWS.RequestError | eff) ListRemoteAccessSessionsResult
listRemoteAccessSessions = AWS.request serviceName "listRemoteAccessSessions" 


-- | <p>Gets information about runs, given an AWS Device Farm project ARN.</p>
listRuns :: forall eff. ListRunsRequest -> Aff (err :: AWS.RequestError | eff) ListRunsResult
listRuns = AWS.request serviceName "listRuns" 


-- | <p>Gets information about samples, given an AWS Device Farm project ARN</p>
listSamples :: forall eff. ListSamplesRequest -> Aff (err :: AWS.RequestError | eff) ListSamplesResult
listSamples = AWS.request serviceName "listSamples" 


-- | <p>Gets information about test suites for a given job.</p>
listSuites :: forall eff. ListSuitesRequest -> Aff (err :: AWS.RequestError | eff) ListSuitesResult
listSuites = AWS.request serviceName "listSuites" 


-- | <p>Gets information about tests in a given test suite.</p>
listTests :: forall eff. ListTestsRequest -> Aff (err :: AWS.RequestError | eff) ListTestsResult
listTests = AWS.request serviceName "listTests" 


-- | <p>Gets information about unique problems.</p>
listUniqueProblems :: forall eff. ListUniqueProblemsRequest -> Aff (err :: AWS.RequestError | eff) ListUniqueProblemsResult
listUniqueProblems = AWS.request serviceName "listUniqueProblems" 


-- | <p>Gets information about uploads, given an AWS Device Farm project ARN.</p>
listUploads :: forall eff. ListUploadsRequest -> Aff (err :: AWS.RequestError | eff) ListUploadsResult
listUploads = AWS.request serviceName "listUploads" 


-- | <p>Immediately purchases offerings for an AWS account. Offerings renew with the latest total purchased quantity for an offering, unless the renewal was overridden. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
purchaseOffering :: forall eff. PurchaseOfferingRequest -> Aff (err :: AWS.RequestError | eff) PurchaseOfferingResult
purchaseOffering = AWS.request serviceName "purchaseOffering" 


-- | <p>Explicitly sets the quantity of devices to renew for an offering, starting from the <code>effectiveDate</code> of the next period. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
renewOffering :: forall eff. RenewOfferingRequest -> Aff (err :: AWS.RequestError | eff) RenewOfferingResult
renewOffering = AWS.request serviceName "renewOffering" 


-- | <p>Schedules a run.</p>
scheduleRun :: forall eff. ScheduleRunRequest -> Aff (err :: AWS.RequestError | eff) ScheduleRunResult
scheduleRun = AWS.request serviceName "scheduleRun" 


-- | <p>Ends a specified remote access session.</p>
stopRemoteAccessSession :: forall eff. StopRemoteAccessSessionRequest -> Aff (err :: AWS.RequestError | eff) StopRemoteAccessSessionResult
stopRemoteAccessSession = AWS.request serviceName "stopRemoteAccessSession" 


-- | <p>Initiates a stop request for the current test run. AWS Device Farm will immediately stop the run on devices where tests have not started executing, and you will not be billed for these devices. On devices where tests have started executing, Setup Suite and Teardown Suite tests will run to completion before stopping execution on those devices. You will be billed for Setup, Teardown, and any tests that were in progress or already completed.</p>
stopRun :: forall eff. StopRunRequest -> Aff (err :: AWS.RequestError | eff) StopRunResult
stopRun = AWS.request serviceName "stopRun" 


-- | <p>Modifies the name, description, and rules in a device pool given the attributes and the pool ARN. Rule updates are all-or-nothing, meaning they can only be updated as a whole (or not at all).</p>
updateDevicePool :: forall eff. UpdateDevicePoolRequest -> Aff (err :: AWS.RequestError | eff) UpdateDevicePoolResult
updateDevicePool = AWS.request serviceName "updateDevicePool" 


-- | <p>Updates the network profile with specific settings.</p>
updateNetworkProfile :: forall eff. UpdateNetworkProfileRequest -> Aff (err :: AWS.RequestError | eff) UpdateNetworkProfileResult
updateNetworkProfile = AWS.request serviceName "updateNetworkProfile" 


-- | <p>Modifies the specified project name, given the project ARN and a new name.</p>
updateProject :: forall eff. UpdateProjectRequest -> Aff (err :: AWS.RequestError | eff) UpdateProjectResult
updateProject = AWS.request serviceName "updateProject" 


newtype AWSAccountNumber = AWSAccountNumber String
derive instance newtypeAWSAccountNumber :: Newtype AWSAccountNumber _


-- | <p>A container for account-level settings within AWS Device Farm.</p>
newtype AccountSettings = AccountSettings 
  { "AwsAccountNumber'" :: NullOrUndefined (AWSAccountNumber)
  , "UnmeteredDevices'" :: NullOrUndefined (PurchasedDevicesMap)
  , "UnmeteredRemoteAccessDevices'" :: NullOrUndefined (PurchasedDevicesMap)
  , "MaxJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes)
  , "TrialMinutes'" :: NullOrUndefined (TrialMinutes)
  , "MaxSlots'" :: NullOrUndefined (MaxSlotMap)
  , "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes)
  }
derive instance newtypeAccountSettings :: Newtype AccountSettings _


newtype AccountsCleanup = AccountsCleanup Boolean
derive instance newtypeAccountsCleanup :: Newtype AccountsCleanup _


newtype AmazonResourceName = AmazonResourceName String
derive instance newtypeAmazonResourceName :: Newtype AmazonResourceName _


newtype AmazonResourceNames = AmazonResourceNames (Array AmazonResourceName)
derive instance newtypeAmazonResourceNames :: Newtype AmazonResourceNames _


newtype AndroidPaths = AndroidPaths (Array String)
derive instance newtypeAndroidPaths :: Newtype AndroidPaths _


newtype AppPackagesCleanup = AppPackagesCleanup Boolean
derive instance newtypeAppPackagesCleanup :: Newtype AppPackagesCleanup _


-- | <p>An invalid argument was specified.</p>
newtype ArgumentException = ArgumentException 
  { "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeArgumentException :: Newtype ArgumentException _


-- | <p>Represents the output of a test. Examples of artifacts include logs and screenshots.</p>
newtype Artifact = Artifact 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Type'" :: NullOrUndefined (ArtifactType)
  , "Extension'" :: NullOrUndefined (String)
  , "Url'" :: NullOrUndefined (URL)
  }
derive instance newtypeArtifact :: Newtype Artifact _


newtype ArtifactCategory = ArtifactCategory String
derive instance newtypeArtifactCategory :: Newtype ArtifactCategory _


newtype ArtifactType = ArtifactType String
derive instance newtypeArtifactType :: Newtype ArtifactType _


newtype Artifacts = Artifacts (Array Artifact)
derive instance newtypeArtifacts :: Newtype Artifacts _


newtype BillingMethod = BillingMethod String
derive instance newtypeBillingMethod :: Newtype BillingMethod _


-- | <p>Represents the amount of CPU that an app is using on a physical device.</p> <p>Note that this does not represent system-wide CPU usage.</p>
newtype CPU = CPU 
  { "Frequency'" :: NullOrUndefined (String)
  , "Architecture'" :: NullOrUndefined (String)
  , "Clock'" :: NullOrUndefined (Number)
  }
derive instance newtypeCPU :: Newtype CPU _


newtype ClientId = ClientId String
derive instance newtypeClientId :: Newtype ClientId _


newtype ContentType = ContentType String
derive instance newtypeContentType :: Newtype ContentType _


-- | <p>Represents entity counters.</p>
newtype Counters = Counters 
  { "Total'" :: NullOrUndefined (Int)
  , "Passed'" :: NullOrUndefined (Int)
  , "Failed'" :: NullOrUndefined (Int)
  , "Warned'" :: NullOrUndefined (Int)
  , "Errored'" :: NullOrUndefined (Int)
  , "Stopped'" :: NullOrUndefined (Int)
  , "Skipped'" :: NullOrUndefined (Int)
  }
derive instance newtypeCounters :: Newtype Counters _


-- | <p>Represents a request to the create device pool operation.</p>
newtype CreateDevicePoolRequest = CreateDevicePoolRequest 
  { "ProjectArn'" :: (AmazonResourceName)
  , "Name'" :: (Name)
  , "Description'" :: NullOrUndefined (Message)
  , "Rules'" :: (Rules)
  }
derive instance newtypeCreateDevicePoolRequest :: Newtype CreateDevicePoolRequest _


-- | <p>Represents the result of a create device pool request.</p>
newtype CreateDevicePoolResult = CreateDevicePoolResult 
  { "DevicePool'" :: NullOrUndefined (DevicePool)
  }
derive instance newtypeCreateDevicePoolResult :: Newtype CreateDevicePoolResult _


newtype CreateNetworkProfileRequest = CreateNetworkProfileRequest 
  { "ProjectArn'" :: (AmazonResourceName)
  , "Name'" :: (Name)
  , "Description'" :: NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined (NetworkProfileType)
  , "UplinkBandwidthBits'" :: NullOrUndefined (Number)
  , "DownlinkBandwidthBits'" :: NullOrUndefined (Number)
  , "UplinkDelayMs'" :: NullOrUndefined (Number)
  , "DownlinkDelayMs'" :: NullOrUndefined (Number)
  , "UplinkJitterMs'" :: NullOrUndefined (Number)
  , "DownlinkJitterMs'" :: NullOrUndefined (Number)
  , "UplinkLossPercent'" :: NullOrUndefined (PercentInteger)
  , "DownlinkLossPercent'" :: NullOrUndefined (PercentInteger)
  }
derive instance newtypeCreateNetworkProfileRequest :: Newtype CreateNetworkProfileRequest _


newtype CreateNetworkProfileResult = CreateNetworkProfileResult 
  { "NetworkProfile'" :: NullOrUndefined (NetworkProfile)
  }
derive instance newtypeCreateNetworkProfileResult :: Newtype CreateNetworkProfileResult _


-- | <p>Represents a request to the create project operation.</p>
newtype CreateProjectRequest = CreateProjectRequest 
  { "Name'" :: (Name)
  , "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes)
  }
derive instance newtypeCreateProjectRequest :: Newtype CreateProjectRequest _


-- | <p>Represents the result of a create project request.</p>
newtype CreateProjectResult = CreateProjectResult 
  { "Project'" :: NullOrUndefined (Project)
  }
derive instance newtypeCreateProjectResult :: Newtype CreateProjectResult _


-- | <p>Creates the configuration settings for a remote access session, including the device model and type.</p>
newtype CreateRemoteAccessSessionConfiguration = CreateRemoteAccessSessionConfiguration 
  { "BillingMethod'" :: NullOrUndefined (BillingMethod)
  }
derive instance newtypeCreateRemoteAccessSessionConfiguration :: Newtype CreateRemoteAccessSessionConfiguration _


-- | <p>Creates and submits a request to start a remote access session.</p>
newtype CreateRemoteAccessSessionRequest = CreateRemoteAccessSessionRequest 
  { "ProjectArn'" :: (AmazonResourceName)
  , "DeviceArn'" :: (AmazonResourceName)
  , "SshPublicKey'" :: NullOrUndefined (SshPublicKey)
  , "RemoteDebugEnabled'" :: NullOrUndefined (Boolean)
  , "RemoteRecordEnabled'" :: NullOrUndefined (Boolean)
  , "RemoteRecordAppArn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "ClientId'" :: NullOrUndefined (ClientId)
  , "Configuration'" :: NullOrUndefined (CreateRemoteAccessSessionConfiguration)
  , "InteractionMode'" :: NullOrUndefined (InteractionMode)
  }
derive instance newtypeCreateRemoteAccessSessionRequest :: Newtype CreateRemoteAccessSessionRequest _


-- | <p>Represents the server response from a request to create a remote access session.</p>
newtype CreateRemoteAccessSessionResult = CreateRemoteAccessSessionResult 
  { "RemoteAccessSession'" :: NullOrUndefined (RemoteAccessSession)
  }
derive instance newtypeCreateRemoteAccessSessionResult :: Newtype CreateRemoteAccessSessionResult _


-- | <p>Represents a request to the create upload operation.</p>
newtype CreateUploadRequest = CreateUploadRequest 
  { "ProjectArn'" :: (AmazonResourceName)
  , "Name'" :: (Name)
  , "Type'" :: (UploadType)
  , "ContentType'" :: NullOrUndefined (ContentType)
  }
derive instance newtypeCreateUploadRequest :: Newtype CreateUploadRequest _


-- | <p>Represents the result of a create upload request.</p>
newtype CreateUploadResult = CreateUploadResult 
  { "Upload'" :: NullOrUndefined (Upload)
  }
derive instance newtypeCreateUploadResult :: Newtype CreateUploadResult _


newtype CurrencyCode = CurrencyCode String
derive instance newtypeCurrencyCode :: Newtype CurrencyCode _


-- | <p>A JSON object specifying the paths where the artifacts generated by the customer's tests, on the device or in the test environment, will be pulled from.</p> <p>Specify <code>deviceHostPaths</code> and optionally specify either <code>iosPaths</code> or <code>androidPaths</code>.</p> <p>For web app tests, you can specify both <code>iosPaths</code> and <code>androidPaths</code>.</p>
newtype CustomerArtifactPaths = CustomerArtifactPaths 
  { "IosPaths'" :: NullOrUndefined (IosPaths)
  , "AndroidPaths'" :: NullOrUndefined (AndroidPaths)
  , "DeviceHostPaths'" :: NullOrUndefined (DeviceHostPaths)
  }
derive instance newtypeCustomerArtifactPaths :: Newtype CustomerArtifactPaths _


newtype DateTime = DateTime Number
derive instance newtypeDateTime :: Newtype DateTime _


-- | <p>Represents a request to the delete device pool operation.</p>
newtype DeleteDevicePoolRequest = DeleteDevicePoolRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeDeleteDevicePoolRequest :: Newtype DeleteDevicePoolRequest _


-- | <p>Represents the result of a delete device pool request.</p>
newtype DeleteDevicePoolResult = DeleteDevicePoolResult 
  { 
  }
derive instance newtypeDeleteDevicePoolResult :: Newtype DeleteDevicePoolResult _


newtype DeleteNetworkProfileRequest = DeleteNetworkProfileRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeDeleteNetworkProfileRequest :: Newtype DeleteNetworkProfileRequest _


newtype DeleteNetworkProfileResult = DeleteNetworkProfileResult 
  { 
  }
derive instance newtypeDeleteNetworkProfileResult :: Newtype DeleteNetworkProfileResult _


-- | <p>Represents a request to the delete project operation.</p>
newtype DeleteProjectRequest = DeleteProjectRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeDeleteProjectRequest :: Newtype DeleteProjectRequest _


-- | <p>Represents the result of a delete project request.</p>
newtype DeleteProjectResult = DeleteProjectResult 
  { 
  }
derive instance newtypeDeleteProjectResult :: Newtype DeleteProjectResult _


-- | <p>Represents the request to delete the specified remote access session.</p>
newtype DeleteRemoteAccessSessionRequest = DeleteRemoteAccessSessionRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeDeleteRemoteAccessSessionRequest :: Newtype DeleteRemoteAccessSessionRequest _


-- | <p>The response from the server when a request is made to delete the remote access session.</p>
newtype DeleteRemoteAccessSessionResult = DeleteRemoteAccessSessionResult 
  { 
  }
derive instance newtypeDeleteRemoteAccessSessionResult :: Newtype DeleteRemoteAccessSessionResult _


-- | <p>Represents a request to the delete run operation.</p>
newtype DeleteRunRequest = DeleteRunRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeDeleteRunRequest :: Newtype DeleteRunRequest _


-- | <p>Represents the result of a delete run request.</p>
newtype DeleteRunResult = DeleteRunResult 
  { 
  }
derive instance newtypeDeleteRunResult :: Newtype DeleteRunResult _


-- | <p>Represents a request to the delete upload operation.</p>
newtype DeleteUploadRequest = DeleteUploadRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeDeleteUploadRequest :: Newtype DeleteUploadRequest _


-- | <p>Represents the result of a delete upload request.</p>
newtype DeleteUploadResult = DeleteUploadResult 
  { 
  }
derive instance newtypeDeleteUploadResult :: Newtype DeleteUploadResult _


-- | <p>Represents a device type that an app is tested against.</p>
newtype Device = Device 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Manufacturer'" :: NullOrUndefined (String)
  , "Model'" :: NullOrUndefined (String)
  , "ModelId'" :: NullOrUndefined (String)
  , "FormFactor'" :: NullOrUndefined (DeviceFormFactor)
  , "Platform'" :: NullOrUndefined (DevicePlatform)
  , "Os'" :: NullOrUndefined (String)
  , "Cpu'" :: NullOrUndefined (CPU)
  , "Resolution'" :: NullOrUndefined (Resolution)
  , "HeapSize'" :: NullOrUndefined (Number)
  , "Memory'" :: NullOrUndefined (Number)
  , "Image'" :: NullOrUndefined (String)
  , "Carrier'" :: NullOrUndefined (String)
  , "Radio'" :: NullOrUndefined (String)
  , "RemoteAccessEnabled'" :: NullOrUndefined (Boolean)
  , "RemoteDebugEnabled'" :: NullOrUndefined (Boolean)
  , "FleetType'" :: NullOrUndefined (String)
  , "FleetName'" :: NullOrUndefined (String)
  }
derive instance newtypeDevice :: Newtype Device _


newtype DeviceAttribute = DeviceAttribute String
derive instance newtypeDeviceAttribute :: Newtype DeviceAttribute _


newtype DeviceFormFactor = DeviceFormFactor String
derive instance newtypeDeviceFormFactor :: Newtype DeviceFormFactor _


newtype DeviceHostPaths = DeviceHostPaths (Array String)
derive instance newtypeDeviceHostPaths :: Newtype DeviceHostPaths _


-- | <p>Represents the total (metered or unmetered) minutes used by the resource to run tests. Contains the sum of minutes consumed by all children.</p>
newtype DeviceMinutes = DeviceMinutes 
  { "Total'" :: NullOrUndefined (Number)
  , "Metered'" :: NullOrUndefined (Number)
  , "Unmetered'" :: NullOrUndefined (Number)
  }
derive instance newtypeDeviceMinutes :: Newtype DeviceMinutes _


newtype DevicePlatform = DevicePlatform String
derive instance newtypeDevicePlatform :: Newtype DevicePlatform _


-- | <p>Represents a collection of device types.</p>
newtype DevicePool = DevicePool 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Description'" :: NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined (DevicePoolType)
  , "Rules'" :: NullOrUndefined (Rules)
  }
derive instance newtypeDevicePool :: Newtype DevicePool _


-- | <p>Represents a device pool compatibility result.</p>
newtype DevicePoolCompatibilityResult = DevicePoolCompatibilityResult 
  { "Device'" :: NullOrUndefined (Device)
  , "Compatible'" :: NullOrUndefined (Boolean)
  , "IncompatibilityMessages'" :: NullOrUndefined (IncompatibilityMessages)
  }
derive instance newtypeDevicePoolCompatibilityResult :: Newtype DevicePoolCompatibilityResult _


newtype DevicePoolCompatibilityResults = DevicePoolCompatibilityResults (Array DevicePoolCompatibilityResult)
derive instance newtypeDevicePoolCompatibilityResults :: Newtype DevicePoolCompatibilityResults _


newtype DevicePoolType = DevicePoolType String
derive instance newtypeDevicePoolType :: Newtype DevicePoolType _


newtype DevicePools = DevicePools (Array DevicePool)
derive instance newtypeDevicePools :: Newtype DevicePools _


newtype Devices = Devices (Array Device)
derive instance newtypeDevices :: Newtype Devices _


-- | <p>Represents configuration information about a test run, such as the execution timeout (in minutes).</p>
newtype ExecutionConfiguration = ExecutionConfiguration 
  { "JobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes)
  , "AccountsCleanup'" :: NullOrUndefined (AccountsCleanup)
  , "AppPackagesCleanup'" :: NullOrUndefined (AppPackagesCleanup)
  }
derive instance newtypeExecutionConfiguration :: Newtype ExecutionConfiguration _


newtype ExecutionResult = ExecutionResult String
derive instance newtypeExecutionResult :: Newtype ExecutionResult _


newtype ExecutionResultCode = ExecutionResultCode String
derive instance newtypeExecutionResultCode :: Newtype ExecutionResultCode _


newtype ExecutionStatus = ExecutionStatus String
derive instance newtypeExecutionStatus :: Newtype ExecutionStatus _


newtype Filter = Filter String
derive instance newtypeFilter :: Newtype Filter _


-- | <p>Represents the request sent to retrieve the account settings.</p>
newtype GetAccountSettingsRequest = GetAccountSettingsRequest 
  { 
  }
derive instance newtypeGetAccountSettingsRequest :: Newtype GetAccountSettingsRequest _


-- | <p>Represents the account settings return values from the <code>GetAccountSettings</code> request.</p>
newtype GetAccountSettingsResult = GetAccountSettingsResult 
  { "AccountSettings'" :: NullOrUndefined (AccountSettings)
  }
derive instance newtypeGetAccountSettingsResult :: Newtype GetAccountSettingsResult _


-- | <p>Represents a request to the get device pool compatibility operation.</p>
newtype GetDevicePoolCompatibilityRequest = GetDevicePoolCompatibilityRequest 
  { "DevicePoolArn'" :: (AmazonResourceName)
  , "AppArn'" :: NullOrUndefined (AmazonResourceName)
  , "TestType'" :: NullOrUndefined (TestType)
  , "Test'" :: NullOrUndefined (ScheduleRunTest)
  }
derive instance newtypeGetDevicePoolCompatibilityRequest :: Newtype GetDevicePoolCompatibilityRequest _


-- | <p>Represents the result of describe device pool compatibility request.</p>
newtype GetDevicePoolCompatibilityResult = GetDevicePoolCompatibilityResult 
  { "CompatibleDevices'" :: NullOrUndefined (DevicePoolCompatibilityResults)
  , "IncompatibleDevices'" :: NullOrUndefined (DevicePoolCompatibilityResults)
  }
derive instance newtypeGetDevicePoolCompatibilityResult :: Newtype GetDevicePoolCompatibilityResult _


-- | <p>Represents a request to the get device pool operation.</p>
newtype GetDevicePoolRequest = GetDevicePoolRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetDevicePoolRequest :: Newtype GetDevicePoolRequest _


-- | <p>Represents the result of a get device pool request.</p>
newtype GetDevicePoolResult = GetDevicePoolResult 
  { "DevicePool'" :: NullOrUndefined (DevicePool)
  }
derive instance newtypeGetDevicePoolResult :: Newtype GetDevicePoolResult _


-- | <p>Represents a request to the get device request.</p>
newtype GetDeviceRequest = GetDeviceRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetDeviceRequest :: Newtype GetDeviceRequest _


-- | <p>Represents the result of a get device request.</p>
newtype GetDeviceResult = GetDeviceResult 
  { "Device'" :: NullOrUndefined (Device)
  }
derive instance newtypeGetDeviceResult :: Newtype GetDeviceResult _


-- | <p>Represents a request to the get job operation.</p>
newtype GetJobRequest = GetJobRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetJobRequest :: Newtype GetJobRequest _


-- | <p>Represents the result of a get job request.</p>
newtype GetJobResult = GetJobResult 
  { "Job'" :: NullOrUndefined (Job)
  }
derive instance newtypeGetJobResult :: Newtype GetJobResult _


newtype GetNetworkProfileRequest = GetNetworkProfileRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetNetworkProfileRequest :: Newtype GetNetworkProfileRequest _


newtype GetNetworkProfileResult = GetNetworkProfileResult 
  { "NetworkProfile'" :: NullOrUndefined (NetworkProfile)
  }
derive instance newtypeGetNetworkProfileResult :: Newtype GetNetworkProfileResult _


-- | <p>Represents the request to retrieve the offering status for the specified customer or account.</p>
newtype GetOfferingStatusRequest = GetOfferingStatusRequest 
  { "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeGetOfferingStatusRequest :: Newtype GetOfferingStatusRequest _


-- | <p>Returns the status result for a device offering.</p>
newtype GetOfferingStatusResult = GetOfferingStatusResult 
  { "Current'" :: NullOrUndefined (OfferingStatusMap)
  , "NextPeriod'" :: NullOrUndefined (OfferingStatusMap)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeGetOfferingStatusResult :: Newtype GetOfferingStatusResult _


-- | <p>Represents a request to the get project operation.</p>
newtype GetProjectRequest = GetProjectRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetProjectRequest :: Newtype GetProjectRequest _


-- | <p>Represents the result of a get project request.</p>
newtype GetProjectResult = GetProjectResult 
  { "Project'" :: NullOrUndefined (Project)
  }
derive instance newtypeGetProjectResult :: Newtype GetProjectResult _


-- | <p>Represents the request to get information about the specified remote access session.</p>
newtype GetRemoteAccessSessionRequest = GetRemoteAccessSessionRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetRemoteAccessSessionRequest :: Newtype GetRemoteAccessSessionRequest _


-- | <p>Represents the response from the server that lists detailed information about the remote access session.</p>
newtype GetRemoteAccessSessionResult = GetRemoteAccessSessionResult 
  { "RemoteAccessSession'" :: NullOrUndefined (RemoteAccessSession)
  }
derive instance newtypeGetRemoteAccessSessionResult :: Newtype GetRemoteAccessSessionResult _


-- | <p>Represents a request to the get run operation.</p>
newtype GetRunRequest = GetRunRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetRunRequest :: Newtype GetRunRequest _


-- | <p>Represents the result of a get run request.</p>
newtype GetRunResult = GetRunResult 
  { "Run'" :: NullOrUndefined (Run)
  }
derive instance newtypeGetRunResult :: Newtype GetRunResult _


-- | <p>Represents a request to the get suite operation.</p>
newtype GetSuiteRequest = GetSuiteRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetSuiteRequest :: Newtype GetSuiteRequest _


-- | <p>Represents the result of a get suite request.</p>
newtype GetSuiteResult = GetSuiteResult 
  { "Suite'" :: NullOrUndefined (Suite)
  }
derive instance newtypeGetSuiteResult :: Newtype GetSuiteResult _


-- | <p>Represents a request to the get test operation.</p>
newtype GetTestRequest = GetTestRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetTestRequest :: Newtype GetTestRequest _


-- | <p>Represents the result of a get test request.</p>
newtype GetTestResult = GetTestResult 
  { "Test'" :: NullOrUndefined (Test)
  }
derive instance newtypeGetTestResult :: Newtype GetTestResult _


-- | <p>Represents a request to the get upload operation.</p>
newtype GetUploadRequest = GetUploadRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetUploadRequest :: Newtype GetUploadRequest _


-- | <p>Represents the result of a get upload request.</p>
newtype GetUploadResult = GetUploadResult 
  { "Upload'" :: NullOrUndefined (Upload)
  }
derive instance newtypeGetUploadResult :: Newtype GetUploadResult _


newtype HostAddress = HostAddress String
derive instance newtypeHostAddress :: Newtype HostAddress _


-- | <p>An entity with the same name already exists.</p>
newtype IdempotencyException = IdempotencyException 
  { "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeIdempotencyException :: Newtype IdempotencyException _


-- | <p>Represents information about incompatibility.</p>
newtype IncompatibilityMessage = IncompatibilityMessage 
  { "Message'" :: NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined (DeviceAttribute)
  }
derive instance newtypeIncompatibilityMessage :: Newtype IncompatibilityMessage _


newtype IncompatibilityMessages = IncompatibilityMessages (Array IncompatibilityMessage)
derive instance newtypeIncompatibilityMessages :: Newtype IncompatibilityMessages _


-- | <p>Represents the request to install an Android application (in .apk format) or an iOS application (in .ipa format) as part of a remote access session.</p>
newtype InstallToRemoteAccessSessionRequest = InstallToRemoteAccessSessionRequest 
  { "RemoteAccessSessionArn'" :: (AmazonResourceName)
  , "AppArn'" :: (AmazonResourceName)
  }
derive instance newtypeInstallToRemoteAccessSessionRequest :: Newtype InstallToRemoteAccessSessionRequest _


-- | <p>Represents the response from the server after AWS Device Farm makes a request to install to a remote access session.</p>
newtype InstallToRemoteAccessSessionResult = InstallToRemoteAccessSessionResult 
  { "AppUpload'" :: NullOrUndefined (Upload)
  }
derive instance newtypeInstallToRemoteAccessSessionResult :: Newtype InstallToRemoteAccessSessionResult _


newtype InteractionMode = InteractionMode String
derive instance newtypeInteractionMode :: Newtype InteractionMode _


newtype IosPaths = IosPaths (Array String)
derive instance newtypeIosPaths :: Newtype IosPaths _


-- | <p>Represents a device.</p>
newtype Job = Job 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Type'" :: NullOrUndefined (TestType)
  , "Created'" :: NullOrUndefined (DateTime)
  , "Status'" :: NullOrUndefined (ExecutionStatus)
  , "Result'" :: NullOrUndefined (ExecutionResult)
  , "Started'" :: NullOrUndefined (DateTime)
  , "Stopped'" :: NullOrUndefined (DateTime)
  , "Counters'" :: NullOrUndefined (Counters)
  , "Message'" :: NullOrUndefined (Message)
  , "Device'" :: NullOrUndefined (Device)
  , "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes)
  }
derive instance newtypeJob :: Newtype Job _


newtype JobTimeoutMinutes = JobTimeoutMinutes Int
derive instance newtypeJobTimeoutMinutes :: Newtype JobTimeoutMinutes _


newtype Jobs = Jobs (Array Job)
derive instance newtypeJobs :: Newtype Jobs _


-- | <p>A limit was exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


-- | <p>Represents a request to the list artifacts operation.</p>
newtype ListArtifactsRequest = ListArtifactsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Type'" :: (ArtifactCategory)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListArtifactsRequest :: Newtype ListArtifactsRequest _


-- | <p>Represents the result of a list artifacts operation.</p>
newtype ListArtifactsResult = ListArtifactsResult 
  { "Artifacts'" :: NullOrUndefined (Artifacts)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListArtifactsResult :: Newtype ListArtifactsResult _


-- | <p>Represents the result of a list device pools request.</p>
newtype ListDevicePoolsRequest = ListDevicePoolsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Type'" :: NullOrUndefined (DevicePoolType)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListDevicePoolsRequest :: Newtype ListDevicePoolsRequest _


-- | <p>Represents the result of a list device pools request.</p>
newtype ListDevicePoolsResult = ListDevicePoolsResult 
  { "DevicePools'" :: NullOrUndefined (DevicePools)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListDevicePoolsResult :: Newtype ListDevicePoolsResult _


-- | <p>Represents the result of a list devices request.</p>
newtype ListDevicesRequest = ListDevicesRequest 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListDevicesRequest :: Newtype ListDevicesRequest _


-- | <p>Represents the result of a list devices operation.</p>
newtype ListDevicesResult = ListDevicesResult 
  { "Devices'" :: NullOrUndefined (Devices)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListDevicesResult :: Newtype ListDevicesResult _


-- | <p>Represents a request to the list jobs operation.</p>
newtype ListJobsRequest = ListJobsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListJobsRequest :: Newtype ListJobsRequest _


-- | <p>Represents the result of a list jobs request.</p>
newtype ListJobsResult = ListJobsResult 
  { "Jobs'" :: NullOrUndefined (Jobs)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListJobsResult :: Newtype ListJobsResult _


newtype ListNetworkProfilesRequest = ListNetworkProfilesRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Type'" :: NullOrUndefined (NetworkProfileType)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListNetworkProfilesRequest :: Newtype ListNetworkProfilesRequest _


newtype ListNetworkProfilesResult = ListNetworkProfilesResult 
  { "NetworkProfiles'" :: NullOrUndefined (NetworkProfiles)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListNetworkProfilesResult :: Newtype ListNetworkProfilesResult _


newtype ListOfferingPromotionsRequest = ListOfferingPromotionsRequest 
  { "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListOfferingPromotionsRequest :: Newtype ListOfferingPromotionsRequest _


newtype ListOfferingPromotionsResult = ListOfferingPromotionsResult 
  { "OfferingPromotions'" :: NullOrUndefined (OfferingPromotions)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListOfferingPromotionsResult :: Newtype ListOfferingPromotionsResult _


-- | <p>Represents the request to list the offering transaction history.</p>
newtype ListOfferingTransactionsRequest = ListOfferingTransactionsRequest 
  { "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListOfferingTransactionsRequest :: Newtype ListOfferingTransactionsRequest _


-- | <p>Returns the transaction log of the specified offerings.</p>
newtype ListOfferingTransactionsResult = ListOfferingTransactionsResult 
  { "OfferingTransactions'" :: NullOrUndefined (OfferingTransactions)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListOfferingTransactionsResult :: Newtype ListOfferingTransactionsResult _


-- | <p>Represents the request to list all offerings.</p>
newtype ListOfferingsRequest = ListOfferingsRequest 
  { "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListOfferingsRequest :: Newtype ListOfferingsRequest _


-- | <p>Represents the return values of the list of offerings.</p>
newtype ListOfferingsResult = ListOfferingsResult 
  { "Offerings'" :: NullOrUndefined (Offerings)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListOfferingsResult :: Newtype ListOfferingsResult _


-- | <p>Represents a request to the list projects operation.</p>
newtype ListProjectsRequest = ListProjectsRequest 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListProjectsRequest :: Newtype ListProjectsRequest _


-- | <p>Represents the result of a list projects request.</p>
newtype ListProjectsResult = ListProjectsResult 
  { "Projects'" :: NullOrUndefined (Projects)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListProjectsResult :: Newtype ListProjectsResult _


-- | <p>Represents the request to return information about the remote access session.</p>
newtype ListRemoteAccessSessionsRequest = ListRemoteAccessSessionsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListRemoteAccessSessionsRequest :: Newtype ListRemoteAccessSessionsRequest _


-- | <p>Represents the response from the server after AWS Device Farm makes a request to return information about the remote access session.</p>
newtype ListRemoteAccessSessionsResult = ListRemoteAccessSessionsResult 
  { "RemoteAccessSessions'" :: NullOrUndefined (RemoteAccessSessions)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListRemoteAccessSessionsResult :: Newtype ListRemoteAccessSessionsResult _


-- | <p>Represents a request to the list runs operation.</p>
newtype ListRunsRequest = ListRunsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListRunsRequest :: Newtype ListRunsRequest _


-- | <p>Represents the result of a list runs request.</p>
newtype ListRunsResult = ListRunsResult 
  { "Runs'" :: NullOrUndefined (Runs)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListRunsResult :: Newtype ListRunsResult _


-- | <p>Represents a request to the list samples operation.</p>
newtype ListSamplesRequest = ListSamplesRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListSamplesRequest :: Newtype ListSamplesRequest _


-- | <p>Represents the result of a list samples request.</p>
newtype ListSamplesResult = ListSamplesResult 
  { "Samples'" :: NullOrUndefined (Samples)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListSamplesResult :: Newtype ListSamplesResult _


-- | <p>Represents a request to the list suites operation.</p>
newtype ListSuitesRequest = ListSuitesRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListSuitesRequest :: Newtype ListSuitesRequest _


-- | <p>Represents the result of a list suites request.</p>
newtype ListSuitesResult = ListSuitesResult 
  { "Suites'" :: NullOrUndefined (Suites)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListSuitesResult :: Newtype ListSuitesResult _


-- | <p>Represents a request to the list tests operation.</p>
newtype ListTestsRequest = ListTestsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListTestsRequest :: Newtype ListTestsRequest _


-- | <p>Represents the result of a list tests request.</p>
newtype ListTestsResult = ListTestsResult 
  { "Tests'" :: NullOrUndefined (Tests)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListTestsResult :: Newtype ListTestsResult _


-- | <p>Represents a request to the list unique problems operation.</p>
newtype ListUniqueProblemsRequest = ListUniqueProblemsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListUniqueProblemsRequest :: Newtype ListUniqueProblemsRequest _


-- | <p>Represents the result of a list unique problems request.</p>
newtype ListUniqueProblemsResult = ListUniqueProblemsResult 
  { "UniqueProblems'" :: NullOrUndefined (UniqueProblemsByExecutionResultMap)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListUniqueProblemsResult :: Newtype ListUniqueProblemsResult _


-- | <p>Represents a request to the list uploads operation.</p>
newtype ListUploadsRequest = ListUploadsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListUploadsRequest :: Newtype ListUploadsRequest _


-- | <p>Represents the result of a list uploads request.</p>
newtype ListUploadsResult = ListUploadsResult 
  { "Uploads'" :: NullOrUndefined (Uploads)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListUploadsResult :: Newtype ListUploadsResult _


-- | <p>Represents a latitude and longitude pair, expressed in geographic coordinate system degrees (for example 47.6204, -122.3491).</p> <p>Elevation is currently not supported.</p>
newtype Location = Location 
  { "Latitude'" :: (Number)
  , "Longitude'" :: (Number)
  }
derive instance newtypeLocation :: Newtype Location _


newtype MaxSlotMap = MaxSlotMap (Map String Int)
derive instance newtypeMaxSlotMap :: Newtype MaxSlotMap _


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


newtype Metadata = Metadata String
derive instance newtypeMetadata :: Newtype Metadata _


-- | <p>A number representing the monetary amount for an offering or transaction.</p>
newtype MonetaryAmount = MonetaryAmount 
  { "Amount'" :: NullOrUndefined (Number)
  , "CurrencyCode'" :: NullOrUndefined (CurrencyCode)
  }
derive instance newtypeMonetaryAmount :: Newtype MonetaryAmount _


newtype Name = Name String
derive instance newtypeName :: Newtype Name _


-- | <p>An array of settings that describes characteristics of a network profile.</p>
newtype NetworkProfile = NetworkProfile 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Description'" :: NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined (NetworkProfileType)
  , "UplinkBandwidthBits'" :: NullOrUndefined (Number)
  , "DownlinkBandwidthBits'" :: NullOrUndefined (Number)
  , "UplinkDelayMs'" :: NullOrUndefined (Number)
  , "DownlinkDelayMs'" :: NullOrUndefined (Number)
  , "UplinkJitterMs'" :: NullOrUndefined (Number)
  , "DownlinkJitterMs'" :: NullOrUndefined (Number)
  , "UplinkLossPercent'" :: NullOrUndefined (PercentInteger)
  , "DownlinkLossPercent'" :: NullOrUndefined (PercentInteger)
  }
derive instance newtypeNetworkProfile :: Newtype NetworkProfile _


newtype NetworkProfileType = NetworkProfileType String
derive instance newtypeNetworkProfileType :: Newtype NetworkProfileType _


newtype NetworkProfiles = NetworkProfiles (Array NetworkProfile)
derive instance newtypeNetworkProfiles :: Newtype NetworkProfiles _


-- | <p>Exception gets thrown when a user is not eligible to perform the specified transaction.</p>
newtype NotEligibleException = NotEligibleException 
  { "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeNotEligibleException :: Newtype NotEligibleException _


-- | <p>The specified entity was not found.</p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


-- | <p>Represents the metadata of a device offering.</p>
newtype Offering = Offering 
  { "Id'" :: NullOrUndefined (OfferingIdentifier)
  , "Description'" :: NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined (OfferingType)
  , "Platform'" :: NullOrUndefined (DevicePlatform)
  , "RecurringCharges'" :: NullOrUndefined (RecurringCharges)
  }
derive instance newtypeOffering :: Newtype Offering _


newtype OfferingIdentifier = OfferingIdentifier String
derive instance newtypeOfferingIdentifier :: Newtype OfferingIdentifier _


-- | <p>Represents information about an offering promotion.</p>
newtype OfferingPromotion = OfferingPromotion 
  { "Id'" :: NullOrUndefined (OfferingPromotionIdentifier)
  , "Description'" :: NullOrUndefined (Message)
  }
derive instance newtypeOfferingPromotion :: Newtype OfferingPromotion _


newtype OfferingPromotionIdentifier = OfferingPromotionIdentifier String
derive instance newtypeOfferingPromotionIdentifier :: Newtype OfferingPromotionIdentifier _


newtype OfferingPromotions = OfferingPromotions (Array OfferingPromotion)
derive instance newtypeOfferingPromotions :: Newtype OfferingPromotions _


-- | <p>The status of the offering.</p>
newtype OfferingStatus = OfferingStatus 
  { "Type'" :: NullOrUndefined (OfferingTransactionType)
  , "Offering'" :: NullOrUndefined (Offering)
  , "Quantity'" :: NullOrUndefined (Int)
  , "EffectiveOn'" :: NullOrUndefined (DateTime)
  }
derive instance newtypeOfferingStatus :: Newtype OfferingStatus _


newtype OfferingStatusMap = OfferingStatusMap (Map OfferingIdentifier OfferingStatus)
derive instance newtypeOfferingStatusMap :: Newtype OfferingStatusMap _


-- | <p>Represents the metadata of an offering transaction.</p>
newtype OfferingTransaction = OfferingTransaction 
  { "OfferingStatus'" :: NullOrUndefined (OfferingStatus)
  , "TransactionId'" :: NullOrUndefined (TransactionIdentifier)
  , "OfferingPromotionId'" :: NullOrUndefined (OfferingPromotionIdentifier)
  , "CreatedOn'" :: NullOrUndefined (DateTime)
  , "Cost'" :: NullOrUndefined (MonetaryAmount)
  }
derive instance newtypeOfferingTransaction :: Newtype OfferingTransaction _


newtype OfferingTransactionType = OfferingTransactionType String
derive instance newtypeOfferingTransactionType :: Newtype OfferingTransactionType _


newtype OfferingTransactions = OfferingTransactions (Array OfferingTransaction)
derive instance newtypeOfferingTransactions :: Newtype OfferingTransactions _


newtype OfferingType = OfferingType String
derive instance newtypeOfferingType :: Newtype OfferingType _


newtype Offerings = Offerings (Array Offering)
derive instance newtypeOfferings :: Newtype Offerings _


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _


newtype PercentInteger = PercentInteger Int
derive instance newtypePercentInteger :: Newtype PercentInteger _


-- | <p>Represents a specific warning or failure.</p>
newtype Problem = Problem 
  { "Run'" :: NullOrUndefined (ProblemDetail)
  , "Job'" :: NullOrUndefined (ProblemDetail)
  , "Suite'" :: NullOrUndefined (ProblemDetail)
  , "Test'" :: NullOrUndefined (ProblemDetail)
  , "Device'" :: NullOrUndefined (Device)
  , "Result'" :: NullOrUndefined (ExecutionResult)
  , "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeProblem :: Newtype Problem _


-- | <p>Information about a problem detail.</p>
newtype ProblemDetail = ProblemDetail 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  }
derive instance newtypeProblemDetail :: Newtype ProblemDetail _


newtype Problems = Problems (Array Problem)
derive instance newtypeProblems :: Newtype Problems _


-- | <p>Represents an operating-system neutral workspace for running and managing tests.</p>
newtype Project = Project 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes)
  , "Created'" :: NullOrUndefined (DateTime)
  }
derive instance newtypeProject :: Newtype Project _


newtype Projects = Projects (Array Project)
derive instance newtypeProjects :: Newtype Projects _


-- | <p>Represents a request for a purchase offering.</p>
newtype PurchaseOfferingRequest = PurchaseOfferingRequest 
  { "OfferingId'" :: NullOrUndefined (OfferingIdentifier)
  , "Quantity'" :: NullOrUndefined (Int)
  , "OfferingPromotionId'" :: NullOrUndefined (OfferingPromotionIdentifier)
  }
derive instance newtypePurchaseOfferingRequest :: Newtype PurchaseOfferingRequest _


-- | <p>The result of the purchase offering (e.g., success or failure).</p>
newtype PurchaseOfferingResult = PurchaseOfferingResult 
  { "OfferingTransaction'" :: NullOrUndefined (OfferingTransaction)
  }
derive instance newtypePurchaseOfferingResult :: Newtype PurchaseOfferingResult _


newtype PurchasedDevicesMap = PurchasedDevicesMap (Map DevicePlatform Int)
derive instance newtypePurchasedDevicesMap :: Newtype PurchasedDevicesMap _


-- | <p>Represents the set of radios and their states on a device. Examples of radios include Wi-Fi, GPS, Bluetooth, and NFC.</p>
newtype Radios = Radios 
  { "Wifi'" :: NullOrUndefined (Boolean)
  , "Bluetooth'" :: NullOrUndefined (Boolean)
  , "Nfc'" :: NullOrUndefined (Boolean)
  , "Gps'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeRadios :: Newtype Radios _


-- | <p>Specifies whether charges for devices will be recurring.</p>
newtype RecurringCharge = RecurringCharge 
  { "Cost'" :: NullOrUndefined (MonetaryAmount)
  , "Frequency'" :: NullOrUndefined (RecurringChargeFrequency)
  }
derive instance newtypeRecurringCharge :: Newtype RecurringCharge _


newtype RecurringChargeFrequency = RecurringChargeFrequency String
derive instance newtypeRecurringChargeFrequency :: Newtype RecurringChargeFrequency _


newtype RecurringCharges = RecurringCharges (Array RecurringCharge)
derive instance newtypeRecurringCharges :: Newtype RecurringCharges _


-- | <p>Represents information about the remote access session.</p>
newtype RemoteAccessSession = RemoteAccessSession 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Created'" :: NullOrUndefined (DateTime)
  , "Status'" :: NullOrUndefined (ExecutionStatus)
  , "Result'" :: NullOrUndefined (ExecutionResult)
  , "Message'" :: NullOrUndefined (Message)
  , "Started'" :: NullOrUndefined (DateTime)
  , "Stopped'" :: NullOrUndefined (DateTime)
  , "Device'" :: NullOrUndefined (Device)
  , "RemoteDebugEnabled'" :: NullOrUndefined (Boolean)
  , "RemoteRecordEnabled'" :: NullOrUndefined (Boolean)
  , "RemoteRecordAppArn'" :: NullOrUndefined (AmazonResourceName)
  , "HostAddress'" :: NullOrUndefined (HostAddress)
  , "ClientId'" :: NullOrUndefined (ClientId)
  , "BillingMethod'" :: NullOrUndefined (BillingMethod)
  , "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes)
  , "Endpoint'" :: NullOrUndefined (String)
  , "DeviceUdid'" :: NullOrUndefined (String)
  , "InteractionMode'" :: NullOrUndefined (InteractionMode)
  }
derive instance newtypeRemoteAccessSession :: Newtype RemoteAccessSession _


newtype RemoteAccessSessions = RemoteAccessSessions (Array RemoteAccessSession)
derive instance newtypeRemoteAccessSessions :: Newtype RemoteAccessSessions _


-- | <p>A request representing an offering renewal.</p>
newtype RenewOfferingRequest = RenewOfferingRequest 
  { "OfferingId'" :: NullOrUndefined (OfferingIdentifier)
  , "Quantity'" :: NullOrUndefined (Int)
  }
derive instance newtypeRenewOfferingRequest :: Newtype RenewOfferingRequest _


-- | <p>The result of a renewal offering.</p>
newtype RenewOfferingResult = RenewOfferingResult 
  { "OfferingTransaction'" :: NullOrUndefined (OfferingTransaction)
  }
derive instance newtypeRenewOfferingResult :: Newtype RenewOfferingResult _


-- | <p>Represents the screen resolution of a device in height and width, expressed in pixels.</p>
newtype Resolution = Resolution 
  { "Width'" :: NullOrUndefined (Int)
  , "Height'" :: NullOrUndefined (Int)
  }
derive instance newtypeResolution :: Newtype Resolution _


-- | <p>Represents a condition for a device pool.</p>
newtype Rule = Rule 
  { "Attribute'" :: NullOrUndefined (DeviceAttribute)
  , "Operator'" :: NullOrUndefined (RuleOperator)
  , "Value'" :: NullOrUndefined (String)
  }
derive instance newtypeRule :: Newtype Rule _


newtype RuleOperator = RuleOperator String
derive instance newtypeRuleOperator :: Newtype RuleOperator _


newtype Rules = Rules (Array Rule)
derive instance newtypeRules :: Newtype Rules _


-- | <p>Represents a test run on a set of devices with a given app package, test parameters, etc.</p>
newtype Run = Run 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Type'" :: NullOrUndefined (TestType)
  , "Platform'" :: NullOrUndefined (DevicePlatform)
  , "Created'" :: NullOrUndefined (DateTime)
  , "Status'" :: NullOrUndefined (ExecutionStatus)
  , "Result'" :: NullOrUndefined (ExecutionResult)
  , "Started'" :: NullOrUndefined (DateTime)
  , "Stopped'" :: NullOrUndefined (DateTime)
  , "Counters'" :: NullOrUndefined (Counters)
  , "Message'" :: NullOrUndefined (Message)
  , "TotalJobs'" :: NullOrUndefined (Int)
  , "CompletedJobs'" :: NullOrUndefined (Int)
  , "BillingMethod'" :: NullOrUndefined (BillingMethod)
  , "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes)
  , "NetworkProfile'" :: NullOrUndefined (NetworkProfile)
  , "ParsingResultUrl'" :: NullOrUndefined (String)
  , "ResultCode'" :: NullOrUndefined (ExecutionResultCode)
  , "Seed'" :: NullOrUndefined (Int)
  , "AppUpload'" :: NullOrUndefined (AmazonResourceName)
  , "EventCount'" :: NullOrUndefined (Int)
  , "JobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes)
  , "DevicePoolArn'" :: NullOrUndefined (AmazonResourceName)
  , "Locale'" :: NullOrUndefined (String)
  , "Radios'" :: NullOrUndefined (Radios)
  , "Location'" :: NullOrUndefined (Location)
  , "CustomerArtifactPaths'" :: NullOrUndefined (CustomerArtifactPaths)
  , "WebUrl'" :: NullOrUndefined (String)
  }
derive instance newtypeRun :: Newtype Run _


newtype Runs = Runs (Array Run)
derive instance newtypeRuns :: Newtype Runs _


-- | <p>Represents a sample of performance data.</p>
newtype Sample = Sample 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Type'" :: NullOrUndefined (SampleType)
  , "Url'" :: NullOrUndefined (URL)
  }
derive instance newtypeSample :: Newtype Sample _


newtype SampleType = SampleType String
derive instance newtypeSampleType :: Newtype SampleType _


newtype Samples = Samples (Array Sample)
derive instance newtypeSamples :: Newtype Samples _


-- | <p>Represents the settings for a run. Includes things like location, radio states, auxiliary apps, and network profiles.</p>
newtype ScheduleRunConfiguration = ScheduleRunConfiguration 
  { "ExtraDataPackageArn'" :: NullOrUndefined (AmazonResourceName)
  , "NetworkProfileArn'" :: NullOrUndefined (AmazonResourceName)
  , "Locale'" :: NullOrUndefined (String)
  , "Location'" :: NullOrUndefined (Location)
  , "CustomerArtifactPaths'" :: NullOrUndefined (CustomerArtifactPaths)
  , "Radios'" :: NullOrUndefined (Radios)
  , "AuxiliaryApps'" :: NullOrUndefined (AmazonResourceNames)
  , "BillingMethod'" :: NullOrUndefined (BillingMethod)
  }
derive instance newtypeScheduleRunConfiguration :: Newtype ScheduleRunConfiguration _


-- | <p>Represents a request to the schedule run operation.</p>
newtype ScheduleRunRequest = ScheduleRunRequest 
  { "ProjectArn'" :: (AmazonResourceName)
  , "AppArn'" :: NullOrUndefined (AmazonResourceName)
  , "DevicePoolArn'" :: (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Test'" :: (ScheduleRunTest)
  , "Configuration'" :: NullOrUndefined (ScheduleRunConfiguration)
  , "ExecutionConfiguration'" :: NullOrUndefined (ExecutionConfiguration)
  }
derive instance newtypeScheduleRunRequest :: Newtype ScheduleRunRequest _


-- | <p>Represents the result of a schedule run request.</p>
newtype ScheduleRunResult = ScheduleRunResult 
  { "Run'" :: NullOrUndefined (Run)
  }
derive instance newtypeScheduleRunResult :: Newtype ScheduleRunResult _


-- | <p>Represents additional test settings.</p>
newtype ScheduleRunTest = ScheduleRunTest 
  { "Type'" :: (TestType)
  , "TestPackageArn'" :: NullOrUndefined (AmazonResourceName)
  , "Filter'" :: NullOrUndefined (Filter)
  , "Parameters'" :: NullOrUndefined (TestParameters)
  }
derive instance newtypeScheduleRunTest :: Newtype ScheduleRunTest _


-- | <p>There was a problem with the service account.</p>
newtype ServiceAccountException = ServiceAccountException 
  { "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeServiceAccountException :: Newtype ServiceAccountException _


newtype SshPublicKey = SshPublicKey String
derive instance newtypeSshPublicKey :: Newtype SshPublicKey _


-- | <p>Represents the request to stop the remote access session.</p>
newtype StopRemoteAccessSessionRequest = StopRemoteAccessSessionRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeStopRemoteAccessSessionRequest :: Newtype StopRemoteAccessSessionRequest _


-- | <p>Represents the response from the server that describes the remote access session when AWS Device Farm stops the session.</p>
newtype StopRemoteAccessSessionResult = StopRemoteAccessSessionResult 
  { "RemoteAccessSession'" :: NullOrUndefined (RemoteAccessSession)
  }
derive instance newtypeStopRemoteAccessSessionResult :: Newtype StopRemoteAccessSessionResult _


-- | <p>Represents the request to stop a specific run.</p>
newtype StopRunRequest = StopRunRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeStopRunRequest :: Newtype StopRunRequest _


-- | <p>Represents the results of your stop run attempt.</p>
newtype StopRunResult = StopRunResult 
  { "Run'" :: NullOrUndefined (Run)
  }
derive instance newtypeStopRunResult :: Newtype StopRunResult _


-- | <p>Represents a collection of one or more tests.</p>
newtype Suite = Suite 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Type'" :: NullOrUndefined (TestType)
  , "Created'" :: NullOrUndefined (DateTime)
  , "Status'" :: NullOrUndefined (ExecutionStatus)
  , "Result'" :: NullOrUndefined (ExecutionResult)
  , "Started'" :: NullOrUndefined (DateTime)
  , "Stopped'" :: NullOrUndefined (DateTime)
  , "Counters'" :: NullOrUndefined (Counters)
  , "Message'" :: NullOrUndefined (Message)
  , "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes)
  }
derive instance newtypeSuite :: Newtype Suite _


newtype Suites = Suites (Array Suite)
derive instance newtypeSuites :: Newtype Suites _


-- | <p>Represents a condition that is evaluated.</p>
newtype Test = Test 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Type'" :: NullOrUndefined (TestType)
  , "Created'" :: NullOrUndefined (DateTime)
  , "Status'" :: NullOrUndefined (ExecutionStatus)
  , "Result'" :: NullOrUndefined (ExecutionResult)
  , "Started'" :: NullOrUndefined (DateTime)
  , "Stopped'" :: NullOrUndefined (DateTime)
  , "Counters'" :: NullOrUndefined (Counters)
  , "Message'" :: NullOrUndefined (Message)
  , "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes)
  }
derive instance newtypeTest :: Newtype Test _


newtype TestParameters = TestParameters (Map String String)
derive instance newtypeTestParameters :: Newtype TestParameters _


newtype TestType = TestType String
derive instance newtypeTestType :: Newtype TestType _


newtype Tests = Tests (Array Test)
derive instance newtypeTests :: Newtype Tests _


newtype TransactionIdentifier = TransactionIdentifier String
derive instance newtypeTransactionIdentifier :: Newtype TransactionIdentifier _


-- | <p>Represents information about free trial device minutes for an AWS account.</p>
newtype TrialMinutes = TrialMinutes 
  { "Total'" :: NullOrUndefined (Number)
  , "Remaining'" :: NullOrUndefined (Number)
  }
derive instance newtypeTrialMinutes :: Newtype TrialMinutes _


newtype URL = URL String
derive instance newtypeURL :: Newtype URL _


-- | <p>A collection of one or more problems, grouped by their result.</p>
newtype UniqueProblem = UniqueProblem 
  { "Message'" :: NullOrUndefined (Message)
  , "Problems'" :: NullOrUndefined (Problems)
  }
derive instance newtypeUniqueProblem :: Newtype UniqueProblem _


newtype UniqueProblems = UniqueProblems (Array UniqueProblem)
derive instance newtypeUniqueProblems :: Newtype UniqueProblems _


newtype UniqueProblemsByExecutionResultMap = UniqueProblemsByExecutionResultMap (Map ExecutionResult UniqueProblems)
derive instance newtypeUniqueProblemsByExecutionResultMap :: Newtype UniqueProblemsByExecutionResultMap _


-- | <p>Represents a request to the update device pool operation.</p>
newtype UpdateDevicePoolRequest = UpdateDevicePoolRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Description'" :: NullOrUndefined (Message)
  , "Rules'" :: NullOrUndefined (Rules)
  }
derive instance newtypeUpdateDevicePoolRequest :: Newtype UpdateDevicePoolRequest _


-- | <p>Represents the result of an update device pool request.</p>
newtype UpdateDevicePoolResult = UpdateDevicePoolResult 
  { "DevicePool'" :: NullOrUndefined (DevicePool)
  }
derive instance newtypeUpdateDevicePoolResult :: Newtype UpdateDevicePoolResult _


newtype UpdateNetworkProfileRequest = UpdateNetworkProfileRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Description'" :: NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined (NetworkProfileType)
  , "UplinkBandwidthBits'" :: NullOrUndefined (Number)
  , "DownlinkBandwidthBits'" :: NullOrUndefined (Number)
  , "UplinkDelayMs'" :: NullOrUndefined (Number)
  , "DownlinkDelayMs'" :: NullOrUndefined (Number)
  , "UplinkJitterMs'" :: NullOrUndefined (Number)
  , "DownlinkJitterMs'" :: NullOrUndefined (Number)
  , "UplinkLossPercent'" :: NullOrUndefined (PercentInteger)
  , "DownlinkLossPercent'" :: NullOrUndefined (PercentInteger)
  }
derive instance newtypeUpdateNetworkProfileRequest :: Newtype UpdateNetworkProfileRequest _


newtype UpdateNetworkProfileResult = UpdateNetworkProfileResult 
  { "NetworkProfile'" :: NullOrUndefined (NetworkProfile)
  }
derive instance newtypeUpdateNetworkProfileResult :: Newtype UpdateNetworkProfileResult _


-- | <p>Represents a request to the update project operation.</p>
newtype UpdateProjectRequest = UpdateProjectRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes)
  }
derive instance newtypeUpdateProjectRequest :: Newtype UpdateProjectRequest _


-- | <p>Represents the result of an update project request.</p>
newtype UpdateProjectResult = UpdateProjectResult 
  { "Project'" :: NullOrUndefined (Project)
  }
derive instance newtypeUpdateProjectResult :: Newtype UpdateProjectResult _


-- | <p>An app or a set of one or more tests to upload or that have been uploaded.</p>
newtype Upload = Upload 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Created'" :: NullOrUndefined (DateTime)
  , "Type'" :: NullOrUndefined (UploadType)
  , "Status'" :: NullOrUndefined (UploadStatus)
  , "Url'" :: NullOrUndefined (URL)
  , "Metadata'" :: NullOrUndefined (Metadata)
  , "ContentType'" :: NullOrUndefined (ContentType)
  , "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeUpload :: Newtype Upload _


newtype UploadStatus = UploadStatus String
derive instance newtypeUploadStatus :: Newtype UploadStatus _


newtype UploadType = UploadType String
derive instance newtypeUploadType :: Newtype UploadType _


newtype Uploads = Uploads (Array Upload)
derive instance newtypeUploads :: Newtype Uploads _
