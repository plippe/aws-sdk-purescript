

-- | <p>AWS Device Farm is a service that enables mobile app developers to test Android, iOS, and Fire OS apps on physical phones, tablets, and other devices in the cloud.</p>
module AWS.DeviceFarm where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "DeviceFarm" :: String


-- | <p>Creates a device pool.</p>
createDevicePool :: forall eff. CreateDevicePoolRequest -> Aff (err :: AWS.RequestError | eff) CreateDevicePoolResult
createDevicePool = AWS.request serviceName "CreateDevicePool" 


-- | <p>Creates a network profile.</p>
createNetworkProfile :: forall eff. CreateNetworkProfileRequest -> Aff (err :: AWS.RequestError | eff) CreateNetworkProfileResult
createNetworkProfile = AWS.request serviceName "CreateNetworkProfile" 


-- | <p>Creates a new project.</p>
createProject :: forall eff. CreateProjectRequest -> Aff (err :: AWS.RequestError | eff) CreateProjectResult
createProject = AWS.request serviceName "CreateProject" 


-- | <p>Specifies and starts a remote access session.</p>
createRemoteAccessSession :: forall eff. CreateRemoteAccessSessionRequest -> Aff (err :: AWS.RequestError | eff) CreateRemoteAccessSessionResult
createRemoteAccessSession = AWS.request serviceName "CreateRemoteAccessSession" 


-- | <p>Uploads an app or test scripts.</p>
createUpload :: forall eff. CreateUploadRequest -> Aff (err :: AWS.RequestError | eff) CreateUploadResult
createUpload = AWS.request serviceName "CreateUpload" 


-- | <p>Deletes a device pool given the pool ARN. Does not allow deletion of curated pools owned by the system.</p>
deleteDevicePool :: forall eff. DeleteDevicePoolRequest -> Aff (err :: AWS.RequestError | eff) DeleteDevicePoolResult
deleteDevicePool = AWS.request serviceName "DeleteDevicePool" 


-- | <p>Deletes a network profile.</p>
deleteNetworkProfile :: forall eff. DeleteNetworkProfileRequest -> Aff (err :: AWS.RequestError | eff) DeleteNetworkProfileResult
deleteNetworkProfile = AWS.request serviceName "DeleteNetworkProfile" 


-- | <p>Deletes an AWS Device Farm project, given the project ARN.</p> <p> <b>Note</b> Deleting this resource does not stop an in-progress run.</p>
deleteProject :: forall eff. DeleteProjectRequest -> Aff (err :: AWS.RequestError | eff) DeleteProjectResult
deleteProject = AWS.request serviceName "DeleteProject" 


-- | <p>Deletes a completed remote access session and its results.</p>
deleteRemoteAccessSession :: forall eff. DeleteRemoteAccessSessionRequest -> Aff (err :: AWS.RequestError | eff) DeleteRemoteAccessSessionResult
deleteRemoteAccessSession = AWS.request serviceName "DeleteRemoteAccessSession" 


-- | <p>Deletes the run, given the run ARN.</p> <p> <b>Note</b> Deleting this resource does not stop an in-progress run.</p>
deleteRun :: forall eff. DeleteRunRequest -> Aff (err :: AWS.RequestError | eff) DeleteRunResult
deleteRun = AWS.request serviceName "DeleteRun" 


-- | <p>Deletes an upload given the upload ARN.</p>
deleteUpload :: forall eff. DeleteUploadRequest -> Aff (err :: AWS.RequestError | eff) DeleteUploadResult
deleteUpload = AWS.request serviceName "DeleteUpload" 


-- | <p>Returns the number of unmetered iOS and/or unmetered Android devices that have been purchased by the account.</p>
getAccountSettings :: forall eff. GetAccountSettingsRequest -> Aff (err :: AWS.RequestError | eff) GetAccountSettingsResult
getAccountSettings = AWS.request serviceName "GetAccountSettings" 


-- | <p>Gets information about a unique device type.</p>
getDevice :: forall eff. GetDeviceRequest -> Aff (err :: AWS.RequestError | eff) GetDeviceResult
getDevice = AWS.request serviceName "GetDevice" 


-- | <p>Gets information about a device pool.</p>
getDevicePool :: forall eff. GetDevicePoolRequest -> Aff (err :: AWS.RequestError | eff) GetDevicePoolResult
getDevicePool = AWS.request serviceName "GetDevicePool" 


-- | <p>Gets information about compatibility with a device pool.</p>
getDevicePoolCompatibility :: forall eff. GetDevicePoolCompatibilityRequest -> Aff (err :: AWS.RequestError | eff) GetDevicePoolCompatibilityResult
getDevicePoolCompatibility = AWS.request serviceName "GetDevicePoolCompatibility" 


-- | <p>Gets information about a job.</p>
getJob :: forall eff. GetJobRequest -> Aff (err :: AWS.RequestError | eff) GetJobResult
getJob = AWS.request serviceName "GetJob" 


-- | <p>Returns information about a network profile.</p>
getNetworkProfile :: forall eff. GetNetworkProfileRequest -> Aff (err :: AWS.RequestError | eff) GetNetworkProfileResult
getNetworkProfile = AWS.request serviceName "GetNetworkProfile" 


-- | <p>Gets the current status and future status of all offerings purchased by an AWS account. The response indicates how many offerings are currently available and the offerings that will be available in the next period. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
getOfferingStatus :: forall eff. GetOfferingStatusRequest -> Aff (err :: AWS.RequestError | eff) GetOfferingStatusResult
getOfferingStatus = AWS.request serviceName "GetOfferingStatus" 


-- | <p>Gets information about a project.</p>
getProject :: forall eff. GetProjectRequest -> Aff (err :: AWS.RequestError | eff) GetProjectResult
getProject = AWS.request serviceName "GetProject" 


-- | <p>Returns a link to a currently running remote access session.</p>
getRemoteAccessSession :: forall eff. GetRemoteAccessSessionRequest -> Aff (err :: AWS.RequestError | eff) GetRemoteAccessSessionResult
getRemoteAccessSession = AWS.request serviceName "GetRemoteAccessSession" 


-- | <p>Gets information about a run.</p>
getRun :: forall eff. GetRunRequest -> Aff (err :: AWS.RequestError | eff) GetRunResult
getRun = AWS.request serviceName "GetRun" 


-- | <p>Gets information about a suite.</p>
getSuite :: forall eff. GetSuiteRequest -> Aff (err :: AWS.RequestError | eff) GetSuiteResult
getSuite = AWS.request serviceName "GetSuite" 


-- | <p>Gets information about a test.</p>
getTest :: forall eff. GetTestRequest -> Aff (err :: AWS.RequestError | eff) GetTestResult
getTest = AWS.request serviceName "GetTest" 


-- | <p>Gets information about an upload.</p>
getUpload :: forall eff. GetUploadRequest -> Aff (err :: AWS.RequestError | eff) GetUploadResult
getUpload = AWS.request serviceName "GetUpload" 


-- | <p>Installs an application to the device in a remote access session. For Android applications, the file must be in .apk format. For iOS applications, the file must be in .ipa format.</p>
installToRemoteAccessSession :: forall eff. InstallToRemoteAccessSessionRequest -> Aff (err :: AWS.RequestError | eff) InstallToRemoteAccessSessionResult
installToRemoteAccessSession = AWS.request serviceName "InstallToRemoteAccessSession" 


-- | <p>Gets information about artifacts.</p>
listArtifacts :: forall eff. ListArtifactsRequest -> Aff (err :: AWS.RequestError | eff) ListArtifactsResult
listArtifacts = AWS.request serviceName "ListArtifacts" 


-- | <p>Gets information about device pools.</p>
listDevicePools :: forall eff. ListDevicePoolsRequest -> Aff (err :: AWS.RequestError | eff) ListDevicePoolsResult
listDevicePools = AWS.request serviceName "ListDevicePools" 


-- | <p>Gets information about unique device types.</p>
listDevices :: forall eff. ListDevicesRequest -> Aff (err :: AWS.RequestError | eff) ListDevicesResult
listDevices = AWS.request serviceName "ListDevices" 


-- | <p>Gets information about jobs for a given test run.</p>
listJobs :: forall eff. ListJobsRequest -> Aff (err :: AWS.RequestError | eff) ListJobsResult
listJobs = AWS.request serviceName "ListJobs" 


-- | <p>Returns the list of available network profiles.</p>
listNetworkProfiles :: forall eff. ListNetworkProfilesRequest -> Aff (err :: AWS.RequestError | eff) ListNetworkProfilesResult
listNetworkProfiles = AWS.request serviceName "ListNetworkProfiles" 


-- | <p>Returns a list of offering promotions. Each offering promotion record contains the ID and description of the promotion. The API returns a <code>NotEligible</code> error if the caller is not permitted to invoke the operation. Contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
listOfferingPromotions :: forall eff. ListOfferingPromotionsRequest -> Aff (err :: AWS.RequestError | eff) ListOfferingPromotionsResult
listOfferingPromotions = AWS.request serviceName "ListOfferingPromotions" 


-- | <p>Returns a list of all historical purchases, renewals, and system renewal transactions for an AWS account. The list is paginated and ordered by a descending timestamp (most recent transactions are first). The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
listOfferingTransactions :: forall eff. ListOfferingTransactionsRequest -> Aff (err :: AWS.RequestError | eff) ListOfferingTransactionsResult
listOfferingTransactions = AWS.request serviceName "ListOfferingTransactions" 


-- | <p>Returns a list of products or offerings that the user can manage through the API. Each offering record indicates the recurring price per unit and the frequency for that offering. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
listOfferings :: forall eff. ListOfferingsRequest -> Aff (err :: AWS.RequestError | eff) ListOfferingsResult
listOfferings = AWS.request serviceName "ListOfferings" 


-- | <p>Gets information about projects.</p>
listProjects :: forall eff. ListProjectsRequest -> Aff (err :: AWS.RequestError | eff) ListProjectsResult
listProjects = AWS.request serviceName "ListProjects" 


-- | <p>Returns a list of all currently running remote access sessions.</p>
listRemoteAccessSessions :: forall eff. ListRemoteAccessSessionsRequest -> Aff (err :: AWS.RequestError | eff) ListRemoteAccessSessionsResult
listRemoteAccessSessions = AWS.request serviceName "ListRemoteAccessSessions" 


-- | <p>Gets information about runs, given an AWS Device Farm project ARN.</p>
listRuns :: forall eff. ListRunsRequest -> Aff (err :: AWS.RequestError | eff) ListRunsResult
listRuns = AWS.request serviceName "ListRuns" 


-- | <p>Gets information about samples, given an AWS Device Farm project ARN</p>
listSamples :: forall eff. ListSamplesRequest -> Aff (err :: AWS.RequestError | eff) ListSamplesResult
listSamples = AWS.request serviceName "ListSamples" 


-- | <p>Gets information about test suites for a given job.</p>
listSuites :: forall eff. ListSuitesRequest -> Aff (err :: AWS.RequestError | eff) ListSuitesResult
listSuites = AWS.request serviceName "ListSuites" 


-- | <p>Gets information about tests in a given test suite.</p>
listTests :: forall eff. ListTestsRequest -> Aff (err :: AWS.RequestError | eff) ListTestsResult
listTests = AWS.request serviceName "ListTests" 


-- | <p>Gets information about unique problems.</p>
listUniqueProblems :: forall eff. ListUniqueProblemsRequest -> Aff (err :: AWS.RequestError | eff) ListUniqueProblemsResult
listUniqueProblems = AWS.request serviceName "ListUniqueProblems" 


-- | <p>Gets information about uploads, given an AWS Device Farm project ARN.</p>
listUploads :: forall eff. ListUploadsRequest -> Aff (err :: AWS.RequestError | eff) ListUploadsResult
listUploads = AWS.request serviceName "ListUploads" 


-- | <p>Immediately purchases offerings for an AWS account. Offerings renew with the latest total purchased quantity for an offering, unless the renewal was overridden. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
purchaseOffering :: forall eff. PurchaseOfferingRequest -> Aff (err :: AWS.RequestError | eff) PurchaseOfferingResult
purchaseOffering = AWS.request serviceName "PurchaseOffering" 


-- | <p>Explicitly sets the quantity of devices to renew for an offering, starting from the <code>effectiveDate</code> of the next period. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
renewOffering :: forall eff. RenewOfferingRequest -> Aff (err :: AWS.RequestError | eff) RenewOfferingResult
renewOffering = AWS.request serviceName "RenewOffering" 


-- | <p>Schedules a run.</p>
scheduleRun :: forall eff. ScheduleRunRequest -> Aff (err :: AWS.RequestError | eff) ScheduleRunResult
scheduleRun = AWS.request serviceName "ScheduleRun" 


-- | <p>Ends a specified remote access session.</p>
stopRemoteAccessSession :: forall eff. StopRemoteAccessSessionRequest -> Aff (err :: AWS.RequestError | eff) StopRemoteAccessSessionResult
stopRemoteAccessSession = AWS.request serviceName "StopRemoteAccessSession" 


-- | <p>Initiates a stop request for the current test run. AWS Device Farm will immediately stop the run on devices where tests have not started executing, and you will not be billed for these devices. On devices where tests have started executing, Setup Suite and Teardown Suite tests will run to completion before stopping execution on those devices. You will be billed for Setup, Teardown, and any tests that were in progress or already completed.</p>
stopRun :: forall eff. StopRunRequest -> Aff (err :: AWS.RequestError | eff) StopRunResult
stopRun = AWS.request serviceName "StopRun" 


-- | <p>Modifies the name, description, and rules in a device pool given the attributes and the pool ARN. Rule updates are all-or-nothing, meaning they can only be updated as a whole (or not at all).</p>
updateDevicePool :: forall eff. UpdateDevicePoolRequest -> Aff (err :: AWS.RequestError | eff) UpdateDevicePoolResult
updateDevicePool = AWS.request serviceName "UpdateDevicePool" 


-- | <p>Updates the network profile with specific settings.</p>
updateNetworkProfile :: forall eff. UpdateNetworkProfileRequest -> Aff (err :: AWS.RequestError | eff) UpdateNetworkProfileResult
updateNetworkProfile = AWS.request serviceName "UpdateNetworkProfile" 


-- | <p>Modifies the specified project name, given the project ARN and a new name.</p>
updateProject :: forall eff. UpdateProjectRequest -> Aff (err :: AWS.RequestError | eff) UpdateProjectResult
updateProject = AWS.request serviceName "UpdateProject" 


newtype AWSAccountNumber = AWSAccountNumber String


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


newtype AccountsCleanup = AccountsCleanup Boolean


newtype AmazonResourceName = AmazonResourceName String


newtype AmazonResourceNames = AmazonResourceNames (Array AmazonResourceName)


newtype AndroidPaths = AndroidPaths (Array String)


newtype AppPackagesCleanup = AppPackagesCleanup Boolean


-- | <p>An invalid argument was specified.</p>
newtype ArgumentException = ArgumentException 
  { "Message'" :: NullOrUndefined (Message)
  }


-- | <p>Represents the output of a test. Examples of artifacts include logs and screenshots.</p>
newtype Artifact = Artifact 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Type'" :: NullOrUndefined (ArtifactType)
  , "Extension'" :: NullOrUndefined (String)
  , "Url'" :: NullOrUndefined (URL)
  }


newtype ArtifactCategory = ArtifactCategory String


newtype ArtifactType = ArtifactType String


newtype Artifacts = Artifacts (Array Artifact)


newtype BillingMethod = BillingMethod String


-- | <p>Represents the amount of CPU that an app is using on a physical device.</p> <p>Note that this does not represent system-wide CPU usage.</p>
newtype CPU = CPU 
  { "Frequency'" :: NullOrUndefined (String)
  , "Architecture'" :: NullOrUndefined (String)
  , "Clock'" :: NullOrUndefined (Number)
  }


newtype ClientId = ClientId String


newtype ContentType = ContentType String


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


-- | <p>Represents a request to the create device pool operation.</p>
newtype CreateDevicePoolRequest = CreateDevicePoolRequest 
  { "ProjectArn'" :: (AmazonResourceName)
  , "Name'" :: (Name)
  , "Description'" :: NullOrUndefined (Message)
  , "Rules'" :: (Rules)
  }


-- | <p>Represents the result of a create device pool request.</p>
newtype CreateDevicePoolResult = CreateDevicePoolResult 
  { "DevicePool'" :: NullOrUndefined (DevicePool)
  }


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


newtype CreateNetworkProfileResult = CreateNetworkProfileResult 
  { "NetworkProfile'" :: NullOrUndefined (NetworkProfile)
  }


-- | <p>Represents a request to the create project operation.</p>
newtype CreateProjectRequest = CreateProjectRequest 
  { "Name'" :: (Name)
  , "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes)
  }


-- | <p>Represents the result of a create project request.</p>
newtype CreateProjectResult = CreateProjectResult 
  { "Project'" :: NullOrUndefined (Project)
  }


-- | <p>Creates the configuration settings for a remote access session, including the device model and type.</p>
newtype CreateRemoteAccessSessionConfiguration = CreateRemoteAccessSessionConfiguration 
  { "BillingMethod'" :: NullOrUndefined (BillingMethod)
  }


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


-- | <p>Represents the server response from a request to create a remote access session.</p>
newtype CreateRemoteAccessSessionResult = CreateRemoteAccessSessionResult 
  { "RemoteAccessSession'" :: NullOrUndefined (RemoteAccessSession)
  }


-- | <p>Represents a request to the create upload operation.</p>
newtype CreateUploadRequest = CreateUploadRequest 
  { "ProjectArn'" :: (AmazonResourceName)
  , "Name'" :: (Name)
  , "Type'" :: (UploadType)
  , "ContentType'" :: NullOrUndefined (ContentType)
  }


-- | <p>Represents the result of a create upload request.</p>
newtype CreateUploadResult = CreateUploadResult 
  { "Upload'" :: NullOrUndefined (Upload)
  }


newtype CurrencyCode = CurrencyCode String


-- | <p>A JSON object specifying the paths where the artifacts generated by the customer's tests, on the device or in the test environment, will be pulled from.</p> <p>Specify <code>deviceHostPaths</code> and optionally specify either <code>iosPaths</code> or <code>androidPaths</code>.</p> <p>For web app tests, you can specify both <code>iosPaths</code> and <code>androidPaths</code>.</p>
newtype CustomerArtifactPaths = CustomerArtifactPaths 
  { "IosPaths'" :: NullOrUndefined (IosPaths)
  , "AndroidPaths'" :: NullOrUndefined (AndroidPaths)
  , "DeviceHostPaths'" :: NullOrUndefined (DeviceHostPaths)
  }


newtype DateTime = DateTime Number


-- | <p>Represents a request to the delete device pool operation.</p>
newtype DeleteDevicePoolRequest = DeleteDevicePoolRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the result of a delete device pool request.</p>
newtype DeleteDevicePoolResult = DeleteDevicePoolResult 
  { 
  }


newtype DeleteNetworkProfileRequest = DeleteNetworkProfileRequest 
  { "Arn'" :: (AmazonResourceName)
  }


newtype DeleteNetworkProfileResult = DeleteNetworkProfileResult 
  { 
  }


-- | <p>Represents a request to the delete project operation.</p>
newtype DeleteProjectRequest = DeleteProjectRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the result of a delete project request.</p>
newtype DeleteProjectResult = DeleteProjectResult 
  { 
  }


-- | <p>Represents the request to delete the specified remote access session.</p>
newtype DeleteRemoteAccessSessionRequest = DeleteRemoteAccessSessionRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>The response from the server when a request is made to delete the remote access session.</p>
newtype DeleteRemoteAccessSessionResult = DeleteRemoteAccessSessionResult 
  { 
  }


-- | <p>Represents a request to the delete run operation.</p>
newtype DeleteRunRequest = DeleteRunRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the result of a delete run request.</p>
newtype DeleteRunResult = DeleteRunResult 
  { 
  }


-- | <p>Represents a request to the delete upload operation.</p>
newtype DeleteUploadRequest = DeleteUploadRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the result of a delete upload request.</p>
newtype DeleteUploadResult = DeleteUploadResult 
  { 
  }


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


newtype DeviceAttribute = DeviceAttribute String


newtype DeviceFormFactor = DeviceFormFactor String


newtype DeviceHostPaths = DeviceHostPaths (Array String)


-- | <p>Represents the total (metered or unmetered) minutes used by the resource to run tests. Contains the sum of minutes consumed by all children.</p>
newtype DeviceMinutes = DeviceMinutes 
  { "Total'" :: NullOrUndefined (Number)
  , "Metered'" :: NullOrUndefined (Number)
  , "Unmetered'" :: NullOrUndefined (Number)
  }


newtype DevicePlatform = DevicePlatform String


-- | <p>Represents a collection of device types.</p>
newtype DevicePool = DevicePool 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Description'" :: NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined (DevicePoolType)
  , "Rules'" :: NullOrUndefined (Rules)
  }


-- | <p>Represents a device pool compatibility result.</p>
newtype DevicePoolCompatibilityResult = DevicePoolCompatibilityResult 
  { "Device'" :: NullOrUndefined (Device)
  , "Compatible'" :: NullOrUndefined (Boolean)
  , "IncompatibilityMessages'" :: NullOrUndefined (IncompatibilityMessages)
  }


newtype DevicePoolCompatibilityResults = DevicePoolCompatibilityResults (Array DevicePoolCompatibilityResult)


newtype DevicePoolType = DevicePoolType String


newtype DevicePools = DevicePools (Array DevicePool)


newtype Devices = Devices (Array Device)


-- | <p>Represents configuration information about a test run, such as the execution timeout (in minutes).</p>
newtype ExecutionConfiguration = ExecutionConfiguration 
  { "JobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes)
  , "AccountsCleanup'" :: NullOrUndefined (AccountsCleanup)
  , "AppPackagesCleanup'" :: NullOrUndefined (AppPackagesCleanup)
  }


newtype ExecutionResult = ExecutionResult String


newtype ExecutionResultCode = ExecutionResultCode String


newtype ExecutionStatus = ExecutionStatus String


newtype Filter = Filter String


-- | <p>Represents the request sent to retrieve the account settings.</p>
newtype GetAccountSettingsRequest = GetAccountSettingsRequest 
  { 
  }


-- | <p>Represents the account settings return values from the <code>GetAccountSettings</code> request.</p>
newtype GetAccountSettingsResult = GetAccountSettingsResult 
  { "AccountSettings'" :: NullOrUndefined (AccountSettings)
  }


-- | <p>Represents a request to the get device pool compatibility operation.</p>
newtype GetDevicePoolCompatibilityRequest = GetDevicePoolCompatibilityRequest 
  { "DevicePoolArn'" :: (AmazonResourceName)
  , "AppArn'" :: NullOrUndefined (AmazonResourceName)
  , "TestType'" :: NullOrUndefined (TestType)
  , "Test'" :: NullOrUndefined (ScheduleRunTest)
  }


-- | <p>Represents the result of describe device pool compatibility request.</p>
newtype GetDevicePoolCompatibilityResult = GetDevicePoolCompatibilityResult 
  { "CompatibleDevices'" :: NullOrUndefined (DevicePoolCompatibilityResults)
  , "IncompatibleDevices'" :: NullOrUndefined (DevicePoolCompatibilityResults)
  }


-- | <p>Represents a request to the get device pool operation.</p>
newtype GetDevicePoolRequest = GetDevicePoolRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the result of a get device pool request.</p>
newtype GetDevicePoolResult = GetDevicePoolResult 
  { "DevicePool'" :: NullOrUndefined (DevicePool)
  }


-- | <p>Represents a request to the get device request.</p>
newtype GetDeviceRequest = GetDeviceRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the result of a get device request.</p>
newtype GetDeviceResult = GetDeviceResult 
  { "Device'" :: NullOrUndefined (Device)
  }


-- | <p>Represents a request to the get job operation.</p>
newtype GetJobRequest = GetJobRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the result of a get job request.</p>
newtype GetJobResult = GetJobResult 
  { "Job'" :: NullOrUndefined (Job)
  }


newtype GetNetworkProfileRequest = GetNetworkProfileRequest 
  { "Arn'" :: (AmazonResourceName)
  }


newtype GetNetworkProfileResult = GetNetworkProfileResult 
  { "NetworkProfile'" :: NullOrUndefined (NetworkProfile)
  }


-- | <p>Represents the request to retrieve the offering status for the specified customer or account.</p>
newtype GetOfferingStatusRequest = GetOfferingStatusRequest 
  { "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Returns the status result for a device offering.</p>
newtype GetOfferingStatusResult = GetOfferingStatusResult 
  { "Current'" :: NullOrUndefined (OfferingStatusMap)
  , "NextPeriod'" :: NullOrUndefined (OfferingStatusMap)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents a request to the get project operation.</p>
newtype GetProjectRequest = GetProjectRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the result of a get project request.</p>
newtype GetProjectResult = GetProjectResult 
  { "Project'" :: NullOrUndefined (Project)
  }


-- | <p>Represents the request to get information about the specified remote access session.</p>
newtype GetRemoteAccessSessionRequest = GetRemoteAccessSessionRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the response from the server that lists detailed information about the remote access session.</p>
newtype GetRemoteAccessSessionResult = GetRemoteAccessSessionResult 
  { "RemoteAccessSession'" :: NullOrUndefined (RemoteAccessSession)
  }


-- | <p>Represents a request to the get run operation.</p>
newtype GetRunRequest = GetRunRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the result of a get run request.</p>
newtype GetRunResult = GetRunResult 
  { "Run'" :: NullOrUndefined (Run)
  }


-- | <p>Represents a request to the get suite operation.</p>
newtype GetSuiteRequest = GetSuiteRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the result of a get suite request.</p>
newtype GetSuiteResult = GetSuiteResult 
  { "Suite'" :: NullOrUndefined (Suite)
  }


-- | <p>Represents a request to the get test operation.</p>
newtype GetTestRequest = GetTestRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the result of a get test request.</p>
newtype GetTestResult = GetTestResult 
  { "Test'" :: NullOrUndefined (Test)
  }


-- | <p>Represents a request to the get upload operation.</p>
newtype GetUploadRequest = GetUploadRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the result of a get upload request.</p>
newtype GetUploadResult = GetUploadResult 
  { "Upload'" :: NullOrUndefined (Upload)
  }


newtype HostAddress = HostAddress String


-- | <p>An entity with the same name already exists.</p>
newtype IdempotencyException = IdempotencyException 
  { "Message'" :: NullOrUndefined (Message)
  }


-- | <p>Represents information about incompatibility.</p>
newtype IncompatibilityMessage = IncompatibilityMessage 
  { "Message'" :: NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined (DeviceAttribute)
  }


newtype IncompatibilityMessages = IncompatibilityMessages (Array IncompatibilityMessage)


-- | <p>Represents the request to install an Android application (in .apk format) or an iOS application (in .ipa format) as part of a remote access session.</p>
newtype InstallToRemoteAccessSessionRequest = InstallToRemoteAccessSessionRequest 
  { "RemoteAccessSessionArn'" :: (AmazonResourceName)
  , "AppArn'" :: (AmazonResourceName)
  }


-- | <p>Represents the response from the server after AWS Device Farm makes a request to install to a remote access session.</p>
newtype InstallToRemoteAccessSessionResult = InstallToRemoteAccessSessionResult 
  { "AppUpload'" :: NullOrUndefined (Upload)
  }


newtype InteractionMode = InteractionMode String


newtype IosPaths = IosPaths (Array String)


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


newtype JobTimeoutMinutes = JobTimeoutMinutes Int


newtype Jobs = Jobs (Array Job)


-- | <p>A limit was exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (Message)
  }


-- | <p>Represents a request to the list artifacts operation.</p>
newtype ListArtifactsRequest = ListArtifactsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Type'" :: (ArtifactCategory)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list artifacts operation.</p>
newtype ListArtifactsResult = ListArtifactsResult 
  { "Artifacts'" :: NullOrUndefined (Artifacts)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list device pools request.</p>
newtype ListDevicePoolsRequest = ListDevicePoolsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Type'" :: NullOrUndefined (DevicePoolType)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list device pools request.</p>
newtype ListDevicePoolsResult = ListDevicePoolsResult 
  { "DevicePools'" :: NullOrUndefined (DevicePools)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list devices request.</p>
newtype ListDevicesRequest = ListDevicesRequest 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list devices operation.</p>
newtype ListDevicesResult = ListDevicesResult 
  { "Devices'" :: NullOrUndefined (Devices)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents a request to the list jobs operation.</p>
newtype ListJobsRequest = ListJobsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list jobs request.</p>
newtype ListJobsResult = ListJobsResult 
  { "Jobs'" :: NullOrUndefined (Jobs)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


newtype ListNetworkProfilesRequest = ListNetworkProfilesRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Type'" :: NullOrUndefined (NetworkProfileType)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


newtype ListNetworkProfilesResult = ListNetworkProfilesResult 
  { "NetworkProfiles'" :: NullOrUndefined (NetworkProfiles)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


newtype ListOfferingPromotionsRequest = ListOfferingPromotionsRequest 
  { "NextToken'" :: NullOrUndefined (PaginationToken)
  }


newtype ListOfferingPromotionsResult = ListOfferingPromotionsResult 
  { "OfferingPromotions'" :: NullOrUndefined (OfferingPromotions)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the request to list the offering transaction history.</p>
newtype ListOfferingTransactionsRequest = ListOfferingTransactionsRequest 
  { "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Returns the transaction log of the specified offerings.</p>
newtype ListOfferingTransactionsResult = ListOfferingTransactionsResult 
  { "OfferingTransactions'" :: NullOrUndefined (OfferingTransactions)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the request to list all offerings.</p>
newtype ListOfferingsRequest = ListOfferingsRequest 
  { "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the return values of the list of offerings.</p>
newtype ListOfferingsResult = ListOfferingsResult 
  { "Offerings'" :: NullOrUndefined (Offerings)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents a request to the list projects operation.</p>
newtype ListProjectsRequest = ListProjectsRequest 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list projects request.</p>
newtype ListProjectsResult = ListProjectsResult 
  { "Projects'" :: NullOrUndefined (Projects)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the request to return information about the remote access session.</p>
newtype ListRemoteAccessSessionsRequest = ListRemoteAccessSessionsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the response from the server after AWS Device Farm makes a request to return information about the remote access session.</p>
newtype ListRemoteAccessSessionsResult = ListRemoteAccessSessionsResult 
  { "RemoteAccessSessions'" :: NullOrUndefined (RemoteAccessSessions)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents a request to the list runs operation.</p>
newtype ListRunsRequest = ListRunsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list runs request.</p>
newtype ListRunsResult = ListRunsResult 
  { "Runs'" :: NullOrUndefined (Runs)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents a request to the list samples operation.</p>
newtype ListSamplesRequest = ListSamplesRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list samples request.</p>
newtype ListSamplesResult = ListSamplesResult 
  { "Samples'" :: NullOrUndefined (Samples)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents a request to the list suites operation.</p>
newtype ListSuitesRequest = ListSuitesRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list suites request.</p>
newtype ListSuitesResult = ListSuitesResult 
  { "Suites'" :: NullOrUndefined (Suites)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents a request to the list tests operation.</p>
newtype ListTestsRequest = ListTestsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list tests request.</p>
newtype ListTestsResult = ListTestsResult 
  { "Tests'" :: NullOrUndefined (Tests)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents a request to the list unique problems operation.</p>
newtype ListUniqueProblemsRequest = ListUniqueProblemsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list unique problems request.</p>
newtype ListUniqueProblemsResult = ListUniqueProblemsResult 
  { "UniqueProblems'" :: NullOrUndefined (UniqueProblemsByExecutionResultMap)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents a request to the list uploads operation.</p>
newtype ListUploadsRequest = ListUploadsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents the result of a list uploads request.</p>
newtype ListUploadsResult = ListUploadsResult 
  { "Uploads'" :: NullOrUndefined (Uploads)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Represents a latitude and longitude pair, expressed in geographic coordinate system degrees (for example 47.6204, -122.3491).</p> <p>Elevation is currently not supported.</p>
newtype Location = Location 
  { "Latitude'" :: (Number)
  , "Longitude'" :: (Number)
  }


newtype MaxSlotMap = MaxSlotMap (Map String Int)


newtype Message = Message String


newtype Metadata = Metadata String


-- | <p>A number representing the monetary amount for an offering or transaction.</p>
newtype MonetaryAmount = MonetaryAmount 
  { "Amount'" :: NullOrUndefined (Number)
  , "CurrencyCode'" :: NullOrUndefined (CurrencyCode)
  }


newtype Name = Name String


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


newtype NetworkProfileType = NetworkProfileType String


newtype NetworkProfiles = NetworkProfiles (Array NetworkProfile)


-- | <p>Exception gets thrown when a user is not eligible to perform the specified transaction.</p>
newtype NotEligibleException = NotEligibleException 
  { "Message'" :: NullOrUndefined (Message)
  }


-- | <p>The specified entity was not found.</p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined (Message)
  }


-- | <p>Represents the metadata of a device offering.</p>
newtype Offering = Offering 
  { "Id'" :: NullOrUndefined (OfferingIdentifier)
  , "Description'" :: NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined (OfferingType)
  , "Platform'" :: NullOrUndefined (DevicePlatform)
  , "RecurringCharges'" :: NullOrUndefined (RecurringCharges)
  }


newtype OfferingIdentifier = OfferingIdentifier String


-- | <p>Represents information about an offering promotion.</p>
newtype OfferingPromotion = OfferingPromotion 
  { "Id'" :: NullOrUndefined (OfferingPromotionIdentifier)
  , "Description'" :: NullOrUndefined (Message)
  }


newtype OfferingPromotionIdentifier = OfferingPromotionIdentifier String


newtype OfferingPromotions = OfferingPromotions (Array OfferingPromotion)


-- | <p>The status of the offering.</p>
newtype OfferingStatus = OfferingStatus 
  { "Type'" :: NullOrUndefined (OfferingTransactionType)
  , "Offering'" :: NullOrUndefined (Offering)
  , "Quantity'" :: NullOrUndefined (Int)
  , "EffectiveOn'" :: NullOrUndefined (DateTime)
  }


newtype OfferingStatusMap = OfferingStatusMap (Map OfferingIdentifier OfferingStatus)


-- | <p>Represents the metadata of an offering transaction.</p>
newtype OfferingTransaction = OfferingTransaction 
  { "OfferingStatus'" :: NullOrUndefined (OfferingStatus)
  , "TransactionId'" :: NullOrUndefined (TransactionIdentifier)
  , "OfferingPromotionId'" :: NullOrUndefined (OfferingPromotionIdentifier)
  , "CreatedOn'" :: NullOrUndefined (DateTime)
  , "Cost'" :: NullOrUndefined (MonetaryAmount)
  }


newtype OfferingTransactionType = OfferingTransactionType String


newtype OfferingTransactions = OfferingTransactions (Array OfferingTransaction)


newtype OfferingType = OfferingType String


newtype Offerings = Offerings (Array Offering)


newtype PaginationToken = PaginationToken String


newtype PercentInteger = PercentInteger Int


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


-- | <p>Information about a problem detail.</p>
newtype ProblemDetail = ProblemDetail 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  }


newtype Problems = Problems (Array Problem)


-- | <p>Represents an operating-system neutral workspace for running and managing tests.</p>
newtype Project = Project 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes)
  , "Created'" :: NullOrUndefined (DateTime)
  }


newtype Projects = Projects (Array Project)


-- | <p>Represents a request for a purchase offering.</p>
newtype PurchaseOfferingRequest = PurchaseOfferingRequest 
  { "OfferingId'" :: NullOrUndefined (OfferingIdentifier)
  , "Quantity'" :: NullOrUndefined (Int)
  , "OfferingPromotionId'" :: NullOrUndefined (OfferingPromotionIdentifier)
  }


-- | <p>The result of the purchase offering (e.g., success or failure).</p>
newtype PurchaseOfferingResult = PurchaseOfferingResult 
  { "OfferingTransaction'" :: NullOrUndefined (OfferingTransaction)
  }


newtype PurchasedDevicesMap = PurchasedDevicesMap (Map DevicePlatform Int)


-- | <p>Represents the set of radios and their states on a device. Examples of radios include Wi-Fi, GPS, Bluetooth, and NFC.</p>
newtype Radios = Radios 
  { "Wifi'" :: NullOrUndefined (Boolean)
  , "Bluetooth'" :: NullOrUndefined (Boolean)
  , "Nfc'" :: NullOrUndefined (Boolean)
  , "Gps'" :: NullOrUndefined (Boolean)
  }


-- | <p>Specifies whether charges for devices will be recurring.</p>
newtype RecurringCharge = RecurringCharge 
  { "Cost'" :: NullOrUndefined (MonetaryAmount)
  , "Frequency'" :: NullOrUndefined (RecurringChargeFrequency)
  }


newtype RecurringChargeFrequency = RecurringChargeFrequency String


newtype RecurringCharges = RecurringCharges (Array RecurringCharge)


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


newtype RemoteAccessSessions = RemoteAccessSessions (Array RemoteAccessSession)


-- | <p>A request representing an offering renewal.</p>
newtype RenewOfferingRequest = RenewOfferingRequest 
  { "OfferingId'" :: NullOrUndefined (OfferingIdentifier)
  , "Quantity'" :: NullOrUndefined (Int)
  }


-- | <p>The result of a renewal offering.</p>
newtype RenewOfferingResult = RenewOfferingResult 
  { "OfferingTransaction'" :: NullOrUndefined (OfferingTransaction)
  }


-- | <p>Represents the screen resolution of a device in height and width, expressed in pixels.</p>
newtype Resolution = Resolution 
  { "Width'" :: NullOrUndefined (Int)
  , "Height'" :: NullOrUndefined (Int)
  }


-- | <p>Represents a condition for a device pool.</p>
newtype Rule = Rule 
  { "Attribute'" :: NullOrUndefined (DeviceAttribute)
  , "Operator'" :: NullOrUndefined (RuleOperator)
  , "Value'" :: NullOrUndefined (String)
  }


newtype RuleOperator = RuleOperator String


newtype Rules = Rules (Array Rule)


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


newtype Runs = Runs (Array Run)


-- | <p>Represents a sample of performance data.</p>
newtype Sample = Sample 
  { "Arn'" :: NullOrUndefined (AmazonResourceName)
  , "Type'" :: NullOrUndefined (SampleType)
  , "Url'" :: NullOrUndefined (URL)
  }


newtype SampleType = SampleType String


newtype Samples = Samples (Array Sample)


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


-- | <p>Represents the result of a schedule run request.</p>
newtype ScheduleRunResult = ScheduleRunResult 
  { "Run'" :: NullOrUndefined (Run)
  }


-- | <p>Represents additional test settings.</p>
newtype ScheduleRunTest = ScheduleRunTest 
  { "Type'" :: (TestType)
  , "TestPackageArn'" :: NullOrUndefined (AmazonResourceName)
  , "Filter'" :: NullOrUndefined (Filter)
  , "Parameters'" :: NullOrUndefined (TestParameters)
  }


-- | <p>There was a problem with the service account.</p>
newtype ServiceAccountException = ServiceAccountException 
  { "Message'" :: NullOrUndefined (Message)
  }


newtype SshPublicKey = SshPublicKey String


-- | <p>Represents the request to stop the remote access session.</p>
newtype StopRemoteAccessSessionRequest = StopRemoteAccessSessionRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the response from the server that describes the remote access session when AWS Device Farm stops the session.</p>
newtype StopRemoteAccessSessionResult = StopRemoteAccessSessionResult 
  { "RemoteAccessSession'" :: NullOrUndefined (RemoteAccessSession)
  }


-- | <p>Represents the request to stop a specific run.</p>
newtype StopRunRequest = StopRunRequest 
  { "Arn'" :: (AmazonResourceName)
  }


-- | <p>Represents the results of your stop run attempt.</p>
newtype StopRunResult = StopRunResult 
  { "Run'" :: NullOrUndefined (Run)
  }


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


newtype Suites = Suites (Array Suite)


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


newtype TestParameters = TestParameters (Map String String)


newtype TestType = TestType String


newtype Tests = Tests (Array Test)


newtype TransactionIdentifier = TransactionIdentifier String


-- | <p>Represents information about free trial device minutes for an AWS account.</p>
newtype TrialMinutes = TrialMinutes 
  { "Total'" :: NullOrUndefined (Number)
  , "Remaining'" :: NullOrUndefined (Number)
  }


newtype URL = URL String


-- | <p>A collection of one or more problems, grouped by their result.</p>
newtype UniqueProblem = UniqueProblem 
  { "Message'" :: NullOrUndefined (Message)
  , "Problems'" :: NullOrUndefined (Problems)
  }


newtype UniqueProblems = UniqueProblems (Array UniqueProblem)


newtype UniqueProblemsByExecutionResultMap = UniqueProblemsByExecutionResultMap (Map ExecutionResult UniqueProblems)


-- | <p>Represents a request to the update device pool operation.</p>
newtype UpdateDevicePoolRequest = UpdateDevicePoolRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "Description'" :: NullOrUndefined (Message)
  , "Rules'" :: NullOrUndefined (Rules)
  }


-- | <p>Represents the result of an update device pool request.</p>
newtype UpdateDevicePoolResult = UpdateDevicePoolResult 
  { "DevicePool'" :: NullOrUndefined (DevicePool)
  }


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


newtype UpdateNetworkProfileResult = UpdateNetworkProfileResult 
  { "NetworkProfile'" :: NullOrUndefined (NetworkProfile)
  }


-- | <p>Represents a request to the update project operation.</p>
newtype UpdateProjectRequest = UpdateProjectRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Name'" :: NullOrUndefined (Name)
  , "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes)
  }


-- | <p>Represents the result of an update project request.</p>
newtype UpdateProjectResult = UpdateProjectResult 
  { "Project'" :: NullOrUndefined (Project)
  }


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


newtype UploadStatus = UploadStatus String


newtype UploadType = UploadType String


newtype Uploads = Uploads (Array Upload)
