## Module AWS.DeviceFarm

<p>AWS Device Farm is a service that enables mobile app developers to test Android, iOS, and Fire OS apps on physical phones, tablets, and other devices in the cloud.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createDevicePool`

``` purescript
createDevicePool :: forall eff. CreateDevicePoolRequest -> Aff (err :: RequestError | eff) CreateDevicePoolResult
```

<p>Creates a device pool.</p>

#### `createNetworkProfile`

``` purescript
createNetworkProfile :: forall eff. CreateNetworkProfileRequest -> Aff (err :: RequestError | eff) CreateNetworkProfileResult
```

<p>Creates a network profile.</p>

#### `createProject`

``` purescript
createProject :: forall eff. CreateProjectRequest -> Aff (err :: RequestError | eff) CreateProjectResult
```

<p>Creates a new project.</p>

#### `createRemoteAccessSession`

``` purescript
createRemoteAccessSession :: forall eff. CreateRemoteAccessSessionRequest -> Aff (err :: RequestError | eff) CreateRemoteAccessSessionResult
```

<p>Specifies and starts a remote access session.</p>

#### `createUpload`

``` purescript
createUpload :: forall eff. CreateUploadRequest -> Aff (err :: RequestError | eff) CreateUploadResult
```

<p>Uploads an app or test scripts.</p>

#### `deleteDevicePool`

``` purescript
deleteDevicePool :: forall eff. DeleteDevicePoolRequest -> Aff (err :: RequestError | eff) DeleteDevicePoolResult
```

<p>Deletes a device pool given the pool ARN. Does not allow deletion of curated pools owned by the system.</p>

#### `deleteNetworkProfile`

``` purescript
deleteNetworkProfile :: forall eff. DeleteNetworkProfileRequest -> Aff (err :: RequestError | eff) DeleteNetworkProfileResult
```

<p>Deletes a network profile.</p>

#### `deleteProject`

``` purescript
deleteProject :: forall eff. DeleteProjectRequest -> Aff (err :: RequestError | eff) DeleteProjectResult
```

<p>Deletes an AWS Device Farm project, given the project ARN.</p> <p> <b>Note</b> Deleting this resource does not stop an in-progress run.</p>

#### `deleteRemoteAccessSession`

``` purescript
deleteRemoteAccessSession :: forall eff. DeleteRemoteAccessSessionRequest -> Aff (err :: RequestError | eff) DeleteRemoteAccessSessionResult
```

<p>Deletes a completed remote access session and its results.</p>

#### `deleteRun`

``` purescript
deleteRun :: forall eff. DeleteRunRequest -> Aff (err :: RequestError | eff) DeleteRunResult
```

<p>Deletes the run, given the run ARN.</p> <p> <b>Note</b> Deleting this resource does not stop an in-progress run.</p>

#### `deleteUpload`

``` purescript
deleteUpload :: forall eff. DeleteUploadRequest -> Aff (err :: RequestError | eff) DeleteUploadResult
```

<p>Deletes an upload given the upload ARN.</p>

#### `getAccountSettings`

``` purescript
getAccountSettings :: forall eff. GetAccountSettingsRequest -> Aff (err :: RequestError | eff) GetAccountSettingsResult
```

<p>Returns the number of unmetered iOS and/or unmetered Android devices that have been purchased by the account.</p>

#### `getDevice`

``` purescript
getDevice :: forall eff. GetDeviceRequest -> Aff (err :: RequestError | eff) GetDeviceResult
```

<p>Gets information about a unique device type.</p>

#### `getDevicePool`

``` purescript
getDevicePool :: forall eff. GetDevicePoolRequest -> Aff (err :: RequestError | eff) GetDevicePoolResult
```

<p>Gets information about a device pool.</p>

#### `getDevicePoolCompatibility`

``` purescript
getDevicePoolCompatibility :: forall eff. GetDevicePoolCompatibilityRequest -> Aff (err :: RequestError | eff) GetDevicePoolCompatibilityResult
```

<p>Gets information about compatibility with a device pool.</p>

#### `getJob`

``` purescript
getJob :: forall eff. GetJobRequest -> Aff (err :: RequestError | eff) GetJobResult
```

<p>Gets information about a job.</p>

#### `getNetworkProfile`

``` purescript
getNetworkProfile :: forall eff. GetNetworkProfileRequest -> Aff (err :: RequestError | eff) GetNetworkProfileResult
```

<p>Returns information about a network profile.</p>

#### `getOfferingStatus`

``` purescript
getOfferingStatus :: forall eff. GetOfferingStatusRequest -> Aff (err :: RequestError | eff) GetOfferingStatusResult
```

<p>Gets the current status and future status of all offerings purchased by an AWS account. The response indicates how many offerings are currently available and the offerings that will be available in the next period. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>

#### `getProject`

``` purescript
getProject :: forall eff. GetProjectRequest -> Aff (err :: RequestError | eff) GetProjectResult
```

<p>Gets information about a project.</p>

#### `getRemoteAccessSession`

``` purescript
getRemoteAccessSession :: forall eff. GetRemoteAccessSessionRequest -> Aff (err :: RequestError | eff) GetRemoteAccessSessionResult
```

<p>Returns a link to a currently running remote access session.</p>

#### `getRun`

``` purescript
getRun :: forall eff. GetRunRequest -> Aff (err :: RequestError | eff) GetRunResult
```

<p>Gets information about a run.</p>

#### `getSuite`

``` purescript
getSuite :: forall eff. GetSuiteRequest -> Aff (err :: RequestError | eff) GetSuiteResult
```

<p>Gets information about a suite.</p>

#### `getTest`

``` purescript
getTest :: forall eff. GetTestRequest -> Aff (err :: RequestError | eff) GetTestResult
```

<p>Gets information about a test.</p>

#### `getUpload`

``` purescript
getUpload :: forall eff. GetUploadRequest -> Aff (err :: RequestError | eff) GetUploadResult
```

<p>Gets information about an upload.</p>

#### `installToRemoteAccessSession`

``` purescript
installToRemoteAccessSession :: forall eff. InstallToRemoteAccessSessionRequest -> Aff (err :: RequestError | eff) InstallToRemoteAccessSessionResult
```

<p>Installs an application to the device in a remote access session. For Android applications, the file must be in .apk format. For iOS applications, the file must be in .ipa format.</p>

#### `listArtifacts`

``` purescript
listArtifacts :: forall eff. ListArtifactsRequest -> Aff (err :: RequestError | eff) ListArtifactsResult
```

<p>Gets information about artifacts.</p>

#### `listDevicePools`

``` purescript
listDevicePools :: forall eff. ListDevicePoolsRequest -> Aff (err :: RequestError | eff) ListDevicePoolsResult
```

<p>Gets information about device pools.</p>

#### `listDevices`

``` purescript
listDevices :: forall eff. ListDevicesRequest -> Aff (err :: RequestError | eff) ListDevicesResult
```

<p>Gets information about unique device types.</p>

#### `listJobs`

``` purescript
listJobs :: forall eff. ListJobsRequest -> Aff (err :: RequestError | eff) ListJobsResult
```

<p>Gets information about jobs for a given test run.</p>

#### `listNetworkProfiles`

``` purescript
listNetworkProfiles :: forall eff. ListNetworkProfilesRequest -> Aff (err :: RequestError | eff) ListNetworkProfilesResult
```

<p>Returns the list of available network profiles.</p>

#### `listOfferingPromotions`

``` purescript
listOfferingPromotions :: forall eff. ListOfferingPromotionsRequest -> Aff (err :: RequestError | eff) ListOfferingPromotionsResult
```

<p>Returns a list of offering promotions. Each offering promotion record contains the ID and description of the promotion. The API returns a <code>NotEligible</code> error if the caller is not permitted to invoke the operation. Contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>

#### `listOfferingTransactions`

``` purescript
listOfferingTransactions :: forall eff. ListOfferingTransactionsRequest -> Aff (err :: RequestError | eff) ListOfferingTransactionsResult
```

<p>Returns a list of all historical purchases, renewals, and system renewal transactions for an AWS account. The list is paginated and ordered by a descending timestamp (most recent transactions are first). The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>

#### `listOfferings`

``` purescript
listOfferings :: forall eff. ListOfferingsRequest -> Aff (err :: RequestError | eff) ListOfferingsResult
```

<p>Returns a list of products or offerings that the user can manage through the API. Each offering record indicates the recurring price per unit and the frequency for that offering. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>

#### `listProjects`

``` purescript
listProjects :: forall eff. ListProjectsRequest -> Aff (err :: RequestError | eff) ListProjectsResult
```

<p>Gets information about projects.</p>

#### `listRemoteAccessSessions`

``` purescript
listRemoteAccessSessions :: forall eff. ListRemoteAccessSessionsRequest -> Aff (err :: RequestError | eff) ListRemoteAccessSessionsResult
```

<p>Returns a list of all currently running remote access sessions.</p>

#### `listRuns`

``` purescript
listRuns :: forall eff. ListRunsRequest -> Aff (err :: RequestError | eff) ListRunsResult
```

<p>Gets information about runs, given an AWS Device Farm project ARN.</p>

#### `listSamples`

``` purescript
listSamples :: forall eff. ListSamplesRequest -> Aff (err :: RequestError | eff) ListSamplesResult
```

<p>Gets information about samples, given an AWS Device Farm project ARN</p>

#### `listSuites`

``` purescript
listSuites :: forall eff. ListSuitesRequest -> Aff (err :: RequestError | eff) ListSuitesResult
```

<p>Gets information about test suites for a given job.</p>

#### `listTests`

``` purescript
listTests :: forall eff. ListTestsRequest -> Aff (err :: RequestError | eff) ListTestsResult
```

<p>Gets information about tests in a given test suite.</p>

#### `listUniqueProblems`

``` purescript
listUniqueProblems :: forall eff. ListUniqueProblemsRequest -> Aff (err :: RequestError | eff) ListUniqueProblemsResult
```

<p>Gets information about unique problems.</p>

#### `listUploads`

``` purescript
listUploads :: forall eff. ListUploadsRequest -> Aff (err :: RequestError | eff) ListUploadsResult
```

<p>Gets information about uploads, given an AWS Device Farm project ARN.</p>

#### `purchaseOffering`

``` purescript
purchaseOffering :: forall eff. PurchaseOfferingRequest -> Aff (err :: RequestError | eff) PurchaseOfferingResult
```

<p>Immediately purchases offerings for an AWS account. Offerings renew with the latest total purchased quantity for an offering, unless the renewal was overridden. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>

#### `renewOffering`

``` purescript
renewOffering :: forall eff. RenewOfferingRequest -> Aff (err :: RequestError | eff) RenewOfferingResult
```

<p>Explicitly sets the quantity of devices to renew for an offering, starting from the <code>effectiveDate</code> of the next period. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>

#### `scheduleRun`

``` purescript
scheduleRun :: forall eff. ScheduleRunRequest -> Aff (err :: RequestError | eff) ScheduleRunResult
```

<p>Schedules a run.</p>

#### `stopRemoteAccessSession`

``` purescript
stopRemoteAccessSession :: forall eff. StopRemoteAccessSessionRequest -> Aff (err :: RequestError | eff) StopRemoteAccessSessionResult
```

<p>Ends a specified remote access session.</p>

#### `stopRun`

``` purescript
stopRun :: forall eff. StopRunRequest -> Aff (err :: RequestError | eff) StopRunResult
```

<p>Initiates a stop request for the current test run. AWS Device Farm will immediately stop the run on devices where tests have not started executing, and you will not be billed for these devices. On devices where tests have started executing, Setup Suite and Teardown Suite tests will run to completion before stopping execution on those devices. You will be billed for Setup, Teardown, and any tests that were in progress or already completed.</p>

#### `updateDevicePool`

``` purescript
updateDevicePool :: forall eff. UpdateDevicePoolRequest -> Aff (err :: RequestError | eff) UpdateDevicePoolResult
```

<p>Modifies the name, description, and rules in a device pool given the attributes and the pool ARN. Rule updates are all-or-nothing, meaning they can only be updated as a whole (or not at all).</p>

#### `updateNetworkProfile`

``` purescript
updateNetworkProfile :: forall eff. UpdateNetworkProfileRequest -> Aff (err :: RequestError | eff) UpdateNetworkProfileResult
```

<p>Updates the network profile with specific settings.</p>

#### `updateProject`

``` purescript
updateProject :: forall eff. UpdateProjectRequest -> Aff (err :: RequestError | eff) UpdateProjectResult
```

<p>Modifies the specified project name, given the project ARN and a new name.</p>

#### `AWSAccountNumber`

``` purescript
newtype AWSAccountNumber
  = AWSAccountNumber String
```

#### `AccountSettings`

``` purescript
newtype AccountSettings
  = AccountSettings { "AwsAccountNumber'" :: NullOrUndefined (AWSAccountNumber), "UnmeteredDevices'" :: NullOrUndefined (PurchasedDevicesMap), "UnmeteredRemoteAccessDevices'" :: NullOrUndefined (PurchasedDevicesMap), "MaxJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes), "TrialMinutes'" :: NullOrUndefined (TrialMinutes), "MaxSlots'" :: NullOrUndefined (MaxSlotMap), "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes) }
```

<p>A container for account-level settings within AWS Device Farm.</p>

#### `AccountsCleanup`

``` purescript
newtype AccountsCleanup
  = AccountsCleanup Boolean
```

#### `AmazonResourceName`

``` purescript
newtype AmazonResourceName
  = AmazonResourceName String
```

#### `AmazonResourceNames`

``` purescript
newtype AmazonResourceNames
  = AmazonResourceNames (Array AmazonResourceName)
```

#### `AndroidPaths`

``` purescript
newtype AndroidPaths
  = AndroidPaths (Array String)
```

#### `AppPackagesCleanup`

``` purescript
newtype AppPackagesCleanup
  = AppPackagesCleanup Boolean
```

#### `ArgumentException`

``` purescript
newtype ArgumentException
  = ArgumentException { "Message'" :: NullOrUndefined (Message) }
```

<p>An invalid argument was specified.</p>

#### `Artifact`

``` purescript
newtype Artifact
  = Artifact { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Type'" :: NullOrUndefined (ArtifactType), "Extension'" :: NullOrUndefined (String), "Url'" :: NullOrUndefined (URL) }
```

<p>Represents the output of a test. Examples of artifacts include logs and screenshots.</p>

#### `ArtifactCategory`

``` purescript
newtype ArtifactCategory
  = ArtifactCategory String
```

#### `ArtifactType`

``` purescript
newtype ArtifactType
  = ArtifactType String
```

#### `Artifacts`

``` purescript
newtype Artifacts
  = Artifacts (Array Artifact)
```

#### `BillingMethod`

``` purescript
newtype BillingMethod
  = BillingMethod String
```

#### `CPU`

``` purescript
newtype CPU
  = CPU { "Frequency'" :: NullOrUndefined (String), "Architecture'" :: NullOrUndefined (String), "Clock'" :: NullOrUndefined (Number) }
```

<p>Represents the amount of CPU that an app is using on a physical device.</p> <p>Note that this does not represent system-wide CPU usage.</p>

#### `ClientId`

``` purescript
newtype ClientId
  = ClientId String
```

#### `ContentType`

``` purescript
newtype ContentType
  = ContentType String
```

#### `Counters`

``` purescript
newtype Counters
  = Counters { "Total'" :: NullOrUndefined (Int), "Passed'" :: NullOrUndefined (Int), "Failed'" :: NullOrUndefined (Int), "Warned'" :: NullOrUndefined (Int), "Errored'" :: NullOrUndefined (Int), "Stopped'" :: NullOrUndefined (Int), "Skipped'" :: NullOrUndefined (Int) }
```

<p>Represents entity counters.</p>

#### `CreateDevicePoolRequest`

``` purescript
newtype CreateDevicePoolRequest
  = CreateDevicePoolRequest { "ProjectArn'" :: AmazonResourceName, "Name'" :: Name, "Description'" :: NullOrUndefined (Message), "Rules'" :: Rules }
```

<p>Represents a request to the create device pool operation.</p>

#### `CreateDevicePoolResult`

``` purescript
newtype CreateDevicePoolResult
  = CreateDevicePoolResult { "DevicePool'" :: NullOrUndefined (DevicePool) }
```

<p>Represents the result of a create device pool request.</p>

#### `CreateNetworkProfileRequest`

``` purescript
newtype CreateNetworkProfileRequest
  = CreateNetworkProfileRequest { "ProjectArn'" :: AmazonResourceName, "Name'" :: Name, "Description'" :: NullOrUndefined (Message), "Type'" :: NullOrUndefined (NetworkProfileType), "UplinkBandwidthBits'" :: NullOrUndefined (Number), "DownlinkBandwidthBits'" :: NullOrUndefined (Number), "UplinkDelayMs'" :: NullOrUndefined (Number), "DownlinkDelayMs'" :: NullOrUndefined (Number), "UplinkJitterMs'" :: NullOrUndefined (Number), "DownlinkJitterMs'" :: NullOrUndefined (Number), "UplinkLossPercent'" :: NullOrUndefined (PercentInteger), "DownlinkLossPercent'" :: NullOrUndefined (PercentInteger) }
```

#### `CreateNetworkProfileResult`

``` purescript
newtype CreateNetworkProfileResult
  = CreateNetworkProfileResult { "NetworkProfile'" :: NullOrUndefined (NetworkProfile) }
```

#### `CreateProjectRequest`

``` purescript
newtype CreateProjectRequest
  = CreateProjectRequest { "Name'" :: Name, "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes) }
```

<p>Represents a request to the create project operation.</p>

#### `CreateProjectResult`

``` purescript
newtype CreateProjectResult
  = CreateProjectResult { "Project'" :: NullOrUndefined (Project) }
```

<p>Represents the result of a create project request.</p>

#### `CreateRemoteAccessSessionConfiguration`

``` purescript
newtype CreateRemoteAccessSessionConfiguration
  = CreateRemoteAccessSessionConfiguration { "BillingMethod'" :: NullOrUndefined (BillingMethod) }
```

<p>Creates the configuration settings for a remote access session, including the device model and type.</p>

#### `CreateRemoteAccessSessionRequest`

``` purescript
newtype CreateRemoteAccessSessionRequest
  = CreateRemoteAccessSessionRequest { "ProjectArn'" :: AmazonResourceName, "DeviceArn'" :: AmazonResourceName, "SshPublicKey'" :: NullOrUndefined (SshPublicKey), "RemoteDebugEnabled'" :: NullOrUndefined (Boolean), "RemoteRecordEnabled'" :: NullOrUndefined (Boolean), "RemoteRecordAppArn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "ClientId'" :: NullOrUndefined (ClientId), "Configuration'" :: NullOrUndefined (CreateRemoteAccessSessionConfiguration), "InteractionMode'" :: NullOrUndefined (InteractionMode) }
```

<p>Creates and submits a request to start a remote access session.</p>

#### `CreateRemoteAccessSessionResult`

``` purescript
newtype CreateRemoteAccessSessionResult
  = CreateRemoteAccessSessionResult { "RemoteAccessSession'" :: NullOrUndefined (RemoteAccessSession) }
```

<p>Represents the server response from a request to create a remote access session.</p>

#### `CreateUploadRequest`

``` purescript
newtype CreateUploadRequest
  = CreateUploadRequest { "ProjectArn'" :: AmazonResourceName, "Name'" :: Name, "Type'" :: UploadType, "ContentType'" :: NullOrUndefined (ContentType) }
```

<p>Represents a request to the create upload operation.</p>

#### `CreateUploadResult`

``` purescript
newtype CreateUploadResult
  = CreateUploadResult { "Upload'" :: NullOrUndefined (Upload) }
```

<p>Represents the result of a create upload request.</p>

#### `CurrencyCode`

``` purescript
newtype CurrencyCode
  = CurrencyCode String
```

#### `CustomerArtifactPaths`

``` purescript
newtype CustomerArtifactPaths
  = CustomerArtifactPaths { "IosPaths'" :: NullOrUndefined (IosPaths), "AndroidPaths'" :: NullOrUndefined (AndroidPaths), "DeviceHostPaths'" :: NullOrUndefined (DeviceHostPaths) }
```

<p>A JSON object specifying the paths where the artifacts generated by the customer's tests, on the device or in the test environment, will be pulled from.</p> <p>Specify <code>deviceHostPaths</code> and optionally specify either <code>iosPaths</code> or <code>androidPaths</code>.</p> <p>For web app tests, you can specify both <code>iosPaths</code> and <code>androidPaths</code>.</p>

#### `DateTime`

``` purescript
newtype DateTime
  = DateTime Number
```

#### `DeleteDevicePoolRequest`

``` purescript
newtype DeleteDevicePoolRequest
  = DeleteDevicePoolRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the delete device pool operation.</p>

#### `DeleteDevicePoolResult`

``` purescript
newtype DeleteDevicePoolResult
  = DeleteDevicePoolResult {  }
```

<p>Represents the result of a delete device pool request.</p>

#### `DeleteNetworkProfileRequest`

``` purescript
newtype DeleteNetworkProfileRequest
  = DeleteNetworkProfileRequest { "Arn'" :: AmazonResourceName }
```

#### `DeleteNetworkProfileResult`

``` purescript
newtype DeleteNetworkProfileResult
  = DeleteNetworkProfileResult {  }
```

#### `DeleteProjectRequest`

``` purescript
newtype DeleteProjectRequest
  = DeleteProjectRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the delete project operation.</p>

#### `DeleteProjectResult`

``` purescript
newtype DeleteProjectResult
  = DeleteProjectResult {  }
```

<p>Represents the result of a delete project request.</p>

#### `DeleteRemoteAccessSessionRequest`

``` purescript
newtype DeleteRemoteAccessSessionRequest
  = DeleteRemoteAccessSessionRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents the request to delete the specified remote access session.</p>

#### `DeleteRemoteAccessSessionResult`

``` purescript
newtype DeleteRemoteAccessSessionResult
  = DeleteRemoteAccessSessionResult {  }
```

<p>The response from the server when a request is made to delete the remote access session.</p>

#### `DeleteRunRequest`

``` purescript
newtype DeleteRunRequest
  = DeleteRunRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the delete run operation.</p>

#### `DeleteRunResult`

``` purescript
newtype DeleteRunResult
  = DeleteRunResult {  }
```

<p>Represents the result of a delete run request.</p>

#### `DeleteUploadRequest`

``` purescript
newtype DeleteUploadRequest
  = DeleteUploadRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the delete upload operation.</p>

#### `DeleteUploadResult`

``` purescript
newtype DeleteUploadResult
  = DeleteUploadResult {  }
```

<p>Represents the result of a delete upload request.</p>

#### `Device`

``` purescript
newtype Device
  = Device { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Manufacturer'" :: NullOrUndefined (String), "Model'" :: NullOrUndefined (String), "ModelId'" :: NullOrUndefined (String), "FormFactor'" :: NullOrUndefined (DeviceFormFactor), "Platform'" :: NullOrUndefined (DevicePlatform), "Os'" :: NullOrUndefined (String), "Cpu'" :: NullOrUndefined (CPU), "Resolution'" :: NullOrUndefined (Resolution), "HeapSize'" :: NullOrUndefined (Number), "Memory'" :: NullOrUndefined (Number), "Image'" :: NullOrUndefined (String), "Carrier'" :: NullOrUndefined (String), "Radio'" :: NullOrUndefined (String), "RemoteAccessEnabled'" :: NullOrUndefined (Boolean), "RemoteDebugEnabled'" :: NullOrUndefined (Boolean), "FleetType'" :: NullOrUndefined (String), "FleetName'" :: NullOrUndefined (String) }
```

<p>Represents a device type that an app is tested against.</p>

#### `DeviceAttribute`

``` purescript
newtype DeviceAttribute
  = DeviceAttribute String
```

#### `DeviceFormFactor`

``` purescript
newtype DeviceFormFactor
  = DeviceFormFactor String
```

#### `DeviceHostPaths`

``` purescript
newtype DeviceHostPaths
  = DeviceHostPaths (Array String)
```

#### `DeviceMinutes`

``` purescript
newtype DeviceMinutes
  = DeviceMinutes { "Total'" :: NullOrUndefined (Number), "Metered'" :: NullOrUndefined (Number), "Unmetered'" :: NullOrUndefined (Number) }
```

<p>Represents the total (metered or unmetered) minutes used by the resource to run tests. Contains the sum of minutes consumed by all children.</p>

#### `DevicePlatform`

``` purescript
newtype DevicePlatform
  = DevicePlatform String
```

#### `DevicePool`

``` purescript
newtype DevicePool
  = DevicePool { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Description'" :: NullOrUndefined (Message), "Type'" :: NullOrUndefined (DevicePoolType), "Rules'" :: NullOrUndefined (Rules) }
```

<p>Represents a collection of device types.</p>

#### `DevicePoolCompatibilityResult`

``` purescript
newtype DevicePoolCompatibilityResult
  = DevicePoolCompatibilityResult { "Device'" :: NullOrUndefined (Device), "Compatible'" :: NullOrUndefined (Boolean), "IncompatibilityMessages'" :: NullOrUndefined (IncompatibilityMessages) }
```

<p>Represents a device pool compatibility result.</p>

#### `DevicePoolCompatibilityResults`

``` purescript
newtype DevicePoolCompatibilityResults
  = DevicePoolCompatibilityResults (Array DevicePoolCompatibilityResult)
```

#### `DevicePoolType`

``` purescript
newtype DevicePoolType
  = DevicePoolType String
```

#### `DevicePools`

``` purescript
newtype DevicePools
  = DevicePools (Array DevicePool)
```

#### `Devices`

``` purescript
newtype Devices
  = Devices (Array Device)
```

#### `ExecutionConfiguration`

``` purescript
newtype ExecutionConfiguration
  = ExecutionConfiguration { "JobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes), "AccountsCleanup'" :: NullOrUndefined (AccountsCleanup), "AppPackagesCleanup'" :: NullOrUndefined (AppPackagesCleanup) }
```

<p>Represents configuration information about a test run, such as the execution timeout (in minutes).</p>

#### `ExecutionResult`

``` purescript
newtype ExecutionResult
  = ExecutionResult String
```

#### `ExecutionResultCode`

``` purescript
newtype ExecutionResultCode
  = ExecutionResultCode String
```

#### `ExecutionStatus`

``` purescript
newtype ExecutionStatus
  = ExecutionStatus String
```

#### `Filter`

``` purescript
newtype Filter
  = Filter String
```

#### `GetAccountSettingsRequest`

``` purescript
newtype GetAccountSettingsRequest
  = GetAccountSettingsRequest {  }
```

<p>Represents the request sent to retrieve the account settings.</p>

#### `GetAccountSettingsResult`

``` purescript
newtype GetAccountSettingsResult
  = GetAccountSettingsResult { "AccountSettings'" :: NullOrUndefined (AccountSettings) }
```

<p>Represents the account settings return values from the <code>GetAccountSettings</code> request.</p>

#### `GetDevicePoolCompatibilityRequest`

``` purescript
newtype GetDevicePoolCompatibilityRequest
  = GetDevicePoolCompatibilityRequest { "DevicePoolArn'" :: AmazonResourceName, "AppArn'" :: NullOrUndefined (AmazonResourceName), "TestType'" :: NullOrUndefined (TestType), "Test'" :: NullOrUndefined (ScheduleRunTest) }
```

<p>Represents a request to the get device pool compatibility operation.</p>

#### `GetDevicePoolCompatibilityResult`

``` purescript
newtype GetDevicePoolCompatibilityResult
  = GetDevicePoolCompatibilityResult { "CompatibleDevices'" :: NullOrUndefined (DevicePoolCompatibilityResults), "IncompatibleDevices'" :: NullOrUndefined (DevicePoolCompatibilityResults) }
```

<p>Represents the result of describe device pool compatibility request.</p>

#### `GetDevicePoolRequest`

``` purescript
newtype GetDevicePoolRequest
  = GetDevicePoolRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get device pool operation.</p>

#### `GetDevicePoolResult`

``` purescript
newtype GetDevicePoolResult
  = GetDevicePoolResult { "DevicePool'" :: NullOrUndefined (DevicePool) }
```

<p>Represents the result of a get device pool request.</p>

#### `GetDeviceRequest`

``` purescript
newtype GetDeviceRequest
  = GetDeviceRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get device request.</p>

#### `GetDeviceResult`

``` purescript
newtype GetDeviceResult
  = GetDeviceResult { "Device'" :: NullOrUndefined (Device) }
```

<p>Represents the result of a get device request.</p>

#### `GetJobRequest`

``` purescript
newtype GetJobRequest
  = GetJobRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get job operation.</p>

#### `GetJobResult`

``` purescript
newtype GetJobResult
  = GetJobResult { "Job'" :: NullOrUndefined (Job) }
```

<p>Represents the result of a get job request.</p>

#### `GetNetworkProfileRequest`

``` purescript
newtype GetNetworkProfileRequest
  = GetNetworkProfileRequest { "Arn'" :: AmazonResourceName }
```

#### `GetNetworkProfileResult`

``` purescript
newtype GetNetworkProfileResult
  = GetNetworkProfileResult { "NetworkProfile'" :: NullOrUndefined (NetworkProfile) }
```

#### `GetOfferingStatusRequest`

``` purescript
newtype GetOfferingStatusRequest
  = GetOfferingStatusRequest { "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the request to retrieve the offering status for the specified customer or account.</p>

#### `GetOfferingStatusResult`

``` purescript
newtype GetOfferingStatusResult
  = GetOfferingStatusResult { "Current'" :: NullOrUndefined (OfferingStatusMap), "NextPeriod'" :: NullOrUndefined (OfferingStatusMap), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Returns the status result for a device offering.</p>

#### `GetProjectRequest`

``` purescript
newtype GetProjectRequest
  = GetProjectRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get project operation.</p>

#### `GetProjectResult`

``` purescript
newtype GetProjectResult
  = GetProjectResult { "Project'" :: NullOrUndefined (Project) }
```

<p>Represents the result of a get project request.</p>

#### `GetRemoteAccessSessionRequest`

``` purescript
newtype GetRemoteAccessSessionRequest
  = GetRemoteAccessSessionRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents the request to get information about the specified remote access session.</p>

#### `GetRemoteAccessSessionResult`

``` purescript
newtype GetRemoteAccessSessionResult
  = GetRemoteAccessSessionResult { "RemoteAccessSession'" :: NullOrUndefined (RemoteAccessSession) }
```

<p>Represents the response from the server that lists detailed information about the remote access session.</p>

#### `GetRunRequest`

``` purescript
newtype GetRunRequest
  = GetRunRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get run operation.</p>

#### `GetRunResult`

``` purescript
newtype GetRunResult
  = GetRunResult { "Run'" :: NullOrUndefined (Run) }
```

<p>Represents the result of a get run request.</p>

#### `GetSuiteRequest`

``` purescript
newtype GetSuiteRequest
  = GetSuiteRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get suite operation.</p>

#### `GetSuiteResult`

``` purescript
newtype GetSuiteResult
  = GetSuiteResult { "Suite'" :: NullOrUndefined (Suite) }
```

<p>Represents the result of a get suite request.</p>

#### `GetTestRequest`

``` purescript
newtype GetTestRequest
  = GetTestRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get test operation.</p>

#### `GetTestResult`

``` purescript
newtype GetTestResult
  = GetTestResult { "Test'" :: NullOrUndefined (Test) }
```

<p>Represents the result of a get test request.</p>

#### `GetUploadRequest`

``` purescript
newtype GetUploadRequest
  = GetUploadRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get upload operation.</p>

#### `GetUploadResult`

``` purescript
newtype GetUploadResult
  = GetUploadResult { "Upload'" :: NullOrUndefined (Upload) }
```

<p>Represents the result of a get upload request.</p>

#### `HostAddress`

``` purescript
newtype HostAddress
  = HostAddress String
```

#### `IdempotencyException`

``` purescript
newtype IdempotencyException
  = IdempotencyException { "Message'" :: NullOrUndefined (Message) }
```

<p>An entity with the same name already exists.</p>

#### `IncompatibilityMessage`

``` purescript
newtype IncompatibilityMessage
  = IncompatibilityMessage { "Message'" :: NullOrUndefined (Message), "Type'" :: NullOrUndefined (DeviceAttribute) }
```

<p>Represents information about incompatibility.</p>

#### `IncompatibilityMessages`

``` purescript
newtype IncompatibilityMessages
  = IncompatibilityMessages (Array IncompatibilityMessage)
```

#### `InstallToRemoteAccessSessionRequest`

``` purescript
newtype InstallToRemoteAccessSessionRequest
  = InstallToRemoteAccessSessionRequest { "RemoteAccessSessionArn'" :: AmazonResourceName, "AppArn'" :: AmazonResourceName }
```

<p>Represents the request to install an Android application (in .apk format) or an iOS application (in .ipa format) as part of a remote access session.</p>

#### `InstallToRemoteAccessSessionResult`

``` purescript
newtype InstallToRemoteAccessSessionResult
  = InstallToRemoteAccessSessionResult { "AppUpload'" :: NullOrUndefined (Upload) }
```

<p>Represents the response from the server after AWS Device Farm makes a request to install to a remote access session.</p>

#### `InteractionMode`

``` purescript
newtype InteractionMode
  = InteractionMode String
```

#### `IosPaths`

``` purescript
newtype IosPaths
  = IosPaths (Array String)
```

#### `Job`

``` purescript
newtype Job
  = Job { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Type'" :: NullOrUndefined (TestType), "Created'" :: NullOrUndefined (DateTime), "Status'" :: NullOrUndefined (ExecutionStatus), "Result'" :: NullOrUndefined (ExecutionResult), "Started'" :: NullOrUndefined (DateTime), "Stopped'" :: NullOrUndefined (DateTime), "Counters'" :: NullOrUndefined (Counters), "Message'" :: NullOrUndefined (Message), "Device'" :: NullOrUndefined (Device), "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes) }
```

<p>Represents a device.</p>

#### `JobTimeoutMinutes`

``` purescript
newtype JobTimeoutMinutes
  = JobTimeoutMinutes Int
```

#### `Jobs`

``` purescript
newtype Jobs
  = Jobs (Array Job)
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (Message) }
```

<p>A limit was exceeded.</p>

#### `ListArtifactsRequest`

``` purescript
newtype ListArtifactsRequest
  = ListArtifactsRequest { "Arn'" :: AmazonResourceName, "Type'" :: ArtifactCategory, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list artifacts operation.</p>

#### `ListArtifactsResult`

``` purescript
newtype ListArtifactsResult
  = ListArtifactsResult { "Artifacts'" :: NullOrUndefined (Artifacts), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list artifacts operation.</p>

#### `ListDevicePoolsRequest`

``` purescript
newtype ListDevicePoolsRequest
  = ListDevicePoolsRequest { "Arn'" :: AmazonResourceName, "Type'" :: NullOrUndefined (DevicePoolType), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list device pools request.</p>

#### `ListDevicePoolsResult`

``` purescript
newtype ListDevicePoolsResult
  = ListDevicePoolsResult { "DevicePools'" :: NullOrUndefined (DevicePools), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list device pools request.</p>

#### `ListDevicesRequest`

``` purescript
newtype ListDevicesRequest
  = ListDevicesRequest { "Arn'" :: NullOrUndefined (AmazonResourceName), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list devices request.</p>

#### `ListDevicesResult`

``` purescript
newtype ListDevicesResult
  = ListDevicesResult { "Devices'" :: NullOrUndefined (Devices), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list devices operation.</p>

#### `ListJobsRequest`

``` purescript
newtype ListJobsRequest
  = ListJobsRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list jobs operation.</p>

#### `ListJobsResult`

``` purescript
newtype ListJobsResult
  = ListJobsResult { "Jobs'" :: NullOrUndefined (Jobs), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list jobs request.</p>

#### `ListNetworkProfilesRequest`

``` purescript
newtype ListNetworkProfilesRequest
  = ListNetworkProfilesRequest { "Arn'" :: AmazonResourceName, "Type'" :: NullOrUndefined (NetworkProfileType), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `ListNetworkProfilesResult`

``` purescript
newtype ListNetworkProfilesResult
  = ListNetworkProfilesResult { "NetworkProfiles'" :: NullOrUndefined (NetworkProfiles), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `ListOfferingPromotionsRequest`

``` purescript
newtype ListOfferingPromotionsRequest
  = ListOfferingPromotionsRequest { "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `ListOfferingPromotionsResult`

``` purescript
newtype ListOfferingPromotionsResult
  = ListOfferingPromotionsResult { "OfferingPromotions'" :: NullOrUndefined (OfferingPromotions), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `ListOfferingTransactionsRequest`

``` purescript
newtype ListOfferingTransactionsRequest
  = ListOfferingTransactionsRequest { "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the request to list the offering transaction history.</p>

#### `ListOfferingTransactionsResult`

``` purescript
newtype ListOfferingTransactionsResult
  = ListOfferingTransactionsResult { "OfferingTransactions'" :: NullOrUndefined (OfferingTransactions), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Returns the transaction log of the specified offerings.</p>

#### `ListOfferingsRequest`

``` purescript
newtype ListOfferingsRequest
  = ListOfferingsRequest { "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the request to list all offerings.</p>

#### `ListOfferingsResult`

``` purescript
newtype ListOfferingsResult
  = ListOfferingsResult { "Offerings'" :: NullOrUndefined (Offerings), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the return values of the list of offerings.</p>

#### `ListProjectsRequest`

``` purescript
newtype ListProjectsRequest
  = ListProjectsRequest { "Arn'" :: NullOrUndefined (AmazonResourceName), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list projects operation.</p>

#### `ListProjectsResult`

``` purescript
newtype ListProjectsResult
  = ListProjectsResult { "Projects'" :: NullOrUndefined (Projects), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list projects request.</p>

#### `ListRemoteAccessSessionsRequest`

``` purescript
newtype ListRemoteAccessSessionsRequest
  = ListRemoteAccessSessionsRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the request to return information about the remote access session.</p>

#### `ListRemoteAccessSessionsResult`

``` purescript
newtype ListRemoteAccessSessionsResult
  = ListRemoteAccessSessionsResult { "RemoteAccessSessions'" :: NullOrUndefined (RemoteAccessSessions), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the response from the server after AWS Device Farm makes a request to return information about the remote access session.</p>

#### `ListRunsRequest`

``` purescript
newtype ListRunsRequest
  = ListRunsRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list runs operation.</p>

#### `ListRunsResult`

``` purescript
newtype ListRunsResult
  = ListRunsResult { "Runs'" :: NullOrUndefined (Runs), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list runs request.</p>

#### `ListSamplesRequest`

``` purescript
newtype ListSamplesRequest
  = ListSamplesRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list samples operation.</p>

#### `ListSamplesResult`

``` purescript
newtype ListSamplesResult
  = ListSamplesResult { "Samples'" :: NullOrUndefined (Samples), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list samples request.</p>

#### `ListSuitesRequest`

``` purescript
newtype ListSuitesRequest
  = ListSuitesRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list suites operation.</p>

#### `ListSuitesResult`

``` purescript
newtype ListSuitesResult
  = ListSuitesResult { "Suites'" :: NullOrUndefined (Suites), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list suites request.</p>

#### `ListTestsRequest`

``` purescript
newtype ListTestsRequest
  = ListTestsRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list tests operation.</p>

#### `ListTestsResult`

``` purescript
newtype ListTestsResult
  = ListTestsResult { "Tests'" :: NullOrUndefined (Tests), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list tests request.</p>

#### `ListUniqueProblemsRequest`

``` purescript
newtype ListUniqueProblemsRequest
  = ListUniqueProblemsRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list unique problems operation.</p>

#### `ListUniqueProblemsResult`

``` purescript
newtype ListUniqueProblemsResult
  = ListUniqueProblemsResult { "UniqueProblems'" :: NullOrUndefined (UniqueProblemsByExecutionResultMap), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list unique problems request.</p>

#### `ListUploadsRequest`

``` purescript
newtype ListUploadsRequest
  = ListUploadsRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list uploads operation.</p>

#### `ListUploadsResult`

``` purescript
newtype ListUploadsResult
  = ListUploadsResult { "Uploads'" :: NullOrUndefined (Uploads), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list uploads request.</p>

#### `Location`

``` purescript
newtype Location
  = Location { "Latitude'" :: Number, "Longitude'" :: Number }
```

<p>Represents a latitude and longitude pair, expressed in geographic coordinate system degrees (for example 47.6204, -122.3491).</p> <p>Elevation is currently not supported.</p>

#### `MaxSlotMap`

``` purescript
newtype MaxSlotMap
  = MaxSlotMap (Map String Int)
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

#### `Metadata`

``` purescript
newtype Metadata
  = Metadata String
```

#### `MonetaryAmount`

``` purescript
newtype MonetaryAmount
  = MonetaryAmount { "Amount'" :: NullOrUndefined (Number), "CurrencyCode'" :: NullOrUndefined (CurrencyCode) }
```

<p>A number representing the monetary amount for an offering or transaction.</p>

#### `Name`

``` purescript
newtype Name
  = Name String
```

#### `NetworkProfile`

``` purescript
newtype NetworkProfile
  = NetworkProfile { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Description'" :: NullOrUndefined (Message), "Type'" :: NullOrUndefined (NetworkProfileType), "UplinkBandwidthBits'" :: NullOrUndefined (Number), "DownlinkBandwidthBits'" :: NullOrUndefined (Number), "UplinkDelayMs'" :: NullOrUndefined (Number), "DownlinkDelayMs'" :: NullOrUndefined (Number), "UplinkJitterMs'" :: NullOrUndefined (Number), "DownlinkJitterMs'" :: NullOrUndefined (Number), "UplinkLossPercent'" :: NullOrUndefined (PercentInteger), "DownlinkLossPercent'" :: NullOrUndefined (PercentInteger) }
```

<p>An array of settings that describes characteristics of a network profile.</p>

#### `NetworkProfileType`

``` purescript
newtype NetworkProfileType
  = NetworkProfileType String
```

#### `NetworkProfiles`

``` purescript
newtype NetworkProfiles
  = NetworkProfiles (Array NetworkProfile)
```

#### `NotEligibleException`

``` purescript
newtype NotEligibleException
  = NotEligibleException { "Message'" :: NullOrUndefined (Message) }
```

<p>Exception gets thrown when a user is not eligible to perform the specified transaction.</p>

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message'" :: NullOrUndefined (Message) }
```

<p>The specified entity was not found.</p>

#### `Offering`

``` purescript
newtype Offering
  = Offering { "Id'" :: NullOrUndefined (OfferingIdentifier), "Description'" :: NullOrUndefined (Message), "Type'" :: NullOrUndefined (OfferingType), "Platform'" :: NullOrUndefined (DevicePlatform), "RecurringCharges'" :: NullOrUndefined (RecurringCharges) }
```

<p>Represents the metadata of a device offering.</p>

#### `OfferingIdentifier`

``` purescript
newtype OfferingIdentifier
  = OfferingIdentifier String
```

#### `OfferingPromotion`

``` purescript
newtype OfferingPromotion
  = OfferingPromotion { "Id'" :: NullOrUndefined (OfferingPromotionIdentifier), "Description'" :: NullOrUndefined (Message) }
```

<p>Represents information about an offering promotion.</p>

#### `OfferingPromotionIdentifier`

``` purescript
newtype OfferingPromotionIdentifier
  = OfferingPromotionIdentifier String
```

#### `OfferingPromotions`

``` purescript
newtype OfferingPromotions
  = OfferingPromotions (Array OfferingPromotion)
```

#### `OfferingStatus`

``` purescript
newtype OfferingStatus
  = OfferingStatus { "Type'" :: NullOrUndefined (OfferingTransactionType), "Offering'" :: NullOrUndefined (Offering), "Quantity'" :: NullOrUndefined (Int), "EffectiveOn'" :: NullOrUndefined (DateTime) }
```

<p>The status of the offering.</p>

#### `OfferingStatusMap`

``` purescript
newtype OfferingStatusMap
  = OfferingStatusMap (Map OfferingIdentifier OfferingStatus)
```

#### `OfferingTransaction`

``` purescript
newtype OfferingTransaction
  = OfferingTransaction { "OfferingStatus'" :: NullOrUndefined (OfferingStatus), "TransactionId'" :: NullOrUndefined (TransactionIdentifier), "OfferingPromotionId'" :: NullOrUndefined (OfferingPromotionIdentifier), "CreatedOn'" :: NullOrUndefined (DateTime), "Cost'" :: NullOrUndefined (MonetaryAmount) }
```

<p>Represents the metadata of an offering transaction.</p>

#### `OfferingTransactionType`

``` purescript
newtype OfferingTransactionType
  = OfferingTransactionType String
```

#### `OfferingTransactions`

``` purescript
newtype OfferingTransactions
  = OfferingTransactions (Array OfferingTransaction)
```

#### `OfferingType`

``` purescript
newtype OfferingType
  = OfferingType String
```

#### `Offerings`

``` purescript
newtype Offerings
  = Offerings (Array Offering)
```

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

#### `PercentInteger`

``` purescript
newtype PercentInteger
  = PercentInteger Int
```

#### `Problem`

``` purescript
newtype Problem
  = Problem { "Run'" :: NullOrUndefined (ProblemDetail), "Job'" :: NullOrUndefined (ProblemDetail), "Suite'" :: NullOrUndefined (ProblemDetail), "Test'" :: NullOrUndefined (ProblemDetail), "Device'" :: NullOrUndefined (Device), "Result'" :: NullOrUndefined (ExecutionResult), "Message'" :: NullOrUndefined (Message) }
```

<p>Represents a specific warning or failure.</p>

#### `ProblemDetail`

``` purescript
newtype ProblemDetail
  = ProblemDetail { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name) }
```

<p>Information about a problem detail.</p>

#### `Problems`

``` purescript
newtype Problems
  = Problems (Array Problem)
```

#### `Project`

``` purescript
newtype Project
  = Project { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes), "Created'" :: NullOrUndefined (DateTime) }
```

<p>Represents an operating-system neutral workspace for running and managing tests.</p>

#### `Projects`

``` purescript
newtype Projects
  = Projects (Array Project)
```

#### `PurchaseOfferingRequest`

``` purescript
newtype PurchaseOfferingRequest
  = PurchaseOfferingRequest { "OfferingId'" :: NullOrUndefined (OfferingIdentifier), "Quantity'" :: NullOrUndefined (Int), "OfferingPromotionId'" :: NullOrUndefined (OfferingPromotionIdentifier) }
```

<p>Represents a request for a purchase offering.</p>

#### `PurchaseOfferingResult`

``` purescript
newtype PurchaseOfferingResult
  = PurchaseOfferingResult { "OfferingTransaction'" :: NullOrUndefined (OfferingTransaction) }
```

<p>The result of the purchase offering (e.g., success or failure).</p>

#### `PurchasedDevicesMap`

``` purescript
newtype PurchasedDevicesMap
  = PurchasedDevicesMap (Map DevicePlatform Int)
```

#### `Radios`

``` purescript
newtype Radios
  = Radios { "Wifi'" :: NullOrUndefined (Boolean), "Bluetooth'" :: NullOrUndefined (Boolean), "Nfc'" :: NullOrUndefined (Boolean), "Gps'" :: NullOrUndefined (Boolean) }
```

<p>Represents the set of radios and their states on a device. Examples of radios include Wi-Fi, GPS, Bluetooth, and NFC.</p>

#### `RecurringCharge`

``` purescript
newtype RecurringCharge
  = RecurringCharge { "Cost'" :: NullOrUndefined (MonetaryAmount), "Frequency'" :: NullOrUndefined (RecurringChargeFrequency) }
```

<p>Specifies whether charges for devices will be recurring.</p>

#### `RecurringChargeFrequency`

``` purescript
newtype RecurringChargeFrequency
  = RecurringChargeFrequency String
```

#### `RecurringCharges`

``` purescript
newtype RecurringCharges
  = RecurringCharges (Array RecurringCharge)
```

#### `RemoteAccessSession`

``` purescript
newtype RemoteAccessSession
  = RemoteAccessSession { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Created'" :: NullOrUndefined (DateTime), "Status'" :: NullOrUndefined (ExecutionStatus), "Result'" :: NullOrUndefined (ExecutionResult), "Message'" :: NullOrUndefined (Message), "Started'" :: NullOrUndefined (DateTime), "Stopped'" :: NullOrUndefined (DateTime), "Device'" :: NullOrUndefined (Device), "RemoteDebugEnabled'" :: NullOrUndefined (Boolean), "RemoteRecordEnabled'" :: NullOrUndefined (Boolean), "RemoteRecordAppArn'" :: NullOrUndefined (AmazonResourceName), "HostAddress'" :: NullOrUndefined (HostAddress), "ClientId'" :: NullOrUndefined (ClientId), "BillingMethod'" :: NullOrUndefined (BillingMethod), "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes), "Endpoint'" :: NullOrUndefined (String), "DeviceUdid'" :: NullOrUndefined (String), "InteractionMode'" :: NullOrUndefined (InteractionMode) }
```

<p>Represents information about the remote access session.</p>

#### `RemoteAccessSessions`

``` purescript
newtype RemoteAccessSessions
  = RemoteAccessSessions (Array RemoteAccessSession)
```

#### `RenewOfferingRequest`

``` purescript
newtype RenewOfferingRequest
  = RenewOfferingRequest { "OfferingId'" :: NullOrUndefined (OfferingIdentifier), "Quantity'" :: NullOrUndefined (Int) }
```

<p>A request representing an offering renewal.</p>

#### `RenewOfferingResult`

``` purescript
newtype RenewOfferingResult
  = RenewOfferingResult { "OfferingTransaction'" :: NullOrUndefined (OfferingTransaction) }
```

<p>The result of a renewal offering.</p>

#### `Resolution`

``` purescript
newtype Resolution
  = Resolution { "Width'" :: NullOrUndefined (Int), "Height'" :: NullOrUndefined (Int) }
```

<p>Represents the screen resolution of a device in height and width, expressed in pixels.</p>

#### `Rule`

``` purescript
newtype Rule
  = Rule { "Attribute'" :: NullOrUndefined (DeviceAttribute), "Operator'" :: NullOrUndefined (RuleOperator), "Value'" :: NullOrUndefined (String) }
```

<p>Represents a condition for a device pool.</p>

#### `RuleOperator`

``` purescript
newtype RuleOperator
  = RuleOperator String
```

#### `Rules`

``` purescript
newtype Rules
  = Rules (Array Rule)
```

#### `Run`

``` purescript
newtype Run
  = Run { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Type'" :: NullOrUndefined (TestType), "Platform'" :: NullOrUndefined (DevicePlatform), "Created'" :: NullOrUndefined (DateTime), "Status'" :: NullOrUndefined (ExecutionStatus), "Result'" :: NullOrUndefined (ExecutionResult), "Started'" :: NullOrUndefined (DateTime), "Stopped'" :: NullOrUndefined (DateTime), "Counters'" :: NullOrUndefined (Counters), "Message'" :: NullOrUndefined (Message), "TotalJobs'" :: NullOrUndefined (Int), "CompletedJobs'" :: NullOrUndefined (Int), "BillingMethod'" :: NullOrUndefined (BillingMethod), "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes), "NetworkProfile'" :: NullOrUndefined (NetworkProfile), "ParsingResultUrl'" :: NullOrUndefined (String), "ResultCode'" :: NullOrUndefined (ExecutionResultCode), "Seed'" :: NullOrUndefined (Int), "AppUpload'" :: NullOrUndefined (AmazonResourceName), "EventCount'" :: NullOrUndefined (Int), "JobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes), "DevicePoolArn'" :: NullOrUndefined (AmazonResourceName), "Locale'" :: NullOrUndefined (String), "Radios'" :: NullOrUndefined (Radios), "Location'" :: NullOrUndefined (Location), "CustomerArtifactPaths'" :: NullOrUndefined (CustomerArtifactPaths), "WebUrl'" :: NullOrUndefined (String) }
```

<p>Represents a test run on a set of devices with a given app package, test parameters, etc.</p>

#### `Runs`

``` purescript
newtype Runs
  = Runs (Array Run)
```

#### `Sample`

``` purescript
newtype Sample
  = Sample { "Arn'" :: NullOrUndefined (AmazonResourceName), "Type'" :: NullOrUndefined (SampleType), "Url'" :: NullOrUndefined (URL) }
```

<p>Represents a sample of performance data.</p>

#### `SampleType`

``` purescript
newtype SampleType
  = SampleType String
```

#### `Samples`

``` purescript
newtype Samples
  = Samples (Array Sample)
```

#### `ScheduleRunConfiguration`

``` purescript
newtype ScheduleRunConfiguration
  = ScheduleRunConfiguration { "ExtraDataPackageArn'" :: NullOrUndefined (AmazonResourceName), "NetworkProfileArn'" :: NullOrUndefined (AmazonResourceName), "Locale'" :: NullOrUndefined (String), "Location'" :: NullOrUndefined (Location), "CustomerArtifactPaths'" :: NullOrUndefined (CustomerArtifactPaths), "Radios'" :: NullOrUndefined (Radios), "AuxiliaryApps'" :: NullOrUndefined (AmazonResourceNames), "BillingMethod'" :: NullOrUndefined (BillingMethod) }
```

<p>Represents the settings for a run. Includes things like location, radio states, auxiliary apps, and network profiles.</p>

#### `ScheduleRunRequest`

``` purescript
newtype ScheduleRunRequest
  = ScheduleRunRequest { "ProjectArn'" :: AmazonResourceName, "AppArn'" :: NullOrUndefined (AmazonResourceName), "DevicePoolArn'" :: AmazonResourceName, "Name'" :: NullOrUndefined (Name), "Test'" :: ScheduleRunTest, "Configuration'" :: NullOrUndefined (ScheduleRunConfiguration), "ExecutionConfiguration'" :: NullOrUndefined (ExecutionConfiguration) }
```

<p>Represents a request to the schedule run operation.</p>

#### `ScheduleRunResult`

``` purescript
newtype ScheduleRunResult
  = ScheduleRunResult { "Run'" :: NullOrUndefined (Run) }
```

<p>Represents the result of a schedule run request.</p>

#### `ScheduleRunTest`

``` purescript
newtype ScheduleRunTest
  = ScheduleRunTest { "Type'" :: TestType, "TestPackageArn'" :: NullOrUndefined (AmazonResourceName), "Filter'" :: NullOrUndefined (Filter), "Parameters'" :: NullOrUndefined (TestParameters) }
```

<p>Represents additional test settings.</p>

#### `ServiceAccountException`

``` purescript
newtype ServiceAccountException
  = ServiceAccountException { "Message'" :: NullOrUndefined (Message) }
```

<p>There was a problem with the service account.</p>

#### `SshPublicKey`

``` purescript
newtype SshPublicKey
  = SshPublicKey String
```

#### `StopRemoteAccessSessionRequest`

``` purescript
newtype StopRemoteAccessSessionRequest
  = StopRemoteAccessSessionRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents the request to stop the remote access session.</p>

#### `StopRemoteAccessSessionResult`

``` purescript
newtype StopRemoteAccessSessionResult
  = StopRemoteAccessSessionResult { "RemoteAccessSession'" :: NullOrUndefined (RemoteAccessSession) }
```

<p>Represents the response from the server that describes the remote access session when AWS Device Farm stops the session.</p>

#### `StopRunRequest`

``` purescript
newtype StopRunRequest
  = StopRunRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents the request to stop a specific run.</p>

#### `StopRunResult`

``` purescript
newtype StopRunResult
  = StopRunResult { "Run'" :: NullOrUndefined (Run) }
```

<p>Represents the results of your stop run attempt.</p>

#### `Suite`

``` purescript
newtype Suite
  = Suite { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Type'" :: NullOrUndefined (TestType), "Created'" :: NullOrUndefined (DateTime), "Status'" :: NullOrUndefined (ExecutionStatus), "Result'" :: NullOrUndefined (ExecutionResult), "Started'" :: NullOrUndefined (DateTime), "Stopped'" :: NullOrUndefined (DateTime), "Counters'" :: NullOrUndefined (Counters), "Message'" :: NullOrUndefined (Message), "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes) }
```

<p>Represents a collection of one or more tests.</p>

#### `Suites`

``` purescript
newtype Suites
  = Suites (Array Suite)
```

#### `Test`

``` purescript
newtype Test
  = Test { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Type'" :: NullOrUndefined (TestType), "Created'" :: NullOrUndefined (DateTime), "Status'" :: NullOrUndefined (ExecutionStatus), "Result'" :: NullOrUndefined (ExecutionResult), "Started'" :: NullOrUndefined (DateTime), "Stopped'" :: NullOrUndefined (DateTime), "Counters'" :: NullOrUndefined (Counters), "Message'" :: NullOrUndefined (Message), "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes) }
```

<p>Represents a condition that is evaluated.</p>

#### `TestParameters`

``` purescript
newtype TestParameters
  = TestParameters (Map String String)
```

#### `TestType`

``` purescript
newtype TestType
  = TestType String
```

#### `Tests`

``` purescript
newtype Tests
  = Tests (Array Test)
```

#### `TransactionIdentifier`

``` purescript
newtype TransactionIdentifier
  = TransactionIdentifier String
```

#### `TrialMinutes`

``` purescript
newtype TrialMinutes
  = TrialMinutes { "Total'" :: NullOrUndefined (Number), "Remaining'" :: NullOrUndefined (Number) }
```

<p>Represents information about free trial device minutes for an AWS account.</p>

#### `URL`

``` purescript
newtype URL
  = URL String
```

#### `UniqueProblem`

``` purescript
newtype UniqueProblem
  = UniqueProblem { "Message'" :: NullOrUndefined (Message), "Problems'" :: NullOrUndefined (Problems) }
```

<p>A collection of one or more problems, grouped by their result.</p>

#### `UniqueProblems`

``` purescript
newtype UniqueProblems
  = UniqueProblems (Array UniqueProblem)
```

#### `UniqueProblemsByExecutionResultMap`

``` purescript
newtype UniqueProblemsByExecutionResultMap
  = UniqueProblemsByExecutionResultMap (Map ExecutionResult UniqueProblems)
```

#### `UpdateDevicePoolRequest`

``` purescript
newtype UpdateDevicePoolRequest
  = UpdateDevicePoolRequest { "Arn'" :: AmazonResourceName, "Name'" :: NullOrUndefined (Name), "Description'" :: NullOrUndefined (Message), "Rules'" :: NullOrUndefined (Rules) }
```

<p>Represents a request to the update device pool operation.</p>

#### `UpdateDevicePoolResult`

``` purescript
newtype UpdateDevicePoolResult
  = UpdateDevicePoolResult { "DevicePool'" :: NullOrUndefined (DevicePool) }
```

<p>Represents the result of an update device pool request.</p>

#### `UpdateNetworkProfileRequest`

``` purescript
newtype UpdateNetworkProfileRequest
  = UpdateNetworkProfileRequest { "Arn'" :: AmazonResourceName, "Name'" :: NullOrUndefined (Name), "Description'" :: NullOrUndefined (Message), "Type'" :: NullOrUndefined (NetworkProfileType), "UplinkBandwidthBits'" :: NullOrUndefined (Number), "DownlinkBandwidthBits'" :: NullOrUndefined (Number), "UplinkDelayMs'" :: NullOrUndefined (Number), "DownlinkDelayMs'" :: NullOrUndefined (Number), "UplinkJitterMs'" :: NullOrUndefined (Number), "DownlinkJitterMs'" :: NullOrUndefined (Number), "UplinkLossPercent'" :: NullOrUndefined (PercentInteger), "DownlinkLossPercent'" :: NullOrUndefined (PercentInteger) }
```

#### `UpdateNetworkProfileResult`

``` purescript
newtype UpdateNetworkProfileResult
  = UpdateNetworkProfileResult { "NetworkProfile'" :: NullOrUndefined (NetworkProfile) }
```

#### `UpdateProjectRequest`

``` purescript
newtype UpdateProjectRequest
  = UpdateProjectRequest { "Arn'" :: AmazonResourceName, "Name'" :: NullOrUndefined (Name), "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes) }
```

<p>Represents a request to the update project operation.</p>

#### `UpdateProjectResult`

``` purescript
newtype UpdateProjectResult
  = UpdateProjectResult { "Project'" :: NullOrUndefined (Project) }
```

<p>Represents the result of an update project request.</p>

#### `Upload`

``` purescript
newtype Upload
  = Upload { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Created'" :: NullOrUndefined (DateTime), "Type'" :: NullOrUndefined (UploadType), "Status'" :: NullOrUndefined (UploadStatus), "Url'" :: NullOrUndefined (URL), "Metadata'" :: NullOrUndefined (Metadata), "ContentType'" :: NullOrUndefined (ContentType), "Message'" :: NullOrUndefined (Message) }
```

<p>An app or a set of one or more tests to upload or that have been uploaded.</p>

#### `UploadStatus`

``` purescript
newtype UploadStatus
  = UploadStatus String
```

#### `UploadType`

``` purescript
newtype UploadType
  = UploadType String
```

#### `Uploads`

``` purescript
newtype Uploads
  = Uploads (Array Upload)
```


