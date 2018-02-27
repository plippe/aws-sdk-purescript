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

##### Instances
``` purescript
Newtype AWSAccountNumber _
```

#### `AccountSettings`

``` purescript
newtype AccountSettings
  = AccountSettings { "AwsAccountNumber'" :: NullOrUndefined (AWSAccountNumber), "UnmeteredDevices'" :: NullOrUndefined (PurchasedDevicesMap), "UnmeteredRemoteAccessDevices'" :: NullOrUndefined (PurchasedDevicesMap), "MaxJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes), "TrialMinutes'" :: NullOrUndefined (TrialMinutes), "MaxSlots'" :: NullOrUndefined (MaxSlotMap), "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes) }
```

<p>A container for account-level settings within AWS Device Farm.</p>

##### Instances
``` purescript
Newtype AccountSettings _
```

#### `AccountsCleanup`

``` purescript
newtype AccountsCleanup
  = AccountsCleanup Boolean
```

##### Instances
``` purescript
Newtype AccountsCleanup _
```

#### `AmazonResourceName`

``` purescript
newtype AmazonResourceName
  = AmazonResourceName String
```

##### Instances
``` purescript
Newtype AmazonResourceName _
```

#### `AmazonResourceNames`

``` purescript
newtype AmazonResourceNames
  = AmazonResourceNames (Array AmazonResourceName)
```

##### Instances
``` purescript
Newtype AmazonResourceNames _
```

#### `AndroidPaths`

``` purescript
newtype AndroidPaths
  = AndroidPaths (Array String)
```

##### Instances
``` purescript
Newtype AndroidPaths _
```

#### `AppPackagesCleanup`

``` purescript
newtype AppPackagesCleanup
  = AppPackagesCleanup Boolean
```

##### Instances
``` purescript
Newtype AppPackagesCleanup _
```

#### `ArgumentException`

``` purescript
newtype ArgumentException
  = ArgumentException { "Message'" :: NullOrUndefined (Message) }
```

<p>An invalid argument was specified.</p>

##### Instances
``` purescript
Newtype ArgumentException _
```

#### `Artifact`

``` purescript
newtype Artifact
  = Artifact { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Type'" :: NullOrUndefined (ArtifactType), "Extension'" :: NullOrUndefined (String), "Url'" :: NullOrUndefined (URL) }
```

<p>Represents the output of a test. Examples of artifacts include logs and screenshots.</p>

##### Instances
``` purescript
Newtype Artifact _
```

#### `ArtifactCategory`

``` purescript
newtype ArtifactCategory
  = ArtifactCategory String
```

##### Instances
``` purescript
Newtype ArtifactCategory _
```

#### `ArtifactType`

``` purescript
newtype ArtifactType
  = ArtifactType String
```

##### Instances
``` purescript
Newtype ArtifactType _
```

#### `Artifacts`

``` purescript
newtype Artifacts
  = Artifacts (Array Artifact)
```

##### Instances
``` purescript
Newtype Artifacts _
```

#### `BillingMethod`

``` purescript
newtype BillingMethod
  = BillingMethod String
```

##### Instances
``` purescript
Newtype BillingMethod _
```

#### `CPU`

``` purescript
newtype CPU
  = CPU { "Frequency'" :: NullOrUndefined (String), "Architecture'" :: NullOrUndefined (String), "Clock'" :: NullOrUndefined (Number) }
```

<p>Represents the amount of CPU that an app is using on a physical device.</p> <p>Note that this does not represent system-wide CPU usage.</p>

##### Instances
``` purescript
Newtype CPU _
```

#### `ClientId`

``` purescript
newtype ClientId
  = ClientId String
```

##### Instances
``` purescript
Newtype ClientId _
```

#### `ContentType`

``` purescript
newtype ContentType
  = ContentType String
```

##### Instances
``` purescript
Newtype ContentType _
```

#### `Counters`

``` purescript
newtype Counters
  = Counters { "Total'" :: NullOrUndefined (Int), "Passed'" :: NullOrUndefined (Int), "Failed'" :: NullOrUndefined (Int), "Warned'" :: NullOrUndefined (Int), "Errored'" :: NullOrUndefined (Int), "Stopped'" :: NullOrUndefined (Int), "Skipped'" :: NullOrUndefined (Int) }
```

<p>Represents entity counters.</p>

##### Instances
``` purescript
Newtype Counters _
```

#### `CreateDevicePoolRequest`

``` purescript
newtype CreateDevicePoolRequest
  = CreateDevicePoolRequest { "ProjectArn'" :: AmazonResourceName, "Name'" :: Name, "Description'" :: NullOrUndefined (Message), "Rules'" :: Rules }
```

<p>Represents a request to the create device pool operation.</p>

##### Instances
``` purescript
Newtype CreateDevicePoolRequest _
```

#### `CreateDevicePoolResult`

``` purescript
newtype CreateDevicePoolResult
  = CreateDevicePoolResult { "DevicePool'" :: NullOrUndefined (DevicePool) }
```

<p>Represents the result of a create device pool request.</p>

##### Instances
``` purescript
Newtype CreateDevicePoolResult _
```

#### `CreateNetworkProfileRequest`

``` purescript
newtype CreateNetworkProfileRequest
  = CreateNetworkProfileRequest { "ProjectArn'" :: AmazonResourceName, "Name'" :: Name, "Description'" :: NullOrUndefined (Message), "Type'" :: NullOrUndefined (NetworkProfileType), "UplinkBandwidthBits'" :: NullOrUndefined (Number), "DownlinkBandwidthBits'" :: NullOrUndefined (Number), "UplinkDelayMs'" :: NullOrUndefined (Number), "DownlinkDelayMs'" :: NullOrUndefined (Number), "UplinkJitterMs'" :: NullOrUndefined (Number), "DownlinkJitterMs'" :: NullOrUndefined (Number), "UplinkLossPercent'" :: NullOrUndefined (PercentInteger), "DownlinkLossPercent'" :: NullOrUndefined (PercentInteger) }
```

##### Instances
``` purescript
Newtype CreateNetworkProfileRequest _
```

#### `CreateNetworkProfileResult`

``` purescript
newtype CreateNetworkProfileResult
  = CreateNetworkProfileResult { "NetworkProfile'" :: NullOrUndefined (NetworkProfile) }
```

##### Instances
``` purescript
Newtype CreateNetworkProfileResult _
```

#### `CreateProjectRequest`

``` purescript
newtype CreateProjectRequest
  = CreateProjectRequest { "Name'" :: Name, "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes) }
```

<p>Represents a request to the create project operation.</p>

##### Instances
``` purescript
Newtype CreateProjectRequest _
```

#### `CreateProjectResult`

``` purescript
newtype CreateProjectResult
  = CreateProjectResult { "Project'" :: NullOrUndefined (Project) }
```

<p>Represents the result of a create project request.</p>

##### Instances
``` purescript
Newtype CreateProjectResult _
```

#### `CreateRemoteAccessSessionConfiguration`

``` purescript
newtype CreateRemoteAccessSessionConfiguration
  = CreateRemoteAccessSessionConfiguration { "BillingMethod'" :: NullOrUndefined (BillingMethod) }
```

<p>Creates the configuration settings for a remote access session, including the device model and type.</p>

##### Instances
``` purescript
Newtype CreateRemoteAccessSessionConfiguration _
```

#### `CreateRemoteAccessSessionRequest`

``` purescript
newtype CreateRemoteAccessSessionRequest
  = CreateRemoteAccessSessionRequest { "ProjectArn'" :: AmazonResourceName, "DeviceArn'" :: AmazonResourceName, "SshPublicKey'" :: NullOrUndefined (SshPublicKey), "RemoteDebugEnabled'" :: NullOrUndefined (Boolean), "RemoteRecordEnabled'" :: NullOrUndefined (Boolean), "RemoteRecordAppArn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "ClientId'" :: NullOrUndefined (ClientId), "Configuration'" :: NullOrUndefined (CreateRemoteAccessSessionConfiguration), "InteractionMode'" :: NullOrUndefined (InteractionMode) }
```

<p>Creates and submits a request to start a remote access session.</p>

##### Instances
``` purescript
Newtype CreateRemoteAccessSessionRequest _
```

#### `CreateRemoteAccessSessionResult`

``` purescript
newtype CreateRemoteAccessSessionResult
  = CreateRemoteAccessSessionResult { "RemoteAccessSession'" :: NullOrUndefined (RemoteAccessSession) }
```

<p>Represents the server response from a request to create a remote access session.</p>

##### Instances
``` purescript
Newtype CreateRemoteAccessSessionResult _
```

#### `CreateUploadRequest`

``` purescript
newtype CreateUploadRequest
  = CreateUploadRequest { "ProjectArn'" :: AmazonResourceName, "Name'" :: Name, "Type'" :: UploadType, "ContentType'" :: NullOrUndefined (ContentType) }
```

<p>Represents a request to the create upload operation.</p>

##### Instances
``` purescript
Newtype CreateUploadRequest _
```

#### `CreateUploadResult`

``` purescript
newtype CreateUploadResult
  = CreateUploadResult { "Upload'" :: NullOrUndefined (Upload) }
```

<p>Represents the result of a create upload request.</p>

##### Instances
``` purescript
Newtype CreateUploadResult _
```

#### `CurrencyCode`

``` purescript
newtype CurrencyCode
  = CurrencyCode String
```

##### Instances
``` purescript
Newtype CurrencyCode _
```

#### `CustomerArtifactPaths`

``` purescript
newtype CustomerArtifactPaths
  = CustomerArtifactPaths { "IosPaths'" :: NullOrUndefined (IosPaths), "AndroidPaths'" :: NullOrUndefined (AndroidPaths), "DeviceHostPaths'" :: NullOrUndefined (DeviceHostPaths) }
```

<p>A JSON object specifying the paths where the artifacts generated by the customer's tests, on the device or in the test environment, will be pulled from.</p> <p>Specify <code>deviceHostPaths</code> and optionally specify either <code>iosPaths</code> or <code>androidPaths</code>.</p> <p>For web app tests, you can specify both <code>iosPaths</code> and <code>androidPaths</code>.</p>

##### Instances
``` purescript
Newtype CustomerArtifactPaths _
```

#### `DateTime`

``` purescript
newtype DateTime
  = DateTime Number
```

##### Instances
``` purescript
Newtype DateTime _
```

#### `DeleteDevicePoolRequest`

``` purescript
newtype DeleteDevicePoolRequest
  = DeleteDevicePoolRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the delete device pool operation.</p>

##### Instances
``` purescript
Newtype DeleteDevicePoolRequest _
```

#### `DeleteDevicePoolResult`

``` purescript
newtype DeleteDevicePoolResult
  = DeleteDevicePoolResult {  }
```

<p>Represents the result of a delete device pool request.</p>

##### Instances
``` purescript
Newtype DeleteDevicePoolResult _
```

#### `DeleteNetworkProfileRequest`

``` purescript
newtype DeleteNetworkProfileRequest
  = DeleteNetworkProfileRequest { "Arn'" :: AmazonResourceName }
```

##### Instances
``` purescript
Newtype DeleteNetworkProfileRequest _
```

#### `DeleteNetworkProfileResult`

``` purescript
newtype DeleteNetworkProfileResult
  = DeleteNetworkProfileResult {  }
```

##### Instances
``` purescript
Newtype DeleteNetworkProfileResult _
```

#### `DeleteProjectRequest`

``` purescript
newtype DeleteProjectRequest
  = DeleteProjectRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the delete project operation.</p>

##### Instances
``` purescript
Newtype DeleteProjectRequest _
```

#### `DeleteProjectResult`

``` purescript
newtype DeleteProjectResult
  = DeleteProjectResult {  }
```

<p>Represents the result of a delete project request.</p>

##### Instances
``` purescript
Newtype DeleteProjectResult _
```

#### `DeleteRemoteAccessSessionRequest`

``` purescript
newtype DeleteRemoteAccessSessionRequest
  = DeleteRemoteAccessSessionRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents the request to delete the specified remote access session.</p>

##### Instances
``` purescript
Newtype DeleteRemoteAccessSessionRequest _
```

#### `DeleteRemoteAccessSessionResult`

``` purescript
newtype DeleteRemoteAccessSessionResult
  = DeleteRemoteAccessSessionResult {  }
```

<p>The response from the server when a request is made to delete the remote access session.</p>

##### Instances
``` purescript
Newtype DeleteRemoteAccessSessionResult _
```

#### `DeleteRunRequest`

``` purescript
newtype DeleteRunRequest
  = DeleteRunRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the delete run operation.</p>

##### Instances
``` purescript
Newtype DeleteRunRequest _
```

#### `DeleteRunResult`

``` purescript
newtype DeleteRunResult
  = DeleteRunResult {  }
```

<p>Represents the result of a delete run request.</p>

##### Instances
``` purescript
Newtype DeleteRunResult _
```

#### `DeleteUploadRequest`

``` purescript
newtype DeleteUploadRequest
  = DeleteUploadRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the delete upload operation.</p>

##### Instances
``` purescript
Newtype DeleteUploadRequest _
```

#### `DeleteUploadResult`

``` purescript
newtype DeleteUploadResult
  = DeleteUploadResult {  }
```

<p>Represents the result of a delete upload request.</p>

##### Instances
``` purescript
Newtype DeleteUploadResult _
```

#### `Device`

``` purescript
newtype Device
  = Device { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Manufacturer'" :: NullOrUndefined (String), "Model'" :: NullOrUndefined (String), "ModelId'" :: NullOrUndefined (String), "FormFactor'" :: NullOrUndefined (DeviceFormFactor), "Platform'" :: NullOrUndefined (DevicePlatform), "Os'" :: NullOrUndefined (String), "Cpu'" :: NullOrUndefined (CPU), "Resolution'" :: NullOrUndefined (Resolution), "HeapSize'" :: NullOrUndefined (Number), "Memory'" :: NullOrUndefined (Number), "Image'" :: NullOrUndefined (String), "Carrier'" :: NullOrUndefined (String), "Radio'" :: NullOrUndefined (String), "RemoteAccessEnabled'" :: NullOrUndefined (Boolean), "RemoteDebugEnabled'" :: NullOrUndefined (Boolean), "FleetType'" :: NullOrUndefined (String), "FleetName'" :: NullOrUndefined (String) }
```

<p>Represents a device type that an app is tested against.</p>

##### Instances
``` purescript
Newtype Device _
```

#### `DeviceAttribute`

``` purescript
newtype DeviceAttribute
  = DeviceAttribute String
```

##### Instances
``` purescript
Newtype DeviceAttribute _
```

#### `DeviceFormFactor`

``` purescript
newtype DeviceFormFactor
  = DeviceFormFactor String
```

##### Instances
``` purescript
Newtype DeviceFormFactor _
```

#### `DeviceHostPaths`

``` purescript
newtype DeviceHostPaths
  = DeviceHostPaths (Array String)
```

##### Instances
``` purescript
Newtype DeviceHostPaths _
```

#### `DeviceMinutes`

``` purescript
newtype DeviceMinutes
  = DeviceMinutes { "Total'" :: NullOrUndefined (Number), "Metered'" :: NullOrUndefined (Number), "Unmetered'" :: NullOrUndefined (Number) }
```

<p>Represents the total (metered or unmetered) minutes used by the resource to run tests. Contains the sum of minutes consumed by all children.</p>

##### Instances
``` purescript
Newtype DeviceMinutes _
```

#### `DevicePlatform`

``` purescript
newtype DevicePlatform
  = DevicePlatform String
```

##### Instances
``` purescript
Newtype DevicePlatform _
```

#### `DevicePool`

``` purescript
newtype DevicePool
  = DevicePool { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Description'" :: NullOrUndefined (Message), "Type'" :: NullOrUndefined (DevicePoolType), "Rules'" :: NullOrUndefined (Rules) }
```

<p>Represents a collection of device types.</p>

##### Instances
``` purescript
Newtype DevicePool _
```

#### `DevicePoolCompatibilityResult`

``` purescript
newtype DevicePoolCompatibilityResult
  = DevicePoolCompatibilityResult { "Device'" :: NullOrUndefined (Device), "Compatible'" :: NullOrUndefined (Boolean), "IncompatibilityMessages'" :: NullOrUndefined (IncompatibilityMessages) }
```

<p>Represents a device pool compatibility result.</p>

##### Instances
``` purescript
Newtype DevicePoolCompatibilityResult _
```

#### `DevicePoolCompatibilityResults`

``` purescript
newtype DevicePoolCompatibilityResults
  = DevicePoolCompatibilityResults (Array DevicePoolCompatibilityResult)
```

##### Instances
``` purescript
Newtype DevicePoolCompatibilityResults _
```

#### `DevicePoolType`

``` purescript
newtype DevicePoolType
  = DevicePoolType String
```

##### Instances
``` purescript
Newtype DevicePoolType _
```

#### `DevicePools`

``` purescript
newtype DevicePools
  = DevicePools (Array DevicePool)
```

##### Instances
``` purescript
Newtype DevicePools _
```

#### `Devices`

``` purescript
newtype Devices
  = Devices (Array Device)
```

##### Instances
``` purescript
Newtype Devices _
```

#### `ExecutionConfiguration`

``` purescript
newtype ExecutionConfiguration
  = ExecutionConfiguration { "JobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes), "AccountsCleanup'" :: NullOrUndefined (AccountsCleanup), "AppPackagesCleanup'" :: NullOrUndefined (AppPackagesCleanup) }
```

<p>Represents configuration information about a test run, such as the execution timeout (in minutes).</p>

##### Instances
``` purescript
Newtype ExecutionConfiguration _
```

#### `ExecutionResult`

``` purescript
newtype ExecutionResult
  = ExecutionResult String
```

##### Instances
``` purescript
Newtype ExecutionResult _
```

#### `ExecutionResultCode`

``` purescript
newtype ExecutionResultCode
  = ExecutionResultCode String
```

##### Instances
``` purescript
Newtype ExecutionResultCode _
```

#### `ExecutionStatus`

``` purescript
newtype ExecutionStatus
  = ExecutionStatus String
```

##### Instances
``` purescript
Newtype ExecutionStatus _
```

#### `Filter`

``` purescript
newtype Filter
  = Filter String
```

##### Instances
``` purescript
Newtype Filter _
```

#### `GetAccountSettingsRequest`

``` purescript
newtype GetAccountSettingsRequest
  = GetAccountSettingsRequest {  }
```

<p>Represents the request sent to retrieve the account settings.</p>

##### Instances
``` purescript
Newtype GetAccountSettingsRequest _
```

#### `GetAccountSettingsResult`

``` purescript
newtype GetAccountSettingsResult
  = GetAccountSettingsResult { "AccountSettings'" :: NullOrUndefined (AccountSettings) }
```

<p>Represents the account settings return values from the <code>GetAccountSettings</code> request.</p>

##### Instances
``` purescript
Newtype GetAccountSettingsResult _
```

#### `GetDevicePoolCompatibilityRequest`

``` purescript
newtype GetDevicePoolCompatibilityRequest
  = GetDevicePoolCompatibilityRequest { "DevicePoolArn'" :: AmazonResourceName, "AppArn'" :: NullOrUndefined (AmazonResourceName), "TestType'" :: NullOrUndefined (TestType), "Test'" :: NullOrUndefined (ScheduleRunTest) }
```

<p>Represents a request to the get device pool compatibility operation.</p>

##### Instances
``` purescript
Newtype GetDevicePoolCompatibilityRequest _
```

#### `GetDevicePoolCompatibilityResult`

``` purescript
newtype GetDevicePoolCompatibilityResult
  = GetDevicePoolCompatibilityResult { "CompatibleDevices'" :: NullOrUndefined (DevicePoolCompatibilityResults), "IncompatibleDevices'" :: NullOrUndefined (DevicePoolCompatibilityResults) }
```

<p>Represents the result of describe device pool compatibility request.</p>

##### Instances
``` purescript
Newtype GetDevicePoolCompatibilityResult _
```

#### `GetDevicePoolRequest`

``` purescript
newtype GetDevicePoolRequest
  = GetDevicePoolRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get device pool operation.</p>

##### Instances
``` purescript
Newtype GetDevicePoolRequest _
```

#### `GetDevicePoolResult`

``` purescript
newtype GetDevicePoolResult
  = GetDevicePoolResult { "DevicePool'" :: NullOrUndefined (DevicePool) }
```

<p>Represents the result of a get device pool request.</p>

##### Instances
``` purescript
Newtype GetDevicePoolResult _
```

#### `GetDeviceRequest`

``` purescript
newtype GetDeviceRequest
  = GetDeviceRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get device request.</p>

##### Instances
``` purescript
Newtype GetDeviceRequest _
```

#### `GetDeviceResult`

``` purescript
newtype GetDeviceResult
  = GetDeviceResult { "Device'" :: NullOrUndefined (Device) }
```

<p>Represents the result of a get device request.</p>

##### Instances
``` purescript
Newtype GetDeviceResult _
```

#### `GetJobRequest`

``` purescript
newtype GetJobRequest
  = GetJobRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get job operation.</p>

##### Instances
``` purescript
Newtype GetJobRequest _
```

#### `GetJobResult`

``` purescript
newtype GetJobResult
  = GetJobResult { "Job'" :: NullOrUndefined (Job) }
```

<p>Represents the result of a get job request.</p>

##### Instances
``` purescript
Newtype GetJobResult _
```

#### `GetNetworkProfileRequest`

``` purescript
newtype GetNetworkProfileRequest
  = GetNetworkProfileRequest { "Arn'" :: AmazonResourceName }
```

##### Instances
``` purescript
Newtype GetNetworkProfileRequest _
```

#### `GetNetworkProfileResult`

``` purescript
newtype GetNetworkProfileResult
  = GetNetworkProfileResult { "NetworkProfile'" :: NullOrUndefined (NetworkProfile) }
```

##### Instances
``` purescript
Newtype GetNetworkProfileResult _
```

#### `GetOfferingStatusRequest`

``` purescript
newtype GetOfferingStatusRequest
  = GetOfferingStatusRequest { "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the request to retrieve the offering status for the specified customer or account.</p>

##### Instances
``` purescript
Newtype GetOfferingStatusRequest _
```

#### `GetOfferingStatusResult`

``` purescript
newtype GetOfferingStatusResult
  = GetOfferingStatusResult { "Current'" :: NullOrUndefined (OfferingStatusMap), "NextPeriod'" :: NullOrUndefined (OfferingStatusMap), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Returns the status result for a device offering.</p>

##### Instances
``` purescript
Newtype GetOfferingStatusResult _
```

#### `GetProjectRequest`

``` purescript
newtype GetProjectRequest
  = GetProjectRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get project operation.</p>

##### Instances
``` purescript
Newtype GetProjectRequest _
```

#### `GetProjectResult`

``` purescript
newtype GetProjectResult
  = GetProjectResult { "Project'" :: NullOrUndefined (Project) }
```

<p>Represents the result of a get project request.</p>

##### Instances
``` purescript
Newtype GetProjectResult _
```

#### `GetRemoteAccessSessionRequest`

``` purescript
newtype GetRemoteAccessSessionRequest
  = GetRemoteAccessSessionRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents the request to get information about the specified remote access session.</p>

##### Instances
``` purescript
Newtype GetRemoteAccessSessionRequest _
```

#### `GetRemoteAccessSessionResult`

``` purescript
newtype GetRemoteAccessSessionResult
  = GetRemoteAccessSessionResult { "RemoteAccessSession'" :: NullOrUndefined (RemoteAccessSession) }
```

<p>Represents the response from the server that lists detailed information about the remote access session.</p>

##### Instances
``` purescript
Newtype GetRemoteAccessSessionResult _
```

#### `GetRunRequest`

``` purescript
newtype GetRunRequest
  = GetRunRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get run operation.</p>

##### Instances
``` purescript
Newtype GetRunRequest _
```

#### `GetRunResult`

``` purescript
newtype GetRunResult
  = GetRunResult { "Run'" :: NullOrUndefined (Run) }
```

<p>Represents the result of a get run request.</p>

##### Instances
``` purescript
Newtype GetRunResult _
```

#### `GetSuiteRequest`

``` purescript
newtype GetSuiteRequest
  = GetSuiteRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get suite operation.</p>

##### Instances
``` purescript
Newtype GetSuiteRequest _
```

#### `GetSuiteResult`

``` purescript
newtype GetSuiteResult
  = GetSuiteResult { "Suite'" :: NullOrUndefined (Suite) }
```

<p>Represents the result of a get suite request.</p>

##### Instances
``` purescript
Newtype GetSuiteResult _
```

#### `GetTestRequest`

``` purescript
newtype GetTestRequest
  = GetTestRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get test operation.</p>

##### Instances
``` purescript
Newtype GetTestRequest _
```

#### `GetTestResult`

``` purescript
newtype GetTestResult
  = GetTestResult { "Test'" :: NullOrUndefined (Test) }
```

<p>Represents the result of a get test request.</p>

##### Instances
``` purescript
Newtype GetTestResult _
```

#### `GetUploadRequest`

``` purescript
newtype GetUploadRequest
  = GetUploadRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents a request to the get upload operation.</p>

##### Instances
``` purescript
Newtype GetUploadRequest _
```

#### `GetUploadResult`

``` purescript
newtype GetUploadResult
  = GetUploadResult { "Upload'" :: NullOrUndefined (Upload) }
```

<p>Represents the result of a get upload request.</p>

##### Instances
``` purescript
Newtype GetUploadResult _
```

#### `HostAddress`

``` purescript
newtype HostAddress
  = HostAddress String
```

##### Instances
``` purescript
Newtype HostAddress _
```

#### `IdempotencyException`

``` purescript
newtype IdempotencyException
  = IdempotencyException { "Message'" :: NullOrUndefined (Message) }
```

<p>An entity with the same name already exists.</p>

##### Instances
``` purescript
Newtype IdempotencyException _
```

#### `IncompatibilityMessage`

``` purescript
newtype IncompatibilityMessage
  = IncompatibilityMessage { "Message'" :: NullOrUndefined (Message), "Type'" :: NullOrUndefined (DeviceAttribute) }
```

<p>Represents information about incompatibility.</p>

##### Instances
``` purescript
Newtype IncompatibilityMessage _
```

#### `IncompatibilityMessages`

``` purescript
newtype IncompatibilityMessages
  = IncompatibilityMessages (Array IncompatibilityMessage)
```

##### Instances
``` purescript
Newtype IncompatibilityMessages _
```

#### `InstallToRemoteAccessSessionRequest`

``` purescript
newtype InstallToRemoteAccessSessionRequest
  = InstallToRemoteAccessSessionRequest { "RemoteAccessSessionArn'" :: AmazonResourceName, "AppArn'" :: AmazonResourceName }
```

<p>Represents the request to install an Android application (in .apk format) or an iOS application (in .ipa format) as part of a remote access session.</p>

##### Instances
``` purescript
Newtype InstallToRemoteAccessSessionRequest _
```

#### `InstallToRemoteAccessSessionResult`

``` purescript
newtype InstallToRemoteAccessSessionResult
  = InstallToRemoteAccessSessionResult { "AppUpload'" :: NullOrUndefined (Upload) }
```

<p>Represents the response from the server after AWS Device Farm makes a request to install to a remote access session.</p>

##### Instances
``` purescript
Newtype InstallToRemoteAccessSessionResult _
```

#### `InteractionMode`

``` purescript
newtype InteractionMode
  = InteractionMode String
```

##### Instances
``` purescript
Newtype InteractionMode _
```

#### `IosPaths`

``` purescript
newtype IosPaths
  = IosPaths (Array String)
```

##### Instances
``` purescript
Newtype IosPaths _
```

#### `Job`

``` purescript
newtype Job
  = Job { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Type'" :: NullOrUndefined (TestType), "Created'" :: NullOrUndefined (DateTime), "Status'" :: NullOrUndefined (ExecutionStatus), "Result'" :: NullOrUndefined (ExecutionResult), "Started'" :: NullOrUndefined (DateTime), "Stopped'" :: NullOrUndefined (DateTime), "Counters'" :: NullOrUndefined (Counters), "Message'" :: NullOrUndefined (Message), "Device'" :: NullOrUndefined (Device), "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes) }
```

<p>Represents a device.</p>

##### Instances
``` purescript
Newtype Job _
```

#### `JobTimeoutMinutes`

``` purescript
newtype JobTimeoutMinutes
  = JobTimeoutMinutes Int
```

##### Instances
``` purescript
Newtype JobTimeoutMinutes _
```

#### `Jobs`

``` purescript
newtype Jobs
  = Jobs (Array Job)
```

##### Instances
``` purescript
Newtype Jobs _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (Message) }
```

<p>A limit was exceeded.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListArtifactsRequest`

``` purescript
newtype ListArtifactsRequest
  = ListArtifactsRequest { "Arn'" :: AmazonResourceName, "Type'" :: ArtifactCategory, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list artifacts operation.</p>

##### Instances
``` purescript
Newtype ListArtifactsRequest _
```

#### `ListArtifactsResult`

``` purescript
newtype ListArtifactsResult
  = ListArtifactsResult { "Artifacts'" :: NullOrUndefined (Artifacts), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list artifacts operation.</p>

##### Instances
``` purescript
Newtype ListArtifactsResult _
```

#### `ListDevicePoolsRequest`

``` purescript
newtype ListDevicePoolsRequest
  = ListDevicePoolsRequest { "Arn'" :: AmazonResourceName, "Type'" :: NullOrUndefined (DevicePoolType), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list device pools request.</p>

##### Instances
``` purescript
Newtype ListDevicePoolsRequest _
```

#### `ListDevicePoolsResult`

``` purescript
newtype ListDevicePoolsResult
  = ListDevicePoolsResult { "DevicePools'" :: NullOrUndefined (DevicePools), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list device pools request.</p>

##### Instances
``` purescript
Newtype ListDevicePoolsResult _
```

#### `ListDevicesRequest`

``` purescript
newtype ListDevicesRequest
  = ListDevicesRequest { "Arn'" :: NullOrUndefined (AmazonResourceName), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list devices request.</p>

##### Instances
``` purescript
Newtype ListDevicesRequest _
```

#### `ListDevicesResult`

``` purescript
newtype ListDevicesResult
  = ListDevicesResult { "Devices'" :: NullOrUndefined (Devices), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list devices operation.</p>

##### Instances
``` purescript
Newtype ListDevicesResult _
```

#### `ListJobsRequest`

``` purescript
newtype ListJobsRequest
  = ListJobsRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list jobs operation.</p>

##### Instances
``` purescript
Newtype ListJobsRequest _
```

#### `ListJobsResult`

``` purescript
newtype ListJobsResult
  = ListJobsResult { "Jobs'" :: NullOrUndefined (Jobs), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list jobs request.</p>

##### Instances
``` purescript
Newtype ListJobsResult _
```

#### `ListNetworkProfilesRequest`

``` purescript
newtype ListNetworkProfilesRequest
  = ListNetworkProfilesRequest { "Arn'" :: AmazonResourceName, "Type'" :: NullOrUndefined (NetworkProfileType), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListNetworkProfilesRequest _
```

#### `ListNetworkProfilesResult`

``` purescript
newtype ListNetworkProfilesResult
  = ListNetworkProfilesResult { "NetworkProfiles'" :: NullOrUndefined (NetworkProfiles), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListNetworkProfilesResult _
```

#### `ListOfferingPromotionsRequest`

``` purescript
newtype ListOfferingPromotionsRequest
  = ListOfferingPromotionsRequest { "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListOfferingPromotionsRequest _
```

#### `ListOfferingPromotionsResult`

``` purescript
newtype ListOfferingPromotionsResult
  = ListOfferingPromotionsResult { "OfferingPromotions'" :: NullOrUndefined (OfferingPromotions), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListOfferingPromotionsResult _
```

#### `ListOfferingTransactionsRequest`

``` purescript
newtype ListOfferingTransactionsRequest
  = ListOfferingTransactionsRequest { "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the request to list the offering transaction history.</p>

##### Instances
``` purescript
Newtype ListOfferingTransactionsRequest _
```

#### `ListOfferingTransactionsResult`

``` purescript
newtype ListOfferingTransactionsResult
  = ListOfferingTransactionsResult { "OfferingTransactions'" :: NullOrUndefined (OfferingTransactions), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Returns the transaction log of the specified offerings.</p>

##### Instances
``` purescript
Newtype ListOfferingTransactionsResult _
```

#### `ListOfferingsRequest`

``` purescript
newtype ListOfferingsRequest
  = ListOfferingsRequest { "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the request to list all offerings.</p>

##### Instances
``` purescript
Newtype ListOfferingsRequest _
```

#### `ListOfferingsResult`

``` purescript
newtype ListOfferingsResult
  = ListOfferingsResult { "Offerings'" :: NullOrUndefined (Offerings), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the return values of the list of offerings.</p>

##### Instances
``` purescript
Newtype ListOfferingsResult _
```

#### `ListProjectsRequest`

``` purescript
newtype ListProjectsRequest
  = ListProjectsRequest { "Arn'" :: NullOrUndefined (AmazonResourceName), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list projects operation.</p>

##### Instances
``` purescript
Newtype ListProjectsRequest _
```

#### `ListProjectsResult`

``` purescript
newtype ListProjectsResult
  = ListProjectsResult { "Projects'" :: NullOrUndefined (Projects), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list projects request.</p>

##### Instances
``` purescript
Newtype ListProjectsResult _
```

#### `ListRemoteAccessSessionsRequest`

``` purescript
newtype ListRemoteAccessSessionsRequest
  = ListRemoteAccessSessionsRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the request to return information about the remote access session.</p>

##### Instances
``` purescript
Newtype ListRemoteAccessSessionsRequest _
```

#### `ListRemoteAccessSessionsResult`

``` purescript
newtype ListRemoteAccessSessionsResult
  = ListRemoteAccessSessionsResult { "RemoteAccessSessions'" :: NullOrUndefined (RemoteAccessSessions), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the response from the server after AWS Device Farm makes a request to return information about the remote access session.</p>

##### Instances
``` purescript
Newtype ListRemoteAccessSessionsResult _
```

#### `ListRunsRequest`

``` purescript
newtype ListRunsRequest
  = ListRunsRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list runs operation.</p>

##### Instances
``` purescript
Newtype ListRunsRequest _
```

#### `ListRunsResult`

``` purescript
newtype ListRunsResult
  = ListRunsResult { "Runs'" :: NullOrUndefined (Runs), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list runs request.</p>

##### Instances
``` purescript
Newtype ListRunsResult _
```

#### `ListSamplesRequest`

``` purescript
newtype ListSamplesRequest
  = ListSamplesRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list samples operation.</p>

##### Instances
``` purescript
Newtype ListSamplesRequest _
```

#### `ListSamplesResult`

``` purescript
newtype ListSamplesResult
  = ListSamplesResult { "Samples'" :: NullOrUndefined (Samples), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list samples request.</p>

##### Instances
``` purescript
Newtype ListSamplesResult _
```

#### `ListSuitesRequest`

``` purescript
newtype ListSuitesRequest
  = ListSuitesRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list suites operation.</p>

##### Instances
``` purescript
Newtype ListSuitesRequest _
```

#### `ListSuitesResult`

``` purescript
newtype ListSuitesResult
  = ListSuitesResult { "Suites'" :: NullOrUndefined (Suites), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list suites request.</p>

##### Instances
``` purescript
Newtype ListSuitesResult _
```

#### `ListTestsRequest`

``` purescript
newtype ListTestsRequest
  = ListTestsRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list tests operation.</p>

##### Instances
``` purescript
Newtype ListTestsRequest _
```

#### `ListTestsResult`

``` purescript
newtype ListTestsResult
  = ListTestsResult { "Tests'" :: NullOrUndefined (Tests), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list tests request.</p>

##### Instances
``` purescript
Newtype ListTestsResult _
```

#### `ListUniqueProblemsRequest`

``` purescript
newtype ListUniqueProblemsRequest
  = ListUniqueProblemsRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list unique problems operation.</p>

##### Instances
``` purescript
Newtype ListUniqueProblemsRequest _
```

#### `ListUniqueProblemsResult`

``` purescript
newtype ListUniqueProblemsResult
  = ListUniqueProblemsResult { "UniqueProblems'" :: NullOrUndefined (UniqueProblemsByExecutionResultMap), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list unique problems request.</p>

##### Instances
``` purescript
Newtype ListUniqueProblemsResult _
```

#### `ListUploadsRequest`

``` purescript
newtype ListUploadsRequest
  = ListUploadsRequest { "Arn'" :: AmazonResourceName, "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents a request to the list uploads operation.</p>

##### Instances
``` purescript
Newtype ListUploadsRequest _
```

#### `ListUploadsResult`

``` purescript
newtype ListUploadsResult
  = ListUploadsResult { "Uploads'" :: NullOrUndefined (Uploads), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

<p>Represents the result of a list uploads request.</p>

##### Instances
``` purescript
Newtype ListUploadsResult _
```

#### `Location`

``` purescript
newtype Location
  = Location { "Latitude'" :: Number, "Longitude'" :: Number }
```

<p>Represents a latitude and longitude pair, expressed in geographic coordinate system degrees (for example 47.6204, -122.3491).</p> <p>Elevation is currently not supported.</p>

##### Instances
``` purescript
Newtype Location _
```

#### `MaxSlotMap`

``` purescript
newtype MaxSlotMap
  = MaxSlotMap (Map String Int)
```

##### Instances
``` purescript
Newtype MaxSlotMap _
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

##### Instances
``` purescript
Newtype Message _
```

#### `Metadata`

``` purescript
newtype Metadata
  = Metadata String
```

##### Instances
``` purescript
Newtype Metadata _
```

#### `MonetaryAmount`

``` purescript
newtype MonetaryAmount
  = MonetaryAmount { "Amount'" :: NullOrUndefined (Number), "CurrencyCode'" :: NullOrUndefined (CurrencyCode) }
```

<p>A number representing the monetary amount for an offering or transaction.</p>

##### Instances
``` purescript
Newtype MonetaryAmount _
```

#### `Name`

``` purescript
newtype Name
  = Name String
```

##### Instances
``` purescript
Newtype Name _
```

#### `NetworkProfile`

``` purescript
newtype NetworkProfile
  = NetworkProfile { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Description'" :: NullOrUndefined (Message), "Type'" :: NullOrUndefined (NetworkProfileType), "UplinkBandwidthBits'" :: NullOrUndefined (Number), "DownlinkBandwidthBits'" :: NullOrUndefined (Number), "UplinkDelayMs'" :: NullOrUndefined (Number), "DownlinkDelayMs'" :: NullOrUndefined (Number), "UplinkJitterMs'" :: NullOrUndefined (Number), "DownlinkJitterMs'" :: NullOrUndefined (Number), "UplinkLossPercent'" :: NullOrUndefined (PercentInteger), "DownlinkLossPercent'" :: NullOrUndefined (PercentInteger) }
```

<p>An array of settings that describes characteristics of a network profile.</p>

##### Instances
``` purescript
Newtype NetworkProfile _
```

#### `NetworkProfileType`

``` purescript
newtype NetworkProfileType
  = NetworkProfileType String
```

##### Instances
``` purescript
Newtype NetworkProfileType _
```

#### `NetworkProfiles`

``` purescript
newtype NetworkProfiles
  = NetworkProfiles (Array NetworkProfile)
```

##### Instances
``` purescript
Newtype NetworkProfiles _
```

#### `NotEligibleException`

``` purescript
newtype NotEligibleException
  = NotEligibleException { "Message'" :: NullOrUndefined (Message) }
```

<p>Exception gets thrown when a user is not eligible to perform the specified transaction.</p>

##### Instances
``` purescript
Newtype NotEligibleException _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message'" :: NullOrUndefined (Message) }
```

<p>The specified entity was not found.</p>

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `Offering`

``` purescript
newtype Offering
  = Offering { "Id'" :: NullOrUndefined (OfferingIdentifier), "Description'" :: NullOrUndefined (Message), "Type'" :: NullOrUndefined (OfferingType), "Platform'" :: NullOrUndefined (DevicePlatform), "RecurringCharges'" :: NullOrUndefined (RecurringCharges) }
```

<p>Represents the metadata of a device offering.</p>

##### Instances
``` purescript
Newtype Offering _
```

#### `OfferingIdentifier`

``` purescript
newtype OfferingIdentifier
  = OfferingIdentifier String
```

##### Instances
``` purescript
Newtype OfferingIdentifier _
```

#### `OfferingPromotion`

``` purescript
newtype OfferingPromotion
  = OfferingPromotion { "Id'" :: NullOrUndefined (OfferingPromotionIdentifier), "Description'" :: NullOrUndefined (Message) }
```

<p>Represents information about an offering promotion.</p>

##### Instances
``` purescript
Newtype OfferingPromotion _
```

#### `OfferingPromotionIdentifier`

``` purescript
newtype OfferingPromotionIdentifier
  = OfferingPromotionIdentifier String
```

##### Instances
``` purescript
Newtype OfferingPromotionIdentifier _
```

#### `OfferingPromotions`

``` purescript
newtype OfferingPromotions
  = OfferingPromotions (Array OfferingPromotion)
```

##### Instances
``` purescript
Newtype OfferingPromotions _
```

#### `OfferingStatus`

``` purescript
newtype OfferingStatus
  = OfferingStatus { "Type'" :: NullOrUndefined (OfferingTransactionType), "Offering'" :: NullOrUndefined (Offering), "Quantity'" :: NullOrUndefined (Int), "EffectiveOn'" :: NullOrUndefined (DateTime) }
```

<p>The status of the offering.</p>

##### Instances
``` purescript
Newtype OfferingStatus _
```

#### `OfferingStatusMap`

``` purescript
newtype OfferingStatusMap
  = OfferingStatusMap (Map OfferingIdentifier OfferingStatus)
```

##### Instances
``` purescript
Newtype OfferingStatusMap _
```

#### `OfferingTransaction`

``` purescript
newtype OfferingTransaction
  = OfferingTransaction { "OfferingStatus'" :: NullOrUndefined (OfferingStatus), "TransactionId'" :: NullOrUndefined (TransactionIdentifier), "OfferingPromotionId'" :: NullOrUndefined (OfferingPromotionIdentifier), "CreatedOn'" :: NullOrUndefined (DateTime), "Cost'" :: NullOrUndefined (MonetaryAmount) }
```

<p>Represents the metadata of an offering transaction.</p>

##### Instances
``` purescript
Newtype OfferingTransaction _
```

#### `OfferingTransactionType`

``` purescript
newtype OfferingTransactionType
  = OfferingTransactionType String
```

##### Instances
``` purescript
Newtype OfferingTransactionType _
```

#### `OfferingTransactions`

``` purescript
newtype OfferingTransactions
  = OfferingTransactions (Array OfferingTransaction)
```

##### Instances
``` purescript
Newtype OfferingTransactions _
```

#### `OfferingType`

``` purescript
newtype OfferingType
  = OfferingType String
```

##### Instances
``` purescript
Newtype OfferingType _
```

#### `Offerings`

``` purescript
newtype Offerings
  = Offerings (Array Offering)
```

##### Instances
``` purescript
Newtype Offerings _
```

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

##### Instances
``` purescript
Newtype PaginationToken _
```

#### `PercentInteger`

``` purescript
newtype PercentInteger
  = PercentInteger Int
```

##### Instances
``` purescript
Newtype PercentInteger _
```

#### `Problem`

``` purescript
newtype Problem
  = Problem { "Run'" :: NullOrUndefined (ProblemDetail), "Job'" :: NullOrUndefined (ProblemDetail), "Suite'" :: NullOrUndefined (ProblemDetail), "Test'" :: NullOrUndefined (ProblemDetail), "Device'" :: NullOrUndefined (Device), "Result'" :: NullOrUndefined (ExecutionResult), "Message'" :: NullOrUndefined (Message) }
```

<p>Represents a specific warning or failure.</p>

##### Instances
``` purescript
Newtype Problem _
```

#### `ProblemDetail`

``` purescript
newtype ProblemDetail
  = ProblemDetail { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name) }
```

<p>Information about a problem detail.</p>

##### Instances
``` purescript
Newtype ProblemDetail _
```

#### `Problems`

``` purescript
newtype Problems
  = Problems (Array Problem)
```

##### Instances
``` purescript
Newtype Problems _
```

#### `Project`

``` purescript
newtype Project
  = Project { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes), "Created'" :: NullOrUndefined (DateTime) }
```

<p>Represents an operating-system neutral workspace for running and managing tests.</p>

##### Instances
``` purescript
Newtype Project _
```

#### `Projects`

``` purescript
newtype Projects
  = Projects (Array Project)
```

##### Instances
``` purescript
Newtype Projects _
```

#### `PurchaseOfferingRequest`

``` purescript
newtype PurchaseOfferingRequest
  = PurchaseOfferingRequest { "OfferingId'" :: NullOrUndefined (OfferingIdentifier), "Quantity'" :: NullOrUndefined (Int), "OfferingPromotionId'" :: NullOrUndefined (OfferingPromotionIdentifier) }
```

<p>Represents a request for a purchase offering.</p>

##### Instances
``` purescript
Newtype PurchaseOfferingRequest _
```

#### `PurchaseOfferingResult`

``` purescript
newtype PurchaseOfferingResult
  = PurchaseOfferingResult { "OfferingTransaction'" :: NullOrUndefined (OfferingTransaction) }
```

<p>The result of the purchase offering (e.g., success or failure).</p>

##### Instances
``` purescript
Newtype PurchaseOfferingResult _
```

#### `PurchasedDevicesMap`

``` purescript
newtype PurchasedDevicesMap
  = PurchasedDevicesMap (Map DevicePlatform Int)
```

##### Instances
``` purescript
Newtype PurchasedDevicesMap _
```

#### `Radios`

``` purescript
newtype Radios
  = Radios { "Wifi'" :: NullOrUndefined (Boolean), "Bluetooth'" :: NullOrUndefined (Boolean), "Nfc'" :: NullOrUndefined (Boolean), "Gps'" :: NullOrUndefined (Boolean) }
```

<p>Represents the set of radios and their states on a device. Examples of radios include Wi-Fi, GPS, Bluetooth, and NFC.</p>

##### Instances
``` purescript
Newtype Radios _
```

#### `RecurringCharge`

``` purescript
newtype RecurringCharge
  = RecurringCharge { "Cost'" :: NullOrUndefined (MonetaryAmount), "Frequency'" :: NullOrUndefined (RecurringChargeFrequency) }
```

<p>Specifies whether charges for devices will be recurring.</p>

##### Instances
``` purescript
Newtype RecurringCharge _
```

#### `RecurringChargeFrequency`

``` purescript
newtype RecurringChargeFrequency
  = RecurringChargeFrequency String
```

##### Instances
``` purescript
Newtype RecurringChargeFrequency _
```

#### `RecurringCharges`

``` purescript
newtype RecurringCharges
  = RecurringCharges (Array RecurringCharge)
```

##### Instances
``` purescript
Newtype RecurringCharges _
```

#### `RemoteAccessSession`

``` purescript
newtype RemoteAccessSession
  = RemoteAccessSession { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Created'" :: NullOrUndefined (DateTime), "Status'" :: NullOrUndefined (ExecutionStatus), "Result'" :: NullOrUndefined (ExecutionResult), "Message'" :: NullOrUndefined (Message), "Started'" :: NullOrUndefined (DateTime), "Stopped'" :: NullOrUndefined (DateTime), "Device'" :: NullOrUndefined (Device), "RemoteDebugEnabled'" :: NullOrUndefined (Boolean), "RemoteRecordEnabled'" :: NullOrUndefined (Boolean), "RemoteRecordAppArn'" :: NullOrUndefined (AmazonResourceName), "HostAddress'" :: NullOrUndefined (HostAddress), "ClientId'" :: NullOrUndefined (ClientId), "BillingMethod'" :: NullOrUndefined (BillingMethod), "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes), "Endpoint'" :: NullOrUndefined (String), "DeviceUdid'" :: NullOrUndefined (String), "InteractionMode'" :: NullOrUndefined (InteractionMode) }
```

<p>Represents information about the remote access session.</p>

##### Instances
``` purescript
Newtype RemoteAccessSession _
```

#### `RemoteAccessSessions`

``` purescript
newtype RemoteAccessSessions
  = RemoteAccessSessions (Array RemoteAccessSession)
```

##### Instances
``` purescript
Newtype RemoteAccessSessions _
```

#### `RenewOfferingRequest`

``` purescript
newtype RenewOfferingRequest
  = RenewOfferingRequest { "OfferingId'" :: NullOrUndefined (OfferingIdentifier), "Quantity'" :: NullOrUndefined (Int) }
```

<p>A request representing an offering renewal.</p>

##### Instances
``` purescript
Newtype RenewOfferingRequest _
```

#### `RenewOfferingResult`

``` purescript
newtype RenewOfferingResult
  = RenewOfferingResult { "OfferingTransaction'" :: NullOrUndefined (OfferingTransaction) }
```

<p>The result of a renewal offering.</p>

##### Instances
``` purescript
Newtype RenewOfferingResult _
```

#### `Resolution`

``` purescript
newtype Resolution
  = Resolution { "Width'" :: NullOrUndefined (Int), "Height'" :: NullOrUndefined (Int) }
```

<p>Represents the screen resolution of a device in height and width, expressed in pixels.</p>

##### Instances
``` purescript
Newtype Resolution _
```

#### `Rule`

``` purescript
newtype Rule
  = Rule { "Attribute'" :: NullOrUndefined (DeviceAttribute), "Operator'" :: NullOrUndefined (RuleOperator), "Value'" :: NullOrUndefined (String) }
```

<p>Represents a condition for a device pool.</p>

##### Instances
``` purescript
Newtype Rule _
```

#### `RuleOperator`

``` purescript
newtype RuleOperator
  = RuleOperator String
```

##### Instances
``` purescript
Newtype RuleOperator _
```

#### `Rules`

``` purescript
newtype Rules
  = Rules (Array Rule)
```

##### Instances
``` purescript
Newtype Rules _
```

#### `Run`

``` purescript
newtype Run
  = Run { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Type'" :: NullOrUndefined (TestType), "Platform'" :: NullOrUndefined (DevicePlatform), "Created'" :: NullOrUndefined (DateTime), "Status'" :: NullOrUndefined (ExecutionStatus), "Result'" :: NullOrUndefined (ExecutionResult), "Started'" :: NullOrUndefined (DateTime), "Stopped'" :: NullOrUndefined (DateTime), "Counters'" :: NullOrUndefined (Counters), "Message'" :: NullOrUndefined (Message), "TotalJobs'" :: NullOrUndefined (Int), "CompletedJobs'" :: NullOrUndefined (Int), "BillingMethod'" :: NullOrUndefined (BillingMethod), "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes), "NetworkProfile'" :: NullOrUndefined (NetworkProfile), "ParsingResultUrl'" :: NullOrUndefined (String), "ResultCode'" :: NullOrUndefined (ExecutionResultCode), "Seed'" :: NullOrUndefined (Int), "AppUpload'" :: NullOrUndefined (AmazonResourceName), "EventCount'" :: NullOrUndefined (Int), "JobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes), "DevicePoolArn'" :: NullOrUndefined (AmazonResourceName), "Locale'" :: NullOrUndefined (String), "Radios'" :: NullOrUndefined (Radios), "Location'" :: NullOrUndefined (Location), "CustomerArtifactPaths'" :: NullOrUndefined (CustomerArtifactPaths), "WebUrl'" :: NullOrUndefined (String) }
```

<p>Represents a test run on a set of devices with a given app package, test parameters, etc.</p>

##### Instances
``` purescript
Newtype Run _
```

#### `Runs`

``` purescript
newtype Runs
  = Runs (Array Run)
```

##### Instances
``` purescript
Newtype Runs _
```

#### `Sample`

``` purescript
newtype Sample
  = Sample { "Arn'" :: NullOrUndefined (AmazonResourceName), "Type'" :: NullOrUndefined (SampleType), "Url'" :: NullOrUndefined (URL) }
```

<p>Represents a sample of performance data.</p>

##### Instances
``` purescript
Newtype Sample _
```

#### `SampleType`

``` purescript
newtype SampleType
  = SampleType String
```

##### Instances
``` purescript
Newtype SampleType _
```

#### `Samples`

``` purescript
newtype Samples
  = Samples (Array Sample)
```

##### Instances
``` purescript
Newtype Samples _
```

#### `ScheduleRunConfiguration`

``` purescript
newtype ScheduleRunConfiguration
  = ScheduleRunConfiguration { "ExtraDataPackageArn'" :: NullOrUndefined (AmazonResourceName), "NetworkProfileArn'" :: NullOrUndefined (AmazonResourceName), "Locale'" :: NullOrUndefined (String), "Location'" :: NullOrUndefined (Location), "CustomerArtifactPaths'" :: NullOrUndefined (CustomerArtifactPaths), "Radios'" :: NullOrUndefined (Radios), "AuxiliaryApps'" :: NullOrUndefined (AmazonResourceNames), "BillingMethod'" :: NullOrUndefined (BillingMethod) }
```

<p>Represents the settings for a run. Includes things like location, radio states, auxiliary apps, and network profiles.</p>

##### Instances
``` purescript
Newtype ScheduleRunConfiguration _
```

#### `ScheduleRunRequest`

``` purescript
newtype ScheduleRunRequest
  = ScheduleRunRequest { "ProjectArn'" :: AmazonResourceName, "AppArn'" :: NullOrUndefined (AmazonResourceName), "DevicePoolArn'" :: AmazonResourceName, "Name'" :: NullOrUndefined (Name), "Test'" :: ScheduleRunTest, "Configuration'" :: NullOrUndefined (ScheduleRunConfiguration), "ExecutionConfiguration'" :: NullOrUndefined (ExecutionConfiguration) }
```

<p>Represents a request to the schedule run operation.</p>

##### Instances
``` purescript
Newtype ScheduleRunRequest _
```

#### `ScheduleRunResult`

``` purescript
newtype ScheduleRunResult
  = ScheduleRunResult { "Run'" :: NullOrUndefined (Run) }
```

<p>Represents the result of a schedule run request.</p>

##### Instances
``` purescript
Newtype ScheduleRunResult _
```

#### `ScheduleRunTest`

``` purescript
newtype ScheduleRunTest
  = ScheduleRunTest { "Type'" :: TestType, "TestPackageArn'" :: NullOrUndefined (AmazonResourceName), "Filter'" :: NullOrUndefined (Filter), "Parameters'" :: NullOrUndefined (TestParameters) }
```

<p>Represents additional test settings.</p>

##### Instances
``` purescript
Newtype ScheduleRunTest _
```

#### `ServiceAccountException`

``` purescript
newtype ServiceAccountException
  = ServiceAccountException { "Message'" :: NullOrUndefined (Message) }
```

<p>There was a problem with the service account.</p>

##### Instances
``` purescript
Newtype ServiceAccountException _
```

#### `SshPublicKey`

``` purescript
newtype SshPublicKey
  = SshPublicKey String
```

##### Instances
``` purescript
Newtype SshPublicKey _
```

#### `StopRemoteAccessSessionRequest`

``` purescript
newtype StopRemoteAccessSessionRequest
  = StopRemoteAccessSessionRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents the request to stop the remote access session.</p>

##### Instances
``` purescript
Newtype StopRemoteAccessSessionRequest _
```

#### `StopRemoteAccessSessionResult`

``` purescript
newtype StopRemoteAccessSessionResult
  = StopRemoteAccessSessionResult { "RemoteAccessSession'" :: NullOrUndefined (RemoteAccessSession) }
```

<p>Represents the response from the server that describes the remote access session when AWS Device Farm stops the session.</p>

##### Instances
``` purescript
Newtype StopRemoteAccessSessionResult _
```

#### `StopRunRequest`

``` purescript
newtype StopRunRequest
  = StopRunRequest { "Arn'" :: AmazonResourceName }
```

<p>Represents the request to stop a specific run.</p>

##### Instances
``` purescript
Newtype StopRunRequest _
```

#### `StopRunResult`

``` purescript
newtype StopRunResult
  = StopRunResult { "Run'" :: NullOrUndefined (Run) }
```

<p>Represents the results of your stop run attempt.</p>

##### Instances
``` purescript
Newtype StopRunResult _
```

#### `Suite`

``` purescript
newtype Suite
  = Suite { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Type'" :: NullOrUndefined (TestType), "Created'" :: NullOrUndefined (DateTime), "Status'" :: NullOrUndefined (ExecutionStatus), "Result'" :: NullOrUndefined (ExecutionResult), "Started'" :: NullOrUndefined (DateTime), "Stopped'" :: NullOrUndefined (DateTime), "Counters'" :: NullOrUndefined (Counters), "Message'" :: NullOrUndefined (Message), "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes) }
```

<p>Represents a collection of one or more tests.</p>

##### Instances
``` purescript
Newtype Suite _
```

#### `Suites`

``` purescript
newtype Suites
  = Suites (Array Suite)
```

##### Instances
``` purescript
Newtype Suites _
```

#### `Test`

``` purescript
newtype Test
  = Test { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Type'" :: NullOrUndefined (TestType), "Created'" :: NullOrUndefined (DateTime), "Status'" :: NullOrUndefined (ExecutionStatus), "Result'" :: NullOrUndefined (ExecutionResult), "Started'" :: NullOrUndefined (DateTime), "Stopped'" :: NullOrUndefined (DateTime), "Counters'" :: NullOrUndefined (Counters), "Message'" :: NullOrUndefined (Message), "DeviceMinutes'" :: NullOrUndefined (DeviceMinutes) }
```

<p>Represents a condition that is evaluated.</p>

##### Instances
``` purescript
Newtype Test _
```

#### `TestParameters`

``` purescript
newtype TestParameters
  = TestParameters (Map String String)
```

##### Instances
``` purescript
Newtype TestParameters _
```

#### `TestType`

``` purescript
newtype TestType
  = TestType String
```

##### Instances
``` purescript
Newtype TestType _
```

#### `Tests`

``` purescript
newtype Tests
  = Tests (Array Test)
```

##### Instances
``` purescript
Newtype Tests _
```

#### `TransactionIdentifier`

``` purescript
newtype TransactionIdentifier
  = TransactionIdentifier String
```

##### Instances
``` purescript
Newtype TransactionIdentifier _
```

#### `TrialMinutes`

``` purescript
newtype TrialMinutes
  = TrialMinutes { "Total'" :: NullOrUndefined (Number), "Remaining'" :: NullOrUndefined (Number) }
```

<p>Represents information about free trial device minutes for an AWS account.</p>

##### Instances
``` purescript
Newtype TrialMinutes _
```

#### `URL`

``` purescript
newtype URL
  = URL String
```

##### Instances
``` purescript
Newtype URL _
```

#### `UniqueProblem`

``` purescript
newtype UniqueProblem
  = UniqueProblem { "Message'" :: NullOrUndefined (Message), "Problems'" :: NullOrUndefined (Problems) }
```

<p>A collection of one or more problems, grouped by their result.</p>

##### Instances
``` purescript
Newtype UniqueProblem _
```

#### `UniqueProblems`

``` purescript
newtype UniqueProblems
  = UniqueProblems (Array UniqueProblem)
```

##### Instances
``` purescript
Newtype UniqueProblems _
```

#### `UniqueProblemsByExecutionResultMap`

``` purescript
newtype UniqueProblemsByExecutionResultMap
  = UniqueProblemsByExecutionResultMap (Map ExecutionResult UniqueProblems)
```

##### Instances
``` purescript
Newtype UniqueProblemsByExecutionResultMap _
```

#### `UpdateDevicePoolRequest`

``` purescript
newtype UpdateDevicePoolRequest
  = UpdateDevicePoolRequest { "Arn'" :: AmazonResourceName, "Name'" :: NullOrUndefined (Name), "Description'" :: NullOrUndefined (Message), "Rules'" :: NullOrUndefined (Rules) }
```

<p>Represents a request to the update device pool operation.</p>

##### Instances
``` purescript
Newtype UpdateDevicePoolRequest _
```

#### `UpdateDevicePoolResult`

``` purescript
newtype UpdateDevicePoolResult
  = UpdateDevicePoolResult { "DevicePool'" :: NullOrUndefined (DevicePool) }
```

<p>Represents the result of an update device pool request.</p>

##### Instances
``` purescript
Newtype UpdateDevicePoolResult _
```

#### `UpdateNetworkProfileRequest`

``` purescript
newtype UpdateNetworkProfileRequest
  = UpdateNetworkProfileRequest { "Arn'" :: AmazonResourceName, "Name'" :: NullOrUndefined (Name), "Description'" :: NullOrUndefined (Message), "Type'" :: NullOrUndefined (NetworkProfileType), "UplinkBandwidthBits'" :: NullOrUndefined (Number), "DownlinkBandwidthBits'" :: NullOrUndefined (Number), "UplinkDelayMs'" :: NullOrUndefined (Number), "DownlinkDelayMs'" :: NullOrUndefined (Number), "UplinkJitterMs'" :: NullOrUndefined (Number), "DownlinkJitterMs'" :: NullOrUndefined (Number), "UplinkLossPercent'" :: NullOrUndefined (PercentInteger), "DownlinkLossPercent'" :: NullOrUndefined (PercentInteger) }
```

##### Instances
``` purescript
Newtype UpdateNetworkProfileRequest _
```

#### `UpdateNetworkProfileResult`

``` purescript
newtype UpdateNetworkProfileResult
  = UpdateNetworkProfileResult { "NetworkProfile'" :: NullOrUndefined (NetworkProfile) }
```

##### Instances
``` purescript
Newtype UpdateNetworkProfileResult _
```

#### `UpdateProjectRequest`

``` purescript
newtype UpdateProjectRequest
  = UpdateProjectRequest { "Arn'" :: AmazonResourceName, "Name'" :: NullOrUndefined (Name), "DefaultJobTimeoutMinutes'" :: NullOrUndefined (JobTimeoutMinutes) }
```

<p>Represents a request to the update project operation.</p>

##### Instances
``` purescript
Newtype UpdateProjectRequest _
```

#### `UpdateProjectResult`

``` purescript
newtype UpdateProjectResult
  = UpdateProjectResult { "Project'" :: NullOrUndefined (Project) }
```

<p>Represents the result of an update project request.</p>

##### Instances
``` purescript
Newtype UpdateProjectResult _
```

#### `Upload`

``` purescript
newtype Upload
  = Upload { "Arn'" :: NullOrUndefined (AmazonResourceName), "Name'" :: NullOrUndefined (Name), "Created'" :: NullOrUndefined (DateTime), "Type'" :: NullOrUndefined (UploadType), "Status'" :: NullOrUndefined (UploadStatus), "Url'" :: NullOrUndefined (URL), "Metadata'" :: NullOrUndefined (Metadata), "ContentType'" :: NullOrUndefined (ContentType), "Message'" :: NullOrUndefined (Message) }
```

<p>An app or a set of one or more tests to upload or that have been uploaded.</p>

##### Instances
``` purescript
Newtype Upload _
```

#### `UploadStatus`

``` purescript
newtype UploadStatus
  = UploadStatus String
```

##### Instances
``` purescript
Newtype UploadStatus _
```

#### `UploadType`

``` purescript
newtype UploadType
  = UploadType String
```

##### Instances
``` purescript
Newtype UploadType _
```

#### `Uploads`

``` purescript
newtype Uploads
  = Uploads (Array Upload)
```

##### Instances
``` purescript
Newtype Uploads _
```


