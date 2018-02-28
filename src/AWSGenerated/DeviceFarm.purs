

-- | <p>AWS Device Farm is a service that enables mobile app developers to test Android, iOS, and Fire OS apps on physical phones, tablets, and other devices in the cloud.</p>
module AWS.DeviceFarm where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined as NullOrUndefined
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap

import AWS.Request as Request
import AWS.Request.Types as Types

serviceName = "DeviceFarm" :: String


-- | <p>Creates a device pool.</p>
createDevicePool :: forall eff. CreateDevicePoolRequest -> Aff (exception :: EXCEPTION | eff) CreateDevicePoolResult
createDevicePool = Request.request serviceName "createDevicePool" 


-- | <p>Creates a network profile.</p>
createNetworkProfile :: forall eff. CreateNetworkProfileRequest -> Aff (exception :: EXCEPTION | eff) CreateNetworkProfileResult
createNetworkProfile = Request.request serviceName "createNetworkProfile" 


-- | <p>Creates a new project.</p>
createProject :: forall eff. CreateProjectRequest -> Aff (exception :: EXCEPTION | eff) CreateProjectResult
createProject = Request.request serviceName "createProject" 


-- | <p>Specifies and starts a remote access session.</p>
createRemoteAccessSession :: forall eff. CreateRemoteAccessSessionRequest -> Aff (exception :: EXCEPTION | eff) CreateRemoteAccessSessionResult
createRemoteAccessSession = Request.request serviceName "createRemoteAccessSession" 


-- | <p>Uploads an app or test scripts.</p>
createUpload :: forall eff. CreateUploadRequest -> Aff (exception :: EXCEPTION | eff) CreateUploadResult
createUpload = Request.request serviceName "createUpload" 


-- | <p>Deletes a device pool given the pool ARN. Does not allow deletion of curated pools owned by the system.</p>
deleteDevicePool :: forall eff. DeleteDevicePoolRequest -> Aff (exception :: EXCEPTION | eff) DeleteDevicePoolResult
deleteDevicePool = Request.request serviceName "deleteDevicePool" 


-- | <p>Deletes a network profile.</p>
deleteNetworkProfile :: forall eff. DeleteNetworkProfileRequest -> Aff (exception :: EXCEPTION | eff) DeleteNetworkProfileResult
deleteNetworkProfile = Request.request serviceName "deleteNetworkProfile" 


-- | <p>Deletes an AWS Device Farm project, given the project ARN.</p> <p> <b>Note</b> Deleting this resource does not stop an in-progress run.</p>
deleteProject :: forall eff. DeleteProjectRequest -> Aff (exception :: EXCEPTION | eff) DeleteProjectResult
deleteProject = Request.request serviceName "deleteProject" 


-- | <p>Deletes a completed remote access session and its results.</p>
deleteRemoteAccessSession :: forall eff. DeleteRemoteAccessSessionRequest -> Aff (exception :: EXCEPTION | eff) DeleteRemoteAccessSessionResult
deleteRemoteAccessSession = Request.request serviceName "deleteRemoteAccessSession" 


-- | <p>Deletes the run, given the run ARN.</p> <p> <b>Note</b> Deleting this resource does not stop an in-progress run.</p>
deleteRun :: forall eff. DeleteRunRequest -> Aff (exception :: EXCEPTION | eff) DeleteRunResult
deleteRun = Request.request serviceName "deleteRun" 


-- | <p>Deletes an upload given the upload ARN.</p>
deleteUpload :: forall eff. DeleteUploadRequest -> Aff (exception :: EXCEPTION | eff) DeleteUploadResult
deleteUpload = Request.request serviceName "deleteUpload" 


-- | <p>Returns the number of unmetered iOS and/or unmetered Android devices that have been purchased by the account.</p>
getAccountSettings :: forall eff. GetAccountSettingsRequest -> Aff (exception :: EXCEPTION | eff) GetAccountSettingsResult
getAccountSettings = Request.request serviceName "getAccountSettings" 


-- | <p>Gets information about a unique device type.</p>
getDevice :: forall eff. GetDeviceRequest -> Aff (exception :: EXCEPTION | eff) GetDeviceResult
getDevice = Request.request serviceName "getDevice" 


-- | <p>Gets information about a device pool.</p>
getDevicePool :: forall eff. GetDevicePoolRequest -> Aff (exception :: EXCEPTION | eff) GetDevicePoolResult
getDevicePool = Request.request serviceName "getDevicePool" 


-- | <p>Gets information about compatibility with a device pool.</p>
getDevicePoolCompatibility :: forall eff. GetDevicePoolCompatibilityRequest -> Aff (exception :: EXCEPTION | eff) GetDevicePoolCompatibilityResult
getDevicePoolCompatibility = Request.request serviceName "getDevicePoolCompatibility" 


-- | <p>Gets information about a job.</p>
getJob :: forall eff. GetJobRequest -> Aff (exception :: EXCEPTION | eff) GetJobResult
getJob = Request.request serviceName "getJob" 


-- | <p>Returns information about a network profile.</p>
getNetworkProfile :: forall eff. GetNetworkProfileRequest -> Aff (exception :: EXCEPTION | eff) GetNetworkProfileResult
getNetworkProfile = Request.request serviceName "getNetworkProfile" 


-- | <p>Gets the current status and future status of all offerings purchased by an AWS account. The response indicates how many offerings are currently available and the offerings that will be available in the next period. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
getOfferingStatus :: forall eff. GetOfferingStatusRequest -> Aff (exception :: EXCEPTION | eff) GetOfferingStatusResult
getOfferingStatus = Request.request serviceName "getOfferingStatus" 


-- | <p>Gets information about a project.</p>
getProject :: forall eff. GetProjectRequest -> Aff (exception :: EXCEPTION | eff) GetProjectResult
getProject = Request.request serviceName "getProject" 


-- | <p>Returns a link to a currently running remote access session.</p>
getRemoteAccessSession :: forall eff. GetRemoteAccessSessionRequest -> Aff (exception :: EXCEPTION | eff) GetRemoteAccessSessionResult
getRemoteAccessSession = Request.request serviceName "getRemoteAccessSession" 


-- | <p>Gets information about a run.</p>
getRun :: forall eff. GetRunRequest -> Aff (exception :: EXCEPTION | eff) GetRunResult
getRun = Request.request serviceName "getRun" 


-- | <p>Gets information about a suite.</p>
getSuite :: forall eff. GetSuiteRequest -> Aff (exception :: EXCEPTION | eff) GetSuiteResult
getSuite = Request.request serviceName "getSuite" 


-- | <p>Gets information about a test.</p>
getTest :: forall eff. GetTestRequest -> Aff (exception :: EXCEPTION | eff) GetTestResult
getTest = Request.request serviceName "getTest" 


-- | <p>Gets information about an upload.</p>
getUpload :: forall eff. GetUploadRequest -> Aff (exception :: EXCEPTION | eff) GetUploadResult
getUpload = Request.request serviceName "getUpload" 


-- | <p>Installs an application to the device in a remote access session. For Android applications, the file must be in .apk format. For iOS applications, the file must be in .ipa format.</p>
installToRemoteAccessSession :: forall eff. InstallToRemoteAccessSessionRequest -> Aff (exception :: EXCEPTION | eff) InstallToRemoteAccessSessionResult
installToRemoteAccessSession = Request.request serviceName "installToRemoteAccessSession" 


-- | <p>Gets information about artifacts.</p>
listArtifacts :: forall eff. ListArtifactsRequest -> Aff (exception :: EXCEPTION | eff) ListArtifactsResult
listArtifacts = Request.request serviceName "listArtifacts" 


-- | <p>Gets information about device pools.</p>
listDevicePools :: forall eff. ListDevicePoolsRequest -> Aff (exception :: EXCEPTION | eff) ListDevicePoolsResult
listDevicePools = Request.request serviceName "listDevicePools" 


-- | <p>Gets information about unique device types.</p>
listDevices :: forall eff. ListDevicesRequest -> Aff (exception :: EXCEPTION | eff) ListDevicesResult
listDevices = Request.request serviceName "listDevices" 


-- | <p>Gets information about jobs for a given test run.</p>
listJobs :: forall eff. ListJobsRequest -> Aff (exception :: EXCEPTION | eff) ListJobsResult
listJobs = Request.request serviceName "listJobs" 


-- | <p>Returns the list of available network profiles.</p>
listNetworkProfiles :: forall eff. ListNetworkProfilesRequest -> Aff (exception :: EXCEPTION | eff) ListNetworkProfilesResult
listNetworkProfiles = Request.request serviceName "listNetworkProfiles" 


-- | <p>Returns a list of offering promotions. Each offering promotion record contains the ID and description of the promotion. The API returns a <code>NotEligible</code> error if the caller is not permitted to invoke the operation. Contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
listOfferingPromotions :: forall eff. ListOfferingPromotionsRequest -> Aff (exception :: EXCEPTION | eff) ListOfferingPromotionsResult
listOfferingPromotions = Request.request serviceName "listOfferingPromotions" 


-- | <p>Returns a list of all historical purchases, renewals, and system renewal transactions for an AWS account. The list is paginated and ordered by a descending timestamp (most recent transactions are first). The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
listOfferingTransactions :: forall eff. ListOfferingTransactionsRequest -> Aff (exception :: EXCEPTION | eff) ListOfferingTransactionsResult
listOfferingTransactions = Request.request serviceName "listOfferingTransactions" 


-- | <p>Returns a list of products or offerings that the user can manage through the API. Each offering record indicates the recurring price per unit and the frequency for that offering. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
listOfferings :: forall eff. ListOfferingsRequest -> Aff (exception :: EXCEPTION | eff) ListOfferingsResult
listOfferings = Request.request serviceName "listOfferings" 


-- | <p>Gets information about projects.</p>
listProjects :: forall eff. ListProjectsRequest -> Aff (exception :: EXCEPTION | eff) ListProjectsResult
listProjects = Request.request serviceName "listProjects" 


-- | <p>Returns a list of all currently running remote access sessions.</p>
listRemoteAccessSessions :: forall eff. ListRemoteAccessSessionsRequest -> Aff (exception :: EXCEPTION | eff) ListRemoteAccessSessionsResult
listRemoteAccessSessions = Request.request serviceName "listRemoteAccessSessions" 


-- | <p>Gets information about runs, given an AWS Device Farm project ARN.</p>
listRuns :: forall eff. ListRunsRequest -> Aff (exception :: EXCEPTION | eff) ListRunsResult
listRuns = Request.request serviceName "listRuns" 


-- | <p>Gets information about samples, given an AWS Device Farm project ARN</p>
listSamples :: forall eff. ListSamplesRequest -> Aff (exception :: EXCEPTION | eff) ListSamplesResult
listSamples = Request.request serviceName "listSamples" 


-- | <p>Gets information about test suites for a given job.</p>
listSuites :: forall eff. ListSuitesRequest -> Aff (exception :: EXCEPTION | eff) ListSuitesResult
listSuites = Request.request serviceName "listSuites" 


-- | <p>Gets information about tests in a given test suite.</p>
listTests :: forall eff. ListTestsRequest -> Aff (exception :: EXCEPTION | eff) ListTestsResult
listTests = Request.request serviceName "listTests" 


-- | <p>Gets information about unique problems.</p>
listUniqueProblems :: forall eff. ListUniqueProblemsRequest -> Aff (exception :: EXCEPTION | eff) ListUniqueProblemsResult
listUniqueProblems = Request.request serviceName "listUniqueProblems" 


-- | <p>Gets information about uploads, given an AWS Device Farm project ARN.</p>
listUploads :: forall eff. ListUploadsRequest -> Aff (exception :: EXCEPTION | eff) ListUploadsResult
listUploads = Request.request serviceName "listUploads" 


-- | <p>Immediately purchases offerings for an AWS account. Offerings renew with the latest total purchased quantity for an offering, unless the renewal was overridden. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
purchaseOffering :: forall eff. PurchaseOfferingRequest -> Aff (exception :: EXCEPTION | eff) PurchaseOfferingResult
purchaseOffering = Request.request serviceName "purchaseOffering" 


-- | <p>Explicitly sets the quantity of devices to renew for an offering, starting from the <code>effectiveDate</code> of the next period. The API returns a <code>NotEligible</code> error if the user is not permitted to invoke the operation. Please contact <a href="mailto:aws-devicefarm-support@amazon.com">aws-devicefarm-support@amazon.com</a> if you believe that you should be able to invoke this operation.</p>
renewOffering :: forall eff. RenewOfferingRequest -> Aff (exception :: EXCEPTION | eff) RenewOfferingResult
renewOffering = Request.request serviceName "renewOffering" 


-- | <p>Schedules a run.</p>
scheduleRun :: forall eff. ScheduleRunRequest -> Aff (exception :: EXCEPTION | eff) ScheduleRunResult
scheduleRun = Request.request serviceName "scheduleRun" 


-- | <p>Ends a specified remote access session.</p>
stopRemoteAccessSession :: forall eff. StopRemoteAccessSessionRequest -> Aff (exception :: EXCEPTION | eff) StopRemoteAccessSessionResult
stopRemoteAccessSession = Request.request serviceName "stopRemoteAccessSession" 


-- | <p>Initiates a stop request for the current test run. AWS Device Farm will immediately stop the run on devices where tests have not started executing, and you will not be billed for these devices. On devices where tests have started executing, Setup Suite and Teardown Suite tests will run to completion before stopping execution on those devices. You will be billed for Setup, Teardown, and any tests that were in progress or already completed.</p>
stopRun :: forall eff. StopRunRequest -> Aff (exception :: EXCEPTION | eff) StopRunResult
stopRun = Request.request serviceName "stopRun" 


-- | <p>Modifies the name, description, and rules in a device pool given the attributes and the pool ARN. Rule updates are all-or-nothing, meaning they can only be updated as a whole (or not at all).</p>
updateDevicePool :: forall eff. UpdateDevicePoolRequest -> Aff (exception :: EXCEPTION | eff) UpdateDevicePoolResult
updateDevicePool = Request.request serviceName "updateDevicePool" 


-- | <p>Updates the network profile with specific settings.</p>
updateNetworkProfile :: forall eff. UpdateNetworkProfileRequest -> Aff (exception :: EXCEPTION | eff) UpdateNetworkProfileResult
updateNetworkProfile = Request.request serviceName "updateNetworkProfile" 


-- | <p>Modifies the specified project name, given the project ARN and a new name.</p>
updateProject :: forall eff. UpdateProjectRequest -> Aff (exception :: EXCEPTION | eff) UpdateProjectResult
updateProject = Request.request serviceName "updateProject" 


newtype AWSAccountNumber = AWSAccountNumber String
derive instance newtypeAWSAccountNumber :: Newtype AWSAccountNumber _
derive instance repGenericAWSAccountNumber :: Generic AWSAccountNumber _
instance showAWSAccountNumber :: Show AWSAccountNumber where
  show = genericShow
instance decodeAWSAccountNumber :: Decode AWSAccountNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAWSAccountNumber :: Encode AWSAccountNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A container for account-level settings within AWS Device Farm.</p>
newtype AccountSettings = AccountSettings 
  { "AwsAccountNumber'" :: NullOrUndefined.NullOrUndefined (AWSAccountNumber)
  , "UnmeteredDevices'" :: NullOrUndefined.NullOrUndefined (PurchasedDevicesMap)
  , "UnmeteredRemoteAccessDevices'" :: NullOrUndefined.NullOrUndefined (PurchasedDevicesMap)
  , "MaxJobTimeoutMinutes'" :: NullOrUndefined.NullOrUndefined (JobTimeoutMinutes)
  , "TrialMinutes'" :: NullOrUndefined.NullOrUndefined (TrialMinutes)
  , "MaxSlots'" :: NullOrUndefined.NullOrUndefined (MaxSlotMap)
  , "DefaultJobTimeoutMinutes'" :: NullOrUndefined.NullOrUndefined (JobTimeoutMinutes)
  }
derive instance newtypeAccountSettings :: Newtype AccountSettings _
derive instance repGenericAccountSettings :: Generic AccountSettings _
instance showAccountSettings :: Show AccountSettings where
  show = genericShow
instance decodeAccountSettings :: Decode AccountSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountSettings :: Encode AccountSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountsCleanup = AccountsCleanup Boolean
derive instance newtypeAccountsCleanup :: Newtype AccountsCleanup _
derive instance repGenericAccountsCleanup :: Generic AccountsCleanup _
instance showAccountsCleanup :: Show AccountsCleanup where
  show = genericShow
instance decodeAccountsCleanup :: Decode AccountsCleanup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountsCleanup :: Encode AccountsCleanup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AmazonResourceName = AmazonResourceName String
derive instance newtypeAmazonResourceName :: Newtype AmazonResourceName _
derive instance repGenericAmazonResourceName :: Generic AmazonResourceName _
instance showAmazonResourceName :: Show AmazonResourceName where
  show = genericShow
instance decodeAmazonResourceName :: Decode AmazonResourceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAmazonResourceName :: Encode AmazonResourceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AmazonResourceNames = AmazonResourceNames (Array AmazonResourceName)
derive instance newtypeAmazonResourceNames :: Newtype AmazonResourceNames _
derive instance repGenericAmazonResourceNames :: Generic AmazonResourceNames _
instance showAmazonResourceNames :: Show AmazonResourceNames where
  show = genericShow
instance decodeAmazonResourceNames :: Decode AmazonResourceNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAmazonResourceNames :: Encode AmazonResourceNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AndroidPaths = AndroidPaths (Array String)
derive instance newtypeAndroidPaths :: Newtype AndroidPaths _
derive instance repGenericAndroidPaths :: Generic AndroidPaths _
instance showAndroidPaths :: Show AndroidPaths where
  show = genericShow
instance decodeAndroidPaths :: Decode AndroidPaths where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAndroidPaths :: Encode AndroidPaths where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AppPackagesCleanup = AppPackagesCleanup Boolean
derive instance newtypeAppPackagesCleanup :: Newtype AppPackagesCleanup _
derive instance repGenericAppPackagesCleanup :: Generic AppPackagesCleanup _
instance showAppPackagesCleanup :: Show AppPackagesCleanup where
  show = genericShow
instance decodeAppPackagesCleanup :: Decode AppPackagesCleanup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAppPackagesCleanup :: Encode AppPackagesCleanup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An invalid argument was specified.</p>
newtype ArgumentException = ArgumentException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeArgumentException :: Newtype ArgumentException _
derive instance repGenericArgumentException :: Generic ArgumentException _
instance showArgumentException :: Show ArgumentException where
  show = genericShow
instance decodeArgumentException :: Decode ArgumentException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArgumentException :: Encode ArgumentException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a test. Examples of artifacts include logs and screenshots.</p>
newtype Artifact = Artifact 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Type'" :: NullOrUndefined.NullOrUndefined (ArtifactType)
  , "Extension'" :: NullOrUndefined.NullOrUndefined (String)
  , "Url'" :: NullOrUndefined.NullOrUndefined (URL)
  }
derive instance newtypeArtifact :: Newtype Artifact _
derive instance repGenericArtifact :: Generic Artifact _
instance showArtifact :: Show Artifact where
  show = genericShow
instance decodeArtifact :: Decode Artifact where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifact :: Encode Artifact where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArtifactCategory = ArtifactCategory String
derive instance newtypeArtifactCategory :: Newtype ArtifactCategory _
derive instance repGenericArtifactCategory :: Generic ArtifactCategory _
instance showArtifactCategory :: Show ArtifactCategory where
  show = genericShow
instance decodeArtifactCategory :: Decode ArtifactCategory where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactCategory :: Encode ArtifactCategory where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArtifactType = ArtifactType String
derive instance newtypeArtifactType :: Newtype ArtifactType _
derive instance repGenericArtifactType :: Generic ArtifactType _
instance showArtifactType :: Show ArtifactType where
  show = genericShow
instance decodeArtifactType :: Decode ArtifactType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactType :: Encode ArtifactType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Artifacts = Artifacts (Array Artifact)
derive instance newtypeArtifacts :: Newtype Artifacts _
derive instance repGenericArtifacts :: Generic Artifacts _
instance showArtifacts :: Show Artifacts where
  show = genericShow
instance decodeArtifacts :: Decode Artifacts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifacts :: Encode Artifacts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BillingMethod = BillingMethod String
derive instance newtypeBillingMethod :: Newtype BillingMethod _
derive instance repGenericBillingMethod :: Generic BillingMethod _
instance showBillingMethod :: Show BillingMethod where
  show = genericShow
instance decodeBillingMethod :: Decode BillingMethod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBillingMethod :: Encode BillingMethod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the amount of CPU that an app is using on a physical device.</p> <p>Note that this does not represent system-wide CPU usage.</p>
newtype CPU = CPU 
  { "Frequency'" :: NullOrUndefined.NullOrUndefined (String)
  , "Architecture'" :: NullOrUndefined.NullOrUndefined (String)
  , "Clock'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeCPU :: Newtype CPU _
derive instance repGenericCPU :: Generic CPU _
instance showCPU :: Show CPU where
  show = genericShow
instance decodeCPU :: Decode CPU where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCPU :: Encode CPU where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientId = ClientId String
derive instance newtypeClientId :: Newtype ClientId _
derive instance repGenericClientId :: Generic ClientId _
instance showClientId :: Show ClientId where
  show = genericShow
instance decodeClientId :: Decode ClientId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientId :: Encode ClientId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentType = ContentType String
derive instance newtypeContentType :: Newtype ContentType _
derive instance repGenericContentType :: Generic ContentType _
instance showContentType :: Show ContentType where
  show = genericShow
instance decodeContentType :: Decode ContentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentType :: Encode ContentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents entity counters.</p>
newtype Counters = Counters 
  { "Total'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Passed'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Failed'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Warned'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Errored'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Stopped'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Skipped'" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeCounters :: Newtype Counters _
derive instance repGenericCounters :: Generic Counters _
instance showCounters :: Show Counters where
  show = genericShow
instance decodeCounters :: Decode Counters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCounters :: Encode Counters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the create device pool operation.</p>
newtype CreateDevicePoolRequest = CreateDevicePoolRequest 
  { "ProjectArn'" :: (AmazonResourceName)
  , "Name'" :: (Name)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Rules'" :: (Rules)
  }
derive instance newtypeCreateDevicePoolRequest :: Newtype CreateDevicePoolRequest _
derive instance repGenericCreateDevicePoolRequest :: Generic CreateDevicePoolRequest _
instance showCreateDevicePoolRequest :: Show CreateDevicePoolRequest where
  show = genericShow
instance decodeCreateDevicePoolRequest :: Decode CreateDevicePoolRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDevicePoolRequest :: Encode CreateDevicePoolRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a create device pool request.</p>
newtype CreateDevicePoolResult = CreateDevicePoolResult 
  { "DevicePool'" :: NullOrUndefined.NullOrUndefined (DevicePool)
  }
derive instance newtypeCreateDevicePoolResult :: Newtype CreateDevicePoolResult _
derive instance repGenericCreateDevicePoolResult :: Generic CreateDevicePoolResult _
instance showCreateDevicePoolResult :: Show CreateDevicePoolResult where
  show = genericShow
instance decodeCreateDevicePoolResult :: Decode CreateDevicePoolResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDevicePoolResult :: Encode CreateDevicePoolResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateNetworkProfileRequest = CreateNetworkProfileRequest 
  { "ProjectArn'" :: (AmazonResourceName)
  , "Name'" :: (Name)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined.NullOrUndefined (NetworkProfileType)
  , "UplinkBandwidthBits'" :: NullOrUndefined.NullOrUndefined (Number)
  , "DownlinkBandwidthBits'" :: NullOrUndefined.NullOrUndefined (Number)
  , "UplinkDelayMs'" :: NullOrUndefined.NullOrUndefined (Number)
  , "DownlinkDelayMs'" :: NullOrUndefined.NullOrUndefined (Number)
  , "UplinkJitterMs'" :: NullOrUndefined.NullOrUndefined (Number)
  , "DownlinkJitterMs'" :: NullOrUndefined.NullOrUndefined (Number)
  , "UplinkLossPercent'" :: NullOrUndefined.NullOrUndefined (PercentInteger)
  , "DownlinkLossPercent'" :: NullOrUndefined.NullOrUndefined (PercentInteger)
  }
derive instance newtypeCreateNetworkProfileRequest :: Newtype CreateNetworkProfileRequest _
derive instance repGenericCreateNetworkProfileRequest :: Generic CreateNetworkProfileRequest _
instance showCreateNetworkProfileRequest :: Show CreateNetworkProfileRequest where
  show = genericShow
instance decodeCreateNetworkProfileRequest :: Decode CreateNetworkProfileRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateNetworkProfileRequest :: Encode CreateNetworkProfileRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateNetworkProfileResult = CreateNetworkProfileResult 
  { "NetworkProfile'" :: NullOrUndefined.NullOrUndefined (NetworkProfile)
  }
derive instance newtypeCreateNetworkProfileResult :: Newtype CreateNetworkProfileResult _
derive instance repGenericCreateNetworkProfileResult :: Generic CreateNetworkProfileResult _
instance showCreateNetworkProfileResult :: Show CreateNetworkProfileResult where
  show = genericShow
instance decodeCreateNetworkProfileResult :: Decode CreateNetworkProfileResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateNetworkProfileResult :: Encode CreateNetworkProfileResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the create project operation.</p>
newtype CreateProjectRequest = CreateProjectRequest 
  { "Name'" :: (Name)
  , "DefaultJobTimeoutMinutes'" :: NullOrUndefined.NullOrUndefined (JobTimeoutMinutes)
  }
derive instance newtypeCreateProjectRequest :: Newtype CreateProjectRequest _
derive instance repGenericCreateProjectRequest :: Generic CreateProjectRequest _
instance showCreateProjectRequest :: Show CreateProjectRequest where
  show = genericShow
instance decodeCreateProjectRequest :: Decode CreateProjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateProjectRequest :: Encode CreateProjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a create project request.</p>
newtype CreateProjectResult = CreateProjectResult 
  { "Project'" :: NullOrUndefined.NullOrUndefined (Project)
  }
derive instance newtypeCreateProjectResult :: Newtype CreateProjectResult _
derive instance repGenericCreateProjectResult :: Generic CreateProjectResult _
instance showCreateProjectResult :: Show CreateProjectResult where
  show = genericShow
instance decodeCreateProjectResult :: Decode CreateProjectResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateProjectResult :: Encode CreateProjectResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Creates the configuration settings for a remote access session, including the device model and type.</p>
newtype CreateRemoteAccessSessionConfiguration = CreateRemoteAccessSessionConfiguration 
  { "BillingMethod'" :: NullOrUndefined.NullOrUndefined (BillingMethod)
  }
derive instance newtypeCreateRemoteAccessSessionConfiguration :: Newtype CreateRemoteAccessSessionConfiguration _
derive instance repGenericCreateRemoteAccessSessionConfiguration :: Generic CreateRemoteAccessSessionConfiguration _
instance showCreateRemoteAccessSessionConfiguration :: Show CreateRemoteAccessSessionConfiguration where
  show = genericShow
instance decodeCreateRemoteAccessSessionConfiguration :: Decode CreateRemoteAccessSessionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRemoteAccessSessionConfiguration :: Encode CreateRemoteAccessSessionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Creates and submits a request to start a remote access session.</p>
newtype CreateRemoteAccessSessionRequest = CreateRemoteAccessSessionRequest 
  { "ProjectArn'" :: (AmazonResourceName)
  , "DeviceArn'" :: (AmazonResourceName)
  , "SshPublicKey'" :: NullOrUndefined.NullOrUndefined (SshPublicKey)
  , "RemoteDebugEnabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RemoteRecordEnabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RemoteRecordAppArn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "ClientId'" :: NullOrUndefined.NullOrUndefined (ClientId)
  , "Configuration'" :: NullOrUndefined.NullOrUndefined (CreateRemoteAccessSessionConfiguration)
  , "InteractionMode'" :: NullOrUndefined.NullOrUndefined (InteractionMode)
  }
derive instance newtypeCreateRemoteAccessSessionRequest :: Newtype CreateRemoteAccessSessionRequest _
derive instance repGenericCreateRemoteAccessSessionRequest :: Generic CreateRemoteAccessSessionRequest _
instance showCreateRemoteAccessSessionRequest :: Show CreateRemoteAccessSessionRequest where
  show = genericShow
instance decodeCreateRemoteAccessSessionRequest :: Decode CreateRemoteAccessSessionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRemoteAccessSessionRequest :: Encode CreateRemoteAccessSessionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the server response from a request to create a remote access session.</p>
newtype CreateRemoteAccessSessionResult = CreateRemoteAccessSessionResult 
  { "RemoteAccessSession'" :: NullOrUndefined.NullOrUndefined (RemoteAccessSession)
  }
derive instance newtypeCreateRemoteAccessSessionResult :: Newtype CreateRemoteAccessSessionResult _
derive instance repGenericCreateRemoteAccessSessionResult :: Generic CreateRemoteAccessSessionResult _
instance showCreateRemoteAccessSessionResult :: Show CreateRemoteAccessSessionResult where
  show = genericShow
instance decodeCreateRemoteAccessSessionResult :: Decode CreateRemoteAccessSessionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRemoteAccessSessionResult :: Encode CreateRemoteAccessSessionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the create upload operation.</p>
newtype CreateUploadRequest = CreateUploadRequest 
  { "ProjectArn'" :: (AmazonResourceName)
  , "Name'" :: (Name)
  , "Type'" :: (UploadType)
  , "ContentType'" :: NullOrUndefined.NullOrUndefined (ContentType)
  }
derive instance newtypeCreateUploadRequest :: Newtype CreateUploadRequest _
derive instance repGenericCreateUploadRequest :: Generic CreateUploadRequest _
instance showCreateUploadRequest :: Show CreateUploadRequest where
  show = genericShow
instance decodeCreateUploadRequest :: Decode CreateUploadRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUploadRequest :: Encode CreateUploadRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a create upload request.</p>
newtype CreateUploadResult = CreateUploadResult 
  { "Upload'" :: NullOrUndefined.NullOrUndefined (Upload)
  }
derive instance newtypeCreateUploadResult :: Newtype CreateUploadResult _
derive instance repGenericCreateUploadResult :: Generic CreateUploadResult _
instance showCreateUploadResult :: Show CreateUploadResult where
  show = genericShow
instance decodeCreateUploadResult :: Decode CreateUploadResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUploadResult :: Encode CreateUploadResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CurrencyCode = CurrencyCode String
derive instance newtypeCurrencyCode :: Newtype CurrencyCode _
derive instance repGenericCurrencyCode :: Generic CurrencyCode _
instance showCurrencyCode :: Show CurrencyCode where
  show = genericShow
instance decodeCurrencyCode :: Decode CurrencyCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCurrencyCode :: Encode CurrencyCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object specifying the paths where the artifacts generated by the customer's tests, on the device or in the test environment, will be pulled from.</p> <p>Specify <code>deviceHostPaths</code> and optionally specify either <code>iosPaths</code> or <code>androidPaths</code>.</p> <p>For web app tests, you can specify both <code>iosPaths</code> and <code>androidPaths</code>.</p>
newtype CustomerArtifactPaths = CustomerArtifactPaths 
  { "IosPaths'" :: NullOrUndefined.NullOrUndefined (IosPaths)
  , "AndroidPaths'" :: NullOrUndefined.NullOrUndefined (AndroidPaths)
  , "DeviceHostPaths'" :: NullOrUndefined.NullOrUndefined (DeviceHostPaths)
  }
derive instance newtypeCustomerArtifactPaths :: Newtype CustomerArtifactPaths _
derive instance repGenericCustomerArtifactPaths :: Generic CustomerArtifactPaths _
instance showCustomerArtifactPaths :: Show CustomerArtifactPaths where
  show = genericShow
instance decodeCustomerArtifactPaths :: Decode CustomerArtifactPaths where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomerArtifactPaths :: Encode CustomerArtifactPaths where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DateTime = DateTime Number
derive instance newtypeDateTime :: Newtype DateTime _
derive instance repGenericDateTime :: Generic DateTime _
instance showDateTime :: Show DateTime where
  show = genericShow
instance decodeDateTime :: Decode DateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDateTime :: Encode DateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the delete device pool operation.</p>
newtype DeleteDevicePoolRequest = DeleteDevicePoolRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeDeleteDevicePoolRequest :: Newtype DeleteDevicePoolRequest _
derive instance repGenericDeleteDevicePoolRequest :: Generic DeleteDevicePoolRequest _
instance showDeleteDevicePoolRequest :: Show DeleteDevicePoolRequest where
  show = genericShow
instance decodeDeleteDevicePoolRequest :: Decode DeleteDevicePoolRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDevicePoolRequest :: Encode DeleteDevicePoolRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a delete device pool request.</p>
newtype DeleteDevicePoolResult = DeleteDevicePoolResult Types.NoArguments
derive instance newtypeDeleteDevicePoolResult :: Newtype DeleteDevicePoolResult _
derive instance repGenericDeleteDevicePoolResult :: Generic DeleteDevicePoolResult _
instance showDeleteDevicePoolResult :: Show DeleteDevicePoolResult where
  show = genericShow
instance decodeDeleteDevicePoolResult :: Decode DeleteDevicePoolResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDevicePoolResult :: Encode DeleteDevicePoolResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteNetworkProfileRequest = DeleteNetworkProfileRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeDeleteNetworkProfileRequest :: Newtype DeleteNetworkProfileRequest _
derive instance repGenericDeleteNetworkProfileRequest :: Generic DeleteNetworkProfileRequest _
instance showDeleteNetworkProfileRequest :: Show DeleteNetworkProfileRequest where
  show = genericShow
instance decodeDeleteNetworkProfileRequest :: Decode DeleteNetworkProfileRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteNetworkProfileRequest :: Encode DeleteNetworkProfileRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteNetworkProfileResult = DeleteNetworkProfileResult Types.NoArguments
derive instance newtypeDeleteNetworkProfileResult :: Newtype DeleteNetworkProfileResult _
derive instance repGenericDeleteNetworkProfileResult :: Generic DeleteNetworkProfileResult _
instance showDeleteNetworkProfileResult :: Show DeleteNetworkProfileResult where
  show = genericShow
instance decodeDeleteNetworkProfileResult :: Decode DeleteNetworkProfileResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteNetworkProfileResult :: Encode DeleteNetworkProfileResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the delete project operation.</p>
newtype DeleteProjectRequest = DeleteProjectRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeDeleteProjectRequest :: Newtype DeleteProjectRequest _
derive instance repGenericDeleteProjectRequest :: Generic DeleteProjectRequest _
instance showDeleteProjectRequest :: Show DeleteProjectRequest where
  show = genericShow
instance decodeDeleteProjectRequest :: Decode DeleteProjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteProjectRequest :: Encode DeleteProjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a delete project request.</p>
newtype DeleteProjectResult = DeleteProjectResult Types.NoArguments
derive instance newtypeDeleteProjectResult :: Newtype DeleteProjectResult _
derive instance repGenericDeleteProjectResult :: Generic DeleteProjectResult _
instance showDeleteProjectResult :: Show DeleteProjectResult where
  show = genericShow
instance decodeDeleteProjectResult :: Decode DeleteProjectResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteProjectResult :: Encode DeleteProjectResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to delete the specified remote access session.</p>
newtype DeleteRemoteAccessSessionRequest = DeleteRemoteAccessSessionRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeDeleteRemoteAccessSessionRequest :: Newtype DeleteRemoteAccessSessionRequest _
derive instance repGenericDeleteRemoteAccessSessionRequest :: Generic DeleteRemoteAccessSessionRequest _
instance showDeleteRemoteAccessSessionRequest :: Show DeleteRemoteAccessSessionRequest where
  show = genericShow
instance decodeDeleteRemoteAccessSessionRequest :: Decode DeleteRemoteAccessSessionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRemoteAccessSessionRequest :: Encode DeleteRemoteAccessSessionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from the server when a request is made to delete the remote access session.</p>
newtype DeleteRemoteAccessSessionResult = DeleteRemoteAccessSessionResult Types.NoArguments
derive instance newtypeDeleteRemoteAccessSessionResult :: Newtype DeleteRemoteAccessSessionResult _
derive instance repGenericDeleteRemoteAccessSessionResult :: Generic DeleteRemoteAccessSessionResult _
instance showDeleteRemoteAccessSessionResult :: Show DeleteRemoteAccessSessionResult where
  show = genericShow
instance decodeDeleteRemoteAccessSessionResult :: Decode DeleteRemoteAccessSessionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRemoteAccessSessionResult :: Encode DeleteRemoteAccessSessionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the delete run operation.</p>
newtype DeleteRunRequest = DeleteRunRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeDeleteRunRequest :: Newtype DeleteRunRequest _
derive instance repGenericDeleteRunRequest :: Generic DeleteRunRequest _
instance showDeleteRunRequest :: Show DeleteRunRequest where
  show = genericShow
instance decodeDeleteRunRequest :: Decode DeleteRunRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRunRequest :: Encode DeleteRunRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a delete run request.</p>
newtype DeleteRunResult = DeleteRunResult Types.NoArguments
derive instance newtypeDeleteRunResult :: Newtype DeleteRunResult _
derive instance repGenericDeleteRunResult :: Generic DeleteRunResult _
instance showDeleteRunResult :: Show DeleteRunResult where
  show = genericShow
instance decodeDeleteRunResult :: Decode DeleteRunResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRunResult :: Encode DeleteRunResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the delete upload operation.</p>
newtype DeleteUploadRequest = DeleteUploadRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeDeleteUploadRequest :: Newtype DeleteUploadRequest _
derive instance repGenericDeleteUploadRequest :: Generic DeleteUploadRequest _
instance showDeleteUploadRequest :: Show DeleteUploadRequest where
  show = genericShow
instance decodeDeleteUploadRequest :: Decode DeleteUploadRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUploadRequest :: Encode DeleteUploadRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a delete upload request.</p>
newtype DeleteUploadResult = DeleteUploadResult Types.NoArguments
derive instance newtypeDeleteUploadResult :: Newtype DeleteUploadResult _
derive instance repGenericDeleteUploadResult :: Generic DeleteUploadResult _
instance showDeleteUploadResult :: Show DeleteUploadResult where
  show = genericShow
instance decodeDeleteUploadResult :: Decode DeleteUploadResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUploadResult :: Encode DeleteUploadResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a device type that an app is tested against.</p>
newtype Device = Device 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Manufacturer'" :: NullOrUndefined.NullOrUndefined (String)
  , "Model'" :: NullOrUndefined.NullOrUndefined (String)
  , "ModelId'" :: NullOrUndefined.NullOrUndefined (String)
  , "FormFactor'" :: NullOrUndefined.NullOrUndefined (DeviceFormFactor)
  , "Platform'" :: NullOrUndefined.NullOrUndefined (DevicePlatform)
  , "Os'" :: NullOrUndefined.NullOrUndefined (String)
  , "Cpu'" :: NullOrUndefined.NullOrUndefined (CPU)
  , "Resolution'" :: NullOrUndefined.NullOrUndefined (Resolution)
  , "HeapSize'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Memory'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Image'" :: NullOrUndefined.NullOrUndefined (String)
  , "Carrier'" :: NullOrUndefined.NullOrUndefined (String)
  , "Radio'" :: NullOrUndefined.NullOrUndefined (String)
  , "RemoteAccessEnabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RemoteDebugEnabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "FleetType'" :: NullOrUndefined.NullOrUndefined (String)
  , "FleetName'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDevice :: Newtype Device _
derive instance repGenericDevice :: Generic Device _
instance showDevice :: Show Device where
  show = genericShow
instance decodeDevice :: Decode Device where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevice :: Encode Device where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeviceAttribute = DeviceAttribute String
derive instance newtypeDeviceAttribute :: Newtype DeviceAttribute _
derive instance repGenericDeviceAttribute :: Generic DeviceAttribute _
instance showDeviceAttribute :: Show DeviceAttribute where
  show = genericShow
instance decodeDeviceAttribute :: Decode DeviceAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceAttribute :: Encode DeviceAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeviceFormFactor = DeviceFormFactor String
derive instance newtypeDeviceFormFactor :: Newtype DeviceFormFactor _
derive instance repGenericDeviceFormFactor :: Generic DeviceFormFactor _
instance showDeviceFormFactor :: Show DeviceFormFactor where
  show = genericShow
instance decodeDeviceFormFactor :: Decode DeviceFormFactor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceFormFactor :: Encode DeviceFormFactor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeviceHostPaths = DeviceHostPaths (Array String)
derive instance newtypeDeviceHostPaths :: Newtype DeviceHostPaths _
derive instance repGenericDeviceHostPaths :: Generic DeviceHostPaths _
instance showDeviceHostPaths :: Show DeviceHostPaths where
  show = genericShow
instance decodeDeviceHostPaths :: Decode DeviceHostPaths where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceHostPaths :: Encode DeviceHostPaths where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the total (metered or unmetered) minutes used by the resource to run tests. Contains the sum of minutes consumed by all children.</p>
newtype DeviceMinutes = DeviceMinutes 
  { "Total'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Metered'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Unmetered'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeDeviceMinutes :: Newtype DeviceMinutes _
derive instance repGenericDeviceMinutes :: Generic DeviceMinutes _
instance showDeviceMinutes :: Show DeviceMinutes where
  show = genericShow
instance decodeDeviceMinutes :: Decode DeviceMinutes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceMinutes :: Encode DeviceMinutes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DevicePlatform = DevicePlatform String
derive instance newtypeDevicePlatform :: Newtype DevicePlatform _
derive instance repGenericDevicePlatform :: Generic DevicePlatform _
instance showDevicePlatform :: Show DevicePlatform where
  show = genericShow
instance decodeDevicePlatform :: Decode DevicePlatform where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevicePlatform :: Encode DevicePlatform where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection of device types.</p>
newtype DevicePool = DevicePool 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined.NullOrUndefined (DevicePoolType)
  , "Rules'" :: NullOrUndefined.NullOrUndefined (Rules)
  }
derive instance newtypeDevicePool :: Newtype DevicePool _
derive instance repGenericDevicePool :: Generic DevicePool _
instance showDevicePool :: Show DevicePool where
  show = genericShow
instance decodeDevicePool :: Decode DevicePool where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevicePool :: Encode DevicePool where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a device pool compatibility result.</p>
newtype DevicePoolCompatibilityResult = DevicePoolCompatibilityResult 
  { "Device'" :: NullOrUndefined.NullOrUndefined (Device)
  , "Compatible'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "IncompatibilityMessages'" :: NullOrUndefined.NullOrUndefined (IncompatibilityMessages)
  }
derive instance newtypeDevicePoolCompatibilityResult :: Newtype DevicePoolCompatibilityResult _
derive instance repGenericDevicePoolCompatibilityResult :: Generic DevicePoolCompatibilityResult _
instance showDevicePoolCompatibilityResult :: Show DevicePoolCompatibilityResult where
  show = genericShow
instance decodeDevicePoolCompatibilityResult :: Decode DevicePoolCompatibilityResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevicePoolCompatibilityResult :: Encode DevicePoolCompatibilityResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DevicePoolCompatibilityResults = DevicePoolCompatibilityResults (Array DevicePoolCompatibilityResult)
derive instance newtypeDevicePoolCompatibilityResults :: Newtype DevicePoolCompatibilityResults _
derive instance repGenericDevicePoolCompatibilityResults :: Generic DevicePoolCompatibilityResults _
instance showDevicePoolCompatibilityResults :: Show DevicePoolCompatibilityResults where
  show = genericShow
instance decodeDevicePoolCompatibilityResults :: Decode DevicePoolCompatibilityResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevicePoolCompatibilityResults :: Encode DevicePoolCompatibilityResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DevicePoolType = DevicePoolType String
derive instance newtypeDevicePoolType :: Newtype DevicePoolType _
derive instance repGenericDevicePoolType :: Generic DevicePoolType _
instance showDevicePoolType :: Show DevicePoolType where
  show = genericShow
instance decodeDevicePoolType :: Decode DevicePoolType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevicePoolType :: Encode DevicePoolType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DevicePools = DevicePools (Array DevicePool)
derive instance newtypeDevicePools :: Newtype DevicePools _
derive instance repGenericDevicePools :: Generic DevicePools _
instance showDevicePools :: Show DevicePools where
  show = genericShow
instance decodeDevicePools :: Decode DevicePools where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevicePools :: Encode DevicePools where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Devices = Devices (Array Device)
derive instance newtypeDevices :: Newtype Devices _
derive instance repGenericDevices :: Generic Devices _
instance showDevices :: Show Devices where
  show = genericShow
instance decodeDevices :: Decode Devices where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevices :: Encode Devices where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents configuration information about a test run, such as the execution timeout (in minutes).</p>
newtype ExecutionConfiguration = ExecutionConfiguration 
  { "JobTimeoutMinutes'" :: NullOrUndefined.NullOrUndefined (JobTimeoutMinutes)
  , "AccountsCleanup'" :: NullOrUndefined.NullOrUndefined (AccountsCleanup)
  , "AppPackagesCleanup'" :: NullOrUndefined.NullOrUndefined (AppPackagesCleanup)
  }
derive instance newtypeExecutionConfiguration :: Newtype ExecutionConfiguration _
derive instance repGenericExecutionConfiguration :: Generic ExecutionConfiguration _
instance showExecutionConfiguration :: Show ExecutionConfiguration where
  show = genericShow
instance decodeExecutionConfiguration :: Decode ExecutionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionConfiguration :: Encode ExecutionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExecutionResult = ExecutionResult String
derive instance newtypeExecutionResult :: Newtype ExecutionResult _
derive instance repGenericExecutionResult :: Generic ExecutionResult _
instance showExecutionResult :: Show ExecutionResult where
  show = genericShow
instance decodeExecutionResult :: Decode ExecutionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionResult :: Encode ExecutionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExecutionResultCode = ExecutionResultCode String
derive instance newtypeExecutionResultCode :: Newtype ExecutionResultCode _
derive instance repGenericExecutionResultCode :: Generic ExecutionResultCode _
instance showExecutionResultCode :: Show ExecutionResultCode where
  show = genericShow
instance decodeExecutionResultCode :: Decode ExecutionResultCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionResultCode :: Encode ExecutionResultCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExecutionStatus = ExecutionStatus String
derive instance newtypeExecutionStatus :: Newtype ExecutionStatus _
derive instance repGenericExecutionStatus :: Generic ExecutionStatus _
instance showExecutionStatus :: Show ExecutionStatus where
  show = genericShow
instance decodeExecutionStatus :: Decode ExecutionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionStatus :: Encode ExecutionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Filter = Filter String
derive instance newtypeFilter :: Newtype Filter _
derive instance repGenericFilter :: Generic Filter _
instance showFilter :: Show Filter where
  show = genericShow
instance decodeFilter :: Decode Filter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilter :: Encode Filter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request sent to retrieve the account settings.</p>
newtype GetAccountSettingsRequest = GetAccountSettingsRequest Types.NoArguments
derive instance newtypeGetAccountSettingsRequest :: Newtype GetAccountSettingsRequest _
derive instance repGenericGetAccountSettingsRequest :: Generic GetAccountSettingsRequest _
instance showGetAccountSettingsRequest :: Show GetAccountSettingsRequest where
  show = genericShow
instance decodeGetAccountSettingsRequest :: Decode GetAccountSettingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAccountSettingsRequest :: Encode GetAccountSettingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the account settings return values from the <code>GetAccountSettings</code> request.</p>
newtype GetAccountSettingsResult = GetAccountSettingsResult 
  { "AccountSettings'" :: NullOrUndefined.NullOrUndefined (AccountSettings)
  }
derive instance newtypeGetAccountSettingsResult :: Newtype GetAccountSettingsResult _
derive instance repGenericGetAccountSettingsResult :: Generic GetAccountSettingsResult _
instance showGetAccountSettingsResult :: Show GetAccountSettingsResult where
  show = genericShow
instance decodeGetAccountSettingsResult :: Decode GetAccountSettingsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAccountSettingsResult :: Encode GetAccountSettingsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the get device pool compatibility operation.</p>
newtype GetDevicePoolCompatibilityRequest = GetDevicePoolCompatibilityRequest 
  { "DevicePoolArn'" :: (AmazonResourceName)
  , "AppArn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "TestType'" :: NullOrUndefined.NullOrUndefined (TestType)
  , "Test'" :: NullOrUndefined.NullOrUndefined (ScheduleRunTest)
  }
derive instance newtypeGetDevicePoolCompatibilityRequest :: Newtype GetDevicePoolCompatibilityRequest _
derive instance repGenericGetDevicePoolCompatibilityRequest :: Generic GetDevicePoolCompatibilityRequest _
instance showGetDevicePoolCompatibilityRequest :: Show GetDevicePoolCompatibilityRequest where
  show = genericShow
instance decodeGetDevicePoolCompatibilityRequest :: Decode GetDevicePoolCompatibilityRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDevicePoolCompatibilityRequest :: Encode GetDevicePoolCompatibilityRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of describe device pool compatibility request.</p>
newtype GetDevicePoolCompatibilityResult = GetDevicePoolCompatibilityResult 
  { "CompatibleDevices'" :: NullOrUndefined.NullOrUndefined (DevicePoolCompatibilityResults)
  , "IncompatibleDevices'" :: NullOrUndefined.NullOrUndefined (DevicePoolCompatibilityResults)
  }
derive instance newtypeGetDevicePoolCompatibilityResult :: Newtype GetDevicePoolCompatibilityResult _
derive instance repGenericGetDevicePoolCompatibilityResult :: Generic GetDevicePoolCompatibilityResult _
instance showGetDevicePoolCompatibilityResult :: Show GetDevicePoolCompatibilityResult where
  show = genericShow
instance decodeGetDevicePoolCompatibilityResult :: Decode GetDevicePoolCompatibilityResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDevicePoolCompatibilityResult :: Encode GetDevicePoolCompatibilityResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the get device pool operation.</p>
newtype GetDevicePoolRequest = GetDevicePoolRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetDevicePoolRequest :: Newtype GetDevicePoolRequest _
derive instance repGenericGetDevicePoolRequest :: Generic GetDevicePoolRequest _
instance showGetDevicePoolRequest :: Show GetDevicePoolRequest where
  show = genericShow
instance decodeGetDevicePoolRequest :: Decode GetDevicePoolRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDevicePoolRequest :: Encode GetDevicePoolRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a get device pool request.</p>
newtype GetDevicePoolResult = GetDevicePoolResult 
  { "DevicePool'" :: NullOrUndefined.NullOrUndefined (DevicePool)
  }
derive instance newtypeGetDevicePoolResult :: Newtype GetDevicePoolResult _
derive instance repGenericGetDevicePoolResult :: Generic GetDevicePoolResult _
instance showGetDevicePoolResult :: Show GetDevicePoolResult where
  show = genericShow
instance decodeGetDevicePoolResult :: Decode GetDevicePoolResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDevicePoolResult :: Encode GetDevicePoolResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the get device request.</p>
newtype GetDeviceRequest = GetDeviceRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetDeviceRequest :: Newtype GetDeviceRequest _
derive instance repGenericGetDeviceRequest :: Generic GetDeviceRequest _
instance showGetDeviceRequest :: Show GetDeviceRequest where
  show = genericShow
instance decodeGetDeviceRequest :: Decode GetDeviceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeviceRequest :: Encode GetDeviceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a get device request.</p>
newtype GetDeviceResult = GetDeviceResult 
  { "Device'" :: NullOrUndefined.NullOrUndefined (Device)
  }
derive instance newtypeGetDeviceResult :: Newtype GetDeviceResult _
derive instance repGenericGetDeviceResult :: Generic GetDeviceResult _
instance showGetDeviceResult :: Show GetDeviceResult where
  show = genericShow
instance decodeGetDeviceResult :: Decode GetDeviceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeviceResult :: Encode GetDeviceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the get job operation.</p>
newtype GetJobRequest = GetJobRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetJobRequest :: Newtype GetJobRequest _
derive instance repGenericGetJobRequest :: Generic GetJobRequest _
instance showGetJobRequest :: Show GetJobRequest where
  show = genericShow
instance decodeGetJobRequest :: Decode GetJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobRequest :: Encode GetJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a get job request.</p>
newtype GetJobResult = GetJobResult 
  { "Job'" :: NullOrUndefined.NullOrUndefined (Job)
  }
derive instance newtypeGetJobResult :: Newtype GetJobResult _
derive instance repGenericGetJobResult :: Generic GetJobResult _
instance showGetJobResult :: Show GetJobResult where
  show = genericShow
instance decodeGetJobResult :: Decode GetJobResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobResult :: Encode GetJobResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetNetworkProfileRequest = GetNetworkProfileRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetNetworkProfileRequest :: Newtype GetNetworkProfileRequest _
derive instance repGenericGetNetworkProfileRequest :: Generic GetNetworkProfileRequest _
instance showGetNetworkProfileRequest :: Show GetNetworkProfileRequest where
  show = genericShow
instance decodeGetNetworkProfileRequest :: Decode GetNetworkProfileRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetNetworkProfileRequest :: Encode GetNetworkProfileRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetNetworkProfileResult = GetNetworkProfileResult 
  { "NetworkProfile'" :: NullOrUndefined.NullOrUndefined (NetworkProfile)
  }
derive instance newtypeGetNetworkProfileResult :: Newtype GetNetworkProfileResult _
derive instance repGenericGetNetworkProfileResult :: Generic GetNetworkProfileResult _
instance showGetNetworkProfileResult :: Show GetNetworkProfileResult where
  show = genericShow
instance decodeGetNetworkProfileResult :: Decode GetNetworkProfileResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetNetworkProfileResult :: Encode GetNetworkProfileResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to retrieve the offering status for the specified customer or account.</p>
newtype GetOfferingStatusRequest = GetOfferingStatusRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeGetOfferingStatusRequest :: Newtype GetOfferingStatusRequest _
derive instance repGenericGetOfferingStatusRequest :: Generic GetOfferingStatusRequest _
instance showGetOfferingStatusRequest :: Show GetOfferingStatusRequest where
  show = genericShow
instance decodeGetOfferingStatusRequest :: Decode GetOfferingStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOfferingStatusRequest :: Encode GetOfferingStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the status result for a device offering.</p>
newtype GetOfferingStatusResult = GetOfferingStatusResult 
  { "Current'" :: NullOrUndefined.NullOrUndefined (OfferingStatusMap)
  , "NextPeriod'" :: NullOrUndefined.NullOrUndefined (OfferingStatusMap)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeGetOfferingStatusResult :: Newtype GetOfferingStatusResult _
derive instance repGenericGetOfferingStatusResult :: Generic GetOfferingStatusResult _
instance showGetOfferingStatusResult :: Show GetOfferingStatusResult where
  show = genericShow
instance decodeGetOfferingStatusResult :: Decode GetOfferingStatusResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOfferingStatusResult :: Encode GetOfferingStatusResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the get project operation.</p>
newtype GetProjectRequest = GetProjectRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetProjectRequest :: Newtype GetProjectRequest _
derive instance repGenericGetProjectRequest :: Generic GetProjectRequest _
instance showGetProjectRequest :: Show GetProjectRequest where
  show = genericShow
instance decodeGetProjectRequest :: Decode GetProjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetProjectRequest :: Encode GetProjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a get project request.</p>
newtype GetProjectResult = GetProjectResult 
  { "Project'" :: NullOrUndefined.NullOrUndefined (Project)
  }
derive instance newtypeGetProjectResult :: Newtype GetProjectResult _
derive instance repGenericGetProjectResult :: Generic GetProjectResult _
instance showGetProjectResult :: Show GetProjectResult where
  show = genericShow
instance decodeGetProjectResult :: Decode GetProjectResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetProjectResult :: Encode GetProjectResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to get information about the specified remote access session.</p>
newtype GetRemoteAccessSessionRequest = GetRemoteAccessSessionRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetRemoteAccessSessionRequest :: Newtype GetRemoteAccessSessionRequest _
derive instance repGenericGetRemoteAccessSessionRequest :: Generic GetRemoteAccessSessionRequest _
instance showGetRemoteAccessSessionRequest :: Show GetRemoteAccessSessionRequest where
  show = genericShow
instance decodeGetRemoteAccessSessionRequest :: Decode GetRemoteAccessSessionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRemoteAccessSessionRequest :: Encode GetRemoteAccessSessionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server that lists detailed information about the remote access session.</p>
newtype GetRemoteAccessSessionResult = GetRemoteAccessSessionResult 
  { "RemoteAccessSession'" :: NullOrUndefined.NullOrUndefined (RemoteAccessSession)
  }
derive instance newtypeGetRemoteAccessSessionResult :: Newtype GetRemoteAccessSessionResult _
derive instance repGenericGetRemoteAccessSessionResult :: Generic GetRemoteAccessSessionResult _
instance showGetRemoteAccessSessionResult :: Show GetRemoteAccessSessionResult where
  show = genericShow
instance decodeGetRemoteAccessSessionResult :: Decode GetRemoteAccessSessionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRemoteAccessSessionResult :: Encode GetRemoteAccessSessionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the get run operation.</p>
newtype GetRunRequest = GetRunRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetRunRequest :: Newtype GetRunRequest _
derive instance repGenericGetRunRequest :: Generic GetRunRequest _
instance showGetRunRequest :: Show GetRunRequest where
  show = genericShow
instance decodeGetRunRequest :: Decode GetRunRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRunRequest :: Encode GetRunRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a get run request.</p>
newtype GetRunResult = GetRunResult 
  { "Run'" :: NullOrUndefined.NullOrUndefined (Run)
  }
derive instance newtypeGetRunResult :: Newtype GetRunResult _
derive instance repGenericGetRunResult :: Generic GetRunResult _
instance showGetRunResult :: Show GetRunResult where
  show = genericShow
instance decodeGetRunResult :: Decode GetRunResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRunResult :: Encode GetRunResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the get suite operation.</p>
newtype GetSuiteRequest = GetSuiteRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetSuiteRequest :: Newtype GetSuiteRequest _
derive instance repGenericGetSuiteRequest :: Generic GetSuiteRequest _
instance showGetSuiteRequest :: Show GetSuiteRequest where
  show = genericShow
instance decodeGetSuiteRequest :: Decode GetSuiteRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSuiteRequest :: Encode GetSuiteRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a get suite request.</p>
newtype GetSuiteResult = GetSuiteResult 
  { "Suite'" :: NullOrUndefined.NullOrUndefined (Suite)
  }
derive instance newtypeGetSuiteResult :: Newtype GetSuiteResult _
derive instance repGenericGetSuiteResult :: Generic GetSuiteResult _
instance showGetSuiteResult :: Show GetSuiteResult where
  show = genericShow
instance decodeGetSuiteResult :: Decode GetSuiteResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSuiteResult :: Encode GetSuiteResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the get test operation.</p>
newtype GetTestRequest = GetTestRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetTestRequest :: Newtype GetTestRequest _
derive instance repGenericGetTestRequest :: Generic GetTestRequest _
instance showGetTestRequest :: Show GetTestRequest where
  show = genericShow
instance decodeGetTestRequest :: Decode GetTestRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTestRequest :: Encode GetTestRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a get test request.</p>
newtype GetTestResult = GetTestResult 
  { "Test'" :: NullOrUndefined.NullOrUndefined (Test)
  }
derive instance newtypeGetTestResult :: Newtype GetTestResult _
derive instance repGenericGetTestResult :: Generic GetTestResult _
instance showGetTestResult :: Show GetTestResult where
  show = genericShow
instance decodeGetTestResult :: Decode GetTestResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTestResult :: Encode GetTestResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the get upload operation.</p>
newtype GetUploadRequest = GetUploadRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeGetUploadRequest :: Newtype GetUploadRequest _
derive instance repGenericGetUploadRequest :: Generic GetUploadRequest _
instance showGetUploadRequest :: Show GetUploadRequest where
  show = genericShow
instance decodeGetUploadRequest :: Decode GetUploadRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUploadRequest :: Encode GetUploadRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a get upload request.</p>
newtype GetUploadResult = GetUploadResult 
  { "Upload'" :: NullOrUndefined.NullOrUndefined (Upload)
  }
derive instance newtypeGetUploadResult :: Newtype GetUploadResult _
derive instance repGenericGetUploadResult :: Generic GetUploadResult _
instance showGetUploadResult :: Show GetUploadResult where
  show = genericShow
instance decodeGetUploadResult :: Decode GetUploadResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUploadResult :: Encode GetUploadResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HostAddress = HostAddress String
derive instance newtypeHostAddress :: Newtype HostAddress _
derive instance repGenericHostAddress :: Generic HostAddress _
instance showHostAddress :: Show HostAddress where
  show = genericShow
instance decodeHostAddress :: Decode HostAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostAddress :: Encode HostAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An entity with the same name already exists.</p>
newtype IdempotencyException = IdempotencyException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeIdempotencyException :: Newtype IdempotencyException _
derive instance repGenericIdempotencyException :: Generic IdempotencyException _
instance showIdempotencyException :: Show IdempotencyException where
  show = genericShow
instance decodeIdempotencyException :: Decode IdempotencyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdempotencyException :: Encode IdempotencyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about incompatibility.</p>
newtype IncompatibilityMessage = IncompatibilityMessage 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined.NullOrUndefined (DeviceAttribute)
  }
derive instance newtypeIncompatibilityMessage :: Newtype IncompatibilityMessage _
derive instance repGenericIncompatibilityMessage :: Generic IncompatibilityMessage _
instance showIncompatibilityMessage :: Show IncompatibilityMessage where
  show = genericShow
instance decodeIncompatibilityMessage :: Decode IncompatibilityMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncompatibilityMessage :: Encode IncompatibilityMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IncompatibilityMessages = IncompatibilityMessages (Array IncompatibilityMessage)
derive instance newtypeIncompatibilityMessages :: Newtype IncompatibilityMessages _
derive instance repGenericIncompatibilityMessages :: Generic IncompatibilityMessages _
instance showIncompatibilityMessages :: Show IncompatibilityMessages where
  show = genericShow
instance decodeIncompatibilityMessages :: Decode IncompatibilityMessages where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncompatibilityMessages :: Encode IncompatibilityMessages where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to install an Android application (in .apk format) or an iOS application (in .ipa format) as part of a remote access session.</p>
newtype InstallToRemoteAccessSessionRequest = InstallToRemoteAccessSessionRequest 
  { "RemoteAccessSessionArn'" :: (AmazonResourceName)
  , "AppArn'" :: (AmazonResourceName)
  }
derive instance newtypeInstallToRemoteAccessSessionRequest :: Newtype InstallToRemoteAccessSessionRequest _
derive instance repGenericInstallToRemoteAccessSessionRequest :: Generic InstallToRemoteAccessSessionRequest _
instance showInstallToRemoteAccessSessionRequest :: Show InstallToRemoteAccessSessionRequest where
  show = genericShow
instance decodeInstallToRemoteAccessSessionRequest :: Decode InstallToRemoteAccessSessionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstallToRemoteAccessSessionRequest :: Encode InstallToRemoteAccessSessionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server after AWS Device Farm makes a request to install to a remote access session.</p>
newtype InstallToRemoteAccessSessionResult = InstallToRemoteAccessSessionResult 
  { "AppUpload'" :: NullOrUndefined.NullOrUndefined (Upload)
  }
derive instance newtypeInstallToRemoteAccessSessionResult :: Newtype InstallToRemoteAccessSessionResult _
derive instance repGenericInstallToRemoteAccessSessionResult :: Generic InstallToRemoteAccessSessionResult _
instance showInstallToRemoteAccessSessionResult :: Show InstallToRemoteAccessSessionResult where
  show = genericShow
instance decodeInstallToRemoteAccessSessionResult :: Decode InstallToRemoteAccessSessionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstallToRemoteAccessSessionResult :: Encode InstallToRemoteAccessSessionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InteractionMode = InteractionMode String
derive instance newtypeInteractionMode :: Newtype InteractionMode _
derive instance repGenericInteractionMode :: Generic InteractionMode _
instance showInteractionMode :: Show InteractionMode where
  show = genericShow
instance decodeInteractionMode :: Decode InteractionMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInteractionMode :: Encode InteractionMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IosPaths = IosPaths (Array String)
derive instance newtypeIosPaths :: Newtype IosPaths _
derive instance repGenericIosPaths :: Generic IosPaths _
instance showIosPaths :: Show IosPaths where
  show = genericShow
instance decodeIosPaths :: Decode IosPaths where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIosPaths :: Encode IosPaths where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a device.</p>
newtype Job = Job 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Type'" :: NullOrUndefined.NullOrUndefined (TestType)
  , "Created'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Status'" :: NullOrUndefined.NullOrUndefined (ExecutionStatus)
  , "Result'" :: NullOrUndefined.NullOrUndefined (ExecutionResult)
  , "Started'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Stopped'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Counters'" :: NullOrUndefined.NullOrUndefined (Counters)
  , "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Device'" :: NullOrUndefined.NullOrUndefined (Device)
  , "DeviceMinutes'" :: NullOrUndefined.NullOrUndefined (DeviceMinutes)
  }
derive instance newtypeJob :: Newtype Job _
derive instance repGenericJob :: Generic Job _
instance showJob :: Show Job where
  show = genericShow
instance decodeJob :: Decode Job where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJob :: Encode Job where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobTimeoutMinutes = JobTimeoutMinutes Int
derive instance newtypeJobTimeoutMinutes :: Newtype JobTimeoutMinutes _
derive instance repGenericJobTimeoutMinutes :: Generic JobTimeoutMinutes _
instance showJobTimeoutMinutes :: Show JobTimeoutMinutes where
  show = genericShow
instance decodeJobTimeoutMinutes :: Decode JobTimeoutMinutes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobTimeoutMinutes :: Encode JobTimeoutMinutes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Jobs = Jobs (Array Job)
derive instance newtypeJobs :: Newtype Jobs _
derive instance repGenericJobs :: Generic Jobs _
instance showJobs :: Show Jobs where
  show = genericShow
instance decodeJobs :: Decode Jobs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobs :: Encode Jobs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A limit was exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the list artifacts operation.</p>
newtype ListArtifactsRequest = ListArtifactsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Type'" :: (ArtifactCategory)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListArtifactsRequest :: Newtype ListArtifactsRequest _
derive instance repGenericListArtifactsRequest :: Generic ListArtifactsRequest _
instance showListArtifactsRequest :: Show ListArtifactsRequest where
  show = genericShow
instance decodeListArtifactsRequest :: Decode ListArtifactsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListArtifactsRequest :: Encode ListArtifactsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list artifacts operation.</p>
newtype ListArtifactsResult = ListArtifactsResult 
  { "Artifacts'" :: NullOrUndefined.NullOrUndefined (Artifacts)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListArtifactsResult :: Newtype ListArtifactsResult _
derive instance repGenericListArtifactsResult :: Generic ListArtifactsResult _
instance showListArtifactsResult :: Show ListArtifactsResult where
  show = genericShow
instance decodeListArtifactsResult :: Decode ListArtifactsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListArtifactsResult :: Encode ListArtifactsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list device pools request.</p>
newtype ListDevicePoolsRequest = ListDevicePoolsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Type'" :: NullOrUndefined.NullOrUndefined (DevicePoolType)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListDevicePoolsRequest :: Newtype ListDevicePoolsRequest _
derive instance repGenericListDevicePoolsRequest :: Generic ListDevicePoolsRequest _
instance showListDevicePoolsRequest :: Show ListDevicePoolsRequest where
  show = genericShow
instance decodeListDevicePoolsRequest :: Decode ListDevicePoolsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDevicePoolsRequest :: Encode ListDevicePoolsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list device pools request.</p>
newtype ListDevicePoolsResult = ListDevicePoolsResult 
  { "DevicePools'" :: NullOrUndefined.NullOrUndefined (DevicePools)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListDevicePoolsResult :: Newtype ListDevicePoolsResult _
derive instance repGenericListDevicePoolsResult :: Generic ListDevicePoolsResult _
instance showListDevicePoolsResult :: Show ListDevicePoolsResult where
  show = genericShow
instance decodeListDevicePoolsResult :: Decode ListDevicePoolsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDevicePoolsResult :: Encode ListDevicePoolsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list devices request.</p>
newtype ListDevicesRequest = ListDevicesRequest 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListDevicesRequest :: Newtype ListDevicesRequest _
derive instance repGenericListDevicesRequest :: Generic ListDevicesRequest _
instance showListDevicesRequest :: Show ListDevicesRequest where
  show = genericShow
instance decodeListDevicesRequest :: Decode ListDevicesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDevicesRequest :: Encode ListDevicesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list devices operation.</p>
newtype ListDevicesResult = ListDevicesResult 
  { "Devices'" :: NullOrUndefined.NullOrUndefined (Devices)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListDevicesResult :: Newtype ListDevicesResult _
derive instance repGenericListDevicesResult :: Generic ListDevicesResult _
instance showListDevicesResult :: Show ListDevicesResult where
  show = genericShow
instance decodeListDevicesResult :: Decode ListDevicesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDevicesResult :: Encode ListDevicesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the list jobs operation.</p>
newtype ListJobsRequest = ListJobsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListJobsRequest :: Newtype ListJobsRequest _
derive instance repGenericListJobsRequest :: Generic ListJobsRequest _
instance showListJobsRequest :: Show ListJobsRequest where
  show = genericShow
instance decodeListJobsRequest :: Decode ListJobsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsRequest :: Encode ListJobsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list jobs request.</p>
newtype ListJobsResult = ListJobsResult 
  { "Jobs'" :: NullOrUndefined.NullOrUndefined (Jobs)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListJobsResult :: Newtype ListJobsResult _
derive instance repGenericListJobsResult :: Generic ListJobsResult _
instance showListJobsResult :: Show ListJobsResult where
  show = genericShow
instance decodeListJobsResult :: Decode ListJobsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsResult :: Encode ListJobsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListNetworkProfilesRequest = ListNetworkProfilesRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Type'" :: NullOrUndefined.NullOrUndefined (NetworkProfileType)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListNetworkProfilesRequest :: Newtype ListNetworkProfilesRequest _
derive instance repGenericListNetworkProfilesRequest :: Generic ListNetworkProfilesRequest _
instance showListNetworkProfilesRequest :: Show ListNetworkProfilesRequest where
  show = genericShow
instance decodeListNetworkProfilesRequest :: Decode ListNetworkProfilesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListNetworkProfilesRequest :: Encode ListNetworkProfilesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListNetworkProfilesResult = ListNetworkProfilesResult 
  { "NetworkProfiles'" :: NullOrUndefined.NullOrUndefined (NetworkProfiles)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListNetworkProfilesResult :: Newtype ListNetworkProfilesResult _
derive instance repGenericListNetworkProfilesResult :: Generic ListNetworkProfilesResult _
instance showListNetworkProfilesResult :: Show ListNetworkProfilesResult where
  show = genericShow
instance decodeListNetworkProfilesResult :: Decode ListNetworkProfilesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListNetworkProfilesResult :: Encode ListNetworkProfilesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfferingPromotionsRequest = ListOfferingPromotionsRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListOfferingPromotionsRequest :: Newtype ListOfferingPromotionsRequest _
derive instance repGenericListOfferingPromotionsRequest :: Generic ListOfferingPromotionsRequest _
instance showListOfferingPromotionsRequest :: Show ListOfferingPromotionsRequest where
  show = genericShow
instance decodeListOfferingPromotionsRequest :: Decode ListOfferingPromotionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfferingPromotionsRequest :: Encode ListOfferingPromotionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfferingPromotionsResult = ListOfferingPromotionsResult 
  { "OfferingPromotions'" :: NullOrUndefined.NullOrUndefined (OfferingPromotions)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListOfferingPromotionsResult :: Newtype ListOfferingPromotionsResult _
derive instance repGenericListOfferingPromotionsResult :: Generic ListOfferingPromotionsResult _
instance showListOfferingPromotionsResult :: Show ListOfferingPromotionsResult where
  show = genericShow
instance decodeListOfferingPromotionsResult :: Decode ListOfferingPromotionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfferingPromotionsResult :: Encode ListOfferingPromotionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to list the offering transaction history.</p>
newtype ListOfferingTransactionsRequest = ListOfferingTransactionsRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListOfferingTransactionsRequest :: Newtype ListOfferingTransactionsRequest _
derive instance repGenericListOfferingTransactionsRequest :: Generic ListOfferingTransactionsRequest _
instance showListOfferingTransactionsRequest :: Show ListOfferingTransactionsRequest where
  show = genericShow
instance decodeListOfferingTransactionsRequest :: Decode ListOfferingTransactionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfferingTransactionsRequest :: Encode ListOfferingTransactionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the transaction log of the specified offerings.</p>
newtype ListOfferingTransactionsResult = ListOfferingTransactionsResult 
  { "OfferingTransactions'" :: NullOrUndefined.NullOrUndefined (OfferingTransactions)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListOfferingTransactionsResult :: Newtype ListOfferingTransactionsResult _
derive instance repGenericListOfferingTransactionsResult :: Generic ListOfferingTransactionsResult _
instance showListOfferingTransactionsResult :: Show ListOfferingTransactionsResult where
  show = genericShow
instance decodeListOfferingTransactionsResult :: Decode ListOfferingTransactionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfferingTransactionsResult :: Encode ListOfferingTransactionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to list all offerings.</p>
newtype ListOfferingsRequest = ListOfferingsRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListOfferingsRequest :: Newtype ListOfferingsRequest _
derive instance repGenericListOfferingsRequest :: Generic ListOfferingsRequest _
instance showListOfferingsRequest :: Show ListOfferingsRequest where
  show = genericShow
instance decodeListOfferingsRequest :: Decode ListOfferingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfferingsRequest :: Encode ListOfferingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the return values of the list of offerings.</p>
newtype ListOfferingsResult = ListOfferingsResult 
  { "Offerings'" :: NullOrUndefined.NullOrUndefined (Offerings)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListOfferingsResult :: Newtype ListOfferingsResult _
derive instance repGenericListOfferingsResult :: Generic ListOfferingsResult _
instance showListOfferingsResult :: Show ListOfferingsResult where
  show = genericShow
instance decodeListOfferingsResult :: Decode ListOfferingsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfferingsResult :: Encode ListOfferingsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the list projects operation.</p>
newtype ListProjectsRequest = ListProjectsRequest 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListProjectsRequest :: Newtype ListProjectsRequest _
derive instance repGenericListProjectsRequest :: Generic ListProjectsRequest _
instance showListProjectsRequest :: Show ListProjectsRequest where
  show = genericShow
instance decodeListProjectsRequest :: Decode ListProjectsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListProjectsRequest :: Encode ListProjectsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list projects request.</p>
newtype ListProjectsResult = ListProjectsResult 
  { "Projects'" :: NullOrUndefined.NullOrUndefined (Projects)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListProjectsResult :: Newtype ListProjectsResult _
derive instance repGenericListProjectsResult :: Generic ListProjectsResult _
instance showListProjectsResult :: Show ListProjectsResult where
  show = genericShow
instance decodeListProjectsResult :: Decode ListProjectsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListProjectsResult :: Encode ListProjectsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to return information about the remote access session.</p>
newtype ListRemoteAccessSessionsRequest = ListRemoteAccessSessionsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListRemoteAccessSessionsRequest :: Newtype ListRemoteAccessSessionsRequest _
derive instance repGenericListRemoteAccessSessionsRequest :: Generic ListRemoteAccessSessionsRequest _
instance showListRemoteAccessSessionsRequest :: Show ListRemoteAccessSessionsRequest where
  show = genericShow
instance decodeListRemoteAccessSessionsRequest :: Decode ListRemoteAccessSessionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRemoteAccessSessionsRequest :: Encode ListRemoteAccessSessionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server after AWS Device Farm makes a request to return information about the remote access session.</p>
newtype ListRemoteAccessSessionsResult = ListRemoteAccessSessionsResult 
  { "RemoteAccessSessions'" :: NullOrUndefined.NullOrUndefined (RemoteAccessSessions)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListRemoteAccessSessionsResult :: Newtype ListRemoteAccessSessionsResult _
derive instance repGenericListRemoteAccessSessionsResult :: Generic ListRemoteAccessSessionsResult _
instance showListRemoteAccessSessionsResult :: Show ListRemoteAccessSessionsResult where
  show = genericShow
instance decodeListRemoteAccessSessionsResult :: Decode ListRemoteAccessSessionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRemoteAccessSessionsResult :: Encode ListRemoteAccessSessionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the list runs operation.</p>
newtype ListRunsRequest = ListRunsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListRunsRequest :: Newtype ListRunsRequest _
derive instance repGenericListRunsRequest :: Generic ListRunsRequest _
instance showListRunsRequest :: Show ListRunsRequest where
  show = genericShow
instance decodeListRunsRequest :: Decode ListRunsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRunsRequest :: Encode ListRunsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list runs request.</p>
newtype ListRunsResult = ListRunsResult 
  { "Runs'" :: NullOrUndefined.NullOrUndefined (Runs)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListRunsResult :: Newtype ListRunsResult _
derive instance repGenericListRunsResult :: Generic ListRunsResult _
instance showListRunsResult :: Show ListRunsResult where
  show = genericShow
instance decodeListRunsResult :: Decode ListRunsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRunsResult :: Encode ListRunsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the list samples operation.</p>
newtype ListSamplesRequest = ListSamplesRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListSamplesRequest :: Newtype ListSamplesRequest _
derive instance repGenericListSamplesRequest :: Generic ListSamplesRequest _
instance showListSamplesRequest :: Show ListSamplesRequest where
  show = genericShow
instance decodeListSamplesRequest :: Decode ListSamplesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSamplesRequest :: Encode ListSamplesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list samples request.</p>
newtype ListSamplesResult = ListSamplesResult 
  { "Samples'" :: NullOrUndefined.NullOrUndefined (Samples)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListSamplesResult :: Newtype ListSamplesResult _
derive instance repGenericListSamplesResult :: Generic ListSamplesResult _
instance showListSamplesResult :: Show ListSamplesResult where
  show = genericShow
instance decodeListSamplesResult :: Decode ListSamplesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSamplesResult :: Encode ListSamplesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the list suites operation.</p>
newtype ListSuitesRequest = ListSuitesRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListSuitesRequest :: Newtype ListSuitesRequest _
derive instance repGenericListSuitesRequest :: Generic ListSuitesRequest _
instance showListSuitesRequest :: Show ListSuitesRequest where
  show = genericShow
instance decodeListSuitesRequest :: Decode ListSuitesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSuitesRequest :: Encode ListSuitesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list suites request.</p>
newtype ListSuitesResult = ListSuitesResult 
  { "Suites'" :: NullOrUndefined.NullOrUndefined (Suites)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListSuitesResult :: Newtype ListSuitesResult _
derive instance repGenericListSuitesResult :: Generic ListSuitesResult _
instance showListSuitesResult :: Show ListSuitesResult where
  show = genericShow
instance decodeListSuitesResult :: Decode ListSuitesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSuitesResult :: Encode ListSuitesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the list tests operation.</p>
newtype ListTestsRequest = ListTestsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListTestsRequest :: Newtype ListTestsRequest _
derive instance repGenericListTestsRequest :: Generic ListTestsRequest _
instance showListTestsRequest :: Show ListTestsRequest where
  show = genericShow
instance decodeListTestsRequest :: Decode ListTestsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTestsRequest :: Encode ListTestsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list tests request.</p>
newtype ListTestsResult = ListTestsResult 
  { "Tests'" :: NullOrUndefined.NullOrUndefined (Tests)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListTestsResult :: Newtype ListTestsResult _
derive instance repGenericListTestsResult :: Generic ListTestsResult _
instance showListTestsResult :: Show ListTestsResult where
  show = genericShow
instance decodeListTestsResult :: Decode ListTestsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTestsResult :: Encode ListTestsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the list unique problems operation.</p>
newtype ListUniqueProblemsRequest = ListUniqueProblemsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListUniqueProblemsRequest :: Newtype ListUniqueProblemsRequest _
derive instance repGenericListUniqueProblemsRequest :: Generic ListUniqueProblemsRequest _
instance showListUniqueProblemsRequest :: Show ListUniqueProblemsRequest where
  show = genericShow
instance decodeListUniqueProblemsRequest :: Decode ListUniqueProblemsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUniqueProblemsRequest :: Encode ListUniqueProblemsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list unique problems request.</p>
newtype ListUniqueProblemsResult = ListUniqueProblemsResult 
  { "UniqueProblems'" :: NullOrUndefined.NullOrUndefined (UniqueProblemsByExecutionResultMap)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListUniqueProblemsResult :: Newtype ListUniqueProblemsResult _
derive instance repGenericListUniqueProblemsResult :: Generic ListUniqueProblemsResult _
instance showListUniqueProblemsResult :: Show ListUniqueProblemsResult where
  show = genericShow
instance decodeListUniqueProblemsResult :: Decode ListUniqueProblemsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUniqueProblemsResult :: Encode ListUniqueProblemsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the list uploads operation.</p>
newtype ListUploadsRequest = ListUploadsRequest 
  { "Arn'" :: (AmazonResourceName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListUploadsRequest :: Newtype ListUploadsRequest _
derive instance repGenericListUploadsRequest :: Generic ListUploadsRequest _
instance showListUploadsRequest :: Show ListUploadsRequest where
  show = genericShow
instance decodeListUploadsRequest :: Decode ListUploadsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUploadsRequest :: Encode ListUploadsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a list uploads request.</p>
newtype ListUploadsResult = ListUploadsResult 
  { "Uploads'" :: NullOrUndefined.NullOrUndefined (Uploads)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListUploadsResult :: Newtype ListUploadsResult _
derive instance repGenericListUploadsResult :: Generic ListUploadsResult _
instance showListUploadsResult :: Show ListUploadsResult where
  show = genericShow
instance decodeListUploadsResult :: Decode ListUploadsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUploadsResult :: Encode ListUploadsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a latitude and longitude pair, expressed in geographic coordinate system degrees (for example 47.6204, -122.3491).</p> <p>Elevation is currently not supported.</p>
newtype Location = Location 
  { "Latitude'" :: (Number)
  , "Longitude'" :: (Number)
  }
derive instance newtypeLocation :: Newtype Location _
derive instance repGenericLocation :: Generic Location _
instance showLocation :: Show Location where
  show = genericShow
instance decodeLocation :: Decode Location where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocation :: Encode Location where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxSlotMap = MaxSlotMap (StrMap.StrMap Int)
derive instance newtypeMaxSlotMap :: Newtype MaxSlotMap _
derive instance repGenericMaxSlotMap :: Generic MaxSlotMap _
instance showMaxSlotMap :: Show MaxSlotMap where
  show = genericShow
instance decodeMaxSlotMap :: Decode MaxSlotMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxSlotMap :: Encode MaxSlotMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Metadata = Metadata String
derive instance newtypeMetadata :: Newtype Metadata _
derive instance repGenericMetadata :: Generic Metadata _
instance showMetadata :: Show Metadata where
  show = genericShow
instance decodeMetadata :: Decode Metadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetadata :: Encode Metadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A number representing the monetary amount for an offering or transaction.</p>
newtype MonetaryAmount = MonetaryAmount 
  { "Amount'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CurrencyCode'" :: NullOrUndefined.NullOrUndefined (CurrencyCode)
  }
derive instance newtypeMonetaryAmount :: Newtype MonetaryAmount _
derive instance repGenericMonetaryAmount :: Generic MonetaryAmount _
instance showMonetaryAmount :: Show MonetaryAmount where
  show = genericShow
instance decodeMonetaryAmount :: Decode MonetaryAmount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMonetaryAmount :: Encode MonetaryAmount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive instance repGenericName :: Generic Name _
instance showName :: Show Name where
  show = genericShow
instance decodeName :: Decode Name where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeName :: Encode Name where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An array of settings that describes characteristics of a network profile.</p>
newtype NetworkProfile = NetworkProfile 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined.NullOrUndefined (NetworkProfileType)
  , "UplinkBandwidthBits'" :: NullOrUndefined.NullOrUndefined (Number)
  , "DownlinkBandwidthBits'" :: NullOrUndefined.NullOrUndefined (Number)
  , "UplinkDelayMs'" :: NullOrUndefined.NullOrUndefined (Number)
  , "DownlinkDelayMs'" :: NullOrUndefined.NullOrUndefined (Number)
  , "UplinkJitterMs'" :: NullOrUndefined.NullOrUndefined (Number)
  , "DownlinkJitterMs'" :: NullOrUndefined.NullOrUndefined (Number)
  , "UplinkLossPercent'" :: NullOrUndefined.NullOrUndefined (PercentInteger)
  , "DownlinkLossPercent'" :: NullOrUndefined.NullOrUndefined (PercentInteger)
  }
derive instance newtypeNetworkProfile :: Newtype NetworkProfile _
derive instance repGenericNetworkProfile :: Generic NetworkProfile _
instance showNetworkProfile :: Show NetworkProfile where
  show = genericShow
instance decodeNetworkProfile :: Decode NetworkProfile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkProfile :: Encode NetworkProfile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NetworkProfileType = NetworkProfileType String
derive instance newtypeNetworkProfileType :: Newtype NetworkProfileType _
derive instance repGenericNetworkProfileType :: Generic NetworkProfileType _
instance showNetworkProfileType :: Show NetworkProfileType where
  show = genericShow
instance decodeNetworkProfileType :: Decode NetworkProfileType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkProfileType :: Encode NetworkProfileType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NetworkProfiles = NetworkProfiles (Array NetworkProfile)
derive instance newtypeNetworkProfiles :: Newtype NetworkProfiles _
derive instance repGenericNetworkProfiles :: Generic NetworkProfiles _
instance showNetworkProfiles :: Show NetworkProfiles where
  show = genericShow
instance decodeNetworkProfiles :: Decode NetworkProfiles where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkProfiles :: Encode NetworkProfiles where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Exception gets thrown when a user is not eligible to perform the specified transaction.</p>
newtype NotEligibleException = NotEligibleException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeNotEligibleException :: Newtype NotEligibleException _
derive instance repGenericNotEligibleException :: Generic NotEligibleException _
instance showNotEligibleException :: Show NotEligibleException where
  show = genericShow
instance decodeNotEligibleException :: Decode NotEligibleException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotEligibleException :: Encode NotEligibleException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified entity was not found.</p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the metadata of a device offering.</p>
newtype Offering = Offering 
  { "Id'" :: NullOrUndefined.NullOrUndefined (OfferingIdentifier)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined.NullOrUndefined (OfferingType)
  , "Platform'" :: NullOrUndefined.NullOrUndefined (DevicePlatform)
  , "RecurringCharges'" :: NullOrUndefined.NullOrUndefined (RecurringCharges)
  }
derive instance newtypeOffering :: Newtype Offering _
derive instance repGenericOffering :: Generic Offering _
instance showOffering :: Show Offering where
  show = genericShow
instance decodeOffering :: Decode Offering where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOffering :: Encode Offering where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OfferingIdentifier = OfferingIdentifier String
derive instance newtypeOfferingIdentifier :: Newtype OfferingIdentifier _
derive instance repGenericOfferingIdentifier :: Generic OfferingIdentifier _
instance showOfferingIdentifier :: Show OfferingIdentifier where
  show = genericShow
instance decodeOfferingIdentifier :: Decode OfferingIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOfferingIdentifier :: Encode OfferingIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about an offering promotion.</p>
newtype OfferingPromotion = OfferingPromotion 
  { "Id'" :: NullOrUndefined.NullOrUndefined (OfferingPromotionIdentifier)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeOfferingPromotion :: Newtype OfferingPromotion _
derive instance repGenericOfferingPromotion :: Generic OfferingPromotion _
instance showOfferingPromotion :: Show OfferingPromotion where
  show = genericShow
instance decodeOfferingPromotion :: Decode OfferingPromotion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOfferingPromotion :: Encode OfferingPromotion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OfferingPromotionIdentifier = OfferingPromotionIdentifier String
derive instance newtypeOfferingPromotionIdentifier :: Newtype OfferingPromotionIdentifier _
derive instance repGenericOfferingPromotionIdentifier :: Generic OfferingPromotionIdentifier _
instance showOfferingPromotionIdentifier :: Show OfferingPromotionIdentifier where
  show = genericShow
instance decodeOfferingPromotionIdentifier :: Decode OfferingPromotionIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOfferingPromotionIdentifier :: Encode OfferingPromotionIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OfferingPromotions = OfferingPromotions (Array OfferingPromotion)
derive instance newtypeOfferingPromotions :: Newtype OfferingPromotions _
derive instance repGenericOfferingPromotions :: Generic OfferingPromotions _
instance showOfferingPromotions :: Show OfferingPromotions where
  show = genericShow
instance decodeOfferingPromotions :: Decode OfferingPromotions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOfferingPromotions :: Encode OfferingPromotions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The status of the offering.</p>
newtype OfferingStatus = OfferingStatus 
  { "Type'" :: NullOrUndefined.NullOrUndefined (OfferingTransactionType)
  , "Offering'" :: NullOrUndefined.NullOrUndefined (Offering)
  , "Quantity'" :: NullOrUndefined.NullOrUndefined (Int)
  , "EffectiveOn'" :: NullOrUndefined.NullOrUndefined (DateTime)
  }
derive instance newtypeOfferingStatus :: Newtype OfferingStatus _
derive instance repGenericOfferingStatus :: Generic OfferingStatus _
instance showOfferingStatus :: Show OfferingStatus where
  show = genericShow
instance decodeOfferingStatus :: Decode OfferingStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOfferingStatus :: Encode OfferingStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OfferingStatusMap = OfferingStatusMap (StrMap.StrMap OfferingStatus)
derive instance newtypeOfferingStatusMap :: Newtype OfferingStatusMap _
derive instance repGenericOfferingStatusMap :: Generic OfferingStatusMap _
instance showOfferingStatusMap :: Show OfferingStatusMap where
  show = genericShow
instance decodeOfferingStatusMap :: Decode OfferingStatusMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOfferingStatusMap :: Encode OfferingStatusMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the metadata of an offering transaction.</p>
newtype OfferingTransaction = OfferingTransaction 
  { "OfferingStatus'" :: NullOrUndefined.NullOrUndefined (OfferingStatus)
  , "TransactionId'" :: NullOrUndefined.NullOrUndefined (TransactionIdentifier)
  , "OfferingPromotionId'" :: NullOrUndefined.NullOrUndefined (OfferingPromotionIdentifier)
  , "CreatedOn'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Cost'" :: NullOrUndefined.NullOrUndefined (MonetaryAmount)
  }
derive instance newtypeOfferingTransaction :: Newtype OfferingTransaction _
derive instance repGenericOfferingTransaction :: Generic OfferingTransaction _
instance showOfferingTransaction :: Show OfferingTransaction where
  show = genericShow
instance decodeOfferingTransaction :: Decode OfferingTransaction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOfferingTransaction :: Encode OfferingTransaction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OfferingTransactionType = OfferingTransactionType String
derive instance newtypeOfferingTransactionType :: Newtype OfferingTransactionType _
derive instance repGenericOfferingTransactionType :: Generic OfferingTransactionType _
instance showOfferingTransactionType :: Show OfferingTransactionType where
  show = genericShow
instance decodeOfferingTransactionType :: Decode OfferingTransactionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOfferingTransactionType :: Encode OfferingTransactionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OfferingTransactions = OfferingTransactions (Array OfferingTransaction)
derive instance newtypeOfferingTransactions :: Newtype OfferingTransactions _
derive instance repGenericOfferingTransactions :: Generic OfferingTransactions _
instance showOfferingTransactions :: Show OfferingTransactions where
  show = genericShow
instance decodeOfferingTransactions :: Decode OfferingTransactions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOfferingTransactions :: Encode OfferingTransactions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OfferingType = OfferingType String
derive instance newtypeOfferingType :: Newtype OfferingType _
derive instance repGenericOfferingType :: Generic OfferingType _
instance showOfferingType :: Show OfferingType where
  show = genericShow
instance decodeOfferingType :: Decode OfferingType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOfferingType :: Encode OfferingType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Offerings = Offerings (Array Offering)
derive instance newtypeOfferings :: Newtype Offerings _
derive instance repGenericOfferings :: Generic Offerings _
instance showOfferings :: Show Offerings where
  show = genericShow
instance decodeOfferings :: Decode Offerings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOfferings :: Encode Offerings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _
derive instance repGenericPaginationToken :: Generic PaginationToken _
instance showPaginationToken :: Show PaginationToken where
  show = genericShow
instance decodePaginationToken :: Decode PaginationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaginationToken :: Encode PaginationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PercentInteger = PercentInteger Int
derive instance newtypePercentInteger :: Newtype PercentInteger _
derive instance repGenericPercentInteger :: Generic PercentInteger _
instance showPercentInteger :: Show PercentInteger where
  show = genericShow
instance decodePercentInteger :: Decode PercentInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePercentInteger :: Encode PercentInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a specific warning or failure.</p>
newtype Problem = Problem 
  { "Run'" :: NullOrUndefined.NullOrUndefined (ProblemDetail)
  , "Job'" :: NullOrUndefined.NullOrUndefined (ProblemDetail)
  , "Suite'" :: NullOrUndefined.NullOrUndefined (ProblemDetail)
  , "Test'" :: NullOrUndefined.NullOrUndefined (ProblemDetail)
  , "Device'" :: NullOrUndefined.NullOrUndefined (Device)
  , "Result'" :: NullOrUndefined.NullOrUndefined (ExecutionResult)
  , "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeProblem :: Newtype Problem _
derive instance repGenericProblem :: Generic Problem _
instance showProblem :: Show Problem where
  show = genericShow
instance decodeProblem :: Decode Problem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProblem :: Encode Problem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a problem detail.</p>
newtype ProblemDetail = ProblemDetail 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  }
derive instance newtypeProblemDetail :: Newtype ProblemDetail _
derive instance repGenericProblemDetail :: Generic ProblemDetail _
instance showProblemDetail :: Show ProblemDetail where
  show = genericShow
instance decodeProblemDetail :: Decode ProblemDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProblemDetail :: Encode ProblemDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Problems = Problems (Array Problem)
derive instance newtypeProblems :: Newtype Problems _
derive instance repGenericProblems :: Generic Problems _
instance showProblems :: Show Problems where
  show = genericShow
instance decodeProblems :: Decode Problems where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProblems :: Encode Problems where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an operating-system neutral workspace for running and managing tests.</p>
newtype Project = Project 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "DefaultJobTimeoutMinutes'" :: NullOrUndefined.NullOrUndefined (JobTimeoutMinutes)
  , "Created'" :: NullOrUndefined.NullOrUndefined (DateTime)
  }
derive instance newtypeProject :: Newtype Project _
derive instance repGenericProject :: Generic Project _
instance showProject :: Show Project where
  show = genericShow
instance decodeProject :: Decode Project where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProject :: Encode Project where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Projects = Projects (Array Project)
derive instance newtypeProjects :: Newtype Projects _
derive instance repGenericProjects :: Generic Projects _
instance showProjects :: Show Projects where
  show = genericShow
instance decodeProjects :: Decode Projects where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjects :: Encode Projects where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request for a purchase offering.</p>
newtype PurchaseOfferingRequest = PurchaseOfferingRequest 
  { "OfferingId'" :: NullOrUndefined.NullOrUndefined (OfferingIdentifier)
  , "Quantity'" :: NullOrUndefined.NullOrUndefined (Int)
  , "OfferingPromotionId'" :: NullOrUndefined.NullOrUndefined (OfferingPromotionIdentifier)
  }
derive instance newtypePurchaseOfferingRequest :: Newtype PurchaseOfferingRequest _
derive instance repGenericPurchaseOfferingRequest :: Generic PurchaseOfferingRequest _
instance showPurchaseOfferingRequest :: Show PurchaseOfferingRequest where
  show = genericShow
instance decodePurchaseOfferingRequest :: Decode PurchaseOfferingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurchaseOfferingRequest :: Encode PurchaseOfferingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of the purchase offering (e.g., success or failure).</p>
newtype PurchaseOfferingResult = PurchaseOfferingResult 
  { "OfferingTransaction'" :: NullOrUndefined.NullOrUndefined (OfferingTransaction)
  }
derive instance newtypePurchaseOfferingResult :: Newtype PurchaseOfferingResult _
derive instance repGenericPurchaseOfferingResult :: Generic PurchaseOfferingResult _
instance showPurchaseOfferingResult :: Show PurchaseOfferingResult where
  show = genericShow
instance decodePurchaseOfferingResult :: Decode PurchaseOfferingResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurchaseOfferingResult :: Encode PurchaseOfferingResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PurchasedDevicesMap = PurchasedDevicesMap (StrMap.StrMap Int)
derive instance newtypePurchasedDevicesMap :: Newtype PurchasedDevicesMap _
derive instance repGenericPurchasedDevicesMap :: Generic PurchasedDevicesMap _
instance showPurchasedDevicesMap :: Show PurchasedDevicesMap where
  show = genericShow
instance decodePurchasedDevicesMap :: Decode PurchasedDevicesMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurchasedDevicesMap :: Encode PurchasedDevicesMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the set of radios and their states on a device. Examples of radios include Wi-Fi, GPS, Bluetooth, and NFC.</p>
newtype Radios = Radios 
  { "Wifi'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Bluetooth'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Nfc'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Gps'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeRadios :: Newtype Radios _
derive instance repGenericRadios :: Generic Radios _
instance showRadios :: Show Radios where
  show = genericShow
instance decodeRadios :: Decode Radios where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRadios :: Encode Radios where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies whether charges for devices will be recurring.</p>
newtype RecurringCharge = RecurringCharge 
  { "Cost'" :: NullOrUndefined.NullOrUndefined (MonetaryAmount)
  , "Frequency'" :: NullOrUndefined.NullOrUndefined (RecurringChargeFrequency)
  }
derive instance newtypeRecurringCharge :: Newtype RecurringCharge _
derive instance repGenericRecurringCharge :: Generic RecurringCharge _
instance showRecurringCharge :: Show RecurringCharge where
  show = genericShow
instance decodeRecurringCharge :: Decode RecurringCharge where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecurringCharge :: Encode RecurringCharge where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecurringChargeFrequency = RecurringChargeFrequency String
derive instance newtypeRecurringChargeFrequency :: Newtype RecurringChargeFrequency _
derive instance repGenericRecurringChargeFrequency :: Generic RecurringChargeFrequency _
instance showRecurringChargeFrequency :: Show RecurringChargeFrequency where
  show = genericShow
instance decodeRecurringChargeFrequency :: Decode RecurringChargeFrequency where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecurringChargeFrequency :: Encode RecurringChargeFrequency where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecurringCharges = RecurringCharges (Array RecurringCharge)
derive instance newtypeRecurringCharges :: Newtype RecurringCharges _
derive instance repGenericRecurringCharges :: Generic RecurringCharges _
instance showRecurringCharges :: Show RecurringCharges where
  show = genericShow
instance decodeRecurringCharges :: Decode RecurringCharges where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecurringCharges :: Encode RecurringCharges where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the remote access session.</p>
newtype RemoteAccessSession = RemoteAccessSession 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Created'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Status'" :: NullOrUndefined.NullOrUndefined (ExecutionStatus)
  , "Result'" :: NullOrUndefined.NullOrUndefined (ExecutionResult)
  , "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Started'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Stopped'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Device'" :: NullOrUndefined.NullOrUndefined (Device)
  , "RemoteDebugEnabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RemoteRecordEnabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RemoteRecordAppArn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "HostAddress'" :: NullOrUndefined.NullOrUndefined (HostAddress)
  , "ClientId'" :: NullOrUndefined.NullOrUndefined (ClientId)
  , "BillingMethod'" :: NullOrUndefined.NullOrUndefined (BillingMethod)
  , "DeviceMinutes'" :: NullOrUndefined.NullOrUndefined (DeviceMinutes)
  , "Endpoint'" :: NullOrUndefined.NullOrUndefined (String)
  , "DeviceUdid'" :: NullOrUndefined.NullOrUndefined (String)
  , "InteractionMode'" :: NullOrUndefined.NullOrUndefined (InteractionMode)
  }
derive instance newtypeRemoteAccessSession :: Newtype RemoteAccessSession _
derive instance repGenericRemoteAccessSession :: Generic RemoteAccessSession _
instance showRemoteAccessSession :: Show RemoteAccessSession where
  show = genericShow
instance decodeRemoteAccessSession :: Decode RemoteAccessSession where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoteAccessSession :: Encode RemoteAccessSession where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoteAccessSessions = RemoteAccessSessions (Array RemoteAccessSession)
derive instance newtypeRemoteAccessSessions :: Newtype RemoteAccessSessions _
derive instance repGenericRemoteAccessSessions :: Generic RemoteAccessSessions _
instance showRemoteAccessSessions :: Show RemoteAccessSessions where
  show = genericShow
instance decodeRemoteAccessSessions :: Decode RemoteAccessSessions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoteAccessSessions :: Encode RemoteAccessSessions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request representing an offering renewal.</p>
newtype RenewOfferingRequest = RenewOfferingRequest 
  { "OfferingId'" :: NullOrUndefined.NullOrUndefined (OfferingIdentifier)
  , "Quantity'" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeRenewOfferingRequest :: Newtype RenewOfferingRequest _
derive instance repGenericRenewOfferingRequest :: Generic RenewOfferingRequest _
instance showRenewOfferingRequest :: Show RenewOfferingRequest where
  show = genericShow
instance decodeRenewOfferingRequest :: Decode RenewOfferingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRenewOfferingRequest :: Encode RenewOfferingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a renewal offering.</p>
newtype RenewOfferingResult = RenewOfferingResult 
  { "OfferingTransaction'" :: NullOrUndefined.NullOrUndefined (OfferingTransaction)
  }
derive instance newtypeRenewOfferingResult :: Newtype RenewOfferingResult _
derive instance repGenericRenewOfferingResult :: Generic RenewOfferingResult _
instance showRenewOfferingResult :: Show RenewOfferingResult where
  show = genericShow
instance decodeRenewOfferingResult :: Decode RenewOfferingResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRenewOfferingResult :: Encode RenewOfferingResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the screen resolution of a device in height and width, expressed in pixels.</p>
newtype Resolution = Resolution 
  { "Width'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Height'" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeResolution :: Newtype Resolution _
derive instance repGenericResolution :: Generic Resolution _
instance showResolution :: Show Resolution where
  show = genericShow
instance decodeResolution :: Decode Resolution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResolution :: Encode Resolution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a condition for a device pool.</p>
newtype Rule = Rule 
  { "Attribute'" :: NullOrUndefined.NullOrUndefined (DeviceAttribute)
  , "Operator'" :: NullOrUndefined.NullOrUndefined (RuleOperator)
  , "Value'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRule :: Newtype Rule _
derive instance repGenericRule :: Generic Rule _
instance showRule :: Show Rule where
  show = genericShow
instance decodeRule :: Decode Rule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRule :: Encode Rule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleOperator = RuleOperator String
derive instance newtypeRuleOperator :: Newtype RuleOperator _
derive instance repGenericRuleOperator :: Generic RuleOperator _
instance showRuleOperator :: Show RuleOperator where
  show = genericShow
instance decodeRuleOperator :: Decode RuleOperator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleOperator :: Encode RuleOperator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Rules = Rules (Array Rule)
derive instance newtypeRules :: Newtype Rules _
derive instance repGenericRules :: Generic Rules _
instance showRules :: Show Rules where
  show = genericShow
instance decodeRules :: Decode Rules where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRules :: Encode Rules where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a test run on a set of devices with a given app package, test parameters, etc.</p>
newtype Run = Run 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Type'" :: NullOrUndefined.NullOrUndefined (TestType)
  , "Platform'" :: NullOrUndefined.NullOrUndefined (DevicePlatform)
  , "Created'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Status'" :: NullOrUndefined.NullOrUndefined (ExecutionStatus)
  , "Result'" :: NullOrUndefined.NullOrUndefined (ExecutionResult)
  , "Started'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Stopped'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Counters'" :: NullOrUndefined.NullOrUndefined (Counters)
  , "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  , "TotalJobs'" :: NullOrUndefined.NullOrUndefined (Int)
  , "CompletedJobs'" :: NullOrUndefined.NullOrUndefined (Int)
  , "BillingMethod'" :: NullOrUndefined.NullOrUndefined (BillingMethod)
  , "DeviceMinutes'" :: NullOrUndefined.NullOrUndefined (DeviceMinutes)
  , "NetworkProfile'" :: NullOrUndefined.NullOrUndefined (NetworkProfile)
  , "ParsingResultUrl'" :: NullOrUndefined.NullOrUndefined (String)
  , "ResultCode'" :: NullOrUndefined.NullOrUndefined (ExecutionResultCode)
  , "Seed'" :: NullOrUndefined.NullOrUndefined (Int)
  , "AppUpload'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "EventCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "JobTimeoutMinutes'" :: NullOrUndefined.NullOrUndefined (JobTimeoutMinutes)
  , "DevicePoolArn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Locale'" :: NullOrUndefined.NullOrUndefined (String)
  , "Radios'" :: NullOrUndefined.NullOrUndefined (Radios)
  , "Location'" :: NullOrUndefined.NullOrUndefined (Location)
  , "CustomerArtifactPaths'" :: NullOrUndefined.NullOrUndefined (CustomerArtifactPaths)
  , "WebUrl'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRun :: Newtype Run _
derive instance repGenericRun :: Generic Run _
instance showRun :: Show Run where
  show = genericShow
instance decodeRun :: Decode Run where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRun :: Encode Run where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Runs = Runs (Array Run)
derive instance newtypeRuns :: Newtype Runs _
derive instance repGenericRuns :: Generic Runs _
instance showRuns :: Show Runs where
  show = genericShow
instance decodeRuns :: Decode Runs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuns :: Encode Runs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a sample of performance data.</p>
newtype Sample = Sample 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Type'" :: NullOrUndefined.NullOrUndefined (SampleType)
  , "Url'" :: NullOrUndefined.NullOrUndefined (URL)
  }
derive instance newtypeSample :: Newtype Sample _
derive instance repGenericSample :: Generic Sample _
instance showSample :: Show Sample where
  show = genericShow
instance decodeSample :: Decode Sample where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSample :: Encode Sample where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SampleType = SampleType String
derive instance newtypeSampleType :: Newtype SampleType _
derive instance repGenericSampleType :: Generic SampleType _
instance showSampleType :: Show SampleType where
  show = genericShow
instance decodeSampleType :: Decode SampleType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSampleType :: Encode SampleType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Samples = Samples (Array Sample)
derive instance newtypeSamples :: Newtype Samples _
derive instance repGenericSamples :: Generic Samples _
instance showSamples :: Show Samples where
  show = genericShow
instance decodeSamples :: Decode Samples where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSamples :: Encode Samples where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the settings for a run. Includes things like location, radio states, auxiliary apps, and network profiles.</p>
newtype ScheduleRunConfiguration = ScheduleRunConfiguration 
  { "ExtraDataPackageArn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "NetworkProfileArn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Locale'" :: NullOrUndefined.NullOrUndefined (String)
  , "Location'" :: NullOrUndefined.NullOrUndefined (Location)
  , "CustomerArtifactPaths'" :: NullOrUndefined.NullOrUndefined (CustomerArtifactPaths)
  , "Radios'" :: NullOrUndefined.NullOrUndefined (Radios)
  , "AuxiliaryApps'" :: NullOrUndefined.NullOrUndefined (AmazonResourceNames)
  , "BillingMethod'" :: NullOrUndefined.NullOrUndefined (BillingMethod)
  }
derive instance newtypeScheduleRunConfiguration :: Newtype ScheduleRunConfiguration _
derive instance repGenericScheduleRunConfiguration :: Generic ScheduleRunConfiguration _
instance showScheduleRunConfiguration :: Show ScheduleRunConfiguration where
  show = genericShow
instance decodeScheduleRunConfiguration :: Decode ScheduleRunConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleRunConfiguration :: Encode ScheduleRunConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the schedule run operation.</p>
newtype ScheduleRunRequest = ScheduleRunRequest 
  { "ProjectArn'" :: (AmazonResourceName)
  , "AppArn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "DevicePoolArn'" :: (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Test'" :: (ScheduleRunTest)
  , "Configuration'" :: NullOrUndefined.NullOrUndefined (ScheduleRunConfiguration)
  , "ExecutionConfiguration'" :: NullOrUndefined.NullOrUndefined (ExecutionConfiguration)
  }
derive instance newtypeScheduleRunRequest :: Newtype ScheduleRunRequest _
derive instance repGenericScheduleRunRequest :: Generic ScheduleRunRequest _
instance showScheduleRunRequest :: Show ScheduleRunRequest where
  show = genericShow
instance decodeScheduleRunRequest :: Decode ScheduleRunRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleRunRequest :: Encode ScheduleRunRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of a schedule run request.</p>
newtype ScheduleRunResult = ScheduleRunResult 
  { "Run'" :: NullOrUndefined.NullOrUndefined (Run)
  }
derive instance newtypeScheduleRunResult :: Newtype ScheduleRunResult _
derive instance repGenericScheduleRunResult :: Generic ScheduleRunResult _
instance showScheduleRunResult :: Show ScheduleRunResult where
  show = genericShow
instance decodeScheduleRunResult :: Decode ScheduleRunResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleRunResult :: Encode ScheduleRunResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents additional test settings.</p>
newtype ScheduleRunTest = ScheduleRunTest 
  { "Type'" :: (TestType)
  , "TestPackageArn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Filter'" :: NullOrUndefined.NullOrUndefined (Filter)
  , "Parameters'" :: NullOrUndefined.NullOrUndefined (TestParameters)
  }
derive instance newtypeScheduleRunTest :: Newtype ScheduleRunTest _
derive instance repGenericScheduleRunTest :: Generic ScheduleRunTest _
instance showScheduleRunTest :: Show ScheduleRunTest where
  show = genericShow
instance decodeScheduleRunTest :: Decode ScheduleRunTest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleRunTest :: Encode ScheduleRunTest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There was a problem with the service account.</p>
newtype ServiceAccountException = ServiceAccountException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeServiceAccountException :: Newtype ServiceAccountException _
derive instance repGenericServiceAccountException :: Generic ServiceAccountException _
instance showServiceAccountException :: Show ServiceAccountException where
  show = genericShow
instance decodeServiceAccountException :: Decode ServiceAccountException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceAccountException :: Encode ServiceAccountException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SshPublicKey = SshPublicKey String
derive instance newtypeSshPublicKey :: Newtype SshPublicKey _
derive instance repGenericSshPublicKey :: Generic SshPublicKey _
instance showSshPublicKey :: Show SshPublicKey where
  show = genericShow
instance decodeSshPublicKey :: Decode SshPublicKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSshPublicKey :: Encode SshPublicKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to stop the remote access session.</p>
newtype StopRemoteAccessSessionRequest = StopRemoteAccessSessionRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeStopRemoteAccessSessionRequest :: Newtype StopRemoteAccessSessionRequest _
derive instance repGenericStopRemoteAccessSessionRequest :: Generic StopRemoteAccessSessionRequest _
instance showStopRemoteAccessSessionRequest :: Show StopRemoteAccessSessionRequest where
  show = genericShow
instance decodeStopRemoteAccessSessionRequest :: Decode StopRemoteAccessSessionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopRemoteAccessSessionRequest :: Encode StopRemoteAccessSessionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server that describes the remote access session when AWS Device Farm stops the session.</p>
newtype StopRemoteAccessSessionResult = StopRemoteAccessSessionResult 
  { "RemoteAccessSession'" :: NullOrUndefined.NullOrUndefined (RemoteAccessSession)
  }
derive instance newtypeStopRemoteAccessSessionResult :: Newtype StopRemoteAccessSessionResult _
derive instance repGenericStopRemoteAccessSessionResult :: Generic StopRemoteAccessSessionResult _
instance showStopRemoteAccessSessionResult :: Show StopRemoteAccessSessionResult where
  show = genericShow
instance decodeStopRemoteAccessSessionResult :: Decode StopRemoteAccessSessionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopRemoteAccessSessionResult :: Encode StopRemoteAccessSessionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to stop a specific run.</p>
newtype StopRunRequest = StopRunRequest 
  { "Arn'" :: (AmazonResourceName)
  }
derive instance newtypeStopRunRequest :: Newtype StopRunRequest _
derive instance repGenericStopRunRequest :: Generic StopRunRequest _
instance showStopRunRequest :: Show StopRunRequest where
  show = genericShow
instance decodeStopRunRequest :: Decode StopRunRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopRunRequest :: Encode StopRunRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the results of your stop run attempt.</p>
newtype StopRunResult = StopRunResult 
  { "Run'" :: NullOrUndefined.NullOrUndefined (Run)
  }
derive instance newtypeStopRunResult :: Newtype StopRunResult _
derive instance repGenericStopRunResult :: Generic StopRunResult _
instance showStopRunResult :: Show StopRunResult where
  show = genericShow
instance decodeStopRunResult :: Decode StopRunResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopRunResult :: Encode StopRunResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection of one or more tests.</p>
newtype Suite = Suite 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Type'" :: NullOrUndefined.NullOrUndefined (TestType)
  , "Created'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Status'" :: NullOrUndefined.NullOrUndefined (ExecutionStatus)
  , "Result'" :: NullOrUndefined.NullOrUndefined (ExecutionResult)
  , "Started'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Stopped'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Counters'" :: NullOrUndefined.NullOrUndefined (Counters)
  , "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  , "DeviceMinutes'" :: NullOrUndefined.NullOrUndefined (DeviceMinutes)
  }
derive instance newtypeSuite :: Newtype Suite _
derive instance repGenericSuite :: Generic Suite _
instance showSuite :: Show Suite where
  show = genericShow
instance decodeSuite :: Decode Suite where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSuite :: Encode Suite where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Suites = Suites (Array Suite)
derive instance newtypeSuites :: Newtype Suites _
derive instance repGenericSuites :: Generic Suites _
instance showSuites :: Show Suites where
  show = genericShow
instance decodeSuites :: Decode Suites where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSuites :: Encode Suites where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a condition that is evaluated.</p>
newtype Test = Test 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Type'" :: NullOrUndefined.NullOrUndefined (TestType)
  , "Created'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Status'" :: NullOrUndefined.NullOrUndefined (ExecutionStatus)
  , "Result'" :: NullOrUndefined.NullOrUndefined (ExecutionResult)
  , "Started'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Stopped'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Counters'" :: NullOrUndefined.NullOrUndefined (Counters)
  , "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  , "DeviceMinutes'" :: NullOrUndefined.NullOrUndefined (DeviceMinutes)
  }
derive instance newtypeTest :: Newtype Test _
derive instance repGenericTest :: Generic Test _
instance showTest :: Show Test where
  show = genericShow
instance decodeTest :: Decode Test where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTest :: Encode Test where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestParameters = TestParameters (StrMap.StrMap String)
derive instance newtypeTestParameters :: Newtype TestParameters _
derive instance repGenericTestParameters :: Generic TestParameters _
instance showTestParameters :: Show TestParameters where
  show = genericShow
instance decodeTestParameters :: Decode TestParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestParameters :: Encode TestParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestType = TestType String
derive instance newtypeTestType :: Newtype TestType _
derive instance repGenericTestType :: Generic TestType _
instance showTestType :: Show TestType where
  show = genericShow
instance decodeTestType :: Decode TestType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestType :: Encode TestType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tests = Tests (Array Test)
derive instance newtypeTests :: Newtype Tests _
derive instance repGenericTests :: Generic Tests _
instance showTests :: Show Tests where
  show = genericShow
instance decodeTests :: Decode Tests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTests :: Encode Tests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TransactionIdentifier = TransactionIdentifier String
derive instance newtypeTransactionIdentifier :: Newtype TransactionIdentifier _
derive instance repGenericTransactionIdentifier :: Generic TransactionIdentifier _
instance showTransactionIdentifier :: Show TransactionIdentifier where
  show = genericShow
instance decodeTransactionIdentifier :: Decode TransactionIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransactionIdentifier :: Encode TransactionIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about free trial device minutes for an AWS account.</p>
newtype TrialMinutes = TrialMinutes 
  { "Total'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Remaining'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeTrialMinutes :: Newtype TrialMinutes _
derive instance repGenericTrialMinutes :: Generic TrialMinutes _
instance showTrialMinutes :: Show TrialMinutes where
  show = genericShow
instance decodeTrialMinutes :: Decode TrialMinutes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrialMinutes :: Encode TrialMinutes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype URL = URL String
derive instance newtypeURL :: Newtype URL _
derive instance repGenericURL :: Generic URL _
instance showURL :: Show URL where
  show = genericShow
instance decodeURL :: Decode URL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeURL :: Encode URL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A collection of one or more problems, grouped by their result.</p>
newtype UniqueProblem = UniqueProblem 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Problems'" :: NullOrUndefined.NullOrUndefined (Problems)
  }
derive instance newtypeUniqueProblem :: Newtype UniqueProblem _
derive instance repGenericUniqueProblem :: Generic UniqueProblem _
instance showUniqueProblem :: Show UniqueProblem where
  show = genericShow
instance decodeUniqueProblem :: Decode UniqueProblem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUniqueProblem :: Encode UniqueProblem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UniqueProblems = UniqueProblems (Array UniqueProblem)
derive instance newtypeUniqueProblems :: Newtype UniqueProblems _
derive instance repGenericUniqueProblems :: Generic UniqueProblems _
instance showUniqueProblems :: Show UniqueProblems where
  show = genericShow
instance decodeUniqueProblems :: Decode UniqueProblems where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUniqueProblems :: Encode UniqueProblems where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UniqueProblemsByExecutionResultMap = UniqueProblemsByExecutionResultMap (StrMap.StrMap UniqueProblems)
derive instance newtypeUniqueProblemsByExecutionResultMap :: Newtype UniqueProblemsByExecutionResultMap _
derive instance repGenericUniqueProblemsByExecutionResultMap :: Generic UniqueProblemsByExecutionResultMap _
instance showUniqueProblemsByExecutionResultMap :: Show UniqueProblemsByExecutionResultMap where
  show = genericShow
instance decodeUniqueProblemsByExecutionResultMap :: Decode UniqueProblemsByExecutionResultMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUniqueProblemsByExecutionResultMap :: Encode UniqueProblemsByExecutionResultMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the update device pool operation.</p>
newtype UpdateDevicePoolRequest = UpdateDevicePoolRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Rules'" :: NullOrUndefined.NullOrUndefined (Rules)
  }
derive instance newtypeUpdateDevicePoolRequest :: Newtype UpdateDevicePoolRequest _
derive instance repGenericUpdateDevicePoolRequest :: Generic UpdateDevicePoolRequest _
instance showUpdateDevicePoolRequest :: Show UpdateDevicePoolRequest where
  show = genericShow
instance decodeUpdateDevicePoolRequest :: Decode UpdateDevicePoolRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDevicePoolRequest :: Encode UpdateDevicePoolRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of an update device pool request.</p>
newtype UpdateDevicePoolResult = UpdateDevicePoolResult 
  { "DevicePool'" :: NullOrUndefined.NullOrUndefined (DevicePool)
  }
derive instance newtypeUpdateDevicePoolResult :: Newtype UpdateDevicePoolResult _
derive instance repGenericUpdateDevicePoolResult :: Generic UpdateDevicePoolResult _
instance showUpdateDevicePoolResult :: Show UpdateDevicePoolResult where
  show = genericShow
instance decodeUpdateDevicePoolResult :: Decode UpdateDevicePoolResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDevicePoolResult :: Encode UpdateDevicePoolResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateNetworkProfileRequest = UpdateNetworkProfileRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Message)
  , "Type'" :: NullOrUndefined.NullOrUndefined (NetworkProfileType)
  , "UplinkBandwidthBits'" :: NullOrUndefined.NullOrUndefined (Number)
  , "DownlinkBandwidthBits'" :: NullOrUndefined.NullOrUndefined (Number)
  , "UplinkDelayMs'" :: NullOrUndefined.NullOrUndefined (Number)
  , "DownlinkDelayMs'" :: NullOrUndefined.NullOrUndefined (Number)
  , "UplinkJitterMs'" :: NullOrUndefined.NullOrUndefined (Number)
  , "DownlinkJitterMs'" :: NullOrUndefined.NullOrUndefined (Number)
  , "UplinkLossPercent'" :: NullOrUndefined.NullOrUndefined (PercentInteger)
  , "DownlinkLossPercent'" :: NullOrUndefined.NullOrUndefined (PercentInteger)
  }
derive instance newtypeUpdateNetworkProfileRequest :: Newtype UpdateNetworkProfileRequest _
derive instance repGenericUpdateNetworkProfileRequest :: Generic UpdateNetworkProfileRequest _
instance showUpdateNetworkProfileRequest :: Show UpdateNetworkProfileRequest where
  show = genericShow
instance decodeUpdateNetworkProfileRequest :: Decode UpdateNetworkProfileRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateNetworkProfileRequest :: Encode UpdateNetworkProfileRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateNetworkProfileResult = UpdateNetworkProfileResult 
  { "NetworkProfile'" :: NullOrUndefined.NullOrUndefined (NetworkProfile)
  }
derive instance newtypeUpdateNetworkProfileResult :: Newtype UpdateNetworkProfileResult _
derive instance repGenericUpdateNetworkProfileResult :: Generic UpdateNetworkProfileResult _
instance showUpdateNetworkProfileResult :: Show UpdateNetworkProfileResult where
  show = genericShow
instance decodeUpdateNetworkProfileResult :: Decode UpdateNetworkProfileResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateNetworkProfileResult :: Encode UpdateNetworkProfileResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to the update project operation.</p>
newtype UpdateProjectRequest = UpdateProjectRequest 
  { "Arn'" :: (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "DefaultJobTimeoutMinutes'" :: NullOrUndefined.NullOrUndefined (JobTimeoutMinutes)
  }
derive instance newtypeUpdateProjectRequest :: Newtype UpdateProjectRequest _
derive instance repGenericUpdateProjectRequest :: Generic UpdateProjectRequest _
instance showUpdateProjectRequest :: Show UpdateProjectRequest where
  show = genericShow
instance decodeUpdateProjectRequest :: Decode UpdateProjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateProjectRequest :: Encode UpdateProjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the result of an update project request.</p>
newtype UpdateProjectResult = UpdateProjectResult 
  { "Project'" :: NullOrUndefined.NullOrUndefined (Project)
  }
derive instance newtypeUpdateProjectResult :: Newtype UpdateProjectResult _
derive instance repGenericUpdateProjectResult :: Generic UpdateProjectResult _
instance showUpdateProjectResult :: Show UpdateProjectResult where
  show = genericShow
instance decodeUpdateProjectResult :: Decode UpdateProjectResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateProjectResult :: Encode UpdateProjectResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An app or a set of one or more tests to upload or that have been uploaded.</p>
newtype Upload = Upload 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Created'" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Type'" :: NullOrUndefined.NullOrUndefined (UploadType)
  , "Status'" :: NullOrUndefined.NullOrUndefined (UploadStatus)
  , "Url'" :: NullOrUndefined.NullOrUndefined (URL)
  , "Metadata'" :: NullOrUndefined.NullOrUndefined (Metadata)
  , "ContentType'" :: NullOrUndefined.NullOrUndefined (ContentType)
  , "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeUpload :: Newtype Upload _
derive instance repGenericUpload :: Generic Upload _
instance showUpload :: Show Upload where
  show = genericShow
instance decodeUpload :: Decode Upload where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpload :: Encode Upload where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UploadStatus = UploadStatus String
derive instance newtypeUploadStatus :: Newtype UploadStatus _
derive instance repGenericUploadStatus :: Generic UploadStatus _
instance showUploadStatus :: Show UploadStatus where
  show = genericShow
instance decodeUploadStatus :: Decode UploadStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadStatus :: Encode UploadStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UploadType = UploadType String
derive instance newtypeUploadType :: Newtype UploadType _
derive instance repGenericUploadType :: Generic UploadType _
instance showUploadType :: Show UploadType where
  show = genericShow
instance decodeUploadType :: Decode UploadType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadType :: Encode UploadType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Uploads = Uploads (Array Upload)
derive instance newtypeUploads :: Newtype Uploads _
derive instance repGenericUploads :: Generic Uploads _
instance showUploads :: Show Uploads where
  show = genericShow
instance decodeUploads :: Decode Uploads where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploads :: Encode Uploads where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
