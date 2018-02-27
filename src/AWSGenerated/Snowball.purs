

-- | <p>AWS Snowball is a petabyte-scale data transport solution that uses secure appliances to transfer large amounts of data between your on-premises data centers and Amazon Simple Storage Service (Amazon S3). The Snowball commands described here provide access to the same functionality that is available in the AWS Snowball Management Console, which enables you to create and manage jobs for Snowball. To transfer data locally with a Snowball appliance, you'll need to use the Snowball client or the Amazon S3 API adapter for Snowball. For more information, see the <a href="http://docs.aws.amazon.com/AWSImportExport/latest/ug/api-reference.html">User Guide</a>.</p>
module AWS.Snowball where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Snowball" :: String


-- | <p>Cancels a cluster job. You can only cancel a cluster job while it's in the <code>AwaitingQuorum</code> status. You'll have at least an hour after creating a cluster job to cancel it.</p>
cancelCluster :: forall eff. CancelClusterRequest -> Aff (err :: AWS.RequestError | eff) CancelClusterResult
cancelCluster = AWS.request serviceName "cancelCluster" 


-- | <p>Cancels the specified job. You can only cancel a job before its <code>JobState</code> value changes to <code>PreparingAppliance</code>. Requesting the <code>ListJobs</code> or <code>DescribeJob</code> action will return a job's <code>JobState</code> as part of the response element data returned.</p>
cancelJob :: forall eff. CancelJobRequest -> Aff (err :: AWS.RequestError | eff) CancelJobResult
cancelJob = AWS.request serviceName "cancelJob" 


-- | <p>Creates an address for a Snowball to be shipped to. In most regions, addresses are validated at the time of creation. The address you provide must be located within the serviceable area of your region. If the address is invalid or unsupported, then an exception is thrown.</p>
createAddress :: forall eff. CreateAddressRequest -> Aff (err :: AWS.RequestError | eff) CreateAddressResult
createAddress = AWS.request serviceName "createAddress" 


-- | <p>Creates an empty cluster. Each cluster supports five nodes. You use the <a>CreateJob</a> action separately to create the jobs for each of these nodes. The cluster does not ship until these five node jobs have been created.</p>
createCluster :: forall eff. CreateClusterRequest -> Aff (err :: AWS.RequestError | eff) CreateClusterResult
createCluster = AWS.request serviceName "createCluster" 


-- | <p>Creates a job to import or export data between Amazon S3 and your on-premises data center. Your AWS account must have the right trust policies and permissions in place to create a job for Snowball. If you're creating a job for a node in a cluster, you only need to provide the <code>clusterId</code> value; the other job attributes are inherited from the cluster. </p>
createJob :: forall eff. CreateJobRequest -> Aff (err :: AWS.RequestError | eff) CreateJobResult
createJob = AWS.request serviceName "createJob" 


-- | <p>Takes an <code>AddressId</code> and returns specific details about that address in the form of an <code>Address</code> object.</p>
describeAddress :: forall eff. DescribeAddressRequest -> Aff (err :: AWS.RequestError | eff) DescribeAddressResult
describeAddress = AWS.request serviceName "describeAddress" 


-- | <p>Returns a specified number of <code>ADDRESS</code> objects. Calling this API in one of the US regions will return addresses from the list of all addresses associated with this account in all US regions.</p>
describeAddresses :: forall eff. DescribeAddressesRequest -> Aff (err :: AWS.RequestError | eff) DescribeAddressesResult
describeAddresses = AWS.request serviceName "describeAddresses" 


-- | <p>Returns information about a specific cluster including shipping information, cluster status, and other important metadata.</p>
describeCluster :: forall eff. DescribeClusterRequest -> Aff (err :: AWS.RequestError | eff) DescribeClusterResult
describeCluster = AWS.request serviceName "describeCluster" 


-- | <p>Returns information about a specific job including shipping information, job status, and other important metadata. </p>
describeJob :: forall eff. DescribeJobRequest -> Aff (err :: AWS.RequestError | eff) DescribeJobResult
describeJob = AWS.request serviceName "describeJob" 


-- | <p>Returns a link to an Amazon S3 presigned URL for the manifest file associated with the specified <code>JobId</code> value. You can access the manifest file for up to 60 minutes after this request has been made. To access the manifest file after 60 minutes have passed, you'll have to make another call to the <code>GetJobManifest</code> action.</p> <p>The manifest is an encrypted file that you can download after your job enters the <code>WithCustomer</code> status. The manifest is decrypted by using the <code>UnlockCode</code> code value, when you pass both values to the Snowball through the Snowball client when the client is started for the first time.</p> <p>As a best practice, we recommend that you don't save a copy of an <code>UnlockCode</code> value in the same location as the manifest file for that job. Saving these separately helps prevent unauthorized parties from gaining access to the Snowball associated with that job.</p> <p>The credentials of a given job, including its manifest file and unlock code, expire 90 days after the job is created.</p>
getJobManifest :: forall eff. GetJobManifestRequest -> Aff (err :: AWS.RequestError | eff) GetJobManifestResult
getJobManifest = AWS.request serviceName "getJobManifest" 


-- | <p>Returns the <code>UnlockCode</code> code value for the specified job. A particular <code>UnlockCode</code> value can be accessed for up to 90 days after the associated job has been created.</p> <p>The <code>UnlockCode</code> value is a 29-character code with 25 alphanumeric characters and 4 hyphens. This code is used to decrypt the manifest file when it is passed along with the manifest to the Snowball through the Snowball client when the client is started for the first time.</p> <p>As a best practice, we recommend that you don't save a copy of the <code>UnlockCode</code> in the same location as the manifest file for that job. Saving these separately helps prevent unauthorized parties from gaining access to the Snowball associated with that job.</p>
getJobUnlockCode :: forall eff. GetJobUnlockCodeRequest -> Aff (err :: AWS.RequestError | eff) GetJobUnlockCodeResult
getJobUnlockCode = AWS.request serviceName "getJobUnlockCode" 


-- | <p>Returns information about the Snowball service limit for your account, and also the number of Snowballs your account has in use.</p> <p>The default service limit for the number of Snowballs that you can have at one time is 1. If you want to increase your service limit, contact AWS Support.</p>
getSnowballUsage :: forall eff. GetSnowballUsageRequest -> Aff (err :: AWS.RequestError | eff) GetSnowballUsageResult
getSnowballUsage = AWS.request serviceName "getSnowballUsage" 


-- | <p>Returns an array of <code>JobListEntry</code> objects of the specified length. Each <code>JobListEntry</code> object is for a job in the specified cluster and contains a job's state, a job's ID, and other information.</p>
listClusterJobs :: forall eff. ListClusterJobsRequest -> Aff (err :: AWS.RequestError | eff) ListClusterJobsResult
listClusterJobs = AWS.request serviceName "listClusterJobs" 


-- | <p>Returns an array of <code>ClusterListEntry</code> objects of the specified length. Each <code>ClusterListEntry</code> object contains a cluster's state, a cluster's ID, and other important status information.</p>
listClusters :: forall eff. ListClustersRequest -> Aff (err :: AWS.RequestError | eff) ListClustersResult
listClusters = AWS.request serviceName "listClusters" 


-- | <p>Returns an array of <code>JobListEntry</code> objects of the specified length. Each <code>JobListEntry</code> object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of export jobs. Calling this API action in one of the US regions will return jobs from the list of all jobs associated with this account in all US regions.</p>
listJobs :: forall eff. ListJobsRequest -> Aff (err :: AWS.RequestError | eff) ListJobsResult
listJobs = AWS.request serviceName "listJobs" 


-- | <p>While a cluster's <code>ClusterState</code> value is in the <code>AwaitingQuorum</code> state, you can update some of the information associated with a cluster. Once the cluster changes to a different job state, usually 60 minutes after the cluster being created, this action is no longer available.</p>
updateCluster :: forall eff. UpdateClusterRequest -> Aff (err :: AWS.RequestError | eff) UpdateClusterResult
updateCluster = AWS.request serviceName "updateCluster" 


-- | <p>While a job's <code>JobState</code> value is <code>New</code>, you can update some of the information associated with a job. Once the job changes to a different job state, usually within 60 minutes of the job being created, this action is no longer available.</p>
updateJob :: forall eff. UpdateJobRequest -> Aff (err :: AWS.RequestError | eff) UpdateJobResult
updateJob = AWS.request serviceName "updateJob" 


-- | <p>The address that you want the Snowball or Snowballs associated with a specific job to be shipped to. Addresses are validated at the time of creation. The address you provide must be located within the serviceable area of your region. Although no individual elements of the <code>Address</code> are required, if the address is invalid or unsupported, then an exception is thrown.</p>
newtype Address = Address 
  { "AddressId" :: NullOrUndefined (AddressId)
  , "Name" :: NullOrUndefined (String)
  , "Company" :: NullOrUndefined (String)
  , "Street1" :: NullOrUndefined (String)
  , "Street2" :: NullOrUndefined (String)
  , "Street3" :: NullOrUndefined (String)
  , "City" :: NullOrUndefined (String)
  , "StateOrProvince" :: NullOrUndefined (String)
  , "PrefectureOrDistrict" :: NullOrUndefined (String)
  , "Landmark" :: NullOrUndefined (String)
  , "Country" :: NullOrUndefined (String)
  , "PostalCode" :: NullOrUndefined (String)
  , "PhoneNumber" :: NullOrUndefined (String)
  , "IsRestricted" :: NullOrUndefined (Boolean)
  }
derive instance newtypeAddress :: Newtype Address _


newtype AddressId = AddressId String
derive instance newtypeAddressId :: Newtype AddressId _


newtype AddressList = AddressList (Array Address)
derive instance newtypeAddressList :: Newtype AddressList _


newtype CancelClusterRequest = CancelClusterRequest 
  { "ClusterId" :: (ClusterId)
  }
derive instance newtypeCancelClusterRequest :: Newtype CancelClusterRequest _


newtype CancelClusterResult = CancelClusterResult 
  { 
  }
derive instance newtypeCancelClusterResult :: Newtype CancelClusterResult _


newtype CancelJobRequest = CancelJobRequest 
  { "JobId" :: (JobId)
  }
derive instance newtypeCancelJobRequest :: Newtype CancelJobRequest _


newtype CancelJobResult = CancelJobResult 
  { 
  }
derive instance newtypeCancelJobResult :: Newtype CancelJobResult _


newtype ClusterId = ClusterId String
derive instance newtypeClusterId :: Newtype ClusterId _


-- | <p>Job creation failed. Currently, clusters support five nodes. If you have less than five nodes for your cluster and you have more nodes to create for this cluster, try again and create jobs until your cluster has exactly five notes.</p>
newtype ClusterLimitExceededException = ClusterLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeClusterLimitExceededException :: Newtype ClusterLimitExceededException _


-- | <p>Contains a cluster's state, a cluster's ID, and other important information.</p>
newtype ClusterListEntry = ClusterListEntry 
  { "ClusterId" :: NullOrUndefined (String)
  , "ClusterState" :: NullOrUndefined (ClusterState)
  , "CreationDate" :: NullOrUndefined (Number)
  , "Description" :: NullOrUndefined (String)
  }
derive instance newtypeClusterListEntry :: Newtype ClusterListEntry _


newtype ClusterListEntryList = ClusterListEntryList (Array ClusterListEntry)
derive instance newtypeClusterListEntryList :: Newtype ClusterListEntryList _


-- | <p>Contains metadata about a specific cluster.</p>
newtype ClusterMetadata = ClusterMetadata 
  { "ClusterId" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "KmsKeyARN" :: NullOrUndefined (KmsKeyARN)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "ClusterState" :: NullOrUndefined (ClusterState)
  , "JobType" :: NullOrUndefined (JobType)
  , "SnowballType" :: NullOrUndefined (SnowballType)
  , "CreationDate" :: NullOrUndefined (Number)
  , "Resources" :: NullOrUndefined (JobResource)
  , "AddressId" :: NullOrUndefined (AddressId)
  , "ShippingOption" :: NullOrUndefined (ShippingOption)
  , "Notification" :: NullOrUndefined (Notification)
  , "ForwardingAddressId" :: NullOrUndefined (AddressId)
  }
derive instance newtypeClusterMetadata :: Newtype ClusterMetadata _


newtype ClusterState = ClusterState String
derive instance newtypeClusterState :: Newtype ClusterState _


newtype CreateAddressRequest = CreateAddressRequest 
  { "Address" :: (Address)
  }
derive instance newtypeCreateAddressRequest :: Newtype CreateAddressRequest _


newtype CreateAddressResult = CreateAddressResult 
  { "AddressId" :: NullOrUndefined (String)
  }
derive instance newtypeCreateAddressResult :: Newtype CreateAddressResult _


newtype CreateClusterRequest = CreateClusterRequest 
  { "JobType" :: (JobType)
  , "Resources" :: (JobResource)
  , "Description" :: NullOrUndefined (String)
  , "AddressId" :: (AddressId)
  , "KmsKeyARN" :: NullOrUndefined (KmsKeyARN)
  , "RoleARN" :: (RoleARN)
  , "SnowballType" :: NullOrUndefined (SnowballType)
  , "ShippingOption" :: (ShippingOption)
  , "Notification" :: NullOrUndefined (Notification)
  , "ForwardingAddressId" :: NullOrUndefined (AddressId)
  }
derive instance newtypeCreateClusterRequest :: Newtype CreateClusterRequest _


newtype CreateClusterResult = CreateClusterResult 
  { "ClusterId" :: NullOrUndefined (ClusterId)
  }
derive instance newtypeCreateClusterResult :: Newtype CreateClusterResult _


newtype CreateJobRequest = CreateJobRequest 
  { "JobType" :: NullOrUndefined (JobType)
  , "Resources" :: NullOrUndefined (JobResource)
  , "Description" :: NullOrUndefined (String)
  , "AddressId" :: NullOrUndefined (AddressId)
  , "KmsKeyARN" :: NullOrUndefined (KmsKeyARN)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "SnowballCapacityPreference" :: NullOrUndefined (SnowballCapacity)
  , "ShippingOption" :: NullOrUndefined (ShippingOption)
  , "Notification" :: NullOrUndefined (Notification)
  , "ClusterId" :: NullOrUndefined (ClusterId)
  , "SnowballType" :: NullOrUndefined (SnowballType)
  , "ForwardingAddressId" :: NullOrUndefined (AddressId)
  }
derive instance newtypeCreateJobRequest :: Newtype CreateJobRequest _


newtype CreateJobResult = CreateJobResult 
  { "JobId" :: NullOrUndefined (JobId)
  }
derive instance newtypeCreateJobResult :: Newtype CreateJobResult _


-- | <p>Defines the real-time status of a Snowball's data transfer while the appliance is at AWS. This data is only available while a job has a <code>JobState</code> value of <code>InProgress</code>, for both import and export jobs.</p>
newtype DataTransfer = DataTransfer 
  { "BytesTransferred" :: NullOrUndefined (Number)
  , "ObjectsTransferred" :: NullOrUndefined (Number)
  , "TotalBytes" :: NullOrUndefined (Number)
  , "TotalObjects" :: NullOrUndefined (Number)
  }
derive instance newtypeDataTransfer :: Newtype DataTransfer _


newtype DescribeAddressRequest = DescribeAddressRequest 
  { "AddressId" :: (AddressId)
  }
derive instance newtypeDescribeAddressRequest :: Newtype DescribeAddressRequest _


newtype DescribeAddressResult = DescribeAddressResult 
  { "Address" :: NullOrUndefined (Address)
  }
derive instance newtypeDescribeAddressResult :: Newtype DescribeAddressResult _


newtype DescribeAddressesRequest = DescribeAddressesRequest 
  { "MaxResults" :: NullOrUndefined (ListLimit)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeAddressesRequest :: Newtype DescribeAddressesRequest _


newtype DescribeAddressesResult = DescribeAddressesResult 
  { "Addresses" :: NullOrUndefined (AddressList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeAddressesResult :: Newtype DescribeAddressesResult _


newtype DescribeClusterRequest = DescribeClusterRequest 
  { "ClusterId" :: (ClusterId)
  }
derive instance newtypeDescribeClusterRequest :: Newtype DescribeClusterRequest _


newtype DescribeClusterResult = DescribeClusterResult 
  { "ClusterMetadata" :: NullOrUndefined (ClusterMetadata)
  }
derive instance newtypeDescribeClusterResult :: Newtype DescribeClusterResult _


newtype DescribeJobRequest = DescribeJobRequest 
  { "JobId" :: (JobId)
  }
derive instance newtypeDescribeJobRequest :: Newtype DescribeJobRequest _


newtype DescribeJobResult = DescribeJobResult 
  { "JobMetadata" :: NullOrUndefined (JobMetadata)
  , "SubJobMetadata" :: NullOrUndefined (JobMetadataList)
  }
derive instance newtypeDescribeJobResult :: Newtype DescribeJobResult _


-- | <p>The container for the <a>EventTriggerDefinition$EventResourceARN</a>.</p>
newtype EventTriggerDefinition = EventTriggerDefinition 
  { "EventResourceARN" :: NullOrUndefined (ResourceARN)
  }
derive instance newtypeEventTriggerDefinition :: Newtype EventTriggerDefinition _


newtype EventTriggerDefinitionList = EventTriggerDefinitionList (Array EventTriggerDefinition)
derive instance newtypeEventTriggerDefinitionList :: Newtype EventTriggerDefinitionList _


newtype GetJobManifestRequest = GetJobManifestRequest 
  { "JobId" :: (JobId)
  }
derive instance newtypeGetJobManifestRequest :: Newtype GetJobManifestRequest _


newtype GetJobManifestResult = GetJobManifestResult 
  { "ManifestURI" :: NullOrUndefined (String)
  }
derive instance newtypeGetJobManifestResult :: Newtype GetJobManifestResult _


newtype GetJobUnlockCodeRequest = GetJobUnlockCodeRequest 
  { "JobId" :: (JobId)
  }
derive instance newtypeGetJobUnlockCodeRequest :: Newtype GetJobUnlockCodeRequest _


newtype GetJobUnlockCodeResult = GetJobUnlockCodeResult 
  { "UnlockCode" :: NullOrUndefined (String)
  }
derive instance newtypeGetJobUnlockCodeResult :: Newtype GetJobUnlockCodeResult _


newtype GetSnowballUsageRequest = GetSnowballUsageRequest 
  { 
  }
derive instance newtypeGetSnowballUsageRequest :: Newtype GetSnowballUsageRequest _


newtype GetSnowballUsageResult = GetSnowballUsageResult 
  { "SnowballLimit" :: NullOrUndefined (Int)
  , "SnowballsInUse" :: NullOrUndefined (Int)
  }
derive instance newtypeGetSnowballUsageResult :: Newtype GetSnowballUsageResult _


-- | <p>The address provided was invalid. Check the address with your region's carrier, and try again.</p>
newtype InvalidAddressException = InvalidAddressException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidAddressException :: Newtype InvalidAddressException _


-- | <p>Job or cluster creation failed. One ore more inputs were invalid. Confirm that the <a>CreateClusterRequest$SnowballType</a> value supports your <a>CreateJobRequest$JobType</a>, and try again.</p>
newtype InvalidInputCombinationException = InvalidInputCombinationException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidInputCombinationException :: Newtype InvalidInputCombinationException _


-- | <p>The action can't be performed because the job's current state doesn't allow that action to be performed.</p>
newtype InvalidJobStateException = InvalidJobStateException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidJobStateException :: Newtype InvalidJobStateException _


-- | <p>The <code>NextToken</code> string was altered unexpectedly, and the operation has stopped. Run the operation without changing the <code>NextToken</code> string, and try again.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _


-- | <p>The specified resource can't be found. Check the information you provided in your last request, and try again.</p>
newtype InvalidResourceException = InvalidResourceException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidResourceException :: Newtype InvalidResourceException _


newtype JobId = JobId String
derive instance newtypeJobId :: Newtype JobId _


-- | <p>Each <code>JobListEntry</code> object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of an export job.</p>
newtype JobListEntry = JobListEntry 
  { "JobId" :: NullOrUndefined (String)
  , "JobState" :: NullOrUndefined (JobState)
  , "IsMaster" :: NullOrUndefined (Boolean)
  , "JobType" :: NullOrUndefined (JobType)
  , "SnowballType" :: NullOrUndefined (SnowballType)
  , "CreationDate" :: NullOrUndefined (Number)
  , "Description" :: NullOrUndefined (String)
  }
derive instance newtypeJobListEntry :: Newtype JobListEntry _


newtype JobListEntryList = JobListEntryList (Array JobListEntry)
derive instance newtypeJobListEntryList :: Newtype JobListEntryList _


-- | <p>Contains job logs. Whenever Snowball is used to import data into or export data out of Amazon S3, you'll have the option of downloading a PDF job report. Job logs are returned as a part of the response syntax of the <code>DescribeJob</code> action in the <code>JobMetadata</code> data type. The job logs can be accessed for up to 60 minutes after this request has been made. To access any of the job logs after 60 minutes have passed, you'll have to make another call to the <code>DescribeJob</code> action.</p> <p>For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snowball for your job part is being delivered to you.</p> <p>The job report provides you insight into the state of your Amazon S3 data transfer. The report includes details about your job or job part for your records.</p> <p>For deeper visibility into the status of your transferred objects, you can look at the two associated logs: a success log and a failure log. The logs are saved in comma-separated value (CSV) format, and the name of each log includes the ID of the job or job part that the log describes.</p>
newtype JobLogs = JobLogs 
  { "JobCompletionReportURI" :: NullOrUndefined (String)
  , "JobSuccessLogURI" :: NullOrUndefined (String)
  , "JobFailureLogURI" :: NullOrUndefined (String)
  }
derive instance newtypeJobLogs :: Newtype JobLogs _


-- | <p>Contains information about a specific job including shipping information, job status, and other important metadata. This information is returned as a part of the response syntax of the <code>DescribeJob</code> action.</p>
newtype JobMetadata = JobMetadata 
  { "JobId" :: NullOrUndefined (String)
  , "JobState" :: NullOrUndefined (JobState)
  , "JobType" :: NullOrUndefined (JobType)
  , "SnowballType" :: NullOrUndefined (SnowballType)
  , "CreationDate" :: NullOrUndefined (Number)
  , "Resources" :: NullOrUndefined (JobResource)
  , "Description" :: NullOrUndefined (String)
  , "KmsKeyARN" :: NullOrUndefined (KmsKeyARN)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "AddressId" :: NullOrUndefined (AddressId)
  , "ShippingDetails" :: NullOrUndefined (ShippingDetails)
  , "SnowballCapacityPreference" :: NullOrUndefined (SnowballCapacity)
  , "Notification" :: NullOrUndefined (Notification)
  , "DataTransferProgress" :: NullOrUndefined (DataTransfer)
  , "JobLogInfo" :: NullOrUndefined (JobLogs)
  , "ClusterId" :: NullOrUndefined (String)
  , "ForwardingAddressId" :: NullOrUndefined (AddressId)
  }
derive instance newtypeJobMetadata :: Newtype JobMetadata _


newtype JobMetadataList = JobMetadataList (Array JobMetadata)
derive instance newtypeJobMetadataList :: Newtype JobMetadataList _


-- | <p>Contains an array of <code>S3Resource</code> objects. Each <code>S3Resource</code> object represents an Amazon S3 bucket that your transferred data will be exported from or imported into.</p>
newtype JobResource = JobResource 
  { "S3Resources" :: NullOrUndefined (S3ResourceList)
  , "LambdaResources" :: NullOrUndefined (LambdaResourceList)
  }
derive instance newtypeJobResource :: Newtype JobResource _


newtype JobState = JobState String
derive instance newtypeJobState :: Newtype JobState _


newtype JobStateList = JobStateList (Array JobState)
derive instance newtypeJobStateList :: Newtype JobStateList _


newtype JobType = JobType String
derive instance newtypeJobType :: Newtype JobType _


-- | <p>The provided AWS Key Management Service key lacks the permissions to perform the specified <a>CreateJob</a> or <a>UpdateJob</a> action.</p>
newtype KMSRequestFailedException = KMSRequestFailedException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeKMSRequestFailedException :: Newtype KMSRequestFailedException _


-- | <p>Contains a key range. For export jobs, a <code>S3Resource</code> object can have an optional <code>KeyRange</code> value. The length of the range is defined at job creation, and has either an inclusive <code>BeginMarker</code>, an inclusive <code>EndMarker</code>, or both. Ranges are UTF-8 binary sorted.</p>
newtype KeyRange = KeyRange 
  { "BeginMarker" :: NullOrUndefined (String)
  , "EndMarker" :: NullOrUndefined (String)
  }
derive instance newtypeKeyRange :: Newtype KeyRange _


newtype KmsKeyARN = KmsKeyARN String
derive instance newtypeKmsKeyARN :: Newtype KmsKeyARN _


-- | <p>Identifies </p>
newtype LambdaResource = LambdaResource 
  { "LambdaArn" :: NullOrUndefined (ResourceARN)
  , "EventTriggers" :: NullOrUndefined (EventTriggerDefinitionList)
  }
derive instance newtypeLambdaResource :: Newtype LambdaResource _


newtype LambdaResourceList = LambdaResourceList (Array LambdaResource)
derive instance newtypeLambdaResourceList :: Newtype LambdaResourceList _


newtype ListClusterJobsRequest = ListClusterJobsRequest 
  { "ClusterId" :: (ClusterId)
  , "MaxResults" :: NullOrUndefined (ListLimit)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListClusterJobsRequest :: Newtype ListClusterJobsRequest _


newtype ListClusterJobsResult = ListClusterJobsResult 
  { "JobListEntries" :: NullOrUndefined (JobListEntryList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListClusterJobsResult :: Newtype ListClusterJobsResult _


newtype ListClustersRequest = ListClustersRequest 
  { "MaxResults" :: NullOrUndefined (ListLimit)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListClustersRequest :: Newtype ListClustersRequest _


newtype ListClustersResult = ListClustersResult 
  { "ClusterListEntries" :: NullOrUndefined (ClusterListEntryList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListClustersResult :: Newtype ListClustersResult _


newtype ListJobsRequest = ListJobsRequest 
  { "MaxResults" :: NullOrUndefined (ListLimit)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListJobsRequest :: Newtype ListJobsRequest _


newtype ListJobsResult = ListJobsResult 
  { "JobListEntries" :: NullOrUndefined (JobListEntryList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListJobsResult :: Newtype ListJobsResult _


newtype ListLimit = ListLimit Int
derive instance newtypeListLimit :: Newtype ListLimit _


-- | <p>The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The <code>Notification</code> object is returned as a part of the response syntax of the <code>DescribeJob</code> action in the <code>JobMetadata</code> data type.</p> <p>When the notification settings are defined during job creation, you can choose to notify based on a specific set of job states using the <code>JobStatesToNotify</code> array of strings, or you can specify that you want to have Amazon SNS notifications sent out for all job states with <code>NotifyAll</code> set to true.</p>
newtype Notification = Notification 
  { "SnsTopicARN" :: NullOrUndefined (SnsTopicARN)
  , "JobStatesToNotify" :: NullOrUndefined (JobStateList)
  , "NotifyAll" :: NullOrUndefined (Boolean)
  }
derive instance newtypeNotification :: Newtype Notification _


newtype ResourceARN = ResourceARN String
derive instance newtypeResourceARN :: Newtype ResourceARN _


newtype RoleARN = RoleARN String
derive instance newtypeRoleARN :: Newtype RoleARN _


-- | <p>Each <code>S3Resource</code> object represents an Amazon S3 bucket that your transferred data will be exported from or imported into. For export jobs, this object can have an optional <code>KeyRange</code> value. The length of the range is defined at job creation, and has either an inclusive <code>BeginMarker</code>, an inclusive <code>EndMarker</code>, or both. Ranges are UTF-8 binary sorted.</p>
newtype S3Resource = S3Resource 
  { "BucketArn" :: NullOrUndefined (ResourceARN)
  , "KeyRange" :: NullOrUndefined (KeyRange)
  }
derive instance newtypeS3Resource :: Newtype S3Resource _


newtype S3ResourceList = S3ResourceList (Array S3Resource)
derive instance newtypeS3ResourceList :: Newtype S3ResourceList _


-- | <p>The <code>Status</code> and <code>TrackingNumber</code> information for an inbound or outbound shipment.</p>
newtype Shipment = Shipment 
  { "Status" :: NullOrUndefined (String)
  , "TrackingNumber" :: NullOrUndefined (String)
  }
derive instance newtypeShipment :: Newtype Shipment _


-- | <p>A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.</p>
newtype ShippingDetails = ShippingDetails 
  { "ShippingOption" :: NullOrUndefined (ShippingOption)
  , "InboundShipment" :: NullOrUndefined (Shipment)
  , "OutboundShipment" :: NullOrUndefined (Shipment)
  }
derive instance newtypeShippingDetails :: Newtype ShippingDetails _


newtype ShippingOption = ShippingOption String
derive instance newtypeShippingOption :: Newtype ShippingOption _


newtype SnowballCapacity = SnowballCapacity String
derive instance newtypeSnowballCapacity :: Newtype SnowballCapacity _


newtype SnowballType = SnowballType String
derive instance newtypeSnowballType :: Newtype SnowballType _


newtype SnsTopicARN = SnsTopicARN String
derive instance newtypeSnsTopicARN :: Newtype SnsTopicARN _


-- | <p>The address is either outside the serviceable area for your region, or an error occurred. Check the address with your region's carrier and try again. If the issue persists, contact AWS Support.</p>
newtype UnsupportedAddressException = UnsupportedAddressException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeUnsupportedAddressException :: Newtype UnsupportedAddressException _


newtype UpdateClusterRequest = UpdateClusterRequest 
  { "ClusterId" :: (ClusterId)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "Description" :: NullOrUndefined (String)
  , "Resources" :: NullOrUndefined (JobResource)
  , "AddressId" :: NullOrUndefined (AddressId)
  , "ShippingOption" :: NullOrUndefined (ShippingOption)
  , "Notification" :: NullOrUndefined (Notification)
  , "ForwardingAddressId" :: NullOrUndefined (AddressId)
  }
derive instance newtypeUpdateClusterRequest :: Newtype UpdateClusterRequest _


newtype UpdateClusterResult = UpdateClusterResult 
  { 
  }
derive instance newtypeUpdateClusterResult :: Newtype UpdateClusterResult _


newtype UpdateJobRequest = UpdateJobRequest 
  { "JobId" :: (JobId)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "Notification" :: NullOrUndefined (Notification)
  , "Resources" :: NullOrUndefined (JobResource)
  , "AddressId" :: NullOrUndefined (AddressId)
  , "ShippingOption" :: NullOrUndefined (ShippingOption)
  , "Description" :: NullOrUndefined (String)
  , "SnowballCapacityPreference" :: NullOrUndefined (SnowballCapacity)
  , "ForwardingAddressId" :: NullOrUndefined (AddressId)
  }
derive instance newtypeUpdateJobRequest :: Newtype UpdateJobRequest _


newtype UpdateJobResult = UpdateJobResult 
  { 
  }
derive instance newtypeUpdateJobResult :: Newtype UpdateJobResult _
