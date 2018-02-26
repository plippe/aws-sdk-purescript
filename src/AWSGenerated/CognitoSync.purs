

-- | <fullname>Amazon Cognito Sync</fullname> <p>Amazon Cognito Sync provides an AWS service and client library that enable cross-device syncing of application-related user data. High-level client libraries are available for both iOS and Android. You can use these libraries to persist data locally so that it's available even if the device is offline. Developer credentials don't need to be stored on the mobile device to access the service. You can use Amazon Cognito to obtain a normalized user ID and credentials. User data is persisted in a dataset that can store up to 1 MB of key-value pairs, and you can have up to 20 datasets per user identity.</p> <p>With Amazon Cognito Sync, the data stored for each identity is accessible only to credentials assigned to that identity. In order to use the Cognito Sync service, you need to make API calls using credentials retrieved with <a href="http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/Welcome.html">Amazon Cognito Identity service</a>.</p> <p>If you want to use Cognito Sync in an Android or iOS application, you will probably want to make API calls via the AWS Mobile SDK. To learn more, see the <a href="http://docs.aws.amazon.com/mobile/sdkforandroid/developerguide/cognito-sync.html">Developer Guide for Android</a> and the <a href="http://docs.aws.amazon.com/mobile/sdkforios/developerguide/cognito-sync.html">Developer Guide for iOS</a>.</p>
module AWS.CognitoSync where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CognitoSync" :: String


-- | <p>Initiates a bulk publish of all existing datasets for an Identity Pool to the configured stream. Customers are limited to one successful bulk publish per 24 hours. Bulk publish is an asynchronous request, customers can see the status of the request via the GetBulkPublishDetails operation.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
bulkPublish :: forall eff. BulkPublishRequest -> Aff (err :: AWS.RequestError | eff) BulkPublishResponse
bulkPublish = AWS.request serviceName "BulkPublish" 


-- | <p>Deletes the specific dataset. The dataset will be deleted permanently, and the action can't be undone. Datasets that this dataset was merged with will no longer report the merge. Any subsequent operation on this dataset will result in a ResourceNotFoundException.</p> <p>This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.</p>
deleteDataset :: forall eff. DeleteDatasetRequest -> Aff (err :: AWS.RequestError | eff) DeleteDatasetResponse
deleteDataset = AWS.request serviceName "DeleteDataset" 


-- | <p>Gets meta data about a dataset by identity and dataset name. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.</p> <p>This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use Cognito Identity credentials to make this API call.</p>
describeDataset :: forall eff. DescribeDatasetRequest -> Aff (err :: AWS.RequestError | eff) DescribeDatasetResponse
describeDataset = AWS.request serviceName "DescribeDataset" 


-- | <p>Gets usage details (for example, data storage) about a particular identity pool.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
describeIdentityPoolUsage :: forall eff. DescribeIdentityPoolUsageRequest -> Aff (err :: AWS.RequestError | eff) DescribeIdentityPoolUsageResponse
describeIdentityPoolUsage = AWS.request serviceName "DescribeIdentityPoolUsage" 


-- | <p>Gets usage information for an identity, including number of datasets and data usage.</p> <p>This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.</p>
describeIdentityUsage :: forall eff. DescribeIdentityUsageRequest -> Aff (err :: AWS.RequestError | eff) DescribeIdentityUsageResponse
describeIdentityUsage = AWS.request serviceName "DescribeIdentityUsage" 


-- | <p>Get the status of the last BulkPublish operation for an identity pool.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
getBulkPublishDetails :: forall eff. GetBulkPublishDetailsRequest -> Aff (err :: AWS.RequestError | eff) GetBulkPublishDetailsResponse
getBulkPublishDetails = AWS.request serviceName "GetBulkPublishDetails" 


-- | <p>Gets the events and the corresponding Lambda functions associated with an identity pool.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
getCognitoEvents :: forall eff. GetCognitoEventsRequest -> Aff (err :: AWS.RequestError | eff) GetCognitoEventsResponse
getCognitoEvents = AWS.request serviceName "GetCognitoEvents" 


-- | <p>Gets the configuration settings of an identity pool.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
getIdentityPoolConfiguration :: forall eff. GetIdentityPoolConfigurationRequest -> Aff (err :: AWS.RequestError | eff) GetIdentityPoolConfigurationResponse
getIdentityPoolConfiguration = AWS.request serviceName "GetIdentityPoolConfiguration" 


-- | <p>Lists datasets for an identity. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.</p> <p>ListDatasets can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use the Cognito Identity credentials to make this API call.</p>
listDatasets :: forall eff. ListDatasetsRequest -> Aff (err :: AWS.RequestError | eff) ListDatasetsResponse
listDatasets = AWS.request serviceName "ListDatasets" 


-- | <p>Gets a list of identity pools registered with Cognito.</p> <p>ListIdentityPoolUsage can only be called with developer credentials. You cannot make this API call with the temporary user credentials provided by Cognito Identity.</p>
listIdentityPoolUsage :: forall eff. ListIdentityPoolUsageRequest -> Aff (err :: AWS.RequestError | eff) ListIdentityPoolUsageResponse
listIdentityPoolUsage = AWS.request serviceName "ListIdentityPoolUsage" 


-- | <p>Gets paginated records, optionally changed after a particular sync count for a dataset and identity. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.</p> <p>ListRecords can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use Cognito Identity credentials to make this API call.</p>
listRecords :: forall eff. ListRecordsRequest -> Aff (err :: AWS.RequestError | eff) ListRecordsResponse
listRecords = AWS.request serviceName "ListRecords" 


-- | <p>Registers a device to receive push sync notifications.</p> <p>This API can only be called with temporary credentials provided by Cognito Identity. You cannot call this API with developer credentials.</p>
registerDevice :: forall eff. RegisterDeviceRequest -> Aff (err :: AWS.RequestError | eff) RegisterDeviceResponse
registerDevice = AWS.request serviceName "RegisterDevice" 


-- | <p>Sets the AWS Lambda function for a given event type for an identity pool. This request only updates the key/value pair specified. Other key/values pairs are not updated. To remove a key value pair, pass a empty value for the particular key.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
setCognitoEvents :: forall eff. SetCognitoEventsRequest -> Aff (err :: AWS.RequestError | eff) Unit
setCognitoEvents = AWS.request serviceName "SetCognitoEvents" 


-- | <p>Sets the necessary configuration for push sync.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
setIdentityPoolConfiguration :: forall eff. SetIdentityPoolConfigurationRequest -> Aff (err :: AWS.RequestError | eff) SetIdentityPoolConfigurationResponse
setIdentityPoolConfiguration = AWS.request serviceName "SetIdentityPoolConfiguration" 


-- | <p>Subscribes to receive notifications when a dataset is modified by another device.</p> <p>This API can only be called with temporary credentials provided by Cognito Identity. You cannot call this API with developer credentials.</p>
subscribeToDataset :: forall eff. SubscribeToDatasetRequest -> Aff (err :: AWS.RequestError | eff) SubscribeToDatasetResponse
subscribeToDataset = AWS.request serviceName "SubscribeToDataset" 


-- | <p>Unsubscribes from receiving notifications when a dataset is modified by another device.</p> <p>This API can only be called with temporary credentials provided by Cognito Identity. You cannot call this API with developer credentials.</p>
unsubscribeFromDataset :: forall eff. UnsubscribeFromDatasetRequest -> Aff (err :: AWS.RequestError | eff) UnsubscribeFromDatasetResponse
unsubscribeFromDataset = AWS.request serviceName "UnsubscribeFromDataset" 


-- | <p>Posts updates to records and adds and deletes records for a dataset and user.</p> <p>The sync count in the record patch is your last known sync count for that record. The server will reject an UpdateRecords request with a ResourceConflictException if you try to patch a record with a new value but a stale sync count.</p> <p>For example, if the sync count on the server is 5 for a key called highScore and you try and submit a new highScore with sync count of 4, the request will be rejected. To obtain the current sync count for a record, call ListRecords. On a successful update of the record, the response returns the new sync count for that record. You should present that sync count the next time you try to update that same record. When the record does not exist, specify the sync count as 0.</p> <p>This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.</p>
updateRecords :: forall eff. UpdateRecordsRequest -> Aff (err :: AWS.RequestError | eff) UpdateRecordsResponse
updateRecords = AWS.request serviceName "UpdateRecords" 


-- | An exception thrown when a bulk publish operation is requested less than 24 hours after a previous bulk publish operation completed successfully.
newtype AlreadyStreamedException = AlreadyStreamedException 
  { "Message'" :: (ExceptionMessage)
  }


newtype ApplicationArn = ApplicationArn String


newtype ApplicationArnList = ApplicationArnList (Array ApplicationArn)


newtype AssumeRoleArn = AssumeRoleArn String


-- | The input for the BulkPublish operation.
newtype BulkPublishRequest = BulkPublishRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  }


-- | The output for the BulkPublish operation.
newtype BulkPublishResponse = BulkPublishResponse 
  { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId)
  }


newtype BulkPublishStatus = BulkPublishStatus String


newtype ClientContext = ClientContext String


newtype CognitoEventType = CognitoEventType String


-- | Configuration options for configure Cognito streams.
newtype CognitoStreams = CognitoStreams 
  { "StreamName" :: NullOrUndefined (StreamName)
  , "RoleArn" :: NullOrUndefined (AssumeRoleArn)
  , "StreamingStatus" :: NullOrUndefined (StreamingStatus)
  }


-- | <p>Thrown if there are parallel requests to modify a resource.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: (String)
  }


-- | A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
newtype Dataset = Dataset 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "DatasetName" :: NullOrUndefined (DatasetName)
  , "CreationDate" :: NullOrUndefined (Date)
  , "LastModifiedDate" :: NullOrUndefined (Date)
  , "LastModifiedBy" :: NullOrUndefined (String)
  , "DataStorage" :: NullOrUndefined (Number)
  , "NumRecords" :: NullOrUndefined (Number)
  }


newtype DatasetList = DatasetList (Array Dataset)


newtype DatasetName = DatasetName String


newtype Date = Date Number


-- | A request to delete the specific dataset.
newtype DeleteDatasetRequest = DeleteDatasetRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "DatasetName" :: (DatasetName)
  }


-- | Response to a successful DeleteDataset request.
newtype DeleteDatasetResponse = DeleteDatasetResponse 
  { "Dataset" :: NullOrUndefined (Dataset)
  }


-- | A request for meta data about a dataset (creation date, number of records, size) by owner and dataset name.
newtype DescribeDatasetRequest = DescribeDatasetRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "DatasetName" :: (DatasetName)
  }


-- | Response to a successful DescribeDataset request.
newtype DescribeDatasetResponse = DescribeDatasetResponse 
  { "Dataset" :: NullOrUndefined (Dataset)
  }


-- | A request for usage information about the identity pool.
newtype DescribeIdentityPoolUsageRequest = DescribeIdentityPoolUsageRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  }


-- | Response to a successful DescribeIdentityPoolUsage request.
newtype DescribeIdentityPoolUsageResponse = DescribeIdentityPoolUsageResponse 
  { "IdentityPoolUsage" :: NullOrUndefined (IdentityPoolUsage)
  }


-- | A request for information about the usage of an identity pool.
newtype DescribeIdentityUsageRequest = DescribeIdentityUsageRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  }


-- | The response to a successful DescribeIdentityUsage request.
newtype DescribeIdentityUsageResponse = DescribeIdentityUsageResponse 
  { "IdentityUsage" :: NullOrUndefined (IdentityUsage)
  }


newtype DeviceId = DeviceId String


-- | An exception thrown when there is an IN_PROGRESS bulk publish operation for the given identity pool.
newtype DuplicateRequestException = DuplicateRequestException 
  { "Message'" :: (ExceptionMessage)
  }


newtype Events = Events (Map CognitoEventType LambdaFunctionArn)


newtype ExceptionMessage = ExceptionMessage String


-- | The input for the GetBulkPublishDetails operation.
newtype GetBulkPublishDetailsRequest = GetBulkPublishDetailsRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  }


-- | The output for the GetBulkPublishDetails operation.
newtype GetBulkPublishDetailsResponse = GetBulkPublishDetailsResponse 
  { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId)
  , "BulkPublishStartTime" :: NullOrUndefined (Date)
  , "BulkPublishCompleteTime" :: NullOrUndefined (Date)
  , "BulkPublishStatus" :: NullOrUndefined (BulkPublishStatus)
  , "FailureMessage" :: NullOrUndefined (String)
  }


-- | <p>A request for a list of the configured Cognito Events</p>
newtype GetCognitoEventsRequest = GetCognitoEventsRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  }


-- | <p>The response from the GetCognitoEvents request</p>
newtype GetCognitoEventsResponse = GetCognitoEventsResponse 
  { "Events" :: NullOrUndefined (Events)
  }


-- | <p>The input for the GetIdentityPoolConfiguration operation.</p>
newtype GetIdentityPoolConfigurationRequest = GetIdentityPoolConfigurationRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  }


-- | <p>The output for the GetIdentityPoolConfiguration operation.</p>
newtype GetIdentityPoolConfigurationResponse = GetIdentityPoolConfigurationResponse 
  { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId)
  , "PushSync" :: NullOrUndefined (PushSync)
  , "CognitoStreams" :: NullOrUndefined (CognitoStreams)
  }


newtype IdentityId = IdentityId String


newtype IdentityPoolId = IdentityPoolId String


-- | Usage information for the identity pool.
newtype IdentityPoolUsage = IdentityPoolUsage 
  { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId)
  , "SyncSessionsCount" :: NullOrUndefined (Number)
  , "DataStorage" :: NullOrUndefined (Number)
  , "LastModifiedDate" :: NullOrUndefined (Date)
  }


newtype IdentityPoolUsageList = IdentityPoolUsageList (Array IdentityPoolUsage)


-- | Usage information for the identity.
newtype IdentityUsage = IdentityUsage 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "IdentityPoolId" :: NullOrUndefined (IdentityPoolId)
  , "LastModifiedDate" :: NullOrUndefined (Date)
  , "DatasetCount" :: NullOrUndefined (Int)
  , "DataStorage" :: NullOrUndefined (Number)
  }


newtype IntegerString = IntegerString Int


-- | Indicates an internal service error.
newtype InternalErrorException = InternalErrorException 
  { "Message'" :: (ExceptionMessage)
  }


newtype InvalidConfigurationException = InvalidConfigurationException 
  { "Message'" :: (ExceptionMessage)
  }


-- | <p>The AWS Lambda function returned invalid output or an exception.</p>
newtype InvalidLambdaFunctionOutputException = InvalidLambdaFunctionOutputException 
  { "Message'" :: (ExceptionMessage)
  }


-- | Thrown when a request parameter does not comply with the associated constraints.
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: (ExceptionMessage)
  }


newtype LambdaFunctionArn = LambdaFunctionArn String


-- | <p>AWS Lambda throttled your account, please contact AWS Support</p>
newtype LambdaThrottledException = LambdaThrottledException 
  { "Message'" :: (ExceptionMessage)
  }


-- | Thrown when the limit on the number of objects or operations has been exceeded.
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: (ExceptionMessage)
  }


-- | Request for a list of datasets for an identity.
newtype ListDatasetsRequest = ListDatasetsRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "NextToken" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (IntegerString)
  }


-- | Returned for a successful ListDatasets request.
newtype ListDatasetsResponse = ListDatasetsResponse 
  { "Datasets" :: NullOrUndefined (DatasetList)
  , "Count" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | A request for usage information on an identity pool.
newtype ListIdentityPoolUsageRequest = ListIdentityPoolUsageRequest 
  { "NextToken" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (IntegerString)
  }


-- | Returned for a successful ListIdentityPoolUsage request.
newtype ListIdentityPoolUsageResponse = ListIdentityPoolUsageResponse 
  { "IdentityPoolUsages" :: NullOrUndefined (IdentityPoolUsageList)
  , "MaxResults" :: NullOrUndefined (Int)
  , "Count" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | A request for a list of records.
newtype ListRecordsRequest = ListRecordsRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "DatasetName" :: (DatasetName)
  , "LastSyncCount" :: NullOrUndefined (Number)
  , "NextToken" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (IntegerString)
  , "SyncSessionToken" :: NullOrUndefined (SyncSessionToken)
  }


-- | Returned for a successful ListRecordsRequest.
newtype ListRecordsResponse = ListRecordsResponse 
  { "Records" :: NullOrUndefined (RecordList)
  , "NextToken" :: NullOrUndefined (String)
  , "Count" :: NullOrUndefined (Int)
  , "DatasetSyncCount" :: NullOrUndefined (Number)
  , "LastModifiedBy" :: NullOrUndefined (String)
  , "MergedDatasetNames" :: NullOrUndefined (MergedDatasetNameList)
  , "DatasetExists" :: NullOrUndefined (Boolean)
  , "DatasetDeletedAfterRequestedSyncCount" :: NullOrUndefined (Boolean)
  , "SyncSessionToken" :: NullOrUndefined (String)
  }


newtype MergedDatasetNameList = MergedDatasetNameList (Array String)


-- | Thrown when a user is not authorized to access the requested resource.
newtype NotAuthorizedException = NotAuthorizedException 
  { "Message'" :: (ExceptionMessage)
  }


newtype Operation = Operation String


newtype Platform = Platform String


-- | <p>Configuration options to be applied to the identity pool.</p>
newtype PushSync = PushSync 
  { "ApplicationArns" :: NullOrUndefined (ApplicationArnList)
  , "RoleArn" :: NullOrUndefined (AssumeRoleArn)
  }


newtype PushToken = PushToken String


-- | The basic data structure of a dataset.
newtype Record'' = Record'' 
  { "Key" :: NullOrUndefined (RecordKey)
  , "Value" :: NullOrUndefined (RecordValue)
  , "SyncCount" :: NullOrUndefined (Number)
  , "LastModifiedDate" :: NullOrUndefined (Date)
  , "LastModifiedBy" :: NullOrUndefined (String)
  , "DeviceLastModifiedDate" :: NullOrUndefined (Date)
  }


newtype RecordKey = RecordKey String


newtype RecordList = RecordList (Array Record'')


-- | An update operation for a record.
newtype RecordPatch = RecordPatch 
  { "Op" :: (Operation)
  , "Key" :: (RecordKey)
  , "Value" :: NullOrUndefined (RecordValue)
  , "SyncCount" :: (Number)
  , "DeviceLastModifiedDate" :: NullOrUndefined (Date)
  }


newtype RecordPatchList = RecordPatchList (Array RecordPatch)


newtype RecordValue = RecordValue String


-- | <p>A request to RegisterDevice.</p>
newtype RegisterDeviceRequest = RegisterDeviceRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "Platform" :: (Platform)
  , "Token" :: (PushToken)
  }


-- | <p>Response to a RegisterDevice request.</p>
newtype RegisterDeviceResponse = RegisterDeviceResponse 
  { "DeviceId" :: NullOrUndefined (DeviceId)
  }


-- | Thrown if an update can't be applied because the resource was changed by another call and this would result in a conflict.
newtype ResourceConflictException = ResourceConflictException 
  { "Message'" :: (ExceptionMessage)
  }


-- | Thrown if the resource doesn't exist.
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: (ExceptionMessage)
  }


-- | <p>A request to configure Cognito Events"</p>"
newtype SetCognitoEventsRequest = SetCognitoEventsRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "Events" :: (Events)
  }


-- | <p>The input for the SetIdentityPoolConfiguration operation.</p>
newtype SetIdentityPoolConfigurationRequest = SetIdentityPoolConfigurationRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "PushSync" :: NullOrUndefined (PushSync)
  , "CognitoStreams" :: NullOrUndefined (CognitoStreams)
  }


-- | <p>The output for the SetIdentityPoolConfiguration operation</p>
newtype SetIdentityPoolConfigurationResponse = SetIdentityPoolConfigurationResponse 
  { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId)
  , "PushSync" :: NullOrUndefined (PushSync)
  , "CognitoStreams" :: NullOrUndefined (CognitoStreams)
  }


newtype StreamName = StreamName String


newtype StreamingStatus = StreamingStatus String


-- | <p>A request to SubscribeToDatasetRequest.</p>
newtype SubscribeToDatasetRequest = SubscribeToDatasetRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "DatasetName" :: (DatasetName)
  , "DeviceId" :: (DeviceId)
  }


-- | <p>Response to a SubscribeToDataset request.</p>
newtype SubscribeToDatasetResponse = SubscribeToDatasetResponse 
  { 
  }


newtype SyncSessionToken = SyncSessionToken String


-- | Thrown if the request is throttled.
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message'" :: (ExceptionMessage)
  }


-- | <p>A request to UnsubscribeFromDataset.</p>
newtype UnsubscribeFromDatasetRequest = UnsubscribeFromDatasetRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "DatasetName" :: (DatasetName)
  , "DeviceId" :: (DeviceId)
  }


-- | <p>Response to an UnsubscribeFromDataset request.</p>
newtype UnsubscribeFromDatasetResponse = UnsubscribeFromDatasetResponse 
  { 
  }


-- | A request to post updates to records or add and delete records for a dataset and user.
newtype UpdateRecordsRequest = UpdateRecordsRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "DatasetName" :: (DatasetName)
  , "DeviceId" :: NullOrUndefined (DeviceId)
  , "RecordPatches" :: NullOrUndefined (RecordPatchList)
  , "SyncSessionToken" :: (SyncSessionToken)
  , "ClientContext" :: NullOrUndefined (ClientContext)
  }


-- | Returned for a successful UpdateRecordsRequest.
newtype UpdateRecordsResponse = UpdateRecordsResponse 
  { "Records" :: NullOrUndefined (RecordList)
  }
