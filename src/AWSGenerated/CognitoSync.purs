

-- | <fullname>Amazon Cognito Sync</fullname> <p>Amazon Cognito Sync provides an AWS service and client library that enable cross-device syncing of application-related user data. High-level client libraries are available for both iOS and Android. You can use these libraries to persist data locally so that it's available even if the device is offline. Developer credentials don't need to be stored on the mobile device to access the service. You can use Amazon Cognito to obtain a normalized user ID and credentials. User data is persisted in a dataset that can store up to 1 MB of key-value pairs, and you can have up to 20 datasets per user identity.</p> <p>With Amazon Cognito Sync, the data stored for each identity is accessible only to credentials assigned to that identity. In order to use the Cognito Sync service, you need to make API calls using credentials retrieved with <a href="http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/Welcome.html">Amazon Cognito Identity service</a>.</p> <p>If you want to use Cognito Sync in an Android or iOS application, you will probably want to make API calls via the AWS Mobile SDK. To learn more, see the <a href="http://docs.aws.amazon.com/mobile/sdkforandroid/developerguide/cognito-sync.html">Developer Guide for Android</a> and the <a href="http://docs.aws.amazon.com/mobile/sdkforios/developerguide/cognito-sync.html">Developer Guide for iOS</a>.</p>
module AWS.CognitoSync where

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

serviceName = "CognitoSync" :: String


-- | <p>Initiates a bulk publish of all existing datasets for an Identity Pool to the configured stream. Customers are limited to one successful bulk publish per 24 hours. Bulk publish is an asynchronous request, customers can see the status of the request via the GetBulkPublishDetails operation.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
bulkPublish :: forall eff. BulkPublishRequest -> Aff (exception :: EXCEPTION | eff) BulkPublishResponse
bulkPublish = Request.request serviceName "bulkPublish" 


-- | <p>Deletes the specific dataset. The dataset will be deleted permanently, and the action can't be undone. Datasets that this dataset was merged with will no longer report the merge. Any subsequent operation on this dataset will result in a ResourceNotFoundException.</p> <p>This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.</p>
deleteDataset :: forall eff. DeleteDatasetRequest -> Aff (exception :: EXCEPTION | eff) DeleteDatasetResponse
deleteDataset = Request.request serviceName "deleteDataset" 


-- | <p>Gets meta data about a dataset by identity and dataset name. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.</p> <p>This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use Cognito Identity credentials to make this API call.</p>
describeDataset :: forall eff. DescribeDatasetRequest -> Aff (exception :: EXCEPTION | eff) DescribeDatasetResponse
describeDataset = Request.request serviceName "describeDataset" 


-- | <p>Gets usage details (for example, data storage) about a particular identity pool.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
describeIdentityPoolUsage :: forall eff. DescribeIdentityPoolUsageRequest -> Aff (exception :: EXCEPTION | eff) DescribeIdentityPoolUsageResponse
describeIdentityPoolUsage = Request.request serviceName "describeIdentityPoolUsage" 


-- | <p>Gets usage information for an identity, including number of datasets and data usage.</p> <p>This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.</p>
describeIdentityUsage :: forall eff. DescribeIdentityUsageRequest -> Aff (exception :: EXCEPTION | eff) DescribeIdentityUsageResponse
describeIdentityUsage = Request.request serviceName "describeIdentityUsage" 


-- | <p>Get the status of the last BulkPublish operation for an identity pool.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
getBulkPublishDetails :: forall eff. GetBulkPublishDetailsRequest -> Aff (exception :: EXCEPTION | eff) GetBulkPublishDetailsResponse
getBulkPublishDetails = Request.request serviceName "getBulkPublishDetails" 


-- | <p>Gets the events and the corresponding Lambda functions associated with an identity pool.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
getCognitoEvents :: forall eff. GetCognitoEventsRequest -> Aff (exception :: EXCEPTION | eff) GetCognitoEventsResponse
getCognitoEvents = Request.request serviceName "getCognitoEvents" 


-- | <p>Gets the configuration settings of an identity pool.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
getIdentityPoolConfiguration :: forall eff. GetIdentityPoolConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetIdentityPoolConfigurationResponse
getIdentityPoolConfiguration = Request.request serviceName "getIdentityPoolConfiguration" 


-- | <p>Lists datasets for an identity. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.</p> <p>ListDatasets can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use the Cognito Identity credentials to make this API call.</p>
listDatasets :: forall eff. ListDatasetsRequest -> Aff (exception :: EXCEPTION | eff) ListDatasetsResponse
listDatasets = Request.request serviceName "listDatasets" 


-- | <p>Gets a list of identity pools registered with Cognito.</p> <p>ListIdentityPoolUsage can only be called with developer credentials. You cannot make this API call with the temporary user credentials provided by Cognito Identity.</p>
listIdentityPoolUsage :: forall eff. ListIdentityPoolUsageRequest -> Aff (exception :: EXCEPTION | eff) ListIdentityPoolUsageResponse
listIdentityPoolUsage = Request.request serviceName "listIdentityPoolUsage" 


-- | <p>Gets paginated records, optionally changed after a particular sync count for a dataset and identity. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.</p> <p>ListRecords can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use Cognito Identity credentials to make this API call.</p>
listRecords :: forall eff. ListRecordsRequest -> Aff (exception :: EXCEPTION | eff) ListRecordsResponse
listRecords = Request.request serviceName "listRecords" 


-- | <p>Registers a device to receive push sync notifications.</p> <p>This API can only be called with temporary credentials provided by Cognito Identity. You cannot call this API with developer credentials.</p>
registerDevice :: forall eff. RegisterDeviceRequest -> Aff (exception :: EXCEPTION | eff) RegisterDeviceResponse
registerDevice = Request.request serviceName "registerDevice" 


-- | <p>Sets the AWS Lambda function for a given event type for an identity pool. This request only updates the key/value pair specified. Other key/values pairs are not updated. To remove a key value pair, pass a empty value for the particular key.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
setCognitoEvents :: forall eff. SetCognitoEventsRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setCognitoEvents = Request.request serviceName "setCognitoEvents" 


-- | <p>Sets the necessary configuration for push sync.</p> <p>This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.</p>
setIdentityPoolConfiguration :: forall eff. SetIdentityPoolConfigurationRequest -> Aff (exception :: EXCEPTION | eff) SetIdentityPoolConfigurationResponse
setIdentityPoolConfiguration = Request.request serviceName "setIdentityPoolConfiguration" 


-- | <p>Subscribes to receive notifications when a dataset is modified by another device.</p> <p>This API can only be called with temporary credentials provided by Cognito Identity. You cannot call this API with developer credentials.</p>
subscribeToDataset :: forall eff. SubscribeToDatasetRequest -> Aff (exception :: EXCEPTION | eff) SubscribeToDatasetResponse
subscribeToDataset = Request.request serviceName "subscribeToDataset" 


-- | <p>Unsubscribes from receiving notifications when a dataset is modified by another device.</p> <p>This API can only be called with temporary credentials provided by Cognito Identity. You cannot call this API with developer credentials.</p>
unsubscribeFromDataset :: forall eff. UnsubscribeFromDatasetRequest -> Aff (exception :: EXCEPTION | eff) UnsubscribeFromDatasetResponse
unsubscribeFromDataset = Request.request serviceName "unsubscribeFromDataset" 


-- | <p>Posts updates to records and adds and deletes records for a dataset and user.</p> <p>The sync count in the record patch is your last known sync count for that record. The server will reject an UpdateRecords request with a ResourceConflictException if you try to patch a record with a new value but a stale sync count.</p> <p>For example, if the sync count on the server is 5 for a key called highScore and you try and submit a new highScore with sync count of 4, the request will be rejected. To obtain the current sync count for a record, call ListRecords. On a successful update of the record, the response returns the new sync count for that record. You should present that sync count the next time you try to update that same record. When the record does not exist, specify the sync count as 0.</p> <p>This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.</p>
updateRecords :: forall eff. UpdateRecordsRequest -> Aff (exception :: EXCEPTION | eff) UpdateRecordsResponse
updateRecords = Request.request serviceName "updateRecords" 


-- | An exception thrown when a bulk publish operation is requested less than 24 hours after a previous bulk publish operation completed successfully.
newtype AlreadyStreamedException = AlreadyStreamedException 
  { "Message'" :: (ExceptionMessage)
  }
derive instance newtypeAlreadyStreamedException :: Newtype AlreadyStreamedException _
derive instance repGenericAlreadyStreamedException :: Generic AlreadyStreamedException _
instance showAlreadyStreamedException :: Show AlreadyStreamedException where
  show = genericShow
instance decodeAlreadyStreamedException :: Decode AlreadyStreamedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlreadyStreamedException :: Encode AlreadyStreamedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationArn = ApplicationArn String
derive instance newtypeApplicationArn :: Newtype ApplicationArn _
derive instance repGenericApplicationArn :: Generic ApplicationArn _
instance showApplicationArn :: Show ApplicationArn where
  show = genericShow
instance decodeApplicationArn :: Decode ApplicationArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationArn :: Encode ApplicationArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationArnList = ApplicationArnList (Array ApplicationArn)
derive instance newtypeApplicationArnList :: Newtype ApplicationArnList _
derive instance repGenericApplicationArnList :: Generic ApplicationArnList _
instance showApplicationArnList :: Show ApplicationArnList where
  show = genericShow
instance decodeApplicationArnList :: Decode ApplicationArnList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationArnList :: Encode ApplicationArnList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssumeRoleArn = AssumeRoleArn String
derive instance newtypeAssumeRoleArn :: Newtype AssumeRoleArn _
derive instance repGenericAssumeRoleArn :: Generic AssumeRoleArn _
instance showAssumeRoleArn :: Show AssumeRoleArn where
  show = genericShow
instance decodeAssumeRoleArn :: Decode AssumeRoleArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssumeRoleArn :: Encode AssumeRoleArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The input for the BulkPublish operation.
newtype BulkPublishRequest = BulkPublishRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeBulkPublishRequest :: Newtype BulkPublishRequest _
derive instance repGenericBulkPublishRequest :: Generic BulkPublishRequest _
instance showBulkPublishRequest :: Show BulkPublishRequest where
  show = genericShow
instance decodeBulkPublishRequest :: Decode BulkPublishRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBulkPublishRequest :: Encode BulkPublishRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The output for the BulkPublish operation.
newtype BulkPublishResponse = BulkPublishResponse 
  { "IdentityPoolId" :: NullOrUndefined.NullOrUndefined (IdentityPoolId)
  }
derive instance newtypeBulkPublishResponse :: Newtype BulkPublishResponse _
derive instance repGenericBulkPublishResponse :: Generic BulkPublishResponse _
instance showBulkPublishResponse :: Show BulkPublishResponse where
  show = genericShow
instance decodeBulkPublishResponse :: Decode BulkPublishResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBulkPublishResponse :: Encode BulkPublishResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BulkPublishStatus = BulkPublishStatus String
derive instance newtypeBulkPublishStatus :: Newtype BulkPublishStatus _
derive instance repGenericBulkPublishStatus :: Generic BulkPublishStatus _
instance showBulkPublishStatus :: Show BulkPublishStatus where
  show = genericShow
instance decodeBulkPublishStatus :: Decode BulkPublishStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBulkPublishStatus :: Encode BulkPublishStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientContext = ClientContext String
derive instance newtypeClientContext :: Newtype ClientContext _
derive instance repGenericClientContext :: Generic ClientContext _
instance showClientContext :: Show ClientContext where
  show = genericShow
instance decodeClientContext :: Decode ClientContext where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientContext :: Encode ClientContext where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CognitoEventType = CognitoEventType String
derive instance newtypeCognitoEventType :: Newtype CognitoEventType _
derive instance repGenericCognitoEventType :: Generic CognitoEventType _
instance showCognitoEventType :: Show CognitoEventType where
  show = genericShow
instance decodeCognitoEventType :: Decode CognitoEventType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCognitoEventType :: Encode CognitoEventType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Configuration options for configure Cognito streams.
newtype CognitoStreams = CognitoStreams 
  { "StreamName" :: NullOrUndefined.NullOrUndefined (StreamName)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (AssumeRoleArn)
  , "StreamingStatus" :: NullOrUndefined.NullOrUndefined (StreamingStatus)
  }
derive instance newtypeCognitoStreams :: Newtype CognitoStreams _
derive instance repGenericCognitoStreams :: Generic CognitoStreams _
instance showCognitoStreams :: Show CognitoStreams where
  show = genericShow
instance decodeCognitoStreams :: Decode CognitoStreams where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCognitoStreams :: Encode CognitoStreams where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thrown if there are parallel requests to modify a resource.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: (String)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _
derive instance repGenericConcurrentModificationException :: Generic ConcurrentModificationException _
instance showConcurrentModificationException :: Show ConcurrentModificationException where
  show = genericShow
instance decodeConcurrentModificationException :: Decode ConcurrentModificationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConcurrentModificationException :: Encode ConcurrentModificationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
newtype Dataset = Dataset 
  { "IdentityId" :: NullOrUndefined.NullOrUndefined (IdentityId)
  , "DatasetName" :: NullOrUndefined.NullOrUndefined (DatasetName)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (Date)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (Date)
  , "LastModifiedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "DataStorage" :: NullOrUndefined.NullOrUndefined (Number)
  , "NumRecords" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeDataset :: Newtype Dataset _
derive instance repGenericDataset :: Generic Dataset _
instance showDataset :: Show Dataset where
  show = genericShow
instance decodeDataset :: Decode Dataset where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataset :: Encode Dataset where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DatasetList = DatasetList (Array Dataset)
derive instance newtypeDatasetList :: Newtype DatasetList _
derive instance repGenericDatasetList :: Generic DatasetList _
instance showDatasetList :: Show DatasetList where
  show = genericShow
instance decodeDatasetList :: Decode DatasetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDatasetList :: Encode DatasetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DatasetName = DatasetName String
derive instance newtypeDatasetName :: Newtype DatasetName _
derive instance repGenericDatasetName :: Generic DatasetName _
instance showDatasetName :: Show DatasetName where
  show = genericShow
instance decodeDatasetName :: Decode DatasetName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDatasetName :: Encode DatasetName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _
derive instance repGenericDate :: Generic Date _
instance showDate :: Show Date where
  show = genericShow
instance decodeDate :: Decode Date where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDate :: Encode Date where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A request to delete the specific dataset.
newtype DeleteDatasetRequest = DeleteDatasetRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "DatasetName" :: (DatasetName)
  }
derive instance newtypeDeleteDatasetRequest :: Newtype DeleteDatasetRequest _
derive instance repGenericDeleteDatasetRequest :: Generic DeleteDatasetRequest _
instance showDeleteDatasetRequest :: Show DeleteDatasetRequest where
  show = genericShow
instance decodeDeleteDatasetRequest :: Decode DeleteDatasetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDatasetRequest :: Encode DeleteDatasetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Response to a successful DeleteDataset request.
newtype DeleteDatasetResponse = DeleteDatasetResponse 
  { "Dataset" :: NullOrUndefined.NullOrUndefined (Dataset)
  }
derive instance newtypeDeleteDatasetResponse :: Newtype DeleteDatasetResponse _
derive instance repGenericDeleteDatasetResponse :: Generic DeleteDatasetResponse _
instance showDeleteDatasetResponse :: Show DeleteDatasetResponse where
  show = genericShow
instance decodeDeleteDatasetResponse :: Decode DeleteDatasetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDatasetResponse :: Encode DeleteDatasetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A request for meta data about a dataset (creation date, number of records, size) by owner and dataset name.
newtype DescribeDatasetRequest = DescribeDatasetRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "DatasetName" :: (DatasetName)
  }
derive instance newtypeDescribeDatasetRequest :: Newtype DescribeDatasetRequest _
derive instance repGenericDescribeDatasetRequest :: Generic DescribeDatasetRequest _
instance showDescribeDatasetRequest :: Show DescribeDatasetRequest where
  show = genericShow
instance decodeDescribeDatasetRequest :: Decode DescribeDatasetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDatasetRequest :: Encode DescribeDatasetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Response to a successful DescribeDataset request.
newtype DescribeDatasetResponse = DescribeDatasetResponse 
  { "Dataset" :: NullOrUndefined.NullOrUndefined (Dataset)
  }
derive instance newtypeDescribeDatasetResponse :: Newtype DescribeDatasetResponse _
derive instance repGenericDescribeDatasetResponse :: Generic DescribeDatasetResponse _
instance showDescribeDatasetResponse :: Show DescribeDatasetResponse where
  show = genericShow
instance decodeDescribeDatasetResponse :: Decode DescribeDatasetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDatasetResponse :: Encode DescribeDatasetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A request for usage information about the identity pool.
newtype DescribeIdentityPoolUsageRequest = DescribeIdentityPoolUsageRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeDescribeIdentityPoolUsageRequest :: Newtype DescribeIdentityPoolUsageRequest _
derive instance repGenericDescribeIdentityPoolUsageRequest :: Generic DescribeIdentityPoolUsageRequest _
instance showDescribeIdentityPoolUsageRequest :: Show DescribeIdentityPoolUsageRequest where
  show = genericShow
instance decodeDescribeIdentityPoolUsageRequest :: Decode DescribeIdentityPoolUsageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeIdentityPoolUsageRequest :: Encode DescribeIdentityPoolUsageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Response to a successful DescribeIdentityPoolUsage request.
newtype DescribeIdentityPoolUsageResponse = DescribeIdentityPoolUsageResponse 
  { "IdentityPoolUsage" :: NullOrUndefined.NullOrUndefined (IdentityPoolUsage)
  }
derive instance newtypeDescribeIdentityPoolUsageResponse :: Newtype DescribeIdentityPoolUsageResponse _
derive instance repGenericDescribeIdentityPoolUsageResponse :: Generic DescribeIdentityPoolUsageResponse _
instance showDescribeIdentityPoolUsageResponse :: Show DescribeIdentityPoolUsageResponse where
  show = genericShow
instance decodeDescribeIdentityPoolUsageResponse :: Decode DescribeIdentityPoolUsageResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeIdentityPoolUsageResponse :: Encode DescribeIdentityPoolUsageResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A request for information about the usage of an identity pool.
newtype DescribeIdentityUsageRequest = DescribeIdentityUsageRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  }
derive instance newtypeDescribeIdentityUsageRequest :: Newtype DescribeIdentityUsageRequest _
derive instance repGenericDescribeIdentityUsageRequest :: Generic DescribeIdentityUsageRequest _
instance showDescribeIdentityUsageRequest :: Show DescribeIdentityUsageRequest where
  show = genericShow
instance decodeDescribeIdentityUsageRequest :: Decode DescribeIdentityUsageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeIdentityUsageRequest :: Encode DescribeIdentityUsageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The response to a successful DescribeIdentityUsage request.
newtype DescribeIdentityUsageResponse = DescribeIdentityUsageResponse 
  { "IdentityUsage" :: NullOrUndefined.NullOrUndefined (IdentityUsage)
  }
derive instance newtypeDescribeIdentityUsageResponse :: Newtype DescribeIdentityUsageResponse _
derive instance repGenericDescribeIdentityUsageResponse :: Generic DescribeIdentityUsageResponse _
instance showDescribeIdentityUsageResponse :: Show DescribeIdentityUsageResponse where
  show = genericShow
instance decodeDescribeIdentityUsageResponse :: Decode DescribeIdentityUsageResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeIdentityUsageResponse :: Encode DescribeIdentityUsageResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeviceId = DeviceId String
derive instance newtypeDeviceId :: Newtype DeviceId _
derive instance repGenericDeviceId :: Generic DeviceId _
instance showDeviceId :: Show DeviceId where
  show = genericShow
instance decodeDeviceId :: Decode DeviceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceId :: Encode DeviceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An exception thrown when there is an IN_PROGRESS bulk publish operation for the given identity pool.
newtype DuplicateRequestException = DuplicateRequestException 
  { "Message'" :: (ExceptionMessage)
  }
derive instance newtypeDuplicateRequestException :: Newtype DuplicateRequestException _
derive instance repGenericDuplicateRequestException :: Generic DuplicateRequestException _
instance showDuplicateRequestException :: Show DuplicateRequestException where
  show = genericShow
instance decodeDuplicateRequestException :: Decode DuplicateRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateRequestException :: Encode DuplicateRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Events = Events (StrMap.StrMap LambdaFunctionArn)
derive instance newtypeEvents :: Newtype Events _
derive instance repGenericEvents :: Generic Events _
instance showEvents :: Show Events where
  show = genericShow
instance decodeEvents :: Decode Events where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvents :: Encode Events where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _
derive instance repGenericExceptionMessage :: Generic ExceptionMessage _
instance showExceptionMessage :: Show ExceptionMessage where
  show = genericShow
instance decodeExceptionMessage :: Decode ExceptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExceptionMessage :: Encode ExceptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The input for the GetBulkPublishDetails operation.
newtype GetBulkPublishDetailsRequest = GetBulkPublishDetailsRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeGetBulkPublishDetailsRequest :: Newtype GetBulkPublishDetailsRequest _
derive instance repGenericGetBulkPublishDetailsRequest :: Generic GetBulkPublishDetailsRequest _
instance showGetBulkPublishDetailsRequest :: Show GetBulkPublishDetailsRequest where
  show = genericShow
instance decodeGetBulkPublishDetailsRequest :: Decode GetBulkPublishDetailsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBulkPublishDetailsRequest :: Encode GetBulkPublishDetailsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The output for the GetBulkPublishDetails operation.
newtype GetBulkPublishDetailsResponse = GetBulkPublishDetailsResponse 
  { "IdentityPoolId" :: NullOrUndefined.NullOrUndefined (IdentityPoolId)
  , "BulkPublishStartTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "BulkPublishCompleteTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "BulkPublishStatus" :: NullOrUndefined.NullOrUndefined (BulkPublishStatus)
  , "FailureMessage" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetBulkPublishDetailsResponse :: Newtype GetBulkPublishDetailsResponse _
derive instance repGenericGetBulkPublishDetailsResponse :: Generic GetBulkPublishDetailsResponse _
instance showGetBulkPublishDetailsResponse :: Show GetBulkPublishDetailsResponse where
  show = genericShow
instance decodeGetBulkPublishDetailsResponse :: Decode GetBulkPublishDetailsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBulkPublishDetailsResponse :: Encode GetBulkPublishDetailsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request for a list of the configured Cognito Events</p>
newtype GetCognitoEventsRequest = GetCognitoEventsRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeGetCognitoEventsRequest :: Newtype GetCognitoEventsRequest _
derive instance repGenericGetCognitoEventsRequest :: Generic GetCognitoEventsRequest _
instance showGetCognitoEventsRequest :: Show GetCognitoEventsRequest where
  show = genericShow
instance decodeGetCognitoEventsRequest :: Decode GetCognitoEventsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCognitoEventsRequest :: Encode GetCognitoEventsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from the GetCognitoEvents request</p>
newtype GetCognitoEventsResponse = GetCognitoEventsResponse 
  { "Events" :: NullOrUndefined.NullOrUndefined (Events)
  }
derive instance newtypeGetCognitoEventsResponse :: Newtype GetCognitoEventsResponse _
derive instance repGenericGetCognitoEventsResponse :: Generic GetCognitoEventsResponse _
instance showGetCognitoEventsResponse :: Show GetCognitoEventsResponse where
  show = genericShow
instance decodeGetCognitoEventsResponse :: Decode GetCognitoEventsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCognitoEventsResponse :: Encode GetCognitoEventsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the GetIdentityPoolConfiguration operation.</p>
newtype GetIdentityPoolConfigurationRequest = GetIdentityPoolConfigurationRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeGetIdentityPoolConfigurationRequest :: Newtype GetIdentityPoolConfigurationRequest _
derive instance repGenericGetIdentityPoolConfigurationRequest :: Generic GetIdentityPoolConfigurationRequest _
instance showGetIdentityPoolConfigurationRequest :: Show GetIdentityPoolConfigurationRequest where
  show = genericShow
instance decodeGetIdentityPoolConfigurationRequest :: Decode GetIdentityPoolConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityPoolConfigurationRequest :: Encode GetIdentityPoolConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the GetIdentityPoolConfiguration operation.</p>
newtype GetIdentityPoolConfigurationResponse = GetIdentityPoolConfigurationResponse 
  { "IdentityPoolId" :: NullOrUndefined.NullOrUndefined (IdentityPoolId)
  , "PushSync" :: NullOrUndefined.NullOrUndefined (PushSync)
  , "CognitoStreams" :: NullOrUndefined.NullOrUndefined (CognitoStreams)
  }
derive instance newtypeGetIdentityPoolConfigurationResponse :: Newtype GetIdentityPoolConfigurationResponse _
derive instance repGenericGetIdentityPoolConfigurationResponse :: Generic GetIdentityPoolConfigurationResponse _
instance showGetIdentityPoolConfigurationResponse :: Show GetIdentityPoolConfigurationResponse where
  show = genericShow
instance decodeGetIdentityPoolConfigurationResponse :: Decode GetIdentityPoolConfigurationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityPoolConfigurationResponse :: Encode GetIdentityPoolConfigurationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityId = IdentityId String
derive instance newtypeIdentityId :: Newtype IdentityId _
derive instance repGenericIdentityId :: Generic IdentityId _
instance showIdentityId :: Show IdentityId where
  show = genericShow
instance decodeIdentityId :: Decode IdentityId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityId :: Encode IdentityId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityPoolId = IdentityPoolId String
derive instance newtypeIdentityPoolId :: Newtype IdentityPoolId _
derive instance repGenericIdentityPoolId :: Generic IdentityPoolId _
instance showIdentityPoolId :: Show IdentityPoolId where
  show = genericShow
instance decodeIdentityPoolId :: Decode IdentityPoolId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityPoolId :: Encode IdentityPoolId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Usage information for the identity pool.
newtype IdentityPoolUsage = IdentityPoolUsage 
  { "IdentityPoolId" :: NullOrUndefined.NullOrUndefined (IdentityPoolId)
  , "SyncSessionsCount" :: NullOrUndefined.NullOrUndefined (Number)
  , "DataStorage" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeIdentityPoolUsage :: Newtype IdentityPoolUsage _
derive instance repGenericIdentityPoolUsage :: Generic IdentityPoolUsage _
instance showIdentityPoolUsage :: Show IdentityPoolUsage where
  show = genericShow
instance decodeIdentityPoolUsage :: Decode IdentityPoolUsage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityPoolUsage :: Encode IdentityPoolUsage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityPoolUsageList = IdentityPoolUsageList (Array IdentityPoolUsage)
derive instance newtypeIdentityPoolUsageList :: Newtype IdentityPoolUsageList _
derive instance repGenericIdentityPoolUsageList :: Generic IdentityPoolUsageList _
instance showIdentityPoolUsageList :: Show IdentityPoolUsageList where
  show = genericShow
instance decodeIdentityPoolUsageList :: Decode IdentityPoolUsageList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityPoolUsageList :: Encode IdentityPoolUsageList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Usage information for the identity.
newtype IdentityUsage = IdentityUsage 
  { "IdentityId" :: NullOrUndefined.NullOrUndefined (IdentityId)
  , "IdentityPoolId" :: NullOrUndefined.NullOrUndefined (IdentityPoolId)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (Date)
  , "DatasetCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "DataStorage" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeIdentityUsage :: Newtype IdentityUsage _
derive instance repGenericIdentityUsage :: Generic IdentityUsage _
instance showIdentityUsage :: Show IdentityUsage where
  show = genericShow
instance decodeIdentityUsage :: Decode IdentityUsage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityUsage :: Encode IdentityUsage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntegerString = IntegerString Int
derive instance newtypeIntegerString :: Newtype IntegerString _
derive instance repGenericIntegerString :: Generic IntegerString _
instance showIntegerString :: Show IntegerString where
  show = genericShow
instance decodeIntegerString :: Decode IntegerString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntegerString :: Encode IntegerString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Indicates an internal service error.
newtype InternalErrorException = InternalErrorException 
  { "Message'" :: (ExceptionMessage)
  }
derive instance newtypeInternalErrorException :: Newtype InternalErrorException _
derive instance repGenericInternalErrorException :: Generic InternalErrorException _
instance showInternalErrorException :: Show InternalErrorException where
  show = genericShow
instance decodeInternalErrorException :: Decode InternalErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalErrorException :: Encode InternalErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidConfigurationException = InvalidConfigurationException 
  { "Message'" :: (ExceptionMessage)
  }
derive instance newtypeInvalidConfigurationException :: Newtype InvalidConfigurationException _
derive instance repGenericInvalidConfigurationException :: Generic InvalidConfigurationException _
instance showInvalidConfigurationException :: Show InvalidConfigurationException where
  show = genericShow
instance decodeInvalidConfigurationException :: Decode InvalidConfigurationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidConfigurationException :: Encode InvalidConfigurationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The AWS Lambda function returned invalid output or an exception.</p>
newtype InvalidLambdaFunctionOutputException = InvalidLambdaFunctionOutputException 
  { "Message'" :: (ExceptionMessage)
  }
derive instance newtypeInvalidLambdaFunctionOutputException :: Newtype InvalidLambdaFunctionOutputException _
derive instance repGenericInvalidLambdaFunctionOutputException :: Generic InvalidLambdaFunctionOutputException _
instance showInvalidLambdaFunctionOutputException :: Show InvalidLambdaFunctionOutputException where
  show = genericShow
instance decodeInvalidLambdaFunctionOutputException :: Decode InvalidLambdaFunctionOutputException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidLambdaFunctionOutputException :: Encode InvalidLambdaFunctionOutputException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Thrown when a request parameter does not comply with the associated constraints.
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: (ExceptionMessage)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LambdaFunctionArn = LambdaFunctionArn String
derive instance newtypeLambdaFunctionArn :: Newtype LambdaFunctionArn _
derive instance repGenericLambdaFunctionArn :: Generic LambdaFunctionArn _
instance showLambdaFunctionArn :: Show LambdaFunctionArn where
  show = genericShow
instance decodeLambdaFunctionArn :: Decode LambdaFunctionArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionArn :: Encode LambdaFunctionArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>AWS Lambda throttled your account, please contact AWS Support</p>
newtype LambdaThrottledException = LambdaThrottledException 
  { "Message'" :: (ExceptionMessage)
  }
derive instance newtypeLambdaThrottledException :: Newtype LambdaThrottledException _
derive instance repGenericLambdaThrottledException :: Generic LambdaThrottledException _
instance showLambdaThrottledException :: Show LambdaThrottledException where
  show = genericShow
instance decodeLambdaThrottledException :: Decode LambdaThrottledException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaThrottledException :: Encode LambdaThrottledException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Thrown when the limit on the number of objects or operations has been exceeded.
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: (ExceptionMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Request for a list of datasets for an identity.
newtype ListDatasetsRequest = ListDatasetsRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (IntegerString)
  }
derive instance newtypeListDatasetsRequest :: Newtype ListDatasetsRequest _
derive instance repGenericListDatasetsRequest :: Generic ListDatasetsRequest _
instance showListDatasetsRequest :: Show ListDatasetsRequest where
  show = genericShow
instance decodeListDatasetsRequest :: Decode ListDatasetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDatasetsRequest :: Encode ListDatasetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Returned for a successful ListDatasets request.
newtype ListDatasetsResponse = ListDatasetsResponse 
  { "Datasets" :: NullOrUndefined.NullOrUndefined (DatasetList)
  , "Count" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListDatasetsResponse :: Newtype ListDatasetsResponse _
derive instance repGenericListDatasetsResponse :: Generic ListDatasetsResponse _
instance showListDatasetsResponse :: Show ListDatasetsResponse where
  show = genericShow
instance decodeListDatasetsResponse :: Decode ListDatasetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDatasetsResponse :: Encode ListDatasetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A request for usage information on an identity pool.
newtype ListIdentityPoolUsageRequest = ListIdentityPoolUsageRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (IntegerString)
  }
derive instance newtypeListIdentityPoolUsageRequest :: Newtype ListIdentityPoolUsageRequest _
derive instance repGenericListIdentityPoolUsageRequest :: Generic ListIdentityPoolUsageRequest _
instance showListIdentityPoolUsageRequest :: Show ListIdentityPoolUsageRequest where
  show = genericShow
instance decodeListIdentityPoolUsageRequest :: Decode ListIdentityPoolUsageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIdentityPoolUsageRequest :: Encode ListIdentityPoolUsageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Returned for a successful ListIdentityPoolUsage request.
newtype ListIdentityPoolUsageResponse = ListIdentityPoolUsageResponse 
  { "IdentityPoolUsages" :: NullOrUndefined.NullOrUndefined (IdentityPoolUsageList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (Int)
  , "Count" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListIdentityPoolUsageResponse :: Newtype ListIdentityPoolUsageResponse _
derive instance repGenericListIdentityPoolUsageResponse :: Generic ListIdentityPoolUsageResponse _
instance showListIdentityPoolUsageResponse :: Show ListIdentityPoolUsageResponse where
  show = genericShow
instance decodeListIdentityPoolUsageResponse :: Decode ListIdentityPoolUsageResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIdentityPoolUsageResponse :: Encode ListIdentityPoolUsageResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A request for a list of records.
newtype ListRecordsRequest = ListRecordsRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "DatasetName" :: (DatasetName)
  , "LastSyncCount" :: NullOrUndefined.NullOrUndefined (Number)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (IntegerString)
  , "SyncSessionToken" :: NullOrUndefined.NullOrUndefined (SyncSessionToken)
  }
derive instance newtypeListRecordsRequest :: Newtype ListRecordsRequest _
derive instance repGenericListRecordsRequest :: Generic ListRecordsRequest _
instance showListRecordsRequest :: Show ListRecordsRequest where
  show = genericShow
instance decodeListRecordsRequest :: Decode ListRecordsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRecordsRequest :: Encode ListRecordsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Returned for a successful ListRecordsRequest.
newtype ListRecordsResponse = ListRecordsResponse 
  { "Records" :: NullOrUndefined.NullOrUndefined (RecordList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Count" :: NullOrUndefined.NullOrUndefined (Int)
  , "DatasetSyncCount" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastModifiedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "MergedDatasetNames" :: NullOrUndefined.NullOrUndefined (MergedDatasetNameList)
  , "DatasetExists" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "DatasetDeletedAfterRequestedSyncCount" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "SyncSessionToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListRecordsResponse :: Newtype ListRecordsResponse _
derive instance repGenericListRecordsResponse :: Generic ListRecordsResponse _
instance showListRecordsResponse :: Show ListRecordsResponse where
  show = genericShow
instance decodeListRecordsResponse :: Decode ListRecordsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRecordsResponse :: Encode ListRecordsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MergedDatasetNameList = MergedDatasetNameList (Array String)
derive instance newtypeMergedDatasetNameList :: Newtype MergedDatasetNameList _
derive instance repGenericMergedDatasetNameList :: Generic MergedDatasetNameList _
instance showMergedDatasetNameList :: Show MergedDatasetNameList where
  show = genericShow
instance decodeMergedDatasetNameList :: Decode MergedDatasetNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMergedDatasetNameList :: Encode MergedDatasetNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Thrown when a user is not authorized to access the requested resource.
newtype NotAuthorizedException = NotAuthorizedException 
  { "Message'" :: (ExceptionMessage)
  }
derive instance newtypeNotAuthorizedException :: Newtype NotAuthorizedException _
derive instance repGenericNotAuthorizedException :: Generic NotAuthorizedException _
instance showNotAuthorizedException :: Show NotAuthorizedException where
  show = genericShow
instance decodeNotAuthorizedException :: Decode NotAuthorizedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotAuthorizedException :: Encode NotAuthorizedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Operation = Operation String
derive instance newtypeOperation :: Newtype Operation _
derive instance repGenericOperation :: Generic Operation _
instance showOperation :: Show Operation where
  show = genericShow
instance decodeOperation :: Decode Operation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperation :: Encode Operation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Platform = Platform String
derive instance newtypePlatform :: Newtype Platform _
derive instance repGenericPlatform :: Generic Platform _
instance showPlatform :: Show Platform where
  show = genericShow
instance decodePlatform :: Decode Platform where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatform :: Encode Platform where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration options to be applied to the identity pool.</p>
newtype PushSync = PushSync 
  { "ApplicationArns" :: NullOrUndefined.NullOrUndefined (ApplicationArnList)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (AssumeRoleArn)
  }
derive instance newtypePushSync :: Newtype PushSync _
derive instance repGenericPushSync :: Generic PushSync _
instance showPushSync :: Show PushSync where
  show = genericShow
instance decodePushSync :: Decode PushSync where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePushSync :: Encode PushSync where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PushToken = PushToken String
derive instance newtypePushToken :: Newtype PushToken _
derive instance repGenericPushToken :: Generic PushToken _
instance showPushToken :: Show PushToken where
  show = genericShow
instance decodePushToken :: Decode PushToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePushToken :: Encode PushToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The basic data structure of a dataset.
newtype Record'' = Record'' 
  { "Key" :: NullOrUndefined.NullOrUndefined (RecordKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (RecordValue)
  , "SyncCount" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (Date)
  , "LastModifiedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "DeviceLastModifiedDate" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeRecord'' :: Newtype Record'' _
derive instance repGenericRecord'' :: Generic Record'' _
instance showRecord'' :: Show Record'' where
  show = genericShow
instance decodeRecord'' :: Decode Record'' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecord'' :: Encode Record'' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordKey = RecordKey String
derive instance newtypeRecordKey :: Newtype RecordKey _
derive instance repGenericRecordKey :: Generic RecordKey _
instance showRecordKey :: Show RecordKey where
  show = genericShow
instance decodeRecordKey :: Decode RecordKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordKey :: Encode RecordKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordList = RecordList (Array Record'')
derive instance newtypeRecordList :: Newtype RecordList _
derive instance repGenericRecordList :: Generic RecordList _
instance showRecordList :: Show RecordList where
  show = genericShow
instance decodeRecordList :: Decode RecordList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordList :: Encode RecordList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An update operation for a record.
newtype RecordPatch = RecordPatch 
  { "Op" :: (Operation)
  , "Key" :: (RecordKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (RecordValue)
  , "SyncCount" :: (Number)
  , "DeviceLastModifiedDate" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeRecordPatch :: Newtype RecordPatch _
derive instance repGenericRecordPatch :: Generic RecordPatch _
instance showRecordPatch :: Show RecordPatch where
  show = genericShow
instance decodeRecordPatch :: Decode RecordPatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordPatch :: Encode RecordPatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordPatchList = RecordPatchList (Array RecordPatch)
derive instance newtypeRecordPatchList :: Newtype RecordPatchList _
derive instance repGenericRecordPatchList :: Generic RecordPatchList _
instance showRecordPatchList :: Show RecordPatchList where
  show = genericShow
instance decodeRecordPatchList :: Decode RecordPatchList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordPatchList :: Encode RecordPatchList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordValue = RecordValue String
derive instance newtypeRecordValue :: Newtype RecordValue _
derive instance repGenericRecordValue :: Generic RecordValue _
instance showRecordValue :: Show RecordValue where
  show = genericShow
instance decodeRecordValue :: Decode RecordValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordValue :: Encode RecordValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to RegisterDevice.</p>
newtype RegisterDeviceRequest = RegisterDeviceRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "Platform" :: (Platform)
  , "Token" :: (PushToken)
  }
derive instance newtypeRegisterDeviceRequest :: Newtype RegisterDeviceRequest _
derive instance repGenericRegisterDeviceRequest :: Generic RegisterDeviceRequest _
instance showRegisterDeviceRequest :: Show RegisterDeviceRequest where
  show = genericShow
instance decodeRegisterDeviceRequest :: Decode RegisterDeviceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterDeviceRequest :: Encode RegisterDeviceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response to a RegisterDevice request.</p>
newtype RegisterDeviceResponse = RegisterDeviceResponse 
  { "DeviceId" :: NullOrUndefined.NullOrUndefined (DeviceId)
  }
derive instance newtypeRegisterDeviceResponse :: Newtype RegisterDeviceResponse _
derive instance repGenericRegisterDeviceResponse :: Generic RegisterDeviceResponse _
instance showRegisterDeviceResponse :: Show RegisterDeviceResponse where
  show = genericShow
instance decodeRegisterDeviceResponse :: Decode RegisterDeviceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterDeviceResponse :: Encode RegisterDeviceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Thrown if an update can't be applied because the resource was changed by another call and this would result in a conflict.
newtype ResourceConflictException = ResourceConflictException 
  { "Message'" :: (ExceptionMessage)
  }
derive instance newtypeResourceConflictException :: Newtype ResourceConflictException _
derive instance repGenericResourceConflictException :: Generic ResourceConflictException _
instance showResourceConflictException :: Show ResourceConflictException where
  show = genericShow
instance decodeResourceConflictException :: Decode ResourceConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceConflictException :: Encode ResourceConflictException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Thrown if the resource doesn't exist.
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: (ExceptionMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to configure Cognito Events"</p>"
newtype SetCognitoEventsRequest = SetCognitoEventsRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "Events" :: (Events)
  }
derive instance newtypeSetCognitoEventsRequest :: Newtype SetCognitoEventsRequest _
derive instance repGenericSetCognitoEventsRequest :: Generic SetCognitoEventsRequest _
instance showSetCognitoEventsRequest :: Show SetCognitoEventsRequest where
  show = genericShow
instance decodeSetCognitoEventsRequest :: Decode SetCognitoEventsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetCognitoEventsRequest :: Encode SetCognitoEventsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the SetIdentityPoolConfiguration operation.</p>
newtype SetIdentityPoolConfigurationRequest = SetIdentityPoolConfigurationRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "PushSync" :: NullOrUndefined.NullOrUndefined (PushSync)
  , "CognitoStreams" :: NullOrUndefined.NullOrUndefined (CognitoStreams)
  }
derive instance newtypeSetIdentityPoolConfigurationRequest :: Newtype SetIdentityPoolConfigurationRequest _
derive instance repGenericSetIdentityPoolConfigurationRequest :: Generic SetIdentityPoolConfigurationRequest _
instance showSetIdentityPoolConfigurationRequest :: Show SetIdentityPoolConfigurationRequest where
  show = genericShow
instance decodeSetIdentityPoolConfigurationRequest :: Decode SetIdentityPoolConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityPoolConfigurationRequest :: Encode SetIdentityPoolConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the SetIdentityPoolConfiguration operation</p>
newtype SetIdentityPoolConfigurationResponse = SetIdentityPoolConfigurationResponse 
  { "IdentityPoolId" :: NullOrUndefined.NullOrUndefined (IdentityPoolId)
  , "PushSync" :: NullOrUndefined.NullOrUndefined (PushSync)
  , "CognitoStreams" :: NullOrUndefined.NullOrUndefined (CognitoStreams)
  }
derive instance newtypeSetIdentityPoolConfigurationResponse :: Newtype SetIdentityPoolConfigurationResponse _
derive instance repGenericSetIdentityPoolConfigurationResponse :: Generic SetIdentityPoolConfigurationResponse _
instance showSetIdentityPoolConfigurationResponse :: Show SetIdentityPoolConfigurationResponse where
  show = genericShow
instance decodeSetIdentityPoolConfigurationResponse :: Decode SetIdentityPoolConfigurationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityPoolConfigurationResponse :: Encode SetIdentityPoolConfigurationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamName = StreamName String
derive instance newtypeStreamName :: Newtype StreamName _
derive instance repGenericStreamName :: Generic StreamName _
instance showStreamName :: Show StreamName where
  show = genericShow
instance decodeStreamName :: Decode StreamName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamName :: Encode StreamName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamingStatus = StreamingStatus String
derive instance newtypeStreamingStatus :: Newtype StreamingStatus _
derive instance repGenericStreamingStatus :: Generic StreamingStatus _
instance showStreamingStatus :: Show StreamingStatus where
  show = genericShow
instance decodeStreamingStatus :: Decode StreamingStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamingStatus :: Encode StreamingStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to SubscribeToDatasetRequest.</p>
newtype SubscribeToDatasetRequest = SubscribeToDatasetRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "DatasetName" :: (DatasetName)
  , "DeviceId" :: (DeviceId)
  }
derive instance newtypeSubscribeToDatasetRequest :: Newtype SubscribeToDatasetRequest _
derive instance repGenericSubscribeToDatasetRequest :: Generic SubscribeToDatasetRequest _
instance showSubscribeToDatasetRequest :: Show SubscribeToDatasetRequest where
  show = genericShow
instance decodeSubscribeToDatasetRequest :: Decode SubscribeToDatasetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscribeToDatasetRequest :: Encode SubscribeToDatasetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response to a SubscribeToDataset request.</p>
newtype SubscribeToDatasetResponse = SubscribeToDatasetResponse Types.NoArguments
derive instance newtypeSubscribeToDatasetResponse :: Newtype SubscribeToDatasetResponse _
derive instance repGenericSubscribeToDatasetResponse :: Generic SubscribeToDatasetResponse _
instance showSubscribeToDatasetResponse :: Show SubscribeToDatasetResponse where
  show = genericShow
instance decodeSubscribeToDatasetResponse :: Decode SubscribeToDatasetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscribeToDatasetResponse :: Encode SubscribeToDatasetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SyncSessionToken = SyncSessionToken String
derive instance newtypeSyncSessionToken :: Newtype SyncSessionToken _
derive instance repGenericSyncSessionToken :: Generic SyncSessionToken _
instance showSyncSessionToken :: Show SyncSessionToken where
  show = genericShow
instance decodeSyncSessionToken :: Decode SyncSessionToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSyncSessionToken :: Encode SyncSessionToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Thrown if the request is throttled.
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message'" :: (ExceptionMessage)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _
derive instance repGenericTooManyRequestsException :: Generic TooManyRequestsException _
instance showTooManyRequestsException :: Show TooManyRequestsException where
  show = genericShow
instance decodeTooManyRequestsException :: Decode TooManyRequestsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyRequestsException :: Encode TooManyRequestsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to UnsubscribeFromDataset.</p>
newtype UnsubscribeFromDatasetRequest = UnsubscribeFromDatasetRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "DatasetName" :: (DatasetName)
  , "DeviceId" :: (DeviceId)
  }
derive instance newtypeUnsubscribeFromDatasetRequest :: Newtype UnsubscribeFromDatasetRequest _
derive instance repGenericUnsubscribeFromDatasetRequest :: Generic UnsubscribeFromDatasetRequest _
instance showUnsubscribeFromDatasetRequest :: Show UnsubscribeFromDatasetRequest where
  show = genericShow
instance decodeUnsubscribeFromDatasetRequest :: Decode UnsubscribeFromDatasetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsubscribeFromDatasetRequest :: Encode UnsubscribeFromDatasetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response to an UnsubscribeFromDataset request.</p>
newtype UnsubscribeFromDatasetResponse = UnsubscribeFromDatasetResponse Types.NoArguments
derive instance newtypeUnsubscribeFromDatasetResponse :: Newtype UnsubscribeFromDatasetResponse _
derive instance repGenericUnsubscribeFromDatasetResponse :: Generic UnsubscribeFromDatasetResponse _
instance showUnsubscribeFromDatasetResponse :: Show UnsubscribeFromDatasetResponse where
  show = genericShow
instance decodeUnsubscribeFromDatasetResponse :: Decode UnsubscribeFromDatasetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsubscribeFromDatasetResponse :: Encode UnsubscribeFromDatasetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A request to post updates to records or add and delete records for a dataset and user.
newtype UpdateRecordsRequest = UpdateRecordsRequest 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: (IdentityId)
  , "DatasetName" :: (DatasetName)
  , "DeviceId" :: NullOrUndefined.NullOrUndefined (DeviceId)
  , "RecordPatches" :: NullOrUndefined.NullOrUndefined (RecordPatchList)
  , "SyncSessionToken" :: (SyncSessionToken)
  , "ClientContext" :: NullOrUndefined.NullOrUndefined (ClientContext)
  }
derive instance newtypeUpdateRecordsRequest :: Newtype UpdateRecordsRequest _
derive instance repGenericUpdateRecordsRequest :: Generic UpdateRecordsRequest _
instance showUpdateRecordsRequest :: Show UpdateRecordsRequest where
  show = genericShow
instance decodeUpdateRecordsRequest :: Decode UpdateRecordsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRecordsRequest :: Encode UpdateRecordsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Returned for a successful UpdateRecordsRequest.
newtype UpdateRecordsResponse = UpdateRecordsResponse 
  { "Records" :: NullOrUndefined.NullOrUndefined (RecordList)
  }
derive instance newtypeUpdateRecordsResponse :: Newtype UpdateRecordsResponse _
derive instance repGenericUpdateRecordsResponse :: Generic UpdateRecordsResponse _
instance showUpdateRecordsResponse :: Show UpdateRecordsResponse where
  show = genericShow
instance decodeUpdateRecordsResponse :: Decode UpdateRecordsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRecordsResponse :: Encode UpdateRecordsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
