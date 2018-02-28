

-- | Provides AWS Marketplace business intelligence data on-demand.
module AWS.MarketplaceCommerceAnalytics where

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

serviceName = "MarketplaceCommerceAnalytics" :: String


-- | Given a data set type and data set publication date, asynchronously publishes the requested data set to the specified S3 bucket and notifies the specified SNS topic once the data is available. Returns a unique request identifier that can be used to correlate requests with notifications from the SNS topic. Data sets will be published in comma-separated values (CSV) format with the file name {data_set_type}_YYYY-MM-DD.csv. If a file with the same name already exists (e.g. if the same data set is requested twice), the original file will be overwritten by the new file. Requires a Role with an attached permissions policy providing Allow permissions for the following actions: s3:PutObject, s3:GetBucketLocation, sns:GetTopicAttributes, sns:Publish, iam:GetRolePolicy.
generateDataSet :: forall eff. GenerateDataSetRequest -> Aff (exception :: EXCEPTION | eff) GenerateDataSetResult
generateDataSet = Request.request serviceName "generateDataSet" 


-- | Given a data set type and a from date, asynchronously publishes the requested customer support data to the specified S3 bucket and notifies the specified SNS topic once the data is available. Returns a unique request identifier that can be used to correlate requests with notifications from the SNS topic. Data sets will be published in comma-separated values (CSV) format with the file name {data_set_type}_YYYY-MM-DD'T'HH-mm-ss'Z'.csv. If a file with the same name already exists (e.g. if the same data set is requested twice), the original file will be overwritten by the new file. Requires a Role with an attached permissions policy providing Allow permissions for the following actions: s3:PutObject, s3:GetBucketLocation, sns:GetTopicAttributes, sns:Publish, iam:GetRolePolicy.
startSupportDataExport :: forall eff. StartSupportDataExportRequest -> Aff (exception :: EXCEPTION | eff) StartSupportDataExportResult
startSupportDataExport = Request.request serviceName "startSupportDataExport" 


newtype CustomerDefinedValues = CustomerDefinedValues (StrMap.StrMap OptionalValue)
derive instance newtypeCustomerDefinedValues :: Newtype CustomerDefinedValues _
derive instance repGenericCustomerDefinedValues :: Generic CustomerDefinedValues _
instance showCustomerDefinedValues :: Show CustomerDefinedValues where
  show = genericShow
instance decodeCustomerDefinedValues :: Decode CustomerDefinedValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomerDefinedValues :: Encode CustomerDefinedValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataSetPublicationDate = DataSetPublicationDate Number
derive instance newtypeDataSetPublicationDate :: Newtype DataSetPublicationDate _
derive instance repGenericDataSetPublicationDate :: Generic DataSetPublicationDate _
instance showDataSetPublicationDate :: Show DataSetPublicationDate where
  show = genericShow
instance decodeDataSetPublicationDate :: Decode DataSetPublicationDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataSetPublicationDate :: Encode DataSetPublicationDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataSetRequestId = DataSetRequestId String
derive instance newtypeDataSetRequestId :: Newtype DataSetRequestId _
derive instance repGenericDataSetRequestId :: Generic DataSetRequestId _
instance showDataSetRequestId :: Show DataSetRequestId where
  show = genericShow
instance decodeDataSetRequestId :: Decode DataSetRequestId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataSetRequestId :: Encode DataSetRequestId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataSetType = DataSetType String
derive instance newtypeDataSetType :: Newtype DataSetType _
derive instance repGenericDataSetType :: Generic DataSetType _
instance showDataSetType :: Show DataSetType where
  show = genericShow
instance decodeDataSetType :: Decode DataSetType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataSetType :: Encode DataSetType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DestinationS3BucketName = DestinationS3BucketName String
derive instance newtypeDestinationS3BucketName :: Newtype DestinationS3BucketName _
derive instance repGenericDestinationS3BucketName :: Generic DestinationS3BucketName _
instance showDestinationS3BucketName :: Show DestinationS3BucketName where
  show = genericShow
instance decodeDestinationS3BucketName :: Decode DestinationS3BucketName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDestinationS3BucketName :: Encode DestinationS3BucketName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DestinationS3Prefix = DestinationS3Prefix String
derive instance newtypeDestinationS3Prefix :: Newtype DestinationS3Prefix _
derive instance repGenericDestinationS3Prefix :: Generic DestinationS3Prefix _
instance showDestinationS3Prefix :: Show DestinationS3Prefix where
  show = genericShow
instance decodeDestinationS3Prefix :: Decode DestinationS3Prefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDestinationS3Prefix :: Encode DestinationS3Prefix where
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


newtype FromDate = FromDate Number
derive instance newtypeFromDate :: Newtype FromDate _
derive instance repGenericFromDate :: Generic FromDate _
instance showFromDate :: Show FromDate where
  show = genericShow
instance decodeFromDate :: Decode FromDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFromDate :: Encode FromDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for the parameters to the GenerateDataSet operation.
newtype GenerateDataSetRequest = GenerateDataSetRequest 
  { "DataSetType'" :: (DataSetType)
  , "DataSetPublicationDate'" :: (DataSetPublicationDate)
  , "RoleNameArn'" :: (RoleNameArn)
  , "DestinationS3BucketName'" :: (DestinationS3BucketName)
  , "DestinationS3Prefix'" :: NullOrUndefined.NullOrUndefined (DestinationS3Prefix)
  , "SnsTopicArn'" :: (SnsTopicArn)
  , "CustomerDefinedValues'" :: NullOrUndefined.NullOrUndefined (CustomerDefinedValues)
  }
derive instance newtypeGenerateDataSetRequest :: Newtype GenerateDataSetRequest _
derive instance repGenericGenerateDataSetRequest :: Generic GenerateDataSetRequest _
instance showGenerateDataSetRequest :: Show GenerateDataSetRequest where
  show = genericShow
instance decodeGenerateDataSetRequest :: Decode GenerateDataSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenerateDataSetRequest :: Encode GenerateDataSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for the result of the GenerateDataSet operation.
newtype GenerateDataSetResult = GenerateDataSetResult 
  { "DataSetRequestId'" :: NullOrUndefined.NullOrUndefined (DataSetRequestId)
  }
derive instance newtypeGenerateDataSetResult :: Newtype GenerateDataSetResult _
derive instance repGenericGenerateDataSetResult :: Generic GenerateDataSetResult _
instance showGenerateDataSetResult :: Show GenerateDataSetResult where
  show = genericShow
instance decodeGenerateDataSetResult :: Decode GenerateDataSetResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenerateDataSetResult :: Encode GenerateDataSetResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This exception is thrown when an internal service error occurs.
newtype MarketplaceCommerceAnalyticsException = MarketplaceCommerceAnalyticsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeMarketplaceCommerceAnalyticsException :: Newtype MarketplaceCommerceAnalyticsException _
derive instance repGenericMarketplaceCommerceAnalyticsException :: Generic MarketplaceCommerceAnalyticsException _
instance showMarketplaceCommerceAnalyticsException :: Show MarketplaceCommerceAnalyticsException where
  show = genericShow
instance decodeMarketplaceCommerceAnalyticsException :: Decode MarketplaceCommerceAnalyticsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarketplaceCommerceAnalyticsException :: Encode MarketplaceCommerceAnalyticsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionalKey = OptionalKey String
derive instance newtypeOptionalKey :: Newtype OptionalKey _
derive instance repGenericOptionalKey :: Generic OptionalKey _
instance showOptionalKey :: Show OptionalKey where
  show = genericShow
instance decodeOptionalKey :: Decode OptionalKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionalKey :: Encode OptionalKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionalValue = OptionalValue String
derive instance newtypeOptionalValue :: Newtype OptionalValue _
derive instance repGenericOptionalValue :: Generic OptionalValue _
instance showOptionalValue :: Show OptionalValue where
  show = genericShow
instance decodeOptionalValue :: Decode OptionalValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionalValue :: Encode OptionalValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleNameArn = RoleNameArn String
derive instance newtypeRoleNameArn :: Newtype RoleNameArn _
derive instance repGenericRoleNameArn :: Generic RoleNameArn _
instance showRoleNameArn :: Show RoleNameArn where
  show = genericShow
instance decodeRoleNameArn :: Decode RoleNameArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleNameArn :: Encode RoleNameArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnsTopicArn = SnsTopicArn String
derive instance newtypeSnsTopicArn :: Newtype SnsTopicArn _
derive instance repGenericSnsTopicArn :: Generic SnsTopicArn _
instance showSnsTopicArn :: Show SnsTopicArn where
  show = genericShow
instance decodeSnsTopicArn :: Decode SnsTopicArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnsTopicArn :: Encode SnsTopicArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for the parameters to the StartSupportDataExport operation.
newtype StartSupportDataExportRequest = StartSupportDataExportRequest 
  { "DataSetType'" :: (SupportDataSetType)
  , "FromDate'" :: (FromDate)
  , "RoleNameArn'" :: (RoleNameArn)
  , "DestinationS3BucketName'" :: (DestinationS3BucketName)
  , "DestinationS3Prefix'" :: NullOrUndefined.NullOrUndefined (DestinationS3Prefix)
  , "SnsTopicArn'" :: (SnsTopicArn)
  , "CustomerDefinedValues'" :: NullOrUndefined.NullOrUndefined (CustomerDefinedValues)
  }
derive instance newtypeStartSupportDataExportRequest :: Newtype StartSupportDataExportRequest _
derive instance repGenericStartSupportDataExportRequest :: Generic StartSupportDataExportRequest _
instance showStartSupportDataExportRequest :: Show StartSupportDataExportRequest where
  show = genericShow
instance decodeStartSupportDataExportRequest :: Decode StartSupportDataExportRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartSupportDataExportRequest :: Encode StartSupportDataExportRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for the result of the StartSupportDataExport operation.
newtype StartSupportDataExportResult = StartSupportDataExportResult 
  { "DataSetRequestId'" :: NullOrUndefined.NullOrUndefined (DataSetRequestId)
  }
derive instance newtypeStartSupportDataExportResult :: Newtype StartSupportDataExportResult _
derive instance repGenericStartSupportDataExportResult :: Generic StartSupportDataExportResult _
instance showStartSupportDataExportResult :: Show StartSupportDataExportResult where
  show = genericShow
instance decodeStartSupportDataExportResult :: Decode StartSupportDataExportResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartSupportDataExportResult :: Encode StartSupportDataExportResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SupportDataSetType = SupportDataSetType String
derive instance newtypeSupportDataSetType :: Newtype SupportDataSetType _
derive instance repGenericSupportDataSetType :: Generic SupportDataSetType _
instance showSupportDataSetType :: Show SupportDataSetType where
  show = genericShow
instance decodeSupportDataSetType :: Decode SupportDataSetType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSupportDataSetType :: Encode SupportDataSetType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
