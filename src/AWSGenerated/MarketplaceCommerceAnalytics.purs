

-- | Provides AWS Marketplace business intelligence data on-demand.
module AWS.MarketplaceCommerceAnalytics where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MarketplaceCommerceAnalytics" :: String


-- | Given a data set type and data set publication date, asynchronously publishes the requested data set to the specified S3 bucket and notifies the specified SNS topic once the data is available. Returns a unique request identifier that can be used to correlate requests with notifications from the SNS topic. Data sets will be published in comma-separated values (CSV) format with the file name {data_set_type}_YYYY-MM-DD.csv. If a file with the same name already exists (e.g. if the same data set is requested twice), the original file will be overwritten by the new file. Requires a Role with an attached permissions policy providing Allow permissions for the following actions: s3:PutObject, s3:GetBucketLocation, sns:GetTopicAttributes, sns:Publish, iam:GetRolePolicy.
generateDataSet :: forall eff. GenerateDataSetRequest -> Aff (err :: AWS.RequestError | eff) GenerateDataSetResult
generateDataSet = AWS.request serviceName "GenerateDataSet" 


-- | Given a data set type and a from date, asynchronously publishes the requested customer support data to the specified S3 bucket and notifies the specified SNS topic once the data is available. Returns a unique request identifier that can be used to correlate requests with notifications from the SNS topic. Data sets will be published in comma-separated values (CSV) format with the file name {data_set_type}_YYYY-MM-DD'T'HH-mm-ss'Z'.csv. If a file with the same name already exists (e.g. if the same data set is requested twice), the original file will be overwritten by the new file. Requires a Role with an attached permissions policy providing Allow permissions for the following actions: s3:PutObject, s3:GetBucketLocation, sns:GetTopicAttributes, sns:Publish, iam:GetRolePolicy.
startSupportDataExport :: forall eff. StartSupportDataExportRequest -> Aff (err :: AWS.RequestError | eff) StartSupportDataExportResult
startSupportDataExport = AWS.request serviceName "StartSupportDataExport" 


newtype CustomerDefinedValues = CustomerDefinedValues (Map OptionalKey OptionalValue)


newtype DataSetPublicationDate = DataSetPublicationDate Number


newtype DataSetRequestId = DataSetRequestId String


newtype DataSetType = DataSetType String


newtype DestinationS3BucketName = DestinationS3BucketName String


newtype DestinationS3Prefix = DestinationS3Prefix String


newtype ExceptionMessage = ExceptionMessage String


newtype FromDate = FromDate Number


-- | Container for the parameters to the GenerateDataSet operation.
newtype GenerateDataSetRequest = GenerateDataSetRequest 
  { "DataSetType'" :: (DataSetType)
  , "DataSetPublicationDate'" :: (DataSetPublicationDate)
  , "RoleNameArn'" :: (RoleNameArn)
  , "DestinationS3BucketName'" :: (DestinationS3BucketName)
  , "DestinationS3Prefix'" :: NullOrUndefined (DestinationS3Prefix)
  , "SnsTopicArn'" :: (SnsTopicArn)
  , "CustomerDefinedValues'" :: NullOrUndefined (CustomerDefinedValues)
  }


-- | Container for the result of the GenerateDataSet operation.
newtype GenerateDataSetResult = GenerateDataSetResult 
  { "DataSetRequestId'" :: NullOrUndefined (DataSetRequestId)
  }


-- | This exception is thrown when an internal service error occurs.
newtype MarketplaceCommerceAnalyticsException = MarketplaceCommerceAnalyticsException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }


newtype OptionalKey = OptionalKey String


newtype OptionalValue = OptionalValue String


newtype RoleNameArn = RoleNameArn String


newtype SnsTopicArn = SnsTopicArn String


-- | Container for the parameters to the StartSupportDataExport operation.
newtype StartSupportDataExportRequest = StartSupportDataExportRequest 
  { "DataSetType'" :: (SupportDataSetType)
  , "FromDate'" :: (FromDate)
  , "RoleNameArn'" :: (RoleNameArn)
  , "DestinationS3BucketName'" :: (DestinationS3BucketName)
  , "DestinationS3Prefix'" :: NullOrUndefined (DestinationS3Prefix)
  , "SnsTopicArn'" :: (SnsTopicArn)
  , "CustomerDefinedValues'" :: NullOrUndefined (CustomerDefinedValues)
  }


-- | Container for the result of the StartSupportDataExport operation.
newtype StartSupportDataExportResult = StartSupportDataExportResult 
  { "DataSetRequestId'" :: NullOrUndefined (DataSetRequestId)
  }


newtype SupportDataSetType = SupportDataSetType String
