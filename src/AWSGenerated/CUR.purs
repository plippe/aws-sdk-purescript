

-- | All public APIs for AWS Cost and Usage Report service
module AWS.CUR where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CUR" :: String


-- | Delete a specified report definition
deleteReportDefinition :: forall eff. DeleteReportDefinitionRequest -> Aff (err :: AWS.RequestError | eff) DeleteReportDefinitionResponse
deleteReportDefinition = AWS.request serviceName "DeleteReportDefinition" 


-- | Describe a list of report definitions owned by the account
describeReportDefinitions :: forall eff. DescribeReportDefinitionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeReportDefinitionsResponse
describeReportDefinitions = AWS.request serviceName "DescribeReportDefinitions" 


-- | Create a new report definition
putReportDefinition :: forall eff. PutReportDefinitionRequest -> Aff (err :: AWS.RequestError | eff) PutReportDefinitionResponse
putReportDefinition = AWS.request serviceName "PutReportDefinition" 


-- | Region of customer S3 bucket.
newtype AWSRegion = AWSRegion String


-- | Enable support for Redshift and/or QuickSight.
newtype AdditionalArtifact = AdditionalArtifact String


-- | A list of additional artifacts.
newtype AdditionalArtifactList = AdditionalArtifactList (Array AdditionalArtifact)


-- | Preferred compression format for report.
newtype CompressionFormat = CompressionFormat String


-- | Request of DeleteReportDefinition
newtype DeleteReportDefinitionRequest = DeleteReportDefinitionRequest 
  { "ReportName" :: NullOrUndefined (ReportName)
  }


-- | Response of DeleteReportDefinition
newtype DeleteReportDefinitionResponse = DeleteReportDefinitionResponse 
  { "ResponseMessage" :: NullOrUndefined (DeleteResponseMessage)
  }


-- | A message indicates if the deletion is successful.
newtype DeleteResponseMessage = DeleteResponseMessage String


-- | Request of DescribeReportDefinitions
newtype DescribeReportDefinitionsRequest = DescribeReportDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


-- | Response of DescribeReportDefinitions
newtype DescribeReportDefinitionsResponse = DescribeReportDefinitionsResponse 
  { "ReportDefinitions" :: NullOrUndefined (ReportDefinitionList)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


-- | This exception is thrown when putting a report preference with a name that already exists.
newtype DuplicateReportNameException = DuplicateReportNameException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | A message to show the detail of the exception.
newtype ErrorMessage = ErrorMessage String


-- | A generic string.
newtype GenericString = GenericString String


-- | This exception is thrown on a known dependency failure.
newtype InternalErrorException = InternalErrorException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | The max number of results returned by the operation.
newtype MaxResults = MaxResults Int


-- | Request of PutReportDefinition
newtype PutReportDefinitionRequest = PutReportDefinitionRequest 
  { "ReportDefinition" :: (ReportDefinition)
  }


-- | Response of PutReportDefinition
newtype PutReportDefinitionResponse = PutReportDefinitionResponse 
  { 
  }


-- | The definition of AWS Cost and Usage Report. Customer can specify the report name, time unit, report format, compression format, S3 bucket and additional artifacts and schema elements in the definition.
newtype ReportDefinition = ReportDefinition 
  { "ReportName" :: (ReportName)
  , "TimeUnit" :: (TimeUnit)
  , "Format" :: (ReportFormat)
  , "Compression" :: (CompressionFormat)
  , "AdditionalSchemaElements" :: (SchemaElementList)
  , "S3Bucket" :: (S3Bucket)
  , "S3Prefix" :: (S3Prefix)
  , "S3Region" :: (AWSRegion)
  , "AdditionalArtifacts" :: NullOrUndefined (AdditionalArtifactList)
  }


-- | A list of report definitions.
newtype ReportDefinitionList = ReportDefinitionList (Array ReportDefinition)


-- | Preferred format for report.
newtype ReportFormat = ReportFormat String


-- | This exception is thrown when the number of report preference reaches max limit. The max number is 5.
newtype ReportLimitReachedException = ReportLimitReachedException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | Preferred name for a report, it has to be unique. Must starts with a number/letter, case sensitive. Limited to 256 characters.
newtype ReportName = ReportName String


-- | Name of customer S3 bucket.
newtype S3Bucket = S3Bucket String


-- | Preferred report path prefix. Limited to 256 characters.
newtype S3Prefix = S3Prefix String


-- | Preference of including Resource IDs. You can include additional details about individual resource IDs in your report.
newtype SchemaElement = SchemaElement String


-- | A list of schema elements.
newtype SchemaElementList = SchemaElementList (Array SchemaElement)


-- | The frequency on which report data are measured and displayed.
newtype TimeUnit = TimeUnit String


-- | This exception is thrown when providing an invalid input. eg. Put a report preference with an invalid report name, or Delete a report preference with an empty report name.
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
