

-- | All public APIs for AWS Cost and Usage Report service
module AWS.CUR where

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

serviceName = "CUR" :: String


-- | Delete a specified report definition
deleteReportDefinition :: forall eff. DeleteReportDefinitionRequest -> Aff (exception :: EXCEPTION | eff) DeleteReportDefinitionResponse
deleteReportDefinition = Request.request serviceName "deleteReportDefinition" 


-- | Describe a list of report definitions owned by the account
describeReportDefinitions :: forall eff. DescribeReportDefinitionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeReportDefinitionsResponse
describeReportDefinitions = Request.request serviceName "describeReportDefinitions" 


-- | Create a new report definition
putReportDefinition :: forall eff. PutReportDefinitionRequest -> Aff (exception :: EXCEPTION | eff) PutReportDefinitionResponse
putReportDefinition = Request.request serviceName "putReportDefinition" 


-- | Region of customer S3 bucket.
newtype AWSRegion = AWSRegion String
derive instance newtypeAWSRegion :: Newtype AWSRegion _
derive instance repGenericAWSRegion :: Generic AWSRegion _
instance showAWSRegion :: Show AWSRegion where
  show = genericShow
instance decodeAWSRegion :: Decode AWSRegion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAWSRegion :: Encode AWSRegion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enable support for Redshift and/or QuickSight.
newtype AdditionalArtifact = AdditionalArtifact String
derive instance newtypeAdditionalArtifact :: Newtype AdditionalArtifact _
derive instance repGenericAdditionalArtifact :: Generic AdditionalArtifact _
instance showAdditionalArtifact :: Show AdditionalArtifact where
  show = genericShow
instance decodeAdditionalArtifact :: Decode AdditionalArtifact where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdditionalArtifact :: Encode AdditionalArtifact where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of additional artifacts.
newtype AdditionalArtifactList = AdditionalArtifactList (Array AdditionalArtifact)
derive instance newtypeAdditionalArtifactList :: Newtype AdditionalArtifactList _
derive instance repGenericAdditionalArtifactList :: Generic AdditionalArtifactList _
instance showAdditionalArtifactList :: Show AdditionalArtifactList where
  show = genericShow
instance decodeAdditionalArtifactList :: Decode AdditionalArtifactList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdditionalArtifactList :: Encode AdditionalArtifactList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Preferred compression format for report.
newtype CompressionFormat = CompressionFormat String
derive instance newtypeCompressionFormat :: Newtype CompressionFormat _
derive instance repGenericCompressionFormat :: Generic CompressionFormat _
instance showCompressionFormat :: Show CompressionFormat where
  show = genericShow
instance decodeCompressionFormat :: Decode CompressionFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompressionFormat :: Encode CompressionFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Request of DeleteReportDefinition
newtype DeleteReportDefinitionRequest = DeleteReportDefinitionRequest 
  { "ReportName" :: NullOrUndefined.NullOrUndefined (ReportName)
  }
derive instance newtypeDeleteReportDefinitionRequest :: Newtype DeleteReportDefinitionRequest _
derive instance repGenericDeleteReportDefinitionRequest :: Generic DeleteReportDefinitionRequest _
instance showDeleteReportDefinitionRequest :: Show DeleteReportDefinitionRequest where
  show = genericShow
instance decodeDeleteReportDefinitionRequest :: Decode DeleteReportDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReportDefinitionRequest :: Encode DeleteReportDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Response of DeleteReportDefinition
newtype DeleteReportDefinitionResponse = DeleteReportDefinitionResponse 
  { "ResponseMessage" :: NullOrUndefined.NullOrUndefined (DeleteResponseMessage)
  }
derive instance newtypeDeleteReportDefinitionResponse :: Newtype DeleteReportDefinitionResponse _
derive instance repGenericDeleteReportDefinitionResponse :: Generic DeleteReportDefinitionResponse _
instance showDeleteReportDefinitionResponse :: Show DeleteReportDefinitionResponse where
  show = genericShow
instance decodeDeleteReportDefinitionResponse :: Decode DeleteReportDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReportDefinitionResponse :: Encode DeleteReportDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A message indicates if the deletion is successful.
newtype DeleteResponseMessage = DeleteResponseMessage String
derive instance newtypeDeleteResponseMessage :: Newtype DeleteResponseMessage _
derive instance repGenericDeleteResponseMessage :: Generic DeleteResponseMessage _
instance showDeleteResponseMessage :: Show DeleteResponseMessage where
  show = genericShow
instance decodeDeleteResponseMessage :: Decode DeleteResponseMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteResponseMessage :: Encode DeleteResponseMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Request of DescribeReportDefinitions
newtype DescribeReportDefinitionsRequest = DescribeReportDefinitionsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeDescribeReportDefinitionsRequest :: Newtype DescribeReportDefinitionsRequest _
derive instance repGenericDescribeReportDefinitionsRequest :: Generic DescribeReportDefinitionsRequest _
instance showDescribeReportDefinitionsRequest :: Show DescribeReportDefinitionsRequest where
  show = genericShow
instance decodeDescribeReportDefinitionsRequest :: Decode DescribeReportDefinitionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReportDefinitionsRequest :: Encode DescribeReportDefinitionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Response of DescribeReportDefinitions
newtype DescribeReportDefinitionsResponse = DescribeReportDefinitionsResponse 
  { "ReportDefinitions" :: NullOrUndefined.NullOrUndefined (ReportDefinitionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeDescribeReportDefinitionsResponse :: Newtype DescribeReportDefinitionsResponse _
derive instance repGenericDescribeReportDefinitionsResponse :: Generic DescribeReportDefinitionsResponse _
instance showDescribeReportDefinitionsResponse :: Show DescribeReportDefinitionsResponse where
  show = genericShow
instance decodeDescribeReportDefinitionsResponse :: Decode DescribeReportDefinitionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReportDefinitionsResponse :: Encode DescribeReportDefinitionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This exception is thrown when putting a report preference with a name that already exists.
newtype DuplicateReportNameException = DuplicateReportNameException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDuplicateReportNameException :: Newtype DuplicateReportNameException _
derive instance repGenericDuplicateReportNameException :: Generic DuplicateReportNameException _
instance showDuplicateReportNameException :: Show DuplicateReportNameException where
  show = genericShow
instance decodeDuplicateReportNameException :: Decode DuplicateReportNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateReportNameException :: Encode DuplicateReportNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A message to show the detail of the exception.
newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A generic string.
newtype GenericString = GenericString String
derive instance newtypeGenericString :: Newtype GenericString _
derive instance repGenericGenericString :: Generic GenericString _
instance showGenericString :: Show GenericString where
  show = genericShow
instance decodeGenericString :: Decode GenericString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenericString :: Encode GenericString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This exception is thrown on a known dependency failure.
newtype InternalErrorException = InternalErrorException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalErrorException :: Newtype InternalErrorException _
derive instance repGenericInternalErrorException :: Generic InternalErrorException _
instance showInternalErrorException :: Show InternalErrorException where
  show = genericShow
instance decodeInternalErrorException :: Decode InternalErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalErrorException :: Encode InternalErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The max number of results returned by the operation.
newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Request of PutReportDefinition
newtype PutReportDefinitionRequest = PutReportDefinitionRequest 
  { "ReportDefinition" :: (ReportDefinition)
  }
derive instance newtypePutReportDefinitionRequest :: Newtype PutReportDefinitionRequest _
derive instance repGenericPutReportDefinitionRequest :: Generic PutReportDefinitionRequest _
instance showPutReportDefinitionRequest :: Show PutReportDefinitionRequest where
  show = genericShow
instance decodePutReportDefinitionRequest :: Decode PutReportDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutReportDefinitionRequest :: Encode PutReportDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Response of PutReportDefinition
newtype PutReportDefinitionResponse = PutReportDefinitionResponse Types.NoArguments
derive instance newtypePutReportDefinitionResponse :: Newtype PutReportDefinitionResponse _
derive instance repGenericPutReportDefinitionResponse :: Generic PutReportDefinitionResponse _
instance showPutReportDefinitionResponse :: Show PutReportDefinitionResponse where
  show = genericShow
instance decodePutReportDefinitionResponse :: Decode PutReportDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutReportDefinitionResponse :: Encode PutReportDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


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
  , "AdditionalArtifacts" :: NullOrUndefined.NullOrUndefined (AdditionalArtifactList)
  }
derive instance newtypeReportDefinition :: Newtype ReportDefinition _
derive instance repGenericReportDefinition :: Generic ReportDefinition _
instance showReportDefinition :: Show ReportDefinition where
  show = genericShow
instance decodeReportDefinition :: Decode ReportDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReportDefinition :: Encode ReportDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of report definitions.
newtype ReportDefinitionList = ReportDefinitionList (Array ReportDefinition)
derive instance newtypeReportDefinitionList :: Newtype ReportDefinitionList _
derive instance repGenericReportDefinitionList :: Generic ReportDefinitionList _
instance showReportDefinitionList :: Show ReportDefinitionList where
  show = genericShow
instance decodeReportDefinitionList :: Decode ReportDefinitionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReportDefinitionList :: Encode ReportDefinitionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Preferred format for report.
newtype ReportFormat = ReportFormat String
derive instance newtypeReportFormat :: Newtype ReportFormat _
derive instance repGenericReportFormat :: Generic ReportFormat _
instance showReportFormat :: Show ReportFormat where
  show = genericShow
instance decodeReportFormat :: Decode ReportFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReportFormat :: Encode ReportFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This exception is thrown when the number of report preference reaches max limit. The max number is 5.
newtype ReportLimitReachedException = ReportLimitReachedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeReportLimitReachedException :: Newtype ReportLimitReachedException _
derive instance repGenericReportLimitReachedException :: Generic ReportLimitReachedException _
instance showReportLimitReachedException :: Show ReportLimitReachedException where
  show = genericShow
instance decodeReportLimitReachedException :: Decode ReportLimitReachedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReportLimitReachedException :: Encode ReportLimitReachedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Preferred name for a report, it has to be unique. Must starts with a number/letter, case sensitive. Limited to 256 characters.
newtype ReportName = ReportName String
derive instance newtypeReportName :: Newtype ReportName _
derive instance repGenericReportName :: Generic ReportName _
instance showReportName :: Show ReportName where
  show = genericShow
instance decodeReportName :: Decode ReportName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReportName :: Encode ReportName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Name of customer S3 bucket.
newtype S3Bucket = S3Bucket String
derive instance newtypeS3Bucket :: Newtype S3Bucket _
derive instance repGenericS3Bucket :: Generic S3Bucket _
instance showS3Bucket :: Show S3Bucket where
  show = genericShow
instance decodeS3Bucket :: Decode S3Bucket where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Bucket :: Encode S3Bucket where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Preferred report path prefix. Limited to 256 characters.
newtype S3Prefix = S3Prefix String
derive instance newtypeS3Prefix :: Newtype S3Prefix _
derive instance repGenericS3Prefix :: Generic S3Prefix _
instance showS3Prefix :: Show S3Prefix where
  show = genericShow
instance decodeS3Prefix :: Decode S3Prefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Prefix :: Encode S3Prefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Preference of including Resource IDs. You can include additional details about individual resource IDs in your report.
newtype SchemaElement = SchemaElement String
derive instance newtypeSchemaElement :: Newtype SchemaElement _
derive instance repGenericSchemaElement :: Generic SchemaElement _
instance showSchemaElement :: Show SchemaElement where
  show = genericShow
instance decodeSchemaElement :: Decode SchemaElement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaElement :: Encode SchemaElement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of schema elements.
newtype SchemaElementList = SchemaElementList (Array SchemaElement)
derive instance newtypeSchemaElementList :: Newtype SchemaElementList _
derive instance repGenericSchemaElementList :: Generic SchemaElementList _
instance showSchemaElementList :: Show SchemaElementList where
  show = genericShow
instance decodeSchemaElementList :: Decode SchemaElementList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaElementList :: Encode SchemaElementList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The frequency on which report data are measured and displayed.
newtype TimeUnit = TimeUnit String
derive instance newtypeTimeUnit :: Newtype TimeUnit _
derive instance repGenericTimeUnit :: Generic TimeUnit _
instance showTimeUnit :: Show TimeUnit where
  show = genericShow
instance decodeTimeUnit :: Decode TimeUnit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeUnit :: Encode TimeUnit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This exception is thrown when providing an invalid input. eg. Put a report preference with an invalid report name, or Delete a report preference with an empty report name.
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeValidationException :: Newtype ValidationException _
derive instance repGenericValidationException :: Generic ValidationException _
instance showValidationException :: Show ValidationException where
  show = genericShow
instance decodeValidationException :: Decode ValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationException :: Encode ValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
