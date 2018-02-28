

-- | <fullname>AWS Import/Export Service</fullname> AWS Import/Export accelerates transferring large amounts of data between the AWS cloud and portable storage devices that you mail to us. AWS Import/Export transfers data directly onto and off of your storage devices using Amazon's high-speed internal network and bypassing the Internet. For large data sets, AWS Import/Export is often faster than Internet transfer and more cost effective than upgrading your connectivity.
module AWS.ImportExport where

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

serviceName = "ImportExport" :: String


-- | This operation cancels a specified job. Only the job owner can cancel it. The operation fails if the job has already started or is complete.
cancelJob :: forall eff. CancelJobInput -> Aff (exception :: EXCEPTION | eff) CancelJobOutput
cancelJob = Request.request serviceName "cancelJob" 


-- | This operation initiates the process of scheduling an upload or download of your data. You include in the request a manifest that describes the data transfer specifics. The response to the request includes a job ID, which you can use in other operations, a signature that you use to identify your storage device, and the address where you should ship your storage device.
createJob :: forall eff. CreateJobInput -> Aff (exception :: EXCEPTION | eff) CreateJobOutput
createJob = Request.request serviceName "createJob" 


-- | This operation generates a pre-paid UPS shipping label that you will use to ship your device to AWS for processing.
getShippingLabel :: forall eff. GetShippingLabelInput -> Aff (exception :: EXCEPTION | eff) GetShippingLabelOutput
getShippingLabel = Request.request serviceName "getShippingLabel" 


-- | This operation returns information about a job, including where the job is in the processing pipeline, the status of the results, and the signature value associated with the job. You can only return information about jobs you own.
getStatus :: forall eff. GetStatusInput -> Aff (exception :: EXCEPTION | eff) GetStatusOutput
getStatus = Request.request serviceName "getStatus" 


-- | This operation returns the jobs associated with the requester. AWS Import/Export lists the jobs in reverse chronological order based on the date of creation. For example if Job Test1 was created 2009Dec30 and Test2 was created 2010Feb05, the ListJobs operation would return Test2 followed by Test1.
listJobs :: forall eff. ListJobsInput -> Aff (exception :: EXCEPTION | eff) ListJobsOutput
listJobs = Request.request serviceName "listJobs" 


-- | You use this operation to change the parameters specified in the original manifest file by supplying a new manifest file. The manifest file attached to this request replaces the original manifest file. You can only use the operation after a CreateJob request but before the data transfer starts and you can only use it on jobs you own.
updateJob :: forall eff. UpdateJobInput -> Aff (exception :: EXCEPTION | eff) UpdateJobOutput
updateJob = Request.request serviceName "updateJob" 


-- | Specifies the version of the client tool.
newtype APIVersion = APIVersion String
derive instance newtypeAPIVersion :: Newtype APIVersion _
derive instance repGenericAPIVersion :: Generic APIVersion _
instance showAPIVersion :: Show APIVersion where
  show = genericShow
instance decodeAPIVersion :: Decode APIVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAPIVersion :: Encode APIVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A discrete item that contains the description and URL of an artifact (such as a PDF).
newtype Artifact = Artifact 
  { "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "URL" :: NullOrUndefined.NullOrUndefined (URL)
  }
derive instance newtypeArtifact :: Newtype Artifact _
derive instance repGenericArtifact :: Generic Artifact _
instance showArtifact :: Show Artifact where
  show = genericShow
instance decodeArtifact :: Decode Artifact where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifact :: Encode Artifact where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A collection of artifacts.
newtype ArtifactList = ArtifactList (Array Artifact)
derive instance newtypeArtifactList :: Newtype ArtifactList _
derive instance repGenericArtifactList :: Generic ArtifactList _
instance showArtifactList :: Show ArtifactList where
  show = genericShow
instance decodeArtifactList :: Decode ArtifactList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactList :: Encode ArtifactList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The account specified does not have the appropriate bucket permissions.
newtype BucketPermissionException = BucketPermissionException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBucketPermissionException :: Newtype BucketPermissionException _
derive instance repGenericBucketPermissionException :: Generic BucketPermissionException _
instance showBucketPermissionException :: Show BucketPermissionException where
  show = genericShow
instance decodeBucketPermissionException :: Decode BucketPermissionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketPermissionException :: Encode BucketPermissionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Input structure for the CancelJob operation.
newtype CancelJobInput = CancelJobInput 
  { "JobId" :: (JobId)
  , "APIVersion" :: NullOrUndefined.NullOrUndefined (APIVersion)
  }
derive instance newtypeCancelJobInput :: Newtype CancelJobInput _
derive instance repGenericCancelJobInput :: Generic CancelJobInput _
instance showCancelJobInput :: Show CancelJobInput where
  show = genericShow
instance decodeCancelJobInput :: Decode CancelJobInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelJobInput :: Encode CancelJobInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Output structure for the CancelJob operation.
newtype CancelJobOutput = CancelJobOutput 
  { "Success" :: NullOrUndefined.NullOrUndefined (Success)
  }
derive instance newtypeCancelJobOutput :: Newtype CancelJobOutput _
derive instance repGenericCancelJobOutput :: Generic CancelJobOutput _
instance showCancelJobOutput :: Show CancelJobOutput where
  show = genericShow
instance decodeCancelJobOutput :: Decode CancelJobOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelJobOutput :: Encode CancelJobOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The specified job ID has been canceled and is no longer valid.
newtype CanceledJobIdException = CanceledJobIdException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeCanceledJobIdException :: Newtype CanceledJobIdException _
derive instance repGenericCanceledJobIdException :: Generic CanceledJobIdException _
instance showCanceledJobIdException :: Show CanceledJobIdException where
  show = genericShow
instance decodeCanceledJobIdException :: Decode CanceledJobIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCanceledJobIdException :: Encode CanceledJobIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Name of the shipping company. This value is included when the LocationCode is "Returned".
newtype Carrier = Carrier String
derive instance newtypeCarrier :: Newtype Carrier _
derive instance repGenericCarrier :: Generic Carrier _
instance showCarrier :: Show Carrier where
  show = genericShow
instance decodeCarrier :: Decode Carrier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCarrier :: Encode Carrier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Input structure for the CreateJob operation.
newtype CreateJobInput = CreateJobInput 
  { "JobType" :: (JobType)
  , "Manifest" :: (Manifest)
  , "ManifestAddendum" :: NullOrUndefined.NullOrUndefined (ManifestAddendum)
  , "ValidateOnly" :: (ValidateOnly)
  , "APIVersion" :: NullOrUndefined.NullOrUndefined (APIVersion)
  }
derive instance newtypeCreateJobInput :: Newtype CreateJobInput _
derive instance repGenericCreateJobInput :: Generic CreateJobInput _
instance showCreateJobInput :: Show CreateJobInput where
  show = genericShow
instance decodeCreateJobInput :: Decode CreateJobInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobInput :: Encode CreateJobInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Output structure for the CreateJob operation.
newtype CreateJobOutput = CreateJobOutput 
  { "JobId" :: NullOrUndefined.NullOrUndefined (JobId)
  , "JobType" :: NullOrUndefined.NullOrUndefined (JobType)
  , "Signature" :: NullOrUndefined.NullOrUndefined (Signature)
  , "SignatureFileContents" :: NullOrUndefined.NullOrUndefined (SignatureFileContents)
  , "WarningMessage" :: NullOrUndefined.NullOrUndefined (WarningMessage)
  , "ArtifactList" :: NullOrUndefined.NullOrUndefined (ArtifactList)
  }
derive instance newtypeCreateJobOutput :: Newtype CreateJobOutput _
derive instance repGenericCreateJobOutput :: Generic CreateJobOutput _
instance showCreateJobOutput :: Show CreateJobOutput where
  show = genericShow
instance decodeCreateJobOutput :: Decode CreateJobOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobOutput :: Encode CreateJobOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Each account can create only a certain number of jobs per day. If you need to create more than this, please contact awsimportexport@amazon.com to explain your particular use case.
newtype CreateJobQuotaExceededException = CreateJobQuotaExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeCreateJobQuotaExceededException :: Newtype CreateJobQuotaExceededException _
derive instance repGenericCreateJobQuotaExceededException :: Generic CreateJobQuotaExceededException _
instance showCreateJobQuotaExceededException :: Show CreateJobQuotaExceededException where
  show = genericShow
instance decodeCreateJobQuotaExceededException :: Decode CreateJobQuotaExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobQuotaExceededException :: Encode CreateJobQuotaExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Timestamp of the CreateJob request in ISO8601 date format. For example "2010-03-28T20:27:35Z".
newtype CreationDate = CreationDate Number
derive instance newtypeCreationDate :: Newtype CreationDate _
derive instance repGenericCreationDate :: Generic CreationDate _
instance showCreationDate :: Show CreationDate where
  show = genericShow
instance decodeCreationDate :: Decode CreationDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreationDate :: Encode CreationDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The last manifest submitted, which will be used to process the job.
newtype CurrentManifest = CurrentManifest String
derive instance newtypeCurrentManifest :: Newtype CurrentManifest _
derive instance repGenericCurrentManifest :: Generic CurrentManifest _
instance showCurrentManifest :: Show CurrentManifest where
  show = genericShow
instance decodeCurrentManifest :: Decode CurrentManifest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCurrentManifest :: Encode CurrentManifest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The associated description for this object.
newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _
derive instance repGenericDescription :: Generic Description _
instance showDescription :: Show Description where
  show = genericShow
instance decodeDescription :: Decode Description where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescription :: Encode Description where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Number of errors. We return this value when the ProgressCode is Success or SuccessWithErrors.
newtype ErrorCount = ErrorCount Int
derive instance newtypeErrorCount :: Newtype ErrorCount _
derive instance repGenericErrorCount :: Generic ErrorCount _
instance showErrorCount :: Show ErrorCount where
  show = genericShow
instance decodeErrorCount :: Decode ErrorCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorCount :: Encode ErrorCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The human-readable description of a particular error.
newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Indicates that the specified job has expired out of the system.
newtype ExpiredJobIdException = ExpiredJobIdException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeExpiredJobIdException :: Newtype ExpiredJobIdException _
derive instance repGenericExpiredJobIdException :: Generic ExpiredJobIdException _
instance showExpiredJobIdException :: Show ExpiredJobIdException where
  show = genericShow
instance decodeExpiredJobIdException :: Decode ExpiredJobIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpiredJobIdException :: Encode ExpiredJobIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GenericString = GenericString String
derive instance newtypeGenericString :: Newtype GenericString _
derive instance repGenericGenericString :: Generic GenericString _
instance showGenericString :: Show GenericString where
  show = genericShow
instance decodeGenericString :: Decode GenericString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenericString :: Encode GenericString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetShippingLabelInput = GetShippingLabelInput 
  { "JobIds'" :: (JobIdList)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name')
  , "Company'" :: NullOrUndefined.NullOrUndefined (Company')
  , "PhoneNumber'" :: NullOrUndefined.NullOrUndefined (PhoneNumber')
  , "Country'" :: NullOrUndefined.NullOrUndefined (Country')
  , "StateOrProvince'" :: NullOrUndefined.NullOrUndefined (StateOrProvince')
  , "City'" :: NullOrUndefined.NullOrUndefined (City')
  , "PostalCode'" :: NullOrUndefined.NullOrUndefined (PostalCode')
  , "Street1'" :: NullOrUndefined.NullOrUndefined (Street1')
  , "Street2'" :: NullOrUndefined.NullOrUndefined (Street2')
  , "Street3'" :: NullOrUndefined.NullOrUndefined (Street3')
  , "APIVersion" :: NullOrUndefined.NullOrUndefined (APIVersion)
  }
derive instance newtypeGetShippingLabelInput :: Newtype GetShippingLabelInput _
derive instance repGenericGetShippingLabelInput :: Generic GetShippingLabelInput _
instance showGetShippingLabelInput :: Show GetShippingLabelInput where
  show = genericShow
instance decodeGetShippingLabelInput :: Decode GetShippingLabelInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetShippingLabelInput :: Encode GetShippingLabelInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetShippingLabelOutput = GetShippingLabelOutput 
  { "ShippingLabelURL" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "Warning" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeGetShippingLabelOutput :: Newtype GetShippingLabelOutput _
derive instance repGenericGetShippingLabelOutput :: Generic GetShippingLabelOutput _
instance showGetShippingLabelOutput :: Show GetShippingLabelOutput where
  show = genericShow
instance decodeGetShippingLabelOutput :: Decode GetShippingLabelOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetShippingLabelOutput :: Encode GetShippingLabelOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Input structure for the GetStatus operation.
newtype GetStatusInput = GetStatusInput 
  { "JobId" :: (JobId)
  , "APIVersion" :: NullOrUndefined.NullOrUndefined (APIVersion)
  }
derive instance newtypeGetStatusInput :: Newtype GetStatusInput _
derive instance repGenericGetStatusInput :: Generic GetStatusInput _
instance showGetStatusInput :: Show GetStatusInput where
  show = genericShow
instance decodeGetStatusInput :: Decode GetStatusInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStatusInput :: Encode GetStatusInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Output structure for the GetStatus operation.
newtype GetStatusOutput = GetStatusOutput 
  { "JobId" :: NullOrUndefined.NullOrUndefined (JobId)
  , "JobType" :: NullOrUndefined.NullOrUndefined (JobType)
  , "LocationCode" :: NullOrUndefined.NullOrUndefined (LocationCode)
  , "LocationMessage" :: NullOrUndefined.NullOrUndefined (LocationMessage)
  , "ProgressCode" :: NullOrUndefined.NullOrUndefined (ProgressCode)
  , "ProgressMessage" :: NullOrUndefined.NullOrUndefined (ProgressMessage)
  , "Carrier" :: NullOrUndefined.NullOrUndefined (Carrier)
  , "TrackingNumber" :: NullOrUndefined.NullOrUndefined (TrackingNumber)
  , "LogBucket" :: NullOrUndefined.NullOrUndefined (LogBucket)
  , "LogKey" :: NullOrUndefined.NullOrUndefined (LogKey)
  , "ErrorCount" :: NullOrUndefined.NullOrUndefined (ErrorCount)
  , "Signature" :: NullOrUndefined.NullOrUndefined (Signature)
  , "SignatureFileContents" :: NullOrUndefined.NullOrUndefined (Signature)
  , "CurrentManifest" :: NullOrUndefined.NullOrUndefined (CurrentManifest)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (CreationDate)
  , "ArtifactList" :: NullOrUndefined.NullOrUndefined (ArtifactList)
  }
derive instance newtypeGetStatusOutput :: Newtype GetStatusOutput _
derive instance repGenericGetStatusOutput :: Generic GetStatusOutput _
instance showGetStatusOutput :: Show GetStatusOutput where
  show = genericShow
instance decodeGetStatusOutput :: Decode GetStatusOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStatusOutput :: Encode GetStatusOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The AWS Access Key ID specified in the request did not match the manifest's accessKeyId value. The manifest and the request authentication must use the same AWS Access Key ID.
newtype InvalidAccessKeyIdException = InvalidAccessKeyIdException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidAccessKeyIdException :: Newtype InvalidAccessKeyIdException _
derive instance repGenericInvalidAccessKeyIdException :: Generic InvalidAccessKeyIdException _
instance showInvalidAccessKeyIdException :: Show InvalidAccessKeyIdException where
  show = genericShow
instance decodeInvalidAccessKeyIdException :: Decode InvalidAccessKeyIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidAccessKeyIdException :: Encode InvalidAccessKeyIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The address specified in the manifest is invalid.
newtype InvalidAddressException = InvalidAddressException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidAddressException :: Newtype InvalidAddressException _
derive instance repGenericInvalidAddressException :: Generic InvalidAddressException _
instance showInvalidAddressException :: Show InvalidAddressException where
  show = genericShow
instance decodeInvalidAddressException :: Decode InvalidAddressException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidAddressException :: Encode InvalidAddressException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | One or more customs parameters was invalid. Please correct and resubmit.
newtype InvalidCustomsException = InvalidCustomsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidCustomsException :: Newtype InvalidCustomsException _
derive instance repGenericInvalidCustomsException :: Generic InvalidCustomsException _
instance showInvalidCustomsException :: Show InvalidCustomsException where
  show = genericShow
instance decodeInvalidCustomsException :: Decode InvalidCustomsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCustomsException :: Encode InvalidCustomsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | File system specified in export manifest is invalid.
newtype InvalidFileSystemException = InvalidFileSystemException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidFileSystemException :: Newtype InvalidFileSystemException _
derive instance repGenericInvalidFileSystemException :: Generic InvalidFileSystemException _
instance showInvalidFileSystemException :: Show InvalidFileSystemException where
  show = genericShow
instance decodeInvalidFileSystemException :: Decode InvalidFileSystemException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidFileSystemException :: Encode InvalidFileSystemException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The JOBID was missing, not found, or not associated with the AWS account.
newtype InvalidJobIdException = InvalidJobIdException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidJobIdException :: Newtype InvalidJobIdException _
derive instance repGenericInvalidJobIdException :: Generic InvalidJobIdException _
instance showInvalidJobIdException :: Show InvalidJobIdException where
  show = genericShow
instance decodeInvalidJobIdException :: Decode InvalidJobIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidJobIdException :: Encode InvalidJobIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | One or more manifest fields was invalid. Please correct and resubmit.
newtype InvalidManifestFieldException = InvalidManifestFieldException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidManifestFieldException :: Newtype InvalidManifestFieldException _
derive instance repGenericInvalidManifestFieldException :: Generic InvalidManifestFieldException _
instance showInvalidManifestFieldException :: Show InvalidManifestFieldException where
  show = genericShow
instance decodeInvalidManifestFieldException :: Decode InvalidManifestFieldException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidManifestFieldException :: Encode InvalidManifestFieldException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | One or more parameters had an invalid value.
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The client tool version is invalid.
newtype InvalidVersionException = InvalidVersionException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidVersionException :: Newtype InvalidVersionException _
derive instance repGenericInvalidVersionException :: Generic InvalidVersionException _
instance showInvalidVersionException :: Show InvalidVersionException where
  show = genericShow
instance decodeInvalidVersionException :: Decode InvalidVersionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidVersionException :: Encode InvalidVersionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Indicates whether the job was canceled.
newtype IsCanceled = IsCanceled Boolean
derive instance newtypeIsCanceled :: Newtype IsCanceled _
derive instance repGenericIsCanceled :: Generic IsCanceled _
instance showIsCanceled :: Show IsCanceled where
  show = genericShow
instance decodeIsCanceled :: Decode IsCanceled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsCanceled :: Encode IsCanceled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Indicates whether the list of jobs was truncated. If true, then call ListJobs again using the last JobId element as the marker.
newtype IsTruncated = IsTruncated Boolean
derive instance newtypeIsTruncated :: Newtype IsTruncated _
derive instance repGenericIsTruncated :: Generic IsTruncated _
instance showIsTruncated :: Show IsTruncated where
  show = genericShow
instance decodeIsTruncated :: Decode IsTruncated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsTruncated :: Encode IsTruncated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Representation of a job returned by the ListJobs operation.
newtype Job = Job 
  { "JobId" :: NullOrUndefined.NullOrUndefined (JobId)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (CreationDate)
  , "IsCanceled" :: NullOrUndefined.NullOrUndefined (IsCanceled)
  , "JobType" :: NullOrUndefined.NullOrUndefined (JobType)
  }
derive instance newtypeJob :: Newtype Job _
derive instance repGenericJob :: Generic Job _
instance showJob :: Show Job where
  show = genericShow
instance decodeJob :: Decode Job where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJob :: Encode Job where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A unique identifier which refers to a particular job.
newtype JobId = JobId String
derive instance newtypeJobId :: Newtype JobId _
derive instance repGenericJobId :: Generic JobId _
instance showJobId :: Show JobId where
  show = genericShow
instance decodeJobId :: Decode JobId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobId :: Encode JobId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobIdList = JobIdList (Array GenericString)
derive instance newtypeJobIdList :: Newtype JobIdList _
derive instance repGenericJobIdList :: Generic JobIdList _
instance showJobIdList :: Show JobIdList where
  show = genericShow
instance decodeJobIdList :: Decode JobIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobIdList :: Encode JobIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies whether the job to initiate is an import or export job.
newtype JobType = JobType String
derive instance newtypeJobType :: Newtype JobType _
derive instance repGenericJobType :: Generic JobType _
instance showJobType :: Show JobType where
  show = genericShow
instance decodeJobType :: Decode JobType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobType :: Encode JobType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list container for Jobs returned by the ListJobs operation.
newtype JobsList = JobsList (Array Job)
derive instance newtypeJobsList :: Newtype JobsList _
derive instance repGenericJobsList :: Generic JobsList _
instance showJobsList :: Show JobsList where
  show = genericShow
instance decodeJobsList :: Decode JobsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobsList :: Encode JobsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Input structure for the ListJobs operation.
newtype ListJobsInput = ListJobsInput 
  { "MaxJobs" :: NullOrUndefined.NullOrUndefined (MaxJobs)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "APIVersion" :: NullOrUndefined.NullOrUndefined (APIVersion)
  }
derive instance newtypeListJobsInput :: Newtype ListJobsInput _
derive instance repGenericListJobsInput :: Generic ListJobsInput _
instance showListJobsInput :: Show ListJobsInput where
  show = genericShow
instance decodeListJobsInput :: Decode ListJobsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsInput :: Encode ListJobsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Output structure for the ListJobs operation.
newtype ListJobsOutput = ListJobsOutput 
  { "Jobs" :: NullOrUndefined.NullOrUndefined (JobsList)
  , "IsTruncated" :: NullOrUndefined.NullOrUndefined (IsTruncated)
  }
derive instance newtypeListJobsOutput :: Newtype ListJobsOutput _
derive instance repGenericListJobsOutput :: Generic ListJobsOutput _
instance showListJobsOutput :: Show ListJobsOutput where
  show = genericShow
instance decodeListJobsOutput :: Decode ListJobsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsOutput :: Encode ListJobsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A token representing the location of the storage device, such as "AtAWS".
newtype LocationCode = LocationCode String
derive instance newtypeLocationCode :: Newtype LocationCode _
derive instance repGenericLocationCode :: Generic LocationCode _
instance showLocationCode :: Show LocationCode where
  show = genericShow
instance decodeLocationCode :: Decode LocationCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocationCode :: Encode LocationCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A more human readable form of the physical location of the storage device.
newtype LocationMessage = LocationMessage String
derive instance newtypeLocationMessage :: Newtype LocationMessage _
derive instance repGenericLocationMessage :: Generic LocationMessage _
instance showLocationMessage :: Show LocationMessage where
  show = genericShow
instance decodeLocationMessage :: Decode LocationMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocationMessage :: Encode LocationMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Amazon S3 bucket for user logs.
newtype LogBucket = LogBucket String
derive instance newtypeLogBucket :: Newtype LogBucket _
derive instance repGenericLogBucket :: Generic LogBucket _
instance showLogBucket :: Show LogBucket where
  show = genericShow
instance decodeLogBucket :: Decode LogBucket where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogBucket :: Encode LogBucket where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The key where the user logs were stored.
newtype LogKey = LogKey String
derive instance newtypeLogKey :: Newtype LogKey _
derive instance repGenericLogKey :: Generic LogKey _
instance showLogKey :: Show LogKey where
  show = genericShow
instance decodeLogKey :: Decode LogKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogKey :: Encode LogKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Your manifest is not well-formed.
newtype MalformedManifestException = MalformedManifestException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMalformedManifestException :: Newtype MalformedManifestException _
derive instance repGenericMalformedManifestException :: Generic MalformedManifestException _
instance showMalformedManifestException :: Show MalformedManifestException where
  show = genericShow
instance decodeMalformedManifestException :: Decode MalformedManifestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMalformedManifestException :: Encode MalformedManifestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The UTF-8 encoded text of the manifest file.
newtype Manifest = Manifest String
derive instance newtypeManifest :: Newtype Manifest _
derive instance repGenericManifest :: Generic Manifest _
instance showManifest :: Show Manifest where
  show = genericShow
instance decodeManifest :: Decode Manifest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeManifest :: Encode Manifest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | For internal use only.
newtype ManifestAddendum = ManifestAddendum String
derive instance newtypeManifestAddendum :: Newtype ManifestAddendum _
derive instance repGenericManifestAddendum :: Generic ManifestAddendum _
instance showManifestAddendum :: Show ManifestAddendum where
  show = genericShow
instance decodeManifestAddendum :: Decode ManifestAddendum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeManifestAddendum :: Encode ManifestAddendum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the JOBID to start after when listing the jobs created with your account. AWS Import/Export lists your jobs in reverse chronological order. See MaxJobs.
newtype Marker = Marker String
derive instance newtypeMarker :: Newtype Marker _
derive instance repGenericMarker :: Generic Marker _
instance showMarker :: Show Marker where
  show = genericShow
instance decodeMarker :: Decode Marker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarker :: Encode Marker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Sets the maximum number of jobs returned in the response. If there are additional jobs that were not returned because MaxJobs was exceeded, the response contains &lt;IsTruncated&gt;true&lt;/IsTruncated&gt;. To return the additional jobs, see Marker.
newtype MaxJobs = MaxJobs Int
derive instance newtypeMaxJobs :: Newtype MaxJobs _
derive instance repGenericMaxJobs :: Generic MaxJobs _
instance showMaxJobs :: Show MaxJobs where
  show = genericShow
instance decodeMaxJobs :: Decode MaxJobs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxJobs :: Encode MaxJobs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | One or more required customs parameters was missing from the manifest.
newtype MissingCustomsException = MissingCustomsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMissingCustomsException :: Newtype MissingCustomsException _
derive instance repGenericMissingCustomsException :: Generic MissingCustomsException _
instance showMissingCustomsException :: Show MissingCustomsException where
  show = genericShow
instance decodeMissingCustomsException :: Decode MissingCustomsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingCustomsException :: Encode MissingCustomsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | One or more required fields were missing from the manifest file. Please correct and resubmit.
newtype MissingManifestFieldException = MissingManifestFieldException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMissingManifestFieldException :: Newtype MissingManifestFieldException _
derive instance repGenericMissingManifestFieldException :: Generic MissingManifestFieldException _
instance showMissingManifestFieldException :: Show MissingManifestFieldException where
  show = genericShow
instance decodeMissingManifestFieldException :: Decode MissingManifestFieldException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingManifestFieldException :: Encode MissingManifestFieldException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | One or more required parameters was missing from the request.
newtype MissingParameterException = MissingParameterException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMissingParameterException :: Newtype MissingParameterException _
derive instance repGenericMissingParameterException :: Generic MissingParameterException _
instance showMissingParameterException :: Show MissingParameterException where
  show = genericShow
instance decodeMissingParameterException :: Decode MissingParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingParameterException :: Encode MissingParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Your manifest file contained buckets from multiple regions. A job is restricted to buckets from one region. Please correct and resubmit.
newtype MultipleRegionsException = MultipleRegionsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMultipleRegionsException :: Newtype MultipleRegionsException _
derive instance repGenericMultipleRegionsException :: Generic MultipleRegionsException _
instance showMultipleRegionsException :: Show MultipleRegionsException where
  show = genericShow
instance decodeMultipleRegionsException :: Decode MultipleRegionsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMultipleRegionsException :: Encode MultipleRegionsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The specified bucket does not exist. Create the specified bucket or change the manifest's bucket, exportBucket, or logBucket field to a bucket that the account, as specified by the manifest's Access Key ID, has write permissions to.
newtype NoSuchBucketException = NoSuchBucketException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchBucketException :: Newtype NoSuchBucketException _
derive instance repGenericNoSuchBucketException :: Generic NoSuchBucketException _
instance showNoSuchBucketException :: Show NoSuchBucketException where
  show = genericShow
instance decodeNoSuchBucketException :: Decode NoSuchBucketException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchBucketException :: Encode NoSuchBucketException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A token representing the state of the job, such as "Started".
newtype ProgressCode = ProgressCode String
derive instance newtypeProgressCode :: Newtype ProgressCode _
derive instance repGenericProgressCode :: Generic ProgressCode _
instance showProgressCode :: Show ProgressCode where
  show = genericShow
instance decodeProgressCode :: Decode ProgressCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProgressCode :: Encode ProgressCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A more human readable form of the job status.
newtype ProgressMessage = ProgressMessage String
derive instance newtypeProgressMessage :: Newtype ProgressMessage _
derive instance repGenericProgressMessage :: Generic ProgressMessage _
instance showProgressMessage :: Show ProgressMessage where
  show = genericShow
instance decodeProgressMessage :: Decode ProgressMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProgressMessage :: Encode ProgressMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An encrypted code used to authenticate the request and response, for example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to create the signature file yourself. Generally you should use the SignatureFileContents value.
newtype Signature = Signature String
derive instance newtypeSignature :: Newtype Signature _
derive instance repGenericSignature :: Generic Signature _
instance showSignature :: Show Signature where
  show = genericShow
instance decodeSignature :: Decode Signature where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignature :: Encode Signature where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The actual text of the SIGNATURE file to be written to disk.
newtype SignatureFileContents = SignatureFileContents String
derive instance newtypeSignatureFileContents :: Newtype SignatureFileContents _
derive instance repGenericSignatureFileContents :: Generic SignatureFileContents _
instance showSignatureFileContents :: Show SignatureFileContents where
  show = genericShow
instance decodeSignatureFileContents :: Decode SignatureFileContents where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignatureFileContents :: Encode SignatureFileContents where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies whether (true) or not (false) AWS Import/Export updated your job.
newtype Success = Success Boolean
derive instance newtypeSuccess :: Newtype Success _
derive instance repGenericSuccess :: Generic Success _
instance showSuccess :: Show Success where
  show = genericShow
instance decodeSuccess :: Decode Success where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSuccess :: Encode Success where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The shipping tracking number assigned by AWS Import/Export to the storage device when it's returned to you. We return this value when the LocationCode is "Returned".
newtype TrackingNumber = TrackingNumber String
derive instance newtypeTrackingNumber :: Newtype TrackingNumber _
derive instance repGenericTrackingNumber :: Generic TrackingNumber _
instance showTrackingNumber :: Show TrackingNumber where
  show = genericShow
instance decodeTrackingNumber :: Decode TrackingNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrackingNumber :: Encode TrackingNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The URL for a given Artifact.
newtype URL = URL String
derive instance newtypeURL :: Newtype URL _
derive instance repGenericURL :: Generic URL _
instance showURL :: Show URL where
  show = genericShow
instance decodeURL :: Decode URL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeURL :: Encode URL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | AWS Import/Export cannot cancel the job
newtype UnableToCancelJobIdException = UnableToCancelJobIdException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnableToCancelJobIdException :: Newtype UnableToCancelJobIdException _
derive instance repGenericUnableToCancelJobIdException :: Generic UnableToCancelJobIdException _
instance showUnableToCancelJobIdException :: Show UnableToCancelJobIdException where
  show = genericShow
instance decodeUnableToCancelJobIdException :: Decode UnableToCancelJobIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnableToCancelJobIdException :: Encode UnableToCancelJobIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | AWS Import/Export cannot update the job
newtype UnableToUpdateJobIdException = UnableToUpdateJobIdException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnableToUpdateJobIdException :: Newtype UnableToUpdateJobIdException _
derive instance repGenericUnableToUpdateJobIdException :: Generic UnableToUpdateJobIdException _
instance showUnableToUpdateJobIdException :: Show UnableToUpdateJobIdException where
  show = genericShow
instance decodeUnableToUpdateJobIdException :: Decode UnableToUpdateJobIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnableToUpdateJobIdException :: Encode UnableToUpdateJobIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Input structure for the UpateJob operation.
newtype UpdateJobInput = UpdateJobInput 
  { "JobId" :: (JobId)
  , "Manifest" :: (Manifest)
  , "JobType" :: (JobType)
  , "ValidateOnly" :: (ValidateOnly)
  , "APIVersion" :: NullOrUndefined.NullOrUndefined (APIVersion)
  }
derive instance newtypeUpdateJobInput :: Newtype UpdateJobInput _
derive instance repGenericUpdateJobInput :: Generic UpdateJobInput _
instance showUpdateJobInput :: Show UpdateJobInput where
  show = genericShow
instance decodeUpdateJobInput :: Decode UpdateJobInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateJobInput :: Encode UpdateJobInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Output structure for the UpateJob operation.
newtype UpdateJobOutput = UpdateJobOutput 
  { "Success" :: NullOrUndefined.NullOrUndefined (Success)
  , "WarningMessage" :: NullOrUndefined.NullOrUndefined (WarningMessage)
  , "ArtifactList" :: NullOrUndefined.NullOrUndefined (ArtifactList)
  }
derive instance newtypeUpdateJobOutput :: Newtype UpdateJobOutput _
derive instance repGenericUpdateJobOutput :: Generic UpdateJobOutput _
instance showUpdateJobOutput :: Show UpdateJobOutput where
  show = genericShow
instance decodeUpdateJobOutput :: Decode UpdateJobOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateJobOutput :: Encode UpdateJobOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Validate the manifest and parameter values in the request but do not actually create a job.
newtype ValidateOnly = ValidateOnly Boolean
derive instance newtypeValidateOnly :: Newtype ValidateOnly _
derive instance repGenericValidateOnly :: Generic ValidateOnly _
instance showValidateOnly :: Show ValidateOnly where
  show = genericShow
instance decodeValidateOnly :: Decode ValidateOnly where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidateOnly :: Encode ValidateOnly where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An optional message notifying you of non-fatal issues with the job, such as use of an incompatible Amazon S3 bucket name.
newtype WarningMessage = WarningMessage String
derive instance newtypeWarningMessage :: Newtype WarningMessage _
derive instance repGenericWarningMessage :: Generic WarningMessage _
instance showWarningMessage :: Show WarningMessage where
  show = genericShow
instance decodeWarningMessage :: Decode WarningMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWarningMessage :: Encode WarningMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the name of your city for the return address.
newtype City' = City' String
derive instance newtypeCity' :: Newtype City' _
derive instance repGenericCity' :: Generic City' _
instance showCity' :: Show City' where
  show = genericShow
instance decodeCity' :: Decode City' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCity' :: Encode City' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the name of the company that will ship this package.
newtype Company' = Company' String
derive instance newtypeCompany' :: Newtype Company' _
derive instance repGenericCompany' :: Generic Company' _
instance showCompany' :: Show Company' where
  show = genericShow
instance decodeCompany' :: Decode Company' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompany' :: Encode Company' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the name of your country for the return address.
newtype Country' = Country' String
derive instance newtypeCountry' :: Newtype Country' _
derive instance repGenericCountry' :: Generic Country' _
instance showCountry' :: Show Country' where
  show = genericShow
instance decodeCountry' :: Decode Country' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCountry' :: Encode Country' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the name of the person responsible for shipping this package.
newtype Name' = Name' String
derive instance newtypeName' :: Newtype Name' _
derive instance repGenericName' :: Generic Name' _
instance showName' :: Show Name' where
  show = genericShow
instance decodeName' :: Decode Name' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeName' :: Encode Name' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the phone number of the person responsible for shipping this package.
newtype PhoneNumber' = PhoneNumber' String
derive instance newtypePhoneNumber' :: Newtype PhoneNumber' _
derive instance repGenericPhoneNumber' :: Generic PhoneNumber' _
instance showPhoneNumber' :: Show PhoneNumber' where
  show = genericShow
instance decodePhoneNumber' :: Decode PhoneNumber' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePhoneNumber' :: Encode PhoneNumber' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the postal code for the return address.
newtype PostalCode' = PostalCode' String
derive instance newtypePostalCode' :: Newtype PostalCode' _
derive instance repGenericPostalCode' :: Generic PostalCode' _
instance showPostalCode' :: Show PostalCode' where
  show = genericShow
instance decodePostalCode' :: Decode PostalCode' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePostalCode' :: Encode PostalCode' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the name of your state or your province for the return address.
newtype StateOrProvince' = StateOrProvince' String
derive instance newtypeStateOrProvince' :: Newtype StateOrProvince' _
derive instance repGenericStateOrProvince' :: Generic StateOrProvince' _
instance showStateOrProvince' :: Show StateOrProvince' where
  show = genericShow
instance decodeStateOrProvince' :: Decode StateOrProvince' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateOrProvince' :: Encode StateOrProvince' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the first part of the street address for the return address, for example 1234 Main Street.
newtype Street1' = Street1' String
derive instance newtypeStreet1' :: Newtype Street1' _
derive instance repGenericStreet1' :: Generic Street1' _
instance showStreet1' :: Show Street1' where
  show = genericShow
instance decodeStreet1' :: Decode Street1' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreet1' :: Encode Street1' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the optional second part of the street address for the return address, for example Suite 100.
newtype Street2' = Street2' String
derive instance newtypeStreet2' :: Newtype Street2' _
derive instance repGenericStreet2' :: Generic Street2' _
instance showStreet2' :: Show Street2' where
  show = genericShow
instance decodeStreet2' :: Decode Street2' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreet2' :: Encode Street2' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the optional third part of the street address for the return address, for example c/o Jane Doe.
newtype Street3' = Street3' String
derive instance newtypeStreet3' :: Newtype Street3' _
derive instance repGenericStreet3' :: Generic Street3' _
instance showStreet3' :: Show Street3' where
  show = genericShow
instance decodeStreet3' :: Decode Street3' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreet3' :: Encode Street3' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
