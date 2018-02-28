

-- | <fullname>AWS Marketplace Metering Service</fullname> <p>This reference provides descriptions of the low-level AWS Marketplace Metering Service API.</p> <p>AWS Marketplace sellers can use this API to submit usage data for custom usage dimensions.</p> <p> <b>Submitting Metering Records</b> </p> <ul> <li> <p> <i>MeterUsage</i>- Submits the metering record for a Marketplace product. MeterUsage is called from an EC2 instance.</p> </li> <li> <p> <i>BatchMeterUsage</i>- Submits the metering record for a set of customers. BatchMeterUsage is called from a software-as-a-service (SaaS) application.</p> </li> </ul> <p> <b>Accepting New Customers</b> </p> <ul> <li> <p> <i>ResolveCustomer</i>- Called by a SaaS application during the registration process. When a buyer visits your website during the registration process, the buyer submits a Registration Token through the browser. The Registration Token is resolved through this API to obtain a CustomerIdentifier and Product Code.</p> </li> </ul>
module AWS.MarketplaceMetering where

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

serviceName = "MarketplaceMetering" :: String


-- | <p>BatchMeterUsage is called from a SaaS application listed on the AWS Marketplace to post metering records for a set of customers.</p> <p>For identical requests, the API is idempotent; requests can be retried with the same records or a subset of the input records.</p> <p>Every request to BatchMeterUsage is for one product. If you need to meter usage for multiple products, you must make multiple calls to BatchMeterUsage.</p> <p>BatchMeterUsage can process up to 25 UsageRecords at a time.</p>
batchMeterUsage :: forall eff. BatchMeterUsageRequest -> Aff (exception :: EXCEPTION | eff) BatchMeterUsageResult
batchMeterUsage = Request.request serviceName "batchMeterUsage" 


-- | <p>API to emit metering records. For identical requests, the API is idempotent. It simply returns the metering record ID.</p> <p>MeterUsage is authenticated on the buyer's AWS account, generally when running from an EC2 instance on the AWS Marketplace.</p>
meterUsage :: forall eff. MeterUsageRequest -> Aff (exception :: EXCEPTION | eff) MeterUsageResult
meterUsage = Request.request serviceName "meterUsage" 


-- | <p>ResolveCustomer is called by a SaaS application during the registration process. When a buyer visits your website during the registration process, the buyer submits a registration token through their browser. The registration token is resolved through this API to obtain a CustomerIdentifier and product code.</p>
resolveCustomer :: forall eff. ResolveCustomerRequest -> Aff (exception :: EXCEPTION | eff) ResolveCustomerResult
resolveCustomer = Request.request serviceName "resolveCustomer" 


-- | <p>A BatchMeterUsageRequest contains UsageRecords, which indicate quantities of usage within your application.</p>
newtype BatchMeterUsageRequest = BatchMeterUsageRequest 
  { "UsageRecords" :: (UsageRecordList)
  , "ProductCode" :: (ProductCode)
  }
derive instance newtypeBatchMeterUsageRequest :: Newtype BatchMeterUsageRequest _
derive instance repGenericBatchMeterUsageRequest :: Generic BatchMeterUsageRequest _
instance showBatchMeterUsageRequest :: Show BatchMeterUsageRequest where
  show = genericShow
instance decodeBatchMeterUsageRequest :: Decode BatchMeterUsageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchMeterUsageRequest :: Encode BatchMeterUsageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the UsageRecords processed by BatchMeterUsage and any records that have failed due to transient error.</p>
newtype BatchMeterUsageResult = BatchMeterUsageResult 
  { "Results" :: NullOrUndefined.NullOrUndefined (UsageRecordResultList)
  , "UnprocessedRecords" :: NullOrUndefined.NullOrUndefined (UsageRecordList)
  }
derive instance newtypeBatchMeterUsageResult :: Newtype BatchMeterUsageResult _
derive instance repGenericBatchMeterUsageResult :: Generic BatchMeterUsageResult _
instance showBatchMeterUsageResult :: Show BatchMeterUsageResult where
  show = genericShow
instance decodeBatchMeterUsageResult :: Decode BatchMeterUsageResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchMeterUsageResult :: Encode BatchMeterUsageResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomerIdentifier = CustomerIdentifier String
derive instance newtypeCustomerIdentifier :: Newtype CustomerIdentifier _
derive instance repGenericCustomerIdentifier :: Generic CustomerIdentifier _
instance showCustomerIdentifier :: Show CustomerIdentifier where
  show = genericShow
instance decodeCustomerIdentifier :: Decode CustomerIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomerIdentifier :: Encode CustomerIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A metering record has already been emitted by the same EC2 instance for the given {usageDimension, timestamp} with a different usageQuantity.</p>
newtype DuplicateRequestException = DuplicateRequestException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeDuplicateRequestException :: Newtype DuplicateRequestException _
derive instance repGenericDuplicateRequestException :: Generic DuplicateRequestException _
instance showDuplicateRequestException :: Show DuplicateRequestException where
  show = genericShow
instance decodeDuplicateRequestException :: Decode DuplicateRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateRequestException :: Encode DuplicateRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The submitted registration token has expired. This can happen if the buyer's browser takes too long to redirect to your page, the buyer has resubmitted the registration token, or your application has held on to the registration token for too long. Your SaaS registration website should redeem this token as soon as it is submitted by the buyer's browser.</p>
newtype ExpiredTokenException = ExpiredTokenException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeExpiredTokenException :: Newtype ExpiredTokenException _
derive instance repGenericExpiredTokenException :: Generic ExpiredTokenException _
instance showExpiredTokenException :: Show ExpiredTokenException where
  show = genericShow
instance decodeExpiredTokenException :: Decode ExpiredTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpiredTokenException :: Encode ExpiredTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An internal error has occurred. Retry your request. If the problem persists, post a message with details on the AWS forums.</p>
newtype InternalServiceErrorException = InternalServiceErrorException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInternalServiceErrorException :: Newtype InternalServiceErrorException _
derive instance repGenericInternalServiceErrorException :: Generic InternalServiceErrorException _
instance showInternalServiceErrorException :: Show InternalServiceErrorException where
  show = genericShow
instance decodeInternalServiceErrorException :: Decode InternalServiceErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServiceErrorException :: Encode InternalServiceErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have metered usage for a CustomerIdentifier that does not exist.</p>
newtype InvalidCustomerIdentifierException = InvalidCustomerIdentifierException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidCustomerIdentifierException :: Newtype InvalidCustomerIdentifierException _
derive instance repGenericInvalidCustomerIdentifierException :: Generic InvalidCustomerIdentifierException _
instance showInvalidCustomerIdentifierException :: Show InvalidCustomerIdentifierException where
  show = genericShow
instance decodeInvalidCustomerIdentifierException :: Decode InvalidCustomerIdentifierException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCustomerIdentifierException :: Encode InvalidCustomerIdentifierException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The endpoint being called is in a region different from your EC2 instance. The region of the Metering service endpoint and the region of the EC2 instance must match.</p>
newtype InvalidEndpointRegionException = InvalidEndpointRegionException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidEndpointRegionException :: Newtype InvalidEndpointRegionException _
derive instance repGenericInvalidEndpointRegionException :: Generic InvalidEndpointRegionException _
instance showInvalidEndpointRegionException :: Show InvalidEndpointRegionException where
  show = genericShow
instance decodeInvalidEndpointRegionException :: Decode InvalidEndpointRegionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidEndpointRegionException :: Encode InvalidEndpointRegionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The product code passed does not match the product code used for publishing the product.</p>
newtype InvalidProductCodeException = InvalidProductCodeException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidProductCodeException :: Newtype InvalidProductCodeException _
derive instance repGenericInvalidProductCodeException :: Generic InvalidProductCodeException _
instance showInvalidProductCodeException :: Show InvalidProductCodeException where
  show = genericShow
instance decodeInvalidProductCodeException :: Decode InvalidProductCodeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidProductCodeException :: Encode InvalidProductCodeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidTokenException = InvalidTokenException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidTokenException :: Newtype InvalidTokenException _
derive instance repGenericInvalidTokenException :: Generic InvalidTokenException _
instance showInvalidTokenException :: Show InvalidTokenException where
  show = genericShow
instance decodeInvalidTokenException :: Decode InvalidTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTokenException :: Encode InvalidTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The usage dimension does not match one of the UsageDimensions associated with products.</p>
newtype InvalidUsageDimensionException = InvalidUsageDimensionException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidUsageDimensionException :: Newtype InvalidUsageDimensionException _
derive instance repGenericInvalidUsageDimensionException :: Generic InvalidUsageDimensionException _
instance showInvalidUsageDimensionException :: Show InvalidUsageDimensionException where
  show = genericShow
instance decodeInvalidUsageDimensionException :: Decode InvalidUsageDimensionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidUsageDimensionException :: Encode InvalidUsageDimensionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MeterUsageRequest = MeterUsageRequest 
  { "ProductCode" :: (ProductCode)
  , "Number" :: (Number)
  , "UsageDimension" :: (UsageDimension)
  , "UsageQuantity" :: (UsageQuantity)
  , "DryRun" :: (Boolean)
  }
derive instance newtypeMeterUsageRequest :: Newtype MeterUsageRequest _
derive instance repGenericMeterUsageRequest :: Generic MeterUsageRequest _
instance showMeterUsageRequest :: Show MeterUsageRequest where
  show = genericShow
instance decodeMeterUsageRequest :: Decode MeterUsageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMeterUsageRequest :: Encode MeterUsageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MeterUsageResult = MeterUsageResult 
  { "MeteringRecordId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMeterUsageResult :: Newtype MeterUsageResult _
derive instance repGenericMeterUsageResult :: Generic MeterUsageResult _
instance showMeterUsageResult :: Show MeterUsageResult where
  show = genericShow
instance decodeMeterUsageResult :: Decode MeterUsageResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMeterUsageResult :: Encode MeterUsageResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString :: Newtype NonEmptyString _
derive instance repGenericNonEmptyString :: Generic NonEmptyString _
instance showNonEmptyString :: Show NonEmptyString where
  show = genericShow
instance decodeNonEmptyString :: Decode NonEmptyString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonEmptyString :: Encode NonEmptyString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProductCode = ProductCode String
derive instance newtypeProductCode :: Newtype ProductCode _
derive instance repGenericProductCode :: Generic ProductCode _
instance showProductCode :: Show ProductCode where
  show = genericShow
instance decodeProductCode :: Decode ProductCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProductCode :: Encode ProductCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains input to the ResolveCustomer operation.</p>
newtype ResolveCustomerRequest = ResolveCustomerRequest 
  { "RegistrationToken" :: (NonEmptyString)
  }
derive instance newtypeResolveCustomerRequest :: Newtype ResolveCustomerRequest _
derive instance repGenericResolveCustomerRequest :: Generic ResolveCustomerRequest _
instance showResolveCustomerRequest :: Show ResolveCustomerRequest where
  show = genericShow
instance decodeResolveCustomerRequest :: Decode ResolveCustomerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResolveCustomerRequest :: Encode ResolveCustomerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of the ResolveCustomer operation. Contains the CustomerIdentifier and product code.</p>
newtype ResolveCustomerResult = ResolveCustomerResult 
  { "CustomerIdentifier" :: NullOrUndefined.NullOrUndefined (CustomerIdentifier)
  , "ProductCode" :: NullOrUndefined.NullOrUndefined (ProductCode)
  }
derive instance newtypeResolveCustomerResult :: Newtype ResolveCustomerResult _
derive instance repGenericResolveCustomerResult :: Generic ResolveCustomerResult _
instance showResolveCustomerResult :: Show ResolveCustomerResult where
  show = genericShow
instance decodeResolveCustomerResult :: Decode ResolveCustomerResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResolveCustomerResult :: Encode ResolveCustomerResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The calls to the MeterUsage API are throttled.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _
derive instance repGenericThrottlingException :: Generic ThrottlingException _
instance showThrottlingException :: Show ThrottlingException where
  show = genericShow
instance decodeThrottlingException :: Decode ThrottlingException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThrottlingException :: Encode ThrottlingException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The timestamp value passed in the meterUsage() is out of allowed range.</p>
newtype TimestampOutOfBoundsException = TimestampOutOfBoundsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeTimestampOutOfBoundsException :: Newtype TimestampOutOfBoundsException _
derive instance repGenericTimestampOutOfBoundsException :: Generic TimestampOutOfBoundsException _
instance showTimestampOutOfBoundsException :: Show TimestampOutOfBoundsException where
  show = genericShow
instance decodeTimestampOutOfBoundsException :: Decode TimestampOutOfBoundsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimestampOutOfBoundsException :: Encode TimestampOutOfBoundsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UsageDimension = UsageDimension String
derive instance newtypeUsageDimension :: Newtype UsageDimension _
derive instance repGenericUsageDimension :: Generic UsageDimension _
instance showUsageDimension :: Show UsageDimension where
  show = genericShow
instance decodeUsageDimension :: Decode UsageDimension where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsageDimension :: Encode UsageDimension where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UsageQuantity = UsageQuantity Int
derive instance newtypeUsageQuantity :: Newtype UsageQuantity _
derive instance repGenericUsageQuantity :: Generic UsageQuantity _
instance showUsageQuantity :: Show UsageQuantity where
  show = genericShow
instance decodeUsageQuantity :: Decode UsageQuantity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsageQuantity :: Encode UsageQuantity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A UsageRecord indicates a quantity of usage for a given product, customer, dimension and time.</p> <p>Multiple requests with the same UsageRecords as input will be deduplicated to prevent double charges.</p>
newtype UsageRecord = UsageRecord 
  { "Number" :: (Number)
  , "CustomerIdentifier" :: (CustomerIdentifier)
  , "Dimension" :: (UsageDimension)
  , "Quantity" :: (UsageQuantity)
  }
derive instance newtypeUsageRecord :: Newtype UsageRecord _
derive instance repGenericUsageRecord :: Generic UsageRecord _
instance showUsageRecord :: Show UsageRecord where
  show = genericShow
instance decodeUsageRecord :: Decode UsageRecord where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsageRecord :: Encode UsageRecord where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UsageRecordList = UsageRecordList (Array UsageRecord)
derive instance newtypeUsageRecordList :: Newtype UsageRecordList _
derive instance repGenericUsageRecordList :: Generic UsageRecordList _
instance showUsageRecordList :: Show UsageRecordList where
  show = genericShow
instance decodeUsageRecordList :: Decode UsageRecordList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsageRecordList :: Encode UsageRecordList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A UsageRecordResult indicates the status of a given UsageRecord processed by BatchMeterUsage.</p>
newtype UsageRecordResult = UsageRecordResult 
  { "UsageRecord" :: NullOrUndefined.NullOrUndefined (UsageRecord)
  , "MeteringRecordId" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (UsageRecordResultStatus)
  }
derive instance newtypeUsageRecordResult :: Newtype UsageRecordResult _
derive instance repGenericUsageRecordResult :: Generic UsageRecordResult _
instance showUsageRecordResult :: Show UsageRecordResult where
  show = genericShow
instance decodeUsageRecordResult :: Decode UsageRecordResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsageRecordResult :: Encode UsageRecordResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UsageRecordResultList = UsageRecordResultList (Array UsageRecordResult)
derive instance newtypeUsageRecordResultList :: Newtype UsageRecordResultList _
derive instance repGenericUsageRecordResultList :: Generic UsageRecordResultList _
instance showUsageRecordResultList :: Show UsageRecordResultList where
  show = genericShow
instance decodeUsageRecordResultList :: Decode UsageRecordResultList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsageRecordResultList :: Encode UsageRecordResultList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UsageRecordResultStatus = UsageRecordResultStatus String
derive instance newtypeUsageRecordResultStatus :: Newtype UsageRecordResultStatus _
derive instance repGenericUsageRecordResultStatus :: Generic UsageRecordResultStatus _
instance showUsageRecordResultStatus :: Show UsageRecordResultStatus where
  show = genericShow
instance decodeUsageRecordResultStatus :: Decode UsageRecordResultStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsageRecordResultStatus :: Encode UsageRecordResultStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
derive instance repGenericErrorMessage' :: Generic ErrorMessage' _
instance showErrorMessage' :: Show ErrorMessage' where
  show = genericShow
instance decodeErrorMessage' :: Decode ErrorMessage' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage' :: Encode ErrorMessage' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
