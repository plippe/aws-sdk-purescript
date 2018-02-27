

-- | <fullname>AWS Marketplace Metering Service</fullname> <p>This reference provides descriptions of the low-level AWS Marketplace Metering Service API.</p> <p>AWS Marketplace sellers can use this API to submit usage data for custom usage dimensions.</p> <p> <b>Submitting Metering Records</b> </p> <ul> <li> <p> <i>MeterUsage</i>- Submits the metering record for a Marketplace product. MeterUsage is called from an EC2 instance.</p> </li> <li> <p> <i>BatchMeterUsage</i>- Submits the metering record for a set of customers. BatchMeterUsage is called from a software-as-a-service (SaaS) application.</p> </li> </ul> <p> <b>Accepting New Customers</b> </p> <ul> <li> <p> <i>ResolveCustomer</i>- Called by a SaaS application during the registration process. When a buyer visits your website during the registration process, the buyer submits a Registration Token through the browser. The Registration Token is resolved through this API to obtain a CustomerIdentifier and Product Code.</p> </li> </ul>
module AWS.MarketplaceMetering where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MarketplaceMetering" :: String


-- | <p>BatchMeterUsage is called from a SaaS application listed on the AWS Marketplace to post metering records for a set of customers.</p> <p>For identical requests, the API is idempotent; requests can be retried with the same records or a subset of the input records.</p> <p>Every request to BatchMeterUsage is for one product. If you need to meter usage for multiple products, you must make multiple calls to BatchMeterUsage.</p> <p>BatchMeterUsage can process up to 25 UsageRecords at a time.</p>
batchMeterUsage :: forall eff. BatchMeterUsageRequest -> Aff (err :: AWS.RequestError | eff) BatchMeterUsageResult
batchMeterUsage = AWS.request serviceName "BatchMeterUsage" 


-- | <p>API to emit metering records. For identical requests, the API is idempotent. It simply returns the metering record ID.</p> <p>MeterUsage is authenticated on the buyer's AWS account, generally when running from an EC2 instance on the AWS Marketplace.</p>
meterUsage :: forall eff. MeterUsageRequest -> Aff (err :: AWS.RequestError | eff) MeterUsageResult
meterUsage = AWS.request serviceName "MeterUsage" 


-- | <p>ResolveCustomer is called by a SaaS application during the registration process. When a buyer visits your website during the registration process, the buyer submits a registration token through their browser. The registration token is resolved through this API to obtain a CustomerIdentifier and product code.</p>
resolveCustomer :: forall eff. ResolveCustomerRequest -> Aff (err :: AWS.RequestError | eff) ResolveCustomerResult
resolveCustomer = AWS.request serviceName "ResolveCustomer" 


-- | <p>A BatchMeterUsageRequest contains UsageRecords, which indicate quantities of usage within your application.</p>
newtype BatchMeterUsageRequest = BatchMeterUsageRequest 
  { "UsageRecords" :: (UsageRecordList)
  , "ProductCode" :: (ProductCode)
  }
derive instance newtypeBatchMeterUsageRequest :: Newtype BatchMeterUsageRequest _


-- | <p>Contains the UsageRecords processed by BatchMeterUsage and any records that have failed due to transient error.</p>
newtype BatchMeterUsageResult = BatchMeterUsageResult 
  { "Results" :: NullOrUndefined (UsageRecordResultList)
  , "UnprocessedRecords" :: NullOrUndefined (UsageRecordList)
  }
derive instance newtypeBatchMeterUsageResult :: Newtype BatchMeterUsageResult _


newtype CustomerIdentifier = CustomerIdentifier String
derive instance newtypeCustomerIdentifier :: Newtype CustomerIdentifier _


-- | <p>A metering record has already been emitted by the same EC2 instance for the given {usageDimension, timestamp} with a different usageQuantity.</p>
newtype DuplicateRequestException = DuplicateRequestException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeDuplicateRequestException :: Newtype DuplicateRequestException _


-- | <p>The submitted registration token has expired. This can happen if the buyer's browser takes too long to redirect to your page, the buyer has resubmitted the registration token, or your application has held on to the registration token for too long. Your SaaS registration website should redeem this token as soon as it is submitted by the buyer's browser.</p>
newtype ExpiredTokenException = ExpiredTokenException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeExpiredTokenException :: Newtype ExpiredTokenException _


-- | <p>An internal error has occurred. Retry your request. If the problem persists, post a message with details on the AWS forums.</p>
newtype InternalServiceErrorException = InternalServiceErrorException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInternalServiceErrorException :: Newtype InternalServiceErrorException _


-- | <p>You have metered usage for a CustomerIdentifier that does not exist.</p>
newtype InvalidCustomerIdentifierException = InvalidCustomerIdentifierException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidCustomerIdentifierException :: Newtype InvalidCustomerIdentifierException _


-- | <p>The endpoint being called is in a region different from your EC2 instance. The region of the Metering service endpoint and the region of the EC2 instance must match.</p>
newtype InvalidEndpointRegionException = InvalidEndpointRegionException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidEndpointRegionException :: Newtype InvalidEndpointRegionException _


-- | <p>The product code passed does not match the product code used for publishing the product.</p>
newtype InvalidProductCodeException = InvalidProductCodeException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidProductCodeException :: Newtype InvalidProductCodeException _


newtype InvalidTokenException = InvalidTokenException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidTokenException :: Newtype InvalidTokenException _


-- | <p>The usage dimension does not match one of the UsageDimensions associated with products.</p>
newtype InvalidUsageDimensionException = InvalidUsageDimensionException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidUsageDimensionException :: Newtype InvalidUsageDimensionException _


newtype MeterUsageRequest = MeterUsageRequest 
  { "ProductCode" :: (ProductCode)
  , "Number" :: (Number)
  , "UsageDimension" :: (UsageDimension)
  , "UsageQuantity" :: (UsageQuantity)
  , "DryRun" :: (Boolean)
  }
derive instance newtypeMeterUsageRequest :: Newtype MeterUsageRequest _


newtype MeterUsageResult = MeterUsageResult 
  { "MeteringRecordId" :: NullOrUndefined (String)
  }
derive instance newtypeMeterUsageResult :: Newtype MeterUsageResult _


newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString :: Newtype NonEmptyString _


newtype ProductCode = ProductCode String
derive instance newtypeProductCode :: Newtype ProductCode _


-- | <p>Contains input to the ResolveCustomer operation.</p>
newtype ResolveCustomerRequest = ResolveCustomerRequest 
  { "RegistrationToken" :: (NonEmptyString)
  }
derive instance newtypeResolveCustomerRequest :: Newtype ResolveCustomerRequest _


-- | <p>The result of the ResolveCustomer operation. Contains the CustomerIdentifier and product code.</p>
newtype ResolveCustomerResult = ResolveCustomerResult 
  { "CustomerIdentifier" :: NullOrUndefined (CustomerIdentifier)
  , "ProductCode" :: NullOrUndefined (ProductCode)
  }
derive instance newtypeResolveCustomerResult :: Newtype ResolveCustomerResult _


-- | <p>The calls to the MeterUsage API are throttled.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _


-- | <p>The timestamp value passed in the meterUsage() is out of allowed range.</p>
newtype TimestampOutOfBoundsException = TimestampOutOfBoundsException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeTimestampOutOfBoundsException :: Newtype TimestampOutOfBoundsException _


newtype UsageDimension = UsageDimension String
derive instance newtypeUsageDimension :: Newtype UsageDimension _


newtype UsageQuantity = UsageQuantity Int
derive instance newtypeUsageQuantity :: Newtype UsageQuantity _


-- | <p>A UsageRecord indicates a quantity of usage for a given product, customer, dimension and time.</p> <p>Multiple requests with the same UsageRecords as input will be deduplicated to prevent double charges.</p>
newtype UsageRecord = UsageRecord 
  { "Number" :: (Number)
  , "CustomerIdentifier" :: (CustomerIdentifier)
  , "Dimension" :: (UsageDimension)
  , "Quantity" :: (UsageQuantity)
  }
derive instance newtypeUsageRecord :: Newtype UsageRecord _


newtype UsageRecordList = UsageRecordList (Array UsageRecord)
derive instance newtypeUsageRecordList :: Newtype UsageRecordList _


-- | <p>A UsageRecordResult indicates the status of a given UsageRecord processed by BatchMeterUsage.</p>
newtype UsageRecordResult = UsageRecordResult 
  { "UsageRecord" :: NullOrUndefined (UsageRecord)
  , "MeteringRecordId" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (UsageRecordResultStatus)
  }
derive instance newtypeUsageRecordResult :: Newtype UsageRecordResult _


newtype UsageRecordResultList = UsageRecordResultList (Array UsageRecordResult)
derive instance newtypeUsageRecordResultList :: Newtype UsageRecordResultList _


newtype UsageRecordResultStatus = UsageRecordResultStatus String
derive instance newtypeUsageRecordResultStatus :: Newtype UsageRecordResultStatus _


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
