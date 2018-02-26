

-- | <fullname>AWS Marketplace Entitlement Service</fullname> <p>This reference provides descriptions of the AWS Marketplace Entitlement Service API.</p> <p>AWS Marketplace Entitlement Service is used to determine the entitlement of a customer to a given product. An entitlement represents capacity in a product owned by the customer. For example, a customer might own some number of users or seats in an SaaS application or some amount of data capacity in a multi-tenant database.</p> <p> <b>Getting Entitlement Records</b> </p> <ul> <li> <p> <i>GetEntitlements</i>- Gets the entitlements for a Marketplace product.</p> </li> </ul>
module AWS.MarketplaceEntitlementService where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MarketplaceEntitlementService" :: String


-- | <p>GetEntitlements retrieves entitlement values for a given product. The results can be filtered based on customer identifier or product dimensions.</p>
getEntitlements :: forall eff. GetEntitlementsRequest -> Aff (err :: AWS.RequestError | eff) GetEntitlementsResult
getEntitlements = AWS.request serviceName "GetEntitlements" 


-- | <p>An entitlement represents capacity in a product owned by the customer. For example, a customer might own some number of users or seats in an SaaS application or some amount of data capacity in a multi-tenant database.</p>
newtype Entitlement = Entitlement 
  { "ProductCode" :: NullOrUndefined (ProductCode)
  , "Dimension" :: NullOrUndefined (NonEmptyString)
  , "CustomerIdentifier" :: NullOrUndefined (NonEmptyString)
  , "Value" :: NullOrUndefined (EntitlementValue)
  , "ExpirationDate" :: NullOrUndefined (Number)
  }


newtype EntitlementList = EntitlementList (Array Entitlement)


-- | <p>The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.</p>
newtype EntitlementValue = EntitlementValue 
  { "IntegerValue" :: NullOrUndefined (Int)
  , "DoubleValue" :: NullOrUndefined (Number)
  , "BooleanValue" :: NullOrUndefined (Boolean)
  , "StringValue" :: NullOrUndefined (String)
  }


newtype ErrorMessage = ErrorMessage String


newtype FilterValue = FilterValue String


newtype FilterValueList = FilterValueList (Array FilterValue)


newtype GetEntitlementFilterName = GetEntitlementFilterName String


newtype GetEntitlementFilters = GetEntitlementFilters (Map GetEntitlementFilterName FilterValueList)


-- | <p>The GetEntitlementsRequest contains parameters for the GetEntitlements operation.</p>
newtype GetEntitlementsRequest = GetEntitlementsRequest 
  { "ProductCode" :: (ProductCode)
  , "Filter" :: NullOrUndefined (GetEntitlementFilters)
  , "NextToken" :: NullOrUndefined (NonEmptyString)
  , "MaxResults" :: NullOrUndefined (Int)
  }


-- | <p>The GetEntitlementsRequest contains results from the GetEntitlements operation.</p>
newtype GetEntitlementsResult = GetEntitlementsResult 
  { "Entitlements" :: NullOrUndefined (EntitlementList)
  , "NextToken" :: NullOrUndefined (NonEmptyString)
  }


-- | <p>An internal error has occurred. Retry your request. If the problem persists, post a message with details on the AWS forums.</p>
newtype InternalServiceErrorException = InternalServiceErrorException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>One or more parameters in your request was invalid.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype NonEmptyString = NonEmptyString String


newtype ProductCode = ProductCode String


-- | <p>The calls to the GetEntitlements API are throttled.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
