

-- | <fullname>AWS Marketplace Entitlement Service</fullname> <p>This reference provides descriptions of the AWS Marketplace Entitlement Service API.</p> <p>AWS Marketplace Entitlement Service is used to determine the entitlement of a customer to a given product. An entitlement represents capacity in a product owned by the customer. For example, a customer might own some number of users or seats in an SaaS application or some amount of data capacity in a multi-tenant database.</p> <p> <b>Getting Entitlement Records</b> </p> <ul> <li> <p> <i>GetEntitlements</i>- Gets the entitlements for a Marketplace product.</p> </li> </ul>
module AWS.MarketplaceEntitlementService where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MarketplaceEntitlementService" :: String


-- | <p>GetEntitlements retrieves entitlement values for a given product. The results can be filtered based on customer identifier or product dimensions.</p>
getEntitlements :: forall eff. GetEntitlementsRequest -> Aff (err :: AWS.RequestError | eff) GetEntitlementsResult
getEntitlements = AWS.request serviceName "getEntitlements" 


-- | <p>An entitlement represents capacity in a product owned by the customer. For example, a customer might own some number of users or seats in an SaaS application or some amount of data capacity in a multi-tenant database.</p>
newtype Entitlement = Entitlement 
  { "ProductCode" :: NullOrUndefined (ProductCode)
  , "Dimension" :: NullOrUndefined (NonEmptyString)
  , "CustomerIdentifier" :: NullOrUndefined (NonEmptyString)
  , "Value" :: NullOrUndefined (EntitlementValue)
  , "ExpirationDate" :: NullOrUndefined (Number)
  }
derive instance newtypeEntitlement :: Newtype Entitlement _


newtype EntitlementList = EntitlementList (Array Entitlement)
derive instance newtypeEntitlementList :: Newtype EntitlementList _


-- | <p>The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.</p>
newtype EntitlementValue = EntitlementValue 
  { "IntegerValue" :: NullOrUndefined (Int)
  , "DoubleValue" :: NullOrUndefined (Number)
  , "BooleanValue" :: NullOrUndefined (Boolean)
  , "StringValue" :: NullOrUndefined (String)
  }
derive instance newtypeEntitlementValue :: Newtype EntitlementValue _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype FilterValue = FilterValue String
derive instance newtypeFilterValue :: Newtype FilterValue _


newtype FilterValueList = FilterValueList (Array FilterValue)
derive instance newtypeFilterValueList :: Newtype FilterValueList _


newtype GetEntitlementFilterName = GetEntitlementFilterName String
derive instance newtypeGetEntitlementFilterName :: Newtype GetEntitlementFilterName _


newtype GetEntitlementFilters = GetEntitlementFilters (Map GetEntitlementFilterName FilterValueList)
derive instance newtypeGetEntitlementFilters :: Newtype GetEntitlementFilters _


-- | <p>The GetEntitlementsRequest contains parameters for the GetEntitlements operation.</p>
newtype GetEntitlementsRequest = GetEntitlementsRequest 
  { "ProductCode" :: (ProductCode)
  , "Filter" :: NullOrUndefined (GetEntitlementFilters)
  , "NextToken" :: NullOrUndefined (NonEmptyString)
  , "MaxResults" :: NullOrUndefined (Int)
  }
derive instance newtypeGetEntitlementsRequest :: Newtype GetEntitlementsRequest _


-- | <p>The GetEntitlementsRequest contains results from the GetEntitlements operation.</p>
newtype GetEntitlementsResult = GetEntitlementsResult 
  { "Entitlements" :: NullOrUndefined (EntitlementList)
  , "NextToken" :: NullOrUndefined (NonEmptyString)
  }
derive instance newtypeGetEntitlementsResult :: Newtype GetEntitlementsResult _


-- | <p>An internal error has occurred. Retry your request. If the problem persists, post a message with details on the AWS forums.</p>
newtype InternalServiceErrorException = InternalServiceErrorException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServiceErrorException :: Newtype InternalServiceErrorException _


-- | <p>One or more parameters in your request was invalid.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _


newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString :: Newtype NonEmptyString _


newtype ProductCode = ProductCode String
derive instance newtypeProductCode :: Newtype ProductCode _


-- | <p>The calls to the GetEntitlements API are throttled.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _
