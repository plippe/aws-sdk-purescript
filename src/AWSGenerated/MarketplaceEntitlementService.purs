

-- | <fullname>AWS Marketplace Entitlement Service</fullname> <p>This reference provides descriptions of the AWS Marketplace Entitlement Service API.</p> <p>AWS Marketplace Entitlement Service is used to determine the entitlement of a customer to a given product. An entitlement represents capacity in a product owned by the customer. For example, a customer might own some number of users or seats in an SaaS application or some amount of data capacity in a multi-tenant database.</p> <p> <b>Getting Entitlement Records</b> </p> <ul> <li> <p> <i>GetEntitlements</i>- Gets the entitlements for a Marketplace product.</p> </li> </ul>
module AWS.MarketplaceEntitlementService where

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

serviceName = "MarketplaceEntitlementService" :: String


-- | <p>GetEntitlements retrieves entitlement values for a given product. The results can be filtered based on customer identifier or product dimensions.</p>
getEntitlements :: forall eff. GetEntitlementsRequest -> Aff (exception :: EXCEPTION | eff) GetEntitlementsResult
getEntitlements = Request.request serviceName "getEntitlements" 


-- | <p>An entitlement represents capacity in a product owned by the customer. For example, a customer might own some number of users or seats in an SaaS application or some amount of data capacity in a multi-tenant database.</p>
newtype Entitlement = Entitlement 
  { "ProductCode" :: NullOrUndefined.NullOrUndefined (ProductCode)
  , "Dimension" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "CustomerIdentifier" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Value" :: NullOrUndefined.NullOrUndefined (EntitlementValue)
  , "ExpirationDate" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeEntitlement :: Newtype Entitlement _
derive instance repGenericEntitlement :: Generic Entitlement _
instance showEntitlement :: Show Entitlement where
  show = genericShow
instance decodeEntitlement :: Decode Entitlement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntitlement :: Encode Entitlement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EntitlementList = EntitlementList (Array Entitlement)
derive instance newtypeEntitlementList :: Newtype EntitlementList _
derive instance repGenericEntitlementList :: Generic EntitlementList _
instance showEntitlementList :: Show EntitlementList where
  show = genericShow
instance decodeEntitlementList :: Decode EntitlementList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntitlementList :: Encode EntitlementList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.</p>
newtype EntitlementValue = EntitlementValue 
  { "IntegerValue" :: NullOrUndefined.NullOrUndefined (Int)
  , "DoubleValue" :: NullOrUndefined.NullOrUndefined (Number)
  , "BooleanValue" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "StringValue" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEntitlementValue :: Newtype EntitlementValue _
derive instance repGenericEntitlementValue :: Generic EntitlementValue _
instance showEntitlementValue :: Show EntitlementValue where
  show = genericShow
instance decodeEntitlementValue :: Decode EntitlementValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntitlementValue :: Encode EntitlementValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterValue = FilterValue String
derive instance newtypeFilterValue :: Newtype FilterValue _
derive instance repGenericFilterValue :: Generic FilterValue _
instance showFilterValue :: Show FilterValue where
  show = genericShow
instance decodeFilterValue :: Decode FilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterValue :: Encode FilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterValueList = FilterValueList (Array FilterValue)
derive instance newtypeFilterValueList :: Newtype FilterValueList _
derive instance repGenericFilterValueList :: Generic FilterValueList _
instance showFilterValueList :: Show FilterValueList where
  show = genericShow
instance decodeFilterValueList :: Decode FilterValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterValueList :: Encode FilterValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetEntitlementFilterName = GetEntitlementFilterName String
derive instance newtypeGetEntitlementFilterName :: Newtype GetEntitlementFilterName _
derive instance repGenericGetEntitlementFilterName :: Generic GetEntitlementFilterName _
instance showGetEntitlementFilterName :: Show GetEntitlementFilterName where
  show = genericShow
instance decodeGetEntitlementFilterName :: Decode GetEntitlementFilterName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEntitlementFilterName :: Encode GetEntitlementFilterName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetEntitlementFilters = GetEntitlementFilters (StrMap.StrMap FilterValueList)
derive instance newtypeGetEntitlementFilters :: Newtype GetEntitlementFilters _
derive instance repGenericGetEntitlementFilters :: Generic GetEntitlementFilters _
instance showGetEntitlementFilters :: Show GetEntitlementFilters where
  show = genericShow
instance decodeGetEntitlementFilters :: Decode GetEntitlementFilters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEntitlementFilters :: Encode GetEntitlementFilters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The GetEntitlementsRequest contains parameters for the GetEntitlements operation.</p>
newtype GetEntitlementsRequest = GetEntitlementsRequest 
  { "ProductCode" :: (ProductCode)
  , "Filter" :: NullOrUndefined.NullOrUndefined (GetEntitlementFilters)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeGetEntitlementsRequest :: Newtype GetEntitlementsRequest _
derive instance repGenericGetEntitlementsRequest :: Generic GetEntitlementsRequest _
instance showGetEntitlementsRequest :: Show GetEntitlementsRequest where
  show = genericShow
instance decodeGetEntitlementsRequest :: Decode GetEntitlementsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEntitlementsRequest :: Encode GetEntitlementsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The GetEntitlementsRequest contains results from the GetEntitlements operation.</p>
newtype GetEntitlementsResult = GetEntitlementsResult 
  { "Entitlements" :: NullOrUndefined.NullOrUndefined (EntitlementList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeGetEntitlementsResult :: Newtype GetEntitlementsResult _
derive instance repGenericGetEntitlementsResult :: Generic GetEntitlementsResult _
instance showGetEntitlementsResult :: Show GetEntitlementsResult where
  show = genericShow
instance decodeGetEntitlementsResult :: Decode GetEntitlementsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEntitlementsResult :: Encode GetEntitlementsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An internal error has occurred. Retry your request. If the problem persists, post a message with details on the AWS forums.</p>
newtype InternalServiceErrorException = InternalServiceErrorException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServiceErrorException :: Newtype InternalServiceErrorException _
derive instance repGenericInternalServiceErrorException :: Generic InternalServiceErrorException _
instance showInternalServiceErrorException :: Show InternalServiceErrorException where
  show = genericShow
instance decodeInternalServiceErrorException :: Decode InternalServiceErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServiceErrorException :: Encode InternalServiceErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more parameters in your request was invalid.</p>
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


-- | <p>The calls to the GetEntitlements API are throttled.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _
derive instance repGenericThrottlingException :: Generic ThrottlingException _
instance showThrottlingException :: Show ThrottlingException where
  show = genericShow
instance decodeThrottlingException :: Decode ThrottlingException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThrottlingException :: Encode ThrottlingException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
