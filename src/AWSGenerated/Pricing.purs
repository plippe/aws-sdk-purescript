

-- | <p>AWS Price List Service API (AWS Price List Service) is a centralized and convenient way to programmatically query Amazon Web Services for services, products, and pricing information. The AWS Price List Service uses standardized product attributes such as <code>Location</code>, <code>Storage Class</code>, and <code>Operating System</code>, and provides prices at the SKU level. You can use the AWS Price List Service to build cost control and scenario planning tools, reconcile billing data, forecast future spend for budgeting purposes, and provide cost benefit analysis that compare your internal workloads with AWS.</p> <p>Use <code>GetServices</code> without a service code to retrieve the service codes for all AWS services, then <code>GetServices</code> with a service code to retreive the attribute names for that service. After you have the service code and attribute names, you can use <code>GetAttributeValues</code> to see what values are available for an attribute. With the service code and an attribute name and value, you can use <code>GetProducts</code> to find specific products that you're interested in, such as an <code>AmazonEC2</code> instance, with a <code>Provisioned IOPS</code> <code>volumeType</code>.</p> <p>Service Endpoint</p> <p>AWS Price List Service API provides the following two endpoints:</p> <ul> <li> <p>https://api.pricing.us-east-1.amazonaws.com</p> </li> <li> <p>https://api.pricing.ap-south-1.amazonaws.com</p> </li> </ul>
module AWS.Pricing where

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

serviceName = "Pricing" :: String


-- | <p>Returns the metadata for one service or a list of the metadata for all services. Use this without a service code to get the service codes for all services. Use it with a service code, such as <code>AmazonEC2</code>, to get information specific to that service, such as the attribute names available for that service. For example, some of the attribute names available for EC2 are <code>volumeType</code>, <code>maxIopsVolume</code>, <code>operation</code>, <code>locationType</code>, and <code>instanceCapacity10xlarge</code>.</p>
describeServices :: forall eff. DescribeServicesRequest -> Aff (exception :: EXCEPTION | eff) DescribeServicesResponse
describeServices = Request.request serviceName "describeServices" 


-- | <p>Returns a list of attribute values. Attibutes are similar to the details in a Price List API offer file. For a list of available attributes, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/reading-an-offer.html#pps-defs">Offer File Definitions</a> in the <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/billing-what-is.html">AWS Billing and Cost Management User Guide</a>.</p>
getAttributeValues :: forall eff. GetAttributeValuesRequest -> Aff (exception :: EXCEPTION | eff) GetAttributeValuesResponse
getAttributeValues = Request.request serviceName "getAttributeValues" 


-- | <p>Returns a list of all products that match the filter criteria.</p>
getProducts :: forall eff. GetProductsRequest -> Aff (exception :: EXCEPTION | eff) GetProductsResponse
getProducts = Request.request serviceName "getProducts" 


newtype AttributeNameList = AttributeNameList (Array String)
derive instance newtypeAttributeNameList :: Newtype AttributeNameList _
derive instance repGenericAttributeNameList :: Generic AttributeNameList _
instance showAttributeNameList :: Show AttributeNameList where
  show = genericShow
instance decodeAttributeNameList :: Decode AttributeNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeNameList :: Encode AttributeNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The values of a given attribute, such as <code>Throughput Optimized HDD</code> or <code>Provisioned IOPS</code> for the <code>Amazon EC2</code> <code>volumeType</code> attribute.</p>
newtype AttributeValue = AttributeValue 
  { "Value" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAttributeValue :: Newtype AttributeValue _
derive instance repGenericAttributeValue :: Generic AttributeValue _
instance showAttributeValue :: Show AttributeValue where
  show = genericShow
instance decodeAttributeValue :: Decode AttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValue :: Encode AttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeValueList = AttributeValueList (Array AttributeValue)
derive instance newtypeAttributeValueList :: Newtype AttributeValueList _
derive instance repGenericAttributeValueList :: Generic AttributeValueList _
instance showAttributeValueList :: Show AttributeValueList where
  show = genericShow
instance decodeAttributeValueList :: Decode AttributeValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValueList :: Encode AttributeValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BoxedInteger = BoxedInteger Int
derive instance newtypeBoxedInteger :: Newtype BoxedInteger _
derive instance repGenericBoxedInteger :: Generic BoxedInteger _
instance showBoxedInteger :: Show BoxedInteger where
  show = genericShow
instance decodeBoxedInteger :: Decode BoxedInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBoxedInteger :: Encode BoxedInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeServicesRequest = DescribeServicesRequest 
  { "ServiceCode" :: NullOrUndefined.NullOrUndefined (String)
  , "FormatVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeDescribeServicesRequest :: Newtype DescribeServicesRequest _
derive instance repGenericDescribeServicesRequest :: Generic DescribeServicesRequest _
instance showDescribeServicesRequest :: Show DescribeServicesRequest where
  show = genericShow
instance decodeDescribeServicesRequest :: Decode DescribeServicesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeServicesRequest :: Encode DescribeServicesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeServicesResponse = DescribeServicesResponse 
  { "Services" :: NullOrUndefined.NullOrUndefined (ServiceList)
  , "FormatVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeServicesResponse :: Newtype DescribeServicesResponse _
derive instance repGenericDescribeServicesResponse :: Generic DescribeServicesResponse _
instance showDescribeServicesResponse :: Show DescribeServicesResponse where
  show = genericShow
instance decodeDescribeServicesResponse :: Decode DescribeServicesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeServicesResponse :: Encode DescribeServicesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pagination token expired. Try again without a pagination token.</p>
newtype ExpiredNextTokenException = ExpiredNextTokenException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeExpiredNextTokenException :: Newtype ExpiredNextTokenException _
derive instance repGenericExpiredNextTokenException :: Generic ExpiredNextTokenException _
instance showExpiredNextTokenException :: Show ExpiredNextTokenException where
  show = genericShow
instance decodeExpiredNextTokenException :: Decode ExpiredNextTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpiredNextTokenException :: Encode ExpiredNextTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The constraints that you want all returned products to match.</p>
newtype Filter = Filter 
  { "Type" :: (FilterType)
  , "Field" :: (String)
  , "Value" :: (String)
  }
derive instance newtypeFilter :: Newtype Filter _
derive instance repGenericFilter :: Generic Filter _
instance showFilter :: Show Filter where
  show = genericShow
instance decodeFilter :: Decode Filter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilter :: Encode Filter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterType = FilterType String
derive instance newtypeFilterType :: Newtype FilterType _
derive instance repGenericFilterType :: Generic FilterType _
instance showFilterType :: Show FilterType where
  show = genericShow
instance decodeFilterType :: Decode FilterType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterType :: Encode FilterType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Filters = Filters (Array Filter)
derive instance newtypeFilters :: Newtype Filters _
derive instance repGenericFilters :: Generic Filters _
instance showFilters :: Show Filters where
  show = genericShow
instance decodeFilters :: Decode Filters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilters :: Encode Filters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAttributeValuesRequest = GetAttributeValuesRequest 
  { "ServiceCode" :: (String)
  , "AttributeName" :: (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeGetAttributeValuesRequest :: Newtype GetAttributeValuesRequest _
derive instance repGenericGetAttributeValuesRequest :: Generic GetAttributeValuesRequest _
instance showGetAttributeValuesRequest :: Show GetAttributeValuesRequest where
  show = genericShow
instance decodeGetAttributeValuesRequest :: Decode GetAttributeValuesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAttributeValuesRequest :: Encode GetAttributeValuesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAttributeValuesResponse = GetAttributeValuesResponse 
  { "AttributeValues" :: NullOrUndefined.NullOrUndefined (AttributeValueList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetAttributeValuesResponse :: Newtype GetAttributeValuesResponse _
derive instance repGenericGetAttributeValuesResponse :: Generic GetAttributeValuesResponse _
instance showGetAttributeValuesResponse :: Show GetAttributeValuesResponse where
  show = genericShow
instance decodeGetAttributeValuesResponse :: Decode GetAttributeValuesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAttributeValuesResponse :: Encode GetAttributeValuesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetProductsRequest = GetProductsRequest 
  { "ServiceCode" :: NullOrUndefined.NullOrUndefined (String)
  , "Filters" :: NullOrUndefined.NullOrUndefined (Filters)
  , "FormatVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeGetProductsRequest :: Newtype GetProductsRequest _
derive instance repGenericGetProductsRequest :: Generic GetProductsRequest _
instance showGetProductsRequest :: Show GetProductsRequest where
  show = genericShow
instance decodeGetProductsRequest :: Decode GetProductsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetProductsRequest :: Encode GetProductsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetProductsResponse = GetProductsResponse 
  { "FormatVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "PriceList" :: NullOrUndefined.NullOrUndefined (PriceList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetProductsResponse :: Newtype GetProductsResponse _
derive instance repGenericGetProductsResponse :: Generic GetProductsResponse _
instance showGetProductsResponse :: Show GetProductsResponse where
  show = genericShow
instance decodeGetProductsResponse :: Decode GetProductsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetProductsResponse :: Encode GetProductsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error on the server occurred during the processing of your request. Try again later.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInternalErrorException :: Newtype InternalErrorException _
derive instance repGenericInternalErrorException :: Generic InternalErrorException _
instance showInternalErrorException :: Show InternalErrorException where
  show = genericShow
instance decodeInternalErrorException :: Decode InternalErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalErrorException :: Encode InternalErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pagination token is invalid. Try again without a pagination token.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _
derive instance repGenericInvalidNextTokenException :: Generic InvalidNextTokenException _
instance showInvalidNextTokenException :: Show InvalidNextTokenException where
  show = genericShow
instance decodeInvalidNextTokenException :: Decode InvalidNextTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNextTokenException :: Encode InvalidNextTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more parameters had an invalid value.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested resource can't be found.</p>
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PriceList = PriceList (Array PriceListItemJSON)
derive instance newtypePriceList :: Newtype PriceList _
derive instance repGenericPriceList :: Generic PriceList _
instance showPriceList :: Show PriceList where
  show = genericShow
instance decodePriceList :: Decode PriceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePriceList :: Encode PriceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PriceListItemJSON = PriceListItemJSON String
derive instance newtypePriceListItemJSON :: Newtype PriceListItemJSON _
derive instance repGenericPriceListItemJSON :: Generic PriceListItemJSON _
instance showPriceListItemJSON :: Show PriceListItemJSON where
  show = genericShow
instance decodePriceListItemJSON :: Decode PriceListItemJSON where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePriceListItemJSON :: Encode PriceListItemJSON where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The metadata for a service, such as the service code and available attribute names.</p>
newtype Service = Service 
  { "ServiceCode" :: NullOrUndefined.NullOrUndefined (String)
  , "AttributeNames" :: NullOrUndefined.NullOrUndefined (AttributeNameList)
  }
derive instance newtypeService :: Newtype Service _
derive instance repGenericService :: Generic Service _
instance showService :: Show Service where
  show = genericShow
instance decodeService :: Decode Service where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeService :: Encode Service where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServiceList = ServiceList (Array Service)
derive instance newtypeServiceList :: Newtype ServiceList _
derive instance repGenericServiceList :: Generic ServiceList _
instance showServiceList :: Show ServiceList where
  show = genericShow
instance decodeServiceList :: Decode ServiceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceList :: Encode ServiceList where
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
