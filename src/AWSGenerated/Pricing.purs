

-- | <p>AWS Price List Service API (AWS Price List Service) is a centralized and convenient way to programmatically query Amazon Web Services for services, products, and pricing information. The AWS Price List Service uses standardized product attributes such as <code>Location</code>, <code>Storage Class</code>, and <code>Operating System</code>, and provides prices at the SKU level. You can use the AWS Price List Service to build cost control and scenario planning tools, reconcile billing data, forecast future spend for budgeting purposes, and provide cost benefit analysis that compare your internal workloads with AWS.</p> <p>Use <code>GetServices</code> without a service code to retrieve the service codes for all AWS services, then <code>GetServices</code> with a service code to retreive the attribute names for that service. After you have the service code and attribute names, you can use <code>GetAttributeValues</code> to see what values are available for an attribute. With the service code and an attribute name and value, you can use <code>GetProducts</code> to find specific products that you're interested in, such as an <code>AmazonEC2</code> instance, with a <code>Provisioned IOPS</code> <code>volumeType</code>.</p> <p>Service Endpoint</p> <p>AWS Price List Service API provides the following two endpoints:</p> <ul> <li> <p>https://api.pricing.us-east-1.amazonaws.com</p> </li> <li> <p>https://api.pricing.ap-south-1.amazonaws.com</p> </li> </ul>
module AWS.Pricing where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Pricing" :: String


-- | <p>Returns the metadata for one service or a list of the metadata for all services. Use this without a service code to get the service codes for all services. Use it with a service code, such as <code>AmazonEC2</code>, to get information specific to that service, such as the attribute names available for that service. For example, some of the attribute names available for EC2 are <code>volumeType</code>, <code>maxIopsVolume</code>, <code>operation</code>, <code>locationType</code>, and <code>instanceCapacity10xlarge</code>.</p>
describeServices :: forall eff. DescribeServicesRequest -> Aff (err :: AWS.RequestError | eff) DescribeServicesResponse
describeServices = AWS.request serviceName "DescribeServices" 


-- | <p>Returns a list of attribute values. Attibutes are similar to the details in a Price List API offer file. For a list of available attributes, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/reading-an-offer.html#pps-defs">Offer File Definitions</a> in the <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/billing-what-is.html">AWS Billing and Cost Management User Guide</a>.</p>
getAttributeValues :: forall eff. GetAttributeValuesRequest -> Aff (err :: AWS.RequestError | eff) GetAttributeValuesResponse
getAttributeValues = AWS.request serviceName "GetAttributeValues" 


-- | <p>Returns a list of all products that match the filter criteria.</p>
getProducts :: forall eff. GetProductsRequest -> Aff (err :: AWS.RequestError | eff) GetProductsResponse
getProducts = AWS.request serviceName "GetProducts" 


newtype AttributeNameList = AttributeNameList (Array String)
derive instance newtypeAttributeNameList :: Newtype AttributeNameList _


-- | <p>The values of a given attribute, such as <code>Throughput Optimized HDD</code> or <code>Provisioned IOPS</code> for the <code>Amazon EC2</code> <code>volumeType</code> attribute.</p>
newtype AttributeValue = AttributeValue 
  { "Value" :: NullOrUndefined (String)
  }
derive instance newtypeAttributeValue :: Newtype AttributeValue _


newtype AttributeValueList = AttributeValueList (Array AttributeValue)
derive instance newtypeAttributeValueList :: Newtype AttributeValueList _


newtype BoxedInteger = BoxedInteger Int
derive instance newtypeBoxedInteger :: Newtype BoxedInteger _


newtype DescribeServicesRequest = DescribeServicesRequest 
  { "ServiceCode" :: NullOrUndefined (String)
  , "FormatVersion" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeDescribeServicesRequest :: Newtype DescribeServicesRequest _


newtype DescribeServicesResponse = DescribeServicesResponse 
  { "Services" :: NullOrUndefined (ServiceList)
  , "FormatVersion" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeServicesResponse :: Newtype DescribeServicesResponse _


-- | <p>The pagination token expired. Try again without a pagination token.</p>
newtype ExpiredNextTokenException = ExpiredNextTokenException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeExpiredNextTokenException :: Newtype ExpiredNextTokenException _


-- | <p>The constraints that you want all returned products to match.</p>
newtype Filter = Filter 
  { "Type" :: (FilterType)
  , "Field" :: (String)
  , "Value" :: (String)
  }
derive instance newtypeFilter :: Newtype Filter _


newtype FilterType = FilterType String
derive instance newtypeFilterType :: Newtype FilterType _


newtype Filters = Filters (Array Filter)
derive instance newtypeFilters :: Newtype Filters _


newtype GetAttributeValuesRequest = GetAttributeValuesRequest 
  { "ServiceCode" :: (String)
  , "AttributeName" :: (String)
  , "NextToken" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeGetAttributeValuesRequest :: Newtype GetAttributeValuesRequest _


newtype GetAttributeValuesResponse = GetAttributeValuesResponse 
  { "AttributeValues" :: NullOrUndefined (AttributeValueList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeGetAttributeValuesResponse :: Newtype GetAttributeValuesResponse _


newtype GetProductsRequest = GetProductsRequest 
  { "ServiceCode" :: NullOrUndefined (String)
  , "Filters" :: NullOrUndefined (Filters)
  , "FormatVersion" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeGetProductsRequest :: Newtype GetProductsRequest _


newtype GetProductsResponse = GetProductsResponse 
  { "FormatVersion" :: NullOrUndefined (String)
  , "PriceList" :: NullOrUndefined (PriceList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeGetProductsResponse :: Newtype GetProductsResponse _


-- | <p>An error on the server occurred during the processing of your request. Try again later.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInternalErrorException :: Newtype InternalErrorException _


-- | <p>The pagination token is invalid. Try again without a pagination token.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _


-- | <p>One or more parameters had an invalid value.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _


-- | <p>The requested resource can't be found.</p>
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


newtype PriceList = PriceList (Array PriceListItemJSON)
derive instance newtypePriceList :: Newtype PriceList _


newtype PriceListItemJSON = PriceListItemJSON String
derive instance newtypePriceListItemJSON :: Newtype PriceListItemJSON _


-- | <p>The metadata for a service, such as the service code and available attribute names.</p>
newtype Service = Service 
  { "ServiceCode" :: NullOrUndefined (String)
  , "AttributeNames" :: NullOrUndefined (AttributeNameList)
  }
derive instance newtypeService :: Newtype Service _


newtype ServiceList = ServiceList (Array Service)
derive instance newtypeServiceList :: Newtype ServiceList _


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
