## Module AWS.Pricing

<p>AWS Price List Service API (AWS Price List Service) is a centralized and convenient way to programmatically query Amazon Web Services for services, products, and pricing information. The AWS Price List Service uses standardized product attributes such as <code>Location</code>, <code>Storage Class</code>, and <code>Operating System</code>, and provides prices at the SKU level. You can use the AWS Price List Service to build cost control and scenario planning tools, reconcile billing data, forecast future spend for budgeting purposes, and provide cost benefit analysis that compare your internal workloads with AWS.</p> <p>Use <code>GetServices</code> without a service code to retrieve the service codes for all AWS services, then <code>GetServices</code> with a service code to retreive the attribute names for that service. After you have the service code and attribute names, you can use <code>GetAttributeValues</code> to see what values are available for an attribute. With the service code and an attribute name and value, you can use <code>GetProducts</code> to find specific products that you're interested in, such as an <code>AmazonEC2</code> instance, with a <code>Provisioned IOPS</code> <code>volumeType</code>.</p> <p>Service Endpoint</p> <p>AWS Price List Service API provides the following two endpoints:</p> <ul> <li> <p>https://api.pricing.us-east-1.amazonaws.com</p> </li> <li> <p>https://api.pricing.ap-south-1.amazonaws.com</p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `describeServices`

``` purescript
describeServices :: forall eff. DescribeServicesRequest -> Aff (err :: RequestError | eff) DescribeServicesResponse
```

<p>Returns the metadata for one service or a list of the metadata for all services. Use this without a service code to get the service codes for all services. Use it with a service code, such as <code>AmazonEC2</code>, to get information specific to that service, such as the attribute names available for that service. For example, some of the attribute names available for EC2 are <code>volumeType</code>, <code>maxIopsVolume</code>, <code>operation</code>, <code>locationType</code>, and <code>instanceCapacity10xlarge</code>.</p>

#### `getAttributeValues`

``` purescript
getAttributeValues :: forall eff. GetAttributeValuesRequest -> Aff (err :: RequestError | eff) GetAttributeValuesResponse
```

<p>Returns a list of attribute values. Attibutes are similar to the details in a Price List API offer file. For a list of available attributes, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/reading-an-offer.html#pps-defs">Offer File Definitions</a> in the <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/billing-what-is.html">AWS Billing and Cost Management User Guide</a>.</p>

#### `getProducts`

``` purescript
getProducts :: forall eff. GetProductsRequest -> Aff (err :: RequestError | eff) GetProductsResponse
```

<p>Returns a list of all products that match the filter criteria.</p>

#### `AttributeNameList`

``` purescript
newtype AttributeNameList
  = AttributeNameList (Array String)
```

##### Instances
``` purescript
Newtype AttributeNameList _
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue { "Value" :: NullOrUndefined (String) }
```

<p>The values of a given attribute, such as <code>Throughput Optimized HDD</code> or <code>Provisioned IOPS</code> for the <code>Amazon EC2</code> <code>volumeType</code> attribute.</p>

##### Instances
``` purescript
Newtype AttributeValue _
```

#### `AttributeValueList`

``` purescript
newtype AttributeValueList
  = AttributeValueList (Array AttributeValue)
```

##### Instances
``` purescript
Newtype AttributeValueList _
```

#### `BoxedInteger`

``` purescript
newtype BoxedInteger
  = BoxedInteger Int
```

##### Instances
``` purescript
Newtype BoxedInteger _
```

#### `DescribeServicesRequest`

``` purescript
newtype DescribeServicesRequest
  = DescribeServicesRequest { "ServiceCode" :: NullOrUndefined (String), "FormatVersion" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (BoxedInteger) }
```

##### Instances
``` purescript
Newtype DescribeServicesRequest _
```

#### `DescribeServicesResponse`

``` purescript
newtype DescribeServicesResponse
  = DescribeServicesResponse { "Services" :: NullOrUndefined (ServiceList), "FormatVersion" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeServicesResponse _
```

#### `ExpiredNextTokenException`

``` purescript
newtype ExpiredNextTokenException
  = ExpiredNextTokenException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>The pagination token expired. Try again without a pagination token.</p>

##### Instances
``` purescript
Newtype ExpiredNextTokenException _
```

#### `Filter`

``` purescript
newtype Filter
  = Filter { "Type" :: FilterType, "Field" :: String, "Value" :: String }
```

<p>The constraints that you want all returned products to match.</p>

##### Instances
``` purescript
Newtype Filter _
```

#### `FilterType`

``` purescript
newtype FilterType
  = FilterType String
```

##### Instances
``` purescript
Newtype FilterType _
```

#### `Filters`

``` purescript
newtype Filters
  = Filters (Array Filter)
```

##### Instances
``` purescript
Newtype Filters _
```

#### `GetAttributeValuesRequest`

``` purescript
newtype GetAttributeValuesRequest
  = GetAttributeValuesRequest { "ServiceCode" :: String, "AttributeName" :: String, "NextToken" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (BoxedInteger) }
```

##### Instances
``` purescript
Newtype GetAttributeValuesRequest _
```

#### `GetAttributeValuesResponse`

``` purescript
newtype GetAttributeValuesResponse
  = GetAttributeValuesResponse { "AttributeValues" :: NullOrUndefined (AttributeValueList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetAttributeValuesResponse _
```

#### `GetProductsRequest`

``` purescript
newtype GetProductsRequest
  = GetProductsRequest { "ServiceCode" :: NullOrUndefined (String), "Filters" :: NullOrUndefined (Filters), "FormatVersion" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (BoxedInteger) }
```

##### Instances
``` purescript
Newtype GetProductsRequest _
```

#### `GetProductsResponse`

``` purescript
newtype GetProductsResponse
  = GetProductsResponse { "FormatVersion" :: NullOrUndefined (String), "PriceList" :: NullOrUndefined (PriceList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetProductsResponse _
```

#### `InternalErrorException`

``` purescript
newtype InternalErrorException
  = InternalErrorException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>An error on the server occurred during the processing of your request. Try again later.</p>

##### Instances
``` purescript
Newtype InternalErrorException _
```

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>The pagination token is invalid. Try again without a pagination token.</p>

##### Instances
``` purescript
Newtype InvalidNextTokenException _
```

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>One or more parameters had an invalid value.</p>

##### Instances
``` purescript
Newtype InvalidParameterException _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>The requested resource can't be found.</p>

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `PriceList`

``` purescript
newtype PriceList
  = PriceList (Array PriceListItemJSON)
```

##### Instances
``` purescript
Newtype PriceList _
```

#### `PriceListItemJSON`

``` purescript
newtype PriceListItemJSON
  = PriceListItemJSON String
```

##### Instances
``` purescript
Newtype PriceListItemJSON _
```

#### `Service`

``` purescript
newtype Service
  = Service { "ServiceCode" :: NullOrUndefined (String), "AttributeNames" :: NullOrUndefined (AttributeNameList) }
```

<p>The metadata for a service, such as the service code and available attribute names.</p>

##### Instances
``` purescript
Newtype Service _
```

#### `ServiceList`

``` purescript
newtype ServiceList
  = ServiceList (Array Service)
```

##### Instances
``` purescript
Newtype ServiceList _
```

#### `ErrorMessage'`

``` purescript
newtype ErrorMessage'
  = ErrorMessage' String
```

##### Instances
``` purescript
Newtype ErrorMessage' _
```


