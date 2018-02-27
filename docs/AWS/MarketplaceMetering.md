## Module AWS.MarketplaceMetering

<fullname>AWS Marketplace Metering Service</fullname> <p>This reference provides descriptions of the low-level AWS Marketplace Metering Service API.</p> <p>AWS Marketplace sellers can use this API to submit usage data for custom usage dimensions.</p> <p> <b>Submitting Metering Records</b> </p> <ul> <li> <p> <i>MeterUsage</i>- Submits the metering record for a Marketplace product. MeterUsage is called from an EC2 instance.</p> </li> <li> <p> <i>BatchMeterUsage</i>- Submits the metering record for a set of customers. BatchMeterUsage is called from a software-as-a-service (SaaS) application.</p> </li> </ul> <p> <b>Accepting New Customers</b> </p> <ul> <li> <p> <i>ResolveCustomer</i>- Called by a SaaS application during the registration process. When a buyer visits your website during the registration process, the buyer submits a Registration Token through the browser. The Registration Token is resolved through this API to obtain a CustomerIdentifier and Product Code.</p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `batchMeterUsage`

``` purescript
batchMeterUsage :: forall eff. BatchMeterUsageRequest -> Aff (err :: RequestError | eff) BatchMeterUsageResult
```

<p>BatchMeterUsage is called from a SaaS application listed on the AWS Marketplace to post metering records for a set of customers.</p> <p>For identical requests, the API is idempotent; requests can be retried with the same records or a subset of the input records.</p> <p>Every request to BatchMeterUsage is for one product. If you need to meter usage for multiple products, you must make multiple calls to BatchMeterUsage.</p> <p>BatchMeterUsage can process up to 25 UsageRecords at a time.</p>

#### `meterUsage`

``` purescript
meterUsage :: forall eff. MeterUsageRequest -> Aff (err :: RequestError | eff) MeterUsageResult
```

<p>API to emit metering records. For identical requests, the API is idempotent. It simply returns the metering record ID.</p> <p>MeterUsage is authenticated on the buyer's AWS account, generally when running from an EC2 instance on the AWS Marketplace.</p>

#### `resolveCustomer`

``` purescript
resolveCustomer :: forall eff. ResolveCustomerRequest -> Aff (err :: RequestError | eff) ResolveCustomerResult
```

<p>ResolveCustomer is called by a SaaS application during the registration process. When a buyer visits your website during the registration process, the buyer submits a registration token through their browser. The registration token is resolved through this API to obtain a CustomerIdentifier and product code.</p>

#### `BatchMeterUsageRequest`

``` purescript
newtype BatchMeterUsageRequest
  = BatchMeterUsageRequest { "UsageRecords" :: UsageRecordList, "ProductCode" :: ProductCode }
```

<p>A BatchMeterUsageRequest contains UsageRecords, which indicate quantities of usage within your application.</p>

##### Instances
``` purescript
Newtype BatchMeterUsageRequest _
```

#### `BatchMeterUsageResult`

``` purescript
newtype BatchMeterUsageResult
  = BatchMeterUsageResult { "Results" :: NullOrUndefined (UsageRecordResultList), "UnprocessedRecords" :: NullOrUndefined (UsageRecordList) }
```

<p>Contains the UsageRecords processed by BatchMeterUsage and any records that have failed due to transient error.</p>

##### Instances
``` purescript
Newtype BatchMeterUsageResult _
```

#### `CustomerIdentifier`

``` purescript
newtype CustomerIdentifier
  = CustomerIdentifier String
```

##### Instances
``` purescript
Newtype CustomerIdentifier _
```

#### `DuplicateRequestException`

``` purescript
newtype DuplicateRequestException
  = DuplicateRequestException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>A metering record has already been emitted by the same EC2 instance for the given {usageDimension, timestamp} with a different usageQuantity.</p>

##### Instances
``` purescript
Newtype DuplicateRequestException _
```

#### `ExpiredTokenException`

``` purescript
newtype ExpiredTokenException
  = ExpiredTokenException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The submitted registration token has expired. This can happen if the buyer's browser takes too long to redirect to your page, the buyer has resubmitted the registration token, or your application has held on to the registration token for too long. Your SaaS registration website should redeem this token as soon as it is submitted by the buyer's browser.</p>

##### Instances
``` purescript
Newtype ExpiredTokenException _
```

#### `InternalServiceErrorException`

``` purescript
newtype InternalServiceErrorException
  = InternalServiceErrorException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>An internal error has occurred. Retry your request. If the problem persists, post a message with details on the AWS forums.</p>

##### Instances
``` purescript
Newtype InternalServiceErrorException _
```

#### `InvalidCustomerIdentifierException`

``` purescript
newtype InvalidCustomerIdentifierException
  = InvalidCustomerIdentifierException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>You have metered usage for a CustomerIdentifier that does not exist.</p>

##### Instances
``` purescript
Newtype InvalidCustomerIdentifierException _
```

#### `InvalidEndpointRegionException`

``` purescript
newtype InvalidEndpointRegionException
  = InvalidEndpointRegionException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The endpoint being called is in a region different from your EC2 instance. The region of the Metering service endpoint and the region of the EC2 instance must match.</p>

##### Instances
``` purescript
Newtype InvalidEndpointRegionException _
```

#### `InvalidProductCodeException`

``` purescript
newtype InvalidProductCodeException
  = InvalidProductCodeException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The product code passed does not match the product code used for publishing the product.</p>

##### Instances
``` purescript
Newtype InvalidProductCodeException _
```

#### `InvalidTokenException`

``` purescript
newtype InvalidTokenException
  = InvalidTokenException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

##### Instances
``` purescript
Newtype InvalidTokenException _
```

#### `InvalidUsageDimensionException`

``` purescript
newtype InvalidUsageDimensionException
  = InvalidUsageDimensionException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The usage dimension does not match one of the UsageDimensions associated with products.</p>

##### Instances
``` purescript
Newtype InvalidUsageDimensionException _
```

#### `MeterUsageRequest`

``` purescript
newtype MeterUsageRequest
  = MeterUsageRequest { "ProductCode" :: ProductCode, "Number" :: Number, "UsageDimension" :: UsageDimension, "UsageQuantity" :: UsageQuantity, "DryRun" :: Boolean }
```

##### Instances
``` purescript
Newtype MeterUsageRequest _
```

#### `MeterUsageResult`

``` purescript
newtype MeterUsageResult
  = MeterUsageResult { "MeteringRecordId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype MeterUsageResult _
```

#### `NonEmptyString`

``` purescript
newtype NonEmptyString
  = NonEmptyString String
```

##### Instances
``` purescript
Newtype NonEmptyString _
```

#### `ProductCode`

``` purescript
newtype ProductCode
  = ProductCode String
```

##### Instances
``` purescript
Newtype ProductCode _
```

#### `ResolveCustomerRequest`

``` purescript
newtype ResolveCustomerRequest
  = ResolveCustomerRequest { "RegistrationToken" :: NonEmptyString }
```

<p>Contains input to the ResolveCustomer operation.</p>

##### Instances
``` purescript
Newtype ResolveCustomerRequest _
```

#### `ResolveCustomerResult`

``` purescript
newtype ResolveCustomerResult
  = ResolveCustomerResult { "CustomerIdentifier" :: NullOrUndefined (CustomerIdentifier), "ProductCode" :: NullOrUndefined (ProductCode) }
```

<p>The result of the ResolveCustomer operation. Contains the CustomerIdentifier and product code.</p>

##### Instances
``` purescript
Newtype ResolveCustomerResult _
```

#### `ThrottlingException`

``` purescript
newtype ThrottlingException
  = ThrottlingException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The calls to the MeterUsage API are throttled.</p>

##### Instances
``` purescript
Newtype ThrottlingException _
```

#### `TimestampOutOfBoundsException`

``` purescript
newtype TimestampOutOfBoundsException
  = TimestampOutOfBoundsException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The timestamp value passed in the meterUsage() is out of allowed range.</p>

##### Instances
``` purescript
Newtype TimestampOutOfBoundsException _
```

#### `UsageDimension`

``` purescript
newtype UsageDimension
  = UsageDimension String
```

##### Instances
``` purescript
Newtype UsageDimension _
```

#### `UsageQuantity`

``` purescript
newtype UsageQuantity
  = UsageQuantity Int
```

##### Instances
``` purescript
Newtype UsageQuantity _
```

#### `UsageRecord`

``` purescript
newtype UsageRecord
  = UsageRecord { "Number" :: Number, "CustomerIdentifier" :: CustomerIdentifier, "Dimension" :: UsageDimension, "Quantity" :: UsageQuantity }
```

<p>A UsageRecord indicates a quantity of usage for a given product, customer, dimension and time.</p> <p>Multiple requests with the same UsageRecords as input will be deduplicated to prevent double charges.</p>

##### Instances
``` purescript
Newtype UsageRecord _
```

#### `UsageRecordList`

``` purescript
newtype UsageRecordList
  = UsageRecordList (Array UsageRecord)
```

##### Instances
``` purescript
Newtype UsageRecordList _
```

#### `UsageRecordResult`

``` purescript
newtype UsageRecordResult
  = UsageRecordResult { "UsageRecord" :: NullOrUndefined (UsageRecord), "MeteringRecordId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (UsageRecordResultStatus) }
```

<p>A UsageRecordResult indicates the status of a given UsageRecord processed by BatchMeterUsage.</p>

##### Instances
``` purescript
Newtype UsageRecordResult _
```

#### `UsageRecordResultList`

``` purescript
newtype UsageRecordResultList
  = UsageRecordResultList (Array UsageRecordResult)
```

##### Instances
``` purescript
Newtype UsageRecordResultList _
```

#### `UsageRecordResultStatus`

``` purescript
newtype UsageRecordResultStatus
  = UsageRecordResultStatus String
```

##### Instances
``` purescript
Newtype UsageRecordResultStatus _
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


