## Module AWS.MarketplaceEntitlementService

<fullname>AWS Marketplace Entitlement Service</fullname> <p>This reference provides descriptions of the AWS Marketplace Entitlement Service API.</p> <p>AWS Marketplace Entitlement Service is used to determine the entitlement of a customer to a given product. An entitlement represents capacity in a product owned by the customer. For example, a customer might own some number of users or seats in an SaaS application or some amount of data capacity in a multi-tenant database.</p> <p> <b>Getting Entitlement Records</b> </p> <ul> <li> <p> <i>GetEntitlements</i>- Gets the entitlements for a Marketplace product.</p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `getEntitlements`

``` purescript
getEntitlements :: forall eff. GetEntitlementsRequest -> Aff (err :: RequestError | eff) GetEntitlementsResult
```

<p>GetEntitlements retrieves entitlement values for a given product. The results can be filtered based on customer identifier or product dimensions.</p>

#### `Entitlement`

``` purescript
newtype Entitlement
  = Entitlement { "ProductCode" :: NullOrUndefined (ProductCode), "Dimension" :: NullOrUndefined (NonEmptyString), "CustomerIdentifier" :: NullOrUndefined (NonEmptyString), "Value" :: NullOrUndefined (EntitlementValue), "ExpirationDate" :: NullOrUndefined (Number) }
```

<p>An entitlement represents capacity in a product owned by the customer. For example, a customer might own some number of users or seats in an SaaS application or some amount of data capacity in a multi-tenant database.</p>

#### `EntitlementList`

``` purescript
newtype EntitlementList
  = EntitlementList (Array Entitlement)
```

#### `EntitlementValue`

``` purescript
newtype EntitlementValue
  = EntitlementValue { "IntegerValue" :: NullOrUndefined (Int), "DoubleValue" :: NullOrUndefined (Number), "BooleanValue" :: NullOrUndefined (Boolean), "StringValue" :: NullOrUndefined (String) }
```

<p>The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.</p>

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `FilterValue`

``` purescript
newtype FilterValue
  = FilterValue String
```

#### `FilterValueList`

``` purescript
newtype FilterValueList
  = FilterValueList (Array FilterValue)
```

#### `GetEntitlementFilterName`

``` purescript
newtype GetEntitlementFilterName
  = GetEntitlementFilterName String
```

#### `GetEntitlementFilters`

``` purescript
newtype GetEntitlementFilters
  = GetEntitlementFilters (Map GetEntitlementFilterName FilterValueList)
```

#### `GetEntitlementsRequest`

``` purescript
newtype GetEntitlementsRequest
  = GetEntitlementsRequest { "ProductCode" :: ProductCode, "Filter" :: NullOrUndefined (GetEntitlementFilters), "NextToken" :: NullOrUndefined (NonEmptyString), "MaxResults" :: NullOrUndefined (Int) }
```

<p>The GetEntitlementsRequest contains parameters for the GetEntitlements operation.</p>

#### `GetEntitlementsResult`

``` purescript
newtype GetEntitlementsResult
  = GetEntitlementsResult { "Entitlements" :: NullOrUndefined (EntitlementList), "NextToken" :: NullOrUndefined (NonEmptyString) }
```

<p>The GetEntitlementsRequest contains results from the GetEntitlements operation.</p>

#### `InternalServiceErrorException`

``` purescript
newtype InternalServiceErrorException
  = InternalServiceErrorException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>An internal error has occurred. Retry your request. If the problem persists, post a message with details on the AWS forums.</p>

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>One or more parameters in your request was invalid.</p>

#### `NonEmptyString`

``` purescript
newtype NonEmptyString
  = NonEmptyString String
```

#### `ProductCode`

``` purescript
newtype ProductCode
  = ProductCode String
```

#### `ThrottlingException`

``` purescript
newtype ThrottlingException
  = ThrottlingException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The calls to the GetEntitlements API are throttled.</p>


