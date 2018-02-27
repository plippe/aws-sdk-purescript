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

##### Instances
``` purescript
Newtype Entitlement _
```

#### `EntitlementList`

``` purescript
newtype EntitlementList
  = EntitlementList (Array Entitlement)
```

##### Instances
``` purescript
Newtype EntitlementList _
```

#### `EntitlementValue`

``` purescript
newtype EntitlementValue
  = EntitlementValue { "IntegerValue" :: NullOrUndefined (Int), "DoubleValue" :: NullOrUndefined (Number), "BooleanValue" :: NullOrUndefined (Boolean), "StringValue" :: NullOrUndefined (String) }
```

<p>The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.</p>

##### Instances
``` purescript
Newtype EntitlementValue _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `FilterValue`

``` purescript
newtype FilterValue
  = FilterValue String
```

##### Instances
``` purescript
Newtype FilterValue _
```

#### `FilterValueList`

``` purescript
newtype FilterValueList
  = FilterValueList (Array FilterValue)
```

##### Instances
``` purescript
Newtype FilterValueList _
```

#### `GetEntitlementFilterName`

``` purescript
newtype GetEntitlementFilterName
  = GetEntitlementFilterName String
```

##### Instances
``` purescript
Newtype GetEntitlementFilterName _
```

#### `GetEntitlementFilters`

``` purescript
newtype GetEntitlementFilters
  = GetEntitlementFilters (Map GetEntitlementFilterName FilterValueList)
```

##### Instances
``` purescript
Newtype GetEntitlementFilters _
```

#### `GetEntitlementsRequest`

``` purescript
newtype GetEntitlementsRequest
  = GetEntitlementsRequest { "ProductCode" :: ProductCode, "Filter" :: NullOrUndefined (GetEntitlementFilters), "NextToken" :: NullOrUndefined (NonEmptyString), "MaxResults" :: NullOrUndefined (Int) }
```

<p>The GetEntitlementsRequest contains parameters for the GetEntitlements operation.</p>

##### Instances
``` purescript
Newtype GetEntitlementsRequest _
```

#### `GetEntitlementsResult`

``` purescript
newtype GetEntitlementsResult
  = GetEntitlementsResult { "Entitlements" :: NullOrUndefined (EntitlementList), "NextToken" :: NullOrUndefined (NonEmptyString) }
```

<p>The GetEntitlementsRequest contains results from the GetEntitlements operation.</p>

##### Instances
``` purescript
Newtype GetEntitlementsResult _
```

#### `InternalServiceErrorException`

``` purescript
newtype InternalServiceErrorException
  = InternalServiceErrorException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>An internal error has occurred. Retry your request. If the problem persists, post a message with details on the AWS forums.</p>

##### Instances
``` purescript
Newtype InternalServiceErrorException _
```

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>One or more parameters in your request was invalid.</p>

##### Instances
``` purescript
Newtype InvalidParameterException _
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

#### `ThrottlingException`

``` purescript
newtype ThrottlingException
  = ThrottlingException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The calls to the GetEntitlements API are throttled.</p>

##### Instances
``` purescript
Newtype ThrottlingException _
```


