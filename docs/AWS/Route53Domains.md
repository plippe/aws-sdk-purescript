## Module AWS.Route53Domains

<p>Amazon Route 53 API actions let you register domain names and perform related operations.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `checkDomainAvailability`

``` purescript
checkDomainAvailability :: forall eff. CheckDomainAvailabilityRequest -> Aff (err :: RequestError | eff) CheckDomainAvailabilityResponse
```

<p>This operation checks the availability of one domain name. Note that if the availability status of a domain is pending, you must submit another request to determine the availability of the domain name.</p>

#### `checkDomainTransferability`

``` purescript
checkDomainTransferability :: forall eff. CheckDomainTransferabilityRequest -> Aff (err :: RequestError | eff) CheckDomainTransferabilityResponse
```

<p>Checks whether a domain name can be transferred to Amazon Route 53. </p>

#### `deleteTagsForDomain`

``` purescript
deleteTagsForDomain :: forall eff. DeleteTagsForDomainRequest -> Aff (err :: RequestError | eff) DeleteTagsForDomainResponse
```

<p>This operation deletes the specified tags for a domain.</p> <p>All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.</p>

#### `disableDomainAutoRenew`

``` purescript
disableDomainAutoRenew :: forall eff. DisableDomainAutoRenewRequest -> Aff (err :: RequestError | eff) DisableDomainAutoRenewResponse
```

<p>This operation disables automatic renewal of domain registration for the specified domain.</p>

#### `disableDomainTransferLock`

``` purescript
disableDomainTransferLock :: forall eff. DisableDomainTransferLockRequest -> Aff (err :: RequestError | eff) DisableDomainTransferLockResponse
```

<p>This operation removes the transfer lock on the domain (specifically the <code>clientTransferProhibited</code> status) to allow domain transfers. We recommend you refrain from performing this action unless you intend to transfer the domain to a different registrar. Successful submission returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>

#### `enableDomainAutoRenew`

``` purescript
enableDomainAutoRenew :: forall eff. EnableDomainAutoRenewRequest -> Aff (err :: RequestError | eff) EnableDomainAutoRenewResponse
```

<p>This operation configures Amazon Route 53 to automatically renew the specified domain before the domain registration expires. The cost of renewing your domain registration is billed to your AWS account.</p> <p>The period during which you can renew a domain name varies by TLD. For a list of TLDs and their renewal policies, see <a href="http://wiki.gandi.net/en/domains/renew#renewal_restoration_and_deletion_times">"Renewal, restoration, and deletion times"</a> on the website for our registrar partner, Gandi. Route 53 requires that you renew before the end of the renewal period that is listed on the Gandi website so we can complete processing before the deadline.</p>

#### `enableDomainTransferLock`

``` purescript
enableDomainTransferLock :: forall eff. EnableDomainTransferLockRequest -> Aff (err :: RequestError | eff) EnableDomainTransferLockResponse
```

<p>This operation sets the transfer lock on the domain (specifically the <code>clientTransferProhibited</code> status) to prevent domain transfers. Successful submission returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>

#### `getContactReachabilityStatus`

``` purescript
getContactReachabilityStatus :: forall eff. GetContactReachabilityStatusRequest -> Aff (err :: RequestError | eff) GetContactReachabilityStatusResponse
```

<p>For operations that require confirmation that the email address for the registrant contact is valid, such as registering a new domain, this operation returns information about whether the registrant contact has responded.</p> <p>If you want us to resend the email, use the <code>ResendContactReachabilityEmail</code> operation.</p>

#### `getDomainDetail`

``` purescript
getDomainDetail :: forall eff. GetDomainDetailRequest -> Aff (err :: RequestError | eff) GetDomainDetailResponse
```

<p>This operation returns detailed information about a specified domain that is associated with the current AWS account. Contact information for the domain is also returned as part of the output.</p>

#### `getDomainSuggestions`

``` purescript
getDomainSuggestions :: forall eff. GetDomainSuggestionsRequest -> Aff (err :: RequestError | eff) GetDomainSuggestionsResponse
```

<p>The GetDomainSuggestions operation returns a list of suggested domain names given a string, which can either be a domain name or simply a word or phrase (without spaces).</p>

#### `getOperationDetail`

``` purescript
getOperationDetail :: forall eff. GetOperationDetailRequest -> Aff (err :: RequestError | eff) GetOperationDetailResponse
```

<p>This operation returns the current status of an operation that is not completed.</p>

#### `listDomains`

``` purescript
listDomains :: forall eff. ListDomainsRequest -> Aff (err :: RequestError | eff) ListDomainsResponse
```

<p>This operation returns all the domain names registered with Amazon Route 53 for the current AWS account.</p>

#### `listOperations`

``` purescript
listOperations :: forall eff. ListOperationsRequest -> Aff (err :: RequestError | eff) ListOperationsResponse
```

<p>This operation returns the operation IDs of operations that are not yet complete.</p>

#### `listTagsForDomain`

``` purescript
listTagsForDomain :: forall eff. ListTagsForDomainRequest -> Aff (err :: RequestError | eff) ListTagsForDomainResponse
```

<p>This operation returns all of the tags that are associated with the specified domain.</p> <p>All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.</p>

#### `registerDomain`

``` purescript
registerDomain :: forall eff. RegisterDomainRequest -> Aff (err :: RequestError | eff) RegisterDomainResponse
```

<p>This operation registers a domain. Domains are registered by the AWS registrar partner, Gandi. For some top-level domains (TLDs), this operation requires extra parameters.</p> <p>When you register a domain, Amazon Route 53 does the following:</p> <ul> <li> <p>Creates a Amazon Route 53 hosted zone that has the same name as the domain. Amazon Route 53 assigns four name servers to your hosted zone and automatically updates your domain registration with the names of these name servers.</p> </li> <li> <p>Enables autorenew, so your domain registration will renew automatically each year. We'll notify you in advance of the renewal date so you can choose whether to renew the registration.</p> </li> <li> <p>Optionally enables privacy protection, so WHOIS queries return contact information for our registrar partner, Gandi, instead of the information you entered for registrant, admin, and tech contacts.</p> </li> <li> <p>If registration is successful, returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant is notified by email.</p> </li> <li> <p>Charges your AWS account an amount based on the top-level domain. For more information, see <a href="http://aws.amazon.com/route53/pricing/">Amazon Route 53 Pricing</a>.</p> </li> </ul>

#### `renewDomain`

``` purescript
renewDomain :: forall eff. RenewDomainRequest -> Aff (err :: RequestError | eff) RenewDomainResponse
```

<p>This operation renews a domain for the specified number of years. The cost of renewing your domain is billed to your AWS account.</p> <p>We recommend that you renew your domain several weeks before the expiration date. Some TLD registries delete domains before the expiration date if you haven't renewed far enough in advance. For more information about renewing domain registration, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-renew.html">Renewing Registration for a Domain</a> in the Amazon Route 53 Developer Guide.</p>

#### `resendContactReachabilityEmail`

``` purescript
resendContactReachabilityEmail :: forall eff. ResendContactReachabilityEmailRequest -> Aff (err :: RequestError | eff) ResendContactReachabilityEmailResponse
```

<p>For operations that require confirmation that the email address for the registrant contact is valid, such as registering a new domain, this operation resends the confirmation email to the current email address for the registrant contact.</p>

#### `retrieveDomainAuthCode`

``` purescript
retrieveDomainAuthCode :: forall eff. RetrieveDomainAuthCodeRequest -> Aff (err :: RequestError | eff) RetrieveDomainAuthCodeResponse
```

<p>This operation returns the AuthCode for the domain. To transfer a domain to another registrar, you provide this value to the new registrar.</p>

#### `transferDomain`

``` purescript
transferDomain :: forall eff. TransferDomainRequest -> Aff (err :: RequestError | eff) TransferDomainResponse
```

<p>This operation transfers a domain from another registrar to Amazon Route 53. When the transfer is complete, the domain is registered with the AWS registrar partner, Gandi.</p> <p>For transfer requirements, a detailed procedure, and information about viewing the status of a domain transfer, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-transfer-to-route-53.html">Transferring Registration for a Domain to Amazon Route 53</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>If the registrar for your domain is also the DNS service provider for the domain, we highly recommend that you consider transferring your DNS service to Amazon Route 53 or to another DNS service provider before you transfer your registration. Some registrars provide free DNS service when you purchase a domain registration. When you transfer the registration, the previous registrar will not renew your domain registration and could end your DNS service at any time.</p> <important> <p>If the registrar for your domain is also the DNS service provider for the domain and you don't transfer DNS service to another provider, your website, email, and the web applications associated with the domain might become unavailable.</p> </important> <p>If the transfer is successful, this method returns an operation ID that you can use to track the progress and completion of the action. If the transfer doesn't complete successfully, the domain registrant will be notified by email.</p>

#### `updateDomainContact`

``` purescript
updateDomainContact :: forall eff. UpdateDomainContactRequest -> Aff (err :: RequestError | eff) UpdateDomainContactResponse
```

<p>This operation updates the contact information for a particular domain. Information for at least one contact (registrant, administrator, or technical) must be supplied for update.</p> <p>If the update is successful, this method returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>

#### `updateDomainContactPrivacy`

``` purescript
updateDomainContactPrivacy :: forall eff. UpdateDomainContactPrivacyRequest -> Aff (err :: RequestError | eff) UpdateDomainContactPrivacyResponse
```

<p>This operation updates the specified domain contact's privacy setting. When the privacy option is enabled, personal information such as postal or email address is hidden from the results of a public WHOIS query. The privacy services are provided by the AWS registrar, Gandi. For more information, see the <a href="http://www.gandi.net/domain/whois/?currency=USD&amp;amp;lang=en">Gandi privacy features</a>.</p> <p>This operation only affects the privacy of the specified contact type (registrant, administrator, or tech). Successful acceptance returns an operation ID that you can use with <a>GetOperationDetail</a> to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>

#### `updateDomainNameservers`

``` purescript
updateDomainNameservers :: forall eff. UpdateDomainNameserversRequest -> Aff (err :: RequestError | eff) UpdateDomainNameserversResponse
```

<p>This operation replaces the current set of name servers for the domain with the specified set of name servers. If you use Amazon Route 53 as your DNS service, specify the four name servers in the delegation set for the hosted zone for the domain.</p> <p>If successful, this operation returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>

#### `updateTagsForDomain`

``` purescript
updateTagsForDomain :: forall eff. UpdateTagsForDomainRequest -> Aff (err :: RequestError | eff) UpdateTagsForDomainResponse
```

<p>This operation adds or updates tags for a specified domain.</p> <p>All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.</p>

#### `viewBilling`

``` purescript
viewBilling :: forall eff. ViewBillingRequest -> Aff (err :: RequestError | eff) ViewBillingResponse
```

<p>Returns all the domain-related billing records for the current AWS account for a specified period</p>

#### `AddressLine`

``` purescript
newtype AddressLine
  = AddressLine String
```

##### Instances
``` purescript
Newtype AddressLine _
```

#### `BillingRecord`

``` purescript
newtype BillingRecord
  = BillingRecord { "DomainName" :: NullOrUndefined (DomainName), "Operation" :: NullOrUndefined (OperationType), "InvoiceId" :: NullOrUndefined (InvoiceId), "BillDate" :: NullOrUndefined (Number), "Price" :: NullOrUndefined (Price) }
```

<p>Information for one billing record.</p>

##### Instances
``` purescript
Newtype BillingRecord _
```

#### `BillingRecords`

``` purescript
newtype BillingRecords
  = BillingRecords (Array BillingRecord)
```

##### Instances
``` purescript
Newtype BillingRecords _
```

#### `CheckDomainAvailabilityRequest`

``` purescript
newtype CheckDomainAvailabilityRequest
  = CheckDomainAvailabilityRequest { "DomainName" :: DomainName, "IdnLangCode" :: NullOrUndefined (LangCode) }
```

<p>The CheckDomainAvailability request contains the following elements.</p>

##### Instances
``` purescript
Newtype CheckDomainAvailabilityRequest _
```

#### `CheckDomainAvailabilityResponse`

``` purescript
newtype CheckDomainAvailabilityResponse
  = CheckDomainAvailabilityResponse { "Availability" :: DomainAvailability }
```

<p>The CheckDomainAvailability response includes the following elements.</p>

##### Instances
``` purescript
Newtype CheckDomainAvailabilityResponse _
```

#### `CheckDomainTransferabilityRequest`

``` purescript
newtype CheckDomainTransferabilityRequest
  = CheckDomainTransferabilityRequest { "DomainName" :: DomainName, "AuthCode" :: NullOrUndefined (DomainAuthCode) }
```

<p>The CheckDomainTransferability request contains the following elements.</p>

##### Instances
``` purescript
Newtype CheckDomainTransferabilityRequest _
```

#### `CheckDomainTransferabilityResponse`

``` purescript
newtype CheckDomainTransferabilityResponse
  = CheckDomainTransferabilityResponse { "Transferability" :: DomainTransferability }
```

<p>The CheckDomainTransferability response includes the following elements.</p>

##### Instances
``` purescript
Newtype CheckDomainTransferabilityResponse _
```

#### `City`

``` purescript
newtype City
  = City String
```

##### Instances
``` purescript
Newtype City _
```

#### `ContactDetail`

``` purescript
newtype ContactDetail
  = ContactDetail { "FirstName" :: NullOrUndefined (ContactName), "LastName" :: NullOrUndefined (ContactName), "ContactType" :: NullOrUndefined (ContactType), "OrganizationName" :: NullOrUndefined (ContactName), "AddressLine1" :: NullOrUndefined (AddressLine), "AddressLine2" :: NullOrUndefined (AddressLine), "City" :: NullOrUndefined (City), "State" :: NullOrUndefined (State), "CountryCode" :: NullOrUndefined (CountryCode), "ZipCode" :: NullOrUndefined (ZipCode), "PhoneNumber" :: NullOrUndefined (ContactNumber), "Email" :: NullOrUndefined (Email), "Fax" :: NullOrUndefined (ContactNumber), "ExtraParams" :: NullOrUndefined (ExtraParamList) }
```

<p>ContactDetail includes the following elements.</p>

##### Instances
``` purescript
Newtype ContactDetail _
```

#### `ContactName`

``` purescript
newtype ContactName
  = ContactName String
```

##### Instances
``` purescript
Newtype ContactName _
```

#### `ContactNumber`

``` purescript
newtype ContactNumber
  = ContactNumber String
```

##### Instances
``` purescript
Newtype ContactNumber _
```

#### `ContactType`

``` purescript
newtype ContactType
  = ContactType String
```

##### Instances
``` purescript
Newtype ContactType _
```

#### `CountryCode`

``` purescript
newtype CountryCode
  = CountryCode String
```

##### Instances
``` purescript
Newtype CountryCode _
```

#### `CurrentExpiryYear`

``` purescript
newtype CurrentExpiryYear
  = CurrentExpiryYear Int
```

##### Instances
``` purescript
Newtype CurrentExpiryYear _
```

#### `DNSSec`

``` purescript
newtype DNSSec
  = DNSSec String
```

##### Instances
``` purescript
Newtype DNSSec _
```

#### `DeleteTagsForDomainRequest`

``` purescript
newtype DeleteTagsForDomainRequest
  = DeleteTagsForDomainRequest { "DomainName" :: DomainName, "TagsToDelete" :: TagKeyList }
```

<p>The DeleteTagsForDomainRequest includes the following elements.</p>

##### Instances
``` purescript
Newtype DeleteTagsForDomainRequest _
```

#### `DeleteTagsForDomainResponse`

``` purescript
newtype DeleteTagsForDomainResponse
  = DeleteTagsForDomainResponse {  }
```

##### Instances
``` purescript
Newtype DeleteTagsForDomainResponse _
```

#### `DisableDomainAutoRenewRequest`

``` purescript
newtype DisableDomainAutoRenewRequest
  = DisableDomainAutoRenewRequest { "DomainName" :: DomainName }
```

##### Instances
``` purescript
Newtype DisableDomainAutoRenewRequest _
```

#### `DisableDomainAutoRenewResponse`

``` purescript
newtype DisableDomainAutoRenewResponse
  = DisableDomainAutoRenewResponse {  }
```

##### Instances
``` purescript
Newtype DisableDomainAutoRenewResponse _
```

#### `DisableDomainTransferLockRequest`

``` purescript
newtype DisableDomainTransferLockRequest
  = DisableDomainTransferLockRequest { "DomainName" :: DomainName }
```

<p>The DisableDomainTransferLock request includes the following element.</p>

##### Instances
``` purescript
Newtype DisableDomainTransferLockRequest _
```

#### `DisableDomainTransferLockResponse`

``` purescript
newtype DisableDomainTransferLockResponse
  = DisableDomainTransferLockResponse { "OperationId" :: OperationId }
```

<p>The DisableDomainTransferLock response includes the following element.</p>

##### Instances
``` purescript
Newtype DisableDomainTransferLockResponse _
```

#### `DomainAuthCode`

``` purescript
newtype DomainAuthCode
  = DomainAuthCode String
```

##### Instances
``` purescript
Newtype DomainAuthCode _
```

#### `DomainAvailability`

``` purescript
newtype DomainAvailability
  = DomainAvailability String
```

##### Instances
``` purescript
Newtype DomainAvailability _
```

#### `DomainLimitExceeded`

``` purescript
newtype DomainLimitExceeded
  = DomainLimitExceeded { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The number of domains has exceeded the allowed threshold for the account.</p>

##### Instances
``` purescript
Newtype DomainLimitExceeded _
```

#### `DomainName`

``` purescript
newtype DomainName
  = DomainName String
```

##### Instances
``` purescript
Newtype DomainName _
```

#### `DomainStatus`

``` purescript
newtype DomainStatus
  = DomainStatus String
```

##### Instances
``` purescript
Newtype DomainStatus _
```

#### `DomainStatusList`

``` purescript
newtype DomainStatusList
  = DomainStatusList (Array DomainStatus)
```

##### Instances
``` purescript
Newtype DomainStatusList _
```

#### `DomainSuggestion`

``` purescript
newtype DomainSuggestion
  = DomainSuggestion { "DomainName" :: NullOrUndefined (DomainName), "Availability" :: NullOrUndefined (String) }
```

<p>Information about one suggested domain name.</p>

##### Instances
``` purescript
Newtype DomainSuggestion _
```

#### `DomainSuggestionsList`

``` purescript
newtype DomainSuggestionsList
  = DomainSuggestionsList (Array DomainSuggestion)
```

##### Instances
``` purescript
Newtype DomainSuggestionsList _
```

#### `DomainSummary`

``` purescript
newtype DomainSummary
  = DomainSummary { "DomainName" :: DomainName, "AutoRenew" :: NullOrUndefined (Boolean), "TransferLock" :: NullOrUndefined (Boolean), "Expiry" :: NullOrUndefined (Number) }
```

<p>Summary information about one domain.</p>

##### Instances
``` purescript
Newtype DomainSummary _
```

#### `DomainSummaryList`

``` purescript
newtype DomainSummaryList
  = DomainSummaryList (Array DomainSummary)
```

##### Instances
``` purescript
Newtype DomainSummaryList _
```

#### `DomainTransferability`

``` purescript
newtype DomainTransferability
  = DomainTransferability { "Transferable" :: NullOrUndefined (Transferable) }
```

##### Instances
``` purescript
Newtype DomainTransferability _
```

#### `DuplicateRequest`

``` purescript
newtype DuplicateRequest
  = DuplicateRequest { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The request is already in progress for the domain.</p>

##### Instances
``` purescript
Newtype DuplicateRequest _
```

#### `DurationInYears`

``` purescript
newtype DurationInYears
  = DurationInYears Int
```

##### Instances
``` purescript
Newtype DurationInYears _
```

#### `Email`

``` purescript
newtype Email
  = Email String
```

##### Instances
``` purescript
Newtype Email _
```

#### `EnableDomainAutoRenewRequest`

``` purescript
newtype EnableDomainAutoRenewRequest
  = EnableDomainAutoRenewRequest { "DomainName" :: DomainName }
```

##### Instances
``` purescript
Newtype EnableDomainAutoRenewRequest _
```

#### `EnableDomainAutoRenewResponse`

``` purescript
newtype EnableDomainAutoRenewResponse
  = EnableDomainAutoRenewResponse {  }
```

##### Instances
``` purescript
Newtype EnableDomainAutoRenewResponse _
```

#### `EnableDomainTransferLockRequest`

``` purescript
newtype EnableDomainTransferLockRequest
  = EnableDomainTransferLockRequest { "DomainName" :: DomainName }
```

<p>A request to set the transfer lock for the specified domain.</p>

##### Instances
``` purescript
Newtype EnableDomainTransferLockRequest _
```

#### `EnableDomainTransferLockResponse`

``` purescript
newtype EnableDomainTransferLockResponse
  = EnableDomainTransferLockResponse { "OperationId" :: OperationId }
```

<p>The EnableDomainTransferLock response includes the following elements.</p>

##### Instances
``` purescript
Newtype EnableDomainTransferLockResponse _
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

#### `ExtraParam`

``` purescript
newtype ExtraParam
  = ExtraParam { "Name" :: ExtraParamName, "Value" :: ExtraParamValue }
```

<p>ExtraParam includes the following elements.</p>

##### Instances
``` purescript
Newtype ExtraParam _
```

#### `ExtraParamList`

``` purescript
newtype ExtraParamList
  = ExtraParamList (Array ExtraParam)
```

##### Instances
``` purescript
Newtype ExtraParamList _
```

#### `ExtraParamName`

``` purescript
newtype ExtraParamName
  = ExtraParamName String
```

##### Instances
``` purescript
Newtype ExtraParamName _
```

#### `ExtraParamValue`

``` purescript
newtype ExtraParamValue
  = ExtraParamValue String
```

##### Instances
``` purescript
Newtype ExtraParamValue _
```

#### `FIAuthKey`

``` purescript
newtype FIAuthKey
  = FIAuthKey String
```

##### Instances
``` purescript
Newtype FIAuthKey _
```

#### `GetContactReachabilityStatusRequest`

``` purescript
newtype GetContactReachabilityStatusRequest
  = GetContactReachabilityStatusRequest { "DomainName'" :: NullOrUndefined (DomainName) }
```

##### Instances
``` purescript
Newtype GetContactReachabilityStatusRequest _
```

#### `GetContactReachabilityStatusResponse`

``` purescript
newtype GetContactReachabilityStatusResponse
  = GetContactReachabilityStatusResponse { "DomainName'" :: NullOrUndefined (DomainName), "Status'" :: NullOrUndefined (ReachabilityStatus) }
```

##### Instances
``` purescript
Newtype GetContactReachabilityStatusResponse _
```

#### `GetDomainDetailRequest`

``` purescript
newtype GetDomainDetailRequest
  = GetDomainDetailRequest { "DomainName" :: DomainName }
```

<p>The GetDomainDetail request includes the following element.</p>

##### Instances
``` purescript
Newtype GetDomainDetailRequest _
```

#### `GetDomainDetailResponse`

``` purescript
newtype GetDomainDetailResponse
  = GetDomainDetailResponse { "DomainName" :: DomainName, "Nameservers" :: NameserverList, "AutoRenew" :: NullOrUndefined (Boolean), "AdminContact" :: ContactDetail, "RegistrantContact" :: ContactDetail, "TechContact" :: ContactDetail, "AdminPrivacy" :: NullOrUndefined (Boolean), "RegistrantPrivacy" :: NullOrUndefined (Boolean), "TechPrivacy" :: NullOrUndefined (Boolean), "RegistrarName" :: NullOrUndefined (RegistrarName), "WhoIsServer" :: NullOrUndefined (RegistrarWhoIsServer), "RegistrarUrl" :: NullOrUndefined (RegistrarUrl), "AbuseContactEmail" :: NullOrUndefined (Email), "AbuseContactPhone" :: NullOrUndefined (ContactNumber), "RegistryDomainId" :: NullOrUndefined (RegistryDomainId), "CreationDate" :: NullOrUndefined (Number), "UpdatedDate" :: NullOrUndefined (Number), "ExpirationDate" :: NullOrUndefined (Number), "Reseller" :: NullOrUndefined (Reseller), "DnsSec" :: NullOrUndefined (DNSSec), "StatusList" :: NullOrUndefined (DomainStatusList) }
```

<p>The GetDomainDetail response includes the following elements.</p>

##### Instances
``` purescript
Newtype GetDomainDetailResponse _
```

#### `GetDomainSuggestionsRequest`

``` purescript
newtype GetDomainSuggestionsRequest
  = GetDomainSuggestionsRequest { "DomainName" :: DomainName, "SuggestionCount" :: Int, "OnlyAvailable" :: Boolean }
```

##### Instances
``` purescript
Newtype GetDomainSuggestionsRequest _
```

#### `GetDomainSuggestionsResponse`

``` purescript
newtype GetDomainSuggestionsResponse
  = GetDomainSuggestionsResponse { "SuggestionsList" :: NullOrUndefined (DomainSuggestionsList) }
```

##### Instances
``` purescript
Newtype GetDomainSuggestionsResponse _
```

#### `GetOperationDetailRequest`

``` purescript
newtype GetOperationDetailRequest
  = GetOperationDetailRequest { "OperationId" :: OperationId }
```

<p>The <a>GetOperationDetail</a> request includes the following element.</p>

##### Instances
``` purescript
Newtype GetOperationDetailRequest _
```

#### `GetOperationDetailResponse`

``` purescript
newtype GetOperationDetailResponse
  = GetOperationDetailResponse { "OperationId" :: NullOrUndefined (OperationId), "Status" :: NullOrUndefined (OperationStatus), "Message" :: NullOrUndefined (ErrorMessage), "DomainName" :: NullOrUndefined (DomainName), "Type" :: NullOrUndefined (OperationType), "SubmittedDate" :: NullOrUndefined (Number) }
```

<p>The GetOperationDetail response includes the following elements.</p>

##### Instances
``` purescript
Newtype GetOperationDetailResponse _
```

#### `GlueIp`

``` purescript
newtype GlueIp
  = GlueIp String
```

##### Instances
``` purescript
Newtype GlueIp _
```

#### `GlueIpList`

``` purescript
newtype GlueIpList
  = GlueIpList (Array GlueIp)
```

##### Instances
``` purescript
Newtype GlueIpList _
```

#### `HostName`

``` purescript
newtype HostName
  = HostName String
```

##### Instances
``` purescript
Newtype HostName _
```

#### `InvalidInput`

``` purescript
newtype InvalidInput
  = InvalidInput { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The requested item is not acceptable. For example, for an OperationId it might refer to the ID of an operation that is already completed. For a domain name, it might not be a valid domain name or belong to the requester account.</p>

##### Instances
``` purescript
Newtype InvalidInput _
```

#### `InvoiceId`

``` purescript
newtype InvoiceId
  = InvoiceId String
```

##### Instances
``` purescript
Newtype InvoiceId _
```

#### `LangCode`

``` purescript
newtype LangCode
  = LangCode String
```

##### Instances
``` purescript
Newtype LangCode _
```

#### `ListDomainsRequest`

``` purescript
newtype ListDomainsRequest
  = ListDomainsRequest { "Marker" :: NullOrUndefined (PageMarker), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>The ListDomains request includes the following elements.</p>

##### Instances
``` purescript
Newtype ListDomainsRequest _
```

#### `ListDomainsResponse`

``` purescript
newtype ListDomainsResponse
  = ListDomainsResponse { "Domains" :: DomainSummaryList, "NextPageMarker" :: NullOrUndefined (PageMarker) }
```

<p>The ListDomains response includes the following elements.</p>

##### Instances
``` purescript
Newtype ListDomainsResponse _
```

#### `ListOperationsRequest`

``` purescript
newtype ListOperationsRequest
  = ListOperationsRequest { "Marker" :: NullOrUndefined (PageMarker), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>The ListOperations request includes the following elements.</p>

##### Instances
``` purescript
Newtype ListOperationsRequest _
```

#### `ListOperationsResponse`

``` purescript
newtype ListOperationsResponse
  = ListOperationsResponse { "Operations" :: OperationSummaryList, "NextPageMarker" :: NullOrUndefined (PageMarker) }
```

<p>The ListOperations response includes the following elements.</p>

##### Instances
``` purescript
Newtype ListOperationsResponse _
```

#### `ListTagsForDomainRequest`

``` purescript
newtype ListTagsForDomainRequest
  = ListTagsForDomainRequest { "DomainName" :: DomainName }
```

<p>The ListTagsForDomainRequest includes the following elements.</p>

##### Instances
``` purescript
Newtype ListTagsForDomainRequest _
```

#### `ListTagsForDomainResponse`

``` purescript
newtype ListTagsForDomainResponse
  = ListTagsForDomainResponse { "TagList" :: TagList }
```

<p>The ListTagsForDomain response includes the following elements.</p>

##### Instances
``` purescript
Newtype ListTagsForDomainResponse _
```

#### `Nameserver`

``` purescript
newtype Nameserver
  = Nameserver { "Name" :: HostName, "GlueIps" :: NullOrUndefined (GlueIpList) }
```

<p>Nameserver includes the following elements.</p>

##### Instances
``` purescript
Newtype Nameserver _
```

#### `NameserverList`

``` purescript
newtype NameserverList
  = NameserverList (Array Nameserver)
```

##### Instances
``` purescript
Newtype NameserverList _
```

#### `OperationId`

``` purescript
newtype OperationId
  = OperationId String
```

##### Instances
``` purescript
Newtype OperationId _
```

#### `OperationLimitExceeded`

``` purescript
newtype OperationLimitExceeded
  = OperationLimitExceeded { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The number of operations or jobs running exceeded the allowed threshold for the account.</p>

##### Instances
``` purescript
Newtype OperationLimitExceeded _
```

#### `OperationStatus`

``` purescript
newtype OperationStatus
  = OperationStatus String
```

##### Instances
``` purescript
Newtype OperationStatus _
```

#### `OperationSummary`

``` purescript
newtype OperationSummary
  = OperationSummary { "OperationId" :: OperationId, "Status" :: OperationStatus, "Type" :: OperationType, "SubmittedDate" :: Number }
```

<p>OperationSummary includes the following elements.</p>

##### Instances
``` purescript
Newtype OperationSummary _
```

#### `OperationSummaryList`

``` purescript
newtype OperationSummaryList
  = OperationSummaryList (Array OperationSummary)
```

##### Instances
``` purescript
Newtype OperationSummaryList _
```

#### `OperationType`

``` purescript
newtype OperationType
  = OperationType String
```

##### Instances
``` purescript
Newtype OperationType _
```

#### `PageMarker`

``` purescript
newtype PageMarker
  = PageMarker String
```

##### Instances
``` purescript
Newtype PageMarker _
```

#### `PageMaxItems`

``` purescript
newtype PageMaxItems
  = PageMaxItems Int
```

##### Instances
``` purescript
Newtype PageMaxItems _
```

#### `Price`

``` purescript
newtype Price
  = Price Number
```

##### Instances
``` purescript
Newtype Price _
```

#### `ReachabilityStatus`

``` purescript
newtype ReachabilityStatus
  = ReachabilityStatus String
```

##### Instances
``` purescript
Newtype ReachabilityStatus _
```

#### `RegisterDomainRequest`

``` purescript
newtype RegisterDomainRequest
  = RegisterDomainRequest { "DomainName" :: DomainName, "IdnLangCode" :: NullOrUndefined (LangCode), "DurationInYears" :: DurationInYears, "AutoRenew" :: NullOrUndefined (Boolean), "AdminContact" :: ContactDetail, "RegistrantContact" :: ContactDetail, "TechContact" :: ContactDetail, "PrivacyProtectAdminContact" :: NullOrUndefined (Boolean), "PrivacyProtectRegistrantContact" :: NullOrUndefined (Boolean), "PrivacyProtectTechContact" :: NullOrUndefined (Boolean) }
```

<p>The RegisterDomain request includes the following elements.</p>

##### Instances
``` purescript
Newtype RegisterDomainRequest _
```

#### `RegisterDomainResponse`

``` purescript
newtype RegisterDomainResponse
  = RegisterDomainResponse { "OperationId" :: OperationId }
```

<p>The RegisterDomain response includes the following element.</p>

##### Instances
``` purescript
Newtype RegisterDomainResponse _
```

#### `RegistrarName`

``` purescript
newtype RegistrarName
  = RegistrarName String
```

##### Instances
``` purescript
Newtype RegistrarName _
```

#### `RegistrarUrl`

``` purescript
newtype RegistrarUrl
  = RegistrarUrl String
```

##### Instances
``` purescript
Newtype RegistrarUrl _
```

#### `RegistrarWhoIsServer`

``` purescript
newtype RegistrarWhoIsServer
  = RegistrarWhoIsServer String
```

##### Instances
``` purescript
Newtype RegistrarWhoIsServer _
```

#### `RegistryDomainId`

``` purescript
newtype RegistryDomainId
  = RegistryDomainId String
```

##### Instances
``` purescript
Newtype RegistryDomainId _
```

#### `RenewDomainRequest`

``` purescript
newtype RenewDomainRequest
  = RenewDomainRequest { "DomainName" :: DomainName, "DurationInYears" :: NullOrUndefined (DurationInYears), "CurrentExpiryYear" :: CurrentExpiryYear }
```

<p>A <code>RenewDomain</code> request includes the number of years that you want to renew for and the current expiration year.</p>

##### Instances
``` purescript
Newtype RenewDomainRequest _
```

#### `RenewDomainResponse`

``` purescript
newtype RenewDomainResponse
  = RenewDomainResponse { "OperationId" :: OperationId }
```

##### Instances
``` purescript
Newtype RenewDomainResponse _
```

#### `Reseller`

``` purescript
newtype Reseller
  = Reseller String
```

##### Instances
``` purescript
Newtype Reseller _
```

#### `ResendContactReachabilityEmailRequest`

``` purescript
newtype ResendContactReachabilityEmailRequest
  = ResendContactReachabilityEmailRequest { "DomainName'" :: NullOrUndefined (DomainName) }
```

##### Instances
``` purescript
Newtype ResendContactReachabilityEmailRequest _
```

#### `ResendContactReachabilityEmailResponse`

``` purescript
newtype ResendContactReachabilityEmailResponse
  = ResendContactReachabilityEmailResponse { "DomainName'" :: NullOrUndefined (DomainName), "EmailAddress'" :: NullOrUndefined (Email), "IsAlreadyVerified'" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype ResendContactReachabilityEmailResponse _
```

#### `RetrieveDomainAuthCodeRequest`

``` purescript
newtype RetrieveDomainAuthCodeRequest
  = RetrieveDomainAuthCodeRequest { "DomainName" :: DomainName }
```

<p>A request for the authorization code for the specified domain. To transfer a domain to another registrar, you provide this value to the new registrar.</p>

##### Instances
``` purescript
Newtype RetrieveDomainAuthCodeRequest _
```

#### `RetrieveDomainAuthCodeResponse`

``` purescript
newtype RetrieveDomainAuthCodeResponse
  = RetrieveDomainAuthCodeResponse { "AuthCode" :: DomainAuthCode }
```

<p>The RetrieveDomainAuthCode response includes the following element.</p>

##### Instances
``` purescript
Newtype RetrieveDomainAuthCodeResponse _
```

#### `State`

``` purescript
newtype State
  = State String
```

##### Instances
``` purescript
Newtype State _
```

#### `TLDRulesViolation`

``` purescript
newtype TLDRulesViolation
  = TLDRulesViolation { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The top-level domain does not support this operation.</p>

##### Instances
``` purescript
Newtype TLDRulesViolation _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (TagKey), "Value" :: NullOrUndefined (TagValue) }
```

<p>Each tag includes the following elements.</p>

##### Instances
``` purescript
Newtype Tag _
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

##### Instances
``` purescript
Newtype TagKey _
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

##### Instances
``` purescript
Newtype TagKeyList _
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

##### Instances
``` purescript
Newtype TagList _
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

##### Instances
``` purescript
Newtype TagValue _
```

#### `TransferDomainRequest`

``` purescript
newtype TransferDomainRequest
  = TransferDomainRequest { "DomainName" :: DomainName, "IdnLangCode" :: NullOrUndefined (LangCode), "DurationInYears" :: DurationInYears, "Nameservers" :: NullOrUndefined (NameserverList), "AuthCode" :: NullOrUndefined (DomainAuthCode), "AutoRenew" :: NullOrUndefined (Boolean), "AdminContact" :: ContactDetail, "RegistrantContact" :: ContactDetail, "TechContact" :: ContactDetail, "PrivacyProtectAdminContact" :: NullOrUndefined (Boolean), "PrivacyProtectRegistrantContact" :: NullOrUndefined (Boolean), "PrivacyProtectTechContact" :: NullOrUndefined (Boolean) }
```

<p>The TransferDomain request includes the following elements.</p>

##### Instances
``` purescript
Newtype TransferDomainRequest _
```

#### `TransferDomainResponse`

``` purescript
newtype TransferDomainResponse
  = TransferDomainResponse { "OperationId" :: OperationId }
```

<p>The TranserDomain response includes the following element.</p>

##### Instances
``` purescript
Newtype TransferDomainResponse _
```

#### `Transferable`

``` purescript
newtype Transferable
  = Transferable String
```

<p>Whether the domain name can be transferred to Amazon Route 53.</p> <note> <p>You can transfer only domains that have a value of <code>TRANSFERABLE</code> for <code>Transferable</code>.</p> </note> <p>Valid values:</p> <dl> <dt>TRANSFERABLE</dt> <dd> <p>The domain name can be transferred to Amazon Route 53.</p> </dd> <dt>UNTRANSFERRABLE</dt> <dd> <p>The domain name can't be transferred to Amazon Route 53.</p> </dd> <dt>DONT_KNOW</dt> <dd> <p>Reserved for future use.</p> </dd> </dl>

##### Instances
``` purescript
Newtype Transferable _
```

#### `UnsupportedTLD`

``` purescript
newtype UnsupportedTLD
  = UnsupportedTLD { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Amazon Route 53 does not support this top-level domain (TLD).</p>

##### Instances
``` purescript
Newtype UnsupportedTLD _
```

#### `UpdateDomainContactPrivacyRequest`

``` purescript
newtype UpdateDomainContactPrivacyRequest
  = UpdateDomainContactPrivacyRequest { "DomainName" :: DomainName, "AdminPrivacy" :: NullOrUndefined (Boolean), "RegistrantPrivacy" :: NullOrUndefined (Boolean), "TechPrivacy" :: NullOrUndefined (Boolean) }
```

<p>The UpdateDomainContactPrivacy request includes the following elements.</p>

##### Instances
``` purescript
Newtype UpdateDomainContactPrivacyRequest _
```

#### `UpdateDomainContactPrivacyResponse`

``` purescript
newtype UpdateDomainContactPrivacyResponse
  = UpdateDomainContactPrivacyResponse { "OperationId" :: OperationId }
```

<p>The UpdateDomainContactPrivacy response includes the following element.</p>

##### Instances
``` purescript
Newtype UpdateDomainContactPrivacyResponse _
```

#### `UpdateDomainContactRequest`

``` purescript
newtype UpdateDomainContactRequest
  = UpdateDomainContactRequest { "DomainName" :: DomainName, "AdminContact" :: NullOrUndefined (ContactDetail), "RegistrantContact" :: NullOrUndefined (ContactDetail), "TechContact" :: NullOrUndefined (ContactDetail) }
```

<p>The UpdateDomainContact request includes the following elements.</p>

##### Instances
``` purescript
Newtype UpdateDomainContactRequest _
```

#### `UpdateDomainContactResponse`

``` purescript
newtype UpdateDomainContactResponse
  = UpdateDomainContactResponse { "OperationId" :: OperationId }
```

<p>The UpdateDomainContact response includes the following element.</p>

##### Instances
``` purescript
Newtype UpdateDomainContactResponse _
```

#### `UpdateDomainNameserversRequest`

``` purescript
newtype UpdateDomainNameserversRequest
  = UpdateDomainNameserversRequest { "DomainName" :: DomainName, "FIAuthKey" :: NullOrUndefined (FIAuthKey), "Nameservers" :: NameserverList }
```

<p>Replaces the current set of name servers for the domain with the specified set of name servers. If you use Amazon Route 53 as your DNS service, specify the four name servers in the delegation set for the hosted zone for the domain.</p> <p>If successful, this operation returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email. </p>

##### Instances
``` purescript
Newtype UpdateDomainNameserversRequest _
```

#### `UpdateDomainNameserversResponse`

``` purescript
newtype UpdateDomainNameserversResponse
  = UpdateDomainNameserversResponse { "OperationId" :: OperationId }
```

<p>The UpdateDomainNameservers response includes the following element.</p>

##### Instances
``` purescript
Newtype UpdateDomainNameserversResponse _
```

#### `UpdateTagsForDomainRequest`

``` purescript
newtype UpdateTagsForDomainRequest
  = UpdateTagsForDomainRequest { "DomainName" :: DomainName, "TagsToUpdate" :: NullOrUndefined (TagList) }
```

<p>The UpdateTagsForDomainRequest includes the following elements.</p>

##### Instances
``` purescript
Newtype UpdateTagsForDomainRequest _
```

#### `UpdateTagsForDomainResponse`

``` purescript
newtype UpdateTagsForDomainResponse
  = UpdateTagsForDomainResponse {  }
```

##### Instances
``` purescript
Newtype UpdateTagsForDomainResponse _
```

#### `ViewBillingRequest`

``` purescript
newtype ViewBillingRequest
  = ViewBillingRequest { "Start" :: NullOrUndefined (Number), "End" :: NullOrUndefined (Number), "Marker" :: NullOrUndefined (PageMarker), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>The ViewBilling request includes the following elements.</p>

##### Instances
``` purescript
Newtype ViewBillingRequest _
```

#### `ViewBillingResponse`

``` purescript
newtype ViewBillingResponse
  = ViewBillingResponse { "NextPageMarker" :: NullOrUndefined (PageMarker), "BillingRecords" :: NullOrUndefined (BillingRecords) }
```

<p>The ViewBilling response includes the following elements.</p>

##### Instances
``` purescript
Newtype ViewBillingResponse _
```

#### `ZipCode`

``` purescript
newtype ZipCode
  = ZipCode String
```

##### Instances
``` purescript
Newtype ZipCode _
```


