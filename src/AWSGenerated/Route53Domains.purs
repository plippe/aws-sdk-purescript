

-- | <p>Amazon Route 53 API actions let you register domain names and perform related operations.</p>
module AWS.Route53Domains where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Route53Domains" :: String


-- | <p>This operation checks the availability of one domain name. Note that if the availability status of a domain is pending, you must submit another request to determine the availability of the domain name.</p>
checkDomainAvailability :: forall eff. CheckDomainAvailabilityRequest -> Aff (err :: AWS.RequestError | eff) CheckDomainAvailabilityResponse
checkDomainAvailability = AWS.request serviceName "CheckDomainAvailability" 


-- | <p>Checks whether a domain name can be transferred to Amazon Route 53. </p>
checkDomainTransferability :: forall eff. CheckDomainTransferabilityRequest -> Aff (err :: AWS.RequestError | eff) CheckDomainTransferabilityResponse
checkDomainTransferability = AWS.request serviceName "CheckDomainTransferability" 


-- | <p>This operation deletes the specified tags for a domain.</p> <p>All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.</p>
deleteTagsForDomain :: forall eff. DeleteTagsForDomainRequest -> Aff (err :: AWS.RequestError | eff) DeleteTagsForDomainResponse
deleteTagsForDomain = AWS.request serviceName "DeleteTagsForDomain" 


-- | <p>This operation disables automatic renewal of domain registration for the specified domain.</p>
disableDomainAutoRenew :: forall eff. DisableDomainAutoRenewRequest -> Aff (err :: AWS.RequestError | eff) DisableDomainAutoRenewResponse
disableDomainAutoRenew = AWS.request serviceName "DisableDomainAutoRenew" 


-- | <p>This operation removes the transfer lock on the domain (specifically the <code>clientTransferProhibited</code> status) to allow domain transfers. We recommend you refrain from performing this action unless you intend to transfer the domain to a different registrar. Successful submission returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>
disableDomainTransferLock :: forall eff. DisableDomainTransferLockRequest -> Aff (err :: AWS.RequestError | eff) DisableDomainTransferLockResponse
disableDomainTransferLock = AWS.request serviceName "DisableDomainTransferLock" 


-- | <p>This operation configures Amazon Route 53 to automatically renew the specified domain before the domain registration expires. The cost of renewing your domain registration is billed to your AWS account.</p> <p>The period during which you can renew a domain name varies by TLD. For a list of TLDs and their renewal policies, see <a href="http://wiki.gandi.net/en/domains/renew#renewal_restoration_and_deletion_times">"Renewal, restoration, and deletion times"</a> on the website for our registrar partner, Gandi. Route 53 requires that you renew before the end of the renewal period that is listed on the Gandi website so we can complete processing before the deadline.</p>
enableDomainAutoRenew :: forall eff. EnableDomainAutoRenewRequest -> Aff (err :: AWS.RequestError | eff) EnableDomainAutoRenewResponse
enableDomainAutoRenew = AWS.request serviceName "EnableDomainAutoRenew" 


-- | <p>This operation sets the transfer lock on the domain (specifically the <code>clientTransferProhibited</code> status) to prevent domain transfers. Successful submission returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>
enableDomainTransferLock :: forall eff. EnableDomainTransferLockRequest -> Aff (err :: AWS.RequestError | eff) EnableDomainTransferLockResponse
enableDomainTransferLock = AWS.request serviceName "EnableDomainTransferLock" 


-- | <p>For operations that require confirmation that the email address for the registrant contact is valid, such as registering a new domain, this operation returns information about whether the registrant contact has responded.</p> <p>If you want us to resend the email, use the <code>ResendContactReachabilityEmail</code> operation.</p>
getContactReachabilityStatus :: forall eff. GetContactReachabilityStatusRequest -> Aff (err :: AWS.RequestError | eff) GetContactReachabilityStatusResponse
getContactReachabilityStatus = AWS.request serviceName "GetContactReachabilityStatus" 


-- | <p>This operation returns detailed information about a specified domain that is associated with the current AWS account. Contact information for the domain is also returned as part of the output.</p>
getDomainDetail :: forall eff. GetDomainDetailRequest -> Aff (err :: AWS.RequestError | eff) GetDomainDetailResponse
getDomainDetail = AWS.request serviceName "GetDomainDetail" 


-- | <p>The GetDomainSuggestions operation returns a list of suggested domain names given a string, which can either be a domain name or simply a word or phrase (without spaces).</p>
getDomainSuggestions :: forall eff. GetDomainSuggestionsRequest -> Aff (err :: AWS.RequestError | eff) GetDomainSuggestionsResponse
getDomainSuggestions = AWS.request serviceName "GetDomainSuggestions" 


-- | <p>This operation returns the current status of an operation that is not completed.</p>
getOperationDetail :: forall eff. GetOperationDetailRequest -> Aff (err :: AWS.RequestError | eff) GetOperationDetailResponse
getOperationDetail = AWS.request serviceName "GetOperationDetail" 


-- | <p>This operation returns all the domain names registered with Amazon Route 53 for the current AWS account.</p>
listDomains :: forall eff. ListDomainsRequest -> Aff (err :: AWS.RequestError | eff) ListDomainsResponse
listDomains = AWS.request serviceName "ListDomains" 


-- | <p>This operation returns the operation IDs of operations that are not yet complete.</p>
listOperations :: forall eff. ListOperationsRequest -> Aff (err :: AWS.RequestError | eff) ListOperationsResponse
listOperations = AWS.request serviceName "ListOperations" 


-- | <p>This operation returns all of the tags that are associated with the specified domain.</p> <p>All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.</p>
listTagsForDomain :: forall eff. ListTagsForDomainRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForDomainResponse
listTagsForDomain = AWS.request serviceName "ListTagsForDomain" 


-- | <p>This operation registers a domain. Domains are registered by the AWS registrar partner, Gandi. For some top-level domains (TLDs), this operation requires extra parameters.</p> <p>When you register a domain, Amazon Route 53 does the following:</p> <ul> <li> <p>Creates a Amazon Route 53 hosted zone that has the same name as the domain. Amazon Route 53 assigns four name servers to your hosted zone and automatically updates your domain registration with the names of these name servers.</p> </li> <li> <p>Enables autorenew, so your domain registration will renew automatically each year. We'll notify you in advance of the renewal date so you can choose whether to renew the registration.</p> </li> <li> <p>Optionally enables privacy protection, so WHOIS queries return contact information for our registrar partner, Gandi, instead of the information you entered for registrant, admin, and tech contacts.</p> </li> <li> <p>If registration is successful, returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant is notified by email.</p> </li> <li> <p>Charges your AWS account an amount based on the top-level domain. For more information, see <a href="http://aws.amazon.com/route53/pricing/">Amazon Route 53 Pricing</a>.</p> </li> </ul>
registerDomain :: forall eff. RegisterDomainRequest -> Aff (err :: AWS.RequestError | eff) RegisterDomainResponse
registerDomain = AWS.request serviceName "RegisterDomain" 


-- | <p>This operation renews a domain for the specified number of years. The cost of renewing your domain is billed to your AWS account.</p> <p>We recommend that you renew your domain several weeks before the expiration date. Some TLD registries delete domains before the expiration date if you haven't renewed far enough in advance. For more information about renewing domain registration, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-renew.html">Renewing Registration for a Domain</a> in the Amazon Route 53 Developer Guide.</p>
renewDomain :: forall eff. RenewDomainRequest -> Aff (err :: AWS.RequestError | eff) RenewDomainResponse
renewDomain = AWS.request serviceName "RenewDomain" 


-- | <p>For operations that require confirmation that the email address for the registrant contact is valid, such as registering a new domain, this operation resends the confirmation email to the current email address for the registrant contact.</p>
resendContactReachabilityEmail :: forall eff. ResendContactReachabilityEmailRequest -> Aff (err :: AWS.RequestError | eff) ResendContactReachabilityEmailResponse
resendContactReachabilityEmail = AWS.request serviceName "ResendContactReachabilityEmail" 


-- | <p>This operation returns the AuthCode for the domain. To transfer a domain to another registrar, you provide this value to the new registrar.</p>
retrieveDomainAuthCode :: forall eff. RetrieveDomainAuthCodeRequest -> Aff (err :: AWS.RequestError | eff) RetrieveDomainAuthCodeResponse
retrieveDomainAuthCode = AWS.request serviceName "RetrieveDomainAuthCode" 


-- | <p>This operation transfers a domain from another registrar to Amazon Route 53. When the transfer is complete, the domain is registered with the AWS registrar partner, Gandi.</p> <p>For transfer requirements, a detailed procedure, and information about viewing the status of a domain transfer, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-transfer-to-route-53.html">Transferring Registration for a Domain to Amazon Route 53</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>If the registrar for your domain is also the DNS service provider for the domain, we highly recommend that you consider transferring your DNS service to Amazon Route 53 or to another DNS service provider before you transfer your registration. Some registrars provide free DNS service when you purchase a domain registration. When you transfer the registration, the previous registrar will not renew your domain registration and could end your DNS service at any time.</p> <important> <p>If the registrar for your domain is also the DNS service provider for the domain and you don't transfer DNS service to another provider, your website, email, and the web applications associated with the domain might become unavailable.</p> </important> <p>If the transfer is successful, this method returns an operation ID that you can use to track the progress and completion of the action. If the transfer doesn't complete successfully, the domain registrant will be notified by email.</p>
transferDomain :: forall eff. TransferDomainRequest -> Aff (err :: AWS.RequestError | eff) TransferDomainResponse
transferDomain = AWS.request serviceName "TransferDomain" 


-- | <p>This operation updates the contact information for a particular domain. Information for at least one contact (registrant, administrator, or technical) must be supplied for update.</p> <p>If the update is successful, this method returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>
updateDomainContact :: forall eff. UpdateDomainContactRequest -> Aff (err :: AWS.RequestError | eff) UpdateDomainContactResponse
updateDomainContact = AWS.request serviceName "UpdateDomainContact" 


-- | <p>This operation updates the specified domain contact's privacy setting. When the privacy option is enabled, personal information such as postal or email address is hidden from the results of a public WHOIS query. The privacy services are provided by the AWS registrar, Gandi. For more information, see the <a href="http://www.gandi.net/domain/whois/?currency=USD&amp;amp;lang=en">Gandi privacy features</a>.</p> <p>This operation only affects the privacy of the specified contact type (registrant, administrator, or tech). Successful acceptance returns an operation ID that you can use with <a>GetOperationDetail</a> to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>
updateDomainContactPrivacy :: forall eff. UpdateDomainContactPrivacyRequest -> Aff (err :: AWS.RequestError | eff) UpdateDomainContactPrivacyResponse
updateDomainContactPrivacy = AWS.request serviceName "UpdateDomainContactPrivacy" 


-- | <p>This operation replaces the current set of name servers for the domain with the specified set of name servers. If you use Amazon Route 53 as your DNS service, specify the four name servers in the delegation set for the hosted zone for the domain.</p> <p>If successful, this operation returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>
updateDomainNameservers :: forall eff. UpdateDomainNameserversRequest -> Aff (err :: AWS.RequestError | eff) UpdateDomainNameserversResponse
updateDomainNameservers = AWS.request serviceName "UpdateDomainNameservers" 


-- | <p>This operation adds or updates tags for a specified domain.</p> <p>All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.</p>
updateTagsForDomain :: forall eff. UpdateTagsForDomainRequest -> Aff (err :: AWS.RequestError | eff) UpdateTagsForDomainResponse
updateTagsForDomain = AWS.request serviceName "UpdateTagsForDomain" 


-- | <p>Returns all the domain-related billing records for the current AWS account for a specified period</p>
viewBilling :: forall eff. ViewBillingRequest -> Aff (err :: AWS.RequestError | eff) ViewBillingResponse
viewBilling = AWS.request serviceName "ViewBilling" 


newtype AddressLine = AddressLine String


-- | <p>Information for one billing record.</p>
newtype BillingRecord = BillingRecord 
  { "DomainName" :: NullOrUndefined (DomainName)
  , "Operation" :: NullOrUndefined (OperationType)
  , "InvoiceId" :: NullOrUndefined (InvoiceId)
  , "BillDate" :: NullOrUndefined (Number)
  , "Price" :: NullOrUndefined (Price)
  }


newtype BillingRecords = BillingRecords (Array BillingRecord)


-- | <p>The CheckDomainAvailability request contains the following elements.</p>
newtype CheckDomainAvailabilityRequest = CheckDomainAvailabilityRequest 
  { "DomainName" :: (DomainName)
  , "IdnLangCode" :: NullOrUndefined (LangCode)
  }


-- | <p>The CheckDomainAvailability response includes the following elements.</p>
newtype CheckDomainAvailabilityResponse = CheckDomainAvailabilityResponse 
  { "Availability" :: (DomainAvailability)
  }


-- | <p>The CheckDomainTransferability request contains the following elements.</p>
newtype CheckDomainTransferabilityRequest = CheckDomainTransferabilityRequest 
  { "DomainName" :: (DomainName)
  , "AuthCode" :: NullOrUndefined (DomainAuthCode)
  }


-- | <p>The CheckDomainTransferability response includes the following elements.</p>
newtype CheckDomainTransferabilityResponse = CheckDomainTransferabilityResponse 
  { "Transferability" :: (DomainTransferability)
  }


newtype City = City String


-- | <p>ContactDetail includes the following elements.</p>
newtype ContactDetail = ContactDetail 
  { "FirstName" :: NullOrUndefined (ContactName)
  , "LastName" :: NullOrUndefined (ContactName)
  , "ContactType" :: NullOrUndefined (ContactType)
  , "OrganizationName" :: NullOrUndefined (ContactName)
  , "AddressLine1" :: NullOrUndefined (AddressLine)
  , "AddressLine2" :: NullOrUndefined (AddressLine)
  , "City" :: NullOrUndefined (City)
  , "State" :: NullOrUndefined (State)
  , "CountryCode" :: NullOrUndefined (CountryCode)
  , "ZipCode" :: NullOrUndefined (ZipCode)
  , "PhoneNumber" :: NullOrUndefined (ContactNumber)
  , "Email" :: NullOrUndefined (Email)
  , "Fax" :: NullOrUndefined (ContactNumber)
  , "ExtraParams" :: NullOrUndefined (ExtraParamList)
  }


newtype ContactName = ContactName String


newtype ContactNumber = ContactNumber String


newtype ContactType = ContactType String


newtype CountryCode = CountryCode String


newtype CurrentExpiryYear = CurrentExpiryYear Int


newtype DNSSec = DNSSec String


-- | <p>The DeleteTagsForDomainRequest includes the following elements.</p>
newtype DeleteTagsForDomainRequest = DeleteTagsForDomainRequest 
  { "DomainName" :: (DomainName)
  , "TagsToDelete" :: (TagKeyList)
  }


newtype DeleteTagsForDomainResponse = DeleteTagsForDomainResponse 
  { 
  }


newtype DisableDomainAutoRenewRequest = DisableDomainAutoRenewRequest 
  { "DomainName" :: (DomainName)
  }


newtype DisableDomainAutoRenewResponse = DisableDomainAutoRenewResponse 
  { 
  }


-- | <p>The DisableDomainTransferLock request includes the following element.</p>
newtype DisableDomainTransferLockRequest = DisableDomainTransferLockRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>The DisableDomainTransferLock response includes the following element.</p>
newtype DisableDomainTransferLockResponse = DisableDomainTransferLockResponse 
  { "OperationId" :: (OperationId)
  }


newtype DomainAuthCode = DomainAuthCode String


newtype DomainAvailability = DomainAvailability String


-- | <p>The number of domains has exceeded the allowed threshold for the account.</p>
newtype DomainLimitExceeded = DomainLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype DomainName = DomainName String


newtype DomainStatus = DomainStatus String


newtype DomainStatusList = DomainStatusList (Array DomainStatus)


-- | <p>Information about one suggested domain name.</p>
newtype DomainSuggestion = DomainSuggestion 
  { "DomainName" :: NullOrUndefined (DomainName)
  , "Availability" :: NullOrUndefined (String)
  }


newtype DomainSuggestionsList = DomainSuggestionsList (Array DomainSuggestion)


-- | <p>Summary information about one domain.</p>
newtype DomainSummary = DomainSummary 
  { "DomainName" :: (DomainName)
  , "AutoRenew" :: NullOrUndefined (Boolean)
  , "TransferLock" :: NullOrUndefined (Boolean)
  , "Expiry" :: NullOrUndefined (Number)
  }


newtype DomainSummaryList = DomainSummaryList (Array DomainSummary)


newtype DomainTransferability = DomainTransferability 
  { "Transferable" :: NullOrUndefined (Transferable)
  }


-- | <p>The request is already in progress for the domain.</p>
newtype DuplicateRequest = DuplicateRequest 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype DurationInYears = DurationInYears Int


newtype Email = Email String


newtype EnableDomainAutoRenewRequest = EnableDomainAutoRenewRequest 
  { "DomainName" :: (DomainName)
  }


newtype EnableDomainAutoRenewResponse = EnableDomainAutoRenewResponse 
  { 
  }


-- | <p>A request to set the transfer lock for the specified domain.</p>
newtype EnableDomainTransferLockRequest = EnableDomainTransferLockRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>The EnableDomainTransferLock response includes the following elements.</p>
newtype EnableDomainTransferLockResponse = EnableDomainTransferLockResponse 
  { "OperationId" :: (OperationId)
  }


newtype ErrorMessage = ErrorMessage String


-- | <p>ExtraParam includes the following elements.</p>
newtype ExtraParam = ExtraParam 
  { "Name" :: (ExtraParamName)
  , "Value" :: (ExtraParamValue)
  }


newtype ExtraParamList = ExtraParamList (Array ExtraParam)


newtype ExtraParamName = ExtraParamName String


newtype ExtraParamValue = ExtraParamValue String


newtype FIAuthKey = FIAuthKey String


newtype GetContactReachabilityStatusRequest = GetContactReachabilityStatusRequest 
  { "DomainName'" :: NullOrUndefined (DomainName)
  }


newtype GetContactReachabilityStatusResponse = GetContactReachabilityStatusResponse 
  { "DomainName'" :: NullOrUndefined (DomainName)
  , "Status'" :: NullOrUndefined (ReachabilityStatus)
  }


-- | <p>The GetDomainDetail request includes the following element.</p>
newtype GetDomainDetailRequest = GetDomainDetailRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>The GetDomainDetail response includes the following elements.</p>
newtype GetDomainDetailResponse = GetDomainDetailResponse 
  { "DomainName" :: (DomainName)
  , "Nameservers" :: (NameserverList)
  , "AutoRenew" :: NullOrUndefined (Boolean)
  , "AdminContact" :: (ContactDetail)
  , "RegistrantContact" :: (ContactDetail)
  , "TechContact" :: (ContactDetail)
  , "AdminPrivacy" :: NullOrUndefined (Boolean)
  , "RegistrantPrivacy" :: NullOrUndefined (Boolean)
  , "TechPrivacy" :: NullOrUndefined (Boolean)
  , "RegistrarName" :: NullOrUndefined (RegistrarName)
  , "WhoIsServer" :: NullOrUndefined (RegistrarWhoIsServer)
  , "RegistrarUrl" :: NullOrUndefined (RegistrarUrl)
  , "AbuseContactEmail" :: NullOrUndefined (Email)
  , "AbuseContactPhone" :: NullOrUndefined (ContactNumber)
  , "RegistryDomainId" :: NullOrUndefined (RegistryDomainId)
  , "CreationDate" :: NullOrUndefined (Number)
  , "UpdatedDate" :: NullOrUndefined (Number)
  , "ExpirationDate" :: NullOrUndefined (Number)
  , "Reseller" :: NullOrUndefined (Reseller)
  , "DnsSec" :: NullOrUndefined (DNSSec)
  , "StatusList" :: NullOrUndefined (DomainStatusList)
  }


newtype GetDomainSuggestionsRequest = GetDomainSuggestionsRequest 
  { "DomainName" :: (DomainName)
  , "SuggestionCount" :: (Int)
  , "OnlyAvailable" :: (Boolean)
  }


newtype GetDomainSuggestionsResponse = GetDomainSuggestionsResponse 
  { "SuggestionsList" :: NullOrUndefined (DomainSuggestionsList)
  }


-- | <p>The <a>GetOperationDetail</a> request includes the following element.</p>
newtype GetOperationDetailRequest = GetOperationDetailRequest 
  { "OperationId" :: (OperationId)
  }


-- | <p>The GetOperationDetail response includes the following elements.</p>
newtype GetOperationDetailResponse = GetOperationDetailResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  , "Status" :: NullOrUndefined (OperationStatus)
  , "Message" :: NullOrUndefined (ErrorMessage)
  , "DomainName" :: NullOrUndefined (DomainName)
  , "Type" :: NullOrUndefined (OperationType)
  , "SubmittedDate" :: NullOrUndefined (Number)
  }


newtype GlueIp = GlueIp String


newtype GlueIpList = GlueIpList (Array GlueIp)


newtype HostName = HostName String


-- | <p>The requested item is not acceptable. For example, for an OperationId it might refer to the ID of an operation that is already completed. For a domain name, it might not be a valid domain name or belong to the requester account.</p>
newtype InvalidInput = InvalidInput 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype InvoiceId = InvoiceId String


newtype LangCode = LangCode String


-- | <p>The ListDomains request includes the following elements.</p>
newtype ListDomainsRequest = ListDomainsRequest 
  { "Marker" :: NullOrUndefined (PageMarker)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }


-- | <p>The ListDomains response includes the following elements.</p>
newtype ListDomainsResponse = ListDomainsResponse 
  { "Domains" :: (DomainSummaryList)
  , "NextPageMarker" :: NullOrUndefined (PageMarker)
  }


-- | <p>The ListOperations request includes the following elements.</p>
newtype ListOperationsRequest = ListOperationsRequest 
  { "Marker" :: NullOrUndefined (PageMarker)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }


-- | <p>The ListOperations response includes the following elements.</p>
newtype ListOperationsResponse = ListOperationsResponse 
  { "Operations" :: (OperationSummaryList)
  , "NextPageMarker" :: NullOrUndefined (PageMarker)
  }


-- | <p>The ListTagsForDomainRequest includes the following elements.</p>
newtype ListTagsForDomainRequest = ListTagsForDomainRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>The ListTagsForDomain response includes the following elements.</p>
newtype ListTagsForDomainResponse = ListTagsForDomainResponse 
  { "TagList" :: (TagList)
  }


-- | <p>Nameserver includes the following elements.</p>
newtype Nameserver = Nameserver 
  { "Name" :: (HostName)
  , "GlueIps" :: NullOrUndefined (GlueIpList)
  }


newtype NameserverList = NameserverList (Array Nameserver)


newtype OperationId = OperationId String


-- | <p>The number of operations or jobs running exceeded the allowed threshold for the account.</p>
newtype OperationLimitExceeded = OperationLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype OperationStatus = OperationStatus String


-- | <p>OperationSummary includes the following elements.</p>
newtype OperationSummary = OperationSummary 
  { "OperationId" :: (OperationId)
  , "Status" :: (OperationStatus)
  , "Type" :: (OperationType)
  , "SubmittedDate" :: (Number)
  }


newtype OperationSummaryList = OperationSummaryList (Array OperationSummary)


newtype OperationType = OperationType String


newtype PageMarker = PageMarker String


newtype PageMaxItems = PageMaxItems Int


newtype Price = Price Number


newtype ReachabilityStatus = ReachabilityStatus String


-- | <p>The RegisterDomain request includes the following elements.</p>
newtype RegisterDomainRequest = RegisterDomainRequest 
  { "DomainName" :: (DomainName)
  , "IdnLangCode" :: NullOrUndefined (LangCode)
  , "DurationInYears" :: (DurationInYears)
  , "AutoRenew" :: NullOrUndefined (Boolean)
  , "AdminContact" :: (ContactDetail)
  , "RegistrantContact" :: (ContactDetail)
  , "TechContact" :: (ContactDetail)
  , "PrivacyProtectAdminContact" :: NullOrUndefined (Boolean)
  , "PrivacyProtectRegistrantContact" :: NullOrUndefined (Boolean)
  , "PrivacyProtectTechContact" :: NullOrUndefined (Boolean)
  }


-- | <p>The RegisterDomain response includes the following element.</p>
newtype RegisterDomainResponse = RegisterDomainResponse 
  { "OperationId" :: (OperationId)
  }


newtype RegistrarName = RegistrarName String


newtype RegistrarUrl = RegistrarUrl String


newtype RegistrarWhoIsServer = RegistrarWhoIsServer String


newtype RegistryDomainId = RegistryDomainId String


-- | <p>A <code>RenewDomain</code> request includes the number of years that you want to renew for and the current expiration year.</p>
newtype RenewDomainRequest = RenewDomainRequest 
  { "DomainName" :: (DomainName)
  , "DurationInYears" :: NullOrUndefined (DurationInYears)
  , "CurrentExpiryYear" :: (CurrentExpiryYear)
  }


newtype RenewDomainResponse = RenewDomainResponse 
  { "OperationId" :: (OperationId)
  }


newtype Reseller = Reseller String


newtype ResendContactReachabilityEmailRequest = ResendContactReachabilityEmailRequest 
  { "DomainName'" :: NullOrUndefined (DomainName)
  }


newtype ResendContactReachabilityEmailResponse = ResendContactReachabilityEmailResponse 
  { "DomainName'" :: NullOrUndefined (DomainName)
  , "EmailAddress'" :: NullOrUndefined (Email)
  , "IsAlreadyVerified'" :: NullOrUndefined (Boolean)
  }


-- | <p>A request for the authorization code for the specified domain. To transfer a domain to another registrar, you provide this value to the new registrar.</p>
newtype RetrieveDomainAuthCodeRequest = RetrieveDomainAuthCodeRequest 
  { "DomainName" :: (DomainName)
  }


-- | <p>The RetrieveDomainAuthCode response includes the following element.</p>
newtype RetrieveDomainAuthCodeResponse = RetrieveDomainAuthCodeResponse 
  { "AuthCode" :: (DomainAuthCode)
  }


newtype State = State String


-- | <p>The top-level domain does not support this operation.</p>
newtype TLDRulesViolation = TLDRulesViolation 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Each tag includes the following elements.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array TagKey)


newtype TagList = TagList (Array Tag)


newtype TagValue = TagValue String


-- | <p>The TransferDomain request includes the following elements.</p>
newtype TransferDomainRequest = TransferDomainRequest 
  { "DomainName" :: (DomainName)
  , "IdnLangCode" :: NullOrUndefined (LangCode)
  , "DurationInYears" :: (DurationInYears)
  , "Nameservers" :: NullOrUndefined (NameserverList)
  , "AuthCode" :: NullOrUndefined (DomainAuthCode)
  , "AutoRenew" :: NullOrUndefined (Boolean)
  , "AdminContact" :: (ContactDetail)
  , "RegistrantContact" :: (ContactDetail)
  , "TechContact" :: (ContactDetail)
  , "PrivacyProtectAdminContact" :: NullOrUndefined (Boolean)
  , "PrivacyProtectRegistrantContact" :: NullOrUndefined (Boolean)
  , "PrivacyProtectTechContact" :: NullOrUndefined (Boolean)
  }


-- | <p>The TranserDomain response includes the following element.</p>
newtype TransferDomainResponse = TransferDomainResponse 
  { "OperationId" :: (OperationId)
  }


-- | <p>Whether the domain name can be transferred to Amazon Route 53.</p> <note> <p>You can transfer only domains that have a value of <code>TRANSFERABLE</code> for <code>Transferable</code>.</p> </note> <p>Valid values:</p> <dl> <dt>TRANSFERABLE</dt> <dd> <p>The domain name can be transferred to Amazon Route 53.</p> </dd> <dt>UNTRANSFERRABLE</dt> <dd> <p>The domain name can't be transferred to Amazon Route 53.</p> </dd> <dt>DONT_KNOW</dt> <dd> <p>Reserved for future use.</p> </dd> </dl>
newtype Transferable = Transferable String


-- | <p>Amazon Route 53 does not support this top-level domain (TLD).</p>
newtype UnsupportedTLD = UnsupportedTLD 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The UpdateDomainContactPrivacy request includes the following elements.</p>
newtype UpdateDomainContactPrivacyRequest = UpdateDomainContactPrivacyRequest 
  { "DomainName" :: (DomainName)
  , "AdminPrivacy" :: NullOrUndefined (Boolean)
  , "RegistrantPrivacy" :: NullOrUndefined (Boolean)
  , "TechPrivacy" :: NullOrUndefined (Boolean)
  }


-- | <p>The UpdateDomainContactPrivacy response includes the following element.</p>
newtype UpdateDomainContactPrivacyResponse = UpdateDomainContactPrivacyResponse 
  { "OperationId" :: (OperationId)
  }


-- | <p>The UpdateDomainContact request includes the following elements.</p>
newtype UpdateDomainContactRequest = UpdateDomainContactRequest 
  { "DomainName" :: (DomainName)
  , "AdminContact" :: NullOrUndefined (ContactDetail)
  , "RegistrantContact" :: NullOrUndefined (ContactDetail)
  , "TechContact" :: NullOrUndefined (ContactDetail)
  }


-- | <p>The UpdateDomainContact response includes the following element.</p>
newtype UpdateDomainContactResponse = UpdateDomainContactResponse 
  { "OperationId" :: (OperationId)
  }


-- | <p>Replaces the current set of name servers for the domain with the specified set of name servers. If you use Amazon Route 53 as your DNS service, specify the four name servers in the delegation set for the hosted zone for the domain.</p> <p>If successful, this operation returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email. </p>
newtype UpdateDomainNameserversRequest = UpdateDomainNameserversRequest 
  { "DomainName" :: (DomainName)
  , "FIAuthKey" :: NullOrUndefined (FIAuthKey)
  , "Nameservers" :: (NameserverList)
  }


-- | <p>The UpdateDomainNameservers response includes the following element.</p>
newtype UpdateDomainNameserversResponse = UpdateDomainNameserversResponse 
  { "OperationId" :: (OperationId)
  }


-- | <p>The UpdateTagsForDomainRequest includes the following elements.</p>
newtype UpdateTagsForDomainRequest = UpdateTagsForDomainRequest 
  { "DomainName" :: (DomainName)
  , "TagsToUpdate" :: NullOrUndefined (TagList)
  }


newtype UpdateTagsForDomainResponse = UpdateTagsForDomainResponse 
  { 
  }


-- | <p>The ViewBilling request includes the following elements.</p>
newtype ViewBillingRequest = ViewBillingRequest 
  { "Start" :: NullOrUndefined (Number)
  , "End" :: NullOrUndefined (Number)
  , "Marker" :: NullOrUndefined (PageMarker)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }


-- | <p>The ViewBilling response includes the following elements.</p>
newtype ViewBillingResponse = ViewBillingResponse 
  { "NextPageMarker" :: NullOrUndefined (PageMarker)
  , "BillingRecords" :: NullOrUndefined (BillingRecords)
  }


newtype ZipCode = ZipCode String
