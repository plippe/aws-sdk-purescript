

-- | <p>Amazon Route 53 API actions let you register domain names and perform related operations.</p>
module AWS.Route53Domains where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Route53Domains" :: String


-- | <p>This operation checks the availability of one domain name. Note that if the availability status of a domain is pending, you must submit another request to determine the availability of the domain name.</p>
checkDomainAvailability :: forall eff. CheckDomainAvailabilityRequest -> Aff (err :: AWS.RequestError | eff) CheckDomainAvailabilityResponse
checkDomainAvailability = AWS.request serviceName "checkDomainAvailability" 


-- | <p>Checks whether a domain name can be transferred to Amazon Route 53. </p>
checkDomainTransferability :: forall eff. CheckDomainTransferabilityRequest -> Aff (err :: AWS.RequestError | eff) CheckDomainTransferabilityResponse
checkDomainTransferability = AWS.request serviceName "checkDomainTransferability" 


-- | <p>This operation deletes the specified tags for a domain.</p> <p>All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.</p>
deleteTagsForDomain :: forall eff. DeleteTagsForDomainRequest -> Aff (err :: AWS.RequestError | eff) DeleteTagsForDomainResponse
deleteTagsForDomain = AWS.request serviceName "deleteTagsForDomain" 


-- | <p>This operation disables automatic renewal of domain registration for the specified domain.</p>
disableDomainAutoRenew :: forall eff. DisableDomainAutoRenewRequest -> Aff (err :: AWS.RequestError | eff) DisableDomainAutoRenewResponse
disableDomainAutoRenew = AWS.request serviceName "disableDomainAutoRenew" 


-- | <p>This operation removes the transfer lock on the domain (specifically the <code>clientTransferProhibited</code> status) to allow domain transfers. We recommend you refrain from performing this action unless you intend to transfer the domain to a different registrar. Successful submission returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>
disableDomainTransferLock :: forall eff. DisableDomainTransferLockRequest -> Aff (err :: AWS.RequestError | eff) DisableDomainTransferLockResponse
disableDomainTransferLock = AWS.request serviceName "disableDomainTransferLock" 


-- | <p>This operation configures Amazon Route 53 to automatically renew the specified domain before the domain registration expires. The cost of renewing your domain registration is billed to your AWS account.</p> <p>The period during which you can renew a domain name varies by TLD. For a list of TLDs and their renewal policies, see <a href="http://wiki.gandi.net/en/domains/renew#renewal_restoration_and_deletion_times">"Renewal, restoration, and deletion times"</a> on the website for our registrar partner, Gandi. Route 53 requires that you renew before the end of the renewal period that is listed on the Gandi website so we can complete processing before the deadline.</p>
enableDomainAutoRenew :: forall eff. EnableDomainAutoRenewRequest -> Aff (err :: AWS.RequestError | eff) EnableDomainAutoRenewResponse
enableDomainAutoRenew = AWS.request serviceName "enableDomainAutoRenew" 


-- | <p>This operation sets the transfer lock on the domain (specifically the <code>clientTransferProhibited</code> status) to prevent domain transfers. Successful submission returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>
enableDomainTransferLock :: forall eff. EnableDomainTransferLockRequest -> Aff (err :: AWS.RequestError | eff) EnableDomainTransferLockResponse
enableDomainTransferLock = AWS.request serviceName "enableDomainTransferLock" 


-- | <p>For operations that require confirmation that the email address for the registrant contact is valid, such as registering a new domain, this operation returns information about whether the registrant contact has responded.</p> <p>If you want us to resend the email, use the <code>ResendContactReachabilityEmail</code> operation.</p>
getContactReachabilityStatus :: forall eff. GetContactReachabilityStatusRequest -> Aff (err :: AWS.RequestError | eff) GetContactReachabilityStatusResponse
getContactReachabilityStatus = AWS.request serviceName "getContactReachabilityStatus" 


-- | <p>This operation returns detailed information about a specified domain that is associated with the current AWS account. Contact information for the domain is also returned as part of the output.</p>
getDomainDetail :: forall eff. GetDomainDetailRequest -> Aff (err :: AWS.RequestError | eff) GetDomainDetailResponse
getDomainDetail = AWS.request serviceName "getDomainDetail" 


-- | <p>The GetDomainSuggestions operation returns a list of suggested domain names given a string, which can either be a domain name or simply a word or phrase (without spaces).</p>
getDomainSuggestions :: forall eff. GetDomainSuggestionsRequest -> Aff (err :: AWS.RequestError | eff) GetDomainSuggestionsResponse
getDomainSuggestions = AWS.request serviceName "getDomainSuggestions" 


-- | <p>This operation returns the current status of an operation that is not completed.</p>
getOperationDetail :: forall eff. GetOperationDetailRequest -> Aff (err :: AWS.RequestError | eff) GetOperationDetailResponse
getOperationDetail = AWS.request serviceName "getOperationDetail" 


-- | <p>This operation returns all the domain names registered with Amazon Route 53 for the current AWS account.</p>
listDomains :: forall eff. ListDomainsRequest -> Aff (err :: AWS.RequestError | eff) ListDomainsResponse
listDomains = AWS.request serviceName "listDomains" 


-- | <p>This operation returns the operation IDs of operations that are not yet complete.</p>
listOperations :: forall eff. ListOperationsRequest -> Aff (err :: AWS.RequestError | eff) ListOperationsResponse
listOperations = AWS.request serviceName "listOperations" 


-- | <p>This operation returns all of the tags that are associated with the specified domain.</p> <p>All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.</p>
listTagsForDomain :: forall eff. ListTagsForDomainRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForDomainResponse
listTagsForDomain = AWS.request serviceName "listTagsForDomain" 


-- | <p>This operation registers a domain. Domains are registered by the AWS registrar partner, Gandi. For some top-level domains (TLDs), this operation requires extra parameters.</p> <p>When you register a domain, Amazon Route 53 does the following:</p> <ul> <li> <p>Creates a Amazon Route 53 hosted zone that has the same name as the domain. Amazon Route 53 assigns four name servers to your hosted zone and automatically updates your domain registration with the names of these name servers.</p> </li> <li> <p>Enables autorenew, so your domain registration will renew automatically each year. We'll notify you in advance of the renewal date so you can choose whether to renew the registration.</p> </li> <li> <p>Optionally enables privacy protection, so WHOIS queries return contact information for our registrar partner, Gandi, instead of the information you entered for registrant, admin, and tech contacts.</p> </li> <li> <p>If registration is successful, returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant is notified by email.</p> </li> <li> <p>Charges your AWS account an amount based on the top-level domain. For more information, see <a href="http://aws.amazon.com/route53/pricing/">Amazon Route 53 Pricing</a>.</p> </li> </ul>
registerDomain :: forall eff. RegisterDomainRequest -> Aff (err :: AWS.RequestError | eff) RegisterDomainResponse
registerDomain = AWS.request serviceName "registerDomain" 


-- | <p>This operation renews a domain for the specified number of years. The cost of renewing your domain is billed to your AWS account.</p> <p>We recommend that you renew your domain several weeks before the expiration date. Some TLD registries delete domains before the expiration date if you haven't renewed far enough in advance. For more information about renewing domain registration, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-renew.html">Renewing Registration for a Domain</a> in the Amazon Route 53 Developer Guide.</p>
renewDomain :: forall eff. RenewDomainRequest -> Aff (err :: AWS.RequestError | eff) RenewDomainResponse
renewDomain = AWS.request serviceName "renewDomain" 


-- | <p>For operations that require confirmation that the email address for the registrant contact is valid, such as registering a new domain, this operation resends the confirmation email to the current email address for the registrant contact.</p>
resendContactReachabilityEmail :: forall eff. ResendContactReachabilityEmailRequest -> Aff (err :: AWS.RequestError | eff) ResendContactReachabilityEmailResponse
resendContactReachabilityEmail = AWS.request serviceName "resendContactReachabilityEmail" 


-- | <p>This operation returns the AuthCode for the domain. To transfer a domain to another registrar, you provide this value to the new registrar.</p>
retrieveDomainAuthCode :: forall eff. RetrieveDomainAuthCodeRequest -> Aff (err :: AWS.RequestError | eff) RetrieveDomainAuthCodeResponse
retrieveDomainAuthCode = AWS.request serviceName "retrieveDomainAuthCode" 


-- | <p>This operation transfers a domain from another registrar to Amazon Route 53. When the transfer is complete, the domain is registered with the AWS registrar partner, Gandi.</p> <p>For transfer requirements, a detailed procedure, and information about viewing the status of a domain transfer, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-transfer-to-route-53.html">Transferring Registration for a Domain to Amazon Route 53</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>If the registrar for your domain is also the DNS service provider for the domain, we highly recommend that you consider transferring your DNS service to Amazon Route 53 or to another DNS service provider before you transfer your registration. Some registrars provide free DNS service when you purchase a domain registration. When you transfer the registration, the previous registrar will not renew your domain registration and could end your DNS service at any time.</p> <important> <p>If the registrar for your domain is also the DNS service provider for the domain and you don't transfer DNS service to another provider, your website, email, and the web applications associated with the domain might become unavailable.</p> </important> <p>If the transfer is successful, this method returns an operation ID that you can use to track the progress and completion of the action. If the transfer doesn't complete successfully, the domain registrant will be notified by email.</p>
transferDomain :: forall eff. TransferDomainRequest -> Aff (err :: AWS.RequestError | eff) TransferDomainResponse
transferDomain = AWS.request serviceName "transferDomain" 


-- | <p>This operation updates the contact information for a particular domain. Information for at least one contact (registrant, administrator, or technical) must be supplied for update.</p> <p>If the update is successful, this method returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>
updateDomainContact :: forall eff. UpdateDomainContactRequest -> Aff (err :: AWS.RequestError | eff) UpdateDomainContactResponse
updateDomainContact = AWS.request serviceName "updateDomainContact" 


-- | <p>This operation updates the specified domain contact's privacy setting. When the privacy option is enabled, personal information such as postal or email address is hidden from the results of a public WHOIS query. The privacy services are provided by the AWS registrar, Gandi. For more information, see the <a href="http://www.gandi.net/domain/whois/?currency=USD&amp;amp;lang=en">Gandi privacy features</a>.</p> <p>This operation only affects the privacy of the specified contact type (registrant, administrator, or tech). Successful acceptance returns an operation ID that you can use with <a>GetOperationDetail</a> to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>
updateDomainContactPrivacy :: forall eff. UpdateDomainContactPrivacyRequest -> Aff (err :: AWS.RequestError | eff) UpdateDomainContactPrivacyResponse
updateDomainContactPrivacy = AWS.request serviceName "updateDomainContactPrivacy" 


-- | <p>This operation replaces the current set of name servers for the domain with the specified set of name servers. If you use Amazon Route 53 as your DNS service, specify the four name servers in the delegation set for the hosted zone for the domain.</p> <p>If successful, this operation returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.</p>
updateDomainNameservers :: forall eff. UpdateDomainNameserversRequest -> Aff (err :: AWS.RequestError | eff) UpdateDomainNameserversResponse
updateDomainNameservers = AWS.request serviceName "updateDomainNameservers" 


-- | <p>This operation adds or updates tags for a specified domain.</p> <p>All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.</p>
updateTagsForDomain :: forall eff. UpdateTagsForDomainRequest -> Aff (err :: AWS.RequestError | eff) UpdateTagsForDomainResponse
updateTagsForDomain = AWS.request serviceName "updateTagsForDomain" 


-- | <p>Returns all the domain-related billing records for the current AWS account for a specified period</p>
viewBilling :: forall eff. ViewBillingRequest -> Aff (err :: AWS.RequestError | eff) ViewBillingResponse
viewBilling = AWS.request serviceName "viewBilling" 


newtype AddressLine = AddressLine String
derive instance newtypeAddressLine :: Newtype AddressLine _


-- | <p>Information for one billing record.</p>
newtype BillingRecord = BillingRecord 
  { "DomainName" :: NullOrUndefined (DomainName)
  , "Operation" :: NullOrUndefined (OperationType)
  , "InvoiceId" :: NullOrUndefined (InvoiceId)
  , "BillDate" :: NullOrUndefined (Number)
  , "Price" :: NullOrUndefined (Price)
  }
derive instance newtypeBillingRecord :: Newtype BillingRecord _


newtype BillingRecords = BillingRecords (Array BillingRecord)
derive instance newtypeBillingRecords :: Newtype BillingRecords _


-- | <p>The CheckDomainAvailability request contains the following elements.</p>
newtype CheckDomainAvailabilityRequest = CheckDomainAvailabilityRequest 
  { "DomainName" :: (DomainName)
  , "IdnLangCode" :: NullOrUndefined (LangCode)
  }
derive instance newtypeCheckDomainAvailabilityRequest :: Newtype CheckDomainAvailabilityRequest _


-- | <p>The CheckDomainAvailability response includes the following elements.</p>
newtype CheckDomainAvailabilityResponse = CheckDomainAvailabilityResponse 
  { "Availability" :: (DomainAvailability)
  }
derive instance newtypeCheckDomainAvailabilityResponse :: Newtype CheckDomainAvailabilityResponse _


-- | <p>The CheckDomainTransferability request contains the following elements.</p>
newtype CheckDomainTransferabilityRequest = CheckDomainTransferabilityRequest 
  { "DomainName" :: (DomainName)
  , "AuthCode" :: NullOrUndefined (DomainAuthCode)
  }
derive instance newtypeCheckDomainTransferabilityRequest :: Newtype CheckDomainTransferabilityRequest _


-- | <p>The CheckDomainTransferability response includes the following elements.</p>
newtype CheckDomainTransferabilityResponse = CheckDomainTransferabilityResponse 
  { "Transferability" :: (DomainTransferability)
  }
derive instance newtypeCheckDomainTransferabilityResponse :: Newtype CheckDomainTransferabilityResponse _


newtype City = City String
derive instance newtypeCity :: Newtype City _


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
derive instance newtypeContactDetail :: Newtype ContactDetail _


newtype ContactName = ContactName String
derive instance newtypeContactName :: Newtype ContactName _


newtype ContactNumber = ContactNumber String
derive instance newtypeContactNumber :: Newtype ContactNumber _


newtype ContactType = ContactType String
derive instance newtypeContactType :: Newtype ContactType _


newtype CountryCode = CountryCode String
derive instance newtypeCountryCode :: Newtype CountryCode _


newtype CurrentExpiryYear = CurrentExpiryYear Int
derive instance newtypeCurrentExpiryYear :: Newtype CurrentExpiryYear _


newtype DNSSec = DNSSec String
derive instance newtypeDNSSec :: Newtype DNSSec _


-- | <p>The DeleteTagsForDomainRequest includes the following elements.</p>
newtype DeleteTagsForDomainRequest = DeleteTagsForDomainRequest 
  { "DomainName" :: (DomainName)
  , "TagsToDelete" :: (TagKeyList)
  }
derive instance newtypeDeleteTagsForDomainRequest :: Newtype DeleteTagsForDomainRequest _


newtype DeleteTagsForDomainResponse = DeleteTagsForDomainResponse 
  { 
  }
derive instance newtypeDeleteTagsForDomainResponse :: Newtype DeleteTagsForDomainResponse _


newtype DisableDomainAutoRenewRequest = DisableDomainAutoRenewRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDisableDomainAutoRenewRequest :: Newtype DisableDomainAutoRenewRequest _


newtype DisableDomainAutoRenewResponse = DisableDomainAutoRenewResponse 
  { 
  }
derive instance newtypeDisableDomainAutoRenewResponse :: Newtype DisableDomainAutoRenewResponse _


-- | <p>The DisableDomainTransferLock request includes the following element.</p>
newtype DisableDomainTransferLockRequest = DisableDomainTransferLockRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeDisableDomainTransferLockRequest :: Newtype DisableDomainTransferLockRequest _


-- | <p>The DisableDomainTransferLock response includes the following element.</p>
newtype DisableDomainTransferLockResponse = DisableDomainTransferLockResponse 
  { "OperationId" :: (OperationId)
  }
derive instance newtypeDisableDomainTransferLockResponse :: Newtype DisableDomainTransferLockResponse _


newtype DomainAuthCode = DomainAuthCode String
derive instance newtypeDomainAuthCode :: Newtype DomainAuthCode _


newtype DomainAvailability = DomainAvailability String
derive instance newtypeDomainAvailability :: Newtype DomainAvailability _


-- | <p>The number of domains has exceeded the allowed threshold for the account.</p>
newtype DomainLimitExceeded = DomainLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDomainLimitExceeded :: Newtype DomainLimitExceeded _


newtype DomainName = DomainName String
derive instance newtypeDomainName :: Newtype DomainName _


newtype DomainStatus = DomainStatus String
derive instance newtypeDomainStatus :: Newtype DomainStatus _


newtype DomainStatusList = DomainStatusList (Array DomainStatus)
derive instance newtypeDomainStatusList :: Newtype DomainStatusList _


-- | <p>Information about one suggested domain name.</p>
newtype DomainSuggestion = DomainSuggestion 
  { "DomainName" :: NullOrUndefined (DomainName)
  , "Availability" :: NullOrUndefined (String)
  }
derive instance newtypeDomainSuggestion :: Newtype DomainSuggestion _


newtype DomainSuggestionsList = DomainSuggestionsList (Array DomainSuggestion)
derive instance newtypeDomainSuggestionsList :: Newtype DomainSuggestionsList _


-- | <p>Summary information about one domain.</p>
newtype DomainSummary = DomainSummary 
  { "DomainName" :: (DomainName)
  , "AutoRenew" :: NullOrUndefined (Boolean)
  , "TransferLock" :: NullOrUndefined (Boolean)
  , "Expiry" :: NullOrUndefined (Number)
  }
derive instance newtypeDomainSummary :: Newtype DomainSummary _


newtype DomainSummaryList = DomainSummaryList (Array DomainSummary)
derive instance newtypeDomainSummaryList :: Newtype DomainSummaryList _


newtype DomainTransferability = DomainTransferability 
  { "Transferable" :: NullOrUndefined (Transferable)
  }
derive instance newtypeDomainTransferability :: Newtype DomainTransferability _


-- | <p>The request is already in progress for the domain.</p>
newtype DuplicateRequest = DuplicateRequest 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDuplicateRequest :: Newtype DuplicateRequest _


newtype DurationInYears = DurationInYears Int
derive instance newtypeDurationInYears :: Newtype DurationInYears _


newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _


newtype EnableDomainAutoRenewRequest = EnableDomainAutoRenewRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeEnableDomainAutoRenewRequest :: Newtype EnableDomainAutoRenewRequest _


newtype EnableDomainAutoRenewResponse = EnableDomainAutoRenewResponse 
  { 
  }
derive instance newtypeEnableDomainAutoRenewResponse :: Newtype EnableDomainAutoRenewResponse _


-- | <p>A request to set the transfer lock for the specified domain.</p>
newtype EnableDomainTransferLockRequest = EnableDomainTransferLockRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeEnableDomainTransferLockRequest :: Newtype EnableDomainTransferLockRequest _


-- | <p>The EnableDomainTransferLock response includes the following elements.</p>
newtype EnableDomainTransferLockResponse = EnableDomainTransferLockResponse 
  { "OperationId" :: (OperationId)
  }
derive instance newtypeEnableDomainTransferLockResponse :: Newtype EnableDomainTransferLockResponse _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p>ExtraParam includes the following elements.</p>
newtype ExtraParam = ExtraParam 
  { "Name" :: (ExtraParamName)
  , "Value" :: (ExtraParamValue)
  }
derive instance newtypeExtraParam :: Newtype ExtraParam _


newtype ExtraParamList = ExtraParamList (Array ExtraParam)
derive instance newtypeExtraParamList :: Newtype ExtraParamList _


newtype ExtraParamName = ExtraParamName String
derive instance newtypeExtraParamName :: Newtype ExtraParamName _


newtype ExtraParamValue = ExtraParamValue String
derive instance newtypeExtraParamValue :: Newtype ExtraParamValue _


newtype FIAuthKey = FIAuthKey String
derive instance newtypeFIAuthKey :: Newtype FIAuthKey _


newtype GetContactReachabilityStatusRequest = GetContactReachabilityStatusRequest 
  { "DomainName'" :: NullOrUndefined (DomainName)
  }
derive instance newtypeGetContactReachabilityStatusRequest :: Newtype GetContactReachabilityStatusRequest _


newtype GetContactReachabilityStatusResponse = GetContactReachabilityStatusResponse 
  { "DomainName'" :: NullOrUndefined (DomainName)
  , "Status'" :: NullOrUndefined (ReachabilityStatus)
  }
derive instance newtypeGetContactReachabilityStatusResponse :: Newtype GetContactReachabilityStatusResponse _


-- | <p>The GetDomainDetail request includes the following element.</p>
newtype GetDomainDetailRequest = GetDomainDetailRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeGetDomainDetailRequest :: Newtype GetDomainDetailRequest _


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
derive instance newtypeGetDomainDetailResponse :: Newtype GetDomainDetailResponse _


newtype GetDomainSuggestionsRequest = GetDomainSuggestionsRequest 
  { "DomainName" :: (DomainName)
  , "SuggestionCount" :: (Int)
  , "OnlyAvailable" :: (Boolean)
  }
derive instance newtypeGetDomainSuggestionsRequest :: Newtype GetDomainSuggestionsRequest _


newtype GetDomainSuggestionsResponse = GetDomainSuggestionsResponse 
  { "SuggestionsList" :: NullOrUndefined (DomainSuggestionsList)
  }
derive instance newtypeGetDomainSuggestionsResponse :: Newtype GetDomainSuggestionsResponse _


-- | <p>The <a>GetOperationDetail</a> request includes the following element.</p>
newtype GetOperationDetailRequest = GetOperationDetailRequest 
  { "OperationId" :: (OperationId)
  }
derive instance newtypeGetOperationDetailRequest :: Newtype GetOperationDetailRequest _


-- | <p>The GetOperationDetail response includes the following elements.</p>
newtype GetOperationDetailResponse = GetOperationDetailResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  , "Status" :: NullOrUndefined (OperationStatus)
  , "Message" :: NullOrUndefined (ErrorMessage)
  , "DomainName" :: NullOrUndefined (DomainName)
  , "Type" :: NullOrUndefined (OperationType)
  , "SubmittedDate" :: NullOrUndefined (Number)
  }
derive instance newtypeGetOperationDetailResponse :: Newtype GetOperationDetailResponse _


newtype GlueIp = GlueIp String
derive instance newtypeGlueIp :: Newtype GlueIp _


newtype GlueIpList = GlueIpList (Array GlueIp)
derive instance newtypeGlueIpList :: Newtype GlueIpList _


newtype HostName = HostName String
derive instance newtypeHostName :: Newtype HostName _


-- | <p>The requested item is not acceptable. For example, for an OperationId it might refer to the ID of an operation that is already completed. For a domain name, it might not be a valid domain name or belong to the requester account.</p>
newtype InvalidInput = InvalidInput 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidInput :: Newtype InvalidInput _


newtype InvoiceId = InvoiceId String
derive instance newtypeInvoiceId :: Newtype InvoiceId _


newtype LangCode = LangCode String
derive instance newtypeLangCode :: Newtype LangCode _


-- | <p>The ListDomains request includes the following elements.</p>
newtype ListDomainsRequest = ListDomainsRequest 
  { "Marker" :: NullOrUndefined (PageMarker)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListDomainsRequest :: Newtype ListDomainsRequest _


-- | <p>The ListDomains response includes the following elements.</p>
newtype ListDomainsResponse = ListDomainsResponse 
  { "Domains" :: (DomainSummaryList)
  , "NextPageMarker" :: NullOrUndefined (PageMarker)
  }
derive instance newtypeListDomainsResponse :: Newtype ListDomainsResponse _


-- | <p>The ListOperations request includes the following elements.</p>
newtype ListOperationsRequest = ListOperationsRequest 
  { "Marker" :: NullOrUndefined (PageMarker)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListOperationsRequest :: Newtype ListOperationsRequest _


-- | <p>The ListOperations response includes the following elements.</p>
newtype ListOperationsResponse = ListOperationsResponse 
  { "Operations" :: (OperationSummaryList)
  , "NextPageMarker" :: NullOrUndefined (PageMarker)
  }
derive instance newtypeListOperationsResponse :: Newtype ListOperationsResponse _


-- | <p>The ListTagsForDomainRequest includes the following elements.</p>
newtype ListTagsForDomainRequest = ListTagsForDomainRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeListTagsForDomainRequest :: Newtype ListTagsForDomainRequest _


-- | <p>The ListTagsForDomain response includes the following elements.</p>
newtype ListTagsForDomainResponse = ListTagsForDomainResponse 
  { "TagList" :: (TagList)
  }
derive instance newtypeListTagsForDomainResponse :: Newtype ListTagsForDomainResponse _


-- | <p>Nameserver includes the following elements.</p>
newtype Nameserver = Nameserver 
  { "Name" :: (HostName)
  , "GlueIps" :: NullOrUndefined (GlueIpList)
  }
derive instance newtypeNameserver :: Newtype Nameserver _


newtype NameserverList = NameserverList (Array Nameserver)
derive instance newtypeNameserverList :: Newtype NameserverList _


newtype OperationId = OperationId String
derive instance newtypeOperationId :: Newtype OperationId _


-- | <p>The number of operations or jobs running exceeded the allowed threshold for the account.</p>
newtype OperationLimitExceeded = OperationLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeOperationLimitExceeded :: Newtype OperationLimitExceeded _


newtype OperationStatus = OperationStatus String
derive instance newtypeOperationStatus :: Newtype OperationStatus _


-- | <p>OperationSummary includes the following elements.</p>
newtype OperationSummary = OperationSummary 
  { "OperationId" :: (OperationId)
  , "Status" :: (OperationStatus)
  , "Type" :: (OperationType)
  , "SubmittedDate" :: (Number)
  }
derive instance newtypeOperationSummary :: Newtype OperationSummary _


newtype OperationSummaryList = OperationSummaryList (Array OperationSummary)
derive instance newtypeOperationSummaryList :: Newtype OperationSummaryList _


newtype OperationType = OperationType String
derive instance newtypeOperationType :: Newtype OperationType _


newtype PageMarker = PageMarker String
derive instance newtypePageMarker :: Newtype PageMarker _


newtype PageMaxItems = PageMaxItems Int
derive instance newtypePageMaxItems :: Newtype PageMaxItems _


newtype Price = Price Number
derive instance newtypePrice :: Newtype Price _


newtype ReachabilityStatus = ReachabilityStatus String
derive instance newtypeReachabilityStatus :: Newtype ReachabilityStatus _


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
derive instance newtypeRegisterDomainRequest :: Newtype RegisterDomainRequest _


-- | <p>The RegisterDomain response includes the following element.</p>
newtype RegisterDomainResponse = RegisterDomainResponse 
  { "OperationId" :: (OperationId)
  }
derive instance newtypeRegisterDomainResponse :: Newtype RegisterDomainResponse _


newtype RegistrarName = RegistrarName String
derive instance newtypeRegistrarName :: Newtype RegistrarName _


newtype RegistrarUrl = RegistrarUrl String
derive instance newtypeRegistrarUrl :: Newtype RegistrarUrl _


newtype RegistrarWhoIsServer = RegistrarWhoIsServer String
derive instance newtypeRegistrarWhoIsServer :: Newtype RegistrarWhoIsServer _


newtype RegistryDomainId = RegistryDomainId String
derive instance newtypeRegistryDomainId :: Newtype RegistryDomainId _


-- | <p>A <code>RenewDomain</code> request includes the number of years that you want to renew for and the current expiration year.</p>
newtype RenewDomainRequest = RenewDomainRequest 
  { "DomainName" :: (DomainName)
  , "DurationInYears" :: NullOrUndefined (DurationInYears)
  , "CurrentExpiryYear" :: (CurrentExpiryYear)
  }
derive instance newtypeRenewDomainRequest :: Newtype RenewDomainRequest _


newtype RenewDomainResponse = RenewDomainResponse 
  { "OperationId" :: (OperationId)
  }
derive instance newtypeRenewDomainResponse :: Newtype RenewDomainResponse _


newtype Reseller = Reseller String
derive instance newtypeReseller :: Newtype Reseller _


newtype ResendContactReachabilityEmailRequest = ResendContactReachabilityEmailRequest 
  { "DomainName'" :: NullOrUndefined (DomainName)
  }
derive instance newtypeResendContactReachabilityEmailRequest :: Newtype ResendContactReachabilityEmailRequest _


newtype ResendContactReachabilityEmailResponse = ResendContactReachabilityEmailResponse 
  { "DomainName'" :: NullOrUndefined (DomainName)
  , "EmailAddress'" :: NullOrUndefined (Email)
  , "IsAlreadyVerified'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeResendContactReachabilityEmailResponse :: Newtype ResendContactReachabilityEmailResponse _


-- | <p>A request for the authorization code for the specified domain. To transfer a domain to another registrar, you provide this value to the new registrar.</p>
newtype RetrieveDomainAuthCodeRequest = RetrieveDomainAuthCodeRequest 
  { "DomainName" :: (DomainName)
  }
derive instance newtypeRetrieveDomainAuthCodeRequest :: Newtype RetrieveDomainAuthCodeRequest _


-- | <p>The RetrieveDomainAuthCode response includes the following element.</p>
newtype RetrieveDomainAuthCodeResponse = RetrieveDomainAuthCodeResponse 
  { "AuthCode" :: (DomainAuthCode)
  }
derive instance newtypeRetrieveDomainAuthCodeResponse :: Newtype RetrieveDomainAuthCodeResponse _


newtype State = State String
derive instance newtypeState :: Newtype State _


-- | <p>The top-level domain does not support this operation.</p>
newtype TLDRulesViolation = TLDRulesViolation 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTLDRulesViolation :: Newtype TLDRulesViolation _


-- | <p>Each tag includes the following elements.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


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
derive instance newtypeTransferDomainRequest :: Newtype TransferDomainRequest _


-- | <p>The TranserDomain response includes the following element.</p>
newtype TransferDomainResponse = TransferDomainResponse 
  { "OperationId" :: (OperationId)
  }
derive instance newtypeTransferDomainResponse :: Newtype TransferDomainResponse _


-- | <p>Whether the domain name can be transferred to Amazon Route 53.</p> <note> <p>You can transfer only domains that have a value of <code>TRANSFERABLE</code> for <code>Transferable</code>.</p> </note> <p>Valid values:</p> <dl> <dt>TRANSFERABLE</dt> <dd> <p>The domain name can be transferred to Amazon Route 53.</p> </dd> <dt>UNTRANSFERRABLE</dt> <dd> <p>The domain name can't be transferred to Amazon Route 53.</p> </dd> <dt>DONT_KNOW</dt> <dd> <p>Reserved for future use.</p> </dd> </dl>
newtype Transferable = Transferable String
derive instance newtypeTransferable :: Newtype Transferable _


-- | <p>Amazon Route 53 does not support this top-level domain (TLD).</p>
newtype UnsupportedTLD = UnsupportedTLD 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnsupportedTLD :: Newtype UnsupportedTLD _


-- | <p>The UpdateDomainContactPrivacy request includes the following elements.</p>
newtype UpdateDomainContactPrivacyRequest = UpdateDomainContactPrivacyRequest 
  { "DomainName" :: (DomainName)
  , "AdminPrivacy" :: NullOrUndefined (Boolean)
  , "RegistrantPrivacy" :: NullOrUndefined (Boolean)
  , "TechPrivacy" :: NullOrUndefined (Boolean)
  }
derive instance newtypeUpdateDomainContactPrivacyRequest :: Newtype UpdateDomainContactPrivacyRequest _


-- | <p>The UpdateDomainContactPrivacy response includes the following element.</p>
newtype UpdateDomainContactPrivacyResponse = UpdateDomainContactPrivacyResponse 
  { "OperationId" :: (OperationId)
  }
derive instance newtypeUpdateDomainContactPrivacyResponse :: Newtype UpdateDomainContactPrivacyResponse _


-- | <p>The UpdateDomainContact request includes the following elements.</p>
newtype UpdateDomainContactRequest = UpdateDomainContactRequest 
  { "DomainName" :: (DomainName)
  , "AdminContact" :: NullOrUndefined (ContactDetail)
  , "RegistrantContact" :: NullOrUndefined (ContactDetail)
  , "TechContact" :: NullOrUndefined (ContactDetail)
  }
derive instance newtypeUpdateDomainContactRequest :: Newtype UpdateDomainContactRequest _


-- | <p>The UpdateDomainContact response includes the following element.</p>
newtype UpdateDomainContactResponse = UpdateDomainContactResponse 
  { "OperationId" :: (OperationId)
  }
derive instance newtypeUpdateDomainContactResponse :: Newtype UpdateDomainContactResponse _


-- | <p>Replaces the current set of name servers for the domain with the specified set of name servers. If you use Amazon Route 53 as your DNS service, specify the four name servers in the delegation set for the hosted zone for the domain.</p> <p>If successful, this operation returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email. </p>
newtype UpdateDomainNameserversRequest = UpdateDomainNameserversRequest 
  { "DomainName" :: (DomainName)
  , "FIAuthKey" :: NullOrUndefined (FIAuthKey)
  , "Nameservers" :: (NameserverList)
  }
derive instance newtypeUpdateDomainNameserversRequest :: Newtype UpdateDomainNameserversRequest _


-- | <p>The UpdateDomainNameservers response includes the following element.</p>
newtype UpdateDomainNameserversResponse = UpdateDomainNameserversResponse 
  { "OperationId" :: (OperationId)
  }
derive instance newtypeUpdateDomainNameserversResponse :: Newtype UpdateDomainNameserversResponse _


-- | <p>The UpdateTagsForDomainRequest includes the following elements.</p>
newtype UpdateTagsForDomainRequest = UpdateTagsForDomainRequest 
  { "DomainName" :: (DomainName)
  , "TagsToUpdate" :: NullOrUndefined (TagList)
  }
derive instance newtypeUpdateTagsForDomainRequest :: Newtype UpdateTagsForDomainRequest _


newtype UpdateTagsForDomainResponse = UpdateTagsForDomainResponse 
  { 
  }
derive instance newtypeUpdateTagsForDomainResponse :: Newtype UpdateTagsForDomainResponse _


-- | <p>The ViewBilling request includes the following elements.</p>
newtype ViewBillingRequest = ViewBillingRequest 
  { "Start" :: NullOrUndefined (Number)
  , "End" :: NullOrUndefined (Number)
  , "Marker" :: NullOrUndefined (PageMarker)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeViewBillingRequest :: Newtype ViewBillingRequest _


-- | <p>The ViewBilling response includes the following elements.</p>
newtype ViewBillingResponse = ViewBillingResponse 
  { "NextPageMarker" :: NullOrUndefined (PageMarker)
  , "BillingRecords" :: NullOrUndefined (BillingRecords)
  }
derive instance newtypeViewBillingResponse :: Newtype ViewBillingResponse _


newtype ZipCode = ZipCode String
derive instance newtypeZipCode :: Newtype ZipCode _
