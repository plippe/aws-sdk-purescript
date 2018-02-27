

-- | <fullname>AWS Certificate Manager</fullname> <p>Welcome to the AWS Certificate Manager (ACM) API documentation.</p> <p>You can use ACM to manage SSL/TLS certificates for your AWS-based websites and applications. For general information about using ACM, see the <a href="http://docs.aws.amazon.com/acm/latest/userguide/"> <i>AWS Certificate Manager User Guide</i> </a>.</p>
module AWS.ACM where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ACM" :: String


-- | <p>Adds one or more tags to an ACM Certificate. Tags are labels that you can use to identify and organize your AWS resources. Each tag consists of a <code>key</code> and an optional <code>value</code>. You specify the certificate on input by its Amazon Resource Name (ARN). You specify the tag by using a key-value pair. </p> <p>You can apply a tag to just one certificate if you want to identify a specific characteristic of that certificate, or you can apply the same tag to multiple certificates if you want to filter for a common relationship among those certificates. Similarly, you can apply the same tag to multiple resources if you want to specify a relationship among those resources. For example, you can add the same tag to an ACM Certificate and an Elastic Load Balancing load balancer to indicate that they are both used by the same website. For more information, see <a href="http://docs.aws.amazon.com/acm/latest/userguide/tags.html">Tagging ACM Certificates</a>. </p> <p>To remove one or more tags, use the <a>RemoveTagsFromCertificate</a> action. To view all of the tags that have been applied to the certificate, use the <a>ListTagsForCertificate</a> action. </p>
addTagsToCertificate :: forall eff. AddTagsToCertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
addTagsToCertificate = AWS.request serviceName "addTagsToCertificate" 


-- | <p>Deletes a certificate and its associated private key. If this action succeeds, the certificate no longer appears in the list that can be displayed by calling the <a>ListCertificates</a> action or be retrieved by calling the <a>GetCertificate</a> action. The certificate will not be available for use by AWS services integrated with ACM. </p> <note> <p>You cannot delete an ACM Certificate that is being used by another AWS service. To delete a certificate that is in use, the certificate association must first be removed.</p> </note>
deleteCertificate :: forall eff. DeleteCertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteCertificate = AWS.request serviceName "deleteCertificate" 


-- | <p>Returns detailed metadata about the specified ACM Certificate.</p>
describeCertificate :: forall eff. DescribeCertificateRequest -> Aff (err :: AWS.RequestError | eff) DescribeCertificateResponse
describeCertificate = AWS.request serviceName "describeCertificate" 


-- | <p>Retrieves a certificate specified by an ARN and its certificate chain . The chain is an ordered list of certificates that contains the end entity certificate, intermediate certificates of subordinate CAs, and the root certificate in that order. The certificate and certificate chain are base64 encoded. If you want to decode the certificate to see the individual fields, you can use OpenSSL.</p>
getCertificate :: forall eff. GetCertificateRequest -> Aff (err :: AWS.RequestError | eff) GetCertificateResponse
getCertificate = AWS.request serviceName "getCertificate" 


-- | <p>Imports a certificate into AWS Certificate Manager (ACM) to use with services that are integrated with ACM. Note that <a href="http://docs.aws.amazon.com/acm/latest/userguide/acm-services.html">integrated services</a> allow only certificate types and keys they support to be associated with their resources. Further, their support differs depending on whether the certificate is imported into IAM or into ACM. For more information, see the documentation for each service. For more information about importing certificates into ACM, see <a href="http://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html">Importing Certificates</a> in the <i>AWS Certificate Manager User Guide</i>. </p> <note> <p>ACM does not provide <a href="http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html">managed renewal</a> for certificates that you import.</p> </note> <p>Note the following guidelines when importing third party certificates:</p> <ul> <li> <p>You must enter the private key that matches the certificate you are importing.</p> </li> <li> <p>The private key must be unencrypted. You cannot import a private key that is protected by a password or a passphrase.</p> </li> <li> <p>If the certificate you are importing is not self-signed, you must enter its certificate chain.</p> </li> <li> <p>If a certificate chain is included, the issuer must be the subject of one of the certificates in the chain.</p> </li> <li> <p>The certificate, private key, and certificate chain must be PEM-encoded.</p> </li> <li> <p>The current time must be between the <code>Not Before</code> and <code>Not After</code> certificate fields.</p> </li> <li> <p>The <code>Issuer</code> field must not be empty.</p> </li> <li> <p>The OCSP authority URL, if present, must not exceed 1000 characters.</p> </li> <li> <p>To import a new certificate, omit the <code>CertificateArn</code> argument. Include this argument only when you want to replace a previously imported certificate.</p> </li> <li> <p>When you import a certificate by using the CLI or one of the SDKs, you must specify the certificate, the certificate chain, and the private key by their file names preceded by <code>file://</code>. For example, you can specify a certificate saved in the <code>C:\temp</code> folder as <code>file://C:\temp\certificate_to_import.pem</code>. If you are making an HTTP or HTTPS Query request, include these arguments as BLOBs. </p> </li> </ul> <p>This operation returns the <a href="http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html">Amazon Resource Name (ARN)</a> of the imported certificate.</p>
importCertificate :: forall eff. ImportCertificateRequest -> Aff (err :: AWS.RequestError | eff) ImportCertificateResponse
importCertificate = AWS.request serviceName "importCertificate" 


-- | <p>Retrieves a list of certificate ARNs and domain names. You can request that only certificates that match a specific status be listed. You can also filter by specific attributes of the certificate. </p>
listCertificates :: forall eff. ListCertificatesRequest -> Aff (err :: AWS.RequestError | eff) ListCertificatesResponse
listCertificates = AWS.request serviceName "listCertificates" 


-- | <p>Lists the tags that have been applied to the ACM Certificate. Use the certificate's Amazon Resource Name (ARN) to specify the certificate. To add a tag to an ACM Certificate, use the <a>AddTagsToCertificate</a> action. To delete a tag, use the <a>RemoveTagsFromCertificate</a> action. </p>
listTagsForCertificate :: forall eff. ListTagsForCertificateRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForCertificateResponse
listTagsForCertificate = AWS.request serviceName "listTagsForCertificate" 


-- | <p>Remove one or more tags from an ACM Certificate. A tag consists of a key-value pair. If you do not specify the value portion of the tag when calling this function, the tag will be removed regardless of value. If you specify a value, the tag is removed only if it is associated with the specified value. </p> <p>To add tags to a certificate, use the <a>AddTagsToCertificate</a> action. To view all of the tags that have been applied to a specific ACM Certificate, use the <a>ListTagsForCertificate</a> action. </p>
removeTagsFromCertificate :: forall eff. RemoveTagsFromCertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
removeTagsFromCertificate = AWS.request serviceName "removeTagsFromCertificate" 


-- | <p>Requests an ACM Certificate for use with other AWS services. To request an ACM Certificate, you must specify the fully qualified domain name (FQDN) for your site in the <code>DomainName</code> parameter. You can also specify additional FQDNs in the <code>SubjectAlternativeNames</code> parameter if users can reach your site by using other names. </p> <p>For each domain name you specify, email is sent to the domain owner to request approval to issue the certificate. Email is sent to three registered contact addresses in the WHOIS database and to five common system administration addresses formed from the <code>DomainName</code> you enter or the optional <code>ValidationDomain</code> parameter. For more information, see <a href="http://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate.html">Validate Domain Ownership</a>. </p> <p>After receiving approval from the domain owner, the ACM Certificate is issued. For more information, see the <a href="http://docs.aws.amazon.com/acm/latest/userguide/">AWS Certificate Manager User Guide</a>. </p>
requestCertificate :: forall eff. RequestCertificateRequest -> Aff (err :: AWS.RequestError | eff) RequestCertificateResponse
requestCertificate = AWS.request serviceName "requestCertificate" 


-- | <p>Resends the email that requests domain ownership validation. The domain owner or an authorized representative must approve the ACM Certificate before it can be issued. The certificate can be approved by clicking a link in the mail to navigate to the Amazon certificate approval website and then clicking <b>I Approve</b>. However, the validation email can be blocked by spam filters. Therefore, if you do not receive the original mail, you can request that the mail be resent within 72 hours of requesting the ACM Certificate. If more than 72 hours have elapsed since your original request or since your last attempt to resend validation mail, you must request a new certificate. For more information about setting up your contact email addresses, see <a href="http://docs.aws.amazon.com/acm/latest/userguide/setup-email.html">Configure Email for your Domain</a>. </p>
resendValidationEmail :: forall eff. ResendValidationEmailRequest -> Aff (err :: AWS.RequestError | eff) Unit
resendValidationEmail = AWS.request serviceName "resendValidationEmail" 


newtype AddTagsToCertificateRequest = AddTagsToCertificateRequest 
  { "CertificateArn" :: (Arn)
  , "Tags" :: (TagList)
  }
derive instance newtypeAddTagsToCertificateRequest :: Newtype AddTagsToCertificateRequest _


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _


newtype CertificateBody = CertificateBody String
derive instance newtypeCertificateBody :: Newtype CertificateBody _


newtype CertificateBodyBlob = CertificateBodyBlob String
derive instance newtypeCertificateBodyBlob :: Newtype CertificateBodyBlob _


newtype CertificateChain = CertificateChain String
derive instance newtypeCertificateChain :: Newtype CertificateChain _


newtype CertificateChainBlob = CertificateChainBlob String
derive instance newtypeCertificateChainBlob :: Newtype CertificateChainBlob _


-- | <p>Contains metadata about an ACM certificate. This structure is returned in the response to a <a>DescribeCertificate</a> request. </p>
newtype CertificateDetail = CertificateDetail 
  { "CertificateArn" :: NullOrUndefined (Arn)
  , "DomainName" :: NullOrUndefined (DomainNameString)
  , "SubjectAlternativeNames" :: NullOrUndefined (DomainList)
  , "DomainValidationOptions" :: NullOrUndefined (DomainValidationList)
  , "Serial" :: NullOrUndefined (String)
  , "Subject" :: NullOrUndefined (String)
  , "Issuer" :: NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined (TStamp)
  , "IssuedAt" :: NullOrUndefined (TStamp)
  , "ImportedAt" :: NullOrUndefined (TStamp)
  , "Status" :: NullOrUndefined (CertificateStatus)
  , "RevokedAt" :: NullOrUndefined (TStamp)
  , "RevocationReason" :: NullOrUndefined (RevocationReason)
  , "NotBefore" :: NullOrUndefined (TStamp)
  , "NotAfter" :: NullOrUndefined (TStamp)
  , "KeyAlgorithm" :: NullOrUndefined (KeyAlgorithm)
  , "SignatureAlgorithm" :: NullOrUndefined (String)
  , "InUseBy" :: NullOrUndefined (InUseList)
  , "FailureReason" :: NullOrUndefined (FailureReason)
  , "Type" :: NullOrUndefined (CertificateType)
  , "RenewalSummary" :: NullOrUndefined (RenewalSummary)
  , "KeyUsages" :: NullOrUndefined (KeyUsageList)
  , "ExtendedKeyUsages" :: NullOrUndefined (ExtendedKeyUsageList)
  }
derive instance newtypeCertificateDetail :: Newtype CertificateDetail _


newtype CertificateStatus = CertificateStatus String
derive instance newtypeCertificateStatus :: Newtype CertificateStatus _


newtype CertificateStatuses = CertificateStatuses (Array CertificateStatus)
derive instance newtypeCertificateStatuses :: Newtype CertificateStatuses _


-- | <p>This structure is returned in the response object of <a>ListCertificates</a> action. </p>
newtype CertificateSummary = CertificateSummary 
  { "CertificateArn" :: NullOrUndefined (Arn)
  , "DomainName" :: NullOrUndefined (DomainNameString)
  }
derive instance newtypeCertificateSummary :: Newtype CertificateSummary _


newtype CertificateSummaryList = CertificateSummaryList (Array CertificateSummary)
derive instance newtypeCertificateSummaryList :: Newtype CertificateSummaryList _


newtype CertificateType = CertificateType String
derive instance newtypeCertificateType :: Newtype CertificateType _


newtype DeleteCertificateRequest = DeleteCertificateRequest 
  { "CertificateArn" :: (Arn)
  }
derive instance newtypeDeleteCertificateRequest :: Newtype DeleteCertificateRequest _


newtype DescribeCertificateRequest = DescribeCertificateRequest 
  { "CertificateArn" :: (Arn)
  }
derive instance newtypeDescribeCertificateRequest :: Newtype DescribeCertificateRequest _


newtype DescribeCertificateResponse = DescribeCertificateResponse 
  { "Certificate" :: NullOrUndefined (CertificateDetail)
  }
derive instance newtypeDescribeCertificateResponse :: Newtype DescribeCertificateResponse _


newtype DomainList = DomainList (Array DomainNameString)
derive instance newtypeDomainList :: Newtype DomainList _


newtype DomainNameString = DomainNameString String
derive instance newtypeDomainNameString :: Newtype DomainNameString _


newtype DomainStatus = DomainStatus String
derive instance newtypeDomainStatus :: Newtype DomainStatus _


-- | <p>Contains information about the validation of each domain name in the certificate.</p>
newtype DomainValidation = DomainValidation 
  { "DomainName" :: (DomainNameString)
  , "ValidationEmails" :: NullOrUndefined (ValidationEmailList)
  , "ValidationDomain" :: NullOrUndefined (DomainNameString)
  , "ValidationStatus" :: NullOrUndefined (DomainStatus)
  , "ResourceRecord" :: NullOrUndefined (ResourceRecord)
  , "ValidationMethod" :: NullOrUndefined (ValidationMethod)
  }
derive instance newtypeDomainValidation :: Newtype DomainValidation _


newtype DomainValidationList = DomainValidationList (Array DomainValidation)
derive instance newtypeDomainValidationList :: Newtype DomainValidationList _


-- | <p>Contains information about the domain names that you want ACM to use to send you emails that enable you to validate domain ownership.</p>
newtype DomainValidationOption = DomainValidationOption 
  { "DomainName" :: (DomainNameString)
  , "ValidationDomain" :: (DomainNameString)
  }
derive instance newtypeDomainValidationOption :: Newtype DomainValidationOption _


newtype DomainValidationOptionList = DomainValidationOptionList (Array DomainValidationOption)
derive instance newtypeDomainValidationOptionList :: Newtype DomainValidationOptionList _


-- | <p>The Extended Key Usage X.509 v3 extension defines one or more purposes for which the public key can be used. This is in addition to or in place of the basic purposes specified by the Key Usage extension. </p>
newtype ExtendedKeyUsage = ExtendedKeyUsage 
  { "Name" :: NullOrUndefined (ExtendedKeyUsageName)
  , "OID" :: NullOrUndefined (String)
  }
derive instance newtypeExtendedKeyUsage :: Newtype ExtendedKeyUsage _


newtype ExtendedKeyUsageFilterList = ExtendedKeyUsageFilterList (Array ExtendedKeyUsageName)
derive instance newtypeExtendedKeyUsageFilterList :: Newtype ExtendedKeyUsageFilterList _


newtype ExtendedKeyUsageList = ExtendedKeyUsageList (Array ExtendedKeyUsage)
derive instance newtypeExtendedKeyUsageList :: Newtype ExtendedKeyUsageList _


newtype ExtendedKeyUsageName = ExtendedKeyUsageName String
derive instance newtypeExtendedKeyUsageName :: Newtype ExtendedKeyUsageName _


newtype FailureReason = FailureReason String
derive instance newtypeFailureReason :: Newtype FailureReason _


-- | <p>This structure can be used in the <a>ListCertificates</a> action to filter the output of the certificate list. </p>
newtype Filters = Filters 
  { "ExtendedKeyUsage'" :: NullOrUndefined (ExtendedKeyUsageFilterList)
  , "KeyUsage'" :: NullOrUndefined (KeyUsageFilterList)
  , "KeyTypes'" :: NullOrUndefined (KeyAlgorithmList)
  }
derive instance newtypeFilters :: Newtype Filters _


newtype GetCertificateRequest = GetCertificateRequest 
  { "CertificateArn" :: (Arn)
  }
derive instance newtypeGetCertificateRequest :: Newtype GetCertificateRequest _


newtype GetCertificateResponse = GetCertificateResponse 
  { "Certificate" :: NullOrUndefined (CertificateBody)
  , "CertificateChain" :: NullOrUndefined (CertificateChain)
  }
derive instance newtypeGetCertificateResponse :: Newtype GetCertificateResponse _


newtype IdempotencyToken = IdempotencyToken String
derive instance newtypeIdempotencyToken :: Newtype IdempotencyToken _


newtype ImportCertificateRequest = ImportCertificateRequest 
  { "CertificateArn" :: NullOrUndefined (Arn)
  , "Certificate" :: (CertificateBodyBlob)
  , "PrivateKey" :: (PrivateKeyBlob)
  , "CertificateChain" :: NullOrUndefined (CertificateChainBlob)
  }
derive instance newtypeImportCertificateRequest :: Newtype ImportCertificateRequest _


newtype ImportCertificateResponse = ImportCertificateResponse 
  { "CertificateArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeImportCertificateResponse :: Newtype ImportCertificateResponse _


newtype InUseList = InUseList (Array String)
derive instance newtypeInUseList :: Newtype InUseList _


-- | <p>The requested Amazon Resource Name (ARN) does not refer to an existing resource.</p>
newtype InvalidArnException = InvalidArnException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidArnException :: Newtype InvalidArnException _


-- | <p>One or more values in the <a>DomainValidationOption</a> structure is incorrect.</p>
newtype InvalidDomainValidationOptionsException = InvalidDomainValidationOptionsException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidDomainValidationOptionsException :: Newtype InvalidDomainValidationOptionsException _


-- | <p>Processing has reached an invalid state. For example, this exception can occur if the specified domain is not using email validation, or the current certificate status does not permit the requested operation. See the exception message returned by ACM to determine which state is not valid.</p>
newtype InvalidStateException = InvalidStateException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidStateException :: Newtype InvalidStateException _


-- | <p>One or both of the values that make up the key-value pair is not valid. For example, you cannot specify a tag value that begins with <code>aws:</code>.</p>
newtype InvalidTagException = InvalidTagException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidTagException :: Newtype InvalidTagException _


newtype KeyAlgorithm = KeyAlgorithm String
derive instance newtypeKeyAlgorithm :: Newtype KeyAlgorithm _


newtype KeyAlgorithmList = KeyAlgorithmList (Array KeyAlgorithm)
derive instance newtypeKeyAlgorithmList :: Newtype KeyAlgorithmList _


-- | <p>The Key Usage X.509 v3 extension defines the purpose of the public key contained in the certificate.</p>
newtype KeyUsage = KeyUsage 
  { "Name" :: NullOrUndefined (KeyUsageName)
  }
derive instance newtypeKeyUsage :: Newtype KeyUsage _


newtype KeyUsageFilterList = KeyUsageFilterList (Array KeyUsageName)
derive instance newtypeKeyUsageFilterList :: Newtype KeyUsageFilterList _


newtype KeyUsageList = KeyUsageList (Array KeyUsage)
derive instance newtypeKeyUsageList :: Newtype KeyUsageList _


newtype KeyUsageName = KeyUsageName String
derive instance newtypeKeyUsageName :: Newtype KeyUsageName _


-- | <p>An ACM limit has been exceeded. For example, you may have input more domains than are allowed or you've requested too many certificates for your account. See the exception message returned by ACM to determine which limit you have violated. For more information about ACM limits, see the <a href="http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html">Limits</a> topic.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListCertificatesRequest = ListCertificatesRequest 
  { "CertificateStatuses" :: NullOrUndefined (CertificateStatuses)
  , "Includes" :: NullOrUndefined (Filters)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxItems" :: NullOrUndefined (MaxItems)
  }
derive instance newtypeListCertificatesRequest :: Newtype ListCertificatesRequest _


newtype ListCertificatesResponse = ListCertificatesResponse 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "CertificateSummaryList" :: NullOrUndefined (CertificateSummaryList)
  }
derive instance newtypeListCertificatesResponse :: Newtype ListCertificatesResponse _


newtype ListTagsForCertificateRequest = ListTagsForCertificateRequest 
  { "CertificateArn" :: (Arn)
  }
derive instance newtypeListTagsForCertificateRequest :: Newtype ListTagsForCertificateRequest _


newtype ListTagsForCertificateResponse = ListTagsForCertificateResponse 
  { "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeListTagsForCertificateResponse :: Newtype ListTagsForCertificateResponse _


newtype MaxItems = MaxItems Int
derive instance newtypeMaxItems :: Newtype MaxItems _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


newtype PrivateKeyBlob = PrivateKeyBlob String
derive instance newtypePrivateKeyBlob :: Newtype PrivateKeyBlob _


newtype RecordType = RecordType String
derive instance newtypeRecordType :: Newtype RecordType _


newtype RemoveTagsFromCertificateRequest = RemoveTagsFromCertificateRequest 
  { "CertificateArn" :: (Arn)
  , "Tags" :: (TagList)
  }
derive instance newtypeRemoveTagsFromCertificateRequest :: Newtype RemoveTagsFromCertificateRequest _


newtype RenewalStatus = RenewalStatus String
derive instance newtypeRenewalStatus :: Newtype RenewalStatus _


-- | <p>Contains information about the status of ACM's <a href="http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html">managed renewal</a> for the certificate. This structure exists only when the certificate type is <code>AMAZON_ISSUED</code>.</p>
newtype RenewalSummary = RenewalSummary 
  { "RenewalStatus" :: (RenewalStatus)
  , "DomainValidationOptions" :: (DomainValidationList)
  }
derive instance newtypeRenewalSummary :: Newtype RenewalSummary _


newtype RequestCertificateRequest = RequestCertificateRequest 
  { "DomainName" :: (DomainNameString)
  , "ValidationMethod" :: NullOrUndefined (ValidationMethod)
  , "SubjectAlternativeNames" :: NullOrUndefined (DomainList)
  , "IdempotencyToken" :: NullOrUndefined (IdempotencyToken)
  , "DomainValidationOptions" :: NullOrUndefined (DomainValidationOptionList)
  }
derive instance newtypeRequestCertificateRequest :: Newtype RequestCertificateRequest _


newtype RequestCertificateResponse = RequestCertificateResponse 
  { "CertificateArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeRequestCertificateResponse :: Newtype RequestCertificateResponse _


-- | <p>The certificate request is in process and the certificate in your account has not yet been issued.</p>
newtype RequestInProgressException = RequestInProgressException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeRequestInProgressException :: Newtype RequestInProgressException _


newtype ResendValidationEmailRequest = ResendValidationEmailRequest 
  { "CertificateArn" :: (Arn)
  , "Domain" :: (DomainNameString)
  , "ValidationDomain" :: (DomainNameString)
  }
derive instance newtypeResendValidationEmailRequest :: Newtype ResendValidationEmailRequest _


-- | <p>The certificate is in use by another AWS service in the caller's account. Remove the association and try again.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


-- | <p>The specified certificate cannot be found in the caller's account, or the caller's account cannot be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>Contains a DNS record value that you can use to can use to validate ownership or control of a domain. This is used by the <a>DescribeCertificate</a> action. </p>
newtype ResourceRecord = ResourceRecord 
  { "Name" :: (String)
  , "Type" :: (RecordType)
  , "Value" :: (String)
  }
derive instance newtypeResourceRecord :: Newtype ResourceRecord _


newtype RevocationReason = RevocationReason String
derive instance newtypeRevocationReason :: Newtype RevocationReason _


newtype TStamp = TStamp Number
derive instance newtypeTStamp :: Newtype TStamp _


-- | <p>A key-value pair that identifies or specifies metadata about an ACM resource.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


-- | <p>The request contains too many tags. Try the request again with fewer tags.</p>
newtype TooManyTagsException = TooManyTagsException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeTooManyTagsException :: Newtype TooManyTagsException _


newtype ValidationEmailList = ValidationEmailList (Array String)
derive instance newtypeValidationEmailList :: Newtype ValidationEmailList _


newtype ValidationMethod = ValidationMethod String
derive instance newtypeValidationMethod :: Newtype ValidationMethod _
