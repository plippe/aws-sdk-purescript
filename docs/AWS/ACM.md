## Module AWS.ACM

<fullname>AWS Certificate Manager</fullname> <p>Welcome to the AWS Certificate Manager (ACM) API documentation.</p> <p>You can use ACM to manage SSL/TLS certificates for your AWS-based websites and applications. For general information about using ACM, see the <a href="http://docs.aws.amazon.com/acm/latest/userguide/"> <i>AWS Certificate Manager User Guide</i> </a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addTagsToCertificate`

``` purescript
addTagsToCertificate :: forall eff. AddTagsToCertificateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds one or more tags to an ACM Certificate. Tags are labels that you can use to identify and organize your AWS resources. Each tag consists of a <code>key</code> and an optional <code>value</code>. You specify the certificate on input by its Amazon Resource Name (ARN). You specify the tag by using a key-value pair. </p> <p>You can apply a tag to just one certificate if you want to identify a specific characteristic of that certificate, or you can apply the same tag to multiple certificates if you want to filter for a common relationship among those certificates. Similarly, you can apply the same tag to multiple resources if you want to specify a relationship among those resources. For example, you can add the same tag to an ACM Certificate and an Elastic Load Balancing load balancer to indicate that they are both used by the same website. For more information, see <a href="http://docs.aws.amazon.com/acm/latest/userguide/tags.html">Tagging ACM Certificates</a>. </p> <p>To remove one or more tags, use the <a>RemoveTagsFromCertificate</a> action. To view all of the tags that have been applied to the certificate, use the <a>ListTagsForCertificate</a> action. </p>

#### `deleteCertificate`

``` purescript
deleteCertificate :: forall eff. DeleteCertificateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a certificate and its associated private key. If this action succeeds, the certificate no longer appears in the list that can be displayed by calling the <a>ListCertificates</a> action or be retrieved by calling the <a>GetCertificate</a> action. The certificate will not be available for use by AWS services integrated with ACM. </p> <note> <p>You cannot delete an ACM Certificate that is being used by another AWS service. To delete a certificate that is in use, the certificate association must first be removed.</p> </note>

#### `describeCertificate`

``` purescript
describeCertificate :: forall eff. DescribeCertificateRequest -> Aff (err :: RequestError | eff) DescribeCertificateResponse
```

<p>Returns detailed metadata about the specified ACM Certificate.</p>

#### `getCertificate`

``` purescript
getCertificate :: forall eff. GetCertificateRequest -> Aff (err :: RequestError | eff) GetCertificateResponse
```

<p>Retrieves a certificate specified by an ARN and its certificate chain . The chain is an ordered list of certificates that contains the end entity certificate, intermediate certificates of subordinate CAs, and the root certificate in that order. The certificate and certificate chain are base64 encoded. If you want to decode the certificate to see the individual fields, you can use OpenSSL.</p>

#### `importCertificate`

``` purescript
importCertificate :: forall eff. ImportCertificateRequest -> Aff (err :: RequestError | eff) ImportCertificateResponse
```

<p>Imports a certificate into AWS Certificate Manager (ACM) to use with services that are integrated with ACM. Note that <a href="http://docs.aws.amazon.com/acm/latest/userguide/acm-services.html">integrated services</a> allow only certificate types and keys they support to be associated with their resources. Further, their support differs depending on whether the certificate is imported into IAM or into ACM. For more information, see the documentation for each service. For more information about importing certificates into ACM, see <a href="http://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html">Importing Certificates</a> in the <i>AWS Certificate Manager User Guide</i>. </p> <note> <p>ACM does not provide <a href="http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html">managed renewal</a> for certificates that you import.</p> </note> <p>Note the following guidelines when importing third party certificates:</p> <ul> <li> <p>You must enter the private key that matches the certificate you are importing.</p> </li> <li> <p>The private key must be unencrypted. You cannot import a private key that is protected by a password or a passphrase.</p> </li> <li> <p>If the certificate you are importing is not self-signed, you must enter its certificate chain.</p> </li> <li> <p>If a certificate chain is included, the issuer must be the subject of one of the certificates in the chain.</p> </li> <li> <p>The certificate, private key, and certificate chain must be PEM-encoded.</p> </li> <li> <p>The current time must be between the <code>Not Before</code> and <code>Not After</code> certificate fields.</p> </li> <li> <p>The <code>Issuer</code> field must not be empty.</p> </li> <li> <p>The OCSP authority URL, if present, must not exceed 1000 characters.</p> </li> <li> <p>To import a new certificate, omit the <code>CertificateArn</code> argument. Include this argument only when you want to replace a previously imported certificate.</p> </li> <li> <p>When you import a certificate by using the CLI or one of the SDKs, you must specify the certificate, the certificate chain, and the private key by their file names preceded by <code>file://</code>. For example, you can specify a certificate saved in the <code>C:\temp</code> folder as <code>file://C:\temp\certificate_to_import.pem</code>. If you are making an HTTP or HTTPS Query request, include these arguments as BLOBs. </p> </li> </ul> <p>This operation returns the <a href="http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html">Amazon Resource Name (ARN)</a> of the imported certificate.</p>

#### `listCertificates`

``` purescript
listCertificates :: forall eff. ListCertificatesRequest -> Aff (err :: RequestError | eff) ListCertificatesResponse
```

<p>Retrieves a list of certificate ARNs and domain names. You can request that only certificates that match a specific status be listed. You can also filter by specific attributes of the certificate. </p>

#### `listTagsForCertificate`

``` purescript
listTagsForCertificate :: forall eff. ListTagsForCertificateRequest -> Aff (err :: RequestError | eff) ListTagsForCertificateResponse
```

<p>Lists the tags that have been applied to the ACM Certificate. Use the certificate's Amazon Resource Name (ARN) to specify the certificate. To add a tag to an ACM Certificate, use the <a>AddTagsToCertificate</a> action. To delete a tag, use the <a>RemoveTagsFromCertificate</a> action. </p>

#### `removeTagsFromCertificate`

``` purescript
removeTagsFromCertificate :: forall eff. RemoveTagsFromCertificateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Remove one or more tags from an ACM Certificate. A tag consists of a key-value pair. If you do not specify the value portion of the tag when calling this function, the tag will be removed regardless of value. If you specify a value, the tag is removed only if it is associated with the specified value. </p> <p>To add tags to a certificate, use the <a>AddTagsToCertificate</a> action. To view all of the tags that have been applied to a specific ACM Certificate, use the <a>ListTagsForCertificate</a> action. </p>

#### `requestCertificate`

``` purescript
requestCertificate :: forall eff. RequestCertificateRequest -> Aff (err :: RequestError | eff) RequestCertificateResponse
```

<p>Requests an ACM Certificate for use with other AWS services. To request an ACM Certificate, you must specify the fully qualified domain name (FQDN) for your site in the <code>DomainName</code> parameter. You can also specify additional FQDNs in the <code>SubjectAlternativeNames</code> parameter if users can reach your site by using other names. </p> <p>For each domain name you specify, email is sent to the domain owner to request approval to issue the certificate. Email is sent to three registered contact addresses in the WHOIS database and to five common system administration addresses formed from the <code>DomainName</code> you enter or the optional <code>ValidationDomain</code> parameter. For more information, see <a href="http://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate.html">Validate Domain Ownership</a>. </p> <p>After receiving approval from the domain owner, the ACM Certificate is issued. For more information, see the <a href="http://docs.aws.amazon.com/acm/latest/userguide/">AWS Certificate Manager User Guide</a>. </p>

#### `resendValidationEmail`

``` purescript
resendValidationEmail :: forall eff. ResendValidationEmailRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Resends the email that requests domain ownership validation. The domain owner or an authorized representative must approve the ACM Certificate before it can be issued. The certificate can be approved by clicking a link in the mail to navigate to the Amazon certificate approval website and then clicking <b>I Approve</b>. However, the validation email can be blocked by spam filters. Therefore, if you do not receive the original mail, you can request that the mail be resent within 72 hours of requesting the ACM Certificate. If more than 72 hours have elapsed since your original request or since your last attempt to resend validation mail, you must request a new certificate. For more information about setting up your contact email addresses, see <a href="http://docs.aws.amazon.com/acm/latest/userguide/setup-email.html">Configure Email for your Domain</a>. </p>

#### `AddTagsToCertificateRequest`

``` purescript
newtype AddTagsToCertificateRequest
  = AddTagsToCertificateRequest { "CertificateArn" :: Arn, "Tags" :: TagList }
```

##### Instances
``` purescript
Newtype AddTagsToCertificateRequest _
```

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

##### Instances
``` purescript
Newtype Arn _
```

#### `CertificateBody`

``` purescript
newtype CertificateBody
  = CertificateBody String
```

##### Instances
``` purescript
Newtype CertificateBody _
```

#### `CertificateBodyBlob`

``` purescript
newtype CertificateBodyBlob
  = CertificateBodyBlob String
```

##### Instances
``` purescript
Newtype CertificateBodyBlob _
```

#### `CertificateChain`

``` purescript
newtype CertificateChain
  = CertificateChain String
```

##### Instances
``` purescript
Newtype CertificateChain _
```

#### `CertificateChainBlob`

``` purescript
newtype CertificateChainBlob
  = CertificateChainBlob String
```

##### Instances
``` purescript
Newtype CertificateChainBlob _
```

#### `CertificateDetail`

``` purescript
newtype CertificateDetail
  = CertificateDetail { "CertificateArn" :: NullOrUndefined (Arn), "DomainName" :: NullOrUndefined (DomainNameString), "SubjectAlternativeNames" :: NullOrUndefined (DomainList), "DomainValidationOptions" :: NullOrUndefined (DomainValidationList), "Serial" :: NullOrUndefined (String), "Subject" :: NullOrUndefined (String), "Issuer" :: NullOrUndefined (String), "CreatedAt" :: NullOrUndefined (TStamp), "IssuedAt" :: NullOrUndefined (TStamp), "ImportedAt" :: NullOrUndefined (TStamp), "Status" :: NullOrUndefined (CertificateStatus), "RevokedAt" :: NullOrUndefined (TStamp), "RevocationReason" :: NullOrUndefined (RevocationReason), "NotBefore" :: NullOrUndefined (TStamp), "NotAfter" :: NullOrUndefined (TStamp), "KeyAlgorithm" :: NullOrUndefined (KeyAlgorithm), "SignatureAlgorithm" :: NullOrUndefined (String), "InUseBy" :: NullOrUndefined (InUseList), "FailureReason" :: NullOrUndefined (FailureReason), "Type" :: NullOrUndefined (CertificateType), "RenewalSummary" :: NullOrUndefined (RenewalSummary), "KeyUsages" :: NullOrUndefined (KeyUsageList), "ExtendedKeyUsages" :: NullOrUndefined (ExtendedKeyUsageList) }
```

<p>Contains metadata about an ACM certificate. This structure is returned in the response to a <a>DescribeCertificate</a> request. </p>

##### Instances
``` purescript
Newtype CertificateDetail _
```

#### `CertificateStatus`

``` purescript
newtype CertificateStatus
  = CertificateStatus String
```

##### Instances
``` purescript
Newtype CertificateStatus _
```

#### `CertificateStatuses`

``` purescript
newtype CertificateStatuses
  = CertificateStatuses (Array CertificateStatus)
```

##### Instances
``` purescript
Newtype CertificateStatuses _
```

#### `CertificateSummary`

``` purescript
newtype CertificateSummary
  = CertificateSummary { "CertificateArn" :: NullOrUndefined (Arn), "DomainName" :: NullOrUndefined (DomainNameString) }
```

<p>This structure is returned in the response object of <a>ListCertificates</a> action. </p>

##### Instances
``` purescript
Newtype CertificateSummary _
```

#### `CertificateSummaryList`

``` purescript
newtype CertificateSummaryList
  = CertificateSummaryList (Array CertificateSummary)
```

##### Instances
``` purescript
Newtype CertificateSummaryList _
```

#### `CertificateType`

``` purescript
newtype CertificateType
  = CertificateType String
```

##### Instances
``` purescript
Newtype CertificateType _
```

#### `DeleteCertificateRequest`

``` purescript
newtype DeleteCertificateRequest
  = DeleteCertificateRequest { "CertificateArn" :: Arn }
```

##### Instances
``` purescript
Newtype DeleteCertificateRequest _
```

#### `DescribeCertificateRequest`

``` purescript
newtype DescribeCertificateRequest
  = DescribeCertificateRequest { "CertificateArn" :: Arn }
```

##### Instances
``` purescript
Newtype DescribeCertificateRequest _
```

#### `DescribeCertificateResponse`

``` purescript
newtype DescribeCertificateResponse
  = DescribeCertificateResponse { "Certificate" :: NullOrUndefined (CertificateDetail) }
```

##### Instances
``` purescript
Newtype DescribeCertificateResponse _
```

#### `DomainList`

``` purescript
newtype DomainList
  = DomainList (Array DomainNameString)
```

##### Instances
``` purescript
Newtype DomainList _
```

#### `DomainNameString`

``` purescript
newtype DomainNameString
  = DomainNameString String
```

##### Instances
``` purescript
Newtype DomainNameString _
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

#### `DomainValidation`

``` purescript
newtype DomainValidation
  = DomainValidation { "DomainName" :: DomainNameString, "ValidationEmails" :: NullOrUndefined (ValidationEmailList), "ValidationDomain" :: NullOrUndefined (DomainNameString), "ValidationStatus" :: NullOrUndefined (DomainStatus), "ResourceRecord" :: NullOrUndefined (ResourceRecord), "ValidationMethod" :: NullOrUndefined (ValidationMethod) }
```

<p>Contains information about the validation of each domain name in the certificate.</p>

##### Instances
``` purescript
Newtype DomainValidation _
```

#### `DomainValidationList`

``` purescript
newtype DomainValidationList
  = DomainValidationList (Array DomainValidation)
```

##### Instances
``` purescript
Newtype DomainValidationList _
```

#### `DomainValidationOption`

``` purescript
newtype DomainValidationOption
  = DomainValidationOption { "DomainName" :: DomainNameString, "ValidationDomain" :: DomainNameString }
```

<p>Contains information about the domain names that you want ACM to use to send you emails that enable you to validate domain ownership.</p>

##### Instances
``` purescript
Newtype DomainValidationOption _
```

#### `DomainValidationOptionList`

``` purescript
newtype DomainValidationOptionList
  = DomainValidationOptionList (Array DomainValidationOption)
```

##### Instances
``` purescript
Newtype DomainValidationOptionList _
```

#### `ExtendedKeyUsage`

``` purescript
newtype ExtendedKeyUsage
  = ExtendedKeyUsage { "Name" :: NullOrUndefined (ExtendedKeyUsageName), "OID" :: NullOrUndefined (String) }
```

<p>The Extended Key Usage X.509 v3 extension defines one or more purposes for which the public key can be used. This is in addition to or in place of the basic purposes specified by the Key Usage extension. </p>

##### Instances
``` purescript
Newtype ExtendedKeyUsage _
```

#### `ExtendedKeyUsageFilterList`

``` purescript
newtype ExtendedKeyUsageFilterList
  = ExtendedKeyUsageFilterList (Array ExtendedKeyUsageName)
```

##### Instances
``` purescript
Newtype ExtendedKeyUsageFilterList _
```

#### `ExtendedKeyUsageList`

``` purescript
newtype ExtendedKeyUsageList
  = ExtendedKeyUsageList (Array ExtendedKeyUsage)
```

##### Instances
``` purescript
Newtype ExtendedKeyUsageList _
```

#### `ExtendedKeyUsageName`

``` purescript
newtype ExtendedKeyUsageName
  = ExtendedKeyUsageName String
```

##### Instances
``` purescript
Newtype ExtendedKeyUsageName _
```

#### `FailureReason`

``` purescript
newtype FailureReason
  = FailureReason String
```

##### Instances
``` purescript
Newtype FailureReason _
```

#### `Filters`

``` purescript
newtype Filters
  = Filters { "ExtendedKeyUsage'" :: NullOrUndefined (ExtendedKeyUsageFilterList), "KeyUsage'" :: NullOrUndefined (KeyUsageFilterList), "KeyTypes'" :: NullOrUndefined (KeyAlgorithmList) }
```

<p>This structure can be used in the <a>ListCertificates</a> action to filter the output of the certificate list. </p>

##### Instances
``` purescript
Newtype Filters _
```

#### `GetCertificateRequest`

``` purescript
newtype GetCertificateRequest
  = GetCertificateRequest { "CertificateArn" :: Arn }
```

##### Instances
``` purescript
Newtype GetCertificateRequest _
```

#### `GetCertificateResponse`

``` purescript
newtype GetCertificateResponse
  = GetCertificateResponse { "Certificate" :: NullOrUndefined (CertificateBody), "CertificateChain" :: NullOrUndefined (CertificateChain) }
```

##### Instances
``` purescript
Newtype GetCertificateResponse _
```

#### `IdempotencyToken`

``` purescript
newtype IdempotencyToken
  = IdempotencyToken String
```

##### Instances
``` purescript
Newtype IdempotencyToken _
```

#### `ImportCertificateRequest`

``` purescript
newtype ImportCertificateRequest
  = ImportCertificateRequest { "CertificateArn" :: NullOrUndefined (Arn), "Certificate" :: CertificateBodyBlob, "PrivateKey" :: PrivateKeyBlob, "CertificateChain" :: NullOrUndefined (CertificateChainBlob) }
```

##### Instances
``` purescript
Newtype ImportCertificateRequest _
```

#### `ImportCertificateResponse`

``` purescript
newtype ImportCertificateResponse
  = ImportCertificateResponse { "CertificateArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype ImportCertificateResponse _
```

#### `InUseList`

``` purescript
newtype InUseList
  = InUseList (Array String)
```

##### Instances
``` purescript
Newtype InUseList _
```

#### `InvalidArnException`

``` purescript
newtype InvalidArnException
  = InvalidArnException { "Message'" :: NullOrUndefined (String) }
```

<p>The requested Amazon Resource Name (ARN) does not refer to an existing resource.</p>

##### Instances
``` purescript
Newtype InvalidArnException _
```

#### `InvalidDomainValidationOptionsException`

``` purescript
newtype InvalidDomainValidationOptionsException
  = InvalidDomainValidationOptionsException { "Message'" :: NullOrUndefined (String) }
```

<p>One or more values in the <a>DomainValidationOption</a> structure is incorrect.</p>

##### Instances
``` purescript
Newtype InvalidDomainValidationOptionsException _
```

#### `InvalidStateException`

``` purescript
newtype InvalidStateException
  = InvalidStateException { "Message'" :: NullOrUndefined (String) }
```

<p>Processing has reached an invalid state. For example, this exception can occur if the specified domain is not using email validation, or the current certificate status does not permit the requested operation. See the exception message returned by ACM to determine which state is not valid.</p>

##### Instances
``` purescript
Newtype InvalidStateException _
```

#### `InvalidTagException`

``` purescript
newtype InvalidTagException
  = InvalidTagException { "Message'" :: NullOrUndefined (String) }
```

<p>One or both of the values that make up the key-value pair is not valid. For example, you cannot specify a tag value that begins with <code>aws:</code>.</p>

##### Instances
``` purescript
Newtype InvalidTagException _
```

#### `KeyAlgorithm`

``` purescript
newtype KeyAlgorithm
  = KeyAlgorithm String
```

##### Instances
``` purescript
Newtype KeyAlgorithm _
```

#### `KeyAlgorithmList`

``` purescript
newtype KeyAlgorithmList
  = KeyAlgorithmList (Array KeyAlgorithm)
```

##### Instances
``` purescript
Newtype KeyAlgorithmList _
```

#### `KeyUsage`

``` purescript
newtype KeyUsage
  = KeyUsage { "Name" :: NullOrUndefined (KeyUsageName) }
```

<p>The Key Usage X.509 v3 extension defines the purpose of the public key contained in the certificate.</p>

##### Instances
``` purescript
Newtype KeyUsage _
```

#### `KeyUsageFilterList`

``` purescript
newtype KeyUsageFilterList
  = KeyUsageFilterList (Array KeyUsageName)
```

##### Instances
``` purescript
Newtype KeyUsageFilterList _
```

#### `KeyUsageList`

``` purescript
newtype KeyUsageList
  = KeyUsageList (Array KeyUsage)
```

##### Instances
``` purescript
Newtype KeyUsageList _
```

#### `KeyUsageName`

``` purescript
newtype KeyUsageName
  = KeyUsageName String
```

##### Instances
``` purescript
Newtype KeyUsageName _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (String) }
```

<p>An ACM limit has been exceeded. For example, you may have input more domains than are allowed or you've requested too many certificates for your account. See the exception message returned by ACM to determine which limit you have violated. For more information about ACM limits, see the <a href="http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html">Limits</a> topic.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListCertificatesRequest`

``` purescript
newtype ListCertificatesRequest
  = ListCertificatesRequest { "CertificateStatuses" :: NullOrUndefined (CertificateStatuses), "Includes" :: NullOrUndefined (Filters), "NextToken" :: NullOrUndefined (NextToken), "MaxItems" :: NullOrUndefined (MaxItems) }
```

##### Instances
``` purescript
Newtype ListCertificatesRequest _
```

#### `ListCertificatesResponse`

``` purescript
newtype ListCertificatesResponse
  = ListCertificatesResponse { "NextToken" :: NullOrUndefined (NextToken), "CertificateSummaryList" :: NullOrUndefined (CertificateSummaryList) }
```

##### Instances
``` purescript
Newtype ListCertificatesResponse _
```

#### `ListTagsForCertificateRequest`

``` purescript
newtype ListTagsForCertificateRequest
  = ListTagsForCertificateRequest { "CertificateArn" :: Arn }
```

##### Instances
``` purescript
Newtype ListTagsForCertificateRequest _
```

#### `ListTagsForCertificateResponse`

``` purescript
newtype ListTagsForCertificateResponse
  = ListTagsForCertificateResponse { "Tags" :: NullOrUndefined (TagList) }
```

##### Instances
``` purescript
Newtype ListTagsForCertificateResponse _
```

#### `MaxItems`

``` purescript
newtype MaxItems
  = MaxItems Int
```

##### Instances
``` purescript
Newtype MaxItems _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

##### Instances
``` purescript
Newtype NextToken _
```

#### `PrivateKeyBlob`

``` purescript
newtype PrivateKeyBlob
  = PrivateKeyBlob String
```

##### Instances
``` purescript
Newtype PrivateKeyBlob _
```

#### `RecordType`

``` purescript
newtype RecordType
  = RecordType String
```

##### Instances
``` purescript
Newtype RecordType _
```

#### `RemoveTagsFromCertificateRequest`

``` purescript
newtype RemoveTagsFromCertificateRequest
  = RemoveTagsFromCertificateRequest { "CertificateArn" :: Arn, "Tags" :: TagList }
```

##### Instances
``` purescript
Newtype RemoveTagsFromCertificateRequest _
```

#### `RenewalStatus`

``` purescript
newtype RenewalStatus
  = RenewalStatus String
```

##### Instances
``` purescript
Newtype RenewalStatus _
```

#### `RenewalSummary`

``` purescript
newtype RenewalSummary
  = RenewalSummary { "RenewalStatus" :: RenewalStatus, "DomainValidationOptions" :: DomainValidationList }
```

<p>Contains information about the status of ACM's <a href="http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html">managed renewal</a> for the certificate. This structure exists only when the certificate type is <code>AMAZON_ISSUED</code>.</p>

##### Instances
``` purescript
Newtype RenewalSummary _
```

#### `RequestCertificateRequest`

``` purescript
newtype RequestCertificateRequest
  = RequestCertificateRequest { "DomainName" :: DomainNameString, "ValidationMethod" :: NullOrUndefined (ValidationMethod), "SubjectAlternativeNames" :: NullOrUndefined (DomainList), "IdempotencyToken" :: NullOrUndefined (IdempotencyToken), "DomainValidationOptions" :: NullOrUndefined (DomainValidationOptionList) }
```

##### Instances
``` purescript
Newtype RequestCertificateRequest _
```

#### `RequestCertificateResponse`

``` purescript
newtype RequestCertificateResponse
  = RequestCertificateResponse { "CertificateArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype RequestCertificateResponse _
```

#### `RequestInProgressException`

``` purescript
newtype RequestInProgressException
  = RequestInProgressException { "Message'" :: NullOrUndefined (String) }
```

<p>The certificate request is in process and the certificate in your account has not yet been issued.</p>

##### Instances
``` purescript
Newtype RequestInProgressException _
```

#### `ResendValidationEmailRequest`

``` purescript
newtype ResendValidationEmailRequest
  = ResendValidationEmailRequest { "CertificateArn" :: Arn, "Domain" :: DomainNameString, "ValidationDomain" :: DomainNameString }
```

##### Instances
``` purescript
Newtype ResendValidationEmailRequest _
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException { "Message'" :: NullOrUndefined (String) }
```

<p>The certificate is in use by another AWS service in the caller's account. Remove the association and try again.</p>

##### Instances
``` purescript
Newtype ResourceInUseException _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (String) }
```

<p>The specified certificate cannot be found in the caller's account, or the caller's account cannot be found.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `ResourceRecord`

``` purescript
newtype ResourceRecord
  = ResourceRecord { "Name" :: String, "Type" :: RecordType, "Value" :: String }
```

<p>Contains a DNS record value that you can use to can use to validate ownership or control of a domain. This is used by the <a>DescribeCertificate</a> action. </p>

##### Instances
``` purescript
Newtype ResourceRecord _
```

#### `RevocationReason`

``` purescript
newtype RevocationReason
  = RevocationReason String
```

##### Instances
``` purescript
Newtype RevocationReason _
```

#### `TStamp`

``` purescript
newtype TStamp
  = TStamp Number
```

##### Instances
``` purescript
Newtype TStamp _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: NullOrUndefined (TagValue) }
```

<p>A key-value pair that identifies or specifies metadata about an ACM resource.</p>

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

#### `TooManyTagsException`

``` purescript
newtype TooManyTagsException
  = TooManyTagsException { "Message'" :: NullOrUndefined (String) }
```

<p>The request contains too many tags. Try the request again with fewer tags.</p>

##### Instances
``` purescript
Newtype TooManyTagsException _
```

#### `ValidationEmailList`

``` purescript
newtype ValidationEmailList
  = ValidationEmailList (Array String)
```

##### Instances
``` purescript
Newtype ValidationEmailList _
```

#### `ValidationMethod`

``` purescript
newtype ValidationMethod
  = ValidationMethod String
```

##### Instances
``` purescript
Newtype ValidationMethod _
```


