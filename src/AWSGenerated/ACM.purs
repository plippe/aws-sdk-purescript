

-- | <fullname>AWS Certificate Manager</fullname> <p>Welcome to the AWS Certificate Manager (ACM) API documentation.</p> <p>You can use ACM to manage SSL/TLS certificates for your AWS-based websites and applications. For general information about using ACM, see the <a href="http://docs.aws.amazon.com/acm/latest/userguide/"> <i>AWS Certificate Manager User Guide</i> </a>.</p>
module AWS.ACM where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined as NullOrUndefined
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap

import AWS.Request as Request
import AWS.Request.Types as Types

serviceName = "ACM" :: String


-- | <p>Adds one or more tags to an ACM Certificate. Tags are labels that you can use to identify and organize your AWS resources. Each tag consists of a <code>key</code> and an optional <code>value</code>. You specify the certificate on input by its Amazon Resource Name (ARN). You specify the tag by using a key-value pair. </p> <p>You can apply a tag to just one certificate if you want to identify a specific characteristic of that certificate, or you can apply the same tag to multiple certificates if you want to filter for a common relationship among those certificates. Similarly, you can apply the same tag to multiple resources if you want to specify a relationship among those resources. For example, you can add the same tag to an ACM Certificate and an Elastic Load Balancing load balancer to indicate that they are both used by the same website. For more information, see <a href="http://docs.aws.amazon.com/acm/latest/userguide/tags.html">Tagging ACM Certificates</a>. </p> <p>To remove one or more tags, use the <a>RemoveTagsFromCertificate</a> action. To view all of the tags that have been applied to the certificate, use the <a>ListTagsForCertificate</a> action. </p>
addTagsToCertificate :: forall eff. AddTagsToCertificateRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
addTagsToCertificate = Request.request serviceName "addTagsToCertificate" 


-- | <p>Deletes a certificate and its associated private key. If this action succeeds, the certificate no longer appears in the list that can be displayed by calling the <a>ListCertificates</a> action or be retrieved by calling the <a>GetCertificate</a> action. The certificate will not be available for use by AWS services integrated with ACM. </p> <note> <p>You cannot delete an ACM Certificate that is being used by another AWS service. To delete a certificate that is in use, the certificate association must first be removed.</p> </note>
deleteCertificate :: forall eff. DeleteCertificateRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteCertificate = Request.request serviceName "deleteCertificate" 


-- | <p>Returns detailed metadata about the specified ACM Certificate.</p>
describeCertificate :: forall eff. DescribeCertificateRequest -> Aff (exception :: EXCEPTION | eff) DescribeCertificateResponse
describeCertificate = Request.request serviceName "describeCertificate" 


-- | <p>Retrieves a certificate specified by an ARN and its certificate chain . The chain is an ordered list of certificates that contains the end entity certificate, intermediate certificates of subordinate CAs, and the root certificate in that order. The certificate and certificate chain are base64 encoded. If you want to decode the certificate to see the individual fields, you can use OpenSSL.</p>
getCertificate :: forall eff. GetCertificateRequest -> Aff (exception :: EXCEPTION | eff) GetCertificateResponse
getCertificate = Request.request serviceName "getCertificate" 


-- | <p>Imports a certificate into AWS Certificate Manager (ACM) to use with services that are integrated with ACM. Note that <a href="http://docs.aws.amazon.com/acm/latest/userguide/acm-services.html">integrated services</a> allow only certificate types and keys they support to be associated with their resources. Further, their support differs depending on whether the certificate is imported into IAM or into ACM. For more information, see the documentation for each service. For more information about importing certificates into ACM, see <a href="http://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html">Importing Certificates</a> in the <i>AWS Certificate Manager User Guide</i>. </p> <note> <p>ACM does not provide <a href="http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html">managed renewal</a> for certificates that you import.</p> </note> <p>Note the following guidelines when importing third party certificates:</p> <ul> <li> <p>You must enter the private key that matches the certificate you are importing.</p> </li> <li> <p>The private key must be unencrypted. You cannot import a private key that is protected by a password or a passphrase.</p> </li> <li> <p>If the certificate you are importing is not self-signed, you must enter its certificate chain.</p> </li> <li> <p>If a certificate chain is included, the issuer must be the subject of one of the certificates in the chain.</p> </li> <li> <p>The certificate, private key, and certificate chain must be PEM-encoded.</p> </li> <li> <p>The current time must be between the <code>Not Before</code> and <code>Not After</code> certificate fields.</p> </li> <li> <p>The <code>Issuer</code> field must not be empty.</p> </li> <li> <p>The OCSP authority URL, if present, must not exceed 1000 characters.</p> </li> <li> <p>To import a new certificate, omit the <code>CertificateArn</code> argument. Include this argument only when you want to replace a previously imported certificate.</p> </li> <li> <p>When you import a certificate by using the CLI or one of the SDKs, you must specify the certificate, the certificate chain, and the private key by their file names preceded by <code>file://</code>. For example, you can specify a certificate saved in the <code>C:\temp</code> folder as <code>file://C:\temp\certificate_to_import.pem</code>. If you are making an HTTP or HTTPS Query request, include these arguments as BLOBs. </p> </li> </ul> <p>This operation returns the <a href="http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html">Amazon Resource Name (ARN)</a> of the imported certificate.</p>
importCertificate :: forall eff. ImportCertificateRequest -> Aff (exception :: EXCEPTION | eff) ImportCertificateResponse
importCertificate = Request.request serviceName "importCertificate" 


-- | <p>Retrieves a list of certificate ARNs and domain names. You can request that only certificates that match a specific status be listed. You can also filter by specific attributes of the certificate. </p>
listCertificates :: forall eff. ListCertificatesRequest -> Aff (exception :: EXCEPTION | eff) ListCertificatesResponse
listCertificates = Request.request serviceName "listCertificates" 


-- | <p>Lists the tags that have been applied to the ACM Certificate. Use the certificate's Amazon Resource Name (ARN) to specify the certificate. To add a tag to an ACM Certificate, use the <a>AddTagsToCertificate</a> action. To delete a tag, use the <a>RemoveTagsFromCertificate</a> action. </p>
listTagsForCertificate :: forall eff. ListTagsForCertificateRequest -> Aff (exception :: EXCEPTION | eff) ListTagsForCertificateResponse
listTagsForCertificate = Request.request serviceName "listTagsForCertificate" 


-- | <p>Remove one or more tags from an ACM Certificate. A tag consists of a key-value pair. If you do not specify the value portion of the tag when calling this function, the tag will be removed regardless of value. If you specify a value, the tag is removed only if it is associated with the specified value. </p> <p>To add tags to a certificate, use the <a>AddTagsToCertificate</a> action. To view all of the tags that have been applied to a specific ACM Certificate, use the <a>ListTagsForCertificate</a> action. </p>
removeTagsFromCertificate :: forall eff. RemoveTagsFromCertificateRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
removeTagsFromCertificate = Request.request serviceName "removeTagsFromCertificate" 


-- | <p>Requests an ACM Certificate for use with other AWS services. To request an ACM Certificate, you must specify the fully qualified domain name (FQDN) for your site in the <code>DomainName</code> parameter. You can also specify additional FQDNs in the <code>SubjectAlternativeNames</code> parameter if users can reach your site by using other names. </p> <p>For each domain name you specify, email is sent to the domain owner to request approval to issue the certificate. Email is sent to three registered contact addresses in the WHOIS database and to five common system administration addresses formed from the <code>DomainName</code> you enter or the optional <code>ValidationDomain</code> parameter. For more information, see <a href="http://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate.html">Validate Domain Ownership</a>. </p> <p>After receiving approval from the domain owner, the ACM Certificate is issued. For more information, see the <a href="http://docs.aws.amazon.com/acm/latest/userguide/">AWS Certificate Manager User Guide</a>. </p>
requestCertificate :: forall eff. RequestCertificateRequest -> Aff (exception :: EXCEPTION | eff) RequestCertificateResponse
requestCertificate = Request.request serviceName "requestCertificate" 


-- | <p>Resends the email that requests domain ownership validation. The domain owner or an authorized representative must approve the ACM Certificate before it can be issued. The certificate can be approved by clicking a link in the mail to navigate to the Amazon certificate approval website and then clicking <b>I Approve</b>. However, the validation email can be blocked by spam filters. Therefore, if you do not receive the original mail, you can request that the mail be resent within 72 hours of requesting the ACM Certificate. If more than 72 hours have elapsed since your original request or since your last attempt to resend validation mail, you must request a new certificate. For more information about setting up your contact email addresses, see <a href="http://docs.aws.amazon.com/acm/latest/userguide/setup-email.html">Configure Email for your Domain</a>. </p>
resendValidationEmail :: forall eff. ResendValidationEmailRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
resendValidationEmail = Request.request serviceName "resendValidationEmail" 


newtype AddTagsToCertificateRequest = AddTagsToCertificateRequest 
  { "CertificateArn" :: (Arn)
  , "Tags" :: (TagList)
  }
derive instance newtypeAddTagsToCertificateRequest :: Newtype AddTagsToCertificateRequest _
derive instance repGenericAddTagsToCertificateRequest :: Generic AddTagsToCertificateRequest _
instance showAddTagsToCertificateRequest :: Show AddTagsToCertificateRequest where
  show = genericShow
instance decodeAddTagsToCertificateRequest :: Decode AddTagsToCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsToCertificateRequest :: Encode AddTagsToCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _
derive instance repGenericArn :: Generic Arn _
instance showArn :: Show Arn where
  show = genericShow
instance decodeArn :: Decode Arn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArn :: Encode Arn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateBody = CertificateBody String
derive instance newtypeCertificateBody :: Newtype CertificateBody _
derive instance repGenericCertificateBody :: Generic CertificateBody _
instance showCertificateBody :: Show CertificateBody where
  show = genericShow
instance decodeCertificateBody :: Decode CertificateBody where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateBody :: Encode CertificateBody where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateBodyBlob = CertificateBodyBlob String
derive instance newtypeCertificateBodyBlob :: Newtype CertificateBodyBlob _
derive instance repGenericCertificateBodyBlob :: Generic CertificateBodyBlob _
instance showCertificateBodyBlob :: Show CertificateBodyBlob where
  show = genericShow
instance decodeCertificateBodyBlob :: Decode CertificateBodyBlob where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateBodyBlob :: Encode CertificateBodyBlob where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateChain = CertificateChain String
derive instance newtypeCertificateChain :: Newtype CertificateChain _
derive instance repGenericCertificateChain :: Generic CertificateChain _
instance showCertificateChain :: Show CertificateChain where
  show = genericShow
instance decodeCertificateChain :: Decode CertificateChain where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateChain :: Encode CertificateChain where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateChainBlob = CertificateChainBlob String
derive instance newtypeCertificateChainBlob :: Newtype CertificateChainBlob _
derive instance repGenericCertificateChainBlob :: Generic CertificateChainBlob _
instance showCertificateChainBlob :: Show CertificateChainBlob where
  show = genericShow
instance decodeCertificateChainBlob :: Decode CertificateChainBlob where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateChainBlob :: Encode CertificateChainBlob where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains metadata about an ACM certificate. This structure is returned in the response to a <a>DescribeCertificate</a> request. </p>
newtype CertificateDetail = CertificateDetail 
  { "CertificateArn" :: NullOrUndefined.NullOrUndefined (Arn)
  , "DomainName" :: NullOrUndefined.NullOrUndefined (DomainNameString)
  , "SubjectAlternativeNames" :: NullOrUndefined.NullOrUndefined (DomainList)
  , "DomainValidationOptions" :: NullOrUndefined.NullOrUndefined (DomainValidationList)
  , "Serial" :: NullOrUndefined.NullOrUndefined (String)
  , "Subject" :: NullOrUndefined.NullOrUndefined (String)
  , "Issuer" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "IssuedAt" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "ImportedAt" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "Status" :: NullOrUndefined.NullOrUndefined (CertificateStatus)
  , "RevokedAt" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "RevocationReason" :: NullOrUndefined.NullOrUndefined (RevocationReason)
  , "NotBefore" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "NotAfter" :: NullOrUndefined.NullOrUndefined (TStamp)
  , "KeyAlgorithm" :: NullOrUndefined.NullOrUndefined (KeyAlgorithm)
  , "SignatureAlgorithm" :: NullOrUndefined.NullOrUndefined (String)
  , "InUseBy" :: NullOrUndefined.NullOrUndefined (InUseList)
  , "FailureReason" :: NullOrUndefined.NullOrUndefined (FailureReason)
  , "Type" :: NullOrUndefined.NullOrUndefined (CertificateType)
  , "RenewalSummary" :: NullOrUndefined.NullOrUndefined (RenewalSummary)
  , "KeyUsages" :: NullOrUndefined.NullOrUndefined (KeyUsageList)
  , "ExtendedKeyUsages" :: NullOrUndefined.NullOrUndefined (ExtendedKeyUsageList)
  }
derive instance newtypeCertificateDetail :: Newtype CertificateDetail _
derive instance repGenericCertificateDetail :: Generic CertificateDetail _
instance showCertificateDetail :: Show CertificateDetail where
  show = genericShow
instance decodeCertificateDetail :: Decode CertificateDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateDetail :: Encode CertificateDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateStatus = CertificateStatus String
derive instance newtypeCertificateStatus :: Newtype CertificateStatus _
derive instance repGenericCertificateStatus :: Generic CertificateStatus _
instance showCertificateStatus :: Show CertificateStatus where
  show = genericShow
instance decodeCertificateStatus :: Decode CertificateStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateStatus :: Encode CertificateStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateStatuses = CertificateStatuses (Array CertificateStatus)
derive instance newtypeCertificateStatuses :: Newtype CertificateStatuses _
derive instance repGenericCertificateStatuses :: Generic CertificateStatuses _
instance showCertificateStatuses :: Show CertificateStatuses where
  show = genericShow
instance decodeCertificateStatuses :: Decode CertificateStatuses where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateStatuses :: Encode CertificateStatuses where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This structure is returned in the response object of <a>ListCertificates</a> action. </p>
newtype CertificateSummary = CertificateSummary 
  { "CertificateArn" :: NullOrUndefined.NullOrUndefined (Arn)
  , "DomainName" :: NullOrUndefined.NullOrUndefined (DomainNameString)
  }
derive instance newtypeCertificateSummary :: Newtype CertificateSummary _
derive instance repGenericCertificateSummary :: Generic CertificateSummary _
instance showCertificateSummary :: Show CertificateSummary where
  show = genericShow
instance decodeCertificateSummary :: Decode CertificateSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateSummary :: Encode CertificateSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateSummaryList = CertificateSummaryList (Array CertificateSummary)
derive instance newtypeCertificateSummaryList :: Newtype CertificateSummaryList _
derive instance repGenericCertificateSummaryList :: Generic CertificateSummaryList _
instance showCertificateSummaryList :: Show CertificateSummaryList where
  show = genericShow
instance decodeCertificateSummaryList :: Decode CertificateSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateSummaryList :: Encode CertificateSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateType = CertificateType String
derive instance newtypeCertificateType :: Newtype CertificateType _
derive instance repGenericCertificateType :: Generic CertificateType _
instance showCertificateType :: Show CertificateType where
  show = genericShow
instance decodeCertificateType :: Decode CertificateType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateType :: Encode CertificateType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCertificateRequest = DeleteCertificateRequest 
  { "CertificateArn" :: (Arn)
  }
derive instance newtypeDeleteCertificateRequest :: Newtype DeleteCertificateRequest _
derive instance repGenericDeleteCertificateRequest :: Generic DeleteCertificateRequest _
instance showDeleteCertificateRequest :: Show DeleteCertificateRequest where
  show = genericShow
instance decodeDeleteCertificateRequest :: Decode DeleteCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCertificateRequest :: Encode DeleteCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeCertificateRequest = DescribeCertificateRequest 
  { "CertificateArn" :: (Arn)
  }
derive instance newtypeDescribeCertificateRequest :: Newtype DescribeCertificateRequest _
derive instance repGenericDescribeCertificateRequest :: Generic DescribeCertificateRequest _
instance showDescribeCertificateRequest :: Show DescribeCertificateRequest where
  show = genericShow
instance decodeDescribeCertificateRequest :: Decode DescribeCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCertificateRequest :: Encode DescribeCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeCertificateResponse = DescribeCertificateResponse 
  { "Certificate" :: NullOrUndefined.NullOrUndefined (CertificateDetail)
  }
derive instance newtypeDescribeCertificateResponse :: Newtype DescribeCertificateResponse _
derive instance repGenericDescribeCertificateResponse :: Generic DescribeCertificateResponse _
instance showDescribeCertificateResponse :: Show DescribeCertificateResponse where
  show = genericShow
instance decodeDescribeCertificateResponse :: Decode DescribeCertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCertificateResponse :: Encode DescribeCertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainList = DomainList (Array DomainNameString)
derive instance newtypeDomainList :: Newtype DomainList _
derive instance repGenericDomainList :: Generic DomainList _
instance showDomainList :: Show DomainList where
  show = genericShow
instance decodeDomainList :: Decode DomainList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainList :: Encode DomainList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainNameString = DomainNameString String
derive instance newtypeDomainNameString :: Newtype DomainNameString _
derive instance repGenericDomainNameString :: Generic DomainNameString _
instance showDomainNameString :: Show DomainNameString where
  show = genericShow
instance decodeDomainNameString :: Decode DomainNameString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainNameString :: Encode DomainNameString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainStatus = DomainStatus String
derive instance newtypeDomainStatus :: Newtype DomainStatus _
derive instance repGenericDomainStatus :: Generic DomainStatus _
instance showDomainStatus :: Show DomainStatus where
  show = genericShow
instance decodeDomainStatus :: Decode DomainStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainStatus :: Encode DomainStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the validation of each domain name in the certificate.</p>
newtype DomainValidation = DomainValidation 
  { "DomainName" :: (DomainNameString)
  , "ValidationEmails" :: NullOrUndefined.NullOrUndefined (ValidationEmailList)
  , "ValidationDomain" :: NullOrUndefined.NullOrUndefined (DomainNameString)
  , "ValidationStatus" :: NullOrUndefined.NullOrUndefined (DomainStatus)
  , "ResourceRecord" :: NullOrUndefined.NullOrUndefined (ResourceRecord)
  , "ValidationMethod" :: NullOrUndefined.NullOrUndefined (ValidationMethod)
  }
derive instance newtypeDomainValidation :: Newtype DomainValidation _
derive instance repGenericDomainValidation :: Generic DomainValidation _
instance showDomainValidation :: Show DomainValidation where
  show = genericShow
instance decodeDomainValidation :: Decode DomainValidation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainValidation :: Encode DomainValidation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainValidationList = DomainValidationList (Array DomainValidation)
derive instance newtypeDomainValidationList :: Newtype DomainValidationList _
derive instance repGenericDomainValidationList :: Generic DomainValidationList _
instance showDomainValidationList :: Show DomainValidationList where
  show = genericShow
instance decodeDomainValidationList :: Decode DomainValidationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainValidationList :: Encode DomainValidationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the domain names that you want ACM to use to send you emails that enable you to validate domain ownership.</p>
newtype DomainValidationOption = DomainValidationOption 
  { "DomainName" :: (DomainNameString)
  , "ValidationDomain" :: (DomainNameString)
  }
derive instance newtypeDomainValidationOption :: Newtype DomainValidationOption _
derive instance repGenericDomainValidationOption :: Generic DomainValidationOption _
instance showDomainValidationOption :: Show DomainValidationOption where
  show = genericShow
instance decodeDomainValidationOption :: Decode DomainValidationOption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainValidationOption :: Encode DomainValidationOption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainValidationOptionList = DomainValidationOptionList (Array DomainValidationOption)
derive instance newtypeDomainValidationOptionList :: Newtype DomainValidationOptionList _
derive instance repGenericDomainValidationOptionList :: Generic DomainValidationOptionList _
instance showDomainValidationOptionList :: Show DomainValidationOptionList where
  show = genericShow
instance decodeDomainValidationOptionList :: Decode DomainValidationOptionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainValidationOptionList :: Encode DomainValidationOptionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Extended Key Usage X.509 v3 extension defines one or more purposes for which the public key can be used. This is in addition to or in place of the basic purposes specified by the Key Usage extension. </p>
newtype ExtendedKeyUsage = ExtendedKeyUsage 
  { "Name" :: NullOrUndefined.NullOrUndefined (ExtendedKeyUsageName)
  , "OID" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeExtendedKeyUsage :: Newtype ExtendedKeyUsage _
derive instance repGenericExtendedKeyUsage :: Generic ExtendedKeyUsage _
instance showExtendedKeyUsage :: Show ExtendedKeyUsage where
  show = genericShow
instance decodeExtendedKeyUsage :: Decode ExtendedKeyUsage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtendedKeyUsage :: Encode ExtendedKeyUsage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExtendedKeyUsageFilterList = ExtendedKeyUsageFilterList (Array ExtendedKeyUsageName)
derive instance newtypeExtendedKeyUsageFilterList :: Newtype ExtendedKeyUsageFilterList _
derive instance repGenericExtendedKeyUsageFilterList :: Generic ExtendedKeyUsageFilterList _
instance showExtendedKeyUsageFilterList :: Show ExtendedKeyUsageFilterList where
  show = genericShow
instance decodeExtendedKeyUsageFilterList :: Decode ExtendedKeyUsageFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtendedKeyUsageFilterList :: Encode ExtendedKeyUsageFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExtendedKeyUsageList = ExtendedKeyUsageList (Array ExtendedKeyUsage)
derive instance newtypeExtendedKeyUsageList :: Newtype ExtendedKeyUsageList _
derive instance repGenericExtendedKeyUsageList :: Generic ExtendedKeyUsageList _
instance showExtendedKeyUsageList :: Show ExtendedKeyUsageList where
  show = genericShow
instance decodeExtendedKeyUsageList :: Decode ExtendedKeyUsageList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtendedKeyUsageList :: Encode ExtendedKeyUsageList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExtendedKeyUsageName = ExtendedKeyUsageName String
derive instance newtypeExtendedKeyUsageName :: Newtype ExtendedKeyUsageName _
derive instance repGenericExtendedKeyUsageName :: Generic ExtendedKeyUsageName _
instance showExtendedKeyUsageName :: Show ExtendedKeyUsageName where
  show = genericShow
instance decodeExtendedKeyUsageName :: Decode ExtendedKeyUsageName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtendedKeyUsageName :: Encode ExtendedKeyUsageName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailureReason = FailureReason String
derive instance newtypeFailureReason :: Newtype FailureReason _
derive instance repGenericFailureReason :: Generic FailureReason _
instance showFailureReason :: Show FailureReason where
  show = genericShow
instance decodeFailureReason :: Decode FailureReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailureReason :: Encode FailureReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This structure can be used in the <a>ListCertificates</a> action to filter the output of the certificate list. </p>
newtype Filters = Filters 
  { "ExtendedKeyUsage'" :: NullOrUndefined.NullOrUndefined (ExtendedKeyUsageFilterList)
  , "KeyUsage'" :: NullOrUndefined.NullOrUndefined (KeyUsageFilterList)
  , "KeyTypes'" :: NullOrUndefined.NullOrUndefined (KeyAlgorithmList)
  }
derive instance newtypeFilters :: Newtype Filters _
derive instance repGenericFilters :: Generic Filters _
instance showFilters :: Show Filters where
  show = genericShow
instance decodeFilters :: Decode Filters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilters :: Encode Filters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCertificateRequest = GetCertificateRequest 
  { "CertificateArn" :: (Arn)
  }
derive instance newtypeGetCertificateRequest :: Newtype GetCertificateRequest _
derive instance repGenericGetCertificateRequest :: Generic GetCertificateRequest _
instance showGetCertificateRequest :: Show GetCertificateRequest where
  show = genericShow
instance decodeGetCertificateRequest :: Decode GetCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCertificateRequest :: Encode GetCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCertificateResponse = GetCertificateResponse 
  { "Certificate" :: NullOrUndefined.NullOrUndefined (CertificateBody)
  , "CertificateChain" :: NullOrUndefined.NullOrUndefined (CertificateChain)
  }
derive instance newtypeGetCertificateResponse :: Newtype GetCertificateResponse _
derive instance repGenericGetCertificateResponse :: Generic GetCertificateResponse _
instance showGetCertificateResponse :: Show GetCertificateResponse where
  show = genericShow
instance decodeGetCertificateResponse :: Decode GetCertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCertificateResponse :: Encode GetCertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdempotencyToken = IdempotencyToken String
derive instance newtypeIdempotencyToken :: Newtype IdempotencyToken _
derive instance repGenericIdempotencyToken :: Generic IdempotencyToken _
instance showIdempotencyToken :: Show IdempotencyToken where
  show = genericShow
instance decodeIdempotencyToken :: Decode IdempotencyToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdempotencyToken :: Encode IdempotencyToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportCertificateRequest = ImportCertificateRequest 
  { "CertificateArn" :: NullOrUndefined.NullOrUndefined (Arn)
  , "Certificate" :: (CertificateBodyBlob)
  , "PrivateKey" :: (PrivateKeyBlob)
  , "CertificateChain" :: NullOrUndefined.NullOrUndefined (CertificateChainBlob)
  }
derive instance newtypeImportCertificateRequest :: Newtype ImportCertificateRequest _
derive instance repGenericImportCertificateRequest :: Generic ImportCertificateRequest _
instance showImportCertificateRequest :: Show ImportCertificateRequest where
  show = genericShow
instance decodeImportCertificateRequest :: Decode ImportCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportCertificateRequest :: Encode ImportCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportCertificateResponse = ImportCertificateResponse 
  { "CertificateArn" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeImportCertificateResponse :: Newtype ImportCertificateResponse _
derive instance repGenericImportCertificateResponse :: Generic ImportCertificateResponse _
instance showImportCertificateResponse :: Show ImportCertificateResponse where
  show = genericShow
instance decodeImportCertificateResponse :: Decode ImportCertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportCertificateResponse :: Encode ImportCertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InUseList = InUseList (Array String)
derive instance newtypeInUseList :: Newtype InUseList _
derive instance repGenericInUseList :: Generic InUseList _
instance showInUseList :: Show InUseList where
  show = genericShow
instance decodeInUseList :: Decode InUseList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInUseList :: Encode InUseList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested Amazon Resource Name (ARN) does not refer to an existing resource.</p>
newtype InvalidArnException = InvalidArnException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidArnException :: Newtype InvalidArnException _
derive instance repGenericInvalidArnException :: Generic InvalidArnException _
instance showInvalidArnException :: Show InvalidArnException where
  show = genericShow
instance decodeInvalidArnException :: Decode InvalidArnException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidArnException :: Encode InvalidArnException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more values in the <a>DomainValidationOption</a> structure is incorrect.</p>
newtype InvalidDomainValidationOptionsException = InvalidDomainValidationOptionsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidDomainValidationOptionsException :: Newtype InvalidDomainValidationOptionsException _
derive instance repGenericInvalidDomainValidationOptionsException :: Generic InvalidDomainValidationOptionsException _
instance showInvalidDomainValidationOptionsException :: Show InvalidDomainValidationOptionsException where
  show = genericShow
instance decodeInvalidDomainValidationOptionsException :: Decode InvalidDomainValidationOptionsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDomainValidationOptionsException :: Encode InvalidDomainValidationOptionsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Processing has reached an invalid state. For example, this exception can occur if the specified domain is not using email validation, or the current certificate status does not permit the requested operation. See the exception message returned by ACM to determine which state is not valid.</p>
newtype InvalidStateException = InvalidStateException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidStateException :: Newtype InvalidStateException _
derive instance repGenericInvalidStateException :: Generic InvalidStateException _
instance showInvalidStateException :: Show InvalidStateException where
  show = genericShow
instance decodeInvalidStateException :: Decode InvalidStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidStateException :: Encode InvalidStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or both of the values that make up the key-value pair is not valid. For example, you cannot specify a tag value that begins with <code>aws:</code>.</p>
newtype InvalidTagException = InvalidTagException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidTagException :: Newtype InvalidTagException _
derive instance repGenericInvalidTagException :: Generic InvalidTagException _
instance showInvalidTagException :: Show InvalidTagException where
  show = genericShow
instance decodeInvalidTagException :: Decode InvalidTagException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTagException :: Encode InvalidTagException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyAlgorithm = KeyAlgorithm String
derive instance newtypeKeyAlgorithm :: Newtype KeyAlgorithm _
derive instance repGenericKeyAlgorithm :: Generic KeyAlgorithm _
instance showKeyAlgorithm :: Show KeyAlgorithm where
  show = genericShow
instance decodeKeyAlgorithm :: Decode KeyAlgorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyAlgorithm :: Encode KeyAlgorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyAlgorithmList = KeyAlgorithmList (Array KeyAlgorithm)
derive instance newtypeKeyAlgorithmList :: Newtype KeyAlgorithmList _
derive instance repGenericKeyAlgorithmList :: Generic KeyAlgorithmList _
instance showKeyAlgorithmList :: Show KeyAlgorithmList where
  show = genericShow
instance decodeKeyAlgorithmList :: Decode KeyAlgorithmList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyAlgorithmList :: Encode KeyAlgorithmList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Key Usage X.509 v3 extension defines the purpose of the public key contained in the certificate.</p>
newtype KeyUsage = KeyUsage 
  { "Name" :: NullOrUndefined.NullOrUndefined (KeyUsageName)
  }
derive instance newtypeKeyUsage :: Newtype KeyUsage _
derive instance repGenericKeyUsage :: Generic KeyUsage _
instance showKeyUsage :: Show KeyUsage where
  show = genericShow
instance decodeKeyUsage :: Decode KeyUsage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyUsage :: Encode KeyUsage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyUsageFilterList = KeyUsageFilterList (Array KeyUsageName)
derive instance newtypeKeyUsageFilterList :: Newtype KeyUsageFilterList _
derive instance repGenericKeyUsageFilterList :: Generic KeyUsageFilterList _
instance showKeyUsageFilterList :: Show KeyUsageFilterList where
  show = genericShow
instance decodeKeyUsageFilterList :: Decode KeyUsageFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyUsageFilterList :: Encode KeyUsageFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyUsageList = KeyUsageList (Array KeyUsage)
derive instance newtypeKeyUsageList :: Newtype KeyUsageList _
derive instance repGenericKeyUsageList :: Generic KeyUsageList _
instance showKeyUsageList :: Show KeyUsageList where
  show = genericShow
instance decodeKeyUsageList :: Decode KeyUsageList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyUsageList :: Encode KeyUsageList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyUsageName = KeyUsageName String
derive instance newtypeKeyUsageName :: Newtype KeyUsageName _
derive instance repGenericKeyUsageName :: Generic KeyUsageName _
instance showKeyUsageName :: Show KeyUsageName where
  show = genericShow
instance decodeKeyUsageName :: Decode KeyUsageName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyUsageName :: Encode KeyUsageName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An ACM limit has been exceeded. For example, you may have input more domains than are allowed or you've requested too many certificates for your account. See the exception message returned by ACM to determine which limit you have violated. For more information about ACM limits, see the <a href="http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html">Limits</a> topic.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCertificatesRequest = ListCertificatesRequest 
  { "CertificateStatuses" :: NullOrUndefined.NullOrUndefined (CertificateStatuses)
  , "Includes" :: NullOrUndefined.NullOrUndefined (Filters)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (MaxItems)
  }
derive instance newtypeListCertificatesRequest :: Newtype ListCertificatesRequest _
derive instance repGenericListCertificatesRequest :: Generic ListCertificatesRequest _
instance showListCertificatesRequest :: Show ListCertificatesRequest where
  show = genericShow
instance decodeListCertificatesRequest :: Decode ListCertificatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCertificatesRequest :: Encode ListCertificatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCertificatesResponse = ListCertificatesResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "CertificateSummaryList" :: NullOrUndefined.NullOrUndefined (CertificateSummaryList)
  }
derive instance newtypeListCertificatesResponse :: Newtype ListCertificatesResponse _
derive instance repGenericListCertificatesResponse :: Generic ListCertificatesResponse _
instance showListCertificatesResponse :: Show ListCertificatesResponse where
  show = genericShow
instance decodeListCertificatesResponse :: Decode ListCertificatesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCertificatesResponse :: Encode ListCertificatesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsForCertificateRequest = ListTagsForCertificateRequest 
  { "CertificateArn" :: (Arn)
  }
derive instance newtypeListTagsForCertificateRequest :: Newtype ListTagsForCertificateRequest _
derive instance repGenericListTagsForCertificateRequest :: Generic ListTagsForCertificateRequest _
instance showListTagsForCertificateRequest :: Show ListTagsForCertificateRequest where
  show = genericShow
instance decodeListTagsForCertificateRequest :: Decode ListTagsForCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForCertificateRequest :: Encode ListTagsForCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsForCertificateResponse = ListTagsForCertificateResponse 
  { "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeListTagsForCertificateResponse :: Newtype ListTagsForCertificateResponse _
derive instance repGenericListTagsForCertificateResponse :: Generic ListTagsForCertificateResponse _
instance showListTagsForCertificateResponse :: Show ListTagsForCertificateResponse where
  show = genericShow
instance decodeListTagsForCertificateResponse :: Decode ListTagsForCertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForCertificateResponse :: Encode ListTagsForCertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxItems = MaxItems Int
derive instance newtypeMaxItems :: Newtype MaxItems _
derive instance repGenericMaxItems :: Generic MaxItems _
instance showMaxItems :: Show MaxItems where
  show = genericShow
instance decodeMaxItems :: Decode MaxItems where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxItems :: Encode MaxItems where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PrivateKeyBlob = PrivateKeyBlob String
derive instance newtypePrivateKeyBlob :: Newtype PrivateKeyBlob _
derive instance repGenericPrivateKeyBlob :: Generic PrivateKeyBlob _
instance showPrivateKeyBlob :: Show PrivateKeyBlob where
  show = genericShow
instance decodePrivateKeyBlob :: Decode PrivateKeyBlob where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrivateKeyBlob :: Encode PrivateKeyBlob where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordType = RecordType String
derive instance newtypeRecordType :: Newtype RecordType _
derive instance repGenericRecordType :: Generic RecordType _
instance showRecordType :: Show RecordType where
  show = genericShow
instance decodeRecordType :: Decode RecordType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordType :: Encode RecordType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTagsFromCertificateRequest = RemoveTagsFromCertificateRequest 
  { "CertificateArn" :: (Arn)
  , "Tags" :: (TagList)
  }
derive instance newtypeRemoveTagsFromCertificateRequest :: Newtype RemoveTagsFromCertificateRequest _
derive instance repGenericRemoveTagsFromCertificateRequest :: Generic RemoveTagsFromCertificateRequest _
instance showRemoveTagsFromCertificateRequest :: Show RemoveTagsFromCertificateRequest where
  show = genericShow
instance decodeRemoveTagsFromCertificateRequest :: Decode RemoveTagsFromCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsFromCertificateRequest :: Encode RemoveTagsFromCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RenewalStatus = RenewalStatus String
derive instance newtypeRenewalStatus :: Newtype RenewalStatus _
derive instance repGenericRenewalStatus :: Generic RenewalStatus _
instance showRenewalStatus :: Show RenewalStatus where
  show = genericShow
instance decodeRenewalStatus :: Decode RenewalStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRenewalStatus :: Encode RenewalStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the status of ACM's <a href="http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html">managed renewal</a> for the certificate. This structure exists only when the certificate type is <code>AMAZON_ISSUED</code>.</p>
newtype RenewalSummary = RenewalSummary 
  { "RenewalStatus" :: (RenewalStatus)
  , "DomainValidationOptions" :: (DomainValidationList)
  }
derive instance newtypeRenewalSummary :: Newtype RenewalSummary _
derive instance repGenericRenewalSummary :: Generic RenewalSummary _
instance showRenewalSummary :: Show RenewalSummary where
  show = genericShow
instance decodeRenewalSummary :: Decode RenewalSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRenewalSummary :: Encode RenewalSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequestCertificateRequest = RequestCertificateRequest 
  { "DomainName" :: (DomainNameString)
  , "ValidationMethod" :: NullOrUndefined.NullOrUndefined (ValidationMethod)
  , "SubjectAlternativeNames" :: NullOrUndefined.NullOrUndefined (DomainList)
  , "IdempotencyToken" :: NullOrUndefined.NullOrUndefined (IdempotencyToken)
  , "DomainValidationOptions" :: NullOrUndefined.NullOrUndefined (DomainValidationOptionList)
  }
derive instance newtypeRequestCertificateRequest :: Newtype RequestCertificateRequest _
derive instance repGenericRequestCertificateRequest :: Generic RequestCertificateRequest _
instance showRequestCertificateRequest :: Show RequestCertificateRequest where
  show = genericShow
instance decodeRequestCertificateRequest :: Decode RequestCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCertificateRequest :: Encode RequestCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequestCertificateResponse = RequestCertificateResponse 
  { "CertificateArn" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeRequestCertificateResponse :: Newtype RequestCertificateResponse _
derive instance repGenericRequestCertificateResponse :: Generic RequestCertificateResponse _
instance showRequestCertificateResponse :: Show RequestCertificateResponse where
  show = genericShow
instance decodeRequestCertificateResponse :: Decode RequestCertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCertificateResponse :: Encode RequestCertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The certificate request is in process and the certificate in your account has not yet been issued.</p>
newtype RequestInProgressException = RequestInProgressException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRequestInProgressException :: Newtype RequestInProgressException _
derive instance repGenericRequestInProgressException :: Generic RequestInProgressException _
instance showRequestInProgressException :: Show RequestInProgressException where
  show = genericShow
instance decodeRequestInProgressException :: Decode RequestInProgressException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestInProgressException :: Encode RequestInProgressException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResendValidationEmailRequest = ResendValidationEmailRequest 
  { "CertificateArn" :: (Arn)
  , "Domain" :: (DomainNameString)
  , "ValidationDomain" :: (DomainNameString)
  }
derive instance newtypeResendValidationEmailRequest :: Newtype ResendValidationEmailRequest _
derive instance repGenericResendValidationEmailRequest :: Generic ResendValidationEmailRequest _
instance showResendValidationEmailRequest :: Show ResendValidationEmailRequest where
  show = genericShow
instance decodeResendValidationEmailRequest :: Decode ResendValidationEmailRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResendValidationEmailRequest :: Encode ResendValidationEmailRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The certificate is in use by another AWS service in the caller's account. Remove the association and try again.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _
derive instance repGenericResourceInUseException :: Generic ResourceInUseException _
instance showResourceInUseException :: Show ResourceInUseException where
  show = genericShow
instance decodeResourceInUseException :: Decode ResourceInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceInUseException :: Encode ResourceInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified certificate cannot be found in the caller's account, or the caller's account cannot be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains a DNS record value that you can use to can use to validate ownership or control of a domain. This is used by the <a>DescribeCertificate</a> action. </p>
newtype ResourceRecord = ResourceRecord 
  { "Name" :: (String)
  , "Type" :: (RecordType)
  , "Value" :: (String)
  }
derive instance newtypeResourceRecord :: Newtype ResourceRecord _
derive instance repGenericResourceRecord :: Generic ResourceRecord _
instance showResourceRecord :: Show ResourceRecord where
  show = genericShow
instance decodeResourceRecord :: Decode ResourceRecord where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceRecord :: Encode ResourceRecord where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RevocationReason = RevocationReason String
derive instance newtypeRevocationReason :: Newtype RevocationReason _
derive instance repGenericRevocationReason :: Generic RevocationReason _
instance showRevocationReason :: Show RevocationReason where
  show = genericShow
instance decodeRevocationReason :: Decode RevocationReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevocationReason :: Encode RevocationReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TStamp = TStamp Number
derive instance newtypeTStamp :: Newtype TStamp _
derive instance repGenericTStamp :: Generic TStamp _
instance showTStamp :: Show TStamp where
  show = genericShow
instance decodeTStamp :: Decode TStamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTStamp :: Encode TStamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A key-value pair that identifies or specifies metadata about an ACM resource.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request contains too many tags. Try the request again with fewer tags.</p>
newtype TooManyTagsException = TooManyTagsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyTagsException :: Newtype TooManyTagsException _
derive instance repGenericTooManyTagsException :: Generic TooManyTagsException _
instance showTooManyTagsException :: Show TooManyTagsException where
  show = genericShow
instance decodeTooManyTagsException :: Decode TooManyTagsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyTagsException :: Encode TooManyTagsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ValidationEmailList = ValidationEmailList (Array String)
derive instance newtypeValidationEmailList :: Newtype ValidationEmailList _
derive instance repGenericValidationEmailList :: Generic ValidationEmailList _
instance showValidationEmailList :: Show ValidationEmailList where
  show = genericShow
instance decodeValidationEmailList :: Decode ValidationEmailList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationEmailList :: Encode ValidationEmailList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ValidationMethod = ValidationMethod String
derive instance newtypeValidationMethod :: Newtype ValidationMethod _
derive instance repGenericValidationMethod :: Generic ValidationMethod _
instance showValidationMethod :: Show ValidationMethod where
  show = genericShow
instance decodeValidationMethod :: Decode ValidationMethod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationMethod :: Encode ValidationMethod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
