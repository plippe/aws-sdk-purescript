

-- | <fullname>Amazon CloudFront</fullname> <p>This is the <i>Amazon CloudFront API Reference</i>. This guide is for developers who need detailed information about the CloudFront API actions, data types, and errors. For detailed information about CloudFront features and their associated API calls, see the <i>Amazon CloudFront Developer Guide</i>.</p>
module AWS.CloudFront where

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

serviceName = "CloudFront" :: String


-- | <p>Creates a new origin access identity. If you're using Amazon S3 for your origin, you can use an origin access identity to require users to access your content using a CloudFront URL instead of the Amazon S3 URL. For more information about how to use origin access identities, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html">Serving Private Content through CloudFront</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
createCloudFrontOriginAccessIdentity :: forall eff. CreateCloudFrontOriginAccessIdentityRequest -> Aff (exception :: EXCEPTION | eff) CreateCloudFrontOriginAccessIdentityResult
createCloudFrontOriginAccessIdentity = Request.request serviceName "createCloudFrontOriginAccessIdentity" 


-- | <p>Creates a new web distribution. Send a <code>GET</code> request to the <code>/<i>CloudFront API version</i>/distribution</code>/<code>distribution ID</code> resource.</p>
createDistribution :: forall eff. CreateDistributionRequest -> Aff (exception :: EXCEPTION | eff) CreateDistributionResult
createDistribution = Request.request serviceName "createDistribution" 


-- | <p>Create a new distribution with tags.</p>
createDistributionWithTags :: forall eff. CreateDistributionWithTagsRequest -> Aff (exception :: EXCEPTION | eff) CreateDistributionWithTagsResult
createDistributionWithTags = Request.request serviceName "createDistributionWithTags" 


-- | <p>Create a new invalidation. </p>
createInvalidation :: forall eff. CreateInvalidationRequest -> Aff (exception :: EXCEPTION | eff) CreateInvalidationResult
createInvalidation = Request.request serviceName "createInvalidation" 


-- | <p>Creates a new RMTP distribution. An RTMP distribution is similar to a web distribution, but an RTMP distribution streams media files using the Adobe Real-Time Messaging Protocol (RTMP) instead of serving files using HTTP. </p> <p>To create a new web distribution, submit a <code>POST</code> request to the <i>CloudFront API version</i>/distribution resource. The request body must include a document with a <i>StreamingDistributionConfig</i> element. The response echoes the <code>StreamingDistributionConfig</code> element and returns other information about the RTMP distribution.</p> <p>To get the status of your request, use the <i>GET StreamingDistribution</i> API action. When the value of <code>Enabled</code> is <code>true</code> and the value of <code>Status</code> is <code>Deployed</code>, your distribution is ready. A distribution usually deploys in less than 15 minutes.</p> <p>For more information about web distributions, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-rtmp.html">Working with RTMP Distributions</a> in the <i>Amazon CloudFront Developer Guide</i>.</p> <important> <p>Beginning with the 2012-05-05 version of the CloudFront API, we made substantial changes to the format of the XML document that you include in the request body when you create or update a web distribution or an RTMP distribution, and when you invalidate objects. With previous versions of the API, we discovered that it was too easy to accidentally delete one or more values for an element that accepts multiple values, for example, CNAMEs and trusted signers. Our changes for the 2012-05-05 release are intended to prevent these accidental deletions and to notify you when there's a mismatch between the number of values you say you're specifying in the <code>Quantity</code> element and the number of values specified.</p> </important>
createStreamingDistribution :: forall eff. CreateStreamingDistributionRequest -> Aff (exception :: EXCEPTION | eff) CreateStreamingDistributionResult
createStreamingDistribution = Request.request serviceName "createStreamingDistribution" 


-- | <p>Create a new streaming distribution with tags.</p>
createStreamingDistributionWithTags :: forall eff. CreateStreamingDistributionWithTagsRequest -> Aff (exception :: EXCEPTION | eff) CreateStreamingDistributionWithTagsResult
createStreamingDistributionWithTags = Request.request serviceName "createStreamingDistributionWithTags" 


-- | <p>Delete an origin access identity. </p>
deleteCloudFrontOriginAccessIdentity :: forall eff. DeleteCloudFrontOriginAccessIdentityRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteCloudFrontOriginAccessIdentity = Request.request serviceName "deleteCloudFrontOriginAccessIdentity" 


-- | <p>Delete a distribution. </p>
deleteDistribution :: forall eff. DeleteDistributionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteDistribution = Request.request serviceName "deleteDistribution" 


-- | <p>Delete a streaming distribution. To delete an RTMP distribution using the CloudFront API, perform the following steps.</p> <p> <b>To delete an RTMP distribution using the CloudFront API</b>:</p> <ol> <li> <p>Disable the RTMP distribution.</p> </li> <li> <p>Submit a <code>GET Streaming Distribution Config</code> request to get the current configuration and the <code>Etag</code> header for the distribution. </p> </li> <li> <p>Update the XML document that was returned in the response to your <code>GET Streaming Distribution Config</code> request to change the value of <code>Enabled</code> to <code>false</code>.</p> </li> <li> <p>Submit a <code>PUT Streaming Distribution Config</code> request to update the configuration for your distribution. In the request body, include the XML document that you updated in Step 3. Then set the value of the HTTP <code>If-Match</code> header to the value of the <code>ETag</code> header that CloudFront returned when you submitted the <code>GET Streaming Distribution Config</code> request in Step 2.</p> </li> <li> <p>Review the response to the <code>PUT Streaming Distribution Config</code> request to confirm that the distribution was successfully disabled.</p> </li> <li> <p>Submit a <code>GET Streaming Distribution Config</code> request to confirm that your changes have propagated. When propagation is complete, the value of <code>Status</code> is <code>Deployed</code>.</p> </li> <li> <p>Submit a <code>DELETE Streaming Distribution</code> request. Set the value of the HTTP <code>If-Match</code> header to the value of the <code>ETag</code> header that CloudFront returned when you submitted the <code>GET Streaming Distribution Config</code> request in Step 2.</p> </li> <li> <p>Review the response to your <code>DELETE Streaming Distribution</code> request to confirm that the distribution was successfully deleted.</p> </li> </ol> <p>For information about deleting a distribution using the CloudFront console, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/HowToDeleteDistribution.html">Deleting a Distribution</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
deleteStreamingDistribution :: forall eff. DeleteStreamingDistributionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteStreamingDistribution = Request.request serviceName "deleteStreamingDistribution" 


-- | <p>Get the information about an origin access identity. </p>
getCloudFrontOriginAccessIdentity :: forall eff. GetCloudFrontOriginAccessIdentityRequest -> Aff (exception :: EXCEPTION | eff) GetCloudFrontOriginAccessIdentityResult
getCloudFrontOriginAccessIdentity = Request.request serviceName "getCloudFrontOriginAccessIdentity" 


-- | <p>Get the configuration information about an origin access identity. </p>
getCloudFrontOriginAccessIdentityConfig :: forall eff. GetCloudFrontOriginAccessIdentityConfigRequest -> Aff (exception :: EXCEPTION | eff) GetCloudFrontOriginAccessIdentityConfigResult
getCloudFrontOriginAccessIdentityConfig = Request.request serviceName "getCloudFrontOriginAccessIdentityConfig" 


-- | <p>Get the information about a distribution. </p>
getDistribution :: forall eff. GetDistributionRequest -> Aff (exception :: EXCEPTION | eff) GetDistributionResult
getDistribution = Request.request serviceName "getDistribution" 


-- | <p>Get the configuration information about a distribution. </p>
getDistributionConfig :: forall eff. GetDistributionConfigRequest -> Aff (exception :: EXCEPTION | eff) GetDistributionConfigResult
getDistributionConfig = Request.request serviceName "getDistributionConfig" 


-- | <p>Get the information about an invalidation. </p>
getInvalidation :: forall eff. GetInvalidationRequest -> Aff (exception :: EXCEPTION | eff) GetInvalidationResult
getInvalidation = Request.request serviceName "getInvalidation" 


-- | <p>Gets information about a specified RTMP distribution, including the distribution configuration.</p>
getStreamingDistribution :: forall eff. GetStreamingDistributionRequest -> Aff (exception :: EXCEPTION | eff) GetStreamingDistributionResult
getStreamingDistribution = Request.request serviceName "getStreamingDistribution" 


-- | <p>Get the configuration information about a streaming distribution. </p>
getStreamingDistributionConfig :: forall eff. GetStreamingDistributionConfigRequest -> Aff (exception :: EXCEPTION | eff) GetStreamingDistributionConfigResult
getStreamingDistributionConfig = Request.request serviceName "getStreamingDistributionConfig" 


-- | <p>Lists origin access identities.</p>
listCloudFrontOriginAccessIdentities :: forall eff. ListCloudFrontOriginAccessIdentitiesRequest -> Aff (exception :: EXCEPTION | eff) ListCloudFrontOriginAccessIdentitiesResult
listCloudFrontOriginAccessIdentities = Request.request serviceName "listCloudFrontOriginAccessIdentities" 


-- | <p>List distributions. </p>
listDistributions :: forall eff. ListDistributionsRequest -> Aff (exception :: EXCEPTION | eff) ListDistributionsResult
listDistributions = Request.request serviceName "listDistributions" 


-- | <p>List the distributions that are associated with a specified AWS WAF web ACL. </p>
listDistributionsByWebACLId :: forall eff. ListDistributionsByWebACLIdRequest -> Aff (exception :: EXCEPTION | eff) ListDistributionsByWebACLIdResult
listDistributionsByWebACLId = Request.request serviceName "listDistributionsByWebACLId" 


-- | <p>Lists invalidation batches. </p>
listInvalidations :: forall eff. ListInvalidationsRequest -> Aff (exception :: EXCEPTION | eff) ListInvalidationsResult
listInvalidations = Request.request serviceName "listInvalidations" 


-- | <p>List streaming distributions. </p>
listStreamingDistributions :: forall eff. ListStreamingDistributionsRequest -> Aff (exception :: EXCEPTION | eff) ListStreamingDistributionsResult
listStreamingDistributions = Request.request serviceName "listStreamingDistributions" 


-- | <p>List tags for a CloudFront resource.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (exception :: EXCEPTION | eff) ListTagsForResourceResult
listTagsForResource = Request.request serviceName "listTagsForResource" 


-- | <p>Add tags to a CloudFront resource.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
tagResource = Request.request serviceName "tagResource" 


-- | <p>Remove tags from a CloudFront resource.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
untagResource = Request.request serviceName "untagResource" 


-- | <p>Update an origin access identity. </p>
updateCloudFrontOriginAccessIdentity :: forall eff. UpdateCloudFrontOriginAccessIdentityRequest -> Aff (exception :: EXCEPTION | eff) UpdateCloudFrontOriginAccessIdentityResult
updateCloudFrontOriginAccessIdentity = Request.request serviceName "updateCloudFrontOriginAccessIdentity" 


-- | <p>Update a distribution. </p>
updateDistribution :: forall eff. UpdateDistributionRequest -> Aff (exception :: EXCEPTION | eff) UpdateDistributionResult
updateDistribution = Request.request serviceName "updateDistribution" 


-- | <p>Update a streaming distribution. </p>
updateStreamingDistribution :: forall eff. UpdateStreamingDistributionRequest -> Aff (exception :: EXCEPTION | eff) UpdateStreamingDistributionResult
updateStreamingDistribution = Request.request serviceName "updateStreamingDistribution" 


-- | <p>Access denied.</p>
newtype AccessDenied = AccessDenied 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAccessDenied :: Newtype AccessDenied _
derive instance repGenericAccessDenied :: Generic AccessDenied _
instance showAccessDenied :: Show AccessDenied where
  show = genericShow
instance decodeAccessDenied :: Decode AccessDenied where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessDenied :: Encode AccessDenied where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that lists the AWS accounts, if any, that you included in the <code>TrustedSigners</code> complex type for this distribution. These are the accounts that you want to allow to create signed URLs for private content.</p> <p>The <code>Signer</code> complex type lists the AWS account number of the trusted signer or <code>self</code> if the signer is the AWS account that created the distribution. The <code>Signer</code> element also includes the IDs of any active CloudFront key pairs that are associated with the trusted signer's AWS account. If no <code>KeyPairId</code> element appears for a <code>Signer</code>, that signer can't create signed URLs. </p> <p>For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html">Serving Private Content through CloudFront</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype ActiveTrustedSigners = ActiveTrustedSigners 
  { "Enabled" :: (Boolean)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (SignerList)
  }
derive instance newtypeActiveTrustedSigners :: Newtype ActiveTrustedSigners _
derive instance repGenericActiveTrustedSigners :: Generic ActiveTrustedSigners _
instance showActiveTrustedSigners :: Show ActiveTrustedSigners where
  show = genericShow
instance decodeActiveTrustedSigners :: Decode ActiveTrustedSigners where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActiveTrustedSigners :: Encode ActiveTrustedSigners where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AliasList = AliasList (Array String)
derive instance newtypeAliasList :: Newtype AliasList _
derive instance repGenericAliasList :: Generic AliasList _
instance showAliasList :: Show AliasList where
  show = genericShow
instance decodeAliasList :: Decode AliasList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasList :: Encode AliasList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution. </p>
newtype Aliases = Aliases 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (AliasList)
  }
derive instance newtypeAliases :: Newtype Aliases _
derive instance repGenericAliases :: Generic Aliases _
instance showAliases :: Show Aliases where
  show = genericShow
instance decodeAliases :: Decode Aliases where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliases :: Encode Aliases where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that controls which HTTP methods CloudFront processes and forwards to your Amazon S3 bucket or your custom origin. There are three choices:</p> <ul> <li> <p>CloudFront forwards only <code>GET</code> and <code>HEAD</code> requests.</p> </li> <li> <p>CloudFront forwards only <code>GET</code>, <code>HEAD</code>, and <code>OPTIONS</code> requests.</p> </li> <li> <p>CloudFront forwards <code>GET, HEAD, OPTIONS, PUT, PATCH, POST</code>, and <code>DELETE</code> requests.</p> </li> </ul> <p>If you pick the third choice, you may need to restrict access to your Amazon S3 bucket or to your custom origin so users can't perform operations that you don't want them to. For example, you might not want users to have permissions to delete objects from your origin.</p>
newtype AllowedMethods = AllowedMethods 
  { "Quantity" :: (Int)
  , "Items" :: (MethodsList)
  , "CachedMethods" :: NullOrUndefined.NullOrUndefined (CachedMethods)
  }
derive instance newtypeAllowedMethods :: Newtype AllowedMethods _
derive instance repGenericAllowedMethods :: Generic AllowedMethods _
instance showAllowedMethods :: Show AllowedMethods where
  show = genericShow
instance decodeAllowedMethods :: Decode AllowedMethods where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedMethods :: Encode AllowedMethods where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AwsAccountNumberList = AwsAccountNumberList (Array String)
derive instance newtypeAwsAccountNumberList :: Newtype AwsAccountNumberList _
derive instance repGenericAwsAccountNumberList :: Generic AwsAccountNumberList _
instance showAwsAccountNumberList :: Show AwsAccountNumberList where
  show = genericShow
instance decodeAwsAccountNumberList :: Decode AwsAccountNumberList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsAccountNumberList :: Encode AwsAccountNumberList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchTooLarge = BatchTooLarge 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBatchTooLarge :: Newtype BatchTooLarge _
derive instance repGenericBatchTooLarge :: Generic BatchTooLarge _
instance showBatchTooLarge :: Show BatchTooLarge where
  show = genericShow
instance decodeBatchTooLarge :: Decode BatchTooLarge where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchTooLarge :: Encode BatchTooLarge where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CNAMEAlreadyExists = CNAMEAlreadyExists 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCNAMEAlreadyExists :: Newtype CNAMEAlreadyExists _
derive instance repGenericCNAMEAlreadyExists :: Generic CNAMEAlreadyExists _
instance showCNAMEAlreadyExists :: Show CNAMEAlreadyExists where
  show = genericShow
instance decodeCNAMEAlreadyExists :: Decode CNAMEAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCNAMEAlreadyExists :: Encode CNAMEAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that describes how CloudFront processes requests.</p> <p>You must create at least as many cache behaviors (including the default cache behavior) as you have origins if you want CloudFront to distribute objects from all of the origins. Each cache behavior specifies the one origin from which you want CloudFront to get objects. If you have two origins and only the default cache behavior, the default cache behavior will cause CloudFront to get objects from one of the origins, but the other origin is never used.</p> <p>For the current limit on the number of cache behaviors that you can add to a distribution, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_cloudfront">Amazon CloudFront Limits</a> in the <i>AWS General Reference</i>.</p> <p>If you don't want to specify any cache behaviors, include only an empty <code>CacheBehaviors</code> element. Don't include an empty <code>CacheBehavior</code> element, or CloudFront returns a <code>MalformedXML</code> error.</p> <p>To delete all cache behaviors in an existing distribution, update the distribution configuration and include only an empty <code>CacheBehaviors</code> element.</p> <p>To add, change, or remove one or more cache behaviors, update the distribution configuration and specify all of the cache behaviors that you want to include in the updated distribution.</p> <p>For more information about cache behaviors, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior">Cache Behaviors</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype CacheBehavior = CacheBehavior 
  { "PathPattern" :: (String)
  , "TargetOriginId" :: (String)
  , "ForwardedValues" :: (ForwardedValues)
  , "TrustedSigners" :: (TrustedSigners)
  , "ViewerProtocolPolicy" :: (ViewerProtocolPolicy)
  , "MinTTL" :: (Number)
  , "AllowedMethods" :: NullOrUndefined.NullOrUndefined (AllowedMethods)
  , "SmoothStreaming" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "DefaultTTL" :: NullOrUndefined.NullOrUndefined (Number)
  , "MaxTTL" :: NullOrUndefined.NullOrUndefined (Number)
  , "Compress" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LambdaFunctionAssociations" :: NullOrUndefined.NullOrUndefined (LambdaFunctionAssociations)
  }
derive instance newtypeCacheBehavior :: Newtype CacheBehavior _
derive instance repGenericCacheBehavior :: Generic CacheBehavior _
instance showCacheBehavior :: Show CacheBehavior where
  show = genericShow
instance decodeCacheBehavior :: Decode CacheBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheBehavior :: Encode CacheBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheBehaviorList = CacheBehaviorList (Array CacheBehavior)
derive instance newtypeCacheBehaviorList :: Newtype CacheBehaviorList _
derive instance repGenericCacheBehaviorList :: Generic CacheBehaviorList _
instance showCacheBehaviorList :: Show CacheBehaviorList where
  show = genericShow
instance decodeCacheBehaviorList :: Decode CacheBehaviorList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheBehaviorList :: Encode CacheBehaviorList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains zero or more <code>CacheBehavior</code> elements. </p>
newtype CacheBehaviors = CacheBehaviors 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (CacheBehaviorList)
  }
derive instance newtypeCacheBehaviors :: Newtype CacheBehaviors _
derive instance repGenericCacheBehaviors :: Generic CacheBehaviors _
instance showCacheBehaviors :: Show CacheBehaviors where
  show = genericShow
instance decodeCacheBehaviors :: Decode CacheBehaviors where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheBehaviors :: Encode CacheBehaviors where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that controls whether CloudFront caches the response to requests using the specified HTTP methods. There are two choices:</p> <ul> <li> <p>CloudFront caches responses to <code>GET</code> and <code>HEAD</code> requests.</p> </li> <li> <p>CloudFront caches responses to <code>GET</code>, <code>HEAD</code>, and <code>OPTIONS</code> requests.</p> </li> </ul> <p>If you pick the second choice for your Amazon S3 Origin, you may need to forward Access-Control-Request-Method, Access-Control-Request-Headers, and Origin headers for the responses to be cached correctly. </p>
newtype CachedMethods = CachedMethods 
  { "Quantity" :: (Int)
  , "Items" :: (MethodsList)
  }
derive instance newtypeCachedMethods :: Newtype CachedMethods _
derive instance repGenericCachedMethods :: Generic CachedMethods _
instance showCachedMethods :: Show CachedMethods where
  show = genericShow
instance decodeCachedMethods :: Decode CachedMethods where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCachedMethods :: Encode CachedMethods where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateSource = CertificateSource String
derive instance newtypeCertificateSource :: Newtype CertificateSource _
derive instance repGenericCertificateSource :: Generic CertificateSource _
instance showCertificateSource :: Show CertificateSource where
  show = genericShow
instance decodeCertificateSource :: Decode CertificateSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateSource :: Encode CertificateSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CloudFront origin access identity.</p>
newtype CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity 
  { "Id" :: (String)
  , "S3CanonicalUserId" :: (String)
  , "CloudFrontOriginAccessIdentityConfig" :: NullOrUndefined.NullOrUndefined (CloudFrontOriginAccessIdentityConfig)
  }
derive instance newtypeCloudFrontOriginAccessIdentity :: Newtype CloudFrontOriginAccessIdentity _
derive instance repGenericCloudFrontOriginAccessIdentity :: Generic CloudFrontOriginAccessIdentity _
instance showCloudFrontOriginAccessIdentity :: Show CloudFrontOriginAccessIdentity where
  show = genericShow
instance decodeCloudFrontOriginAccessIdentity :: Decode CloudFrontOriginAccessIdentity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudFrontOriginAccessIdentity :: Encode CloudFrontOriginAccessIdentity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>If the <code>CallerReference</code> is a value you already sent in a previous request to create an identity but the content of the <code>CloudFrontOriginAccessIdentityConfig</code> is different from the original request, CloudFront returns a <code>CloudFrontOriginAccessIdentityAlreadyExists</code> error. </p>
newtype CloudFrontOriginAccessIdentityAlreadyExists = CloudFrontOriginAccessIdentityAlreadyExists 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCloudFrontOriginAccessIdentityAlreadyExists :: Newtype CloudFrontOriginAccessIdentityAlreadyExists _
derive instance repGenericCloudFrontOriginAccessIdentityAlreadyExists :: Generic CloudFrontOriginAccessIdentityAlreadyExists _
instance showCloudFrontOriginAccessIdentityAlreadyExists :: Show CloudFrontOriginAccessIdentityAlreadyExists where
  show = genericShow
instance decodeCloudFrontOriginAccessIdentityAlreadyExists :: Decode CloudFrontOriginAccessIdentityAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudFrontOriginAccessIdentityAlreadyExists :: Encode CloudFrontOriginAccessIdentityAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Origin access identity configuration. Send a <code>GET</code> request to the <code>/<i>CloudFront API version</i>/CloudFront/identity ID/config</code> resource. </p>
newtype CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig 
  { "CallerReference" :: (String)
  , "Comment" :: (String)
  }
derive instance newtypeCloudFrontOriginAccessIdentityConfig :: Newtype CloudFrontOriginAccessIdentityConfig _
derive instance repGenericCloudFrontOriginAccessIdentityConfig :: Generic CloudFrontOriginAccessIdentityConfig _
instance showCloudFrontOriginAccessIdentityConfig :: Show CloudFrontOriginAccessIdentityConfig where
  show = genericShow
instance decodeCloudFrontOriginAccessIdentityConfig :: Decode CloudFrontOriginAccessIdentityConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudFrontOriginAccessIdentityConfig :: Encode CloudFrontOriginAccessIdentityConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudFrontOriginAccessIdentityInUse = CloudFrontOriginAccessIdentityInUse 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCloudFrontOriginAccessIdentityInUse :: Newtype CloudFrontOriginAccessIdentityInUse _
derive instance repGenericCloudFrontOriginAccessIdentityInUse :: Generic CloudFrontOriginAccessIdentityInUse _
instance showCloudFrontOriginAccessIdentityInUse :: Show CloudFrontOriginAccessIdentityInUse where
  show = genericShow
instance decodeCloudFrontOriginAccessIdentityInUse :: Decode CloudFrontOriginAccessIdentityInUse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudFrontOriginAccessIdentityInUse :: Encode CloudFrontOriginAccessIdentityInUse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lists the origin access identities for CloudFront.Send a <code>GET</code> request to the <code>/<i>CloudFront API version</i>/origin-access-identity/cloudfront</code> resource. The response includes a <code>CloudFrontOriginAccessIdentityList</code> element with zero or more <code>CloudFrontOriginAccessIdentitySummary</code> child elements. By default, your entire list of origin access identities is returned in one single page. If the list is long, you can paginate it using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
newtype CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList 
  { "Marker" :: (String)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxItems" :: (Int)
  , "IsTruncated" :: (Boolean)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (CloudFrontOriginAccessIdentitySummaryList)
  }
derive instance newtypeCloudFrontOriginAccessIdentityList :: Newtype CloudFrontOriginAccessIdentityList _
derive instance repGenericCloudFrontOriginAccessIdentityList :: Generic CloudFrontOriginAccessIdentityList _
instance showCloudFrontOriginAccessIdentityList :: Show CloudFrontOriginAccessIdentityList where
  show = genericShow
instance decodeCloudFrontOriginAccessIdentityList :: Decode CloudFrontOriginAccessIdentityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudFrontOriginAccessIdentityList :: Encode CloudFrontOriginAccessIdentityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Summary of the information about a CloudFront origin access identity.</p>
newtype CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary 
  { "Id" :: (String)
  , "S3CanonicalUserId" :: (String)
  , "Comment" :: (String)
  }
derive instance newtypeCloudFrontOriginAccessIdentitySummary :: Newtype CloudFrontOriginAccessIdentitySummary _
derive instance repGenericCloudFrontOriginAccessIdentitySummary :: Generic CloudFrontOriginAccessIdentitySummary _
instance showCloudFrontOriginAccessIdentitySummary :: Show CloudFrontOriginAccessIdentitySummary where
  show = genericShow
instance decodeCloudFrontOriginAccessIdentitySummary :: Decode CloudFrontOriginAccessIdentitySummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudFrontOriginAccessIdentitySummary :: Encode CloudFrontOriginAccessIdentitySummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudFrontOriginAccessIdentitySummaryList = CloudFrontOriginAccessIdentitySummaryList (Array CloudFrontOriginAccessIdentitySummary)
derive instance newtypeCloudFrontOriginAccessIdentitySummaryList :: Newtype CloudFrontOriginAccessIdentitySummaryList _
derive instance repGenericCloudFrontOriginAccessIdentitySummaryList :: Generic CloudFrontOriginAccessIdentitySummaryList _
instance showCloudFrontOriginAccessIdentitySummaryList :: Show CloudFrontOriginAccessIdentitySummaryList where
  show = genericShow
instance decodeCloudFrontOriginAccessIdentitySummaryList :: Decode CloudFrontOriginAccessIdentitySummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudFrontOriginAccessIdentitySummaryList :: Encode CloudFrontOriginAccessIdentitySummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CookieNameList = CookieNameList (Array String)
derive instance newtypeCookieNameList :: Newtype CookieNameList _
derive instance repGenericCookieNameList :: Generic CookieNameList _
instance showCookieNameList :: Show CookieNameList where
  show = genericShow
instance decodeCookieNameList :: Decode CookieNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCookieNameList :: Encode CookieNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html">How CloudFront Forwards, Caches, and Logs Cookies</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype CookieNames = CookieNames 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (CookieNameList)
  }
derive instance newtypeCookieNames :: Newtype CookieNames _
derive instance repGenericCookieNames :: Generic CookieNames _
instance showCookieNames :: Show CookieNames where
  show = genericShow
instance decodeCookieNames :: Decode CookieNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCookieNames :: Encode CookieNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html">How CloudFront Forwards, Caches, and Logs Cookies</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype CookiePreference = CookiePreference 
  { "Forward" :: (ItemSelection)
  , "WhitelistedNames" :: NullOrUndefined.NullOrUndefined (CookieNames)
  }
derive instance newtypeCookiePreference :: Newtype CookiePreference _
derive instance repGenericCookiePreference :: Generic CookiePreference _
instance showCookiePreference :: Show CookiePreference where
  show = genericShow
instance decodeCookiePreference :: Decode CookiePreference where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCookiePreference :: Encode CookiePreference where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to create a new origin access identity.</p>
newtype CreateCloudFrontOriginAccessIdentityRequest = CreateCloudFrontOriginAccessIdentityRequest 
  { "CloudFrontOriginAccessIdentityConfig" :: (CloudFrontOriginAccessIdentityConfig)
  }
derive instance newtypeCreateCloudFrontOriginAccessIdentityRequest :: Newtype CreateCloudFrontOriginAccessIdentityRequest _
derive instance repGenericCreateCloudFrontOriginAccessIdentityRequest :: Generic CreateCloudFrontOriginAccessIdentityRequest _
instance showCreateCloudFrontOriginAccessIdentityRequest :: Show CreateCloudFrontOriginAccessIdentityRequest where
  show = genericShow
instance decodeCreateCloudFrontOriginAccessIdentityRequest :: Decode CreateCloudFrontOriginAccessIdentityRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCloudFrontOriginAccessIdentityRequest :: Encode CreateCloudFrontOriginAccessIdentityRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype CreateCloudFrontOriginAccessIdentityResult = CreateCloudFrontOriginAccessIdentityResult 
  { "CloudFrontOriginAccessIdentity" :: NullOrUndefined.NullOrUndefined (CloudFrontOriginAccessIdentity)
  , "Location" :: NullOrUndefined.NullOrUndefined (String)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateCloudFrontOriginAccessIdentityResult :: Newtype CreateCloudFrontOriginAccessIdentityResult _
derive instance repGenericCreateCloudFrontOriginAccessIdentityResult :: Generic CreateCloudFrontOriginAccessIdentityResult _
instance showCreateCloudFrontOriginAccessIdentityResult :: Show CreateCloudFrontOriginAccessIdentityResult where
  show = genericShow
instance decodeCreateCloudFrontOriginAccessIdentityResult :: Decode CreateCloudFrontOriginAccessIdentityResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCloudFrontOriginAccessIdentityResult :: Encode CreateCloudFrontOriginAccessIdentityResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to create a new distribution.</p>
newtype CreateDistributionRequest = CreateDistributionRequest 
  { "DistributionConfig" :: (DistributionConfig)
  }
derive instance newtypeCreateDistributionRequest :: Newtype CreateDistributionRequest _
derive instance repGenericCreateDistributionRequest :: Generic CreateDistributionRequest _
instance showCreateDistributionRequest :: Show CreateDistributionRequest where
  show = genericShow
instance decodeCreateDistributionRequest :: Decode CreateDistributionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDistributionRequest :: Encode CreateDistributionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype CreateDistributionResult = CreateDistributionResult 
  { "Distribution" :: NullOrUndefined.NullOrUndefined (Distribution)
  , "Location" :: NullOrUndefined.NullOrUndefined (String)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateDistributionResult :: Newtype CreateDistributionResult _
derive instance repGenericCreateDistributionResult :: Generic CreateDistributionResult _
instance showCreateDistributionResult :: Show CreateDistributionResult where
  show = genericShow
instance decodeCreateDistributionResult :: Decode CreateDistributionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDistributionResult :: Encode CreateDistributionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to create a new distribution with tags. </p>
newtype CreateDistributionWithTagsRequest = CreateDistributionWithTagsRequest 
  { "DistributionConfigWithTags" :: (DistributionConfigWithTags)
  }
derive instance newtypeCreateDistributionWithTagsRequest :: Newtype CreateDistributionWithTagsRequest _
derive instance repGenericCreateDistributionWithTagsRequest :: Generic CreateDistributionWithTagsRequest _
instance showCreateDistributionWithTagsRequest :: Show CreateDistributionWithTagsRequest where
  show = genericShow
instance decodeCreateDistributionWithTagsRequest :: Decode CreateDistributionWithTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDistributionWithTagsRequest :: Encode CreateDistributionWithTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request. </p>
newtype CreateDistributionWithTagsResult = CreateDistributionWithTagsResult 
  { "Distribution" :: NullOrUndefined.NullOrUndefined (Distribution)
  , "Location" :: NullOrUndefined.NullOrUndefined (String)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateDistributionWithTagsResult :: Newtype CreateDistributionWithTagsResult _
derive instance repGenericCreateDistributionWithTagsResult :: Generic CreateDistributionWithTagsResult _
instance showCreateDistributionWithTagsResult :: Show CreateDistributionWithTagsResult where
  show = genericShow
instance decodeCreateDistributionWithTagsResult :: Decode CreateDistributionWithTagsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDistributionWithTagsResult :: Encode CreateDistributionWithTagsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to create an invalidation.</p>
newtype CreateInvalidationRequest = CreateInvalidationRequest 
  { "DistributionId" :: (String)
  , "InvalidationBatch" :: (InvalidationBatch)
  }
derive instance newtypeCreateInvalidationRequest :: Newtype CreateInvalidationRequest _
derive instance repGenericCreateInvalidationRequest :: Generic CreateInvalidationRequest _
instance showCreateInvalidationRequest :: Show CreateInvalidationRequest where
  show = genericShow
instance decodeCreateInvalidationRequest :: Decode CreateInvalidationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInvalidationRequest :: Encode CreateInvalidationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype CreateInvalidationResult = CreateInvalidationResult 
  { "Location" :: NullOrUndefined.NullOrUndefined (String)
  , "Invalidation" :: NullOrUndefined.NullOrUndefined (Invalidation)
  }
derive instance newtypeCreateInvalidationResult :: Newtype CreateInvalidationResult _
derive instance repGenericCreateInvalidationResult :: Generic CreateInvalidationResult _
instance showCreateInvalidationResult :: Show CreateInvalidationResult where
  show = genericShow
instance decodeCreateInvalidationResult :: Decode CreateInvalidationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInvalidationResult :: Encode CreateInvalidationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to create a new streaming distribution.</p>
newtype CreateStreamingDistributionRequest = CreateStreamingDistributionRequest 
  { "StreamingDistributionConfig" :: (StreamingDistributionConfig)
  }
derive instance newtypeCreateStreamingDistributionRequest :: Newtype CreateStreamingDistributionRequest _
derive instance repGenericCreateStreamingDistributionRequest :: Generic CreateStreamingDistributionRequest _
instance showCreateStreamingDistributionRequest :: Show CreateStreamingDistributionRequest where
  show = genericShow
instance decodeCreateStreamingDistributionRequest :: Decode CreateStreamingDistributionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStreamingDistributionRequest :: Encode CreateStreamingDistributionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype CreateStreamingDistributionResult = CreateStreamingDistributionResult 
  { "StreamingDistribution" :: NullOrUndefined.NullOrUndefined (StreamingDistribution)
  , "Location" :: NullOrUndefined.NullOrUndefined (String)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateStreamingDistributionResult :: Newtype CreateStreamingDistributionResult _
derive instance repGenericCreateStreamingDistributionResult :: Generic CreateStreamingDistributionResult _
instance showCreateStreamingDistributionResult :: Show CreateStreamingDistributionResult where
  show = genericShow
instance decodeCreateStreamingDistributionResult :: Decode CreateStreamingDistributionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStreamingDistributionResult :: Encode CreateStreamingDistributionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to create a new streaming distribution with tags.</p>
newtype CreateStreamingDistributionWithTagsRequest = CreateStreamingDistributionWithTagsRequest 
  { "StreamingDistributionConfigWithTags" :: (StreamingDistributionConfigWithTags)
  }
derive instance newtypeCreateStreamingDistributionWithTagsRequest :: Newtype CreateStreamingDistributionWithTagsRequest _
derive instance repGenericCreateStreamingDistributionWithTagsRequest :: Generic CreateStreamingDistributionWithTagsRequest _
instance showCreateStreamingDistributionWithTagsRequest :: Show CreateStreamingDistributionWithTagsRequest where
  show = genericShow
instance decodeCreateStreamingDistributionWithTagsRequest :: Decode CreateStreamingDistributionWithTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStreamingDistributionWithTagsRequest :: Encode CreateStreamingDistributionWithTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request. </p>
newtype CreateStreamingDistributionWithTagsResult = CreateStreamingDistributionWithTagsResult 
  { "StreamingDistribution" :: NullOrUndefined.NullOrUndefined (StreamingDistribution)
  , "Location" :: NullOrUndefined.NullOrUndefined (String)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateStreamingDistributionWithTagsResult :: Newtype CreateStreamingDistributionWithTagsResult _
derive instance repGenericCreateStreamingDistributionWithTagsResult :: Generic CreateStreamingDistributionWithTagsResult _
instance showCreateStreamingDistributionWithTagsResult :: Show CreateStreamingDistributionWithTagsResult where
  show = genericShow
instance decodeCreateStreamingDistributionWithTagsResult :: Decode CreateStreamingDistributionWithTagsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStreamingDistributionWithTagsResult :: Encode CreateStreamingDistributionWithTagsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that controls:</p> <ul> <li> <p>Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer. </p> </li> <li> <p>How long CloudFront caches HTTP status codes in the 4xx and 5xx range.</p> </li> </ul> <p>For more information about custom error pages, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html">Customizing Error Responses</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype CustomErrorResponse = CustomErrorResponse 
  { "ErrorCode" :: (Int)
  , "ResponsePagePath" :: NullOrUndefined.NullOrUndefined (String)
  , "ResponseCode" :: NullOrUndefined.NullOrUndefined (String)
  , "ErrorCachingMinTTL" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeCustomErrorResponse :: Newtype CustomErrorResponse _
derive instance repGenericCustomErrorResponse :: Generic CustomErrorResponse _
instance showCustomErrorResponse :: Show CustomErrorResponse where
  show = genericShow
instance decodeCustomErrorResponse :: Decode CustomErrorResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomErrorResponse :: Encode CustomErrorResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomErrorResponseList = CustomErrorResponseList (Array CustomErrorResponse)
derive instance newtypeCustomErrorResponseList :: Newtype CustomErrorResponseList _
derive instance repGenericCustomErrorResponseList :: Generic CustomErrorResponseList _
instance showCustomErrorResponseList :: Show CustomErrorResponseList where
  show = genericShow
instance decodeCustomErrorResponseList :: Decode CustomErrorResponseList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomErrorResponseList :: Encode CustomErrorResponseList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that controls:</p> <ul> <li> <p>Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.</p> </li> <li> <p>How long CloudFront caches HTTP status codes in the 4xx and 5xx range.</p> </li> </ul> <p>For more information about custom error pages, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html">Customizing Error Responses</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype CustomErrorResponses = CustomErrorResponses 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (CustomErrorResponseList)
  }
derive instance newtypeCustomErrorResponses :: Newtype CustomErrorResponses _
derive instance repGenericCustomErrorResponses :: Generic CustomErrorResponses _
instance showCustomErrorResponses :: Show CustomErrorResponses where
  show = genericShow
instance decodeCustomErrorResponses :: Decode CustomErrorResponses where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomErrorResponses :: Encode CustomErrorResponses where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the list of Custom Headers for each origin. </p>
newtype CustomHeaders = CustomHeaders 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (OriginCustomHeadersList)
  }
derive instance newtypeCustomHeaders :: Newtype CustomHeaders _
derive instance repGenericCustomHeaders :: Generic CustomHeaders _
instance showCustomHeaders :: Show CustomHeaders where
  show = genericShow
instance decodeCustomHeaders :: Decode CustomHeaders where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomHeaders :: Encode CustomHeaders where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A customer origin.</p>
newtype CustomOriginConfig = CustomOriginConfig 
  { "HTTPPort" :: (Int)
  , "HTTPSPort" :: (Int)
  , "OriginProtocolPolicy" :: (OriginProtocolPolicy)
  , "OriginSslProtocols" :: NullOrUndefined.NullOrUndefined (OriginSslProtocols)
  }
derive instance newtypeCustomOriginConfig :: Newtype CustomOriginConfig _
derive instance repGenericCustomOriginConfig :: Generic CustomOriginConfig _
instance showCustomOriginConfig :: Show CustomOriginConfig where
  show = genericShow
instance decodeCustomOriginConfig :: Decode CustomOriginConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomOriginConfig :: Encode CustomOriginConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that describes the default cache behavior if you do not specify a <code>CacheBehavior</code> element or if files don't match any of the values of <code>PathPattern</code> in <code>CacheBehavior</code> elements. You must create exactly one default cache behavior.</p>
newtype DefaultCacheBehavior = DefaultCacheBehavior 
  { "TargetOriginId" :: (String)
  , "ForwardedValues" :: (ForwardedValues)
  , "TrustedSigners" :: (TrustedSigners)
  , "ViewerProtocolPolicy" :: (ViewerProtocolPolicy)
  , "MinTTL" :: (Number)
  , "AllowedMethods" :: NullOrUndefined.NullOrUndefined (AllowedMethods)
  , "SmoothStreaming" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "DefaultTTL" :: NullOrUndefined.NullOrUndefined (Number)
  , "MaxTTL" :: NullOrUndefined.NullOrUndefined (Number)
  , "Compress" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LambdaFunctionAssociations" :: NullOrUndefined.NullOrUndefined (LambdaFunctionAssociations)
  }
derive instance newtypeDefaultCacheBehavior :: Newtype DefaultCacheBehavior _
derive instance repGenericDefaultCacheBehavior :: Generic DefaultCacheBehavior _
instance showDefaultCacheBehavior :: Show DefaultCacheBehavior where
  show = genericShow
instance decodeDefaultCacheBehavior :: Decode DefaultCacheBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultCacheBehavior :: Encode DefaultCacheBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Deletes a origin access identity.</p>
newtype DeleteCloudFrontOriginAccessIdentityRequest = DeleteCloudFrontOriginAccessIdentityRequest 
  { "Id" :: (String)
  , "IfMatch" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDeleteCloudFrontOriginAccessIdentityRequest :: Newtype DeleteCloudFrontOriginAccessIdentityRequest _
derive instance repGenericDeleteCloudFrontOriginAccessIdentityRequest :: Generic DeleteCloudFrontOriginAccessIdentityRequest _
instance showDeleteCloudFrontOriginAccessIdentityRequest :: Show DeleteCloudFrontOriginAccessIdentityRequest where
  show = genericShow
instance decodeDeleteCloudFrontOriginAccessIdentityRequest :: Decode DeleteCloudFrontOriginAccessIdentityRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCloudFrontOriginAccessIdentityRequest :: Encode DeleteCloudFrontOriginAccessIdentityRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This action deletes a web distribution. To delete a web distribution using the CloudFront API, perform the following steps.</p> <p> <b>To delete a web distribution using the CloudFront API:</b> </p> <ol> <li> <p>Disable the web distribution </p> </li> <li> <p>Submit a <code>GET Distribution Config</code> request to get the current configuration and the <code>Etag</code> header for the distribution.</p> </li> <li> <p>Update the XML document that was returned in the response to your <code>GET Distribution Config</code> request to change the value of <code>Enabled</code> to <code>false</code>.</p> </li> <li> <p>Submit a <code>PUT Distribution Config</code> request to update the configuration for your distribution. In the request body, include the XML document that you updated in Step 3. Set the value of the HTTP <code>If-Match</code> header to the value of the <code>ETag</code> header that CloudFront returned when you submitted the <code>GET Distribution Config</code> request in Step 2.</p> </li> <li> <p>Review the response to the <code>PUT Distribution Config</code> request to confirm that the distribution was successfully disabled.</p> </li> <li> <p>Submit a <code>GET Distribution</code> request to confirm that your changes have propagated. When propagation is complete, the value of <code>Status</code> is <code>Deployed</code>.</p> </li> <li> <p>Submit a <code>DELETE Distribution</code> request. Set the value of the HTTP <code>If-Match</code> header to the value of the <code>ETag</code> header that CloudFront returned when you submitted the <code>GET Distribution Config</code> request in Step 6.</p> </li> <li> <p>Review the response to your <code>DELETE Distribution</code> request to confirm that the distribution was successfully deleted.</p> </li> </ol> <p>For information about deleting a distribution using the CloudFront console, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/HowToDeleteDistribution.html">Deleting a Distribution</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype DeleteDistributionRequest = DeleteDistributionRequest 
  { "Id" :: (String)
  , "IfMatch" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDeleteDistributionRequest :: Newtype DeleteDistributionRequest _
derive instance repGenericDeleteDistributionRequest :: Generic DeleteDistributionRequest _
instance showDeleteDistributionRequest :: Show DeleteDistributionRequest where
  show = genericShow
instance decodeDeleteDistributionRequest :: Decode DeleteDistributionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDistributionRequest :: Encode DeleteDistributionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to delete a streaming distribution.</p>
newtype DeleteStreamingDistributionRequest = DeleteStreamingDistributionRequest 
  { "Id" :: (String)
  , "IfMatch" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDeleteStreamingDistributionRequest :: Newtype DeleteStreamingDistributionRequest _
derive instance repGenericDeleteStreamingDistributionRequest :: Generic DeleteStreamingDistributionRequest _
instance showDeleteStreamingDistributionRequest :: Show DeleteStreamingDistributionRequest where
  show = genericShow
instance decodeDeleteStreamingDistributionRequest :: Decode DeleteStreamingDistributionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStreamingDistributionRequest :: Encode DeleteStreamingDistributionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The distribution's information.</p>
newtype Distribution = Distribution 
  { "Id" :: (String)
  , "ARN" :: (String)
  , "Status" :: (String)
  , "LastModifiedTime" :: (Number)
  , "InProgressInvalidationBatches" :: (Int)
  , "DomainName" :: (String)
  , "ActiveTrustedSigners" :: (ActiveTrustedSigners)
  , "DistributionConfig" :: (DistributionConfig)
  }
derive instance newtypeDistribution :: Newtype Distribution _
derive instance repGenericDistribution :: Generic Distribution _
instance showDistribution :: Show Distribution where
  show = genericShow
instance decodeDistribution :: Decode Distribution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDistribution :: Encode Distribution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The caller reference you attempted to create the distribution with is associated with another distribution.</p>
newtype DistributionAlreadyExists = DistributionAlreadyExists 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDistributionAlreadyExists :: Newtype DistributionAlreadyExists _
derive instance repGenericDistributionAlreadyExists :: Generic DistributionAlreadyExists _
instance showDistributionAlreadyExists :: Show DistributionAlreadyExists where
  show = genericShow
instance decodeDistributionAlreadyExists :: Decode DistributionAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDistributionAlreadyExists :: Encode DistributionAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A distribution configuration.</p>
newtype DistributionConfig = DistributionConfig 
  { "CallerReference" :: (String)
  , "Aliases" :: NullOrUndefined.NullOrUndefined (Aliases)
  , "DefaultRootObject" :: NullOrUndefined.NullOrUndefined (String)
  , "Origins" :: (Origins)
  , "DefaultCacheBehavior" :: (DefaultCacheBehavior)
  , "CacheBehaviors" :: NullOrUndefined.NullOrUndefined (CacheBehaviors)
  , "CustomErrorResponses" :: NullOrUndefined.NullOrUndefined (CustomErrorResponses)
  , "Comment" :: (String)
  , "Logging" :: NullOrUndefined.NullOrUndefined (LoggingConfig)
  , "PriceClass" :: NullOrUndefined.NullOrUndefined (PriceClass)
  , "Enabled" :: (Boolean)
  , "ViewerCertificate" :: NullOrUndefined.NullOrUndefined (ViewerCertificate)
  , "Restrictions" :: NullOrUndefined.NullOrUndefined (Restrictions)
  , "WebACLId" :: NullOrUndefined.NullOrUndefined (String)
  , "HttpVersion" :: NullOrUndefined.NullOrUndefined (HttpVersion)
  , "IsIPV6Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeDistributionConfig :: Newtype DistributionConfig _
derive instance repGenericDistributionConfig :: Generic DistributionConfig _
instance showDistributionConfig :: Show DistributionConfig where
  show = genericShow
instance decodeDistributionConfig :: Decode DistributionConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDistributionConfig :: Encode DistributionConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A distribution Configuration and a list of tags to be associated with the distribution.</p>
newtype DistributionConfigWithTags = DistributionConfigWithTags 
  { "DistributionConfig" :: (DistributionConfig)
  , "Tags" :: (Tags)
  }
derive instance newtypeDistributionConfigWithTags :: Newtype DistributionConfigWithTags _
derive instance repGenericDistributionConfigWithTags :: Generic DistributionConfigWithTags _
instance showDistributionConfigWithTags :: Show DistributionConfigWithTags where
  show = genericShow
instance decodeDistributionConfigWithTags :: Decode DistributionConfigWithTags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDistributionConfigWithTags :: Encode DistributionConfigWithTags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A distribution list.</p>
newtype DistributionList = DistributionList 
  { "Marker" :: (String)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxItems" :: (Int)
  , "IsTruncated" :: (Boolean)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (DistributionSummaryList)
  }
derive instance newtypeDistributionList :: Newtype DistributionList _
derive instance repGenericDistributionList :: Generic DistributionList _
instance showDistributionList :: Show DistributionList where
  show = genericShow
instance decodeDistributionList :: Decode DistributionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDistributionList :: Encode DistributionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DistributionNotDisabled = DistributionNotDisabled 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDistributionNotDisabled :: Newtype DistributionNotDisabled _
derive instance repGenericDistributionNotDisabled :: Generic DistributionNotDisabled _
instance showDistributionNotDisabled :: Show DistributionNotDisabled where
  show = genericShow
instance decodeDistributionNotDisabled :: Decode DistributionNotDisabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDistributionNotDisabled :: Encode DistributionNotDisabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A summary of the information about a CloudFront distribution.</p>
newtype DistributionSummary = DistributionSummary 
  { "Id" :: (String)
  , "ARN" :: (String)
  , "Status" :: (String)
  , "LastModifiedTime" :: (Number)
  , "DomainName" :: (String)
  , "Aliases" :: (Aliases)
  , "Origins" :: (Origins)
  , "DefaultCacheBehavior" :: (DefaultCacheBehavior)
  , "CacheBehaviors" :: (CacheBehaviors)
  , "CustomErrorResponses" :: (CustomErrorResponses)
  , "Comment" :: (String)
  , "PriceClass" :: (PriceClass)
  , "Enabled" :: (Boolean)
  , "ViewerCertificate" :: (ViewerCertificate)
  , "Restrictions" :: (Restrictions)
  , "WebACLId" :: (String)
  , "HttpVersion" :: (HttpVersion)
  , "IsIPV6Enabled" :: (Boolean)
  }
derive instance newtypeDistributionSummary :: Newtype DistributionSummary _
derive instance repGenericDistributionSummary :: Generic DistributionSummary _
instance showDistributionSummary :: Show DistributionSummary where
  show = genericShow
instance decodeDistributionSummary :: Decode DistributionSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDistributionSummary :: Encode DistributionSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DistributionSummaryList = DistributionSummaryList (Array DistributionSummary)
derive instance newtypeDistributionSummaryList :: Newtype DistributionSummaryList _
derive instance repGenericDistributionSummaryList :: Generic DistributionSummaryList _
instance showDistributionSummaryList :: Show DistributionSummaryList where
  show = genericShow
instance decodeDistributionSummaryList :: Decode DistributionSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDistributionSummaryList :: Encode DistributionSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventType = EventType String
derive instance newtypeEventType :: Newtype EventType _
derive instance repGenericEventType :: Generic EventType _
instance showEventType :: Show EventType where
  show = genericShow
instance decodeEventType :: Decode EventType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventType :: Encode EventType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that specifies how CloudFront handles query strings and cookies.</p>
newtype ForwardedValues = ForwardedValues 
  { "QueryString" :: (Boolean)
  , "Cookies" :: (CookiePreference)
  , "Headers" :: NullOrUndefined.NullOrUndefined (Headers)
  , "QueryStringCacheKeys" :: NullOrUndefined.NullOrUndefined (QueryStringCacheKeys)
  }
derive instance newtypeForwardedValues :: Newtype ForwardedValues _
derive instance repGenericForwardedValues :: Generic ForwardedValues _
instance showForwardedValues :: Show ForwardedValues where
  show = genericShow
instance decodeForwardedValues :: Decode ForwardedValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeForwardedValues :: Encode ForwardedValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using <code>MaxMind</code> GeoIP databases. </p>
newtype GeoRestriction = GeoRestriction 
  { "RestrictionType" :: (GeoRestrictionType)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (LocationList)
  }
derive instance newtypeGeoRestriction :: Newtype GeoRestriction _
derive instance repGenericGeoRestriction :: Generic GeoRestriction _
instance showGeoRestriction :: Show GeoRestriction where
  show = genericShow
instance decodeGeoRestriction :: Decode GeoRestriction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoRestriction :: Encode GeoRestriction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoRestrictionType = GeoRestrictionType String
derive instance newtypeGeoRestrictionType :: Newtype GeoRestrictionType _
derive instance repGenericGeoRestrictionType :: Generic GeoRestrictionType _
instance showGeoRestrictionType :: Show GeoRestrictionType where
  show = genericShow
instance decodeGeoRestrictionType :: Decode GeoRestrictionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoRestrictionType :: Encode GeoRestrictionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The origin access identity's configuration information. For more information, see <a>CloudFrontOriginAccessIdentityConfigComplexType</a>.</p>
newtype GetCloudFrontOriginAccessIdentityConfigRequest = GetCloudFrontOriginAccessIdentityConfigRequest 
  { "Id" :: (String)
  }
derive instance newtypeGetCloudFrontOriginAccessIdentityConfigRequest :: Newtype GetCloudFrontOriginAccessIdentityConfigRequest _
derive instance repGenericGetCloudFrontOriginAccessIdentityConfigRequest :: Generic GetCloudFrontOriginAccessIdentityConfigRequest _
instance showGetCloudFrontOriginAccessIdentityConfigRequest :: Show GetCloudFrontOriginAccessIdentityConfigRequest where
  show = genericShow
instance decodeGetCloudFrontOriginAccessIdentityConfigRequest :: Decode GetCloudFrontOriginAccessIdentityConfigRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCloudFrontOriginAccessIdentityConfigRequest :: Encode GetCloudFrontOriginAccessIdentityConfigRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype GetCloudFrontOriginAccessIdentityConfigResult = GetCloudFrontOriginAccessIdentityConfigResult 
  { "CloudFrontOriginAccessIdentityConfig" :: NullOrUndefined.NullOrUndefined (CloudFrontOriginAccessIdentityConfig)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetCloudFrontOriginAccessIdentityConfigResult :: Newtype GetCloudFrontOriginAccessIdentityConfigResult _
derive instance repGenericGetCloudFrontOriginAccessIdentityConfigResult :: Generic GetCloudFrontOriginAccessIdentityConfigResult _
instance showGetCloudFrontOriginAccessIdentityConfigResult :: Show GetCloudFrontOriginAccessIdentityConfigResult where
  show = genericShow
instance decodeGetCloudFrontOriginAccessIdentityConfigResult :: Decode GetCloudFrontOriginAccessIdentityConfigResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCloudFrontOriginAccessIdentityConfigResult :: Encode GetCloudFrontOriginAccessIdentityConfigResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to get an origin access identity's information.</p>
newtype GetCloudFrontOriginAccessIdentityRequest = GetCloudFrontOriginAccessIdentityRequest 
  { "Id" :: (String)
  }
derive instance newtypeGetCloudFrontOriginAccessIdentityRequest :: Newtype GetCloudFrontOriginAccessIdentityRequest _
derive instance repGenericGetCloudFrontOriginAccessIdentityRequest :: Generic GetCloudFrontOriginAccessIdentityRequest _
instance showGetCloudFrontOriginAccessIdentityRequest :: Show GetCloudFrontOriginAccessIdentityRequest where
  show = genericShow
instance decodeGetCloudFrontOriginAccessIdentityRequest :: Decode GetCloudFrontOriginAccessIdentityRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCloudFrontOriginAccessIdentityRequest :: Encode GetCloudFrontOriginAccessIdentityRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype GetCloudFrontOriginAccessIdentityResult = GetCloudFrontOriginAccessIdentityResult 
  { "CloudFrontOriginAccessIdentity" :: NullOrUndefined.NullOrUndefined (CloudFrontOriginAccessIdentity)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetCloudFrontOriginAccessIdentityResult :: Newtype GetCloudFrontOriginAccessIdentityResult _
derive instance repGenericGetCloudFrontOriginAccessIdentityResult :: Generic GetCloudFrontOriginAccessIdentityResult _
instance showGetCloudFrontOriginAccessIdentityResult :: Show GetCloudFrontOriginAccessIdentityResult where
  show = genericShow
instance decodeGetCloudFrontOriginAccessIdentityResult :: Decode GetCloudFrontOriginAccessIdentityResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCloudFrontOriginAccessIdentityResult :: Encode GetCloudFrontOriginAccessIdentityResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to get a distribution configuration.</p>
newtype GetDistributionConfigRequest = GetDistributionConfigRequest 
  { "Id" :: (String)
  }
derive instance newtypeGetDistributionConfigRequest :: Newtype GetDistributionConfigRequest _
derive instance repGenericGetDistributionConfigRequest :: Generic GetDistributionConfigRequest _
instance showGetDistributionConfigRequest :: Show GetDistributionConfigRequest where
  show = genericShow
instance decodeGetDistributionConfigRequest :: Decode GetDistributionConfigRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDistributionConfigRequest :: Encode GetDistributionConfigRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype GetDistributionConfigResult = GetDistributionConfigResult 
  { "DistributionConfig" :: NullOrUndefined.NullOrUndefined (DistributionConfig)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetDistributionConfigResult :: Newtype GetDistributionConfigResult _
derive instance repGenericGetDistributionConfigResult :: Generic GetDistributionConfigResult _
instance showGetDistributionConfigResult :: Show GetDistributionConfigResult where
  show = genericShow
instance decodeGetDistributionConfigResult :: Decode GetDistributionConfigResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDistributionConfigResult :: Encode GetDistributionConfigResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to get a distribution's information.</p>
newtype GetDistributionRequest = GetDistributionRequest 
  { "Id" :: (String)
  }
derive instance newtypeGetDistributionRequest :: Newtype GetDistributionRequest _
derive instance repGenericGetDistributionRequest :: Generic GetDistributionRequest _
instance showGetDistributionRequest :: Show GetDistributionRequest where
  show = genericShow
instance decodeGetDistributionRequest :: Decode GetDistributionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDistributionRequest :: Encode GetDistributionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype GetDistributionResult = GetDistributionResult 
  { "Distribution" :: NullOrUndefined.NullOrUndefined (Distribution)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetDistributionResult :: Newtype GetDistributionResult _
derive instance repGenericGetDistributionResult :: Generic GetDistributionResult _
instance showGetDistributionResult :: Show GetDistributionResult where
  show = genericShow
instance decodeGetDistributionResult :: Decode GetDistributionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDistributionResult :: Encode GetDistributionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to get an invalidation's information. </p>
newtype GetInvalidationRequest = GetInvalidationRequest 
  { "DistributionId" :: (String)
  , "Id" :: (String)
  }
derive instance newtypeGetInvalidationRequest :: Newtype GetInvalidationRequest _
derive instance repGenericGetInvalidationRequest :: Generic GetInvalidationRequest _
instance showGetInvalidationRequest :: Show GetInvalidationRequest where
  show = genericShow
instance decodeGetInvalidationRequest :: Decode GetInvalidationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInvalidationRequest :: Encode GetInvalidationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype GetInvalidationResult = GetInvalidationResult 
  { "Invalidation" :: NullOrUndefined.NullOrUndefined (Invalidation)
  }
derive instance newtypeGetInvalidationResult :: Newtype GetInvalidationResult _
derive instance repGenericGetInvalidationResult :: Generic GetInvalidationResult _
instance showGetInvalidationResult :: Show GetInvalidationResult where
  show = genericShow
instance decodeGetInvalidationResult :: Decode GetInvalidationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInvalidationResult :: Encode GetInvalidationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>To request to get a streaming distribution configuration.</p>
newtype GetStreamingDistributionConfigRequest = GetStreamingDistributionConfigRequest 
  { "Id" :: (String)
  }
derive instance newtypeGetStreamingDistributionConfigRequest :: Newtype GetStreamingDistributionConfigRequest _
derive instance repGenericGetStreamingDistributionConfigRequest :: Generic GetStreamingDistributionConfigRequest _
instance showGetStreamingDistributionConfigRequest :: Show GetStreamingDistributionConfigRequest where
  show = genericShow
instance decodeGetStreamingDistributionConfigRequest :: Decode GetStreamingDistributionConfigRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStreamingDistributionConfigRequest :: Encode GetStreamingDistributionConfigRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype GetStreamingDistributionConfigResult = GetStreamingDistributionConfigResult 
  { "StreamingDistributionConfig" :: NullOrUndefined.NullOrUndefined (StreamingDistributionConfig)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetStreamingDistributionConfigResult :: Newtype GetStreamingDistributionConfigResult _
derive instance repGenericGetStreamingDistributionConfigResult :: Generic GetStreamingDistributionConfigResult _
instance showGetStreamingDistributionConfigResult :: Show GetStreamingDistributionConfigResult where
  show = genericShow
instance decodeGetStreamingDistributionConfigResult :: Decode GetStreamingDistributionConfigResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStreamingDistributionConfigResult :: Encode GetStreamingDistributionConfigResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to get a streaming distribution's information.</p>
newtype GetStreamingDistributionRequest = GetStreamingDistributionRequest 
  { "Id" :: (String)
  }
derive instance newtypeGetStreamingDistributionRequest :: Newtype GetStreamingDistributionRequest _
derive instance repGenericGetStreamingDistributionRequest :: Generic GetStreamingDistributionRequest _
instance showGetStreamingDistributionRequest :: Show GetStreamingDistributionRequest where
  show = genericShow
instance decodeGetStreamingDistributionRequest :: Decode GetStreamingDistributionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStreamingDistributionRequest :: Encode GetStreamingDistributionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype GetStreamingDistributionResult = GetStreamingDistributionResult 
  { "StreamingDistribution" :: NullOrUndefined.NullOrUndefined (StreamingDistribution)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetStreamingDistributionResult :: Newtype GetStreamingDistributionResult _
derive instance repGenericGetStreamingDistributionResult :: Generic GetStreamingDistributionResult _
instance showGetStreamingDistributionResult :: Show GetStreamingDistributionResult where
  show = genericShow
instance decodeGetStreamingDistributionResult :: Decode GetStreamingDistributionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStreamingDistributionResult :: Encode GetStreamingDistributionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HeaderList = HeaderList (Array String)
derive instance newtypeHeaderList :: Newtype HeaderList _
derive instance repGenericHeaderList :: Generic HeaderList _
instance showHeaderList :: Show HeaderList where
  show = genericShow
instance decodeHeaderList :: Decode HeaderList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHeaderList :: Encode HeaderList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that specifies the headers that you want CloudFront to forward to the origin for this cache behavior.</p> <p>For the headers that you specify, CloudFront also caches separate versions of a specified object based on the header values in viewer requests. For example, suppose viewer requests for <code>logo.jpg</code> contain a custom <code>Product</code> header that has a value of either <code>Acme</code> or <code>Apex</code>, and you configure CloudFront to cache your content based on values in the <code>Product</code> header. CloudFront forwards the <code>Product</code> header to the origin and caches the response from the origin once for each header value. For more information about caching based on header values, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html">How CloudFront Forwards and Caches Headers</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype Headers = Headers 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (HeaderList)
  }
derive instance newtypeHeaders :: Newtype Headers _
derive instance repGenericHeaders :: Generic Headers _
instance showHeaders :: Show Headers where
  show = genericShow
instance decodeHeaders :: Decode Headers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHeaders :: Encode Headers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HttpVersion = HttpVersion String
derive instance newtypeHttpVersion :: Newtype HttpVersion _
derive instance repGenericHttpVersion :: Generic HttpVersion _
instance showHttpVersion :: Show HttpVersion where
  show = genericShow
instance decodeHttpVersion :: Decode HttpVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHttpVersion :: Encode HttpVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Origin and <code>CallerReference</code> cannot be updated. </p>
newtype IllegalUpdate = IllegalUpdate 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeIllegalUpdate :: Newtype IllegalUpdate _
derive instance repGenericIllegalUpdate :: Generic IllegalUpdate _
instance showIllegalUpdate :: Show IllegalUpdate where
  show = genericShow
instance decodeIllegalUpdate :: Decode IllegalUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIllegalUpdate :: Encode IllegalUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value of <code>Quantity</code> and the size of <code>Items</code> do not match.</p>
newtype InconsistentQuantities = InconsistentQuantities 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInconsistentQuantities :: Newtype InconsistentQuantities _
derive instance repGenericInconsistentQuantities :: Generic InconsistentQuantities _
instance showInconsistentQuantities :: Show InconsistentQuantities where
  show = genericShow
instance decodeInconsistentQuantities :: Decode InconsistentQuantities where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInconsistentQuantities :: Encode InconsistentQuantities where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The argument is invalid.</p>
newtype InvalidArgument = InvalidArgument 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidArgument :: Newtype InvalidArgument _
derive instance repGenericInvalidArgument :: Generic InvalidArgument _
instance showInvalidArgument :: Show InvalidArgument where
  show = genericShow
instance decodeInvalidArgument :: Decode InvalidArgument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidArgument :: Encode InvalidArgument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The default root object file name is too big or contains an invalid character.</p>
newtype InvalidDefaultRootObject = InvalidDefaultRootObject 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidDefaultRootObject :: Newtype InvalidDefaultRootObject _
derive instance repGenericInvalidDefaultRootObject :: Generic InvalidDefaultRootObject _
instance showInvalidDefaultRootObject :: Show InvalidDefaultRootObject where
  show = genericShow
instance decodeInvalidDefaultRootObject :: Decode InvalidDefaultRootObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDefaultRootObject :: Encode InvalidDefaultRootObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidErrorCode = InvalidErrorCode 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidErrorCode :: Newtype InvalidErrorCode _
derive instance repGenericInvalidErrorCode :: Generic InvalidErrorCode _
instance showInvalidErrorCode :: Show InvalidErrorCode where
  show = genericShow
instance decodeInvalidErrorCode :: Decode InvalidErrorCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidErrorCode :: Encode InvalidErrorCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Your request contains forward cookies option which doesn't match with the expectation for the <code>whitelisted</code> list of cookie names. Either list of cookie names has been specified when not allowed or list of cookie names is missing when expected.</p>
newtype InvalidForwardCookies = InvalidForwardCookies 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidForwardCookies :: Newtype InvalidForwardCookies _
derive instance repGenericInvalidForwardCookies :: Generic InvalidForwardCookies _
instance showInvalidForwardCookies :: Show InvalidForwardCookies where
  show = genericShow
instance decodeInvalidForwardCookies :: Decode InvalidForwardCookies where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidForwardCookies :: Encode InvalidForwardCookies where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidGeoRestrictionParameter = InvalidGeoRestrictionParameter 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidGeoRestrictionParameter :: Newtype InvalidGeoRestrictionParameter _
derive instance repGenericInvalidGeoRestrictionParameter :: Generic InvalidGeoRestrictionParameter _
instance showInvalidGeoRestrictionParameter :: Show InvalidGeoRestrictionParameter where
  show = genericShow
instance decodeInvalidGeoRestrictionParameter :: Decode InvalidGeoRestrictionParameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidGeoRestrictionParameter :: Encode InvalidGeoRestrictionParameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidHeadersForS3Origin = InvalidHeadersForS3Origin 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidHeadersForS3Origin :: Newtype InvalidHeadersForS3Origin _
derive instance repGenericInvalidHeadersForS3Origin :: Generic InvalidHeadersForS3Origin _
instance showInvalidHeadersForS3Origin :: Show InvalidHeadersForS3Origin where
  show = genericShow
instance decodeInvalidHeadersForS3Origin :: Decode InvalidHeadersForS3Origin where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidHeadersForS3Origin :: Encode InvalidHeadersForS3Origin where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>If-Match</code> version is missing or not valid for the distribution.</p>
newtype InvalidIfMatchVersion = InvalidIfMatchVersion 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidIfMatchVersion :: Newtype InvalidIfMatchVersion _
derive instance repGenericInvalidIfMatchVersion :: Generic InvalidIfMatchVersion _
instance showInvalidIfMatchVersion :: Show InvalidIfMatchVersion where
  show = genericShow
instance decodeInvalidIfMatchVersion :: Decode InvalidIfMatchVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidIfMatchVersion :: Encode InvalidIfMatchVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified Lambda function association is invalid.</p>
newtype InvalidLambdaFunctionAssociation = InvalidLambdaFunctionAssociation 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidLambdaFunctionAssociation :: Newtype InvalidLambdaFunctionAssociation _
derive instance repGenericInvalidLambdaFunctionAssociation :: Generic InvalidLambdaFunctionAssociation _
instance showInvalidLambdaFunctionAssociation :: Show InvalidLambdaFunctionAssociation where
  show = genericShow
instance decodeInvalidLambdaFunctionAssociation :: Decode InvalidLambdaFunctionAssociation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidLambdaFunctionAssociation :: Encode InvalidLambdaFunctionAssociation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidLocationCode = InvalidLocationCode 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidLocationCode :: Newtype InvalidLocationCode _
derive instance repGenericInvalidLocationCode :: Generic InvalidLocationCode _
instance showInvalidLocationCode :: Show InvalidLocationCode where
  show = genericShow
instance decodeInvalidLocationCode :: Decode InvalidLocationCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidLocationCode :: Encode InvalidLocationCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidMinimumProtocolVersion = InvalidMinimumProtocolVersion 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidMinimumProtocolVersion :: Newtype InvalidMinimumProtocolVersion _
derive instance repGenericInvalidMinimumProtocolVersion :: Generic InvalidMinimumProtocolVersion _
instance showInvalidMinimumProtocolVersion :: Show InvalidMinimumProtocolVersion where
  show = genericShow
instance decodeInvalidMinimumProtocolVersion :: Decode InvalidMinimumProtocolVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidMinimumProtocolVersion :: Encode InvalidMinimumProtocolVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon S3 origin server specified does not refer to a valid Amazon S3 bucket.</p>
newtype InvalidOrigin = InvalidOrigin 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidOrigin :: Newtype InvalidOrigin _
derive instance repGenericInvalidOrigin :: Generic InvalidOrigin _
instance showInvalidOrigin :: Show InvalidOrigin where
  show = genericShow
instance decodeInvalidOrigin :: Decode InvalidOrigin where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidOrigin :: Encode InvalidOrigin where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The origin access identity is not valid or doesn't exist.</p>
newtype InvalidOriginAccessIdentity = InvalidOriginAccessIdentity 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidOriginAccessIdentity :: Newtype InvalidOriginAccessIdentity _
derive instance repGenericInvalidOriginAccessIdentity :: Generic InvalidOriginAccessIdentity _
instance showInvalidOriginAccessIdentity :: Show InvalidOriginAccessIdentity where
  show = genericShow
instance decodeInvalidOriginAccessIdentity :: Decode InvalidOriginAccessIdentity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidOriginAccessIdentity :: Encode InvalidOriginAccessIdentity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot specify SSLv3 as the minimum protocol version if you only want to support only clients that support Server Name Indication (SNI).</p>
newtype InvalidProtocolSettings = InvalidProtocolSettings 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidProtocolSettings :: Newtype InvalidProtocolSettings _
derive instance repGenericInvalidProtocolSettings :: Generic InvalidProtocolSettings _
instance showInvalidProtocolSettings :: Show InvalidProtocolSettings where
  show = genericShow
instance decodeInvalidProtocolSettings :: Decode InvalidProtocolSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidProtocolSettings :: Encode InvalidProtocolSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidQueryStringParameters = InvalidQueryStringParameters 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidQueryStringParameters :: Newtype InvalidQueryStringParameters _
derive instance repGenericInvalidQueryStringParameters :: Generic InvalidQueryStringParameters _
instance showInvalidQueryStringParameters :: Show InvalidQueryStringParameters where
  show = genericShow
instance decodeInvalidQueryStringParameters :: Decode InvalidQueryStringParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidQueryStringParameters :: Encode InvalidQueryStringParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The relative path is too big, is not URL-encoded, or does not begin with a slash (/).</p>
newtype InvalidRelativePath = InvalidRelativePath 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidRelativePath :: Newtype InvalidRelativePath _
derive instance repGenericInvalidRelativePath :: Generic InvalidRelativePath _
instance showInvalidRelativePath :: Show InvalidRelativePath where
  show = genericShow
instance decodeInvalidRelativePath :: Decode InvalidRelativePath where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRelativePath :: Encode InvalidRelativePath where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This operation requires the HTTPS protocol. Ensure that you specify the HTTPS protocol in your request, or omit the <code>RequiredProtocols</code> element from your distribution configuration.</p>
newtype InvalidRequiredProtocol = InvalidRequiredProtocol 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidRequiredProtocol :: Newtype InvalidRequiredProtocol _
derive instance repGenericInvalidRequiredProtocol :: Generic InvalidRequiredProtocol _
instance showInvalidRequiredProtocol :: Show InvalidRequiredProtocol where
  show = genericShow
instance decodeInvalidRequiredProtocol :: Decode InvalidRequiredProtocol where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRequiredProtocol :: Encode InvalidRequiredProtocol where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidResponseCode = InvalidResponseCode 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidResponseCode :: Newtype InvalidResponseCode _
derive instance repGenericInvalidResponseCode :: Generic InvalidResponseCode _
instance showInvalidResponseCode :: Show InvalidResponseCode where
  show = genericShow
instance decodeInvalidResponseCode :: Decode InvalidResponseCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidResponseCode :: Encode InvalidResponseCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidTTLOrder = InvalidTTLOrder 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidTTLOrder :: Newtype InvalidTTLOrder _
derive instance repGenericInvalidTTLOrder :: Generic InvalidTTLOrder _
instance showInvalidTTLOrder :: Show InvalidTTLOrder where
  show = genericShow
instance decodeInvalidTTLOrder :: Decode InvalidTTLOrder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTTLOrder :: Encode InvalidTTLOrder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidTagging = InvalidTagging 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidTagging :: Newtype InvalidTagging _
derive instance repGenericInvalidTagging :: Generic InvalidTagging _
instance showInvalidTagging :: Show InvalidTagging where
  show = genericShow
instance decodeInvalidTagging :: Decode InvalidTagging where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTagging :: Encode InvalidTagging where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidViewerCertificate = InvalidViewerCertificate 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidViewerCertificate :: Newtype InvalidViewerCertificate _
derive instance repGenericInvalidViewerCertificate :: Generic InvalidViewerCertificate _
instance showInvalidViewerCertificate :: Show InvalidViewerCertificate where
  show = genericShow
instance decodeInvalidViewerCertificate :: Decode InvalidViewerCertificate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidViewerCertificate :: Encode InvalidViewerCertificate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidWebACLId = InvalidWebACLId 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidWebACLId :: Newtype InvalidWebACLId _
derive instance repGenericInvalidWebACLId :: Generic InvalidWebACLId _
instance showInvalidWebACLId :: Show InvalidWebACLId where
  show = genericShow
instance decodeInvalidWebACLId :: Decode InvalidWebACLId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidWebACLId :: Encode InvalidWebACLId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An invalidation. </p>
newtype Invalidation = Invalidation 
  { "Id" :: (String)
  , "Status" :: (String)
  , "CreateTime" :: (Number)
  , "InvalidationBatch" :: (InvalidationBatch)
  }
derive instance newtypeInvalidation :: Newtype Invalidation _
derive instance repGenericInvalidation :: Generic Invalidation _
instance showInvalidation :: Show Invalidation where
  show = genericShow
instance decodeInvalidation :: Decode Invalidation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidation :: Encode Invalidation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An invalidation batch.</p>
newtype InvalidationBatch = InvalidationBatch 
  { "Paths" :: (Paths)
  , "CallerReference" :: (String)
  }
derive instance newtypeInvalidationBatch :: Newtype InvalidationBatch _
derive instance repGenericInvalidationBatch :: Generic InvalidationBatch _
instance showInvalidationBatch :: Show InvalidationBatch where
  show = genericShow
instance decodeInvalidationBatch :: Decode InvalidationBatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidationBatch :: Encode InvalidationBatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>InvalidationList</code> complex type describes the list of invalidation objects. For more information about invalidation, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html">Invalidating Objects (Web Distributions Only)</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype InvalidationList = InvalidationList 
  { "Marker" :: (String)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxItems" :: (Int)
  , "IsTruncated" :: (Boolean)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (InvalidationSummaryList)
  }
derive instance newtypeInvalidationList :: Newtype InvalidationList _
derive instance repGenericInvalidationList :: Generic InvalidationList _
instance showInvalidationList :: Show InvalidationList where
  show = genericShow
instance decodeInvalidationList :: Decode InvalidationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidationList :: Encode InvalidationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A summary of an invalidation request.</p>
newtype InvalidationSummary = InvalidationSummary 
  { "Id" :: (String)
  , "CreateTime" :: (Number)
  , "Status" :: (String)
  }
derive instance newtypeInvalidationSummary :: Newtype InvalidationSummary _
derive instance repGenericInvalidationSummary :: Generic InvalidationSummary _
instance showInvalidationSummary :: Show InvalidationSummary where
  show = genericShow
instance decodeInvalidationSummary :: Decode InvalidationSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidationSummary :: Encode InvalidationSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidationSummaryList = InvalidationSummaryList (Array InvalidationSummary)
derive instance newtypeInvalidationSummaryList :: Newtype InvalidationSummaryList _
derive instance repGenericInvalidationSummaryList :: Generic InvalidationSummaryList _
instance showInvalidationSummaryList :: Show InvalidationSummaryList where
  show = genericShow
instance decodeInvalidationSummaryList :: Decode InvalidationSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidationSummaryList :: Encode InvalidationSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ItemSelection = ItemSelection String
derive instance newtypeItemSelection :: Newtype ItemSelection _
derive instance repGenericItemSelection :: Generic ItemSelection _
instance showItemSelection :: Show ItemSelection where
  show = genericShow
instance decodeItemSelection :: Decode ItemSelection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeItemSelection :: Encode ItemSelection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyPairIdList = KeyPairIdList (Array String)
derive instance newtypeKeyPairIdList :: Newtype KeyPairIdList _
derive instance repGenericKeyPairIdList :: Generic KeyPairIdList _
instance showKeyPairIdList :: Show KeyPairIdList where
  show = genericShow
instance decodeKeyPairIdList :: Decode KeyPairIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyPairIdList :: Encode KeyPairIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that lists the active CloudFront key pairs, if any, that are associated with <code>AwsAccountNumber</code>. </p> <p>For more information, see <a>ActiveTrustedSigners</a>.</p>
newtype KeyPairIds = KeyPairIds 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (KeyPairIdList)
  }
derive instance newtypeKeyPairIds :: Newtype KeyPairIds _
derive instance repGenericKeyPairIds :: Generic KeyPairIds _
instance showKeyPairIds :: Show KeyPairIds where
  show = genericShow
instance decodeKeyPairIds :: Decode KeyPairIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyPairIds :: Encode KeyPairIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains a Lambda function association.</p>
newtype LambdaFunctionAssociation = LambdaFunctionAssociation 
  { "LambdaFunctionARN" :: NullOrUndefined.NullOrUndefined (String)
  , "EventType" :: NullOrUndefined.NullOrUndefined (EventType)
  }
derive instance newtypeLambdaFunctionAssociation :: Newtype LambdaFunctionAssociation _
derive instance repGenericLambdaFunctionAssociation :: Generic LambdaFunctionAssociation _
instance showLambdaFunctionAssociation :: Show LambdaFunctionAssociation where
  show = genericShow
instance decodeLambdaFunctionAssociation :: Decode LambdaFunctionAssociation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionAssociation :: Encode LambdaFunctionAssociation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LambdaFunctionAssociationList = LambdaFunctionAssociationList (Array LambdaFunctionAssociation)
derive instance newtypeLambdaFunctionAssociationList :: Newtype LambdaFunctionAssociationList _
derive instance repGenericLambdaFunctionAssociationList :: Generic LambdaFunctionAssociationList _
instance showLambdaFunctionAssociationList :: Show LambdaFunctionAssociationList where
  show = genericShow
instance decodeLambdaFunctionAssociationList :: Decode LambdaFunctionAssociationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionAssociationList :: Encode LambdaFunctionAssociationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that specifies a list of Lambda functions associations for a cache behavior.</p> <p>If you want to invoke one or more Lambda functions triggered by requests that match the <code>PathPattern</code> of the cache behavior, specify the applicable values for <code>Quantity</code> and <code>Items</code>. Note that there can be up to 4 <code>LambdaFunctionAssociation</code> items in this list (one for each possible value of <code>EventType</code>) and each <code>EventType</code> can be associated with the Lambda function only once.</p> <p>If you don't want to invoke any Lambda functions for the requests that match <code>PathPattern</code>, specify <code>0</code> for <code>Quantity</code> and omit <code>Items</code>. </p>
newtype LambdaFunctionAssociations = LambdaFunctionAssociations 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (LambdaFunctionAssociationList)
  }
derive instance newtypeLambdaFunctionAssociations :: Newtype LambdaFunctionAssociations _
derive instance repGenericLambdaFunctionAssociations :: Generic LambdaFunctionAssociations _
instance showLambdaFunctionAssociations :: Show LambdaFunctionAssociations where
  show = genericShow
instance decodeLambdaFunctionAssociations :: Decode LambdaFunctionAssociations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionAssociations :: Encode LambdaFunctionAssociations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to list origin access identities. </p>
newtype ListCloudFrontOriginAccessIdentitiesRequest = ListCloudFrontOriginAccessIdentitiesRequest 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListCloudFrontOriginAccessIdentitiesRequest :: Newtype ListCloudFrontOriginAccessIdentitiesRequest _
derive instance repGenericListCloudFrontOriginAccessIdentitiesRequest :: Generic ListCloudFrontOriginAccessIdentitiesRequest _
instance showListCloudFrontOriginAccessIdentitiesRequest :: Show ListCloudFrontOriginAccessIdentitiesRequest where
  show = genericShow
instance decodeListCloudFrontOriginAccessIdentitiesRequest :: Decode ListCloudFrontOriginAccessIdentitiesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCloudFrontOriginAccessIdentitiesRequest :: Encode ListCloudFrontOriginAccessIdentitiesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request. </p>
newtype ListCloudFrontOriginAccessIdentitiesResult = ListCloudFrontOriginAccessIdentitiesResult 
  { "CloudFrontOriginAccessIdentityList" :: NullOrUndefined.NullOrUndefined (CloudFrontOriginAccessIdentityList)
  }
derive instance newtypeListCloudFrontOriginAccessIdentitiesResult :: Newtype ListCloudFrontOriginAccessIdentitiesResult _
derive instance repGenericListCloudFrontOriginAccessIdentitiesResult :: Generic ListCloudFrontOriginAccessIdentitiesResult _
instance showListCloudFrontOriginAccessIdentitiesResult :: Show ListCloudFrontOriginAccessIdentitiesResult where
  show = genericShow
instance decodeListCloudFrontOriginAccessIdentitiesResult :: Decode ListCloudFrontOriginAccessIdentitiesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCloudFrontOriginAccessIdentitiesResult :: Encode ListCloudFrontOriginAccessIdentitiesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to list distributions that are associated with a specified AWS WAF web ACL. </p>
newtype ListDistributionsByWebACLIdRequest = ListDistributionsByWebACLIdRequest 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (String)
  , "WebACLId" :: (String)
  }
derive instance newtypeListDistributionsByWebACLIdRequest :: Newtype ListDistributionsByWebACLIdRequest _
derive instance repGenericListDistributionsByWebACLIdRequest :: Generic ListDistributionsByWebACLIdRequest _
instance showListDistributionsByWebACLIdRequest :: Show ListDistributionsByWebACLIdRequest where
  show = genericShow
instance decodeListDistributionsByWebACLIdRequest :: Decode ListDistributionsByWebACLIdRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDistributionsByWebACLIdRequest :: Encode ListDistributionsByWebACLIdRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to a request to list the distributions that are associated with a specified AWS WAF web ACL. </p>
newtype ListDistributionsByWebACLIdResult = ListDistributionsByWebACLIdResult 
  { "DistributionList" :: NullOrUndefined.NullOrUndefined (DistributionList)
  }
derive instance newtypeListDistributionsByWebACLIdResult :: Newtype ListDistributionsByWebACLIdResult _
derive instance repGenericListDistributionsByWebACLIdResult :: Generic ListDistributionsByWebACLIdResult _
instance showListDistributionsByWebACLIdResult :: Show ListDistributionsByWebACLIdResult where
  show = genericShow
instance decodeListDistributionsByWebACLIdResult :: Decode ListDistributionsByWebACLIdResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDistributionsByWebACLIdResult :: Encode ListDistributionsByWebACLIdResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to list your distributions. </p>
newtype ListDistributionsRequest = ListDistributionsRequest 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListDistributionsRequest :: Newtype ListDistributionsRequest _
derive instance repGenericListDistributionsRequest :: Generic ListDistributionsRequest _
instance showListDistributionsRequest :: Show ListDistributionsRequest where
  show = genericShow
instance decodeListDistributionsRequest :: Decode ListDistributionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDistributionsRequest :: Encode ListDistributionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request. </p>
newtype ListDistributionsResult = ListDistributionsResult 
  { "DistributionList" :: NullOrUndefined.NullOrUndefined (DistributionList)
  }
derive instance newtypeListDistributionsResult :: Newtype ListDistributionsResult _
derive instance repGenericListDistributionsResult :: Generic ListDistributionsResult _
instance showListDistributionsResult :: Show ListDistributionsResult where
  show = genericShow
instance decodeListDistributionsResult :: Decode ListDistributionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDistributionsResult :: Encode ListDistributionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to list invalidations. </p>
newtype ListInvalidationsRequest = ListInvalidationsRequest 
  { "DistributionId" :: (String)
  , "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListInvalidationsRequest :: Newtype ListInvalidationsRequest _
derive instance repGenericListInvalidationsRequest :: Generic ListInvalidationsRequest _
instance showListInvalidationsRequest :: Show ListInvalidationsRequest where
  show = genericShow
instance decodeListInvalidationsRequest :: Decode ListInvalidationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInvalidationsRequest :: Encode ListInvalidationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request. </p>
newtype ListInvalidationsResult = ListInvalidationsResult 
  { "InvalidationList" :: NullOrUndefined.NullOrUndefined (InvalidationList)
  }
derive instance newtypeListInvalidationsResult :: Newtype ListInvalidationsResult _
derive instance repGenericListInvalidationsResult :: Generic ListInvalidationsResult _
instance showListInvalidationsResult :: Show ListInvalidationsResult where
  show = genericShow
instance decodeListInvalidationsResult :: Decode ListInvalidationsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInvalidationsResult :: Encode ListInvalidationsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to list your streaming distributions. </p>
newtype ListStreamingDistributionsRequest = ListStreamingDistributionsRequest 
  { "Marker" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListStreamingDistributionsRequest :: Newtype ListStreamingDistributionsRequest _
derive instance repGenericListStreamingDistributionsRequest :: Generic ListStreamingDistributionsRequest _
instance showListStreamingDistributionsRequest :: Show ListStreamingDistributionsRequest where
  show = genericShow
instance decodeListStreamingDistributionsRequest :: Decode ListStreamingDistributionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStreamingDistributionsRequest :: Encode ListStreamingDistributionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request. </p>
newtype ListStreamingDistributionsResult = ListStreamingDistributionsResult 
  { "StreamingDistributionList" :: NullOrUndefined.NullOrUndefined (StreamingDistributionList)
  }
derive instance newtypeListStreamingDistributionsResult :: Newtype ListStreamingDistributionsResult _
derive instance repGenericListStreamingDistributionsResult :: Generic ListStreamingDistributionsResult _
instance showListStreamingDistributionsResult :: Show ListStreamingDistributionsResult where
  show = genericShow
instance decodeListStreamingDistributionsResult :: Decode ListStreamingDistributionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStreamingDistributionsResult :: Encode ListStreamingDistributionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The request to list tags for a CloudFront resource.</p>
newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "Resource" :: (ResourceARN)
  }
derive instance newtypeListTagsForResourceRequest :: Newtype ListTagsForResourceRequest _
derive instance repGenericListTagsForResourceRequest :: Generic ListTagsForResourceRequest _
instance showListTagsForResourceRequest :: Show ListTagsForResourceRequest where
  show = genericShow
instance decodeListTagsForResourceRequest :: Decode ListTagsForResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceRequest :: Encode ListTagsForResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The returned result of the corresponding request.</p>
newtype ListTagsForResourceResult = ListTagsForResourceResult 
  { "Tags" :: (Tags)
  }
derive instance newtypeListTagsForResourceResult :: Newtype ListTagsForResourceResult _
derive instance repGenericListTagsForResourceResult :: Generic ListTagsForResourceResult _
instance showListTagsForResourceResult :: Show ListTagsForResourceResult where
  show = genericShow
instance decodeListTagsForResourceResult :: Decode ListTagsForResourceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceResult :: Encode ListTagsForResourceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LocationList = LocationList (Array String)
derive instance newtypeLocationList :: Newtype LocationList _
derive instance repGenericLocationList :: Generic LocationList _
instance showLocationList :: Show LocationList where
  show = genericShow
instance decodeLocationList :: Decode LocationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocationList :: Encode LocationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that controls whether access logs are written for the distribution.</p>
newtype LoggingConfig = LoggingConfig 
  { "Enabled" :: (Boolean)
  , "IncludeCookies" :: (Boolean)
  , "Bucket" :: (String)
  , "Prefix" :: (String)
  }
derive instance newtypeLoggingConfig :: Newtype LoggingConfig _
derive instance repGenericLoggingConfig :: Generic LoggingConfig _
instance showLoggingConfig :: Show LoggingConfig where
  show = genericShow
instance decodeLoggingConfig :: Decode LoggingConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoggingConfig :: Encode LoggingConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Method = Method String
derive instance newtypeMethod :: Newtype Method _
derive instance repGenericMethod :: Generic Method _
instance showMethod :: Show Method where
  show = genericShow
instance decodeMethod :: Decode Method where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMethod :: Encode Method where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MethodsList = MethodsList (Array Method)
derive instance newtypeMethodsList :: Newtype MethodsList _
derive instance repGenericMethodsList :: Generic MethodsList _
instance showMethodsList :: Show MethodsList where
  show = genericShow
instance decodeMethodsList :: Decode MethodsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMethodsList :: Encode MethodsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MinimumProtocolVersion = MinimumProtocolVersion String
derive instance newtypeMinimumProtocolVersion :: Newtype MinimumProtocolVersion _
derive instance repGenericMinimumProtocolVersion :: Generic MinimumProtocolVersion _
instance showMinimumProtocolVersion :: Show MinimumProtocolVersion where
  show = genericShow
instance decodeMinimumProtocolVersion :: Decode MinimumProtocolVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMinimumProtocolVersion :: Encode MinimumProtocolVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This operation requires a body. Ensure that the body is present and the Content-Type header is set.</p>
newtype MissingBody = MissingBody 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMissingBody :: Newtype MissingBody _
derive instance repGenericMissingBody :: Generic MissingBody _
instance showMissingBody :: Show MissingBody where
  show = genericShow
instance decodeMissingBody :: Decode MissingBody where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingBody :: Encode MissingBody where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified origin access identity does not exist.</p>
newtype NoSuchCloudFrontOriginAccessIdentity = NoSuchCloudFrontOriginAccessIdentity 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNoSuchCloudFrontOriginAccessIdentity :: Newtype NoSuchCloudFrontOriginAccessIdentity _
derive instance repGenericNoSuchCloudFrontOriginAccessIdentity :: Generic NoSuchCloudFrontOriginAccessIdentity _
instance showNoSuchCloudFrontOriginAccessIdentity :: Show NoSuchCloudFrontOriginAccessIdentity where
  show = genericShow
instance decodeNoSuchCloudFrontOriginAccessIdentity :: Decode NoSuchCloudFrontOriginAccessIdentity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchCloudFrontOriginAccessIdentity :: Encode NoSuchCloudFrontOriginAccessIdentity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified distribution does not exist.</p>
newtype NoSuchDistribution = NoSuchDistribution 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNoSuchDistribution :: Newtype NoSuchDistribution _
derive instance repGenericNoSuchDistribution :: Generic NoSuchDistribution _
instance showNoSuchDistribution :: Show NoSuchDistribution where
  show = genericShow
instance decodeNoSuchDistribution :: Decode NoSuchDistribution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchDistribution :: Encode NoSuchDistribution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified invalidation does not exist.</p>
newtype NoSuchInvalidation = NoSuchInvalidation 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNoSuchInvalidation :: Newtype NoSuchInvalidation _
derive instance repGenericNoSuchInvalidation :: Generic NoSuchInvalidation _
instance showNoSuchInvalidation :: Show NoSuchInvalidation where
  show = genericShow
instance decodeNoSuchInvalidation :: Decode NoSuchInvalidation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchInvalidation :: Encode NoSuchInvalidation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>No origin exists with the specified <code>Origin Id</code>. </p>
newtype NoSuchOrigin = NoSuchOrigin 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNoSuchOrigin :: Newtype NoSuchOrigin _
derive instance repGenericNoSuchOrigin :: Generic NoSuchOrigin _
instance showNoSuchOrigin :: Show NoSuchOrigin where
  show = genericShow
instance decodeNoSuchOrigin :: Decode NoSuchOrigin where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchOrigin :: Encode NoSuchOrigin where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NoSuchResource = NoSuchResource 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNoSuchResource :: Newtype NoSuchResource _
derive instance repGenericNoSuchResource :: Generic NoSuchResource _
instance showNoSuchResource :: Show NoSuchResource where
  show = genericShow
instance decodeNoSuchResource :: Decode NoSuchResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchResource :: Encode NoSuchResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified streaming distribution does not exist.</p>
newtype NoSuchStreamingDistribution = NoSuchStreamingDistribution 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNoSuchStreamingDistribution :: Newtype NoSuchStreamingDistribution _
derive instance repGenericNoSuchStreamingDistribution :: Generic NoSuchStreamingDistribution _
instance showNoSuchStreamingDistribution :: Show NoSuchStreamingDistribution where
  show = genericShow
instance decodeNoSuchStreamingDistribution :: Decode NoSuchStreamingDistribution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchStreamingDistribution :: Encode NoSuchStreamingDistribution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that describes the Amazon S3 bucket or the HTTP server (for example, a web server) from which CloudFront gets your files. You must create at least one origin.</p> <p>For the current limit on the number of origins that you can create for a distribution, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_cloudfront">Amazon CloudFront Limits</a> in the <i>AWS General Reference</i>.</p>
newtype Origin = Origin 
  { "Id" :: (String)
  , "DomainName" :: (String)
  , "OriginPath" :: NullOrUndefined.NullOrUndefined (String)
  , "CustomHeaders" :: NullOrUndefined.NullOrUndefined (CustomHeaders)
  , "S3OriginConfig" :: NullOrUndefined.NullOrUndefined (S3OriginConfig)
  , "CustomOriginConfig" :: NullOrUndefined.NullOrUndefined (CustomOriginConfig)
  }
derive instance newtypeOrigin :: Newtype Origin _
derive instance repGenericOrigin :: Generic Origin _
instance showOrigin :: Show Origin where
  show = genericShow
instance decodeOrigin :: Decode Origin where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrigin :: Encode Origin where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains <code>HeaderName</code> and <code>HeaderValue</code> elements, if any, for this distribution. </p>
newtype OriginCustomHeader = OriginCustomHeader 
  { "HeaderName" :: (String)
  , "HeaderValue" :: (String)
  }
derive instance newtypeOriginCustomHeader :: Newtype OriginCustomHeader _
derive instance repGenericOriginCustomHeader :: Generic OriginCustomHeader _
instance showOriginCustomHeader :: Show OriginCustomHeader where
  show = genericShow
instance decodeOriginCustomHeader :: Decode OriginCustomHeader where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOriginCustomHeader :: Encode OriginCustomHeader where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OriginCustomHeadersList = OriginCustomHeadersList (Array OriginCustomHeader)
derive instance newtypeOriginCustomHeadersList :: Newtype OriginCustomHeadersList _
derive instance repGenericOriginCustomHeadersList :: Generic OriginCustomHeadersList _
instance showOriginCustomHeadersList :: Show OriginCustomHeadersList where
  show = genericShow
instance decodeOriginCustomHeadersList :: Decode OriginCustomHeadersList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOriginCustomHeadersList :: Encode OriginCustomHeadersList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OriginList = OriginList (Array Origin)
derive instance newtypeOriginList :: Newtype OriginList _
derive instance repGenericOriginList :: Generic OriginList _
instance showOriginList :: Show OriginList where
  show = genericShow
instance decodeOriginList :: Decode OriginList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOriginList :: Encode OriginList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OriginProtocolPolicy = OriginProtocolPolicy String
derive instance newtypeOriginProtocolPolicy :: Newtype OriginProtocolPolicy _
derive instance repGenericOriginProtocolPolicy :: Generic OriginProtocolPolicy _
instance showOriginProtocolPolicy :: Show OriginProtocolPolicy where
  show = genericShow
instance decodeOriginProtocolPolicy :: Decode OriginProtocolPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOriginProtocolPolicy :: Encode OriginProtocolPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the SSL/TLS protocols that CloudFront can use when establishing an HTTPS connection with your origin. </p>
newtype OriginSslProtocols = OriginSslProtocols 
  { "Quantity" :: (Int)
  , "Items" :: (SslProtocolsList)
  }
derive instance newtypeOriginSslProtocols :: Newtype OriginSslProtocols _
derive instance repGenericOriginSslProtocols :: Generic OriginSslProtocols _
instance showOriginSslProtocols :: Show OriginSslProtocols where
  show = genericShow
instance decodeOriginSslProtocols :: Decode OriginSslProtocols where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOriginSslProtocols :: Encode OriginSslProtocols where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about origins for this distribution. </p>
newtype Origins = Origins 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (OriginList)
  }
derive instance newtypeOrigins :: Newtype Origins _
derive instance repGenericOrigins :: Generic Origins _
instance showOrigins :: Show Origins where
  show = genericShow
instance decodeOrigins :: Decode Origins where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrigins :: Encode Origins where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PathList = PathList (Array String)
derive instance newtypePathList :: Newtype PathList _
derive instance repGenericPathList :: Generic PathList _
instance showPathList :: Show PathList where
  show = genericShow
instance decodePathList :: Decode PathList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePathList :: Encode PathList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the objects that you want to invalidate. For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects">Specifying the Objects to Invalidate</a> in the <i>Amazon CloudFront Developer Guide</i>. </p>
newtype Paths = Paths 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (PathList)
  }
derive instance newtypePaths :: Newtype Paths _
derive instance repGenericPaths :: Generic Paths _
instance showPaths :: Show Paths where
  show = genericShow
instance decodePaths :: Decode Paths where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaths :: Encode Paths where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The precondition given in one or more of the request-header fields evaluated to <code>false</code>. </p>
newtype PreconditionFailed = PreconditionFailed 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePreconditionFailed :: Newtype PreconditionFailed _
derive instance repGenericPreconditionFailed :: Generic PreconditionFailed _
instance showPreconditionFailed :: Show PreconditionFailed where
  show = genericShow
instance decodePreconditionFailed :: Decode PreconditionFailed where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePreconditionFailed :: Encode PreconditionFailed where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PriceClass = PriceClass String
derive instance newtypePriceClass :: Newtype PriceClass _
derive instance repGenericPriceClass :: Generic PriceClass _
instance showPriceClass :: Show PriceClass where
  show = genericShow
instance decodePriceClass :: Decode PriceClass where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePriceClass :: Encode PriceClass where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueryStringCacheKeys = QueryStringCacheKeys 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (QueryStringCacheKeysList)
  }
derive instance newtypeQueryStringCacheKeys :: Newtype QueryStringCacheKeys _
derive instance repGenericQueryStringCacheKeys :: Generic QueryStringCacheKeys _
instance showQueryStringCacheKeys :: Show QueryStringCacheKeys where
  show = genericShow
instance decodeQueryStringCacheKeys :: Decode QueryStringCacheKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryStringCacheKeys :: Encode QueryStringCacheKeys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueryStringCacheKeysList = QueryStringCacheKeysList (Array String)
derive instance newtypeQueryStringCacheKeysList :: Newtype QueryStringCacheKeysList _
derive instance repGenericQueryStringCacheKeysList :: Generic QueryStringCacheKeysList _
instance showQueryStringCacheKeysList :: Show QueryStringCacheKeysList where
  show = genericShow
instance decodeQueryStringCacheKeysList :: Decode QueryStringCacheKeysList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryStringCacheKeysList :: Encode QueryStringCacheKeysList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceARN = ResourceARN String
derive instance newtypeResourceARN :: Newtype ResourceARN _
derive instance repGenericResourceARN :: Generic ResourceARN _
instance showResourceARN :: Show ResourceARN where
  show = genericShow
instance decodeResourceARN :: Decode ResourceARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceARN :: Encode ResourceARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that identifies ways in which you want to restrict distribution of your content.</p>
newtype Restrictions = Restrictions 
  { "GeoRestriction" :: (GeoRestriction)
  }
derive instance newtypeRestrictions :: Newtype Restrictions _
derive instance repGenericRestrictions :: Generic Restrictions _
instance showRestrictions :: Show Restrictions where
  show = genericShow
instance decodeRestrictions :: Decode Restrictions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestrictions :: Encode Restrictions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.</p>
newtype S3Origin = S3Origin 
  { "DomainName" :: (String)
  , "OriginAccessIdentity" :: (String)
  }
derive instance newtypeS3Origin :: Newtype S3Origin _
derive instance repGenericS3Origin :: Generic S3Origin _
instance showS3Origin :: Show S3Origin where
  show = genericShow
instance decodeS3Origin :: Decode S3Origin where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Origin :: Encode S3Origin where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the Amazon S3 origin. If the origin is a custom origin, use the <code>CustomOriginConfig</code> element instead.</p>
newtype S3OriginConfig = S3OriginConfig 
  { "OriginAccessIdentity" :: (String)
  }
derive instance newtypeS3OriginConfig :: Newtype S3OriginConfig _
derive instance repGenericS3OriginConfig :: Generic S3OriginConfig _
instance showS3OriginConfig :: Show S3OriginConfig where
  show = genericShow
instance decodeS3OriginConfig :: Decode S3OriginConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3OriginConfig :: Encode S3OriginConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SSLSupportMethod = SSLSupportMethod String
derive instance newtypeSSLSupportMethod :: Newtype SSLSupportMethod _
derive instance repGenericSSLSupportMethod :: Generic SSLSupportMethod _
instance showSSLSupportMethod :: Show SSLSupportMethod where
  show = genericShow
instance decodeSSLSupportMethod :: Decode SSLSupportMethod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSSLSupportMethod :: Encode SSLSupportMethod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that lists the AWS accounts that were included in the <code>TrustedSigners</code> complex type, as well as their active CloudFront key pair IDs, if any. </p>
newtype Signer = Signer 
  { "AwsAccountNumber" :: NullOrUndefined.NullOrUndefined (String)
  , "KeyPairIds" :: NullOrUndefined.NullOrUndefined (KeyPairIds)
  }
derive instance newtypeSigner :: Newtype Signer _
derive instance repGenericSigner :: Generic Signer _
instance showSigner :: Show Signer where
  show = genericShow
instance decodeSigner :: Decode Signer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSigner :: Encode Signer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SignerList = SignerList (Array Signer)
derive instance newtypeSignerList :: Newtype SignerList _
derive instance repGenericSignerList :: Generic SignerList _
instance showSignerList :: Show SignerList where
  show = genericShow
instance decodeSignerList :: Decode SignerList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignerList :: Encode SignerList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SslProtocol = SslProtocol String
derive instance newtypeSslProtocol :: Newtype SslProtocol _
derive instance repGenericSslProtocol :: Generic SslProtocol _
instance showSslProtocol :: Show SslProtocol where
  show = genericShow
instance decodeSslProtocol :: Decode SslProtocol where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSslProtocol :: Encode SslProtocol where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SslProtocolsList = SslProtocolsList (Array SslProtocol)
derive instance newtypeSslProtocolsList :: Newtype SslProtocolsList _
derive instance repGenericSslProtocolsList :: Generic SslProtocolsList _
instance showSslProtocolsList :: Show SslProtocolsList where
  show = genericShow
instance decodeSslProtocolsList :: Decode SslProtocolsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSslProtocolsList :: Encode SslProtocolsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A streaming distribution. </p>
newtype StreamingDistribution = StreamingDistribution 
  { "Id" :: (String)
  , "ARN" :: (String)
  , "Status" :: (String)
  , "LastModifiedTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "DomainName" :: (String)
  , "ActiveTrustedSigners" :: (ActiveTrustedSigners)
  , "StreamingDistributionConfig" :: (StreamingDistributionConfig)
  }
derive instance newtypeStreamingDistribution :: Newtype StreamingDistribution _
derive instance repGenericStreamingDistribution :: Generic StreamingDistribution _
instance showStreamingDistribution :: Show StreamingDistribution where
  show = genericShow
instance decodeStreamingDistribution :: Decode StreamingDistribution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamingDistribution :: Encode StreamingDistribution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamingDistributionAlreadyExists = StreamingDistributionAlreadyExists 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeStreamingDistributionAlreadyExists :: Newtype StreamingDistributionAlreadyExists _
derive instance repGenericStreamingDistributionAlreadyExists :: Generic StreamingDistributionAlreadyExists _
instance showStreamingDistributionAlreadyExists :: Show StreamingDistributionAlreadyExists where
  show = genericShow
instance decodeStreamingDistributionAlreadyExists :: Decode StreamingDistributionAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamingDistributionAlreadyExists :: Encode StreamingDistributionAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The RTMP distribution's configuration information.</p>
newtype StreamingDistributionConfig = StreamingDistributionConfig 
  { "CallerReference" :: (String)
  , "S3Origin" :: (S3Origin)
  , "Aliases" :: NullOrUndefined.NullOrUndefined (Aliases)
  , "Comment" :: (String)
  , "Logging" :: NullOrUndefined.NullOrUndefined (StreamingLoggingConfig)
  , "TrustedSigners" :: (TrustedSigners)
  , "PriceClass" :: NullOrUndefined.NullOrUndefined (PriceClass)
  , "Enabled" :: (Boolean)
  }
derive instance newtypeStreamingDistributionConfig :: Newtype StreamingDistributionConfig _
derive instance repGenericStreamingDistributionConfig :: Generic StreamingDistributionConfig _
instance showStreamingDistributionConfig :: Show StreamingDistributionConfig where
  show = genericShow
instance decodeStreamingDistributionConfig :: Decode StreamingDistributionConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamingDistributionConfig :: Encode StreamingDistributionConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A streaming distribution Configuration and a list of tags to be associated with the streaming distribution.</p>
newtype StreamingDistributionConfigWithTags = StreamingDistributionConfigWithTags 
  { "StreamingDistributionConfig" :: (StreamingDistributionConfig)
  , "Tags" :: (Tags)
  }
derive instance newtypeStreamingDistributionConfigWithTags :: Newtype StreamingDistributionConfigWithTags _
derive instance repGenericStreamingDistributionConfigWithTags :: Generic StreamingDistributionConfigWithTags _
instance showStreamingDistributionConfigWithTags :: Show StreamingDistributionConfigWithTags where
  show = genericShow
instance decodeStreamingDistributionConfigWithTags :: Decode StreamingDistributionConfigWithTags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamingDistributionConfigWithTags :: Encode StreamingDistributionConfigWithTags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A streaming distribution list. </p>
newtype StreamingDistributionList = StreamingDistributionList 
  { "Marker" :: (String)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxItems" :: (Int)
  , "IsTruncated" :: (Boolean)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (StreamingDistributionSummaryList)
  }
derive instance newtypeStreamingDistributionList :: Newtype StreamingDistributionList _
derive instance repGenericStreamingDistributionList :: Generic StreamingDistributionList _
instance showStreamingDistributionList :: Show StreamingDistributionList where
  show = genericShow
instance decodeStreamingDistributionList :: Decode StreamingDistributionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamingDistributionList :: Encode StreamingDistributionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamingDistributionNotDisabled = StreamingDistributionNotDisabled 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeStreamingDistributionNotDisabled :: Newtype StreamingDistributionNotDisabled _
derive instance repGenericStreamingDistributionNotDisabled :: Generic StreamingDistributionNotDisabled _
instance showStreamingDistributionNotDisabled :: Show StreamingDistributionNotDisabled where
  show = genericShow
instance decodeStreamingDistributionNotDisabled :: Decode StreamingDistributionNotDisabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamingDistributionNotDisabled :: Encode StreamingDistributionNotDisabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A summary of the information for an Amazon CloudFront streaming distribution.</p>
newtype StreamingDistributionSummary = StreamingDistributionSummary 
  { "Id" :: (String)
  , "ARN" :: (String)
  , "Status" :: (String)
  , "LastModifiedTime" :: (Number)
  , "DomainName" :: (String)
  , "S3Origin" :: (S3Origin)
  , "Aliases" :: (Aliases)
  , "TrustedSigners" :: (TrustedSigners)
  , "Comment" :: (String)
  , "PriceClass" :: (PriceClass)
  , "Enabled" :: (Boolean)
  }
derive instance newtypeStreamingDistributionSummary :: Newtype StreamingDistributionSummary _
derive instance repGenericStreamingDistributionSummary :: Generic StreamingDistributionSummary _
instance showStreamingDistributionSummary :: Show StreamingDistributionSummary where
  show = genericShow
instance decodeStreamingDistributionSummary :: Decode StreamingDistributionSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamingDistributionSummary :: Encode StreamingDistributionSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamingDistributionSummaryList = StreamingDistributionSummaryList (Array StreamingDistributionSummary)
derive instance newtypeStreamingDistributionSummaryList :: Newtype StreamingDistributionSummaryList _
derive instance repGenericStreamingDistributionSummaryList :: Generic StreamingDistributionSummaryList _
instance showStreamingDistributionSummaryList :: Show StreamingDistributionSummaryList where
  show = genericShow
instance decodeStreamingDistributionSummaryList :: Decode StreamingDistributionSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamingDistributionSummaryList :: Encode StreamingDistributionSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that controls whether access logs are written for this streaming distribution.</p>
newtype StreamingLoggingConfig = StreamingLoggingConfig 
  { "Enabled" :: (Boolean)
  , "Bucket" :: (String)
  , "Prefix" :: (String)
  }
derive instance newtypeStreamingLoggingConfig :: Newtype StreamingLoggingConfig _
derive instance repGenericStreamingLoggingConfig :: Generic StreamingLoggingConfig _
instance showStreamingLoggingConfig :: Show StreamingLoggingConfig where
  show = genericShow
instance decodeStreamingLoggingConfig :: Decode StreamingLoggingConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamingLoggingConfig :: Encode StreamingLoggingConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A complex type that contains <code>Tag</code> key and <code>Tag</code> value.</p>
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


-- | <p> A string that contains <code>Tag</code> key.</p> <p>The string length should be between 1 and 128 characters. Valid characters include <code>a-z</code>, <code>A-Z</code>, <code>0-9</code>, space, and the special characters <code>_ - . : / = + @</code>.</p>
newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A complex type that contains zero or more <code>Tag</code> elements.</p>
newtype TagKeys = TagKeys 
  { "Items" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  }
derive instance newtypeTagKeys :: Newtype TagKeys _
derive instance repGenericTagKeys :: Generic TagKeys _
instance showTagKeys :: Show TagKeys where
  show = genericShow
instance decodeTagKeys :: Decode TagKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeys :: Encode TagKeys where
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


-- | <p> The request to add tags to a CloudFront resource.</p>
newtype TagResourceRequest = TagResourceRequest 
  { "Resource" :: (ResourceARN)
  , "Tags" :: (Tags)
  }
derive instance newtypeTagResourceRequest :: Newtype TagResourceRequest _
derive instance repGenericTagResourceRequest :: Generic TagResourceRequest _
instance showTagResourceRequest :: Show TagResourceRequest where
  show = genericShow
instance decodeTagResourceRequest :: Decode TagResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourceRequest :: Encode TagResourceRequest where
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


-- | <p> A complex type that contains zero or more <code>Tag</code> elements.</p>
newtype Tags = Tags 
  { "Items" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeTags :: Newtype Tags _
derive instance repGenericTags :: Generic Tags _
instance showTags :: Show Tags where
  show = genericShow
instance decodeTags :: Decode Tags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTags :: Encode Tags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot create more cache behaviors for the distribution.</p>
newtype TooManyCacheBehaviors = TooManyCacheBehaviors 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyCacheBehaviors :: Newtype TooManyCacheBehaviors _
derive instance repGenericTooManyCacheBehaviors :: Generic TooManyCacheBehaviors _
instance showTooManyCacheBehaviors :: Show TooManyCacheBehaviors where
  show = genericShow
instance decodeTooManyCacheBehaviors :: Decode TooManyCacheBehaviors where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyCacheBehaviors :: Encode TooManyCacheBehaviors where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot create anymore custom SSL/TLS certificates.</p>
newtype TooManyCertificates = TooManyCertificates 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyCertificates :: Newtype TooManyCertificates _
derive instance repGenericTooManyCertificates :: Generic TooManyCertificates _
instance showTooManyCertificates :: Show TooManyCertificates where
  show = genericShow
instance decodeTooManyCertificates :: Decode TooManyCertificates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyCertificates :: Encode TooManyCertificates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Processing your request would cause you to exceed the maximum number of origin access identities allowed.</p>
newtype TooManyCloudFrontOriginAccessIdentities = TooManyCloudFrontOriginAccessIdentities 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyCloudFrontOriginAccessIdentities :: Newtype TooManyCloudFrontOriginAccessIdentities _
derive instance repGenericTooManyCloudFrontOriginAccessIdentities :: Generic TooManyCloudFrontOriginAccessIdentities _
instance showTooManyCloudFrontOriginAccessIdentities :: Show TooManyCloudFrontOriginAccessIdentities where
  show = genericShow
instance decodeTooManyCloudFrontOriginAccessIdentities :: Decode TooManyCloudFrontOriginAccessIdentities where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyCloudFrontOriginAccessIdentities :: Encode TooManyCloudFrontOriginAccessIdentities where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Your request contains more cookie names in the whitelist than are allowed per cache behavior.</p>
newtype TooManyCookieNamesInWhiteList = TooManyCookieNamesInWhiteList 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyCookieNamesInWhiteList :: Newtype TooManyCookieNamesInWhiteList _
derive instance repGenericTooManyCookieNamesInWhiteList :: Generic TooManyCookieNamesInWhiteList _
instance showTooManyCookieNamesInWhiteList :: Show TooManyCookieNamesInWhiteList where
  show = genericShow
instance decodeTooManyCookieNamesInWhiteList :: Decode TooManyCookieNamesInWhiteList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyCookieNamesInWhiteList :: Encode TooManyCookieNamesInWhiteList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Your request contains more CNAMEs than are allowed per distribution.</p>
newtype TooManyDistributionCNAMEs = TooManyDistributionCNAMEs 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyDistributionCNAMEs :: Newtype TooManyDistributionCNAMEs _
derive instance repGenericTooManyDistributionCNAMEs :: Generic TooManyDistributionCNAMEs _
instance showTooManyDistributionCNAMEs :: Show TooManyDistributionCNAMEs where
  show = genericShow
instance decodeTooManyDistributionCNAMEs :: Decode TooManyDistributionCNAMEs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyDistributionCNAMEs :: Encode TooManyDistributionCNAMEs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Processing your request would cause you to exceed the maximum number of distributions allowed.</p>
newtype TooManyDistributions = TooManyDistributions 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyDistributions :: Newtype TooManyDistributions _
derive instance repGenericTooManyDistributions :: Generic TooManyDistributions _
instance showTooManyDistributions :: Show TooManyDistributions where
  show = genericShow
instance decodeTooManyDistributions :: Decode TooManyDistributions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyDistributions :: Encode TooManyDistributions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Processing your request would cause the maximum number of distributions with Lambda function associations per owner to be exceeded.</p>
newtype TooManyDistributionsWithLambdaAssociations = TooManyDistributionsWithLambdaAssociations 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyDistributionsWithLambdaAssociations :: Newtype TooManyDistributionsWithLambdaAssociations _
derive instance repGenericTooManyDistributionsWithLambdaAssociations :: Generic TooManyDistributionsWithLambdaAssociations _
instance showTooManyDistributionsWithLambdaAssociations :: Show TooManyDistributionsWithLambdaAssociations where
  show = genericShow
instance decodeTooManyDistributionsWithLambdaAssociations :: Decode TooManyDistributionsWithLambdaAssociations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyDistributionsWithLambdaAssociations :: Encode TooManyDistributionsWithLambdaAssociations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TooManyHeadersInForwardedValues = TooManyHeadersInForwardedValues 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyHeadersInForwardedValues :: Newtype TooManyHeadersInForwardedValues _
derive instance repGenericTooManyHeadersInForwardedValues :: Generic TooManyHeadersInForwardedValues _
instance showTooManyHeadersInForwardedValues :: Show TooManyHeadersInForwardedValues where
  show = genericShow
instance decodeTooManyHeadersInForwardedValues :: Decode TooManyHeadersInForwardedValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyHeadersInForwardedValues :: Encode TooManyHeadersInForwardedValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have exceeded the maximum number of allowable InProgress invalidation batch requests, or invalidation objects.</p>
newtype TooManyInvalidationsInProgress = TooManyInvalidationsInProgress 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyInvalidationsInProgress :: Newtype TooManyInvalidationsInProgress _
derive instance repGenericTooManyInvalidationsInProgress :: Generic TooManyInvalidationsInProgress _
instance showTooManyInvalidationsInProgress :: Show TooManyInvalidationsInProgress where
  show = genericShow
instance decodeTooManyInvalidationsInProgress :: Decode TooManyInvalidationsInProgress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyInvalidationsInProgress :: Encode TooManyInvalidationsInProgress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Your request contains more Lambda function associations than are allowed per distribution.</p>
newtype TooManyLambdaFunctionAssociations = TooManyLambdaFunctionAssociations 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyLambdaFunctionAssociations :: Newtype TooManyLambdaFunctionAssociations _
derive instance repGenericTooManyLambdaFunctionAssociations :: Generic TooManyLambdaFunctionAssociations _
instance showTooManyLambdaFunctionAssociations :: Show TooManyLambdaFunctionAssociations where
  show = genericShow
instance decodeTooManyLambdaFunctionAssociations :: Decode TooManyLambdaFunctionAssociations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyLambdaFunctionAssociations :: Encode TooManyLambdaFunctionAssociations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TooManyOriginCustomHeaders = TooManyOriginCustomHeaders 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyOriginCustomHeaders :: Newtype TooManyOriginCustomHeaders _
derive instance repGenericTooManyOriginCustomHeaders :: Generic TooManyOriginCustomHeaders _
instance showTooManyOriginCustomHeaders :: Show TooManyOriginCustomHeaders where
  show = genericShow
instance decodeTooManyOriginCustomHeaders :: Decode TooManyOriginCustomHeaders where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyOriginCustomHeaders :: Encode TooManyOriginCustomHeaders where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot create more origins for the distribution.</p>
newtype TooManyOrigins = TooManyOrigins 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyOrigins :: Newtype TooManyOrigins _
derive instance repGenericTooManyOrigins :: Generic TooManyOrigins _
instance showTooManyOrigins :: Show TooManyOrigins where
  show = genericShow
instance decodeTooManyOrigins :: Decode TooManyOrigins where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyOrigins :: Encode TooManyOrigins where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TooManyQueryStringParameters = TooManyQueryStringParameters 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyQueryStringParameters :: Newtype TooManyQueryStringParameters _
derive instance repGenericTooManyQueryStringParameters :: Generic TooManyQueryStringParameters _
instance showTooManyQueryStringParameters :: Show TooManyQueryStringParameters where
  show = genericShow
instance decodeTooManyQueryStringParameters :: Decode TooManyQueryStringParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyQueryStringParameters :: Encode TooManyQueryStringParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TooManyStreamingDistributionCNAMEs = TooManyStreamingDistributionCNAMEs 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyStreamingDistributionCNAMEs :: Newtype TooManyStreamingDistributionCNAMEs _
derive instance repGenericTooManyStreamingDistributionCNAMEs :: Generic TooManyStreamingDistributionCNAMEs _
instance showTooManyStreamingDistributionCNAMEs :: Show TooManyStreamingDistributionCNAMEs where
  show = genericShow
instance decodeTooManyStreamingDistributionCNAMEs :: Decode TooManyStreamingDistributionCNAMEs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyStreamingDistributionCNAMEs :: Encode TooManyStreamingDistributionCNAMEs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Processing your request would cause you to exceed the maximum number of streaming distributions allowed.</p>
newtype TooManyStreamingDistributions = TooManyStreamingDistributions 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyStreamingDistributions :: Newtype TooManyStreamingDistributions _
derive instance repGenericTooManyStreamingDistributions :: Generic TooManyStreamingDistributions _
instance showTooManyStreamingDistributions :: Show TooManyStreamingDistributions where
  show = genericShow
instance decodeTooManyStreamingDistributions :: Decode TooManyStreamingDistributions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyStreamingDistributions :: Encode TooManyStreamingDistributions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Your request contains more trusted signers than are allowed per distribution.</p>
newtype TooManyTrustedSigners = TooManyTrustedSigners 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyTrustedSigners :: Newtype TooManyTrustedSigners _
derive instance repGenericTooManyTrustedSigners :: Generic TooManyTrustedSigners _
instance showTooManyTrustedSigners :: Show TooManyTrustedSigners where
  show = genericShow
instance decodeTooManyTrustedSigners :: Decode TooManyTrustedSigners where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyTrustedSigners :: Encode TooManyTrustedSigners where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more of your trusted signers do not exist.</p>
newtype TrustedSignerDoesNotExist = TrustedSignerDoesNotExist 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTrustedSignerDoesNotExist :: Newtype TrustedSignerDoesNotExist _
derive instance repGenericTrustedSignerDoesNotExist :: Generic TrustedSignerDoesNotExist _
instance showTrustedSignerDoesNotExist :: Show TrustedSignerDoesNotExist where
  show = genericShow
instance decodeTrustedSignerDoesNotExist :: Decode TrustedSignerDoesNotExist where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrustedSignerDoesNotExist :: Encode TrustedSignerDoesNotExist where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content.</p> <p>If you want to require signed URLs in requests for objects in the target origin that match the <code>PathPattern</code> for this cache behavior, specify <code>true</code> for <code>Enabled</code>, and specify the applicable values for <code>Quantity</code> and <code>Items</code>. For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html">Serving Private Content through CloudFront</a> in the <i>Amazon Amazon CloudFront Developer Guide</i>.</p> <p>If you don't want to require signed URLs in requests for objects that match <code>PathPattern</code>, specify <code>false</code> for <code>Enabled</code> and <code>0</code> for <code>Quantity</code>. Omit <code>Items</code>.</p> <p>To add, change, or remove one or more trusted signers, change <code>Enabled</code> to <code>true</code> (if it's currently <code>false</code>), change <code>Quantity</code> as applicable, and specify all of the trusted signers that you want to include in the updated distribution.</p> <p>For more information about updating the distribution configuration, see <a>DistributionConfig</a> .</p>
newtype TrustedSigners = TrustedSigners 
  { "Enabled" :: (Boolean)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined.NullOrUndefined (AwsAccountNumberList)
  }
derive instance newtypeTrustedSigners :: Newtype TrustedSigners _
derive instance repGenericTrustedSigners :: Generic TrustedSigners _
instance showTrustedSigners :: Show TrustedSigners where
  show = genericShow
instance decodeTrustedSigners :: Decode TrustedSigners where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrustedSigners :: Encode TrustedSigners where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The request to remove tags from a CloudFront resource.</p>
newtype UntagResourceRequest = UntagResourceRequest 
  { "Resource" :: (ResourceARN)
  , "TagKeys" :: (TagKeys)
  }
derive instance newtypeUntagResourceRequest :: Newtype UntagResourceRequest _
derive instance repGenericUntagResourceRequest :: Generic UntagResourceRequest _
instance showUntagResourceRequest :: Show UntagResourceRequest where
  show = genericShow
instance decodeUntagResourceRequest :: Decode UntagResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagResourceRequest :: Encode UntagResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to update an origin access identity.</p>
newtype UpdateCloudFrontOriginAccessIdentityRequest = UpdateCloudFrontOriginAccessIdentityRequest 
  { "CloudFrontOriginAccessIdentityConfig" :: (CloudFrontOriginAccessIdentityConfig)
  , "Id" :: (String)
  , "IfMatch" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateCloudFrontOriginAccessIdentityRequest :: Newtype UpdateCloudFrontOriginAccessIdentityRequest _
derive instance repGenericUpdateCloudFrontOriginAccessIdentityRequest :: Generic UpdateCloudFrontOriginAccessIdentityRequest _
instance showUpdateCloudFrontOriginAccessIdentityRequest :: Show UpdateCloudFrontOriginAccessIdentityRequest where
  show = genericShow
instance decodeUpdateCloudFrontOriginAccessIdentityRequest :: Decode UpdateCloudFrontOriginAccessIdentityRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCloudFrontOriginAccessIdentityRequest :: Encode UpdateCloudFrontOriginAccessIdentityRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype UpdateCloudFrontOriginAccessIdentityResult = UpdateCloudFrontOriginAccessIdentityResult 
  { "CloudFrontOriginAccessIdentity" :: NullOrUndefined.NullOrUndefined (CloudFrontOriginAccessIdentity)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateCloudFrontOriginAccessIdentityResult :: Newtype UpdateCloudFrontOriginAccessIdentityResult _
derive instance repGenericUpdateCloudFrontOriginAccessIdentityResult :: Generic UpdateCloudFrontOriginAccessIdentityResult _
instance showUpdateCloudFrontOriginAccessIdentityResult :: Show UpdateCloudFrontOriginAccessIdentityResult where
  show = genericShow
instance decodeUpdateCloudFrontOriginAccessIdentityResult :: Decode UpdateCloudFrontOriginAccessIdentityResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCloudFrontOriginAccessIdentityResult :: Encode UpdateCloudFrontOriginAccessIdentityResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to update a distribution.</p>
newtype UpdateDistributionRequest = UpdateDistributionRequest 
  { "DistributionConfig" :: (DistributionConfig)
  , "Id" :: (String)
  , "IfMatch" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateDistributionRequest :: Newtype UpdateDistributionRequest _
derive instance repGenericUpdateDistributionRequest :: Generic UpdateDistributionRequest _
instance showUpdateDistributionRequest :: Show UpdateDistributionRequest where
  show = genericShow
instance decodeUpdateDistributionRequest :: Decode UpdateDistributionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDistributionRequest :: Encode UpdateDistributionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype UpdateDistributionResult = UpdateDistributionResult 
  { "Distribution" :: NullOrUndefined.NullOrUndefined (Distribution)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateDistributionResult :: Newtype UpdateDistributionResult _
derive instance repGenericUpdateDistributionResult :: Generic UpdateDistributionResult _
instance showUpdateDistributionResult :: Show UpdateDistributionResult where
  show = genericShow
instance decodeUpdateDistributionResult :: Decode UpdateDistributionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDistributionResult :: Encode UpdateDistributionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to update a streaming distribution.</p>
newtype UpdateStreamingDistributionRequest = UpdateStreamingDistributionRequest 
  { "StreamingDistributionConfig" :: (StreamingDistributionConfig)
  , "Id" :: (String)
  , "IfMatch" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateStreamingDistributionRequest :: Newtype UpdateStreamingDistributionRequest _
derive instance repGenericUpdateStreamingDistributionRequest :: Generic UpdateStreamingDistributionRequest _
instance showUpdateStreamingDistributionRequest :: Show UpdateStreamingDistributionRequest where
  show = genericShow
instance decodeUpdateStreamingDistributionRequest :: Decode UpdateStreamingDistributionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStreamingDistributionRequest :: Encode UpdateStreamingDistributionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The returned result of the corresponding request.</p>
newtype UpdateStreamingDistributionResult = UpdateStreamingDistributionResult 
  { "StreamingDistribution" :: NullOrUndefined.NullOrUndefined (StreamingDistribution)
  , "ETag" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateStreamingDistributionResult :: Newtype UpdateStreamingDistributionResult _
derive instance repGenericUpdateStreamingDistributionResult :: Generic UpdateStreamingDistributionResult _
instance showUpdateStreamingDistributionResult :: Show UpdateStreamingDistributionResult where
  show = genericShow
instance decodeUpdateStreamingDistributionResult :: Decode UpdateStreamingDistributionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStreamingDistributionResult :: Encode UpdateStreamingDistributionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that specifies the following:</p> <ul> <li> <p>Which SSL/TLS certificate to use when viewers request objects using HTTPS</p> </li> <li> <p>Whether you want CloudFront to use dedicated IP addresses or SNI when you're using alternate domain names in your object names</p> </li> <li> <p>The minimum protocol version that you want CloudFront to use when communicating with viewers</p> </li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html">Using an HTTPS Connection to Access Your Objects</a> in the <i>Amazon Amazon CloudFront Developer Guide</i>.</p>
newtype ViewerCertificate = ViewerCertificate 
  { "CloudFrontDefaultCertificate" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "IAMCertificateId" :: NullOrUndefined.NullOrUndefined (String)
  , "ACMCertificateArn" :: NullOrUndefined.NullOrUndefined (String)
  , "SSLSupportMethod" :: NullOrUndefined.NullOrUndefined (SSLSupportMethod)
  , "MinimumProtocolVersion" :: NullOrUndefined.NullOrUndefined (MinimumProtocolVersion)
  , "Certificate" :: NullOrUndefined.NullOrUndefined (String)
  , "CertificateSource" :: NullOrUndefined.NullOrUndefined (CertificateSource)
  }
derive instance newtypeViewerCertificate :: Newtype ViewerCertificate _
derive instance repGenericViewerCertificate :: Generic ViewerCertificate _
instance showViewerCertificate :: Show ViewerCertificate where
  show = genericShow
instance decodeViewerCertificate :: Decode ViewerCertificate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeViewerCertificate :: Encode ViewerCertificate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ViewerProtocolPolicy = ViewerProtocolPolicy String
derive instance newtypeViewerProtocolPolicy :: Newtype ViewerProtocolPolicy _
derive instance repGenericViewerProtocolPolicy :: Generic ViewerProtocolPolicy _
instance showViewerProtocolPolicy :: Show ViewerProtocolPolicy where
  show = genericShow
instance decodeViewerProtocolPolicy :: Decode ViewerProtocolPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeViewerProtocolPolicy :: Encode ViewerProtocolPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
