

-- | <fullname>Amazon CloudFront</fullname> <p>This is the <i>Amazon CloudFront API Reference</i>. This guide is for developers who need detailed information about the CloudFront API actions, data types, and errors. For detailed information about CloudFront features and their associated API calls, see the <i>Amazon CloudFront Developer Guide</i>.</p>
module AWS.CloudFront where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudFront" :: String


-- | <p>Creates a new origin access identity. If you're using Amazon S3 for your origin, you can use an origin access identity to require users to access your content using a CloudFront URL instead of the Amazon S3 URL. For more information about how to use origin access identities, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html">Serving Private Content through CloudFront</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
createCloudFrontOriginAccessIdentity :: forall eff. CreateCloudFrontOriginAccessIdentityRequest -> Aff (err :: AWS.RequestError | eff) CreateCloudFrontOriginAccessIdentityResult
createCloudFrontOriginAccessIdentity = AWS.request serviceName "CreateCloudFrontOriginAccessIdentity" 


-- | <p>Creates a new web distribution. Send a <code>GET</code> request to the <code>/<i>CloudFront API version</i>/distribution</code>/<code>distribution ID</code> resource.</p>
createDistribution :: forall eff. CreateDistributionRequest -> Aff (err :: AWS.RequestError | eff) CreateDistributionResult
createDistribution = AWS.request serviceName "CreateDistribution" 


-- | <p>Create a new distribution with tags.</p>
createDistributionWithTags :: forall eff. CreateDistributionWithTagsRequest -> Aff (err :: AWS.RequestError | eff) CreateDistributionWithTagsResult
createDistributionWithTags = AWS.request serviceName "CreateDistributionWithTags" 


-- | <p>Create a new invalidation. </p>
createInvalidation :: forall eff. CreateInvalidationRequest -> Aff (err :: AWS.RequestError | eff) CreateInvalidationResult
createInvalidation = AWS.request serviceName "CreateInvalidation" 


-- | <p>Creates a new RMTP distribution. An RTMP distribution is similar to a web distribution, but an RTMP distribution streams media files using the Adobe Real-Time Messaging Protocol (RTMP) instead of serving files using HTTP. </p> <p>To create a new web distribution, submit a <code>POST</code> request to the <i>CloudFront API version</i>/distribution resource. The request body must include a document with a <i>StreamingDistributionConfig</i> element. The response echoes the <code>StreamingDistributionConfig</code> element and returns other information about the RTMP distribution.</p> <p>To get the status of your request, use the <i>GET StreamingDistribution</i> API action. When the value of <code>Enabled</code> is <code>true</code> and the value of <code>Status</code> is <code>Deployed</code>, your distribution is ready. A distribution usually deploys in less than 15 minutes.</p> <p>For more information about web distributions, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-rtmp.html">Working with RTMP Distributions</a> in the <i>Amazon CloudFront Developer Guide</i>.</p> <important> <p>Beginning with the 2012-05-05 version of the CloudFront API, we made substantial changes to the format of the XML document that you include in the request body when you create or update a web distribution or an RTMP distribution, and when you invalidate objects. With previous versions of the API, we discovered that it was too easy to accidentally delete one or more values for an element that accepts multiple values, for example, CNAMEs and trusted signers. Our changes for the 2012-05-05 release are intended to prevent these accidental deletions and to notify you when there's a mismatch between the number of values you say you're specifying in the <code>Quantity</code> element and the number of values specified.</p> </important>
createStreamingDistribution :: forall eff. CreateStreamingDistributionRequest -> Aff (err :: AWS.RequestError | eff) CreateStreamingDistributionResult
createStreamingDistribution = AWS.request serviceName "CreateStreamingDistribution" 


-- | <p>Create a new streaming distribution with tags.</p>
createStreamingDistributionWithTags :: forall eff. CreateStreamingDistributionWithTagsRequest -> Aff (err :: AWS.RequestError | eff) CreateStreamingDistributionWithTagsResult
createStreamingDistributionWithTags = AWS.request serviceName "CreateStreamingDistributionWithTags" 


-- | <p>Delete an origin access identity. </p>
deleteCloudFrontOriginAccessIdentity :: forall eff. DeleteCloudFrontOriginAccessIdentityRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteCloudFrontOriginAccessIdentity = AWS.request serviceName "DeleteCloudFrontOriginAccessIdentity" 


-- | <p>Delete a distribution. </p>
deleteDistribution :: forall eff. DeleteDistributionRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteDistribution = AWS.request serviceName "DeleteDistribution" 


-- | <p>Delete a streaming distribution. To delete an RTMP distribution using the CloudFront API, perform the following steps.</p> <p> <b>To delete an RTMP distribution using the CloudFront API</b>:</p> <ol> <li> <p>Disable the RTMP distribution.</p> </li> <li> <p>Submit a <code>GET Streaming Distribution Config</code> request to get the current configuration and the <code>Etag</code> header for the distribution. </p> </li> <li> <p>Update the XML document that was returned in the response to your <code>GET Streaming Distribution Config</code> request to change the value of <code>Enabled</code> to <code>false</code>.</p> </li> <li> <p>Submit a <code>PUT Streaming Distribution Config</code> request to update the configuration for your distribution. In the request body, include the XML document that you updated in Step 3. Then set the value of the HTTP <code>If-Match</code> header to the value of the <code>ETag</code> header that CloudFront returned when you submitted the <code>GET Streaming Distribution Config</code> request in Step 2.</p> </li> <li> <p>Review the response to the <code>PUT Streaming Distribution Config</code> request to confirm that the distribution was successfully disabled.</p> </li> <li> <p>Submit a <code>GET Streaming Distribution Config</code> request to confirm that your changes have propagated. When propagation is complete, the value of <code>Status</code> is <code>Deployed</code>.</p> </li> <li> <p>Submit a <code>DELETE Streaming Distribution</code> request. Set the value of the HTTP <code>If-Match</code> header to the value of the <code>ETag</code> header that CloudFront returned when you submitted the <code>GET Streaming Distribution Config</code> request in Step 2.</p> </li> <li> <p>Review the response to your <code>DELETE Streaming Distribution</code> request to confirm that the distribution was successfully deleted.</p> </li> </ol> <p>For information about deleting a distribution using the CloudFront console, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/HowToDeleteDistribution.html">Deleting a Distribution</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
deleteStreamingDistribution :: forall eff. DeleteStreamingDistributionRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteStreamingDistribution = AWS.request serviceName "DeleteStreamingDistribution" 


-- | <p>Get the information about an origin access identity. </p>
getCloudFrontOriginAccessIdentity :: forall eff. GetCloudFrontOriginAccessIdentityRequest -> Aff (err :: AWS.RequestError | eff) GetCloudFrontOriginAccessIdentityResult
getCloudFrontOriginAccessIdentity = AWS.request serviceName "GetCloudFrontOriginAccessIdentity" 


-- | <p>Get the configuration information about an origin access identity. </p>
getCloudFrontOriginAccessIdentityConfig :: forall eff. GetCloudFrontOriginAccessIdentityConfigRequest -> Aff (err :: AWS.RequestError | eff) GetCloudFrontOriginAccessIdentityConfigResult
getCloudFrontOriginAccessIdentityConfig = AWS.request serviceName "GetCloudFrontOriginAccessIdentityConfig" 


-- | <p>Get the information about a distribution. </p>
getDistribution :: forall eff. GetDistributionRequest -> Aff (err :: AWS.RequestError | eff) GetDistributionResult
getDistribution = AWS.request serviceName "GetDistribution" 


-- | <p>Get the configuration information about a distribution. </p>
getDistributionConfig :: forall eff. GetDistributionConfigRequest -> Aff (err :: AWS.RequestError | eff) GetDistributionConfigResult
getDistributionConfig = AWS.request serviceName "GetDistributionConfig" 


-- | <p>Get the information about an invalidation. </p>
getInvalidation :: forall eff. GetInvalidationRequest -> Aff (err :: AWS.RequestError | eff) GetInvalidationResult
getInvalidation = AWS.request serviceName "GetInvalidation" 


-- | <p>Gets information about a specified RTMP distribution, including the distribution configuration.</p>
getStreamingDistribution :: forall eff. GetStreamingDistributionRequest -> Aff (err :: AWS.RequestError | eff) GetStreamingDistributionResult
getStreamingDistribution = AWS.request serviceName "GetStreamingDistribution" 


-- | <p>Get the configuration information about a streaming distribution. </p>
getStreamingDistributionConfig :: forall eff. GetStreamingDistributionConfigRequest -> Aff (err :: AWS.RequestError | eff) GetStreamingDistributionConfigResult
getStreamingDistributionConfig = AWS.request serviceName "GetStreamingDistributionConfig" 


-- | <p>Lists origin access identities.</p>
listCloudFrontOriginAccessIdentities :: forall eff. ListCloudFrontOriginAccessIdentitiesRequest -> Aff (err :: AWS.RequestError | eff) ListCloudFrontOriginAccessIdentitiesResult
listCloudFrontOriginAccessIdentities = AWS.request serviceName "ListCloudFrontOriginAccessIdentities" 


-- | <p>List distributions. </p>
listDistributions :: forall eff. ListDistributionsRequest -> Aff (err :: AWS.RequestError | eff) ListDistributionsResult
listDistributions = AWS.request serviceName "ListDistributions" 


-- | <p>List the distributions that are associated with a specified AWS WAF web ACL. </p>
listDistributionsByWebACLId :: forall eff. ListDistributionsByWebACLIdRequest -> Aff (err :: AWS.RequestError | eff) ListDistributionsByWebACLIdResult
listDistributionsByWebACLId = AWS.request serviceName "ListDistributionsByWebACLId" 


-- | <p>Lists invalidation batches. </p>
listInvalidations :: forall eff. ListInvalidationsRequest -> Aff (err :: AWS.RequestError | eff) ListInvalidationsResult
listInvalidations = AWS.request serviceName "ListInvalidations" 


-- | <p>List streaming distributions. </p>
listStreamingDistributions :: forall eff. ListStreamingDistributionsRequest -> Aff (err :: AWS.RequestError | eff) ListStreamingDistributionsResult
listStreamingDistributions = AWS.request serviceName "ListStreamingDistributions" 


-- | <p>List tags for a CloudFront resource.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForResourceResult
listTagsForResource = AWS.request serviceName "ListTagsForResource" 


-- | <p>Add tags to a CloudFront resource.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (err :: AWS.RequestError | eff) Unit
tagResource = AWS.request serviceName "TagResource" 


-- | <p>Remove tags from a CloudFront resource.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: AWS.RequestError | eff) Unit
untagResource = AWS.request serviceName "UntagResource" 


-- | <p>Update an origin access identity. </p>
updateCloudFrontOriginAccessIdentity :: forall eff. UpdateCloudFrontOriginAccessIdentityRequest -> Aff (err :: AWS.RequestError | eff) UpdateCloudFrontOriginAccessIdentityResult
updateCloudFrontOriginAccessIdentity = AWS.request serviceName "UpdateCloudFrontOriginAccessIdentity" 


-- | <p>Update a distribution. </p>
updateDistribution :: forall eff. UpdateDistributionRequest -> Aff (err :: AWS.RequestError | eff) UpdateDistributionResult
updateDistribution = AWS.request serviceName "UpdateDistribution" 


-- | <p>Update a streaming distribution. </p>
updateStreamingDistribution :: forall eff. UpdateStreamingDistributionRequest -> Aff (err :: AWS.RequestError | eff) UpdateStreamingDistributionResult
updateStreamingDistribution = AWS.request serviceName "UpdateStreamingDistribution" 


-- | <p>Access denied.</p>
newtype AccessDenied = AccessDenied 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>A complex type that lists the AWS accounts, if any, that you included in the <code>TrustedSigners</code> complex type for this distribution. These are the accounts that you want to allow to create signed URLs for private content.</p> <p>The <code>Signer</code> complex type lists the AWS account number of the trusted signer or <code>self</code> if the signer is the AWS account that created the distribution. The <code>Signer</code> element also includes the IDs of any active CloudFront key pairs that are associated with the trusted signer's AWS account. If no <code>KeyPairId</code> element appears for a <code>Signer</code>, that signer can't create signed URLs. </p> <p>For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html">Serving Private Content through CloudFront</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype ActiveTrustedSigners = ActiveTrustedSigners 
  { "Enabled" :: (Boolean)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (SignerList)
  }


newtype AliasList = AliasList (Array String)


-- | <p>A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution. </p>
newtype Aliases = Aliases 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (AliasList)
  }


-- | <p>A complex type that controls which HTTP methods CloudFront processes and forwards to your Amazon S3 bucket or your custom origin. There are three choices:</p> <ul> <li> <p>CloudFront forwards only <code>GET</code> and <code>HEAD</code> requests.</p> </li> <li> <p>CloudFront forwards only <code>GET</code>, <code>HEAD</code>, and <code>OPTIONS</code> requests.</p> </li> <li> <p>CloudFront forwards <code>GET, HEAD, OPTIONS, PUT, PATCH, POST</code>, and <code>DELETE</code> requests.</p> </li> </ul> <p>If you pick the third choice, you may need to restrict access to your Amazon S3 bucket or to your custom origin so users can't perform operations that you don't want them to. For example, you might not want users to have permissions to delete objects from your origin.</p>
newtype AllowedMethods = AllowedMethods 
  { "Quantity" :: (Int)
  , "Items" :: (MethodsList)
  , "CachedMethods" :: NullOrUndefined (CachedMethods)
  }


newtype AwsAccountNumberList = AwsAccountNumberList (Array String)


newtype BatchTooLarge = BatchTooLarge 
  { "Message" :: NullOrUndefined (String)
  }


newtype CNAMEAlreadyExists = CNAMEAlreadyExists 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>A complex type that describes how CloudFront processes requests.</p> <p>You must create at least as many cache behaviors (including the default cache behavior) as you have origins if you want CloudFront to distribute objects from all of the origins. Each cache behavior specifies the one origin from which you want CloudFront to get objects. If you have two origins and only the default cache behavior, the default cache behavior will cause CloudFront to get objects from one of the origins, but the other origin is never used.</p> <p>For the current limit on the number of cache behaviors that you can add to a distribution, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_cloudfront">Amazon CloudFront Limits</a> in the <i>AWS General Reference</i>.</p> <p>If you don't want to specify any cache behaviors, include only an empty <code>CacheBehaviors</code> element. Don't include an empty <code>CacheBehavior</code> element, or CloudFront returns a <code>MalformedXML</code> error.</p> <p>To delete all cache behaviors in an existing distribution, update the distribution configuration and include only an empty <code>CacheBehaviors</code> element.</p> <p>To add, change, or remove one or more cache behaviors, update the distribution configuration and specify all of the cache behaviors that you want to include in the updated distribution.</p> <p>For more information about cache behaviors, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior">Cache Behaviors</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype CacheBehavior = CacheBehavior 
  { "PathPattern" :: (String)
  , "TargetOriginId" :: (String)
  , "ForwardedValues" :: (ForwardedValues)
  , "TrustedSigners" :: (TrustedSigners)
  , "ViewerProtocolPolicy" :: (ViewerProtocolPolicy)
  , "MinTTL" :: (Number)
  , "AllowedMethods" :: NullOrUndefined (AllowedMethods)
  , "SmoothStreaming" :: NullOrUndefined (Boolean)
  , "DefaultTTL" :: NullOrUndefined (Number)
  , "MaxTTL" :: NullOrUndefined (Number)
  , "Compress" :: NullOrUndefined (Boolean)
  , "LambdaFunctionAssociations" :: NullOrUndefined (LambdaFunctionAssociations)
  }


newtype CacheBehaviorList = CacheBehaviorList (Array CacheBehavior)


-- | <p>A complex type that contains zero or more <code>CacheBehavior</code> elements. </p>
newtype CacheBehaviors = CacheBehaviors 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (CacheBehaviorList)
  }


-- | <p>A complex type that controls whether CloudFront caches the response to requests using the specified HTTP methods. There are two choices:</p> <ul> <li> <p>CloudFront caches responses to <code>GET</code> and <code>HEAD</code> requests.</p> </li> <li> <p>CloudFront caches responses to <code>GET</code>, <code>HEAD</code>, and <code>OPTIONS</code> requests.</p> </li> </ul> <p>If you pick the second choice for your Amazon S3 Origin, you may need to forward Access-Control-Request-Method, Access-Control-Request-Headers, and Origin headers for the responses to be cached correctly. </p>
newtype CachedMethods = CachedMethods 
  { "Quantity" :: (Int)
  , "Items" :: (MethodsList)
  }


newtype CertificateSource = CertificateSource String


-- | <p>CloudFront origin access identity.</p>
newtype CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity 
  { "Id" :: (String)
  , "S3CanonicalUserId" :: (String)
  , "CloudFrontOriginAccessIdentityConfig" :: NullOrUndefined (CloudFrontOriginAccessIdentityConfig)
  }


-- | <p>If the <code>CallerReference</code> is a value you already sent in a previous request to create an identity but the content of the <code>CloudFrontOriginAccessIdentityConfig</code> is different from the original request, CloudFront returns a <code>CloudFrontOriginAccessIdentityAlreadyExists</code> error. </p>
newtype CloudFrontOriginAccessIdentityAlreadyExists = CloudFrontOriginAccessIdentityAlreadyExists 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Origin access identity configuration. Send a <code>GET</code> request to the <code>/<i>CloudFront API version</i>/CloudFront/identity ID/config</code> resource. </p>
newtype CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig 
  { "CallerReference" :: (String)
  , "Comment" :: (String)
  }


newtype CloudFrontOriginAccessIdentityInUse = CloudFrontOriginAccessIdentityInUse 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Lists the origin access identities for CloudFront.Send a <code>GET</code> request to the <code>/<i>CloudFront API version</i>/origin-access-identity/cloudfront</code> resource. The response includes a <code>CloudFrontOriginAccessIdentityList</code> element with zero or more <code>CloudFrontOriginAccessIdentitySummary</code> child elements. By default, your entire list of origin access identities is returned in one single page. If the list is long, you can paginate it using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
newtype CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList 
  { "Marker" :: (String)
  , "NextMarker" :: NullOrUndefined (String)
  , "MaxItems" :: (Int)
  , "IsTruncated" :: (Boolean)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (CloudFrontOriginAccessIdentitySummaryList)
  }


-- | <p>Summary of the information about a CloudFront origin access identity.</p>
newtype CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary 
  { "Id" :: (String)
  , "S3CanonicalUserId" :: (String)
  , "Comment" :: (String)
  }


newtype CloudFrontOriginAccessIdentitySummaryList = CloudFrontOriginAccessIdentitySummaryList (Array CloudFrontOriginAccessIdentitySummary)


newtype CookieNameList = CookieNameList (Array String)


-- | <p>A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html">How CloudFront Forwards, Caches, and Logs Cookies</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype CookieNames = CookieNames 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (CookieNameList)
  }


-- | <p>A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html">How CloudFront Forwards, Caches, and Logs Cookies</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype CookiePreference = CookiePreference 
  { "Forward" :: (ItemSelection)
  , "WhitelistedNames" :: NullOrUndefined (CookieNames)
  }


-- | <p>The request to create a new origin access identity.</p>
newtype CreateCloudFrontOriginAccessIdentityRequest = CreateCloudFrontOriginAccessIdentityRequest 
  { "CloudFrontOriginAccessIdentityConfig" :: (CloudFrontOriginAccessIdentityConfig)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype CreateCloudFrontOriginAccessIdentityResult = CreateCloudFrontOriginAccessIdentityResult 
  { "CloudFrontOriginAccessIdentity" :: NullOrUndefined (CloudFrontOriginAccessIdentity)
  , "Location" :: NullOrUndefined (String)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>The request to create a new distribution.</p>
newtype CreateDistributionRequest = CreateDistributionRequest 
  { "DistributionConfig" :: (DistributionConfig)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype CreateDistributionResult = CreateDistributionResult 
  { "Distribution" :: NullOrUndefined (Distribution)
  , "Location" :: NullOrUndefined (String)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>The request to create a new distribution with tags. </p>
newtype CreateDistributionWithTagsRequest = CreateDistributionWithTagsRequest 
  { "DistributionConfigWithTags" :: (DistributionConfigWithTags)
  }


-- | <p>The returned result of the corresponding request. </p>
newtype CreateDistributionWithTagsResult = CreateDistributionWithTagsResult 
  { "Distribution" :: NullOrUndefined (Distribution)
  , "Location" :: NullOrUndefined (String)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>The request to create an invalidation.</p>
newtype CreateInvalidationRequest = CreateInvalidationRequest 
  { "DistributionId" :: (String)
  , "InvalidationBatch" :: (InvalidationBatch)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype CreateInvalidationResult = CreateInvalidationResult 
  { "Location" :: NullOrUndefined (String)
  , "Invalidation" :: NullOrUndefined (Invalidation)
  }


-- | <p>The request to create a new streaming distribution.</p>
newtype CreateStreamingDistributionRequest = CreateStreamingDistributionRequest 
  { "StreamingDistributionConfig" :: (StreamingDistributionConfig)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype CreateStreamingDistributionResult = CreateStreamingDistributionResult 
  { "StreamingDistribution" :: NullOrUndefined (StreamingDistribution)
  , "Location" :: NullOrUndefined (String)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>The request to create a new streaming distribution with tags.</p>
newtype CreateStreamingDistributionWithTagsRequest = CreateStreamingDistributionWithTagsRequest 
  { "StreamingDistributionConfigWithTags" :: (StreamingDistributionConfigWithTags)
  }


-- | <p>The returned result of the corresponding request. </p>
newtype CreateStreamingDistributionWithTagsResult = CreateStreamingDistributionWithTagsResult 
  { "StreamingDistribution" :: NullOrUndefined (StreamingDistribution)
  , "Location" :: NullOrUndefined (String)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>A complex type that controls:</p> <ul> <li> <p>Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer. </p> </li> <li> <p>How long CloudFront caches HTTP status codes in the 4xx and 5xx range.</p> </li> </ul> <p>For more information about custom error pages, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html">Customizing Error Responses</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype CustomErrorResponse = CustomErrorResponse 
  { "ErrorCode" :: (Int)
  , "ResponsePagePath" :: NullOrUndefined (String)
  , "ResponseCode" :: NullOrUndefined (String)
  , "ErrorCachingMinTTL" :: NullOrUndefined (Number)
  }


newtype CustomErrorResponseList = CustomErrorResponseList (Array CustomErrorResponse)


-- | <p>A complex type that controls:</p> <ul> <li> <p>Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.</p> </li> <li> <p>How long CloudFront caches HTTP status codes in the 4xx and 5xx range.</p> </li> </ul> <p>For more information about custom error pages, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html">Customizing Error Responses</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype CustomErrorResponses = CustomErrorResponses 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (CustomErrorResponseList)
  }


-- | <p>A complex type that contains the list of Custom Headers for each origin. </p>
newtype CustomHeaders = CustomHeaders 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (OriginCustomHeadersList)
  }


-- | <p>A customer origin.</p>
newtype CustomOriginConfig = CustomOriginConfig 
  { "HTTPPort" :: (Int)
  , "HTTPSPort" :: (Int)
  , "OriginProtocolPolicy" :: (OriginProtocolPolicy)
  , "OriginSslProtocols" :: NullOrUndefined (OriginSslProtocols)
  }


-- | <p>A complex type that describes the default cache behavior if you do not specify a <code>CacheBehavior</code> element or if files don't match any of the values of <code>PathPattern</code> in <code>CacheBehavior</code> elements. You must create exactly one default cache behavior.</p>
newtype DefaultCacheBehavior = DefaultCacheBehavior 
  { "TargetOriginId" :: (String)
  , "ForwardedValues" :: (ForwardedValues)
  , "TrustedSigners" :: (TrustedSigners)
  , "ViewerProtocolPolicy" :: (ViewerProtocolPolicy)
  , "MinTTL" :: (Number)
  , "AllowedMethods" :: NullOrUndefined (AllowedMethods)
  , "SmoothStreaming" :: NullOrUndefined (Boolean)
  , "DefaultTTL" :: NullOrUndefined (Number)
  , "MaxTTL" :: NullOrUndefined (Number)
  , "Compress" :: NullOrUndefined (Boolean)
  , "LambdaFunctionAssociations" :: NullOrUndefined (LambdaFunctionAssociations)
  }


-- | <p>Deletes a origin access identity.</p>
newtype DeleteCloudFrontOriginAccessIdentityRequest = DeleteCloudFrontOriginAccessIdentityRequest 
  { "Id" :: (String)
  , "IfMatch" :: NullOrUndefined (String)
  }


-- | <p>This action deletes a web distribution. To delete a web distribution using the CloudFront API, perform the following steps.</p> <p> <b>To delete a web distribution using the CloudFront API:</b> </p> <ol> <li> <p>Disable the web distribution </p> </li> <li> <p>Submit a <code>GET Distribution Config</code> request to get the current configuration and the <code>Etag</code> header for the distribution.</p> </li> <li> <p>Update the XML document that was returned in the response to your <code>GET Distribution Config</code> request to change the value of <code>Enabled</code> to <code>false</code>.</p> </li> <li> <p>Submit a <code>PUT Distribution Config</code> request to update the configuration for your distribution. In the request body, include the XML document that you updated in Step 3. Set the value of the HTTP <code>If-Match</code> header to the value of the <code>ETag</code> header that CloudFront returned when you submitted the <code>GET Distribution Config</code> request in Step 2.</p> </li> <li> <p>Review the response to the <code>PUT Distribution Config</code> request to confirm that the distribution was successfully disabled.</p> </li> <li> <p>Submit a <code>GET Distribution</code> request to confirm that your changes have propagated. When propagation is complete, the value of <code>Status</code> is <code>Deployed</code>.</p> </li> <li> <p>Submit a <code>DELETE Distribution</code> request. Set the value of the HTTP <code>If-Match</code> header to the value of the <code>ETag</code> header that CloudFront returned when you submitted the <code>GET Distribution Config</code> request in Step 6.</p> </li> <li> <p>Review the response to your <code>DELETE Distribution</code> request to confirm that the distribution was successfully deleted.</p> </li> </ol> <p>For information about deleting a distribution using the CloudFront console, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/HowToDeleteDistribution.html">Deleting a Distribution</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype DeleteDistributionRequest = DeleteDistributionRequest 
  { "Id" :: (String)
  , "IfMatch" :: NullOrUndefined (String)
  }


-- | <p>The request to delete a streaming distribution.</p>
newtype DeleteStreamingDistributionRequest = DeleteStreamingDistributionRequest 
  { "Id" :: (String)
  , "IfMatch" :: NullOrUndefined (String)
  }


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


-- | <p>The caller reference you attempted to create the distribution with is associated with another distribution.</p>
newtype DistributionAlreadyExists = DistributionAlreadyExists 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>A distribution configuration.</p>
newtype DistributionConfig = DistributionConfig 
  { "CallerReference" :: (String)
  , "Aliases" :: NullOrUndefined (Aliases)
  , "DefaultRootObject" :: NullOrUndefined (String)
  , "Origins" :: (Origins)
  , "DefaultCacheBehavior" :: (DefaultCacheBehavior)
  , "CacheBehaviors" :: NullOrUndefined (CacheBehaviors)
  , "CustomErrorResponses" :: NullOrUndefined (CustomErrorResponses)
  , "Comment" :: (String)
  , "Logging" :: NullOrUndefined (LoggingConfig)
  , "PriceClass" :: NullOrUndefined (PriceClass)
  , "Enabled" :: (Boolean)
  , "ViewerCertificate" :: NullOrUndefined (ViewerCertificate)
  , "Restrictions" :: NullOrUndefined (Restrictions)
  , "WebACLId" :: NullOrUndefined (String)
  , "HttpVersion" :: NullOrUndefined (HttpVersion)
  , "IsIPV6Enabled" :: NullOrUndefined (Boolean)
  }


-- | <p>A distribution Configuration and a list of tags to be associated with the distribution.</p>
newtype DistributionConfigWithTags = DistributionConfigWithTags 
  { "DistributionConfig" :: (DistributionConfig)
  , "Tags" :: (Tags)
  }


-- | <p>A distribution list.</p>
newtype DistributionList = DistributionList 
  { "Marker" :: (String)
  , "NextMarker" :: NullOrUndefined (String)
  , "MaxItems" :: (Int)
  , "IsTruncated" :: (Boolean)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (DistributionSummaryList)
  }


newtype DistributionNotDisabled = DistributionNotDisabled 
  { "Message" :: NullOrUndefined (String)
  }


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


newtype DistributionSummaryList = DistributionSummaryList (Array DistributionSummary)


newtype EventType = EventType String


-- | <p>A complex type that specifies how CloudFront handles query strings and cookies.</p>
newtype ForwardedValues = ForwardedValues 
  { "QueryString" :: (Boolean)
  , "Cookies" :: (CookiePreference)
  , "Headers" :: NullOrUndefined (Headers)
  , "QueryStringCacheKeys" :: NullOrUndefined (QueryStringCacheKeys)
  }


-- | <p>A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using <code>MaxMind</code> GeoIP databases. </p>
newtype GeoRestriction = GeoRestriction 
  { "RestrictionType" :: (GeoRestrictionType)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (LocationList)
  }


newtype GeoRestrictionType = GeoRestrictionType String


-- | <p>The origin access identity's configuration information. For more information, see <a>CloudFrontOriginAccessIdentityConfigComplexType</a>.</p>
newtype GetCloudFrontOriginAccessIdentityConfigRequest = GetCloudFrontOriginAccessIdentityConfigRequest 
  { "Id" :: (String)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype GetCloudFrontOriginAccessIdentityConfigResult = GetCloudFrontOriginAccessIdentityConfigResult 
  { "CloudFrontOriginAccessIdentityConfig" :: NullOrUndefined (CloudFrontOriginAccessIdentityConfig)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>The request to get an origin access identity's information.</p>
newtype GetCloudFrontOriginAccessIdentityRequest = GetCloudFrontOriginAccessIdentityRequest 
  { "Id" :: (String)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype GetCloudFrontOriginAccessIdentityResult = GetCloudFrontOriginAccessIdentityResult 
  { "CloudFrontOriginAccessIdentity" :: NullOrUndefined (CloudFrontOriginAccessIdentity)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>The request to get a distribution configuration.</p>
newtype GetDistributionConfigRequest = GetDistributionConfigRequest 
  { "Id" :: (String)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype GetDistributionConfigResult = GetDistributionConfigResult 
  { "DistributionConfig" :: NullOrUndefined (DistributionConfig)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>The request to get a distribution's information.</p>
newtype GetDistributionRequest = GetDistributionRequest 
  { "Id" :: (String)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype GetDistributionResult = GetDistributionResult 
  { "Distribution" :: NullOrUndefined (Distribution)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>The request to get an invalidation's information. </p>
newtype GetInvalidationRequest = GetInvalidationRequest 
  { "DistributionId" :: (String)
  , "Id" :: (String)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype GetInvalidationResult = GetInvalidationResult 
  { "Invalidation" :: NullOrUndefined (Invalidation)
  }


-- | <p>To request to get a streaming distribution configuration.</p>
newtype GetStreamingDistributionConfigRequest = GetStreamingDistributionConfigRequest 
  { "Id" :: (String)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype GetStreamingDistributionConfigResult = GetStreamingDistributionConfigResult 
  { "StreamingDistributionConfig" :: NullOrUndefined (StreamingDistributionConfig)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>The request to get a streaming distribution's information.</p>
newtype GetStreamingDistributionRequest = GetStreamingDistributionRequest 
  { "Id" :: (String)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype GetStreamingDistributionResult = GetStreamingDistributionResult 
  { "StreamingDistribution" :: NullOrUndefined (StreamingDistribution)
  , "ETag" :: NullOrUndefined (String)
  }


newtype HeaderList = HeaderList (Array String)


-- | <p>A complex type that specifies the headers that you want CloudFront to forward to the origin for this cache behavior.</p> <p>For the headers that you specify, CloudFront also caches separate versions of a specified object based on the header values in viewer requests. For example, suppose viewer requests for <code>logo.jpg</code> contain a custom <code>Product</code> header that has a value of either <code>Acme</code> or <code>Apex</code>, and you configure CloudFront to cache your content based on values in the <code>Product</code> header. CloudFront forwards the <code>Product</code> header to the origin and caches the response from the origin once for each header value. For more information about caching based on header values, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html">How CloudFront Forwards and Caches Headers</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype Headers = Headers 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (HeaderList)
  }


newtype HttpVersion = HttpVersion String


-- | <p>Origin and <code>CallerReference</code> cannot be updated. </p>
newtype IllegalUpdate = IllegalUpdate 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The value of <code>Quantity</code> and the size of <code>Items</code> do not match.</p>
newtype InconsistentQuantities = InconsistentQuantities 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The argument is invalid.</p>
newtype InvalidArgument = InvalidArgument 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The default root object file name is too big or contains an invalid character.</p>
newtype InvalidDefaultRootObject = InvalidDefaultRootObject 
  { "Message" :: NullOrUndefined (String)
  }


newtype InvalidErrorCode = InvalidErrorCode 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Your request contains forward cookies option which doesn't match with the expectation for the <code>whitelisted</code> list of cookie names. Either list of cookie names has been specified when not allowed or list of cookie names is missing when expected.</p>
newtype InvalidForwardCookies = InvalidForwardCookies 
  { "Message" :: NullOrUndefined (String)
  }


newtype InvalidGeoRestrictionParameter = InvalidGeoRestrictionParameter 
  { "Message" :: NullOrUndefined (String)
  }


newtype InvalidHeadersForS3Origin = InvalidHeadersForS3Origin 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The <code>If-Match</code> version is missing or not valid for the distribution.</p>
newtype InvalidIfMatchVersion = InvalidIfMatchVersion 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The specified Lambda function association is invalid.</p>
newtype InvalidLambdaFunctionAssociation = InvalidLambdaFunctionAssociation 
  { "Message" :: NullOrUndefined (String)
  }


newtype InvalidLocationCode = InvalidLocationCode 
  { "Message" :: NullOrUndefined (String)
  }


newtype InvalidMinimumProtocolVersion = InvalidMinimumProtocolVersion 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The Amazon S3 origin server specified does not refer to a valid Amazon S3 bucket.</p>
newtype InvalidOrigin = InvalidOrigin 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The origin access identity is not valid or doesn't exist.</p>
newtype InvalidOriginAccessIdentity = InvalidOriginAccessIdentity 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>You cannot specify SSLv3 as the minimum protocol version if you only want to support only clients that support Server Name Indication (SNI).</p>
newtype InvalidProtocolSettings = InvalidProtocolSettings 
  { "Message" :: NullOrUndefined (String)
  }


newtype InvalidQueryStringParameters = InvalidQueryStringParameters 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The relative path is too big, is not URL-encoded, or does not begin with a slash (/).</p>
newtype InvalidRelativePath = InvalidRelativePath 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>This operation requires the HTTPS protocol. Ensure that you specify the HTTPS protocol in your request, or omit the <code>RequiredProtocols</code> element from your distribution configuration.</p>
newtype InvalidRequiredProtocol = InvalidRequiredProtocol 
  { "Message" :: NullOrUndefined (String)
  }


newtype InvalidResponseCode = InvalidResponseCode 
  { "Message" :: NullOrUndefined (String)
  }


newtype InvalidTTLOrder = InvalidTTLOrder 
  { "Message" :: NullOrUndefined (String)
  }


newtype InvalidTagging = InvalidTagging 
  { "Message" :: NullOrUndefined (String)
  }


newtype InvalidViewerCertificate = InvalidViewerCertificate 
  { "Message" :: NullOrUndefined (String)
  }


newtype InvalidWebACLId = InvalidWebACLId 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>An invalidation. </p>
newtype Invalidation = Invalidation 
  { "Id" :: (String)
  , "Status" :: (String)
  , "CreateTime" :: (Number)
  , "InvalidationBatch" :: (InvalidationBatch)
  }


-- | <p>An invalidation batch.</p>
newtype InvalidationBatch = InvalidationBatch 
  { "Paths" :: (Paths)
  , "CallerReference" :: (String)
  }


-- | <p>The <code>InvalidationList</code> complex type describes the list of invalidation objects. For more information about invalidation, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html">Invalidating Objects (Web Distributions Only)</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>
newtype InvalidationList = InvalidationList 
  { "Marker" :: (String)
  , "NextMarker" :: NullOrUndefined (String)
  , "MaxItems" :: (Int)
  , "IsTruncated" :: (Boolean)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (InvalidationSummaryList)
  }


-- | <p>A summary of an invalidation request.</p>
newtype InvalidationSummary = InvalidationSummary 
  { "Id" :: (String)
  , "CreateTime" :: (Number)
  , "Status" :: (String)
  }


newtype InvalidationSummaryList = InvalidationSummaryList (Array InvalidationSummary)


newtype ItemSelection = ItemSelection String


newtype KeyPairIdList = KeyPairIdList (Array String)


-- | <p>A complex type that lists the active CloudFront key pairs, if any, that are associated with <code>AwsAccountNumber</code>. </p> <p>For more information, see <a>ActiveTrustedSigners</a>.</p>
newtype KeyPairIds = KeyPairIds 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (KeyPairIdList)
  }


-- | <p>A complex type that contains a Lambda function association.</p>
newtype LambdaFunctionAssociation = LambdaFunctionAssociation 
  { "LambdaFunctionARN" :: NullOrUndefined (String)
  , "EventType" :: NullOrUndefined (EventType)
  }


newtype LambdaFunctionAssociationList = LambdaFunctionAssociationList (Array LambdaFunctionAssociation)


-- | <p>A complex type that specifies a list of Lambda functions associations for a cache behavior.</p> <p>If you want to invoke one or more Lambda functions triggered by requests that match the <code>PathPattern</code> of the cache behavior, specify the applicable values for <code>Quantity</code> and <code>Items</code>. Note that there can be up to 4 <code>LambdaFunctionAssociation</code> items in this list (one for each possible value of <code>EventType</code>) and each <code>EventType</code> can be associated with the Lambda function only once.</p> <p>If you don't want to invoke any Lambda functions for the requests that match <code>PathPattern</code>, specify <code>0</code> for <code>Quantity</code> and omit <code>Items</code>. </p>
newtype LambdaFunctionAssociations = LambdaFunctionAssociations 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (LambdaFunctionAssociationList)
  }


-- | <p>The request to list origin access identities. </p>
newtype ListCloudFrontOriginAccessIdentitiesRequest = ListCloudFrontOriginAccessIdentitiesRequest 
  { "Marker" :: NullOrUndefined (String)
  , "MaxItems" :: NullOrUndefined (String)
  }


-- | <p>The returned result of the corresponding request. </p>
newtype ListCloudFrontOriginAccessIdentitiesResult = ListCloudFrontOriginAccessIdentitiesResult 
  { "CloudFrontOriginAccessIdentityList" :: NullOrUndefined (CloudFrontOriginAccessIdentityList)
  }


-- | <p>The request to list distributions that are associated with a specified AWS WAF web ACL. </p>
newtype ListDistributionsByWebACLIdRequest = ListDistributionsByWebACLIdRequest 
  { "Marker" :: NullOrUndefined (String)
  , "MaxItems" :: NullOrUndefined (String)
  , "WebACLId" :: (String)
  }


-- | <p>The response to a request to list the distributions that are associated with a specified AWS WAF web ACL. </p>
newtype ListDistributionsByWebACLIdResult = ListDistributionsByWebACLIdResult 
  { "DistributionList" :: NullOrUndefined (DistributionList)
  }


-- | <p>The request to list your distributions. </p>
newtype ListDistributionsRequest = ListDistributionsRequest 
  { "Marker" :: NullOrUndefined (String)
  , "MaxItems" :: NullOrUndefined (String)
  }


-- | <p>The returned result of the corresponding request. </p>
newtype ListDistributionsResult = ListDistributionsResult 
  { "DistributionList" :: NullOrUndefined (DistributionList)
  }


-- | <p>The request to list invalidations. </p>
newtype ListInvalidationsRequest = ListInvalidationsRequest 
  { "DistributionId" :: (String)
  , "Marker" :: NullOrUndefined (String)
  , "MaxItems" :: NullOrUndefined (String)
  }


-- | <p>The returned result of the corresponding request. </p>
newtype ListInvalidationsResult = ListInvalidationsResult 
  { "InvalidationList" :: NullOrUndefined (InvalidationList)
  }


-- | <p>The request to list your streaming distributions. </p>
newtype ListStreamingDistributionsRequest = ListStreamingDistributionsRequest 
  { "Marker" :: NullOrUndefined (String)
  , "MaxItems" :: NullOrUndefined (String)
  }


-- | <p>The returned result of the corresponding request. </p>
newtype ListStreamingDistributionsResult = ListStreamingDistributionsResult 
  { "StreamingDistributionList" :: NullOrUndefined (StreamingDistributionList)
  }


-- | <p> The request to list tags for a CloudFront resource.</p>
newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "Resource" :: (ResourceARN)
  }


-- | <p> The returned result of the corresponding request.</p>
newtype ListTagsForResourceResult = ListTagsForResourceResult 
  { "Tags" :: (Tags)
  }


newtype LocationList = LocationList (Array String)


-- | <p>A complex type that controls whether access logs are written for the distribution.</p>
newtype LoggingConfig = LoggingConfig 
  { "Enabled" :: (Boolean)
  , "IncludeCookies" :: (Boolean)
  , "Bucket" :: (String)
  , "Prefix" :: (String)
  }


newtype Method = Method String


newtype MethodsList = MethodsList (Array Method)


newtype MinimumProtocolVersion = MinimumProtocolVersion String


-- | <p>This operation requires a body. Ensure that the body is present and the Content-Type header is set.</p>
newtype MissingBody = MissingBody 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The specified origin access identity does not exist.</p>
newtype NoSuchCloudFrontOriginAccessIdentity = NoSuchCloudFrontOriginAccessIdentity 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The specified distribution does not exist.</p>
newtype NoSuchDistribution = NoSuchDistribution 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The specified invalidation does not exist.</p>
newtype NoSuchInvalidation = NoSuchInvalidation 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>No origin exists with the specified <code>Origin Id</code>. </p>
newtype NoSuchOrigin = NoSuchOrigin 
  { "Message" :: NullOrUndefined (String)
  }


newtype NoSuchResource = NoSuchResource 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The specified streaming distribution does not exist.</p>
newtype NoSuchStreamingDistribution = NoSuchStreamingDistribution 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>A complex type that describes the Amazon S3 bucket or the HTTP server (for example, a web server) from which CloudFront gets your files. You must create at least one origin.</p> <p>For the current limit on the number of origins that you can create for a distribution, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_cloudfront">Amazon CloudFront Limits</a> in the <i>AWS General Reference</i>.</p>
newtype Origin = Origin 
  { "Id" :: (String)
  , "DomainName" :: (String)
  , "OriginPath" :: NullOrUndefined (String)
  , "CustomHeaders" :: NullOrUndefined (CustomHeaders)
  , "S3OriginConfig" :: NullOrUndefined (S3OriginConfig)
  , "CustomOriginConfig" :: NullOrUndefined (CustomOriginConfig)
  }


-- | <p>A complex type that contains <code>HeaderName</code> and <code>HeaderValue</code> elements, if any, for this distribution. </p>
newtype OriginCustomHeader = OriginCustomHeader 
  { "HeaderName" :: (String)
  , "HeaderValue" :: (String)
  }


newtype OriginCustomHeadersList = OriginCustomHeadersList (Array OriginCustomHeader)


newtype OriginList = OriginList (Array Origin)


newtype OriginProtocolPolicy = OriginProtocolPolicy String


-- | <p>A complex type that contains information about the SSL/TLS protocols that CloudFront can use when establishing an HTTPS connection with your origin. </p>
newtype OriginSslProtocols = OriginSslProtocols 
  { "Quantity" :: (Int)
  , "Items" :: (SslProtocolsList)
  }


-- | <p>A complex type that contains information about origins for this distribution. </p>
newtype Origins = Origins 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (OriginList)
  }


newtype PathList = PathList (Array String)


-- | <p>A complex type that contains information about the objects that you want to invalidate. For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects">Specifying the Objects to Invalidate</a> in the <i>Amazon CloudFront Developer Guide</i>. </p>
newtype Paths = Paths 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (PathList)
  }


-- | <p>The precondition given in one or more of the request-header fields evaluated to <code>false</code>. </p>
newtype PreconditionFailed = PreconditionFailed 
  { "Message" :: NullOrUndefined (String)
  }


newtype PriceClass = PriceClass String


newtype QueryStringCacheKeys = QueryStringCacheKeys 
  { "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (QueryStringCacheKeysList)
  }


newtype QueryStringCacheKeysList = QueryStringCacheKeysList (Array String)


newtype ResourceARN = ResourceARN String


-- | <p>A complex type that identifies ways in which you want to restrict distribution of your content.</p>
newtype Restrictions = Restrictions 
  { "GeoRestriction" :: (GeoRestriction)
  }


-- | <p>A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.</p>
newtype S3Origin = S3Origin 
  { "DomainName" :: (String)
  , "OriginAccessIdentity" :: (String)
  }


-- | <p>A complex type that contains information about the Amazon S3 origin. If the origin is a custom origin, use the <code>CustomOriginConfig</code> element instead.</p>
newtype S3OriginConfig = S3OriginConfig 
  { "OriginAccessIdentity" :: (String)
  }


newtype SSLSupportMethod = SSLSupportMethod String


-- | <p>A complex type that lists the AWS accounts that were included in the <code>TrustedSigners</code> complex type, as well as their active CloudFront key pair IDs, if any. </p>
newtype Signer = Signer 
  { "AwsAccountNumber" :: NullOrUndefined (String)
  , "KeyPairIds" :: NullOrUndefined (KeyPairIds)
  }


newtype SignerList = SignerList (Array Signer)


newtype SslProtocol = SslProtocol String


newtype SslProtocolsList = SslProtocolsList (Array SslProtocol)


-- | <p>A streaming distribution. </p>
newtype StreamingDistribution = StreamingDistribution 
  { "Id" :: (String)
  , "ARN" :: (String)
  , "Status" :: (String)
  , "LastModifiedTime" :: NullOrUndefined (Number)
  , "DomainName" :: (String)
  , "ActiveTrustedSigners" :: (ActiveTrustedSigners)
  , "StreamingDistributionConfig" :: (StreamingDistributionConfig)
  }


newtype StreamingDistributionAlreadyExists = StreamingDistributionAlreadyExists 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The RTMP distribution's configuration information.</p>
newtype StreamingDistributionConfig = StreamingDistributionConfig 
  { "CallerReference" :: (String)
  , "S3Origin" :: (S3Origin)
  , "Aliases" :: NullOrUndefined (Aliases)
  , "Comment" :: (String)
  , "Logging" :: NullOrUndefined (StreamingLoggingConfig)
  , "TrustedSigners" :: (TrustedSigners)
  , "PriceClass" :: NullOrUndefined (PriceClass)
  , "Enabled" :: (Boolean)
  }


-- | <p>A streaming distribution Configuration and a list of tags to be associated with the streaming distribution.</p>
newtype StreamingDistributionConfigWithTags = StreamingDistributionConfigWithTags 
  { "StreamingDistributionConfig" :: (StreamingDistributionConfig)
  , "Tags" :: (Tags)
  }


-- | <p>A streaming distribution list. </p>
newtype StreamingDistributionList = StreamingDistributionList 
  { "Marker" :: (String)
  , "NextMarker" :: NullOrUndefined (String)
  , "MaxItems" :: (Int)
  , "IsTruncated" :: (Boolean)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (StreamingDistributionSummaryList)
  }


newtype StreamingDistributionNotDisabled = StreamingDistributionNotDisabled 
  { "Message" :: NullOrUndefined (String)
  }


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


newtype StreamingDistributionSummaryList = StreamingDistributionSummaryList (Array StreamingDistributionSummary)


-- | <p>A complex type that controls whether access logs are written for this streaming distribution.</p>
newtype StreamingLoggingConfig = StreamingLoggingConfig 
  { "Enabled" :: (Boolean)
  , "Bucket" :: (String)
  , "Prefix" :: (String)
  }


-- | <p> A complex type that contains <code>Tag</code> key and <code>Tag</code> value.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }


-- | <p> A string that contains <code>Tag</code> key.</p> <p>The string length should be between 1 and 128 characters. Valid characters include <code>a-z</code>, <code>A-Z</code>, <code>0-9</code>, space, and the special characters <code>_ - . : / = + @</code>.</p>
newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array TagKey)


-- | <p> A complex type that contains zero or more <code>Tag</code> elements.</p>
newtype TagKeys = TagKeys 
  { "Items" :: NullOrUndefined (TagKeyList)
  }


newtype TagList = TagList (Array Tag)


-- | <p> The request to add tags to a CloudFront resource.</p>
newtype TagResourceRequest = TagResourceRequest 
  { "Resource" :: (ResourceARN)
  , "Tags" :: (Tags)
  }


newtype TagValue = TagValue String


-- | <p> A complex type that contains zero or more <code>Tag</code> elements.</p>
newtype Tags = Tags 
  { "Items" :: NullOrUndefined (TagList)
  }


-- | <p>You cannot create more cache behaviors for the distribution.</p>
newtype TooManyCacheBehaviors = TooManyCacheBehaviors 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>You cannot create anymore custom SSL/TLS certificates.</p>
newtype TooManyCertificates = TooManyCertificates 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Processing your request would cause you to exceed the maximum number of origin access identities allowed.</p>
newtype TooManyCloudFrontOriginAccessIdentities = TooManyCloudFrontOriginAccessIdentities 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Your request contains more cookie names in the whitelist than are allowed per cache behavior.</p>
newtype TooManyCookieNamesInWhiteList = TooManyCookieNamesInWhiteList 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Your request contains more CNAMEs than are allowed per distribution.</p>
newtype TooManyDistributionCNAMEs = TooManyDistributionCNAMEs 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Processing your request would cause you to exceed the maximum number of distributions allowed.</p>
newtype TooManyDistributions = TooManyDistributions 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Processing your request would cause the maximum number of distributions with Lambda function associations per owner to be exceeded.</p>
newtype TooManyDistributionsWithLambdaAssociations = TooManyDistributionsWithLambdaAssociations 
  { "Message" :: NullOrUndefined (String)
  }


newtype TooManyHeadersInForwardedValues = TooManyHeadersInForwardedValues 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>You have exceeded the maximum number of allowable InProgress invalidation batch requests, or invalidation objects.</p>
newtype TooManyInvalidationsInProgress = TooManyInvalidationsInProgress 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Your request contains more Lambda function associations than are allowed per distribution.</p>
newtype TooManyLambdaFunctionAssociations = TooManyLambdaFunctionAssociations 
  { "Message" :: NullOrUndefined (String)
  }


newtype TooManyOriginCustomHeaders = TooManyOriginCustomHeaders 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>You cannot create more origins for the distribution.</p>
newtype TooManyOrigins = TooManyOrigins 
  { "Message" :: NullOrUndefined (String)
  }


newtype TooManyQueryStringParameters = TooManyQueryStringParameters 
  { "Message" :: NullOrUndefined (String)
  }


newtype TooManyStreamingDistributionCNAMEs = TooManyStreamingDistributionCNAMEs 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Processing your request would cause you to exceed the maximum number of streaming distributions allowed.</p>
newtype TooManyStreamingDistributions = TooManyStreamingDistributions 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Your request contains more trusted signers than are allowed per distribution.</p>
newtype TooManyTrustedSigners = TooManyTrustedSigners 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>One or more of your trusted signers do not exist.</p>
newtype TrustedSignerDoesNotExist = TrustedSignerDoesNotExist 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content.</p> <p>If you want to require signed URLs in requests for objects in the target origin that match the <code>PathPattern</code> for this cache behavior, specify <code>true</code> for <code>Enabled</code>, and specify the applicable values for <code>Quantity</code> and <code>Items</code>. For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html">Serving Private Content through CloudFront</a> in the <i>Amazon Amazon CloudFront Developer Guide</i>.</p> <p>If you don't want to require signed URLs in requests for objects that match <code>PathPattern</code>, specify <code>false</code> for <code>Enabled</code> and <code>0</code> for <code>Quantity</code>. Omit <code>Items</code>.</p> <p>To add, change, or remove one or more trusted signers, change <code>Enabled</code> to <code>true</code> (if it's currently <code>false</code>), change <code>Quantity</code> as applicable, and specify all of the trusted signers that you want to include in the updated distribution.</p> <p>For more information about updating the distribution configuration, see <a>DistributionConfig</a> .</p>
newtype TrustedSigners = TrustedSigners 
  { "Enabled" :: (Boolean)
  , "Quantity" :: (Int)
  , "Items" :: NullOrUndefined (AwsAccountNumberList)
  }


-- | <p> The request to remove tags from a CloudFront resource.</p>
newtype UntagResourceRequest = UntagResourceRequest 
  { "Resource" :: (ResourceARN)
  , "TagKeys" :: (TagKeys)
  }


-- | <p>The request to update an origin access identity.</p>
newtype UpdateCloudFrontOriginAccessIdentityRequest = UpdateCloudFrontOriginAccessIdentityRequest 
  { "CloudFrontOriginAccessIdentityConfig" :: (CloudFrontOriginAccessIdentityConfig)
  , "Id" :: (String)
  , "IfMatch" :: NullOrUndefined (String)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype UpdateCloudFrontOriginAccessIdentityResult = UpdateCloudFrontOriginAccessIdentityResult 
  { "CloudFrontOriginAccessIdentity" :: NullOrUndefined (CloudFrontOriginAccessIdentity)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>The request to update a distribution.</p>
newtype UpdateDistributionRequest = UpdateDistributionRequest 
  { "DistributionConfig" :: (DistributionConfig)
  , "Id" :: (String)
  , "IfMatch" :: NullOrUndefined (String)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype UpdateDistributionResult = UpdateDistributionResult 
  { "Distribution" :: NullOrUndefined (Distribution)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>The request to update a streaming distribution.</p>
newtype UpdateStreamingDistributionRequest = UpdateStreamingDistributionRequest 
  { "StreamingDistributionConfig" :: (StreamingDistributionConfig)
  , "Id" :: (String)
  , "IfMatch" :: NullOrUndefined (String)
  }


-- | <p>The returned result of the corresponding request.</p>
newtype UpdateStreamingDistributionResult = UpdateStreamingDistributionResult 
  { "StreamingDistribution" :: NullOrUndefined (StreamingDistribution)
  , "ETag" :: NullOrUndefined (String)
  }


-- | <p>A complex type that specifies the following:</p> <ul> <li> <p>Which SSL/TLS certificate to use when viewers request objects using HTTPS</p> </li> <li> <p>Whether you want CloudFront to use dedicated IP addresses or SNI when you're using alternate domain names in your object names</p> </li> <li> <p>The minimum protocol version that you want CloudFront to use when communicating with viewers</p> </li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html">Using an HTTPS Connection to Access Your Objects</a> in the <i>Amazon Amazon CloudFront Developer Guide</i>.</p>
newtype ViewerCertificate = ViewerCertificate 
  { "CloudFrontDefaultCertificate" :: NullOrUndefined (Boolean)
  , "IAMCertificateId" :: NullOrUndefined (String)
  , "ACMCertificateArn" :: NullOrUndefined (String)
  , "SSLSupportMethod" :: NullOrUndefined (SSLSupportMethod)
  , "MinimumProtocolVersion" :: NullOrUndefined (MinimumProtocolVersion)
  , "Certificate" :: NullOrUndefined (String)
  , "CertificateSource" :: NullOrUndefined (CertificateSource)
  }


newtype ViewerProtocolPolicy = ViewerProtocolPolicy String
