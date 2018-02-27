## Module AWS.CloudFront

<fullname>Amazon CloudFront</fullname> <p>This is the <i>Amazon CloudFront API Reference</i>. This guide is for developers who need detailed information about the CloudFront API actions, data types, and errors. For detailed information about CloudFront features and their associated API calls, see the <i>Amazon CloudFront Developer Guide</i>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createCloudFrontOriginAccessIdentity`

``` purescript
createCloudFrontOriginAccessIdentity :: forall eff. CreateCloudFrontOriginAccessIdentityRequest -> Aff (err :: RequestError | eff) CreateCloudFrontOriginAccessIdentityResult
```

<p>Creates a new origin access identity. If you're using Amazon S3 for your origin, you can use an origin access identity to require users to access your content using a CloudFront URL instead of the Amazon S3 URL. For more information about how to use origin access identities, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html">Serving Private Content through CloudFront</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>

#### `createDistribution`

``` purescript
createDistribution :: forall eff. CreateDistributionRequest -> Aff (err :: RequestError | eff) CreateDistributionResult
```

<p>Creates a new web distribution. Send a <code>GET</code> request to the <code>/<i>CloudFront API version</i>/distribution</code>/<code>distribution ID</code> resource.</p>

#### `createDistributionWithTags`

``` purescript
createDistributionWithTags :: forall eff. CreateDistributionWithTagsRequest -> Aff (err :: RequestError | eff) CreateDistributionWithTagsResult
```

<p>Create a new distribution with tags.</p>

#### `createInvalidation`

``` purescript
createInvalidation :: forall eff. CreateInvalidationRequest -> Aff (err :: RequestError | eff) CreateInvalidationResult
```

<p>Create a new invalidation. </p>

#### `createStreamingDistribution`

``` purescript
createStreamingDistribution :: forall eff. CreateStreamingDistributionRequest -> Aff (err :: RequestError | eff) CreateStreamingDistributionResult
```

<p>Creates a new RMTP distribution. An RTMP distribution is similar to a web distribution, but an RTMP distribution streams media files using the Adobe Real-Time Messaging Protocol (RTMP) instead of serving files using HTTP. </p> <p>To create a new web distribution, submit a <code>POST</code> request to the <i>CloudFront API version</i>/distribution resource. The request body must include a document with a <i>StreamingDistributionConfig</i> element. The response echoes the <code>StreamingDistributionConfig</code> element and returns other information about the RTMP distribution.</p> <p>To get the status of your request, use the <i>GET StreamingDistribution</i> API action. When the value of <code>Enabled</code> is <code>true</code> and the value of <code>Status</code> is <code>Deployed</code>, your distribution is ready. A distribution usually deploys in less than 15 minutes.</p> <p>For more information about web distributions, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-rtmp.html">Working with RTMP Distributions</a> in the <i>Amazon CloudFront Developer Guide</i>.</p> <important> <p>Beginning with the 2012-05-05 version of the CloudFront API, we made substantial changes to the format of the XML document that you include in the request body when you create or update a web distribution or an RTMP distribution, and when you invalidate objects. With previous versions of the API, we discovered that it was too easy to accidentally delete one or more values for an element that accepts multiple values, for example, CNAMEs and trusted signers. Our changes for the 2012-05-05 release are intended to prevent these accidental deletions and to notify you when there's a mismatch between the number of values you say you're specifying in the <code>Quantity</code> element and the number of values specified.</p> </important>

#### `createStreamingDistributionWithTags`

``` purescript
createStreamingDistributionWithTags :: forall eff. CreateStreamingDistributionWithTagsRequest -> Aff (err :: RequestError | eff) CreateStreamingDistributionWithTagsResult
```

<p>Create a new streaming distribution with tags.</p>

#### `deleteCloudFrontOriginAccessIdentity`

``` purescript
deleteCloudFrontOriginAccessIdentity :: forall eff. DeleteCloudFrontOriginAccessIdentityRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Delete an origin access identity. </p>

#### `deleteDistribution`

``` purescript
deleteDistribution :: forall eff. DeleteDistributionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Delete a distribution. </p>

#### `deleteStreamingDistribution`

``` purescript
deleteStreamingDistribution :: forall eff. DeleteStreamingDistributionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Delete a streaming distribution. To delete an RTMP distribution using the CloudFront API, perform the following steps.</p> <p> <b>To delete an RTMP distribution using the CloudFront API</b>:</p> <ol> <li> <p>Disable the RTMP distribution.</p> </li> <li> <p>Submit a <code>GET Streaming Distribution Config</code> request to get the current configuration and the <code>Etag</code> header for the distribution. </p> </li> <li> <p>Update the XML document that was returned in the response to your <code>GET Streaming Distribution Config</code> request to change the value of <code>Enabled</code> to <code>false</code>.</p> </li> <li> <p>Submit a <code>PUT Streaming Distribution Config</code> request to update the configuration for your distribution. In the request body, include the XML document that you updated in Step 3. Then set the value of the HTTP <code>If-Match</code> header to the value of the <code>ETag</code> header that CloudFront returned when you submitted the <code>GET Streaming Distribution Config</code> request in Step 2.</p> </li> <li> <p>Review the response to the <code>PUT Streaming Distribution Config</code> request to confirm that the distribution was successfully disabled.</p> </li> <li> <p>Submit a <code>GET Streaming Distribution Config</code> request to confirm that your changes have propagated. When propagation is complete, the value of <code>Status</code> is <code>Deployed</code>.</p> </li> <li> <p>Submit a <code>DELETE Streaming Distribution</code> request. Set the value of the HTTP <code>If-Match</code> header to the value of the <code>ETag</code> header that CloudFront returned when you submitted the <code>GET Streaming Distribution Config</code> request in Step 2.</p> </li> <li> <p>Review the response to your <code>DELETE Streaming Distribution</code> request to confirm that the distribution was successfully deleted.</p> </li> </ol> <p>For information about deleting a distribution using the CloudFront console, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/HowToDeleteDistribution.html">Deleting a Distribution</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>

#### `getCloudFrontOriginAccessIdentity`

``` purescript
getCloudFrontOriginAccessIdentity :: forall eff. GetCloudFrontOriginAccessIdentityRequest -> Aff (err :: RequestError | eff) GetCloudFrontOriginAccessIdentityResult
```

<p>Get the information about an origin access identity. </p>

#### `getCloudFrontOriginAccessIdentityConfig`

``` purescript
getCloudFrontOriginAccessIdentityConfig :: forall eff. GetCloudFrontOriginAccessIdentityConfigRequest -> Aff (err :: RequestError | eff) GetCloudFrontOriginAccessIdentityConfigResult
```

<p>Get the configuration information about an origin access identity. </p>

#### `getDistribution`

``` purescript
getDistribution :: forall eff. GetDistributionRequest -> Aff (err :: RequestError | eff) GetDistributionResult
```

<p>Get the information about a distribution. </p>

#### `getDistributionConfig`

``` purescript
getDistributionConfig :: forall eff. GetDistributionConfigRequest -> Aff (err :: RequestError | eff) GetDistributionConfigResult
```

<p>Get the configuration information about a distribution. </p>

#### `getInvalidation`

``` purescript
getInvalidation :: forall eff. GetInvalidationRequest -> Aff (err :: RequestError | eff) GetInvalidationResult
```

<p>Get the information about an invalidation. </p>

#### `getStreamingDistribution`

``` purescript
getStreamingDistribution :: forall eff. GetStreamingDistributionRequest -> Aff (err :: RequestError | eff) GetStreamingDistributionResult
```

<p>Gets information about a specified RTMP distribution, including the distribution configuration.</p>

#### `getStreamingDistributionConfig`

``` purescript
getStreamingDistributionConfig :: forall eff. GetStreamingDistributionConfigRequest -> Aff (err :: RequestError | eff) GetStreamingDistributionConfigResult
```

<p>Get the configuration information about a streaming distribution. </p>

#### `listCloudFrontOriginAccessIdentities`

``` purescript
listCloudFrontOriginAccessIdentities :: forall eff. ListCloudFrontOriginAccessIdentitiesRequest -> Aff (err :: RequestError | eff) ListCloudFrontOriginAccessIdentitiesResult
```

<p>Lists origin access identities.</p>

#### `listDistributions`

``` purescript
listDistributions :: forall eff. ListDistributionsRequest -> Aff (err :: RequestError | eff) ListDistributionsResult
```

<p>List distributions. </p>

#### `listDistributionsByWebACLId`

``` purescript
listDistributionsByWebACLId :: forall eff. ListDistributionsByWebACLIdRequest -> Aff (err :: RequestError | eff) ListDistributionsByWebACLIdResult
```

<p>List the distributions that are associated with a specified AWS WAF web ACL. </p>

#### `listInvalidations`

``` purescript
listInvalidations :: forall eff. ListInvalidationsRequest -> Aff (err :: RequestError | eff) ListInvalidationsResult
```

<p>Lists invalidation batches. </p>

#### `listStreamingDistributions`

``` purescript
listStreamingDistributions :: forall eff. ListStreamingDistributionsRequest -> Aff (err :: RequestError | eff) ListStreamingDistributionsResult
```

<p>List streaming distributions. </p>

#### `listTagsForResource`

``` purescript
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: RequestError | eff) ListTagsForResourceResult
```

<p>List tags for a CloudFront resource.</p>

#### `tagResource`

``` purescript
tagResource :: forall eff. TagResourceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Add tags to a CloudFront resource.</p>

#### `untagResource`

``` purescript
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Remove tags from a CloudFront resource.</p>

#### `updateCloudFrontOriginAccessIdentity`

``` purescript
updateCloudFrontOriginAccessIdentity :: forall eff. UpdateCloudFrontOriginAccessIdentityRequest -> Aff (err :: RequestError | eff) UpdateCloudFrontOriginAccessIdentityResult
```

<p>Update an origin access identity. </p>

#### `updateDistribution`

``` purescript
updateDistribution :: forall eff. UpdateDistributionRequest -> Aff (err :: RequestError | eff) UpdateDistributionResult
```

<p>Update a distribution. </p>

#### `updateStreamingDistribution`

``` purescript
updateStreamingDistribution :: forall eff. UpdateStreamingDistributionRequest -> Aff (err :: RequestError | eff) UpdateStreamingDistributionResult
```

<p>Update a streaming distribution. </p>

#### `AccessDenied`

``` purescript
newtype AccessDenied
  = AccessDenied { "Message" :: NullOrUndefined (String) }
```

<p>Access denied.</p>

##### Instances
``` purescript
Newtype AccessDenied _
```

#### `ActiveTrustedSigners`

``` purescript
newtype ActiveTrustedSigners
  = ActiveTrustedSigners { "Enabled" :: Boolean, "Quantity" :: Int, "Items" :: NullOrUndefined (SignerList) }
```

<p>A complex type that lists the AWS accounts, if any, that you included in the <code>TrustedSigners</code> complex type for this distribution. These are the accounts that you want to allow to create signed URLs for private content.</p> <p>The <code>Signer</code> complex type lists the AWS account number of the trusted signer or <code>self</code> if the signer is the AWS account that created the distribution. The <code>Signer</code> element also includes the IDs of any active CloudFront key pairs that are associated with the trusted signer's AWS account. If no <code>KeyPairId</code> element appears for a <code>Signer</code>, that signer can't create signed URLs. </p> <p>For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html">Serving Private Content through CloudFront</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype ActiveTrustedSigners _
```

#### `AliasList`

``` purescript
newtype AliasList
  = AliasList (Array String)
```

##### Instances
``` purescript
Newtype AliasList _
```

#### `Aliases`

``` purescript
newtype Aliases
  = Aliases { "Quantity" :: Int, "Items" :: NullOrUndefined (AliasList) }
```

<p>A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution. </p>

##### Instances
``` purescript
Newtype Aliases _
```

#### `AllowedMethods`

``` purescript
newtype AllowedMethods
  = AllowedMethods { "Quantity" :: Int, "Items" :: MethodsList, "CachedMethods" :: NullOrUndefined (CachedMethods) }
```

<p>A complex type that controls which HTTP methods CloudFront processes and forwards to your Amazon S3 bucket or your custom origin. There are three choices:</p> <ul> <li> <p>CloudFront forwards only <code>GET</code> and <code>HEAD</code> requests.</p> </li> <li> <p>CloudFront forwards only <code>GET</code>, <code>HEAD</code>, and <code>OPTIONS</code> requests.</p> </li> <li> <p>CloudFront forwards <code>GET, HEAD, OPTIONS, PUT, PATCH, POST</code>, and <code>DELETE</code> requests.</p> </li> </ul> <p>If you pick the third choice, you may need to restrict access to your Amazon S3 bucket or to your custom origin so users can't perform operations that you don't want them to. For example, you might not want users to have permissions to delete objects from your origin.</p>

##### Instances
``` purescript
Newtype AllowedMethods _
```

#### `AwsAccountNumberList`

``` purescript
newtype AwsAccountNumberList
  = AwsAccountNumberList (Array String)
```

##### Instances
``` purescript
Newtype AwsAccountNumberList _
```

#### `BatchTooLarge`

``` purescript
newtype BatchTooLarge
  = BatchTooLarge { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype BatchTooLarge _
```

#### `CNAMEAlreadyExists`

``` purescript
newtype CNAMEAlreadyExists
  = CNAMEAlreadyExists { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CNAMEAlreadyExists _
```

#### `CacheBehavior`

``` purescript
newtype CacheBehavior
  = CacheBehavior { "PathPattern" :: String, "TargetOriginId" :: String, "ForwardedValues" :: ForwardedValues, "TrustedSigners" :: TrustedSigners, "ViewerProtocolPolicy" :: ViewerProtocolPolicy, "MinTTL" :: Number, "AllowedMethods" :: NullOrUndefined (AllowedMethods), "SmoothStreaming" :: NullOrUndefined (Boolean), "DefaultTTL" :: NullOrUndefined (Number), "MaxTTL" :: NullOrUndefined (Number), "Compress" :: NullOrUndefined (Boolean), "LambdaFunctionAssociations" :: NullOrUndefined (LambdaFunctionAssociations) }
```

<p>A complex type that describes how CloudFront processes requests.</p> <p>You must create at least as many cache behaviors (including the default cache behavior) as you have origins if you want CloudFront to distribute objects from all of the origins. Each cache behavior specifies the one origin from which you want CloudFront to get objects. If you have two origins and only the default cache behavior, the default cache behavior will cause CloudFront to get objects from one of the origins, but the other origin is never used.</p> <p>For the current limit on the number of cache behaviors that you can add to a distribution, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_cloudfront">Amazon CloudFront Limits</a> in the <i>AWS General Reference</i>.</p> <p>If you don't want to specify any cache behaviors, include only an empty <code>CacheBehaviors</code> element. Don't include an empty <code>CacheBehavior</code> element, or CloudFront returns a <code>MalformedXML</code> error.</p> <p>To delete all cache behaviors in an existing distribution, update the distribution configuration and include only an empty <code>CacheBehaviors</code> element.</p> <p>To add, change, or remove one or more cache behaviors, update the distribution configuration and specify all of the cache behaviors that you want to include in the updated distribution.</p> <p>For more information about cache behaviors, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior">Cache Behaviors</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype CacheBehavior _
```

#### `CacheBehaviorList`

``` purescript
newtype CacheBehaviorList
  = CacheBehaviorList (Array CacheBehavior)
```

##### Instances
``` purescript
Newtype CacheBehaviorList _
```

#### `CacheBehaviors`

``` purescript
newtype CacheBehaviors
  = CacheBehaviors { "Quantity" :: Int, "Items" :: NullOrUndefined (CacheBehaviorList) }
```

<p>A complex type that contains zero or more <code>CacheBehavior</code> elements. </p>

##### Instances
``` purescript
Newtype CacheBehaviors _
```

#### `CachedMethods`

``` purescript
newtype CachedMethods
  = CachedMethods { "Quantity" :: Int, "Items" :: MethodsList }
```

<p>A complex type that controls whether CloudFront caches the response to requests using the specified HTTP methods. There are two choices:</p> <ul> <li> <p>CloudFront caches responses to <code>GET</code> and <code>HEAD</code> requests.</p> </li> <li> <p>CloudFront caches responses to <code>GET</code>, <code>HEAD</code>, and <code>OPTIONS</code> requests.</p> </li> </ul> <p>If you pick the second choice for your Amazon S3 Origin, you may need to forward Access-Control-Request-Method, Access-Control-Request-Headers, and Origin headers for the responses to be cached correctly. </p>

##### Instances
``` purescript
Newtype CachedMethods _
```

#### `CertificateSource`

``` purescript
newtype CertificateSource
  = CertificateSource String
```

##### Instances
``` purescript
Newtype CertificateSource _
```

#### `CloudFrontOriginAccessIdentity`

``` purescript
newtype CloudFrontOriginAccessIdentity
  = CloudFrontOriginAccessIdentity { "Id" :: String, "S3CanonicalUserId" :: String, "CloudFrontOriginAccessIdentityConfig" :: NullOrUndefined (CloudFrontOriginAccessIdentityConfig) }
```

<p>CloudFront origin access identity.</p>

##### Instances
``` purescript
Newtype CloudFrontOriginAccessIdentity _
```

#### `CloudFrontOriginAccessIdentityAlreadyExists`

``` purescript
newtype CloudFrontOriginAccessIdentityAlreadyExists
  = CloudFrontOriginAccessIdentityAlreadyExists { "Message" :: NullOrUndefined (String) }
```

<p>If the <code>CallerReference</code> is a value you already sent in a previous request to create an identity but the content of the <code>CloudFrontOriginAccessIdentityConfig</code> is different from the original request, CloudFront returns a <code>CloudFrontOriginAccessIdentityAlreadyExists</code> error. </p>

##### Instances
``` purescript
Newtype CloudFrontOriginAccessIdentityAlreadyExists _
```

#### `CloudFrontOriginAccessIdentityConfig`

``` purescript
newtype CloudFrontOriginAccessIdentityConfig
  = CloudFrontOriginAccessIdentityConfig { "CallerReference" :: String, "Comment" :: String }
```

<p>Origin access identity configuration. Send a <code>GET</code> request to the <code>/<i>CloudFront API version</i>/CloudFront/identity ID/config</code> resource. </p>

##### Instances
``` purescript
Newtype CloudFrontOriginAccessIdentityConfig _
```

#### `CloudFrontOriginAccessIdentityInUse`

``` purescript
newtype CloudFrontOriginAccessIdentityInUse
  = CloudFrontOriginAccessIdentityInUse { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CloudFrontOriginAccessIdentityInUse _
```

#### `CloudFrontOriginAccessIdentityList`

``` purescript
newtype CloudFrontOriginAccessIdentityList
  = CloudFrontOriginAccessIdentityList { "Marker" :: String, "NextMarker" :: NullOrUndefined (String), "MaxItems" :: Int, "IsTruncated" :: Boolean, "Quantity" :: Int, "Items" :: NullOrUndefined (CloudFrontOriginAccessIdentitySummaryList) }
```

<p>Lists the origin access identities for CloudFront.Send a <code>GET</code> request to the <code>/<i>CloudFront API version</i>/origin-access-identity/cloudfront</code> resource. The response includes a <code>CloudFrontOriginAccessIdentityList</code> element with zero or more <code>CloudFrontOriginAccessIdentitySummary</code> child elements. By default, your entire list of origin access identities is returned in one single page. If the list is long, you can paginate it using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

##### Instances
``` purescript
Newtype CloudFrontOriginAccessIdentityList _
```

#### `CloudFrontOriginAccessIdentitySummary`

``` purescript
newtype CloudFrontOriginAccessIdentitySummary
  = CloudFrontOriginAccessIdentitySummary { "Id" :: String, "S3CanonicalUserId" :: String, "Comment" :: String }
```

<p>Summary of the information about a CloudFront origin access identity.</p>

##### Instances
``` purescript
Newtype CloudFrontOriginAccessIdentitySummary _
```

#### `CloudFrontOriginAccessIdentitySummaryList`

``` purescript
newtype CloudFrontOriginAccessIdentitySummaryList
  = CloudFrontOriginAccessIdentitySummaryList (Array CloudFrontOriginAccessIdentitySummary)
```

##### Instances
``` purescript
Newtype CloudFrontOriginAccessIdentitySummaryList _
```

#### `CookieNameList`

``` purescript
newtype CookieNameList
  = CookieNameList (Array String)
```

##### Instances
``` purescript
Newtype CookieNameList _
```

#### `CookieNames`

``` purescript
newtype CookieNames
  = CookieNames { "Quantity" :: Int, "Items" :: NullOrUndefined (CookieNameList) }
```

<p>A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html">How CloudFront Forwards, Caches, and Logs Cookies</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype CookieNames _
```

#### `CookiePreference`

``` purescript
newtype CookiePreference
  = CookiePreference { "Forward" :: ItemSelection, "WhitelistedNames" :: NullOrUndefined (CookieNames) }
```

<p>A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html">How CloudFront Forwards, Caches, and Logs Cookies</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype CookiePreference _
```

#### `CreateCloudFrontOriginAccessIdentityRequest`

``` purescript
newtype CreateCloudFrontOriginAccessIdentityRequest
  = CreateCloudFrontOriginAccessIdentityRequest { "CloudFrontOriginAccessIdentityConfig" :: CloudFrontOriginAccessIdentityConfig }
```

<p>The request to create a new origin access identity.</p>

##### Instances
``` purescript
Newtype CreateCloudFrontOriginAccessIdentityRequest _
```

#### `CreateCloudFrontOriginAccessIdentityResult`

``` purescript
newtype CreateCloudFrontOriginAccessIdentityResult
  = CreateCloudFrontOriginAccessIdentityResult { "CloudFrontOriginAccessIdentity" :: NullOrUndefined (CloudFrontOriginAccessIdentity), "Location" :: NullOrUndefined (String), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype CreateCloudFrontOriginAccessIdentityResult _
```

#### `CreateDistributionRequest`

``` purescript
newtype CreateDistributionRequest
  = CreateDistributionRequest { "DistributionConfig" :: DistributionConfig }
```

<p>The request to create a new distribution.</p>

##### Instances
``` purescript
Newtype CreateDistributionRequest _
```

#### `CreateDistributionResult`

``` purescript
newtype CreateDistributionResult
  = CreateDistributionResult { "Distribution" :: NullOrUndefined (Distribution), "Location" :: NullOrUndefined (String), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype CreateDistributionResult _
```

#### `CreateDistributionWithTagsRequest`

``` purescript
newtype CreateDistributionWithTagsRequest
  = CreateDistributionWithTagsRequest { "DistributionConfigWithTags" :: DistributionConfigWithTags }
```

<p>The request to create a new distribution with tags. </p>

##### Instances
``` purescript
Newtype CreateDistributionWithTagsRequest _
```

#### `CreateDistributionWithTagsResult`

``` purescript
newtype CreateDistributionWithTagsResult
  = CreateDistributionWithTagsResult { "Distribution" :: NullOrUndefined (Distribution), "Location" :: NullOrUndefined (String), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request. </p>

##### Instances
``` purescript
Newtype CreateDistributionWithTagsResult _
```

#### `CreateInvalidationRequest`

``` purescript
newtype CreateInvalidationRequest
  = CreateInvalidationRequest { "DistributionId" :: String, "InvalidationBatch" :: InvalidationBatch }
```

<p>The request to create an invalidation.</p>

##### Instances
``` purescript
Newtype CreateInvalidationRequest _
```

#### `CreateInvalidationResult`

``` purescript
newtype CreateInvalidationResult
  = CreateInvalidationResult { "Location" :: NullOrUndefined (String), "Invalidation" :: NullOrUndefined (Invalidation) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype CreateInvalidationResult _
```

#### `CreateStreamingDistributionRequest`

``` purescript
newtype CreateStreamingDistributionRequest
  = CreateStreamingDistributionRequest { "StreamingDistributionConfig" :: StreamingDistributionConfig }
```

<p>The request to create a new streaming distribution.</p>

##### Instances
``` purescript
Newtype CreateStreamingDistributionRequest _
```

#### `CreateStreamingDistributionResult`

``` purescript
newtype CreateStreamingDistributionResult
  = CreateStreamingDistributionResult { "StreamingDistribution" :: NullOrUndefined (StreamingDistribution), "Location" :: NullOrUndefined (String), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype CreateStreamingDistributionResult _
```

#### `CreateStreamingDistributionWithTagsRequest`

``` purescript
newtype CreateStreamingDistributionWithTagsRequest
  = CreateStreamingDistributionWithTagsRequest { "StreamingDistributionConfigWithTags" :: StreamingDistributionConfigWithTags }
```

<p>The request to create a new streaming distribution with tags.</p>

##### Instances
``` purescript
Newtype CreateStreamingDistributionWithTagsRequest _
```

#### `CreateStreamingDistributionWithTagsResult`

``` purescript
newtype CreateStreamingDistributionWithTagsResult
  = CreateStreamingDistributionWithTagsResult { "StreamingDistribution" :: NullOrUndefined (StreamingDistribution), "Location" :: NullOrUndefined (String), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request. </p>

##### Instances
``` purescript
Newtype CreateStreamingDistributionWithTagsResult _
```

#### `CustomErrorResponse`

``` purescript
newtype CustomErrorResponse
  = CustomErrorResponse { "ErrorCode" :: Int, "ResponsePagePath" :: NullOrUndefined (String), "ResponseCode" :: NullOrUndefined (String), "ErrorCachingMinTTL" :: NullOrUndefined (Number) }
```

<p>A complex type that controls:</p> <ul> <li> <p>Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer. </p> </li> <li> <p>How long CloudFront caches HTTP status codes in the 4xx and 5xx range.</p> </li> </ul> <p>For more information about custom error pages, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html">Customizing Error Responses</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype CustomErrorResponse _
```

#### `CustomErrorResponseList`

``` purescript
newtype CustomErrorResponseList
  = CustomErrorResponseList (Array CustomErrorResponse)
```

##### Instances
``` purescript
Newtype CustomErrorResponseList _
```

#### `CustomErrorResponses`

``` purescript
newtype CustomErrorResponses
  = CustomErrorResponses { "Quantity" :: Int, "Items" :: NullOrUndefined (CustomErrorResponseList) }
```

<p>A complex type that controls:</p> <ul> <li> <p>Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.</p> </li> <li> <p>How long CloudFront caches HTTP status codes in the 4xx and 5xx range.</p> </li> </ul> <p>For more information about custom error pages, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html">Customizing Error Responses</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype CustomErrorResponses _
```

#### `CustomHeaders`

``` purescript
newtype CustomHeaders
  = CustomHeaders { "Quantity" :: Int, "Items" :: NullOrUndefined (OriginCustomHeadersList) }
```

<p>A complex type that contains the list of Custom Headers for each origin. </p>

##### Instances
``` purescript
Newtype CustomHeaders _
```

#### `CustomOriginConfig`

``` purescript
newtype CustomOriginConfig
  = CustomOriginConfig { "HTTPPort" :: Int, "HTTPSPort" :: Int, "OriginProtocolPolicy" :: OriginProtocolPolicy, "OriginSslProtocols" :: NullOrUndefined (OriginSslProtocols) }
```

<p>A customer origin.</p>

##### Instances
``` purescript
Newtype CustomOriginConfig _
```

#### `DefaultCacheBehavior`

``` purescript
newtype DefaultCacheBehavior
  = DefaultCacheBehavior { "TargetOriginId" :: String, "ForwardedValues" :: ForwardedValues, "TrustedSigners" :: TrustedSigners, "ViewerProtocolPolicy" :: ViewerProtocolPolicy, "MinTTL" :: Number, "AllowedMethods" :: NullOrUndefined (AllowedMethods), "SmoothStreaming" :: NullOrUndefined (Boolean), "DefaultTTL" :: NullOrUndefined (Number), "MaxTTL" :: NullOrUndefined (Number), "Compress" :: NullOrUndefined (Boolean), "LambdaFunctionAssociations" :: NullOrUndefined (LambdaFunctionAssociations) }
```

<p>A complex type that describes the default cache behavior if you do not specify a <code>CacheBehavior</code> element or if files don't match any of the values of <code>PathPattern</code> in <code>CacheBehavior</code> elements. You must create exactly one default cache behavior.</p>

##### Instances
``` purescript
Newtype DefaultCacheBehavior _
```

#### `DeleteCloudFrontOriginAccessIdentityRequest`

``` purescript
newtype DeleteCloudFrontOriginAccessIdentityRequest
  = DeleteCloudFrontOriginAccessIdentityRequest { "Id" :: String, "IfMatch" :: NullOrUndefined (String) }
```

<p>Deletes a origin access identity.</p>

##### Instances
``` purescript
Newtype DeleteCloudFrontOriginAccessIdentityRequest _
```

#### `DeleteDistributionRequest`

``` purescript
newtype DeleteDistributionRequest
  = DeleteDistributionRequest { "Id" :: String, "IfMatch" :: NullOrUndefined (String) }
```

<p>This action deletes a web distribution. To delete a web distribution using the CloudFront API, perform the following steps.</p> <p> <b>To delete a web distribution using the CloudFront API:</b> </p> <ol> <li> <p>Disable the web distribution </p> </li> <li> <p>Submit a <code>GET Distribution Config</code> request to get the current configuration and the <code>Etag</code> header for the distribution.</p> </li> <li> <p>Update the XML document that was returned in the response to your <code>GET Distribution Config</code> request to change the value of <code>Enabled</code> to <code>false</code>.</p> </li> <li> <p>Submit a <code>PUT Distribution Config</code> request to update the configuration for your distribution. In the request body, include the XML document that you updated in Step 3. Set the value of the HTTP <code>If-Match</code> header to the value of the <code>ETag</code> header that CloudFront returned when you submitted the <code>GET Distribution Config</code> request in Step 2.</p> </li> <li> <p>Review the response to the <code>PUT Distribution Config</code> request to confirm that the distribution was successfully disabled.</p> </li> <li> <p>Submit a <code>GET Distribution</code> request to confirm that your changes have propagated. When propagation is complete, the value of <code>Status</code> is <code>Deployed</code>.</p> </li> <li> <p>Submit a <code>DELETE Distribution</code> request. Set the value of the HTTP <code>If-Match</code> header to the value of the <code>ETag</code> header that CloudFront returned when you submitted the <code>GET Distribution Config</code> request in Step 6.</p> </li> <li> <p>Review the response to your <code>DELETE Distribution</code> request to confirm that the distribution was successfully deleted.</p> </li> </ol> <p>For information about deleting a distribution using the CloudFront console, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/HowToDeleteDistribution.html">Deleting a Distribution</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype DeleteDistributionRequest _
```

#### `DeleteStreamingDistributionRequest`

``` purescript
newtype DeleteStreamingDistributionRequest
  = DeleteStreamingDistributionRequest { "Id" :: String, "IfMatch" :: NullOrUndefined (String) }
```

<p>The request to delete a streaming distribution.</p>

##### Instances
``` purescript
Newtype DeleteStreamingDistributionRequest _
```

#### `Distribution`

``` purescript
newtype Distribution
  = Distribution { "Id" :: String, "ARN" :: String, "Status" :: String, "LastModifiedTime" :: Number, "InProgressInvalidationBatches" :: Int, "DomainName" :: String, "ActiveTrustedSigners" :: ActiveTrustedSigners, "DistributionConfig" :: DistributionConfig }
```

<p>The distribution's information.</p>

##### Instances
``` purescript
Newtype Distribution _
```

#### `DistributionAlreadyExists`

``` purescript
newtype DistributionAlreadyExists
  = DistributionAlreadyExists { "Message" :: NullOrUndefined (String) }
```

<p>The caller reference you attempted to create the distribution with is associated with another distribution.</p>

##### Instances
``` purescript
Newtype DistributionAlreadyExists _
```

#### `DistributionConfig`

``` purescript
newtype DistributionConfig
  = DistributionConfig { "CallerReference" :: String, "Aliases" :: NullOrUndefined (Aliases), "DefaultRootObject" :: NullOrUndefined (String), "Origins" :: Origins, "DefaultCacheBehavior" :: DefaultCacheBehavior, "CacheBehaviors" :: NullOrUndefined (CacheBehaviors), "CustomErrorResponses" :: NullOrUndefined (CustomErrorResponses), "Comment" :: String, "Logging" :: NullOrUndefined (LoggingConfig), "PriceClass" :: NullOrUndefined (PriceClass), "Enabled" :: Boolean, "ViewerCertificate" :: NullOrUndefined (ViewerCertificate), "Restrictions" :: NullOrUndefined (Restrictions), "WebACLId" :: NullOrUndefined (String), "HttpVersion" :: NullOrUndefined (HttpVersion), "IsIPV6Enabled" :: NullOrUndefined (Boolean) }
```

<p>A distribution configuration.</p>

##### Instances
``` purescript
Newtype DistributionConfig _
```

#### `DistributionConfigWithTags`

``` purescript
newtype DistributionConfigWithTags
  = DistributionConfigWithTags { "DistributionConfig" :: DistributionConfig, "Tags" :: Tags }
```

<p>A distribution Configuration and a list of tags to be associated with the distribution.</p>

##### Instances
``` purescript
Newtype DistributionConfigWithTags _
```

#### `DistributionList`

``` purescript
newtype DistributionList
  = DistributionList { "Marker" :: String, "NextMarker" :: NullOrUndefined (String), "MaxItems" :: Int, "IsTruncated" :: Boolean, "Quantity" :: Int, "Items" :: NullOrUndefined (DistributionSummaryList) }
```

<p>A distribution list.</p>

##### Instances
``` purescript
Newtype DistributionList _
```

#### `DistributionNotDisabled`

``` purescript
newtype DistributionNotDisabled
  = DistributionNotDisabled { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DistributionNotDisabled _
```

#### `DistributionSummary`

``` purescript
newtype DistributionSummary
  = DistributionSummary { "Id" :: String, "ARN" :: String, "Status" :: String, "LastModifiedTime" :: Number, "DomainName" :: String, "Aliases" :: Aliases, "Origins" :: Origins, "DefaultCacheBehavior" :: DefaultCacheBehavior, "CacheBehaviors" :: CacheBehaviors, "CustomErrorResponses" :: CustomErrorResponses, "Comment" :: String, "PriceClass" :: PriceClass, "Enabled" :: Boolean, "ViewerCertificate" :: ViewerCertificate, "Restrictions" :: Restrictions, "WebACLId" :: String, "HttpVersion" :: HttpVersion, "IsIPV6Enabled" :: Boolean }
```

<p>A summary of the information about a CloudFront distribution.</p>

##### Instances
``` purescript
Newtype DistributionSummary _
```

#### `DistributionSummaryList`

``` purescript
newtype DistributionSummaryList
  = DistributionSummaryList (Array DistributionSummary)
```

##### Instances
``` purescript
Newtype DistributionSummaryList _
```

#### `EventType`

``` purescript
newtype EventType
  = EventType String
```

##### Instances
``` purescript
Newtype EventType _
```

#### `ForwardedValues`

``` purescript
newtype ForwardedValues
  = ForwardedValues { "QueryString" :: Boolean, "Cookies" :: CookiePreference, "Headers" :: NullOrUndefined (Headers), "QueryStringCacheKeys" :: NullOrUndefined (QueryStringCacheKeys) }
```

<p>A complex type that specifies how CloudFront handles query strings and cookies.</p>

##### Instances
``` purescript
Newtype ForwardedValues _
```

#### `GeoRestriction`

``` purescript
newtype GeoRestriction
  = GeoRestriction { "RestrictionType" :: GeoRestrictionType, "Quantity" :: Int, "Items" :: NullOrUndefined (LocationList) }
```

<p>A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using <code>MaxMind</code> GeoIP databases. </p>

##### Instances
``` purescript
Newtype GeoRestriction _
```

#### `GeoRestrictionType`

``` purescript
newtype GeoRestrictionType
  = GeoRestrictionType String
```

##### Instances
``` purescript
Newtype GeoRestrictionType _
```

#### `GetCloudFrontOriginAccessIdentityConfigRequest`

``` purescript
newtype GetCloudFrontOriginAccessIdentityConfigRequest
  = GetCloudFrontOriginAccessIdentityConfigRequest { "Id" :: String }
```

<p>The origin access identity's configuration information. For more information, see <a>CloudFrontOriginAccessIdentityConfigComplexType</a>.</p>

##### Instances
``` purescript
Newtype GetCloudFrontOriginAccessIdentityConfigRequest _
```

#### `GetCloudFrontOriginAccessIdentityConfigResult`

``` purescript
newtype GetCloudFrontOriginAccessIdentityConfigResult
  = GetCloudFrontOriginAccessIdentityConfigResult { "CloudFrontOriginAccessIdentityConfig" :: NullOrUndefined (CloudFrontOriginAccessIdentityConfig), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype GetCloudFrontOriginAccessIdentityConfigResult _
```

#### `GetCloudFrontOriginAccessIdentityRequest`

``` purescript
newtype GetCloudFrontOriginAccessIdentityRequest
  = GetCloudFrontOriginAccessIdentityRequest { "Id" :: String }
```

<p>The request to get an origin access identity's information.</p>

##### Instances
``` purescript
Newtype GetCloudFrontOriginAccessIdentityRequest _
```

#### `GetCloudFrontOriginAccessIdentityResult`

``` purescript
newtype GetCloudFrontOriginAccessIdentityResult
  = GetCloudFrontOriginAccessIdentityResult { "CloudFrontOriginAccessIdentity" :: NullOrUndefined (CloudFrontOriginAccessIdentity), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype GetCloudFrontOriginAccessIdentityResult _
```

#### `GetDistributionConfigRequest`

``` purescript
newtype GetDistributionConfigRequest
  = GetDistributionConfigRequest { "Id" :: String }
```

<p>The request to get a distribution configuration.</p>

##### Instances
``` purescript
Newtype GetDistributionConfigRequest _
```

#### `GetDistributionConfigResult`

``` purescript
newtype GetDistributionConfigResult
  = GetDistributionConfigResult { "DistributionConfig" :: NullOrUndefined (DistributionConfig), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype GetDistributionConfigResult _
```

#### `GetDistributionRequest`

``` purescript
newtype GetDistributionRequest
  = GetDistributionRequest { "Id" :: String }
```

<p>The request to get a distribution's information.</p>

##### Instances
``` purescript
Newtype GetDistributionRequest _
```

#### `GetDistributionResult`

``` purescript
newtype GetDistributionResult
  = GetDistributionResult { "Distribution" :: NullOrUndefined (Distribution), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype GetDistributionResult _
```

#### `GetInvalidationRequest`

``` purescript
newtype GetInvalidationRequest
  = GetInvalidationRequest { "DistributionId" :: String, "Id" :: String }
```

<p>The request to get an invalidation's information. </p>

##### Instances
``` purescript
Newtype GetInvalidationRequest _
```

#### `GetInvalidationResult`

``` purescript
newtype GetInvalidationResult
  = GetInvalidationResult { "Invalidation" :: NullOrUndefined (Invalidation) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype GetInvalidationResult _
```

#### `GetStreamingDistributionConfigRequest`

``` purescript
newtype GetStreamingDistributionConfigRequest
  = GetStreamingDistributionConfigRequest { "Id" :: String }
```

<p>To request to get a streaming distribution configuration.</p>

##### Instances
``` purescript
Newtype GetStreamingDistributionConfigRequest _
```

#### `GetStreamingDistributionConfigResult`

``` purescript
newtype GetStreamingDistributionConfigResult
  = GetStreamingDistributionConfigResult { "StreamingDistributionConfig" :: NullOrUndefined (StreamingDistributionConfig), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype GetStreamingDistributionConfigResult _
```

#### `GetStreamingDistributionRequest`

``` purescript
newtype GetStreamingDistributionRequest
  = GetStreamingDistributionRequest { "Id" :: String }
```

<p>The request to get a streaming distribution's information.</p>

##### Instances
``` purescript
Newtype GetStreamingDistributionRequest _
```

#### `GetStreamingDistributionResult`

``` purescript
newtype GetStreamingDistributionResult
  = GetStreamingDistributionResult { "StreamingDistribution" :: NullOrUndefined (StreamingDistribution), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype GetStreamingDistributionResult _
```

#### `HeaderList`

``` purescript
newtype HeaderList
  = HeaderList (Array String)
```

##### Instances
``` purescript
Newtype HeaderList _
```

#### `Headers`

``` purescript
newtype Headers
  = Headers { "Quantity" :: Int, "Items" :: NullOrUndefined (HeaderList) }
```

<p>A complex type that specifies the headers that you want CloudFront to forward to the origin for this cache behavior.</p> <p>For the headers that you specify, CloudFront also caches separate versions of a specified object based on the header values in viewer requests. For example, suppose viewer requests for <code>logo.jpg</code> contain a custom <code>Product</code> header that has a value of either <code>Acme</code> or <code>Apex</code>, and you configure CloudFront to cache your content based on values in the <code>Product</code> header. CloudFront forwards the <code>Product</code> header to the origin and caches the response from the origin once for each header value. For more information about caching based on header values, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html">How CloudFront Forwards and Caches Headers</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype Headers _
```

#### `HttpVersion`

``` purescript
newtype HttpVersion
  = HttpVersion String
```

##### Instances
``` purescript
Newtype HttpVersion _
```

#### `IllegalUpdate`

``` purescript
newtype IllegalUpdate
  = IllegalUpdate { "Message" :: NullOrUndefined (String) }
```

<p>Origin and <code>CallerReference</code> cannot be updated. </p>

##### Instances
``` purescript
Newtype IllegalUpdate _
```

#### `InconsistentQuantities`

``` purescript
newtype InconsistentQuantities
  = InconsistentQuantities { "Message" :: NullOrUndefined (String) }
```

<p>The value of <code>Quantity</code> and the size of <code>Items</code> do not match.</p>

##### Instances
``` purescript
Newtype InconsistentQuantities _
```

#### `InvalidArgument`

``` purescript
newtype InvalidArgument
  = InvalidArgument { "Message" :: NullOrUndefined (String) }
```

<p>The argument is invalid.</p>

##### Instances
``` purescript
Newtype InvalidArgument _
```

#### `InvalidDefaultRootObject`

``` purescript
newtype InvalidDefaultRootObject
  = InvalidDefaultRootObject { "Message" :: NullOrUndefined (String) }
```

<p>The default root object file name is too big or contains an invalid character.</p>

##### Instances
``` purescript
Newtype InvalidDefaultRootObject _
```

#### `InvalidErrorCode`

``` purescript
newtype InvalidErrorCode
  = InvalidErrorCode { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype InvalidErrorCode _
```

#### `InvalidForwardCookies`

``` purescript
newtype InvalidForwardCookies
  = InvalidForwardCookies { "Message" :: NullOrUndefined (String) }
```

<p>Your request contains forward cookies option which doesn't match with the expectation for the <code>whitelisted</code> list of cookie names. Either list of cookie names has been specified when not allowed or list of cookie names is missing when expected.</p>

##### Instances
``` purescript
Newtype InvalidForwardCookies _
```

#### `InvalidGeoRestrictionParameter`

``` purescript
newtype InvalidGeoRestrictionParameter
  = InvalidGeoRestrictionParameter { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype InvalidGeoRestrictionParameter _
```

#### `InvalidHeadersForS3Origin`

``` purescript
newtype InvalidHeadersForS3Origin
  = InvalidHeadersForS3Origin { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype InvalidHeadersForS3Origin _
```

#### `InvalidIfMatchVersion`

``` purescript
newtype InvalidIfMatchVersion
  = InvalidIfMatchVersion { "Message" :: NullOrUndefined (String) }
```

<p>The <code>If-Match</code> version is missing or not valid for the distribution.</p>

##### Instances
``` purescript
Newtype InvalidIfMatchVersion _
```

#### `InvalidLambdaFunctionAssociation`

``` purescript
newtype InvalidLambdaFunctionAssociation
  = InvalidLambdaFunctionAssociation { "Message" :: NullOrUndefined (String) }
```

<p>The specified Lambda function association is invalid.</p>

##### Instances
``` purescript
Newtype InvalidLambdaFunctionAssociation _
```

#### `InvalidLocationCode`

``` purescript
newtype InvalidLocationCode
  = InvalidLocationCode { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype InvalidLocationCode _
```

#### `InvalidMinimumProtocolVersion`

``` purescript
newtype InvalidMinimumProtocolVersion
  = InvalidMinimumProtocolVersion { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype InvalidMinimumProtocolVersion _
```

#### `InvalidOrigin`

``` purescript
newtype InvalidOrigin
  = InvalidOrigin { "Message" :: NullOrUndefined (String) }
```

<p>The Amazon S3 origin server specified does not refer to a valid Amazon S3 bucket.</p>

##### Instances
``` purescript
Newtype InvalidOrigin _
```

#### `InvalidOriginAccessIdentity`

``` purescript
newtype InvalidOriginAccessIdentity
  = InvalidOriginAccessIdentity { "Message" :: NullOrUndefined (String) }
```

<p>The origin access identity is not valid or doesn't exist.</p>

##### Instances
``` purescript
Newtype InvalidOriginAccessIdentity _
```

#### `InvalidProtocolSettings`

``` purescript
newtype InvalidProtocolSettings
  = InvalidProtocolSettings { "Message" :: NullOrUndefined (String) }
```

<p>You cannot specify SSLv3 as the minimum protocol version if you only want to support only clients that support Server Name Indication (SNI).</p>

##### Instances
``` purescript
Newtype InvalidProtocolSettings _
```

#### `InvalidQueryStringParameters`

``` purescript
newtype InvalidQueryStringParameters
  = InvalidQueryStringParameters { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype InvalidQueryStringParameters _
```

#### `InvalidRelativePath`

``` purescript
newtype InvalidRelativePath
  = InvalidRelativePath { "Message" :: NullOrUndefined (String) }
```

<p>The relative path is too big, is not URL-encoded, or does not begin with a slash (/).</p>

##### Instances
``` purescript
Newtype InvalidRelativePath _
```

#### `InvalidRequiredProtocol`

``` purescript
newtype InvalidRequiredProtocol
  = InvalidRequiredProtocol { "Message" :: NullOrUndefined (String) }
```

<p>This operation requires the HTTPS protocol. Ensure that you specify the HTTPS protocol in your request, or omit the <code>RequiredProtocols</code> element from your distribution configuration.</p>

##### Instances
``` purescript
Newtype InvalidRequiredProtocol _
```

#### `InvalidResponseCode`

``` purescript
newtype InvalidResponseCode
  = InvalidResponseCode { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype InvalidResponseCode _
```

#### `InvalidTTLOrder`

``` purescript
newtype InvalidTTLOrder
  = InvalidTTLOrder { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype InvalidTTLOrder _
```

#### `InvalidTagging`

``` purescript
newtype InvalidTagging
  = InvalidTagging { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype InvalidTagging _
```

#### `InvalidViewerCertificate`

``` purescript
newtype InvalidViewerCertificate
  = InvalidViewerCertificate { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype InvalidViewerCertificate _
```

#### `InvalidWebACLId`

``` purescript
newtype InvalidWebACLId
  = InvalidWebACLId { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype InvalidWebACLId _
```

#### `Invalidation`

``` purescript
newtype Invalidation
  = Invalidation { "Id" :: String, "Status" :: String, "CreateTime" :: Number, "InvalidationBatch" :: InvalidationBatch }
```

<p>An invalidation. </p>

##### Instances
``` purescript
Newtype Invalidation _
```

#### `InvalidationBatch`

``` purescript
newtype InvalidationBatch
  = InvalidationBatch { "Paths" :: Paths, "CallerReference" :: String }
```

<p>An invalidation batch.</p>

##### Instances
``` purescript
Newtype InvalidationBatch _
```

#### `InvalidationList`

``` purescript
newtype InvalidationList
  = InvalidationList { "Marker" :: String, "NextMarker" :: NullOrUndefined (String), "MaxItems" :: Int, "IsTruncated" :: Boolean, "Quantity" :: Int, "Items" :: NullOrUndefined (InvalidationSummaryList) }
```

<p>The <code>InvalidationList</code> complex type describes the list of invalidation objects. For more information about invalidation, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html">Invalidating Objects (Web Distributions Only)</a> in the <i>Amazon CloudFront Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype InvalidationList _
```

#### `InvalidationSummary`

``` purescript
newtype InvalidationSummary
  = InvalidationSummary { "Id" :: String, "CreateTime" :: Number, "Status" :: String }
```

<p>A summary of an invalidation request.</p>

##### Instances
``` purescript
Newtype InvalidationSummary _
```

#### `InvalidationSummaryList`

``` purescript
newtype InvalidationSummaryList
  = InvalidationSummaryList (Array InvalidationSummary)
```

##### Instances
``` purescript
Newtype InvalidationSummaryList _
```

#### `ItemSelection`

``` purescript
newtype ItemSelection
  = ItemSelection String
```

##### Instances
``` purescript
Newtype ItemSelection _
```

#### `KeyPairIdList`

``` purescript
newtype KeyPairIdList
  = KeyPairIdList (Array String)
```

##### Instances
``` purescript
Newtype KeyPairIdList _
```

#### `KeyPairIds`

``` purescript
newtype KeyPairIds
  = KeyPairIds { "Quantity" :: Int, "Items" :: NullOrUndefined (KeyPairIdList) }
```

<p>A complex type that lists the active CloudFront key pairs, if any, that are associated with <code>AwsAccountNumber</code>. </p> <p>For more information, see <a>ActiveTrustedSigners</a>.</p>

##### Instances
``` purescript
Newtype KeyPairIds _
```

#### `LambdaFunctionAssociation`

``` purescript
newtype LambdaFunctionAssociation
  = LambdaFunctionAssociation { "LambdaFunctionARN" :: NullOrUndefined (String), "EventType" :: NullOrUndefined (EventType) }
```

<p>A complex type that contains a Lambda function association.</p>

##### Instances
``` purescript
Newtype LambdaFunctionAssociation _
```

#### `LambdaFunctionAssociationList`

``` purescript
newtype LambdaFunctionAssociationList
  = LambdaFunctionAssociationList (Array LambdaFunctionAssociation)
```

##### Instances
``` purescript
Newtype LambdaFunctionAssociationList _
```

#### `LambdaFunctionAssociations`

``` purescript
newtype LambdaFunctionAssociations
  = LambdaFunctionAssociations { "Quantity" :: Int, "Items" :: NullOrUndefined (LambdaFunctionAssociationList) }
```

<p>A complex type that specifies a list of Lambda functions associations for a cache behavior.</p> <p>If you want to invoke one or more Lambda functions triggered by requests that match the <code>PathPattern</code> of the cache behavior, specify the applicable values for <code>Quantity</code> and <code>Items</code>. Note that there can be up to 4 <code>LambdaFunctionAssociation</code> items in this list (one for each possible value of <code>EventType</code>) and each <code>EventType</code> can be associated with the Lambda function only once.</p> <p>If you don't want to invoke any Lambda functions for the requests that match <code>PathPattern</code>, specify <code>0</code> for <code>Quantity</code> and omit <code>Items</code>. </p>

##### Instances
``` purescript
Newtype LambdaFunctionAssociations _
```

#### `ListCloudFrontOriginAccessIdentitiesRequest`

``` purescript
newtype ListCloudFrontOriginAccessIdentitiesRequest
  = ListCloudFrontOriginAccessIdentitiesRequest { "Marker" :: NullOrUndefined (String), "MaxItems" :: NullOrUndefined (String) }
```

<p>The request to list origin access identities. </p>

##### Instances
``` purescript
Newtype ListCloudFrontOriginAccessIdentitiesRequest _
```

#### `ListCloudFrontOriginAccessIdentitiesResult`

``` purescript
newtype ListCloudFrontOriginAccessIdentitiesResult
  = ListCloudFrontOriginAccessIdentitiesResult { "CloudFrontOriginAccessIdentityList" :: NullOrUndefined (CloudFrontOriginAccessIdentityList) }
```

<p>The returned result of the corresponding request. </p>

##### Instances
``` purescript
Newtype ListCloudFrontOriginAccessIdentitiesResult _
```

#### `ListDistributionsByWebACLIdRequest`

``` purescript
newtype ListDistributionsByWebACLIdRequest
  = ListDistributionsByWebACLIdRequest { "Marker" :: NullOrUndefined (String), "MaxItems" :: NullOrUndefined (String), "WebACLId" :: String }
```

<p>The request to list distributions that are associated with a specified AWS WAF web ACL. </p>

##### Instances
``` purescript
Newtype ListDistributionsByWebACLIdRequest _
```

#### `ListDistributionsByWebACLIdResult`

``` purescript
newtype ListDistributionsByWebACLIdResult
  = ListDistributionsByWebACLIdResult { "DistributionList" :: NullOrUndefined (DistributionList) }
```

<p>The response to a request to list the distributions that are associated with a specified AWS WAF web ACL. </p>

##### Instances
``` purescript
Newtype ListDistributionsByWebACLIdResult _
```

#### `ListDistributionsRequest`

``` purescript
newtype ListDistributionsRequest
  = ListDistributionsRequest { "Marker" :: NullOrUndefined (String), "MaxItems" :: NullOrUndefined (String) }
```

<p>The request to list your distributions. </p>

##### Instances
``` purescript
Newtype ListDistributionsRequest _
```

#### `ListDistributionsResult`

``` purescript
newtype ListDistributionsResult
  = ListDistributionsResult { "DistributionList" :: NullOrUndefined (DistributionList) }
```

<p>The returned result of the corresponding request. </p>

##### Instances
``` purescript
Newtype ListDistributionsResult _
```

#### `ListInvalidationsRequest`

``` purescript
newtype ListInvalidationsRequest
  = ListInvalidationsRequest { "DistributionId" :: String, "Marker" :: NullOrUndefined (String), "MaxItems" :: NullOrUndefined (String) }
```

<p>The request to list invalidations. </p>

##### Instances
``` purescript
Newtype ListInvalidationsRequest _
```

#### `ListInvalidationsResult`

``` purescript
newtype ListInvalidationsResult
  = ListInvalidationsResult { "InvalidationList" :: NullOrUndefined (InvalidationList) }
```

<p>The returned result of the corresponding request. </p>

##### Instances
``` purescript
Newtype ListInvalidationsResult _
```

#### `ListStreamingDistributionsRequest`

``` purescript
newtype ListStreamingDistributionsRequest
  = ListStreamingDistributionsRequest { "Marker" :: NullOrUndefined (String), "MaxItems" :: NullOrUndefined (String) }
```

<p>The request to list your streaming distributions. </p>

##### Instances
``` purescript
Newtype ListStreamingDistributionsRequest _
```

#### `ListStreamingDistributionsResult`

``` purescript
newtype ListStreamingDistributionsResult
  = ListStreamingDistributionsResult { "StreamingDistributionList" :: NullOrUndefined (StreamingDistributionList) }
```

<p>The returned result of the corresponding request. </p>

##### Instances
``` purescript
Newtype ListStreamingDistributionsResult _
```

#### `ListTagsForResourceRequest`

``` purescript
newtype ListTagsForResourceRequest
  = ListTagsForResourceRequest { "Resource" :: ResourceARN }
```

<p> The request to list tags for a CloudFront resource.</p>

##### Instances
``` purescript
Newtype ListTagsForResourceRequest _
```

#### `ListTagsForResourceResult`

``` purescript
newtype ListTagsForResourceResult
  = ListTagsForResourceResult { "Tags" :: Tags }
```

<p> The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype ListTagsForResourceResult _
```

#### `LocationList`

``` purescript
newtype LocationList
  = LocationList (Array String)
```

##### Instances
``` purescript
Newtype LocationList _
```

#### `LoggingConfig`

``` purescript
newtype LoggingConfig
  = LoggingConfig { "Enabled" :: Boolean, "IncludeCookies" :: Boolean, "Bucket" :: String, "Prefix" :: String }
```

<p>A complex type that controls whether access logs are written for the distribution.</p>

##### Instances
``` purescript
Newtype LoggingConfig _
```

#### `Method`

``` purescript
newtype Method
  = Method String
```

##### Instances
``` purescript
Newtype Method _
```

#### `MethodsList`

``` purescript
newtype MethodsList
  = MethodsList (Array Method)
```

##### Instances
``` purescript
Newtype MethodsList _
```

#### `MinimumProtocolVersion`

``` purescript
newtype MinimumProtocolVersion
  = MinimumProtocolVersion String
```

##### Instances
``` purescript
Newtype MinimumProtocolVersion _
```

#### `MissingBody`

``` purescript
newtype MissingBody
  = MissingBody { "Message" :: NullOrUndefined (String) }
```

<p>This operation requires a body. Ensure that the body is present and the Content-Type header is set.</p>

##### Instances
``` purescript
Newtype MissingBody _
```

#### `NoSuchCloudFrontOriginAccessIdentity`

``` purescript
newtype NoSuchCloudFrontOriginAccessIdentity
  = NoSuchCloudFrontOriginAccessIdentity { "Message" :: NullOrUndefined (String) }
```

<p>The specified origin access identity does not exist.</p>

##### Instances
``` purescript
Newtype NoSuchCloudFrontOriginAccessIdentity _
```

#### `NoSuchDistribution`

``` purescript
newtype NoSuchDistribution
  = NoSuchDistribution { "Message" :: NullOrUndefined (String) }
```

<p>The specified distribution does not exist.</p>

##### Instances
``` purescript
Newtype NoSuchDistribution _
```

#### `NoSuchInvalidation`

``` purescript
newtype NoSuchInvalidation
  = NoSuchInvalidation { "Message" :: NullOrUndefined (String) }
```

<p>The specified invalidation does not exist.</p>

##### Instances
``` purescript
Newtype NoSuchInvalidation _
```

#### `NoSuchOrigin`

``` purescript
newtype NoSuchOrigin
  = NoSuchOrigin { "Message" :: NullOrUndefined (String) }
```

<p>No origin exists with the specified <code>Origin Id</code>. </p>

##### Instances
``` purescript
Newtype NoSuchOrigin _
```

#### `NoSuchResource`

``` purescript
newtype NoSuchResource
  = NoSuchResource { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype NoSuchResource _
```

#### `NoSuchStreamingDistribution`

``` purescript
newtype NoSuchStreamingDistribution
  = NoSuchStreamingDistribution { "Message" :: NullOrUndefined (String) }
```

<p>The specified streaming distribution does not exist.</p>

##### Instances
``` purescript
Newtype NoSuchStreamingDistribution _
```

#### `Origin`

``` purescript
newtype Origin
  = Origin { "Id" :: String, "DomainName" :: String, "OriginPath" :: NullOrUndefined (String), "CustomHeaders" :: NullOrUndefined (CustomHeaders), "S3OriginConfig" :: NullOrUndefined (S3OriginConfig), "CustomOriginConfig" :: NullOrUndefined (CustomOriginConfig) }
```

<p>A complex type that describes the Amazon S3 bucket or the HTTP server (for example, a web server) from which CloudFront gets your files. You must create at least one origin.</p> <p>For the current limit on the number of origins that you can create for a distribution, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_cloudfront">Amazon CloudFront Limits</a> in the <i>AWS General Reference</i>.</p>

##### Instances
``` purescript
Newtype Origin _
```

#### `OriginCustomHeader`

``` purescript
newtype OriginCustomHeader
  = OriginCustomHeader { "HeaderName" :: String, "HeaderValue" :: String }
```

<p>A complex type that contains <code>HeaderName</code> and <code>HeaderValue</code> elements, if any, for this distribution. </p>

##### Instances
``` purescript
Newtype OriginCustomHeader _
```

#### `OriginCustomHeadersList`

``` purescript
newtype OriginCustomHeadersList
  = OriginCustomHeadersList (Array OriginCustomHeader)
```

##### Instances
``` purescript
Newtype OriginCustomHeadersList _
```

#### `OriginList`

``` purescript
newtype OriginList
  = OriginList (Array Origin)
```

##### Instances
``` purescript
Newtype OriginList _
```

#### `OriginProtocolPolicy`

``` purescript
newtype OriginProtocolPolicy
  = OriginProtocolPolicy String
```

##### Instances
``` purescript
Newtype OriginProtocolPolicy _
```

#### `OriginSslProtocols`

``` purescript
newtype OriginSslProtocols
  = OriginSslProtocols { "Quantity" :: Int, "Items" :: SslProtocolsList }
```

<p>A complex type that contains information about the SSL/TLS protocols that CloudFront can use when establishing an HTTPS connection with your origin. </p>

##### Instances
``` purescript
Newtype OriginSslProtocols _
```

#### `Origins`

``` purescript
newtype Origins
  = Origins { "Quantity" :: Int, "Items" :: NullOrUndefined (OriginList) }
```

<p>A complex type that contains information about origins for this distribution. </p>

##### Instances
``` purescript
Newtype Origins _
```

#### `PathList`

``` purescript
newtype PathList
  = PathList (Array String)
```

##### Instances
``` purescript
Newtype PathList _
```

#### `Paths`

``` purescript
newtype Paths
  = Paths { "Quantity" :: Int, "Items" :: NullOrUndefined (PathList) }
```

<p>A complex type that contains information about the objects that you want to invalidate. For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects">Specifying the Objects to Invalidate</a> in the <i>Amazon CloudFront Developer Guide</i>. </p>

##### Instances
``` purescript
Newtype Paths _
```

#### `PreconditionFailed`

``` purescript
newtype PreconditionFailed
  = PreconditionFailed { "Message" :: NullOrUndefined (String) }
```

<p>The precondition given in one or more of the request-header fields evaluated to <code>false</code>. </p>

##### Instances
``` purescript
Newtype PreconditionFailed _
```

#### `PriceClass`

``` purescript
newtype PriceClass
  = PriceClass String
```

##### Instances
``` purescript
Newtype PriceClass _
```

#### `QueryStringCacheKeys`

``` purescript
newtype QueryStringCacheKeys
  = QueryStringCacheKeys { "Quantity" :: Int, "Items" :: NullOrUndefined (QueryStringCacheKeysList) }
```

##### Instances
``` purescript
Newtype QueryStringCacheKeys _
```

#### `QueryStringCacheKeysList`

``` purescript
newtype QueryStringCacheKeysList
  = QueryStringCacheKeysList (Array String)
```

##### Instances
``` purescript
Newtype QueryStringCacheKeysList _
```

#### `ResourceARN`

``` purescript
newtype ResourceARN
  = ResourceARN String
```

##### Instances
``` purescript
Newtype ResourceARN _
```

#### `Restrictions`

``` purescript
newtype Restrictions
  = Restrictions { "GeoRestriction" :: GeoRestriction }
```

<p>A complex type that identifies ways in which you want to restrict distribution of your content.</p>

##### Instances
``` purescript
Newtype Restrictions _
```

#### `S3Origin`

``` purescript
newtype S3Origin
  = S3Origin { "DomainName" :: String, "OriginAccessIdentity" :: String }
```

<p>A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.</p>

##### Instances
``` purescript
Newtype S3Origin _
```

#### `S3OriginConfig`

``` purescript
newtype S3OriginConfig
  = S3OriginConfig { "OriginAccessIdentity" :: String }
```

<p>A complex type that contains information about the Amazon S3 origin. If the origin is a custom origin, use the <code>CustomOriginConfig</code> element instead.</p>

##### Instances
``` purescript
Newtype S3OriginConfig _
```

#### `SSLSupportMethod`

``` purescript
newtype SSLSupportMethod
  = SSLSupportMethod String
```

##### Instances
``` purescript
Newtype SSLSupportMethod _
```

#### `Signer`

``` purescript
newtype Signer
  = Signer { "AwsAccountNumber" :: NullOrUndefined (String), "KeyPairIds" :: NullOrUndefined (KeyPairIds) }
```

<p>A complex type that lists the AWS accounts that were included in the <code>TrustedSigners</code> complex type, as well as their active CloudFront key pair IDs, if any. </p>

##### Instances
``` purescript
Newtype Signer _
```

#### `SignerList`

``` purescript
newtype SignerList
  = SignerList (Array Signer)
```

##### Instances
``` purescript
Newtype SignerList _
```

#### `SslProtocol`

``` purescript
newtype SslProtocol
  = SslProtocol String
```

##### Instances
``` purescript
Newtype SslProtocol _
```

#### `SslProtocolsList`

``` purescript
newtype SslProtocolsList
  = SslProtocolsList (Array SslProtocol)
```

##### Instances
``` purescript
Newtype SslProtocolsList _
```

#### `StreamingDistribution`

``` purescript
newtype StreamingDistribution
  = StreamingDistribution { "Id" :: String, "ARN" :: String, "Status" :: String, "LastModifiedTime" :: NullOrUndefined (Number), "DomainName" :: String, "ActiveTrustedSigners" :: ActiveTrustedSigners, "StreamingDistributionConfig" :: StreamingDistributionConfig }
```

<p>A streaming distribution. </p>

##### Instances
``` purescript
Newtype StreamingDistribution _
```

#### `StreamingDistributionAlreadyExists`

``` purescript
newtype StreamingDistributionAlreadyExists
  = StreamingDistributionAlreadyExists { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype StreamingDistributionAlreadyExists _
```

#### `StreamingDistributionConfig`

``` purescript
newtype StreamingDistributionConfig
  = StreamingDistributionConfig { "CallerReference" :: String, "S3Origin" :: S3Origin, "Aliases" :: NullOrUndefined (Aliases), "Comment" :: String, "Logging" :: NullOrUndefined (StreamingLoggingConfig), "TrustedSigners" :: TrustedSigners, "PriceClass" :: NullOrUndefined (PriceClass), "Enabled" :: Boolean }
```

<p>The RTMP distribution's configuration information.</p>

##### Instances
``` purescript
Newtype StreamingDistributionConfig _
```

#### `StreamingDistributionConfigWithTags`

``` purescript
newtype StreamingDistributionConfigWithTags
  = StreamingDistributionConfigWithTags { "StreamingDistributionConfig" :: StreamingDistributionConfig, "Tags" :: Tags }
```

<p>A streaming distribution Configuration and a list of tags to be associated with the streaming distribution.</p>

##### Instances
``` purescript
Newtype StreamingDistributionConfigWithTags _
```

#### `StreamingDistributionList`

``` purescript
newtype StreamingDistributionList
  = StreamingDistributionList { "Marker" :: String, "NextMarker" :: NullOrUndefined (String), "MaxItems" :: Int, "IsTruncated" :: Boolean, "Quantity" :: Int, "Items" :: NullOrUndefined (StreamingDistributionSummaryList) }
```

<p>A streaming distribution list. </p>

##### Instances
``` purescript
Newtype StreamingDistributionList _
```

#### `StreamingDistributionNotDisabled`

``` purescript
newtype StreamingDistributionNotDisabled
  = StreamingDistributionNotDisabled { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype StreamingDistributionNotDisabled _
```

#### `StreamingDistributionSummary`

``` purescript
newtype StreamingDistributionSummary
  = StreamingDistributionSummary { "Id" :: String, "ARN" :: String, "Status" :: String, "LastModifiedTime" :: Number, "DomainName" :: String, "S3Origin" :: S3Origin, "Aliases" :: Aliases, "TrustedSigners" :: TrustedSigners, "Comment" :: String, "PriceClass" :: PriceClass, "Enabled" :: Boolean }
```

<p> A summary of the information for an Amazon CloudFront streaming distribution.</p>

##### Instances
``` purescript
Newtype StreamingDistributionSummary _
```

#### `StreamingDistributionSummaryList`

``` purescript
newtype StreamingDistributionSummaryList
  = StreamingDistributionSummaryList (Array StreamingDistributionSummary)
```

##### Instances
``` purescript
Newtype StreamingDistributionSummaryList _
```

#### `StreamingLoggingConfig`

``` purescript
newtype StreamingLoggingConfig
  = StreamingLoggingConfig { "Enabled" :: Boolean, "Bucket" :: String, "Prefix" :: String }
```

<p>A complex type that controls whether access logs are written for this streaming distribution.</p>

##### Instances
``` purescript
Newtype StreamingLoggingConfig _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: NullOrUndefined (TagValue) }
```

<p> A complex type that contains <code>Tag</code> key and <code>Tag</code> value.</p>

##### Instances
``` purescript
Newtype Tag _
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

<p> A string that contains <code>Tag</code> key.</p> <p>The string length should be between 1 and 128 characters. Valid characters include <code>a-z</code>, <code>A-Z</code>, <code>0-9</code>, space, and the special characters <code>_ - . : / = + @</code>.</p>

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

#### `TagKeys`

``` purescript
newtype TagKeys
  = TagKeys { "Items" :: NullOrUndefined (TagKeyList) }
```

<p> A complex type that contains zero or more <code>Tag</code> elements.</p>

##### Instances
``` purescript
Newtype TagKeys _
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

#### `TagResourceRequest`

``` purescript
newtype TagResourceRequest
  = TagResourceRequest { "Resource" :: ResourceARN, "Tags" :: Tags }
```

<p> The request to add tags to a CloudFront resource.</p>

##### Instances
``` purescript
Newtype TagResourceRequest _
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

#### `Tags`

``` purescript
newtype Tags
  = Tags { "Items" :: NullOrUndefined (TagList) }
```

<p> A complex type that contains zero or more <code>Tag</code> elements.</p>

##### Instances
``` purescript
Newtype Tags _
```

#### `TooManyCacheBehaviors`

``` purescript
newtype TooManyCacheBehaviors
  = TooManyCacheBehaviors { "Message" :: NullOrUndefined (String) }
```

<p>You cannot create more cache behaviors for the distribution.</p>

##### Instances
``` purescript
Newtype TooManyCacheBehaviors _
```

#### `TooManyCertificates`

``` purescript
newtype TooManyCertificates
  = TooManyCertificates { "Message" :: NullOrUndefined (String) }
```

<p>You cannot create anymore custom SSL/TLS certificates.</p>

##### Instances
``` purescript
Newtype TooManyCertificates _
```

#### `TooManyCloudFrontOriginAccessIdentities`

``` purescript
newtype TooManyCloudFrontOriginAccessIdentities
  = TooManyCloudFrontOriginAccessIdentities { "Message" :: NullOrUndefined (String) }
```

<p>Processing your request would cause you to exceed the maximum number of origin access identities allowed.</p>

##### Instances
``` purescript
Newtype TooManyCloudFrontOriginAccessIdentities _
```

#### `TooManyCookieNamesInWhiteList`

``` purescript
newtype TooManyCookieNamesInWhiteList
  = TooManyCookieNamesInWhiteList { "Message" :: NullOrUndefined (String) }
```

<p>Your request contains more cookie names in the whitelist than are allowed per cache behavior.</p>

##### Instances
``` purescript
Newtype TooManyCookieNamesInWhiteList _
```

#### `TooManyDistributionCNAMEs`

``` purescript
newtype TooManyDistributionCNAMEs
  = TooManyDistributionCNAMEs { "Message" :: NullOrUndefined (String) }
```

<p>Your request contains more CNAMEs than are allowed per distribution.</p>

##### Instances
``` purescript
Newtype TooManyDistributionCNAMEs _
```

#### `TooManyDistributions`

``` purescript
newtype TooManyDistributions
  = TooManyDistributions { "Message" :: NullOrUndefined (String) }
```

<p>Processing your request would cause you to exceed the maximum number of distributions allowed.</p>

##### Instances
``` purescript
Newtype TooManyDistributions _
```

#### `TooManyDistributionsWithLambdaAssociations`

``` purescript
newtype TooManyDistributionsWithLambdaAssociations
  = TooManyDistributionsWithLambdaAssociations { "Message" :: NullOrUndefined (String) }
```

<p>Processing your request would cause the maximum number of distributions with Lambda function associations per owner to be exceeded.</p>

##### Instances
``` purescript
Newtype TooManyDistributionsWithLambdaAssociations _
```

#### `TooManyHeadersInForwardedValues`

``` purescript
newtype TooManyHeadersInForwardedValues
  = TooManyHeadersInForwardedValues { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype TooManyHeadersInForwardedValues _
```

#### `TooManyInvalidationsInProgress`

``` purescript
newtype TooManyInvalidationsInProgress
  = TooManyInvalidationsInProgress { "Message" :: NullOrUndefined (String) }
```

<p>You have exceeded the maximum number of allowable InProgress invalidation batch requests, or invalidation objects.</p>

##### Instances
``` purescript
Newtype TooManyInvalidationsInProgress _
```

#### `TooManyLambdaFunctionAssociations`

``` purescript
newtype TooManyLambdaFunctionAssociations
  = TooManyLambdaFunctionAssociations { "Message" :: NullOrUndefined (String) }
```

<p>Your request contains more Lambda function associations than are allowed per distribution.</p>

##### Instances
``` purescript
Newtype TooManyLambdaFunctionAssociations _
```

#### `TooManyOriginCustomHeaders`

``` purescript
newtype TooManyOriginCustomHeaders
  = TooManyOriginCustomHeaders { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype TooManyOriginCustomHeaders _
```

#### `TooManyOrigins`

``` purescript
newtype TooManyOrigins
  = TooManyOrigins { "Message" :: NullOrUndefined (String) }
```

<p>You cannot create more origins for the distribution.</p>

##### Instances
``` purescript
Newtype TooManyOrigins _
```

#### `TooManyQueryStringParameters`

``` purescript
newtype TooManyQueryStringParameters
  = TooManyQueryStringParameters { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype TooManyQueryStringParameters _
```

#### `TooManyStreamingDistributionCNAMEs`

``` purescript
newtype TooManyStreamingDistributionCNAMEs
  = TooManyStreamingDistributionCNAMEs { "Message" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype TooManyStreamingDistributionCNAMEs _
```

#### `TooManyStreamingDistributions`

``` purescript
newtype TooManyStreamingDistributions
  = TooManyStreamingDistributions { "Message" :: NullOrUndefined (String) }
```

<p>Processing your request would cause you to exceed the maximum number of streaming distributions allowed.</p>

##### Instances
``` purescript
Newtype TooManyStreamingDistributions _
```

#### `TooManyTrustedSigners`

``` purescript
newtype TooManyTrustedSigners
  = TooManyTrustedSigners { "Message" :: NullOrUndefined (String) }
```

<p>Your request contains more trusted signers than are allowed per distribution.</p>

##### Instances
``` purescript
Newtype TooManyTrustedSigners _
```

#### `TrustedSignerDoesNotExist`

``` purescript
newtype TrustedSignerDoesNotExist
  = TrustedSignerDoesNotExist { "Message" :: NullOrUndefined (String) }
```

<p>One or more of your trusted signers do not exist.</p>

##### Instances
``` purescript
Newtype TrustedSignerDoesNotExist _
```

#### `TrustedSigners`

``` purescript
newtype TrustedSigners
  = TrustedSigners { "Enabled" :: Boolean, "Quantity" :: Int, "Items" :: NullOrUndefined (AwsAccountNumberList) }
```

<p>A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content.</p> <p>If you want to require signed URLs in requests for objects in the target origin that match the <code>PathPattern</code> for this cache behavior, specify <code>true</code> for <code>Enabled</code>, and specify the applicable values for <code>Quantity</code> and <code>Items</code>. For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html">Serving Private Content through CloudFront</a> in the <i>Amazon Amazon CloudFront Developer Guide</i>.</p> <p>If you don't want to require signed URLs in requests for objects that match <code>PathPattern</code>, specify <code>false</code> for <code>Enabled</code> and <code>0</code> for <code>Quantity</code>. Omit <code>Items</code>.</p> <p>To add, change, or remove one or more trusted signers, change <code>Enabled</code> to <code>true</code> (if it's currently <code>false</code>), change <code>Quantity</code> as applicable, and specify all of the trusted signers that you want to include in the updated distribution.</p> <p>For more information about updating the distribution configuration, see <a>DistributionConfig</a> .</p>

##### Instances
``` purescript
Newtype TrustedSigners _
```

#### `UntagResourceRequest`

``` purescript
newtype UntagResourceRequest
  = UntagResourceRequest { "Resource" :: ResourceARN, "TagKeys" :: TagKeys }
```

<p> The request to remove tags from a CloudFront resource.</p>

##### Instances
``` purescript
Newtype UntagResourceRequest _
```

#### `UpdateCloudFrontOriginAccessIdentityRequest`

``` purescript
newtype UpdateCloudFrontOriginAccessIdentityRequest
  = UpdateCloudFrontOriginAccessIdentityRequest { "CloudFrontOriginAccessIdentityConfig" :: CloudFrontOriginAccessIdentityConfig, "Id" :: String, "IfMatch" :: NullOrUndefined (String) }
```

<p>The request to update an origin access identity.</p>

##### Instances
``` purescript
Newtype UpdateCloudFrontOriginAccessIdentityRequest _
```

#### `UpdateCloudFrontOriginAccessIdentityResult`

``` purescript
newtype UpdateCloudFrontOriginAccessIdentityResult
  = UpdateCloudFrontOriginAccessIdentityResult { "CloudFrontOriginAccessIdentity" :: NullOrUndefined (CloudFrontOriginAccessIdentity), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype UpdateCloudFrontOriginAccessIdentityResult _
```

#### `UpdateDistributionRequest`

``` purescript
newtype UpdateDistributionRequest
  = UpdateDistributionRequest { "DistributionConfig" :: DistributionConfig, "Id" :: String, "IfMatch" :: NullOrUndefined (String) }
```

<p>The request to update a distribution.</p>

##### Instances
``` purescript
Newtype UpdateDistributionRequest _
```

#### `UpdateDistributionResult`

``` purescript
newtype UpdateDistributionResult
  = UpdateDistributionResult { "Distribution" :: NullOrUndefined (Distribution), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype UpdateDistributionResult _
```

#### `UpdateStreamingDistributionRequest`

``` purescript
newtype UpdateStreamingDistributionRequest
  = UpdateStreamingDistributionRequest { "StreamingDistributionConfig" :: StreamingDistributionConfig, "Id" :: String, "IfMatch" :: NullOrUndefined (String) }
```

<p>The request to update a streaming distribution.</p>

##### Instances
``` purescript
Newtype UpdateStreamingDistributionRequest _
```

#### `UpdateStreamingDistributionResult`

``` purescript
newtype UpdateStreamingDistributionResult
  = UpdateStreamingDistributionResult { "StreamingDistribution" :: NullOrUndefined (StreamingDistribution), "ETag" :: NullOrUndefined (String) }
```

<p>The returned result of the corresponding request.</p>

##### Instances
``` purescript
Newtype UpdateStreamingDistributionResult _
```

#### `ViewerCertificate`

``` purescript
newtype ViewerCertificate
  = ViewerCertificate { "CloudFrontDefaultCertificate" :: NullOrUndefined (Boolean), "IAMCertificateId" :: NullOrUndefined (String), "ACMCertificateArn" :: NullOrUndefined (String), "SSLSupportMethod" :: NullOrUndefined (SSLSupportMethod), "MinimumProtocolVersion" :: NullOrUndefined (MinimumProtocolVersion), "Certificate" :: NullOrUndefined (String), "CertificateSource" :: NullOrUndefined (CertificateSource) }
```

<p>A complex type that specifies the following:</p> <ul> <li> <p>Which SSL/TLS certificate to use when viewers request objects using HTTPS</p> </li> <li> <p>Whether you want CloudFront to use dedicated IP addresses or SNI when you're using alternate domain names in your object names</p> </li> <li> <p>The minimum protocol version that you want CloudFront to use when communicating with viewers</p> </li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html">Using an HTTPS Connection to Access Your Objects</a> in the <i>Amazon Amazon CloudFront Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype ViewerCertificate _
```

#### `ViewerProtocolPolicy`

``` purescript
newtype ViewerProtocolPolicy
  = ViewerProtocolPolicy String
```

##### Instances
``` purescript
Newtype ViewerProtocolPolicy _
```


