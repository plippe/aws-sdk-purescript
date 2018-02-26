## Module AWS.ECR

<p>Amazon EC2 Container Registry (Amazon ECR) is a managed Docker registry service. Customers can use the familiar Docker CLI to push, pull, and manage images. Amazon ECR provides a secure, scalable, and reliable registry. Amazon ECR supports private Docker repositories with resource-based permissions using IAM so that specific users or Amazon EC2 instances can access repositories and images. Developers can use the Docker CLI to author and manage images.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `batchCheckLayerAvailability`

``` purescript
batchCheckLayerAvailability :: forall eff. BatchCheckLayerAvailabilityRequest -> Aff (err :: RequestError | eff) BatchCheckLayerAvailabilityResponse
```

<p>Check the availability of multiple image layers in a specified registry and repository.</p> <note> <p>This operation is used by the Amazon ECR proxy, and it is not intended for general use by customers for pulling and pushing images. In most cases, you should use the <code>docker</code> CLI to pull, tag, and push images.</p> </note>

#### `batchDeleteImage`

``` purescript
batchDeleteImage :: forall eff. BatchDeleteImageRequest -> Aff (err :: RequestError | eff) BatchDeleteImageResponse
```

<p>Deletes a list of specified images within a specified repository. Images are specified with either <code>imageTag</code> or <code>imageDigest</code>.</p> <p>You can remove a tag from an image by specifying the image's tag in your request. When you remove the last tag from an image, the image is deleted from your repository.</p> <p>You can completely delete an image (and all of its tags) by specifying the image's digest in your request.</p>

#### `batchGetImage`

``` purescript
batchGetImage :: forall eff. BatchGetImageRequest -> Aff (err :: RequestError | eff) BatchGetImageResponse
```

<p>Gets detailed information for specified images within a specified repository. Images are specified with either <code>imageTag</code> or <code>imageDigest</code>.</p>

#### `completeLayerUpload`

``` purescript
completeLayerUpload :: forall eff. CompleteLayerUploadRequest -> Aff (err :: RequestError | eff) CompleteLayerUploadResponse
```

<p>Informs Amazon ECR that the image layer upload has completed for a specified registry, repository name, and upload ID. You can optionally provide a <code>sha256</code> digest of the image layer for data validation purposes.</p> <note> <p>This operation is used by the Amazon ECR proxy, and it is not intended for general use by customers for pulling and pushing images. In most cases, you should use the <code>docker</code> CLI to pull, tag, and push images.</p> </note>

#### `createRepository`

``` purescript
createRepository :: forall eff. CreateRepositoryRequest -> Aff (err :: RequestError | eff) CreateRepositoryResponse
```

<p>Creates an image repository.</p>

#### `deleteLifecyclePolicy`

``` purescript
deleteLifecyclePolicy :: forall eff. DeleteLifecyclePolicyRequest -> Aff (err :: RequestError | eff) DeleteLifecyclePolicyResponse
```

<p>Deletes the specified lifecycle policy.</p>

#### `deleteRepository`

``` purescript
deleteRepository :: forall eff. DeleteRepositoryRequest -> Aff (err :: RequestError | eff) DeleteRepositoryResponse
```

<p>Deletes an existing image repository. If a repository contains images, you must use the <code>force</code> option to delete it.</p>

#### `deleteRepositoryPolicy`

``` purescript
deleteRepositoryPolicy :: forall eff. DeleteRepositoryPolicyRequest -> Aff (err :: RequestError | eff) DeleteRepositoryPolicyResponse
```

<p>Deletes the repository policy from a specified repository.</p>

#### `describeImages`

``` purescript
describeImages :: forall eff. DescribeImagesRequest -> Aff (err :: RequestError | eff) DescribeImagesResponse
```

<p>Returns metadata about the images in a repository, including image size, image tags, and creation date.</p> <note> <p>Beginning with Docker version 1.9, the Docker client compresses image layers before pushing them to a V2 Docker registry. The output of the <code>docker images</code> command shows the uncompressed image size, so it may return a larger image size than the image sizes returned by <a>DescribeImages</a>.</p> </note>

#### `describeRepositories`

``` purescript
describeRepositories :: forall eff. DescribeRepositoriesRequest -> Aff (err :: RequestError | eff) DescribeRepositoriesResponse
```

<p>Describes image repositories in a registry.</p>

#### `getAuthorizationToken`

``` purescript
getAuthorizationToken :: forall eff. GetAuthorizationTokenRequest -> Aff (err :: RequestError | eff) GetAuthorizationTokenResponse
```

<p>Retrieves a token that is valid for a specified registry for 12 hours. This command allows you to use the <code>docker</code> CLI to push and pull images with Amazon ECR. If you do not specify a registry, the default registry is assumed.</p> <p>The <code>authorizationToken</code> returned for each registry specified is a base64 encoded string that can be decoded and used in a <code>docker login</code> command to authenticate to a registry. The AWS CLI offers an <code>aws ecr get-login</code> command that simplifies the login process.</p>

#### `getDownloadUrlForLayer`

``` purescript
getDownloadUrlForLayer :: forall eff. GetDownloadUrlForLayerRequest -> Aff (err :: RequestError | eff) GetDownloadUrlForLayerResponse
```

<p>Retrieves the pre-signed Amazon S3 download URL corresponding to an image layer. You can only get URLs for image layers that are referenced in an image.</p> <note> <p>This operation is used by the Amazon ECR proxy, and it is not intended for general use by customers for pulling and pushing images. In most cases, you should use the <code>docker</code> CLI to pull, tag, and push images.</p> </note>

#### `getLifecyclePolicy`

``` purescript
getLifecyclePolicy :: forall eff. GetLifecyclePolicyRequest -> Aff (err :: RequestError | eff) GetLifecyclePolicyResponse
```

<p>Retrieves the specified lifecycle policy.</p>

#### `getLifecyclePolicyPreview`

``` purescript
getLifecyclePolicyPreview :: forall eff. GetLifecyclePolicyPreviewRequest -> Aff (err :: RequestError | eff) GetLifecyclePolicyPreviewResponse
```

<p>Retrieves the results of the specified lifecycle policy preview request.</p>

#### `getRepositoryPolicy`

``` purescript
getRepositoryPolicy :: forall eff. GetRepositoryPolicyRequest -> Aff (err :: RequestError | eff) GetRepositoryPolicyResponse
```

<p>Retrieves the repository policy for a specified repository.</p>

#### `initiateLayerUpload`

``` purescript
initiateLayerUpload :: forall eff. InitiateLayerUploadRequest -> Aff (err :: RequestError | eff) InitiateLayerUploadResponse
```

<p>Notify Amazon ECR that you intend to upload an image layer.</p> <note> <p>This operation is used by the Amazon ECR proxy, and it is not intended for general use by customers for pulling and pushing images. In most cases, you should use the <code>docker</code> CLI to pull, tag, and push images.</p> </note>

#### `listImages`

``` purescript
listImages :: forall eff. ListImagesRequest -> Aff (err :: RequestError | eff) ListImagesResponse
```

<p>Lists all the image IDs for a given repository.</p> <p>You can filter images based on whether or not they are tagged by setting the <code>tagStatus</code> parameter to <code>TAGGED</code> or <code>UNTAGGED</code>. For example, you can filter your results to return only <code>UNTAGGED</code> images and then pipe that result to a <a>BatchDeleteImage</a> operation to delete them. Or, you can filter your results to return only <code>TAGGED</code> images to list all of the tags in your repository.</p>

#### `putImage`

``` purescript
putImage :: forall eff. PutImageRequest -> Aff (err :: RequestError | eff) PutImageResponse
```

<p>Creates or updates the image manifest and tags associated with an image.</p> <note> <p>This operation is used by the Amazon ECR proxy, and it is not intended for general use by customers for pulling and pushing images. In most cases, you should use the <code>docker</code> CLI to pull, tag, and push images.</p> </note>

#### `putLifecyclePolicy`

``` purescript
putLifecyclePolicy :: forall eff. PutLifecyclePolicyRequest -> Aff (err :: RequestError | eff) PutLifecyclePolicyResponse
```

<p>Creates or updates a lifecycle policy.</p>

#### `setRepositoryPolicy`

``` purescript
setRepositoryPolicy :: forall eff. SetRepositoryPolicyRequest -> Aff (err :: RequestError | eff) SetRepositoryPolicyResponse
```

<p>Applies a repository policy on a specified repository to control access permissions.</p>

#### `startLifecyclePolicyPreview`

``` purescript
startLifecyclePolicyPreview :: forall eff. StartLifecyclePolicyPreviewRequest -> Aff (err :: RequestError | eff) StartLifecyclePolicyPreviewResponse
```

<p>Starts a preview of the specified lifecycle policy. This allows you to see the results before creating the lifecycle policy.</p>

#### `uploadLayerPart`

``` purescript
uploadLayerPart :: forall eff. UploadLayerPartRequest -> Aff (err :: RequestError | eff) UploadLayerPartResponse
```

<p>Uploads an image layer part to Amazon ECR.</p> <note> <p>This operation is used by the Amazon ECR proxy, and it is not intended for general use by customers for pulling and pushing images. In most cases, you should use the <code>docker</code> CLI to pull, tag, and push images.</p> </note>

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

#### `AuthorizationData`

``` purescript
newtype AuthorizationData
  = AuthorizationData { "AuthorizationToken'" :: NullOrUndefined (Base64), "ExpiresAt'" :: NullOrUndefined (ExpirationTimestamp), "ProxyEndpoint'" :: NullOrUndefined (ProxyEndpoint) }
```

<p>An object representing authorization data for an Amazon ECR registry.</p>

#### `AuthorizationDataList`

``` purescript
newtype AuthorizationDataList
  = AuthorizationDataList (Array AuthorizationData)
```

#### `Base64`

``` purescript
newtype Base64
  = Base64 String
```

#### `BatchCheckLayerAvailabilityRequest`

``` purescript
newtype BatchCheckLayerAvailabilityRequest
  = BatchCheckLayerAvailabilityRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "LayerDigests'" :: BatchedOperationLayerDigestList }
```

#### `BatchCheckLayerAvailabilityResponse`

``` purescript
newtype BatchCheckLayerAvailabilityResponse
  = BatchCheckLayerAvailabilityResponse { "Layers'" :: NullOrUndefined (LayerList), "Failures'" :: NullOrUndefined (LayerFailureList) }
```

#### `BatchDeleteImageRequest`

``` purescript
newtype BatchDeleteImageRequest
  = BatchDeleteImageRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "ImageIds'" :: ImageIdentifierList }
```

<p>Deletes specified images within a specified repository. Images are specified with either the <code>imageTag</code> or <code>imageDigest</code>.</p>

#### `BatchDeleteImageResponse`

``` purescript
newtype BatchDeleteImageResponse
  = BatchDeleteImageResponse { "ImageIds'" :: NullOrUndefined (ImageIdentifierList), "Failures'" :: NullOrUndefined (ImageFailureList) }
```

#### `BatchGetImageRequest`

``` purescript
newtype BatchGetImageRequest
  = BatchGetImageRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "ImageIds'" :: ImageIdentifierList, "AcceptedMediaTypes'" :: NullOrUndefined (MediaTypeList) }
```

#### `BatchGetImageResponse`

``` purescript
newtype BatchGetImageResponse
  = BatchGetImageResponse { "Images'" :: NullOrUndefined (ImageList), "Failures'" :: NullOrUndefined (ImageFailureList) }
```

#### `BatchedOperationLayerDigest`

``` purescript
newtype BatchedOperationLayerDigest
  = BatchedOperationLayerDigest String
```

#### `BatchedOperationLayerDigestList`

``` purescript
newtype BatchedOperationLayerDigestList
  = BatchedOperationLayerDigestList (Array BatchedOperationLayerDigest)
```

#### `CompleteLayerUploadRequest`

``` purescript
newtype CompleteLayerUploadRequest
  = CompleteLayerUploadRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "UploadId'" :: UploadId, "LayerDigests'" :: LayerDigestList }
```

#### `CompleteLayerUploadResponse`

``` purescript
newtype CompleteLayerUploadResponse
  = CompleteLayerUploadResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "UploadId'" :: NullOrUndefined (UploadId), "LayerDigest'" :: NullOrUndefined (LayerDigest) }
```

#### `CreateRepositoryRequest`

``` purescript
newtype CreateRepositoryRequest
  = CreateRepositoryRequest { "RepositoryName'" :: RepositoryName }
```

#### `CreateRepositoryResponse`

``` purescript
newtype CreateRepositoryResponse
  = CreateRepositoryResponse { "Repository'" :: NullOrUndefined (Repository) }
```

#### `CreationTimestamp`

``` purescript
newtype CreationTimestamp
  = CreationTimestamp Number
```

#### `DeleteLifecyclePolicyRequest`

``` purescript
newtype DeleteLifecyclePolicyRequest
  = DeleteLifecyclePolicyRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName }
```

#### `DeleteLifecyclePolicyResponse`

``` purescript
newtype DeleteLifecyclePolicyResponse
  = DeleteLifecyclePolicyResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText), "LastEvaluatedAt'" :: NullOrUndefined (EvaluationTimestamp) }
```

#### `DeleteRepositoryPolicyRequest`

``` purescript
newtype DeleteRepositoryPolicyRequest
  = DeleteRepositoryPolicyRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName }
```

#### `DeleteRepositoryPolicyResponse`

``` purescript
newtype DeleteRepositoryPolicyResponse
  = DeleteRepositoryPolicyResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "PolicyText'" :: NullOrUndefined (RepositoryPolicyText) }
```

#### `DeleteRepositoryRequest`

``` purescript
newtype DeleteRepositoryRequest
  = DeleteRepositoryRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "Force'" :: NullOrUndefined (ForceFlag) }
```

#### `DeleteRepositoryResponse`

``` purescript
newtype DeleteRepositoryResponse
  = DeleteRepositoryResponse { "Repository'" :: NullOrUndefined (Repository) }
```

#### `DescribeImagesFilter`

``` purescript
newtype DescribeImagesFilter
  = DescribeImagesFilter { "TagStatus'" :: NullOrUndefined (TagStatus) }
```

<p>An object representing a filter on a <a>DescribeImages</a> operation.</p>

#### `DescribeImagesRequest`

``` purescript
newtype DescribeImagesRequest
  = DescribeImagesRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "ImageIds'" :: NullOrUndefined (ImageIdentifierList), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults), "Filter'" :: NullOrUndefined (DescribeImagesFilter) }
```

#### `DescribeImagesResponse`

``` purescript
newtype DescribeImagesResponse
  = DescribeImagesResponse { "ImageDetails'" :: NullOrUndefined (ImageDetailList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `DescribeRepositoriesRequest`

``` purescript
newtype DescribeRepositoriesRequest
  = DescribeRepositoriesRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryNames'" :: NullOrUndefined (RepositoryNameList), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `DescribeRepositoriesResponse`

``` purescript
newtype DescribeRepositoriesResponse
  = DescribeRepositoriesResponse { "Repositories'" :: NullOrUndefined (RepositoryList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `EmptyUploadException`

``` purescript
newtype EmptyUploadException
  = EmptyUploadException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified layer upload does not contain any layer parts.</p>

#### `EvaluationTimestamp`

``` purescript
newtype EvaluationTimestamp
  = EvaluationTimestamp Number
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

#### `ExpirationTimestamp`

``` purescript
newtype ExpirationTimestamp
  = ExpirationTimestamp Number
```

#### `ForceFlag`

``` purescript
newtype ForceFlag
  = ForceFlag Boolean
```

#### `GetAuthorizationTokenRegistryIdList`

``` purescript
newtype GetAuthorizationTokenRegistryIdList
  = GetAuthorizationTokenRegistryIdList (Array RegistryId)
```

#### `GetAuthorizationTokenRequest`

``` purescript
newtype GetAuthorizationTokenRequest
  = GetAuthorizationTokenRequest { "RegistryIds'" :: NullOrUndefined (GetAuthorizationTokenRegistryIdList) }
```

#### `GetAuthorizationTokenResponse`

``` purescript
newtype GetAuthorizationTokenResponse
  = GetAuthorizationTokenResponse { "AuthorizationData'" :: NullOrUndefined (AuthorizationDataList) }
```

#### `GetDownloadUrlForLayerRequest`

``` purescript
newtype GetDownloadUrlForLayerRequest
  = GetDownloadUrlForLayerRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "LayerDigest'" :: LayerDigest }
```

#### `GetDownloadUrlForLayerResponse`

``` purescript
newtype GetDownloadUrlForLayerResponse
  = GetDownloadUrlForLayerResponse { "DownloadUrl'" :: NullOrUndefined (Url), "LayerDigest'" :: NullOrUndefined (LayerDigest) }
```

#### `GetLifecyclePolicyPreviewRequest`

``` purescript
newtype GetLifecyclePolicyPreviewRequest
  = GetLifecyclePolicyPreviewRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "ImageIds'" :: NullOrUndefined (ImageIdentifierList), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults), "Filter'" :: NullOrUndefined (LifecyclePolicyPreviewFilter) }
```

#### `GetLifecyclePolicyPreviewResponse`

``` purescript
newtype GetLifecyclePolicyPreviewResponse
  = GetLifecyclePolicyPreviewResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText), "Status'" :: NullOrUndefined (LifecyclePolicyPreviewStatus), "NextToken'" :: NullOrUndefined (NextToken), "PreviewResults'" :: NullOrUndefined (LifecyclePolicyPreviewResultList), "Summary'" :: NullOrUndefined (LifecyclePolicyPreviewSummary) }
```

#### `GetLifecyclePolicyRequest`

``` purescript
newtype GetLifecyclePolicyRequest
  = GetLifecyclePolicyRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName }
```

#### `GetLifecyclePolicyResponse`

``` purescript
newtype GetLifecyclePolicyResponse
  = GetLifecyclePolicyResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText), "LastEvaluatedAt'" :: NullOrUndefined (EvaluationTimestamp) }
```

#### `GetRepositoryPolicyRequest`

``` purescript
newtype GetRepositoryPolicyRequest
  = GetRepositoryPolicyRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName }
```

#### `GetRepositoryPolicyResponse`

``` purescript
newtype GetRepositoryPolicyResponse
  = GetRepositoryPolicyResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "PolicyText'" :: NullOrUndefined (RepositoryPolicyText) }
```

#### `Image`

``` purescript
newtype Image
  = Image { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "ImageId'" :: NullOrUndefined (ImageIdentifier), "ImageManifest'" :: NullOrUndefined (ImageManifest) }
```

<p>An object representing an Amazon ECR image.</p>

#### `ImageActionType`

``` purescript
newtype ImageActionType
  = ImageActionType String
```

#### `ImageAlreadyExistsException`

``` purescript
newtype ImageAlreadyExistsException
  = ImageAlreadyExistsException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified image has already been pushed, and there were no changes to the manifest or image tag after the last push.</p>

#### `ImageCount`

``` purescript
newtype ImageCount
  = ImageCount Int
```

#### `ImageDetail`

``` purescript
newtype ImageDetail
  = ImageDetail { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "ImageDigest'" :: NullOrUndefined (ImageDigest), "ImageTags'" :: NullOrUndefined (ImageTagList), "ImageSizeInBytes'" :: NullOrUndefined (ImageSizeInBytes), "ImagePushedAt'" :: NullOrUndefined (PushTimestamp) }
```

<p>An object that describes an image returned by a <a>DescribeImages</a> operation.</p>

#### `ImageDetailList`

``` purescript
newtype ImageDetailList
  = ImageDetailList (Array ImageDetail)
```

#### `ImageDigest`

``` purescript
newtype ImageDigest
  = ImageDigest String
```

#### `ImageFailure`

``` purescript
newtype ImageFailure
  = ImageFailure { "ImageId'" :: NullOrUndefined (ImageIdentifier), "FailureCode'" :: NullOrUndefined (ImageFailureCode), "FailureReason'" :: NullOrUndefined (ImageFailureReason) }
```

<p>An object representing an Amazon ECR image failure.</p>

#### `ImageFailureCode`

``` purescript
newtype ImageFailureCode
  = ImageFailureCode String
```

#### `ImageFailureList`

``` purescript
newtype ImageFailureList
  = ImageFailureList (Array ImageFailure)
```

#### `ImageFailureReason`

``` purescript
newtype ImageFailureReason
  = ImageFailureReason String
```

#### `ImageIdentifier`

``` purescript
newtype ImageIdentifier
  = ImageIdentifier { "ImageDigest'" :: NullOrUndefined (ImageDigest), "ImageTag'" :: NullOrUndefined (ImageTag) }
```

<p>An object with identifying information for an Amazon ECR image.</p>

#### `ImageIdentifierList`

``` purescript
newtype ImageIdentifierList
  = ImageIdentifierList (Array ImageIdentifier)
```

#### `ImageList`

``` purescript
newtype ImageList
  = ImageList (Array Image)
```

#### `ImageManifest`

``` purescript
newtype ImageManifest
  = ImageManifest String
```

#### `ImageNotFoundException`

``` purescript
newtype ImageNotFoundException
  = ImageNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The image requested does not exist in the specified repository.</p>

#### `ImageSizeInBytes`

``` purescript
newtype ImageSizeInBytes
  = ImageSizeInBytes Number
```

#### `ImageTag`

``` purescript
newtype ImageTag
  = ImageTag String
```

#### `ImageTagList`

``` purescript
newtype ImageTagList
  = ImageTagList (Array ImageTag)
```

#### `InitiateLayerUploadRequest`

``` purescript
newtype InitiateLayerUploadRequest
  = InitiateLayerUploadRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName }
```

#### `InitiateLayerUploadResponse`

``` purescript
newtype InitiateLayerUploadResponse
  = InitiateLayerUploadResponse { "UploadId'" :: NullOrUndefined (UploadId), "PartSize'" :: NullOrUndefined (PartSize) }
```

#### `InvalidLayerException`

``` purescript
newtype InvalidLayerException
  = InvalidLayerException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The layer digest calculation performed by Amazon ECR upon receipt of the image layer does not match the digest specified.</p>

#### `InvalidLayerPartException`

``` purescript
newtype InvalidLayerPartException
  = InvalidLayerPartException { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "UploadId'" :: NullOrUndefined (UploadId), "LastValidByteReceived'" :: NullOrUndefined (PartSize), "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The layer part size is not valid, or the first byte specified is not consecutive to the last byte of a previous layer part upload.</p>

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified parameter is invalid. Review the available parameters for the API request.</p>

#### `Layer`

``` purescript
newtype Layer
  = Layer { "LayerDigest'" :: NullOrUndefined (LayerDigest), "LayerAvailability'" :: NullOrUndefined (LayerAvailability), "LayerSize'" :: NullOrUndefined (LayerSizeInBytes), "MediaType'" :: NullOrUndefined (MediaType) }
```

<p>An object representing an Amazon ECR image layer.</p>

#### `LayerAlreadyExistsException`

``` purescript
newtype LayerAlreadyExistsException
  = LayerAlreadyExistsException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The image layer already exists in the associated repository.</p>

#### `LayerAvailability`

``` purescript
newtype LayerAvailability
  = LayerAvailability String
```

#### `LayerDigest`

``` purescript
newtype LayerDigest
  = LayerDigest String
```

#### `LayerDigestList`

``` purescript
newtype LayerDigestList
  = LayerDigestList (Array LayerDigest)
```

#### `LayerFailure`

``` purescript
newtype LayerFailure
  = LayerFailure { "LayerDigest'" :: NullOrUndefined (BatchedOperationLayerDigest), "FailureCode'" :: NullOrUndefined (LayerFailureCode), "FailureReason'" :: NullOrUndefined (LayerFailureReason) }
```

<p>An object representing an Amazon ECR image layer failure.</p>

#### `LayerFailureCode`

``` purescript
newtype LayerFailureCode
  = LayerFailureCode String
```

#### `LayerFailureList`

``` purescript
newtype LayerFailureList
  = LayerFailureList (Array LayerFailure)
```

#### `LayerFailureReason`

``` purescript
newtype LayerFailureReason
  = LayerFailureReason String
```

#### `LayerInaccessibleException`

``` purescript
newtype LayerInaccessibleException
  = LayerInaccessibleException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified layer is not available because it is not associated with an image. Unassociated image layers may be cleaned up at any time.</p>

#### `LayerList`

``` purescript
newtype LayerList
  = LayerList (Array Layer)
```

#### `LayerPartBlob`

``` purescript
newtype LayerPartBlob
  = LayerPartBlob String
```

#### `LayerPartTooSmallException`

``` purescript
newtype LayerPartTooSmallException
  = LayerPartTooSmallException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>Layer parts must be at least 5 MiB in size.</p>

#### `LayerSizeInBytes`

``` purescript
newtype LayerSizeInBytes
  = LayerSizeInBytes Number
```

#### `LayersNotFoundException`

``` purescript
newtype LayersNotFoundException
  = LayersNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified layers could not be found, or the specified layer is not valid for this repository.</p>

#### `LifecyclePolicyNotFoundException`

``` purescript
newtype LifecyclePolicyNotFoundException
  = LifecyclePolicyNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The lifecycle policy could not be found, and no policy is set to the repository.</p>

#### `LifecyclePolicyPreviewFilter`

``` purescript
newtype LifecyclePolicyPreviewFilter
  = LifecyclePolicyPreviewFilter { "TagStatus'" :: NullOrUndefined (TagStatus) }
```

<p>The filter for the lifecycle policy preview.</p>

#### `LifecyclePolicyPreviewInProgressException`

``` purescript
newtype LifecyclePolicyPreviewInProgressException
  = LifecyclePolicyPreviewInProgressException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The previous lifecycle policy preview request has not completed. Please try again later.</p>

#### `LifecyclePolicyPreviewNotFoundException`

``` purescript
newtype LifecyclePolicyPreviewNotFoundException
  = LifecyclePolicyPreviewNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>There is no dry run for this repository.</p>

#### `LifecyclePolicyPreviewResult`

``` purescript
newtype LifecyclePolicyPreviewResult
  = LifecyclePolicyPreviewResult { "ImageTags'" :: NullOrUndefined (ImageTagList), "ImageDigest'" :: NullOrUndefined (ImageDigest), "ImagePushedAt'" :: NullOrUndefined (PushTimestamp), "Action'" :: NullOrUndefined (LifecyclePolicyRuleAction), "AppliedRulePriority'" :: NullOrUndefined (LifecyclePolicyRulePriority) }
```

<p>The result of the lifecycle policy preview.</p>

#### `LifecyclePolicyPreviewResultList`

``` purescript
newtype LifecyclePolicyPreviewResultList
  = LifecyclePolicyPreviewResultList (Array LifecyclePolicyPreviewResult)
```

#### `LifecyclePolicyPreviewStatus`

``` purescript
newtype LifecyclePolicyPreviewStatus
  = LifecyclePolicyPreviewStatus String
```

#### `LifecyclePolicyPreviewSummary`

``` purescript
newtype LifecyclePolicyPreviewSummary
  = LifecyclePolicyPreviewSummary { "ExpiringImageTotalCount'" :: NullOrUndefined (ImageCount) }
```

<p>The summary of the lifecycle policy preview request.</p>

#### `LifecyclePolicyRuleAction`

``` purescript
newtype LifecyclePolicyRuleAction
  = LifecyclePolicyRuleAction { "Type'" :: NullOrUndefined (ImageActionType) }
```

<p>The type of action to be taken.</p>

#### `LifecyclePolicyRulePriority`

``` purescript
newtype LifecyclePolicyRulePriority
  = LifecyclePolicyRulePriority Int
```

#### `LifecyclePolicyText`

``` purescript
newtype LifecyclePolicyText
  = LifecyclePolicyText String
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The operation did not succeed because it would have exceeded a service limit for your account. For more information, see <a href="http://docs.aws.amazon.com/AmazonECR/latest/userguide/service_limits.html">Amazon ECR Default Service Limits</a> in the Amazon EC2 Container Registry User Guide.</p>

#### `ListImagesFilter`

``` purescript
newtype ListImagesFilter
  = ListImagesFilter { "TagStatus'" :: NullOrUndefined (TagStatus) }
```

<p>An object representing a filter on a <a>ListImages</a> operation.</p>

#### `ListImagesRequest`

``` purescript
newtype ListImagesRequest
  = ListImagesRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults), "Filter'" :: NullOrUndefined (ListImagesFilter) }
```

#### `ListImagesResponse`

``` purescript
newtype ListImagesResponse
  = ListImagesResponse { "ImageIds'" :: NullOrUndefined (ImageIdentifierList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `MediaType`

``` purescript
newtype MediaType
  = MediaType String
```

#### `MediaTypeList`

``` purescript
newtype MediaTypeList
  = MediaTypeList (Array MediaType)
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `PartSize`

``` purescript
newtype PartSize
  = PartSize Number
```

#### `ProxyEndpoint`

``` purescript
newtype ProxyEndpoint
  = ProxyEndpoint String
```

#### `PushTimestamp`

``` purescript
newtype PushTimestamp
  = PushTimestamp Number
```

#### `PutImageRequest`

``` purescript
newtype PutImageRequest
  = PutImageRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "ImageManifest'" :: ImageManifest, "ImageTag'" :: NullOrUndefined (ImageTag) }
```

#### `PutImageResponse`

``` purescript
newtype PutImageResponse
  = PutImageResponse { "Image'" :: NullOrUndefined (Image) }
```

#### `PutLifecyclePolicyRequest`

``` purescript
newtype PutLifecyclePolicyRequest
  = PutLifecyclePolicyRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "LifecyclePolicyText'" :: LifecyclePolicyText }
```

#### `PutLifecyclePolicyResponse`

``` purescript
newtype PutLifecyclePolicyResponse
  = PutLifecyclePolicyResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText) }
```

#### `RegistryId`

``` purescript
newtype RegistryId
  = RegistryId String
```

#### `Repository`

``` purescript
newtype Repository
  = Repository { "RepositoryArn'" :: NullOrUndefined (Arn), "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "RepositoryUri'" :: NullOrUndefined (Url), "CreatedAt'" :: NullOrUndefined (CreationTimestamp) }
```

<p>An object representing a repository.</p>

#### `RepositoryAlreadyExistsException`

``` purescript
newtype RepositoryAlreadyExistsException
  = RepositoryAlreadyExistsException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified repository already exists in the specified registry.</p>

#### `RepositoryList`

``` purescript
newtype RepositoryList
  = RepositoryList (Array Repository)
```

#### `RepositoryName`

``` purescript
newtype RepositoryName
  = RepositoryName String
```

#### `RepositoryNameList`

``` purescript
newtype RepositoryNameList
  = RepositoryNameList (Array RepositoryName)
```

#### `RepositoryNotEmptyException`

``` purescript
newtype RepositoryNotEmptyException
  = RepositoryNotEmptyException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified repository contains images. To delete a repository that contains images, you must force the deletion with the <code>force</code> parameter.</p>

#### `RepositoryNotFoundException`

``` purescript
newtype RepositoryNotFoundException
  = RepositoryNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified repository could not be found. Check the spelling of the specified repository and ensure that you are performing operations on the correct registry.</p>

#### `RepositoryPolicyNotFoundException`

``` purescript
newtype RepositoryPolicyNotFoundException
  = RepositoryPolicyNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified repository and registry combination does not have an associated repository policy.</p>

#### `RepositoryPolicyText`

``` purescript
newtype RepositoryPolicyText
  = RepositoryPolicyText String
```

#### `ServerException`

``` purescript
newtype ServerException
  = ServerException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>These errors are usually caused by a server-side issue.</p>

#### `SetRepositoryPolicyRequest`

``` purescript
newtype SetRepositoryPolicyRequest
  = SetRepositoryPolicyRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "PolicyText'" :: RepositoryPolicyText, "Force'" :: NullOrUndefined (ForceFlag) }
```

#### `SetRepositoryPolicyResponse`

``` purescript
newtype SetRepositoryPolicyResponse
  = SetRepositoryPolicyResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "PolicyText'" :: NullOrUndefined (RepositoryPolicyText) }
```

#### `StartLifecyclePolicyPreviewRequest`

``` purescript
newtype StartLifecyclePolicyPreviewRequest
  = StartLifecyclePolicyPreviewRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText) }
```

#### `StartLifecyclePolicyPreviewResponse`

``` purescript
newtype StartLifecyclePolicyPreviewResponse
  = StartLifecyclePolicyPreviewResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText), "Status'" :: NullOrUndefined (LifecyclePolicyPreviewStatus) }
```

#### `TagStatus`

``` purescript
newtype TagStatus
  = TagStatus String
```

#### `UploadId`

``` purescript
newtype UploadId
  = UploadId String
```

#### `UploadLayerPartRequest`

``` purescript
newtype UploadLayerPartRequest
  = UploadLayerPartRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "UploadId'" :: UploadId, "PartFirstByte'" :: PartSize, "PartLastByte'" :: PartSize, "LayerPartBlob'" :: LayerPartBlob }
```

#### `UploadLayerPartResponse`

``` purescript
newtype UploadLayerPartResponse
  = UploadLayerPartResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "UploadId'" :: NullOrUndefined (UploadId), "LastByteReceived'" :: NullOrUndefined (PartSize) }
```

#### `UploadNotFoundException`

``` purescript
newtype UploadNotFoundException
  = UploadNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The upload could not be found, or the specified upload id is not valid for this repository.</p>

#### `Url`

``` purescript
newtype Url
  = Url String
```


