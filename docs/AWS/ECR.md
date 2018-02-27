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

##### Instances
``` purescript
Newtype Arn _
```

#### `AuthorizationData`

``` purescript
newtype AuthorizationData
  = AuthorizationData { "AuthorizationToken'" :: NullOrUndefined (Base64), "ExpiresAt'" :: NullOrUndefined (ExpirationTimestamp), "ProxyEndpoint'" :: NullOrUndefined (ProxyEndpoint) }
```

<p>An object representing authorization data for an Amazon ECR registry.</p>

##### Instances
``` purescript
Newtype AuthorizationData _
```

#### `AuthorizationDataList`

``` purescript
newtype AuthorizationDataList
  = AuthorizationDataList (Array AuthorizationData)
```

##### Instances
``` purescript
Newtype AuthorizationDataList _
```

#### `Base64`

``` purescript
newtype Base64
  = Base64 String
```

##### Instances
``` purescript
Newtype Base64 _
```

#### `BatchCheckLayerAvailabilityRequest`

``` purescript
newtype BatchCheckLayerAvailabilityRequest
  = BatchCheckLayerAvailabilityRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "LayerDigests'" :: BatchedOperationLayerDigestList }
```

##### Instances
``` purescript
Newtype BatchCheckLayerAvailabilityRequest _
```

#### `BatchCheckLayerAvailabilityResponse`

``` purescript
newtype BatchCheckLayerAvailabilityResponse
  = BatchCheckLayerAvailabilityResponse { "Layers'" :: NullOrUndefined (LayerList), "Failures'" :: NullOrUndefined (LayerFailureList) }
```

##### Instances
``` purescript
Newtype BatchCheckLayerAvailabilityResponse _
```

#### `BatchDeleteImageRequest`

``` purescript
newtype BatchDeleteImageRequest
  = BatchDeleteImageRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "ImageIds'" :: ImageIdentifierList }
```

<p>Deletes specified images within a specified repository. Images are specified with either the <code>imageTag</code> or <code>imageDigest</code>.</p>

##### Instances
``` purescript
Newtype BatchDeleteImageRequest _
```

#### `BatchDeleteImageResponse`

``` purescript
newtype BatchDeleteImageResponse
  = BatchDeleteImageResponse { "ImageIds'" :: NullOrUndefined (ImageIdentifierList), "Failures'" :: NullOrUndefined (ImageFailureList) }
```

##### Instances
``` purescript
Newtype BatchDeleteImageResponse _
```

#### `BatchGetImageRequest`

``` purescript
newtype BatchGetImageRequest
  = BatchGetImageRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "ImageIds'" :: ImageIdentifierList, "AcceptedMediaTypes'" :: NullOrUndefined (MediaTypeList) }
```

##### Instances
``` purescript
Newtype BatchGetImageRequest _
```

#### `BatchGetImageResponse`

``` purescript
newtype BatchGetImageResponse
  = BatchGetImageResponse { "Images'" :: NullOrUndefined (ImageList), "Failures'" :: NullOrUndefined (ImageFailureList) }
```

##### Instances
``` purescript
Newtype BatchGetImageResponse _
```

#### `BatchedOperationLayerDigest`

``` purescript
newtype BatchedOperationLayerDigest
  = BatchedOperationLayerDigest String
```

##### Instances
``` purescript
Newtype BatchedOperationLayerDigest _
```

#### `BatchedOperationLayerDigestList`

``` purescript
newtype BatchedOperationLayerDigestList
  = BatchedOperationLayerDigestList (Array BatchedOperationLayerDigest)
```

##### Instances
``` purescript
Newtype BatchedOperationLayerDigestList _
```

#### `CompleteLayerUploadRequest`

``` purescript
newtype CompleteLayerUploadRequest
  = CompleteLayerUploadRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "UploadId'" :: UploadId, "LayerDigests'" :: LayerDigestList }
```

##### Instances
``` purescript
Newtype CompleteLayerUploadRequest _
```

#### `CompleteLayerUploadResponse`

``` purescript
newtype CompleteLayerUploadResponse
  = CompleteLayerUploadResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "UploadId'" :: NullOrUndefined (UploadId), "LayerDigest'" :: NullOrUndefined (LayerDigest) }
```

##### Instances
``` purescript
Newtype CompleteLayerUploadResponse _
```

#### `CreateRepositoryRequest`

``` purescript
newtype CreateRepositoryRequest
  = CreateRepositoryRequest { "RepositoryName'" :: RepositoryName }
```

##### Instances
``` purescript
Newtype CreateRepositoryRequest _
```

#### `CreateRepositoryResponse`

``` purescript
newtype CreateRepositoryResponse
  = CreateRepositoryResponse { "Repository'" :: NullOrUndefined (Repository) }
```

##### Instances
``` purescript
Newtype CreateRepositoryResponse _
```

#### `CreationTimestamp`

``` purescript
newtype CreationTimestamp
  = CreationTimestamp Number
```

##### Instances
``` purescript
Newtype CreationTimestamp _
```

#### `DeleteLifecyclePolicyRequest`

``` purescript
newtype DeleteLifecyclePolicyRequest
  = DeleteLifecyclePolicyRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName }
```

##### Instances
``` purescript
Newtype DeleteLifecyclePolicyRequest _
```

#### `DeleteLifecyclePolicyResponse`

``` purescript
newtype DeleteLifecyclePolicyResponse
  = DeleteLifecyclePolicyResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText), "LastEvaluatedAt'" :: NullOrUndefined (EvaluationTimestamp) }
```

##### Instances
``` purescript
Newtype DeleteLifecyclePolicyResponse _
```

#### `DeleteRepositoryPolicyRequest`

``` purescript
newtype DeleteRepositoryPolicyRequest
  = DeleteRepositoryPolicyRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName }
```

##### Instances
``` purescript
Newtype DeleteRepositoryPolicyRequest _
```

#### `DeleteRepositoryPolicyResponse`

``` purescript
newtype DeleteRepositoryPolicyResponse
  = DeleteRepositoryPolicyResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "PolicyText'" :: NullOrUndefined (RepositoryPolicyText) }
```

##### Instances
``` purescript
Newtype DeleteRepositoryPolicyResponse _
```

#### `DeleteRepositoryRequest`

``` purescript
newtype DeleteRepositoryRequest
  = DeleteRepositoryRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "Force'" :: NullOrUndefined (ForceFlag) }
```

##### Instances
``` purescript
Newtype DeleteRepositoryRequest _
```

#### `DeleteRepositoryResponse`

``` purescript
newtype DeleteRepositoryResponse
  = DeleteRepositoryResponse { "Repository'" :: NullOrUndefined (Repository) }
```

##### Instances
``` purescript
Newtype DeleteRepositoryResponse _
```

#### `DescribeImagesFilter`

``` purescript
newtype DescribeImagesFilter
  = DescribeImagesFilter { "TagStatus'" :: NullOrUndefined (TagStatus) }
```

<p>An object representing a filter on a <a>DescribeImages</a> operation.</p>

##### Instances
``` purescript
Newtype DescribeImagesFilter _
```

#### `DescribeImagesRequest`

``` purescript
newtype DescribeImagesRequest
  = DescribeImagesRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "ImageIds'" :: NullOrUndefined (ImageIdentifierList), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults), "Filter'" :: NullOrUndefined (DescribeImagesFilter) }
```

##### Instances
``` purescript
Newtype DescribeImagesRequest _
```

#### `DescribeImagesResponse`

``` purescript
newtype DescribeImagesResponse
  = DescribeImagesResponse { "ImageDetails'" :: NullOrUndefined (ImageDetailList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeImagesResponse _
```

#### `DescribeRepositoriesRequest`

``` purescript
newtype DescribeRepositoriesRequest
  = DescribeRepositoriesRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryNames'" :: NullOrUndefined (RepositoryNameList), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype DescribeRepositoriesRequest _
```

#### `DescribeRepositoriesResponse`

``` purescript
newtype DescribeRepositoriesResponse
  = DescribeRepositoriesResponse { "Repositories'" :: NullOrUndefined (RepositoryList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeRepositoriesResponse _
```

#### `EmptyUploadException`

``` purescript
newtype EmptyUploadException
  = EmptyUploadException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified layer upload does not contain any layer parts.</p>

##### Instances
``` purescript
Newtype EmptyUploadException _
```

#### `EvaluationTimestamp`

``` purescript
newtype EvaluationTimestamp
  = EvaluationTimestamp Number
```

##### Instances
``` purescript
Newtype EvaluationTimestamp _
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

##### Instances
``` purescript
Newtype ExceptionMessage _
```

#### `ExpirationTimestamp`

``` purescript
newtype ExpirationTimestamp
  = ExpirationTimestamp Number
```

##### Instances
``` purescript
Newtype ExpirationTimestamp _
```

#### `ForceFlag`

``` purescript
newtype ForceFlag
  = ForceFlag Boolean
```

##### Instances
``` purescript
Newtype ForceFlag _
```

#### `GetAuthorizationTokenRegistryIdList`

``` purescript
newtype GetAuthorizationTokenRegistryIdList
  = GetAuthorizationTokenRegistryIdList (Array RegistryId)
```

##### Instances
``` purescript
Newtype GetAuthorizationTokenRegistryIdList _
```

#### `GetAuthorizationTokenRequest`

``` purescript
newtype GetAuthorizationTokenRequest
  = GetAuthorizationTokenRequest { "RegistryIds'" :: NullOrUndefined (GetAuthorizationTokenRegistryIdList) }
```

##### Instances
``` purescript
Newtype GetAuthorizationTokenRequest _
```

#### `GetAuthorizationTokenResponse`

``` purescript
newtype GetAuthorizationTokenResponse
  = GetAuthorizationTokenResponse { "AuthorizationData'" :: NullOrUndefined (AuthorizationDataList) }
```

##### Instances
``` purescript
Newtype GetAuthorizationTokenResponse _
```

#### `GetDownloadUrlForLayerRequest`

``` purescript
newtype GetDownloadUrlForLayerRequest
  = GetDownloadUrlForLayerRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "LayerDigest'" :: LayerDigest }
```

##### Instances
``` purescript
Newtype GetDownloadUrlForLayerRequest _
```

#### `GetDownloadUrlForLayerResponse`

``` purescript
newtype GetDownloadUrlForLayerResponse
  = GetDownloadUrlForLayerResponse { "DownloadUrl'" :: NullOrUndefined (Url), "LayerDigest'" :: NullOrUndefined (LayerDigest) }
```

##### Instances
``` purescript
Newtype GetDownloadUrlForLayerResponse _
```

#### `GetLifecyclePolicyPreviewRequest`

``` purescript
newtype GetLifecyclePolicyPreviewRequest
  = GetLifecyclePolicyPreviewRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "ImageIds'" :: NullOrUndefined (ImageIdentifierList), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults), "Filter'" :: NullOrUndefined (LifecyclePolicyPreviewFilter) }
```

##### Instances
``` purescript
Newtype GetLifecyclePolicyPreviewRequest _
```

#### `GetLifecyclePolicyPreviewResponse`

``` purescript
newtype GetLifecyclePolicyPreviewResponse
  = GetLifecyclePolicyPreviewResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText), "Status'" :: NullOrUndefined (LifecyclePolicyPreviewStatus), "NextToken'" :: NullOrUndefined (NextToken), "PreviewResults'" :: NullOrUndefined (LifecyclePolicyPreviewResultList), "Summary'" :: NullOrUndefined (LifecyclePolicyPreviewSummary) }
```

##### Instances
``` purescript
Newtype GetLifecyclePolicyPreviewResponse _
```

#### `GetLifecyclePolicyRequest`

``` purescript
newtype GetLifecyclePolicyRequest
  = GetLifecyclePolicyRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName }
```

##### Instances
``` purescript
Newtype GetLifecyclePolicyRequest _
```

#### `GetLifecyclePolicyResponse`

``` purescript
newtype GetLifecyclePolicyResponse
  = GetLifecyclePolicyResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText), "LastEvaluatedAt'" :: NullOrUndefined (EvaluationTimestamp) }
```

##### Instances
``` purescript
Newtype GetLifecyclePolicyResponse _
```

#### `GetRepositoryPolicyRequest`

``` purescript
newtype GetRepositoryPolicyRequest
  = GetRepositoryPolicyRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName }
```

##### Instances
``` purescript
Newtype GetRepositoryPolicyRequest _
```

#### `GetRepositoryPolicyResponse`

``` purescript
newtype GetRepositoryPolicyResponse
  = GetRepositoryPolicyResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "PolicyText'" :: NullOrUndefined (RepositoryPolicyText) }
```

##### Instances
``` purescript
Newtype GetRepositoryPolicyResponse _
```

#### `Image`

``` purescript
newtype Image
  = Image { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "ImageId'" :: NullOrUndefined (ImageIdentifier), "ImageManifest'" :: NullOrUndefined (ImageManifest) }
```

<p>An object representing an Amazon ECR image.</p>

##### Instances
``` purescript
Newtype Image _
```

#### `ImageActionType`

``` purescript
newtype ImageActionType
  = ImageActionType String
```

##### Instances
``` purescript
Newtype ImageActionType _
```

#### `ImageAlreadyExistsException`

``` purescript
newtype ImageAlreadyExistsException
  = ImageAlreadyExistsException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified image has already been pushed, and there were no changes to the manifest or image tag after the last push.</p>

##### Instances
``` purescript
Newtype ImageAlreadyExistsException _
```

#### `ImageCount`

``` purescript
newtype ImageCount
  = ImageCount Int
```

##### Instances
``` purescript
Newtype ImageCount _
```

#### `ImageDetail`

``` purescript
newtype ImageDetail
  = ImageDetail { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "ImageDigest'" :: NullOrUndefined (ImageDigest), "ImageTags'" :: NullOrUndefined (ImageTagList), "ImageSizeInBytes'" :: NullOrUndefined (ImageSizeInBytes), "ImagePushedAt'" :: NullOrUndefined (PushTimestamp) }
```

<p>An object that describes an image returned by a <a>DescribeImages</a> operation.</p>

##### Instances
``` purescript
Newtype ImageDetail _
```

#### `ImageDetailList`

``` purescript
newtype ImageDetailList
  = ImageDetailList (Array ImageDetail)
```

##### Instances
``` purescript
Newtype ImageDetailList _
```

#### `ImageDigest`

``` purescript
newtype ImageDigest
  = ImageDigest String
```

##### Instances
``` purescript
Newtype ImageDigest _
```

#### `ImageFailure`

``` purescript
newtype ImageFailure
  = ImageFailure { "ImageId'" :: NullOrUndefined (ImageIdentifier), "FailureCode'" :: NullOrUndefined (ImageFailureCode), "FailureReason'" :: NullOrUndefined (ImageFailureReason) }
```

<p>An object representing an Amazon ECR image failure.</p>

##### Instances
``` purescript
Newtype ImageFailure _
```

#### `ImageFailureCode`

``` purescript
newtype ImageFailureCode
  = ImageFailureCode String
```

##### Instances
``` purescript
Newtype ImageFailureCode _
```

#### `ImageFailureList`

``` purescript
newtype ImageFailureList
  = ImageFailureList (Array ImageFailure)
```

##### Instances
``` purescript
Newtype ImageFailureList _
```

#### `ImageFailureReason`

``` purescript
newtype ImageFailureReason
  = ImageFailureReason String
```

##### Instances
``` purescript
Newtype ImageFailureReason _
```

#### `ImageIdentifier`

``` purescript
newtype ImageIdentifier
  = ImageIdentifier { "ImageDigest'" :: NullOrUndefined (ImageDigest), "ImageTag'" :: NullOrUndefined (ImageTag) }
```

<p>An object with identifying information for an Amazon ECR image.</p>

##### Instances
``` purescript
Newtype ImageIdentifier _
```

#### `ImageIdentifierList`

``` purescript
newtype ImageIdentifierList
  = ImageIdentifierList (Array ImageIdentifier)
```

##### Instances
``` purescript
Newtype ImageIdentifierList _
```

#### `ImageList`

``` purescript
newtype ImageList
  = ImageList (Array Image)
```

##### Instances
``` purescript
Newtype ImageList _
```

#### `ImageManifest`

``` purescript
newtype ImageManifest
  = ImageManifest String
```

##### Instances
``` purescript
Newtype ImageManifest _
```

#### `ImageNotFoundException`

``` purescript
newtype ImageNotFoundException
  = ImageNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The image requested does not exist in the specified repository.</p>

##### Instances
``` purescript
Newtype ImageNotFoundException _
```

#### `ImageSizeInBytes`

``` purescript
newtype ImageSizeInBytes
  = ImageSizeInBytes Number
```

##### Instances
``` purescript
Newtype ImageSizeInBytes _
```

#### `ImageTag`

``` purescript
newtype ImageTag
  = ImageTag String
```

##### Instances
``` purescript
Newtype ImageTag _
```

#### `ImageTagList`

``` purescript
newtype ImageTagList
  = ImageTagList (Array ImageTag)
```

##### Instances
``` purescript
Newtype ImageTagList _
```

#### `InitiateLayerUploadRequest`

``` purescript
newtype InitiateLayerUploadRequest
  = InitiateLayerUploadRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName }
```

##### Instances
``` purescript
Newtype InitiateLayerUploadRequest _
```

#### `InitiateLayerUploadResponse`

``` purescript
newtype InitiateLayerUploadResponse
  = InitiateLayerUploadResponse { "UploadId'" :: NullOrUndefined (UploadId), "PartSize'" :: NullOrUndefined (PartSize) }
```

##### Instances
``` purescript
Newtype InitiateLayerUploadResponse _
```

#### `InvalidLayerException`

``` purescript
newtype InvalidLayerException
  = InvalidLayerException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The layer digest calculation performed by Amazon ECR upon receipt of the image layer does not match the digest specified.</p>

##### Instances
``` purescript
Newtype InvalidLayerException _
```

#### `InvalidLayerPartException`

``` purescript
newtype InvalidLayerPartException
  = InvalidLayerPartException { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "UploadId'" :: NullOrUndefined (UploadId), "LastValidByteReceived'" :: NullOrUndefined (PartSize), "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The layer part size is not valid, or the first byte specified is not consecutive to the last byte of a previous layer part upload.</p>

##### Instances
``` purescript
Newtype InvalidLayerPartException _
```

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified parameter is invalid. Review the available parameters for the API request.</p>

##### Instances
``` purescript
Newtype InvalidParameterException _
```

#### `Layer`

``` purescript
newtype Layer
  = Layer { "LayerDigest'" :: NullOrUndefined (LayerDigest), "LayerAvailability'" :: NullOrUndefined (LayerAvailability), "LayerSize'" :: NullOrUndefined (LayerSizeInBytes), "MediaType'" :: NullOrUndefined (MediaType) }
```

<p>An object representing an Amazon ECR image layer.</p>

##### Instances
``` purescript
Newtype Layer _
```

#### `LayerAlreadyExistsException`

``` purescript
newtype LayerAlreadyExistsException
  = LayerAlreadyExistsException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The image layer already exists in the associated repository.</p>

##### Instances
``` purescript
Newtype LayerAlreadyExistsException _
```

#### `LayerAvailability`

``` purescript
newtype LayerAvailability
  = LayerAvailability String
```

##### Instances
``` purescript
Newtype LayerAvailability _
```

#### `LayerDigest`

``` purescript
newtype LayerDigest
  = LayerDigest String
```

##### Instances
``` purescript
Newtype LayerDigest _
```

#### `LayerDigestList`

``` purescript
newtype LayerDigestList
  = LayerDigestList (Array LayerDigest)
```

##### Instances
``` purescript
Newtype LayerDigestList _
```

#### `LayerFailure`

``` purescript
newtype LayerFailure
  = LayerFailure { "LayerDigest'" :: NullOrUndefined (BatchedOperationLayerDigest), "FailureCode'" :: NullOrUndefined (LayerFailureCode), "FailureReason'" :: NullOrUndefined (LayerFailureReason) }
```

<p>An object representing an Amazon ECR image layer failure.</p>

##### Instances
``` purescript
Newtype LayerFailure _
```

#### `LayerFailureCode`

``` purescript
newtype LayerFailureCode
  = LayerFailureCode String
```

##### Instances
``` purescript
Newtype LayerFailureCode _
```

#### `LayerFailureList`

``` purescript
newtype LayerFailureList
  = LayerFailureList (Array LayerFailure)
```

##### Instances
``` purescript
Newtype LayerFailureList _
```

#### `LayerFailureReason`

``` purescript
newtype LayerFailureReason
  = LayerFailureReason String
```

##### Instances
``` purescript
Newtype LayerFailureReason _
```

#### `LayerInaccessibleException`

``` purescript
newtype LayerInaccessibleException
  = LayerInaccessibleException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified layer is not available because it is not associated with an image. Unassociated image layers may be cleaned up at any time.</p>

##### Instances
``` purescript
Newtype LayerInaccessibleException _
```

#### `LayerList`

``` purescript
newtype LayerList
  = LayerList (Array Layer)
```

##### Instances
``` purescript
Newtype LayerList _
```

#### `LayerPartBlob`

``` purescript
newtype LayerPartBlob
  = LayerPartBlob String
```

##### Instances
``` purescript
Newtype LayerPartBlob _
```

#### `LayerPartTooSmallException`

``` purescript
newtype LayerPartTooSmallException
  = LayerPartTooSmallException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>Layer parts must be at least 5 MiB in size.</p>

##### Instances
``` purescript
Newtype LayerPartTooSmallException _
```

#### `LayerSizeInBytes`

``` purescript
newtype LayerSizeInBytes
  = LayerSizeInBytes Number
```

##### Instances
``` purescript
Newtype LayerSizeInBytes _
```

#### `LayersNotFoundException`

``` purescript
newtype LayersNotFoundException
  = LayersNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified layers could not be found, or the specified layer is not valid for this repository.</p>

##### Instances
``` purescript
Newtype LayersNotFoundException _
```

#### `LifecyclePolicyNotFoundException`

``` purescript
newtype LifecyclePolicyNotFoundException
  = LifecyclePolicyNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The lifecycle policy could not be found, and no policy is set to the repository.</p>

##### Instances
``` purescript
Newtype LifecyclePolicyNotFoundException _
```

#### `LifecyclePolicyPreviewFilter`

``` purescript
newtype LifecyclePolicyPreviewFilter
  = LifecyclePolicyPreviewFilter { "TagStatus'" :: NullOrUndefined (TagStatus) }
```

<p>The filter for the lifecycle policy preview.</p>

##### Instances
``` purescript
Newtype LifecyclePolicyPreviewFilter _
```

#### `LifecyclePolicyPreviewInProgressException`

``` purescript
newtype LifecyclePolicyPreviewInProgressException
  = LifecyclePolicyPreviewInProgressException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The previous lifecycle policy preview request has not completed. Please try again later.</p>

##### Instances
``` purescript
Newtype LifecyclePolicyPreviewInProgressException _
```

#### `LifecyclePolicyPreviewNotFoundException`

``` purescript
newtype LifecyclePolicyPreviewNotFoundException
  = LifecyclePolicyPreviewNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>There is no dry run for this repository.</p>

##### Instances
``` purescript
Newtype LifecyclePolicyPreviewNotFoundException _
```

#### `LifecyclePolicyPreviewResult`

``` purescript
newtype LifecyclePolicyPreviewResult
  = LifecyclePolicyPreviewResult { "ImageTags'" :: NullOrUndefined (ImageTagList), "ImageDigest'" :: NullOrUndefined (ImageDigest), "ImagePushedAt'" :: NullOrUndefined (PushTimestamp), "Action'" :: NullOrUndefined (LifecyclePolicyRuleAction), "AppliedRulePriority'" :: NullOrUndefined (LifecyclePolicyRulePriority) }
```

<p>The result of the lifecycle policy preview.</p>

##### Instances
``` purescript
Newtype LifecyclePolicyPreviewResult _
```

#### `LifecyclePolicyPreviewResultList`

``` purescript
newtype LifecyclePolicyPreviewResultList
  = LifecyclePolicyPreviewResultList (Array LifecyclePolicyPreviewResult)
```

##### Instances
``` purescript
Newtype LifecyclePolicyPreviewResultList _
```

#### `LifecyclePolicyPreviewStatus`

``` purescript
newtype LifecyclePolicyPreviewStatus
  = LifecyclePolicyPreviewStatus String
```

##### Instances
``` purescript
Newtype LifecyclePolicyPreviewStatus _
```

#### `LifecyclePolicyPreviewSummary`

``` purescript
newtype LifecyclePolicyPreviewSummary
  = LifecyclePolicyPreviewSummary { "ExpiringImageTotalCount'" :: NullOrUndefined (ImageCount) }
```

<p>The summary of the lifecycle policy preview request.</p>

##### Instances
``` purescript
Newtype LifecyclePolicyPreviewSummary _
```

#### `LifecyclePolicyRuleAction`

``` purescript
newtype LifecyclePolicyRuleAction
  = LifecyclePolicyRuleAction { "Type'" :: NullOrUndefined (ImageActionType) }
```

<p>The type of action to be taken.</p>

##### Instances
``` purescript
Newtype LifecyclePolicyRuleAction _
```

#### `LifecyclePolicyRulePriority`

``` purescript
newtype LifecyclePolicyRulePriority
  = LifecyclePolicyRulePriority Int
```

##### Instances
``` purescript
Newtype LifecyclePolicyRulePriority _
```

#### `LifecyclePolicyText`

``` purescript
newtype LifecyclePolicyText
  = LifecyclePolicyText String
```

##### Instances
``` purescript
Newtype LifecyclePolicyText _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The operation did not succeed because it would have exceeded a service limit for your account. For more information, see <a href="http://docs.aws.amazon.com/AmazonECR/latest/userguide/service_limits.html">Amazon ECR Default Service Limits</a> in the Amazon EC2 Container Registry User Guide.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListImagesFilter`

``` purescript
newtype ListImagesFilter
  = ListImagesFilter { "TagStatus'" :: NullOrUndefined (TagStatus) }
```

<p>An object representing a filter on a <a>ListImages</a> operation.</p>

##### Instances
``` purescript
Newtype ListImagesFilter _
```

#### `ListImagesRequest`

``` purescript
newtype ListImagesRequest
  = ListImagesRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults), "Filter'" :: NullOrUndefined (ListImagesFilter) }
```

##### Instances
``` purescript
Newtype ListImagesRequest _
```

#### `ListImagesResponse`

``` purescript
newtype ListImagesResponse
  = ListImagesResponse { "ImageIds'" :: NullOrUndefined (ImageIdentifierList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListImagesResponse _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

##### Instances
``` purescript
Newtype MaxResults _
```

#### `MediaType`

``` purescript
newtype MediaType
  = MediaType String
```

##### Instances
``` purescript
Newtype MediaType _
```

#### `MediaTypeList`

``` purescript
newtype MediaTypeList
  = MediaTypeList (Array MediaType)
```

##### Instances
``` purescript
Newtype MediaTypeList _
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

#### `PartSize`

``` purescript
newtype PartSize
  = PartSize Number
```

##### Instances
``` purescript
Newtype PartSize _
```

#### `ProxyEndpoint`

``` purescript
newtype ProxyEndpoint
  = ProxyEndpoint String
```

##### Instances
``` purescript
Newtype ProxyEndpoint _
```

#### `PushTimestamp`

``` purescript
newtype PushTimestamp
  = PushTimestamp Number
```

##### Instances
``` purescript
Newtype PushTimestamp _
```

#### `PutImageRequest`

``` purescript
newtype PutImageRequest
  = PutImageRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "ImageManifest'" :: ImageManifest, "ImageTag'" :: NullOrUndefined (ImageTag) }
```

##### Instances
``` purescript
Newtype PutImageRequest _
```

#### `PutImageResponse`

``` purescript
newtype PutImageResponse
  = PutImageResponse { "Image'" :: NullOrUndefined (Image) }
```

##### Instances
``` purescript
Newtype PutImageResponse _
```

#### `PutLifecyclePolicyRequest`

``` purescript
newtype PutLifecyclePolicyRequest
  = PutLifecyclePolicyRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "LifecyclePolicyText'" :: LifecyclePolicyText }
```

##### Instances
``` purescript
Newtype PutLifecyclePolicyRequest _
```

#### `PutLifecyclePolicyResponse`

``` purescript
newtype PutLifecyclePolicyResponse
  = PutLifecyclePolicyResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText) }
```

##### Instances
``` purescript
Newtype PutLifecyclePolicyResponse _
```

#### `RegistryId`

``` purescript
newtype RegistryId
  = RegistryId String
```

##### Instances
``` purescript
Newtype RegistryId _
```

#### `Repository`

``` purescript
newtype Repository
  = Repository { "RepositoryArn'" :: NullOrUndefined (Arn), "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "RepositoryUri'" :: NullOrUndefined (Url), "CreatedAt'" :: NullOrUndefined (CreationTimestamp) }
```

<p>An object representing a repository.</p>

##### Instances
``` purescript
Newtype Repository _
```

#### `RepositoryAlreadyExistsException`

``` purescript
newtype RepositoryAlreadyExistsException
  = RepositoryAlreadyExistsException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified repository already exists in the specified registry.</p>

##### Instances
``` purescript
Newtype RepositoryAlreadyExistsException _
```

#### `RepositoryList`

``` purescript
newtype RepositoryList
  = RepositoryList (Array Repository)
```

##### Instances
``` purescript
Newtype RepositoryList _
```

#### `RepositoryName`

``` purescript
newtype RepositoryName
  = RepositoryName String
```

##### Instances
``` purescript
Newtype RepositoryName _
```

#### `RepositoryNameList`

``` purescript
newtype RepositoryNameList
  = RepositoryNameList (Array RepositoryName)
```

##### Instances
``` purescript
Newtype RepositoryNameList _
```

#### `RepositoryNotEmptyException`

``` purescript
newtype RepositoryNotEmptyException
  = RepositoryNotEmptyException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified repository contains images. To delete a repository that contains images, you must force the deletion with the <code>force</code> parameter.</p>

##### Instances
``` purescript
Newtype RepositoryNotEmptyException _
```

#### `RepositoryNotFoundException`

``` purescript
newtype RepositoryNotFoundException
  = RepositoryNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified repository could not be found. Check the spelling of the specified repository and ensure that you are performing operations on the correct registry.</p>

##### Instances
``` purescript
Newtype RepositoryNotFoundException _
```

#### `RepositoryPolicyNotFoundException`

``` purescript
newtype RepositoryPolicyNotFoundException
  = RepositoryPolicyNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified repository and registry combination does not have an associated repository policy.</p>

##### Instances
``` purescript
Newtype RepositoryPolicyNotFoundException _
```

#### `RepositoryPolicyText`

``` purescript
newtype RepositoryPolicyText
  = RepositoryPolicyText String
```

##### Instances
``` purescript
Newtype RepositoryPolicyText _
```

#### `ServerException`

``` purescript
newtype ServerException
  = ServerException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>These errors are usually caused by a server-side issue.</p>

##### Instances
``` purescript
Newtype ServerException _
```

#### `SetRepositoryPolicyRequest`

``` purescript
newtype SetRepositoryPolicyRequest
  = SetRepositoryPolicyRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "PolicyText'" :: RepositoryPolicyText, "Force'" :: NullOrUndefined (ForceFlag) }
```

##### Instances
``` purescript
Newtype SetRepositoryPolicyRequest _
```

#### `SetRepositoryPolicyResponse`

``` purescript
newtype SetRepositoryPolicyResponse
  = SetRepositoryPolicyResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "PolicyText'" :: NullOrUndefined (RepositoryPolicyText) }
```

##### Instances
``` purescript
Newtype SetRepositoryPolicyResponse _
```

#### `StartLifecyclePolicyPreviewRequest`

``` purescript
newtype StartLifecyclePolicyPreviewRequest
  = StartLifecyclePolicyPreviewRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText) }
```

##### Instances
``` purescript
Newtype StartLifecyclePolicyPreviewRequest _
```

#### `StartLifecyclePolicyPreviewResponse`

``` purescript
newtype StartLifecyclePolicyPreviewResponse
  = StartLifecyclePolicyPreviewResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText), "Status'" :: NullOrUndefined (LifecyclePolicyPreviewStatus) }
```

##### Instances
``` purescript
Newtype StartLifecyclePolicyPreviewResponse _
```

#### `TagStatus`

``` purescript
newtype TagStatus
  = TagStatus String
```

##### Instances
``` purescript
Newtype TagStatus _
```

#### `UploadId`

``` purescript
newtype UploadId
  = UploadId String
```

##### Instances
``` purescript
Newtype UploadId _
```

#### `UploadLayerPartRequest`

``` purescript
newtype UploadLayerPartRequest
  = UploadLayerPartRequest { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: RepositoryName, "UploadId'" :: UploadId, "PartFirstByte'" :: PartSize, "PartLastByte'" :: PartSize, "LayerPartBlob'" :: LayerPartBlob }
```

##### Instances
``` purescript
Newtype UploadLayerPartRequest _
```

#### `UploadLayerPartResponse`

``` purescript
newtype UploadLayerPartResponse
  = UploadLayerPartResponse { "RegistryId'" :: NullOrUndefined (RegistryId), "RepositoryName'" :: NullOrUndefined (RepositoryName), "UploadId'" :: NullOrUndefined (UploadId), "LastByteReceived'" :: NullOrUndefined (PartSize) }
```

##### Instances
``` purescript
Newtype UploadLayerPartResponse _
```

#### `UploadNotFoundException`

``` purescript
newtype UploadNotFoundException
  = UploadNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The upload could not be found, or the specified upload id is not valid for this repository.</p>

##### Instances
``` purescript
Newtype UploadNotFoundException _
```

#### `Url`

``` purescript
newtype Url
  = Url String
```

##### Instances
``` purescript
Newtype Url _
```


