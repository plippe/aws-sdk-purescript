

-- | <p>Amazon EC2 Container Registry (Amazon ECR) is a managed Docker registry service. Customers can use the familiar Docker CLI to push, pull, and manage images. Amazon ECR provides a secure, scalable, and reliable registry. Amazon ECR supports private Docker repositories with resource-based permissions using IAM so that specific users or Amazon EC2 instances can access repositories and images. Developers can use the Docker CLI to author and manage images.</p>
module AWS.ECR where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ECR" :: String


-- | <p>Check the availability of multiple image layers in a specified registry and repository.</p> <note> <p>This operation is used by the Amazon ECR proxy, and it is not intended for general use by customers for pulling and pushing images. In most cases, you should use the <code>docker</code> CLI to pull, tag, and push images.</p> </note>
batchCheckLayerAvailability :: forall eff. BatchCheckLayerAvailabilityRequest -> Aff (err :: AWS.RequestError | eff) BatchCheckLayerAvailabilityResponse
batchCheckLayerAvailability = AWS.request serviceName "BatchCheckLayerAvailability" 


-- | <p>Deletes a list of specified images within a specified repository. Images are specified with either <code>imageTag</code> or <code>imageDigest</code>.</p> <p>You can remove a tag from an image by specifying the image's tag in your request. When you remove the last tag from an image, the image is deleted from your repository.</p> <p>You can completely delete an image (and all of its tags) by specifying the image's digest in your request.</p>
batchDeleteImage :: forall eff. BatchDeleteImageRequest -> Aff (err :: AWS.RequestError | eff) BatchDeleteImageResponse
batchDeleteImage = AWS.request serviceName "BatchDeleteImage" 


-- | <p>Gets detailed information for specified images within a specified repository. Images are specified with either <code>imageTag</code> or <code>imageDigest</code>.</p>
batchGetImage :: forall eff. BatchGetImageRequest -> Aff (err :: AWS.RequestError | eff) BatchGetImageResponse
batchGetImage = AWS.request serviceName "BatchGetImage" 


-- | <p>Informs Amazon ECR that the image layer upload has completed for a specified registry, repository name, and upload ID. You can optionally provide a <code>sha256</code> digest of the image layer for data validation purposes.</p> <note> <p>This operation is used by the Amazon ECR proxy, and it is not intended for general use by customers for pulling and pushing images. In most cases, you should use the <code>docker</code> CLI to pull, tag, and push images.</p> </note>
completeLayerUpload :: forall eff. CompleteLayerUploadRequest -> Aff (err :: AWS.RequestError | eff) CompleteLayerUploadResponse
completeLayerUpload = AWS.request serviceName "CompleteLayerUpload" 


-- | <p>Creates an image repository.</p>
createRepository :: forall eff. CreateRepositoryRequest -> Aff (err :: AWS.RequestError | eff) CreateRepositoryResponse
createRepository = AWS.request serviceName "CreateRepository" 


-- | <p>Deletes the specified lifecycle policy.</p>
deleteLifecyclePolicy :: forall eff. DeleteLifecyclePolicyRequest -> Aff (err :: AWS.RequestError | eff) DeleteLifecyclePolicyResponse
deleteLifecyclePolicy = AWS.request serviceName "DeleteLifecyclePolicy" 


-- | <p>Deletes an existing image repository. If a repository contains images, you must use the <code>force</code> option to delete it.</p>
deleteRepository :: forall eff. DeleteRepositoryRequest -> Aff (err :: AWS.RequestError | eff) DeleteRepositoryResponse
deleteRepository = AWS.request serviceName "DeleteRepository" 


-- | <p>Deletes the repository policy from a specified repository.</p>
deleteRepositoryPolicy :: forall eff. DeleteRepositoryPolicyRequest -> Aff (err :: AWS.RequestError | eff) DeleteRepositoryPolicyResponse
deleteRepositoryPolicy = AWS.request serviceName "DeleteRepositoryPolicy" 


-- | <p>Returns metadata about the images in a repository, including image size, image tags, and creation date.</p> <note> <p>Beginning with Docker version 1.9, the Docker client compresses image layers before pushing them to a V2 Docker registry. The output of the <code>docker images</code> command shows the uncompressed image size, so it may return a larger image size than the image sizes returned by <a>DescribeImages</a>.</p> </note>
describeImages :: forall eff. DescribeImagesRequest -> Aff (err :: AWS.RequestError | eff) DescribeImagesResponse
describeImages = AWS.request serviceName "DescribeImages" 


-- | <p>Describes image repositories in a registry.</p>
describeRepositories :: forall eff. DescribeRepositoriesRequest -> Aff (err :: AWS.RequestError | eff) DescribeRepositoriesResponse
describeRepositories = AWS.request serviceName "DescribeRepositories" 


-- | <p>Retrieves a token that is valid for a specified registry for 12 hours. This command allows you to use the <code>docker</code> CLI to push and pull images with Amazon ECR. If you do not specify a registry, the default registry is assumed.</p> <p>The <code>authorizationToken</code> returned for each registry specified is a base64 encoded string that can be decoded and used in a <code>docker login</code> command to authenticate to a registry. The AWS CLI offers an <code>aws ecr get-login</code> command that simplifies the login process.</p>
getAuthorizationToken :: forall eff. GetAuthorizationTokenRequest -> Aff (err :: AWS.RequestError | eff) GetAuthorizationTokenResponse
getAuthorizationToken = AWS.request serviceName "GetAuthorizationToken" 


-- | <p>Retrieves the pre-signed Amazon S3 download URL corresponding to an image layer. You can only get URLs for image layers that are referenced in an image.</p> <note> <p>This operation is used by the Amazon ECR proxy, and it is not intended for general use by customers for pulling and pushing images. In most cases, you should use the <code>docker</code> CLI to pull, tag, and push images.</p> </note>
getDownloadUrlForLayer :: forall eff. GetDownloadUrlForLayerRequest -> Aff (err :: AWS.RequestError | eff) GetDownloadUrlForLayerResponse
getDownloadUrlForLayer = AWS.request serviceName "GetDownloadUrlForLayer" 


-- | <p>Retrieves the specified lifecycle policy.</p>
getLifecyclePolicy :: forall eff. GetLifecyclePolicyRequest -> Aff (err :: AWS.RequestError | eff) GetLifecyclePolicyResponse
getLifecyclePolicy = AWS.request serviceName "GetLifecyclePolicy" 


-- | <p>Retrieves the results of the specified lifecycle policy preview request.</p>
getLifecyclePolicyPreview :: forall eff. GetLifecyclePolicyPreviewRequest -> Aff (err :: AWS.RequestError | eff) GetLifecyclePolicyPreviewResponse
getLifecyclePolicyPreview = AWS.request serviceName "GetLifecyclePolicyPreview" 


-- | <p>Retrieves the repository policy for a specified repository.</p>
getRepositoryPolicy :: forall eff. GetRepositoryPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetRepositoryPolicyResponse
getRepositoryPolicy = AWS.request serviceName "GetRepositoryPolicy" 


-- | <p>Notify Amazon ECR that you intend to upload an image layer.</p> <note> <p>This operation is used by the Amazon ECR proxy, and it is not intended for general use by customers for pulling and pushing images. In most cases, you should use the <code>docker</code> CLI to pull, tag, and push images.</p> </note>
initiateLayerUpload :: forall eff. InitiateLayerUploadRequest -> Aff (err :: AWS.RequestError | eff) InitiateLayerUploadResponse
initiateLayerUpload = AWS.request serviceName "InitiateLayerUpload" 


-- | <p>Lists all the image IDs for a given repository.</p> <p>You can filter images based on whether or not they are tagged by setting the <code>tagStatus</code> parameter to <code>TAGGED</code> or <code>UNTAGGED</code>. For example, you can filter your results to return only <code>UNTAGGED</code> images and then pipe that result to a <a>BatchDeleteImage</a> operation to delete them. Or, you can filter your results to return only <code>TAGGED</code> images to list all of the tags in your repository.</p>
listImages :: forall eff. ListImagesRequest -> Aff (err :: AWS.RequestError | eff) ListImagesResponse
listImages = AWS.request serviceName "ListImages" 


-- | <p>Creates or updates the image manifest and tags associated with an image.</p> <note> <p>This operation is used by the Amazon ECR proxy, and it is not intended for general use by customers for pulling and pushing images. In most cases, you should use the <code>docker</code> CLI to pull, tag, and push images.</p> </note>
putImage :: forall eff. PutImageRequest -> Aff (err :: AWS.RequestError | eff) PutImageResponse
putImage = AWS.request serviceName "PutImage" 


-- | <p>Creates or updates a lifecycle policy.</p>
putLifecyclePolicy :: forall eff. PutLifecyclePolicyRequest -> Aff (err :: AWS.RequestError | eff) PutLifecyclePolicyResponse
putLifecyclePolicy = AWS.request serviceName "PutLifecyclePolicy" 


-- | <p>Applies a repository policy on a specified repository to control access permissions.</p>
setRepositoryPolicy :: forall eff. SetRepositoryPolicyRequest -> Aff (err :: AWS.RequestError | eff) SetRepositoryPolicyResponse
setRepositoryPolicy = AWS.request serviceName "SetRepositoryPolicy" 


-- | <p>Starts a preview of the specified lifecycle policy. This allows you to see the results before creating the lifecycle policy.</p>
startLifecyclePolicyPreview :: forall eff. StartLifecyclePolicyPreviewRequest -> Aff (err :: AWS.RequestError | eff) StartLifecyclePolicyPreviewResponse
startLifecyclePolicyPreview = AWS.request serviceName "StartLifecyclePolicyPreview" 


-- | <p>Uploads an image layer part to Amazon ECR.</p> <note> <p>This operation is used by the Amazon ECR proxy, and it is not intended for general use by customers for pulling and pushing images. In most cases, you should use the <code>docker</code> CLI to pull, tag, and push images.</p> </note>
uploadLayerPart :: forall eff. UploadLayerPartRequest -> Aff (err :: AWS.RequestError | eff) UploadLayerPartResponse
uploadLayerPart = AWS.request serviceName "UploadLayerPart" 


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _


-- | <p>An object representing authorization data for an Amazon ECR registry.</p>
newtype AuthorizationData = AuthorizationData 
  { "AuthorizationToken'" :: NullOrUndefined (Base64)
  , "ExpiresAt'" :: NullOrUndefined (ExpirationTimestamp)
  , "ProxyEndpoint'" :: NullOrUndefined (ProxyEndpoint)
  }
derive instance newtypeAuthorizationData :: Newtype AuthorizationData _


newtype AuthorizationDataList = AuthorizationDataList (Array AuthorizationData)
derive instance newtypeAuthorizationDataList :: Newtype AuthorizationDataList _


newtype Base64 = Base64 String
derive instance newtypeBase64 :: Newtype Base64 _


newtype BatchCheckLayerAvailabilityRequest = BatchCheckLayerAvailabilityRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "LayerDigests'" :: (BatchedOperationLayerDigestList)
  }
derive instance newtypeBatchCheckLayerAvailabilityRequest :: Newtype BatchCheckLayerAvailabilityRequest _


newtype BatchCheckLayerAvailabilityResponse = BatchCheckLayerAvailabilityResponse 
  { "Layers'" :: NullOrUndefined (LayerList)
  , "Failures'" :: NullOrUndefined (LayerFailureList)
  }
derive instance newtypeBatchCheckLayerAvailabilityResponse :: Newtype BatchCheckLayerAvailabilityResponse _


-- | <p>Deletes specified images within a specified repository. Images are specified with either the <code>imageTag</code> or <code>imageDigest</code>.</p>
newtype BatchDeleteImageRequest = BatchDeleteImageRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "ImageIds'" :: (ImageIdentifierList)
  }
derive instance newtypeBatchDeleteImageRequest :: Newtype BatchDeleteImageRequest _


newtype BatchDeleteImageResponse = BatchDeleteImageResponse 
  { "ImageIds'" :: NullOrUndefined (ImageIdentifierList)
  , "Failures'" :: NullOrUndefined (ImageFailureList)
  }
derive instance newtypeBatchDeleteImageResponse :: Newtype BatchDeleteImageResponse _


newtype BatchGetImageRequest = BatchGetImageRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "ImageIds'" :: (ImageIdentifierList)
  , "AcceptedMediaTypes'" :: NullOrUndefined (MediaTypeList)
  }
derive instance newtypeBatchGetImageRequest :: Newtype BatchGetImageRequest _


newtype BatchGetImageResponse = BatchGetImageResponse 
  { "Images'" :: NullOrUndefined (ImageList)
  , "Failures'" :: NullOrUndefined (ImageFailureList)
  }
derive instance newtypeBatchGetImageResponse :: Newtype BatchGetImageResponse _


newtype BatchedOperationLayerDigest = BatchedOperationLayerDigest String
derive instance newtypeBatchedOperationLayerDigest :: Newtype BatchedOperationLayerDigest _


newtype BatchedOperationLayerDigestList = BatchedOperationLayerDigestList (Array BatchedOperationLayerDigest)
derive instance newtypeBatchedOperationLayerDigestList :: Newtype BatchedOperationLayerDigestList _


newtype CompleteLayerUploadRequest = CompleteLayerUploadRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "UploadId'" :: (UploadId)
  , "LayerDigests'" :: (LayerDigestList)
  }
derive instance newtypeCompleteLayerUploadRequest :: Newtype CompleteLayerUploadRequest _


newtype CompleteLayerUploadResponse = CompleteLayerUploadResponse 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "UploadId'" :: NullOrUndefined (UploadId)
  , "LayerDigest'" :: NullOrUndefined (LayerDigest)
  }
derive instance newtypeCompleteLayerUploadResponse :: Newtype CompleteLayerUploadResponse _


newtype CreateRepositoryRequest = CreateRepositoryRequest 
  { "RepositoryName'" :: (RepositoryName)
  }
derive instance newtypeCreateRepositoryRequest :: Newtype CreateRepositoryRequest _


newtype CreateRepositoryResponse = CreateRepositoryResponse 
  { "Repository'" :: NullOrUndefined (Repository)
  }
derive instance newtypeCreateRepositoryResponse :: Newtype CreateRepositoryResponse _


newtype CreationTimestamp = CreationTimestamp Number
derive instance newtypeCreationTimestamp :: Newtype CreationTimestamp _


newtype DeleteLifecyclePolicyRequest = DeleteLifecyclePolicyRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  }
derive instance newtypeDeleteLifecyclePolicyRequest :: Newtype DeleteLifecyclePolicyRequest _


newtype DeleteLifecyclePolicyResponse = DeleteLifecyclePolicyResponse 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText)
  , "LastEvaluatedAt'" :: NullOrUndefined (EvaluationTimestamp)
  }
derive instance newtypeDeleteLifecyclePolicyResponse :: Newtype DeleteLifecyclePolicyResponse _


newtype DeleteRepositoryPolicyRequest = DeleteRepositoryPolicyRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  }
derive instance newtypeDeleteRepositoryPolicyRequest :: Newtype DeleteRepositoryPolicyRequest _


newtype DeleteRepositoryPolicyResponse = DeleteRepositoryPolicyResponse 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "PolicyText'" :: NullOrUndefined (RepositoryPolicyText)
  }
derive instance newtypeDeleteRepositoryPolicyResponse :: Newtype DeleteRepositoryPolicyResponse _


newtype DeleteRepositoryRequest = DeleteRepositoryRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "Force'" :: NullOrUndefined (ForceFlag)
  }
derive instance newtypeDeleteRepositoryRequest :: Newtype DeleteRepositoryRequest _


newtype DeleteRepositoryResponse = DeleteRepositoryResponse 
  { "Repository'" :: NullOrUndefined (Repository)
  }
derive instance newtypeDeleteRepositoryResponse :: Newtype DeleteRepositoryResponse _


-- | <p>An object representing a filter on a <a>DescribeImages</a> operation.</p>
newtype DescribeImagesFilter = DescribeImagesFilter 
  { "TagStatus'" :: NullOrUndefined (TagStatus)
  }
derive instance newtypeDescribeImagesFilter :: Newtype DescribeImagesFilter _


newtype DescribeImagesRequest = DescribeImagesRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "ImageIds'" :: NullOrUndefined (ImageIdentifierList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  , "Filter'" :: NullOrUndefined (DescribeImagesFilter)
  }
derive instance newtypeDescribeImagesRequest :: Newtype DescribeImagesRequest _


newtype DescribeImagesResponse = DescribeImagesResponse 
  { "ImageDetails'" :: NullOrUndefined (ImageDetailList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeImagesResponse :: Newtype DescribeImagesResponse _


newtype DescribeRepositoriesRequest = DescribeRepositoriesRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryNames'" :: NullOrUndefined (RepositoryNameList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeDescribeRepositoriesRequest :: Newtype DescribeRepositoriesRequest _


newtype DescribeRepositoriesResponse = DescribeRepositoriesResponse 
  { "Repositories'" :: NullOrUndefined (RepositoryList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeRepositoriesResponse :: Newtype DescribeRepositoriesResponse _


-- | <p>The specified layer upload does not contain any layer parts.</p>
newtype EmptyUploadException = EmptyUploadException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeEmptyUploadException :: Newtype EmptyUploadException _


newtype EvaluationTimestamp = EvaluationTimestamp Number
derive instance newtypeEvaluationTimestamp :: Newtype EvaluationTimestamp _


newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _


newtype ExpirationTimestamp = ExpirationTimestamp Number
derive instance newtypeExpirationTimestamp :: Newtype ExpirationTimestamp _


newtype ForceFlag = ForceFlag Boolean
derive instance newtypeForceFlag :: Newtype ForceFlag _


newtype GetAuthorizationTokenRegistryIdList = GetAuthorizationTokenRegistryIdList (Array RegistryId)
derive instance newtypeGetAuthorizationTokenRegistryIdList :: Newtype GetAuthorizationTokenRegistryIdList _


newtype GetAuthorizationTokenRequest = GetAuthorizationTokenRequest 
  { "RegistryIds'" :: NullOrUndefined (GetAuthorizationTokenRegistryIdList)
  }
derive instance newtypeGetAuthorizationTokenRequest :: Newtype GetAuthorizationTokenRequest _


newtype GetAuthorizationTokenResponse = GetAuthorizationTokenResponse 
  { "AuthorizationData'" :: NullOrUndefined (AuthorizationDataList)
  }
derive instance newtypeGetAuthorizationTokenResponse :: Newtype GetAuthorizationTokenResponse _


newtype GetDownloadUrlForLayerRequest = GetDownloadUrlForLayerRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "LayerDigest'" :: (LayerDigest)
  }
derive instance newtypeGetDownloadUrlForLayerRequest :: Newtype GetDownloadUrlForLayerRequest _


newtype GetDownloadUrlForLayerResponse = GetDownloadUrlForLayerResponse 
  { "DownloadUrl'" :: NullOrUndefined (Url)
  , "LayerDigest'" :: NullOrUndefined (LayerDigest)
  }
derive instance newtypeGetDownloadUrlForLayerResponse :: Newtype GetDownloadUrlForLayerResponse _


newtype GetLifecyclePolicyPreviewRequest = GetLifecyclePolicyPreviewRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "ImageIds'" :: NullOrUndefined (ImageIdentifierList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  , "Filter'" :: NullOrUndefined (LifecyclePolicyPreviewFilter)
  }
derive instance newtypeGetLifecyclePolicyPreviewRequest :: Newtype GetLifecyclePolicyPreviewRequest _


newtype GetLifecyclePolicyPreviewResponse = GetLifecyclePolicyPreviewResponse 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText)
  , "Status'" :: NullOrUndefined (LifecyclePolicyPreviewStatus)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "PreviewResults'" :: NullOrUndefined (LifecyclePolicyPreviewResultList)
  , "Summary'" :: NullOrUndefined (LifecyclePolicyPreviewSummary)
  }
derive instance newtypeGetLifecyclePolicyPreviewResponse :: Newtype GetLifecyclePolicyPreviewResponse _


newtype GetLifecyclePolicyRequest = GetLifecyclePolicyRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  }
derive instance newtypeGetLifecyclePolicyRequest :: Newtype GetLifecyclePolicyRequest _


newtype GetLifecyclePolicyResponse = GetLifecyclePolicyResponse 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText)
  , "LastEvaluatedAt'" :: NullOrUndefined (EvaluationTimestamp)
  }
derive instance newtypeGetLifecyclePolicyResponse :: Newtype GetLifecyclePolicyResponse _


newtype GetRepositoryPolicyRequest = GetRepositoryPolicyRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  }
derive instance newtypeGetRepositoryPolicyRequest :: Newtype GetRepositoryPolicyRequest _


newtype GetRepositoryPolicyResponse = GetRepositoryPolicyResponse 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "PolicyText'" :: NullOrUndefined (RepositoryPolicyText)
  }
derive instance newtypeGetRepositoryPolicyResponse :: Newtype GetRepositoryPolicyResponse _


-- | <p>An object representing an Amazon ECR image.</p>
newtype Image = Image 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "ImageId'" :: NullOrUndefined (ImageIdentifier)
  , "ImageManifest'" :: NullOrUndefined (ImageManifest)
  }
derive instance newtypeImage :: Newtype Image _


newtype ImageActionType = ImageActionType String
derive instance newtypeImageActionType :: Newtype ImageActionType _


-- | <p>The specified image has already been pushed, and there were no changes to the manifest or image tag after the last push.</p>
newtype ImageAlreadyExistsException = ImageAlreadyExistsException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeImageAlreadyExistsException :: Newtype ImageAlreadyExistsException _


newtype ImageCount = ImageCount Int
derive instance newtypeImageCount :: Newtype ImageCount _


-- | <p>An object that describes an image returned by a <a>DescribeImages</a> operation.</p>
newtype ImageDetail = ImageDetail 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "ImageDigest'" :: NullOrUndefined (ImageDigest)
  , "ImageTags'" :: NullOrUndefined (ImageTagList)
  , "ImageSizeInBytes'" :: NullOrUndefined (ImageSizeInBytes)
  , "ImagePushedAt'" :: NullOrUndefined (PushTimestamp)
  }
derive instance newtypeImageDetail :: Newtype ImageDetail _


newtype ImageDetailList = ImageDetailList (Array ImageDetail)
derive instance newtypeImageDetailList :: Newtype ImageDetailList _


newtype ImageDigest = ImageDigest String
derive instance newtypeImageDigest :: Newtype ImageDigest _


-- | <p>An object representing an Amazon ECR image failure.</p>
newtype ImageFailure = ImageFailure 
  { "ImageId'" :: NullOrUndefined (ImageIdentifier)
  , "FailureCode'" :: NullOrUndefined (ImageFailureCode)
  , "FailureReason'" :: NullOrUndefined (ImageFailureReason)
  }
derive instance newtypeImageFailure :: Newtype ImageFailure _


newtype ImageFailureCode = ImageFailureCode String
derive instance newtypeImageFailureCode :: Newtype ImageFailureCode _


newtype ImageFailureList = ImageFailureList (Array ImageFailure)
derive instance newtypeImageFailureList :: Newtype ImageFailureList _


newtype ImageFailureReason = ImageFailureReason String
derive instance newtypeImageFailureReason :: Newtype ImageFailureReason _


-- | <p>An object with identifying information for an Amazon ECR image.</p>
newtype ImageIdentifier = ImageIdentifier 
  { "ImageDigest'" :: NullOrUndefined (ImageDigest)
  , "ImageTag'" :: NullOrUndefined (ImageTag)
  }
derive instance newtypeImageIdentifier :: Newtype ImageIdentifier _


newtype ImageIdentifierList = ImageIdentifierList (Array ImageIdentifier)
derive instance newtypeImageIdentifierList :: Newtype ImageIdentifierList _


newtype ImageList = ImageList (Array Image)
derive instance newtypeImageList :: Newtype ImageList _


newtype ImageManifest = ImageManifest String
derive instance newtypeImageManifest :: Newtype ImageManifest _


-- | <p>The image requested does not exist in the specified repository.</p>
newtype ImageNotFoundException = ImageNotFoundException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeImageNotFoundException :: Newtype ImageNotFoundException _


newtype ImageSizeInBytes = ImageSizeInBytes Number
derive instance newtypeImageSizeInBytes :: Newtype ImageSizeInBytes _


newtype ImageTag = ImageTag String
derive instance newtypeImageTag :: Newtype ImageTag _


newtype ImageTagList = ImageTagList (Array ImageTag)
derive instance newtypeImageTagList :: Newtype ImageTagList _


newtype InitiateLayerUploadRequest = InitiateLayerUploadRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  }
derive instance newtypeInitiateLayerUploadRequest :: Newtype InitiateLayerUploadRequest _


newtype InitiateLayerUploadResponse = InitiateLayerUploadResponse 
  { "UploadId'" :: NullOrUndefined (UploadId)
  , "PartSize'" :: NullOrUndefined (PartSize)
  }
derive instance newtypeInitiateLayerUploadResponse :: Newtype InitiateLayerUploadResponse _


-- | <p>The layer digest calculation performed by Amazon ECR upon receipt of the image layer does not match the digest specified.</p>
newtype InvalidLayerException = InvalidLayerException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidLayerException :: Newtype InvalidLayerException _


-- | <p>The layer part size is not valid, or the first byte specified is not consecutive to the last byte of a previous layer part upload.</p>
newtype InvalidLayerPartException = InvalidLayerPartException 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "UploadId'" :: NullOrUndefined (UploadId)
  , "LastValidByteReceived'" :: NullOrUndefined (PartSize)
  , "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidLayerPartException :: Newtype InvalidLayerPartException _


-- | <p>The specified parameter is invalid. Review the available parameters for the API request.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _


-- | <p>An object representing an Amazon ECR image layer.</p>
newtype Layer = Layer 
  { "LayerDigest'" :: NullOrUndefined (LayerDigest)
  , "LayerAvailability'" :: NullOrUndefined (LayerAvailability)
  , "LayerSize'" :: NullOrUndefined (LayerSizeInBytes)
  , "MediaType'" :: NullOrUndefined (MediaType)
  }
derive instance newtypeLayer :: Newtype Layer _


-- | <p>The image layer already exists in the associated repository.</p>
newtype LayerAlreadyExistsException = LayerAlreadyExistsException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeLayerAlreadyExistsException :: Newtype LayerAlreadyExistsException _


newtype LayerAvailability = LayerAvailability String
derive instance newtypeLayerAvailability :: Newtype LayerAvailability _


newtype LayerDigest = LayerDigest String
derive instance newtypeLayerDigest :: Newtype LayerDigest _


newtype LayerDigestList = LayerDigestList (Array LayerDigest)
derive instance newtypeLayerDigestList :: Newtype LayerDigestList _


-- | <p>An object representing an Amazon ECR image layer failure.</p>
newtype LayerFailure = LayerFailure 
  { "LayerDigest'" :: NullOrUndefined (BatchedOperationLayerDigest)
  , "FailureCode'" :: NullOrUndefined (LayerFailureCode)
  , "FailureReason'" :: NullOrUndefined (LayerFailureReason)
  }
derive instance newtypeLayerFailure :: Newtype LayerFailure _


newtype LayerFailureCode = LayerFailureCode String
derive instance newtypeLayerFailureCode :: Newtype LayerFailureCode _


newtype LayerFailureList = LayerFailureList (Array LayerFailure)
derive instance newtypeLayerFailureList :: Newtype LayerFailureList _


newtype LayerFailureReason = LayerFailureReason String
derive instance newtypeLayerFailureReason :: Newtype LayerFailureReason _


-- | <p>The specified layer is not available because it is not associated with an image. Unassociated image layers may be cleaned up at any time.</p>
newtype LayerInaccessibleException = LayerInaccessibleException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeLayerInaccessibleException :: Newtype LayerInaccessibleException _


newtype LayerList = LayerList (Array Layer)
derive instance newtypeLayerList :: Newtype LayerList _


newtype LayerPartBlob = LayerPartBlob String
derive instance newtypeLayerPartBlob :: Newtype LayerPartBlob _


-- | <p>Layer parts must be at least 5 MiB in size.</p>
newtype LayerPartTooSmallException = LayerPartTooSmallException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeLayerPartTooSmallException :: Newtype LayerPartTooSmallException _


newtype LayerSizeInBytes = LayerSizeInBytes Number
derive instance newtypeLayerSizeInBytes :: Newtype LayerSizeInBytes _


-- | <p>The specified layers could not be found, or the specified layer is not valid for this repository.</p>
newtype LayersNotFoundException = LayersNotFoundException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeLayersNotFoundException :: Newtype LayersNotFoundException _


-- | <p>The lifecycle policy could not be found, and no policy is set to the repository.</p>
newtype LifecyclePolicyNotFoundException = LifecyclePolicyNotFoundException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeLifecyclePolicyNotFoundException :: Newtype LifecyclePolicyNotFoundException _


-- | <p>The filter for the lifecycle policy preview.</p>
newtype LifecyclePolicyPreviewFilter = LifecyclePolicyPreviewFilter 
  { "TagStatus'" :: NullOrUndefined (TagStatus)
  }
derive instance newtypeLifecyclePolicyPreviewFilter :: Newtype LifecyclePolicyPreviewFilter _


-- | <p>The previous lifecycle policy preview request has not completed. Please try again later.</p>
newtype LifecyclePolicyPreviewInProgressException = LifecyclePolicyPreviewInProgressException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeLifecyclePolicyPreviewInProgressException :: Newtype LifecyclePolicyPreviewInProgressException _


-- | <p>There is no dry run for this repository.</p>
newtype LifecyclePolicyPreviewNotFoundException = LifecyclePolicyPreviewNotFoundException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeLifecyclePolicyPreviewNotFoundException :: Newtype LifecyclePolicyPreviewNotFoundException _


-- | <p>The result of the lifecycle policy preview.</p>
newtype LifecyclePolicyPreviewResult = LifecyclePolicyPreviewResult 
  { "ImageTags'" :: NullOrUndefined (ImageTagList)
  , "ImageDigest'" :: NullOrUndefined (ImageDigest)
  , "ImagePushedAt'" :: NullOrUndefined (PushTimestamp)
  , "Action'" :: NullOrUndefined (LifecyclePolicyRuleAction)
  , "AppliedRulePriority'" :: NullOrUndefined (LifecyclePolicyRulePriority)
  }
derive instance newtypeLifecyclePolicyPreviewResult :: Newtype LifecyclePolicyPreviewResult _


newtype LifecyclePolicyPreviewResultList = LifecyclePolicyPreviewResultList (Array LifecyclePolicyPreviewResult)
derive instance newtypeLifecyclePolicyPreviewResultList :: Newtype LifecyclePolicyPreviewResultList _


newtype LifecyclePolicyPreviewStatus = LifecyclePolicyPreviewStatus String
derive instance newtypeLifecyclePolicyPreviewStatus :: Newtype LifecyclePolicyPreviewStatus _


-- | <p>The summary of the lifecycle policy preview request.</p>
newtype LifecyclePolicyPreviewSummary = LifecyclePolicyPreviewSummary 
  { "ExpiringImageTotalCount'" :: NullOrUndefined (ImageCount)
  }
derive instance newtypeLifecyclePolicyPreviewSummary :: Newtype LifecyclePolicyPreviewSummary _


-- | <p>The type of action to be taken.</p>
newtype LifecyclePolicyRuleAction = LifecyclePolicyRuleAction 
  { "Type'" :: NullOrUndefined (ImageActionType)
  }
derive instance newtypeLifecyclePolicyRuleAction :: Newtype LifecyclePolicyRuleAction _


newtype LifecyclePolicyRulePriority = LifecyclePolicyRulePriority Int
derive instance newtypeLifecyclePolicyRulePriority :: Newtype LifecyclePolicyRulePriority _


newtype LifecyclePolicyText = LifecyclePolicyText String
derive instance newtypeLifecyclePolicyText :: Newtype LifecyclePolicyText _


-- | <p>The operation did not succeed because it would have exceeded a service limit for your account. For more information, see <a href="http://docs.aws.amazon.com/AmazonECR/latest/userguide/service_limits.html">Amazon ECR Default Service Limits</a> in the Amazon EC2 Container Registry User Guide.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


-- | <p>An object representing a filter on a <a>ListImages</a> operation.</p>
newtype ListImagesFilter = ListImagesFilter 
  { "TagStatus'" :: NullOrUndefined (TagStatus)
  }
derive instance newtypeListImagesFilter :: Newtype ListImagesFilter _


newtype ListImagesRequest = ListImagesRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  , "Filter'" :: NullOrUndefined (ListImagesFilter)
  }
derive instance newtypeListImagesRequest :: Newtype ListImagesRequest _


newtype ListImagesResponse = ListImagesResponse 
  { "ImageIds'" :: NullOrUndefined (ImageIdentifierList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListImagesResponse :: Newtype ListImagesResponse _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype MediaType = MediaType String
derive instance newtypeMediaType :: Newtype MediaType _


newtype MediaTypeList = MediaTypeList (Array MediaType)
derive instance newtypeMediaTypeList :: Newtype MediaTypeList _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


newtype PartSize = PartSize Number
derive instance newtypePartSize :: Newtype PartSize _


newtype ProxyEndpoint = ProxyEndpoint String
derive instance newtypeProxyEndpoint :: Newtype ProxyEndpoint _


newtype PushTimestamp = PushTimestamp Number
derive instance newtypePushTimestamp :: Newtype PushTimestamp _


newtype PutImageRequest = PutImageRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "ImageManifest'" :: (ImageManifest)
  , "ImageTag'" :: NullOrUndefined (ImageTag)
  }
derive instance newtypePutImageRequest :: Newtype PutImageRequest _


newtype PutImageResponse = PutImageResponse 
  { "Image'" :: NullOrUndefined (Image)
  }
derive instance newtypePutImageResponse :: Newtype PutImageResponse _


newtype PutLifecyclePolicyRequest = PutLifecyclePolicyRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "LifecyclePolicyText'" :: (LifecyclePolicyText)
  }
derive instance newtypePutLifecyclePolicyRequest :: Newtype PutLifecyclePolicyRequest _


newtype PutLifecyclePolicyResponse = PutLifecyclePolicyResponse 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText)
  }
derive instance newtypePutLifecyclePolicyResponse :: Newtype PutLifecyclePolicyResponse _


newtype RegistryId = RegistryId String
derive instance newtypeRegistryId :: Newtype RegistryId _


-- | <p>An object representing a repository.</p>
newtype Repository = Repository 
  { "RepositoryArn'" :: NullOrUndefined (Arn)
  , "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "RepositoryUri'" :: NullOrUndefined (Url)
  , "CreatedAt'" :: NullOrUndefined (CreationTimestamp)
  }
derive instance newtypeRepository :: Newtype Repository _


-- | <p>The specified repository already exists in the specified registry.</p>
newtype RepositoryAlreadyExistsException = RepositoryAlreadyExistsException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeRepositoryAlreadyExistsException :: Newtype RepositoryAlreadyExistsException _


newtype RepositoryList = RepositoryList (Array Repository)
derive instance newtypeRepositoryList :: Newtype RepositoryList _


newtype RepositoryName = RepositoryName String
derive instance newtypeRepositoryName :: Newtype RepositoryName _


newtype RepositoryNameList = RepositoryNameList (Array RepositoryName)
derive instance newtypeRepositoryNameList :: Newtype RepositoryNameList _


-- | <p>The specified repository contains images. To delete a repository that contains images, you must force the deletion with the <code>force</code> parameter.</p>
newtype RepositoryNotEmptyException = RepositoryNotEmptyException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeRepositoryNotEmptyException :: Newtype RepositoryNotEmptyException _


-- | <p>The specified repository could not be found. Check the spelling of the specified repository and ensure that you are performing operations on the correct registry.</p>
newtype RepositoryNotFoundException = RepositoryNotFoundException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeRepositoryNotFoundException :: Newtype RepositoryNotFoundException _


-- | <p>The specified repository and registry combination does not have an associated repository policy.</p>
newtype RepositoryPolicyNotFoundException = RepositoryPolicyNotFoundException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeRepositoryPolicyNotFoundException :: Newtype RepositoryPolicyNotFoundException _


newtype RepositoryPolicyText = RepositoryPolicyText String
derive instance newtypeRepositoryPolicyText :: Newtype RepositoryPolicyText _


-- | <p>These errors are usually caused by a server-side issue.</p>
newtype ServerException = ServerException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeServerException :: Newtype ServerException _


newtype SetRepositoryPolicyRequest = SetRepositoryPolicyRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "PolicyText'" :: (RepositoryPolicyText)
  , "Force'" :: NullOrUndefined (ForceFlag)
  }
derive instance newtypeSetRepositoryPolicyRequest :: Newtype SetRepositoryPolicyRequest _


newtype SetRepositoryPolicyResponse = SetRepositoryPolicyResponse 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "PolicyText'" :: NullOrUndefined (RepositoryPolicyText)
  }
derive instance newtypeSetRepositoryPolicyResponse :: Newtype SetRepositoryPolicyResponse _


newtype StartLifecyclePolicyPreviewRequest = StartLifecyclePolicyPreviewRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText)
  }
derive instance newtypeStartLifecyclePolicyPreviewRequest :: Newtype StartLifecyclePolicyPreviewRequest _


newtype StartLifecyclePolicyPreviewResponse = StartLifecyclePolicyPreviewResponse 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "LifecyclePolicyText'" :: NullOrUndefined (LifecyclePolicyText)
  , "Status'" :: NullOrUndefined (LifecyclePolicyPreviewStatus)
  }
derive instance newtypeStartLifecyclePolicyPreviewResponse :: Newtype StartLifecyclePolicyPreviewResponse _


newtype TagStatus = TagStatus String
derive instance newtypeTagStatus :: Newtype TagStatus _


newtype UploadId = UploadId String
derive instance newtypeUploadId :: Newtype UploadId _


newtype UploadLayerPartRequest = UploadLayerPartRequest 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: (RepositoryName)
  , "UploadId'" :: (UploadId)
  , "PartFirstByte'" :: (PartSize)
  , "PartLastByte'" :: (PartSize)
  , "LayerPartBlob'" :: (LayerPartBlob)
  }
derive instance newtypeUploadLayerPartRequest :: Newtype UploadLayerPartRequest _


newtype UploadLayerPartResponse = UploadLayerPartResponse 
  { "RegistryId'" :: NullOrUndefined (RegistryId)
  , "RepositoryName'" :: NullOrUndefined (RepositoryName)
  , "UploadId'" :: NullOrUndefined (UploadId)
  , "LastByteReceived'" :: NullOrUndefined (PartSize)
  }
derive instance newtypeUploadLayerPartResponse :: Newtype UploadLayerPartResponse _


-- | <p>The upload could not be found, or the specified upload id is not valid for this repository.</p>
newtype UploadNotFoundException = UploadNotFoundException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeUploadNotFoundException :: Newtype UploadNotFoundException _


newtype Url = Url String
derive instance newtypeUrl :: Newtype Url _
