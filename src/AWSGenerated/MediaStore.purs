

-- | <p>An AWS Elemental MediaStore container is a namespace that holds folders and objects. You use a container endpoint to create, read, and delete objects. </p>
module AWS.MediaStore where

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

serviceName = "MediaStore" :: String


-- | <p>Creates a storage container to hold objects. A container is similar to a bucket in the Amazon S3 service.</p>
createContainer :: forall eff. CreateContainerInput -> Aff (exception :: EXCEPTION | eff) CreateContainerOutput
createContainer = Request.request serviceName "createContainer" 


-- | <p>Deletes the specified container. Before you make a <code>DeleteContainer</code> request, delete any objects in the container or in any folders in the container. You can delete only empty containers. </p>
deleteContainer :: forall eff. DeleteContainerInput -> Aff (exception :: EXCEPTION | eff) DeleteContainerOutput
deleteContainer = Request.request serviceName "deleteContainer" 


-- | <p>Deletes the access policy that is associated with the specified container.</p>
deleteContainerPolicy :: forall eff. DeleteContainerPolicyInput -> Aff (exception :: EXCEPTION | eff) DeleteContainerPolicyOutput
deleteContainerPolicy = Request.request serviceName "deleteContainerPolicy" 


-- | <p>Deletes the cross-origin resource sharing (CORS) configuration information that is set for the container.</p> <p>To use this operation, you must have permission to perform the <code>MediaStore:DeleteCorsPolicy</code> action. The container owner has this permission by default and can grant this permission to others.</p>
deleteCorsPolicy :: forall eff. DeleteCorsPolicyInput -> Aff (exception :: EXCEPTION | eff) DeleteCorsPolicyOutput
deleteCorsPolicy = Request.request serviceName "deleteCorsPolicy" 


-- | <p>Retrieves the properties of the requested container. This request is commonly used to retrieve the endpoint of a container. An endpoint is a value assigned by the service when a new container is created. A container's endpoint does not change after it has been assigned. The <code>DescribeContainer</code> request returns a single <code>Container</code> object based on <code>ContainerName</code>. To return all <code>Container</code> objects that are associated with a specified AWS account, use <a>ListContainers</a>.</p>
describeContainer :: forall eff. DescribeContainerInput -> Aff (exception :: EXCEPTION | eff) DescribeContainerOutput
describeContainer = Request.request serviceName "describeContainer" 


-- | <p>Retrieves the access policy for the specified container. For information about the data that is included in an access policy, see the <a href="https://aws.amazon.com/documentation/iam/">AWS Identity and Access Management User Guide</a>.</p>
getContainerPolicy :: forall eff. GetContainerPolicyInput -> Aff (exception :: EXCEPTION | eff) GetContainerPolicyOutput
getContainerPolicy = Request.request serviceName "getContainerPolicy" 


-- | <p>Returns the cross-origin resource sharing (CORS) configuration information that is set for the container.</p> <p>To use this operation, you must have permission to perform the <code>MediaStore:GetCorsPolicy</code> action. By default, the container owner has this permission and can grant it to others.</p>
getCorsPolicy :: forall eff. GetCorsPolicyInput -> Aff (exception :: EXCEPTION | eff) GetCorsPolicyOutput
getCorsPolicy = Request.request serviceName "getCorsPolicy" 


-- | <p>Lists the properties of all containers in AWS Elemental MediaStore. </p> <p>You can query to receive all the containers in one response. Or you can include the <code>MaxResults</code> parameter to receive a limited number of containers in each response. In this case, the response includes a token. To get the next set of containers, send the command again, this time with the <code>NextToken</code> parameter (with the returned token as its value). The next set of responses appears, with a token if there are still more containers to receive. </p> <p>See also <a>DescribeContainer</a>, which gets the properties of one container. </p>
listContainers :: forall eff. ListContainersInput -> Aff (exception :: EXCEPTION | eff) ListContainersOutput
listContainers = Request.request serviceName "listContainers" 


-- | <p>Creates an access policy for the specified container to restrict the users and clients that can access it. For information about the data that is included in an access policy, see the <a href="https://aws.amazon.com/documentation/iam/">AWS Identity and Access Management User Guide</a>.</p> <p>For this release of the REST API, you can create only one policy for a container. If you enter <code>PutContainerPolicy</code> twice, the second command modifies the existing policy. </p>
putContainerPolicy :: forall eff. PutContainerPolicyInput -> Aff (exception :: EXCEPTION | eff) PutContainerPolicyOutput
putContainerPolicy = Request.request serviceName "putContainerPolicy" 


-- | <p>Sets the cross-origin resource sharing (CORS) configuration on a container so that the container can service cross-origin requests. For example, you might want to enable a request whose origin is http://www.example.com to access your AWS Elemental MediaStore container at my.example.container.com by using the browser's XMLHttpRequest capability.</p> <p>To enable CORS on a container, you attach a CORS policy to the container. In the CORS policy, you configure rules that identify origins and the HTTP methods that can be executed on your container. The policy can contain up to 398,000 characters. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.</p>
putCorsPolicy :: forall eff. PutCorsPolicyInput -> Aff (exception :: EXCEPTION | eff) PutCorsPolicyOutput
putCorsPolicy = Request.request serviceName "putCorsPolicy" 


newtype AllowedHeaders = AllowedHeaders (Array Header)
derive instance newtypeAllowedHeaders :: Newtype AllowedHeaders _
derive instance repGenericAllowedHeaders :: Generic AllowedHeaders _
instance showAllowedHeaders :: Show AllowedHeaders where
  show = genericShow
instance decodeAllowedHeaders :: Decode AllowedHeaders where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedHeaders :: Encode AllowedHeaders where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllowedMethods = AllowedMethods (Array MethodName)
derive instance newtypeAllowedMethods :: Newtype AllowedMethods _
derive instance repGenericAllowedMethods :: Generic AllowedMethods _
instance showAllowedMethods :: Show AllowedMethods where
  show = genericShow
instance decodeAllowedMethods :: Decode AllowedMethods where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedMethods :: Encode AllowedMethods where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllowedOrigins = AllowedOrigins (Array Origin)
derive instance newtypeAllowedOrigins :: Newtype AllowedOrigins _
derive instance repGenericAllowedOrigins :: Generic AllowedOrigins _
instance showAllowedOrigins :: Show AllowedOrigins where
  show = genericShow
instance decodeAllowedOrigins :: Decode AllowedOrigins where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedOrigins :: Encode AllowedOrigins where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This section describes operations that you can perform on an AWS Elemental MediaStore container.</p>
newtype Container = Container 
  { "Endpoint" :: NullOrUndefined.NullOrUndefined (Endpoint)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (TimeStamp)
  , "ARN" :: NullOrUndefined.NullOrUndefined (ContainerARN)
  , "Name" :: NullOrUndefined.NullOrUndefined (ContainerName)
  , "Status" :: NullOrUndefined.NullOrUndefined (ContainerStatus)
  }
derive instance newtypeContainer :: Newtype Container _
derive instance repGenericContainer :: Generic Container _
instance showContainer :: Show Container where
  show = genericShow
instance decodeContainer :: Decode Container where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainer :: Encode Container where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContainerARN = ContainerARN String
derive instance newtypeContainerARN :: Newtype ContainerARN _
derive instance repGenericContainerARN :: Generic ContainerARN _
instance showContainerARN :: Show ContainerARN where
  show = genericShow
instance decodeContainerARN :: Decode ContainerARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerARN :: Encode ContainerARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Resource already exists or is being updated.</p>
newtype ContainerInUseException = ContainerInUseException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeContainerInUseException :: Newtype ContainerInUseException _
derive instance repGenericContainerInUseException :: Generic ContainerInUseException _
instance showContainerInUseException :: Show ContainerInUseException where
  show = genericShow
instance decodeContainerInUseException :: Decode ContainerInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerInUseException :: Encode ContainerInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContainerList = ContainerList (Array Container)
derive instance newtypeContainerList :: Newtype ContainerList _
derive instance repGenericContainerList :: Generic ContainerList _
instance showContainerList :: Show ContainerList where
  show = genericShow
instance decodeContainerList :: Decode ContainerList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerList :: Encode ContainerList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContainerListLimit = ContainerListLimit Int
derive instance newtypeContainerListLimit :: Newtype ContainerListLimit _
derive instance repGenericContainerListLimit :: Generic ContainerListLimit _
instance showContainerListLimit :: Show ContainerListLimit where
  show = genericShow
instance decodeContainerListLimit :: Decode ContainerListLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerListLimit :: Encode ContainerListLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContainerName = ContainerName String
derive instance newtypeContainerName :: Newtype ContainerName _
derive instance repGenericContainerName :: Generic ContainerName _
instance showContainerName :: Show ContainerName where
  show = genericShow
instance decodeContainerName :: Decode ContainerName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerName :: Encode ContainerName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Could not perform an operation on a container that does not exist.</p>
newtype ContainerNotFoundException = ContainerNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeContainerNotFoundException :: Newtype ContainerNotFoundException _
derive instance repGenericContainerNotFoundException :: Generic ContainerNotFoundException _
instance showContainerNotFoundException :: Show ContainerNotFoundException where
  show = genericShow
instance decodeContainerNotFoundException :: Decode ContainerNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerNotFoundException :: Encode ContainerNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContainerPolicy = ContainerPolicy String
derive instance newtypeContainerPolicy :: Newtype ContainerPolicy _
derive instance repGenericContainerPolicy :: Generic ContainerPolicy _
instance showContainerPolicy :: Show ContainerPolicy where
  show = genericShow
instance decodeContainerPolicy :: Decode ContainerPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerPolicy :: Encode ContainerPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContainerStatus = ContainerStatus String
derive instance newtypeContainerStatus :: Newtype ContainerStatus _
derive instance repGenericContainerStatus :: Generic ContainerStatus _
instance showContainerStatus :: Show ContainerStatus where
  show = genericShow
instance decodeContainerStatus :: Decode ContainerStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerStatus :: Encode ContainerStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The CORS policy of the container. </p>
newtype CorsPolicy = CorsPolicy (Array CorsRule)
derive instance newtypeCorsPolicy :: Newtype CorsPolicy _
derive instance repGenericCorsPolicy :: Generic CorsPolicy _
instance showCorsPolicy :: Show CorsPolicy where
  show = genericShow
instance decodeCorsPolicy :: Decode CorsPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCorsPolicy :: Encode CorsPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Could not perform an operation on a policy that does not exist.</p>
newtype CorsPolicyNotFoundException = CorsPolicyNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeCorsPolicyNotFoundException :: Newtype CorsPolicyNotFoundException _
derive instance repGenericCorsPolicyNotFoundException :: Generic CorsPolicyNotFoundException _
instance showCorsPolicyNotFoundException :: Show CorsPolicyNotFoundException where
  show = genericShow
instance decodeCorsPolicyNotFoundException :: Decode CorsPolicyNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCorsPolicyNotFoundException :: Encode CorsPolicyNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A rule for a CORS policy. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.</p>
newtype CorsRule = CorsRule 
  { "AllowedOrigins" :: NullOrUndefined.NullOrUndefined (AllowedOrigins)
  , "AllowedMethods" :: NullOrUndefined.NullOrUndefined (AllowedMethods)
  , "AllowedHeaders" :: NullOrUndefined.NullOrUndefined (AllowedHeaders)
  , "MaxAgeSeconds" :: NullOrUndefined.NullOrUndefined (MaxAgeSeconds)
  , "ExposeHeaders" :: NullOrUndefined.NullOrUndefined (ExposeHeaders)
  }
derive instance newtypeCorsRule :: Newtype CorsRule _
derive instance repGenericCorsRule :: Generic CorsRule _
instance showCorsRule :: Show CorsRule where
  show = genericShow
instance decodeCorsRule :: Decode CorsRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCorsRule :: Encode CorsRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateContainerInput = CreateContainerInput 
  { "ContainerName" :: (ContainerName)
  }
derive instance newtypeCreateContainerInput :: Newtype CreateContainerInput _
derive instance repGenericCreateContainerInput :: Generic CreateContainerInput _
instance showCreateContainerInput :: Show CreateContainerInput where
  show = genericShow
instance decodeCreateContainerInput :: Decode CreateContainerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateContainerInput :: Encode CreateContainerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateContainerOutput = CreateContainerOutput 
  { "Container" :: (Container)
  }
derive instance newtypeCreateContainerOutput :: Newtype CreateContainerOutput _
derive instance repGenericCreateContainerOutput :: Generic CreateContainerOutput _
instance showCreateContainerOutput :: Show CreateContainerOutput where
  show = genericShow
instance decodeCreateContainerOutput :: Decode CreateContainerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateContainerOutput :: Encode CreateContainerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteContainerInput = DeleteContainerInput 
  { "ContainerName" :: (ContainerName)
  }
derive instance newtypeDeleteContainerInput :: Newtype DeleteContainerInput _
derive instance repGenericDeleteContainerInput :: Generic DeleteContainerInput _
instance showDeleteContainerInput :: Show DeleteContainerInput where
  show = genericShow
instance decodeDeleteContainerInput :: Decode DeleteContainerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteContainerInput :: Encode DeleteContainerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteContainerOutput = DeleteContainerOutput Types.NoArguments
derive instance newtypeDeleteContainerOutput :: Newtype DeleteContainerOutput _
derive instance repGenericDeleteContainerOutput :: Generic DeleteContainerOutput _
instance showDeleteContainerOutput :: Show DeleteContainerOutput where
  show = genericShow
instance decodeDeleteContainerOutput :: Decode DeleteContainerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteContainerOutput :: Encode DeleteContainerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteContainerPolicyInput = DeleteContainerPolicyInput 
  { "ContainerName" :: (ContainerName)
  }
derive instance newtypeDeleteContainerPolicyInput :: Newtype DeleteContainerPolicyInput _
derive instance repGenericDeleteContainerPolicyInput :: Generic DeleteContainerPolicyInput _
instance showDeleteContainerPolicyInput :: Show DeleteContainerPolicyInput where
  show = genericShow
instance decodeDeleteContainerPolicyInput :: Decode DeleteContainerPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteContainerPolicyInput :: Encode DeleteContainerPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteContainerPolicyOutput = DeleteContainerPolicyOutput Types.NoArguments
derive instance newtypeDeleteContainerPolicyOutput :: Newtype DeleteContainerPolicyOutput _
derive instance repGenericDeleteContainerPolicyOutput :: Generic DeleteContainerPolicyOutput _
instance showDeleteContainerPolicyOutput :: Show DeleteContainerPolicyOutput where
  show = genericShow
instance decodeDeleteContainerPolicyOutput :: Decode DeleteContainerPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteContainerPolicyOutput :: Encode DeleteContainerPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCorsPolicyInput = DeleteCorsPolicyInput 
  { "ContainerName" :: (ContainerName)
  }
derive instance newtypeDeleteCorsPolicyInput :: Newtype DeleteCorsPolicyInput _
derive instance repGenericDeleteCorsPolicyInput :: Generic DeleteCorsPolicyInput _
instance showDeleteCorsPolicyInput :: Show DeleteCorsPolicyInput where
  show = genericShow
instance decodeDeleteCorsPolicyInput :: Decode DeleteCorsPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCorsPolicyInput :: Encode DeleteCorsPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCorsPolicyOutput = DeleteCorsPolicyOutput Types.NoArguments
derive instance newtypeDeleteCorsPolicyOutput :: Newtype DeleteCorsPolicyOutput _
derive instance repGenericDeleteCorsPolicyOutput :: Generic DeleteCorsPolicyOutput _
instance showDeleteCorsPolicyOutput :: Show DeleteCorsPolicyOutput where
  show = genericShow
instance decodeDeleteCorsPolicyOutput :: Decode DeleteCorsPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCorsPolicyOutput :: Encode DeleteCorsPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeContainerInput = DescribeContainerInput 
  { "ContainerName" :: NullOrUndefined.NullOrUndefined (ContainerName)
  }
derive instance newtypeDescribeContainerInput :: Newtype DescribeContainerInput _
derive instance repGenericDescribeContainerInput :: Generic DescribeContainerInput _
instance showDescribeContainerInput :: Show DescribeContainerInput where
  show = genericShow
instance decodeDescribeContainerInput :: Decode DescribeContainerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeContainerInput :: Encode DescribeContainerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeContainerOutput = DescribeContainerOutput 
  { "Container" :: NullOrUndefined.NullOrUndefined (Container)
  }
derive instance newtypeDescribeContainerOutput :: Newtype DescribeContainerOutput _
derive instance repGenericDescribeContainerOutput :: Generic DescribeContainerOutput _
instance showDescribeContainerOutput :: Show DescribeContainerOutput where
  show = genericShow
instance decodeDescribeContainerOutput :: Decode DescribeContainerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeContainerOutput :: Encode DescribeContainerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Endpoint = Endpoint String
derive instance newtypeEndpoint :: Newtype Endpoint _
derive instance repGenericEndpoint :: Generic Endpoint _
instance showEndpoint :: Show Endpoint where
  show = genericShow
instance decodeEndpoint :: Decode Endpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpoint :: Encode Endpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExposeHeaders = ExposeHeaders (Array Header)
derive instance newtypeExposeHeaders :: Newtype ExposeHeaders _
derive instance repGenericExposeHeaders :: Generic ExposeHeaders _
instance showExposeHeaders :: Show ExposeHeaders where
  show = genericShow
instance decodeExposeHeaders :: Decode ExposeHeaders where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExposeHeaders :: Encode ExposeHeaders where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetContainerPolicyInput = GetContainerPolicyInput 
  { "ContainerName" :: (ContainerName)
  }
derive instance newtypeGetContainerPolicyInput :: Newtype GetContainerPolicyInput _
derive instance repGenericGetContainerPolicyInput :: Generic GetContainerPolicyInput _
instance showGetContainerPolicyInput :: Show GetContainerPolicyInput where
  show = genericShow
instance decodeGetContainerPolicyInput :: Decode GetContainerPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetContainerPolicyInput :: Encode GetContainerPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetContainerPolicyOutput = GetContainerPolicyOutput 
  { "Policy" :: (ContainerPolicy)
  }
derive instance newtypeGetContainerPolicyOutput :: Newtype GetContainerPolicyOutput _
derive instance repGenericGetContainerPolicyOutput :: Generic GetContainerPolicyOutput _
instance showGetContainerPolicyOutput :: Show GetContainerPolicyOutput where
  show = genericShow
instance decodeGetContainerPolicyOutput :: Decode GetContainerPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetContainerPolicyOutput :: Encode GetContainerPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCorsPolicyInput = GetCorsPolicyInput 
  { "ContainerName" :: (ContainerName)
  }
derive instance newtypeGetCorsPolicyInput :: Newtype GetCorsPolicyInput _
derive instance repGenericGetCorsPolicyInput :: Generic GetCorsPolicyInput _
instance showGetCorsPolicyInput :: Show GetCorsPolicyInput where
  show = genericShow
instance decodeGetCorsPolicyInput :: Decode GetCorsPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCorsPolicyInput :: Encode GetCorsPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCorsPolicyOutput = GetCorsPolicyOutput 
  { "CorsPolicy" :: (CorsPolicy)
  }
derive instance newtypeGetCorsPolicyOutput :: Newtype GetCorsPolicyOutput _
derive instance repGenericGetCorsPolicyOutput :: Generic GetCorsPolicyOutput _
instance showGetCorsPolicyOutput :: Show GetCorsPolicyOutput where
  show = genericShow
instance decodeGetCorsPolicyOutput :: Decode GetCorsPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCorsPolicyOutput :: Encode GetCorsPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Header = Header String
derive instance newtypeHeader :: Newtype Header _
derive instance repGenericHeader :: Generic Header _
instance showHeader :: Show Header where
  show = genericShow
instance decodeHeader :: Decode Header where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHeader :: Encode Header where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The service is temporarily unavailable.</p>
newtype InternalServerError = InternalServerError 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _
derive instance repGenericInternalServerError :: Generic InternalServerError _
instance showInternalServerError :: Show InternalServerError where
  show = genericShow
instance decodeInternalServerError :: Decode InternalServerError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerError :: Encode InternalServerError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A service limit has been exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListContainersInput = ListContainersInput 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (ContainerListLimit)
  }
derive instance newtypeListContainersInput :: Newtype ListContainersInput _
derive instance repGenericListContainersInput :: Generic ListContainersInput _
instance showListContainersInput :: Show ListContainersInput where
  show = genericShow
instance decodeListContainersInput :: Decode ListContainersInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListContainersInput :: Encode ListContainersInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListContainersOutput = ListContainersOutput 
  { "Containers" :: (ContainerList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListContainersOutput :: Newtype ListContainersOutput _
derive instance repGenericListContainersOutput :: Generic ListContainersOutput _
instance showListContainersOutput :: Show ListContainersOutput where
  show = genericShow
instance decodeListContainersOutput :: Decode ListContainersOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListContainersOutput :: Encode ListContainersOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxAgeSeconds = MaxAgeSeconds Int
derive instance newtypeMaxAgeSeconds :: Newtype MaxAgeSeconds _
derive instance repGenericMaxAgeSeconds :: Generic MaxAgeSeconds _
instance showMaxAgeSeconds :: Show MaxAgeSeconds where
  show = genericShow
instance decodeMaxAgeSeconds :: Decode MaxAgeSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxAgeSeconds :: Encode MaxAgeSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MethodName = MethodName String
derive instance newtypeMethodName :: Newtype MethodName _
derive instance repGenericMethodName :: Generic MethodName _
instance showMethodName :: Show MethodName where
  show = genericShow
instance decodeMethodName :: Decode MethodName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMethodName :: Encode MethodName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Origin = Origin String
derive instance newtypeOrigin :: Newtype Origin _
derive instance repGenericOrigin :: Generic Origin _
instance showOrigin :: Show Origin where
  show = genericShow
instance decodeOrigin :: Decode Origin where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrigin :: Encode Origin where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _
derive instance repGenericPaginationToken :: Generic PaginationToken _
instance showPaginationToken :: Show PaginationToken where
  show = genericShow
instance decodePaginationToken :: Decode PaginationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaginationToken :: Encode PaginationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Could not perform an operation on a policy that does not exist.</p>
newtype PolicyNotFoundException = PolicyNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypePolicyNotFoundException :: Newtype PolicyNotFoundException _
derive instance repGenericPolicyNotFoundException :: Generic PolicyNotFoundException _
instance showPolicyNotFoundException :: Show PolicyNotFoundException where
  show = genericShow
instance decodePolicyNotFoundException :: Decode PolicyNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyNotFoundException :: Encode PolicyNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutContainerPolicyInput = PutContainerPolicyInput 
  { "ContainerName" :: (ContainerName)
  , "Policy" :: (ContainerPolicy)
  }
derive instance newtypePutContainerPolicyInput :: Newtype PutContainerPolicyInput _
derive instance repGenericPutContainerPolicyInput :: Generic PutContainerPolicyInput _
instance showPutContainerPolicyInput :: Show PutContainerPolicyInput where
  show = genericShow
instance decodePutContainerPolicyInput :: Decode PutContainerPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutContainerPolicyInput :: Encode PutContainerPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutContainerPolicyOutput = PutContainerPolicyOutput Types.NoArguments
derive instance newtypePutContainerPolicyOutput :: Newtype PutContainerPolicyOutput _
derive instance repGenericPutContainerPolicyOutput :: Generic PutContainerPolicyOutput _
instance showPutContainerPolicyOutput :: Show PutContainerPolicyOutput where
  show = genericShow
instance decodePutContainerPolicyOutput :: Decode PutContainerPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutContainerPolicyOutput :: Encode PutContainerPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutCorsPolicyInput = PutCorsPolicyInput 
  { "ContainerName" :: (ContainerName)
  , "CorsPolicy" :: (CorsPolicy)
  }
derive instance newtypePutCorsPolicyInput :: Newtype PutCorsPolicyInput _
derive instance repGenericPutCorsPolicyInput :: Generic PutCorsPolicyInput _
instance showPutCorsPolicyInput :: Show PutCorsPolicyInput where
  show = genericShow
instance decodePutCorsPolicyInput :: Decode PutCorsPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutCorsPolicyInput :: Encode PutCorsPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutCorsPolicyOutput = PutCorsPolicyOutput Types.NoArguments
derive instance newtypePutCorsPolicyOutput :: Newtype PutCorsPolicyOutput _
derive instance repGenericPutCorsPolicyOutput :: Generic PutCorsPolicyOutput _
instance showPutCorsPolicyOutput :: Show PutCorsPolicyOutput where
  show = genericShow
instance decodePutCorsPolicyOutput :: Decode PutCorsPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutCorsPolicyOutput :: Encode PutCorsPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimeStamp = TimeStamp Number
derive instance newtypeTimeStamp :: Newtype TimeStamp _
derive instance repGenericTimeStamp :: Generic TimeStamp _
instance showTimeStamp :: Show TimeStamp where
  show = genericShow
instance decodeTimeStamp :: Decode TimeStamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeStamp :: Encode TimeStamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
