

-- | <p>An AWS Elemental MediaStore container is a namespace that holds folders and objects. You use a container endpoint to create, read, and delete objects. </p>
module AWS.MediaStore where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MediaStore" :: String


-- | <p>Creates a storage container to hold objects. A container is similar to a bucket in the Amazon S3 service.</p>
createContainer :: forall eff. CreateContainerInput -> Aff (err :: AWS.RequestError | eff) CreateContainerOutput
createContainer = AWS.request serviceName "createContainer" 


-- | <p>Deletes the specified container. Before you make a <code>DeleteContainer</code> request, delete any objects in the container or in any folders in the container. You can delete only empty containers. </p>
deleteContainer :: forall eff. DeleteContainerInput -> Aff (err :: AWS.RequestError | eff) DeleteContainerOutput
deleteContainer = AWS.request serviceName "deleteContainer" 


-- | <p>Deletes the access policy that is associated with the specified container.</p>
deleteContainerPolicy :: forall eff. DeleteContainerPolicyInput -> Aff (err :: AWS.RequestError | eff) DeleteContainerPolicyOutput
deleteContainerPolicy = AWS.request serviceName "deleteContainerPolicy" 


-- | <p>Deletes the cross-origin resource sharing (CORS) configuration information that is set for the container.</p> <p>To use this operation, you must have permission to perform the <code>MediaStore:DeleteCorsPolicy</code> action. The container owner has this permission by default and can grant this permission to others.</p>
deleteCorsPolicy :: forall eff. DeleteCorsPolicyInput -> Aff (err :: AWS.RequestError | eff) DeleteCorsPolicyOutput
deleteCorsPolicy = AWS.request serviceName "deleteCorsPolicy" 


-- | <p>Retrieves the properties of the requested container. This request is commonly used to retrieve the endpoint of a container. An endpoint is a value assigned by the service when a new container is created. A container's endpoint does not change after it has been assigned. The <code>DescribeContainer</code> request returns a single <code>Container</code> object based on <code>ContainerName</code>. To return all <code>Container</code> objects that are associated with a specified AWS account, use <a>ListContainers</a>.</p>
describeContainer :: forall eff. DescribeContainerInput -> Aff (err :: AWS.RequestError | eff) DescribeContainerOutput
describeContainer = AWS.request serviceName "describeContainer" 


-- | <p>Retrieves the access policy for the specified container. For information about the data that is included in an access policy, see the <a href="https://aws.amazon.com/documentation/iam/">AWS Identity and Access Management User Guide</a>.</p>
getContainerPolicy :: forall eff. GetContainerPolicyInput -> Aff (err :: AWS.RequestError | eff) GetContainerPolicyOutput
getContainerPolicy = AWS.request serviceName "getContainerPolicy" 


-- | <p>Returns the cross-origin resource sharing (CORS) configuration information that is set for the container.</p> <p>To use this operation, you must have permission to perform the <code>MediaStore:GetCorsPolicy</code> action. By default, the container owner has this permission and can grant it to others.</p>
getCorsPolicy :: forall eff. GetCorsPolicyInput -> Aff (err :: AWS.RequestError | eff) GetCorsPolicyOutput
getCorsPolicy = AWS.request serviceName "getCorsPolicy" 


-- | <p>Lists the properties of all containers in AWS Elemental MediaStore. </p> <p>You can query to receive all the containers in one response. Or you can include the <code>MaxResults</code> parameter to receive a limited number of containers in each response. In this case, the response includes a token. To get the next set of containers, send the command again, this time with the <code>NextToken</code> parameter (with the returned token as its value). The next set of responses appears, with a token if there are still more containers to receive. </p> <p>See also <a>DescribeContainer</a>, which gets the properties of one container. </p>
listContainers :: forall eff. ListContainersInput -> Aff (err :: AWS.RequestError | eff) ListContainersOutput
listContainers = AWS.request serviceName "listContainers" 


-- | <p>Creates an access policy for the specified container to restrict the users and clients that can access it. For information about the data that is included in an access policy, see the <a href="https://aws.amazon.com/documentation/iam/">AWS Identity and Access Management User Guide</a>.</p> <p>For this release of the REST API, you can create only one policy for a container. If you enter <code>PutContainerPolicy</code> twice, the second command modifies the existing policy. </p>
putContainerPolicy :: forall eff. PutContainerPolicyInput -> Aff (err :: AWS.RequestError | eff) PutContainerPolicyOutput
putContainerPolicy = AWS.request serviceName "putContainerPolicy" 


-- | <p>Sets the cross-origin resource sharing (CORS) configuration on a container so that the container can service cross-origin requests. For example, you might want to enable a request whose origin is http://www.example.com to access your AWS Elemental MediaStore container at my.example.container.com by using the browser's XMLHttpRequest capability.</p> <p>To enable CORS on a container, you attach a CORS policy to the container. In the CORS policy, you configure rules that identify origins and the HTTP methods that can be executed on your container. The policy can contain up to 398,000 characters. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.</p>
putCorsPolicy :: forall eff. PutCorsPolicyInput -> Aff (err :: AWS.RequestError | eff) PutCorsPolicyOutput
putCorsPolicy = AWS.request serviceName "putCorsPolicy" 


newtype AllowedHeaders = AllowedHeaders (Array Header)
derive instance newtypeAllowedHeaders :: Newtype AllowedHeaders _


newtype AllowedMethods = AllowedMethods (Array MethodName)
derive instance newtypeAllowedMethods :: Newtype AllowedMethods _


newtype AllowedOrigins = AllowedOrigins (Array Origin)
derive instance newtypeAllowedOrigins :: Newtype AllowedOrigins _


-- | <p>This section describes operations that you can perform on an AWS Elemental MediaStore container.</p>
newtype Container = Container 
  { "Endpoint" :: NullOrUndefined (Endpoint)
  , "CreationTime" :: NullOrUndefined (TimeStamp)
  , "ARN" :: NullOrUndefined (ContainerARN)
  , "Name" :: NullOrUndefined (ContainerName)
  , "Status" :: NullOrUndefined (ContainerStatus)
  }
derive instance newtypeContainer :: Newtype Container _


newtype ContainerARN = ContainerARN String
derive instance newtypeContainerARN :: Newtype ContainerARN _


-- | <p>Resource already exists or is being updated.</p>
newtype ContainerInUseException = ContainerInUseException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeContainerInUseException :: Newtype ContainerInUseException _


newtype ContainerList = ContainerList (Array Container)
derive instance newtypeContainerList :: Newtype ContainerList _


newtype ContainerListLimit = ContainerListLimit Int
derive instance newtypeContainerListLimit :: Newtype ContainerListLimit _


newtype ContainerName = ContainerName String
derive instance newtypeContainerName :: Newtype ContainerName _


-- | <p>Could not perform an operation on a container that does not exist.</p>
newtype ContainerNotFoundException = ContainerNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeContainerNotFoundException :: Newtype ContainerNotFoundException _


newtype ContainerPolicy = ContainerPolicy String
derive instance newtypeContainerPolicy :: Newtype ContainerPolicy _


newtype ContainerStatus = ContainerStatus String
derive instance newtypeContainerStatus :: Newtype ContainerStatus _


-- | <p>The CORS policy of the container. </p>
newtype CorsPolicy = CorsPolicy (Array CorsRule)
derive instance newtypeCorsPolicy :: Newtype CorsPolicy _


-- | <p>Could not perform an operation on a policy that does not exist.</p>
newtype CorsPolicyNotFoundException = CorsPolicyNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeCorsPolicyNotFoundException :: Newtype CorsPolicyNotFoundException _


-- | <p>A rule for a CORS policy. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.</p>
newtype CorsRule = CorsRule 
  { "AllowedOrigins" :: NullOrUndefined (AllowedOrigins)
  , "AllowedMethods" :: NullOrUndefined (AllowedMethods)
  , "AllowedHeaders" :: NullOrUndefined (AllowedHeaders)
  , "MaxAgeSeconds" :: NullOrUndefined (MaxAgeSeconds)
  , "ExposeHeaders" :: NullOrUndefined (ExposeHeaders)
  }
derive instance newtypeCorsRule :: Newtype CorsRule _


newtype CreateContainerInput = CreateContainerInput 
  { "ContainerName" :: (ContainerName)
  }
derive instance newtypeCreateContainerInput :: Newtype CreateContainerInput _


newtype CreateContainerOutput = CreateContainerOutput 
  { "Container" :: (Container)
  }
derive instance newtypeCreateContainerOutput :: Newtype CreateContainerOutput _


newtype DeleteContainerInput = DeleteContainerInput 
  { "ContainerName" :: (ContainerName)
  }
derive instance newtypeDeleteContainerInput :: Newtype DeleteContainerInput _


newtype DeleteContainerOutput = DeleteContainerOutput 
  { 
  }
derive instance newtypeDeleteContainerOutput :: Newtype DeleteContainerOutput _


newtype DeleteContainerPolicyInput = DeleteContainerPolicyInput 
  { "ContainerName" :: (ContainerName)
  }
derive instance newtypeDeleteContainerPolicyInput :: Newtype DeleteContainerPolicyInput _


newtype DeleteContainerPolicyOutput = DeleteContainerPolicyOutput 
  { 
  }
derive instance newtypeDeleteContainerPolicyOutput :: Newtype DeleteContainerPolicyOutput _


newtype DeleteCorsPolicyInput = DeleteCorsPolicyInput 
  { "ContainerName" :: (ContainerName)
  }
derive instance newtypeDeleteCorsPolicyInput :: Newtype DeleteCorsPolicyInput _


newtype DeleteCorsPolicyOutput = DeleteCorsPolicyOutput 
  { 
  }
derive instance newtypeDeleteCorsPolicyOutput :: Newtype DeleteCorsPolicyOutput _


newtype DescribeContainerInput = DescribeContainerInput 
  { "ContainerName" :: NullOrUndefined (ContainerName)
  }
derive instance newtypeDescribeContainerInput :: Newtype DescribeContainerInput _


newtype DescribeContainerOutput = DescribeContainerOutput 
  { "Container" :: NullOrUndefined (Container)
  }
derive instance newtypeDescribeContainerOutput :: Newtype DescribeContainerOutput _


newtype Endpoint = Endpoint String
derive instance newtypeEndpoint :: Newtype Endpoint _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype ExposeHeaders = ExposeHeaders (Array Header)
derive instance newtypeExposeHeaders :: Newtype ExposeHeaders _


newtype GetContainerPolicyInput = GetContainerPolicyInput 
  { "ContainerName" :: (ContainerName)
  }
derive instance newtypeGetContainerPolicyInput :: Newtype GetContainerPolicyInput _


newtype GetContainerPolicyOutput = GetContainerPolicyOutput 
  { "Policy" :: (ContainerPolicy)
  }
derive instance newtypeGetContainerPolicyOutput :: Newtype GetContainerPolicyOutput _


newtype GetCorsPolicyInput = GetCorsPolicyInput 
  { "ContainerName" :: (ContainerName)
  }
derive instance newtypeGetCorsPolicyInput :: Newtype GetCorsPolicyInput _


newtype GetCorsPolicyOutput = GetCorsPolicyOutput 
  { "CorsPolicy" :: (CorsPolicy)
  }
derive instance newtypeGetCorsPolicyOutput :: Newtype GetCorsPolicyOutput _


newtype Header = Header String
derive instance newtypeHeader :: Newtype Header _


-- | <p>The service is temporarily unavailable.</p>
newtype InternalServerError = InternalServerError 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _


-- | <p>A service limit has been exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListContainersInput = ListContainersInput 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined (ContainerListLimit)
  }
derive instance newtypeListContainersInput :: Newtype ListContainersInput _


newtype ListContainersOutput = ListContainersOutput 
  { "Containers" :: (ContainerList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListContainersOutput :: Newtype ListContainersOutput _


newtype MaxAgeSeconds = MaxAgeSeconds Int
derive instance newtypeMaxAgeSeconds :: Newtype MaxAgeSeconds _


newtype MethodName = MethodName String
derive instance newtypeMethodName :: Newtype MethodName _


newtype Origin = Origin String
derive instance newtypeOrigin :: Newtype Origin _


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _


-- | <p>Could not perform an operation on a policy that does not exist.</p>
newtype PolicyNotFoundException = PolicyNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypePolicyNotFoundException :: Newtype PolicyNotFoundException _


newtype PutContainerPolicyInput = PutContainerPolicyInput 
  { "ContainerName" :: (ContainerName)
  , "Policy" :: (ContainerPolicy)
  }
derive instance newtypePutContainerPolicyInput :: Newtype PutContainerPolicyInput _


newtype PutContainerPolicyOutput = PutContainerPolicyOutput 
  { 
  }
derive instance newtypePutContainerPolicyOutput :: Newtype PutContainerPolicyOutput _


newtype PutCorsPolicyInput = PutCorsPolicyInput 
  { "ContainerName" :: (ContainerName)
  , "CorsPolicy" :: (CorsPolicy)
  }
derive instance newtypePutCorsPolicyInput :: Newtype PutCorsPolicyInput _


newtype PutCorsPolicyOutput = PutCorsPolicyOutput 
  { 
  }
derive instance newtypePutCorsPolicyOutput :: Newtype PutCorsPolicyOutput _


newtype TimeStamp = TimeStamp Number
derive instance newtypeTimeStamp :: Newtype TimeStamp _
