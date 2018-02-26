

-- | <p>An AWS Elemental MediaStore container is a namespace that holds folders and objects. You use a container endpoint to create, read, and delete objects. </p>
module AWS.MediaStore where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MediaStore" :: String


-- | <p>Creates a storage container to hold objects. A container is similar to a bucket in the Amazon S3 service.</p>
createContainer :: forall eff. CreateContainerInput -> Aff (err :: AWS.RequestError | eff) CreateContainerOutput
createContainer = AWS.request serviceName "CreateContainer" 


-- | <p>Deletes the specified container. Before you make a <code>DeleteContainer</code> request, delete any objects in the container or in any folders in the container. You can delete only empty containers. </p>
deleteContainer :: forall eff. DeleteContainerInput -> Aff (err :: AWS.RequestError | eff) DeleteContainerOutput
deleteContainer = AWS.request serviceName "DeleteContainer" 


-- | <p>Deletes the access policy that is associated with the specified container.</p>
deleteContainerPolicy :: forall eff. DeleteContainerPolicyInput -> Aff (err :: AWS.RequestError | eff) DeleteContainerPolicyOutput
deleteContainerPolicy = AWS.request serviceName "DeleteContainerPolicy" 


-- | <p>Deletes the cross-origin resource sharing (CORS) configuration information that is set for the container.</p> <p>To use this operation, you must have permission to perform the <code>MediaStore:DeleteCorsPolicy</code> action. The container owner has this permission by default and can grant this permission to others.</p>
deleteCorsPolicy :: forall eff. DeleteCorsPolicyInput -> Aff (err :: AWS.RequestError | eff) DeleteCorsPolicyOutput
deleteCorsPolicy = AWS.request serviceName "DeleteCorsPolicy" 


-- | <p>Retrieves the properties of the requested container. This request is commonly used to retrieve the endpoint of a container. An endpoint is a value assigned by the service when a new container is created. A container's endpoint does not change after it has been assigned. The <code>DescribeContainer</code> request returns a single <code>Container</code> object based on <code>ContainerName</code>. To return all <code>Container</code> objects that are associated with a specified AWS account, use <a>ListContainers</a>.</p>
describeContainer :: forall eff. DescribeContainerInput -> Aff (err :: AWS.RequestError | eff) DescribeContainerOutput
describeContainer = AWS.request serviceName "DescribeContainer" 


-- | <p>Retrieves the access policy for the specified container. For information about the data that is included in an access policy, see the <a href="https://aws.amazon.com/documentation/iam/">AWS Identity and Access Management User Guide</a>.</p>
getContainerPolicy :: forall eff. GetContainerPolicyInput -> Aff (err :: AWS.RequestError | eff) GetContainerPolicyOutput
getContainerPolicy = AWS.request serviceName "GetContainerPolicy" 


-- | <p>Returns the cross-origin resource sharing (CORS) configuration information that is set for the container.</p> <p>To use this operation, you must have permission to perform the <code>MediaStore:GetCorsPolicy</code> action. By default, the container owner has this permission and can grant it to others.</p>
getCorsPolicy :: forall eff. GetCorsPolicyInput -> Aff (err :: AWS.RequestError | eff) GetCorsPolicyOutput
getCorsPolicy = AWS.request serviceName "GetCorsPolicy" 


-- | <p>Lists the properties of all containers in AWS Elemental MediaStore. </p> <p>You can query to receive all the containers in one response. Or you can include the <code>MaxResults</code> parameter to receive a limited number of containers in each response. In this case, the response includes a token. To get the next set of containers, send the command again, this time with the <code>NextToken</code> parameter (with the returned token as its value). The next set of responses appears, with a token if there are still more containers to receive. </p> <p>See also <a>DescribeContainer</a>, which gets the properties of one container. </p>
listContainers :: forall eff. ListContainersInput -> Aff (err :: AWS.RequestError | eff) ListContainersOutput
listContainers = AWS.request serviceName "ListContainers" 


-- | <p>Creates an access policy for the specified container to restrict the users and clients that can access it. For information about the data that is included in an access policy, see the <a href="https://aws.amazon.com/documentation/iam/">AWS Identity and Access Management User Guide</a>.</p> <p>For this release of the REST API, you can create only one policy for a container. If you enter <code>PutContainerPolicy</code> twice, the second command modifies the existing policy. </p>
putContainerPolicy :: forall eff. PutContainerPolicyInput -> Aff (err :: AWS.RequestError | eff) PutContainerPolicyOutput
putContainerPolicy = AWS.request serviceName "PutContainerPolicy" 


-- | <p>Sets the cross-origin resource sharing (CORS) configuration on a container so that the container can service cross-origin requests. For example, you might want to enable a request whose origin is http://www.example.com to access your AWS Elemental MediaStore container at my.example.container.com by using the browser's XMLHttpRequest capability.</p> <p>To enable CORS on a container, you attach a CORS policy to the container. In the CORS policy, you configure rules that identify origins and the HTTP methods that can be executed on your container. The policy can contain up to 398,000 characters. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.</p>
putCorsPolicy :: forall eff. PutCorsPolicyInput -> Aff (err :: AWS.RequestError | eff) PutCorsPolicyOutput
putCorsPolicy = AWS.request serviceName "PutCorsPolicy" 


newtype AllowedHeaders = AllowedHeaders (Array Header)


newtype AllowedMethods = AllowedMethods (Array MethodName)


newtype AllowedOrigins = AllowedOrigins (Array Origin)


-- | <p>This section describes operations that you can perform on an AWS Elemental MediaStore container.</p>
newtype Container = Container 
  { "Endpoint" :: NullOrUndefined (Endpoint)
  , "CreationTime" :: NullOrUndefined (TimeStamp)
  , "ARN" :: NullOrUndefined (ContainerARN)
  , "Name" :: NullOrUndefined (ContainerName)
  , "Status" :: NullOrUndefined (ContainerStatus)
  }


newtype ContainerARN = ContainerARN String


-- | <p>Resource already exists or is being updated.</p>
newtype ContainerInUseException = ContainerInUseException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ContainerList = ContainerList (Array Container)


newtype ContainerListLimit = ContainerListLimit Int


newtype ContainerName = ContainerName String


-- | <p>Could not perform an operation on a container that does not exist.</p>
newtype ContainerNotFoundException = ContainerNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ContainerPolicy = ContainerPolicy String


newtype ContainerStatus = ContainerStatus String


-- | <p>The CORS policy of the container. </p>
newtype CorsPolicy = CorsPolicy (Array CorsRule)


-- | <p>Could not perform an operation on a policy that does not exist.</p>
newtype CorsPolicyNotFoundException = CorsPolicyNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>A rule for a CORS policy. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.</p>
newtype CorsRule = CorsRule 
  { "AllowedOrigins" :: NullOrUndefined (AllowedOrigins)
  , "AllowedMethods" :: NullOrUndefined (AllowedMethods)
  , "AllowedHeaders" :: NullOrUndefined (AllowedHeaders)
  , "MaxAgeSeconds" :: NullOrUndefined (MaxAgeSeconds)
  , "ExposeHeaders" :: NullOrUndefined (ExposeHeaders)
  }


newtype CreateContainerInput = CreateContainerInput 
  { "ContainerName" :: (ContainerName)
  }


newtype CreateContainerOutput = CreateContainerOutput 
  { "Container" :: (Container)
  }


newtype DeleteContainerInput = DeleteContainerInput 
  { "ContainerName" :: (ContainerName)
  }


newtype DeleteContainerOutput = DeleteContainerOutput 
  { 
  }


newtype DeleteContainerPolicyInput = DeleteContainerPolicyInput 
  { "ContainerName" :: (ContainerName)
  }


newtype DeleteContainerPolicyOutput = DeleteContainerPolicyOutput 
  { 
  }


newtype DeleteCorsPolicyInput = DeleteCorsPolicyInput 
  { "ContainerName" :: (ContainerName)
  }


newtype DeleteCorsPolicyOutput = DeleteCorsPolicyOutput 
  { 
  }


newtype DescribeContainerInput = DescribeContainerInput 
  { "ContainerName" :: NullOrUndefined (ContainerName)
  }


newtype DescribeContainerOutput = DescribeContainerOutput 
  { "Container" :: NullOrUndefined (Container)
  }


newtype Endpoint = Endpoint String


newtype ErrorMessage = ErrorMessage String


newtype ExposeHeaders = ExposeHeaders (Array Header)


newtype GetContainerPolicyInput = GetContainerPolicyInput 
  { "ContainerName" :: (ContainerName)
  }


newtype GetContainerPolicyOutput = GetContainerPolicyOutput 
  { "Policy" :: (ContainerPolicy)
  }


newtype GetCorsPolicyInput = GetCorsPolicyInput 
  { "ContainerName" :: (ContainerName)
  }


newtype GetCorsPolicyOutput = GetCorsPolicyOutput 
  { "CorsPolicy" :: (CorsPolicy)
  }


newtype Header = Header String


-- | <p>The service is temporarily unavailable.</p>
newtype InternalServerError = InternalServerError 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>A service limit has been exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ListContainersInput = ListContainersInput 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined (ContainerListLimit)
  }


newtype ListContainersOutput = ListContainersOutput 
  { "Containers" :: (ContainerList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype MaxAgeSeconds = MaxAgeSeconds Int


newtype MethodName = MethodName String


newtype Origin = Origin String


newtype PaginationToken = PaginationToken String


-- | <p>Could not perform an operation on a policy that does not exist.</p>
newtype PolicyNotFoundException = PolicyNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype PutContainerPolicyInput = PutContainerPolicyInput 
  { "ContainerName" :: (ContainerName)
  , "Policy" :: (ContainerPolicy)
  }


newtype PutContainerPolicyOutput = PutContainerPolicyOutput 
  { 
  }


newtype PutCorsPolicyInput = PutCorsPolicyInput 
  { "ContainerName" :: (ContainerName)
  , "CorsPolicy" :: (CorsPolicy)
  }


newtype PutCorsPolicyOutput = PutCorsPolicyOutput 
  { 
  }


newtype TimeStamp = TimeStamp Number
