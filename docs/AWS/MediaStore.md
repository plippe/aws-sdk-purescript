## Module AWS.MediaStore

<p>An AWS Elemental MediaStore container is a namespace that holds folders and objects. You use a container endpoint to create, read, and delete objects. </p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createContainer`

``` purescript
createContainer :: forall eff. CreateContainerInput -> Aff (err :: RequestError | eff) CreateContainerOutput
```

<p>Creates a storage container to hold objects. A container is similar to a bucket in the Amazon S3 service.</p>

#### `deleteContainer`

``` purescript
deleteContainer :: forall eff. DeleteContainerInput -> Aff (err :: RequestError | eff) DeleteContainerOutput
```

<p>Deletes the specified container. Before you make a <code>DeleteContainer</code> request, delete any objects in the container or in any folders in the container. You can delete only empty containers. </p>

#### `deleteContainerPolicy`

``` purescript
deleteContainerPolicy :: forall eff. DeleteContainerPolicyInput -> Aff (err :: RequestError | eff) DeleteContainerPolicyOutput
```

<p>Deletes the access policy that is associated with the specified container.</p>

#### `deleteCorsPolicy`

``` purescript
deleteCorsPolicy :: forall eff. DeleteCorsPolicyInput -> Aff (err :: RequestError | eff) DeleteCorsPolicyOutput
```

<p>Deletes the cross-origin resource sharing (CORS) configuration information that is set for the container.</p> <p>To use this operation, you must have permission to perform the <code>MediaStore:DeleteCorsPolicy</code> action. The container owner has this permission by default and can grant this permission to others.</p>

#### `describeContainer`

``` purescript
describeContainer :: forall eff. DescribeContainerInput -> Aff (err :: RequestError | eff) DescribeContainerOutput
```

<p>Retrieves the properties of the requested container. This request is commonly used to retrieve the endpoint of a container. An endpoint is a value assigned by the service when a new container is created. A container's endpoint does not change after it has been assigned. The <code>DescribeContainer</code> request returns a single <code>Container</code> object based on <code>ContainerName</code>. To return all <code>Container</code> objects that are associated with a specified AWS account, use <a>ListContainers</a>.</p>

#### `getContainerPolicy`

``` purescript
getContainerPolicy :: forall eff. GetContainerPolicyInput -> Aff (err :: RequestError | eff) GetContainerPolicyOutput
```

<p>Retrieves the access policy for the specified container. For information about the data that is included in an access policy, see the <a href="https://aws.amazon.com/documentation/iam/">AWS Identity and Access Management User Guide</a>.</p>

#### `getCorsPolicy`

``` purescript
getCorsPolicy :: forall eff. GetCorsPolicyInput -> Aff (err :: RequestError | eff) GetCorsPolicyOutput
```

<p>Returns the cross-origin resource sharing (CORS) configuration information that is set for the container.</p> <p>To use this operation, you must have permission to perform the <code>MediaStore:GetCorsPolicy</code> action. By default, the container owner has this permission and can grant it to others.</p>

#### `listContainers`

``` purescript
listContainers :: forall eff. ListContainersInput -> Aff (err :: RequestError | eff) ListContainersOutput
```

<p>Lists the properties of all containers in AWS Elemental MediaStore. </p> <p>You can query to receive all the containers in one response. Or you can include the <code>MaxResults</code> parameter to receive a limited number of containers in each response. In this case, the response includes a token. To get the next set of containers, send the command again, this time with the <code>NextToken</code> parameter (with the returned token as its value). The next set of responses appears, with a token if there are still more containers to receive. </p> <p>See also <a>DescribeContainer</a>, which gets the properties of one container. </p>

#### `putContainerPolicy`

``` purescript
putContainerPolicy :: forall eff. PutContainerPolicyInput -> Aff (err :: RequestError | eff) PutContainerPolicyOutput
```

<p>Creates an access policy for the specified container to restrict the users and clients that can access it. For information about the data that is included in an access policy, see the <a href="https://aws.amazon.com/documentation/iam/">AWS Identity and Access Management User Guide</a>.</p> <p>For this release of the REST API, you can create only one policy for a container. If you enter <code>PutContainerPolicy</code> twice, the second command modifies the existing policy. </p>

#### `putCorsPolicy`

``` purescript
putCorsPolicy :: forall eff. PutCorsPolicyInput -> Aff (err :: RequestError | eff) PutCorsPolicyOutput
```

<p>Sets the cross-origin resource sharing (CORS) configuration on a container so that the container can service cross-origin requests. For example, you might want to enable a request whose origin is http://www.example.com to access your AWS Elemental MediaStore container at my.example.container.com by using the browser's XMLHttpRequest capability.</p> <p>To enable CORS on a container, you attach a CORS policy to the container. In the CORS policy, you configure rules that identify origins and the HTTP methods that can be executed on your container. The policy can contain up to 398,000 characters. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.</p>

#### `AllowedHeaders`

``` purescript
newtype AllowedHeaders
  = AllowedHeaders (Array Header)
```

##### Instances
``` purescript
Newtype AllowedHeaders _
```

#### `AllowedMethods`

``` purescript
newtype AllowedMethods
  = AllowedMethods (Array MethodName)
```

##### Instances
``` purescript
Newtype AllowedMethods _
```

#### `AllowedOrigins`

``` purescript
newtype AllowedOrigins
  = AllowedOrigins (Array Origin)
```

##### Instances
``` purescript
Newtype AllowedOrigins _
```

#### `Container`

``` purescript
newtype Container
  = Container { "Endpoint" :: NullOrUndefined (Endpoint), "CreationTime" :: NullOrUndefined (TimeStamp), "ARN" :: NullOrUndefined (ContainerARN), "Name" :: NullOrUndefined (ContainerName), "Status" :: NullOrUndefined (ContainerStatus) }
```

<p>This section describes operations that you can perform on an AWS Elemental MediaStore container.</p>

##### Instances
``` purescript
Newtype Container _
```

#### `ContainerARN`

``` purescript
newtype ContainerARN
  = ContainerARN String
```

##### Instances
``` purescript
Newtype ContainerARN _
```

#### `ContainerInUseException`

``` purescript
newtype ContainerInUseException
  = ContainerInUseException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Resource already exists or is being updated.</p>

##### Instances
``` purescript
Newtype ContainerInUseException _
```

#### `ContainerList`

``` purescript
newtype ContainerList
  = ContainerList (Array Container)
```

##### Instances
``` purescript
Newtype ContainerList _
```

#### `ContainerListLimit`

``` purescript
newtype ContainerListLimit
  = ContainerListLimit Int
```

##### Instances
``` purescript
Newtype ContainerListLimit _
```

#### `ContainerName`

``` purescript
newtype ContainerName
  = ContainerName String
```

##### Instances
``` purescript
Newtype ContainerName _
```

#### `ContainerNotFoundException`

``` purescript
newtype ContainerNotFoundException
  = ContainerNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Could not perform an operation on a container that does not exist.</p>

##### Instances
``` purescript
Newtype ContainerNotFoundException _
```

#### `ContainerPolicy`

``` purescript
newtype ContainerPolicy
  = ContainerPolicy String
```

##### Instances
``` purescript
Newtype ContainerPolicy _
```

#### `ContainerStatus`

``` purescript
newtype ContainerStatus
  = ContainerStatus String
```

##### Instances
``` purescript
Newtype ContainerStatus _
```

#### `CorsPolicy`

``` purescript
newtype CorsPolicy
  = CorsPolicy (Array CorsRule)
```

<p>The CORS policy of the container. </p>

##### Instances
``` purescript
Newtype CorsPolicy _
```

#### `CorsPolicyNotFoundException`

``` purescript
newtype CorsPolicyNotFoundException
  = CorsPolicyNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Could not perform an operation on a policy that does not exist.</p>

##### Instances
``` purescript
Newtype CorsPolicyNotFoundException _
```

#### `CorsRule`

``` purescript
newtype CorsRule
  = CorsRule { "AllowedOrigins" :: NullOrUndefined (AllowedOrigins), "AllowedMethods" :: NullOrUndefined (AllowedMethods), "AllowedHeaders" :: NullOrUndefined (AllowedHeaders), "MaxAgeSeconds" :: NullOrUndefined (MaxAgeSeconds), "ExposeHeaders" :: NullOrUndefined (ExposeHeaders) }
```

<p>A rule for a CORS policy. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.</p>

##### Instances
``` purescript
Newtype CorsRule _
```

#### `CreateContainerInput`

``` purescript
newtype CreateContainerInput
  = CreateContainerInput { "ContainerName" :: ContainerName }
```

##### Instances
``` purescript
Newtype CreateContainerInput _
```

#### `CreateContainerOutput`

``` purescript
newtype CreateContainerOutput
  = CreateContainerOutput { "Container" :: Container }
```

##### Instances
``` purescript
Newtype CreateContainerOutput _
```

#### `DeleteContainerInput`

``` purescript
newtype DeleteContainerInput
  = DeleteContainerInput { "ContainerName" :: ContainerName }
```

##### Instances
``` purescript
Newtype DeleteContainerInput _
```

#### `DeleteContainerOutput`

``` purescript
newtype DeleteContainerOutput
  = DeleteContainerOutput {  }
```

##### Instances
``` purescript
Newtype DeleteContainerOutput _
```

#### `DeleteContainerPolicyInput`

``` purescript
newtype DeleteContainerPolicyInput
  = DeleteContainerPolicyInput { "ContainerName" :: ContainerName }
```

##### Instances
``` purescript
Newtype DeleteContainerPolicyInput _
```

#### `DeleteContainerPolicyOutput`

``` purescript
newtype DeleteContainerPolicyOutput
  = DeleteContainerPolicyOutput {  }
```

##### Instances
``` purescript
Newtype DeleteContainerPolicyOutput _
```

#### `DeleteCorsPolicyInput`

``` purescript
newtype DeleteCorsPolicyInput
  = DeleteCorsPolicyInput { "ContainerName" :: ContainerName }
```

##### Instances
``` purescript
Newtype DeleteCorsPolicyInput _
```

#### `DeleteCorsPolicyOutput`

``` purescript
newtype DeleteCorsPolicyOutput
  = DeleteCorsPolicyOutput {  }
```

##### Instances
``` purescript
Newtype DeleteCorsPolicyOutput _
```

#### `DescribeContainerInput`

``` purescript
newtype DescribeContainerInput
  = DescribeContainerInput { "ContainerName" :: NullOrUndefined (ContainerName) }
```

##### Instances
``` purescript
Newtype DescribeContainerInput _
```

#### `DescribeContainerOutput`

``` purescript
newtype DescribeContainerOutput
  = DescribeContainerOutput { "Container" :: NullOrUndefined (Container) }
```

##### Instances
``` purescript
Newtype DescribeContainerOutput _
```

#### `Endpoint`

``` purescript
newtype Endpoint
  = Endpoint String
```

##### Instances
``` purescript
Newtype Endpoint _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `ExposeHeaders`

``` purescript
newtype ExposeHeaders
  = ExposeHeaders (Array Header)
```

##### Instances
``` purescript
Newtype ExposeHeaders _
```

#### `GetContainerPolicyInput`

``` purescript
newtype GetContainerPolicyInput
  = GetContainerPolicyInput { "ContainerName" :: ContainerName }
```

##### Instances
``` purescript
Newtype GetContainerPolicyInput _
```

#### `GetContainerPolicyOutput`

``` purescript
newtype GetContainerPolicyOutput
  = GetContainerPolicyOutput { "Policy" :: ContainerPolicy }
```

##### Instances
``` purescript
Newtype GetContainerPolicyOutput _
```

#### `GetCorsPolicyInput`

``` purescript
newtype GetCorsPolicyInput
  = GetCorsPolicyInput { "ContainerName" :: ContainerName }
```

##### Instances
``` purescript
Newtype GetCorsPolicyInput _
```

#### `GetCorsPolicyOutput`

``` purescript
newtype GetCorsPolicyOutput
  = GetCorsPolicyOutput { "CorsPolicy" :: CorsPolicy }
```

##### Instances
``` purescript
Newtype GetCorsPolicyOutput _
```

#### `Header`

``` purescript
newtype Header
  = Header String
```

##### Instances
``` purescript
Newtype Header _
```

#### `InternalServerError`

``` purescript
newtype InternalServerError
  = InternalServerError { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The service is temporarily unavailable.</p>

##### Instances
``` purescript
Newtype InternalServerError _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>A service limit has been exceeded.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListContainersInput`

``` purescript
newtype ListContainersInput
  = ListContainersInput { "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (ContainerListLimit) }
```

##### Instances
``` purescript
Newtype ListContainersInput _
```

#### `ListContainersOutput`

``` purescript
newtype ListContainersOutput
  = ListContainersOutput { "Containers" :: ContainerList, "NextToken" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListContainersOutput _
```

#### `MaxAgeSeconds`

``` purescript
newtype MaxAgeSeconds
  = MaxAgeSeconds Int
```

##### Instances
``` purescript
Newtype MaxAgeSeconds _
```

#### `MethodName`

``` purescript
newtype MethodName
  = MethodName String
```

##### Instances
``` purescript
Newtype MethodName _
```

#### `Origin`

``` purescript
newtype Origin
  = Origin String
```

##### Instances
``` purescript
Newtype Origin _
```

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

##### Instances
``` purescript
Newtype PaginationToken _
```

#### `PolicyNotFoundException`

``` purescript
newtype PolicyNotFoundException
  = PolicyNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Could not perform an operation on a policy that does not exist.</p>

##### Instances
``` purescript
Newtype PolicyNotFoundException _
```

#### `PutContainerPolicyInput`

``` purescript
newtype PutContainerPolicyInput
  = PutContainerPolicyInput { "ContainerName" :: ContainerName, "Policy" :: ContainerPolicy }
```

##### Instances
``` purescript
Newtype PutContainerPolicyInput _
```

#### `PutContainerPolicyOutput`

``` purescript
newtype PutContainerPolicyOutput
  = PutContainerPolicyOutput {  }
```

##### Instances
``` purescript
Newtype PutContainerPolicyOutput _
```

#### `PutCorsPolicyInput`

``` purescript
newtype PutCorsPolicyInput
  = PutCorsPolicyInput { "ContainerName" :: ContainerName, "CorsPolicy" :: CorsPolicy }
```

##### Instances
``` purescript
Newtype PutCorsPolicyInput _
```

#### `PutCorsPolicyOutput`

``` purescript
newtype PutCorsPolicyOutput
  = PutCorsPolicyOutput {  }
```

##### Instances
``` purescript
Newtype PutCorsPolicyOutput _
```

#### `TimeStamp`

``` purescript
newtype TimeStamp
  = TimeStamp Number
```

##### Instances
``` purescript
Newtype TimeStamp _
```


