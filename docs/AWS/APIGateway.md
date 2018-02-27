## Module AWS.APIGateway

<fullname>Amazon API Gateway</fullname> <p>Amazon API Gateway helps developers deliver robust, secure, and scalable mobile and web application back ends. API Gateway allows developers to securely connect mobile and web applications to APIs that run on AWS Lambda, Amazon EC2, or other publicly addressable web services that are hosted outside of AWS.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createApiKey`

``` purescript
createApiKey :: forall eff. CreateApiKeyRequest -> Aff (err :: RequestError | eff) ApiKey
```

<p>Create an <a>ApiKey</a> resource. </p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/create-api-key.html">AWS CLI</a></div>

#### `createAuthorizer`

``` purescript
createAuthorizer :: forall eff. CreateAuthorizerRequest -> Aff (err :: RequestError | eff) Authorizer
```

<p>Adds a new <a>Authorizer</a> resource to an existing <a>RestApi</a> resource.</p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/create-authorizer.html">AWS CLI</a></div>

#### `createBasePathMapping`

``` purescript
createBasePathMapping :: forall eff. CreateBasePathMappingRequest -> Aff (err :: RequestError | eff) BasePathMapping
```

<p>Creates a new <a>BasePathMapping</a> resource.</p>

#### `createDeployment`

``` purescript
createDeployment :: forall eff. CreateDeploymentRequest -> Aff (err :: RequestError | eff) Deployment
```

<p>Creates a <a>Deployment</a> resource, which makes a specified <a>RestApi</a> callable over the internet.</p>

#### `createDocumentationPart`

``` purescript
createDocumentationPart :: forall eff. CreateDocumentationPartRequest -> Aff (err :: RequestError | eff) DocumentationPart
```

#### `createDocumentationVersion`

``` purescript
createDocumentationVersion :: forall eff. CreateDocumentationVersionRequest -> Aff (err :: RequestError | eff) DocumentationVersion
```

#### `createDomainName`

``` purescript
createDomainName :: forall eff. CreateDomainNameRequest -> Aff (err :: RequestError | eff) DomainName
```

<p>Creates a new domain name.</p>

#### `createModel`

``` purescript
createModel :: forall eff. CreateModelRequest -> Aff (err :: RequestError | eff) Model
```

<p>Adds a new <a>Model</a> resource to an existing <a>RestApi</a> resource.</p>

#### `createRequestValidator`

``` purescript
createRequestValidator :: forall eff. CreateRequestValidatorRequest -> Aff (err :: RequestError | eff) RequestValidator
```

<p>Creates a <a>ReqeustValidator</a> of a given <a>RestApi</a>.</p>

#### `createResource`

``` purescript
createResource :: forall eff. CreateResourceRequest -> Aff (err :: RequestError | eff) Resource
```

<p>Creates a <a>Resource</a> resource.</p>

#### `createRestApi`

``` purescript
createRestApi :: forall eff. CreateRestApiRequest -> Aff (err :: RequestError | eff) RestApi
```

<p>Creates a new <a>RestApi</a> resource.</p>

#### `createStage`

``` purescript
createStage :: forall eff. CreateStageRequest -> Aff (err :: RequestError | eff) Stage
```

<p>Creates a new <a>Stage</a> resource that references a pre-existing <a>Deployment</a> for the API. </p>

#### `createUsagePlan`

``` purescript
createUsagePlan :: forall eff. CreateUsagePlanRequest -> Aff (err :: RequestError | eff) UsagePlan
```

<p>Creates a usage plan with the throttle and quota limits, as well as the associated API stages, specified in the payload. </p>

#### `createUsagePlanKey`

``` purescript
createUsagePlanKey :: forall eff. CreateUsagePlanKeyRequest -> Aff (err :: RequestError | eff) UsagePlanKey
```

<p>Creates a usage plan key for adding an existing API key to a usage plan.</p>

#### `createVpcLink`

``` purescript
createVpcLink :: forall eff. CreateVpcLinkRequest -> Aff (err :: RequestError | eff) VpcLink
```

<p>Creates a VPC link, under the caller's account in a selected region, in an asynchronous operation that typically takes 2-4 minutes to complete and become operational. The caller must have permissions to create and update VPC Endpoint services.</p>

#### `deleteApiKey`

``` purescript
deleteApiKey :: forall eff. DeleteApiKeyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the <a>ApiKey</a> resource.</p>

#### `deleteAuthorizer`

``` purescript
deleteAuthorizer :: forall eff. DeleteAuthorizerRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes an existing <a>Authorizer</a> resource.</p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/delete-authorizer.html">AWS CLI</a></div>

#### `deleteBasePathMapping`

``` purescript
deleteBasePathMapping :: forall eff. DeleteBasePathMappingRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the <a>BasePathMapping</a> resource.</p>

#### `deleteClientCertificate`

``` purescript
deleteClientCertificate :: forall eff. DeleteClientCertificateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the <a>ClientCertificate</a> resource.</p>

#### `deleteDeployment`

``` purescript
deleteDeployment :: forall eff. DeleteDeploymentRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a <a>Deployment</a> resource. Deleting a deployment will only succeed if there are no <a>Stage</a> resources associated with it.</p>

#### `deleteDocumentationPart`

``` purescript
deleteDocumentationPart :: forall eff. DeleteDocumentationPartRequest -> Aff (err :: RequestError | eff) Unit
```

#### `deleteDocumentationVersion`

``` purescript
deleteDocumentationVersion :: forall eff. DeleteDocumentationVersionRequest -> Aff (err :: RequestError | eff) Unit
```

#### `deleteDomainName`

``` purescript
deleteDomainName :: forall eff. DeleteDomainNameRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the <a>DomainName</a> resource.</p>

#### `deleteGatewayResponse`

``` purescript
deleteGatewayResponse :: forall eff. DeleteGatewayResponseRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Clears any customization of a <a>GatewayResponse</a> of a specified response type on the given <a>RestApi</a> and resets it with the default settings.</p>

#### `deleteIntegration`

``` purescript
deleteIntegration :: forall eff. DeleteIntegrationRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Represents a delete integration.</p>

#### `deleteIntegrationResponse`

``` purescript
deleteIntegrationResponse :: forall eff. DeleteIntegrationResponseRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Represents a delete integration response.</p>

#### `deleteMethod`

``` purescript
deleteMethod :: forall eff. DeleteMethodRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes an existing <a>Method</a> resource.</p>

#### `deleteMethodResponse`

``` purescript
deleteMethodResponse :: forall eff. DeleteMethodResponseRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes an existing <a>MethodResponse</a> resource.</p>

#### `deleteModel`

``` purescript
deleteModel :: forall eff. DeleteModelRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a model.</p>

#### `deleteRequestValidator`

``` purescript
deleteRequestValidator :: forall eff. DeleteRequestValidatorRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a <a>RequestValidator</a> of a given <a>RestApi</a>.</p>

#### `deleteResource`

``` purescript
deleteResource :: forall eff. DeleteResourceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a <a>Resource</a> resource.</p>

#### `deleteRestApi`

``` purescript
deleteRestApi :: forall eff. DeleteRestApiRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified API.</p>

#### `deleteStage`

``` purescript
deleteStage :: forall eff. DeleteStageRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a <a>Stage</a> resource.</p>

#### `deleteUsagePlan`

``` purescript
deleteUsagePlan :: forall eff. DeleteUsagePlanRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a usage plan of a given plan Id.</p>

#### `deleteUsagePlanKey`

``` purescript
deleteUsagePlanKey :: forall eff. DeleteUsagePlanKeyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a usage plan key and remove the underlying API key from the associated usage plan.</p>

#### `deleteVpcLink`

``` purescript
deleteVpcLink :: forall eff. DeleteVpcLinkRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes an existing <a>VpcLink</a> of a specified identifier.</p>

#### `flushStageAuthorizersCache`

``` purescript
flushStageAuthorizersCache :: forall eff. FlushStageAuthorizersCacheRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Flushes all authorizer cache entries on a stage.</p>

#### `flushStageCache`

``` purescript
flushStageCache :: forall eff. FlushStageCacheRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Flushes a stage's cache.</p>

#### `generateClientCertificate`

``` purescript
generateClientCertificate :: forall eff. GenerateClientCertificateRequest -> Aff (err :: RequestError | eff) ClientCertificate
```

<p>Generates a <a>ClientCertificate</a> resource.</p>

#### `getAccount`

``` purescript
getAccount :: forall eff. GetAccountRequest -> Aff (err :: RequestError | eff) Account
```

<p>Gets information about the current <a>Account</a> resource.</p>

#### `getApiKey`

``` purescript
getApiKey :: forall eff. GetApiKeyRequest -> Aff (err :: RequestError | eff) ApiKey
```

<p>Gets information about the current <a>ApiKey</a> resource.</p>

#### `getApiKeys`

``` purescript
getApiKeys :: forall eff. GetApiKeysRequest -> Aff (err :: RequestError | eff) ApiKeys
```

<p>Gets information about the current <a>ApiKeys</a> resource.</p>

#### `getAuthorizer`

``` purescript
getAuthorizer :: forall eff. GetAuthorizerRequest -> Aff (err :: RequestError | eff) Authorizer
```

<p>Describe an existing <a>Authorizer</a> resource.</p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-authorizer.html">AWS CLI</a></div>

#### `getAuthorizers`

``` purescript
getAuthorizers :: forall eff. GetAuthorizersRequest -> Aff (err :: RequestError | eff) Authorizers
```

<p>Describe an existing <a>Authorizers</a> resource.</p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-authorizers.html">AWS CLI</a></div>

#### `getBasePathMapping`

``` purescript
getBasePathMapping :: forall eff. GetBasePathMappingRequest -> Aff (err :: RequestError | eff) BasePathMapping
```

<p>Describe a <a>BasePathMapping</a> resource.</p>

#### `getBasePathMappings`

``` purescript
getBasePathMappings :: forall eff. GetBasePathMappingsRequest -> Aff (err :: RequestError | eff) BasePathMappings
```

<p>Represents a collection of <a>BasePathMapping</a> resources.</p>

#### `getClientCertificate`

``` purescript
getClientCertificate :: forall eff. GetClientCertificateRequest -> Aff (err :: RequestError | eff) ClientCertificate
```

<p>Gets information about the current <a>ClientCertificate</a> resource.</p>

#### `getClientCertificates`

``` purescript
getClientCertificates :: forall eff. GetClientCertificatesRequest -> Aff (err :: RequestError | eff) ClientCertificates
```

<p>Gets a collection of <a>ClientCertificate</a> resources.</p>

#### `getDeployment`

``` purescript
getDeployment :: forall eff. GetDeploymentRequest -> Aff (err :: RequestError | eff) Deployment
```

<p>Gets information about a <a>Deployment</a> resource.</p>

#### `getDeployments`

``` purescript
getDeployments :: forall eff. GetDeploymentsRequest -> Aff (err :: RequestError | eff) Deployments
```

<p>Gets information about a <a>Deployments</a> collection.</p>

#### `getDocumentationPart`

``` purescript
getDocumentationPart :: forall eff. GetDocumentationPartRequest -> Aff (err :: RequestError | eff) DocumentationPart
```

#### `getDocumentationParts`

``` purescript
getDocumentationParts :: forall eff. GetDocumentationPartsRequest -> Aff (err :: RequestError | eff) DocumentationParts
```

#### `getDocumentationVersion`

``` purescript
getDocumentationVersion :: forall eff. GetDocumentationVersionRequest -> Aff (err :: RequestError | eff) DocumentationVersion
```

#### `getDocumentationVersions`

``` purescript
getDocumentationVersions :: forall eff. GetDocumentationVersionsRequest -> Aff (err :: RequestError | eff) DocumentationVersions
```

#### `getDomainName`

``` purescript
getDomainName :: forall eff. GetDomainNameRequest -> Aff (err :: RequestError | eff) DomainName
```

<p>Represents a domain name that is contained in a simpler, more intuitive URL that can be called.</p>

#### `getDomainNames`

``` purescript
getDomainNames :: forall eff. GetDomainNamesRequest -> Aff (err :: RequestError | eff) DomainNames
```

<p>Represents a collection of <a>DomainName</a> resources.</p>

#### `getExport`

``` purescript
getExport :: forall eff. GetExportRequest -> Aff (err :: RequestError | eff) ExportResponse
```

<p>Exports a deployed version of a <a>RestApi</a> in a specified format.</p>

#### `getGatewayResponse`

``` purescript
getGatewayResponse :: forall eff. GetGatewayResponseRequest -> Aff (err :: RequestError | eff) GatewayResponse
```

<p>Gets a <a>GatewayResponse</a> of a specified response type on the given <a>RestApi</a>.</p>

#### `getGatewayResponses`

``` purescript
getGatewayResponses :: forall eff. GetGatewayResponsesRequest -> Aff (err :: RequestError | eff) GatewayResponses
```

<p>Gets the <a>GatewayResponses</a> collection on the given <a>RestApi</a>. If an API developer has not added any definitions for gateway responses, the result will be the API Gateway-generated default <a>GatewayResponses</a> collection for the supported response types.</p>

#### `getIntegration`

``` purescript
getIntegration :: forall eff. GetIntegrationRequest -> Aff (err :: RequestError | eff) Integration
```

<p>Get the integration settings.</p>

#### `getIntegrationResponse`

``` purescript
getIntegrationResponse :: forall eff. GetIntegrationResponseRequest -> Aff (err :: RequestError | eff) IntegrationResponse
```

<p>Represents a get integration response.</p>

#### `getMethod`

``` purescript
getMethod :: forall eff. GetMethodRequest -> Aff (err :: RequestError | eff) Method
```

<p>Describe an existing <a>Method</a> resource.</p>

#### `getMethodResponse`

``` purescript
getMethodResponse :: forall eff. GetMethodResponseRequest -> Aff (err :: RequestError | eff) MethodResponse
```

<p>Describes a <a>MethodResponse</a> resource.</p>

#### `getModel`

``` purescript
getModel :: forall eff. GetModelRequest -> Aff (err :: RequestError | eff) Model
```

<p>Describes an existing model defined for a <a>RestApi</a> resource.</p>

#### `getModelTemplate`

``` purescript
getModelTemplate :: forall eff. GetModelTemplateRequest -> Aff (err :: RequestError | eff) Template
```

<p>Generates a sample mapping template that can be used to transform a payload into the structure of a model.</p>

#### `getModels`

``` purescript
getModels :: forall eff. GetModelsRequest -> Aff (err :: RequestError | eff) Models
```

<p>Describes existing <a>Models</a> defined for a <a>RestApi</a> resource.</p>

#### `getRequestValidator`

``` purescript
getRequestValidator :: forall eff. GetRequestValidatorRequest -> Aff (err :: RequestError | eff) RequestValidator
```

<p>Gets a <a>RequestValidator</a> of a given <a>RestApi</a>.</p>

#### `getRequestValidators`

``` purescript
getRequestValidators :: forall eff. GetRequestValidatorsRequest -> Aff (err :: RequestError | eff) RequestValidators
```

<p>Gets the <a>RequestValidators</a> collection of a given <a>RestApi</a>.</p>

#### `getResource`

``` purescript
getResource :: forall eff. GetResourceRequest -> Aff (err :: RequestError | eff) Resource
```

<p>Lists information about a resource.</p>

#### `getResources`

``` purescript
getResources :: forall eff. GetResourcesRequest -> Aff (err :: RequestError | eff) Resources
```

<p>Lists information about a collection of <a>Resource</a> resources.</p>

#### `getRestApi`

``` purescript
getRestApi :: forall eff. GetRestApiRequest -> Aff (err :: RequestError | eff) RestApi
```

<p>Lists the <a>RestApi</a> resource in the collection.</p>

#### `getRestApis`

``` purescript
getRestApis :: forall eff. GetRestApisRequest -> Aff (err :: RequestError | eff) RestApis
```

<p>Lists the <a>RestApis</a> resources for your collection.</p>

#### `getSdk`

``` purescript
getSdk :: forall eff. GetSdkRequest -> Aff (err :: RequestError | eff) SdkResponse
```

<p>Generates a client SDK for a <a>RestApi</a> and <a>Stage</a>.</p>

#### `getSdkType`

``` purescript
getSdkType :: forall eff. GetSdkTypeRequest -> Aff (err :: RequestError | eff) SdkType
```

#### `getSdkTypes`

``` purescript
getSdkTypes :: forall eff. GetSdkTypesRequest -> Aff (err :: RequestError | eff) SdkTypes
```

#### `getStage`

``` purescript
getStage :: forall eff. GetStageRequest -> Aff (err :: RequestError | eff) Stage
```

<p>Gets information about a <a>Stage</a> resource.</p>

#### `getStages`

``` purescript
getStages :: forall eff. GetStagesRequest -> Aff (err :: RequestError | eff) Stages
```

<p>Gets information about one or more <a>Stage</a> resources.</p>

#### `getTags`

``` purescript
getTags :: forall eff. GetTagsRequest -> Aff (err :: RequestError | eff) Tags
```

<p>Gets the Tags collection for a given resource.</p>

#### `getUsage`

``` purescript
getUsage :: forall eff. GetUsageRequest -> Aff (err :: RequestError | eff) Usage
```

<p>Gets the usage data of a usage plan in a specified time interval.</p>

#### `getUsagePlan`

``` purescript
getUsagePlan :: forall eff. GetUsagePlanRequest -> Aff (err :: RequestError | eff) UsagePlan
```

<p>Gets a usage plan of a given plan identifier.</p>

#### `getUsagePlanKey`

``` purescript
getUsagePlanKey :: forall eff. GetUsagePlanKeyRequest -> Aff (err :: RequestError | eff) UsagePlanKey
```

<p>Gets a usage plan key of a given key identifier.</p>

#### `getUsagePlanKeys`

``` purescript
getUsagePlanKeys :: forall eff. GetUsagePlanKeysRequest -> Aff (err :: RequestError | eff) UsagePlanKeys
```

<p>Gets all the usage plan keys representing the API keys added to a specified usage plan.</p>

#### `getUsagePlans`

``` purescript
getUsagePlans :: forall eff. GetUsagePlansRequest -> Aff (err :: RequestError | eff) UsagePlans
```

<p>Gets all the usage plans of the caller's account.</p>

#### `getVpcLink`

``` purescript
getVpcLink :: forall eff. GetVpcLinkRequest -> Aff (err :: RequestError | eff) VpcLink
```

<p>Gets a specified VPC link under the caller's account in a region.</p>

#### `getVpcLinks`

``` purescript
getVpcLinks :: forall eff. GetVpcLinksRequest -> Aff (err :: RequestError | eff) VpcLinks
```

<p>Gets the <a>VpcLinks</a> collection under the caller's account in a selected region.</p>

#### `importApiKeys`

``` purescript
importApiKeys :: forall eff. ImportApiKeysRequest -> Aff (err :: RequestError | eff) ApiKeyIds
```

<p>Import API keys from an external source, such as a CSV-formatted file.</p>

#### `importDocumentationParts`

``` purescript
importDocumentationParts :: forall eff. ImportDocumentationPartsRequest -> Aff (err :: RequestError | eff) DocumentationPartIds
```

#### `importRestApi`

``` purescript
importRestApi :: forall eff. ImportRestApiRequest -> Aff (err :: RequestError | eff) RestApi
```

<p>A feature of the API Gateway control service for creating a new API from an external API definition file.</p>

#### `putGatewayResponse`

``` purescript
putGatewayResponse :: forall eff. PutGatewayResponseRequest -> Aff (err :: RequestError | eff) GatewayResponse
```

<p>Creates a customization of a <a>GatewayResponse</a> of a specified response type and status code on the given <a>RestApi</a>.</p>

#### `putIntegration`

``` purescript
putIntegration :: forall eff. PutIntegrationRequest -> Aff (err :: RequestError | eff) Integration
```

<p>Sets up a method's integration.</p>

#### `putIntegrationResponse`

``` purescript
putIntegrationResponse :: forall eff. PutIntegrationResponseRequest -> Aff (err :: RequestError | eff) IntegrationResponse
```

<p>Represents a put integration.</p>

#### `putMethod`

``` purescript
putMethod :: forall eff. PutMethodRequest -> Aff (err :: RequestError | eff) Method
```

<p>Add a method to an existing <a>Resource</a> resource.</p>

#### `putMethodResponse`

``` purescript
putMethodResponse :: forall eff. PutMethodResponseRequest -> Aff (err :: RequestError | eff) MethodResponse
```

<p>Adds a <a>MethodResponse</a> to an existing <a>Method</a> resource.</p>

#### `putRestApi`

``` purescript
putRestApi :: forall eff. PutRestApiRequest -> Aff (err :: RequestError | eff) RestApi
```

<p>A feature of the API Gateway control service for updating an existing API with an input of external API definitions. The update can take the form of merging the supplied definition into the existing API or overwriting the existing API.</p>

#### `tagResource`

``` purescript
tagResource :: forall eff. TagResourceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds or updates Tags on a gievn resource.</p>

#### `testInvokeAuthorizer`

``` purescript
testInvokeAuthorizer :: forall eff. TestInvokeAuthorizerRequest -> Aff (err :: RequestError | eff) TestInvokeAuthorizerResponse
```

<p>Simulate the execution of an <a>Authorizer</a> in your <a>RestApi</a> with headers, parameters, and an incoming request body.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/use-custom-authorizer.html">Enable custom authorizers</a> </div>

#### `testInvokeMethod`

``` purescript
testInvokeMethod :: forall eff. TestInvokeMethodRequest -> Aff (err :: RequestError | eff) TestInvokeMethodResponse
```

<p>Simulate the execution of a <a>Method</a> in your <a>RestApi</a> with headers, parameters, and an incoming request body.</p>

#### `untagResource`

``` purescript
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes Tags from a given resource.</p>

#### `updateAccount`

``` purescript
updateAccount :: forall eff. UpdateAccountRequest -> Aff (err :: RequestError | eff) Account
```

<p>Changes information about the current <a>Account</a> resource.</p>

#### `updateApiKey`

``` purescript
updateApiKey :: forall eff. UpdateApiKeyRequest -> Aff (err :: RequestError | eff) ApiKey
```

<p>Changes information about an <a>ApiKey</a> resource.</p>

#### `updateAuthorizer`

``` purescript
updateAuthorizer :: forall eff. UpdateAuthorizerRequest -> Aff (err :: RequestError | eff) Authorizer
```

<p>Updates an existing <a>Authorizer</a> resource.</p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/update-authorizer.html">AWS CLI</a></div>

#### `updateBasePathMapping`

``` purescript
updateBasePathMapping :: forall eff. UpdateBasePathMappingRequest -> Aff (err :: RequestError | eff) BasePathMapping
```

<p>Changes information about the <a>BasePathMapping</a> resource.</p>

#### `updateClientCertificate`

``` purescript
updateClientCertificate :: forall eff. UpdateClientCertificateRequest -> Aff (err :: RequestError | eff) ClientCertificate
```

<p>Changes information about an <a>ClientCertificate</a> resource.</p>

#### `updateDeployment`

``` purescript
updateDeployment :: forall eff. UpdateDeploymentRequest -> Aff (err :: RequestError | eff) Deployment
```

<p>Changes information about a <a>Deployment</a> resource.</p>

#### `updateDocumentationPart`

``` purescript
updateDocumentationPart :: forall eff. UpdateDocumentationPartRequest -> Aff (err :: RequestError | eff) DocumentationPart
```

#### `updateDocumentationVersion`

``` purescript
updateDocumentationVersion :: forall eff. UpdateDocumentationVersionRequest -> Aff (err :: RequestError | eff) DocumentationVersion
```

#### `updateDomainName`

``` purescript
updateDomainName :: forall eff. UpdateDomainNameRequest -> Aff (err :: RequestError | eff) DomainName
```

<p>Changes information about the <a>DomainName</a> resource.</p>

#### `updateGatewayResponse`

``` purescript
updateGatewayResponse :: forall eff. UpdateGatewayResponseRequest -> Aff (err :: RequestError | eff) GatewayResponse
```

<p>Updates a <a>GatewayResponse</a> of a specified response type on the given <a>RestApi</a>.</p>

#### `updateIntegration`

``` purescript
updateIntegration :: forall eff. UpdateIntegrationRequest -> Aff (err :: RequestError | eff) Integration
```

<p>Represents an update integration.</p>

#### `updateIntegrationResponse`

``` purescript
updateIntegrationResponse :: forall eff. UpdateIntegrationResponseRequest -> Aff (err :: RequestError | eff) IntegrationResponse
```

<p>Represents an update integration response.</p>

#### `updateMethod`

``` purescript
updateMethod :: forall eff. UpdateMethodRequest -> Aff (err :: RequestError | eff) Method
```

<p>Updates an existing <a>Method</a> resource.</p>

#### `updateMethodResponse`

``` purescript
updateMethodResponse :: forall eff. UpdateMethodResponseRequest -> Aff (err :: RequestError | eff) MethodResponse
```

<p>Updates an existing <a>MethodResponse</a> resource.</p>

#### `updateModel`

``` purescript
updateModel :: forall eff. UpdateModelRequest -> Aff (err :: RequestError | eff) Model
```

<p>Changes information about a model.</p>

#### `updateRequestValidator`

``` purescript
updateRequestValidator :: forall eff. UpdateRequestValidatorRequest -> Aff (err :: RequestError | eff) RequestValidator
```

<p>Updates a <a>RequestValidator</a> of a given <a>RestApi</a>.</p>

#### `updateResource`

``` purescript
updateResource :: forall eff. UpdateResourceRequest -> Aff (err :: RequestError | eff) Resource
```

<p>Changes information about a <a>Resource</a> resource.</p>

#### `updateRestApi`

``` purescript
updateRestApi :: forall eff. UpdateRestApiRequest -> Aff (err :: RequestError | eff) RestApi
```

<p>Changes information about the specified API.</p>

#### `updateStage`

``` purescript
updateStage :: forall eff. UpdateStageRequest -> Aff (err :: RequestError | eff) Stage
```

<p>Changes information about a <a>Stage</a> resource.</p>

#### `updateUsage`

``` purescript
updateUsage :: forall eff. UpdateUsageRequest -> Aff (err :: RequestError | eff) Usage
```

<p>Grants a temporary extension to the remaining quota of a usage plan associated with a specified API key.</p>

#### `updateUsagePlan`

``` purescript
updateUsagePlan :: forall eff. UpdateUsagePlanRequest -> Aff (err :: RequestError | eff) UsagePlan
```

<p>Updates a usage plan of a given plan Id.</p>

#### `updateVpcLink`

``` purescript
updateVpcLink :: forall eff. UpdateVpcLinkRequest -> Aff (err :: RequestError | eff) VpcLink
```

<p>Updates an existing <a>VpcLink</a> of a specified identifier.</p>

#### `AccessLogSettings`

``` purescript
newtype AccessLogSettings
  = AccessLogSettings { "Format'" :: NullOrUndefined (String), "DestinationArn'" :: NullOrUndefined (String) }
```

<p>Access log settings, including the access log format and access log destination ARN.</p>

##### Instances
``` purescript
Newtype AccessLogSettings _
```

#### `Account`

``` purescript
newtype Account
  = Account { "CloudwatchRoleArn'" :: NullOrUndefined (String), "ThrottleSettings'" :: NullOrUndefined (ThrottleSettings), "Features'" :: NullOrUndefined (ListOfString), "ApiKeyVersion'" :: NullOrUndefined (String) }
```

<p>Represents an AWS account that is associated with API Gateway.</p> <div class="remarks"> <p>To view the account info, call <code>GET</code> on this resource.</p> <h4>Error Codes</h4> <p>The following exception may be thrown when the request fails.</p> <ul> <li>UnauthorizedException</li> <li>NotFoundException</li> <li>TooManyRequestsException</li> </ul> <p>For detailed error code information, including the corresponding HTTP Status Codes, see <a href="http://docs.aws.amazon.com/apigateway/api-reference/handling-errors/#api-error-codes">API Gateway Error Codes</a></p> <h4>Example: Get the information about an account.</h4> <h5>Request</h5> <pre><code>GET /account HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160531T184618Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash} </code></pre> <h5>Response</h5> <p>The successful response returns a <code>200 OK</code> status code and a payload similar to the following:</p> <pre><code>{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/account-apigateway-{rel}.html", "name": "account", "templated": true }, "self": { "href": "/account" }, "account:update": { "href": "/account" } }, "cloudwatchRoleArn": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "throttleSettings": { "rateLimit": 500, "burstLimit": 1000 } } </code></pre> <p>In addition to making the REST API call directly, you can use the AWS CLI and an AWS SDK to access this resource.</p> </div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-limits.html">API Gateway Limits</a> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/welcome.html">Developer Guide</a>, <a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-account.html">AWS CLI</a> </div>

##### Instances
``` purescript
Newtype Account _
```

#### `ApiKey`

``` purescript
newtype ApiKey
  = ApiKey { "Id'" :: NullOrUndefined (String), "Value'" :: NullOrUndefined (String), "Name'" :: NullOrUndefined (String), "CustomerId'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "Enabled'" :: NullOrUndefined (Boolean), "CreatedDate'" :: NullOrUndefined (Number), "LastUpdatedDate'" :: NullOrUndefined (Number), "StageKeys'" :: NullOrUndefined (ListOfString) }
```

<p>A resource that can be distributed to callers for executing <a>Method</a> resources that require an API key. API keys can be mapped to any <a>Stage</a> on any <a>RestApi</a>, which indicates that the callers with the API key can make requests to that stage.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-api-keys.html">Use API Keys</a> </div>

##### Instances
``` purescript
Newtype ApiKey _
```

#### `ApiKeyIds`

``` purescript
newtype ApiKeyIds
  = ApiKeyIds { "Ids'" :: NullOrUndefined (ListOfString), "Warnings'" :: NullOrUndefined (ListOfString) }
```

<p>The identifier of an <a>ApiKey</a> used in a <a>UsagePlan</a>.</p>

##### Instances
``` purescript
Newtype ApiKeyIds _
```

#### `ApiKeySourceType`

``` purescript
newtype ApiKeySourceType
  = ApiKeySourceType String
```

##### Instances
``` purescript
Newtype ApiKeySourceType _
```

#### `ApiKeys`

``` purescript
newtype ApiKeys
  = ApiKeys { "Warnings'" :: NullOrUndefined (ListOfString), "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfApiKey) }
```

<p>Represents a collection of API keys as represented by an <a>ApiKeys</a> resource.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-api-keys.html">Use API Keys</a> </div>

##### Instances
``` purescript
Newtype ApiKeys _
```

#### `ApiKeysFormat`

``` purescript
newtype ApiKeysFormat
  = ApiKeysFormat String
```

##### Instances
``` purescript
Newtype ApiKeysFormat _
```

#### `ApiStage`

``` purescript
newtype ApiStage
  = ApiStage { "ApiId'" :: NullOrUndefined (String), "Stage'" :: NullOrUndefined (String) }
```

<p>API stage name of the associated API stage in a usage plan.</p>

##### Instances
``` purescript
Newtype ApiStage _
```

#### `Authorizer`

``` purescript
newtype Authorizer
  = Authorizer { "Id'" :: NullOrUndefined (String), "Name'" :: NullOrUndefined (String), "Type'" :: NullOrUndefined (AuthorizerType), "ProviderARNs'" :: NullOrUndefined (ListOfARNs), "AuthType'" :: NullOrUndefined (String), "AuthorizerUri'" :: NullOrUndefined (String), "AuthorizerCredentials'" :: NullOrUndefined (String), "IdentitySource'" :: NullOrUndefined (String), "IdentityValidationExpression'" :: NullOrUndefined (String), "AuthorizerResultTtlInSeconds'" :: NullOrUndefined (NullableInteger) }
```

<p>Represents an authorization layer for methods. If enabled on a method, API Gateway will activate the authorizer when a client calls the method.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/use-custom-authorizer.html">Enable custom authorization</a> </div>

##### Instances
``` purescript
Newtype Authorizer _
```

#### `AuthorizerType`

``` purescript
newtype AuthorizerType
  = AuthorizerType String
```

<p>[Required] The authorizer type. Valid values are <code>TOKEN</code> for a Lambda function using a single authorization token submitted in a custom header, <code>REQUEST</code> for a Lambda function using incoming request parameters, and <code>COGNITO_USER_POOLS</code> for using an Amazon Cognito user pool.</p>

##### Instances
``` purescript
Newtype AuthorizerType _
```

#### `Authorizers`

``` purescript
newtype Authorizers
  = Authorizers { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfAuthorizer) }
```

<p>Represents a collection of <a>Authorizer</a> resources.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/use-custom-authorizer.html">Enable custom authorization</a> </div>

##### Instances
``` purescript
Newtype Authorizers _
```

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message'" :: NullOrUndefined (String) }
```

<p>The submitted request is not valid, for example, the input is incomplete or incorrect. See the accompanying error message for details.</p>

##### Instances
``` purescript
Newtype BadRequestException _
```

#### `BasePathMapping`

``` purescript
newtype BasePathMapping
  = BasePathMapping { "BasePath'" :: NullOrUndefined (String), "RestApiId'" :: NullOrUndefined (String), "Stage'" :: NullOrUndefined (String) }
```

<p>Represents the base path that callers of the API must provide as part of the URL after the domain name.</p> <div class="remarks">A custom domain name plus a <code>BasePathMapping</code> specification identifies a deployed <a>RestApi</a> in a given stage of the owner <a>Account</a>.</div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html">Use Custom Domain Names</a> </div>

##### Instances
``` purescript
Newtype BasePathMapping _
```

#### `BasePathMappings`

``` purescript
newtype BasePathMappings
  = BasePathMappings { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfBasePathMapping) }
```

<p>Represents a collection of <a>BasePathMapping</a> resources.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html">Use Custom Domain Names</a> </div>

##### Instances
``` purescript
Newtype BasePathMappings _
```

#### `CacheClusterSize`

``` purescript
newtype CacheClusterSize
  = CacheClusterSize String
```

<p>Returns the size of the <b>CacheCluster</b>.</p>

##### Instances
``` purescript
Newtype CacheClusterSize _
```

#### `CacheClusterStatus`

``` purescript
newtype CacheClusterStatus
  = CacheClusterStatus String
```

<p>Returns the status of the <b>CacheCluster</b>.</p>

##### Instances
``` purescript
Newtype CacheClusterStatus _
```

#### `CanarySettings`

``` purescript
newtype CanarySettings
  = CanarySettings { "PercentTraffic'" :: NullOrUndefined (Number), "DeploymentId'" :: NullOrUndefined (String), "StageVariableOverrides'" :: NullOrUndefined (MapOfStringToString), "UseStageCache'" :: NullOrUndefined (Boolean) }
```

<p>Configuration settings of a canary deployment.</p>

##### Instances
``` purescript
Newtype CanarySettings _
```

#### `ClientCertificate`

``` purescript
newtype ClientCertificate
  = ClientCertificate { "ClientCertificateId'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "PemEncodedCertificate'" :: NullOrUndefined (String), "CreatedDate'" :: NullOrUndefined (Number), "ExpirationDate'" :: NullOrUndefined (Number) }
```

<p>Represents a client certificate used to configure client-side SSL authentication while sending requests to the integration endpoint.</p> <div class="remarks">Client certificates are used to authenticate an API by the backend server. To authenticate an API client (or user), use IAM roles and policies, a custom <a>Authorizer</a> or an Amazon Cognito user pool.</div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-client-side-ssl-authentication.html">Use Client-Side Certificate</a> </div>

##### Instances
``` purescript
Newtype ClientCertificate _
```

#### `ClientCertificates`

``` purescript
newtype ClientCertificates
  = ClientCertificates { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfClientCertificate) }
```

<p>Represents a collection of <a>ClientCertificate</a> resources.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-client-side-ssl-authentication.html">Use Client-Side Certificate</a> </div>

##### Instances
``` purescript
Newtype ClientCertificates _
```

#### `ConflictException`

``` purescript
newtype ConflictException
  = ConflictException { "Message'" :: NullOrUndefined (String) }
```

<p>The request configuration has conflicts. For details, see the accompanying error message.</p>

##### Instances
``` purescript
Newtype ConflictException _
```

#### `ConnectionType`

``` purescript
newtype ConnectionType
  = ConnectionType String
```

##### Instances
``` purescript
Newtype ConnectionType _
```

#### `ContentHandlingStrategy`

``` purescript
newtype ContentHandlingStrategy
  = ContentHandlingStrategy String
```

##### Instances
``` purescript
Newtype ContentHandlingStrategy _
```

#### `CreateApiKeyRequest`

``` purescript
newtype CreateApiKeyRequest
  = CreateApiKeyRequest { "Name'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "Enabled'" :: NullOrUndefined (Boolean), "GenerateDistinctId'" :: NullOrUndefined (Boolean), "Value'" :: NullOrUndefined (String), "StageKeys'" :: NullOrUndefined (ListOfStageKeys), "CustomerId'" :: NullOrUndefined (String) }
```

<p>Request to create an <a>ApiKey</a> resource.</p>

##### Instances
``` purescript
Newtype CreateApiKeyRequest _
```

#### `CreateAuthorizerRequest`

``` purescript
newtype CreateAuthorizerRequest
  = CreateAuthorizerRequest { "RestApiId'" :: String, "Name'" :: String, "Type'" :: AuthorizerType, "ProviderARNs'" :: NullOrUndefined (ListOfARNs), "AuthType'" :: NullOrUndefined (String), "AuthorizerUri'" :: NullOrUndefined (String), "AuthorizerCredentials'" :: NullOrUndefined (String), "IdentitySource'" :: NullOrUndefined (String), "IdentityValidationExpression'" :: NullOrUndefined (String), "AuthorizerResultTtlInSeconds'" :: NullOrUndefined (NullableInteger) }
```

<p>Request to add a new <a>Authorizer</a> to an existing <a>RestApi</a> resource.</p>

##### Instances
``` purescript
Newtype CreateAuthorizerRequest _
```

#### `CreateBasePathMappingRequest`

``` purescript
newtype CreateBasePathMappingRequest
  = CreateBasePathMappingRequest { "DomainName'" :: String, "BasePath'" :: NullOrUndefined (String), "RestApiId'" :: String, "Stage'" :: NullOrUndefined (String) }
```

<p>Requests API Gateway to create a new <a>BasePathMapping</a> resource.</p>

##### Instances
``` purescript
Newtype CreateBasePathMappingRequest _
```

#### `CreateDeploymentRequest`

``` purescript
newtype CreateDeploymentRequest
  = CreateDeploymentRequest { "RestApiId'" :: String, "StageName'" :: NullOrUndefined (String), "StageDescription'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "CacheClusterEnabled'" :: NullOrUndefined (NullableBoolean), "CacheClusterSize'" :: NullOrUndefined (CacheClusterSize), "Variables'" :: NullOrUndefined (MapOfStringToString), "CanarySettings'" :: NullOrUndefined (DeploymentCanarySettings) }
```

<p>Requests API Gateway to create a <a>Deployment</a> resource.</p>

##### Instances
``` purescript
Newtype CreateDeploymentRequest _
```

#### `CreateDocumentationPartRequest`

``` purescript
newtype CreateDocumentationPartRequest
  = CreateDocumentationPartRequest { "RestApiId'" :: String, "Location'" :: DocumentationPartLocation, "Properties'" :: String }
```

<p>Creates a new documentation part of a given API.</p>

##### Instances
``` purescript
Newtype CreateDocumentationPartRequest _
```

#### `CreateDocumentationVersionRequest`

``` purescript
newtype CreateDocumentationVersionRequest
  = CreateDocumentationVersionRequest { "RestApiId'" :: String, "DocumentationVersion'" :: String, "StageName'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String) }
```

<p>Creates a new documentation version of a given API.</p>

##### Instances
``` purescript
Newtype CreateDocumentationVersionRequest _
```

#### `CreateDomainNameRequest`

``` purescript
newtype CreateDomainNameRequest
  = CreateDomainNameRequest { "DomainName'" :: String, "CertificateName'" :: NullOrUndefined (String), "CertificateBody'" :: NullOrUndefined (String), "CertificatePrivateKey'" :: NullOrUndefined (String), "CertificateChain'" :: NullOrUndefined (String), "CertificateArn'" :: NullOrUndefined (String), "RegionalCertificateName'" :: NullOrUndefined (String), "RegionalCertificateArn'" :: NullOrUndefined (String), "EndpointConfiguration'" :: NullOrUndefined (EndpointConfiguration) }
```

<p>A request to create a new domain name.</p>

##### Instances
``` purescript
Newtype CreateDomainNameRequest _
```

#### `CreateModelRequest`

``` purescript
newtype CreateModelRequest
  = CreateModelRequest { "RestApiId'" :: String, "Name'" :: String, "Description'" :: NullOrUndefined (String), "Schema'" :: NullOrUndefined (String), "ContentType'" :: String }
```

<p>Request to add a new <a>Model</a> to an existing <a>RestApi</a> resource.</p>

##### Instances
``` purescript
Newtype CreateModelRequest _
```

#### `CreateRequestValidatorRequest`

``` purescript
newtype CreateRequestValidatorRequest
  = CreateRequestValidatorRequest { "RestApiId'" :: String, "Name'" :: NullOrUndefined (String), "ValidateRequestBody'" :: NullOrUndefined (Boolean), "ValidateRequestParameters'" :: NullOrUndefined (Boolean) }
```

<p>Creates a <a>RequestValidator</a> of a given <a>RestApi</a>.</p>

##### Instances
``` purescript
Newtype CreateRequestValidatorRequest _
```

#### `CreateResourceRequest`

``` purescript
newtype CreateResourceRequest
  = CreateResourceRequest { "RestApiId'" :: String, "ParentId'" :: String, "PathPart'" :: String }
```

<p>Requests API Gateway to create a <a>Resource</a> resource.</p>

##### Instances
``` purescript
Newtype CreateResourceRequest _
```

#### `CreateRestApiRequest`

``` purescript
newtype CreateRestApiRequest
  = CreateRestApiRequest { "Name'" :: String, "Description'" :: NullOrUndefined (String), "Version'" :: NullOrUndefined (String), "CloneFrom'" :: NullOrUndefined (String), "BinaryMediaTypes'" :: NullOrUndefined (ListOfString), "MinimumCompressionSize'" :: NullOrUndefined (NullableInteger), "ApiKeySource'" :: NullOrUndefined (ApiKeySourceType), "EndpointConfiguration'" :: NullOrUndefined (EndpointConfiguration) }
```

<p>The POST Request to add a new <a>RestApi</a> resource to your collection.</p>

##### Instances
``` purescript
Newtype CreateRestApiRequest _
```

#### `CreateStageRequest`

``` purescript
newtype CreateStageRequest
  = CreateStageRequest { "RestApiId'" :: String, "StageName'" :: String, "DeploymentId'" :: String, "Description'" :: NullOrUndefined (String), "CacheClusterEnabled'" :: NullOrUndefined (Boolean), "CacheClusterSize'" :: NullOrUndefined (CacheClusterSize), "Variables'" :: NullOrUndefined (MapOfStringToString), "DocumentationVersion'" :: NullOrUndefined (String), "CanarySettings'" :: NullOrUndefined (CanarySettings), "Tags'" :: NullOrUndefined (MapOfStringToString) }
```

<p>Requests API Gateway to create a <a>Stage</a> resource.</p>

##### Instances
``` purescript
Newtype CreateStageRequest _
```

#### `CreateUsagePlanKeyRequest`

``` purescript
newtype CreateUsagePlanKeyRequest
  = CreateUsagePlanKeyRequest { "UsagePlanId'" :: String, "KeyId'" :: String, "KeyType'" :: String }
```

<p>The POST request to create a usage plan key for adding an existing API key to a usage plan.</p>

##### Instances
``` purescript
Newtype CreateUsagePlanKeyRequest _
```

#### `CreateUsagePlanRequest`

``` purescript
newtype CreateUsagePlanRequest
  = CreateUsagePlanRequest { "Name'" :: String, "Description'" :: NullOrUndefined (String), "ApiStages'" :: NullOrUndefined (ListOfApiStage), "Throttle'" :: NullOrUndefined (ThrottleSettings), "Quota'" :: NullOrUndefined (QuotaSettings) }
```

<p>The POST request to create a usage plan with the name, description, throttle limits and quota limits, as well as the associated API stages, specified in the payload.</p>

##### Instances
``` purescript
Newtype CreateUsagePlanRequest _
```

#### `CreateVpcLinkRequest`

``` purescript
newtype CreateVpcLinkRequest
  = CreateVpcLinkRequest { "Name'" :: String, "Description'" :: NullOrUndefined (String), "TargetArns'" :: ListOfString }
```

<p>Creates a VPC link, under the caller's account in a selected region, in an asynchronous operation that typically takes 2-4 minutes to complete and become operational. The caller must have permissions to create and update VPC Endpoint services.</p>

##### Instances
``` purescript
Newtype CreateVpcLinkRequest _
```

#### `DeleteApiKeyRequest`

``` purescript
newtype DeleteApiKeyRequest
  = DeleteApiKeyRequest { "ApiKey'" :: String }
```

<p>A request to delete the <a>ApiKey</a> resource.</p>

##### Instances
``` purescript
Newtype DeleteApiKeyRequest _
```

#### `DeleteAuthorizerRequest`

``` purescript
newtype DeleteAuthorizerRequest
  = DeleteAuthorizerRequest { "RestApiId'" :: String, "AuthorizerId'" :: String }
```

<p>Request to delete an existing <a>Authorizer</a> resource.</p>

##### Instances
``` purescript
Newtype DeleteAuthorizerRequest _
```

#### `DeleteBasePathMappingRequest`

``` purescript
newtype DeleteBasePathMappingRequest
  = DeleteBasePathMappingRequest { "DomainName'" :: String, "BasePath'" :: String }
```

<p>A request to delete the <a>BasePathMapping</a> resource.</p>

##### Instances
``` purescript
Newtype DeleteBasePathMappingRequest _
```

#### `DeleteClientCertificateRequest`

``` purescript
newtype DeleteClientCertificateRequest
  = DeleteClientCertificateRequest { "ClientCertificateId'" :: String }
```

<p>A request to delete the <a>ClientCertificate</a> resource.</p>

##### Instances
``` purescript
Newtype DeleteClientCertificateRequest _
```

#### `DeleteDeploymentRequest`

``` purescript
newtype DeleteDeploymentRequest
  = DeleteDeploymentRequest { "RestApiId'" :: String, "DeploymentId'" :: String }
```

<p>Requests API Gateway to delete a <a>Deployment</a> resource.</p>

##### Instances
``` purescript
Newtype DeleteDeploymentRequest _
```

#### `DeleteDocumentationPartRequest`

``` purescript
newtype DeleteDocumentationPartRequest
  = DeleteDocumentationPartRequest { "RestApiId'" :: String, "DocumentationPartId'" :: String }
```

<p>Deletes an existing documentation part of an API.</p>

##### Instances
``` purescript
Newtype DeleteDocumentationPartRequest _
```

#### `DeleteDocumentationVersionRequest`

``` purescript
newtype DeleteDocumentationVersionRequest
  = DeleteDocumentationVersionRequest { "RestApiId'" :: String, "DocumentationVersion'" :: String }
```

<p>Deletes an existing documentation version of an API.</p>

##### Instances
``` purescript
Newtype DeleteDocumentationVersionRequest _
```

#### `DeleteDomainNameRequest`

``` purescript
newtype DeleteDomainNameRequest
  = DeleteDomainNameRequest { "DomainName'" :: String }
```

<p>A request to delete the <a>DomainName</a> resource.</p>

##### Instances
``` purescript
Newtype DeleteDomainNameRequest _
```

#### `DeleteGatewayResponseRequest`

``` purescript
newtype DeleteGatewayResponseRequest
  = DeleteGatewayResponseRequest { "RestApiId'" :: String, "ResponseType'" :: GatewayResponseType }
```

<p>Clears any customization of a <a>GatewayResponse</a> of a specified response type on the given <a>RestApi</a> and resets it with the default settings.</p>

##### Instances
``` purescript
Newtype DeleteGatewayResponseRequest _
```

#### `DeleteIntegrationRequest`

``` purescript
newtype DeleteIntegrationRequest
  = DeleteIntegrationRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String }
```

<p>Represents a delete integration request.</p>

##### Instances
``` purescript
Newtype DeleteIntegrationRequest _
```

#### `DeleteIntegrationResponseRequest`

``` purescript
newtype DeleteIntegrationResponseRequest
  = DeleteIntegrationResponseRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "StatusCode'" :: StatusCode }
```

<p>Represents a delete integration response request.</p>

##### Instances
``` purescript
Newtype DeleteIntegrationResponseRequest _
```

#### `DeleteMethodRequest`

``` purescript
newtype DeleteMethodRequest
  = DeleteMethodRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String }
```

<p>Request to delete an existing <a>Method</a> resource.</p>

##### Instances
``` purescript
Newtype DeleteMethodRequest _
```

#### `DeleteMethodResponseRequest`

``` purescript
newtype DeleteMethodResponseRequest
  = DeleteMethodResponseRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "StatusCode'" :: StatusCode }
```

<p>A request to delete an existing <a>MethodResponse</a> resource.</p>

##### Instances
``` purescript
Newtype DeleteMethodResponseRequest _
```

#### `DeleteModelRequest`

``` purescript
newtype DeleteModelRequest
  = DeleteModelRequest { "RestApiId'" :: String, "ModelName'" :: String }
```

<p>Request to delete an existing model in an existing <a>RestApi</a> resource.</p>

##### Instances
``` purescript
Newtype DeleteModelRequest _
```

#### `DeleteRequestValidatorRequest`

``` purescript
newtype DeleteRequestValidatorRequest
  = DeleteRequestValidatorRequest { "RestApiId'" :: String, "RequestValidatorId'" :: String }
```

<p>Deletes a specified <a>RequestValidator</a> of a given <a>RestApi</a>.</p>

##### Instances
``` purescript
Newtype DeleteRequestValidatorRequest _
```

#### `DeleteResourceRequest`

``` purescript
newtype DeleteResourceRequest
  = DeleteResourceRequest { "RestApiId'" :: String, "ResourceId'" :: String }
```

<p>Request to delete a <a>Resource</a>.</p>

##### Instances
``` purescript
Newtype DeleteResourceRequest _
```

#### `DeleteRestApiRequest`

``` purescript
newtype DeleteRestApiRequest
  = DeleteRestApiRequest { "RestApiId'" :: String }
```

<p>Request to delete the specified API from your collection.</p>

##### Instances
``` purescript
Newtype DeleteRestApiRequest _
```

#### `DeleteStageRequest`

``` purescript
newtype DeleteStageRequest
  = DeleteStageRequest { "RestApiId'" :: String, "StageName'" :: String }
```

<p>Requests API Gateway to delete a <a>Stage</a> resource.</p>

##### Instances
``` purescript
Newtype DeleteStageRequest _
```

#### `DeleteUsagePlanKeyRequest`

``` purescript
newtype DeleteUsagePlanKeyRequest
  = DeleteUsagePlanKeyRequest { "UsagePlanId'" :: String, "KeyId'" :: String }
```

<p>The DELETE request to delete a usage plan key and remove the underlying API key from the associated usage plan.</p>

##### Instances
``` purescript
Newtype DeleteUsagePlanKeyRequest _
```

#### `DeleteUsagePlanRequest`

``` purescript
newtype DeleteUsagePlanRequest
  = DeleteUsagePlanRequest { "UsagePlanId'" :: String }
```

<p>The DELETE request to delete a usage plan of a given plan Id.</p>

##### Instances
``` purescript
Newtype DeleteUsagePlanRequest _
```

#### `DeleteVpcLinkRequest`

``` purescript
newtype DeleteVpcLinkRequest
  = DeleteVpcLinkRequest { "VpcLinkId'" :: String }
```

<p>Deletes an existing <a>VpcLink</a> of a specified identifier.</p>

##### Instances
``` purescript
Newtype DeleteVpcLinkRequest _
```

#### `Deployment`

``` purescript
newtype Deployment
  = Deployment { "Id'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "CreatedDate'" :: NullOrUndefined (Number), "ApiSummary'" :: NullOrUndefined (PathToMapOfMethodSnapshot) }
```

<p>An immutable representation of a <a>RestApi</a> resource that can be called by users using <a>Stages</a>. A deployment must be associated with a <a>Stage</a> for it to be callable over the Internet.</p> <div class="remarks">To create a deployment, call <code>POST</code> on the <a>Deployments</a> resource of a <a>RestApi</a>. To view, update, or delete a deployment, call <code>GET</code>, <code>PATCH</code>, or <code>DELETE</code> on the specified deployment resource (<code>/restapis/{restapi_id}/deployments/{deployment_id}</code>).</div> <div class="seeAlso"><a>RestApi</a>, <a>Deployments</a>, <a>Stage</a>, <a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html">AWS CLI</a>, <a href="https://aws.amazon.com/tools/">AWS SDKs</a> </div>

##### Instances
``` purescript
Newtype Deployment _
```

#### `DeploymentCanarySettings`

``` purescript
newtype DeploymentCanarySettings
  = DeploymentCanarySettings { "PercentTraffic'" :: NullOrUndefined (Number), "StageVariableOverrides'" :: NullOrUndefined (MapOfStringToString), "UseStageCache'" :: NullOrUndefined (Boolean) }
```

<p>The input configuration for a canary deployment.</p>

##### Instances
``` purescript
Newtype DeploymentCanarySettings _
```

#### `Deployments`

``` purescript
newtype Deployments
  = Deployments { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfDeployment) }
```

<p>Represents a collection resource that contains zero or more references to your existing deployments, and links that guide you on how to interact with your collection. The collection offers a paginated view of the contained deployments.</p> <div class="remarks">To create a new deployment of a <a>RestApi</a>, make a <code>POST</code> request against this resource. To view, update, or delete an existing deployment, make a <code>GET</code>, <code>PATCH</code>, or <code>DELETE</code> request, respectively, on a specified <a>Deployment</a> resource.</div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html">Deploying an API</a>, <a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html">AWS CLI</a>, <a href="https://aws.amazon.com/tools/">AWS SDKs</a> </div>

##### Instances
``` purescript
Newtype Deployments _
```

#### `DocumentationPart`

``` purescript
newtype DocumentationPart
  = DocumentationPart { "Id'" :: NullOrUndefined (String), "Location'" :: NullOrUndefined (DocumentationPartLocation), "Properties'" :: NullOrUndefined (String) }
```

<p>A documentation part for a targeted API entity.</p> <div class="remarks"> <p>A documentation part consists of a content map (<code>properties</code>) and a target (<code>location</code>). The target specifies an API entity to which the documentation content applies. The supported API entity types are <code>API</code>, <code>AUTHORIZER</code>, <code>MODEL</code>, <code>RESOURCE</code>, <code>METHOD</code>, <code>PATH_PARAMETER</code>, <code>QUERY_PARAMETER</code>, <code>REQUEST_HEADER</code>, <code>REQUEST_BODY</code>, <code>RESPONSE</code>, <code>RESPONSE_HEADER</code>, and <code>RESPONSE_BODY</code>. Valid <code>location</code> fields depend on the API entity type. All valid fields are not required.</p> <p>The content map is a JSON string of API-specific key-value pairs. Although an API can use any shape for the content map, only the Swagger-compliant documentation fields will be injected into the associated API entity definition in the exported Swagger definition file.</p></div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html">Documenting an API</a>, <a>DocumentationParts</a> </div>

##### Instances
``` purescript
Newtype DocumentationPart _
```

#### `DocumentationPartIds`

``` purescript
newtype DocumentationPartIds
  = DocumentationPartIds { "Ids'" :: NullOrUndefined (ListOfString), "Warnings'" :: NullOrUndefined (ListOfString) }
```

<p>A collection of the imported <a>DocumentationPart</a> identifiers.</p> <div class="remarks">This is used to return the result when documentation parts in an external (e.g., Swagger) file are imported into API Gateway</div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html">Documenting an API</a>, <a href="http://docs.aws.amazon.com/apigateway/api-reference/link-relation/documentationpart-import/">documentationpart:import</a>, <a>DocumentationPart</a> </div>

##### Instances
``` purescript
Newtype DocumentationPartIds _
```

#### `DocumentationPartLocation`

``` purescript
newtype DocumentationPartLocation
  = DocumentationPartLocation { "Type'" :: DocumentationPartType, "Path'" :: NullOrUndefined (String), "Method'" :: NullOrUndefined (String), "StatusCode'" :: NullOrUndefined (DocumentationPartLocationStatusCode), "Name'" :: NullOrUndefined (String) }
```

<p>Specifies the target API entity to which the documentation applies.</p>

##### Instances
``` purescript
Newtype DocumentationPartLocation _
```

#### `DocumentationPartLocationStatusCode`

``` purescript
newtype DocumentationPartLocationStatusCode
  = DocumentationPartLocationStatusCode String
```

##### Instances
``` purescript
Newtype DocumentationPartLocationStatusCode _
```

#### `DocumentationPartType`

``` purescript
newtype DocumentationPartType
  = DocumentationPartType String
```

##### Instances
``` purescript
Newtype DocumentationPartType _
```

#### `DocumentationParts`

``` purescript
newtype DocumentationParts
  = DocumentationParts { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfDocumentationPart) }
```

<p>The collection of documentation parts of an API.</p> <div class="remarks"/> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html">Documenting an API</a>, <a>DocumentationPart</a> </div>

##### Instances
``` purescript
Newtype DocumentationParts _
```

#### `DocumentationVersion`

``` purescript
newtype DocumentationVersion
  = DocumentationVersion { "Version'" :: NullOrUndefined (String), "CreatedDate'" :: NullOrUndefined (Number), "Description'" :: NullOrUndefined (String) }
```

<p>A snapshot of the documentation of an API.</p> <div class="remarks"><p>Publishing API documentation involves creating a documentation version associated with an API stage and exporting the versioned documentation to an external (e.g., Swagger) file.</p></div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html">Documenting an API</a>, <a>DocumentationPart</a>, <a>DocumentationVersions</a> </div>

##### Instances
``` purescript
Newtype DocumentationVersion _
```

#### `DocumentationVersions`

``` purescript
newtype DocumentationVersions
  = DocumentationVersions { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfDocumentationVersion) }
```

<p>The collection of documentation snapshots of an API. </p> <div class="remarks"><p>Use the <a>DocumentationVersions</a> to manage documentation snapshots associated with various API stages.</p></div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html">Documenting an API</a>, <a>DocumentationPart</a>, <a>DocumentationVersion</a> </div>

##### Instances
``` purescript
Newtype DocumentationVersions _
```

#### `DomainName`

``` purescript
newtype DomainName
  = DomainName { "DomainName'" :: NullOrUndefined (String), "CertificateName'" :: NullOrUndefined (String), "CertificateArn'" :: NullOrUndefined (String), "CertificateUploadDate'" :: NullOrUndefined (Number), "RegionalDomainName'" :: NullOrUndefined (String), "RegionalHostedZoneId'" :: NullOrUndefined (String), "RegionalCertificateName'" :: NullOrUndefined (String), "RegionalCertificateArn'" :: NullOrUndefined (String), "DistributionDomainName'" :: NullOrUndefined (String), "DistributionHostedZoneId'" :: NullOrUndefined (String), "EndpointConfiguration'" :: NullOrUndefined (EndpointConfiguration) }
```

<p>Represents a custom domain name as a user-friendly host name of an API (<a>RestApi</a>).</p> <div class="Remarks"> <p>When you deploy an API, API Gateway creates a default host name for the API. This default API host name is of the <code>{restapi-id}.execute-api.{region}.amazonaws.com</code> format. With the default host name, you can access the API's root resource with the URL of <code>https://{restapi-id}.execute-api.{region}.amazonaws.com/{stage}/</code>. When you set up a custom domain name of <code>apis.example.com</code> for this API, you can then access the same resource using the URL of the <code>https://apis.examples.com/myApi</code>, where <code>myApi</code> is the base path mapping (<a>BasePathMapping</a>) of your API under the custom domain name. </p> </div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html">Set a Custom Host Name for an API</a> </div>

##### Instances
``` purescript
Newtype DomainName _
```

#### `DomainNames`

``` purescript
newtype DomainNames
  = DomainNames { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfDomainName) }
```

<p>Represents a collection of <a>DomainName</a> resources.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html">Use Client-Side Certificate</a> </div>

##### Instances
``` purescript
Newtype DomainNames _
```

#### `EndpointConfiguration`

``` purescript
newtype EndpointConfiguration
  = EndpointConfiguration { "Types'" :: NullOrUndefined (ListOfEndpointType) }
```

<p>The endpoint configuration to indicate the types of endpoints an API (<a>RestApi</a>) or its custom domain name (<a>DomainName</a>) has. </p>

##### Instances
``` purescript
Newtype EndpointConfiguration _
```

#### `EndpointType`

``` purescript
newtype EndpointType
  = EndpointType String
```

<p>The endpoint type. The valid value is <code>EDGE</code> for edge-optimized API setup, most suitable for mobile applications, <code>REGIONAL</code> for regional API endpoint setup, most suitable for calling from AWS Region</p>

##### Instances
``` purescript
Newtype EndpointType _
```

#### `ExportResponse`

``` purescript
newtype ExportResponse
  = ExportResponse { "ContentType'" :: NullOrUndefined (String), "ContentDisposition'" :: NullOrUndefined (String), "Body'" :: NullOrUndefined (String) }
```

<p>The binary blob response to <a>GetExport</a>, which contains the generated SDK.</p>

##### Instances
``` purescript
Newtype ExportResponse _
```

#### `FlushStageAuthorizersCacheRequest`

``` purescript
newtype FlushStageAuthorizersCacheRequest
  = FlushStageAuthorizersCacheRequest { "RestApiId'" :: String, "StageName'" :: String }
```

<p>Request to flush authorizer cache entries on a specified stage.</p>

##### Instances
``` purescript
Newtype FlushStageAuthorizersCacheRequest _
```

#### `FlushStageCacheRequest`

``` purescript
newtype FlushStageCacheRequest
  = FlushStageCacheRequest { "RestApiId'" :: String, "StageName'" :: String }
```

<p>Requests API Gateway to flush a stage's cache.</p>

##### Instances
``` purescript
Newtype FlushStageCacheRequest _
```

#### `GatewayResponse`

``` purescript
newtype GatewayResponse
  = GatewayResponse { "ResponseType'" :: NullOrUndefined (GatewayResponseType), "StatusCode'" :: NullOrUndefined (StatusCode), "ResponseParameters'" :: NullOrUndefined (MapOfStringToString), "ResponseTemplates'" :: NullOrUndefined (MapOfStringToString), "DefaultResponse'" :: NullOrUndefined (Boolean) }
```

<p>A gateway response of a given response type and status code, with optional response parameters and mapping templates.</p> <div class="remarks"> For more information about valid gateway response types, see <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/supported-gateway-response-types.html">Gateway Response Types Supported by API Gateway</a> <div class="example"> <h4>Example: Get a Gateway Response of a given response type</h4> <h5>Request</h5> <p>This example shows how to get a gateway response of the <code>MISSING_AUTHENTICATION_TOKEN</code> type.</p> <pre><code>GET /restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN HTTP/1.1 Host: beta-apigateway.us-east-1.amazonaws.com Content-Type: application/json X-Amz-Date: 20170503T202516Z Authorization: AWS4-HMAC-SHA256 Credential={access-key-id}/20170503/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature=1b52460e3159c1a26cff29093855d50ea141c1c5b937528fecaf60f51129697a Cache-Control: no-cache Postman-Token: 3b2a1ce9-c848-2e26-2e2f-9c2caefbed45 </code></pre> <p>The response type is specified as a URL path.</p> <h5>Response</h5> <p>The successful operation returns the <code>200 OK</code> status code and a payload similar to the following:</p> <pre><code>{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-gatewayresponse-{rel}.html", "name": "gatewayresponse", "templated": true }, "self": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" }, "gatewayresponse:delete": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" } }, "defaultResponse": false, "responseParameters": { "gatewayresponse.header.x-request-path": "method.request.path.petId", "gatewayresponse.header.Access-Control-Allow-Origin": "&apos;a.b.c&apos;", "gatewayresponse.header.x-request-query": "method.request.querystring.q", "gatewayresponse.header.x-request-header": "method.request.header.Accept" }, "responseTemplates": { "application/json": "{\n \"message\": $context.error.messageString,\n \"type\": \"$context.error.responseType\",\n \"stage\": \"$context.stage\",\n \"resourcePath\": \"$context.resourcePath\",\n \"stageVariables.a\": \"$stageVariables.a\",\n \"statusCode\": \"&apos;404&apos;\"\n}" }, "responseType": "MISSING_AUTHENTICATION_TOKEN", "statusCode": "404" }</code></pre> <p></p> </div> </div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/customize-gateway-responses.html">Customize Gateway Responses</a> </div>

##### Instances
``` purescript
Newtype GatewayResponse _
```

#### `GatewayResponseType`

``` purescript
newtype GatewayResponseType
  = GatewayResponseType String
```

##### Instances
``` purescript
Newtype GatewayResponseType _
```

#### `GatewayResponses`

``` purescript
newtype GatewayResponses
  = GatewayResponses { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfGatewayResponse) }
```

<p>The collection of the <a>GatewayResponse</a> instances of a <a>RestApi</a> as a <code>responseType</code>-to-<a>GatewayResponse</a> object map of key-value pairs. As such, pagination is not supported for querying this collection.</p> <div class="remarks"> For more information about valid gateway response types, see <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/supported-gateway-response-types.html">Gateway Response Types Supported by API Gateway</a> <div class="example"> <h4>Example: Get the collection of gateway responses of an API</h4> <h5>Request</h5> <p>This example request shows how to retrieve the <a>GatewayResponses</a> collection from an API.</p> <pre><code>GET /restapis/o81lxisefl/gatewayresponses HTTP/1.1 Host: beta-apigateway.us-east-1.amazonaws.com Content-Type: application/json X-Amz-Date: 20170503T220604Z Authorization: AWS4-HMAC-SHA256 Credential={access-key-id}/20170503/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature=59b42fe54a76a5de8adf2c67baa6d39206f8e9ad49a1d77ccc6a5da3103a398a Cache-Control: no-cache Postman-Token: 5637af27-dc29-fc5c-9dfe-0645d52cb515 </code></pre> <p></p> <h5>Response</h5> <p>The successful operation returns the <code>200 OK</code> status code and a payload similar to the following:</p> <pre><code>{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-gatewayresponse-{rel}.html", "name": "gatewayresponse", "templated": true }, "self": { "href": "/restapis/o81lxisefl/gatewayresponses" }, "first": { "href": "/restapis/o81lxisefl/gatewayresponses" }, "gatewayresponse:by-type": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "item": [ { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_FAILURE" }, { "href": "/restapis/o81lxisefl/gatewayresponses/RESOURCE_NOT_FOUND" }, { "href": "/restapis/o81lxisefl/gatewayresponses/REQUEST_TOO_LARGE" }, { "href": "/restapis/o81lxisefl/gatewayresponses/THROTTLED" }, { "href": "/restapis/o81lxisefl/gatewayresponses/UNSUPPORTED_MEDIA_TYPE" }, { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_CONFIGURATION_ERROR" }, { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_5XX" }, { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_4XX" }, { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_PARAMETERS" }, { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_BODY" }, { "href": "/restapis/o81lxisefl/gatewayresponses/EXPIRED_TOKEN" }, { "href": "/restapis/o81lxisefl/gatewayresponses/ACCESS_DENIED" }, { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_API_KEY" }, { "href": "/restapis/o81lxisefl/gatewayresponses/UNAUTHORIZED" }, { "href": "/restapis/o81lxisefl/gatewayresponses/API_CONFIGURATION_ERROR" }, { "href": "/restapis/o81lxisefl/gatewayresponses/QUOTA_EXCEEDED" }, { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_TIMEOUT" }, { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" }, { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_SIGNATURE" }, { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_FAILURE" } ] }, "_embedded": { "item": [ { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_FAILURE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_FAILURE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "INTEGRATION_FAILURE", "statusCode": "504" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/RESOURCE_NOT_FOUND" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/RESOURCE_NOT_FOUND" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "RESOURCE_NOT_FOUND", "statusCode": "404" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/REQUEST_TOO_LARGE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/REQUEST_TOO_LARGE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "REQUEST_TOO_LARGE", "statusCode": "413" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/THROTTLED" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/THROTTLED" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "THROTTLED", "statusCode": "429" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/UNSUPPORTED_MEDIA_TYPE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/UNSUPPORTED_MEDIA_TYPE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "UNSUPPORTED_MEDIA_TYPE", "statusCode": "415" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_CONFIGURATION_ERROR" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_CONFIGURATION_ERROR" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "AUTHORIZER_CONFIGURATION_ERROR", "statusCode": "500" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_5XX" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_5XX" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "DEFAULT_5XX" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_4XX" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_4XX" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "DEFAULT_4XX" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_PARAMETERS" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_PARAMETERS" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "BAD_REQUEST_PARAMETERS", "statusCode": "400" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_BODY" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_BODY" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "BAD_REQUEST_BODY", "statusCode": "400" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/EXPIRED_TOKEN" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/EXPIRED_TOKEN" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "EXPIRED_TOKEN", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/ACCESS_DENIED" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/ACCESS_DENIED" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "ACCESS_DENIED", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_API_KEY" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_API_KEY" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "INVALID_API_KEY", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/UNAUTHORIZED" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/UNAUTHORIZED" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "UNAUTHORIZED", "statusCode": "401" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/API_CONFIGURATION_ERROR" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/API_CONFIGURATION_ERROR" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "API_CONFIGURATION_ERROR", "statusCode": "500" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/QUOTA_EXCEEDED" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/QUOTA_EXCEEDED" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "QUOTA_EXCEEDED", "statusCode": "429" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_TIMEOUT" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_TIMEOUT" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "INTEGRATION_TIMEOUT", "statusCode": "504" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "MISSING_AUTHENTICATION_TOKEN", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_SIGNATURE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_SIGNATURE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "INVALID_SIGNATURE", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_FAILURE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_FAILURE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "AUTHORIZER_FAILURE", "statusCode": "500" } ] } }</code></pre> <p></p> </div> </div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/customize-gateway-responses.html">Customize Gateway Responses</a> </div>

##### Instances
``` purescript
Newtype GatewayResponses _
```

#### `GenerateClientCertificateRequest`

``` purescript
newtype GenerateClientCertificateRequest
  = GenerateClientCertificateRequest { "Description'" :: NullOrUndefined (String) }
```

<p>A request to generate a <a>ClientCertificate</a> resource.</p>

##### Instances
``` purescript
Newtype GenerateClientCertificateRequest _
```

#### `GetAccountRequest`

``` purescript
newtype GetAccountRequest
  = GetAccountRequest {  }
```

<p>Requests API Gateway to get information about the current <a>Account</a> resource.</p>

##### Instances
``` purescript
Newtype GetAccountRequest _
```

#### `GetApiKeyRequest`

``` purescript
newtype GetApiKeyRequest
  = GetApiKeyRequest { "ApiKey'" :: String, "IncludeValue'" :: NullOrUndefined (NullableBoolean) }
```

<p>A request to get information about the current <a>ApiKey</a> resource.</p>

##### Instances
``` purescript
Newtype GetApiKeyRequest _
```

#### `GetApiKeysRequest`

``` purescript
newtype GetApiKeysRequest
  = GetApiKeysRequest { "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger), "NameQuery'" :: NullOrUndefined (String), "CustomerId'" :: NullOrUndefined (String), "IncludeValues'" :: NullOrUndefined (NullableBoolean) }
```

<p>A request to get information about the current <a>ApiKeys</a> resource.</p>

##### Instances
``` purescript
Newtype GetApiKeysRequest _
```

#### `GetAuthorizerRequest`

``` purescript
newtype GetAuthorizerRequest
  = GetAuthorizerRequest { "RestApiId'" :: String, "AuthorizerId'" :: String }
```

<p>Request to describe an existing <a>Authorizer</a> resource.</p>

##### Instances
``` purescript
Newtype GetAuthorizerRequest _
```

#### `GetAuthorizersRequest`

``` purescript
newtype GetAuthorizersRequest
  = GetAuthorizersRequest { "RestApiId'" :: String, "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>Request to describe an existing <a>Authorizers</a> resource.</p>

##### Instances
``` purescript
Newtype GetAuthorizersRequest _
```

#### `GetBasePathMappingRequest`

``` purescript
newtype GetBasePathMappingRequest
  = GetBasePathMappingRequest { "DomainName'" :: String, "BasePath'" :: String }
```

<p>Request to describe a <a>BasePathMapping</a> resource.</p>

##### Instances
``` purescript
Newtype GetBasePathMappingRequest _
```

#### `GetBasePathMappingsRequest`

``` purescript
newtype GetBasePathMappingsRequest
  = GetBasePathMappingsRequest { "DomainName'" :: String, "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>A request to get information about a collection of <a>BasePathMapping</a> resources.</p>

##### Instances
``` purescript
Newtype GetBasePathMappingsRequest _
```

#### `GetClientCertificateRequest`

``` purescript
newtype GetClientCertificateRequest
  = GetClientCertificateRequest { "ClientCertificateId'" :: String }
```

<p>A request to get information about the current <a>ClientCertificate</a> resource.</p>

##### Instances
``` purescript
Newtype GetClientCertificateRequest _
```

#### `GetClientCertificatesRequest`

``` purescript
newtype GetClientCertificatesRequest
  = GetClientCertificatesRequest { "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>A request to get information about a collection of <a>ClientCertificate</a> resources.</p>

##### Instances
``` purescript
Newtype GetClientCertificatesRequest _
```

#### `GetDeploymentRequest`

``` purescript
newtype GetDeploymentRequest
  = GetDeploymentRequest { "RestApiId'" :: String, "DeploymentId'" :: String, "Embed'" :: NullOrUndefined (ListOfString) }
```

<p>Requests API Gateway to get information about a <a>Deployment</a> resource.</p>

##### Instances
``` purescript
Newtype GetDeploymentRequest _
```

#### `GetDeploymentsRequest`

``` purescript
newtype GetDeploymentsRequest
  = GetDeploymentsRequest { "RestApiId'" :: String, "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>Requests API Gateway to get information about a <a>Deployments</a> collection.</p>

##### Instances
``` purescript
Newtype GetDeploymentsRequest _
```

#### `GetDocumentationPartRequest`

``` purescript
newtype GetDocumentationPartRequest
  = GetDocumentationPartRequest { "RestApiId'" :: String, "DocumentationPartId'" :: String }
```

<p>Gets a specified documentation part of a given API.</p>

##### Instances
``` purescript
Newtype GetDocumentationPartRequest _
```

#### `GetDocumentationPartsRequest`

``` purescript
newtype GetDocumentationPartsRequest
  = GetDocumentationPartsRequest { "RestApiId'" :: String, "Type'" :: NullOrUndefined (DocumentationPartType), "NameQuery'" :: NullOrUndefined (String), "Path'" :: NullOrUndefined (String), "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger), "LocationStatus'" :: NullOrUndefined (LocationStatusType) }
```

<p>Gets the documentation parts of an API. The result may be filtered by the type, name, or path of API entities (targets).</p>

##### Instances
``` purescript
Newtype GetDocumentationPartsRequest _
```

#### `GetDocumentationVersionRequest`

``` purescript
newtype GetDocumentationVersionRequest
  = GetDocumentationVersionRequest { "RestApiId'" :: String, "DocumentationVersion'" :: String }
```

<p>Gets a documentation snapshot of an API.</p>

##### Instances
``` purescript
Newtype GetDocumentationVersionRequest _
```

#### `GetDocumentationVersionsRequest`

``` purescript
newtype GetDocumentationVersionsRequest
  = GetDocumentationVersionsRequest { "RestApiId'" :: String, "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>Gets the documentation versions of an API.</p>

##### Instances
``` purescript
Newtype GetDocumentationVersionsRequest _
```

#### `GetDomainNameRequest`

``` purescript
newtype GetDomainNameRequest
  = GetDomainNameRequest { "DomainName'" :: String }
```

<p>Request to get the name of a <a>DomainName</a> resource.</p>

##### Instances
``` purescript
Newtype GetDomainNameRequest _
```

#### `GetDomainNamesRequest`

``` purescript
newtype GetDomainNamesRequest
  = GetDomainNamesRequest { "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>Request to describe a collection of <a>DomainName</a> resources.</p>

##### Instances
``` purescript
Newtype GetDomainNamesRequest _
```

#### `GetExportRequest`

``` purescript
newtype GetExportRequest
  = GetExportRequest { "RestApiId'" :: String, "StageName'" :: String, "ExportType'" :: String, "Parameters'" :: NullOrUndefined (MapOfStringToString), "Accepts'" :: NullOrUndefined (String) }
```

<p>Request a new export of a <a>RestApi</a> for a particular <a>Stage</a>.</p>

##### Instances
``` purescript
Newtype GetExportRequest _
```

#### `GetGatewayResponseRequest`

``` purescript
newtype GetGatewayResponseRequest
  = GetGatewayResponseRequest { "RestApiId'" :: String, "ResponseType'" :: GatewayResponseType }
```

<p>Gets a <a>GatewayResponse</a> of a specified response type on the given <a>RestApi</a>.</p>

##### Instances
``` purescript
Newtype GetGatewayResponseRequest _
```

#### `GetGatewayResponsesRequest`

``` purescript
newtype GetGatewayResponsesRequest
  = GetGatewayResponsesRequest { "RestApiId'" :: String, "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>Gets the <a>GatewayResponses</a> collection on the given <a>RestApi</a>. If an API developer has not added any definitions for gateway responses, the result will be the API Gateway-generated default <a>GatewayResponses</a> collection for the supported response types.</p>

##### Instances
``` purescript
Newtype GetGatewayResponsesRequest _
```

#### `GetIntegrationRequest`

``` purescript
newtype GetIntegrationRequest
  = GetIntegrationRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String }
```

<p>Represents a request to get the integration configuration.</p>

##### Instances
``` purescript
Newtype GetIntegrationRequest _
```

#### `GetIntegrationResponseRequest`

``` purescript
newtype GetIntegrationResponseRequest
  = GetIntegrationResponseRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "StatusCode'" :: StatusCode }
```

<p>Represents a get integration response request.</p>

##### Instances
``` purescript
Newtype GetIntegrationResponseRequest _
```

#### `GetMethodRequest`

``` purescript
newtype GetMethodRequest
  = GetMethodRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String }
```

<p>Request to describe an existing <a>Method</a> resource.</p>

##### Instances
``` purescript
Newtype GetMethodRequest _
```

#### `GetMethodResponseRequest`

``` purescript
newtype GetMethodResponseRequest
  = GetMethodResponseRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "StatusCode'" :: StatusCode }
```

<p>Request to describe a <a>MethodResponse</a> resource.</p>

##### Instances
``` purescript
Newtype GetMethodResponseRequest _
```

#### `GetModelRequest`

``` purescript
newtype GetModelRequest
  = GetModelRequest { "RestApiId'" :: String, "ModelName'" :: String, "Flatten'" :: NullOrUndefined (Boolean) }
```

<p>Request to list information about a model in an existing <a>RestApi</a> resource.</p>

##### Instances
``` purescript
Newtype GetModelRequest _
```

#### `GetModelTemplateRequest`

``` purescript
newtype GetModelTemplateRequest
  = GetModelTemplateRequest { "RestApiId'" :: String, "ModelName'" :: String }
```

<p>Request to generate a sample mapping template used to transform the payload.</p>

##### Instances
``` purescript
Newtype GetModelTemplateRequest _
```

#### `GetModelsRequest`

``` purescript
newtype GetModelsRequest
  = GetModelsRequest { "RestApiId'" :: String, "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>Request to list existing <a>Models</a> defined for a <a>RestApi</a> resource.</p>

##### Instances
``` purescript
Newtype GetModelsRequest _
```

#### `GetRequestValidatorRequest`

``` purescript
newtype GetRequestValidatorRequest
  = GetRequestValidatorRequest { "RestApiId'" :: String, "RequestValidatorId'" :: String }
```

<p>Gets a <a>RequestValidator</a> of a given <a>RestApi</a>.</p>

##### Instances
``` purescript
Newtype GetRequestValidatorRequest _
```

#### `GetRequestValidatorsRequest`

``` purescript
newtype GetRequestValidatorsRequest
  = GetRequestValidatorsRequest { "RestApiId'" :: String, "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>Gets the <a>RequestValidators</a> collection of a given <a>RestApi</a>.</p>

##### Instances
``` purescript
Newtype GetRequestValidatorsRequest _
```

#### `GetResourceRequest`

``` purescript
newtype GetResourceRequest
  = GetResourceRequest { "RestApiId'" :: String, "ResourceId'" :: String, "Embed'" :: NullOrUndefined (ListOfString) }
```

<p>Request to list information about a resource.</p>

##### Instances
``` purescript
Newtype GetResourceRequest _
```

#### `GetResourcesRequest`

``` purescript
newtype GetResourcesRequest
  = GetResourcesRequest { "RestApiId'" :: String, "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger), "Embed'" :: NullOrUndefined (ListOfString) }
```

<p>Request to list information about a collection of resources.</p>

##### Instances
``` purescript
Newtype GetResourcesRequest _
```

#### `GetRestApiRequest`

``` purescript
newtype GetRestApiRequest
  = GetRestApiRequest { "RestApiId'" :: String }
```

<p>The GET request to list an existing <a>RestApi</a> defined for your collection. </p>

##### Instances
``` purescript
Newtype GetRestApiRequest _
```

#### `GetRestApisRequest`

``` purescript
newtype GetRestApisRequest
  = GetRestApisRequest { "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>The GET request to list existing <a>RestApis</a> defined for your collection.</p>

##### Instances
``` purescript
Newtype GetRestApisRequest _
```

#### `GetSdkRequest`

``` purescript
newtype GetSdkRequest
  = GetSdkRequest { "RestApiId'" :: String, "StageName'" :: String, "SdkType'" :: String, "Parameters'" :: NullOrUndefined (MapOfStringToString) }
```

<p>Request a new generated client SDK for a <a>RestApi</a> and <a>Stage</a>.</p>

##### Instances
``` purescript
Newtype GetSdkRequest _
```

#### `GetSdkTypeRequest`

``` purescript
newtype GetSdkTypeRequest
  = GetSdkTypeRequest { "Id'" :: String }
```

<p>Get an <a>SdkType</a> instance.</p>

##### Instances
``` purescript
Newtype GetSdkTypeRequest _
```

#### `GetSdkTypesRequest`

``` purescript
newtype GetSdkTypesRequest
  = GetSdkTypesRequest { "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>Get the <a>SdkTypes</a> collection.</p>

##### Instances
``` purescript
Newtype GetSdkTypesRequest _
```

#### `GetStageRequest`

``` purescript
newtype GetStageRequest
  = GetStageRequest { "RestApiId'" :: String, "StageName'" :: String }
```

<p>Requests API Gateway to get information about a <a>Stage</a> resource.</p>

##### Instances
``` purescript
Newtype GetStageRequest _
```

#### `GetStagesRequest`

``` purescript
newtype GetStagesRequest
  = GetStagesRequest { "RestApiId'" :: String, "DeploymentId'" :: NullOrUndefined (String) }
```

<p>Requests API Gateway to get information about one or more <a>Stage</a> resources.</p>

##### Instances
``` purescript
Newtype GetStagesRequest _
```

#### `GetTagsRequest`

``` purescript
newtype GetTagsRequest
  = GetTagsRequest { "ResourceArn'" :: String, "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>Gets the Tags collection for a given resource.</p>

##### Instances
``` purescript
Newtype GetTagsRequest _
```

#### `GetUsagePlanKeyRequest`

``` purescript
newtype GetUsagePlanKeyRequest
  = GetUsagePlanKeyRequest { "UsagePlanId'" :: String, "KeyId'" :: String }
```

<p>The GET request to get a usage plan key of a given key identifier.</p>

##### Instances
``` purescript
Newtype GetUsagePlanKeyRequest _
```

#### `GetUsagePlanKeysRequest`

``` purescript
newtype GetUsagePlanKeysRequest
  = GetUsagePlanKeysRequest { "UsagePlanId'" :: String, "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger), "NameQuery'" :: NullOrUndefined (String) }
```

<p>The GET request to get all the usage plan keys representing the API keys added to a specified usage plan.</p>

##### Instances
``` purescript
Newtype GetUsagePlanKeysRequest _
```

#### `GetUsagePlanRequest`

``` purescript
newtype GetUsagePlanRequest
  = GetUsagePlanRequest { "UsagePlanId'" :: String }
```

<p>The GET request to get a usage plan of a given plan identifier.</p>

##### Instances
``` purescript
Newtype GetUsagePlanRequest _
```

#### `GetUsagePlansRequest`

``` purescript
newtype GetUsagePlansRequest
  = GetUsagePlansRequest { "Position'" :: NullOrUndefined (String), "KeyId'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>The GET request to get all the usage plans of the caller's account.</p>

##### Instances
``` purescript
Newtype GetUsagePlansRequest _
```

#### `GetUsageRequest`

``` purescript
newtype GetUsageRequest
  = GetUsageRequest { "UsagePlanId'" :: String, "KeyId'" :: NullOrUndefined (String), "StartDate'" :: String, "EndDate'" :: String, "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>The GET request to get the usage data of a usage plan in a specified time interval.</p>

##### Instances
``` purescript
Newtype GetUsageRequest _
```

#### `GetVpcLinkRequest`

``` purescript
newtype GetVpcLinkRequest
  = GetVpcLinkRequest { "VpcLinkId'" :: String }
```

<p>Gets a specified VPC link under the caller's account in a region.</p>

##### Instances
``` purescript
Newtype GetVpcLinkRequest _
```

#### `GetVpcLinksRequest`

``` purescript
newtype GetVpcLinksRequest
  = GetVpcLinksRequest { "Position'" :: NullOrUndefined (String), "Limit'" :: NullOrUndefined (NullableInteger) }
```

<p>Gets the <a>VpcLinks</a> collection under the caller's account in a selected region.</p>

##### Instances
``` purescript
Newtype GetVpcLinksRequest _
```

#### `ImportApiKeysRequest`

``` purescript
newtype ImportApiKeysRequest
  = ImportApiKeysRequest { "Body'" :: String, "Format'" :: ApiKeysFormat, "FailOnWarnings'" :: NullOrUndefined (Boolean) }
```

<p>The POST request to import API keys from an external source, such as a CSV-formatted file.</p>

##### Instances
``` purescript
Newtype ImportApiKeysRequest _
```

#### `ImportDocumentationPartsRequest`

``` purescript
newtype ImportDocumentationPartsRequest
  = ImportDocumentationPartsRequest { "RestApiId'" :: String, "Mode'" :: NullOrUndefined (PutMode), "FailOnWarnings'" :: NullOrUndefined (Boolean), "Body'" :: String }
```

<p>Import documentation parts from an external (e.g., Swagger) definition file. </p>

##### Instances
``` purescript
Newtype ImportDocumentationPartsRequest _
```

#### `ImportRestApiRequest`

``` purescript
newtype ImportRestApiRequest
  = ImportRestApiRequest { "FailOnWarnings'" :: NullOrUndefined (Boolean), "Parameters'" :: NullOrUndefined (MapOfStringToString), "Body'" :: String }
```

<p>A POST request to import an API to API Gateway using an input of an API definition file.</p>

##### Instances
``` purescript
Newtype ImportRestApiRequest _
```

#### `Integration`

``` purescript
newtype Integration
  = Integration { "Type'" :: NullOrUndefined (IntegrationType), "HttpMethod'" :: NullOrUndefined (String), "Uri'" :: NullOrUndefined (String), "ConnectionType'" :: NullOrUndefined (ConnectionType), "ConnectionId'" :: NullOrUndefined (String), "Credentials'" :: NullOrUndefined (String), "RequestParameters'" :: NullOrUndefined (MapOfStringToString), "RequestTemplates'" :: NullOrUndefined (MapOfStringToString), "PassthroughBehavior'" :: NullOrUndefined (String), "ContentHandling'" :: NullOrUndefined (ContentHandlingStrategy), "TimeoutInMillis'" :: NullOrUndefined (Int), "CacheNamespace'" :: NullOrUndefined (String), "CacheKeyParameters'" :: NullOrUndefined (ListOfString), "IntegrationResponses'" :: NullOrUndefined (MapOfIntegrationResponse) }
```

<p>Represents an HTTP, HTTP_PROXY, AWS, AWS_PROXY, or Mock integration.</p> <div class="remarks">In the API Gateway console, the built-in Lambda integration is an AWS integration.</div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Creating an API</a> </div>

##### Instances
``` purescript
Newtype Integration _
```

#### `IntegrationResponse`

``` purescript
newtype IntegrationResponse
  = IntegrationResponse { "StatusCode'" :: NullOrUndefined (StatusCode), "SelectionPattern'" :: NullOrUndefined (String), "ResponseParameters'" :: NullOrUndefined (MapOfStringToString), "ResponseTemplates'" :: NullOrUndefined (MapOfStringToString), "ContentHandling'" :: NullOrUndefined (ContentHandlingStrategy) }
```

<p>Represents an integration response. The status code must map to an existing <a>MethodResponse</a>, and parameters and templates can be used to transform the back-end response.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Creating an API</a> </div>

##### Instances
``` purescript
Newtype IntegrationResponse _
```

#### `IntegrationType`

``` purescript
newtype IntegrationType
  = IntegrationType String
```

<p>The integration type. The valid value is <code>HTTP</code> for integrating an API method with an HTTP backend; <code>AWS</code> with any AWS service endpoints; <code>MOCK</code> for testing without actually invoking the backend; <code>HTTP_PROXY</code> for integrating with the HTTP proxy integration; <code>AWS_PROXY</code> for integrating with the Lambda proxy integration. </p>

##### Instances
``` purescript
Newtype IntegrationType _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "RetryAfterSeconds'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String) }
```

<p>The request exceeded the rate limit. Retry after the specified time period.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListOfARNs`

``` purescript
newtype ListOfARNs
  = ListOfARNs (Array ProviderARN)
```

##### Instances
``` purescript
Newtype ListOfARNs _
```

#### `ListOfApiKey`

``` purescript
newtype ListOfApiKey
  = ListOfApiKey (Array ApiKey)
```

##### Instances
``` purescript
Newtype ListOfApiKey _
```

#### `ListOfApiStage`

``` purescript
newtype ListOfApiStage
  = ListOfApiStage (Array ApiStage)
```

##### Instances
``` purescript
Newtype ListOfApiStage _
```

#### `ListOfAuthorizer`

``` purescript
newtype ListOfAuthorizer
  = ListOfAuthorizer (Array Authorizer)
```

##### Instances
``` purescript
Newtype ListOfAuthorizer _
```

#### `ListOfBasePathMapping`

``` purescript
newtype ListOfBasePathMapping
  = ListOfBasePathMapping (Array BasePathMapping)
```

##### Instances
``` purescript
Newtype ListOfBasePathMapping _
```

#### `ListOfClientCertificate`

``` purescript
newtype ListOfClientCertificate
  = ListOfClientCertificate (Array ClientCertificate)
```

##### Instances
``` purescript
Newtype ListOfClientCertificate _
```

#### `ListOfDeployment`

``` purescript
newtype ListOfDeployment
  = ListOfDeployment (Array Deployment)
```

##### Instances
``` purescript
Newtype ListOfDeployment _
```

#### `ListOfDocumentationPart`

``` purescript
newtype ListOfDocumentationPart
  = ListOfDocumentationPart (Array DocumentationPart)
```

##### Instances
``` purescript
Newtype ListOfDocumentationPart _
```

#### `ListOfDocumentationVersion`

``` purescript
newtype ListOfDocumentationVersion
  = ListOfDocumentationVersion (Array DocumentationVersion)
```

##### Instances
``` purescript
Newtype ListOfDocumentationVersion _
```

#### `ListOfDomainName`

``` purescript
newtype ListOfDomainName
  = ListOfDomainName (Array DomainName)
```

##### Instances
``` purescript
Newtype ListOfDomainName _
```

#### `ListOfEndpointType`

``` purescript
newtype ListOfEndpointType
  = ListOfEndpointType (Array EndpointType)
```

##### Instances
``` purescript
Newtype ListOfEndpointType _
```

#### `ListOfGatewayResponse`

``` purescript
newtype ListOfGatewayResponse
  = ListOfGatewayResponse (Array GatewayResponse)
```

##### Instances
``` purescript
Newtype ListOfGatewayResponse _
```

#### `ListOfLong`

``` purescript
newtype ListOfLong
  = ListOfLong (Array Number)
```

##### Instances
``` purescript
Newtype ListOfLong _
```

#### `ListOfModel`

``` purescript
newtype ListOfModel
  = ListOfModel (Array Model)
```

##### Instances
``` purescript
Newtype ListOfModel _
```

#### `ListOfPatchOperation`

``` purescript
newtype ListOfPatchOperation
  = ListOfPatchOperation (Array PatchOperation)
```

A list of operations describing the updates to apply to the specified resource. The patches are applied in the order specified in the list.

##### Instances
``` purescript
Newtype ListOfPatchOperation _
```

#### `ListOfRequestValidator`

``` purescript
newtype ListOfRequestValidator
  = ListOfRequestValidator (Array RequestValidator)
```

##### Instances
``` purescript
Newtype ListOfRequestValidator _
```

#### `ListOfResource`

``` purescript
newtype ListOfResource
  = ListOfResource (Array Resource)
```

##### Instances
``` purescript
Newtype ListOfResource _
```

#### `ListOfRestApi`

``` purescript
newtype ListOfRestApi
  = ListOfRestApi (Array RestApi)
```

##### Instances
``` purescript
Newtype ListOfRestApi _
```

#### `ListOfSdkConfigurationProperty`

``` purescript
newtype ListOfSdkConfigurationProperty
  = ListOfSdkConfigurationProperty (Array SdkConfigurationProperty)
```

##### Instances
``` purescript
Newtype ListOfSdkConfigurationProperty _
```

#### `ListOfSdkType`

``` purescript
newtype ListOfSdkType
  = ListOfSdkType (Array SdkType)
```

##### Instances
``` purescript
Newtype ListOfSdkType _
```

#### `ListOfStage`

``` purescript
newtype ListOfStage
  = ListOfStage (Array Stage)
```

##### Instances
``` purescript
Newtype ListOfStage _
```

#### `ListOfStageKeys`

``` purescript
newtype ListOfStageKeys
  = ListOfStageKeys (Array StageKey)
```

##### Instances
``` purescript
Newtype ListOfStageKeys _
```

#### `ListOfString`

``` purescript
newtype ListOfString
  = ListOfString (Array String)
```

##### Instances
``` purescript
Newtype ListOfString _
```

#### `ListOfUsage`

``` purescript
newtype ListOfUsage
  = ListOfUsage (Array ListOfLong)
```

##### Instances
``` purescript
Newtype ListOfUsage _
```

#### `ListOfUsagePlan`

``` purescript
newtype ListOfUsagePlan
  = ListOfUsagePlan (Array UsagePlan)
```

##### Instances
``` purescript
Newtype ListOfUsagePlan _
```

#### `ListOfUsagePlanKey`

``` purescript
newtype ListOfUsagePlanKey
  = ListOfUsagePlanKey (Array UsagePlanKey)
```

##### Instances
``` purescript
Newtype ListOfUsagePlanKey _
```

#### `ListOfVpcLink`

``` purescript
newtype ListOfVpcLink
  = ListOfVpcLink (Array VpcLink)
```

##### Instances
``` purescript
Newtype ListOfVpcLink _
```

#### `LocationStatusType`

``` purescript
newtype LocationStatusType
  = LocationStatusType String
```

##### Instances
``` purescript
Newtype LocationStatusType _
```

#### `MapOfHeaderValues`

``` purescript
newtype MapOfHeaderValues
  = MapOfHeaderValues (Map String String)
```

##### Instances
``` purescript
Newtype MapOfHeaderValues _
```

#### `MapOfIntegrationResponse`

``` purescript
newtype MapOfIntegrationResponse
  = MapOfIntegrationResponse (Map String IntegrationResponse)
```

##### Instances
``` purescript
Newtype MapOfIntegrationResponse _
```

#### `MapOfKeyUsages`

``` purescript
newtype MapOfKeyUsages
  = MapOfKeyUsages (Map String ListOfUsage)
```

##### Instances
``` purescript
Newtype MapOfKeyUsages _
```

#### `MapOfMethod`

``` purescript
newtype MapOfMethod
  = MapOfMethod (Map String Method)
```

##### Instances
``` purescript
Newtype MapOfMethod _
```

#### `MapOfMethodResponse`

``` purescript
newtype MapOfMethodResponse
  = MapOfMethodResponse (Map String MethodResponse)
```

##### Instances
``` purescript
Newtype MapOfMethodResponse _
```

#### `MapOfMethodSettings`

``` purescript
newtype MapOfMethodSettings
  = MapOfMethodSettings (Map String MethodSetting)
```

##### Instances
``` purescript
Newtype MapOfMethodSettings _
```

#### `MapOfMethodSnapshot`

``` purescript
newtype MapOfMethodSnapshot
  = MapOfMethodSnapshot (Map String MethodSnapshot)
```

##### Instances
``` purescript
Newtype MapOfMethodSnapshot _
```

#### `MapOfStringToBoolean`

``` purescript
newtype MapOfStringToBoolean
  = MapOfStringToBoolean (Map String NullableBoolean)
```

##### Instances
``` purescript
Newtype MapOfStringToBoolean _
```

#### `MapOfStringToList`

``` purescript
newtype MapOfStringToList
  = MapOfStringToList (Map String ListOfString)
```

##### Instances
``` purescript
Newtype MapOfStringToList _
```

#### `MapOfStringToString`

``` purescript
newtype MapOfStringToString
  = MapOfStringToString (Map String String)
```

##### Instances
``` purescript
Newtype MapOfStringToString _
```

#### `Method`

``` purescript
newtype Method
  = Method { "HttpMethod'" :: NullOrUndefined (String), "AuthorizationType'" :: NullOrUndefined (String), "AuthorizerId'" :: NullOrUndefined (String), "ApiKeyRequired'" :: NullOrUndefined (NullableBoolean), "RequestValidatorId'" :: NullOrUndefined (String), "OperationName'" :: NullOrUndefined (String), "RequestParameters'" :: NullOrUndefined (MapOfStringToBoolean), "RequestModels'" :: NullOrUndefined (MapOfStringToString), "MethodResponses'" :: NullOrUndefined (MapOfMethodResponse), "MethodIntegration'" :: NullOrUndefined (Integration), "AuthorizationScopes'" :: NullOrUndefined (ListOfString) }
```

<p> Represents a client-facing interface by which the client calls the API to access back-end resources. A <b>Method</b> resource is integrated with an <a>Integration</a> resource. Both consist of a request and one or more responses. The method request takes the client input that is passed to the back end through the integration request. A method response returns the output from the back end to the client through an integration response. A method request is embodied in a <b>Method</b> resource, whereas an integration request is embodied in an <a>Integration</a> resource. On the other hand, a method response is represented by a <a>MethodResponse</a> resource, whereas an integration response is represented by an <a>IntegrationResponse</a> resource. </p> <div class="remarks"> <p/> <h4>Example: Retrive the GET method on a specified resource</h4> <h5>Request</h5> <p>The following example request retrieves the information about the GET method on an API resource (<code>3kzxbg5sa2</code>) of an API (<code>fugvjdxtri</code>). </p> <pre><code>GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160603T210259Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160603/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}</code></pre> <h5>Response</h5> <p>The successful response returns a <code>200 OK</code> status code and a payload similar to the following:</p> <pre><code>{ "_links": { "curies": [ { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-{rel}.html", "name": "method", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true } ], "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET", "name": "GET", "title": "GET" }, "integration:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "method:integration": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "method:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "methodresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/{status_code}", "templated": true } }, "apiKeyRequired": true, "authorizationType": "NONE", "httpMethod": "GET", "_embedded": { "method:integration": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "3kzxbg5sa2", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestParameters": { "integration.request.header.Content-Type": "'application/x-amz-json-1.1'" }, "requestTemplates": { "application/json": "{\n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-east-1:kinesis:action/ListStreams", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E%23foreach(%24stream%20in%20%24input.path(%27%24.StreamNames%27))%3Cstream%3E%3Cname%3E%24stream%3C%2Fname%3E%3C%2Fstream%3E%23end%3C%2FkinesisStreams%3E\")" }, "statusCode": "200" } } }, "method:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.Content-Type": false }, "statusCode": "200" } } }</code></pre> <p>In the example above, the response template for the <code>200 OK</code> response maps the JSON output from the <code>ListStreams</code> action in the back end to an XML output. The mapping template is URL-encoded as <code>%3CkinesisStreams%3E%23foreach(%24stream%20in%20%24input.path(%27%24.StreamNames%27))%3Cstream%3E%3Cname%3E%24stream%3C%2Fname%3E%3C%2Fstream%3E%23end%3C%2FkinesisStreams%3E</code> and the output is decoded using the <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#util-templat-reference">$util.urlDecode()</a> helper function.</p> </div> <div class="seeAlso"> <a>MethodResponse</a>, <a>Integration</a>, <a>IntegrationResponse</a>, <a>Resource</a>, <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-method-settings.html">Set up an API's method</a> </div>

##### Instances
``` purescript
Newtype Method _
```

#### `MethodResponse`

``` purescript
newtype MethodResponse
  = MethodResponse { "StatusCode'" :: NullOrUndefined (StatusCode), "ResponseParameters'" :: NullOrUndefined (MapOfStringToBoolean), "ResponseModels'" :: NullOrUndefined (MapOfStringToString) }
```

<p>Represents a method response of a given HTTP status code returned to the client. The method response is passed from the back end through the associated integration response that can be transformed using a mapping template. </p> <div class="remarks"> <p/> <h4>Example: A <b>MethodResponse</b> instance of an API</h4> <h5>Request</h5> <p>The example request retrieves a <b>MethodResponse</b> of the 200 status code.</p> <pre><code>GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160603T222952Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160603/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}</code></pre> <h5>Response</h5> <p>The successful response returns <code>200 OK</code> status and a payload as follows:</p> <pre><code>{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true }, "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.Content-Type": false }, "statusCode": "200" }</code></pre> <p/> </div> <div class="seeAlso"> <a>Method</a>, <a>IntegrationResponse</a>, <a>Integration</a> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Creating an API</a> </div>

##### Instances
``` purescript
Newtype MethodResponse _
```

#### `MethodSetting`

``` purescript
newtype MethodSetting
  = MethodSetting { "MetricsEnabled'" :: NullOrUndefined (Boolean), "LoggingLevel'" :: NullOrUndefined (String), "DataTraceEnabled'" :: NullOrUndefined (Boolean), "ThrottlingBurstLimit'" :: NullOrUndefined (Int), "ThrottlingRateLimit'" :: NullOrUndefined (Number), "CachingEnabled'" :: NullOrUndefined (Boolean), "CacheTtlInSeconds'" :: NullOrUndefined (Int), "CacheDataEncrypted'" :: NullOrUndefined (Boolean), "RequireAuthorizationForCacheControl'" :: NullOrUndefined (Boolean), "UnauthorizedCacheControlHeaderStrategy'" :: NullOrUndefined (UnauthorizedCacheControlHeaderStrategy) }
```

<p>Specifies the method setting properties.</p>

##### Instances
``` purescript
Newtype MethodSetting _
```

#### `MethodSnapshot`

``` purescript
newtype MethodSnapshot
  = MethodSnapshot { "AuthorizationType'" :: NullOrUndefined (String), "ApiKeyRequired'" :: NullOrUndefined (Boolean) }
```

<p>Represents a summary of a <a>Method</a> resource, given a particular date and time.</p>

##### Instances
``` purescript
Newtype MethodSnapshot _
```

#### `Model`

``` purescript
newtype Model
  = Model { "Id'" :: NullOrUndefined (String), "Name'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "Schema'" :: NullOrUndefined (String), "ContentType'" :: NullOrUndefined (String) }
```

<p>Represents the data structure of a method's request or response payload.</p> <div class="remarks"> <p>A request model defines the data structure of the client-supplied request payload. A response model defines the data structure of the response payload returned by the back end. Although not required, models are useful for mapping payloads between the front end and back end.</p> <p>A model is used for generating an API's SDK, validating the input request body, and creating a skeletal mapping template.</p> </div> <div class="seeAlso"> <a>Method</a>, <a>MethodResponse</a>, <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html">Models and Mappings</a> </div>

##### Instances
``` purescript
Newtype Model _
```

#### `Models`

``` purescript
newtype Models
  = Models { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfModel) }
```

<p>Represents a collection of <a>Model</a> resources.</p> <div class="seeAlso"> <a>Method</a>, <a>MethodResponse</a>, <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html">Models and Mappings</a> </div>

##### Instances
``` purescript
Newtype Models _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message'" :: NullOrUndefined (String) }
```

<p>The requested resource is not found. Make sure that the request URI is correct.</p>

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `NullableBoolean`

``` purescript
newtype NullableBoolean
  = NullableBoolean Boolean
```

##### Instances
``` purescript
Newtype NullableBoolean _
```

#### `NullableInteger`

``` purescript
newtype NullableInteger
  = NullableInteger Int
```

##### Instances
``` purescript
Newtype NullableInteger _
```

#### `Op`

``` purescript
newtype Op
  = Op String
```

##### Instances
``` purescript
Newtype Op _
```

#### `PatchOperation`

``` purescript
newtype PatchOperation
  = PatchOperation { "Op'" :: NullOrUndefined (Op), "Path'" :: NullOrUndefined (String), "Value'" :: NullOrUndefined (String), "From'" :: NullOrUndefined (String) }
```

A single patch operation to apply to the specified resource. Please refer to http://tools.ietf.org/html/rfc6902#section-4 for an explanation of how each operation is used.

##### Instances
``` purescript
Newtype PatchOperation _
```

#### `PathToMapOfMethodSnapshot`

``` purescript
newtype PathToMapOfMethodSnapshot
  = PathToMapOfMethodSnapshot (Map String MapOfMethodSnapshot)
```

##### Instances
``` purescript
Newtype PathToMapOfMethodSnapshot _
```

#### `ProviderARN`

``` purescript
newtype ProviderARN
  = ProviderARN String
```

##### Instances
``` purescript
Newtype ProviderARN _
```

#### `PutGatewayResponseRequest`

``` purescript
newtype PutGatewayResponseRequest
  = PutGatewayResponseRequest { "RestApiId'" :: String, "ResponseType'" :: GatewayResponseType, "StatusCode'" :: NullOrUndefined (StatusCode), "ResponseParameters'" :: NullOrUndefined (MapOfStringToString), "ResponseTemplates'" :: NullOrUndefined (MapOfStringToString) }
```

<p>Creates a customization of a <a>GatewayResponse</a> of a specified response type and status code on the given <a>RestApi</a>.</p>

##### Instances
``` purescript
Newtype PutGatewayResponseRequest _
```

#### `PutIntegrationRequest`

``` purescript
newtype PutIntegrationRequest
  = PutIntegrationRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "Type'" :: IntegrationType, "IntegrationHttpMethod'" :: NullOrUndefined (String), "Uri'" :: NullOrUndefined (String), "ConnectionType'" :: NullOrUndefined (ConnectionType), "ConnectionId'" :: NullOrUndefined (String), "Credentials'" :: NullOrUndefined (String), "RequestParameters'" :: NullOrUndefined (MapOfStringToString), "RequestTemplates'" :: NullOrUndefined (MapOfStringToString), "PassthroughBehavior'" :: NullOrUndefined (String), "CacheNamespace'" :: NullOrUndefined (String), "CacheKeyParameters'" :: NullOrUndefined (ListOfString), "ContentHandling'" :: NullOrUndefined (ContentHandlingStrategy), "TimeoutInMillis'" :: NullOrUndefined (NullableInteger) }
```

<p>Sets up a method's integration.</p>

##### Instances
``` purescript
Newtype PutIntegrationRequest _
```

#### `PutIntegrationResponseRequest`

``` purescript
newtype PutIntegrationResponseRequest
  = PutIntegrationResponseRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "StatusCode'" :: StatusCode, "SelectionPattern'" :: NullOrUndefined (String), "ResponseParameters'" :: NullOrUndefined (MapOfStringToString), "ResponseTemplates'" :: NullOrUndefined (MapOfStringToString), "ContentHandling'" :: NullOrUndefined (ContentHandlingStrategy) }
```

<p>Represents a put integration response request.</p>

##### Instances
``` purescript
Newtype PutIntegrationResponseRequest _
```

#### `PutMethodRequest`

``` purescript
newtype PutMethodRequest
  = PutMethodRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "AuthorizationType'" :: String, "AuthorizerId'" :: NullOrUndefined (String), "ApiKeyRequired'" :: NullOrUndefined (Boolean), "OperationName'" :: NullOrUndefined (String), "RequestParameters'" :: NullOrUndefined (MapOfStringToBoolean), "RequestModels'" :: NullOrUndefined (MapOfStringToString), "RequestValidatorId'" :: NullOrUndefined (String), "AuthorizationScopes'" :: NullOrUndefined (ListOfString) }
```

<p>Request to add a method to an existing <a>Resource</a> resource.</p>

##### Instances
``` purescript
Newtype PutMethodRequest _
```

#### `PutMethodResponseRequest`

``` purescript
newtype PutMethodResponseRequest
  = PutMethodResponseRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "StatusCode'" :: StatusCode, "ResponseParameters'" :: NullOrUndefined (MapOfStringToBoolean), "ResponseModels'" :: NullOrUndefined (MapOfStringToString) }
```

<p>Request to add a <a>MethodResponse</a> to an existing <a>Method</a> resource.</p>

##### Instances
``` purescript
Newtype PutMethodResponseRequest _
```

#### `PutMode`

``` purescript
newtype PutMode
  = PutMode String
```

##### Instances
``` purescript
Newtype PutMode _
```

#### `PutRestApiRequest`

``` purescript
newtype PutRestApiRequest
  = PutRestApiRequest { "RestApiId'" :: String, "Mode'" :: NullOrUndefined (PutMode), "FailOnWarnings'" :: NullOrUndefined (Boolean), "Parameters'" :: NullOrUndefined (MapOfStringToString), "Body'" :: String }
```

<p>A PUT request to update an existing API, with external API definitions specified as the request body.</p>

##### Instances
``` purescript
Newtype PutRestApiRequest _
```

#### `QuotaPeriodType`

``` purescript
newtype QuotaPeriodType
  = QuotaPeriodType String
```

##### Instances
``` purescript
Newtype QuotaPeriodType _
```

#### `QuotaSettings`

``` purescript
newtype QuotaSettings
  = QuotaSettings { "Limit'" :: NullOrUndefined (Int), "Offset'" :: NullOrUndefined (Int), "Period'" :: NullOrUndefined (QuotaPeriodType) }
```

<p>Quotas configured for a usage plan.</p>

##### Instances
``` purescript
Newtype QuotaSettings _
```

#### `RequestValidator`

``` purescript
newtype RequestValidator
  = RequestValidator { "Id'" :: NullOrUndefined (String), "Name'" :: NullOrUndefined (String), "ValidateRequestBody'" :: NullOrUndefined (Boolean), "ValidateRequestParameters'" :: NullOrUndefined (Boolean) }
```

<p>A set of validation rules for incoming <a>Method</a> requests.</p> <div class="remarks"> <p>In Swagger, a <a>RequestValidator</a> of an API is defined by the <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validators.requestValidator.html">x-amazon-apigateway-request-validators.requestValidator</a> object. It the referenced using the <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validator">x-amazon-apigateway-request-validator</a> property.</p> </div> <div class="seeAlso"><a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-method-request-validation.html">Enable Basic Request Validation in API Gateway</a></div>

##### Instances
``` purescript
Newtype RequestValidator _
```

#### `RequestValidators`

``` purescript
newtype RequestValidators
  = RequestValidators { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfRequestValidator) }
```

<p>A collection of <a>RequestValidator</a> resources of a given <a>RestApi</a>.</p> <div class="remarks"> <p>In Swagger, the <a>RequestValidators</a> of an API is defined by the <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validators.html">x-amazon-apigateway-request-validators</a> extension.</p> </div> <div class="seeAlso"><a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-method-request-validation.html">Enable Basic Request Validation in API Gateway</a></div>

##### Instances
``` purescript
Newtype RequestValidators _
```

#### `Resource`

``` purescript
newtype Resource
  = Resource { "Id'" :: NullOrUndefined (String), "ParentId'" :: NullOrUndefined (String), "PathPart'" :: NullOrUndefined (String), "Path'" :: NullOrUndefined (String), "ResourceMethods'" :: NullOrUndefined (MapOfMethod) }
```

<p>Represents an API resource.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Create an API</a> </div>

##### Instances
``` purescript
Newtype Resource _
```

#### `Resources`

``` purescript
newtype Resources
  = Resources { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfResource) }
```

<p>Represents a collection of <a>Resource</a> resources.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Create an API</a> </div>

##### Instances
``` purescript
Newtype Resources _
```

#### `RestApi`

``` purescript
newtype RestApi
  = RestApi { "Id'" :: NullOrUndefined (String), "Name'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "CreatedDate'" :: NullOrUndefined (Number), "Version'" :: NullOrUndefined (String), "Warnings'" :: NullOrUndefined (ListOfString), "BinaryMediaTypes'" :: NullOrUndefined (ListOfString), "MinimumCompressionSize'" :: NullOrUndefined (NullableInteger), "ApiKeySource'" :: NullOrUndefined (ApiKeySourceType), "EndpointConfiguration'" :: NullOrUndefined (EndpointConfiguration) }
```

<p>Represents a REST API.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Create an API</a> </div>

##### Instances
``` purescript
Newtype RestApi _
```

#### `RestApis`

``` purescript
newtype RestApis
  = RestApis { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfRestApi) }
```

<p>Contains references to your APIs and links that guide you in how to interact with your collection. A collection offers a paginated view of your APIs.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Create an API</a> </div>

##### Instances
``` purescript
Newtype RestApis _
```

#### `SdkConfigurationProperty`

``` purescript
newtype SdkConfigurationProperty
  = SdkConfigurationProperty { "Name'" :: NullOrUndefined (String), "FriendlyName'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "Required'" :: NullOrUndefined (Boolean), "DefaultValue'" :: NullOrUndefined (String) }
```

<p>A configuration property of an SDK type.</p>

##### Instances
``` purescript
Newtype SdkConfigurationProperty _
```

#### `SdkResponse`

``` purescript
newtype SdkResponse
  = SdkResponse { "ContentType'" :: NullOrUndefined (String), "ContentDisposition'" :: NullOrUndefined (String), "Body'" :: NullOrUndefined (String) }
```

<p>The binary blob response to <a>GetSdk</a>, which contains the generated SDK.</p>

##### Instances
``` purescript
Newtype SdkResponse _
```

#### `SdkType`

``` purescript
newtype SdkType
  = SdkType { "Id'" :: NullOrUndefined (String), "FriendlyName'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "ConfigurationProperties'" :: NullOrUndefined (ListOfSdkConfigurationProperty) }
```

<p>A type of SDK that API Gateway can generate.</p>

##### Instances
``` purescript
Newtype SdkType _
```

#### `SdkTypes`

``` purescript
newtype SdkTypes
  = SdkTypes { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfSdkType) }
```

<p>The collection of <a>SdkType</a> instances.</p>

##### Instances
``` purescript
Newtype SdkTypes _
```

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "RetryAfterSeconds'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String) }
```

<p>The requested service is not available. For details see the accompanying error message. Retry after the specified time period.</p>

##### Instances
``` purescript
Newtype ServiceUnavailableException _
```

#### `Stage`

``` purescript
newtype Stage
  = Stage { "DeploymentId'" :: NullOrUndefined (String), "ClientCertificateId'" :: NullOrUndefined (String), "StageName'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "CacheClusterEnabled'" :: NullOrUndefined (Boolean), "CacheClusterSize'" :: NullOrUndefined (CacheClusterSize), "CacheClusterStatus'" :: NullOrUndefined (CacheClusterStatus), "MethodSettings'" :: NullOrUndefined (MapOfMethodSettings), "Variables'" :: NullOrUndefined (MapOfStringToString), "DocumentationVersion'" :: NullOrUndefined (String), "AccessLogSettings'" :: NullOrUndefined (AccessLogSettings), "CanarySettings'" :: NullOrUndefined (CanarySettings), "Tags'" :: NullOrUndefined (MapOfStringToString), "CreatedDate'" :: NullOrUndefined (Number), "LastUpdatedDate'" :: NullOrUndefined (Number) }
```

<p>Represents a unique identifier for a version of a deployed <a>RestApi</a> that is callable by users.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html">Deploy an API</a> </div>

##### Instances
``` purescript
Newtype Stage _
```

#### `StageKey`

``` purescript
newtype StageKey
  = StageKey { "RestApiId'" :: NullOrUndefined (String), "StageName'" :: NullOrUndefined (String) }
```

<p>A reference to a unique stage identified in the format <code>{restApiId}/{stage}</code>.</p>

##### Instances
``` purescript
Newtype StageKey _
```

#### `Stages`

``` purescript
newtype Stages
  = Stages { "Item'" :: NullOrUndefined (ListOfStage) }
```

<p>A list of <a>Stage</a> resources that are associated with the <a>ApiKey</a> resource.</p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/stages.html">Deploying API in Stages</a></div>

##### Instances
``` purescript
Newtype Stages _
```

#### `StatusCode`

``` purescript
newtype StatusCode
  = StatusCode String
```

<p>The status code.</p>

##### Instances
``` purescript
Newtype StatusCode _
```

#### `TagResourceRequest`

``` purescript
newtype TagResourceRequest
  = TagResourceRequest { "ResourceArn'" :: String, "Tags'" :: MapOfStringToString }
```

<p>Adds or updates Tags on a gievn resource.</p>

##### Instances
``` purescript
Newtype TagResourceRequest _
```

#### `Tags`

``` purescript
newtype Tags
  = Tags { "Tags'" :: NullOrUndefined (MapOfStringToString) }
```

<p>A collection of Tags associated with a given resource.</p>

##### Instances
``` purescript
Newtype Tags _
```

#### `Template`

``` purescript
newtype Template
  = Template { "Value'" :: NullOrUndefined (String) }
```

<p>Represents a mapping template used to transform a payload.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html#models-mappings-mappings">Mapping Templates</a> </div>

##### Instances
``` purescript
Newtype Template _
```

#### `TestInvokeAuthorizerRequest`

``` purescript
newtype TestInvokeAuthorizerRequest
  = TestInvokeAuthorizerRequest { "RestApiId'" :: String, "AuthorizerId'" :: String, "Headers'" :: NullOrUndefined (MapOfHeaderValues), "PathWithQueryString'" :: NullOrUndefined (String), "Body'" :: NullOrUndefined (String), "StageVariables'" :: NullOrUndefined (MapOfStringToString), "AdditionalContext'" :: NullOrUndefined (MapOfStringToString) }
```

<p>Make a request to simulate the execution of an <a>Authorizer</a>.</p>

##### Instances
``` purescript
Newtype TestInvokeAuthorizerRequest _
```

#### `TestInvokeAuthorizerResponse`

``` purescript
newtype TestInvokeAuthorizerResponse
  = TestInvokeAuthorizerResponse { "ClientStatus'" :: NullOrUndefined (Int), "Log'" :: NullOrUndefined (String), "Latency'" :: NullOrUndefined (Number), "PrincipalId'" :: NullOrUndefined (String), "Policy'" :: NullOrUndefined (String), "Authorization'" :: NullOrUndefined (MapOfStringToList), "Claims'" :: NullOrUndefined (MapOfStringToString) }
```

<p>Represents the response of the test invoke request for a custom <a>Authorizer</a></p>

##### Instances
``` purescript
Newtype TestInvokeAuthorizerResponse _
```

#### `TestInvokeMethodRequest`

``` purescript
newtype TestInvokeMethodRequest
  = TestInvokeMethodRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "PathWithQueryString'" :: NullOrUndefined (String), "Body'" :: NullOrUndefined (String), "Headers'" :: NullOrUndefined (MapOfHeaderValues), "ClientCertificateId'" :: NullOrUndefined (String), "StageVariables'" :: NullOrUndefined (MapOfStringToString) }
```

<p>Make a request to simulate the execution of a <a>Method</a>.</p>

##### Instances
``` purescript
Newtype TestInvokeMethodRequest _
```

#### `TestInvokeMethodResponse`

``` purescript
newtype TestInvokeMethodResponse
  = TestInvokeMethodResponse { "Status'" :: NullOrUndefined (Int), "Body'" :: NullOrUndefined (String), "Headers'" :: NullOrUndefined (MapOfHeaderValues), "Log'" :: NullOrUndefined (String), "Latency'" :: NullOrUndefined (Number) }
```

<p>Represents the response of the test invoke request in the HTTP method.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-test-method.html#how-to-test-method-console">Test API using the API Gateway console</a> </div>

##### Instances
``` purescript
Newtype TestInvokeMethodResponse _
```

#### `ThrottleSettings`

``` purescript
newtype ThrottleSettings
  = ThrottleSettings { "BurstLimit'" :: NullOrUndefined (Int), "RateLimit'" :: NullOrUndefined (Number) }
```

<p> The API request rate limits.</p>

##### Instances
``` purescript
Newtype ThrottleSettings _
```

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "RetryAfterSeconds'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String) }
```

<p>The request has reached its throttling limit. Retry after the specified time period.</p>

##### Instances
``` purescript
Newtype TooManyRequestsException _
```

#### `UnauthorizedCacheControlHeaderStrategy`

``` purescript
newtype UnauthorizedCacheControlHeaderStrategy
  = UnauthorizedCacheControlHeaderStrategy String
```

##### Instances
``` purescript
Newtype UnauthorizedCacheControlHeaderStrategy _
```

#### `UnauthorizedException`

``` purescript
newtype UnauthorizedException
  = UnauthorizedException { "Message'" :: NullOrUndefined (String) }
```

<p>The request is denied because the caller has insufficient permissions.</p>

##### Instances
``` purescript
Newtype UnauthorizedException _
```

#### `UntagResourceRequest`

``` purescript
newtype UntagResourceRequest
  = UntagResourceRequest { "ResourceArn'" :: String, "TagKeys'" :: ListOfString }
```

<p>Removes Tags from a given resource.</p>

##### Instances
``` purescript
Newtype UntagResourceRequest _
```

#### `UpdateAccountRequest`

``` purescript
newtype UpdateAccountRequest
  = UpdateAccountRequest { "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Requests API Gateway to change information about the current <a>Account</a> resource.</p>

##### Instances
``` purescript
Newtype UpdateAccountRequest _
```

#### `UpdateApiKeyRequest`

``` purescript
newtype UpdateApiKeyRequest
  = UpdateApiKeyRequest { "ApiKey'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>A request to change information about an <a>ApiKey</a> resource.</p>

##### Instances
``` purescript
Newtype UpdateApiKeyRequest _
```

#### `UpdateAuthorizerRequest`

``` purescript
newtype UpdateAuthorizerRequest
  = UpdateAuthorizerRequest { "RestApiId'" :: String, "AuthorizerId'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Request to update an existing <a>Authorizer</a> resource.</p>

##### Instances
``` purescript
Newtype UpdateAuthorizerRequest _
```

#### `UpdateBasePathMappingRequest`

``` purescript
newtype UpdateBasePathMappingRequest
  = UpdateBasePathMappingRequest { "DomainName'" :: String, "BasePath'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>A request to change information about the <a>BasePathMapping</a> resource.</p>

##### Instances
``` purescript
Newtype UpdateBasePathMappingRequest _
```

#### `UpdateClientCertificateRequest`

``` purescript
newtype UpdateClientCertificateRequest
  = UpdateClientCertificateRequest { "ClientCertificateId'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>A request to change information about an <a>ClientCertificate</a> resource.</p>

##### Instances
``` purescript
Newtype UpdateClientCertificateRequest _
```

#### `UpdateDeploymentRequest`

``` purescript
newtype UpdateDeploymentRequest
  = UpdateDeploymentRequest { "RestApiId'" :: String, "DeploymentId'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Requests API Gateway to change information about a <a>Deployment</a> resource.</p>

##### Instances
``` purescript
Newtype UpdateDeploymentRequest _
```

#### `UpdateDocumentationPartRequest`

``` purescript
newtype UpdateDocumentationPartRequest
  = UpdateDocumentationPartRequest { "RestApiId'" :: String, "DocumentationPartId'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Updates an existing documentation part of a given API.</p>

##### Instances
``` purescript
Newtype UpdateDocumentationPartRequest _
```

#### `UpdateDocumentationVersionRequest`

``` purescript
newtype UpdateDocumentationVersionRequest
  = UpdateDocumentationVersionRequest { "RestApiId'" :: String, "DocumentationVersion'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Updates an existing documentation version of an API.</p>

##### Instances
``` purescript
Newtype UpdateDocumentationVersionRequest _
```

#### `UpdateDomainNameRequest`

``` purescript
newtype UpdateDomainNameRequest
  = UpdateDomainNameRequest { "DomainName'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>A request to change information about the <a>DomainName</a> resource.</p>

##### Instances
``` purescript
Newtype UpdateDomainNameRequest _
```

#### `UpdateGatewayResponseRequest`

``` purescript
newtype UpdateGatewayResponseRequest
  = UpdateGatewayResponseRequest { "RestApiId'" :: String, "ResponseType'" :: GatewayResponseType, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Updates a <a>GatewayResponse</a> of a specified response type on the given <a>RestApi</a>.</p>

##### Instances
``` purescript
Newtype UpdateGatewayResponseRequest _
```

#### `UpdateIntegrationRequest`

``` purescript
newtype UpdateIntegrationRequest
  = UpdateIntegrationRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Represents an update integration request.</p>

##### Instances
``` purescript
Newtype UpdateIntegrationRequest _
```

#### `UpdateIntegrationResponseRequest`

``` purescript
newtype UpdateIntegrationResponseRequest
  = UpdateIntegrationResponseRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "StatusCode'" :: StatusCode, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Represents an update integration response request.</p>

##### Instances
``` purescript
Newtype UpdateIntegrationResponseRequest _
```

#### `UpdateMethodRequest`

``` purescript
newtype UpdateMethodRequest
  = UpdateMethodRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Request to update an existing <a>Method</a> resource.</p>

##### Instances
``` purescript
Newtype UpdateMethodRequest _
```

#### `UpdateMethodResponseRequest`

``` purescript
newtype UpdateMethodResponseRequest
  = UpdateMethodResponseRequest { "RestApiId'" :: String, "ResourceId'" :: String, "HttpMethod'" :: String, "StatusCode'" :: StatusCode, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>A request to update an existing <a>MethodResponse</a> resource.</p>

##### Instances
``` purescript
Newtype UpdateMethodResponseRequest _
```

#### `UpdateModelRequest`

``` purescript
newtype UpdateModelRequest
  = UpdateModelRequest { "RestApiId'" :: String, "ModelName'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Request to update an existing model in an existing <a>RestApi</a> resource.</p>

##### Instances
``` purescript
Newtype UpdateModelRequest _
```

#### `UpdateRequestValidatorRequest`

``` purescript
newtype UpdateRequestValidatorRequest
  = UpdateRequestValidatorRequest { "RestApiId'" :: String, "RequestValidatorId'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Updates a <a>RequestValidator</a> of a given <a>RestApi</a>.</p>

##### Instances
``` purescript
Newtype UpdateRequestValidatorRequest _
```

#### `UpdateResourceRequest`

``` purescript
newtype UpdateResourceRequest
  = UpdateResourceRequest { "RestApiId'" :: String, "ResourceId'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Request to change information about a <a>Resource</a> resource.</p>

##### Instances
``` purescript
Newtype UpdateResourceRequest _
```

#### `UpdateRestApiRequest`

``` purescript
newtype UpdateRestApiRequest
  = UpdateRestApiRequest { "RestApiId'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Request to update an existing <a>RestApi</a> resource in your collection.</p>

##### Instances
``` purescript
Newtype UpdateRestApiRequest _
```

#### `UpdateStageRequest`

``` purescript
newtype UpdateStageRequest
  = UpdateStageRequest { "RestApiId'" :: String, "StageName'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Requests API Gateway to change information about a <a>Stage</a> resource.</p>

##### Instances
``` purescript
Newtype UpdateStageRequest _
```

#### `UpdateUsagePlanRequest`

``` purescript
newtype UpdateUsagePlanRequest
  = UpdateUsagePlanRequest { "UsagePlanId'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>The PATCH request to update a usage plan of a given plan Id.</p>

##### Instances
``` purescript
Newtype UpdateUsagePlanRequest _
```

#### `UpdateUsageRequest`

``` purescript
newtype UpdateUsageRequest
  = UpdateUsageRequest { "UsagePlanId'" :: String, "KeyId'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>The PATCH request to grant a temporary extension to the remaining quota of a usage plan associated with a specified API key.</p>

##### Instances
``` purescript
Newtype UpdateUsageRequest _
```

#### `UpdateVpcLinkRequest`

``` purescript
newtype UpdateVpcLinkRequest
  = UpdateVpcLinkRequest { "VpcLinkId'" :: String, "PatchOperations'" :: NullOrUndefined (ListOfPatchOperation) }
```

<p>Updates an existing <a>VpcLink</a> of a specified identifier.</p>

##### Instances
``` purescript
Newtype UpdateVpcLinkRequest _
```

#### `Usage`

``` purescript
newtype Usage
  = Usage { "UsagePlanId'" :: NullOrUndefined (String), "StartDate'" :: NullOrUndefined (String), "EndDate'" :: NullOrUndefined (String), "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (MapOfKeyUsages) }
```

<p>Represents the usage data of a usage plan.</p> <div class="remarks"/> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html">Create and Use Usage Plans</a>, <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-create-usage-plans-with-console.html#api-gateway-usage-plan-manage-usage">Manage Usage in a Usage Plan</a> </div>

##### Instances
``` purescript
Newtype Usage _
```

#### `UsagePlan`

``` purescript
newtype UsagePlan
  = UsagePlan { "Id'" :: NullOrUndefined (String), "Name'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "ApiStages'" :: NullOrUndefined (ListOfApiStage), "Throttle'" :: NullOrUndefined (ThrottleSettings), "Quota'" :: NullOrUndefined (QuotaSettings), "ProductCode'" :: NullOrUndefined (String) }
```

<p>Represents a usage plan than can specify who can assess associated API stages with specified request limits and quotas.</p> <div class="remarks"> <p>In a usage plan, you associate an API by specifying the API's Id and a stage name of the specified API. You add plan customers by adding API keys to the plan. </p> </div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html">Create and Use Usage Plans</a> </div>

##### Instances
``` purescript
Newtype UsagePlan _
```

#### `UsagePlanKey`

``` purescript
newtype UsagePlanKey
  = UsagePlanKey { "Id'" :: NullOrUndefined (String), "Type'" :: NullOrUndefined (String), "Value'" :: NullOrUndefined (String), "Name'" :: NullOrUndefined (String) }
```

<p>Represents a usage plan key to identify a plan customer.</p> <div class="remarks"> <p>To associate an API stage with a selected API key in a usage plan, you must create a UsagePlanKey resource to represent the selected <a>ApiKey</a>.</p> </div>" <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html">Create and Use Usage Plans</a> </div>

##### Instances
``` purescript
Newtype UsagePlanKey _
```

#### `UsagePlanKeys`

``` purescript
newtype UsagePlanKeys
  = UsagePlanKeys { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfUsagePlanKey) }
```

<p>Represents the collection of usage plan keys added to usage plans for the associated API keys and, possibly, other types of keys.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html">Create and Use Usage Plans</a> </div>

##### Instances
``` purescript
Newtype UsagePlanKeys _
```

#### `UsagePlans`

``` purescript
newtype UsagePlans
  = UsagePlans { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfUsagePlan) }
```

<p>Represents a collection of usage plans for an AWS account.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html">Create and Use Usage Plans</a> </div>

##### Instances
``` purescript
Newtype UsagePlans _
```

#### `VpcLink`

``` purescript
newtype VpcLink
  = VpcLink { "Id'" :: NullOrUndefined (String), "Name'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "TargetArns'" :: NullOrUndefined (ListOfString), "Status'" :: NullOrUndefined (VpcLinkStatus), "StatusMessage'" :: NullOrUndefined (String) }
```

<p>A API Gateway VPC link for a <a>RestApi</a> to access resources in an Amazon Virtual Private Cloud (VPC).</p> <div class="remarks"> <p><p>To enable access to a resource in an Amazon Virtual Private Cloud through Amazon API Gateway, you, as an API developer, create a <a>VpcLink</a> resource targeted for one or more network load balancers of the VPC and then integrate an API method with a private integration that uses the <a>VpcLink</a>. The private integration has an integration type of <code>HTTP</code> or <code>HTTP_PROXY</code> and has a connection type of <code>VPC_LINK</code>. The integration uses the <code>connectionId</code> property to identify the <a>VpcLink</a> used.</p> </p> </div>

##### Instances
``` purescript
Newtype VpcLink _
```

#### `VpcLinkStatus`

``` purescript
newtype VpcLinkStatus
  = VpcLinkStatus String
```

##### Instances
``` purescript
Newtype VpcLinkStatus _
```

#### `VpcLinks`

``` purescript
newtype VpcLinks
  = VpcLinks { "Position'" :: NullOrUndefined (String), "Items'" :: NullOrUndefined (ListOfVpcLink) }
```

<p>The collection of VPC links under the caller's account in a region.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-with-private-integration.html">Getting Started with Private Integrations</a>, <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-private-integration.html">Set up Private Integrations</a> </div>

##### Instances
``` purescript
Newtype VpcLinks _
```


