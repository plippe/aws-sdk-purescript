

-- | <fullname>Amazon API Gateway</fullname> <p>Amazon API Gateway helps developers deliver robust, secure, and scalable mobile and web application back ends. API Gateway allows developers to securely connect mobile and web applications to APIs that run on AWS Lambda, Amazon EC2, or other publicly addressable web services that are hosted outside of AWS.</p>
module AWS.APIGateway where

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

serviceName = "APIGateway" :: String


-- | <p>Create an <a>ApiKey</a> resource. </p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/create-api-key.html">AWS CLI</a></div>
createApiKey :: forall eff. CreateApiKeyRequest -> Aff (exception :: EXCEPTION | eff) ApiKey
createApiKey = Request.request serviceName "createApiKey" 


-- | <p>Adds a new <a>Authorizer</a> resource to an existing <a>RestApi</a> resource.</p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/create-authorizer.html">AWS CLI</a></div>
createAuthorizer :: forall eff. CreateAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) Authorizer
createAuthorizer = Request.request serviceName "createAuthorizer" 


-- | <p>Creates a new <a>BasePathMapping</a> resource.</p>
createBasePathMapping :: forall eff. CreateBasePathMappingRequest -> Aff (exception :: EXCEPTION | eff) BasePathMapping
createBasePathMapping = Request.request serviceName "createBasePathMapping" 


-- | <p>Creates a <a>Deployment</a> resource, which makes a specified <a>RestApi</a> callable over the internet.</p>
createDeployment :: forall eff. CreateDeploymentRequest -> Aff (exception :: EXCEPTION | eff) Deployment
createDeployment = Request.request serviceName "createDeployment" 


createDocumentationPart :: forall eff. CreateDocumentationPartRequest -> Aff (exception :: EXCEPTION | eff) DocumentationPart
createDocumentationPart = Request.request serviceName "createDocumentationPart" 


createDocumentationVersion :: forall eff. CreateDocumentationVersionRequest -> Aff (exception :: EXCEPTION | eff) DocumentationVersion
createDocumentationVersion = Request.request serviceName "createDocumentationVersion" 


-- | <p>Creates a new domain name.</p>
createDomainName :: forall eff. CreateDomainNameRequest -> Aff (exception :: EXCEPTION | eff) DomainName
createDomainName = Request.request serviceName "createDomainName" 


-- | <p>Adds a new <a>Model</a> resource to an existing <a>RestApi</a> resource.</p>
createModel :: forall eff. CreateModelRequest -> Aff (exception :: EXCEPTION | eff) Model
createModel = Request.request serviceName "createModel" 


-- | <p>Creates a <a>ReqeustValidator</a> of a given <a>RestApi</a>.</p>
createRequestValidator :: forall eff. CreateRequestValidatorRequest -> Aff (exception :: EXCEPTION | eff) RequestValidator
createRequestValidator = Request.request serviceName "createRequestValidator" 


-- | <p>Creates a <a>Resource</a> resource.</p>
createResource :: forall eff. CreateResourceRequest -> Aff (exception :: EXCEPTION | eff) Resource
createResource = Request.request serviceName "createResource" 


-- | <p>Creates a new <a>RestApi</a> resource.</p>
createRestApi :: forall eff. CreateRestApiRequest -> Aff (exception :: EXCEPTION | eff) RestApi
createRestApi = Request.request serviceName "createRestApi" 


-- | <p>Creates a new <a>Stage</a> resource that references a pre-existing <a>Deployment</a> for the API. </p>
createStage :: forall eff. CreateStageRequest -> Aff (exception :: EXCEPTION | eff) Stage
createStage = Request.request serviceName "createStage" 


-- | <p>Creates a usage plan with the throttle and quota limits, as well as the associated API stages, specified in the payload. </p>
createUsagePlan :: forall eff. CreateUsagePlanRequest -> Aff (exception :: EXCEPTION | eff) UsagePlan
createUsagePlan = Request.request serviceName "createUsagePlan" 


-- | <p>Creates a usage plan key for adding an existing API key to a usage plan.</p>
createUsagePlanKey :: forall eff. CreateUsagePlanKeyRequest -> Aff (exception :: EXCEPTION | eff) UsagePlanKey
createUsagePlanKey = Request.request serviceName "createUsagePlanKey" 


-- | <p>Creates a VPC link, under the caller's account in a selected region, in an asynchronous operation that typically takes 2-4 minutes to complete and become operational. The caller must have permissions to create and update VPC Endpoint services.</p>
createVpcLink :: forall eff. CreateVpcLinkRequest -> Aff (exception :: EXCEPTION | eff) VpcLink
createVpcLink = Request.request serviceName "createVpcLink" 


-- | <p>Deletes the <a>ApiKey</a> resource.</p>
deleteApiKey :: forall eff. DeleteApiKeyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteApiKey = Request.request serviceName "deleteApiKey" 


-- | <p>Deletes an existing <a>Authorizer</a> resource.</p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/delete-authorizer.html">AWS CLI</a></div>
deleteAuthorizer :: forall eff. DeleteAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteAuthorizer = Request.request serviceName "deleteAuthorizer" 


-- | <p>Deletes the <a>BasePathMapping</a> resource.</p>
deleteBasePathMapping :: forall eff. DeleteBasePathMappingRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBasePathMapping = Request.request serviceName "deleteBasePathMapping" 


-- | <p>Deletes the <a>ClientCertificate</a> resource.</p>
deleteClientCertificate :: forall eff. DeleteClientCertificateRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteClientCertificate = Request.request serviceName "deleteClientCertificate" 


-- | <p>Deletes a <a>Deployment</a> resource. Deleting a deployment will only succeed if there are no <a>Stage</a> resources associated with it.</p>
deleteDeployment :: forall eff. DeleteDeploymentRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteDeployment = Request.request serviceName "deleteDeployment" 


deleteDocumentationPart :: forall eff. DeleteDocumentationPartRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteDocumentationPart = Request.request serviceName "deleteDocumentationPart" 


deleteDocumentationVersion :: forall eff. DeleteDocumentationVersionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteDocumentationVersion = Request.request serviceName "deleteDocumentationVersion" 


-- | <p>Deletes the <a>DomainName</a> resource.</p>
deleteDomainName :: forall eff. DeleteDomainNameRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteDomainName = Request.request serviceName "deleteDomainName" 


-- | <p>Clears any customization of a <a>GatewayResponse</a> of a specified response type on the given <a>RestApi</a> and resets it with the default settings.</p>
deleteGatewayResponse :: forall eff. DeleteGatewayResponseRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteGatewayResponse = Request.request serviceName "deleteGatewayResponse" 


-- | <p>Represents a delete integration.</p>
deleteIntegration :: forall eff. DeleteIntegrationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteIntegration = Request.request serviceName "deleteIntegration" 


-- | <p>Represents a delete integration response.</p>
deleteIntegrationResponse :: forall eff. DeleteIntegrationResponseRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteIntegrationResponse = Request.request serviceName "deleteIntegrationResponse" 


-- | <p>Deletes an existing <a>Method</a> resource.</p>
deleteMethod :: forall eff. DeleteMethodRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteMethod = Request.request serviceName "deleteMethod" 


-- | <p>Deletes an existing <a>MethodResponse</a> resource.</p>
deleteMethodResponse :: forall eff. DeleteMethodResponseRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteMethodResponse = Request.request serviceName "deleteMethodResponse" 


-- | <p>Deletes a model.</p>
deleteModel :: forall eff. DeleteModelRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteModel = Request.request serviceName "deleteModel" 


-- | <p>Deletes a <a>RequestValidator</a> of a given <a>RestApi</a>.</p>
deleteRequestValidator :: forall eff. DeleteRequestValidatorRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteRequestValidator = Request.request serviceName "deleteRequestValidator" 


-- | <p>Deletes a <a>Resource</a> resource.</p>
deleteResource :: forall eff. DeleteResourceRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteResource = Request.request serviceName "deleteResource" 


-- | <p>Deletes the specified API.</p>
deleteRestApi :: forall eff. DeleteRestApiRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteRestApi = Request.request serviceName "deleteRestApi" 


-- | <p>Deletes a <a>Stage</a> resource.</p>
deleteStage :: forall eff. DeleteStageRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteStage = Request.request serviceName "deleteStage" 


-- | <p>Deletes a usage plan of a given plan Id.</p>
deleteUsagePlan :: forall eff. DeleteUsagePlanRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteUsagePlan = Request.request serviceName "deleteUsagePlan" 


-- | <p>Deletes a usage plan key and remove the underlying API key from the associated usage plan.</p>
deleteUsagePlanKey :: forall eff. DeleteUsagePlanKeyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteUsagePlanKey = Request.request serviceName "deleteUsagePlanKey" 


-- | <p>Deletes an existing <a>VpcLink</a> of a specified identifier.</p>
deleteVpcLink :: forall eff. DeleteVpcLinkRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteVpcLink = Request.request serviceName "deleteVpcLink" 


-- | <p>Flushes all authorizer cache entries on a stage.</p>
flushStageAuthorizersCache :: forall eff. FlushStageAuthorizersCacheRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
flushStageAuthorizersCache = Request.request serviceName "flushStageAuthorizersCache" 


-- | <p>Flushes a stage's cache.</p>
flushStageCache :: forall eff. FlushStageCacheRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
flushStageCache = Request.request serviceName "flushStageCache" 


-- | <p>Generates a <a>ClientCertificate</a> resource.</p>
generateClientCertificate :: forall eff. GenerateClientCertificateRequest -> Aff (exception :: EXCEPTION | eff) ClientCertificate
generateClientCertificate = Request.request serviceName "generateClientCertificate" 


-- | <p>Gets information about the current <a>Account</a> resource.</p>
getAccount :: forall eff. GetAccountRequest -> Aff (exception :: EXCEPTION | eff) Account
getAccount = Request.request serviceName "getAccount" 


-- | <p>Gets information about the current <a>ApiKey</a> resource.</p>
getApiKey :: forall eff. GetApiKeyRequest -> Aff (exception :: EXCEPTION | eff) ApiKey
getApiKey = Request.request serviceName "getApiKey" 


-- | <p>Gets information about the current <a>ApiKeys</a> resource.</p>
getApiKeys :: forall eff. GetApiKeysRequest -> Aff (exception :: EXCEPTION | eff) ApiKeys
getApiKeys = Request.request serviceName "getApiKeys" 


-- | <p>Describe an existing <a>Authorizer</a> resource.</p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-authorizer.html">AWS CLI</a></div>
getAuthorizer :: forall eff. GetAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) Authorizer
getAuthorizer = Request.request serviceName "getAuthorizer" 


-- | <p>Describe an existing <a>Authorizers</a> resource.</p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-authorizers.html">AWS CLI</a></div>
getAuthorizers :: forall eff. GetAuthorizersRequest -> Aff (exception :: EXCEPTION | eff) Authorizers
getAuthorizers = Request.request serviceName "getAuthorizers" 


-- | <p>Describe a <a>BasePathMapping</a> resource.</p>
getBasePathMapping :: forall eff. GetBasePathMappingRequest -> Aff (exception :: EXCEPTION | eff) BasePathMapping
getBasePathMapping = Request.request serviceName "getBasePathMapping" 


-- | <p>Represents a collection of <a>BasePathMapping</a> resources.</p>
getBasePathMappings :: forall eff. GetBasePathMappingsRequest -> Aff (exception :: EXCEPTION | eff) BasePathMappings
getBasePathMappings = Request.request serviceName "getBasePathMappings" 


-- | <p>Gets information about the current <a>ClientCertificate</a> resource.</p>
getClientCertificate :: forall eff. GetClientCertificateRequest -> Aff (exception :: EXCEPTION | eff) ClientCertificate
getClientCertificate = Request.request serviceName "getClientCertificate" 


-- | <p>Gets a collection of <a>ClientCertificate</a> resources.</p>
getClientCertificates :: forall eff. GetClientCertificatesRequest -> Aff (exception :: EXCEPTION | eff) ClientCertificates
getClientCertificates = Request.request serviceName "getClientCertificates" 


-- | <p>Gets information about a <a>Deployment</a> resource.</p>
getDeployment :: forall eff. GetDeploymentRequest -> Aff (exception :: EXCEPTION | eff) Deployment
getDeployment = Request.request serviceName "getDeployment" 


-- | <p>Gets information about a <a>Deployments</a> collection.</p>
getDeployments :: forall eff. GetDeploymentsRequest -> Aff (exception :: EXCEPTION | eff) Deployments
getDeployments = Request.request serviceName "getDeployments" 


getDocumentationPart :: forall eff. GetDocumentationPartRequest -> Aff (exception :: EXCEPTION | eff) DocumentationPart
getDocumentationPart = Request.request serviceName "getDocumentationPart" 


getDocumentationParts :: forall eff. GetDocumentationPartsRequest -> Aff (exception :: EXCEPTION | eff) DocumentationParts
getDocumentationParts = Request.request serviceName "getDocumentationParts" 


getDocumentationVersion :: forall eff. GetDocumentationVersionRequest -> Aff (exception :: EXCEPTION | eff) DocumentationVersion
getDocumentationVersion = Request.request serviceName "getDocumentationVersion" 


getDocumentationVersions :: forall eff. GetDocumentationVersionsRequest -> Aff (exception :: EXCEPTION | eff) DocumentationVersions
getDocumentationVersions = Request.request serviceName "getDocumentationVersions" 


-- | <p>Represents a domain name that is contained in a simpler, more intuitive URL that can be called.</p>
getDomainName :: forall eff. GetDomainNameRequest -> Aff (exception :: EXCEPTION | eff) DomainName
getDomainName = Request.request serviceName "getDomainName" 


-- | <p>Represents a collection of <a>DomainName</a> resources.</p>
getDomainNames :: forall eff. GetDomainNamesRequest -> Aff (exception :: EXCEPTION | eff) DomainNames
getDomainNames = Request.request serviceName "getDomainNames" 


-- | <p>Exports a deployed version of a <a>RestApi</a> in a specified format.</p>
getExport :: forall eff. GetExportRequest -> Aff (exception :: EXCEPTION | eff) ExportResponse
getExport = Request.request serviceName "getExport" 


-- | <p>Gets a <a>GatewayResponse</a> of a specified response type on the given <a>RestApi</a>.</p>
getGatewayResponse :: forall eff. GetGatewayResponseRequest -> Aff (exception :: EXCEPTION | eff) GatewayResponse
getGatewayResponse = Request.request serviceName "getGatewayResponse" 


-- | <p>Gets the <a>GatewayResponses</a> collection on the given <a>RestApi</a>. If an API developer has not added any definitions for gateway responses, the result will be the API Gateway-generated default <a>GatewayResponses</a> collection for the supported response types.</p>
getGatewayResponses :: forall eff. GetGatewayResponsesRequest -> Aff (exception :: EXCEPTION | eff) GatewayResponses
getGatewayResponses = Request.request serviceName "getGatewayResponses" 


-- | <p>Get the integration settings.</p>
getIntegration :: forall eff. GetIntegrationRequest -> Aff (exception :: EXCEPTION | eff) Integration
getIntegration = Request.request serviceName "getIntegration" 


-- | <p>Represents a get integration response.</p>
getIntegrationResponse :: forall eff. GetIntegrationResponseRequest -> Aff (exception :: EXCEPTION | eff) IntegrationResponse
getIntegrationResponse = Request.request serviceName "getIntegrationResponse" 


-- | <p>Describe an existing <a>Method</a> resource.</p>
getMethod :: forall eff. GetMethodRequest -> Aff (exception :: EXCEPTION | eff) Method
getMethod = Request.request serviceName "getMethod" 


-- | <p>Describes a <a>MethodResponse</a> resource.</p>
getMethodResponse :: forall eff. GetMethodResponseRequest -> Aff (exception :: EXCEPTION | eff) MethodResponse
getMethodResponse = Request.request serviceName "getMethodResponse" 


-- | <p>Describes an existing model defined for a <a>RestApi</a> resource.</p>
getModel :: forall eff. GetModelRequest -> Aff (exception :: EXCEPTION | eff) Model
getModel = Request.request serviceName "getModel" 


-- | <p>Generates a sample mapping template that can be used to transform a payload into the structure of a model.</p>
getModelTemplate :: forall eff. GetModelTemplateRequest -> Aff (exception :: EXCEPTION | eff) Template
getModelTemplate = Request.request serviceName "getModelTemplate" 


-- | <p>Describes existing <a>Models</a> defined for a <a>RestApi</a> resource.</p>
getModels :: forall eff. GetModelsRequest -> Aff (exception :: EXCEPTION | eff) Models
getModels = Request.request serviceName "getModels" 


-- | <p>Gets a <a>RequestValidator</a> of a given <a>RestApi</a>.</p>
getRequestValidator :: forall eff. GetRequestValidatorRequest -> Aff (exception :: EXCEPTION | eff) RequestValidator
getRequestValidator = Request.request serviceName "getRequestValidator" 


-- | <p>Gets the <a>RequestValidators</a> collection of a given <a>RestApi</a>.</p>
getRequestValidators :: forall eff. GetRequestValidatorsRequest -> Aff (exception :: EXCEPTION | eff) RequestValidators
getRequestValidators = Request.request serviceName "getRequestValidators" 


-- | <p>Lists information about a resource.</p>
getResource :: forall eff. GetResourceRequest -> Aff (exception :: EXCEPTION | eff) Resource
getResource = Request.request serviceName "getResource" 


-- | <p>Lists information about a collection of <a>Resource</a> resources.</p>
getResources :: forall eff. GetResourcesRequest -> Aff (exception :: EXCEPTION | eff) Resources
getResources = Request.request serviceName "getResources" 


-- | <p>Lists the <a>RestApi</a> resource in the collection.</p>
getRestApi :: forall eff. GetRestApiRequest -> Aff (exception :: EXCEPTION | eff) RestApi
getRestApi = Request.request serviceName "getRestApi" 


-- | <p>Lists the <a>RestApis</a> resources for your collection.</p>
getRestApis :: forall eff. GetRestApisRequest -> Aff (exception :: EXCEPTION | eff) RestApis
getRestApis = Request.request serviceName "getRestApis" 


-- | <p>Generates a client SDK for a <a>RestApi</a> and <a>Stage</a>.</p>
getSdk :: forall eff. GetSdkRequest -> Aff (exception :: EXCEPTION | eff) SdkResponse
getSdk = Request.request serviceName "getSdk" 


getSdkType :: forall eff. GetSdkTypeRequest -> Aff (exception :: EXCEPTION | eff) SdkType
getSdkType = Request.request serviceName "getSdkType" 


getSdkTypes :: forall eff. GetSdkTypesRequest -> Aff (exception :: EXCEPTION | eff) SdkTypes
getSdkTypes = Request.request serviceName "getSdkTypes" 


-- | <p>Gets information about a <a>Stage</a> resource.</p>
getStage :: forall eff. GetStageRequest -> Aff (exception :: EXCEPTION | eff) Stage
getStage = Request.request serviceName "getStage" 


-- | <p>Gets information about one or more <a>Stage</a> resources.</p>
getStages :: forall eff. GetStagesRequest -> Aff (exception :: EXCEPTION | eff) Stages
getStages = Request.request serviceName "getStages" 


-- | <p>Gets the Tags collection for a given resource.</p>
getTags :: forall eff. GetTagsRequest -> Aff (exception :: EXCEPTION | eff) Tags
getTags = Request.request serviceName "getTags" 


-- | <p>Gets the usage data of a usage plan in a specified time interval.</p>
getUsage :: forall eff. GetUsageRequest -> Aff (exception :: EXCEPTION | eff) Usage
getUsage = Request.request serviceName "getUsage" 


-- | <p>Gets a usage plan of a given plan identifier.</p>
getUsagePlan :: forall eff. GetUsagePlanRequest -> Aff (exception :: EXCEPTION | eff) UsagePlan
getUsagePlan = Request.request serviceName "getUsagePlan" 


-- | <p>Gets a usage plan key of a given key identifier.</p>
getUsagePlanKey :: forall eff. GetUsagePlanKeyRequest -> Aff (exception :: EXCEPTION | eff) UsagePlanKey
getUsagePlanKey = Request.request serviceName "getUsagePlanKey" 


-- | <p>Gets all the usage plan keys representing the API keys added to a specified usage plan.</p>
getUsagePlanKeys :: forall eff. GetUsagePlanKeysRequest -> Aff (exception :: EXCEPTION | eff) UsagePlanKeys
getUsagePlanKeys = Request.request serviceName "getUsagePlanKeys" 


-- | <p>Gets all the usage plans of the caller's account.</p>
getUsagePlans :: forall eff. GetUsagePlansRequest -> Aff (exception :: EXCEPTION | eff) UsagePlans
getUsagePlans = Request.request serviceName "getUsagePlans" 


-- | <p>Gets a specified VPC link under the caller's account in a region.</p>
getVpcLink :: forall eff. GetVpcLinkRequest -> Aff (exception :: EXCEPTION | eff) VpcLink
getVpcLink = Request.request serviceName "getVpcLink" 


-- | <p>Gets the <a>VpcLinks</a> collection under the caller's account in a selected region.</p>
getVpcLinks :: forall eff. GetVpcLinksRequest -> Aff (exception :: EXCEPTION | eff) VpcLinks
getVpcLinks = Request.request serviceName "getVpcLinks" 


-- | <p>Import API keys from an external source, such as a CSV-formatted file.</p>
importApiKeys :: forall eff. ImportApiKeysRequest -> Aff (exception :: EXCEPTION | eff) ApiKeyIds
importApiKeys = Request.request serviceName "importApiKeys" 


importDocumentationParts :: forall eff. ImportDocumentationPartsRequest -> Aff (exception :: EXCEPTION | eff) DocumentationPartIds
importDocumentationParts = Request.request serviceName "importDocumentationParts" 


-- | <p>A feature of the API Gateway control service for creating a new API from an external API definition file.</p>
importRestApi :: forall eff. ImportRestApiRequest -> Aff (exception :: EXCEPTION | eff) RestApi
importRestApi = Request.request serviceName "importRestApi" 


-- | <p>Creates a customization of a <a>GatewayResponse</a> of a specified response type and status code on the given <a>RestApi</a>.</p>
putGatewayResponse :: forall eff. PutGatewayResponseRequest -> Aff (exception :: EXCEPTION | eff) GatewayResponse
putGatewayResponse = Request.request serviceName "putGatewayResponse" 


-- | <p>Sets up a method's integration.</p>
putIntegration :: forall eff. PutIntegrationRequest -> Aff (exception :: EXCEPTION | eff) Integration
putIntegration = Request.request serviceName "putIntegration" 


-- | <p>Represents a put integration.</p>
putIntegrationResponse :: forall eff. PutIntegrationResponseRequest -> Aff (exception :: EXCEPTION | eff) IntegrationResponse
putIntegrationResponse = Request.request serviceName "putIntegrationResponse" 


-- | <p>Add a method to an existing <a>Resource</a> resource.</p>
putMethod :: forall eff. PutMethodRequest -> Aff (exception :: EXCEPTION | eff) Method
putMethod = Request.request serviceName "putMethod" 


-- | <p>Adds a <a>MethodResponse</a> to an existing <a>Method</a> resource.</p>
putMethodResponse :: forall eff. PutMethodResponseRequest -> Aff (exception :: EXCEPTION | eff) MethodResponse
putMethodResponse = Request.request serviceName "putMethodResponse" 


-- | <p>A feature of the API Gateway control service for updating an existing API with an input of external API definitions. The update can take the form of merging the supplied definition into the existing API or overwriting the existing API.</p>
putRestApi :: forall eff. PutRestApiRequest -> Aff (exception :: EXCEPTION | eff) RestApi
putRestApi = Request.request serviceName "putRestApi" 


-- | <p>Adds or updates Tags on a gievn resource.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
tagResource = Request.request serviceName "tagResource" 


-- | <p>Simulate the execution of an <a>Authorizer</a> in your <a>RestApi</a> with headers, parameters, and an incoming request body.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/use-custom-authorizer.html">Enable custom authorizers</a> </div>
testInvokeAuthorizer :: forall eff. TestInvokeAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) TestInvokeAuthorizerResponse
testInvokeAuthorizer = Request.request serviceName "testInvokeAuthorizer" 


-- | <p>Simulate the execution of a <a>Method</a> in your <a>RestApi</a> with headers, parameters, and an incoming request body.</p>
testInvokeMethod :: forall eff. TestInvokeMethodRequest -> Aff (exception :: EXCEPTION | eff) TestInvokeMethodResponse
testInvokeMethod = Request.request serviceName "testInvokeMethod" 


-- | <p>Removes Tags from a given resource.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
untagResource = Request.request serviceName "untagResource" 


-- | <p>Changes information about the current <a>Account</a> resource.</p>
updateAccount :: forall eff. UpdateAccountRequest -> Aff (exception :: EXCEPTION | eff) Account
updateAccount = Request.request serviceName "updateAccount" 


-- | <p>Changes information about an <a>ApiKey</a> resource.</p>
updateApiKey :: forall eff. UpdateApiKeyRequest -> Aff (exception :: EXCEPTION | eff) ApiKey
updateApiKey = Request.request serviceName "updateApiKey" 


-- | <p>Updates an existing <a>Authorizer</a> resource.</p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/update-authorizer.html">AWS CLI</a></div>
updateAuthorizer :: forall eff. UpdateAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) Authorizer
updateAuthorizer = Request.request serviceName "updateAuthorizer" 


-- | <p>Changes information about the <a>BasePathMapping</a> resource.</p>
updateBasePathMapping :: forall eff. UpdateBasePathMappingRequest -> Aff (exception :: EXCEPTION | eff) BasePathMapping
updateBasePathMapping = Request.request serviceName "updateBasePathMapping" 


-- | <p>Changes information about an <a>ClientCertificate</a> resource.</p>
updateClientCertificate :: forall eff. UpdateClientCertificateRequest -> Aff (exception :: EXCEPTION | eff) ClientCertificate
updateClientCertificate = Request.request serviceName "updateClientCertificate" 


-- | <p>Changes information about a <a>Deployment</a> resource.</p>
updateDeployment :: forall eff. UpdateDeploymentRequest -> Aff (exception :: EXCEPTION | eff) Deployment
updateDeployment = Request.request serviceName "updateDeployment" 


updateDocumentationPart :: forall eff. UpdateDocumentationPartRequest -> Aff (exception :: EXCEPTION | eff) DocumentationPart
updateDocumentationPart = Request.request serviceName "updateDocumentationPart" 


updateDocumentationVersion :: forall eff. UpdateDocumentationVersionRequest -> Aff (exception :: EXCEPTION | eff) DocumentationVersion
updateDocumentationVersion = Request.request serviceName "updateDocumentationVersion" 


-- | <p>Changes information about the <a>DomainName</a> resource.</p>
updateDomainName :: forall eff. UpdateDomainNameRequest -> Aff (exception :: EXCEPTION | eff) DomainName
updateDomainName = Request.request serviceName "updateDomainName" 


-- | <p>Updates a <a>GatewayResponse</a> of a specified response type on the given <a>RestApi</a>.</p>
updateGatewayResponse :: forall eff. UpdateGatewayResponseRequest -> Aff (exception :: EXCEPTION | eff) GatewayResponse
updateGatewayResponse = Request.request serviceName "updateGatewayResponse" 


-- | <p>Represents an update integration.</p>
updateIntegration :: forall eff. UpdateIntegrationRequest -> Aff (exception :: EXCEPTION | eff) Integration
updateIntegration = Request.request serviceName "updateIntegration" 


-- | <p>Represents an update integration response.</p>
updateIntegrationResponse :: forall eff. UpdateIntegrationResponseRequest -> Aff (exception :: EXCEPTION | eff) IntegrationResponse
updateIntegrationResponse = Request.request serviceName "updateIntegrationResponse" 


-- | <p>Updates an existing <a>Method</a> resource.</p>
updateMethod :: forall eff. UpdateMethodRequest -> Aff (exception :: EXCEPTION | eff) Method
updateMethod = Request.request serviceName "updateMethod" 


-- | <p>Updates an existing <a>MethodResponse</a> resource.</p>
updateMethodResponse :: forall eff. UpdateMethodResponseRequest -> Aff (exception :: EXCEPTION | eff) MethodResponse
updateMethodResponse = Request.request serviceName "updateMethodResponse" 


-- | <p>Changes information about a model.</p>
updateModel :: forall eff. UpdateModelRequest -> Aff (exception :: EXCEPTION | eff) Model
updateModel = Request.request serviceName "updateModel" 


-- | <p>Updates a <a>RequestValidator</a> of a given <a>RestApi</a>.</p>
updateRequestValidator :: forall eff. UpdateRequestValidatorRequest -> Aff (exception :: EXCEPTION | eff) RequestValidator
updateRequestValidator = Request.request serviceName "updateRequestValidator" 


-- | <p>Changes information about a <a>Resource</a> resource.</p>
updateResource :: forall eff. UpdateResourceRequest -> Aff (exception :: EXCEPTION | eff) Resource
updateResource = Request.request serviceName "updateResource" 


-- | <p>Changes information about the specified API.</p>
updateRestApi :: forall eff. UpdateRestApiRequest -> Aff (exception :: EXCEPTION | eff) RestApi
updateRestApi = Request.request serviceName "updateRestApi" 


-- | <p>Changes information about a <a>Stage</a> resource.</p>
updateStage :: forall eff. UpdateStageRequest -> Aff (exception :: EXCEPTION | eff) Stage
updateStage = Request.request serviceName "updateStage" 


-- | <p>Grants a temporary extension to the remaining quota of a usage plan associated with a specified API key.</p>
updateUsage :: forall eff. UpdateUsageRequest -> Aff (exception :: EXCEPTION | eff) Usage
updateUsage = Request.request serviceName "updateUsage" 


-- | <p>Updates a usage plan of a given plan Id.</p>
updateUsagePlan :: forall eff. UpdateUsagePlanRequest -> Aff (exception :: EXCEPTION | eff) UsagePlan
updateUsagePlan = Request.request serviceName "updateUsagePlan" 


-- | <p>Updates an existing <a>VpcLink</a> of a specified identifier.</p>
updateVpcLink :: forall eff. UpdateVpcLinkRequest -> Aff (exception :: EXCEPTION | eff) VpcLink
updateVpcLink = Request.request serviceName "updateVpcLink" 


-- | <p>Access log settings, including the access log format and access log destination ARN.</p>
newtype AccessLogSettings = AccessLogSettings 
  { "Format'" :: NullOrUndefined.NullOrUndefined (String)
  , "DestinationArn'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAccessLogSettings :: Newtype AccessLogSettings _
derive instance repGenericAccessLogSettings :: Generic AccessLogSettings _
instance showAccessLogSettings :: Show AccessLogSettings where
  show = genericShow
instance decodeAccessLogSettings :: Decode AccessLogSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessLogSettings :: Encode AccessLogSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an AWS account that is associated with API Gateway.</p> <div class="remarks"> <p>To view the account info, call <code>GET</code> on this resource.</p> <h4>Error Codes</h4> <p>The following exception may be thrown when the request fails.</p> <ul> <li>UnauthorizedException</li> <li>NotFoundException</li> <li>TooManyRequestsException</li> </ul> <p>For detailed error code information, including the corresponding HTTP Status Codes, see <a href="http://docs.aws.amazon.com/apigateway/api-reference/handling-errors/#api-error-codes">API Gateway Error Codes</a></p> <h4>Example: Get the information about an account.</h4> <h5>Request</h5> <pre><code>GET /account HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160531T184618Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash} </code></pre> <h5>Response</h5> <p>The successful response returns a <code>200 OK</code> status code and a payload similar to the following:</p> <pre><code>{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/account-apigateway-{rel}.html", "name": "account", "templated": true }, "self": { "href": "/account" }, "account:update": { "href": "/account" } }, "cloudwatchRoleArn": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "throttleSettings": { "rateLimit": 500, "burstLimit": 1000 } } </code></pre> <p>In addition to making the REST API call directly, you can use the AWS CLI and an AWS SDK to access this resource.</p> </div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-limits.html">API Gateway Limits</a> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/welcome.html">Developer Guide</a>, <a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-account.html">AWS CLI</a> </div>
newtype Account = Account 
  { "CloudwatchRoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "ThrottleSettings'" :: NullOrUndefined.NullOrUndefined (ThrottleSettings)
  , "Features'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  , "ApiKeyVersion'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAccount :: Newtype Account _
derive instance repGenericAccount :: Generic Account _
instance showAccount :: Show Account where
  show = genericShow
instance decodeAccount :: Decode Account where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccount :: Encode Account where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A resource that can be distributed to callers for executing <a>Method</a> resources that require an API key. API keys can be mapped to any <a>Stage</a> on any <a>RestApi</a>, which indicates that the callers with the API key can make requests to that stage.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-api-keys.html">Use API Keys</a> </div>
newtype ApiKey = ApiKey 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Value'" :: NullOrUndefined.NullOrUndefined (String)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "CustomerId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "StageKeys'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  }
derive instance newtypeApiKey :: Newtype ApiKey _
derive instance repGenericApiKey :: Generic ApiKey _
instance showApiKey :: Show ApiKey where
  show = genericShow
instance decodeApiKey :: Decode ApiKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApiKey :: Encode ApiKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The identifier of an <a>ApiKey</a> used in a <a>UsagePlan</a>.</p>
newtype ApiKeyIds = ApiKeyIds 
  { "Ids'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  , "Warnings'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  }
derive instance newtypeApiKeyIds :: Newtype ApiKeyIds _
derive instance repGenericApiKeyIds :: Generic ApiKeyIds _
instance showApiKeyIds :: Show ApiKeyIds where
  show = genericShow
instance decodeApiKeyIds :: Decode ApiKeyIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApiKeyIds :: Encode ApiKeyIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApiKeySourceType = ApiKeySourceType String
derive instance newtypeApiKeySourceType :: Newtype ApiKeySourceType _
derive instance repGenericApiKeySourceType :: Generic ApiKeySourceType _
instance showApiKeySourceType :: Show ApiKeySourceType where
  show = genericShow
instance decodeApiKeySourceType :: Decode ApiKeySourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApiKeySourceType :: Encode ApiKeySourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection of API keys as represented by an <a>ApiKeys</a> resource.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-api-keys.html">Use API Keys</a> </div>
newtype ApiKeys = ApiKeys 
  { "Warnings'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfApiKey)
  }
derive instance newtypeApiKeys :: Newtype ApiKeys _
derive instance repGenericApiKeys :: Generic ApiKeys _
instance showApiKeys :: Show ApiKeys where
  show = genericShow
instance decodeApiKeys :: Decode ApiKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApiKeys :: Encode ApiKeys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApiKeysFormat = ApiKeysFormat String
derive instance newtypeApiKeysFormat :: Newtype ApiKeysFormat _
derive instance repGenericApiKeysFormat :: Generic ApiKeysFormat _
instance showApiKeysFormat :: Show ApiKeysFormat where
  show = genericShow
instance decodeApiKeysFormat :: Decode ApiKeysFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApiKeysFormat :: Encode ApiKeysFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>API stage name of the associated API stage in a usage plan.</p>
newtype ApiStage = ApiStage 
  { "ApiId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Stage'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeApiStage :: Newtype ApiStage _
derive instance repGenericApiStage :: Generic ApiStage _
instance showApiStage :: Show ApiStage where
  show = genericShow
instance decodeApiStage :: Decode ApiStage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApiStage :: Encode ApiStage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an authorization layer for methods. If enabled on a method, API Gateway will activate the authorizer when a client calls the method.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/use-custom-authorizer.html">Enable custom authorization</a> </div>
newtype Authorizer = Authorizer 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Type'" :: NullOrUndefined.NullOrUndefined (AuthorizerType)
  , "ProviderARNs'" :: NullOrUndefined.NullOrUndefined (ListOfARNs)
  , "AuthType'" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthorizerUri'" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthorizerCredentials'" :: NullOrUndefined.NullOrUndefined (String)
  , "IdentitySource'" :: NullOrUndefined.NullOrUndefined (String)
  , "IdentityValidationExpression'" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthorizerResultTtlInSeconds'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeAuthorizer :: Newtype Authorizer _
derive instance repGenericAuthorizer :: Generic Authorizer _
instance showAuthorizer :: Show Authorizer where
  show = genericShow
instance decodeAuthorizer :: Decode Authorizer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizer :: Encode Authorizer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>[Required] The authorizer type. Valid values are <code>TOKEN</code> for a Lambda function using a single authorization token submitted in a custom header, <code>REQUEST</code> for a Lambda function using incoming request parameters, and <code>COGNITO_USER_POOLS</code> for using an Amazon Cognito user pool.</p>
newtype AuthorizerType = AuthorizerType String
derive instance newtypeAuthorizerType :: Newtype AuthorizerType _
derive instance repGenericAuthorizerType :: Generic AuthorizerType _
instance showAuthorizerType :: Show AuthorizerType where
  show = genericShow
instance decodeAuthorizerType :: Decode AuthorizerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizerType :: Encode AuthorizerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection of <a>Authorizer</a> resources.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/use-custom-authorizer.html">Enable custom authorization</a> </div>
newtype Authorizers = Authorizers 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfAuthorizer)
  }
derive instance newtypeAuthorizers :: Newtype Authorizers _
derive instance repGenericAuthorizers :: Generic Authorizers _
instance showAuthorizers :: Show Authorizers where
  show = genericShow
instance decodeAuthorizers :: Decode Authorizers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizers :: Encode Authorizers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The submitted request is not valid, for example, the input is incomplete or incorrect. See the accompanying error message for details.</p>
newtype BadRequestException = BadRequestException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _
derive instance repGenericBadRequestException :: Generic BadRequestException _
instance showBadRequestException :: Show BadRequestException where
  show = genericShow
instance decodeBadRequestException :: Decode BadRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequestException :: Encode BadRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the base path that callers of the API must provide as part of the URL after the domain name.</p> <div class="remarks">A custom domain name plus a <code>BasePathMapping</code> specification identifies a deployed <a>RestApi</a> in a given stage of the owner <a>Account</a>.</div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html">Use Custom Domain Names</a> </div>
newtype BasePathMapping = BasePathMapping 
  { "BasePath'" :: NullOrUndefined.NullOrUndefined (String)
  , "RestApiId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Stage'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBasePathMapping :: Newtype BasePathMapping _
derive instance repGenericBasePathMapping :: Generic BasePathMapping _
instance showBasePathMapping :: Show BasePathMapping where
  show = genericShow
instance decodeBasePathMapping :: Decode BasePathMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBasePathMapping :: Encode BasePathMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection of <a>BasePathMapping</a> resources.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html">Use Custom Domain Names</a> </div>
newtype BasePathMappings = BasePathMappings 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfBasePathMapping)
  }
derive instance newtypeBasePathMappings :: Newtype BasePathMappings _
derive instance repGenericBasePathMappings :: Generic BasePathMappings _
instance showBasePathMappings :: Show BasePathMappings where
  show = genericShow
instance decodeBasePathMappings :: Decode BasePathMappings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBasePathMappings :: Encode BasePathMappings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the size of the <b>CacheCluster</b>.</p>
newtype CacheClusterSize = CacheClusterSize String
derive instance newtypeCacheClusterSize :: Newtype CacheClusterSize _
derive instance repGenericCacheClusterSize :: Generic CacheClusterSize _
instance showCacheClusterSize :: Show CacheClusterSize where
  show = genericShow
instance decodeCacheClusterSize :: Decode CacheClusterSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheClusterSize :: Encode CacheClusterSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the status of the <b>CacheCluster</b>.</p>
newtype CacheClusterStatus = CacheClusterStatus String
derive instance newtypeCacheClusterStatus :: Newtype CacheClusterStatus _
derive instance repGenericCacheClusterStatus :: Generic CacheClusterStatus _
instance showCacheClusterStatus :: Show CacheClusterStatus where
  show = genericShow
instance decodeCacheClusterStatus :: Decode CacheClusterStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheClusterStatus :: Encode CacheClusterStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration settings of a canary deployment.</p>
newtype CanarySettings = CanarySettings 
  { "PercentTraffic'" :: NullOrUndefined.NullOrUndefined (Number)
  , "DeploymentId'" :: NullOrUndefined.NullOrUndefined (String)
  , "StageVariableOverrides'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "UseStageCache'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeCanarySettings :: Newtype CanarySettings _
derive instance repGenericCanarySettings :: Generic CanarySettings _
instance showCanarySettings :: Show CanarySettings where
  show = genericShow
instance decodeCanarySettings :: Decode CanarySettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCanarySettings :: Encode CanarySettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a client certificate used to configure client-side SSL authentication while sending requests to the integration endpoint.</p> <div class="remarks">Client certificates are used to authenticate an API by the backend server. To authenticate an API client (or user), use IAM roles and policies, a custom <a>Authorizer</a> or an Amazon Cognito user pool.</div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-client-side-ssl-authentication.html">Use Client-Side Certificate</a> </div>
newtype ClientCertificate = ClientCertificate 
  { "ClientCertificateId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "PemEncodedCertificate'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "ExpirationDate'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeClientCertificate :: Newtype ClientCertificate _
derive instance repGenericClientCertificate :: Generic ClientCertificate _
instance showClientCertificate :: Show ClientCertificate where
  show = genericShow
instance decodeClientCertificate :: Decode ClientCertificate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientCertificate :: Encode ClientCertificate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection of <a>ClientCertificate</a> resources.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-client-side-ssl-authentication.html">Use Client-Side Certificate</a> </div>
newtype ClientCertificates = ClientCertificates 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfClientCertificate)
  }
derive instance newtypeClientCertificates :: Newtype ClientCertificates _
derive instance repGenericClientCertificates :: Generic ClientCertificates _
instance showClientCertificates :: Show ClientCertificates where
  show = genericShow
instance decodeClientCertificates :: Decode ClientCertificates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientCertificates :: Encode ClientCertificates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request configuration has conflicts. For details, see the accompanying error message.</p>
newtype ConflictException = ConflictException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeConflictException :: Newtype ConflictException _
derive instance repGenericConflictException :: Generic ConflictException _
instance showConflictException :: Show ConflictException where
  show = genericShow
instance decodeConflictException :: Decode ConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConflictException :: Encode ConflictException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConnectionType = ConnectionType String
derive instance newtypeConnectionType :: Newtype ConnectionType _
derive instance repGenericConnectionType :: Generic ConnectionType _
instance showConnectionType :: Show ConnectionType where
  show = genericShow
instance decodeConnectionType :: Decode ConnectionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionType :: Encode ConnectionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentHandlingStrategy = ContentHandlingStrategy String
derive instance newtypeContentHandlingStrategy :: Newtype ContentHandlingStrategy _
derive instance repGenericContentHandlingStrategy :: Generic ContentHandlingStrategy _
instance showContentHandlingStrategy :: Show ContentHandlingStrategy where
  show = genericShow
instance decodeContentHandlingStrategy :: Decode ContentHandlingStrategy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentHandlingStrategy :: Encode ContentHandlingStrategy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to create an <a>ApiKey</a> resource.</p>
newtype CreateApiKeyRequest = CreateApiKeyRequest 
  { "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "GenerateDistinctId'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Value'" :: NullOrUndefined.NullOrUndefined (String)
  , "StageKeys'" :: NullOrUndefined.NullOrUndefined (ListOfStageKeys)
  , "CustomerId'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateApiKeyRequest :: Newtype CreateApiKeyRequest _
derive instance repGenericCreateApiKeyRequest :: Generic CreateApiKeyRequest _
instance showCreateApiKeyRequest :: Show CreateApiKeyRequest where
  show = genericShow
instance decodeCreateApiKeyRequest :: Decode CreateApiKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApiKeyRequest :: Encode CreateApiKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to add a new <a>Authorizer</a> to an existing <a>RestApi</a> resource.</p>
newtype CreateAuthorizerRequest = CreateAuthorizerRequest 
  { "RestApiId'" :: (String)
  , "Name'" :: (String)
  , "Type'" :: (AuthorizerType)
  , "ProviderARNs'" :: NullOrUndefined.NullOrUndefined (ListOfARNs)
  , "AuthType'" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthorizerUri'" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthorizerCredentials'" :: NullOrUndefined.NullOrUndefined (String)
  , "IdentitySource'" :: NullOrUndefined.NullOrUndefined (String)
  , "IdentityValidationExpression'" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthorizerResultTtlInSeconds'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeCreateAuthorizerRequest :: Newtype CreateAuthorizerRequest _
derive instance repGenericCreateAuthorizerRequest :: Generic CreateAuthorizerRequest _
instance showCreateAuthorizerRequest :: Show CreateAuthorizerRequest where
  show = genericShow
instance decodeCreateAuthorizerRequest :: Decode CreateAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAuthorizerRequest :: Encode CreateAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to create a new <a>BasePathMapping</a> resource.</p>
newtype CreateBasePathMappingRequest = CreateBasePathMappingRequest 
  { "DomainName'" :: (String)
  , "BasePath'" :: NullOrUndefined.NullOrUndefined (String)
  , "RestApiId'" :: (String)
  , "Stage'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateBasePathMappingRequest :: Newtype CreateBasePathMappingRequest _
derive instance repGenericCreateBasePathMappingRequest :: Generic CreateBasePathMappingRequest _
instance showCreateBasePathMappingRequest :: Show CreateBasePathMappingRequest where
  show = genericShow
instance decodeCreateBasePathMappingRequest :: Decode CreateBasePathMappingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBasePathMappingRequest :: Encode CreateBasePathMappingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to create a <a>Deployment</a> resource.</p>
newtype CreateDeploymentRequest = CreateDeploymentRequest 
  { "RestApiId'" :: (String)
  , "StageName'" :: NullOrUndefined.NullOrUndefined (String)
  , "StageDescription'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheClusterEnabled'" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "CacheClusterSize'" :: NullOrUndefined.NullOrUndefined (CacheClusterSize)
  , "Variables'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "CanarySettings'" :: NullOrUndefined.NullOrUndefined (DeploymentCanarySettings)
  }
derive instance newtypeCreateDeploymentRequest :: Newtype CreateDeploymentRequest _
derive instance repGenericCreateDeploymentRequest :: Generic CreateDeploymentRequest _
instance showCreateDeploymentRequest :: Show CreateDeploymentRequest where
  show = genericShow
instance decodeCreateDeploymentRequest :: Decode CreateDeploymentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDeploymentRequest :: Encode CreateDeploymentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Creates a new documentation part of a given API.</p>
newtype CreateDocumentationPartRequest = CreateDocumentationPartRequest 
  { "RestApiId'" :: (String)
  , "Location'" :: (DocumentationPartLocation)
  , "Properties'" :: (String)
  }
derive instance newtypeCreateDocumentationPartRequest :: Newtype CreateDocumentationPartRequest _
derive instance repGenericCreateDocumentationPartRequest :: Generic CreateDocumentationPartRequest _
instance showCreateDocumentationPartRequest :: Show CreateDocumentationPartRequest where
  show = genericShow
instance decodeCreateDocumentationPartRequest :: Decode CreateDocumentationPartRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDocumentationPartRequest :: Encode CreateDocumentationPartRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Creates a new documentation version of a given API.</p>
newtype CreateDocumentationVersionRequest = CreateDocumentationVersionRequest 
  { "RestApiId'" :: (String)
  , "DocumentationVersion'" :: (String)
  , "StageName'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateDocumentationVersionRequest :: Newtype CreateDocumentationVersionRequest _
derive instance repGenericCreateDocumentationVersionRequest :: Generic CreateDocumentationVersionRequest _
instance showCreateDocumentationVersionRequest :: Show CreateDocumentationVersionRequest where
  show = genericShow
instance decodeCreateDocumentationVersionRequest :: Decode CreateDocumentationVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDocumentationVersionRequest :: Encode CreateDocumentationVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to create a new domain name.</p>
newtype CreateDomainNameRequest = CreateDomainNameRequest 
  { "DomainName'" :: (String)
  , "CertificateName'" :: NullOrUndefined.NullOrUndefined (String)
  , "CertificateBody'" :: NullOrUndefined.NullOrUndefined (String)
  , "CertificatePrivateKey'" :: NullOrUndefined.NullOrUndefined (String)
  , "CertificateChain'" :: NullOrUndefined.NullOrUndefined (String)
  , "CertificateArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "RegionalCertificateName'" :: NullOrUndefined.NullOrUndefined (String)
  , "RegionalCertificateArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "EndpointConfiguration'" :: NullOrUndefined.NullOrUndefined (EndpointConfiguration)
  }
derive instance newtypeCreateDomainNameRequest :: Newtype CreateDomainNameRequest _
derive instance repGenericCreateDomainNameRequest :: Generic CreateDomainNameRequest _
instance showCreateDomainNameRequest :: Show CreateDomainNameRequest where
  show = genericShow
instance decodeCreateDomainNameRequest :: Decode CreateDomainNameRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDomainNameRequest :: Encode CreateDomainNameRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to add a new <a>Model</a> to an existing <a>RestApi</a> resource.</p>
newtype CreateModelRequest = CreateModelRequest 
  { "RestApiId'" :: (String)
  , "Name'" :: (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Schema'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContentType'" :: (String)
  }
derive instance newtypeCreateModelRequest :: Newtype CreateModelRequest _
derive instance repGenericCreateModelRequest :: Generic CreateModelRequest _
instance showCreateModelRequest :: Show CreateModelRequest where
  show = genericShow
instance decodeCreateModelRequest :: Decode CreateModelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateModelRequest :: Encode CreateModelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Creates a <a>RequestValidator</a> of a given <a>RestApi</a>.</p>
newtype CreateRequestValidatorRequest = CreateRequestValidatorRequest 
  { "RestApiId'" :: (String)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "ValidateRequestBody'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ValidateRequestParameters'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeCreateRequestValidatorRequest :: Newtype CreateRequestValidatorRequest _
derive instance repGenericCreateRequestValidatorRequest :: Generic CreateRequestValidatorRequest _
instance showCreateRequestValidatorRequest :: Show CreateRequestValidatorRequest where
  show = genericShow
instance decodeCreateRequestValidatorRequest :: Decode CreateRequestValidatorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRequestValidatorRequest :: Encode CreateRequestValidatorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to create a <a>Resource</a> resource.</p>
newtype CreateResourceRequest = CreateResourceRequest 
  { "RestApiId'" :: (String)
  , "ParentId'" :: (String)
  , "PathPart'" :: (String)
  }
derive instance newtypeCreateResourceRequest :: Newtype CreateResourceRequest _
derive instance repGenericCreateResourceRequest :: Generic CreateResourceRequest _
instance showCreateResourceRequest :: Show CreateResourceRequest where
  show = genericShow
instance decodeCreateResourceRequest :: Decode CreateResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResourceRequest :: Encode CreateResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The POST Request to add a new <a>RestApi</a> resource to your collection.</p>
newtype CreateRestApiRequest = CreateRestApiRequest 
  { "Name'" :: (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Version'" :: NullOrUndefined.NullOrUndefined (String)
  , "CloneFrom'" :: NullOrUndefined.NullOrUndefined (String)
  , "BinaryMediaTypes'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  , "MinimumCompressionSize'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "ApiKeySource'" :: NullOrUndefined.NullOrUndefined (ApiKeySourceType)
  , "EndpointConfiguration'" :: NullOrUndefined.NullOrUndefined (EndpointConfiguration)
  }
derive instance newtypeCreateRestApiRequest :: Newtype CreateRestApiRequest _
derive instance repGenericCreateRestApiRequest :: Generic CreateRestApiRequest _
instance showCreateRestApiRequest :: Show CreateRestApiRequest where
  show = genericShow
instance decodeCreateRestApiRequest :: Decode CreateRestApiRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRestApiRequest :: Encode CreateRestApiRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to create a <a>Stage</a> resource.</p>
newtype CreateStageRequest = CreateStageRequest 
  { "RestApiId'" :: (String)
  , "StageName'" :: (String)
  , "DeploymentId'" :: (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheClusterEnabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "CacheClusterSize'" :: NullOrUndefined.NullOrUndefined (CacheClusterSize)
  , "Variables'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "DocumentationVersion'" :: NullOrUndefined.NullOrUndefined (String)
  , "CanarySettings'" :: NullOrUndefined.NullOrUndefined (CanarySettings)
  , "Tags'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  }
derive instance newtypeCreateStageRequest :: Newtype CreateStageRequest _
derive instance repGenericCreateStageRequest :: Generic CreateStageRequest _
instance showCreateStageRequest :: Show CreateStageRequest where
  show = genericShow
instance decodeCreateStageRequest :: Decode CreateStageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStageRequest :: Encode CreateStageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The POST request to create a usage plan key for adding an existing API key to a usage plan.</p>
newtype CreateUsagePlanKeyRequest = CreateUsagePlanKeyRequest 
  { "UsagePlanId'" :: (String)
  , "KeyId'" :: (String)
  , "KeyType'" :: (String)
  }
derive instance newtypeCreateUsagePlanKeyRequest :: Newtype CreateUsagePlanKeyRequest _
derive instance repGenericCreateUsagePlanKeyRequest :: Generic CreateUsagePlanKeyRequest _
instance showCreateUsagePlanKeyRequest :: Show CreateUsagePlanKeyRequest where
  show = genericShow
instance decodeCreateUsagePlanKeyRequest :: Decode CreateUsagePlanKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUsagePlanKeyRequest :: Encode CreateUsagePlanKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The POST request to create a usage plan with the name, description, throttle limits and quota limits, as well as the associated API stages, specified in the payload.</p>
newtype CreateUsagePlanRequest = CreateUsagePlanRequest 
  { "Name'" :: (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "ApiStages'" :: NullOrUndefined.NullOrUndefined (ListOfApiStage)
  , "Throttle'" :: NullOrUndefined.NullOrUndefined (ThrottleSettings)
  , "Quota'" :: NullOrUndefined.NullOrUndefined (QuotaSettings)
  }
derive instance newtypeCreateUsagePlanRequest :: Newtype CreateUsagePlanRequest _
derive instance repGenericCreateUsagePlanRequest :: Generic CreateUsagePlanRequest _
instance showCreateUsagePlanRequest :: Show CreateUsagePlanRequest where
  show = genericShow
instance decodeCreateUsagePlanRequest :: Decode CreateUsagePlanRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUsagePlanRequest :: Encode CreateUsagePlanRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Creates a VPC link, under the caller's account in a selected region, in an asynchronous operation that typically takes 2-4 minutes to complete and become operational. The caller must have permissions to create and update VPC Endpoint services.</p>
newtype CreateVpcLinkRequest = CreateVpcLinkRequest 
  { "Name'" :: (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "TargetArns'" :: (ListOfString)
  }
derive instance newtypeCreateVpcLinkRequest :: Newtype CreateVpcLinkRequest _
derive instance repGenericCreateVpcLinkRequest :: Generic CreateVpcLinkRequest _
instance showCreateVpcLinkRequest :: Show CreateVpcLinkRequest where
  show = genericShow
instance decodeCreateVpcLinkRequest :: Decode CreateVpcLinkRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateVpcLinkRequest :: Encode CreateVpcLinkRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to delete the <a>ApiKey</a> resource.</p>
newtype DeleteApiKeyRequest = DeleteApiKeyRequest 
  { "ApiKey'" :: (String)
  }
derive instance newtypeDeleteApiKeyRequest :: Newtype DeleteApiKeyRequest _
derive instance repGenericDeleteApiKeyRequest :: Generic DeleteApiKeyRequest _
instance showDeleteApiKeyRequest :: Show DeleteApiKeyRequest where
  show = genericShow
instance decodeDeleteApiKeyRequest :: Decode DeleteApiKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApiKeyRequest :: Encode DeleteApiKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to delete an existing <a>Authorizer</a> resource.</p>
newtype DeleteAuthorizerRequest = DeleteAuthorizerRequest 
  { "RestApiId'" :: (String)
  , "AuthorizerId'" :: (String)
  }
derive instance newtypeDeleteAuthorizerRequest :: Newtype DeleteAuthorizerRequest _
derive instance repGenericDeleteAuthorizerRequest :: Generic DeleteAuthorizerRequest _
instance showDeleteAuthorizerRequest :: Show DeleteAuthorizerRequest where
  show = genericShow
instance decodeDeleteAuthorizerRequest :: Decode DeleteAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAuthorizerRequest :: Encode DeleteAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to delete the <a>BasePathMapping</a> resource.</p>
newtype DeleteBasePathMappingRequest = DeleteBasePathMappingRequest 
  { "DomainName'" :: (String)
  , "BasePath'" :: (String)
  }
derive instance newtypeDeleteBasePathMappingRequest :: Newtype DeleteBasePathMappingRequest _
derive instance repGenericDeleteBasePathMappingRequest :: Generic DeleteBasePathMappingRequest _
instance showDeleteBasePathMappingRequest :: Show DeleteBasePathMappingRequest where
  show = genericShow
instance decodeDeleteBasePathMappingRequest :: Decode DeleteBasePathMappingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBasePathMappingRequest :: Encode DeleteBasePathMappingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to delete the <a>ClientCertificate</a> resource.</p>
newtype DeleteClientCertificateRequest = DeleteClientCertificateRequest 
  { "ClientCertificateId'" :: (String)
  }
derive instance newtypeDeleteClientCertificateRequest :: Newtype DeleteClientCertificateRequest _
derive instance repGenericDeleteClientCertificateRequest :: Generic DeleteClientCertificateRequest _
instance showDeleteClientCertificateRequest :: Show DeleteClientCertificateRequest where
  show = genericShow
instance decodeDeleteClientCertificateRequest :: Decode DeleteClientCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClientCertificateRequest :: Encode DeleteClientCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to delete a <a>Deployment</a> resource.</p>
newtype DeleteDeploymentRequest = DeleteDeploymentRequest 
  { "RestApiId'" :: (String)
  , "DeploymentId'" :: (String)
  }
derive instance newtypeDeleteDeploymentRequest :: Newtype DeleteDeploymentRequest _
derive instance repGenericDeleteDeploymentRequest :: Generic DeleteDeploymentRequest _
instance showDeleteDeploymentRequest :: Show DeleteDeploymentRequest where
  show = genericShow
instance decodeDeleteDeploymentRequest :: Decode DeleteDeploymentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDeploymentRequest :: Encode DeleteDeploymentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Deletes an existing documentation part of an API.</p>
newtype DeleteDocumentationPartRequest = DeleteDocumentationPartRequest 
  { "RestApiId'" :: (String)
  , "DocumentationPartId'" :: (String)
  }
derive instance newtypeDeleteDocumentationPartRequest :: Newtype DeleteDocumentationPartRequest _
derive instance repGenericDeleteDocumentationPartRequest :: Generic DeleteDocumentationPartRequest _
instance showDeleteDocumentationPartRequest :: Show DeleteDocumentationPartRequest where
  show = genericShow
instance decodeDeleteDocumentationPartRequest :: Decode DeleteDocumentationPartRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDocumentationPartRequest :: Encode DeleteDocumentationPartRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Deletes an existing documentation version of an API.</p>
newtype DeleteDocumentationVersionRequest = DeleteDocumentationVersionRequest 
  { "RestApiId'" :: (String)
  , "DocumentationVersion'" :: (String)
  }
derive instance newtypeDeleteDocumentationVersionRequest :: Newtype DeleteDocumentationVersionRequest _
derive instance repGenericDeleteDocumentationVersionRequest :: Generic DeleteDocumentationVersionRequest _
instance showDeleteDocumentationVersionRequest :: Show DeleteDocumentationVersionRequest where
  show = genericShow
instance decodeDeleteDocumentationVersionRequest :: Decode DeleteDocumentationVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDocumentationVersionRequest :: Encode DeleteDocumentationVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to delete the <a>DomainName</a> resource.</p>
newtype DeleteDomainNameRequest = DeleteDomainNameRequest 
  { "DomainName'" :: (String)
  }
derive instance newtypeDeleteDomainNameRequest :: Newtype DeleteDomainNameRequest _
derive instance repGenericDeleteDomainNameRequest :: Generic DeleteDomainNameRequest _
instance showDeleteDomainNameRequest :: Show DeleteDomainNameRequest where
  show = genericShow
instance decodeDeleteDomainNameRequest :: Decode DeleteDomainNameRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDomainNameRequest :: Encode DeleteDomainNameRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Clears any customization of a <a>GatewayResponse</a> of a specified response type on the given <a>RestApi</a> and resets it with the default settings.</p>
newtype DeleteGatewayResponseRequest = DeleteGatewayResponseRequest 
  { "RestApiId'" :: (String)
  , "ResponseType'" :: (GatewayResponseType)
  }
derive instance newtypeDeleteGatewayResponseRequest :: Newtype DeleteGatewayResponseRequest _
derive instance repGenericDeleteGatewayResponseRequest :: Generic DeleteGatewayResponseRequest _
instance showDeleteGatewayResponseRequest :: Show DeleteGatewayResponseRequest where
  show = genericShow
instance decodeDeleteGatewayResponseRequest :: Decode DeleteGatewayResponseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGatewayResponseRequest :: Encode DeleteGatewayResponseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a delete integration request.</p>
newtype DeleteIntegrationRequest = DeleteIntegrationRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  }
derive instance newtypeDeleteIntegrationRequest :: Newtype DeleteIntegrationRequest _
derive instance repGenericDeleteIntegrationRequest :: Generic DeleteIntegrationRequest _
instance showDeleteIntegrationRequest :: Show DeleteIntegrationRequest where
  show = genericShow
instance decodeDeleteIntegrationRequest :: Decode DeleteIntegrationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIntegrationRequest :: Encode DeleteIntegrationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a delete integration response request.</p>
newtype DeleteIntegrationResponseRequest = DeleteIntegrationResponseRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "StatusCode'" :: (StatusCode)
  }
derive instance newtypeDeleteIntegrationResponseRequest :: Newtype DeleteIntegrationResponseRequest _
derive instance repGenericDeleteIntegrationResponseRequest :: Generic DeleteIntegrationResponseRequest _
instance showDeleteIntegrationResponseRequest :: Show DeleteIntegrationResponseRequest where
  show = genericShow
instance decodeDeleteIntegrationResponseRequest :: Decode DeleteIntegrationResponseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIntegrationResponseRequest :: Encode DeleteIntegrationResponseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to delete an existing <a>Method</a> resource.</p>
newtype DeleteMethodRequest = DeleteMethodRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  }
derive instance newtypeDeleteMethodRequest :: Newtype DeleteMethodRequest _
derive instance repGenericDeleteMethodRequest :: Generic DeleteMethodRequest _
instance showDeleteMethodRequest :: Show DeleteMethodRequest where
  show = genericShow
instance decodeDeleteMethodRequest :: Decode DeleteMethodRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMethodRequest :: Encode DeleteMethodRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to delete an existing <a>MethodResponse</a> resource.</p>
newtype DeleteMethodResponseRequest = DeleteMethodResponseRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "StatusCode'" :: (StatusCode)
  }
derive instance newtypeDeleteMethodResponseRequest :: Newtype DeleteMethodResponseRequest _
derive instance repGenericDeleteMethodResponseRequest :: Generic DeleteMethodResponseRequest _
instance showDeleteMethodResponseRequest :: Show DeleteMethodResponseRequest where
  show = genericShow
instance decodeDeleteMethodResponseRequest :: Decode DeleteMethodResponseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMethodResponseRequest :: Encode DeleteMethodResponseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to delete an existing model in an existing <a>RestApi</a> resource.</p>
newtype DeleteModelRequest = DeleteModelRequest 
  { "RestApiId'" :: (String)
  , "ModelName'" :: (String)
  }
derive instance newtypeDeleteModelRequest :: Newtype DeleteModelRequest _
derive instance repGenericDeleteModelRequest :: Generic DeleteModelRequest _
instance showDeleteModelRequest :: Show DeleteModelRequest where
  show = genericShow
instance decodeDeleteModelRequest :: Decode DeleteModelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteModelRequest :: Encode DeleteModelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Deletes a specified <a>RequestValidator</a> of a given <a>RestApi</a>.</p>
newtype DeleteRequestValidatorRequest = DeleteRequestValidatorRequest 
  { "RestApiId'" :: (String)
  , "RequestValidatorId'" :: (String)
  }
derive instance newtypeDeleteRequestValidatorRequest :: Newtype DeleteRequestValidatorRequest _
derive instance repGenericDeleteRequestValidatorRequest :: Generic DeleteRequestValidatorRequest _
instance showDeleteRequestValidatorRequest :: Show DeleteRequestValidatorRequest where
  show = genericShow
instance decodeDeleteRequestValidatorRequest :: Decode DeleteRequestValidatorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRequestValidatorRequest :: Encode DeleteRequestValidatorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to delete a <a>Resource</a>.</p>
newtype DeleteResourceRequest = DeleteResourceRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  }
derive instance newtypeDeleteResourceRequest :: Newtype DeleteResourceRequest _
derive instance repGenericDeleteResourceRequest :: Generic DeleteResourceRequest _
instance showDeleteResourceRequest :: Show DeleteResourceRequest where
  show = genericShow
instance decodeDeleteResourceRequest :: Decode DeleteResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteResourceRequest :: Encode DeleteResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to delete the specified API from your collection.</p>
newtype DeleteRestApiRequest = DeleteRestApiRequest 
  { "RestApiId'" :: (String)
  }
derive instance newtypeDeleteRestApiRequest :: Newtype DeleteRestApiRequest _
derive instance repGenericDeleteRestApiRequest :: Generic DeleteRestApiRequest _
instance showDeleteRestApiRequest :: Show DeleteRestApiRequest where
  show = genericShow
instance decodeDeleteRestApiRequest :: Decode DeleteRestApiRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRestApiRequest :: Encode DeleteRestApiRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to delete a <a>Stage</a> resource.</p>
newtype DeleteStageRequest = DeleteStageRequest 
  { "RestApiId'" :: (String)
  , "StageName'" :: (String)
  }
derive instance newtypeDeleteStageRequest :: Newtype DeleteStageRequest _
derive instance repGenericDeleteStageRequest :: Generic DeleteStageRequest _
instance showDeleteStageRequest :: Show DeleteStageRequest where
  show = genericShow
instance decodeDeleteStageRequest :: Decode DeleteStageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStageRequest :: Encode DeleteStageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The DELETE request to delete a usage plan key and remove the underlying API key from the associated usage plan.</p>
newtype DeleteUsagePlanKeyRequest = DeleteUsagePlanKeyRequest 
  { "UsagePlanId'" :: (String)
  , "KeyId'" :: (String)
  }
derive instance newtypeDeleteUsagePlanKeyRequest :: Newtype DeleteUsagePlanKeyRequest _
derive instance repGenericDeleteUsagePlanKeyRequest :: Generic DeleteUsagePlanKeyRequest _
instance showDeleteUsagePlanKeyRequest :: Show DeleteUsagePlanKeyRequest where
  show = genericShow
instance decodeDeleteUsagePlanKeyRequest :: Decode DeleteUsagePlanKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUsagePlanKeyRequest :: Encode DeleteUsagePlanKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The DELETE request to delete a usage plan of a given plan Id.</p>
newtype DeleteUsagePlanRequest = DeleteUsagePlanRequest 
  { "UsagePlanId'" :: (String)
  }
derive instance newtypeDeleteUsagePlanRequest :: Newtype DeleteUsagePlanRequest _
derive instance repGenericDeleteUsagePlanRequest :: Generic DeleteUsagePlanRequest _
instance showDeleteUsagePlanRequest :: Show DeleteUsagePlanRequest where
  show = genericShow
instance decodeDeleteUsagePlanRequest :: Decode DeleteUsagePlanRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUsagePlanRequest :: Encode DeleteUsagePlanRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Deletes an existing <a>VpcLink</a> of a specified identifier.</p>
newtype DeleteVpcLinkRequest = DeleteVpcLinkRequest 
  { "VpcLinkId'" :: (String)
  }
derive instance newtypeDeleteVpcLinkRequest :: Newtype DeleteVpcLinkRequest _
derive instance repGenericDeleteVpcLinkRequest :: Generic DeleteVpcLinkRequest _
instance showDeleteVpcLinkRequest :: Show DeleteVpcLinkRequest where
  show = genericShow
instance decodeDeleteVpcLinkRequest :: Decode DeleteVpcLinkRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVpcLinkRequest :: Encode DeleteVpcLinkRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An immutable representation of a <a>RestApi</a> resource that can be called by users using <a>Stages</a>. A deployment must be associated with a <a>Stage</a> for it to be callable over the Internet.</p> <div class="remarks">To create a deployment, call <code>POST</code> on the <a>Deployments</a> resource of a <a>RestApi</a>. To view, update, or delete a deployment, call <code>GET</code>, <code>PATCH</code>, or <code>DELETE</code> on the specified deployment resource (<code>/restapis/{restapi_id}/deployments/{deployment_id}</code>).</div> <div class="seeAlso"><a>RestApi</a>, <a>Deployments</a>, <a>Stage</a>, <a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html">AWS CLI</a>, <a href="https://aws.amazon.com/tools/">AWS SDKs</a> </div>
newtype Deployment = Deployment 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "ApiSummary'" :: NullOrUndefined.NullOrUndefined (PathToMapOfMethodSnapshot)
  }
derive instance newtypeDeployment :: Newtype Deployment _
derive instance repGenericDeployment :: Generic Deployment _
instance showDeployment :: Show Deployment where
  show = genericShow
instance decodeDeployment :: Decode Deployment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeployment :: Encode Deployment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input configuration for a canary deployment.</p>
newtype DeploymentCanarySettings = DeploymentCanarySettings 
  { "PercentTraffic'" :: NullOrUndefined.NullOrUndefined (Number)
  , "StageVariableOverrides'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "UseStageCache'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeDeploymentCanarySettings :: Newtype DeploymentCanarySettings _
derive instance repGenericDeploymentCanarySettings :: Generic DeploymentCanarySettings _
instance showDeploymentCanarySettings :: Show DeploymentCanarySettings where
  show = genericShow
instance decodeDeploymentCanarySettings :: Decode DeploymentCanarySettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeploymentCanarySettings :: Encode DeploymentCanarySettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection resource that contains zero or more references to your existing deployments, and links that guide you on how to interact with your collection. The collection offers a paginated view of the contained deployments.</p> <div class="remarks">To create a new deployment of a <a>RestApi</a>, make a <code>POST</code> request against this resource. To view, update, or delete an existing deployment, make a <code>GET</code>, <code>PATCH</code>, or <code>DELETE</code> request, respectively, on a specified <a>Deployment</a> resource.</div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html">Deploying an API</a>, <a href="http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html">AWS CLI</a>, <a href="https://aws.amazon.com/tools/">AWS SDKs</a> </div>
newtype Deployments = Deployments 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfDeployment)
  }
derive instance newtypeDeployments :: Newtype Deployments _
derive instance repGenericDeployments :: Generic Deployments _
instance showDeployments :: Show Deployments where
  show = genericShow
instance decodeDeployments :: Decode Deployments where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeployments :: Encode Deployments where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A documentation part for a targeted API entity.</p> <div class="remarks"> <p>A documentation part consists of a content map (<code>properties</code>) and a target (<code>location</code>). The target specifies an API entity to which the documentation content applies. The supported API entity types are <code>API</code>, <code>AUTHORIZER</code>, <code>MODEL</code>, <code>RESOURCE</code>, <code>METHOD</code>, <code>PATH_PARAMETER</code>, <code>QUERY_PARAMETER</code>, <code>REQUEST_HEADER</code>, <code>REQUEST_BODY</code>, <code>RESPONSE</code>, <code>RESPONSE_HEADER</code>, and <code>RESPONSE_BODY</code>. Valid <code>location</code> fields depend on the API entity type. All valid fields are not required.</p> <p>The content map is a JSON string of API-specific key-value pairs. Although an API can use any shape for the content map, only the Swagger-compliant documentation fields will be injected into the associated API entity definition in the exported Swagger definition file.</p></div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html">Documenting an API</a>, <a>DocumentationParts</a> </div>
newtype DocumentationPart = DocumentationPart 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Location'" :: NullOrUndefined.NullOrUndefined (DocumentationPartLocation)
  , "Properties'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDocumentationPart :: Newtype DocumentationPart _
derive instance repGenericDocumentationPart :: Generic DocumentationPart _
instance showDocumentationPart :: Show DocumentationPart where
  show = genericShow
instance decodeDocumentationPart :: Decode DocumentationPart where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentationPart :: Encode DocumentationPart where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A collection of the imported <a>DocumentationPart</a> identifiers.</p> <div class="remarks">This is used to return the result when documentation parts in an external (e.g., Swagger) file are imported into API Gateway</div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html">Documenting an API</a>, <a href="http://docs.aws.amazon.com/apigateway/api-reference/link-relation/documentationpart-import/">documentationpart:import</a>, <a>DocumentationPart</a> </div>
newtype DocumentationPartIds = DocumentationPartIds 
  { "Ids'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  , "Warnings'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  }
derive instance newtypeDocumentationPartIds :: Newtype DocumentationPartIds _
derive instance repGenericDocumentationPartIds :: Generic DocumentationPartIds _
instance showDocumentationPartIds :: Show DocumentationPartIds where
  show = genericShow
instance decodeDocumentationPartIds :: Decode DocumentationPartIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentationPartIds :: Encode DocumentationPartIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the target API entity to which the documentation applies.</p>
newtype DocumentationPartLocation = DocumentationPartLocation 
  { "Type'" :: (DocumentationPartType)
  , "Path'" :: NullOrUndefined.NullOrUndefined (String)
  , "Method'" :: NullOrUndefined.NullOrUndefined (String)
  , "StatusCode'" :: NullOrUndefined.NullOrUndefined (DocumentationPartLocationStatusCode)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDocumentationPartLocation :: Newtype DocumentationPartLocation _
derive instance repGenericDocumentationPartLocation :: Generic DocumentationPartLocation _
instance showDocumentationPartLocation :: Show DocumentationPartLocation where
  show = genericShow
instance decodeDocumentationPartLocation :: Decode DocumentationPartLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentationPartLocation :: Encode DocumentationPartLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentationPartLocationStatusCode = DocumentationPartLocationStatusCode String
derive instance newtypeDocumentationPartLocationStatusCode :: Newtype DocumentationPartLocationStatusCode _
derive instance repGenericDocumentationPartLocationStatusCode :: Generic DocumentationPartLocationStatusCode _
instance showDocumentationPartLocationStatusCode :: Show DocumentationPartLocationStatusCode where
  show = genericShow
instance decodeDocumentationPartLocationStatusCode :: Decode DocumentationPartLocationStatusCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentationPartLocationStatusCode :: Encode DocumentationPartLocationStatusCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentationPartType = DocumentationPartType String
derive instance newtypeDocumentationPartType :: Newtype DocumentationPartType _
derive instance repGenericDocumentationPartType :: Generic DocumentationPartType _
instance showDocumentationPartType :: Show DocumentationPartType where
  show = genericShow
instance decodeDocumentationPartType :: Decode DocumentationPartType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentationPartType :: Encode DocumentationPartType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The collection of documentation parts of an API.</p> <div class="remarks"/> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html">Documenting an API</a>, <a>DocumentationPart</a> </div>
newtype DocumentationParts = DocumentationParts 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfDocumentationPart)
  }
derive instance newtypeDocumentationParts :: Newtype DocumentationParts _
derive instance repGenericDocumentationParts :: Generic DocumentationParts _
instance showDocumentationParts :: Show DocumentationParts where
  show = genericShow
instance decodeDocumentationParts :: Decode DocumentationParts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentationParts :: Encode DocumentationParts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A snapshot of the documentation of an API.</p> <div class="remarks"><p>Publishing API documentation involves creating a documentation version associated with an API stage and exporting the versioned documentation to an external (e.g., Swagger) file.</p></div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html">Documenting an API</a>, <a>DocumentationPart</a>, <a>DocumentationVersions</a> </div>
newtype DocumentationVersion = DocumentationVersion 
  { "Version'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDocumentationVersion :: Newtype DocumentationVersion _
derive instance repGenericDocumentationVersion :: Generic DocumentationVersion _
instance showDocumentationVersion :: Show DocumentationVersion where
  show = genericShow
instance decodeDocumentationVersion :: Decode DocumentationVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentationVersion :: Encode DocumentationVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The collection of documentation snapshots of an API. </p> <div class="remarks"><p>Use the <a>DocumentationVersions</a> to manage documentation snapshots associated with various API stages.</p></div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html">Documenting an API</a>, <a>DocumentationPart</a>, <a>DocumentationVersion</a> </div>
newtype DocumentationVersions = DocumentationVersions 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfDocumentationVersion)
  }
derive instance newtypeDocumentationVersions :: Newtype DocumentationVersions _
derive instance repGenericDocumentationVersions :: Generic DocumentationVersions _
instance showDocumentationVersions :: Show DocumentationVersions where
  show = genericShow
instance decodeDocumentationVersions :: Decode DocumentationVersions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentationVersions :: Encode DocumentationVersions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a custom domain name as a user-friendly host name of an API (<a>RestApi</a>).</p> <div class="Remarks"> <p>When you deploy an API, API Gateway creates a default host name for the API. This default API host name is of the <code>{restapi-id}.execute-api.{region}.amazonaws.com</code> format. With the default host name, you can access the API's root resource with the URL of <code>https://{restapi-id}.execute-api.{region}.amazonaws.com/{stage}/</code>. When you set up a custom domain name of <code>apis.example.com</code> for this API, you can then access the same resource using the URL of the <code>https://apis.examples.com/myApi</code>, where <code>myApi</code> is the base path mapping (<a>BasePathMapping</a>) of your API under the custom domain name. </p> </div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html">Set a Custom Host Name for an API</a> </div>
newtype DomainName = DomainName 
  { "DomainName'" :: NullOrUndefined.NullOrUndefined (String)
  , "CertificateName'" :: NullOrUndefined.NullOrUndefined (String)
  , "CertificateArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "CertificateUploadDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "RegionalDomainName'" :: NullOrUndefined.NullOrUndefined (String)
  , "RegionalHostedZoneId'" :: NullOrUndefined.NullOrUndefined (String)
  , "RegionalCertificateName'" :: NullOrUndefined.NullOrUndefined (String)
  , "RegionalCertificateArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "DistributionDomainName'" :: NullOrUndefined.NullOrUndefined (String)
  , "DistributionHostedZoneId'" :: NullOrUndefined.NullOrUndefined (String)
  , "EndpointConfiguration'" :: NullOrUndefined.NullOrUndefined (EndpointConfiguration)
  }
derive instance newtypeDomainName :: Newtype DomainName _
derive instance repGenericDomainName :: Generic DomainName _
instance showDomainName :: Show DomainName where
  show = genericShow
instance decodeDomainName :: Decode DomainName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainName :: Encode DomainName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection of <a>DomainName</a> resources.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html">Use Client-Side Certificate</a> </div>
newtype DomainNames = DomainNames 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfDomainName)
  }
derive instance newtypeDomainNames :: Newtype DomainNames _
derive instance repGenericDomainNames :: Generic DomainNames _
instance showDomainNames :: Show DomainNames where
  show = genericShow
instance decodeDomainNames :: Decode DomainNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainNames :: Encode DomainNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The endpoint configuration to indicate the types of endpoints an API (<a>RestApi</a>) or its custom domain name (<a>DomainName</a>) has. </p>
newtype EndpointConfiguration = EndpointConfiguration 
  { "Types'" :: NullOrUndefined.NullOrUndefined (ListOfEndpointType)
  }
derive instance newtypeEndpointConfiguration :: Newtype EndpointConfiguration _
derive instance repGenericEndpointConfiguration :: Generic EndpointConfiguration _
instance showEndpointConfiguration :: Show EndpointConfiguration where
  show = genericShow
instance decodeEndpointConfiguration :: Decode EndpointConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointConfiguration :: Encode EndpointConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The endpoint type. The valid value is <code>EDGE</code> for edge-optimized API setup, most suitable for mobile applications, <code>REGIONAL</code> for regional API endpoint setup, most suitable for calling from AWS Region</p>
newtype EndpointType = EndpointType String
derive instance newtypeEndpointType :: Newtype EndpointType _
derive instance repGenericEndpointType :: Generic EndpointType _
instance showEndpointType :: Show EndpointType where
  show = genericShow
instance decodeEndpointType :: Decode EndpointType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointType :: Encode EndpointType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The binary blob response to <a>GetExport</a>, which contains the generated SDK.</p>
newtype ExportResponse = ExportResponse 
  { "ContentType'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContentDisposition'" :: NullOrUndefined.NullOrUndefined (String)
  , "Body'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeExportResponse :: Newtype ExportResponse _
derive instance repGenericExportResponse :: Generic ExportResponse _
instance showExportResponse :: Show ExportResponse where
  show = genericShow
instance decodeExportResponse :: Decode ExportResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportResponse :: Encode ExportResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to flush authorizer cache entries on a specified stage.</p>
newtype FlushStageAuthorizersCacheRequest = FlushStageAuthorizersCacheRequest 
  { "RestApiId'" :: (String)
  , "StageName'" :: (String)
  }
derive instance newtypeFlushStageAuthorizersCacheRequest :: Newtype FlushStageAuthorizersCacheRequest _
derive instance repGenericFlushStageAuthorizersCacheRequest :: Generic FlushStageAuthorizersCacheRequest _
instance showFlushStageAuthorizersCacheRequest :: Show FlushStageAuthorizersCacheRequest where
  show = genericShow
instance decodeFlushStageAuthorizersCacheRequest :: Decode FlushStageAuthorizersCacheRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFlushStageAuthorizersCacheRequest :: Encode FlushStageAuthorizersCacheRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to flush a stage's cache.</p>
newtype FlushStageCacheRequest = FlushStageCacheRequest 
  { "RestApiId'" :: (String)
  , "StageName'" :: (String)
  }
derive instance newtypeFlushStageCacheRequest :: Newtype FlushStageCacheRequest _
derive instance repGenericFlushStageCacheRequest :: Generic FlushStageCacheRequest _
instance showFlushStageCacheRequest :: Show FlushStageCacheRequest where
  show = genericShow
instance decodeFlushStageCacheRequest :: Decode FlushStageCacheRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFlushStageCacheRequest :: Encode FlushStageCacheRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A gateway response of a given response type and status code, with optional response parameters and mapping templates.</p> <div class="remarks"> For more information about valid gateway response types, see <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/supported-gateway-response-types.html">Gateway Response Types Supported by API Gateway</a> <div class="example"> <h4>Example: Get a Gateway Response of a given response type</h4> <h5>Request</h5> <p>This example shows how to get a gateway response of the <code>MISSING_AUTHENTICATION_TOKEN</code> type.</p> <pre><code>GET /restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN HTTP/1.1 Host: beta-apigateway.us-east-1.amazonaws.com Content-Type: application/json X-Amz-Date: 20170503T202516Z Authorization: AWS4-HMAC-SHA256 Credential={access-key-id}/20170503/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature=1b52460e3159c1a26cff29093855d50ea141c1c5b937528fecaf60f51129697a Cache-Control: no-cache Postman-Token: 3b2a1ce9-c848-2e26-2e2f-9c2caefbed45 </code></pre> <p>The response type is specified as a URL path.</p> <h5>Response</h5> <p>The successful operation returns the <code>200 OK</code> status code and a payload similar to the following:</p> <pre><code>{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-gatewayresponse-{rel}.html", "name": "gatewayresponse", "templated": true }, "self": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" }, "gatewayresponse:delete": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" } }, "defaultResponse": false, "responseParameters": { "gatewayresponse.header.x-request-path": "method.request.path.petId", "gatewayresponse.header.Access-Control-Allow-Origin": "&apos;a.b.c&apos;", "gatewayresponse.header.x-request-query": "method.request.querystring.q", "gatewayresponse.header.x-request-header": "method.request.header.Accept" }, "responseTemplates": { "application/json": "{\n \"message\": $context.error.messageString,\n \"type\": \"$context.error.responseType\",\n \"stage\": \"$context.stage\",\n \"resourcePath\": \"$context.resourcePath\",\n \"stageVariables.a\": \"$stageVariables.a\",\n \"statusCode\": \"&apos;404&apos;\"\n}" }, "responseType": "MISSING_AUTHENTICATION_TOKEN", "statusCode": "404" }</code></pre> <p></p> </div> </div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/customize-gateway-responses.html">Customize Gateway Responses</a> </div>
newtype GatewayResponse = GatewayResponse 
  { "ResponseType'" :: NullOrUndefined.NullOrUndefined (GatewayResponseType)
  , "StatusCode'" :: NullOrUndefined.NullOrUndefined (StatusCode)
  , "ResponseParameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "ResponseTemplates'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "DefaultResponse'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeGatewayResponse :: Newtype GatewayResponse _
derive instance repGenericGatewayResponse :: Generic GatewayResponse _
instance showGatewayResponse :: Show GatewayResponse where
  show = genericShow
instance decodeGatewayResponse :: Decode GatewayResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayResponse :: Encode GatewayResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GatewayResponseType = GatewayResponseType String
derive instance newtypeGatewayResponseType :: Newtype GatewayResponseType _
derive instance repGenericGatewayResponseType :: Generic GatewayResponseType _
instance showGatewayResponseType :: Show GatewayResponseType where
  show = genericShow
instance decodeGatewayResponseType :: Decode GatewayResponseType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayResponseType :: Encode GatewayResponseType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The collection of the <a>GatewayResponse</a> instances of a <a>RestApi</a> as a <code>responseType</code>-to-<a>GatewayResponse</a> object map of key-value pairs. As such, pagination is not supported for querying this collection.</p> <div class="remarks"> For more information about valid gateway response types, see <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/supported-gateway-response-types.html">Gateway Response Types Supported by API Gateway</a> <div class="example"> <h4>Example: Get the collection of gateway responses of an API</h4> <h5>Request</h5> <p>This example request shows how to retrieve the <a>GatewayResponses</a> collection from an API.</p> <pre><code>GET /restapis/o81lxisefl/gatewayresponses HTTP/1.1 Host: beta-apigateway.us-east-1.amazonaws.com Content-Type: application/json X-Amz-Date: 20170503T220604Z Authorization: AWS4-HMAC-SHA256 Credential={access-key-id}/20170503/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature=59b42fe54a76a5de8adf2c67baa6d39206f8e9ad49a1d77ccc6a5da3103a398a Cache-Control: no-cache Postman-Token: 5637af27-dc29-fc5c-9dfe-0645d52cb515 </code></pre> <p></p> <h5>Response</h5> <p>The successful operation returns the <code>200 OK</code> status code and a payload similar to the following:</p> <pre><code>{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-gatewayresponse-{rel}.html", "name": "gatewayresponse", "templated": true }, "self": { "href": "/restapis/o81lxisefl/gatewayresponses" }, "first": { "href": "/restapis/o81lxisefl/gatewayresponses" }, "gatewayresponse:by-type": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "item": [ { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_FAILURE" }, { "href": "/restapis/o81lxisefl/gatewayresponses/RESOURCE_NOT_FOUND" }, { "href": "/restapis/o81lxisefl/gatewayresponses/REQUEST_TOO_LARGE" }, { "href": "/restapis/o81lxisefl/gatewayresponses/THROTTLED" }, { "href": "/restapis/o81lxisefl/gatewayresponses/UNSUPPORTED_MEDIA_TYPE" }, { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_CONFIGURATION_ERROR" }, { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_5XX" }, { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_4XX" }, { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_PARAMETERS" }, { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_BODY" }, { "href": "/restapis/o81lxisefl/gatewayresponses/EXPIRED_TOKEN" }, { "href": "/restapis/o81lxisefl/gatewayresponses/ACCESS_DENIED" }, { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_API_KEY" }, { "href": "/restapis/o81lxisefl/gatewayresponses/UNAUTHORIZED" }, { "href": "/restapis/o81lxisefl/gatewayresponses/API_CONFIGURATION_ERROR" }, { "href": "/restapis/o81lxisefl/gatewayresponses/QUOTA_EXCEEDED" }, { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_TIMEOUT" }, { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" }, { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_SIGNATURE" }, { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_FAILURE" } ] }, "_embedded": { "item": [ { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_FAILURE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_FAILURE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "INTEGRATION_FAILURE", "statusCode": "504" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/RESOURCE_NOT_FOUND" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/RESOURCE_NOT_FOUND" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "RESOURCE_NOT_FOUND", "statusCode": "404" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/REQUEST_TOO_LARGE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/REQUEST_TOO_LARGE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "REQUEST_TOO_LARGE", "statusCode": "413" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/THROTTLED" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/THROTTLED" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "THROTTLED", "statusCode": "429" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/UNSUPPORTED_MEDIA_TYPE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/UNSUPPORTED_MEDIA_TYPE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "UNSUPPORTED_MEDIA_TYPE", "statusCode": "415" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_CONFIGURATION_ERROR" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_CONFIGURATION_ERROR" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "AUTHORIZER_CONFIGURATION_ERROR", "statusCode": "500" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_5XX" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_5XX" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "DEFAULT_5XX" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_4XX" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/DEFAULT_4XX" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "DEFAULT_4XX" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_PARAMETERS" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_PARAMETERS" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "BAD_REQUEST_PARAMETERS", "statusCode": "400" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_BODY" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/BAD_REQUEST_BODY" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "BAD_REQUEST_BODY", "statusCode": "400" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/EXPIRED_TOKEN" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/EXPIRED_TOKEN" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "EXPIRED_TOKEN", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/ACCESS_DENIED" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/ACCESS_DENIED" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "ACCESS_DENIED", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_API_KEY" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_API_KEY" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "INVALID_API_KEY", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/UNAUTHORIZED" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/UNAUTHORIZED" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "UNAUTHORIZED", "statusCode": "401" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/API_CONFIGURATION_ERROR" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/API_CONFIGURATION_ERROR" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "API_CONFIGURATION_ERROR", "statusCode": "500" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/QUOTA_EXCEEDED" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/QUOTA_EXCEEDED" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "QUOTA_EXCEEDED", "statusCode": "429" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_TIMEOUT" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/INTEGRATION_TIMEOUT" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "INTEGRATION_TIMEOUT", "statusCode": "504" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "MISSING_AUTHENTICATION_TOKEN", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_SIGNATURE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/INVALID_SIGNATURE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "INVALID_SIGNATURE", "statusCode": "403" }, { "_links": { "self": { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_FAILURE" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/AUTHORIZER_FAILURE" } }, "defaultResponse": true, "responseParameters": {}, "responseTemplates": { "application/json": "{\"message\":$context.error.messageString}" }, "responseType": "AUTHORIZER_FAILURE", "statusCode": "500" } ] } }</code></pre> <p></p> </div> </div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/customize-gateway-responses.html">Customize Gateway Responses</a> </div>
newtype GatewayResponses = GatewayResponses 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfGatewayResponse)
  }
derive instance newtypeGatewayResponses :: Newtype GatewayResponses _
derive instance repGenericGatewayResponses :: Generic GatewayResponses _
instance showGatewayResponses :: Show GatewayResponses where
  show = genericShow
instance decodeGatewayResponses :: Decode GatewayResponses where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayResponses :: Encode GatewayResponses where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to generate a <a>ClientCertificate</a> resource.</p>
newtype GenerateClientCertificateRequest = GenerateClientCertificateRequest 
  { "Description'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGenerateClientCertificateRequest :: Newtype GenerateClientCertificateRequest _
derive instance repGenericGenerateClientCertificateRequest :: Generic GenerateClientCertificateRequest _
instance showGenerateClientCertificateRequest :: Show GenerateClientCertificateRequest where
  show = genericShow
instance decodeGenerateClientCertificateRequest :: Decode GenerateClientCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenerateClientCertificateRequest :: Encode GenerateClientCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to get information about the current <a>Account</a> resource.</p>
newtype GetAccountRequest = GetAccountRequest Types.NoArguments
derive instance newtypeGetAccountRequest :: Newtype GetAccountRequest _
derive instance repGenericGetAccountRequest :: Generic GetAccountRequest _
instance showGetAccountRequest :: Show GetAccountRequest where
  show = genericShow
instance decodeGetAccountRequest :: Decode GetAccountRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAccountRequest :: Encode GetAccountRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get information about the current <a>ApiKey</a> resource.</p>
newtype GetApiKeyRequest = GetApiKeyRequest 
  { "ApiKey'" :: (String)
  , "IncludeValue'" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  }
derive instance newtypeGetApiKeyRequest :: Newtype GetApiKeyRequest _
derive instance repGenericGetApiKeyRequest :: Generic GetApiKeyRequest _
instance showGetApiKeyRequest :: Show GetApiKeyRequest where
  show = genericShow
instance decodeGetApiKeyRequest :: Decode GetApiKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApiKeyRequest :: Encode GetApiKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get information about the current <a>ApiKeys</a> resource.</p>
newtype GetApiKeysRequest = GetApiKeysRequest 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "NameQuery'" :: NullOrUndefined.NullOrUndefined (String)
  , "CustomerId'" :: NullOrUndefined.NullOrUndefined (String)
  , "IncludeValues'" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  }
derive instance newtypeGetApiKeysRequest :: Newtype GetApiKeysRequest _
derive instance repGenericGetApiKeysRequest :: Generic GetApiKeysRequest _
instance showGetApiKeysRequest :: Show GetApiKeysRequest where
  show = genericShow
instance decodeGetApiKeysRequest :: Decode GetApiKeysRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApiKeysRequest :: Encode GetApiKeysRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to describe an existing <a>Authorizer</a> resource.</p>
newtype GetAuthorizerRequest = GetAuthorizerRequest 
  { "RestApiId'" :: (String)
  , "AuthorizerId'" :: (String)
  }
derive instance newtypeGetAuthorizerRequest :: Newtype GetAuthorizerRequest _
derive instance repGenericGetAuthorizerRequest :: Generic GetAuthorizerRequest _
instance showGetAuthorizerRequest :: Show GetAuthorizerRequest where
  show = genericShow
instance decodeGetAuthorizerRequest :: Decode GetAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAuthorizerRequest :: Encode GetAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to describe an existing <a>Authorizers</a> resource.</p>
newtype GetAuthorizersRequest = GetAuthorizersRequest 
  { "RestApiId'" :: (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetAuthorizersRequest :: Newtype GetAuthorizersRequest _
derive instance repGenericGetAuthorizersRequest :: Generic GetAuthorizersRequest _
instance showGetAuthorizersRequest :: Show GetAuthorizersRequest where
  show = genericShow
instance decodeGetAuthorizersRequest :: Decode GetAuthorizersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAuthorizersRequest :: Encode GetAuthorizersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to describe a <a>BasePathMapping</a> resource.</p>
newtype GetBasePathMappingRequest = GetBasePathMappingRequest 
  { "DomainName'" :: (String)
  , "BasePath'" :: (String)
  }
derive instance newtypeGetBasePathMappingRequest :: Newtype GetBasePathMappingRequest _
derive instance repGenericGetBasePathMappingRequest :: Generic GetBasePathMappingRequest _
instance showGetBasePathMappingRequest :: Show GetBasePathMappingRequest where
  show = genericShow
instance decodeGetBasePathMappingRequest :: Decode GetBasePathMappingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBasePathMappingRequest :: Encode GetBasePathMappingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get information about a collection of <a>BasePathMapping</a> resources.</p>
newtype GetBasePathMappingsRequest = GetBasePathMappingsRequest 
  { "DomainName'" :: (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetBasePathMappingsRequest :: Newtype GetBasePathMappingsRequest _
derive instance repGenericGetBasePathMappingsRequest :: Generic GetBasePathMappingsRequest _
instance showGetBasePathMappingsRequest :: Show GetBasePathMappingsRequest where
  show = genericShow
instance decodeGetBasePathMappingsRequest :: Decode GetBasePathMappingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBasePathMappingsRequest :: Encode GetBasePathMappingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get information about the current <a>ClientCertificate</a> resource.</p>
newtype GetClientCertificateRequest = GetClientCertificateRequest 
  { "ClientCertificateId'" :: (String)
  }
derive instance newtypeGetClientCertificateRequest :: Newtype GetClientCertificateRequest _
derive instance repGenericGetClientCertificateRequest :: Generic GetClientCertificateRequest _
instance showGetClientCertificateRequest :: Show GetClientCertificateRequest where
  show = genericShow
instance decodeGetClientCertificateRequest :: Decode GetClientCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetClientCertificateRequest :: Encode GetClientCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get information about a collection of <a>ClientCertificate</a> resources.</p>
newtype GetClientCertificatesRequest = GetClientCertificatesRequest 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetClientCertificatesRequest :: Newtype GetClientCertificatesRequest _
derive instance repGenericGetClientCertificatesRequest :: Generic GetClientCertificatesRequest _
instance showGetClientCertificatesRequest :: Show GetClientCertificatesRequest where
  show = genericShow
instance decodeGetClientCertificatesRequest :: Decode GetClientCertificatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetClientCertificatesRequest :: Encode GetClientCertificatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to get information about a <a>Deployment</a> resource.</p>
newtype GetDeploymentRequest = GetDeploymentRequest 
  { "RestApiId'" :: (String)
  , "DeploymentId'" :: (String)
  , "Embed'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  }
derive instance newtypeGetDeploymentRequest :: Newtype GetDeploymentRequest _
derive instance repGenericGetDeploymentRequest :: Generic GetDeploymentRequest _
instance showGetDeploymentRequest :: Show GetDeploymentRequest where
  show = genericShow
instance decodeGetDeploymentRequest :: Decode GetDeploymentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeploymentRequest :: Encode GetDeploymentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to get information about a <a>Deployments</a> collection.</p>
newtype GetDeploymentsRequest = GetDeploymentsRequest 
  { "RestApiId'" :: (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetDeploymentsRequest :: Newtype GetDeploymentsRequest _
derive instance repGenericGetDeploymentsRequest :: Generic GetDeploymentsRequest _
instance showGetDeploymentsRequest :: Show GetDeploymentsRequest where
  show = genericShow
instance decodeGetDeploymentsRequest :: Decode GetDeploymentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeploymentsRequest :: Encode GetDeploymentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets a specified documentation part of a given API.</p>
newtype GetDocumentationPartRequest = GetDocumentationPartRequest 
  { "RestApiId'" :: (String)
  , "DocumentationPartId'" :: (String)
  }
derive instance newtypeGetDocumentationPartRequest :: Newtype GetDocumentationPartRequest _
derive instance repGenericGetDocumentationPartRequest :: Generic GetDocumentationPartRequest _
instance showGetDocumentationPartRequest :: Show GetDocumentationPartRequest where
  show = genericShow
instance decodeGetDocumentationPartRequest :: Decode GetDocumentationPartRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDocumentationPartRequest :: Encode GetDocumentationPartRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets the documentation parts of an API. The result may be filtered by the type, name, or path of API entities (targets).</p>
newtype GetDocumentationPartsRequest = GetDocumentationPartsRequest 
  { "RestApiId'" :: (String)
  , "Type'" :: NullOrUndefined.NullOrUndefined (DocumentationPartType)
  , "NameQuery'" :: NullOrUndefined.NullOrUndefined (String)
  , "Path'" :: NullOrUndefined.NullOrUndefined (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "LocationStatus'" :: NullOrUndefined.NullOrUndefined (LocationStatusType)
  }
derive instance newtypeGetDocumentationPartsRequest :: Newtype GetDocumentationPartsRequest _
derive instance repGenericGetDocumentationPartsRequest :: Generic GetDocumentationPartsRequest _
instance showGetDocumentationPartsRequest :: Show GetDocumentationPartsRequest where
  show = genericShow
instance decodeGetDocumentationPartsRequest :: Decode GetDocumentationPartsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDocumentationPartsRequest :: Encode GetDocumentationPartsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets a documentation snapshot of an API.</p>
newtype GetDocumentationVersionRequest = GetDocumentationVersionRequest 
  { "RestApiId'" :: (String)
  , "DocumentationVersion'" :: (String)
  }
derive instance newtypeGetDocumentationVersionRequest :: Newtype GetDocumentationVersionRequest _
derive instance repGenericGetDocumentationVersionRequest :: Generic GetDocumentationVersionRequest _
instance showGetDocumentationVersionRequest :: Show GetDocumentationVersionRequest where
  show = genericShow
instance decodeGetDocumentationVersionRequest :: Decode GetDocumentationVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDocumentationVersionRequest :: Encode GetDocumentationVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets the documentation versions of an API.</p>
newtype GetDocumentationVersionsRequest = GetDocumentationVersionsRequest 
  { "RestApiId'" :: (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetDocumentationVersionsRequest :: Newtype GetDocumentationVersionsRequest _
derive instance repGenericGetDocumentationVersionsRequest :: Generic GetDocumentationVersionsRequest _
instance showGetDocumentationVersionsRequest :: Show GetDocumentationVersionsRequest where
  show = genericShow
instance decodeGetDocumentationVersionsRequest :: Decode GetDocumentationVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDocumentationVersionsRequest :: Encode GetDocumentationVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to get the name of a <a>DomainName</a> resource.</p>
newtype GetDomainNameRequest = GetDomainNameRequest 
  { "DomainName'" :: (String)
  }
derive instance newtypeGetDomainNameRequest :: Newtype GetDomainNameRequest _
derive instance repGenericGetDomainNameRequest :: Generic GetDomainNameRequest _
instance showGetDomainNameRequest :: Show GetDomainNameRequest where
  show = genericShow
instance decodeGetDomainNameRequest :: Decode GetDomainNameRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDomainNameRequest :: Encode GetDomainNameRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to describe a collection of <a>DomainName</a> resources.</p>
newtype GetDomainNamesRequest = GetDomainNamesRequest 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetDomainNamesRequest :: Newtype GetDomainNamesRequest _
derive instance repGenericGetDomainNamesRequest :: Generic GetDomainNamesRequest _
instance showGetDomainNamesRequest :: Show GetDomainNamesRequest where
  show = genericShow
instance decodeGetDomainNamesRequest :: Decode GetDomainNamesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDomainNamesRequest :: Encode GetDomainNamesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request a new export of a <a>RestApi</a> for a particular <a>Stage</a>.</p>
newtype GetExportRequest = GetExportRequest 
  { "RestApiId'" :: (String)
  , "StageName'" :: (String)
  , "ExportType'" :: (String)
  , "Parameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "Accepts'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetExportRequest :: Newtype GetExportRequest _
derive instance repGenericGetExportRequest :: Generic GetExportRequest _
instance showGetExportRequest :: Show GetExportRequest where
  show = genericShow
instance decodeGetExportRequest :: Decode GetExportRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetExportRequest :: Encode GetExportRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets a <a>GatewayResponse</a> of a specified response type on the given <a>RestApi</a>.</p>
newtype GetGatewayResponseRequest = GetGatewayResponseRequest 
  { "RestApiId'" :: (String)
  , "ResponseType'" :: (GatewayResponseType)
  }
derive instance newtypeGetGatewayResponseRequest :: Newtype GetGatewayResponseRequest _
derive instance repGenericGetGatewayResponseRequest :: Generic GetGatewayResponseRequest _
instance showGetGatewayResponseRequest :: Show GetGatewayResponseRequest where
  show = genericShow
instance decodeGetGatewayResponseRequest :: Decode GetGatewayResponseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGatewayResponseRequest :: Encode GetGatewayResponseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets the <a>GatewayResponses</a> collection on the given <a>RestApi</a>. If an API developer has not added any definitions for gateway responses, the result will be the API Gateway-generated default <a>GatewayResponses</a> collection for the supported response types.</p>
newtype GetGatewayResponsesRequest = GetGatewayResponsesRequest 
  { "RestApiId'" :: (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetGatewayResponsesRequest :: Newtype GetGatewayResponsesRequest _
derive instance repGenericGetGatewayResponsesRequest :: Generic GetGatewayResponsesRequest _
instance showGetGatewayResponsesRequest :: Show GetGatewayResponsesRequest where
  show = genericShow
instance decodeGetGatewayResponsesRequest :: Decode GetGatewayResponsesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGatewayResponsesRequest :: Encode GetGatewayResponsesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to get the integration configuration.</p>
newtype GetIntegrationRequest = GetIntegrationRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  }
derive instance newtypeGetIntegrationRequest :: Newtype GetIntegrationRequest _
derive instance repGenericGetIntegrationRequest :: Generic GetIntegrationRequest _
instance showGetIntegrationRequest :: Show GetIntegrationRequest where
  show = genericShow
instance decodeGetIntegrationRequest :: Decode GetIntegrationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIntegrationRequest :: Encode GetIntegrationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a get integration response request.</p>
newtype GetIntegrationResponseRequest = GetIntegrationResponseRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "StatusCode'" :: (StatusCode)
  }
derive instance newtypeGetIntegrationResponseRequest :: Newtype GetIntegrationResponseRequest _
derive instance repGenericGetIntegrationResponseRequest :: Generic GetIntegrationResponseRequest _
instance showGetIntegrationResponseRequest :: Show GetIntegrationResponseRequest where
  show = genericShow
instance decodeGetIntegrationResponseRequest :: Decode GetIntegrationResponseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIntegrationResponseRequest :: Encode GetIntegrationResponseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to describe an existing <a>Method</a> resource.</p>
newtype GetMethodRequest = GetMethodRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  }
derive instance newtypeGetMethodRequest :: Newtype GetMethodRequest _
derive instance repGenericGetMethodRequest :: Generic GetMethodRequest _
instance showGetMethodRequest :: Show GetMethodRequest where
  show = genericShow
instance decodeGetMethodRequest :: Decode GetMethodRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMethodRequest :: Encode GetMethodRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to describe a <a>MethodResponse</a> resource.</p>
newtype GetMethodResponseRequest = GetMethodResponseRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "StatusCode'" :: (StatusCode)
  }
derive instance newtypeGetMethodResponseRequest :: Newtype GetMethodResponseRequest _
derive instance repGenericGetMethodResponseRequest :: Generic GetMethodResponseRequest _
instance showGetMethodResponseRequest :: Show GetMethodResponseRequest where
  show = genericShow
instance decodeGetMethodResponseRequest :: Decode GetMethodResponseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMethodResponseRequest :: Encode GetMethodResponseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to list information about a model in an existing <a>RestApi</a> resource.</p>
newtype GetModelRequest = GetModelRequest 
  { "RestApiId'" :: (String)
  , "ModelName'" :: (String)
  , "Flatten'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeGetModelRequest :: Newtype GetModelRequest _
derive instance repGenericGetModelRequest :: Generic GetModelRequest _
instance showGetModelRequest :: Show GetModelRequest where
  show = genericShow
instance decodeGetModelRequest :: Decode GetModelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetModelRequest :: Encode GetModelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to generate a sample mapping template used to transform the payload.</p>
newtype GetModelTemplateRequest = GetModelTemplateRequest 
  { "RestApiId'" :: (String)
  , "ModelName'" :: (String)
  }
derive instance newtypeGetModelTemplateRequest :: Newtype GetModelTemplateRequest _
derive instance repGenericGetModelTemplateRequest :: Generic GetModelTemplateRequest _
instance showGetModelTemplateRequest :: Show GetModelTemplateRequest where
  show = genericShow
instance decodeGetModelTemplateRequest :: Decode GetModelTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetModelTemplateRequest :: Encode GetModelTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to list existing <a>Models</a> defined for a <a>RestApi</a> resource.</p>
newtype GetModelsRequest = GetModelsRequest 
  { "RestApiId'" :: (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetModelsRequest :: Newtype GetModelsRequest _
derive instance repGenericGetModelsRequest :: Generic GetModelsRequest _
instance showGetModelsRequest :: Show GetModelsRequest where
  show = genericShow
instance decodeGetModelsRequest :: Decode GetModelsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetModelsRequest :: Encode GetModelsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets a <a>RequestValidator</a> of a given <a>RestApi</a>.</p>
newtype GetRequestValidatorRequest = GetRequestValidatorRequest 
  { "RestApiId'" :: (String)
  , "RequestValidatorId'" :: (String)
  }
derive instance newtypeGetRequestValidatorRequest :: Newtype GetRequestValidatorRequest _
derive instance repGenericGetRequestValidatorRequest :: Generic GetRequestValidatorRequest _
instance showGetRequestValidatorRequest :: Show GetRequestValidatorRequest where
  show = genericShow
instance decodeGetRequestValidatorRequest :: Decode GetRequestValidatorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRequestValidatorRequest :: Encode GetRequestValidatorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets the <a>RequestValidators</a> collection of a given <a>RestApi</a>.</p>
newtype GetRequestValidatorsRequest = GetRequestValidatorsRequest 
  { "RestApiId'" :: (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetRequestValidatorsRequest :: Newtype GetRequestValidatorsRequest _
derive instance repGenericGetRequestValidatorsRequest :: Generic GetRequestValidatorsRequest _
instance showGetRequestValidatorsRequest :: Show GetRequestValidatorsRequest where
  show = genericShow
instance decodeGetRequestValidatorsRequest :: Decode GetRequestValidatorsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRequestValidatorsRequest :: Encode GetRequestValidatorsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to list information about a resource.</p>
newtype GetResourceRequest = GetResourceRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "Embed'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  }
derive instance newtypeGetResourceRequest :: Newtype GetResourceRequest _
derive instance repGenericGetResourceRequest :: Generic GetResourceRequest _
instance showGetResourceRequest :: Show GetResourceRequest where
  show = genericShow
instance decodeGetResourceRequest :: Decode GetResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetResourceRequest :: Encode GetResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to list information about a collection of resources.</p>
newtype GetResourcesRequest = GetResourcesRequest 
  { "RestApiId'" :: (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Embed'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  }
derive instance newtypeGetResourcesRequest :: Newtype GetResourcesRequest _
derive instance repGenericGetResourcesRequest :: Generic GetResourcesRequest _
instance showGetResourcesRequest :: Show GetResourcesRequest where
  show = genericShow
instance decodeGetResourcesRequest :: Decode GetResourcesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetResourcesRequest :: Encode GetResourcesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The GET request to list an existing <a>RestApi</a> defined for your collection. </p>
newtype GetRestApiRequest = GetRestApiRequest 
  { "RestApiId'" :: (String)
  }
derive instance newtypeGetRestApiRequest :: Newtype GetRestApiRequest _
derive instance repGenericGetRestApiRequest :: Generic GetRestApiRequest _
instance showGetRestApiRequest :: Show GetRestApiRequest where
  show = genericShow
instance decodeGetRestApiRequest :: Decode GetRestApiRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRestApiRequest :: Encode GetRestApiRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The GET request to list existing <a>RestApis</a> defined for your collection.</p>
newtype GetRestApisRequest = GetRestApisRequest 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetRestApisRequest :: Newtype GetRestApisRequest _
derive instance repGenericGetRestApisRequest :: Generic GetRestApisRequest _
instance showGetRestApisRequest :: Show GetRestApisRequest where
  show = genericShow
instance decodeGetRestApisRequest :: Decode GetRestApisRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRestApisRequest :: Encode GetRestApisRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request a new generated client SDK for a <a>RestApi</a> and <a>Stage</a>.</p>
newtype GetSdkRequest = GetSdkRequest 
  { "RestApiId'" :: (String)
  , "StageName'" :: (String)
  , "SdkType'" :: (String)
  , "Parameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  }
derive instance newtypeGetSdkRequest :: Newtype GetSdkRequest _
derive instance repGenericGetSdkRequest :: Generic GetSdkRequest _
instance showGetSdkRequest :: Show GetSdkRequest where
  show = genericShow
instance decodeGetSdkRequest :: Decode GetSdkRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSdkRequest :: Encode GetSdkRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Get an <a>SdkType</a> instance.</p>
newtype GetSdkTypeRequest = GetSdkTypeRequest 
  { "Id'" :: (String)
  }
derive instance newtypeGetSdkTypeRequest :: Newtype GetSdkTypeRequest _
derive instance repGenericGetSdkTypeRequest :: Generic GetSdkTypeRequest _
instance showGetSdkTypeRequest :: Show GetSdkTypeRequest where
  show = genericShow
instance decodeGetSdkTypeRequest :: Decode GetSdkTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSdkTypeRequest :: Encode GetSdkTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Get the <a>SdkTypes</a> collection.</p>
newtype GetSdkTypesRequest = GetSdkTypesRequest 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetSdkTypesRequest :: Newtype GetSdkTypesRequest _
derive instance repGenericGetSdkTypesRequest :: Generic GetSdkTypesRequest _
instance showGetSdkTypesRequest :: Show GetSdkTypesRequest where
  show = genericShow
instance decodeGetSdkTypesRequest :: Decode GetSdkTypesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSdkTypesRequest :: Encode GetSdkTypesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to get information about a <a>Stage</a> resource.</p>
newtype GetStageRequest = GetStageRequest 
  { "RestApiId'" :: (String)
  , "StageName'" :: (String)
  }
derive instance newtypeGetStageRequest :: Newtype GetStageRequest _
derive instance repGenericGetStageRequest :: Generic GetStageRequest _
instance showGetStageRequest :: Show GetStageRequest where
  show = genericShow
instance decodeGetStageRequest :: Decode GetStageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStageRequest :: Encode GetStageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to get information about one or more <a>Stage</a> resources.</p>
newtype GetStagesRequest = GetStagesRequest 
  { "RestApiId'" :: (String)
  , "DeploymentId'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetStagesRequest :: Newtype GetStagesRequest _
derive instance repGenericGetStagesRequest :: Generic GetStagesRequest _
instance showGetStagesRequest :: Show GetStagesRequest where
  show = genericShow
instance decodeGetStagesRequest :: Decode GetStagesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStagesRequest :: Encode GetStagesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets the Tags collection for a given resource.</p>
newtype GetTagsRequest = GetTagsRequest 
  { "ResourceArn'" :: (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetTagsRequest :: Newtype GetTagsRequest _
derive instance repGenericGetTagsRequest :: Generic GetTagsRequest _
instance showGetTagsRequest :: Show GetTagsRequest where
  show = genericShow
instance decodeGetTagsRequest :: Decode GetTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTagsRequest :: Encode GetTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The GET request to get a usage plan key of a given key identifier.</p>
newtype GetUsagePlanKeyRequest = GetUsagePlanKeyRequest 
  { "UsagePlanId'" :: (String)
  , "KeyId'" :: (String)
  }
derive instance newtypeGetUsagePlanKeyRequest :: Newtype GetUsagePlanKeyRequest _
derive instance repGenericGetUsagePlanKeyRequest :: Generic GetUsagePlanKeyRequest _
instance showGetUsagePlanKeyRequest :: Show GetUsagePlanKeyRequest where
  show = genericShow
instance decodeGetUsagePlanKeyRequest :: Decode GetUsagePlanKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUsagePlanKeyRequest :: Encode GetUsagePlanKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The GET request to get all the usage plan keys representing the API keys added to a specified usage plan.</p>
newtype GetUsagePlanKeysRequest = GetUsagePlanKeysRequest 
  { "UsagePlanId'" :: (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "NameQuery'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetUsagePlanKeysRequest :: Newtype GetUsagePlanKeysRequest _
derive instance repGenericGetUsagePlanKeysRequest :: Generic GetUsagePlanKeysRequest _
instance showGetUsagePlanKeysRequest :: Show GetUsagePlanKeysRequest where
  show = genericShow
instance decodeGetUsagePlanKeysRequest :: Decode GetUsagePlanKeysRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUsagePlanKeysRequest :: Encode GetUsagePlanKeysRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The GET request to get a usage plan of a given plan identifier.</p>
newtype GetUsagePlanRequest = GetUsagePlanRequest 
  { "UsagePlanId'" :: (String)
  }
derive instance newtypeGetUsagePlanRequest :: Newtype GetUsagePlanRequest _
derive instance repGenericGetUsagePlanRequest :: Generic GetUsagePlanRequest _
instance showGetUsagePlanRequest :: Show GetUsagePlanRequest where
  show = genericShow
instance decodeGetUsagePlanRequest :: Decode GetUsagePlanRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUsagePlanRequest :: Encode GetUsagePlanRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The GET request to get all the usage plans of the caller's account.</p>
newtype GetUsagePlansRequest = GetUsagePlansRequest 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "KeyId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetUsagePlansRequest :: Newtype GetUsagePlansRequest _
derive instance repGenericGetUsagePlansRequest :: Generic GetUsagePlansRequest _
instance showGetUsagePlansRequest :: Show GetUsagePlansRequest where
  show = genericShow
instance decodeGetUsagePlansRequest :: Decode GetUsagePlansRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUsagePlansRequest :: Encode GetUsagePlansRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The GET request to get the usage data of a usage plan in a specified time interval.</p>
newtype GetUsageRequest = GetUsageRequest 
  { "UsagePlanId'" :: (String)
  , "KeyId'" :: NullOrUndefined.NullOrUndefined (String)
  , "StartDate'" :: (String)
  , "EndDate'" :: (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetUsageRequest :: Newtype GetUsageRequest _
derive instance repGenericGetUsageRequest :: Generic GetUsageRequest _
instance showGetUsageRequest :: Show GetUsageRequest where
  show = genericShow
instance decodeGetUsageRequest :: Decode GetUsageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUsageRequest :: Encode GetUsageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets a specified VPC link under the caller's account in a region.</p>
newtype GetVpcLinkRequest = GetVpcLinkRequest 
  { "VpcLinkId'" :: (String)
  }
derive instance newtypeGetVpcLinkRequest :: Newtype GetVpcLinkRequest _
derive instance repGenericGetVpcLinkRequest :: Generic GetVpcLinkRequest _
instance showGetVpcLinkRequest :: Show GetVpcLinkRequest where
  show = genericShow
instance decodeGetVpcLinkRequest :: Decode GetVpcLinkRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetVpcLinkRequest :: Encode GetVpcLinkRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets the <a>VpcLinks</a> collection under the caller's account in a selected region.</p>
newtype GetVpcLinksRequest = GetVpcLinksRequest 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeGetVpcLinksRequest :: Newtype GetVpcLinksRequest _
derive instance repGenericGetVpcLinksRequest :: Generic GetVpcLinksRequest _
instance showGetVpcLinksRequest :: Show GetVpcLinksRequest where
  show = genericShow
instance decodeGetVpcLinksRequest :: Decode GetVpcLinksRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetVpcLinksRequest :: Encode GetVpcLinksRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The POST request to import API keys from an external source, such as a CSV-formatted file.</p>
newtype ImportApiKeysRequest = ImportApiKeysRequest 
  { "Body'" :: (String)
  , "Format'" :: (ApiKeysFormat)
  , "FailOnWarnings'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeImportApiKeysRequest :: Newtype ImportApiKeysRequest _
derive instance repGenericImportApiKeysRequest :: Generic ImportApiKeysRequest _
instance showImportApiKeysRequest :: Show ImportApiKeysRequest where
  show = genericShow
instance decodeImportApiKeysRequest :: Decode ImportApiKeysRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportApiKeysRequest :: Encode ImportApiKeysRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Import documentation parts from an external (e.g., Swagger) definition file. </p>
newtype ImportDocumentationPartsRequest = ImportDocumentationPartsRequest 
  { "RestApiId'" :: (String)
  , "Mode'" :: NullOrUndefined.NullOrUndefined (PutMode)
  , "FailOnWarnings'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Body'" :: (String)
  }
derive instance newtypeImportDocumentationPartsRequest :: Newtype ImportDocumentationPartsRequest _
derive instance repGenericImportDocumentationPartsRequest :: Generic ImportDocumentationPartsRequest _
instance showImportDocumentationPartsRequest :: Show ImportDocumentationPartsRequest where
  show = genericShow
instance decodeImportDocumentationPartsRequest :: Decode ImportDocumentationPartsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportDocumentationPartsRequest :: Encode ImportDocumentationPartsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A POST request to import an API to API Gateway using an input of an API definition file.</p>
newtype ImportRestApiRequest = ImportRestApiRequest 
  { "FailOnWarnings'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Parameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "Body'" :: (String)
  }
derive instance newtypeImportRestApiRequest :: Newtype ImportRestApiRequest _
derive instance repGenericImportRestApiRequest :: Generic ImportRestApiRequest _
instance showImportRestApiRequest :: Show ImportRestApiRequest where
  show = genericShow
instance decodeImportRestApiRequest :: Decode ImportRestApiRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportRestApiRequest :: Encode ImportRestApiRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an HTTP, HTTP_PROXY, AWS, AWS_PROXY, or Mock integration.</p> <div class="remarks">In the API Gateway console, the built-in Lambda integration is an AWS integration.</div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Creating an API</a> </div>
newtype Integration = Integration 
  { "Type'" :: NullOrUndefined.NullOrUndefined (IntegrationType)
  , "HttpMethod'" :: NullOrUndefined.NullOrUndefined (String)
  , "Uri'" :: NullOrUndefined.NullOrUndefined (String)
  , "ConnectionType'" :: NullOrUndefined.NullOrUndefined (ConnectionType)
  , "ConnectionId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Credentials'" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestParameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "RequestTemplates'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "PassthroughBehavior'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContentHandling'" :: NullOrUndefined.NullOrUndefined (ContentHandlingStrategy)
  , "TimeoutInMillis'" :: NullOrUndefined.NullOrUndefined (Int)
  , "CacheNamespace'" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheKeyParameters'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  , "IntegrationResponses'" :: NullOrUndefined.NullOrUndefined (MapOfIntegrationResponse)
  }
derive instance newtypeIntegration :: Newtype Integration _
derive instance repGenericIntegration :: Generic Integration _
instance showIntegration :: Show Integration where
  show = genericShow
instance decodeIntegration :: Decode Integration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntegration :: Encode Integration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an integration response. The status code must map to an existing <a>MethodResponse</a>, and parameters and templates can be used to transform the back-end response.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Creating an API</a> </div>
newtype IntegrationResponse = IntegrationResponse 
  { "StatusCode'" :: NullOrUndefined.NullOrUndefined (StatusCode)
  , "SelectionPattern'" :: NullOrUndefined.NullOrUndefined (String)
  , "ResponseParameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "ResponseTemplates'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "ContentHandling'" :: NullOrUndefined.NullOrUndefined (ContentHandlingStrategy)
  }
derive instance newtypeIntegrationResponse :: Newtype IntegrationResponse _
derive instance repGenericIntegrationResponse :: Generic IntegrationResponse _
instance showIntegrationResponse :: Show IntegrationResponse where
  show = genericShow
instance decodeIntegrationResponse :: Decode IntegrationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntegrationResponse :: Encode IntegrationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The integration type. The valid value is <code>HTTP</code> for integrating an API method with an HTTP backend; <code>AWS</code> with any AWS service endpoints; <code>MOCK</code> for testing without actually invoking the backend; <code>HTTP_PROXY</code> for integrating with the HTTP proxy integration; <code>AWS_PROXY</code> for integrating with the Lambda proxy integration. </p>
newtype IntegrationType = IntegrationType String
derive instance newtypeIntegrationType :: Newtype IntegrationType _
derive instance repGenericIntegrationType :: Generic IntegrationType _
instance showIntegrationType :: Show IntegrationType where
  show = genericShow
instance decodeIntegrationType :: Decode IntegrationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntegrationType :: Encode IntegrationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request exceeded the rate limit. Retry after the specified time period.</p>
newtype LimitExceededException = LimitExceededException 
  { "RetryAfterSeconds'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfARNs = ListOfARNs (Array ProviderARN)
derive instance newtypeListOfARNs :: Newtype ListOfARNs _
derive instance repGenericListOfARNs :: Generic ListOfARNs _
instance showListOfARNs :: Show ListOfARNs where
  show = genericShow
instance decodeListOfARNs :: Decode ListOfARNs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfARNs :: Encode ListOfARNs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfApiKey = ListOfApiKey (Array ApiKey)
derive instance newtypeListOfApiKey :: Newtype ListOfApiKey _
derive instance repGenericListOfApiKey :: Generic ListOfApiKey _
instance showListOfApiKey :: Show ListOfApiKey where
  show = genericShow
instance decodeListOfApiKey :: Decode ListOfApiKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfApiKey :: Encode ListOfApiKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfApiStage = ListOfApiStage (Array ApiStage)
derive instance newtypeListOfApiStage :: Newtype ListOfApiStage _
derive instance repGenericListOfApiStage :: Generic ListOfApiStage _
instance showListOfApiStage :: Show ListOfApiStage where
  show = genericShow
instance decodeListOfApiStage :: Decode ListOfApiStage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfApiStage :: Encode ListOfApiStage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfAuthorizer = ListOfAuthorizer (Array Authorizer)
derive instance newtypeListOfAuthorizer :: Newtype ListOfAuthorizer _
derive instance repGenericListOfAuthorizer :: Generic ListOfAuthorizer _
instance showListOfAuthorizer :: Show ListOfAuthorizer where
  show = genericShow
instance decodeListOfAuthorizer :: Decode ListOfAuthorizer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfAuthorizer :: Encode ListOfAuthorizer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfBasePathMapping = ListOfBasePathMapping (Array BasePathMapping)
derive instance newtypeListOfBasePathMapping :: Newtype ListOfBasePathMapping _
derive instance repGenericListOfBasePathMapping :: Generic ListOfBasePathMapping _
instance showListOfBasePathMapping :: Show ListOfBasePathMapping where
  show = genericShow
instance decodeListOfBasePathMapping :: Decode ListOfBasePathMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfBasePathMapping :: Encode ListOfBasePathMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfClientCertificate = ListOfClientCertificate (Array ClientCertificate)
derive instance newtypeListOfClientCertificate :: Newtype ListOfClientCertificate _
derive instance repGenericListOfClientCertificate :: Generic ListOfClientCertificate _
instance showListOfClientCertificate :: Show ListOfClientCertificate where
  show = genericShow
instance decodeListOfClientCertificate :: Decode ListOfClientCertificate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfClientCertificate :: Encode ListOfClientCertificate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfDeployment = ListOfDeployment (Array Deployment)
derive instance newtypeListOfDeployment :: Newtype ListOfDeployment _
derive instance repGenericListOfDeployment :: Generic ListOfDeployment _
instance showListOfDeployment :: Show ListOfDeployment where
  show = genericShow
instance decodeListOfDeployment :: Decode ListOfDeployment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfDeployment :: Encode ListOfDeployment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfDocumentationPart = ListOfDocumentationPart (Array DocumentationPart)
derive instance newtypeListOfDocumentationPart :: Newtype ListOfDocumentationPart _
derive instance repGenericListOfDocumentationPart :: Generic ListOfDocumentationPart _
instance showListOfDocumentationPart :: Show ListOfDocumentationPart where
  show = genericShow
instance decodeListOfDocumentationPart :: Decode ListOfDocumentationPart where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfDocumentationPart :: Encode ListOfDocumentationPart where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfDocumentationVersion = ListOfDocumentationVersion (Array DocumentationVersion)
derive instance newtypeListOfDocumentationVersion :: Newtype ListOfDocumentationVersion _
derive instance repGenericListOfDocumentationVersion :: Generic ListOfDocumentationVersion _
instance showListOfDocumentationVersion :: Show ListOfDocumentationVersion where
  show = genericShow
instance decodeListOfDocumentationVersion :: Decode ListOfDocumentationVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfDocumentationVersion :: Encode ListOfDocumentationVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfDomainName = ListOfDomainName (Array DomainName)
derive instance newtypeListOfDomainName :: Newtype ListOfDomainName _
derive instance repGenericListOfDomainName :: Generic ListOfDomainName _
instance showListOfDomainName :: Show ListOfDomainName where
  show = genericShow
instance decodeListOfDomainName :: Decode ListOfDomainName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfDomainName :: Encode ListOfDomainName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfEndpointType = ListOfEndpointType (Array EndpointType)
derive instance newtypeListOfEndpointType :: Newtype ListOfEndpointType _
derive instance repGenericListOfEndpointType :: Generic ListOfEndpointType _
instance showListOfEndpointType :: Show ListOfEndpointType where
  show = genericShow
instance decodeListOfEndpointType :: Decode ListOfEndpointType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfEndpointType :: Encode ListOfEndpointType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfGatewayResponse = ListOfGatewayResponse (Array GatewayResponse)
derive instance newtypeListOfGatewayResponse :: Newtype ListOfGatewayResponse _
derive instance repGenericListOfGatewayResponse :: Generic ListOfGatewayResponse _
instance showListOfGatewayResponse :: Show ListOfGatewayResponse where
  show = genericShow
instance decodeListOfGatewayResponse :: Decode ListOfGatewayResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfGatewayResponse :: Encode ListOfGatewayResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfLong = ListOfLong (Array Number)
derive instance newtypeListOfLong :: Newtype ListOfLong _
derive instance repGenericListOfLong :: Generic ListOfLong _
instance showListOfLong :: Show ListOfLong where
  show = genericShow
instance decodeListOfLong :: Decode ListOfLong where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfLong :: Encode ListOfLong where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfModel = ListOfModel (Array Model)
derive instance newtypeListOfModel :: Newtype ListOfModel _
derive instance repGenericListOfModel :: Generic ListOfModel _
instance showListOfModel :: Show ListOfModel where
  show = genericShow
instance decodeListOfModel :: Decode ListOfModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfModel :: Encode ListOfModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of operations describing the updates to apply to the specified resource. The patches are applied in the order specified in the list.
newtype ListOfPatchOperation = ListOfPatchOperation (Array PatchOperation)
derive instance newtypeListOfPatchOperation :: Newtype ListOfPatchOperation _
derive instance repGenericListOfPatchOperation :: Generic ListOfPatchOperation _
instance showListOfPatchOperation :: Show ListOfPatchOperation where
  show = genericShow
instance decodeListOfPatchOperation :: Decode ListOfPatchOperation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfPatchOperation :: Encode ListOfPatchOperation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfRequestValidator = ListOfRequestValidator (Array RequestValidator)
derive instance newtypeListOfRequestValidator :: Newtype ListOfRequestValidator _
derive instance repGenericListOfRequestValidator :: Generic ListOfRequestValidator _
instance showListOfRequestValidator :: Show ListOfRequestValidator where
  show = genericShow
instance decodeListOfRequestValidator :: Decode ListOfRequestValidator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfRequestValidator :: Encode ListOfRequestValidator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfResource = ListOfResource (Array Resource)
derive instance newtypeListOfResource :: Newtype ListOfResource _
derive instance repGenericListOfResource :: Generic ListOfResource _
instance showListOfResource :: Show ListOfResource where
  show = genericShow
instance decodeListOfResource :: Decode ListOfResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfResource :: Encode ListOfResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfRestApi = ListOfRestApi (Array RestApi)
derive instance newtypeListOfRestApi :: Newtype ListOfRestApi _
derive instance repGenericListOfRestApi :: Generic ListOfRestApi _
instance showListOfRestApi :: Show ListOfRestApi where
  show = genericShow
instance decodeListOfRestApi :: Decode ListOfRestApi where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfRestApi :: Encode ListOfRestApi where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfSdkConfigurationProperty = ListOfSdkConfigurationProperty (Array SdkConfigurationProperty)
derive instance newtypeListOfSdkConfigurationProperty :: Newtype ListOfSdkConfigurationProperty _
derive instance repGenericListOfSdkConfigurationProperty :: Generic ListOfSdkConfigurationProperty _
instance showListOfSdkConfigurationProperty :: Show ListOfSdkConfigurationProperty where
  show = genericShow
instance decodeListOfSdkConfigurationProperty :: Decode ListOfSdkConfigurationProperty where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfSdkConfigurationProperty :: Encode ListOfSdkConfigurationProperty where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfSdkType = ListOfSdkType (Array SdkType)
derive instance newtypeListOfSdkType :: Newtype ListOfSdkType _
derive instance repGenericListOfSdkType :: Generic ListOfSdkType _
instance showListOfSdkType :: Show ListOfSdkType where
  show = genericShow
instance decodeListOfSdkType :: Decode ListOfSdkType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfSdkType :: Encode ListOfSdkType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfStage = ListOfStage (Array Stage)
derive instance newtypeListOfStage :: Newtype ListOfStage _
derive instance repGenericListOfStage :: Generic ListOfStage _
instance showListOfStage :: Show ListOfStage where
  show = genericShow
instance decodeListOfStage :: Decode ListOfStage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfStage :: Encode ListOfStage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfStageKeys = ListOfStageKeys (Array StageKey)
derive instance newtypeListOfStageKeys :: Newtype ListOfStageKeys _
derive instance repGenericListOfStageKeys :: Generic ListOfStageKeys _
instance showListOfStageKeys :: Show ListOfStageKeys where
  show = genericShow
instance decodeListOfStageKeys :: Decode ListOfStageKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfStageKeys :: Encode ListOfStageKeys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfString = ListOfString (Array String)
derive instance newtypeListOfString :: Newtype ListOfString _
derive instance repGenericListOfString :: Generic ListOfString _
instance showListOfString :: Show ListOfString where
  show = genericShow
instance decodeListOfString :: Decode ListOfString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfString :: Encode ListOfString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfUsage = ListOfUsage (Array ListOfLong)
derive instance newtypeListOfUsage :: Newtype ListOfUsage _
derive instance repGenericListOfUsage :: Generic ListOfUsage _
instance showListOfUsage :: Show ListOfUsage where
  show = genericShow
instance decodeListOfUsage :: Decode ListOfUsage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfUsage :: Encode ListOfUsage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfUsagePlan = ListOfUsagePlan (Array UsagePlan)
derive instance newtypeListOfUsagePlan :: Newtype ListOfUsagePlan _
derive instance repGenericListOfUsagePlan :: Generic ListOfUsagePlan _
instance showListOfUsagePlan :: Show ListOfUsagePlan where
  show = genericShow
instance decodeListOfUsagePlan :: Decode ListOfUsagePlan where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfUsagePlan :: Encode ListOfUsagePlan where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfUsagePlanKey = ListOfUsagePlanKey (Array UsagePlanKey)
derive instance newtypeListOfUsagePlanKey :: Newtype ListOfUsagePlanKey _
derive instance repGenericListOfUsagePlanKey :: Generic ListOfUsagePlanKey _
instance showListOfUsagePlanKey :: Show ListOfUsagePlanKey where
  show = genericShow
instance decodeListOfUsagePlanKey :: Decode ListOfUsagePlanKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfUsagePlanKey :: Encode ListOfUsagePlanKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfVpcLink = ListOfVpcLink (Array VpcLink)
derive instance newtypeListOfVpcLink :: Newtype ListOfVpcLink _
derive instance repGenericListOfVpcLink :: Generic ListOfVpcLink _
instance showListOfVpcLink :: Show ListOfVpcLink where
  show = genericShow
instance decodeListOfVpcLink :: Decode ListOfVpcLink where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfVpcLink :: Encode ListOfVpcLink where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LocationStatusType = LocationStatusType String
derive instance newtypeLocationStatusType :: Newtype LocationStatusType _
derive instance repGenericLocationStatusType :: Generic LocationStatusType _
instance showLocationStatusType :: Show LocationStatusType where
  show = genericShow
instance decodeLocationStatusType :: Decode LocationStatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocationStatusType :: Encode LocationStatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfHeaderValues = MapOfHeaderValues (StrMap.StrMap String)
derive instance newtypeMapOfHeaderValues :: Newtype MapOfHeaderValues _
derive instance repGenericMapOfHeaderValues :: Generic MapOfHeaderValues _
instance showMapOfHeaderValues :: Show MapOfHeaderValues where
  show = genericShow
instance decodeMapOfHeaderValues :: Decode MapOfHeaderValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfHeaderValues :: Encode MapOfHeaderValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfIntegrationResponse = MapOfIntegrationResponse (StrMap.StrMap IntegrationResponse)
derive instance newtypeMapOfIntegrationResponse :: Newtype MapOfIntegrationResponse _
derive instance repGenericMapOfIntegrationResponse :: Generic MapOfIntegrationResponse _
instance showMapOfIntegrationResponse :: Show MapOfIntegrationResponse where
  show = genericShow
instance decodeMapOfIntegrationResponse :: Decode MapOfIntegrationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfIntegrationResponse :: Encode MapOfIntegrationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfKeyUsages = MapOfKeyUsages (StrMap.StrMap ListOfUsage)
derive instance newtypeMapOfKeyUsages :: Newtype MapOfKeyUsages _
derive instance repGenericMapOfKeyUsages :: Generic MapOfKeyUsages _
instance showMapOfKeyUsages :: Show MapOfKeyUsages where
  show = genericShow
instance decodeMapOfKeyUsages :: Decode MapOfKeyUsages where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfKeyUsages :: Encode MapOfKeyUsages where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfMethod = MapOfMethod (StrMap.StrMap Method)
derive instance newtypeMapOfMethod :: Newtype MapOfMethod _
derive instance repGenericMapOfMethod :: Generic MapOfMethod _
instance showMapOfMethod :: Show MapOfMethod where
  show = genericShow
instance decodeMapOfMethod :: Decode MapOfMethod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfMethod :: Encode MapOfMethod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfMethodResponse = MapOfMethodResponse (StrMap.StrMap MethodResponse)
derive instance newtypeMapOfMethodResponse :: Newtype MapOfMethodResponse _
derive instance repGenericMapOfMethodResponse :: Generic MapOfMethodResponse _
instance showMapOfMethodResponse :: Show MapOfMethodResponse where
  show = genericShow
instance decodeMapOfMethodResponse :: Decode MapOfMethodResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfMethodResponse :: Encode MapOfMethodResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfMethodSettings = MapOfMethodSettings (StrMap.StrMap MethodSetting)
derive instance newtypeMapOfMethodSettings :: Newtype MapOfMethodSettings _
derive instance repGenericMapOfMethodSettings :: Generic MapOfMethodSettings _
instance showMapOfMethodSettings :: Show MapOfMethodSettings where
  show = genericShow
instance decodeMapOfMethodSettings :: Decode MapOfMethodSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfMethodSettings :: Encode MapOfMethodSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfMethodSnapshot = MapOfMethodSnapshot (StrMap.StrMap MethodSnapshot)
derive instance newtypeMapOfMethodSnapshot :: Newtype MapOfMethodSnapshot _
derive instance repGenericMapOfMethodSnapshot :: Generic MapOfMethodSnapshot _
instance showMapOfMethodSnapshot :: Show MapOfMethodSnapshot where
  show = genericShow
instance decodeMapOfMethodSnapshot :: Decode MapOfMethodSnapshot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfMethodSnapshot :: Encode MapOfMethodSnapshot where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfStringToBoolean = MapOfStringToBoolean (StrMap.StrMap NullableBoolean)
derive instance newtypeMapOfStringToBoolean :: Newtype MapOfStringToBoolean _
derive instance repGenericMapOfStringToBoolean :: Generic MapOfStringToBoolean _
instance showMapOfStringToBoolean :: Show MapOfStringToBoolean where
  show = genericShow
instance decodeMapOfStringToBoolean :: Decode MapOfStringToBoolean where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfStringToBoolean :: Encode MapOfStringToBoolean where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfStringToList = MapOfStringToList (StrMap.StrMap ListOfString)
derive instance newtypeMapOfStringToList :: Newtype MapOfStringToList _
derive instance repGenericMapOfStringToList :: Generic MapOfStringToList _
instance showMapOfStringToList :: Show MapOfStringToList where
  show = genericShow
instance decodeMapOfStringToList :: Decode MapOfStringToList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfStringToList :: Encode MapOfStringToList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfStringToString = MapOfStringToString (StrMap.StrMap String)
derive instance newtypeMapOfStringToString :: Newtype MapOfStringToString _
derive instance repGenericMapOfStringToString :: Generic MapOfStringToString _
instance showMapOfStringToString :: Show MapOfStringToString where
  show = genericShow
instance decodeMapOfStringToString :: Decode MapOfStringToString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfStringToString :: Encode MapOfStringToString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents a client-facing interface by which the client calls the API to access back-end resources. A <b>Method</b> resource is integrated with an <a>Integration</a> resource. Both consist of a request and one or more responses. The method request takes the client input that is passed to the back end through the integration request. A method response returns the output from the back end to the client through an integration response. A method request is embodied in a <b>Method</b> resource, whereas an integration request is embodied in an <a>Integration</a> resource. On the other hand, a method response is represented by a <a>MethodResponse</a> resource, whereas an integration response is represented by an <a>IntegrationResponse</a> resource. </p> <div class="remarks"> <p/> <h4>Example: Retrive the GET method on a specified resource</h4> <h5>Request</h5> <p>The following example request retrieves the information about the GET method on an API resource (<code>3kzxbg5sa2</code>) of an API (<code>fugvjdxtri</code>). </p> <pre><code>GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160603T210259Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160603/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}</code></pre> <h5>Response</h5> <p>The successful response returns a <code>200 OK</code> status code and a payload similar to the following:</p> <pre><code>{ "_links": { "curies": [ { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-{rel}.html", "name": "method", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true } ], "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET", "name": "GET", "title": "GET" }, "integration:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "method:integration": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "method:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "methodresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/{status_code}", "templated": true } }, "apiKeyRequired": true, "authorizationType": "NONE", "httpMethod": "GET", "_embedded": { "method:integration": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "3kzxbg5sa2", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestParameters": { "integration.request.header.Content-Type": "'application/x-amz-json-1.1'" }, "requestTemplates": { "application/json": "{\n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-east-1:kinesis:action/ListStreams", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E%23foreach(%24stream%20in%20%24input.path(%27%24.StreamNames%27))%3Cstream%3E%3Cname%3E%24stream%3C%2Fname%3E%3C%2Fstream%3E%23end%3C%2FkinesisStreams%3E\")" }, "statusCode": "200" } } }, "method:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.Content-Type": false }, "statusCode": "200" } } }</code></pre> <p>In the example above, the response template for the <code>200 OK</code> response maps the JSON output from the <code>ListStreams</code> action in the back end to an XML output. The mapping template is URL-encoded as <code>%3CkinesisStreams%3E%23foreach(%24stream%20in%20%24input.path(%27%24.StreamNames%27))%3Cstream%3E%3Cname%3E%24stream%3C%2Fname%3E%3C%2Fstream%3E%23end%3C%2FkinesisStreams%3E</code> and the output is decoded using the <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#util-templat-reference">$util.urlDecode()</a> helper function.</p> </div> <div class="seeAlso"> <a>MethodResponse</a>, <a>Integration</a>, <a>IntegrationResponse</a>, <a>Resource</a>, <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-method-settings.html">Set up an API's method</a> </div>
newtype Method = Method 
  { "HttpMethod'" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthorizationType'" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthorizerId'" :: NullOrUndefined.NullOrUndefined (String)
  , "ApiKeyRequired'" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "RequestValidatorId'" :: NullOrUndefined.NullOrUndefined (String)
  , "OperationName'" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestParameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToBoolean)
  , "RequestModels'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "MethodResponses'" :: NullOrUndefined.NullOrUndefined (MapOfMethodResponse)
  , "MethodIntegration'" :: NullOrUndefined.NullOrUndefined (Integration)
  , "AuthorizationScopes'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  }
derive instance newtypeMethod :: Newtype Method _
derive instance repGenericMethod :: Generic Method _
instance showMethod :: Show Method where
  show = genericShow
instance decodeMethod :: Decode Method where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMethod :: Encode Method where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a method response of a given HTTP status code returned to the client. The method response is passed from the back end through the associated integration response that can be transformed using a mapping template. </p> <div class="remarks"> <p/> <h4>Example: A <b>MethodResponse</b> instance of an API</h4> <h5>Request</h5> <p>The example request retrieves a <b>MethodResponse</b> of the 200 status code.</p> <pre><code>GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160603T222952Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160603/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}</code></pre> <h5>Response</h5> <p>The successful response returns <code>200 OK</code> status and a payload as follows:</p> <pre><code>{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true }, "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.Content-Type": false }, "statusCode": "200" }</code></pre> <p/> </div> <div class="seeAlso"> <a>Method</a>, <a>IntegrationResponse</a>, <a>Integration</a> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Creating an API</a> </div>
newtype MethodResponse = MethodResponse 
  { "StatusCode'" :: NullOrUndefined.NullOrUndefined (StatusCode)
  , "ResponseParameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToBoolean)
  , "ResponseModels'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  }
derive instance newtypeMethodResponse :: Newtype MethodResponse _
derive instance repGenericMethodResponse :: Generic MethodResponse _
instance showMethodResponse :: Show MethodResponse where
  show = genericShow
instance decodeMethodResponse :: Decode MethodResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMethodResponse :: Encode MethodResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the method setting properties.</p>
newtype MethodSetting = MethodSetting 
  { "MetricsEnabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LoggingLevel'" :: NullOrUndefined.NullOrUndefined (String)
  , "DataTraceEnabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ThrottlingBurstLimit'" :: NullOrUndefined.NullOrUndefined (Int)
  , "ThrottlingRateLimit'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CachingEnabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "CacheTtlInSeconds'" :: NullOrUndefined.NullOrUndefined (Int)
  , "CacheDataEncrypted'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RequireAuthorizationForCacheControl'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "UnauthorizedCacheControlHeaderStrategy'" :: NullOrUndefined.NullOrUndefined (UnauthorizedCacheControlHeaderStrategy)
  }
derive instance newtypeMethodSetting :: Newtype MethodSetting _
derive instance repGenericMethodSetting :: Generic MethodSetting _
instance showMethodSetting :: Show MethodSetting where
  show = genericShow
instance decodeMethodSetting :: Decode MethodSetting where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMethodSetting :: Encode MethodSetting where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a summary of a <a>Method</a> resource, given a particular date and time.</p>
newtype MethodSnapshot = MethodSnapshot 
  { "AuthorizationType'" :: NullOrUndefined.NullOrUndefined (String)
  , "ApiKeyRequired'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeMethodSnapshot :: Newtype MethodSnapshot _
derive instance repGenericMethodSnapshot :: Generic MethodSnapshot _
instance showMethodSnapshot :: Show MethodSnapshot where
  show = genericShow
instance decodeMethodSnapshot :: Decode MethodSnapshot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMethodSnapshot :: Encode MethodSnapshot where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the data structure of a method's request or response payload.</p> <div class="remarks"> <p>A request model defines the data structure of the client-supplied request payload. A response model defines the data structure of the response payload returned by the back end. Although not required, models are useful for mapping payloads between the front end and back end.</p> <p>A model is used for generating an API's SDK, validating the input request body, and creating a skeletal mapping template.</p> </div> <div class="seeAlso"> <a>Method</a>, <a>MethodResponse</a>, <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html">Models and Mappings</a> </div>
newtype Model = Model 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Schema'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContentType'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeModel :: Newtype Model _
derive instance repGenericModel :: Generic Model _
instance showModel :: Show Model where
  show = genericShow
instance decodeModel :: Decode Model where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModel :: Encode Model where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection of <a>Model</a> resources.</p> <div class="seeAlso"> <a>Method</a>, <a>MethodResponse</a>, <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html">Models and Mappings</a> </div>
newtype Models = Models 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfModel)
  }
derive instance newtypeModels :: Newtype Models _
derive instance repGenericModels :: Generic Models _
instance showModels :: Show Models where
  show = genericShow
instance decodeModels :: Decode Models where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModels :: Encode Models where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested resource is not found. Make sure that the request URI is correct.</p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableBoolean = NullableBoolean Boolean
derive instance newtypeNullableBoolean :: Newtype NullableBoolean _
derive instance repGenericNullableBoolean :: Generic NullableBoolean _
instance showNullableBoolean :: Show NullableBoolean where
  show = genericShow
instance decodeNullableBoolean :: Decode NullableBoolean where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableBoolean :: Encode NullableBoolean where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableInteger = NullableInteger Int
derive instance newtypeNullableInteger :: Newtype NullableInteger _
derive instance repGenericNullableInteger :: Generic NullableInteger _
instance showNullableInteger :: Show NullableInteger where
  show = genericShow
instance decodeNullableInteger :: Decode NullableInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableInteger :: Encode NullableInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Op = Op String
derive instance newtypeOp :: Newtype Op _
derive instance repGenericOp :: Generic Op _
instance showOp :: Show Op where
  show = genericShow
instance decodeOp :: Decode Op where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOp :: Encode Op where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A single patch operation to apply to the specified resource. Please refer to http://tools.ietf.org/html/rfc6902#section-4 for an explanation of how each operation is used.
newtype PatchOperation = PatchOperation 
  { "Op'" :: NullOrUndefined.NullOrUndefined (Op)
  , "Path'" :: NullOrUndefined.NullOrUndefined (String)
  , "Value'" :: NullOrUndefined.NullOrUndefined (String)
  , "From'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePatchOperation :: Newtype PatchOperation _
derive instance repGenericPatchOperation :: Generic PatchOperation _
instance showPatchOperation :: Show PatchOperation where
  show = genericShow
instance decodePatchOperation :: Decode PatchOperation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchOperation :: Encode PatchOperation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PathToMapOfMethodSnapshot = PathToMapOfMethodSnapshot (StrMap.StrMap MapOfMethodSnapshot)
derive instance newtypePathToMapOfMethodSnapshot :: Newtype PathToMapOfMethodSnapshot _
derive instance repGenericPathToMapOfMethodSnapshot :: Generic PathToMapOfMethodSnapshot _
instance showPathToMapOfMethodSnapshot :: Show PathToMapOfMethodSnapshot where
  show = genericShow
instance decodePathToMapOfMethodSnapshot :: Decode PathToMapOfMethodSnapshot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePathToMapOfMethodSnapshot :: Encode PathToMapOfMethodSnapshot where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProviderARN = ProviderARN String
derive instance newtypeProviderARN :: Newtype ProviderARN _
derive instance repGenericProviderARN :: Generic ProviderARN _
instance showProviderARN :: Show ProviderARN where
  show = genericShow
instance decodeProviderARN :: Decode ProviderARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProviderARN :: Encode ProviderARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Creates a customization of a <a>GatewayResponse</a> of a specified response type and status code on the given <a>RestApi</a>.</p>
newtype PutGatewayResponseRequest = PutGatewayResponseRequest 
  { "RestApiId'" :: (String)
  , "ResponseType'" :: (GatewayResponseType)
  , "StatusCode'" :: NullOrUndefined.NullOrUndefined (StatusCode)
  , "ResponseParameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "ResponseTemplates'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  }
derive instance newtypePutGatewayResponseRequest :: Newtype PutGatewayResponseRequest _
derive instance repGenericPutGatewayResponseRequest :: Generic PutGatewayResponseRequest _
instance showPutGatewayResponseRequest :: Show PutGatewayResponseRequest where
  show = genericShow
instance decodePutGatewayResponseRequest :: Decode PutGatewayResponseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutGatewayResponseRequest :: Encode PutGatewayResponseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Sets up a method's integration.</p>
newtype PutIntegrationRequest = PutIntegrationRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "Type'" :: (IntegrationType)
  , "IntegrationHttpMethod'" :: NullOrUndefined.NullOrUndefined (String)
  , "Uri'" :: NullOrUndefined.NullOrUndefined (String)
  , "ConnectionType'" :: NullOrUndefined.NullOrUndefined (ConnectionType)
  , "ConnectionId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Credentials'" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestParameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "RequestTemplates'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "PassthroughBehavior'" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheNamespace'" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheKeyParameters'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  , "ContentHandling'" :: NullOrUndefined.NullOrUndefined (ContentHandlingStrategy)
  , "TimeoutInMillis'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypePutIntegrationRequest :: Newtype PutIntegrationRequest _
derive instance repGenericPutIntegrationRequest :: Generic PutIntegrationRequest _
instance showPutIntegrationRequest :: Show PutIntegrationRequest where
  show = genericShow
instance decodePutIntegrationRequest :: Decode PutIntegrationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutIntegrationRequest :: Encode PutIntegrationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a put integration response request.</p>
newtype PutIntegrationResponseRequest = PutIntegrationResponseRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "StatusCode'" :: (StatusCode)
  , "SelectionPattern'" :: NullOrUndefined.NullOrUndefined (String)
  , "ResponseParameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "ResponseTemplates'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "ContentHandling'" :: NullOrUndefined.NullOrUndefined (ContentHandlingStrategy)
  }
derive instance newtypePutIntegrationResponseRequest :: Newtype PutIntegrationResponseRequest _
derive instance repGenericPutIntegrationResponseRequest :: Generic PutIntegrationResponseRequest _
instance showPutIntegrationResponseRequest :: Show PutIntegrationResponseRequest where
  show = genericShow
instance decodePutIntegrationResponseRequest :: Decode PutIntegrationResponseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutIntegrationResponseRequest :: Encode PutIntegrationResponseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to add a method to an existing <a>Resource</a> resource.</p>
newtype PutMethodRequest = PutMethodRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "AuthorizationType'" :: (String)
  , "AuthorizerId'" :: NullOrUndefined.NullOrUndefined (String)
  , "ApiKeyRequired'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "OperationName'" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestParameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToBoolean)
  , "RequestModels'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "RequestValidatorId'" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthorizationScopes'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  }
derive instance newtypePutMethodRequest :: Newtype PutMethodRequest _
derive instance repGenericPutMethodRequest :: Generic PutMethodRequest _
instance showPutMethodRequest :: Show PutMethodRequest where
  show = genericShow
instance decodePutMethodRequest :: Decode PutMethodRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutMethodRequest :: Encode PutMethodRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to add a <a>MethodResponse</a> to an existing <a>Method</a> resource.</p>
newtype PutMethodResponseRequest = PutMethodResponseRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "StatusCode'" :: (StatusCode)
  , "ResponseParameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToBoolean)
  , "ResponseModels'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  }
derive instance newtypePutMethodResponseRequest :: Newtype PutMethodResponseRequest _
derive instance repGenericPutMethodResponseRequest :: Generic PutMethodResponseRequest _
instance showPutMethodResponseRequest :: Show PutMethodResponseRequest where
  show = genericShow
instance decodePutMethodResponseRequest :: Decode PutMethodResponseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutMethodResponseRequest :: Encode PutMethodResponseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutMode = PutMode String
derive instance newtypePutMode :: Newtype PutMode _
derive instance repGenericPutMode :: Generic PutMode _
instance showPutMode :: Show PutMode where
  show = genericShow
instance decodePutMode :: Decode PutMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutMode :: Encode PutMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A PUT request to update an existing API, with external API definitions specified as the request body.</p>
newtype PutRestApiRequest = PutRestApiRequest 
  { "RestApiId'" :: (String)
  , "Mode'" :: NullOrUndefined.NullOrUndefined (PutMode)
  , "FailOnWarnings'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Parameters'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "Body'" :: (String)
  }
derive instance newtypePutRestApiRequest :: Newtype PutRestApiRequest _
derive instance repGenericPutRestApiRequest :: Generic PutRestApiRequest _
instance showPutRestApiRequest :: Show PutRestApiRequest where
  show = genericShow
instance decodePutRestApiRequest :: Decode PutRestApiRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRestApiRequest :: Encode PutRestApiRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QuotaPeriodType = QuotaPeriodType String
derive instance newtypeQuotaPeriodType :: Newtype QuotaPeriodType _
derive instance repGenericQuotaPeriodType :: Generic QuotaPeriodType _
instance showQuotaPeriodType :: Show QuotaPeriodType where
  show = genericShow
instance decodeQuotaPeriodType :: Decode QuotaPeriodType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQuotaPeriodType :: Encode QuotaPeriodType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Quotas configured for a usage plan.</p>
newtype QuotaSettings = QuotaSettings 
  { "Limit'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Offset'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Period'" :: NullOrUndefined.NullOrUndefined (QuotaPeriodType)
  }
derive instance newtypeQuotaSettings :: Newtype QuotaSettings _
derive instance repGenericQuotaSettings :: Generic QuotaSettings _
instance showQuotaSettings :: Show QuotaSettings where
  show = genericShow
instance decodeQuotaSettings :: Decode QuotaSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQuotaSettings :: Encode QuotaSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A set of validation rules for incoming <a>Method</a> requests.</p> <div class="remarks"> <p>In Swagger, a <a>RequestValidator</a> of an API is defined by the <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validators.requestValidator.html">x-amazon-apigateway-request-validators.requestValidator</a> object. It the referenced using the <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validator">x-amazon-apigateway-request-validator</a> property.</p> </div> <div class="seeAlso"><a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-method-request-validation.html">Enable Basic Request Validation in API Gateway</a></div>
newtype RequestValidator = RequestValidator 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "ValidateRequestBody'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ValidateRequestParameters'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeRequestValidator :: Newtype RequestValidator _
derive instance repGenericRequestValidator :: Generic RequestValidator _
instance showRequestValidator :: Show RequestValidator where
  show = genericShow
instance decodeRequestValidator :: Decode RequestValidator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestValidator :: Encode RequestValidator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A collection of <a>RequestValidator</a> resources of a given <a>RestApi</a>.</p> <div class="remarks"> <p>In Swagger, the <a>RequestValidators</a> of an API is defined by the <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validators.html">x-amazon-apigateway-request-validators</a> extension.</p> </div> <div class="seeAlso"><a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-method-request-validation.html">Enable Basic Request Validation in API Gateway</a></div>
newtype RequestValidators = RequestValidators 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfRequestValidator)
  }
derive instance newtypeRequestValidators :: Newtype RequestValidators _
derive instance repGenericRequestValidators :: Generic RequestValidators _
instance showRequestValidators :: Show RequestValidators where
  show = genericShow
instance decodeRequestValidators :: Decode RequestValidators where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestValidators :: Encode RequestValidators where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an API resource.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Create an API</a> </div>
newtype Resource = Resource 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "ParentId'" :: NullOrUndefined.NullOrUndefined (String)
  , "PathPart'" :: NullOrUndefined.NullOrUndefined (String)
  , "Path'" :: NullOrUndefined.NullOrUndefined (String)
  , "ResourceMethods'" :: NullOrUndefined.NullOrUndefined (MapOfMethod)
  }
derive instance newtypeResource :: Newtype Resource _
derive instance repGenericResource :: Generic Resource _
instance showResource :: Show Resource where
  show = genericShow
instance decodeResource :: Decode Resource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResource :: Encode Resource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection of <a>Resource</a> resources.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Create an API</a> </div>
newtype Resources = Resources 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfResource)
  }
derive instance newtypeResources :: Newtype Resources _
derive instance repGenericResources :: Generic Resources _
instance showResources :: Show Resources where
  show = genericShow
instance decodeResources :: Decode Resources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResources :: Encode Resources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a REST API.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Create an API</a> </div>
newtype RestApi = RestApi 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version'" :: NullOrUndefined.NullOrUndefined (String)
  , "Warnings'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  , "BinaryMediaTypes'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  , "MinimumCompressionSize'" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "ApiKeySource'" :: NullOrUndefined.NullOrUndefined (ApiKeySourceType)
  , "EndpointConfiguration'" :: NullOrUndefined.NullOrUndefined (EndpointConfiguration)
  }
derive instance newtypeRestApi :: Newtype RestApi _
derive instance repGenericRestApi :: Generic RestApi _
instance showRestApi :: Show RestApi where
  show = genericShow
instance decodeRestApi :: Decode RestApi where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestApi :: Encode RestApi where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains references to your APIs and links that guide you in how to interact with your collection. A collection offers a paginated view of your APIs.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html">Create an API</a> </div>
newtype RestApis = RestApis 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfRestApi)
  }
derive instance newtypeRestApis :: Newtype RestApis _
derive instance repGenericRestApis :: Generic RestApis _
instance showRestApis :: Show RestApis where
  show = genericShow
instance decodeRestApis :: Decode RestApis where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestApis :: Encode RestApis where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A configuration property of an SDK type.</p>
newtype SdkConfigurationProperty = SdkConfigurationProperty 
  { "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "FriendlyName'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Required'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "DefaultValue'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSdkConfigurationProperty :: Newtype SdkConfigurationProperty _
derive instance repGenericSdkConfigurationProperty :: Generic SdkConfigurationProperty _
instance showSdkConfigurationProperty :: Show SdkConfigurationProperty where
  show = genericShow
instance decodeSdkConfigurationProperty :: Decode SdkConfigurationProperty where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSdkConfigurationProperty :: Encode SdkConfigurationProperty where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The binary blob response to <a>GetSdk</a>, which contains the generated SDK.</p>
newtype SdkResponse = SdkResponse 
  { "ContentType'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContentDisposition'" :: NullOrUndefined.NullOrUndefined (String)
  , "Body'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSdkResponse :: Newtype SdkResponse _
derive instance repGenericSdkResponse :: Generic SdkResponse _
instance showSdkResponse :: Show SdkResponse where
  show = genericShow
instance decodeSdkResponse :: Decode SdkResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSdkResponse :: Encode SdkResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A type of SDK that API Gateway can generate.</p>
newtype SdkType = SdkType 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "FriendlyName'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "ConfigurationProperties'" :: NullOrUndefined.NullOrUndefined (ListOfSdkConfigurationProperty)
  }
derive instance newtypeSdkType :: Newtype SdkType _
derive instance repGenericSdkType :: Generic SdkType _
instance showSdkType :: Show SdkType where
  show = genericShow
instance decodeSdkType :: Decode SdkType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSdkType :: Encode SdkType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The collection of <a>SdkType</a> instances.</p>
newtype SdkTypes = SdkTypes 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfSdkType)
  }
derive instance newtypeSdkTypes :: Newtype SdkTypes _
derive instance repGenericSdkTypes :: Generic SdkTypes _
instance showSdkTypes :: Show SdkTypes where
  show = genericShow
instance decodeSdkTypes :: Decode SdkTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSdkTypes :: Encode SdkTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested service is not available. For details see the accompanying error message. Retry after the specified time period.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "RetryAfterSeconds'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _
derive instance repGenericServiceUnavailableException :: Generic ServiceUnavailableException _
instance showServiceUnavailableException :: Show ServiceUnavailableException where
  show = genericShow
instance decodeServiceUnavailableException :: Decode ServiceUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUnavailableException :: Encode ServiceUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a unique identifier for a version of a deployed <a>RestApi</a> that is callable by users.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html">Deploy an API</a> </div>
newtype Stage = Stage 
  { "DeploymentId'" :: NullOrUndefined.NullOrUndefined (String)
  , "ClientCertificateId'" :: NullOrUndefined.NullOrUndefined (String)
  , "StageName'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "CacheClusterEnabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "CacheClusterSize'" :: NullOrUndefined.NullOrUndefined (CacheClusterSize)
  , "CacheClusterStatus'" :: NullOrUndefined.NullOrUndefined (CacheClusterStatus)
  , "MethodSettings'" :: NullOrUndefined.NullOrUndefined (MapOfMethodSettings)
  , "Variables'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "DocumentationVersion'" :: NullOrUndefined.NullOrUndefined (String)
  , "AccessLogSettings'" :: NullOrUndefined.NullOrUndefined (AccessLogSettings)
  , "CanarySettings'" :: NullOrUndefined.NullOrUndefined (CanarySettings)
  , "Tags'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeStage :: Newtype Stage _
derive instance repGenericStage :: Generic Stage _
instance showStage :: Show Stage where
  show = genericShow
instance decodeStage :: Decode Stage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStage :: Encode Stage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A reference to a unique stage identified in the format <code>{restApiId}/{stage}</code>.</p>
newtype StageKey = StageKey 
  { "RestApiId'" :: NullOrUndefined.NullOrUndefined (String)
  , "StageName'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeStageKey :: Newtype StageKey _
derive instance repGenericStageKey :: Generic StageKey _
instance showStageKey :: Show StageKey where
  show = genericShow
instance decodeStageKey :: Decode StageKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageKey :: Encode StageKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of <a>Stage</a> resources that are associated with the <a>ApiKey</a> resource.</p> <div class="seeAlso"><a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/stages.html">Deploying API in Stages</a></div>
newtype Stages = Stages 
  { "Item'" :: NullOrUndefined.NullOrUndefined (ListOfStage)
  }
derive instance newtypeStages :: Newtype Stages _
derive instance repGenericStages :: Generic Stages _
instance showStages :: Show Stages where
  show = genericShow
instance decodeStages :: Decode Stages where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStages :: Encode Stages where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The status code.</p>
newtype StatusCode = StatusCode String
derive instance newtypeStatusCode :: Newtype StatusCode _
derive instance repGenericStatusCode :: Generic StatusCode _
instance showStatusCode :: Show StatusCode where
  show = genericShow
instance decodeStatusCode :: Decode StatusCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusCode :: Encode StatusCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Adds or updates Tags on a gievn resource.</p>
newtype TagResourceRequest = TagResourceRequest 
  { "ResourceArn'" :: (String)
  , "Tags'" :: (MapOfStringToString)
  }
derive instance newtypeTagResourceRequest :: Newtype TagResourceRequest _
derive instance repGenericTagResourceRequest :: Generic TagResourceRequest _
instance showTagResourceRequest :: Show TagResourceRequest where
  show = genericShow
instance decodeTagResourceRequest :: Decode TagResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourceRequest :: Encode TagResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A collection of Tags associated with a given resource.</p>
newtype Tags = Tags 
  { "Tags'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  }
derive instance newtypeTags :: Newtype Tags _
derive instance repGenericTags :: Generic Tags _
instance showTags :: Show Tags where
  show = genericShow
instance decodeTags :: Decode Tags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTags :: Encode Tags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a mapping template used to transform a payload.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html#models-mappings-mappings">Mapping Templates</a> </div>
newtype Template = Template 
  { "Value'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTemplate :: Newtype Template _
derive instance repGenericTemplate :: Generic Template _
instance showTemplate :: Show Template where
  show = genericShow
instance decodeTemplate :: Decode Template where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplate :: Encode Template where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Make a request to simulate the execution of an <a>Authorizer</a>.</p>
newtype TestInvokeAuthorizerRequest = TestInvokeAuthorizerRequest 
  { "RestApiId'" :: (String)
  , "AuthorizerId'" :: (String)
  , "Headers'" :: NullOrUndefined.NullOrUndefined (MapOfHeaderValues)
  , "PathWithQueryString'" :: NullOrUndefined.NullOrUndefined (String)
  , "Body'" :: NullOrUndefined.NullOrUndefined (String)
  , "StageVariables'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "AdditionalContext'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  }
derive instance newtypeTestInvokeAuthorizerRequest :: Newtype TestInvokeAuthorizerRequest _
derive instance repGenericTestInvokeAuthorizerRequest :: Generic TestInvokeAuthorizerRequest _
instance showTestInvokeAuthorizerRequest :: Show TestInvokeAuthorizerRequest where
  show = genericShow
instance decodeTestInvokeAuthorizerRequest :: Decode TestInvokeAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestInvokeAuthorizerRequest :: Encode TestInvokeAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response of the test invoke request for a custom <a>Authorizer</a></p>
newtype TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse 
  { "ClientStatus'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Log'" :: NullOrUndefined.NullOrUndefined (String)
  , "Latency'" :: NullOrUndefined.NullOrUndefined (Number)
  , "PrincipalId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Policy'" :: NullOrUndefined.NullOrUndefined (String)
  , "Authorization'" :: NullOrUndefined.NullOrUndefined (MapOfStringToList)
  , "Claims'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  }
derive instance newtypeTestInvokeAuthorizerResponse :: Newtype TestInvokeAuthorizerResponse _
derive instance repGenericTestInvokeAuthorizerResponse :: Generic TestInvokeAuthorizerResponse _
instance showTestInvokeAuthorizerResponse :: Show TestInvokeAuthorizerResponse where
  show = genericShow
instance decodeTestInvokeAuthorizerResponse :: Decode TestInvokeAuthorizerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestInvokeAuthorizerResponse :: Encode TestInvokeAuthorizerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Make a request to simulate the execution of a <a>Method</a>.</p>
newtype TestInvokeMethodRequest = TestInvokeMethodRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "PathWithQueryString'" :: NullOrUndefined.NullOrUndefined (String)
  , "Body'" :: NullOrUndefined.NullOrUndefined (String)
  , "Headers'" :: NullOrUndefined.NullOrUndefined (MapOfHeaderValues)
  , "ClientCertificateId'" :: NullOrUndefined.NullOrUndefined (String)
  , "StageVariables'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  }
derive instance newtypeTestInvokeMethodRequest :: Newtype TestInvokeMethodRequest _
derive instance repGenericTestInvokeMethodRequest :: Generic TestInvokeMethodRequest _
instance showTestInvokeMethodRequest :: Show TestInvokeMethodRequest where
  show = genericShow
instance decodeTestInvokeMethodRequest :: Decode TestInvokeMethodRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestInvokeMethodRequest :: Encode TestInvokeMethodRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response of the test invoke request in the HTTP method.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-test-method.html#how-to-test-method-console">Test API using the API Gateway console</a> </div>
newtype TestInvokeMethodResponse = TestInvokeMethodResponse 
  { "Status'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Body'" :: NullOrUndefined.NullOrUndefined (String)
  , "Headers'" :: NullOrUndefined.NullOrUndefined (MapOfHeaderValues)
  , "Log'" :: NullOrUndefined.NullOrUndefined (String)
  , "Latency'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeTestInvokeMethodResponse :: Newtype TestInvokeMethodResponse _
derive instance repGenericTestInvokeMethodResponse :: Generic TestInvokeMethodResponse _
instance showTestInvokeMethodResponse :: Show TestInvokeMethodResponse where
  show = genericShow
instance decodeTestInvokeMethodResponse :: Decode TestInvokeMethodResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestInvokeMethodResponse :: Encode TestInvokeMethodResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The API request rate limits.</p>
newtype ThrottleSettings = ThrottleSettings 
  { "BurstLimit'" :: NullOrUndefined.NullOrUndefined (Int)
  , "RateLimit'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeThrottleSettings :: Newtype ThrottleSettings _
derive instance repGenericThrottleSettings :: Generic ThrottleSettings _
instance showThrottleSettings :: Show ThrottleSettings where
  show = genericShow
instance decodeThrottleSettings :: Decode ThrottleSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThrottleSettings :: Encode ThrottleSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request has reached its throttling limit. Retry after the specified time period.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "RetryAfterSeconds'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _
derive instance repGenericTooManyRequestsException :: Generic TooManyRequestsException _
instance showTooManyRequestsException :: Show TooManyRequestsException where
  show = genericShow
instance decodeTooManyRequestsException :: Decode TooManyRequestsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyRequestsException :: Encode TooManyRequestsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UnauthorizedCacheControlHeaderStrategy = UnauthorizedCacheControlHeaderStrategy String
derive instance newtypeUnauthorizedCacheControlHeaderStrategy :: Newtype UnauthorizedCacheControlHeaderStrategy _
derive instance repGenericUnauthorizedCacheControlHeaderStrategy :: Generic UnauthorizedCacheControlHeaderStrategy _
instance showUnauthorizedCacheControlHeaderStrategy :: Show UnauthorizedCacheControlHeaderStrategy where
  show = genericShow
instance decodeUnauthorizedCacheControlHeaderStrategy :: Decode UnauthorizedCacheControlHeaderStrategy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnauthorizedCacheControlHeaderStrategy :: Encode UnauthorizedCacheControlHeaderStrategy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request is denied because the caller has insufficient permissions.</p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnauthorizedException :: Newtype UnauthorizedException _
derive instance repGenericUnauthorizedException :: Generic UnauthorizedException _
instance showUnauthorizedException :: Show UnauthorizedException where
  show = genericShow
instance decodeUnauthorizedException :: Decode UnauthorizedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnauthorizedException :: Encode UnauthorizedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Removes Tags from a given resource.</p>
newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceArn'" :: (String)
  , "TagKeys'" :: (ListOfString)
  }
derive instance newtypeUntagResourceRequest :: Newtype UntagResourceRequest _
derive instance repGenericUntagResourceRequest :: Generic UntagResourceRequest _
instance showUntagResourceRequest :: Show UntagResourceRequest where
  show = genericShow
instance decodeUntagResourceRequest :: Decode UntagResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagResourceRequest :: Encode UntagResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to change information about the current <a>Account</a> resource.</p>
newtype UpdateAccountRequest = UpdateAccountRequest 
  { "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateAccountRequest :: Newtype UpdateAccountRequest _
derive instance repGenericUpdateAccountRequest :: Generic UpdateAccountRequest _
instance showUpdateAccountRequest :: Show UpdateAccountRequest where
  show = genericShow
instance decodeUpdateAccountRequest :: Decode UpdateAccountRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAccountRequest :: Encode UpdateAccountRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to change information about an <a>ApiKey</a> resource.</p>
newtype UpdateApiKeyRequest = UpdateApiKeyRequest 
  { "ApiKey'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateApiKeyRequest :: Newtype UpdateApiKeyRequest _
derive instance repGenericUpdateApiKeyRequest :: Generic UpdateApiKeyRequest _
instance showUpdateApiKeyRequest :: Show UpdateApiKeyRequest where
  show = genericShow
instance decodeUpdateApiKeyRequest :: Decode UpdateApiKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApiKeyRequest :: Encode UpdateApiKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to update an existing <a>Authorizer</a> resource.</p>
newtype UpdateAuthorizerRequest = UpdateAuthorizerRequest 
  { "RestApiId'" :: (String)
  , "AuthorizerId'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateAuthorizerRequest :: Newtype UpdateAuthorizerRequest _
derive instance repGenericUpdateAuthorizerRequest :: Generic UpdateAuthorizerRequest _
instance showUpdateAuthorizerRequest :: Show UpdateAuthorizerRequest where
  show = genericShow
instance decodeUpdateAuthorizerRequest :: Decode UpdateAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAuthorizerRequest :: Encode UpdateAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to change information about the <a>BasePathMapping</a> resource.</p>
newtype UpdateBasePathMappingRequest = UpdateBasePathMappingRequest 
  { "DomainName'" :: (String)
  , "BasePath'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateBasePathMappingRequest :: Newtype UpdateBasePathMappingRequest _
derive instance repGenericUpdateBasePathMappingRequest :: Generic UpdateBasePathMappingRequest _
instance showUpdateBasePathMappingRequest :: Show UpdateBasePathMappingRequest where
  show = genericShow
instance decodeUpdateBasePathMappingRequest :: Decode UpdateBasePathMappingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateBasePathMappingRequest :: Encode UpdateBasePathMappingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to change information about an <a>ClientCertificate</a> resource.</p>
newtype UpdateClientCertificateRequest = UpdateClientCertificateRequest 
  { "ClientCertificateId'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateClientCertificateRequest :: Newtype UpdateClientCertificateRequest _
derive instance repGenericUpdateClientCertificateRequest :: Generic UpdateClientCertificateRequest _
instance showUpdateClientCertificateRequest :: Show UpdateClientCertificateRequest where
  show = genericShow
instance decodeUpdateClientCertificateRequest :: Decode UpdateClientCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateClientCertificateRequest :: Encode UpdateClientCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to change information about a <a>Deployment</a> resource.</p>
newtype UpdateDeploymentRequest = UpdateDeploymentRequest 
  { "RestApiId'" :: (String)
  , "DeploymentId'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateDeploymentRequest :: Newtype UpdateDeploymentRequest _
derive instance repGenericUpdateDeploymentRequest :: Generic UpdateDeploymentRequest _
instance showUpdateDeploymentRequest :: Show UpdateDeploymentRequest where
  show = genericShow
instance decodeUpdateDeploymentRequest :: Decode UpdateDeploymentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDeploymentRequest :: Encode UpdateDeploymentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Updates an existing documentation part of a given API.</p>
newtype UpdateDocumentationPartRequest = UpdateDocumentationPartRequest 
  { "RestApiId'" :: (String)
  , "DocumentationPartId'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateDocumentationPartRequest :: Newtype UpdateDocumentationPartRequest _
derive instance repGenericUpdateDocumentationPartRequest :: Generic UpdateDocumentationPartRequest _
instance showUpdateDocumentationPartRequest :: Show UpdateDocumentationPartRequest where
  show = genericShow
instance decodeUpdateDocumentationPartRequest :: Decode UpdateDocumentationPartRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDocumentationPartRequest :: Encode UpdateDocumentationPartRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Updates an existing documentation version of an API.</p>
newtype UpdateDocumentationVersionRequest = UpdateDocumentationVersionRequest 
  { "RestApiId'" :: (String)
  , "DocumentationVersion'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateDocumentationVersionRequest :: Newtype UpdateDocumentationVersionRequest _
derive instance repGenericUpdateDocumentationVersionRequest :: Generic UpdateDocumentationVersionRequest _
instance showUpdateDocumentationVersionRequest :: Show UpdateDocumentationVersionRequest where
  show = genericShow
instance decodeUpdateDocumentationVersionRequest :: Decode UpdateDocumentationVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDocumentationVersionRequest :: Encode UpdateDocumentationVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to change information about the <a>DomainName</a> resource.</p>
newtype UpdateDomainNameRequest = UpdateDomainNameRequest 
  { "DomainName'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateDomainNameRequest :: Newtype UpdateDomainNameRequest _
derive instance repGenericUpdateDomainNameRequest :: Generic UpdateDomainNameRequest _
instance showUpdateDomainNameRequest :: Show UpdateDomainNameRequest where
  show = genericShow
instance decodeUpdateDomainNameRequest :: Decode UpdateDomainNameRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDomainNameRequest :: Encode UpdateDomainNameRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Updates a <a>GatewayResponse</a> of a specified response type on the given <a>RestApi</a>.</p>
newtype UpdateGatewayResponseRequest = UpdateGatewayResponseRequest 
  { "RestApiId'" :: (String)
  , "ResponseType'" :: (GatewayResponseType)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateGatewayResponseRequest :: Newtype UpdateGatewayResponseRequest _
derive instance repGenericUpdateGatewayResponseRequest :: Generic UpdateGatewayResponseRequest _
instance showUpdateGatewayResponseRequest :: Show UpdateGatewayResponseRequest where
  show = genericShow
instance decodeUpdateGatewayResponseRequest :: Decode UpdateGatewayResponseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGatewayResponseRequest :: Encode UpdateGatewayResponseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an update integration request.</p>
newtype UpdateIntegrationRequest = UpdateIntegrationRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateIntegrationRequest :: Newtype UpdateIntegrationRequest _
derive instance repGenericUpdateIntegrationRequest :: Generic UpdateIntegrationRequest _
instance showUpdateIntegrationRequest :: Show UpdateIntegrationRequest where
  show = genericShow
instance decodeUpdateIntegrationRequest :: Decode UpdateIntegrationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateIntegrationRequest :: Encode UpdateIntegrationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an update integration response request.</p>
newtype UpdateIntegrationResponseRequest = UpdateIntegrationResponseRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "StatusCode'" :: (StatusCode)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateIntegrationResponseRequest :: Newtype UpdateIntegrationResponseRequest _
derive instance repGenericUpdateIntegrationResponseRequest :: Generic UpdateIntegrationResponseRequest _
instance showUpdateIntegrationResponseRequest :: Show UpdateIntegrationResponseRequest where
  show = genericShow
instance decodeUpdateIntegrationResponseRequest :: Decode UpdateIntegrationResponseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateIntegrationResponseRequest :: Encode UpdateIntegrationResponseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to update an existing <a>Method</a> resource.</p>
newtype UpdateMethodRequest = UpdateMethodRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateMethodRequest :: Newtype UpdateMethodRequest _
derive instance repGenericUpdateMethodRequest :: Generic UpdateMethodRequest _
instance showUpdateMethodRequest :: Show UpdateMethodRequest where
  show = genericShow
instance decodeUpdateMethodRequest :: Decode UpdateMethodRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMethodRequest :: Encode UpdateMethodRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to update an existing <a>MethodResponse</a> resource.</p>
newtype UpdateMethodResponseRequest = UpdateMethodResponseRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "HttpMethod'" :: (String)
  , "StatusCode'" :: (StatusCode)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateMethodResponseRequest :: Newtype UpdateMethodResponseRequest _
derive instance repGenericUpdateMethodResponseRequest :: Generic UpdateMethodResponseRequest _
instance showUpdateMethodResponseRequest :: Show UpdateMethodResponseRequest where
  show = genericShow
instance decodeUpdateMethodResponseRequest :: Decode UpdateMethodResponseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMethodResponseRequest :: Encode UpdateMethodResponseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to update an existing model in an existing <a>RestApi</a> resource.</p>
newtype UpdateModelRequest = UpdateModelRequest 
  { "RestApiId'" :: (String)
  , "ModelName'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateModelRequest :: Newtype UpdateModelRequest _
derive instance repGenericUpdateModelRequest :: Generic UpdateModelRequest _
instance showUpdateModelRequest :: Show UpdateModelRequest where
  show = genericShow
instance decodeUpdateModelRequest :: Decode UpdateModelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateModelRequest :: Encode UpdateModelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Updates a <a>RequestValidator</a> of a given <a>RestApi</a>.</p>
newtype UpdateRequestValidatorRequest = UpdateRequestValidatorRequest 
  { "RestApiId'" :: (String)
  , "RequestValidatorId'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateRequestValidatorRequest :: Newtype UpdateRequestValidatorRequest _
derive instance repGenericUpdateRequestValidatorRequest :: Generic UpdateRequestValidatorRequest _
instance showUpdateRequestValidatorRequest :: Show UpdateRequestValidatorRequest where
  show = genericShow
instance decodeUpdateRequestValidatorRequest :: Decode UpdateRequestValidatorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRequestValidatorRequest :: Encode UpdateRequestValidatorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to change information about a <a>Resource</a> resource.</p>
newtype UpdateResourceRequest = UpdateResourceRequest 
  { "RestApiId'" :: (String)
  , "ResourceId'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateResourceRequest :: Newtype UpdateResourceRequest _
derive instance repGenericUpdateResourceRequest :: Generic UpdateResourceRequest _
instance showUpdateResourceRequest :: Show UpdateResourceRequest where
  show = genericShow
instance decodeUpdateResourceRequest :: Decode UpdateResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateResourceRequest :: Encode UpdateResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to update an existing <a>RestApi</a> resource in your collection.</p>
newtype UpdateRestApiRequest = UpdateRestApiRequest 
  { "RestApiId'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateRestApiRequest :: Newtype UpdateRestApiRequest _
derive instance repGenericUpdateRestApiRequest :: Generic UpdateRestApiRequest _
instance showUpdateRestApiRequest :: Show UpdateRestApiRequest where
  show = genericShow
instance decodeUpdateRestApiRequest :: Decode UpdateRestApiRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRestApiRequest :: Encode UpdateRestApiRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests API Gateway to change information about a <a>Stage</a> resource.</p>
newtype UpdateStageRequest = UpdateStageRequest 
  { "RestApiId'" :: (String)
  , "StageName'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateStageRequest :: Newtype UpdateStageRequest _
derive instance repGenericUpdateStageRequest :: Generic UpdateStageRequest _
instance showUpdateStageRequest :: Show UpdateStageRequest where
  show = genericShow
instance decodeUpdateStageRequest :: Decode UpdateStageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStageRequest :: Encode UpdateStageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The PATCH request to update a usage plan of a given plan Id.</p>
newtype UpdateUsagePlanRequest = UpdateUsagePlanRequest 
  { "UsagePlanId'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateUsagePlanRequest :: Newtype UpdateUsagePlanRequest _
derive instance repGenericUpdateUsagePlanRequest :: Generic UpdateUsagePlanRequest _
instance showUpdateUsagePlanRequest :: Show UpdateUsagePlanRequest where
  show = genericShow
instance decodeUpdateUsagePlanRequest :: Decode UpdateUsagePlanRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateUsagePlanRequest :: Encode UpdateUsagePlanRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The PATCH request to grant a temporary extension to the remaining quota of a usage plan associated with a specified API key.</p>
newtype UpdateUsageRequest = UpdateUsageRequest 
  { "UsagePlanId'" :: (String)
  , "KeyId'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateUsageRequest :: Newtype UpdateUsageRequest _
derive instance repGenericUpdateUsageRequest :: Generic UpdateUsageRequest _
instance showUpdateUsageRequest :: Show UpdateUsageRequest where
  show = genericShow
instance decodeUpdateUsageRequest :: Decode UpdateUsageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateUsageRequest :: Encode UpdateUsageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Updates an existing <a>VpcLink</a> of a specified identifier.</p>
newtype UpdateVpcLinkRequest = UpdateVpcLinkRequest 
  { "VpcLinkId'" :: (String)
  , "PatchOperations'" :: NullOrUndefined.NullOrUndefined (ListOfPatchOperation)
  }
derive instance newtypeUpdateVpcLinkRequest :: Newtype UpdateVpcLinkRequest _
derive instance repGenericUpdateVpcLinkRequest :: Generic UpdateVpcLinkRequest _
instance showUpdateVpcLinkRequest :: Show UpdateVpcLinkRequest where
  show = genericShow
instance decodeUpdateVpcLinkRequest :: Decode UpdateVpcLinkRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateVpcLinkRequest :: Encode UpdateVpcLinkRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the usage data of a usage plan.</p> <div class="remarks"/> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html">Create and Use Usage Plans</a>, <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-create-usage-plans-with-console.html#api-gateway-usage-plan-manage-usage">Manage Usage in a Usage Plan</a> </div>
newtype Usage = Usage 
  { "UsagePlanId'" :: NullOrUndefined.NullOrUndefined (String)
  , "StartDate'" :: NullOrUndefined.NullOrUndefined (String)
  , "EndDate'" :: NullOrUndefined.NullOrUndefined (String)
  , "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (MapOfKeyUsages)
  }
derive instance newtypeUsage :: Newtype Usage _
derive instance repGenericUsage :: Generic Usage _
instance showUsage :: Show Usage where
  show = genericShow
instance decodeUsage :: Decode Usage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsage :: Encode Usage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a usage plan than can specify who can assess associated API stages with specified request limits and quotas.</p> <div class="remarks"> <p>In a usage plan, you associate an API by specifying the API's Id and a stage name of the specified API. You add plan customers by adding API keys to the plan. </p> </div> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html">Create and Use Usage Plans</a> </div>
newtype UsagePlan = UsagePlan 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "ApiStages'" :: NullOrUndefined.NullOrUndefined (ListOfApiStage)
  , "Throttle'" :: NullOrUndefined.NullOrUndefined (ThrottleSettings)
  , "Quota'" :: NullOrUndefined.NullOrUndefined (QuotaSettings)
  , "ProductCode'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUsagePlan :: Newtype UsagePlan _
derive instance repGenericUsagePlan :: Generic UsagePlan _
instance showUsagePlan :: Show UsagePlan where
  show = genericShow
instance decodeUsagePlan :: Decode UsagePlan where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsagePlan :: Encode UsagePlan where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a usage plan key to identify a plan customer.</p> <div class="remarks"> <p>To associate an API stage with a selected API key in a usage plan, you must create a UsagePlanKey resource to represent the selected <a>ApiKey</a>.</p> </div>" <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html">Create and Use Usage Plans</a> </div>
newtype UsagePlanKey = UsagePlanKey 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Type'" :: NullOrUndefined.NullOrUndefined (String)
  , "Value'" :: NullOrUndefined.NullOrUndefined (String)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUsagePlanKey :: Newtype UsagePlanKey _
derive instance repGenericUsagePlanKey :: Generic UsagePlanKey _
instance showUsagePlanKey :: Show UsagePlanKey where
  show = genericShow
instance decodeUsagePlanKey :: Decode UsagePlanKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsagePlanKey :: Encode UsagePlanKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the collection of usage plan keys added to usage plans for the associated API keys and, possibly, other types of keys.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html">Create and Use Usage Plans</a> </div>
newtype UsagePlanKeys = UsagePlanKeys 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfUsagePlanKey)
  }
derive instance newtypeUsagePlanKeys :: Newtype UsagePlanKeys _
derive instance repGenericUsagePlanKeys :: Generic UsagePlanKeys _
instance showUsagePlanKeys :: Show UsagePlanKeys where
  show = genericShow
instance decodeUsagePlanKeys :: Decode UsagePlanKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsagePlanKeys :: Encode UsagePlanKeys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection of usage plans for an AWS account.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html">Create and Use Usage Plans</a> </div>
newtype UsagePlans = UsagePlans 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfUsagePlan)
  }
derive instance newtypeUsagePlans :: Newtype UsagePlans _
derive instance repGenericUsagePlans :: Generic UsagePlans _
instance showUsagePlans :: Show UsagePlans where
  show = genericShow
instance decodeUsagePlans :: Decode UsagePlans where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsagePlans :: Encode UsagePlans where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A API Gateway VPC link for a <a>RestApi</a> to access resources in an Amazon Virtual Private Cloud (VPC).</p> <div class="remarks"> <p><p>To enable access to a resource in an Amazon Virtual Private Cloud through Amazon API Gateway, you, as an API developer, create a <a>VpcLink</a> resource targeted for one or more network load balancers of the VPC and then integrate an API method with a private integration that uses the <a>VpcLink</a>. The private integration has an integration type of <code>HTTP</code> or <code>HTTP_PROXY</code> and has a connection type of <code>VPC_LINK</code>. The integration uses the <code>connectionId</code> property to identify the <a>VpcLink</a> used.</p> </p> </div>
newtype VpcLink = VpcLink 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "TargetArns'" :: NullOrUndefined.NullOrUndefined (ListOfString)
  , "Status'" :: NullOrUndefined.NullOrUndefined (VpcLinkStatus)
  , "StatusMessage'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeVpcLink :: Newtype VpcLink _
derive instance repGenericVpcLink :: Generic VpcLink _
instance showVpcLink :: Show VpcLink where
  show = genericShow
instance decodeVpcLink :: Decode VpcLink where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcLink :: Encode VpcLink where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VpcLinkStatus = VpcLinkStatus String
derive instance newtypeVpcLinkStatus :: Newtype VpcLinkStatus _
derive instance repGenericVpcLinkStatus :: Generic VpcLinkStatus _
instance showVpcLinkStatus :: Show VpcLinkStatus where
  show = genericShow
instance decodeVpcLinkStatus :: Decode VpcLinkStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcLinkStatus :: Encode VpcLinkStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The collection of VPC links under the caller's account in a region.</p> <div class="seeAlso"> <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-with-private-integration.html">Getting Started with Private Integrations</a>, <a href="http://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-private-integration.html">Set up Private Integrations</a> </div>
newtype VpcLinks = VpcLinks 
  { "Position'" :: NullOrUndefined.NullOrUndefined (String)
  , "Items'" :: NullOrUndefined.NullOrUndefined (ListOfVpcLink)
  }
derive instance newtypeVpcLinks :: Newtype VpcLinks _
derive instance repGenericVpcLinks :: Generic VpcLinks _
instance showVpcLinks :: Show VpcLinks where
  show = genericShow
instance decodeVpcLinks :: Decode VpcLinks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcLinks :: Encode VpcLinks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
