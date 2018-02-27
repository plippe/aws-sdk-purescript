

-- | <p>AWS AppSync provides API actions for creating and interacting with data sources using GraphQL from your application.</p>
module AWS.AppSync where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "AppSync" :: String


-- | <p>Creates a unique key that you can distribute to clients who are executing your API.</p>
createApiKey :: forall eff. CreateApiKeyRequest -> Aff (err :: AWS.RequestError | eff) CreateApiKeyResponse
createApiKey = AWS.request serviceName "createApiKey" 


-- | <p>Creates a <code>DataSource</code> object.</p>
createDataSource :: forall eff. CreateDataSourceRequest -> Aff (err :: AWS.RequestError | eff) CreateDataSourceResponse
createDataSource = AWS.request serviceName "createDataSource" 


-- | <p>Creates a <code>GraphqlApi</code> object.</p>
createGraphqlApi :: forall eff. CreateGraphqlApiRequest -> Aff (err :: AWS.RequestError | eff) CreateGraphqlApiResponse
createGraphqlApi = AWS.request serviceName "createGraphqlApi" 


-- | <p>Creates a <code>Resolver</code> object.</p> <p>A resolver converts incoming requests into a format that a data source can understand and converts the data source's responses into GraphQL.</p>
createResolver :: forall eff. CreateResolverRequest -> Aff (err :: AWS.RequestError | eff) CreateResolverResponse
createResolver = AWS.request serviceName "createResolver" 


-- | <p>Creates a <code>Type</code> object.</p>
createType :: forall eff. CreateTypeRequest -> Aff (err :: AWS.RequestError | eff) CreateTypeResponse
createType = AWS.request serviceName "createType" 


-- | <p>Deletes an API key.</p>
deleteApiKey :: forall eff. DeleteApiKeyRequest -> Aff (err :: AWS.RequestError | eff) DeleteApiKeyResponse
deleteApiKey = AWS.request serviceName "deleteApiKey" 


-- | <p>Deletes a <code>DataSource</code> object.</p>
deleteDataSource :: forall eff. DeleteDataSourceRequest -> Aff (err :: AWS.RequestError | eff) DeleteDataSourceResponse
deleteDataSource = AWS.request serviceName "deleteDataSource" 


-- | <p>Deletes a <code>GraphqlApi</code> object.</p>
deleteGraphqlApi :: forall eff. DeleteGraphqlApiRequest -> Aff (err :: AWS.RequestError | eff) DeleteGraphqlApiResponse
deleteGraphqlApi = AWS.request serviceName "deleteGraphqlApi" 


-- | <p>Deletes a <code>Resolver</code> object.</p>
deleteResolver :: forall eff. DeleteResolverRequest -> Aff (err :: AWS.RequestError | eff) DeleteResolverResponse
deleteResolver = AWS.request serviceName "deleteResolver" 


-- | <p>Deletes a <code>Type</code> object.</p>
deleteType :: forall eff. DeleteTypeRequest -> Aff (err :: AWS.RequestError | eff) DeleteTypeResponse
deleteType = AWS.request serviceName "deleteType" 


-- | <p>Retrieves a <code>DataSource</code> object.</p>
getDataSource :: forall eff. GetDataSourceRequest -> Aff (err :: AWS.RequestError | eff) GetDataSourceResponse
getDataSource = AWS.request serviceName "getDataSource" 


-- | <p>Retrieves a <code>GraphqlApi</code> object.</p>
getGraphqlApi :: forall eff. GetGraphqlApiRequest -> Aff (err :: AWS.RequestError | eff) GetGraphqlApiResponse
getGraphqlApi = AWS.request serviceName "getGraphqlApi" 


-- | <p>Retrieves the introspection schema for a GraphQL API.</p>
getIntrospectionSchema :: forall eff. GetIntrospectionSchemaRequest -> Aff (err :: AWS.RequestError | eff) GetIntrospectionSchemaResponse
getIntrospectionSchema = AWS.request serviceName "getIntrospectionSchema" 


-- | <p>Retrieves a <code>Resolver</code> object.</p>
getResolver :: forall eff. GetResolverRequest -> Aff (err :: AWS.RequestError | eff) GetResolverResponse
getResolver = AWS.request serviceName "getResolver" 


-- | <p>Retrieves the current status of a schema creation operation.</p>
getSchemaCreationStatus :: forall eff. GetSchemaCreationStatusRequest -> Aff (err :: AWS.RequestError | eff) GetSchemaCreationStatusResponse
getSchemaCreationStatus = AWS.request serviceName "getSchemaCreationStatus" 


-- | <p>Retrieves a <code>Type</code> object.</p>
getType :: forall eff. GetTypeRequest -> Aff (err :: AWS.RequestError | eff) GetTypeResponse
getType = AWS.request serviceName "getType" 


-- | <p>Lists the API keys for a given API.</p>
listApiKeys :: forall eff. ListApiKeysRequest -> Aff (err :: AWS.RequestError | eff) ListApiKeysResponse
listApiKeys = AWS.request serviceName "listApiKeys" 


-- | <p>Lists the data sources for a given API.</p>
listDataSources :: forall eff. ListDataSourcesRequest -> Aff (err :: AWS.RequestError | eff) ListDataSourcesResponse
listDataSources = AWS.request serviceName "listDataSources" 


-- | <p>Lists your GraphQL APIs.</p>
listGraphqlApis :: forall eff. ListGraphqlApisRequest -> Aff (err :: AWS.RequestError | eff) ListGraphqlApisResponse
listGraphqlApis = AWS.request serviceName "listGraphqlApis" 


-- | <p>Lists the resolvers for a given API and type.</p>
listResolvers :: forall eff. ListResolversRequest -> Aff (err :: AWS.RequestError | eff) ListResolversResponse
listResolvers = AWS.request serviceName "listResolvers" 


-- | <p>Lists the types for a given API.</p>
listTypes :: forall eff. ListTypesRequest -> Aff (err :: AWS.RequestError | eff) ListTypesResponse
listTypes = AWS.request serviceName "listTypes" 


-- | <p>Adds a new schema to your GraphQL API.</p> <p>This operation is asynchronous. Use to determine when it has completed.</p>
startSchemaCreation :: forall eff. StartSchemaCreationRequest -> Aff (err :: AWS.RequestError | eff) StartSchemaCreationResponse
startSchemaCreation = AWS.request serviceName "startSchemaCreation" 


-- | <p>Updates an API key.</p>
updateApiKey :: forall eff. UpdateApiKeyRequest -> Aff (err :: AWS.RequestError | eff) UpdateApiKeyResponse
updateApiKey = AWS.request serviceName "updateApiKey" 


-- | <p>Updates a <code>DataSource</code> object.</p>
updateDataSource :: forall eff. UpdateDataSourceRequest -> Aff (err :: AWS.RequestError | eff) UpdateDataSourceResponse
updateDataSource = AWS.request serviceName "updateDataSource" 


-- | <p>Updates a <code>GraphqlApi</code> object.</p>
updateGraphqlApi :: forall eff. UpdateGraphqlApiRequest -> Aff (err :: AWS.RequestError | eff) UpdateGraphqlApiResponse
updateGraphqlApi = AWS.request serviceName "updateGraphqlApi" 


-- | <p>Updates a <code>Resolver</code> object.</p>
updateResolver :: forall eff. UpdateResolverRequest -> Aff (err :: AWS.RequestError | eff) UpdateResolverResponse
updateResolver = AWS.request serviceName "updateResolver" 


-- | <p>Updates a <code>Type</code> object.</p>
updateType :: forall eff. UpdateTypeRequest -> Aff (err :: AWS.RequestError | eff) UpdateTypeResponse
updateType = AWS.request serviceName "updateType" 


-- | <p>Describes an API key.</p>
newtype ApiKey = ApiKey 
  { "Id'" :: NullOrUndefined (String)
  , "Description'" :: NullOrUndefined (String)
  , "Expires'" :: NullOrUndefined (Number)
  }
derive instance newtypeApiKey :: Newtype ApiKey _


-- | <p>The API key exceeded a limit. Try your request again.</p>
newtype ApiKeyLimitExceededException = ApiKeyLimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeApiKeyLimitExceededException :: Newtype ApiKeyLimitExceededException _


-- | <p>The API key expiration must be set to a value between 1 and 365 days.</p>
newtype ApiKeyValidityOutOfBoundsException = ApiKeyValidityOutOfBoundsException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeApiKeyValidityOutOfBoundsException :: Newtype ApiKeyValidityOutOfBoundsException _


newtype ApiKeys = ApiKeys (Array ApiKey)
derive instance newtypeApiKeys :: Newtype ApiKeys _


-- | <p>The GraphQL API exceeded a limit. Try your request again.</p>
newtype ApiLimitExceededException = ApiLimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeApiLimitExceededException :: Newtype ApiLimitExceededException _


newtype AuthenticationType = AuthenticationType String
derive instance newtypeAuthenticationType :: Newtype AuthenticationType _


-- | <p>The request is not well formed. For example, a value is invalid or a required field is missing. Check the field values, and try again. </p>
newtype BadRequestException = BadRequestException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _


-- | <p>Another modification is being made. That modification must complete before you can make your change. </p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _


newtype CreateApiKeyRequest = CreateApiKeyRequest 
  { "ApiId'" :: (String)
  , "Description'" :: NullOrUndefined (String)
  , "Expires'" :: NullOrUndefined (Number)
  }
derive instance newtypeCreateApiKeyRequest :: Newtype CreateApiKeyRequest _


newtype CreateApiKeyResponse = CreateApiKeyResponse 
  { "ApiKey'" :: NullOrUndefined (ApiKey)
  }
derive instance newtypeCreateApiKeyResponse :: Newtype CreateApiKeyResponse _


newtype CreateDataSourceRequest = CreateDataSourceRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (ResourceName)
  , "Description'" :: NullOrUndefined (String)
  , "Type'" :: (DataSourceType)
  , "ServiceRoleArn'" :: NullOrUndefined (String)
  , "DynamodbConfig'" :: NullOrUndefined (DynamodbDataSourceConfig)
  , "LambdaConfig'" :: NullOrUndefined (LambdaDataSourceConfig)
  , "ElasticsearchConfig'" :: NullOrUndefined (ElasticsearchDataSourceConfig)
  }
derive instance newtypeCreateDataSourceRequest :: Newtype CreateDataSourceRequest _


newtype CreateDataSourceResponse = CreateDataSourceResponse 
  { "DataSource'" :: NullOrUndefined (DataSource)
  }
derive instance newtypeCreateDataSourceResponse :: Newtype CreateDataSourceResponse _


newtype CreateGraphqlApiRequest = CreateGraphqlApiRequest 
  { "Name'" :: (String)
  , "AuthenticationType'" :: (AuthenticationType)
  , "UserPoolConfig'" :: NullOrUndefined (UserPoolConfig)
  }
derive instance newtypeCreateGraphqlApiRequest :: Newtype CreateGraphqlApiRequest _


newtype CreateGraphqlApiResponse = CreateGraphqlApiResponse 
  { "GraphqlApi'" :: NullOrUndefined (GraphqlApi)
  }
derive instance newtypeCreateGraphqlApiResponse :: Newtype CreateGraphqlApiResponse _


newtype CreateResolverRequest = CreateResolverRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "FieldName'" :: (ResourceName)
  , "DataSourceName'" :: (ResourceName)
  , "RequestMappingTemplate'" :: (MappingTemplate)
  , "ResponseMappingTemplate'" :: NullOrUndefined (MappingTemplate)
  }
derive instance newtypeCreateResolverRequest :: Newtype CreateResolverRequest _


newtype CreateResolverResponse = CreateResolverResponse 
  { "Resolver'" :: NullOrUndefined (Resolver)
  }
derive instance newtypeCreateResolverResponse :: Newtype CreateResolverResponse _


newtype CreateTypeRequest = CreateTypeRequest 
  { "ApiId'" :: (String)
  , "Definition'" :: (String)
  , "Format'" :: (TypeDefinitionFormat)
  }
derive instance newtypeCreateTypeRequest :: Newtype CreateTypeRequest _


newtype CreateTypeResponse = CreateTypeResponse 
  { "Type'" :: NullOrUndefined (Type)
  }
derive instance newtypeCreateTypeResponse :: Newtype CreateTypeResponse _


-- | <p>Describes a data source.</p>
newtype DataSource = DataSource 
  { "DataSourceArn'" :: NullOrUndefined (String)
  , "Name'" :: NullOrUndefined (ResourceName)
  , "Description'" :: NullOrUndefined (String)
  , "Type'" :: NullOrUndefined (DataSourceType)
  , "ServiceRoleArn'" :: NullOrUndefined (String)
  , "DynamodbConfig'" :: NullOrUndefined (DynamodbDataSourceConfig)
  , "LambdaConfig'" :: NullOrUndefined (LambdaDataSourceConfig)
  , "ElasticsearchConfig'" :: NullOrUndefined (ElasticsearchDataSourceConfig)
  }
derive instance newtypeDataSource :: Newtype DataSource _


newtype DataSourceType = DataSourceType String
derive instance newtypeDataSourceType :: Newtype DataSourceType _


newtype DataSources = DataSources (Array DataSource)
derive instance newtypeDataSources :: Newtype DataSources _


newtype DefaultAction = DefaultAction String
derive instance newtypeDefaultAction :: Newtype DefaultAction _


newtype DeleteApiKeyRequest = DeleteApiKeyRequest 
  { "ApiId'" :: (String)
  , "Id'" :: (String)
  }
derive instance newtypeDeleteApiKeyRequest :: Newtype DeleteApiKeyRequest _


newtype DeleteApiKeyResponse = DeleteApiKeyResponse 
  { 
  }
derive instance newtypeDeleteApiKeyResponse :: Newtype DeleteApiKeyResponse _


newtype DeleteDataSourceRequest = DeleteDataSourceRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (ResourceName)
  }
derive instance newtypeDeleteDataSourceRequest :: Newtype DeleteDataSourceRequest _


newtype DeleteDataSourceResponse = DeleteDataSourceResponse 
  { 
  }
derive instance newtypeDeleteDataSourceResponse :: Newtype DeleteDataSourceResponse _


newtype DeleteGraphqlApiRequest = DeleteGraphqlApiRequest 
  { "ApiId'" :: (String)
  }
derive instance newtypeDeleteGraphqlApiRequest :: Newtype DeleteGraphqlApiRequest _


newtype DeleteGraphqlApiResponse = DeleteGraphqlApiResponse 
  { 
  }
derive instance newtypeDeleteGraphqlApiResponse :: Newtype DeleteGraphqlApiResponse _


newtype DeleteResolverRequest = DeleteResolverRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "FieldName'" :: (ResourceName)
  }
derive instance newtypeDeleteResolverRequest :: Newtype DeleteResolverRequest _


newtype DeleteResolverResponse = DeleteResolverResponse 
  { 
  }
derive instance newtypeDeleteResolverResponse :: Newtype DeleteResolverResponse _


newtype DeleteTypeRequest = DeleteTypeRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  }
derive instance newtypeDeleteTypeRequest :: Newtype DeleteTypeRequest _


newtype DeleteTypeResponse = DeleteTypeResponse 
  { 
  }
derive instance newtypeDeleteTypeResponse :: Newtype DeleteTypeResponse _


-- | <p>Describes a DynamoDB data source configuration.</p>
newtype DynamodbDataSourceConfig = DynamodbDataSourceConfig 
  { "TableName'" :: (String)
  , "AwsRegion'" :: (String)
  , "UseCallerCredentials'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeDynamodbDataSourceConfig :: Newtype DynamodbDataSourceConfig _


-- | <p>Describes an Elasticsearch data source configuration.</p>
newtype ElasticsearchDataSourceConfig = ElasticsearchDataSourceConfig 
  { "Endpoint'" :: (String)
  , "AwsRegion'" :: (String)
  }
derive instance newtypeElasticsearchDataSourceConfig :: Newtype ElasticsearchDataSourceConfig _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype GetDataSourceRequest = GetDataSourceRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (ResourceName)
  }
derive instance newtypeGetDataSourceRequest :: Newtype GetDataSourceRequest _


newtype GetDataSourceResponse = GetDataSourceResponse 
  { "DataSource'" :: NullOrUndefined (DataSource)
  }
derive instance newtypeGetDataSourceResponse :: Newtype GetDataSourceResponse _


newtype GetGraphqlApiRequest = GetGraphqlApiRequest 
  { "ApiId'" :: (String)
  }
derive instance newtypeGetGraphqlApiRequest :: Newtype GetGraphqlApiRequest _


newtype GetGraphqlApiResponse = GetGraphqlApiResponse 
  { "GraphqlApi'" :: NullOrUndefined (GraphqlApi)
  }
derive instance newtypeGetGraphqlApiResponse :: Newtype GetGraphqlApiResponse _


newtype GetIntrospectionSchemaRequest = GetIntrospectionSchemaRequest 
  { "ApiId'" :: (String)
  , "Format'" :: (OutputType)
  }
derive instance newtypeGetIntrospectionSchemaRequest :: Newtype GetIntrospectionSchemaRequest _


newtype GetIntrospectionSchemaResponse = GetIntrospectionSchemaResponse 
  { "Schema'" :: NullOrUndefined (String)
  }
derive instance newtypeGetIntrospectionSchemaResponse :: Newtype GetIntrospectionSchemaResponse _


newtype GetResolverRequest = GetResolverRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "FieldName'" :: (ResourceName)
  }
derive instance newtypeGetResolverRequest :: Newtype GetResolverRequest _


newtype GetResolverResponse = GetResolverResponse 
  { "Resolver'" :: NullOrUndefined (Resolver)
  }
derive instance newtypeGetResolverResponse :: Newtype GetResolverResponse _


newtype GetSchemaCreationStatusRequest = GetSchemaCreationStatusRequest 
  { "ApiId'" :: (String)
  }
derive instance newtypeGetSchemaCreationStatusRequest :: Newtype GetSchemaCreationStatusRequest _


newtype GetSchemaCreationStatusResponse = GetSchemaCreationStatusResponse 
  { "Status'" :: NullOrUndefined (SchemaStatus)
  , "Details'" :: NullOrUndefined (String)
  }
derive instance newtypeGetSchemaCreationStatusResponse :: Newtype GetSchemaCreationStatusResponse _


newtype GetTypeRequest = GetTypeRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "Format'" :: (TypeDefinitionFormat)
  }
derive instance newtypeGetTypeRequest :: Newtype GetTypeRequest _


newtype GetTypeResponse = GetTypeResponse 
  { "Type'" :: NullOrUndefined (Type)
  }
derive instance newtypeGetTypeResponse :: Newtype GetTypeResponse _


-- | <p>The GraphQL schema is not valid.</p>
newtype GraphQLSchemaException = GraphQLSchemaException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeGraphQLSchemaException :: Newtype GraphQLSchemaException _


-- | <p>Describes a GraphQL API.</p>
newtype GraphqlApi = GraphqlApi 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "ApiId'" :: NullOrUndefined (String)
  , "AuthenticationType'" :: NullOrUndefined (AuthenticationType)
  , "UserPoolConfig'" :: NullOrUndefined (UserPoolConfig)
  , "Arn'" :: NullOrUndefined (String)
  , "Uris'" :: NullOrUndefined (MapOfStringToString)
  }
derive instance newtypeGraphqlApi :: Newtype GraphqlApi _


newtype GraphqlApis = GraphqlApis (Array GraphqlApi)
derive instance newtypeGraphqlApis :: Newtype GraphqlApis _


-- | <p>An internal AWS AppSync error occurred. Try your request again.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInternalFailureException :: Newtype InternalFailureException _


-- | <p>Describes a Lambda data source configuration.</p>
newtype LambdaDataSourceConfig = LambdaDataSourceConfig 
  { "LambdaFunctionArn'" :: (String)
  }
derive instance newtypeLambdaDataSourceConfig :: Newtype LambdaDataSourceConfig _


-- | <p>The request exceeded a limit. Try your request again.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListApiKeysRequest = ListApiKeysRequest 
  { "ApiId'" :: (String)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListApiKeysRequest :: Newtype ListApiKeysRequest _


newtype ListApiKeysResponse = ListApiKeysResponse 
  { "ApiKeys'" :: NullOrUndefined (ApiKeys)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListApiKeysResponse :: Newtype ListApiKeysResponse _


newtype ListDataSourcesRequest = ListDataSourcesRequest 
  { "ApiId'" :: (String)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListDataSourcesRequest :: Newtype ListDataSourcesRequest _


newtype ListDataSourcesResponse = ListDataSourcesResponse 
  { "DataSources'" :: NullOrUndefined (DataSources)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListDataSourcesResponse :: Newtype ListDataSourcesResponse _


newtype ListGraphqlApisRequest = ListGraphqlApisRequest 
  { "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListGraphqlApisRequest :: Newtype ListGraphqlApisRequest _


newtype ListGraphqlApisResponse = ListGraphqlApisResponse 
  { "GraphqlApis'" :: NullOrUndefined (GraphqlApis)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListGraphqlApisResponse :: Newtype ListGraphqlApisResponse _


newtype ListResolversRequest = ListResolversRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (String)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListResolversRequest :: Newtype ListResolversRequest _


newtype ListResolversResponse = ListResolversResponse 
  { "Resolvers'" :: NullOrUndefined (Resolvers)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListResolversResponse :: Newtype ListResolversResponse _


newtype ListTypesRequest = ListTypesRequest 
  { "ApiId'" :: (String)
  , "Format'" :: (TypeDefinitionFormat)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListTypesRequest :: Newtype ListTypesRequest _


newtype ListTypesResponse = ListTypesResponse 
  { "Types'" :: NullOrUndefined (TypeList)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListTypesResponse :: Newtype ListTypesResponse _


newtype MapOfStringToString = MapOfStringToString (Map String String)
derive instance newtypeMapOfStringToString :: Newtype MapOfStringToString _


newtype MappingTemplate = MappingTemplate String
derive instance newtypeMappingTemplate :: Newtype MappingTemplate _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | <p>The resource specified in the request was not found. Check the resource and try again.</p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


newtype OutputType = OutputType String
derive instance newtypeOutputType :: Newtype OutputType _


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _


-- | <p>Describes a resolver.</p>
newtype Resolver = Resolver 
  { "TypeName'" :: NullOrUndefined (ResourceName)
  , "FieldName'" :: NullOrUndefined (ResourceName)
  , "DataSourceName'" :: NullOrUndefined (ResourceName)
  , "ResolverArn'" :: NullOrUndefined (String)
  , "RequestMappingTemplate'" :: NullOrUndefined (MappingTemplate)
  , "ResponseMappingTemplate'" :: NullOrUndefined (MappingTemplate)
  }
derive instance newtypeResolver :: Newtype Resolver _


newtype Resolvers = Resolvers (Array Resolver)
derive instance newtypeResolvers :: Newtype Resolvers _


newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _


newtype SchemaStatus = SchemaStatus String
derive instance newtypeSchemaStatus :: Newtype SchemaStatus _


newtype StartSchemaCreationRequest = StartSchemaCreationRequest 
  { "ApiId'" :: (String)
  , "Definition'" :: (String)
  }
derive instance newtypeStartSchemaCreationRequest :: Newtype StartSchemaCreationRequest _


newtype StartSchemaCreationResponse = StartSchemaCreationResponse 
  { "Status'" :: NullOrUndefined (SchemaStatus)
  }
derive instance newtypeStartSchemaCreationResponse :: Newtype StartSchemaCreationResponse _


-- | <p>Describes a type.</p>
newtype Type = Type 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "Description'" :: NullOrUndefined (String)
  , "Arn'" :: NullOrUndefined (String)
  , "Definition'" :: NullOrUndefined (String)
  , "Format'" :: NullOrUndefined (TypeDefinitionFormat)
  }
derive instance newtypeType :: Newtype Type _


newtype TypeDefinitionFormat = TypeDefinitionFormat String
derive instance newtypeTypeDefinitionFormat :: Newtype TypeDefinitionFormat _


newtype TypeList = TypeList (Array Type)
derive instance newtypeTypeList :: Newtype TypeList _


-- | <p>You are not authorized to perform this operation.</p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeUnauthorizedException :: Newtype UnauthorizedException _


newtype UpdateApiKeyRequest = UpdateApiKeyRequest 
  { "ApiId'" :: (String)
  , "Id'" :: (String)
  , "Description'" :: NullOrUndefined (String)
  , "Expires'" :: NullOrUndefined (Number)
  }
derive instance newtypeUpdateApiKeyRequest :: Newtype UpdateApiKeyRequest _


newtype UpdateApiKeyResponse = UpdateApiKeyResponse 
  { "ApiKey'" :: NullOrUndefined (ApiKey)
  }
derive instance newtypeUpdateApiKeyResponse :: Newtype UpdateApiKeyResponse _


newtype UpdateDataSourceRequest = UpdateDataSourceRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (ResourceName)
  , "Description'" :: NullOrUndefined (String)
  , "Type'" :: (DataSourceType)
  , "ServiceRoleArn'" :: NullOrUndefined (String)
  , "DynamodbConfig'" :: NullOrUndefined (DynamodbDataSourceConfig)
  , "LambdaConfig'" :: NullOrUndefined (LambdaDataSourceConfig)
  , "ElasticsearchConfig'" :: NullOrUndefined (ElasticsearchDataSourceConfig)
  }
derive instance newtypeUpdateDataSourceRequest :: Newtype UpdateDataSourceRequest _


newtype UpdateDataSourceResponse = UpdateDataSourceResponse 
  { "DataSource'" :: NullOrUndefined (DataSource)
  }
derive instance newtypeUpdateDataSourceResponse :: Newtype UpdateDataSourceResponse _


newtype UpdateGraphqlApiRequest = UpdateGraphqlApiRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (String)
  , "AuthenticationType'" :: NullOrUndefined (AuthenticationType)
  , "UserPoolConfig'" :: NullOrUndefined (UserPoolConfig)
  }
derive instance newtypeUpdateGraphqlApiRequest :: Newtype UpdateGraphqlApiRequest _


newtype UpdateGraphqlApiResponse = UpdateGraphqlApiResponse 
  { "GraphqlApi'" :: NullOrUndefined (GraphqlApi)
  }
derive instance newtypeUpdateGraphqlApiResponse :: Newtype UpdateGraphqlApiResponse _


newtype UpdateResolverRequest = UpdateResolverRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "FieldName'" :: (ResourceName)
  , "DataSourceName'" :: (ResourceName)
  , "RequestMappingTemplate'" :: (MappingTemplate)
  , "ResponseMappingTemplate'" :: NullOrUndefined (MappingTemplate)
  }
derive instance newtypeUpdateResolverRequest :: Newtype UpdateResolverRequest _


newtype UpdateResolverResponse = UpdateResolverResponse 
  { "Resolver'" :: NullOrUndefined (Resolver)
  }
derive instance newtypeUpdateResolverResponse :: Newtype UpdateResolverResponse _


newtype UpdateTypeRequest = UpdateTypeRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "Definition'" :: NullOrUndefined (String)
  , "Format'" :: (TypeDefinitionFormat)
  }
derive instance newtypeUpdateTypeRequest :: Newtype UpdateTypeRequest _


newtype UpdateTypeResponse = UpdateTypeResponse 
  { "Type'" :: NullOrUndefined (Type)
  }
derive instance newtypeUpdateTypeResponse :: Newtype UpdateTypeResponse _


-- | <p>Describes an Amazon Cognito User Pool configuration.</p>
newtype UserPoolConfig = UserPoolConfig 
  { "UserPoolId'" :: (String)
  , "AwsRegion'" :: (String)
  , "DefaultAction'" :: (DefaultAction)
  , "AppIdClientRegex'" :: NullOrUndefined (String)
  }
derive instance newtypeUserPoolConfig :: Newtype UserPoolConfig _
