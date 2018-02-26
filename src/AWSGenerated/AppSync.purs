

-- | <p>AWS AppSync provides API actions for creating and interacting with data sources using GraphQL from your application.</p>
module AWS.AppSync where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "AppSync" :: String


-- | <p>Creates a unique key that you can distribute to clients who are executing your API.</p>
createApiKey :: forall eff. CreateApiKeyRequest -> Aff (err :: AWS.RequestError | eff) CreateApiKeyResponse
createApiKey = AWS.request serviceName "CreateApiKey" 


-- | <p>Creates a <code>DataSource</code> object.</p>
createDataSource :: forall eff. CreateDataSourceRequest -> Aff (err :: AWS.RequestError | eff) CreateDataSourceResponse
createDataSource = AWS.request serviceName "CreateDataSource" 


-- | <p>Creates a <code>GraphqlApi</code> object.</p>
createGraphqlApi :: forall eff. CreateGraphqlApiRequest -> Aff (err :: AWS.RequestError | eff) CreateGraphqlApiResponse
createGraphqlApi = AWS.request serviceName "CreateGraphqlApi" 


-- | <p>Creates a <code>Resolver</code> object.</p> <p>A resolver converts incoming requests into a format that a data source can understand and converts the data source's responses into GraphQL.</p>
createResolver :: forall eff. CreateResolverRequest -> Aff (err :: AWS.RequestError | eff) CreateResolverResponse
createResolver = AWS.request serviceName "CreateResolver" 


-- | <p>Creates a <code>Type</code> object.</p>
createType :: forall eff. CreateTypeRequest -> Aff (err :: AWS.RequestError | eff) CreateTypeResponse
createType = AWS.request serviceName "CreateType" 


-- | <p>Deletes an API key.</p>
deleteApiKey :: forall eff. DeleteApiKeyRequest -> Aff (err :: AWS.RequestError | eff) DeleteApiKeyResponse
deleteApiKey = AWS.request serviceName "DeleteApiKey" 


-- | <p>Deletes a <code>DataSource</code> object.</p>
deleteDataSource :: forall eff. DeleteDataSourceRequest -> Aff (err :: AWS.RequestError | eff) DeleteDataSourceResponse
deleteDataSource = AWS.request serviceName "DeleteDataSource" 


-- | <p>Deletes a <code>GraphqlApi</code> object.</p>
deleteGraphqlApi :: forall eff. DeleteGraphqlApiRequest -> Aff (err :: AWS.RequestError | eff) DeleteGraphqlApiResponse
deleteGraphqlApi = AWS.request serviceName "DeleteGraphqlApi" 


-- | <p>Deletes a <code>Resolver</code> object.</p>
deleteResolver :: forall eff. DeleteResolverRequest -> Aff (err :: AWS.RequestError | eff) DeleteResolverResponse
deleteResolver = AWS.request serviceName "DeleteResolver" 


-- | <p>Deletes a <code>Type</code> object.</p>
deleteType :: forall eff. DeleteTypeRequest -> Aff (err :: AWS.RequestError | eff) DeleteTypeResponse
deleteType = AWS.request serviceName "DeleteType" 


-- | <p>Retrieves a <code>DataSource</code> object.</p>
getDataSource :: forall eff. GetDataSourceRequest -> Aff (err :: AWS.RequestError | eff) GetDataSourceResponse
getDataSource = AWS.request serviceName "GetDataSource" 


-- | <p>Retrieves a <code>GraphqlApi</code> object.</p>
getGraphqlApi :: forall eff. GetGraphqlApiRequest -> Aff (err :: AWS.RequestError | eff) GetGraphqlApiResponse
getGraphqlApi = AWS.request serviceName "GetGraphqlApi" 


-- | <p>Retrieves the introspection schema for a GraphQL API.</p>
getIntrospectionSchema :: forall eff. GetIntrospectionSchemaRequest -> Aff (err :: AWS.RequestError | eff) GetIntrospectionSchemaResponse
getIntrospectionSchema = AWS.request serviceName "GetIntrospectionSchema" 


-- | <p>Retrieves a <code>Resolver</code> object.</p>
getResolver :: forall eff. GetResolverRequest -> Aff (err :: AWS.RequestError | eff) GetResolverResponse
getResolver = AWS.request serviceName "GetResolver" 


-- | <p>Retrieves the current status of a schema creation operation.</p>
getSchemaCreationStatus :: forall eff. GetSchemaCreationStatusRequest -> Aff (err :: AWS.RequestError | eff) GetSchemaCreationStatusResponse
getSchemaCreationStatus = AWS.request serviceName "GetSchemaCreationStatus" 


-- | <p>Retrieves a <code>Type</code> object.</p>
getType :: forall eff. GetTypeRequest -> Aff (err :: AWS.RequestError | eff) GetTypeResponse
getType = AWS.request serviceName "GetType" 


-- | <p>Lists the API keys for a given API.</p>
listApiKeys :: forall eff. ListApiKeysRequest -> Aff (err :: AWS.RequestError | eff) ListApiKeysResponse
listApiKeys = AWS.request serviceName "ListApiKeys" 


-- | <p>Lists the data sources for a given API.</p>
listDataSources :: forall eff. ListDataSourcesRequest -> Aff (err :: AWS.RequestError | eff) ListDataSourcesResponse
listDataSources = AWS.request serviceName "ListDataSources" 


-- | <p>Lists your GraphQL APIs.</p>
listGraphqlApis :: forall eff. ListGraphqlApisRequest -> Aff (err :: AWS.RequestError | eff) ListGraphqlApisResponse
listGraphqlApis = AWS.request serviceName "ListGraphqlApis" 


-- | <p>Lists the resolvers for a given API and type.</p>
listResolvers :: forall eff. ListResolversRequest -> Aff (err :: AWS.RequestError | eff) ListResolversResponse
listResolvers = AWS.request serviceName "ListResolvers" 


-- | <p>Lists the types for a given API.</p>
listTypes :: forall eff. ListTypesRequest -> Aff (err :: AWS.RequestError | eff) ListTypesResponse
listTypes = AWS.request serviceName "ListTypes" 


-- | <p>Adds a new schema to your GraphQL API.</p> <p>This operation is asynchronous. Use to determine when it has completed.</p>
startSchemaCreation :: forall eff. StartSchemaCreationRequest -> Aff (err :: AWS.RequestError | eff) StartSchemaCreationResponse
startSchemaCreation = AWS.request serviceName "StartSchemaCreation" 


-- | <p>Updates an API key.</p>
updateApiKey :: forall eff. UpdateApiKeyRequest -> Aff (err :: AWS.RequestError | eff) UpdateApiKeyResponse
updateApiKey = AWS.request serviceName "UpdateApiKey" 


-- | <p>Updates a <code>DataSource</code> object.</p>
updateDataSource :: forall eff. UpdateDataSourceRequest -> Aff (err :: AWS.RequestError | eff) UpdateDataSourceResponse
updateDataSource = AWS.request serviceName "UpdateDataSource" 


-- | <p>Updates a <code>GraphqlApi</code> object.</p>
updateGraphqlApi :: forall eff. UpdateGraphqlApiRequest -> Aff (err :: AWS.RequestError | eff) UpdateGraphqlApiResponse
updateGraphqlApi = AWS.request serviceName "UpdateGraphqlApi" 


-- | <p>Updates a <code>Resolver</code> object.</p>
updateResolver :: forall eff. UpdateResolverRequest -> Aff (err :: AWS.RequestError | eff) UpdateResolverResponse
updateResolver = AWS.request serviceName "UpdateResolver" 


-- | <p>Updates a <code>Type</code> object.</p>
updateType :: forall eff. UpdateTypeRequest -> Aff (err :: AWS.RequestError | eff) UpdateTypeResponse
updateType = AWS.request serviceName "UpdateType" 


-- | <p>Describes an API key.</p>
newtype ApiKey = ApiKey 
  { "Id'" :: NullOrUndefined (String)
  , "Description'" :: NullOrUndefined (String)
  , "Expires'" :: NullOrUndefined (Number)
  }


-- | <p>The API key exceeded a limit. Try your request again.</p>
newtype ApiKeyLimitExceededException = ApiKeyLimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>The API key expiration must be set to a value between 1 and 365 days.</p>
newtype ApiKeyValidityOutOfBoundsException = ApiKeyValidityOutOfBoundsException 
  { "Message'" :: NullOrUndefined (String)
  }


newtype ApiKeys = ApiKeys (Array ApiKey)


-- | <p>The GraphQL API exceeded a limit. Try your request again.</p>
newtype ApiLimitExceededException = ApiLimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }


newtype AuthenticationType = AuthenticationType String


-- | <p>The request is not well formed. For example, a value is invalid or a required field is missing. Check the field values, and try again. </p>
newtype BadRequestException = BadRequestException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Another modification is being made. That modification must complete before you can make your change. </p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype CreateApiKeyRequest = CreateApiKeyRequest 
  { "ApiId'" :: (String)
  , "Description'" :: NullOrUndefined (String)
  , "Expires'" :: NullOrUndefined (Number)
  }


newtype CreateApiKeyResponse = CreateApiKeyResponse 
  { "ApiKey'" :: NullOrUndefined (ApiKey)
  }


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


newtype CreateDataSourceResponse = CreateDataSourceResponse 
  { "DataSource'" :: NullOrUndefined (DataSource)
  }


newtype CreateGraphqlApiRequest = CreateGraphqlApiRequest 
  { "Name'" :: (String)
  , "AuthenticationType'" :: (AuthenticationType)
  , "UserPoolConfig'" :: NullOrUndefined (UserPoolConfig)
  }


newtype CreateGraphqlApiResponse = CreateGraphqlApiResponse 
  { "GraphqlApi'" :: NullOrUndefined (GraphqlApi)
  }


newtype CreateResolverRequest = CreateResolverRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "FieldName'" :: (ResourceName)
  , "DataSourceName'" :: (ResourceName)
  , "RequestMappingTemplate'" :: (MappingTemplate)
  , "ResponseMappingTemplate'" :: NullOrUndefined (MappingTemplate)
  }


newtype CreateResolverResponse = CreateResolverResponse 
  { "Resolver'" :: NullOrUndefined (Resolver)
  }


newtype CreateTypeRequest = CreateTypeRequest 
  { "ApiId'" :: (String)
  , "Definition'" :: (String)
  , "Format'" :: (TypeDefinitionFormat)
  }


newtype CreateTypeResponse = CreateTypeResponse 
  { "Type'" :: NullOrUndefined (Type)
  }


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


newtype DataSourceType = DataSourceType String


newtype DataSources = DataSources (Array DataSource)


newtype DefaultAction = DefaultAction String


newtype DeleteApiKeyRequest = DeleteApiKeyRequest 
  { "ApiId'" :: (String)
  , "Id'" :: (String)
  }


newtype DeleteApiKeyResponse = DeleteApiKeyResponse 
  { 
  }


newtype DeleteDataSourceRequest = DeleteDataSourceRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (ResourceName)
  }


newtype DeleteDataSourceResponse = DeleteDataSourceResponse 
  { 
  }


newtype DeleteGraphqlApiRequest = DeleteGraphqlApiRequest 
  { "ApiId'" :: (String)
  }


newtype DeleteGraphqlApiResponse = DeleteGraphqlApiResponse 
  { 
  }


newtype DeleteResolverRequest = DeleteResolverRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "FieldName'" :: (ResourceName)
  }


newtype DeleteResolverResponse = DeleteResolverResponse 
  { 
  }


newtype DeleteTypeRequest = DeleteTypeRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  }


newtype DeleteTypeResponse = DeleteTypeResponse 
  { 
  }


-- | <p>Describes a DynamoDB data source configuration.</p>
newtype DynamodbDataSourceConfig = DynamodbDataSourceConfig 
  { "TableName'" :: (String)
  , "AwsRegion'" :: (String)
  , "UseCallerCredentials'" :: NullOrUndefined (Boolean)
  }


-- | <p>Describes an Elasticsearch data source configuration.</p>
newtype ElasticsearchDataSourceConfig = ElasticsearchDataSourceConfig 
  { "Endpoint'" :: (String)
  , "AwsRegion'" :: (String)
  }


newtype ErrorMessage = ErrorMessage String


newtype GetDataSourceRequest = GetDataSourceRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (ResourceName)
  }


newtype GetDataSourceResponse = GetDataSourceResponse 
  { "DataSource'" :: NullOrUndefined (DataSource)
  }


newtype GetGraphqlApiRequest = GetGraphqlApiRequest 
  { "ApiId'" :: (String)
  }


newtype GetGraphqlApiResponse = GetGraphqlApiResponse 
  { "GraphqlApi'" :: NullOrUndefined (GraphqlApi)
  }


newtype GetIntrospectionSchemaRequest = GetIntrospectionSchemaRequest 
  { "ApiId'" :: (String)
  , "Format'" :: (OutputType)
  }


newtype GetIntrospectionSchemaResponse = GetIntrospectionSchemaResponse 
  { "Schema'" :: NullOrUndefined (String)
  }


newtype GetResolverRequest = GetResolverRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "FieldName'" :: (ResourceName)
  }


newtype GetResolverResponse = GetResolverResponse 
  { "Resolver'" :: NullOrUndefined (Resolver)
  }


newtype GetSchemaCreationStatusRequest = GetSchemaCreationStatusRequest 
  { "ApiId'" :: (String)
  }


newtype GetSchemaCreationStatusResponse = GetSchemaCreationStatusResponse 
  { "Status'" :: NullOrUndefined (SchemaStatus)
  , "Details'" :: NullOrUndefined (String)
  }


newtype GetTypeRequest = GetTypeRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "Format'" :: (TypeDefinitionFormat)
  }


newtype GetTypeResponse = GetTypeResponse 
  { "Type'" :: NullOrUndefined (Type)
  }


-- | <p>The GraphQL schema is not valid.</p>
newtype GraphQLSchemaException = GraphQLSchemaException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Describes a GraphQL API.</p>
newtype GraphqlApi = GraphqlApi 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "ApiId'" :: NullOrUndefined (String)
  , "AuthenticationType'" :: NullOrUndefined (AuthenticationType)
  , "UserPoolConfig'" :: NullOrUndefined (UserPoolConfig)
  , "Arn'" :: NullOrUndefined (String)
  , "Uris'" :: NullOrUndefined (MapOfStringToString)
  }


newtype GraphqlApis = GraphqlApis (Array GraphqlApi)


-- | <p>An internal AWS AppSync error occurred. Try your request again.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>Describes a Lambda data source configuration.</p>
newtype LambdaDataSourceConfig = LambdaDataSourceConfig 
  { "LambdaFunctionArn'" :: (String)
  }


-- | <p>The request exceeded a limit. Try your request again.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }


newtype ListApiKeysRequest = ListApiKeysRequest 
  { "ApiId'" :: (String)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype ListApiKeysResponse = ListApiKeysResponse 
  { "ApiKeys'" :: NullOrUndefined (ApiKeys)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


newtype ListDataSourcesRequest = ListDataSourcesRequest 
  { "ApiId'" :: (String)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype ListDataSourcesResponse = ListDataSourcesResponse 
  { "DataSources'" :: NullOrUndefined (DataSources)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


newtype ListGraphqlApisRequest = ListGraphqlApisRequest 
  { "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype ListGraphqlApisResponse = ListGraphqlApisResponse 
  { "GraphqlApis'" :: NullOrUndefined (GraphqlApis)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


newtype ListResolversRequest = ListResolversRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (String)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype ListResolversResponse = ListResolversResponse 
  { "Resolvers'" :: NullOrUndefined (Resolvers)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


newtype ListTypesRequest = ListTypesRequest 
  { "ApiId'" :: (String)
  , "Format'" :: (TypeDefinitionFormat)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype ListTypesResponse = ListTypesResponse 
  { "Types'" :: NullOrUndefined (TypeList)
  , "NextToken'" :: NullOrUndefined (PaginationToken)
  }


newtype MapOfStringToString = MapOfStringToString (Map String String)


newtype MappingTemplate = MappingTemplate String


newtype MaxResults = MaxResults Int


-- | <p>The resource specified in the request was not found. Check the resource and try again.</p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined (String)
  }


newtype OutputType = OutputType String


newtype PaginationToken = PaginationToken String


-- | <p>Describes a resolver.</p>
newtype Resolver = Resolver 
  { "TypeName'" :: NullOrUndefined (ResourceName)
  , "FieldName'" :: NullOrUndefined (ResourceName)
  , "DataSourceName'" :: NullOrUndefined (ResourceName)
  , "ResolverArn'" :: NullOrUndefined (String)
  , "RequestMappingTemplate'" :: NullOrUndefined (MappingTemplate)
  , "ResponseMappingTemplate'" :: NullOrUndefined (MappingTemplate)
  }


newtype Resolvers = Resolvers (Array Resolver)


newtype ResourceName = ResourceName String


newtype SchemaStatus = SchemaStatus String


newtype StartSchemaCreationRequest = StartSchemaCreationRequest 
  { "ApiId'" :: (String)
  , "Definition'" :: (String)
  }


newtype StartSchemaCreationResponse = StartSchemaCreationResponse 
  { "Status'" :: NullOrUndefined (SchemaStatus)
  }


-- | <p>Describes a type.</p>
newtype Type = Type 
  { "Name'" :: NullOrUndefined (ResourceName)
  , "Description'" :: NullOrUndefined (String)
  , "Arn'" :: NullOrUndefined (String)
  , "Definition'" :: NullOrUndefined (String)
  , "Format'" :: NullOrUndefined (TypeDefinitionFormat)
  }


newtype TypeDefinitionFormat = TypeDefinitionFormat String


newtype TypeList = TypeList (Array Type)


-- | <p>You are not authorized to perform this operation.</p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message'" :: NullOrUndefined (String)
  }


newtype UpdateApiKeyRequest = UpdateApiKeyRequest 
  { "ApiId'" :: (String)
  , "Id'" :: (String)
  , "Description'" :: NullOrUndefined (String)
  , "Expires'" :: NullOrUndefined (Number)
  }


newtype UpdateApiKeyResponse = UpdateApiKeyResponse 
  { "ApiKey'" :: NullOrUndefined (ApiKey)
  }


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


newtype UpdateDataSourceResponse = UpdateDataSourceResponse 
  { "DataSource'" :: NullOrUndefined (DataSource)
  }


newtype UpdateGraphqlApiRequest = UpdateGraphqlApiRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (String)
  , "AuthenticationType'" :: NullOrUndefined (AuthenticationType)
  , "UserPoolConfig'" :: NullOrUndefined (UserPoolConfig)
  }


newtype UpdateGraphqlApiResponse = UpdateGraphqlApiResponse 
  { "GraphqlApi'" :: NullOrUndefined (GraphqlApi)
  }


newtype UpdateResolverRequest = UpdateResolverRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "FieldName'" :: (ResourceName)
  , "DataSourceName'" :: (ResourceName)
  , "RequestMappingTemplate'" :: (MappingTemplate)
  , "ResponseMappingTemplate'" :: NullOrUndefined (MappingTemplate)
  }


newtype UpdateResolverResponse = UpdateResolverResponse 
  { "Resolver'" :: NullOrUndefined (Resolver)
  }


newtype UpdateTypeRequest = UpdateTypeRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "Definition'" :: NullOrUndefined (String)
  , "Format'" :: (TypeDefinitionFormat)
  }


newtype UpdateTypeResponse = UpdateTypeResponse 
  { "Type'" :: NullOrUndefined (Type)
  }


-- | <p>Describes an Amazon Cognito User Pool configuration.</p>
newtype UserPoolConfig = UserPoolConfig 
  { "UserPoolId'" :: (String)
  , "AwsRegion'" :: (String)
  , "DefaultAction'" :: (DefaultAction)
  , "AppIdClientRegex'" :: NullOrUndefined (String)
  }
