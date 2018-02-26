## Module AWS.AppSync

<p>AWS AppSync provides API actions for creating and interacting with data sources using GraphQL from your application.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createApiKey`

``` purescript
createApiKey :: forall eff. CreateApiKeyRequest -> Aff (err :: RequestError | eff) CreateApiKeyResponse
```

<p>Creates a unique key that you can distribute to clients who are executing your API.</p>

#### `createDataSource`

``` purescript
createDataSource :: forall eff. CreateDataSourceRequest -> Aff (err :: RequestError | eff) CreateDataSourceResponse
```

<p>Creates a <code>DataSource</code> object.</p>

#### `createGraphqlApi`

``` purescript
createGraphqlApi :: forall eff. CreateGraphqlApiRequest -> Aff (err :: RequestError | eff) CreateGraphqlApiResponse
```

<p>Creates a <code>GraphqlApi</code> object.</p>

#### `createResolver`

``` purescript
createResolver :: forall eff. CreateResolverRequest -> Aff (err :: RequestError | eff) CreateResolverResponse
```

<p>Creates a <code>Resolver</code> object.</p> <p>A resolver converts incoming requests into a format that a data source can understand and converts the data source's responses into GraphQL.</p>

#### `createType`

``` purescript
createType :: forall eff. CreateTypeRequest -> Aff (err :: RequestError | eff) CreateTypeResponse
```

<p>Creates a <code>Type</code> object.</p>

#### `deleteApiKey`

``` purescript
deleteApiKey :: forall eff. DeleteApiKeyRequest -> Aff (err :: RequestError | eff) DeleteApiKeyResponse
```

<p>Deletes an API key.</p>

#### `deleteDataSource`

``` purescript
deleteDataSource :: forall eff. DeleteDataSourceRequest -> Aff (err :: RequestError | eff) DeleteDataSourceResponse
```

<p>Deletes a <code>DataSource</code> object.</p>

#### `deleteGraphqlApi`

``` purescript
deleteGraphqlApi :: forall eff. DeleteGraphqlApiRequest -> Aff (err :: RequestError | eff) DeleteGraphqlApiResponse
```

<p>Deletes a <code>GraphqlApi</code> object.</p>

#### `deleteResolver`

``` purescript
deleteResolver :: forall eff. DeleteResolverRequest -> Aff (err :: RequestError | eff) DeleteResolverResponse
```

<p>Deletes a <code>Resolver</code> object.</p>

#### `deleteType`

``` purescript
deleteType :: forall eff. DeleteTypeRequest -> Aff (err :: RequestError | eff) DeleteTypeResponse
```

<p>Deletes a <code>Type</code> object.</p>

#### `getDataSource`

``` purescript
getDataSource :: forall eff. GetDataSourceRequest -> Aff (err :: RequestError | eff) GetDataSourceResponse
```

<p>Retrieves a <code>DataSource</code> object.</p>

#### `getGraphqlApi`

``` purescript
getGraphqlApi :: forall eff. GetGraphqlApiRequest -> Aff (err :: RequestError | eff) GetGraphqlApiResponse
```

<p>Retrieves a <code>GraphqlApi</code> object.</p>

#### `getIntrospectionSchema`

``` purescript
getIntrospectionSchema :: forall eff. GetIntrospectionSchemaRequest -> Aff (err :: RequestError | eff) GetIntrospectionSchemaResponse
```

<p>Retrieves the introspection schema for a GraphQL API.</p>

#### `getResolver`

``` purescript
getResolver :: forall eff. GetResolverRequest -> Aff (err :: RequestError | eff) GetResolverResponse
```

<p>Retrieves a <code>Resolver</code> object.</p>

#### `getSchemaCreationStatus`

``` purescript
getSchemaCreationStatus :: forall eff. GetSchemaCreationStatusRequest -> Aff (err :: RequestError | eff) GetSchemaCreationStatusResponse
```

<p>Retrieves the current status of a schema creation operation.</p>

#### `getType`

``` purescript
getType :: forall eff. GetTypeRequest -> Aff (err :: RequestError | eff) GetTypeResponse
```

<p>Retrieves a <code>Type</code> object.</p>

#### `listApiKeys`

``` purescript
listApiKeys :: forall eff. ListApiKeysRequest -> Aff (err :: RequestError | eff) ListApiKeysResponse
```

<p>Lists the API keys for a given API.</p>

#### `listDataSources`

``` purescript
listDataSources :: forall eff. ListDataSourcesRequest -> Aff (err :: RequestError | eff) ListDataSourcesResponse
```

<p>Lists the data sources for a given API.</p>

#### `listGraphqlApis`

``` purescript
listGraphqlApis :: forall eff. ListGraphqlApisRequest -> Aff (err :: RequestError | eff) ListGraphqlApisResponse
```

<p>Lists your GraphQL APIs.</p>

#### `listResolvers`

``` purescript
listResolvers :: forall eff. ListResolversRequest -> Aff (err :: RequestError | eff) ListResolversResponse
```

<p>Lists the resolvers for a given API and type.</p>

#### `listTypes`

``` purescript
listTypes :: forall eff. ListTypesRequest -> Aff (err :: RequestError | eff) ListTypesResponse
```

<p>Lists the types for a given API.</p>

#### `startSchemaCreation`

``` purescript
startSchemaCreation :: forall eff. StartSchemaCreationRequest -> Aff (err :: RequestError | eff) StartSchemaCreationResponse
```

<p>Adds a new schema to your GraphQL API.</p> <p>This operation is asynchronous. Use to determine when it has completed.</p>

#### `updateApiKey`

``` purescript
updateApiKey :: forall eff. UpdateApiKeyRequest -> Aff (err :: RequestError | eff) UpdateApiKeyResponse
```

<p>Updates an API key.</p>

#### `updateDataSource`

``` purescript
updateDataSource :: forall eff. UpdateDataSourceRequest -> Aff (err :: RequestError | eff) UpdateDataSourceResponse
```

<p>Updates a <code>DataSource</code> object.</p>

#### `updateGraphqlApi`

``` purescript
updateGraphqlApi :: forall eff. UpdateGraphqlApiRequest -> Aff (err :: RequestError | eff) UpdateGraphqlApiResponse
```

<p>Updates a <code>GraphqlApi</code> object.</p>

#### `updateResolver`

``` purescript
updateResolver :: forall eff. UpdateResolverRequest -> Aff (err :: RequestError | eff) UpdateResolverResponse
```

<p>Updates a <code>Resolver</code> object.</p>

#### `updateType`

``` purescript
updateType :: forall eff. UpdateTypeRequest -> Aff (err :: RequestError | eff) UpdateTypeResponse
```

<p>Updates a <code>Type</code> object.</p>

#### `ApiKey`

``` purescript
newtype ApiKey
  = ApiKey { "Id'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "Expires'" :: NullOrUndefined (Number) }
```

<p>Describes an API key.</p>

#### `ApiKeyLimitExceededException`

``` purescript
newtype ApiKeyLimitExceededException
  = ApiKeyLimitExceededException { "Message'" :: NullOrUndefined (String) }
```

<p>The API key exceeded a limit. Try your request again.</p>

#### `ApiKeyValidityOutOfBoundsException`

``` purescript
newtype ApiKeyValidityOutOfBoundsException
  = ApiKeyValidityOutOfBoundsException { "Message'" :: NullOrUndefined (String) }
```

<p>The API key expiration must be set to a value between 1 and 365 days.</p>

#### `ApiKeys`

``` purescript
newtype ApiKeys
  = ApiKeys (Array ApiKey)
```

#### `ApiLimitExceededException`

``` purescript
newtype ApiLimitExceededException
  = ApiLimitExceededException { "Message'" :: NullOrUndefined (String) }
```

<p>The GraphQL API exceeded a limit. Try your request again.</p>

#### `AuthenticationType`

``` purescript
newtype AuthenticationType
  = AuthenticationType String
```

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The request is not well formed. For example, a value is invalid or a required field is missing. Check the field values, and try again. </p>

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Another modification is being made. That modification must complete before you can make your change. </p>

#### `CreateApiKeyRequest`

``` purescript
newtype CreateApiKeyRequest
  = CreateApiKeyRequest { "ApiId'" :: String, "Description'" :: NullOrUndefined (String), "Expires'" :: NullOrUndefined (Number) }
```

#### `CreateApiKeyResponse`

``` purescript
newtype CreateApiKeyResponse
  = CreateApiKeyResponse { "ApiKey'" :: NullOrUndefined (ApiKey) }
```

#### `CreateDataSourceRequest`

``` purescript
newtype CreateDataSourceRequest
  = CreateDataSourceRequest { "ApiId'" :: String, "Name'" :: ResourceName, "Description'" :: NullOrUndefined (String), "Type'" :: DataSourceType, "ServiceRoleArn'" :: NullOrUndefined (String), "DynamodbConfig'" :: NullOrUndefined (DynamodbDataSourceConfig), "LambdaConfig'" :: NullOrUndefined (LambdaDataSourceConfig), "ElasticsearchConfig'" :: NullOrUndefined (ElasticsearchDataSourceConfig) }
```

#### `CreateDataSourceResponse`

``` purescript
newtype CreateDataSourceResponse
  = CreateDataSourceResponse { "DataSource'" :: NullOrUndefined (DataSource) }
```

#### `CreateGraphqlApiRequest`

``` purescript
newtype CreateGraphqlApiRequest
  = CreateGraphqlApiRequest { "Name'" :: String, "AuthenticationType'" :: AuthenticationType, "UserPoolConfig'" :: NullOrUndefined (UserPoolConfig) }
```

#### `CreateGraphqlApiResponse`

``` purescript
newtype CreateGraphqlApiResponse
  = CreateGraphqlApiResponse { "GraphqlApi'" :: NullOrUndefined (GraphqlApi) }
```

#### `CreateResolverRequest`

``` purescript
newtype CreateResolverRequest
  = CreateResolverRequest { "ApiId'" :: String, "TypeName'" :: ResourceName, "FieldName'" :: ResourceName, "DataSourceName'" :: ResourceName, "RequestMappingTemplate'" :: MappingTemplate, "ResponseMappingTemplate'" :: NullOrUndefined (MappingTemplate) }
```

#### `CreateResolverResponse`

``` purescript
newtype CreateResolverResponse
  = CreateResolverResponse { "Resolver'" :: NullOrUndefined (Resolver) }
```

#### `CreateTypeRequest`

``` purescript
newtype CreateTypeRequest
  = CreateTypeRequest { "ApiId'" :: String, "Definition'" :: String, "Format'" :: TypeDefinitionFormat }
```

#### `CreateTypeResponse`

``` purescript
newtype CreateTypeResponse
  = CreateTypeResponse { "Type'" :: NullOrUndefined (Type) }
```

#### `DataSource`

``` purescript
newtype DataSource
  = DataSource { "DataSourceArn'" :: NullOrUndefined (String), "Name'" :: NullOrUndefined (ResourceName), "Description'" :: NullOrUndefined (String), "Type'" :: NullOrUndefined (DataSourceType), "ServiceRoleArn'" :: NullOrUndefined (String), "DynamodbConfig'" :: NullOrUndefined (DynamodbDataSourceConfig), "LambdaConfig'" :: NullOrUndefined (LambdaDataSourceConfig), "ElasticsearchConfig'" :: NullOrUndefined (ElasticsearchDataSourceConfig) }
```

<p>Describes a data source.</p>

#### `DataSourceType`

``` purescript
newtype DataSourceType
  = DataSourceType String
```

#### `DataSources`

``` purescript
newtype DataSources
  = DataSources (Array DataSource)
```

#### `DefaultAction`

``` purescript
newtype DefaultAction
  = DefaultAction String
```

#### `DeleteApiKeyRequest`

``` purescript
newtype DeleteApiKeyRequest
  = DeleteApiKeyRequest { "ApiId'" :: String, "Id'" :: String }
```

#### `DeleteApiKeyResponse`

``` purescript
newtype DeleteApiKeyResponse
  = DeleteApiKeyResponse {  }
```

#### `DeleteDataSourceRequest`

``` purescript
newtype DeleteDataSourceRequest
  = DeleteDataSourceRequest { "ApiId'" :: String, "Name'" :: ResourceName }
```

#### `DeleteDataSourceResponse`

``` purescript
newtype DeleteDataSourceResponse
  = DeleteDataSourceResponse {  }
```

#### `DeleteGraphqlApiRequest`

``` purescript
newtype DeleteGraphqlApiRequest
  = DeleteGraphqlApiRequest { "ApiId'" :: String }
```

#### `DeleteGraphqlApiResponse`

``` purescript
newtype DeleteGraphqlApiResponse
  = DeleteGraphqlApiResponse {  }
```

#### `DeleteResolverRequest`

``` purescript
newtype DeleteResolverRequest
  = DeleteResolverRequest { "ApiId'" :: String, "TypeName'" :: ResourceName, "FieldName'" :: ResourceName }
```

#### `DeleteResolverResponse`

``` purescript
newtype DeleteResolverResponse
  = DeleteResolverResponse {  }
```

#### `DeleteTypeRequest`

``` purescript
newtype DeleteTypeRequest
  = DeleteTypeRequest { "ApiId'" :: String, "TypeName'" :: ResourceName }
```

#### `DeleteTypeResponse`

``` purescript
newtype DeleteTypeResponse
  = DeleteTypeResponse {  }
```

#### `DynamodbDataSourceConfig`

``` purescript
newtype DynamodbDataSourceConfig
  = DynamodbDataSourceConfig { "TableName'" :: String, "AwsRegion'" :: String, "UseCallerCredentials'" :: NullOrUndefined (Boolean) }
```

<p>Describes a DynamoDB data source configuration.</p>

#### `ElasticsearchDataSourceConfig`

``` purescript
newtype ElasticsearchDataSourceConfig
  = ElasticsearchDataSourceConfig { "Endpoint'" :: String, "AwsRegion'" :: String }
```

<p>Describes an Elasticsearch data source configuration.</p>

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `GetDataSourceRequest`

``` purescript
newtype GetDataSourceRequest
  = GetDataSourceRequest { "ApiId'" :: String, "Name'" :: ResourceName }
```

#### `GetDataSourceResponse`

``` purescript
newtype GetDataSourceResponse
  = GetDataSourceResponse { "DataSource'" :: NullOrUndefined (DataSource) }
```

#### `GetGraphqlApiRequest`

``` purescript
newtype GetGraphqlApiRequest
  = GetGraphqlApiRequest { "ApiId'" :: String }
```

#### `GetGraphqlApiResponse`

``` purescript
newtype GetGraphqlApiResponse
  = GetGraphqlApiResponse { "GraphqlApi'" :: NullOrUndefined (GraphqlApi) }
```

#### `GetIntrospectionSchemaRequest`

``` purescript
newtype GetIntrospectionSchemaRequest
  = GetIntrospectionSchemaRequest { "ApiId'" :: String, "Format'" :: OutputType }
```

#### `GetIntrospectionSchemaResponse`

``` purescript
newtype GetIntrospectionSchemaResponse
  = GetIntrospectionSchemaResponse { "Schema'" :: NullOrUndefined (String) }
```

#### `GetResolverRequest`

``` purescript
newtype GetResolverRequest
  = GetResolverRequest { "ApiId'" :: String, "TypeName'" :: ResourceName, "FieldName'" :: ResourceName }
```

#### `GetResolverResponse`

``` purescript
newtype GetResolverResponse
  = GetResolverResponse { "Resolver'" :: NullOrUndefined (Resolver) }
```

#### `GetSchemaCreationStatusRequest`

``` purescript
newtype GetSchemaCreationStatusRequest
  = GetSchemaCreationStatusRequest { "ApiId'" :: String }
```

#### `GetSchemaCreationStatusResponse`

``` purescript
newtype GetSchemaCreationStatusResponse
  = GetSchemaCreationStatusResponse { "Status'" :: NullOrUndefined (SchemaStatus), "Details'" :: NullOrUndefined (String) }
```

#### `GetTypeRequest`

``` purescript
newtype GetTypeRequest
  = GetTypeRequest { "ApiId'" :: String, "TypeName'" :: ResourceName, "Format'" :: TypeDefinitionFormat }
```

#### `GetTypeResponse`

``` purescript
newtype GetTypeResponse
  = GetTypeResponse { "Type'" :: NullOrUndefined (Type) }
```

#### `GraphQLSchemaException`

``` purescript
newtype GraphQLSchemaException
  = GraphQLSchemaException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The GraphQL schema is not valid.</p>

#### `GraphqlApi`

``` purescript
newtype GraphqlApi
  = GraphqlApi { "Name'" :: NullOrUndefined (ResourceName), "ApiId'" :: NullOrUndefined (String), "AuthenticationType'" :: NullOrUndefined (AuthenticationType), "UserPoolConfig'" :: NullOrUndefined (UserPoolConfig), "Arn'" :: NullOrUndefined (String), "Uris'" :: NullOrUndefined (MapOfStringToString) }
```

<p>Describes a GraphQL API.</p>

#### `GraphqlApis`

``` purescript
newtype GraphqlApis
  = GraphqlApis (Array GraphqlApi)
```

#### `InternalFailureException`

``` purescript
newtype InternalFailureException
  = InternalFailureException { "Message'" :: NullOrUndefined (String) }
```

<p>An internal AWS AppSync error occurred. Try your request again.</p>

#### `LambdaDataSourceConfig`

``` purescript
newtype LambdaDataSourceConfig
  = LambdaDataSourceConfig { "LambdaFunctionArn'" :: String }
```

<p>Describes a Lambda data source configuration.</p>

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (String) }
```

<p>The request exceeded a limit. Try your request again.</p>

#### `ListApiKeysRequest`

``` purescript
newtype ListApiKeysRequest
  = ListApiKeysRequest { "ApiId'" :: String, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `ListApiKeysResponse`

``` purescript
newtype ListApiKeysResponse
  = ListApiKeysResponse { "ApiKeys'" :: NullOrUndefined (ApiKeys), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `ListDataSourcesRequest`

``` purescript
newtype ListDataSourcesRequest
  = ListDataSourcesRequest { "ApiId'" :: String, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `ListDataSourcesResponse`

``` purescript
newtype ListDataSourcesResponse
  = ListDataSourcesResponse { "DataSources'" :: NullOrUndefined (DataSources), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `ListGraphqlApisRequest`

``` purescript
newtype ListGraphqlApisRequest
  = ListGraphqlApisRequest { "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `ListGraphqlApisResponse`

``` purescript
newtype ListGraphqlApisResponse
  = ListGraphqlApisResponse { "GraphqlApis'" :: NullOrUndefined (GraphqlApis), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `ListResolversRequest`

``` purescript
newtype ListResolversRequest
  = ListResolversRequest { "ApiId'" :: String, "TypeName'" :: String, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `ListResolversResponse`

``` purescript
newtype ListResolversResponse
  = ListResolversResponse { "Resolvers'" :: NullOrUndefined (Resolvers), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `ListTypesRequest`

``` purescript
newtype ListTypesRequest
  = ListTypesRequest { "ApiId'" :: String, "Format'" :: TypeDefinitionFormat, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `ListTypesResponse`

``` purescript
newtype ListTypesResponse
  = ListTypesResponse { "Types'" :: NullOrUndefined (TypeList), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

#### `MapOfStringToString`

``` purescript
newtype MapOfStringToString
  = MapOfStringToString (Map String String)
```

#### `MappingTemplate`

``` purescript
newtype MappingTemplate
  = MappingTemplate String
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message'" :: NullOrUndefined (String) }
```

<p>The resource specified in the request was not found. Check the resource and try again.</p>

#### `OutputType`

``` purescript
newtype OutputType
  = OutputType String
```

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

#### `Resolver`

``` purescript
newtype Resolver
  = Resolver { "TypeName'" :: NullOrUndefined (ResourceName), "FieldName'" :: NullOrUndefined (ResourceName), "DataSourceName'" :: NullOrUndefined (ResourceName), "ResolverArn'" :: NullOrUndefined (String), "RequestMappingTemplate'" :: NullOrUndefined (MappingTemplate), "ResponseMappingTemplate'" :: NullOrUndefined (MappingTemplate) }
```

<p>Describes a resolver.</p>

#### `Resolvers`

``` purescript
newtype Resolvers
  = Resolvers (Array Resolver)
```

#### `ResourceName`

``` purescript
newtype ResourceName
  = ResourceName String
```

#### `SchemaStatus`

``` purescript
newtype SchemaStatus
  = SchemaStatus String
```

#### `StartSchemaCreationRequest`

``` purescript
newtype StartSchemaCreationRequest
  = StartSchemaCreationRequest { "ApiId'" :: String, "Definition'" :: String }
```

#### `StartSchemaCreationResponse`

``` purescript
newtype StartSchemaCreationResponse
  = StartSchemaCreationResponse { "Status'" :: NullOrUndefined (SchemaStatus) }
```

#### `Type`

``` purescript
newtype Type
  = Type { "Name'" :: NullOrUndefined (ResourceName), "Description'" :: NullOrUndefined (String), "Arn'" :: NullOrUndefined (String), "Definition'" :: NullOrUndefined (String), "Format'" :: NullOrUndefined (TypeDefinitionFormat) }
```

<p>Describes a type.</p>

#### `TypeDefinitionFormat`

``` purescript
newtype TypeDefinitionFormat
  = TypeDefinitionFormat String
```

#### `TypeList`

``` purescript
newtype TypeList
  = TypeList (Array Type)
```

#### `UnauthorizedException`

``` purescript
newtype UnauthorizedException
  = UnauthorizedException { "Message'" :: NullOrUndefined (String) }
```

<p>You are not authorized to perform this operation.</p>

#### `UpdateApiKeyRequest`

``` purescript
newtype UpdateApiKeyRequest
  = UpdateApiKeyRequest { "ApiId'" :: String, "Id'" :: String, "Description'" :: NullOrUndefined (String), "Expires'" :: NullOrUndefined (Number) }
```

#### `UpdateApiKeyResponse`

``` purescript
newtype UpdateApiKeyResponse
  = UpdateApiKeyResponse { "ApiKey'" :: NullOrUndefined (ApiKey) }
```

#### `UpdateDataSourceRequest`

``` purescript
newtype UpdateDataSourceRequest
  = UpdateDataSourceRequest { "ApiId'" :: String, "Name'" :: ResourceName, "Description'" :: NullOrUndefined (String), "Type'" :: DataSourceType, "ServiceRoleArn'" :: NullOrUndefined (String), "DynamodbConfig'" :: NullOrUndefined (DynamodbDataSourceConfig), "LambdaConfig'" :: NullOrUndefined (LambdaDataSourceConfig), "ElasticsearchConfig'" :: NullOrUndefined (ElasticsearchDataSourceConfig) }
```

#### `UpdateDataSourceResponse`

``` purescript
newtype UpdateDataSourceResponse
  = UpdateDataSourceResponse { "DataSource'" :: NullOrUndefined (DataSource) }
```

#### `UpdateGraphqlApiRequest`

``` purescript
newtype UpdateGraphqlApiRequest
  = UpdateGraphqlApiRequest { "ApiId'" :: String, "Name'" :: String, "AuthenticationType'" :: NullOrUndefined (AuthenticationType), "UserPoolConfig'" :: NullOrUndefined (UserPoolConfig) }
```

#### `UpdateGraphqlApiResponse`

``` purescript
newtype UpdateGraphqlApiResponse
  = UpdateGraphqlApiResponse { "GraphqlApi'" :: NullOrUndefined (GraphqlApi) }
```

#### `UpdateResolverRequest`

``` purescript
newtype UpdateResolverRequest
  = UpdateResolverRequest { "ApiId'" :: String, "TypeName'" :: ResourceName, "FieldName'" :: ResourceName, "DataSourceName'" :: ResourceName, "RequestMappingTemplate'" :: MappingTemplate, "ResponseMappingTemplate'" :: NullOrUndefined (MappingTemplate) }
```

#### `UpdateResolverResponse`

``` purescript
newtype UpdateResolverResponse
  = UpdateResolverResponse { "Resolver'" :: NullOrUndefined (Resolver) }
```

#### `UpdateTypeRequest`

``` purescript
newtype UpdateTypeRequest
  = UpdateTypeRequest { "ApiId'" :: String, "TypeName'" :: ResourceName, "Definition'" :: NullOrUndefined (String), "Format'" :: TypeDefinitionFormat }
```

#### `UpdateTypeResponse`

``` purescript
newtype UpdateTypeResponse
  = UpdateTypeResponse { "Type'" :: NullOrUndefined (Type) }
```

#### `UserPoolConfig`

``` purescript
newtype UserPoolConfig
  = UserPoolConfig { "UserPoolId'" :: String, "AwsRegion'" :: String, "DefaultAction'" :: DefaultAction, "AppIdClientRegex'" :: NullOrUndefined (String) }
```

<p>Describes an Amazon Cognito User Pool configuration.</p>


