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

##### Instances
``` purescript
Newtype ApiKey _
```

#### `ApiKeyLimitExceededException`

``` purescript
newtype ApiKeyLimitExceededException
  = ApiKeyLimitExceededException { "Message'" :: NullOrUndefined (String) }
```

<p>The API key exceeded a limit. Try your request again.</p>

##### Instances
``` purescript
Newtype ApiKeyLimitExceededException _
```

#### `ApiKeyValidityOutOfBoundsException`

``` purescript
newtype ApiKeyValidityOutOfBoundsException
  = ApiKeyValidityOutOfBoundsException { "Message'" :: NullOrUndefined (String) }
```

<p>The API key expiration must be set to a value between 1 and 365 days.</p>

##### Instances
``` purescript
Newtype ApiKeyValidityOutOfBoundsException _
```

#### `ApiKeys`

``` purescript
newtype ApiKeys
  = ApiKeys (Array ApiKey)
```

##### Instances
``` purescript
Newtype ApiKeys _
```

#### `ApiLimitExceededException`

``` purescript
newtype ApiLimitExceededException
  = ApiLimitExceededException { "Message'" :: NullOrUndefined (String) }
```

<p>The GraphQL API exceeded a limit. Try your request again.</p>

##### Instances
``` purescript
Newtype ApiLimitExceededException _
```

#### `AuthenticationType`

``` purescript
newtype AuthenticationType
  = AuthenticationType String
```

##### Instances
``` purescript
Newtype AuthenticationType _
```

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The request is not well formed. For example, a value is invalid or a required field is missing. Check the field values, and try again. </p>

##### Instances
``` purescript
Newtype BadRequestException _
```

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Another modification is being made. That modification must complete before you can make your change. </p>

##### Instances
``` purescript
Newtype ConcurrentModificationException _
```

#### `CreateApiKeyRequest`

``` purescript
newtype CreateApiKeyRequest
  = CreateApiKeyRequest { "ApiId'" :: String, "Description'" :: NullOrUndefined (String), "Expires'" :: NullOrUndefined (Number) }
```

##### Instances
``` purescript
Newtype CreateApiKeyRequest _
```

#### `CreateApiKeyResponse`

``` purescript
newtype CreateApiKeyResponse
  = CreateApiKeyResponse { "ApiKey'" :: NullOrUndefined (ApiKey) }
```

##### Instances
``` purescript
Newtype CreateApiKeyResponse _
```

#### `CreateDataSourceRequest`

``` purescript
newtype CreateDataSourceRequest
  = CreateDataSourceRequest { "ApiId'" :: String, "Name'" :: ResourceName, "Description'" :: NullOrUndefined (String), "Type'" :: DataSourceType, "ServiceRoleArn'" :: NullOrUndefined (String), "DynamodbConfig'" :: NullOrUndefined (DynamodbDataSourceConfig), "LambdaConfig'" :: NullOrUndefined (LambdaDataSourceConfig), "ElasticsearchConfig'" :: NullOrUndefined (ElasticsearchDataSourceConfig) }
```

##### Instances
``` purescript
Newtype CreateDataSourceRequest _
```

#### `CreateDataSourceResponse`

``` purescript
newtype CreateDataSourceResponse
  = CreateDataSourceResponse { "DataSource'" :: NullOrUndefined (DataSource) }
```

##### Instances
``` purescript
Newtype CreateDataSourceResponse _
```

#### `CreateGraphqlApiRequest`

``` purescript
newtype CreateGraphqlApiRequest
  = CreateGraphqlApiRequest { "Name'" :: String, "AuthenticationType'" :: AuthenticationType, "UserPoolConfig'" :: NullOrUndefined (UserPoolConfig) }
```

##### Instances
``` purescript
Newtype CreateGraphqlApiRequest _
```

#### `CreateGraphqlApiResponse`

``` purescript
newtype CreateGraphqlApiResponse
  = CreateGraphqlApiResponse { "GraphqlApi'" :: NullOrUndefined (GraphqlApi) }
```

##### Instances
``` purescript
Newtype CreateGraphqlApiResponse _
```

#### `CreateResolverRequest`

``` purescript
newtype CreateResolverRequest
  = CreateResolverRequest { "ApiId'" :: String, "TypeName'" :: ResourceName, "FieldName'" :: ResourceName, "DataSourceName'" :: ResourceName, "RequestMappingTemplate'" :: MappingTemplate, "ResponseMappingTemplate'" :: NullOrUndefined (MappingTemplate) }
```

##### Instances
``` purescript
Newtype CreateResolverRequest _
```

#### `CreateResolverResponse`

``` purescript
newtype CreateResolverResponse
  = CreateResolverResponse { "Resolver'" :: NullOrUndefined (Resolver) }
```

##### Instances
``` purescript
Newtype CreateResolverResponse _
```

#### `CreateTypeRequest`

``` purescript
newtype CreateTypeRequest
  = CreateTypeRequest { "ApiId'" :: String, "Definition'" :: String, "Format'" :: TypeDefinitionFormat }
```

##### Instances
``` purescript
Newtype CreateTypeRequest _
```

#### `CreateTypeResponse`

``` purescript
newtype CreateTypeResponse
  = CreateTypeResponse { "Type'" :: NullOrUndefined (Type) }
```

##### Instances
``` purescript
Newtype CreateTypeResponse _
```

#### `DataSource`

``` purescript
newtype DataSource
  = DataSource { "DataSourceArn'" :: NullOrUndefined (String), "Name'" :: NullOrUndefined (ResourceName), "Description'" :: NullOrUndefined (String), "Type'" :: NullOrUndefined (DataSourceType), "ServiceRoleArn'" :: NullOrUndefined (String), "DynamodbConfig'" :: NullOrUndefined (DynamodbDataSourceConfig), "LambdaConfig'" :: NullOrUndefined (LambdaDataSourceConfig), "ElasticsearchConfig'" :: NullOrUndefined (ElasticsearchDataSourceConfig) }
```

<p>Describes a data source.</p>

##### Instances
``` purescript
Newtype DataSource _
```

#### `DataSourceType`

``` purescript
newtype DataSourceType
  = DataSourceType String
```

##### Instances
``` purescript
Newtype DataSourceType _
```

#### `DataSources`

``` purescript
newtype DataSources
  = DataSources (Array DataSource)
```

##### Instances
``` purescript
Newtype DataSources _
```

#### `DefaultAction`

``` purescript
newtype DefaultAction
  = DefaultAction String
```

##### Instances
``` purescript
Newtype DefaultAction _
```

#### `DeleteApiKeyRequest`

``` purescript
newtype DeleteApiKeyRequest
  = DeleteApiKeyRequest { "ApiId'" :: String, "Id'" :: String }
```

##### Instances
``` purescript
Newtype DeleteApiKeyRequest _
```

#### `DeleteApiKeyResponse`

``` purescript
newtype DeleteApiKeyResponse
  = DeleteApiKeyResponse {  }
```

##### Instances
``` purescript
Newtype DeleteApiKeyResponse _
```

#### `DeleteDataSourceRequest`

``` purescript
newtype DeleteDataSourceRequest
  = DeleteDataSourceRequest { "ApiId'" :: String, "Name'" :: ResourceName }
```

##### Instances
``` purescript
Newtype DeleteDataSourceRequest _
```

#### `DeleteDataSourceResponse`

``` purescript
newtype DeleteDataSourceResponse
  = DeleteDataSourceResponse {  }
```

##### Instances
``` purescript
Newtype DeleteDataSourceResponse _
```

#### `DeleteGraphqlApiRequest`

``` purescript
newtype DeleteGraphqlApiRequest
  = DeleteGraphqlApiRequest { "ApiId'" :: String }
```

##### Instances
``` purescript
Newtype DeleteGraphqlApiRequest _
```

#### `DeleteGraphqlApiResponse`

``` purescript
newtype DeleteGraphqlApiResponse
  = DeleteGraphqlApiResponse {  }
```

##### Instances
``` purescript
Newtype DeleteGraphqlApiResponse _
```

#### `DeleteResolverRequest`

``` purescript
newtype DeleteResolverRequest
  = DeleteResolverRequest { "ApiId'" :: String, "TypeName'" :: ResourceName, "FieldName'" :: ResourceName }
```

##### Instances
``` purescript
Newtype DeleteResolverRequest _
```

#### `DeleteResolverResponse`

``` purescript
newtype DeleteResolverResponse
  = DeleteResolverResponse {  }
```

##### Instances
``` purescript
Newtype DeleteResolverResponse _
```

#### `DeleteTypeRequest`

``` purescript
newtype DeleteTypeRequest
  = DeleteTypeRequest { "ApiId'" :: String, "TypeName'" :: ResourceName }
```

##### Instances
``` purescript
Newtype DeleteTypeRequest _
```

#### `DeleteTypeResponse`

``` purescript
newtype DeleteTypeResponse
  = DeleteTypeResponse {  }
```

##### Instances
``` purescript
Newtype DeleteTypeResponse _
```

#### `DynamodbDataSourceConfig`

``` purescript
newtype DynamodbDataSourceConfig
  = DynamodbDataSourceConfig { "TableName'" :: String, "AwsRegion'" :: String, "UseCallerCredentials'" :: NullOrUndefined (Boolean) }
```

<p>Describes a DynamoDB data source configuration.</p>

##### Instances
``` purescript
Newtype DynamodbDataSourceConfig _
```

#### `ElasticsearchDataSourceConfig`

``` purescript
newtype ElasticsearchDataSourceConfig
  = ElasticsearchDataSourceConfig { "Endpoint'" :: String, "AwsRegion'" :: String }
```

<p>Describes an Elasticsearch data source configuration.</p>

##### Instances
``` purescript
Newtype ElasticsearchDataSourceConfig _
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

#### `GetDataSourceRequest`

``` purescript
newtype GetDataSourceRequest
  = GetDataSourceRequest { "ApiId'" :: String, "Name'" :: ResourceName }
```

##### Instances
``` purescript
Newtype GetDataSourceRequest _
```

#### `GetDataSourceResponse`

``` purescript
newtype GetDataSourceResponse
  = GetDataSourceResponse { "DataSource'" :: NullOrUndefined (DataSource) }
```

##### Instances
``` purescript
Newtype GetDataSourceResponse _
```

#### `GetGraphqlApiRequest`

``` purescript
newtype GetGraphqlApiRequest
  = GetGraphqlApiRequest { "ApiId'" :: String }
```

##### Instances
``` purescript
Newtype GetGraphqlApiRequest _
```

#### `GetGraphqlApiResponse`

``` purescript
newtype GetGraphqlApiResponse
  = GetGraphqlApiResponse { "GraphqlApi'" :: NullOrUndefined (GraphqlApi) }
```

##### Instances
``` purescript
Newtype GetGraphqlApiResponse _
```

#### `GetIntrospectionSchemaRequest`

``` purescript
newtype GetIntrospectionSchemaRequest
  = GetIntrospectionSchemaRequest { "ApiId'" :: String, "Format'" :: OutputType }
```

##### Instances
``` purescript
Newtype GetIntrospectionSchemaRequest _
```

#### `GetIntrospectionSchemaResponse`

``` purescript
newtype GetIntrospectionSchemaResponse
  = GetIntrospectionSchemaResponse { "Schema'" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetIntrospectionSchemaResponse _
```

#### `GetResolverRequest`

``` purescript
newtype GetResolverRequest
  = GetResolverRequest { "ApiId'" :: String, "TypeName'" :: ResourceName, "FieldName'" :: ResourceName }
```

##### Instances
``` purescript
Newtype GetResolverRequest _
```

#### `GetResolverResponse`

``` purescript
newtype GetResolverResponse
  = GetResolverResponse { "Resolver'" :: NullOrUndefined (Resolver) }
```

##### Instances
``` purescript
Newtype GetResolverResponse _
```

#### `GetSchemaCreationStatusRequest`

``` purescript
newtype GetSchemaCreationStatusRequest
  = GetSchemaCreationStatusRequest { "ApiId'" :: String }
```

##### Instances
``` purescript
Newtype GetSchemaCreationStatusRequest _
```

#### `GetSchemaCreationStatusResponse`

``` purescript
newtype GetSchemaCreationStatusResponse
  = GetSchemaCreationStatusResponse { "Status'" :: NullOrUndefined (SchemaStatus), "Details'" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetSchemaCreationStatusResponse _
```

#### `GetTypeRequest`

``` purescript
newtype GetTypeRequest
  = GetTypeRequest { "ApiId'" :: String, "TypeName'" :: ResourceName, "Format'" :: TypeDefinitionFormat }
```

##### Instances
``` purescript
Newtype GetTypeRequest _
```

#### `GetTypeResponse`

``` purescript
newtype GetTypeResponse
  = GetTypeResponse { "Type'" :: NullOrUndefined (Type) }
```

##### Instances
``` purescript
Newtype GetTypeResponse _
```

#### `GraphQLSchemaException`

``` purescript
newtype GraphQLSchemaException
  = GraphQLSchemaException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The GraphQL schema is not valid.</p>

##### Instances
``` purescript
Newtype GraphQLSchemaException _
```

#### `GraphqlApi`

``` purescript
newtype GraphqlApi
  = GraphqlApi { "Name'" :: NullOrUndefined (ResourceName), "ApiId'" :: NullOrUndefined (String), "AuthenticationType'" :: NullOrUndefined (AuthenticationType), "UserPoolConfig'" :: NullOrUndefined (UserPoolConfig), "Arn'" :: NullOrUndefined (String), "Uris'" :: NullOrUndefined (MapOfStringToString) }
```

<p>Describes a GraphQL API.</p>

##### Instances
``` purescript
Newtype GraphqlApi _
```

#### `GraphqlApis`

``` purescript
newtype GraphqlApis
  = GraphqlApis (Array GraphqlApi)
```

##### Instances
``` purescript
Newtype GraphqlApis _
```

#### `InternalFailureException`

``` purescript
newtype InternalFailureException
  = InternalFailureException { "Message'" :: NullOrUndefined (String) }
```

<p>An internal AWS AppSync error occurred. Try your request again.</p>

##### Instances
``` purescript
Newtype InternalFailureException _
```

#### `LambdaDataSourceConfig`

``` purescript
newtype LambdaDataSourceConfig
  = LambdaDataSourceConfig { "LambdaFunctionArn'" :: String }
```

<p>Describes a Lambda data source configuration.</p>

##### Instances
``` purescript
Newtype LambdaDataSourceConfig _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (String) }
```

<p>The request exceeded a limit. Try your request again.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListApiKeysRequest`

``` purescript
newtype ListApiKeysRequest
  = ListApiKeysRequest { "ApiId'" :: String, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListApiKeysRequest _
```

#### `ListApiKeysResponse`

``` purescript
newtype ListApiKeysResponse
  = ListApiKeysResponse { "ApiKeys'" :: NullOrUndefined (ApiKeys), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListApiKeysResponse _
```

#### `ListDataSourcesRequest`

``` purescript
newtype ListDataSourcesRequest
  = ListDataSourcesRequest { "ApiId'" :: String, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListDataSourcesRequest _
```

#### `ListDataSourcesResponse`

``` purescript
newtype ListDataSourcesResponse
  = ListDataSourcesResponse { "DataSources'" :: NullOrUndefined (DataSources), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListDataSourcesResponse _
```

#### `ListGraphqlApisRequest`

``` purescript
newtype ListGraphqlApisRequest
  = ListGraphqlApisRequest { "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListGraphqlApisRequest _
```

#### `ListGraphqlApisResponse`

``` purescript
newtype ListGraphqlApisResponse
  = ListGraphqlApisResponse { "GraphqlApis'" :: NullOrUndefined (GraphqlApis), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListGraphqlApisResponse _
```

#### `ListResolversRequest`

``` purescript
newtype ListResolversRequest
  = ListResolversRequest { "ApiId'" :: String, "TypeName'" :: String, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListResolversRequest _
```

#### `ListResolversResponse`

``` purescript
newtype ListResolversResponse
  = ListResolversResponse { "Resolvers'" :: NullOrUndefined (Resolvers), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListResolversResponse _
```

#### `ListTypesRequest`

``` purescript
newtype ListTypesRequest
  = ListTypesRequest { "ApiId'" :: String, "Format'" :: TypeDefinitionFormat, "NextToken'" :: NullOrUndefined (PaginationToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListTypesRequest _
```

#### `ListTypesResponse`

``` purescript
newtype ListTypesResponse
  = ListTypesResponse { "Types'" :: NullOrUndefined (TypeList), "NextToken'" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListTypesResponse _
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

#### `MappingTemplate`

``` purescript
newtype MappingTemplate
  = MappingTemplate String
```

##### Instances
``` purescript
Newtype MappingTemplate _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

##### Instances
``` purescript
Newtype MaxResults _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message'" :: NullOrUndefined (String) }
```

<p>The resource specified in the request was not found. Check the resource and try again.</p>

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `OutputType`

``` purescript
newtype OutputType
  = OutputType String
```

##### Instances
``` purescript
Newtype OutputType _
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

#### `Resolver`

``` purescript
newtype Resolver
  = Resolver { "TypeName'" :: NullOrUndefined (ResourceName), "FieldName'" :: NullOrUndefined (ResourceName), "DataSourceName'" :: NullOrUndefined (ResourceName), "ResolverArn'" :: NullOrUndefined (String), "RequestMappingTemplate'" :: NullOrUndefined (MappingTemplate), "ResponseMappingTemplate'" :: NullOrUndefined (MappingTemplate) }
```

<p>Describes a resolver.</p>

##### Instances
``` purescript
Newtype Resolver _
```

#### `Resolvers`

``` purescript
newtype Resolvers
  = Resolvers (Array Resolver)
```

##### Instances
``` purescript
Newtype Resolvers _
```

#### `ResourceName`

``` purescript
newtype ResourceName
  = ResourceName String
```

##### Instances
``` purescript
Newtype ResourceName _
```

#### `SchemaStatus`

``` purescript
newtype SchemaStatus
  = SchemaStatus String
```

##### Instances
``` purescript
Newtype SchemaStatus _
```

#### `StartSchemaCreationRequest`

``` purescript
newtype StartSchemaCreationRequest
  = StartSchemaCreationRequest { "ApiId'" :: String, "Definition'" :: String }
```

##### Instances
``` purescript
Newtype StartSchemaCreationRequest _
```

#### `StartSchemaCreationResponse`

``` purescript
newtype StartSchemaCreationResponse
  = StartSchemaCreationResponse { "Status'" :: NullOrUndefined (SchemaStatus) }
```

##### Instances
``` purescript
Newtype StartSchemaCreationResponse _
```

#### `Type`

``` purescript
newtype Type
  = Type { "Name'" :: NullOrUndefined (ResourceName), "Description'" :: NullOrUndefined (String), "Arn'" :: NullOrUndefined (String), "Definition'" :: NullOrUndefined (String), "Format'" :: NullOrUndefined (TypeDefinitionFormat) }
```

<p>Describes a type.</p>

##### Instances
``` purescript
Newtype Type _
```

#### `TypeDefinitionFormat`

``` purescript
newtype TypeDefinitionFormat
  = TypeDefinitionFormat String
```

##### Instances
``` purescript
Newtype TypeDefinitionFormat _
```

#### `TypeList`

``` purescript
newtype TypeList
  = TypeList (Array Type)
```

##### Instances
``` purescript
Newtype TypeList _
```

#### `UnauthorizedException`

``` purescript
newtype UnauthorizedException
  = UnauthorizedException { "Message'" :: NullOrUndefined (String) }
```

<p>You are not authorized to perform this operation.</p>

##### Instances
``` purescript
Newtype UnauthorizedException _
```

#### `UpdateApiKeyRequest`

``` purescript
newtype UpdateApiKeyRequest
  = UpdateApiKeyRequest { "ApiId'" :: String, "Id'" :: String, "Description'" :: NullOrUndefined (String), "Expires'" :: NullOrUndefined (Number) }
```

##### Instances
``` purescript
Newtype UpdateApiKeyRequest _
```

#### `UpdateApiKeyResponse`

``` purescript
newtype UpdateApiKeyResponse
  = UpdateApiKeyResponse { "ApiKey'" :: NullOrUndefined (ApiKey) }
```

##### Instances
``` purescript
Newtype UpdateApiKeyResponse _
```

#### `UpdateDataSourceRequest`

``` purescript
newtype UpdateDataSourceRequest
  = UpdateDataSourceRequest { "ApiId'" :: String, "Name'" :: ResourceName, "Description'" :: NullOrUndefined (String), "Type'" :: DataSourceType, "ServiceRoleArn'" :: NullOrUndefined (String), "DynamodbConfig'" :: NullOrUndefined (DynamodbDataSourceConfig), "LambdaConfig'" :: NullOrUndefined (LambdaDataSourceConfig), "ElasticsearchConfig'" :: NullOrUndefined (ElasticsearchDataSourceConfig) }
```

##### Instances
``` purescript
Newtype UpdateDataSourceRequest _
```

#### `UpdateDataSourceResponse`

``` purescript
newtype UpdateDataSourceResponse
  = UpdateDataSourceResponse { "DataSource'" :: NullOrUndefined (DataSource) }
```

##### Instances
``` purescript
Newtype UpdateDataSourceResponse _
```

#### `UpdateGraphqlApiRequest`

``` purescript
newtype UpdateGraphqlApiRequest
  = UpdateGraphqlApiRequest { "ApiId'" :: String, "Name'" :: String, "AuthenticationType'" :: NullOrUndefined (AuthenticationType), "UserPoolConfig'" :: NullOrUndefined (UserPoolConfig) }
```

##### Instances
``` purescript
Newtype UpdateGraphqlApiRequest _
```

#### `UpdateGraphqlApiResponse`

``` purescript
newtype UpdateGraphqlApiResponse
  = UpdateGraphqlApiResponse { "GraphqlApi'" :: NullOrUndefined (GraphqlApi) }
```

##### Instances
``` purescript
Newtype UpdateGraphqlApiResponse _
```

#### `UpdateResolverRequest`

``` purescript
newtype UpdateResolverRequest
  = UpdateResolverRequest { "ApiId'" :: String, "TypeName'" :: ResourceName, "FieldName'" :: ResourceName, "DataSourceName'" :: ResourceName, "RequestMappingTemplate'" :: MappingTemplate, "ResponseMappingTemplate'" :: NullOrUndefined (MappingTemplate) }
```

##### Instances
``` purescript
Newtype UpdateResolverRequest _
```

#### `UpdateResolverResponse`

``` purescript
newtype UpdateResolverResponse
  = UpdateResolverResponse { "Resolver'" :: NullOrUndefined (Resolver) }
```

##### Instances
``` purescript
Newtype UpdateResolverResponse _
```

#### `UpdateTypeRequest`

``` purescript
newtype UpdateTypeRequest
  = UpdateTypeRequest { "ApiId'" :: String, "TypeName'" :: ResourceName, "Definition'" :: NullOrUndefined (String), "Format'" :: TypeDefinitionFormat }
```

##### Instances
``` purescript
Newtype UpdateTypeRequest _
```

#### `UpdateTypeResponse`

``` purescript
newtype UpdateTypeResponse
  = UpdateTypeResponse { "Type'" :: NullOrUndefined (Type) }
```

##### Instances
``` purescript
Newtype UpdateTypeResponse _
```

#### `UserPoolConfig`

``` purescript
newtype UserPoolConfig
  = UserPoolConfig { "UserPoolId'" :: String, "AwsRegion'" :: String, "DefaultAction'" :: DefaultAction, "AppIdClientRegex'" :: NullOrUndefined (String) }
```

<p>Describes an Amazon Cognito User Pool configuration.</p>

##### Instances
``` purescript
Newtype UserPoolConfig _
```


