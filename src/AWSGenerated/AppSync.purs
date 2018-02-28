

-- | <p>AWS AppSync provides API actions for creating and interacting with data sources using GraphQL from your application.</p>
module AWS.AppSync where

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

serviceName = "AppSync" :: String


-- | <p>Creates a unique key that you can distribute to clients who are executing your API.</p>
createApiKey :: forall eff. CreateApiKeyRequest -> Aff (exception :: EXCEPTION | eff) CreateApiKeyResponse
createApiKey = Request.request serviceName "createApiKey" 


-- | <p>Creates a <code>DataSource</code> object.</p>
createDataSource :: forall eff. CreateDataSourceRequest -> Aff (exception :: EXCEPTION | eff) CreateDataSourceResponse
createDataSource = Request.request serviceName "createDataSource" 


-- | <p>Creates a <code>GraphqlApi</code> object.</p>
createGraphqlApi :: forall eff. CreateGraphqlApiRequest -> Aff (exception :: EXCEPTION | eff) CreateGraphqlApiResponse
createGraphqlApi = Request.request serviceName "createGraphqlApi" 


-- | <p>Creates a <code>Resolver</code> object.</p> <p>A resolver converts incoming requests into a format that a data source can understand and converts the data source's responses into GraphQL.</p>
createResolver :: forall eff. CreateResolverRequest -> Aff (exception :: EXCEPTION | eff) CreateResolverResponse
createResolver = Request.request serviceName "createResolver" 


-- | <p>Creates a <code>Type</code> object.</p>
createType :: forall eff. CreateTypeRequest -> Aff (exception :: EXCEPTION | eff) CreateTypeResponse
createType = Request.request serviceName "createType" 


-- | <p>Deletes an API key.</p>
deleteApiKey :: forall eff. DeleteApiKeyRequest -> Aff (exception :: EXCEPTION | eff) DeleteApiKeyResponse
deleteApiKey = Request.request serviceName "deleteApiKey" 


-- | <p>Deletes a <code>DataSource</code> object.</p>
deleteDataSource :: forall eff. DeleteDataSourceRequest -> Aff (exception :: EXCEPTION | eff) DeleteDataSourceResponse
deleteDataSource = Request.request serviceName "deleteDataSource" 


-- | <p>Deletes a <code>GraphqlApi</code> object.</p>
deleteGraphqlApi :: forall eff. DeleteGraphqlApiRequest -> Aff (exception :: EXCEPTION | eff) DeleteGraphqlApiResponse
deleteGraphqlApi = Request.request serviceName "deleteGraphqlApi" 


-- | <p>Deletes a <code>Resolver</code> object.</p>
deleteResolver :: forall eff. DeleteResolverRequest -> Aff (exception :: EXCEPTION | eff) DeleteResolverResponse
deleteResolver = Request.request serviceName "deleteResolver" 


-- | <p>Deletes a <code>Type</code> object.</p>
deleteType :: forall eff. DeleteTypeRequest -> Aff (exception :: EXCEPTION | eff) DeleteTypeResponse
deleteType = Request.request serviceName "deleteType" 


-- | <p>Retrieves a <code>DataSource</code> object.</p>
getDataSource :: forall eff. GetDataSourceRequest -> Aff (exception :: EXCEPTION | eff) GetDataSourceResponse
getDataSource = Request.request serviceName "getDataSource" 


-- | <p>Retrieves a <code>GraphqlApi</code> object.</p>
getGraphqlApi :: forall eff. GetGraphqlApiRequest -> Aff (exception :: EXCEPTION | eff) GetGraphqlApiResponse
getGraphqlApi = Request.request serviceName "getGraphqlApi" 


-- | <p>Retrieves the introspection schema for a GraphQL API.</p>
getIntrospectionSchema :: forall eff. GetIntrospectionSchemaRequest -> Aff (exception :: EXCEPTION | eff) GetIntrospectionSchemaResponse
getIntrospectionSchema = Request.request serviceName "getIntrospectionSchema" 


-- | <p>Retrieves a <code>Resolver</code> object.</p>
getResolver :: forall eff. GetResolverRequest -> Aff (exception :: EXCEPTION | eff) GetResolverResponse
getResolver = Request.request serviceName "getResolver" 


-- | <p>Retrieves the current status of a schema creation operation.</p>
getSchemaCreationStatus :: forall eff. GetSchemaCreationStatusRequest -> Aff (exception :: EXCEPTION | eff) GetSchemaCreationStatusResponse
getSchemaCreationStatus = Request.request serviceName "getSchemaCreationStatus" 


-- | <p>Retrieves a <code>Type</code> object.</p>
getType :: forall eff. GetTypeRequest -> Aff (exception :: EXCEPTION | eff) GetTypeResponse
getType = Request.request serviceName "getType" 


-- | <p>Lists the API keys for a given API.</p>
listApiKeys :: forall eff. ListApiKeysRequest -> Aff (exception :: EXCEPTION | eff) ListApiKeysResponse
listApiKeys = Request.request serviceName "listApiKeys" 


-- | <p>Lists the data sources for a given API.</p>
listDataSources :: forall eff. ListDataSourcesRequest -> Aff (exception :: EXCEPTION | eff) ListDataSourcesResponse
listDataSources = Request.request serviceName "listDataSources" 


-- | <p>Lists your GraphQL APIs.</p>
listGraphqlApis :: forall eff. ListGraphqlApisRequest -> Aff (exception :: EXCEPTION | eff) ListGraphqlApisResponse
listGraphqlApis = Request.request serviceName "listGraphqlApis" 


-- | <p>Lists the resolvers for a given API and type.</p>
listResolvers :: forall eff. ListResolversRequest -> Aff (exception :: EXCEPTION | eff) ListResolversResponse
listResolvers = Request.request serviceName "listResolvers" 


-- | <p>Lists the types for a given API.</p>
listTypes :: forall eff. ListTypesRequest -> Aff (exception :: EXCEPTION | eff) ListTypesResponse
listTypes = Request.request serviceName "listTypes" 


-- | <p>Adds a new schema to your GraphQL API.</p> <p>This operation is asynchronous. Use to determine when it has completed.</p>
startSchemaCreation :: forall eff. StartSchemaCreationRequest -> Aff (exception :: EXCEPTION | eff) StartSchemaCreationResponse
startSchemaCreation = Request.request serviceName "startSchemaCreation" 


-- | <p>Updates an API key.</p>
updateApiKey :: forall eff. UpdateApiKeyRequest -> Aff (exception :: EXCEPTION | eff) UpdateApiKeyResponse
updateApiKey = Request.request serviceName "updateApiKey" 


-- | <p>Updates a <code>DataSource</code> object.</p>
updateDataSource :: forall eff. UpdateDataSourceRequest -> Aff (exception :: EXCEPTION | eff) UpdateDataSourceResponse
updateDataSource = Request.request serviceName "updateDataSource" 


-- | <p>Updates a <code>GraphqlApi</code> object.</p>
updateGraphqlApi :: forall eff. UpdateGraphqlApiRequest -> Aff (exception :: EXCEPTION | eff) UpdateGraphqlApiResponse
updateGraphqlApi = Request.request serviceName "updateGraphqlApi" 


-- | <p>Updates a <code>Resolver</code> object.</p>
updateResolver :: forall eff. UpdateResolverRequest -> Aff (exception :: EXCEPTION | eff) UpdateResolverResponse
updateResolver = Request.request serviceName "updateResolver" 


-- | <p>Updates a <code>Type</code> object.</p>
updateType :: forall eff. UpdateTypeRequest -> Aff (exception :: EXCEPTION | eff) UpdateTypeResponse
updateType = Request.request serviceName "updateType" 


-- | <p>Describes an API key.</p>
newtype ApiKey = ApiKey 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Expires'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeApiKey :: Newtype ApiKey _
derive instance repGenericApiKey :: Generic ApiKey _
instance showApiKey :: Show ApiKey where
  show = genericShow
instance decodeApiKey :: Decode ApiKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApiKey :: Encode ApiKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The API key exceeded a limit. Try your request again.</p>
newtype ApiKeyLimitExceededException = ApiKeyLimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeApiKeyLimitExceededException :: Newtype ApiKeyLimitExceededException _
derive instance repGenericApiKeyLimitExceededException :: Generic ApiKeyLimitExceededException _
instance showApiKeyLimitExceededException :: Show ApiKeyLimitExceededException where
  show = genericShow
instance decodeApiKeyLimitExceededException :: Decode ApiKeyLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApiKeyLimitExceededException :: Encode ApiKeyLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The API key expiration must be set to a value between 1 and 365 days.</p>
newtype ApiKeyValidityOutOfBoundsException = ApiKeyValidityOutOfBoundsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeApiKeyValidityOutOfBoundsException :: Newtype ApiKeyValidityOutOfBoundsException _
derive instance repGenericApiKeyValidityOutOfBoundsException :: Generic ApiKeyValidityOutOfBoundsException _
instance showApiKeyValidityOutOfBoundsException :: Show ApiKeyValidityOutOfBoundsException where
  show = genericShow
instance decodeApiKeyValidityOutOfBoundsException :: Decode ApiKeyValidityOutOfBoundsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApiKeyValidityOutOfBoundsException :: Encode ApiKeyValidityOutOfBoundsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApiKeys = ApiKeys (Array ApiKey)
derive instance newtypeApiKeys :: Newtype ApiKeys _
derive instance repGenericApiKeys :: Generic ApiKeys _
instance showApiKeys :: Show ApiKeys where
  show = genericShow
instance decodeApiKeys :: Decode ApiKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApiKeys :: Encode ApiKeys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The GraphQL API exceeded a limit. Try your request again.</p>
newtype ApiLimitExceededException = ApiLimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeApiLimitExceededException :: Newtype ApiLimitExceededException _
derive instance repGenericApiLimitExceededException :: Generic ApiLimitExceededException _
instance showApiLimitExceededException :: Show ApiLimitExceededException where
  show = genericShow
instance decodeApiLimitExceededException :: Decode ApiLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApiLimitExceededException :: Encode ApiLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthenticationType = AuthenticationType String
derive instance newtypeAuthenticationType :: Newtype AuthenticationType _
derive instance repGenericAuthenticationType :: Generic AuthenticationType _
instance showAuthenticationType :: Show AuthenticationType where
  show = genericShow
instance decodeAuthenticationType :: Decode AuthenticationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthenticationType :: Encode AuthenticationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request is not well formed. For example, a value is invalid or a required field is missing. Check the field values, and try again. </p>
newtype BadRequestException = BadRequestException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _
derive instance repGenericBadRequestException :: Generic BadRequestException _
instance showBadRequestException :: Show BadRequestException where
  show = genericShow
instance decodeBadRequestException :: Decode BadRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequestException :: Encode BadRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Another modification is being made. That modification must complete before you can make your change. </p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _
derive instance repGenericConcurrentModificationException :: Generic ConcurrentModificationException _
instance showConcurrentModificationException :: Show ConcurrentModificationException where
  show = genericShow
instance decodeConcurrentModificationException :: Decode ConcurrentModificationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConcurrentModificationException :: Encode ConcurrentModificationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateApiKeyRequest = CreateApiKeyRequest 
  { "ApiId'" :: (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Expires'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeCreateApiKeyRequest :: Newtype CreateApiKeyRequest _
derive instance repGenericCreateApiKeyRequest :: Generic CreateApiKeyRequest _
instance showCreateApiKeyRequest :: Show CreateApiKeyRequest where
  show = genericShow
instance decodeCreateApiKeyRequest :: Decode CreateApiKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApiKeyRequest :: Encode CreateApiKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateApiKeyResponse = CreateApiKeyResponse 
  { "ApiKey'" :: NullOrUndefined.NullOrUndefined (ApiKey)
  }
derive instance newtypeCreateApiKeyResponse :: Newtype CreateApiKeyResponse _
derive instance repGenericCreateApiKeyResponse :: Generic CreateApiKeyResponse _
instance showCreateApiKeyResponse :: Show CreateApiKeyResponse where
  show = genericShow
instance decodeCreateApiKeyResponse :: Decode CreateApiKeyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApiKeyResponse :: Encode CreateApiKeyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDataSourceRequest = CreateDataSourceRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (ResourceName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Type'" :: (DataSourceType)
  , "ServiceRoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "DynamodbConfig'" :: NullOrUndefined.NullOrUndefined (DynamodbDataSourceConfig)
  , "LambdaConfig'" :: NullOrUndefined.NullOrUndefined (LambdaDataSourceConfig)
  , "ElasticsearchConfig'" :: NullOrUndefined.NullOrUndefined (ElasticsearchDataSourceConfig)
  }
derive instance newtypeCreateDataSourceRequest :: Newtype CreateDataSourceRequest _
derive instance repGenericCreateDataSourceRequest :: Generic CreateDataSourceRequest _
instance showCreateDataSourceRequest :: Show CreateDataSourceRequest where
  show = genericShow
instance decodeCreateDataSourceRequest :: Decode CreateDataSourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDataSourceRequest :: Encode CreateDataSourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDataSourceResponse = CreateDataSourceResponse 
  { "DataSource'" :: NullOrUndefined.NullOrUndefined (DataSource)
  }
derive instance newtypeCreateDataSourceResponse :: Newtype CreateDataSourceResponse _
derive instance repGenericCreateDataSourceResponse :: Generic CreateDataSourceResponse _
instance showCreateDataSourceResponse :: Show CreateDataSourceResponse where
  show = genericShow
instance decodeCreateDataSourceResponse :: Decode CreateDataSourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDataSourceResponse :: Encode CreateDataSourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGraphqlApiRequest = CreateGraphqlApiRequest 
  { "Name'" :: (String)
  , "AuthenticationType'" :: (AuthenticationType)
  , "UserPoolConfig'" :: NullOrUndefined.NullOrUndefined (UserPoolConfig)
  }
derive instance newtypeCreateGraphqlApiRequest :: Newtype CreateGraphqlApiRequest _
derive instance repGenericCreateGraphqlApiRequest :: Generic CreateGraphqlApiRequest _
instance showCreateGraphqlApiRequest :: Show CreateGraphqlApiRequest where
  show = genericShow
instance decodeCreateGraphqlApiRequest :: Decode CreateGraphqlApiRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGraphqlApiRequest :: Encode CreateGraphqlApiRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGraphqlApiResponse = CreateGraphqlApiResponse 
  { "GraphqlApi'" :: NullOrUndefined.NullOrUndefined (GraphqlApi)
  }
derive instance newtypeCreateGraphqlApiResponse :: Newtype CreateGraphqlApiResponse _
derive instance repGenericCreateGraphqlApiResponse :: Generic CreateGraphqlApiResponse _
instance showCreateGraphqlApiResponse :: Show CreateGraphqlApiResponse where
  show = genericShow
instance decodeCreateGraphqlApiResponse :: Decode CreateGraphqlApiResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGraphqlApiResponse :: Encode CreateGraphqlApiResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateResolverRequest = CreateResolverRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "FieldName'" :: (ResourceName)
  , "DataSourceName'" :: (ResourceName)
  , "RequestMappingTemplate'" :: (MappingTemplate)
  , "ResponseMappingTemplate'" :: NullOrUndefined.NullOrUndefined (MappingTemplate)
  }
derive instance newtypeCreateResolverRequest :: Newtype CreateResolverRequest _
derive instance repGenericCreateResolverRequest :: Generic CreateResolverRequest _
instance showCreateResolverRequest :: Show CreateResolverRequest where
  show = genericShow
instance decodeCreateResolverRequest :: Decode CreateResolverRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResolverRequest :: Encode CreateResolverRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateResolverResponse = CreateResolverResponse 
  { "Resolver'" :: NullOrUndefined.NullOrUndefined (Resolver)
  }
derive instance newtypeCreateResolverResponse :: Newtype CreateResolverResponse _
derive instance repGenericCreateResolverResponse :: Generic CreateResolverResponse _
instance showCreateResolverResponse :: Show CreateResolverResponse where
  show = genericShow
instance decodeCreateResolverResponse :: Decode CreateResolverResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResolverResponse :: Encode CreateResolverResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTypeRequest = CreateTypeRequest 
  { "ApiId'" :: (String)
  , "Definition'" :: (String)
  , "Format'" :: (TypeDefinitionFormat)
  }
derive instance newtypeCreateTypeRequest :: Newtype CreateTypeRequest _
derive instance repGenericCreateTypeRequest :: Generic CreateTypeRequest _
instance showCreateTypeRequest :: Show CreateTypeRequest where
  show = genericShow
instance decodeCreateTypeRequest :: Decode CreateTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTypeRequest :: Encode CreateTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTypeResponse = CreateTypeResponse 
  { "Type'" :: NullOrUndefined.NullOrUndefined (Type)
  }
derive instance newtypeCreateTypeResponse :: Newtype CreateTypeResponse _
derive instance repGenericCreateTypeResponse :: Generic CreateTypeResponse _
instance showCreateTypeResponse :: Show CreateTypeResponse where
  show = genericShow
instance decodeCreateTypeResponse :: Decode CreateTypeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTypeResponse :: Encode CreateTypeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a data source.</p>
newtype DataSource = DataSource 
  { "DataSourceArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Type'" :: NullOrUndefined.NullOrUndefined (DataSourceType)
  , "ServiceRoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "DynamodbConfig'" :: NullOrUndefined.NullOrUndefined (DynamodbDataSourceConfig)
  , "LambdaConfig'" :: NullOrUndefined.NullOrUndefined (LambdaDataSourceConfig)
  , "ElasticsearchConfig'" :: NullOrUndefined.NullOrUndefined (ElasticsearchDataSourceConfig)
  }
derive instance newtypeDataSource :: Newtype DataSource _
derive instance repGenericDataSource :: Generic DataSource _
instance showDataSource :: Show DataSource where
  show = genericShow
instance decodeDataSource :: Decode DataSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataSource :: Encode DataSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataSourceType = DataSourceType String
derive instance newtypeDataSourceType :: Newtype DataSourceType _
derive instance repGenericDataSourceType :: Generic DataSourceType _
instance showDataSourceType :: Show DataSourceType where
  show = genericShow
instance decodeDataSourceType :: Decode DataSourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataSourceType :: Encode DataSourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataSources = DataSources (Array DataSource)
derive instance newtypeDataSources :: Newtype DataSources _
derive instance repGenericDataSources :: Generic DataSources _
instance showDataSources :: Show DataSources where
  show = genericShow
instance decodeDataSources :: Decode DataSources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataSources :: Encode DataSources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DefaultAction = DefaultAction String
derive instance newtypeDefaultAction :: Newtype DefaultAction _
derive instance repGenericDefaultAction :: Generic DefaultAction _
instance showDefaultAction :: Show DefaultAction where
  show = genericShow
instance decodeDefaultAction :: Decode DefaultAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultAction :: Encode DefaultAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApiKeyRequest = DeleteApiKeyRequest 
  { "ApiId'" :: (String)
  , "Id'" :: (String)
  }
derive instance newtypeDeleteApiKeyRequest :: Newtype DeleteApiKeyRequest _
derive instance repGenericDeleteApiKeyRequest :: Generic DeleteApiKeyRequest _
instance showDeleteApiKeyRequest :: Show DeleteApiKeyRequest where
  show = genericShow
instance decodeDeleteApiKeyRequest :: Decode DeleteApiKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApiKeyRequest :: Encode DeleteApiKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApiKeyResponse = DeleteApiKeyResponse Types.NoArguments
derive instance newtypeDeleteApiKeyResponse :: Newtype DeleteApiKeyResponse _
derive instance repGenericDeleteApiKeyResponse :: Generic DeleteApiKeyResponse _
instance showDeleteApiKeyResponse :: Show DeleteApiKeyResponse where
  show = genericShow
instance decodeDeleteApiKeyResponse :: Decode DeleteApiKeyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApiKeyResponse :: Encode DeleteApiKeyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDataSourceRequest = DeleteDataSourceRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (ResourceName)
  }
derive instance newtypeDeleteDataSourceRequest :: Newtype DeleteDataSourceRequest _
derive instance repGenericDeleteDataSourceRequest :: Generic DeleteDataSourceRequest _
instance showDeleteDataSourceRequest :: Show DeleteDataSourceRequest where
  show = genericShow
instance decodeDeleteDataSourceRequest :: Decode DeleteDataSourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDataSourceRequest :: Encode DeleteDataSourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDataSourceResponse = DeleteDataSourceResponse Types.NoArguments
derive instance newtypeDeleteDataSourceResponse :: Newtype DeleteDataSourceResponse _
derive instance repGenericDeleteDataSourceResponse :: Generic DeleteDataSourceResponse _
instance showDeleteDataSourceResponse :: Show DeleteDataSourceResponse where
  show = genericShow
instance decodeDeleteDataSourceResponse :: Decode DeleteDataSourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDataSourceResponse :: Encode DeleteDataSourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGraphqlApiRequest = DeleteGraphqlApiRequest 
  { "ApiId'" :: (String)
  }
derive instance newtypeDeleteGraphqlApiRequest :: Newtype DeleteGraphqlApiRequest _
derive instance repGenericDeleteGraphqlApiRequest :: Generic DeleteGraphqlApiRequest _
instance showDeleteGraphqlApiRequest :: Show DeleteGraphqlApiRequest where
  show = genericShow
instance decodeDeleteGraphqlApiRequest :: Decode DeleteGraphqlApiRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGraphqlApiRequest :: Encode DeleteGraphqlApiRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGraphqlApiResponse = DeleteGraphqlApiResponse Types.NoArguments
derive instance newtypeDeleteGraphqlApiResponse :: Newtype DeleteGraphqlApiResponse _
derive instance repGenericDeleteGraphqlApiResponse :: Generic DeleteGraphqlApiResponse _
instance showDeleteGraphqlApiResponse :: Show DeleteGraphqlApiResponse where
  show = genericShow
instance decodeDeleteGraphqlApiResponse :: Decode DeleteGraphqlApiResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGraphqlApiResponse :: Encode DeleteGraphqlApiResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteResolverRequest = DeleteResolverRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "FieldName'" :: (ResourceName)
  }
derive instance newtypeDeleteResolverRequest :: Newtype DeleteResolverRequest _
derive instance repGenericDeleteResolverRequest :: Generic DeleteResolverRequest _
instance showDeleteResolverRequest :: Show DeleteResolverRequest where
  show = genericShow
instance decodeDeleteResolverRequest :: Decode DeleteResolverRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteResolverRequest :: Encode DeleteResolverRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteResolverResponse = DeleteResolverResponse Types.NoArguments
derive instance newtypeDeleteResolverResponse :: Newtype DeleteResolverResponse _
derive instance repGenericDeleteResolverResponse :: Generic DeleteResolverResponse _
instance showDeleteResolverResponse :: Show DeleteResolverResponse where
  show = genericShow
instance decodeDeleteResolverResponse :: Decode DeleteResolverResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteResolverResponse :: Encode DeleteResolverResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTypeRequest = DeleteTypeRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  }
derive instance newtypeDeleteTypeRequest :: Newtype DeleteTypeRequest _
derive instance repGenericDeleteTypeRequest :: Generic DeleteTypeRequest _
instance showDeleteTypeRequest :: Show DeleteTypeRequest where
  show = genericShow
instance decodeDeleteTypeRequest :: Decode DeleteTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTypeRequest :: Encode DeleteTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTypeResponse = DeleteTypeResponse Types.NoArguments
derive instance newtypeDeleteTypeResponse :: Newtype DeleteTypeResponse _
derive instance repGenericDeleteTypeResponse :: Generic DeleteTypeResponse _
instance showDeleteTypeResponse :: Show DeleteTypeResponse where
  show = genericShow
instance decodeDeleteTypeResponse :: Decode DeleteTypeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTypeResponse :: Encode DeleteTypeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a DynamoDB data source configuration.</p>
newtype DynamodbDataSourceConfig = DynamodbDataSourceConfig 
  { "TableName'" :: (String)
  , "AwsRegion'" :: (String)
  , "UseCallerCredentials'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeDynamodbDataSourceConfig :: Newtype DynamodbDataSourceConfig _
derive instance repGenericDynamodbDataSourceConfig :: Generic DynamodbDataSourceConfig _
instance showDynamodbDataSourceConfig :: Show DynamodbDataSourceConfig where
  show = genericShow
instance decodeDynamodbDataSourceConfig :: Decode DynamodbDataSourceConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDynamodbDataSourceConfig :: Encode DynamodbDataSourceConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an Elasticsearch data source configuration.</p>
newtype ElasticsearchDataSourceConfig = ElasticsearchDataSourceConfig 
  { "Endpoint'" :: (String)
  , "AwsRegion'" :: (String)
  }
derive instance newtypeElasticsearchDataSourceConfig :: Newtype ElasticsearchDataSourceConfig _
derive instance repGenericElasticsearchDataSourceConfig :: Generic ElasticsearchDataSourceConfig _
instance showElasticsearchDataSourceConfig :: Show ElasticsearchDataSourceConfig where
  show = genericShow
instance decodeElasticsearchDataSourceConfig :: Decode ElasticsearchDataSourceConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchDataSourceConfig :: Encode ElasticsearchDataSourceConfig where
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


newtype GetDataSourceRequest = GetDataSourceRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (ResourceName)
  }
derive instance newtypeGetDataSourceRequest :: Newtype GetDataSourceRequest _
derive instance repGenericGetDataSourceRequest :: Generic GetDataSourceRequest _
instance showGetDataSourceRequest :: Show GetDataSourceRequest where
  show = genericShow
instance decodeGetDataSourceRequest :: Decode GetDataSourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDataSourceRequest :: Encode GetDataSourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDataSourceResponse = GetDataSourceResponse 
  { "DataSource'" :: NullOrUndefined.NullOrUndefined (DataSource)
  }
derive instance newtypeGetDataSourceResponse :: Newtype GetDataSourceResponse _
derive instance repGenericGetDataSourceResponse :: Generic GetDataSourceResponse _
instance showGetDataSourceResponse :: Show GetDataSourceResponse where
  show = genericShow
instance decodeGetDataSourceResponse :: Decode GetDataSourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDataSourceResponse :: Encode GetDataSourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGraphqlApiRequest = GetGraphqlApiRequest 
  { "ApiId'" :: (String)
  }
derive instance newtypeGetGraphqlApiRequest :: Newtype GetGraphqlApiRequest _
derive instance repGenericGetGraphqlApiRequest :: Generic GetGraphqlApiRequest _
instance showGetGraphqlApiRequest :: Show GetGraphqlApiRequest where
  show = genericShow
instance decodeGetGraphqlApiRequest :: Decode GetGraphqlApiRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGraphqlApiRequest :: Encode GetGraphqlApiRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGraphqlApiResponse = GetGraphqlApiResponse 
  { "GraphqlApi'" :: NullOrUndefined.NullOrUndefined (GraphqlApi)
  }
derive instance newtypeGetGraphqlApiResponse :: Newtype GetGraphqlApiResponse _
derive instance repGenericGetGraphqlApiResponse :: Generic GetGraphqlApiResponse _
instance showGetGraphqlApiResponse :: Show GetGraphqlApiResponse where
  show = genericShow
instance decodeGetGraphqlApiResponse :: Decode GetGraphqlApiResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGraphqlApiResponse :: Encode GetGraphqlApiResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIntrospectionSchemaRequest = GetIntrospectionSchemaRequest 
  { "ApiId'" :: (String)
  , "Format'" :: (OutputType)
  }
derive instance newtypeGetIntrospectionSchemaRequest :: Newtype GetIntrospectionSchemaRequest _
derive instance repGenericGetIntrospectionSchemaRequest :: Generic GetIntrospectionSchemaRequest _
instance showGetIntrospectionSchemaRequest :: Show GetIntrospectionSchemaRequest where
  show = genericShow
instance decodeGetIntrospectionSchemaRequest :: Decode GetIntrospectionSchemaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIntrospectionSchemaRequest :: Encode GetIntrospectionSchemaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIntrospectionSchemaResponse = GetIntrospectionSchemaResponse 
  { "Schema'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetIntrospectionSchemaResponse :: Newtype GetIntrospectionSchemaResponse _
derive instance repGenericGetIntrospectionSchemaResponse :: Generic GetIntrospectionSchemaResponse _
instance showGetIntrospectionSchemaResponse :: Show GetIntrospectionSchemaResponse where
  show = genericShow
instance decodeGetIntrospectionSchemaResponse :: Decode GetIntrospectionSchemaResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIntrospectionSchemaResponse :: Encode GetIntrospectionSchemaResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetResolverRequest = GetResolverRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "FieldName'" :: (ResourceName)
  }
derive instance newtypeGetResolverRequest :: Newtype GetResolverRequest _
derive instance repGenericGetResolverRequest :: Generic GetResolverRequest _
instance showGetResolverRequest :: Show GetResolverRequest where
  show = genericShow
instance decodeGetResolverRequest :: Decode GetResolverRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetResolverRequest :: Encode GetResolverRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetResolverResponse = GetResolverResponse 
  { "Resolver'" :: NullOrUndefined.NullOrUndefined (Resolver)
  }
derive instance newtypeGetResolverResponse :: Newtype GetResolverResponse _
derive instance repGenericGetResolverResponse :: Generic GetResolverResponse _
instance showGetResolverResponse :: Show GetResolverResponse where
  show = genericShow
instance decodeGetResolverResponse :: Decode GetResolverResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetResolverResponse :: Encode GetResolverResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSchemaCreationStatusRequest = GetSchemaCreationStatusRequest 
  { "ApiId'" :: (String)
  }
derive instance newtypeGetSchemaCreationStatusRequest :: Newtype GetSchemaCreationStatusRequest _
derive instance repGenericGetSchemaCreationStatusRequest :: Generic GetSchemaCreationStatusRequest _
instance showGetSchemaCreationStatusRequest :: Show GetSchemaCreationStatusRequest where
  show = genericShow
instance decodeGetSchemaCreationStatusRequest :: Decode GetSchemaCreationStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSchemaCreationStatusRequest :: Encode GetSchemaCreationStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSchemaCreationStatusResponse = GetSchemaCreationStatusResponse 
  { "Status'" :: NullOrUndefined.NullOrUndefined (SchemaStatus)
  , "Details'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetSchemaCreationStatusResponse :: Newtype GetSchemaCreationStatusResponse _
derive instance repGenericGetSchemaCreationStatusResponse :: Generic GetSchemaCreationStatusResponse _
instance showGetSchemaCreationStatusResponse :: Show GetSchemaCreationStatusResponse where
  show = genericShow
instance decodeGetSchemaCreationStatusResponse :: Decode GetSchemaCreationStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSchemaCreationStatusResponse :: Encode GetSchemaCreationStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTypeRequest = GetTypeRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "Format'" :: (TypeDefinitionFormat)
  }
derive instance newtypeGetTypeRequest :: Newtype GetTypeRequest _
derive instance repGenericGetTypeRequest :: Generic GetTypeRequest _
instance showGetTypeRequest :: Show GetTypeRequest where
  show = genericShow
instance decodeGetTypeRequest :: Decode GetTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTypeRequest :: Encode GetTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTypeResponse = GetTypeResponse 
  { "Type'" :: NullOrUndefined.NullOrUndefined (Type)
  }
derive instance newtypeGetTypeResponse :: Newtype GetTypeResponse _
derive instance repGenericGetTypeResponse :: Generic GetTypeResponse _
instance showGetTypeResponse :: Show GetTypeResponse where
  show = genericShow
instance decodeGetTypeResponse :: Decode GetTypeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTypeResponse :: Encode GetTypeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The GraphQL schema is not valid.</p>
newtype GraphQLSchemaException = GraphQLSchemaException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeGraphQLSchemaException :: Newtype GraphQLSchemaException _
derive instance repGenericGraphQLSchemaException :: Generic GraphQLSchemaException _
instance showGraphQLSchemaException :: Show GraphQLSchemaException where
  show = genericShow
instance decodeGraphQLSchemaException :: Decode GraphQLSchemaException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGraphQLSchemaException :: Encode GraphQLSchemaException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a GraphQL API.</p>
newtype GraphqlApi = GraphqlApi 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "ApiId'" :: NullOrUndefined.NullOrUndefined (String)
  , "AuthenticationType'" :: NullOrUndefined.NullOrUndefined (AuthenticationType)
  , "UserPoolConfig'" :: NullOrUndefined.NullOrUndefined (UserPoolConfig)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (String)
  , "Uris'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  }
derive instance newtypeGraphqlApi :: Newtype GraphqlApi _
derive instance repGenericGraphqlApi :: Generic GraphqlApi _
instance showGraphqlApi :: Show GraphqlApi where
  show = genericShow
instance decodeGraphqlApi :: Decode GraphqlApi where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGraphqlApi :: Encode GraphqlApi where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GraphqlApis = GraphqlApis (Array GraphqlApi)
derive instance newtypeGraphqlApis :: Newtype GraphqlApis _
derive instance repGenericGraphqlApis :: Generic GraphqlApis _
instance showGraphqlApis :: Show GraphqlApis where
  show = genericShow
instance decodeGraphqlApis :: Decode GraphqlApis where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGraphqlApis :: Encode GraphqlApis where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An internal AWS AppSync error occurred. Try your request again.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalFailureException :: Newtype InternalFailureException _
derive instance repGenericInternalFailureException :: Generic InternalFailureException _
instance showInternalFailureException :: Show InternalFailureException where
  show = genericShow
instance decodeInternalFailureException :: Decode InternalFailureException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalFailureException :: Encode InternalFailureException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a Lambda data source configuration.</p>
newtype LambdaDataSourceConfig = LambdaDataSourceConfig 
  { "LambdaFunctionArn'" :: (String)
  }
derive instance newtypeLambdaDataSourceConfig :: Newtype LambdaDataSourceConfig _
derive instance repGenericLambdaDataSourceConfig :: Generic LambdaDataSourceConfig _
instance showLambdaDataSourceConfig :: Show LambdaDataSourceConfig where
  show = genericShow
instance decodeLambdaDataSourceConfig :: Decode LambdaDataSourceConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaDataSourceConfig :: Encode LambdaDataSourceConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request exceeded a limit. Try your request again.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListApiKeysRequest = ListApiKeysRequest 
  { "ApiId'" :: (String)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListApiKeysRequest :: Newtype ListApiKeysRequest _
derive instance repGenericListApiKeysRequest :: Generic ListApiKeysRequest _
instance showListApiKeysRequest :: Show ListApiKeysRequest where
  show = genericShow
instance decodeListApiKeysRequest :: Decode ListApiKeysRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListApiKeysRequest :: Encode ListApiKeysRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListApiKeysResponse = ListApiKeysResponse 
  { "ApiKeys'" :: NullOrUndefined.NullOrUndefined (ApiKeys)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListApiKeysResponse :: Newtype ListApiKeysResponse _
derive instance repGenericListApiKeysResponse :: Generic ListApiKeysResponse _
instance showListApiKeysResponse :: Show ListApiKeysResponse where
  show = genericShow
instance decodeListApiKeysResponse :: Decode ListApiKeysResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListApiKeysResponse :: Encode ListApiKeysResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDataSourcesRequest = ListDataSourcesRequest 
  { "ApiId'" :: (String)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListDataSourcesRequest :: Newtype ListDataSourcesRequest _
derive instance repGenericListDataSourcesRequest :: Generic ListDataSourcesRequest _
instance showListDataSourcesRequest :: Show ListDataSourcesRequest where
  show = genericShow
instance decodeListDataSourcesRequest :: Decode ListDataSourcesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDataSourcesRequest :: Encode ListDataSourcesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDataSourcesResponse = ListDataSourcesResponse 
  { "DataSources'" :: NullOrUndefined.NullOrUndefined (DataSources)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListDataSourcesResponse :: Newtype ListDataSourcesResponse _
derive instance repGenericListDataSourcesResponse :: Generic ListDataSourcesResponse _
instance showListDataSourcesResponse :: Show ListDataSourcesResponse where
  show = genericShow
instance decodeListDataSourcesResponse :: Decode ListDataSourcesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDataSourcesResponse :: Encode ListDataSourcesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGraphqlApisRequest = ListGraphqlApisRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListGraphqlApisRequest :: Newtype ListGraphqlApisRequest _
derive instance repGenericListGraphqlApisRequest :: Generic ListGraphqlApisRequest _
instance showListGraphqlApisRequest :: Show ListGraphqlApisRequest where
  show = genericShow
instance decodeListGraphqlApisRequest :: Decode ListGraphqlApisRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGraphqlApisRequest :: Encode ListGraphqlApisRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGraphqlApisResponse = ListGraphqlApisResponse 
  { "GraphqlApis'" :: NullOrUndefined.NullOrUndefined (GraphqlApis)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListGraphqlApisResponse :: Newtype ListGraphqlApisResponse _
derive instance repGenericListGraphqlApisResponse :: Generic ListGraphqlApisResponse _
instance showListGraphqlApisResponse :: Show ListGraphqlApisResponse where
  show = genericShow
instance decodeListGraphqlApisResponse :: Decode ListGraphqlApisResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGraphqlApisResponse :: Encode ListGraphqlApisResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResolversRequest = ListResolversRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (String)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListResolversRequest :: Newtype ListResolversRequest _
derive instance repGenericListResolversRequest :: Generic ListResolversRequest _
instance showListResolversRequest :: Show ListResolversRequest where
  show = genericShow
instance decodeListResolversRequest :: Decode ListResolversRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResolversRequest :: Encode ListResolversRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResolversResponse = ListResolversResponse 
  { "Resolvers'" :: NullOrUndefined.NullOrUndefined (Resolvers)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListResolversResponse :: Newtype ListResolversResponse _
derive instance repGenericListResolversResponse :: Generic ListResolversResponse _
instance showListResolversResponse :: Show ListResolversResponse where
  show = genericShow
instance decodeListResolversResponse :: Decode ListResolversResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResolversResponse :: Encode ListResolversResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTypesRequest = ListTypesRequest 
  { "ApiId'" :: (String)
  , "Format'" :: (TypeDefinitionFormat)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListTypesRequest :: Newtype ListTypesRequest _
derive instance repGenericListTypesRequest :: Generic ListTypesRequest _
instance showListTypesRequest :: Show ListTypesRequest where
  show = genericShow
instance decodeListTypesRequest :: Decode ListTypesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTypesRequest :: Encode ListTypesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTypesResponse = ListTypesResponse 
  { "Types'" :: NullOrUndefined.NullOrUndefined (TypeList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListTypesResponse :: Newtype ListTypesResponse _
derive instance repGenericListTypesResponse :: Generic ListTypesResponse _
instance showListTypesResponse :: Show ListTypesResponse where
  show = genericShow
instance decodeListTypesResponse :: Decode ListTypesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTypesResponse :: Encode ListTypesResponse where
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


newtype MappingTemplate = MappingTemplate String
derive instance newtypeMappingTemplate :: Newtype MappingTemplate _
derive instance repGenericMappingTemplate :: Generic MappingTemplate _
instance showMappingTemplate :: Show MappingTemplate where
  show = genericShow
instance decodeMappingTemplate :: Decode MappingTemplate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMappingTemplate :: Encode MappingTemplate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource specified in the request was not found. Check the resource and try again.</p>
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


newtype OutputType = OutputType String
derive instance newtypeOutputType :: Newtype OutputType _
derive instance repGenericOutputType :: Generic OutputType _
instance showOutputType :: Show OutputType where
  show = genericShow
instance decodeOutputType :: Decode OutputType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputType :: Encode OutputType where
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


-- | <p>Describes a resolver.</p>
newtype Resolver = Resolver 
  { "TypeName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "FieldName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "DataSourceName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "ResolverArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestMappingTemplate'" :: NullOrUndefined.NullOrUndefined (MappingTemplate)
  , "ResponseMappingTemplate'" :: NullOrUndefined.NullOrUndefined (MappingTemplate)
  }
derive instance newtypeResolver :: Newtype Resolver _
derive instance repGenericResolver :: Generic Resolver _
instance showResolver :: Show Resolver where
  show = genericShow
instance decodeResolver :: Decode Resolver where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResolver :: Encode Resolver where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Resolvers = Resolvers (Array Resolver)
derive instance newtypeResolvers :: Newtype Resolvers _
derive instance repGenericResolvers :: Generic Resolvers _
instance showResolvers :: Show Resolvers where
  show = genericShow
instance decodeResolvers :: Decode Resolvers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResolvers :: Encode Resolvers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _
derive instance repGenericResourceName :: Generic ResourceName _
instance showResourceName :: Show ResourceName where
  show = genericShow
instance decodeResourceName :: Decode ResourceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceName :: Encode ResourceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SchemaStatus = SchemaStatus String
derive instance newtypeSchemaStatus :: Newtype SchemaStatus _
derive instance repGenericSchemaStatus :: Generic SchemaStatus _
instance showSchemaStatus :: Show SchemaStatus where
  show = genericShow
instance decodeSchemaStatus :: Decode SchemaStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaStatus :: Encode SchemaStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartSchemaCreationRequest = StartSchemaCreationRequest 
  { "ApiId'" :: (String)
  , "Definition'" :: (String)
  }
derive instance newtypeStartSchemaCreationRequest :: Newtype StartSchemaCreationRequest _
derive instance repGenericStartSchemaCreationRequest :: Generic StartSchemaCreationRequest _
instance showStartSchemaCreationRequest :: Show StartSchemaCreationRequest where
  show = genericShow
instance decodeStartSchemaCreationRequest :: Decode StartSchemaCreationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartSchemaCreationRequest :: Encode StartSchemaCreationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartSchemaCreationResponse = StartSchemaCreationResponse 
  { "Status'" :: NullOrUndefined.NullOrUndefined (SchemaStatus)
  }
derive instance newtypeStartSchemaCreationResponse :: Newtype StartSchemaCreationResponse _
derive instance repGenericStartSchemaCreationResponse :: Generic StartSchemaCreationResponse _
instance showStartSchemaCreationResponse :: Show StartSchemaCreationResponse where
  show = genericShow
instance decodeStartSchemaCreationResponse :: Decode StartSchemaCreationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartSchemaCreationResponse :: Encode StartSchemaCreationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a type.</p>
newtype Type = Type 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (String)
  , "Definition'" :: NullOrUndefined.NullOrUndefined (String)
  , "Format'" :: NullOrUndefined.NullOrUndefined (TypeDefinitionFormat)
  }
derive instance newtypeType :: Newtype Type _
derive instance repGenericType :: Generic Type _
instance showType :: Show Type where
  show = genericShow
instance decodeType :: Decode Type where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeType :: Encode Type where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TypeDefinitionFormat = TypeDefinitionFormat String
derive instance newtypeTypeDefinitionFormat :: Newtype TypeDefinitionFormat _
derive instance repGenericTypeDefinitionFormat :: Generic TypeDefinitionFormat _
instance showTypeDefinitionFormat :: Show TypeDefinitionFormat where
  show = genericShow
instance decodeTypeDefinitionFormat :: Decode TypeDefinitionFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypeDefinitionFormat :: Encode TypeDefinitionFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TypeList = TypeList (Array Type)
derive instance newtypeTypeList :: Newtype TypeList _
derive instance repGenericTypeList :: Generic TypeList _
instance showTypeList :: Show TypeList where
  show = genericShow
instance decodeTypeList :: Decode TypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypeList :: Encode TypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You are not authorized to perform this operation.</p>
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


newtype UpdateApiKeyRequest = UpdateApiKeyRequest 
  { "ApiId'" :: (String)
  , "Id'" :: (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Expires'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeUpdateApiKeyRequest :: Newtype UpdateApiKeyRequest _
derive instance repGenericUpdateApiKeyRequest :: Generic UpdateApiKeyRequest _
instance showUpdateApiKeyRequest :: Show UpdateApiKeyRequest where
  show = genericShow
instance decodeUpdateApiKeyRequest :: Decode UpdateApiKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApiKeyRequest :: Encode UpdateApiKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApiKeyResponse = UpdateApiKeyResponse 
  { "ApiKey'" :: NullOrUndefined.NullOrUndefined (ApiKey)
  }
derive instance newtypeUpdateApiKeyResponse :: Newtype UpdateApiKeyResponse _
derive instance repGenericUpdateApiKeyResponse :: Generic UpdateApiKeyResponse _
instance showUpdateApiKeyResponse :: Show UpdateApiKeyResponse where
  show = genericShow
instance decodeUpdateApiKeyResponse :: Decode UpdateApiKeyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApiKeyResponse :: Encode UpdateApiKeyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDataSourceRequest = UpdateDataSourceRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (ResourceName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Type'" :: (DataSourceType)
  , "ServiceRoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "DynamodbConfig'" :: NullOrUndefined.NullOrUndefined (DynamodbDataSourceConfig)
  , "LambdaConfig'" :: NullOrUndefined.NullOrUndefined (LambdaDataSourceConfig)
  , "ElasticsearchConfig'" :: NullOrUndefined.NullOrUndefined (ElasticsearchDataSourceConfig)
  }
derive instance newtypeUpdateDataSourceRequest :: Newtype UpdateDataSourceRequest _
derive instance repGenericUpdateDataSourceRequest :: Generic UpdateDataSourceRequest _
instance showUpdateDataSourceRequest :: Show UpdateDataSourceRequest where
  show = genericShow
instance decodeUpdateDataSourceRequest :: Decode UpdateDataSourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDataSourceRequest :: Encode UpdateDataSourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDataSourceResponse = UpdateDataSourceResponse 
  { "DataSource'" :: NullOrUndefined.NullOrUndefined (DataSource)
  }
derive instance newtypeUpdateDataSourceResponse :: Newtype UpdateDataSourceResponse _
derive instance repGenericUpdateDataSourceResponse :: Generic UpdateDataSourceResponse _
instance showUpdateDataSourceResponse :: Show UpdateDataSourceResponse where
  show = genericShow
instance decodeUpdateDataSourceResponse :: Decode UpdateDataSourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDataSourceResponse :: Encode UpdateDataSourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGraphqlApiRequest = UpdateGraphqlApiRequest 
  { "ApiId'" :: (String)
  , "Name'" :: (String)
  , "AuthenticationType'" :: NullOrUndefined.NullOrUndefined (AuthenticationType)
  , "UserPoolConfig'" :: NullOrUndefined.NullOrUndefined (UserPoolConfig)
  }
derive instance newtypeUpdateGraphqlApiRequest :: Newtype UpdateGraphqlApiRequest _
derive instance repGenericUpdateGraphqlApiRequest :: Generic UpdateGraphqlApiRequest _
instance showUpdateGraphqlApiRequest :: Show UpdateGraphqlApiRequest where
  show = genericShow
instance decodeUpdateGraphqlApiRequest :: Decode UpdateGraphqlApiRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGraphqlApiRequest :: Encode UpdateGraphqlApiRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGraphqlApiResponse = UpdateGraphqlApiResponse 
  { "GraphqlApi'" :: NullOrUndefined.NullOrUndefined (GraphqlApi)
  }
derive instance newtypeUpdateGraphqlApiResponse :: Newtype UpdateGraphqlApiResponse _
derive instance repGenericUpdateGraphqlApiResponse :: Generic UpdateGraphqlApiResponse _
instance showUpdateGraphqlApiResponse :: Show UpdateGraphqlApiResponse where
  show = genericShow
instance decodeUpdateGraphqlApiResponse :: Decode UpdateGraphqlApiResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGraphqlApiResponse :: Encode UpdateGraphqlApiResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateResolverRequest = UpdateResolverRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "FieldName'" :: (ResourceName)
  , "DataSourceName'" :: (ResourceName)
  , "RequestMappingTemplate'" :: (MappingTemplate)
  , "ResponseMappingTemplate'" :: NullOrUndefined.NullOrUndefined (MappingTemplate)
  }
derive instance newtypeUpdateResolverRequest :: Newtype UpdateResolverRequest _
derive instance repGenericUpdateResolverRequest :: Generic UpdateResolverRequest _
instance showUpdateResolverRequest :: Show UpdateResolverRequest where
  show = genericShow
instance decodeUpdateResolverRequest :: Decode UpdateResolverRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateResolverRequest :: Encode UpdateResolverRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateResolverResponse = UpdateResolverResponse 
  { "Resolver'" :: NullOrUndefined.NullOrUndefined (Resolver)
  }
derive instance newtypeUpdateResolverResponse :: Newtype UpdateResolverResponse _
derive instance repGenericUpdateResolverResponse :: Generic UpdateResolverResponse _
instance showUpdateResolverResponse :: Show UpdateResolverResponse where
  show = genericShow
instance decodeUpdateResolverResponse :: Decode UpdateResolverResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateResolverResponse :: Encode UpdateResolverResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTypeRequest = UpdateTypeRequest 
  { "ApiId'" :: (String)
  , "TypeName'" :: (ResourceName)
  , "Definition'" :: NullOrUndefined.NullOrUndefined (String)
  , "Format'" :: (TypeDefinitionFormat)
  }
derive instance newtypeUpdateTypeRequest :: Newtype UpdateTypeRequest _
derive instance repGenericUpdateTypeRequest :: Generic UpdateTypeRequest _
instance showUpdateTypeRequest :: Show UpdateTypeRequest where
  show = genericShow
instance decodeUpdateTypeRequest :: Decode UpdateTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTypeRequest :: Encode UpdateTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTypeResponse = UpdateTypeResponse 
  { "Type'" :: NullOrUndefined.NullOrUndefined (Type)
  }
derive instance newtypeUpdateTypeResponse :: Newtype UpdateTypeResponse _
derive instance repGenericUpdateTypeResponse :: Generic UpdateTypeResponse _
instance showUpdateTypeResponse :: Show UpdateTypeResponse where
  show = genericShow
instance decodeUpdateTypeResponse :: Decode UpdateTypeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTypeResponse :: Encode UpdateTypeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an Amazon Cognito User Pool configuration.</p>
newtype UserPoolConfig = UserPoolConfig 
  { "UserPoolId'" :: (String)
  , "AwsRegion'" :: (String)
  , "DefaultAction'" :: (DefaultAction)
  , "AppIdClientRegex'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUserPoolConfig :: Newtype UserPoolConfig _
derive instance repGenericUserPoolConfig :: Generic UserPoolConfig _
instance showUserPoolConfig :: Show UserPoolConfig where
  show = genericShow
instance decodeUserPoolConfig :: Decode UserPoolConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolConfig :: Encode UserPoolConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
