

-- | <fullname>Amazon Cloud Directory</fullname> <p>Amazon Cloud Directory is a component of the AWS Directory Service that simplifies the development and management of cloud-scale web, mobile, and IoT applications. This guide describes the Cloud Directory operations that you can call programmatically and includes detailed information on data types and errors. For information about AWS Directory Services features, see <a href="https://aws.amazon.com/directoryservice/">AWS Directory Service</a> and the <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html">AWS Directory Service Administration Guide</a>.</p>
module AWS.CloudDirectory where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudDirectory" :: String


-- | <p>Adds a new <a>Facet</a> to an object.</p>
addFacetToObject :: forall eff. AddFacetToObjectRequest -> Aff (err :: AWS.RequestError | eff) AddFacetToObjectResponse
addFacetToObject = AWS.request serviceName "AddFacetToObject" 


-- | <p>Copies the input published schema, at the specified version, into the <a>Directory</a> with the same name and version as that of the published schema.</p>
applySchema :: forall eff. ApplySchemaRequest -> Aff (err :: AWS.RequestError | eff) ApplySchemaResponse
applySchema = AWS.request serviceName "ApplySchema" 


-- | <p>Attaches an existing object to another object. An object can be accessed in two ways:</p> <ol> <li> <p>Using the path</p> </li> <li> <p>Using <code>ObjectIdentifier</code> </p> </li> </ol>
attachObject :: forall eff. AttachObjectRequest -> Aff (err :: AWS.RequestError | eff) AttachObjectResponse
attachObject = AWS.request serviceName "AttachObject" 


-- | <p>Attaches a policy object to a regular object. An object can have a limited number of attached policies.</p>
attachPolicy :: forall eff. AttachPolicyRequest -> Aff (err :: AWS.RequestError | eff) AttachPolicyResponse
attachPolicy = AWS.request serviceName "AttachPolicy" 


-- | <p>Attaches the specified object to the specified index.</p>
attachToIndex :: forall eff. AttachToIndexRequest -> Aff (err :: AWS.RequestError | eff) AttachToIndexResponse
attachToIndex = AWS.request serviceName "AttachToIndex" 


-- | <p>Attaches a typed link to a specified source and target object. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
attachTypedLink :: forall eff. AttachTypedLinkRequest -> Aff (err :: AWS.RequestError | eff) AttachTypedLinkResponse
attachTypedLink = AWS.request serviceName "AttachTypedLink" 


-- | <p>Performs all the read operations in a batch. </p>
batchRead :: forall eff. BatchReadRequest -> Aff (err :: AWS.RequestError | eff) BatchReadResponse
batchRead = AWS.request serviceName "BatchRead" 


-- | <p>Performs all the write operations in a batch. Either all the operations succeed or none.</p>
batchWrite :: forall eff. BatchWriteRequest -> Aff (err :: AWS.RequestError | eff) BatchWriteResponse
batchWrite = AWS.request serviceName "BatchWrite" 


-- | <p>Creates a <a>Directory</a> by copying the published schema into the directory. A directory cannot be created without a schema.</p>
createDirectory :: forall eff. CreateDirectoryRequest -> Aff (err :: AWS.RequestError | eff) CreateDirectoryResponse
createDirectory = AWS.request serviceName "CreateDirectory" 


-- | <p>Creates a new <a>Facet</a> in a schema. Facet creation is allowed only in development or applied schemas.</p>
createFacet :: forall eff. CreateFacetRequest -> Aff (err :: AWS.RequestError | eff) CreateFacetResponse
createFacet = AWS.request serviceName "CreateFacet" 


-- | <p>Creates an index object. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_indexing.html">Indexing</a> for more information.</p>
createIndex :: forall eff. CreateIndexRequest -> Aff (err :: AWS.RequestError | eff) CreateIndexResponse
createIndex = AWS.request serviceName "CreateIndex" 


-- | <p>Creates an object in a <a>Directory</a>. Additionally attaches the object to a parent, if a parent reference and <code>LinkName</code> is specified. An object is simply a collection of <a>Facet</a> attributes. You can also use this API call to create a policy object, if the facet from which you create the object is a policy facet. </p>
createObject :: forall eff. CreateObjectRequest -> Aff (err :: AWS.RequestError | eff) CreateObjectResponse
createObject = AWS.request serviceName "CreateObject" 


-- | <p>Creates a new schema in a development state. A schema can exist in three phases:</p> <ul> <li> <p> <i>Development:</i> This is a mutable phase of the schema. All new schemas are in the development phase. Once the schema is finalized, it can be published.</p> </li> <li> <p> <i>Published:</i> Published schemas are immutable and have a version associated with them.</p> </li> <li> <p> <i>Applied:</i> Applied schemas are mutable in a way that allows you to add new schema facets. You can also add new, nonrequired attributes to existing schema facets. You can apply only published schemas to directories. </p> </li> </ul>
createSchema :: forall eff. CreateSchemaRequest -> Aff (err :: AWS.RequestError | eff) CreateSchemaResponse
createSchema = AWS.request serviceName "CreateSchema" 


-- | <p>Creates a <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
createTypedLinkFacet :: forall eff. CreateTypedLinkFacetRequest -> Aff (err :: AWS.RequestError | eff) CreateTypedLinkFacetResponse
createTypedLinkFacet = AWS.request serviceName "CreateTypedLinkFacet" 


-- | <p>Deletes a directory. Only disabled directories can be deleted. A deleted directory cannot be undone. Exercise extreme caution when deleting directories.</p>
deleteDirectory :: forall eff. DeleteDirectoryRequest -> Aff (err :: AWS.RequestError | eff) DeleteDirectoryResponse
deleteDirectory = AWS.request serviceName "DeleteDirectory" 


-- | <p>Deletes a given <a>Facet</a>. All attributes and <a>Rule</a>s that are associated with the facet will be deleted. Only development schema facets are allowed deletion.</p>
deleteFacet :: forall eff. DeleteFacetRequest -> Aff (err :: AWS.RequestError | eff) DeleteFacetResponse
deleteFacet = AWS.request serviceName "DeleteFacet" 


-- | <p>Deletes an object and its associated attributes. Only objects with no children and no parents can be deleted.</p>
deleteObject :: forall eff. DeleteObjectRequest -> Aff (err :: AWS.RequestError | eff) DeleteObjectResponse
deleteObject = AWS.request serviceName "DeleteObject" 


-- | <p>Deletes a given schema. Schemas in a development and published state can only be deleted. </p>
deleteSchema :: forall eff. DeleteSchemaRequest -> Aff (err :: AWS.RequestError | eff) DeleteSchemaResponse
deleteSchema = AWS.request serviceName "DeleteSchema" 


-- | <p>Deletes a <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
deleteTypedLinkFacet :: forall eff. DeleteTypedLinkFacetRequest -> Aff (err :: AWS.RequestError | eff) DeleteTypedLinkFacetResponse
deleteTypedLinkFacet = AWS.request serviceName "DeleteTypedLinkFacet" 


-- | <p>Detaches the specified object from the specified index.</p>
detachFromIndex :: forall eff. DetachFromIndexRequest -> Aff (err :: AWS.RequestError | eff) DetachFromIndexResponse
detachFromIndex = AWS.request serviceName "DetachFromIndex" 


-- | <p>Detaches a given object from the parent object. The object that is to be detached from the parent is specified by the link name.</p>
detachObject :: forall eff. DetachObjectRequest -> Aff (err :: AWS.RequestError | eff) DetachObjectResponse
detachObject = AWS.request serviceName "DetachObject" 


-- | <p>Detaches a policy from an object.</p>
detachPolicy :: forall eff. DetachPolicyRequest -> Aff (err :: AWS.RequestError | eff) DetachPolicyResponse
detachPolicy = AWS.request serviceName "DetachPolicy" 


-- | <p>Detaches a typed link from a specified source and target object. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
detachTypedLink :: forall eff. DetachTypedLinkRequest -> Aff (err :: AWS.RequestError | eff) Unit
detachTypedLink = AWS.request serviceName "DetachTypedLink" 


-- | <p>Disables the specified directory. Disabled directories cannot be read or written to. Only enabled directories can be disabled. Disabled directories may be reenabled.</p>
disableDirectory :: forall eff. DisableDirectoryRequest -> Aff (err :: AWS.RequestError | eff) DisableDirectoryResponse
disableDirectory = AWS.request serviceName "DisableDirectory" 


-- | <p>Enables the specified directory. Only disabled directories can be enabled. Once enabled, the directory can then be read and written to.</p>
enableDirectory :: forall eff. EnableDirectoryRequest -> Aff (err :: AWS.RequestError | eff) EnableDirectoryResponse
enableDirectory = AWS.request serviceName "EnableDirectory" 


-- | <p>Returns current applied schema version ARN, including the minor version in use.</p>
getAppliedSchemaVersion :: forall eff. GetAppliedSchemaVersionRequest -> Aff (err :: AWS.RequestError | eff) GetAppliedSchemaVersionResponse
getAppliedSchemaVersion = AWS.request serviceName "GetAppliedSchemaVersion" 


-- | <p>Retrieves metadata about a directory.</p>
getDirectory :: forall eff. GetDirectoryRequest -> Aff (err :: AWS.RequestError | eff) GetDirectoryResponse
getDirectory = AWS.request serviceName "GetDirectory" 


-- | <p>Gets details of the <a>Facet</a>, such as facet name, attributes, <a>Rule</a>s, or <code>ObjectType</code>. You can call this on all kinds of schema facets -- published, development, or applied.</p>
getFacet :: forall eff. GetFacetRequest -> Aff (err :: AWS.RequestError | eff) GetFacetResponse
getFacet = AWS.request serviceName "GetFacet" 


-- | <p>Retrieves metadata about an object.</p>
getObjectInformation :: forall eff. GetObjectInformationRequest -> Aff (err :: AWS.RequestError | eff) GetObjectInformationResponse
getObjectInformation = AWS.request serviceName "GetObjectInformation" 


-- | <p>Retrieves a JSON representation of the schema. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_schemas.html#jsonformat">JSON Schema Format</a> for more information.</p>
getSchemaAsJson :: forall eff. GetSchemaAsJsonRequest -> Aff (err :: AWS.RequestError | eff) GetSchemaAsJsonResponse
getSchemaAsJson = AWS.request serviceName "GetSchemaAsJson" 


-- | <p>Returns the identity attribute order for a specific <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
getTypedLinkFacetInformation :: forall eff. GetTypedLinkFacetInformationRequest -> Aff (err :: AWS.RequestError | eff) GetTypedLinkFacetInformationResponse
getTypedLinkFacetInformation = AWS.request serviceName "GetTypedLinkFacetInformation" 


-- | <p>Lists schema major versions applied to a directory. If <code>SchemaArn</code> is provided, lists the minor version.</p>
listAppliedSchemaArns :: forall eff. ListAppliedSchemaArnsRequest -> Aff (err :: AWS.RequestError | eff) ListAppliedSchemaArnsResponse
listAppliedSchemaArns = AWS.request serviceName "ListAppliedSchemaArns" 


-- | <p>Lists indices attached to the specified object.</p>
listAttachedIndices :: forall eff. ListAttachedIndicesRequest -> Aff (err :: AWS.RequestError | eff) ListAttachedIndicesResponse
listAttachedIndices = AWS.request serviceName "ListAttachedIndices" 


-- | <p>Retrieves each Amazon Resource Name (ARN) of schemas in the development state.</p>
listDevelopmentSchemaArns :: forall eff. ListDevelopmentSchemaArnsRequest -> Aff (err :: AWS.RequestError | eff) ListDevelopmentSchemaArnsResponse
listDevelopmentSchemaArns = AWS.request serviceName "ListDevelopmentSchemaArns" 


-- | <p>Lists directories created within an account.</p>
listDirectories :: forall eff. ListDirectoriesRequest -> Aff (err :: AWS.RequestError | eff) ListDirectoriesResponse
listDirectories = AWS.request serviceName "ListDirectories" 


-- | <p>Retrieves attributes attached to the facet.</p>
listFacetAttributes :: forall eff. ListFacetAttributesRequest -> Aff (err :: AWS.RequestError | eff) ListFacetAttributesResponse
listFacetAttributes = AWS.request serviceName "ListFacetAttributes" 


-- | <p>Retrieves the names of facets that exist in a schema.</p>
listFacetNames :: forall eff. ListFacetNamesRequest -> Aff (err :: AWS.RequestError | eff) ListFacetNamesResponse
listFacetNames = AWS.request serviceName "ListFacetNames" 


-- | <p>Returns a paginated list of all the incoming <a>TypedLinkSpecifier</a> information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
listIncomingTypedLinks :: forall eff. ListIncomingTypedLinksRequest -> Aff (err :: AWS.RequestError | eff) ListIncomingTypedLinksResponse
listIncomingTypedLinks = AWS.request serviceName "ListIncomingTypedLinks" 


-- | <p>Lists objects and indexed values attached to the index.</p>
listIndex :: forall eff. ListIndexRequest -> Aff (err :: AWS.RequestError | eff) ListIndexResponse
listIndex = AWS.request serviceName "ListIndex" 


-- | <p>Lists all attributes that are associated with an object. </p>
listObjectAttributes :: forall eff. ListObjectAttributesRequest -> Aff (err :: AWS.RequestError | eff) ListObjectAttributesResponse
listObjectAttributes = AWS.request serviceName "ListObjectAttributes" 


-- | <p>Returns a paginated list of child objects that are associated with a given object.</p>
listObjectChildren :: forall eff. ListObjectChildrenRequest -> Aff (err :: AWS.RequestError | eff) ListObjectChildrenResponse
listObjectChildren = AWS.request serviceName "ListObjectChildren" 


-- | <p>Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#dirstructure">Directory Structure</a>.</p> <p>Use this API to evaluate all parents for an object. The call returns all objects from the root of the directory up to the requested object. The API returns the number of paths based on user-defined <code>MaxResults</code>, in case there are multiple paths to the parent. The order of the paths and nodes returned is consistent among multiple API calls unless the objects are deleted or moved. Paths not leading to the directory root are ignored from the target object.</p>
listObjectParentPaths :: forall eff. ListObjectParentPathsRequest -> Aff (err :: AWS.RequestError | eff) ListObjectParentPathsResponse
listObjectParentPaths = AWS.request serviceName "ListObjectParentPaths" 


-- | <p>Lists parent objects that are associated with a given object in pagination fashion.</p>
listObjectParents :: forall eff. ListObjectParentsRequest -> Aff (err :: AWS.RequestError | eff) ListObjectParentsResponse
listObjectParents = AWS.request serviceName "ListObjectParents" 


-- | <p>Returns policies attached to an object in pagination fashion.</p>
listObjectPolicies :: forall eff. ListObjectPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListObjectPoliciesResponse
listObjectPolicies = AWS.request serviceName "ListObjectPolicies" 


-- | <p>Returns a paginated list of all the outgoing <a>TypedLinkSpecifier</a> information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
listOutgoingTypedLinks :: forall eff. ListOutgoingTypedLinksRequest -> Aff (err :: AWS.RequestError | eff) ListOutgoingTypedLinksResponse
listOutgoingTypedLinks = AWS.request serviceName "ListOutgoingTypedLinks" 


-- | <p>Returns all of the <code>ObjectIdentifiers</code> to which a given policy is attached.</p>
listPolicyAttachments :: forall eff. ListPolicyAttachmentsRequest -> Aff (err :: AWS.RequestError | eff) ListPolicyAttachmentsResponse
listPolicyAttachments = AWS.request serviceName "ListPolicyAttachments" 


-- | <p>Lists schema major versions for a published schema. If <code>SchemaArn</code> is provided, lists the minor version.</p>
listPublishedSchemaArns :: forall eff. ListPublishedSchemaArnsRequest -> Aff (err :: AWS.RequestError | eff) ListPublishedSchemaArnsResponse
listPublishedSchemaArns = AWS.request serviceName "ListPublishedSchemaArns" 


-- | <p>Returns tags for a resource. Tagging is currently supported only for directories with a limit of 50 tags per directory. All 50 tags are returned for a given directory with this API call.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForResourceResponse
listTagsForResource = AWS.request serviceName "ListTagsForResource" 


-- | <p>Returns a paginated list of all attribute definitions for a particular <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
listTypedLinkFacetAttributes :: forall eff. ListTypedLinkFacetAttributesRequest -> Aff (err :: AWS.RequestError | eff) ListTypedLinkFacetAttributesResponse
listTypedLinkFacetAttributes = AWS.request serviceName "ListTypedLinkFacetAttributes" 


-- | <p>Returns a paginated list of <code>TypedLink</code> facet names for a particular schema. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
listTypedLinkFacetNames :: forall eff. ListTypedLinkFacetNamesRequest -> Aff (err :: AWS.RequestError | eff) ListTypedLinkFacetNamesResponse
listTypedLinkFacetNames = AWS.request serviceName "ListTypedLinkFacetNames" 


-- | <p>Lists all policies from the root of the <a>Directory</a> to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the <code>ObjectIdentifier</code> for such objects. If policies are present, it returns <code>ObjectIdentifier</code>, <code>policyId</code>, and <code>policyType</code>. Paths that don't lead to the root from the target object are ignored. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies">Policies</a>.</p>
lookupPolicy :: forall eff. LookupPolicyRequest -> Aff (err :: AWS.RequestError | eff) LookupPolicyResponse
lookupPolicy = AWS.request serviceName "LookupPolicy" 


-- | <p>Publishes a development schema with a major version and a recommended minor version.</p>
publishSchema :: forall eff. PublishSchemaRequest -> Aff (err :: AWS.RequestError | eff) PublishSchemaResponse
publishSchema = AWS.request serviceName "PublishSchema" 


-- | <p>Allows a schema to be updated using JSON upload. Only available for development schemas. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_schemas.html#jsonformat">JSON Schema Format</a> for more information.</p>
putSchemaFromJson :: forall eff. PutSchemaFromJsonRequest -> Aff (err :: AWS.RequestError | eff) PutSchemaFromJsonResponse
putSchemaFromJson = AWS.request serviceName "PutSchemaFromJson" 


-- | <p>Removes the specified facet from the specified object.</p>
removeFacetFromObject :: forall eff. RemoveFacetFromObjectRequest -> Aff (err :: AWS.RequestError | eff) RemoveFacetFromObjectResponse
removeFacetFromObject = AWS.request serviceName "RemoveFacetFromObject" 


-- | <p>An API operation for adding tags to a resource.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (err :: AWS.RequestError | eff) TagResourceResponse
tagResource = AWS.request serviceName "TagResource" 


-- | <p>An API operation for removing tags from a resource.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: AWS.RequestError | eff) UntagResourceResponse
untagResource = AWS.request serviceName "UntagResource" 


-- | <p>Does the following:</p> <ol> <li> <p>Adds new <code>Attributes</code>, <code>Rules</code>, or <code>ObjectTypes</code>.</p> </li> <li> <p>Updates existing <code>Attributes</code>, <code>Rules</code>, or <code>ObjectTypes</code>.</p> </li> <li> <p>Deletes existing <code>Attributes</code>, <code>Rules</code>, or <code>ObjectTypes</code>.</p> </li> </ol>
updateFacet :: forall eff. UpdateFacetRequest -> Aff (err :: AWS.RequestError | eff) UpdateFacetResponse
updateFacet = AWS.request serviceName "UpdateFacet" 


-- | <p>Updates a given object's attributes.</p>
updateObjectAttributes :: forall eff. UpdateObjectAttributesRequest -> Aff (err :: AWS.RequestError | eff) UpdateObjectAttributesResponse
updateObjectAttributes = AWS.request serviceName "UpdateObjectAttributes" 


-- | <p>Updates the schema name with a new name. Only development schema names can be updated.</p>
updateSchema :: forall eff. UpdateSchemaRequest -> Aff (err :: AWS.RequestError | eff) UpdateSchemaResponse
updateSchema = AWS.request serviceName "UpdateSchema" 


-- | <p>Updates a <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
updateTypedLinkFacet :: forall eff. UpdateTypedLinkFacetRequest -> Aff (err :: AWS.RequestError | eff) UpdateTypedLinkFacetResponse
updateTypedLinkFacet = AWS.request serviceName "UpdateTypedLinkFacet" 


-- | <p>Upgrades a single directory in-place using the <code>PublishedSchemaArn</code> with schema updates found in <code>MinorVersion</code>. Backwards-compatible minor version upgrades are instantaneously available for readers on all objects in the directory. Note: This is a synchronous API call and upgrades only one schema on a given directory per call. To upgrade multiple directories from one schema, you would need to call this API on each directory.</p>
upgradeAppliedSchema :: forall eff. UpgradeAppliedSchemaRequest -> Aff (err :: AWS.RequestError | eff) UpgradeAppliedSchemaResponse
upgradeAppliedSchema = AWS.request serviceName "UpgradeAppliedSchema" 


-- | <p>Upgrades a published schema under a new minor version revision using the current contents of <code>DevelopmentSchemaArn</code>.</p>
upgradePublishedSchema :: forall eff. UpgradePublishedSchemaRequest -> Aff (err :: AWS.RequestError | eff) UpgradePublishedSchemaResponse
upgradePublishedSchema = AWS.request serviceName "UpgradePublishedSchema" 


-- | <p>Access denied. Check your permissions.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _


newtype AddFacetToObjectRequest = AddFacetToObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "SchemaFacet" :: (SchemaFacet)
  , "ObjectAttributeList" :: NullOrUndefined (AttributeKeyAndValueList)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeAddFacetToObjectRequest :: Newtype AddFacetToObjectRequest _


newtype AddFacetToObjectResponse = AddFacetToObjectResponse 
  { 
  }
derive instance newtypeAddFacetToObjectResponse :: Newtype AddFacetToObjectResponse _


newtype ApplySchemaRequest = ApplySchemaRequest 
  { "PublishedSchemaArn" :: (Arn)
  , "DirectoryArn" :: (Arn)
  }
derive instance newtypeApplySchemaRequest :: Newtype ApplySchemaRequest _


newtype ApplySchemaResponse = ApplySchemaResponse 
  { "AppliedSchemaArn" :: NullOrUndefined (Arn)
  , "DirectoryArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeApplySchemaResponse :: Newtype ApplySchemaResponse _


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _


newtype Arns = Arns (Array Arn)
derive instance newtypeArns :: Newtype Arns _


newtype AttachObjectRequest = AttachObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "ParentReference" :: (ObjectReference)
  , "ChildReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  }
derive instance newtypeAttachObjectRequest :: Newtype AttachObjectRequest _


newtype AttachObjectResponse = AttachObjectResponse 
  { "AttachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeAttachObjectResponse :: Newtype AttachObjectResponse _


newtype AttachPolicyRequest = AttachPolicyRequest 
  { "DirectoryArn" :: NullOrUndefined (Arn)
  , "PolicyReference" :: (ObjectReference)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeAttachPolicyRequest :: Newtype AttachPolicyRequest _


newtype AttachPolicyResponse = AttachPolicyResponse 
  { 
  }
derive instance newtypeAttachPolicyResponse :: Newtype AttachPolicyResponse _


newtype AttachToIndexRequest = AttachToIndexRequest 
  { "DirectoryArn" :: (Arn)
  , "IndexReference" :: (ObjectReference)
  , "TargetReference" :: (ObjectReference)
  }
derive instance newtypeAttachToIndexRequest :: Newtype AttachToIndexRequest _


newtype AttachToIndexResponse = AttachToIndexResponse 
  { "AttachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeAttachToIndexResponse :: Newtype AttachToIndexResponse _


newtype AttachTypedLinkRequest = AttachTypedLinkRequest 
  { "DirectoryArn" :: (Arn)
  , "SourceObjectReference" :: (ObjectReference)
  , "TargetObjectReference" :: (ObjectReference)
  , "TypedLinkFacet" :: (TypedLinkSchemaAndFacetName)
  , "Attributes" :: (AttributeNameAndValueList)
  }
derive instance newtypeAttachTypedLinkRequest :: Newtype AttachTypedLinkRequest _


newtype AttachTypedLinkResponse = AttachTypedLinkResponse 
  { "TypedLinkSpecifier" :: NullOrUndefined (TypedLinkSpecifier)
  }
derive instance newtypeAttachTypedLinkResponse :: Newtype AttachTypedLinkResponse _


-- | <p>A unique identifier for an attribute.</p>
newtype AttributeKey = AttributeKey 
  { "SchemaArn" :: (Arn)
  , "FacetName" :: (FacetName)
  , "Name" :: (AttributeName)
  }
derive instance newtypeAttributeKey :: Newtype AttributeKey _


-- | <p>The combination of an attribute key and an attribute value.</p>
newtype AttributeKeyAndValue = AttributeKeyAndValue 
  { "Key" :: (AttributeKey)
  , "Value" :: (TypedAttributeValue)
  }
derive instance newtypeAttributeKeyAndValue :: Newtype AttributeKeyAndValue _


newtype AttributeKeyAndValueList = AttributeKeyAndValueList (Array AttributeKeyAndValue)
derive instance newtypeAttributeKeyAndValueList :: Newtype AttributeKeyAndValueList _


newtype AttributeKeyList = AttributeKeyList (Array AttributeKey)
derive instance newtypeAttributeKeyList :: Newtype AttributeKeyList _


newtype AttributeName = AttributeName String
derive instance newtypeAttributeName :: Newtype AttributeName _


-- | <p>Identifies the attribute name and value for a typed link.</p>
newtype AttributeNameAndValue = AttributeNameAndValue 
  { "AttributeName" :: (AttributeName)
  , "Value" :: (TypedAttributeValue)
  }
derive instance newtypeAttributeNameAndValue :: Newtype AttributeNameAndValue _


newtype AttributeNameAndValueList = AttributeNameAndValueList (Array AttributeNameAndValue)
derive instance newtypeAttributeNameAndValueList :: Newtype AttributeNameAndValueList _


newtype AttributeNameList = AttributeNameList (Array AttributeName)
derive instance newtypeAttributeNameList :: Newtype AttributeNameList _


-- | <p>Represents the output of a batch add facet to object operation.</p>
newtype BatchAddFacetToObject = BatchAddFacetToObject 
  { "SchemaFacet" :: (SchemaFacet)
  , "ObjectAttributeList" :: (AttributeKeyAndValueList)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeBatchAddFacetToObject :: Newtype BatchAddFacetToObject _


-- | <p>The result of a batch add facet to object operation.</p>
newtype BatchAddFacetToObjectResponse = BatchAddFacetToObjectResponse 
  { 
  }
derive instance newtypeBatchAddFacetToObjectResponse :: Newtype BatchAddFacetToObjectResponse _


-- | <p>Represents the output of an <a>AttachObject</a> operation.</p>
newtype BatchAttachObject = BatchAttachObject 
  { "ParentReference" :: (ObjectReference)
  , "ChildReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  }
derive instance newtypeBatchAttachObject :: Newtype BatchAttachObject _


-- | <p>Represents the output batch <a>AttachObject</a> response operation.</p>
newtype BatchAttachObjectResponse = BatchAttachObjectResponse 
  { "AttachedObjectIdentifier'" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchAttachObjectResponse :: Newtype BatchAttachObjectResponse _


-- | <p>Attaches a policy object to a regular object inside a <a>BatchRead</a> operation. For more information, see <a>AttachPolicy</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchAttachPolicy = BatchAttachPolicy 
  { "PolicyReference" :: (ObjectReference)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeBatchAttachPolicy :: Newtype BatchAttachPolicy _


-- | <p>Represents the output of an <a>AttachPolicy</a> response operation.</p>
newtype BatchAttachPolicyResponse = BatchAttachPolicyResponse 
  { 
  }
derive instance newtypeBatchAttachPolicyResponse :: Newtype BatchAttachPolicyResponse _


-- | <p>Attaches the specified object to the specified index inside a <a>BatchRead</a> operation. For more information, see <a>AttachToIndex</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchAttachToIndex = BatchAttachToIndex 
  { "IndexReference" :: (ObjectReference)
  , "TargetReference" :: (ObjectReference)
  }
derive instance newtypeBatchAttachToIndex :: Newtype BatchAttachToIndex _


-- | <p>Represents the output of a <a>AttachToIndex</a> response operation.</p>
newtype BatchAttachToIndexResponse = BatchAttachToIndexResponse 
  { "AttachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchAttachToIndexResponse :: Newtype BatchAttachToIndexResponse _


-- | <p>Attaches a typed link to a specified source and target object inside a <a>BatchRead</a> operation. For more information, see <a>AttachTypedLink</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchAttachTypedLink = BatchAttachTypedLink 
  { "SourceObjectReference" :: (ObjectReference)
  , "TargetObjectReference" :: (ObjectReference)
  , "TypedLinkFacet" :: (TypedLinkSchemaAndFacetName)
  , "Attributes" :: (AttributeNameAndValueList)
  }
derive instance newtypeBatchAttachTypedLink :: Newtype BatchAttachTypedLink _


-- | <p>Represents the output of a <a>AttachTypedLink</a> response operation.</p>
newtype BatchAttachTypedLinkResponse = BatchAttachTypedLinkResponse 
  { "TypedLinkSpecifier" :: NullOrUndefined (TypedLinkSpecifier)
  }
derive instance newtypeBatchAttachTypedLinkResponse :: Newtype BatchAttachTypedLinkResponse _


-- | <p>Creates an index object inside of a <a>BatchRead</a> operation. For more information, see <a>CreateIndex</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchCreateIndex = BatchCreateIndex 
  { "OrderedIndexedAttributeList" :: (AttributeKeyList)
  , "IsUnique" :: (Bool)
  , "ParentReference" :: NullOrUndefined (ObjectReference)
  , "LinkName" :: NullOrUndefined (LinkName)
  , "BatchReferenceName" :: NullOrUndefined (BatchReferenceName)
  }
derive instance newtypeBatchCreateIndex :: Newtype BatchCreateIndex _


-- | <p>Represents the output of a <a>CreateIndex</a> response operation.</p>
newtype BatchCreateIndexResponse = BatchCreateIndexResponse 
  { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchCreateIndexResponse :: Newtype BatchCreateIndexResponse _


-- | <p>Represents the output of a <a>CreateObject</a> operation.</p>
newtype BatchCreateObject = BatchCreateObject 
  { "SchemaFacet" :: (SchemaFacetList)
  , "ObjectAttributeList" :: (AttributeKeyAndValueList)
  , "ParentReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  , "BatchReferenceName" :: (BatchReferenceName)
  }
derive instance newtypeBatchCreateObject :: Newtype BatchCreateObject _


-- | <p>Represents the output of a <a>CreateObject</a> response operation.</p>
newtype BatchCreateObjectResponse = BatchCreateObjectResponse 
  { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchCreateObjectResponse :: Newtype BatchCreateObjectResponse _


-- | <p>Represents the output of a <a>DeleteObject</a> operation.</p>
newtype BatchDeleteObject = BatchDeleteObject 
  { "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeBatchDeleteObject :: Newtype BatchDeleteObject _


-- | <p>Represents the output of a <a>DeleteObject</a> response operation.</p>
newtype BatchDeleteObjectResponse = BatchDeleteObjectResponse 
  { 
  }
derive instance newtypeBatchDeleteObjectResponse :: Newtype BatchDeleteObjectResponse _


-- | <p>Detaches the specified object from the specified index inside a <a>BatchRead</a> operation. For more information, see <a>DetachFromIndex</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchDetachFromIndex = BatchDetachFromIndex 
  { "IndexReference" :: (ObjectReference)
  , "TargetReference" :: (ObjectReference)
  }
derive instance newtypeBatchDetachFromIndex :: Newtype BatchDetachFromIndex _


-- | <p>Represents the output of a <a>DetachFromIndex</a> response operation.</p>
newtype BatchDetachFromIndexResponse = BatchDetachFromIndexResponse 
  { "DetachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchDetachFromIndexResponse :: Newtype BatchDetachFromIndexResponse _


-- | <p>Represents the output of a <a>DetachObject</a> operation.</p>
newtype BatchDetachObject = BatchDetachObject 
  { "ParentReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  , "BatchReferenceName" :: (BatchReferenceName)
  }
derive instance newtypeBatchDetachObject :: Newtype BatchDetachObject _


-- | <p>Represents the output of a <a>DetachObject</a> response operation.</p>
newtype BatchDetachObjectResponse = BatchDetachObjectResponse 
  { "DetachedObjectIdentifier'" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchDetachObjectResponse :: Newtype BatchDetachObjectResponse _


-- | <p>Detaches the specified policy from the specified directory inside a <a>BatchWrite</a> operation. For more information, see <a>DetachPolicy</a> and <a>BatchWriteRequest$Operations</a>.</p>
newtype BatchDetachPolicy = BatchDetachPolicy 
  { "PolicyReference" :: (ObjectReference)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeBatchDetachPolicy :: Newtype BatchDetachPolicy _


-- | <p>Represents the output of a <a>DetachPolicy</a> response operation.</p>
newtype BatchDetachPolicyResponse = BatchDetachPolicyResponse 
  { 
  }
derive instance newtypeBatchDetachPolicyResponse :: Newtype BatchDetachPolicyResponse _


-- | <p>Detaches a typed link from a specified source and target object inside a <a>BatchRead</a> operation. For more information, see <a>DetachTypedLink</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchDetachTypedLink = BatchDetachTypedLink 
  { "TypedLinkSpecifier" :: (TypedLinkSpecifier)
  }
derive instance newtypeBatchDetachTypedLink :: Newtype BatchDetachTypedLink _


-- | <p>Represents the output of a <a>DetachTypedLink</a> response operation.</p>
newtype BatchDetachTypedLinkResponse = BatchDetachTypedLinkResponse 
  { 
  }
derive instance newtypeBatchDetachTypedLinkResponse :: Newtype BatchDetachTypedLinkResponse _


-- | <p>Retrieves metadata about an object inside a <a>BatchRead</a> operation. For more information, see <a>GetObjectInformation</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchGetObjectInformation = BatchGetObjectInformation 
  { "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeBatchGetObjectInformation :: Newtype BatchGetObjectInformation _


-- | <p>Represents the output of a <a>GetObjectInformation</a> response operation.</p>
newtype BatchGetObjectInformationResponse = BatchGetObjectInformationResponse 
  { "SchemaFacets" :: NullOrUndefined (SchemaFacetList)
  , "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchGetObjectInformationResponse :: Newtype BatchGetObjectInformationResponse _


-- | <p>Lists indices attached to an object inside a <a>BatchRead</a> operation. For more information, see <a>ListAttachedIndices</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListAttachedIndices = BatchListAttachedIndices 
  { "TargetReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListAttachedIndices :: Newtype BatchListAttachedIndices _


-- | <p>Represents the output of a <a>ListAttachedIndices</a> response operation.</p>
newtype BatchListAttachedIndicesResponse = BatchListAttachedIndicesResponse 
  { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListAttachedIndicesResponse :: Newtype BatchListAttachedIndicesResponse _


-- | <p>Returns a paginated list of all the incoming <a>TypedLinkSpecifier</a> information for an object inside a <a>BatchRead</a> operation. For more information, see <a>ListIncomingTypedLinks</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListIncomingTypedLinks = BatchListIncomingTypedLinks 
  { "ObjectReference" :: (ObjectReference)
  , "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList)
  , "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListIncomingTypedLinks :: Newtype BatchListIncomingTypedLinks _


-- | <p>Represents the output of a <a>ListIncomingTypedLinks</a> response operation.</p>
newtype BatchListIncomingTypedLinksResponse = BatchListIncomingTypedLinksResponse 
  { "LinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListIncomingTypedLinksResponse :: Newtype BatchListIncomingTypedLinksResponse _


-- | <p>Lists objects attached to the specified index inside a <a>BatchRead</a> operation. For more information, see <a>ListIndex</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListIndex = BatchListIndex 
  { "RangesOnIndexedValues" :: NullOrUndefined (ObjectAttributeRangeList)
  , "IndexReference" :: (ObjectReference)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListIndex :: Newtype BatchListIndex _


-- | <p>Represents the output of a <a>ListIndex</a> response operation.</p>
newtype BatchListIndexResponse = BatchListIndexResponse 
  { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListIndexResponse :: Newtype BatchListIndexResponse _


-- | <p>Represents the output of a <a>ListObjectAttributes</a> operation.</p>
newtype BatchListObjectAttributes = BatchListObjectAttributes 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "FacetFilter" :: NullOrUndefined (SchemaFacet)
  }
derive instance newtypeBatchListObjectAttributes :: Newtype BatchListObjectAttributes _


-- | <p>Represents the output of a <a>ListObjectAttributes</a> response operation.</p>
newtype BatchListObjectAttributesResponse = BatchListObjectAttributesResponse 
  { "Attributes" :: NullOrUndefined (AttributeKeyAndValueList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListObjectAttributesResponse :: Newtype BatchListObjectAttributesResponse _


-- | <p>Represents the output of a <a>ListObjectChildren</a> operation.</p>
newtype BatchListObjectChildren = BatchListObjectChildren 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListObjectChildren :: Newtype BatchListObjectChildren _


-- | <p>Represents the output of a <a>ListObjectChildren</a> response operation.</p>
newtype BatchListObjectChildrenResponse = BatchListObjectChildrenResponse 
  { "Children" :: NullOrUndefined (LinkNameToObjectIdentifierMap)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListObjectChildrenResponse :: Newtype BatchListObjectChildrenResponse _


-- | <p>Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects inside a <a>BatchRead</a> operation. For more information, see <a>ListObjectParentPaths</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListObjectParentPaths = BatchListObjectParentPaths 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListObjectParentPaths :: Newtype BatchListObjectParentPaths _


-- | <p>Represents the output of a <a>ListObjectParentPaths</a> response operation.</p>
newtype BatchListObjectParentPathsResponse = BatchListObjectParentPathsResponse 
  { "PathToObjectIdentifiersList" :: NullOrUndefined (PathToObjectIdentifiersList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListObjectParentPathsResponse :: Newtype BatchListObjectParentPathsResponse _


-- | <p>Returns policies attached to an object in pagination fashion inside a <a>BatchRead</a> operation. For more information, see <a>ListObjectPolicies</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListObjectPolicies = BatchListObjectPolicies 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListObjectPolicies :: Newtype BatchListObjectPolicies _


-- | <p>Represents the output of a <a>ListObjectPolicies</a> response operation.</p>
newtype BatchListObjectPoliciesResponse = BatchListObjectPoliciesResponse 
  { "AttachedPolicyIds" :: NullOrUndefined (ObjectIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListObjectPoliciesResponse :: Newtype BatchListObjectPoliciesResponse _


-- | <p>Returns a paginated list of all the outgoing <a>TypedLinkSpecifier</a> information for an object inside a <a>BatchRead</a> operation. For more information, see <a>ListOutgoingTypedLinks</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListOutgoingTypedLinks = BatchListOutgoingTypedLinks 
  { "ObjectReference" :: (ObjectReference)
  , "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList)
  , "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListOutgoingTypedLinks :: Newtype BatchListOutgoingTypedLinks _


-- | <p>Represents the output of a <a>ListOutgoingTypedLinks</a> response operation.</p>
newtype BatchListOutgoingTypedLinksResponse = BatchListOutgoingTypedLinksResponse 
  { "TypedLinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListOutgoingTypedLinksResponse :: Newtype BatchListOutgoingTypedLinksResponse _


-- | <p>Returns all of the <code>ObjectIdentifiers</code> to which a given policy is attached inside a <a>BatchRead</a> operation. For more information, see <a>ListPolicyAttachments</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListPolicyAttachments = BatchListPolicyAttachments 
  { "PolicyReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListPolicyAttachments :: Newtype BatchListPolicyAttachments _


-- | <p>Represents the output of a <a>ListPolicyAttachments</a> response operation.</p>
newtype BatchListPolicyAttachmentsResponse = BatchListPolicyAttachmentsResponse 
  { "ObjectIdentifiers" :: NullOrUndefined (ObjectIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListPolicyAttachmentsResponse :: Newtype BatchListPolicyAttachmentsResponse _


-- | <p>Lists all policies from the root of the Directory to the object specified inside a <a>BatchRead</a> operation. For more information, see <a>LookupPolicy</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchLookupPolicy = BatchLookupPolicy 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchLookupPolicy :: Newtype BatchLookupPolicy _


-- | <p>Represents the output of a <a>LookupPolicy</a> response operation.</p>
newtype BatchLookupPolicyResponse = BatchLookupPolicyResponse 
  { "PolicyToPathList" :: NullOrUndefined (PolicyToPathList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeBatchLookupPolicyResponse :: Newtype BatchLookupPolicyResponse _


newtype BatchOperationIndex = BatchOperationIndex Int
derive instance newtypeBatchOperationIndex :: Newtype BatchOperationIndex _


-- | <p>The batch read exception structure, which contains the exception type and message.</p>
newtype BatchReadException = BatchReadException 
  { "Type" :: NullOrUndefined (BatchReadExceptionType)
  , "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeBatchReadException :: Newtype BatchReadException _


newtype BatchReadExceptionType = BatchReadExceptionType String
derive instance newtypeBatchReadExceptionType :: Newtype BatchReadExceptionType _


-- | <p>Represents the output of a <code>BatchRead</code> operation.</p>
newtype BatchReadOperation = BatchReadOperation 
  { "ListObjectAttributes" :: NullOrUndefined (BatchListObjectAttributes)
  , "ListObjectChildren" :: NullOrUndefined (BatchListObjectChildren)
  , "ListAttachedIndices" :: NullOrUndefined (BatchListAttachedIndices)
  , "ListObjectParentPaths" :: NullOrUndefined (BatchListObjectParentPaths)
  , "GetObjectInformation" :: NullOrUndefined (BatchGetObjectInformation)
  , "ListObjectPolicies" :: NullOrUndefined (BatchListObjectPolicies)
  , "ListPolicyAttachments" :: NullOrUndefined (BatchListPolicyAttachments)
  , "LookupPolicy" :: NullOrUndefined (BatchLookupPolicy)
  , "ListIndex" :: NullOrUndefined (BatchListIndex)
  , "ListOutgoingTypedLinks" :: NullOrUndefined (BatchListOutgoingTypedLinks)
  , "ListIncomingTypedLinks" :: NullOrUndefined (BatchListIncomingTypedLinks)
  }
derive instance newtypeBatchReadOperation :: Newtype BatchReadOperation _


newtype BatchReadOperationList = BatchReadOperationList (Array BatchReadOperation)
derive instance newtypeBatchReadOperationList :: Newtype BatchReadOperationList _


-- | <p>Represents the output of a <code>BatchRead</code> response operation.</p>
newtype BatchReadOperationResponse = BatchReadOperationResponse 
  { "SuccessfulResponse" :: NullOrUndefined (BatchReadSuccessfulResponse)
  , "ExceptionResponse" :: NullOrUndefined (BatchReadException)
  }
derive instance newtypeBatchReadOperationResponse :: Newtype BatchReadOperationResponse _


newtype BatchReadOperationResponseList = BatchReadOperationResponseList (Array BatchReadOperationResponse)
derive instance newtypeBatchReadOperationResponseList :: Newtype BatchReadOperationResponseList _


newtype BatchReadRequest = BatchReadRequest 
  { "DirectoryArn" :: (Arn)
  , "Operations" :: (BatchReadOperationList)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeBatchReadRequest :: Newtype BatchReadRequest _


newtype BatchReadResponse = BatchReadResponse 
  { "Responses" :: NullOrUndefined (BatchReadOperationResponseList)
  }
derive instance newtypeBatchReadResponse :: Newtype BatchReadResponse _


-- | <p>Represents the output of a <code>BatchRead</code> success response operation.</p>
newtype BatchReadSuccessfulResponse = BatchReadSuccessfulResponse 
  { "ListObjectAttributes" :: NullOrUndefined (BatchListObjectAttributesResponse)
  , "ListObjectChildren" :: NullOrUndefined (BatchListObjectChildrenResponse)
  , "GetObjectInformation" :: NullOrUndefined (BatchGetObjectInformationResponse)
  , "ListAttachedIndices" :: NullOrUndefined (BatchListAttachedIndicesResponse)
  , "ListObjectParentPaths" :: NullOrUndefined (BatchListObjectParentPathsResponse)
  , "ListObjectPolicies" :: NullOrUndefined (BatchListObjectPoliciesResponse)
  , "ListPolicyAttachments" :: NullOrUndefined (BatchListPolicyAttachmentsResponse)
  , "LookupPolicy" :: NullOrUndefined (BatchLookupPolicyResponse)
  , "ListIndex" :: NullOrUndefined (BatchListIndexResponse)
  , "ListOutgoingTypedLinks" :: NullOrUndefined (BatchListOutgoingTypedLinksResponse)
  , "ListIncomingTypedLinks" :: NullOrUndefined (BatchListIncomingTypedLinksResponse)
  }
derive instance newtypeBatchReadSuccessfulResponse :: Newtype BatchReadSuccessfulResponse _


newtype BatchReferenceName = BatchReferenceName String
derive instance newtypeBatchReferenceName :: Newtype BatchReferenceName _


-- | <p>A batch operation to remove a facet from an object.</p>
newtype BatchRemoveFacetFromObject = BatchRemoveFacetFromObject 
  { "SchemaFacet" :: (SchemaFacet)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeBatchRemoveFacetFromObject :: Newtype BatchRemoveFacetFromObject _


-- | <p>An empty result that represents success.</p>
newtype BatchRemoveFacetFromObjectResponse = BatchRemoveFacetFromObjectResponse 
  { 
  }
derive instance newtypeBatchRemoveFacetFromObjectResponse :: Newtype BatchRemoveFacetFromObjectResponse _


-- | <p>Represents the output of a <code>BatchUpdate</code> operation. </p>
newtype BatchUpdateObjectAttributes = BatchUpdateObjectAttributes 
  { "ObjectReference" :: (ObjectReference)
  , "AttributeUpdates" :: (ObjectAttributeUpdateList)
  }
derive instance newtypeBatchUpdateObjectAttributes :: Newtype BatchUpdateObjectAttributes _


-- | <p>Represents the output of a <code>BatchUpdate</code> response operation.</p>
newtype BatchUpdateObjectAttributesResponse = BatchUpdateObjectAttributesResponse 
  { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchUpdateObjectAttributesResponse :: Newtype BatchUpdateObjectAttributesResponse _


-- | <p>A <code>BatchWrite</code> exception has occurred.</p>
newtype BatchWriteException = BatchWriteException 
  { "Index" :: NullOrUndefined (BatchOperationIndex)
  , "Type" :: NullOrUndefined (BatchWriteExceptionType)
  , "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeBatchWriteException :: Newtype BatchWriteException _


newtype BatchWriteExceptionType = BatchWriteExceptionType String
derive instance newtypeBatchWriteExceptionType :: Newtype BatchWriteExceptionType _


-- | <p>Represents the output of a <code>BatchWrite</code> operation. </p>
newtype BatchWriteOperation = BatchWriteOperation 
  { "CreateObject" :: NullOrUndefined (BatchCreateObject)
  , "AttachObject" :: NullOrUndefined (BatchAttachObject)
  , "DetachObject" :: NullOrUndefined (BatchDetachObject)
  , "UpdateObjectAttributes" :: NullOrUndefined (BatchUpdateObjectAttributes)
  , "DeleteObject" :: NullOrUndefined (BatchDeleteObject)
  , "AddFacetToObject" :: NullOrUndefined (BatchAddFacetToObject)
  , "RemoveFacetFromObject" :: NullOrUndefined (BatchRemoveFacetFromObject)
  , "AttachPolicy" :: NullOrUndefined (BatchAttachPolicy)
  , "DetachPolicy" :: NullOrUndefined (BatchDetachPolicy)
  , "CreateIndex" :: NullOrUndefined (BatchCreateIndex)
  , "AttachToIndex" :: NullOrUndefined (BatchAttachToIndex)
  , "DetachFromIndex" :: NullOrUndefined (BatchDetachFromIndex)
  , "AttachTypedLink" :: NullOrUndefined (BatchAttachTypedLink)
  , "DetachTypedLink" :: NullOrUndefined (BatchDetachTypedLink)
  }
derive instance newtypeBatchWriteOperation :: Newtype BatchWriteOperation _


newtype BatchWriteOperationList = BatchWriteOperationList (Array BatchWriteOperation)
derive instance newtypeBatchWriteOperationList :: Newtype BatchWriteOperationList _


-- | <p>Represents the output of a <code>BatchWrite</code> response operation.</p>
newtype BatchWriteOperationResponse = BatchWriteOperationResponse 
  { "CreateObject" :: NullOrUndefined (BatchCreateObjectResponse)
  , "AttachObject" :: NullOrUndefined (BatchAttachObjectResponse)
  , "DetachObject" :: NullOrUndefined (BatchDetachObjectResponse)
  , "UpdateObjectAttributes" :: NullOrUndefined (BatchUpdateObjectAttributesResponse)
  , "DeleteObject" :: NullOrUndefined (BatchDeleteObjectResponse)
  , "AddFacetToObject" :: NullOrUndefined (BatchAddFacetToObjectResponse)
  , "RemoveFacetFromObject" :: NullOrUndefined (BatchRemoveFacetFromObjectResponse)
  , "AttachPolicy" :: NullOrUndefined (BatchAttachPolicyResponse)
  , "DetachPolicy" :: NullOrUndefined (BatchDetachPolicyResponse)
  , "CreateIndex" :: NullOrUndefined (BatchCreateIndexResponse)
  , "AttachToIndex" :: NullOrUndefined (BatchAttachToIndexResponse)
  , "DetachFromIndex" :: NullOrUndefined (BatchDetachFromIndexResponse)
  , "AttachTypedLink" :: NullOrUndefined (BatchAttachTypedLinkResponse)
  , "DetachTypedLink" :: NullOrUndefined (BatchDetachTypedLinkResponse)
  }
derive instance newtypeBatchWriteOperationResponse :: Newtype BatchWriteOperationResponse _


newtype BatchWriteOperationResponseList = BatchWriteOperationResponseList (Array BatchWriteOperationResponse)
derive instance newtypeBatchWriteOperationResponseList :: Newtype BatchWriteOperationResponseList _


newtype BatchWriteRequest = BatchWriteRequest 
  { "DirectoryArn" :: (Arn)
  , "Operations" :: (BatchWriteOperationList)
  }
derive instance newtypeBatchWriteRequest :: Newtype BatchWriteRequest _


newtype BatchWriteResponse = BatchWriteResponse 
  { "Responses" :: NullOrUndefined (BatchWriteOperationResponseList)
  }
derive instance newtypeBatchWriteResponse :: Newtype BatchWriteResponse _


newtype BinaryAttributeValue = BinaryAttributeValue String
derive instance newtypeBinaryAttributeValue :: Newtype BinaryAttributeValue _


newtype Bool = Bool Boolean
derive instance newtypeBool :: Newtype Bool _


newtype BooleanAttributeValue = BooleanAttributeValue Boolean
derive instance newtypeBooleanAttributeValue :: Newtype BooleanAttributeValue _


-- | <p>Cannot list the parents of a <a>Directory</a> root.</p>
newtype CannotListParentOfRootException = CannotListParentOfRootException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeCannotListParentOfRootException :: Newtype CannotListParentOfRootException _


newtype ConsistencyLevel = ConsistencyLevel String
derive instance newtypeConsistencyLevel :: Newtype ConsistencyLevel _


newtype CreateDirectoryRequest = CreateDirectoryRequest 
  { "Name" :: (DirectoryName)
  , "SchemaArn" :: (Arn)
  }
derive instance newtypeCreateDirectoryRequest :: Newtype CreateDirectoryRequest _


newtype CreateDirectoryResponse = CreateDirectoryResponse 
  { "DirectoryArn" :: (DirectoryArn)
  , "Name" :: (DirectoryName)
  , "ObjectIdentifier" :: (ObjectIdentifier)
  , "AppliedSchemaArn" :: (Arn)
  }
derive instance newtypeCreateDirectoryResponse :: Newtype CreateDirectoryResponse _


newtype CreateFacetRequest = CreateFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  , "Attributes" :: NullOrUndefined (FacetAttributeList)
  , "ObjectType" :: (ObjectType)
  }
derive instance newtypeCreateFacetRequest :: Newtype CreateFacetRequest _


newtype CreateFacetResponse = CreateFacetResponse 
  { 
  }
derive instance newtypeCreateFacetResponse :: Newtype CreateFacetResponse _


newtype CreateIndexRequest = CreateIndexRequest 
  { "DirectoryArn" :: (Arn)
  , "OrderedIndexedAttributeList" :: (AttributeKeyList)
  , "IsUnique" :: (Bool)
  , "ParentReference" :: NullOrUndefined (ObjectReference)
  , "LinkName" :: NullOrUndefined (LinkName)
  }
derive instance newtypeCreateIndexRequest :: Newtype CreateIndexRequest _


newtype CreateIndexResponse = CreateIndexResponse 
  { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeCreateIndexResponse :: Newtype CreateIndexResponse _


newtype CreateObjectRequest = CreateObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "SchemaFacets" :: (SchemaFacetList)
  , "ObjectAttributeList" :: NullOrUndefined (AttributeKeyAndValueList)
  , "ParentReference" :: NullOrUndefined (ObjectReference)
  , "LinkName" :: NullOrUndefined (LinkName)
  }
derive instance newtypeCreateObjectRequest :: Newtype CreateObjectRequest _


newtype CreateObjectResponse = CreateObjectResponse 
  { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeCreateObjectResponse :: Newtype CreateObjectResponse _


newtype CreateSchemaRequest = CreateSchemaRequest 
  { "Name" :: (SchemaName)
  }
derive instance newtypeCreateSchemaRequest :: Newtype CreateSchemaRequest _


newtype CreateSchemaResponse = CreateSchemaResponse 
  { "SchemaArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeCreateSchemaResponse :: Newtype CreateSchemaResponse _


newtype CreateTypedLinkFacetRequest = CreateTypedLinkFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Facet" :: (TypedLinkFacet)
  }
derive instance newtypeCreateTypedLinkFacetRequest :: Newtype CreateTypedLinkFacetRequest _


newtype CreateTypedLinkFacetResponse = CreateTypedLinkFacetResponse 
  { 
  }
derive instance newtypeCreateTypedLinkFacetResponse :: Newtype CreateTypedLinkFacetResponse _


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _


newtype DatetimeAttributeValue = DatetimeAttributeValue Number
derive instance newtypeDatetimeAttributeValue :: Newtype DatetimeAttributeValue _


newtype DeleteDirectoryRequest = DeleteDirectoryRequest 
  { "DirectoryArn" :: (Arn)
  }
derive instance newtypeDeleteDirectoryRequest :: Newtype DeleteDirectoryRequest _


newtype DeleteDirectoryResponse = DeleteDirectoryResponse 
  { "DirectoryArn" :: (Arn)
  }
derive instance newtypeDeleteDirectoryResponse :: Newtype DeleteDirectoryResponse _


newtype DeleteFacetRequest = DeleteFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  }
derive instance newtypeDeleteFacetRequest :: Newtype DeleteFacetRequest _


newtype DeleteFacetResponse = DeleteFacetResponse 
  { 
  }
derive instance newtypeDeleteFacetResponse :: Newtype DeleteFacetResponse _


newtype DeleteObjectRequest = DeleteObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeDeleteObjectRequest :: Newtype DeleteObjectRequest _


newtype DeleteObjectResponse = DeleteObjectResponse 
  { 
  }
derive instance newtypeDeleteObjectResponse :: Newtype DeleteObjectResponse _


newtype DeleteSchemaRequest = DeleteSchemaRequest 
  { "SchemaArn" :: (Arn)
  }
derive instance newtypeDeleteSchemaRequest :: Newtype DeleteSchemaRequest _


newtype DeleteSchemaResponse = DeleteSchemaResponse 
  { "SchemaArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeDeleteSchemaResponse :: Newtype DeleteSchemaResponse _


newtype DeleteTypedLinkFacetRequest = DeleteTypedLinkFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (TypedLinkName)
  }
derive instance newtypeDeleteTypedLinkFacetRequest :: Newtype DeleteTypedLinkFacetRequest _


newtype DeleteTypedLinkFacetResponse = DeleteTypedLinkFacetResponse 
  { 
  }
derive instance newtypeDeleteTypedLinkFacetResponse :: Newtype DeleteTypedLinkFacetResponse _


newtype DetachFromIndexRequest = DetachFromIndexRequest 
  { "DirectoryArn" :: (Arn)
  , "IndexReference" :: (ObjectReference)
  , "TargetReference" :: (ObjectReference)
  }
derive instance newtypeDetachFromIndexRequest :: Newtype DetachFromIndexRequest _


newtype DetachFromIndexResponse = DetachFromIndexResponse 
  { "DetachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeDetachFromIndexResponse :: Newtype DetachFromIndexResponse _


newtype DetachObjectRequest = DetachObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "ParentReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  }
derive instance newtypeDetachObjectRequest :: Newtype DetachObjectRequest _


newtype DetachObjectResponse = DetachObjectResponse 
  { "DetachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeDetachObjectResponse :: Newtype DetachObjectResponse _


newtype DetachPolicyRequest = DetachPolicyRequest 
  { "DirectoryArn" :: (Arn)
  , "PolicyReference" :: (ObjectReference)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeDetachPolicyRequest :: Newtype DetachPolicyRequest _


newtype DetachPolicyResponse = DetachPolicyResponse 
  { 
  }
derive instance newtypeDetachPolicyResponse :: Newtype DetachPolicyResponse _


newtype DetachTypedLinkRequest = DetachTypedLinkRequest 
  { "DirectoryArn" :: (Arn)
  , "TypedLinkSpecifier" :: (TypedLinkSpecifier)
  }
derive instance newtypeDetachTypedLinkRequest :: Newtype DetachTypedLinkRequest _


-- | <p>Directory structure that includes the directory name and directory ARN.</p>
newtype Directory = Directory 
  { "Name" :: NullOrUndefined (DirectoryName)
  , "DirectoryArn" :: NullOrUndefined (DirectoryArn)
  , "State" :: NullOrUndefined (DirectoryState)
  , "CreationDateTime" :: NullOrUndefined (Date)
  }
derive instance newtypeDirectory :: Newtype Directory _


-- | <p>Indicates that a <a>Directory</a> could not be created due to a naming conflict. Choose a different name and try again.</p>
newtype DirectoryAlreadyExistsException = DirectoryAlreadyExistsException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDirectoryAlreadyExistsException :: Newtype DirectoryAlreadyExistsException _


newtype DirectoryArn = DirectoryArn String
derive instance newtypeDirectoryArn :: Newtype DirectoryArn _


-- | <p>A directory that has been deleted and to which access has been attempted. Note: The requested resource will eventually cease to exist.</p>
newtype DirectoryDeletedException = DirectoryDeletedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDirectoryDeletedException :: Newtype DirectoryDeletedException _


newtype DirectoryList = DirectoryList (Array Directory)
derive instance newtypeDirectoryList :: Newtype DirectoryList _


newtype DirectoryName = DirectoryName String
derive instance newtypeDirectoryName :: Newtype DirectoryName _


-- | <p>An operation can only operate on a disabled directory.</p>
newtype DirectoryNotDisabledException = DirectoryNotDisabledException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDirectoryNotDisabledException :: Newtype DirectoryNotDisabledException _


-- | <p>An operation can only operate on a directory that is not enabled.</p>
newtype DirectoryNotEnabledException = DirectoryNotEnabledException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDirectoryNotEnabledException :: Newtype DirectoryNotEnabledException _


newtype DirectoryState = DirectoryState String
derive instance newtypeDirectoryState :: Newtype DirectoryState _


newtype DisableDirectoryRequest = DisableDirectoryRequest 
  { "DirectoryArn" :: (Arn)
  }
derive instance newtypeDisableDirectoryRequest :: Newtype DisableDirectoryRequest _


newtype DisableDirectoryResponse = DisableDirectoryResponse 
  { "DirectoryArn" :: (Arn)
  }
derive instance newtypeDisableDirectoryResponse :: Newtype DisableDirectoryResponse _


newtype EnableDirectoryRequest = EnableDirectoryRequest 
  { "DirectoryArn" :: (Arn)
  }
derive instance newtypeEnableDirectoryRequest :: Newtype EnableDirectoryRequest _


newtype EnableDirectoryResponse = EnableDirectoryResponse 
  { "DirectoryArn" :: (Arn)
  }
derive instance newtypeEnableDirectoryResponse :: Newtype EnableDirectoryResponse _


newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _


-- | <p>A structure that contains <code>Name</code>, <code>ARN</code>, <code>Attributes</code>, <a>Rule</a>s, and <code>ObjectTypes</code>.</p>
newtype Facet = Facet 
  { "Name" :: NullOrUndefined (FacetName)
  , "ObjectType" :: NullOrUndefined (ObjectType)
  }
derive instance newtypeFacet :: Newtype Facet _


-- | <p>A facet with the same name already exists.</p>
newtype FacetAlreadyExistsException = FacetAlreadyExistsException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeFacetAlreadyExistsException :: Newtype FacetAlreadyExistsException _


-- | <p>An attribute that is associated with the <a>Facet</a>.</p>
newtype FacetAttribute = FacetAttribute 
  { "Name" :: (AttributeName)
  , "AttributeDefinition" :: NullOrUndefined (FacetAttributeDefinition)
  , "AttributeReference" :: NullOrUndefined (FacetAttributeReference)
  , "RequiredBehavior" :: NullOrUndefined (RequiredAttributeBehavior)
  }
derive instance newtypeFacetAttribute :: Newtype FacetAttribute _


-- | <p>A facet attribute definition. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences">Attribute References</a> for more information.</p>
newtype FacetAttributeDefinition = FacetAttributeDefinition 
  { "Type" :: (FacetAttributeType)
  , "DefaultValue" :: NullOrUndefined (TypedAttributeValue)
  , "IsImmutable" :: NullOrUndefined (Bool)
  , "Rules" :: NullOrUndefined (RuleMap)
  }
derive instance newtypeFacetAttributeDefinition :: Newtype FacetAttributeDefinition _


newtype FacetAttributeList = FacetAttributeList (Array FacetAttribute)
derive instance newtypeFacetAttributeList :: Newtype FacetAttributeList _


-- | <p>The facet attribute reference that specifies the attribute definition that contains the attribute facet name and attribute name.</p>
newtype FacetAttributeReference = FacetAttributeReference 
  { "TargetFacetName" :: (FacetName)
  , "TargetAttributeName" :: (AttributeName)
  }
derive instance newtypeFacetAttributeReference :: Newtype FacetAttributeReference _


newtype FacetAttributeType = FacetAttributeType String
derive instance newtypeFacetAttributeType :: Newtype FacetAttributeType _


-- | <p>A structure that contains information used to update an attribute.</p>
newtype FacetAttributeUpdate = FacetAttributeUpdate 
  { "Attribute" :: NullOrUndefined (FacetAttribute)
  , "Action" :: NullOrUndefined (UpdateActionType)
  }
derive instance newtypeFacetAttributeUpdate :: Newtype FacetAttributeUpdate _


newtype FacetAttributeUpdateList = FacetAttributeUpdateList (Array FacetAttributeUpdate)
derive instance newtypeFacetAttributeUpdateList :: Newtype FacetAttributeUpdateList _


-- | <p>Occurs when deleting a facet that contains an attribute that is a target to an attribute reference in a different facet.</p>
newtype FacetInUseException = FacetInUseException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeFacetInUseException :: Newtype FacetInUseException _


newtype FacetName = FacetName String
derive instance newtypeFacetName :: Newtype FacetName _


newtype FacetNameList = FacetNameList (Array FacetName)
derive instance newtypeFacetNameList :: Newtype FacetNameList _


-- | <p>The specified <a>Facet</a> could not be found.</p>
newtype FacetNotFoundException = FacetNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeFacetNotFoundException :: Newtype FacetNotFoundException _


-- | <p>The <a>Facet</a> that you provided was not well formed or could not be validated with the schema.</p>
newtype FacetValidationException = FacetValidationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeFacetValidationException :: Newtype FacetValidationException _


newtype GetAppliedSchemaVersionRequest = GetAppliedSchemaVersionRequest 
  { "SchemaArn" :: (Arn)
  }
derive instance newtypeGetAppliedSchemaVersionRequest :: Newtype GetAppliedSchemaVersionRequest _


newtype GetAppliedSchemaVersionResponse = GetAppliedSchemaVersionResponse 
  { "AppliedSchemaArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeGetAppliedSchemaVersionResponse :: Newtype GetAppliedSchemaVersionResponse _


newtype GetDirectoryRequest = GetDirectoryRequest 
  { "DirectoryArn" :: (DirectoryArn)
  }
derive instance newtypeGetDirectoryRequest :: Newtype GetDirectoryRequest _


newtype GetDirectoryResponse = GetDirectoryResponse 
  { "Directory" :: (Directory)
  }
derive instance newtypeGetDirectoryResponse :: Newtype GetDirectoryResponse _


newtype GetFacetRequest = GetFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  }
derive instance newtypeGetFacetRequest :: Newtype GetFacetRequest _


newtype GetFacetResponse = GetFacetResponse 
  { "Facet" :: NullOrUndefined (Facet)
  }
derive instance newtypeGetFacetResponse :: Newtype GetFacetResponse _


newtype GetObjectInformationRequest = GetObjectInformationRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeGetObjectInformationRequest :: Newtype GetObjectInformationRequest _


newtype GetObjectInformationResponse = GetObjectInformationResponse 
  { "SchemaFacets" :: NullOrUndefined (SchemaFacetList)
  , "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeGetObjectInformationResponse :: Newtype GetObjectInformationResponse _


newtype GetSchemaAsJsonRequest = GetSchemaAsJsonRequest 
  { "SchemaArn" :: (Arn)
  }
derive instance newtypeGetSchemaAsJsonRequest :: Newtype GetSchemaAsJsonRequest _


newtype GetSchemaAsJsonResponse = GetSchemaAsJsonResponse 
  { "Name" :: NullOrUndefined (SchemaName)
  , "Document" :: NullOrUndefined (SchemaJsonDocument)
  }
derive instance newtypeGetSchemaAsJsonResponse :: Newtype GetSchemaAsJsonResponse _


newtype GetTypedLinkFacetInformationRequest = GetTypedLinkFacetInformationRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (TypedLinkName)
  }
derive instance newtypeGetTypedLinkFacetInformationRequest :: Newtype GetTypedLinkFacetInformationRequest _


newtype GetTypedLinkFacetInformationResponse = GetTypedLinkFacetInformationResponse 
  { "IdentityAttributeOrder" :: NullOrUndefined (AttributeNameList)
  }
derive instance newtypeGetTypedLinkFacetInformationResponse :: Newtype GetTypedLinkFacetInformationResponse _


-- | <p>Indicates a failure occurred while performing a check for backward compatibility between the specified schema and the schema that is currently applied to the directory.</p>
newtype IncompatibleSchemaException = IncompatibleSchemaException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeIncompatibleSchemaException :: Newtype IncompatibleSchemaException _


-- | <p>Represents an index and an attached object.</p>
newtype IndexAttachment = IndexAttachment 
  { "IndexedAttributes" :: NullOrUndefined (AttributeKeyAndValueList)
  , "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeIndexAttachment :: Newtype IndexAttachment _


newtype IndexAttachmentList = IndexAttachmentList (Array IndexAttachment)
derive instance newtypeIndexAttachmentList :: Newtype IndexAttachmentList _


-- | <p>An object has been attempted to be attached to an object that does not have the appropriate attribute value.</p>
newtype IndexedAttributeMissingException = IndexedAttributeMissingException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeIndexedAttributeMissingException :: Newtype IndexedAttributeMissingException _


-- | <p>Indicates a problem that must be resolved by Amazon Web Services. This might be a transient error in which case you can retry your request until it succeeds. Otherwise, go to the <a href="http://status.aws.amazon.com/">AWS Service Health Dashboard</a> site to see if there are any operational issues with the service.</p>
newtype InternalServiceException = InternalServiceException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInternalServiceException :: Newtype InternalServiceException _


-- | <p>Indicates that the provided ARN value is not valid.</p>
newtype InvalidArnException = InvalidArnException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidArnException :: Newtype InvalidArnException _


-- | <p>Indicates that an attempt to attach an object with the same link name or to apply a schema with the same name has occurred. Rename the link or the schema and then try again.</p>
newtype InvalidAttachmentException = InvalidAttachmentException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidAttachmentException :: Newtype InvalidAttachmentException _


-- | <p>An attempt to modify a <a>Facet</a> resulted in an invalid schema exception.</p>
newtype InvalidFacetUpdateException = InvalidFacetUpdateException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidFacetUpdateException :: Newtype InvalidFacetUpdateException _


-- | <p>Indicates that the <code>NextToken</code> value is not valid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _


-- | <p>Occurs when any of the rule parameter keys or values are invalid.</p>
newtype InvalidRuleException = InvalidRuleException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidRuleException :: Newtype InvalidRuleException _


-- | <p>Indicates that the provided <code>SchemaDoc</code> value is not valid.</p>
newtype InvalidSchemaDocException = InvalidSchemaDocException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidSchemaDocException :: Newtype InvalidSchemaDocException _


-- | <p>Can occur for multiple reasons such as when you tag a resource that doesn’t exist or if you specify a higher number of tags for a resource than the allowed limit. Allowed limit is 50 tags per resource.</p>
newtype InvalidTaggingRequestException = InvalidTaggingRequestException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidTaggingRequestException :: Newtype InvalidTaggingRequestException _


-- | <p>Indicates that limits are exceeded. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/limits.html">Limits</a> for more information.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype LinkName = LinkName String
derive instance newtypeLinkName :: Newtype LinkName _


-- | <p>Indicates that a link could not be created due to a naming conflict. Choose a different name and then try again.</p>
newtype LinkNameAlreadyInUseException = LinkNameAlreadyInUseException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeLinkNameAlreadyInUseException :: Newtype LinkNameAlreadyInUseException _


newtype LinkNameToObjectIdentifierMap = LinkNameToObjectIdentifierMap (Map LinkName ObjectIdentifier)
derive instance newtypeLinkNameToObjectIdentifierMap :: Newtype LinkNameToObjectIdentifierMap _


newtype ListAppliedSchemaArnsRequest = ListAppliedSchemaArnsRequest 
  { "DirectoryArn" :: (Arn)
  , "SchemaArn" :: NullOrUndefined (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeListAppliedSchemaArnsRequest :: Newtype ListAppliedSchemaArnsRequest _


newtype ListAppliedSchemaArnsResponse = ListAppliedSchemaArnsResponse 
  { "SchemaArns" :: NullOrUndefined (Arns)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListAppliedSchemaArnsResponse :: Newtype ListAppliedSchemaArnsResponse _


newtype ListAttachedIndicesRequest = ListAttachedIndicesRequest 
  { "DirectoryArn" :: (Arn)
  , "TargetReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListAttachedIndicesRequest :: Newtype ListAttachedIndicesRequest _


newtype ListAttachedIndicesResponse = ListAttachedIndicesResponse 
  { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListAttachedIndicesResponse :: Newtype ListAttachedIndicesResponse _


newtype ListDevelopmentSchemaArnsRequest = ListDevelopmentSchemaArnsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeListDevelopmentSchemaArnsRequest :: Newtype ListDevelopmentSchemaArnsRequest _


newtype ListDevelopmentSchemaArnsResponse = ListDevelopmentSchemaArnsResponse 
  { "SchemaArns" :: NullOrUndefined (Arns)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDevelopmentSchemaArnsResponse :: Newtype ListDevelopmentSchemaArnsResponse _


newtype ListDirectoriesRequest = ListDirectoriesRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "State'" :: NullOrUndefined (DirectoryState)
  }
derive instance newtypeListDirectoriesRequest :: Newtype ListDirectoriesRequest _


newtype ListDirectoriesResponse = ListDirectoriesResponse 
  { "Directories" :: (DirectoryList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDirectoriesResponse :: Newtype ListDirectoriesResponse _


newtype ListFacetAttributesRequest = ListFacetAttributesRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeListFacetAttributesRequest :: Newtype ListFacetAttributesRequest _


newtype ListFacetAttributesResponse = ListFacetAttributesResponse 
  { "Attributes" :: NullOrUndefined (FacetAttributeList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListFacetAttributesResponse :: Newtype ListFacetAttributesResponse _


newtype ListFacetNamesRequest = ListFacetNamesRequest 
  { "SchemaArn" :: (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeListFacetNamesRequest :: Newtype ListFacetNamesRequest _


newtype ListFacetNamesResponse = ListFacetNamesResponse 
  { "FacetNames" :: NullOrUndefined (FacetNameList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListFacetNamesResponse :: Newtype ListFacetNamesResponse _


newtype ListIncomingTypedLinksRequest = ListIncomingTypedLinksRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList)
  , "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListIncomingTypedLinksRequest :: Newtype ListIncomingTypedLinksRequest _


newtype ListIncomingTypedLinksResponse = ListIncomingTypedLinksResponse 
  { "LinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListIncomingTypedLinksResponse :: Newtype ListIncomingTypedLinksResponse _


newtype ListIndexRequest = ListIndexRequest 
  { "DirectoryArn" :: (Arn)
  , "RangesOnIndexedValues" :: NullOrUndefined (ObjectAttributeRangeList)
  , "IndexReference" :: (ObjectReference)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListIndexRequest :: Newtype ListIndexRequest _


newtype ListIndexResponse = ListIndexResponse 
  { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListIndexResponse :: Newtype ListIndexResponse _


newtype ListObjectAttributesRequest = ListObjectAttributesRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  , "FacetFilter" :: NullOrUndefined (SchemaFacet)
  }
derive instance newtypeListObjectAttributesRequest :: Newtype ListObjectAttributesRequest _


newtype ListObjectAttributesResponse = ListObjectAttributesResponse 
  { "Attributes" :: NullOrUndefined (AttributeKeyAndValueList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListObjectAttributesResponse :: Newtype ListObjectAttributesResponse _


newtype ListObjectChildrenRequest = ListObjectChildrenRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListObjectChildrenRequest :: Newtype ListObjectChildrenRequest _


newtype ListObjectChildrenResponse = ListObjectChildrenResponse 
  { "Children" :: NullOrUndefined (LinkNameToObjectIdentifierMap)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListObjectChildrenResponse :: Newtype ListObjectChildrenResponse _


newtype ListObjectParentPathsRequest = ListObjectParentPathsRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeListObjectParentPathsRequest :: Newtype ListObjectParentPathsRequest _


newtype ListObjectParentPathsResponse = ListObjectParentPathsResponse 
  { "PathToObjectIdentifiersList" :: NullOrUndefined (PathToObjectIdentifiersList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListObjectParentPathsResponse :: Newtype ListObjectParentPathsResponse _


newtype ListObjectParentsRequest = ListObjectParentsRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListObjectParentsRequest :: Newtype ListObjectParentsRequest _


newtype ListObjectParentsResponse = ListObjectParentsResponse 
  { "Parents" :: NullOrUndefined (ObjectIdentifierToLinkNameMap)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListObjectParentsResponse :: Newtype ListObjectParentsResponse _


newtype ListObjectPoliciesRequest = ListObjectPoliciesRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListObjectPoliciesRequest :: Newtype ListObjectPoliciesRequest _


newtype ListObjectPoliciesResponse = ListObjectPoliciesResponse 
  { "AttachedPolicyIds" :: NullOrUndefined (ObjectIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListObjectPoliciesResponse :: Newtype ListObjectPoliciesResponse _


newtype ListOutgoingTypedLinksRequest = ListOutgoingTypedLinksRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList)
  , "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListOutgoingTypedLinksRequest :: Newtype ListOutgoingTypedLinksRequest _


newtype ListOutgoingTypedLinksResponse = ListOutgoingTypedLinksResponse 
  { "TypedLinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListOutgoingTypedLinksResponse :: Newtype ListOutgoingTypedLinksResponse _


newtype ListPolicyAttachmentsRequest = ListPolicyAttachmentsRequest 
  { "DirectoryArn" :: (Arn)
  , "PolicyReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListPolicyAttachmentsRequest :: Newtype ListPolicyAttachmentsRequest _


newtype ListPolicyAttachmentsResponse = ListPolicyAttachmentsResponse 
  { "ObjectIdentifiers" :: NullOrUndefined (ObjectIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListPolicyAttachmentsResponse :: Newtype ListPolicyAttachmentsResponse _


newtype ListPublishedSchemaArnsRequest = ListPublishedSchemaArnsRequest 
  { "SchemaArn" :: NullOrUndefined (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeListPublishedSchemaArnsRequest :: Newtype ListPublishedSchemaArnsRequest _


newtype ListPublishedSchemaArnsResponse = ListPublishedSchemaArnsResponse 
  { "SchemaArns" :: NullOrUndefined (Arns)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListPublishedSchemaArnsResponse :: Newtype ListPublishedSchemaArnsResponse _


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceArn" :: (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (TagsNumberResults)
  }
derive instance newtypeListTagsForResourceRequest :: Newtype ListTagsForResourceRequest _


newtype ListTagsForResourceResponse = ListTagsForResourceResponse 
  { "Tags" :: NullOrUndefined (TagList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListTagsForResourceResponse :: Newtype ListTagsForResourceResponse _


newtype ListTypedLinkFacetAttributesRequest = ListTypedLinkFacetAttributesRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (TypedLinkName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeListTypedLinkFacetAttributesRequest :: Newtype ListTypedLinkFacetAttributesRequest _


newtype ListTypedLinkFacetAttributesResponse = ListTypedLinkFacetAttributesResponse 
  { "Attributes" :: NullOrUndefined (TypedLinkAttributeDefinitionList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListTypedLinkFacetAttributesResponse :: Newtype ListTypedLinkFacetAttributesResponse _


newtype ListTypedLinkFacetNamesRequest = ListTypedLinkFacetNamesRequest 
  { "SchemaArn" :: (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeListTypedLinkFacetNamesRequest :: Newtype ListTypedLinkFacetNamesRequest _


newtype ListTypedLinkFacetNamesResponse = ListTypedLinkFacetNamesResponse 
  { "FacetNames" :: NullOrUndefined (TypedLinkNameList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListTypedLinkFacetNamesResponse :: Newtype ListTypedLinkFacetNamesResponse _


newtype LookupPolicyRequest = LookupPolicyRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }
derive instance newtypeLookupPolicyRequest :: Newtype LookupPolicyRequest _


newtype LookupPolicyResponse = LookupPolicyResponse 
  { "PolicyToPathList" :: NullOrUndefined (PolicyToPathList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeLookupPolicyResponse :: Newtype LookupPolicyResponse _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>Indicates that the requested operation can only operate on index objects.</p>
newtype NotIndexException = NotIndexException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeNotIndexException :: Newtype NotIndexException _


-- | <p>Occurs when any invalid operations are performed on an object that is not a node, such as calling <code>ListObjectChildren</code> for a leaf node object.</p>
newtype NotNodeException = NotNodeException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeNotNodeException :: Newtype NotNodeException _


-- | <p>Indicates that the requested operation can only operate on policy objects.</p>
newtype NotPolicyException = NotPolicyException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeNotPolicyException :: Newtype NotPolicyException _


newtype NumberAttributeValue = NumberAttributeValue String
derive instance newtypeNumberAttributeValue :: Newtype NumberAttributeValue _


newtype NumberResults = NumberResults Int
derive instance newtypeNumberResults :: Newtype NumberResults _


-- | <p>Indicates that the object is not attached to the index.</p>
newtype ObjectAlreadyDetachedException = ObjectAlreadyDetachedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeObjectAlreadyDetachedException :: Newtype ObjectAlreadyDetachedException _


-- | <p>The action to take on the object attribute.</p>
newtype ObjectAttributeAction = ObjectAttributeAction 
  { "ObjectAttributeActionType" :: NullOrUndefined (UpdateActionType)
  , "ObjectAttributeUpdateValue" :: NullOrUndefined (TypedAttributeValue)
  }
derive instance newtypeObjectAttributeAction :: Newtype ObjectAttributeAction _


-- | <p>A range of attributes.</p>
newtype ObjectAttributeRange = ObjectAttributeRange 
  { "AttributeKey" :: NullOrUndefined (AttributeKey)
  , "Range" :: NullOrUndefined (TypedAttributeValueRange)
  }
derive instance newtypeObjectAttributeRange :: Newtype ObjectAttributeRange _


newtype ObjectAttributeRangeList = ObjectAttributeRangeList (Array ObjectAttributeRange)
derive instance newtypeObjectAttributeRangeList :: Newtype ObjectAttributeRangeList _


-- | <p>Structure that contains attribute update information.</p>
newtype ObjectAttributeUpdate = ObjectAttributeUpdate 
  { "ObjectAttributeKey" :: NullOrUndefined (AttributeKey)
  , "ObjectAttributeAction" :: NullOrUndefined (ObjectAttributeAction)
  }
derive instance newtypeObjectAttributeUpdate :: Newtype ObjectAttributeUpdate _


newtype ObjectAttributeUpdateList = ObjectAttributeUpdateList (Array ObjectAttributeUpdate)
derive instance newtypeObjectAttributeUpdateList :: Newtype ObjectAttributeUpdateList _


newtype ObjectIdentifier = ObjectIdentifier String
derive instance newtypeObjectIdentifier :: Newtype ObjectIdentifier _


newtype ObjectIdentifierList = ObjectIdentifierList (Array ObjectIdentifier)
derive instance newtypeObjectIdentifierList :: Newtype ObjectIdentifierList _


newtype ObjectIdentifierToLinkNameMap = ObjectIdentifierToLinkNameMap (Map ObjectIdentifier LinkName)
derive instance newtypeObjectIdentifierToLinkNameMap :: Newtype ObjectIdentifierToLinkNameMap _


-- | <p>Indicates that the requested operation cannot be completed because the object has not been detached from the tree.</p>
newtype ObjectNotDetachedException = ObjectNotDetachedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeObjectNotDetachedException :: Newtype ObjectNotDetachedException _


-- | <p>The reference that identifies an object.</p>
newtype ObjectReference = ObjectReference 
  { "Selector" :: NullOrUndefined (SelectorObjectReference)
  }
derive instance newtypeObjectReference :: Newtype ObjectReference _


newtype ObjectType = ObjectType String
derive instance newtypeObjectType :: Newtype ObjectType _


newtype PathString = PathString String
derive instance newtypePathString :: Newtype PathString _


-- | <p>Returns the path to the <code>ObjectIdentifiers</code> that is associated with the directory.</p>
newtype PathToObjectIdentifiers = PathToObjectIdentifiers 
  { "Path" :: NullOrUndefined (PathString)
  , "ObjectIdentifiers" :: NullOrUndefined (ObjectIdentifierList)
  }
derive instance newtypePathToObjectIdentifiers :: Newtype PathToObjectIdentifiers _


newtype PathToObjectIdentifiersList = PathToObjectIdentifiersList (Array PathToObjectIdentifiers)
derive instance newtypePathToObjectIdentifiersList :: Newtype PathToObjectIdentifiersList _


-- | <p>Contains the <code>PolicyType</code>, <code>PolicyId</code>, and the <code>ObjectIdentifier</code> to which it is attached. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies">Policies</a>.</p>
newtype PolicyAttachment = PolicyAttachment 
  { "PolicyId" :: NullOrUndefined (ObjectIdentifier)
  , "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  , "PolicyType" :: NullOrUndefined (PolicyType)
  }
derive instance newtypePolicyAttachment :: Newtype PolicyAttachment _


newtype PolicyAttachmentList = PolicyAttachmentList (Array PolicyAttachment)
derive instance newtypePolicyAttachmentList :: Newtype PolicyAttachmentList _


-- | <p>Used when a regular object exists in a <a>Directory</a> and you want to find all of the policies that are associated with that object and the parent to that object.</p>
newtype PolicyToPath = PolicyToPath 
  { "Path" :: NullOrUndefined (PathString)
  , "Policies" :: NullOrUndefined (PolicyAttachmentList)
  }
derive instance newtypePolicyToPath :: Newtype PolicyToPath _


newtype PolicyToPathList = PolicyToPathList (Array PolicyToPath)
derive instance newtypePolicyToPathList :: Newtype PolicyToPathList _


newtype PolicyType = PolicyType String
derive instance newtypePolicyType :: Newtype PolicyType _


newtype PublishSchemaRequest = PublishSchemaRequest 
  { "DevelopmentSchemaArn" :: (Arn)
  , "Version" :: (Version)
  , "MinorVersion" :: NullOrUndefined (Version)
  , "Name" :: NullOrUndefined (SchemaName)
  }
derive instance newtypePublishSchemaRequest :: Newtype PublishSchemaRequest _


newtype PublishSchemaResponse = PublishSchemaResponse 
  { "PublishedSchemaArn" :: NullOrUndefined (Arn)
  }
derive instance newtypePublishSchemaResponse :: Newtype PublishSchemaResponse _


newtype PutSchemaFromJsonRequest = PutSchemaFromJsonRequest 
  { "SchemaArn" :: (Arn)
  , "Document" :: (SchemaJsonDocument)
  }
derive instance newtypePutSchemaFromJsonRequest :: Newtype PutSchemaFromJsonRequest _


newtype PutSchemaFromJsonResponse = PutSchemaFromJsonResponse 
  { "Arn" :: NullOrUndefined (Arn)
  }
derive instance newtypePutSchemaFromJsonResponse :: Newtype PutSchemaFromJsonResponse _


newtype RangeMode = RangeMode String
derive instance newtypeRangeMode :: Newtype RangeMode _


newtype RemoveFacetFromObjectRequest = RemoveFacetFromObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "SchemaFacet" :: (SchemaFacet)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeRemoveFacetFromObjectRequest :: Newtype RemoveFacetFromObjectRequest _


newtype RemoveFacetFromObjectResponse = RemoveFacetFromObjectResponse 
  { 
  }
derive instance newtypeRemoveFacetFromObjectResponse :: Newtype RemoveFacetFromObjectResponse _


newtype RequiredAttributeBehavior = RequiredAttributeBehavior String
derive instance newtypeRequiredAttributeBehavior :: Newtype RequiredAttributeBehavior _


-- | <p>The specified resource could not be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>Occurs when a conflict with a previous successful write is detected. For example, if a write operation occurs on an object and then an attempt is made to read the object using “SERIALIZABLE” consistency, this exception may result. This generally occurs when the previous write did not have time to propagate to the host serving the current request. A retry (with appropriate backoff logic) is the recommended response to this exception.</p>
newtype RetryableConflictException = RetryableConflictException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeRetryableConflictException :: Newtype RetryableConflictException _


-- | <p>Contains an Amazon Resource Name (ARN) and parameters that are associated with the rule.</p>
newtype Rule = Rule 
  { "Type" :: NullOrUndefined (RuleType)
  , "Parameters" :: NullOrUndefined (RuleParameterMap)
  }
derive instance newtypeRule :: Newtype Rule _


newtype RuleKey = RuleKey String
derive instance newtypeRuleKey :: Newtype RuleKey _


newtype RuleMap = RuleMap (Map RuleKey Rule)
derive instance newtypeRuleMap :: Newtype RuleMap _


newtype RuleParameterKey = RuleParameterKey String
derive instance newtypeRuleParameterKey :: Newtype RuleParameterKey _


newtype RuleParameterMap = RuleParameterMap (Map RuleParameterKey RuleParameterValue)
derive instance newtypeRuleParameterMap :: Newtype RuleParameterMap _


newtype RuleParameterValue = RuleParameterValue String
derive instance newtypeRuleParameterValue :: Newtype RuleParameterValue _


newtype RuleType = RuleType String
derive instance newtypeRuleType :: Newtype RuleType _


-- | <p>Indicates that a schema could not be created due to a naming conflict. Please select a different name and then try again.</p>
newtype SchemaAlreadyExistsException = SchemaAlreadyExistsException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeSchemaAlreadyExistsException :: Newtype SchemaAlreadyExistsException _


-- | <p>Indicates that a schema is already published.</p>
newtype SchemaAlreadyPublishedException = SchemaAlreadyPublishedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeSchemaAlreadyPublishedException :: Newtype SchemaAlreadyPublishedException _


-- | <p>A facet.</p>
newtype SchemaFacet = SchemaFacet 
  { "SchemaArn" :: NullOrUndefined (Arn)
  , "FacetName" :: NullOrUndefined (FacetName)
  }
derive instance newtypeSchemaFacet :: Newtype SchemaFacet _


newtype SchemaFacetList = SchemaFacetList (Array SchemaFacet)
derive instance newtypeSchemaFacetList :: Newtype SchemaFacetList _


newtype SchemaJsonDocument = SchemaJsonDocument String
derive instance newtypeSchemaJsonDocument :: Newtype SchemaJsonDocument _


newtype SchemaName = SchemaName String
derive instance newtypeSchemaName :: Newtype SchemaName _


newtype SelectorObjectReference = SelectorObjectReference String
derive instance newtypeSelectorObjectReference :: Newtype SelectorObjectReference _


-- | <p>The object could not be deleted because links still exist. Remove the links and then try the operation again.</p>
newtype StillContainsLinksException = StillContainsLinksException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeStillContainsLinksException :: Newtype StillContainsLinksException _


newtype StringAttributeValue = StringAttributeValue String
derive instance newtypeStringAttributeValue :: Newtype StringAttributeValue _


-- | <p>The tag structure that contains a tag key and value.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagResourceRequest = TagResourceRequest 
  { "ResourceArn" :: (Arn)
  , "Tags" :: (TagList)
  }
derive instance newtypeTagResourceRequest :: Newtype TagResourceRequest _


newtype TagResourceResponse = TagResourceResponse 
  { 
  }
derive instance newtypeTagResourceResponse :: Newtype TagResourceResponse _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype TagsNumberResults = TagsNumberResults Int
derive instance newtypeTagsNumberResults :: Newtype TagsNumberResults _


-- | <p>Represents the data for a typed attribute. You can set one, and only one, of the elements. Each attribute in an item is a name-value pair. Attributes have a single value.</p>
newtype TypedAttributeValue = TypedAttributeValue 
  { "StringValue" :: NullOrUndefined (StringAttributeValue)
  , "BinaryValue" :: NullOrUndefined (BinaryAttributeValue)
  , "BooleanValue" :: NullOrUndefined (BooleanAttributeValue)
  , "NumberValue" :: NullOrUndefined (NumberAttributeValue)
  , "DatetimeValue" :: NullOrUndefined (DatetimeAttributeValue)
  }
derive instance newtypeTypedAttributeValue :: Newtype TypedAttributeValue _


-- | <p>A range of attribute values.</p>
newtype TypedAttributeValueRange = TypedAttributeValueRange 
  { "StartMode" :: (RangeMode)
  , "StartValue" :: NullOrUndefined (TypedAttributeValue)
  , "EndMode" :: (RangeMode)
  , "EndValue" :: NullOrUndefined (TypedAttributeValue)
  }
derive instance newtypeTypedAttributeValueRange :: Newtype TypedAttributeValueRange _


-- | <p>A typed link attribute definition.</p>
newtype TypedLinkAttributeDefinition = TypedLinkAttributeDefinition 
  { "Name" :: (AttributeName)
  , "Type" :: (FacetAttributeType)
  , "DefaultValue" :: NullOrUndefined (TypedAttributeValue)
  , "IsImmutable" :: NullOrUndefined (Bool)
  , "Rules" :: NullOrUndefined (RuleMap)
  , "RequiredBehavior" :: (RequiredAttributeBehavior)
  }
derive instance newtypeTypedLinkAttributeDefinition :: Newtype TypedLinkAttributeDefinition _


newtype TypedLinkAttributeDefinitionList = TypedLinkAttributeDefinitionList (Array TypedLinkAttributeDefinition)
derive instance newtypeTypedLinkAttributeDefinitionList :: Newtype TypedLinkAttributeDefinitionList _


-- | <p>Identifies the range of attributes that are used by a specified filter.</p>
newtype TypedLinkAttributeRange = TypedLinkAttributeRange 
  { "AttributeName" :: NullOrUndefined (AttributeName)
  , "Range" :: (TypedAttributeValueRange)
  }
derive instance newtypeTypedLinkAttributeRange :: Newtype TypedLinkAttributeRange _


newtype TypedLinkAttributeRangeList = TypedLinkAttributeRangeList (Array TypedLinkAttributeRange)
derive instance newtypeTypedLinkAttributeRangeList :: Newtype TypedLinkAttributeRangeList _


-- | <p>Defines the typed links structure and its attributes. To create a typed link facet, use the <a>CreateTypedLinkFacet</a> API.</p>
newtype TypedLinkFacet = TypedLinkFacet 
  { "Name" :: (TypedLinkName)
  , "Attributes" :: (TypedLinkAttributeDefinitionList)
  , "IdentityAttributeOrder" :: (AttributeNameList)
  }
derive instance newtypeTypedLinkFacet :: Newtype TypedLinkFacet _


-- | <p>A typed link facet attribute update.</p>
newtype TypedLinkFacetAttributeUpdate = TypedLinkFacetAttributeUpdate 
  { "Attribute" :: (TypedLinkAttributeDefinition)
  , "Action" :: (UpdateActionType)
  }
derive instance newtypeTypedLinkFacetAttributeUpdate :: Newtype TypedLinkFacetAttributeUpdate _


newtype TypedLinkFacetAttributeUpdateList = TypedLinkFacetAttributeUpdateList (Array TypedLinkFacetAttributeUpdate)
derive instance newtypeTypedLinkFacetAttributeUpdateList :: Newtype TypedLinkFacetAttributeUpdateList _


newtype TypedLinkName = TypedLinkName String
derive instance newtypeTypedLinkName :: Newtype TypedLinkName _


newtype TypedLinkNameList = TypedLinkNameList (Array TypedLinkName)
derive instance newtypeTypedLinkNameList :: Newtype TypedLinkNameList _


-- | <p>Identifies the schema Amazon Resource Name (ARN) and facet name for the typed link.</p>
newtype TypedLinkSchemaAndFacetName = TypedLinkSchemaAndFacetName 
  { "SchemaArn" :: (Arn)
  , "TypedLinkName" :: (TypedLinkName)
  }
derive instance newtypeTypedLinkSchemaAndFacetName :: Newtype TypedLinkSchemaAndFacetName _


-- | <p>Contains all the information that is used to uniquely identify a typed link. The parameters discussed in this topic are used to uniquely specify the typed link being operated on. The <a>AttachTypedLink</a> API returns a typed link specifier while the <a>DetachTypedLink</a> API accepts one as input. Similarly, the <a>ListIncomingTypedLinks</a> and <a>ListOutgoingTypedLinks</a> API operations provide typed link specifiers as output. You can also construct a typed link specifier from scratch.</p>
newtype TypedLinkSpecifier = TypedLinkSpecifier 
  { "TypedLinkFacet" :: (TypedLinkSchemaAndFacetName)
  , "SourceObjectReference" :: (ObjectReference)
  , "TargetObjectReference" :: (ObjectReference)
  , "IdentityAttributeValues" :: (AttributeNameAndValueList)
  }
derive instance newtypeTypedLinkSpecifier :: Newtype TypedLinkSpecifier _


newtype TypedLinkSpecifierList = TypedLinkSpecifierList (Array TypedLinkSpecifier)
derive instance newtypeTypedLinkSpecifierList :: Newtype TypedLinkSpecifierList _


-- | <p>Indicates that the requested index type is not supported.</p>
newtype UnsupportedIndexTypeException = UnsupportedIndexTypeException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeUnsupportedIndexTypeException :: Newtype UnsupportedIndexTypeException _


newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceArn" :: (Arn)
  , "TagKeys" :: (TagKeyList)
  }
derive instance newtypeUntagResourceRequest :: Newtype UntagResourceRequest _


newtype UntagResourceResponse = UntagResourceResponse 
  { 
  }
derive instance newtypeUntagResourceResponse :: Newtype UntagResourceResponse _


newtype UpdateActionType = UpdateActionType String
derive instance newtypeUpdateActionType :: Newtype UpdateActionType _


newtype UpdateFacetRequest = UpdateFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  , "AttributeUpdates" :: NullOrUndefined (FacetAttributeUpdateList)
  , "ObjectType" :: NullOrUndefined (ObjectType)
  }
derive instance newtypeUpdateFacetRequest :: Newtype UpdateFacetRequest _


newtype UpdateFacetResponse = UpdateFacetResponse 
  { 
  }
derive instance newtypeUpdateFacetResponse :: Newtype UpdateFacetResponse _


newtype UpdateObjectAttributesRequest = UpdateObjectAttributesRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "AttributeUpdates" :: (ObjectAttributeUpdateList)
  }
derive instance newtypeUpdateObjectAttributesRequest :: Newtype UpdateObjectAttributesRequest _


newtype UpdateObjectAttributesResponse = UpdateObjectAttributesResponse 
  { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeUpdateObjectAttributesResponse :: Newtype UpdateObjectAttributesResponse _


newtype UpdateSchemaRequest = UpdateSchemaRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (SchemaName)
  }
derive instance newtypeUpdateSchemaRequest :: Newtype UpdateSchemaRequest _


newtype UpdateSchemaResponse = UpdateSchemaResponse 
  { "SchemaArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeUpdateSchemaResponse :: Newtype UpdateSchemaResponse _


newtype UpdateTypedLinkFacetRequest = UpdateTypedLinkFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (TypedLinkName)
  , "AttributeUpdates" :: (TypedLinkFacetAttributeUpdateList)
  , "IdentityAttributeOrder" :: (AttributeNameList)
  }
derive instance newtypeUpdateTypedLinkFacetRequest :: Newtype UpdateTypedLinkFacetRequest _


newtype UpdateTypedLinkFacetResponse = UpdateTypedLinkFacetResponse 
  { 
  }
derive instance newtypeUpdateTypedLinkFacetResponse :: Newtype UpdateTypedLinkFacetResponse _


newtype UpgradeAppliedSchemaRequest = UpgradeAppliedSchemaRequest 
  { "PublishedSchemaArn" :: (Arn)
  , "DirectoryArn" :: (Arn)
  , "DryRun" :: NullOrUndefined (Bool)
  }
derive instance newtypeUpgradeAppliedSchemaRequest :: Newtype UpgradeAppliedSchemaRequest _


newtype UpgradeAppliedSchemaResponse = UpgradeAppliedSchemaResponse 
  { "UpgradedSchemaArn" :: NullOrUndefined (Arn)
  , "DirectoryArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeUpgradeAppliedSchemaResponse :: Newtype UpgradeAppliedSchemaResponse _


newtype UpgradePublishedSchemaRequest = UpgradePublishedSchemaRequest 
  { "DevelopmentSchemaArn" :: (Arn)
  , "PublishedSchemaArn" :: (Arn)
  , "MinorVersion" :: (Version)
  , "DryRun" :: NullOrUndefined (Bool)
  }
derive instance newtypeUpgradePublishedSchemaRequest :: Newtype UpgradePublishedSchemaRequest _


newtype UpgradePublishedSchemaResponse = UpgradePublishedSchemaResponse 
  { "UpgradedSchemaArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeUpgradePublishedSchemaResponse :: Newtype UpgradePublishedSchemaResponse _


-- | <p>Indicates that your request is malformed in some manner. See the exception message.</p>
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeValidationException :: Newtype ValidationException _


newtype Version = Version String
derive instance newtypeVersion :: Newtype Version _
