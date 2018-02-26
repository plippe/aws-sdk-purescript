## Module AWS.CloudDirectory

<fullname>Amazon Cloud Directory</fullname> <p>Amazon Cloud Directory is a component of the AWS Directory Service that simplifies the development and management of cloud-scale web, mobile, and IoT applications. This guide describes the Cloud Directory operations that you can call programmatically and includes detailed information on data types and errors. For information about AWS Directory Services features, see <a href="https://aws.amazon.com/directoryservice/">AWS Directory Service</a> and the <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html">AWS Directory Service Administration Guide</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addFacetToObject`

``` purescript
addFacetToObject :: forall eff. AddFacetToObjectRequest -> Aff (err :: RequestError | eff) AddFacetToObjectResponse
```

<p>Adds a new <a>Facet</a> to an object.</p>

#### `applySchema`

``` purescript
applySchema :: forall eff. ApplySchemaRequest -> Aff (err :: RequestError | eff) ApplySchemaResponse
```

<p>Copies the input published schema, at the specified version, into the <a>Directory</a> with the same name and version as that of the published schema.</p>

#### `attachObject`

``` purescript
attachObject :: forall eff. AttachObjectRequest -> Aff (err :: RequestError | eff) AttachObjectResponse
```

<p>Attaches an existing object to another object. An object can be accessed in two ways:</p> <ol> <li> <p>Using the path</p> </li> <li> <p>Using <code>ObjectIdentifier</code> </p> </li> </ol>

#### `attachPolicy`

``` purescript
attachPolicy :: forall eff. AttachPolicyRequest -> Aff (err :: RequestError | eff) AttachPolicyResponse
```

<p>Attaches a policy object to a regular object. An object can have a limited number of attached policies.</p>

#### `attachToIndex`

``` purescript
attachToIndex :: forall eff. AttachToIndexRequest -> Aff (err :: RequestError | eff) AttachToIndexResponse
```

<p>Attaches the specified object to the specified index.</p>

#### `attachTypedLink`

``` purescript
attachTypedLink :: forall eff. AttachTypedLinkRequest -> Aff (err :: RequestError | eff) AttachTypedLinkResponse
```

<p>Attaches a typed link to a specified source and target object. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>

#### `batchRead`

``` purescript
batchRead :: forall eff. BatchReadRequest -> Aff (err :: RequestError | eff) BatchReadResponse
```

<p>Performs all the read operations in a batch. </p>

#### `batchWrite`

``` purescript
batchWrite :: forall eff. BatchWriteRequest -> Aff (err :: RequestError | eff) BatchWriteResponse
```

<p>Performs all the write operations in a batch. Either all the operations succeed or none.</p>

#### `createDirectory`

``` purescript
createDirectory :: forall eff. CreateDirectoryRequest -> Aff (err :: RequestError | eff) CreateDirectoryResponse
```

<p>Creates a <a>Directory</a> by copying the published schema into the directory. A directory cannot be created without a schema.</p>

#### `createFacet`

``` purescript
createFacet :: forall eff. CreateFacetRequest -> Aff (err :: RequestError | eff) CreateFacetResponse
```

<p>Creates a new <a>Facet</a> in a schema. Facet creation is allowed only in development or applied schemas.</p>

#### `createIndex`

``` purescript
createIndex :: forall eff. CreateIndexRequest -> Aff (err :: RequestError | eff) CreateIndexResponse
```

<p>Creates an index object. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_indexing.html">Indexing</a> for more information.</p>

#### `createObject`

``` purescript
createObject :: forall eff. CreateObjectRequest -> Aff (err :: RequestError | eff) CreateObjectResponse
```

<p>Creates an object in a <a>Directory</a>. Additionally attaches the object to a parent, if a parent reference and <code>LinkName</code> is specified. An object is simply a collection of <a>Facet</a> attributes. You can also use this API call to create a policy object, if the facet from which you create the object is a policy facet. </p>

#### `createSchema`

``` purescript
createSchema :: forall eff. CreateSchemaRequest -> Aff (err :: RequestError | eff) CreateSchemaResponse
```

<p>Creates a new schema in a development state. A schema can exist in three phases:</p> <ul> <li> <p> <i>Development:</i> This is a mutable phase of the schema. All new schemas are in the development phase. Once the schema is finalized, it can be published.</p> </li> <li> <p> <i>Published:</i> Published schemas are immutable and have a version associated with them.</p> </li> <li> <p> <i>Applied:</i> Applied schemas are mutable in a way that allows you to add new schema facets. You can also add new, nonrequired attributes to existing schema facets. You can apply only published schemas to directories. </p> </li> </ul>

#### `createTypedLinkFacet`

``` purescript
createTypedLinkFacet :: forall eff. CreateTypedLinkFacetRequest -> Aff (err :: RequestError | eff) CreateTypedLinkFacetResponse
```

<p>Creates a <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>

#### `deleteDirectory`

``` purescript
deleteDirectory :: forall eff. DeleteDirectoryRequest -> Aff (err :: RequestError | eff) DeleteDirectoryResponse
```

<p>Deletes a directory. Only disabled directories can be deleted. A deleted directory cannot be undone. Exercise extreme caution when deleting directories.</p>

#### `deleteFacet`

``` purescript
deleteFacet :: forall eff. DeleteFacetRequest -> Aff (err :: RequestError | eff) DeleteFacetResponse
```

<p>Deletes a given <a>Facet</a>. All attributes and <a>Rule</a>s that are associated with the facet will be deleted. Only development schema facets are allowed deletion.</p>

#### `deleteObject`

``` purescript
deleteObject :: forall eff. DeleteObjectRequest -> Aff (err :: RequestError | eff) DeleteObjectResponse
```

<p>Deletes an object and its associated attributes. Only objects with no children and no parents can be deleted.</p>

#### `deleteSchema`

``` purescript
deleteSchema :: forall eff. DeleteSchemaRequest -> Aff (err :: RequestError | eff) DeleteSchemaResponse
```

<p>Deletes a given schema. Schemas in a development and published state can only be deleted. </p>

#### `deleteTypedLinkFacet`

``` purescript
deleteTypedLinkFacet :: forall eff. DeleteTypedLinkFacetRequest -> Aff (err :: RequestError | eff) DeleteTypedLinkFacetResponse
```

<p>Deletes a <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>

#### `detachFromIndex`

``` purescript
detachFromIndex :: forall eff. DetachFromIndexRequest -> Aff (err :: RequestError | eff) DetachFromIndexResponse
```

<p>Detaches the specified object from the specified index.</p>

#### `detachObject`

``` purescript
detachObject :: forall eff. DetachObjectRequest -> Aff (err :: RequestError | eff) DetachObjectResponse
```

<p>Detaches a given object from the parent object. The object that is to be detached from the parent is specified by the link name.</p>

#### `detachPolicy`

``` purescript
detachPolicy :: forall eff. DetachPolicyRequest -> Aff (err :: RequestError | eff) DetachPolicyResponse
```

<p>Detaches a policy from an object.</p>

#### `detachTypedLink`

``` purescript
detachTypedLink :: forall eff. DetachTypedLinkRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Detaches a typed link from a specified source and target object. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>

#### `disableDirectory`

``` purescript
disableDirectory :: forall eff. DisableDirectoryRequest -> Aff (err :: RequestError | eff) DisableDirectoryResponse
```

<p>Disables the specified directory. Disabled directories cannot be read or written to. Only enabled directories can be disabled. Disabled directories may be reenabled.</p>

#### `enableDirectory`

``` purescript
enableDirectory :: forall eff. EnableDirectoryRequest -> Aff (err :: RequestError | eff) EnableDirectoryResponse
```

<p>Enables the specified directory. Only disabled directories can be enabled. Once enabled, the directory can then be read and written to.</p>

#### `getAppliedSchemaVersion`

``` purescript
getAppliedSchemaVersion :: forall eff. GetAppliedSchemaVersionRequest -> Aff (err :: RequestError | eff) GetAppliedSchemaVersionResponse
```

<p>Returns current applied schema version ARN, including the minor version in use.</p>

#### `getDirectory`

``` purescript
getDirectory :: forall eff. GetDirectoryRequest -> Aff (err :: RequestError | eff) GetDirectoryResponse
```

<p>Retrieves metadata about a directory.</p>

#### `getFacet`

``` purescript
getFacet :: forall eff. GetFacetRequest -> Aff (err :: RequestError | eff) GetFacetResponse
```

<p>Gets details of the <a>Facet</a>, such as facet name, attributes, <a>Rule</a>s, or <code>ObjectType</code>. You can call this on all kinds of schema facets -- published, development, or applied.</p>

#### `getObjectInformation`

``` purescript
getObjectInformation :: forall eff. GetObjectInformationRequest -> Aff (err :: RequestError | eff) GetObjectInformationResponse
```

<p>Retrieves metadata about an object.</p>

#### `getSchemaAsJson`

``` purescript
getSchemaAsJson :: forall eff. GetSchemaAsJsonRequest -> Aff (err :: RequestError | eff) GetSchemaAsJsonResponse
```

<p>Retrieves a JSON representation of the schema. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_schemas.html#jsonformat">JSON Schema Format</a> for more information.</p>

#### `getTypedLinkFacetInformation`

``` purescript
getTypedLinkFacetInformation :: forall eff. GetTypedLinkFacetInformationRequest -> Aff (err :: RequestError | eff) GetTypedLinkFacetInformationResponse
```

<p>Returns the identity attribute order for a specific <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>

#### `listAppliedSchemaArns`

``` purescript
listAppliedSchemaArns :: forall eff. ListAppliedSchemaArnsRequest -> Aff (err :: RequestError | eff) ListAppliedSchemaArnsResponse
```

<p>Lists schema major versions applied to a directory. If <code>SchemaArn</code> is provided, lists the minor version.</p>

#### `listAttachedIndices`

``` purescript
listAttachedIndices :: forall eff. ListAttachedIndicesRequest -> Aff (err :: RequestError | eff) ListAttachedIndicesResponse
```

<p>Lists indices attached to the specified object.</p>

#### `listDevelopmentSchemaArns`

``` purescript
listDevelopmentSchemaArns :: forall eff. ListDevelopmentSchemaArnsRequest -> Aff (err :: RequestError | eff) ListDevelopmentSchemaArnsResponse
```

<p>Retrieves each Amazon Resource Name (ARN) of schemas in the development state.</p>

#### `listDirectories`

``` purescript
listDirectories :: forall eff. ListDirectoriesRequest -> Aff (err :: RequestError | eff) ListDirectoriesResponse
```

<p>Lists directories created within an account.</p>

#### `listFacetAttributes`

``` purescript
listFacetAttributes :: forall eff. ListFacetAttributesRequest -> Aff (err :: RequestError | eff) ListFacetAttributesResponse
```

<p>Retrieves attributes attached to the facet.</p>

#### `listFacetNames`

``` purescript
listFacetNames :: forall eff. ListFacetNamesRequest -> Aff (err :: RequestError | eff) ListFacetNamesResponse
```

<p>Retrieves the names of facets that exist in a schema.</p>

#### `listIncomingTypedLinks`

``` purescript
listIncomingTypedLinks :: forall eff. ListIncomingTypedLinksRequest -> Aff (err :: RequestError | eff) ListIncomingTypedLinksResponse
```

<p>Returns a paginated list of all the incoming <a>TypedLinkSpecifier</a> information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>

#### `listIndex`

``` purescript
listIndex :: forall eff. ListIndexRequest -> Aff (err :: RequestError | eff) ListIndexResponse
```

<p>Lists objects and indexed values attached to the index.</p>

#### `listObjectAttributes`

``` purescript
listObjectAttributes :: forall eff. ListObjectAttributesRequest -> Aff (err :: RequestError | eff) ListObjectAttributesResponse
```

<p>Lists all attributes that are associated with an object. </p>

#### `listObjectChildren`

``` purescript
listObjectChildren :: forall eff. ListObjectChildrenRequest -> Aff (err :: RequestError | eff) ListObjectChildrenResponse
```

<p>Returns a paginated list of child objects that are associated with a given object.</p>

#### `listObjectParentPaths`

``` purescript
listObjectParentPaths :: forall eff. ListObjectParentPathsRequest -> Aff (err :: RequestError | eff) ListObjectParentPathsResponse
```

<p>Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#dirstructure">Directory Structure</a>.</p> <p>Use this API to evaluate all parents for an object. The call returns all objects from the root of the directory up to the requested object. The API returns the number of paths based on user-defined <code>MaxResults</code>, in case there are multiple paths to the parent. The order of the paths and nodes returned is consistent among multiple API calls unless the objects are deleted or moved. Paths not leading to the directory root are ignored from the target object.</p>

#### `listObjectParents`

``` purescript
listObjectParents :: forall eff. ListObjectParentsRequest -> Aff (err :: RequestError | eff) ListObjectParentsResponse
```

<p>Lists parent objects that are associated with a given object in pagination fashion.</p>

#### `listObjectPolicies`

``` purescript
listObjectPolicies :: forall eff. ListObjectPoliciesRequest -> Aff (err :: RequestError | eff) ListObjectPoliciesResponse
```

<p>Returns policies attached to an object in pagination fashion.</p>

#### `listOutgoingTypedLinks`

``` purescript
listOutgoingTypedLinks :: forall eff. ListOutgoingTypedLinksRequest -> Aff (err :: RequestError | eff) ListOutgoingTypedLinksResponse
```

<p>Returns a paginated list of all the outgoing <a>TypedLinkSpecifier</a> information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>

#### `listPolicyAttachments`

``` purescript
listPolicyAttachments :: forall eff. ListPolicyAttachmentsRequest -> Aff (err :: RequestError | eff) ListPolicyAttachmentsResponse
```

<p>Returns all of the <code>ObjectIdentifiers</code> to which a given policy is attached.</p>

#### `listPublishedSchemaArns`

``` purescript
listPublishedSchemaArns :: forall eff. ListPublishedSchemaArnsRequest -> Aff (err :: RequestError | eff) ListPublishedSchemaArnsResponse
```

<p>Lists schema major versions for a published schema. If <code>SchemaArn</code> is provided, lists the minor version.</p>

#### `listTagsForResource`

``` purescript
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: RequestError | eff) ListTagsForResourceResponse
```

<p>Returns tags for a resource. Tagging is currently supported only for directories with a limit of 50 tags per directory. All 50 tags are returned for a given directory with this API call.</p>

#### `listTypedLinkFacetAttributes`

``` purescript
listTypedLinkFacetAttributes :: forall eff. ListTypedLinkFacetAttributesRequest -> Aff (err :: RequestError | eff) ListTypedLinkFacetAttributesResponse
```

<p>Returns a paginated list of all attribute definitions for a particular <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>

#### `listTypedLinkFacetNames`

``` purescript
listTypedLinkFacetNames :: forall eff. ListTypedLinkFacetNamesRequest -> Aff (err :: RequestError | eff) ListTypedLinkFacetNamesResponse
```

<p>Returns a paginated list of <code>TypedLink</code> facet names for a particular schema. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>

#### `lookupPolicy`

``` purescript
lookupPolicy :: forall eff. LookupPolicyRequest -> Aff (err :: RequestError | eff) LookupPolicyResponse
```

<p>Lists all policies from the root of the <a>Directory</a> to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the <code>ObjectIdentifier</code> for such objects. If policies are present, it returns <code>ObjectIdentifier</code>, <code>policyId</code>, and <code>policyType</code>. Paths that don't lead to the root from the target object are ignored. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies">Policies</a>.</p>

#### `publishSchema`

``` purescript
publishSchema :: forall eff. PublishSchemaRequest -> Aff (err :: RequestError | eff) PublishSchemaResponse
```

<p>Publishes a development schema with a major version and a recommended minor version.</p>

#### `putSchemaFromJson`

``` purescript
putSchemaFromJson :: forall eff. PutSchemaFromJsonRequest -> Aff (err :: RequestError | eff) PutSchemaFromJsonResponse
```

<p>Allows a schema to be updated using JSON upload. Only available for development schemas. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_schemas.html#jsonformat">JSON Schema Format</a> for more information.</p>

#### `removeFacetFromObject`

``` purescript
removeFacetFromObject :: forall eff. RemoveFacetFromObjectRequest -> Aff (err :: RequestError | eff) RemoveFacetFromObjectResponse
```

<p>Removes the specified facet from the specified object.</p>

#### `tagResource`

``` purescript
tagResource :: forall eff. TagResourceRequest -> Aff (err :: RequestError | eff) TagResourceResponse
```

<p>An API operation for adding tags to a resource.</p>

#### `untagResource`

``` purescript
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: RequestError | eff) UntagResourceResponse
```

<p>An API operation for removing tags from a resource.</p>

#### `updateFacet`

``` purescript
updateFacet :: forall eff. UpdateFacetRequest -> Aff (err :: RequestError | eff) UpdateFacetResponse
```

<p>Does the following:</p> <ol> <li> <p>Adds new <code>Attributes</code>, <code>Rules</code>, or <code>ObjectTypes</code>.</p> </li> <li> <p>Updates existing <code>Attributes</code>, <code>Rules</code>, or <code>ObjectTypes</code>.</p> </li> <li> <p>Deletes existing <code>Attributes</code>, <code>Rules</code>, or <code>ObjectTypes</code>.</p> </li> </ol>

#### `updateObjectAttributes`

``` purescript
updateObjectAttributes :: forall eff. UpdateObjectAttributesRequest -> Aff (err :: RequestError | eff) UpdateObjectAttributesResponse
```

<p>Updates a given object's attributes.</p>

#### `updateSchema`

``` purescript
updateSchema :: forall eff. UpdateSchemaRequest -> Aff (err :: RequestError | eff) UpdateSchemaResponse
```

<p>Updates the schema name with a new name. Only development schema names can be updated.</p>

#### `updateTypedLinkFacet`

``` purescript
updateTypedLinkFacet :: forall eff. UpdateTypedLinkFacetRequest -> Aff (err :: RequestError | eff) UpdateTypedLinkFacetResponse
```

<p>Updates a <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>

#### `upgradeAppliedSchema`

``` purescript
upgradeAppliedSchema :: forall eff. UpgradeAppliedSchemaRequest -> Aff (err :: RequestError | eff) UpgradeAppliedSchemaResponse
```

<p>Upgrades a single directory in-place using the <code>PublishedSchemaArn</code> with schema updates found in <code>MinorVersion</code>. Backwards-compatible minor version upgrades are instantaneously available for readers on all objects in the directory. Note: This is a synchronous API call and upgrades only one schema on a given directory per call. To upgrade multiple directories from one schema, you would need to call this API on each directory.</p>

#### `upgradePublishedSchema`

``` purescript
upgradePublishedSchema :: forall eff. UpgradePublishedSchemaRequest -> Aff (err :: RequestError | eff) UpgradePublishedSchemaResponse
```

<p>Upgrades a published schema under a new minor version revision using the current contents of <code>DevelopmentSchemaArn</code>.</p>

#### `AccessDeniedException`

``` purescript
newtype AccessDeniedException
  = AccessDeniedException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Access denied. Check your permissions.</p>

#### `AddFacetToObjectRequest`

``` purescript
newtype AddFacetToObjectRequest
  = AddFacetToObjectRequest { "DirectoryArn" :: Arn, "SchemaFacet" :: SchemaFacet, "ObjectAttributeList" :: NullOrUndefined (AttributeKeyAndValueList), "ObjectReference" :: ObjectReference }
```

#### `AddFacetToObjectResponse`

``` purescript
newtype AddFacetToObjectResponse
  = AddFacetToObjectResponse {  }
```

#### `ApplySchemaRequest`

``` purescript
newtype ApplySchemaRequest
  = ApplySchemaRequest { "PublishedSchemaArn" :: Arn, "DirectoryArn" :: Arn }
```

#### `ApplySchemaResponse`

``` purescript
newtype ApplySchemaResponse
  = ApplySchemaResponse { "AppliedSchemaArn" :: NullOrUndefined (Arn), "DirectoryArn" :: NullOrUndefined (Arn) }
```

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

#### `Arns`

``` purescript
newtype Arns
  = Arns (Array Arn)
```

#### `AttachObjectRequest`

``` purescript
newtype AttachObjectRequest
  = AttachObjectRequest { "DirectoryArn" :: Arn, "ParentReference" :: ObjectReference, "ChildReference" :: ObjectReference, "LinkName" :: LinkName }
```

#### `AttachObjectResponse`

``` purescript
newtype AttachObjectResponse
  = AttachObjectResponse { "AttachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

#### `AttachPolicyRequest`

``` purescript
newtype AttachPolicyRequest
  = AttachPolicyRequest { "DirectoryArn" :: NullOrUndefined (Arn), "PolicyReference" :: ObjectReference, "ObjectReference" :: ObjectReference }
```

#### `AttachPolicyResponse`

``` purescript
newtype AttachPolicyResponse
  = AttachPolicyResponse {  }
```

#### `AttachToIndexRequest`

``` purescript
newtype AttachToIndexRequest
  = AttachToIndexRequest { "DirectoryArn" :: Arn, "IndexReference" :: ObjectReference, "TargetReference" :: ObjectReference }
```

#### `AttachToIndexResponse`

``` purescript
newtype AttachToIndexResponse
  = AttachToIndexResponse { "AttachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

#### `AttachTypedLinkRequest`

``` purescript
newtype AttachTypedLinkRequest
  = AttachTypedLinkRequest { "DirectoryArn" :: Arn, "SourceObjectReference" :: ObjectReference, "TargetObjectReference" :: ObjectReference, "TypedLinkFacet" :: TypedLinkSchemaAndFacetName, "Attributes" :: AttributeNameAndValueList }
```

#### `AttachTypedLinkResponse`

``` purescript
newtype AttachTypedLinkResponse
  = AttachTypedLinkResponse { "TypedLinkSpecifier" :: NullOrUndefined (TypedLinkSpecifier) }
```

#### `AttributeKey`

``` purescript
newtype AttributeKey
  = AttributeKey { "SchemaArn" :: Arn, "FacetName" :: FacetName, "Name" :: AttributeName }
```

<p>A unique identifier for an attribute.</p>

#### `AttributeKeyAndValue`

``` purescript
newtype AttributeKeyAndValue
  = AttributeKeyAndValue { "Key" :: AttributeKey, "Value" :: TypedAttributeValue }
```

<p>The combination of an attribute key and an attribute value.</p>

#### `AttributeKeyAndValueList`

``` purescript
newtype AttributeKeyAndValueList
  = AttributeKeyAndValueList (Array AttributeKeyAndValue)
```

#### `AttributeKeyList`

``` purescript
newtype AttributeKeyList
  = AttributeKeyList (Array AttributeKey)
```

#### `AttributeName`

``` purescript
newtype AttributeName
  = AttributeName String
```

#### `AttributeNameAndValue`

``` purescript
newtype AttributeNameAndValue
  = AttributeNameAndValue { "AttributeName" :: AttributeName, "Value" :: TypedAttributeValue }
```

<p>Identifies the attribute name and value for a typed link.</p>

#### `AttributeNameAndValueList`

``` purescript
newtype AttributeNameAndValueList
  = AttributeNameAndValueList (Array AttributeNameAndValue)
```

#### `AttributeNameList`

``` purescript
newtype AttributeNameList
  = AttributeNameList (Array AttributeName)
```

#### `BatchAddFacetToObject`

``` purescript
newtype BatchAddFacetToObject
  = BatchAddFacetToObject { "SchemaFacet" :: SchemaFacet, "ObjectAttributeList" :: AttributeKeyAndValueList, "ObjectReference" :: ObjectReference }
```

<p>Represents the output of a batch add facet to object operation.</p>

#### `BatchAddFacetToObjectResponse`

``` purescript
newtype BatchAddFacetToObjectResponse
  = BatchAddFacetToObjectResponse {  }
```

<p>The result of a batch add facet to object operation.</p>

#### `BatchAttachObject`

``` purescript
newtype BatchAttachObject
  = BatchAttachObject { "ParentReference" :: ObjectReference, "ChildReference" :: ObjectReference, "LinkName" :: LinkName }
```

<p>Represents the output of an <a>AttachObject</a> operation.</p>

#### `BatchAttachObjectResponse`

``` purescript
newtype BatchAttachObjectResponse
  = BatchAttachObjectResponse { "AttachedObjectIdentifier'" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output batch <a>AttachObject</a> response operation.</p>

#### `BatchAttachPolicy`

``` purescript
newtype BatchAttachPolicy
  = BatchAttachPolicy { "PolicyReference" :: ObjectReference, "ObjectReference" :: ObjectReference }
```

<p>Attaches a policy object to a regular object inside a <a>BatchRead</a> operation. For more information, see <a>AttachPolicy</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchAttachPolicyResponse`

``` purescript
newtype BatchAttachPolicyResponse
  = BatchAttachPolicyResponse {  }
```

<p>Represents the output of an <a>AttachPolicy</a> response operation.</p>

#### `BatchAttachToIndex`

``` purescript
newtype BatchAttachToIndex
  = BatchAttachToIndex { "IndexReference" :: ObjectReference, "TargetReference" :: ObjectReference }
```

<p>Attaches the specified object to the specified index inside a <a>BatchRead</a> operation. For more information, see <a>AttachToIndex</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchAttachToIndexResponse`

``` purescript
newtype BatchAttachToIndexResponse
  = BatchAttachToIndexResponse { "AttachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <a>AttachToIndex</a> response operation.</p>

#### `BatchAttachTypedLink`

``` purescript
newtype BatchAttachTypedLink
  = BatchAttachTypedLink { "SourceObjectReference" :: ObjectReference, "TargetObjectReference" :: ObjectReference, "TypedLinkFacet" :: TypedLinkSchemaAndFacetName, "Attributes" :: AttributeNameAndValueList }
```

<p>Attaches a typed link to a specified source and target object inside a <a>BatchRead</a> operation. For more information, see <a>AttachTypedLink</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchAttachTypedLinkResponse`

``` purescript
newtype BatchAttachTypedLinkResponse
  = BatchAttachTypedLinkResponse { "TypedLinkSpecifier" :: NullOrUndefined (TypedLinkSpecifier) }
```

<p>Represents the output of a <a>AttachTypedLink</a> response operation.</p>

#### `BatchCreateIndex`

``` purescript
newtype BatchCreateIndex
  = BatchCreateIndex { "OrderedIndexedAttributeList" :: AttributeKeyList, "IsUnique" :: Bool, "ParentReference" :: NullOrUndefined (ObjectReference), "LinkName" :: NullOrUndefined (LinkName), "BatchReferenceName" :: NullOrUndefined (BatchReferenceName) }
```

<p>Creates an index object inside of a <a>BatchRead</a> operation. For more information, see <a>CreateIndex</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchCreateIndexResponse`

``` purescript
newtype BatchCreateIndexResponse
  = BatchCreateIndexResponse { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <a>CreateIndex</a> response operation.</p>

#### `BatchCreateObject`

``` purescript
newtype BatchCreateObject
  = BatchCreateObject { "SchemaFacet" :: SchemaFacetList, "ObjectAttributeList" :: AttributeKeyAndValueList, "ParentReference" :: ObjectReference, "LinkName" :: LinkName, "BatchReferenceName" :: BatchReferenceName }
```

<p>Represents the output of a <a>CreateObject</a> operation.</p>

#### `BatchCreateObjectResponse`

``` purescript
newtype BatchCreateObjectResponse
  = BatchCreateObjectResponse { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <a>CreateObject</a> response operation.</p>

#### `BatchDeleteObject`

``` purescript
newtype BatchDeleteObject
  = BatchDeleteObject { "ObjectReference" :: ObjectReference }
```

<p>Represents the output of a <a>DeleteObject</a> operation.</p>

#### `BatchDeleteObjectResponse`

``` purescript
newtype BatchDeleteObjectResponse
  = BatchDeleteObjectResponse {  }
```

<p>Represents the output of a <a>DeleteObject</a> response operation.</p>

#### `BatchDetachFromIndex`

``` purescript
newtype BatchDetachFromIndex
  = BatchDetachFromIndex { "IndexReference" :: ObjectReference, "TargetReference" :: ObjectReference }
```

<p>Detaches the specified object from the specified index inside a <a>BatchRead</a> operation. For more information, see <a>DetachFromIndex</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchDetachFromIndexResponse`

``` purescript
newtype BatchDetachFromIndexResponse
  = BatchDetachFromIndexResponse { "DetachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <a>DetachFromIndex</a> response operation.</p>

#### `BatchDetachObject`

``` purescript
newtype BatchDetachObject
  = BatchDetachObject { "ParentReference" :: ObjectReference, "LinkName" :: LinkName, "BatchReferenceName" :: BatchReferenceName }
```

<p>Represents the output of a <a>DetachObject</a> operation.</p>

#### `BatchDetachObjectResponse`

``` purescript
newtype BatchDetachObjectResponse
  = BatchDetachObjectResponse { "DetachedObjectIdentifier'" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <a>DetachObject</a> response operation.</p>

#### `BatchDetachPolicy`

``` purescript
newtype BatchDetachPolicy
  = BatchDetachPolicy { "PolicyReference" :: ObjectReference, "ObjectReference" :: ObjectReference }
```

<p>Detaches the specified policy from the specified directory inside a <a>BatchWrite</a> operation. For more information, see <a>DetachPolicy</a> and <a>BatchWriteRequest$Operations</a>.</p>

#### `BatchDetachPolicyResponse`

``` purescript
newtype BatchDetachPolicyResponse
  = BatchDetachPolicyResponse {  }
```

<p>Represents the output of a <a>DetachPolicy</a> response operation.</p>

#### `BatchDetachTypedLink`

``` purescript
newtype BatchDetachTypedLink
  = BatchDetachTypedLink { "TypedLinkSpecifier" :: TypedLinkSpecifier }
```

<p>Detaches a typed link from a specified source and target object inside a <a>BatchRead</a> operation. For more information, see <a>DetachTypedLink</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchDetachTypedLinkResponse`

``` purescript
newtype BatchDetachTypedLinkResponse
  = BatchDetachTypedLinkResponse {  }
```

<p>Represents the output of a <a>DetachTypedLink</a> response operation.</p>

#### `BatchGetObjectInformation`

``` purescript
newtype BatchGetObjectInformation
  = BatchGetObjectInformation { "ObjectReference" :: ObjectReference }
```

<p>Retrieves metadata about an object inside a <a>BatchRead</a> operation. For more information, see <a>GetObjectInformation</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchGetObjectInformationResponse`

``` purescript
newtype BatchGetObjectInformationResponse
  = BatchGetObjectInformationResponse { "SchemaFacets" :: NullOrUndefined (SchemaFacetList), "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <a>GetObjectInformation</a> response operation.</p>

#### `BatchListAttachedIndices`

``` purescript
newtype BatchListAttachedIndices
  = BatchListAttachedIndices { "TargetReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Lists indices attached to an object inside a <a>BatchRead</a> operation. For more information, see <a>ListAttachedIndices</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchListAttachedIndicesResponse`

``` purescript
newtype BatchListAttachedIndicesResponse
  = BatchListAttachedIndicesResponse { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListAttachedIndices</a> response operation.</p>

#### `BatchListIncomingTypedLinks`

``` purescript
newtype BatchListIncomingTypedLinks
  = BatchListIncomingTypedLinks { "ObjectReference" :: ObjectReference, "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList), "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Returns a paginated list of all the incoming <a>TypedLinkSpecifier</a> information for an object inside a <a>BatchRead</a> operation. For more information, see <a>ListIncomingTypedLinks</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchListIncomingTypedLinksResponse`

``` purescript
newtype BatchListIncomingTypedLinksResponse
  = BatchListIncomingTypedLinksResponse { "LinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListIncomingTypedLinks</a> response operation.</p>

#### `BatchListIndex`

``` purescript
newtype BatchListIndex
  = BatchListIndex { "RangesOnIndexedValues" :: NullOrUndefined (ObjectAttributeRangeList), "IndexReference" :: ObjectReference, "MaxResults" :: NullOrUndefined (NumberResults), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Lists objects attached to the specified index inside a <a>BatchRead</a> operation. For more information, see <a>ListIndex</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchListIndexResponse`

``` purescript
newtype BatchListIndexResponse
  = BatchListIndexResponse { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListIndex</a> response operation.</p>

#### `BatchListObjectAttributes`

``` purescript
newtype BatchListObjectAttributes
  = BatchListObjectAttributes { "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "FacetFilter" :: NullOrUndefined (SchemaFacet) }
```

<p>Represents the output of a <a>ListObjectAttributes</a> operation.</p>

#### `BatchListObjectAttributesResponse`

``` purescript
newtype BatchListObjectAttributesResponse
  = BatchListObjectAttributesResponse { "Attributes" :: NullOrUndefined (AttributeKeyAndValueList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListObjectAttributes</a> response operation.</p>

#### `BatchListObjectChildren`

``` purescript
newtype BatchListObjectChildren
  = BatchListObjectChildren { "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Represents the output of a <a>ListObjectChildren</a> operation.</p>

#### `BatchListObjectChildrenResponse`

``` purescript
newtype BatchListObjectChildrenResponse
  = BatchListObjectChildrenResponse { "Children" :: NullOrUndefined (LinkNameToObjectIdentifierMap), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListObjectChildren</a> response operation.</p>

#### `BatchListObjectParentPaths`

``` purescript
newtype BatchListObjectParentPaths
  = BatchListObjectParentPaths { "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects inside a <a>BatchRead</a> operation. For more information, see <a>ListObjectParentPaths</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchListObjectParentPathsResponse`

``` purescript
newtype BatchListObjectParentPathsResponse
  = BatchListObjectParentPathsResponse { "PathToObjectIdentifiersList" :: NullOrUndefined (PathToObjectIdentifiersList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListObjectParentPaths</a> response operation.</p>

#### `BatchListObjectPolicies`

``` purescript
newtype BatchListObjectPolicies
  = BatchListObjectPolicies { "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Returns policies attached to an object in pagination fashion inside a <a>BatchRead</a> operation. For more information, see <a>ListObjectPolicies</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchListObjectPoliciesResponse`

``` purescript
newtype BatchListObjectPoliciesResponse
  = BatchListObjectPoliciesResponse { "AttachedPolicyIds" :: NullOrUndefined (ObjectIdentifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListObjectPolicies</a> response operation.</p>

#### `BatchListOutgoingTypedLinks`

``` purescript
newtype BatchListOutgoingTypedLinks
  = BatchListOutgoingTypedLinks { "ObjectReference" :: ObjectReference, "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList), "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Returns a paginated list of all the outgoing <a>TypedLinkSpecifier</a> information for an object inside a <a>BatchRead</a> operation. For more information, see <a>ListOutgoingTypedLinks</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchListOutgoingTypedLinksResponse`

``` purescript
newtype BatchListOutgoingTypedLinksResponse
  = BatchListOutgoingTypedLinksResponse { "TypedLinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListOutgoingTypedLinks</a> response operation.</p>

#### `BatchListPolicyAttachments`

``` purescript
newtype BatchListPolicyAttachments
  = BatchListPolicyAttachments { "PolicyReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Returns all of the <code>ObjectIdentifiers</code> to which a given policy is attached inside a <a>BatchRead</a> operation. For more information, see <a>ListPolicyAttachments</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchListPolicyAttachmentsResponse`

``` purescript
newtype BatchListPolicyAttachmentsResponse
  = BatchListPolicyAttachmentsResponse { "ObjectIdentifiers" :: NullOrUndefined (ObjectIdentifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListPolicyAttachments</a> response operation.</p>

#### `BatchLookupPolicy`

``` purescript
newtype BatchLookupPolicy
  = BatchLookupPolicy { "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Lists all policies from the root of the Directory to the object specified inside a <a>BatchRead</a> operation. For more information, see <a>LookupPolicy</a> and <a>BatchReadRequest$Operations</a>.</p>

#### `BatchLookupPolicyResponse`

``` purescript
newtype BatchLookupPolicyResponse
  = BatchLookupPolicyResponse { "PolicyToPathList" :: NullOrUndefined (PolicyToPathList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>LookupPolicy</a> response operation.</p>

#### `BatchOperationIndex`

``` purescript
newtype BatchOperationIndex
  = BatchOperationIndex Int
```

#### `BatchReadException`

``` purescript
newtype BatchReadException
  = BatchReadException { "Type" :: NullOrUndefined (BatchReadExceptionType), "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The batch read exception structure, which contains the exception type and message.</p>

#### `BatchReadExceptionType`

``` purescript
newtype BatchReadExceptionType
  = BatchReadExceptionType String
```

#### `BatchReadOperation`

``` purescript
newtype BatchReadOperation
  = BatchReadOperation { "ListObjectAttributes" :: NullOrUndefined (BatchListObjectAttributes), "ListObjectChildren" :: NullOrUndefined (BatchListObjectChildren), "ListAttachedIndices" :: NullOrUndefined (BatchListAttachedIndices), "ListObjectParentPaths" :: NullOrUndefined (BatchListObjectParentPaths), "GetObjectInformation" :: NullOrUndefined (BatchGetObjectInformation), "ListObjectPolicies" :: NullOrUndefined (BatchListObjectPolicies), "ListPolicyAttachments" :: NullOrUndefined (BatchListPolicyAttachments), "LookupPolicy" :: NullOrUndefined (BatchLookupPolicy), "ListIndex" :: NullOrUndefined (BatchListIndex), "ListOutgoingTypedLinks" :: NullOrUndefined (BatchListOutgoingTypedLinks), "ListIncomingTypedLinks" :: NullOrUndefined (BatchListIncomingTypedLinks) }
```

<p>Represents the output of a <code>BatchRead</code> operation.</p>

#### `BatchReadOperationList`

``` purescript
newtype BatchReadOperationList
  = BatchReadOperationList (Array BatchReadOperation)
```

#### `BatchReadOperationResponse`

``` purescript
newtype BatchReadOperationResponse
  = BatchReadOperationResponse { "SuccessfulResponse" :: NullOrUndefined (BatchReadSuccessfulResponse), "ExceptionResponse" :: NullOrUndefined (BatchReadException) }
```

<p>Represents the output of a <code>BatchRead</code> response operation.</p>

#### `BatchReadOperationResponseList`

``` purescript
newtype BatchReadOperationResponseList
  = BatchReadOperationResponseList (Array BatchReadOperationResponse)
```

#### `BatchReadRequest`

``` purescript
newtype BatchReadRequest
  = BatchReadRequest { "DirectoryArn" :: Arn, "Operations" :: BatchReadOperationList, "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

#### `BatchReadResponse`

``` purescript
newtype BatchReadResponse
  = BatchReadResponse { "Responses" :: NullOrUndefined (BatchReadOperationResponseList) }
```

#### `BatchReadSuccessfulResponse`

``` purescript
newtype BatchReadSuccessfulResponse
  = BatchReadSuccessfulResponse { "ListObjectAttributes" :: NullOrUndefined (BatchListObjectAttributesResponse), "ListObjectChildren" :: NullOrUndefined (BatchListObjectChildrenResponse), "GetObjectInformation" :: NullOrUndefined (BatchGetObjectInformationResponse), "ListAttachedIndices" :: NullOrUndefined (BatchListAttachedIndicesResponse), "ListObjectParentPaths" :: NullOrUndefined (BatchListObjectParentPathsResponse), "ListObjectPolicies" :: NullOrUndefined (BatchListObjectPoliciesResponse), "ListPolicyAttachments" :: NullOrUndefined (BatchListPolicyAttachmentsResponse), "LookupPolicy" :: NullOrUndefined (BatchLookupPolicyResponse), "ListIndex" :: NullOrUndefined (BatchListIndexResponse), "ListOutgoingTypedLinks" :: NullOrUndefined (BatchListOutgoingTypedLinksResponse), "ListIncomingTypedLinks" :: NullOrUndefined (BatchListIncomingTypedLinksResponse) }
```

<p>Represents the output of a <code>BatchRead</code> success response operation.</p>

#### `BatchReferenceName`

``` purescript
newtype BatchReferenceName
  = BatchReferenceName String
```

#### `BatchRemoveFacetFromObject`

``` purescript
newtype BatchRemoveFacetFromObject
  = BatchRemoveFacetFromObject { "SchemaFacet" :: SchemaFacet, "ObjectReference" :: ObjectReference }
```

<p>A batch operation to remove a facet from an object.</p>

#### `BatchRemoveFacetFromObjectResponse`

``` purescript
newtype BatchRemoveFacetFromObjectResponse
  = BatchRemoveFacetFromObjectResponse {  }
```

<p>An empty result that represents success.</p>

#### `BatchUpdateObjectAttributes`

``` purescript
newtype BatchUpdateObjectAttributes
  = BatchUpdateObjectAttributes { "ObjectReference" :: ObjectReference, "AttributeUpdates" :: ObjectAttributeUpdateList }
```

<p>Represents the output of a <code>BatchUpdate</code> operation. </p>

#### `BatchUpdateObjectAttributesResponse`

``` purescript
newtype BatchUpdateObjectAttributesResponse
  = BatchUpdateObjectAttributesResponse { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <code>BatchUpdate</code> response operation.</p>

#### `BatchWriteException`

``` purescript
newtype BatchWriteException
  = BatchWriteException { "Index" :: NullOrUndefined (BatchOperationIndex), "Type" :: NullOrUndefined (BatchWriteExceptionType), "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>A <code>BatchWrite</code> exception has occurred.</p>

#### `BatchWriteExceptionType`

``` purescript
newtype BatchWriteExceptionType
  = BatchWriteExceptionType String
```

#### `BatchWriteOperation`

``` purescript
newtype BatchWriteOperation
  = BatchWriteOperation { "CreateObject" :: NullOrUndefined (BatchCreateObject), "AttachObject" :: NullOrUndefined (BatchAttachObject), "DetachObject" :: NullOrUndefined (BatchDetachObject), "UpdateObjectAttributes" :: NullOrUndefined (BatchUpdateObjectAttributes), "DeleteObject" :: NullOrUndefined (BatchDeleteObject), "AddFacetToObject" :: NullOrUndefined (BatchAddFacetToObject), "RemoveFacetFromObject" :: NullOrUndefined (BatchRemoveFacetFromObject), "AttachPolicy" :: NullOrUndefined (BatchAttachPolicy), "DetachPolicy" :: NullOrUndefined (BatchDetachPolicy), "CreateIndex" :: NullOrUndefined (BatchCreateIndex), "AttachToIndex" :: NullOrUndefined (BatchAttachToIndex), "DetachFromIndex" :: NullOrUndefined (BatchDetachFromIndex), "AttachTypedLink" :: NullOrUndefined (BatchAttachTypedLink), "DetachTypedLink" :: NullOrUndefined (BatchDetachTypedLink) }
```

<p>Represents the output of a <code>BatchWrite</code> operation. </p>

#### `BatchWriteOperationList`

``` purescript
newtype BatchWriteOperationList
  = BatchWriteOperationList (Array BatchWriteOperation)
```

#### `BatchWriteOperationResponse`

``` purescript
newtype BatchWriteOperationResponse
  = BatchWriteOperationResponse { "CreateObject" :: NullOrUndefined (BatchCreateObjectResponse), "AttachObject" :: NullOrUndefined (BatchAttachObjectResponse), "DetachObject" :: NullOrUndefined (BatchDetachObjectResponse), "UpdateObjectAttributes" :: NullOrUndefined (BatchUpdateObjectAttributesResponse), "DeleteObject" :: NullOrUndefined (BatchDeleteObjectResponse), "AddFacetToObject" :: NullOrUndefined (BatchAddFacetToObjectResponse), "RemoveFacetFromObject" :: NullOrUndefined (BatchRemoveFacetFromObjectResponse), "AttachPolicy" :: NullOrUndefined (BatchAttachPolicyResponse), "DetachPolicy" :: NullOrUndefined (BatchDetachPolicyResponse), "CreateIndex" :: NullOrUndefined (BatchCreateIndexResponse), "AttachToIndex" :: NullOrUndefined (BatchAttachToIndexResponse), "DetachFromIndex" :: NullOrUndefined (BatchDetachFromIndexResponse), "AttachTypedLink" :: NullOrUndefined (BatchAttachTypedLinkResponse), "DetachTypedLink" :: NullOrUndefined (BatchDetachTypedLinkResponse) }
```

<p>Represents the output of a <code>BatchWrite</code> response operation.</p>

#### `BatchWriteOperationResponseList`

``` purescript
newtype BatchWriteOperationResponseList
  = BatchWriteOperationResponseList (Array BatchWriteOperationResponse)
```

#### `BatchWriteRequest`

``` purescript
newtype BatchWriteRequest
  = BatchWriteRequest { "DirectoryArn" :: Arn, "Operations" :: BatchWriteOperationList }
```

#### `BatchWriteResponse`

``` purescript
newtype BatchWriteResponse
  = BatchWriteResponse { "Responses" :: NullOrUndefined (BatchWriteOperationResponseList) }
```

#### `BinaryAttributeValue`

``` purescript
newtype BinaryAttributeValue
  = BinaryAttributeValue String
```

#### `Bool`

``` purescript
newtype Bool
  = Bool Boolean
```

#### `BooleanAttributeValue`

``` purescript
newtype BooleanAttributeValue
  = BooleanAttributeValue Boolean
```

#### `CannotListParentOfRootException`

``` purescript
newtype CannotListParentOfRootException
  = CannotListParentOfRootException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Cannot list the parents of a <a>Directory</a> root.</p>

#### `ConsistencyLevel`

``` purescript
newtype ConsistencyLevel
  = ConsistencyLevel String
```

#### `CreateDirectoryRequest`

``` purescript
newtype CreateDirectoryRequest
  = CreateDirectoryRequest { "Name" :: DirectoryName, "SchemaArn" :: Arn }
```

#### `CreateDirectoryResponse`

``` purescript
newtype CreateDirectoryResponse
  = CreateDirectoryResponse { "DirectoryArn" :: DirectoryArn, "Name" :: DirectoryName, "ObjectIdentifier" :: ObjectIdentifier, "AppliedSchemaArn" :: Arn }
```

#### `CreateFacetRequest`

``` purescript
newtype CreateFacetRequest
  = CreateFacetRequest { "SchemaArn" :: Arn, "Name" :: FacetName, "Attributes" :: NullOrUndefined (FacetAttributeList), "ObjectType" :: ObjectType }
```

#### `CreateFacetResponse`

``` purescript
newtype CreateFacetResponse
  = CreateFacetResponse {  }
```

#### `CreateIndexRequest`

``` purescript
newtype CreateIndexRequest
  = CreateIndexRequest { "DirectoryArn" :: Arn, "OrderedIndexedAttributeList" :: AttributeKeyList, "IsUnique" :: Bool, "ParentReference" :: NullOrUndefined (ObjectReference), "LinkName" :: NullOrUndefined (LinkName) }
```

#### `CreateIndexResponse`

``` purescript
newtype CreateIndexResponse
  = CreateIndexResponse { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

#### `CreateObjectRequest`

``` purescript
newtype CreateObjectRequest
  = CreateObjectRequest { "DirectoryArn" :: Arn, "SchemaFacets" :: SchemaFacetList, "ObjectAttributeList" :: NullOrUndefined (AttributeKeyAndValueList), "ParentReference" :: NullOrUndefined (ObjectReference), "LinkName" :: NullOrUndefined (LinkName) }
```

#### `CreateObjectResponse`

``` purescript
newtype CreateObjectResponse
  = CreateObjectResponse { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

#### `CreateSchemaRequest`

``` purescript
newtype CreateSchemaRequest
  = CreateSchemaRequest { "Name" :: SchemaName }
```

#### `CreateSchemaResponse`

``` purescript
newtype CreateSchemaResponse
  = CreateSchemaResponse { "SchemaArn" :: NullOrUndefined (Arn) }
```

#### `CreateTypedLinkFacetRequest`

``` purescript
newtype CreateTypedLinkFacetRequest
  = CreateTypedLinkFacetRequest { "SchemaArn" :: Arn, "Facet" :: TypedLinkFacet }
```

#### `CreateTypedLinkFacetResponse`

``` purescript
newtype CreateTypedLinkFacetResponse
  = CreateTypedLinkFacetResponse {  }
```

#### `Date`

``` purescript
newtype Date
  = Date Number
```

#### `DatetimeAttributeValue`

``` purescript
newtype DatetimeAttributeValue
  = DatetimeAttributeValue Number
```

#### `DeleteDirectoryRequest`

``` purescript
newtype DeleteDirectoryRequest
  = DeleteDirectoryRequest { "DirectoryArn" :: Arn }
```

#### `DeleteDirectoryResponse`

``` purescript
newtype DeleteDirectoryResponse
  = DeleteDirectoryResponse { "DirectoryArn" :: Arn }
```

#### `DeleteFacetRequest`

``` purescript
newtype DeleteFacetRequest
  = DeleteFacetRequest { "SchemaArn" :: Arn, "Name" :: FacetName }
```

#### `DeleteFacetResponse`

``` purescript
newtype DeleteFacetResponse
  = DeleteFacetResponse {  }
```

#### `DeleteObjectRequest`

``` purescript
newtype DeleteObjectRequest
  = DeleteObjectRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference }
```

#### `DeleteObjectResponse`

``` purescript
newtype DeleteObjectResponse
  = DeleteObjectResponse {  }
```

#### `DeleteSchemaRequest`

``` purescript
newtype DeleteSchemaRequest
  = DeleteSchemaRequest { "SchemaArn" :: Arn }
```

#### `DeleteSchemaResponse`

``` purescript
newtype DeleteSchemaResponse
  = DeleteSchemaResponse { "SchemaArn" :: NullOrUndefined (Arn) }
```

#### `DeleteTypedLinkFacetRequest`

``` purescript
newtype DeleteTypedLinkFacetRequest
  = DeleteTypedLinkFacetRequest { "SchemaArn" :: Arn, "Name" :: TypedLinkName }
```

#### `DeleteTypedLinkFacetResponse`

``` purescript
newtype DeleteTypedLinkFacetResponse
  = DeleteTypedLinkFacetResponse {  }
```

#### `DetachFromIndexRequest`

``` purescript
newtype DetachFromIndexRequest
  = DetachFromIndexRequest { "DirectoryArn" :: Arn, "IndexReference" :: ObjectReference, "TargetReference" :: ObjectReference }
```

#### `DetachFromIndexResponse`

``` purescript
newtype DetachFromIndexResponse
  = DetachFromIndexResponse { "DetachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

#### `DetachObjectRequest`

``` purescript
newtype DetachObjectRequest
  = DetachObjectRequest { "DirectoryArn" :: Arn, "ParentReference" :: ObjectReference, "LinkName" :: LinkName }
```

#### `DetachObjectResponse`

``` purescript
newtype DetachObjectResponse
  = DetachObjectResponse { "DetachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

#### `DetachPolicyRequest`

``` purescript
newtype DetachPolicyRequest
  = DetachPolicyRequest { "DirectoryArn" :: Arn, "PolicyReference" :: ObjectReference, "ObjectReference" :: ObjectReference }
```

#### `DetachPolicyResponse`

``` purescript
newtype DetachPolicyResponse
  = DetachPolicyResponse {  }
```

#### `DetachTypedLinkRequest`

``` purescript
newtype DetachTypedLinkRequest
  = DetachTypedLinkRequest { "DirectoryArn" :: Arn, "TypedLinkSpecifier" :: TypedLinkSpecifier }
```

#### `Directory`

``` purescript
newtype Directory
  = Directory { "Name" :: NullOrUndefined (DirectoryName), "DirectoryArn" :: NullOrUndefined (DirectoryArn), "State" :: NullOrUndefined (DirectoryState), "CreationDateTime" :: NullOrUndefined (Date) }
```

<p>Directory structure that includes the directory name and directory ARN.</p>

#### `DirectoryAlreadyExistsException`

``` purescript
newtype DirectoryAlreadyExistsException
  = DirectoryAlreadyExistsException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that a <a>Directory</a> could not be created due to a naming conflict. Choose a different name and try again.</p>

#### `DirectoryArn`

``` purescript
newtype DirectoryArn
  = DirectoryArn String
```

#### `DirectoryDeletedException`

``` purescript
newtype DirectoryDeletedException
  = DirectoryDeletedException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>A directory that has been deleted and to which access has been attempted. Note: The requested resource will eventually cease to exist.</p>

#### `DirectoryList`

``` purescript
newtype DirectoryList
  = DirectoryList (Array Directory)
```

#### `DirectoryName`

``` purescript
newtype DirectoryName
  = DirectoryName String
```

#### `DirectoryNotDisabledException`

``` purescript
newtype DirectoryNotDisabledException
  = DirectoryNotDisabledException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>An operation can only operate on a disabled directory.</p>

#### `DirectoryNotEnabledException`

``` purescript
newtype DirectoryNotEnabledException
  = DirectoryNotEnabledException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>An operation can only operate on a directory that is not enabled.</p>

#### `DirectoryState`

``` purescript
newtype DirectoryState
  = DirectoryState String
```

#### `DisableDirectoryRequest`

``` purescript
newtype DisableDirectoryRequest
  = DisableDirectoryRequest { "DirectoryArn" :: Arn }
```

#### `DisableDirectoryResponse`

``` purescript
newtype DisableDirectoryResponse
  = DisableDirectoryResponse { "DirectoryArn" :: Arn }
```

#### `EnableDirectoryRequest`

``` purescript
newtype EnableDirectoryRequest
  = EnableDirectoryRequest { "DirectoryArn" :: Arn }
```

#### `EnableDirectoryResponse`

``` purescript
newtype EnableDirectoryResponse
  = EnableDirectoryResponse { "DirectoryArn" :: Arn }
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

#### `Facet`

``` purescript
newtype Facet
  = Facet { "Name" :: NullOrUndefined (FacetName), "ObjectType" :: NullOrUndefined (ObjectType) }
```

<p>A structure that contains <code>Name</code>, <code>ARN</code>, <code>Attributes</code>, <a>Rule</a>s, and <code>ObjectTypes</code>.</p>

#### `FacetAlreadyExistsException`

``` purescript
newtype FacetAlreadyExistsException
  = FacetAlreadyExistsException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>A facet with the same name already exists.</p>

#### `FacetAttribute`

``` purescript
newtype FacetAttribute
  = FacetAttribute { "Name" :: AttributeName, "AttributeDefinition" :: NullOrUndefined (FacetAttributeDefinition), "AttributeReference" :: NullOrUndefined (FacetAttributeReference), "RequiredBehavior" :: NullOrUndefined (RequiredAttributeBehavior) }
```

<p>An attribute that is associated with the <a>Facet</a>.</p>

#### `FacetAttributeDefinition`

``` purescript
newtype FacetAttributeDefinition
  = FacetAttributeDefinition { "Type" :: FacetAttributeType, "DefaultValue" :: NullOrUndefined (TypedAttributeValue), "IsImmutable" :: NullOrUndefined (Bool), "Rules" :: NullOrUndefined (RuleMap) }
```

<p>A facet attribute definition. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences">Attribute References</a> for more information.</p>

#### `FacetAttributeList`

``` purescript
newtype FacetAttributeList
  = FacetAttributeList (Array FacetAttribute)
```

#### `FacetAttributeReference`

``` purescript
newtype FacetAttributeReference
  = FacetAttributeReference { "TargetFacetName" :: FacetName, "TargetAttributeName" :: AttributeName }
```

<p>The facet attribute reference that specifies the attribute definition that contains the attribute facet name and attribute name.</p>

#### `FacetAttributeType`

``` purescript
newtype FacetAttributeType
  = FacetAttributeType String
```

#### `FacetAttributeUpdate`

``` purescript
newtype FacetAttributeUpdate
  = FacetAttributeUpdate { "Attribute" :: NullOrUndefined (FacetAttribute), "Action" :: NullOrUndefined (UpdateActionType) }
```

<p>A structure that contains information used to update an attribute.</p>

#### `FacetAttributeUpdateList`

``` purescript
newtype FacetAttributeUpdateList
  = FacetAttributeUpdateList (Array FacetAttributeUpdate)
```

#### `FacetInUseException`

``` purescript
newtype FacetInUseException
  = FacetInUseException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Occurs when deleting a facet that contains an attribute that is a target to an attribute reference in a different facet.</p>

#### `FacetName`

``` purescript
newtype FacetName
  = FacetName String
```

#### `FacetNameList`

``` purescript
newtype FacetNameList
  = FacetNameList (Array FacetName)
```

#### `FacetNotFoundException`

``` purescript
newtype FacetNotFoundException
  = FacetNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified <a>Facet</a> could not be found.</p>

#### `FacetValidationException`

``` purescript
newtype FacetValidationException
  = FacetValidationException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The <a>Facet</a> that you provided was not well formed or could not be validated with the schema.</p>

#### `GetAppliedSchemaVersionRequest`

``` purescript
newtype GetAppliedSchemaVersionRequest
  = GetAppliedSchemaVersionRequest { "SchemaArn" :: Arn }
```

#### `GetAppliedSchemaVersionResponse`

``` purescript
newtype GetAppliedSchemaVersionResponse
  = GetAppliedSchemaVersionResponse { "AppliedSchemaArn" :: NullOrUndefined (Arn) }
```

#### `GetDirectoryRequest`

``` purescript
newtype GetDirectoryRequest
  = GetDirectoryRequest { "DirectoryArn" :: DirectoryArn }
```

#### `GetDirectoryResponse`

``` purescript
newtype GetDirectoryResponse
  = GetDirectoryResponse { "Directory" :: Directory }
```

#### `GetFacetRequest`

``` purescript
newtype GetFacetRequest
  = GetFacetRequest { "SchemaArn" :: Arn, "Name" :: FacetName }
```

#### `GetFacetResponse`

``` purescript
newtype GetFacetResponse
  = GetFacetResponse { "Facet" :: NullOrUndefined (Facet) }
```

#### `GetObjectInformationRequest`

``` purescript
newtype GetObjectInformationRequest
  = GetObjectInformationRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

#### `GetObjectInformationResponse`

``` purescript
newtype GetObjectInformationResponse
  = GetObjectInformationResponse { "SchemaFacets" :: NullOrUndefined (SchemaFacetList), "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

#### `GetSchemaAsJsonRequest`

``` purescript
newtype GetSchemaAsJsonRequest
  = GetSchemaAsJsonRequest { "SchemaArn" :: Arn }
```

#### `GetSchemaAsJsonResponse`

``` purescript
newtype GetSchemaAsJsonResponse
  = GetSchemaAsJsonResponse { "Name" :: NullOrUndefined (SchemaName), "Document" :: NullOrUndefined (SchemaJsonDocument) }
```

#### `GetTypedLinkFacetInformationRequest`

``` purescript
newtype GetTypedLinkFacetInformationRequest
  = GetTypedLinkFacetInformationRequest { "SchemaArn" :: Arn, "Name" :: TypedLinkName }
```

#### `GetTypedLinkFacetInformationResponse`

``` purescript
newtype GetTypedLinkFacetInformationResponse
  = GetTypedLinkFacetInformationResponse { "IdentityAttributeOrder" :: NullOrUndefined (AttributeNameList) }
```

#### `IncompatibleSchemaException`

``` purescript
newtype IncompatibleSchemaException
  = IncompatibleSchemaException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates a failure occurred while performing a check for backward compatibility between the specified schema and the schema that is currently applied to the directory.</p>

#### `IndexAttachment`

``` purescript
newtype IndexAttachment
  = IndexAttachment { "IndexedAttributes" :: NullOrUndefined (AttributeKeyAndValueList), "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents an index and an attached object.</p>

#### `IndexAttachmentList`

``` purescript
newtype IndexAttachmentList
  = IndexAttachmentList (Array IndexAttachment)
```

#### `IndexedAttributeMissingException`

``` purescript
newtype IndexedAttributeMissingException
  = IndexedAttributeMissingException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>An object has been attempted to be attached to an object that does not have the appropriate attribute value.</p>

#### `InternalServiceException`

``` purescript
newtype InternalServiceException
  = InternalServiceException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates a problem that must be resolved by Amazon Web Services. This might be a transient error in which case you can retry your request until it succeeds. Otherwise, go to the <a href="http://status.aws.amazon.com/">AWS Service Health Dashboard</a> site to see if there are any operational issues with the service.</p>

#### `InvalidArnException`

``` purescript
newtype InvalidArnException
  = InvalidArnException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the provided ARN value is not valid.</p>

#### `InvalidAttachmentException`

``` purescript
newtype InvalidAttachmentException
  = InvalidAttachmentException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that an attempt to attach an object with the same link name or to apply a schema with the same name has occurred. Rename the link or the schema and then try again.</p>

#### `InvalidFacetUpdateException`

``` purescript
newtype InvalidFacetUpdateException
  = InvalidFacetUpdateException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>An attempt to modify a <a>Facet</a> resulted in an invalid schema exception.</p>

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the <code>NextToken</code> value is not valid.</p>

#### `InvalidRuleException`

``` purescript
newtype InvalidRuleException
  = InvalidRuleException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Occurs when any of the rule parameter keys or values are invalid.</p>

#### `InvalidSchemaDocException`

``` purescript
newtype InvalidSchemaDocException
  = InvalidSchemaDocException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the provided <code>SchemaDoc</code> value is not valid.</p>

#### `InvalidTaggingRequestException`

``` purescript
newtype InvalidTaggingRequestException
  = InvalidTaggingRequestException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Can occur for multiple reasons such as when you tag a resource that doesn’t exist or if you specify a higher number of tags for a resource than the allowed limit. Allowed limit is 50 tags per resource.</p>

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that limits are exceeded. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/limits.html">Limits</a> for more information.</p>

#### `LinkName`

``` purescript
newtype LinkName
  = LinkName String
```

#### `LinkNameAlreadyInUseException`

``` purescript
newtype LinkNameAlreadyInUseException
  = LinkNameAlreadyInUseException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that a link could not be created due to a naming conflict. Choose a different name and then try again.</p>

#### `LinkNameToObjectIdentifierMap`

``` purescript
newtype LinkNameToObjectIdentifierMap
  = LinkNameToObjectIdentifierMap (Map LinkName ObjectIdentifier)
```

#### `ListAppliedSchemaArnsRequest`

``` purescript
newtype ListAppliedSchemaArnsRequest
  = ListAppliedSchemaArnsRequest { "DirectoryArn" :: Arn, "SchemaArn" :: NullOrUndefined (Arn), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

#### `ListAppliedSchemaArnsResponse`

``` purescript
newtype ListAppliedSchemaArnsResponse
  = ListAppliedSchemaArnsResponse { "SchemaArns" :: NullOrUndefined (Arns), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListAttachedIndicesRequest`

``` purescript
newtype ListAttachedIndicesRequest
  = ListAttachedIndicesRequest { "DirectoryArn" :: Arn, "TargetReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

#### `ListAttachedIndicesResponse`

``` purescript
newtype ListAttachedIndicesResponse
  = ListAttachedIndicesResponse { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListDevelopmentSchemaArnsRequest`

``` purescript
newtype ListDevelopmentSchemaArnsRequest
  = ListDevelopmentSchemaArnsRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

#### `ListDevelopmentSchemaArnsResponse`

``` purescript
newtype ListDevelopmentSchemaArnsResponse
  = ListDevelopmentSchemaArnsResponse { "SchemaArns" :: NullOrUndefined (Arns), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListDirectoriesRequest`

``` purescript
newtype ListDirectoriesRequest
  = ListDirectoriesRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "State'" :: NullOrUndefined (DirectoryState) }
```

#### `ListDirectoriesResponse`

``` purescript
newtype ListDirectoriesResponse
  = ListDirectoriesResponse { "Directories" :: DirectoryList, "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListFacetAttributesRequest`

``` purescript
newtype ListFacetAttributesRequest
  = ListFacetAttributesRequest { "SchemaArn" :: Arn, "Name" :: FacetName, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

#### `ListFacetAttributesResponse`

``` purescript
newtype ListFacetAttributesResponse
  = ListFacetAttributesResponse { "Attributes" :: NullOrUndefined (FacetAttributeList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListFacetNamesRequest`

``` purescript
newtype ListFacetNamesRequest
  = ListFacetNamesRequest { "SchemaArn" :: Arn, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

#### `ListFacetNamesResponse`

``` purescript
newtype ListFacetNamesResponse
  = ListFacetNamesResponse { "FacetNames" :: NullOrUndefined (FacetNameList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListIncomingTypedLinksRequest`

``` purescript
newtype ListIncomingTypedLinksRequest
  = ListIncomingTypedLinksRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList), "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

#### `ListIncomingTypedLinksResponse`

``` purescript
newtype ListIncomingTypedLinksResponse
  = ListIncomingTypedLinksResponse { "LinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListIndexRequest`

``` purescript
newtype ListIndexRequest
  = ListIndexRequest { "DirectoryArn" :: Arn, "RangesOnIndexedValues" :: NullOrUndefined (ObjectAttributeRangeList), "IndexReference" :: ObjectReference, "MaxResults" :: NullOrUndefined (NumberResults), "NextToken" :: NullOrUndefined (NextToken), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

#### `ListIndexResponse`

``` purescript
newtype ListIndexResponse
  = ListIndexResponse { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListObjectAttributesRequest`

``` purescript
newtype ListObjectAttributesRequest
  = ListObjectAttributesRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel), "FacetFilter" :: NullOrUndefined (SchemaFacet) }
```

#### `ListObjectAttributesResponse`

``` purescript
newtype ListObjectAttributesResponse
  = ListObjectAttributesResponse { "Attributes" :: NullOrUndefined (AttributeKeyAndValueList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListObjectChildrenRequest`

``` purescript
newtype ListObjectChildrenRequest
  = ListObjectChildrenRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

#### `ListObjectChildrenResponse`

``` purescript
newtype ListObjectChildrenResponse
  = ListObjectChildrenResponse { "Children" :: NullOrUndefined (LinkNameToObjectIdentifierMap), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListObjectParentPathsRequest`

``` purescript
newtype ListObjectParentPathsRequest
  = ListObjectParentPathsRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

#### `ListObjectParentPathsResponse`

``` purescript
newtype ListObjectParentPathsResponse
  = ListObjectParentPathsResponse { "PathToObjectIdentifiersList" :: NullOrUndefined (PathToObjectIdentifiersList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListObjectParentsRequest`

``` purescript
newtype ListObjectParentsRequest
  = ListObjectParentsRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

#### `ListObjectParentsResponse`

``` purescript
newtype ListObjectParentsResponse
  = ListObjectParentsResponse { "Parents" :: NullOrUndefined (ObjectIdentifierToLinkNameMap), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListObjectPoliciesRequest`

``` purescript
newtype ListObjectPoliciesRequest
  = ListObjectPoliciesRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

#### `ListObjectPoliciesResponse`

``` purescript
newtype ListObjectPoliciesResponse
  = ListObjectPoliciesResponse { "AttachedPolicyIds" :: NullOrUndefined (ObjectIdentifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListOutgoingTypedLinksRequest`

``` purescript
newtype ListOutgoingTypedLinksRequest
  = ListOutgoingTypedLinksRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList), "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

#### `ListOutgoingTypedLinksResponse`

``` purescript
newtype ListOutgoingTypedLinksResponse
  = ListOutgoingTypedLinksResponse { "TypedLinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListPolicyAttachmentsRequest`

``` purescript
newtype ListPolicyAttachmentsRequest
  = ListPolicyAttachmentsRequest { "DirectoryArn" :: Arn, "PolicyReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

#### `ListPolicyAttachmentsResponse`

``` purescript
newtype ListPolicyAttachmentsResponse
  = ListPolicyAttachmentsResponse { "ObjectIdentifiers" :: NullOrUndefined (ObjectIdentifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListPublishedSchemaArnsRequest`

``` purescript
newtype ListPublishedSchemaArnsRequest
  = ListPublishedSchemaArnsRequest { "SchemaArn" :: NullOrUndefined (Arn), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

#### `ListPublishedSchemaArnsResponse`

``` purescript
newtype ListPublishedSchemaArnsResponse
  = ListPublishedSchemaArnsResponse { "SchemaArns" :: NullOrUndefined (Arns), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListTagsForResourceRequest`

``` purescript
newtype ListTagsForResourceRequest
  = ListTagsForResourceRequest { "ResourceArn" :: Arn, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (TagsNumberResults) }
```

#### `ListTagsForResourceResponse`

``` purescript
newtype ListTagsForResourceResponse
  = ListTagsForResourceResponse { "Tags" :: NullOrUndefined (TagList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListTypedLinkFacetAttributesRequest`

``` purescript
newtype ListTypedLinkFacetAttributesRequest
  = ListTypedLinkFacetAttributesRequest { "SchemaArn" :: Arn, "Name" :: TypedLinkName, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

#### `ListTypedLinkFacetAttributesResponse`

``` purescript
newtype ListTypedLinkFacetAttributesResponse
  = ListTypedLinkFacetAttributesResponse { "Attributes" :: NullOrUndefined (TypedLinkAttributeDefinitionList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListTypedLinkFacetNamesRequest`

``` purescript
newtype ListTypedLinkFacetNamesRequest
  = ListTypedLinkFacetNamesRequest { "SchemaArn" :: Arn, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

#### `ListTypedLinkFacetNamesResponse`

``` purescript
newtype ListTypedLinkFacetNamesResponse
  = ListTypedLinkFacetNamesResponse { "FacetNames" :: NullOrUndefined (TypedLinkNameList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `LookupPolicyRequest`

``` purescript
newtype LookupPolicyRequest
  = LookupPolicyRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

#### `LookupPolicyResponse`

``` purescript
newtype LookupPolicyResponse
  = LookupPolicyResponse { "PolicyToPathList" :: NullOrUndefined (PolicyToPathList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `NotIndexException`

``` purescript
newtype NotIndexException
  = NotIndexException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the requested operation can only operate on index objects.</p>

#### `NotNodeException`

``` purescript
newtype NotNodeException
  = NotNodeException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Occurs when any invalid operations are performed on an object that is not a node, such as calling <code>ListObjectChildren</code> for a leaf node object.</p>

#### `NotPolicyException`

``` purescript
newtype NotPolicyException
  = NotPolicyException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the requested operation can only operate on policy objects.</p>

#### `NumberAttributeValue`

``` purescript
newtype NumberAttributeValue
  = NumberAttributeValue String
```

#### `NumberResults`

``` purescript
newtype NumberResults
  = NumberResults Int
```

#### `ObjectAlreadyDetachedException`

``` purescript
newtype ObjectAlreadyDetachedException
  = ObjectAlreadyDetachedException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the object is not attached to the index.</p>

#### `ObjectAttributeAction`

``` purescript
newtype ObjectAttributeAction
  = ObjectAttributeAction { "ObjectAttributeActionType" :: NullOrUndefined (UpdateActionType), "ObjectAttributeUpdateValue" :: NullOrUndefined (TypedAttributeValue) }
```

<p>The action to take on the object attribute.</p>

#### `ObjectAttributeRange`

``` purescript
newtype ObjectAttributeRange
  = ObjectAttributeRange { "AttributeKey" :: NullOrUndefined (AttributeKey), "Range" :: NullOrUndefined (TypedAttributeValueRange) }
```

<p>A range of attributes.</p>

#### `ObjectAttributeRangeList`

``` purescript
newtype ObjectAttributeRangeList
  = ObjectAttributeRangeList (Array ObjectAttributeRange)
```

#### `ObjectAttributeUpdate`

``` purescript
newtype ObjectAttributeUpdate
  = ObjectAttributeUpdate { "ObjectAttributeKey" :: NullOrUndefined (AttributeKey), "ObjectAttributeAction" :: NullOrUndefined (ObjectAttributeAction) }
```

<p>Structure that contains attribute update information.</p>

#### `ObjectAttributeUpdateList`

``` purescript
newtype ObjectAttributeUpdateList
  = ObjectAttributeUpdateList (Array ObjectAttributeUpdate)
```

#### `ObjectIdentifier`

``` purescript
newtype ObjectIdentifier
  = ObjectIdentifier String
```

#### `ObjectIdentifierList`

``` purescript
newtype ObjectIdentifierList
  = ObjectIdentifierList (Array ObjectIdentifier)
```

#### `ObjectIdentifierToLinkNameMap`

``` purescript
newtype ObjectIdentifierToLinkNameMap
  = ObjectIdentifierToLinkNameMap (Map ObjectIdentifier LinkName)
```

#### `ObjectNotDetachedException`

``` purescript
newtype ObjectNotDetachedException
  = ObjectNotDetachedException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the requested operation cannot be completed because the object has not been detached from the tree.</p>

#### `ObjectReference`

``` purescript
newtype ObjectReference
  = ObjectReference { "Selector" :: NullOrUndefined (SelectorObjectReference) }
```

<p>The reference that identifies an object.</p>

#### `ObjectType`

``` purescript
newtype ObjectType
  = ObjectType String
```

#### `PathString`

``` purescript
newtype PathString
  = PathString String
```

#### `PathToObjectIdentifiers`

``` purescript
newtype PathToObjectIdentifiers
  = PathToObjectIdentifiers { "Path" :: NullOrUndefined (PathString), "ObjectIdentifiers" :: NullOrUndefined (ObjectIdentifierList) }
```

<p>Returns the path to the <code>ObjectIdentifiers</code> that is associated with the directory.</p>

#### `PathToObjectIdentifiersList`

``` purescript
newtype PathToObjectIdentifiersList
  = PathToObjectIdentifiersList (Array PathToObjectIdentifiers)
```

#### `PolicyAttachment`

``` purescript
newtype PolicyAttachment
  = PolicyAttachment { "PolicyId" :: NullOrUndefined (ObjectIdentifier), "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier), "PolicyType" :: NullOrUndefined (PolicyType) }
```

<p>Contains the <code>PolicyType</code>, <code>PolicyId</code>, and the <code>ObjectIdentifier</code> to which it is attached. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies">Policies</a>.</p>

#### `PolicyAttachmentList`

``` purescript
newtype PolicyAttachmentList
  = PolicyAttachmentList (Array PolicyAttachment)
```

#### `PolicyToPath`

``` purescript
newtype PolicyToPath
  = PolicyToPath { "Path" :: NullOrUndefined (PathString), "Policies" :: NullOrUndefined (PolicyAttachmentList) }
```

<p>Used when a regular object exists in a <a>Directory</a> and you want to find all of the policies that are associated with that object and the parent to that object.</p>

#### `PolicyToPathList`

``` purescript
newtype PolicyToPathList
  = PolicyToPathList (Array PolicyToPath)
```

#### `PolicyType`

``` purescript
newtype PolicyType
  = PolicyType String
```

#### `PublishSchemaRequest`

``` purescript
newtype PublishSchemaRequest
  = PublishSchemaRequest { "DevelopmentSchemaArn" :: Arn, "Version" :: Version, "MinorVersion" :: NullOrUndefined (Version), "Name" :: NullOrUndefined (SchemaName) }
```

#### `PublishSchemaResponse`

``` purescript
newtype PublishSchemaResponse
  = PublishSchemaResponse { "PublishedSchemaArn" :: NullOrUndefined (Arn) }
```

#### `PutSchemaFromJsonRequest`

``` purescript
newtype PutSchemaFromJsonRequest
  = PutSchemaFromJsonRequest { "SchemaArn" :: Arn, "Document" :: SchemaJsonDocument }
```

#### `PutSchemaFromJsonResponse`

``` purescript
newtype PutSchemaFromJsonResponse
  = PutSchemaFromJsonResponse { "Arn" :: NullOrUndefined (Arn) }
```

#### `RangeMode`

``` purescript
newtype RangeMode
  = RangeMode String
```

#### `RemoveFacetFromObjectRequest`

``` purescript
newtype RemoveFacetFromObjectRequest
  = RemoveFacetFromObjectRequest { "DirectoryArn" :: Arn, "SchemaFacet" :: SchemaFacet, "ObjectReference" :: ObjectReference }
```

#### `RemoveFacetFromObjectResponse`

``` purescript
newtype RemoveFacetFromObjectResponse
  = RemoveFacetFromObjectResponse {  }
```

#### `RequiredAttributeBehavior`

``` purescript
newtype RequiredAttributeBehavior
  = RequiredAttributeBehavior String
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified resource could not be found.</p>

#### `RetryableConflictException`

``` purescript
newtype RetryableConflictException
  = RetryableConflictException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Occurs when a conflict with a previous successful write is detected. For example, if a write operation occurs on an object and then an attempt is made to read the object using “SERIALIZABLE” consistency, this exception may result. This generally occurs when the previous write did not have time to propagate to the host serving the current request. A retry (with appropriate backoff logic) is the recommended response to this exception.</p>

#### `Rule`

``` purescript
newtype Rule
  = Rule { "Type" :: NullOrUndefined (RuleType), "Parameters" :: NullOrUndefined (RuleParameterMap) }
```

<p>Contains an Amazon Resource Name (ARN) and parameters that are associated with the rule.</p>

#### `RuleKey`

``` purescript
newtype RuleKey
  = RuleKey String
```

#### `RuleMap`

``` purescript
newtype RuleMap
  = RuleMap (Map RuleKey Rule)
```

#### `RuleParameterKey`

``` purescript
newtype RuleParameterKey
  = RuleParameterKey String
```

#### `RuleParameterMap`

``` purescript
newtype RuleParameterMap
  = RuleParameterMap (Map RuleParameterKey RuleParameterValue)
```

#### `RuleParameterValue`

``` purescript
newtype RuleParameterValue
  = RuleParameterValue String
```

#### `RuleType`

``` purescript
newtype RuleType
  = RuleType String
```

#### `SchemaAlreadyExistsException`

``` purescript
newtype SchemaAlreadyExistsException
  = SchemaAlreadyExistsException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that a schema could not be created due to a naming conflict. Please select a different name and then try again.</p>

#### `SchemaAlreadyPublishedException`

``` purescript
newtype SchemaAlreadyPublishedException
  = SchemaAlreadyPublishedException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that a schema is already published.</p>

#### `SchemaFacet`

``` purescript
newtype SchemaFacet
  = SchemaFacet { "SchemaArn" :: NullOrUndefined (Arn), "FacetName" :: NullOrUndefined (FacetName) }
```

<p>A facet.</p>

#### `SchemaFacetList`

``` purescript
newtype SchemaFacetList
  = SchemaFacetList (Array SchemaFacet)
```

#### `SchemaJsonDocument`

``` purescript
newtype SchemaJsonDocument
  = SchemaJsonDocument String
```

#### `SchemaName`

``` purescript
newtype SchemaName
  = SchemaName String
```

#### `SelectorObjectReference`

``` purescript
newtype SelectorObjectReference
  = SelectorObjectReference String
```

#### `StillContainsLinksException`

``` purescript
newtype StillContainsLinksException
  = StillContainsLinksException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The object could not be deleted because links still exist. Remove the links and then try the operation again.</p>

#### `StringAttributeValue`

``` purescript
newtype StringAttributeValue
  = StringAttributeValue String
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (TagKey), "Value" :: NullOrUndefined (TagValue) }
```

<p>The tag structure that contains a tag key and value.</p>

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

#### `TagResourceRequest`

``` purescript
newtype TagResourceRequest
  = TagResourceRequest { "ResourceArn" :: Arn, "Tags" :: TagList }
```

#### `TagResourceResponse`

``` purescript
newtype TagResourceResponse
  = TagResourceResponse {  }
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `TagsNumberResults`

``` purescript
newtype TagsNumberResults
  = TagsNumberResults Int
```

#### `TypedAttributeValue`

``` purescript
newtype TypedAttributeValue
  = TypedAttributeValue { "StringValue" :: NullOrUndefined (StringAttributeValue), "BinaryValue" :: NullOrUndefined (BinaryAttributeValue), "BooleanValue" :: NullOrUndefined (BooleanAttributeValue), "NumberValue" :: NullOrUndefined (NumberAttributeValue), "DatetimeValue" :: NullOrUndefined (DatetimeAttributeValue) }
```

<p>Represents the data for a typed attribute. You can set one, and only one, of the elements. Each attribute in an item is a name-value pair. Attributes have a single value.</p>

#### `TypedAttributeValueRange`

``` purescript
newtype TypedAttributeValueRange
  = TypedAttributeValueRange { "StartMode" :: RangeMode, "StartValue" :: NullOrUndefined (TypedAttributeValue), "EndMode" :: RangeMode, "EndValue" :: NullOrUndefined (TypedAttributeValue) }
```

<p>A range of attribute values.</p>

#### `TypedLinkAttributeDefinition`

``` purescript
newtype TypedLinkAttributeDefinition
  = TypedLinkAttributeDefinition { "Name" :: AttributeName, "Type" :: FacetAttributeType, "DefaultValue" :: NullOrUndefined (TypedAttributeValue), "IsImmutable" :: NullOrUndefined (Bool), "Rules" :: NullOrUndefined (RuleMap), "RequiredBehavior" :: RequiredAttributeBehavior }
```

<p>A typed link attribute definition.</p>

#### `TypedLinkAttributeDefinitionList`

``` purescript
newtype TypedLinkAttributeDefinitionList
  = TypedLinkAttributeDefinitionList (Array TypedLinkAttributeDefinition)
```

#### `TypedLinkAttributeRange`

``` purescript
newtype TypedLinkAttributeRange
  = TypedLinkAttributeRange { "AttributeName" :: NullOrUndefined (AttributeName), "Range" :: TypedAttributeValueRange }
```

<p>Identifies the range of attributes that are used by a specified filter.</p>

#### `TypedLinkAttributeRangeList`

``` purescript
newtype TypedLinkAttributeRangeList
  = TypedLinkAttributeRangeList (Array TypedLinkAttributeRange)
```

#### `TypedLinkFacet`

``` purescript
newtype TypedLinkFacet
  = TypedLinkFacet { "Name" :: TypedLinkName, "Attributes" :: TypedLinkAttributeDefinitionList, "IdentityAttributeOrder" :: AttributeNameList }
```

<p>Defines the typed links structure and its attributes. To create a typed link facet, use the <a>CreateTypedLinkFacet</a> API.</p>

#### `TypedLinkFacetAttributeUpdate`

``` purescript
newtype TypedLinkFacetAttributeUpdate
  = TypedLinkFacetAttributeUpdate { "Attribute" :: TypedLinkAttributeDefinition, "Action" :: UpdateActionType }
```

<p>A typed link facet attribute update.</p>

#### `TypedLinkFacetAttributeUpdateList`

``` purescript
newtype TypedLinkFacetAttributeUpdateList
  = TypedLinkFacetAttributeUpdateList (Array TypedLinkFacetAttributeUpdate)
```

#### `TypedLinkName`

``` purescript
newtype TypedLinkName
  = TypedLinkName String
```

#### `TypedLinkNameList`

``` purescript
newtype TypedLinkNameList
  = TypedLinkNameList (Array TypedLinkName)
```

#### `TypedLinkSchemaAndFacetName`

``` purescript
newtype TypedLinkSchemaAndFacetName
  = TypedLinkSchemaAndFacetName { "SchemaArn" :: Arn, "TypedLinkName" :: TypedLinkName }
```

<p>Identifies the schema Amazon Resource Name (ARN) and facet name for the typed link.</p>

#### `TypedLinkSpecifier`

``` purescript
newtype TypedLinkSpecifier
  = TypedLinkSpecifier { "TypedLinkFacet" :: TypedLinkSchemaAndFacetName, "SourceObjectReference" :: ObjectReference, "TargetObjectReference" :: ObjectReference, "IdentityAttributeValues" :: AttributeNameAndValueList }
```

<p>Contains all the information that is used to uniquely identify a typed link. The parameters discussed in this topic are used to uniquely specify the typed link being operated on. The <a>AttachTypedLink</a> API returns a typed link specifier while the <a>DetachTypedLink</a> API accepts one as input. Similarly, the <a>ListIncomingTypedLinks</a> and <a>ListOutgoingTypedLinks</a> API operations provide typed link specifiers as output. You can also construct a typed link specifier from scratch.</p>

#### `TypedLinkSpecifierList`

``` purescript
newtype TypedLinkSpecifierList
  = TypedLinkSpecifierList (Array TypedLinkSpecifier)
```

#### `UnsupportedIndexTypeException`

``` purescript
newtype UnsupportedIndexTypeException
  = UnsupportedIndexTypeException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the requested index type is not supported.</p>

#### `UntagResourceRequest`

``` purescript
newtype UntagResourceRequest
  = UntagResourceRequest { "ResourceArn" :: Arn, "TagKeys" :: TagKeyList }
```

#### `UntagResourceResponse`

``` purescript
newtype UntagResourceResponse
  = UntagResourceResponse {  }
```

#### `UpdateActionType`

``` purescript
newtype UpdateActionType
  = UpdateActionType String
```

#### `UpdateFacetRequest`

``` purescript
newtype UpdateFacetRequest
  = UpdateFacetRequest { "SchemaArn" :: Arn, "Name" :: FacetName, "AttributeUpdates" :: NullOrUndefined (FacetAttributeUpdateList), "ObjectType" :: NullOrUndefined (ObjectType) }
```

#### `UpdateFacetResponse`

``` purescript
newtype UpdateFacetResponse
  = UpdateFacetResponse {  }
```

#### `UpdateObjectAttributesRequest`

``` purescript
newtype UpdateObjectAttributesRequest
  = UpdateObjectAttributesRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "AttributeUpdates" :: ObjectAttributeUpdateList }
```

#### `UpdateObjectAttributesResponse`

``` purescript
newtype UpdateObjectAttributesResponse
  = UpdateObjectAttributesResponse { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

#### `UpdateSchemaRequest`

``` purescript
newtype UpdateSchemaRequest
  = UpdateSchemaRequest { "SchemaArn" :: Arn, "Name" :: SchemaName }
```

#### `UpdateSchemaResponse`

``` purescript
newtype UpdateSchemaResponse
  = UpdateSchemaResponse { "SchemaArn" :: NullOrUndefined (Arn) }
```

#### `UpdateTypedLinkFacetRequest`

``` purescript
newtype UpdateTypedLinkFacetRequest
  = UpdateTypedLinkFacetRequest { "SchemaArn" :: Arn, "Name" :: TypedLinkName, "AttributeUpdates" :: TypedLinkFacetAttributeUpdateList, "IdentityAttributeOrder" :: AttributeNameList }
```

#### `UpdateTypedLinkFacetResponse`

``` purescript
newtype UpdateTypedLinkFacetResponse
  = UpdateTypedLinkFacetResponse {  }
```

#### `UpgradeAppliedSchemaRequest`

``` purescript
newtype UpgradeAppliedSchemaRequest
  = UpgradeAppliedSchemaRequest { "PublishedSchemaArn" :: Arn, "DirectoryArn" :: Arn, "DryRun" :: NullOrUndefined (Bool) }
```

#### `UpgradeAppliedSchemaResponse`

``` purescript
newtype UpgradeAppliedSchemaResponse
  = UpgradeAppliedSchemaResponse { "UpgradedSchemaArn" :: NullOrUndefined (Arn), "DirectoryArn" :: NullOrUndefined (Arn) }
```

#### `UpgradePublishedSchemaRequest`

``` purescript
newtype UpgradePublishedSchemaRequest
  = UpgradePublishedSchemaRequest { "DevelopmentSchemaArn" :: Arn, "PublishedSchemaArn" :: Arn, "MinorVersion" :: Version, "DryRun" :: NullOrUndefined (Bool) }
```

#### `UpgradePublishedSchemaResponse`

``` purescript
newtype UpgradePublishedSchemaResponse
  = UpgradePublishedSchemaResponse { "UpgradedSchemaArn" :: NullOrUndefined (Arn) }
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that your request is malformed in some manner. See the exception message.</p>

#### `Version`

``` purescript
newtype Version
  = Version String
```


