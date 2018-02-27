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

##### Instances
``` purescript
Newtype AccessDeniedException _
```

#### `AddFacetToObjectRequest`

``` purescript
newtype AddFacetToObjectRequest
  = AddFacetToObjectRequest { "DirectoryArn" :: Arn, "SchemaFacet" :: SchemaFacet, "ObjectAttributeList" :: NullOrUndefined (AttributeKeyAndValueList), "ObjectReference" :: ObjectReference }
```

##### Instances
``` purescript
Newtype AddFacetToObjectRequest _
```

#### `AddFacetToObjectResponse`

``` purescript
newtype AddFacetToObjectResponse
  = AddFacetToObjectResponse {  }
```

##### Instances
``` purescript
Newtype AddFacetToObjectResponse _
```

#### `ApplySchemaRequest`

``` purescript
newtype ApplySchemaRequest
  = ApplySchemaRequest { "PublishedSchemaArn" :: Arn, "DirectoryArn" :: Arn }
```

##### Instances
``` purescript
Newtype ApplySchemaRequest _
```

#### `ApplySchemaResponse`

``` purescript
newtype ApplySchemaResponse
  = ApplySchemaResponse { "AppliedSchemaArn" :: NullOrUndefined (Arn), "DirectoryArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype ApplySchemaResponse _
```

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

##### Instances
``` purescript
Newtype Arn _
```

#### `Arns`

``` purescript
newtype Arns
  = Arns (Array Arn)
```

##### Instances
``` purescript
Newtype Arns _
```

#### `AttachObjectRequest`

``` purescript
newtype AttachObjectRequest
  = AttachObjectRequest { "DirectoryArn" :: Arn, "ParentReference" :: ObjectReference, "ChildReference" :: ObjectReference, "LinkName" :: LinkName }
```

##### Instances
``` purescript
Newtype AttachObjectRequest _
```

#### `AttachObjectResponse`

``` purescript
newtype AttachObjectResponse
  = AttachObjectResponse { "AttachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

##### Instances
``` purescript
Newtype AttachObjectResponse _
```

#### `AttachPolicyRequest`

``` purescript
newtype AttachPolicyRequest
  = AttachPolicyRequest { "DirectoryArn" :: NullOrUndefined (Arn), "PolicyReference" :: ObjectReference, "ObjectReference" :: ObjectReference }
```

##### Instances
``` purescript
Newtype AttachPolicyRequest _
```

#### `AttachPolicyResponse`

``` purescript
newtype AttachPolicyResponse
  = AttachPolicyResponse {  }
```

##### Instances
``` purescript
Newtype AttachPolicyResponse _
```

#### `AttachToIndexRequest`

``` purescript
newtype AttachToIndexRequest
  = AttachToIndexRequest { "DirectoryArn" :: Arn, "IndexReference" :: ObjectReference, "TargetReference" :: ObjectReference }
```

##### Instances
``` purescript
Newtype AttachToIndexRequest _
```

#### `AttachToIndexResponse`

``` purescript
newtype AttachToIndexResponse
  = AttachToIndexResponse { "AttachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

##### Instances
``` purescript
Newtype AttachToIndexResponse _
```

#### `AttachTypedLinkRequest`

``` purescript
newtype AttachTypedLinkRequest
  = AttachTypedLinkRequest { "DirectoryArn" :: Arn, "SourceObjectReference" :: ObjectReference, "TargetObjectReference" :: ObjectReference, "TypedLinkFacet" :: TypedLinkSchemaAndFacetName, "Attributes" :: AttributeNameAndValueList }
```

##### Instances
``` purescript
Newtype AttachTypedLinkRequest _
```

#### `AttachTypedLinkResponse`

``` purescript
newtype AttachTypedLinkResponse
  = AttachTypedLinkResponse { "TypedLinkSpecifier" :: NullOrUndefined (TypedLinkSpecifier) }
```

##### Instances
``` purescript
Newtype AttachTypedLinkResponse _
```

#### `AttributeKey`

``` purescript
newtype AttributeKey
  = AttributeKey { "SchemaArn" :: Arn, "FacetName" :: FacetName, "Name" :: AttributeName }
```

<p>A unique identifier for an attribute.</p>

##### Instances
``` purescript
Newtype AttributeKey _
```

#### `AttributeKeyAndValue`

``` purescript
newtype AttributeKeyAndValue
  = AttributeKeyAndValue { "Key" :: AttributeKey, "Value" :: TypedAttributeValue }
```

<p>The combination of an attribute key and an attribute value.</p>

##### Instances
``` purescript
Newtype AttributeKeyAndValue _
```

#### `AttributeKeyAndValueList`

``` purescript
newtype AttributeKeyAndValueList
  = AttributeKeyAndValueList (Array AttributeKeyAndValue)
```

##### Instances
``` purescript
Newtype AttributeKeyAndValueList _
```

#### `AttributeKeyList`

``` purescript
newtype AttributeKeyList
  = AttributeKeyList (Array AttributeKey)
```

##### Instances
``` purescript
Newtype AttributeKeyList _
```

#### `AttributeName`

``` purescript
newtype AttributeName
  = AttributeName String
```

##### Instances
``` purescript
Newtype AttributeName _
```

#### `AttributeNameAndValue`

``` purescript
newtype AttributeNameAndValue
  = AttributeNameAndValue { "AttributeName" :: AttributeName, "Value" :: TypedAttributeValue }
```

<p>Identifies the attribute name and value for a typed link.</p>

##### Instances
``` purescript
Newtype AttributeNameAndValue _
```

#### `AttributeNameAndValueList`

``` purescript
newtype AttributeNameAndValueList
  = AttributeNameAndValueList (Array AttributeNameAndValue)
```

##### Instances
``` purescript
Newtype AttributeNameAndValueList _
```

#### `AttributeNameList`

``` purescript
newtype AttributeNameList
  = AttributeNameList (Array AttributeName)
```

##### Instances
``` purescript
Newtype AttributeNameList _
```

#### `BatchAddFacetToObject`

``` purescript
newtype BatchAddFacetToObject
  = BatchAddFacetToObject { "SchemaFacet" :: SchemaFacet, "ObjectAttributeList" :: AttributeKeyAndValueList, "ObjectReference" :: ObjectReference }
```

<p>Represents the output of a batch add facet to object operation.</p>

##### Instances
``` purescript
Newtype BatchAddFacetToObject _
```

#### `BatchAddFacetToObjectResponse`

``` purescript
newtype BatchAddFacetToObjectResponse
  = BatchAddFacetToObjectResponse {  }
```

<p>The result of a batch add facet to object operation.</p>

##### Instances
``` purescript
Newtype BatchAddFacetToObjectResponse _
```

#### `BatchAttachObject`

``` purescript
newtype BatchAttachObject
  = BatchAttachObject { "ParentReference" :: ObjectReference, "ChildReference" :: ObjectReference, "LinkName" :: LinkName }
```

<p>Represents the output of an <a>AttachObject</a> operation.</p>

##### Instances
``` purescript
Newtype BatchAttachObject _
```

#### `BatchAttachObjectResponse`

``` purescript
newtype BatchAttachObjectResponse
  = BatchAttachObjectResponse { "AttachedObjectIdentifier'" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output batch <a>AttachObject</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchAttachObjectResponse _
```

#### `BatchAttachPolicy`

``` purescript
newtype BatchAttachPolicy
  = BatchAttachPolicy { "PolicyReference" :: ObjectReference, "ObjectReference" :: ObjectReference }
```

<p>Attaches a policy object to a regular object inside a <a>BatchRead</a> operation. For more information, see <a>AttachPolicy</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchAttachPolicy _
```

#### `BatchAttachPolicyResponse`

``` purescript
newtype BatchAttachPolicyResponse
  = BatchAttachPolicyResponse {  }
```

<p>Represents the output of an <a>AttachPolicy</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchAttachPolicyResponse _
```

#### `BatchAttachToIndex`

``` purescript
newtype BatchAttachToIndex
  = BatchAttachToIndex { "IndexReference" :: ObjectReference, "TargetReference" :: ObjectReference }
```

<p>Attaches the specified object to the specified index inside a <a>BatchRead</a> operation. For more information, see <a>AttachToIndex</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchAttachToIndex _
```

#### `BatchAttachToIndexResponse`

``` purescript
newtype BatchAttachToIndexResponse
  = BatchAttachToIndexResponse { "AttachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <a>AttachToIndex</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchAttachToIndexResponse _
```

#### `BatchAttachTypedLink`

``` purescript
newtype BatchAttachTypedLink
  = BatchAttachTypedLink { "SourceObjectReference" :: ObjectReference, "TargetObjectReference" :: ObjectReference, "TypedLinkFacet" :: TypedLinkSchemaAndFacetName, "Attributes" :: AttributeNameAndValueList }
```

<p>Attaches a typed link to a specified source and target object inside a <a>BatchRead</a> operation. For more information, see <a>AttachTypedLink</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchAttachTypedLink _
```

#### `BatchAttachTypedLinkResponse`

``` purescript
newtype BatchAttachTypedLinkResponse
  = BatchAttachTypedLinkResponse { "TypedLinkSpecifier" :: NullOrUndefined (TypedLinkSpecifier) }
```

<p>Represents the output of a <a>AttachTypedLink</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchAttachTypedLinkResponse _
```

#### `BatchCreateIndex`

``` purescript
newtype BatchCreateIndex
  = BatchCreateIndex { "OrderedIndexedAttributeList" :: AttributeKeyList, "IsUnique" :: Bool, "ParentReference" :: NullOrUndefined (ObjectReference), "LinkName" :: NullOrUndefined (LinkName), "BatchReferenceName" :: NullOrUndefined (BatchReferenceName) }
```

<p>Creates an index object inside of a <a>BatchRead</a> operation. For more information, see <a>CreateIndex</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchCreateIndex _
```

#### `BatchCreateIndexResponse`

``` purescript
newtype BatchCreateIndexResponse
  = BatchCreateIndexResponse { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <a>CreateIndex</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchCreateIndexResponse _
```

#### `BatchCreateObject`

``` purescript
newtype BatchCreateObject
  = BatchCreateObject { "SchemaFacet" :: SchemaFacetList, "ObjectAttributeList" :: AttributeKeyAndValueList, "ParentReference" :: ObjectReference, "LinkName" :: LinkName, "BatchReferenceName" :: BatchReferenceName }
```

<p>Represents the output of a <a>CreateObject</a> operation.</p>

##### Instances
``` purescript
Newtype BatchCreateObject _
```

#### `BatchCreateObjectResponse`

``` purescript
newtype BatchCreateObjectResponse
  = BatchCreateObjectResponse { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <a>CreateObject</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchCreateObjectResponse _
```

#### `BatchDeleteObject`

``` purescript
newtype BatchDeleteObject
  = BatchDeleteObject { "ObjectReference" :: ObjectReference }
```

<p>Represents the output of a <a>DeleteObject</a> operation.</p>

##### Instances
``` purescript
Newtype BatchDeleteObject _
```

#### `BatchDeleteObjectResponse`

``` purescript
newtype BatchDeleteObjectResponse
  = BatchDeleteObjectResponse {  }
```

<p>Represents the output of a <a>DeleteObject</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchDeleteObjectResponse _
```

#### `BatchDetachFromIndex`

``` purescript
newtype BatchDetachFromIndex
  = BatchDetachFromIndex { "IndexReference" :: ObjectReference, "TargetReference" :: ObjectReference }
```

<p>Detaches the specified object from the specified index inside a <a>BatchRead</a> operation. For more information, see <a>DetachFromIndex</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchDetachFromIndex _
```

#### `BatchDetachFromIndexResponse`

``` purescript
newtype BatchDetachFromIndexResponse
  = BatchDetachFromIndexResponse { "DetachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <a>DetachFromIndex</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchDetachFromIndexResponse _
```

#### `BatchDetachObject`

``` purescript
newtype BatchDetachObject
  = BatchDetachObject { "ParentReference" :: ObjectReference, "LinkName" :: LinkName, "BatchReferenceName" :: BatchReferenceName }
```

<p>Represents the output of a <a>DetachObject</a> operation.</p>

##### Instances
``` purescript
Newtype BatchDetachObject _
```

#### `BatchDetachObjectResponse`

``` purescript
newtype BatchDetachObjectResponse
  = BatchDetachObjectResponse { "DetachedObjectIdentifier'" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <a>DetachObject</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchDetachObjectResponse _
```

#### `BatchDetachPolicy`

``` purescript
newtype BatchDetachPolicy
  = BatchDetachPolicy { "PolicyReference" :: ObjectReference, "ObjectReference" :: ObjectReference }
```

<p>Detaches the specified policy from the specified directory inside a <a>BatchWrite</a> operation. For more information, see <a>DetachPolicy</a> and <a>BatchWriteRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchDetachPolicy _
```

#### `BatchDetachPolicyResponse`

``` purescript
newtype BatchDetachPolicyResponse
  = BatchDetachPolicyResponse {  }
```

<p>Represents the output of a <a>DetachPolicy</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchDetachPolicyResponse _
```

#### `BatchDetachTypedLink`

``` purescript
newtype BatchDetachTypedLink
  = BatchDetachTypedLink { "TypedLinkSpecifier" :: TypedLinkSpecifier }
```

<p>Detaches a typed link from a specified source and target object inside a <a>BatchRead</a> operation. For more information, see <a>DetachTypedLink</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchDetachTypedLink _
```

#### `BatchDetachTypedLinkResponse`

``` purescript
newtype BatchDetachTypedLinkResponse
  = BatchDetachTypedLinkResponse {  }
```

<p>Represents the output of a <a>DetachTypedLink</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchDetachTypedLinkResponse _
```

#### `BatchGetObjectInformation`

``` purescript
newtype BatchGetObjectInformation
  = BatchGetObjectInformation { "ObjectReference" :: ObjectReference }
```

<p>Retrieves metadata about an object inside a <a>BatchRead</a> operation. For more information, see <a>GetObjectInformation</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchGetObjectInformation _
```

#### `BatchGetObjectInformationResponse`

``` purescript
newtype BatchGetObjectInformationResponse
  = BatchGetObjectInformationResponse { "SchemaFacets" :: NullOrUndefined (SchemaFacetList), "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <a>GetObjectInformation</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchGetObjectInformationResponse _
```

#### `BatchListAttachedIndices`

``` purescript
newtype BatchListAttachedIndices
  = BatchListAttachedIndices { "TargetReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Lists indices attached to an object inside a <a>BatchRead</a> operation. For more information, see <a>ListAttachedIndices</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchListAttachedIndices _
```

#### `BatchListAttachedIndicesResponse`

``` purescript
newtype BatchListAttachedIndicesResponse
  = BatchListAttachedIndicesResponse { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListAttachedIndices</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchListAttachedIndicesResponse _
```

#### `BatchListIncomingTypedLinks`

``` purescript
newtype BatchListIncomingTypedLinks
  = BatchListIncomingTypedLinks { "ObjectReference" :: ObjectReference, "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList), "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Returns a paginated list of all the incoming <a>TypedLinkSpecifier</a> information for an object inside a <a>BatchRead</a> operation. For more information, see <a>ListIncomingTypedLinks</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchListIncomingTypedLinks _
```

#### `BatchListIncomingTypedLinksResponse`

``` purescript
newtype BatchListIncomingTypedLinksResponse
  = BatchListIncomingTypedLinksResponse { "LinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListIncomingTypedLinks</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchListIncomingTypedLinksResponse _
```

#### `BatchListIndex`

``` purescript
newtype BatchListIndex
  = BatchListIndex { "RangesOnIndexedValues" :: NullOrUndefined (ObjectAttributeRangeList), "IndexReference" :: ObjectReference, "MaxResults" :: NullOrUndefined (NumberResults), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Lists objects attached to the specified index inside a <a>BatchRead</a> operation. For more information, see <a>ListIndex</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchListIndex _
```

#### `BatchListIndexResponse`

``` purescript
newtype BatchListIndexResponse
  = BatchListIndexResponse { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListIndex</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchListIndexResponse _
```

#### `BatchListObjectAttributes`

``` purescript
newtype BatchListObjectAttributes
  = BatchListObjectAttributes { "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "FacetFilter" :: NullOrUndefined (SchemaFacet) }
```

<p>Represents the output of a <a>ListObjectAttributes</a> operation.</p>

##### Instances
``` purescript
Newtype BatchListObjectAttributes _
```

#### `BatchListObjectAttributesResponse`

``` purescript
newtype BatchListObjectAttributesResponse
  = BatchListObjectAttributesResponse { "Attributes" :: NullOrUndefined (AttributeKeyAndValueList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListObjectAttributes</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchListObjectAttributesResponse _
```

#### `BatchListObjectChildren`

``` purescript
newtype BatchListObjectChildren
  = BatchListObjectChildren { "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Represents the output of a <a>ListObjectChildren</a> operation.</p>

##### Instances
``` purescript
Newtype BatchListObjectChildren _
```

#### `BatchListObjectChildrenResponse`

``` purescript
newtype BatchListObjectChildrenResponse
  = BatchListObjectChildrenResponse { "Children" :: NullOrUndefined (LinkNameToObjectIdentifierMap), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListObjectChildren</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchListObjectChildrenResponse _
```

#### `BatchListObjectParentPaths`

``` purescript
newtype BatchListObjectParentPaths
  = BatchListObjectParentPaths { "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects inside a <a>BatchRead</a> operation. For more information, see <a>ListObjectParentPaths</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchListObjectParentPaths _
```

#### `BatchListObjectParentPathsResponse`

``` purescript
newtype BatchListObjectParentPathsResponse
  = BatchListObjectParentPathsResponse { "PathToObjectIdentifiersList" :: NullOrUndefined (PathToObjectIdentifiersList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListObjectParentPaths</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchListObjectParentPathsResponse _
```

#### `BatchListObjectPolicies`

``` purescript
newtype BatchListObjectPolicies
  = BatchListObjectPolicies { "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Returns policies attached to an object in pagination fashion inside a <a>BatchRead</a> operation. For more information, see <a>ListObjectPolicies</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchListObjectPolicies _
```

#### `BatchListObjectPoliciesResponse`

``` purescript
newtype BatchListObjectPoliciesResponse
  = BatchListObjectPoliciesResponse { "AttachedPolicyIds" :: NullOrUndefined (ObjectIdentifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListObjectPolicies</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchListObjectPoliciesResponse _
```

#### `BatchListOutgoingTypedLinks`

``` purescript
newtype BatchListOutgoingTypedLinks
  = BatchListOutgoingTypedLinks { "ObjectReference" :: ObjectReference, "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList), "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Returns a paginated list of all the outgoing <a>TypedLinkSpecifier</a> information for an object inside a <a>BatchRead</a> operation. For more information, see <a>ListOutgoingTypedLinks</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchListOutgoingTypedLinks _
```

#### `BatchListOutgoingTypedLinksResponse`

``` purescript
newtype BatchListOutgoingTypedLinksResponse
  = BatchListOutgoingTypedLinksResponse { "TypedLinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListOutgoingTypedLinks</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchListOutgoingTypedLinksResponse _
```

#### `BatchListPolicyAttachments`

``` purescript
newtype BatchListPolicyAttachments
  = BatchListPolicyAttachments { "PolicyReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Returns all of the <code>ObjectIdentifiers</code> to which a given policy is attached inside a <a>BatchRead</a> operation. For more information, see <a>ListPolicyAttachments</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchListPolicyAttachments _
```

#### `BatchListPolicyAttachmentsResponse`

``` purescript
newtype BatchListPolicyAttachmentsResponse
  = BatchListPolicyAttachmentsResponse { "ObjectIdentifiers" :: NullOrUndefined (ObjectIdentifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>ListPolicyAttachments</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchListPolicyAttachmentsResponse _
```

#### `BatchLookupPolicy`

``` purescript
newtype BatchLookupPolicy
  = BatchLookupPolicy { "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

<p>Lists all policies from the root of the Directory to the object specified inside a <a>BatchRead</a> operation. For more information, see <a>LookupPolicy</a> and <a>BatchReadRequest$Operations</a>.</p>

##### Instances
``` purescript
Newtype BatchLookupPolicy _
```

#### `BatchLookupPolicyResponse`

``` purescript
newtype BatchLookupPolicyResponse
  = BatchLookupPolicyResponse { "PolicyToPathList" :: NullOrUndefined (PolicyToPathList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a <a>LookupPolicy</a> response operation.</p>

##### Instances
``` purescript
Newtype BatchLookupPolicyResponse _
```

#### `BatchOperationIndex`

``` purescript
newtype BatchOperationIndex
  = BatchOperationIndex Int
```

##### Instances
``` purescript
Newtype BatchOperationIndex _
```

#### `BatchReadException`

``` purescript
newtype BatchReadException
  = BatchReadException { "Type" :: NullOrUndefined (BatchReadExceptionType), "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The batch read exception structure, which contains the exception type and message.</p>

##### Instances
``` purescript
Newtype BatchReadException _
```

#### `BatchReadExceptionType`

``` purescript
newtype BatchReadExceptionType
  = BatchReadExceptionType String
```

##### Instances
``` purescript
Newtype BatchReadExceptionType _
```

#### `BatchReadOperation`

``` purescript
newtype BatchReadOperation
  = BatchReadOperation { "ListObjectAttributes" :: NullOrUndefined (BatchListObjectAttributes), "ListObjectChildren" :: NullOrUndefined (BatchListObjectChildren), "ListAttachedIndices" :: NullOrUndefined (BatchListAttachedIndices), "ListObjectParentPaths" :: NullOrUndefined (BatchListObjectParentPaths), "GetObjectInformation" :: NullOrUndefined (BatchGetObjectInformation), "ListObjectPolicies" :: NullOrUndefined (BatchListObjectPolicies), "ListPolicyAttachments" :: NullOrUndefined (BatchListPolicyAttachments), "LookupPolicy" :: NullOrUndefined (BatchLookupPolicy), "ListIndex" :: NullOrUndefined (BatchListIndex), "ListOutgoingTypedLinks" :: NullOrUndefined (BatchListOutgoingTypedLinks), "ListIncomingTypedLinks" :: NullOrUndefined (BatchListIncomingTypedLinks) }
```

<p>Represents the output of a <code>BatchRead</code> operation.</p>

##### Instances
``` purescript
Newtype BatchReadOperation _
```

#### `BatchReadOperationList`

``` purescript
newtype BatchReadOperationList
  = BatchReadOperationList (Array BatchReadOperation)
```

##### Instances
``` purescript
Newtype BatchReadOperationList _
```

#### `BatchReadOperationResponse`

``` purescript
newtype BatchReadOperationResponse
  = BatchReadOperationResponse { "SuccessfulResponse" :: NullOrUndefined (BatchReadSuccessfulResponse), "ExceptionResponse" :: NullOrUndefined (BatchReadException) }
```

<p>Represents the output of a <code>BatchRead</code> response operation.</p>

##### Instances
``` purescript
Newtype BatchReadOperationResponse _
```

#### `BatchReadOperationResponseList`

``` purescript
newtype BatchReadOperationResponseList
  = BatchReadOperationResponseList (Array BatchReadOperationResponse)
```

##### Instances
``` purescript
Newtype BatchReadOperationResponseList _
```

#### `BatchReadRequest`

``` purescript
newtype BatchReadRequest
  = BatchReadRequest { "DirectoryArn" :: Arn, "Operations" :: BatchReadOperationList, "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

##### Instances
``` purescript
Newtype BatchReadRequest _
```

#### `BatchReadResponse`

``` purescript
newtype BatchReadResponse
  = BatchReadResponse { "Responses" :: NullOrUndefined (BatchReadOperationResponseList) }
```

##### Instances
``` purescript
Newtype BatchReadResponse _
```

#### `BatchReadSuccessfulResponse`

``` purescript
newtype BatchReadSuccessfulResponse
  = BatchReadSuccessfulResponse { "ListObjectAttributes" :: NullOrUndefined (BatchListObjectAttributesResponse), "ListObjectChildren" :: NullOrUndefined (BatchListObjectChildrenResponse), "GetObjectInformation" :: NullOrUndefined (BatchGetObjectInformationResponse), "ListAttachedIndices" :: NullOrUndefined (BatchListAttachedIndicesResponse), "ListObjectParentPaths" :: NullOrUndefined (BatchListObjectParentPathsResponse), "ListObjectPolicies" :: NullOrUndefined (BatchListObjectPoliciesResponse), "ListPolicyAttachments" :: NullOrUndefined (BatchListPolicyAttachmentsResponse), "LookupPolicy" :: NullOrUndefined (BatchLookupPolicyResponse), "ListIndex" :: NullOrUndefined (BatchListIndexResponse), "ListOutgoingTypedLinks" :: NullOrUndefined (BatchListOutgoingTypedLinksResponse), "ListIncomingTypedLinks" :: NullOrUndefined (BatchListIncomingTypedLinksResponse) }
```

<p>Represents the output of a <code>BatchRead</code> success response operation.</p>

##### Instances
``` purescript
Newtype BatchReadSuccessfulResponse _
```

#### `BatchReferenceName`

``` purescript
newtype BatchReferenceName
  = BatchReferenceName String
```

##### Instances
``` purescript
Newtype BatchReferenceName _
```

#### `BatchRemoveFacetFromObject`

``` purescript
newtype BatchRemoveFacetFromObject
  = BatchRemoveFacetFromObject { "SchemaFacet" :: SchemaFacet, "ObjectReference" :: ObjectReference }
```

<p>A batch operation to remove a facet from an object.</p>

##### Instances
``` purescript
Newtype BatchRemoveFacetFromObject _
```

#### `BatchRemoveFacetFromObjectResponse`

``` purescript
newtype BatchRemoveFacetFromObjectResponse
  = BatchRemoveFacetFromObjectResponse {  }
```

<p>An empty result that represents success.</p>

##### Instances
``` purescript
Newtype BatchRemoveFacetFromObjectResponse _
```

#### `BatchUpdateObjectAttributes`

``` purescript
newtype BatchUpdateObjectAttributes
  = BatchUpdateObjectAttributes { "ObjectReference" :: ObjectReference, "AttributeUpdates" :: ObjectAttributeUpdateList }
```

<p>Represents the output of a <code>BatchUpdate</code> operation. </p>

##### Instances
``` purescript
Newtype BatchUpdateObjectAttributes _
```

#### `BatchUpdateObjectAttributesResponse`

``` purescript
newtype BatchUpdateObjectAttributesResponse
  = BatchUpdateObjectAttributesResponse { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents the output of a <code>BatchUpdate</code> response operation.</p>

##### Instances
``` purescript
Newtype BatchUpdateObjectAttributesResponse _
```

#### `BatchWriteException`

``` purescript
newtype BatchWriteException
  = BatchWriteException { "Index" :: NullOrUndefined (BatchOperationIndex), "Type" :: NullOrUndefined (BatchWriteExceptionType), "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>A <code>BatchWrite</code> exception has occurred.</p>

##### Instances
``` purescript
Newtype BatchWriteException _
```

#### `BatchWriteExceptionType`

``` purescript
newtype BatchWriteExceptionType
  = BatchWriteExceptionType String
```

##### Instances
``` purescript
Newtype BatchWriteExceptionType _
```

#### `BatchWriteOperation`

``` purescript
newtype BatchWriteOperation
  = BatchWriteOperation { "CreateObject" :: NullOrUndefined (BatchCreateObject), "AttachObject" :: NullOrUndefined (BatchAttachObject), "DetachObject" :: NullOrUndefined (BatchDetachObject), "UpdateObjectAttributes" :: NullOrUndefined (BatchUpdateObjectAttributes), "DeleteObject" :: NullOrUndefined (BatchDeleteObject), "AddFacetToObject" :: NullOrUndefined (BatchAddFacetToObject), "RemoveFacetFromObject" :: NullOrUndefined (BatchRemoveFacetFromObject), "AttachPolicy" :: NullOrUndefined (BatchAttachPolicy), "DetachPolicy" :: NullOrUndefined (BatchDetachPolicy), "CreateIndex" :: NullOrUndefined (BatchCreateIndex), "AttachToIndex" :: NullOrUndefined (BatchAttachToIndex), "DetachFromIndex" :: NullOrUndefined (BatchDetachFromIndex), "AttachTypedLink" :: NullOrUndefined (BatchAttachTypedLink), "DetachTypedLink" :: NullOrUndefined (BatchDetachTypedLink) }
```

<p>Represents the output of a <code>BatchWrite</code> operation. </p>

##### Instances
``` purescript
Newtype BatchWriteOperation _
```

#### `BatchWriteOperationList`

``` purescript
newtype BatchWriteOperationList
  = BatchWriteOperationList (Array BatchWriteOperation)
```

##### Instances
``` purescript
Newtype BatchWriteOperationList _
```

#### `BatchWriteOperationResponse`

``` purescript
newtype BatchWriteOperationResponse
  = BatchWriteOperationResponse { "CreateObject" :: NullOrUndefined (BatchCreateObjectResponse), "AttachObject" :: NullOrUndefined (BatchAttachObjectResponse), "DetachObject" :: NullOrUndefined (BatchDetachObjectResponse), "UpdateObjectAttributes" :: NullOrUndefined (BatchUpdateObjectAttributesResponse), "DeleteObject" :: NullOrUndefined (BatchDeleteObjectResponse), "AddFacetToObject" :: NullOrUndefined (BatchAddFacetToObjectResponse), "RemoveFacetFromObject" :: NullOrUndefined (BatchRemoveFacetFromObjectResponse), "AttachPolicy" :: NullOrUndefined (BatchAttachPolicyResponse), "DetachPolicy" :: NullOrUndefined (BatchDetachPolicyResponse), "CreateIndex" :: NullOrUndefined (BatchCreateIndexResponse), "AttachToIndex" :: NullOrUndefined (BatchAttachToIndexResponse), "DetachFromIndex" :: NullOrUndefined (BatchDetachFromIndexResponse), "AttachTypedLink" :: NullOrUndefined (BatchAttachTypedLinkResponse), "DetachTypedLink" :: NullOrUndefined (BatchDetachTypedLinkResponse) }
```

<p>Represents the output of a <code>BatchWrite</code> response operation.</p>

##### Instances
``` purescript
Newtype BatchWriteOperationResponse _
```

#### `BatchWriteOperationResponseList`

``` purescript
newtype BatchWriteOperationResponseList
  = BatchWriteOperationResponseList (Array BatchWriteOperationResponse)
```

##### Instances
``` purescript
Newtype BatchWriteOperationResponseList _
```

#### `BatchWriteRequest`

``` purescript
newtype BatchWriteRequest
  = BatchWriteRequest { "DirectoryArn" :: Arn, "Operations" :: BatchWriteOperationList }
```

##### Instances
``` purescript
Newtype BatchWriteRequest _
```

#### `BatchWriteResponse`

``` purescript
newtype BatchWriteResponse
  = BatchWriteResponse { "Responses" :: NullOrUndefined (BatchWriteOperationResponseList) }
```

##### Instances
``` purescript
Newtype BatchWriteResponse _
```

#### `BinaryAttributeValue`

``` purescript
newtype BinaryAttributeValue
  = BinaryAttributeValue String
```

##### Instances
``` purescript
Newtype BinaryAttributeValue _
```

#### `Bool`

``` purescript
newtype Bool
  = Bool Boolean
```

##### Instances
``` purescript
Newtype Bool _
```

#### `BooleanAttributeValue`

``` purescript
newtype BooleanAttributeValue
  = BooleanAttributeValue Boolean
```

##### Instances
``` purescript
Newtype BooleanAttributeValue _
```

#### `CannotListParentOfRootException`

``` purescript
newtype CannotListParentOfRootException
  = CannotListParentOfRootException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Cannot list the parents of a <a>Directory</a> root.</p>

##### Instances
``` purescript
Newtype CannotListParentOfRootException _
```

#### `ConsistencyLevel`

``` purescript
newtype ConsistencyLevel
  = ConsistencyLevel String
```

##### Instances
``` purescript
Newtype ConsistencyLevel _
```

#### `CreateDirectoryRequest`

``` purescript
newtype CreateDirectoryRequest
  = CreateDirectoryRequest { "Name" :: DirectoryName, "SchemaArn" :: Arn }
```

##### Instances
``` purescript
Newtype CreateDirectoryRequest _
```

#### `CreateDirectoryResponse`

``` purescript
newtype CreateDirectoryResponse
  = CreateDirectoryResponse { "DirectoryArn" :: DirectoryArn, "Name" :: DirectoryName, "ObjectIdentifier" :: ObjectIdentifier, "AppliedSchemaArn" :: Arn }
```

##### Instances
``` purescript
Newtype CreateDirectoryResponse _
```

#### `CreateFacetRequest`

``` purescript
newtype CreateFacetRequest
  = CreateFacetRequest { "SchemaArn" :: Arn, "Name" :: FacetName, "Attributes" :: NullOrUndefined (FacetAttributeList), "ObjectType" :: ObjectType }
```

##### Instances
``` purescript
Newtype CreateFacetRequest _
```

#### `CreateFacetResponse`

``` purescript
newtype CreateFacetResponse
  = CreateFacetResponse {  }
```

##### Instances
``` purescript
Newtype CreateFacetResponse _
```

#### `CreateIndexRequest`

``` purescript
newtype CreateIndexRequest
  = CreateIndexRequest { "DirectoryArn" :: Arn, "OrderedIndexedAttributeList" :: AttributeKeyList, "IsUnique" :: Bool, "ParentReference" :: NullOrUndefined (ObjectReference), "LinkName" :: NullOrUndefined (LinkName) }
```

##### Instances
``` purescript
Newtype CreateIndexRequest _
```

#### `CreateIndexResponse`

``` purescript
newtype CreateIndexResponse
  = CreateIndexResponse { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

##### Instances
``` purescript
Newtype CreateIndexResponse _
```

#### `CreateObjectRequest`

``` purescript
newtype CreateObjectRequest
  = CreateObjectRequest { "DirectoryArn" :: Arn, "SchemaFacets" :: SchemaFacetList, "ObjectAttributeList" :: NullOrUndefined (AttributeKeyAndValueList), "ParentReference" :: NullOrUndefined (ObjectReference), "LinkName" :: NullOrUndefined (LinkName) }
```

##### Instances
``` purescript
Newtype CreateObjectRequest _
```

#### `CreateObjectResponse`

``` purescript
newtype CreateObjectResponse
  = CreateObjectResponse { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

##### Instances
``` purescript
Newtype CreateObjectResponse _
```

#### `CreateSchemaRequest`

``` purescript
newtype CreateSchemaRequest
  = CreateSchemaRequest { "Name" :: SchemaName }
```

##### Instances
``` purescript
Newtype CreateSchemaRequest _
```

#### `CreateSchemaResponse`

``` purescript
newtype CreateSchemaResponse
  = CreateSchemaResponse { "SchemaArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype CreateSchemaResponse _
```

#### `CreateTypedLinkFacetRequest`

``` purescript
newtype CreateTypedLinkFacetRequest
  = CreateTypedLinkFacetRequest { "SchemaArn" :: Arn, "Facet" :: TypedLinkFacet }
```

##### Instances
``` purescript
Newtype CreateTypedLinkFacetRequest _
```

#### `CreateTypedLinkFacetResponse`

``` purescript
newtype CreateTypedLinkFacetResponse
  = CreateTypedLinkFacetResponse {  }
```

##### Instances
``` purescript
Newtype CreateTypedLinkFacetResponse _
```

#### `Date`

``` purescript
newtype Date
  = Date Number
```

##### Instances
``` purescript
Newtype Date _
```

#### `DatetimeAttributeValue`

``` purescript
newtype DatetimeAttributeValue
  = DatetimeAttributeValue Number
```

##### Instances
``` purescript
Newtype DatetimeAttributeValue _
```

#### `DeleteDirectoryRequest`

``` purescript
newtype DeleteDirectoryRequest
  = DeleteDirectoryRequest { "DirectoryArn" :: Arn }
```

##### Instances
``` purescript
Newtype DeleteDirectoryRequest _
```

#### `DeleteDirectoryResponse`

``` purescript
newtype DeleteDirectoryResponse
  = DeleteDirectoryResponse { "DirectoryArn" :: Arn }
```

##### Instances
``` purescript
Newtype DeleteDirectoryResponse _
```

#### `DeleteFacetRequest`

``` purescript
newtype DeleteFacetRequest
  = DeleteFacetRequest { "SchemaArn" :: Arn, "Name" :: FacetName }
```

##### Instances
``` purescript
Newtype DeleteFacetRequest _
```

#### `DeleteFacetResponse`

``` purescript
newtype DeleteFacetResponse
  = DeleteFacetResponse {  }
```

##### Instances
``` purescript
Newtype DeleteFacetResponse _
```

#### `DeleteObjectRequest`

``` purescript
newtype DeleteObjectRequest
  = DeleteObjectRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference }
```

##### Instances
``` purescript
Newtype DeleteObjectRequest _
```

#### `DeleteObjectResponse`

``` purescript
newtype DeleteObjectResponse
  = DeleteObjectResponse {  }
```

##### Instances
``` purescript
Newtype DeleteObjectResponse _
```

#### `DeleteSchemaRequest`

``` purescript
newtype DeleteSchemaRequest
  = DeleteSchemaRequest { "SchemaArn" :: Arn }
```

##### Instances
``` purescript
Newtype DeleteSchemaRequest _
```

#### `DeleteSchemaResponse`

``` purescript
newtype DeleteSchemaResponse
  = DeleteSchemaResponse { "SchemaArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype DeleteSchemaResponse _
```

#### `DeleteTypedLinkFacetRequest`

``` purescript
newtype DeleteTypedLinkFacetRequest
  = DeleteTypedLinkFacetRequest { "SchemaArn" :: Arn, "Name" :: TypedLinkName }
```

##### Instances
``` purescript
Newtype DeleteTypedLinkFacetRequest _
```

#### `DeleteTypedLinkFacetResponse`

``` purescript
newtype DeleteTypedLinkFacetResponse
  = DeleteTypedLinkFacetResponse {  }
```

##### Instances
``` purescript
Newtype DeleteTypedLinkFacetResponse _
```

#### `DetachFromIndexRequest`

``` purescript
newtype DetachFromIndexRequest
  = DetachFromIndexRequest { "DirectoryArn" :: Arn, "IndexReference" :: ObjectReference, "TargetReference" :: ObjectReference }
```

##### Instances
``` purescript
Newtype DetachFromIndexRequest _
```

#### `DetachFromIndexResponse`

``` purescript
newtype DetachFromIndexResponse
  = DetachFromIndexResponse { "DetachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

##### Instances
``` purescript
Newtype DetachFromIndexResponse _
```

#### `DetachObjectRequest`

``` purescript
newtype DetachObjectRequest
  = DetachObjectRequest { "DirectoryArn" :: Arn, "ParentReference" :: ObjectReference, "LinkName" :: LinkName }
```

##### Instances
``` purescript
Newtype DetachObjectRequest _
```

#### `DetachObjectResponse`

``` purescript
newtype DetachObjectResponse
  = DetachObjectResponse { "DetachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

##### Instances
``` purescript
Newtype DetachObjectResponse _
```

#### `DetachPolicyRequest`

``` purescript
newtype DetachPolicyRequest
  = DetachPolicyRequest { "DirectoryArn" :: Arn, "PolicyReference" :: ObjectReference, "ObjectReference" :: ObjectReference }
```

##### Instances
``` purescript
Newtype DetachPolicyRequest _
```

#### `DetachPolicyResponse`

``` purescript
newtype DetachPolicyResponse
  = DetachPolicyResponse {  }
```

##### Instances
``` purescript
Newtype DetachPolicyResponse _
```

#### `DetachTypedLinkRequest`

``` purescript
newtype DetachTypedLinkRequest
  = DetachTypedLinkRequest { "DirectoryArn" :: Arn, "TypedLinkSpecifier" :: TypedLinkSpecifier }
```

##### Instances
``` purescript
Newtype DetachTypedLinkRequest _
```

#### `Directory`

``` purescript
newtype Directory
  = Directory { "Name" :: NullOrUndefined (DirectoryName), "DirectoryArn" :: NullOrUndefined (DirectoryArn), "State" :: NullOrUndefined (DirectoryState), "CreationDateTime" :: NullOrUndefined (Date) }
```

<p>Directory structure that includes the directory name and directory ARN.</p>

##### Instances
``` purescript
Newtype Directory _
```

#### `DirectoryAlreadyExistsException`

``` purescript
newtype DirectoryAlreadyExistsException
  = DirectoryAlreadyExistsException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that a <a>Directory</a> could not be created due to a naming conflict. Choose a different name and try again.</p>

##### Instances
``` purescript
Newtype DirectoryAlreadyExistsException _
```

#### `DirectoryArn`

``` purescript
newtype DirectoryArn
  = DirectoryArn String
```

##### Instances
``` purescript
Newtype DirectoryArn _
```

#### `DirectoryDeletedException`

``` purescript
newtype DirectoryDeletedException
  = DirectoryDeletedException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>A directory that has been deleted and to which access has been attempted. Note: The requested resource will eventually cease to exist.</p>

##### Instances
``` purescript
Newtype DirectoryDeletedException _
```

#### `DirectoryList`

``` purescript
newtype DirectoryList
  = DirectoryList (Array Directory)
```

##### Instances
``` purescript
Newtype DirectoryList _
```

#### `DirectoryName`

``` purescript
newtype DirectoryName
  = DirectoryName String
```

##### Instances
``` purescript
Newtype DirectoryName _
```

#### `DirectoryNotDisabledException`

``` purescript
newtype DirectoryNotDisabledException
  = DirectoryNotDisabledException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>An operation can only operate on a disabled directory.</p>

##### Instances
``` purescript
Newtype DirectoryNotDisabledException _
```

#### `DirectoryNotEnabledException`

``` purescript
newtype DirectoryNotEnabledException
  = DirectoryNotEnabledException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>An operation can only operate on a directory that is not enabled.</p>

##### Instances
``` purescript
Newtype DirectoryNotEnabledException _
```

#### `DirectoryState`

``` purescript
newtype DirectoryState
  = DirectoryState String
```

##### Instances
``` purescript
Newtype DirectoryState _
```

#### `DisableDirectoryRequest`

``` purescript
newtype DisableDirectoryRequest
  = DisableDirectoryRequest { "DirectoryArn" :: Arn }
```

##### Instances
``` purescript
Newtype DisableDirectoryRequest _
```

#### `DisableDirectoryResponse`

``` purescript
newtype DisableDirectoryResponse
  = DisableDirectoryResponse { "DirectoryArn" :: Arn }
```

##### Instances
``` purescript
Newtype DisableDirectoryResponse _
```

#### `EnableDirectoryRequest`

``` purescript
newtype EnableDirectoryRequest
  = EnableDirectoryRequest { "DirectoryArn" :: Arn }
```

##### Instances
``` purescript
Newtype EnableDirectoryRequest _
```

#### `EnableDirectoryResponse`

``` purescript
newtype EnableDirectoryResponse
  = EnableDirectoryResponse { "DirectoryArn" :: Arn }
```

##### Instances
``` purescript
Newtype EnableDirectoryResponse _
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

##### Instances
``` purescript
Newtype ExceptionMessage _
```

#### `Facet`

``` purescript
newtype Facet
  = Facet { "Name" :: NullOrUndefined (FacetName), "ObjectType" :: NullOrUndefined (ObjectType) }
```

<p>A structure that contains <code>Name</code>, <code>ARN</code>, <code>Attributes</code>, <a>Rule</a>s, and <code>ObjectTypes</code>.</p>

##### Instances
``` purescript
Newtype Facet _
```

#### `FacetAlreadyExistsException`

``` purescript
newtype FacetAlreadyExistsException
  = FacetAlreadyExistsException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>A facet with the same name already exists.</p>

##### Instances
``` purescript
Newtype FacetAlreadyExistsException _
```

#### `FacetAttribute`

``` purescript
newtype FacetAttribute
  = FacetAttribute { "Name" :: AttributeName, "AttributeDefinition" :: NullOrUndefined (FacetAttributeDefinition), "AttributeReference" :: NullOrUndefined (FacetAttributeReference), "RequiredBehavior" :: NullOrUndefined (RequiredAttributeBehavior) }
```

<p>An attribute that is associated with the <a>Facet</a>.</p>

##### Instances
``` purescript
Newtype FacetAttribute _
```

#### `FacetAttributeDefinition`

``` purescript
newtype FacetAttributeDefinition
  = FacetAttributeDefinition { "Type" :: FacetAttributeType, "DefaultValue" :: NullOrUndefined (TypedAttributeValue), "IsImmutable" :: NullOrUndefined (Bool), "Rules" :: NullOrUndefined (RuleMap) }
```

<p>A facet attribute definition. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences">Attribute References</a> for more information.</p>

##### Instances
``` purescript
Newtype FacetAttributeDefinition _
```

#### `FacetAttributeList`

``` purescript
newtype FacetAttributeList
  = FacetAttributeList (Array FacetAttribute)
```

##### Instances
``` purescript
Newtype FacetAttributeList _
```

#### `FacetAttributeReference`

``` purescript
newtype FacetAttributeReference
  = FacetAttributeReference { "TargetFacetName" :: FacetName, "TargetAttributeName" :: AttributeName }
```

<p>The facet attribute reference that specifies the attribute definition that contains the attribute facet name and attribute name.</p>

##### Instances
``` purescript
Newtype FacetAttributeReference _
```

#### `FacetAttributeType`

``` purescript
newtype FacetAttributeType
  = FacetAttributeType String
```

##### Instances
``` purescript
Newtype FacetAttributeType _
```

#### `FacetAttributeUpdate`

``` purescript
newtype FacetAttributeUpdate
  = FacetAttributeUpdate { "Attribute" :: NullOrUndefined (FacetAttribute), "Action" :: NullOrUndefined (UpdateActionType) }
```

<p>A structure that contains information used to update an attribute.</p>

##### Instances
``` purescript
Newtype FacetAttributeUpdate _
```

#### `FacetAttributeUpdateList`

``` purescript
newtype FacetAttributeUpdateList
  = FacetAttributeUpdateList (Array FacetAttributeUpdate)
```

##### Instances
``` purescript
Newtype FacetAttributeUpdateList _
```

#### `FacetInUseException`

``` purescript
newtype FacetInUseException
  = FacetInUseException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Occurs when deleting a facet that contains an attribute that is a target to an attribute reference in a different facet.</p>

##### Instances
``` purescript
Newtype FacetInUseException _
```

#### `FacetName`

``` purescript
newtype FacetName
  = FacetName String
```

##### Instances
``` purescript
Newtype FacetName _
```

#### `FacetNameList`

``` purescript
newtype FacetNameList
  = FacetNameList (Array FacetName)
```

##### Instances
``` purescript
Newtype FacetNameList _
```

#### `FacetNotFoundException`

``` purescript
newtype FacetNotFoundException
  = FacetNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified <a>Facet</a> could not be found.</p>

##### Instances
``` purescript
Newtype FacetNotFoundException _
```

#### `FacetValidationException`

``` purescript
newtype FacetValidationException
  = FacetValidationException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The <a>Facet</a> that you provided was not well formed or could not be validated with the schema.</p>

##### Instances
``` purescript
Newtype FacetValidationException _
```

#### `GetAppliedSchemaVersionRequest`

``` purescript
newtype GetAppliedSchemaVersionRequest
  = GetAppliedSchemaVersionRequest { "SchemaArn" :: Arn }
```

##### Instances
``` purescript
Newtype GetAppliedSchemaVersionRequest _
```

#### `GetAppliedSchemaVersionResponse`

``` purescript
newtype GetAppliedSchemaVersionResponse
  = GetAppliedSchemaVersionResponse { "AppliedSchemaArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype GetAppliedSchemaVersionResponse _
```

#### `GetDirectoryRequest`

``` purescript
newtype GetDirectoryRequest
  = GetDirectoryRequest { "DirectoryArn" :: DirectoryArn }
```

##### Instances
``` purescript
Newtype GetDirectoryRequest _
```

#### `GetDirectoryResponse`

``` purescript
newtype GetDirectoryResponse
  = GetDirectoryResponse { "Directory" :: Directory }
```

##### Instances
``` purescript
Newtype GetDirectoryResponse _
```

#### `GetFacetRequest`

``` purescript
newtype GetFacetRequest
  = GetFacetRequest { "SchemaArn" :: Arn, "Name" :: FacetName }
```

##### Instances
``` purescript
Newtype GetFacetRequest _
```

#### `GetFacetResponse`

``` purescript
newtype GetFacetResponse
  = GetFacetResponse { "Facet" :: NullOrUndefined (Facet) }
```

##### Instances
``` purescript
Newtype GetFacetResponse _
```

#### `GetObjectInformationRequest`

``` purescript
newtype GetObjectInformationRequest
  = GetObjectInformationRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

##### Instances
``` purescript
Newtype GetObjectInformationRequest _
```

#### `GetObjectInformationResponse`

``` purescript
newtype GetObjectInformationResponse
  = GetObjectInformationResponse { "SchemaFacets" :: NullOrUndefined (SchemaFacetList), "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

##### Instances
``` purescript
Newtype GetObjectInformationResponse _
```

#### `GetSchemaAsJsonRequest`

``` purescript
newtype GetSchemaAsJsonRequest
  = GetSchemaAsJsonRequest { "SchemaArn" :: Arn }
```

##### Instances
``` purescript
Newtype GetSchemaAsJsonRequest _
```

#### `GetSchemaAsJsonResponse`

``` purescript
newtype GetSchemaAsJsonResponse
  = GetSchemaAsJsonResponse { "Name" :: NullOrUndefined (SchemaName), "Document" :: NullOrUndefined (SchemaJsonDocument) }
```

##### Instances
``` purescript
Newtype GetSchemaAsJsonResponse _
```

#### `GetTypedLinkFacetInformationRequest`

``` purescript
newtype GetTypedLinkFacetInformationRequest
  = GetTypedLinkFacetInformationRequest { "SchemaArn" :: Arn, "Name" :: TypedLinkName }
```

##### Instances
``` purescript
Newtype GetTypedLinkFacetInformationRequest _
```

#### `GetTypedLinkFacetInformationResponse`

``` purescript
newtype GetTypedLinkFacetInformationResponse
  = GetTypedLinkFacetInformationResponse { "IdentityAttributeOrder" :: NullOrUndefined (AttributeNameList) }
```

##### Instances
``` purescript
Newtype GetTypedLinkFacetInformationResponse _
```

#### `IncompatibleSchemaException`

``` purescript
newtype IncompatibleSchemaException
  = IncompatibleSchemaException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates a failure occurred while performing a check for backward compatibility between the specified schema and the schema that is currently applied to the directory.</p>

##### Instances
``` purescript
Newtype IncompatibleSchemaException _
```

#### `IndexAttachment`

``` purescript
newtype IndexAttachment
  = IndexAttachment { "IndexedAttributes" :: NullOrUndefined (AttributeKeyAndValueList), "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

<p>Represents an index and an attached object.</p>

##### Instances
``` purescript
Newtype IndexAttachment _
```

#### `IndexAttachmentList`

``` purescript
newtype IndexAttachmentList
  = IndexAttachmentList (Array IndexAttachment)
```

##### Instances
``` purescript
Newtype IndexAttachmentList _
```

#### `IndexedAttributeMissingException`

``` purescript
newtype IndexedAttributeMissingException
  = IndexedAttributeMissingException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>An object has been attempted to be attached to an object that does not have the appropriate attribute value.</p>

##### Instances
``` purescript
Newtype IndexedAttributeMissingException _
```

#### `InternalServiceException`

``` purescript
newtype InternalServiceException
  = InternalServiceException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates a problem that must be resolved by Amazon Web Services. This might be a transient error in which case you can retry your request until it succeeds. Otherwise, go to the <a href="http://status.aws.amazon.com/">AWS Service Health Dashboard</a> site to see if there are any operational issues with the service.</p>

##### Instances
``` purescript
Newtype InternalServiceException _
```

#### `InvalidArnException`

``` purescript
newtype InvalidArnException
  = InvalidArnException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the provided ARN value is not valid.</p>

##### Instances
``` purescript
Newtype InvalidArnException _
```

#### `InvalidAttachmentException`

``` purescript
newtype InvalidAttachmentException
  = InvalidAttachmentException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that an attempt to attach an object with the same link name or to apply a schema with the same name has occurred. Rename the link or the schema and then try again.</p>

##### Instances
``` purescript
Newtype InvalidAttachmentException _
```

#### `InvalidFacetUpdateException`

``` purescript
newtype InvalidFacetUpdateException
  = InvalidFacetUpdateException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>An attempt to modify a <a>Facet</a> resulted in an invalid schema exception.</p>

##### Instances
``` purescript
Newtype InvalidFacetUpdateException _
```

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the <code>NextToken</code> value is not valid.</p>

##### Instances
``` purescript
Newtype InvalidNextTokenException _
```

#### `InvalidRuleException`

``` purescript
newtype InvalidRuleException
  = InvalidRuleException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Occurs when any of the rule parameter keys or values are invalid.</p>

##### Instances
``` purescript
Newtype InvalidRuleException _
```

#### `InvalidSchemaDocException`

``` purescript
newtype InvalidSchemaDocException
  = InvalidSchemaDocException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the provided <code>SchemaDoc</code> value is not valid.</p>

##### Instances
``` purescript
Newtype InvalidSchemaDocException _
```

#### `InvalidTaggingRequestException`

``` purescript
newtype InvalidTaggingRequestException
  = InvalidTaggingRequestException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Can occur for multiple reasons such as when you tag a resource that doesn’t exist or if you specify a higher number of tags for a resource than the allowed limit. Allowed limit is 50 tags per resource.</p>

##### Instances
``` purescript
Newtype InvalidTaggingRequestException _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that limits are exceeded. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/limits.html">Limits</a> for more information.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `LinkName`

``` purescript
newtype LinkName
  = LinkName String
```

##### Instances
``` purescript
Newtype LinkName _
```

#### `LinkNameAlreadyInUseException`

``` purescript
newtype LinkNameAlreadyInUseException
  = LinkNameAlreadyInUseException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that a link could not be created due to a naming conflict. Choose a different name and then try again.</p>

##### Instances
``` purescript
Newtype LinkNameAlreadyInUseException _
```

#### `LinkNameToObjectIdentifierMap`

``` purescript
newtype LinkNameToObjectIdentifierMap
  = LinkNameToObjectIdentifierMap (Map LinkName ObjectIdentifier)
```

##### Instances
``` purescript
Newtype LinkNameToObjectIdentifierMap _
```

#### `ListAppliedSchemaArnsRequest`

``` purescript
newtype ListAppliedSchemaArnsRequest
  = ListAppliedSchemaArnsRequest { "DirectoryArn" :: Arn, "SchemaArn" :: NullOrUndefined (Arn), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

##### Instances
``` purescript
Newtype ListAppliedSchemaArnsRequest _
```

#### `ListAppliedSchemaArnsResponse`

``` purescript
newtype ListAppliedSchemaArnsResponse
  = ListAppliedSchemaArnsResponse { "SchemaArns" :: NullOrUndefined (Arns), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListAppliedSchemaArnsResponse _
```

#### `ListAttachedIndicesRequest`

``` purescript
newtype ListAttachedIndicesRequest
  = ListAttachedIndicesRequest { "DirectoryArn" :: Arn, "TargetReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

##### Instances
``` purescript
Newtype ListAttachedIndicesRequest _
```

#### `ListAttachedIndicesResponse`

``` purescript
newtype ListAttachedIndicesResponse
  = ListAttachedIndicesResponse { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListAttachedIndicesResponse _
```

#### `ListDevelopmentSchemaArnsRequest`

``` purescript
newtype ListDevelopmentSchemaArnsRequest
  = ListDevelopmentSchemaArnsRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

##### Instances
``` purescript
Newtype ListDevelopmentSchemaArnsRequest _
```

#### `ListDevelopmentSchemaArnsResponse`

``` purescript
newtype ListDevelopmentSchemaArnsResponse
  = ListDevelopmentSchemaArnsResponse { "SchemaArns" :: NullOrUndefined (Arns), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListDevelopmentSchemaArnsResponse _
```

#### `ListDirectoriesRequest`

``` purescript
newtype ListDirectoriesRequest
  = ListDirectoriesRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "State'" :: NullOrUndefined (DirectoryState) }
```

##### Instances
``` purescript
Newtype ListDirectoriesRequest _
```

#### `ListDirectoriesResponse`

``` purescript
newtype ListDirectoriesResponse
  = ListDirectoriesResponse { "Directories" :: DirectoryList, "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListDirectoriesResponse _
```

#### `ListFacetAttributesRequest`

``` purescript
newtype ListFacetAttributesRequest
  = ListFacetAttributesRequest { "SchemaArn" :: Arn, "Name" :: FacetName, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

##### Instances
``` purescript
Newtype ListFacetAttributesRequest _
```

#### `ListFacetAttributesResponse`

``` purescript
newtype ListFacetAttributesResponse
  = ListFacetAttributesResponse { "Attributes" :: NullOrUndefined (FacetAttributeList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListFacetAttributesResponse _
```

#### `ListFacetNamesRequest`

``` purescript
newtype ListFacetNamesRequest
  = ListFacetNamesRequest { "SchemaArn" :: Arn, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

##### Instances
``` purescript
Newtype ListFacetNamesRequest _
```

#### `ListFacetNamesResponse`

``` purescript
newtype ListFacetNamesResponse
  = ListFacetNamesResponse { "FacetNames" :: NullOrUndefined (FacetNameList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListFacetNamesResponse _
```

#### `ListIncomingTypedLinksRequest`

``` purescript
newtype ListIncomingTypedLinksRequest
  = ListIncomingTypedLinksRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList), "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

##### Instances
``` purescript
Newtype ListIncomingTypedLinksRequest _
```

#### `ListIncomingTypedLinksResponse`

``` purescript
newtype ListIncomingTypedLinksResponse
  = ListIncomingTypedLinksResponse { "LinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListIncomingTypedLinksResponse _
```

#### `ListIndexRequest`

``` purescript
newtype ListIndexRequest
  = ListIndexRequest { "DirectoryArn" :: Arn, "RangesOnIndexedValues" :: NullOrUndefined (ObjectAttributeRangeList), "IndexReference" :: ObjectReference, "MaxResults" :: NullOrUndefined (NumberResults), "NextToken" :: NullOrUndefined (NextToken), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

##### Instances
``` purescript
Newtype ListIndexRequest _
```

#### `ListIndexResponse`

``` purescript
newtype ListIndexResponse
  = ListIndexResponse { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListIndexResponse _
```

#### `ListObjectAttributesRequest`

``` purescript
newtype ListObjectAttributesRequest
  = ListObjectAttributesRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel), "FacetFilter" :: NullOrUndefined (SchemaFacet) }
```

##### Instances
``` purescript
Newtype ListObjectAttributesRequest _
```

#### `ListObjectAttributesResponse`

``` purescript
newtype ListObjectAttributesResponse
  = ListObjectAttributesResponse { "Attributes" :: NullOrUndefined (AttributeKeyAndValueList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListObjectAttributesResponse _
```

#### `ListObjectChildrenRequest`

``` purescript
newtype ListObjectChildrenRequest
  = ListObjectChildrenRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

##### Instances
``` purescript
Newtype ListObjectChildrenRequest _
```

#### `ListObjectChildrenResponse`

``` purescript
newtype ListObjectChildrenResponse
  = ListObjectChildrenResponse { "Children" :: NullOrUndefined (LinkNameToObjectIdentifierMap), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListObjectChildrenResponse _
```

#### `ListObjectParentPathsRequest`

``` purescript
newtype ListObjectParentPathsRequest
  = ListObjectParentPathsRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

##### Instances
``` purescript
Newtype ListObjectParentPathsRequest _
```

#### `ListObjectParentPathsResponse`

``` purescript
newtype ListObjectParentPathsResponse
  = ListObjectParentPathsResponse { "PathToObjectIdentifiersList" :: NullOrUndefined (PathToObjectIdentifiersList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListObjectParentPathsResponse _
```

#### `ListObjectParentsRequest`

``` purescript
newtype ListObjectParentsRequest
  = ListObjectParentsRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

##### Instances
``` purescript
Newtype ListObjectParentsRequest _
```

#### `ListObjectParentsResponse`

``` purescript
newtype ListObjectParentsResponse
  = ListObjectParentsResponse { "Parents" :: NullOrUndefined (ObjectIdentifierToLinkNameMap), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListObjectParentsResponse _
```

#### `ListObjectPoliciesRequest`

``` purescript
newtype ListObjectPoliciesRequest
  = ListObjectPoliciesRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

##### Instances
``` purescript
Newtype ListObjectPoliciesRequest _
```

#### `ListObjectPoliciesResponse`

``` purescript
newtype ListObjectPoliciesResponse
  = ListObjectPoliciesResponse { "AttachedPolicyIds" :: NullOrUndefined (ObjectIdentifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListObjectPoliciesResponse _
```

#### `ListOutgoingTypedLinksRequest`

``` purescript
newtype ListOutgoingTypedLinksRequest
  = ListOutgoingTypedLinksRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList), "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

##### Instances
``` purescript
Newtype ListOutgoingTypedLinksRequest _
```

#### `ListOutgoingTypedLinksResponse`

``` purescript
newtype ListOutgoingTypedLinksResponse
  = ListOutgoingTypedLinksResponse { "TypedLinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListOutgoingTypedLinksResponse _
```

#### `ListPolicyAttachmentsRequest`

``` purescript
newtype ListPolicyAttachmentsRequest
  = ListPolicyAttachmentsRequest { "DirectoryArn" :: Arn, "PolicyReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults), "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel) }
```

##### Instances
``` purescript
Newtype ListPolicyAttachmentsRequest _
```

#### `ListPolicyAttachmentsResponse`

``` purescript
newtype ListPolicyAttachmentsResponse
  = ListPolicyAttachmentsResponse { "ObjectIdentifiers" :: NullOrUndefined (ObjectIdentifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListPolicyAttachmentsResponse _
```

#### `ListPublishedSchemaArnsRequest`

``` purescript
newtype ListPublishedSchemaArnsRequest
  = ListPublishedSchemaArnsRequest { "SchemaArn" :: NullOrUndefined (Arn), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

##### Instances
``` purescript
Newtype ListPublishedSchemaArnsRequest _
```

#### `ListPublishedSchemaArnsResponse`

``` purescript
newtype ListPublishedSchemaArnsResponse
  = ListPublishedSchemaArnsResponse { "SchemaArns" :: NullOrUndefined (Arns), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListPublishedSchemaArnsResponse _
```

#### `ListTagsForResourceRequest`

``` purescript
newtype ListTagsForResourceRequest
  = ListTagsForResourceRequest { "ResourceArn" :: Arn, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (TagsNumberResults) }
```

##### Instances
``` purescript
Newtype ListTagsForResourceRequest _
```

#### `ListTagsForResourceResponse`

``` purescript
newtype ListTagsForResourceResponse
  = ListTagsForResourceResponse { "Tags" :: NullOrUndefined (TagList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListTagsForResourceResponse _
```

#### `ListTypedLinkFacetAttributesRequest`

``` purescript
newtype ListTypedLinkFacetAttributesRequest
  = ListTypedLinkFacetAttributesRequest { "SchemaArn" :: Arn, "Name" :: TypedLinkName, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

##### Instances
``` purescript
Newtype ListTypedLinkFacetAttributesRequest _
```

#### `ListTypedLinkFacetAttributesResponse`

``` purescript
newtype ListTypedLinkFacetAttributesResponse
  = ListTypedLinkFacetAttributesResponse { "Attributes" :: NullOrUndefined (TypedLinkAttributeDefinitionList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListTypedLinkFacetAttributesResponse _
```

#### `ListTypedLinkFacetNamesRequest`

``` purescript
newtype ListTypedLinkFacetNamesRequest
  = ListTypedLinkFacetNamesRequest { "SchemaArn" :: Arn, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

##### Instances
``` purescript
Newtype ListTypedLinkFacetNamesRequest _
```

#### `ListTypedLinkFacetNamesResponse`

``` purescript
newtype ListTypedLinkFacetNamesResponse
  = ListTypedLinkFacetNamesResponse { "FacetNames" :: NullOrUndefined (TypedLinkNameList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListTypedLinkFacetNamesResponse _
```

#### `LookupPolicyRequest`

``` purescript
newtype LookupPolicyRequest
  = LookupPolicyRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (NumberResults) }
```

##### Instances
``` purescript
Newtype LookupPolicyRequest _
```

#### `LookupPolicyResponse`

``` purescript
newtype LookupPolicyResponse
  = LookupPolicyResponse { "PolicyToPathList" :: NullOrUndefined (PolicyToPathList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype LookupPolicyResponse _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

##### Instances
``` purescript
Newtype NextToken _
```

#### `NotIndexException`

``` purescript
newtype NotIndexException
  = NotIndexException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the requested operation can only operate on index objects.</p>

##### Instances
``` purescript
Newtype NotIndexException _
```

#### `NotNodeException`

``` purescript
newtype NotNodeException
  = NotNodeException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Occurs when any invalid operations are performed on an object that is not a node, such as calling <code>ListObjectChildren</code> for a leaf node object.</p>

##### Instances
``` purescript
Newtype NotNodeException _
```

#### `NotPolicyException`

``` purescript
newtype NotPolicyException
  = NotPolicyException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the requested operation can only operate on policy objects.</p>

##### Instances
``` purescript
Newtype NotPolicyException _
```

#### `NumberAttributeValue`

``` purescript
newtype NumberAttributeValue
  = NumberAttributeValue String
```

##### Instances
``` purescript
Newtype NumberAttributeValue _
```

#### `NumberResults`

``` purescript
newtype NumberResults
  = NumberResults Int
```

##### Instances
``` purescript
Newtype NumberResults _
```

#### `ObjectAlreadyDetachedException`

``` purescript
newtype ObjectAlreadyDetachedException
  = ObjectAlreadyDetachedException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the object is not attached to the index.</p>

##### Instances
``` purescript
Newtype ObjectAlreadyDetachedException _
```

#### `ObjectAttributeAction`

``` purescript
newtype ObjectAttributeAction
  = ObjectAttributeAction { "ObjectAttributeActionType" :: NullOrUndefined (UpdateActionType), "ObjectAttributeUpdateValue" :: NullOrUndefined (TypedAttributeValue) }
```

<p>The action to take on the object attribute.</p>

##### Instances
``` purescript
Newtype ObjectAttributeAction _
```

#### `ObjectAttributeRange`

``` purescript
newtype ObjectAttributeRange
  = ObjectAttributeRange { "AttributeKey" :: NullOrUndefined (AttributeKey), "Range" :: NullOrUndefined (TypedAttributeValueRange) }
```

<p>A range of attributes.</p>

##### Instances
``` purescript
Newtype ObjectAttributeRange _
```

#### `ObjectAttributeRangeList`

``` purescript
newtype ObjectAttributeRangeList
  = ObjectAttributeRangeList (Array ObjectAttributeRange)
```

##### Instances
``` purescript
Newtype ObjectAttributeRangeList _
```

#### `ObjectAttributeUpdate`

``` purescript
newtype ObjectAttributeUpdate
  = ObjectAttributeUpdate { "ObjectAttributeKey" :: NullOrUndefined (AttributeKey), "ObjectAttributeAction" :: NullOrUndefined (ObjectAttributeAction) }
```

<p>Structure that contains attribute update information.</p>

##### Instances
``` purescript
Newtype ObjectAttributeUpdate _
```

#### `ObjectAttributeUpdateList`

``` purescript
newtype ObjectAttributeUpdateList
  = ObjectAttributeUpdateList (Array ObjectAttributeUpdate)
```

##### Instances
``` purescript
Newtype ObjectAttributeUpdateList _
```

#### `ObjectIdentifier`

``` purescript
newtype ObjectIdentifier
  = ObjectIdentifier String
```

##### Instances
``` purescript
Newtype ObjectIdentifier _
```

#### `ObjectIdentifierList`

``` purescript
newtype ObjectIdentifierList
  = ObjectIdentifierList (Array ObjectIdentifier)
```

##### Instances
``` purescript
Newtype ObjectIdentifierList _
```

#### `ObjectIdentifierToLinkNameMap`

``` purescript
newtype ObjectIdentifierToLinkNameMap
  = ObjectIdentifierToLinkNameMap (Map ObjectIdentifier LinkName)
```

##### Instances
``` purescript
Newtype ObjectIdentifierToLinkNameMap _
```

#### `ObjectNotDetachedException`

``` purescript
newtype ObjectNotDetachedException
  = ObjectNotDetachedException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the requested operation cannot be completed because the object has not been detached from the tree.</p>

##### Instances
``` purescript
Newtype ObjectNotDetachedException _
```

#### `ObjectReference`

``` purescript
newtype ObjectReference
  = ObjectReference { "Selector" :: NullOrUndefined (SelectorObjectReference) }
```

<p>The reference that identifies an object.</p>

##### Instances
``` purescript
Newtype ObjectReference _
```

#### `ObjectType`

``` purescript
newtype ObjectType
  = ObjectType String
```

##### Instances
``` purescript
Newtype ObjectType _
```

#### `PathString`

``` purescript
newtype PathString
  = PathString String
```

##### Instances
``` purescript
Newtype PathString _
```

#### `PathToObjectIdentifiers`

``` purescript
newtype PathToObjectIdentifiers
  = PathToObjectIdentifiers { "Path" :: NullOrUndefined (PathString), "ObjectIdentifiers" :: NullOrUndefined (ObjectIdentifierList) }
```

<p>Returns the path to the <code>ObjectIdentifiers</code> that is associated with the directory.</p>

##### Instances
``` purescript
Newtype PathToObjectIdentifiers _
```

#### `PathToObjectIdentifiersList`

``` purescript
newtype PathToObjectIdentifiersList
  = PathToObjectIdentifiersList (Array PathToObjectIdentifiers)
```

##### Instances
``` purescript
Newtype PathToObjectIdentifiersList _
```

#### `PolicyAttachment`

``` purescript
newtype PolicyAttachment
  = PolicyAttachment { "PolicyId" :: NullOrUndefined (ObjectIdentifier), "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier), "PolicyType" :: NullOrUndefined (PolicyType) }
```

<p>Contains the <code>PolicyType</code>, <code>PolicyId</code>, and the <code>ObjectIdentifier</code> to which it is attached. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies">Policies</a>.</p>

##### Instances
``` purescript
Newtype PolicyAttachment _
```

#### `PolicyAttachmentList`

``` purescript
newtype PolicyAttachmentList
  = PolicyAttachmentList (Array PolicyAttachment)
```

##### Instances
``` purescript
Newtype PolicyAttachmentList _
```

#### `PolicyToPath`

``` purescript
newtype PolicyToPath
  = PolicyToPath { "Path" :: NullOrUndefined (PathString), "Policies" :: NullOrUndefined (PolicyAttachmentList) }
```

<p>Used when a regular object exists in a <a>Directory</a> and you want to find all of the policies that are associated with that object and the parent to that object.</p>

##### Instances
``` purescript
Newtype PolicyToPath _
```

#### `PolicyToPathList`

``` purescript
newtype PolicyToPathList
  = PolicyToPathList (Array PolicyToPath)
```

##### Instances
``` purescript
Newtype PolicyToPathList _
```

#### `PolicyType`

``` purescript
newtype PolicyType
  = PolicyType String
```

##### Instances
``` purescript
Newtype PolicyType _
```

#### `PublishSchemaRequest`

``` purescript
newtype PublishSchemaRequest
  = PublishSchemaRequest { "DevelopmentSchemaArn" :: Arn, "Version" :: Version, "MinorVersion" :: NullOrUndefined (Version), "Name" :: NullOrUndefined (SchemaName) }
```

##### Instances
``` purescript
Newtype PublishSchemaRequest _
```

#### `PublishSchemaResponse`

``` purescript
newtype PublishSchemaResponse
  = PublishSchemaResponse { "PublishedSchemaArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype PublishSchemaResponse _
```

#### `PutSchemaFromJsonRequest`

``` purescript
newtype PutSchemaFromJsonRequest
  = PutSchemaFromJsonRequest { "SchemaArn" :: Arn, "Document" :: SchemaJsonDocument }
```

##### Instances
``` purescript
Newtype PutSchemaFromJsonRequest _
```

#### `PutSchemaFromJsonResponse`

``` purescript
newtype PutSchemaFromJsonResponse
  = PutSchemaFromJsonResponse { "Arn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype PutSchemaFromJsonResponse _
```

#### `RangeMode`

``` purescript
newtype RangeMode
  = RangeMode String
```

##### Instances
``` purescript
Newtype RangeMode _
```

#### `RemoveFacetFromObjectRequest`

``` purescript
newtype RemoveFacetFromObjectRequest
  = RemoveFacetFromObjectRequest { "DirectoryArn" :: Arn, "SchemaFacet" :: SchemaFacet, "ObjectReference" :: ObjectReference }
```

##### Instances
``` purescript
Newtype RemoveFacetFromObjectRequest _
```

#### `RemoveFacetFromObjectResponse`

``` purescript
newtype RemoveFacetFromObjectResponse
  = RemoveFacetFromObjectResponse {  }
```

##### Instances
``` purescript
Newtype RemoveFacetFromObjectResponse _
```

#### `RequiredAttributeBehavior`

``` purescript
newtype RequiredAttributeBehavior
  = RequiredAttributeBehavior String
```

##### Instances
``` purescript
Newtype RequiredAttributeBehavior _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified resource could not be found.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `RetryableConflictException`

``` purescript
newtype RetryableConflictException
  = RetryableConflictException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Occurs when a conflict with a previous successful write is detected. For example, if a write operation occurs on an object and then an attempt is made to read the object using “SERIALIZABLE” consistency, this exception may result. This generally occurs when the previous write did not have time to propagate to the host serving the current request. A retry (with appropriate backoff logic) is the recommended response to this exception.</p>

##### Instances
``` purescript
Newtype RetryableConflictException _
```

#### `Rule`

``` purescript
newtype Rule
  = Rule { "Type" :: NullOrUndefined (RuleType), "Parameters" :: NullOrUndefined (RuleParameterMap) }
```

<p>Contains an Amazon Resource Name (ARN) and parameters that are associated with the rule.</p>

##### Instances
``` purescript
Newtype Rule _
```

#### `RuleKey`

``` purescript
newtype RuleKey
  = RuleKey String
```

##### Instances
``` purescript
Newtype RuleKey _
```

#### `RuleMap`

``` purescript
newtype RuleMap
  = RuleMap (Map RuleKey Rule)
```

##### Instances
``` purescript
Newtype RuleMap _
```

#### `RuleParameterKey`

``` purescript
newtype RuleParameterKey
  = RuleParameterKey String
```

##### Instances
``` purescript
Newtype RuleParameterKey _
```

#### `RuleParameterMap`

``` purescript
newtype RuleParameterMap
  = RuleParameterMap (Map RuleParameterKey RuleParameterValue)
```

##### Instances
``` purescript
Newtype RuleParameterMap _
```

#### `RuleParameterValue`

``` purescript
newtype RuleParameterValue
  = RuleParameterValue String
```

##### Instances
``` purescript
Newtype RuleParameterValue _
```

#### `RuleType`

``` purescript
newtype RuleType
  = RuleType String
```

##### Instances
``` purescript
Newtype RuleType _
```

#### `SchemaAlreadyExistsException`

``` purescript
newtype SchemaAlreadyExistsException
  = SchemaAlreadyExistsException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that a schema could not be created due to a naming conflict. Please select a different name and then try again.</p>

##### Instances
``` purescript
Newtype SchemaAlreadyExistsException _
```

#### `SchemaAlreadyPublishedException`

``` purescript
newtype SchemaAlreadyPublishedException
  = SchemaAlreadyPublishedException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that a schema is already published.</p>

##### Instances
``` purescript
Newtype SchemaAlreadyPublishedException _
```

#### `SchemaFacet`

``` purescript
newtype SchemaFacet
  = SchemaFacet { "SchemaArn" :: NullOrUndefined (Arn), "FacetName" :: NullOrUndefined (FacetName) }
```

<p>A facet.</p>

##### Instances
``` purescript
Newtype SchemaFacet _
```

#### `SchemaFacetList`

``` purescript
newtype SchemaFacetList
  = SchemaFacetList (Array SchemaFacet)
```

##### Instances
``` purescript
Newtype SchemaFacetList _
```

#### `SchemaJsonDocument`

``` purescript
newtype SchemaJsonDocument
  = SchemaJsonDocument String
```

##### Instances
``` purescript
Newtype SchemaJsonDocument _
```

#### `SchemaName`

``` purescript
newtype SchemaName
  = SchemaName String
```

##### Instances
``` purescript
Newtype SchemaName _
```

#### `SelectorObjectReference`

``` purescript
newtype SelectorObjectReference
  = SelectorObjectReference String
```

##### Instances
``` purescript
Newtype SelectorObjectReference _
```

#### `StillContainsLinksException`

``` purescript
newtype StillContainsLinksException
  = StillContainsLinksException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The object could not be deleted because links still exist. Remove the links and then try the operation again.</p>

##### Instances
``` purescript
Newtype StillContainsLinksException _
```

#### `StringAttributeValue`

``` purescript
newtype StringAttributeValue
  = StringAttributeValue String
```

##### Instances
``` purescript
Newtype StringAttributeValue _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (TagKey), "Value" :: NullOrUndefined (TagValue) }
```

<p>The tag structure that contains a tag key and value.</p>

##### Instances
``` purescript
Newtype Tag _
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

##### Instances
``` purescript
Newtype TagKey _
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

##### Instances
``` purescript
Newtype TagKeyList _
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

##### Instances
``` purescript
Newtype TagList _
```

#### `TagResourceRequest`

``` purescript
newtype TagResourceRequest
  = TagResourceRequest { "ResourceArn" :: Arn, "Tags" :: TagList }
```

##### Instances
``` purescript
Newtype TagResourceRequest _
```

#### `TagResourceResponse`

``` purescript
newtype TagResourceResponse
  = TagResourceResponse {  }
```

##### Instances
``` purescript
Newtype TagResourceResponse _
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

##### Instances
``` purescript
Newtype TagValue _
```

#### `TagsNumberResults`

``` purescript
newtype TagsNumberResults
  = TagsNumberResults Int
```

##### Instances
``` purescript
Newtype TagsNumberResults _
```

#### `TypedAttributeValue`

``` purescript
newtype TypedAttributeValue
  = TypedAttributeValue { "StringValue" :: NullOrUndefined (StringAttributeValue), "BinaryValue" :: NullOrUndefined (BinaryAttributeValue), "BooleanValue" :: NullOrUndefined (BooleanAttributeValue), "NumberValue" :: NullOrUndefined (NumberAttributeValue), "DatetimeValue" :: NullOrUndefined (DatetimeAttributeValue) }
```

<p>Represents the data for a typed attribute. You can set one, and only one, of the elements. Each attribute in an item is a name-value pair. Attributes have a single value.</p>

##### Instances
``` purescript
Newtype TypedAttributeValue _
```

#### `TypedAttributeValueRange`

``` purescript
newtype TypedAttributeValueRange
  = TypedAttributeValueRange { "StartMode" :: RangeMode, "StartValue" :: NullOrUndefined (TypedAttributeValue), "EndMode" :: RangeMode, "EndValue" :: NullOrUndefined (TypedAttributeValue) }
```

<p>A range of attribute values.</p>

##### Instances
``` purescript
Newtype TypedAttributeValueRange _
```

#### `TypedLinkAttributeDefinition`

``` purescript
newtype TypedLinkAttributeDefinition
  = TypedLinkAttributeDefinition { "Name" :: AttributeName, "Type" :: FacetAttributeType, "DefaultValue" :: NullOrUndefined (TypedAttributeValue), "IsImmutable" :: NullOrUndefined (Bool), "Rules" :: NullOrUndefined (RuleMap), "RequiredBehavior" :: RequiredAttributeBehavior }
```

<p>A typed link attribute definition.</p>

##### Instances
``` purescript
Newtype TypedLinkAttributeDefinition _
```

#### `TypedLinkAttributeDefinitionList`

``` purescript
newtype TypedLinkAttributeDefinitionList
  = TypedLinkAttributeDefinitionList (Array TypedLinkAttributeDefinition)
```

##### Instances
``` purescript
Newtype TypedLinkAttributeDefinitionList _
```

#### `TypedLinkAttributeRange`

``` purescript
newtype TypedLinkAttributeRange
  = TypedLinkAttributeRange { "AttributeName" :: NullOrUndefined (AttributeName), "Range" :: TypedAttributeValueRange }
```

<p>Identifies the range of attributes that are used by a specified filter.</p>

##### Instances
``` purescript
Newtype TypedLinkAttributeRange _
```

#### `TypedLinkAttributeRangeList`

``` purescript
newtype TypedLinkAttributeRangeList
  = TypedLinkAttributeRangeList (Array TypedLinkAttributeRange)
```

##### Instances
``` purescript
Newtype TypedLinkAttributeRangeList _
```

#### `TypedLinkFacet`

``` purescript
newtype TypedLinkFacet
  = TypedLinkFacet { "Name" :: TypedLinkName, "Attributes" :: TypedLinkAttributeDefinitionList, "IdentityAttributeOrder" :: AttributeNameList }
```

<p>Defines the typed links structure and its attributes. To create a typed link facet, use the <a>CreateTypedLinkFacet</a> API.</p>

##### Instances
``` purescript
Newtype TypedLinkFacet _
```

#### `TypedLinkFacetAttributeUpdate`

``` purescript
newtype TypedLinkFacetAttributeUpdate
  = TypedLinkFacetAttributeUpdate { "Attribute" :: TypedLinkAttributeDefinition, "Action" :: UpdateActionType }
```

<p>A typed link facet attribute update.</p>

##### Instances
``` purescript
Newtype TypedLinkFacetAttributeUpdate _
```

#### `TypedLinkFacetAttributeUpdateList`

``` purescript
newtype TypedLinkFacetAttributeUpdateList
  = TypedLinkFacetAttributeUpdateList (Array TypedLinkFacetAttributeUpdate)
```

##### Instances
``` purescript
Newtype TypedLinkFacetAttributeUpdateList _
```

#### `TypedLinkName`

``` purescript
newtype TypedLinkName
  = TypedLinkName String
```

##### Instances
``` purescript
Newtype TypedLinkName _
```

#### `TypedLinkNameList`

``` purescript
newtype TypedLinkNameList
  = TypedLinkNameList (Array TypedLinkName)
```

##### Instances
``` purescript
Newtype TypedLinkNameList _
```

#### `TypedLinkSchemaAndFacetName`

``` purescript
newtype TypedLinkSchemaAndFacetName
  = TypedLinkSchemaAndFacetName { "SchemaArn" :: Arn, "TypedLinkName" :: TypedLinkName }
```

<p>Identifies the schema Amazon Resource Name (ARN) and facet name for the typed link.</p>

##### Instances
``` purescript
Newtype TypedLinkSchemaAndFacetName _
```

#### `TypedLinkSpecifier`

``` purescript
newtype TypedLinkSpecifier
  = TypedLinkSpecifier { "TypedLinkFacet" :: TypedLinkSchemaAndFacetName, "SourceObjectReference" :: ObjectReference, "TargetObjectReference" :: ObjectReference, "IdentityAttributeValues" :: AttributeNameAndValueList }
```

<p>Contains all the information that is used to uniquely identify a typed link. The parameters discussed in this topic are used to uniquely specify the typed link being operated on. The <a>AttachTypedLink</a> API returns a typed link specifier while the <a>DetachTypedLink</a> API accepts one as input. Similarly, the <a>ListIncomingTypedLinks</a> and <a>ListOutgoingTypedLinks</a> API operations provide typed link specifiers as output. You can also construct a typed link specifier from scratch.</p>

##### Instances
``` purescript
Newtype TypedLinkSpecifier _
```

#### `TypedLinkSpecifierList`

``` purescript
newtype TypedLinkSpecifierList
  = TypedLinkSpecifierList (Array TypedLinkSpecifier)
```

##### Instances
``` purescript
Newtype TypedLinkSpecifierList _
```

#### `UnsupportedIndexTypeException`

``` purescript
newtype UnsupportedIndexTypeException
  = UnsupportedIndexTypeException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that the requested index type is not supported.</p>

##### Instances
``` purescript
Newtype UnsupportedIndexTypeException _
```

#### `UntagResourceRequest`

``` purescript
newtype UntagResourceRequest
  = UntagResourceRequest { "ResourceArn" :: Arn, "TagKeys" :: TagKeyList }
```

##### Instances
``` purescript
Newtype UntagResourceRequest _
```

#### `UntagResourceResponse`

``` purescript
newtype UntagResourceResponse
  = UntagResourceResponse {  }
```

##### Instances
``` purescript
Newtype UntagResourceResponse _
```

#### `UpdateActionType`

``` purescript
newtype UpdateActionType
  = UpdateActionType String
```

##### Instances
``` purescript
Newtype UpdateActionType _
```

#### `UpdateFacetRequest`

``` purescript
newtype UpdateFacetRequest
  = UpdateFacetRequest { "SchemaArn" :: Arn, "Name" :: FacetName, "AttributeUpdates" :: NullOrUndefined (FacetAttributeUpdateList), "ObjectType" :: NullOrUndefined (ObjectType) }
```

##### Instances
``` purescript
Newtype UpdateFacetRequest _
```

#### `UpdateFacetResponse`

``` purescript
newtype UpdateFacetResponse
  = UpdateFacetResponse {  }
```

##### Instances
``` purescript
Newtype UpdateFacetResponse _
```

#### `UpdateObjectAttributesRequest`

``` purescript
newtype UpdateObjectAttributesRequest
  = UpdateObjectAttributesRequest { "DirectoryArn" :: Arn, "ObjectReference" :: ObjectReference, "AttributeUpdates" :: ObjectAttributeUpdateList }
```

##### Instances
``` purescript
Newtype UpdateObjectAttributesRequest _
```

#### `UpdateObjectAttributesResponse`

``` purescript
newtype UpdateObjectAttributesResponse
  = UpdateObjectAttributesResponse { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier) }
```

##### Instances
``` purescript
Newtype UpdateObjectAttributesResponse _
```

#### `UpdateSchemaRequest`

``` purescript
newtype UpdateSchemaRequest
  = UpdateSchemaRequest { "SchemaArn" :: Arn, "Name" :: SchemaName }
```

##### Instances
``` purescript
Newtype UpdateSchemaRequest _
```

#### `UpdateSchemaResponse`

``` purescript
newtype UpdateSchemaResponse
  = UpdateSchemaResponse { "SchemaArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype UpdateSchemaResponse _
```

#### `UpdateTypedLinkFacetRequest`

``` purescript
newtype UpdateTypedLinkFacetRequest
  = UpdateTypedLinkFacetRequest { "SchemaArn" :: Arn, "Name" :: TypedLinkName, "AttributeUpdates" :: TypedLinkFacetAttributeUpdateList, "IdentityAttributeOrder" :: AttributeNameList }
```

##### Instances
``` purescript
Newtype UpdateTypedLinkFacetRequest _
```

#### `UpdateTypedLinkFacetResponse`

``` purescript
newtype UpdateTypedLinkFacetResponse
  = UpdateTypedLinkFacetResponse {  }
```

##### Instances
``` purescript
Newtype UpdateTypedLinkFacetResponse _
```

#### `UpgradeAppliedSchemaRequest`

``` purescript
newtype UpgradeAppliedSchemaRequest
  = UpgradeAppliedSchemaRequest { "PublishedSchemaArn" :: Arn, "DirectoryArn" :: Arn, "DryRun" :: NullOrUndefined (Bool) }
```

##### Instances
``` purescript
Newtype UpgradeAppliedSchemaRequest _
```

#### `UpgradeAppliedSchemaResponse`

``` purescript
newtype UpgradeAppliedSchemaResponse
  = UpgradeAppliedSchemaResponse { "UpgradedSchemaArn" :: NullOrUndefined (Arn), "DirectoryArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype UpgradeAppliedSchemaResponse _
```

#### `UpgradePublishedSchemaRequest`

``` purescript
newtype UpgradePublishedSchemaRequest
  = UpgradePublishedSchemaRequest { "DevelopmentSchemaArn" :: Arn, "PublishedSchemaArn" :: Arn, "MinorVersion" :: Version, "DryRun" :: NullOrUndefined (Bool) }
```

##### Instances
``` purescript
Newtype UpgradePublishedSchemaRequest _
```

#### `UpgradePublishedSchemaResponse`

``` purescript
newtype UpgradePublishedSchemaResponse
  = UpgradePublishedSchemaResponse { "UpgradedSchemaArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype UpgradePublishedSchemaResponse _
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Indicates that your request is malformed in some manner. See the exception message.</p>

##### Instances
``` purescript
Newtype ValidationException _
```

#### `Version`

``` purescript
newtype Version
  = Version String
```

##### Instances
``` purescript
Newtype Version _
```


