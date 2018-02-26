

-- | <fullname>Amazon Cloud Directory</fullname> <p>Amazon Cloud Directory is a component of the AWS Directory Service that simplifies the development and management of cloud-scale web, mobile, and IoT applications. This guide describes the Cloud Directory operations that you can call programmatically and includes detailed information on data types and errors. For information about AWS Directory Services features, see <a href="https://aws.amazon.com/directoryservice/">AWS Directory Service</a> and the <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html">AWS Directory Service Administration Guide</a>.</p>
module AWS.CloudDirectory where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
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


newtype AddFacetToObjectRequest = AddFacetToObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "SchemaFacet" :: (SchemaFacet)
  , "ObjectAttributeList" :: NullOrUndefined (AttributeKeyAndValueList)
  , "ObjectReference" :: (ObjectReference)
  }


newtype AddFacetToObjectResponse = AddFacetToObjectResponse 
  { 
  }


newtype ApplySchemaRequest = ApplySchemaRequest 
  { "PublishedSchemaArn" :: (Arn)
  , "DirectoryArn" :: (Arn)
  }


newtype ApplySchemaResponse = ApplySchemaResponse 
  { "AppliedSchemaArn" :: NullOrUndefined (Arn)
  , "DirectoryArn" :: NullOrUndefined (Arn)
  }


newtype Arn = Arn String


newtype Arns = Arns (Array Arn)


newtype AttachObjectRequest = AttachObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "ParentReference" :: (ObjectReference)
  , "ChildReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  }


newtype AttachObjectResponse = AttachObjectResponse 
  { "AttachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


newtype AttachPolicyRequest = AttachPolicyRequest 
  { "DirectoryArn" :: NullOrUndefined (Arn)
  , "PolicyReference" :: (ObjectReference)
  , "ObjectReference" :: (ObjectReference)
  }


newtype AttachPolicyResponse = AttachPolicyResponse 
  { 
  }


newtype AttachToIndexRequest = AttachToIndexRequest 
  { "DirectoryArn" :: (Arn)
  , "IndexReference" :: (ObjectReference)
  , "TargetReference" :: (ObjectReference)
  }


newtype AttachToIndexResponse = AttachToIndexResponse 
  { "AttachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


newtype AttachTypedLinkRequest = AttachTypedLinkRequest 
  { "DirectoryArn" :: (Arn)
  , "SourceObjectReference" :: (ObjectReference)
  , "TargetObjectReference" :: (ObjectReference)
  , "TypedLinkFacet" :: (TypedLinkSchemaAndFacetName)
  , "Attributes" :: (AttributeNameAndValueList)
  }


newtype AttachTypedLinkResponse = AttachTypedLinkResponse 
  { "TypedLinkSpecifier" :: NullOrUndefined (TypedLinkSpecifier)
  }


-- | <p>A unique identifier for an attribute.</p>
newtype AttributeKey = AttributeKey 
  { "SchemaArn" :: (Arn)
  , "FacetName" :: (FacetName)
  , "Name" :: (AttributeName)
  }


-- | <p>The combination of an attribute key and an attribute value.</p>
newtype AttributeKeyAndValue = AttributeKeyAndValue 
  { "Key" :: (AttributeKey)
  , "Value" :: (TypedAttributeValue)
  }


newtype AttributeKeyAndValueList = AttributeKeyAndValueList (Array AttributeKeyAndValue)


newtype AttributeKeyList = AttributeKeyList (Array AttributeKey)


newtype AttributeName = AttributeName String


-- | <p>Identifies the attribute name and value for a typed link.</p>
newtype AttributeNameAndValue = AttributeNameAndValue 
  { "AttributeName" :: (AttributeName)
  , "Value" :: (TypedAttributeValue)
  }


newtype AttributeNameAndValueList = AttributeNameAndValueList (Array AttributeNameAndValue)


newtype AttributeNameList = AttributeNameList (Array AttributeName)


-- | <p>Represents the output of a batch add facet to object operation.</p>
newtype BatchAddFacetToObject = BatchAddFacetToObject 
  { "SchemaFacet" :: (SchemaFacet)
  , "ObjectAttributeList" :: (AttributeKeyAndValueList)
  , "ObjectReference" :: (ObjectReference)
  }


-- | <p>The result of a batch add facet to object operation.</p>
newtype BatchAddFacetToObjectResponse = BatchAddFacetToObjectResponse 
  { 
  }


-- | <p>Represents the output of an <a>AttachObject</a> operation.</p>
newtype BatchAttachObject = BatchAttachObject 
  { "ParentReference" :: (ObjectReference)
  , "ChildReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  }


-- | <p>Represents the output batch <a>AttachObject</a> response operation.</p>
newtype BatchAttachObjectResponse = BatchAttachObjectResponse 
  { "AttachedObjectIdentifier'" :: NullOrUndefined (ObjectIdentifier)
  }


-- | <p>Attaches a policy object to a regular object inside a <a>BatchRead</a> operation. For more information, see <a>AttachPolicy</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchAttachPolicy = BatchAttachPolicy 
  { "PolicyReference" :: (ObjectReference)
  , "ObjectReference" :: (ObjectReference)
  }


-- | <p>Represents the output of an <a>AttachPolicy</a> response operation.</p>
newtype BatchAttachPolicyResponse = BatchAttachPolicyResponse 
  { 
  }


-- | <p>Attaches the specified object to the specified index inside a <a>BatchRead</a> operation. For more information, see <a>AttachToIndex</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchAttachToIndex = BatchAttachToIndex 
  { "IndexReference" :: (ObjectReference)
  , "TargetReference" :: (ObjectReference)
  }


-- | <p>Represents the output of a <a>AttachToIndex</a> response operation.</p>
newtype BatchAttachToIndexResponse = BatchAttachToIndexResponse 
  { "AttachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


-- | <p>Attaches a typed link to a specified source and target object inside a <a>BatchRead</a> operation. For more information, see <a>AttachTypedLink</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchAttachTypedLink = BatchAttachTypedLink 
  { "SourceObjectReference" :: (ObjectReference)
  , "TargetObjectReference" :: (ObjectReference)
  , "TypedLinkFacet" :: (TypedLinkSchemaAndFacetName)
  , "Attributes" :: (AttributeNameAndValueList)
  }


-- | <p>Represents the output of a <a>AttachTypedLink</a> response operation.</p>
newtype BatchAttachTypedLinkResponse = BatchAttachTypedLinkResponse 
  { "TypedLinkSpecifier" :: NullOrUndefined (TypedLinkSpecifier)
  }


-- | <p>Creates an index object inside of a <a>BatchRead</a> operation. For more information, see <a>CreateIndex</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchCreateIndex = BatchCreateIndex 
  { "OrderedIndexedAttributeList" :: (AttributeKeyList)
  , "IsUnique" :: (Bool)
  , "ParentReference" :: NullOrUndefined (ObjectReference)
  , "LinkName" :: NullOrUndefined (LinkName)
  , "BatchReferenceName" :: NullOrUndefined (BatchReferenceName)
  }


-- | <p>Represents the output of a <a>CreateIndex</a> response operation.</p>
newtype BatchCreateIndexResponse = BatchCreateIndexResponse 
  { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


-- | <p>Represents the output of a <a>CreateObject</a> operation.</p>
newtype BatchCreateObject = BatchCreateObject 
  { "SchemaFacet" :: (SchemaFacetList)
  , "ObjectAttributeList" :: (AttributeKeyAndValueList)
  , "ParentReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  , "BatchReferenceName" :: (BatchReferenceName)
  }


-- | <p>Represents the output of a <a>CreateObject</a> response operation.</p>
newtype BatchCreateObjectResponse = BatchCreateObjectResponse 
  { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


-- | <p>Represents the output of a <a>DeleteObject</a> operation.</p>
newtype BatchDeleteObject = BatchDeleteObject 
  { "ObjectReference" :: (ObjectReference)
  }


-- | <p>Represents the output of a <a>DeleteObject</a> response operation.</p>
newtype BatchDeleteObjectResponse = BatchDeleteObjectResponse 
  { 
  }


-- | <p>Detaches the specified object from the specified index inside a <a>BatchRead</a> operation. For more information, see <a>DetachFromIndex</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchDetachFromIndex = BatchDetachFromIndex 
  { "IndexReference" :: (ObjectReference)
  , "TargetReference" :: (ObjectReference)
  }


-- | <p>Represents the output of a <a>DetachFromIndex</a> response operation.</p>
newtype BatchDetachFromIndexResponse = BatchDetachFromIndexResponse 
  { "DetachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


-- | <p>Represents the output of a <a>DetachObject</a> operation.</p>
newtype BatchDetachObject = BatchDetachObject 
  { "ParentReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  , "BatchReferenceName" :: (BatchReferenceName)
  }


-- | <p>Represents the output of a <a>DetachObject</a> response operation.</p>
newtype BatchDetachObjectResponse = BatchDetachObjectResponse 
  { "DetachedObjectIdentifier'" :: NullOrUndefined (ObjectIdentifier)
  }


-- | <p>Detaches the specified policy from the specified directory inside a <a>BatchWrite</a> operation. For more information, see <a>DetachPolicy</a> and <a>BatchWriteRequest$Operations</a>.</p>
newtype BatchDetachPolicy = BatchDetachPolicy 
  { "PolicyReference" :: (ObjectReference)
  , "ObjectReference" :: (ObjectReference)
  }


-- | <p>Represents the output of a <a>DetachPolicy</a> response operation.</p>
newtype BatchDetachPolicyResponse = BatchDetachPolicyResponse 
  { 
  }


-- | <p>Detaches a typed link from a specified source and target object inside a <a>BatchRead</a> operation. For more information, see <a>DetachTypedLink</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchDetachTypedLink = BatchDetachTypedLink 
  { "TypedLinkSpecifier" :: (TypedLinkSpecifier)
  }


-- | <p>Represents the output of a <a>DetachTypedLink</a> response operation.</p>
newtype BatchDetachTypedLinkResponse = BatchDetachTypedLinkResponse 
  { 
  }


-- | <p>Retrieves metadata about an object inside a <a>BatchRead</a> operation. For more information, see <a>GetObjectInformation</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchGetObjectInformation = BatchGetObjectInformation 
  { "ObjectReference" :: (ObjectReference)
  }


-- | <p>Represents the output of a <a>GetObjectInformation</a> response operation.</p>
newtype BatchGetObjectInformationResponse = BatchGetObjectInformationResponse 
  { "SchemaFacets" :: NullOrUndefined (SchemaFacetList)
  , "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


-- | <p>Lists indices attached to an object inside a <a>BatchRead</a> operation. For more information, see <a>ListAttachedIndices</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListAttachedIndices = BatchListAttachedIndices 
  { "TargetReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


-- | <p>Represents the output of a <a>ListAttachedIndices</a> response operation.</p>
newtype BatchListAttachedIndicesResponse = BatchListAttachedIndicesResponse 
  { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Returns a paginated list of all the incoming <a>TypedLinkSpecifier</a> information for an object inside a <a>BatchRead</a> operation. For more information, see <a>ListIncomingTypedLinks</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListIncomingTypedLinks = BatchListIncomingTypedLinks 
  { "ObjectReference" :: (ObjectReference)
  , "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList)
  , "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


-- | <p>Represents the output of a <a>ListIncomingTypedLinks</a> response operation.</p>
newtype BatchListIncomingTypedLinksResponse = BatchListIncomingTypedLinksResponse 
  { "LinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Lists objects attached to the specified index inside a <a>BatchRead</a> operation. For more information, see <a>ListIndex</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListIndex = BatchListIndex 
  { "RangesOnIndexedValues" :: NullOrUndefined (ObjectAttributeRangeList)
  , "IndexReference" :: (ObjectReference)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of a <a>ListIndex</a> response operation.</p>
newtype BatchListIndexResponse = BatchListIndexResponse 
  { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of a <a>ListObjectAttributes</a> operation.</p>
newtype BatchListObjectAttributes = BatchListObjectAttributes 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "FacetFilter" :: NullOrUndefined (SchemaFacet)
  }


-- | <p>Represents the output of a <a>ListObjectAttributes</a> response operation.</p>
newtype BatchListObjectAttributesResponse = BatchListObjectAttributesResponse 
  { "Attributes" :: NullOrUndefined (AttributeKeyAndValueList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of a <a>ListObjectChildren</a> operation.</p>
newtype BatchListObjectChildren = BatchListObjectChildren 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


-- | <p>Represents the output of a <a>ListObjectChildren</a> response operation.</p>
newtype BatchListObjectChildrenResponse = BatchListObjectChildrenResponse 
  { "Children" :: NullOrUndefined (LinkNameToObjectIdentifierMap)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects inside a <a>BatchRead</a> operation. For more information, see <a>ListObjectParentPaths</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListObjectParentPaths = BatchListObjectParentPaths 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


-- | <p>Represents the output of a <a>ListObjectParentPaths</a> response operation.</p>
newtype BatchListObjectParentPathsResponse = BatchListObjectParentPathsResponse 
  { "PathToObjectIdentifiersList" :: NullOrUndefined (PathToObjectIdentifiersList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Returns policies attached to an object in pagination fashion inside a <a>BatchRead</a> operation. For more information, see <a>ListObjectPolicies</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListObjectPolicies = BatchListObjectPolicies 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


-- | <p>Represents the output of a <a>ListObjectPolicies</a> response operation.</p>
newtype BatchListObjectPoliciesResponse = BatchListObjectPoliciesResponse 
  { "AttachedPolicyIds" :: NullOrUndefined (ObjectIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Returns a paginated list of all the outgoing <a>TypedLinkSpecifier</a> information for an object inside a <a>BatchRead</a> operation. For more information, see <a>ListOutgoingTypedLinks</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListOutgoingTypedLinks = BatchListOutgoingTypedLinks 
  { "ObjectReference" :: (ObjectReference)
  , "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList)
  , "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


-- | <p>Represents the output of a <a>ListOutgoingTypedLinks</a> response operation.</p>
newtype BatchListOutgoingTypedLinksResponse = BatchListOutgoingTypedLinksResponse 
  { "TypedLinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Returns all of the <code>ObjectIdentifiers</code> to which a given policy is attached inside a <a>BatchRead</a> operation. For more information, see <a>ListPolicyAttachments</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListPolicyAttachments = BatchListPolicyAttachments 
  { "PolicyReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


-- | <p>Represents the output of a <a>ListPolicyAttachments</a> response operation.</p>
newtype BatchListPolicyAttachmentsResponse = BatchListPolicyAttachmentsResponse 
  { "ObjectIdentifiers" :: NullOrUndefined (ObjectIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Lists all policies from the root of the Directory to the object specified inside a <a>BatchRead</a> operation. For more information, see <a>LookupPolicy</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchLookupPolicy = BatchLookupPolicy 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


-- | <p>Represents the output of a <a>LookupPolicy</a> response operation.</p>
newtype BatchLookupPolicyResponse = BatchLookupPolicyResponse 
  { "PolicyToPathList" :: NullOrUndefined (PolicyToPathList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype BatchOperationIndex = BatchOperationIndex Int


-- | <p>The batch read exception structure, which contains the exception type and message.</p>
newtype BatchReadException = BatchReadException 
  { "Type" :: NullOrUndefined (BatchReadExceptionType)
  , "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype BatchReadExceptionType = BatchReadExceptionType String


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


newtype BatchReadOperationList = BatchReadOperationList (Array BatchReadOperation)


-- | <p>Represents the output of a <code>BatchRead</code> response operation.</p>
newtype BatchReadOperationResponse = BatchReadOperationResponse 
  { "SuccessfulResponse" :: NullOrUndefined (BatchReadSuccessfulResponse)
  , "ExceptionResponse" :: NullOrUndefined (BatchReadException)
  }


newtype BatchReadOperationResponseList = BatchReadOperationResponseList (Array BatchReadOperationResponse)


newtype BatchReadRequest = BatchReadRequest 
  { "DirectoryArn" :: (Arn)
  , "Operations" :: (BatchReadOperationList)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }


newtype BatchReadResponse = BatchReadResponse 
  { "Responses" :: NullOrUndefined (BatchReadOperationResponseList)
  }


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


newtype BatchReferenceName = BatchReferenceName String


-- | <p>A batch operation to remove a facet from an object.</p>
newtype BatchRemoveFacetFromObject = BatchRemoveFacetFromObject 
  { "SchemaFacet" :: (SchemaFacet)
  , "ObjectReference" :: (ObjectReference)
  }


-- | <p>An empty result that represents success.</p>
newtype BatchRemoveFacetFromObjectResponse = BatchRemoveFacetFromObjectResponse 
  { 
  }


-- | <p>Represents the output of a <code>BatchUpdate</code> operation. </p>
newtype BatchUpdateObjectAttributes = BatchUpdateObjectAttributes 
  { "ObjectReference" :: (ObjectReference)
  , "AttributeUpdates" :: (ObjectAttributeUpdateList)
  }


-- | <p>Represents the output of a <code>BatchUpdate</code> response operation.</p>
newtype BatchUpdateObjectAttributesResponse = BatchUpdateObjectAttributesResponse 
  { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


-- | <p>A <code>BatchWrite</code> exception has occurred.</p>
newtype BatchWriteException = BatchWriteException 
  { "Index" :: NullOrUndefined (BatchOperationIndex)
  , "Type" :: NullOrUndefined (BatchWriteExceptionType)
  , "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype BatchWriteExceptionType = BatchWriteExceptionType String


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


newtype BatchWriteOperationList = BatchWriteOperationList (Array BatchWriteOperation)


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


newtype BatchWriteOperationResponseList = BatchWriteOperationResponseList (Array BatchWriteOperationResponse)


newtype BatchWriteRequest = BatchWriteRequest 
  { "DirectoryArn" :: (Arn)
  , "Operations" :: (BatchWriteOperationList)
  }


newtype BatchWriteResponse = BatchWriteResponse 
  { "Responses" :: NullOrUndefined (BatchWriteOperationResponseList)
  }


newtype BinaryAttributeValue = BinaryAttributeValue String


newtype Bool = Bool Boolean


newtype BooleanAttributeValue = BooleanAttributeValue Boolean


-- | <p>Cannot list the parents of a <a>Directory</a> root.</p>
newtype CannotListParentOfRootException = CannotListParentOfRootException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype ConsistencyLevel = ConsistencyLevel String


newtype CreateDirectoryRequest = CreateDirectoryRequest 
  { "Name" :: (DirectoryName)
  , "SchemaArn" :: (Arn)
  }


newtype CreateDirectoryResponse = CreateDirectoryResponse 
  { "DirectoryArn" :: (DirectoryArn)
  , "Name" :: (DirectoryName)
  , "ObjectIdentifier" :: (ObjectIdentifier)
  , "AppliedSchemaArn" :: (Arn)
  }


newtype CreateFacetRequest = CreateFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  , "Attributes" :: NullOrUndefined (FacetAttributeList)
  , "ObjectType" :: (ObjectType)
  }


newtype CreateFacetResponse = CreateFacetResponse 
  { 
  }


newtype CreateIndexRequest = CreateIndexRequest 
  { "DirectoryArn" :: (Arn)
  , "OrderedIndexedAttributeList" :: (AttributeKeyList)
  , "IsUnique" :: (Bool)
  , "ParentReference" :: NullOrUndefined (ObjectReference)
  , "LinkName" :: NullOrUndefined (LinkName)
  }


newtype CreateIndexResponse = CreateIndexResponse 
  { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


newtype CreateObjectRequest = CreateObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "SchemaFacets" :: (SchemaFacetList)
  , "ObjectAttributeList" :: NullOrUndefined (AttributeKeyAndValueList)
  , "ParentReference" :: NullOrUndefined (ObjectReference)
  , "LinkName" :: NullOrUndefined (LinkName)
  }


newtype CreateObjectResponse = CreateObjectResponse 
  { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


newtype CreateSchemaRequest = CreateSchemaRequest 
  { "Name" :: (SchemaName)
  }


newtype CreateSchemaResponse = CreateSchemaResponse 
  { "SchemaArn" :: NullOrUndefined (Arn)
  }


newtype CreateTypedLinkFacetRequest = CreateTypedLinkFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Facet" :: (TypedLinkFacet)
  }


newtype CreateTypedLinkFacetResponse = CreateTypedLinkFacetResponse 
  { 
  }


newtype Date = Date Number


newtype DatetimeAttributeValue = DatetimeAttributeValue Number


newtype DeleteDirectoryRequest = DeleteDirectoryRequest 
  { "DirectoryArn" :: (Arn)
  }


newtype DeleteDirectoryResponse = DeleteDirectoryResponse 
  { "DirectoryArn" :: (Arn)
  }


newtype DeleteFacetRequest = DeleteFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  }


newtype DeleteFacetResponse = DeleteFacetResponse 
  { 
  }


newtype DeleteObjectRequest = DeleteObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  }


newtype DeleteObjectResponse = DeleteObjectResponse 
  { 
  }


newtype DeleteSchemaRequest = DeleteSchemaRequest 
  { "SchemaArn" :: (Arn)
  }


newtype DeleteSchemaResponse = DeleteSchemaResponse 
  { "SchemaArn" :: NullOrUndefined (Arn)
  }


newtype DeleteTypedLinkFacetRequest = DeleteTypedLinkFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (TypedLinkName)
  }


newtype DeleteTypedLinkFacetResponse = DeleteTypedLinkFacetResponse 
  { 
  }


newtype DetachFromIndexRequest = DetachFromIndexRequest 
  { "DirectoryArn" :: (Arn)
  , "IndexReference" :: (ObjectReference)
  , "TargetReference" :: (ObjectReference)
  }


newtype DetachFromIndexResponse = DetachFromIndexResponse 
  { "DetachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


newtype DetachObjectRequest = DetachObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "ParentReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  }


newtype DetachObjectResponse = DetachObjectResponse 
  { "DetachedObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


newtype DetachPolicyRequest = DetachPolicyRequest 
  { "DirectoryArn" :: (Arn)
  , "PolicyReference" :: (ObjectReference)
  , "ObjectReference" :: (ObjectReference)
  }


newtype DetachPolicyResponse = DetachPolicyResponse 
  { 
  }


newtype DetachTypedLinkRequest = DetachTypedLinkRequest 
  { "DirectoryArn" :: (Arn)
  , "TypedLinkSpecifier" :: (TypedLinkSpecifier)
  }


-- | <p>Directory structure that includes the directory name and directory ARN.</p>
newtype Directory = Directory 
  { "Name" :: NullOrUndefined (DirectoryName)
  , "DirectoryArn" :: NullOrUndefined (DirectoryArn)
  , "State" :: NullOrUndefined (DirectoryState)
  , "CreationDateTime" :: NullOrUndefined (Date)
  }


-- | <p>Indicates that a <a>Directory</a> could not be created due to a naming conflict. Choose a different name and try again.</p>
newtype DirectoryAlreadyExistsException = DirectoryAlreadyExistsException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype DirectoryArn = DirectoryArn String


-- | <p>A directory that has been deleted and to which access has been attempted. Note: The requested resource will eventually cease to exist.</p>
newtype DirectoryDeletedException = DirectoryDeletedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype DirectoryList = DirectoryList (Array Directory)


newtype DirectoryName = DirectoryName String


-- | <p>An operation can only operate on a disabled directory.</p>
newtype DirectoryNotDisabledException = DirectoryNotDisabledException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>An operation can only operate on a directory that is not enabled.</p>
newtype DirectoryNotEnabledException = DirectoryNotEnabledException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype DirectoryState = DirectoryState String


newtype DisableDirectoryRequest = DisableDirectoryRequest 
  { "DirectoryArn" :: (Arn)
  }


newtype DisableDirectoryResponse = DisableDirectoryResponse 
  { "DirectoryArn" :: (Arn)
  }


newtype EnableDirectoryRequest = EnableDirectoryRequest 
  { "DirectoryArn" :: (Arn)
  }


newtype EnableDirectoryResponse = EnableDirectoryResponse 
  { "DirectoryArn" :: (Arn)
  }


newtype ExceptionMessage = ExceptionMessage String


-- | <p>A structure that contains <code>Name</code>, <code>ARN</code>, <code>Attributes</code>, <a>Rule</a>s, and <code>ObjectTypes</code>.</p>
newtype Facet = Facet 
  { "Name" :: NullOrUndefined (FacetName)
  , "ObjectType" :: NullOrUndefined (ObjectType)
  }


-- | <p>A facet with the same name already exists.</p>
newtype FacetAlreadyExistsException = FacetAlreadyExistsException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>An attribute that is associated with the <a>Facet</a>.</p>
newtype FacetAttribute = FacetAttribute 
  { "Name" :: (AttributeName)
  , "AttributeDefinition" :: NullOrUndefined (FacetAttributeDefinition)
  , "AttributeReference" :: NullOrUndefined (FacetAttributeReference)
  , "RequiredBehavior" :: NullOrUndefined (RequiredAttributeBehavior)
  }


-- | <p>A facet attribute definition. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences">Attribute References</a> for more information.</p>
newtype FacetAttributeDefinition = FacetAttributeDefinition 
  { "Type" :: (FacetAttributeType)
  , "DefaultValue" :: NullOrUndefined (TypedAttributeValue)
  , "IsImmutable" :: NullOrUndefined (Bool)
  , "Rules" :: NullOrUndefined (RuleMap)
  }


newtype FacetAttributeList = FacetAttributeList (Array FacetAttribute)


-- | <p>The facet attribute reference that specifies the attribute definition that contains the attribute facet name and attribute name.</p>
newtype FacetAttributeReference = FacetAttributeReference 
  { "TargetFacetName" :: (FacetName)
  , "TargetAttributeName" :: (AttributeName)
  }


newtype FacetAttributeType = FacetAttributeType String


-- | <p>A structure that contains information used to update an attribute.</p>
newtype FacetAttributeUpdate = FacetAttributeUpdate 
  { "Attribute" :: NullOrUndefined (FacetAttribute)
  , "Action" :: NullOrUndefined (UpdateActionType)
  }


newtype FacetAttributeUpdateList = FacetAttributeUpdateList (Array FacetAttributeUpdate)


-- | <p>Occurs when deleting a facet that contains an attribute that is a target to an attribute reference in a different facet.</p>
newtype FacetInUseException = FacetInUseException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype FacetName = FacetName String


newtype FacetNameList = FacetNameList (Array FacetName)


-- | <p>The specified <a>Facet</a> could not be found.</p>
newtype FacetNotFoundException = FacetNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>The <a>Facet</a> that you provided was not well formed or could not be validated with the schema.</p>
newtype FacetValidationException = FacetValidationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype GetAppliedSchemaVersionRequest = GetAppliedSchemaVersionRequest 
  { "SchemaArn" :: (Arn)
  }


newtype GetAppliedSchemaVersionResponse = GetAppliedSchemaVersionResponse 
  { "AppliedSchemaArn" :: NullOrUndefined (Arn)
  }


newtype GetDirectoryRequest = GetDirectoryRequest 
  { "DirectoryArn" :: (DirectoryArn)
  }


newtype GetDirectoryResponse = GetDirectoryResponse 
  { "Directory" :: (Directory)
  }


newtype GetFacetRequest = GetFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  }


newtype GetFacetResponse = GetFacetResponse 
  { "Facet" :: NullOrUndefined (Facet)
  }


newtype GetObjectInformationRequest = GetObjectInformationRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }


newtype GetObjectInformationResponse = GetObjectInformationResponse 
  { "SchemaFacets" :: NullOrUndefined (SchemaFacetList)
  , "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


newtype GetSchemaAsJsonRequest = GetSchemaAsJsonRequest 
  { "SchemaArn" :: (Arn)
  }


newtype GetSchemaAsJsonResponse = GetSchemaAsJsonResponse 
  { "Name" :: NullOrUndefined (SchemaName)
  , "Document" :: NullOrUndefined (SchemaJsonDocument)
  }


newtype GetTypedLinkFacetInformationRequest = GetTypedLinkFacetInformationRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (TypedLinkName)
  }


newtype GetTypedLinkFacetInformationResponse = GetTypedLinkFacetInformationResponse 
  { "IdentityAttributeOrder" :: NullOrUndefined (AttributeNameList)
  }


-- | <p>Indicates a failure occurred while performing a check for backward compatibility between the specified schema and the schema that is currently applied to the directory.</p>
newtype IncompatibleSchemaException = IncompatibleSchemaException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Represents an index and an attached object.</p>
newtype IndexAttachment = IndexAttachment 
  { "IndexedAttributes" :: NullOrUndefined (AttributeKeyAndValueList)
  , "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


newtype IndexAttachmentList = IndexAttachmentList (Array IndexAttachment)


-- | <p>An object has been attempted to be attached to an object that does not have the appropriate attribute value.</p>
newtype IndexedAttributeMissingException = IndexedAttributeMissingException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Indicates a problem that must be resolved by Amazon Web Services. This might be a transient error in which case you can retry your request until it succeeds. Otherwise, go to the <a href="http://status.aws.amazon.com/">AWS Service Health Dashboard</a> site to see if there are any operational issues with the service.</p>
newtype InternalServiceException = InternalServiceException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Indicates that the provided ARN value is not valid.</p>
newtype InvalidArnException = InvalidArnException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Indicates that an attempt to attach an object with the same link name or to apply a schema with the same name has occurred. Rename the link or the schema and then try again.</p>
newtype InvalidAttachmentException = InvalidAttachmentException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>An attempt to modify a <a>Facet</a> resulted in an invalid schema exception.</p>
newtype InvalidFacetUpdateException = InvalidFacetUpdateException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Indicates that the <code>NextToken</code> value is not valid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Occurs when any of the rule parameter keys or values are invalid.</p>
newtype InvalidRuleException = InvalidRuleException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Indicates that the provided <code>SchemaDoc</code> value is not valid.</p>
newtype InvalidSchemaDocException = InvalidSchemaDocException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Can occur for multiple reasons such as when you tag a resource that doesn’t exist or if you specify a higher number of tags for a resource than the allowed limit. Allowed limit is 50 tags per resource.</p>
newtype InvalidTaggingRequestException = InvalidTaggingRequestException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Indicates that limits are exceeded. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/limits.html">Limits</a> for more information.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype LinkName = LinkName String


-- | <p>Indicates that a link could not be created due to a naming conflict. Choose a different name and then try again.</p>
newtype LinkNameAlreadyInUseException = LinkNameAlreadyInUseException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype LinkNameToObjectIdentifierMap = LinkNameToObjectIdentifierMap (Map LinkName ObjectIdentifier)


newtype ListAppliedSchemaArnsRequest = ListAppliedSchemaArnsRequest 
  { "DirectoryArn" :: (Arn)
  , "SchemaArn" :: NullOrUndefined (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


newtype ListAppliedSchemaArnsResponse = ListAppliedSchemaArnsResponse 
  { "SchemaArns" :: NullOrUndefined (Arns)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListAttachedIndicesRequest = ListAttachedIndicesRequest 
  { "DirectoryArn" :: (Arn)
  , "TargetReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }


newtype ListAttachedIndicesResponse = ListAttachedIndicesResponse 
  { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListDevelopmentSchemaArnsRequest = ListDevelopmentSchemaArnsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


newtype ListDevelopmentSchemaArnsResponse = ListDevelopmentSchemaArnsResponse 
  { "SchemaArns" :: NullOrUndefined (Arns)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListDirectoriesRequest = ListDirectoriesRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "State'" :: NullOrUndefined (DirectoryState)
  }


newtype ListDirectoriesResponse = ListDirectoriesResponse 
  { "Directories" :: (DirectoryList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListFacetAttributesRequest = ListFacetAttributesRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


newtype ListFacetAttributesResponse = ListFacetAttributesResponse 
  { "Attributes" :: NullOrUndefined (FacetAttributeList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListFacetNamesRequest = ListFacetNamesRequest 
  { "SchemaArn" :: (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


newtype ListFacetNamesResponse = ListFacetNamesResponse 
  { "FacetNames" :: NullOrUndefined (FacetNameList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListIncomingTypedLinksRequest = ListIncomingTypedLinksRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList)
  , "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }


newtype ListIncomingTypedLinksResponse = ListIncomingTypedLinksResponse 
  { "LinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListIndexRequest = ListIndexRequest 
  { "DirectoryArn" :: (Arn)
  , "RangesOnIndexedValues" :: NullOrUndefined (ObjectAttributeRangeList)
  , "IndexReference" :: (ObjectReference)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }


newtype ListIndexResponse = ListIndexResponse 
  { "IndexAttachments" :: NullOrUndefined (IndexAttachmentList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListObjectAttributesRequest = ListObjectAttributesRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  , "FacetFilter" :: NullOrUndefined (SchemaFacet)
  }


newtype ListObjectAttributesResponse = ListObjectAttributesResponse 
  { "Attributes" :: NullOrUndefined (AttributeKeyAndValueList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListObjectChildrenRequest = ListObjectChildrenRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }


newtype ListObjectChildrenResponse = ListObjectChildrenResponse 
  { "Children" :: NullOrUndefined (LinkNameToObjectIdentifierMap)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListObjectParentPathsRequest = ListObjectParentPathsRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


newtype ListObjectParentPathsResponse = ListObjectParentPathsResponse 
  { "PathToObjectIdentifiersList" :: NullOrUndefined (PathToObjectIdentifiersList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListObjectParentsRequest = ListObjectParentsRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }


newtype ListObjectParentsResponse = ListObjectParentsResponse 
  { "Parents" :: NullOrUndefined (ObjectIdentifierToLinkNameMap)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListObjectPoliciesRequest = ListObjectPoliciesRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }


newtype ListObjectPoliciesResponse = ListObjectPoliciesResponse 
  { "AttachedPolicyIds" :: NullOrUndefined (ObjectIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListOutgoingTypedLinksRequest = ListOutgoingTypedLinksRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "FilterAttributeRanges" :: NullOrUndefined (TypedLinkAttributeRangeList)
  , "FilterTypedLink" :: NullOrUndefined (TypedLinkSchemaAndFacetName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }


newtype ListOutgoingTypedLinksResponse = ListOutgoingTypedLinksResponse 
  { "TypedLinkSpecifiers" :: NullOrUndefined (TypedLinkSpecifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListPolicyAttachmentsRequest = ListPolicyAttachmentsRequest 
  { "DirectoryArn" :: (Arn)
  , "PolicyReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined (ConsistencyLevel)
  }


newtype ListPolicyAttachmentsResponse = ListPolicyAttachmentsResponse 
  { "ObjectIdentifiers" :: NullOrUndefined (ObjectIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListPublishedSchemaArnsRequest = ListPublishedSchemaArnsRequest 
  { "SchemaArn" :: NullOrUndefined (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


newtype ListPublishedSchemaArnsResponse = ListPublishedSchemaArnsResponse 
  { "SchemaArns" :: NullOrUndefined (Arns)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceArn" :: (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (TagsNumberResults)
  }


newtype ListTagsForResourceResponse = ListTagsForResourceResponse 
  { "Tags" :: NullOrUndefined (TagList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListTypedLinkFacetAttributesRequest = ListTypedLinkFacetAttributesRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (TypedLinkName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


newtype ListTypedLinkFacetAttributesResponse = ListTypedLinkFacetAttributesResponse 
  { "Attributes" :: NullOrUndefined (TypedLinkAttributeDefinitionList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListTypedLinkFacetNamesRequest = ListTypedLinkFacetNamesRequest 
  { "SchemaArn" :: (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


newtype ListTypedLinkFacetNamesResponse = ListTypedLinkFacetNamesResponse 
  { "FacetNames" :: NullOrUndefined (TypedLinkNameList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype LookupPolicyRequest = LookupPolicyRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (NumberResults)
  }


newtype LookupPolicyResponse = LookupPolicyResponse 
  { "PolicyToPathList" :: NullOrUndefined (PolicyToPathList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype NextToken = NextToken String


-- | <p>Indicates that the requested operation can only operate on index objects.</p>
newtype NotIndexException = NotIndexException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Occurs when any invalid operations are performed on an object that is not a node, such as calling <code>ListObjectChildren</code> for a leaf node object.</p>
newtype NotNodeException = NotNodeException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Indicates that the requested operation can only operate on policy objects.</p>
newtype NotPolicyException = NotPolicyException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype NumberAttributeValue = NumberAttributeValue String


newtype NumberResults = NumberResults Int


-- | <p>Indicates that the object is not attached to the index.</p>
newtype ObjectAlreadyDetachedException = ObjectAlreadyDetachedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>The action to take on the object attribute.</p>
newtype ObjectAttributeAction = ObjectAttributeAction 
  { "ObjectAttributeActionType" :: NullOrUndefined (UpdateActionType)
  , "ObjectAttributeUpdateValue" :: NullOrUndefined (TypedAttributeValue)
  }


-- | <p>A range of attributes.</p>
newtype ObjectAttributeRange = ObjectAttributeRange 
  { "AttributeKey" :: NullOrUndefined (AttributeKey)
  , "Range" :: NullOrUndefined (TypedAttributeValueRange)
  }


newtype ObjectAttributeRangeList = ObjectAttributeRangeList (Array ObjectAttributeRange)


-- | <p>Structure that contains attribute update information.</p>
newtype ObjectAttributeUpdate = ObjectAttributeUpdate 
  { "ObjectAttributeKey" :: NullOrUndefined (AttributeKey)
  , "ObjectAttributeAction" :: NullOrUndefined (ObjectAttributeAction)
  }


newtype ObjectAttributeUpdateList = ObjectAttributeUpdateList (Array ObjectAttributeUpdate)


newtype ObjectIdentifier = ObjectIdentifier String


newtype ObjectIdentifierList = ObjectIdentifierList (Array ObjectIdentifier)


newtype ObjectIdentifierToLinkNameMap = ObjectIdentifierToLinkNameMap (Map ObjectIdentifier LinkName)


-- | <p>Indicates that the requested operation cannot be completed because the object has not been detached from the tree.</p>
newtype ObjectNotDetachedException = ObjectNotDetachedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>The reference that identifies an object.</p>
newtype ObjectReference = ObjectReference 
  { "Selector" :: NullOrUndefined (SelectorObjectReference)
  }


newtype ObjectType = ObjectType String


newtype PathString = PathString String


-- | <p>Returns the path to the <code>ObjectIdentifiers</code> that is associated with the directory.</p>
newtype PathToObjectIdentifiers = PathToObjectIdentifiers 
  { "Path" :: NullOrUndefined (PathString)
  , "ObjectIdentifiers" :: NullOrUndefined (ObjectIdentifierList)
  }


newtype PathToObjectIdentifiersList = PathToObjectIdentifiersList (Array PathToObjectIdentifiers)


-- | <p>Contains the <code>PolicyType</code>, <code>PolicyId</code>, and the <code>ObjectIdentifier</code> to which it is attached. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies">Policies</a>.</p>
newtype PolicyAttachment = PolicyAttachment 
  { "PolicyId" :: NullOrUndefined (ObjectIdentifier)
  , "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  , "PolicyType" :: NullOrUndefined (PolicyType)
  }


newtype PolicyAttachmentList = PolicyAttachmentList (Array PolicyAttachment)


-- | <p>Used when a regular object exists in a <a>Directory</a> and you want to find all of the policies that are associated with that object and the parent to that object.</p>
newtype PolicyToPath = PolicyToPath 
  { "Path" :: NullOrUndefined (PathString)
  , "Policies" :: NullOrUndefined (PolicyAttachmentList)
  }


newtype PolicyToPathList = PolicyToPathList (Array PolicyToPath)


newtype PolicyType = PolicyType String


newtype PublishSchemaRequest = PublishSchemaRequest 
  { "DevelopmentSchemaArn" :: (Arn)
  , "Version" :: (Version)
  , "MinorVersion" :: NullOrUndefined (Version)
  , "Name" :: NullOrUndefined (SchemaName)
  }


newtype PublishSchemaResponse = PublishSchemaResponse 
  { "PublishedSchemaArn" :: NullOrUndefined (Arn)
  }


newtype PutSchemaFromJsonRequest = PutSchemaFromJsonRequest 
  { "SchemaArn" :: (Arn)
  , "Document" :: (SchemaJsonDocument)
  }


newtype PutSchemaFromJsonResponse = PutSchemaFromJsonResponse 
  { "Arn" :: NullOrUndefined (Arn)
  }


newtype RangeMode = RangeMode String


newtype RemoveFacetFromObjectRequest = RemoveFacetFromObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "SchemaFacet" :: (SchemaFacet)
  , "ObjectReference" :: (ObjectReference)
  }


newtype RemoveFacetFromObjectResponse = RemoveFacetFromObjectResponse 
  { 
  }


newtype RequiredAttributeBehavior = RequiredAttributeBehavior String


-- | <p>The specified resource could not be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Occurs when a conflict with a previous successful write is detected. For example, if a write operation occurs on an object and then an attempt is made to read the object using “SERIALIZABLE” consistency, this exception may result. This generally occurs when the previous write did not have time to propagate to the host serving the current request. A retry (with appropriate backoff logic) is the recommended response to this exception.</p>
newtype RetryableConflictException = RetryableConflictException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Contains an Amazon Resource Name (ARN) and parameters that are associated with the rule.</p>
newtype Rule = Rule 
  { "Type" :: NullOrUndefined (RuleType)
  , "Parameters" :: NullOrUndefined (RuleParameterMap)
  }


newtype RuleKey = RuleKey String


newtype RuleMap = RuleMap (Map RuleKey Rule)


newtype RuleParameterKey = RuleParameterKey String


newtype RuleParameterMap = RuleParameterMap (Map RuleParameterKey RuleParameterValue)


newtype RuleParameterValue = RuleParameterValue String


newtype RuleType = RuleType String


-- | <p>Indicates that a schema could not be created due to a naming conflict. Please select a different name and then try again.</p>
newtype SchemaAlreadyExistsException = SchemaAlreadyExistsException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>Indicates that a schema is already published.</p>
newtype SchemaAlreadyPublishedException = SchemaAlreadyPublishedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>A facet.</p>
newtype SchemaFacet = SchemaFacet 
  { "SchemaArn" :: NullOrUndefined (Arn)
  , "FacetName" :: NullOrUndefined (FacetName)
  }


newtype SchemaFacetList = SchemaFacetList (Array SchemaFacet)


newtype SchemaJsonDocument = SchemaJsonDocument String


newtype SchemaName = SchemaName String


newtype SelectorObjectReference = SelectorObjectReference String


-- | <p>The object could not be deleted because links still exist. Remove the links and then try the operation again.</p>
newtype StillContainsLinksException = StillContainsLinksException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype StringAttributeValue = StringAttributeValue String


-- | <p>The tag structure that contains a tag key and value.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array TagKey)


newtype TagList = TagList (Array Tag)


newtype TagResourceRequest = TagResourceRequest 
  { "ResourceArn" :: (Arn)
  , "Tags" :: (TagList)
  }


newtype TagResourceResponse = TagResourceResponse 
  { 
  }


newtype TagValue = TagValue String


newtype TagsNumberResults = TagsNumberResults Int


-- | <p>Represents the data for a typed attribute. You can set one, and only one, of the elements. Each attribute in an item is a name-value pair. Attributes have a single value.</p>
newtype TypedAttributeValue = TypedAttributeValue 
  { "StringValue" :: NullOrUndefined (StringAttributeValue)
  , "BinaryValue" :: NullOrUndefined (BinaryAttributeValue)
  , "BooleanValue" :: NullOrUndefined (BooleanAttributeValue)
  , "NumberValue" :: NullOrUndefined (NumberAttributeValue)
  , "DatetimeValue" :: NullOrUndefined (DatetimeAttributeValue)
  }


-- | <p>A range of attribute values.</p>
newtype TypedAttributeValueRange = TypedAttributeValueRange 
  { "StartMode" :: (RangeMode)
  , "StartValue" :: NullOrUndefined (TypedAttributeValue)
  , "EndMode" :: (RangeMode)
  , "EndValue" :: NullOrUndefined (TypedAttributeValue)
  }


-- | <p>A typed link attribute definition.</p>
newtype TypedLinkAttributeDefinition = TypedLinkAttributeDefinition 
  { "Name" :: (AttributeName)
  , "Type" :: (FacetAttributeType)
  , "DefaultValue" :: NullOrUndefined (TypedAttributeValue)
  , "IsImmutable" :: NullOrUndefined (Bool)
  , "Rules" :: NullOrUndefined (RuleMap)
  , "RequiredBehavior" :: (RequiredAttributeBehavior)
  }


newtype TypedLinkAttributeDefinitionList = TypedLinkAttributeDefinitionList (Array TypedLinkAttributeDefinition)


-- | <p>Identifies the range of attributes that are used by a specified filter.</p>
newtype TypedLinkAttributeRange = TypedLinkAttributeRange 
  { "AttributeName" :: NullOrUndefined (AttributeName)
  , "Range" :: (TypedAttributeValueRange)
  }


newtype TypedLinkAttributeRangeList = TypedLinkAttributeRangeList (Array TypedLinkAttributeRange)


-- | <p>Defines the typed links structure and its attributes. To create a typed link facet, use the <a>CreateTypedLinkFacet</a> API.</p>
newtype TypedLinkFacet = TypedLinkFacet 
  { "Name" :: (TypedLinkName)
  , "Attributes" :: (TypedLinkAttributeDefinitionList)
  , "IdentityAttributeOrder" :: (AttributeNameList)
  }


-- | <p>A typed link facet attribute update.</p>
newtype TypedLinkFacetAttributeUpdate = TypedLinkFacetAttributeUpdate 
  { "Attribute" :: (TypedLinkAttributeDefinition)
  , "Action" :: (UpdateActionType)
  }


newtype TypedLinkFacetAttributeUpdateList = TypedLinkFacetAttributeUpdateList (Array TypedLinkFacetAttributeUpdate)


newtype TypedLinkName = TypedLinkName String


newtype TypedLinkNameList = TypedLinkNameList (Array TypedLinkName)


-- | <p>Identifies the schema Amazon Resource Name (ARN) and facet name for the typed link.</p>
newtype TypedLinkSchemaAndFacetName = TypedLinkSchemaAndFacetName 
  { "SchemaArn" :: (Arn)
  , "TypedLinkName" :: (TypedLinkName)
  }


-- | <p>Contains all the information that is used to uniquely identify a typed link. The parameters discussed in this topic are used to uniquely specify the typed link being operated on. The <a>AttachTypedLink</a> API returns a typed link specifier while the <a>DetachTypedLink</a> API accepts one as input. Similarly, the <a>ListIncomingTypedLinks</a> and <a>ListOutgoingTypedLinks</a> API operations provide typed link specifiers as output. You can also construct a typed link specifier from scratch.</p>
newtype TypedLinkSpecifier = TypedLinkSpecifier 
  { "TypedLinkFacet" :: (TypedLinkSchemaAndFacetName)
  , "SourceObjectReference" :: (ObjectReference)
  , "TargetObjectReference" :: (ObjectReference)
  , "IdentityAttributeValues" :: (AttributeNameAndValueList)
  }


newtype TypedLinkSpecifierList = TypedLinkSpecifierList (Array TypedLinkSpecifier)


-- | <p>Indicates that the requested index type is not supported.</p>
newtype UnsupportedIndexTypeException = UnsupportedIndexTypeException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceArn" :: (Arn)
  , "TagKeys" :: (TagKeyList)
  }


newtype UntagResourceResponse = UntagResourceResponse 
  { 
  }


newtype UpdateActionType = UpdateActionType String


newtype UpdateFacetRequest = UpdateFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  , "AttributeUpdates" :: NullOrUndefined (FacetAttributeUpdateList)
  , "ObjectType" :: NullOrUndefined (ObjectType)
  }


newtype UpdateFacetResponse = UpdateFacetResponse 
  { 
  }


newtype UpdateObjectAttributesRequest = UpdateObjectAttributesRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "AttributeUpdates" :: (ObjectAttributeUpdateList)
  }


newtype UpdateObjectAttributesResponse = UpdateObjectAttributesResponse 
  { "ObjectIdentifier" :: NullOrUndefined (ObjectIdentifier)
  }


newtype UpdateSchemaRequest = UpdateSchemaRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (SchemaName)
  }


newtype UpdateSchemaResponse = UpdateSchemaResponse 
  { "SchemaArn" :: NullOrUndefined (Arn)
  }


newtype UpdateTypedLinkFacetRequest = UpdateTypedLinkFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (TypedLinkName)
  , "AttributeUpdates" :: (TypedLinkFacetAttributeUpdateList)
  , "IdentityAttributeOrder" :: (AttributeNameList)
  }


newtype UpdateTypedLinkFacetResponse = UpdateTypedLinkFacetResponse 
  { 
  }


newtype UpgradeAppliedSchemaRequest = UpgradeAppliedSchemaRequest 
  { "PublishedSchemaArn" :: (Arn)
  , "DirectoryArn" :: (Arn)
  , "DryRun" :: NullOrUndefined (Bool)
  }


newtype UpgradeAppliedSchemaResponse = UpgradeAppliedSchemaResponse 
  { "UpgradedSchemaArn" :: NullOrUndefined (Arn)
  , "DirectoryArn" :: NullOrUndefined (Arn)
  }


newtype UpgradePublishedSchemaRequest = UpgradePublishedSchemaRequest 
  { "DevelopmentSchemaArn" :: (Arn)
  , "PublishedSchemaArn" :: (Arn)
  , "MinorVersion" :: (Version)
  , "DryRun" :: NullOrUndefined (Bool)
  }


newtype UpgradePublishedSchemaResponse = UpgradePublishedSchemaResponse 
  { "UpgradedSchemaArn" :: NullOrUndefined (Arn)
  }


-- | <p>Indicates that your request is malformed in some manner. See the exception message.</p>
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }


newtype Version = Version String
