

-- | <fullname>Amazon Cloud Directory</fullname> <p>Amazon Cloud Directory is a component of the AWS Directory Service that simplifies the development and management of cloud-scale web, mobile, and IoT applications. This guide describes the Cloud Directory operations that you can call programmatically and includes detailed information on data types and errors. For information about AWS Directory Services features, see <a href="https://aws.amazon.com/directoryservice/">AWS Directory Service</a> and the <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html">AWS Directory Service Administration Guide</a>.</p>
module AWS.CloudDirectory where

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

serviceName = "CloudDirectory" :: String


-- | <p>Adds a new <a>Facet</a> to an object.</p>
addFacetToObject :: forall eff. AddFacetToObjectRequest -> Aff (exception :: EXCEPTION | eff) AddFacetToObjectResponse
addFacetToObject = Request.request serviceName "addFacetToObject" 


-- | <p>Copies the input published schema, at the specified version, into the <a>Directory</a> with the same name and version as that of the published schema.</p>
applySchema :: forall eff. ApplySchemaRequest -> Aff (exception :: EXCEPTION | eff) ApplySchemaResponse
applySchema = Request.request serviceName "applySchema" 


-- | <p>Attaches an existing object to another object. An object can be accessed in two ways:</p> <ol> <li> <p>Using the path</p> </li> <li> <p>Using <code>ObjectIdentifier</code> </p> </li> </ol>
attachObject :: forall eff. AttachObjectRequest -> Aff (exception :: EXCEPTION | eff) AttachObjectResponse
attachObject = Request.request serviceName "attachObject" 


-- | <p>Attaches a policy object to a regular object. An object can have a limited number of attached policies.</p>
attachPolicy :: forall eff. AttachPolicyRequest -> Aff (exception :: EXCEPTION | eff) AttachPolicyResponse
attachPolicy = Request.request serviceName "attachPolicy" 


-- | <p>Attaches the specified object to the specified index.</p>
attachToIndex :: forall eff. AttachToIndexRequest -> Aff (exception :: EXCEPTION | eff) AttachToIndexResponse
attachToIndex = Request.request serviceName "attachToIndex" 


-- | <p>Attaches a typed link to a specified source and target object. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
attachTypedLink :: forall eff. AttachTypedLinkRequest -> Aff (exception :: EXCEPTION | eff) AttachTypedLinkResponse
attachTypedLink = Request.request serviceName "attachTypedLink" 


-- | <p>Performs all the read operations in a batch. </p>
batchRead :: forall eff. BatchReadRequest -> Aff (exception :: EXCEPTION | eff) BatchReadResponse
batchRead = Request.request serviceName "batchRead" 


-- | <p>Performs all the write operations in a batch. Either all the operations succeed or none.</p>
batchWrite :: forall eff. BatchWriteRequest -> Aff (exception :: EXCEPTION | eff) BatchWriteResponse
batchWrite = Request.request serviceName "batchWrite" 


-- | <p>Creates a <a>Directory</a> by copying the published schema into the directory. A directory cannot be created without a schema.</p>
createDirectory :: forall eff. CreateDirectoryRequest -> Aff (exception :: EXCEPTION | eff) CreateDirectoryResponse
createDirectory = Request.request serviceName "createDirectory" 


-- | <p>Creates a new <a>Facet</a> in a schema. Facet creation is allowed only in development or applied schemas.</p>
createFacet :: forall eff. CreateFacetRequest -> Aff (exception :: EXCEPTION | eff) CreateFacetResponse
createFacet = Request.request serviceName "createFacet" 


-- | <p>Creates an index object. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_indexing.html">Indexing</a> for more information.</p>
createIndex :: forall eff. CreateIndexRequest -> Aff (exception :: EXCEPTION | eff) CreateIndexResponse
createIndex = Request.request serviceName "createIndex" 


-- | <p>Creates an object in a <a>Directory</a>. Additionally attaches the object to a parent, if a parent reference and <code>LinkName</code> is specified. An object is simply a collection of <a>Facet</a> attributes. You can also use this API call to create a policy object, if the facet from which you create the object is a policy facet. </p>
createObject :: forall eff. CreateObjectRequest -> Aff (exception :: EXCEPTION | eff) CreateObjectResponse
createObject = Request.request serviceName "createObject" 


-- | <p>Creates a new schema in a development state. A schema can exist in three phases:</p> <ul> <li> <p> <i>Development:</i> This is a mutable phase of the schema. All new schemas are in the development phase. Once the schema is finalized, it can be published.</p> </li> <li> <p> <i>Published:</i> Published schemas are immutable and have a version associated with them.</p> </li> <li> <p> <i>Applied:</i> Applied schemas are mutable in a way that allows you to add new schema facets. You can also add new, nonrequired attributes to existing schema facets. You can apply only published schemas to directories. </p> </li> </ul>
createSchema :: forall eff. CreateSchemaRequest -> Aff (exception :: EXCEPTION | eff) CreateSchemaResponse
createSchema = Request.request serviceName "createSchema" 


-- | <p>Creates a <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
createTypedLinkFacet :: forall eff. CreateTypedLinkFacetRequest -> Aff (exception :: EXCEPTION | eff) CreateTypedLinkFacetResponse
createTypedLinkFacet = Request.request serviceName "createTypedLinkFacet" 


-- | <p>Deletes a directory. Only disabled directories can be deleted. A deleted directory cannot be undone. Exercise extreme caution when deleting directories.</p>
deleteDirectory :: forall eff. DeleteDirectoryRequest -> Aff (exception :: EXCEPTION | eff) DeleteDirectoryResponse
deleteDirectory = Request.request serviceName "deleteDirectory" 


-- | <p>Deletes a given <a>Facet</a>. All attributes and <a>Rule</a>s that are associated with the facet will be deleted. Only development schema facets are allowed deletion.</p>
deleteFacet :: forall eff. DeleteFacetRequest -> Aff (exception :: EXCEPTION | eff) DeleteFacetResponse
deleteFacet = Request.request serviceName "deleteFacet" 


-- | <p>Deletes an object and its associated attributes. Only objects with no children and no parents can be deleted.</p>
deleteObject :: forall eff. DeleteObjectRequest -> Aff (exception :: EXCEPTION | eff) DeleteObjectResponse
deleteObject = Request.request serviceName "deleteObject" 


-- | <p>Deletes a given schema. Schemas in a development and published state can only be deleted. </p>
deleteSchema :: forall eff. DeleteSchemaRequest -> Aff (exception :: EXCEPTION | eff) DeleteSchemaResponse
deleteSchema = Request.request serviceName "deleteSchema" 


-- | <p>Deletes a <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
deleteTypedLinkFacet :: forall eff. DeleteTypedLinkFacetRequest -> Aff (exception :: EXCEPTION | eff) DeleteTypedLinkFacetResponse
deleteTypedLinkFacet = Request.request serviceName "deleteTypedLinkFacet" 


-- | <p>Detaches the specified object from the specified index.</p>
detachFromIndex :: forall eff. DetachFromIndexRequest -> Aff (exception :: EXCEPTION | eff) DetachFromIndexResponse
detachFromIndex = Request.request serviceName "detachFromIndex" 


-- | <p>Detaches a given object from the parent object. The object that is to be detached from the parent is specified by the link name.</p>
detachObject :: forall eff. DetachObjectRequest -> Aff (exception :: EXCEPTION | eff) DetachObjectResponse
detachObject = Request.request serviceName "detachObject" 


-- | <p>Detaches a policy from an object.</p>
detachPolicy :: forall eff. DetachPolicyRequest -> Aff (exception :: EXCEPTION | eff) DetachPolicyResponse
detachPolicy = Request.request serviceName "detachPolicy" 


-- | <p>Detaches a typed link from a specified source and target object. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
detachTypedLink :: forall eff. DetachTypedLinkRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
detachTypedLink = Request.request serviceName "detachTypedLink" 


-- | <p>Disables the specified directory. Disabled directories cannot be read or written to. Only enabled directories can be disabled. Disabled directories may be reenabled.</p>
disableDirectory :: forall eff. DisableDirectoryRequest -> Aff (exception :: EXCEPTION | eff) DisableDirectoryResponse
disableDirectory = Request.request serviceName "disableDirectory" 


-- | <p>Enables the specified directory. Only disabled directories can be enabled. Once enabled, the directory can then be read and written to.</p>
enableDirectory :: forall eff. EnableDirectoryRequest -> Aff (exception :: EXCEPTION | eff) EnableDirectoryResponse
enableDirectory = Request.request serviceName "enableDirectory" 


-- | <p>Returns current applied schema version ARN, including the minor version in use.</p>
getAppliedSchemaVersion :: forall eff. GetAppliedSchemaVersionRequest -> Aff (exception :: EXCEPTION | eff) GetAppliedSchemaVersionResponse
getAppliedSchemaVersion = Request.request serviceName "getAppliedSchemaVersion" 


-- | <p>Retrieves metadata about a directory.</p>
getDirectory :: forall eff. GetDirectoryRequest -> Aff (exception :: EXCEPTION | eff) GetDirectoryResponse
getDirectory = Request.request serviceName "getDirectory" 


-- | <p>Gets details of the <a>Facet</a>, such as facet name, attributes, <a>Rule</a>s, or <code>ObjectType</code>. You can call this on all kinds of schema facets -- published, development, or applied.</p>
getFacet :: forall eff. GetFacetRequest -> Aff (exception :: EXCEPTION | eff) GetFacetResponse
getFacet = Request.request serviceName "getFacet" 


-- | <p>Retrieves metadata about an object.</p>
getObjectInformation :: forall eff. GetObjectInformationRequest -> Aff (exception :: EXCEPTION | eff) GetObjectInformationResponse
getObjectInformation = Request.request serviceName "getObjectInformation" 


-- | <p>Retrieves a JSON representation of the schema. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_schemas.html#jsonformat">JSON Schema Format</a> for more information.</p>
getSchemaAsJson :: forall eff. GetSchemaAsJsonRequest -> Aff (exception :: EXCEPTION | eff) GetSchemaAsJsonResponse
getSchemaAsJson = Request.request serviceName "getSchemaAsJson" 


-- | <p>Returns the identity attribute order for a specific <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
getTypedLinkFacetInformation :: forall eff. GetTypedLinkFacetInformationRequest -> Aff (exception :: EXCEPTION | eff) GetTypedLinkFacetInformationResponse
getTypedLinkFacetInformation = Request.request serviceName "getTypedLinkFacetInformation" 


-- | <p>Lists schema major versions applied to a directory. If <code>SchemaArn</code> is provided, lists the minor version.</p>
listAppliedSchemaArns :: forall eff. ListAppliedSchemaArnsRequest -> Aff (exception :: EXCEPTION | eff) ListAppliedSchemaArnsResponse
listAppliedSchemaArns = Request.request serviceName "listAppliedSchemaArns" 


-- | <p>Lists indices attached to the specified object.</p>
listAttachedIndices :: forall eff. ListAttachedIndicesRequest -> Aff (exception :: EXCEPTION | eff) ListAttachedIndicesResponse
listAttachedIndices = Request.request serviceName "listAttachedIndices" 


-- | <p>Retrieves each Amazon Resource Name (ARN) of schemas in the development state.</p>
listDevelopmentSchemaArns :: forall eff. ListDevelopmentSchemaArnsRequest -> Aff (exception :: EXCEPTION | eff) ListDevelopmentSchemaArnsResponse
listDevelopmentSchemaArns = Request.request serviceName "listDevelopmentSchemaArns" 


-- | <p>Lists directories created within an account.</p>
listDirectories :: forall eff. ListDirectoriesRequest -> Aff (exception :: EXCEPTION | eff) ListDirectoriesResponse
listDirectories = Request.request serviceName "listDirectories" 


-- | <p>Retrieves attributes attached to the facet.</p>
listFacetAttributes :: forall eff. ListFacetAttributesRequest -> Aff (exception :: EXCEPTION | eff) ListFacetAttributesResponse
listFacetAttributes = Request.request serviceName "listFacetAttributes" 


-- | <p>Retrieves the names of facets that exist in a schema.</p>
listFacetNames :: forall eff. ListFacetNamesRequest -> Aff (exception :: EXCEPTION | eff) ListFacetNamesResponse
listFacetNames = Request.request serviceName "listFacetNames" 


-- | <p>Returns a paginated list of all the incoming <a>TypedLinkSpecifier</a> information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
listIncomingTypedLinks :: forall eff. ListIncomingTypedLinksRequest -> Aff (exception :: EXCEPTION | eff) ListIncomingTypedLinksResponse
listIncomingTypedLinks = Request.request serviceName "listIncomingTypedLinks" 


-- | <p>Lists objects and indexed values attached to the index.</p>
listIndex :: forall eff. ListIndexRequest -> Aff (exception :: EXCEPTION | eff) ListIndexResponse
listIndex = Request.request serviceName "listIndex" 


-- | <p>Lists all attributes that are associated with an object. </p>
listObjectAttributes :: forall eff. ListObjectAttributesRequest -> Aff (exception :: EXCEPTION | eff) ListObjectAttributesResponse
listObjectAttributes = Request.request serviceName "listObjectAttributes" 


-- | <p>Returns a paginated list of child objects that are associated with a given object.</p>
listObjectChildren :: forall eff. ListObjectChildrenRequest -> Aff (exception :: EXCEPTION | eff) ListObjectChildrenResponse
listObjectChildren = Request.request serviceName "listObjectChildren" 


-- | <p>Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#dirstructure">Directory Structure</a>.</p> <p>Use this API to evaluate all parents for an object. The call returns all objects from the root of the directory up to the requested object. The API returns the number of paths based on user-defined <code>MaxResults</code>, in case there are multiple paths to the parent. The order of the paths and nodes returned is consistent among multiple API calls unless the objects are deleted or moved. Paths not leading to the directory root are ignored from the target object.</p>
listObjectParentPaths :: forall eff. ListObjectParentPathsRequest -> Aff (exception :: EXCEPTION | eff) ListObjectParentPathsResponse
listObjectParentPaths = Request.request serviceName "listObjectParentPaths" 


-- | <p>Lists parent objects that are associated with a given object in pagination fashion.</p>
listObjectParents :: forall eff. ListObjectParentsRequest -> Aff (exception :: EXCEPTION | eff) ListObjectParentsResponse
listObjectParents = Request.request serviceName "listObjectParents" 


-- | <p>Returns policies attached to an object in pagination fashion.</p>
listObjectPolicies :: forall eff. ListObjectPoliciesRequest -> Aff (exception :: EXCEPTION | eff) ListObjectPoliciesResponse
listObjectPolicies = Request.request serviceName "listObjectPolicies" 


-- | <p>Returns a paginated list of all the outgoing <a>TypedLinkSpecifier</a> information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
listOutgoingTypedLinks :: forall eff. ListOutgoingTypedLinksRequest -> Aff (exception :: EXCEPTION | eff) ListOutgoingTypedLinksResponse
listOutgoingTypedLinks = Request.request serviceName "listOutgoingTypedLinks" 


-- | <p>Returns all of the <code>ObjectIdentifiers</code> to which a given policy is attached.</p>
listPolicyAttachments :: forall eff. ListPolicyAttachmentsRequest -> Aff (exception :: EXCEPTION | eff) ListPolicyAttachmentsResponse
listPolicyAttachments = Request.request serviceName "listPolicyAttachments" 


-- | <p>Lists schema major versions for a published schema. If <code>SchemaArn</code> is provided, lists the minor version.</p>
listPublishedSchemaArns :: forall eff. ListPublishedSchemaArnsRequest -> Aff (exception :: EXCEPTION | eff) ListPublishedSchemaArnsResponse
listPublishedSchemaArns = Request.request serviceName "listPublishedSchemaArns" 


-- | <p>Returns tags for a resource. Tagging is currently supported only for directories with a limit of 50 tags per directory. All 50 tags are returned for a given directory with this API call.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (exception :: EXCEPTION | eff) ListTagsForResourceResponse
listTagsForResource = Request.request serviceName "listTagsForResource" 


-- | <p>Returns a paginated list of all attribute definitions for a particular <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
listTypedLinkFacetAttributes :: forall eff. ListTypedLinkFacetAttributesRequest -> Aff (exception :: EXCEPTION | eff) ListTypedLinkFacetAttributesResponse
listTypedLinkFacetAttributes = Request.request serviceName "listTypedLinkFacetAttributes" 


-- | <p>Returns a paginated list of <code>TypedLink</code> facet names for a particular schema. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
listTypedLinkFacetNames :: forall eff. ListTypedLinkFacetNamesRequest -> Aff (exception :: EXCEPTION | eff) ListTypedLinkFacetNamesResponse
listTypedLinkFacetNames = Request.request serviceName "listTypedLinkFacetNames" 


-- | <p>Lists all policies from the root of the <a>Directory</a> to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the <code>ObjectIdentifier</code> for such objects. If policies are present, it returns <code>ObjectIdentifier</code>, <code>policyId</code>, and <code>policyType</code>. Paths that don't lead to the root from the target object are ignored. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies">Policies</a>.</p>
lookupPolicy :: forall eff. LookupPolicyRequest -> Aff (exception :: EXCEPTION | eff) LookupPolicyResponse
lookupPolicy = Request.request serviceName "lookupPolicy" 


-- | <p>Publishes a development schema with a major version and a recommended minor version.</p>
publishSchema :: forall eff. PublishSchemaRequest -> Aff (exception :: EXCEPTION | eff) PublishSchemaResponse
publishSchema = Request.request serviceName "publishSchema" 


-- | <p>Allows a schema to be updated using JSON upload. Only available for development schemas. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_schemas.html#jsonformat">JSON Schema Format</a> for more information.</p>
putSchemaFromJson :: forall eff. PutSchemaFromJsonRequest -> Aff (exception :: EXCEPTION | eff) PutSchemaFromJsonResponse
putSchemaFromJson = Request.request serviceName "putSchemaFromJson" 


-- | <p>Removes the specified facet from the specified object.</p>
removeFacetFromObject :: forall eff. RemoveFacetFromObjectRequest -> Aff (exception :: EXCEPTION | eff) RemoveFacetFromObjectResponse
removeFacetFromObject = Request.request serviceName "removeFacetFromObject" 


-- | <p>An API operation for adding tags to a resource.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (exception :: EXCEPTION | eff) TagResourceResponse
tagResource = Request.request serviceName "tagResource" 


-- | <p>An API operation for removing tags from a resource.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (exception :: EXCEPTION | eff) UntagResourceResponse
untagResource = Request.request serviceName "untagResource" 


-- | <p>Does the following:</p> <ol> <li> <p>Adds new <code>Attributes</code>, <code>Rules</code>, or <code>ObjectTypes</code>.</p> </li> <li> <p>Updates existing <code>Attributes</code>, <code>Rules</code>, or <code>ObjectTypes</code>.</p> </li> <li> <p>Deletes existing <code>Attributes</code>, <code>Rules</code>, or <code>ObjectTypes</code>.</p> </li> </ol>
updateFacet :: forall eff. UpdateFacetRequest -> Aff (exception :: EXCEPTION | eff) UpdateFacetResponse
updateFacet = Request.request serviceName "updateFacet" 


-- | <p>Updates a given object's attributes.</p>
updateObjectAttributes :: forall eff. UpdateObjectAttributesRequest -> Aff (exception :: EXCEPTION | eff) UpdateObjectAttributesResponse
updateObjectAttributes = Request.request serviceName "updateObjectAttributes" 


-- | <p>Updates the schema name with a new name. Only development schema names can be updated.</p>
updateSchema :: forall eff. UpdateSchemaRequest -> Aff (exception :: EXCEPTION | eff) UpdateSchemaResponse
updateSchema = Request.request serviceName "updateSchema" 


-- | <p>Updates a <a>TypedLinkFacet</a>. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink">Typed link</a>.</p>
updateTypedLinkFacet :: forall eff. UpdateTypedLinkFacetRequest -> Aff (exception :: EXCEPTION | eff) UpdateTypedLinkFacetResponse
updateTypedLinkFacet = Request.request serviceName "updateTypedLinkFacet" 


-- | <p>Upgrades a single directory in-place using the <code>PublishedSchemaArn</code> with schema updates found in <code>MinorVersion</code>. Backwards-compatible minor version upgrades are instantaneously available for readers on all objects in the directory. Note: This is a synchronous API call and upgrades only one schema on a given directory per call. To upgrade multiple directories from one schema, you would need to call this API on each directory.</p>
upgradeAppliedSchema :: forall eff. UpgradeAppliedSchemaRequest -> Aff (exception :: EXCEPTION | eff) UpgradeAppliedSchemaResponse
upgradeAppliedSchema = Request.request serviceName "upgradeAppliedSchema" 


-- | <p>Upgrades a published schema under a new minor version revision using the current contents of <code>DevelopmentSchemaArn</code>.</p>
upgradePublishedSchema :: forall eff. UpgradePublishedSchemaRequest -> Aff (exception :: EXCEPTION | eff) UpgradePublishedSchemaResponse
upgradePublishedSchema = Request.request serviceName "upgradePublishedSchema" 


-- | <p>Access denied. Check your permissions.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _
derive instance repGenericAccessDeniedException :: Generic AccessDeniedException _
instance showAccessDeniedException :: Show AccessDeniedException where
  show = genericShow
instance decodeAccessDeniedException :: Decode AccessDeniedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessDeniedException :: Encode AccessDeniedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddFacetToObjectRequest = AddFacetToObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "SchemaFacet" :: (SchemaFacet)
  , "ObjectAttributeList" :: NullOrUndefined.NullOrUndefined (AttributeKeyAndValueList)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeAddFacetToObjectRequest :: Newtype AddFacetToObjectRequest _
derive instance repGenericAddFacetToObjectRequest :: Generic AddFacetToObjectRequest _
instance showAddFacetToObjectRequest :: Show AddFacetToObjectRequest where
  show = genericShow
instance decodeAddFacetToObjectRequest :: Decode AddFacetToObjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddFacetToObjectRequest :: Encode AddFacetToObjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddFacetToObjectResponse = AddFacetToObjectResponse Types.NoArguments
derive instance newtypeAddFacetToObjectResponse :: Newtype AddFacetToObjectResponse _
derive instance repGenericAddFacetToObjectResponse :: Generic AddFacetToObjectResponse _
instance showAddFacetToObjectResponse :: Show AddFacetToObjectResponse where
  show = genericShow
instance decodeAddFacetToObjectResponse :: Decode AddFacetToObjectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddFacetToObjectResponse :: Encode AddFacetToObjectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplySchemaRequest = ApplySchemaRequest 
  { "PublishedSchemaArn" :: (Arn)
  , "DirectoryArn" :: (Arn)
  }
derive instance newtypeApplySchemaRequest :: Newtype ApplySchemaRequest _
derive instance repGenericApplySchemaRequest :: Generic ApplySchemaRequest _
instance showApplySchemaRequest :: Show ApplySchemaRequest where
  show = genericShow
instance decodeApplySchemaRequest :: Decode ApplySchemaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplySchemaRequest :: Encode ApplySchemaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplySchemaResponse = ApplySchemaResponse 
  { "AppliedSchemaArn" :: NullOrUndefined.NullOrUndefined (Arn)
  , "DirectoryArn" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeApplySchemaResponse :: Newtype ApplySchemaResponse _
derive instance repGenericApplySchemaResponse :: Generic ApplySchemaResponse _
instance showApplySchemaResponse :: Show ApplySchemaResponse where
  show = genericShow
instance decodeApplySchemaResponse :: Decode ApplySchemaResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplySchemaResponse :: Encode ApplySchemaResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _
derive instance repGenericArn :: Generic Arn _
instance showArn :: Show Arn where
  show = genericShow
instance decodeArn :: Decode Arn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArn :: Encode Arn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Arns = Arns (Array Arn)
derive instance newtypeArns :: Newtype Arns _
derive instance repGenericArns :: Generic Arns _
instance showArns :: Show Arns where
  show = genericShow
instance decodeArns :: Decode Arns where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArns :: Encode Arns where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachObjectRequest = AttachObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "ParentReference" :: (ObjectReference)
  , "ChildReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  }
derive instance newtypeAttachObjectRequest :: Newtype AttachObjectRequest _
derive instance repGenericAttachObjectRequest :: Generic AttachObjectRequest _
instance showAttachObjectRequest :: Show AttachObjectRequest where
  show = genericShow
instance decodeAttachObjectRequest :: Decode AttachObjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachObjectRequest :: Encode AttachObjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachObjectResponse = AttachObjectResponse 
  { "AttachedObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeAttachObjectResponse :: Newtype AttachObjectResponse _
derive instance repGenericAttachObjectResponse :: Generic AttachObjectResponse _
instance showAttachObjectResponse :: Show AttachObjectResponse where
  show = genericShow
instance decodeAttachObjectResponse :: Decode AttachObjectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachObjectResponse :: Encode AttachObjectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachPolicyRequest = AttachPolicyRequest 
  { "DirectoryArn" :: NullOrUndefined.NullOrUndefined (Arn)
  , "PolicyReference" :: (ObjectReference)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeAttachPolicyRequest :: Newtype AttachPolicyRequest _
derive instance repGenericAttachPolicyRequest :: Generic AttachPolicyRequest _
instance showAttachPolicyRequest :: Show AttachPolicyRequest where
  show = genericShow
instance decodeAttachPolicyRequest :: Decode AttachPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachPolicyRequest :: Encode AttachPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachPolicyResponse = AttachPolicyResponse Types.NoArguments
derive instance newtypeAttachPolicyResponse :: Newtype AttachPolicyResponse _
derive instance repGenericAttachPolicyResponse :: Generic AttachPolicyResponse _
instance showAttachPolicyResponse :: Show AttachPolicyResponse where
  show = genericShow
instance decodeAttachPolicyResponse :: Decode AttachPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachPolicyResponse :: Encode AttachPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachToIndexRequest = AttachToIndexRequest 
  { "DirectoryArn" :: (Arn)
  , "IndexReference" :: (ObjectReference)
  , "TargetReference" :: (ObjectReference)
  }
derive instance newtypeAttachToIndexRequest :: Newtype AttachToIndexRequest _
derive instance repGenericAttachToIndexRequest :: Generic AttachToIndexRequest _
instance showAttachToIndexRequest :: Show AttachToIndexRequest where
  show = genericShow
instance decodeAttachToIndexRequest :: Decode AttachToIndexRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachToIndexRequest :: Encode AttachToIndexRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachToIndexResponse = AttachToIndexResponse 
  { "AttachedObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeAttachToIndexResponse :: Newtype AttachToIndexResponse _
derive instance repGenericAttachToIndexResponse :: Generic AttachToIndexResponse _
instance showAttachToIndexResponse :: Show AttachToIndexResponse where
  show = genericShow
instance decodeAttachToIndexResponse :: Decode AttachToIndexResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachToIndexResponse :: Encode AttachToIndexResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachTypedLinkRequest = AttachTypedLinkRequest 
  { "DirectoryArn" :: (Arn)
  , "SourceObjectReference" :: (ObjectReference)
  , "TargetObjectReference" :: (ObjectReference)
  , "TypedLinkFacet" :: (TypedLinkSchemaAndFacetName)
  , "Attributes" :: (AttributeNameAndValueList)
  }
derive instance newtypeAttachTypedLinkRequest :: Newtype AttachTypedLinkRequest _
derive instance repGenericAttachTypedLinkRequest :: Generic AttachTypedLinkRequest _
instance showAttachTypedLinkRequest :: Show AttachTypedLinkRequest where
  show = genericShow
instance decodeAttachTypedLinkRequest :: Decode AttachTypedLinkRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachTypedLinkRequest :: Encode AttachTypedLinkRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachTypedLinkResponse = AttachTypedLinkResponse 
  { "TypedLinkSpecifier" :: NullOrUndefined.NullOrUndefined (TypedLinkSpecifier)
  }
derive instance newtypeAttachTypedLinkResponse :: Newtype AttachTypedLinkResponse _
derive instance repGenericAttachTypedLinkResponse :: Generic AttachTypedLinkResponse _
instance showAttachTypedLinkResponse :: Show AttachTypedLinkResponse where
  show = genericShow
instance decodeAttachTypedLinkResponse :: Decode AttachTypedLinkResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachTypedLinkResponse :: Encode AttachTypedLinkResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A unique identifier for an attribute.</p>
newtype AttributeKey = AttributeKey 
  { "SchemaArn" :: (Arn)
  , "FacetName" :: (FacetName)
  , "Name" :: (AttributeName)
  }
derive instance newtypeAttributeKey :: Newtype AttributeKey _
derive instance repGenericAttributeKey :: Generic AttributeKey _
instance showAttributeKey :: Show AttributeKey where
  show = genericShow
instance decodeAttributeKey :: Decode AttributeKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeKey :: Encode AttributeKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The combination of an attribute key and an attribute value.</p>
newtype AttributeKeyAndValue = AttributeKeyAndValue 
  { "Key" :: (AttributeKey)
  , "Value" :: (TypedAttributeValue)
  }
derive instance newtypeAttributeKeyAndValue :: Newtype AttributeKeyAndValue _
derive instance repGenericAttributeKeyAndValue :: Generic AttributeKeyAndValue _
instance showAttributeKeyAndValue :: Show AttributeKeyAndValue where
  show = genericShow
instance decodeAttributeKeyAndValue :: Decode AttributeKeyAndValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeKeyAndValue :: Encode AttributeKeyAndValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeKeyAndValueList = AttributeKeyAndValueList (Array AttributeKeyAndValue)
derive instance newtypeAttributeKeyAndValueList :: Newtype AttributeKeyAndValueList _
derive instance repGenericAttributeKeyAndValueList :: Generic AttributeKeyAndValueList _
instance showAttributeKeyAndValueList :: Show AttributeKeyAndValueList where
  show = genericShow
instance decodeAttributeKeyAndValueList :: Decode AttributeKeyAndValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeKeyAndValueList :: Encode AttributeKeyAndValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeKeyList = AttributeKeyList (Array AttributeKey)
derive instance newtypeAttributeKeyList :: Newtype AttributeKeyList _
derive instance repGenericAttributeKeyList :: Generic AttributeKeyList _
instance showAttributeKeyList :: Show AttributeKeyList where
  show = genericShow
instance decodeAttributeKeyList :: Decode AttributeKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeKeyList :: Encode AttributeKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeName = AttributeName String
derive instance newtypeAttributeName :: Newtype AttributeName _
derive instance repGenericAttributeName :: Generic AttributeName _
instance showAttributeName :: Show AttributeName where
  show = genericShow
instance decodeAttributeName :: Decode AttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeName :: Encode AttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Identifies the attribute name and value for a typed link.</p>
newtype AttributeNameAndValue = AttributeNameAndValue 
  { "AttributeName" :: (AttributeName)
  , "Value" :: (TypedAttributeValue)
  }
derive instance newtypeAttributeNameAndValue :: Newtype AttributeNameAndValue _
derive instance repGenericAttributeNameAndValue :: Generic AttributeNameAndValue _
instance showAttributeNameAndValue :: Show AttributeNameAndValue where
  show = genericShow
instance decodeAttributeNameAndValue :: Decode AttributeNameAndValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeNameAndValue :: Encode AttributeNameAndValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeNameAndValueList = AttributeNameAndValueList (Array AttributeNameAndValue)
derive instance newtypeAttributeNameAndValueList :: Newtype AttributeNameAndValueList _
derive instance repGenericAttributeNameAndValueList :: Generic AttributeNameAndValueList _
instance showAttributeNameAndValueList :: Show AttributeNameAndValueList where
  show = genericShow
instance decodeAttributeNameAndValueList :: Decode AttributeNameAndValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeNameAndValueList :: Encode AttributeNameAndValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeNameList = AttributeNameList (Array AttributeName)
derive instance newtypeAttributeNameList :: Newtype AttributeNameList _
derive instance repGenericAttributeNameList :: Generic AttributeNameList _
instance showAttributeNameList :: Show AttributeNameList where
  show = genericShow
instance decodeAttributeNameList :: Decode AttributeNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeNameList :: Encode AttributeNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a batch add facet to object operation.</p>
newtype BatchAddFacetToObject = BatchAddFacetToObject 
  { "SchemaFacet" :: (SchemaFacet)
  , "ObjectAttributeList" :: (AttributeKeyAndValueList)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeBatchAddFacetToObject :: Newtype BatchAddFacetToObject _
derive instance repGenericBatchAddFacetToObject :: Generic BatchAddFacetToObject _
instance showBatchAddFacetToObject :: Show BatchAddFacetToObject where
  show = genericShow
instance decodeBatchAddFacetToObject :: Decode BatchAddFacetToObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchAddFacetToObject :: Encode BatchAddFacetToObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a batch add facet to object operation.</p>
newtype BatchAddFacetToObjectResponse = BatchAddFacetToObjectResponse Types.NoArguments
derive instance newtypeBatchAddFacetToObjectResponse :: Newtype BatchAddFacetToObjectResponse _
derive instance repGenericBatchAddFacetToObjectResponse :: Generic BatchAddFacetToObjectResponse _
instance showBatchAddFacetToObjectResponse :: Show BatchAddFacetToObjectResponse where
  show = genericShow
instance decodeBatchAddFacetToObjectResponse :: Decode BatchAddFacetToObjectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchAddFacetToObjectResponse :: Encode BatchAddFacetToObjectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of an <a>AttachObject</a> operation.</p>
newtype BatchAttachObject = BatchAttachObject 
  { "ParentReference" :: (ObjectReference)
  , "ChildReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  }
derive instance newtypeBatchAttachObject :: Newtype BatchAttachObject _
derive instance repGenericBatchAttachObject :: Generic BatchAttachObject _
instance showBatchAttachObject :: Show BatchAttachObject where
  show = genericShow
instance decodeBatchAttachObject :: Decode BatchAttachObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchAttachObject :: Encode BatchAttachObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output batch <a>AttachObject</a> response operation.</p>
newtype BatchAttachObjectResponse = BatchAttachObjectResponse 
  { "AttachedObjectIdentifier'" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchAttachObjectResponse :: Newtype BatchAttachObjectResponse _
derive instance repGenericBatchAttachObjectResponse :: Generic BatchAttachObjectResponse _
instance showBatchAttachObjectResponse :: Show BatchAttachObjectResponse where
  show = genericShow
instance decodeBatchAttachObjectResponse :: Decode BatchAttachObjectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchAttachObjectResponse :: Encode BatchAttachObjectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Attaches a policy object to a regular object inside a <a>BatchRead</a> operation. For more information, see <a>AttachPolicy</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchAttachPolicy = BatchAttachPolicy 
  { "PolicyReference" :: (ObjectReference)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeBatchAttachPolicy :: Newtype BatchAttachPolicy _
derive instance repGenericBatchAttachPolicy :: Generic BatchAttachPolicy _
instance showBatchAttachPolicy :: Show BatchAttachPolicy where
  show = genericShow
instance decodeBatchAttachPolicy :: Decode BatchAttachPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchAttachPolicy :: Encode BatchAttachPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of an <a>AttachPolicy</a> response operation.</p>
newtype BatchAttachPolicyResponse = BatchAttachPolicyResponse Types.NoArguments
derive instance newtypeBatchAttachPolicyResponse :: Newtype BatchAttachPolicyResponse _
derive instance repGenericBatchAttachPolicyResponse :: Generic BatchAttachPolicyResponse _
instance showBatchAttachPolicyResponse :: Show BatchAttachPolicyResponse where
  show = genericShow
instance decodeBatchAttachPolicyResponse :: Decode BatchAttachPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchAttachPolicyResponse :: Encode BatchAttachPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Attaches the specified object to the specified index inside a <a>BatchRead</a> operation. For more information, see <a>AttachToIndex</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchAttachToIndex = BatchAttachToIndex 
  { "IndexReference" :: (ObjectReference)
  , "TargetReference" :: (ObjectReference)
  }
derive instance newtypeBatchAttachToIndex :: Newtype BatchAttachToIndex _
derive instance repGenericBatchAttachToIndex :: Generic BatchAttachToIndex _
instance showBatchAttachToIndex :: Show BatchAttachToIndex where
  show = genericShow
instance decodeBatchAttachToIndex :: Decode BatchAttachToIndex where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchAttachToIndex :: Encode BatchAttachToIndex where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>AttachToIndex</a> response operation.</p>
newtype BatchAttachToIndexResponse = BatchAttachToIndexResponse 
  { "AttachedObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchAttachToIndexResponse :: Newtype BatchAttachToIndexResponse _
derive instance repGenericBatchAttachToIndexResponse :: Generic BatchAttachToIndexResponse _
instance showBatchAttachToIndexResponse :: Show BatchAttachToIndexResponse where
  show = genericShow
instance decodeBatchAttachToIndexResponse :: Decode BatchAttachToIndexResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchAttachToIndexResponse :: Encode BatchAttachToIndexResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Attaches a typed link to a specified source and target object inside a <a>BatchRead</a> operation. For more information, see <a>AttachTypedLink</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchAttachTypedLink = BatchAttachTypedLink 
  { "SourceObjectReference" :: (ObjectReference)
  , "TargetObjectReference" :: (ObjectReference)
  , "TypedLinkFacet" :: (TypedLinkSchemaAndFacetName)
  , "Attributes" :: (AttributeNameAndValueList)
  }
derive instance newtypeBatchAttachTypedLink :: Newtype BatchAttachTypedLink _
derive instance repGenericBatchAttachTypedLink :: Generic BatchAttachTypedLink _
instance showBatchAttachTypedLink :: Show BatchAttachTypedLink where
  show = genericShow
instance decodeBatchAttachTypedLink :: Decode BatchAttachTypedLink where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchAttachTypedLink :: Encode BatchAttachTypedLink where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>AttachTypedLink</a> response operation.</p>
newtype BatchAttachTypedLinkResponse = BatchAttachTypedLinkResponse 
  { "TypedLinkSpecifier" :: NullOrUndefined.NullOrUndefined (TypedLinkSpecifier)
  }
derive instance newtypeBatchAttachTypedLinkResponse :: Newtype BatchAttachTypedLinkResponse _
derive instance repGenericBatchAttachTypedLinkResponse :: Generic BatchAttachTypedLinkResponse _
instance showBatchAttachTypedLinkResponse :: Show BatchAttachTypedLinkResponse where
  show = genericShow
instance decodeBatchAttachTypedLinkResponse :: Decode BatchAttachTypedLinkResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchAttachTypedLinkResponse :: Encode BatchAttachTypedLinkResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Creates an index object inside of a <a>BatchRead</a> operation. For more information, see <a>CreateIndex</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchCreateIndex = BatchCreateIndex 
  { "OrderedIndexedAttributeList" :: (AttributeKeyList)
  , "IsUnique" :: (Bool)
  , "ParentReference" :: NullOrUndefined.NullOrUndefined (ObjectReference)
  , "LinkName" :: NullOrUndefined.NullOrUndefined (LinkName)
  , "BatchReferenceName" :: NullOrUndefined.NullOrUndefined (BatchReferenceName)
  }
derive instance newtypeBatchCreateIndex :: Newtype BatchCreateIndex _
derive instance repGenericBatchCreateIndex :: Generic BatchCreateIndex _
instance showBatchCreateIndex :: Show BatchCreateIndex where
  show = genericShow
instance decodeBatchCreateIndex :: Decode BatchCreateIndex where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchCreateIndex :: Encode BatchCreateIndex where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>CreateIndex</a> response operation.</p>
newtype BatchCreateIndexResponse = BatchCreateIndexResponse 
  { "ObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchCreateIndexResponse :: Newtype BatchCreateIndexResponse _
derive instance repGenericBatchCreateIndexResponse :: Generic BatchCreateIndexResponse _
instance showBatchCreateIndexResponse :: Show BatchCreateIndexResponse where
  show = genericShow
instance decodeBatchCreateIndexResponse :: Decode BatchCreateIndexResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchCreateIndexResponse :: Encode BatchCreateIndexResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>CreateObject</a> operation.</p>
newtype BatchCreateObject = BatchCreateObject 
  { "SchemaFacet" :: (SchemaFacetList)
  , "ObjectAttributeList" :: (AttributeKeyAndValueList)
  , "ParentReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  , "BatchReferenceName" :: (BatchReferenceName)
  }
derive instance newtypeBatchCreateObject :: Newtype BatchCreateObject _
derive instance repGenericBatchCreateObject :: Generic BatchCreateObject _
instance showBatchCreateObject :: Show BatchCreateObject where
  show = genericShow
instance decodeBatchCreateObject :: Decode BatchCreateObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchCreateObject :: Encode BatchCreateObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>CreateObject</a> response operation.</p>
newtype BatchCreateObjectResponse = BatchCreateObjectResponse 
  { "ObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchCreateObjectResponse :: Newtype BatchCreateObjectResponse _
derive instance repGenericBatchCreateObjectResponse :: Generic BatchCreateObjectResponse _
instance showBatchCreateObjectResponse :: Show BatchCreateObjectResponse where
  show = genericShow
instance decodeBatchCreateObjectResponse :: Decode BatchCreateObjectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchCreateObjectResponse :: Encode BatchCreateObjectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>DeleteObject</a> operation.</p>
newtype BatchDeleteObject = BatchDeleteObject 
  { "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeBatchDeleteObject :: Newtype BatchDeleteObject _
derive instance repGenericBatchDeleteObject :: Generic BatchDeleteObject _
instance showBatchDeleteObject :: Show BatchDeleteObject where
  show = genericShow
instance decodeBatchDeleteObject :: Decode BatchDeleteObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeleteObject :: Encode BatchDeleteObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>DeleteObject</a> response operation.</p>
newtype BatchDeleteObjectResponse = BatchDeleteObjectResponse Types.NoArguments
derive instance newtypeBatchDeleteObjectResponse :: Newtype BatchDeleteObjectResponse _
derive instance repGenericBatchDeleteObjectResponse :: Generic BatchDeleteObjectResponse _
instance showBatchDeleteObjectResponse :: Show BatchDeleteObjectResponse where
  show = genericShow
instance decodeBatchDeleteObjectResponse :: Decode BatchDeleteObjectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeleteObjectResponse :: Encode BatchDeleteObjectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detaches the specified object from the specified index inside a <a>BatchRead</a> operation. For more information, see <a>DetachFromIndex</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchDetachFromIndex = BatchDetachFromIndex 
  { "IndexReference" :: (ObjectReference)
  , "TargetReference" :: (ObjectReference)
  }
derive instance newtypeBatchDetachFromIndex :: Newtype BatchDetachFromIndex _
derive instance repGenericBatchDetachFromIndex :: Generic BatchDetachFromIndex _
instance showBatchDetachFromIndex :: Show BatchDetachFromIndex where
  show = genericShow
instance decodeBatchDetachFromIndex :: Decode BatchDetachFromIndex where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDetachFromIndex :: Encode BatchDetachFromIndex where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>DetachFromIndex</a> response operation.</p>
newtype BatchDetachFromIndexResponse = BatchDetachFromIndexResponse 
  { "DetachedObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchDetachFromIndexResponse :: Newtype BatchDetachFromIndexResponse _
derive instance repGenericBatchDetachFromIndexResponse :: Generic BatchDetachFromIndexResponse _
instance showBatchDetachFromIndexResponse :: Show BatchDetachFromIndexResponse where
  show = genericShow
instance decodeBatchDetachFromIndexResponse :: Decode BatchDetachFromIndexResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDetachFromIndexResponse :: Encode BatchDetachFromIndexResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>DetachObject</a> operation.</p>
newtype BatchDetachObject = BatchDetachObject 
  { "ParentReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  , "BatchReferenceName" :: (BatchReferenceName)
  }
derive instance newtypeBatchDetachObject :: Newtype BatchDetachObject _
derive instance repGenericBatchDetachObject :: Generic BatchDetachObject _
instance showBatchDetachObject :: Show BatchDetachObject where
  show = genericShow
instance decodeBatchDetachObject :: Decode BatchDetachObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDetachObject :: Encode BatchDetachObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>DetachObject</a> response operation.</p>
newtype BatchDetachObjectResponse = BatchDetachObjectResponse 
  { "DetachedObjectIdentifier'" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchDetachObjectResponse :: Newtype BatchDetachObjectResponse _
derive instance repGenericBatchDetachObjectResponse :: Generic BatchDetachObjectResponse _
instance showBatchDetachObjectResponse :: Show BatchDetachObjectResponse where
  show = genericShow
instance decodeBatchDetachObjectResponse :: Decode BatchDetachObjectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDetachObjectResponse :: Encode BatchDetachObjectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detaches the specified policy from the specified directory inside a <a>BatchWrite</a> operation. For more information, see <a>DetachPolicy</a> and <a>BatchWriteRequest$Operations</a>.</p>
newtype BatchDetachPolicy = BatchDetachPolicy 
  { "PolicyReference" :: (ObjectReference)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeBatchDetachPolicy :: Newtype BatchDetachPolicy _
derive instance repGenericBatchDetachPolicy :: Generic BatchDetachPolicy _
instance showBatchDetachPolicy :: Show BatchDetachPolicy where
  show = genericShow
instance decodeBatchDetachPolicy :: Decode BatchDetachPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDetachPolicy :: Encode BatchDetachPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>DetachPolicy</a> response operation.</p>
newtype BatchDetachPolicyResponse = BatchDetachPolicyResponse Types.NoArguments
derive instance newtypeBatchDetachPolicyResponse :: Newtype BatchDetachPolicyResponse _
derive instance repGenericBatchDetachPolicyResponse :: Generic BatchDetachPolicyResponse _
instance showBatchDetachPolicyResponse :: Show BatchDetachPolicyResponse where
  show = genericShow
instance decodeBatchDetachPolicyResponse :: Decode BatchDetachPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDetachPolicyResponse :: Encode BatchDetachPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detaches a typed link from a specified source and target object inside a <a>BatchRead</a> operation. For more information, see <a>DetachTypedLink</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchDetachTypedLink = BatchDetachTypedLink 
  { "TypedLinkSpecifier" :: (TypedLinkSpecifier)
  }
derive instance newtypeBatchDetachTypedLink :: Newtype BatchDetachTypedLink _
derive instance repGenericBatchDetachTypedLink :: Generic BatchDetachTypedLink _
instance showBatchDetachTypedLink :: Show BatchDetachTypedLink where
  show = genericShow
instance decodeBatchDetachTypedLink :: Decode BatchDetachTypedLink where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDetachTypedLink :: Encode BatchDetachTypedLink where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>DetachTypedLink</a> response operation.</p>
newtype BatchDetachTypedLinkResponse = BatchDetachTypedLinkResponse Types.NoArguments
derive instance newtypeBatchDetachTypedLinkResponse :: Newtype BatchDetachTypedLinkResponse _
derive instance repGenericBatchDetachTypedLinkResponse :: Generic BatchDetachTypedLinkResponse _
instance showBatchDetachTypedLinkResponse :: Show BatchDetachTypedLinkResponse where
  show = genericShow
instance decodeBatchDetachTypedLinkResponse :: Decode BatchDetachTypedLinkResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDetachTypedLinkResponse :: Encode BatchDetachTypedLinkResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Retrieves metadata about an object inside a <a>BatchRead</a> operation. For more information, see <a>GetObjectInformation</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchGetObjectInformation = BatchGetObjectInformation 
  { "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeBatchGetObjectInformation :: Newtype BatchGetObjectInformation _
derive instance repGenericBatchGetObjectInformation :: Generic BatchGetObjectInformation _
instance showBatchGetObjectInformation :: Show BatchGetObjectInformation where
  show = genericShow
instance decodeBatchGetObjectInformation :: Decode BatchGetObjectInformation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetObjectInformation :: Encode BatchGetObjectInformation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>GetObjectInformation</a> response operation.</p>
newtype BatchGetObjectInformationResponse = BatchGetObjectInformationResponse 
  { "SchemaFacets" :: NullOrUndefined.NullOrUndefined (SchemaFacetList)
  , "ObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchGetObjectInformationResponse :: Newtype BatchGetObjectInformationResponse _
derive instance repGenericBatchGetObjectInformationResponse :: Generic BatchGetObjectInformationResponse _
instance showBatchGetObjectInformationResponse :: Show BatchGetObjectInformationResponse where
  show = genericShow
instance decodeBatchGetObjectInformationResponse :: Decode BatchGetObjectInformationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetObjectInformationResponse :: Encode BatchGetObjectInformationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lists indices attached to an object inside a <a>BatchRead</a> operation. For more information, see <a>ListAttachedIndices</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListAttachedIndices = BatchListAttachedIndices 
  { "TargetReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListAttachedIndices :: Newtype BatchListAttachedIndices _
derive instance repGenericBatchListAttachedIndices :: Generic BatchListAttachedIndices _
instance showBatchListAttachedIndices :: Show BatchListAttachedIndices where
  show = genericShow
instance decodeBatchListAttachedIndices :: Decode BatchListAttachedIndices where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListAttachedIndices :: Encode BatchListAttachedIndices where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>ListAttachedIndices</a> response operation.</p>
newtype BatchListAttachedIndicesResponse = BatchListAttachedIndicesResponse 
  { "IndexAttachments" :: NullOrUndefined.NullOrUndefined (IndexAttachmentList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListAttachedIndicesResponse :: Newtype BatchListAttachedIndicesResponse _
derive instance repGenericBatchListAttachedIndicesResponse :: Generic BatchListAttachedIndicesResponse _
instance showBatchListAttachedIndicesResponse :: Show BatchListAttachedIndicesResponse where
  show = genericShow
instance decodeBatchListAttachedIndicesResponse :: Decode BatchListAttachedIndicesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListAttachedIndicesResponse :: Encode BatchListAttachedIndicesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns a paginated list of all the incoming <a>TypedLinkSpecifier</a> information for an object inside a <a>BatchRead</a> operation. For more information, see <a>ListIncomingTypedLinks</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListIncomingTypedLinks = BatchListIncomingTypedLinks 
  { "ObjectReference" :: (ObjectReference)
  , "FilterAttributeRanges" :: NullOrUndefined.NullOrUndefined (TypedLinkAttributeRangeList)
  , "FilterTypedLink" :: NullOrUndefined.NullOrUndefined (TypedLinkSchemaAndFacetName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListIncomingTypedLinks :: Newtype BatchListIncomingTypedLinks _
derive instance repGenericBatchListIncomingTypedLinks :: Generic BatchListIncomingTypedLinks _
instance showBatchListIncomingTypedLinks :: Show BatchListIncomingTypedLinks where
  show = genericShow
instance decodeBatchListIncomingTypedLinks :: Decode BatchListIncomingTypedLinks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListIncomingTypedLinks :: Encode BatchListIncomingTypedLinks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>ListIncomingTypedLinks</a> response operation.</p>
newtype BatchListIncomingTypedLinksResponse = BatchListIncomingTypedLinksResponse 
  { "LinkSpecifiers" :: NullOrUndefined.NullOrUndefined (TypedLinkSpecifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListIncomingTypedLinksResponse :: Newtype BatchListIncomingTypedLinksResponse _
derive instance repGenericBatchListIncomingTypedLinksResponse :: Generic BatchListIncomingTypedLinksResponse _
instance showBatchListIncomingTypedLinksResponse :: Show BatchListIncomingTypedLinksResponse where
  show = genericShow
instance decodeBatchListIncomingTypedLinksResponse :: Decode BatchListIncomingTypedLinksResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListIncomingTypedLinksResponse :: Encode BatchListIncomingTypedLinksResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lists objects attached to the specified index inside a <a>BatchRead</a> operation. For more information, see <a>ListIndex</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListIndex = BatchListIndex 
  { "RangesOnIndexedValues" :: NullOrUndefined.NullOrUndefined (ObjectAttributeRangeList)
  , "IndexReference" :: (ObjectReference)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListIndex :: Newtype BatchListIndex _
derive instance repGenericBatchListIndex :: Generic BatchListIndex _
instance showBatchListIndex :: Show BatchListIndex where
  show = genericShow
instance decodeBatchListIndex :: Decode BatchListIndex where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListIndex :: Encode BatchListIndex where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>ListIndex</a> response operation.</p>
newtype BatchListIndexResponse = BatchListIndexResponse 
  { "IndexAttachments" :: NullOrUndefined.NullOrUndefined (IndexAttachmentList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListIndexResponse :: Newtype BatchListIndexResponse _
derive instance repGenericBatchListIndexResponse :: Generic BatchListIndexResponse _
instance showBatchListIndexResponse :: Show BatchListIndexResponse where
  show = genericShow
instance decodeBatchListIndexResponse :: Decode BatchListIndexResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListIndexResponse :: Encode BatchListIndexResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>ListObjectAttributes</a> operation.</p>
newtype BatchListObjectAttributes = BatchListObjectAttributes 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  , "FacetFilter" :: NullOrUndefined.NullOrUndefined (SchemaFacet)
  }
derive instance newtypeBatchListObjectAttributes :: Newtype BatchListObjectAttributes _
derive instance repGenericBatchListObjectAttributes :: Generic BatchListObjectAttributes _
instance showBatchListObjectAttributes :: Show BatchListObjectAttributes where
  show = genericShow
instance decodeBatchListObjectAttributes :: Decode BatchListObjectAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListObjectAttributes :: Encode BatchListObjectAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>ListObjectAttributes</a> response operation.</p>
newtype BatchListObjectAttributesResponse = BatchListObjectAttributesResponse 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (AttributeKeyAndValueList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListObjectAttributesResponse :: Newtype BatchListObjectAttributesResponse _
derive instance repGenericBatchListObjectAttributesResponse :: Generic BatchListObjectAttributesResponse _
instance showBatchListObjectAttributesResponse :: Show BatchListObjectAttributesResponse where
  show = genericShow
instance decodeBatchListObjectAttributesResponse :: Decode BatchListObjectAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListObjectAttributesResponse :: Encode BatchListObjectAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>ListObjectChildren</a> operation.</p>
newtype BatchListObjectChildren = BatchListObjectChildren 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListObjectChildren :: Newtype BatchListObjectChildren _
derive instance repGenericBatchListObjectChildren :: Generic BatchListObjectChildren _
instance showBatchListObjectChildren :: Show BatchListObjectChildren where
  show = genericShow
instance decodeBatchListObjectChildren :: Decode BatchListObjectChildren where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListObjectChildren :: Encode BatchListObjectChildren where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>ListObjectChildren</a> response operation.</p>
newtype BatchListObjectChildrenResponse = BatchListObjectChildrenResponse 
  { "Children" :: NullOrUndefined.NullOrUndefined (LinkNameToObjectIdentifierMap)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListObjectChildrenResponse :: Newtype BatchListObjectChildrenResponse _
derive instance repGenericBatchListObjectChildrenResponse :: Generic BatchListObjectChildrenResponse _
instance showBatchListObjectChildrenResponse :: Show BatchListObjectChildrenResponse where
  show = genericShow
instance decodeBatchListObjectChildrenResponse :: Decode BatchListObjectChildrenResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListObjectChildrenResponse :: Encode BatchListObjectChildrenResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects inside a <a>BatchRead</a> operation. For more information, see <a>ListObjectParentPaths</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListObjectParentPaths = BatchListObjectParentPaths 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListObjectParentPaths :: Newtype BatchListObjectParentPaths _
derive instance repGenericBatchListObjectParentPaths :: Generic BatchListObjectParentPaths _
instance showBatchListObjectParentPaths :: Show BatchListObjectParentPaths where
  show = genericShow
instance decodeBatchListObjectParentPaths :: Decode BatchListObjectParentPaths where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListObjectParentPaths :: Encode BatchListObjectParentPaths where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>ListObjectParentPaths</a> response operation.</p>
newtype BatchListObjectParentPathsResponse = BatchListObjectParentPathsResponse 
  { "PathToObjectIdentifiersList" :: NullOrUndefined.NullOrUndefined (PathToObjectIdentifiersList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListObjectParentPathsResponse :: Newtype BatchListObjectParentPathsResponse _
derive instance repGenericBatchListObjectParentPathsResponse :: Generic BatchListObjectParentPathsResponse _
instance showBatchListObjectParentPathsResponse :: Show BatchListObjectParentPathsResponse where
  show = genericShow
instance decodeBatchListObjectParentPathsResponse :: Decode BatchListObjectParentPathsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListObjectParentPathsResponse :: Encode BatchListObjectParentPathsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns policies attached to an object in pagination fashion inside a <a>BatchRead</a> operation. For more information, see <a>ListObjectPolicies</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListObjectPolicies = BatchListObjectPolicies 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListObjectPolicies :: Newtype BatchListObjectPolicies _
derive instance repGenericBatchListObjectPolicies :: Generic BatchListObjectPolicies _
instance showBatchListObjectPolicies :: Show BatchListObjectPolicies where
  show = genericShow
instance decodeBatchListObjectPolicies :: Decode BatchListObjectPolicies where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListObjectPolicies :: Encode BatchListObjectPolicies where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>ListObjectPolicies</a> response operation.</p>
newtype BatchListObjectPoliciesResponse = BatchListObjectPoliciesResponse 
  { "AttachedPolicyIds" :: NullOrUndefined.NullOrUndefined (ObjectIdentifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListObjectPoliciesResponse :: Newtype BatchListObjectPoliciesResponse _
derive instance repGenericBatchListObjectPoliciesResponse :: Generic BatchListObjectPoliciesResponse _
instance showBatchListObjectPoliciesResponse :: Show BatchListObjectPoliciesResponse where
  show = genericShow
instance decodeBatchListObjectPoliciesResponse :: Decode BatchListObjectPoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListObjectPoliciesResponse :: Encode BatchListObjectPoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns a paginated list of all the outgoing <a>TypedLinkSpecifier</a> information for an object inside a <a>BatchRead</a> operation. For more information, see <a>ListOutgoingTypedLinks</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListOutgoingTypedLinks = BatchListOutgoingTypedLinks 
  { "ObjectReference" :: (ObjectReference)
  , "FilterAttributeRanges" :: NullOrUndefined.NullOrUndefined (TypedLinkAttributeRangeList)
  , "FilterTypedLink" :: NullOrUndefined.NullOrUndefined (TypedLinkSchemaAndFacetName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListOutgoingTypedLinks :: Newtype BatchListOutgoingTypedLinks _
derive instance repGenericBatchListOutgoingTypedLinks :: Generic BatchListOutgoingTypedLinks _
instance showBatchListOutgoingTypedLinks :: Show BatchListOutgoingTypedLinks where
  show = genericShow
instance decodeBatchListOutgoingTypedLinks :: Decode BatchListOutgoingTypedLinks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListOutgoingTypedLinks :: Encode BatchListOutgoingTypedLinks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>ListOutgoingTypedLinks</a> response operation.</p>
newtype BatchListOutgoingTypedLinksResponse = BatchListOutgoingTypedLinksResponse 
  { "TypedLinkSpecifiers" :: NullOrUndefined.NullOrUndefined (TypedLinkSpecifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListOutgoingTypedLinksResponse :: Newtype BatchListOutgoingTypedLinksResponse _
derive instance repGenericBatchListOutgoingTypedLinksResponse :: Generic BatchListOutgoingTypedLinksResponse _
instance showBatchListOutgoingTypedLinksResponse :: Show BatchListOutgoingTypedLinksResponse where
  show = genericShow
instance decodeBatchListOutgoingTypedLinksResponse :: Decode BatchListOutgoingTypedLinksResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListOutgoingTypedLinksResponse :: Encode BatchListOutgoingTypedLinksResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns all of the <code>ObjectIdentifiers</code> to which a given policy is attached inside a <a>BatchRead</a> operation. For more information, see <a>ListPolicyAttachments</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchListPolicyAttachments = BatchListPolicyAttachments 
  { "PolicyReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchListPolicyAttachments :: Newtype BatchListPolicyAttachments _
derive instance repGenericBatchListPolicyAttachments :: Generic BatchListPolicyAttachments _
instance showBatchListPolicyAttachments :: Show BatchListPolicyAttachments where
  show = genericShow
instance decodeBatchListPolicyAttachments :: Decode BatchListPolicyAttachments where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListPolicyAttachments :: Encode BatchListPolicyAttachments where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>ListPolicyAttachments</a> response operation.</p>
newtype BatchListPolicyAttachmentsResponse = BatchListPolicyAttachmentsResponse 
  { "ObjectIdentifiers" :: NullOrUndefined.NullOrUndefined (ObjectIdentifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeBatchListPolicyAttachmentsResponse :: Newtype BatchListPolicyAttachmentsResponse _
derive instance repGenericBatchListPolicyAttachmentsResponse :: Generic BatchListPolicyAttachmentsResponse _
instance showBatchListPolicyAttachmentsResponse :: Show BatchListPolicyAttachmentsResponse where
  show = genericShow
instance decodeBatchListPolicyAttachmentsResponse :: Decode BatchListPolicyAttachmentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchListPolicyAttachmentsResponse :: Encode BatchListPolicyAttachmentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lists all policies from the root of the Directory to the object specified inside a <a>BatchRead</a> operation. For more information, see <a>LookupPolicy</a> and <a>BatchReadRequest$Operations</a>.</p>
newtype BatchLookupPolicy = BatchLookupPolicy 
  { "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeBatchLookupPolicy :: Newtype BatchLookupPolicy _
derive instance repGenericBatchLookupPolicy :: Generic BatchLookupPolicy _
instance showBatchLookupPolicy :: Show BatchLookupPolicy where
  show = genericShow
instance decodeBatchLookupPolicy :: Decode BatchLookupPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchLookupPolicy :: Encode BatchLookupPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <a>LookupPolicy</a> response operation.</p>
newtype BatchLookupPolicyResponse = BatchLookupPolicyResponse 
  { "PolicyToPathList" :: NullOrUndefined.NullOrUndefined (PolicyToPathList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeBatchLookupPolicyResponse :: Newtype BatchLookupPolicyResponse _
derive instance repGenericBatchLookupPolicyResponse :: Generic BatchLookupPolicyResponse _
instance showBatchLookupPolicyResponse :: Show BatchLookupPolicyResponse where
  show = genericShow
instance decodeBatchLookupPolicyResponse :: Decode BatchLookupPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchLookupPolicyResponse :: Encode BatchLookupPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchOperationIndex = BatchOperationIndex Int
derive instance newtypeBatchOperationIndex :: Newtype BatchOperationIndex _
derive instance repGenericBatchOperationIndex :: Generic BatchOperationIndex _
instance showBatchOperationIndex :: Show BatchOperationIndex where
  show = genericShow
instance decodeBatchOperationIndex :: Decode BatchOperationIndex where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchOperationIndex :: Encode BatchOperationIndex where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The batch read exception structure, which contains the exception type and message.</p>
newtype BatchReadException = BatchReadException 
  { "Type" :: NullOrUndefined.NullOrUndefined (BatchReadExceptionType)
  , "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeBatchReadException :: Newtype BatchReadException _
derive instance repGenericBatchReadException :: Generic BatchReadException _
instance showBatchReadException :: Show BatchReadException where
  show = genericShow
instance decodeBatchReadException :: Decode BatchReadException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchReadException :: Encode BatchReadException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchReadExceptionType = BatchReadExceptionType String
derive instance newtypeBatchReadExceptionType :: Newtype BatchReadExceptionType _
derive instance repGenericBatchReadExceptionType :: Generic BatchReadExceptionType _
instance showBatchReadExceptionType :: Show BatchReadExceptionType where
  show = genericShow
instance decodeBatchReadExceptionType :: Decode BatchReadExceptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchReadExceptionType :: Encode BatchReadExceptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>BatchRead</code> operation.</p>
newtype BatchReadOperation = BatchReadOperation 
  { "ListObjectAttributes" :: NullOrUndefined.NullOrUndefined (BatchListObjectAttributes)
  , "ListObjectChildren" :: NullOrUndefined.NullOrUndefined (BatchListObjectChildren)
  , "ListAttachedIndices" :: NullOrUndefined.NullOrUndefined (BatchListAttachedIndices)
  , "ListObjectParentPaths" :: NullOrUndefined.NullOrUndefined (BatchListObjectParentPaths)
  , "GetObjectInformation" :: NullOrUndefined.NullOrUndefined (BatchGetObjectInformation)
  , "ListObjectPolicies" :: NullOrUndefined.NullOrUndefined (BatchListObjectPolicies)
  , "ListPolicyAttachments" :: NullOrUndefined.NullOrUndefined (BatchListPolicyAttachments)
  , "LookupPolicy" :: NullOrUndefined.NullOrUndefined (BatchLookupPolicy)
  , "ListIndex" :: NullOrUndefined.NullOrUndefined (BatchListIndex)
  , "ListOutgoingTypedLinks" :: NullOrUndefined.NullOrUndefined (BatchListOutgoingTypedLinks)
  , "ListIncomingTypedLinks" :: NullOrUndefined.NullOrUndefined (BatchListIncomingTypedLinks)
  }
derive instance newtypeBatchReadOperation :: Newtype BatchReadOperation _
derive instance repGenericBatchReadOperation :: Generic BatchReadOperation _
instance showBatchReadOperation :: Show BatchReadOperation where
  show = genericShow
instance decodeBatchReadOperation :: Decode BatchReadOperation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchReadOperation :: Encode BatchReadOperation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchReadOperationList = BatchReadOperationList (Array BatchReadOperation)
derive instance newtypeBatchReadOperationList :: Newtype BatchReadOperationList _
derive instance repGenericBatchReadOperationList :: Generic BatchReadOperationList _
instance showBatchReadOperationList :: Show BatchReadOperationList where
  show = genericShow
instance decodeBatchReadOperationList :: Decode BatchReadOperationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchReadOperationList :: Encode BatchReadOperationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>BatchRead</code> response operation.</p>
newtype BatchReadOperationResponse = BatchReadOperationResponse 
  { "SuccessfulResponse" :: NullOrUndefined.NullOrUndefined (BatchReadSuccessfulResponse)
  , "ExceptionResponse" :: NullOrUndefined.NullOrUndefined (BatchReadException)
  }
derive instance newtypeBatchReadOperationResponse :: Newtype BatchReadOperationResponse _
derive instance repGenericBatchReadOperationResponse :: Generic BatchReadOperationResponse _
instance showBatchReadOperationResponse :: Show BatchReadOperationResponse where
  show = genericShow
instance decodeBatchReadOperationResponse :: Decode BatchReadOperationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchReadOperationResponse :: Encode BatchReadOperationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchReadOperationResponseList = BatchReadOperationResponseList (Array BatchReadOperationResponse)
derive instance newtypeBatchReadOperationResponseList :: Newtype BatchReadOperationResponseList _
derive instance repGenericBatchReadOperationResponseList :: Generic BatchReadOperationResponseList _
instance showBatchReadOperationResponseList :: Show BatchReadOperationResponseList where
  show = genericShow
instance decodeBatchReadOperationResponseList :: Decode BatchReadOperationResponseList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchReadOperationResponseList :: Encode BatchReadOperationResponseList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchReadRequest = BatchReadRequest 
  { "DirectoryArn" :: (Arn)
  , "Operations" :: (BatchReadOperationList)
  , "ConsistencyLevel" :: NullOrUndefined.NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeBatchReadRequest :: Newtype BatchReadRequest _
derive instance repGenericBatchReadRequest :: Generic BatchReadRequest _
instance showBatchReadRequest :: Show BatchReadRequest where
  show = genericShow
instance decodeBatchReadRequest :: Decode BatchReadRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchReadRequest :: Encode BatchReadRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchReadResponse = BatchReadResponse 
  { "Responses" :: NullOrUndefined.NullOrUndefined (BatchReadOperationResponseList)
  }
derive instance newtypeBatchReadResponse :: Newtype BatchReadResponse _
derive instance repGenericBatchReadResponse :: Generic BatchReadResponse _
instance showBatchReadResponse :: Show BatchReadResponse where
  show = genericShow
instance decodeBatchReadResponse :: Decode BatchReadResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchReadResponse :: Encode BatchReadResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>BatchRead</code> success response operation.</p>
newtype BatchReadSuccessfulResponse = BatchReadSuccessfulResponse 
  { "ListObjectAttributes" :: NullOrUndefined.NullOrUndefined (BatchListObjectAttributesResponse)
  , "ListObjectChildren" :: NullOrUndefined.NullOrUndefined (BatchListObjectChildrenResponse)
  , "GetObjectInformation" :: NullOrUndefined.NullOrUndefined (BatchGetObjectInformationResponse)
  , "ListAttachedIndices" :: NullOrUndefined.NullOrUndefined (BatchListAttachedIndicesResponse)
  , "ListObjectParentPaths" :: NullOrUndefined.NullOrUndefined (BatchListObjectParentPathsResponse)
  , "ListObjectPolicies" :: NullOrUndefined.NullOrUndefined (BatchListObjectPoliciesResponse)
  , "ListPolicyAttachments" :: NullOrUndefined.NullOrUndefined (BatchListPolicyAttachmentsResponse)
  , "LookupPolicy" :: NullOrUndefined.NullOrUndefined (BatchLookupPolicyResponse)
  , "ListIndex" :: NullOrUndefined.NullOrUndefined (BatchListIndexResponse)
  , "ListOutgoingTypedLinks" :: NullOrUndefined.NullOrUndefined (BatchListOutgoingTypedLinksResponse)
  , "ListIncomingTypedLinks" :: NullOrUndefined.NullOrUndefined (BatchListIncomingTypedLinksResponse)
  }
derive instance newtypeBatchReadSuccessfulResponse :: Newtype BatchReadSuccessfulResponse _
derive instance repGenericBatchReadSuccessfulResponse :: Generic BatchReadSuccessfulResponse _
instance showBatchReadSuccessfulResponse :: Show BatchReadSuccessfulResponse where
  show = genericShow
instance decodeBatchReadSuccessfulResponse :: Decode BatchReadSuccessfulResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchReadSuccessfulResponse :: Encode BatchReadSuccessfulResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchReferenceName = BatchReferenceName String
derive instance newtypeBatchReferenceName :: Newtype BatchReferenceName _
derive instance repGenericBatchReferenceName :: Generic BatchReferenceName _
instance showBatchReferenceName :: Show BatchReferenceName where
  show = genericShow
instance decodeBatchReferenceName :: Decode BatchReferenceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchReferenceName :: Encode BatchReferenceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A batch operation to remove a facet from an object.</p>
newtype BatchRemoveFacetFromObject = BatchRemoveFacetFromObject 
  { "SchemaFacet" :: (SchemaFacet)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeBatchRemoveFacetFromObject :: Newtype BatchRemoveFacetFromObject _
derive instance repGenericBatchRemoveFacetFromObject :: Generic BatchRemoveFacetFromObject _
instance showBatchRemoveFacetFromObject :: Show BatchRemoveFacetFromObject where
  show = genericShow
instance decodeBatchRemoveFacetFromObject :: Decode BatchRemoveFacetFromObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchRemoveFacetFromObject :: Encode BatchRemoveFacetFromObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty result that represents success.</p>
newtype BatchRemoveFacetFromObjectResponse = BatchRemoveFacetFromObjectResponse Types.NoArguments
derive instance newtypeBatchRemoveFacetFromObjectResponse :: Newtype BatchRemoveFacetFromObjectResponse _
derive instance repGenericBatchRemoveFacetFromObjectResponse :: Generic BatchRemoveFacetFromObjectResponse _
instance showBatchRemoveFacetFromObjectResponse :: Show BatchRemoveFacetFromObjectResponse where
  show = genericShow
instance decodeBatchRemoveFacetFromObjectResponse :: Decode BatchRemoveFacetFromObjectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchRemoveFacetFromObjectResponse :: Encode BatchRemoveFacetFromObjectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>BatchUpdate</code> operation. </p>
newtype BatchUpdateObjectAttributes = BatchUpdateObjectAttributes 
  { "ObjectReference" :: (ObjectReference)
  , "AttributeUpdates" :: (ObjectAttributeUpdateList)
  }
derive instance newtypeBatchUpdateObjectAttributes :: Newtype BatchUpdateObjectAttributes _
derive instance repGenericBatchUpdateObjectAttributes :: Generic BatchUpdateObjectAttributes _
instance showBatchUpdateObjectAttributes :: Show BatchUpdateObjectAttributes where
  show = genericShow
instance decodeBatchUpdateObjectAttributes :: Decode BatchUpdateObjectAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchUpdateObjectAttributes :: Encode BatchUpdateObjectAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>BatchUpdate</code> response operation.</p>
newtype BatchUpdateObjectAttributesResponse = BatchUpdateObjectAttributesResponse 
  { "ObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeBatchUpdateObjectAttributesResponse :: Newtype BatchUpdateObjectAttributesResponse _
derive instance repGenericBatchUpdateObjectAttributesResponse :: Generic BatchUpdateObjectAttributesResponse _
instance showBatchUpdateObjectAttributesResponse :: Show BatchUpdateObjectAttributesResponse where
  show = genericShow
instance decodeBatchUpdateObjectAttributesResponse :: Decode BatchUpdateObjectAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchUpdateObjectAttributesResponse :: Encode BatchUpdateObjectAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A <code>BatchWrite</code> exception has occurred.</p>
newtype BatchWriteException = BatchWriteException 
  { "Index" :: NullOrUndefined.NullOrUndefined (BatchOperationIndex)
  , "Type" :: NullOrUndefined.NullOrUndefined (BatchWriteExceptionType)
  , "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeBatchWriteException :: Newtype BatchWriteException _
derive instance repGenericBatchWriteException :: Generic BatchWriteException _
instance showBatchWriteException :: Show BatchWriteException where
  show = genericShow
instance decodeBatchWriteException :: Decode BatchWriteException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchWriteException :: Encode BatchWriteException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchWriteExceptionType = BatchWriteExceptionType String
derive instance newtypeBatchWriteExceptionType :: Newtype BatchWriteExceptionType _
derive instance repGenericBatchWriteExceptionType :: Generic BatchWriteExceptionType _
instance showBatchWriteExceptionType :: Show BatchWriteExceptionType where
  show = genericShow
instance decodeBatchWriteExceptionType :: Decode BatchWriteExceptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchWriteExceptionType :: Encode BatchWriteExceptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>BatchWrite</code> operation. </p>
newtype BatchWriteOperation = BatchWriteOperation 
  { "CreateObject" :: NullOrUndefined.NullOrUndefined (BatchCreateObject)
  , "AttachObject" :: NullOrUndefined.NullOrUndefined (BatchAttachObject)
  , "DetachObject" :: NullOrUndefined.NullOrUndefined (BatchDetachObject)
  , "UpdateObjectAttributes" :: NullOrUndefined.NullOrUndefined (BatchUpdateObjectAttributes)
  , "DeleteObject" :: NullOrUndefined.NullOrUndefined (BatchDeleteObject)
  , "AddFacetToObject" :: NullOrUndefined.NullOrUndefined (BatchAddFacetToObject)
  , "RemoveFacetFromObject" :: NullOrUndefined.NullOrUndefined (BatchRemoveFacetFromObject)
  , "AttachPolicy" :: NullOrUndefined.NullOrUndefined (BatchAttachPolicy)
  , "DetachPolicy" :: NullOrUndefined.NullOrUndefined (BatchDetachPolicy)
  , "CreateIndex" :: NullOrUndefined.NullOrUndefined (BatchCreateIndex)
  , "AttachToIndex" :: NullOrUndefined.NullOrUndefined (BatchAttachToIndex)
  , "DetachFromIndex" :: NullOrUndefined.NullOrUndefined (BatchDetachFromIndex)
  , "AttachTypedLink" :: NullOrUndefined.NullOrUndefined (BatchAttachTypedLink)
  , "DetachTypedLink" :: NullOrUndefined.NullOrUndefined (BatchDetachTypedLink)
  }
derive instance newtypeBatchWriteOperation :: Newtype BatchWriteOperation _
derive instance repGenericBatchWriteOperation :: Generic BatchWriteOperation _
instance showBatchWriteOperation :: Show BatchWriteOperation where
  show = genericShow
instance decodeBatchWriteOperation :: Decode BatchWriteOperation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchWriteOperation :: Encode BatchWriteOperation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchWriteOperationList = BatchWriteOperationList (Array BatchWriteOperation)
derive instance newtypeBatchWriteOperationList :: Newtype BatchWriteOperationList _
derive instance repGenericBatchWriteOperationList :: Generic BatchWriteOperationList _
instance showBatchWriteOperationList :: Show BatchWriteOperationList where
  show = genericShow
instance decodeBatchWriteOperationList :: Decode BatchWriteOperationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchWriteOperationList :: Encode BatchWriteOperationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>BatchWrite</code> response operation.</p>
newtype BatchWriteOperationResponse = BatchWriteOperationResponse 
  { "CreateObject" :: NullOrUndefined.NullOrUndefined (BatchCreateObjectResponse)
  , "AttachObject" :: NullOrUndefined.NullOrUndefined (BatchAttachObjectResponse)
  , "DetachObject" :: NullOrUndefined.NullOrUndefined (BatchDetachObjectResponse)
  , "UpdateObjectAttributes" :: NullOrUndefined.NullOrUndefined (BatchUpdateObjectAttributesResponse)
  , "DeleteObject" :: NullOrUndefined.NullOrUndefined (BatchDeleteObjectResponse)
  , "AddFacetToObject" :: NullOrUndefined.NullOrUndefined (BatchAddFacetToObjectResponse)
  , "RemoveFacetFromObject" :: NullOrUndefined.NullOrUndefined (BatchRemoveFacetFromObjectResponse)
  , "AttachPolicy" :: NullOrUndefined.NullOrUndefined (BatchAttachPolicyResponse)
  , "DetachPolicy" :: NullOrUndefined.NullOrUndefined (BatchDetachPolicyResponse)
  , "CreateIndex" :: NullOrUndefined.NullOrUndefined (BatchCreateIndexResponse)
  , "AttachToIndex" :: NullOrUndefined.NullOrUndefined (BatchAttachToIndexResponse)
  , "DetachFromIndex" :: NullOrUndefined.NullOrUndefined (BatchDetachFromIndexResponse)
  , "AttachTypedLink" :: NullOrUndefined.NullOrUndefined (BatchAttachTypedLinkResponse)
  , "DetachTypedLink" :: NullOrUndefined.NullOrUndefined (BatchDetachTypedLinkResponse)
  }
derive instance newtypeBatchWriteOperationResponse :: Newtype BatchWriteOperationResponse _
derive instance repGenericBatchWriteOperationResponse :: Generic BatchWriteOperationResponse _
instance showBatchWriteOperationResponse :: Show BatchWriteOperationResponse where
  show = genericShow
instance decodeBatchWriteOperationResponse :: Decode BatchWriteOperationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchWriteOperationResponse :: Encode BatchWriteOperationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchWriteOperationResponseList = BatchWriteOperationResponseList (Array BatchWriteOperationResponse)
derive instance newtypeBatchWriteOperationResponseList :: Newtype BatchWriteOperationResponseList _
derive instance repGenericBatchWriteOperationResponseList :: Generic BatchWriteOperationResponseList _
instance showBatchWriteOperationResponseList :: Show BatchWriteOperationResponseList where
  show = genericShow
instance decodeBatchWriteOperationResponseList :: Decode BatchWriteOperationResponseList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchWriteOperationResponseList :: Encode BatchWriteOperationResponseList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchWriteRequest = BatchWriteRequest 
  { "DirectoryArn" :: (Arn)
  , "Operations" :: (BatchWriteOperationList)
  }
derive instance newtypeBatchWriteRequest :: Newtype BatchWriteRequest _
derive instance repGenericBatchWriteRequest :: Generic BatchWriteRequest _
instance showBatchWriteRequest :: Show BatchWriteRequest where
  show = genericShow
instance decodeBatchWriteRequest :: Decode BatchWriteRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchWriteRequest :: Encode BatchWriteRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchWriteResponse = BatchWriteResponse 
  { "Responses" :: NullOrUndefined.NullOrUndefined (BatchWriteOperationResponseList)
  }
derive instance newtypeBatchWriteResponse :: Newtype BatchWriteResponse _
derive instance repGenericBatchWriteResponse :: Generic BatchWriteResponse _
instance showBatchWriteResponse :: Show BatchWriteResponse where
  show = genericShow
instance decodeBatchWriteResponse :: Decode BatchWriteResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchWriteResponse :: Encode BatchWriteResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BinaryAttributeValue = BinaryAttributeValue String
derive instance newtypeBinaryAttributeValue :: Newtype BinaryAttributeValue _
derive instance repGenericBinaryAttributeValue :: Generic BinaryAttributeValue _
instance showBinaryAttributeValue :: Show BinaryAttributeValue where
  show = genericShow
instance decodeBinaryAttributeValue :: Decode BinaryAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBinaryAttributeValue :: Encode BinaryAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Bool = Bool Boolean
derive instance newtypeBool :: Newtype Bool _
derive instance repGenericBool :: Generic Bool _
instance showBool :: Show Bool where
  show = genericShow
instance decodeBool :: Decode Bool where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBool :: Encode Bool where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanAttributeValue = BooleanAttributeValue Boolean
derive instance newtypeBooleanAttributeValue :: Newtype BooleanAttributeValue _
derive instance repGenericBooleanAttributeValue :: Generic BooleanAttributeValue _
instance showBooleanAttributeValue :: Show BooleanAttributeValue where
  show = genericShow
instance decodeBooleanAttributeValue :: Decode BooleanAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanAttributeValue :: Encode BooleanAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Cannot list the parents of a <a>Directory</a> root.</p>
newtype CannotListParentOfRootException = CannotListParentOfRootException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeCannotListParentOfRootException :: Newtype CannotListParentOfRootException _
derive instance repGenericCannotListParentOfRootException :: Generic CannotListParentOfRootException _
instance showCannotListParentOfRootException :: Show CannotListParentOfRootException where
  show = genericShow
instance decodeCannotListParentOfRootException :: Decode CannotListParentOfRootException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCannotListParentOfRootException :: Encode CannotListParentOfRootException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConsistencyLevel = ConsistencyLevel String
derive instance newtypeConsistencyLevel :: Newtype ConsistencyLevel _
derive instance repGenericConsistencyLevel :: Generic ConsistencyLevel _
instance showConsistencyLevel :: Show ConsistencyLevel where
  show = genericShow
instance decodeConsistencyLevel :: Decode ConsistencyLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConsistencyLevel :: Encode ConsistencyLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDirectoryRequest = CreateDirectoryRequest 
  { "Name" :: (DirectoryName)
  , "SchemaArn" :: (Arn)
  }
derive instance newtypeCreateDirectoryRequest :: Newtype CreateDirectoryRequest _
derive instance repGenericCreateDirectoryRequest :: Generic CreateDirectoryRequest _
instance showCreateDirectoryRequest :: Show CreateDirectoryRequest where
  show = genericShow
instance decodeCreateDirectoryRequest :: Decode CreateDirectoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDirectoryRequest :: Encode CreateDirectoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDirectoryResponse = CreateDirectoryResponse 
  { "DirectoryArn" :: (DirectoryArn)
  , "Name" :: (DirectoryName)
  , "ObjectIdentifier" :: (ObjectIdentifier)
  , "AppliedSchemaArn" :: (Arn)
  }
derive instance newtypeCreateDirectoryResponse :: Newtype CreateDirectoryResponse _
derive instance repGenericCreateDirectoryResponse :: Generic CreateDirectoryResponse _
instance showCreateDirectoryResponse :: Show CreateDirectoryResponse where
  show = genericShow
instance decodeCreateDirectoryResponse :: Decode CreateDirectoryResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDirectoryResponse :: Encode CreateDirectoryResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateFacetRequest = CreateFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (FacetAttributeList)
  , "ObjectType" :: (ObjectType)
  }
derive instance newtypeCreateFacetRequest :: Newtype CreateFacetRequest _
derive instance repGenericCreateFacetRequest :: Generic CreateFacetRequest _
instance showCreateFacetRequest :: Show CreateFacetRequest where
  show = genericShow
instance decodeCreateFacetRequest :: Decode CreateFacetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateFacetRequest :: Encode CreateFacetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateFacetResponse = CreateFacetResponse Types.NoArguments
derive instance newtypeCreateFacetResponse :: Newtype CreateFacetResponse _
derive instance repGenericCreateFacetResponse :: Generic CreateFacetResponse _
instance showCreateFacetResponse :: Show CreateFacetResponse where
  show = genericShow
instance decodeCreateFacetResponse :: Decode CreateFacetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateFacetResponse :: Encode CreateFacetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateIndexRequest = CreateIndexRequest 
  { "DirectoryArn" :: (Arn)
  , "OrderedIndexedAttributeList" :: (AttributeKeyList)
  , "IsUnique" :: (Bool)
  , "ParentReference" :: NullOrUndefined.NullOrUndefined (ObjectReference)
  , "LinkName" :: NullOrUndefined.NullOrUndefined (LinkName)
  }
derive instance newtypeCreateIndexRequest :: Newtype CreateIndexRequest _
derive instance repGenericCreateIndexRequest :: Generic CreateIndexRequest _
instance showCreateIndexRequest :: Show CreateIndexRequest where
  show = genericShow
instance decodeCreateIndexRequest :: Decode CreateIndexRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateIndexRequest :: Encode CreateIndexRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateIndexResponse = CreateIndexResponse 
  { "ObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeCreateIndexResponse :: Newtype CreateIndexResponse _
derive instance repGenericCreateIndexResponse :: Generic CreateIndexResponse _
instance showCreateIndexResponse :: Show CreateIndexResponse where
  show = genericShow
instance decodeCreateIndexResponse :: Decode CreateIndexResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateIndexResponse :: Encode CreateIndexResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateObjectRequest = CreateObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "SchemaFacets" :: (SchemaFacetList)
  , "ObjectAttributeList" :: NullOrUndefined.NullOrUndefined (AttributeKeyAndValueList)
  , "ParentReference" :: NullOrUndefined.NullOrUndefined (ObjectReference)
  , "LinkName" :: NullOrUndefined.NullOrUndefined (LinkName)
  }
derive instance newtypeCreateObjectRequest :: Newtype CreateObjectRequest _
derive instance repGenericCreateObjectRequest :: Generic CreateObjectRequest _
instance showCreateObjectRequest :: Show CreateObjectRequest where
  show = genericShow
instance decodeCreateObjectRequest :: Decode CreateObjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateObjectRequest :: Encode CreateObjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateObjectResponse = CreateObjectResponse 
  { "ObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeCreateObjectResponse :: Newtype CreateObjectResponse _
derive instance repGenericCreateObjectResponse :: Generic CreateObjectResponse _
instance showCreateObjectResponse :: Show CreateObjectResponse where
  show = genericShow
instance decodeCreateObjectResponse :: Decode CreateObjectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateObjectResponse :: Encode CreateObjectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSchemaRequest = CreateSchemaRequest 
  { "Name" :: (SchemaName)
  }
derive instance newtypeCreateSchemaRequest :: Newtype CreateSchemaRequest _
derive instance repGenericCreateSchemaRequest :: Generic CreateSchemaRequest _
instance showCreateSchemaRequest :: Show CreateSchemaRequest where
  show = genericShow
instance decodeCreateSchemaRequest :: Decode CreateSchemaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSchemaRequest :: Encode CreateSchemaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSchemaResponse = CreateSchemaResponse 
  { "SchemaArn" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeCreateSchemaResponse :: Newtype CreateSchemaResponse _
derive instance repGenericCreateSchemaResponse :: Generic CreateSchemaResponse _
instance showCreateSchemaResponse :: Show CreateSchemaResponse where
  show = genericShow
instance decodeCreateSchemaResponse :: Decode CreateSchemaResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSchemaResponse :: Encode CreateSchemaResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTypedLinkFacetRequest = CreateTypedLinkFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Facet" :: (TypedLinkFacet)
  }
derive instance newtypeCreateTypedLinkFacetRequest :: Newtype CreateTypedLinkFacetRequest _
derive instance repGenericCreateTypedLinkFacetRequest :: Generic CreateTypedLinkFacetRequest _
instance showCreateTypedLinkFacetRequest :: Show CreateTypedLinkFacetRequest where
  show = genericShow
instance decodeCreateTypedLinkFacetRequest :: Decode CreateTypedLinkFacetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTypedLinkFacetRequest :: Encode CreateTypedLinkFacetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTypedLinkFacetResponse = CreateTypedLinkFacetResponse Types.NoArguments
derive instance newtypeCreateTypedLinkFacetResponse :: Newtype CreateTypedLinkFacetResponse _
derive instance repGenericCreateTypedLinkFacetResponse :: Generic CreateTypedLinkFacetResponse _
instance showCreateTypedLinkFacetResponse :: Show CreateTypedLinkFacetResponse where
  show = genericShow
instance decodeCreateTypedLinkFacetResponse :: Decode CreateTypedLinkFacetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTypedLinkFacetResponse :: Encode CreateTypedLinkFacetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _
derive instance repGenericDate :: Generic Date _
instance showDate :: Show Date where
  show = genericShow
instance decodeDate :: Decode Date where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDate :: Encode Date where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DatetimeAttributeValue = DatetimeAttributeValue Number
derive instance newtypeDatetimeAttributeValue :: Newtype DatetimeAttributeValue _
derive instance repGenericDatetimeAttributeValue :: Generic DatetimeAttributeValue _
instance showDatetimeAttributeValue :: Show DatetimeAttributeValue where
  show = genericShow
instance decodeDatetimeAttributeValue :: Decode DatetimeAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDatetimeAttributeValue :: Encode DatetimeAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDirectoryRequest = DeleteDirectoryRequest 
  { "DirectoryArn" :: (Arn)
  }
derive instance newtypeDeleteDirectoryRequest :: Newtype DeleteDirectoryRequest _
derive instance repGenericDeleteDirectoryRequest :: Generic DeleteDirectoryRequest _
instance showDeleteDirectoryRequest :: Show DeleteDirectoryRequest where
  show = genericShow
instance decodeDeleteDirectoryRequest :: Decode DeleteDirectoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDirectoryRequest :: Encode DeleteDirectoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDirectoryResponse = DeleteDirectoryResponse 
  { "DirectoryArn" :: (Arn)
  }
derive instance newtypeDeleteDirectoryResponse :: Newtype DeleteDirectoryResponse _
derive instance repGenericDeleteDirectoryResponse :: Generic DeleteDirectoryResponse _
instance showDeleteDirectoryResponse :: Show DeleteDirectoryResponse where
  show = genericShow
instance decodeDeleteDirectoryResponse :: Decode DeleteDirectoryResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDirectoryResponse :: Encode DeleteDirectoryResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteFacetRequest = DeleteFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  }
derive instance newtypeDeleteFacetRequest :: Newtype DeleteFacetRequest _
derive instance repGenericDeleteFacetRequest :: Generic DeleteFacetRequest _
instance showDeleteFacetRequest :: Show DeleteFacetRequest where
  show = genericShow
instance decodeDeleteFacetRequest :: Decode DeleteFacetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteFacetRequest :: Encode DeleteFacetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteFacetResponse = DeleteFacetResponse Types.NoArguments
derive instance newtypeDeleteFacetResponse :: Newtype DeleteFacetResponse _
derive instance repGenericDeleteFacetResponse :: Generic DeleteFacetResponse _
instance showDeleteFacetResponse :: Show DeleteFacetResponse where
  show = genericShow
instance decodeDeleteFacetResponse :: Decode DeleteFacetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteFacetResponse :: Encode DeleteFacetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteObjectRequest = DeleteObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeDeleteObjectRequest :: Newtype DeleteObjectRequest _
derive instance repGenericDeleteObjectRequest :: Generic DeleteObjectRequest _
instance showDeleteObjectRequest :: Show DeleteObjectRequest where
  show = genericShow
instance decodeDeleteObjectRequest :: Decode DeleteObjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteObjectRequest :: Encode DeleteObjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteObjectResponse = DeleteObjectResponse Types.NoArguments
derive instance newtypeDeleteObjectResponse :: Newtype DeleteObjectResponse _
derive instance repGenericDeleteObjectResponse :: Generic DeleteObjectResponse _
instance showDeleteObjectResponse :: Show DeleteObjectResponse where
  show = genericShow
instance decodeDeleteObjectResponse :: Decode DeleteObjectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteObjectResponse :: Encode DeleteObjectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSchemaRequest = DeleteSchemaRequest 
  { "SchemaArn" :: (Arn)
  }
derive instance newtypeDeleteSchemaRequest :: Newtype DeleteSchemaRequest _
derive instance repGenericDeleteSchemaRequest :: Generic DeleteSchemaRequest _
instance showDeleteSchemaRequest :: Show DeleteSchemaRequest where
  show = genericShow
instance decodeDeleteSchemaRequest :: Decode DeleteSchemaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSchemaRequest :: Encode DeleteSchemaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSchemaResponse = DeleteSchemaResponse 
  { "SchemaArn" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeDeleteSchemaResponse :: Newtype DeleteSchemaResponse _
derive instance repGenericDeleteSchemaResponse :: Generic DeleteSchemaResponse _
instance showDeleteSchemaResponse :: Show DeleteSchemaResponse where
  show = genericShow
instance decodeDeleteSchemaResponse :: Decode DeleteSchemaResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSchemaResponse :: Encode DeleteSchemaResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTypedLinkFacetRequest = DeleteTypedLinkFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (TypedLinkName)
  }
derive instance newtypeDeleteTypedLinkFacetRequest :: Newtype DeleteTypedLinkFacetRequest _
derive instance repGenericDeleteTypedLinkFacetRequest :: Generic DeleteTypedLinkFacetRequest _
instance showDeleteTypedLinkFacetRequest :: Show DeleteTypedLinkFacetRequest where
  show = genericShow
instance decodeDeleteTypedLinkFacetRequest :: Decode DeleteTypedLinkFacetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTypedLinkFacetRequest :: Encode DeleteTypedLinkFacetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTypedLinkFacetResponse = DeleteTypedLinkFacetResponse Types.NoArguments
derive instance newtypeDeleteTypedLinkFacetResponse :: Newtype DeleteTypedLinkFacetResponse _
derive instance repGenericDeleteTypedLinkFacetResponse :: Generic DeleteTypedLinkFacetResponse _
instance showDeleteTypedLinkFacetResponse :: Show DeleteTypedLinkFacetResponse where
  show = genericShow
instance decodeDeleteTypedLinkFacetResponse :: Decode DeleteTypedLinkFacetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTypedLinkFacetResponse :: Encode DeleteTypedLinkFacetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachFromIndexRequest = DetachFromIndexRequest 
  { "DirectoryArn" :: (Arn)
  , "IndexReference" :: (ObjectReference)
  , "TargetReference" :: (ObjectReference)
  }
derive instance newtypeDetachFromIndexRequest :: Newtype DetachFromIndexRequest _
derive instance repGenericDetachFromIndexRequest :: Generic DetachFromIndexRequest _
instance showDetachFromIndexRequest :: Show DetachFromIndexRequest where
  show = genericShow
instance decodeDetachFromIndexRequest :: Decode DetachFromIndexRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachFromIndexRequest :: Encode DetachFromIndexRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachFromIndexResponse = DetachFromIndexResponse 
  { "DetachedObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeDetachFromIndexResponse :: Newtype DetachFromIndexResponse _
derive instance repGenericDetachFromIndexResponse :: Generic DetachFromIndexResponse _
instance showDetachFromIndexResponse :: Show DetachFromIndexResponse where
  show = genericShow
instance decodeDetachFromIndexResponse :: Decode DetachFromIndexResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachFromIndexResponse :: Encode DetachFromIndexResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachObjectRequest = DetachObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "ParentReference" :: (ObjectReference)
  , "LinkName" :: (LinkName)
  }
derive instance newtypeDetachObjectRequest :: Newtype DetachObjectRequest _
derive instance repGenericDetachObjectRequest :: Generic DetachObjectRequest _
instance showDetachObjectRequest :: Show DetachObjectRequest where
  show = genericShow
instance decodeDetachObjectRequest :: Decode DetachObjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachObjectRequest :: Encode DetachObjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachObjectResponse = DetachObjectResponse 
  { "DetachedObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeDetachObjectResponse :: Newtype DetachObjectResponse _
derive instance repGenericDetachObjectResponse :: Generic DetachObjectResponse _
instance showDetachObjectResponse :: Show DetachObjectResponse where
  show = genericShow
instance decodeDetachObjectResponse :: Decode DetachObjectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachObjectResponse :: Encode DetachObjectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachPolicyRequest = DetachPolicyRequest 
  { "DirectoryArn" :: (Arn)
  , "PolicyReference" :: (ObjectReference)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeDetachPolicyRequest :: Newtype DetachPolicyRequest _
derive instance repGenericDetachPolicyRequest :: Generic DetachPolicyRequest _
instance showDetachPolicyRequest :: Show DetachPolicyRequest where
  show = genericShow
instance decodeDetachPolicyRequest :: Decode DetachPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachPolicyRequest :: Encode DetachPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachPolicyResponse = DetachPolicyResponse Types.NoArguments
derive instance newtypeDetachPolicyResponse :: Newtype DetachPolicyResponse _
derive instance repGenericDetachPolicyResponse :: Generic DetachPolicyResponse _
instance showDetachPolicyResponse :: Show DetachPolicyResponse where
  show = genericShow
instance decodeDetachPolicyResponse :: Decode DetachPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachPolicyResponse :: Encode DetachPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachTypedLinkRequest = DetachTypedLinkRequest 
  { "DirectoryArn" :: (Arn)
  , "TypedLinkSpecifier" :: (TypedLinkSpecifier)
  }
derive instance newtypeDetachTypedLinkRequest :: Newtype DetachTypedLinkRequest _
derive instance repGenericDetachTypedLinkRequest :: Generic DetachTypedLinkRequest _
instance showDetachTypedLinkRequest :: Show DetachTypedLinkRequest where
  show = genericShow
instance decodeDetachTypedLinkRequest :: Decode DetachTypedLinkRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachTypedLinkRequest :: Encode DetachTypedLinkRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Directory structure that includes the directory name and directory ARN.</p>
newtype Directory = Directory 
  { "Name" :: NullOrUndefined.NullOrUndefined (DirectoryName)
  , "DirectoryArn" :: NullOrUndefined.NullOrUndefined (DirectoryArn)
  , "State" :: NullOrUndefined.NullOrUndefined (DirectoryState)
  , "CreationDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeDirectory :: Newtype Directory _
derive instance repGenericDirectory :: Generic Directory _
instance showDirectory :: Show Directory where
  show = genericShow
instance decodeDirectory :: Decode Directory where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectory :: Encode Directory where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that a <a>Directory</a> could not be created due to a naming conflict. Choose a different name and try again.</p>
newtype DirectoryAlreadyExistsException = DirectoryAlreadyExistsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDirectoryAlreadyExistsException :: Newtype DirectoryAlreadyExistsException _
derive instance repGenericDirectoryAlreadyExistsException :: Generic DirectoryAlreadyExistsException _
instance showDirectoryAlreadyExistsException :: Show DirectoryAlreadyExistsException where
  show = genericShow
instance decodeDirectoryAlreadyExistsException :: Decode DirectoryAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryAlreadyExistsException :: Encode DirectoryAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectoryArn = DirectoryArn String
derive instance newtypeDirectoryArn :: Newtype DirectoryArn _
derive instance repGenericDirectoryArn :: Generic DirectoryArn _
instance showDirectoryArn :: Show DirectoryArn where
  show = genericShow
instance decodeDirectoryArn :: Decode DirectoryArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryArn :: Encode DirectoryArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A directory that has been deleted and to which access has been attempted. Note: The requested resource will eventually cease to exist.</p>
newtype DirectoryDeletedException = DirectoryDeletedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDirectoryDeletedException :: Newtype DirectoryDeletedException _
derive instance repGenericDirectoryDeletedException :: Generic DirectoryDeletedException _
instance showDirectoryDeletedException :: Show DirectoryDeletedException where
  show = genericShow
instance decodeDirectoryDeletedException :: Decode DirectoryDeletedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryDeletedException :: Encode DirectoryDeletedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectoryList = DirectoryList (Array Directory)
derive instance newtypeDirectoryList :: Newtype DirectoryList _
derive instance repGenericDirectoryList :: Generic DirectoryList _
instance showDirectoryList :: Show DirectoryList where
  show = genericShow
instance decodeDirectoryList :: Decode DirectoryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryList :: Encode DirectoryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectoryName = DirectoryName String
derive instance newtypeDirectoryName :: Newtype DirectoryName _
derive instance repGenericDirectoryName :: Generic DirectoryName _
instance showDirectoryName :: Show DirectoryName where
  show = genericShow
instance decodeDirectoryName :: Decode DirectoryName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryName :: Encode DirectoryName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An operation can only operate on a disabled directory.</p>
newtype DirectoryNotDisabledException = DirectoryNotDisabledException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDirectoryNotDisabledException :: Newtype DirectoryNotDisabledException _
derive instance repGenericDirectoryNotDisabledException :: Generic DirectoryNotDisabledException _
instance showDirectoryNotDisabledException :: Show DirectoryNotDisabledException where
  show = genericShow
instance decodeDirectoryNotDisabledException :: Decode DirectoryNotDisabledException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryNotDisabledException :: Encode DirectoryNotDisabledException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An operation can only operate on a directory that is not enabled.</p>
newtype DirectoryNotEnabledException = DirectoryNotEnabledException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDirectoryNotEnabledException :: Newtype DirectoryNotEnabledException _
derive instance repGenericDirectoryNotEnabledException :: Generic DirectoryNotEnabledException _
instance showDirectoryNotEnabledException :: Show DirectoryNotEnabledException where
  show = genericShow
instance decodeDirectoryNotEnabledException :: Decode DirectoryNotEnabledException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryNotEnabledException :: Encode DirectoryNotEnabledException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectoryState = DirectoryState String
derive instance newtypeDirectoryState :: Newtype DirectoryState _
derive instance repGenericDirectoryState :: Generic DirectoryState _
instance showDirectoryState :: Show DirectoryState where
  show = genericShow
instance decodeDirectoryState :: Decode DirectoryState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryState :: Encode DirectoryState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisableDirectoryRequest = DisableDirectoryRequest 
  { "DirectoryArn" :: (Arn)
  }
derive instance newtypeDisableDirectoryRequest :: Newtype DisableDirectoryRequest _
derive instance repGenericDisableDirectoryRequest :: Generic DisableDirectoryRequest _
instance showDisableDirectoryRequest :: Show DisableDirectoryRequest where
  show = genericShow
instance decodeDisableDirectoryRequest :: Decode DisableDirectoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableDirectoryRequest :: Encode DisableDirectoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisableDirectoryResponse = DisableDirectoryResponse 
  { "DirectoryArn" :: (Arn)
  }
derive instance newtypeDisableDirectoryResponse :: Newtype DisableDirectoryResponse _
derive instance repGenericDisableDirectoryResponse :: Generic DisableDirectoryResponse _
instance showDisableDirectoryResponse :: Show DisableDirectoryResponse where
  show = genericShow
instance decodeDisableDirectoryResponse :: Decode DisableDirectoryResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableDirectoryResponse :: Encode DisableDirectoryResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnableDirectoryRequest = EnableDirectoryRequest 
  { "DirectoryArn" :: (Arn)
  }
derive instance newtypeEnableDirectoryRequest :: Newtype EnableDirectoryRequest _
derive instance repGenericEnableDirectoryRequest :: Generic EnableDirectoryRequest _
instance showEnableDirectoryRequest :: Show EnableDirectoryRequest where
  show = genericShow
instance decodeEnableDirectoryRequest :: Decode EnableDirectoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableDirectoryRequest :: Encode EnableDirectoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnableDirectoryResponse = EnableDirectoryResponse 
  { "DirectoryArn" :: (Arn)
  }
derive instance newtypeEnableDirectoryResponse :: Newtype EnableDirectoryResponse _
derive instance repGenericEnableDirectoryResponse :: Generic EnableDirectoryResponse _
instance showEnableDirectoryResponse :: Show EnableDirectoryResponse where
  show = genericShow
instance decodeEnableDirectoryResponse :: Decode EnableDirectoryResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableDirectoryResponse :: Encode EnableDirectoryResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _
derive instance repGenericExceptionMessage :: Generic ExceptionMessage _
instance showExceptionMessage :: Show ExceptionMessage where
  show = genericShow
instance decodeExceptionMessage :: Decode ExceptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExceptionMessage :: Encode ExceptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure that contains <code>Name</code>, <code>ARN</code>, <code>Attributes</code>, <a>Rule</a>s, and <code>ObjectTypes</code>.</p>
newtype Facet = Facet 
  { "Name" :: NullOrUndefined.NullOrUndefined (FacetName)
  , "ObjectType" :: NullOrUndefined.NullOrUndefined (ObjectType)
  }
derive instance newtypeFacet :: Newtype Facet _
derive instance repGenericFacet :: Generic Facet _
instance showFacet :: Show Facet where
  show = genericShow
instance decodeFacet :: Decode Facet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacet :: Encode Facet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A facet with the same name already exists.</p>
newtype FacetAlreadyExistsException = FacetAlreadyExistsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeFacetAlreadyExistsException :: Newtype FacetAlreadyExistsException _
derive instance repGenericFacetAlreadyExistsException :: Generic FacetAlreadyExistsException _
instance showFacetAlreadyExistsException :: Show FacetAlreadyExistsException where
  show = genericShow
instance decodeFacetAlreadyExistsException :: Decode FacetAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetAlreadyExistsException :: Encode FacetAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An attribute that is associated with the <a>Facet</a>.</p>
newtype FacetAttribute = FacetAttribute 
  { "Name" :: (AttributeName)
  , "AttributeDefinition" :: NullOrUndefined.NullOrUndefined (FacetAttributeDefinition)
  , "AttributeReference" :: NullOrUndefined.NullOrUndefined (FacetAttributeReference)
  , "RequiredBehavior" :: NullOrUndefined.NullOrUndefined (RequiredAttributeBehavior)
  }
derive instance newtypeFacetAttribute :: Newtype FacetAttribute _
derive instance repGenericFacetAttribute :: Generic FacetAttribute _
instance showFacetAttribute :: Show FacetAttribute where
  show = genericShow
instance decodeFacetAttribute :: Decode FacetAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetAttribute :: Encode FacetAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A facet attribute definition. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences">Attribute References</a> for more information.</p>
newtype FacetAttributeDefinition = FacetAttributeDefinition 
  { "Type" :: (FacetAttributeType)
  , "DefaultValue" :: NullOrUndefined.NullOrUndefined (TypedAttributeValue)
  , "IsImmutable" :: NullOrUndefined.NullOrUndefined (Bool)
  , "Rules" :: NullOrUndefined.NullOrUndefined (RuleMap)
  }
derive instance newtypeFacetAttributeDefinition :: Newtype FacetAttributeDefinition _
derive instance repGenericFacetAttributeDefinition :: Generic FacetAttributeDefinition _
instance showFacetAttributeDefinition :: Show FacetAttributeDefinition where
  show = genericShow
instance decodeFacetAttributeDefinition :: Decode FacetAttributeDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetAttributeDefinition :: Encode FacetAttributeDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FacetAttributeList = FacetAttributeList (Array FacetAttribute)
derive instance newtypeFacetAttributeList :: Newtype FacetAttributeList _
derive instance repGenericFacetAttributeList :: Generic FacetAttributeList _
instance showFacetAttributeList :: Show FacetAttributeList where
  show = genericShow
instance decodeFacetAttributeList :: Decode FacetAttributeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetAttributeList :: Encode FacetAttributeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The facet attribute reference that specifies the attribute definition that contains the attribute facet name and attribute name.</p>
newtype FacetAttributeReference = FacetAttributeReference 
  { "TargetFacetName" :: (FacetName)
  , "TargetAttributeName" :: (AttributeName)
  }
derive instance newtypeFacetAttributeReference :: Newtype FacetAttributeReference _
derive instance repGenericFacetAttributeReference :: Generic FacetAttributeReference _
instance showFacetAttributeReference :: Show FacetAttributeReference where
  show = genericShow
instance decodeFacetAttributeReference :: Decode FacetAttributeReference where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetAttributeReference :: Encode FacetAttributeReference where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FacetAttributeType = FacetAttributeType String
derive instance newtypeFacetAttributeType :: Newtype FacetAttributeType _
derive instance repGenericFacetAttributeType :: Generic FacetAttributeType _
instance showFacetAttributeType :: Show FacetAttributeType where
  show = genericShow
instance decodeFacetAttributeType :: Decode FacetAttributeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetAttributeType :: Encode FacetAttributeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure that contains information used to update an attribute.</p>
newtype FacetAttributeUpdate = FacetAttributeUpdate 
  { "Attribute" :: NullOrUndefined.NullOrUndefined (FacetAttribute)
  , "Action" :: NullOrUndefined.NullOrUndefined (UpdateActionType)
  }
derive instance newtypeFacetAttributeUpdate :: Newtype FacetAttributeUpdate _
derive instance repGenericFacetAttributeUpdate :: Generic FacetAttributeUpdate _
instance showFacetAttributeUpdate :: Show FacetAttributeUpdate where
  show = genericShow
instance decodeFacetAttributeUpdate :: Decode FacetAttributeUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetAttributeUpdate :: Encode FacetAttributeUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FacetAttributeUpdateList = FacetAttributeUpdateList (Array FacetAttributeUpdate)
derive instance newtypeFacetAttributeUpdateList :: Newtype FacetAttributeUpdateList _
derive instance repGenericFacetAttributeUpdateList :: Generic FacetAttributeUpdateList _
instance showFacetAttributeUpdateList :: Show FacetAttributeUpdateList where
  show = genericShow
instance decodeFacetAttributeUpdateList :: Decode FacetAttributeUpdateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetAttributeUpdateList :: Encode FacetAttributeUpdateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Occurs when deleting a facet that contains an attribute that is a target to an attribute reference in a different facet.</p>
newtype FacetInUseException = FacetInUseException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeFacetInUseException :: Newtype FacetInUseException _
derive instance repGenericFacetInUseException :: Generic FacetInUseException _
instance showFacetInUseException :: Show FacetInUseException where
  show = genericShow
instance decodeFacetInUseException :: Decode FacetInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetInUseException :: Encode FacetInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FacetName = FacetName String
derive instance newtypeFacetName :: Newtype FacetName _
derive instance repGenericFacetName :: Generic FacetName _
instance showFacetName :: Show FacetName where
  show = genericShow
instance decodeFacetName :: Decode FacetName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetName :: Encode FacetName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FacetNameList = FacetNameList (Array FacetName)
derive instance newtypeFacetNameList :: Newtype FacetNameList _
derive instance repGenericFacetNameList :: Generic FacetNameList _
instance showFacetNameList :: Show FacetNameList where
  show = genericShow
instance decodeFacetNameList :: Decode FacetNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetNameList :: Encode FacetNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified <a>Facet</a> could not be found.</p>
newtype FacetNotFoundException = FacetNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeFacetNotFoundException :: Newtype FacetNotFoundException _
derive instance repGenericFacetNotFoundException :: Generic FacetNotFoundException _
instance showFacetNotFoundException :: Show FacetNotFoundException where
  show = genericShow
instance decodeFacetNotFoundException :: Decode FacetNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetNotFoundException :: Encode FacetNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <a>Facet</a> that you provided was not well formed or could not be validated with the schema.</p>
newtype FacetValidationException = FacetValidationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeFacetValidationException :: Newtype FacetValidationException _
derive instance repGenericFacetValidationException :: Generic FacetValidationException _
instance showFacetValidationException :: Show FacetValidationException where
  show = genericShow
instance decodeFacetValidationException :: Decode FacetValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFacetValidationException :: Encode FacetValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAppliedSchemaVersionRequest = GetAppliedSchemaVersionRequest 
  { "SchemaArn" :: (Arn)
  }
derive instance newtypeGetAppliedSchemaVersionRequest :: Newtype GetAppliedSchemaVersionRequest _
derive instance repGenericGetAppliedSchemaVersionRequest :: Generic GetAppliedSchemaVersionRequest _
instance showGetAppliedSchemaVersionRequest :: Show GetAppliedSchemaVersionRequest where
  show = genericShow
instance decodeGetAppliedSchemaVersionRequest :: Decode GetAppliedSchemaVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAppliedSchemaVersionRequest :: Encode GetAppliedSchemaVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAppliedSchemaVersionResponse = GetAppliedSchemaVersionResponse 
  { "AppliedSchemaArn" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeGetAppliedSchemaVersionResponse :: Newtype GetAppliedSchemaVersionResponse _
derive instance repGenericGetAppliedSchemaVersionResponse :: Generic GetAppliedSchemaVersionResponse _
instance showGetAppliedSchemaVersionResponse :: Show GetAppliedSchemaVersionResponse where
  show = genericShow
instance decodeGetAppliedSchemaVersionResponse :: Decode GetAppliedSchemaVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAppliedSchemaVersionResponse :: Encode GetAppliedSchemaVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDirectoryRequest = GetDirectoryRequest 
  { "DirectoryArn" :: (DirectoryArn)
  }
derive instance newtypeGetDirectoryRequest :: Newtype GetDirectoryRequest _
derive instance repGenericGetDirectoryRequest :: Generic GetDirectoryRequest _
instance showGetDirectoryRequest :: Show GetDirectoryRequest where
  show = genericShow
instance decodeGetDirectoryRequest :: Decode GetDirectoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDirectoryRequest :: Encode GetDirectoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDirectoryResponse = GetDirectoryResponse 
  { "Directory" :: (Directory)
  }
derive instance newtypeGetDirectoryResponse :: Newtype GetDirectoryResponse _
derive instance repGenericGetDirectoryResponse :: Generic GetDirectoryResponse _
instance showGetDirectoryResponse :: Show GetDirectoryResponse where
  show = genericShow
instance decodeGetDirectoryResponse :: Decode GetDirectoryResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDirectoryResponse :: Encode GetDirectoryResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFacetRequest = GetFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  }
derive instance newtypeGetFacetRequest :: Newtype GetFacetRequest _
derive instance repGenericGetFacetRequest :: Generic GetFacetRequest _
instance showGetFacetRequest :: Show GetFacetRequest where
  show = genericShow
instance decodeGetFacetRequest :: Decode GetFacetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFacetRequest :: Encode GetFacetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFacetResponse = GetFacetResponse 
  { "Facet" :: NullOrUndefined.NullOrUndefined (Facet)
  }
derive instance newtypeGetFacetResponse :: Newtype GetFacetResponse _
derive instance repGenericGetFacetResponse :: Generic GetFacetResponse _
instance showGetFacetResponse :: Show GetFacetResponse where
  show = genericShow
instance decodeGetFacetResponse :: Decode GetFacetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFacetResponse :: Encode GetFacetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetObjectInformationRequest = GetObjectInformationRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "ConsistencyLevel" :: NullOrUndefined.NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeGetObjectInformationRequest :: Newtype GetObjectInformationRequest _
derive instance repGenericGetObjectInformationRequest :: Generic GetObjectInformationRequest _
instance showGetObjectInformationRequest :: Show GetObjectInformationRequest where
  show = genericShow
instance decodeGetObjectInformationRequest :: Decode GetObjectInformationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetObjectInformationRequest :: Encode GetObjectInformationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetObjectInformationResponse = GetObjectInformationResponse 
  { "SchemaFacets" :: NullOrUndefined.NullOrUndefined (SchemaFacetList)
  , "ObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeGetObjectInformationResponse :: Newtype GetObjectInformationResponse _
derive instance repGenericGetObjectInformationResponse :: Generic GetObjectInformationResponse _
instance showGetObjectInformationResponse :: Show GetObjectInformationResponse where
  show = genericShow
instance decodeGetObjectInformationResponse :: Decode GetObjectInformationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetObjectInformationResponse :: Encode GetObjectInformationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSchemaAsJsonRequest = GetSchemaAsJsonRequest 
  { "SchemaArn" :: (Arn)
  }
derive instance newtypeGetSchemaAsJsonRequest :: Newtype GetSchemaAsJsonRequest _
derive instance repGenericGetSchemaAsJsonRequest :: Generic GetSchemaAsJsonRequest _
instance showGetSchemaAsJsonRequest :: Show GetSchemaAsJsonRequest where
  show = genericShow
instance decodeGetSchemaAsJsonRequest :: Decode GetSchemaAsJsonRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSchemaAsJsonRequest :: Encode GetSchemaAsJsonRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSchemaAsJsonResponse = GetSchemaAsJsonResponse 
  { "Name" :: NullOrUndefined.NullOrUndefined (SchemaName)
  , "Document" :: NullOrUndefined.NullOrUndefined (SchemaJsonDocument)
  }
derive instance newtypeGetSchemaAsJsonResponse :: Newtype GetSchemaAsJsonResponse _
derive instance repGenericGetSchemaAsJsonResponse :: Generic GetSchemaAsJsonResponse _
instance showGetSchemaAsJsonResponse :: Show GetSchemaAsJsonResponse where
  show = genericShow
instance decodeGetSchemaAsJsonResponse :: Decode GetSchemaAsJsonResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSchemaAsJsonResponse :: Encode GetSchemaAsJsonResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTypedLinkFacetInformationRequest = GetTypedLinkFacetInformationRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (TypedLinkName)
  }
derive instance newtypeGetTypedLinkFacetInformationRequest :: Newtype GetTypedLinkFacetInformationRequest _
derive instance repGenericGetTypedLinkFacetInformationRequest :: Generic GetTypedLinkFacetInformationRequest _
instance showGetTypedLinkFacetInformationRequest :: Show GetTypedLinkFacetInformationRequest where
  show = genericShow
instance decodeGetTypedLinkFacetInformationRequest :: Decode GetTypedLinkFacetInformationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTypedLinkFacetInformationRequest :: Encode GetTypedLinkFacetInformationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTypedLinkFacetInformationResponse = GetTypedLinkFacetInformationResponse 
  { "IdentityAttributeOrder" :: NullOrUndefined.NullOrUndefined (AttributeNameList)
  }
derive instance newtypeGetTypedLinkFacetInformationResponse :: Newtype GetTypedLinkFacetInformationResponse _
derive instance repGenericGetTypedLinkFacetInformationResponse :: Generic GetTypedLinkFacetInformationResponse _
instance showGetTypedLinkFacetInformationResponse :: Show GetTypedLinkFacetInformationResponse where
  show = genericShow
instance decodeGetTypedLinkFacetInformationResponse :: Decode GetTypedLinkFacetInformationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTypedLinkFacetInformationResponse :: Encode GetTypedLinkFacetInformationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates a failure occurred while performing a check for backward compatibility between the specified schema and the schema that is currently applied to the directory.</p>
newtype IncompatibleSchemaException = IncompatibleSchemaException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeIncompatibleSchemaException :: Newtype IncompatibleSchemaException _
derive instance repGenericIncompatibleSchemaException :: Generic IncompatibleSchemaException _
instance showIncompatibleSchemaException :: Show IncompatibleSchemaException where
  show = genericShow
instance decodeIncompatibleSchemaException :: Decode IncompatibleSchemaException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncompatibleSchemaException :: Encode IncompatibleSchemaException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an index and an attached object.</p>
newtype IndexAttachment = IndexAttachment 
  { "IndexedAttributes" :: NullOrUndefined.NullOrUndefined (AttributeKeyAndValueList)
  , "ObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeIndexAttachment :: Newtype IndexAttachment _
derive instance repGenericIndexAttachment :: Generic IndexAttachment _
instance showIndexAttachment :: Show IndexAttachment where
  show = genericShow
instance decodeIndexAttachment :: Decode IndexAttachment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexAttachment :: Encode IndexAttachment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IndexAttachmentList = IndexAttachmentList (Array IndexAttachment)
derive instance newtypeIndexAttachmentList :: Newtype IndexAttachmentList _
derive instance repGenericIndexAttachmentList :: Generic IndexAttachmentList _
instance showIndexAttachmentList :: Show IndexAttachmentList where
  show = genericShow
instance decodeIndexAttachmentList :: Decode IndexAttachmentList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexAttachmentList :: Encode IndexAttachmentList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object has been attempted to be attached to an object that does not have the appropriate attribute value.</p>
newtype IndexedAttributeMissingException = IndexedAttributeMissingException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeIndexedAttributeMissingException :: Newtype IndexedAttributeMissingException _
derive instance repGenericIndexedAttributeMissingException :: Generic IndexedAttributeMissingException _
instance showIndexedAttributeMissingException :: Show IndexedAttributeMissingException where
  show = genericShow
instance decodeIndexedAttributeMissingException :: Decode IndexedAttributeMissingException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexedAttributeMissingException :: Encode IndexedAttributeMissingException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates a problem that must be resolved by Amazon Web Services. This might be a transient error in which case you can retry your request until it succeeds. Otherwise, go to the <a href="http://status.aws.amazon.com/">AWS Service Health Dashboard</a> site to see if there are any operational issues with the service.</p>
newtype InternalServiceException = InternalServiceException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInternalServiceException :: Newtype InternalServiceException _
derive instance repGenericInternalServiceException :: Generic InternalServiceException _
instance showInternalServiceException :: Show InternalServiceException where
  show = genericShow
instance decodeInternalServiceException :: Decode InternalServiceException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServiceException :: Encode InternalServiceException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the provided ARN value is not valid.</p>
newtype InvalidArnException = InvalidArnException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidArnException :: Newtype InvalidArnException _
derive instance repGenericInvalidArnException :: Generic InvalidArnException _
instance showInvalidArnException :: Show InvalidArnException where
  show = genericShow
instance decodeInvalidArnException :: Decode InvalidArnException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidArnException :: Encode InvalidArnException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that an attempt to attach an object with the same link name or to apply a schema with the same name has occurred. Rename the link or the schema and then try again.</p>
newtype InvalidAttachmentException = InvalidAttachmentException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidAttachmentException :: Newtype InvalidAttachmentException _
derive instance repGenericInvalidAttachmentException :: Generic InvalidAttachmentException _
instance showInvalidAttachmentException :: Show InvalidAttachmentException where
  show = genericShow
instance decodeInvalidAttachmentException :: Decode InvalidAttachmentException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidAttachmentException :: Encode InvalidAttachmentException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An attempt to modify a <a>Facet</a> resulted in an invalid schema exception.</p>
newtype InvalidFacetUpdateException = InvalidFacetUpdateException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidFacetUpdateException :: Newtype InvalidFacetUpdateException _
derive instance repGenericInvalidFacetUpdateException :: Generic InvalidFacetUpdateException _
instance showInvalidFacetUpdateException :: Show InvalidFacetUpdateException where
  show = genericShow
instance decodeInvalidFacetUpdateException :: Decode InvalidFacetUpdateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidFacetUpdateException :: Encode InvalidFacetUpdateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the <code>NextToken</code> value is not valid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _
derive instance repGenericInvalidNextTokenException :: Generic InvalidNextTokenException _
instance showInvalidNextTokenException :: Show InvalidNextTokenException where
  show = genericShow
instance decodeInvalidNextTokenException :: Decode InvalidNextTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNextTokenException :: Encode InvalidNextTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Occurs when any of the rule parameter keys or values are invalid.</p>
newtype InvalidRuleException = InvalidRuleException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidRuleException :: Newtype InvalidRuleException _
derive instance repGenericInvalidRuleException :: Generic InvalidRuleException _
instance showInvalidRuleException :: Show InvalidRuleException where
  show = genericShow
instance decodeInvalidRuleException :: Decode InvalidRuleException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRuleException :: Encode InvalidRuleException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the provided <code>SchemaDoc</code> value is not valid.</p>
newtype InvalidSchemaDocException = InvalidSchemaDocException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidSchemaDocException :: Newtype InvalidSchemaDocException _
derive instance repGenericInvalidSchemaDocException :: Generic InvalidSchemaDocException _
instance showInvalidSchemaDocException :: Show InvalidSchemaDocException where
  show = genericShow
instance decodeInvalidSchemaDocException :: Decode InvalidSchemaDocException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSchemaDocException :: Encode InvalidSchemaDocException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Can occur for multiple reasons such as when you tag a resource that doesn’t exist or if you specify a higher number of tags for a resource than the allowed limit. Allowed limit is 50 tags per resource.</p>
newtype InvalidTaggingRequestException = InvalidTaggingRequestException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidTaggingRequestException :: Newtype InvalidTaggingRequestException _
derive instance repGenericInvalidTaggingRequestException :: Generic InvalidTaggingRequestException _
instance showInvalidTaggingRequestException :: Show InvalidTaggingRequestException where
  show = genericShow
instance decodeInvalidTaggingRequestException :: Decode InvalidTaggingRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTaggingRequestException :: Encode InvalidTaggingRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that limits are exceeded. See <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/limits.html">Limits</a> for more information.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LinkName = LinkName String
derive instance newtypeLinkName :: Newtype LinkName _
derive instance repGenericLinkName :: Generic LinkName _
instance showLinkName :: Show LinkName where
  show = genericShow
instance decodeLinkName :: Decode LinkName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLinkName :: Encode LinkName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that a link could not be created due to a naming conflict. Choose a different name and then try again.</p>
newtype LinkNameAlreadyInUseException = LinkNameAlreadyInUseException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeLinkNameAlreadyInUseException :: Newtype LinkNameAlreadyInUseException _
derive instance repGenericLinkNameAlreadyInUseException :: Generic LinkNameAlreadyInUseException _
instance showLinkNameAlreadyInUseException :: Show LinkNameAlreadyInUseException where
  show = genericShow
instance decodeLinkNameAlreadyInUseException :: Decode LinkNameAlreadyInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLinkNameAlreadyInUseException :: Encode LinkNameAlreadyInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LinkNameToObjectIdentifierMap = LinkNameToObjectIdentifierMap (StrMap.StrMap ObjectIdentifier)
derive instance newtypeLinkNameToObjectIdentifierMap :: Newtype LinkNameToObjectIdentifierMap _
derive instance repGenericLinkNameToObjectIdentifierMap :: Generic LinkNameToObjectIdentifierMap _
instance showLinkNameToObjectIdentifierMap :: Show LinkNameToObjectIdentifierMap where
  show = genericShow
instance decodeLinkNameToObjectIdentifierMap :: Decode LinkNameToObjectIdentifierMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLinkNameToObjectIdentifierMap :: Encode LinkNameToObjectIdentifierMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAppliedSchemaArnsRequest = ListAppliedSchemaArnsRequest 
  { "DirectoryArn" :: (Arn)
  , "SchemaArn" :: NullOrUndefined.NullOrUndefined (Arn)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeListAppliedSchemaArnsRequest :: Newtype ListAppliedSchemaArnsRequest _
derive instance repGenericListAppliedSchemaArnsRequest :: Generic ListAppliedSchemaArnsRequest _
instance showListAppliedSchemaArnsRequest :: Show ListAppliedSchemaArnsRequest where
  show = genericShow
instance decodeListAppliedSchemaArnsRequest :: Decode ListAppliedSchemaArnsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAppliedSchemaArnsRequest :: Encode ListAppliedSchemaArnsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAppliedSchemaArnsResponse = ListAppliedSchemaArnsResponse 
  { "SchemaArns" :: NullOrUndefined.NullOrUndefined (Arns)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListAppliedSchemaArnsResponse :: Newtype ListAppliedSchemaArnsResponse _
derive instance repGenericListAppliedSchemaArnsResponse :: Generic ListAppliedSchemaArnsResponse _
instance showListAppliedSchemaArnsResponse :: Show ListAppliedSchemaArnsResponse where
  show = genericShow
instance decodeListAppliedSchemaArnsResponse :: Decode ListAppliedSchemaArnsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAppliedSchemaArnsResponse :: Encode ListAppliedSchemaArnsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAttachedIndicesRequest = ListAttachedIndicesRequest 
  { "DirectoryArn" :: (Arn)
  , "TargetReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined.NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListAttachedIndicesRequest :: Newtype ListAttachedIndicesRequest _
derive instance repGenericListAttachedIndicesRequest :: Generic ListAttachedIndicesRequest _
instance showListAttachedIndicesRequest :: Show ListAttachedIndicesRequest where
  show = genericShow
instance decodeListAttachedIndicesRequest :: Decode ListAttachedIndicesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAttachedIndicesRequest :: Encode ListAttachedIndicesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAttachedIndicesResponse = ListAttachedIndicesResponse 
  { "IndexAttachments" :: NullOrUndefined.NullOrUndefined (IndexAttachmentList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListAttachedIndicesResponse :: Newtype ListAttachedIndicesResponse _
derive instance repGenericListAttachedIndicesResponse :: Generic ListAttachedIndicesResponse _
instance showListAttachedIndicesResponse :: Show ListAttachedIndicesResponse where
  show = genericShow
instance decodeListAttachedIndicesResponse :: Decode ListAttachedIndicesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAttachedIndicesResponse :: Encode ListAttachedIndicesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDevelopmentSchemaArnsRequest = ListDevelopmentSchemaArnsRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeListDevelopmentSchemaArnsRequest :: Newtype ListDevelopmentSchemaArnsRequest _
derive instance repGenericListDevelopmentSchemaArnsRequest :: Generic ListDevelopmentSchemaArnsRequest _
instance showListDevelopmentSchemaArnsRequest :: Show ListDevelopmentSchemaArnsRequest where
  show = genericShow
instance decodeListDevelopmentSchemaArnsRequest :: Decode ListDevelopmentSchemaArnsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDevelopmentSchemaArnsRequest :: Encode ListDevelopmentSchemaArnsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDevelopmentSchemaArnsResponse = ListDevelopmentSchemaArnsResponse 
  { "SchemaArns" :: NullOrUndefined.NullOrUndefined (Arns)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListDevelopmentSchemaArnsResponse :: Newtype ListDevelopmentSchemaArnsResponse _
derive instance repGenericListDevelopmentSchemaArnsResponse :: Generic ListDevelopmentSchemaArnsResponse _
instance showListDevelopmentSchemaArnsResponse :: Show ListDevelopmentSchemaArnsResponse where
  show = genericShow
instance decodeListDevelopmentSchemaArnsResponse :: Decode ListDevelopmentSchemaArnsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDevelopmentSchemaArnsResponse :: Encode ListDevelopmentSchemaArnsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDirectoriesRequest = ListDirectoriesRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  , "State'" :: NullOrUndefined.NullOrUndefined (DirectoryState)
  }
derive instance newtypeListDirectoriesRequest :: Newtype ListDirectoriesRequest _
derive instance repGenericListDirectoriesRequest :: Generic ListDirectoriesRequest _
instance showListDirectoriesRequest :: Show ListDirectoriesRequest where
  show = genericShow
instance decodeListDirectoriesRequest :: Decode ListDirectoriesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDirectoriesRequest :: Encode ListDirectoriesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDirectoriesResponse = ListDirectoriesResponse 
  { "Directories" :: (DirectoryList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListDirectoriesResponse :: Newtype ListDirectoriesResponse _
derive instance repGenericListDirectoriesResponse :: Generic ListDirectoriesResponse _
instance showListDirectoriesResponse :: Show ListDirectoriesResponse where
  show = genericShow
instance decodeListDirectoriesResponse :: Decode ListDirectoriesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDirectoriesResponse :: Encode ListDirectoriesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListFacetAttributesRequest = ListFacetAttributesRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeListFacetAttributesRequest :: Newtype ListFacetAttributesRequest _
derive instance repGenericListFacetAttributesRequest :: Generic ListFacetAttributesRequest _
instance showListFacetAttributesRequest :: Show ListFacetAttributesRequest where
  show = genericShow
instance decodeListFacetAttributesRequest :: Decode ListFacetAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFacetAttributesRequest :: Encode ListFacetAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListFacetAttributesResponse = ListFacetAttributesResponse 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (FacetAttributeList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListFacetAttributesResponse :: Newtype ListFacetAttributesResponse _
derive instance repGenericListFacetAttributesResponse :: Generic ListFacetAttributesResponse _
instance showListFacetAttributesResponse :: Show ListFacetAttributesResponse where
  show = genericShow
instance decodeListFacetAttributesResponse :: Decode ListFacetAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFacetAttributesResponse :: Encode ListFacetAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListFacetNamesRequest = ListFacetNamesRequest 
  { "SchemaArn" :: (Arn)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeListFacetNamesRequest :: Newtype ListFacetNamesRequest _
derive instance repGenericListFacetNamesRequest :: Generic ListFacetNamesRequest _
instance showListFacetNamesRequest :: Show ListFacetNamesRequest where
  show = genericShow
instance decodeListFacetNamesRequest :: Decode ListFacetNamesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFacetNamesRequest :: Encode ListFacetNamesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListFacetNamesResponse = ListFacetNamesResponse 
  { "FacetNames" :: NullOrUndefined.NullOrUndefined (FacetNameList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListFacetNamesResponse :: Newtype ListFacetNamesResponse _
derive instance repGenericListFacetNamesResponse :: Generic ListFacetNamesResponse _
instance showListFacetNamesResponse :: Show ListFacetNamesResponse where
  show = genericShow
instance decodeListFacetNamesResponse :: Decode ListFacetNamesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFacetNamesResponse :: Encode ListFacetNamesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIncomingTypedLinksRequest = ListIncomingTypedLinksRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "FilterAttributeRanges" :: NullOrUndefined.NullOrUndefined (TypedLinkAttributeRangeList)
  , "FilterTypedLink" :: NullOrUndefined.NullOrUndefined (TypedLinkSchemaAndFacetName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined.NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListIncomingTypedLinksRequest :: Newtype ListIncomingTypedLinksRequest _
derive instance repGenericListIncomingTypedLinksRequest :: Generic ListIncomingTypedLinksRequest _
instance showListIncomingTypedLinksRequest :: Show ListIncomingTypedLinksRequest where
  show = genericShow
instance decodeListIncomingTypedLinksRequest :: Decode ListIncomingTypedLinksRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIncomingTypedLinksRequest :: Encode ListIncomingTypedLinksRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIncomingTypedLinksResponse = ListIncomingTypedLinksResponse 
  { "LinkSpecifiers" :: NullOrUndefined.NullOrUndefined (TypedLinkSpecifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListIncomingTypedLinksResponse :: Newtype ListIncomingTypedLinksResponse _
derive instance repGenericListIncomingTypedLinksResponse :: Generic ListIncomingTypedLinksResponse _
instance showListIncomingTypedLinksResponse :: Show ListIncomingTypedLinksResponse where
  show = genericShow
instance decodeListIncomingTypedLinksResponse :: Decode ListIncomingTypedLinksResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIncomingTypedLinksResponse :: Encode ListIncomingTypedLinksResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIndexRequest = ListIndexRequest 
  { "DirectoryArn" :: (Arn)
  , "RangesOnIndexedValues" :: NullOrUndefined.NullOrUndefined (ObjectAttributeRangeList)
  , "IndexReference" :: (ObjectReference)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "ConsistencyLevel" :: NullOrUndefined.NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListIndexRequest :: Newtype ListIndexRequest _
derive instance repGenericListIndexRequest :: Generic ListIndexRequest _
instance showListIndexRequest :: Show ListIndexRequest where
  show = genericShow
instance decodeListIndexRequest :: Decode ListIndexRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIndexRequest :: Encode ListIndexRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIndexResponse = ListIndexResponse 
  { "IndexAttachments" :: NullOrUndefined.NullOrUndefined (IndexAttachmentList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListIndexResponse :: Newtype ListIndexResponse _
derive instance repGenericListIndexResponse :: Generic ListIndexResponse _
instance showListIndexResponse :: Show ListIndexResponse where
  show = genericShow
instance decodeListIndexResponse :: Decode ListIndexResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIndexResponse :: Encode ListIndexResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectAttributesRequest = ListObjectAttributesRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined.NullOrUndefined (ConsistencyLevel)
  , "FacetFilter" :: NullOrUndefined.NullOrUndefined (SchemaFacet)
  }
derive instance newtypeListObjectAttributesRequest :: Newtype ListObjectAttributesRequest _
derive instance repGenericListObjectAttributesRequest :: Generic ListObjectAttributesRequest _
instance showListObjectAttributesRequest :: Show ListObjectAttributesRequest where
  show = genericShow
instance decodeListObjectAttributesRequest :: Decode ListObjectAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectAttributesRequest :: Encode ListObjectAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectAttributesResponse = ListObjectAttributesResponse 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (AttributeKeyAndValueList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListObjectAttributesResponse :: Newtype ListObjectAttributesResponse _
derive instance repGenericListObjectAttributesResponse :: Generic ListObjectAttributesResponse _
instance showListObjectAttributesResponse :: Show ListObjectAttributesResponse where
  show = genericShow
instance decodeListObjectAttributesResponse :: Decode ListObjectAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectAttributesResponse :: Encode ListObjectAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectChildrenRequest = ListObjectChildrenRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined.NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListObjectChildrenRequest :: Newtype ListObjectChildrenRequest _
derive instance repGenericListObjectChildrenRequest :: Generic ListObjectChildrenRequest _
instance showListObjectChildrenRequest :: Show ListObjectChildrenRequest where
  show = genericShow
instance decodeListObjectChildrenRequest :: Decode ListObjectChildrenRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectChildrenRequest :: Encode ListObjectChildrenRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectChildrenResponse = ListObjectChildrenResponse 
  { "Children" :: NullOrUndefined.NullOrUndefined (LinkNameToObjectIdentifierMap)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListObjectChildrenResponse :: Newtype ListObjectChildrenResponse _
derive instance repGenericListObjectChildrenResponse :: Generic ListObjectChildrenResponse _
instance showListObjectChildrenResponse :: Show ListObjectChildrenResponse where
  show = genericShow
instance decodeListObjectChildrenResponse :: Decode ListObjectChildrenResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectChildrenResponse :: Encode ListObjectChildrenResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectParentPathsRequest = ListObjectParentPathsRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeListObjectParentPathsRequest :: Newtype ListObjectParentPathsRequest _
derive instance repGenericListObjectParentPathsRequest :: Generic ListObjectParentPathsRequest _
instance showListObjectParentPathsRequest :: Show ListObjectParentPathsRequest where
  show = genericShow
instance decodeListObjectParentPathsRequest :: Decode ListObjectParentPathsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectParentPathsRequest :: Encode ListObjectParentPathsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectParentPathsResponse = ListObjectParentPathsResponse 
  { "PathToObjectIdentifiersList" :: NullOrUndefined.NullOrUndefined (PathToObjectIdentifiersList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListObjectParentPathsResponse :: Newtype ListObjectParentPathsResponse _
derive instance repGenericListObjectParentPathsResponse :: Generic ListObjectParentPathsResponse _
instance showListObjectParentPathsResponse :: Show ListObjectParentPathsResponse where
  show = genericShow
instance decodeListObjectParentPathsResponse :: Decode ListObjectParentPathsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectParentPathsResponse :: Encode ListObjectParentPathsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectParentsRequest = ListObjectParentsRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined.NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListObjectParentsRequest :: Newtype ListObjectParentsRequest _
derive instance repGenericListObjectParentsRequest :: Generic ListObjectParentsRequest _
instance showListObjectParentsRequest :: Show ListObjectParentsRequest where
  show = genericShow
instance decodeListObjectParentsRequest :: Decode ListObjectParentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectParentsRequest :: Encode ListObjectParentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectParentsResponse = ListObjectParentsResponse 
  { "Parents" :: NullOrUndefined.NullOrUndefined (ObjectIdentifierToLinkNameMap)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListObjectParentsResponse :: Newtype ListObjectParentsResponse _
derive instance repGenericListObjectParentsResponse :: Generic ListObjectParentsResponse _
instance showListObjectParentsResponse :: Show ListObjectParentsResponse where
  show = genericShow
instance decodeListObjectParentsResponse :: Decode ListObjectParentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectParentsResponse :: Encode ListObjectParentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectPoliciesRequest = ListObjectPoliciesRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined.NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListObjectPoliciesRequest :: Newtype ListObjectPoliciesRequest _
derive instance repGenericListObjectPoliciesRequest :: Generic ListObjectPoliciesRequest _
instance showListObjectPoliciesRequest :: Show ListObjectPoliciesRequest where
  show = genericShow
instance decodeListObjectPoliciesRequest :: Decode ListObjectPoliciesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectPoliciesRequest :: Encode ListObjectPoliciesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListObjectPoliciesResponse = ListObjectPoliciesResponse 
  { "AttachedPolicyIds" :: NullOrUndefined.NullOrUndefined (ObjectIdentifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListObjectPoliciesResponse :: Newtype ListObjectPoliciesResponse _
derive instance repGenericListObjectPoliciesResponse :: Generic ListObjectPoliciesResponse _
instance showListObjectPoliciesResponse :: Show ListObjectPoliciesResponse where
  show = genericShow
instance decodeListObjectPoliciesResponse :: Decode ListObjectPoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListObjectPoliciesResponse :: Encode ListObjectPoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOutgoingTypedLinksRequest = ListOutgoingTypedLinksRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "FilterAttributeRanges" :: NullOrUndefined.NullOrUndefined (TypedLinkAttributeRangeList)
  , "FilterTypedLink" :: NullOrUndefined.NullOrUndefined (TypedLinkSchemaAndFacetName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined.NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListOutgoingTypedLinksRequest :: Newtype ListOutgoingTypedLinksRequest _
derive instance repGenericListOutgoingTypedLinksRequest :: Generic ListOutgoingTypedLinksRequest _
instance showListOutgoingTypedLinksRequest :: Show ListOutgoingTypedLinksRequest where
  show = genericShow
instance decodeListOutgoingTypedLinksRequest :: Decode ListOutgoingTypedLinksRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOutgoingTypedLinksRequest :: Encode ListOutgoingTypedLinksRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOutgoingTypedLinksResponse = ListOutgoingTypedLinksResponse 
  { "TypedLinkSpecifiers" :: NullOrUndefined.NullOrUndefined (TypedLinkSpecifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListOutgoingTypedLinksResponse :: Newtype ListOutgoingTypedLinksResponse _
derive instance repGenericListOutgoingTypedLinksResponse :: Generic ListOutgoingTypedLinksResponse _
instance showListOutgoingTypedLinksResponse :: Show ListOutgoingTypedLinksResponse where
  show = genericShow
instance decodeListOutgoingTypedLinksResponse :: Decode ListOutgoingTypedLinksResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOutgoingTypedLinksResponse :: Encode ListOutgoingTypedLinksResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListPolicyAttachmentsRequest = ListPolicyAttachmentsRequest 
  { "DirectoryArn" :: (Arn)
  , "PolicyReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  , "ConsistencyLevel" :: NullOrUndefined.NullOrUndefined (ConsistencyLevel)
  }
derive instance newtypeListPolicyAttachmentsRequest :: Newtype ListPolicyAttachmentsRequest _
derive instance repGenericListPolicyAttachmentsRequest :: Generic ListPolicyAttachmentsRequest _
instance showListPolicyAttachmentsRequest :: Show ListPolicyAttachmentsRequest where
  show = genericShow
instance decodeListPolicyAttachmentsRequest :: Decode ListPolicyAttachmentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPolicyAttachmentsRequest :: Encode ListPolicyAttachmentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListPolicyAttachmentsResponse = ListPolicyAttachmentsResponse 
  { "ObjectIdentifiers" :: NullOrUndefined.NullOrUndefined (ObjectIdentifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListPolicyAttachmentsResponse :: Newtype ListPolicyAttachmentsResponse _
derive instance repGenericListPolicyAttachmentsResponse :: Generic ListPolicyAttachmentsResponse _
instance showListPolicyAttachmentsResponse :: Show ListPolicyAttachmentsResponse where
  show = genericShow
instance decodeListPolicyAttachmentsResponse :: Decode ListPolicyAttachmentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPolicyAttachmentsResponse :: Encode ListPolicyAttachmentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListPublishedSchemaArnsRequest = ListPublishedSchemaArnsRequest 
  { "SchemaArn" :: NullOrUndefined.NullOrUndefined (Arn)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeListPublishedSchemaArnsRequest :: Newtype ListPublishedSchemaArnsRequest _
derive instance repGenericListPublishedSchemaArnsRequest :: Generic ListPublishedSchemaArnsRequest _
instance showListPublishedSchemaArnsRequest :: Show ListPublishedSchemaArnsRequest where
  show = genericShow
instance decodeListPublishedSchemaArnsRequest :: Decode ListPublishedSchemaArnsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPublishedSchemaArnsRequest :: Encode ListPublishedSchemaArnsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListPublishedSchemaArnsResponse = ListPublishedSchemaArnsResponse 
  { "SchemaArns" :: NullOrUndefined.NullOrUndefined (Arns)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListPublishedSchemaArnsResponse :: Newtype ListPublishedSchemaArnsResponse _
derive instance repGenericListPublishedSchemaArnsResponse :: Generic ListPublishedSchemaArnsResponse _
instance showListPublishedSchemaArnsResponse :: Show ListPublishedSchemaArnsResponse where
  show = genericShow
instance decodeListPublishedSchemaArnsResponse :: Decode ListPublishedSchemaArnsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPublishedSchemaArnsResponse :: Encode ListPublishedSchemaArnsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceArn" :: (Arn)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (TagsNumberResults)
  }
derive instance newtypeListTagsForResourceRequest :: Newtype ListTagsForResourceRequest _
derive instance repGenericListTagsForResourceRequest :: Generic ListTagsForResourceRequest _
instance showListTagsForResourceRequest :: Show ListTagsForResourceRequest where
  show = genericShow
instance decodeListTagsForResourceRequest :: Decode ListTagsForResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceRequest :: Encode ListTagsForResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsForResourceResponse = ListTagsForResourceResponse 
  { "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListTagsForResourceResponse :: Newtype ListTagsForResourceResponse _
derive instance repGenericListTagsForResourceResponse :: Generic ListTagsForResourceResponse _
instance showListTagsForResourceResponse :: Show ListTagsForResourceResponse where
  show = genericShow
instance decodeListTagsForResourceResponse :: Decode ListTagsForResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceResponse :: Encode ListTagsForResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTypedLinkFacetAttributesRequest = ListTypedLinkFacetAttributesRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (TypedLinkName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeListTypedLinkFacetAttributesRequest :: Newtype ListTypedLinkFacetAttributesRequest _
derive instance repGenericListTypedLinkFacetAttributesRequest :: Generic ListTypedLinkFacetAttributesRequest _
instance showListTypedLinkFacetAttributesRequest :: Show ListTypedLinkFacetAttributesRequest where
  show = genericShow
instance decodeListTypedLinkFacetAttributesRequest :: Decode ListTypedLinkFacetAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTypedLinkFacetAttributesRequest :: Encode ListTypedLinkFacetAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTypedLinkFacetAttributesResponse = ListTypedLinkFacetAttributesResponse 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (TypedLinkAttributeDefinitionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListTypedLinkFacetAttributesResponse :: Newtype ListTypedLinkFacetAttributesResponse _
derive instance repGenericListTypedLinkFacetAttributesResponse :: Generic ListTypedLinkFacetAttributesResponse _
instance showListTypedLinkFacetAttributesResponse :: Show ListTypedLinkFacetAttributesResponse where
  show = genericShow
instance decodeListTypedLinkFacetAttributesResponse :: Decode ListTypedLinkFacetAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTypedLinkFacetAttributesResponse :: Encode ListTypedLinkFacetAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTypedLinkFacetNamesRequest = ListTypedLinkFacetNamesRequest 
  { "SchemaArn" :: (Arn)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeListTypedLinkFacetNamesRequest :: Newtype ListTypedLinkFacetNamesRequest _
derive instance repGenericListTypedLinkFacetNamesRequest :: Generic ListTypedLinkFacetNamesRequest _
instance showListTypedLinkFacetNamesRequest :: Show ListTypedLinkFacetNamesRequest where
  show = genericShow
instance decodeListTypedLinkFacetNamesRequest :: Decode ListTypedLinkFacetNamesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTypedLinkFacetNamesRequest :: Encode ListTypedLinkFacetNamesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTypedLinkFacetNamesResponse = ListTypedLinkFacetNamesResponse 
  { "FacetNames" :: NullOrUndefined.NullOrUndefined (TypedLinkNameList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListTypedLinkFacetNamesResponse :: Newtype ListTypedLinkFacetNamesResponse _
derive instance repGenericListTypedLinkFacetNamesResponse :: Generic ListTypedLinkFacetNamesResponse _
instance showListTypedLinkFacetNamesResponse :: Show ListTypedLinkFacetNamesResponse where
  show = genericShow
instance decodeListTypedLinkFacetNamesResponse :: Decode ListTypedLinkFacetNamesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTypedLinkFacetNamesResponse :: Encode ListTypedLinkFacetNamesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LookupPolicyRequest = LookupPolicyRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (NumberResults)
  }
derive instance newtypeLookupPolicyRequest :: Newtype LookupPolicyRequest _
derive instance repGenericLookupPolicyRequest :: Generic LookupPolicyRequest _
instance showLookupPolicyRequest :: Show LookupPolicyRequest where
  show = genericShow
instance decodeLookupPolicyRequest :: Decode LookupPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLookupPolicyRequest :: Encode LookupPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LookupPolicyResponse = LookupPolicyResponse 
  { "PolicyToPathList" :: NullOrUndefined.NullOrUndefined (PolicyToPathList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeLookupPolicyResponse :: Newtype LookupPolicyResponse _
derive instance repGenericLookupPolicyResponse :: Generic LookupPolicyResponse _
instance showLookupPolicyResponse :: Show LookupPolicyResponse where
  show = genericShow
instance decodeLookupPolicyResponse :: Decode LookupPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLookupPolicyResponse :: Encode LookupPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the requested operation can only operate on index objects.</p>
newtype NotIndexException = NotIndexException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeNotIndexException :: Newtype NotIndexException _
derive instance repGenericNotIndexException :: Generic NotIndexException _
instance showNotIndexException :: Show NotIndexException where
  show = genericShow
instance decodeNotIndexException :: Decode NotIndexException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotIndexException :: Encode NotIndexException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Occurs when any invalid operations are performed on an object that is not a node, such as calling <code>ListObjectChildren</code> for a leaf node object.</p>
newtype NotNodeException = NotNodeException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeNotNodeException :: Newtype NotNodeException _
derive instance repGenericNotNodeException :: Generic NotNodeException _
instance showNotNodeException :: Show NotNodeException where
  show = genericShow
instance decodeNotNodeException :: Decode NotNodeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotNodeException :: Encode NotNodeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the requested operation can only operate on policy objects.</p>
newtype NotPolicyException = NotPolicyException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeNotPolicyException :: Newtype NotPolicyException _
derive instance repGenericNotPolicyException :: Generic NotPolicyException _
instance showNotPolicyException :: Show NotPolicyException where
  show = genericShow
instance decodeNotPolicyException :: Decode NotPolicyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotPolicyException :: Encode NotPolicyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NumberAttributeValue = NumberAttributeValue String
derive instance newtypeNumberAttributeValue :: Newtype NumberAttributeValue _
derive instance repGenericNumberAttributeValue :: Generic NumberAttributeValue _
instance showNumberAttributeValue :: Show NumberAttributeValue where
  show = genericShow
instance decodeNumberAttributeValue :: Decode NumberAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNumberAttributeValue :: Encode NumberAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NumberResults = NumberResults Int
derive instance newtypeNumberResults :: Newtype NumberResults _
derive instance repGenericNumberResults :: Generic NumberResults _
instance showNumberResults :: Show NumberResults where
  show = genericShow
instance decodeNumberResults :: Decode NumberResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNumberResults :: Encode NumberResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the object is not attached to the index.</p>
newtype ObjectAlreadyDetachedException = ObjectAlreadyDetachedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeObjectAlreadyDetachedException :: Newtype ObjectAlreadyDetachedException _
derive instance repGenericObjectAlreadyDetachedException :: Generic ObjectAlreadyDetachedException _
instance showObjectAlreadyDetachedException :: Show ObjectAlreadyDetachedException where
  show = genericShow
instance decodeObjectAlreadyDetachedException :: Decode ObjectAlreadyDetachedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectAlreadyDetachedException :: Encode ObjectAlreadyDetachedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The action to take on the object attribute.</p>
newtype ObjectAttributeAction = ObjectAttributeAction 
  { "ObjectAttributeActionType" :: NullOrUndefined.NullOrUndefined (UpdateActionType)
  , "ObjectAttributeUpdateValue" :: NullOrUndefined.NullOrUndefined (TypedAttributeValue)
  }
derive instance newtypeObjectAttributeAction :: Newtype ObjectAttributeAction _
derive instance repGenericObjectAttributeAction :: Generic ObjectAttributeAction _
instance showObjectAttributeAction :: Show ObjectAttributeAction where
  show = genericShow
instance decodeObjectAttributeAction :: Decode ObjectAttributeAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectAttributeAction :: Encode ObjectAttributeAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A range of attributes.</p>
newtype ObjectAttributeRange = ObjectAttributeRange 
  { "AttributeKey" :: NullOrUndefined.NullOrUndefined (AttributeKey)
  , "Range" :: NullOrUndefined.NullOrUndefined (TypedAttributeValueRange)
  }
derive instance newtypeObjectAttributeRange :: Newtype ObjectAttributeRange _
derive instance repGenericObjectAttributeRange :: Generic ObjectAttributeRange _
instance showObjectAttributeRange :: Show ObjectAttributeRange where
  show = genericShow
instance decodeObjectAttributeRange :: Decode ObjectAttributeRange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectAttributeRange :: Encode ObjectAttributeRange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectAttributeRangeList = ObjectAttributeRangeList (Array ObjectAttributeRange)
derive instance newtypeObjectAttributeRangeList :: Newtype ObjectAttributeRangeList _
derive instance repGenericObjectAttributeRangeList :: Generic ObjectAttributeRangeList _
instance showObjectAttributeRangeList :: Show ObjectAttributeRangeList where
  show = genericShow
instance decodeObjectAttributeRangeList :: Decode ObjectAttributeRangeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectAttributeRangeList :: Encode ObjectAttributeRangeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Structure that contains attribute update information.</p>
newtype ObjectAttributeUpdate = ObjectAttributeUpdate 
  { "ObjectAttributeKey" :: NullOrUndefined.NullOrUndefined (AttributeKey)
  , "ObjectAttributeAction" :: NullOrUndefined.NullOrUndefined (ObjectAttributeAction)
  }
derive instance newtypeObjectAttributeUpdate :: Newtype ObjectAttributeUpdate _
derive instance repGenericObjectAttributeUpdate :: Generic ObjectAttributeUpdate _
instance showObjectAttributeUpdate :: Show ObjectAttributeUpdate where
  show = genericShow
instance decodeObjectAttributeUpdate :: Decode ObjectAttributeUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectAttributeUpdate :: Encode ObjectAttributeUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectAttributeUpdateList = ObjectAttributeUpdateList (Array ObjectAttributeUpdate)
derive instance newtypeObjectAttributeUpdateList :: Newtype ObjectAttributeUpdateList _
derive instance repGenericObjectAttributeUpdateList :: Generic ObjectAttributeUpdateList _
instance showObjectAttributeUpdateList :: Show ObjectAttributeUpdateList where
  show = genericShow
instance decodeObjectAttributeUpdateList :: Decode ObjectAttributeUpdateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectAttributeUpdateList :: Encode ObjectAttributeUpdateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectIdentifier = ObjectIdentifier String
derive instance newtypeObjectIdentifier :: Newtype ObjectIdentifier _
derive instance repGenericObjectIdentifier :: Generic ObjectIdentifier _
instance showObjectIdentifier :: Show ObjectIdentifier where
  show = genericShow
instance decodeObjectIdentifier :: Decode ObjectIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectIdentifier :: Encode ObjectIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectIdentifierList = ObjectIdentifierList (Array ObjectIdentifier)
derive instance newtypeObjectIdentifierList :: Newtype ObjectIdentifierList _
derive instance repGenericObjectIdentifierList :: Generic ObjectIdentifierList _
instance showObjectIdentifierList :: Show ObjectIdentifierList where
  show = genericShow
instance decodeObjectIdentifierList :: Decode ObjectIdentifierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectIdentifierList :: Encode ObjectIdentifierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectIdentifierToLinkNameMap = ObjectIdentifierToLinkNameMap (StrMap.StrMap LinkName)
derive instance newtypeObjectIdentifierToLinkNameMap :: Newtype ObjectIdentifierToLinkNameMap _
derive instance repGenericObjectIdentifierToLinkNameMap :: Generic ObjectIdentifierToLinkNameMap _
instance showObjectIdentifierToLinkNameMap :: Show ObjectIdentifierToLinkNameMap where
  show = genericShow
instance decodeObjectIdentifierToLinkNameMap :: Decode ObjectIdentifierToLinkNameMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectIdentifierToLinkNameMap :: Encode ObjectIdentifierToLinkNameMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the requested operation cannot be completed because the object has not been detached from the tree.</p>
newtype ObjectNotDetachedException = ObjectNotDetachedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeObjectNotDetachedException :: Newtype ObjectNotDetachedException _
derive instance repGenericObjectNotDetachedException :: Generic ObjectNotDetachedException _
instance showObjectNotDetachedException :: Show ObjectNotDetachedException where
  show = genericShow
instance decodeObjectNotDetachedException :: Decode ObjectNotDetachedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectNotDetachedException :: Encode ObjectNotDetachedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The reference that identifies an object.</p>
newtype ObjectReference = ObjectReference 
  { "Selector" :: NullOrUndefined.NullOrUndefined (SelectorObjectReference)
  }
derive instance newtypeObjectReference :: Newtype ObjectReference _
derive instance repGenericObjectReference :: Generic ObjectReference _
instance showObjectReference :: Show ObjectReference where
  show = genericShow
instance decodeObjectReference :: Decode ObjectReference where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectReference :: Encode ObjectReference where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ObjectType = ObjectType String
derive instance newtypeObjectType :: Newtype ObjectType _
derive instance repGenericObjectType :: Generic ObjectType _
instance showObjectType :: Show ObjectType where
  show = genericShow
instance decodeObjectType :: Decode ObjectType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeObjectType :: Encode ObjectType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PathString = PathString String
derive instance newtypePathString :: Newtype PathString _
derive instance repGenericPathString :: Generic PathString _
instance showPathString :: Show PathString where
  show = genericShow
instance decodePathString :: Decode PathString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePathString :: Encode PathString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the path to the <code>ObjectIdentifiers</code> that is associated with the directory.</p>
newtype PathToObjectIdentifiers = PathToObjectIdentifiers 
  { "Path" :: NullOrUndefined.NullOrUndefined (PathString)
  , "ObjectIdentifiers" :: NullOrUndefined.NullOrUndefined (ObjectIdentifierList)
  }
derive instance newtypePathToObjectIdentifiers :: Newtype PathToObjectIdentifiers _
derive instance repGenericPathToObjectIdentifiers :: Generic PathToObjectIdentifiers _
instance showPathToObjectIdentifiers :: Show PathToObjectIdentifiers where
  show = genericShow
instance decodePathToObjectIdentifiers :: Decode PathToObjectIdentifiers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePathToObjectIdentifiers :: Encode PathToObjectIdentifiers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PathToObjectIdentifiersList = PathToObjectIdentifiersList (Array PathToObjectIdentifiers)
derive instance newtypePathToObjectIdentifiersList :: Newtype PathToObjectIdentifiersList _
derive instance repGenericPathToObjectIdentifiersList :: Generic PathToObjectIdentifiersList _
instance showPathToObjectIdentifiersList :: Show PathToObjectIdentifiersList where
  show = genericShow
instance decodePathToObjectIdentifiersList :: Decode PathToObjectIdentifiersList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePathToObjectIdentifiersList :: Encode PathToObjectIdentifiersList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the <code>PolicyType</code>, <code>PolicyId</code>, and the <code>ObjectIdentifier</code> to which it is attached. For more information, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies">Policies</a>.</p>
newtype PolicyAttachment = PolicyAttachment 
  { "PolicyId" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  , "ObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  , "PolicyType" :: NullOrUndefined.NullOrUndefined (PolicyType)
  }
derive instance newtypePolicyAttachment :: Newtype PolicyAttachment _
derive instance repGenericPolicyAttachment :: Generic PolicyAttachment _
instance showPolicyAttachment :: Show PolicyAttachment where
  show = genericShow
instance decodePolicyAttachment :: Decode PolicyAttachment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyAttachment :: Encode PolicyAttachment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyAttachmentList = PolicyAttachmentList (Array PolicyAttachment)
derive instance newtypePolicyAttachmentList :: Newtype PolicyAttachmentList _
derive instance repGenericPolicyAttachmentList :: Generic PolicyAttachmentList _
instance showPolicyAttachmentList :: Show PolicyAttachmentList where
  show = genericShow
instance decodePolicyAttachmentList :: Decode PolicyAttachmentList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyAttachmentList :: Encode PolicyAttachmentList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Used when a regular object exists in a <a>Directory</a> and you want to find all of the policies that are associated with that object and the parent to that object.</p>
newtype PolicyToPath = PolicyToPath 
  { "Path" :: NullOrUndefined.NullOrUndefined (PathString)
  , "Policies" :: NullOrUndefined.NullOrUndefined (PolicyAttachmentList)
  }
derive instance newtypePolicyToPath :: Newtype PolicyToPath _
derive instance repGenericPolicyToPath :: Generic PolicyToPath _
instance showPolicyToPath :: Show PolicyToPath where
  show = genericShow
instance decodePolicyToPath :: Decode PolicyToPath where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyToPath :: Encode PolicyToPath where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyToPathList = PolicyToPathList (Array PolicyToPath)
derive instance newtypePolicyToPathList :: Newtype PolicyToPathList _
derive instance repGenericPolicyToPathList :: Generic PolicyToPathList _
instance showPolicyToPathList :: Show PolicyToPathList where
  show = genericShow
instance decodePolicyToPathList :: Decode PolicyToPathList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyToPathList :: Encode PolicyToPathList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyType = PolicyType String
derive instance newtypePolicyType :: Newtype PolicyType _
derive instance repGenericPolicyType :: Generic PolicyType _
instance showPolicyType :: Show PolicyType where
  show = genericShow
instance decodePolicyType :: Decode PolicyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyType :: Encode PolicyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PublishSchemaRequest = PublishSchemaRequest 
  { "DevelopmentSchemaArn" :: (Arn)
  , "Version" :: (Version)
  , "MinorVersion" :: NullOrUndefined.NullOrUndefined (Version)
  , "Name" :: NullOrUndefined.NullOrUndefined (SchemaName)
  }
derive instance newtypePublishSchemaRequest :: Newtype PublishSchemaRequest _
derive instance repGenericPublishSchemaRequest :: Generic PublishSchemaRequest _
instance showPublishSchemaRequest :: Show PublishSchemaRequest where
  show = genericShow
instance decodePublishSchemaRequest :: Decode PublishSchemaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePublishSchemaRequest :: Encode PublishSchemaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PublishSchemaResponse = PublishSchemaResponse 
  { "PublishedSchemaArn" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypePublishSchemaResponse :: Newtype PublishSchemaResponse _
derive instance repGenericPublishSchemaResponse :: Generic PublishSchemaResponse _
instance showPublishSchemaResponse :: Show PublishSchemaResponse where
  show = genericShow
instance decodePublishSchemaResponse :: Decode PublishSchemaResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePublishSchemaResponse :: Encode PublishSchemaResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutSchemaFromJsonRequest = PutSchemaFromJsonRequest 
  { "SchemaArn" :: (Arn)
  , "Document" :: (SchemaJsonDocument)
  }
derive instance newtypePutSchemaFromJsonRequest :: Newtype PutSchemaFromJsonRequest _
derive instance repGenericPutSchemaFromJsonRequest :: Generic PutSchemaFromJsonRequest _
instance showPutSchemaFromJsonRequest :: Show PutSchemaFromJsonRequest where
  show = genericShow
instance decodePutSchemaFromJsonRequest :: Decode PutSchemaFromJsonRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutSchemaFromJsonRequest :: Encode PutSchemaFromJsonRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutSchemaFromJsonResponse = PutSchemaFromJsonResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypePutSchemaFromJsonResponse :: Newtype PutSchemaFromJsonResponse _
derive instance repGenericPutSchemaFromJsonResponse :: Generic PutSchemaFromJsonResponse _
instance showPutSchemaFromJsonResponse :: Show PutSchemaFromJsonResponse where
  show = genericShow
instance decodePutSchemaFromJsonResponse :: Decode PutSchemaFromJsonResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutSchemaFromJsonResponse :: Encode PutSchemaFromJsonResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RangeMode = RangeMode String
derive instance newtypeRangeMode :: Newtype RangeMode _
derive instance repGenericRangeMode :: Generic RangeMode _
instance showRangeMode :: Show RangeMode where
  show = genericShow
instance decodeRangeMode :: Decode RangeMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRangeMode :: Encode RangeMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveFacetFromObjectRequest = RemoveFacetFromObjectRequest 
  { "DirectoryArn" :: (Arn)
  , "SchemaFacet" :: (SchemaFacet)
  , "ObjectReference" :: (ObjectReference)
  }
derive instance newtypeRemoveFacetFromObjectRequest :: Newtype RemoveFacetFromObjectRequest _
derive instance repGenericRemoveFacetFromObjectRequest :: Generic RemoveFacetFromObjectRequest _
instance showRemoveFacetFromObjectRequest :: Show RemoveFacetFromObjectRequest where
  show = genericShow
instance decodeRemoveFacetFromObjectRequest :: Decode RemoveFacetFromObjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveFacetFromObjectRequest :: Encode RemoveFacetFromObjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveFacetFromObjectResponse = RemoveFacetFromObjectResponse Types.NoArguments
derive instance newtypeRemoveFacetFromObjectResponse :: Newtype RemoveFacetFromObjectResponse _
derive instance repGenericRemoveFacetFromObjectResponse :: Generic RemoveFacetFromObjectResponse _
instance showRemoveFacetFromObjectResponse :: Show RemoveFacetFromObjectResponse where
  show = genericShow
instance decodeRemoveFacetFromObjectResponse :: Decode RemoveFacetFromObjectResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveFacetFromObjectResponse :: Encode RemoveFacetFromObjectResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequiredAttributeBehavior = RequiredAttributeBehavior String
derive instance newtypeRequiredAttributeBehavior :: Newtype RequiredAttributeBehavior _
derive instance repGenericRequiredAttributeBehavior :: Generic RequiredAttributeBehavior _
instance showRequiredAttributeBehavior :: Show RequiredAttributeBehavior where
  show = genericShow
instance decodeRequiredAttributeBehavior :: Decode RequiredAttributeBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequiredAttributeBehavior :: Encode RequiredAttributeBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified resource could not be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Occurs when a conflict with a previous successful write is detected. For example, if a write operation occurs on an object and then an attempt is made to read the object using “SERIALIZABLE” consistency, this exception may result. This generally occurs when the previous write did not have time to propagate to the host serving the current request. A retry (with appropriate backoff logic) is the recommended response to this exception.</p>
newtype RetryableConflictException = RetryableConflictException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeRetryableConflictException :: Newtype RetryableConflictException _
derive instance repGenericRetryableConflictException :: Generic RetryableConflictException _
instance showRetryableConflictException :: Show RetryableConflictException where
  show = genericShow
instance decodeRetryableConflictException :: Decode RetryableConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetryableConflictException :: Encode RetryableConflictException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains an Amazon Resource Name (ARN) and parameters that are associated with the rule.</p>
newtype Rule = Rule 
  { "Type" :: NullOrUndefined.NullOrUndefined (RuleType)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (RuleParameterMap)
  }
derive instance newtypeRule :: Newtype Rule _
derive instance repGenericRule :: Generic Rule _
instance showRule :: Show Rule where
  show = genericShow
instance decodeRule :: Decode Rule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRule :: Encode Rule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleKey = RuleKey String
derive instance newtypeRuleKey :: Newtype RuleKey _
derive instance repGenericRuleKey :: Generic RuleKey _
instance showRuleKey :: Show RuleKey where
  show = genericShow
instance decodeRuleKey :: Decode RuleKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleKey :: Encode RuleKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleMap = RuleMap (StrMap.StrMap Rule)
derive instance newtypeRuleMap :: Newtype RuleMap _
derive instance repGenericRuleMap :: Generic RuleMap _
instance showRuleMap :: Show RuleMap where
  show = genericShow
instance decodeRuleMap :: Decode RuleMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleMap :: Encode RuleMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleParameterKey = RuleParameterKey String
derive instance newtypeRuleParameterKey :: Newtype RuleParameterKey _
derive instance repGenericRuleParameterKey :: Generic RuleParameterKey _
instance showRuleParameterKey :: Show RuleParameterKey where
  show = genericShow
instance decodeRuleParameterKey :: Decode RuleParameterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleParameterKey :: Encode RuleParameterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleParameterMap = RuleParameterMap (StrMap.StrMap RuleParameterValue)
derive instance newtypeRuleParameterMap :: Newtype RuleParameterMap _
derive instance repGenericRuleParameterMap :: Generic RuleParameterMap _
instance showRuleParameterMap :: Show RuleParameterMap where
  show = genericShow
instance decodeRuleParameterMap :: Decode RuleParameterMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleParameterMap :: Encode RuleParameterMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleParameterValue = RuleParameterValue String
derive instance newtypeRuleParameterValue :: Newtype RuleParameterValue _
derive instance repGenericRuleParameterValue :: Generic RuleParameterValue _
instance showRuleParameterValue :: Show RuleParameterValue where
  show = genericShow
instance decodeRuleParameterValue :: Decode RuleParameterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleParameterValue :: Encode RuleParameterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleType = RuleType String
derive instance newtypeRuleType :: Newtype RuleType _
derive instance repGenericRuleType :: Generic RuleType _
instance showRuleType :: Show RuleType where
  show = genericShow
instance decodeRuleType :: Decode RuleType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleType :: Encode RuleType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that a schema could not be created due to a naming conflict. Please select a different name and then try again.</p>
newtype SchemaAlreadyExistsException = SchemaAlreadyExistsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeSchemaAlreadyExistsException :: Newtype SchemaAlreadyExistsException _
derive instance repGenericSchemaAlreadyExistsException :: Generic SchemaAlreadyExistsException _
instance showSchemaAlreadyExistsException :: Show SchemaAlreadyExistsException where
  show = genericShow
instance decodeSchemaAlreadyExistsException :: Decode SchemaAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaAlreadyExistsException :: Encode SchemaAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that a schema is already published.</p>
newtype SchemaAlreadyPublishedException = SchemaAlreadyPublishedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeSchemaAlreadyPublishedException :: Newtype SchemaAlreadyPublishedException _
derive instance repGenericSchemaAlreadyPublishedException :: Generic SchemaAlreadyPublishedException _
instance showSchemaAlreadyPublishedException :: Show SchemaAlreadyPublishedException where
  show = genericShow
instance decodeSchemaAlreadyPublishedException :: Decode SchemaAlreadyPublishedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaAlreadyPublishedException :: Encode SchemaAlreadyPublishedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A facet.</p>
newtype SchemaFacet = SchemaFacet 
  { "SchemaArn" :: NullOrUndefined.NullOrUndefined (Arn)
  , "FacetName" :: NullOrUndefined.NullOrUndefined (FacetName)
  }
derive instance newtypeSchemaFacet :: Newtype SchemaFacet _
derive instance repGenericSchemaFacet :: Generic SchemaFacet _
instance showSchemaFacet :: Show SchemaFacet where
  show = genericShow
instance decodeSchemaFacet :: Decode SchemaFacet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaFacet :: Encode SchemaFacet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SchemaFacetList = SchemaFacetList (Array SchemaFacet)
derive instance newtypeSchemaFacetList :: Newtype SchemaFacetList _
derive instance repGenericSchemaFacetList :: Generic SchemaFacetList _
instance showSchemaFacetList :: Show SchemaFacetList where
  show = genericShow
instance decodeSchemaFacetList :: Decode SchemaFacetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaFacetList :: Encode SchemaFacetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SchemaJsonDocument = SchemaJsonDocument String
derive instance newtypeSchemaJsonDocument :: Newtype SchemaJsonDocument _
derive instance repGenericSchemaJsonDocument :: Generic SchemaJsonDocument _
instance showSchemaJsonDocument :: Show SchemaJsonDocument where
  show = genericShow
instance decodeSchemaJsonDocument :: Decode SchemaJsonDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaJsonDocument :: Encode SchemaJsonDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SchemaName = SchemaName String
derive instance newtypeSchemaName :: Newtype SchemaName _
derive instance repGenericSchemaName :: Generic SchemaName _
instance showSchemaName :: Show SchemaName where
  show = genericShow
instance decodeSchemaName :: Decode SchemaName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaName :: Encode SchemaName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SelectorObjectReference = SelectorObjectReference String
derive instance newtypeSelectorObjectReference :: Newtype SelectorObjectReference _
derive instance repGenericSelectorObjectReference :: Generic SelectorObjectReference _
instance showSelectorObjectReference :: Show SelectorObjectReference where
  show = genericShow
instance decodeSelectorObjectReference :: Decode SelectorObjectReference where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSelectorObjectReference :: Encode SelectorObjectReference where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The object could not be deleted because links still exist. Remove the links and then try the operation again.</p>
newtype StillContainsLinksException = StillContainsLinksException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeStillContainsLinksException :: Newtype StillContainsLinksException _
derive instance repGenericStillContainsLinksException :: Generic StillContainsLinksException _
instance showStillContainsLinksException :: Show StillContainsLinksException where
  show = genericShow
instance decodeStillContainsLinksException :: Decode StillContainsLinksException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStillContainsLinksException :: Encode StillContainsLinksException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringAttributeValue = StringAttributeValue String
derive instance newtypeStringAttributeValue :: Newtype StringAttributeValue _
derive instance repGenericStringAttributeValue :: Generic StringAttributeValue _
instance showStringAttributeValue :: Show StringAttributeValue where
  show = genericShow
instance decodeStringAttributeValue :: Decode StringAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringAttributeValue :: Encode StringAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The tag structure that contains a tag key and value.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined.NullOrUndefined (TagKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagResourceRequest = TagResourceRequest 
  { "ResourceArn" :: (Arn)
  , "Tags" :: (TagList)
  }
derive instance newtypeTagResourceRequest :: Newtype TagResourceRequest _
derive instance repGenericTagResourceRequest :: Generic TagResourceRequest _
instance showTagResourceRequest :: Show TagResourceRequest where
  show = genericShow
instance decodeTagResourceRequest :: Decode TagResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourceRequest :: Encode TagResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagResourceResponse = TagResourceResponse Types.NoArguments
derive instance newtypeTagResourceResponse :: Newtype TagResourceResponse _
derive instance repGenericTagResourceResponse :: Generic TagResourceResponse _
instance showTagResourceResponse :: Show TagResourceResponse where
  show = genericShow
instance decodeTagResourceResponse :: Decode TagResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourceResponse :: Encode TagResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagsNumberResults = TagsNumberResults Int
derive instance newtypeTagsNumberResults :: Newtype TagsNumberResults _
derive instance repGenericTagsNumberResults :: Generic TagsNumberResults _
instance showTagsNumberResults :: Show TagsNumberResults where
  show = genericShow
instance decodeTagsNumberResults :: Decode TagsNumberResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagsNumberResults :: Encode TagsNumberResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the data for a typed attribute. You can set one, and only one, of the elements. Each attribute in an item is a name-value pair. Attributes have a single value.</p>
newtype TypedAttributeValue = TypedAttributeValue 
  { "StringValue" :: NullOrUndefined.NullOrUndefined (StringAttributeValue)
  , "BinaryValue" :: NullOrUndefined.NullOrUndefined (BinaryAttributeValue)
  , "BooleanValue" :: NullOrUndefined.NullOrUndefined (BooleanAttributeValue)
  , "NumberValue" :: NullOrUndefined.NullOrUndefined (NumberAttributeValue)
  , "DatetimeValue" :: NullOrUndefined.NullOrUndefined (DatetimeAttributeValue)
  }
derive instance newtypeTypedAttributeValue :: Newtype TypedAttributeValue _
derive instance repGenericTypedAttributeValue :: Generic TypedAttributeValue _
instance showTypedAttributeValue :: Show TypedAttributeValue where
  show = genericShow
instance decodeTypedAttributeValue :: Decode TypedAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedAttributeValue :: Encode TypedAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A range of attribute values.</p>
newtype TypedAttributeValueRange = TypedAttributeValueRange 
  { "StartMode" :: (RangeMode)
  , "StartValue" :: NullOrUndefined.NullOrUndefined (TypedAttributeValue)
  , "EndMode" :: (RangeMode)
  , "EndValue" :: NullOrUndefined.NullOrUndefined (TypedAttributeValue)
  }
derive instance newtypeTypedAttributeValueRange :: Newtype TypedAttributeValueRange _
derive instance repGenericTypedAttributeValueRange :: Generic TypedAttributeValueRange _
instance showTypedAttributeValueRange :: Show TypedAttributeValueRange where
  show = genericShow
instance decodeTypedAttributeValueRange :: Decode TypedAttributeValueRange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedAttributeValueRange :: Encode TypedAttributeValueRange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A typed link attribute definition.</p>
newtype TypedLinkAttributeDefinition = TypedLinkAttributeDefinition 
  { "Name" :: (AttributeName)
  , "Type" :: (FacetAttributeType)
  , "DefaultValue" :: NullOrUndefined.NullOrUndefined (TypedAttributeValue)
  , "IsImmutable" :: NullOrUndefined.NullOrUndefined (Bool)
  , "Rules" :: NullOrUndefined.NullOrUndefined (RuleMap)
  , "RequiredBehavior" :: (RequiredAttributeBehavior)
  }
derive instance newtypeTypedLinkAttributeDefinition :: Newtype TypedLinkAttributeDefinition _
derive instance repGenericTypedLinkAttributeDefinition :: Generic TypedLinkAttributeDefinition _
instance showTypedLinkAttributeDefinition :: Show TypedLinkAttributeDefinition where
  show = genericShow
instance decodeTypedLinkAttributeDefinition :: Decode TypedLinkAttributeDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedLinkAttributeDefinition :: Encode TypedLinkAttributeDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TypedLinkAttributeDefinitionList = TypedLinkAttributeDefinitionList (Array TypedLinkAttributeDefinition)
derive instance newtypeTypedLinkAttributeDefinitionList :: Newtype TypedLinkAttributeDefinitionList _
derive instance repGenericTypedLinkAttributeDefinitionList :: Generic TypedLinkAttributeDefinitionList _
instance showTypedLinkAttributeDefinitionList :: Show TypedLinkAttributeDefinitionList where
  show = genericShow
instance decodeTypedLinkAttributeDefinitionList :: Decode TypedLinkAttributeDefinitionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedLinkAttributeDefinitionList :: Encode TypedLinkAttributeDefinitionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Identifies the range of attributes that are used by a specified filter.</p>
newtype TypedLinkAttributeRange = TypedLinkAttributeRange 
  { "AttributeName" :: NullOrUndefined.NullOrUndefined (AttributeName)
  , "Range" :: (TypedAttributeValueRange)
  }
derive instance newtypeTypedLinkAttributeRange :: Newtype TypedLinkAttributeRange _
derive instance repGenericTypedLinkAttributeRange :: Generic TypedLinkAttributeRange _
instance showTypedLinkAttributeRange :: Show TypedLinkAttributeRange where
  show = genericShow
instance decodeTypedLinkAttributeRange :: Decode TypedLinkAttributeRange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedLinkAttributeRange :: Encode TypedLinkAttributeRange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TypedLinkAttributeRangeList = TypedLinkAttributeRangeList (Array TypedLinkAttributeRange)
derive instance newtypeTypedLinkAttributeRangeList :: Newtype TypedLinkAttributeRangeList _
derive instance repGenericTypedLinkAttributeRangeList :: Generic TypedLinkAttributeRangeList _
instance showTypedLinkAttributeRangeList :: Show TypedLinkAttributeRangeList where
  show = genericShow
instance decodeTypedLinkAttributeRangeList :: Decode TypedLinkAttributeRangeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedLinkAttributeRangeList :: Encode TypedLinkAttributeRangeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines the typed links structure and its attributes. To create a typed link facet, use the <a>CreateTypedLinkFacet</a> API.</p>
newtype TypedLinkFacet = TypedLinkFacet 
  { "Name" :: (TypedLinkName)
  , "Attributes" :: (TypedLinkAttributeDefinitionList)
  , "IdentityAttributeOrder" :: (AttributeNameList)
  }
derive instance newtypeTypedLinkFacet :: Newtype TypedLinkFacet _
derive instance repGenericTypedLinkFacet :: Generic TypedLinkFacet _
instance showTypedLinkFacet :: Show TypedLinkFacet where
  show = genericShow
instance decodeTypedLinkFacet :: Decode TypedLinkFacet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedLinkFacet :: Encode TypedLinkFacet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A typed link facet attribute update.</p>
newtype TypedLinkFacetAttributeUpdate = TypedLinkFacetAttributeUpdate 
  { "Attribute" :: (TypedLinkAttributeDefinition)
  , "Action" :: (UpdateActionType)
  }
derive instance newtypeTypedLinkFacetAttributeUpdate :: Newtype TypedLinkFacetAttributeUpdate _
derive instance repGenericTypedLinkFacetAttributeUpdate :: Generic TypedLinkFacetAttributeUpdate _
instance showTypedLinkFacetAttributeUpdate :: Show TypedLinkFacetAttributeUpdate where
  show = genericShow
instance decodeTypedLinkFacetAttributeUpdate :: Decode TypedLinkFacetAttributeUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedLinkFacetAttributeUpdate :: Encode TypedLinkFacetAttributeUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TypedLinkFacetAttributeUpdateList = TypedLinkFacetAttributeUpdateList (Array TypedLinkFacetAttributeUpdate)
derive instance newtypeTypedLinkFacetAttributeUpdateList :: Newtype TypedLinkFacetAttributeUpdateList _
derive instance repGenericTypedLinkFacetAttributeUpdateList :: Generic TypedLinkFacetAttributeUpdateList _
instance showTypedLinkFacetAttributeUpdateList :: Show TypedLinkFacetAttributeUpdateList where
  show = genericShow
instance decodeTypedLinkFacetAttributeUpdateList :: Decode TypedLinkFacetAttributeUpdateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedLinkFacetAttributeUpdateList :: Encode TypedLinkFacetAttributeUpdateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TypedLinkName = TypedLinkName String
derive instance newtypeTypedLinkName :: Newtype TypedLinkName _
derive instance repGenericTypedLinkName :: Generic TypedLinkName _
instance showTypedLinkName :: Show TypedLinkName where
  show = genericShow
instance decodeTypedLinkName :: Decode TypedLinkName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedLinkName :: Encode TypedLinkName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TypedLinkNameList = TypedLinkNameList (Array TypedLinkName)
derive instance newtypeTypedLinkNameList :: Newtype TypedLinkNameList _
derive instance repGenericTypedLinkNameList :: Generic TypedLinkNameList _
instance showTypedLinkNameList :: Show TypedLinkNameList where
  show = genericShow
instance decodeTypedLinkNameList :: Decode TypedLinkNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedLinkNameList :: Encode TypedLinkNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Identifies the schema Amazon Resource Name (ARN) and facet name for the typed link.</p>
newtype TypedLinkSchemaAndFacetName = TypedLinkSchemaAndFacetName 
  { "SchemaArn" :: (Arn)
  , "TypedLinkName" :: (TypedLinkName)
  }
derive instance newtypeTypedLinkSchemaAndFacetName :: Newtype TypedLinkSchemaAndFacetName _
derive instance repGenericTypedLinkSchemaAndFacetName :: Generic TypedLinkSchemaAndFacetName _
instance showTypedLinkSchemaAndFacetName :: Show TypedLinkSchemaAndFacetName where
  show = genericShow
instance decodeTypedLinkSchemaAndFacetName :: Decode TypedLinkSchemaAndFacetName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedLinkSchemaAndFacetName :: Encode TypedLinkSchemaAndFacetName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains all the information that is used to uniquely identify a typed link. The parameters discussed in this topic are used to uniquely specify the typed link being operated on. The <a>AttachTypedLink</a> API returns a typed link specifier while the <a>DetachTypedLink</a> API accepts one as input. Similarly, the <a>ListIncomingTypedLinks</a> and <a>ListOutgoingTypedLinks</a> API operations provide typed link specifiers as output. You can also construct a typed link specifier from scratch.</p>
newtype TypedLinkSpecifier = TypedLinkSpecifier 
  { "TypedLinkFacet" :: (TypedLinkSchemaAndFacetName)
  , "SourceObjectReference" :: (ObjectReference)
  , "TargetObjectReference" :: (ObjectReference)
  , "IdentityAttributeValues" :: (AttributeNameAndValueList)
  }
derive instance newtypeTypedLinkSpecifier :: Newtype TypedLinkSpecifier _
derive instance repGenericTypedLinkSpecifier :: Generic TypedLinkSpecifier _
instance showTypedLinkSpecifier :: Show TypedLinkSpecifier where
  show = genericShow
instance decodeTypedLinkSpecifier :: Decode TypedLinkSpecifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedLinkSpecifier :: Encode TypedLinkSpecifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TypedLinkSpecifierList = TypedLinkSpecifierList (Array TypedLinkSpecifier)
derive instance newtypeTypedLinkSpecifierList :: Newtype TypedLinkSpecifierList _
derive instance repGenericTypedLinkSpecifierList :: Generic TypedLinkSpecifierList _
instance showTypedLinkSpecifierList :: Show TypedLinkSpecifierList where
  show = genericShow
instance decodeTypedLinkSpecifierList :: Decode TypedLinkSpecifierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypedLinkSpecifierList :: Encode TypedLinkSpecifierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the requested index type is not supported.</p>
newtype UnsupportedIndexTypeException = UnsupportedIndexTypeException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeUnsupportedIndexTypeException :: Newtype UnsupportedIndexTypeException _
derive instance repGenericUnsupportedIndexTypeException :: Generic UnsupportedIndexTypeException _
instance showUnsupportedIndexTypeException :: Show UnsupportedIndexTypeException where
  show = genericShow
instance decodeUnsupportedIndexTypeException :: Decode UnsupportedIndexTypeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedIndexTypeException :: Encode UnsupportedIndexTypeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceArn" :: (Arn)
  , "TagKeys" :: (TagKeyList)
  }
derive instance newtypeUntagResourceRequest :: Newtype UntagResourceRequest _
derive instance repGenericUntagResourceRequest :: Generic UntagResourceRequest _
instance showUntagResourceRequest :: Show UntagResourceRequest where
  show = genericShow
instance decodeUntagResourceRequest :: Decode UntagResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagResourceRequest :: Encode UntagResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UntagResourceResponse = UntagResourceResponse Types.NoArguments
derive instance newtypeUntagResourceResponse :: Newtype UntagResourceResponse _
derive instance repGenericUntagResourceResponse :: Generic UntagResourceResponse _
instance showUntagResourceResponse :: Show UntagResourceResponse where
  show = genericShow
instance decodeUntagResourceResponse :: Decode UntagResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagResourceResponse :: Encode UntagResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateActionType = UpdateActionType String
derive instance newtypeUpdateActionType :: Newtype UpdateActionType _
derive instance repGenericUpdateActionType :: Generic UpdateActionType _
instance showUpdateActionType :: Show UpdateActionType where
  show = genericShow
instance decodeUpdateActionType :: Decode UpdateActionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateActionType :: Encode UpdateActionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateFacetRequest = UpdateFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (FacetName)
  , "AttributeUpdates" :: NullOrUndefined.NullOrUndefined (FacetAttributeUpdateList)
  , "ObjectType" :: NullOrUndefined.NullOrUndefined (ObjectType)
  }
derive instance newtypeUpdateFacetRequest :: Newtype UpdateFacetRequest _
derive instance repGenericUpdateFacetRequest :: Generic UpdateFacetRequest _
instance showUpdateFacetRequest :: Show UpdateFacetRequest where
  show = genericShow
instance decodeUpdateFacetRequest :: Decode UpdateFacetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFacetRequest :: Encode UpdateFacetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateFacetResponse = UpdateFacetResponse Types.NoArguments
derive instance newtypeUpdateFacetResponse :: Newtype UpdateFacetResponse _
derive instance repGenericUpdateFacetResponse :: Generic UpdateFacetResponse _
instance showUpdateFacetResponse :: Show UpdateFacetResponse where
  show = genericShow
instance decodeUpdateFacetResponse :: Decode UpdateFacetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFacetResponse :: Encode UpdateFacetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateObjectAttributesRequest = UpdateObjectAttributesRequest 
  { "DirectoryArn" :: (Arn)
  , "ObjectReference" :: (ObjectReference)
  , "AttributeUpdates" :: (ObjectAttributeUpdateList)
  }
derive instance newtypeUpdateObjectAttributesRequest :: Newtype UpdateObjectAttributesRequest _
derive instance repGenericUpdateObjectAttributesRequest :: Generic UpdateObjectAttributesRequest _
instance showUpdateObjectAttributesRequest :: Show UpdateObjectAttributesRequest where
  show = genericShow
instance decodeUpdateObjectAttributesRequest :: Decode UpdateObjectAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateObjectAttributesRequest :: Encode UpdateObjectAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateObjectAttributesResponse = UpdateObjectAttributesResponse 
  { "ObjectIdentifier" :: NullOrUndefined.NullOrUndefined (ObjectIdentifier)
  }
derive instance newtypeUpdateObjectAttributesResponse :: Newtype UpdateObjectAttributesResponse _
derive instance repGenericUpdateObjectAttributesResponse :: Generic UpdateObjectAttributesResponse _
instance showUpdateObjectAttributesResponse :: Show UpdateObjectAttributesResponse where
  show = genericShow
instance decodeUpdateObjectAttributesResponse :: Decode UpdateObjectAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateObjectAttributesResponse :: Encode UpdateObjectAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateSchemaRequest = UpdateSchemaRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (SchemaName)
  }
derive instance newtypeUpdateSchemaRequest :: Newtype UpdateSchemaRequest _
derive instance repGenericUpdateSchemaRequest :: Generic UpdateSchemaRequest _
instance showUpdateSchemaRequest :: Show UpdateSchemaRequest where
  show = genericShow
instance decodeUpdateSchemaRequest :: Decode UpdateSchemaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSchemaRequest :: Encode UpdateSchemaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateSchemaResponse = UpdateSchemaResponse 
  { "SchemaArn" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeUpdateSchemaResponse :: Newtype UpdateSchemaResponse _
derive instance repGenericUpdateSchemaResponse :: Generic UpdateSchemaResponse _
instance showUpdateSchemaResponse :: Show UpdateSchemaResponse where
  show = genericShow
instance decodeUpdateSchemaResponse :: Decode UpdateSchemaResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSchemaResponse :: Encode UpdateSchemaResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTypedLinkFacetRequest = UpdateTypedLinkFacetRequest 
  { "SchemaArn" :: (Arn)
  , "Name" :: (TypedLinkName)
  , "AttributeUpdates" :: (TypedLinkFacetAttributeUpdateList)
  , "IdentityAttributeOrder" :: (AttributeNameList)
  }
derive instance newtypeUpdateTypedLinkFacetRequest :: Newtype UpdateTypedLinkFacetRequest _
derive instance repGenericUpdateTypedLinkFacetRequest :: Generic UpdateTypedLinkFacetRequest _
instance showUpdateTypedLinkFacetRequest :: Show UpdateTypedLinkFacetRequest where
  show = genericShow
instance decodeUpdateTypedLinkFacetRequest :: Decode UpdateTypedLinkFacetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTypedLinkFacetRequest :: Encode UpdateTypedLinkFacetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTypedLinkFacetResponse = UpdateTypedLinkFacetResponse Types.NoArguments
derive instance newtypeUpdateTypedLinkFacetResponse :: Newtype UpdateTypedLinkFacetResponse _
derive instance repGenericUpdateTypedLinkFacetResponse :: Generic UpdateTypedLinkFacetResponse _
instance showUpdateTypedLinkFacetResponse :: Show UpdateTypedLinkFacetResponse where
  show = genericShow
instance decodeUpdateTypedLinkFacetResponse :: Decode UpdateTypedLinkFacetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTypedLinkFacetResponse :: Encode UpdateTypedLinkFacetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpgradeAppliedSchemaRequest = UpgradeAppliedSchemaRequest 
  { "PublishedSchemaArn" :: (Arn)
  , "DirectoryArn" :: (Arn)
  , "DryRun" :: NullOrUndefined.NullOrUndefined (Bool)
  }
derive instance newtypeUpgradeAppliedSchemaRequest :: Newtype UpgradeAppliedSchemaRequest _
derive instance repGenericUpgradeAppliedSchemaRequest :: Generic UpgradeAppliedSchemaRequest _
instance showUpgradeAppliedSchemaRequest :: Show UpgradeAppliedSchemaRequest where
  show = genericShow
instance decodeUpgradeAppliedSchemaRequest :: Decode UpgradeAppliedSchemaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpgradeAppliedSchemaRequest :: Encode UpgradeAppliedSchemaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpgradeAppliedSchemaResponse = UpgradeAppliedSchemaResponse 
  { "UpgradedSchemaArn" :: NullOrUndefined.NullOrUndefined (Arn)
  , "DirectoryArn" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeUpgradeAppliedSchemaResponse :: Newtype UpgradeAppliedSchemaResponse _
derive instance repGenericUpgradeAppliedSchemaResponse :: Generic UpgradeAppliedSchemaResponse _
instance showUpgradeAppliedSchemaResponse :: Show UpgradeAppliedSchemaResponse where
  show = genericShow
instance decodeUpgradeAppliedSchemaResponse :: Decode UpgradeAppliedSchemaResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpgradeAppliedSchemaResponse :: Encode UpgradeAppliedSchemaResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpgradePublishedSchemaRequest = UpgradePublishedSchemaRequest 
  { "DevelopmentSchemaArn" :: (Arn)
  , "PublishedSchemaArn" :: (Arn)
  , "MinorVersion" :: (Version)
  , "DryRun" :: NullOrUndefined.NullOrUndefined (Bool)
  }
derive instance newtypeUpgradePublishedSchemaRequest :: Newtype UpgradePublishedSchemaRequest _
derive instance repGenericUpgradePublishedSchemaRequest :: Generic UpgradePublishedSchemaRequest _
instance showUpgradePublishedSchemaRequest :: Show UpgradePublishedSchemaRequest where
  show = genericShow
instance decodeUpgradePublishedSchemaRequest :: Decode UpgradePublishedSchemaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpgradePublishedSchemaRequest :: Encode UpgradePublishedSchemaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpgradePublishedSchemaResponse = UpgradePublishedSchemaResponse 
  { "UpgradedSchemaArn" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeUpgradePublishedSchemaResponse :: Newtype UpgradePublishedSchemaResponse _
derive instance repGenericUpgradePublishedSchemaResponse :: Generic UpgradePublishedSchemaResponse _
instance showUpgradePublishedSchemaResponse :: Show UpgradePublishedSchemaResponse where
  show = genericShow
instance decodeUpgradePublishedSchemaResponse :: Decode UpgradePublishedSchemaResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpgradePublishedSchemaResponse :: Encode UpgradePublishedSchemaResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that your request is malformed in some manner. See the exception message.</p>
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeValidationException :: Newtype ValidationException _
derive instance repGenericValidationException :: Generic ValidationException _
instance showValidationException :: Show ValidationException where
  show = genericShow
instance decodeValidationException :: Decode ValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationException :: Encode ValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Version = Version String
derive instance newtypeVersion :: Newtype Version _
derive instance repGenericVersion :: Generic Version _
instance showVersion :: Show Version where
  show = genericShow
instance decodeVersion :: Decode Version where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersion :: Encode Version where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
