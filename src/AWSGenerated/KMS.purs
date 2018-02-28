

-- | <fullname>AWS Key Management Service</fullname> <p>AWS Key Management Service (AWS KMS) is an encryption and key management web service. This guide describes the AWS KMS operations that you can call programmatically. For general information about AWS KMS, see the <a href="http://docs.aws.amazon.com/kms/latest/developerguide/">AWS Key Management Service Developer Guide</a>.</p> <note> <p>AWS provides SDKs that consist of libraries and sample code for various programming languages and platforms (Java, Ruby, .Net, iOS, Android, etc.). The SDKs provide a convenient way to create programmatic access to AWS KMS and other AWS services. For example, the SDKs take care of tasks such as signing requests (see below), managing errors, and retrying requests automatically. For more information about the AWS SDKs, including how to download and install them, see <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a>.</p> </note> <p>We recommend that you use the AWS SDKs to make programmatic API calls to AWS KMS.</p> <p>Clients must support TLS (Transport Layer Security) 1.0. We recommend TLS 1.2. Clients must also support cipher suites with Perfect Forward Secrecy (PFS) such as Ephemeral Diffie-Hellman (DHE) or Elliptic Curve Ephemeral Diffie-Hellman (ECDHE). Most modern systems such as Java 7 and later support these modes.</p> <p> <b>Signing Requests</b> </p> <p>Requests must be signed by using an access key ID and a secret access key. We strongly recommend that you <i>do not</i> use your AWS account (root) access key ID and secret key for everyday work with AWS KMS. Instead, use the access key ID and secret access key for an IAM user, or you can use the AWS Security Token Service to generate temporary security credentials that you can use to sign requests.</p> <p>All AWS KMS operations require <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>.</p> <p> <b>Logging API Requests</b> </p> <p>AWS KMS supports AWS CloudTrail, a service that logs AWS API calls and related events for your AWS account and delivers them to an Amazon S3 bucket that you specify. By using the information collected by CloudTrail, you can determine what requests were made to AWS KMS, who made the request, when it was made, and so on. To learn more about CloudTrail, including how to turn it on and find your log files, see the <a href="http://docs.aws.amazon.com/awscloudtrail/latest/userguide/">AWS CloudTrail User Guide</a>.</p> <p> <b>Additional Resources</b> </p> <p>For more information about credentials and request signing, see the following:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/general/latest/gr/aws-security-credentials.html">AWS Security Credentials</a> - This topic provides general information about the types of credentials used for accessing AWS.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html">Temporary Security Credentials</a> - This section of the <i>IAM User Guide</i> describes how to create and use temporary security credentials.</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4 Signing Process</a> - This set of topics walks you through the process of signing a request using an access key ID and a secret access key.</p> </li> </ul> <p> <b>Commonly Used APIs</b> </p> <p>Of the APIs discussed in this guide, the following will prove the most useful for most applications. You will likely perform actions other than these, such as creating keys and assigning policies, by using the console.</p> <ul> <li> <p> <a>Encrypt</a> </p> </li> <li> <p> <a>Decrypt</a> </p> </li> <li> <p> <a>GenerateDataKey</a> </p> </li> <li> <p> <a>GenerateDataKeyWithoutPlaintext</a> </p> </li> </ul>
module AWS.KMS where

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

serviceName = "KMS" :: String


-- | <p>Cancels the deletion of a customer master key (CMK). When this operation is successful, the CMK is set to the <code>Disabled</code> state. To enable a CMK, use <a>EnableKey</a>. You cannot perform this operation on a CMK in a different AWS account.</p> <p>For more information about scheduling and canceling deletion of a CMK, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/deleting-keys.html">Deleting Customer Master Keys</a> in the <i>AWS Key Management Service Developer Guide</i>.</p>
cancelKeyDeletion :: forall eff. CancelKeyDeletionRequest -> Aff (exception :: EXCEPTION | eff) CancelKeyDeletionResponse
cancelKeyDeletion = Request.request serviceName "cancelKeyDeletion" 


-- | <p>Creates a display name for a customer master key (CMK). You can use an alias to identify a CMK in selected operations, such as <a>Encrypt</a> and <a>GenerateDataKey</a>. </p> <p>Each CMK can have multiple aliases, but each alias points to only one CMK. The alias name must be unique in the AWS account and region. To simplify code that runs in multiple regions, use the same alias name, but point it to a different CMK in each region. </p> <p>Because an alias is not a property of a CMK, you can delete and change the aliases of a CMK without affecting the CMK. Also, aliases do not appear in the response from the <a>DescribeKey</a> operation. To get the aliases of all CMKs, use the <a>ListAliases</a> operation.</p> <p>An alias must start with the word <code>alias</code> followed by a forward slash (<code>alias/</code>). The alias name can contain only alphanumeric characters, forward slashes (/), underscores (_), and dashes (-). Alias names cannot begin with <code>aws</code>; that alias name prefix is reserved by Amazon Web Services (AWS).</p> <p>The alias and the CMK it is mapped to must be in the same AWS account and the same region. You cannot perform this operation on an alias in a different AWS account.</p> <p>To map an existing alias to a different CMK, call <a>UpdateAlias</a>.</p>
createAlias :: forall eff. CreateAliasRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
createAlias = Request.request serviceName "createAlias" 


-- | <p>Adds a grant to a customer master key (CMK). The grant specifies who can use the CMK and under what conditions. When setting permissions, grants are an alternative to key policies. </p> <p>To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the KeyId parameter. For more information about grants, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/grants.html">Grants</a> in the <i>AWS Key Management Service Developer Guide</i>.</p>
createGrant :: forall eff. CreateGrantRequest -> Aff (exception :: EXCEPTION | eff) CreateGrantResponse
createGrant = Request.request serviceName "createGrant" 


-- | <p>Creates a customer master key (CMK) in the caller's AWS account.</p> <p>You can use a CMK to encrypt small amounts of data (4 KiB or less) directly, but CMKs are more commonly used to encrypt data encryption keys (DEKs), which are used to encrypt raw data. For more information about DEKs and the difference between CMKs and DEKs, see the following:</p> <ul> <li> <p>The <a>GenerateDataKey</a> operation</p> </li> <li> <p> <a href="http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html">AWS Key Management Service Concepts</a> in the <i>AWS Key Management Service Developer Guide</i> </p> </li> </ul> <p>You cannot use this operation to create a CMK in a different AWS account.</p>
createKey :: forall eff. CreateKeyRequest -> Aff (exception :: EXCEPTION | eff) CreateKeyResponse
createKey = Request.request serviceName "createKey" 


-- | <p>Decrypts ciphertext. Ciphertext is plaintext that has been previously encrypted by using any of the following operations:</p> <ul> <li> <p> <a>GenerateDataKey</a> </p> </li> <li> <p> <a>GenerateDataKeyWithoutPlaintext</a> </p> </li> <li> <p> <a>Encrypt</a> </p> </li> </ul> <p>Note that if a caller has been granted access permissions to all keys (through, for example, IAM user policies that grant <code>Decrypt</code> permission on all resources), then ciphertext encrypted by using keys in other accounts where the key grants access to the caller can be decrypted. To remedy this, we recommend that you do not grant <code>Decrypt</code> access in an IAM user policy. Instead grant <code>Decrypt</code> access only in key policies. If you must grant <code>Decrypt</code> access in an IAM user policy, you should scope the resource to specific keys or to specific trusted accounts.</p>
decrypt :: forall eff. DecryptRequest -> Aff (exception :: EXCEPTION | eff) DecryptResponse
decrypt = Request.request serviceName "decrypt" 


-- | <p>Deletes the specified alias. You cannot perform this operation on an alias in a different AWS account. </p> <p>Because an alias is not a property of a CMK, you can delete and change the aliases of a CMK without affecting the CMK. Also, aliases do not appear in the response from the <a>DescribeKey</a> operation. To get the aliases of all CMKs, use the <a>ListAliases</a> operation. </p> <p>Each CMK can have multiple aliases. To change the alias of a CMK, use <a>DeleteAlias</a> to delete the current alias and <a>CreateAlias</a> to create a new alias. To associate an existing alias with a different customer master key (CMK), call <a>UpdateAlias</a>.</p>
deleteAlias :: forall eff. DeleteAliasRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteAlias = Request.request serviceName "deleteAlias" 


-- | <p>Deletes key material that you previously imported. This operation makes the specified customer master key (CMK) unusable. For more information about importing key material into AWS KMS, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html">Importing Key Material</a> in the <i>AWS Key Management Service Developer Guide</i>. You cannot perform this operation on a CMK in a different AWS account.</p> <p>When the specified CMK is in the <code>PendingDeletion</code> state, this operation does not change the CMK's state. Otherwise, it changes the CMK's state to <code>PendingImport</code>.</p> <p>After you delete key material, you can use <a>ImportKeyMaterial</a> to reimport the same key material into the CMK.</p>
deleteImportedKeyMaterial :: forall eff. DeleteImportedKeyMaterialRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteImportedKeyMaterial = Request.request serviceName "deleteImportedKeyMaterial" 


-- | <p>Provides detailed information about the specified customer master key (CMK).</p> <p>To perform this operation on a CMK in a different AWS account, specify the key ARN or alias ARN in the value of the KeyId parameter.</p>
describeKey :: forall eff. DescribeKeyRequest -> Aff (exception :: EXCEPTION | eff) DescribeKeyResponse
describeKey = Request.request serviceName "describeKey" 


-- | <p>Sets the state of a customer master key (CMK) to disabled, thereby preventing its use for cryptographic operations. You cannot perform this operation on a CMK in a different AWS account.</p> <p>For more information about how key state affects the use of a CMK, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html">How Key State Affects the Use of a Customer Master Key</a> in the <i>AWS Key Management Service Developer Guide</i>.</p>
disableKey :: forall eff. DisableKeyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
disableKey = Request.request serviceName "disableKey" 


-- | <p>Disables automatic rotation of the key material for the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.</p>
disableKeyRotation :: forall eff. DisableKeyRotationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
disableKeyRotation = Request.request serviceName "disableKeyRotation" 


-- | <p>Sets the state of a customer master key (CMK) to enabled, thereby permitting its use for cryptographic operations. You cannot perform this operation on a CMK in a different AWS account.</p>
enableKey :: forall eff. EnableKeyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
enableKey = Request.request serviceName "enableKey" 


-- | <p>Enables automatic rotation of the key material for the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.</p>
enableKeyRotation :: forall eff. EnableKeyRotationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
enableKeyRotation = Request.request serviceName "enableKeyRotation" 


-- | <p>Encrypts plaintext into ciphertext by using a customer master key (CMK). The <code>Encrypt</code> operation has two primary use cases:</p> <ul> <li> <p>You can encrypt up to 4 kilobytes (4096 bytes) of arbitrary data such as an RSA key, a database password, or other sensitive information.</p> </li> <li> <p>To move encrypted data from one AWS region to another, you can use this operation to encrypt in the new region the plaintext data key that was used to encrypt the data in the original region. This provides you with an encrypted copy of the data key that can be decrypted in the new region and used there to decrypt the encrypted data.</p> </li> </ul> <p>To perform this operation on a CMK in a different AWS account, specify the key ARN or alias ARN in the value of the KeyId parameter.</p> <p>Unless you are moving encrypted data from one region to another, you don't use this operation to encrypt a generated data key within a region. To get data keys that are already encrypted, call the <a>GenerateDataKey</a> or <a>GenerateDataKeyWithoutPlaintext</a> operation. Data keys don't need to be encrypted again by calling <code>Encrypt</code>.</p> <p>To encrypt data locally in your application, use the <a>GenerateDataKey</a> operation to return a plaintext data encryption key and a copy of the key encrypted under the CMK of your choosing.</p>
encrypt :: forall eff. EncryptRequest -> Aff (exception :: EXCEPTION | eff) EncryptResponse
encrypt = Request.request serviceName "encrypt" 


-- | <p>Returns a data encryption key that you can use in your application to encrypt data locally. </p> <p>You must specify the customer master key (CMK) under which to generate the data key. You must also specify the length of the data key using either the <code>KeySpec</code> or <code>NumberOfBytes</code> field. You must specify one field or the other, but not both. For common key lengths (128-bit and 256-bit symmetric keys), we recommend that you use <code>KeySpec</code>. To perform this operation on a CMK in a different AWS account, specify the key ARN or alias ARN in the value of the KeyId parameter.</p> <p>This operation returns a plaintext copy of the data key in the <code>Plaintext</code> field of the response, and an encrypted copy of the data key in the <code>CiphertextBlob</code> field. The data key is encrypted under the CMK specified in the <code>KeyId</code> field of the request. </p> <p>We recommend that you use the following pattern to encrypt data locally in your application:</p> <ol> <li> <p>Use this operation (<code>GenerateDataKey</code>) to get a data encryption key.</p> </li> <li> <p>Use the plaintext data encryption key (returned in the <code>Plaintext</code> field of the response) to encrypt data locally, then erase the plaintext data key from memory.</p> </li> <li> <p>Store the encrypted data key (returned in the <code>CiphertextBlob</code> field of the response) alongside the locally encrypted data.</p> </li> </ol> <p>To decrypt data locally:</p> <ol> <li> <p>Use the <a>Decrypt</a> operation to decrypt the encrypted data key into a plaintext copy of the data key.</p> </li> <li> <p>Use the plaintext data key to decrypt data locally, then erase the plaintext data key from memory.</p> </li> </ol> <p>To return only an encrypted copy of the data key, use <a>GenerateDataKeyWithoutPlaintext</a>. To return a random byte string that is cryptographically secure, use <a>GenerateRandom</a>.</p> <p>If you use the optional <code>EncryptionContext</code> field, you must store at least enough information to be able to reconstruct the full encryption context when you later send the ciphertext to the <a>Decrypt</a> operation. It is a good practice to choose an encryption context that you can reconstruct on the fly to better secure the ciphertext. For more information, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/encryption-context.html">Encryption Context</a> in the <i>AWS Key Management Service Developer Guide</i>.</p>
generateDataKey :: forall eff. GenerateDataKeyRequest -> Aff (exception :: EXCEPTION | eff) GenerateDataKeyResponse
generateDataKey = Request.request serviceName "generateDataKey" 


-- | <p>Returns a data encryption key encrypted under a customer master key (CMK). This operation is identical to <a>GenerateDataKey</a> but returns only the encrypted copy of the data key. </p> <p>To perform this operation on a CMK in a different AWS account, specify the key ARN or alias ARN in the value of the KeyId parameter.</p> <p>This operation is useful in a system that has multiple components with different degrees of trust. For example, consider a system that stores encrypted data in containers. Each container stores the encrypted data and an encrypted copy of the data key. One component of the system, called the <i>control plane</i>, creates new containers. When it creates a new container, it uses this operation (<code>GenerateDataKeyWithoutPlaintext</code>) to get an encrypted data key and then stores it in the container. Later, a different component of the system, called the <i>data plane</i>, puts encrypted data into the containers. To do this, it passes the encrypted data key to the <a>Decrypt</a> operation, then uses the returned plaintext data key to encrypt data, and finally stores the encrypted data in the container. In this system, the control plane never sees the plaintext data key.</p>
generateDataKeyWithoutPlaintext :: forall eff. GenerateDataKeyWithoutPlaintextRequest -> Aff (exception :: EXCEPTION | eff) GenerateDataKeyWithoutPlaintextResponse
generateDataKeyWithoutPlaintext = Request.request serviceName "generateDataKeyWithoutPlaintext" 


-- | <p>Returns a random byte string that is cryptographically secure.</p> <p>For more information about entropy and random number generation, see the <a href="https://d0.awsstatic.com/whitepapers/KMS-Cryptographic-Details.pdf">AWS Key Management Service Cryptographic Details</a> whitepaper.</p>
generateRandom :: forall eff. GenerateRandomRequest -> Aff (exception :: EXCEPTION | eff) GenerateRandomResponse
generateRandom = Request.request serviceName "generateRandom" 


-- | <p>Gets a key policy attached to the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.</p>
getKeyPolicy :: forall eff. GetKeyPolicyRequest -> Aff (exception :: EXCEPTION | eff) GetKeyPolicyResponse
getKeyPolicy = Request.request serviceName "getKeyPolicy" 


-- | <p>Gets a Boolean value that indicates whether automatic rotation of the key material is enabled for the specified customer master key (CMK).</p> <p>To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the KeyId parameter.</p>
getKeyRotationStatus :: forall eff. GetKeyRotationStatusRequest -> Aff (exception :: EXCEPTION | eff) GetKeyRotationStatusResponse
getKeyRotationStatus = Request.request serviceName "getKeyRotationStatus" 


-- | <p>Returns the items you need in order to import key material into AWS KMS from your existing key management infrastructure. For more information about importing key material into AWS KMS, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html">Importing Key Material</a> in the <i>AWS Key Management Service Developer Guide</i>.</p> <p>You must specify the key ID of the customer master key (CMK) into which you will import key material. This CMK's <code>Origin</code> must be <code>EXTERNAL</code>. You must also specify the wrapping algorithm and type of wrapping key (public key) that you will use to encrypt the key material. You cannot perform this operation on a CMK in a different AWS account.</p> <p>This operation returns a public key and an import token. Use the public key to encrypt the key material. Store the import token to send with a subsequent <a>ImportKeyMaterial</a> request. The public key and import token from the same response must be used together. These items are valid for 24 hours. When they expire, they cannot be used for a subsequent <a>ImportKeyMaterial</a> request. To get new ones, send another <code>GetParametersForImport</code> request.</p>
getParametersForImport :: forall eff. GetParametersForImportRequest -> Aff (exception :: EXCEPTION | eff) GetParametersForImportResponse
getParametersForImport = Request.request serviceName "getParametersForImport" 


-- | <p>Imports key material into an existing AWS KMS customer master key (CMK) that was created without key material. You cannot perform this operation on a CMK in a different AWS account. For more information about creating CMKs with no key material and then importing key material, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html">Importing Key Material</a> in the <i>AWS Key Management Service Developer Guide</i>.</p> <p>Before using this operation, call <a>GetParametersForImport</a>. Its response includes a public key and an import token. Use the public key to encrypt the key material. Then, submit the import token from the same <code>GetParametersForImport</code> response.</p> <p>When calling this operation, you must specify the following values:</p> <ul> <li> <p>The key ID or key ARN of a CMK with no key material. Its <code>Origin</code> must be <code>EXTERNAL</code>.</p> <p>To create a CMK with no key material, call <a>CreateKey</a> and set the value of its <code>Origin</code> parameter to <code>EXTERNAL</code>. To get the <code>Origin</code> of a CMK, call <a>DescribeKey</a>.)</p> </li> <li> <p>The encrypted key material. To get the public key to encrypt the key material, call <a>GetParametersForImport</a>.</p> </li> <li> <p>The import token that <a>GetParametersForImport</a> returned. This token and the public key used to encrypt the key material must have come from the same response.</p> </li> <li> <p>Whether the key material expires and if so, when. If you set an expiration date, you can change it only by reimporting the same key material and specifying a new expiration date. If the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. To use the CMK again, you must reimport the same key material.</p> </li> </ul> <p>When this operation is successful, the CMK's key state changes from <code>PendingImport</code> to <code>Enabled</code>, and you can use the CMK. After you successfully import key material into a CMK, you can reimport the same key material into that CMK, but you cannot import different key material.</p>
importKeyMaterial :: forall eff. ImportKeyMaterialRequest -> Aff (exception :: EXCEPTION | eff) ImportKeyMaterialResponse
importKeyMaterial = Request.request serviceName "importKeyMaterial" 


-- | <p>Gets a list of all aliases in the caller's AWS account and region. You cannot list aliases in other accounts. For more information about aliases, see <a>CreateAlias</a>.</p> <p>The response might include several aliases that do not have a <code>TargetKeyId</code> field because they are not associated with a CMK. These are predefined aliases that are reserved for CMKs managed by AWS services. If an alias is not associated with a CMK, the alias does not count against the <a href="http://docs.aws.amazon.com/kms/latest/developerguide/limits.html#aliases-limit">alias limit</a> for your account.</p>
listAliases :: forall eff. ListAliasesRequest -> Aff (exception :: EXCEPTION | eff) ListAliasesResponse
listAliases = Request.request serviceName "listAliases" 


-- | <p>Gets a list of all grants for the specified customer master key (CMK).</p> <p>To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the KeyId parameter.</p>
listGrants :: forall eff. ListGrantsRequest -> Aff (exception :: EXCEPTION | eff) ListGrantsResponse
listGrants = Request.request serviceName "listGrants" 


-- | <p>Gets the names of the key policies that are attached to a customer master key (CMK). This operation is designed to get policy names that you can use in a <a>GetKeyPolicy</a> operation. However, the only valid policy name is <code>default</code>. You cannot perform this operation on a CMK in a different AWS account.</p>
listKeyPolicies :: forall eff. ListKeyPoliciesRequest -> Aff (exception :: EXCEPTION | eff) ListKeyPoliciesResponse
listKeyPolicies = Request.request serviceName "listKeyPolicies" 


-- | <p>Gets a list of all customer master keys (CMKs) in the caller's AWS account and region.</p>
listKeys :: forall eff. ListKeysRequest -> Aff (exception :: EXCEPTION | eff) ListKeysResponse
listKeys = Request.request serviceName "listKeys" 


-- | <p>Returns a list of all tags for the specified customer master key (CMK).</p> <p>You cannot perform this operation on a CMK in a different AWS account.</p>
listResourceTags :: forall eff. ListResourceTagsRequest -> Aff (exception :: EXCEPTION | eff) ListResourceTagsResponse
listResourceTags = Request.request serviceName "listResourceTags" 


-- | <p>Returns a list of all grants for which the grant's <code>RetiringPrincipal</code> matches the one specified.</p> <p>A typical use is to list all grants that you are able to retire. To retire a grant, use <a>RetireGrant</a>.</p>
listRetirableGrants :: forall eff. ListRetirableGrantsRequest -> Aff (exception :: EXCEPTION | eff) ListGrantsResponse
listRetirableGrants = Request.request serviceName "listRetirableGrants" 


-- | <p>Attaches a key policy to the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.</p> <p>For more information about key policies, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html">Key Policies</a> in the <i>AWS Key Management Service Developer Guide</i>.</p>
putKeyPolicy :: forall eff. PutKeyPolicyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putKeyPolicy = Request.request serviceName "putKeyPolicy" 


-- | <p>Encrypts data on the server side with a new customer master key (CMK) without exposing the plaintext of the data on the client side. The data is first decrypted and then reencrypted. You can also use this operation to change the encryption context of a ciphertext. </p> <p>You can reencrypt data using CMKs in different AWS accounts.</p> <p>Unlike other operations, <code>ReEncrypt</code> is authorized twice, once as <code>ReEncryptFrom</code> on the source CMK and once as <code>ReEncryptTo</code> on the destination CMK. We recommend that you include the <code>"kms:ReEncrypt*"</code> permission in your <a href="http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html">key policies</a> to permit reencryption from or to the CMK. This permission is automatically included in the key policy when you create a CMK through the console, but you must include it manually when you create a CMK programmatically or when you set a key policy with the <a>PutKeyPolicy</a> operation.</p>
reEncrypt :: forall eff. ReEncryptRequest -> Aff (exception :: EXCEPTION | eff) ReEncryptResponse
reEncrypt = Request.request serviceName "reEncrypt" 


-- | <p>Retires a grant. To clean up, you can retire a grant when you're done using it. You should revoke a grant when you intend to actively deny operations that depend on it. The following are permitted to call this API:</p> <ul> <li> <p>The AWS account (root user) under which the grant was created</p> </li> <li> <p>The <code>RetiringPrincipal</code>, if present in the grant</p> </li> <li> <p>The <code>GranteePrincipal</code>, if <code>RetireGrant</code> is an operation specified in the grant</p> </li> </ul> <p>You must identify the grant to retire by its grant token or by a combination of the grant ID and the Amazon Resource Name (ARN) of the customer master key (CMK). A grant token is a unique variable-length base64-encoded string. A grant ID is a 64 character unique identifier of a grant. The <a>CreateGrant</a> operation returns both.</p>
retireGrant :: forall eff. RetireGrantRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
retireGrant = Request.request serviceName "retireGrant" 


-- | <p>Revokes the specified grant for the specified customer master key (CMK). You can revoke a grant to actively deny operations that depend on it.</p> <p>To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the KeyId parameter.</p>
revokeGrant :: forall eff. RevokeGrantRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
revokeGrant = Request.request serviceName "revokeGrant" 


-- | <p>Schedules the deletion of a customer master key (CMK). You may provide a waiting period, specified in days, before deletion occurs. If you do not provide a waiting period, the default period of 30 days is used. When this operation is successful, the state of the CMK changes to <code>PendingDeletion</code>. Before the waiting period ends, you can use <a>CancelKeyDeletion</a> to cancel the deletion of the CMK. After the waiting period ends, AWS KMS deletes the CMK and all AWS KMS data associated with it, including all aliases that refer to it.</p> <p>You cannot perform this operation on a CMK in a different AWS account.</p> <important> <p>Deleting a CMK is a destructive and potentially dangerous operation. When a CMK is deleted, all data that was encrypted under the CMK is rendered unrecoverable. To restrict the use of a CMK without deleting it, use <a>DisableKey</a>.</p> </important> <p>For more information about scheduling a CMK for deletion, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/deleting-keys.html">Deleting Customer Master Keys</a> in the <i>AWS Key Management Service Developer Guide</i>.</p>
scheduleKeyDeletion :: forall eff. ScheduleKeyDeletionRequest -> Aff (exception :: EXCEPTION | eff) ScheduleKeyDeletionResponse
scheduleKeyDeletion = Request.request serviceName "scheduleKeyDeletion" 


-- | <p>Adds or overwrites one or more tags for the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.</p> <p>Each tag consists of a tag key and a tag value. Tag keys and tag values are both required, but tag values can be empty (null) strings.</p> <p>You cannot use the same tag key more than once per CMK. For example, consider a CMK with one tag whose tag key is <code>Purpose</code> and tag value is <code>Test</code>. If you send a <code>TagResource</code> request for this CMK with a tag key of <code>Purpose</code> and a tag value of <code>Prod</code>, it does not create a second tag. Instead, the original tag is overwritten with the new tag value.</p> <p>For information about the rules that apply to tag keys and tag values, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html">User-Defined Tag Restrictions</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
tagResource = Request.request serviceName "tagResource" 


-- | <p>Removes the specified tag or tags from the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.</p> <p>To remove a tag, you specify the tag key for each tag to remove. You do not specify the tag value. To overwrite the tag value for an existing tag, use <a>TagResource</a>.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
untagResource = Request.request serviceName "untagResource" 


-- | <p>Associates an existing alias with a different customer master key (CMK). Each CMK can have multiple aliases, but the aliases must be unique within the account and region. You cannot perform this operation on an alias in a different AWS account.</p> <p>This operation works only on existing aliases. To change the alias of a CMK to a new value, use <a>CreateAlias</a> to create a new alias and <a>DeleteAlias</a> to delete the old alias.</p> <p>Because an alias is not a property of a CMK, you can create, update, and delete the aliases of a CMK without affecting the CMK. Also, aliases do not appear in the response from the <a>DescribeKey</a> operation. To get the aliases of all CMKs in the account, use the <a>ListAliases</a> operation. </p> <p>An alias name can contain only alphanumeric characters, forward slashes (/), underscores (_), and dashes (-). An alias must start with the word <code>alias</code> followed by a forward slash (<code>alias/</code>). The alias name can contain only alphanumeric characters, forward slashes (/), underscores (_), and dashes (-). Alias names cannot begin with <code>aws</code>; that alias name prefix is reserved by Amazon Web Services (AWS).</p>
updateAlias :: forall eff. UpdateAliasRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateAlias = Request.request serviceName "updateAlias" 


-- | <p>Updates the description of a customer master key (CMK). To see the decription of a CMK, use <a>DescribeKey</a>. </p> <p>You cannot perform this operation on a CMK in a different AWS account.</p>
updateKeyDescription :: forall eff. UpdateKeyDescriptionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateKeyDescription = Request.request serviceName "updateKeyDescription" 


newtype AWSAccountIdType = AWSAccountIdType String
derive instance newtypeAWSAccountIdType :: Newtype AWSAccountIdType _
derive instance repGenericAWSAccountIdType :: Generic AWSAccountIdType _
instance showAWSAccountIdType :: Show AWSAccountIdType where
  show = genericShow
instance decodeAWSAccountIdType :: Decode AWSAccountIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAWSAccountIdType :: Encode AWSAccountIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AlgorithmSpec = AlgorithmSpec String
derive instance newtypeAlgorithmSpec :: Newtype AlgorithmSpec _
derive instance repGenericAlgorithmSpec :: Generic AlgorithmSpec _
instance showAlgorithmSpec :: Show AlgorithmSpec where
  show = genericShow
instance decodeAlgorithmSpec :: Decode AlgorithmSpec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlgorithmSpec :: Encode AlgorithmSpec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AliasList = AliasList (Array AliasListEntry)
derive instance newtypeAliasList :: Newtype AliasList _
derive instance repGenericAliasList :: Generic AliasList _
instance showAliasList :: Show AliasList where
  show = genericShow
instance decodeAliasList :: Decode AliasList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasList :: Encode AliasList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about an alias.</p>
newtype AliasListEntry = AliasListEntry 
  { "AliasName" :: NullOrUndefined.NullOrUndefined (AliasNameType)
  , "AliasArn" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "TargetKeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  }
derive instance newtypeAliasListEntry :: Newtype AliasListEntry _
derive instance repGenericAliasListEntry :: Generic AliasListEntry _
instance showAliasListEntry :: Show AliasListEntry where
  show = genericShow
instance decodeAliasListEntry :: Decode AliasListEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasListEntry :: Encode AliasListEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AliasNameType = AliasNameType String
derive instance newtypeAliasNameType :: Newtype AliasNameType _
derive instance repGenericAliasNameType :: Generic AliasNameType _
instance showAliasNameType :: Show AliasNameType where
  show = genericShow
instance decodeAliasNameType :: Decode AliasNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasNameType :: Encode AliasNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because it attempted to create a resource that already exists.</p>
newtype AlreadyExistsException = AlreadyExistsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeAlreadyExistsException :: Newtype AlreadyExistsException _
derive instance repGenericAlreadyExistsException :: Generic AlreadyExistsException _
instance showAlreadyExistsException :: Show AlreadyExistsException where
  show = genericShow
instance decodeAlreadyExistsException :: Decode AlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlreadyExistsException :: Encode AlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArnType = ArnType String
derive instance newtypeArnType :: Newtype ArnType _
derive instance repGenericArnType :: Generic ArnType _
instance showArnType :: Show ArnType where
  show = genericShow
instance decodeArnType :: Decode ArnType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArnType :: Encode ArnType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanType = BooleanType Boolean
derive instance newtypeBooleanType :: Newtype BooleanType _
derive instance repGenericBooleanType :: Generic BooleanType _
instance showBooleanType :: Show BooleanType where
  show = genericShow
instance decodeBooleanType :: Decode BooleanType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanType :: Encode BooleanType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelKeyDeletionRequest = CancelKeyDeletionRequest 
  { "KeyId" :: (KeyIdType)
  }
derive instance newtypeCancelKeyDeletionRequest :: Newtype CancelKeyDeletionRequest _
derive instance repGenericCancelKeyDeletionRequest :: Generic CancelKeyDeletionRequest _
instance showCancelKeyDeletionRequest :: Show CancelKeyDeletionRequest where
  show = genericShow
instance decodeCancelKeyDeletionRequest :: Decode CancelKeyDeletionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelKeyDeletionRequest :: Encode CancelKeyDeletionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelKeyDeletionResponse = CancelKeyDeletionResponse 
  { "KeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  }
derive instance newtypeCancelKeyDeletionResponse :: Newtype CancelKeyDeletionResponse _
derive instance repGenericCancelKeyDeletionResponse :: Generic CancelKeyDeletionResponse _
instance showCancelKeyDeletionResponse :: Show CancelKeyDeletionResponse where
  show = genericShow
instance decodeCancelKeyDeletionResponse :: Decode CancelKeyDeletionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelKeyDeletionResponse :: Encode CancelKeyDeletionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CiphertextType = CiphertextType String
derive instance newtypeCiphertextType :: Newtype CiphertextType _
derive instance repGenericCiphertextType :: Generic CiphertextType _
instance showCiphertextType :: Show CiphertextType where
  show = genericShow
instance decodeCiphertextType :: Decode CiphertextType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCiphertextType :: Encode CiphertextType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateAliasRequest = CreateAliasRequest 
  { "AliasName" :: (AliasNameType)
  , "TargetKeyId" :: (KeyIdType)
  }
derive instance newtypeCreateAliasRequest :: Newtype CreateAliasRequest _
derive instance repGenericCreateAliasRequest :: Generic CreateAliasRequest _
instance showCreateAliasRequest :: Show CreateAliasRequest where
  show = genericShow
instance decodeCreateAliasRequest :: Decode CreateAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAliasRequest :: Encode CreateAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGrantRequest = CreateGrantRequest 
  { "KeyId" :: (KeyIdType)
  , "GranteePrincipal" :: (PrincipalIdType)
  , "RetiringPrincipal" :: NullOrUndefined.NullOrUndefined (PrincipalIdType)
  , "Operations" :: (GrantOperationList)
  , "Constraints" :: NullOrUndefined.NullOrUndefined (GrantConstraints)
  , "GrantTokens" :: NullOrUndefined.NullOrUndefined (GrantTokenList)
  , "Name" :: NullOrUndefined.NullOrUndefined (GrantNameType)
  }
derive instance newtypeCreateGrantRequest :: Newtype CreateGrantRequest _
derive instance repGenericCreateGrantRequest :: Generic CreateGrantRequest _
instance showCreateGrantRequest :: Show CreateGrantRequest where
  show = genericShow
instance decodeCreateGrantRequest :: Decode CreateGrantRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGrantRequest :: Encode CreateGrantRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGrantResponse = CreateGrantResponse 
  { "GrantToken" :: NullOrUndefined.NullOrUndefined (GrantTokenType)
  , "GrantId" :: NullOrUndefined.NullOrUndefined (GrantIdType)
  }
derive instance newtypeCreateGrantResponse :: Newtype CreateGrantResponse _
derive instance repGenericCreateGrantResponse :: Generic CreateGrantResponse _
instance showCreateGrantResponse :: Show CreateGrantResponse where
  show = genericShow
instance decodeCreateGrantResponse :: Decode CreateGrantResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGrantResponse :: Encode CreateGrantResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateKeyRequest = CreateKeyRequest 
  { "Policy" :: NullOrUndefined.NullOrUndefined (PolicyType)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionType)
  , "KeyUsage" :: NullOrUndefined.NullOrUndefined (KeyUsageType)
  , "Origin" :: NullOrUndefined.NullOrUndefined (OriginType)
  , "BypassPolicyLockoutSafetyCheck" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeCreateKeyRequest :: Newtype CreateKeyRequest _
derive instance repGenericCreateKeyRequest :: Generic CreateKeyRequest _
instance showCreateKeyRequest :: Show CreateKeyRequest where
  show = genericShow
instance decodeCreateKeyRequest :: Decode CreateKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateKeyRequest :: Encode CreateKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateKeyResponse = CreateKeyResponse 
  { "KeyMetadata" :: NullOrUndefined.NullOrUndefined (KeyMetadata)
  }
derive instance newtypeCreateKeyResponse :: Newtype CreateKeyResponse _
derive instance repGenericCreateKeyResponse :: Generic CreateKeyResponse _
instance showCreateKeyResponse :: Show CreateKeyResponse where
  show = genericShow
instance decodeCreateKeyResponse :: Decode CreateKeyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateKeyResponse :: Encode CreateKeyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataKeySpec = DataKeySpec String
derive instance newtypeDataKeySpec :: Newtype DataKeySpec _
derive instance repGenericDataKeySpec :: Generic DataKeySpec _
instance showDataKeySpec :: Show DataKeySpec where
  show = genericShow
instance decodeDataKeySpec :: Decode DataKeySpec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataKeySpec :: Encode DataKeySpec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DateType = DateType Number
derive instance newtypeDateType :: Newtype DateType _
derive instance repGenericDateType :: Generic DateType _
instance showDateType :: Show DateType where
  show = genericShow
instance decodeDateType :: Decode DateType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDateType :: Encode DateType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DecryptRequest = DecryptRequest 
  { "CiphertextBlob" :: (CiphertextType)
  , "EncryptionContext" :: NullOrUndefined.NullOrUndefined (EncryptionContextType)
  , "GrantTokens" :: NullOrUndefined.NullOrUndefined (GrantTokenList)
  }
derive instance newtypeDecryptRequest :: Newtype DecryptRequest _
derive instance repGenericDecryptRequest :: Generic DecryptRequest _
instance showDecryptRequest :: Show DecryptRequest where
  show = genericShow
instance decodeDecryptRequest :: Decode DecryptRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDecryptRequest :: Encode DecryptRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DecryptResponse = DecryptResponse 
  { "KeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  , "Plaintext" :: NullOrUndefined.NullOrUndefined (PlaintextType)
  }
derive instance newtypeDecryptResponse :: Newtype DecryptResponse _
derive instance repGenericDecryptResponse :: Generic DecryptResponse _
instance showDecryptResponse :: Show DecryptResponse where
  show = genericShow
instance decodeDecryptResponse :: Decode DecryptResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDecryptResponse :: Encode DecryptResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAliasRequest = DeleteAliasRequest 
  { "AliasName" :: (AliasNameType)
  }
derive instance newtypeDeleteAliasRequest :: Newtype DeleteAliasRequest _
derive instance repGenericDeleteAliasRequest :: Generic DeleteAliasRequest _
instance showDeleteAliasRequest :: Show DeleteAliasRequest where
  show = genericShow
instance decodeDeleteAliasRequest :: Decode DeleteAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAliasRequest :: Encode DeleteAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteImportedKeyMaterialRequest = DeleteImportedKeyMaterialRequest 
  { "KeyId" :: (KeyIdType)
  }
derive instance newtypeDeleteImportedKeyMaterialRequest :: Newtype DeleteImportedKeyMaterialRequest _
derive instance repGenericDeleteImportedKeyMaterialRequest :: Generic DeleteImportedKeyMaterialRequest _
instance showDeleteImportedKeyMaterialRequest :: Show DeleteImportedKeyMaterialRequest where
  show = genericShow
instance decodeDeleteImportedKeyMaterialRequest :: Decode DeleteImportedKeyMaterialRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteImportedKeyMaterialRequest :: Encode DeleteImportedKeyMaterialRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The system timed out while trying to fulfill the request. The request can be retried.</p>
newtype DependencyTimeoutException = DependencyTimeoutException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeDependencyTimeoutException :: Newtype DependencyTimeoutException _
derive instance repGenericDependencyTimeoutException :: Generic DependencyTimeoutException _
instance showDependencyTimeoutException :: Show DependencyTimeoutException where
  show = genericShow
instance decodeDependencyTimeoutException :: Decode DependencyTimeoutException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDependencyTimeoutException :: Encode DependencyTimeoutException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeKeyRequest = DescribeKeyRequest 
  { "KeyId" :: (KeyIdType)
  , "GrantTokens" :: NullOrUndefined.NullOrUndefined (GrantTokenList)
  }
derive instance newtypeDescribeKeyRequest :: Newtype DescribeKeyRequest _
derive instance repGenericDescribeKeyRequest :: Generic DescribeKeyRequest _
instance showDescribeKeyRequest :: Show DescribeKeyRequest where
  show = genericShow
instance decodeDescribeKeyRequest :: Decode DescribeKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeKeyRequest :: Encode DescribeKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeKeyResponse = DescribeKeyResponse 
  { "KeyMetadata" :: NullOrUndefined.NullOrUndefined (KeyMetadata)
  }
derive instance newtypeDescribeKeyResponse :: Newtype DescribeKeyResponse _
derive instance repGenericDescribeKeyResponse :: Generic DescribeKeyResponse _
instance showDescribeKeyResponse :: Show DescribeKeyResponse where
  show = genericShow
instance decodeDescribeKeyResponse :: Decode DescribeKeyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeKeyResponse :: Encode DescribeKeyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescriptionType = DescriptionType String
derive instance newtypeDescriptionType :: Newtype DescriptionType _
derive instance repGenericDescriptionType :: Generic DescriptionType _
instance showDescriptionType :: Show DescriptionType where
  show = genericShow
instance decodeDescriptionType :: Decode DescriptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescriptionType :: Encode DescriptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisableKeyRequest = DisableKeyRequest 
  { "KeyId" :: (KeyIdType)
  }
derive instance newtypeDisableKeyRequest :: Newtype DisableKeyRequest _
derive instance repGenericDisableKeyRequest :: Generic DisableKeyRequest _
instance showDisableKeyRequest :: Show DisableKeyRequest where
  show = genericShow
instance decodeDisableKeyRequest :: Decode DisableKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableKeyRequest :: Encode DisableKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisableKeyRotationRequest = DisableKeyRotationRequest 
  { "KeyId" :: (KeyIdType)
  }
derive instance newtypeDisableKeyRotationRequest :: Newtype DisableKeyRotationRequest _
derive instance repGenericDisableKeyRotationRequest :: Generic DisableKeyRotationRequest _
instance showDisableKeyRotationRequest :: Show DisableKeyRotationRequest where
  show = genericShow
instance decodeDisableKeyRotationRequest :: Decode DisableKeyRotationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableKeyRotationRequest :: Encode DisableKeyRotationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the specified CMK is not enabled.</p>
newtype DisabledException = DisabledException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeDisabledException :: Newtype DisabledException _
derive instance repGenericDisabledException :: Generic DisabledException _
instance showDisabledException :: Show DisabledException where
  show = genericShow
instance decodeDisabledException :: Decode DisabledException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisabledException :: Encode DisabledException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnableKeyRequest = EnableKeyRequest 
  { "KeyId" :: (KeyIdType)
  }
derive instance newtypeEnableKeyRequest :: Newtype EnableKeyRequest _
derive instance repGenericEnableKeyRequest :: Generic EnableKeyRequest _
instance showEnableKeyRequest :: Show EnableKeyRequest where
  show = genericShow
instance decodeEnableKeyRequest :: Decode EnableKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableKeyRequest :: Encode EnableKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnableKeyRotationRequest = EnableKeyRotationRequest 
  { "KeyId" :: (KeyIdType)
  }
derive instance newtypeEnableKeyRotationRequest :: Newtype EnableKeyRotationRequest _
derive instance repGenericEnableKeyRotationRequest :: Generic EnableKeyRotationRequest _
instance showEnableKeyRotationRequest :: Show EnableKeyRotationRequest where
  show = genericShow
instance decodeEnableKeyRotationRequest :: Decode EnableKeyRotationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableKeyRotationRequest :: Encode EnableKeyRotationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EncryptRequest = EncryptRequest 
  { "KeyId" :: (KeyIdType)
  , "Plaintext" :: (PlaintextType)
  , "EncryptionContext" :: NullOrUndefined.NullOrUndefined (EncryptionContextType)
  , "GrantTokens" :: NullOrUndefined.NullOrUndefined (GrantTokenList)
  }
derive instance newtypeEncryptRequest :: Newtype EncryptRequest _
derive instance repGenericEncryptRequest :: Generic EncryptRequest _
instance showEncryptRequest :: Show EncryptRequest where
  show = genericShow
instance decodeEncryptRequest :: Decode EncryptRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptRequest :: Encode EncryptRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EncryptResponse = EncryptResponse 
  { "CiphertextBlob" :: NullOrUndefined.NullOrUndefined (CiphertextType)
  , "KeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  }
derive instance newtypeEncryptResponse :: Newtype EncryptResponse _
derive instance repGenericEncryptResponse :: Generic EncryptResponse _
instance showEncryptResponse :: Show EncryptResponse where
  show = genericShow
instance decodeEncryptResponse :: Decode EncryptResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptResponse :: Encode EncryptResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EncryptionContextKey = EncryptionContextKey String
derive instance newtypeEncryptionContextKey :: Newtype EncryptionContextKey _
derive instance repGenericEncryptionContextKey :: Generic EncryptionContextKey _
instance showEncryptionContextKey :: Show EncryptionContextKey where
  show = genericShow
instance decodeEncryptionContextKey :: Decode EncryptionContextKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionContextKey :: Encode EncryptionContextKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EncryptionContextType = EncryptionContextType (StrMap.StrMap EncryptionContextValue)
derive instance newtypeEncryptionContextType :: Newtype EncryptionContextType _
derive instance repGenericEncryptionContextType :: Generic EncryptionContextType _
instance showEncryptionContextType :: Show EncryptionContextType where
  show = genericShow
instance decodeEncryptionContextType :: Decode EncryptionContextType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionContextType :: Encode EncryptionContextType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EncryptionContextValue = EncryptionContextValue String
derive instance newtypeEncryptionContextValue :: Newtype EncryptionContextValue _
derive instance repGenericEncryptionContextValue :: Generic EncryptionContextValue _
instance showEncryptionContextValue :: Show EncryptionContextValue where
  show = genericShow
instance decodeEncryptionContextValue :: Decode EncryptionContextValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionContextValue :: Encode EncryptionContextValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessageType = ErrorMessageType String
derive instance newtypeErrorMessageType :: Newtype ErrorMessageType _
derive instance repGenericErrorMessageType :: Generic ErrorMessageType _
instance showErrorMessageType :: Show ErrorMessageType where
  show = genericShow
instance decodeErrorMessageType :: Decode ErrorMessageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessageType :: Encode ErrorMessageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExpirationModelType = ExpirationModelType String
derive instance newtypeExpirationModelType :: Newtype ExpirationModelType _
derive instance repGenericExpirationModelType :: Generic ExpirationModelType _
instance showExpirationModelType :: Show ExpirationModelType where
  show = genericShow
instance decodeExpirationModelType :: Decode ExpirationModelType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpirationModelType :: Encode ExpirationModelType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the provided import token is expired. Use <a>GetParametersForImport</a> to get a new import token and public key, use the new public key to encrypt the key material, and then try the request again.</p>
newtype ExpiredImportTokenException = ExpiredImportTokenException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeExpiredImportTokenException :: Newtype ExpiredImportTokenException _
derive instance repGenericExpiredImportTokenException :: Generic ExpiredImportTokenException _
instance showExpiredImportTokenException :: Show ExpiredImportTokenException where
  show = genericShow
instance decodeExpiredImportTokenException :: Decode ExpiredImportTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpiredImportTokenException :: Encode ExpiredImportTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GenerateDataKeyRequest = GenerateDataKeyRequest 
  { "KeyId" :: (KeyIdType)
  , "EncryptionContext" :: NullOrUndefined.NullOrUndefined (EncryptionContextType)
  , "NumberOfBytes" :: NullOrUndefined.NullOrUndefined (NumberOfBytesType)
  , "KeySpec" :: NullOrUndefined.NullOrUndefined (DataKeySpec)
  , "GrantTokens" :: NullOrUndefined.NullOrUndefined (GrantTokenList)
  }
derive instance newtypeGenerateDataKeyRequest :: Newtype GenerateDataKeyRequest _
derive instance repGenericGenerateDataKeyRequest :: Generic GenerateDataKeyRequest _
instance showGenerateDataKeyRequest :: Show GenerateDataKeyRequest where
  show = genericShow
instance decodeGenerateDataKeyRequest :: Decode GenerateDataKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenerateDataKeyRequest :: Encode GenerateDataKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GenerateDataKeyResponse = GenerateDataKeyResponse 
  { "CiphertextBlob" :: NullOrUndefined.NullOrUndefined (CiphertextType)
  , "Plaintext" :: NullOrUndefined.NullOrUndefined (PlaintextType)
  , "KeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  }
derive instance newtypeGenerateDataKeyResponse :: Newtype GenerateDataKeyResponse _
derive instance repGenericGenerateDataKeyResponse :: Generic GenerateDataKeyResponse _
instance showGenerateDataKeyResponse :: Show GenerateDataKeyResponse where
  show = genericShow
instance decodeGenerateDataKeyResponse :: Decode GenerateDataKeyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenerateDataKeyResponse :: Encode GenerateDataKeyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GenerateDataKeyWithoutPlaintextRequest = GenerateDataKeyWithoutPlaintextRequest 
  { "KeyId" :: (KeyIdType)
  , "EncryptionContext" :: NullOrUndefined.NullOrUndefined (EncryptionContextType)
  , "KeySpec" :: NullOrUndefined.NullOrUndefined (DataKeySpec)
  , "NumberOfBytes" :: NullOrUndefined.NullOrUndefined (NumberOfBytesType)
  , "GrantTokens" :: NullOrUndefined.NullOrUndefined (GrantTokenList)
  }
derive instance newtypeGenerateDataKeyWithoutPlaintextRequest :: Newtype GenerateDataKeyWithoutPlaintextRequest _
derive instance repGenericGenerateDataKeyWithoutPlaintextRequest :: Generic GenerateDataKeyWithoutPlaintextRequest _
instance showGenerateDataKeyWithoutPlaintextRequest :: Show GenerateDataKeyWithoutPlaintextRequest where
  show = genericShow
instance decodeGenerateDataKeyWithoutPlaintextRequest :: Decode GenerateDataKeyWithoutPlaintextRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenerateDataKeyWithoutPlaintextRequest :: Encode GenerateDataKeyWithoutPlaintextRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GenerateDataKeyWithoutPlaintextResponse = GenerateDataKeyWithoutPlaintextResponse 
  { "CiphertextBlob" :: NullOrUndefined.NullOrUndefined (CiphertextType)
  , "KeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  }
derive instance newtypeGenerateDataKeyWithoutPlaintextResponse :: Newtype GenerateDataKeyWithoutPlaintextResponse _
derive instance repGenericGenerateDataKeyWithoutPlaintextResponse :: Generic GenerateDataKeyWithoutPlaintextResponse _
instance showGenerateDataKeyWithoutPlaintextResponse :: Show GenerateDataKeyWithoutPlaintextResponse where
  show = genericShow
instance decodeGenerateDataKeyWithoutPlaintextResponse :: Decode GenerateDataKeyWithoutPlaintextResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenerateDataKeyWithoutPlaintextResponse :: Encode GenerateDataKeyWithoutPlaintextResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GenerateRandomRequest = GenerateRandomRequest 
  { "NumberOfBytes" :: NullOrUndefined.NullOrUndefined (NumberOfBytesType)
  }
derive instance newtypeGenerateRandomRequest :: Newtype GenerateRandomRequest _
derive instance repGenericGenerateRandomRequest :: Generic GenerateRandomRequest _
instance showGenerateRandomRequest :: Show GenerateRandomRequest where
  show = genericShow
instance decodeGenerateRandomRequest :: Decode GenerateRandomRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenerateRandomRequest :: Encode GenerateRandomRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GenerateRandomResponse = GenerateRandomResponse 
  { "Plaintext" :: NullOrUndefined.NullOrUndefined (PlaintextType)
  }
derive instance newtypeGenerateRandomResponse :: Newtype GenerateRandomResponse _
derive instance repGenericGenerateRandomResponse :: Generic GenerateRandomResponse _
instance showGenerateRandomResponse :: Show GenerateRandomResponse where
  show = genericShow
instance decodeGenerateRandomResponse :: Decode GenerateRandomResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenerateRandomResponse :: Encode GenerateRandomResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetKeyPolicyRequest = GetKeyPolicyRequest 
  { "KeyId" :: (KeyIdType)
  , "PolicyName" :: (PolicyNameType)
  }
derive instance newtypeGetKeyPolicyRequest :: Newtype GetKeyPolicyRequest _
derive instance repGenericGetKeyPolicyRequest :: Generic GetKeyPolicyRequest _
instance showGetKeyPolicyRequest :: Show GetKeyPolicyRequest where
  show = genericShow
instance decodeGetKeyPolicyRequest :: Decode GetKeyPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetKeyPolicyRequest :: Encode GetKeyPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetKeyPolicyResponse = GetKeyPolicyResponse 
  { "Policy" :: NullOrUndefined.NullOrUndefined (PolicyType)
  }
derive instance newtypeGetKeyPolicyResponse :: Newtype GetKeyPolicyResponse _
derive instance repGenericGetKeyPolicyResponse :: Generic GetKeyPolicyResponse _
instance showGetKeyPolicyResponse :: Show GetKeyPolicyResponse where
  show = genericShow
instance decodeGetKeyPolicyResponse :: Decode GetKeyPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetKeyPolicyResponse :: Encode GetKeyPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetKeyRotationStatusRequest = GetKeyRotationStatusRequest 
  { "KeyId" :: (KeyIdType)
  }
derive instance newtypeGetKeyRotationStatusRequest :: Newtype GetKeyRotationStatusRequest _
derive instance repGenericGetKeyRotationStatusRequest :: Generic GetKeyRotationStatusRequest _
instance showGetKeyRotationStatusRequest :: Show GetKeyRotationStatusRequest where
  show = genericShow
instance decodeGetKeyRotationStatusRequest :: Decode GetKeyRotationStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetKeyRotationStatusRequest :: Encode GetKeyRotationStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetKeyRotationStatusResponse = GetKeyRotationStatusResponse 
  { "KeyRotationEnabled" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeGetKeyRotationStatusResponse :: Newtype GetKeyRotationStatusResponse _
derive instance repGenericGetKeyRotationStatusResponse :: Generic GetKeyRotationStatusResponse _
instance showGetKeyRotationStatusResponse :: Show GetKeyRotationStatusResponse where
  show = genericShow
instance decodeGetKeyRotationStatusResponse :: Decode GetKeyRotationStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetKeyRotationStatusResponse :: Encode GetKeyRotationStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetParametersForImportRequest = GetParametersForImportRequest 
  { "KeyId" :: (KeyIdType)
  , "WrappingAlgorithm" :: (AlgorithmSpec)
  , "WrappingKeySpec" :: (WrappingKeySpec)
  }
derive instance newtypeGetParametersForImportRequest :: Newtype GetParametersForImportRequest _
derive instance repGenericGetParametersForImportRequest :: Generic GetParametersForImportRequest _
instance showGetParametersForImportRequest :: Show GetParametersForImportRequest where
  show = genericShow
instance decodeGetParametersForImportRequest :: Decode GetParametersForImportRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetParametersForImportRequest :: Encode GetParametersForImportRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetParametersForImportResponse = GetParametersForImportResponse 
  { "KeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  , "ImportToken" :: NullOrUndefined.NullOrUndefined (CiphertextType)
  , "PublicKey" :: NullOrUndefined.NullOrUndefined (PlaintextType)
  , "ParametersValidTo" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeGetParametersForImportResponse :: Newtype GetParametersForImportResponse _
derive instance repGenericGetParametersForImportResponse :: Generic GetParametersForImportResponse _
instance showGetParametersForImportResponse :: Show GetParametersForImportResponse where
  show = genericShow
instance decodeGetParametersForImportResponse :: Decode GetParametersForImportResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetParametersForImportResponse :: Encode GetParametersForImportResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure that you can use to allow certain operations in the grant only when the desired encryption context is present. For more information about encryption context, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/encryption-context.html">Encryption Context</a> in the <i>AWS Key Management Service Developer Guide</i>.</p> <p>Grant constraints apply only to operations that accept encryption context as input. For example, the <code> <a>DescribeKey</a> </code> operation does not accept encryption context as input. A grant that allows the <code>DescribeKey</code> operation does so regardless of the grant constraints. In constrast, the <code> <a>Encrypt</a> </code> operation accepts encryption context as input. A grant that allows the <code>Encrypt</code> operation does so only when the encryption context of the <code>Encrypt</code> operation satisfies the grant constraints.</p>
newtype GrantConstraints = GrantConstraints 
  { "EncryptionContextSubset" :: NullOrUndefined.NullOrUndefined (EncryptionContextType)
  , "EncryptionContextEquals" :: NullOrUndefined.NullOrUndefined (EncryptionContextType)
  }
derive instance newtypeGrantConstraints :: Newtype GrantConstraints _
derive instance repGenericGrantConstraints :: Generic GrantConstraints _
instance showGrantConstraints :: Show GrantConstraints where
  show = genericShow
instance decodeGrantConstraints :: Decode GrantConstraints where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantConstraints :: Encode GrantConstraints where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrantIdType = GrantIdType String
derive instance newtypeGrantIdType :: Newtype GrantIdType _
derive instance repGenericGrantIdType :: Generic GrantIdType _
instance showGrantIdType :: Show GrantIdType where
  show = genericShow
instance decodeGrantIdType :: Decode GrantIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantIdType :: Encode GrantIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrantList = GrantList (Array GrantListEntry)
derive instance newtypeGrantList :: Newtype GrantList _
derive instance repGenericGrantList :: Generic GrantList _
instance showGrantList :: Show GrantList where
  show = genericShow
instance decodeGrantList :: Decode GrantList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantList :: Encode GrantList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about an entry in a list of grants.</p>
newtype GrantListEntry = GrantListEntry 
  { "KeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  , "GrantId" :: NullOrUndefined.NullOrUndefined (GrantIdType)
  , "Name" :: NullOrUndefined.NullOrUndefined (GrantNameType)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "GranteePrincipal" :: NullOrUndefined.NullOrUndefined (PrincipalIdType)
  , "RetiringPrincipal" :: NullOrUndefined.NullOrUndefined (PrincipalIdType)
  , "IssuingAccount" :: NullOrUndefined.NullOrUndefined (PrincipalIdType)
  , "Operations" :: NullOrUndefined.NullOrUndefined (GrantOperationList)
  , "Constraints" :: NullOrUndefined.NullOrUndefined (GrantConstraints)
  }
derive instance newtypeGrantListEntry :: Newtype GrantListEntry _
derive instance repGenericGrantListEntry :: Generic GrantListEntry _
instance showGrantListEntry :: Show GrantListEntry where
  show = genericShow
instance decodeGrantListEntry :: Decode GrantListEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantListEntry :: Encode GrantListEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrantNameType = GrantNameType String
derive instance newtypeGrantNameType :: Newtype GrantNameType _
derive instance repGenericGrantNameType :: Generic GrantNameType _
instance showGrantNameType :: Show GrantNameType where
  show = genericShow
instance decodeGrantNameType :: Decode GrantNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantNameType :: Encode GrantNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrantOperation = GrantOperation String
derive instance newtypeGrantOperation :: Newtype GrantOperation _
derive instance repGenericGrantOperation :: Generic GrantOperation _
instance showGrantOperation :: Show GrantOperation where
  show = genericShow
instance decodeGrantOperation :: Decode GrantOperation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantOperation :: Encode GrantOperation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrantOperationList = GrantOperationList (Array GrantOperation)
derive instance newtypeGrantOperationList :: Newtype GrantOperationList _
derive instance repGenericGrantOperationList :: Generic GrantOperationList _
instance showGrantOperationList :: Show GrantOperationList where
  show = genericShow
instance decodeGrantOperationList :: Decode GrantOperationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantOperationList :: Encode GrantOperationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrantTokenList = GrantTokenList (Array GrantTokenType)
derive instance newtypeGrantTokenList :: Newtype GrantTokenList _
derive instance repGenericGrantTokenList :: Generic GrantTokenList _
instance showGrantTokenList :: Show GrantTokenList where
  show = genericShow
instance decodeGrantTokenList :: Decode GrantTokenList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantTokenList :: Encode GrantTokenList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrantTokenType = GrantTokenType String
derive instance newtypeGrantTokenType :: Newtype GrantTokenType _
derive instance repGenericGrantTokenType :: Generic GrantTokenType _
instance showGrantTokenType :: Show GrantTokenType where
  show = genericShow
instance decodeGrantTokenType :: Decode GrantTokenType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantTokenType :: Encode GrantTokenType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportKeyMaterialRequest = ImportKeyMaterialRequest 
  { "KeyId" :: (KeyIdType)
  , "ImportToken" :: (CiphertextType)
  , "EncryptedKeyMaterial" :: (CiphertextType)
  , "ValidTo" :: NullOrUndefined.NullOrUndefined (DateType)
  , "ExpirationModel" :: NullOrUndefined.NullOrUndefined (ExpirationModelType)
  }
derive instance newtypeImportKeyMaterialRequest :: Newtype ImportKeyMaterialRequest _
derive instance repGenericImportKeyMaterialRequest :: Generic ImportKeyMaterialRequest _
instance showImportKeyMaterialRequest :: Show ImportKeyMaterialRequest where
  show = genericShow
instance decodeImportKeyMaterialRequest :: Decode ImportKeyMaterialRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportKeyMaterialRequest :: Encode ImportKeyMaterialRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportKeyMaterialResponse = ImportKeyMaterialResponse Types.NoArguments
derive instance newtypeImportKeyMaterialResponse :: Newtype ImportKeyMaterialResponse _
derive instance repGenericImportKeyMaterialResponse :: Generic ImportKeyMaterialResponse _
instance showImportKeyMaterialResponse :: Show ImportKeyMaterialResponse where
  show = genericShow
instance decodeImportKeyMaterialResponse :: Decode ImportKeyMaterialResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportKeyMaterialResponse :: Encode ImportKeyMaterialResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the provided key material is invalid or is not the same key material that was previously imported into this customer master key (CMK).</p>
newtype IncorrectKeyMaterialException = IncorrectKeyMaterialException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeIncorrectKeyMaterialException :: Newtype IncorrectKeyMaterialException _
derive instance repGenericIncorrectKeyMaterialException :: Generic IncorrectKeyMaterialException _
instance showIncorrectKeyMaterialException :: Show IncorrectKeyMaterialException where
  show = genericShow
instance decodeIncorrectKeyMaterialException :: Decode IncorrectKeyMaterialException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncorrectKeyMaterialException :: Encode IncorrectKeyMaterialException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the specified alias name is not valid.</p>
newtype InvalidAliasNameException = InvalidAliasNameException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidAliasNameException :: Newtype InvalidAliasNameException _
derive instance repGenericInvalidAliasNameException :: Generic InvalidAliasNameException _
instance showInvalidAliasNameException :: Show InvalidAliasNameException where
  show = genericShow
instance decodeInvalidAliasNameException :: Decode InvalidAliasNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidAliasNameException :: Encode InvalidAliasNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because a specified ARN was not valid.</p>
newtype InvalidArnException = InvalidArnException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidArnException :: Newtype InvalidArnException _
derive instance repGenericInvalidArnException :: Generic InvalidArnException _
instance showInvalidArnException :: Show InvalidArnException where
  show = genericShow
instance decodeInvalidArnException :: Decode InvalidArnException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidArnException :: Encode InvalidArnException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the specified ciphertext, or additional authenticated data incorporated into the ciphertext, such as the encryption context, is corrupted, missing, or otherwise invalid.</p>
newtype InvalidCiphertextException = InvalidCiphertextException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidCiphertextException :: Newtype InvalidCiphertextException _
derive instance repGenericInvalidCiphertextException :: Generic InvalidCiphertextException _
instance showInvalidCiphertextException :: Show InvalidCiphertextException where
  show = genericShow
instance decodeInvalidCiphertextException :: Decode InvalidCiphertextException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCiphertextException :: Encode InvalidCiphertextException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the specified <code>GrantId</code> is not valid.</p>
newtype InvalidGrantIdException = InvalidGrantIdException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidGrantIdException :: Newtype InvalidGrantIdException _
derive instance repGenericInvalidGrantIdException :: Generic InvalidGrantIdException _
instance showInvalidGrantIdException :: Show InvalidGrantIdException where
  show = genericShow
instance decodeInvalidGrantIdException :: Decode InvalidGrantIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidGrantIdException :: Encode InvalidGrantIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the specified grant token is not valid.</p>
newtype InvalidGrantTokenException = InvalidGrantTokenException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidGrantTokenException :: Newtype InvalidGrantTokenException _
derive instance repGenericInvalidGrantTokenException :: Generic InvalidGrantTokenException _
instance showInvalidGrantTokenException :: Show InvalidGrantTokenException where
  show = genericShow
instance decodeInvalidGrantTokenException :: Decode InvalidGrantTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidGrantTokenException :: Encode InvalidGrantTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the provided import token is invalid or is associated with a different customer master key (CMK).</p>
newtype InvalidImportTokenException = InvalidImportTokenException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidImportTokenException :: Newtype InvalidImportTokenException _
derive instance repGenericInvalidImportTokenException :: Generic InvalidImportTokenException _
instance showInvalidImportTokenException :: Show InvalidImportTokenException where
  show = genericShow
instance decodeInvalidImportTokenException :: Decode InvalidImportTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidImportTokenException :: Encode InvalidImportTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the specified <code>KeySpec</code> value is not valid.</p>
newtype InvalidKeyUsageException = InvalidKeyUsageException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidKeyUsageException :: Newtype InvalidKeyUsageException _
derive instance repGenericInvalidKeyUsageException :: Generic InvalidKeyUsageException _
instance showInvalidKeyUsageException :: Show InvalidKeyUsageException where
  show = genericShow
instance decodeInvalidKeyUsageException :: Decode InvalidKeyUsageException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidKeyUsageException :: Encode InvalidKeyUsageException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the marker that specifies where pagination should next begin is not valid.</p>
newtype InvalidMarkerException = InvalidMarkerException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidMarkerException :: Newtype InvalidMarkerException _
derive instance repGenericInvalidMarkerException :: Generic InvalidMarkerException _
instance showInvalidMarkerException :: Show InvalidMarkerException where
  show = genericShow
instance decodeInvalidMarkerException :: Decode InvalidMarkerException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidMarkerException :: Encode InvalidMarkerException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because an internal exception occurred. The request can be retried.</p>
newtype KMSInternalException = KMSInternalException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeKMSInternalException :: Newtype KMSInternalException _
derive instance repGenericKMSInternalException :: Generic KMSInternalException _
instance showKMSInternalException :: Show KMSInternalException where
  show = genericShow
instance decodeKMSInternalException :: Decode KMSInternalException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKMSInternalException :: Encode KMSInternalException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the state of the specified resource is not valid for this request.</p> <p>For more information about how key state affects the use of a CMK, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html">How Key State Affects Use of a Customer Master Key</a> in the <i>AWS Key Management Service Developer Guide</i>.</p>
newtype KMSInvalidStateException = KMSInvalidStateException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeKMSInvalidStateException :: Newtype KMSInvalidStateException _
derive instance repGenericKMSInvalidStateException :: Generic KMSInvalidStateException _
instance showKMSInvalidStateException :: Show KMSInvalidStateException where
  show = genericShow
instance decodeKMSInvalidStateException :: Decode KMSInvalidStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKMSInvalidStateException :: Encode KMSInvalidStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyIdType = KeyIdType String
derive instance newtypeKeyIdType :: Newtype KeyIdType _
derive instance repGenericKeyIdType :: Generic KeyIdType _
instance showKeyIdType :: Show KeyIdType where
  show = genericShow
instance decodeKeyIdType :: Decode KeyIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyIdType :: Encode KeyIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyList = KeyList (Array KeyListEntry)
derive instance newtypeKeyList :: Newtype KeyList _
derive instance repGenericKeyList :: Generic KeyList _
instance showKeyList :: Show KeyList where
  show = genericShow
instance decodeKeyList :: Decode KeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyList :: Encode KeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about each entry in the key list.</p>
newtype KeyListEntry = KeyListEntry 
  { "KeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  , "KeyArn" :: NullOrUndefined.NullOrUndefined (ArnType)
  }
derive instance newtypeKeyListEntry :: Newtype KeyListEntry _
derive instance repGenericKeyListEntry :: Generic KeyListEntry _
instance showKeyListEntry :: Show KeyListEntry where
  show = genericShow
instance decodeKeyListEntry :: Decode KeyListEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyListEntry :: Encode KeyListEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyManagerType = KeyManagerType String
derive instance newtypeKeyManagerType :: Newtype KeyManagerType _
derive instance repGenericKeyManagerType :: Generic KeyManagerType _
instance showKeyManagerType :: Show KeyManagerType where
  show = genericShow
instance decodeKeyManagerType :: Decode KeyManagerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyManagerType :: Encode KeyManagerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains metadata about a customer master key (CMK).</p> <p>This data type is used as a response element for the <a>CreateKey</a> and <a>DescribeKey</a> operations.</p>
newtype KeyMetadata = KeyMetadata 
  { "AWSAccountId" :: NullOrUndefined.NullOrUndefined (AWSAccountIdType)
  , "KeyId" :: (KeyIdType)
  , "Arn" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionType)
  , "KeyUsage" :: NullOrUndefined.NullOrUndefined (KeyUsageType)
  , "KeyState" :: NullOrUndefined.NullOrUndefined (KeyState)
  , "DeletionDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "ValidTo" :: NullOrUndefined.NullOrUndefined (DateType)
  , "Origin" :: NullOrUndefined.NullOrUndefined (OriginType)
  , "ExpirationModel" :: NullOrUndefined.NullOrUndefined (ExpirationModelType)
  , "KeyManager" :: NullOrUndefined.NullOrUndefined (KeyManagerType)
  }
derive instance newtypeKeyMetadata :: Newtype KeyMetadata _
derive instance repGenericKeyMetadata :: Generic KeyMetadata _
instance showKeyMetadata :: Show KeyMetadata where
  show = genericShow
instance decodeKeyMetadata :: Decode KeyMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyMetadata :: Encode KeyMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyState = KeyState String
derive instance newtypeKeyState :: Newtype KeyState _
derive instance repGenericKeyState :: Generic KeyState _
instance showKeyState :: Show KeyState where
  show = genericShow
instance decodeKeyState :: Decode KeyState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyState :: Encode KeyState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the specified CMK was not available. The request can be retried.</p>
newtype KeyUnavailableException = KeyUnavailableException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeKeyUnavailableException :: Newtype KeyUnavailableException _
derive instance repGenericKeyUnavailableException :: Generic KeyUnavailableException _
instance showKeyUnavailableException :: Show KeyUnavailableException where
  show = genericShow
instance decodeKeyUnavailableException :: Decode KeyUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyUnavailableException :: Encode KeyUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyUsageType = KeyUsageType String
derive instance newtypeKeyUsageType :: Newtype KeyUsageType _
derive instance repGenericKeyUsageType :: Generic KeyUsageType _
instance showKeyUsageType :: Show KeyUsageType where
  show = genericShow
instance decodeKeyUsageType :: Decode KeyUsageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyUsageType :: Encode KeyUsageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because a limit was exceeded. For more information, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/limits.html">Limits</a> in the <i>AWS Key Management Service Developer Guide</i>.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LimitType = LimitType Int
derive instance newtypeLimitType :: Newtype LimitType _
derive instance repGenericLimitType :: Generic LimitType _
instance showLimitType :: Show LimitType where
  show = genericShow
instance decodeLimitType :: Decode LimitType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitType :: Encode LimitType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAliasesRequest = ListAliasesRequest 
  { "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  }
derive instance newtypeListAliasesRequest :: Newtype ListAliasesRequest _
derive instance repGenericListAliasesRequest :: Generic ListAliasesRequest _
instance showListAliasesRequest :: Show ListAliasesRequest where
  show = genericShow
instance decodeListAliasesRequest :: Decode ListAliasesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAliasesRequest :: Encode ListAliasesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAliasesResponse = ListAliasesResponse 
  { "Aliases" :: NullOrUndefined.NullOrUndefined (AliasList)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  , "Truncated" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeListAliasesResponse :: Newtype ListAliasesResponse _
derive instance repGenericListAliasesResponse :: Generic ListAliasesResponse _
instance showListAliasesResponse :: Show ListAliasesResponse where
  show = genericShow
instance decodeListAliasesResponse :: Decode ListAliasesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAliasesResponse :: Encode ListAliasesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGrantsRequest = ListGrantsRequest 
  { "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  , "KeyId" :: (KeyIdType)
  }
derive instance newtypeListGrantsRequest :: Newtype ListGrantsRequest _
derive instance repGenericListGrantsRequest :: Generic ListGrantsRequest _
instance showListGrantsRequest :: Show ListGrantsRequest where
  show = genericShow
instance decodeListGrantsRequest :: Decode ListGrantsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGrantsRequest :: Encode ListGrantsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGrantsResponse = ListGrantsResponse 
  { "Grants" :: NullOrUndefined.NullOrUndefined (GrantList)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  , "Truncated" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeListGrantsResponse :: Newtype ListGrantsResponse _
derive instance repGenericListGrantsResponse :: Generic ListGrantsResponse _
instance showListGrantsResponse :: Show ListGrantsResponse where
  show = genericShow
instance decodeListGrantsResponse :: Decode ListGrantsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGrantsResponse :: Encode ListGrantsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListKeyPoliciesRequest = ListKeyPoliciesRequest 
  { "KeyId" :: (KeyIdType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  }
derive instance newtypeListKeyPoliciesRequest :: Newtype ListKeyPoliciesRequest _
derive instance repGenericListKeyPoliciesRequest :: Generic ListKeyPoliciesRequest _
instance showListKeyPoliciesRequest :: Show ListKeyPoliciesRequest where
  show = genericShow
instance decodeListKeyPoliciesRequest :: Decode ListKeyPoliciesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListKeyPoliciesRequest :: Encode ListKeyPoliciesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListKeyPoliciesResponse = ListKeyPoliciesResponse 
  { "PolicyNames" :: NullOrUndefined.NullOrUndefined (PolicyNameList)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  , "Truncated" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeListKeyPoliciesResponse :: Newtype ListKeyPoliciesResponse _
derive instance repGenericListKeyPoliciesResponse :: Generic ListKeyPoliciesResponse _
instance showListKeyPoliciesResponse :: Show ListKeyPoliciesResponse where
  show = genericShow
instance decodeListKeyPoliciesResponse :: Decode ListKeyPoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListKeyPoliciesResponse :: Encode ListKeyPoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListKeysRequest = ListKeysRequest 
  { "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  }
derive instance newtypeListKeysRequest :: Newtype ListKeysRequest _
derive instance repGenericListKeysRequest :: Generic ListKeysRequest _
instance showListKeysRequest :: Show ListKeysRequest where
  show = genericShow
instance decodeListKeysRequest :: Decode ListKeysRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListKeysRequest :: Encode ListKeysRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListKeysResponse = ListKeysResponse 
  { "Keys" :: NullOrUndefined.NullOrUndefined (KeyList)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  , "Truncated" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeListKeysResponse :: Newtype ListKeysResponse _
derive instance repGenericListKeysResponse :: Generic ListKeysResponse _
instance showListKeysResponse :: Show ListKeysResponse where
  show = genericShow
instance decodeListKeysResponse :: Decode ListKeysResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListKeysResponse :: Encode ListKeysResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceTagsRequest = ListResourceTagsRequest 
  { "KeyId" :: (KeyIdType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  }
derive instance newtypeListResourceTagsRequest :: Newtype ListResourceTagsRequest _
derive instance repGenericListResourceTagsRequest :: Generic ListResourceTagsRequest _
instance showListResourceTagsRequest :: Show ListResourceTagsRequest where
  show = genericShow
instance decodeListResourceTagsRequest :: Decode ListResourceTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceTagsRequest :: Encode ListResourceTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceTagsResponse = ListResourceTagsResponse 
  { "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  , "Truncated" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeListResourceTagsResponse :: Newtype ListResourceTagsResponse _
derive instance repGenericListResourceTagsResponse :: Generic ListResourceTagsResponse _
instance showListResourceTagsResponse :: Show ListResourceTagsResponse where
  show = genericShow
instance decodeListResourceTagsResponse :: Decode ListResourceTagsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceTagsResponse :: Encode ListResourceTagsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRetirableGrantsRequest = ListRetirableGrantsRequest 
  { "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  , "RetiringPrincipal" :: (PrincipalIdType)
  }
derive instance newtypeListRetirableGrantsRequest :: Newtype ListRetirableGrantsRequest _
derive instance repGenericListRetirableGrantsRequest :: Generic ListRetirableGrantsRequest _
instance showListRetirableGrantsRequest :: Show ListRetirableGrantsRequest where
  show = genericShow
instance decodeListRetirableGrantsRequest :: Decode ListRetirableGrantsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRetirableGrantsRequest :: Encode ListRetirableGrantsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the specified policy is not syntactically or semantically correct.</p>
newtype MalformedPolicyDocumentException = MalformedPolicyDocumentException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeMalformedPolicyDocumentException :: Newtype MalformedPolicyDocumentException _
derive instance repGenericMalformedPolicyDocumentException :: Generic MalformedPolicyDocumentException _
instance showMalformedPolicyDocumentException :: Show MalformedPolicyDocumentException where
  show = genericShow
instance decodeMalformedPolicyDocumentException :: Decode MalformedPolicyDocumentException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMalformedPolicyDocumentException :: Encode MalformedPolicyDocumentException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MarkerType = MarkerType String
derive instance newtypeMarkerType :: Newtype MarkerType _
derive instance repGenericMarkerType :: Generic MarkerType _
instance showMarkerType :: Show MarkerType where
  show = genericShow
instance decodeMarkerType :: Decode MarkerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarkerType :: Encode MarkerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because the specified entity or resource could not be found.</p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NumberOfBytesType = NumberOfBytesType Int
derive instance newtypeNumberOfBytesType :: Newtype NumberOfBytesType _
derive instance repGenericNumberOfBytesType :: Generic NumberOfBytesType _
instance showNumberOfBytesType :: Show NumberOfBytesType where
  show = genericShow
instance decodeNumberOfBytesType :: Decode NumberOfBytesType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNumberOfBytesType :: Encode NumberOfBytesType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OriginType = OriginType String
derive instance newtypeOriginType :: Newtype OriginType _
derive instance repGenericOriginType :: Generic OriginType _
instance showOriginType :: Show OriginType where
  show = genericShow
instance decodeOriginType :: Decode OriginType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOriginType :: Encode OriginType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PendingWindowInDaysType = PendingWindowInDaysType Int
derive instance newtypePendingWindowInDaysType :: Newtype PendingWindowInDaysType _
derive instance repGenericPendingWindowInDaysType :: Generic PendingWindowInDaysType _
instance showPendingWindowInDaysType :: Show PendingWindowInDaysType where
  show = genericShow
instance decodePendingWindowInDaysType :: Decode PendingWindowInDaysType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePendingWindowInDaysType :: Encode PendingWindowInDaysType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlaintextType = PlaintextType String
derive instance newtypePlaintextType :: Newtype PlaintextType _
derive instance repGenericPlaintextType :: Generic PlaintextType _
instance showPlaintextType :: Show PlaintextType where
  show = genericShow
instance decodePlaintextType :: Decode PlaintextType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlaintextType :: Encode PlaintextType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyNameList = PolicyNameList (Array PolicyNameType)
derive instance newtypePolicyNameList :: Newtype PolicyNameList _
derive instance repGenericPolicyNameList :: Generic PolicyNameList _
instance showPolicyNameList :: Show PolicyNameList where
  show = genericShow
instance decodePolicyNameList :: Decode PolicyNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyNameList :: Encode PolicyNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyNameType = PolicyNameType String
derive instance newtypePolicyNameType :: Newtype PolicyNameType _
derive instance repGenericPolicyNameType :: Generic PolicyNameType _
instance showPolicyNameType :: Show PolicyNameType where
  show = genericShow
instance decodePolicyNameType :: Decode PolicyNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyNameType :: Encode PolicyNameType where
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


newtype PrincipalIdType = PrincipalIdType String
derive instance newtypePrincipalIdType :: Newtype PrincipalIdType _
derive instance repGenericPrincipalIdType :: Generic PrincipalIdType _
instance showPrincipalIdType :: Show PrincipalIdType where
  show = genericShow
instance decodePrincipalIdType :: Decode PrincipalIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrincipalIdType :: Encode PrincipalIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutKeyPolicyRequest = PutKeyPolicyRequest 
  { "KeyId" :: (KeyIdType)
  , "PolicyName" :: (PolicyNameType)
  , "Policy" :: (PolicyType)
  , "BypassPolicyLockoutSafetyCheck" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypePutKeyPolicyRequest :: Newtype PutKeyPolicyRequest _
derive instance repGenericPutKeyPolicyRequest :: Generic PutKeyPolicyRequest _
instance showPutKeyPolicyRequest :: Show PutKeyPolicyRequest where
  show = genericShow
instance decodePutKeyPolicyRequest :: Decode PutKeyPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutKeyPolicyRequest :: Encode PutKeyPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReEncryptRequest = ReEncryptRequest 
  { "CiphertextBlob" :: (CiphertextType)
  , "SourceEncryptionContext" :: NullOrUndefined.NullOrUndefined (EncryptionContextType)
  , "DestinationKeyId" :: (KeyIdType)
  , "DestinationEncryptionContext" :: NullOrUndefined.NullOrUndefined (EncryptionContextType)
  , "GrantTokens" :: NullOrUndefined.NullOrUndefined (GrantTokenList)
  }
derive instance newtypeReEncryptRequest :: Newtype ReEncryptRequest _
derive instance repGenericReEncryptRequest :: Generic ReEncryptRequest _
instance showReEncryptRequest :: Show ReEncryptRequest where
  show = genericShow
instance decodeReEncryptRequest :: Decode ReEncryptRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReEncryptRequest :: Encode ReEncryptRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReEncryptResponse = ReEncryptResponse 
  { "CiphertextBlob" :: NullOrUndefined.NullOrUndefined (CiphertextType)
  , "SourceKeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  , "KeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  }
derive instance newtypeReEncryptResponse :: Newtype ReEncryptResponse _
derive instance repGenericReEncryptResponse :: Generic ReEncryptResponse _
instance showReEncryptResponse :: Show ReEncryptResponse where
  show = genericShow
instance decodeReEncryptResponse :: Decode ReEncryptResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReEncryptResponse :: Encode ReEncryptResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RetireGrantRequest = RetireGrantRequest 
  { "GrantToken" :: NullOrUndefined.NullOrUndefined (GrantTokenType)
  , "KeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  , "GrantId" :: NullOrUndefined.NullOrUndefined (GrantIdType)
  }
derive instance newtypeRetireGrantRequest :: Newtype RetireGrantRequest _
derive instance repGenericRetireGrantRequest :: Generic RetireGrantRequest _
instance showRetireGrantRequest :: Show RetireGrantRequest where
  show = genericShow
instance decodeRetireGrantRequest :: Decode RetireGrantRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetireGrantRequest :: Encode RetireGrantRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RevokeGrantRequest = RevokeGrantRequest 
  { "KeyId" :: (KeyIdType)
  , "GrantId" :: (GrantIdType)
  }
derive instance newtypeRevokeGrantRequest :: Newtype RevokeGrantRequest _
derive instance repGenericRevokeGrantRequest :: Generic RevokeGrantRequest _
instance showRevokeGrantRequest :: Show RevokeGrantRequest where
  show = genericShow
instance decodeRevokeGrantRequest :: Decode RevokeGrantRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevokeGrantRequest :: Encode RevokeGrantRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScheduleKeyDeletionRequest = ScheduleKeyDeletionRequest 
  { "KeyId" :: (KeyIdType)
  , "PendingWindowInDays" :: NullOrUndefined.NullOrUndefined (PendingWindowInDaysType)
  }
derive instance newtypeScheduleKeyDeletionRequest :: Newtype ScheduleKeyDeletionRequest _
derive instance repGenericScheduleKeyDeletionRequest :: Generic ScheduleKeyDeletionRequest _
instance showScheduleKeyDeletionRequest :: Show ScheduleKeyDeletionRequest where
  show = genericShow
instance decodeScheduleKeyDeletionRequest :: Decode ScheduleKeyDeletionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleKeyDeletionRequest :: Encode ScheduleKeyDeletionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScheduleKeyDeletionResponse = ScheduleKeyDeletionResponse 
  { "KeyId" :: NullOrUndefined.NullOrUndefined (KeyIdType)
  , "DeletionDate" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeScheduleKeyDeletionResponse :: Newtype ScheduleKeyDeletionResponse _
derive instance repGenericScheduleKeyDeletionResponse :: Generic ScheduleKeyDeletionResponse _
instance showScheduleKeyDeletionResponse :: Show ScheduleKeyDeletionResponse where
  show = genericShow
instance decodeScheduleKeyDeletionResponse :: Decode ScheduleKeyDeletionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleKeyDeletionResponse :: Encode ScheduleKeyDeletionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A key-value pair. A tag consists of a tag key and a tag value. Tag keys and tag values are both required, but tag values can be empty (null) strings.</p> <p>For information about the rules that apply to tag keys and tag values, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html">User-Defined Tag Restrictions</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p>
newtype Tag = Tag 
  { "TagKey" :: (TagKeyType)
  , "TagValue" :: (TagValueType)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because one or more tags are not valid.</p>
newtype TagException = TagException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeTagException :: Newtype TagException _
derive instance repGenericTagException :: Generic TagException _
instance showTagException :: Show TagException where
  show = genericShow
instance decodeTagException :: Decode TagException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagException :: Encode TagException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeyList = TagKeyList (Array TagKeyType)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeyType = TagKeyType String
derive instance newtypeTagKeyType :: Newtype TagKeyType _
derive instance repGenericTagKeyType :: Generic TagKeyType _
instance showTagKeyType :: Show TagKeyType where
  show = genericShow
instance decodeTagKeyType :: Decode TagKeyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyType :: Encode TagKeyType where
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
  { "KeyId" :: (KeyIdType)
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


newtype TagValueType = TagValueType String
derive instance newtypeTagValueType :: Newtype TagValueType _
derive instance repGenericTagValueType :: Generic TagValueType _
instance showTagValueType :: Show TagValueType where
  show = genericShow
instance decodeTagValueType :: Decode TagValueType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValueType :: Encode TagValueType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request was rejected because a specified parameter is not supported or a specified resource is not valid for this operation.</p>
newtype UnsupportedOperationException = UnsupportedOperationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeUnsupportedOperationException :: Newtype UnsupportedOperationException _
derive instance repGenericUnsupportedOperationException :: Generic UnsupportedOperationException _
instance showUnsupportedOperationException :: Show UnsupportedOperationException where
  show = genericShow
instance decodeUnsupportedOperationException :: Decode UnsupportedOperationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedOperationException :: Encode UnsupportedOperationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UntagResourceRequest = UntagResourceRequest 
  { "KeyId" :: (KeyIdType)
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


newtype UpdateAliasRequest = UpdateAliasRequest 
  { "AliasName" :: (AliasNameType)
  , "TargetKeyId" :: (KeyIdType)
  }
derive instance newtypeUpdateAliasRequest :: Newtype UpdateAliasRequest _
derive instance repGenericUpdateAliasRequest :: Generic UpdateAliasRequest _
instance showUpdateAliasRequest :: Show UpdateAliasRequest where
  show = genericShow
instance decodeUpdateAliasRequest :: Decode UpdateAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAliasRequest :: Encode UpdateAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateKeyDescriptionRequest = UpdateKeyDescriptionRequest 
  { "KeyId" :: (KeyIdType)
  , "Description" :: (DescriptionType)
  }
derive instance newtypeUpdateKeyDescriptionRequest :: Newtype UpdateKeyDescriptionRequest _
derive instance repGenericUpdateKeyDescriptionRequest :: Generic UpdateKeyDescriptionRequest _
instance showUpdateKeyDescriptionRequest :: Show UpdateKeyDescriptionRequest where
  show = genericShow
instance decodeUpdateKeyDescriptionRequest :: Decode UpdateKeyDescriptionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateKeyDescriptionRequest :: Encode UpdateKeyDescriptionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WrappingKeySpec = WrappingKeySpec String
derive instance newtypeWrappingKeySpec :: Newtype WrappingKeySpec _
derive instance repGenericWrappingKeySpec :: Generic WrappingKeySpec _
instance showWrappingKeySpec :: Show WrappingKeySpec where
  show = genericShow
instance decodeWrappingKeySpec :: Decode WrappingKeySpec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWrappingKeySpec :: Encode WrappingKeySpec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
