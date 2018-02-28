

-- | <fullname>AWS IoT</fullname> <p>AWS IoT provides secure, bi-directional communication between Internet-connected things (such as sensors, actuators, embedded devices, or smart appliances) and the AWS cloud. You can discover your custom IoT-Data endpoint to communicate with, configure rules for data processing and integration with other services, organize resources associated with each thing (Thing Registry), configure logging, and create and manage policies and credentials to authenticate things.</p> <p>For more information about how AWS IoT works, see the <a href="http://docs.aws.amazon.com/iot/latest/developerguide/aws-iot-how-it-works.html">Developer Guide</a>.</p>
module AWS.Iot where

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

serviceName = "Iot" :: String


-- | <p>Accepts a pending certificate transfer. The default state of the certificate is INACTIVE.</p> <p>To check for pending certificate transfers, call <a>ListCertificates</a> to enumerate your certificates.</p>
acceptCertificateTransfer :: forall eff. AcceptCertificateTransferRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
acceptCertificateTransfer = Request.request serviceName "acceptCertificateTransfer" 


-- | <p>Adds a thing to a thing group.</p>
addThingToThingGroup :: forall eff. AddThingToThingGroupRequest -> Aff (exception :: EXCEPTION | eff) AddThingToThingGroupResponse
addThingToThingGroup = Request.request serviceName "addThingToThingGroup" 


-- | <p>Associates a group with a continuous job. The following criteria must be met: </p> <ul> <li> <p>The job must have been created with the <code>targetSelection</code> field set to "CONTINUOUS".</p> </li> <li> <p>The job status must currently be "IN_PROGRESS".</p> </li> <li> <p>The total number of targets associated with a job must not exceed 100.</p> </li> </ul>
associateTargetsWithJob :: forall eff. AssociateTargetsWithJobRequest -> Aff (exception :: EXCEPTION | eff) AssociateTargetsWithJobResponse
associateTargetsWithJob = Request.request serviceName "associateTargetsWithJob" 


-- | <p>Attaches a policy to the specified target.</p>
attachPolicy :: forall eff. AttachPolicyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
attachPolicy = Request.request serviceName "attachPolicy" 


-- | <p>Attaches the specified policy to the specified principal (certificate or other credential).</p> <p> <b>Note:</b> This API is deprecated. Please use <a>AttachPolicy</a> instead.</p>
attachPrincipalPolicy :: forall eff. AttachPrincipalPolicyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
attachPrincipalPolicy = Request.request serviceName "attachPrincipalPolicy" 


-- | <p>Attaches the specified principal to the specified thing.</p>
attachThingPrincipal :: forall eff. AttachThingPrincipalRequest -> Aff (exception :: EXCEPTION | eff) AttachThingPrincipalResponse
attachThingPrincipal = Request.request serviceName "attachThingPrincipal" 


-- | <p>Cancels a pending transfer for the specified certificate.</p> <p> <b>Note</b> Only the transfer source account can use this operation to cancel a transfer. (Transfer destinations can use <a>RejectCertificateTransfer</a> instead.) After transfer, AWS IoT returns the certificate to the source account in the INACTIVE state. After the destination account has accepted the transfer, the transfer cannot be cancelled.</p> <p>After a certificate transfer is cancelled, the status of the certificate changes from PENDING_TRANSFER to INACTIVE.</p>
cancelCertificateTransfer :: forall eff. CancelCertificateTransferRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
cancelCertificateTransfer = Request.request serviceName "cancelCertificateTransfer" 


-- | <p>Cancels a job.</p>
cancelJob :: forall eff. CancelJobRequest -> Aff (exception :: EXCEPTION | eff) CancelJobResponse
cancelJob = Request.request serviceName "cancelJob" 


-- | <p>Clears the default authorizer.</p>
clearDefaultAuthorizer :: forall eff. ClearDefaultAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) ClearDefaultAuthorizerResponse
clearDefaultAuthorizer = Request.request serviceName "clearDefaultAuthorizer" 


-- | <p>Creates an authorizer.</p>
createAuthorizer :: forall eff. CreateAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) CreateAuthorizerResponse
createAuthorizer = Request.request serviceName "createAuthorizer" 


-- | <p>Creates an X.509 certificate using the specified certificate signing request.</p> <p> <b>Note:</b> The CSR must include a public key that is either an RSA key with a length of at least 2048 bits or an ECC key from NIST P-256 or NIST P-384 curves. </p> <p> <b>Note:</b> Reusing the same certificate signing request (CSR) results in a distinct certificate.</p> <p>You can create multiple certificates in a batch by creating a directory, copying multiple .csr files into that directory, and then specifying that directory on the command line. The following commands show how to create a batch of certificates given a batch of CSRs.</p> <p>Assuming a set of CSRs are located inside of the directory my-csr-directory:</p> <p>On Linux and OS X, the command is:</p> <p>$ ls my-csr-directory/ | xargs -I {} aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/{}</p> <p>This command lists all of the CSRs in my-csr-directory and pipes each CSR file name to the aws iot create-certificate-from-csr AWS CLI command to create a certificate for the corresponding CSR.</p> <p>The aws iot create-certificate-from-csr part of the command can also be run in parallel to speed up the certificate creation process:</p> <p>$ ls my-csr-directory/ | xargs -P 10 -I {} aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/{}</p> <p>On Windows PowerShell, the command to create certificates for all CSRs in my-csr-directory is:</p> <p>&gt; ls -Name my-csr-directory | %{aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/$_}</p> <p>On a Windows command prompt, the command to create certificates for all CSRs in my-csr-directory is:</p> <p>&gt; forfiles /p my-csr-directory /c "cmd /c aws iot create-certificate-from-csr --certificate-signing-request file://@path"</p>
createCertificateFromCsr :: forall eff. CreateCertificateFromCsrRequest -> Aff (exception :: EXCEPTION | eff) CreateCertificateFromCsrResponse
createCertificateFromCsr = Request.request serviceName "createCertificateFromCsr" 


-- | <p>Creates a job.</p>
createJob :: forall eff. CreateJobRequest -> Aff (exception :: EXCEPTION | eff) CreateJobResponse
createJob = Request.request serviceName "createJob" 


-- | <p>Creates a 2048-bit RSA key pair and issues an X.509 certificate using the issued public key.</p> <p> <b>Note</b> This is the only time AWS IoT issues the private key for this certificate, so it is important to keep it in a secure location.</p>
createKeysAndCertificate :: forall eff. CreateKeysAndCertificateRequest -> Aff (exception :: EXCEPTION | eff) CreateKeysAndCertificateResponse
createKeysAndCertificate = Request.request serviceName "createKeysAndCertificate" 


-- | <p>Creates an AWS IoT OTAUpdate on a target group of things or groups.</p>
createOTAUpdate :: forall eff. CreateOTAUpdateRequest -> Aff (exception :: EXCEPTION | eff) CreateOTAUpdateResponse
createOTAUpdate = Request.request serviceName "createOTAUpdate" 


-- | <p>Creates an AWS IoT policy.</p> <p>The created policy is the default version for the policy. This operation creates a policy version with a version identifier of <b>1</b> and sets <b>1</b> as the policy's default version.</p>
createPolicy :: forall eff. CreatePolicyRequest -> Aff (exception :: EXCEPTION | eff) CreatePolicyResponse
createPolicy = Request.request serviceName "createPolicy" 


-- | <p>Creates a new version of the specified AWS IoT policy. To update a policy, create a new policy version. A managed policy can have up to five versions. If the policy has five versions, you must use <a>DeletePolicyVersion</a> to delete an existing version before you create a new one.</p> <p>Optionally, you can set the new version as the policy's default version. The default version is the operative version (that is, the version that is in effect for the certificates to which the policy is attached).</p>
createPolicyVersion :: forall eff. CreatePolicyVersionRequest -> Aff (exception :: EXCEPTION | eff) CreatePolicyVersionResponse
createPolicyVersion = Request.request serviceName "createPolicyVersion" 


-- | <p>Creates a role alias.</p>
createRoleAlias :: forall eff. CreateRoleAliasRequest -> Aff (exception :: EXCEPTION | eff) CreateRoleAliasResponse
createRoleAlias = Request.request serviceName "createRoleAlias" 


-- | <p>Creates a stream for delivering one or more large files in chunks over MQTT. A stream transports data bytes in chunks or blocks packaged as MQTT messages from a source like S3. You can have one or more files associated with a stream. The total size of a file associated with the stream cannot exceed more than 2 MB. The stream will be created with version 0. If a stream is created with the same streamID as a stream that existed and was deleted within last 90 days, we will resurrect that old stream by incrementing the version by 1.</p>
createStream :: forall eff. CreateStreamRequest -> Aff (exception :: EXCEPTION | eff) CreateStreamResponse
createStream = Request.request serviceName "createStream" 


-- | <p>Creates a thing record in the thing registry.</p>
createThing :: forall eff. CreateThingRequest -> Aff (exception :: EXCEPTION | eff) CreateThingResponse
createThing = Request.request serviceName "createThing" 


-- | <p>Create a thing group.</p>
createThingGroup :: forall eff. CreateThingGroupRequest -> Aff (exception :: EXCEPTION | eff) CreateThingGroupResponse
createThingGroup = Request.request serviceName "createThingGroup" 


-- | <p>Creates a new thing type.</p>
createThingType :: forall eff. CreateThingTypeRequest -> Aff (exception :: EXCEPTION | eff) CreateThingTypeResponse
createThingType = Request.request serviceName "createThingType" 


-- | <p>Creates a rule. Creating rules is an administrator-level action. Any user who has permission to create rules will be able to access data processed by the rule.</p>
createTopicRule :: forall eff. CreateTopicRuleRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
createTopicRule = Request.request serviceName "createTopicRule" 


-- | <p>Deletes an authorizer.</p>
deleteAuthorizer :: forall eff. DeleteAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) DeleteAuthorizerResponse
deleteAuthorizer = Request.request serviceName "deleteAuthorizer" 


-- | <p>Deletes a registered CA certificate.</p>
deleteCACertificate :: forall eff. DeleteCACertificateRequest -> Aff (exception :: EXCEPTION | eff) DeleteCACertificateResponse
deleteCACertificate = Request.request serviceName "deleteCACertificate" 


-- | <p>Deletes the specified certificate.</p> <p>A certificate cannot be deleted if it has a policy attached to it or if its status is set to ACTIVE. To delete a certificate, first use the <a>DetachPrincipalPolicy</a> API to detach all policies. Next, use the <a>UpdateCertificate</a> API to set the certificate to the INACTIVE status.</p>
deleteCertificate :: forall eff. DeleteCertificateRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteCertificate = Request.request serviceName "deleteCertificate" 


-- | <p>Delete an OTA update.</p>
deleteOTAUpdate :: forall eff. DeleteOTAUpdateRequest -> Aff (exception :: EXCEPTION | eff) DeleteOTAUpdateResponse
deleteOTAUpdate = Request.request serviceName "deleteOTAUpdate" 


-- | <p>Deletes the specified policy.</p> <p>A policy cannot be deleted if it has non-default versions or it is attached to any certificate.</p> <p>To delete a policy, use the DeletePolicyVersion API to delete all non-default versions of the policy; use the DetachPrincipalPolicy API to detach the policy from any certificate; and then use the DeletePolicy API to delete the policy.</p> <p>When a policy is deleted using DeletePolicy, its default version is deleted with it.</p>
deletePolicy :: forall eff. DeletePolicyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deletePolicy = Request.request serviceName "deletePolicy" 


-- | <p>Deletes the specified version of the specified policy. You cannot delete the default version of a policy using this API. To delete the default version of a policy, use <a>DeletePolicy</a>. To find out which version of a policy is marked as the default version, use ListPolicyVersions.</p>
deletePolicyVersion :: forall eff. DeletePolicyVersionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deletePolicyVersion = Request.request serviceName "deletePolicyVersion" 


-- | <p>Deletes a CA certificate registration code.</p>
deleteRegistrationCode :: forall eff. DeleteRegistrationCodeRequest -> Aff (exception :: EXCEPTION | eff) DeleteRegistrationCodeResponse
deleteRegistrationCode = Request.request serviceName "deleteRegistrationCode" 


-- | <p>Deletes a role alias</p>
deleteRoleAlias :: forall eff. DeleteRoleAliasRequest -> Aff (exception :: EXCEPTION | eff) DeleteRoleAliasResponse
deleteRoleAlias = Request.request serviceName "deleteRoleAlias" 


-- | <p>Deletes a stream.</p>
deleteStream :: forall eff. DeleteStreamRequest -> Aff (exception :: EXCEPTION | eff) DeleteStreamResponse
deleteStream = Request.request serviceName "deleteStream" 


-- | <p>Deletes the specified thing.</p>
deleteThing :: forall eff. DeleteThingRequest -> Aff (exception :: EXCEPTION | eff) DeleteThingResponse
deleteThing = Request.request serviceName "deleteThing" 


-- | <p>Deletes a thing group.</p>
deleteThingGroup :: forall eff. DeleteThingGroupRequest -> Aff (exception :: EXCEPTION | eff) DeleteThingGroupResponse
deleteThingGroup = Request.request serviceName "deleteThingGroup" 


-- | <p>Deletes the specified thing type . You cannot delete a thing type if it has things associated with it. To delete a thing type, first mark it as deprecated by calling <a>DeprecateThingType</a>, then remove any associated things by calling <a>UpdateThing</a> to change the thing type on any associated thing, and finally use <a>DeleteThingType</a> to delete the thing type.</p>
deleteThingType :: forall eff. DeleteThingTypeRequest -> Aff (exception :: EXCEPTION | eff) DeleteThingTypeResponse
deleteThingType = Request.request serviceName "deleteThingType" 


-- | <p>Deletes the rule.</p>
deleteTopicRule :: forall eff. DeleteTopicRuleRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteTopicRule = Request.request serviceName "deleteTopicRule" 


-- | <p>Deletes a logging level.</p>
deleteV2LoggingLevel :: forall eff. DeleteV2LoggingLevelRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteV2LoggingLevel = Request.request serviceName "deleteV2LoggingLevel" 


-- | <p>Deprecates a thing type. You can not associate new things with deprecated thing type.</p>
deprecateThingType :: forall eff. DeprecateThingTypeRequest -> Aff (exception :: EXCEPTION | eff) DeprecateThingTypeResponse
deprecateThingType = Request.request serviceName "deprecateThingType" 


-- | <p>Describes an authorizer.</p>
describeAuthorizer :: forall eff. DescribeAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) DescribeAuthorizerResponse
describeAuthorizer = Request.request serviceName "describeAuthorizer" 


-- | <p>Describes a registered CA certificate.</p>
describeCACertificate :: forall eff. DescribeCACertificateRequest -> Aff (exception :: EXCEPTION | eff) DescribeCACertificateResponse
describeCACertificate = Request.request serviceName "describeCACertificate" 


-- | <p>Gets information about the specified certificate.</p>
describeCertificate :: forall eff. DescribeCertificateRequest -> Aff (exception :: EXCEPTION | eff) DescribeCertificateResponse
describeCertificate = Request.request serviceName "describeCertificate" 


-- | <p>Describes the default authorizer.</p>
describeDefaultAuthorizer :: forall eff. DescribeDefaultAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) DescribeDefaultAuthorizerResponse
describeDefaultAuthorizer = Request.request serviceName "describeDefaultAuthorizer" 


-- | <p>Returns a unique endpoint specific to the AWS account making the call.</p>
describeEndpoint :: forall eff. DescribeEndpointRequest -> Aff (exception :: EXCEPTION | eff) DescribeEndpointResponse
describeEndpoint = Request.request serviceName "describeEndpoint" 


-- | <p>Describes event configurations.</p>
describeEventConfigurations :: forall eff. DescribeEventConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) DescribeEventConfigurationsResponse
describeEventConfigurations = Request.request serviceName "describeEventConfigurations" 


-- | <p>Describes a search index.</p>
describeIndex :: forall eff. DescribeIndexRequest -> Aff (exception :: EXCEPTION | eff) DescribeIndexResponse
describeIndex = Request.request serviceName "describeIndex" 


-- | <p>Describes a job.</p>
describeJob :: forall eff. DescribeJobRequest -> Aff (exception :: EXCEPTION | eff) DescribeJobResponse
describeJob = Request.request serviceName "describeJob" 


-- | <p>Describes a job execution.</p>
describeJobExecution :: forall eff. DescribeJobExecutionRequest -> Aff (exception :: EXCEPTION | eff) DescribeJobExecutionResponse
describeJobExecution = Request.request serviceName "describeJobExecution" 


-- | <p>Describes a role alias.</p>
describeRoleAlias :: forall eff. DescribeRoleAliasRequest -> Aff (exception :: EXCEPTION | eff) DescribeRoleAliasResponse
describeRoleAlias = Request.request serviceName "describeRoleAlias" 


-- | <p>Gets information about a stream.</p>
describeStream :: forall eff. DescribeStreamRequest -> Aff (exception :: EXCEPTION | eff) DescribeStreamResponse
describeStream = Request.request serviceName "describeStream" 


-- | <p>Gets information about the specified thing.</p>
describeThing :: forall eff. DescribeThingRequest -> Aff (exception :: EXCEPTION | eff) DescribeThingResponse
describeThing = Request.request serviceName "describeThing" 


-- | <p>Describe a thing group.</p>
describeThingGroup :: forall eff. DescribeThingGroupRequest -> Aff (exception :: EXCEPTION | eff) DescribeThingGroupResponse
describeThingGroup = Request.request serviceName "describeThingGroup" 


-- | <p>Describes a bulk thing provisioning task.</p>
describeThingRegistrationTask :: forall eff. DescribeThingRegistrationTaskRequest -> Aff (exception :: EXCEPTION | eff) DescribeThingRegistrationTaskResponse
describeThingRegistrationTask = Request.request serviceName "describeThingRegistrationTask" 


-- | <p>Gets information about the specified thing type.</p>
describeThingType :: forall eff. DescribeThingTypeRequest -> Aff (exception :: EXCEPTION | eff) DescribeThingTypeResponse
describeThingType = Request.request serviceName "describeThingType" 


-- | <p>Detaches a policy from the specified target.</p>
detachPolicy :: forall eff. DetachPolicyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
detachPolicy = Request.request serviceName "detachPolicy" 


-- | <p>Removes the specified policy from the specified certificate.</p> <p> <b>Note:</b> This API is deprecated. Please use <a>DetachPolicy</a> instead.</p>
detachPrincipalPolicy :: forall eff. DetachPrincipalPolicyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
detachPrincipalPolicy = Request.request serviceName "detachPrincipalPolicy" 


-- | <p>Detaches the specified principal from the specified thing.</p>
detachThingPrincipal :: forall eff. DetachThingPrincipalRequest -> Aff (exception :: EXCEPTION | eff) DetachThingPrincipalResponse
detachThingPrincipal = Request.request serviceName "detachThingPrincipal" 


-- | <p>Disables the rule.</p>
disableTopicRule :: forall eff. DisableTopicRuleRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
disableTopicRule = Request.request serviceName "disableTopicRule" 


-- | <p>Enables the rule.</p>
enableTopicRule :: forall eff. EnableTopicRuleRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
enableTopicRule = Request.request serviceName "enableTopicRule" 


-- | <p>Gets effective policies.</p>
getEffectivePolicies :: forall eff. GetEffectivePoliciesRequest -> Aff (exception :: EXCEPTION | eff) GetEffectivePoliciesResponse
getEffectivePolicies = Request.request serviceName "getEffectivePolicies" 


-- | <p>Gets the search configuration.</p>
getIndexingConfiguration :: forall eff. GetIndexingConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetIndexingConfigurationResponse
getIndexingConfiguration = Request.request serviceName "getIndexingConfiguration" 


-- | <p>Gets a job document.</p>
getJobDocument :: forall eff. GetJobDocumentRequest -> Aff (exception :: EXCEPTION | eff) GetJobDocumentResponse
getJobDocument = Request.request serviceName "getJobDocument" 


-- | <p>Gets the logging options.</p>
getLoggingOptions :: forall eff. GetLoggingOptionsRequest -> Aff (exception :: EXCEPTION | eff) GetLoggingOptionsResponse
getLoggingOptions = Request.request serviceName "getLoggingOptions" 


-- | <p>Gets an OTA update.</p>
getOTAUpdate :: forall eff. GetOTAUpdateRequest -> Aff (exception :: EXCEPTION | eff) GetOTAUpdateResponse
getOTAUpdate = Request.request serviceName "getOTAUpdate" 


-- | <p>Gets information about the specified policy with the policy document of the default version.</p>
getPolicy :: forall eff. GetPolicyRequest -> Aff (exception :: EXCEPTION | eff) GetPolicyResponse
getPolicy = Request.request serviceName "getPolicy" 


-- | <p>Gets information about the specified policy version.</p>
getPolicyVersion :: forall eff. GetPolicyVersionRequest -> Aff (exception :: EXCEPTION | eff) GetPolicyVersionResponse
getPolicyVersion = Request.request serviceName "getPolicyVersion" 


-- | <p>Gets a registration code used to register a CA certificate with AWS IoT.</p>
getRegistrationCode :: forall eff. GetRegistrationCodeRequest -> Aff (exception :: EXCEPTION | eff) GetRegistrationCodeResponse
getRegistrationCode = Request.request serviceName "getRegistrationCode" 


-- | <p>Gets information about the rule.</p>
getTopicRule :: forall eff. GetTopicRuleRequest -> Aff (exception :: EXCEPTION | eff) GetTopicRuleResponse
getTopicRule = Request.request serviceName "getTopicRule" 


-- | <p>Gets the fine grained logging options.</p>
getV2LoggingOptions :: forall eff. GetV2LoggingOptionsRequest -> Aff (exception :: EXCEPTION | eff) GetV2LoggingOptionsResponse
getV2LoggingOptions = Request.request serviceName "getV2LoggingOptions" 


-- | <p>Lists the policies attached to the specified thing group.</p>
listAttachedPolicies :: forall eff. ListAttachedPoliciesRequest -> Aff (exception :: EXCEPTION | eff) ListAttachedPoliciesResponse
listAttachedPolicies = Request.request serviceName "listAttachedPolicies" 


-- | <p>Lists the authorizers registered in your account.</p>
listAuthorizers :: forall eff. ListAuthorizersRequest -> Aff (exception :: EXCEPTION | eff) ListAuthorizersResponse
listAuthorizers = Request.request serviceName "listAuthorizers" 


-- | <p>Lists the CA certificates registered for your AWS account.</p> <p>The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.</p>
listCACertificates :: forall eff. ListCACertificatesRequest -> Aff (exception :: EXCEPTION | eff) ListCACertificatesResponse
listCACertificates = Request.request serviceName "listCACertificates" 


-- | <p>Lists the certificates registered in your AWS account.</p> <p>The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.</p>
listCertificates :: forall eff. ListCertificatesRequest -> Aff (exception :: EXCEPTION | eff) ListCertificatesResponse
listCertificates = Request.request serviceName "listCertificates" 


-- | <p>List the device certificates signed by the specified CA certificate.</p>
listCertificatesByCA :: forall eff. ListCertificatesByCARequest -> Aff (exception :: EXCEPTION | eff) ListCertificatesByCAResponse
listCertificatesByCA = Request.request serviceName "listCertificatesByCA" 


-- | <p>Lists the search indices.</p>
listIndices :: forall eff. ListIndicesRequest -> Aff (exception :: EXCEPTION | eff) ListIndicesResponse
listIndices = Request.request serviceName "listIndices" 


-- | <p>Lists the job executions for a job.</p>
listJobExecutionsForJob :: forall eff. ListJobExecutionsForJobRequest -> Aff (exception :: EXCEPTION | eff) ListJobExecutionsForJobResponse
listJobExecutionsForJob = Request.request serviceName "listJobExecutionsForJob" 


-- | <p>Lists the job executions for the specified thing.</p>
listJobExecutionsForThing :: forall eff. ListJobExecutionsForThingRequest -> Aff (exception :: EXCEPTION | eff) ListJobExecutionsForThingResponse
listJobExecutionsForThing = Request.request serviceName "listJobExecutionsForThing" 


-- | <p>Lists jobs.</p>
listJobs :: forall eff. ListJobsRequest -> Aff (exception :: EXCEPTION | eff) ListJobsResponse
listJobs = Request.request serviceName "listJobs" 


-- | <p>Lists OTA updates.</p>
listOTAUpdates :: forall eff. ListOTAUpdatesRequest -> Aff (exception :: EXCEPTION | eff) ListOTAUpdatesResponse
listOTAUpdates = Request.request serviceName "listOTAUpdates" 


-- | <p>Lists certificates that are being transferred but not yet accepted.</p>
listOutgoingCertificates :: forall eff. ListOutgoingCertificatesRequest -> Aff (exception :: EXCEPTION | eff) ListOutgoingCertificatesResponse
listOutgoingCertificates = Request.request serviceName "listOutgoingCertificates" 


-- | <p>Lists your policies.</p>
listPolicies :: forall eff. ListPoliciesRequest -> Aff (exception :: EXCEPTION | eff) ListPoliciesResponse
listPolicies = Request.request serviceName "listPolicies" 


-- | <p>Lists the principals associated with the specified policy.</p> <p> <b>Note:</b> This API is deprecated. Please use <a>ListTargetsForPolicy</a> instead.</p>
listPolicyPrincipals :: forall eff. ListPolicyPrincipalsRequest -> Aff (exception :: EXCEPTION | eff) ListPolicyPrincipalsResponse
listPolicyPrincipals = Request.request serviceName "listPolicyPrincipals" 


-- | <p>Lists the versions of the specified policy and identifies the default version.</p>
listPolicyVersions :: forall eff. ListPolicyVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListPolicyVersionsResponse
listPolicyVersions = Request.request serviceName "listPolicyVersions" 


-- | <p>Lists the policies attached to the specified principal. If you use an Cognito identity, the ID must be in <a href="http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetCredentialsForIdentity.html#API_GetCredentialsForIdentity_RequestSyntax">AmazonCognito Identity format</a>.</p> <p> <b>Note:</b> This API is deprecated. Please use <a>ListAttachedPolicies</a> instead.</p>
listPrincipalPolicies :: forall eff. ListPrincipalPoliciesRequest -> Aff (exception :: EXCEPTION | eff) ListPrincipalPoliciesResponse
listPrincipalPolicies = Request.request serviceName "listPrincipalPolicies" 


-- | <p>Lists the things associated with the specified principal.</p>
listPrincipalThings :: forall eff. ListPrincipalThingsRequest -> Aff (exception :: EXCEPTION | eff) ListPrincipalThingsResponse
listPrincipalThings = Request.request serviceName "listPrincipalThings" 


-- | <p>Lists the role aliases registered in your account.</p>
listRoleAliases :: forall eff. ListRoleAliasesRequest -> Aff (exception :: EXCEPTION | eff) ListRoleAliasesResponse
listRoleAliases = Request.request serviceName "listRoleAliases" 


-- | <p>Lists all of the streams in your AWS account.</p>
listStreams :: forall eff. ListStreamsRequest -> Aff (exception :: EXCEPTION | eff) ListStreamsResponse
listStreams = Request.request serviceName "listStreams" 


-- | <p>List targets for the specified policy.</p>
listTargetsForPolicy :: forall eff. ListTargetsForPolicyRequest -> Aff (exception :: EXCEPTION | eff) ListTargetsForPolicyResponse
listTargetsForPolicy = Request.request serviceName "listTargetsForPolicy" 


-- | <p>List the thing groups in your account.</p>
listThingGroups :: forall eff. ListThingGroupsRequest -> Aff (exception :: EXCEPTION | eff) ListThingGroupsResponse
listThingGroups = Request.request serviceName "listThingGroups" 


-- | <p>List the thing groups to which the specified thing belongs.</p>
listThingGroupsForThing :: forall eff. ListThingGroupsForThingRequest -> Aff (exception :: EXCEPTION | eff) ListThingGroupsForThingResponse
listThingGroupsForThing = Request.request serviceName "listThingGroupsForThing" 


-- | <p>Lists the principals associated with the specified thing.</p>
listThingPrincipals :: forall eff. ListThingPrincipalsRequest -> Aff (exception :: EXCEPTION | eff) ListThingPrincipalsResponse
listThingPrincipals = Request.request serviceName "listThingPrincipals" 


-- | <p>Information about the thing registration tasks.</p>
listThingRegistrationTaskReports :: forall eff. ListThingRegistrationTaskReportsRequest -> Aff (exception :: EXCEPTION | eff) ListThingRegistrationTaskReportsResponse
listThingRegistrationTaskReports = Request.request serviceName "listThingRegistrationTaskReports" 


-- | <p>List bulk thing provisioning tasks.</p>
listThingRegistrationTasks :: forall eff. ListThingRegistrationTasksRequest -> Aff (exception :: EXCEPTION | eff) ListThingRegistrationTasksResponse
listThingRegistrationTasks = Request.request serviceName "listThingRegistrationTasks" 


-- | <p>Lists the existing thing types.</p>
listThingTypes :: forall eff. ListThingTypesRequest -> Aff (exception :: EXCEPTION | eff) ListThingTypesResponse
listThingTypes = Request.request serviceName "listThingTypes" 


-- | <p>Lists your things. Use the <b>attributeName</b> and <b>attributeValue</b> parameters to filter your things. For example, calling <code>ListThings</code> with attributeName=Color and attributeValue=Red retrieves all things in the registry that contain an attribute <b>Color</b> with the value <b>Red</b>. </p>
listThings :: forall eff. ListThingsRequest -> Aff (exception :: EXCEPTION | eff) ListThingsResponse
listThings = Request.request serviceName "listThings" 


-- | <p>Lists the things in the specified group.</p>
listThingsInThingGroup :: forall eff. ListThingsInThingGroupRequest -> Aff (exception :: EXCEPTION | eff) ListThingsInThingGroupResponse
listThingsInThingGroup = Request.request serviceName "listThingsInThingGroup" 


-- | <p>Lists the rules for the specific topic.</p>
listTopicRules :: forall eff. ListTopicRulesRequest -> Aff (exception :: EXCEPTION | eff) ListTopicRulesResponse
listTopicRules = Request.request serviceName "listTopicRules" 


-- | <p>Lists logging levels.</p>
listV2LoggingLevels :: forall eff. ListV2LoggingLevelsRequest -> Aff (exception :: EXCEPTION | eff) ListV2LoggingLevelsResponse
listV2LoggingLevels = Request.request serviceName "listV2LoggingLevels" 


-- | <p>Registers a CA certificate with AWS IoT. This CA certificate can then be used to sign device certificates, which can be then registered with AWS IoT. You can register up to 10 CA certificates per AWS account that have the same subject field. This enables you to have up to 10 certificate authorities sign your device certificates. If you have more than one CA certificate registered, make sure you pass the CA certificate when you register your device certificates with the RegisterCertificate API.</p>
registerCACertificate :: forall eff. RegisterCACertificateRequest -> Aff (exception :: EXCEPTION | eff) RegisterCACertificateResponse
registerCACertificate = Request.request serviceName "registerCACertificate" 


-- | <p>Registers a device certificate with AWS IoT. If you have more than one CA certificate that has the same subject field, you must specify the CA certificate that was used to sign the device certificate being registered.</p>
registerCertificate :: forall eff. RegisterCertificateRequest -> Aff (exception :: EXCEPTION | eff) RegisterCertificateResponse
registerCertificate = Request.request serviceName "registerCertificate" 


-- | <p>Provisions a thing.</p>
registerThing :: forall eff. RegisterThingRequest -> Aff (exception :: EXCEPTION | eff) RegisterThingResponse
registerThing = Request.request serviceName "registerThing" 


-- | <p>Rejects a pending certificate transfer. After AWS IoT rejects a certificate transfer, the certificate status changes from <b>PENDING_TRANSFER</b> to <b>INACTIVE</b>.</p> <p>To check for pending certificate transfers, call <a>ListCertificates</a> to enumerate your certificates.</p> <p>This operation can only be called by the transfer destination. After it is called, the certificate will be returned to the source's account in the INACTIVE state.</p>
rejectCertificateTransfer :: forall eff. RejectCertificateTransferRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
rejectCertificateTransfer = Request.request serviceName "rejectCertificateTransfer" 


-- | <p>Remove the specified thing from the specified group.</p>
removeThingFromThingGroup :: forall eff. RemoveThingFromThingGroupRequest -> Aff (exception :: EXCEPTION | eff) RemoveThingFromThingGroupResponse
removeThingFromThingGroup = Request.request serviceName "removeThingFromThingGroup" 


-- | <p>Replaces the rule. You must specify all parameters for the new rule. Creating rules is an administrator-level action. Any user who has permission to create rules will be able to access data processed by the rule.</p>
replaceTopicRule :: forall eff. ReplaceTopicRuleRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
replaceTopicRule = Request.request serviceName "replaceTopicRule" 


-- | <p>The query search index.</p>
searchIndex :: forall eff. SearchIndexRequest -> Aff (exception :: EXCEPTION | eff) SearchIndexResponse
searchIndex = Request.request serviceName "searchIndex" 


-- | <p>Sets the default authorizer. This will be used if a websocket connection is made without specifying an authorizer.</p>
setDefaultAuthorizer :: forall eff. SetDefaultAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) SetDefaultAuthorizerResponse
setDefaultAuthorizer = Request.request serviceName "setDefaultAuthorizer" 


-- | <p>Sets the specified version of the specified policy as the policy's default (operative) version. This action affects all certificates to which the policy is attached. To list the principals the policy is attached to, use the ListPrincipalPolicy API.</p>
setDefaultPolicyVersion :: forall eff. SetDefaultPolicyVersionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setDefaultPolicyVersion = Request.request serviceName "setDefaultPolicyVersion" 


-- | <p>Sets the logging options.</p>
setLoggingOptions :: forall eff. SetLoggingOptionsRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setLoggingOptions = Request.request serviceName "setLoggingOptions" 


-- | <p>Sets the logging level.</p>
setV2LoggingLevel :: forall eff. SetV2LoggingLevelRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setV2LoggingLevel = Request.request serviceName "setV2LoggingLevel" 


-- | <p>Sets the logging options for the V2 logging service.</p>
setV2LoggingOptions :: forall eff. SetV2LoggingOptionsRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setV2LoggingOptions = Request.request serviceName "setV2LoggingOptions" 


-- | <p>Creates a bulk thing provisioning task.</p>
startThingRegistrationTask :: forall eff. StartThingRegistrationTaskRequest -> Aff (exception :: EXCEPTION | eff) StartThingRegistrationTaskResponse
startThingRegistrationTask = Request.request serviceName "startThingRegistrationTask" 


-- | <p>Cancels a bulk thing provisioning task.</p>
stopThingRegistrationTask :: forall eff. StopThingRegistrationTaskRequest -> Aff (exception :: EXCEPTION | eff) StopThingRegistrationTaskResponse
stopThingRegistrationTask = Request.request serviceName "stopThingRegistrationTask" 


-- | <p>Test custom authorization.</p>
testAuthorization :: forall eff. TestAuthorizationRequest -> Aff (exception :: EXCEPTION | eff) TestAuthorizationResponse
testAuthorization = Request.request serviceName "testAuthorization" 


-- | <p>Invoke the specified custom authorizer for testing purposes.</p>
testInvokeAuthorizer :: forall eff. TestInvokeAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) TestInvokeAuthorizerResponse
testInvokeAuthorizer = Request.request serviceName "testInvokeAuthorizer" 


-- | <p>Transfers the specified certificate to the specified AWS account.</p> <p>You can cancel the transfer until it is acknowledged by the recipient.</p> <p>No notification is sent to the transfer destination's account. It is up to the caller to notify the transfer target.</p> <p>The certificate being transferred must not be in the ACTIVE state. You can use the UpdateCertificate API to deactivate it.</p> <p>The certificate must not have any policies attached to it. You can use the DetachPrincipalPolicy API to detach them.</p>
transferCertificate :: forall eff. TransferCertificateRequest -> Aff (exception :: EXCEPTION | eff) TransferCertificateResponse
transferCertificate = Request.request serviceName "transferCertificate" 


-- | <p>Updates an authorizer.</p>
updateAuthorizer :: forall eff. UpdateAuthorizerRequest -> Aff (exception :: EXCEPTION | eff) UpdateAuthorizerResponse
updateAuthorizer = Request.request serviceName "updateAuthorizer" 


-- | <p>Updates a registered CA certificate.</p>
updateCACertificate :: forall eff. UpdateCACertificateRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateCACertificate = Request.request serviceName "updateCACertificate" 


-- | <p>Updates the status of the specified certificate. This operation is idempotent.</p> <p>Moving a certificate from the ACTIVE state (including REVOKED) will not disconnect currently connected devices, but these devices will be unable to reconnect.</p> <p>The ACTIVE state is required to authenticate devices connecting to AWS IoT using a certificate.</p>
updateCertificate :: forall eff. UpdateCertificateRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateCertificate = Request.request serviceName "updateCertificate" 


-- | <p>Updates the event configurations.</p>
updateEventConfigurations :: forall eff. UpdateEventConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) UpdateEventConfigurationsResponse
updateEventConfigurations = Request.request serviceName "updateEventConfigurations" 


-- | <p>Updates the search configuration.</p>
updateIndexingConfiguration :: forall eff. UpdateIndexingConfigurationRequest -> Aff (exception :: EXCEPTION | eff) UpdateIndexingConfigurationResponse
updateIndexingConfiguration = Request.request serviceName "updateIndexingConfiguration" 


-- | <p>Updates a role alias.</p>
updateRoleAlias :: forall eff. UpdateRoleAliasRequest -> Aff (exception :: EXCEPTION | eff) UpdateRoleAliasResponse
updateRoleAlias = Request.request serviceName "updateRoleAlias" 


-- | <p>Updates an existing stream. The stream version will be incremented by one.</p>
updateStream :: forall eff. UpdateStreamRequest -> Aff (exception :: EXCEPTION | eff) UpdateStreamResponse
updateStream = Request.request serviceName "updateStream" 


-- | <p>Updates the data for a thing.</p>
updateThing :: forall eff. UpdateThingRequest -> Aff (exception :: EXCEPTION | eff) UpdateThingResponse
updateThing = Request.request serviceName "updateThing" 


-- | <p>Update a thing group.</p>
updateThingGroup :: forall eff. UpdateThingGroupRequest -> Aff (exception :: EXCEPTION | eff) UpdateThingGroupResponse
updateThingGroup = Request.request serviceName "updateThingGroup" 


-- | <p>Updates the groups to which the thing belongs.</p>
updateThingGroupsForThing :: forall eff. UpdateThingGroupsForThingRequest -> Aff (exception :: EXCEPTION | eff) UpdateThingGroupsForThingResponse
updateThingGroupsForThing = Request.request serviceName "updateThingGroupsForThing" 


-- | <p>The input for the AcceptCertificateTransfer operation.</p>
newtype AcceptCertificateTransferRequest = AcceptCertificateTransferRequest 
  { "CertificateId'" :: (CertificateId)
  , "SetAsActive'" :: NullOrUndefined.NullOrUndefined (SetAsActive)
  }
derive instance newtypeAcceptCertificateTransferRequest :: Newtype AcceptCertificateTransferRequest _
derive instance repGenericAcceptCertificateTransferRequest :: Generic AcceptCertificateTransferRequest _
instance showAcceptCertificateTransferRequest :: Show AcceptCertificateTransferRequest where
  show = genericShow
instance decodeAcceptCertificateTransferRequest :: Decode AcceptCertificateTransferRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAcceptCertificateTransferRequest :: Encode AcceptCertificateTransferRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the actions associated with a rule.</p>
newtype Action = Action 
  { "DynamoDB'" :: NullOrUndefined.NullOrUndefined (DynamoDBAction)
  , "DynamoDBv2'" :: NullOrUndefined.NullOrUndefined (DynamoDBv2Action)
  , "Lambda'" :: NullOrUndefined.NullOrUndefined (LambdaAction)
  , "Sns'" :: NullOrUndefined.NullOrUndefined (SnsAction)
  , "Sqs'" :: NullOrUndefined.NullOrUndefined (SqsAction)
  , "Kinesis'" :: NullOrUndefined.NullOrUndefined (KinesisAction)
  , "Republish'" :: NullOrUndefined.NullOrUndefined (RepublishAction)
  , "S3'" :: NullOrUndefined.NullOrUndefined (S3Action)
  , "Firehose'" :: NullOrUndefined.NullOrUndefined (FirehoseAction)
  , "CloudwatchMetric'" :: NullOrUndefined.NullOrUndefined (CloudwatchMetricAction)
  , "CloudwatchAlarm'" :: NullOrUndefined.NullOrUndefined (CloudwatchAlarmAction)
  , "Elasticsearch'" :: NullOrUndefined.NullOrUndefined (ElasticsearchAction)
  , "Salesforce'" :: NullOrUndefined.NullOrUndefined (SalesforceAction)
  }
derive instance newtypeAction :: Newtype Action _
derive instance repGenericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow
instance decodeAction :: Decode Action where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAction :: Encode Action where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionList = ActionList (Array Action)
derive instance newtypeActionList :: Newtype ActionList _
derive instance repGenericActionList :: Generic ActionList _
instance showActionList :: Show ActionList where
  show = genericShow
instance decodeActionList :: Decode ActionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionList :: Encode ActionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionType = ActionType String
derive instance newtypeActionType :: Newtype ActionType _
derive instance repGenericActionType :: Generic ActionType _
instance showActionType :: Show ActionType where
  show = genericShow
instance decodeActionType :: Decode ActionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionType :: Encode ActionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddThingToThingGroupRequest = AddThingToThingGroupRequest 
  { "ThingGroupName'" :: NullOrUndefined.NullOrUndefined (ThingGroupName)
  , "ThingGroupArn'" :: NullOrUndefined.NullOrUndefined (ThingGroupArn)
  , "ThingName'" :: NullOrUndefined.NullOrUndefined (ThingName)
  , "ThingArn'" :: NullOrUndefined.NullOrUndefined (ThingArn)
  }
derive instance newtypeAddThingToThingGroupRequest :: Newtype AddThingToThingGroupRequest _
derive instance repGenericAddThingToThingGroupRequest :: Generic AddThingToThingGroupRequest _
instance showAddThingToThingGroupRequest :: Show AddThingToThingGroupRequest where
  show = genericShow
instance decodeAddThingToThingGroupRequest :: Decode AddThingToThingGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddThingToThingGroupRequest :: Encode AddThingToThingGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddThingToThingGroupResponse = AddThingToThingGroupResponse Types.NoArguments
derive instance newtypeAddThingToThingGroupResponse :: Newtype AddThingToThingGroupResponse _
derive instance repGenericAddThingToThingGroupResponse :: Generic AddThingToThingGroupResponse _
instance showAddThingToThingGroupResponse :: Show AddThingToThingGroupResponse where
  show = genericShow
instance decodeAddThingToThingGroupResponse :: Decode AddThingToThingGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddThingToThingGroupResponse :: Encode AddThingToThingGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdditionalParameterMap = AdditionalParameterMap (StrMap.StrMap Value)
derive instance newtypeAdditionalParameterMap :: Newtype AdditionalParameterMap _
derive instance repGenericAdditionalParameterMap :: Generic AdditionalParameterMap _
instance showAdditionalParameterMap :: Show AdditionalParameterMap where
  show = genericShow
instance decodeAdditionalParameterMap :: Decode AdditionalParameterMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdditionalParameterMap :: Encode AdditionalParameterMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AlarmName = AlarmName String
derive instance newtypeAlarmName :: Newtype AlarmName _
derive instance repGenericAlarmName :: Generic AlarmName _
instance showAlarmName :: Show AlarmName where
  show = genericShow
instance decodeAlarmName :: Decode AlarmName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlarmName :: Encode AlarmName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllowAutoRegistration = AllowAutoRegistration Boolean
derive instance newtypeAllowAutoRegistration :: Newtype AllowAutoRegistration _
derive instance repGenericAllowAutoRegistration :: Generic AllowAutoRegistration _
instance showAllowAutoRegistration :: Show AllowAutoRegistration where
  show = genericShow
instance decodeAllowAutoRegistration :: Decode AllowAutoRegistration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowAutoRegistration :: Encode AllowAutoRegistration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information that allowed the authorization.</p>
newtype Allowed = Allowed 
  { "Policies'" :: NullOrUndefined.NullOrUndefined (Policies)
  }
derive instance newtypeAllowed :: Newtype Allowed _
derive instance repGenericAllowed :: Generic Allowed _
instance showAllowed :: Show Allowed where
  show = genericShow
instance decodeAllowed :: Decode Allowed where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowed :: Encode Allowed where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AscendingOrder = AscendingOrder Boolean
derive instance newtypeAscendingOrder :: Newtype AscendingOrder _
derive instance repGenericAscendingOrder :: Generic AscendingOrder _
instance showAscendingOrder :: Show AscendingOrder where
  show = genericShow
instance decodeAscendingOrder :: Decode AscendingOrder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAscendingOrder :: Encode AscendingOrder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateTargetsWithJobRequest = AssociateTargetsWithJobRequest 
  { "Targets'" :: (JobTargets)
  , "JobId'" :: (JobId)
  , "Comment'" :: NullOrUndefined.NullOrUndefined (Comment)
  }
derive instance newtypeAssociateTargetsWithJobRequest :: Newtype AssociateTargetsWithJobRequest _
derive instance repGenericAssociateTargetsWithJobRequest :: Generic AssociateTargetsWithJobRequest _
instance showAssociateTargetsWithJobRequest :: Show AssociateTargetsWithJobRequest where
  show = genericShow
instance decodeAssociateTargetsWithJobRequest :: Decode AssociateTargetsWithJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateTargetsWithJobRequest :: Encode AssociateTargetsWithJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateTargetsWithJobResponse = AssociateTargetsWithJobResponse 
  { "JobArn'" :: NullOrUndefined.NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined.NullOrUndefined (JobId)
  , "Description'" :: NullOrUndefined.NullOrUndefined (JobDescription)
  }
derive instance newtypeAssociateTargetsWithJobResponse :: Newtype AssociateTargetsWithJobResponse _
derive instance repGenericAssociateTargetsWithJobResponse :: Generic AssociateTargetsWithJobResponse _
instance showAssociateTargetsWithJobResponse :: Show AssociateTargetsWithJobResponse where
  show = genericShow
instance decodeAssociateTargetsWithJobResponse :: Decode AssociateTargetsWithJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateTargetsWithJobResponse :: Encode AssociateTargetsWithJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachPolicyRequest = AttachPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Target'" :: (PolicyTarget)
  }
derive instance newtypeAttachPolicyRequest :: Newtype AttachPolicyRequest _
derive instance repGenericAttachPolicyRequest :: Generic AttachPolicyRequest _
instance showAttachPolicyRequest :: Show AttachPolicyRequest where
  show = genericShow
instance decodeAttachPolicyRequest :: Decode AttachPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachPolicyRequest :: Encode AttachPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the AttachPrincipalPolicy operation.</p>
newtype AttachPrincipalPolicyRequest = AttachPrincipalPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Principal'" :: (Principal)
  }
derive instance newtypeAttachPrincipalPolicyRequest :: Newtype AttachPrincipalPolicyRequest _
derive instance repGenericAttachPrincipalPolicyRequest :: Generic AttachPrincipalPolicyRequest _
instance showAttachPrincipalPolicyRequest :: Show AttachPrincipalPolicyRequest where
  show = genericShow
instance decodeAttachPrincipalPolicyRequest :: Decode AttachPrincipalPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachPrincipalPolicyRequest :: Encode AttachPrincipalPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the AttachThingPrincipal operation.</p>
newtype AttachThingPrincipalRequest = AttachThingPrincipalRequest 
  { "ThingName'" :: (ThingName)
  , "Principal'" :: (Principal)
  }
derive instance newtypeAttachThingPrincipalRequest :: Newtype AttachThingPrincipalRequest _
derive instance repGenericAttachThingPrincipalRequest :: Generic AttachThingPrincipalRequest _
instance showAttachThingPrincipalRequest :: Show AttachThingPrincipalRequest where
  show = genericShow
instance decodeAttachThingPrincipalRequest :: Decode AttachThingPrincipalRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachThingPrincipalRequest :: Encode AttachThingPrincipalRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the AttachThingPrincipal operation.</p>
newtype AttachThingPrincipalResponse = AttachThingPrincipalResponse Types.NoArguments
derive instance newtypeAttachThingPrincipalResponse :: Newtype AttachThingPrincipalResponse _
derive instance repGenericAttachThingPrincipalResponse :: Generic AttachThingPrincipalResponse _
instance showAttachThingPrincipalResponse :: Show AttachThingPrincipalResponse where
  show = genericShow
instance decodeAttachThingPrincipalResponse :: Decode AttachThingPrincipalResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachThingPrincipalResponse :: Encode AttachThingPrincipalResponse where
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


-- | <p>The attribute payload.</p>
newtype AttributePayload = AttributePayload 
  { "Attributes'" :: NullOrUndefined.NullOrUndefined (Attributes)
  , "Merge'" :: NullOrUndefined.NullOrUndefined (Flag)
  }
derive instance newtypeAttributePayload :: Newtype AttributePayload _
derive instance repGenericAttributePayload :: Generic AttributePayload _
instance showAttributePayload :: Show AttributePayload where
  show = genericShow
instance decodeAttributePayload :: Decode AttributePayload where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributePayload :: Encode AttributePayload where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeValue = AttributeValue String
derive instance newtypeAttributeValue :: Newtype AttributeValue _
derive instance repGenericAttributeValue :: Generic AttributeValue _
instance showAttributeValue :: Show AttributeValue where
  show = genericShow
instance decodeAttributeValue :: Decode AttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValue :: Encode AttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Attributes = Attributes (StrMap.StrMap AttributeValue)
derive instance newtypeAttributes :: Newtype Attributes _
derive instance repGenericAttributes :: Generic Attributes _
instance showAttributes :: Show Attributes where
  show = genericShow
instance decodeAttributes :: Decode Attributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributes :: Encode Attributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributesMap = AttributesMap (StrMap.StrMap Value)
derive instance newtypeAttributesMap :: Newtype AttributesMap _
derive instance repGenericAttributesMap :: Generic AttributesMap _
instance showAttributesMap :: Show AttributesMap where
  show = genericShow
instance decodeAttributesMap :: Decode AttributesMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributesMap :: Encode AttributesMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthDecision = AuthDecision String
derive instance newtypeAuthDecision :: Newtype AuthDecision _
derive instance repGenericAuthDecision :: Generic AuthDecision _
instance showAuthDecision :: Show AuthDecision where
  show = genericShow
instance decodeAuthDecision :: Decode AuthDecision where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthDecision :: Encode AuthDecision where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A collection of authorization information.</p>
newtype AuthInfo = AuthInfo 
  { "ActionType'" :: NullOrUndefined.NullOrUndefined (ActionType)
  , "Resources'" :: NullOrUndefined.NullOrUndefined (Resources)
  }
derive instance newtypeAuthInfo :: Newtype AuthInfo _
derive instance repGenericAuthInfo :: Generic AuthInfo _
instance showAuthInfo :: Show AuthInfo where
  show = genericShow
instance decodeAuthInfo :: Decode AuthInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthInfo :: Encode AuthInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthInfos = AuthInfos (Array AuthInfo)
derive instance newtypeAuthInfos :: Newtype AuthInfos _
derive instance repGenericAuthInfos :: Generic AuthInfos _
instance showAuthInfos :: Show AuthInfos where
  show = genericShow
instance decodeAuthInfos :: Decode AuthInfos where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthInfos :: Encode AuthInfos where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The authorizer result.</p>
newtype AuthResult = AuthResult 
  { "AuthInfo'" :: NullOrUndefined.NullOrUndefined (AuthInfo)
  , "Allowed'" :: NullOrUndefined.NullOrUndefined (Allowed)
  , "Denied'" :: NullOrUndefined.NullOrUndefined (Denied)
  , "AuthDecision'" :: NullOrUndefined.NullOrUndefined (AuthDecision)
  , "MissingContextValues'" :: NullOrUndefined.NullOrUndefined (MissingContextValues)
  }
derive instance newtypeAuthResult :: Newtype AuthResult _
derive instance repGenericAuthResult :: Generic AuthResult _
instance showAuthResult :: Show AuthResult where
  show = genericShow
instance decodeAuthResult :: Decode AuthResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthResult :: Encode AuthResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthResults = AuthResults (Array AuthResult)
derive instance newtypeAuthResults :: Newtype AuthResults _
derive instance repGenericAuthResults :: Generic AuthResults _
instance showAuthResults :: Show AuthResults where
  show = genericShow
instance decodeAuthResults :: Decode AuthResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthResults :: Encode AuthResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthorizerArn = AuthorizerArn String
derive instance newtypeAuthorizerArn :: Newtype AuthorizerArn _
derive instance repGenericAuthorizerArn :: Generic AuthorizerArn _
instance showAuthorizerArn :: Show AuthorizerArn where
  show = genericShow
instance decodeAuthorizerArn :: Decode AuthorizerArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizerArn :: Encode AuthorizerArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The authorizer description.</p>
newtype AuthorizerDescription = AuthorizerDescription 
  { "AuthorizerName'" :: NullOrUndefined.NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined.NullOrUndefined (AuthorizerArn)
  , "AuthorizerFunctionArn'" :: NullOrUndefined.NullOrUndefined (AuthorizerFunctionArn)
  , "TokenKeyName'" :: NullOrUndefined.NullOrUndefined (TokenKeyName)
  , "TokenSigningPublicKeys'" :: NullOrUndefined.NullOrUndefined (PublicKeyMap)
  , "Status'" :: NullOrUndefined.NullOrUndefined (AuthorizerStatus)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "LastModifiedDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeAuthorizerDescription :: Newtype AuthorizerDescription _
derive instance repGenericAuthorizerDescription :: Generic AuthorizerDescription _
instance showAuthorizerDescription :: Show AuthorizerDescription where
  show = genericShow
instance decodeAuthorizerDescription :: Decode AuthorizerDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizerDescription :: Encode AuthorizerDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthorizerFunctionArn = AuthorizerFunctionArn String
derive instance newtypeAuthorizerFunctionArn :: Newtype AuthorizerFunctionArn _
derive instance repGenericAuthorizerFunctionArn :: Generic AuthorizerFunctionArn _
instance showAuthorizerFunctionArn :: Show AuthorizerFunctionArn where
  show = genericShow
instance decodeAuthorizerFunctionArn :: Decode AuthorizerFunctionArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizerFunctionArn :: Encode AuthorizerFunctionArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthorizerName = AuthorizerName String
derive instance newtypeAuthorizerName :: Newtype AuthorizerName _
derive instance repGenericAuthorizerName :: Generic AuthorizerName _
instance showAuthorizerName :: Show AuthorizerName where
  show = genericShow
instance decodeAuthorizerName :: Decode AuthorizerName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizerName :: Encode AuthorizerName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthorizerStatus = AuthorizerStatus String
derive instance newtypeAuthorizerStatus :: Newtype AuthorizerStatus _
derive instance repGenericAuthorizerStatus :: Generic AuthorizerStatus _
instance showAuthorizerStatus :: Show AuthorizerStatus where
  show = genericShow
instance decodeAuthorizerStatus :: Decode AuthorizerStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizerStatus :: Encode AuthorizerStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The authorizer summary.</p>
newtype AuthorizerSummary = AuthorizerSummary 
  { "AuthorizerName'" :: NullOrUndefined.NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined.NullOrUndefined (AuthorizerArn)
  }
derive instance newtypeAuthorizerSummary :: Newtype AuthorizerSummary _
derive instance repGenericAuthorizerSummary :: Generic AuthorizerSummary _
instance showAuthorizerSummary :: Show AuthorizerSummary where
  show = genericShow
instance decodeAuthorizerSummary :: Decode AuthorizerSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizerSummary :: Encode AuthorizerSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Authorizers = Authorizers (Array AuthorizerSummary)
derive instance newtypeAuthorizers :: Newtype Authorizers _
derive instance repGenericAuthorizers :: Generic Authorizers _
instance showAuthorizers :: Show Authorizers where
  show = genericShow
instance decodeAuthorizers :: Decode Authorizers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizers :: Encode Authorizers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutoRegistrationStatus = AutoRegistrationStatus String
derive instance newtypeAutoRegistrationStatus :: Newtype AutoRegistrationStatus _
derive instance repGenericAutoRegistrationStatus :: Generic AutoRegistrationStatus _
instance showAutoRegistrationStatus :: Show AutoRegistrationStatus where
  show = genericShow
instance decodeAutoRegistrationStatus :: Decode AutoRegistrationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutoRegistrationStatus :: Encode AutoRegistrationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AwsAccountId = AwsAccountId String
derive instance newtypeAwsAccountId :: Newtype AwsAccountId _
derive instance repGenericAwsAccountId :: Generic AwsAccountId _
instance showAwsAccountId :: Show AwsAccountId where
  show = genericShow
instance decodeAwsAccountId :: Decode AwsAccountId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsAccountId :: Encode AwsAccountId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AwsArn = AwsArn String
derive instance newtypeAwsArn :: Newtype AwsArn _
derive instance repGenericAwsArn :: Generic AwsArn _
instance showAwsArn :: Show AwsArn where
  show = genericShow
instance decodeAwsArn :: Decode AwsArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsArn :: Encode AwsArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AwsIotJobArn = AwsIotJobArn String
derive instance newtypeAwsIotJobArn :: Newtype AwsIotJobArn _
derive instance repGenericAwsIotJobArn :: Generic AwsIotJobArn _
instance showAwsIotJobArn :: Show AwsIotJobArn where
  show = genericShow
instance decodeAwsIotJobArn :: Decode AwsIotJobArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsIotJobArn :: Encode AwsIotJobArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AwsIotJobId = AwsIotJobId String
derive instance newtypeAwsIotJobId :: Newtype AwsIotJobId _
derive instance repGenericAwsIotJobId :: Generic AwsIotJobId _
instance showAwsIotJobId :: Show AwsIotJobId where
  show = genericShow
instance decodeAwsIotJobId :: Decode AwsIotJobId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsIotJobId :: Encode AwsIotJobId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AwsIotSqlVersion = AwsIotSqlVersion String
derive instance newtypeAwsIotSqlVersion :: Newtype AwsIotSqlVersion _
derive instance repGenericAwsIotSqlVersion :: Generic AwsIotSqlVersion _
instance showAwsIotSqlVersion :: Show AwsIotSqlVersion where
  show = genericShow
instance decodeAwsIotSqlVersion :: Decode AwsIotSqlVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsIotSqlVersion :: Encode AwsIotSqlVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketName = BucketName String
derive instance newtypeBucketName :: Newtype BucketName _
derive instance repGenericBucketName :: Generic BucketName _
instance showBucketName :: Show BucketName where
  show = genericShow
instance decodeBucketName :: Decode BucketName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketName :: Encode BucketName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A CA certificate.</p>
newtype CACertificate = CACertificate 
  { "CertificateArn'" :: NullOrUndefined.NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined.NullOrUndefined (CertificateId)
  , "Status'" :: NullOrUndefined.NullOrUndefined (CACertificateStatus)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeCACertificate :: Newtype CACertificate _
derive instance repGenericCACertificate :: Generic CACertificate _
instance showCACertificate :: Show CACertificate where
  show = genericShow
instance decodeCACertificate :: Decode CACertificate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCACertificate :: Encode CACertificate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a CA certificate.</p>
newtype CACertificateDescription = CACertificateDescription 
  { "CertificateArn'" :: NullOrUndefined.NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined.NullOrUndefined (CertificateId)
  , "Status'" :: NullOrUndefined.NullOrUndefined (CACertificateStatus)
  , "CertificatePem'" :: NullOrUndefined.NullOrUndefined (CertificatePem)
  , "OwnedBy'" :: NullOrUndefined.NullOrUndefined (AwsAccountId)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "AutoRegistrationStatus'" :: NullOrUndefined.NullOrUndefined (AutoRegistrationStatus)
  }
derive instance newtypeCACertificateDescription :: Newtype CACertificateDescription _
derive instance repGenericCACertificateDescription :: Generic CACertificateDescription _
instance showCACertificateDescription :: Show CACertificateDescription where
  show = genericShow
instance decodeCACertificateDescription :: Decode CACertificateDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCACertificateDescription :: Encode CACertificateDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CACertificateStatus = CACertificateStatus String
derive instance newtypeCACertificateStatus :: Newtype CACertificateStatus _
derive instance repGenericCACertificateStatus :: Generic CACertificateStatus _
instance showCACertificateStatus :: Show CACertificateStatus where
  show = genericShow
instance decodeCACertificateStatus :: Decode CACertificateStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCACertificateStatus :: Encode CACertificateStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CACertificates = CACertificates (Array CACertificate)
derive instance newtypeCACertificates :: Newtype CACertificates _
derive instance repGenericCACertificates :: Generic CACertificates _
instance showCACertificates :: Show CACertificates where
  show = genericShow
instance decodeCACertificates :: Decode CACertificates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCACertificates :: Encode CACertificates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the CancelCertificateTransfer operation.</p>
newtype CancelCertificateTransferRequest = CancelCertificateTransferRequest 
  { "CertificateId'" :: (CertificateId)
  }
derive instance newtypeCancelCertificateTransferRequest :: Newtype CancelCertificateTransferRequest _
derive instance repGenericCancelCertificateTransferRequest :: Generic CancelCertificateTransferRequest _
instance showCancelCertificateTransferRequest :: Show CancelCertificateTransferRequest where
  show = genericShow
instance decodeCancelCertificateTransferRequest :: Decode CancelCertificateTransferRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelCertificateTransferRequest :: Encode CancelCertificateTransferRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelJobRequest = CancelJobRequest 
  { "JobId'" :: (JobId)
  , "Comment'" :: NullOrUndefined.NullOrUndefined (Comment)
  }
derive instance newtypeCancelJobRequest :: Newtype CancelJobRequest _
derive instance repGenericCancelJobRequest :: Generic CancelJobRequest _
instance showCancelJobRequest :: Show CancelJobRequest where
  show = genericShow
instance decodeCancelJobRequest :: Decode CancelJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelJobRequest :: Encode CancelJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelJobResponse = CancelJobResponse 
  { "JobArn'" :: NullOrUndefined.NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined.NullOrUndefined (JobId)
  , "Description'" :: NullOrUndefined.NullOrUndefined (JobDescription)
  }
derive instance newtypeCancelJobResponse :: Newtype CancelJobResponse _
derive instance repGenericCancelJobResponse :: Generic CancelJobResponse _
instance showCancelJobResponse :: Show CancelJobResponse where
  show = genericShow
instance decodeCancelJobResponse :: Decode CancelJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelJobResponse :: Encode CancelJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CanceledThings = CanceledThings Int
derive instance newtypeCanceledThings :: Newtype CanceledThings _
derive instance repGenericCanceledThings :: Generic CanceledThings _
instance showCanceledThings :: Show CanceledThings where
  show = genericShow
instance decodeCanceledThings :: Decode CanceledThings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCanceledThings :: Encode CanceledThings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CannedAccessControlList = CannedAccessControlList String
derive instance newtypeCannedAccessControlList :: Newtype CannedAccessControlList _
derive instance repGenericCannedAccessControlList :: Generic CannedAccessControlList _
instance showCannedAccessControlList :: Show CannedAccessControlList where
  show = genericShow
instance decodeCannedAccessControlList :: Decode CannedAccessControlList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCannedAccessControlList :: Encode CannedAccessControlList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a certificate.</p>
newtype Certificate = Certificate 
  { "CertificateArn'" :: NullOrUndefined.NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined.NullOrUndefined (CertificateId)
  , "Status'" :: NullOrUndefined.NullOrUndefined (CertificateStatus)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeCertificate :: Newtype Certificate _
derive instance repGenericCertificate :: Generic Certificate _
instance showCertificate :: Show Certificate where
  show = genericShow
instance decodeCertificate :: Decode Certificate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificate :: Encode Certificate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateArn = CertificateArn String
derive instance newtypeCertificateArn :: Newtype CertificateArn _
derive instance repGenericCertificateArn :: Generic CertificateArn _
instance showCertificateArn :: Show CertificateArn where
  show = genericShow
instance decodeCertificateArn :: Decode CertificateArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateArn :: Encode CertificateArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Unable to verify the CA certificate used to sign the device certificate you are attempting to register. This is happens when you have registered more than one CA certificate that has the same subject field and public key.</p>
newtype CertificateConflictException = CertificateConflictException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCertificateConflictException :: Newtype CertificateConflictException _
derive instance repGenericCertificateConflictException :: Generic CertificateConflictException _
instance showCertificateConflictException :: Show CertificateConflictException where
  show = genericShow
instance decodeCertificateConflictException :: Decode CertificateConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateConflictException :: Encode CertificateConflictException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a certificate.</p>
newtype CertificateDescription = CertificateDescription 
  { "CertificateArn'" :: NullOrUndefined.NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined.NullOrUndefined (CertificateId)
  , "CaCertificateId'" :: NullOrUndefined.NullOrUndefined (CertificateId)
  , "Status'" :: NullOrUndefined.NullOrUndefined (CertificateStatus)
  , "CertificatePem'" :: NullOrUndefined.NullOrUndefined (CertificatePem)
  , "OwnedBy'" :: NullOrUndefined.NullOrUndefined (AwsAccountId)
  , "PreviousOwnedBy'" :: NullOrUndefined.NullOrUndefined (AwsAccountId)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "LastModifiedDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "TransferData'" :: NullOrUndefined.NullOrUndefined (TransferData)
  }
derive instance newtypeCertificateDescription :: Newtype CertificateDescription _
derive instance repGenericCertificateDescription :: Generic CertificateDescription _
instance showCertificateDescription :: Show CertificateDescription where
  show = genericShow
instance decodeCertificateDescription :: Decode CertificateDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateDescription :: Encode CertificateDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateId = CertificateId String
derive instance newtypeCertificateId :: Newtype CertificateId _
derive instance repGenericCertificateId :: Generic CertificateId _
instance showCertificateId :: Show CertificateId where
  show = genericShow
instance decodeCertificateId :: Decode CertificateId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateId :: Encode CertificateId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateName = CertificateName String
derive instance newtypeCertificateName :: Newtype CertificateName _
derive instance repGenericCertificateName :: Generic CertificateName _
instance showCertificateName :: Show CertificateName where
  show = genericShow
instance decodeCertificateName :: Decode CertificateName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateName :: Encode CertificateName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The PEM of a certificate.</p>
newtype CertificatePem = CertificatePem String
derive instance newtypeCertificatePem :: Newtype CertificatePem _
derive instance repGenericCertificatePem :: Generic CertificatePem _
instance showCertificatePem :: Show CertificatePem where
  show = genericShow
instance decodeCertificatePem :: Decode CertificatePem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificatePem :: Encode CertificatePem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateSigningRequest = CertificateSigningRequest String
derive instance newtypeCertificateSigningRequest :: Newtype CertificateSigningRequest _
derive instance repGenericCertificateSigningRequest :: Generic CertificateSigningRequest _
instance showCertificateSigningRequest :: Show CertificateSigningRequest where
  show = genericShow
instance decodeCertificateSigningRequest :: Decode CertificateSigningRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateSigningRequest :: Encode CertificateSigningRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The certificate operation is not allowed.</p>
newtype CertificateStateException = CertificateStateException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCertificateStateException :: Newtype CertificateStateException _
derive instance repGenericCertificateStateException :: Generic CertificateStateException _
instance showCertificateStateException :: Show CertificateStateException where
  show = genericShow
instance decodeCertificateStateException :: Decode CertificateStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateStateException :: Encode CertificateStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateStatus = CertificateStatus String
derive instance newtypeCertificateStatus :: Newtype CertificateStatus _
derive instance repGenericCertificateStatus :: Generic CertificateStatus _
instance showCertificateStatus :: Show CertificateStatus where
  show = genericShow
instance decodeCertificateStatus :: Decode CertificateStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateStatus :: Encode CertificateStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The certificate is invalid.</p>
newtype CertificateValidationException = CertificateValidationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCertificateValidationException :: Newtype CertificateValidationException _
derive instance repGenericCertificateValidationException :: Generic CertificateValidationException _
instance showCertificateValidationException :: Show CertificateValidationException where
  show = genericShow
instance decodeCertificateValidationException :: Decode CertificateValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateValidationException :: Encode CertificateValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Certificates = Certificates (Array Certificate)
derive instance newtypeCertificates :: Newtype Certificates _
derive instance repGenericCertificates :: Generic Certificates _
instance showCertificates :: Show Certificates where
  show = genericShow
instance decodeCertificates :: Decode Certificates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificates :: Encode Certificates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClearDefaultAuthorizerRequest = ClearDefaultAuthorizerRequest Types.NoArguments
derive instance newtypeClearDefaultAuthorizerRequest :: Newtype ClearDefaultAuthorizerRequest _
derive instance repGenericClearDefaultAuthorizerRequest :: Generic ClearDefaultAuthorizerRequest _
instance showClearDefaultAuthorizerRequest :: Show ClearDefaultAuthorizerRequest where
  show = genericShow
instance decodeClearDefaultAuthorizerRequest :: Decode ClearDefaultAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClearDefaultAuthorizerRequest :: Encode ClearDefaultAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClearDefaultAuthorizerResponse = ClearDefaultAuthorizerResponse Types.NoArguments
derive instance newtypeClearDefaultAuthorizerResponse :: Newtype ClearDefaultAuthorizerResponse _
derive instance repGenericClearDefaultAuthorizerResponse :: Generic ClearDefaultAuthorizerResponse _
instance showClearDefaultAuthorizerResponse :: Show ClearDefaultAuthorizerResponse where
  show = genericShow
instance decodeClearDefaultAuthorizerResponse :: Decode ClearDefaultAuthorizerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClearDefaultAuthorizerResponse :: Encode ClearDefaultAuthorizerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientId = ClientId String
derive instance newtypeClientId :: Newtype ClientId _
derive instance repGenericClientId :: Generic ClientId _
instance showClientId :: Show ClientId where
  show = genericShow
instance decodeClientId :: Decode ClientId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientId :: Encode ClientId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action that updates a CloudWatch alarm.</p>
newtype CloudwatchAlarmAction = CloudwatchAlarmAction 
  { "RoleArn'" :: (AwsArn)
  , "AlarmName'" :: (AlarmName)
  , "StateReason'" :: (StateReason)
  , "StateValue'" :: (StateValue)
  }
derive instance newtypeCloudwatchAlarmAction :: Newtype CloudwatchAlarmAction _
derive instance repGenericCloudwatchAlarmAction :: Generic CloudwatchAlarmAction _
instance showCloudwatchAlarmAction :: Show CloudwatchAlarmAction where
  show = genericShow
instance decodeCloudwatchAlarmAction :: Decode CloudwatchAlarmAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudwatchAlarmAction :: Encode CloudwatchAlarmAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action that captures a CloudWatch metric.</p>
newtype CloudwatchMetricAction = CloudwatchMetricAction 
  { "RoleArn'" :: (AwsArn)
  , "MetricNamespace'" :: (MetricNamespace)
  , "MetricName'" :: (MetricName)
  , "MetricValue'" :: (MetricValue)
  , "MetricUnit'" :: (MetricUnit)
  , "MetricTimestamp'" :: NullOrUndefined.NullOrUndefined (MetricTimestamp)
  }
derive instance newtypeCloudwatchMetricAction :: Newtype CloudwatchMetricAction _
derive instance repGenericCloudwatchMetricAction :: Generic CloudwatchMetricAction _
instance showCloudwatchMetricAction :: Show CloudwatchMetricAction where
  show = genericShow
instance decodeCloudwatchMetricAction :: Decode CloudwatchMetricAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudwatchMetricAction :: Encode CloudwatchMetricAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Code = Code String
derive instance newtypeCode :: Newtype Code _
derive instance repGenericCode :: Generic Code _
instance showCode :: Show Code where
  show = genericShow
instance decodeCode :: Decode Code where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCode :: Encode Code where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the method to use when code signing a file.</p>
newtype CodeSigning = CodeSigning 
  { "AwsSignerJobId'" :: NullOrUndefined.NullOrUndefined (SigningJobId)
  , "CustomCodeSigning'" :: NullOrUndefined.NullOrUndefined (CustomCodeSigning)
  }
derive instance newtypeCodeSigning :: Newtype CodeSigning _
derive instance repGenericCodeSigning :: Generic CodeSigning _
instance showCodeSigning :: Show CodeSigning where
  show = genericShow
instance decodeCodeSigning :: Decode CodeSigning where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeSigning :: Encode CodeSigning where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the certificate chain being used when code signing a file.</p>
newtype CodeSigningCertificateChain = CodeSigningCertificateChain 
  { "Stream'" :: NullOrUndefined.NullOrUndefined (Stream)
  , "CertificateName'" :: NullOrUndefined.NullOrUndefined (CertificateName)
  , "InlineDocument'" :: NullOrUndefined.NullOrUndefined (InlineDocument)
  }
derive instance newtypeCodeSigningCertificateChain :: Newtype CodeSigningCertificateChain _
derive instance repGenericCodeSigningCertificateChain :: Generic CodeSigningCertificateChain _
instance showCodeSigningCertificateChain :: Show CodeSigningCertificateChain where
  show = genericShow
instance decodeCodeSigningCertificateChain :: Decode CodeSigningCertificateChain where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeSigningCertificateChain :: Encode CodeSigningCertificateChain where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the signature for a file.</p>
newtype CodeSigningSignature = CodeSigningSignature 
  { "Stream'" :: NullOrUndefined.NullOrUndefined (Stream)
  , "InlineDocument'" :: NullOrUndefined.NullOrUndefined (Signature)
  }
derive instance newtypeCodeSigningSignature :: Newtype CodeSigningSignature _
derive instance repGenericCodeSigningSignature :: Generic CodeSigningSignature _
instance showCodeSigningSignature :: Show CodeSigningSignature where
  show = genericShow
instance decodeCodeSigningSignature :: Decode CodeSigningSignature where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeSigningSignature :: Encode CodeSigningSignature where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CognitoIdentityPoolId = CognitoIdentityPoolId String
derive instance newtypeCognitoIdentityPoolId :: Newtype CognitoIdentityPoolId _
derive instance repGenericCognitoIdentityPoolId :: Generic CognitoIdentityPoolId _
instance showCognitoIdentityPoolId :: Show CognitoIdentityPoolId where
  show = genericShow
instance decodeCognitoIdentityPoolId :: Decode CognitoIdentityPoolId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCognitoIdentityPoolId :: Encode CognitoIdentityPoolId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Comment = Comment String
derive instance newtypeComment :: Newtype Comment _
derive instance repGenericComment :: Generic Comment _
instance showComment :: Show Comment where
  show = genericShow
instance decodeComment :: Decode Comment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComment :: Encode Comment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration.</p>
newtype Configuration = Configuration 
  { "Enabled" :: NullOrUndefined.NullOrUndefined (Enabled)
  }
derive instance newtypeConfiguration :: Newtype Configuration _
derive instance repGenericConfiguration :: Generic Configuration _
instance showConfiguration :: Show Configuration where
  show = genericShow
instance decodeConfiguration :: Decode Configuration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfiguration :: Encode Configuration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A conflicting resource update exception. This exception is thrown when two pending updates cause a conflict.</p>
newtype ConflictingResourceUpdateException = ConflictingResourceUpdateException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeConflictingResourceUpdateException :: Newtype ConflictingResourceUpdateException _
derive instance repGenericConflictingResourceUpdateException :: Generic ConflictingResourceUpdateException _
instance showConflictingResourceUpdateException :: Show ConflictingResourceUpdateException where
  show = genericShow
instance decodeConflictingResourceUpdateException :: Decode ConflictingResourceUpdateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConflictingResourceUpdateException :: Encode ConflictingResourceUpdateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Count = Count Int
derive instance newtypeCount :: Newtype Count _
derive instance repGenericCount :: Generic Count _
instance showCount :: Show Count where
  show = genericShow
instance decodeCount :: Decode Count where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCount :: Encode Count where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateAuthorizerRequest = CreateAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  , "AuthorizerFunctionArn'" :: (AuthorizerFunctionArn)
  , "TokenKeyName'" :: (TokenKeyName)
  , "TokenSigningPublicKeys'" :: (PublicKeyMap)
  , "Status'" :: NullOrUndefined.NullOrUndefined (AuthorizerStatus)
  }
derive instance newtypeCreateAuthorizerRequest :: Newtype CreateAuthorizerRequest _
derive instance repGenericCreateAuthorizerRequest :: Generic CreateAuthorizerRequest _
instance showCreateAuthorizerRequest :: Show CreateAuthorizerRequest where
  show = genericShow
instance decodeCreateAuthorizerRequest :: Decode CreateAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAuthorizerRequest :: Encode CreateAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateAuthorizerResponse = CreateAuthorizerResponse 
  { "AuthorizerName'" :: NullOrUndefined.NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined.NullOrUndefined (AuthorizerArn)
  }
derive instance newtypeCreateAuthorizerResponse :: Newtype CreateAuthorizerResponse _
derive instance repGenericCreateAuthorizerResponse :: Generic CreateAuthorizerResponse _
instance showCreateAuthorizerResponse :: Show CreateAuthorizerResponse where
  show = genericShow
instance decodeCreateAuthorizerResponse :: Decode CreateAuthorizerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAuthorizerResponse :: Encode CreateAuthorizerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the CreateCertificateFromCsr operation.</p>
newtype CreateCertificateFromCsrRequest = CreateCertificateFromCsrRequest 
  { "CertificateSigningRequest'" :: (CertificateSigningRequest)
  , "SetAsActive'" :: NullOrUndefined.NullOrUndefined (SetAsActive)
  }
derive instance newtypeCreateCertificateFromCsrRequest :: Newtype CreateCertificateFromCsrRequest _
derive instance repGenericCreateCertificateFromCsrRequest :: Generic CreateCertificateFromCsrRequest _
instance showCreateCertificateFromCsrRequest :: Show CreateCertificateFromCsrRequest where
  show = genericShow
instance decodeCreateCertificateFromCsrRequest :: Decode CreateCertificateFromCsrRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCertificateFromCsrRequest :: Encode CreateCertificateFromCsrRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the CreateCertificateFromCsr operation.</p>
newtype CreateCertificateFromCsrResponse = CreateCertificateFromCsrResponse 
  { "CertificateArn'" :: NullOrUndefined.NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined.NullOrUndefined (CertificateId)
  , "CertificatePem'" :: NullOrUndefined.NullOrUndefined (CertificatePem)
  }
derive instance newtypeCreateCertificateFromCsrResponse :: Newtype CreateCertificateFromCsrResponse _
derive instance repGenericCreateCertificateFromCsrResponse :: Generic CreateCertificateFromCsrResponse _
instance showCreateCertificateFromCsrResponse :: Show CreateCertificateFromCsrResponse where
  show = genericShow
instance decodeCreateCertificateFromCsrResponse :: Decode CreateCertificateFromCsrResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCertificateFromCsrResponse :: Encode CreateCertificateFromCsrResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateJobRequest = CreateJobRequest 
  { "JobId'" :: (JobId)
  , "Targets'" :: (JobTargets)
  , "DocumentSource'" :: NullOrUndefined.NullOrUndefined (JobDocumentSource)
  , "Document'" :: NullOrUndefined.NullOrUndefined (JobDocument)
  , "Description'" :: NullOrUndefined.NullOrUndefined (JobDescription)
  , "PresignedUrlConfig'" :: NullOrUndefined.NullOrUndefined (PresignedUrlConfig)
  , "TargetSelection'" :: NullOrUndefined.NullOrUndefined (TargetSelection)
  , "JobExecutionsRolloutConfig'" :: NullOrUndefined.NullOrUndefined (JobExecutionsRolloutConfig)
  , "DocumentParameters'" :: NullOrUndefined.NullOrUndefined (JobDocumentParameters)
  }
derive instance newtypeCreateJobRequest :: Newtype CreateJobRequest _
derive instance repGenericCreateJobRequest :: Generic CreateJobRequest _
instance showCreateJobRequest :: Show CreateJobRequest where
  show = genericShow
instance decodeCreateJobRequest :: Decode CreateJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobRequest :: Encode CreateJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateJobResponse = CreateJobResponse 
  { "JobArn'" :: NullOrUndefined.NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined.NullOrUndefined (JobId)
  , "Description'" :: NullOrUndefined.NullOrUndefined (JobDescription)
  }
derive instance newtypeCreateJobResponse :: Newtype CreateJobResponse _
derive instance repGenericCreateJobResponse :: Generic CreateJobResponse _
instance showCreateJobResponse :: Show CreateJobResponse where
  show = genericShow
instance decodeCreateJobResponse :: Decode CreateJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobResponse :: Encode CreateJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the CreateKeysAndCertificate operation.</p>
newtype CreateKeysAndCertificateRequest = CreateKeysAndCertificateRequest 
  { "SetAsActive'" :: NullOrUndefined.NullOrUndefined (SetAsActive)
  }
derive instance newtypeCreateKeysAndCertificateRequest :: Newtype CreateKeysAndCertificateRequest _
derive instance repGenericCreateKeysAndCertificateRequest :: Generic CreateKeysAndCertificateRequest _
instance showCreateKeysAndCertificateRequest :: Show CreateKeysAndCertificateRequest where
  show = genericShow
instance decodeCreateKeysAndCertificateRequest :: Decode CreateKeysAndCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateKeysAndCertificateRequest :: Encode CreateKeysAndCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output of the CreateKeysAndCertificate operation.</p>
newtype CreateKeysAndCertificateResponse = CreateKeysAndCertificateResponse 
  { "CertificateArn'" :: NullOrUndefined.NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined.NullOrUndefined (CertificateId)
  , "CertificatePem'" :: NullOrUndefined.NullOrUndefined (CertificatePem)
  , "KeyPair'" :: NullOrUndefined.NullOrUndefined (KeyPair)
  }
derive instance newtypeCreateKeysAndCertificateResponse :: Newtype CreateKeysAndCertificateResponse _
derive instance repGenericCreateKeysAndCertificateResponse :: Generic CreateKeysAndCertificateResponse _
instance showCreateKeysAndCertificateResponse :: Show CreateKeysAndCertificateResponse where
  show = genericShow
instance decodeCreateKeysAndCertificateResponse :: Decode CreateKeysAndCertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateKeysAndCertificateResponse :: Encode CreateKeysAndCertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateOTAUpdateRequest = CreateOTAUpdateRequest 
  { "OtaUpdateId'" :: (OTAUpdateId)
  , "Description'" :: NullOrUndefined.NullOrUndefined (OTAUpdateDescription)
  , "Targets'" :: (Targets)
  , "TargetSelection'" :: NullOrUndefined.NullOrUndefined (TargetSelection)
  , "Files'" :: (OTAUpdateFiles)
  , "RoleArn'" :: (RoleArn)
  , "AdditionalParameters'" :: NullOrUndefined.NullOrUndefined (AdditionalParameterMap)
  }
derive instance newtypeCreateOTAUpdateRequest :: Newtype CreateOTAUpdateRequest _
derive instance repGenericCreateOTAUpdateRequest :: Generic CreateOTAUpdateRequest _
instance showCreateOTAUpdateRequest :: Show CreateOTAUpdateRequest where
  show = genericShow
instance decodeCreateOTAUpdateRequest :: Decode CreateOTAUpdateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateOTAUpdateRequest :: Encode CreateOTAUpdateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateOTAUpdateResponse = CreateOTAUpdateResponse 
  { "OtaUpdateId'" :: NullOrUndefined.NullOrUndefined (OTAUpdateId)
  , "AwsIotJobId'" :: NullOrUndefined.NullOrUndefined (AwsIotJobId)
  , "OtaUpdateArn'" :: NullOrUndefined.NullOrUndefined (OTAUpdateArn)
  , "AwsIotJobArn'" :: NullOrUndefined.NullOrUndefined (AwsIotJobArn)
  , "OtaUpdateStatus'" :: NullOrUndefined.NullOrUndefined (OTAUpdateStatus)
  }
derive instance newtypeCreateOTAUpdateResponse :: Newtype CreateOTAUpdateResponse _
derive instance repGenericCreateOTAUpdateResponse :: Generic CreateOTAUpdateResponse _
instance showCreateOTAUpdateResponse :: Show CreateOTAUpdateResponse where
  show = genericShow
instance decodeCreateOTAUpdateResponse :: Decode CreateOTAUpdateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateOTAUpdateResponse :: Encode CreateOTAUpdateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the CreatePolicy operation.</p>
newtype CreatePolicyRequest = CreatePolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyDocument'" :: (PolicyDocument)
  }
derive instance newtypeCreatePolicyRequest :: Newtype CreatePolicyRequest _
derive instance repGenericCreatePolicyRequest :: Generic CreatePolicyRequest _
instance showCreatePolicyRequest :: Show CreatePolicyRequest where
  show = genericShow
instance decodeCreatePolicyRequest :: Decode CreatePolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePolicyRequest :: Encode CreatePolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the CreatePolicy operation.</p>
newtype CreatePolicyResponse = CreatePolicyResponse 
  { "PolicyName'" :: NullOrUndefined.NullOrUndefined (PolicyName)
  , "PolicyArn'" :: NullOrUndefined.NullOrUndefined (PolicyArn)
  , "PolicyDocument'" :: NullOrUndefined.NullOrUndefined (PolicyDocument)
  , "PolicyVersionId'" :: NullOrUndefined.NullOrUndefined (PolicyVersionId)
  }
derive instance newtypeCreatePolicyResponse :: Newtype CreatePolicyResponse _
derive instance repGenericCreatePolicyResponse :: Generic CreatePolicyResponse _
instance showCreatePolicyResponse :: Show CreatePolicyResponse where
  show = genericShow
instance decodeCreatePolicyResponse :: Decode CreatePolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePolicyResponse :: Encode CreatePolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the CreatePolicyVersion operation.</p>
newtype CreatePolicyVersionRequest = CreatePolicyVersionRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyDocument'" :: (PolicyDocument)
  , "SetAsDefault'" :: NullOrUndefined.NullOrUndefined (SetAsDefault)
  }
derive instance newtypeCreatePolicyVersionRequest :: Newtype CreatePolicyVersionRequest _
derive instance repGenericCreatePolicyVersionRequest :: Generic CreatePolicyVersionRequest _
instance showCreatePolicyVersionRequest :: Show CreatePolicyVersionRequest where
  show = genericShow
instance decodeCreatePolicyVersionRequest :: Decode CreatePolicyVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePolicyVersionRequest :: Encode CreatePolicyVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output of the CreatePolicyVersion operation.</p>
newtype CreatePolicyVersionResponse = CreatePolicyVersionResponse 
  { "PolicyArn'" :: NullOrUndefined.NullOrUndefined (PolicyArn)
  , "PolicyDocument'" :: NullOrUndefined.NullOrUndefined (PolicyDocument)
  , "PolicyVersionId'" :: NullOrUndefined.NullOrUndefined (PolicyVersionId)
  , "IsDefaultVersion'" :: NullOrUndefined.NullOrUndefined (IsDefaultVersion)
  }
derive instance newtypeCreatePolicyVersionResponse :: Newtype CreatePolicyVersionResponse _
derive instance repGenericCreatePolicyVersionResponse :: Generic CreatePolicyVersionResponse _
instance showCreatePolicyVersionResponse :: Show CreatePolicyVersionResponse where
  show = genericShow
instance decodeCreatePolicyVersionResponse :: Decode CreatePolicyVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePolicyVersionResponse :: Encode CreatePolicyVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRoleAliasRequest = CreateRoleAliasRequest 
  { "RoleAlias'" :: (RoleAlias)
  , "RoleArn'" :: (RoleArn)
  , "CredentialDurationSeconds'" :: NullOrUndefined.NullOrUndefined (CredentialDurationSeconds)
  }
derive instance newtypeCreateRoleAliasRequest :: Newtype CreateRoleAliasRequest _
derive instance repGenericCreateRoleAliasRequest :: Generic CreateRoleAliasRequest _
instance showCreateRoleAliasRequest :: Show CreateRoleAliasRequest where
  show = genericShow
instance decodeCreateRoleAliasRequest :: Decode CreateRoleAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRoleAliasRequest :: Encode CreateRoleAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRoleAliasResponse = CreateRoleAliasResponse 
  { "RoleAlias'" :: NullOrUndefined.NullOrUndefined (RoleAlias)
  , "RoleAliasArn'" :: NullOrUndefined.NullOrUndefined (RoleAliasArn)
  }
derive instance newtypeCreateRoleAliasResponse :: Newtype CreateRoleAliasResponse _
derive instance repGenericCreateRoleAliasResponse :: Generic CreateRoleAliasResponse _
instance showCreateRoleAliasResponse :: Show CreateRoleAliasResponse where
  show = genericShow
instance decodeCreateRoleAliasResponse :: Decode CreateRoleAliasResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRoleAliasResponse :: Encode CreateRoleAliasResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateStreamRequest = CreateStreamRequest 
  { "StreamId'" :: (StreamId)
  , "Description'" :: NullOrUndefined.NullOrUndefined (StreamDescription)
  , "Files'" :: (StreamFiles)
  , "RoleArn'" :: (RoleArn)
  }
derive instance newtypeCreateStreamRequest :: Newtype CreateStreamRequest _
derive instance repGenericCreateStreamRequest :: Generic CreateStreamRequest _
instance showCreateStreamRequest :: Show CreateStreamRequest where
  show = genericShow
instance decodeCreateStreamRequest :: Decode CreateStreamRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStreamRequest :: Encode CreateStreamRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateStreamResponse = CreateStreamResponse 
  { "StreamId'" :: NullOrUndefined.NullOrUndefined (StreamId)
  , "StreamArn'" :: NullOrUndefined.NullOrUndefined (StreamArn)
  , "Description'" :: NullOrUndefined.NullOrUndefined (StreamDescription)
  , "StreamVersion'" :: NullOrUndefined.NullOrUndefined (StreamVersion)
  }
derive instance newtypeCreateStreamResponse :: Newtype CreateStreamResponse _
derive instance repGenericCreateStreamResponse :: Generic CreateStreamResponse _
instance showCreateStreamResponse :: Show CreateStreamResponse where
  show = genericShow
instance decodeCreateStreamResponse :: Decode CreateStreamResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStreamResponse :: Encode CreateStreamResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateThingGroupRequest = CreateThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  , "ParentGroupName'" :: NullOrUndefined.NullOrUndefined (ThingGroupName)
  , "ThingGroupProperties'" :: NullOrUndefined.NullOrUndefined (ThingGroupProperties)
  }
derive instance newtypeCreateThingGroupRequest :: Newtype CreateThingGroupRequest _
derive instance repGenericCreateThingGroupRequest :: Generic CreateThingGroupRequest _
instance showCreateThingGroupRequest :: Show CreateThingGroupRequest where
  show = genericShow
instance decodeCreateThingGroupRequest :: Decode CreateThingGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateThingGroupRequest :: Encode CreateThingGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateThingGroupResponse = CreateThingGroupResponse 
  { "ThingGroupName'" :: NullOrUndefined.NullOrUndefined (ThingGroupName)
  , "ThingGroupArn'" :: NullOrUndefined.NullOrUndefined (ThingGroupArn)
  , "ThingGroupId'" :: NullOrUndefined.NullOrUndefined (ThingGroupId)
  }
derive instance newtypeCreateThingGroupResponse :: Newtype CreateThingGroupResponse _
derive instance repGenericCreateThingGroupResponse :: Generic CreateThingGroupResponse _
instance showCreateThingGroupResponse :: Show CreateThingGroupResponse where
  show = genericShow
instance decodeCreateThingGroupResponse :: Decode CreateThingGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateThingGroupResponse :: Encode CreateThingGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the CreateThing operation.</p>
newtype CreateThingRequest = CreateThingRequest 
  { "ThingName'" :: (ThingName)
  , "ThingTypeName'" :: NullOrUndefined.NullOrUndefined (ThingTypeName)
  , "AttributePayload'" :: NullOrUndefined.NullOrUndefined (AttributePayload)
  }
derive instance newtypeCreateThingRequest :: Newtype CreateThingRequest _
derive instance repGenericCreateThingRequest :: Generic CreateThingRequest _
instance showCreateThingRequest :: Show CreateThingRequest where
  show = genericShow
instance decodeCreateThingRequest :: Decode CreateThingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateThingRequest :: Encode CreateThingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output of the CreateThing operation.</p>
newtype CreateThingResponse = CreateThingResponse 
  { "ThingName'" :: NullOrUndefined.NullOrUndefined (ThingName)
  , "ThingArn'" :: NullOrUndefined.NullOrUndefined (ThingArn)
  , "ThingId'" :: NullOrUndefined.NullOrUndefined (ThingId)
  }
derive instance newtypeCreateThingResponse :: Newtype CreateThingResponse _
derive instance repGenericCreateThingResponse :: Generic CreateThingResponse _
instance showCreateThingResponse :: Show CreateThingResponse where
  show = genericShow
instance decodeCreateThingResponse :: Decode CreateThingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateThingResponse :: Encode CreateThingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the CreateThingType operation.</p>
newtype CreateThingTypeRequest = CreateThingTypeRequest 
  { "ThingTypeName'" :: (ThingTypeName)
  , "ThingTypeProperties'" :: NullOrUndefined.NullOrUndefined (ThingTypeProperties)
  }
derive instance newtypeCreateThingTypeRequest :: Newtype CreateThingTypeRequest _
derive instance repGenericCreateThingTypeRequest :: Generic CreateThingTypeRequest _
instance showCreateThingTypeRequest :: Show CreateThingTypeRequest where
  show = genericShow
instance decodeCreateThingTypeRequest :: Decode CreateThingTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateThingTypeRequest :: Encode CreateThingTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output of the CreateThingType operation.</p>
newtype CreateThingTypeResponse = CreateThingTypeResponse 
  { "ThingTypeName'" :: NullOrUndefined.NullOrUndefined (ThingTypeName)
  , "ThingTypeArn'" :: NullOrUndefined.NullOrUndefined (ThingTypeArn)
  , "ThingTypeId'" :: NullOrUndefined.NullOrUndefined (ThingTypeId)
  }
derive instance newtypeCreateThingTypeResponse :: Newtype CreateThingTypeResponse _
derive instance repGenericCreateThingTypeResponse :: Generic CreateThingTypeResponse _
instance showCreateThingTypeResponse :: Show CreateThingTypeResponse where
  show = genericShow
instance decodeCreateThingTypeResponse :: Decode CreateThingTypeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateThingTypeResponse :: Encode CreateThingTypeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the CreateTopicRule operation.</p>
newtype CreateTopicRuleRequest = CreateTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  , "TopicRulePayload'" :: (TopicRulePayload)
  }
derive instance newtypeCreateTopicRuleRequest :: Newtype CreateTopicRuleRequest _
derive instance repGenericCreateTopicRuleRequest :: Generic CreateTopicRuleRequest _
instance showCreateTopicRuleRequest :: Show CreateTopicRuleRequest where
  show = genericShow
instance decodeCreateTopicRuleRequest :: Decode CreateTopicRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTopicRuleRequest :: Encode CreateTopicRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatedAtDate = CreatedAtDate Number
derive instance newtypeCreatedAtDate :: Newtype CreatedAtDate _
derive instance repGenericCreatedAtDate :: Generic CreatedAtDate _
instance showCreatedAtDate :: Show CreatedAtDate where
  show = genericShow
instance decodeCreatedAtDate :: Decode CreatedAtDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatedAtDate :: Encode CreatedAtDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreationDate = CreationDate Number
derive instance newtypeCreationDate :: Newtype CreationDate _
derive instance repGenericCreationDate :: Generic CreationDate _
instance showCreationDate :: Show CreationDate where
  show = genericShow
instance decodeCreationDate :: Decode CreationDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreationDate :: Encode CreationDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CredentialDurationSeconds = CredentialDurationSeconds Int
derive instance newtypeCredentialDurationSeconds :: Newtype CredentialDurationSeconds _
derive instance repGenericCredentialDurationSeconds :: Generic CredentialDurationSeconds _
instance showCredentialDurationSeconds :: Show CredentialDurationSeconds where
  show = genericShow
instance decodeCredentialDurationSeconds :: Decode CredentialDurationSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCredentialDurationSeconds :: Encode CredentialDurationSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a custom method used to code sign a file.</p>
newtype CustomCodeSigning = CustomCodeSigning 
  { "Signature'" :: NullOrUndefined.NullOrUndefined (CodeSigningSignature)
  , "CertificateChain'" :: NullOrUndefined.NullOrUndefined (CodeSigningCertificateChain)
  , "HashAlgorithm'" :: NullOrUndefined.NullOrUndefined (HashAlgorithm)
  , "SignatureAlgorithm'" :: NullOrUndefined.NullOrUndefined (SignatureAlgorithm)
  }
derive instance newtypeCustomCodeSigning :: Newtype CustomCodeSigning _
derive instance repGenericCustomCodeSigning :: Generic CustomCodeSigning _
instance showCustomCodeSigning :: Show CustomCodeSigning where
  show = genericShow
instance decodeCustomCodeSigning :: Decode CustomCodeSigning where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomCodeSigning :: Encode CustomCodeSigning where
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


newtype DeleteAuthorizerRequest = DeleteAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  }
derive instance newtypeDeleteAuthorizerRequest :: Newtype DeleteAuthorizerRequest _
derive instance repGenericDeleteAuthorizerRequest :: Generic DeleteAuthorizerRequest _
instance showDeleteAuthorizerRequest :: Show DeleteAuthorizerRequest where
  show = genericShow
instance decodeDeleteAuthorizerRequest :: Decode DeleteAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAuthorizerRequest :: Encode DeleteAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAuthorizerResponse = DeleteAuthorizerResponse Types.NoArguments
derive instance newtypeDeleteAuthorizerResponse :: Newtype DeleteAuthorizerResponse _
derive instance repGenericDeleteAuthorizerResponse :: Generic DeleteAuthorizerResponse _
instance showDeleteAuthorizerResponse :: Show DeleteAuthorizerResponse where
  show = genericShow
instance decodeDeleteAuthorizerResponse :: Decode DeleteAuthorizerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAuthorizerResponse :: Encode DeleteAuthorizerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for the DeleteCACertificate operation.</p>
newtype DeleteCACertificateRequest = DeleteCACertificateRequest 
  { "CertificateId'" :: (CertificateId)
  }
derive instance newtypeDeleteCACertificateRequest :: Newtype DeleteCACertificateRequest _
derive instance repGenericDeleteCACertificateRequest :: Generic DeleteCACertificateRequest _
instance showDeleteCACertificateRequest :: Show DeleteCACertificateRequest where
  show = genericShow
instance decodeDeleteCACertificateRequest :: Decode DeleteCACertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCACertificateRequest :: Encode DeleteCACertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the DeleteCACertificate operation.</p>
newtype DeleteCACertificateResponse = DeleteCACertificateResponse Types.NoArguments
derive instance newtypeDeleteCACertificateResponse :: Newtype DeleteCACertificateResponse _
derive instance repGenericDeleteCACertificateResponse :: Generic DeleteCACertificateResponse _
instance showDeleteCACertificateResponse :: Show DeleteCACertificateResponse where
  show = genericShow
instance decodeDeleteCACertificateResponse :: Decode DeleteCACertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCACertificateResponse :: Encode DeleteCACertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DeleteCertificate operation.</p>
newtype DeleteCertificateRequest = DeleteCertificateRequest 
  { "CertificateId'" :: (CertificateId)
  , "ForceDelete'" :: NullOrUndefined.NullOrUndefined (ForceDelete)
  }
derive instance newtypeDeleteCertificateRequest :: Newtype DeleteCertificateRequest _
derive instance repGenericDeleteCertificateRequest :: Generic DeleteCertificateRequest _
instance showDeleteCertificateRequest :: Show DeleteCertificateRequest where
  show = genericShow
instance decodeDeleteCertificateRequest :: Decode DeleteCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCertificateRequest :: Encode DeleteCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You can't delete the resource because it is attached to one or more resources.</p>
newtype DeleteConflictException = DeleteConflictException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeDeleteConflictException :: Newtype DeleteConflictException _
derive instance repGenericDeleteConflictException :: Generic DeleteConflictException _
instance showDeleteConflictException :: Show DeleteConflictException where
  show = genericShow
instance decodeDeleteConflictException :: Decode DeleteConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConflictException :: Encode DeleteConflictException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteOTAUpdateRequest = DeleteOTAUpdateRequest 
  { "OtaUpdateId'" :: (OTAUpdateId)
  }
derive instance newtypeDeleteOTAUpdateRequest :: Newtype DeleteOTAUpdateRequest _
derive instance repGenericDeleteOTAUpdateRequest :: Generic DeleteOTAUpdateRequest _
instance showDeleteOTAUpdateRequest :: Show DeleteOTAUpdateRequest where
  show = genericShow
instance decodeDeleteOTAUpdateRequest :: Decode DeleteOTAUpdateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteOTAUpdateRequest :: Encode DeleteOTAUpdateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteOTAUpdateResponse = DeleteOTAUpdateResponse Types.NoArguments
derive instance newtypeDeleteOTAUpdateResponse :: Newtype DeleteOTAUpdateResponse _
derive instance repGenericDeleteOTAUpdateResponse :: Generic DeleteOTAUpdateResponse _
instance showDeleteOTAUpdateResponse :: Show DeleteOTAUpdateResponse where
  show = genericShow
instance decodeDeleteOTAUpdateResponse :: Decode DeleteOTAUpdateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteOTAUpdateResponse :: Encode DeleteOTAUpdateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DeletePolicy operation.</p>
newtype DeletePolicyRequest = DeletePolicyRequest 
  { "PolicyName'" :: (PolicyName)
  }
derive instance newtypeDeletePolicyRequest :: Newtype DeletePolicyRequest _
derive instance repGenericDeletePolicyRequest :: Generic DeletePolicyRequest _
instance showDeletePolicyRequest :: Show DeletePolicyRequest where
  show = genericShow
instance decodeDeletePolicyRequest :: Decode DeletePolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePolicyRequest :: Encode DeletePolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DeletePolicyVersion operation.</p>
newtype DeletePolicyVersionRequest = DeletePolicyVersionRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyVersionId'" :: (PolicyVersionId)
  }
derive instance newtypeDeletePolicyVersionRequest :: Newtype DeletePolicyVersionRequest _
derive instance repGenericDeletePolicyVersionRequest :: Generic DeletePolicyVersionRequest _
instance showDeletePolicyVersionRequest :: Show DeletePolicyVersionRequest where
  show = genericShow
instance decodeDeletePolicyVersionRequest :: Decode DeletePolicyVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePolicyVersionRequest :: Encode DeletePolicyVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DeleteRegistrationCode operation.</p>
newtype DeleteRegistrationCodeRequest = DeleteRegistrationCodeRequest Types.NoArguments
derive instance newtypeDeleteRegistrationCodeRequest :: Newtype DeleteRegistrationCodeRequest _
derive instance repGenericDeleteRegistrationCodeRequest :: Generic DeleteRegistrationCodeRequest _
instance showDeleteRegistrationCodeRequest :: Show DeleteRegistrationCodeRequest where
  show = genericShow
instance decodeDeleteRegistrationCodeRequest :: Decode DeleteRegistrationCodeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRegistrationCodeRequest :: Encode DeleteRegistrationCodeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the DeleteRegistrationCode operation.</p>
newtype DeleteRegistrationCodeResponse = DeleteRegistrationCodeResponse Types.NoArguments
derive instance newtypeDeleteRegistrationCodeResponse :: Newtype DeleteRegistrationCodeResponse _
derive instance repGenericDeleteRegistrationCodeResponse :: Generic DeleteRegistrationCodeResponse _
instance showDeleteRegistrationCodeResponse :: Show DeleteRegistrationCodeResponse where
  show = genericShow
instance decodeDeleteRegistrationCodeResponse :: Decode DeleteRegistrationCodeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRegistrationCodeResponse :: Encode DeleteRegistrationCodeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRoleAliasRequest = DeleteRoleAliasRequest 
  { "RoleAlias'" :: (RoleAlias)
  }
derive instance newtypeDeleteRoleAliasRequest :: Newtype DeleteRoleAliasRequest _
derive instance repGenericDeleteRoleAliasRequest :: Generic DeleteRoleAliasRequest _
instance showDeleteRoleAliasRequest :: Show DeleteRoleAliasRequest where
  show = genericShow
instance decodeDeleteRoleAliasRequest :: Decode DeleteRoleAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRoleAliasRequest :: Encode DeleteRoleAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRoleAliasResponse = DeleteRoleAliasResponse Types.NoArguments
derive instance newtypeDeleteRoleAliasResponse :: Newtype DeleteRoleAliasResponse _
derive instance repGenericDeleteRoleAliasResponse :: Generic DeleteRoleAliasResponse _
instance showDeleteRoleAliasResponse :: Show DeleteRoleAliasResponse where
  show = genericShow
instance decodeDeleteRoleAliasResponse :: Decode DeleteRoleAliasResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRoleAliasResponse :: Encode DeleteRoleAliasResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteStreamRequest = DeleteStreamRequest 
  { "StreamId'" :: (StreamId)
  }
derive instance newtypeDeleteStreamRequest :: Newtype DeleteStreamRequest _
derive instance repGenericDeleteStreamRequest :: Generic DeleteStreamRequest _
instance showDeleteStreamRequest :: Show DeleteStreamRequest where
  show = genericShow
instance decodeDeleteStreamRequest :: Decode DeleteStreamRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStreamRequest :: Encode DeleteStreamRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteStreamResponse = DeleteStreamResponse Types.NoArguments
derive instance newtypeDeleteStreamResponse :: Newtype DeleteStreamResponse _
derive instance repGenericDeleteStreamResponse :: Generic DeleteStreamResponse _
instance showDeleteStreamResponse :: Show DeleteStreamResponse where
  show = genericShow
instance decodeDeleteStreamResponse :: Decode DeleteStreamResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStreamResponse :: Encode DeleteStreamResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteThingGroupRequest = DeleteThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  , "ExpectedVersion'" :: NullOrUndefined.NullOrUndefined (OptionalVersion)
  }
derive instance newtypeDeleteThingGroupRequest :: Newtype DeleteThingGroupRequest _
derive instance repGenericDeleteThingGroupRequest :: Generic DeleteThingGroupRequest _
instance showDeleteThingGroupRequest :: Show DeleteThingGroupRequest where
  show = genericShow
instance decodeDeleteThingGroupRequest :: Decode DeleteThingGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteThingGroupRequest :: Encode DeleteThingGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteThingGroupResponse = DeleteThingGroupResponse Types.NoArguments
derive instance newtypeDeleteThingGroupResponse :: Newtype DeleteThingGroupResponse _
derive instance repGenericDeleteThingGroupResponse :: Generic DeleteThingGroupResponse _
instance showDeleteThingGroupResponse :: Show DeleteThingGroupResponse where
  show = genericShow
instance decodeDeleteThingGroupResponse :: Decode DeleteThingGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteThingGroupResponse :: Encode DeleteThingGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DeleteThing operation.</p>
newtype DeleteThingRequest = DeleteThingRequest 
  { "ThingName'" :: (ThingName)
  , "ExpectedVersion'" :: NullOrUndefined.NullOrUndefined (OptionalVersion)
  }
derive instance newtypeDeleteThingRequest :: Newtype DeleteThingRequest _
derive instance repGenericDeleteThingRequest :: Generic DeleteThingRequest _
instance showDeleteThingRequest :: Show DeleteThingRequest where
  show = genericShow
instance decodeDeleteThingRequest :: Decode DeleteThingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteThingRequest :: Encode DeleteThingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output of the DeleteThing operation.</p>
newtype DeleteThingResponse = DeleteThingResponse Types.NoArguments
derive instance newtypeDeleteThingResponse :: Newtype DeleteThingResponse _
derive instance repGenericDeleteThingResponse :: Generic DeleteThingResponse _
instance showDeleteThingResponse :: Show DeleteThingResponse where
  show = genericShow
instance decodeDeleteThingResponse :: Decode DeleteThingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteThingResponse :: Encode DeleteThingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DeleteThingType operation.</p>
newtype DeleteThingTypeRequest = DeleteThingTypeRequest 
  { "ThingTypeName'" :: (ThingTypeName)
  }
derive instance newtypeDeleteThingTypeRequest :: Newtype DeleteThingTypeRequest _
derive instance repGenericDeleteThingTypeRequest :: Generic DeleteThingTypeRequest _
instance showDeleteThingTypeRequest :: Show DeleteThingTypeRequest where
  show = genericShow
instance decodeDeleteThingTypeRequest :: Decode DeleteThingTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteThingTypeRequest :: Encode DeleteThingTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the DeleteThingType operation.</p>
newtype DeleteThingTypeResponse = DeleteThingTypeResponse Types.NoArguments
derive instance newtypeDeleteThingTypeResponse :: Newtype DeleteThingTypeResponse _
derive instance repGenericDeleteThingTypeResponse :: Generic DeleteThingTypeResponse _
instance showDeleteThingTypeResponse :: Show DeleteThingTypeResponse where
  show = genericShow
instance decodeDeleteThingTypeResponse :: Decode DeleteThingTypeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteThingTypeResponse :: Encode DeleteThingTypeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DeleteTopicRule operation.</p>
newtype DeleteTopicRuleRequest = DeleteTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  }
derive instance newtypeDeleteTopicRuleRequest :: Newtype DeleteTopicRuleRequest _
derive instance repGenericDeleteTopicRuleRequest :: Generic DeleteTopicRuleRequest _
instance showDeleteTopicRuleRequest :: Show DeleteTopicRuleRequest where
  show = genericShow
instance decodeDeleteTopicRuleRequest :: Decode DeleteTopicRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTopicRuleRequest :: Encode DeleteTopicRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteV2LoggingLevelRequest = DeleteV2LoggingLevelRequest 
  { "TargetType'" :: (LogTargetType)
  , "TargetName'" :: (LogTargetName)
  }
derive instance newtypeDeleteV2LoggingLevelRequest :: Newtype DeleteV2LoggingLevelRequest _
derive instance repGenericDeleteV2LoggingLevelRequest :: Generic DeleteV2LoggingLevelRequest _
instance showDeleteV2LoggingLevelRequest :: Show DeleteV2LoggingLevelRequest where
  show = genericShow
instance decodeDeleteV2LoggingLevelRequest :: Decode DeleteV2LoggingLevelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteV2LoggingLevelRequest :: Encode DeleteV2LoggingLevelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryStreamName = DeliveryStreamName String
derive instance newtypeDeliveryStreamName :: Newtype DeliveryStreamName _
derive instance repGenericDeliveryStreamName :: Generic DeliveryStreamName _
instance showDeliveryStreamName :: Show DeliveryStreamName where
  show = genericShow
instance decodeDeliveryStreamName :: Decode DeliveryStreamName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryStreamName :: Encode DeliveryStreamName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information that denied the authorization.</p>
newtype Denied = Denied 
  { "ImplicitDeny'" :: NullOrUndefined.NullOrUndefined (ImplicitDeny)
  , "ExplicitDeny'" :: NullOrUndefined.NullOrUndefined (ExplicitDeny)
  }
derive instance newtypeDenied :: Newtype Denied _
derive instance repGenericDenied :: Generic Denied _
instance showDenied :: Show Denied where
  show = genericShow
instance decodeDenied :: Decode Denied where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDenied :: Encode Denied where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DeprecateThingType operation.</p>
newtype DeprecateThingTypeRequest = DeprecateThingTypeRequest 
  { "ThingTypeName'" :: (ThingTypeName)
  , "UndoDeprecate'" :: NullOrUndefined.NullOrUndefined (UndoDeprecate)
  }
derive instance newtypeDeprecateThingTypeRequest :: Newtype DeprecateThingTypeRequest _
derive instance repGenericDeprecateThingTypeRequest :: Generic DeprecateThingTypeRequest _
instance showDeprecateThingTypeRequest :: Show DeprecateThingTypeRequest where
  show = genericShow
instance decodeDeprecateThingTypeRequest :: Decode DeprecateThingTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeprecateThingTypeRequest :: Encode DeprecateThingTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the DeprecateThingType operation.</p>
newtype DeprecateThingTypeResponse = DeprecateThingTypeResponse Types.NoArguments
derive instance newtypeDeprecateThingTypeResponse :: Newtype DeprecateThingTypeResponse _
derive instance repGenericDeprecateThingTypeResponse :: Generic DeprecateThingTypeResponse _
instance showDeprecateThingTypeResponse :: Show DeprecateThingTypeResponse where
  show = genericShow
instance decodeDeprecateThingTypeResponse :: Decode DeprecateThingTypeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeprecateThingTypeResponse :: Encode DeprecateThingTypeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeprecationDate = DeprecationDate Number
derive instance newtypeDeprecationDate :: Newtype DeprecationDate _
derive instance repGenericDeprecationDate :: Generic DeprecationDate _
instance showDeprecationDate :: Show DeprecationDate where
  show = genericShow
instance decodeDeprecationDate :: Decode DeprecationDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeprecationDate :: Encode DeprecationDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAuthorizerRequest = DescribeAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  }
derive instance newtypeDescribeAuthorizerRequest :: Newtype DescribeAuthorizerRequest _
derive instance repGenericDescribeAuthorizerRequest :: Generic DescribeAuthorizerRequest _
instance showDescribeAuthorizerRequest :: Show DescribeAuthorizerRequest where
  show = genericShow
instance decodeDescribeAuthorizerRequest :: Decode DescribeAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAuthorizerRequest :: Encode DescribeAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAuthorizerResponse = DescribeAuthorizerResponse 
  { "AuthorizerDescription'" :: NullOrUndefined.NullOrUndefined (AuthorizerDescription)
  }
derive instance newtypeDescribeAuthorizerResponse :: Newtype DescribeAuthorizerResponse _
derive instance repGenericDescribeAuthorizerResponse :: Generic DescribeAuthorizerResponse _
instance showDescribeAuthorizerResponse :: Show DescribeAuthorizerResponse where
  show = genericShow
instance decodeDescribeAuthorizerResponse :: Decode DescribeAuthorizerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAuthorizerResponse :: Encode DescribeAuthorizerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DescribeCACertificate operation.</p>
newtype DescribeCACertificateRequest = DescribeCACertificateRequest 
  { "CertificateId'" :: (CertificateId)
  }
derive instance newtypeDescribeCACertificateRequest :: Newtype DescribeCACertificateRequest _
derive instance repGenericDescribeCACertificateRequest :: Generic DescribeCACertificateRequest _
instance showDescribeCACertificateRequest :: Show DescribeCACertificateRequest where
  show = genericShow
instance decodeDescribeCACertificateRequest :: Decode DescribeCACertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCACertificateRequest :: Encode DescribeCACertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the DescribeCACertificate operation.</p>
newtype DescribeCACertificateResponse = DescribeCACertificateResponse 
  { "CertificateDescription'" :: NullOrUndefined.NullOrUndefined (CACertificateDescription)
  , "RegistrationConfig'" :: NullOrUndefined.NullOrUndefined (RegistrationConfig)
  }
derive instance newtypeDescribeCACertificateResponse :: Newtype DescribeCACertificateResponse _
derive instance repGenericDescribeCACertificateResponse :: Generic DescribeCACertificateResponse _
instance showDescribeCACertificateResponse :: Show DescribeCACertificateResponse where
  show = genericShow
instance decodeDescribeCACertificateResponse :: Decode DescribeCACertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCACertificateResponse :: Encode DescribeCACertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DescribeCertificate operation.</p>
newtype DescribeCertificateRequest = DescribeCertificateRequest 
  { "CertificateId'" :: (CertificateId)
  }
derive instance newtypeDescribeCertificateRequest :: Newtype DescribeCertificateRequest _
derive instance repGenericDescribeCertificateRequest :: Generic DescribeCertificateRequest _
instance showDescribeCertificateRequest :: Show DescribeCertificateRequest where
  show = genericShow
instance decodeDescribeCertificateRequest :: Decode DescribeCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCertificateRequest :: Encode DescribeCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output of the DescribeCertificate operation.</p>
newtype DescribeCertificateResponse = DescribeCertificateResponse 
  { "CertificateDescription'" :: NullOrUndefined.NullOrUndefined (CertificateDescription)
  }
derive instance newtypeDescribeCertificateResponse :: Newtype DescribeCertificateResponse _
derive instance repGenericDescribeCertificateResponse :: Generic DescribeCertificateResponse _
instance showDescribeCertificateResponse :: Show DescribeCertificateResponse where
  show = genericShow
instance decodeDescribeCertificateResponse :: Decode DescribeCertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCertificateResponse :: Encode DescribeCertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDefaultAuthorizerRequest = DescribeDefaultAuthorizerRequest Types.NoArguments
derive instance newtypeDescribeDefaultAuthorizerRequest :: Newtype DescribeDefaultAuthorizerRequest _
derive instance repGenericDescribeDefaultAuthorizerRequest :: Generic DescribeDefaultAuthorizerRequest _
instance showDescribeDefaultAuthorizerRequest :: Show DescribeDefaultAuthorizerRequest where
  show = genericShow
instance decodeDescribeDefaultAuthorizerRequest :: Decode DescribeDefaultAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDefaultAuthorizerRequest :: Encode DescribeDefaultAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDefaultAuthorizerResponse = DescribeDefaultAuthorizerResponse 
  { "AuthorizerDescription'" :: NullOrUndefined.NullOrUndefined (AuthorizerDescription)
  }
derive instance newtypeDescribeDefaultAuthorizerResponse :: Newtype DescribeDefaultAuthorizerResponse _
derive instance repGenericDescribeDefaultAuthorizerResponse :: Generic DescribeDefaultAuthorizerResponse _
instance showDescribeDefaultAuthorizerResponse :: Show DescribeDefaultAuthorizerResponse where
  show = genericShow
instance decodeDescribeDefaultAuthorizerResponse :: Decode DescribeDefaultAuthorizerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDefaultAuthorizerResponse :: Encode DescribeDefaultAuthorizerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DescribeEndpoint operation.</p>
newtype DescribeEndpointRequest = DescribeEndpointRequest 
  { "EndpointType'" :: NullOrUndefined.NullOrUndefined (EndpointType)
  }
derive instance newtypeDescribeEndpointRequest :: Newtype DescribeEndpointRequest _
derive instance repGenericDescribeEndpointRequest :: Generic DescribeEndpointRequest _
instance showDescribeEndpointRequest :: Show DescribeEndpointRequest where
  show = genericShow
instance decodeDescribeEndpointRequest :: Decode DescribeEndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEndpointRequest :: Encode DescribeEndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the DescribeEndpoint operation.</p>
newtype DescribeEndpointResponse = DescribeEndpointResponse 
  { "EndpointAddress'" :: NullOrUndefined.NullOrUndefined (EndpointAddress)
  }
derive instance newtypeDescribeEndpointResponse :: Newtype DescribeEndpointResponse _
derive instance repGenericDescribeEndpointResponse :: Generic DescribeEndpointResponse _
instance showDescribeEndpointResponse :: Show DescribeEndpointResponse where
  show = genericShow
instance decodeDescribeEndpointResponse :: Decode DescribeEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEndpointResponse :: Encode DescribeEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventConfigurationsRequest = DescribeEventConfigurationsRequest Types.NoArguments
derive instance newtypeDescribeEventConfigurationsRequest :: Newtype DescribeEventConfigurationsRequest _
derive instance repGenericDescribeEventConfigurationsRequest :: Generic DescribeEventConfigurationsRequest _
instance showDescribeEventConfigurationsRequest :: Show DescribeEventConfigurationsRequest where
  show = genericShow
instance decodeDescribeEventConfigurationsRequest :: Decode DescribeEventConfigurationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventConfigurationsRequest :: Encode DescribeEventConfigurationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventConfigurationsResponse = DescribeEventConfigurationsResponse 
  { "EventConfigurations'" :: NullOrUndefined.NullOrUndefined (EventConfigurations)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (CreationDate)
  , "LastModifiedDate'" :: NullOrUndefined.NullOrUndefined (LastModifiedDate)
  }
derive instance newtypeDescribeEventConfigurationsResponse :: Newtype DescribeEventConfigurationsResponse _
derive instance repGenericDescribeEventConfigurationsResponse :: Generic DescribeEventConfigurationsResponse _
instance showDescribeEventConfigurationsResponse :: Show DescribeEventConfigurationsResponse where
  show = genericShow
instance decodeDescribeEventConfigurationsResponse :: Decode DescribeEventConfigurationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventConfigurationsResponse :: Encode DescribeEventConfigurationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeIndexRequest = DescribeIndexRequest 
  { "IndexName'" :: (IndexName)
  }
derive instance newtypeDescribeIndexRequest :: Newtype DescribeIndexRequest _
derive instance repGenericDescribeIndexRequest :: Generic DescribeIndexRequest _
instance showDescribeIndexRequest :: Show DescribeIndexRequest where
  show = genericShow
instance decodeDescribeIndexRequest :: Decode DescribeIndexRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeIndexRequest :: Encode DescribeIndexRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeIndexResponse = DescribeIndexResponse 
  { "IndexName'" :: NullOrUndefined.NullOrUndefined (IndexName)
  , "IndexStatus'" :: NullOrUndefined.NullOrUndefined (IndexStatus)
  , "Schema'" :: NullOrUndefined.NullOrUndefined (IndexSchema)
  }
derive instance newtypeDescribeIndexResponse :: Newtype DescribeIndexResponse _
derive instance repGenericDescribeIndexResponse :: Generic DescribeIndexResponse _
instance showDescribeIndexResponse :: Show DescribeIndexResponse where
  show = genericShow
instance decodeDescribeIndexResponse :: Decode DescribeIndexResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeIndexResponse :: Encode DescribeIndexResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobExecutionRequest = DescribeJobExecutionRequest 
  { "JobId'" :: (JobId)
  , "ThingName'" :: (ThingName)
  , "ExecutionNumber'" :: NullOrUndefined.NullOrUndefined (ExecutionNumber)
  }
derive instance newtypeDescribeJobExecutionRequest :: Newtype DescribeJobExecutionRequest _
derive instance repGenericDescribeJobExecutionRequest :: Generic DescribeJobExecutionRequest _
instance showDescribeJobExecutionRequest :: Show DescribeJobExecutionRequest where
  show = genericShow
instance decodeDescribeJobExecutionRequest :: Decode DescribeJobExecutionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobExecutionRequest :: Encode DescribeJobExecutionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobExecutionResponse = DescribeJobExecutionResponse 
  { "Execution'" :: NullOrUndefined.NullOrUndefined (JobExecution)
  }
derive instance newtypeDescribeJobExecutionResponse :: Newtype DescribeJobExecutionResponse _
derive instance repGenericDescribeJobExecutionResponse :: Generic DescribeJobExecutionResponse _
instance showDescribeJobExecutionResponse :: Show DescribeJobExecutionResponse where
  show = genericShow
instance decodeDescribeJobExecutionResponse :: Decode DescribeJobExecutionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobExecutionResponse :: Encode DescribeJobExecutionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobRequest = DescribeJobRequest 
  { "JobId'" :: (JobId)
  }
derive instance newtypeDescribeJobRequest :: Newtype DescribeJobRequest _
derive instance repGenericDescribeJobRequest :: Generic DescribeJobRequest _
instance showDescribeJobRequest :: Show DescribeJobRequest where
  show = genericShow
instance decodeDescribeJobRequest :: Decode DescribeJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobRequest :: Encode DescribeJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobResponse = DescribeJobResponse 
  { "DocumentSource'" :: NullOrUndefined.NullOrUndefined (JobDocumentSource)
  , "Job'" :: NullOrUndefined.NullOrUndefined (Job)
  }
derive instance newtypeDescribeJobResponse :: Newtype DescribeJobResponse _
derive instance repGenericDescribeJobResponse :: Generic DescribeJobResponse _
instance showDescribeJobResponse :: Show DescribeJobResponse where
  show = genericShow
instance decodeDescribeJobResponse :: Decode DescribeJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobResponse :: Encode DescribeJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeRoleAliasRequest = DescribeRoleAliasRequest 
  { "RoleAlias'" :: (RoleAlias)
  }
derive instance newtypeDescribeRoleAliasRequest :: Newtype DescribeRoleAliasRequest _
derive instance repGenericDescribeRoleAliasRequest :: Generic DescribeRoleAliasRequest _
instance showDescribeRoleAliasRequest :: Show DescribeRoleAliasRequest where
  show = genericShow
instance decodeDescribeRoleAliasRequest :: Decode DescribeRoleAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRoleAliasRequest :: Encode DescribeRoleAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeRoleAliasResponse = DescribeRoleAliasResponse 
  { "RoleAliasDescription'" :: NullOrUndefined.NullOrUndefined (RoleAliasDescription)
  }
derive instance newtypeDescribeRoleAliasResponse :: Newtype DescribeRoleAliasResponse _
derive instance repGenericDescribeRoleAliasResponse :: Generic DescribeRoleAliasResponse _
instance showDescribeRoleAliasResponse :: Show DescribeRoleAliasResponse where
  show = genericShow
instance decodeDescribeRoleAliasResponse :: Decode DescribeRoleAliasResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRoleAliasResponse :: Encode DescribeRoleAliasResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStreamRequest = DescribeStreamRequest 
  { "StreamId'" :: (StreamId)
  }
derive instance newtypeDescribeStreamRequest :: Newtype DescribeStreamRequest _
derive instance repGenericDescribeStreamRequest :: Generic DescribeStreamRequest _
instance showDescribeStreamRequest :: Show DescribeStreamRequest where
  show = genericShow
instance decodeDescribeStreamRequest :: Decode DescribeStreamRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStreamRequest :: Encode DescribeStreamRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStreamResponse = DescribeStreamResponse 
  { "StreamInfo'" :: NullOrUndefined.NullOrUndefined (StreamInfo)
  }
derive instance newtypeDescribeStreamResponse :: Newtype DescribeStreamResponse _
derive instance repGenericDescribeStreamResponse :: Generic DescribeStreamResponse _
instance showDescribeStreamResponse :: Show DescribeStreamResponse where
  show = genericShow
instance decodeDescribeStreamResponse :: Decode DescribeStreamResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStreamResponse :: Encode DescribeStreamResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeThingGroupRequest = DescribeThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  }
derive instance newtypeDescribeThingGroupRequest :: Newtype DescribeThingGroupRequest _
derive instance repGenericDescribeThingGroupRequest :: Generic DescribeThingGroupRequest _
instance showDescribeThingGroupRequest :: Show DescribeThingGroupRequest where
  show = genericShow
instance decodeDescribeThingGroupRequest :: Decode DescribeThingGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeThingGroupRequest :: Encode DescribeThingGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeThingGroupResponse = DescribeThingGroupResponse 
  { "ThingGroupName'" :: NullOrUndefined.NullOrUndefined (ThingGroupName)
  , "ThingGroupId'" :: NullOrUndefined.NullOrUndefined (ThingGroupId)
  , "ThingGroupArn'" :: NullOrUndefined.NullOrUndefined (ThingGroupArn)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  , "ThingGroupProperties'" :: NullOrUndefined.NullOrUndefined (ThingGroupProperties)
  , "ThingGroupMetadata'" :: NullOrUndefined.NullOrUndefined (ThingGroupMetadata)
  }
derive instance newtypeDescribeThingGroupResponse :: Newtype DescribeThingGroupResponse _
derive instance repGenericDescribeThingGroupResponse :: Generic DescribeThingGroupResponse _
instance showDescribeThingGroupResponse :: Show DescribeThingGroupResponse where
  show = genericShow
instance decodeDescribeThingGroupResponse :: Decode DescribeThingGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeThingGroupResponse :: Encode DescribeThingGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeThingRegistrationTaskRequest = DescribeThingRegistrationTaskRequest 
  { "TaskId'" :: (TaskId)
  }
derive instance newtypeDescribeThingRegistrationTaskRequest :: Newtype DescribeThingRegistrationTaskRequest _
derive instance repGenericDescribeThingRegistrationTaskRequest :: Generic DescribeThingRegistrationTaskRequest _
instance showDescribeThingRegistrationTaskRequest :: Show DescribeThingRegistrationTaskRequest where
  show = genericShow
instance decodeDescribeThingRegistrationTaskRequest :: Decode DescribeThingRegistrationTaskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeThingRegistrationTaskRequest :: Encode DescribeThingRegistrationTaskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeThingRegistrationTaskResponse = DescribeThingRegistrationTaskResponse 
  { "TaskId'" :: NullOrUndefined.NullOrUndefined (TaskId)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (CreationDate)
  , "LastModifiedDate'" :: NullOrUndefined.NullOrUndefined (LastModifiedDate)
  , "TemplateBody'" :: NullOrUndefined.NullOrUndefined (TemplateBody)
  , "InputFileBucket'" :: NullOrUndefined.NullOrUndefined (RegistryS3BucketName)
  , "InputFileKey'" :: NullOrUndefined.NullOrUndefined (RegistryS3KeyName)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (RoleArn)
  , "Status'" :: NullOrUndefined.NullOrUndefined (Status)
  , "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  , "SuccessCount'" :: NullOrUndefined.NullOrUndefined (Count)
  , "FailureCount'" :: NullOrUndefined.NullOrUndefined (Count)
  , "PercentageProgress'" :: NullOrUndefined.NullOrUndefined (Percentage)
  }
derive instance newtypeDescribeThingRegistrationTaskResponse :: Newtype DescribeThingRegistrationTaskResponse _
derive instance repGenericDescribeThingRegistrationTaskResponse :: Generic DescribeThingRegistrationTaskResponse _
instance showDescribeThingRegistrationTaskResponse :: Show DescribeThingRegistrationTaskResponse where
  show = genericShow
instance decodeDescribeThingRegistrationTaskResponse :: Decode DescribeThingRegistrationTaskResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeThingRegistrationTaskResponse :: Encode DescribeThingRegistrationTaskResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DescribeThing operation.</p>
newtype DescribeThingRequest = DescribeThingRequest 
  { "ThingName'" :: (ThingName)
  }
derive instance newtypeDescribeThingRequest :: Newtype DescribeThingRequest _
derive instance repGenericDescribeThingRequest :: Generic DescribeThingRequest _
instance showDescribeThingRequest :: Show DescribeThingRequest where
  show = genericShow
instance decodeDescribeThingRequest :: Decode DescribeThingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeThingRequest :: Encode DescribeThingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the DescribeThing operation.</p>
newtype DescribeThingResponse = DescribeThingResponse 
  { "DefaultClientId'" :: NullOrUndefined.NullOrUndefined (ClientId)
  , "ThingName'" :: NullOrUndefined.NullOrUndefined (ThingName)
  , "ThingId'" :: NullOrUndefined.NullOrUndefined (ThingId)
  , "ThingArn'" :: NullOrUndefined.NullOrUndefined (ThingArn)
  , "ThingTypeName'" :: NullOrUndefined.NullOrUndefined (ThingTypeName)
  , "Attributes'" :: NullOrUndefined.NullOrUndefined (Attributes)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  }
derive instance newtypeDescribeThingResponse :: Newtype DescribeThingResponse _
derive instance repGenericDescribeThingResponse :: Generic DescribeThingResponse _
instance showDescribeThingResponse :: Show DescribeThingResponse where
  show = genericShow
instance decodeDescribeThingResponse :: Decode DescribeThingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeThingResponse :: Encode DescribeThingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DescribeThingType operation.</p>
newtype DescribeThingTypeRequest = DescribeThingTypeRequest 
  { "ThingTypeName'" :: (ThingTypeName)
  }
derive instance newtypeDescribeThingTypeRequest :: Newtype DescribeThingTypeRequest _
derive instance repGenericDescribeThingTypeRequest :: Generic DescribeThingTypeRequest _
instance showDescribeThingTypeRequest :: Show DescribeThingTypeRequest where
  show = genericShow
instance decodeDescribeThingTypeRequest :: Decode DescribeThingTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeThingTypeRequest :: Encode DescribeThingTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the DescribeThingType operation.</p>
newtype DescribeThingTypeResponse = DescribeThingTypeResponse 
  { "ThingTypeName'" :: NullOrUndefined.NullOrUndefined (ThingTypeName)
  , "ThingTypeId'" :: NullOrUndefined.NullOrUndefined (ThingTypeId)
  , "ThingTypeArn'" :: NullOrUndefined.NullOrUndefined (ThingTypeArn)
  , "ThingTypeProperties'" :: NullOrUndefined.NullOrUndefined (ThingTypeProperties)
  , "ThingTypeMetadata'" :: NullOrUndefined.NullOrUndefined (ThingTypeMetadata)
  }
derive instance newtypeDescribeThingTypeResponse :: Newtype DescribeThingTypeResponse _
derive instance repGenericDescribeThingTypeResponse :: Generic DescribeThingTypeResponse _
instance showDescribeThingTypeResponse :: Show DescribeThingTypeResponse where
  show = genericShow
instance decodeDescribeThingTypeResponse :: Decode DescribeThingTypeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeThingTypeResponse :: Encode DescribeThingTypeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _
derive instance repGenericDescription :: Generic Description _
instance showDescription :: Show Description where
  show = genericShow
instance decodeDescription :: Decode Description where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescription :: Encode Description where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetachPolicyRequest = DetachPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Target'" :: (PolicyTarget)
  }
derive instance newtypeDetachPolicyRequest :: Newtype DetachPolicyRequest _
derive instance repGenericDetachPolicyRequest :: Generic DetachPolicyRequest _
instance showDetachPolicyRequest :: Show DetachPolicyRequest where
  show = genericShow
instance decodeDetachPolicyRequest :: Decode DetachPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachPolicyRequest :: Encode DetachPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DetachPrincipalPolicy operation.</p>
newtype DetachPrincipalPolicyRequest = DetachPrincipalPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Principal'" :: (Principal)
  }
derive instance newtypeDetachPrincipalPolicyRequest :: Newtype DetachPrincipalPolicyRequest _
derive instance repGenericDetachPrincipalPolicyRequest :: Generic DetachPrincipalPolicyRequest _
instance showDetachPrincipalPolicyRequest :: Show DetachPrincipalPolicyRequest where
  show = genericShow
instance decodeDetachPrincipalPolicyRequest :: Decode DetachPrincipalPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachPrincipalPolicyRequest :: Encode DetachPrincipalPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DetachThingPrincipal operation.</p>
newtype DetachThingPrincipalRequest = DetachThingPrincipalRequest 
  { "ThingName'" :: (ThingName)
  , "Principal'" :: (Principal)
  }
derive instance newtypeDetachThingPrincipalRequest :: Newtype DetachThingPrincipalRequest _
derive instance repGenericDetachThingPrincipalRequest :: Generic DetachThingPrincipalRequest _
instance showDetachThingPrincipalRequest :: Show DetachThingPrincipalRequest where
  show = genericShow
instance decodeDetachThingPrincipalRequest :: Decode DetachThingPrincipalRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachThingPrincipalRequest :: Encode DetachThingPrincipalRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the DetachThingPrincipal operation.</p>
newtype DetachThingPrincipalResponse = DetachThingPrincipalResponse Types.NoArguments
derive instance newtypeDetachThingPrincipalResponse :: Newtype DetachThingPrincipalResponse _
derive instance repGenericDetachThingPrincipalResponse :: Generic DetachThingPrincipalResponse _
instance showDetachThingPrincipalResponse :: Show DetachThingPrincipalResponse where
  show = genericShow
instance decodeDetachThingPrincipalResponse :: Decode DetachThingPrincipalResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachThingPrincipalResponse :: Encode DetachThingPrincipalResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetailsKey = DetailsKey String
derive instance newtypeDetailsKey :: Newtype DetailsKey _
derive instance repGenericDetailsKey :: Generic DetailsKey _
instance showDetailsKey :: Show DetailsKey where
  show = genericShow
instance decodeDetailsKey :: Decode DetailsKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetailsKey :: Encode DetailsKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetailsMap = DetailsMap (StrMap.StrMap DetailsValue)
derive instance newtypeDetailsMap :: Newtype DetailsMap _
derive instance repGenericDetailsMap :: Generic DetailsMap _
instance showDetailsMap :: Show DetailsMap where
  show = genericShow
instance decodeDetailsMap :: Decode DetailsMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetailsMap :: Encode DetailsMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetailsValue = DetailsValue String
derive instance newtypeDetailsValue :: Newtype DetailsValue _
derive instance repGenericDetailsValue :: Generic DetailsValue _
instance showDetailsValue :: Show DetailsValue where
  show = genericShow
instance decodeDetailsValue :: Decode DetailsValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetailsValue :: Encode DetailsValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisableAllLogs = DisableAllLogs Boolean
derive instance newtypeDisableAllLogs :: Newtype DisableAllLogs _
derive instance repGenericDisableAllLogs :: Generic DisableAllLogs _
instance showDisableAllLogs :: Show DisableAllLogs where
  show = genericShow
instance decodeDisableAllLogs :: Decode DisableAllLogs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableAllLogs :: Encode DisableAllLogs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DisableTopicRuleRequest operation.</p>
newtype DisableTopicRuleRequest = DisableTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  }
derive instance newtypeDisableTopicRuleRequest :: Newtype DisableTopicRuleRequest _
derive instance repGenericDisableTopicRuleRequest :: Generic DisableTopicRuleRequest _
instance showDisableTopicRuleRequest :: Show DisableTopicRuleRequest where
  show = genericShow
instance decodeDisableTopicRuleRequest :: Decode DisableTopicRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableTopicRuleRequest :: Encode DisableTopicRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action to write to a DynamoDB table.</p> <p>The <code>tableName</code>, <code>hashKeyField</code>, and <code>rangeKeyField</code> values must match the values used when you created the table.</p> <p>The <code>hashKeyValue</code> and <code>rangeKeyvalue</code> fields use a substitution template syntax. These templates provide data at runtime. The syntax is as follows: ${<i>sql-expression</i>}.</p> <p>You can specify any valid expression in a WHERE or SELECT clause, including JSON properties, comparisons, calculations, and functions. For example, the following field uses the third level of the topic:</p> <p> <code>"hashKeyValue": "${topic(3)}"</code> </p> <p>The following field uses the timestamp:</p> <p> <code>"rangeKeyValue": "${timestamp()}"</code> </p>
newtype DynamoDBAction = DynamoDBAction 
  { "TableName'" :: (TableName)
  , "RoleArn'" :: (AwsArn)
  , "Operation'" :: NullOrUndefined.NullOrUndefined (DynamoOperation)
  , "HashKeyField'" :: (HashKeyField)
  , "HashKeyValue'" :: (HashKeyValue)
  , "HashKeyType'" :: NullOrUndefined.NullOrUndefined (DynamoKeyType)
  , "RangeKeyField'" :: NullOrUndefined.NullOrUndefined (RangeKeyField)
  , "RangeKeyValue'" :: NullOrUndefined.NullOrUndefined (RangeKeyValue)
  , "RangeKeyType'" :: NullOrUndefined.NullOrUndefined (DynamoKeyType)
  , "PayloadField'" :: NullOrUndefined.NullOrUndefined (PayloadField)
  }
derive instance newtypeDynamoDBAction :: Newtype DynamoDBAction _
derive instance repGenericDynamoDBAction :: Generic DynamoDBAction _
instance showDynamoDBAction :: Show DynamoDBAction where
  show = genericShow
instance decodeDynamoDBAction :: Decode DynamoDBAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDynamoDBAction :: Encode DynamoDBAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action to write to a DynamoDB table.</p> <p>This DynamoDB action writes each attribute in the message payload into it's own column in the DynamoDB table.</p>
newtype DynamoDBv2Action = DynamoDBv2Action 
  { "RoleArn'" :: NullOrUndefined.NullOrUndefined (AwsArn)
  , "PutItem'" :: NullOrUndefined.NullOrUndefined (PutItemInput)
  }
derive instance newtypeDynamoDBv2Action :: Newtype DynamoDBv2Action _
derive instance repGenericDynamoDBv2Action :: Generic DynamoDBv2Action _
instance showDynamoDBv2Action :: Show DynamoDBv2Action where
  show = genericShow
instance decodeDynamoDBv2Action :: Decode DynamoDBv2Action where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDynamoDBv2Action :: Encode DynamoDBv2Action where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DynamoKeyType = DynamoKeyType String
derive instance newtypeDynamoKeyType :: Newtype DynamoKeyType _
derive instance repGenericDynamoKeyType :: Generic DynamoKeyType _
instance showDynamoKeyType :: Show DynamoKeyType where
  show = genericShow
instance decodeDynamoKeyType :: Decode DynamoKeyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDynamoKeyType :: Encode DynamoKeyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DynamoOperation = DynamoOperation String
derive instance newtypeDynamoOperation :: Newtype DynamoOperation _
derive instance repGenericDynamoOperation :: Generic DynamoOperation _
instance showDynamoOperation :: Show DynamoOperation where
  show = genericShow
instance decodeDynamoOperation :: Decode DynamoOperation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDynamoOperation :: Encode DynamoOperation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EffectivePolicies = EffectivePolicies (Array EffectivePolicy)
derive instance newtypeEffectivePolicies :: Newtype EffectivePolicies _
derive instance repGenericEffectivePolicies :: Generic EffectivePolicies _
instance showEffectivePolicies :: Show EffectivePolicies where
  show = genericShow
instance decodeEffectivePolicies :: Decode EffectivePolicies where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEffectivePolicies :: Encode EffectivePolicies where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The policy that has the effect on the authorization results.</p>
newtype EffectivePolicy = EffectivePolicy 
  { "PolicyName'" :: NullOrUndefined.NullOrUndefined (PolicyName)
  , "PolicyArn'" :: NullOrUndefined.NullOrUndefined (PolicyArn)
  , "PolicyDocument'" :: NullOrUndefined.NullOrUndefined (PolicyDocument)
  }
derive instance newtypeEffectivePolicy :: Newtype EffectivePolicy _
derive instance repGenericEffectivePolicy :: Generic EffectivePolicy _
instance showEffectivePolicy :: Show EffectivePolicy where
  show = genericShow
instance decodeEffectivePolicy :: Decode EffectivePolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEffectivePolicy :: Encode EffectivePolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action that writes data to an Amazon Elasticsearch Service domain.</p>
newtype ElasticsearchAction = ElasticsearchAction 
  { "RoleArn'" :: (AwsArn)
  , "Endpoint'" :: (ElasticsearchEndpoint)
  , "Index'" :: (ElasticsearchIndex)
  , "Type'" :: (ElasticsearchType)
  , "Id'" :: (ElasticsearchId)
  }
derive instance newtypeElasticsearchAction :: Newtype ElasticsearchAction _
derive instance repGenericElasticsearchAction :: Generic ElasticsearchAction _
instance showElasticsearchAction :: Show ElasticsearchAction where
  show = genericShow
instance decodeElasticsearchAction :: Decode ElasticsearchAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchAction :: Encode ElasticsearchAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchEndpoint = ElasticsearchEndpoint String
derive instance newtypeElasticsearchEndpoint :: Newtype ElasticsearchEndpoint _
derive instance repGenericElasticsearchEndpoint :: Generic ElasticsearchEndpoint _
instance showElasticsearchEndpoint :: Show ElasticsearchEndpoint where
  show = genericShow
instance decodeElasticsearchEndpoint :: Decode ElasticsearchEndpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchEndpoint :: Encode ElasticsearchEndpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchId = ElasticsearchId String
derive instance newtypeElasticsearchId :: Newtype ElasticsearchId _
derive instance repGenericElasticsearchId :: Generic ElasticsearchId _
instance showElasticsearchId :: Show ElasticsearchId where
  show = genericShow
instance decodeElasticsearchId :: Decode ElasticsearchId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchId :: Encode ElasticsearchId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchIndex = ElasticsearchIndex String
derive instance newtypeElasticsearchIndex :: Newtype ElasticsearchIndex _
derive instance repGenericElasticsearchIndex :: Generic ElasticsearchIndex _
instance showElasticsearchIndex :: Show ElasticsearchIndex where
  show = genericShow
instance decodeElasticsearchIndex :: Decode ElasticsearchIndex where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchIndex :: Encode ElasticsearchIndex where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchType = ElasticsearchType String
derive instance newtypeElasticsearchType :: Newtype ElasticsearchType _
derive instance repGenericElasticsearchType :: Generic ElasticsearchType _
instance showElasticsearchType :: Show ElasticsearchType where
  show = genericShow
instance decodeElasticsearchType :: Decode ElasticsearchType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchType :: Encode ElasticsearchType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the EnableTopicRuleRequest operation.</p>
newtype EnableTopicRuleRequest = EnableTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  }
derive instance newtypeEnableTopicRuleRequest :: Newtype EnableTopicRuleRequest _
derive instance repGenericEnableTopicRuleRequest :: Generic EnableTopicRuleRequest _
instance showEnableTopicRuleRequest :: Show EnableTopicRuleRequest where
  show = genericShow
instance decodeEnableTopicRuleRequest :: Decode EnableTopicRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableTopicRuleRequest :: Encode EnableTopicRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Enabled = Enabled Boolean
derive instance newtypeEnabled :: Newtype Enabled _
derive instance repGenericEnabled :: Generic Enabled _
instance showEnabled :: Show Enabled where
  show = genericShow
instance decodeEnabled :: Decode Enabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnabled :: Encode Enabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EndpointAddress = EndpointAddress String
derive instance newtypeEndpointAddress :: Newtype EndpointAddress _
derive instance repGenericEndpointAddress :: Generic EndpointAddress _
instance showEndpointAddress :: Show EndpointAddress where
  show = genericShow
instance decodeEndpointAddress :: Decode EndpointAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointAddress :: Encode EndpointAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EndpointType = EndpointType String
derive instance newtypeEndpointType :: Newtype EndpointType _
derive instance repGenericEndpointType :: Generic EndpointType _
instance showEndpointType :: Show EndpointType where
  show = genericShow
instance decodeEndpointType :: Decode EndpointType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointType :: Encode EndpointType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Error information.</p>
newtype ErrorInfo = ErrorInfo 
  { "Code'" :: NullOrUndefined.NullOrUndefined (Code)
  , "Message'" :: NullOrUndefined.NullOrUndefined (OTAUpdateErrorMessage)
  }
derive instance newtypeErrorInfo :: Newtype ErrorInfo _
derive instance repGenericErrorInfo :: Generic ErrorInfo _
instance showErrorInfo :: Show ErrorInfo where
  show = genericShow
instance decodeErrorInfo :: Decode ErrorInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorInfo :: Encode ErrorInfo where
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


newtype EventConfigurations = EventConfigurations (StrMap.StrMap Configuration)
derive instance newtypeEventConfigurations :: Newtype EventConfigurations _
derive instance repGenericEventConfigurations :: Generic EventConfigurations _
instance showEventConfigurations :: Show EventConfigurations where
  show = genericShow
instance decodeEventConfigurations :: Decode EventConfigurations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventConfigurations :: Encode EventConfigurations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventType = EventType String
derive instance newtypeEventType :: Newtype EventType _
derive instance repGenericEventType :: Generic EventType _
instance showEventType :: Show EventType where
  show = genericShow
instance decodeEventType :: Decode EventType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventType :: Encode EventType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExecutionNumber = ExecutionNumber Number
derive instance newtypeExecutionNumber :: Newtype ExecutionNumber _
derive instance repGenericExecutionNumber :: Generic ExecutionNumber _
instance showExecutionNumber :: Show ExecutionNumber where
  show = genericShow
instance decodeExecutionNumber :: Decode ExecutionNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionNumber :: Encode ExecutionNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExpiresInSec = ExpiresInSec Number
derive instance newtypeExpiresInSec :: Newtype ExpiresInSec _
derive instance repGenericExpiresInSec :: Generic ExpiresInSec _
instance showExpiresInSec :: Show ExpiresInSec where
  show = genericShow
instance decodeExpiresInSec :: Decode ExpiresInSec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpiresInSec :: Encode ExpiresInSec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information that explicitly denies authorization.</p>
newtype ExplicitDeny = ExplicitDeny 
  { "Policies'" :: NullOrUndefined.NullOrUndefined (Policies)
  }
derive instance newtypeExplicitDeny :: Newtype ExplicitDeny _
derive instance repGenericExplicitDeny :: Generic ExplicitDeny _
instance showExplicitDeny :: Show ExplicitDeny where
  show = genericShow
instance decodeExplicitDeny :: Decode ExplicitDeny where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExplicitDeny :: Encode ExplicitDeny where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailedThings = FailedThings Int
derive instance newtypeFailedThings :: Newtype FailedThings _
derive instance repGenericFailedThings :: Generic FailedThings _
instance showFailedThings :: Show FailedThings where
  show = genericShow
instance decodeFailedThings :: Decode FailedThings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedThings :: Encode FailedThings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FileId = FileId Int
derive instance newtypeFileId :: Newtype FileId _
derive instance repGenericFileId :: Generic FileId _
instance showFileId :: Show FileId where
  show = genericShow
instance decodeFileId :: Decode FileId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileId :: Encode FileId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FileName = FileName String
derive instance newtypeFileName :: Newtype FileName _
derive instance repGenericFileName :: Generic FileName _
instance showFileName :: Show FileName where
  show = genericShow
instance decodeFileName :: Decode FileName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileName :: Encode FileName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action that writes data to an Amazon Kinesis Firehose stream.</p>
newtype FirehoseAction = FirehoseAction 
  { "RoleArn'" :: (AwsArn)
  , "DeliveryStreamName'" :: (DeliveryStreamName)
  , "Separator'" :: NullOrUndefined.NullOrUndefined (FirehoseSeparator)
  }
derive instance newtypeFirehoseAction :: Newtype FirehoseAction _
derive instance repGenericFirehoseAction :: Generic FirehoseAction _
instance showFirehoseAction :: Show FirehoseAction where
  show = genericShow
instance decodeFirehoseAction :: Decode FirehoseAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFirehoseAction :: Encode FirehoseAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FirehoseSeparator = FirehoseSeparator String
derive instance newtypeFirehoseSeparator :: Newtype FirehoseSeparator _
derive instance repGenericFirehoseSeparator :: Generic FirehoseSeparator _
instance showFirehoseSeparator :: Show FirehoseSeparator where
  show = genericShow
instance decodeFirehoseSeparator :: Decode FirehoseSeparator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFirehoseSeparator :: Encode FirehoseSeparator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Flag = Flag Boolean
derive instance newtypeFlag :: Newtype Flag _
derive instance repGenericFlag :: Generic Flag _
instance showFlag :: Show Flag where
  show = genericShow
instance decodeFlag :: Decode Flag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFlag :: Encode Flag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ForceDelete = ForceDelete Boolean
derive instance newtypeForceDelete :: Newtype ForceDelete _
derive instance repGenericForceDelete :: Generic ForceDelete _
instance showForceDelete :: Show ForceDelete where
  show = genericShow
instance decodeForceDelete :: Decode ForceDelete where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeForceDelete :: Encode ForceDelete where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FunctionArn = FunctionArn String
derive instance newtypeFunctionArn :: Newtype FunctionArn _
derive instance repGenericFunctionArn :: Generic FunctionArn _
instance showFunctionArn :: Show FunctionArn where
  show = genericShow
instance decodeFunctionArn :: Decode FunctionArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFunctionArn :: Encode FunctionArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GEMaxResults = GEMaxResults Int
derive instance newtypeGEMaxResults :: Newtype GEMaxResults _
derive instance repGenericGEMaxResults :: Generic GEMaxResults _
instance showGEMaxResults :: Show GEMaxResults where
  show = genericShow
instance decodeGEMaxResults :: Decode GEMaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGEMaxResults :: Encode GEMaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetEffectivePoliciesRequest = GetEffectivePoliciesRequest 
  { "Principal'" :: NullOrUndefined.NullOrUndefined (Principal)
  , "CognitoIdentityPoolId'" :: NullOrUndefined.NullOrUndefined (CognitoIdentityPoolId)
  , "ThingName'" :: NullOrUndefined.NullOrUndefined (ThingName)
  }
derive instance newtypeGetEffectivePoliciesRequest :: Newtype GetEffectivePoliciesRequest _
derive instance repGenericGetEffectivePoliciesRequest :: Generic GetEffectivePoliciesRequest _
instance showGetEffectivePoliciesRequest :: Show GetEffectivePoliciesRequest where
  show = genericShow
instance decodeGetEffectivePoliciesRequest :: Decode GetEffectivePoliciesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEffectivePoliciesRequest :: Encode GetEffectivePoliciesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetEffectivePoliciesResponse = GetEffectivePoliciesResponse 
  { "EffectivePolicies'" :: NullOrUndefined.NullOrUndefined (EffectivePolicies)
  }
derive instance newtypeGetEffectivePoliciesResponse :: Newtype GetEffectivePoliciesResponse _
derive instance repGenericGetEffectivePoliciesResponse :: Generic GetEffectivePoliciesResponse _
instance showGetEffectivePoliciesResponse :: Show GetEffectivePoliciesResponse where
  show = genericShow
instance decodeGetEffectivePoliciesResponse :: Decode GetEffectivePoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEffectivePoliciesResponse :: Encode GetEffectivePoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIndexingConfigurationRequest = GetIndexingConfigurationRequest Types.NoArguments
derive instance newtypeGetIndexingConfigurationRequest :: Newtype GetIndexingConfigurationRequest _
derive instance repGenericGetIndexingConfigurationRequest :: Generic GetIndexingConfigurationRequest _
instance showGetIndexingConfigurationRequest :: Show GetIndexingConfigurationRequest where
  show = genericShow
instance decodeGetIndexingConfigurationRequest :: Decode GetIndexingConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIndexingConfigurationRequest :: Encode GetIndexingConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIndexingConfigurationResponse = GetIndexingConfigurationResponse 
  { "ThingIndexingConfiguration'" :: NullOrUndefined.NullOrUndefined (ThingIndexingConfiguration)
  }
derive instance newtypeGetIndexingConfigurationResponse :: Newtype GetIndexingConfigurationResponse _
derive instance repGenericGetIndexingConfigurationResponse :: Generic GetIndexingConfigurationResponse _
instance showGetIndexingConfigurationResponse :: Show GetIndexingConfigurationResponse where
  show = genericShow
instance decodeGetIndexingConfigurationResponse :: Decode GetIndexingConfigurationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIndexingConfigurationResponse :: Encode GetIndexingConfigurationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobDocumentRequest = GetJobDocumentRequest 
  { "JobId'" :: (JobId)
  }
derive instance newtypeGetJobDocumentRequest :: Newtype GetJobDocumentRequest _
derive instance repGenericGetJobDocumentRequest :: Generic GetJobDocumentRequest _
instance showGetJobDocumentRequest :: Show GetJobDocumentRequest where
  show = genericShow
instance decodeGetJobDocumentRequest :: Decode GetJobDocumentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobDocumentRequest :: Encode GetJobDocumentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobDocumentResponse = GetJobDocumentResponse 
  { "Document'" :: NullOrUndefined.NullOrUndefined (JobDocument)
  }
derive instance newtypeGetJobDocumentResponse :: Newtype GetJobDocumentResponse _
derive instance repGenericGetJobDocumentResponse :: Generic GetJobDocumentResponse _
instance showGetJobDocumentResponse :: Show GetJobDocumentResponse where
  show = genericShow
instance decodeGetJobDocumentResponse :: Decode GetJobDocumentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobDocumentResponse :: Encode GetJobDocumentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the GetLoggingOptions operation.</p>
newtype GetLoggingOptionsRequest = GetLoggingOptionsRequest Types.NoArguments
derive instance newtypeGetLoggingOptionsRequest :: Newtype GetLoggingOptionsRequest _
derive instance repGenericGetLoggingOptionsRequest :: Generic GetLoggingOptionsRequest _
instance showGetLoggingOptionsRequest :: Show GetLoggingOptionsRequest where
  show = genericShow
instance decodeGetLoggingOptionsRequest :: Decode GetLoggingOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoggingOptionsRequest :: Encode GetLoggingOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the GetLoggingOptions operation.</p>
newtype GetLoggingOptionsResponse = GetLoggingOptionsResponse 
  { "RoleArn'" :: NullOrUndefined.NullOrUndefined (AwsArn)
  , "LogLevel'" :: NullOrUndefined.NullOrUndefined (LogLevel)
  }
derive instance newtypeGetLoggingOptionsResponse :: Newtype GetLoggingOptionsResponse _
derive instance repGenericGetLoggingOptionsResponse :: Generic GetLoggingOptionsResponse _
instance showGetLoggingOptionsResponse :: Show GetLoggingOptionsResponse where
  show = genericShow
instance decodeGetLoggingOptionsResponse :: Decode GetLoggingOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLoggingOptionsResponse :: Encode GetLoggingOptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetOTAUpdateRequest = GetOTAUpdateRequest 
  { "OtaUpdateId'" :: (OTAUpdateId)
  }
derive instance newtypeGetOTAUpdateRequest :: Newtype GetOTAUpdateRequest _
derive instance repGenericGetOTAUpdateRequest :: Generic GetOTAUpdateRequest _
instance showGetOTAUpdateRequest :: Show GetOTAUpdateRequest where
  show = genericShow
instance decodeGetOTAUpdateRequest :: Decode GetOTAUpdateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOTAUpdateRequest :: Encode GetOTAUpdateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetOTAUpdateResponse = GetOTAUpdateResponse 
  { "OtaUpdateInfo'" :: NullOrUndefined.NullOrUndefined (OTAUpdateInfo)
  }
derive instance newtypeGetOTAUpdateResponse :: Newtype GetOTAUpdateResponse _
derive instance repGenericGetOTAUpdateResponse :: Generic GetOTAUpdateResponse _
instance showGetOTAUpdateResponse :: Show GetOTAUpdateResponse where
  show = genericShow
instance decodeGetOTAUpdateResponse :: Decode GetOTAUpdateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOTAUpdateResponse :: Encode GetOTAUpdateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the GetPolicy operation.</p>
newtype GetPolicyRequest = GetPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  }
derive instance newtypeGetPolicyRequest :: Newtype GetPolicyRequest _
derive instance repGenericGetPolicyRequest :: Generic GetPolicyRequest _
instance showGetPolicyRequest :: Show GetPolicyRequest where
  show = genericShow
instance decodeGetPolicyRequest :: Decode GetPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPolicyRequest :: Encode GetPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the GetPolicy operation.</p>
newtype GetPolicyResponse = GetPolicyResponse 
  { "PolicyName'" :: NullOrUndefined.NullOrUndefined (PolicyName)
  , "PolicyArn'" :: NullOrUndefined.NullOrUndefined (PolicyArn)
  , "PolicyDocument'" :: NullOrUndefined.NullOrUndefined (PolicyDocument)
  , "DefaultVersionId'" :: NullOrUndefined.NullOrUndefined (PolicyVersionId)
  }
derive instance newtypeGetPolicyResponse :: Newtype GetPolicyResponse _
derive instance repGenericGetPolicyResponse :: Generic GetPolicyResponse _
instance showGetPolicyResponse :: Show GetPolicyResponse where
  show = genericShow
instance decodeGetPolicyResponse :: Decode GetPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPolicyResponse :: Encode GetPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the GetPolicyVersion operation.</p>
newtype GetPolicyVersionRequest = GetPolicyVersionRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyVersionId'" :: (PolicyVersionId)
  }
derive instance newtypeGetPolicyVersionRequest :: Newtype GetPolicyVersionRequest _
derive instance repGenericGetPolicyVersionRequest :: Generic GetPolicyVersionRequest _
instance showGetPolicyVersionRequest :: Show GetPolicyVersionRequest where
  show = genericShow
instance decodeGetPolicyVersionRequest :: Decode GetPolicyVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPolicyVersionRequest :: Encode GetPolicyVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the GetPolicyVersion operation.</p>
newtype GetPolicyVersionResponse = GetPolicyVersionResponse 
  { "PolicyArn'" :: NullOrUndefined.NullOrUndefined (PolicyArn)
  , "PolicyName'" :: NullOrUndefined.NullOrUndefined (PolicyName)
  , "PolicyDocument'" :: NullOrUndefined.NullOrUndefined (PolicyDocument)
  , "PolicyVersionId'" :: NullOrUndefined.NullOrUndefined (PolicyVersionId)
  , "IsDefaultVersion'" :: NullOrUndefined.NullOrUndefined (IsDefaultVersion)
  }
derive instance newtypeGetPolicyVersionResponse :: Newtype GetPolicyVersionResponse _
derive instance repGenericGetPolicyVersionResponse :: Generic GetPolicyVersionResponse _
instance showGetPolicyVersionResponse :: Show GetPolicyVersionResponse where
  show = genericShow
instance decodeGetPolicyVersionResponse :: Decode GetPolicyVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPolicyVersionResponse :: Encode GetPolicyVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input to the GetRegistrationCode operation.</p>
newtype GetRegistrationCodeRequest = GetRegistrationCodeRequest Types.NoArguments
derive instance newtypeGetRegistrationCodeRequest :: Newtype GetRegistrationCodeRequest _
derive instance repGenericGetRegistrationCodeRequest :: Generic GetRegistrationCodeRequest _
instance showGetRegistrationCodeRequest :: Show GetRegistrationCodeRequest where
  show = genericShow
instance decodeGetRegistrationCodeRequest :: Decode GetRegistrationCodeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRegistrationCodeRequest :: Encode GetRegistrationCodeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the GetRegistrationCode operation.</p>
newtype GetRegistrationCodeResponse = GetRegistrationCodeResponse 
  { "RegistrationCode'" :: NullOrUndefined.NullOrUndefined (RegistrationCode)
  }
derive instance newtypeGetRegistrationCodeResponse :: Newtype GetRegistrationCodeResponse _
derive instance repGenericGetRegistrationCodeResponse :: Generic GetRegistrationCodeResponse _
instance showGetRegistrationCodeResponse :: Show GetRegistrationCodeResponse where
  show = genericShow
instance decodeGetRegistrationCodeResponse :: Decode GetRegistrationCodeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRegistrationCodeResponse :: Encode GetRegistrationCodeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the GetTopicRule operation.</p>
newtype GetTopicRuleRequest = GetTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  }
derive instance newtypeGetTopicRuleRequest :: Newtype GetTopicRuleRequest _
derive instance repGenericGetTopicRuleRequest :: Generic GetTopicRuleRequest _
instance showGetTopicRuleRequest :: Show GetTopicRuleRequest where
  show = genericShow
instance decodeGetTopicRuleRequest :: Decode GetTopicRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTopicRuleRequest :: Encode GetTopicRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the GetTopicRule operation.</p>
newtype GetTopicRuleResponse = GetTopicRuleResponse 
  { "RuleArn'" :: NullOrUndefined.NullOrUndefined (RuleArn)
  , "Rule'" :: NullOrUndefined.NullOrUndefined (TopicRule)
  }
derive instance newtypeGetTopicRuleResponse :: Newtype GetTopicRuleResponse _
derive instance repGenericGetTopicRuleResponse :: Generic GetTopicRuleResponse _
instance showGetTopicRuleResponse :: Show GetTopicRuleResponse where
  show = genericShow
instance decodeGetTopicRuleResponse :: Decode GetTopicRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTopicRuleResponse :: Encode GetTopicRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetV2LoggingOptionsRequest = GetV2LoggingOptionsRequest Types.NoArguments
derive instance newtypeGetV2LoggingOptionsRequest :: Newtype GetV2LoggingOptionsRequest _
derive instance repGenericGetV2LoggingOptionsRequest :: Generic GetV2LoggingOptionsRequest _
instance showGetV2LoggingOptionsRequest :: Show GetV2LoggingOptionsRequest where
  show = genericShow
instance decodeGetV2LoggingOptionsRequest :: Decode GetV2LoggingOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetV2LoggingOptionsRequest :: Encode GetV2LoggingOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetV2LoggingOptionsResponse = GetV2LoggingOptionsResponse 
  { "RoleArn'" :: NullOrUndefined.NullOrUndefined (AwsArn)
  , "DefaultLogLevel'" :: NullOrUndefined.NullOrUndefined (LogLevel)
  , "DisableAllLogs'" :: NullOrUndefined.NullOrUndefined (DisableAllLogs)
  }
derive instance newtypeGetV2LoggingOptionsResponse :: Newtype GetV2LoggingOptionsResponse _
derive instance repGenericGetV2LoggingOptionsResponse :: Generic GetV2LoggingOptionsResponse _
instance showGetV2LoggingOptionsResponse :: Show GetV2LoggingOptionsResponse where
  show = genericShow
instance decodeGetV2LoggingOptionsResponse :: Decode GetV2LoggingOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetV2LoggingOptionsResponse :: Encode GetV2LoggingOptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name and ARN of a group.</p>
newtype GroupNameAndArn = GroupNameAndArn 
  { "GroupName'" :: NullOrUndefined.NullOrUndefined (ThingGroupName)
  , "GroupArn'" :: NullOrUndefined.NullOrUndefined (ThingGroupArn)
  }
derive instance newtypeGroupNameAndArn :: Newtype GroupNameAndArn _
derive instance repGenericGroupNameAndArn :: Generic GroupNameAndArn _
instance showGroupNameAndArn :: Show GroupNameAndArn where
  show = genericShow
instance decodeGroupNameAndArn :: Decode GroupNameAndArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupNameAndArn :: Encode GroupNameAndArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HashAlgorithm = HashAlgorithm String
derive instance newtypeHashAlgorithm :: Newtype HashAlgorithm _
derive instance repGenericHashAlgorithm :: Generic HashAlgorithm _
instance showHashAlgorithm :: Show HashAlgorithm where
  show = genericShow
instance decodeHashAlgorithm :: Decode HashAlgorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHashAlgorithm :: Encode HashAlgorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HashKeyField = HashKeyField String
derive instance newtypeHashKeyField :: Newtype HashKeyField _
derive instance repGenericHashKeyField :: Generic HashKeyField _
instance showHashKeyField :: Show HashKeyField where
  show = genericShow
instance decodeHashKeyField :: Decode HashKeyField where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHashKeyField :: Encode HashKeyField where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HashKeyValue = HashKeyValue String
derive instance newtypeHashKeyValue :: Newtype HashKeyValue _
derive instance repGenericHashKeyValue :: Generic HashKeyValue _
instance showHashKeyValue :: Show HashKeyValue where
  show = genericShow
instance decodeHashKeyValue :: Decode HashKeyValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHashKeyValue :: Encode HashKeyValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information that implicitly denies authorization. When policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.</p>
newtype ImplicitDeny = ImplicitDeny 
  { "Policies'" :: NullOrUndefined.NullOrUndefined (Policies)
  }
derive instance newtypeImplicitDeny :: Newtype ImplicitDeny _
derive instance repGenericImplicitDeny :: Generic ImplicitDeny _
instance showImplicitDeny :: Show ImplicitDeny where
  show = genericShow
instance decodeImplicitDeny :: Decode ImplicitDeny where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImplicitDeny :: Encode ImplicitDeny where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InProgressThings = InProgressThings Int
derive instance newtypeInProgressThings :: Newtype InProgressThings _
derive instance repGenericInProgressThings :: Generic InProgressThings _
instance showInProgressThings :: Show InProgressThings where
  show = genericShow
instance decodeInProgressThings :: Decode InProgressThings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInProgressThings :: Encode InProgressThings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IndexName = IndexName String
derive instance newtypeIndexName :: Newtype IndexName _
derive instance repGenericIndexName :: Generic IndexName _
instance showIndexName :: Show IndexName where
  show = genericShow
instance decodeIndexName :: Decode IndexName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexName :: Encode IndexName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IndexNamesList = IndexNamesList (Array IndexName)
derive instance newtypeIndexNamesList :: Newtype IndexNamesList _
derive instance repGenericIndexNamesList :: Generic IndexNamesList _
instance showIndexNamesList :: Show IndexNamesList where
  show = genericShow
instance decodeIndexNamesList :: Decode IndexNamesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexNamesList :: Encode IndexNamesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The index is not ready.</p>
newtype IndexNotReadyException = IndexNotReadyException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeIndexNotReadyException :: Newtype IndexNotReadyException _
derive instance repGenericIndexNotReadyException :: Generic IndexNotReadyException _
instance showIndexNotReadyException :: Show IndexNotReadyException where
  show = genericShow
instance decodeIndexNotReadyException :: Decode IndexNotReadyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexNotReadyException :: Encode IndexNotReadyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IndexSchema = IndexSchema String
derive instance newtypeIndexSchema :: Newtype IndexSchema _
derive instance repGenericIndexSchema :: Generic IndexSchema _
instance showIndexSchema :: Show IndexSchema where
  show = genericShow
instance decodeIndexSchema :: Decode IndexSchema where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexSchema :: Encode IndexSchema where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IndexStatus = IndexStatus String
derive instance newtypeIndexStatus :: Newtype IndexStatus _
derive instance repGenericIndexStatus :: Generic IndexStatus _
instance showIndexStatus :: Show IndexStatus where
  show = genericShow
instance decodeIndexStatus :: Decode IndexStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexStatus :: Encode IndexStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InlineDocument = InlineDocument String
derive instance newtypeInlineDocument :: Newtype InlineDocument _
derive instance repGenericInlineDocument :: Generic InlineDocument _
instance showInlineDocument :: Show InlineDocument where
  show = genericShow
instance decodeInlineDocument :: Decode InlineDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInlineDocument :: Encode InlineDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An unexpected error has occurred.</p>
newtype InternalException = InternalException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInternalException :: Newtype InternalException _
derive instance repGenericInternalException :: Generic InternalException _
instance showInternalException :: Show InternalException where
  show = genericShow
instance decodeInternalException :: Decode InternalException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalException :: Encode InternalException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An unexpected error has occurred.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInternalFailureException :: Newtype InternalFailureException _
derive instance repGenericInternalFailureException :: Generic InternalFailureException _
instance showInternalFailureException :: Show InternalFailureException where
  show = genericShow
instance decodeInternalFailureException :: Decode InternalFailureException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalFailureException :: Encode InternalFailureException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The query is invalid.</p>
newtype InvalidQueryException = InvalidQueryException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidQueryException :: Newtype InvalidQueryException _
derive instance repGenericInvalidQueryException :: Generic InvalidQueryException _
instance showInvalidQueryException :: Show InvalidQueryException where
  show = genericShow
instance decodeInvalidQueryException :: Decode InvalidQueryException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidQueryException :: Encode InvalidQueryException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request is not valid.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _
derive instance repGenericInvalidRequestException :: Generic InvalidRequestException _
instance showInvalidRequestException :: Show InvalidRequestException where
  show = genericShow
instance decodeInvalidRequestException :: Decode InvalidRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRequestException :: Encode InvalidRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response is invalid.</p>
newtype InvalidResponseException = InvalidResponseException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidResponseException :: Newtype InvalidResponseException _
derive instance repGenericInvalidResponseException :: Generic InvalidResponseException _
instance showInvalidResponseException :: Show InvalidResponseException where
  show = genericShow
instance decodeInvalidResponseException :: Decode InvalidResponseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidResponseException :: Encode InvalidResponseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsAuthenticated = IsAuthenticated Boolean
derive instance newtypeIsAuthenticated :: Newtype IsAuthenticated _
derive instance repGenericIsAuthenticated :: Generic IsAuthenticated _
instance showIsAuthenticated :: Show IsAuthenticated where
  show = genericShow
instance decodeIsAuthenticated :: Decode IsAuthenticated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsAuthenticated :: Encode IsAuthenticated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsDefaultVersion = IsDefaultVersion Boolean
derive instance newtypeIsDefaultVersion :: Newtype IsDefaultVersion _
derive instance repGenericIsDefaultVersion :: Generic IsDefaultVersion _
instance showIsDefaultVersion :: Show IsDefaultVersion where
  show = genericShow
instance decodeIsDefaultVersion :: Decode IsDefaultVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsDefaultVersion :: Encode IsDefaultVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsDisabled = IsDisabled Boolean
derive instance newtypeIsDisabled :: Newtype IsDisabled _
derive instance repGenericIsDisabled :: Generic IsDisabled _
instance showIsDisabled :: Show IsDisabled where
  show = genericShow
instance decodeIsDisabled :: Decode IsDisabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsDisabled :: Encode IsDisabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>Job</code> object contains details about a job.</p>
newtype Job = Job 
  { "JobArn'" :: NullOrUndefined.NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined.NullOrUndefined (JobId)
  , "TargetSelection'" :: NullOrUndefined.NullOrUndefined (TargetSelection)
  , "Status'" :: NullOrUndefined.NullOrUndefined (JobStatus)
  , "Comment'" :: NullOrUndefined.NullOrUndefined (Comment)
  , "Targets'" :: NullOrUndefined.NullOrUndefined (JobTargets)
  , "Description'" :: NullOrUndefined.NullOrUndefined (JobDescription)
  , "PresignedUrlConfig'" :: NullOrUndefined.NullOrUndefined (PresignedUrlConfig)
  , "JobExecutionsRolloutConfig'" :: NullOrUndefined.NullOrUndefined (JobExecutionsRolloutConfig)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "LastUpdatedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "CompletedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "JobProcessDetails'" :: NullOrUndefined.NullOrUndefined (JobProcessDetails)
  , "DocumentParameters'" :: NullOrUndefined.NullOrUndefined (JobDocumentParameters)
  }
derive instance newtypeJob :: Newtype Job _
derive instance repGenericJob :: Generic Job _
instance showJob :: Show Job where
  show = genericShow
instance decodeJob :: Decode Job where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJob :: Encode Job where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobArn = JobArn String
derive instance newtypeJobArn :: Newtype JobArn _
derive instance repGenericJobArn :: Generic JobArn _
instance showJobArn :: Show JobArn where
  show = genericShow
instance decodeJobArn :: Decode JobArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobArn :: Encode JobArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobDescription = JobDescription String
derive instance newtypeJobDescription :: Newtype JobDescription _
derive instance repGenericJobDescription :: Generic JobDescription _
instance showJobDescription :: Show JobDescription where
  show = genericShow
instance decodeJobDescription :: Decode JobDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDescription :: Encode JobDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobDocument = JobDocument String
derive instance newtypeJobDocument :: Newtype JobDocument _
derive instance repGenericJobDocument :: Generic JobDocument _
instance showJobDocument :: Show JobDocument where
  show = genericShow
instance decodeJobDocument :: Decode JobDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDocument :: Encode JobDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobDocumentParameters = JobDocumentParameters (StrMap.StrMap ParameterValue)
derive instance newtypeJobDocumentParameters :: Newtype JobDocumentParameters _
derive instance repGenericJobDocumentParameters :: Generic JobDocumentParameters _
instance showJobDocumentParameters :: Show JobDocumentParameters where
  show = genericShow
instance decodeJobDocumentParameters :: Decode JobDocumentParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDocumentParameters :: Encode JobDocumentParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobDocumentSource = JobDocumentSource String
derive instance newtypeJobDocumentSource :: Newtype JobDocumentSource _
derive instance repGenericJobDocumentSource :: Generic JobDocumentSource _
instance showJobDocumentSource :: Show JobDocumentSource where
  show = genericShow
instance decodeJobDocumentSource :: Decode JobDocumentSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDocumentSource :: Encode JobDocumentSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The job execution object represents the execution of a job on a particular device.</p>
newtype JobExecution = JobExecution 
  { "JobId'" :: NullOrUndefined.NullOrUndefined (JobId)
  , "Status'" :: NullOrUndefined.NullOrUndefined (JobExecutionStatus)
  , "StatusDetails'" :: NullOrUndefined.NullOrUndefined (JobExecutionStatusDetails)
  , "ThingArn'" :: NullOrUndefined.NullOrUndefined (ThingArn)
  , "QueuedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "StartedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "LastUpdatedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "ExecutionNumber'" :: NullOrUndefined.NullOrUndefined (ExecutionNumber)
  }
derive instance newtypeJobExecution :: Newtype JobExecution _
derive instance repGenericJobExecution :: Generic JobExecution _
instance showJobExecution :: Show JobExecution where
  show = genericShow
instance decodeJobExecution :: Decode JobExecution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecution :: Encode JobExecution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobExecutionStatus = JobExecutionStatus String
derive instance newtypeJobExecutionStatus :: Newtype JobExecutionStatus _
derive instance repGenericJobExecutionStatus :: Generic JobExecutionStatus _
instance showJobExecutionStatus :: Show JobExecutionStatus where
  show = genericShow
instance decodeJobExecutionStatus :: Decode JobExecutionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecutionStatus :: Encode JobExecutionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details of the job execution status.</p>
newtype JobExecutionStatusDetails = JobExecutionStatusDetails 
  { "DetailsMap'" :: NullOrUndefined.NullOrUndefined (DetailsMap)
  }
derive instance newtypeJobExecutionStatusDetails :: Newtype JobExecutionStatusDetails _
derive instance repGenericJobExecutionStatusDetails :: Generic JobExecutionStatusDetails _
instance showJobExecutionStatusDetails :: Show JobExecutionStatusDetails where
  show = genericShow
instance decodeJobExecutionStatusDetails :: Decode JobExecutionStatusDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecutionStatusDetails :: Encode JobExecutionStatusDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The job execution summary.</p>
newtype JobExecutionSummary = JobExecutionSummary 
  { "Status'" :: NullOrUndefined.NullOrUndefined (JobExecutionStatus)
  , "QueuedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "StartedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "LastUpdatedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "ExecutionNumber'" :: NullOrUndefined.NullOrUndefined (ExecutionNumber)
  }
derive instance newtypeJobExecutionSummary :: Newtype JobExecutionSummary _
derive instance repGenericJobExecutionSummary :: Generic JobExecutionSummary _
instance showJobExecutionSummary :: Show JobExecutionSummary where
  show = genericShow
instance decodeJobExecutionSummary :: Decode JobExecutionSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecutionSummary :: Encode JobExecutionSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains a summary of information about job executions for a specific job.</p>
newtype JobExecutionSummaryForJob = JobExecutionSummaryForJob 
  { "ThingArn'" :: NullOrUndefined.NullOrUndefined (ThingArn)
  , "JobExecutionSummary'" :: NullOrUndefined.NullOrUndefined (JobExecutionSummary)
  }
derive instance newtypeJobExecutionSummaryForJob :: Newtype JobExecutionSummaryForJob _
derive instance repGenericJobExecutionSummaryForJob :: Generic JobExecutionSummaryForJob _
instance showJobExecutionSummaryForJob :: Show JobExecutionSummaryForJob where
  show = genericShow
instance decodeJobExecutionSummaryForJob :: Decode JobExecutionSummaryForJob where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecutionSummaryForJob :: Encode JobExecutionSummaryForJob where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobExecutionSummaryForJobList = JobExecutionSummaryForJobList (Array JobExecutionSummaryForJob)
derive instance newtypeJobExecutionSummaryForJobList :: Newtype JobExecutionSummaryForJobList _
derive instance repGenericJobExecutionSummaryForJobList :: Generic JobExecutionSummaryForJobList _
instance showJobExecutionSummaryForJobList :: Show JobExecutionSummaryForJobList where
  show = genericShow
instance decodeJobExecutionSummaryForJobList :: Decode JobExecutionSummaryForJobList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecutionSummaryForJobList :: Encode JobExecutionSummaryForJobList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The job execution summary for a thing.</p>
newtype JobExecutionSummaryForThing = JobExecutionSummaryForThing 
  { "JobId'" :: NullOrUndefined.NullOrUndefined (JobId)
  , "JobExecutionSummary'" :: NullOrUndefined.NullOrUndefined (JobExecutionSummary)
  }
derive instance newtypeJobExecutionSummaryForThing :: Newtype JobExecutionSummaryForThing _
derive instance repGenericJobExecutionSummaryForThing :: Generic JobExecutionSummaryForThing _
instance showJobExecutionSummaryForThing :: Show JobExecutionSummaryForThing where
  show = genericShow
instance decodeJobExecutionSummaryForThing :: Decode JobExecutionSummaryForThing where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecutionSummaryForThing :: Encode JobExecutionSummaryForThing where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobExecutionSummaryForThingList = JobExecutionSummaryForThingList (Array JobExecutionSummaryForThing)
derive instance newtypeJobExecutionSummaryForThingList :: Newtype JobExecutionSummaryForThingList _
derive instance repGenericJobExecutionSummaryForThingList :: Generic JobExecutionSummaryForThingList _
instance showJobExecutionSummaryForThingList :: Show JobExecutionSummaryForThingList where
  show = genericShow
instance decodeJobExecutionSummaryForThingList :: Decode JobExecutionSummaryForThingList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecutionSummaryForThingList :: Encode JobExecutionSummaryForThingList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Allows you to create a staged rollout of a job.</p>
newtype JobExecutionsRolloutConfig = JobExecutionsRolloutConfig 
  { "MaximumPerMinute'" :: NullOrUndefined.NullOrUndefined (MaxJobExecutionsPerMin)
  }
derive instance newtypeJobExecutionsRolloutConfig :: Newtype JobExecutionsRolloutConfig _
derive instance repGenericJobExecutionsRolloutConfig :: Generic JobExecutionsRolloutConfig _
instance showJobExecutionsRolloutConfig :: Show JobExecutionsRolloutConfig where
  show = genericShow
instance decodeJobExecutionsRolloutConfig :: Decode JobExecutionsRolloutConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecutionsRolloutConfig :: Encode JobExecutionsRolloutConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobId = JobId String
derive instance newtypeJobId :: Newtype JobId _
derive instance repGenericJobId :: Generic JobId _
instance showJobId :: Show JobId where
  show = genericShow
instance decodeJobId :: Decode JobId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobId :: Encode JobId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The job process details.</p>
newtype JobProcessDetails = JobProcessDetails 
  { "ProcessingTargets'" :: NullOrUndefined.NullOrUndefined (ProcessingTargetNameList)
  , "NumberOfCanceledThings'" :: NullOrUndefined.NullOrUndefined (CanceledThings)
  , "NumberOfSucceededThings'" :: NullOrUndefined.NullOrUndefined (SucceededThings)
  , "NumberOfFailedThings'" :: NullOrUndefined.NullOrUndefined (FailedThings)
  , "NumberOfRejectedThings'" :: NullOrUndefined.NullOrUndefined (RejectedThings)
  , "NumberOfQueuedThings'" :: NullOrUndefined.NullOrUndefined (QueuedThings)
  , "NumberOfInProgressThings'" :: NullOrUndefined.NullOrUndefined (InProgressThings)
  , "NumberOfRemovedThings'" :: NullOrUndefined.NullOrUndefined (RemovedThings)
  }
derive instance newtypeJobProcessDetails :: Newtype JobProcessDetails _
derive instance repGenericJobProcessDetails :: Generic JobProcessDetails _
instance showJobProcessDetails :: Show JobProcessDetails where
  show = genericShow
instance decodeJobProcessDetails :: Decode JobProcessDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobProcessDetails :: Encode JobProcessDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobStatus = JobStatus String
derive instance newtypeJobStatus :: Newtype JobStatus _
derive instance repGenericJobStatus :: Generic JobStatus _
instance showJobStatus :: Show JobStatus where
  show = genericShow
instance decodeJobStatus :: Decode JobStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobStatus :: Encode JobStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The job summary.</p>
newtype JobSummary = JobSummary 
  { "JobArn'" :: NullOrUndefined.NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined.NullOrUndefined (JobId)
  , "ThingGroupId'" :: NullOrUndefined.NullOrUndefined (ThingGroupId)
  , "TargetSelection'" :: NullOrUndefined.NullOrUndefined (TargetSelection)
  , "Status'" :: NullOrUndefined.NullOrUndefined (JobStatus)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "LastUpdatedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "CompletedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeJobSummary :: Newtype JobSummary _
derive instance repGenericJobSummary :: Generic JobSummary _
instance showJobSummary :: Show JobSummary where
  show = genericShow
instance decodeJobSummary :: Decode JobSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobSummary :: Encode JobSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobSummaryList = JobSummaryList (Array JobSummary)
derive instance newtypeJobSummaryList :: Newtype JobSummaryList _
derive instance repGenericJobSummaryList :: Generic JobSummaryList _
instance showJobSummaryList :: Show JobSummaryList where
  show = genericShow
instance decodeJobSummaryList :: Decode JobSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobSummaryList :: Encode JobSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobTargets = JobTargets (Array TargetArn)
derive instance newtypeJobTargets :: Newtype JobTargets _
derive instance repGenericJobTargets :: Generic JobTargets _
instance showJobTargets :: Show JobTargets where
  show = genericShow
instance decodeJobTargets :: Decode JobTargets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobTargets :: Encode JobTargets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JsonDocument = JsonDocument String
derive instance newtypeJsonDocument :: Newtype JsonDocument _
derive instance repGenericJsonDocument :: Generic JsonDocument _
instance showJsonDocument :: Show JsonDocument where
  show = genericShow
instance decodeJsonDocument :: Decode JsonDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJsonDocument :: Encode JsonDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Key = Key String
derive instance newtypeKey :: Newtype Key _
derive instance repGenericKey :: Generic Key _
instance showKey :: Show Key where
  show = genericShow
instance decodeKey :: Decode Key where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKey :: Encode Key where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyName = KeyName String
derive instance newtypeKeyName :: Newtype KeyName _
derive instance repGenericKeyName :: Generic KeyName _
instance showKeyName :: Show KeyName where
  show = genericShow
instance decodeKeyName :: Decode KeyName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyName :: Encode KeyName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a key pair.</p>
newtype KeyPair = KeyPair 
  { "PublicKey" :: NullOrUndefined.NullOrUndefined (PublicKey)
  , "PrivateKey" :: NullOrUndefined.NullOrUndefined (PrivateKey)
  }
derive instance newtypeKeyPair :: Newtype KeyPair _
derive instance repGenericKeyPair :: Generic KeyPair _
instance showKeyPair :: Show KeyPair where
  show = genericShow
instance decodeKeyPair :: Decode KeyPair where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyPair :: Encode KeyPair where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyValue = KeyValue String
derive instance newtypeKeyValue :: Newtype KeyValue _
derive instance repGenericKeyValue :: Generic KeyValue _
instance showKeyValue :: Show KeyValue where
  show = genericShow
instance decodeKeyValue :: Decode KeyValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyValue :: Encode KeyValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action to write data to an Amazon Kinesis stream.</p>
newtype KinesisAction = KinesisAction 
  { "RoleArn'" :: (AwsArn)
  , "StreamName'" :: (StreamName)
  , "PartitionKey'" :: NullOrUndefined.NullOrUndefined (PartitionKey)
  }
derive instance newtypeKinesisAction :: Newtype KinesisAction _
derive instance repGenericKinesisAction :: Generic KinesisAction _
instance showKinesisAction :: Show KinesisAction where
  show = genericShow
instance decodeKinesisAction :: Decode KinesisAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisAction :: Encode KinesisAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action to invoke a Lambda function.</p>
newtype LambdaAction = LambdaAction 
  { "FunctionArn'" :: (FunctionArn)
  }
derive instance newtypeLambdaAction :: Newtype LambdaAction _
derive instance repGenericLambdaAction :: Generic LambdaAction _
instance showLambdaAction :: Show LambdaAction where
  show = genericShow
instance decodeLambdaAction :: Decode LambdaAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaAction :: Encode LambdaAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LaserMaxResults = LaserMaxResults Int
derive instance newtypeLaserMaxResults :: Newtype LaserMaxResults _
derive instance repGenericLaserMaxResults :: Generic LaserMaxResults _
instance showLaserMaxResults :: Show LaserMaxResults where
  show = genericShow
instance decodeLaserMaxResults :: Decode LaserMaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLaserMaxResults :: Encode LaserMaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastModifiedDate = LastModifiedDate Number
derive instance newtypeLastModifiedDate :: Newtype LastModifiedDate _
derive instance repGenericLastModifiedDate :: Generic LastModifiedDate _
instance showLastModifiedDate :: Show LastModifiedDate where
  show = genericShow
instance decodeLastModifiedDate :: Decode LastModifiedDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastModifiedDate :: Encode LastModifiedDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of attached entities exceeds the limit.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAttachedPoliciesRequest = ListAttachedPoliciesRequest 
  { "Target'" :: (PolicyTarget)
  , "Recursive'" :: NullOrUndefined.NullOrUndefined (Recursive)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeListAttachedPoliciesRequest :: Newtype ListAttachedPoliciesRequest _
derive instance repGenericListAttachedPoliciesRequest :: Generic ListAttachedPoliciesRequest _
instance showListAttachedPoliciesRequest :: Show ListAttachedPoliciesRequest where
  show = genericShow
instance decodeListAttachedPoliciesRequest :: Decode ListAttachedPoliciesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAttachedPoliciesRequest :: Encode ListAttachedPoliciesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAttachedPoliciesResponse = ListAttachedPoliciesResponse 
  { "Policies'" :: NullOrUndefined.NullOrUndefined (Policies)
  , "NextMarker'" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListAttachedPoliciesResponse :: Newtype ListAttachedPoliciesResponse _
derive instance repGenericListAttachedPoliciesResponse :: Generic ListAttachedPoliciesResponse _
instance showListAttachedPoliciesResponse :: Show ListAttachedPoliciesResponse where
  show = genericShow
instance decodeListAttachedPoliciesResponse :: Decode ListAttachedPoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAttachedPoliciesResponse :: Encode ListAttachedPoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAuthorizersRequest = ListAuthorizersRequest 
  { "PageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined.NullOrUndefined (AscendingOrder)
  , "Status'" :: NullOrUndefined.NullOrUndefined (AuthorizerStatus)
  }
derive instance newtypeListAuthorizersRequest :: Newtype ListAuthorizersRequest _
derive instance repGenericListAuthorizersRequest :: Generic ListAuthorizersRequest _
instance showListAuthorizersRequest :: Show ListAuthorizersRequest where
  show = genericShow
instance decodeListAuthorizersRequest :: Decode ListAuthorizersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAuthorizersRequest :: Encode ListAuthorizersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAuthorizersResponse = ListAuthorizersResponse 
  { "Authorizers'" :: NullOrUndefined.NullOrUndefined (Authorizers)
  , "NextMarker'" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListAuthorizersResponse :: Newtype ListAuthorizersResponse _
derive instance repGenericListAuthorizersResponse :: Generic ListAuthorizersResponse _
instance showListAuthorizersResponse :: Show ListAuthorizersResponse where
  show = genericShow
instance decodeListAuthorizersResponse :: Decode ListAuthorizersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAuthorizersResponse :: Encode ListAuthorizersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for the ListCACertificates operation.</p>
newtype ListCACertificatesRequest = ListCACertificatesRequest 
  { "PageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined.NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListCACertificatesRequest :: Newtype ListCACertificatesRequest _
derive instance repGenericListCACertificatesRequest :: Generic ListCACertificatesRequest _
instance showListCACertificatesRequest :: Show ListCACertificatesRequest where
  show = genericShow
instance decodeListCACertificatesRequest :: Decode ListCACertificatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCACertificatesRequest :: Encode ListCACertificatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the ListCACertificates operation.</p>
newtype ListCACertificatesResponse = ListCACertificatesResponse 
  { "Certificates'" :: NullOrUndefined.NullOrUndefined (CACertificates)
  , "NextMarker'" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListCACertificatesResponse :: Newtype ListCACertificatesResponse _
derive instance repGenericListCACertificatesResponse :: Generic ListCACertificatesResponse _
instance showListCACertificatesResponse :: Show ListCACertificatesResponse where
  show = genericShow
instance decodeListCACertificatesResponse :: Decode ListCACertificatesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCACertificatesResponse :: Encode ListCACertificatesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input to the ListCertificatesByCA operation.</p>
newtype ListCertificatesByCARequest = ListCertificatesByCARequest 
  { "CaCertificateId'" :: (CertificateId)
  , "PageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined.NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListCertificatesByCARequest :: Newtype ListCertificatesByCARequest _
derive instance repGenericListCertificatesByCARequest :: Generic ListCertificatesByCARequest _
instance showListCertificatesByCARequest :: Show ListCertificatesByCARequest where
  show = genericShow
instance decodeListCertificatesByCARequest :: Decode ListCertificatesByCARequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCertificatesByCARequest :: Encode ListCertificatesByCARequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output of the ListCertificatesByCA operation.</p>
newtype ListCertificatesByCAResponse = ListCertificatesByCAResponse 
  { "Certificates'" :: NullOrUndefined.NullOrUndefined (Certificates)
  , "NextMarker'" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListCertificatesByCAResponse :: Newtype ListCertificatesByCAResponse _
derive instance repGenericListCertificatesByCAResponse :: Generic ListCertificatesByCAResponse _
instance showListCertificatesByCAResponse :: Show ListCertificatesByCAResponse where
  show = genericShow
instance decodeListCertificatesByCAResponse :: Decode ListCertificatesByCAResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCertificatesByCAResponse :: Encode ListCertificatesByCAResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the ListCertificates operation.</p>
newtype ListCertificatesRequest = ListCertificatesRequest 
  { "PageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined.NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListCertificatesRequest :: Newtype ListCertificatesRequest _
derive instance repGenericListCertificatesRequest :: Generic ListCertificatesRequest _
instance showListCertificatesRequest :: Show ListCertificatesRequest where
  show = genericShow
instance decodeListCertificatesRequest :: Decode ListCertificatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCertificatesRequest :: Encode ListCertificatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output of the ListCertificates operation.</p>
newtype ListCertificatesResponse = ListCertificatesResponse 
  { "Certificates'" :: NullOrUndefined.NullOrUndefined (Certificates)
  , "NextMarker'" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListCertificatesResponse :: Newtype ListCertificatesResponse _
derive instance repGenericListCertificatesResponse :: Generic ListCertificatesResponse _
instance showListCertificatesResponse :: Show ListCertificatesResponse where
  show = genericShow
instance decodeListCertificatesResponse :: Decode ListCertificatesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCertificatesResponse :: Encode ListCertificatesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIndicesRequest = ListIndicesRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (QueryMaxResults)
  }
derive instance newtypeListIndicesRequest :: Newtype ListIndicesRequest _
derive instance repGenericListIndicesRequest :: Generic ListIndicesRequest _
instance showListIndicesRequest :: Show ListIndicesRequest where
  show = genericShow
instance decodeListIndicesRequest :: Decode ListIndicesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIndicesRequest :: Encode ListIndicesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIndicesResponse = ListIndicesResponse 
  { "IndexNames'" :: NullOrUndefined.NullOrUndefined (IndexNamesList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListIndicesResponse :: Newtype ListIndicesResponse _
derive instance repGenericListIndicesResponse :: Generic ListIndicesResponse _
instance showListIndicesResponse :: Show ListIndicesResponse where
  show = genericShow
instance decodeListIndicesResponse :: Decode ListIndicesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIndicesResponse :: Encode ListIndicesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListJobExecutionsForJobRequest = ListJobExecutionsForJobRequest 
  { "JobId'" :: (JobId)
  , "Status'" :: NullOrUndefined.NullOrUndefined (JobExecutionStatus)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (LaserMaxResults)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListJobExecutionsForJobRequest :: Newtype ListJobExecutionsForJobRequest _
derive instance repGenericListJobExecutionsForJobRequest :: Generic ListJobExecutionsForJobRequest _
instance showListJobExecutionsForJobRequest :: Show ListJobExecutionsForJobRequest where
  show = genericShow
instance decodeListJobExecutionsForJobRequest :: Decode ListJobExecutionsForJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobExecutionsForJobRequest :: Encode ListJobExecutionsForJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListJobExecutionsForJobResponse = ListJobExecutionsForJobResponse 
  { "ExecutionSummaries'" :: NullOrUndefined.NullOrUndefined (JobExecutionSummaryForJobList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListJobExecutionsForJobResponse :: Newtype ListJobExecutionsForJobResponse _
derive instance repGenericListJobExecutionsForJobResponse :: Generic ListJobExecutionsForJobResponse _
instance showListJobExecutionsForJobResponse :: Show ListJobExecutionsForJobResponse where
  show = genericShow
instance decodeListJobExecutionsForJobResponse :: Decode ListJobExecutionsForJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobExecutionsForJobResponse :: Encode ListJobExecutionsForJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListJobExecutionsForThingRequest = ListJobExecutionsForThingRequest 
  { "ThingName'" :: (ThingName)
  , "Status'" :: NullOrUndefined.NullOrUndefined (JobExecutionStatus)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (LaserMaxResults)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListJobExecutionsForThingRequest :: Newtype ListJobExecutionsForThingRequest _
derive instance repGenericListJobExecutionsForThingRequest :: Generic ListJobExecutionsForThingRequest _
instance showListJobExecutionsForThingRequest :: Show ListJobExecutionsForThingRequest where
  show = genericShow
instance decodeListJobExecutionsForThingRequest :: Decode ListJobExecutionsForThingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobExecutionsForThingRequest :: Encode ListJobExecutionsForThingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListJobExecutionsForThingResponse = ListJobExecutionsForThingResponse 
  { "ExecutionSummaries'" :: NullOrUndefined.NullOrUndefined (JobExecutionSummaryForThingList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListJobExecutionsForThingResponse :: Newtype ListJobExecutionsForThingResponse _
derive instance repGenericListJobExecutionsForThingResponse :: Generic ListJobExecutionsForThingResponse _
instance showListJobExecutionsForThingResponse :: Show ListJobExecutionsForThingResponse where
  show = genericShow
instance decodeListJobExecutionsForThingResponse :: Decode ListJobExecutionsForThingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobExecutionsForThingResponse :: Encode ListJobExecutionsForThingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListJobsRequest = ListJobsRequest 
  { "Status'" :: NullOrUndefined.NullOrUndefined (JobStatus)
  , "TargetSelection'" :: NullOrUndefined.NullOrUndefined (TargetSelection)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (LaserMaxResults)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "ThingGroupName'" :: NullOrUndefined.NullOrUndefined (ThingGroupName)
  , "ThingGroupId'" :: NullOrUndefined.NullOrUndefined (ThingGroupId)
  }
derive instance newtypeListJobsRequest :: Newtype ListJobsRequest _
derive instance repGenericListJobsRequest :: Generic ListJobsRequest _
instance showListJobsRequest :: Show ListJobsRequest where
  show = genericShow
instance decodeListJobsRequest :: Decode ListJobsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsRequest :: Encode ListJobsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListJobsResponse = ListJobsResponse 
  { "Jobs'" :: NullOrUndefined.NullOrUndefined (JobSummaryList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListJobsResponse :: Newtype ListJobsResponse _
derive instance repGenericListJobsResponse :: Generic ListJobsResponse _
instance showListJobsResponse :: Show ListJobsResponse where
  show = genericShow
instance decodeListJobsResponse :: Decode ListJobsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsResponse :: Encode ListJobsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOTAUpdatesRequest = ListOTAUpdatesRequest 
  { "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "OtaUpdateStatus'" :: NullOrUndefined.NullOrUndefined (OTAUpdateStatus)
  }
derive instance newtypeListOTAUpdatesRequest :: Newtype ListOTAUpdatesRequest _
derive instance repGenericListOTAUpdatesRequest :: Generic ListOTAUpdatesRequest _
instance showListOTAUpdatesRequest :: Show ListOTAUpdatesRequest where
  show = genericShow
instance decodeListOTAUpdatesRequest :: Decode ListOTAUpdatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOTAUpdatesRequest :: Encode ListOTAUpdatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOTAUpdatesResponse = ListOTAUpdatesResponse 
  { "OtaUpdates'" :: NullOrUndefined.NullOrUndefined (OTAUpdatesSummary)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListOTAUpdatesResponse :: Newtype ListOTAUpdatesResponse _
derive instance repGenericListOTAUpdatesResponse :: Generic ListOTAUpdatesResponse _
instance showListOTAUpdatesResponse :: Show ListOTAUpdatesResponse where
  show = genericShow
instance decodeListOTAUpdatesResponse :: Decode ListOTAUpdatesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOTAUpdatesResponse :: Encode ListOTAUpdatesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input to the ListOutgoingCertificates operation.</p>
newtype ListOutgoingCertificatesRequest = ListOutgoingCertificatesRequest 
  { "PageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined.NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListOutgoingCertificatesRequest :: Newtype ListOutgoingCertificatesRequest _
derive instance repGenericListOutgoingCertificatesRequest :: Generic ListOutgoingCertificatesRequest _
instance showListOutgoingCertificatesRequest :: Show ListOutgoingCertificatesRequest where
  show = genericShow
instance decodeListOutgoingCertificatesRequest :: Decode ListOutgoingCertificatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOutgoingCertificatesRequest :: Encode ListOutgoingCertificatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the ListOutgoingCertificates operation.</p>
newtype ListOutgoingCertificatesResponse = ListOutgoingCertificatesResponse 
  { "OutgoingCertificates'" :: NullOrUndefined.NullOrUndefined (OutgoingCertificates)
  , "NextMarker'" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListOutgoingCertificatesResponse :: Newtype ListOutgoingCertificatesResponse _
derive instance repGenericListOutgoingCertificatesResponse :: Generic ListOutgoingCertificatesResponse _
instance showListOutgoingCertificatesResponse :: Show ListOutgoingCertificatesResponse where
  show = genericShow
instance decodeListOutgoingCertificatesResponse :: Decode ListOutgoingCertificatesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOutgoingCertificatesResponse :: Encode ListOutgoingCertificatesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the ListPolicies operation.</p>
newtype ListPoliciesRequest = ListPoliciesRequest 
  { "Marker'" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "AscendingOrder'" :: NullOrUndefined.NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListPoliciesRequest :: Newtype ListPoliciesRequest _
derive instance repGenericListPoliciesRequest :: Generic ListPoliciesRequest _
instance showListPoliciesRequest :: Show ListPoliciesRequest where
  show = genericShow
instance decodeListPoliciesRequest :: Decode ListPoliciesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPoliciesRequest :: Encode ListPoliciesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the ListPolicies operation.</p>
newtype ListPoliciesResponse = ListPoliciesResponse 
  { "Policies'" :: NullOrUndefined.NullOrUndefined (Policies)
  , "NextMarker'" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListPoliciesResponse :: Newtype ListPoliciesResponse _
derive instance repGenericListPoliciesResponse :: Generic ListPoliciesResponse _
instance showListPoliciesResponse :: Show ListPoliciesResponse where
  show = genericShow
instance decodeListPoliciesResponse :: Decode ListPoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPoliciesResponse :: Encode ListPoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the ListPolicyPrincipals operation.</p>
newtype ListPolicyPrincipalsRequest = ListPolicyPrincipalsRequest 
  { "PolicyName'" :: (PolicyName)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "AscendingOrder'" :: NullOrUndefined.NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListPolicyPrincipalsRequest :: Newtype ListPolicyPrincipalsRequest _
derive instance repGenericListPolicyPrincipalsRequest :: Generic ListPolicyPrincipalsRequest _
instance showListPolicyPrincipalsRequest :: Show ListPolicyPrincipalsRequest where
  show = genericShow
instance decodeListPolicyPrincipalsRequest :: Decode ListPolicyPrincipalsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPolicyPrincipalsRequest :: Encode ListPolicyPrincipalsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the ListPolicyPrincipals operation.</p>
newtype ListPolicyPrincipalsResponse = ListPolicyPrincipalsResponse 
  { "Principals'" :: NullOrUndefined.NullOrUndefined (Principals)
  , "NextMarker'" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListPolicyPrincipalsResponse :: Newtype ListPolicyPrincipalsResponse _
derive instance repGenericListPolicyPrincipalsResponse :: Generic ListPolicyPrincipalsResponse _
instance showListPolicyPrincipalsResponse :: Show ListPolicyPrincipalsResponse where
  show = genericShow
instance decodeListPolicyPrincipalsResponse :: Decode ListPolicyPrincipalsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPolicyPrincipalsResponse :: Encode ListPolicyPrincipalsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the ListPolicyVersions operation.</p>
newtype ListPolicyVersionsRequest = ListPolicyVersionsRequest 
  { "PolicyName'" :: (PolicyName)
  }
derive instance newtypeListPolicyVersionsRequest :: Newtype ListPolicyVersionsRequest _
derive instance repGenericListPolicyVersionsRequest :: Generic ListPolicyVersionsRequest _
instance showListPolicyVersionsRequest :: Show ListPolicyVersionsRequest where
  show = genericShow
instance decodeListPolicyVersionsRequest :: Decode ListPolicyVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPolicyVersionsRequest :: Encode ListPolicyVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the ListPolicyVersions operation.</p>
newtype ListPolicyVersionsResponse = ListPolicyVersionsResponse 
  { "PolicyVersions'" :: NullOrUndefined.NullOrUndefined (PolicyVersions)
  }
derive instance newtypeListPolicyVersionsResponse :: Newtype ListPolicyVersionsResponse _
derive instance repGenericListPolicyVersionsResponse :: Generic ListPolicyVersionsResponse _
instance showListPolicyVersionsResponse :: Show ListPolicyVersionsResponse where
  show = genericShow
instance decodeListPolicyVersionsResponse :: Decode ListPolicyVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPolicyVersionsResponse :: Encode ListPolicyVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the ListPrincipalPolicies operation.</p>
newtype ListPrincipalPoliciesRequest = ListPrincipalPoliciesRequest 
  { "Principal'" :: (Principal)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "AscendingOrder'" :: NullOrUndefined.NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListPrincipalPoliciesRequest :: Newtype ListPrincipalPoliciesRequest _
derive instance repGenericListPrincipalPoliciesRequest :: Generic ListPrincipalPoliciesRequest _
instance showListPrincipalPoliciesRequest :: Show ListPrincipalPoliciesRequest where
  show = genericShow
instance decodeListPrincipalPoliciesRequest :: Decode ListPrincipalPoliciesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPrincipalPoliciesRequest :: Encode ListPrincipalPoliciesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the ListPrincipalPolicies operation.</p>
newtype ListPrincipalPoliciesResponse = ListPrincipalPoliciesResponse 
  { "Policies'" :: NullOrUndefined.NullOrUndefined (Policies)
  , "NextMarker'" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListPrincipalPoliciesResponse :: Newtype ListPrincipalPoliciesResponse _
derive instance repGenericListPrincipalPoliciesResponse :: Generic ListPrincipalPoliciesResponse _
instance showListPrincipalPoliciesResponse :: Show ListPrincipalPoliciesResponse where
  show = genericShow
instance decodeListPrincipalPoliciesResponse :: Decode ListPrincipalPoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPrincipalPoliciesResponse :: Encode ListPrincipalPoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the ListPrincipalThings operation.</p>
newtype ListPrincipalThingsRequest = ListPrincipalThingsRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (RegistryMaxResults)
  , "Principal'" :: (Principal)
  }
derive instance newtypeListPrincipalThingsRequest :: Newtype ListPrincipalThingsRequest _
derive instance repGenericListPrincipalThingsRequest :: Generic ListPrincipalThingsRequest _
instance showListPrincipalThingsRequest :: Show ListPrincipalThingsRequest where
  show = genericShow
instance decodeListPrincipalThingsRequest :: Decode ListPrincipalThingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPrincipalThingsRequest :: Encode ListPrincipalThingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the ListPrincipalThings operation.</p>
newtype ListPrincipalThingsResponse = ListPrincipalThingsResponse 
  { "Things'" :: NullOrUndefined.NullOrUndefined (ThingNameList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListPrincipalThingsResponse :: Newtype ListPrincipalThingsResponse _
derive instance repGenericListPrincipalThingsResponse :: Generic ListPrincipalThingsResponse _
instance showListPrincipalThingsResponse :: Show ListPrincipalThingsResponse where
  show = genericShow
instance decodeListPrincipalThingsResponse :: Decode ListPrincipalThingsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPrincipalThingsResponse :: Encode ListPrincipalThingsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRoleAliasesRequest = ListRoleAliasesRequest 
  { "PageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined.NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListRoleAliasesRequest :: Newtype ListRoleAliasesRequest _
derive instance repGenericListRoleAliasesRequest :: Generic ListRoleAliasesRequest _
instance showListRoleAliasesRequest :: Show ListRoleAliasesRequest where
  show = genericShow
instance decodeListRoleAliasesRequest :: Decode ListRoleAliasesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRoleAliasesRequest :: Encode ListRoleAliasesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRoleAliasesResponse = ListRoleAliasesResponse 
  { "RoleAliases'" :: NullOrUndefined.NullOrUndefined (RoleAliases)
  , "NextMarker'" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListRoleAliasesResponse :: Newtype ListRoleAliasesResponse _
derive instance repGenericListRoleAliasesResponse :: Generic ListRoleAliasesResponse _
instance showListRoleAliasesResponse :: Show ListRoleAliasesResponse where
  show = genericShow
instance decodeListRoleAliasesResponse :: Decode ListRoleAliasesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRoleAliasesResponse :: Encode ListRoleAliasesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStreamsRequest = ListStreamsRequest 
  { "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "AscendingOrder'" :: NullOrUndefined.NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListStreamsRequest :: Newtype ListStreamsRequest _
derive instance repGenericListStreamsRequest :: Generic ListStreamsRequest _
instance showListStreamsRequest :: Show ListStreamsRequest where
  show = genericShow
instance decodeListStreamsRequest :: Decode ListStreamsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStreamsRequest :: Encode ListStreamsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStreamsResponse = ListStreamsResponse 
  { "Streams'" :: NullOrUndefined.NullOrUndefined (StreamsSummary)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListStreamsResponse :: Newtype ListStreamsResponse _
derive instance repGenericListStreamsResponse :: Generic ListStreamsResponse _
instance showListStreamsResponse :: Show ListStreamsResponse where
  show = genericShow
instance decodeListStreamsResponse :: Decode ListStreamsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStreamsResponse :: Encode ListStreamsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTargetsForPolicyRequest = ListTargetsForPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Marker'" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeListTargetsForPolicyRequest :: Newtype ListTargetsForPolicyRequest _
derive instance repGenericListTargetsForPolicyRequest :: Generic ListTargetsForPolicyRequest _
instance showListTargetsForPolicyRequest :: Show ListTargetsForPolicyRequest where
  show = genericShow
instance decodeListTargetsForPolicyRequest :: Decode ListTargetsForPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTargetsForPolicyRequest :: Encode ListTargetsForPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTargetsForPolicyResponse = ListTargetsForPolicyResponse 
  { "Targets'" :: NullOrUndefined.NullOrUndefined (PolicyTargets)
  , "NextMarker'" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListTargetsForPolicyResponse :: Newtype ListTargetsForPolicyResponse _
derive instance repGenericListTargetsForPolicyResponse :: Generic ListTargetsForPolicyResponse _
instance showListTargetsForPolicyResponse :: Show ListTargetsForPolicyResponse where
  show = genericShow
instance decodeListTargetsForPolicyResponse :: Decode ListTargetsForPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTargetsForPolicyResponse :: Encode ListTargetsForPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListThingGroupsForThingRequest = ListThingGroupsForThingRequest 
  { "ThingName'" :: (ThingName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (RegistryMaxResults)
  }
derive instance newtypeListThingGroupsForThingRequest :: Newtype ListThingGroupsForThingRequest _
derive instance repGenericListThingGroupsForThingRequest :: Generic ListThingGroupsForThingRequest _
instance showListThingGroupsForThingRequest :: Show ListThingGroupsForThingRequest where
  show = genericShow
instance decodeListThingGroupsForThingRequest :: Decode ListThingGroupsForThingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingGroupsForThingRequest :: Encode ListThingGroupsForThingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListThingGroupsForThingResponse = ListThingGroupsForThingResponse 
  { "ThingGroups'" :: NullOrUndefined.NullOrUndefined (ThingGroupNameAndArnList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListThingGroupsForThingResponse :: Newtype ListThingGroupsForThingResponse _
derive instance repGenericListThingGroupsForThingResponse :: Generic ListThingGroupsForThingResponse _
instance showListThingGroupsForThingResponse :: Show ListThingGroupsForThingResponse where
  show = genericShow
instance decodeListThingGroupsForThingResponse :: Decode ListThingGroupsForThingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingGroupsForThingResponse :: Encode ListThingGroupsForThingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListThingGroupsRequest = ListThingGroupsRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (RegistryMaxResults)
  , "ParentGroup'" :: NullOrUndefined.NullOrUndefined (ThingGroupName)
  , "NamePrefixFilter'" :: NullOrUndefined.NullOrUndefined (ThingGroupName)
  , "Recursive'" :: NullOrUndefined.NullOrUndefined (RecursiveWithoutDefault)
  }
derive instance newtypeListThingGroupsRequest :: Newtype ListThingGroupsRequest _
derive instance repGenericListThingGroupsRequest :: Generic ListThingGroupsRequest _
instance showListThingGroupsRequest :: Show ListThingGroupsRequest where
  show = genericShow
instance decodeListThingGroupsRequest :: Decode ListThingGroupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingGroupsRequest :: Encode ListThingGroupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListThingGroupsResponse = ListThingGroupsResponse 
  { "ThingGroups'" :: NullOrUndefined.NullOrUndefined (ThingGroupNameAndArnList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListThingGroupsResponse :: Newtype ListThingGroupsResponse _
derive instance repGenericListThingGroupsResponse :: Generic ListThingGroupsResponse _
instance showListThingGroupsResponse :: Show ListThingGroupsResponse where
  show = genericShow
instance decodeListThingGroupsResponse :: Decode ListThingGroupsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingGroupsResponse :: Encode ListThingGroupsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the ListThingPrincipal operation.</p>
newtype ListThingPrincipalsRequest = ListThingPrincipalsRequest 
  { "ThingName'" :: (ThingName)
  }
derive instance newtypeListThingPrincipalsRequest :: Newtype ListThingPrincipalsRequest _
derive instance repGenericListThingPrincipalsRequest :: Generic ListThingPrincipalsRequest _
instance showListThingPrincipalsRequest :: Show ListThingPrincipalsRequest where
  show = genericShow
instance decodeListThingPrincipalsRequest :: Decode ListThingPrincipalsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingPrincipalsRequest :: Encode ListThingPrincipalsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the ListThingPrincipals operation.</p>
newtype ListThingPrincipalsResponse = ListThingPrincipalsResponse 
  { "Principals'" :: NullOrUndefined.NullOrUndefined (Principals)
  }
derive instance newtypeListThingPrincipalsResponse :: Newtype ListThingPrincipalsResponse _
derive instance repGenericListThingPrincipalsResponse :: Generic ListThingPrincipalsResponse _
instance showListThingPrincipalsResponse :: Show ListThingPrincipalsResponse where
  show = genericShow
instance decodeListThingPrincipalsResponse :: Decode ListThingPrincipalsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingPrincipalsResponse :: Encode ListThingPrincipalsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListThingRegistrationTaskReportsRequest = ListThingRegistrationTaskReportsRequest 
  { "TaskId'" :: (TaskId)
  , "ReportType'" :: (ReportType)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (RegistryMaxResults)
  }
derive instance newtypeListThingRegistrationTaskReportsRequest :: Newtype ListThingRegistrationTaskReportsRequest _
derive instance repGenericListThingRegistrationTaskReportsRequest :: Generic ListThingRegistrationTaskReportsRequest _
instance showListThingRegistrationTaskReportsRequest :: Show ListThingRegistrationTaskReportsRequest where
  show = genericShow
instance decodeListThingRegistrationTaskReportsRequest :: Decode ListThingRegistrationTaskReportsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingRegistrationTaskReportsRequest :: Encode ListThingRegistrationTaskReportsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListThingRegistrationTaskReportsResponse = ListThingRegistrationTaskReportsResponse 
  { "ResourceLinks'" :: NullOrUndefined.NullOrUndefined (S3FileUrlList)
  , "ReportType'" :: NullOrUndefined.NullOrUndefined (ReportType)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListThingRegistrationTaskReportsResponse :: Newtype ListThingRegistrationTaskReportsResponse _
derive instance repGenericListThingRegistrationTaskReportsResponse :: Generic ListThingRegistrationTaskReportsResponse _
instance showListThingRegistrationTaskReportsResponse :: Show ListThingRegistrationTaskReportsResponse where
  show = genericShow
instance decodeListThingRegistrationTaskReportsResponse :: Decode ListThingRegistrationTaskReportsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingRegistrationTaskReportsResponse :: Encode ListThingRegistrationTaskReportsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListThingRegistrationTasksRequest = ListThingRegistrationTasksRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (RegistryMaxResults)
  , "Status'" :: NullOrUndefined.NullOrUndefined (Status)
  }
derive instance newtypeListThingRegistrationTasksRequest :: Newtype ListThingRegistrationTasksRequest _
derive instance repGenericListThingRegistrationTasksRequest :: Generic ListThingRegistrationTasksRequest _
instance showListThingRegistrationTasksRequest :: Show ListThingRegistrationTasksRequest where
  show = genericShow
instance decodeListThingRegistrationTasksRequest :: Decode ListThingRegistrationTasksRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingRegistrationTasksRequest :: Encode ListThingRegistrationTasksRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListThingRegistrationTasksResponse = ListThingRegistrationTasksResponse 
  { "TaskIds'" :: NullOrUndefined.NullOrUndefined (TaskIdList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListThingRegistrationTasksResponse :: Newtype ListThingRegistrationTasksResponse _
derive instance repGenericListThingRegistrationTasksResponse :: Generic ListThingRegistrationTasksResponse _
instance showListThingRegistrationTasksResponse :: Show ListThingRegistrationTasksResponse where
  show = genericShow
instance decodeListThingRegistrationTasksResponse :: Decode ListThingRegistrationTasksResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingRegistrationTasksResponse :: Encode ListThingRegistrationTasksResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the ListThingTypes operation.</p>
newtype ListThingTypesRequest = ListThingTypesRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (RegistryMaxResults)
  , "ThingTypeName'" :: NullOrUndefined.NullOrUndefined (ThingTypeName)
  }
derive instance newtypeListThingTypesRequest :: Newtype ListThingTypesRequest _
derive instance repGenericListThingTypesRequest :: Generic ListThingTypesRequest _
instance showListThingTypesRequest :: Show ListThingTypesRequest where
  show = genericShow
instance decodeListThingTypesRequest :: Decode ListThingTypesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingTypesRequest :: Encode ListThingTypesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the ListThingTypes operation.</p>
newtype ListThingTypesResponse = ListThingTypesResponse 
  { "ThingTypes'" :: NullOrUndefined.NullOrUndefined (ThingTypeList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListThingTypesResponse :: Newtype ListThingTypesResponse _
derive instance repGenericListThingTypesResponse :: Generic ListThingTypesResponse _
instance showListThingTypesResponse :: Show ListThingTypesResponse where
  show = genericShow
instance decodeListThingTypesResponse :: Decode ListThingTypesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingTypesResponse :: Encode ListThingTypesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListThingsInThingGroupRequest = ListThingsInThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  , "Recursive'" :: NullOrUndefined.NullOrUndefined (Recursive)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (RegistryMaxResults)
  }
derive instance newtypeListThingsInThingGroupRequest :: Newtype ListThingsInThingGroupRequest _
derive instance repGenericListThingsInThingGroupRequest :: Generic ListThingsInThingGroupRequest _
instance showListThingsInThingGroupRequest :: Show ListThingsInThingGroupRequest where
  show = genericShow
instance decodeListThingsInThingGroupRequest :: Decode ListThingsInThingGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingsInThingGroupRequest :: Encode ListThingsInThingGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListThingsInThingGroupResponse = ListThingsInThingGroupResponse 
  { "Things'" :: NullOrUndefined.NullOrUndefined (ThingNameList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListThingsInThingGroupResponse :: Newtype ListThingsInThingGroupResponse _
derive instance repGenericListThingsInThingGroupResponse :: Generic ListThingsInThingGroupResponse _
instance showListThingsInThingGroupResponse :: Show ListThingsInThingGroupResponse where
  show = genericShow
instance decodeListThingsInThingGroupResponse :: Decode ListThingsInThingGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingsInThingGroupResponse :: Encode ListThingsInThingGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the ListThings operation.</p>
newtype ListThingsRequest = ListThingsRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (RegistryMaxResults)
  , "AttributeName'" :: NullOrUndefined.NullOrUndefined (AttributeName)
  , "AttributeValue'" :: NullOrUndefined.NullOrUndefined (AttributeValue)
  , "ThingTypeName'" :: NullOrUndefined.NullOrUndefined (ThingTypeName)
  }
derive instance newtypeListThingsRequest :: Newtype ListThingsRequest _
derive instance repGenericListThingsRequest :: Generic ListThingsRequest _
instance showListThingsRequest :: Show ListThingsRequest where
  show = genericShow
instance decodeListThingsRequest :: Decode ListThingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingsRequest :: Encode ListThingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the ListThings operation.</p>
newtype ListThingsResponse = ListThingsResponse 
  { "Things'" :: NullOrUndefined.NullOrUndefined (ThingAttributeList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListThingsResponse :: Newtype ListThingsResponse _
derive instance repGenericListThingsResponse :: Generic ListThingsResponse _
instance showListThingsResponse :: Show ListThingsResponse where
  show = genericShow
instance decodeListThingsResponse :: Decode ListThingsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThingsResponse :: Encode ListThingsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the ListTopicRules operation.</p>
newtype ListTopicRulesRequest = ListTopicRulesRequest 
  { "Topic'" :: NullOrUndefined.NullOrUndefined (Topic)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (GEMaxResults)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "RuleDisabled'" :: NullOrUndefined.NullOrUndefined (IsDisabled)
  }
derive instance newtypeListTopicRulesRequest :: Newtype ListTopicRulesRequest _
derive instance repGenericListTopicRulesRequest :: Generic ListTopicRulesRequest _
instance showListTopicRulesRequest :: Show ListTopicRulesRequest where
  show = genericShow
instance decodeListTopicRulesRequest :: Decode ListTopicRulesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTopicRulesRequest :: Encode ListTopicRulesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the ListTopicRules operation.</p>
newtype ListTopicRulesResponse = ListTopicRulesResponse 
  { "Rules'" :: NullOrUndefined.NullOrUndefined (TopicRuleList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListTopicRulesResponse :: Newtype ListTopicRulesResponse _
derive instance repGenericListTopicRulesResponse :: Generic ListTopicRulesResponse _
instance showListTopicRulesResponse :: Show ListTopicRulesResponse where
  show = genericShow
instance decodeListTopicRulesResponse :: Decode ListTopicRulesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTopicRulesResponse :: Encode ListTopicRulesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListV2LoggingLevelsRequest = ListV2LoggingLevelsRequest 
  { "TargetType'" :: NullOrUndefined.NullOrUndefined (LogTargetType)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (SkyfallMaxResults)
  }
derive instance newtypeListV2LoggingLevelsRequest :: Newtype ListV2LoggingLevelsRequest _
derive instance repGenericListV2LoggingLevelsRequest :: Generic ListV2LoggingLevelsRequest _
instance showListV2LoggingLevelsRequest :: Show ListV2LoggingLevelsRequest where
  show = genericShow
instance decodeListV2LoggingLevelsRequest :: Decode ListV2LoggingLevelsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListV2LoggingLevelsRequest :: Encode ListV2LoggingLevelsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListV2LoggingLevelsResponse = ListV2LoggingLevelsResponse 
  { "LogTargetConfigurations'" :: NullOrUndefined.NullOrUndefined (LogTargetConfigurations)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListV2LoggingLevelsResponse :: Newtype ListV2LoggingLevelsResponse _
derive instance repGenericListV2LoggingLevelsResponse :: Generic ListV2LoggingLevelsResponse _
instance showListV2LoggingLevelsResponse :: Show ListV2LoggingLevelsResponse where
  show = genericShow
instance decodeListV2LoggingLevelsResponse :: Decode ListV2LoggingLevelsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListV2LoggingLevelsResponse :: Encode ListV2LoggingLevelsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogLevel = LogLevel String
derive instance newtypeLogLevel :: Newtype LogLevel _
derive instance repGenericLogLevel :: Generic LogLevel _
instance showLogLevel :: Show LogLevel where
  show = genericShow
instance decodeLogLevel :: Decode LogLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogLevel :: Encode LogLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A log target.</p>
newtype LogTarget = LogTarget 
  { "TargetType'" :: (LogTargetType)
  , "TargetName'" :: NullOrUndefined.NullOrUndefined (LogTargetName)
  }
derive instance newtypeLogTarget :: Newtype LogTarget _
derive instance repGenericLogTarget :: Generic LogTarget _
instance showLogTarget :: Show LogTarget where
  show = genericShow
instance decodeLogTarget :: Decode LogTarget where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogTarget :: Encode LogTarget where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The target configuration.</p>
newtype LogTargetConfiguration = LogTargetConfiguration 
  { "LogTarget'" :: NullOrUndefined.NullOrUndefined (LogTarget)
  , "LogLevel'" :: NullOrUndefined.NullOrUndefined (LogLevel)
  }
derive instance newtypeLogTargetConfiguration :: Newtype LogTargetConfiguration _
derive instance repGenericLogTargetConfiguration :: Generic LogTargetConfiguration _
instance showLogTargetConfiguration :: Show LogTargetConfiguration where
  show = genericShow
instance decodeLogTargetConfiguration :: Decode LogTargetConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogTargetConfiguration :: Encode LogTargetConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogTargetConfigurations = LogTargetConfigurations (Array LogTargetConfiguration)
derive instance newtypeLogTargetConfigurations :: Newtype LogTargetConfigurations _
derive instance repGenericLogTargetConfigurations :: Generic LogTargetConfigurations _
instance showLogTargetConfigurations :: Show LogTargetConfigurations where
  show = genericShow
instance decodeLogTargetConfigurations :: Decode LogTargetConfigurations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogTargetConfigurations :: Encode LogTargetConfigurations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogTargetName = LogTargetName String
derive instance newtypeLogTargetName :: Newtype LogTargetName _
derive instance repGenericLogTargetName :: Generic LogTargetName _
instance showLogTargetName :: Show LogTargetName where
  show = genericShow
instance decodeLogTargetName :: Decode LogTargetName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogTargetName :: Encode LogTargetName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogTargetType = LogTargetType String
derive instance newtypeLogTargetType :: Newtype LogTargetType _
derive instance repGenericLogTargetType :: Generic LogTargetType _
instance showLogTargetType :: Show LogTargetType where
  show = genericShow
instance decodeLogTargetType :: Decode LogTargetType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogTargetType :: Encode LogTargetType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the logging options payload.</p>
newtype LoggingOptionsPayload = LoggingOptionsPayload 
  { "RoleArn'" :: (AwsArn)
  , "LogLevel'" :: NullOrUndefined.NullOrUndefined (LogLevel)
  }
derive instance newtypeLoggingOptionsPayload :: Newtype LoggingOptionsPayload _
derive instance repGenericLoggingOptionsPayload :: Generic LoggingOptionsPayload _
instance showLoggingOptionsPayload :: Show LoggingOptionsPayload where
  show = genericShow
instance decodeLoggingOptionsPayload :: Decode LoggingOptionsPayload where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoggingOptionsPayload :: Encode LoggingOptionsPayload where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The policy documentation is not valid.</p>
newtype MalformedPolicyException = MalformedPolicyException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeMalformedPolicyException :: Newtype MalformedPolicyException _
derive instance repGenericMalformedPolicyException :: Generic MalformedPolicyException _
instance showMalformedPolicyException :: Show MalformedPolicyException where
  show = genericShow
instance decodeMalformedPolicyException :: Decode MalformedPolicyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMalformedPolicyException :: Encode MalformedPolicyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Marker = Marker String
derive instance newtypeMarker :: Newtype Marker _
derive instance repGenericMarker :: Generic Marker _
instance showMarker :: Show Marker where
  show = genericShow
instance decodeMarker :: Decode Marker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarker :: Encode Marker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxJobExecutionsPerMin = MaxJobExecutionsPerMin Int
derive instance newtypeMaxJobExecutionsPerMin :: Newtype MaxJobExecutionsPerMin _
derive instance repGenericMaxJobExecutionsPerMin :: Generic MaxJobExecutionsPerMin _
instance showMaxJobExecutionsPerMin :: Show MaxJobExecutionsPerMin where
  show = genericShow
instance decodeMaxJobExecutionsPerMin :: Decode MaxJobExecutionsPerMin where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxJobExecutionsPerMin :: Encode MaxJobExecutionsPerMin where
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


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageFormat = MessageFormat String
derive instance newtypeMessageFormat :: Newtype MessageFormat _
derive instance repGenericMessageFormat :: Generic MessageFormat _
instance showMessageFormat :: Show MessageFormat where
  show = genericShow
instance decodeMessageFormat :: Decode MessageFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageFormat :: Encode MessageFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricName = MetricName String
derive instance newtypeMetricName :: Newtype MetricName _
derive instance repGenericMetricName :: Generic MetricName _
instance showMetricName :: Show MetricName where
  show = genericShow
instance decodeMetricName :: Decode MetricName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricName :: Encode MetricName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricNamespace = MetricNamespace String
derive instance newtypeMetricNamespace :: Newtype MetricNamespace _
derive instance repGenericMetricNamespace :: Generic MetricNamespace _
instance showMetricNamespace :: Show MetricNamespace where
  show = genericShow
instance decodeMetricNamespace :: Decode MetricNamespace where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricNamespace :: Encode MetricNamespace where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricTimestamp = MetricTimestamp String
derive instance newtypeMetricTimestamp :: Newtype MetricTimestamp _
derive instance repGenericMetricTimestamp :: Generic MetricTimestamp _
instance showMetricTimestamp :: Show MetricTimestamp where
  show = genericShow
instance decodeMetricTimestamp :: Decode MetricTimestamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricTimestamp :: Encode MetricTimestamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricUnit = MetricUnit String
derive instance newtypeMetricUnit :: Newtype MetricUnit _
derive instance repGenericMetricUnit :: Generic MetricUnit _
instance showMetricUnit :: Show MetricUnit where
  show = genericShow
instance decodeMetricUnit :: Decode MetricUnit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricUnit :: Encode MetricUnit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricValue = MetricValue String
derive instance newtypeMetricValue :: Newtype MetricValue _
derive instance repGenericMetricValue :: Generic MetricValue _
instance showMetricValue :: Show MetricValue where
  show = genericShow
instance decodeMetricValue :: Decode MetricValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricValue :: Encode MetricValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MissingContextValue = MissingContextValue String
derive instance newtypeMissingContextValue :: Newtype MissingContextValue _
derive instance repGenericMissingContextValue :: Generic MissingContextValue _
instance showMissingContextValue :: Show MissingContextValue where
  show = genericShow
instance decodeMissingContextValue :: Decode MissingContextValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingContextValue :: Encode MissingContextValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MissingContextValues = MissingContextValues (Array MissingContextValue)
derive instance newtypeMissingContextValues :: Newtype MissingContextValues _
derive instance repGenericMissingContextValues :: Generic MissingContextValues _
instance showMissingContextValues :: Show MissingContextValues where
  show = genericShow
instance decodeMissingContextValues :: Decode MissingContextValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingContextValues :: Encode MissingContextValues where
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


-- | <p>The resource is not configured.</p>
newtype NotConfiguredException = NotConfiguredException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeNotConfiguredException :: Newtype NotConfiguredException _
derive instance repGenericNotConfiguredException :: Generic NotConfiguredException _
instance showNotConfiguredException :: Show NotConfiguredException where
  show = genericShow
instance decodeNotConfiguredException :: Decode NotConfiguredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotConfiguredException :: Encode NotConfiguredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OTAUpdateArn = OTAUpdateArn String
derive instance newtypeOTAUpdateArn :: Newtype OTAUpdateArn _
derive instance repGenericOTAUpdateArn :: Generic OTAUpdateArn _
instance showOTAUpdateArn :: Show OTAUpdateArn where
  show = genericShow
instance decodeOTAUpdateArn :: Decode OTAUpdateArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOTAUpdateArn :: Encode OTAUpdateArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OTAUpdateDescription = OTAUpdateDescription String
derive instance newtypeOTAUpdateDescription :: Newtype OTAUpdateDescription _
derive instance repGenericOTAUpdateDescription :: Generic OTAUpdateDescription _
instance showOTAUpdateDescription :: Show OTAUpdateDescription where
  show = genericShow
instance decodeOTAUpdateDescription :: Decode OTAUpdateDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOTAUpdateDescription :: Encode OTAUpdateDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OTAUpdateErrorMessage = OTAUpdateErrorMessage String
derive instance newtypeOTAUpdateErrorMessage :: Newtype OTAUpdateErrorMessage _
derive instance repGenericOTAUpdateErrorMessage :: Generic OTAUpdateErrorMessage _
instance showOTAUpdateErrorMessage :: Show OTAUpdateErrorMessage where
  show = genericShow
instance decodeOTAUpdateErrorMessage :: Decode OTAUpdateErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOTAUpdateErrorMessage :: Encode OTAUpdateErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a file to be associated with an OTA update.</p>
newtype OTAUpdateFile = OTAUpdateFile 
  { "FileName'" :: NullOrUndefined.NullOrUndefined (FileName)
  , "FileVersion'" :: NullOrUndefined.NullOrUndefined (OTAUpdateFileVersion)
  , "FileSource'" :: NullOrUndefined.NullOrUndefined (Stream)
  , "CodeSigning'" :: NullOrUndefined.NullOrUndefined (CodeSigning)
  , "Attributes'" :: NullOrUndefined.NullOrUndefined (AttributesMap)
  }
derive instance newtypeOTAUpdateFile :: Newtype OTAUpdateFile _
derive instance repGenericOTAUpdateFile :: Generic OTAUpdateFile _
instance showOTAUpdateFile :: Show OTAUpdateFile where
  show = genericShow
instance decodeOTAUpdateFile :: Decode OTAUpdateFile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOTAUpdateFile :: Encode OTAUpdateFile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OTAUpdateFileVersion = OTAUpdateFileVersion String
derive instance newtypeOTAUpdateFileVersion :: Newtype OTAUpdateFileVersion _
derive instance repGenericOTAUpdateFileVersion :: Generic OTAUpdateFileVersion _
instance showOTAUpdateFileVersion :: Show OTAUpdateFileVersion where
  show = genericShow
instance decodeOTAUpdateFileVersion :: Decode OTAUpdateFileVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOTAUpdateFileVersion :: Encode OTAUpdateFileVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OTAUpdateFiles = OTAUpdateFiles (Array OTAUpdateFile)
derive instance newtypeOTAUpdateFiles :: Newtype OTAUpdateFiles _
derive instance repGenericOTAUpdateFiles :: Generic OTAUpdateFiles _
instance showOTAUpdateFiles :: Show OTAUpdateFiles where
  show = genericShow
instance decodeOTAUpdateFiles :: Decode OTAUpdateFiles where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOTAUpdateFiles :: Encode OTAUpdateFiles where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OTAUpdateId = OTAUpdateId String
derive instance newtypeOTAUpdateId :: Newtype OTAUpdateId _
derive instance repGenericOTAUpdateId :: Generic OTAUpdateId _
instance showOTAUpdateId :: Show OTAUpdateId where
  show = genericShow
instance decodeOTAUpdateId :: Decode OTAUpdateId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOTAUpdateId :: Encode OTAUpdateId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about an OTA update.</p>
newtype OTAUpdateInfo = OTAUpdateInfo 
  { "OtaUpdateId'" :: NullOrUndefined.NullOrUndefined (OTAUpdateId)
  , "OtaUpdateArn'" :: NullOrUndefined.NullOrUndefined (OTAUpdateArn)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "LastModifiedDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "Description'" :: NullOrUndefined.NullOrUndefined (OTAUpdateDescription)
  , "Targets'" :: NullOrUndefined.NullOrUndefined (Targets)
  , "TargetSelection'" :: NullOrUndefined.NullOrUndefined (TargetSelection)
  , "OtaUpdateFiles'" :: NullOrUndefined.NullOrUndefined (OTAUpdateFiles)
  , "OtaUpdateStatus'" :: NullOrUndefined.NullOrUndefined (OTAUpdateStatus)
  , "AwsIotJobId'" :: NullOrUndefined.NullOrUndefined (AwsIotJobId)
  , "AwsIotJobArn'" :: NullOrUndefined.NullOrUndefined (AwsIotJobArn)
  , "ErrorInfo'" :: NullOrUndefined.NullOrUndefined (ErrorInfo)
  , "AdditionalParameters'" :: NullOrUndefined.NullOrUndefined (AdditionalParameterMap)
  }
derive instance newtypeOTAUpdateInfo :: Newtype OTAUpdateInfo _
derive instance repGenericOTAUpdateInfo :: Generic OTAUpdateInfo _
instance showOTAUpdateInfo :: Show OTAUpdateInfo where
  show = genericShow
instance decodeOTAUpdateInfo :: Decode OTAUpdateInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOTAUpdateInfo :: Encode OTAUpdateInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OTAUpdateStatus = OTAUpdateStatus String
derive instance newtypeOTAUpdateStatus :: Newtype OTAUpdateStatus _
derive instance repGenericOTAUpdateStatus :: Generic OTAUpdateStatus _
instance showOTAUpdateStatus :: Show OTAUpdateStatus where
  show = genericShow
instance decodeOTAUpdateStatus :: Decode OTAUpdateStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOTAUpdateStatus :: Encode OTAUpdateStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An OTA update summary.</p>
newtype OTAUpdateSummary = OTAUpdateSummary 
  { "OtaUpdateId'" :: NullOrUndefined.NullOrUndefined (OTAUpdateId)
  , "OtaUpdateArn'" :: NullOrUndefined.NullOrUndefined (OTAUpdateArn)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeOTAUpdateSummary :: Newtype OTAUpdateSummary _
derive instance repGenericOTAUpdateSummary :: Generic OTAUpdateSummary _
instance showOTAUpdateSummary :: Show OTAUpdateSummary where
  show = genericShow
instance decodeOTAUpdateSummary :: Decode OTAUpdateSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOTAUpdateSummary :: Encode OTAUpdateSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OTAUpdatesSummary = OTAUpdatesSummary (Array OTAUpdateSummary)
derive instance newtypeOTAUpdatesSummary :: Newtype OTAUpdatesSummary _
derive instance repGenericOTAUpdatesSummary :: Generic OTAUpdatesSummary _
instance showOTAUpdatesSummary :: Show OTAUpdatesSummary where
  show = genericShow
instance decodeOTAUpdatesSummary :: Decode OTAUpdatesSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOTAUpdatesSummary :: Encode OTAUpdatesSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionalVersion = OptionalVersion Number
derive instance newtypeOptionalVersion :: Newtype OptionalVersion _
derive instance repGenericOptionalVersion :: Generic OptionalVersion _
instance showOptionalVersion :: Show OptionalVersion where
  show = genericShow
instance decodeOptionalVersion :: Decode OptionalVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionalVersion :: Encode OptionalVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A certificate that has been transferred but not yet accepted.</p>
newtype OutgoingCertificate = OutgoingCertificate 
  { "CertificateArn'" :: NullOrUndefined.NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined.NullOrUndefined (CertificateId)
  , "TransferredTo'" :: NullOrUndefined.NullOrUndefined (AwsAccountId)
  , "TransferDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "TransferMessage'" :: NullOrUndefined.NullOrUndefined (Message)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeOutgoingCertificate :: Newtype OutgoingCertificate _
derive instance repGenericOutgoingCertificate :: Generic OutgoingCertificate _
instance showOutgoingCertificate :: Show OutgoingCertificate where
  show = genericShow
instance decodeOutgoingCertificate :: Decode OutgoingCertificate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutgoingCertificate :: Encode OutgoingCertificate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OutgoingCertificates = OutgoingCertificates (Array OutgoingCertificate)
derive instance newtypeOutgoingCertificates :: Newtype OutgoingCertificates _
derive instance repGenericOutgoingCertificates :: Generic OutgoingCertificates _
instance showOutgoingCertificates :: Show OutgoingCertificates where
  show = genericShow
instance decodeOutgoingCertificates :: Decode OutgoingCertificates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutgoingCertificates :: Encode OutgoingCertificates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _
derive instance repGenericPageSize :: Generic PageSize _
instance showPageSize :: Show PageSize where
  show = genericShow
instance decodePageSize :: Decode PageSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageSize :: Encode PageSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Parameter = Parameter String
derive instance newtypeParameter :: Newtype Parameter _
derive instance repGenericParameter :: Generic Parameter _
instance showParameter :: Show Parameter where
  show = genericShow
instance decodeParameter :: Decode Parameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameter :: Encode Parameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterKey = ParameterKey String
derive instance newtypeParameterKey :: Newtype ParameterKey _
derive instance repGenericParameterKey :: Generic ParameterKey _
instance showParameterKey :: Show ParameterKey where
  show = genericShow
instance decodeParameterKey :: Decode ParameterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterKey :: Encode ParameterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterValue = ParameterValue String
derive instance newtypeParameterValue :: Newtype ParameterValue _
derive instance repGenericParameterValue :: Generic ParameterValue _
instance showParameterValue :: Show ParameterValue where
  show = genericShow
instance decodeParameterValue :: Decode ParameterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterValue :: Encode ParameterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Parameters = Parameters (StrMap.StrMap Value)
derive instance newtypeParameters :: Newtype Parameters _
derive instance repGenericParameters :: Generic Parameters _
instance showParameters :: Show Parameters where
  show = genericShow
instance decodeParameters :: Decode Parameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameters :: Encode Parameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartitionKey = PartitionKey String
derive instance newtypePartitionKey :: Newtype PartitionKey _
derive instance repGenericPartitionKey :: Generic PartitionKey _
instance showPartitionKey :: Show PartitionKey where
  show = genericShow
instance decodePartitionKey :: Decode PartitionKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartitionKey :: Encode PartitionKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PayloadField = PayloadField String
derive instance newtypePayloadField :: Newtype PayloadField _
derive instance repGenericPayloadField :: Generic PayloadField _
instance showPayloadField :: Show PayloadField where
  show = genericShow
instance decodePayloadField :: Decode PayloadField where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePayloadField :: Encode PayloadField where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Percentage = Percentage Int
derive instance newtypePercentage :: Newtype Percentage _
derive instance repGenericPercentage :: Generic Percentage _
instance showPercentage :: Show Percentage where
  show = genericShow
instance decodePercentage :: Decode Percentage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePercentage :: Encode Percentage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Policies = Policies (Array Policy)
derive instance newtypePolicies :: Newtype Policies _
derive instance repGenericPolicies :: Generic Policies _
instance showPolicies :: Show Policies where
  show = genericShow
instance decodePolicies :: Decode Policies where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicies :: Encode Policies where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an AWS IoT policy.</p>
newtype Policy = Policy 
  { "PolicyName'" :: NullOrUndefined.NullOrUndefined (PolicyName)
  , "PolicyArn'" :: NullOrUndefined.NullOrUndefined (PolicyArn)
  }
derive instance newtypePolicy :: Newtype Policy _
derive instance repGenericPolicy :: Generic Policy _
instance showPolicy :: Show Policy where
  show = genericShow
instance decodePolicy :: Decode Policy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicy :: Encode Policy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyArn = PolicyArn String
derive instance newtypePolicyArn :: Newtype PolicyArn _
derive instance repGenericPolicyArn :: Generic PolicyArn _
instance showPolicyArn :: Show PolicyArn where
  show = genericShow
instance decodePolicyArn :: Decode PolicyArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyArn :: Encode PolicyArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyDocument = PolicyDocument String
derive instance newtypePolicyDocument :: Newtype PolicyDocument _
derive instance repGenericPolicyDocument :: Generic PolicyDocument _
instance showPolicyDocument :: Show PolicyDocument where
  show = genericShow
instance decodePolicyDocument :: Decode PolicyDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyDocument :: Encode PolicyDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyDocuments = PolicyDocuments (Array PolicyDocument)
derive instance newtypePolicyDocuments :: Newtype PolicyDocuments _
derive instance repGenericPolicyDocuments :: Generic PolicyDocuments _
instance showPolicyDocuments :: Show PolicyDocuments where
  show = genericShow
instance decodePolicyDocuments :: Decode PolicyDocuments where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyDocuments :: Encode PolicyDocuments where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyName = PolicyName String
derive instance newtypePolicyName :: Newtype PolicyName _
derive instance repGenericPolicyName :: Generic PolicyName _
instance showPolicyName :: Show PolicyName where
  show = genericShow
instance decodePolicyName :: Decode PolicyName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyName :: Encode PolicyName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyNames = PolicyNames (Array PolicyName)
derive instance newtypePolicyNames :: Newtype PolicyNames _
derive instance repGenericPolicyNames :: Generic PolicyNames _
instance showPolicyNames :: Show PolicyNames where
  show = genericShow
instance decodePolicyNames :: Decode PolicyNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyNames :: Encode PolicyNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyTarget = PolicyTarget String
derive instance newtypePolicyTarget :: Newtype PolicyTarget _
derive instance repGenericPolicyTarget :: Generic PolicyTarget _
instance showPolicyTarget :: Show PolicyTarget where
  show = genericShow
instance decodePolicyTarget :: Decode PolicyTarget where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyTarget :: Encode PolicyTarget where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyTargets = PolicyTargets (Array PolicyTarget)
derive instance newtypePolicyTargets :: Newtype PolicyTargets _
derive instance repGenericPolicyTargets :: Generic PolicyTargets _
instance showPolicyTargets :: Show PolicyTargets where
  show = genericShow
instance decodePolicyTargets :: Decode PolicyTargets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyTargets :: Encode PolicyTargets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a policy version.</p>
newtype PolicyVersion = PolicyVersion 
  { "VersionId'" :: NullOrUndefined.NullOrUndefined (PolicyVersionId)
  , "IsDefaultVersion'" :: NullOrUndefined.NullOrUndefined (IsDefaultVersion)
  , "CreateDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypePolicyVersion :: Newtype PolicyVersion _
derive instance repGenericPolicyVersion :: Generic PolicyVersion _
instance showPolicyVersion :: Show PolicyVersion where
  show = genericShow
instance decodePolicyVersion :: Decode PolicyVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyVersion :: Encode PolicyVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyVersionId = PolicyVersionId String
derive instance newtypePolicyVersionId :: Newtype PolicyVersionId _
derive instance repGenericPolicyVersionId :: Generic PolicyVersionId _
instance showPolicyVersionId :: Show PolicyVersionId where
  show = genericShow
instance decodePolicyVersionId :: Decode PolicyVersionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyVersionId :: Encode PolicyVersionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyVersions = PolicyVersions (Array PolicyVersion)
derive instance newtypePolicyVersions :: Newtype PolicyVersions _
derive instance repGenericPolicyVersions :: Generic PolicyVersions _
instance showPolicyVersions :: Show PolicyVersions where
  show = genericShow
instance decodePolicyVersions :: Decode PolicyVersions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyVersions :: Encode PolicyVersions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration for pre-signed S3 URLs.</p>
newtype PresignedUrlConfig = PresignedUrlConfig 
  { "RoleArn'" :: NullOrUndefined.NullOrUndefined (RoleArn)
  , "ExpiresInSec'" :: NullOrUndefined.NullOrUndefined (ExpiresInSec)
  }
derive instance newtypePresignedUrlConfig :: Newtype PresignedUrlConfig _
derive instance repGenericPresignedUrlConfig :: Generic PresignedUrlConfig _
instance showPresignedUrlConfig :: Show PresignedUrlConfig where
  show = genericShow
instance decodePresignedUrlConfig :: Decode PresignedUrlConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePresignedUrlConfig :: Encode PresignedUrlConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Principal = Principal String
derive instance newtypePrincipal :: Newtype Principal _
derive instance repGenericPrincipal :: Generic Principal _
instance showPrincipal :: Show Principal where
  show = genericShow
instance decodePrincipal :: Decode Principal where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrincipal :: Encode Principal where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PrincipalArn = PrincipalArn String
derive instance newtypePrincipalArn :: Newtype PrincipalArn _
derive instance repGenericPrincipalArn :: Generic PrincipalArn _
instance showPrincipalArn :: Show PrincipalArn where
  show = genericShow
instance decodePrincipalArn :: Decode PrincipalArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrincipalArn :: Encode PrincipalArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PrincipalId = PrincipalId String
derive instance newtypePrincipalId :: Newtype PrincipalId _
derive instance repGenericPrincipalId :: Generic PrincipalId _
instance showPrincipalId :: Show PrincipalId where
  show = genericShow
instance decodePrincipalId :: Decode PrincipalId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrincipalId :: Encode PrincipalId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Principals = Principals (Array PrincipalArn)
derive instance newtypePrincipals :: Newtype Principals _
derive instance repGenericPrincipals :: Generic Principals _
instance showPrincipals :: Show Principals where
  show = genericShow
instance decodePrincipals :: Decode Principals where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrincipals :: Encode Principals where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PrivateKey = PrivateKey String
derive instance newtypePrivateKey :: Newtype PrivateKey _
derive instance repGenericPrivateKey :: Generic PrivateKey _
instance showPrivateKey :: Show PrivateKey where
  show = genericShow
instance decodePrivateKey :: Decode PrivateKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrivateKey :: Encode PrivateKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProcessingTargetName = ProcessingTargetName String
derive instance newtypeProcessingTargetName :: Newtype ProcessingTargetName _
derive instance repGenericProcessingTargetName :: Generic ProcessingTargetName _
instance showProcessingTargetName :: Show ProcessingTargetName where
  show = genericShow
instance decodeProcessingTargetName :: Decode ProcessingTargetName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessingTargetName :: Encode ProcessingTargetName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProcessingTargetNameList = ProcessingTargetNameList (Array ProcessingTargetName)
derive instance newtypeProcessingTargetNameList :: Newtype ProcessingTargetNameList _
derive instance repGenericProcessingTargetNameList :: Generic ProcessingTargetNameList _
instance showProcessingTargetNameList :: Show ProcessingTargetNameList where
  show = genericShow
instance decodeProcessingTargetNameList :: Decode ProcessingTargetNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessingTargetNameList :: Encode ProcessingTargetNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PublicKey = PublicKey String
derive instance newtypePublicKey :: Newtype PublicKey _
derive instance repGenericPublicKey :: Generic PublicKey _
instance showPublicKey :: Show PublicKey where
  show = genericShow
instance decodePublicKey :: Decode PublicKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePublicKey :: Encode PublicKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PublicKeyMap = PublicKeyMap (StrMap.StrMap KeyValue)
derive instance newtypePublicKeyMap :: Newtype PublicKeyMap _
derive instance repGenericPublicKeyMap :: Generic PublicKeyMap _
instance showPublicKeyMap :: Show PublicKeyMap where
  show = genericShow
instance decodePublicKeyMap :: Decode PublicKeyMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePublicKeyMap :: Encode PublicKeyMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the DynamoActionVS action that specifies the DynamoDB table to which the message data will be written.</p>
newtype PutItemInput = PutItemInput 
  { "TableName'" :: (TableName)
  }
derive instance newtypePutItemInput :: Newtype PutItemInput _
derive instance repGenericPutItemInput :: Generic PutItemInput _
instance showPutItemInput :: Show PutItemInput where
  show = genericShow
instance decodePutItemInput :: Decode PutItemInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutItemInput :: Encode PutItemInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueryMaxResults = QueryMaxResults Int
derive instance newtypeQueryMaxResults :: Newtype QueryMaxResults _
derive instance repGenericQueryMaxResults :: Generic QueryMaxResults _
instance showQueryMaxResults :: Show QueryMaxResults where
  show = genericShow
instance decodeQueryMaxResults :: Decode QueryMaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryMaxResults :: Encode QueryMaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueryString = QueryString String
derive instance newtypeQueryString :: Newtype QueryString _
derive instance repGenericQueryString :: Generic QueryString _
instance showQueryString :: Show QueryString where
  show = genericShow
instance decodeQueryString :: Decode QueryString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryString :: Encode QueryString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueryVersion = QueryVersion String
derive instance newtypeQueryVersion :: Newtype QueryVersion _
derive instance repGenericQueryVersion :: Generic QueryVersion _
instance showQueryVersion :: Show QueryVersion where
  show = genericShow
instance decodeQueryVersion :: Decode QueryVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryVersion :: Encode QueryVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueueUrl = QueueUrl String
derive instance newtypeQueueUrl :: Newtype QueueUrl _
derive instance repGenericQueueUrl :: Generic QueueUrl _
instance showQueueUrl :: Show QueueUrl where
  show = genericShow
instance decodeQueueUrl :: Decode QueueUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueUrl :: Encode QueueUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueuedThings = QueuedThings Int
derive instance newtypeQueuedThings :: Newtype QueuedThings _
derive instance repGenericQueuedThings :: Generic QueuedThings _
instance showQueuedThings :: Show QueuedThings where
  show = genericShow
instance decodeQueuedThings :: Decode QueuedThings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueuedThings :: Encode QueuedThings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RangeKeyField = RangeKeyField String
derive instance newtypeRangeKeyField :: Newtype RangeKeyField _
derive instance repGenericRangeKeyField :: Generic RangeKeyField _
instance showRangeKeyField :: Show RangeKeyField where
  show = genericShow
instance decodeRangeKeyField :: Decode RangeKeyField where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRangeKeyField :: Encode RangeKeyField where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RangeKeyValue = RangeKeyValue String
derive instance newtypeRangeKeyValue :: Newtype RangeKeyValue _
derive instance repGenericRangeKeyValue :: Generic RangeKeyValue _
instance showRangeKeyValue :: Show RangeKeyValue where
  show = genericShow
instance decodeRangeKeyValue :: Decode RangeKeyValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRangeKeyValue :: Encode RangeKeyValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Recursive = Recursive Boolean
derive instance newtypeRecursive :: Newtype Recursive _
derive instance repGenericRecursive :: Generic Recursive _
instance showRecursive :: Show Recursive where
  show = genericShow
instance decodeRecursive :: Decode Recursive where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecursive :: Encode Recursive where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecursiveWithoutDefault = RecursiveWithoutDefault Boolean
derive instance newtypeRecursiveWithoutDefault :: Newtype RecursiveWithoutDefault _
derive instance repGenericRecursiveWithoutDefault :: Generic RecursiveWithoutDefault _
instance showRecursiveWithoutDefault :: Show RecursiveWithoutDefault where
  show = genericShow
instance decodeRecursiveWithoutDefault :: Decode RecursiveWithoutDefault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecursiveWithoutDefault :: Encode RecursiveWithoutDefault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input to the RegisterCACertificate operation.</p>
newtype RegisterCACertificateRequest = RegisterCACertificateRequest 
  { "CaCertificate'" :: (CertificatePem)
  , "VerificationCertificate'" :: (CertificatePem)
  , "SetAsActive'" :: NullOrUndefined.NullOrUndefined (SetAsActive)
  , "AllowAutoRegistration'" :: NullOrUndefined.NullOrUndefined (AllowAutoRegistration)
  , "RegistrationConfig'" :: NullOrUndefined.NullOrUndefined (RegistrationConfig)
  }
derive instance newtypeRegisterCACertificateRequest :: Newtype RegisterCACertificateRequest _
derive instance repGenericRegisterCACertificateRequest :: Generic RegisterCACertificateRequest _
instance showRegisterCACertificateRequest :: Show RegisterCACertificateRequest where
  show = genericShow
instance decodeRegisterCACertificateRequest :: Decode RegisterCACertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterCACertificateRequest :: Encode RegisterCACertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the RegisterCACertificateResponse operation.</p>
newtype RegisterCACertificateResponse = RegisterCACertificateResponse 
  { "CertificateArn'" :: NullOrUndefined.NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined.NullOrUndefined (CertificateId)
  }
derive instance newtypeRegisterCACertificateResponse :: Newtype RegisterCACertificateResponse _
derive instance repGenericRegisterCACertificateResponse :: Generic RegisterCACertificateResponse _
instance showRegisterCACertificateResponse :: Show RegisterCACertificateResponse where
  show = genericShow
instance decodeRegisterCACertificateResponse :: Decode RegisterCACertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterCACertificateResponse :: Encode RegisterCACertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input to the RegisterCertificate operation.</p>
newtype RegisterCertificateRequest = RegisterCertificateRequest 
  { "CertificatePem'" :: (CertificatePem)
  , "CaCertificatePem'" :: NullOrUndefined.NullOrUndefined (CertificatePem)
  , "SetAsActive'" :: NullOrUndefined.NullOrUndefined (SetAsActiveFlag)
  , "Status'" :: NullOrUndefined.NullOrUndefined (CertificateStatus)
  }
derive instance newtypeRegisterCertificateRequest :: Newtype RegisterCertificateRequest _
derive instance repGenericRegisterCertificateRequest :: Generic RegisterCertificateRequest _
instance showRegisterCertificateRequest :: Show RegisterCertificateRequest where
  show = genericShow
instance decodeRegisterCertificateRequest :: Decode RegisterCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterCertificateRequest :: Encode RegisterCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the RegisterCertificate operation.</p>
newtype RegisterCertificateResponse = RegisterCertificateResponse 
  { "CertificateArn'" :: NullOrUndefined.NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined.NullOrUndefined (CertificateId)
  }
derive instance newtypeRegisterCertificateResponse :: Newtype RegisterCertificateResponse _
derive instance repGenericRegisterCertificateResponse :: Generic RegisterCertificateResponse _
instance showRegisterCertificateResponse :: Show RegisterCertificateResponse where
  show = genericShow
instance decodeRegisterCertificateResponse :: Decode RegisterCertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterCertificateResponse :: Encode RegisterCertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterThingRequest = RegisterThingRequest 
  { "TemplateBody'" :: (TemplateBody)
  , "Parameters'" :: NullOrUndefined.NullOrUndefined (Parameters)
  }
derive instance newtypeRegisterThingRequest :: Newtype RegisterThingRequest _
derive instance repGenericRegisterThingRequest :: Generic RegisterThingRequest _
instance showRegisterThingRequest :: Show RegisterThingRequest where
  show = genericShow
instance decodeRegisterThingRequest :: Decode RegisterThingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterThingRequest :: Encode RegisterThingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterThingResponse = RegisterThingResponse 
  { "CertificatePem'" :: NullOrUndefined.NullOrUndefined (CertificatePem)
  , "ResourceArns'" :: NullOrUndefined.NullOrUndefined (ResourceArns)
  }
derive instance newtypeRegisterThingResponse :: Newtype RegisterThingResponse _
derive instance repGenericRegisterThingResponse :: Generic RegisterThingResponse _
instance showRegisterThingResponse :: Show RegisterThingResponse where
  show = genericShow
instance decodeRegisterThingResponse :: Decode RegisterThingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterThingResponse :: Encode RegisterThingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegistrationCode = RegistrationCode String
derive instance newtypeRegistrationCode :: Newtype RegistrationCode _
derive instance repGenericRegistrationCode :: Generic RegistrationCode _
instance showRegistrationCode :: Show RegistrationCode where
  show = genericShow
instance decodeRegistrationCode :: Decode RegistrationCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegistrationCode :: Encode RegistrationCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The registration code is invalid.</p>
newtype RegistrationCodeValidationException = RegistrationCodeValidationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeRegistrationCodeValidationException :: Newtype RegistrationCodeValidationException _
derive instance repGenericRegistrationCodeValidationException :: Generic RegistrationCodeValidationException _
instance showRegistrationCodeValidationException :: Show RegistrationCodeValidationException where
  show = genericShow
instance decodeRegistrationCodeValidationException :: Decode RegistrationCodeValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegistrationCodeValidationException :: Encode RegistrationCodeValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The registration configuration.</p>
newtype RegistrationConfig = RegistrationConfig 
  { "TemplateBody'" :: NullOrUndefined.NullOrUndefined (TemplateBody)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (RoleArn)
  }
derive instance newtypeRegistrationConfig :: Newtype RegistrationConfig _
derive instance repGenericRegistrationConfig :: Generic RegistrationConfig _
instance showRegistrationConfig :: Show RegistrationConfig where
  show = genericShow
instance decodeRegistrationConfig :: Decode RegistrationConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegistrationConfig :: Encode RegistrationConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegistryMaxResults = RegistryMaxResults Int
derive instance newtypeRegistryMaxResults :: Newtype RegistryMaxResults _
derive instance repGenericRegistryMaxResults :: Generic RegistryMaxResults _
instance showRegistryMaxResults :: Show RegistryMaxResults where
  show = genericShow
instance decodeRegistryMaxResults :: Decode RegistryMaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegistryMaxResults :: Encode RegistryMaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegistryS3BucketName = RegistryS3BucketName String
derive instance newtypeRegistryS3BucketName :: Newtype RegistryS3BucketName _
derive instance repGenericRegistryS3BucketName :: Generic RegistryS3BucketName _
instance showRegistryS3BucketName :: Show RegistryS3BucketName where
  show = genericShow
instance decodeRegistryS3BucketName :: Decode RegistryS3BucketName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegistryS3BucketName :: Encode RegistryS3BucketName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegistryS3KeyName = RegistryS3KeyName String
derive instance newtypeRegistryS3KeyName :: Newtype RegistryS3KeyName _
derive instance repGenericRegistryS3KeyName :: Generic RegistryS3KeyName _
instance showRegistryS3KeyName :: Show RegistryS3KeyName where
  show = genericShow
instance decodeRegistryS3KeyName :: Decode RegistryS3KeyName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegistryS3KeyName :: Encode RegistryS3KeyName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the RejectCertificateTransfer operation.</p>
newtype RejectCertificateTransferRequest = RejectCertificateTransferRequest 
  { "CertificateId'" :: (CertificateId)
  , "RejectReason'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeRejectCertificateTransferRequest :: Newtype RejectCertificateTransferRequest _
derive instance repGenericRejectCertificateTransferRequest :: Generic RejectCertificateTransferRequest _
instance showRejectCertificateTransferRequest :: Show RejectCertificateTransferRequest where
  show = genericShow
instance decodeRejectCertificateTransferRequest :: Decode RejectCertificateTransferRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRejectCertificateTransferRequest :: Encode RejectCertificateTransferRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RejectedThings = RejectedThings Int
derive instance newtypeRejectedThings :: Newtype RejectedThings _
derive instance repGenericRejectedThings :: Generic RejectedThings _
instance showRejectedThings :: Show RejectedThings where
  show = genericShow
instance decodeRejectedThings :: Decode RejectedThings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRejectedThings :: Encode RejectedThings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveAutoRegistration = RemoveAutoRegistration Boolean
derive instance newtypeRemoveAutoRegistration :: Newtype RemoveAutoRegistration _
derive instance repGenericRemoveAutoRegistration :: Generic RemoveAutoRegistration _
instance showRemoveAutoRegistration :: Show RemoveAutoRegistration where
  show = genericShow
instance decodeRemoveAutoRegistration :: Decode RemoveAutoRegistration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveAutoRegistration :: Encode RemoveAutoRegistration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveThingFromThingGroupRequest = RemoveThingFromThingGroupRequest 
  { "ThingGroupName'" :: NullOrUndefined.NullOrUndefined (ThingGroupName)
  , "ThingGroupArn'" :: NullOrUndefined.NullOrUndefined (ThingGroupArn)
  , "ThingName'" :: NullOrUndefined.NullOrUndefined (ThingName)
  , "ThingArn'" :: NullOrUndefined.NullOrUndefined (ThingArn)
  }
derive instance newtypeRemoveThingFromThingGroupRequest :: Newtype RemoveThingFromThingGroupRequest _
derive instance repGenericRemoveThingFromThingGroupRequest :: Generic RemoveThingFromThingGroupRequest _
instance showRemoveThingFromThingGroupRequest :: Show RemoveThingFromThingGroupRequest where
  show = genericShow
instance decodeRemoveThingFromThingGroupRequest :: Decode RemoveThingFromThingGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveThingFromThingGroupRequest :: Encode RemoveThingFromThingGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveThingFromThingGroupResponse = RemoveThingFromThingGroupResponse Types.NoArguments
derive instance newtypeRemoveThingFromThingGroupResponse :: Newtype RemoveThingFromThingGroupResponse _
derive instance repGenericRemoveThingFromThingGroupResponse :: Generic RemoveThingFromThingGroupResponse _
instance showRemoveThingFromThingGroupResponse :: Show RemoveThingFromThingGroupResponse where
  show = genericShow
instance decodeRemoveThingFromThingGroupResponse :: Decode RemoveThingFromThingGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveThingFromThingGroupResponse :: Encode RemoveThingFromThingGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveThingType = RemoveThingType Boolean
derive instance newtypeRemoveThingType :: Newtype RemoveThingType _
derive instance repGenericRemoveThingType :: Generic RemoveThingType _
instance showRemoveThingType :: Show RemoveThingType where
  show = genericShow
instance decodeRemoveThingType :: Decode RemoveThingType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveThingType :: Encode RemoveThingType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemovedThings = RemovedThings Int
derive instance newtypeRemovedThings :: Newtype RemovedThings _
derive instance repGenericRemovedThings :: Generic RemovedThings _
instance showRemovedThings :: Show RemovedThings where
  show = genericShow
instance decodeRemovedThings :: Decode RemovedThings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemovedThings :: Encode RemovedThings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the ReplaceTopicRule operation.</p>
newtype ReplaceTopicRuleRequest = ReplaceTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  , "TopicRulePayload'" :: (TopicRulePayload)
  }
derive instance newtypeReplaceTopicRuleRequest :: Newtype ReplaceTopicRuleRequest _
derive instance repGenericReplaceTopicRuleRequest :: Generic ReplaceTopicRuleRequest _
instance showReplaceTopicRuleRequest :: Show ReplaceTopicRuleRequest where
  show = genericShow
instance decodeReplaceTopicRuleRequest :: Decode ReplaceTopicRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplaceTopicRuleRequest :: Encode ReplaceTopicRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReportType = ReportType String
derive instance newtypeReportType :: Newtype ReportType _
derive instance repGenericReportType :: Generic ReportType _
instance showReportType :: Show ReportType where
  show = genericShow
instance decodeReportType :: Decode ReportType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReportType :: Encode ReportType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action to republish to another topic.</p>
newtype RepublishAction = RepublishAction 
  { "RoleArn'" :: (AwsArn)
  , "Topic'" :: (TopicPattern)
  }
derive instance newtypeRepublishAction :: Newtype RepublishAction _
derive instance repGenericRepublishAction :: Generic RepublishAction _
instance showRepublishAction :: Show RepublishAction where
  show = genericShow
instance decodeRepublishAction :: Decode RepublishAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepublishAction :: Encode RepublishAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Resource = Resource String
derive instance newtypeResource :: Newtype Resource _
derive instance repGenericResource :: Generic Resource _
instance showResource :: Show Resource where
  show = genericShow
instance decodeResource :: Decode Resource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResource :: Encode Resource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  , "ResourceId'" :: NullOrUndefined.NullOrUndefined (ResourceId')
  , "ResourceArn'" :: NullOrUndefined.NullOrUndefined (ResourceArn')
  }
derive instance newtypeResourceAlreadyExistsException :: Newtype ResourceAlreadyExistsException _
derive instance repGenericResourceAlreadyExistsException :: Generic ResourceAlreadyExistsException _
instance showResourceAlreadyExistsException :: Show ResourceAlreadyExistsException where
  show = genericShow
instance decodeResourceAlreadyExistsException :: Decode ResourceAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceAlreadyExistsException :: Encode ResourceAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _
derive instance repGenericResourceArn :: Generic ResourceArn _
instance showResourceArn :: Show ResourceArn where
  show = genericShow
instance decodeResourceArn :: Decode ResourceArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceArn :: Encode ResourceArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceArns = ResourceArns (StrMap.StrMap ResourceArn)
derive instance newtypeResourceArns :: Newtype ResourceArns _
derive instance repGenericResourceArns :: Generic ResourceArns _
instance showResourceArns :: Show ResourceArns where
  show = genericShow
instance decodeResourceArns :: Decode ResourceArns where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceArns :: Encode ResourceArns where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceLogicalId = ResourceLogicalId String
derive instance newtypeResourceLogicalId :: Newtype ResourceLogicalId _
derive instance repGenericResourceLogicalId :: Generic ResourceLogicalId _
instance showResourceLogicalId :: Show ResourceLogicalId where
  show = genericShow
instance decodeResourceLogicalId :: Decode ResourceLogicalId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceLogicalId :: Encode ResourceLogicalId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified resource does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource registration failed.</p>
newtype ResourceRegistrationFailureException = ResourceRegistrationFailureException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeResourceRegistrationFailureException :: Newtype ResourceRegistrationFailureException _
derive instance repGenericResourceRegistrationFailureException :: Generic ResourceRegistrationFailureException _
instance showResourceRegistrationFailureException :: Show ResourceRegistrationFailureException where
  show = genericShow
instance decodeResourceRegistrationFailureException :: Decode ResourceRegistrationFailureException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceRegistrationFailureException :: Encode ResourceRegistrationFailureException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Resources = Resources (Array Resource)
derive instance newtypeResources :: Newtype Resources _
derive instance repGenericResources :: Generic Resources _
instance showResources :: Show Resources where
  show = genericShow
instance decodeResources :: Decode Resources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResources :: Encode Resources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleAlias = RoleAlias String
derive instance newtypeRoleAlias :: Newtype RoleAlias _
derive instance repGenericRoleAlias :: Generic RoleAlias _
instance showRoleAlias :: Show RoleAlias where
  show = genericShow
instance decodeRoleAlias :: Decode RoleAlias where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleAlias :: Encode RoleAlias where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleAliasArn = RoleAliasArn String
derive instance newtypeRoleAliasArn :: Newtype RoleAliasArn _
derive instance repGenericRoleAliasArn :: Generic RoleAliasArn _
instance showRoleAliasArn :: Show RoleAliasArn where
  show = genericShow
instance decodeRoleAliasArn :: Decode RoleAliasArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleAliasArn :: Encode RoleAliasArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Role alias description.</p>
newtype RoleAliasDescription = RoleAliasDescription 
  { "RoleAlias'" :: NullOrUndefined.NullOrUndefined (RoleAlias)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (RoleArn)
  , "Owner'" :: NullOrUndefined.NullOrUndefined (AwsAccountId)
  , "CredentialDurationSeconds'" :: NullOrUndefined.NullOrUndefined (CredentialDurationSeconds)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "LastModifiedDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeRoleAliasDescription :: Newtype RoleAliasDescription _
derive instance repGenericRoleAliasDescription :: Generic RoleAliasDescription _
instance showRoleAliasDescription :: Show RoleAliasDescription where
  show = genericShow
instance decodeRoleAliasDescription :: Decode RoleAliasDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleAliasDescription :: Encode RoleAliasDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleAliases = RoleAliases (Array RoleAlias)
derive instance newtypeRoleAliases :: Newtype RoleAliases _
derive instance repGenericRoleAliases :: Generic RoleAliases _
instance showRoleAliases :: Show RoleAliases where
  show = genericShow
instance decodeRoleAliases :: Decode RoleAliases where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleAliases :: Encode RoleAliases where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleArn = RoleArn String
derive instance newtypeRoleArn :: Newtype RoleArn _
derive instance repGenericRoleArn :: Generic RoleArn _
instance showRoleArn :: Show RoleArn where
  show = genericShow
instance decodeRoleArn :: Decode RoleArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleArn :: Encode RoleArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleArn = RuleArn String
derive instance newtypeRuleArn :: Newtype RuleArn _
derive instance repGenericRuleArn :: Generic RuleArn _
instance showRuleArn :: Show RuleArn where
  show = genericShow
instance decodeRuleArn :: Decode RuleArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleArn :: Encode RuleArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleName = RuleName String
derive instance newtypeRuleName :: Newtype RuleName _
derive instance repGenericRuleName :: Generic RuleName _
instance showRuleName :: Show RuleName where
  show = genericShow
instance decodeRuleName :: Decode RuleName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleName :: Encode RuleName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action to write data to an Amazon S3 bucket.</p>
newtype S3Action = S3Action 
  { "RoleArn'" :: (AwsArn)
  , "BucketName'" :: (BucketName)
  , "Key'" :: (Key)
  , "CannedAcl'" :: NullOrUndefined.NullOrUndefined (CannedAccessControlList)
  }
derive instance newtypeS3Action :: Newtype S3Action _
derive instance repGenericS3Action :: Generic S3Action _
instance showS3Action :: Show S3Action where
  show = genericShow
instance decodeS3Action :: Decode S3Action where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Action :: Encode S3Action where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3Bucket = S3Bucket String
derive instance newtypeS3Bucket :: Newtype S3Bucket _
derive instance repGenericS3Bucket :: Generic S3Bucket _
instance showS3Bucket :: Show S3Bucket where
  show = genericShow
instance decodeS3Bucket :: Decode S3Bucket where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Bucket :: Encode S3Bucket where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3FileUrl = S3FileUrl String
derive instance newtypeS3FileUrl :: Newtype S3FileUrl _
derive instance repGenericS3FileUrl :: Generic S3FileUrl _
instance showS3FileUrl :: Show S3FileUrl where
  show = genericShow
instance decodeS3FileUrl :: Decode S3FileUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3FileUrl :: Encode S3FileUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3FileUrlList = S3FileUrlList (Array S3FileUrl)
derive instance newtypeS3FileUrlList :: Newtype S3FileUrlList _
derive instance repGenericS3FileUrlList :: Generic S3FileUrlList _
instance showS3FileUrlList :: Show S3FileUrlList where
  show = genericShow
instance decodeS3FileUrlList :: Decode S3FileUrlList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3FileUrlList :: Encode S3FileUrlList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3Key = S3Key String
derive instance newtypeS3Key :: Newtype S3Key _
derive instance repGenericS3Key :: Generic S3Key _
instance showS3Key :: Show S3Key where
  show = genericShow
instance decodeS3Key :: Decode S3Key where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Key :: Encode S3Key where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The location in S3 the contains the files to stream.</p>
newtype S3Location = S3Location 
  { "Bucket'" :: (S3Bucket)
  , "Key'" :: (S3Key)
  , "Version'" :: NullOrUndefined.NullOrUndefined (S3Version)
  }
derive instance newtypeS3Location :: Newtype S3Location _
derive instance repGenericS3Location :: Generic S3Location _
instance showS3Location :: Show S3Location where
  show = genericShow
instance decodeS3Location :: Decode S3Location where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Location :: Encode S3Location where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3Version = S3Version String
derive instance newtypeS3Version :: Newtype S3Version _
derive instance repGenericS3Version :: Generic S3Version _
instance showS3Version :: Show S3Version where
  show = genericShow
instance decodeS3Version :: Decode S3Version where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Version :: Encode S3Version where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SQL = SQL String
derive instance newtypeSQL :: Newtype SQL _
derive instance repGenericSQL :: Generic SQL _
instance showSQL :: Show SQL where
  show = genericShow
instance decodeSQL :: Decode SQL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSQL :: Encode SQL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action to write a message to a Salesforce IoT Cloud Input Stream.</p>
newtype SalesforceAction = SalesforceAction 
  { "Token'" :: (SalesforceToken)
  , "Url'" :: (SalesforceEndpoint)
  }
derive instance newtypeSalesforceAction :: Newtype SalesforceAction _
derive instance repGenericSalesforceAction :: Generic SalesforceAction _
instance showSalesforceAction :: Show SalesforceAction where
  show = genericShow
instance decodeSalesforceAction :: Decode SalesforceAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSalesforceAction :: Encode SalesforceAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SalesforceEndpoint = SalesforceEndpoint String
derive instance newtypeSalesforceEndpoint :: Newtype SalesforceEndpoint _
derive instance repGenericSalesforceEndpoint :: Generic SalesforceEndpoint _
instance showSalesforceEndpoint :: Show SalesforceEndpoint where
  show = genericShow
instance decodeSalesforceEndpoint :: Decode SalesforceEndpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSalesforceEndpoint :: Encode SalesforceEndpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SalesforceToken = SalesforceToken String
derive instance newtypeSalesforceToken :: Newtype SalesforceToken _
derive instance repGenericSalesforceToken :: Generic SalesforceToken _
instance showSalesforceToken :: Show SalesforceToken where
  show = genericShow
instance decodeSalesforceToken :: Decode SalesforceToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSalesforceToken :: Encode SalesforceToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchIndexRequest = SearchIndexRequest 
  { "IndexName'" :: NullOrUndefined.NullOrUndefined (IndexName)
  , "QueryString'" :: (QueryString)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (QueryMaxResults)
  , "QueryVersion'" :: NullOrUndefined.NullOrUndefined (QueryVersion)
  }
derive instance newtypeSearchIndexRequest :: Newtype SearchIndexRequest _
derive instance repGenericSearchIndexRequest :: Generic SearchIndexRequest _
instance showSearchIndexRequest :: Show SearchIndexRequest where
  show = genericShow
instance decodeSearchIndexRequest :: Decode SearchIndexRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchIndexRequest :: Encode SearchIndexRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchIndexResponse = SearchIndexResponse 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Things'" :: NullOrUndefined.NullOrUndefined (ThingDocumentList)
  }
derive instance newtypeSearchIndexResponse :: Newtype SearchIndexResponse _
derive instance repGenericSearchIndexResponse :: Generic SearchIndexResponse _
instance showSearchIndexResponse :: Show SearchIndexResponse where
  show = genericShow
instance decodeSearchIndexResponse :: Decode SearchIndexResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchIndexResponse :: Encode SearchIndexResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchableAttributes = SearchableAttributes (Array AttributeName)
derive instance newtypeSearchableAttributes :: Newtype SearchableAttributes _
derive instance repGenericSearchableAttributes :: Generic SearchableAttributes _
instance showSearchableAttributes :: Show SearchableAttributes where
  show = genericShow
instance decodeSearchableAttributes :: Decode SearchableAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchableAttributes :: Encode SearchableAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Seconds = Seconds Int
derive instance newtypeSeconds :: Newtype Seconds _
derive instance repGenericSeconds :: Generic Seconds _
instance showSeconds :: Show Seconds where
  show = genericShow
instance decodeSeconds :: Decode Seconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSeconds :: Encode Seconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The service is temporarily unavailable.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _
derive instance repGenericServiceUnavailableException :: Generic ServiceUnavailableException _
instance showServiceUnavailableException :: Show ServiceUnavailableException where
  show = genericShow
instance decodeServiceUnavailableException :: Decode ServiceUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUnavailableException :: Encode ServiceUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetAsActive = SetAsActive Boolean
derive instance newtypeSetAsActive :: Newtype SetAsActive _
derive instance repGenericSetAsActive :: Generic SetAsActive _
instance showSetAsActive :: Show SetAsActive where
  show = genericShow
instance decodeSetAsActive :: Decode SetAsActive where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetAsActive :: Encode SetAsActive where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetAsActiveFlag = SetAsActiveFlag Boolean
derive instance newtypeSetAsActiveFlag :: Newtype SetAsActiveFlag _
derive instance repGenericSetAsActiveFlag :: Generic SetAsActiveFlag _
instance showSetAsActiveFlag :: Show SetAsActiveFlag where
  show = genericShow
instance decodeSetAsActiveFlag :: Decode SetAsActiveFlag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetAsActiveFlag :: Encode SetAsActiveFlag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetAsDefault = SetAsDefault Boolean
derive instance newtypeSetAsDefault :: Newtype SetAsDefault _
derive instance repGenericSetAsDefault :: Generic SetAsDefault _
instance showSetAsDefault :: Show SetAsDefault where
  show = genericShow
instance decodeSetAsDefault :: Decode SetAsDefault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetAsDefault :: Encode SetAsDefault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetDefaultAuthorizerRequest = SetDefaultAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  }
derive instance newtypeSetDefaultAuthorizerRequest :: Newtype SetDefaultAuthorizerRequest _
derive instance repGenericSetDefaultAuthorizerRequest :: Generic SetDefaultAuthorizerRequest _
instance showSetDefaultAuthorizerRequest :: Show SetDefaultAuthorizerRequest where
  show = genericShow
instance decodeSetDefaultAuthorizerRequest :: Decode SetDefaultAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetDefaultAuthorizerRequest :: Encode SetDefaultAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetDefaultAuthorizerResponse = SetDefaultAuthorizerResponse 
  { "AuthorizerName'" :: NullOrUndefined.NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined.NullOrUndefined (AuthorizerArn)
  }
derive instance newtypeSetDefaultAuthorizerResponse :: Newtype SetDefaultAuthorizerResponse _
derive instance repGenericSetDefaultAuthorizerResponse :: Generic SetDefaultAuthorizerResponse _
instance showSetDefaultAuthorizerResponse :: Show SetDefaultAuthorizerResponse where
  show = genericShow
instance decodeSetDefaultAuthorizerResponse :: Decode SetDefaultAuthorizerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetDefaultAuthorizerResponse :: Encode SetDefaultAuthorizerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the SetDefaultPolicyVersion operation.</p>
newtype SetDefaultPolicyVersionRequest = SetDefaultPolicyVersionRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyVersionId'" :: (PolicyVersionId)
  }
derive instance newtypeSetDefaultPolicyVersionRequest :: Newtype SetDefaultPolicyVersionRequest _
derive instance repGenericSetDefaultPolicyVersionRequest :: Generic SetDefaultPolicyVersionRequest _
instance showSetDefaultPolicyVersionRequest :: Show SetDefaultPolicyVersionRequest where
  show = genericShow
instance decodeSetDefaultPolicyVersionRequest :: Decode SetDefaultPolicyVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetDefaultPolicyVersionRequest :: Encode SetDefaultPolicyVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the SetLoggingOptions operation.</p>
newtype SetLoggingOptionsRequest = SetLoggingOptionsRequest 
  { "LoggingOptionsPayload'" :: (LoggingOptionsPayload)
  }
derive instance newtypeSetLoggingOptionsRequest :: Newtype SetLoggingOptionsRequest _
derive instance repGenericSetLoggingOptionsRequest :: Generic SetLoggingOptionsRequest _
instance showSetLoggingOptionsRequest :: Show SetLoggingOptionsRequest where
  show = genericShow
instance decodeSetLoggingOptionsRequest :: Decode SetLoggingOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetLoggingOptionsRequest :: Encode SetLoggingOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetV2LoggingLevelRequest = SetV2LoggingLevelRequest 
  { "LogTarget'" :: (LogTarget)
  , "LogLevel'" :: (LogLevel)
  }
derive instance newtypeSetV2LoggingLevelRequest :: Newtype SetV2LoggingLevelRequest _
derive instance repGenericSetV2LoggingLevelRequest :: Generic SetV2LoggingLevelRequest _
instance showSetV2LoggingLevelRequest :: Show SetV2LoggingLevelRequest where
  show = genericShow
instance decodeSetV2LoggingLevelRequest :: Decode SetV2LoggingLevelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetV2LoggingLevelRequest :: Encode SetV2LoggingLevelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetV2LoggingOptionsRequest = SetV2LoggingOptionsRequest 
  { "RoleArn'" :: NullOrUndefined.NullOrUndefined (AwsArn)
  , "DefaultLogLevel'" :: NullOrUndefined.NullOrUndefined (LogLevel)
  , "DisableAllLogs'" :: NullOrUndefined.NullOrUndefined (DisableAllLogs)
  }
derive instance newtypeSetV2LoggingOptionsRequest :: Newtype SetV2LoggingOptionsRequest _
derive instance repGenericSetV2LoggingOptionsRequest :: Generic SetV2LoggingOptionsRequest _
instance showSetV2LoggingOptionsRequest :: Show SetV2LoggingOptionsRequest where
  show = genericShow
instance decodeSetV2LoggingOptionsRequest :: Decode SetV2LoggingOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetV2LoggingOptionsRequest :: Encode SetV2LoggingOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Signature = Signature String
derive instance newtypeSignature :: Newtype Signature _
derive instance repGenericSignature :: Generic Signature _
instance showSignature :: Show Signature where
  show = genericShow
instance decodeSignature :: Decode Signature where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignature :: Encode Signature where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SignatureAlgorithm = SignatureAlgorithm String
derive instance newtypeSignatureAlgorithm :: Newtype SignatureAlgorithm _
derive instance repGenericSignatureAlgorithm :: Generic SignatureAlgorithm _
instance showSignatureAlgorithm :: Show SignatureAlgorithm where
  show = genericShow
instance decodeSignatureAlgorithm :: Decode SignatureAlgorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignatureAlgorithm :: Encode SignatureAlgorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SigningJobId = SigningJobId String
derive instance newtypeSigningJobId :: Newtype SigningJobId _
derive instance repGenericSigningJobId :: Generic SigningJobId _
instance showSigningJobId :: Show SigningJobId where
  show = genericShow
instance decodeSigningJobId :: Decode SigningJobId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSigningJobId :: Encode SigningJobId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SkyfallMaxResults = SkyfallMaxResults Int
derive instance newtypeSkyfallMaxResults :: Newtype SkyfallMaxResults _
derive instance repGenericSkyfallMaxResults :: Generic SkyfallMaxResults _
instance showSkyfallMaxResults :: Show SkyfallMaxResults where
  show = genericShow
instance decodeSkyfallMaxResults :: Decode SkyfallMaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSkyfallMaxResults :: Encode SkyfallMaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action to publish to an Amazon SNS topic.</p>
newtype SnsAction = SnsAction 
  { "TargetArn'" :: (AwsArn)
  , "RoleArn'" :: (AwsArn)
  , "MessageFormat'" :: NullOrUndefined.NullOrUndefined (MessageFormat)
  }
derive instance newtypeSnsAction :: Newtype SnsAction _
derive instance repGenericSnsAction :: Generic SnsAction _
instance showSnsAction :: Show SnsAction where
  show = genericShow
instance decodeSnsAction :: Decode SnsAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnsAction :: Encode SnsAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Rule-SQL expression can't be parsed correctly.</p>
newtype SqlParseException = SqlParseException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeSqlParseException :: Newtype SqlParseException _
derive instance repGenericSqlParseException :: Generic SqlParseException _
instance showSqlParseException :: Show SqlParseException where
  show = genericShow
instance decodeSqlParseException :: Decode SqlParseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSqlParseException :: Encode SqlParseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an action to publish data to an Amazon SQS queue.</p>
newtype SqsAction = SqsAction 
  { "RoleArn'" :: (AwsArn)
  , "QueueUrl'" :: (QueueUrl)
  , "UseBase64'" :: NullOrUndefined.NullOrUndefined (UseBase64)
  }
derive instance newtypeSqsAction :: Newtype SqsAction _
derive instance repGenericSqsAction :: Generic SqsAction _
instance showSqsAction :: Show SqsAction where
  show = genericShow
instance decodeSqsAction :: Decode SqsAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSqsAction :: Encode SqsAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartThingRegistrationTaskRequest = StartThingRegistrationTaskRequest 
  { "TemplateBody'" :: (TemplateBody)
  , "InputFileBucket'" :: (RegistryS3BucketName)
  , "InputFileKey'" :: (RegistryS3KeyName)
  , "RoleArn'" :: (RoleArn)
  }
derive instance newtypeStartThingRegistrationTaskRequest :: Newtype StartThingRegistrationTaskRequest _
derive instance repGenericStartThingRegistrationTaskRequest :: Generic StartThingRegistrationTaskRequest _
instance showStartThingRegistrationTaskRequest :: Show StartThingRegistrationTaskRequest where
  show = genericShow
instance decodeStartThingRegistrationTaskRequest :: Decode StartThingRegistrationTaskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartThingRegistrationTaskRequest :: Encode StartThingRegistrationTaskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartThingRegistrationTaskResponse = StartThingRegistrationTaskResponse 
  { "TaskId'" :: NullOrUndefined.NullOrUndefined (TaskId)
  }
derive instance newtypeStartThingRegistrationTaskResponse :: Newtype StartThingRegistrationTaskResponse _
derive instance repGenericStartThingRegistrationTaskResponse :: Generic StartThingRegistrationTaskResponse _
instance showStartThingRegistrationTaskResponse :: Show StartThingRegistrationTaskResponse where
  show = genericShow
instance decodeStartThingRegistrationTaskResponse :: Decode StartThingRegistrationTaskResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartThingRegistrationTaskResponse :: Encode StartThingRegistrationTaskResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StateReason = StateReason String
derive instance newtypeStateReason :: Newtype StateReason _
derive instance repGenericStateReason :: Generic StateReason _
instance showStateReason :: Show StateReason where
  show = genericShow
instance decodeStateReason :: Decode StateReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateReason :: Encode StateReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StateValue = StateValue String
derive instance newtypeStateValue :: Newtype StateValue _
derive instance repGenericStateValue :: Generic StateValue _
instance showStateValue :: Show StateValue where
  show = genericShow
instance decodeStateValue :: Decode StateValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateValue :: Encode StateValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Status = Status String
derive instance newtypeStatus :: Newtype Status _
derive instance repGenericStatus :: Generic Status _
instance showStatus :: Show Status where
  show = genericShow
instance decodeStatus :: Decode Status where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatus :: Encode Status where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopThingRegistrationTaskRequest = StopThingRegistrationTaskRequest 
  { "TaskId'" :: (TaskId)
  }
derive instance newtypeStopThingRegistrationTaskRequest :: Newtype StopThingRegistrationTaskRequest _
derive instance repGenericStopThingRegistrationTaskRequest :: Generic StopThingRegistrationTaskRequest _
instance showStopThingRegistrationTaskRequest :: Show StopThingRegistrationTaskRequest where
  show = genericShow
instance decodeStopThingRegistrationTaskRequest :: Decode StopThingRegistrationTaskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopThingRegistrationTaskRequest :: Encode StopThingRegistrationTaskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopThingRegistrationTaskResponse = StopThingRegistrationTaskResponse Types.NoArguments
derive instance newtypeStopThingRegistrationTaskResponse :: Newtype StopThingRegistrationTaskResponse _
derive instance repGenericStopThingRegistrationTaskResponse :: Generic StopThingRegistrationTaskResponse _
instance showStopThingRegistrationTaskResponse :: Show StopThingRegistrationTaskResponse where
  show = genericShow
instance decodeStopThingRegistrationTaskResponse :: Decode StopThingRegistrationTaskResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopThingRegistrationTaskResponse :: Encode StopThingRegistrationTaskResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a group of files that can be streamed.</p>
newtype Stream = Stream 
  { "StreamId'" :: NullOrUndefined.NullOrUndefined (StreamId)
  , "FileId'" :: NullOrUndefined.NullOrUndefined (FileId)
  }
derive instance newtypeStream :: Newtype Stream _
derive instance repGenericStream :: Generic Stream _
instance showStream :: Show Stream where
  show = genericShow
instance decodeStream :: Decode Stream where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStream :: Encode Stream where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamArn = StreamArn String
derive instance newtypeStreamArn :: Newtype StreamArn _
derive instance repGenericStreamArn :: Generic StreamArn _
instance showStreamArn :: Show StreamArn where
  show = genericShow
instance decodeStreamArn :: Decode StreamArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamArn :: Encode StreamArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamDescription = StreamDescription String
derive instance newtypeStreamDescription :: Newtype StreamDescription _
derive instance repGenericStreamDescription :: Generic StreamDescription _
instance showStreamDescription :: Show StreamDescription where
  show = genericShow
instance decodeStreamDescription :: Decode StreamDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamDescription :: Encode StreamDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a file to stream.</p>
newtype StreamFile = StreamFile 
  { "FileId'" :: NullOrUndefined.NullOrUndefined (FileId)
  , "S3Location'" :: NullOrUndefined.NullOrUndefined (S3Location)
  }
derive instance newtypeStreamFile :: Newtype StreamFile _
derive instance repGenericStreamFile :: Generic StreamFile _
instance showStreamFile :: Show StreamFile where
  show = genericShow
instance decodeStreamFile :: Decode StreamFile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamFile :: Encode StreamFile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamFiles = StreamFiles (Array StreamFile)
derive instance newtypeStreamFiles :: Newtype StreamFiles _
derive instance repGenericStreamFiles :: Generic StreamFiles _
instance showStreamFiles :: Show StreamFiles where
  show = genericShow
instance decodeStreamFiles :: Decode StreamFiles where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamFiles :: Encode StreamFiles where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamId = StreamId String
derive instance newtypeStreamId :: Newtype StreamId _
derive instance repGenericStreamId :: Generic StreamId _
instance showStreamId :: Show StreamId where
  show = genericShow
instance decodeStreamId :: Decode StreamId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamId :: Encode StreamId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a stream.</p>
newtype StreamInfo = StreamInfo 
  { "StreamId'" :: NullOrUndefined.NullOrUndefined (StreamId)
  , "StreamArn'" :: NullOrUndefined.NullOrUndefined (StreamArn)
  , "StreamVersion'" :: NullOrUndefined.NullOrUndefined (StreamVersion)
  , "Description'" :: NullOrUndefined.NullOrUndefined (StreamDescription)
  , "Files'" :: NullOrUndefined.NullOrUndefined (StreamFiles)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "LastUpdatedAt'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (RoleArn)
  }
derive instance newtypeStreamInfo :: Newtype StreamInfo _
derive instance repGenericStreamInfo :: Generic StreamInfo _
instance showStreamInfo :: Show StreamInfo where
  show = genericShow
instance decodeStreamInfo :: Decode StreamInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamInfo :: Encode StreamInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamName = StreamName String
derive instance newtypeStreamName :: Newtype StreamName _
derive instance repGenericStreamName :: Generic StreamName _
instance showStreamName :: Show StreamName where
  show = genericShow
instance decodeStreamName :: Decode StreamName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamName :: Encode StreamName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A summary of a stream.</p>
newtype StreamSummary = StreamSummary 
  { "StreamId'" :: NullOrUndefined.NullOrUndefined (StreamId)
  , "StreamArn'" :: NullOrUndefined.NullOrUndefined (StreamArn)
  , "StreamVersion'" :: NullOrUndefined.NullOrUndefined (StreamVersion)
  , "Description'" :: NullOrUndefined.NullOrUndefined (StreamDescription)
  }
derive instance newtypeStreamSummary :: Newtype StreamSummary _
derive instance repGenericStreamSummary :: Generic StreamSummary _
instance showStreamSummary :: Show StreamSummary where
  show = genericShow
instance decodeStreamSummary :: Decode StreamSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamSummary :: Encode StreamSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamVersion = StreamVersion Int
derive instance newtypeStreamVersion :: Newtype StreamVersion _
derive instance repGenericStreamVersion :: Generic StreamVersion _
instance showStreamVersion :: Show StreamVersion where
  show = genericShow
instance decodeStreamVersion :: Decode StreamVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamVersion :: Encode StreamVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamsSummary = StreamsSummary (Array StreamSummary)
derive instance newtypeStreamsSummary :: Newtype StreamsSummary _
derive instance repGenericStreamsSummary :: Generic StreamsSummary _
instance showStreamsSummary :: Show StreamsSummary where
  show = genericShow
instance decodeStreamsSummary :: Decode StreamsSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamsSummary :: Encode StreamsSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SucceededThings = SucceededThings Int
derive instance newtypeSucceededThings :: Newtype SucceededThings _
derive instance repGenericSucceededThings :: Generic SucceededThings _
instance showSucceededThings :: Show SucceededThings where
  show = genericShow
instance decodeSucceededThings :: Decode SucceededThings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSucceededThings :: Encode SucceededThings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TableName = TableName String
derive instance newtypeTableName :: Newtype TableName _
derive instance repGenericTableName :: Generic TableName _
instance showTableName :: Show TableName where
  show = genericShow
instance decodeTableName :: Decode TableName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableName :: Encode TableName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Target = Target String
derive instance newtypeTarget :: Newtype Target _
derive instance repGenericTarget :: Generic Target _
instance showTarget :: Show Target where
  show = genericShow
instance decodeTarget :: Decode Target where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTarget :: Encode Target where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetArn = TargetArn String
derive instance newtypeTargetArn :: Newtype TargetArn _
derive instance repGenericTargetArn :: Generic TargetArn _
instance showTargetArn :: Show TargetArn where
  show = genericShow
instance decodeTargetArn :: Decode TargetArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetArn :: Encode TargetArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetSelection = TargetSelection String
derive instance newtypeTargetSelection :: Newtype TargetSelection _
derive instance repGenericTargetSelection :: Generic TargetSelection _
instance showTargetSelection :: Show TargetSelection where
  show = genericShow
instance decodeTargetSelection :: Decode TargetSelection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetSelection :: Encode TargetSelection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Targets = Targets (Array Target)
derive instance newtypeTargets :: Newtype Targets _
derive instance repGenericTargets :: Generic Targets _
instance showTargets :: Show Targets where
  show = genericShow
instance decodeTargets :: Decode Targets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargets :: Encode Targets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaskId = TaskId String
derive instance newtypeTaskId :: Newtype TaskId _
derive instance repGenericTaskId :: Generic TaskId _
instance showTaskId :: Show TaskId where
  show = genericShow
instance decodeTaskId :: Decode TaskId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskId :: Encode TaskId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaskIdList = TaskIdList (Array TaskId)
derive instance newtypeTaskIdList :: Newtype TaskIdList _
derive instance repGenericTaskIdList :: Generic TaskIdList _
instance showTaskIdList :: Show TaskIdList where
  show = genericShow
instance decodeTaskIdList :: Decode TaskIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskIdList :: Encode TaskIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TemplateBody = TemplateBody String
derive instance newtypeTemplateBody :: Newtype TemplateBody _
derive instance repGenericTemplateBody :: Generic TemplateBody _
instance showTemplateBody :: Show TemplateBody where
  show = genericShow
instance decodeTemplateBody :: Decode TemplateBody where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateBody :: Encode TemplateBody where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestAuthorizationRequest = TestAuthorizationRequest 
  { "Principal'" :: NullOrUndefined.NullOrUndefined (Principal)
  , "CognitoIdentityPoolId'" :: NullOrUndefined.NullOrUndefined (CognitoIdentityPoolId)
  , "AuthInfos'" :: (AuthInfos)
  , "ClientId'" :: NullOrUndefined.NullOrUndefined (ClientId)
  , "PolicyNamesToAdd'" :: NullOrUndefined.NullOrUndefined (PolicyNames)
  , "PolicyNamesToSkip'" :: NullOrUndefined.NullOrUndefined (PolicyNames)
  }
derive instance newtypeTestAuthorizationRequest :: Newtype TestAuthorizationRequest _
derive instance repGenericTestAuthorizationRequest :: Generic TestAuthorizationRequest _
instance showTestAuthorizationRequest :: Show TestAuthorizationRequest where
  show = genericShow
instance decodeTestAuthorizationRequest :: Decode TestAuthorizationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestAuthorizationRequest :: Encode TestAuthorizationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestAuthorizationResponse = TestAuthorizationResponse 
  { "AuthResults'" :: NullOrUndefined.NullOrUndefined (AuthResults)
  }
derive instance newtypeTestAuthorizationResponse :: Newtype TestAuthorizationResponse _
derive instance repGenericTestAuthorizationResponse :: Generic TestAuthorizationResponse _
instance showTestAuthorizationResponse :: Show TestAuthorizationResponse where
  show = genericShow
instance decodeTestAuthorizationResponse :: Decode TestAuthorizationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestAuthorizationResponse :: Encode TestAuthorizationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestInvokeAuthorizerRequest = TestInvokeAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  , "Token'" :: (Token)
  , "TokenSignature'" :: (TokenSignature)
  }
derive instance newtypeTestInvokeAuthorizerRequest :: Newtype TestInvokeAuthorizerRequest _
derive instance repGenericTestInvokeAuthorizerRequest :: Generic TestInvokeAuthorizerRequest _
instance showTestInvokeAuthorizerRequest :: Show TestInvokeAuthorizerRequest where
  show = genericShow
instance decodeTestInvokeAuthorizerRequest :: Decode TestInvokeAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestInvokeAuthorizerRequest :: Encode TestInvokeAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse 
  { "IsAuthenticated'" :: NullOrUndefined.NullOrUndefined (IsAuthenticated)
  , "PrincipalId'" :: NullOrUndefined.NullOrUndefined (PrincipalId)
  , "PolicyDocuments'" :: NullOrUndefined.NullOrUndefined (PolicyDocuments)
  , "RefreshAfterInSeconds'" :: NullOrUndefined.NullOrUndefined (Seconds)
  , "DisconnectAfterInSeconds'" :: NullOrUndefined.NullOrUndefined (Seconds)
  }
derive instance newtypeTestInvokeAuthorizerResponse :: Newtype TestInvokeAuthorizerResponse _
derive instance repGenericTestInvokeAuthorizerResponse :: Generic TestInvokeAuthorizerResponse _
instance showTestInvokeAuthorizerResponse :: Show TestInvokeAuthorizerResponse where
  show = genericShow
instance decodeTestInvokeAuthorizerResponse :: Decode TestInvokeAuthorizerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestInvokeAuthorizerResponse :: Encode TestInvokeAuthorizerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingArn = ThingArn String
derive instance newtypeThingArn :: Newtype ThingArn _
derive instance repGenericThingArn :: Generic ThingArn _
instance showThingArn :: Show ThingArn where
  show = genericShow
instance decodeThingArn :: Decode ThingArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingArn :: Encode ThingArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The properties of the thing, including thing name, thing type name, and a list of thing attributes.</p>
newtype ThingAttribute = ThingAttribute 
  { "ThingName'" :: NullOrUndefined.NullOrUndefined (ThingName)
  , "ThingTypeName'" :: NullOrUndefined.NullOrUndefined (ThingTypeName)
  , "ThingArn'" :: NullOrUndefined.NullOrUndefined (ThingArn)
  , "Attributes'" :: NullOrUndefined.NullOrUndefined (Attributes)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  }
derive instance newtypeThingAttribute :: Newtype ThingAttribute _
derive instance repGenericThingAttribute :: Generic ThingAttribute _
instance showThingAttribute :: Show ThingAttribute where
  show = genericShow
instance decodeThingAttribute :: Decode ThingAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingAttribute :: Encode ThingAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingAttributeList = ThingAttributeList (Array ThingAttribute)
derive instance newtypeThingAttributeList :: Newtype ThingAttributeList _
derive instance repGenericThingAttributeList :: Generic ThingAttributeList _
instance showThingAttributeList :: Show ThingAttributeList where
  show = genericShow
instance decodeThingAttributeList :: Decode ThingAttributeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingAttributeList :: Encode ThingAttributeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The thing search index document.</p>
newtype ThingDocument = ThingDocument 
  { "ThingName'" :: NullOrUndefined.NullOrUndefined (ThingName)
  , "ThingId'" :: NullOrUndefined.NullOrUndefined (ThingId)
  , "ThingTypeName'" :: NullOrUndefined.NullOrUndefined (ThingTypeName)
  , "ThingGroupNames'" :: NullOrUndefined.NullOrUndefined (ThingGroupNameList)
  , "Attributes'" :: NullOrUndefined.NullOrUndefined (Attributes)
  , "Shadow'" :: NullOrUndefined.NullOrUndefined (JsonDocument)
  }
derive instance newtypeThingDocument :: Newtype ThingDocument _
derive instance repGenericThingDocument :: Generic ThingDocument _
instance showThingDocument :: Show ThingDocument where
  show = genericShow
instance decodeThingDocument :: Decode ThingDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingDocument :: Encode ThingDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingDocumentList = ThingDocumentList (Array ThingDocument)
derive instance newtypeThingDocumentList :: Newtype ThingDocumentList _
derive instance repGenericThingDocumentList :: Generic ThingDocumentList _
instance showThingDocumentList :: Show ThingDocumentList where
  show = genericShow
instance decodeThingDocumentList :: Decode ThingDocumentList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingDocumentList :: Encode ThingDocumentList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingGroupArn = ThingGroupArn String
derive instance newtypeThingGroupArn :: Newtype ThingGroupArn _
derive instance repGenericThingGroupArn :: Generic ThingGroupArn _
instance showThingGroupArn :: Show ThingGroupArn where
  show = genericShow
instance decodeThingGroupArn :: Decode ThingGroupArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingGroupArn :: Encode ThingGroupArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingGroupDescription = ThingGroupDescription String
derive instance newtypeThingGroupDescription :: Newtype ThingGroupDescription _
derive instance repGenericThingGroupDescription :: Generic ThingGroupDescription _
instance showThingGroupDescription :: Show ThingGroupDescription where
  show = genericShow
instance decodeThingGroupDescription :: Decode ThingGroupDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingGroupDescription :: Encode ThingGroupDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingGroupId = ThingGroupId String
derive instance newtypeThingGroupId :: Newtype ThingGroupId _
derive instance repGenericThingGroupId :: Generic ThingGroupId _
instance showThingGroupId :: Show ThingGroupId where
  show = genericShow
instance decodeThingGroupId :: Decode ThingGroupId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingGroupId :: Encode ThingGroupId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingGroupList = ThingGroupList (Array ThingGroupName)
derive instance newtypeThingGroupList :: Newtype ThingGroupList _
derive instance repGenericThingGroupList :: Generic ThingGroupList _
instance showThingGroupList :: Show ThingGroupList where
  show = genericShow
instance decodeThingGroupList :: Decode ThingGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingGroupList :: Encode ThingGroupList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thing group metadata.</p>
newtype ThingGroupMetadata = ThingGroupMetadata 
  { "ParentGroupName'" :: NullOrUndefined.NullOrUndefined (ThingGroupName)
  , "RootToParentThingGroups'" :: NullOrUndefined.NullOrUndefined (ThingGroupNameAndArnList)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (CreationDate)
  }
derive instance newtypeThingGroupMetadata :: Newtype ThingGroupMetadata _
derive instance repGenericThingGroupMetadata :: Generic ThingGroupMetadata _
instance showThingGroupMetadata :: Show ThingGroupMetadata where
  show = genericShow
instance decodeThingGroupMetadata :: Decode ThingGroupMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingGroupMetadata :: Encode ThingGroupMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingGroupName = ThingGroupName String
derive instance newtypeThingGroupName :: Newtype ThingGroupName _
derive instance repGenericThingGroupName :: Generic ThingGroupName _
instance showThingGroupName :: Show ThingGroupName where
  show = genericShow
instance decodeThingGroupName :: Decode ThingGroupName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingGroupName :: Encode ThingGroupName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingGroupNameAndArnList = ThingGroupNameAndArnList (Array GroupNameAndArn)
derive instance newtypeThingGroupNameAndArnList :: Newtype ThingGroupNameAndArnList _
derive instance repGenericThingGroupNameAndArnList :: Generic ThingGroupNameAndArnList _
instance showThingGroupNameAndArnList :: Show ThingGroupNameAndArnList where
  show = genericShow
instance decodeThingGroupNameAndArnList :: Decode ThingGroupNameAndArnList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingGroupNameAndArnList :: Encode ThingGroupNameAndArnList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingGroupNameList = ThingGroupNameList (Array ThingGroupName)
derive instance newtypeThingGroupNameList :: Newtype ThingGroupNameList _
derive instance repGenericThingGroupNameList :: Generic ThingGroupNameList _
instance showThingGroupNameList :: Show ThingGroupNameList where
  show = genericShow
instance decodeThingGroupNameList :: Decode ThingGroupNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingGroupNameList :: Encode ThingGroupNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thing group properties.</p>
newtype ThingGroupProperties = ThingGroupProperties 
  { "ThingGroupDescription'" :: NullOrUndefined.NullOrUndefined (ThingGroupDescription)
  , "AttributePayload'" :: NullOrUndefined.NullOrUndefined (AttributePayload)
  }
derive instance newtypeThingGroupProperties :: Newtype ThingGroupProperties _
derive instance repGenericThingGroupProperties :: Generic ThingGroupProperties _
instance showThingGroupProperties :: Show ThingGroupProperties where
  show = genericShow
instance decodeThingGroupProperties :: Decode ThingGroupProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingGroupProperties :: Encode ThingGroupProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingId = ThingId String
derive instance newtypeThingId :: Newtype ThingId _
derive instance repGenericThingId :: Generic ThingId _
instance showThingId :: Show ThingId where
  show = genericShow
instance decodeThingId :: Decode ThingId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingId :: Encode ThingId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thing indexing configuration.</p>
newtype ThingIndexingConfiguration = ThingIndexingConfiguration 
  { "ThingIndexingMode'" :: NullOrUndefined.NullOrUndefined (ThingIndexingMode)
  }
derive instance newtypeThingIndexingConfiguration :: Newtype ThingIndexingConfiguration _
derive instance repGenericThingIndexingConfiguration :: Generic ThingIndexingConfiguration _
instance showThingIndexingConfiguration :: Show ThingIndexingConfiguration where
  show = genericShow
instance decodeThingIndexingConfiguration :: Decode ThingIndexingConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingIndexingConfiguration :: Encode ThingIndexingConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingIndexingMode = ThingIndexingMode String
derive instance newtypeThingIndexingMode :: Newtype ThingIndexingMode _
derive instance repGenericThingIndexingMode :: Generic ThingIndexingMode _
instance showThingIndexingMode :: Show ThingIndexingMode where
  show = genericShow
instance decodeThingIndexingMode :: Decode ThingIndexingMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingIndexingMode :: Encode ThingIndexingMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingName = ThingName String
derive instance newtypeThingName :: Newtype ThingName _
derive instance repGenericThingName :: Generic ThingName _
instance showThingName :: Show ThingName where
  show = genericShow
instance decodeThingName :: Decode ThingName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingName :: Encode ThingName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingNameList = ThingNameList (Array ThingName)
derive instance newtypeThingNameList :: Newtype ThingNameList _
derive instance repGenericThingNameList :: Generic ThingNameList _
instance showThingNameList :: Show ThingNameList where
  show = genericShow
instance decodeThingNameList :: Decode ThingNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingNameList :: Encode ThingNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingTypeArn = ThingTypeArn String
derive instance newtypeThingTypeArn :: Newtype ThingTypeArn _
derive instance repGenericThingTypeArn :: Generic ThingTypeArn _
instance showThingTypeArn :: Show ThingTypeArn where
  show = genericShow
instance decodeThingTypeArn :: Decode ThingTypeArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingTypeArn :: Encode ThingTypeArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The definition of the thing type, including thing type name and description.</p>
newtype ThingTypeDefinition = ThingTypeDefinition 
  { "ThingTypeName'" :: NullOrUndefined.NullOrUndefined (ThingTypeName)
  , "ThingTypeArn'" :: NullOrUndefined.NullOrUndefined (ThingTypeArn)
  , "ThingTypeProperties'" :: NullOrUndefined.NullOrUndefined (ThingTypeProperties)
  , "ThingTypeMetadata'" :: NullOrUndefined.NullOrUndefined (ThingTypeMetadata)
  }
derive instance newtypeThingTypeDefinition :: Newtype ThingTypeDefinition _
derive instance repGenericThingTypeDefinition :: Generic ThingTypeDefinition _
instance showThingTypeDefinition :: Show ThingTypeDefinition where
  show = genericShow
instance decodeThingTypeDefinition :: Decode ThingTypeDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingTypeDefinition :: Encode ThingTypeDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingTypeDescription = ThingTypeDescription String
derive instance newtypeThingTypeDescription :: Newtype ThingTypeDescription _
derive instance repGenericThingTypeDescription :: Generic ThingTypeDescription _
instance showThingTypeDescription :: Show ThingTypeDescription where
  show = genericShow
instance decodeThingTypeDescription :: Decode ThingTypeDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingTypeDescription :: Encode ThingTypeDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingTypeId = ThingTypeId String
derive instance newtypeThingTypeId :: Newtype ThingTypeId _
derive instance repGenericThingTypeId :: Generic ThingTypeId _
instance showThingTypeId :: Show ThingTypeId where
  show = genericShow
instance decodeThingTypeId :: Decode ThingTypeId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingTypeId :: Encode ThingTypeId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingTypeList = ThingTypeList (Array ThingTypeDefinition)
derive instance newtypeThingTypeList :: Newtype ThingTypeList _
derive instance repGenericThingTypeList :: Generic ThingTypeList _
instance showThingTypeList :: Show ThingTypeList where
  show = genericShow
instance decodeThingTypeList :: Decode ThingTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingTypeList :: Encode ThingTypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when time was deprecated.</p>
newtype ThingTypeMetadata = ThingTypeMetadata 
  { "Deprecated'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "DeprecationDate'" :: NullOrUndefined.NullOrUndefined (DeprecationDate)
  , "CreationDate'" :: NullOrUndefined.NullOrUndefined (CreationDate)
  }
derive instance newtypeThingTypeMetadata :: Newtype ThingTypeMetadata _
derive instance repGenericThingTypeMetadata :: Generic ThingTypeMetadata _
instance showThingTypeMetadata :: Show ThingTypeMetadata where
  show = genericShow
instance decodeThingTypeMetadata :: Decode ThingTypeMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingTypeMetadata :: Encode ThingTypeMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingTypeName = ThingTypeName String
derive instance newtypeThingTypeName :: Newtype ThingTypeName _
derive instance repGenericThingTypeName :: Generic ThingTypeName _
instance showThingTypeName :: Show ThingTypeName where
  show = genericShow
instance decodeThingTypeName :: Decode ThingTypeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingTypeName :: Encode ThingTypeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ThingTypeProperties contains information about the thing type including: a thing type description, and a list of searchable thing attribute names.</p>
newtype ThingTypeProperties = ThingTypeProperties 
  { "ThingTypeDescription'" :: NullOrUndefined.NullOrUndefined (ThingTypeDescription)
  , "SearchableAttributes'" :: NullOrUndefined.NullOrUndefined (SearchableAttributes)
  }
derive instance newtypeThingTypeProperties :: Newtype ThingTypeProperties _
derive instance repGenericThingTypeProperties :: Generic ThingTypeProperties _
instance showThingTypeProperties :: Show ThingTypeProperties where
  show = genericShow
instance decodeThingTypeProperties :: Decode ThingTypeProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingTypeProperties :: Encode ThingTypeProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The rate exceeds the limit.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _
derive instance repGenericThrottlingException :: Generic ThrottlingException _
instance showThrottlingException :: Show ThrottlingException where
  show = genericShow
instance decodeThrottlingException :: Decode ThrottlingException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThrottlingException :: Encode ThrottlingException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Token = Token String
derive instance newtypeToken :: Newtype Token _
derive instance repGenericToken :: Generic Token _
instance showToken :: Show Token where
  show = genericShow
instance decodeToken :: Decode Token where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeToken :: Encode Token where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TokenKeyName = TokenKeyName String
derive instance newtypeTokenKeyName :: Newtype TokenKeyName _
derive instance repGenericTokenKeyName :: Generic TokenKeyName _
instance showTokenKeyName :: Show TokenKeyName where
  show = genericShow
instance decodeTokenKeyName :: Decode TokenKeyName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTokenKeyName :: Encode TokenKeyName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TokenSignature = TokenSignature String
derive instance newtypeTokenSignature :: Newtype TokenSignature _
derive instance repGenericTokenSignature :: Generic TokenSignature _
instance showTokenSignature :: Show TokenSignature where
  show = genericShow
instance decodeTokenSignature :: Decode TokenSignature where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTokenSignature :: Encode TokenSignature where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Topic = Topic String
derive instance newtypeTopic :: Newtype Topic _
derive instance repGenericTopic :: Generic Topic _
instance showTopic :: Show Topic where
  show = genericShow
instance decodeTopic :: Decode Topic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopic :: Encode Topic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicPattern = TopicPattern String
derive instance newtypeTopicPattern :: Newtype TopicPattern _
derive instance repGenericTopicPattern :: Generic TopicPattern _
instance showTopicPattern :: Show TopicPattern where
  show = genericShow
instance decodeTopicPattern :: Decode TopicPattern where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicPattern :: Encode TopicPattern where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a rule.</p>
newtype TopicRule = TopicRule 
  { "RuleName'" :: NullOrUndefined.NullOrUndefined (RuleName)
  , "Sql'" :: NullOrUndefined.NullOrUndefined (SQL)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (CreatedAtDate)
  , "Actions'" :: NullOrUndefined.NullOrUndefined (ActionList)
  , "RuleDisabled'" :: NullOrUndefined.NullOrUndefined (IsDisabled)
  , "AwsIotSqlVersion'" :: NullOrUndefined.NullOrUndefined (AwsIotSqlVersion)
  , "ErrorAction'" :: NullOrUndefined.NullOrUndefined (Action)
  }
derive instance newtypeTopicRule :: Newtype TopicRule _
derive instance repGenericTopicRule :: Generic TopicRule _
instance showTopicRule :: Show TopicRule where
  show = genericShow
instance decodeTopicRule :: Decode TopicRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicRule :: Encode TopicRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicRuleList = TopicRuleList (Array TopicRuleListItem)
derive instance newtypeTopicRuleList :: Newtype TopicRuleList _
derive instance repGenericTopicRuleList :: Generic TopicRuleList _
instance showTopicRuleList :: Show TopicRuleList where
  show = genericShow
instance decodeTopicRuleList :: Decode TopicRuleList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicRuleList :: Encode TopicRuleList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a rule.</p>
newtype TopicRuleListItem = TopicRuleListItem 
  { "RuleArn'" :: NullOrUndefined.NullOrUndefined (RuleArn)
  , "RuleName'" :: NullOrUndefined.NullOrUndefined (RuleName)
  , "TopicPattern'" :: NullOrUndefined.NullOrUndefined (TopicPattern)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (CreatedAtDate)
  , "RuleDisabled'" :: NullOrUndefined.NullOrUndefined (IsDisabled)
  }
derive instance newtypeTopicRuleListItem :: Newtype TopicRuleListItem _
derive instance repGenericTopicRuleListItem :: Generic TopicRuleListItem _
instance showTopicRuleListItem :: Show TopicRuleListItem where
  show = genericShow
instance decodeTopicRuleListItem :: Decode TopicRuleListItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicRuleListItem :: Encode TopicRuleListItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a rule.</p>
newtype TopicRulePayload = TopicRulePayload 
  { "Sql'" :: (SQL)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "Actions'" :: (ActionList)
  , "RuleDisabled'" :: NullOrUndefined.NullOrUndefined (IsDisabled)
  , "AwsIotSqlVersion'" :: NullOrUndefined.NullOrUndefined (AwsIotSqlVersion)
  , "ErrorAction'" :: NullOrUndefined.NullOrUndefined (Action)
  }
derive instance newtypeTopicRulePayload :: Newtype TopicRulePayload _
derive instance repGenericTopicRulePayload :: Generic TopicRulePayload _
instance showTopicRulePayload :: Show TopicRulePayload where
  show = genericShow
instance decodeTopicRulePayload :: Decode TopicRulePayload where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicRulePayload :: Encode TopicRulePayload where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You can't revert the certificate transfer because the transfer is already complete.</p>
newtype TransferAlreadyCompletedException = TransferAlreadyCompletedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeTransferAlreadyCompletedException :: Newtype TransferAlreadyCompletedException _
derive instance repGenericTransferAlreadyCompletedException :: Generic TransferAlreadyCompletedException _
instance showTransferAlreadyCompletedException :: Show TransferAlreadyCompletedException where
  show = genericShow
instance decodeTransferAlreadyCompletedException :: Decode TransferAlreadyCompletedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransferAlreadyCompletedException :: Encode TransferAlreadyCompletedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the TransferCertificate operation.</p>
newtype TransferCertificateRequest = TransferCertificateRequest 
  { "CertificateId'" :: (CertificateId)
  , "TargetAwsAccount'" :: (AwsAccountId)
  , "TransferMessage'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeTransferCertificateRequest :: Newtype TransferCertificateRequest _
derive instance repGenericTransferCertificateRequest :: Generic TransferCertificateRequest _
instance showTransferCertificateRequest :: Show TransferCertificateRequest where
  show = genericShow
instance decodeTransferCertificateRequest :: Decode TransferCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransferCertificateRequest :: Encode TransferCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the TransferCertificate operation.</p>
newtype TransferCertificateResponse = TransferCertificateResponse 
  { "TransferredCertificateArn'" :: NullOrUndefined.NullOrUndefined (CertificateArn)
  }
derive instance newtypeTransferCertificateResponse :: Newtype TransferCertificateResponse _
derive instance repGenericTransferCertificateResponse :: Generic TransferCertificateResponse _
instance showTransferCertificateResponse :: Show TransferCertificateResponse where
  show = genericShow
instance decodeTransferCertificateResponse :: Decode TransferCertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransferCertificateResponse :: Encode TransferCertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You can't transfer the certificate because authorization policies are still attached.</p>
newtype TransferConflictException = TransferConflictException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeTransferConflictException :: Newtype TransferConflictException _
derive instance repGenericTransferConflictException :: Generic TransferConflictException _
instance showTransferConflictException :: Show TransferConflictException where
  show = genericShow
instance decodeTransferConflictException :: Decode TransferConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransferConflictException :: Encode TransferConflictException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Data used to transfer a certificate to an AWS account.</p>
newtype TransferData = TransferData 
  { "TransferMessage'" :: NullOrUndefined.NullOrUndefined (Message)
  , "RejectReason'" :: NullOrUndefined.NullOrUndefined (Message)
  , "TransferDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "AcceptDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  , "RejectDate'" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeTransferData :: Newtype TransferData _
derive instance repGenericTransferData :: Generic TransferData _
instance showTransferData :: Show TransferData where
  show = genericShow
instance decodeTransferData :: Decode TransferData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransferData :: Encode TransferData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You are not authorized to perform this operation.</p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeUnauthorizedException :: Newtype UnauthorizedException _
derive instance repGenericUnauthorizedException :: Generic UnauthorizedException _
instance showUnauthorizedException :: Show UnauthorizedException where
  show = genericShow
instance decodeUnauthorizedException :: Decode UnauthorizedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnauthorizedException :: Encode UnauthorizedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UndoDeprecate = UndoDeprecate Boolean
derive instance newtypeUndoDeprecate :: Newtype UndoDeprecate _
derive instance repGenericUndoDeprecate :: Generic UndoDeprecate _
instance showUndoDeprecate :: Show UndoDeprecate where
  show = genericShow
instance decodeUndoDeprecate :: Decode UndoDeprecate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUndoDeprecate :: Encode UndoDeprecate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateAuthorizerRequest = UpdateAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  , "AuthorizerFunctionArn'" :: NullOrUndefined.NullOrUndefined (AuthorizerFunctionArn)
  , "TokenKeyName'" :: NullOrUndefined.NullOrUndefined (TokenKeyName)
  , "TokenSigningPublicKeys'" :: NullOrUndefined.NullOrUndefined (PublicKeyMap)
  , "Status'" :: NullOrUndefined.NullOrUndefined (AuthorizerStatus)
  }
derive instance newtypeUpdateAuthorizerRequest :: Newtype UpdateAuthorizerRequest _
derive instance repGenericUpdateAuthorizerRequest :: Generic UpdateAuthorizerRequest _
instance showUpdateAuthorizerRequest :: Show UpdateAuthorizerRequest where
  show = genericShow
instance decodeUpdateAuthorizerRequest :: Decode UpdateAuthorizerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAuthorizerRequest :: Encode UpdateAuthorizerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateAuthorizerResponse = UpdateAuthorizerResponse 
  { "AuthorizerName'" :: NullOrUndefined.NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined.NullOrUndefined (AuthorizerArn)
  }
derive instance newtypeUpdateAuthorizerResponse :: Newtype UpdateAuthorizerResponse _
derive instance repGenericUpdateAuthorizerResponse :: Generic UpdateAuthorizerResponse _
instance showUpdateAuthorizerResponse :: Show UpdateAuthorizerResponse where
  show = genericShow
instance decodeUpdateAuthorizerResponse :: Decode UpdateAuthorizerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAuthorizerResponse :: Encode UpdateAuthorizerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input to the UpdateCACertificate operation.</p>
newtype UpdateCACertificateRequest = UpdateCACertificateRequest 
  { "CertificateId'" :: (CertificateId)
  , "NewStatus'" :: NullOrUndefined.NullOrUndefined (CACertificateStatus)
  , "NewAutoRegistrationStatus'" :: NullOrUndefined.NullOrUndefined (AutoRegistrationStatus)
  , "RegistrationConfig'" :: NullOrUndefined.NullOrUndefined (RegistrationConfig)
  , "RemoveAutoRegistration'" :: NullOrUndefined.NullOrUndefined (RemoveAutoRegistration)
  }
derive instance newtypeUpdateCACertificateRequest :: Newtype UpdateCACertificateRequest _
derive instance repGenericUpdateCACertificateRequest :: Generic UpdateCACertificateRequest _
instance showUpdateCACertificateRequest :: Show UpdateCACertificateRequest where
  show = genericShow
instance decodeUpdateCACertificateRequest :: Decode UpdateCACertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCACertificateRequest :: Encode UpdateCACertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the UpdateCertificate operation.</p>
newtype UpdateCertificateRequest = UpdateCertificateRequest 
  { "CertificateId'" :: (CertificateId)
  , "NewStatus'" :: (CertificateStatus)
  }
derive instance newtypeUpdateCertificateRequest :: Newtype UpdateCertificateRequest _
derive instance repGenericUpdateCertificateRequest :: Generic UpdateCertificateRequest _
instance showUpdateCertificateRequest :: Show UpdateCertificateRequest where
  show = genericShow
instance decodeUpdateCertificateRequest :: Decode UpdateCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCertificateRequest :: Encode UpdateCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateEventConfigurationsRequest = UpdateEventConfigurationsRequest 
  { "EventConfigurations'" :: NullOrUndefined.NullOrUndefined (EventConfigurations)
  }
derive instance newtypeUpdateEventConfigurationsRequest :: Newtype UpdateEventConfigurationsRequest _
derive instance repGenericUpdateEventConfigurationsRequest :: Generic UpdateEventConfigurationsRequest _
instance showUpdateEventConfigurationsRequest :: Show UpdateEventConfigurationsRequest where
  show = genericShow
instance decodeUpdateEventConfigurationsRequest :: Decode UpdateEventConfigurationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateEventConfigurationsRequest :: Encode UpdateEventConfigurationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateEventConfigurationsResponse = UpdateEventConfigurationsResponse Types.NoArguments
derive instance newtypeUpdateEventConfigurationsResponse :: Newtype UpdateEventConfigurationsResponse _
derive instance repGenericUpdateEventConfigurationsResponse :: Generic UpdateEventConfigurationsResponse _
instance showUpdateEventConfigurationsResponse :: Show UpdateEventConfigurationsResponse where
  show = genericShow
instance decodeUpdateEventConfigurationsResponse :: Decode UpdateEventConfigurationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateEventConfigurationsResponse :: Encode UpdateEventConfigurationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateIndexingConfigurationRequest = UpdateIndexingConfigurationRequest 
  { "ThingIndexingConfiguration'" :: NullOrUndefined.NullOrUndefined (ThingIndexingConfiguration)
  }
derive instance newtypeUpdateIndexingConfigurationRequest :: Newtype UpdateIndexingConfigurationRequest _
derive instance repGenericUpdateIndexingConfigurationRequest :: Generic UpdateIndexingConfigurationRequest _
instance showUpdateIndexingConfigurationRequest :: Show UpdateIndexingConfigurationRequest where
  show = genericShow
instance decodeUpdateIndexingConfigurationRequest :: Decode UpdateIndexingConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateIndexingConfigurationRequest :: Encode UpdateIndexingConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateIndexingConfigurationResponse = UpdateIndexingConfigurationResponse Types.NoArguments
derive instance newtypeUpdateIndexingConfigurationResponse :: Newtype UpdateIndexingConfigurationResponse _
derive instance repGenericUpdateIndexingConfigurationResponse :: Generic UpdateIndexingConfigurationResponse _
instance showUpdateIndexingConfigurationResponse :: Show UpdateIndexingConfigurationResponse where
  show = genericShow
instance decodeUpdateIndexingConfigurationResponse :: Decode UpdateIndexingConfigurationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateIndexingConfigurationResponse :: Encode UpdateIndexingConfigurationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateRoleAliasRequest = UpdateRoleAliasRequest 
  { "RoleAlias'" :: (RoleAlias)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (RoleArn)
  , "CredentialDurationSeconds'" :: NullOrUndefined.NullOrUndefined (CredentialDurationSeconds)
  }
derive instance newtypeUpdateRoleAliasRequest :: Newtype UpdateRoleAliasRequest _
derive instance repGenericUpdateRoleAliasRequest :: Generic UpdateRoleAliasRequest _
instance showUpdateRoleAliasRequest :: Show UpdateRoleAliasRequest where
  show = genericShow
instance decodeUpdateRoleAliasRequest :: Decode UpdateRoleAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRoleAliasRequest :: Encode UpdateRoleAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateRoleAliasResponse = UpdateRoleAliasResponse 
  { "RoleAlias'" :: NullOrUndefined.NullOrUndefined (RoleAlias)
  , "RoleAliasArn'" :: NullOrUndefined.NullOrUndefined (RoleAliasArn)
  }
derive instance newtypeUpdateRoleAliasResponse :: Newtype UpdateRoleAliasResponse _
derive instance repGenericUpdateRoleAliasResponse :: Generic UpdateRoleAliasResponse _
instance showUpdateRoleAliasResponse :: Show UpdateRoleAliasResponse where
  show = genericShow
instance decodeUpdateRoleAliasResponse :: Decode UpdateRoleAliasResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRoleAliasResponse :: Encode UpdateRoleAliasResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateStreamRequest = UpdateStreamRequest 
  { "StreamId'" :: (StreamId)
  , "Description'" :: NullOrUndefined.NullOrUndefined (StreamDescription)
  , "Files'" :: NullOrUndefined.NullOrUndefined (StreamFiles)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (RoleArn)
  }
derive instance newtypeUpdateStreamRequest :: Newtype UpdateStreamRequest _
derive instance repGenericUpdateStreamRequest :: Generic UpdateStreamRequest _
instance showUpdateStreamRequest :: Show UpdateStreamRequest where
  show = genericShow
instance decodeUpdateStreamRequest :: Decode UpdateStreamRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStreamRequest :: Encode UpdateStreamRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateStreamResponse = UpdateStreamResponse 
  { "StreamId'" :: NullOrUndefined.NullOrUndefined (StreamId)
  , "StreamArn'" :: NullOrUndefined.NullOrUndefined (StreamArn)
  , "Description'" :: NullOrUndefined.NullOrUndefined (StreamDescription)
  , "StreamVersion'" :: NullOrUndefined.NullOrUndefined (StreamVersion)
  }
derive instance newtypeUpdateStreamResponse :: Newtype UpdateStreamResponse _
derive instance repGenericUpdateStreamResponse :: Generic UpdateStreamResponse _
instance showUpdateStreamResponse :: Show UpdateStreamResponse where
  show = genericShow
instance decodeUpdateStreamResponse :: Decode UpdateStreamResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStreamResponse :: Encode UpdateStreamResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateThingGroupRequest = UpdateThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  , "ThingGroupProperties'" :: (ThingGroupProperties)
  , "ExpectedVersion'" :: NullOrUndefined.NullOrUndefined (OptionalVersion)
  }
derive instance newtypeUpdateThingGroupRequest :: Newtype UpdateThingGroupRequest _
derive instance repGenericUpdateThingGroupRequest :: Generic UpdateThingGroupRequest _
instance showUpdateThingGroupRequest :: Show UpdateThingGroupRequest where
  show = genericShow
instance decodeUpdateThingGroupRequest :: Decode UpdateThingGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateThingGroupRequest :: Encode UpdateThingGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateThingGroupResponse = UpdateThingGroupResponse 
  { "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  }
derive instance newtypeUpdateThingGroupResponse :: Newtype UpdateThingGroupResponse _
derive instance repGenericUpdateThingGroupResponse :: Generic UpdateThingGroupResponse _
instance showUpdateThingGroupResponse :: Show UpdateThingGroupResponse where
  show = genericShow
instance decodeUpdateThingGroupResponse :: Decode UpdateThingGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateThingGroupResponse :: Encode UpdateThingGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateThingGroupsForThingRequest = UpdateThingGroupsForThingRequest 
  { "ThingName'" :: NullOrUndefined.NullOrUndefined (ThingName)
  , "ThingGroupsToAdd'" :: NullOrUndefined.NullOrUndefined (ThingGroupList)
  , "ThingGroupsToRemove'" :: NullOrUndefined.NullOrUndefined (ThingGroupList)
  }
derive instance newtypeUpdateThingGroupsForThingRequest :: Newtype UpdateThingGroupsForThingRequest _
derive instance repGenericUpdateThingGroupsForThingRequest :: Generic UpdateThingGroupsForThingRequest _
instance showUpdateThingGroupsForThingRequest :: Show UpdateThingGroupsForThingRequest where
  show = genericShow
instance decodeUpdateThingGroupsForThingRequest :: Decode UpdateThingGroupsForThingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateThingGroupsForThingRequest :: Encode UpdateThingGroupsForThingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateThingGroupsForThingResponse = UpdateThingGroupsForThingResponse Types.NoArguments
derive instance newtypeUpdateThingGroupsForThingResponse :: Newtype UpdateThingGroupsForThingResponse _
derive instance repGenericUpdateThingGroupsForThingResponse :: Generic UpdateThingGroupsForThingResponse _
instance showUpdateThingGroupsForThingResponse :: Show UpdateThingGroupsForThingResponse where
  show = genericShow
instance decodeUpdateThingGroupsForThingResponse :: Decode UpdateThingGroupsForThingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateThingGroupsForThingResponse :: Encode UpdateThingGroupsForThingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the UpdateThing operation.</p>
newtype UpdateThingRequest = UpdateThingRequest 
  { "ThingName'" :: (ThingName)
  , "ThingTypeName'" :: NullOrUndefined.NullOrUndefined (ThingTypeName)
  , "AttributePayload'" :: NullOrUndefined.NullOrUndefined (AttributePayload)
  , "ExpectedVersion'" :: NullOrUndefined.NullOrUndefined (OptionalVersion)
  , "RemoveThingType'" :: NullOrUndefined.NullOrUndefined (RemoveThingType)
  }
derive instance newtypeUpdateThingRequest :: Newtype UpdateThingRequest _
derive instance repGenericUpdateThingRequest :: Generic UpdateThingRequest _
instance showUpdateThingRequest :: Show UpdateThingRequest where
  show = genericShow
instance decodeUpdateThingRequest :: Decode UpdateThingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateThingRequest :: Encode UpdateThingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from the UpdateThing operation.</p>
newtype UpdateThingResponse = UpdateThingResponse Types.NoArguments
derive instance newtypeUpdateThingResponse :: Newtype UpdateThingResponse _
derive instance repGenericUpdateThingResponse :: Generic UpdateThingResponse _
instance showUpdateThingResponse :: Show UpdateThingResponse where
  show = genericShow
instance decodeUpdateThingResponse :: Decode UpdateThingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateThingResponse :: Encode UpdateThingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UseBase64 = UseBase64 Boolean
derive instance newtypeUseBase64 :: Newtype UseBase64 _
derive instance repGenericUseBase64 :: Generic UseBase64 _
instance showUseBase64 :: Show UseBase64 where
  show = genericShow
instance decodeUseBase64 :: Decode UseBase64 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUseBase64 :: Encode UseBase64 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Value = Value String
derive instance newtypeValue :: Newtype Value _
derive instance repGenericValue :: Generic Value _
instance showValue :: Show Value where
  show = genericShow
instance decodeValue :: Decode Value where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValue :: Encode Value where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Version = Version Number
derive instance newtypeVersion :: Newtype Version _
derive instance repGenericVersion :: Generic Version _
instance showVersion :: Show Version where
  show = genericShow
instance decodeVersion :: Decode Version where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersion :: Encode Version where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An exception thrown when the version of a thing passed to a command is different than the version specified with the --version parameter.</p>
newtype VersionConflictException = VersionConflictException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeVersionConflictException :: Newtype VersionConflictException _
derive instance repGenericVersionConflictException :: Generic VersionConflictException _
instance showVersionConflictException :: Show VersionConflictException where
  show = genericShow
instance decodeVersionConflictException :: Decode VersionConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionConflictException :: Encode VersionConflictException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of policy versions exceeds the limit.</p>
newtype VersionsLimitExceededException = VersionsLimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeVersionsLimitExceededException :: Newtype VersionsLimitExceededException _
derive instance repGenericVersionsLimitExceededException :: Generic VersionsLimitExceededException _
instance showVersionsLimitExceededException :: Show VersionsLimitExceededException where
  show = genericShow
instance decodeVersionsLimitExceededException :: Decode VersionsLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionsLimitExceededException :: Encode VersionsLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
derive instance repGenericErrorMessage' :: Generic ErrorMessage' _
instance showErrorMessage' :: Show ErrorMessage' where
  show = genericShow
instance decodeErrorMessage' :: Decode ErrorMessage' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage' :: Encode ErrorMessage' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceArn' = ResourceArn' String
derive instance newtypeResourceArn' :: Newtype ResourceArn' _
derive instance repGenericResourceArn' :: Generic ResourceArn' _
instance showResourceArn' :: Show ResourceArn' where
  show = genericShow
instance decodeResourceArn' :: Decode ResourceArn' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceArn' :: Encode ResourceArn' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceId' = ResourceId' String
derive instance newtypeResourceId' :: Newtype ResourceId' _
derive instance repGenericResourceId' :: Generic ResourceId' _
instance showResourceId' :: Show ResourceId' where
  show = genericShow
instance decodeResourceId' :: Decode ResourceId' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceId' :: Encode ResourceId' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
