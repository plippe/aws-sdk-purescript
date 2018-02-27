

-- | <fullname>AWS IoT</fullname> <p>AWS IoT provides secure, bi-directional communication between Internet-connected things (such as sensors, actuators, embedded devices, or smart appliances) and the AWS cloud. You can discover your custom IoT-Data endpoint to communicate with, configure rules for data processing and integration with other services, organize resources associated with each thing (Thing Registry), configure logging, and create and manage policies and credentials to authenticate things.</p> <p>For more information about how AWS IoT works, see the <a href="http://docs.aws.amazon.com/iot/latest/developerguide/aws-iot-how-it-works.html">Developer Guide</a>.</p>
module AWS.Iot where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Iot" :: String


-- | <p>Accepts a pending certificate transfer. The default state of the certificate is INACTIVE.</p> <p>To check for pending certificate transfers, call <a>ListCertificates</a> to enumerate your certificates.</p>
acceptCertificateTransfer :: forall eff. AcceptCertificateTransferRequest -> Aff (err :: AWS.RequestError | eff) Unit
acceptCertificateTransfer = AWS.request serviceName "acceptCertificateTransfer" 


-- | <p>Adds a thing to a thing group.</p>
addThingToThingGroup :: forall eff. AddThingToThingGroupRequest -> Aff (err :: AWS.RequestError | eff) AddThingToThingGroupResponse
addThingToThingGroup = AWS.request serviceName "addThingToThingGroup" 


-- | <p>Associates a group with a continuous job. The following criteria must be met: </p> <ul> <li> <p>The job must have been created with the <code>targetSelection</code> field set to "CONTINUOUS".</p> </li> <li> <p>The job status must currently be "IN_PROGRESS".</p> </li> <li> <p>The total number of targets associated with a job must not exceed 100.</p> </li> </ul>
associateTargetsWithJob :: forall eff. AssociateTargetsWithJobRequest -> Aff (err :: AWS.RequestError | eff) AssociateTargetsWithJobResponse
associateTargetsWithJob = AWS.request serviceName "associateTargetsWithJob" 


-- | <p>Attaches a policy to the specified target.</p>
attachPolicy :: forall eff. AttachPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
attachPolicy = AWS.request serviceName "attachPolicy" 


-- | <p>Attaches the specified policy to the specified principal (certificate or other credential).</p> <p> <b>Note:</b> This API is deprecated. Please use <a>AttachPolicy</a> instead.</p>
attachPrincipalPolicy :: forall eff. AttachPrincipalPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
attachPrincipalPolicy = AWS.request serviceName "attachPrincipalPolicy" 


-- | <p>Attaches the specified principal to the specified thing.</p>
attachThingPrincipal :: forall eff. AttachThingPrincipalRequest -> Aff (err :: AWS.RequestError | eff) AttachThingPrincipalResponse
attachThingPrincipal = AWS.request serviceName "attachThingPrincipal" 


-- | <p>Cancels a pending transfer for the specified certificate.</p> <p> <b>Note</b> Only the transfer source account can use this operation to cancel a transfer. (Transfer destinations can use <a>RejectCertificateTransfer</a> instead.) After transfer, AWS IoT returns the certificate to the source account in the INACTIVE state. After the destination account has accepted the transfer, the transfer cannot be cancelled.</p> <p>After a certificate transfer is cancelled, the status of the certificate changes from PENDING_TRANSFER to INACTIVE.</p>
cancelCertificateTransfer :: forall eff. CancelCertificateTransferRequest -> Aff (err :: AWS.RequestError | eff) Unit
cancelCertificateTransfer = AWS.request serviceName "cancelCertificateTransfer" 


-- | <p>Cancels a job.</p>
cancelJob :: forall eff. CancelJobRequest -> Aff (err :: AWS.RequestError | eff) CancelJobResponse
cancelJob = AWS.request serviceName "cancelJob" 


-- | <p>Clears the default authorizer.</p>
clearDefaultAuthorizer :: forall eff. ClearDefaultAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) ClearDefaultAuthorizerResponse
clearDefaultAuthorizer = AWS.request serviceName "clearDefaultAuthorizer" 


-- | <p>Creates an authorizer.</p>
createAuthorizer :: forall eff. CreateAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) CreateAuthorizerResponse
createAuthorizer = AWS.request serviceName "createAuthorizer" 


-- | <p>Creates an X.509 certificate using the specified certificate signing request.</p> <p> <b>Note:</b> The CSR must include a public key that is either an RSA key with a length of at least 2048 bits or an ECC key from NIST P-256 or NIST P-384 curves. </p> <p> <b>Note:</b> Reusing the same certificate signing request (CSR) results in a distinct certificate.</p> <p>You can create multiple certificates in a batch by creating a directory, copying multiple .csr files into that directory, and then specifying that directory on the command line. The following commands show how to create a batch of certificates given a batch of CSRs.</p> <p>Assuming a set of CSRs are located inside of the directory my-csr-directory:</p> <p>On Linux and OS X, the command is:</p> <p>$ ls my-csr-directory/ | xargs -I {} aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/{}</p> <p>This command lists all of the CSRs in my-csr-directory and pipes each CSR file name to the aws iot create-certificate-from-csr AWS CLI command to create a certificate for the corresponding CSR.</p> <p>The aws iot create-certificate-from-csr part of the command can also be run in parallel to speed up the certificate creation process:</p> <p>$ ls my-csr-directory/ | xargs -P 10 -I {} aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/{}</p> <p>On Windows PowerShell, the command to create certificates for all CSRs in my-csr-directory is:</p> <p>&gt; ls -Name my-csr-directory | %{aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/$_}</p> <p>On a Windows command prompt, the command to create certificates for all CSRs in my-csr-directory is:</p> <p>&gt; forfiles /p my-csr-directory /c "cmd /c aws iot create-certificate-from-csr --certificate-signing-request file://@path"</p>
createCertificateFromCsr :: forall eff. CreateCertificateFromCsrRequest -> Aff (err :: AWS.RequestError | eff) CreateCertificateFromCsrResponse
createCertificateFromCsr = AWS.request serviceName "createCertificateFromCsr" 


-- | <p>Creates a job.</p>
createJob :: forall eff. CreateJobRequest -> Aff (err :: AWS.RequestError | eff) CreateJobResponse
createJob = AWS.request serviceName "createJob" 


-- | <p>Creates a 2048-bit RSA key pair and issues an X.509 certificate using the issued public key.</p> <p> <b>Note</b> This is the only time AWS IoT issues the private key for this certificate, so it is important to keep it in a secure location.</p>
createKeysAndCertificate :: forall eff. CreateKeysAndCertificateRequest -> Aff (err :: AWS.RequestError | eff) CreateKeysAndCertificateResponse
createKeysAndCertificate = AWS.request serviceName "createKeysAndCertificate" 


-- | <p>Creates an AWS IoT OTAUpdate on a target group of things or groups.</p>
createOTAUpdate :: forall eff. CreateOTAUpdateRequest -> Aff (err :: AWS.RequestError | eff) CreateOTAUpdateResponse
createOTAUpdate = AWS.request serviceName "createOTAUpdate" 


-- | <p>Creates an AWS IoT policy.</p> <p>The created policy is the default version for the policy. This operation creates a policy version with a version identifier of <b>1</b> and sets <b>1</b> as the policy's default version.</p>
createPolicy :: forall eff. CreatePolicyRequest -> Aff (err :: AWS.RequestError | eff) CreatePolicyResponse
createPolicy = AWS.request serviceName "createPolicy" 


-- | <p>Creates a new version of the specified AWS IoT policy. To update a policy, create a new policy version. A managed policy can have up to five versions. If the policy has five versions, you must use <a>DeletePolicyVersion</a> to delete an existing version before you create a new one.</p> <p>Optionally, you can set the new version as the policy's default version. The default version is the operative version (that is, the version that is in effect for the certificates to which the policy is attached).</p>
createPolicyVersion :: forall eff. CreatePolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) CreatePolicyVersionResponse
createPolicyVersion = AWS.request serviceName "createPolicyVersion" 


-- | <p>Creates a role alias.</p>
createRoleAlias :: forall eff. CreateRoleAliasRequest -> Aff (err :: AWS.RequestError | eff) CreateRoleAliasResponse
createRoleAlias = AWS.request serviceName "createRoleAlias" 


-- | <p>Creates a stream for delivering one or more large files in chunks over MQTT. A stream transports data bytes in chunks or blocks packaged as MQTT messages from a source like S3. You can have one or more files associated with a stream. The total size of a file associated with the stream cannot exceed more than 2 MB. The stream will be created with version 0. If a stream is created with the same streamID as a stream that existed and was deleted within last 90 days, we will resurrect that old stream by incrementing the version by 1.</p>
createStream :: forall eff. CreateStreamRequest -> Aff (err :: AWS.RequestError | eff) CreateStreamResponse
createStream = AWS.request serviceName "createStream" 


-- | <p>Creates a thing record in the thing registry.</p>
createThing :: forall eff. CreateThingRequest -> Aff (err :: AWS.RequestError | eff) CreateThingResponse
createThing = AWS.request serviceName "createThing" 


-- | <p>Create a thing group.</p>
createThingGroup :: forall eff. CreateThingGroupRequest -> Aff (err :: AWS.RequestError | eff) CreateThingGroupResponse
createThingGroup = AWS.request serviceName "createThingGroup" 


-- | <p>Creates a new thing type.</p>
createThingType :: forall eff. CreateThingTypeRequest -> Aff (err :: AWS.RequestError | eff) CreateThingTypeResponse
createThingType = AWS.request serviceName "createThingType" 


-- | <p>Creates a rule. Creating rules is an administrator-level action. Any user who has permission to create rules will be able to access data processed by the rule.</p>
createTopicRule :: forall eff. CreateTopicRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
createTopicRule = AWS.request serviceName "createTopicRule" 


-- | <p>Deletes an authorizer.</p>
deleteAuthorizer :: forall eff. DeleteAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) DeleteAuthorizerResponse
deleteAuthorizer = AWS.request serviceName "deleteAuthorizer" 


-- | <p>Deletes a registered CA certificate.</p>
deleteCACertificate :: forall eff. DeleteCACertificateRequest -> Aff (err :: AWS.RequestError | eff) DeleteCACertificateResponse
deleteCACertificate = AWS.request serviceName "deleteCACertificate" 


-- | <p>Deletes the specified certificate.</p> <p>A certificate cannot be deleted if it has a policy attached to it or if its status is set to ACTIVE. To delete a certificate, first use the <a>DetachPrincipalPolicy</a> API to detach all policies. Next, use the <a>UpdateCertificate</a> API to set the certificate to the INACTIVE status.</p>
deleteCertificate :: forall eff. DeleteCertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteCertificate = AWS.request serviceName "deleteCertificate" 


-- | <p>Delete an OTA update.</p>
deleteOTAUpdate :: forall eff. DeleteOTAUpdateRequest -> Aff (err :: AWS.RequestError | eff) DeleteOTAUpdateResponse
deleteOTAUpdate = AWS.request serviceName "deleteOTAUpdate" 


-- | <p>Deletes the specified policy.</p> <p>A policy cannot be deleted if it has non-default versions or it is attached to any certificate.</p> <p>To delete a policy, use the DeletePolicyVersion API to delete all non-default versions of the policy; use the DetachPrincipalPolicy API to detach the policy from any certificate; and then use the DeletePolicy API to delete the policy.</p> <p>When a policy is deleted using DeletePolicy, its default version is deleted with it.</p>
deletePolicy :: forall eff. DeletePolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
deletePolicy = AWS.request serviceName "deletePolicy" 


-- | <p>Deletes the specified version of the specified policy. You cannot delete the default version of a policy using this API. To delete the default version of a policy, use <a>DeletePolicy</a>. To find out which version of a policy is marked as the default version, use ListPolicyVersions.</p>
deletePolicyVersion :: forall eff. DeletePolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) Unit
deletePolicyVersion = AWS.request serviceName "deletePolicyVersion" 


-- | <p>Deletes a CA certificate registration code.</p>
deleteRegistrationCode :: forall eff. DeleteRegistrationCodeRequest -> Aff (err :: AWS.RequestError | eff) DeleteRegistrationCodeResponse
deleteRegistrationCode = AWS.request serviceName "deleteRegistrationCode" 


-- | <p>Deletes a role alias</p>
deleteRoleAlias :: forall eff. DeleteRoleAliasRequest -> Aff (err :: AWS.RequestError | eff) DeleteRoleAliasResponse
deleteRoleAlias = AWS.request serviceName "deleteRoleAlias" 


-- | <p>Deletes a stream.</p>
deleteStream :: forall eff. DeleteStreamRequest -> Aff (err :: AWS.RequestError | eff) DeleteStreamResponse
deleteStream = AWS.request serviceName "deleteStream" 


-- | <p>Deletes the specified thing.</p>
deleteThing :: forall eff. DeleteThingRequest -> Aff (err :: AWS.RequestError | eff) DeleteThingResponse
deleteThing = AWS.request serviceName "deleteThing" 


-- | <p>Deletes a thing group.</p>
deleteThingGroup :: forall eff. DeleteThingGroupRequest -> Aff (err :: AWS.RequestError | eff) DeleteThingGroupResponse
deleteThingGroup = AWS.request serviceName "deleteThingGroup" 


-- | <p>Deletes the specified thing type . You cannot delete a thing type if it has things associated with it. To delete a thing type, first mark it as deprecated by calling <a>DeprecateThingType</a>, then remove any associated things by calling <a>UpdateThing</a> to change the thing type on any associated thing, and finally use <a>DeleteThingType</a> to delete the thing type.</p>
deleteThingType :: forall eff. DeleteThingTypeRequest -> Aff (err :: AWS.RequestError | eff) DeleteThingTypeResponse
deleteThingType = AWS.request serviceName "deleteThingType" 


-- | <p>Deletes the rule.</p>
deleteTopicRule :: forall eff. DeleteTopicRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteTopicRule = AWS.request serviceName "deleteTopicRule" 


-- | <p>Deletes a logging level.</p>
deleteV2LoggingLevel :: forall eff. DeleteV2LoggingLevelRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteV2LoggingLevel = AWS.request serviceName "deleteV2LoggingLevel" 


-- | <p>Deprecates a thing type. You can not associate new things with deprecated thing type.</p>
deprecateThingType :: forall eff. DeprecateThingTypeRequest -> Aff (err :: AWS.RequestError | eff) DeprecateThingTypeResponse
deprecateThingType = AWS.request serviceName "deprecateThingType" 


-- | <p>Describes an authorizer.</p>
describeAuthorizer :: forall eff. DescribeAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) DescribeAuthorizerResponse
describeAuthorizer = AWS.request serviceName "describeAuthorizer" 


-- | <p>Describes a registered CA certificate.</p>
describeCACertificate :: forall eff. DescribeCACertificateRequest -> Aff (err :: AWS.RequestError | eff) DescribeCACertificateResponse
describeCACertificate = AWS.request serviceName "describeCACertificate" 


-- | <p>Gets information about the specified certificate.</p>
describeCertificate :: forall eff. DescribeCertificateRequest -> Aff (err :: AWS.RequestError | eff) DescribeCertificateResponse
describeCertificate = AWS.request serviceName "describeCertificate" 


-- | <p>Describes the default authorizer.</p>
describeDefaultAuthorizer :: forall eff. DescribeDefaultAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) DescribeDefaultAuthorizerResponse
describeDefaultAuthorizer = AWS.request serviceName "describeDefaultAuthorizer" 


-- | <p>Returns a unique endpoint specific to the AWS account making the call.</p>
describeEndpoint :: forall eff. DescribeEndpointRequest -> Aff (err :: AWS.RequestError | eff) DescribeEndpointResponse
describeEndpoint = AWS.request serviceName "describeEndpoint" 


-- | <p>Describes event configurations.</p>
describeEventConfigurations :: forall eff. DescribeEventConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventConfigurationsResponse
describeEventConfigurations = AWS.request serviceName "describeEventConfigurations" 


-- | <p>Describes a search index.</p>
describeIndex :: forall eff. DescribeIndexRequest -> Aff (err :: AWS.RequestError | eff) DescribeIndexResponse
describeIndex = AWS.request serviceName "describeIndex" 


-- | <p>Describes a job.</p>
describeJob :: forall eff. DescribeJobRequest -> Aff (err :: AWS.RequestError | eff) DescribeJobResponse
describeJob = AWS.request serviceName "describeJob" 


-- | <p>Describes a job execution.</p>
describeJobExecution :: forall eff. DescribeJobExecutionRequest -> Aff (err :: AWS.RequestError | eff) DescribeJobExecutionResponse
describeJobExecution = AWS.request serviceName "describeJobExecution" 


-- | <p>Describes a role alias.</p>
describeRoleAlias :: forall eff. DescribeRoleAliasRequest -> Aff (err :: AWS.RequestError | eff) DescribeRoleAliasResponse
describeRoleAlias = AWS.request serviceName "describeRoleAlias" 


-- | <p>Gets information about a stream.</p>
describeStream :: forall eff. DescribeStreamRequest -> Aff (err :: AWS.RequestError | eff) DescribeStreamResponse
describeStream = AWS.request serviceName "describeStream" 


-- | <p>Gets information about the specified thing.</p>
describeThing :: forall eff. DescribeThingRequest -> Aff (err :: AWS.RequestError | eff) DescribeThingResponse
describeThing = AWS.request serviceName "describeThing" 


-- | <p>Describe a thing group.</p>
describeThingGroup :: forall eff. DescribeThingGroupRequest -> Aff (err :: AWS.RequestError | eff) DescribeThingGroupResponse
describeThingGroup = AWS.request serviceName "describeThingGroup" 


-- | <p>Describes a bulk thing provisioning task.</p>
describeThingRegistrationTask :: forall eff. DescribeThingRegistrationTaskRequest -> Aff (err :: AWS.RequestError | eff) DescribeThingRegistrationTaskResponse
describeThingRegistrationTask = AWS.request serviceName "describeThingRegistrationTask" 


-- | <p>Gets information about the specified thing type.</p>
describeThingType :: forall eff. DescribeThingTypeRequest -> Aff (err :: AWS.RequestError | eff) DescribeThingTypeResponse
describeThingType = AWS.request serviceName "describeThingType" 


-- | <p>Detaches a policy from the specified target.</p>
detachPolicy :: forall eff. DetachPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
detachPolicy = AWS.request serviceName "detachPolicy" 


-- | <p>Removes the specified policy from the specified certificate.</p> <p> <b>Note:</b> This API is deprecated. Please use <a>DetachPolicy</a> instead.</p>
detachPrincipalPolicy :: forall eff. DetachPrincipalPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
detachPrincipalPolicy = AWS.request serviceName "detachPrincipalPolicy" 


-- | <p>Detaches the specified principal from the specified thing.</p>
detachThingPrincipal :: forall eff. DetachThingPrincipalRequest -> Aff (err :: AWS.RequestError | eff) DetachThingPrincipalResponse
detachThingPrincipal = AWS.request serviceName "detachThingPrincipal" 


-- | <p>Disables the rule.</p>
disableTopicRule :: forall eff. DisableTopicRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
disableTopicRule = AWS.request serviceName "disableTopicRule" 


-- | <p>Enables the rule.</p>
enableTopicRule :: forall eff. EnableTopicRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
enableTopicRule = AWS.request serviceName "enableTopicRule" 


-- | <p>Gets effective policies.</p>
getEffectivePolicies :: forall eff. GetEffectivePoliciesRequest -> Aff (err :: AWS.RequestError | eff) GetEffectivePoliciesResponse
getEffectivePolicies = AWS.request serviceName "getEffectivePolicies" 


-- | <p>Gets the search configuration.</p>
getIndexingConfiguration :: forall eff. GetIndexingConfigurationRequest -> Aff (err :: AWS.RequestError | eff) GetIndexingConfigurationResponse
getIndexingConfiguration = AWS.request serviceName "getIndexingConfiguration" 


-- | <p>Gets a job document.</p>
getJobDocument :: forall eff. GetJobDocumentRequest -> Aff (err :: AWS.RequestError | eff) GetJobDocumentResponse
getJobDocument = AWS.request serviceName "getJobDocument" 


-- | <p>Gets the logging options.</p>
getLoggingOptions :: forall eff. GetLoggingOptionsRequest -> Aff (err :: AWS.RequestError | eff) GetLoggingOptionsResponse
getLoggingOptions = AWS.request serviceName "getLoggingOptions" 


-- | <p>Gets an OTA update.</p>
getOTAUpdate :: forall eff. GetOTAUpdateRequest -> Aff (err :: AWS.RequestError | eff) GetOTAUpdateResponse
getOTAUpdate = AWS.request serviceName "getOTAUpdate" 


-- | <p>Gets information about the specified policy with the policy document of the default version.</p>
getPolicy :: forall eff. GetPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetPolicyResponse
getPolicy = AWS.request serviceName "getPolicy" 


-- | <p>Gets information about the specified policy version.</p>
getPolicyVersion :: forall eff. GetPolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) GetPolicyVersionResponse
getPolicyVersion = AWS.request serviceName "getPolicyVersion" 


-- | <p>Gets a registration code used to register a CA certificate with AWS IoT.</p>
getRegistrationCode :: forall eff. GetRegistrationCodeRequest -> Aff (err :: AWS.RequestError | eff) GetRegistrationCodeResponse
getRegistrationCode = AWS.request serviceName "getRegistrationCode" 


-- | <p>Gets information about the rule.</p>
getTopicRule :: forall eff. GetTopicRuleRequest -> Aff (err :: AWS.RequestError | eff) GetTopicRuleResponse
getTopicRule = AWS.request serviceName "getTopicRule" 


-- | <p>Gets the fine grained logging options.</p>
getV2LoggingOptions :: forall eff. GetV2LoggingOptionsRequest -> Aff (err :: AWS.RequestError | eff) GetV2LoggingOptionsResponse
getV2LoggingOptions = AWS.request serviceName "getV2LoggingOptions" 


-- | <p>Lists the policies attached to the specified thing group.</p>
listAttachedPolicies :: forall eff. ListAttachedPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListAttachedPoliciesResponse
listAttachedPolicies = AWS.request serviceName "listAttachedPolicies" 


-- | <p>Lists the authorizers registered in your account.</p>
listAuthorizers :: forall eff. ListAuthorizersRequest -> Aff (err :: AWS.RequestError | eff) ListAuthorizersResponse
listAuthorizers = AWS.request serviceName "listAuthorizers" 


-- | <p>Lists the CA certificates registered for your AWS account.</p> <p>The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.</p>
listCACertificates :: forall eff. ListCACertificatesRequest -> Aff (err :: AWS.RequestError | eff) ListCACertificatesResponse
listCACertificates = AWS.request serviceName "listCACertificates" 


-- | <p>Lists the certificates registered in your AWS account.</p> <p>The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.</p>
listCertificates :: forall eff. ListCertificatesRequest -> Aff (err :: AWS.RequestError | eff) ListCertificatesResponse
listCertificates = AWS.request serviceName "listCertificates" 


-- | <p>List the device certificates signed by the specified CA certificate.</p>
listCertificatesByCA :: forall eff. ListCertificatesByCARequest -> Aff (err :: AWS.RequestError | eff) ListCertificatesByCAResponse
listCertificatesByCA = AWS.request serviceName "listCertificatesByCA" 


-- | <p>Lists the search indices.</p>
listIndices :: forall eff. ListIndicesRequest -> Aff (err :: AWS.RequestError | eff) ListIndicesResponse
listIndices = AWS.request serviceName "listIndices" 


-- | <p>Lists the job executions for a job.</p>
listJobExecutionsForJob :: forall eff. ListJobExecutionsForJobRequest -> Aff (err :: AWS.RequestError | eff) ListJobExecutionsForJobResponse
listJobExecutionsForJob = AWS.request serviceName "listJobExecutionsForJob" 


-- | <p>Lists the job executions for the specified thing.</p>
listJobExecutionsForThing :: forall eff. ListJobExecutionsForThingRequest -> Aff (err :: AWS.RequestError | eff) ListJobExecutionsForThingResponse
listJobExecutionsForThing = AWS.request serviceName "listJobExecutionsForThing" 


-- | <p>Lists jobs.</p>
listJobs :: forall eff. ListJobsRequest -> Aff (err :: AWS.RequestError | eff) ListJobsResponse
listJobs = AWS.request serviceName "listJobs" 


-- | <p>Lists OTA updates.</p>
listOTAUpdates :: forall eff. ListOTAUpdatesRequest -> Aff (err :: AWS.RequestError | eff) ListOTAUpdatesResponse
listOTAUpdates = AWS.request serviceName "listOTAUpdates" 


-- | <p>Lists certificates that are being transferred but not yet accepted.</p>
listOutgoingCertificates :: forall eff. ListOutgoingCertificatesRequest -> Aff (err :: AWS.RequestError | eff) ListOutgoingCertificatesResponse
listOutgoingCertificates = AWS.request serviceName "listOutgoingCertificates" 


-- | <p>Lists your policies.</p>
listPolicies :: forall eff. ListPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListPoliciesResponse
listPolicies = AWS.request serviceName "listPolicies" 


-- | <p>Lists the principals associated with the specified policy.</p> <p> <b>Note:</b> This API is deprecated. Please use <a>ListTargetsForPolicy</a> instead.</p>
listPolicyPrincipals :: forall eff. ListPolicyPrincipalsRequest -> Aff (err :: AWS.RequestError | eff) ListPolicyPrincipalsResponse
listPolicyPrincipals = AWS.request serviceName "listPolicyPrincipals" 


-- | <p>Lists the versions of the specified policy and identifies the default version.</p>
listPolicyVersions :: forall eff. ListPolicyVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListPolicyVersionsResponse
listPolicyVersions = AWS.request serviceName "listPolicyVersions" 


-- | <p>Lists the policies attached to the specified principal. If you use an Cognito identity, the ID must be in <a href="http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetCredentialsForIdentity.html#API_GetCredentialsForIdentity_RequestSyntax">AmazonCognito Identity format</a>.</p> <p> <b>Note:</b> This API is deprecated. Please use <a>ListAttachedPolicies</a> instead.</p>
listPrincipalPolicies :: forall eff. ListPrincipalPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListPrincipalPoliciesResponse
listPrincipalPolicies = AWS.request serviceName "listPrincipalPolicies" 


-- | <p>Lists the things associated with the specified principal.</p>
listPrincipalThings :: forall eff. ListPrincipalThingsRequest -> Aff (err :: AWS.RequestError | eff) ListPrincipalThingsResponse
listPrincipalThings = AWS.request serviceName "listPrincipalThings" 


-- | <p>Lists the role aliases registered in your account.</p>
listRoleAliases :: forall eff. ListRoleAliasesRequest -> Aff (err :: AWS.RequestError | eff) ListRoleAliasesResponse
listRoleAliases = AWS.request serviceName "listRoleAliases" 


-- | <p>Lists all of the streams in your AWS account.</p>
listStreams :: forall eff. ListStreamsRequest -> Aff (err :: AWS.RequestError | eff) ListStreamsResponse
listStreams = AWS.request serviceName "listStreams" 


-- | <p>List targets for the specified policy.</p>
listTargetsForPolicy :: forall eff. ListTargetsForPolicyRequest -> Aff (err :: AWS.RequestError | eff) ListTargetsForPolicyResponse
listTargetsForPolicy = AWS.request serviceName "listTargetsForPolicy" 


-- | <p>List the thing groups in your account.</p>
listThingGroups :: forall eff. ListThingGroupsRequest -> Aff (err :: AWS.RequestError | eff) ListThingGroupsResponse
listThingGroups = AWS.request serviceName "listThingGroups" 


-- | <p>List the thing groups to which the specified thing belongs.</p>
listThingGroupsForThing :: forall eff. ListThingGroupsForThingRequest -> Aff (err :: AWS.RequestError | eff) ListThingGroupsForThingResponse
listThingGroupsForThing = AWS.request serviceName "listThingGroupsForThing" 


-- | <p>Lists the principals associated with the specified thing.</p>
listThingPrincipals :: forall eff. ListThingPrincipalsRequest -> Aff (err :: AWS.RequestError | eff) ListThingPrincipalsResponse
listThingPrincipals = AWS.request serviceName "listThingPrincipals" 


-- | <p>Information about the thing registration tasks.</p>
listThingRegistrationTaskReports :: forall eff. ListThingRegistrationTaskReportsRequest -> Aff (err :: AWS.RequestError | eff) ListThingRegistrationTaskReportsResponse
listThingRegistrationTaskReports = AWS.request serviceName "listThingRegistrationTaskReports" 


-- | <p>List bulk thing provisioning tasks.</p>
listThingRegistrationTasks :: forall eff. ListThingRegistrationTasksRequest -> Aff (err :: AWS.RequestError | eff) ListThingRegistrationTasksResponse
listThingRegistrationTasks = AWS.request serviceName "listThingRegistrationTasks" 


-- | <p>Lists the existing thing types.</p>
listThingTypes :: forall eff. ListThingTypesRequest -> Aff (err :: AWS.RequestError | eff) ListThingTypesResponse
listThingTypes = AWS.request serviceName "listThingTypes" 


-- | <p>Lists your things. Use the <b>attributeName</b> and <b>attributeValue</b> parameters to filter your things. For example, calling <code>ListThings</code> with attributeName=Color and attributeValue=Red retrieves all things in the registry that contain an attribute <b>Color</b> with the value <b>Red</b>. </p>
listThings :: forall eff. ListThingsRequest -> Aff (err :: AWS.RequestError | eff) ListThingsResponse
listThings = AWS.request serviceName "listThings" 


-- | <p>Lists the things in the specified group.</p>
listThingsInThingGroup :: forall eff. ListThingsInThingGroupRequest -> Aff (err :: AWS.RequestError | eff) ListThingsInThingGroupResponse
listThingsInThingGroup = AWS.request serviceName "listThingsInThingGroup" 


-- | <p>Lists the rules for the specific topic.</p>
listTopicRules :: forall eff. ListTopicRulesRequest -> Aff (err :: AWS.RequestError | eff) ListTopicRulesResponse
listTopicRules = AWS.request serviceName "listTopicRules" 


-- | <p>Lists logging levels.</p>
listV2LoggingLevels :: forall eff. ListV2LoggingLevelsRequest -> Aff (err :: AWS.RequestError | eff) ListV2LoggingLevelsResponse
listV2LoggingLevels = AWS.request serviceName "listV2LoggingLevels" 


-- | <p>Registers a CA certificate with AWS IoT. This CA certificate can then be used to sign device certificates, which can be then registered with AWS IoT. You can register up to 10 CA certificates per AWS account that have the same subject field. This enables you to have up to 10 certificate authorities sign your device certificates. If you have more than one CA certificate registered, make sure you pass the CA certificate when you register your device certificates with the RegisterCertificate API.</p>
registerCACertificate :: forall eff. RegisterCACertificateRequest -> Aff (err :: AWS.RequestError | eff) RegisterCACertificateResponse
registerCACertificate = AWS.request serviceName "registerCACertificate" 


-- | <p>Registers a device certificate with AWS IoT. If you have more than one CA certificate that has the same subject field, you must specify the CA certificate that was used to sign the device certificate being registered.</p>
registerCertificate :: forall eff. RegisterCertificateRequest -> Aff (err :: AWS.RequestError | eff) RegisterCertificateResponse
registerCertificate = AWS.request serviceName "registerCertificate" 


-- | <p>Provisions a thing.</p>
registerThing :: forall eff. RegisterThingRequest -> Aff (err :: AWS.RequestError | eff) RegisterThingResponse
registerThing = AWS.request serviceName "registerThing" 


-- | <p>Rejects a pending certificate transfer. After AWS IoT rejects a certificate transfer, the certificate status changes from <b>PENDING_TRANSFER</b> to <b>INACTIVE</b>.</p> <p>To check for pending certificate transfers, call <a>ListCertificates</a> to enumerate your certificates.</p> <p>This operation can only be called by the transfer destination. After it is called, the certificate will be returned to the source's account in the INACTIVE state.</p>
rejectCertificateTransfer :: forall eff. RejectCertificateTransferRequest -> Aff (err :: AWS.RequestError | eff) Unit
rejectCertificateTransfer = AWS.request serviceName "rejectCertificateTransfer" 


-- | <p>Remove the specified thing from the specified group.</p>
removeThingFromThingGroup :: forall eff. RemoveThingFromThingGroupRequest -> Aff (err :: AWS.RequestError | eff) RemoveThingFromThingGroupResponse
removeThingFromThingGroup = AWS.request serviceName "removeThingFromThingGroup" 


-- | <p>Replaces the rule. You must specify all parameters for the new rule. Creating rules is an administrator-level action. Any user who has permission to create rules will be able to access data processed by the rule.</p>
replaceTopicRule :: forall eff. ReplaceTopicRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
replaceTopicRule = AWS.request serviceName "replaceTopicRule" 


-- | <p>The query search index.</p>
searchIndex :: forall eff. SearchIndexRequest -> Aff (err :: AWS.RequestError | eff) SearchIndexResponse
searchIndex = AWS.request serviceName "searchIndex" 


-- | <p>Sets the default authorizer. This will be used if a websocket connection is made without specifying an authorizer.</p>
setDefaultAuthorizer :: forall eff. SetDefaultAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) SetDefaultAuthorizerResponse
setDefaultAuthorizer = AWS.request serviceName "setDefaultAuthorizer" 


-- | <p>Sets the specified version of the specified policy as the policy's default (operative) version. This action affects all certificates to which the policy is attached. To list the principals the policy is attached to, use the ListPrincipalPolicy API.</p>
setDefaultPolicyVersion :: forall eff. SetDefaultPolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) Unit
setDefaultPolicyVersion = AWS.request serviceName "setDefaultPolicyVersion" 


-- | <p>Sets the logging options.</p>
setLoggingOptions :: forall eff. SetLoggingOptionsRequest -> Aff (err :: AWS.RequestError | eff) Unit
setLoggingOptions = AWS.request serviceName "setLoggingOptions" 


-- | <p>Sets the logging level.</p>
setV2LoggingLevel :: forall eff. SetV2LoggingLevelRequest -> Aff (err :: AWS.RequestError | eff) Unit
setV2LoggingLevel = AWS.request serviceName "setV2LoggingLevel" 


-- | <p>Sets the logging options for the V2 logging service.</p>
setV2LoggingOptions :: forall eff. SetV2LoggingOptionsRequest -> Aff (err :: AWS.RequestError | eff) Unit
setV2LoggingOptions = AWS.request serviceName "setV2LoggingOptions" 


-- | <p>Creates a bulk thing provisioning task.</p>
startThingRegistrationTask :: forall eff. StartThingRegistrationTaskRequest -> Aff (err :: AWS.RequestError | eff) StartThingRegistrationTaskResponse
startThingRegistrationTask = AWS.request serviceName "startThingRegistrationTask" 


-- | <p>Cancels a bulk thing provisioning task.</p>
stopThingRegistrationTask :: forall eff. StopThingRegistrationTaskRequest -> Aff (err :: AWS.RequestError | eff) StopThingRegistrationTaskResponse
stopThingRegistrationTask = AWS.request serviceName "stopThingRegistrationTask" 


-- | <p>Test custom authorization.</p>
testAuthorization :: forall eff. TestAuthorizationRequest -> Aff (err :: AWS.RequestError | eff) TestAuthorizationResponse
testAuthorization = AWS.request serviceName "testAuthorization" 


-- | <p>Invoke the specified custom authorizer for testing purposes.</p>
testInvokeAuthorizer :: forall eff. TestInvokeAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) TestInvokeAuthorizerResponse
testInvokeAuthorizer = AWS.request serviceName "testInvokeAuthorizer" 


-- | <p>Transfers the specified certificate to the specified AWS account.</p> <p>You can cancel the transfer until it is acknowledged by the recipient.</p> <p>No notification is sent to the transfer destination's account. It is up to the caller to notify the transfer target.</p> <p>The certificate being transferred must not be in the ACTIVE state. You can use the UpdateCertificate API to deactivate it.</p> <p>The certificate must not have any policies attached to it. You can use the DetachPrincipalPolicy API to detach them.</p>
transferCertificate :: forall eff. TransferCertificateRequest -> Aff (err :: AWS.RequestError | eff) TransferCertificateResponse
transferCertificate = AWS.request serviceName "transferCertificate" 


-- | <p>Updates an authorizer.</p>
updateAuthorizer :: forall eff. UpdateAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) UpdateAuthorizerResponse
updateAuthorizer = AWS.request serviceName "updateAuthorizer" 


-- | <p>Updates a registered CA certificate.</p>
updateCACertificate :: forall eff. UpdateCACertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateCACertificate = AWS.request serviceName "updateCACertificate" 


-- | <p>Updates the status of the specified certificate. This operation is idempotent.</p> <p>Moving a certificate from the ACTIVE state (including REVOKED) will not disconnect currently connected devices, but these devices will be unable to reconnect.</p> <p>The ACTIVE state is required to authenticate devices connecting to AWS IoT using a certificate.</p>
updateCertificate :: forall eff. UpdateCertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateCertificate = AWS.request serviceName "updateCertificate" 


-- | <p>Updates the event configurations.</p>
updateEventConfigurations :: forall eff. UpdateEventConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) UpdateEventConfigurationsResponse
updateEventConfigurations = AWS.request serviceName "updateEventConfigurations" 


-- | <p>Updates the search configuration.</p>
updateIndexingConfiguration :: forall eff. UpdateIndexingConfigurationRequest -> Aff (err :: AWS.RequestError | eff) UpdateIndexingConfigurationResponse
updateIndexingConfiguration = AWS.request serviceName "updateIndexingConfiguration" 


-- | <p>Updates a role alias.</p>
updateRoleAlias :: forall eff. UpdateRoleAliasRequest -> Aff (err :: AWS.RequestError | eff) UpdateRoleAliasResponse
updateRoleAlias = AWS.request serviceName "updateRoleAlias" 


-- | <p>Updates an existing stream. The stream version will be incremented by one.</p>
updateStream :: forall eff. UpdateStreamRequest -> Aff (err :: AWS.RequestError | eff) UpdateStreamResponse
updateStream = AWS.request serviceName "updateStream" 


-- | <p>Updates the data for a thing.</p>
updateThing :: forall eff. UpdateThingRequest -> Aff (err :: AWS.RequestError | eff) UpdateThingResponse
updateThing = AWS.request serviceName "updateThing" 


-- | <p>Update a thing group.</p>
updateThingGroup :: forall eff. UpdateThingGroupRequest -> Aff (err :: AWS.RequestError | eff) UpdateThingGroupResponse
updateThingGroup = AWS.request serviceName "updateThingGroup" 


-- | <p>Updates the groups to which the thing belongs.</p>
updateThingGroupsForThing :: forall eff. UpdateThingGroupsForThingRequest -> Aff (err :: AWS.RequestError | eff) UpdateThingGroupsForThingResponse
updateThingGroupsForThing = AWS.request serviceName "updateThingGroupsForThing" 


-- | <p>The input for the AcceptCertificateTransfer operation.</p>
newtype AcceptCertificateTransferRequest = AcceptCertificateTransferRequest 
  { "CertificateId'" :: (CertificateId)
  , "SetAsActive'" :: NullOrUndefined (SetAsActive)
  }
derive instance newtypeAcceptCertificateTransferRequest :: Newtype AcceptCertificateTransferRequest _


-- | <p>Describes the actions associated with a rule.</p>
newtype Action = Action 
  { "DynamoDB'" :: NullOrUndefined (DynamoDBAction)
  , "DynamoDBv2'" :: NullOrUndefined (DynamoDBv2Action)
  , "Lambda'" :: NullOrUndefined (LambdaAction)
  , "Sns'" :: NullOrUndefined (SnsAction)
  , "Sqs'" :: NullOrUndefined (SqsAction)
  , "Kinesis'" :: NullOrUndefined (KinesisAction)
  , "Republish'" :: NullOrUndefined (RepublishAction)
  , "S3'" :: NullOrUndefined (S3Action)
  , "Firehose'" :: NullOrUndefined (FirehoseAction)
  , "CloudwatchMetric'" :: NullOrUndefined (CloudwatchMetricAction)
  , "CloudwatchAlarm'" :: NullOrUndefined (CloudwatchAlarmAction)
  , "Elasticsearch'" :: NullOrUndefined (ElasticsearchAction)
  , "Salesforce'" :: NullOrUndefined (SalesforceAction)
  }
derive instance newtypeAction :: Newtype Action _


newtype ActionList = ActionList (Array Action)
derive instance newtypeActionList :: Newtype ActionList _


newtype ActionType = ActionType String
derive instance newtypeActionType :: Newtype ActionType _


newtype AddThingToThingGroupRequest = AddThingToThingGroupRequest 
  { "ThingGroupName'" :: NullOrUndefined (ThingGroupName)
  , "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn)
  , "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingArn'" :: NullOrUndefined (ThingArn)
  }
derive instance newtypeAddThingToThingGroupRequest :: Newtype AddThingToThingGroupRequest _


newtype AddThingToThingGroupResponse = AddThingToThingGroupResponse 
  { 
  }
derive instance newtypeAddThingToThingGroupResponse :: Newtype AddThingToThingGroupResponse _


newtype AdditionalParameterMap = AdditionalParameterMap (Map Key Value)
derive instance newtypeAdditionalParameterMap :: Newtype AdditionalParameterMap _


newtype AlarmName = AlarmName String
derive instance newtypeAlarmName :: Newtype AlarmName _


newtype AllowAutoRegistration = AllowAutoRegistration Boolean
derive instance newtypeAllowAutoRegistration :: Newtype AllowAutoRegistration _


-- | <p>Contains information that allowed the authorization.</p>
newtype Allowed = Allowed 
  { "Policies'" :: NullOrUndefined (Policies)
  }
derive instance newtypeAllowed :: Newtype Allowed _


newtype AscendingOrder = AscendingOrder Boolean
derive instance newtypeAscendingOrder :: Newtype AscendingOrder _


newtype AssociateTargetsWithJobRequest = AssociateTargetsWithJobRequest 
  { "Targets'" :: (JobTargets)
  , "JobId'" :: (JobId)
  , "Comment'" :: NullOrUndefined (Comment)
  }
derive instance newtypeAssociateTargetsWithJobRequest :: Newtype AssociateTargetsWithJobRequest _


newtype AssociateTargetsWithJobResponse = AssociateTargetsWithJobResponse 
  { "JobArn'" :: NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined (JobId)
  , "Description'" :: NullOrUndefined (JobDescription)
  }
derive instance newtypeAssociateTargetsWithJobResponse :: Newtype AssociateTargetsWithJobResponse _


newtype AttachPolicyRequest = AttachPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Target'" :: (PolicyTarget)
  }
derive instance newtypeAttachPolicyRequest :: Newtype AttachPolicyRequest _


-- | <p>The input for the AttachPrincipalPolicy operation.</p>
newtype AttachPrincipalPolicyRequest = AttachPrincipalPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Principal'" :: (Principal)
  }
derive instance newtypeAttachPrincipalPolicyRequest :: Newtype AttachPrincipalPolicyRequest _


-- | <p>The input for the AttachThingPrincipal operation.</p>
newtype AttachThingPrincipalRequest = AttachThingPrincipalRequest 
  { "ThingName'" :: (ThingName)
  , "Principal'" :: (Principal)
  }
derive instance newtypeAttachThingPrincipalRequest :: Newtype AttachThingPrincipalRequest _


-- | <p>The output from the AttachThingPrincipal operation.</p>
newtype AttachThingPrincipalResponse = AttachThingPrincipalResponse 
  { 
  }
derive instance newtypeAttachThingPrincipalResponse :: Newtype AttachThingPrincipalResponse _


newtype AttributeName = AttributeName String
derive instance newtypeAttributeName :: Newtype AttributeName _


-- | <p>The attribute payload.</p>
newtype AttributePayload = AttributePayload 
  { "Attributes'" :: NullOrUndefined (Attributes)
  , "Merge'" :: NullOrUndefined (Flag)
  }
derive instance newtypeAttributePayload :: Newtype AttributePayload _


newtype AttributeValue = AttributeValue String
derive instance newtypeAttributeValue :: Newtype AttributeValue _


newtype Attributes = Attributes (Map AttributeName AttributeValue)
derive instance newtypeAttributes :: Newtype Attributes _


newtype AttributesMap = AttributesMap (Map Key Value)
derive instance newtypeAttributesMap :: Newtype AttributesMap _


newtype AuthDecision = AuthDecision String
derive instance newtypeAuthDecision :: Newtype AuthDecision _


-- | <p>A collection of authorization information.</p>
newtype AuthInfo = AuthInfo 
  { "ActionType'" :: NullOrUndefined (ActionType)
  , "Resources'" :: NullOrUndefined (Resources)
  }
derive instance newtypeAuthInfo :: Newtype AuthInfo _


newtype AuthInfos = AuthInfos (Array AuthInfo)
derive instance newtypeAuthInfos :: Newtype AuthInfos _


-- | <p>The authorizer result.</p>
newtype AuthResult = AuthResult 
  { "AuthInfo'" :: NullOrUndefined (AuthInfo)
  , "Allowed'" :: NullOrUndefined (Allowed)
  , "Denied'" :: NullOrUndefined (Denied)
  , "AuthDecision'" :: NullOrUndefined (AuthDecision)
  , "MissingContextValues'" :: NullOrUndefined (MissingContextValues)
  }
derive instance newtypeAuthResult :: Newtype AuthResult _


newtype AuthResults = AuthResults (Array AuthResult)
derive instance newtypeAuthResults :: Newtype AuthResults _


newtype AuthorizerArn = AuthorizerArn String
derive instance newtypeAuthorizerArn :: Newtype AuthorizerArn _


-- | <p>The authorizer description.</p>
newtype AuthorizerDescription = AuthorizerDescription 
  { "AuthorizerName'" :: NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn)
  , "AuthorizerFunctionArn'" :: NullOrUndefined (AuthorizerFunctionArn)
  , "TokenKeyName'" :: NullOrUndefined (TokenKeyName)
  , "TokenSigningPublicKeys'" :: NullOrUndefined (PublicKeyMap)
  , "Status'" :: NullOrUndefined (AuthorizerStatus)
  , "CreationDate'" :: NullOrUndefined (DateType)
  , "LastModifiedDate'" :: NullOrUndefined (DateType)
  }
derive instance newtypeAuthorizerDescription :: Newtype AuthorizerDescription _


newtype AuthorizerFunctionArn = AuthorizerFunctionArn String
derive instance newtypeAuthorizerFunctionArn :: Newtype AuthorizerFunctionArn _


newtype AuthorizerName = AuthorizerName String
derive instance newtypeAuthorizerName :: Newtype AuthorizerName _


newtype AuthorizerStatus = AuthorizerStatus String
derive instance newtypeAuthorizerStatus :: Newtype AuthorizerStatus _


-- | <p>The authorizer summary.</p>
newtype AuthorizerSummary = AuthorizerSummary 
  { "AuthorizerName'" :: NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn)
  }
derive instance newtypeAuthorizerSummary :: Newtype AuthorizerSummary _


newtype Authorizers = Authorizers (Array AuthorizerSummary)
derive instance newtypeAuthorizers :: Newtype Authorizers _


newtype AutoRegistrationStatus = AutoRegistrationStatus String
derive instance newtypeAutoRegistrationStatus :: Newtype AutoRegistrationStatus _


newtype AwsAccountId = AwsAccountId String
derive instance newtypeAwsAccountId :: Newtype AwsAccountId _


newtype AwsArn = AwsArn String
derive instance newtypeAwsArn :: Newtype AwsArn _


newtype AwsIotJobArn = AwsIotJobArn String
derive instance newtypeAwsIotJobArn :: Newtype AwsIotJobArn _


newtype AwsIotJobId = AwsIotJobId String
derive instance newtypeAwsIotJobId :: Newtype AwsIotJobId _


newtype AwsIotSqlVersion = AwsIotSqlVersion String
derive instance newtypeAwsIotSqlVersion :: Newtype AwsIotSqlVersion _


newtype BucketName = BucketName String
derive instance newtypeBucketName :: Newtype BucketName _


-- | <p>A CA certificate.</p>
newtype CACertificate = CACertificate 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  , "Status'" :: NullOrUndefined (CACertificateStatus)
  , "CreationDate'" :: NullOrUndefined (DateType)
  }
derive instance newtypeCACertificate :: Newtype CACertificate _


-- | <p>Describes a CA certificate.</p>
newtype CACertificateDescription = CACertificateDescription 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  , "Status'" :: NullOrUndefined (CACertificateStatus)
  , "CertificatePem'" :: NullOrUndefined (CertificatePem)
  , "OwnedBy'" :: NullOrUndefined (AwsAccountId)
  , "CreationDate'" :: NullOrUndefined (DateType)
  , "AutoRegistrationStatus'" :: NullOrUndefined (AutoRegistrationStatus)
  }
derive instance newtypeCACertificateDescription :: Newtype CACertificateDescription _


newtype CACertificateStatus = CACertificateStatus String
derive instance newtypeCACertificateStatus :: Newtype CACertificateStatus _


newtype CACertificates = CACertificates (Array CACertificate)
derive instance newtypeCACertificates :: Newtype CACertificates _


-- | <p>The input for the CancelCertificateTransfer operation.</p>
newtype CancelCertificateTransferRequest = CancelCertificateTransferRequest 
  { "CertificateId'" :: (CertificateId)
  }
derive instance newtypeCancelCertificateTransferRequest :: Newtype CancelCertificateTransferRequest _


newtype CancelJobRequest = CancelJobRequest 
  { "JobId'" :: (JobId)
  , "Comment'" :: NullOrUndefined (Comment)
  }
derive instance newtypeCancelJobRequest :: Newtype CancelJobRequest _


newtype CancelJobResponse = CancelJobResponse 
  { "JobArn'" :: NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined (JobId)
  , "Description'" :: NullOrUndefined (JobDescription)
  }
derive instance newtypeCancelJobResponse :: Newtype CancelJobResponse _


newtype CanceledThings = CanceledThings Int
derive instance newtypeCanceledThings :: Newtype CanceledThings _


newtype CannedAccessControlList = CannedAccessControlList String
derive instance newtypeCannedAccessControlList :: Newtype CannedAccessControlList _


-- | <p>Information about a certificate.</p>
newtype Certificate = Certificate 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  , "Status'" :: NullOrUndefined (CertificateStatus)
  , "CreationDate'" :: NullOrUndefined (DateType)
  }
derive instance newtypeCertificate :: Newtype Certificate _


newtype CertificateArn = CertificateArn String
derive instance newtypeCertificateArn :: Newtype CertificateArn _


-- | <p>Unable to verify the CA certificate used to sign the device certificate you are attempting to register. This is happens when you have registered more than one CA certificate that has the same subject field and public key.</p>
newtype CertificateConflictException = CertificateConflictException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCertificateConflictException :: Newtype CertificateConflictException _


-- | <p>Describes a certificate.</p>
newtype CertificateDescription = CertificateDescription 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  , "CaCertificateId'" :: NullOrUndefined (CertificateId)
  , "Status'" :: NullOrUndefined (CertificateStatus)
  , "CertificatePem'" :: NullOrUndefined (CertificatePem)
  , "OwnedBy'" :: NullOrUndefined (AwsAccountId)
  , "PreviousOwnedBy'" :: NullOrUndefined (AwsAccountId)
  , "CreationDate'" :: NullOrUndefined (DateType)
  , "LastModifiedDate'" :: NullOrUndefined (DateType)
  , "TransferData'" :: NullOrUndefined (TransferData)
  }
derive instance newtypeCertificateDescription :: Newtype CertificateDescription _


newtype CertificateId = CertificateId String
derive instance newtypeCertificateId :: Newtype CertificateId _


newtype CertificateName = CertificateName String
derive instance newtypeCertificateName :: Newtype CertificateName _


-- | <p>The PEM of a certificate.</p>
newtype CertificatePem = CertificatePem String
derive instance newtypeCertificatePem :: Newtype CertificatePem _


newtype CertificateSigningRequest = CertificateSigningRequest String
derive instance newtypeCertificateSigningRequest :: Newtype CertificateSigningRequest _


-- | <p>The certificate operation is not allowed.</p>
newtype CertificateStateException = CertificateStateException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCertificateStateException :: Newtype CertificateStateException _


newtype CertificateStatus = CertificateStatus String
derive instance newtypeCertificateStatus :: Newtype CertificateStatus _


-- | <p>The certificate is invalid.</p>
newtype CertificateValidationException = CertificateValidationException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCertificateValidationException :: Newtype CertificateValidationException _


newtype Certificates = Certificates (Array Certificate)
derive instance newtypeCertificates :: Newtype Certificates _


newtype ClearDefaultAuthorizerRequest = ClearDefaultAuthorizerRequest 
  { 
  }
derive instance newtypeClearDefaultAuthorizerRequest :: Newtype ClearDefaultAuthorizerRequest _


newtype ClearDefaultAuthorizerResponse = ClearDefaultAuthorizerResponse 
  { 
  }
derive instance newtypeClearDefaultAuthorizerResponse :: Newtype ClearDefaultAuthorizerResponse _


newtype ClientId = ClientId String
derive instance newtypeClientId :: Newtype ClientId _


-- | <p>Describes an action that updates a CloudWatch alarm.</p>
newtype CloudwatchAlarmAction = CloudwatchAlarmAction 
  { "RoleArn'" :: (AwsArn)
  , "AlarmName'" :: (AlarmName)
  , "StateReason'" :: (StateReason)
  , "StateValue'" :: (StateValue)
  }
derive instance newtypeCloudwatchAlarmAction :: Newtype CloudwatchAlarmAction _


-- | <p>Describes an action that captures a CloudWatch metric.</p>
newtype CloudwatchMetricAction = CloudwatchMetricAction 
  { "RoleArn'" :: (AwsArn)
  , "MetricNamespace'" :: (MetricNamespace)
  , "MetricName'" :: (MetricName)
  , "MetricValue'" :: (MetricValue)
  , "MetricUnit'" :: (MetricUnit)
  , "MetricTimestamp'" :: NullOrUndefined (MetricTimestamp)
  }
derive instance newtypeCloudwatchMetricAction :: Newtype CloudwatchMetricAction _


newtype Code = Code String
derive instance newtypeCode :: Newtype Code _


-- | <p>Describes the method to use when code signing a file.</p>
newtype CodeSigning = CodeSigning 
  { "AwsSignerJobId'" :: NullOrUndefined (SigningJobId)
  , "CustomCodeSigning'" :: NullOrUndefined (CustomCodeSigning)
  }
derive instance newtypeCodeSigning :: Newtype CodeSigning _


-- | <p>Describes the certificate chain being used when code signing a file.</p>
newtype CodeSigningCertificateChain = CodeSigningCertificateChain 
  { "Stream'" :: NullOrUndefined (Stream)
  , "CertificateName'" :: NullOrUndefined (CertificateName)
  , "InlineDocument'" :: NullOrUndefined (InlineDocument)
  }
derive instance newtypeCodeSigningCertificateChain :: Newtype CodeSigningCertificateChain _


-- | <p>Describes the signature for a file.</p>
newtype CodeSigningSignature = CodeSigningSignature 
  { "Stream'" :: NullOrUndefined (Stream)
  , "InlineDocument'" :: NullOrUndefined (Signature)
  }
derive instance newtypeCodeSigningSignature :: Newtype CodeSigningSignature _


newtype CognitoIdentityPoolId = CognitoIdentityPoolId String
derive instance newtypeCognitoIdentityPoolId :: Newtype CognitoIdentityPoolId _


newtype Comment = Comment String
derive instance newtypeComment :: Newtype Comment _


-- | <p>Configuration.</p>
newtype Configuration = Configuration 
  { "Enabled" :: NullOrUndefined (Enabled)
  }
derive instance newtypeConfiguration :: Newtype Configuration _


-- | <p>A conflicting resource update exception. This exception is thrown when two pending updates cause a conflict.</p>
newtype ConflictingResourceUpdateException = ConflictingResourceUpdateException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeConflictingResourceUpdateException :: Newtype ConflictingResourceUpdateException _


newtype Count = Count Int
derive instance newtypeCount :: Newtype Count _


newtype CreateAuthorizerRequest = CreateAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  , "AuthorizerFunctionArn'" :: (AuthorizerFunctionArn)
  , "TokenKeyName'" :: (TokenKeyName)
  , "TokenSigningPublicKeys'" :: (PublicKeyMap)
  , "Status'" :: NullOrUndefined (AuthorizerStatus)
  }
derive instance newtypeCreateAuthorizerRequest :: Newtype CreateAuthorizerRequest _


newtype CreateAuthorizerResponse = CreateAuthorizerResponse 
  { "AuthorizerName'" :: NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn)
  }
derive instance newtypeCreateAuthorizerResponse :: Newtype CreateAuthorizerResponse _


-- | <p>The input for the CreateCertificateFromCsr operation.</p>
newtype CreateCertificateFromCsrRequest = CreateCertificateFromCsrRequest 
  { "CertificateSigningRequest'" :: (CertificateSigningRequest)
  , "SetAsActive'" :: NullOrUndefined (SetAsActive)
  }
derive instance newtypeCreateCertificateFromCsrRequest :: Newtype CreateCertificateFromCsrRequest _


-- | <p>The output from the CreateCertificateFromCsr operation.</p>
newtype CreateCertificateFromCsrResponse = CreateCertificateFromCsrResponse 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  , "CertificatePem'" :: NullOrUndefined (CertificatePem)
  }
derive instance newtypeCreateCertificateFromCsrResponse :: Newtype CreateCertificateFromCsrResponse _


newtype CreateJobRequest = CreateJobRequest 
  { "JobId'" :: (JobId)
  , "Targets'" :: (JobTargets)
  , "DocumentSource'" :: NullOrUndefined (JobDocumentSource)
  , "Document'" :: NullOrUndefined (JobDocument)
  , "Description'" :: NullOrUndefined (JobDescription)
  , "PresignedUrlConfig'" :: NullOrUndefined (PresignedUrlConfig)
  , "TargetSelection'" :: NullOrUndefined (TargetSelection)
  , "JobExecutionsRolloutConfig'" :: NullOrUndefined (JobExecutionsRolloutConfig)
  , "DocumentParameters'" :: NullOrUndefined (JobDocumentParameters)
  }
derive instance newtypeCreateJobRequest :: Newtype CreateJobRequest _


newtype CreateJobResponse = CreateJobResponse 
  { "JobArn'" :: NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined (JobId)
  , "Description'" :: NullOrUndefined (JobDescription)
  }
derive instance newtypeCreateJobResponse :: Newtype CreateJobResponse _


-- | <p>The input for the CreateKeysAndCertificate operation.</p>
newtype CreateKeysAndCertificateRequest = CreateKeysAndCertificateRequest 
  { "SetAsActive'" :: NullOrUndefined (SetAsActive)
  }
derive instance newtypeCreateKeysAndCertificateRequest :: Newtype CreateKeysAndCertificateRequest _


-- | <p>The output of the CreateKeysAndCertificate operation.</p>
newtype CreateKeysAndCertificateResponse = CreateKeysAndCertificateResponse 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  , "CertificatePem'" :: NullOrUndefined (CertificatePem)
  , "KeyPair'" :: NullOrUndefined (KeyPair)
  }
derive instance newtypeCreateKeysAndCertificateResponse :: Newtype CreateKeysAndCertificateResponse _


newtype CreateOTAUpdateRequest = CreateOTAUpdateRequest 
  { "OtaUpdateId'" :: (OTAUpdateId)
  , "Description'" :: NullOrUndefined (OTAUpdateDescription)
  , "Targets'" :: (Targets)
  , "TargetSelection'" :: NullOrUndefined (TargetSelection)
  , "Files'" :: (OTAUpdateFiles)
  , "RoleArn'" :: (RoleArn)
  , "AdditionalParameters'" :: NullOrUndefined (AdditionalParameterMap)
  }
derive instance newtypeCreateOTAUpdateRequest :: Newtype CreateOTAUpdateRequest _


newtype CreateOTAUpdateResponse = CreateOTAUpdateResponse 
  { "OtaUpdateId'" :: NullOrUndefined (OTAUpdateId)
  , "AwsIotJobId'" :: NullOrUndefined (AwsIotJobId)
  , "OtaUpdateArn'" :: NullOrUndefined (OTAUpdateArn)
  , "AwsIotJobArn'" :: NullOrUndefined (AwsIotJobArn)
  , "OtaUpdateStatus'" :: NullOrUndefined (OTAUpdateStatus)
  }
derive instance newtypeCreateOTAUpdateResponse :: Newtype CreateOTAUpdateResponse _


-- | <p>The input for the CreatePolicy operation.</p>
newtype CreatePolicyRequest = CreatePolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyDocument'" :: (PolicyDocument)
  }
derive instance newtypeCreatePolicyRequest :: Newtype CreatePolicyRequest _


-- | <p>The output from the CreatePolicy operation.</p>
newtype CreatePolicyResponse = CreatePolicyResponse 
  { "PolicyName'" :: NullOrUndefined (PolicyName)
  , "PolicyArn'" :: NullOrUndefined (PolicyArn)
  , "PolicyDocument'" :: NullOrUndefined (PolicyDocument)
  , "PolicyVersionId'" :: NullOrUndefined (PolicyVersionId)
  }
derive instance newtypeCreatePolicyResponse :: Newtype CreatePolicyResponse _


-- | <p>The input for the CreatePolicyVersion operation.</p>
newtype CreatePolicyVersionRequest = CreatePolicyVersionRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyDocument'" :: (PolicyDocument)
  , "SetAsDefault'" :: NullOrUndefined (SetAsDefault)
  }
derive instance newtypeCreatePolicyVersionRequest :: Newtype CreatePolicyVersionRequest _


-- | <p>The output of the CreatePolicyVersion operation.</p>
newtype CreatePolicyVersionResponse = CreatePolicyVersionResponse 
  { "PolicyArn'" :: NullOrUndefined (PolicyArn)
  , "PolicyDocument'" :: NullOrUndefined (PolicyDocument)
  , "PolicyVersionId'" :: NullOrUndefined (PolicyVersionId)
  , "IsDefaultVersion'" :: NullOrUndefined (IsDefaultVersion)
  }
derive instance newtypeCreatePolicyVersionResponse :: Newtype CreatePolicyVersionResponse _


newtype CreateRoleAliasRequest = CreateRoleAliasRequest 
  { "RoleAlias'" :: (RoleAlias)
  , "RoleArn'" :: (RoleArn)
  , "CredentialDurationSeconds'" :: NullOrUndefined (CredentialDurationSeconds)
  }
derive instance newtypeCreateRoleAliasRequest :: Newtype CreateRoleAliasRequest _


newtype CreateRoleAliasResponse = CreateRoleAliasResponse 
  { "RoleAlias'" :: NullOrUndefined (RoleAlias)
  , "RoleAliasArn'" :: NullOrUndefined (RoleAliasArn)
  }
derive instance newtypeCreateRoleAliasResponse :: Newtype CreateRoleAliasResponse _


newtype CreateStreamRequest = CreateStreamRequest 
  { "StreamId'" :: (StreamId)
  , "Description'" :: NullOrUndefined (StreamDescription)
  , "Files'" :: (StreamFiles)
  , "RoleArn'" :: (RoleArn)
  }
derive instance newtypeCreateStreamRequest :: Newtype CreateStreamRequest _


newtype CreateStreamResponse = CreateStreamResponse 
  { "StreamId'" :: NullOrUndefined (StreamId)
  , "StreamArn'" :: NullOrUndefined (StreamArn)
  , "Description'" :: NullOrUndefined (StreamDescription)
  , "StreamVersion'" :: NullOrUndefined (StreamVersion)
  }
derive instance newtypeCreateStreamResponse :: Newtype CreateStreamResponse _


newtype CreateThingGroupRequest = CreateThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  , "ParentGroupName'" :: NullOrUndefined (ThingGroupName)
  , "ThingGroupProperties'" :: NullOrUndefined (ThingGroupProperties)
  }
derive instance newtypeCreateThingGroupRequest :: Newtype CreateThingGroupRequest _


newtype CreateThingGroupResponse = CreateThingGroupResponse 
  { "ThingGroupName'" :: NullOrUndefined (ThingGroupName)
  , "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn)
  , "ThingGroupId'" :: NullOrUndefined (ThingGroupId)
  }
derive instance newtypeCreateThingGroupResponse :: Newtype CreateThingGroupResponse _


-- | <p>The input for the CreateThing operation.</p>
newtype CreateThingRequest = CreateThingRequest 
  { "ThingName'" :: (ThingName)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "AttributePayload'" :: NullOrUndefined (AttributePayload)
  }
derive instance newtypeCreateThingRequest :: Newtype CreateThingRequest _


-- | <p>The output of the CreateThing operation.</p>
newtype CreateThingResponse = CreateThingResponse 
  { "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingArn'" :: NullOrUndefined (ThingArn)
  , "ThingId'" :: NullOrUndefined (ThingId)
  }
derive instance newtypeCreateThingResponse :: Newtype CreateThingResponse _


-- | <p>The input for the CreateThingType operation.</p>
newtype CreateThingTypeRequest = CreateThingTypeRequest 
  { "ThingTypeName'" :: (ThingTypeName)
  , "ThingTypeProperties'" :: NullOrUndefined (ThingTypeProperties)
  }
derive instance newtypeCreateThingTypeRequest :: Newtype CreateThingTypeRequest _


-- | <p>The output of the CreateThingType operation.</p>
newtype CreateThingTypeResponse = CreateThingTypeResponse 
  { "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "ThingTypeArn'" :: NullOrUndefined (ThingTypeArn)
  , "ThingTypeId'" :: NullOrUndefined (ThingTypeId)
  }
derive instance newtypeCreateThingTypeResponse :: Newtype CreateThingTypeResponse _


-- | <p>The input for the CreateTopicRule operation.</p>
newtype CreateTopicRuleRequest = CreateTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  , "TopicRulePayload'" :: (TopicRulePayload)
  }
derive instance newtypeCreateTopicRuleRequest :: Newtype CreateTopicRuleRequest _


newtype CreatedAtDate = CreatedAtDate Number
derive instance newtypeCreatedAtDate :: Newtype CreatedAtDate _


newtype CreationDate = CreationDate Number
derive instance newtypeCreationDate :: Newtype CreationDate _


newtype CredentialDurationSeconds = CredentialDurationSeconds Int
derive instance newtypeCredentialDurationSeconds :: Newtype CredentialDurationSeconds _


-- | <p>Describes a custom method used to code sign a file.</p>
newtype CustomCodeSigning = CustomCodeSigning 
  { "Signature'" :: NullOrUndefined (CodeSigningSignature)
  , "CertificateChain'" :: NullOrUndefined (CodeSigningCertificateChain)
  , "HashAlgorithm'" :: NullOrUndefined (HashAlgorithm)
  , "SignatureAlgorithm'" :: NullOrUndefined (SignatureAlgorithm)
  }
derive instance newtypeCustomCodeSigning :: Newtype CustomCodeSigning _


newtype DateType = DateType Number
derive instance newtypeDateType :: Newtype DateType _


newtype DeleteAuthorizerRequest = DeleteAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  }
derive instance newtypeDeleteAuthorizerRequest :: Newtype DeleteAuthorizerRequest _


newtype DeleteAuthorizerResponse = DeleteAuthorizerResponse 
  { 
  }
derive instance newtypeDeleteAuthorizerResponse :: Newtype DeleteAuthorizerResponse _


-- | <p>Input for the DeleteCACertificate operation.</p>
newtype DeleteCACertificateRequest = DeleteCACertificateRequest 
  { "CertificateId'" :: (CertificateId)
  }
derive instance newtypeDeleteCACertificateRequest :: Newtype DeleteCACertificateRequest _


-- | <p>The output for the DeleteCACertificate operation.</p>
newtype DeleteCACertificateResponse = DeleteCACertificateResponse 
  { 
  }
derive instance newtypeDeleteCACertificateResponse :: Newtype DeleteCACertificateResponse _


-- | <p>The input for the DeleteCertificate operation.</p>
newtype DeleteCertificateRequest = DeleteCertificateRequest 
  { "CertificateId'" :: (CertificateId)
  , "ForceDelete'" :: NullOrUndefined (ForceDelete)
  }
derive instance newtypeDeleteCertificateRequest :: Newtype DeleteCertificateRequest _


-- | <p>You can't delete the resource because it is attached to one or more resources.</p>
newtype DeleteConflictException = DeleteConflictException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeDeleteConflictException :: Newtype DeleteConflictException _


newtype DeleteOTAUpdateRequest = DeleteOTAUpdateRequest 
  { "OtaUpdateId'" :: (OTAUpdateId)
  }
derive instance newtypeDeleteOTAUpdateRequest :: Newtype DeleteOTAUpdateRequest _


newtype DeleteOTAUpdateResponse = DeleteOTAUpdateResponse 
  { 
  }
derive instance newtypeDeleteOTAUpdateResponse :: Newtype DeleteOTAUpdateResponse _


-- | <p>The input for the DeletePolicy operation.</p>
newtype DeletePolicyRequest = DeletePolicyRequest 
  { "PolicyName'" :: (PolicyName)
  }
derive instance newtypeDeletePolicyRequest :: Newtype DeletePolicyRequest _


-- | <p>The input for the DeletePolicyVersion operation.</p>
newtype DeletePolicyVersionRequest = DeletePolicyVersionRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyVersionId'" :: (PolicyVersionId)
  }
derive instance newtypeDeletePolicyVersionRequest :: Newtype DeletePolicyVersionRequest _


-- | <p>The input for the DeleteRegistrationCode operation.</p>
newtype DeleteRegistrationCodeRequest = DeleteRegistrationCodeRequest 
  { 
  }
derive instance newtypeDeleteRegistrationCodeRequest :: Newtype DeleteRegistrationCodeRequest _


-- | <p>The output for the DeleteRegistrationCode operation.</p>
newtype DeleteRegistrationCodeResponse = DeleteRegistrationCodeResponse 
  { 
  }
derive instance newtypeDeleteRegistrationCodeResponse :: Newtype DeleteRegistrationCodeResponse _


newtype DeleteRoleAliasRequest = DeleteRoleAliasRequest 
  { "RoleAlias'" :: (RoleAlias)
  }
derive instance newtypeDeleteRoleAliasRequest :: Newtype DeleteRoleAliasRequest _


newtype DeleteRoleAliasResponse = DeleteRoleAliasResponse 
  { 
  }
derive instance newtypeDeleteRoleAliasResponse :: Newtype DeleteRoleAliasResponse _


newtype DeleteStreamRequest = DeleteStreamRequest 
  { "StreamId'" :: (StreamId)
  }
derive instance newtypeDeleteStreamRequest :: Newtype DeleteStreamRequest _


newtype DeleteStreamResponse = DeleteStreamResponse 
  { 
  }
derive instance newtypeDeleteStreamResponse :: Newtype DeleteStreamResponse _


newtype DeleteThingGroupRequest = DeleteThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  , "ExpectedVersion'" :: NullOrUndefined (OptionalVersion)
  }
derive instance newtypeDeleteThingGroupRequest :: Newtype DeleteThingGroupRequest _


newtype DeleteThingGroupResponse = DeleteThingGroupResponse 
  { 
  }
derive instance newtypeDeleteThingGroupResponse :: Newtype DeleteThingGroupResponse _


-- | <p>The input for the DeleteThing operation.</p>
newtype DeleteThingRequest = DeleteThingRequest 
  { "ThingName'" :: (ThingName)
  , "ExpectedVersion'" :: NullOrUndefined (OptionalVersion)
  }
derive instance newtypeDeleteThingRequest :: Newtype DeleteThingRequest _


-- | <p>The output of the DeleteThing operation.</p>
newtype DeleteThingResponse = DeleteThingResponse 
  { 
  }
derive instance newtypeDeleteThingResponse :: Newtype DeleteThingResponse _


-- | <p>The input for the DeleteThingType operation.</p>
newtype DeleteThingTypeRequest = DeleteThingTypeRequest 
  { "ThingTypeName'" :: (ThingTypeName)
  }
derive instance newtypeDeleteThingTypeRequest :: Newtype DeleteThingTypeRequest _


-- | <p>The output for the DeleteThingType operation.</p>
newtype DeleteThingTypeResponse = DeleteThingTypeResponse 
  { 
  }
derive instance newtypeDeleteThingTypeResponse :: Newtype DeleteThingTypeResponse _


-- | <p>The input for the DeleteTopicRule operation.</p>
newtype DeleteTopicRuleRequest = DeleteTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  }
derive instance newtypeDeleteTopicRuleRequest :: Newtype DeleteTopicRuleRequest _


newtype DeleteV2LoggingLevelRequest = DeleteV2LoggingLevelRequest 
  { "TargetType'" :: (LogTargetType)
  , "TargetName'" :: (LogTargetName)
  }
derive instance newtypeDeleteV2LoggingLevelRequest :: Newtype DeleteV2LoggingLevelRequest _


newtype DeliveryStreamName = DeliveryStreamName String
derive instance newtypeDeliveryStreamName :: Newtype DeliveryStreamName _


-- | <p>Contains information that denied the authorization.</p>
newtype Denied = Denied 
  { "ImplicitDeny'" :: NullOrUndefined (ImplicitDeny)
  , "ExplicitDeny'" :: NullOrUndefined (ExplicitDeny)
  }
derive instance newtypeDenied :: Newtype Denied _


-- | <p>The input for the DeprecateThingType operation.</p>
newtype DeprecateThingTypeRequest = DeprecateThingTypeRequest 
  { "ThingTypeName'" :: (ThingTypeName)
  , "UndoDeprecate'" :: NullOrUndefined (UndoDeprecate)
  }
derive instance newtypeDeprecateThingTypeRequest :: Newtype DeprecateThingTypeRequest _


-- | <p>The output for the DeprecateThingType operation.</p>
newtype DeprecateThingTypeResponse = DeprecateThingTypeResponse 
  { 
  }
derive instance newtypeDeprecateThingTypeResponse :: Newtype DeprecateThingTypeResponse _


newtype DeprecationDate = DeprecationDate Number
derive instance newtypeDeprecationDate :: Newtype DeprecationDate _


newtype DescribeAuthorizerRequest = DescribeAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  }
derive instance newtypeDescribeAuthorizerRequest :: Newtype DescribeAuthorizerRequest _


newtype DescribeAuthorizerResponse = DescribeAuthorizerResponse 
  { "AuthorizerDescription'" :: NullOrUndefined (AuthorizerDescription)
  }
derive instance newtypeDescribeAuthorizerResponse :: Newtype DescribeAuthorizerResponse _


-- | <p>The input for the DescribeCACertificate operation.</p>
newtype DescribeCACertificateRequest = DescribeCACertificateRequest 
  { "CertificateId'" :: (CertificateId)
  }
derive instance newtypeDescribeCACertificateRequest :: Newtype DescribeCACertificateRequest _


-- | <p>The output from the DescribeCACertificate operation.</p>
newtype DescribeCACertificateResponse = DescribeCACertificateResponse 
  { "CertificateDescription'" :: NullOrUndefined (CACertificateDescription)
  , "RegistrationConfig'" :: NullOrUndefined (RegistrationConfig)
  }
derive instance newtypeDescribeCACertificateResponse :: Newtype DescribeCACertificateResponse _


-- | <p>The input for the DescribeCertificate operation.</p>
newtype DescribeCertificateRequest = DescribeCertificateRequest 
  { "CertificateId'" :: (CertificateId)
  }
derive instance newtypeDescribeCertificateRequest :: Newtype DescribeCertificateRequest _


-- | <p>The output of the DescribeCertificate operation.</p>
newtype DescribeCertificateResponse = DescribeCertificateResponse 
  { "CertificateDescription'" :: NullOrUndefined (CertificateDescription)
  }
derive instance newtypeDescribeCertificateResponse :: Newtype DescribeCertificateResponse _


newtype DescribeDefaultAuthorizerRequest = DescribeDefaultAuthorizerRequest 
  { 
  }
derive instance newtypeDescribeDefaultAuthorizerRequest :: Newtype DescribeDefaultAuthorizerRequest _


newtype DescribeDefaultAuthorizerResponse = DescribeDefaultAuthorizerResponse 
  { "AuthorizerDescription'" :: NullOrUndefined (AuthorizerDescription)
  }
derive instance newtypeDescribeDefaultAuthorizerResponse :: Newtype DescribeDefaultAuthorizerResponse _


-- | <p>The input for the DescribeEndpoint operation.</p>
newtype DescribeEndpointRequest = DescribeEndpointRequest 
  { "EndpointType'" :: NullOrUndefined (EndpointType)
  }
derive instance newtypeDescribeEndpointRequest :: Newtype DescribeEndpointRequest _


-- | <p>The output from the DescribeEndpoint operation.</p>
newtype DescribeEndpointResponse = DescribeEndpointResponse 
  { "EndpointAddress'" :: NullOrUndefined (EndpointAddress)
  }
derive instance newtypeDescribeEndpointResponse :: Newtype DescribeEndpointResponse _


newtype DescribeEventConfigurationsRequest = DescribeEventConfigurationsRequest 
  { 
  }
derive instance newtypeDescribeEventConfigurationsRequest :: Newtype DescribeEventConfigurationsRequest _


newtype DescribeEventConfigurationsResponse = DescribeEventConfigurationsResponse 
  { "EventConfigurations'" :: NullOrUndefined (EventConfigurations)
  , "CreationDate'" :: NullOrUndefined (CreationDate)
  , "LastModifiedDate'" :: NullOrUndefined (LastModifiedDate)
  }
derive instance newtypeDescribeEventConfigurationsResponse :: Newtype DescribeEventConfigurationsResponse _


newtype DescribeIndexRequest = DescribeIndexRequest 
  { "IndexName'" :: (IndexName)
  }
derive instance newtypeDescribeIndexRequest :: Newtype DescribeIndexRequest _


newtype DescribeIndexResponse = DescribeIndexResponse 
  { "IndexName'" :: NullOrUndefined (IndexName)
  , "IndexStatus'" :: NullOrUndefined (IndexStatus)
  , "Schema'" :: NullOrUndefined (IndexSchema)
  }
derive instance newtypeDescribeIndexResponse :: Newtype DescribeIndexResponse _


newtype DescribeJobExecutionRequest = DescribeJobExecutionRequest 
  { "JobId'" :: (JobId)
  , "ThingName'" :: (ThingName)
  , "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber)
  }
derive instance newtypeDescribeJobExecutionRequest :: Newtype DescribeJobExecutionRequest _


newtype DescribeJobExecutionResponse = DescribeJobExecutionResponse 
  { "Execution'" :: NullOrUndefined (JobExecution)
  }
derive instance newtypeDescribeJobExecutionResponse :: Newtype DescribeJobExecutionResponse _


newtype DescribeJobRequest = DescribeJobRequest 
  { "JobId'" :: (JobId)
  }
derive instance newtypeDescribeJobRequest :: Newtype DescribeJobRequest _


newtype DescribeJobResponse = DescribeJobResponse 
  { "DocumentSource'" :: NullOrUndefined (JobDocumentSource)
  , "Job'" :: NullOrUndefined (Job)
  }
derive instance newtypeDescribeJobResponse :: Newtype DescribeJobResponse _


newtype DescribeRoleAliasRequest = DescribeRoleAliasRequest 
  { "RoleAlias'" :: (RoleAlias)
  }
derive instance newtypeDescribeRoleAliasRequest :: Newtype DescribeRoleAliasRequest _


newtype DescribeRoleAliasResponse = DescribeRoleAliasResponse 
  { "RoleAliasDescription'" :: NullOrUndefined (RoleAliasDescription)
  }
derive instance newtypeDescribeRoleAliasResponse :: Newtype DescribeRoleAliasResponse _


newtype DescribeStreamRequest = DescribeStreamRequest 
  { "StreamId'" :: (StreamId)
  }
derive instance newtypeDescribeStreamRequest :: Newtype DescribeStreamRequest _


newtype DescribeStreamResponse = DescribeStreamResponse 
  { "StreamInfo'" :: NullOrUndefined (StreamInfo)
  }
derive instance newtypeDescribeStreamResponse :: Newtype DescribeStreamResponse _


newtype DescribeThingGroupRequest = DescribeThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  }
derive instance newtypeDescribeThingGroupRequest :: Newtype DescribeThingGroupRequest _


newtype DescribeThingGroupResponse = DescribeThingGroupResponse 
  { "ThingGroupName'" :: NullOrUndefined (ThingGroupName)
  , "ThingGroupId'" :: NullOrUndefined (ThingGroupId)
  , "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn)
  , "Version'" :: NullOrUndefined (Version)
  , "ThingGroupProperties'" :: NullOrUndefined (ThingGroupProperties)
  , "ThingGroupMetadata'" :: NullOrUndefined (ThingGroupMetadata)
  }
derive instance newtypeDescribeThingGroupResponse :: Newtype DescribeThingGroupResponse _


newtype DescribeThingRegistrationTaskRequest = DescribeThingRegistrationTaskRequest 
  { "TaskId'" :: (TaskId)
  }
derive instance newtypeDescribeThingRegistrationTaskRequest :: Newtype DescribeThingRegistrationTaskRequest _


newtype DescribeThingRegistrationTaskResponse = DescribeThingRegistrationTaskResponse 
  { "TaskId'" :: NullOrUndefined (TaskId)
  , "CreationDate'" :: NullOrUndefined (CreationDate)
  , "LastModifiedDate'" :: NullOrUndefined (LastModifiedDate)
  , "TemplateBody'" :: NullOrUndefined (TemplateBody)
  , "InputFileBucket'" :: NullOrUndefined (RegistryS3BucketName)
  , "InputFileKey'" :: NullOrUndefined (RegistryS3KeyName)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  , "Status'" :: NullOrUndefined (Status)
  , "Message'" :: NullOrUndefined (ErrorMessage)
  , "SuccessCount'" :: NullOrUndefined (Count)
  , "FailureCount'" :: NullOrUndefined (Count)
  , "PercentageProgress'" :: NullOrUndefined (Percentage)
  }
derive instance newtypeDescribeThingRegistrationTaskResponse :: Newtype DescribeThingRegistrationTaskResponse _


-- | <p>The input for the DescribeThing operation.</p>
newtype DescribeThingRequest = DescribeThingRequest 
  { "ThingName'" :: (ThingName)
  }
derive instance newtypeDescribeThingRequest :: Newtype DescribeThingRequest _


-- | <p>The output from the DescribeThing operation.</p>
newtype DescribeThingResponse = DescribeThingResponse 
  { "DefaultClientId'" :: NullOrUndefined (ClientId)
  , "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingId'" :: NullOrUndefined (ThingId)
  , "ThingArn'" :: NullOrUndefined (ThingArn)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "Attributes'" :: NullOrUndefined (Attributes)
  , "Version'" :: NullOrUndefined (Version)
  }
derive instance newtypeDescribeThingResponse :: Newtype DescribeThingResponse _


-- | <p>The input for the DescribeThingType operation.</p>
newtype DescribeThingTypeRequest = DescribeThingTypeRequest 
  { "ThingTypeName'" :: (ThingTypeName)
  }
derive instance newtypeDescribeThingTypeRequest :: Newtype DescribeThingTypeRequest _


-- | <p>The output for the DescribeThingType operation.</p>
newtype DescribeThingTypeResponse = DescribeThingTypeResponse 
  { "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "ThingTypeId'" :: NullOrUndefined (ThingTypeId)
  , "ThingTypeArn'" :: NullOrUndefined (ThingTypeArn)
  , "ThingTypeProperties'" :: NullOrUndefined (ThingTypeProperties)
  , "ThingTypeMetadata'" :: NullOrUndefined (ThingTypeMetadata)
  }
derive instance newtypeDescribeThingTypeResponse :: Newtype DescribeThingTypeResponse _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


newtype DetachPolicyRequest = DetachPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Target'" :: (PolicyTarget)
  }
derive instance newtypeDetachPolicyRequest :: Newtype DetachPolicyRequest _


-- | <p>The input for the DetachPrincipalPolicy operation.</p>
newtype DetachPrincipalPolicyRequest = DetachPrincipalPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Principal'" :: (Principal)
  }
derive instance newtypeDetachPrincipalPolicyRequest :: Newtype DetachPrincipalPolicyRequest _


-- | <p>The input for the DetachThingPrincipal operation.</p>
newtype DetachThingPrincipalRequest = DetachThingPrincipalRequest 
  { "ThingName'" :: (ThingName)
  , "Principal'" :: (Principal)
  }
derive instance newtypeDetachThingPrincipalRequest :: Newtype DetachThingPrincipalRequest _


-- | <p>The output from the DetachThingPrincipal operation.</p>
newtype DetachThingPrincipalResponse = DetachThingPrincipalResponse 
  { 
  }
derive instance newtypeDetachThingPrincipalResponse :: Newtype DetachThingPrincipalResponse _


newtype DetailsKey = DetailsKey String
derive instance newtypeDetailsKey :: Newtype DetailsKey _


newtype DetailsMap = DetailsMap (Map DetailsKey DetailsValue)
derive instance newtypeDetailsMap :: Newtype DetailsMap _


newtype DetailsValue = DetailsValue String
derive instance newtypeDetailsValue :: Newtype DetailsValue _


newtype DisableAllLogs = DisableAllLogs Boolean
derive instance newtypeDisableAllLogs :: Newtype DisableAllLogs _


-- | <p>The input for the DisableTopicRuleRequest operation.</p>
newtype DisableTopicRuleRequest = DisableTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  }
derive instance newtypeDisableTopicRuleRequest :: Newtype DisableTopicRuleRequest _


-- | <p>Describes an action to write to a DynamoDB table.</p> <p>The <code>tableName</code>, <code>hashKeyField</code>, and <code>rangeKeyField</code> values must match the values used when you created the table.</p> <p>The <code>hashKeyValue</code> and <code>rangeKeyvalue</code> fields use a substitution template syntax. These templates provide data at runtime. The syntax is as follows: ${<i>sql-expression</i>}.</p> <p>You can specify any valid expression in a WHERE or SELECT clause, including JSON properties, comparisons, calculations, and functions. For example, the following field uses the third level of the topic:</p> <p> <code>"hashKeyValue": "${topic(3)}"</code> </p> <p>The following field uses the timestamp:</p> <p> <code>"rangeKeyValue": "${timestamp()}"</code> </p>
newtype DynamoDBAction = DynamoDBAction 
  { "TableName'" :: (TableName)
  , "RoleArn'" :: (AwsArn)
  , "Operation'" :: NullOrUndefined (DynamoOperation)
  , "HashKeyField'" :: (HashKeyField)
  , "HashKeyValue'" :: (HashKeyValue)
  , "HashKeyType'" :: NullOrUndefined (DynamoKeyType)
  , "RangeKeyField'" :: NullOrUndefined (RangeKeyField)
  , "RangeKeyValue'" :: NullOrUndefined (RangeKeyValue)
  , "RangeKeyType'" :: NullOrUndefined (DynamoKeyType)
  , "PayloadField'" :: NullOrUndefined (PayloadField)
  }
derive instance newtypeDynamoDBAction :: Newtype DynamoDBAction _


-- | <p>Describes an action to write to a DynamoDB table.</p> <p>This DynamoDB action writes each attribute in the message payload into it's own column in the DynamoDB table.</p>
newtype DynamoDBv2Action = DynamoDBv2Action 
  { "RoleArn'" :: NullOrUndefined (AwsArn)
  , "PutItem'" :: NullOrUndefined (PutItemInput)
  }
derive instance newtypeDynamoDBv2Action :: Newtype DynamoDBv2Action _


newtype DynamoKeyType = DynamoKeyType String
derive instance newtypeDynamoKeyType :: Newtype DynamoKeyType _


newtype DynamoOperation = DynamoOperation String
derive instance newtypeDynamoOperation :: Newtype DynamoOperation _


newtype EffectivePolicies = EffectivePolicies (Array EffectivePolicy)
derive instance newtypeEffectivePolicies :: Newtype EffectivePolicies _


-- | <p>The policy that has the effect on the authorization results.</p>
newtype EffectivePolicy = EffectivePolicy 
  { "PolicyName'" :: NullOrUndefined (PolicyName)
  , "PolicyArn'" :: NullOrUndefined (PolicyArn)
  , "PolicyDocument'" :: NullOrUndefined (PolicyDocument)
  }
derive instance newtypeEffectivePolicy :: Newtype EffectivePolicy _


-- | <p>Describes an action that writes data to an Amazon Elasticsearch Service domain.</p>
newtype ElasticsearchAction = ElasticsearchAction 
  { "RoleArn'" :: (AwsArn)
  , "Endpoint'" :: (ElasticsearchEndpoint)
  , "Index'" :: (ElasticsearchIndex)
  , "Type'" :: (ElasticsearchType)
  , "Id'" :: (ElasticsearchId)
  }
derive instance newtypeElasticsearchAction :: Newtype ElasticsearchAction _


newtype ElasticsearchEndpoint = ElasticsearchEndpoint String
derive instance newtypeElasticsearchEndpoint :: Newtype ElasticsearchEndpoint _


newtype ElasticsearchId = ElasticsearchId String
derive instance newtypeElasticsearchId :: Newtype ElasticsearchId _


newtype ElasticsearchIndex = ElasticsearchIndex String
derive instance newtypeElasticsearchIndex :: Newtype ElasticsearchIndex _


newtype ElasticsearchType = ElasticsearchType String
derive instance newtypeElasticsearchType :: Newtype ElasticsearchType _


-- | <p>The input for the EnableTopicRuleRequest operation.</p>
newtype EnableTopicRuleRequest = EnableTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  }
derive instance newtypeEnableTopicRuleRequest :: Newtype EnableTopicRuleRequest _


newtype Enabled = Enabled Boolean
derive instance newtypeEnabled :: Newtype Enabled _


newtype EndpointAddress = EndpointAddress String
derive instance newtypeEndpointAddress :: Newtype EndpointAddress _


newtype EndpointType = EndpointType String
derive instance newtypeEndpointType :: Newtype EndpointType _


-- | <p>Error information.</p>
newtype ErrorInfo = ErrorInfo 
  { "Code'" :: NullOrUndefined (Code)
  , "Message'" :: NullOrUndefined (OTAUpdateErrorMessage)
  }
derive instance newtypeErrorInfo :: Newtype ErrorInfo _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype EventConfigurations = EventConfigurations (Map EventType Configuration)
derive instance newtypeEventConfigurations :: Newtype EventConfigurations _


newtype EventType = EventType String
derive instance newtypeEventType :: Newtype EventType _


newtype ExecutionNumber = ExecutionNumber Number
derive instance newtypeExecutionNumber :: Newtype ExecutionNumber _


newtype ExpiresInSec = ExpiresInSec Number
derive instance newtypeExpiresInSec :: Newtype ExpiresInSec _


-- | <p>Information that explicitly denies authorization.</p>
newtype ExplicitDeny = ExplicitDeny 
  { "Policies'" :: NullOrUndefined (Policies)
  }
derive instance newtypeExplicitDeny :: Newtype ExplicitDeny _


newtype FailedThings = FailedThings Int
derive instance newtypeFailedThings :: Newtype FailedThings _


newtype FileId = FileId Int
derive instance newtypeFileId :: Newtype FileId _


newtype FileName = FileName String
derive instance newtypeFileName :: Newtype FileName _


-- | <p>Describes an action that writes data to an Amazon Kinesis Firehose stream.</p>
newtype FirehoseAction = FirehoseAction 
  { "RoleArn'" :: (AwsArn)
  , "DeliveryStreamName'" :: (DeliveryStreamName)
  , "Separator'" :: NullOrUndefined (FirehoseSeparator)
  }
derive instance newtypeFirehoseAction :: Newtype FirehoseAction _


newtype FirehoseSeparator = FirehoseSeparator String
derive instance newtypeFirehoseSeparator :: Newtype FirehoseSeparator _


newtype Flag = Flag Boolean
derive instance newtypeFlag :: Newtype Flag _


newtype ForceDelete = ForceDelete Boolean
derive instance newtypeForceDelete :: Newtype ForceDelete _


newtype FunctionArn = FunctionArn String
derive instance newtypeFunctionArn :: Newtype FunctionArn _


newtype GEMaxResults = GEMaxResults Int
derive instance newtypeGEMaxResults :: Newtype GEMaxResults _


newtype GetEffectivePoliciesRequest = GetEffectivePoliciesRequest 
  { "Principal'" :: NullOrUndefined (Principal)
  , "CognitoIdentityPoolId'" :: NullOrUndefined (CognitoIdentityPoolId)
  , "ThingName'" :: NullOrUndefined (ThingName)
  }
derive instance newtypeGetEffectivePoliciesRequest :: Newtype GetEffectivePoliciesRequest _


newtype GetEffectivePoliciesResponse = GetEffectivePoliciesResponse 
  { "EffectivePolicies'" :: NullOrUndefined (EffectivePolicies)
  }
derive instance newtypeGetEffectivePoliciesResponse :: Newtype GetEffectivePoliciesResponse _


newtype GetIndexingConfigurationRequest = GetIndexingConfigurationRequest 
  { 
  }
derive instance newtypeGetIndexingConfigurationRequest :: Newtype GetIndexingConfigurationRequest _


newtype GetIndexingConfigurationResponse = GetIndexingConfigurationResponse 
  { "ThingIndexingConfiguration'" :: NullOrUndefined (ThingIndexingConfiguration)
  }
derive instance newtypeGetIndexingConfigurationResponse :: Newtype GetIndexingConfigurationResponse _


newtype GetJobDocumentRequest = GetJobDocumentRequest 
  { "JobId'" :: (JobId)
  }
derive instance newtypeGetJobDocumentRequest :: Newtype GetJobDocumentRequest _


newtype GetJobDocumentResponse = GetJobDocumentResponse 
  { "Document'" :: NullOrUndefined (JobDocument)
  }
derive instance newtypeGetJobDocumentResponse :: Newtype GetJobDocumentResponse _


-- | <p>The input for the GetLoggingOptions operation.</p>
newtype GetLoggingOptionsRequest = GetLoggingOptionsRequest 
  { 
  }
derive instance newtypeGetLoggingOptionsRequest :: Newtype GetLoggingOptionsRequest _


-- | <p>The output from the GetLoggingOptions operation.</p>
newtype GetLoggingOptionsResponse = GetLoggingOptionsResponse 
  { "RoleArn'" :: NullOrUndefined (AwsArn)
  , "LogLevel'" :: NullOrUndefined (LogLevel)
  }
derive instance newtypeGetLoggingOptionsResponse :: Newtype GetLoggingOptionsResponse _


newtype GetOTAUpdateRequest = GetOTAUpdateRequest 
  { "OtaUpdateId'" :: (OTAUpdateId)
  }
derive instance newtypeGetOTAUpdateRequest :: Newtype GetOTAUpdateRequest _


newtype GetOTAUpdateResponse = GetOTAUpdateResponse 
  { "OtaUpdateInfo'" :: NullOrUndefined (OTAUpdateInfo)
  }
derive instance newtypeGetOTAUpdateResponse :: Newtype GetOTAUpdateResponse _


-- | <p>The input for the GetPolicy operation.</p>
newtype GetPolicyRequest = GetPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  }
derive instance newtypeGetPolicyRequest :: Newtype GetPolicyRequest _


-- | <p>The output from the GetPolicy operation.</p>
newtype GetPolicyResponse = GetPolicyResponse 
  { "PolicyName'" :: NullOrUndefined (PolicyName)
  , "PolicyArn'" :: NullOrUndefined (PolicyArn)
  , "PolicyDocument'" :: NullOrUndefined (PolicyDocument)
  , "DefaultVersionId'" :: NullOrUndefined (PolicyVersionId)
  }
derive instance newtypeGetPolicyResponse :: Newtype GetPolicyResponse _


-- | <p>The input for the GetPolicyVersion operation.</p>
newtype GetPolicyVersionRequest = GetPolicyVersionRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyVersionId'" :: (PolicyVersionId)
  }
derive instance newtypeGetPolicyVersionRequest :: Newtype GetPolicyVersionRequest _


-- | <p>The output from the GetPolicyVersion operation.</p>
newtype GetPolicyVersionResponse = GetPolicyVersionResponse 
  { "PolicyArn'" :: NullOrUndefined (PolicyArn)
  , "PolicyName'" :: NullOrUndefined (PolicyName)
  , "PolicyDocument'" :: NullOrUndefined (PolicyDocument)
  , "PolicyVersionId'" :: NullOrUndefined (PolicyVersionId)
  , "IsDefaultVersion'" :: NullOrUndefined (IsDefaultVersion)
  }
derive instance newtypeGetPolicyVersionResponse :: Newtype GetPolicyVersionResponse _


-- | <p>The input to the GetRegistrationCode operation.</p>
newtype GetRegistrationCodeRequest = GetRegistrationCodeRequest 
  { 
  }
derive instance newtypeGetRegistrationCodeRequest :: Newtype GetRegistrationCodeRequest _


-- | <p>The output from the GetRegistrationCode operation.</p>
newtype GetRegistrationCodeResponse = GetRegistrationCodeResponse 
  { "RegistrationCode'" :: NullOrUndefined (RegistrationCode)
  }
derive instance newtypeGetRegistrationCodeResponse :: Newtype GetRegistrationCodeResponse _


-- | <p>The input for the GetTopicRule operation.</p>
newtype GetTopicRuleRequest = GetTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  }
derive instance newtypeGetTopicRuleRequest :: Newtype GetTopicRuleRequest _


-- | <p>The output from the GetTopicRule operation.</p>
newtype GetTopicRuleResponse = GetTopicRuleResponse 
  { "RuleArn'" :: NullOrUndefined (RuleArn)
  , "Rule'" :: NullOrUndefined (TopicRule)
  }
derive instance newtypeGetTopicRuleResponse :: Newtype GetTopicRuleResponse _


newtype GetV2LoggingOptionsRequest = GetV2LoggingOptionsRequest 
  { 
  }
derive instance newtypeGetV2LoggingOptionsRequest :: Newtype GetV2LoggingOptionsRequest _


newtype GetV2LoggingOptionsResponse = GetV2LoggingOptionsResponse 
  { "RoleArn'" :: NullOrUndefined (AwsArn)
  , "DefaultLogLevel'" :: NullOrUndefined (LogLevel)
  , "DisableAllLogs'" :: NullOrUndefined (DisableAllLogs)
  }
derive instance newtypeGetV2LoggingOptionsResponse :: Newtype GetV2LoggingOptionsResponse _


-- | <p>The name and ARN of a group.</p>
newtype GroupNameAndArn = GroupNameAndArn 
  { "GroupName'" :: NullOrUndefined (ThingGroupName)
  , "GroupArn'" :: NullOrUndefined (ThingGroupArn)
  }
derive instance newtypeGroupNameAndArn :: Newtype GroupNameAndArn _


newtype HashAlgorithm = HashAlgorithm String
derive instance newtypeHashAlgorithm :: Newtype HashAlgorithm _


newtype HashKeyField = HashKeyField String
derive instance newtypeHashKeyField :: Newtype HashKeyField _


newtype HashKeyValue = HashKeyValue String
derive instance newtypeHashKeyValue :: Newtype HashKeyValue _


-- | <p>Information that implicitly denies authorization. When policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.</p>
newtype ImplicitDeny = ImplicitDeny 
  { "Policies'" :: NullOrUndefined (Policies)
  }
derive instance newtypeImplicitDeny :: Newtype ImplicitDeny _


newtype InProgressThings = InProgressThings Int
derive instance newtypeInProgressThings :: Newtype InProgressThings _


newtype IndexName = IndexName String
derive instance newtypeIndexName :: Newtype IndexName _


newtype IndexNamesList = IndexNamesList (Array IndexName)
derive instance newtypeIndexNamesList :: Newtype IndexNamesList _


-- | <p>The index is not ready.</p>
newtype IndexNotReadyException = IndexNotReadyException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeIndexNotReadyException :: Newtype IndexNotReadyException _


newtype IndexSchema = IndexSchema String
derive instance newtypeIndexSchema :: Newtype IndexSchema _


newtype IndexStatus = IndexStatus String
derive instance newtypeIndexStatus :: Newtype IndexStatus _


newtype InlineDocument = InlineDocument String
derive instance newtypeInlineDocument :: Newtype InlineDocument _


-- | <p>An unexpected error has occurred.</p>
newtype InternalException = InternalException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInternalException :: Newtype InternalException _


-- | <p>An unexpected error has occurred.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInternalFailureException :: Newtype InternalFailureException _


-- | <p>The query is invalid.</p>
newtype InvalidQueryException = InvalidQueryException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidQueryException :: Newtype InvalidQueryException _


-- | <p>The request is not valid.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _


-- | <p>The response is invalid.</p>
newtype InvalidResponseException = InvalidResponseException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidResponseException :: Newtype InvalidResponseException _


newtype IsAuthenticated = IsAuthenticated Boolean
derive instance newtypeIsAuthenticated :: Newtype IsAuthenticated _


newtype IsDefaultVersion = IsDefaultVersion Boolean
derive instance newtypeIsDefaultVersion :: Newtype IsDefaultVersion _


newtype IsDisabled = IsDisabled Boolean
derive instance newtypeIsDisabled :: Newtype IsDisabled _


-- | <p>The <code>Job</code> object contains details about a job.</p>
newtype Job = Job 
  { "JobArn'" :: NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined (JobId)
  , "TargetSelection'" :: NullOrUndefined (TargetSelection)
  , "Status'" :: NullOrUndefined (JobStatus)
  , "Comment'" :: NullOrUndefined (Comment)
  , "Targets'" :: NullOrUndefined (JobTargets)
  , "Description'" :: NullOrUndefined (JobDescription)
  , "PresignedUrlConfig'" :: NullOrUndefined (PresignedUrlConfig)
  , "JobExecutionsRolloutConfig'" :: NullOrUndefined (JobExecutionsRolloutConfig)
  , "CreatedAt'" :: NullOrUndefined (DateType)
  , "LastUpdatedAt'" :: NullOrUndefined (DateType)
  , "CompletedAt'" :: NullOrUndefined (DateType)
  , "JobProcessDetails'" :: NullOrUndefined (JobProcessDetails)
  , "DocumentParameters'" :: NullOrUndefined (JobDocumentParameters)
  }
derive instance newtypeJob :: Newtype Job _


newtype JobArn = JobArn String
derive instance newtypeJobArn :: Newtype JobArn _


newtype JobDescription = JobDescription String
derive instance newtypeJobDescription :: Newtype JobDescription _


newtype JobDocument = JobDocument String
derive instance newtypeJobDocument :: Newtype JobDocument _


newtype JobDocumentParameters = JobDocumentParameters (Map ParameterKey ParameterValue)
derive instance newtypeJobDocumentParameters :: Newtype JobDocumentParameters _


newtype JobDocumentSource = JobDocumentSource String
derive instance newtypeJobDocumentSource :: Newtype JobDocumentSource _


-- | <p>The job execution object represents the execution of a job on a particular device.</p>
newtype JobExecution = JobExecution 
  { "JobId'" :: NullOrUndefined (JobId)
  , "Status'" :: NullOrUndefined (JobExecutionStatus)
  , "StatusDetails'" :: NullOrUndefined (JobExecutionStatusDetails)
  , "ThingArn'" :: NullOrUndefined (ThingArn)
  , "QueuedAt'" :: NullOrUndefined (DateType)
  , "StartedAt'" :: NullOrUndefined (DateType)
  , "LastUpdatedAt'" :: NullOrUndefined (DateType)
  , "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber)
  }
derive instance newtypeJobExecution :: Newtype JobExecution _


newtype JobExecutionStatus = JobExecutionStatus String
derive instance newtypeJobExecutionStatus :: Newtype JobExecutionStatus _


-- | <p>Details of the job execution status.</p>
newtype JobExecutionStatusDetails = JobExecutionStatusDetails 
  { "DetailsMap'" :: NullOrUndefined (DetailsMap)
  }
derive instance newtypeJobExecutionStatusDetails :: Newtype JobExecutionStatusDetails _


-- | <p>The job execution summary.</p>
newtype JobExecutionSummary = JobExecutionSummary 
  { "Status'" :: NullOrUndefined (JobExecutionStatus)
  , "QueuedAt'" :: NullOrUndefined (DateType)
  , "StartedAt'" :: NullOrUndefined (DateType)
  , "LastUpdatedAt'" :: NullOrUndefined (DateType)
  , "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber)
  }
derive instance newtypeJobExecutionSummary :: Newtype JobExecutionSummary _


-- | <p>Contains a summary of information about job executions for a specific job.</p>
newtype JobExecutionSummaryForJob = JobExecutionSummaryForJob 
  { "ThingArn'" :: NullOrUndefined (ThingArn)
  , "JobExecutionSummary'" :: NullOrUndefined (JobExecutionSummary)
  }
derive instance newtypeJobExecutionSummaryForJob :: Newtype JobExecutionSummaryForJob _


newtype JobExecutionSummaryForJobList = JobExecutionSummaryForJobList (Array JobExecutionSummaryForJob)
derive instance newtypeJobExecutionSummaryForJobList :: Newtype JobExecutionSummaryForJobList _


-- | <p>The job execution summary for a thing.</p>
newtype JobExecutionSummaryForThing = JobExecutionSummaryForThing 
  { "JobId'" :: NullOrUndefined (JobId)
  , "JobExecutionSummary'" :: NullOrUndefined (JobExecutionSummary)
  }
derive instance newtypeJobExecutionSummaryForThing :: Newtype JobExecutionSummaryForThing _


newtype JobExecutionSummaryForThingList = JobExecutionSummaryForThingList (Array JobExecutionSummaryForThing)
derive instance newtypeJobExecutionSummaryForThingList :: Newtype JobExecutionSummaryForThingList _


-- | <p>Allows you to create a staged rollout of a job.</p>
newtype JobExecutionsRolloutConfig = JobExecutionsRolloutConfig 
  { "MaximumPerMinute'" :: NullOrUndefined (MaxJobExecutionsPerMin)
  }
derive instance newtypeJobExecutionsRolloutConfig :: Newtype JobExecutionsRolloutConfig _


newtype JobId = JobId String
derive instance newtypeJobId :: Newtype JobId _


-- | <p>The job process details.</p>
newtype JobProcessDetails = JobProcessDetails 
  { "ProcessingTargets'" :: NullOrUndefined (ProcessingTargetNameList)
  , "NumberOfCanceledThings'" :: NullOrUndefined (CanceledThings)
  , "NumberOfSucceededThings'" :: NullOrUndefined (SucceededThings)
  , "NumberOfFailedThings'" :: NullOrUndefined (FailedThings)
  , "NumberOfRejectedThings'" :: NullOrUndefined (RejectedThings)
  , "NumberOfQueuedThings'" :: NullOrUndefined (QueuedThings)
  , "NumberOfInProgressThings'" :: NullOrUndefined (InProgressThings)
  , "NumberOfRemovedThings'" :: NullOrUndefined (RemovedThings)
  }
derive instance newtypeJobProcessDetails :: Newtype JobProcessDetails _


newtype JobStatus = JobStatus String
derive instance newtypeJobStatus :: Newtype JobStatus _


-- | <p>The job summary.</p>
newtype JobSummary = JobSummary 
  { "JobArn'" :: NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined (JobId)
  , "ThingGroupId'" :: NullOrUndefined (ThingGroupId)
  , "TargetSelection'" :: NullOrUndefined (TargetSelection)
  , "Status'" :: NullOrUndefined (JobStatus)
  , "CreatedAt'" :: NullOrUndefined (DateType)
  , "LastUpdatedAt'" :: NullOrUndefined (DateType)
  , "CompletedAt'" :: NullOrUndefined (DateType)
  }
derive instance newtypeJobSummary :: Newtype JobSummary _


newtype JobSummaryList = JobSummaryList (Array JobSummary)
derive instance newtypeJobSummaryList :: Newtype JobSummaryList _


newtype JobTargets = JobTargets (Array TargetArn)
derive instance newtypeJobTargets :: Newtype JobTargets _


newtype JsonDocument = JsonDocument String
derive instance newtypeJsonDocument :: Newtype JsonDocument _


newtype Key = Key String
derive instance newtypeKey :: Newtype Key _


newtype KeyName = KeyName String
derive instance newtypeKeyName :: Newtype KeyName _


-- | <p>Describes a key pair.</p>
newtype KeyPair = KeyPair 
  { "PublicKey" :: NullOrUndefined (PublicKey)
  , "PrivateKey" :: NullOrUndefined (PrivateKey)
  }
derive instance newtypeKeyPair :: Newtype KeyPair _


newtype KeyValue = KeyValue String
derive instance newtypeKeyValue :: Newtype KeyValue _


-- | <p>Describes an action to write data to an Amazon Kinesis stream.</p>
newtype KinesisAction = KinesisAction 
  { "RoleArn'" :: (AwsArn)
  , "StreamName'" :: (StreamName)
  , "PartitionKey'" :: NullOrUndefined (PartitionKey)
  }
derive instance newtypeKinesisAction :: Newtype KinesisAction _


-- | <p>Describes an action to invoke a Lambda function.</p>
newtype LambdaAction = LambdaAction 
  { "FunctionArn'" :: (FunctionArn)
  }
derive instance newtypeLambdaAction :: Newtype LambdaAction _


newtype LaserMaxResults = LaserMaxResults Int
derive instance newtypeLaserMaxResults :: Newtype LaserMaxResults _


newtype LastModifiedDate = LastModifiedDate Number
derive instance newtypeLastModifiedDate :: Newtype LastModifiedDate _


-- | <p>The number of attached entities exceeds the limit.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListAttachedPoliciesRequest = ListAttachedPoliciesRequest 
  { "Target'" :: (PolicyTarget)
  , "Recursive'" :: NullOrUndefined (Recursive)
  , "Marker'" :: NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined (PageSize)
  }
derive instance newtypeListAttachedPoliciesRequest :: Newtype ListAttachedPoliciesRequest _


newtype ListAttachedPoliciesResponse = ListAttachedPoliciesResponse 
  { "Policies'" :: NullOrUndefined (Policies)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }
derive instance newtypeListAttachedPoliciesResponse :: Newtype ListAttachedPoliciesResponse _


newtype ListAuthorizersRequest = ListAuthorizersRequest 
  { "PageSize'" :: NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  , "Status'" :: NullOrUndefined (AuthorizerStatus)
  }
derive instance newtypeListAuthorizersRequest :: Newtype ListAuthorizersRequest _


newtype ListAuthorizersResponse = ListAuthorizersResponse 
  { "Authorizers'" :: NullOrUndefined (Authorizers)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }
derive instance newtypeListAuthorizersResponse :: Newtype ListAuthorizersResponse _


-- | <p>Input for the ListCACertificates operation.</p>
newtype ListCACertificatesRequest = ListCACertificatesRequest 
  { "PageSize'" :: NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListCACertificatesRequest :: Newtype ListCACertificatesRequest _


-- | <p>The output from the ListCACertificates operation.</p>
newtype ListCACertificatesResponse = ListCACertificatesResponse 
  { "Certificates'" :: NullOrUndefined (CACertificates)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }
derive instance newtypeListCACertificatesResponse :: Newtype ListCACertificatesResponse _


-- | <p>The input to the ListCertificatesByCA operation.</p>
newtype ListCertificatesByCARequest = ListCertificatesByCARequest 
  { "CaCertificateId'" :: (CertificateId)
  , "PageSize'" :: NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListCertificatesByCARequest :: Newtype ListCertificatesByCARequest _


-- | <p>The output of the ListCertificatesByCA operation.</p>
newtype ListCertificatesByCAResponse = ListCertificatesByCAResponse 
  { "Certificates'" :: NullOrUndefined (Certificates)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }
derive instance newtypeListCertificatesByCAResponse :: Newtype ListCertificatesByCAResponse _


-- | <p>The input for the ListCertificates operation.</p>
newtype ListCertificatesRequest = ListCertificatesRequest 
  { "PageSize'" :: NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListCertificatesRequest :: Newtype ListCertificatesRequest _


-- | <p>The output of the ListCertificates operation.</p>
newtype ListCertificatesResponse = ListCertificatesResponse 
  { "Certificates'" :: NullOrUndefined (Certificates)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }
derive instance newtypeListCertificatesResponse :: Newtype ListCertificatesResponse _


newtype ListIndicesRequest = ListIndicesRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (QueryMaxResults)
  }
derive instance newtypeListIndicesRequest :: Newtype ListIndicesRequest _


newtype ListIndicesResponse = ListIndicesResponse 
  { "IndexNames'" :: NullOrUndefined (IndexNamesList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListIndicesResponse :: Newtype ListIndicesResponse _


newtype ListJobExecutionsForJobRequest = ListJobExecutionsForJobRequest 
  { "JobId'" :: (JobId)
  , "Status'" :: NullOrUndefined (JobExecutionStatus)
  , "MaxResults'" :: NullOrUndefined (LaserMaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListJobExecutionsForJobRequest :: Newtype ListJobExecutionsForJobRequest _


newtype ListJobExecutionsForJobResponse = ListJobExecutionsForJobResponse 
  { "ExecutionSummaries'" :: NullOrUndefined (JobExecutionSummaryForJobList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListJobExecutionsForJobResponse :: Newtype ListJobExecutionsForJobResponse _


newtype ListJobExecutionsForThingRequest = ListJobExecutionsForThingRequest 
  { "ThingName'" :: (ThingName)
  , "Status'" :: NullOrUndefined (JobExecutionStatus)
  , "MaxResults'" :: NullOrUndefined (LaserMaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListJobExecutionsForThingRequest :: Newtype ListJobExecutionsForThingRequest _


newtype ListJobExecutionsForThingResponse = ListJobExecutionsForThingResponse 
  { "ExecutionSummaries'" :: NullOrUndefined (JobExecutionSummaryForThingList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListJobExecutionsForThingResponse :: Newtype ListJobExecutionsForThingResponse _


newtype ListJobsRequest = ListJobsRequest 
  { "Status'" :: NullOrUndefined (JobStatus)
  , "TargetSelection'" :: NullOrUndefined (TargetSelection)
  , "MaxResults'" :: NullOrUndefined (LaserMaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "ThingGroupName'" :: NullOrUndefined (ThingGroupName)
  , "ThingGroupId'" :: NullOrUndefined (ThingGroupId)
  }
derive instance newtypeListJobsRequest :: Newtype ListJobsRequest _


newtype ListJobsResponse = ListJobsResponse 
  { "Jobs'" :: NullOrUndefined (JobSummaryList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListJobsResponse :: Newtype ListJobsResponse _


newtype ListOTAUpdatesRequest = ListOTAUpdatesRequest 
  { "MaxResults'" :: NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "OtaUpdateStatus'" :: NullOrUndefined (OTAUpdateStatus)
  }
derive instance newtypeListOTAUpdatesRequest :: Newtype ListOTAUpdatesRequest _


newtype ListOTAUpdatesResponse = ListOTAUpdatesResponse 
  { "OtaUpdates'" :: NullOrUndefined (OTAUpdatesSummary)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListOTAUpdatesResponse :: Newtype ListOTAUpdatesResponse _


-- | <p>The input to the ListOutgoingCertificates operation.</p>
newtype ListOutgoingCertificatesRequest = ListOutgoingCertificatesRequest 
  { "PageSize'" :: NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListOutgoingCertificatesRequest :: Newtype ListOutgoingCertificatesRequest _


-- | <p>The output from the ListOutgoingCertificates operation.</p>
newtype ListOutgoingCertificatesResponse = ListOutgoingCertificatesResponse 
  { "OutgoingCertificates'" :: NullOrUndefined (OutgoingCertificates)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }
derive instance newtypeListOutgoingCertificatesResponse :: Newtype ListOutgoingCertificatesResponse _


-- | <p>The input for the ListPolicies operation.</p>
newtype ListPoliciesRequest = ListPoliciesRequest 
  { "Marker'" :: NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined (PageSize)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListPoliciesRequest :: Newtype ListPoliciesRequest _


-- | <p>The output from the ListPolicies operation.</p>
newtype ListPoliciesResponse = ListPoliciesResponse 
  { "Policies'" :: NullOrUndefined (Policies)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }
derive instance newtypeListPoliciesResponse :: Newtype ListPoliciesResponse _


-- | <p>The input for the ListPolicyPrincipals operation.</p>
newtype ListPolicyPrincipalsRequest = ListPolicyPrincipalsRequest 
  { "PolicyName'" :: (PolicyName)
  , "Marker'" :: NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined (PageSize)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListPolicyPrincipalsRequest :: Newtype ListPolicyPrincipalsRequest _


-- | <p>The output from the ListPolicyPrincipals operation.</p>
newtype ListPolicyPrincipalsResponse = ListPolicyPrincipalsResponse 
  { "Principals'" :: NullOrUndefined (Principals)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }
derive instance newtypeListPolicyPrincipalsResponse :: Newtype ListPolicyPrincipalsResponse _


-- | <p>The input for the ListPolicyVersions operation.</p>
newtype ListPolicyVersionsRequest = ListPolicyVersionsRequest 
  { "PolicyName'" :: (PolicyName)
  }
derive instance newtypeListPolicyVersionsRequest :: Newtype ListPolicyVersionsRequest _


-- | <p>The output from the ListPolicyVersions operation.</p>
newtype ListPolicyVersionsResponse = ListPolicyVersionsResponse 
  { "PolicyVersions'" :: NullOrUndefined (PolicyVersions)
  }
derive instance newtypeListPolicyVersionsResponse :: Newtype ListPolicyVersionsResponse _


-- | <p>The input for the ListPrincipalPolicies operation.</p>
newtype ListPrincipalPoliciesRequest = ListPrincipalPoliciesRequest 
  { "Principal'" :: (Principal)
  , "Marker'" :: NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined (PageSize)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListPrincipalPoliciesRequest :: Newtype ListPrincipalPoliciesRequest _


-- | <p>The output from the ListPrincipalPolicies operation.</p>
newtype ListPrincipalPoliciesResponse = ListPrincipalPoliciesResponse 
  { "Policies'" :: NullOrUndefined (Policies)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }
derive instance newtypeListPrincipalPoliciesResponse :: Newtype ListPrincipalPoliciesResponse _


-- | <p>The input for the ListPrincipalThings operation.</p>
newtype ListPrincipalThingsRequest = ListPrincipalThingsRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  , "Principal'" :: (Principal)
  }
derive instance newtypeListPrincipalThingsRequest :: Newtype ListPrincipalThingsRequest _


-- | <p>The output from the ListPrincipalThings operation.</p>
newtype ListPrincipalThingsResponse = ListPrincipalThingsResponse 
  { "Things'" :: NullOrUndefined (ThingNameList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListPrincipalThingsResponse :: Newtype ListPrincipalThingsResponse _


newtype ListRoleAliasesRequest = ListRoleAliasesRequest 
  { "PageSize'" :: NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListRoleAliasesRequest :: Newtype ListRoleAliasesRequest _


newtype ListRoleAliasesResponse = ListRoleAliasesResponse 
  { "RoleAliases'" :: NullOrUndefined (RoleAliases)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }
derive instance newtypeListRoleAliasesResponse :: Newtype ListRoleAliasesResponse _


newtype ListStreamsRequest = ListStreamsRequest 
  { "MaxResults'" :: NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }
derive instance newtypeListStreamsRequest :: Newtype ListStreamsRequest _


newtype ListStreamsResponse = ListStreamsResponse 
  { "Streams'" :: NullOrUndefined (StreamsSummary)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListStreamsResponse :: Newtype ListStreamsResponse _


newtype ListTargetsForPolicyRequest = ListTargetsForPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Marker'" :: NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined (PageSize)
  }
derive instance newtypeListTargetsForPolicyRequest :: Newtype ListTargetsForPolicyRequest _


newtype ListTargetsForPolicyResponse = ListTargetsForPolicyResponse 
  { "Targets'" :: NullOrUndefined (PolicyTargets)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }
derive instance newtypeListTargetsForPolicyResponse :: Newtype ListTargetsForPolicyResponse _


newtype ListThingGroupsForThingRequest = ListThingGroupsForThingRequest 
  { "ThingName'" :: (ThingName)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  }
derive instance newtypeListThingGroupsForThingRequest :: Newtype ListThingGroupsForThingRequest _


newtype ListThingGroupsForThingResponse = ListThingGroupsForThingResponse 
  { "ThingGroups'" :: NullOrUndefined (ThingGroupNameAndArnList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListThingGroupsForThingResponse :: Newtype ListThingGroupsForThingResponse _


newtype ListThingGroupsRequest = ListThingGroupsRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  , "ParentGroup'" :: NullOrUndefined (ThingGroupName)
  , "NamePrefixFilter'" :: NullOrUndefined (ThingGroupName)
  , "Recursive'" :: NullOrUndefined (RecursiveWithoutDefault)
  }
derive instance newtypeListThingGroupsRequest :: Newtype ListThingGroupsRequest _


newtype ListThingGroupsResponse = ListThingGroupsResponse 
  { "ThingGroups'" :: NullOrUndefined (ThingGroupNameAndArnList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListThingGroupsResponse :: Newtype ListThingGroupsResponse _


-- | <p>The input for the ListThingPrincipal operation.</p>
newtype ListThingPrincipalsRequest = ListThingPrincipalsRequest 
  { "ThingName'" :: (ThingName)
  }
derive instance newtypeListThingPrincipalsRequest :: Newtype ListThingPrincipalsRequest _


-- | <p>The output from the ListThingPrincipals operation.</p>
newtype ListThingPrincipalsResponse = ListThingPrincipalsResponse 
  { "Principals'" :: NullOrUndefined (Principals)
  }
derive instance newtypeListThingPrincipalsResponse :: Newtype ListThingPrincipalsResponse _


newtype ListThingRegistrationTaskReportsRequest = ListThingRegistrationTaskReportsRequest 
  { "TaskId'" :: (TaskId)
  , "ReportType'" :: (ReportType)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  }
derive instance newtypeListThingRegistrationTaskReportsRequest :: Newtype ListThingRegistrationTaskReportsRequest _


newtype ListThingRegistrationTaskReportsResponse = ListThingRegistrationTaskReportsResponse 
  { "ResourceLinks'" :: NullOrUndefined (S3FileUrlList)
  , "ReportType'" :: NullOrUndefined (ReportType)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListThingRegistrationTaskReportsResponse :: Newtype ListThingRegistrationTaskReportsResponse _


newtype ListThingRegistrationTasksRequest = ListThingRegistrationTasksRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  , "Status'" :: NullOrUndefined (Status)
  }
derive instance newtypeListThingRegistrationTasksRequest :: Newtype ListThingRegistrationTasksRequest _


newtype ListThingRegistrationTasksResponse = ListThingRegistrationTasksResponse 
  { "TaskIds'" :: NullOrUndefined (TaskIdList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListThingRegistrationTasksResponse :: Newtype ListThingRegistrationTasksResponse _


-- | <p>The input for the ListThingTypes operation.</p>
newtype ListThingTypesRequest = ListThingTypesRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  }
derive instance newtypeListThingTypesRequest :: Newtype ListThingTypesRequest _


-- | <p>The output for the ListThingTypes operation.</p>
newtype ListThingTypesResponse = ListThingTypesResponse 
  { "ThingTypes'" :: NullOrUndefined (ThingTypeList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListThingTypesResponse :: Newtype ListThingTypesResponse _


newtype ListThingsInThingGroupRequest = ListThingsInThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  , "Recursive'" :: NullOrUndefined (Recursive)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  }
derive instance newtypeListThingsInThingGroupRequest :: Newtype ListThingsInThingGroupRequest _


newtype ListThingsInThingGroupResponse = ListThingsInThingGroupResponse 
  { "Things'" :: NullOrUndefined (ThingNameList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListThingsInThingGroupResponse :: Newtype ListThingsInThingGroupResponse _


-- | <p>The input for the ListThings operation.</p>
newtype ListThingsRequest = ListThingsRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  , "AttributeName'" :: NullOrUndefined (AttributeName)
  , "AttributeValue'" :: NullOrUndefined (AttributeValue)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  }
derive instance newtypeListThingsRequest :: Newtype ListThingsRequest _


-- | <p>The output from the ListThings operation.</p>
newtype ListThingsResponse = ListThingsResponse 
  { "Things'" :: NullOrUndefined (ThingAttributeList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListThingsResponse :: Newtype ListThingsResponse _


-- | <p>The input for the ListTopicRules operation.</p>
newtype ListTopicRulesRequest = ListTopicRulesRequest 
  { "Topic'" :: NullOrUndefined (Topic)
  , "MaxResults'" :: NullOrUndefined (GEMaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "RuleDisabled'" :: NullOrUndefined (IsDisabled)
  }
derive instance newtypeListTopicRulesRequest :: Newtype ListTopicRulesRequest _


-- | <p>The output from the ListTopicRules operation.</p>
newtype ListTopicRulesResponse = ListTopicRulesResponse 
  { "Rules'" :: NullOrUndefined (TopicRuleList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListTopicRulesResponse :: Newtype ListTopicRulesResponse _


newtype ListV2LoggingLevelsRequest = ListV2LoggingLevelsRequest 
  { "TargetType'" :: NullOrUndefined (LogTargetType)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (SkyfallMaxResults)
  }
derive instance newtypeListV2LoggingLevelsRequest :: Newtype ListV2LoggingLevelsRequest _


newtype ListV2LoggingLevelsResponse = ListV2LoggingLevelsResponse 
  { "LogTargetConfigurations'" :: NullOrUndefined (LogTargetConfigurations)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListV2LoggingLevelsResponse :: Newtype ListV2LoggingLevelsResponse _


newtype LogLevel = LogLevel String
derive instance newtypeLogLevel :: Newtype LogLevel _


-- | <p>A log target.</p>
newtype LogTarget = LogTarget 
  { "TargetType'" :: (LogTargetType)
  , "TargetName'" :: NullOrUndefined (LogTargetName)
  }
derive instance newtypeLogTarget :: Newtype LogTarget _


-- | <p>The target configuration.</p>
newtype LogTargetConfiguration = LogTargetConfiguration 
  { "LogTarget'" :: NullOrUndefined (LogTarget)
  , "LogLevel'" :: NullOrUndefined (LogLevel)
  }
derive instance newtypeLogTargetConfiguration :: Newtype LogTargetConfiguration _


newtype LogTargetConfigurations = LogTargetConfigurations (Array LogTargetConfiguration)
derive instance newtypeLogTargetConfigurations :: Newtype LogTargetConfigurations _


newtype LogTargetName = LogTargetName String
derive instance newtypeLogTargetName :: Newtype LogTargetName _


newtype LogTargetType = LogTargetType String
derive instance newtypeLogTargetType :: Newtype LogTargetType _


-- | <p>Describes the logging options payload.</p>
newtype LoggingOptionsPayload = LoggingOptionsPayload 
  { "RoleArn'" :: (AwsArn)
  , "LogLevel'" :: NullOrUndefined (LogLevel)
  }
derive instance newtypeLoggingOptionsPayload :: Newtype LoggingOptionsPayload _


-- | <p>The policy documentation is not valid.</p>
newtype MalformedPolicyException = MalformedPolicyException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeMalformedPolicyException :: Newtype MalformedPolicyException _


newtype Marker = Marker String
derive instance newtypeMarker :: Newtype Marker _


newtype MaxJobExecutionsPerMin = MaxJobExecutionsPerMin Int
derive instance newtypeMaxJobExecutionsPerMin :: Newtype MaxJobExecutionsPerMin _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


newtype MessageFormat = MessageFormat String
derive instance newtypeMessageFormat :: Newtype MessageFormat _


newtype MetricName = MetricName String
derive instance newtypeMetricName :: Newtype MetricName _


newtype MetricNamespace = MetricNamespace String
derive instance newtypeMetricNamespace :: Newtype MetricNamespace _


newtype MetricTimestamp = MetricTimestamp String
derive instance newtypeMetricTimestamp :: Newtype MetricTimestamp _


newtype MetricUnit = MetricUnit String
derive instance newtypeMetricUnit :: Newtype MetricUnit _


newtype MetricValue = MetricValue String
derive instance newtypeMetricValue :: Newtype MetricValue _


newtype MissingContextValue = MissingContextValue String
derive instance newtypeMissingContextValue :: Newtype MissingContextValue _


newtype MissingContextValues = MissingContextValues (Array MissingContextValue)
derive instance newtypeMissingContextValues :: Newtype MissingContextValues _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>The resource is not configured.</p>
newtype NotConfiguredException = NotConfiguredException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeNotConfiguredException :: Newtype NotConfiguredException _


newtype OTAUpdateArn = OTAUpdateArn String
derive instance newtypeOTAUpdateArn :: Newtype OTAUpdateArn _


newtype OTAUpdateDescription = OTAUpdateDescription String
derive instance newtypeOTAUpdateDescription :: Newtype OTAUpdateDescription _


newtype OTAUpdateErrorMessage = OTAUpdateErrorMessage String
derive instance newtypeOTAUpdateErrorMessage :: Newtype OTAUpdateErrorMessage _


-- | <p>Describes a file to be associated with an OTA update.</p>
newtype OTAUpdateFile = OTAUpdateFile 
  { "FileName'" :: NullOrUndefined (FileName)
  , "FileVersion'" :: NullOrUndefined (OTAUpdateFileVersion)
  , "FileSource'" :: NullOrUndefined (Stream)
  , "CodeSigning'" :: NullOrUndefined (CodeSigning)
  , "Attributes'" :: NullOrUndefined (AttributesMap)
  }
derive instance newtypeOTAUpdateFile :: Newtype OTAUpdateFile _


newtype OTAUpdateFileVersion = OTAUpdateFileVersion String
derive instance newtypeOTAUpdateFileVersion :: Newtype OTAUpdateFileVersion _


newtype OTAUpdateFiles = OTAUpdateFiles (Array OTAUpdateFile)
derive instance newtypeOTAUpdateFiles :: Newtype OTAUpdateFiles _


newtype OTAUpdateId = OTAUpdateId String
derive instance newtypeOTAUpdateId :: Newtype OTAUpdateId _


-- | <p>Information about an OTA update.</p>
newtype OTAUpdateInfo = OTAUpdateInfo 
  { "OtaUpdateId'" :: NullOrUndefined (OTAUpdateId)
  , "OtaUpdateArn'" :: NullOrUndefined (OTAUpdateArn)
  , "CreationDate'" :: NullOrUndefined (DateType)
  , "LastModifiedDate'" :: NullOrUndefined (DateType)
  , "Description'" :: NullOrUndefined (OTAUpdateDescription)
  , "Targets'" :: NullOrUndefined (Targets)
  , "TargetSelection'" :: NullOrUndefined (TargetSelection)
  , "OtaUpdateFiles'" :: NullOrUndefined (OTAUpdateFiles)
  , "OtaUpdateStatus'" :: NullOrUndefined (OTAUpdateStatus)
  , "AwsIotJobId'" :: NullOrUndefined (AwsIotJobId)
  , "AwsIotJobArn'" :: NullOrUndefined (AwsIotJobArn)
  , "ErrorInfo'" :: NullOrUndefined (ErrorInfo)
  , "AdditionalParameters'" :: NullOrUndefined (AdditionalParameterMap)
  }
derive instance newtypeOTAUpdateInfo :: Newtype OTAUpdateInfo _


newtype OTAUpdateStatus = OTAUpdateStatus String
derive instance newtypeOTAUpdateStatus :: Newtype OTAUpdateStatus _


-- | <p>An OTA update summary.</p>
newtype OTAUpdateSummary = OTAUpdateSummary 
  { "OtaUpdateId'" :: NullOrUndefined (OTAUpdateId)
  , "OtaUpdateArn'" :: NullOrUndefined (OTAUpdateArn)
  , "CreationDate'" :: NullOrUndefined (DateType)
  }
derive instance newtypeOTAUpdateSummary :: Newtype OTAUpdateSummary _


newtype OTAUpdatesSummary = OTAUpdatesSummary (Array OTAUpdateSummary)
derive instance newtypeOTAUpdatesSummary :: Newtype OTAUpdatesSummary _


newtype OptionalVersion = OptionalVersion Number
derive instance newtypeOptionalVersion :: Newtype OptionalVersion _


-- | <p>A certificate that has been transferred but not yet accepted.</p>
newtype OutgoingCertificate = OutgoingCertificate 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  , "TransferredTo'" :: NullOrUndefined (AwsAccountId)
  , "TransferDate'" :: NullOrUndefined (DateType)
  , "TransferMessage'" :: NullOrUndefined (Message)
  , "CreationDate'" :: NullOrUndefined (DateType)
  }
derive instance newtypeOutgoingCertificate :: Newtype OutgoingCertificate _


newtype OutgoingCertificates = OutgoingCertificates (Array OutgoingCertificate)
derive instance newtypeOutgoingCertificates :: Newtype OutgoingCertificates _


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _


newtype Parameter = Parameter String
derive instance newtypeParameter :: Newtype Parameter _


newtype ParameterKey = ParameterKey String
derive instance newtypeParameterKey :: Newtype ParameterKey _


newtype ParameterValue = ParameterValue String
derive instance newtypeParameterValue :: Newtype ParameterValue _


newtype Parameters = Parameters (Map Parameter Value)
derive instance newtypeParameters :: Newtype Parameters _


newtype PartitionKey = PartitionKey String
derive instance newtypePartitionKey :: Newtype PartitionKey _


newtype PayloadField = PayloadField String
derive instance newtypePayloadField :: Newtype PayloadField _


newtype Percentage = Percentage Int
derive instance newtypePercentage :: Newtype Percentage _


newtype Policies = Policies (Array Policy)
derive instance newtypePolicies :: Newtype Policies _


-- | <p>Describes an AWS IoT policy.</p>
newtype Policy = Policy 
  { "PolicyName'" :: NullOrUndefined (PolicyName)
  , "PolicyArn'" :: NullOrUndefined (PolicyArn)
  }
derive instance newtypePolicy :: Newtype Policy _


newtype PolicyArn = PolicyArn String
derive instance newtypePolicyArn :: Newtype PolicyArn _


newtype PolicyDocument = PolicyDocument String
derive instance newtypePolicyDocument :: Newtype PolicyDocument _


newtype PolicyDocuments = PolicyDocuments (Array PolicyDocument)
derive instance newtypePolicyDocuments :: Newtype PolicyDocuments _


newtype PolicyName = PolicyName String
derive instance newtypePolicyName :: Newtype PolicyName _


newtype PolicyNames = PolicyNames (Array PolicyName)
derive instance newtypePolicyNames :: Newtype PolicyNames _


newtype PolicyTarget = PolicyTarget String
derive instance newtypePolicyTarget :: Newtype PolicyTarget _


newtype PolicyTargets = PolicyTargets (Array PolicyTarget)
derive instance newtypePolicyTargets :: Newtype PolicyTargets _


-- | <p>Describes a policy version.</p>
newtype PolicyVersion = PolicyVersion 
  { "VersionId'" :: NullOrUndefined (PolicyVersionId)
  , "IsDefaultVersion'" :: NullOrUndefined (IsDefaultVersion)
  , "CreateDate'" :: NullOrUndefined (DateType)
  }
derive instance newtypePolicyVersion :: Newtype PolicyVersion _


newtype PolicyVersionId = PolicyVersionId String
derive instance newtypePolicyVersionId :: Newtype PolicyVersionId _


newtype PolicyVersions = PolicyVersions (Array PolicyVersion)
derive instance newtypePolicyVersions :: Newtype PolicyVersions _


-- | <p>Configuration for pre-signed S3 URLs.</p>
newtype PresignedUrlConfig = PresignedUrlConfig 
  { "RoleArn'" :: NullOrUndefined (RoleArn)
  , "ExpiresInSec'" :: NullOrUndefined (ExpiresInSec)
  }
derive instance newtypePresignedUrlConfig :: Newtype PresignedUrlConfig _


newtype Principal = Principal String
derive instance newtypePrincipal :: Newtype Principal _


newtype PrincipalArn = PrincipalArn String
derive instance newtypePrincipalArn :: Newtype PrincipalArn _


newtype PrincipalId = PrincipalId String
derive instance newtypePrincipalId :: Newtype PrincipalId _


newtype Principals = Principals (Array PrincipalArn)
derive instance newtypePrincipals :: Newtype Principals _


newtype PrivateKey = PrivateKey String
derive instance newtypePrivateKey :: Newtype PrivateKey _


newtype ProcessingTargetName = ProcessingTargetName String
derive instance newtypeProcessingTargetName :: Newtype ProcessingTargetName _


newtype ProcessingTargetNameList = ProcessingTargetNameList (Array ProcessingTargetName)
derive instance newtypeProcessingTargetNameList :: Newtype ProcessingTargetNameList _


newtype PublicKey = PublicKey String
derive instance newtypePublicKey :: Newtype PublicKey _


newtype PublicKeyMap = PublicKeyMap (Map KeyName KeyValue)
derive instance newtypePublicKeyMap :: Newtype PublicKeyMap _


-- | <p>The input for the DynamoActionVS action that specifies the DynamoDB table to which the message data will be written.</p>
newtype PutItemInput = PutItemInput 
  { "TableName'" :: (TableName)
  }
derive instance newtypePutItemInput :: Newtype PutItemInput _


newtype QueryMaxResults = QueryMaxResults Int
derive instance newtypeQueryMaxResults :: Newtype QueryMaxResults _


newtype QueryString = QueryString String
derive instance newtypeQueryString :: Newtype QueryString _


newtype QueryVersion = QueryVersion String
derive instance newtypeQueryVersion :: Newtype QueryVersion _


newtype QueueUrl = QueueUrl String
derive instance newtypeQueueUrl :: Newtype QueueUrl _


newtype QueuedThings = QueuedThings Int
derive instance newtypeQueuedThings :: Newtype QueuedThings _


newtype RangeKeyField = RangeKeyField String
derive instance newtypeRangeKeyField :: Newtype RangeKeyField _


newtype RangeKeyValue = RangeKeyValue String
derive instance newtypeRangeKeyValue :: Newtype RangeKeyValue _


newtype Recursive = Recursive Boolean
derive instance newtypeRecursive :: Newtype Recursive _


newtype RecursiveWithoutDefault = RecursiveWithoutDefault Boolean
derive instance newtypeRecursiveWithoutDefault :: Newtype RecursiveWithoutDefault _


-- | <p>The input to the RegisterCACertificate operation.</p>
newtype RegisterCACertificateRequest = RegisterCACertificateRequest 
  { "CaCertificate'" :: (CertificatePem)
  , "VerificationCertificate'" :: (CertificatePem)
  , "SetAsActive'" :: NullOrUndefined (SetAsActive)
  , "AllowAutoRegistration'" :: NullOrUndefined (AllowAutoRegistration)
  , "RegistrationConfig'" :: NullOrUndefined (RegistrationConfig)
  }
derive instance newtypeRegisterCACertificateRequest :: Newtype RegisterCACertificateRequest _


-- | <p>The output from the RegisterCACertificateResponse operation.</p>
newtype RegisterCACertificateResponse = RegisterCACertificateResponse 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  }
derive instance newtypeRegisterCACertificateResponse :: Newtype RegisterCACertificateResponse _


-- | <p>The input to the RegisterCertificate operation.</p>
newtype RegisterCertificateRequest = RegisterCertificateRequest 
  { "CertificatePem'" :: (CertificatePem)
  , "CaCertificatePem'" :: NullOrUndefined (CertificatePem)
  , "SetAsActive'" :: NullOrUndefined (SetAsActiveFlag)
  , "Status'" :: NullOrUndefined (CertificateStatus)
  }
derive instance newtypeRegisterCertificateRequest :: Newtype RegisterCertificateRequest _


-- | <p>The output from the RegisterCertificate operation.</p>
newtype RegisterCertificateResponse = RegisterCertificateResponse 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  }
derive instance newtypeRegisterCertificateResponse :: Newtype RegisterCertificateResponse _


newtype RegisterThingRequest = RegisterThingRequest 
  { "TemplateBody'" :: (TemplateBody)
  , "Parameters'" :: NullOrUndefined (Parameters)
  }
derive instance newtypeRegisterThingRequest :: Newtype RegisterThingRequest _


newtype RegisterThingResponse = RegisterThingResponse 
  { "CertificatePem'" :: NullOrUndefined (CertificatePem)
  , "ResourceArns'" :: NullOrUndefined (ResourceArns)
  }
derive instance newtypeRegisterThingResponse :: Newtype RegisterThingResponse _


newtype RegistrationCode = RegistrationCode String
derive instance newtypeRegistrationCode :: Newtype RegistrationCode _


-- | <p>The registration code is invalid.</p>
newtype RegistrationCodeValidationException = RegistrationCodeValidationException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeRegistrationCodeValidationException :: Newtype RegistrationCodeValidationException _


-- | <p>The registration configuration.</p>
newtype RegistrationConfig = RegistrationConfig 
  { "TemplateBody'" :: NullOrUndefined (TemplateBody)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  }
derive instance newtypeRegistrationConfig :: Newtype RegistrationConfig _


newtype RegistryMaxResults = RegistryMaxResults Int
derive instance newtypeRegistryMaxResults :: Newtype RegistryMaxResults _


newtype RegistryS3BucketName = RegistryS3BucketName String
derive instance newtypeRegistryS3BucketName :: Newtype RegistryS3BucketName _


newtype RegistryS3KeyName = RegistryS3KeyName String
derive instance newtypeRegistryS3KeyName :: Newtype RegistryS3KeyName _


-- | <p>The input for the RejectCertificateTransfer operation.</p>
newtype RejectCertificateTransferRequest = RejectCertificateTransferRequest 
  { "CertificateId'" :: (CertificateId)
  , "RejectReason'" :: NullOrUndefined (Message)
  }
derive instance newtypeRejectCertificateTransferRequest :: Newtype RejectCertificateTransferRequest _


newtype RejectedThings = RejectedThings Int
derive instance newtypeRejectedThings :: Newtype RejectedThings _


newtype RemoveAutoRegistration = RemoveAutoRegistration Boolean
derive instance newtypeRemoveAutoRegistration :: Newtype RemoveAutoRegistration _


newtype RemoveThingFromThingGroupRequest = RemoveThingFromThingGroupRequest 
  { "ThingGroupName'" :: NullOrUndefined (ThingGroupName)
  , "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn)
  , "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingArn'" :: NullOrUndefined (ThingArn)
  }
derive instance newtypeRemoveThingFromThingGroupRequest :: Newtype RemoveThingFromThingGroupRequest _


newtype RemoveThingFromThingGroupResponse = RemoveThingFromThingGroupResponse 
  { 
  }
derive instance newtypeRemoveThingFromThingGroupResponse :: Newtype RemoveThingFromThingGroupResponse _


newtype RemoveThingType = RemoveThingType Boolean
derive instance newtypeRemoveThingType :: Newtype RemoveThingType _


newtype RemovedThings = RemovedThings Int
derive instance newtypeRemovedThings :: Newtype RemovedThings _


-- | <p>The input for the ReplaceTopicRule operation.</p>
newtype ReplaceTopicRuleRequest = ReplaceTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  , "TopicRulePayload'" :: (TopicRulePayload)
  }
derive instance newtypeReplaceTopicRuleRequest :: Newtype ReplaceTopicRuleRequest _


newtype ReportType = ReportType String
derive instance newtypeReportType :: Newtype ReportType _


-- | <p>Describes an action to republish to another topic.</p>
newtype RepublishAction = RepublishAction 
  { "RoleArn'" :: (AwsArn)
  , "Topic'" :: (TopicPattern)
  }
derive instance newtypeRepublishAction :: Newtype RepublishAction _


newtype Resource = Resource String
derive instance newtypeResource :: Newtype Resource _


-- | <p>The resource already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  , "ResourceId'" :: NullOrUndefined (ResourceId')
  , "ResourceArn'" :: NullOrUndefined (ResourceArn')
  }
derive instance newtypeResourceAlreadyExistsException :: Newtype ResourceAlreadyExistsException _


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _


newtype ResourceArns = ResourceArns (Map ResourceLogicalId ResourceArn)
derive instance newtypeResourceArns :: Newtype ResourceArns _


newtype ResourceLogicalId = ResourceLogicalId String
derive instance newtypeResourceLogicalId :: Newtype ResourceLogicalId _


-- | <p>The specified resource does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>The resource registration failed.</p>
newtype ResourceRegistrationFailureException = ResourceRegistrationFailureException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeResourceRegistrationFailureException :: Newtype ResourceRegistrationFailureException _


newtype Resources = Resources (Array Resource)
derive instance newtypeResources :: Newtype Resources _


newtype RoleAlias = RoleAlias String
derive instance newtypeRoleAlias :: Newtype RoleAlias _


newtype RoleAliasArn = RoleAliasArn String
derive instance newtypeRoleAliasArn :: Newtype RoleAliasArn _


-- | <p>Role alias description.</p>
newtype RoleAliasDescription = RoleAliasDescription 
  { "RoleAlias'" :: NullOrUndefined (RoleAlias)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  , "Owner'" :: NullOrUndefined (AwsAccountId)
  , "CredentialDurationSeconds'" :: NullOrUndefined (CredentialDurationSeconds)
  , "CreationDate'" :: NullOrUndefined (DateType)
  , "LastModifiedDate'" :: NullOrUndefined (DateType)
  }
derive instance newtypeRoleAliasDescription :: Newtype RoleAliasDescription _


newtype RoleAliases = RoleAliases (Array RoleAlias)
derive instance newtypeRoleAliases :: Newtype RoleAliases _


newtype RoleArn = RoleArn String
derive instance newtypeRoleArn :: Newtype RoleArn _


newtype RuleArn = RuleArn String
derive instance newtypeRuleArn :: Newtype RuleArn _


newtype RuleName = RuleName String
derive instance newtypeRuleName :: Newtype RuleName _


-- | <p>Describes an action to write data to an Amazon S3 bucket.</p>
newtype S3Action = S3Action 
  { "RoleArn'" :: (AwsArn)
  , "BucketName'" :: (BucketName)
  , "Key'" :: (Key)
  , "CannedAcl'" :: NullOrUndefined (CannedAccessControlList)
  }
derive instance newtypeS3Action :: Newtype S3Action _


newtype S3Bucket = S3Bucket String
derive instance newtypeS3Bucket :: Newtype S3Bucket _


newtype S3FileUrl = S3FileUrl String
derive instance newtypeS3FileUrl :: Newtype S3FileUrl _


newtype S3FileUrlList = S3FileUrlList (Array S3FileUrl)
derive instance newtypeS3FileUrlList :: Newtype S3FileUrlList _


newtype S3Key = S3Key String
derive instance newtypeS3Key :: Newtype S3Key _


-- | <p>The location in S3 the contains the files to stream.</p>
newtype S3Location = S3Location 
  { "Bucket'" :: (S3Bucket)
  , "Key'" :: (S3Key)
  , "Version'" :: NullOrUndefined (S3Version)
  }
derive instance newtypeS3Location :: Newtype S3Location _


newtype S3Version = S3Version String
derive instance newtypeS3Version :: Newtype S3Version _


newtype SQL = SQL String
derive instance newtypeSQL :: Newtype SQL _


-- | <p>Describes an action to write a message to a Salesforce IoT Cloud Input Stream.</p>
newtype SalesforceAction = SalesforceAction 
  { "Token'" :: (SalesforceToken)
  , "Url'" :: (SalesforceEndpoint)
  }
derive instance newtypeSalesforceAction :: Newtype SalesforceAction _


newtype SalesforceEndpoint = SalesforceEndpoint String
derive instance newtypeSalesforceEndpoint :: Newtype SalesforceEndpoint _


newtype SalesforceToken = SalesforceToken String
derive instance newtypeSalesforceToken :: Newtype SalesforceToken _


newtype SearchIndexRequest = SearchIndexRequest 
  { "IndexName'" :: NullOrUndefined (IndexName)
  , "QueryString'" :: (QueryString)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (QueryMaxResults)
  , "QueryVersion'" :: NullOrUndefined (QueryVersion)
  }
derive instance newtypeSearchIndexRequest :: Newtype SearchIndexRequest _


newtype SearchIndexResponse = SearchIndexResponse 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "Things'" :: NullOrUndefined (ThingDocumentList)
  }
derive instance newtypeSearchIndexResponse :: Newtype SearchIndexResponse _


newtype SearchableAttributes = SearchableAttributes (Array AttributeName)
derive instance newtypeSearchableAttributes :: Newtype SearchableAttributes _


newtype Seconds = Seconds Int
derive instance newtypeSeconds :: Newtype Seconds _


-- | <p>The service is temporarily unavailable.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _


newtype SetAsActive = SetAsActive Boolean
derive instance newtypeSetAsActive :: Newtype SetAsActive _


newtype SetAsActiveFlag = SetAsActiveFlag Boolean
derive instance newtypeSetAsActiveFlag :: Newtype SetAsActiveFlag _


newtype SetAsDefault = SetAsDefault Boolean
derive instance newtypeSetAsDefault :: Newtype SetAsDefault _


newtype SetDefaultAuthorizerRequest = SetDefaultAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  }
derive instance newtypeSetDefaultAuthorizerRequest :: Newtype SetDefaultAuthorizerRequest _


newtype SetDefaultAuthorizerResponse = SetDefaultAuthorizerResponse 
  { "AuthorizerName'" :: NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn)
  }
derive instance newtypeSetDefaultAuthorizerResponse :: Newtype SetDefaultAuthorizerResponse _


-- | <p>The input for the SetDefaultPolicyVersion operation.</p>
newtype SetDefaultPolicyVersionRequest = SetDefaultPolicyVersionRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyVersionId'" :: (PolicyVersionId)
  }
derive instance newtypeSetDefaultPolicyVersionRequest :: Newtype SetDefaultPolicyVersionRequest _


-- | <p>The input for the SetLoggingOptions operation.</p>
newtype SetLoggingOptionsRequest = SetLoggingOptionsRequest 
  { "LoggingOptionsPayload'" :: (LoggingOptionsPayload)
  }
derive instance newtypeSetLoggingOptionsRequest :: Newtype SetLoggingOptionsRequest _


newtype SetV2LoggingLevelRequest = SetV2LoggingLevelRequest 
  { "LogTarget'" :: (LogTarget)
  , "LogLevel'" :: (LogLevel)
  }
derive instance newtypeSetV2LoggingLevelRequest :: Newtype SetV2LoggingLevelRequest _


newtype SetV2LoggingOptionsRequest = SetV2LoggingOptionsRequest 
  { "RoleArn'" :: NullOrUndefined (AwsArn)
  , "DefaultLogLevel'" :: NullOrUndefined (LogLevel)
  , "DisableAllLogs'" :: NullOrUndefined (DisableAllLogs)
  }
derive instance newtypeSetV2LoggingOptionsRequest :: Newtype SetV2LoggingOptionsRequest _


newtype Signature = Signature String
derive instance newtypeSignature :: Newtype Signature _


newtype SignatureAlgorithm = SignatureAlgorithm String
derive instance newtypeSignatureAlgorithm :: Newtype SignatureAlgorithm _


newtype SigningJobId = SigningJobId String
derive instance newtypeSigningJobId :: Newtype SigningJobId _


newtype SkyfallMaxResults = SkyfallMaxResults Int
derive instance newtypeSkyfallMaxResults :: Newtype SkyfallMaxResults _


-- | <p>Describes an action to publish to an Amazon SNS topic.</p>
newtype SnsAction = SnsAction 
  { "TargetArn'" :: (AwsArn)
  , "RoleArn'" :: (AwsArn)
  , "MessageFormat'" :: NullOrUndefined (MessageFormat)
  }
derive instance newtypeSnsAction :: Newtype SnsAction _


-- | <p>The Rule-SQL expression can't be parsed correctly.</p>
newtype SqlParseException = SqlParseException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeSqlParseException :: Newtype SqlParseException _


-- | <p>Describes an action to publish data to an Amazon SQS queue.</p>
newtype SqsAction = SqsAction 
  { "RoleArn'" :: (AwsArn)
  , "QueueUrl'" :: (QueueUrl)
  , "UseBase64'" :: NullOrUndefined (UseBase64)
  }
derive instance newtypeSqsAction :: Newtype SqsAction _


newtype StartThingRegistrationTaskRequest = StartThingRegistrationTaskRequest 
  { "TemplateBody'" :: (TemplateBody)
  , "InputFileBucket'" :: (RegistryS3BucketName)
  , "InputFileKey'" :: (RegistryS3KeyName)
  , "RoleArn'" :: (RoleArn)
  }
derive instance newtypeStartThingRegistrationTaskRequest :: Newtype StartThingRegistrationTaskRequest _


newtype StartThingRegistrationTaskResponse = StartThingRegistrationTaskResponse 
  { "TaskId'" :: NullOrUndefined (TaskId)
  }
derive instance newtypeStartThingRegistrationTaskResponse :: Newtype StartThingRegistrationTaskResponse _


newtype StateReason = StateReason String
derive instance newtypeStateReason :: Newtype StateReason _


newtype StateValue = StateValue String
derive instance newtypeStateValue :: Newtype StateValue _


newtype Status = Status String
derive instance newtypeStatus :: Newtype Status _


newtype StopThingRegistrationTaskRequest = StopThingRegistrationTaskRequest 
  { "TaskId'" :: (TaskId)
  }
derive instance newtypeStopThingRegistrationTaskRequest :: Newtype StopThingRegistrationTaskRequest _


newtype StopThingRegistrationTaskResponse = StopThingRegistrationTaskResponse 
  { 
  }
derive instance newtypeStopThingRegistrationTaskResponse :: Newtype StopThingRegistrationTaskResponse _


-- | <p>Describes a group of files that can be streamed.</p>
newtype Stream = Stream 
  { "StreamId'" :: NullOrUndefined (StreamId)
  , "FileId'" :: NullOrUndefined (FileId)
  }
derive instance newtypeStream :: Newtype Stream _


newtype StreamArn = StreamArn String
derive instance newtypeStreamArn :: Newtype StreamArn _


newtype StreamDescription = StreamDescription String
derive instance newtypeStreamDescription :: Newtype StreamDescription _


-- | <p>Represents a file to stream.</p>
newtype StreamFile = StreamFile 
  { "FileId'" :: NullOrUndefined (FileId)
  , "S3Location'" :: NullOrUndefined (S3Location)
  }
derive instance newtypeStreamFile :: Newtype StreamFile _


newtype StreamFiles = StreamFiles (Array StreamFile)
derive instance newtypeStreamFiles :: Newtype StreamFiles _


newtype StreamId = StreamId String
derive instance newtypeStreamId :: Newtype StreamId _


-- | <p>Information about a stream.</p>
newtype StreamInfo = StreamInfo 
  { "StreamId'" :: NullOrUndefined (StreamId)
  , "StreamArn'" :: NullOrUndefined (StreamArn)
  , "StreamVersion'" :: NullOrUndefined (StreamVersion)
  , "Description'" :: NullOrUndefined (StreamDescription)
  , "Files'" :: NullOrUndefined (StreamFiles)
  , "CreatedAt'" :: NullOrUndefined (DateType)
  , "LastUpdatedAt'" :: NullOrUndefined (DateType)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  }
derive instance newtypeStreamInfo :: Newtype StreamInfo _


newtype StreamName = StreamName String
derive instance newtypeStreamName :: Newtype StreamName _


-- | <p>A summary of a stream.</p>
newtype StreamSummary = StreamSummary 
  { "StreamId'" :: NullOrUndefined (StreamId)
  , "StreamArn'" :: NullOrUndefined (StreamArn)
  , "StreamVersion'" :: NullOrUndefined (StreamVersion)
  , "Description'" :: NullOrUndefined (StreamDescription)
  }
derive instance newtypeStreamSummary :: Newtype StreamSummary _


newtype StreamVersion = StreamVersion Int
derive instance newtypeStreamVersion :: Newtype StreamVersion _


newtype StreamsSummary = StreamsSummary (Array StreamSummary)
derive instance newtypeStreamsSummary :: Newtype StreamsSummary _


newtype SucceededThings = SucceededThings Int
derive instance newtypeSucceededThings :: Newtype SucceededThings _


newtype TableName = TableName String
derive instance newtypeTableName :: Newtype TableName _


newtype Target = Target String
derive instance newtypeTarget :: Newtype Target _


newtype TargetArn = TargetArn String
derive instance newtypeTargetArn :: Newtype TargetArn _


newtype TargetSelection = TargetSelection String
derive instance newtypeTargetSelection :: Newtype TargetSelection _


newtype Targets = Targets (Array Target)
derive instance newtypeTargets :: Newtype Targets _


newtype TaskId = TaskId String
derive instance newtypeTaskId :: Newtype TaskId _


newtype TaskIdList = TaskIdList (Array TaskId)
derive instance newtypeTaskIdList :: Newtype TaskIdList _


newtype TemplateBody = TemplateBody String
derive instance newtypeTemplateBody :: Newtype TemplateBody _


newtype TestAuthorizationRequest = TestAuthorizationRequest 
  { "Principal'" :: NullOrUndefined (Principal)
  , "CognitoIdentityPoolId'" :: NullOrUndefined (CognitoIdentityPoolId)
  , "AuthInfos'" :: (AuthInfos)
  , "ClientId'" :: NullOrUndefined (ClientId)
  , "PolicyNamesToAdd'" :: NullOrUndefined (PolicyNames)
  , "PolicyNamesToSkip'" :: NullOrUndefined (PolicyNames)
  }
derive instance newtypeTestAuthorizationRequest :: Newtype TestAuthorizationRequest _


newtype TestAuthorizationResponse = TestAuthorizationResponse 
  { "AuthResults'" :: NullOrUndefined (AuthResults)
  }
derive instance newtypeTestAuthorizationResponse :: Newtype TestAuthorizationResponse _


newtype TestInvokeAuthorizerRequest = TestInvokeAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  , "Token'" :: (Token)
  , "TokenSignature'" :: (TokenSignature)
  }
derive instance newtypeTestInvokeAuthorizerRequest :: Newtype TestInvokeAuthorizerRequest _


newtype TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse 
  { "IsAuthenticated'" :: NullOrUndefined (IsAuthenticated)
  , "PrincipalId'" :: NullOrUndefined (PrincipalId)
  , "PolicyDocuments'" :: NullOrUndefined (PolicyDocuments)
  , "RefreshAfterInSeconds'" :: NullOrUndefined (Seconds)
  , "DisconnectAfterInSeconds'" :: NullOrUndefined (Seconds)
  }
derive instance newtypeTestInvokeAuthorizerResponse :: Newtype TestInvokeAuthorizerResponse _


newtype ThingArn = ThingArn String
derive instance newtypeThingArn :: Newtype ThingArn _


-- | <p>The properties of the thing, including thing name, thing type name, and a list of thing attributes.</p>
newtype ThingAttribute = ThingAttribute 
  { "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "ThingArn'" :: NullOrUndefined (ThingArn)
  , "Attributes'" :: NullOrUndefined (Attributes)
  , "Version'" :: NullOrUndefined (Version)
  }
derive instance newtypeThingAttribute :: Newtype ThingAttribute _


newtype ThingAttributeList = ThingAttributeList (Array ThingAttribute)
derive instance newtypeThingAttributeList :: Newtype ThingAttributeList _


-- | <p>The thing search index document.</p>
newtype ThingDocument = ThingDocument 
  { "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingId'" :: NullOrUndefined (ThingId)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "ThingGroupNames'" :: NullOrUndefined (ThingGroupNameList)
  , "Attributes'" :: NullOrUndefined (Attributes)
  , "Shadow'" :: NullOrUndefined (JsonDocument)
  }
derive instance newtypeThingDocument :: Newtype ThingDocument _


newtype ThingDocumentList = ThingDocumentList (Array ThingDocument)
derive instance newtypeThingDocumentList :: Newtype ThingDocumentList _


newtype ThingGroupArn = ThingGroupArn String
derive instance newtypeThingGroupArn :: Newtype ThingGroupArn _


newtype ThingGroupDescription = ThingGroupDescription String
derive instance newtypeThingGroupDescription :: Newtype ThingGroupDescription _


newtype ThingGroupId = ThingGroupId String
derive instance newtypeThingGroupId :: Newtype ThingGroupId _


newtype ThingGroupList = ThingGroupList (Array ThingGroupName)
derive instance newtypeThingGroupList :: Newtype ThingGroupList _


-- | <p>Thing group metadata.</p>
newtype ThingGroupMetadata = ThingGroupMetadata 
  { "ParentGroupName'" :: NullOrUndefined (ThingGroupName)
  , "RootToParentThingGroups'" :: NullOrUndefined (ThingGroupNameAndArnList)
  , "CreationDate'" :: NullOrUndefined (CreationDate)
  }
derive instance newtypeThingGroupMetadata :: Newtype ThingGroupMetadata _


newtype ThingGroupName = ThingGroupName String
derive instance newtypeThingGroupName :: Newtype ThingGroupName _


newtype ThingGroupNameAndArnList = ThingGroupNameAndArnList (Array GroupNameAndArn)
derive instance newtypeThingGroupNameAndArnList :: Newtype ThingGroupNameAndArnList _


newtype ThingGroupNameList = ThingGroupNameList (Array ThingGroupName)
derive instance newtypeThingGroupNameList :: Newtype ThingGroupNameList _


-- | <p>Thing group properties.</p>
newtype ThingGroupProperties = ThingGroupProperties 
  { "ThingGroupDescription'" :: NullOrUndefined (ThingGroupDescription)
  , "AttributePayload'" :: NullOrUndefined (AttributePayload)
  }
derive instance newtypeThingGroupProperties :: Newtype ThingGroupProperties _


newtype ThingId = ThingId String
derive instance newtypeThingId :: Newtype ThingId _


-- | <p>Thing indexing configuration.</p>
newtype ThingIndexingConfiguration = ThingIndexingConfiguration 
  { "ThingIndexingMode'" :: NullOrUndefined (ThingIndexingMode)
  }
derive instance newtypeThingIndexingConfiguration :: Newtype ThingIndexingConfiguration _


newtype ThingIndexingMode = ThingIndexingMode String
derive instance newtypeThingIndexingMode :: Newtype ThingIndexingMode _


newtype ThingName = ThingName String
derive instance newtypeThingName :: Newtype ThingName _


newtype ThingNameList = ThingNameList (Array ThingName)
derive instance newtypeThingNameList :: Newtype ThingNameList _


newtype ThingTypeArn = ThingTypeArn String
derive instance newtypeThingTypeArn :: Newtype ThingTypeArn _


-- | <p>The definition of the thing type, including thing type name and description.</p>
newtype ThingTypeDefinition = ThingTypeDefinition 
  { "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "ThingTypeArn'" :: NullOrUndefined (ThingTypeArn)
  , "ThingTypeProperties'" :: NullOrUndefined (ThingTypeProperties)
  , "ThingTypeMetadata'" :: NullOrUndefined (ThingTypeMetadata)
  }
derive instance newtypeThingTypeDefinition :: Newtype ThingTypeDefinition _


newtype ThingTypeDescription = ThingTypeDescription String
derive instance newtypeThingTypeDescription :: Newtype ThingTypeDescription _


newtype ThingTypeId = ThingTypeId String
derive instance newtypeThingTypeId :: Newtype ThingTypeId _


newtype ThingTypeList = ThingTypeList (Array ThingTypeDefinition)
derive instance newtypeThingTypeList :: Newtype ThingTypeList _


-- | <p>The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when time was deprecated.</p>
newtype ThingTypeMetadata = ThingTypeMetadata 
  { "Deprecated'" :: NullOrUndefined (Boolean)
  , "DeprecationDate'" :: NullOrUndefined (DeprecationDate)
  , "CreationDate'" :: NullOrUndefined (CreationDate)
  }
derive instance newtypeThingTypeMetadata :: Newtype ThingTypeMetadata _


newtype ThingTypeName = ThingTypeName String
derive instance newtypeThingTypeName :: Newtype ThingTypeName _


-- | <p>The ThingTypeProperties contains information about the thing type including: a thing type description, and a list of searchable thing attribute names.</p>
newtype ThingTypeProperties = ThingTypeProperties 
  { "ThingTypeDescription'" :: NullOrUndefined (ThingTypeDescription)
  , "SearchableAttributes'" :: NullOrUndefined (SearchableAttributes)
  }
derive instance newtypeThingTypeProperties :: Newtype ThingTypeProperties _


-- | <p>The rate exceeds the limit.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _


newtype Token = Token String
derive instance newtypeToken :: Newtype Token _


newtype TokenKeyName = TokenKeyName String
derive instance newtypeTokenKeyName :: Newtype TokenKeyName _


newtype TokenSignature = TokenSignature String
derive instance newtypeTokenSignature :: Newtype TokenSignature _


newtype Topic = Topic String
derive instance newtypeTopic :: Newtype Topic _


newtype TopicPattern = TopicPattern String
derive instance newtypeTopicPattern :: Newtype TopicPattern _


-- | <p>Describes a rule.</p>
newtype TopicRule = TopicRule 
  { "RuleName'" :: NullOrUndefined (RuleName)
  , "Sql'" :: NullOrUndefined (SQL)
  , "Description'" :: NullOrUndefined (Description)
  , "CreatedAt'" :: NullOrUndefined (CreatedAtDate)
  , "Actions'" :: NullOrUndefined (ActionList)
  , "RuleDisabled'" :: NullOrUndefined (IsDisabled)
  , "AwsIotSqlVersion'" :: NullOrUndefined (AwsIotSqlVersion)
  , "ErrorAction'" :: NullOrUndefined (Action)
  }
derive instance newtypeTopicRule :: Newtype TopicRule _


newtype TopicRuleList = TopicRuleList (Array TopicRuleListItem)
derive instance newtypeTopicRuleList :: Newtype TopicRuleList _


-- | <p>Describes a rule.</p>
newtype TopicRuleListItem = TopicRuleListItem 
  { "RuleArn'" :: NullOrUndefined (RuleArn)
  , "RuleName'" :: NullOrUndefined (RuleName)
  , "TopicPattern'" :: NullOrUndefined (TopicPattern)
  , "CreatedAt'" :: NullOrUndefined (CreatedAtDate)
  , "RuleDisabled'" :: NullOrUndefined (IsDisabled)
  }
derive instance newtypeTopicRuleListItem :: Newtype TopicRuleListItem _


-- | <p>Describes a rule.</p>
newtype TopicRulePayload = TopicRulePayload 
  { "Sql'" :: (SQL)
  , "Description'" :: NullOrUndefined (Description)
  , "Actions'" :: (ActionList)
  , "RuleDisabled'" :: NullOrUndefined (IsDisabled)
  , "AwsIotSqlVersion'" :: NullOrUndefined (AwsIotSqlVersion)
  , "ErrorAction'" :: NullOrUndefined (Action)
  }
derive instance newtypeTopicRulePayload :: Newtype TopicRulePayload _


-- | <p>You can't revert the certificate transfer because the transfer is already complete.</p>
newtype TransferAlreadyCompletedException = TransferAlreadyCompletedException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeTransferAlreadyCompletedException :: Newtype TransferAlreadyCompletedException _


-- | <p>The input for the TransferCertificate operation.</p>
newtype TransferCertificateRequest = TransferCertificateRequest 
  { "CertificateId'" :: (CertificateId)
  , "TargetAwsAccount'" :: (AwsAccountId)
  , "TransferMessage'" :: NullOrUndefined (Message)
  }
derive instance newtypeTransferCertificateRequest :: Newtype TransferCertificateRequest _


-- | <p>The output from the TransferCertificate operation.</p>
newtype TransferCertificateResponse = TransferCertificateResponse 
  { "TransferredCertificateArn'" :: NullOrUndefined (CertificateArn)
  }
derive instance newtypeTransferCertificateResponse :: Newtype TransferCertificateResponse _


-- | <p>You can't transfer the certificate because authorization policies are still attached.</p>
newtype TransferConflictException = TransferConflictException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeTransferConflictException :: Newtype TransferConflictException _


-- | <p>Data used to transfer a certificate to an AWS account.</p>
newtype TransferData = TransferData 
  { "TransferMessage'" :: NullOrUndefined (Message)
  , "RejectReason'" :: NullOrUndefined (Message)
  , "TransferDate'" :: NullOrUndefined (DateType)
  , "AcceptDate'" :: NullOrUndefined (DateType)
  , "RejectDate'" :: NullOrUndefined (DateType)
  }
derive instance newtypeTransferData :: Newtype TransferData _


-- | <p>You are not authorized to perform this operation.</p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeUnauthorizedException :: Newtype UnauthorizedException _


newtype UndoDeprecate = UndoDeprecate Boolean
derive instance newtypeUndoDeprecate :: Newtype UndoDeprecate _


newtype UpdateAuthorizerRequest = UpdateAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  , "AuthorizerFunctionArn'" :: NullOrUndefined (AuthorizerFunctionArn)
  , "TokenKeyName'" :: NullOrUndefined (TokenKeyName)
  , "TokenSigningPublicKeys'" :: NullOrUndefined (PublicKeyMap)
  , "Status'" :: NullOrUndefined (AuthorizerStatus)
  }
derive instance newtypeUpdateAuthorizerRequest :: Newtype UpdateAuthorizerRequest _


newtype UpdateAuthorizerResponse = UpdateAuthorizerResponse 
  { "AuthorizerName'" :: NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn)
  }
derive instance newtypeUpdateAuthorizerResponse :: Newtype UpdateAuthorizerResponse _


-- | <p>The input to the UpdateCACertificate operation.</p>
newtype UpdateCACertificateRequest = UpdateCACertificateRequest 
  { "CertificateId'" :: (CertificateId)
  , "NewStatus'" :: NullOrUndefined (CACertificateStatus)
  , "NewAutoRegistrationStatus'" :: NullOrUndefined (AutoRegistrationStatus)
  , "RegistrationConfig'" :: NullOrUndefined (RegistrationConfig)
  , "RemoveAutoRegistration'" :: NullOrUndefined (RemoveAutoRegistration)
  }
derive instance newtypeUpdateCACertificateRequest :: Newtype UpdateCACertificateRequest _


-- | <p>The input for the UpdateCertificate operation.</p>
newtype UpdateCertificateRequest = UpdateCertificateRequest 
  { "CertificateId'" :: (CertificateId)
  , "NewStatus'" :: (CertificateStatus)
  }
derive instance newtypeUpdateCertificateRequest :: Newtype UpdateCertificateRequest _


newtype UpdateEventConfigurationsRequest = UpdateEventConfigurationsRequest 
  { "EventConfigurations'" :: NullOrUndefined (EventConfigurations)
  }
derive instance newtypeUpdateEventConfigurationsRequest :: Newtype UpdateEventConfigurationsRequest _


newtype UpdateEventConfigurationsResponse = UpdateEventConfigurationsResponse 
  { 
  }
derive instance newtypeUpdateEventConfigurationsResponse :: Newtype UpdateEventConfigurationsResponse _


newtype UpdateIndexingConfigurationRequest = UpdateIndexingConfigurationRequest 
  { "ThingIndexingConfiguration'" :: NullOrUndefined (ThingIndexingConfiguration)
  }
derive instance newtypeUpdateIndexingConfigurationRequest :: Newtype UpdateIndexingConfigurationRequest _


newtype UpdateIndexingConfigurationResponse = UpdateIndexingConfigurationResponse 
  { 
  }
derive instance newtypeUpdateIndexingConfigurationResponse :: Newtype UpdateIndexingConfigurationResponse _


newtype UpdateRoleAliasRequest = UpdateRoleAliasRequest 
  { "RoleAlias'" :: (RoleAlias)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  , "CredentialDurationSeconds'" :: NullOrUndefined (CredentialDurationSeconds)
  }
derive instance newtypeUpdateRoleAliasRequest :: Newtype UpdateRoleAliasRequest _


newtype UpdateRoleAliasResponse = UpdateRoleAliasResponse 
  { "RoleAlias'" :: NullOrUndefined (RoleAlias)
  , "RoleAliasArn'" :: NullOrUndefined (RoleAliasArn)
  }
derive instance newtypeUpdateRoleAliasResponse :: Newtype UpdateRoleAliasResponse _


newtype UpdateStreamRequest = UpdateStreamRequest 
  { "StreamId'" :: (StreamId)
  , "Description'" :: NullOrUndefined (StreamDescription)
  , "Files'" :: NullOrUndefined (StreamFiles)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  }
derive instance newtypeUpdateStreamRequest :: Newtype UpdateStreamRequest _


newtype UpdateStreamResponse = UpdateStreamResponse 
  { "StreamId'" :: NullOrUndefined (StreamId)
  , "StreamArn'" :: NullOrUndefined (StreamArn)
  , "Description'" :: NullOrUndefined (StreamDescription)
  , "StreamVersion'" :: NullOrUndefined (StreamVersion)
  }
derive instance newtypeUpdateStreamResponse :: Newtype UpdateStreamResponse _


newtype UpdateThingGroupRequest = UpdateThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  , "ThingGroupProperties'" :: (ThingGroupProperties)
  , "ExpectedVersion'" :: NullOrUndefined (OptionalVersion)
  }
derive instance newtypeUpdateThingGroupRequest :: Newtype UpdateThingGroupRequest _


newtype UpdateThingGroupResponse = UpdateThingGroupResponse 
  { "Version'" :: NullOrUndefined (Version)
  }
derive instance newtypeUpdateThingGroupResponse :: Newtype UpdateThingGroupResponse _


newtype UpdateThingGroupsForThingRequest = UpdateThingGroupsForThingRequest 
  { "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingGroupsToAdd'" :: NullOrUndefined (ThingGroupList)
  , "ThingGroupsToRemove'" :: NullOrUndefined (ThingGroupList)
  }
derive instance newtypeUpdateThingGroupsForThingRequest :: Newtype UpdateThingGroupsForThingRequest _


newtype UpdateThingGroupsForThingResponse = UpdateThingGroupsForThingResponse 
  { 
  }
derive instance newtypeUpdateThingGroupsForThingResponse :: Newtype UpdateThingGroupsForThingResponse _


-- | <p>The input for the UpdateThing operation.</p>
newtype UpdateThingRequest = UpdateThingRequest 
  { "ThingName'" :: (ThingName)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "AttributePayload'" :: NullOrUndefined (AttributePayload)
  , "ExpectedVersion'" :: NullOrUndefined (OptionalVersion)
  , "RemoveThingType'" :: NullOrUndefined (RemoveThingType)
  }
derive instance newtypeUpdateThingRequest :: Newtype UpdateThingRequest _


-- | <p>The output from the UpdateThing operation.</p>
newtype UpdateThingResponse = UpdateThingResponse 
  { 
  }
derive instance newtypeUpdateThingResponse :: Newtype UpdateThingResponse _


newtype UseBase64 = UseBase64 Boolean
derive instance newtypeUseBase64 :: Newtype UseBase64 _


newtype Value = Value String
derive instance newtypeValue :: Newtype Value _


newtype Version = Version Number
derive instance newtypeVersion :: Newtype Version _


-- | <p>An exception thrown when the version of a thing passed to a command is different than the version specified with the --version parameter.</p>
newtype VersionConflictException = VersionConflictException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeVersionConflictException :: Newtype VersionConflictException _


-- | <p>The number of policy versions exceeds the limit.</p>
newtype VersionsLimitExceededException = VersionsLimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeVersionsLimitExceededException :: Newtype VersionsLimitExceededException _


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _


newtype ResourceArn' = ResourceArn' String
derive instance newtypeResourceArn' :: Newtype ResourceArn' _


newtype ResourceId' = ResourceId' String
derive instance newtypeResourceId' :: Newtype ResourceId' _
